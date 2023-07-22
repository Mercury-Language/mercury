%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: inst_lookup.m.
% Author: fjh.
%
% This module looks up insts in the module_info, optionally expanding out
% references to user defined insts in the process.
%
%---------------------------------------------------------------------------%

:- module check_hlds.inst_lookup.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

%---------------------------------------------------------------------------%
%
% Looking up named insts.
%

    % Given a user-defined or compiler-defined inst name, look up the
    % corresponding inst in the inst table.
    %
:- pred inst_lookup(module_info::in, inst_name::in, mer_inst::out) is det.

%---------------------------------------------------------------------------%

:- inst mer_inst_expanded for mer_inst/0
    --->        ground(ground, ground)
    ;           free
    ;           bound(ground, ground, ground)
    ;           constrained_inst_vars(ground, ground)
    ;           not_reached
    ;           any(ground, ground)
    ;           inst_var(ground).

:- inst mer_inst_expanded_no_constraints for mer_inst/0
    --->        ground(ground, ground)
    ;           free
    ;           bound(ground, ground, ground)
    ;           not_reached
    ;           any(ground, ground)
    ;           inst_var(ground).

    % inst_expand(ModuleInfo, Inst0, Inst) checks if the top-level part
    % of the inst is a defined inst, and if so replaces it with the definition.
    %
    % This leaves insts with constrained_inst_vars at the top level unchanged.
    %
:- pred inst_expand(module_info::in, mer_inst::in,
    mer_inst::out(mer_inst_expanded)) is det.

    % inst_expand_and_remove_constrained_inst_vars is the same as inst_expand
    % except that it also removes constrained_inst_vars from the top level,
    % replacing them with the constraining inst.
    %
:- pred inst_expand_and_remove_constrained_inst_vars(module_info::in,
    mer_inst::in, mer_inst::out(mer_inst_expanded_no_constraints)) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_mode_type_prop.
:- import_module hlds.hlds_inst_mode.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.

:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

inst_lookup(ModuleInfo, InstName, Inst) :-
    (
        InstName = unify_inst(Live, Real, InstA, InstB),
        UnifyInstInfo = unify_inst_info(Live, Real, InstA, InstB),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_unify_insts(InstTable, UnifyInstTable),
        lookup_unify_inst(UnifyInstTable, UnifyInstInfo, MaybeInstDet),
        (
            MaybeInstDet = inst_det_known(Inst, _)
        ;
            MaybeInstDet = inst_det_unknown,
            Inst = defined_inst(InstName)
        )
    ;
        InstName = merge_inst(InstA, InstB),
        MergeInstInfo = merge_inst_info(InstA, InstB),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_merge_insts(InstTable, MergeInstTable),
        lookup_merge_inst(MergeInstTable, MergeInstInfo, MaybeInst),
        (
            MaybeInst = inst_known(Inst)
        ;
            MaybeInst = inst_unknown,
            Inst = defined_inst(InstName)
        )
    ;
        InstName = ground_inst(SubInstName, Uniq, Live, Real),
        GroundInstInfo = ground_inst_info(SubInstName, Uniq, Live, Real),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_ground_insts(InstTable, GroundInstTable),
        lookup_ground_inst(GroundInstTable, GroundInstInfo, MaybeInstDet),
        (
            MaybeInstDet = inst_det_known(Inst, _)
        ;
            MaybeInstDet = inst_det_unknown,
            Inst = defined_inst(InstName)
        )
    ;
        InstName = any_inst(SubInstName, Uniq, Live, Real),
        AnyInstInfo = any_inst_info(SubInstName, Uniq, Live, Real),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_any_insts(InstTable, AnyInstTable),
        lookup_any_inst(AnyInstTable, AnyInstInfo, MaybeInstDet),
        (
            MaybeInstDet = inst_det_known(Inst, _)
        ;
            MaybeInstDet = inst_det_unknown,
            Inst = defined_inst(InstName)
        )
    ;
        InstName = shared_inst(SharedInstName),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_shared_insts(InstTable, SharedInstTable),
        lookup_shared_inst(SharedInstTable, SharedInstName, MaybeInst),
        (
            MaybeInst = inst_known(Inst)
        ;
            MaybeInst = inst_unknown,
            Inst = defined_inst(InstName)
        )
    ;
        InstName = mostly_uniq_inst(NondetLiveInstName),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_mostly_uniq_insts(InstTable, MostlyUniqInstTable),
        lookup_mostly_uniq_inst(MostlyUniqInstTable, NondetLiveInstName,
            MaybeInst),
        (
            MaybeInst = inst_known(Inst)
        ;
            MaybeInst = inst_unknown,
            Inst = defined_inst(InstName)
        )
    ;
        InstName = user_inst(Name, Args),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_user_insts(InstTable, UserInstTable),
        list.length(Args, Arity),
        ( if map.search(UserInstTable, inst_ctor(Name, Arity), InstDefn) then
            InstDefn = hlds_inst_defn(_VarSet, Params, InstBody, _MMTC,
                _Context, _Status),
            InstBody = eqv_inst(Inst0),
            inst_substitute_arg_list(Params, Args, Inst0, Inst)
        else
            NameStr = sym_name_to_string(Name),
            string.format("reference to undefined inst %s", [s(NameStr)], Msg),
            unexpected($pred, Msg)
        )
    ;
        InstName = typed_ground(Uniq, Type),
        Inst0 = ground(Uniq, none_or_default_func),
        propagate_unchecked_type_into_inst(ModuleInfo, Type, Inst0, Inst)
    ;
        InstName = typed_inst(Type, TypedInstName),
        inst_lookup(ModuleInfo, TypedInstName, Inst0),
        % XXX Each invocation of inst_lookup expands out one inst_name.
        % An inst_name of nonzeero arity will be applied to a list of insts,
        % some of which may contain other inst_names, whose arguments
        % may contain other inst_names, and so on.
        %
        % Such situations represent potential performance problems, because
        % this call will propagate type information into *all* parts of Inst0,
        % not just the top layer. This means that an inst inside argument
        % lists of N nested inst_names will have type information propagated
        % into it N times.
        propagate_unchecked_type_into_inst(ModuleInfo, Type, Inst0, Inst)
    ).

%---------------------------------------------------------------------------%

inst_expand(ModuleInfo, !Inst) :-
    (
        !.Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, !:Inst),
        disable_warning [suspicious_recursion] (
            inst_expand(ModuleInfo, !Inst)
        )
    ;
        ( !.Inst = free
        ; !.Inst = not_reached
        ; !.Inst = ground(_, _)
        ; !.Inst = any(_, _)
        ; !.Inst = bound(_, _, _)
        ; !.Inst = constrained_inst_vars(_, _)
        ; !.Inst = inst_var(_)
        )
    ).

inst_expand_and_remove_constrained_inst_vars(ModuleInfo, !Inst) :-
    (
        !.Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, !:Inst),
        inst_expand_and_remove_constrained_inst_vars(ModuleInfo, !Inst)
    ;
        !.Inst = constrained_inst_vars(_, !:Inst),
        inst_expand_and_remove_constrained_inst_vars(ModuleInfo, !Inst)
    ;
        ( !.Inst = free
        ; !.Inst = not_reached
        ; !.Inst = ground(_, _)
        ; !.Inst = any(_, _)
        ; !.Inst = bound(_, _, _)
        ; !.Inst = inst_var(_)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.inst_lookup.
%---------------------------------------------------------------------------%
