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
    % Named insts may be recursive, i.e. they may contain references to
    % themselves. When we unify or merge two insts, we therefore have to be
    % able to recognize and handle situations in which such recursive
    % references could lead to infinite loops.
    %
    % The way in which we do so for unifications is as follows.
    %
    % 1     Before we start to unify InstA and InstB, we create a unify_inst
    %       for the pair, and insert it into the unify inst table as the key,
    %       with the value being inst_det_unknown.
    %
    % 2     We start unifying the two insts.
    %
    % 3     If, during this process, we find that the two (sub)insts we are
    %       trying to unify are already in the unify inst table with value
    %       inst_det_unknown, then (due to 4 below) we know that the process
    %       of unifying that pair of (sub)insts is already in progress.
    %       After step 4, he unify_inst we constructed in step 1 for that pair
    %       of (sub) insts, which may or may not be InstA and InstB,
    %       will represent the outcome of the unification of that pair of
    %       (sub)insts, so in that case, we return that unify_inst as
    %       the result, even though the result inst is not known yet.
    %       (In effect, we know the inst_name of the result without knowing
    %       the actual inst result.)
    %
    % 4     When the unification of InstA and InstB is finished,
    %       we update the entry of the pair in the unify inst table
    %       to record the actual result.
    %
    % We handle the merging of two insts, or making an inst ground,
    % similarly.
    %
    % Regardless of the form of the input inst_name, the value recorded
    % for it in the appropriate part of the inst table (the parts being
    % the unify inst table, the merge inst table, and so on) should be
    % inst_unknown or inst_det_unknown *only while a recursive operation
    % on insts is executing steps 1 through 3. As soon as the last recursive
    % call of that operation is finished, all the entries it has set
    % to inst_unknown or inst_det_unknown should have been set to inst_known or
    % inst_det_known.
    %
:- pred inst_lookup(module_info::in, inst_name::in, mer_inst::out) is det.

    % This predicate is intended to help detect and report bugs in the
    % operations that update (or are supposed to update) the inst table.
    % It looks for and detects two separate kinds of bugs:
    %
    % - unknown insts, where a key is left in one of the inst tables
    %   (such as the unify inst table or the merge inst table) with a value of
    %   inst_unknown or inst_det_unknown *even after the operation
    %   that created that entry has finished*; and
    %
    % - missing insts, which are inst names left in the HLDS which have
    %   *no entry* in the relevant inst table.
    %
    % If inst_lookup_debug finds one that given inst_name has one of
    % these problems, it will return a user_inst with a special form
    % that is_unknown_or_missing_user_inst_name will recognize.
    % The original inst_name passed to inst_lookup_debug will be
    % reconstructable from this inst.
    %
:- pred inst_lookup_debug(module_info::in, inst_name::in, mer_inst::out)
    is det.

:- inst inst_name_user for inst_name/0
    --->    user_inst(ground, ground).

    % Was the user_inst inst_name constructed by inst_lookup_debug
    % to represent an error?
    %
    % If the answer is yes, then succeed; the sym_name part of the user_inst
    % will describe the non-inst parts of the failed inst_name lookup, and
    % its argument insts will describe its inst parts.
    %
    % If the answer is no, then fail.
    %
:- pred is_unknown_or_missing_user_inst_name(inst_name::in(inst_name_user))
    is semidet.

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
:- import_module parse_tree.parse_tree_to_term.
:- import_module parse_tree.prog_mode.

:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

inst_lookup(ModuleInfo, InstName, Inst) :-
    % The non-error-handling parts of inst_lookup and inst_lookup_debug
    % should be kept in sync.
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
        InstName = user_inst(SymName, ArgInsts),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_user_insts(InstTable, UserInstTable),
        list.length(ArgInsts, Arity),
        InstCtor = inst_ctor(SymName, Arity),
        ( if map.search(UserInstTable, InstCtor, InstDefn) then
            InstDefn = hlds_inst_defn(_VarSet, InstParams, InstBody, _MMTC,
                _Context, _Status),
            InstBody = eqv_inst(Inst0),
            inst_substitute_arg_list(InstParams, ArgInsts, Inst0, Inst)
        else
            NameStr = sym_name_to_string(SymName),
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

inst_lookup_debug(ModuleInfo, InstName, Inst) :-
    % The non-error-handling parts of inst_lookup and inst_lookup_debug
    % should be kept in sync.
    (
        InstName = unify_inst(Live, Real, InstA, InstB),
        UnifyInstInfo = unify_inst_info(Live, Real, InstA, InstB),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_unify_insts(InstTable, UnifyInstTable),
        string.format("unify_%s_%s",
            [s(is_live_to_str(Live)), s(unify_is_real_to_str(Real))], Desc),
        ( if
            search_unify_inst(UnifyInstTable, UnifyInstInfo, MaybeInstDet)
        then
            (
                MaybeInstDet = inst_det_known(Inst, _)
            ;
                MaybeInstDet = inst_det_unknown,
                Inst = make_unknown_inst_name(Desc, [InstA, InstB])
            )
        else
            Inst = make_missing_inst_name(Desc, [InstA, InstB])
        )
    ;
        InstName = merge_inst(InstA, InstB),
        MergeInstInfo = merge_inst_info(InstA, InstB),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_merge_insts(InstTable, MergeInstTable),
        ( if search_merge_inst(MergeInstTable, MergeInstInfo, MaybeInst) then
            (
                MaybeInst = inst_known(Inst)
            ;
                MaybeInst = inst_unknown,
                Inst = make_unknown_inst_name("merge", [InstA, InstB])
            )
        else
            Inst = make_missing_inst_name("merge", [InstA, InstB])
        )
    ;
        InstName = ground_inst(SubInstName, Uniq, Live, Real),
        GroundInstInfo = ground_inst_info(SubInstName, Uniq, Live, Real),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_ground_insts(InstTable, GroundInstTable),
        string.format("ground_%s_%s_%s",
            [s(inst_uniqueness(Uniq, "ground")), s(is_live_to_str(Live)),
            s(unify_is_real_to_str(Real))], Desc),
        ( if
            search_ground_inst(GroundInstTable, GroundInstInfo, MaybeInstDet)
        then
            (
                MaybeInstDet = inst_det_known(Inst, _)
            ;
                MaybeInstDet = inst_det_unknown,
                Inst = make_unknown_inst_name(Desc,
                    [defined_inst(SubInstName)])
            )
        else
            Inst = make_missing_inst_name(Desc, [defined_inst(SubInstName)])
        )
    ;
        InstName = any_inst(SubInstName, Uniq, Live, Real),
        AnyInstInfo = any_inst_info(SubInstName, Uniq, Live, Real),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_any_insts(InstTable, AnyInstTable),
        string.format("any_%s_%s_%s",
            [s(inst_uniqueness(Uniq, "shared")), s(is_live_to_str(Live)),
            s(unify_is_real_to_str(Real))], Desc),
        ( if search_any_inst(AnyInstTable, AnyInstInfo, MaybeInstDet) then
            (
                MaybeInstDet = inst_det_known(Inst, _)
            ;
                MaybeInstDet = inst_det_unknown,
                Inst = make_unknown_inst_name(Desc,
                    [defined_inst(SubInstName)])
            )
        else
            Inst = make_missing_inst_name(Desc, [defined_inst(SubInstName)])
        )
    ;
        InstName = shared_inst(SharedInstName),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_shared_insts(InstTable, SharedInstTable),
        ( if
            search_shared_inst(SharedInstTable, SharedInstName, MaybeInst)
        then
            (
                MaybeInst = inst_known(Inst)
            ;
                MaybeInst = inst_unknown,
                Inst = make_unknown_inst_name("shared",
                    [defined_inst(SharedInstName)])
            )
        else
            Inst = make_missing_inst_name("shared",
                [defined_inst(SharedInstName)])
        )
    ;
        InstName = mostly_uniq_inst(NondetLiveInstName),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_mostly_uniq_insts(InstTable, MostlyUniqInstTable),
        ( if
            search_mostly_uniq_inst(MostlyUniqInstTable, NondetLiveInstName,
                MaybeInst)
        then
            (
                MaybeInst = inst_known(Inst)
            ;
                MaybeInst = inst_unknown,
                Inst = make_unknown_inst_name("mostly_uniq",
                    [defined_inst(NondetLiveInstName)])
            )
        else
            Inst = make_missing_inst_name("mostly_uniq",
                [defined_inst(NondetLiveInstName)])
        )
    ;
        InstName = user_inst(SymName, ArgInsts),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_user_insts(InstTable, UserInstTable),
        list.length(ArgInsts, Arity),
        InstCtor = inst_ctor(SymName, Arity),
        ( if map.search(UserInstTable, InstCtor, InstDefn) then
            InstDefn = hlds_inst_defn(_VarSet, InstParams, InstBody, _MMTC,
                _Context, _Status),
            InstBody = eqv_inst(Inst0),
            inst_substitute_arg_list(InstParams, ArgInsts, Inst0, Inst)
        else if is_unknown_or_missing_user_inst_name(InstName) then
            Inst = defined_inst(InstName)
        else
            NameStr = sym_name_to_string(SymName),
            string.format("reference to undefined inst %s", [s(NameStr)], Msg),
            unexpected($pred, Msg)
        )
    ;
        InstName = typed_ground(Uniq, Type),
        Inst0 = ground(Uniq, none_or_default_func),
        propagate_unchecked_type_into_inst(ModuleInfo, Type, Inst0, Inst)
    ;
        InstName = typed_inst(Type, TypedInstName),
        inst_lookup_debug(ModuleInfo, TypedInstName, Inst0),
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

:- func make_unknown_inst_name(string, list(mer_inst)) = mer_inst.

make_unknown_inst_name(Desc, ArgInsts) = UnknownInst :-
    UnknownInstNameStr = "UNKNOWN_INST_" ++ Desc,
    UnknownInstName = user_inst(unqualified(UnknownInstNameStr), ArgInsts),
    UnknownInst = defined_inst(UnknownInstName).

:- func make_missing_inst_name(string, list(mer_inst)) = mer_inst.

make_missing_inst_name(Desc, ArgInsts) = MissingInst :-
    MissingInstNameStr = "MISSING_INST_" ++ Desc,
    MissingInstName = user_inst(unqualified(MissingInstNameStr), ArgInsts),
    MissingInst = defined_inst(MissingInstName).

is_unknown_or_missing_user_inst_name(user_inst(SymName, _ArgInsts)) :-
    SymName = unqualified(Name),
    ( string.prefix(Name, "UNKNOWN_INST_")
    ; string.prefix(Name, "MISSING_INST_")
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
