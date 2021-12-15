%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mode_top_functor.m.
%
% This module computes top_functor_modes.
%
%---------------------------------------------------------------------------%

:- module check_hlds.mode_top_functor.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%
%
% Computing top_functor_modes.
%

    % mode_to_top_functor_mode converts a mode (and corresponding type)
    % to a top_functor_mode.
    %
    % A mode is a high-level notion, the normal Mercury language mode.
    % A top_functor_mode is a low-level notion used for code generation,
    % which indicates the argument passing convention (top_in, top_out, or
    % top_unused) that corresponds to that mode. We need to know the type,
    % not just the mode, because the argument passing convention can depend
    % on the type's representation.
    %
:- pred mode_to_top_functor_mode(module_info::in, mer_mode::in,
    mer_type::in, top_functor_mode::out) is det.
:- pred init_final_insts_to_top_functor_mode(module_info::in,
    mer_inst::in, mer_inst::in, mer_type::in, top_functor_mode::out) is det.

    % Zip together the given lists of modes and types, and return
    % the top_functor_mode computed by mode_to_top_functor_mode
    % for each pair.
    %
:- pred modes_to_top_functor_modes(module_info::in,
    list(mer_mode)::in, list(mer_type)::in, list(top_functor_mode)::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type.

:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%

mode_to_top_functor_mode(ModuleInfo, Mode, Type, TopFunctorMode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    find_top_functor_mode_loop_over_notags(ModuleInfo, Type, [],
        InitialInst, FinalInst, TopFunctorMode).

init_final_insts_to_top_functor_mode(ModuleInfo, InitialInst, FinalInst, Type,
        TopFunctorMode) :-
    find_top_functor_mode_loop_over_notags(ModuleInfo, Type, [],
        InitialInst, FinalInst, TopFunctorMode).

:- pred find_top_functor_mode_loop_over_notags(module_info::in,
    mer_type::in, list(type_ctor)::in, mer_inst::in, mer_inst::in,
    top_functor_mode::out) is det.

find_top_functor_mode_loop_over_notags(ModuleInfo, Type, ContainingTypes,
        InitialInst, FinalInst, TopFunctorMode) :-
    % We need to handle no_tag types (types which have exactly one constructor,
    % and whose one constructor has exactly one argument) specially here,
    % since for them an inst of bound(f(free)) is not really bound as far as
    % code generation is concerned, since the f/1 will get optimized away.
    ( if
        % Is this a no_tag type?
        type_is_no_tag_type(ModuleInfo, Type, FunctorName, ArgType),
        % Avoid infinite recursion.
        type_to_ctor(Type, TypeCtor),
        not list.member(TypeCtor, ContainingTypes)
    then
        % The top_functor_mode will be determined by the mode and type of the
        % functor's argument, so we figure out the mode and type of the
        % argument, and then recurse.

        ConsId = cons(FunctorName, 1, TypeCtor),
        get_single_arg_inst(ModuleInfo, InitialInst, ConsId, InitialArgInst),
        get_single_arg_inst(ModuleInfo, FinalInst, ConsId, FinalArgInst),
        find_top_functor_mode_loop_over_notags(ModuleInfo,
            ArgType, [TypeCtor | ContainingTypes],
            InitialArgInst, FinalArgInst, TopFunctorMode)
    else
        ( if inst_is_bound(ModuleInfo, InitialInst) then
            TopFunctorMode = top_in
        else if inst_is_bound(ModuleInfo, FinalInst) then
            TopFunctorMode = top_out
        else
            TopFunctorMode = top_unused
        )
    ).

    % get_single_arg_inst(ModuleInfo, Inst, ConsId, ArgInsts):
    %
    % Given an inst `Inst', figure out what the inst of the argument would be,
    % assuming that the functor is the one given by the specified ConsId,
    % whose arity is 1.
    %
:- pred get_single_arg_inst(module_info::in, mer_inst::in, cons_id::in,
    mer_inst::out) is det.

get_single_arg_inst(ModuleInfo, Inst, ConsId, ArgInst) :-
    % XXX This is very similar to get_arg_insts in prog_mode.
    (
        Inst = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NamedInst),
        get_single_arg_inst(ModuleInfo, NamedInst, ConsId, ArgInst)
    ;
        Inst = not_reached,
        ArgInst = not_reached
    ;
        Inst = ground(Uniq, _PredInst),
        ArgInst = ground(Uniq, none_or_default_func)
    ;
        Inst = bound(_Uniq, _InstResult, List),
        ( if get_single_arg_inst_in_bound_insts(List, ConsId, ArgInst0) then
            ArgInst = ArgInst0
        else
            % The code is unreachable.
            ArgInst = not_reached
        )
    ;
        Inst = free,
        ArgInst = free
    ;
        Inst = free(_Type),
        ArgInst = free   % XXX loses type info
    ;
        Inst = any(Uniq, _),
        ArgInst = any(Uniq, none_or_default_func)
    ;
        Inst = abstract_inst(_, _),
        unexpected($pred, "abstract insts not supported")
    ;
        Inst = inst_var(_),
        unexpected($pred, "inst_var")
    ;
        Inst = constrained_inst_vars(_, InsideInst),
        get_single_arg_inst(ModuleInfo, InsideInst, ConsId, ArgInst)
    ).

:- pred get_single_arg_inst_in_bound_insts(list(bound_inst)::in, cons_id::in,
    mer_inst::out) is semidet.

get_single_arg_inst_in_bound_insts([BoundInst | BoundInsts], ConsId,
        ArgInst) :-
    ( if
        BoundInst = bound_functor(InstConsId, [ArgInst0]),
        % The cons_ids for types and insts can differ in the type_ctor field
        % so we must ignore them.
        equivalent_cons_ids(ConsId, InstConsId)
    then
        ArgInst = ArgInst0
    else
        get_single_arg_inst_in_bound_insts(BoundInsts, ConsId, ArgInst)
    ).

modes_to_top_functor_modes(_ModuleInfo, [], [], []).
modes_to_top_functor_modes(_ModuleInfo, [], [_ | _], _) :-
        unexpected($pred, "length mismatch").
modes_to_top_functor_modes(_ModuleInfo, [_ | _], [], _) :-
        unexpected($pred, "length mismatch").
modes_to_top_functor_modes(ModuleInfo, [Mode | Modes], [Type | Types],
        [TopFunctorMode | TopFunctorModes]) :-
    mode_to_top_functor_mode(ModuleInfo, Mode, Type, TopFunctorMode),
    modes_to_top_functor_modes(ModuleInfo, Modes, Types, TopFunctorModes).

%---------------------------------------------------------------------------%
:- end_module check_hlds.mode_top_functor.
%---------------------------------------------------------------------------%
