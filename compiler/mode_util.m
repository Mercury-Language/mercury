%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mode_util.m.
% Main author: fjh.
%
% This module contains utility predicates for dealing with modes and insts.
%
%---------------------------------------------------------------------------%

:- module check_hlds.mode_util.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%
% XXX Group related declarations together.
% XXX Put the groups in a logical order.
% XXX Reorder the predicate definitions to match the declaration order.
%---------------------------------------------------------------------------%

:- func from_to_insts_to_mode(from_to_insts) = mer_mode.
:- func mode_to_from_to_insts(module_info, mer_mode) = from_to_insts.

:- func from_to_insts_to_init_inst(from_to_insts) = mer_inst.
:- func from_to_insts_to_final_inst(from_to_insts) = mer_inst.
% :- pred from_to_insts_to_both_insts(from_to_insts::in,
%     mer_inst::out, mer_inst::out) is det.

:- pred unify_mode_to_lhs_rhs_from_to_insts(unify_mode::in,
    from_to_insts::out, from_to_insts::out) is det.

%---------------------------------------------------------------------------%

    % Return the initial and the final instantiatedness for the given mode.
    % Throw an exception if the mode is undefined.
    %
:- pred mode_get_insts(module_info::in, mer_mode::in,
    mer_inst::out, mer_inst::out) is det.

:- pred mode_get_from_to_insts(module_info::in, mer_mode::in,
    from_to_insts::out) is det.

    % Return the initial and final instantiatedness for the given mode.
    % Fail if the mode is undefined.
    %
:- pred mode_get_insts_semidet(module_info::in, mer_mode::in,
    mer_inst::out, mer_inst::out) is semidet.

    % Succeed iff the mode is input.
    % Throw an exception if the mode is undefined.
    %
    % A mode is considered input if the initial inst is bound.
    %
:- pred mode_is_input(module_info::in, mer_mode::in) is semidet.
:- pred from_to_insts_is_input(module_info::in, from_to_insts::in) is semidet.

    % Succeed iff the mode is fully input.
    % Throw an exception if the mode is undefined.
    %
    % A mode is considered fully input if the initial inst is ground.
    %
:- pred mode_is_fully_input(module_info::in, mer_mode::in) is semidet.
:- pred from_to_insts_is_fully_input(module_info::in, from_to_insts::in)
    is semidet.

    % Succeed iff the mode is output.
    % Throw an exception if the mode is undefined.
    %
    % A mode is considered output if the initial inst is free and
    % the final inst is bound.
    %
:- pred mode_is_output(module_info::in, mer_mode::in) is semidet.
:- pred from_to_insts_is_output(module_info::in, from_to_insts::in) is semidet.

    % Succeed iff the mode is fully output.
    % Throw an exception if the mode is undefined.
    %
    % A mode is considered fully output if the initial inst is free
    % and the final inst is ground.
    %
:- pred mode_is_fully_output(module_info::in, mer_mode::in) is semidet.
:- pred from_to_insts_is_fully_output(module_info::in, from_to_insts::in)
    is semidet.

    % Succeed iff the mode is unused.
    % Throws an exception if the mode is undefined.
    %
    % A mode is considered unused if both the initial and final insts are free.
    %
:- pred mode_is_unused(module_info::in, mer_mode::in) is semidet.
:- pred from_to_insts_is_unused(module_info::in, from_to_insts::in) is semidet.

%---------------------%

    % Return the modes of the operands on the given side of the unifications.
    %
:- func unify_modes_to_lhs_mode(unify_mode) = mer_mode.
:- func unify_modes_to_rhs_mode(unify_mode) = mer_mode.
:- func unify_modes_to_lhs_from_to_insts(unify_mode) = from_to_insts.
:- func unify_modes_to_rhs_from_to_insts(unify_mode) = from_to_insts.

    % Given the modes of the two sides of a unification, return the unify_mode.
    %
:- pred modes_to_unify_mode(module_info::in,
    mer_mode::in, mer_mode::in, unify_mode::out) is det.
:- pred from_to_insts_to_unify_mode(from_to_insts::in, from_to_insts::in,
    unify_mode::out) is det.

    % Given two lists of modes (inst mappings) of equal length,
    % return a unify_mode for each corresponding pair of modes.
    %
:- pred modes_to_unify_modes(module_info::in,
    list(mer_mode)::in, list(mer_mode)::in, list(unify_mode)::out) is det.
:- pred from_to_insts_to_unify_modes(
    list(from_to_insts)::in, list(from_to_insts)::in, list(unify_mode)::out)
    is det.

%---------------------%

:- pred modes_to_top_functor_modes(module_info::in, list(mer_mode)::in,
    list(mer_type)::in, list(top_functor_mode)::out) is det.

    % mode_to_top_functor_mode converts a mode (and corresponding type)
    % to a top_functor_mode.
    % A mode is a high-level notion, the normal Mercury language mode.
    % A top_functor_mode is a low-level notion used for code generation,
    % which indicates the argument passing convention (top_in, top_out, or
    % top_unused) that corresponds to that mode. We need to know the type,
    % not just the mode, because the argument passing convention can depend
    % on the type's representation.
    %
:- pred mode_to_top_functor_mode(module_info::in, mer_mode::in,
    mer_type::in, top_functor_mode::out) is det.
:- pred from_to_insts_to_top_functor_mode(module_info::in, from_to_insts::in,
    mer_type::in, top_functor_mode::out) is det.

%---------------------%

    % Given a list of variables and their corresponding modes,
    % return a list containing only those variables which have an output mode.
    %
:- func select_output_vars(module_info, list(prog_var), list(mer_mode),
    vartypes) = list(prog_var).
:- func select_output_things(module_info, list(Thing), list(mer_mode),
    map(Thing, mer_type)) = list(Thing).

%---------------------%

:- func mode_get_initial_inst(module_info, mer_mode) = mer_inst.

:- func mode_get_final_inst(module_info, mer_mode) = mer_inst.

:- pred mode_list_get_initial_insts(module_info::in,
    list(mer_mode)::in, list(mer_inst)::out) is det.

:- pred mode_list_get_final_insts(module_info::in,
    list(mer_mode)::in, list(mer_inst)::out) is det.

%---------------------%

    % Given a user-defined or compiler-defined inst name, lookup the
    % corresponding inst in the inst table.
    %
:- pred inst_lookup(module_info::in, inst_name::in, mer_inst::out) is det.

%---------------------%

:- type recompute_atomic_instmap_deltas
    --->    recompute_atomic_instmap_deltas
    ;       do_not_recompute_atomic_instmap_deltas.

    % Use the instmap deltas for all the atomic sub-goals to recompute
    % the instmap deltas for all the non-atomic sub-goals of a goal.
    % Used to ensure that the instmap deltas remain valid after code has
    % been re-arranged, e.g. by followcode. This also takes the module_info
    % as input and output since it may need to insert new merge_insts
    % into the merge_inst table. The first argument says whether the
    % instmap_deltas for calls and deconstruction unifications
    % should also recomputed.
    %
:- pred recompute_instmap_delta_proc(recompute_atomic_instmap_deltas::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

:- pred recompute_instmap_delta(recompute_atomic_instmap_deltas::in,
    hlds_goal::in, hlds_goal::out, vartypes::in, inst_varset::in,
    instmap::in, module_info::in, module_info::out) is det.

    % Given corresponding lists of types and modes, produce a new list
    % of modes which includes the information provided by the
    % corresponding types.
    %
:- pred propagate_types_into_mode_list(module_info::in, list(mer_type)::in,
    list(mer_mode)::in, list(mer_mode)::out) is det.

    % Given corresponding lists of types and insts and a substitution
    % for the type variables in the type, produce a new list of insts
    % which includes the information provided by the corresponding types.
    %
:- pred propagate_types_into_inst_list(module_info::in, tsubst::in,
    list(mer_type)::in, list(mer_inst)::in, list(mer_inst)::out) is det.

    % Convert a list of constructors to a list of bound_insts where the
    % arguments are `ground'.
    %
    % NOTE: the list(bound_inst) is not sorted and may contain duplicates.
    %
:- pred constructors_to_bound_insts(module_info::in, uniqueness::in,
    type_ctor::in, list(constructor)::in, list(bound_inst)::out) is det.

    % Convert a list of constructors to a list of bound_insts where the
    % arguments are `any'.
    %
    % NOTE: the list(bound_inst) is not sorted and may contain duplicates.
    %
:- pred constructors_to_bound_any_insts(module_info::in, uniqueness::in,
    type_ctor::in, list(constructor)::in, list(bound_inst)::out) is det.

    % A bound(_, _, BoundInsts) inst contains a cons_id in each of the
    % BoundInsts. This predicate records, for each of those cons_ids,
    % that the cons_id belongs to the given type, *if* in fact that cons_id
    % is one of the function symbols of the give type.
    %
    % NOTE: If insts were required to belong to just one explicitly specified
    % type, as they should be, this predicate would not be necessary.
    %
:- pred propagate_ctor_info_into_bound_inst(module_info::in, mer_type::in,
    mer_inst::in(mer_inst_is_bound), mer_inst::out) is det.

    % Given the mode of a predicate, work out which arguments are live
    % (might be used again by the caller of that predicate) and which are dead.
    %
:- pred get_arg_lives(module_info::in, list(mer_mode)::in, list(is_live)::out)
    is det.

    % Given the switched on variable and the instmaps before the switch
    % and after a branch make sure that any information added by the
    % functor test gets added to the instmap for the case.
    %
:- pred fixup_instmap_switch_var(prog_var::in, instmap::in, instmap::in,
    hlds_goal::in, hlds_goal::out) is det.

%---------------------------------------------------------------------------%

:- pred normalise_insts(module_info::in, list(mer_type)::in,
    list(mer_inst)::in, list(mer_inst)::out) is det.

:- pred normalise_inst(module_info::in, mer_type::in,
    mer_inst::in, mer_inst::out) is det.

%---------------------------------------------------------------------------%

    % Partition a list of arguments into inputs and others.
    % Throws an exception if one of the modes is undefined.
    %
:- pred partition_args(module_info::in, list(mer_mode)::in, list(T)::in,
    list(T)::out, list(T)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_data.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

from_to_insts_to_mode(FromToInsts) = Mode :-
    FromToInsts = from_to_insts(Init, Final),
    Mode = from_to_mode(Init, Final).

mode_to_from_to_insts(ModuleInfo, Mode) = FromToInsts :-
    mode_get_insts(ModuleInfo, Mode, Init, Final),
    FromToInsts = from_to_insts(Init, Final).

from_to_insts_to_init_inst(FromToInsts) = Init :-
    FromToInsts = from_to_insts(Init, _Final).

from_to_insts_to_final_inst(FromToInsts) = Final :-
    FromToInsts = from_to_insts(_Init, Final).

unify_mode_to_lhs_rhs_from_to_insts(UnifyMode, LHSInsts, RHSInsts) :-
    UnifyMode = unify_modes_lhs_rhs(LHSInsts, RHSInsts).

%---------------------------------------------------------------------------%

mode_get_initial_inst(ModuleInfo, Mode) = Inst :-
    mode_get_insts(ModuleInfo, Mode, Inst, _).

mode_get_final_inst(ModuleInfo, Mode) = Inst :-
    mode_get_insts(ModuleInfo, Mode, _, Inst).

mode_list_get_initial_insts(_ModuleInfo, [], []).
mode_list_get_initial_insts(ModuleInfo, [Mode | Modes], [Inst | Insts]) :-
    mode_get_insts(ModuleInfo, Mode, Inst, _),
    mode_list_get_initial_insts(ModuleInfo, Modes, Insts).

mode_list_get_final_insts(_ModuleInfo, [], []).
mode_list_get_final_insts(ModuleInfo, [Mode | Modes], [Inst | Insts]) :-
    mode_get_insts(ModuleInfo, Mode, _, Inst),
    mode_list_get_final_insts(ModuleInfo, Modes, Insts).

%---------------------------------------------------------------------------%

mode_is_input(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, _FinalInst),
    inst_is_bound(ModuleInfo, InitialInst).

from_to_insts_is_input(ModuleInfo, FromToInsts) :-
    FromToInsts = from_to_insts(InitialInst, _FinalInst),
    inst_is_bound(ModuleInfo, InitialInst).

mode_is_fully_input(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, _FinalInst),
    inst_is_ground(ModuleInfo, InitialInst).

from_to_insts_is_fully_input(ModuleInfo, FromToInsts) :-
    FromToInsts = from_to_insts(InitialInst, _FinalInst),
    inst_is_ground(ModuleInfo, InitialInst).

mode_is_output(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_bound(ModuleInfo, FinalInst).

from_to_insts_is_output(ModuleInfo, FromToInsts) :-
    FromToInsts = from_to_insts(InitialInst, FinalInst),
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_bound(ModuleInfo, FinalInst).

mode_is_fully_output(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_ground(ModuleInfo, FinalInst).

from_to_insts_is_fully_output(ModuleInfo, FromToInsts) :-
    FromToInsts = from_to_insts(InitialInst, FinalInst),
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_ground(ModuleInfo, FinalInst).

mode_is_unused(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_free(ModuleInfo, FinalInst).

from_to_insts_is_unused(ModuleInfo, FromToInsts) :-
    FromToInsts = from_to_insts(InitialInst, FinalInst),
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_free(ModuleInfo, FinalInst).

%---------------------------------------------------------------------------%

unify_modes_to_lhs_mode(UnifyMode) = LHSMode :-
    UnifyMode = unify_modes_lhs_rhs(LHSFromToInsts, _RHSFromToInsts),
    LHSFromToInsts = from_to_insts(LHSInitInst, LHSFinalInst),
    LHSMode = from_to_mode(LHSInitInst, LHSFinalInst).

unify_modes_to_rhs_mode(UnifyMode) = RHSMode :-
    UnifyMode = unify_modes_lhs_rhs(_LHSFromToInsts, RHSFromToInsts),
    RHSFromToInsts = from_to_insts(RHSInitInst, RHSFinalInst),
    RHSMode = from_to_mode(RHSInitInst, RHSFinalInst).

unify_modes_to_lhs_from_to_insts(UnifyMode) = LHSFromToInsts :-
    UnifyMode = unify_modes_lhs_rhs(LHSFromToInsts, _RHSFromToInsts).

unify_modes_to_rhs_from_to_insts(UnifyMode) = RHSFromToInsts :-
    UnifyMode = unify_modes_lhs_rhs(_LHSFromToInsts, RHSFromToInsts).

%---------------------%

modes_to_top_functor_modes(_ModuleInfo, [], [], []).
modes_to_top_functor_modes(_ModuleInfo, [], [_ | _], _) :-
        unexpected($module, $pred, "length mismatch").
modes_to_top_functor_modes(_ModuleInfo, [_ | _], [], _) :-
        unexpected($module, $pred, "length mismatch").
modes_to_top_functor_modes(ModuleInfo, [Mode | Modes], [Type | Types],
        [TopFunctorMode | TopFunctorModes]) :-
    mode_to_top_functor_mode(ModuleInfo, Mode, Type, TopFunctorMode),
    modes_to_top_functor_modes(ModuleInfo, Modes, Types, TopFunctorModes).

mode_to_top_functor_mode(ModuleInfo, Mode, Type, TopFunctorMode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    find_top_functor_mode_loop_over_notags(ModuleInfo, Type, [],
        InitialInst, FinalInst, TopFunctorMode).

from_to_insts_to_top_functor_mode(ModuleInfo, FromToInsts, Type,
        TopFunctorMode) :-
    FromToInsts = from_to_insts(InitialInst, FinalInst),
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

select_output_vars(ModuleInfo, HeadVars, HeadModes, VarTypes) = OutputVars :-
    (
        HeadVars = [],
        HeadModes = [],
        OutputVars = []
    ;
        HeadVars = [Var | Vars],
        HeadModes = [Mode | Modes],
        lookup_var_type(VarTypes, Var, VarType),
        mode_to_top_functor_mode(ModuleInfo, Mode, VarType, Top),
        (
            Top = top_out,
            OutputVars1 = select_output_vars(ModuleInfo, Vars, Modes,
                VarTypes),
            OutputVars = [Var | OutputVars1]
        ;
            ( Top = top_in
            ; Top = top_unused
            ),
            OutputVars = select_output_vars(ModuleInfo, Vars, Modes, VarTypes)
        )
    ;
        HeadVars = [],
        HeadModes = [_ | _],
        unexpected($module, $pred, "length mismatch")
    ;
        HeadVars = [_ | _],
        HeadModes = [],
        unexpected($module, $pred, "length mismatch")
    ).

select_output_things(ModuleInfo, HeadThings, HeadModes, ThingTypes) =
        OutputThings :-
    (
        HeadThings = [],
        HeadModes = [],
        OutputThings = []
    ;
        HeadThings = [Thing | Things],
        HeadModes = [Mode | Modes],
        map.lookup(ThingTypes, Thing, ThingType),
        mode_to_top_functor_mode(ModuleInfo, Mode, ThingType, Top),
        (
            Top = top_out,
            OutputThings1 = select_output_things(ModuleInfo, Things, Modes,
                ThingTypes),
            OutputThings = [Thing | OutputThings1]
        ;
            ( Top = top_in
            ; Top = top_unused
            ),
            OutputThings = select_output_things(ModuleInfo, Things, Modes,
                ThingTypes)
        )
    ;
        HeadThings = [],
        HeadModes = [_ | _],
        unexpected($module, $pred, "length mismatch")
    ;
        HeadThings = [_ | _],
        HeadModes = [],
        unexpected($module, $pred, "length mismatch")
    ).

%---------------------------------------------------------------------------%

    % get_single_arg_inst(ModuleInfo, Inst, ConsId, ArgInsts):
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
        unexpected($module, $pred, "abstract insts not supported")
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "inst_var")
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

%---------------------------------------------------------------------------%

modes_to_unify_mode(ModuleInfo, ModeX, ModeY, UnifyMode) :-
    mode_get_insts(ModuleInfo, ModeX, InitialX, FinalX),
    mode_get_insts(ModuleInfo, ModeY, InitialY, FinalY),
    UnifyMode = unify_modes_lhs_rhs(
        from_to_insts(InitialX, FinalX),
        from_to_insts(InitialY, FinalY)).

from_to_insts_to_unify_mode(FromToInstsX, FromToInstsY, UnifyMode) :-
    UnifyMode = unify_modes_lhs_rhs(FromToInstsX, FromToInstsY).

modes_to_unify_modes(_ModuleInfo, [], [], []).
modes_to_unify_modes(_ModuleInfo, [], [_ | _], _) :-
    unexpected($module, $pred, "length mismatch").
modes_to_unify_modes(_ModuleInfo, [_ | _], [], _) :-
    unexpected($module, $pred, "length mismatch").
modes_to_unify_modes(ModuleInfo,
        [ModeX | ModeXs], [ModeY | ModeYs],
        [UnifyMode | UnifyModes]) :-
    modes_to_unify_mode(ModuleInfo, ModeX, ModeY, UnifyMode),
    modes_to_unify_modes(ModuleInfo, ModeXs, ModeYs, UnifyModes).

from_to_insts_to_unify_modes([], [], []).
from_to_insts_to_unify_modes([], [_ | _], _) :-
    unexpected($module, $pred, "length mismatch").
from_to_insts_to_unify_modes([_ | _], [], _) :-
    unexpected($module, $pred, "length mismatch").
from_to_insts_to_unify_modes(
        [FromToInstsX | FromToInstsXs], [FromToInstsY | FromToInstsYs],
        [UnifyMode | UnifyModes]) :-
    from_to_insts_to_unify_mode(FromToInstsX, FromToInstsY, UnifyMode),
    from_to_insts_to_unify_modes(FromToInstsXs, FromToInstsYs, UnifyModes).

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
        ( if map.search(UserInstTable, inst_id(Name, Arity), InstDefn) then
            InstDefn = hlds_inst_defn(_VarSet, Params, InstBody, _MMTC,
                _Context, _Status),
            inst_lookup_subst_args(InstBody, Params, Name, Args, Inst)
        else
            Inst = abstract_inst(Name, Args)
        )
    ;
        InstName = typed_ground(Uniq, Type),
        map.init(Subst),
        propagate_type_into_inst(ModuleInfo, Subst, Type,
            ground(Uniq, none_or_default_func), Inst)
    ;
        InstName = typed_inst(Type, TypedInstName),
        inst_lookup(ModuleInfo, TypedInstName, Inst0),
        map.init(Subst),
        propagate_type_into_inst(ModuleInfo, Subst, Type, Inst0, Inst)
    ).

%---------------------------------------------------------------------------%

propagate_types_into_mode_list(_, [], [], []).
propagate_types_into_mode_list(ModuleInfo, [Type | Types],
        [Mode0 | Modes0], [Mode | Modes]) :-
    propagate_type_into_mode(ModuleInfo, Type, Mode0, Mode),
    propagate_types_into_mode_list(ModuleInfo, Types, Modes0, Modes).
propagate_types_into_mode_list(_, [], [_ | _], []) :-
    unexpected($module, $pred, "length mismatch").
propagate_types_into_mode_list(_, [_ | _], [], []) :-
    unexpected($module, $pred, "length mismatch").

propagate_types_into_inst_list(_, _, [], [], []).
propagate_types_into_inst_list(ModuleInfo, Subst, [Type | Types],
        [Inst0 | Insts0], [Inst | Insts]) :-
    propagate_type_into_inst(ModuleInfo, Subst, Type, Inst0, Inst),
    propagate_types_into_inst_list(ModuleInfo, Subst, Types, Insts0, Insts).
propagate_types_into_inst_list(_, _, [], [_ | _], []) :-
    unexpected($module, $pred, "length mismatch").
propagate_types_into_inst_list(_, _, [_ | _], [], []) :-
    unexpected($module, $pred, "length mismatch").

    % Given a type and a mode, produce a new mode that includes the
    % information provided by the type.
    %
:- pred propagate_type_into_mode(module_info::in, mer_type::in,
    mer_mode::in, mer_mode::out) is det.

propagate_type_into_mode(ModuleInfo, Type, Mode0, Mode) :-
    mode_get_insts(ModuleInfo, Mode0, InitialInst0, FinalInst0),
    map.init(Subst),
    propagate_type_into_inst_lazily(ModuleInfo, Subst, Type,
        InitialInst0, InitialInst),
    propagate_type_into_inst_lazily(ModuleInfo, Subst, Type,
        FinalInst0, FinalInst),
    Mode = from_to_mode(InitialInst, FinalInst).

    % Given a type, an inst and a substitution for the type variables in
    % the type, produce a new inst that includes the information
    % provided by the type.
    %
    % There are three sorts of information added:
    % 1 Module qualifiers.
    % 2 The set of constructors in the type.
    % 3 For higher-order function types (but not higher-order predicate
    %   types), the higher-order inst, i.e. the argument modes and
    %   the determinism.
    %
    % Currently #2 is not yet implemented, due to unsolved
    % efficiency problems. (See the XXX's below.)
    %
    % There are two versions, an "eager" one and a "lazy" one. In general,
    % eager expansion is to be preferred, because the expansion is done
    % just once, whereas with lazy expansion the work will be done N times.
    % However, for recursive insts we must use lazy expansion (otherwise
    % we would get infinite regress). Also, usually many of the imported
    % procedures will not be called, so for the insts in imported mode
    % declarations N is often zero.
    %
:- pred propagate_type_into_inst(module_info::in, tsubst::in, mer_type::in,
    mer_inst::in, mer_inst::out) is det.

:- pred propagate_type_into_inst_lazily(module_info::in, tsubst::in,
    mer_type::in, mer_inst::in, mer_inst::out) is det.

propagate_type_into_inst(ModuleInfo, Subst, Type0, Inst0, Inst) :-
    ( if semidet_fail then
        % XXX We ought to expand things eagerly here, using this code.
        % However, that causes efficiency problems, so for the moment
        % we always do propagation lazily.
        apply_type_subst(Type0, Subst, Type),
        ( if type_constructors(ModuleInfo, Type, Constructors) then
            propagate_ctor_info(ModuleInfo, Type, Constructors, Inst0, Inst)
        else
            Inst = Inst0
        )
    else
        propagate_ctor_info_lazily(ModuleInfo, Subst, Type0, Inst0, Inst)
    ).

propagate_type_into_inst_lazily(ModuleInfo, Subst, Type, Inst0, Inst) :-
    propagate_ctor_info_lazily(ModuleInfo, Subst, Type, Inst0, Inst).

%---------------------------------------------------------------------------%

:- pred propagate_ctor_info(module_info::in, mer_type::in,
    list(constructor)::in, mer_inst::in, mer_inst::out) is det.

propagate_ctor_info(ModuleInfo, Type, Constructors, Inst0, Inst) :-
    (
        Inst0 = free,
        % Inst = free(Type)
        Inst = free             % XXX temporary hack
    ;
        Inst0 = free(_),
        unexpected($module, $pred, "type info already present")
    ;
        Inst0 = ground(Uniq, none_or_default_func),
        ( if
            type_is_higher_order_details(Type, _, pf_function, _, ArgTypes)
        then
            default_higher_order_func_inst(ModuleInfo, ArgTypes,
                HigherOrderInstInfo),
            Inst = ground(Uniq, higher_order(HigherOrderInstInfo))
        else
            type_to_ctor_det(Type, TypeCtor),
            constructors_to_bound_insts(ModuleInfo, Uniq, TypeCtor,
                Constructors, BoundInsts0),
            list.sort_and_remove_dups(BoundInsts0, BoundInsts),
            InstResults = inst_test_results(
                inst_result_is_ground,
                inst_result_does_not_contain_any,
                inst_result_contains_inst_names_known(set.init),
                inst_result_contains_inst_vars_known(set.init),
                inst_result_contains_types_known(set.init),
                inst_result_type_ctor_propagated(TypeCtor)
            ),
            Inst = bound(Uniq, InstResults, BoundInsts)
        )
    ;
        Inst0 = any(Uniq, none_or_default_func),
        ( if
            type_is_higher_order_details(Type, _, pf_function, _, ArgTypes)
        then
            default_higher_order_func_inst(ModuleInfo, ArgTypes, PredInstInfo),
            Inst = any(Uniq, higher_order(PredInstInfo))
        else
            type_to_ctor_det(Type, TypeCtor),
            constructors_to_bound_any_insts(ModuleInfo, Uniq, TypeCtor,
                Constructors, BoundInsts0),
            list.sort_and_remove_dups(BoundInsts0, BoundInsts),
            % Normally, Inst is not ground, and contains any.
            % But if all the Ctors are constants, it is ground,
            % and does not contain any.
            InstResults = inst_test_results(
                inst_result_groundness_unknown,
                inst_result_contains_any_unknown,
                inst_result_contains_inst_names_known(set.init),
                inst_result_contains_inst_vars_known(set.init),
                inst_result_contains_types_known(set.init),
                inst_result_type_ctor_propagated(TypeCtor)
            ),
            Inst = bound(Uniq, InstResults, BoundInsts)
        )
    ;
        Inst0 = ground(Uniq, higher_order(PredInstInfo0)),
        PredInstInfo0 = pred_inst_info(PredOrFunc, Modes0, MaybeArgRegs, Det),
        ( if
            type_is_higher_order_details(Type, _, PredOrFunc, _, ArgTypes),
            list.same_length(ArgTypes, Modes0)
        then
            propagate_types_into_mode_list(ModuleInfo, ArgTypes, Modes0, Modes)
        else
            % The inst is not a valid inst for the type, so leave it alone.
            % This can only happen if the user has made a mistake. A mode
            % error should hopefully be reported if anything tries to match
            % with the inst.
            Modes = Modes0
        ),
        PredInstInfo = pred_inst_info(PredOrFunc, Modes, MaybeArgRegs, Det),
        Inst = ground(Uniq, higher_order(PredInstInfo))
    ;
        Inst0 = any(Uniq, higher_order(PredInstInfo0)),
        PredInstInfo0 = pred_inst_info(PredOrFunc, Modes0, MaybeArgRegs, Det),
        ( if
            type_is_higher_order_details(Type, _, PredOrFunc, _, ArgTypes),
            list.same_length(ArgTypes, Modes0)
        then
            propagate_types_into_mode_list(ModuleInfo, ArgTypes, Modes0, Modes)
        else
            % The inst is not a valid inst for the type, so leave it alone.
            % This can only happen if the user has made a mistake. A mode
            % error should hopefully be reported if anything tries to match
            % with the inst.
            Modes = Modes0
        ),
        PredInstInfo = pred_inst_info(PredOrFunc, Modes, MaybeArgRegs, Det),
        Inst = any(Uniq, higher_order(PredInstInfo))
    ;
        Inst0 = bound(_Uniq, _InstResult, _BoundInsts0),
        propagate_ctor_info_into_bound_inst(ModuleInfo, Type, Inst0, Inst)
    ;
        Inst0 = not_reached,
        Inst = Inst0
    ;
        Inst0 = inst_var(_),
        Inst = Inst0
    ;
        Inst0 = constrained_inst_vars(V, SubInst0),
        propagate_ctor_info(ModuleInfo, Type, Constructors, SubInst0, SubInst),
        Inst = constrained_inst_vars(V, SubInst)
    ;
        Inst0 = abstract_inst(_Name, _Args),
        Inst = Inst0                        % XXX loses info
    ;
        Inst0 = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NamedInst),
        propagate_ctor_info(ModuleInfo, Type, Constructors, NamedInst, Inst)
    ).

:- pred propagate_ctor_info_lazily(module_info::in, tsubst::in, mer_type::in,
    mer_inst::in, mer_inst::out) is det.

propagate_ctor_info_lazily(ModuleInfo, Subst, Type0, Inst0, Inst) :-
    (
        Inst0 = free,
        % Inst = free(Type0)
        Inst = free             % XXX temporary hack
    ;
        Inst0 = free(_),
        unexpected($module, $pred, "typeinfo already present")
    ;
        Inst0 = ground(Uniq, none_or_default_func),
        apply_type_subst(Type0, Subst, Type),
        ( if
            type_is_higher_order_details(Type, _, pf_function, _, ArgTypes)
        then
            default_higher_order_func_inst(ModuleInfo, ArgTypes, HOInstInfo),
            Inst = ground(Uniq, higher_order(HOInstInfo))
        else
            % XXX The information added by this is not yet used, so
            % it is disabled since it unnecessarily complicates the insts.
            %
            % Inst = defined_inst(typed_ground(Uniq, Type))
            Inst = ground(Uniq, none_or_default_func)
        )
    ;
        Inst0 = any(Uniq, none_or_default_func),
        apply_type_subst(Type0, Subst, Type),
        ( if
            type_is_higher_order_details(Type, _, pf_function, _, ArgTypes)
        then
            default_higher_order_func_inst(ModuleInfo, ArgTypes, HOInstInfo),
            Inst = any(Uniq, higher_order(HOInstInfo))
        else
            Inst = any(Uniq, none_or_default_func)
        )
    ;
        Inst0 = ground(Uniq, higher_order(PredInstInfo0)),
        PredInstInfo0 = pred_inst_info(PredOrFunc, Modes0, MaybeArgRegs, Det),
        apply_type_subst(Type0, Subst, Type),
        ( if
            type_is_higher_order_details(Type, _, PredOrFunc, _, ArgTypes),
            list.same_length(ArgTypes, Modes0)
        then
            propagate_types_into_mode_list(ModuleInfo, ArgTypes, Modes0, Modes)
        else
            % The inst is not a valid inst for the type, so leave it alone.
            % This can only happen if the user has made a mistake. A mode error
            % should hopefully be reported if anything tries to match with the
            % inst.
            Modes = Modes0
        ),
        PredInstInfo = pred_inst_info(PredOrFunc, Modes, MaybeArgRegs, Det),
        Inst = ground(Uniq, higher_order(PredInstInfo))
    ;
        Inst0 = any(Uniq, higher_order(PredInstInfo0)),
        PredInstInfo0 = pred_inst_info(PredOrFunc, Modes0, MaybeArgRegs, Det),
        apply_type_subst(Type0, Subst, Type),
        ( if
            type_is_higher_order_details(Type, _, PredOrFunc, _, ArgTypes),
            list.same_length(ArgTypes, Modes0)
        then
            propagate_types_into_mode_list(ModuleInfo, ArgTypes, Modes0, Modes)
        else
            % The inst is not a valid inst for the type, so leave it alone.
            % This can only happen if the user has made a mistake. A mode error
            % should hopefully be reported if anything tries to match with the
            % inst.
            Modes = Modes0
        ),
        PredInstInfo = pred_inst_info(PredOrFunc, Modes, MaybeArgRegs, Det),
        Inst = any(Uniq, higher_order(PredInstInfo))
    ;
        Inst0 = bound(_Uniq, _InstResult, _BoundInsts0),
        apply_type_subst(Type0, Subst, Type),
        propagate_ctor_info_into_bound_inst(ModuleInfo, Type, Inst0, Inst)
    ;
        Inst0 = not_reached,
        Inst = Inst0
    ;
        Inst0 = inst_var(_),
        Inst = Inst0
    ;
        Inst0 = constrained_inst_vars(V, SubInst0),
        propagate_ctor_info_lazily(ModuleInfo, Subst, Type0,
            SubInst0, SubInst),
        Inst = constrained_inst_vars(V, SubInst)
    ;
        Inst0 = abstract_inst(_Name, _Args),
        Inst = Inst0                        % XXX loses info
    ;
        Inst0 = defined_inst(InstName0),
        apply_type_subst(Type0, Subst, Type),
        ( if InstName0 = typed_inst(_, _) then
            % If this happens, it means that we have already lazily propagated
            % type info into this inst. We want to avoid creating insts of
            % the form typed_inst(_, typed_inst(...)), because that would be
            % unnecessary, and could cause efficiency problems or perhaps
            % even infinite loops.
            InstName = InstName0
        else
            InstName = typed_inst(Type, InstName0)
        ),
        Inst = defined_inst(InstName)
    ).

    % If the user does not explicitly specify a higher-order inst
    % for a higher-order function type, it defaults to
    % `func(in, in, ..., in) = out is det',
    % i.e. all args input, return value output, and det.
    % This applies recursively to the arguments and return
    % value too.
    %
:- pred default_higher_order_func_inst(module_info::in, list(mer_type)::in,
    pred_inst_info::out) is det.

default_higher_order_func_inst(ModuleInfo, PredArgTypes, PredInstInfo) :-
    Ground = ground(shared, none_or_default_func),
    In = from_to_mode(Ground, Ground),
    Out = from_to_mode(free, Ground),
    list.length(PredArgTypes, NumPredArgs),
    NumFuncArgs = NumPredArgs - 1,
    list.duplicate(NumFuncArgs, In, FuncArgModes),
    FuncRetMode = Out,
    list.append(FuncArgModes, [FuncRetMode], PredArgModes0),
    propagate_types_into_mode_list(ModuleInfo, PredArgTypes,
        PredArgModes0, PredArgModes),
    PredInstInfo = pred_inst_info(pf_function, PredArgModes,
        arg_reg_types_unset, detism_det).

constructors_to_bound_insts(ModuleInfo, Uniq, TypeCtor, Constructors,
        BoundInsts) :-
    constructors_to_bound_insts_loop_over_ctors(ModuleInfo, Uniq, TypeCtor,
        Constructors, ground(Uniq, none_or_default_func), BoundInsts).

constructors_to_bound_any_insts(ModuleInfo, Uniq, TypeCtor, Constructors,
        BoundInsts) :-
    constructors_to_bound_insts_loop_over_ctors(ModuleInfo, Uniq, TypeCtor,
        Constructors, any(Uniq, none_or_default_func), BoundInsts).

:- pred constructors_to_bound_insts_loop_over_ctors(module_info::in,
    uniqueness::in, type_ctor::in, list(constructor)::in, mer_inst::in,
    list(bound_inst)::out) is det.

constructors_to_bound_insts_loop_over_ctors(_, _, _, [], _, []).
constructors_to_bound_insts_loop_over_ctors(ModuleInfo, Uniq, TypeCtor,
        [Ctor | Ctors], ArgInst, [BoundInst | BoundInsts]) :-
    Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Arity, _Ctxt),
    ctor_arg_list_to_inst_list(Args, ArgInst, Insts),
    list.length(Insts, Arity),
    BoundInst = bound_functor(cons(Name, Arity, TypeCtor), Insts),
    constructors_to_bound_insts_loop_over_ctors(ModuleInfo, Uniq, TypeCtor,
        Ctors, ArgInst, BoundInsts).

:- pred ctor_arg_list_to_inst_list(list(constructor_arg)::in, mer_inst::in,
    list(mer_inst)::out) is det.

ctor_arg_list_to_inst_list([], _, []).
ctor_arg_list_to_inst_list([_ | Args], Inst, [Inst | Insts]) :-
    ctor_arg_list_to_inst_list(Args, Inst, Insts).

propagate_ctor_info_into_bound_inst(ModuleInfo, Type, Inst0, Inst) :-
    Inst0 = bound(Uniq, InstResults0, BoundInsts0),
    ( if
        type_is_tuple(Type, TupleArgTypes)
    then
        list.map(propagate_ctor_info_tuple(ModuleInfo, TupleArgTypes),
            BoundInsts0, BoundInsts),
        % Tuples don't have a *conventional* type_ctor.
        PropagatedResult = inst_result_no_type_ctor_propagated,
        ConstructNewInst = yes
    else if
        type_to_ctor_and_args(Type, TypeCtor, TypeArgs),
        TypeCtor = type_ctor(qualified(TypeModule, _), _),
        module_info_get_type_table(ModuleInfo, TypeTable),
        search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        Constructors = TypeBody ^ du_type_ctors
    then
        ( if
            InstResults0 = inst_test_results(_, _, _, _, _, PropagatedResult0),
            PropagatedResult0 =
                inst_result_type_ctor_propagated(PropagatedTypeCtor),
            PropagatedTypeCtor = TypeCtor,
            TypeParams = []
        then
            BoundInsts = BoundInsts0,
            PropagatedResult = PropagatedResult0,
            ConstructNewInst = no
        else
            map.from_corresponding_lists(TypeParams, TypeArgs, ArgSubst),
            propagate_ctor_info_into_bound_functors(ModuleInfo, ArgSubst,
                TypeCtor, TypeModule, Constructors, BoundInsts0, BoundInsts1),
            list.sort(BoundInsts1, BoundInsts),
            PropagatedResult = inst_result_type_ctor_propagated(TypeCtor),
            ConstructNewInst = yes
        )
    else
        % Builtin types don't need processing.
        BoundInsts = BoundInsts0,                                   % dummy
        PropagatedResult = inst_result_no_type_ctor_propagated,     % dummy
        ConstructNewInst = no
    ),
    % The code here would be slightly cleaner if ConstructNewInst's type
    % was maybe(list(bound_inst)), since we wouldn't have to set BoundInsts
    % and PropagatedResult if ConstructNewInst = no, but this predicate
    % is a performance bottleneck, so we want to minimize our memory
    % allocations.
    (
        ConstructNewInst = no,
        Inst = Inst0
    ;
        ConstructNewInst = yes,
        (
            BoundInsts = [],
            Inst = not_reached
        ;
            BoundInsts = [_ | _],
            (
                InstResults0 = inst_test_results_fgtc,
                InstResults = InstResults0
            ;
                InstResults0 = inst_test_no_results,
                InstResults = inst_test_results(inst_result_groundness_unknown,
                    inst_result_contains_any_unknown,
                    inst_result_contains_inst_names_unknown,
                    inst_result_contains_inst_vars_unknown,
                    inst_result_contains_types_unknown, PropagatedResult)
            ;
                InstResults0 = inst_test_results(GroundNessResult0,
                    ContainsAnyResult, _, _, _, _),
                % XXX I (zs) don't understand the predicate
                % propagate_ctor_info_into_bound_functors
                % well enough to figure out under what circumstances we could
                % keep the parts of InstResult0 we are clobbering here.
                InstResults = inst_test_results(GroundNessResult0,
                    ContainsAnyResult, inst_result_contains_inst_names_unknown,
                    inst_result_contains_inst_vars_unknown,
                    inst_result_contains_types_unknown, PropagatedResult)
            ),
            % We shouldn't need to sort BoundInsts. The cons_ids in the
            % bound_insts in the list should have been either all typed
            % or all non-typed. If they were all typed, then pushing the
            % type_ctor into them should not have modified them. If they
            % were all non-typed, then pushing the same type_ctor into them all
            % should not have changed their order.
            Inst = bound(Uniq, InstResults, BoundInsts)
        )
    ).

:- pred propagate_ctor_info_tuple(module_info::in, list(mer_type)::in,
    bound_inst::in, bound_inst::out) is det.

propagate_ctor_info_tuple(ModuleInfo, TupleArgTypes, BoundInst0, BoundInst) :-
    BoundInst0 = bound_functor(Functor, ArgInsts0),
    ( if
        Functor = tuple_cons(_),
        list.length(ArgInsts0, ArgInstsLen),
        list.length(TupleArgTypes, TupleArgTypesLen),
        ArgInstsLen = TupleArgTypesLen
    then
        map.init(Subst),
        propagate_types_into_inst_list(ModuleInfo, Subst, TupleArgTypes,
            ArgInsts0, ArgInsts)
    else
        % The bound_inst's arity does not match the tuple's arity, so leave it
        % alone. This can only happen in a user defined bound_inst.
        % A mode error should be reported if anything tries to match with
        % the inst.
        ArgInsts = ArgInsts0
    ),
    BoundInst = bound_functor(Functor, ArgInsts).

:- pred propagate_ctor_info_into_bound_functors(module_info::in, tsubst::in,
    type_ctor::in, module_name::in, list(constructor)::in,
    list(bound_inst)::in, list(bound_inst)::out) is det.

propagate_ctor_info_into_bound_functors(_, _, _, _, _, [], []).
propagate_ctor_info_into_bound_functors(ModuleInfo, Subst,
        TypeCtor, TypeModule, Constructors,
        [BoundInst0 | BoundInsts0], [BoundInst | BoundInsts]) :-
    BoundInst0 = bound_functor(ConsId0, ArgInsts0),
    ( if ConsId0 = cons(unqualified(Name), ConsArity, _ConsTypeCtor) then
        % _ConsTypeCtor should be either TypeCtor or cons_id_dummy_type_ctor.
        ConsId = cons(qualified(TypeModule, Name), ConsArity, TypeCtor)
    else
        ConsId = ConsId0
    ),
    ( if
        ConsId = cons(ConsName, Arity, _),
        find_first_matching_constructor(ConsName, Arity, Constructors,
            MatchingConstructor)
    then
        MatchingConstructor = ctor(_ExistQVars, _Constraints, _Name, Args,
            _Arity, _Ctxt),
        get_constructor_arg_types(Args, ArgTypes),
        propagate_types_into_inst_list(ModuleInfo, Subst, ArgTypes,
            ArgInsts0, ArgInsts),
        BoundInst = bound_functor(ConsId, ArgInsts)
    else
        % The cons_id is not a valid constructor for the type,
        % so leave it alone. This can only happen in a user defined
        % bound_inst. A mode error should be reported if anything
        % tries to match with the inst.
        BoundInst = bound_functor(ConsId, ArgInsts0)
    ),
    propagate_ctor_info_into_bound_functors(ModuleInfo, Subst,
        TypeCtor, TypeModule, Constructors, BoundInsts0, BoundInsts).

    % Find the first constructor in the egiven list of constructors
    % that match the given functor name and arity. Since the constructors
    % should all come from the same type definition, there should be
    % at most one matching constructor in the list anyway.
    %
:- pred find_first_matching_constructor(sym_name::in, arity::in,
    list(constructor)::in, constructor::out) is semidet.

find_first_matching_constructor(_ConsName, _Arity, [], _MatchingCtor) :-
    fail.
find_first_matching_constructor(ConsName, Arity, [Ctor | Ctors],
        MatchingCtor) :-
    ( if Ctor = ctor(_, _, ConsName, _, Arity, _) then
        MatchingCtor = Ctor
    else
        find_first_matching_constructor(ConsName, Arity, Ctors, MatchingCtor)
    ).

:- pred get_constructor_arg_types(list(constructor_arg)::in,
    list(mer_type)::out) is det.

get_constructor_arg_types([], []).
get_constructor_arg_types([Arg | Args], [ArgType | ArgTypes]) :-
    ArgType = Arg ^ arg_type,
    get_constructor_arg_types(Args, ArgTypes).

:- pred apply_type_subst(mer_type::in, tsubst::in, mer_type::out) is det.

apply_type_subst(Type0, Subst, Type) :-
    % Optimize common case.
    ( if map.is_empty(Subst) then
        Type = Type0
    else
        apply_subst_to_type(Subst, Type0, Type)
    ).

%---------------------------------------------------------------------------%

:- pred inst_lookup_subst_args(hlds_inst_body::in, list(inst_var)::in,
    sym_name::in, list(mer_inst)::in, mer_inst::out) is det.

inst_lookup_subst_args(InstBody, Params, Name, Args, Inst) :-
    (
        InstBody = eqv_inst(Inst0),
        inst_substitute_arg_list(Params, Args, Inst0, Inst)
    ;
        InstBody = abstract_inst,
        Inst = abstract_inst(Name, Args)
    ).

%---------------------------------------------------------------------------%

mode_get_insts_semidet(_ModuleInfo, from_to_mode(InitialInst, FinalInst),
        InitialInst, FinalInst).
mode_get_insts_semidet(ModuleInfo, user_defined_mode(Name, Args),
        Initial, Final) :-
    list.length(Args, Arity),
    module_info_get_mode_table(ModuleInfo, Modes),
    mode_table_get_mode_defns(Modes, ModeDefns),
    % Try looking up Name as-is. If that fails and Name is unqualified,
    % try looking it up with the builtin qualifier.
    % XXX This is a makeshift fix for a problem that requires more
    % investigation (without this fix the compiler occasionally
    % throws an exception in mode_get_insts/4).
    ( if map.search(ModeDefns, mode_id(Name, Arity), HLDS_Mode0) then
        HLDS_Mode = HLDS_Mode0
    else
        Name = unqualified(String),
        BuiltinName = qualified(mercury_public_builtin_module, String),
        map.search(ModeDefns, mode_id(BuiltinName, Arity), HLDS_Mode)
    ),
    HLDS_Mode = hlds_mode_defn(_VarSet, Params, ModeDefn, _Context, _Status),
    ModeDefn = eqv_mode(Mode0),
    mode_substitute_arg_list(Mode0, Params, Args, Mode),
    mode_get_insts_semidet(ModuleInfo, Mode, Initial, Final).

mode_get_insts(ModuleInfo, Mode, InitInst, FinalInst) :-
    ( if
        mode_get_insts_semidet(ModuleInfo, Mode, InitInstPrime, FinalInstPrime)
    then
        InitInst = InitInstPrime,
        FinalInst = FinalInstPrime
    else
        unexpected($module, $pred, "mode_get_insts_semidet failed")
    ).

mode_get_from_to_insts(ModuleInfo, Mode, FromToInsts) :-
    mode_get_insts(ModuleInfo, Mode, InitInst, FinalInst),
    FromToInsts = from_to_insts(InitInst, FinalInst).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Use the instmap deltas for all the atomic sub-goals to recompute
    % the instmap deltas for all the non-atomic sub-goals of a goal.
    % Used to ensure that the instmap deltas remain valid after
    % code has been re-arranged, e.g. by followcode.
    % After common.m has been run, it may be necessary to recompute
    % instmap deltas for atomic goals, since more outputs of calls
    % and deconstructions may become non-local (XXX does this require
    % rerunning mode analysis rather than just recompute_instmap_delta?).
    %
recompute_instmap_delta_proc(RecomputeAtomic, !ProcInfo, !ModuleInfo) :-
    proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InstMap0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes),
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_inst_varset(!.ProcInfo, InstVarSet),
    recompute_instmap_delta(RecomputeAtomic, Goal0, Goal,
        VarTypes, InstVarSet, InstMap0, !ModuleInfo),
    proc_info_set_goal(Goal, !ProcInfo).

recompute_instmap_delta(RecomputeAtomic, Goal0, Goal, VarTypes, InstVarSet,
        InstMap0, ModuleInfo0, ModuleInfo) :-
    RI0 = recompute_info(ModuleInfo0, InstVarSet),
    recompute_instmap_delta_1(RecomputeAtomic, Goal0, Goal, VarTypes,
        InstMap0, _, RI0, RI),
    ModuleInfo = RI ^ ri_module_info.

:- pred recompute_instmap_delta_1(recompute_atomic_instmap_deltas::in,
    hlds_goal::in, hlds_goal::out, vartypes::in, instmap::in,
    instmap_delta::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_1(RecomputeAtomic, Goal0, Goal, VarTypes,
        InstMap0, InstMapDelta, !RI) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ( if
        RecomputeAtomic = do_not_recompute_atomic_instmap_deltas,
        goal_expr_has_subgoals(GoalExpr0) = does_not_have_subgoals,
        not (
            GoalExpr0 = unify(_, RHS, _, _, _),
            RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _, _)
        )
        % Lambda expressions always need to be processed.
    then
        GoalExpr = GoalExpr0,
        GoalInfo1 = GoalInfo0
    else
        recompute_instmap_delta_2(RecomputeAtomic, GoalExpr0, GoalExpr,
            GoalInfo0, VarTypes, InstMap0, InstMapDelta0, !RI),
        NonLocals = goal_info_get_nonlocals(GoalInfo0),
        instmap_delta_restrict(NonLocals, InstMapDelta0, InstMapDelta1),
        goal_info_set_instmap_delta(InstMapDelta1, GoalInfo0, GoalInfo1)
    ),

    % If the initial instmap is unreachable so is the final instmap.
    ( if instmap_is_unreachable(InstMap0) then
        instmap_delta_init_unreachable(UnreachableInstMapDelta),
        goal_info_set_instmap_delta(UnreachableInstMapDelta,
            GoalInfo1, GoalInfo)
    else
        GoalInfo = GoalInfo1
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo).

:- type recompute_info
    --->    recompute_info(
                ri_module_info  :: module_info,
                ri_inst_varset  :: inst_varset
            ).

    % update_module_info(P, R, RI0, RI) will call predicate P, passing it
    % the module_info from RI0 and placing the output module_info in RI.
    % The output of P's first argument is returned in R.
    %
:- pred update_module_info(
    pred(T, module_info, module_info)::in(pred(out, in, out) is det),
    T::out, recompute_info::in, recompute_info::out) is det.

update_module_info(P, R, !RI) :-
    ModuleInfo0 = !.RI ^ ri_module_info,
    P(R, ModuleInfo0, ModuleInfo),
    !RI ^ ri_module_info := ModuleInfo.

:- pred recompute_instmap_delta_2(recompute_atomic_instmap_deltas::in,
    hlds_goal_expr::in, hlds_goal_expr::out, hlds_goal_info::in,
    vartypes::in, instmap::in, instmap_delta::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_2(RecomputeAtomic, GoalExpr0, GoalExpr, GoalInfo,
        VarTypes, InstMap0, InstMapDelta, !RI) :-
    (
        GoalExpr0 = switch(Var, Det, Cases0),
        ( if
            goal_info_has_feature(GoalInfo, feature_mode_check_clauses_goal)
        then
            Cases = Cases0,
            InstMapDelta = goal_info_get_instmap_delta(GoalInfo)
        else
            NonLocals = goal_info_get_nonlocals(GoalInfo),
            recompute_instmap_delta_cases(RecomputeAtomic, Var, Cases0, Cases,
                VarTypes, InstMap0, NonLocals, InstMapDelta, !RI)
        ),
        GoalExpr = switch(Var, Det, Cases)
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        recompute_instmap_delta_conj(RecomputeAtomic, Goals0, Goals,
            VarTypes, InstMap0, InstMapDelta, !RI),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        ( if
            goal_info_has_feature(GoalInfo, feature_mode_check_clauses_goal)
        then
            Goals = Goals0,
            InstMapDelta = goal_info_get_instmap_delta(GoalInfo)
        else
            NonLocals = goal_info_get_nonlocals(GoalInfo),
            recompute_instmap_delta_disj(RecomputeAtomic, Goals0, Goals,
                VarTypes, InstMap0, NonLocals, InstMapDelta, !RI)
        ),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = negation(SubGoal0),
        InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo),
        ( if instmap_delta_is_reachable(InstMapDelta0) then
            instmap_delta_init_reachable(InstMapDelta)
        else
            instmap_delta_init_unreachable(InstMapDelta)
        ),
        recompute_instmap_delta_1(RecomputeAtomic, SubGoal0, SubGoal, VarTypes,
            InstMap0, _, !RI),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        recompute_instmap_delta_1(RecomputeAtomic, Cond0, Cond, VarTypes,
            InstMap0, InstMapDeltaCond, !RI),
        instmap.apply_instmap_delta(InstMap0, InstMapDeltaCond, InstMapCond),
        recompute_instmap_delta_1(RecomputeAtomic, Then0, Then, VarTypes,
            InstMapCond, InstMapDeltaThen, !RI),
        recompute_instmap_delta_1(RecomputeAtomic, Else0, Else, VarTypes,
            InstMap0, InstMapDeltaElse, !RI),
        instmap_delta_apply_instmap_delta(InstMapDeltaCond, InstMapDeltaThen,
            test_size, InstMapDeltaCondThen),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        update_module_info(
            merge_instmap_delta(InstMap0, NonLocals,
                VarTypes, InstMapDeltaElse, InstMapDeltaCondThen),
            InstMapDelta, !RI),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if Reason = from_ground_term(_, FGT) then
            (
                ( FGT = from_ground_term_construct
                ; FGT = from_ground_term_deconstruct
                ),
                SubGoal = SubGoal0,
                SubGoal = hlds_goal(_, SubGoalInfo),
                InstMapDelta = goal_info_get_instmap_delta(SubGoalInfo)
            ;
                FGT = from_ground_term_initial,
                unexpected($module, $pred, "from_ground_term_initial")
            ;
                FGT = from_ground_term_other,
                recompute_instmap_delta_1(RecomputeAtomic, SubGoal0, SubGoal,
                    VarTypes, InstMap0, InstMapDelta, !RI)
            )
        else
            recompute_instmap_delta_1(RecomputeAtomic, SubGoal0, SubGoal,
                VarTypes, InstMap0, InstMapDelta, !RI)
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = generic_call(_Details, Vars, Modes, _, Detism),
        ( if determinism_components(Detism, _, at_most_zero) then
            instmap_delta_init_unreachable(InstMapDelta)
        else
            ModuleInfo = !.RI ^ ri_module_info,
            instmap_delta_from_mode_list(ModuleInfo, Vars, Modes, InstMapDelta)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = plain_call(PredId, ProcId, Args, _BI, _UC, _Name),
        recompute_instmap_delta_call(PredId, ProcId, Args, VarTypes,
            InstMap0, InstMapDelta, !RI),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = unify(LHS, RHS0, UniMode0, Uni, Context),
        GoalExpr = unify(LHS, RHS, UniMode, Uni, Context),
        (
            RHS0 = rhs_lambda_goal(Purity, Groundness, PorF, EvalMethod,
                NonLocals, LambdaVars, Modes, Det, Goal0),
            ModuleInfo0 = !.RI ^ ri_module_info,
            instmap.pre_lambda_update(ModuleInfo0, LambdaVars, Modes,
                InstMap0, InstMap),
            recompute_instmap_delta_1(RecomputeAtomic, Goal0, Goal, VarTypes,
                InstMap, _, !RI),
            RHS = rhs_lambda_goal(Purity, Groundness, PorF, EvalMethod,
                NonLocals, LambdaVars, Modes, Det, Goal)
        ;
            ( RHS0 = rhs_var(_)
            ; RHS0 = rhs_functor(_, _, _)
            ),
            RHS = RHS0
        ),
        (
            RecomputeAtomic = recompute_atomic_instmap_deltas,
            recompute_instmap_delta_unify(Uni, UniMode0, UniMode,
                GoalInfo, InstMap0, InstMapDelta, !RI)
        ;
            RecomputeAtomic = do_not_recompute_atomic_instmap_deltas,
            UniMode = UniMode0,
            InstMapDelta = goal_info_get_instmap_delta(GoalInfo)
        )
    ;
        GoalExpr0 = call_foreign_proc(_Attr, PredId, ProcId, Args, ExtraArgs,
            _MTRC, _Impl),
        ArgVars = list.map(foreign_arg_var, Args),
        recompute_instmap_delta_call(PredId, ProcId, ArgVars, VarTypes,
            InstMap0, InstMapDelta0, !RI),
        (
            ExtraArgs = [],
            InstMapDelta = InstMapDelta0
        ;
            ExtraArgs = [_ | _],
            OldInstMapDelta = goal_info_get_instmap_delta(GoalInfo),
            ExtraArgVars = list.map(foreign_arg_var, ExtraArgs),
            instmap_delta_restrict(set_of_var.list_to_set(ExtraArgVars),
                OldInstMapDelta, ExtraArgsInstMapDelta),
            instmap_delta_apply_instmap_delta(InstMapDelta0,
                ExtraArgsInstMapDelta, large_base, InstMapDelta)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            Goals0 = [MainGoal0 | OrElseGoals0],
            NonLocals = goal_info_get_nonlocals(GoalInfo),
            recompute_instmap_delta_disj(RecomputeAtomic, Goals0, Goals,
                VarTypes, InstMap0, NonLocals, InstMapDelta, !RI),
            (
                Goals = [],
                unexpected($module, $pred, "Goals = []")
            ;
                Goals = [MainGoal | OrElseGoals]
            ),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            recompute_instmap_delta_1(RecomputeAtomic, SubGoal0, SubGoal,
                VarTypes, InstMap0, InstMapDelta, !RI),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($module, $pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ).

%---------------------------------------------------------------------------%

:- pred recompute_instmap_delta_conj(recompute_atomic_instmap_deltas::in,
    list(hlds_goal)::in, list(hlds_goal)::out, vartypes::in, instmap::in,
    instmap_delta::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_conj(_, [], [], _, _, InstMapDelta, !RI) :-
    instmap_delta_init_reachable(InstMapDelta).
recompute_instmap_delta_conj(RecomputeAtomic, [Goal0 | Goals0], [Goal | Goals],
        VarTypes, InstMap0, InstMapDelta, !RI) :-
    recompute_instmap_delta_1(RecomputeAtomic, Goal0, Goal,
        VarTypes, InstMap0, InstMapDelta0, !RI),
    instmap.apply_instmap_delta(InstMap0, InstMapDelta0, InstMap1),
    recompute_instmap_delta_conj(RecomputeAtomic, Goals0, Goals,
        VarTypes, InstMap1, InstMapDelta1, !RI),
    instmap_delta_apply_instmap_delta(InstMapDelta0, InstMapDelta1,
        large_overlay, InstMapDelta).

%---------------------------------------------------------------------------%

:- pred recompute_instmap_delta_disj(recompute_atomic_instmap_deltas::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    vartypes::in, instmap::in, set_of_progvar::in, instmap_delta::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_disj(RecomputeAtomic, Goals0, Goals,
        VarTypes, InstMap, NonLocals, InstMapDelta, !RI) :-
    recompute_instmap_delta_disj_2(RecomputeAtomic, Goals0, Goals,
        VarTypes, InstMap, NonLocals, InstMapDeltas, !RI),
    (
        InstMapDeltas = [],
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        InstMapDeltas = [_ | _],
        update_module_info(
            merge_instmap_deltas(InstMap, NonLocals, VarTypes, InstMapDeltas),
            InstMapDelta, !RI)
    ).

:- pred recompute_instmap_delta_disj_2(recompute_atomic_instmap_deltas::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    vartypes::in, instmap::in, set_of_progvar::in, list(instmap_delta)::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_disj_2(_RecomputeAtomic, [], [],
        _VarTypes, _InstMap, _NonLocals, [], !RI).
recompute_instmap_delta_disj_2(RecomputeAtomic,
        [Goal0 | Goals0], [Goal | Goals], VarTypes, InstMap, NonLocals,
        [InstMapDelta | InstMapDeltas], !RI) :-
    recompute_instmap_delta_1(RecomputeAtomic, Goal0, Goal,
        VarTypes, InstMap, InstMapDelta, !RI),
    recompute_instmap_delta_disj_2(RecomputeAtomic, Goals0, Goals,
        VarTypes, InstMap, NonLocals, InstMapDeltas, !RI).

%---------------------------------------------------------------------------%

:- pred recompute_instmap_delta_cases(recompute_atomic_instmap_deltas::in,
    prog_var::in, list(case)::in, list(case)::out,
    vartypes::in, instmap::in, set_of_progvar::in, instmap_delta::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_cases(RecomputeAtomic, Var, Cases0, Cases,
        VarTypes, InstMap0, NonLocals, InstMapDelta, !RI) :-
    recompute_instmap_delta_cases_2(RecomputeAtomic, Var, Cases0, Cases,
        VarTypes, InstMap0, NonLocals, InstMapDeltas, !RI),
    (
        InstMapDeltas = [],
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        InstMapDeltas = [_ | _],
        update_module_info(
            merge_instmap_deltas(InstMap0, NonLocals, VarTypes, InstMapDeltas),
            InstMapDelta, !RI)
    ).

:- pred recompute_instmap_delta_cases_2(recompute_atomic_instmap_deltas::in,
    prog_var::in, list(case)::in,
    list(case)::out, vartypes::in, instmap::in, set_of_progvar::in,
    list(instmap_delta)::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_cases_2(_RecomputeAtomic, _Var, [], [],
        _VarTypes, _InstMap, _NonLocals, [], !RI).
recompute_instmap_delta_cases_2(RecomputeAtomic, Var,
        [Case0 | Cases0], [Case | Cases], VarTypes, InstMap0, NonLocals,
        [InstMapDelta | InstMapDeltas], !RI) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    lookup_var_type(VarTypes, Var, Type),
    update_module_info(bind_var_to_functors(Var, Type,
        MainConsId, OtherConsIds, InstMap0), InstMap1, !RI),
    recompute_instmap_delta_1(RecomputeAtomic, Goal0, Goal, VarTypes, InstMap1,
        InstMapDelta0, !RI),
    update_module_info(instmap_delta_bind_var_to_functors(Var, Type,
        MainConsId, OtherConsIds, InstMap0, InstMapDelta0), InstMapDelta, !RI),
    Case = case(MainConsId, OtherConsIds, Goal),
    recompute_instmap_delta_cases_2(RecomputeAtomic, Var, Cases0, Cases,
        VarTypes, InstMap0, NonLocals, InstMapDeltas, !RI).

%---------------------------------------------------------------------------%

:- pred recompute_instmap_delta_call(pred_id::in, proc_id::in,
    list(prog_var)::in, vartypes::in, instmap::in, instmap_delta::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_call(PredId, ProcId, Args, VarTypes, InstMap,
        InstMapDelta, !RI) :-
    ModuleInfo0 = !.RI ^ ri_module_info,
    module_info_pred_proc_info(ModuleInfo0, PredId, ProcId, _, ProcInfo),
    proc_info_interface_determinism(ProcInfo, Detism),
    ( if determinism_components(Detism, _, at_most_zero) then
        instmap_delta_init_unreachable(InstMapDelta)
    else
        proc_info_get_argmodes(ProcInfo, ArgModes0),
        proc_info_get_inst_varset(ProcInfo, ProcInstVarSet),
        InstVarSet0 = !.RI ^ ri_inst_varset,
        rename_apart_inst_vars(InstVarSet0, ProcInstVarSet, InstVarSet,
            ArgModes0, ArgModes1),
        !RI ^ ri_inst_varset := InstVarSet,
        mode_list_get_initial_insts(ModuleInfo0, ArgModes1, InitialInsts),

        % Compute the inst_var substitution from the initial insts
        % of the called procedure and the insts of the argument variables.
        ( if instmap_is_reachable(InstMap) then
            map.init(InstVarSub0),
            compute_inst_var_sub(Args, VarTypes, InstMap, InitialInsts,
                InstVarSub0, InstVarSub, ModuleInfo0, ModuleInfo1),

            % Apply the inst_var substitution to the argument modes.
            mode_list_apply_substitution(InstVarSub, ArgModes1, ArgModes2),

            % Calculate the final insts of the argument variables from their
            % initial insts and the final insts of the called procedure
            % (with inst_var substitutions applied).
            recompute_instmap_delta_call_2(Args, InstMap, ArgModes2, ArgModes,
                ModuleInfo1, ModuleInfo),
            !RI ^ ri_module_info := ModuleInfo
        else
            list.length(Args, NumArgs),
            list.duplicate(NumArgs, from_to_mode(not_reached, not_reached),
                ArgModes),
            ModuleInfo = ModuleInfo0
        ),
        instmap_delta_from_mode_list(ModuleInfo, Args, ArgModes, InstMapDelta)
    ).

:- pred compute_inst_var_sub(list(prog_var)::in, vartypes::in, instmap::in,
    list(mer_inst)::in, inst_var_sub::in, inst_var_sub::out,
    module_info::in, module_info::out) is det.

compute_inst_var_sub([], _, _, [], !Sub, !ModuleInfo).
compute_inst_var_sub([_ | _], _, _, [], !Sub, !ModuleInfo) :-
    unexpected($module, $pred, "length mismatch").
compute_inst_var_sub([], _, _, [_ | _], !Sub, !ModuleInfo) :-
    unexpected($module, $pred, "length mismatch").
compute_inst_var_sub([Arg | Args], VarTypes, InstMap, [Inst | Insts],
        !Sub, !ModuleInfo) :-
    % This is similar to modecheck_var_has_inst.
    SaveModuleInfo = !.ModuleInfo,
    SaveSub = !.Sub,
    instmap_lookup_var(InstMap, Arg, ArgInst),
    lookup_var_type(VarTypes, Arg, Type),
    ( if inst_matches_initial_sub(ArgInst, Inst, Type, !ModuleInfo, !Sub) then
        true
    else
        % error("compute_inst_var_sub: " ++
        %   ++ "inst_matches_initial failed")
        % XXX  We shouldn't ever get here, but unfortunately
        % the mode system currently has several problems (most
        % noticeably lack of alias tracking for unique modes)
        % which mean inst_matches_initial can sometimes fail here.
        !:ModuleInfo = SaveModuleInfo,
        !:Sub = SaveSub
    ),
    compute_inst_var_sub(Args, VarTypes, InstMap, Insts, !Sub, !ModuleInfo).

:- pred recompute_instmap_delta_call_2(list(prog_var)::in, instmap::in,
    list(mer_mode)::in, list(mer_mode)::out, module_info::in, module_info::out)
    is det.

recompute_instmap_delta_call_2([], _, [], [], !ModuleInfo).
recompute_instmap_delta_call_2([_ | _], _, [], _, !ModuleInfo) :-
    unexpected($module, $pred, "length mismatch").
recompute_instmap_delta_call_2([], _, [_ | _], _, !ModuleInfo) :-
    unexpected($module, $pred, "length mismatch").
recompute_instmap_delta_call_2([Arg | Args], InstMap, [Mode0 | Modes0],
        [Mode | Modes], !ModuleInfo) :-
    % This is similar to modecheck_set_var_inst.
    instmap_lookup_var(InstMap, Arg, ArgInst0),
    mode_get_insts(!.ModuleInfo, Mode0, _, FinalInst),
    ( if
        % The is_dead allows abstractly_unify_inst to succeed when
        % some parts of ArgInst0 and the corresponding parts of FinalInst
        % are free.
        % XXX There should be a better way to communicate that information.
        abstractly_unify_inst(is_dead, ArgInst0, FinalInst,
            fake_unify, UnifyInst, _, !ModuleInfo)
    then
        Mode = from_to_mode(ArgInst0, UnifyInst)
    else
        unexpected($module, $pred, "unify_inst failed")
    ),
    recompute_instmap_delta_call_2(Args, InstMap, Modes0, Modes,
        !ModuleInfo).

:- pred recompute_instmap_delta_unify(unification::in, unify_mode::in,
    unify_mode::out, hlds_goal_info::in, instmap::in, instmap_delta::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_unify(Unification, UniMode0, UniMode, GoalInfo,
        InstMap, InstMapDelta, !RI) :-
    % Deconstructions are the only types of unifications that can require
    % updating of the instmap_delta after simplify.m has been run.
    % Type specialization may require constructions of type-infos,
    % typeclass-infos or predicate constants to be added to the
    % instmap_delta.
    ModuleInfo0 = !.RI ^ ri_module_info,
    (
        Unification = deconstruct(LHSVar, _ConsId, RHSVars, ArgModes,
            _, _CanCGC),

        % Get the final inst of the deconstructed var, which will be the same
        % as in the old instmap.

        OldInstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        instmap_lookup_var(InstMap, LHSVar, LHSInitialInst),
        ( if instmap_delta_search_var(OldInstMapDelta, LHSVar, DeltaInst) then
            % Inlining can result in situations where the initial inst
            % (from procedure 1) can decide that a variable must be bound
            % to one set of function symbols, while the instmap delta from
            % a later unification (from procedure 2) can say that it is bound
            % to a different, non-overlapping set of function symbols.
            %
            % The is_dead allows abstractly_unify_inst to succeed when some
            % parts of InitialInst and the corresponding parts of DeltaInst
            % are free.
            % XXX There should be a better way to communicate that information.
            ( if
                abstractly_unify_inst(is_dead, LHSInitialInst, DeltaInst,
                    fake_unify, LHSFinalInstPrime, _Detism,
                    ModuleInfo0, ModuleInfo1)
            then
                LHSFinalInst = LHSFinalInstPrime,
                ModuleInfo = ModuleInfo1,
                !RI ^ ri_module_info := ModuleInfo
            else
                unexpected($module, $pred, "abstractly_unify_inst failed")
            )
        else
            % It wasn't in the instmap_delta, so the inst didn't change.
            LHSFinalInst = LHSInitialInst,
            ModuleInfo = ModuleInfo0
        ),
        ArgModeToRHSFromToInsts =
            ( pred(AMode::in, RHSInsts::out) is det :-
                AMode = unify_modes_lhs_rhs(_LHSInsts, RHSInsts)
            ),
        list.map(ArgModeToRHSFromToInsts, ArgModes, RHSFromToInsts),
        instmap_delta_from_from_to_insts_list(ModuleInfo, [LHSVar | RHSVars],
            [from_to_insts(LHSInitialInst, LHSFinalInst) |  RHSFromToInsts],
            InstMapDelta),
        UniMode = UniMode0
    ;
        Unification = construct(Var, ConsId, Args, _, _, _, _),
        ( if
            NonLocals = goal_info_get_nonlocals(GoalInfo),
            set_of_var.member(NonLocals, Var),
            OldInstMapDelta = goal_info_get_instmap_delta(GoalInfo),
            not instmap_delta_search_var(OldInstMapDelta, Var, _),
            MaybeInst = cons_id_to_shared_inst(ModuleInfo0, ConsId,
                list.length(Args)),
            MaybeInst = yes(Inst)
        then
            UniMode = UniMode0,
            instmap_delta_init_reachable(InstMapDelta0),
            instmap_delta_set_var(Var, Inst, InstMapDelta0, InstMapDelta)
        else
            InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
            UniMode = UniMode0
        )
    ;
        ( Unification = assign(_, _)
        ; Unification = simple_test(_, _)
        ; Unification = complicated_unify(_, _, _)
        ),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        UniMode = UniMode0
    ).

    % For a builtin constructor, return the inst of the constructed term.
    % Handling user-defined constructors properly would require running
    % mode analysis again.
    %
:- func cons_id_to_shared_inst(module_info, cons_id, int) = maybe(mer_inst).

cons_id_to_shared_inst(ModuleInfo, ConsId, NumArgs) = MaybeInst :-
    (
        ( ConsId = cons(_, _, _)
        ; ConsId = tuple_cons(_)
        ),
        MaybeInst = no
    ;
        % Note that before the change that introduced the char_const functor,
        % we used to handle character constants as user-defined cons_ids.
        ( ConsId = int_const(_)
        ; ConsId = uint_const(_)
        ; ConsId = int8_const(_)
        ; ConsId = uint8_const(_)
        ; ConsId = int16_const(_)
        ; ConsId = uint16_const(_)
        ; ConsId = int32_const(_)
        ; ConsId = uint32_const(_)
        ; ConsId = int64_const(_)
        ; ConsId = uint64_const(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ),
        MaybeInst = yes(bound(shared, inst_test_results_fgtc,
            [bound_functor(ConsId, [])]))
    ;
        ConsId = impl_defined_const(_),
        unexpected($module, $pred, "impl_defined_const")
    ;
        ConsId = closure_cons(PredProcId, _),
        module_info_pred_proc_info(ModuleInfo,
            unshroud_pred_proc_id(PredProcId), PredInfo, ProcInfo),
        PorF = pred_info_is_pred_or_func(PredInfo),
        proc_info_interface_determinism(ProcInfo, Det),
        proc_info_get_argmodes(ProcInfo, ProcArgModes),
        list.det_drop(NumArgs, ProcArgModes, Modes),
        Inst = ground(shared, higher_order(pred_inst_info(PorF, Modes,
            arg_reg_types_unset, Det))),
        MaybeInst = yes(Inst)
    ;
        ( ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ; ConsId = ground_term_const(_, _)
        ; ConsId = tabling_info_const(_)
        ; ConsId = table_io_entry_desc(_)
        ; ConsId = deep_profiling_proc_layout(_)
        ),
        MaybeInst = yes(ground(shared, none_or_default_func))
    ).

%---------------------------------------------------------------------------%

get_arg_lives(_, [], []).
get_arg_lives(ModuleInfo, [Mode | Modes], [IsLive | IsLives]) :-
    % Arguments with final inst `clobbered' are dead, any others
    % are assumed to be live.
    mode_get_insts(ModuleInfo, Mode, _InitialInst, FinalInst),
    ( if inst_is_clobbered(ModuleInfo, FinalInst) then
        IsLive = is_dead
    else
        IsLive = is_live
    ),
    get_arg_lives(ModuleInfo, Modes, IsLives).

%---------------------------------------------------------------------------%

fixup_instmap_switch_var(Var, InstMap0, InstMap, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
    instmap_lookup_var(InstMap0, Var, Inst0),
    instmap_lookup_var(InstMap, Var, Inst),
    ( if Inst = Inst0 then
        GoalInfo = GoalInfo0
    else
        instmap_delta_set_var(Var, Inst, InstMapDelta0, InstMapDelta),
        goal_info_set_instmap_delta(InstMapDelta, GoalInfo0, GoalInfo)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

normalise_insts(_, [], [], []).
normalise_insts(_, [], [_ | _], _) :-
    unexpected($module, $pred, "length mismatch").
normalise_insts(_, [_ | _], [], _) :-
    unexpected($module, $pred, "length mismatch").
normalise_insts(ModuleInfo, [Type | Types],
        [Inst0 | Insts0], [Inst | Insts]) :-
    normalise_inst(ModuleInfo, Type, Inst0, Inst),
    normalise_insts(ModuleInfo, Types, Insts0, Insts).

normalise_inst(ModuleInfo, Type, Inst0, NormalisedInst) :-
    % This is a bit of a hack.
    % The aim is to avoid non-termination due to the creation
    % of ever-expanding insts.
    % XXX should also normalise partially instantiated insts.

    inst_expand(ModuleInfo, Inst0, Inst),
    ( if Inst = bound(_, _, _) then
        ( if
            % Don't infer unique modes for introduced type_info arguments,
            % because that leads to an increase in the number of inferred modes
            % without any benefit.
            not is_introduced_type_info_type(Type),

            inst_is_ground(ModuleInfo, Inst),
            ( if inst_is_unique(ModuleInfo, Inst) then
                Uniq = unique
            else if inst_is_mostly_unique(ModuleInfo, Inst) then
                Uniq = mostly_unique
            else
                fail
            ),
            not inst_contains_nondefault_func_mode(ModuleInfo, Inst)
        then
            NormalisedInst = ground(Uniq, none_or_default_func)
        else if
            inst_is_ground(ModuleInfo, Inst),
            not inst_is_clobbered(ModuleInfo, Inst),
            not inst_contains_nondefault_func_mode(ModuleInfo, Inst)
        then
            NormalisedInst = ground(shared, none_or_default_func)
        else
            % XXX We need to limit the potential size of insts here
            % in order to avoid infinite loops in mode inference.
            NormalisedInst = Inst
        )
    else
        NormalisedInst = Inst
    ).

%---------------------------------------------------------------------------%

partition_args(_, [], [_ | _], _, _) :-
    unexpected($module, $pred, "length mismatch").
partition_args(_, [_ | _], [], _, _) :-
    unexpected($module, $pred, "length mismatch").
partition_args(_, [], [], [], []).
partition_args(ModuleInfo, [ArgMode | ArgModes], [Arg | Args],
        !:InputArgs, !:OutputArgs) :-
    partition_args(ModuleInfo, ArgModes, Args, !:InputArgs, !:OutputArgs),
    ( if mode_is_input(ModuleInfo, ArgMode) then
        !:InputArgs = [Arg | !.InputArgs]
    else
        !:OutputArgs = [Arg | !.OutputArgs]
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.mode_util.
%---------------------------------------------------------------------------%
