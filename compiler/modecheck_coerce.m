%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: modecheck_coerce.m.
% Main author: wangp.
%
% This file contains the code to modecheck a coerce expression.
%
%---------------------------------------------------------------------------%

:- module check_hlds.modecheck_coerce.
:- interface.

:- import_module check_hlds.mode_info.
:- import_module check_hlds.modecheck_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

:- pred modecheck_coerce(list(prog_var)::in, list(prog_var)::out,
    list(mer_mode)::in, list(mer_mode)::out, determinism::out,
    extra_goals::out, mode_info::in, mode_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_test.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.modecheck_unify.
:- import_module check_hlds.type_util.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

:- type modecheck_coerce_result
    --->    coerce_mode_ok(
                list(prog_var),
                list(mer_mode),
                extra_goals
            )
    ;       coerce_mode_error.

%---------------------------------------------------------------------------%

modecheck_coerce(Args0, Args, Modes0, Modes, Det, ExtraGoals, !ModeInfo) :-
    ( if Args0 = [X, Y] then
        mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
        mode_info_get_instmap(!.ModeInfo, InstMap),
        ( if instmap_is_reachable(InstMap) then
            instmap_lookup_var(InstMap, X, InstX),
            instmap_lookup_var(InstMap, Y, InstY),
            ( if
                inst_is_ground(ModuleInfo0, InstX),
                not inst_is_clobbered(ModuleInfo0, InstX)
            then
                modecheck_coerce_vars(ModuleInfo0, X, Y, InstX, InstY, Res,
                    !ModeInfo),
                (
                    Res = coerce_mode_ok(Args, Modes, ExtraGoals),
                    Det = detism_det
                ;
                    Res = coerce_mode_error,
                    Args = [X, Y],
                    Modes = Modes0,
                    Det = detism_erroneous,
                    ExtraGoals = no_extra_goals
                )
            else
                WaitingVars = set_of_var.make_singleton(X),
                ModeError = mode_error_coerce_input_not_ground(X, InstX),
                mode_info_error(WaitingVars, ModeError, !ModeInfo),
                Args = [X, Y],
                Modes = Modes0,
                Det = detism_erroneous,
                ExtraGoals = no_extra_goals
            )
        else
            Args = Args0,
            Modes = Modes0,
            Det = detism_erroneous,
            ExtraGoals = no_extra_goals
        )
    else
        unexpected($pred, "bad coerce")
    ).

:- pred modecheck_coerce_vars(module_info::in, prog_var::in, prog_var::in,
    mer_inst::in, mer_inst::in, modecheck_coerce_result::out,
    mode_info::in, mode_info::out) is det.

modecheck_coerce_vars(ModuleInfo0, X, Y, InstX, InstY, Res, !ModeInfo) :-
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    lookup_var_type(VarTypes, X, TypeX),
    lookup_var_type(VarTypes, Y, TypeY),

    mode_info_var_is_live(!.ModeInfo, X, LiveX),
    mode_info_var_is_live(!.ModeInfo, Y, LiveY),
    ( if LiveX = is_live, LiveY = is_live then
        BothLive = is_live
    else
        BothLive = is_dead
    ),

    set.init(Seen0),
    make_bound_inst_for_type(ModuleInfo0, Seen0, TypeX, BoundInstForTypeX),
    ( if
        abstractly_unify_inst(BothLive, BoundInstForTypeX, InstX, real_unify,
            UnifyInstForTypeX, _Det, ModuleInfo0, ModuleInfo)
    then
        ( if
            check_bound_functors_in_type(ModuleInfo, TypeY,
                UnifyInstForTypeX, UnifyInstForTypeY)
        then
            mode_info_set_module_info(ModuleInfo, !ModeInfo),
            % Y aliases X, so X loses uniqueness.
            % (but if X becomes dead, Y could be unique?)
            modecheck_set_var_inst(X, UnifyInstForTypeX, no, !ModeInfo),
            ModeX = from_to_mode(InstX, UnifyInstForTypeX),
            ( if inst_is_free(ModuleInfo0, InstY) then
                % Y is free so bind the coercion result to Y.
                modecheck_set_var_inst(Y, UnifyInstForTypeY, no, !ModeInfo),
                ModeY = from_to_mode(InstY, UnifyInstForTypeY),
                Res = coerce_mode_ok([X, Y], [ModeX, ModeY], no_extra_goals)
            else
                % Y is bound so bind the coercion result to a fresh
                % variable YPrime, then unify Y = YPrime.
                create_fresh_var(TypeY, YPrime, !ModeInfo),
                create_var_var_unification(Y, YPrime, TypeY,
                    !.ModeInfo, ExtraGoal),
                ExtraGoals = extra_goals([], [ExtraGoal]),
                modecheck_set_var_inst(YPrime, UnifyInstForTypeY, no,
                    !ModeInfo),
                ModeYPrime = from_to_mode(free_inst, UnifyInstForTypeY),
                Res = coerce_mode_ok([X, YPrime], [ModeX, ModeYPrime],
                    ExtraGoals)
            )
        else
            set_of_var.init(WaitingVars),
            ModeError =
                mode_error_coerce_bad_result_inst(UnifyInstForTypeX, TypeY),
            mode_info_error(WaitingVars, ModeError, !ModeInfo),
            Res = coerce_mode_error
        )
    else
        unexpected($pred, "abstractly_unify_inst failed")
    ).

%---------------------------------------------------------------------------%

    % Produce a bound() inst covering all constructors of the du type, and
    % recursively produce bound() insts for constructor arguments. To prevent
    % infinite recursion, return ground for recursive nodes in the type tree.
    %
    % If the given type is not a du or tuple type, simply return ground.
    %
:- pred make_bound_inst_for_type(module_info::in, set(type_ctor)::in,
    mer_type::in, mer_inst::out) is det.

make_bound_inst_for_type(ModuleInfo, Seen0, Type, Inst) :-
    (
        Type = type_variable(_, _),
        Inst = ground_inst
    ;
        Type = builtin_type(_),
        Inst = ground_inst
    ;
        Type = defined_type(_SymName, _ArgTypes, _Kind),
        % XXX the seen set should probably include type arguments, not only the
        % type_ctor
        type_to_ctor_det(Type, TypeCtor),
        ( if set.insert_new(TypeCtor, Seen0, Seen) then
            % type_constructors substitutes type args into constructors.
            ( if type_constructors(ModuleInfo, Type, Constructors) then
                constructors_to_bound_insts_rec(ModuleInfo, Seen, TypeCtor,
                    Constructors, BoundInsts0),
                list.sort_and_remove_dups(BoundInsts0, BoundInsts),
                % XXX A better approximation of InstResults is probably
                % possible.
                InstResults = inst_test_results(
                    inst_result_is_ground,
                    inst_result_does_not_contain_any,
                    inst_result_contains_inst_names_unknown,
                    inst_result_contains_inst_vars_unknown,
                    inst_result_contains_types_unknown,
                    inst_result_no_type_ctor_propagated
                ),
                Inst = bound(shared, InstResults, BoundInsts)
            else
                % Type with no definition, e.g. void
                Inst = ground_inst
            )
        else
            % Does typed_ground help?
            Inst = defined_inst(typed_ground(shared, Type))
        )
    ;
        Type = tuple_type(ArgTypes, _Kind),
        list.length(ArgTypes, Arity),
        ConsId = tuple_cons(Arity),
        list.map(make_bound_inst_for_type(ModuleInfo, Seen0),
            ArgTypes, ArgInsts),
        BoundInst = bound_functor(ConsId, ArgInsts),
        % XXX A better approximation of InstResults is probably possible.
        InstResults = inst_test_results(
            inst_result_is_ground,
            inst_result_does_not_contain_any,
            inst_result_contains_inst_names_unknown,
            inst_result_contains_inst_vars_unknown,
            inst_result_contains_types_unknown,
            inst_result_no_type_ctor_propagated
        ),
        Inst = bound(shared, InstResults, [BoundInst])
    ;
        Type = higher_order_type(_PredOrFunc, _ArgTypes, _HOInstInfo, _Purity,
            _EvalMethod),
        Inst = ground_inst
    ;
        Type = apply_n_type(_, _, _),
        sorry($pred, "apply_n_type")
    ;
        Type = kinded_type(Type1, _),
        make_bound_inst_for_type(ModuleInfo, Seen0, Type1, Inst)
    ).

    % Similar to mode_util.constructors_to_bound_insts but also produces
    % bound() insts for the constructor arguments, recursively.
    %
:- pred constructors_to_bound_insts_rec(module_info::in, set(type_ctor)::in,
    type_ctor::in, list(constructor)::in, list(bound_inst)::out) is det.

constructors_to_bound_insts_rec(ModuleInfo, Seen, TypeCtor, Constructors,
        BoundInsts) :-
    constructors_to_bound_insts_loop_over_ctors(ModuleInfo, Seen, TypeCtor,
        Constructors, BoundInsts).

:- pred constructors_to_bound_insts_loop_over_ctors(module_info::in,
    set(type_ctor)::in, type_ctor::in, list(constructor)::in,
    list(bound_inst)::out) is det.

constructors_to_bound_insts_loop_over_ctors(_ModuleInfo, _Seen, _TypeCtor,
        [], []).
constructors_to_bound_insts_loop_over_ctors(ModuleInfo, Seen, TypeCtor,
        [Ctor | Ctors], [BoundInst | BoundInsts]) :-
    Ctor = ctor(_Ordinal, _MaybeExistConstraints, Name, Args, _Arity, _Ctxt),
    ctor_arg_list_to_inst_list(ModuleInfo, Seen, Args, ArgInsts),
    list.length(ArgInsts, Arity),
    BoundInst = bound_functor(cons(Name, Arity, TypeCtor), ArgInsts),
    constructors_to_bound_insts_loop_over_ctors(ModuleInfo, Seen, TypeCtor,
        Ctors, BoundInsts).

:- pred ctor_arg_list_to_inst_list(module_info::in, set(type_ctor)::in,
    list(constructor_arg)::in, list(mer_inst)::out) is det.

ctor_arg_list_to_inst_list(_ModuleInfo, _Seen, [], []).
ctor_arg_list_to_inst_list(ModuleInfo, Seen,
        [Arg | Args], [ArgInst | ArgInsts]) :-
    Arg = ctor_arg(_MaybeFieldName, ArgType, _Context),
    make_bound_inst_for_type(ModuleInfo, Seen, ArgType, ArgInst),
    ctor_arg_list_to_inst_list(ModuleInfo, Seen, Args, ArgInsts).

%---------------------------------------------------------------------------%

    % Check that a bound() inst only includes functors that are constructors of
    % the given type, recursively. Insts are otherwise assumed to be valid for
    % the type, and not checked to be valid for the type in other respects.
    %
    % On success, Inst is Inst0 (possibly expanded), with the type_ctors in
    % cons_ids replaced by the type_ctor of Type.
    %
:- pred check_bound_functors_in_type(module_info::in, mer_type::in,
    mer_inst::in, mer_inst::out) is semidet.

check_bound_functors_in_type(ModuleInfo, Type, Inst0, Inst) :-
    inst_expand(ModuleInfo, Inst0, Inst1),
    require_complete_switch [Type]
    (
        Type = type_variable(_, _),
        Inst = Inst1
    ;
        Type = builtin_type(_),
        Inst = Inst1
    ;
        Type = defined_type(_, _, _),
        check_bound_functors_in_defined_type(ModuleInfo, Type, Inst1, Inst)
    ;
        Type = tuple_type(ArgTypes, _Kind),
        check_bound_functors_in_tuple(ModuleInfo, ArgTypes, Inst1, Inst)
    ;
        Type = higher_order_type(_PredOrFunc, ArgTypes, _HOInstInfo, _Purity,
            _EvalMethod),
        check_bound_functors_in_higher_order_type(ModuleInfo, ArgTypes,
            Inst1, Inst)
    ;
        Type = apply_n_type(_, _, _),
        fail
    ;
        Type = kinded_type(Type1, _Kind),
        check_bound_functors_in_type(ModuleInfo, Type1, Inst1, Inst)
    ).

%---------------------%

:- pred check_bound_functors_in_defined_type(module_info::in, mer_type::in,
    mer_inst::in, mer_inst::out) is semidet.

check_bound_functors_in_defined_type(ModuleInfo, Type, Inst0, Inst) :-
    require_complete_switch [Inst0]
    (
        Inst0 = bound(Uniq, _InstResults0, BoundInsts0),
        type_to_ctor(Type, TypeCtor),
        % type_constructors substitutes type args into constructors.
        type_constructors(ModuleInfo, Type, Constructors),
        list.map(
            check_bound_functor_in_du_type(ModuleInfo, TypeCtor, Constructors),
            BoundInsts0, BoundInsts),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(Uniq, inst_test_no_results, BoundInsts)
    ;
        Inst0 = ground(_Uniq, _HOInstInfo),
        Inst = Inst0
    ;
        Inst0 = constrained_inst_vars(InstVars, SubInst0),
        check_bound_functors_in_defined_type(ModuleInfo, Type,
            SubInst0, SubInst),
        Inst = constrained_inst_vars(InstVars, SubInst)
    ;
        ( Inst0 = free
        ; Inst0 = free(_)
        ; Inst0 = any(_, _)
        ; Inst0 = not_reached
        ; Inst0 = inst_var(_)
        ; Inst0 = abstract_inst(_, _)
        ),
        fail
    ;
        Inst0 = defined_inst(_),
        unexpected($pred, "unexpanded inst")
    ).

:- pred check_bound_functor_in_du_type(module_info::in, type_ctor::in,
    list(constructor)::in, bound_inst::in, bound_inst::out) is semidet.

check_bound_functor_in_du_type(ModuleInfo, TypeCtor, Ctors,
        BoundInst0, BoundInst) :-
    BoundInst0 = bound_functor(ConsId0, ArgInsts0),
    ConsId0 = cons(SymName0, Arity, _TypeCtor0),
    Name = unqualify_name(SymName0),
    find_first_matching_constructor_unqual(Name, Arity, Ctors, MatchingCtor),
    MatchingCtor = ctor(_, _, SymName, CtorArgs, _, _),
    ConsId = cons(SymName, Arity, TypeCtor),
    check_bound_functors_in_ctor_args(ModuleInfo, CtorArgs,
        ArgInsts0, ArgInsts),
    BoundInst = bound_functor(ConsId, ArgInsts).

:- pred find_first_matching_constructor_unqual(string::in, int::in,
    list(constructor)::in, constructor::out) is semidet.

find_first_matching_constructor_unqual(Name, Arity, [Ctor | Ctors],
        MatchingCtor) :-
    ( if
        Ctor = ctor(_, _, ConsName, _, Arity, _),
        unqualify_name(ConsName) = Name
    then
        MatchingCtor = Ctor
    else
        find_first_matching_constructor_unqual(Name, Arity, Ctors, MatchingCtor)
    ).

:- pred check_bound_functors_in_ctor_args(module_info::in,
    list(constructor_arg)::in, list(mer_inst)::in, list(mer_inst)::out)
    is semidet.

check_bound_functors_in_ctor_args(_, [], [], []).
check_bound_functors_in_ctor_args(ModuleInfo, [CtorArg | CtorArgs],
        [ArgInst0 | ArgInsts0], [ArgInst | ArgInsts]) :-
    check_bound_functors_in_ctor_arg(ModuleInfo, CtorArg, ArgInst0, ArgInst),
    check_bound_functors_in_ctor_args(ModuleInfo, CtorArgs,
        ArgInsts0, ArgInsts).

:- pred check_bound_functors_in_ctor_arg(module_info::in, constructor_arg::in,
    mer_inst::in, mer_inst::out) is semidet.

check_bound_functors_in_ctor_arg(ModuleInfo, CtorArg, ArgInst0, ArgInst) :-
    CtorArg = ctor_arg(_MaybeFieldName, ArgType, _Context),
    check_bound_functors_in_type(ModuleInfo, ArgType, ArgInst0, ArgInst).

%---------------------%

:- pred check_bound_functors_in_tuple(module_info::in, list(mer_type)::in,
    mer_inst::in, mer_inst::out) is semidet.

check_bound_functors_in_tuple(ModuleInfo, ArgTypes, Inst0, Inst) :-
    require_complete_switch [Inst0]
    (
        Inst0 = bound(Uniq, _InstTestsResults, BoundInsts0),
        list.map(bound_check_bound_functors_in_tuple(ModuleInfo, ArgTypes),
            BoundInsts0, BoundInsts),
        Inst = bound(Uniq, inst_test_no_results, BoundInsts)
    ;
        Inst0 = ground(_Uniq, _HOInstInfo),
        Inst = Inst0
    ;
        Inst0 = constrained_inst_vars(_, _),
        sorry($pred, "constrained_inst_vars")
    ;
        ( Inst0 = free
        ; Inst0 = free(_)
        ; Inst0 = any(_, _)
        ; Inst0 = not_reached
        ; Inst0 = inst_var(_)
        ; Inst0 = abstract_inst(_, _)
        ),
        fail
    ;
        Inst0 = defined_inst(_),
        unexpected($pred, "unexpanded inst")
    ).

:- pred bound_check_bound_functors_in_tuple(module_info::in,
    list(mer_type)::in, bound_inst::in, bound_inst::out) is semidet.

bound_check_bound_functors_in_tuple(ModuleInfo, ArgTypes,
        BoundInst0, BoundInst) :-
    BoundInst0 = bound_functor(ConsId, ArgInsts0),
    list.length(ArgTypes, Arity),
    ConsId = tuple_cons(Arity),
    check_bound_functors_in_tuple_args(ModuleInfo, ArgTypes,
        ArgInsts0, ArgInsts),
    BoundInst = bound_functor(ConsId, ArgInsts).

:- pred check_bound_functors_in_tuple_args(module_info::in, list(mer_type)::in,
    list(mer_inst)::in, list(mer_inst)::out) is semidet.

check_bound_functors_in_tuple_args(_ModuleInfo, [], [], []).
check_bound_functors_in_tuple_args(ModuleInfo,
        [Type | Types], [Inst0 | Insts0], [Inst | Insts]) :-
    check_bound_functors_in_type(ModuleInfo, Type, Inst0, Inst),
    check_bound_functors_in_tuple_args(ModuleInfo, Types, Insts0, Insts).

%---------------------%

:- pred check_bound_functors_in_higher_order_type(module_info::in,
    list(mer_type)::in, mer_inst::in, mer_inst::out) is semidet.

check_bound_functors_in_higher_order_type(_ModuleInfo, _ArgTypes,
        Inst0, Inst) :-
    require_complete_switch [Inst0]
    (
        Inst0 = ground(_Uniq, _HOInstInfo),
        Inst = Inst0
    ;
        Inst0 = bound(_Uniq, _InstTestsResults, _BoundInsts),
        % XXX is this reachable?
        sorry($pred, "bound inst")
    ;
        Inst0 = constrained_inst_vars(_, _),
        sorry($pred, "constrained_inst_vars")
    ;
        ( Inst0 = free
        ; Inst0 = free(_)
        ; Inst0 = any(_, _)
        ; Inst0 = not_reached
        ; Inst0 = inst_var(_)
        ; Inst0 = abstract_inst(_, _)
        ),
        fail
    ;
        Inst0 = defined_inst(_),
        unexpected($pred, "unexpanded inst")
    ).

%---------------------------------------------------------------------------%

:- pred create_fresh_var(mer_type::in, prog_var::out,
    mode_info::in, mode_info::out) is det.

create_fresh_var(VarType, Var, !ModeInfo) :-
    mode_info_get_var_types(!.ModeInfo, VarTypes0),
    mode_info_get_varset(!.ModeInfo, VarSet0),
    varset.new_var(Var, VarSet0, VarSet),
    add_var_type(Var, VarType, VarTypes0, VarTypes),
    mode_info_set_varset(VarSet, !ModeInfo),
    mode_info_set_var_types(VarTypes, !ModeInfo).

%---------------------------------------------------------------------------%
:- end_module check_hlds.modecheck_coerce.
%---------------------------------------------------------------------------%
