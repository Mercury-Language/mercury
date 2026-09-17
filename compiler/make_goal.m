%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2015, 2018-2019, 2021-2024, 2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make_hlds.m.
%
% This module defines predicates that construct HLDS goals.
%
%---------------------------------------------------------------------------%

:- module hlds.make_goal.

:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_proc.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.var_table.

:- import_module char.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Return the HLDS equivalent of `true'.
    %
:- func true_goal_expr = hlds_goal_expr.
:- func true_goal(prog_context) = hlds_goal.

    % Return the HLDS equivalent of `fail'.
    %
:- func fail_goal_expr = hlds_goal_expr.
:- func fail_goal_info(prog_context) = hlds_goal_info.
:- func fail_goal(prog_context) = hlds_goal.

%---------------------------------------------------------------------------%

    % Create the hlds_goal for a unification, filling in all the as yet
    % unknown slots with dummy values. The unification is constructed as a
    % complicated unification; turning it into some other kind of unification,
    % if appropriate is left to mode analysis. Therefore this predicate
    % shouldn't be used unless you know mode analysis will be run on its
    % output.
    %
:- pred create_atomic_complicated_unification(prog_var::in, unify_rhs::in,
    prog_context::in, unify_main_context::in, list(unify_sub_context)::in,
    purity::in, hlds_goal::out) is det.

    % As above, but with default purity pure.
    %
:- pred create_pure_atomic_complicated_unification(prog_var::in, unify_rhs::in,
    prog_context::in, unify_main_context::in, list(unify_sub_context)::in,
    hlds_goal::out) is det.

%---------------------------------------------------------------------------%

:- pred make_complicated_unify_assigns(prog_context::in,
    list(prog_var)::in, list(prog_var)::in, list(hlds_goal)::out) is det.

:- pred make_complicated_unify_assign(prog_context::in,
    prog_var::in, prog_var::in, hlds_goal::out) is det.

%---------------------------------------------------------------------------%

    % Create the hlds_goal for a unification that assigns the second variable
    % to the first. The initial inst of the second variable should be
    % ground_inst. The resulting goal has all its fields filled in.
    %
:- pred make_simple_assign(prog_var::in, prog_var::in,
    prog_context::in, unify_main_context::in, list(unify_sub_context)::in,
    hlds_goal::out) is det.

    % Create the hlds_goal for a unification that tests the equality of two
    % values of atomic types. The resulting goal has all its fields filled in.
    %
:- pred make_simple_test(prog_var::in, prog_var::in,
    prog_context::in, unify_main_context::in, list(unify_sub_context)::in,
    hlds_goal::out) is det.

%---------------------------------------------------------------------------%

    % Produce a goal to construct a given constant. These predicates all
    % fill in the non-locals, instmap_delta and determinism fields of the
    % goal_info of the returned goal. With alias tracking, the instmap_delta
    % will be correct only if the variable being assigned to has no aliases.
    %
    % The cons_id passed to make_const_construction must be fully module
    % qualified.
    %
:- pred make_int_const_construction(prog_context::in,
    prog_var::in, int::in, hlds_goal::out) is det.
:- pred make_string_const_construction(prog_context::in,
    prog_var::in, string::in, hlds_goal::out) is det.
:- pred make_float_const_construction(prog_context::in,
    prog_var::in, float::in, hlds_goal::out) is det.
:- pred make_char_const_construction(prog_context::in,
    prog_var::in, char::in, hlds_goal::out) is det.
:- pred make_const_construction(prog_context::in,
    prog_var::in, cons_id::in, hlds_goal::out) is det.

%---------------------------------------------------------------------------%

:- pred make_int_const_construction_alloc_in_proc(prog_context::in,
    int::in, string::in, hlds_goal::out, prog_var::out,
    proc_info::in, proc_info::out) is det.
:- pred make_string_const_construction_alloc_in_proc(prog_context::in,
    string::in, string::in, hlds_goal::out, prog_var::out,
    proc_info::in, proc_info::out) is det.
:- pred make_float_const_construction_alloc_in_proc(prog_context::in,
    float::in, string::in, hlds_goal::out, prog_var::out,
    proc_info::in, proc_info::out) is det.
:- pred make_char_const_construction_alloc_in_proc(prog_context::in,
    char::in, string::in, hlds_goal::out, prog_var::out,
    proc_info::in, proc_info::out) is det.
:- pred make_const_construction_alloc_in_proc(prog_context::in,
    cons_id::in, mer_type::in, is_dummy_type::in, string::in,
    hlds_goal::out, prog_var::out, proc_info::in, proc_info::out) is det.

%---------------------------------------------------------------------------%

:- pred make_int_const_construction_alloc(prog_context::in, int::in,
    string::in, hlds_goal::out, prog_var::out,
    var_table::in, var_table::out) is det.
:- pred make_string_const_construction_alloc(prog_context::in, string::in,
    string::in, hlds_goal::out, prog_var::out,
    var_table::in, var_table::out) is det.
:- pred make_float_const_construction_alloc(prog_context::in, float::in,
    string::in, hlds_goal::out, prog_var::out,
    var_table::in, var_table::out) is det.
:- pred make_char_const_construction_alloc(prog_context::in, char::in,
    string::in, hlds_goal::out, prog_var::out,
    var_table::in, var_table::out) is det.
:- pred make_const_construction_alloc(prog_context::in, cons_id::in,
    mer_type::in, is_dummy_type::in, string::in, hlds_goal::out, prog_var::out,
    var_table::in, var_table::out) is det.

%---------------------------------------------------------------------------%

    % Produce a goal to construct or deconstruct a unification with a functor.
    % It fills in the non-locals, instmap_delta and determinism fields
    % of the goal_info.
    %
:- pred construct_functor(prog_context::in, prog_var::in, cons_id::in,
    list(prog_var)::in, hlds_goal::out) is det.
:- pred deconstruct_functor(prog_context::in, prog_var::in, cons_id::in,
    list(prog_var)::in, hlds_goal::out) is det.

%---------------------------------------------------------------------------%

    % Produce a goal to construct or deconstruct a tuple containing
    % the given list of arguments, filling in the non-locals,
    % instmap_delta and determinism fields of the goal_info.
    %
:- pred construct_tuple(prog_context::in, prog_var::in, list(prog_var)::in,
    hlds_goal::out) is det.
:- pred deconstruct_tuple(prog_context::in, prog_var::in, list(prog_var)::in,
    hlds_goal::out) is det.

%---------------------------------------------------------------------------%

    % generate_plain_call(ModuleInfo, PredOrFunc, ModuleName, ProcName,
    %   TIArgVars, ArgVars, InstMapDelta, ModeNo, Detism, Purity, Features,
    %   Context, CallGoal):
    %
    % Generate a call to a builtin procedure (e.g. from the private_builtin
    % or table_builtin module). This is used by HLDS->HLDS transformation
    % passes that introduce calls to builtin procedures.
    %
    % If ModeNo = only_mode, then the predicate must have exactly one
    % procedure; an error is raised if this is not the case.
    %
    % If ModeNo = mode_no(N) then the Nth procedure is used, counting from 0.
    %
:- pred generate_plain_call(module_info::in, pred_or_func::in,
    module_name::in, string::in, list(prog_var)::in, list(prog_var)::in,
    instmap_delta::in, mode_no::in, determinism::in, purity::in,
    list(goal_feature)::in, prog_context::in, hlds_goal::out) is det.

    % generate_call_foreign_proc(ModuleInfo, PredOrFunc, ModuleName, ProcName,
    %   TIArgs, Args, ExtraArgs, InstMapDelta, ModeNo, Detism, Purity,
    %   Features, Attributes, MaybeTraceRuntimeCond, Code, Context, CallGoal):
    %
    % generate_call_foreign_proc is similar to generate_plain_call,
    % but also assumes that the called predicate is defined via a
    % foreign_proc, that the foreign_proc's arguments are as given in
    % TIArgs and Args, its attributes are Attributes, and its code is Code.
    % As well as returning a foreign_code instead of a call, effectively
    % inlining the call, generate_call_foreign_proc also passes ExtraArgs
    % as well as TIArgs and Args.
    %
:- pred generate_call_foreign_proc(module_info::in, pred_or_func::in,
    module_name::in, string::in, list(foreign_arg)::in, list(foreign_arg)::in,
    list(foreign_arg)::in, instmap_delta::in, mode_no::in,
    determinism::in, purity::in, list(goal_feature)::in,
    foreign_proc_attributes::in, maybe(trace_expr(trace_runtime))::in,
    string::in, prog_context::in, hlds_goal::out) is det.

    % Generate a cast goal. The input and output insts are just ground.
    %
:- pred generate_cast(cast_kind::in, prog_var::in, prog_var::in,
    prog_context::in, hlds_goal::out) is det.

    % This version takes input and output inst arguments, which may be
    % necessary when casting, say, solver type values with inst any,
    % or casting between enumeration types and ints.
    %
:- pred generate_cast_with_insts(cast_kind::in, prog_var::in, prog_var::in,
    mer_inst::in, mer_inst::in, prog_context::in, hlds_goal::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_pred_tests.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.pred_proc_id.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module pair.
:- import_module require.
:- import_module term_context.

%---------------------------------------------------------------------------%

true_goal_expr = conj(plain_conj, []).

:- func true_goal_info(prog_context) = hlds_goal_info.

true_goal_info(Context) = GoalInfo :-
    instmap_delta_init_reachable(InstMapDelta),
    goal_info_init(set_of_var.init, InstMapDelta, detism_det, purity_pure,
        Context, GoalInfo).

true_goal(Context) = Goal :-
    Goal = hlds_goal(true_goal_expr, true_goal_info(Context)).

fail_goal_expr = disj([]).

fail_goal_info(Context) = GoalInfo :-
    instmap_delta_init_unreachable(InstMapDelta),
    goal_info_init(set_of_var.init, InstMapDelta, detism_failure, purity_pure,
        Context, GoalInfo).

fail_goal(Context) = Goal :-
    Goal = hlds_goal(fail_goal_expr, fail_goal_info(Context)).

%---------------------------------------------------------------------------%

create_atomic_complicated_unification(LHS, RHS, Context,
        UnifyMainContext, UnifySubContext, Purity, Goal) :-
    UnifyMode = unify_modes_li_lf_ri_rf(free, free, free, free),
    Unification = complicated_unify(UnifyMode, can_fail, []),
    UnifyContext = unify_context(UnifyMainContext, UnifySubContext),
    goal_info_init_context_purity(Context, Purity, GoalInfo),
    GoalExpr = unify(LHS, RHS, UnifyMode, Unification, UnifyContext),
    Goal = hlds_goal(GoalExpr, GoalInfo).

create_pure_atomic_complicated_unification(LHS, RHS, Context,
        UnifyMainContext, UnifySubContext, Goal) :-
    create_atomic_complicated_unification(LHS, RHS, Context,
        UnifyMainContext, UnifySubContext, purity_pure, Goal).

%---------------------------------------------------------------------------%

make_complicated_unify_assigns(_, [], [_ | _], _) :-
    unexpected($pred, "length mismatch").
make_complicated_unify_assigns(_, [_ | _], [], _) :-
    unexpected($pred, "length mismatch").
make_complicated_unify_assigns(_, [], [], []).
make_complicated_unify_assigns(Context, [Var1 | Vars1], [Var2 | Vars2],
        [Goal | Goals]) :-
    make_complicated_unify_assign(Context, Var1, Var2, Goal),
    make_complicated_unify_assigns(Context, Vars1, Vars2, Goals).

make_complicated_unify_assign(Context, Var1, Var2, Goal) :-
    ( if Var1 = Var2 then
        Goal = true_goal(Context)
    else
        create_pure_atomic_complicated_unification(Var1, rhs_var(Var2),
            Context, umc_explicit, [], Goal)
    ).

%---------------------------------------------------------------------------%

make_simple_assign(X, Y, Context, UnifyMainContext, UnifySubContext, Goal) :-
    Ground = ground(shared, none_or_default_func),
    UnifyMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
    Unification = assign(X, Y),
    UnifyContext = unify_context(UnifyMainContext, UnifySubContext),
    goal_info_init(set_of_var.list_to_set([X, Y]), instmap_delta_bind_var(X),
        detism_det, purity_pure, Context, GoalInfo),
    GoalExpr = unify(X, rhs_var(Y), UnifyMode, Unification, UnifyContext),
    Goal = hlds_goal(GoalExpr, GoalInfo).

make_simple_test(X, Y, Context, UnifyMainContext, UnifySubContext, Goal) :-
    Ground = ground(shared, none_or_default_func),
    UnifyMode = unify_modes_li_lf_ri_rf(Ground, Ground, Ground, Ground),
    Unification = simple_test(X, Y),
    UnifyContext = unify_context(UnifyMainContext, UnifySubContext),
    goal_info_init(set_of_var.list_to_set([X, Y]), instmap_delta_bind_no_var,
        detism_semi, purity_pure, Context, GoalInfo),
    GoalExpr = unify(X, rhs_var(Y), UnifyMode, Unification, UnifyContext),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%---------------------------------------------------------------------------%

make_int_const_construction(Context, Var, Int, Goal) :-
    ConsId = some_int_const(int_const(Int)),
    make_const_construction(Context, Var, ConsId, Goal).

make_string_const_construction(Context, Var, String, Goal) :-
    make_const_construction(Context, Var, string_const(String), Goal).

make_float_const_construction(Context, Var, Float, Goal) :-
    make_const_construction(Context, Var, float_const(Float), Goal).

make_char_const_construction(Context, Var, Char, Goal) :-
    make_const_construction(Context, Var, char_const(Char), Goal).

make_const_construction(Context, Var, ConsId, Goal) :-
    RHS = rhs_functor(ConsId, is_not_exist_constr, []),
    Inst = bound(unique, inst_test_results_fgtc, [bound_functor(ConsId, [])]),
    UnifyMode = unify_modes_li_lf_ri_rf(free, Inst, Inst, Inst),
    Unification = construct(Var, ConsId, [], [],
        construct_dynamically, cell_is_unique, no_construct_sub_info),
    UnifyContext = unify_context(umc_explicit, []),
    GoalExpr = unify(Var, RHS, UnifyMode, Unification, UnifyContext),
    NonLocals = set_of_var.make_singleton(Var),
    instmap_delta_init_reachable(InstMapDelta0),
    instmap_delta_insert_var(Var, Inst, InstMapDelta0, InstMapDelta),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, Context,
        GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%---------------------------------------------------------------------------%

make_int_const_construction_alloc_in_proc(Context, Int, Name, Goal, Var,
        !ProcInfo) :-
    proc_info_create_var_from_type(Name, int_type, is_not_dummy_type,
        Var, !ProcInfo),
    make_int_const_construction(Context, Var, Int, Goal).

make_string_const_construction_alloc_in_proc(Context, String, Name, Goal, Var,
        !ProcInfo) :-
    proc_info_create_var_from_type(Name, string_type, is_not_dummy_type,
        Var, !ProcInfo),
    make_string_const_construction(Context, Var, String, Goal).

make_float_const_construction_alloc_in_proc(Context, Float, Name, Goal, Var,
        !ProcInfo) :-
    proc_info_create_var_from_type(Name, float_type, is_not_dummy_type,
        Var, !ProcInfo),
    make_float_const_construction(Context, Var, Float, Goal).

make_char_const_construction_alloc_in_proc(Context, Char, Name, Goal, Var,
        !ProcInfo) :-
    proc_info_create_var_from_type(Name, char_type, is_not_dummy_type, Var,
        !ProcInfo),
    make_char_const_construction(Context, Var, Char, Goal).

make_const_construction_alloc_in_proc(Context, ConsId, Type, IsDummy, Name,
        Goal, Var, !ProcInfo) :-
    proc_info_create_var_from_type(Name, Type, IsDummy, Var, !ProcInfo),
    make_const_construction(Context, Var, ConsId, Goal).

%---------------------------------------------------------------------------%

make_int_const_construction_alloc(Context, Int, Name, Goal, Var,
        !VarTable) :-
    Entry = vte(Name, int_type, is_not_dummy_type),
    add_var_entry(Entry, Var, !VarTable),
    make_int_const_construction(Context, Var, Int, Goal).

make_string_const_construction_alloc(Context, String, Name, Goal, Var,
        !VarTable) :-
    Entry = vte(Name, string_type, is_not_dummy_type),
    add_var_entry(Entry, Var, !VarTable),
    make_string_const_construction(Context, Var, String, Goal).

make_float_const_construction_alloc(Context, Float, Name, Goal, Var,
        !VarTable) :-
    Entry = vte(Name, float_type, is_not_dummy_type),
    add_var_entry(Entry, Var, !VarTable),
    make_float_const_construction(Context, Var, Float, Goal).

make_char_const_construction_alloc(Context, Char, Name, Goal, Var,
        !VarTable) :-
    Entry = vte(Name, char_type, is_not_dummy_type),
    add_var_entry(Entry, Var, !VarTable),
    make_char_const_construction(Context, Var, Char, Goal).

make_const_construction_alloc(Context, ConsId, Type, IsDummyType, Name,
        Goal, Var, !VarTable) :-
    Entry = vte(Name, Type, IsDummyType),
    add_var_entry(Entry, Var, !VarTable),
    make_const_construction(Context, Var, ConsId, Goal).

%---------------------------------------------------------------------------%

construct_functor(Context, Var, ConsId, Args, Goal) :-
    list.length(Args, Arity),
    RHS = rhs_functor(ConsId, is_not_exist_constr, Args),
    UnifyMode = unify_modes_li_lf_ri_rf(free_inst, ground_inst,
        ground_inst, ground_inst),
    list.duplicate(Arity, UnifyMode, ArgModes),
    Unification = construct(Var, ConsId, Args, ArgModes,
        construct_dynamically, cell_is_unique, no_construct_sub_info),
    UnifyContext = unify_context(umc_explicit, []),
    Unify = unify(Var, RHS, UnifyMode, Unification, UnifyContext),
    set_of_var.list_to_set([Var | Args], NonLocals),
    InstMapDelta = instmap_delta_bind_var(Var),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure,
        Context, GoalInfo),
    Goal = hlds_goal(Unify, GoalInfo).

deconstruct_functor(Context, Var, ConsId, Args, Goal) :-
    list.length(Args, Arity),
    RHS = rhs_functor(ConsId, is_not_exist_constr, Args),
    UnifyMode = unify_modes_li_lf_ri_rf(ground_inst, ground_inst,
        free_inst, ground_inst),
    list.duplicate(Arity, UnifyMode, ArgModes),
    UnifyContext = unify_context(umc_explicit, []),
    Unification = deconstruct(Var, ConsId, Args, ArgModes, cannot_fail,
        cannot_cgc),
    Unify = unify(Var, RHS, UnifyMode, Unification, UnifyContext),
    set_of_var.list_to_set([Var | Args], NonLocals),
    InstMapDelta = instmap_delta_bind_vars(Args),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure,
        Context, GoalInfo),
    Goal = hlds_goal(Unify, GoalInfo).

%---------------------------------------------------------------------------%

construct_tuple(Context, Tuple, Args, Goal) :-
    list.length(Args, Arity),
    ConsId = tuple_cons(Arity),
    construct_functor(Context, Tuple, ConsId, Args, Goal).

deconstruct_tuple(Context, Tuple, Args, Goal) :-
    list.length(Args, Arity),
    ConsId = tuple_cons(Arity),
    deconstruct_functor(Context, Tuple, ConsId, Args, Goal).

%---------------------------------------------------------------------------%

generate_plain_call(ModuleInfo, PredOrFunc, ModuleName, ProcName,
        TIArgVars, NonTIArgVars, InstMapDelta0, ModeNo, Detism, Purity,
        Features, Context, Goal) :-
    PredFormArity = arg_list_arity(NonTIArgVars),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    lookup_builtin_pred_proc_id(ModuleInfo, ModuleName, ProcName,
        PredOrFunc, UserArity, ModeNo, PredId, ProcId),

    % builtin_state only uses this to work out whether
    % this is the "recursive" clause generated for the compiler
    % for each builtin, so an invalid pred_id won't cause problems.
    InvalidPredId = invalid_pred_id,
    BuiltinState = pred_builtin_state(ModuleInfo, InvalidPredId,
        PredId, ProcId),

    ArgVars = TIArgVars ++ NonTIArgVars,
    GoalExpr = plain_call(PredId, ProcId, ArgVars, BuiltinState, no,
        qualified(ModuleName, ProcName)),
    set_of_var.list_to_set(ArgVars, NonLocals),
    determinism_components(Detism, _CanFail, NumSolns),
    (
        NumSolns = at_most_zero,
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        ( NumSolns = at_most_one
        ; NumSolns = at_most_many
        ; NumSolns = at_most_many_cc
        ),
        InstMapDelta = InstMapDelta0
    ),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_purity(PredInfo, PredPurity),
    expect(unify(Purity, PredPurity), $pred, "purity disagreement"),
    goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context,
        GoalInfo0),
    list.foldl(goal_info_add_feature, Features, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

generate_call_foreign_proc(ModuleInfo, PredOrFunc, ModuleName, ProcName,
        TIArgs, NonTIArgs, ExtraArgs, InstMapDelta0, ModeNo, Detism, Purity,
        Features, Attributes, MaybeTraceRuntimeCond, Code, Context, Goal) :-
    PredFormArity = arg_list_arity(NonTIArgs),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    lookup_builtin_pred_proc_id(ModuleInfo, ModuleName, ProcName,
        PredOrFunc, UserArity, ModeNo, PredId, ProcId),

    Args = TIArgs ++ NonTIArgs,
    GoalExpr = call_foreign_proc(Attributes, PredId, ProcId, Args, ExtraArgs,
        MaybeTraceRuntimeCond, fp_impl_ordinary(Code, no)),
    ArgVars = list.map(foreign_arg_var, Args),
    ExtraArgVars = list.map(foreign_arg_var, ExtraArgs),
    Vars = ArgVars ++ ExtraArgVars,
    set_of_var.list_to_set(Vars, NonLocals),
    determinism_components(Detism, _CanFail, NumSolns),
    (
        NumSolns = at_most_zero,
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        ( NumSolns = at_most_one
        ; NumSolns = at_most_many
        ; NumSolns = at_most_many_cc
        ),
        InstMapDelta = InstMapDelta0
    ),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_purity(PredInfo, PredPurity),
    expect(unify(Purity, PredPurity), $pred, "purity disagreement"),
    goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context,
        GoalInfo0),
    list.foldl(goal_info_add_feature, Features, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

generate_cast(CastType, InArg, OutArg, Context, Goal) :-
    Ground = ground_inst,
    generate_cast_with_insts(CastType, InArg, OutArg, Ground, Ground, Context,
        Goal).

generate_cast_with_insts(CastType, InArg, OutArg, InInst, OutInst, Context,
        Goal) :-
    set_of_var.list_to_set([InArg, OutArg], NonLocals),
    InstMapDelta = instmap_delta_from_assoc_list([OutArg - OutInst]),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, Context,
        GoalInfo),
    GoalExpr = generic_call(cast(CastType), [InArg, OutArg],
        [in_mode(InInst), out_mode(OutInst)], arg_reg_types_unset, detism_det),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%---------------------------------------------------------------------------%
:- end_module hlds.make_goal.
%---------------------------------------------------------------------------%
