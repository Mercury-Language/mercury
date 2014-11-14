%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.goal_expr_to_goal.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.make_hlds.state_var.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type loc_kind
    --->    loc_whole_goal
    ;       loc_inside_atomic_goal.

    % Convert goals from the prog_data `goal' structure into the HLDS
    % `hlds_goal' structure.  At the same time,
    %
    % - convert it to super-homogeneous form by unravelling all the complex
    %   unifications, and annotate those unifications with a unify_context
    %   so that we can still give good error messages;
    % - apply the given substitution to the goal, to rename it apart
    %   from the other clauses; and
    % - expand references to state variables.
    %
:- pred transform_goal_expr_context_to_goal(loc_kind::in, goal::in,
    prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.clause_to_proc.
:- import_module check_hlds.mode_errors.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_goal.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.make_hlds.field_access.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds.superhomogeneous.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

transform_goal_expr_context_to_goal(LocKind, Goal0 - Context, Renaming, Goal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    transform_goal_expr_to_goal(LocKind, Goal0, Context, Renaming, Goal1,
        !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),
    Goal1 = hlds_goal(GoalExpr, GoalInfo1),
    goal_info_set_context(Context, GoalInfo1, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred transform_goal_expr_to_goal(loc_kind::in, goal_expr::in,
    prog_context::in, prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_goal_expr_to_goal(LocKind, Expr, Context, Renaming, Goal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    (
        (
            Expr = fail_expr,
            GoalExpr = disj([])
        ;
            Expr = true_expr,
            GoalExpr = conj(plain_conj, [])
        ),
        goal_info_init(GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        % Convert `all [Vars] Goal' into `not some [Vars] not Goal'.
        (
            Expr = all_expr(Vars0, Goal0),
            TransformedExpr = not_expr(some_expr(Vars0,
                not_expr(Goal0) - Context) - Context)
        ;
            Expr = all_state_vars_expr(StateVars, Goal0),
            TransformedExpr = not_expr(some_state_vars_expr(StateVars,
                not_expr(Goal0) - Context) - Context)
        ),
        transform_goal_expr_to_goal(LocKind, TransformedExpr, Context,
            Renaming, Goal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Expr = some_expr(Vars0, SubExpr),
        rename_var_list(need_not_rename, Renaming, Vars0, Vars),
        transform_goal_expr_context_to_goal(LocKind, SubExpr, Renaming,
            SubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        Reason = exist_quant(Vars),
        GoalExpr = scope(Reason, SubGoal),
        goal_info_init(GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Expr = some_state_vars_expr(StateVars0, SubExpr0),
        BeforeOutsideSVarState = !.SVarState,
        rename_var_list(need_not_rename, Renaming, StateVars0, StateVars),
        svar_prepare_for_local_state_vars(Context, !.VarSet, StateVars,
            BeforeOutsideSVarState, BeforeInsideSVarState, !Specs),
        transform_goal_expr_context_to_goal(LocKind, SubExpr0, Renaming,
            SubGoal, BeforeInsideSVarState, AfterInsideSVarState,
            !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        svar_finish_local_state_vars(StateVars, BeforeOutsideSVarState,
            AfterInsideSVarState, AfterOutsideSVarState),
        !:SVarState = AfterOutsideSVarState,
        Reason = exist_quant([]),
        GoalExpr = scope(Reason, SubGoal),
        goal_info_init(GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Expr = promise_purity_expr(Purity, SubExpr0),
        transform_goal_expr_context_to_goal(LocKind, SubExpr0, Renaming,
            SubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        Reason = promise_purity(Purity),
        GoalExpr = scope(Reason, SubGoal),
        goal_info_init(GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Expr = promise_equivalent_solutions_expr(Vars0, StateVars0,
            DotSVars0, ColonSVars0, SubExpr0),
        transform_promise_eqv_goal(LocKind, Vars0, StateVars0,
            DotSVars0, ColonSVars0, Context, Renaming, Vars, SubExpr0,
            SubGoal, GoalInfo, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        Reason = promise_solutions(Vars, equivalent_solutions),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Expr = promise_equivalent_solution_sets_expr(Vars0, StateVars0,
            DotSVars0, ColonSVars0, SubExpr0),
        transform_promise_eqv_goal(LocKind, Vars0, StateVars0,
            DotSVars0, ColonSVars0, Context, Renaming, Vars, SubExpr0,
            SubGoal, GoalInfo, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        GoalExpr = scope(promise_solutions(Vars, equivalent_solution_sets),
            SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Expr = promise_equivalent_solution_arbitrary_expr(Vars0, StateVars0,
            DotSVars0, ColonSVars0, SubExpr0),
        transform_promise_eqv_goal(LocKind, Vars0, StateVars0,
            DotSVars0, ColonSVars0, Context, Renaming, Vars, SubExpr0,
            SubGoal, GoalInfo, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        GoalExpr = scope(promise_solutions(Vars,
            equivalent_solution_sets_arbitrary), SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Expr = require_detism_expr(Detism, SubExpr),
        transform_goal_expr_context_to_goal(LocKind, SubExpr, Renaming,
            SubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        GoalExpr = scope(require_detism(Detism), SubGoal),
        goal_info_init(GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Expr = require_complete_switch_expr(Var0, SubExpr),
        rename_var(need_not_rename, Renaming, Var0, Var),
        transform_goal_expr_context_to_goal(LocKind, SubExpr, Renaming,
            SubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        GoalExpr = scope(require_complete_switch(Var), SubGoal),
        goal_info_init(GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Expr = require_switch_arms_detism_expr(Var0, Detism, SubExpr),
        rename_var(need_not_rename, Renaming, Var0, Var),
        transform_goal_expr_context_to_goal(LocKind, SubExpr, Renaming,
            SubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        GoalExpr = scope(require_switch_arms_detism(Var, Detism), SubGoal),
        goal_info_init(GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Expr = atomic_expr(Outer0, Inner0, MaybeOutputVars0,
            MainExpr, OrElseExprs),
        (
            MaybeOutputVars0 = no,
            MaybeOutputVars = no
        ;
            MaybeOutputVars0 = yes(OutputVars0),
            rename_var_list(need_not_rename, Renaming,
                OutputVars0, OutputVars),
            MaybeOutputVars = yes(OutputVars)
        ),
        (
            Outer0 = atomic_state_var(OuterStateVar0),
            rename_var(need_not_rename, Renaming,
                OuterStateVar0, OuterStateVar),
            svar_start_outer_atomic_scope(Context, OuterStateVar,
                OuterDI, OuterUO, OuterScopeInfo, !SVarState, !VarSet, !Specs),
            MaybeOuterScopeInfo = yes(OuterScopeInfo),
            Outer = atomic_interface_vars(OuterDI, OuterUO)
        ;
            Outer0 = atomic_var_pair(OuterDI0, OuterUO0),
            rename_var(need_not_rename, Renaming, OuterDI0, OuterDI),
            rename_var(need_not_rename, Renaming, OuterUO0, OuterUO),
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            MaybeOuterScopeInfo = no
        ),
        (
            Inner0 = atomic_state_var(InnerStateVar0),
            rename_var(need_not_rename, Renaming,
                InnerStateVar0, InnerStateVar),
            svar_start_inner_atomic_scope(Context, InnerStateVar,
                InnerScopeInfo, !SVarState, !VarSet, !Specs),
            MaybeInnerScopeInfo = yes(InnerScopeInfo)
        ;
            Inner0 = atomic_var_pair(_InnerDI0, _InnerUO0),
            MaybeInnerScopeInfo = no
        ),
        BeforeDisjSVarState = !.SVarState,
        transform_goal_expr_context_to_goal(LocKind, MainExpr, Renaming,
            HLDSMainGoal0, BeforeDisjSVarState, AfterMainSVarState,
            !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        MainDisjState =
            hlds_goal_svar_state(HLDSMainGoal0, AfterMainSVarState),
        transform_orelse_goals(LocKind, OrElseExprs, Renaming,
            OrElseDisjStates, BeforeDisjSVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        AllDisjStates = [MainDisjState | OrElseDisjStates],
        svar_finish_disjunction(Context, AllDisjStates, HLDSGoals, !VarSet,
            BeforeDisjSVarState, !:SVarState, !SVarStore),
        (
            HLDSGoals = [HLDSMainGoal | HLDSOrElseGoals]
        ;
            HLDSGoals = [],
            unexpected($module, $pred, "atomic HLDSGoals = []")
        ),
        (
            Inner0 = atomic_state_var(_),
            (
                MaybeInnerScopeInfo = yes(InnerScopeInfo2),
                svar_finish_inner_atomic_scope(Context, InnerScopeInfo2,
                    InnerDI, InnerUO, !SVarState, !VarSet, !Specs),
                Inner = atomic_interface_vars(InnerDI, InnerUO)
            ;
                MaybeInnerScopeInfo = no,
                unexpected($module, $pred, "MaybeInnerScopeInfo = no")
            )
        ;
            Inner0 = atomic_var_pair(InnerDI0, InnerUO0),
            rename_var(need_not_rename, Renaming, InnerDI0, InnerDI),
            rename_var(need_not_rename, Renaming, InnerUO0, InnerUO),
            Inner = atomic_interface_vars(InnerDI, InnerUO)
        ),
        (
            MaybeOuterScopeInfo = yes(OuterScopeInfo2),
            svar_finish_outer_atomic_scope(OuterScopeInfo2, !SVarState)
        ;
            MaybeOuterScopeInfo = no
        ),
        ShortHand = atomic_goal(unknown_atomic_goal_type, Outer, Inner,
            MaybeOutputVars, HLDSMainGoal, HLDSOrElseGoals, []),
        GoalExpr = shorthand(ShortHand),
        goal_info_init(Context, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo),
        trace [compiletime(flag("atomic_scope_syntax")), io(!IO)] (
            io.write_string("atomic:\n", !IO),
            module_info_get_globals(!.ModuleInfo, Globals),
            OutInfo = init_hlds_out_info(Globals),
            write_goal(OutInfo, Goal, !.ModuleInfo, !.VarSet, yes, 0, "\n",
                !IO),
            io.nl(!IO)
        )
    ;
        Expr = trace_expr(MaybeCompileTime, MaybeRunTime, MaybeIO,
            Mutables, SubExpr0),
        list.map4(extract_trace_mutable_var(Context, !.VarSet), Mutables,
            MutableHLDSs, MutableStateVars, MutableGetExprs, MutableSetExprs),
        (
            MaybeIO = yes(IOStateVar),
            varset.lookup_name(!.VarSet, IOStateVar, IOStateVarName),
            MaybeIOHLDS = yes(IOStateVarName),
            extract_trace_io_var(Context, IOStateVar, IOGetExpr, IOSetExpr),
            StateVars0 = [IOStateVar | MutableStateVars],
            GetExprs = [IOGetExpr | MutableGetExprs],
            SetExprs = [IOSetExpr | MutableSetExprs]
        ;
            MaybeIO = no,
            MaybeIOHLDS = no,
            StateVars0 = MutableStateVars,
            GetExprs = MutableGetExprs,
            SetExprs = MutableSetExprs
        ),
        SubExpr1 =
            goal_list_to_conj(Context, GetExprs ++ [SubExpr0] ++ SetExprs),
        BeforeSVarState = !.SVarState,
        rename_var_list(need_not_rename, Renaming, StateVars0, StateVars),
        svar_prepare_for_local_state_vars(Context, !.VarSet, StateVars,
            BeforeSVarState, BeforeInsideSVarState, !Specs),
        transform_goal_expr_context_to_goal(LocKind, SubExpr1, Renaming,
            SubGoal, BeforeInsideSVarState, AfterInsideSVarState,
            !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        svar_finish_local_state_vars(StateVars, BeforeSVarState,
            AfterInsideSVarState, AfterSVarState),
        !:SVarState = AfterSVarState,
        % The QuantVars field is a lie, but a white lie.
        Reason = trace_goal(MaybeCompileTime, MaybeRunTime, MaybeIOHLDS,
            MutableHLDSs, []),
        GoalExpr = scope(Reason, SubGoal),
        goal_info_init(GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Expr = try_expr(MaybeIO0, SubExpr0, Then0, MaybeElse0,
            Catches0, MaybeCatchAny0),
        (
            MaybeIO0 = yes(IOStateVar0),
            (
                MaybeElse0 = no,
                rename_var(need_not_rename, Renaming, IOStateVar0, IOStateVar),
                transform_try_expr_with_io(LocKind, IOStateVar0, IOStateVar,
                    SubExpr0, Then0, Catches0, MaybeCatchAny0, Context,
                    Renaming, Goal, !SVarState, !SVarStore,
                    !VarSet, !ModuleInfo, !QualInfo, !Specs)
            ;
                MaybeElse0 = yes(_),
                Pieces = [words("Error: a"), quote("try"),
                    words("goal with an"), quote("io"),
                    words("parameter cannot have an"), quote("else"),
                    words("part."), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error,
                    phase_parse_tree_to_hlds, [Msg]),
                !:Specs = [Spec | !.Specs],
                Goal = true_goal
            )
        ;
            MaybeIO0 = no,
            transform_try_expr_without_io(LocKind, SubExpr0, Then0, MaybeElse0,
                Catches0, MaybeCatchAny0, Context, Renaming, Goal,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs)
        )
    ;
        Expr = if_then_else_expr(Vars0, StateVars0, Cond0, Then0, Else0),
        BeforeSVarState = !.SVarState,
        rename_var_list(need_not_rename, Renaming, Vars0, Vars),
        rename_var_list(need_not_rename, Renaming, StateVars0, StateVars),
        svar_prepare_for_local_state_vars(Context, !.VarSet, StateVars,
            BeforeSVarState, BeforeCondSVarState, !Specs),
        transform_goal_expr_context_to_goal(LocKind, Cond0, Renaming, Cond,
            BeforeCondSVarState, AfterCondSVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs),
        transform_goal_expr_context_to_goal(LocKind, Then0, Renaming, Then1,
            AfterCondSVarState, AfterThenSVarState0, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs),
        svar_finish_local_state_vars(StateVars, BeforeSVarState,
            AfterThenSVarState0, AfterThenSVarState),
        transform_goal_expr_context_to_goal(LocKind, Else0, Renaming, Else1,
            BeforeSVarState, AfterElseSVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs),
        goal_info_init(Context, GoalInfo),
        svar_finish_if_then_else(LocKind, Context, StateVars,
            Then1, Then, Else1, Else,
            BeforeSVarState, AfterCondSVarState, AfterThenSVarState,
            AfterElseSVarState, !:SVarState, !VarSet, !SVarStore, !Specs),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Expr = not_expr(SubExpr0),
        BeforeOutsideState = !.SVarState,
        transform_goal_expr_context_to_goal(LocKind, SubExpr0, Renaming,
            SubGoal, !.SVarState, _, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs),
        !:SVarState = BeforeOutsideState,
        GoalExpr = negation(SubGoal),
        goal_info_init(GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Expr = conj_expr(A0, B0),
        get_rev_conj(LocKind, A0, Renaming, [], R0,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        get_rev_conj(LocKind, B0, Renaming, R0, R,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        L = list.reverse(R),
        goal_info_init(GoalInfo),
        conj_list_to_goal(L, GoalInfo, Goal)
    ;
        Expr = par_conj_expr(A0, B0),
        get_rev_par_conj(LocKind, A0, Renaming, [], R0,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        get_rev_par_conj(LocKind, B0, Renaming, R0, R,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        L = list.reverse(R),
        goal_info_init(GoalInfo),
        par_conj_list_to_goal(L, GoalInfo, Goal)
    ;
        Expr = disj_expr(A0, B0),
        SVarStateBefore = !.SVarState,
        get_disj(LocKind, B0, Renaming, [], DisjunctsSVarStates1,
            SVarStateBefore, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        get_disj(LocKind, A0, Renaming,
            DisjunctsSVarStates1, DisjunctsSVarStates,
            SVarStateBefore, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        svar_finish_disjunction(Context, DisjunctsSVarStates, Disjuncts,
            !VarSet, SVarStateBefore, SVarStateAfter, !SVarStore),
        !:SVarState = SVarStateAfter,
        goal_info_init(Context, GoalInfo),
        disj_list_to_goal(Disjuncts, GoalInfo, Goal)
    ;
        Expr = implies_expr(P, Q),
        % `P => Q' is defined as `not (P, not Q)'
        TransformedExpr = not_expr(conj_expr(P, not_expr(Q) - Context)
            - Context),
        transform_goal_expr_to_goal(LocKind, TransformedExpr, Context,
            Renaming, Goal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Expr = equivalent_expr(P0, Q0),
        % `P <=> Q' is defined as `(P => Q), (Q => P)',
        % but that transformation must not be done until after quantification,
        % lest the duplication of the goals concerned affect the implicit
        % quantification of the variables inside them.

        SVarStateBefore = !.SVarState,
        transform_goal_expr_context_to_goal(LocKind, P0, Renaming, P,
            !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        transform_goal_expr_context_to_goal(LocKind, Q0, Renaming, Q,
            !.SVarState, _, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        !:SVarState = SVarStateBefore,
        GoalExpr = shorthand(bi_implication(P, Q)),
        goal_info_init(GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Expr = event_expr(EventName, Args0),
        expand_bang_states(Args0, Args1),
        rename_vars_in_term_list(need_not_rename, Renaming, Args1, Args),
        make_fresh_arg_vars_subst_svars(Args, HeadVars, !VarSet,
            !SVarState, !Specs),
        list.length(HeadVars, Arity),
        list.duplicate(Arity, in_mode, Modes),
        goal_info_init(Context, GoalInfo),
        Details = event_call(EventName),
        GoalExpr0 = generic_call(Details, HeadVars, Modes, arg_reg_types_unset,
            detism_det),
        Goal0 = hlds_goal(GoalExpr0, GoalInfo),
        CallId = generic_call_id(gcid_event_call(EventName)),
        insert_arg_unifications(HeadVars, Args, Context, ac_call(CallId),
            Goal0, Goal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        svar_finish_atomic_goal(LocKind, !SVarState)
    ;
        Expr = call_expr(Name, Args0, Purity),
        expand_bang_states(Args0, Args1),
        (
            Name = unqualified("\\="),
            Args1 = [LHS, RHS]
        ->
            % `LHS \= RHS' is defined as `not (LHS = RHS)'
            TransformedExpr = not_expr(unify_expr(LHS, RHS, Purity) - Context),
            transform_goal_expr_to_goal(LocKind, TransformedExpr, Context,
                Renaming, Goal, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs)
        ;
            % check for a state var record assignment:
            % !Var ^ field := Value
            Name = unqualified(":="),
            Args1 = [LHS0, RHS0],
            LHS0 = functor(atom("^"), [StateVar0, Remainder],
                FieldListContext),
            StateVar0 = functor(atom("!"), Args @ [variable(_, _)],
                StateVarContext)
        ->
            % !Var ^ field := Value is defined as
            % !:Var = !.Var ^ field := Value.
            LHS = functor(atom("!:"), Args, StateVarContext),
            StateVar = functor(atom("!."), Args, StateVarContext),
            FieldList = functor(atom("^"), [StateVar, Remainder],
                FieldListContext),
            RHS = functor(atom(":="), [FieldList, RHS0], Context),
            TransformedExpr = unify_expr(LHS, RHS, Purity),
            transform_goal_expr_to_goal(LocKind, TransformedExpr, Context,
                Renaming, Goal, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs)
        ;
            % check for a DCG field access goal:
            % get: Field =^ field
            % set: ^ field := Field
            ( Name = unqualified(Operator) ),
            ( Operator = "=^"
            ; Operator = ":="
            )
        ->
            rename_vars_in_term_list(need_not_rename, Renaming, Args1, Args2),
            transform_dcg_record_syntax(LocKind, Operator, Args2, Context,
                Goal, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs)
        ;
            rename_vars_in_term_list(need_not_rename, Renaming, Args1, Args),
            make_fresh_arg_vars_subst_svars(Args, HeadVars, !VarSet,
                !SVarState, !Specs),
            list.length(Args, Arity),
            (
                % Check for a higher-order call,
                % i.e. a call to either call/N or ''/N.
                ( Name = unqualified("call")
                ; Name = unqualified("")
                ),
                HeadVars = [PredVar | RealHeadVars]
            ->
                % Initialize some fields to junk.
                Modes = [],
                MaybeArgRegs = arg_reg_types_unset,
                Det = detism_erroneous,

                GenericCall = higher_order(PredVar, Purity, pf_predicate,
                    Arity),
                Call = generic_call(GenericCall, RealHeadVars, Modes,
                    MaybeArgRegs, Det),

                hlds_goal.generic_call_id(GenericCall, CallId)
            ;
                % Initialize some fields to junk.
                PredId = invalid_pred_id,
                ModeId = invalid_proc_id,

                MaybeUnifyContext = no,
                Call = plain_call(PredId, ModeId, HeadVars, not_builtin,
                    MaybeUnifyContext, Name),
                CallId =
                    plain_call_id(simple_call_id(pf_predicate, Name, Arity))
            ),
            goal_info_init(Context, GoalInfo0),
            goal_info_set_purity(Purity, GoalInfo0, GoalInfo),
            Goal0 = hlds_goal(Call, GoalInfo),

            record_called_pred_or_func(pf_predicate, Name, Arity, !QualInfo),
            insert_arg_unifications(HeadVars, Args, Context, ac_call(CallId),
                Goal0, Goal, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs)
        ),
        svar_finish_atomic_goal(LocKind, !SVarState)
    ;
        Expr = unify_expr(A0, B0, Purity),
        rename_vars_in_term(need_not_rename, Renaming, A0, A),
        rename_vars_in_term(need_not_rename, Renaming, B0, B),
        % It is an error for the left or right hand side of a unification
        % to be !X (it may be !.X or !:X, however).
        ( A = functor(atom("!"), [variable(StateVarA, _)], _) ->
            report_svar_unify_error(Context, !.VarSet, StateVarA, !Specs),
            ( B = functor(atom("!"), [variable(StateVarB, _)], _) ->
                report_svar_unify_error(Context, !.VarSet, StateVarB, !Specs)
            ;
                true
            ),
            Goal = true_goal
        ; B = functor(atom("!"), [variable(StateVarB, _)], _) ->
            report_svar_unify_error(Context, !.VarSet, StateVarB, !Specs),
            Goal = true_goal
        ;
            unravel_unification(A, B, Context, umc_explicit, [], Purity, Goal,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            svar_finish_atomic_goal(LocKind, !SVarState)
        )
    ).

:- pred extract_trace_mutable_var(prog_context::in, prog_varset::in,
    trace_mutable_var::in, trace_mutable_var_hlds::out,
    prog_var::out, goal::out, goal::out) is det.

extract_trace_mutable_var(Context, VarSet, Mutable, MutableHLDS, StateVar,
        GetGoal, SetGoal) :-
    Mutable = trace_mutable_var(MutableName, StateVar),
    varset.lookup_name(VarSet, StateVar, StateVarName),
    MutableHLDS = trace_mutable_var_hlds(MutableName, StateVarName),
    GetPredName = unqualified("get_" ++ MutableName),
    SetPredName = unqualified("set_" ++ MutableName),
    SetVar = functor(atom("!:"), [variable(StateVar, Context)], Context),
    UseVar = functor(atom("!."), [variable(StateVar, Context)], Context),
    GetPurity = purity_semipure,
    SetPurity = purity_impure,
    GetGoal = call_expr(GetPredName, [SetVar], GetPurity) - Context,
    SetGoal = call_expr(SetPredName, [UseVar], SetPurity) - Context.

:- pred extract_trace_io_var(prog_context::in, prog_var::in,
    goal::out, goal::out) is det.

extract_trace_io_var(Context, StateVar, GetGoal, SetGoal) :-
    Builtin = mercury_private_builtin_module,
    GetPredName = qualified(Builtin, "trace_get_io_state"),
    SetPredName = qualified(Builtin, "trace_set_io_state"),
    SetVar = functor(atom("!:"), [variable(StateVar, Context)], Context),
    UseVar = functor(atom("!."), [variable(StateVar, Context)], Context),
    GetPurity = purity_semipure,
    SetPurity = purity_impure,
    GetGoal = call_expr(GetPredName, [SetVar], GetPurity) - Context,
    SetGoal = call_expr(SetPredName, [UseVar], SetPurity) - Context.

:- pred transform_promise_eqv_goal(loc_kind::in,
    list(prog_var)::in, list(prog_var)::in,
    list(prog_var)::in, list(prog_var)::in,
    prog_context::in, prog_var_renaming::in, list(prog_var)::out,
    goal::in, hlds_goal::out, hlds_goal_info::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_promise_eqv_goal(LocKind, Vars0, StateVars0, DotSVars0, ColonSVars0,
        Context, Renaming, QuantVars, Goal0, Goal, GoalInfo,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    rename_var_list(need_not_rename, Renaming, Vars0, Vars),
    rename_var_list(need_not_rename, Renaming, StateVars0, StateVars1),
    rename_var_list(need_not_rename, Renaming, DotSVars0, DotSVars1),
    rename_var_list(need_not_rename, Renaming, ColonSVars0, ColonSVars1),
    list.map_foldl3(lookup_dot_state_var(Context), StateVars1, OldStateVars,
        !VarSet, !SVarState, !Specs),
    list.map_foldl3(lookup_dot_state_var(Context), DotSVars1, DotSVars,
        !VarSet, !SVarState, !Specs),
    transform_goal_expr_context_to_goal(LocKind, Goal0, Renaming, Goal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    goal_info_init(GoalInfo),
    list.map_foldl3(lookup_dot_state_var(Context), StateVars1, NewStateVars,
        !VarSet, !SVarState, !Specs),
    list.map_foldl3(lookup_dot_state_var(Context), ColonSVars1, ColonSVars,
        !VarSet, !SVarState, !Specs),
    QuantVars = Vars ++ OldStateVars ++ NewStateVars ++ DotSVars ++ ColonSVars.

:- pred report_svar_unify_error(prog_context::in, prog_varset::in, svar::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_svar_unify_error(Context, VarSet, StateVar, !Specs) :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error:"), fixed("!" ++ Name),
        words("cannot appear as a unification argument."), nl,
        words("You probably meant"), fixed("!." ++ Name),
        words("or"), fixed("!:" ++ Name), suffix(".")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- inst dcg_record_syntax_op == bound("=^"; ":=").

:- pred transform_dcg_record_syntax(loc_kind::in,
    string::in(dcg_record_syntax_op), list(prog_term)::in, prog_context::in,
    hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_dcg_record_syntax(LocKind, Operator, ArgTerms0, Context, Goal,
        !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    goal_info_init(Context, GoalInfo),
    (
        ArgTerms0 = [LHSTerm, RHSTerm, TermInputTerm, TermOutputTerm],
        (
            Operator = "=^",
            AccessType = get,
            FieldNameTerm = RHSTerm,
            FieldValueTerm = LHSTerm
        ;
            Operator = ":=",
            AccessType = set,
            LHSTerm = term.functor(term.atom("^"), [FieldNameTerm0], _),
            FieldNameTerm = FieldNameTerm0,
            FieldValueTerm = RHSTerm
        )
    ->
        ContextPieces = dcg_field_error_context_pieces(AccessType),
        parse_field_list(FieldNameTerm, !.VarSet, ContextPieces,
            MaybeFieldNames),
        (
            MaybeFieldNames = ok1(FieldNames),
            ArgTerms = [FieldValueTerm, TermInputTerm, TermOutputTerm],
            transform_dcg_record_syntax_2(AccessType, FieldNames, ArgTerms,
                Context, Goal, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            svar_finish_atomic_goal(LocKind, !SVarState)
        ;
            MaybeFieldNames = error1(FieldNamesSpecs),
            !:Specs = FieldNamesSpecs ++ !.Specs,
            invalid_goal("^", ArgTerms0, GoalInfo, Goal, !VarSet,
                !SVarState, !Specs),
            qual_info_set_found_syntax_error(yes, !QualInfo)
        )
    ;
        invalid_goal("^", ArgTerms0, GoalInfo, Goal, !VarSet, !SVarState,
            !Specs),
        qual_info_set_found_syntax_error(yes, !QualInfo),
        Pieces = [words("Error: expected `Field =^ field1 ^ ... ^ fieldN'"),
            words("or `^ field1 ^ ... ^ fieldN := Field'"),
            words("in DCG field access goal."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred transform_dcg_record_syntax_2(field_access_type::in, field_list::in,
    list(prog_term)::in, prog_context::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_dcg_record_syntax_2(AccessType, FieldNames, ArgTerms, Context, Goal,
        !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    make_fresh_arg_vars_subst_svars(ArgTerms, ArgVars, !VarSet,
        !SVarState, !Specs),
    ( ArgVars = [FieldValueVar, TermInputVar, TermOutputVar] ->
        (
            AccessType = set,
            expand_set_field_function_call(Context, umc_explicit, [],
                FieldNames, FieldValueVar, TermInputVar, TermOutputVar,
                Functor, InnermostFunctor - InnermostSubContext, Goal0,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),

            FieldArgNumber = 2,
            FieldArgContext = ac_functor(InnermostFunctor, umc_explicit,
                InnermostSubContext),
            InputTermArgNumber = 1,
            InputTermArgContext = ac_functor(Functor, umc_explicit, []),
            ( Functor = cons(FuncNamePrime, FuncArityPrime, _TypeCtor) ->
                FuncName = FuncNamePrime,
                FuncArity = FuncArityPrime
            ;
                unexpected($module, $pred, "not cons")
            ),
            % DCG arguments should always be distinct variables,
            % so this context should never be used.
            OutputTermArgNumber = 3,
            SimpleCallId = simple_call_id(pf_function, FuncName, FuncArity),
            OutputTermArgContext = ac_call(plain_call_id(SimpleCallId)),

            ArgContexts = [
                FieldArgNumber - FieldArgContext,
                InputTermArgNumber - InputTermArgContext,
                OutputTermArgNumber - OutputTermArgContext
            ],
            insert_arg_unifications_with_contexts(ArgVars, ArgTerms,
                ArgContexts, Context, Goal0, Goal, !SVarState, !SVarStore,
                !VarSet, !ModuleInfo, !QualInfo, !Specs)
        ;
            AccessType = get,
            expand_dcg_field_extraction_goal(Context, umc_explicit, [],
                FieldNames, FieldValueVar, TermInputVar, TermOutputVar,
                Functor, InnermostFunctor - _InnerSubContext, Goal0,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            InputTermArgNumber = 1,
            InputTermArgContext = ac_functor(Functor, umc_explicit, []),

            ( InnermostFunctor = cons(FuncNamePrime, FuncArityPrime, _TC) ->
                FuncName = FuncNamePrime,
                FuncArity = FuncArityPrime
            ;
                unexpected($module, $pred, "not cons")
            ),
            FieldArgNumber = 2,
            SimpleCallId = simple_call_id(pf_function, FuncName, FuncArity),
            FieldArgContext = ac_call(plain_call_id(SimpleCallId)),

            % DCG arguments should always be distinct variables,
            % so this context should never be used.
            OutputTermArgNumber = 1,
            OutputTermArgContext = ac_functor(Functor, umc_explicit, []),
            ArgContexts = [
                FieldArgNumber - FieldArgContext,
                InputTermArgNumber - InputTermArgContext,
                OutputTermArgNumber - OutputTermArgContext
            ],
            insert_arg_unifications_with_contexts(ArgVars, ArgTerms,
                ArgContexts, Context, Goal0, Goal, !SVarState, !SVarStore,
                !VarSet, !ModuleInfo, !QualInfo, !Specs)
        )
    ;
        unexpected($module, $pred, "arity != 3")
    ).

    % get_rev_conj(LocKind, Goal, Renaming, RevConj0, RevConj, ...):
    %
    % Goal is a tree of conjuncts. Flatten it into a list (applying Renaming),
    % reverse it, append RevConj0, and return the result in RevConj.
    %
:- pred get_rev_conj(loc_kind::in, goal::in, prog_var_renaming::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_rev_conj(LocKind, Goal, Renaming, RevConj0, RevConj,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    ( Goal = conj_expr(A, B) - _Context ->
        get_rev_conj(LocKind, A, Renaming, RevConj0, RevConj1,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        get_rev_conj(LocKind, B, Renaming, RevConj1, RevConj,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs)
    ;
        transform_goal_expr_context_to_goal(LocKind, Goal, Renaming, Goal1,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        goal_to_conj_list(Goal1, ConjList),
        RevConj = list.reverse(ConjList) ++ RevConj0
    ).

    % get_rev_par_conj(LocKind, Goal, Renaming, RevParConj0, RevParConj, ...):
    %
    % Goal is a tree of conjuncts.  Flatten it into a list (applying Renaming),
    % reverse it, append RevParConj0, and return the result in RevParConj.
    %
:- pred get_rev_par_conj(loc_kind::in, goal::in, prog_var_renaming::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_rev_par_conj(LocKind, Goal, Renaming, RevParConj0, RevParConj,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    ( Goal = par_conj_expr(A, B) - _Context ->
        get_rev_par_conj(LocKind, A, Renaming, RevParConj0, RevParConj1,
            !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        get_rev_par_conj(LocKind, B, Renaming, RevParConj1, RevParConj,
            !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        transform_goal_expr_context_to_goal(LocKind, Goal, Renaming, Goal1,
            !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        goal_to_par_conj_list(Goal1, ParConjList),
        RevParConj = list.reverse(ParConjList) ++ RevParConj0
    ).

    % get_disj(LocKind, Goal, Renaming, Disj0, Disj, ...):
    %
    % Goal is a tree of disjuncts. Flatten it into a list (applying Renaming),
    % append Disj0, and return the result in Disj.
    %
:- pred get_disj(loc_kind::in, goal::in, prog_var_renaming::in,
    list(hlds_goal_svar_state)::in, list(hlds_goal_svar_state)::out,
    svar_state::in, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_disj(LocKind, Goal, Renaming, DisjStates0, DisjStates, SVarStateBefore,
        !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    ( Goal = disj_expr(A, B) - _Context ->
        % We recurse on the *second* arm first, so that we will put the
        % disjuncts from *that* arm at the front of DisjStates0, before
        % putting the disjuncts from the first arm at the front of the
        % resulting DisjStates1. This way, the overall result, DisjStates,
        % will have the disjuncts and their svar_infos in the correct order.
        get_disj(LocKind, B, Renaming, DisjStates0, DisjStates1,
            SVarStateBefore, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        get_disj(LocKind, A, Renaming, DisjStates1, DisjStates,
            SVarStateBefore, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        transform_goal_expr_context_to_goal(LocKind, Goal, Renaming,
            HLDSGoal, SVarStateBefore, SVarStateAfterDisjunct,
            !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        DisjState = hlds_goal_svar_state(HLDSGoal, SVarStateAfterDisjunct),
        DisjStates = [DisjState | DisjStates0]
    ).

:- pred transform_orelse_goals(loc_kind::in, list(goal)::in,
    prog_var_renaming::in, list(hlds_goal_svar_state)::out,
    svar_state::in, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_orelse_goals(_, [], _, [], _SVarStateBefore, !SVarState,
        !VarSet, !ModuleInfo, !QualInfo, !Specs).
transform_orelse_goals(LocKind, [Goal | Goals], Renaming,
        [DisjState | DisjStates], SVarStateBefore, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    transform_goal_expr_context_to_goal(LocKind, Goal, Renaming, HLDSGoal,
        SVarStateBefore, SVarStateAfterDisjunct, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),
    DisjState = hlds_goal_svar_state(HLDSGoal, SVarStateAfterDisjunct),
    transform_orelse_goals(LocKind, Goals, Renaming, DisjStates,
        SVarStateBefore, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).

%----------------------------------------------------------------------------%
%
% Try goals.
%

    % Transform a try_expr which needs to perform I/O.  The end result looks
    % like:
    %
    %   magic_exception_result(TryResult),
    %   (
    %       TryResult = succeeded({}),
    %       some [] (
    %           !:IO = !.IO,
    %           Goal
    %       ),
    %       some [] ( Then )
    %   ;
    %       TryResult = exception(Excp),
    %       ExcpHandling
    %   )
    %
    % Unlike in the non-I/O case, we have to transform the three pieces Goal,
    % Then, ExcpHandling separately then stitch them together into HLDS goals.
    % This is because we need to find out the variable for !.IO at the end of
    % Goal, before entering Then.  The variable will be used in the later
    % post-transformation.
    %
:- pred transform_try_expr_with_io(loc_kind::in, svar::in, svar::in,
    goal::in, goal::in, list(catch_expr)::in, maybe(catch_any_expr)::in,
    prog_context::in, prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_try_expr_with_io(LocKind, IOStateVarUnrenamed, IOStateVar, Goal0,
        Then0, Catches0, MaybeCatchAny0, Context, Renaming, TryGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    varset.new_named_var("TryResult", ResultVar, !VarSet),
    varset.new_var(ExcpVar, !VarSet),

    ResultVarTerm = variable(ResultVar, Context),
    ExcpVarTerm = variable(ExcpVar, Context),
    NullTupleTerm = functor(atom("{}"), [], Context),

    goal_info_init(Context, GoalInfo),

    % Make the call to magic_exception_result.
    CallMagic0 = call_expr(magic_exception_result_sym_name, [ResultVarTerm],
        purity_pure) - Context,
    transform_goal_expr_context_to_goal(LocKind, CallMagic0, Renaming,
        CallMagic, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),

    % Get the variable for !.IO before the (eventual) try_io call.
    lookup_dot_state_var(Context, IOStateVar, IOStateVarBefore,
        !VarSet, !SVarState, !Specs),

    SVarStateBeforeDisjunction = !.SVarState,

    % Build "TryResult = succeeded({})".
    ResultIsSucceededUnify0 =
        unify_expr(
            ResultVarTerm,
            exception_functor("succeeded", NullTupleTerm, Context),
            purity_pure
        ) - Context,
    transform_goal_expr_context_to_goal(LocKind, ResultIsSucceededUnify0,
        Renaming, ResultIsSucceededUnify, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),

    % Build "some [] ( !:IO = !.IO, Goal )".
    %
    % The explicit unification avoids a degenerate case where Goal doesn't bind
    % the final !:IO variable, which would lead to trouble later when we move
    % Goal into its own lambda.
    IOUnify = unify_expr(
        functor(atom("!:"), [variable(IOStateVarUnrenamed, Context)], Context),
        functor(atom("!."), [variable(IOStateVarUnrenamed, Context)], Context),
        purity_pure
    ) - Context,
    ScopedGoal0 = some_expr([], conj_expr(IOUnify, Goal0) - Context) - Context,
    transform_goal_expr_context_to_goal(LocKind, ScopedGoal0, Renaming,
        ScopedGoal, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),

    % Remember the variable for !.IO after the (eventual) try_io Goal.
    lookup_dot_state_var(Context, IOStateVar, IOStateVarAfter,
        !VarSet, !SVarState, !Specs),

    % Build "some [] ( Then )".
    ScopedThen0 = some_expr([], Then0) - Context,
    transform_goal_expr_context_to_goal(LocKind, ScopedThen0, Renaming,
        ScopedThen, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),

    % Build:
    %
    %   TryResult = succeeded({}),
    %   some [] ( !:IO = !.IO, Goal ),
    %   some [] ( Then )
    %
    conj_list_to_goal([ResultIsSucceededUnify, ScopedGoal, ScopedThen],
        GoalInfo, ResultIsSucceededDisjunct),

    SVarStateAfterResultIsSucceededDisjunct = !.SVarState,
    !:SVarState = SVarStateBeforeDisjunction,

    % Build the disjunct for "TryResult = exception(Excp), ...".
    make_exception_handling_disjunct(ResultVarTerm, ExcpVarTerm, Catches0,
        MaybeCatchAny0, Context, ResultIsExceptionDisjunct0),
    transform_goal_expr_context_to_goal(LocKind, ResultIsExceptionDisjunct0,
        Renaming, ResultIsExceptionDisjunct, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),

    SVarStateAfterResultIsExceptionDisjunct = !.SVarState,

    % Get the disjuncts.
    DisjunctSVarStates = [
        hlds_goal_svar_state(ResultIsSucceededDisjunct,
            SVarStateAfterResultIsSucceededDisjunct),
        hlds_goal_svar_state(ResultIsExceptionDisjunct,
            SVarStateAfterResultIsExceptionDisjunct)
    ],
    svar_finish_disjunction(Context, DisjunctSVarStates, Disjuncts, !VarSet,
        SVarStateBeforeDisjunction, !:SVarState, !SVarStore),
    disj_list_to_goal(Disjuncts, GoalInfo, Disjunction),

    % Build the call to magic_exception_result followed by the disjunction.
    conj_list_to_goal([CallMagic, Disjunction], GoalInfo,
        CallMagicThenDisjunction),

    IOStateVars = try_io_state_vars(IOStateVarBefore, IOStateVarAfter),
    GoalExpr = shorthand(try_goal(yes(IOStateVars), ResultVar,
        CallMagicThenDisjunction)),
    TryGoal = hlds_goal(GoalExpr, GoalInfo).

    % Transform a try_expr which does not need I/O.
    %
    % If the try goal has an else part, the end result looks like:
    %
    %   magic_exception_result(TryResult),
    %   (
    %       TryResult = succeeded({}),
    %       ( Goal ->
    %           Then
    %       ;
    %           Else
    %       )
    %   ;
    %       TryResult = exception(Excp),
    %       ExcpHandling
    %   )
    %
    % If the try goal does not have an else part, the end result looks like:
    %
    %   magic_exception_result(TryResult),
    %   (
    %       TryResult = succeeded({}),
    %       some [] ( Goal ),
    %       some [] ( Then )
    %   ;
    %       TryResult = exception(Excp),
    %       ExcpHandling
    %   )
    %
:- pred transform_try_expr_without_io(loc_kind::in, goal::in, goal::in,
    maybe(goal)::in, list(catch_expr)::in, maybe(catch_any_expr)::in,
    prog_context::in, prog_var_renaming::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_try_expr_without_io(LocKind, Goal0, Then0, MaybeElse0, Catches0,
        MaybeCatchAny0, Context, Renaming, TryGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    varset.new_named_var("TryResult", ResultVar, !VarSet),
    varset.new_var(ExcpVar, !VarSet),

    ResultVarTerm = variable(ResultVar, Context),
    ExcpVarTerm = variable(ExcpVar, Context),
    NullTupleTerm = functor(atom("{}"), [], Context),

    goal_info_init(Context, GoalInfo),

    % Build the call to magic_exception_result.
    CallMagic0 = call_expr(magic_exception_result_sym_name, [ResultVarTerm],
        purity_pure) - Context,

    % Build "TryResult = succeeded({}), ..." disjunct.
    ResultIsSucceededUnify0 =
        unify_expr(
            ResultVarTerm,
            exception_functor("succeeded", NullTupleTerm, Context),
            purity_pure
        ) - Context,
    (
        MaybeElse0 = yes(Else0),
        SucceededSubGoal =
            if_then_else_expr([], [], Goal0, Then0, Else0) - Context
    ;
        MaybeElse0 = no,
        SucceededSubGoal =
            conj_expr(
                some_expr([], Goal0) - Context,
                some_expr([], Then0) - Context
            ) - Context
    ),
    ResultIsSucceededDisjunct0 =
        conj_expr(ResultIsSucceededUnify0, SucceededSubGoal) - Context,

    % Build the disjunct for "TryResult = exception(Excp), ...".
    make_exception_handling_disjunct(ResultVarTerm, ExcpVarTerm, Catches0,
        MaybeCatchAny0, Context, ResultIsExceptionDisjunct0),

    % Build the call followed by the disjunction.
    CallMagicThenDisjunction0 =
        conj_expr(
            CallMagic0,
            disj_expr(
                ResultIsSucceededDisjunct0,
                ResultIsExceptionDisjunct0
            ) - Context
        ) - Context,
    transform_goal_expr_context_to_goal(LocKind, CallMagicThenDisjunction0,
        Renaming, CallMagicThenDisjunction, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),

    GoalExpr = shorthand(try_goal(no, ResultVar, CallMagicThenDisjunction)),
    TryGoal = hlds_goal(GoalExpr, GoalInfo).

:- pred make_exception_handling_disjunct(prog_term::in, prog_term::in,
    list(catch_expr)::in, maybe(catch_any_expr)::in, prog_context::in,
    goal::out) is det.

make_exception_handling_disjunct(ResultVarTerm, ExcpVarTerm, Catches,
        MaybeCatchAny, Context, Goal) :-
    ResultIsExceptionUnify =
        unify_expr(
            ResultVarTerm,
            exception_functor("exception", ExcpVarTerm, Context),
            purity_pure
        ) - Context,
    make_catch_ite_chain(ResultVarTerm, ExcpVarTerm, Catches, MaybeCatchAny,
        CatchChain),
    Goal = conj_expr(ResultIsExceptionUnify, CatchChain) - Context.

:- pred make_catch_ite_chain(prog_term::in, prog_term::in,
    list(catch_expr)::in, maybe(catch_any_expr)::in, goal::out) is det.

make_catch_ite_chain(ResultVarTerm, ExcpVarTerm, Catches, MaybeCatchAny,
        Goal) :-
    (
        Catches = [catch_expr(FirstPattern, FirstGoal) | RestCatches],
        make_catch_ite_chain(ResultVarTerm, ExcpVarTerm, RestCatches,
            MaybeCatchAny, ElseGoal),
        make_catch_pattern_unify_goal(FirstPattern, ExcpVarTerm,
            FirstPatternGoal),
        Goal = if_then_else_expr([], [], FirstPatternGoal, FirstGoal,
            ElseGoal) - get_term_context(FirstPattern)
    ;
        Catches = [],
        (
            MaybeCatchAny = yes(catch_any_expr(CatchAnyVar, CatchAnyGoal)),
            % With a catch_any part, end the if-then-else chain with:
            %   CatchAnyVar = exc_univ_value(Excp),
            %   CatchAnyGoal
            CatchAnyGoal = _ - Context,
            GetUnivValue = unify_expr(
                variable(CatchAnyVar, Context),
                exception_functor("exc_univ_value", ExcpVarTerm, Context),
                purity_pure) - Context,
            Goal = conj_expr(GetUnivValue, CatchAnyGoal) - Context
        ;
            MaybeCatchAny = no,
            % Without a catch_any part, end the if-then-else chain
            % by rethrowing the exception.
            Rethrow = qualified(mercury_exception_module, "rethrow"),
            Goal = call_expr(Rethrow, [ResultVarTerm], purity_pure)
                - get_term_context(ExcpVarTerm)
        )
    ).

:- pred make_catch_pattern_unify_goal(prog_term::in, prog_term::in,
    goal::out) is det.

make_catch_pattern_unify_goal(CatchPatternTerm, ExcpVarTerm, Goal) :-
    GoalExpr = call_expr(
        qualified(mercury_exception_module, "exc_univ_to_type"),
        [ExcpVarTerm, CatchPatternTerm], purity_pure),
    Goal = GoalExpr - get_term_context(CatchPatternTerm).

:- func magic_exception_result_sym_name = sym_name.

magic_exception_result_sym_name =
    qualified(mercury_exception_module, "magic_exception_result").

:- func exception_functor(string, prog_term, term.context) = prog_term.

exception_functor(Atom, Arg, Context) = Term :-
    SymName = qualified(mercury_exception_module, Atom),
    construct_qualified_term_with_context(SymName, [Arg], Context, Term).

%----------------------------------------------------------------------------%

:- func dcg_field_error_context_pieces(field_access_type) =
    list(format_component).

dcg_field_error_context_pieces(AccessType) = ContextPieces :-
    (
        AccessType = set,
        ContextPieces = [words("In DCG field update goal:"), nl]
    ;
        AccessType = get,
        ContextPieces = [words("In DCG field extraction goal:"), nl]
    ).

    % Produce an invalid goal.
    %
:- pred invalid_goal(string::in, list(prog_term)::in, hlds_goal_info::in,
    hlds_goal::out, prog_varset::in, prog_varset::out,
    svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

invalid_goal(UpdateStr, Args0, GoalInfo, Goal, !VarSet, !SVarState, !Specs) :-
    make_fresh_arg_vars_subst_svars(Args0, HeadVars, !VarSet,
        !SVarState, !Specs),
    MaybeUnifyContext = no,
    GoalExpr = plain_call(invalid_pred_id, invalid_proc_id, HeadVars,
        not_builtin, MaybeUnifyContext, unqualified(UpdateStr)),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.goal_expr_to_goal.
%----------------------------------------------------------------------------%
