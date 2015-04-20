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
    % `hlds_goal' structure. At the same time,
    %
    % - convert it to super-homogeneous form by unravelling all the complex
    %   unifications, and annotate those unifications with a unify_context
    %   so that we can still give good error messages;
    % - apply the given substitution to the goal, to rename it apart
    %   from the other clauses; and
    % - expand references to state variables.
    %
:- pred transform_parse_tree_goal_to_hlds(loc_kind::in, goal::in,
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
:- import_module cord.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

transform_parse_tree_goal_to_hlds(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    (
        (
            Goal = fail_expr(Context),
            GoalExpr = disj([])
        ;
            Goal = true_expr(Context),
            GoalExpr = conj(plain_conj, [])
        ),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        % Convert `all [Vars] SubGoal' into `not (some [Vars] (not SubGoal))'.
        (
            Goal = all_expr(Context, Vars, SubGoal),
            TransformedGoal =
                not_expr(Context,
                    some_expr(Context, Vars,
                        not_expr(Context, SubGoal)))
        ;
            Goal = all_state_vars_expr(Context, StateVars, SubGoal),
            TransformedGoal =
                not_expr(Context,
                    some_state_vars_expr(Context, StateVars,
                        not_expr(Context, SubGoal)))
        ),
        transform_parse_tree_goal_to_hlds(LocKind, TransformedGoal, Renaming,
            HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = some_expr(Context, Vars0, SubGoal),
        rename_var_list(need_not_rename, Renaming, Vars0, Vars),
        transform_parse_tree_goal_to_hlds(LocKind, SubGoal, Renaming,
            HLDSSubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        Reason = exist_quant(Vars),
        GoalExpr = scope(Reason, HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = some_state_vars_expr(Context, StateVars0, SubGoal0),
        BeforeOutsideSVarState = !.SVarState,
        rename_var_list(need_not_rename, Renaming, StateVars0, StateVars),
        svar_prepare_for_local_state_vars(Context, !.VarSet, StateVars,
            BeforeOutsideSVarState, BeforeInsideSVarState, !Specs),
        transform_parse_tree_goal_to_hlds(LocKind, SubGoal0, Renaming,
            HLDSSubGoal, BeforeInsideSVarState, AfterInsideSVarState,
            !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        svar_finish_local_state_vars(StateVars, BeforeOutsideSVarState,
            AfterInsideSVarState, AfterOutsideSVarState),
        !:SVarState = AfterOutsideSVarState,
        Reason = exist_quant([]),
        GoalExpr = scope(Reason, HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = promise_purity_expr(Context, Purity, SubGoal),
        transform_parse_tree_goal_to_hlds(LocKind, SubGoal, Renaming,
            HLDSSubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        Reason = promise_purity(Purity),
        GoalExpr = scope(Reason, HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = promise_equivalent_solutions_expr(Context, Vars, StateVars,
            DotSVars, ColonSVars, SubGoal),
        transform_promise_eqv_goal(LocKind, Vars, StateVars,
            DotSVars, ColonSVars, Context, Renaming, PromiseVars,
            SubGoal, HLDSSubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        Reason = promise_solutions(PromiseVars, equivalent_solutions),
        GoalExpr = scope(Reason, HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = promise_equivalent_solution_sets_expr(Context,
            Vars, StateVars, DotSVars, ColonSVars, SubGoal),
        transform_promise_eqv_goal(LocKind, Vars, StateVars,
            DotSVars, ColonSVars, Context, Renaming, PromiseVars,
            SubGoal, HLDSSubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        Reason = promise_solutions(PromiseVars, equivalent_solution_sets),
        GoalExpr = scope(Reason, HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = promise_equivalent_solution_arbitrary_expr(Context,
            Vars, StateVars, DotSVars, ColonSVars, SubGoal),
        transform_promise_eqv_goal(LocKind, Vars, StateVars,
            DotSVars, ColonSVars, Context, Renaming, PromiseVars,
            SubGoal, HLDSSubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        Reason =
            promise_solutions(PromiseVars, equivalent_solution_sets_arbitrary),
        GoalExpr = scope(Reason, HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = require_detism_expr(Context, Detism, SubGoal),
        transform_parse_tree_goal_to_hlds(LocKind, SubGoal, Renaming,
            HLDSSubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        GoalExpr = scope(require_detism(Detism), HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = require_complete_switch_expr(Context, Var0, SubGoal),
        rename_var(need_not_rename, Renaming, Var0, Var),
        transform_parse_tree_goal_to_hlds(LocKind, SubGoal, Renaming,
            HLDSSubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        GoalExpr = scope(require_complete_switch(Var), HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = require_switch_arms_detism_expr(Context, Var0, Detism,
            SubGoal),
        rename_var(need_not_rename, Renaming, Var0, Var),
        transform_parse_tree_goal_to_hlds(LocKind, SubGoal, Renaming,
            HLDSSubGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        GoalExpr = scope(require_switch_arms_detism(Var, Detism), HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = atomic_expr(Context, Outer0, Inner0, MaybeOutputVars0,
            MainGoal, OrElseGoals),
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
        transform_parse_tree_goal_to_hlds(LocKind, MainGoal, Renaming,
            HLDSMainGoal0, BeforeDisjSVarState, AfterMainSVarState,
            !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        MainDisjState =
            hlds_goal_svar_state(HLDSMainGoal0, AfterMainSVarState),
        transform_orelse_goals(LocKind, OrElseGoals, Renaming,
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
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo),
        trace [compiletime(flag("atomic_scope_syntax")), io(!IO)] (
            io.write_string("atomic:\n", !IO),
            module_info_get_globals(!.ModuleInfo, Globals),
            OutInfo = init_hlds_out_info(Globals, output_debug),
            write_goal(OutInfo, HLDSGoal, !.ModuleInfo, !.VarSet, yes, 0, "\n",
                !IO),
            io.nl(!IO)
        )
    ;
        Goal = trace_expr(Context, MaybeCompileTime, MaybeRunTime, MaybeIO,
            Mutables, SubGoal0),
        list.map4(extract_trace_mutable_var(Context, !.VarSet), Mutables,
            MutableHLDSs, MutableStateVars, MutableGetGoals, MutableSetGoals),
        (
            MaybeIO = yes(IOStateVar),
            varset.lookup_name(!.VarSet, IOStateVar, IOStateVarName),
            MaybeIOHLDS = yes(IOStateVarName),
            extract_trace_io_var(Context, IOStateVar, IOGetGoal, IOSetGoal),
            StateVars0 = [IOStateVar | MutableStateVars],
            GetGoals = [IOGetGoal | MutableGetGoals],
            SetGoals = [IOSetGoal | MutableSetGoals]
        ;
            MaybeIO = no,
            MaybeIOHLDS = no,
            StateVars0 = MutableStateVars,
            GetGoals = MutableGetGoals,
            SetGoals = MutableSetGoals
        ),
        SubGoal1 =
            goal_list_to_conj(Context, GetGoals ++ [SubGoal0] ++ SetGoals),
        BeforeSVarState = !.SVarState,
        rename_var_list(need_not_rename, Renaming, StateVars0, StateVars),
        svar_prepare_for_local_state_vars(Context, !.VarSet, StateVars,
            BeforeSVarState, BeforeInsideSVarState, !Specs),
        transform_parse_tree_goal_to_hlds(LocKind, SubGoal1, Renaming,
            HLDSSubGoal, BeforeInsideSVarState, AfterInsideSVarState,
            !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        svar_finish_local_state_vars(StateVars, BeforeSVarState,
            AfterInsideSVarState, AfterSVarState),
        !:SVarState = AfterSVarState,
        % The QuantVars field is a lie, but a white lie.
        Reason = trace_goal(MaybeCompileTime, MaybeRunTime, MaybeIOHLDS,
            MutableHLDSs, []),
        GoalExpr = scope(Reason, HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = try_expr(Context, MaybeIO0, SubGoal0, Then0, MaybeElse0,
            Catches0, MaybeCatchAny0),
        (
            MaybeIO0 = yes(IOStateVar0),
            (
                MaybeElse0 = no,
                rename_var(need_not_rename, Renaming, IOStateVar0, IOStateVar),
                transform_try_expr_with_io(LocKind, IOStateVar0, IOStateVar,
                    SubGoal0, Then0, Catches0, MaybeCatchAny0, Context,
                    Renaming, HLDSGoal, !SVarState, !SVarStore,
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
                HLDSGoal = true_goal_with_context(Context)
            )
        ;
            MaybeIO0 = no,
            transform_try_expr_without_io(LocKind, SubGoal0, Then0, MaybeElse0,
                Catches0, MaybeCatchAny0, Context, Renaming, HLDSGoal,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs)
        )
    ;
        Goal = if_then_else_expr(Context, Vars0, StateVars0,
            Cond, Then, Else),
        BeforeSVarState = !.SVarState,
        rename_var_list(need_not_rename, Renaming, Vars0, Vars),
        rename_var_list(need_not_rename, Renaming, StateVars0, StateVars),
        svar_prepare_for_local_state_vars(Context, !.VarSet, StateVars,
            BeforeSVarState, BeforeCondSVarState, !Specs),
        transform_parse_tree_goal_to_hlds(LocKind, Cond, Renaming, HLDSCond,
            BeforeCondSVarState, AfterCondSVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs),
        transform_parse_tree_goal_to_hlds(LocKind, Then, Renaming, HLDSThen0,
            AfterCondSVarState, AfterThenSVarState0, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs),
        svar_finish_local_state_vars(StateVars, BeforeSVarState,
            AfterThenSVarState0, AfterThenSVarState),
        transform_parse_tree_goal_to_hlds(LocKind, Else, Renaming, HLDSElse0,
            BeforeSVarState, AfterElseSVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs),
        svar_finish_if_then_else(LocKind, Context, StateVars,
            HLDSThen0, HLDSThen, HLDSElse0, HLDSElse,
            BeforeSVarState, AfterCondSVarState, AfterThenSVarState,
            AfterElseSVarState, !:SVarState, !VarSet, !SVarStore, !Specs),
        GoalExpr = if_then_else(Vars, HLDSCond, HLDSThen, HLDSElse),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = not_expr(Context, SubGoal),
        BeforeOutsideState = !.SVarState,
        transform_parse_tree_goal_to_hlds(LocKind, SubGoal, Renaming,
            HLDSSubGoal, !.SVarState, _, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs),
        !:SVarState = BeforeOutsideState,
        GoalExpr = negation(HLDSSubGoal),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = conj_expr(Context, SubGoalA, SubGoalB),
        accumulate_plain_conjuncts(LocKind, SubGoalA, Renaming,
            cord.init, HLDSConjunctsCordA,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        accumulate_plain_conjuncts(LocKind, SubGoalB, Renaming,
            HLDSConjunctsCordA, HLDSConjunctsCordAB,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        HLDSConjuncts = cord.list(HLDSConjunctsCordAB),
        goal_info_init(Context, GoalInfo),
        conj_list_to_goal(HLDSConjuncts, GoalInfo, HLDSGoal)
    ;
        Goal = par_conj_expr(Context, SubGoalA, SubGoalB),
        accumulate_par_conjuncts(LocKind, SubGoalA, Renaming,
            cord.init, HLDSConjunctsCordA,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        accumulate_par_conjuncts(LocKind, SubGoalB, Renaming,
            HLDSConjunctsCordA, HLDSConjunctsCordAB,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        HLDSConjuncts = cord.list(HLDSConjunctsCordAB),
        goal_info_init(Context, GoalInfo),
        par_conj_list_to_goal(HLDSConjuncts, GoalInfo, HLDSGoal)
    ;
        Goal = disj_expr(Context, SubGoalA, SubGoalB),
        SVarStateBefore = !.SVarState,
        get_disj(LocKind, SubGoalB, Renaming, [], DisjunctsSVarStates1,
            SVarStateBefore, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        get_disj(LocKind, SubGoalA, Renaming,
            DisjunctsSVarStates1, DisjunctsSVarStates,
            SVarStateBefore, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        svar_finish_disjunction(Context, DisjunctsSVarStates, Disjuncts,
            !VarSet, SVarStateBefore, SVarStateAfter, !SVarStore),
        !:SVarState = SVarStateAfter,
        goal_info_init(Context, GoalInfo),
        disj_list_to_goal(Disjuncts, GoalInfo, HLDSGoal)
    ;
        Goal = implies_expr(Context, P, Q),
        % `P => Q' is defined as `not (P, not Q)'
        TransformedGoal =
            not_expr(Context,
                conj_expr(Context, P, not_expr(Context, Q))),
        transform_parse_tree_goal_to_hlds(LocKind, TransformedGoal,
            Renaming, HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Goal = equivalent_expr(Context, SubGoalA, SubGoalB),
        % `P <=> Q' is defined as `(P => Q), (Q => P)',
        % but that transformation must not be done until after quantification,
        % lest the duplication of the goals concerned affect the implicit
        % quantification of the variables inside them.

        SVarStateBefore = !.SVarState,
        transform_parse_tree_goal_to_hlds(LocKind, SubGoalA, Renaming,
            HLDSSubGoalA, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        transform_parse_tree_goal_to_hlds(LocKind, SubGoalB, Renaming,
            HLDSSubGoalB, !.SVarState, _, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        !:SVarState = SVarStateBefore,
        GoalExpr = shorthand(bi_implication(HLDSSubGoalA, HLDSSubGoalB)),
        goal_info_init(Context, GoalInfo),
        HLDSGoal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Goal = event_expr(Context, EventName, ArgTerms0),
        expand_bang_states(ArgTerms0, ArgTerms1),
        rename_vars_in_term_list(need_not_rename, Renaming,
            ArgTerms1, ArgTerms),
        make_fresh_arg_vars_subst_svars(ArgTerms, HeadVars, !VarSet,
            !SVarState, !Specs),
        list.length(HeadVars, Arity),
        list.duplicate(Arity, in_mode, Modes),
        Details = event_call(EventName),
        GoalExpr0 = generic_call(Details, HeadVars, Modes, arg_reg_types_unset,
            detism_det),
        goal_info_init(Context, GoalInfo),
        HLDSGoal0 = hlds_goal(GoalExpr0, GoalInfo),
        CallId = generic_call_id(gcid_event_call(EventName)),
        insert_arg_unifications(HeadVars, ArgTerms, Context, ac_call(CallId),
            HLDSGoal0, HLDSGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        svar_finish_atomic_goal(LocKind, !SVarState)
    ;
        Goal = call_expr(Context, Name, ArgTerms0, Purity),
        expand_bang_states(ArgTerms0, ArgTerms1),
        (
            Name = unqualified("\\="),
            ArgTerms1 = [LHSTerm, RHSTerm]
        ->
            % `LHS \= RHS' is defined as `not (LHS = RHS)'
            TransformedGoal = not_expr(Context,
                unify_expr(Context, LHSTerm, RHSTerm, Purity)),
            transform_parse_tree_goal_to_hlds(LocKind, TransformedGoal,
                Renaming, HLDSGoal, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs)
        ;
            % check for a state var record assignment:
            % !Var ^ field := Value
            Name = unqualified(":="),
            ArgTerms1 = [LHSTerm0, RHSTerm0],
            LHSTerm0 = functor(atom("^"), [StateVar0, Remainder],
                FieldListContext),
            StateVar0 = functor(atom("!"), StateVarNameTerms, StateVarContext),
            StateVarNameTerms = [variable(_, _)]
        ->
            % !Var ^ field := Value is defined as
            % !:Var = !.Var ^ field := Value.
            LHSTerm = functor(atom("!:"), StateVarNameTerms, StateVarContext),
            StateVar = functor(atom("!."), StateVarNameTerms, StateVarContext),
            FieldList = functor(atom("^"), [StateVar, Remainder],
                FieldListContext),
            RHSTerm = functor(atom(":="), [FieldList, RHSTerm0], Context),
            TransformedGoal = unify_expr(Context, LHSTerm, RHSTerm, Purity),
            transform_parse_tree_goal_to_hlds(LocKind, TransformedGoal,
                Renaming, HLDSGoal, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs)
        ;
            % check for a DCG field access goal:
            % get: Field =^ field
            % set: ^ field := Field
            ( Name = unqualified(Operator) ),
            ( Operator = "=^", AccessType = get
            ; Operator = ":=", AccessType = set
            )
        ->
            rename_vars_in_term_list(need_not_rename, Renaming,
                ArgTerms1, ArgTerms),
            transform_dcg_record_syntax(LocKind, AccessType, ArgTerms, Context,
                HLDSGoal, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs)
        ;
            rename_vars_in_term_list(need_not_rename, Renaming,
                ArgTerms1, ArgTerms),
            make_fresh_arg_vars_subst_svars(ArgTerms, HeadVars, !VarSet,
                !SVarState, !Specs),
            list.length(ArgTerms, Arity),
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

                hlds_goal.generic_call_to_id(GenericCall, GenericCallId),
                CallId = generic_call_id(GenericCallId)
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
            HLDSGoal0 = hlds_goal(Call, GoalInfo),

            record_called_pred_or_func(pf_predicate, Name, Arity, !QualInfo),
            insert_arg_unifications(HeadVars, ArgTerms, Context,
                ac_call(CallId), HLDSGoal0, HLDSGoal, !SVarState, !SVarStore,
                !VarSet, !ModuleInfo, !QualInfo, !Specs)
        ),
        svar_finish_atomic_goal(LocKind, !SVarState)
    ;
        Goal = unify_expr(Context, TermA0, TermB0, Purity),
        rename_vars_in_term(need_not_rename, Renaming, TermA0, TermA),
        rename_vars_in_term(need_not_rename, Renaming, TermB0, TermB),
        % It is an error for the left or right hand side of a unification
        % to be !A (although it may be !.A or !:A).
        ( TermA = functor(atom("!"), [variable(StateVarA, _)], _) ->
            report_svar_unify_error(Context, StateVarA, 
                !VarSet, !SVarState, !Specs),
            ( TermB = functor(atom("!"), [variable(StateVarB, _)], _) ->
                report_svar_unify_error(Context, StateVarB, 
                    !VarSet, !SVarState, !Specs)
            ;
                true
            ),
            HLDSGoal = true_goal_with_context(Context)
        ; TermB = functor(atom("!"), [variable(StateVarB, _)], _) ->
            report_svar_unify_error(Context, StateVarB,
                !VarSet, !SVarState, !Specs),
            HLDSGoal = true_goal
        ;
            unravel_unification(TermA, TermB, Context, umc_explicit, [],
                Purity, HLDSGoal, !SVarState, !SVarStore, !VarSet,
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
    GetGoal = call_expr(Context, GetPredName, [SetVar], GetPurity),
    SetGoal = call_expr(Context, SetPredName, [UseVar], SetPurity).

:- pred extract_trace_io_var(prog_context::in, prog_var::in,
    goal::out, goal::out) is det.

extract_trace_io_var(Context, StateVar, GetGoal, SetGoal) :-
    IO = mercury_io_module,
    GetPredName = qualified(IO, "unsafe_get_io_state"),
    SetPredName = qualified(IO, "unsafe_set_io_state"),
    SetVar = functor(atom("!:"), [variable(StateVar, Context)], Context),
    UseVar = functor(atom("!."), [variable(StateVar, Context)], Context),
    GetPurity = purity_semipure,
    SetPurity = purity_impure,
    GetGoal = call_expr(Context, GetPredName, [SetVar], GetPurity),
    SetGoal = call_expr(Context, SetPredName, [UseVar], SetPurity).

:- pred transform_promise_eqv_goal(loc_kind::in,
    list(prog_var)::in, list(prog_var)::in,
    list(prog_var)::in, list(prog_var)::in,
    prog_context::in, prog_var_renaming::in, list(prog_var)::out,
    goal::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_promise_eqv_goal(LocKind, Vars0, StateVars0, DotSVars0, ColonSVars0,
        Context, Renaming, QuantVars, Goal, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    rename_var_list(need_not_rename, Renaming, Vars0, Vars),
    rename_var_list(need_not_rename, Renaming, StateVars0, StateVars1),
    rename_var_list(need_not_rename, Renaming, DotSVars0, DotSVars1),
    rename_var_list(need_not_rename, Renaming, ColonSVars0, ColonSVars1),
    list.map_foldl3(lookup_dot_state_var(Context), StateVars1, OldStateVars,
        !VarSet, !SVarState, !Specs),
    list.map_foldl3(lookup_dot_state_var(Context), DotSVars1, DotSVars,
        !VarSet, !SVarState, !Specs),
    transform_parse_tree_goal_to_hlds(LocKind, Goal, Renaming, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    list.map_foldl3(lookup_dot_state_var(Context), StateVars1, NewStateVars,
        !VarSet, !SVarState, !Specs),
    list.map_foldl3(lookup_dot_state_var(Context), ColonSVars1, ColonSVars,
        !VarSet, !SVarState, !Specs),
    QuantVars = Vars ++ OldStateVars ++ NewStateVars ++ DotSVars ++ ColonSVars.

:- pred transform_dcg_record_syntax(loc_kind::in,
    field_access_type::in, list(prog_term)::in, prog_context::in,
    hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_dcg_record_syntax(LocKind, AccessType, ArgTerms0, Context, HLDSGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    goal_info_init(Context, GoalInfo),
    (
        ArgTerms0 = [LHSTerm, RHSTerm, TermInputTerm, TermOutputTerm],
        (
            AccessType = get,
            FieldNameTerm = RHSTerm,
            FieldValueTerm = LHSTerm
        ;
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
                Context, HLDSGoal, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            svar_finish_atomic_goal(LocKind, !SVarState)
        ;
            MaybeFieldNames = error1(FieldNamesSpecs),
            !:Specs = FieldNamesSpecs ++ !.Specs,
            invalid_goal("^", ArgTerms0, GoalInfo, HLDSGoal, !VarSet,
                !SVarState, !Specs),
            qual_info_set_found_syntax_error(yes, !QualInfo)
        )
    ;
        invalid_goal("^", ArgTerms0, GoalInfo, HLDSGoal, !VarSet, !SVarState,
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

transform_dcg_record_syntax_2(AccessType, FieldNames, ArgTerms, Context,
        HLDSGoal, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    make_fresh_arg_vars_subst_svars(ArgTerms, ArgVars, !VarSet,
        !SVarState, !Specs),
    ( ArgVars = [FieldValueVarPrime, TermInputVarPrime, TermOutputVarPrime] ->
        FieldValueVar = FieldValueVarPrime,
        TermInputVar = TermInputVarPrime,
        TermOutputVar = TermOutputVarPrime
    ;
        unexpected($module, $pred, "arity != 3")
    ),
    (
        AccessType = set,
        expand_set_field_function_call(Context, umc_explicit, [],
            FieldNames, FieldValueVar, TermInputVar, TermOutputVar,
            Functor, InnermostFunctor - InnermostSubContext, HLDSGoal0,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),

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
            ArgContexts, Context, HLDSGoal0, HLDSGoal,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs)
    ;
        AccessType = get,
        expand_dcg_field_extraction_goal(Context, umc_explicit, [],
            FieldNames, FieldValueVar, TermInputVar, TermOutputVar,
            Functor, InnermostFunctor - _InnerSubContext, HLDSGoal0,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
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
            ArgContexts, Context, HLDSGoal0, HLDSGoal,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs)
    ).

    % accumulate_plain_conjuncts(LocKind, Goal, Renaming, !HLDSConjunctsCord,
    %   ...):
    %
    % Goal is a tree of conjuncts. Flatten it into a list (applying Renaming),
    % and append the result to the end of !HLDSConjunctsCord.
    %
:- pred accumulate_plain_conjuncts(loc_kind::in, goal::in,
    prog_var_renaming::in, cord(hlds_goal)::in, cord(hlds_goal)::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

accumulate_plain_conjuncts(LocKind, Goal, Renaming, !HLDSConjunctsCord,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    ( Goal = conj_expr(_Context, SubGoalA, SubGoalB) ->
        accumulate_plain_conjuncts(LocKind, SubGoalA, Renaming,
            !HLDSConjunctsCord, !SVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs),
        accumulate_plain_conjuncts(LocKind, SubGoalB, Renaming,
            !HLDSConjunctsCord, !SVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs)
    ;
        transform_parse_tree_goal_to_hlds(LocKind, Goal, Renaming, HLDSGoal,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        HLDSGoal = hlds_goal(HLDSGoalExpr, _),
        ( HLDSGoalExpr = conj(plain_conj, HLDSConjuncts) ->
            !:HLDSConjunctsCord = !.HLDSConjunctsCord ++
                cord.from_list(HLDSConjuncts)
        ;
            !:HLDSConjunctsCord = cord.snoc(!.HLDSConjunctsCord, HLDSGoal)
        )
    ).

    % accumulate_par_conjuncts does the same job as accumulate_plain_conjuncts
    % but for parallel conjunctions.
    %
:- pred accumulate_par_conjuncts(loc_kind::in, goal::in, prog_var_renaming::in,
    cord(hlds_goal)::in, cord(hlds_goal)::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

accumulate_par_conjuncts(LocKind, Goal, Renaming, !HLDSConjunctsCord,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    ( Goal = par_conj_expr(_Context, SubGoalA, SubGoalB) ->
        accumulate_par_conjuncts(LocKind, SubGoalA, Renaming,
            !HLDSConjunctsCord, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        accumulate_par_conjuncts(LocKind, SubGoalB, Renaming,
            !HLDSConjunctsCord, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        transform_parse_tree_goal_to_hlds(LocKind, Goal, Renaming, HLDSGoal,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        HLDSGoal = hlds_goal(HLDSGoalExpr, _),
        ( HLDSGoalExpr = conj(parallel_conj, HLDSConjuncts) ->
            !:HLDSConjunctsCord = !.HLDSConjunctsCord ++
                cord.from_list(HLDSConjuncts)
        ;
            !:HLDSConjunctsCord = cord.snoc(!.HLDSConjunctsCord, HLDSGoal)
        )
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
    ( Goal = disj_expr(_Context, SubGoalA, SubGoalB) ->
        % We recurse on the *second* arm first, so that we will put the
        % disjuncts from *that* arm at the front of DisjStates0, before
        % putting the disjuncts from the first arm at the front of the
        % resulting DisjStates1. This way, the overall result, DisjStates,
        % will have the disjuncts and their svar_infos in the correct order.
        get_disj(LocKind, SubGoalB, Renaming, DisjStates0, DisjStates1,
            SVarStateBefore, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        get_disj(LocKind, SubGoalA, Renaming, DisjStates1, DisjStates,
            SVarStateBefore, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        transform_parse_tree_goal_to_hlds(LocKind, Goal, Renaming,
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
    transform_parse_tree_goal_to_hlds(LocKind, Goal, Renaming, HLDSGoal,
        SVarStateBefore, SVarStateAfterDisjunct, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),
    DisjState = hlds_goal_svar_state(HLDSGoal, SVarStateAfterDisjunct),
    transform_orelse_goals(LocKind, Goals, Renaming, DisjStates,
        SVarStateBefore, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).

%----------------------------------------------------------------------------%
%
% Try goals.
%

    % Transform a try_expr which needs to perform I/O. The end result looks
    % like this:
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
    % Then, ExcpHandling separately, then stitch them together into HLDS goals.
    % This is because we need to find out the variable for !.IO at the end of
    % Goal, before entering Then. The variable will be used in the later
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
    CallMagicGoal = call_expr(Context, magic_exception_result_sym_name,
        [ResultVarTerm], purity_pure),
    transform_parse_tree_goal_to_hlds(LocKind, CallMagicGoal, Renaming,
        HLDSCallMagicGoal, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),

    % Get the variable for !.IO before the (eventual) try_io call.
    lookup_dot_state_var(Context, IOStateVar, IOStateVarBefore,
        !VarSet, !SVarState, !Specs),

    SVarStateBeforeDisjunction = !.SVarState,

    % Build "TryResult = succeeded({})".
    ResultIsSucceededUnifyGoal = unify_expr(Context,
        ResultVarTerm,
        exception_functor("succeeded", NullTupleTerm, Context),
        purity_pure),
    transform_parse_tree_goal_to_hlds(LocKind, ResultIsSucceededUnifyGoal,
        Renaming, HLDSResultIsSucceededUnifyGoal, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),

    % Build "some [] ( !:IO = !.IO, Goal )".
    %
    % The explicit unification avoids a degenerate case where Goal doesn't bind
    % the final !:IO variable, which would lead to trouble later when we move
    % Goal into its own lambda.
    IOUnify = unify_expr(Context,
        functor(atom("!:"), [variable(IOStateVarUnrenamed, Context)], Context),
        functor(atom("!."), [variable(IOStateVarUnrenamed, Context)], Context),
        purity_pure),
    ScopedGoal = some_expr(Context, [], conj_expr(Context, IOUnify, Goal0)),
    transform_parse_tree_goal_to_hlds(LocKind, ScopedGoal, Renaming,
        HLDSScopedGoal, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),

    % Remember the variable for !.IO after the (eventual) try_io Goal.
    lookup_dot_state_var(Context, IOStateVar, IOStateVarAfter,
        !VarSet, !SVarState, !Specs),

    % Build "some [] ( Then )".
    ScopedThenGoal = some_expr(Context, [], Then0),
    transform_parse_tree_goal_to_hlds(LocKind, ScopedThenGoal, Renaming,
        HLDSScopedThenGoal, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),

    % Build:
    %
    %   TryResult = succeeded({}),
    %   some [] ( !:IO = !.IO, Goal ),
    %   some [] ( Then )
    %
    conj_list_to_goal(
        [HLDSResultIsSucceededUnifyGoal, HLDSScopedGoal, HLDSScopedThenGoal],
        GoalInfo, HLDSResultIsSucceededDisjunctGoal),

    SVarStateAfterResultIsSucceededDisjunct = !.SVarState,
    !:SVarState = SVarStateBeforeDisjunction,

    % Build the disjunct for "TryResult = exception(Excp), ...".
    make_exception_handling_disjunct(ResultVarTerm, ExcpVarTerm, Catches0,
        MaybeCatchAny0, Context, ResultIsExceptionDisjunctGoal),
    transform_parse_tree_goal_to_hlds(LocKind, ResultIsExceptionDisjunctGoal,
        Renaming, HLDSResultIsExceptionDisjunctGoal, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),

    SVarStateAfterResultIsExceptionDisjunct = !.SVarState,

    % Get the disjuncts.
    DisjunctSVarStates = [
        hlds_goal_svar_state(HLDSResultIsSucceededDisjunctGoal,
            SVarStateAfterResultIsSucceededDisjunct),
        hlds_goal_svar_state(HLDSResultIsExceptionDisjunctGoal,
            SVarStateAfterResultIsExceptionDisjunct)
    ],
    svar_finish_disjunction(Context, DisjunctSVarStates, HLDSDisjuncts,
        !VarSet, SVarStateBeforeDisjunction, !:SVarState, !SVarStore),
    disj_list_to_goal(HLDSDisjuncts, GoalInfo, HLDSDisjunction),

    % Build the call to magic_exception_result followed by the disjunction.
    conj_list_to_goal([HLDSCallMagicGoal, HLDSDisjunction], GoalInfo,
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

transform_try_expr_without_io(LocKind, SubGoal, ThenGoal, MaybeElseGoal,
        Catches, MaybeCatchAny, Context, Renaming, TryGoal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    varset.new_named_var("TryResult", ResultVar, !VarSet),
    varset.new_var(ExcpVar, !VarSet),

    ResultVarTerm = variable(ResultVar, Context),
    ExcpVarTerm = variable(ExcpVar, Context),
    NullTupleTerm = functor(atom("{}"), [], Context),

    goal_info_init(Context, GoalInfo),

    % Build the call to magic_exception_result.
    CallMagicGoal = call_expr(Context, magic_exception_result_sym_name,
        [ResultVarTerm], purity_pure),

    % Build "TryResult = succeeded({}), ..." disjunct.
    ResultIsSucceededUnifyGoal = unify_expr(Context,
        ResultVarTerm,
        exception_functor("succeeded", NullTupleTerm, Context),
        purity_pure),
    (
        MaybeElseGoal = yes(ElseGoal),
        SucceededSubGoal = if_then_else_expr(Context, [], [],
            SubGoal, ThenGoal, ElseGoal)
    ;
        MaybeElseGoal = no,
        SucceededSubGoal =
            conj_expr(Context,
                some_expr(Context, [], SubGoal),
                some_expr(Context, [], ThenGoal)
            )
    ),
    ResultIsSucceededDisjunctGoal =
        conj_expr(Context, ResultIsSucceededUnifyGoal, SucceededSubGoal),

    % Build the disjunct for "TryResult = exception(Excp), ...".
    make_exception_handling_disjunct(ResultVarTerm, ExcpVarTerm,
        Catches, MaybeCatchAny, Context, ResultIsExceptionDisjunctGoal),

    % Build the call followed by the disjunction.
    CallMagicThenDisjunctionGoal =
        conj_expr(Context,
            CallMagicGoal,
            disj_expr(Context,
                ResultIsSucceededDisjunctGoal,
                ResultIsExceptionDisjunctGoal
            )
        ),
    transform_parse_tree_goal_to_hlds(LocKind, CallMagicThenDisjunctionGoal,
        Renaming, HLDSCallMagicThenDisjunctionGoal, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),

    ShortHand = try_goal(no, ResultVar, HLDSCallMagicThenDisjunctionGoal),
    GoalExpr = shorthand(ShortHand),
    TryGoal = hlds_goal(GoalExpr, GoalInfo).

:- pred make_exception_handling_disjunct(prog_term::in, prog_term::in,
    list(catch_expr)::in, maybe(catch_any_expr)::in, prog_context::in,
    goal::out) is det.

make_exception_handling_disjunct(ResultVarTerm, ExcpVarTerm, Catches,
        MaybeCatchAny, Context, Goal) :-
    ResultIsExceptionUnify = unify_expr(Context,
        ResultVarTerm,
        exception_functor("exception", ExcpVarTerm, Context),
        purity_pure),
    make_catch_ite_chain(ResultVarTerm, ExcpVarTerm, Catches, MaybeCatchAny,
        CatchChain),
    Goal = conj_expr(Context, ResultIsExceptionUnify, CatchChain).

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
        Goal = if_then_else_expr(get_term_context(FirstPattern), [], [],
            FirstPatternGoal, FirstGoal, ElseGoal)
    ;
        Catches = [],
        (
            MaybeCatchAny = yes(catch_any_expr(CatchAnyVar, CatchAnyGoal)),
            % With a catch_any part, end the if-then-else chain with:
            %   CatchAnyVar = exc_univ_value(Excp),
            %   CatchAnyGoal
            Context = goal_get_context(CatchAnyGoal),
            GetUnivValue = unify_expr(Context,
                variable(CatchAnyVar, Context),
                exception_functor("exc_univ_value", ExcpVarTerm, Context),
                purity_pure),
            Goal = conj_expr(Context, GetUnivValue, CatchAnyGoal)
        ;
            MaybeCatchAny = no,
            % Without a catch_any part, end the if-then-else chain
            % by rethrowing the exception.
            Rethrow = qualified(mercury_exception_module, "rethrow"),
            Goal = call_expr( get_term_context(ExcpVarTerm),
                Rethrow, [ResultVarTerm], purity_pure)
        )
    ).

:- pred make_catch_pattern_unify_goal(prog_term::in, prog_term::in,
    goal::out) is det.

make_catch_pattern_unify_goal(CatchPatternTerm, ExcpVarTerm, Goal) :-
    Goal = call_expr(get_term_context(CatchPatternTerm),
        qualified(mercury_exception_module, "exc_univ_to_type"),
        [ExcpVarTerm, CatchPatternTerm], purity_pure).

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
