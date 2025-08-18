%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: det_infer_goal.m.
% Main authors: conway, fjh, zs.
%
% This module is the main part of determinism inference: it infers the
% determinism of a goal.
%
%---------------------------------------------------------------------------%

:- module check_hlds.det_infer_goal.
:- interface.

:- import_module check_hlds.det_util.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

%---------------------------------------------------------------------------%

    % Determinism has three-ish components:
    %
    %   1:  whether a goal can fail (i.e. whether it can have zero solutions)
    %   2a: whether a goal can have more than zero solution
    %   2b: whether a goal can have more than one solution
    %   3:  whether a goal occurs in a context where only the first solution
    %           is required
    %
    % Components 1, 2a and 2b are synthesized attributes: they are inferred
    % bottom-up. Component 3 is an inherited attribute: it is propagated
    % top-down. The soln_context type represents component 3.
    %
:- type soln_context
    --->    all_solns
    ;       first_soln.

    % How many solutions (one or all) are needed for a given determinism?
    %
:- pred det_get_soln_context(determinism::in, soln_context::out) is det.

%---------------------------------------------------------------------------%

    % det_infer_proc_goal(InstMap0, SolnContext, Detism,
    %   Goal0, Goal, !DetInfo):
    %
    % This is a version of det_infer_goal (see below) which assumes that
    % the goal it is given is the whole body of a procedure. This allows us
    % to simplify its interface for external callers.
    %
:- pred det_infer_proc_goal(instmap::in, soln_context::in, determinism::out,
    hlds_goal::in, hlds_goal::out, det_info::in, det_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_check_goal.
:- import_module check_hlds.mode_comparison.
:- import_module check_hlds.simplify.
:- import_module check_hlds.simplify.format_call.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.passes_aux.
:- import_module libs.
:- import_module libs.maybe_util.
:- import_module libs.options.
:- import_module parse_tree.error_sort.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

det_get_soln_context(DeclaredDetism, SolnContext) :-
    determinism_components(DeclaredDetism, _, SolnCount),
    (
        ( SolnCount = at_most_zero
        ; SolnCount = at_most_one
        ; SolnCount = at_most_many
        ),
        SolnContext = all_solns
    ;
        SolnCount = at_most_many_cc,
        SolnContext = first_soln
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

det_infer_proc_goal(InstMap0, SolnContext, Detism, Goal0, Goal, !DetInfo) :-
    MaybePromiseEqvSolutionSets = no,
    RightFailingContexts = [],
    det_infer_goal(InstMap0, SolnContext, MaybePromiseEqvSolutionSets, Detism,
        RightFailingContexts, _GoalFailingContexts, Goal0, Goal, !DetInfo).

%---------------------------------------------------------------------------%

    % "pess" is short for promise_equivalent_solution_sets_info.
    %
    % The maybe(pess_info) argument of det_infer_goal specifies whether
    %
    % - the goal is inside a promise_equivalent_solution_sets goal, and
    % - if it is, then the set of variables in being promised equivalent,
    % - and the context of that promise.
    %
    % Note that such scopes may not be nested, so we only need one context.
:- type pess_info
    --->    pess_info(list(prog_var), prog_context).

:- type failing_context
    --->    failing_context(
                prog_context,
                failing_goal
            ).

:- type failing_goal
    --->    incomplete_switch(prog_var)
    ;       fail_goal
    ;       test_goal(prog_var, prog_var)
    ;       deconstruct_goal(prog_var, cons_id)
    ;       call_goal(pred_id, proc_id)
    ;       generic_call_goal(generic_call)
    ;       negated_goal.

    % det_infer_goal(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
    %   Detism, RightFailingContexts, GoalFailingContexts,
    %   Goal0, Goal, !DetInfo):
    %
    % Infers the determinism of `Goal0' and returns this in `Detism'.
    % It annotates the goal and all its subgoals with their determinisms,
    % and returns the annotated goal in `Goal'.
    %
    % InstMap0 should be the instmap at the start of Goal0.
    % SolnContext should tell us whether Goal0 occurs in a context
    % where only the first solution is required (the inherited component
    % of determinism mentioned at the top).
    %
    % RightFailingContexts should specify the set of failing_contexts
    % (goals that can fail, with descriptions of how they can fail)
    % to the right of Goal0 in the surrounding code. In GoalFailingContexts,
    % we return the set of failing_contexts that can fail inside Goal.
    %
    % The reason why we need to know which goals can fail to the right of
    % Goal0 has to do with committed choice code. If you only need
    % the first solution of a conjunction, you may only need the first
    % solution of each conjunct, but if conjunct k may fail, then it is not
    % enough for a conjunct i for i<k to generate its first solution,
    % since this solution may be rejected by conjunct k. The conjuncts i
    % are therefore in an all_solns context, even if the conjunction
    % is in a first_soln context. The reason why we want to know
    % not only *whether* there are any goals to the right of Goal0
    % that can fail, but also *where* they are, and *how* they can fail,
    % is for the generation of informative error messages.
    %
    % If Goal0 is inside a promise_equivalent_solution_sets scope,
    % then MaybePromiseEqvSolutionSets should specify its details.
    % Otherwise, it should be `no'.
    %
:- pred det_infer_goal(instmap::in, soln_context::in, maybe(pess_info)::in,
    determinism::out, list(failing_context)::in, list(failing_context)::out,
    hlds_goal::in, hlds_goal::out, det_info::in, det_info::out) is det.

det_infer_goal(InstMap0, !.SolnContext, MaybePromiseEqvSolutionSets, Detism,
        RightFailingContexts, GoalFailingContexts, Goal0, Goal, !DetInfo) :-
    Goal0 = hlds_goal(_, GoalInfo0),
    NonLocalVars = goal_info_get_nonlocals(GoalInfo0),
    InstmapDelta = goal_info_get_instmap_delta(GoalInfo0),

    % If a pure or semipure goal has no output variables, then the goal
    % is in a single-solution context.
    ( if
        det_no_output_vars(!.DetInfo, InstMap0, InstmapDelta, NonLocalVars),
        Purity = goal_info_get_purity(GoalInfo0),
        (
            Purity = purity_impure
        =>
            goal_info_has_feature(GoalInfo0,
                feature_not_impure_for_determinism)
        )
    then
        AddPruning = yes,
        !:SolnContext = first_soln
    else
        AddPruning = no
    ),
    det_infer_goal_known_pruning(InstMap0, !.SolnContext,
        MaybePromiseEqvSolutionSets, AddPruning, Detism,
        RightFailingContexts, GoalFailingContexts, Goal0, Goal, !DetInfo).

:- pred det_infer_goal_known_pruning(instmap::in, soln_context::in,
    maybe(pess_info)::in, bool::in, determinism::out,
    list(failing_context)::in, list(failing_context)::out,
    hlds_goal::in, hlds_goal::out, det_info::in, det_info::out) is det.

det_infer_goal_known_pruning(InstMap0, SolnContext,
        MaybePromiseEqvSolutionSets, AddPruning, Detism,
        RightFailingContexts, GoalFailingContexts, Goal0, Goal, !DetInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    InstmapDelta = goal_info_get_instmap_delta(GoalInfo0),

    ( if
        GoalExpr0 = scope(ScopeReason, _),
        (
            % Some other part of the compiler has determined that we need
            % to keep the cut represented by this quantification. This can
            % happen e.g. when deep profiling adds impure code to the goal
            % inside the scope; it doesn't want to change the behavior of
            % the scope, even though the addition of impurity would make
            % the if-then-else treat it differently.

            ScopeReason = commit(force_pruning)
        ;
            % If all solutions are promised to be equivalent according to the
            % relevant equality theory, we want to prune away all but one
            % of those solutions.

            ScopeReason = promise_solutions(_, PromiseEqvSolnsKind),
            promise_eqv_solutions_kind_prunes(PromiseEqvSolnsKind) = yes
        )
    then
        Prune = yes
    else
        Prune = AddPruning
    ),

    det_infer_goal_expr(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        GoalInfo0, InternalDetism0,
        RightFailingContexts, GoalFailingContexts,
        GoalExpr0, GoalExpr1, !DetInfo),

    determinism_components(InternalDetism0, InternalCanFail, InternalSolns0),
    ( if
        % If mode analysis notices that a goal cannot succeed,
        % then determinism analysis should notice this too.

        instmap_delta_is_unreachable(InstmapDelta)
    then
        InternalSolns = at_most_zero
    else
        InternalSolns = InternalSolns0
    ),
    ( if
        ( InternalSolns = at_most_many
        ; InternalSolns = at_most_many_cc
        ),
        Prune = yes
    then
        Solns = at_most_one
    else if
        % If a goal with multiple solutions occurs in a single-solution
        % context, then we will need to do pruning.

        InternalSolns = at_most_many,
        SolnContext = first_soln
    then
        Solns = at_most_many_cc
    else
        Solns = InternalSolns
    ),
    determinism_components(Detism, InternalCanFail, Solns),
    goal_info_set_determinism(Detism, GoalInfo0, GoalInfo),

    % The code generators assume that conjunctions containing multi or nondet
    % goals and if-then-elses containing multi or nondet conditions can only
    % occur inside other multi or nondet goals. simplify.m modifies the code
    % to make these invariants hold. Determinism analysis can be rerun after
    % simplification, and without this code here the invariants would not hold
    % after determinism analysis (the number of solutions of the inner goal
    % would be changed back from at_most_many to at_most_one or at_most_zero).
    ( if
        % If-then-elses that are det or semidet may nevertheless contain nondet
        % or multidet conditions. If this happens, the if-then-else must be put
        % inside a `scope' to appease the code generator. (Both the MLDS and
        % LLDS back-ends rely on this.)

        GoalExpr1 = if_then_else(_, hlds_goal(_, CondInfo), _, _),
        CondDetism = goal_info_get_determinism(CondInfo),
        determinism_components(CondDetism, _, at_most_many),
        Solns \= at_most_many
    then
        FinalInternalSolns = at_most_many
    else if
        % Conjunctions that cannot produce solutions may nevertheless contain
        % nondet and multidet goals. If this happens, we put the conjunction
        % inside a scope goal to appease the code generator.

        GoalExpr1 = conj(plain_conj, ConjGoals),
        Solns = at_most_zero,
        some_goal_is_at_most_many(ConjGoals)
    then
        FinalInternalSolns = at_most_many
    else
        FinalInternalSolns = InternalSolns
    ),
    determinism_components(FinalInternalDetism, InternalCanFail,
        FinalInternalSolns),

    % See how we should introduce the commit operator, if one is needed.
    ( if
        % Do we need a commit?
        Detism \= FinalInternalDetism,

        % Disjunctions, we want to use a semidet or cc_nondet disjunction
        % which avoids creating a choice point at all, rather than wrapping
        % a some [] around a nondet disj, which would create a choice point
        % and then prune it.
        GoalExpr1 \= disj(_),

        % Do we already have a commit?
        GoalExpr1 \= scope(_, _)
    then
        % A commit is needed - we must introduce an explicit `commit' so that
        % the code generator knows to insert the appropriate code for pruning.
        goal_info_set_determinism(FinalInternalDetism, GoalInfo0, InnerInfo),
        GoalExpr = scope(commit(do_not_force_pruning),
            hlds_goal(GoalExpr1, InnerInfo))
    else
        % Either no commit is needed, or a `scope' is already present.
        GoalExpr = GoalExpr1
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- func promise_eqv_solutions_kind_prunes(promise_solutions_kind) = bool.

promise_eqv_solutions_kind_prunes(equivalent_solutions) = yes.
promise_eqv_solutions_kind_prunes(equivalent_solution_sets) = no.
promise_eqv_solutions_kind_prunes(equivalent_solution_sets_arbitrary) = yes.

:- pred some_goal_is_at_most_many(list(hlds_goal)::in) is semidet.

some_goal_is_at_most_many([ConjGoal | ConjGoals]) :-
    ( if
        ConjGoal = hlds_goal(_, ConjGoalInfo),
        ConjGoalDetism = goal_info_get_determinism(ConjGoalInfo),
        determinism_components(ConjGoalDetism, _, at_most_many)
    then
        true
    else
        some_goal_is_at_most_many(ConjGoals)
    ).

%---------------------------------------------------------------------------%

:- pred det_infer_goal_expr(instmap::in, soln_context::in,
    maybe(pess_info)::in, hlds_goal_info::in,
    determinism::out, list(failing_context)::in, list(failing_context)::out,
    hlds_goal_expr::in, hlds_goal_expr::out,
    det_info::in, det_info::out) is det.

det_infer_goal_expr(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        GoalInfo, Detism, RightFailingContexts, GoalFailingContexts,
        GoalExpr0, GoalExpr, !DetInfo) :-
    (
        GoalExpr0 = unify(LHS, RHS0, Mode, Unify, UnifyContext),
        det_infer_unify(InstMap0, SolnContext, GoalInfo,
            LHS, Unify, UnifyContext, Detism,
            RightFailingContexts, GoalFailingContexts, RHS0, RHS, !DetInfo),
        GoalExpr = unify(LHS, RHS, Mode, Unify, UnifyContext)
    ;
        GoalExpr0 = plain_call(PredId, ProcId0, ArgVars, Builtin, UnifyContext,
            Name),
        det_infer_call(SolnContext, GoalInfo, PredId, ArgVars, Detism,
            RightFailingContexts, GoalFailingContexts,
            ProcId0, ProcId, !DetInfo),
        GoalExpr = plain_call(PredId, ProcId, ArgVars, Builtin, UnifyContext,
            Name)
    ;
        GoalExpr0 = generic_call(GenericCall, _ArgVars, _Modes, _MaybArgRegs,
            CallDetism),
        det_infer_generic_call(SolnContext, GoalInfo, GenericCall,
            CallDetism, Detism, RightFailingContexts, GoalFailingContexts,
            !DetInfo),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = call_foreign_proc(Attributes, PredId, ProcId,
            _ArgVars, _ExtraArgVars, _MaybeTraceRuntimeCond, PragmaCode),
        det_infer_foreign_proc(Attributes, PredId, ProcId, PragmaCode,
            GoalInfo, SolnContext, RightFailingContexts, Detism,
            GoalFailingContexts, !DetInfo),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            % The determinism of a conjunction is the worst case of the
            % determinisms of the goals of that conjuction.
            det_infer_conj(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
                Detism, RightFailingContexts, [], GoalFailingContexts,
                Goals0, Goals, !DetInfo)
        ;
            ConjType = parallel_conj,
            det_infer_par_conj(InstMap0, SolnContext,
                MaybePromiseEqvSolutionSets, GoalInfo, Detism,
                RightFailingContexts, GoalFailingContexts,
                Goals0, Goals, !DetInfo)
        ),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        det_infer_disj(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
            GoalInfo, Detism, RightFailingContexts, GoalFailingContexts,
            Goals0, Goals, !DetInfo),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, SwitchCanFail, Cases0),
        (
            SwitchCanFail = cannot_fail
        ;
            SwitchCanFail = can_fail,
            det_info_set_has_incomplete_switch(!DetInfo)
        ),
        trace [compiletime(flag("debug-det-analysis-progress")), io(!IO)] (
            get_det_debug_output_stream(!.DetInfo, DebugStream, !IO),
            io.write_string(DebugStream, "inferring switch on ", !IO),
            io.write_line(DebugStream, Var, !IO)
        ),
        det_infer_switch(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
            GoalInfo, Var, SwitchCanFail, Detism,
            RightFailingContexts, GoalFailingContexts,
            Cases0, Cases, !DetInfo),
        trace [compiletime(flag("debug-det-analysis-progress")), io(!IO)] (
            get_det_debug_output_stream(!.DetInfo, DebugStream, !IO),
            io.write_string(DebugStream, "done inferring switch on ", !IO),
            io.write_line(DebugStream, Var, !IO)
        ),
        GoalExpr = switch(Var, SwitchCanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        det_infer_if_then_else(InstMap0, SolnContext,
            MaybePromiseEqvSolutionSets, Detism,
            RightFailingContexts, GoalFailingContexts,
            Cond0, Cond, Then0, Then, Else0, Else, !DetInfo),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(Goal0),
        det_infer_negation(InstMap0, MaybePromiseEqvSolutionSets, GoalInfo,
            Detism, GoalFailingContexts, Goal0, Goal, !DetInfo),
        GoalExpr = negation(Goal)
    ;
        GoalExpr0 = scope(Reason, Goal0),
        det_infer_scope(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
            GoalInfo, Reason, Detism,
            RightFailingContexts, GoalFailingContexts, Goal0, Goal, !DetInfo),
        GoalExpr = scope(Reason, Goal)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Inner, Outer, Vars, MainGoal0,
                OrElseGoals0, OrElseInners),
            det_infer_atomic(InstMap0, SolnContext,
                MaybePromiseEqvSolutionSets, RightFailingContexts, Detism,
                MainGoal0, MainGoal, OrElseGoals0, OrElseGoals, !DetInfo),
            GoalFailingContexts = [],
            ShortHand = atomic_goal(GoalType, Inner, Outer, Vars, MainGoal,
                OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, TryGoal0),
            % Don't allow det_infer_goal_known_pruning to insert a commit scope
            % around the code that is standing in place for the code we will
            % actually create for a try goal.
            det_infer_goal_known_pruning(InstMap0, SolnContext,
                MaybePromiseEqvSolutionSets, no, Detism,
                RightFailingContexts, GoalFailingContexts,
                TryGoal0, TryGoal, !DetInfo),
            ShortHand = try_goal(MaybeIO, ResultVar, TryGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ).

%---------------------------------------------------------------------------%

:- pred det_infer_unify(instmap::in, soln_context::in, hlds_goal_info::in,
    prog_var::in, unification::in, unify_context::in, determinism::out,
    list(failing_context)::in, list(failing_context)::out,
    unify_rhs::in, unify_rhs::out, det_info::in, det_info::out) is det.

det_infer_unify(InstMap0, SolnContext, GoalInfo, LHS, Unify, UnifyContext,
        Detism, RightFailingContexts, GoalFailingContexts,
        RHS0, RHS, !DetInfo) :-
    trace [compiletime(flag("debug-det-analysis-progress")), io(!IO)] (
        get_det_debug_output_stream(!.DetInfo, DebugStream, !IO),
        io.write_string(DebugStream, "inferring unification ", !IO),
        io.write(DebugStream, LHS, !IO),
        io.write_string(DebugStream, " = ", !IO),
        io.write_line(DebugStream, RHS0, !IO),
        io.write_line(DebugStream, Unify, !IO)
    ),
    % Unifications are either deterministic or semideterministic.
    (
        RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc,
            NonLocalVars, ArgVarsModes, LambdaDeclaredDet, Goal0),
        ( if determinism_components(LambdaDeclaredDet, _, at_most_many_cc) then
            LambdaSolnContext = first_soln
        else
            LambdaSolnContext = all_solns
        ),
        det_info_get_module_info(!.DetInfo, ModuleInfo),
        instmap.pre_lambda_update(ModuleInfo, ArgVarsModes,
            InstMap0, InstMap1),
        det_infer_goal(InstMap1, LambdaSolnContext, no, LambdaInferredDet,
            [], _LambdaFailingContexts, Goal0, Goal, !DetInfo),
        det_check_lambda(LambdaDeclaredDet, LambdaInferredDet,
            Goal, GoalInfo, InstMap1, !DetInfo),
        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc,
            NonLocalVars, ArgVarsModes, LambdaDeclaredDet, Goal)
    ;
        ( RHS0 = rhs_var(_)
        ; RHS0 = rhs_functor(_, _, _)
        ),
        RHS = RHS0
    ),
    det_infer_unify_canfail(Unify, UnifyCanFail),
    det_infer_unify_examines_rep(Unify, ExaminesRepresentation),
    det_check_for_noncanonical_type(LHS, ExaminesRepresentation,
        UnifyCanFail, SolnContext, RightFailingContexts, [], GoalInfo,
        ccuc_unify(UnifyContext), UnifyNumSolns, !DetInfo),
    determinism_components(Detism, UnifyCanFail, UnifyNumSolns),
    (
        UnifyCanFail = can_fail,
        Context = goal_info_get_context(GoalInfo),
        (
            Unify = construct(_, _, _, _, _, _, _),
            unexpected($pred, "can_fail construct")
        ;
            Unify = assign(_, _),
            unexpected($pred, "can_fail assign")
        ;
            Unify = complicated_unify(_, _, _),
            (
                RHS = rhs_var(RHSVar),
                FailingGoal = test_goal(LHS, RHSVar)
            ;
                RHS = rhs_functor(ConsId, _, _),
                FailingGoal = deconstruct_goal(LHS, ConsId)
            ;
                RHS = rhs_lambda_goal(_, _, _, _, _, _, _),
                unexpected($pred, "complicated_unify but no fail context")
            ),
            FailingContext = failing_context(Context, FailingGoal),
            GoalFailingContexts = [FailingContext]
        ;
            Unify = deconstruct(Var, ConsId, _, _, _, _),
            FailingGoal = deconstruct_goal(Var, ConsId),
            FailingContext = failing_context(Context, FailingGoal),
            GoalFailingContexts = [FailingContext]
        ;
            Unify = simple_test(Var1, Var2),
            FailingGoal = test_goal(Var1, Var2),
            FailingContext = failing_context(Context, FailingGoal),
            GoalFailingContexts = [FailingContext]
        )
    ;
        UnifyCanFail = cannot_fail,
        GoalFailingContexts = []
    ).

    % Check a lambda goal with the specified declared and inferred
    % determinisms.
    %
:- pred det_check_lambda(determinism::in, determinism::in, hlds_goal::in,
    hlds_goal_info::in, instmap::in, det_info::in, det_info::out) is det.

det_check_lambda(DeclaredDetism, InferredDetism, Goal, GoalInfo, InstMap0,
        !DetInfo) :-
    compare_determinisms(DeclaredDetism, InferredDetism, Cmp),
    (
        ( Cmp = first_detism_tighter_than
        ; Cmp = first_detism_incomparable
        ),
        det_info_get_pred_proc_id(!.DetInfo, PredProcId),
        Context = goal_info_get_context(GoalInfo),
        det_info_get_module_info(!.DetInfo, ModuleInfo),
        ProcColonPieces = describe_one_proc_name_maybe_argmodes(ModuleInfo,
            output_mercury, yes(color_subject), should_not_module_qualify,
            [suffix(":")], PredProcId),
        DeclaredStr = determinism_to_string(DeclaredDetism),
        InferredStr = determinism_to_string(InferredDetism),
        DeclaredPieces = color_as_correct([quote(DeclaredStr), suffix(",")]),
        InferredPieces = color_as_incorrect([quote(InferredStr), suffix(".")]),
        Pieces = [words("In")] ++ ProcColonPieces ++ [nl,
            words("determinism error in lambda expression."), nl] ++
            [words("Declared")] ++ DeclaredPieces ++
            [words("inferred")] ++ InferredPieces ++ [nl],
        det_diagnose_goal_get_msgs(InstMap0, DeclaredDetism, Goal,
            GoalMsgs, !DetInfo),
        Spec = error_spec($pred, severity_error, phase_detism_check,
            [msg(Context, Pieces) | GoalMsgs]),
        det_info_add_error_spec(Spec, !DetInfo)
    ;
        ( Cmp = first_detism_same_as
        ; Cmp = first_detism_looser_than
        )
        % We don't bother issuing warnings if the determinism was too loose;
        % that will often be the case, and should not be warned about.
    ).

%---------------------------------------------------------------------------%

:- pred det_infer_call(soln_context::in, hlds_goal_info::in,
    pred_id::in, list(prog_var)::in, determinism::out,
    list(failing_context)::in, list(failing_context)::out,
    proc_id::in, proc_id::out, det_info::in, det_info::out) is det.

det_infer_call(SolnContext, GoalInfo, PredId, ArgVars,
        Detism, RightFailingContexts, GoalFailingContexts,
        ProcId0, ProcId, !DetInfo) :-
    % For calls, just look up the determinism entry associated with
    % the called predicate.
    % This is the point at which annotations start changing
    % when we iterate to fixpoint for global determinism inference.
    det_lookup_pred_info_and_detism(!.DetInfo, PredId, ProcId0,
        CalleePredInfo, Detism0),

    % We do the following so that simplify.m knows whether to invoke
    % format_call.m *without* first having to traverse the procedure body.
    ( if is_format_call(CalleePredInfo, ArgVars) then
        det_info_set_has_format_call(!DetInfo)
    else
        true
    ),

    % Make sure we don't try to call a committed-choice pred
    % from a non-committed-choice context.
    determinism_components(Detism0, CanFail, NumSolns),
    ( if
        NumSolns = at_most_many_cc,
        SolnContext = all_solns
    then
        ( if
            det_find_matching_non_cc_mode(!.DetInfo, PredId, ProcId0,
                ProcIdPrime)
        then
            ProcId = ProcIdPrime,
            determinism_components(Detism, CanFail, at_most_many)
        else
            det_info_get_module_info(!.DetInfo, ModuleInfo),
            GoalContext = goal_info_get_context(GoalInfo),
            det_info_get_var_table(!.DetInfo, VarTable),
            PredPieces = describe_one_pred_name(ModuleInfo, yes(color_subject),
                should_module_qualify, [], PredId),
            FirstPieces = [words("Error: call to")] ++ PredPieces ++
                [words("with determinism"),
                quote(mercury_det_to_string(Detism0))] ++
                color_as_incorrect([words("occurs in a context"),
                    words("which requires all solutions.")]) ++
                [nl],
            ContextMsgs = failing_contexts_description(ModuleInfo, VarTable,
                RightFailingContexts),
            Spec = error_spec($pred, severity_error, phase_detism_check,
                [msg(GoalContext, FirstPieces) | ContextMsgs]),
            det_info_add_error_spec(Spec, !DetInfo),

            ProcId = ProcId0,
            % Code elsewhere relies on the assumption that
            % SolnContext = all_solns => NumSolns \= at_most_many_cc,
            % so we need to enforce that here.
            determinism_components(Detism, CanFail, at_most_many)
        )
    else
        ProcId = ProcId0,
        Detism = Detism0
    ),
    (
        CanFail = can_fail,
        Context = goal_info_get_context(GoalInfo),
        FailingContext = failing_context(Context, call_goal(PredId, ProcId)),
        GoalFailingContexts = [FailingContext]
    ;
        CanFail = cannot_fail,
        GoalFailingContexts = []
    ).

    % det_find_matching_non_cc_mode(DetInfo, PredId, ProcId0, ProcId):
    %
    % Search for a mode of the given predicate that is identical to the mode
    % ProcId0, except that its determinism is non-cc whereas ProcId0's detism
    % is cc. Let ProcId be the first such mode.
    %
:- pred det_find_matching_non_cc_mode(det_info::in, pred_id::in,
    proc_id::in, proc_id::out) is semidet.

det_find_matching_non_cc_mode(DetInfo, PredId, CcProcId, NonCcProcId) :-
    det_info_get_module_info(DetInfo, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.to_assoc_list(ProcTable, ProcIdsInfos),
    det_find_matching_non_cc_mode_procs(ModuleInfo, PredInfo, ProcIdsInfos,
        CcProcId, NonCcProcId).

:- pred det_find_matching_non_cc_mode_procs(module_info::in, pred_info::in,
    assoc_list(proc_id, proc_info)::in, proc_id::in, proc_id::out) is semidet.

det_find_matching_non_cc_mode_procs(ModuleInfo, PredInfo,
        [ProcId - ProcInfo | ProcIdsInfos], CcProcId, NonCcProcId) :-
    ( if
        ProcId \= CcProcId,
        proc_info_interface_determinism(ProcInfo, Detism),
        determinism_components(Detism, _CanFail, MaxSoln),
        MaxSoln = at_most_many,
        modes_are_identical_bar_cc(ModuleInfo, PredInfo, CcProcId, ProcId)
    then
        NonCcProcId = ProcId
    else
        det_find_matching_non_cc_mode_procs(ModuleInfo, PredInfo, ProcIdsInfos,
            CcProcId, NonCcProcId)
    ).

%---------------------------------------------------------------------------%

:- pred det_infer_generic_call(soln_context::in, hlds_goal_info::in,
    generic_call::in, determinism::in, determinism::out,
    list(failing_context)::in, list(failing_context)::out,
    det_info::in, det_info::out) is det.

det_infer_generic_call(SolnContext, GoalInfo, GenericCall, CallDetism, Detism,
        RightFailingContexts, GoalFailingContexts, !DetInfo) :-
    determinism_components(CallDetism, CanFail, NumSolns),
    Context = goal_info_get_context(GoalInfo),
    ( if
        NumSolns = at_most_many_cc,
        SolnContext = all_solns
    then
        % This error can only occur for higher-order calls.
        % Class method calls are only introduced by polymorphism.
        det_info_get_var_table(!.DetInfo, VarTable),
        FirstPieces = [words("Error: higher-order call to predicate with"),
            words("determinism"), quote(mercury_det_to_string(CallDetism))] ++
            color_as_incorrect([words("occurs in a context"),
                words("which requires all solutions.")]) ++
            [nl],
        det_info_get_module_info(!.DetInfo, ModuleInfo),
        ContextMsgs = failing_contexts_description(ModuleInfo, VarTable,
            RightFailingContexts),
        Spec = error_spec($pred, severity_error, phase_detism_check,
            [msg(Context, FirstPieces) | ContextMsgs]),
        det_info_add_error_spec(Spec, !DetInfo),

        % Code elsewhere relies on the assumption that
        % SolnContext = all_soln => NumSolns \= at_most_many_cc,
        % so we need to enforce that here.
        determinism_components(Detism, CanFail, at_most_many)
    else
        Detism = CallDetism
    ),
    (
        CanFail = can_fail,
        FailingContext = failing_context(Context,
            generic_call_goal(GenericCall)),
        GoalFailingContexts = [FailingContext]
    ;
        CanFail = cannot_fail,
        GoalFailingContexts = []
    ).

%---------------------------------------------------------------------------%

:- pred det_infer_foreign_proc(foreign_proc_attributes::in,
    pred_id::in, proc_id::in, pragma_foreign_proc_impl::in,
    hlds_goal_info::in, soln_context::in,
    list(failing_context)::in, determinism::out, list(failing_context)::out,
    det_info::in, det_info::out) is det.

det_infer_foreign_proc(Attributes, PredId, ProcId, _PragmaCode,
        GoalInfo, SolnContext, RightFailingContexts,
        Detism, GoalFailingContexts, !DetInfo) :-
    % We handle foreign_procs pretty much the same way as predicate calls.
    det_info_get_module_info(!.DetInfo, ModuleInfo),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
    (
        MaybeDetism = yes(Detism0),
        determinism_components(Detism0, CanFail, NumSolns0),
        ( if
            get_may_throw_exception(Attributes) =
                proc_will_not_throw_exception,
            Detism0 = detism_erroneous
        then
            proc_info_get_context(ProcInfo, ProcContext),
            WillNotThrowProcPieces =
                describe_one_proc_name_maybe_argmodes(ModuleInfo,
                    output_mercury, yes(color_subject),
                    should_not_module_qualify, [], proc(PredId, ProcId)),
            WillNotThrowPieces = [words("Error:")] ++ WillNotThrowProcPieces ++
                [words("has determinism erroneous, but also has"),
                words("foreign clauses that have a")] ++
                color_as_subject([quote("will_not_throw_exception")]) ++
                [words("attribute.")] ++
                color_as_incorrect([words("This attribute cannot be applied"),
                    words("to erroneous procedures.")]) ++
                [nl],
            WillNotThrowSpec = spec($pred, severity_error, phase_detism_check,
                ProcContext, WillNotThrowPieces),
            det_info_add_error_spec(WillNotThrowSpec, !DetInfo)
        else
            true
        ),
        ( if
            NumSolns0 = at_most_many_cc,
            SolnContext = all_solns
        then
            GoalContext = goal_info_get_context(GoalInfo),
            det_info_get_var_table(!.DetInfo, VarTable),
            WrongContextPredPieces = describe_one_pred_name(ModuleInfo,
                yes(color_subject), should_module_qualify, [], PredId),
            WrongContextFirstPieces = [words("Error: call to")] ++
                WrongContextPredPieces ++
                [words("with determinism"),
                quote(mercury_det_to_string(Detism0))] ++
                color_as_incorrect([words("occurs in a context"),
                    words("which requires all solutions.")]) ++
                [nl],
            ContextMsgs = failing_contexts_description(ModuleInfo, VarTable,
                RightFailingContexts),
            Spec = error_spec($pred, severity_error, phase_detism_check,
                [msg(GoalContext, WrongContextFirstPieces) | ContextMsgs]),
            det_info_add_error_spec(Spec, !DetInfo),
            NumSolns = at_most_many
        else
            NumSolns = NumSolns0
        ),
        determinism_components(Detism, CanFail, NumSolns),
        (
            CanFail = can_fail,
            Context = goal_info_get_context(GoalInfo),
            FailingContext = failing_context(Context,
                call_goal(PredId, ProcId)),
            GoalFailingContexts = [FailingContext]
        ;
            CanFail = cannot_fail,
            GoalFailingContexts = []
        )
    ;
        MaybeDetism = no,
        % The context in ProcInfo gives the location predicate declaration;
        % the context in the goal gives the location of the foreign_proc
        % pragma.
        Context = goal_info_get_context(GoalInfo),
        ProcPieces = describe_one_proc_name_maybe_argmodes(ModuleInfo,
            output_mercury, no, should_not_module_qualify, [],
            proc(PredId, ProcId)),
        Pieces = [words("Error: the procedure specification in this"),
            pragma_decl("foreign_proc"), words("declaration for")] ++
            ProcPieces ++
            [words("is")] ++ color_as_incorrect([words("missing")]) ++
            [words("the final"), quote("is <determinism>"),
            words("part."), nl],
        Spec = spec($pred, severity_error, phase_detism_check,
            Context, Pieces),
        det_info_add_error_spec(Spec, !DetInfo),
        Detism = detism_erroneous,
        GoalFailingContexts = []
    ).

%---------------------------------------------------------------------------%

:- pred det_infer_conj(instmap::in, soln_context::in, maybe(pess_info)::in,
    determinism::out, list(failing_context)::in,
    list(failing_context)::in, list(failing_context)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    det_info::in, det_info::out) is det.

det_infer_conj(_InstMap0, _SolnContext, _MaybePromiseEqvSolutionSets,
        detism_det, _RightFailingContexts, !ConjFailingContexts,
        [], [], !DetInfo).
det_infer_conj(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        Detism, RightFailingContexts, !ConjFailingContexts,
        [Goal0 | Goals0], [Goal | Goals], !DetInfo) :-
    % We should look to see when we get to a not_reached point
    % and optimize away the remaining elements of the conjunction.
    % But that optimization is done in the code generator anyway.

    % We infer the determinisms right-to-left, so that we can propagate
    % the SolnContext properly.

    % First, process the second and subsequent conjuncts.
    apply_goal_instmap_delta(Goal0, InstMap0, InstMap1),
    det_infer_conj(InstMap1, SolnContext, MaybePromiseEqvSolutionSets,
        TailDetism, RightFailingContexts, !ConjFailingContexts,
        Goals0, Goals, !DetInfo),
    determinism_components(TailDetism, TailCanFail, _TailMaxSolns),

    % Next, work out whether the first conjunct is in a first_soln context
    % or not. We obviously need all its solutions if we need all the solutions
    % of the conjunction. However, even if we need only the first solution
    % of the conjunction, we may need to generate more than one solution
    % of the first conjunct if the later conjuncts may possibly fail.
    ( if
        TailCanFail = cannot_fail,
        SolnContext = first_soln
    then
        HeadSolnContext = first_soln
    else
        HeadSolnContext = all_solns
    ),
    % Process the first conjunct.
    GoalRightFailingContexts = !.ConjFailingContexts ++ RightFailingContexts,
    det_infer_goal(InstMap0, HeadSolnContext, MaybePromiseEqvSolutionSets,
        HeadDetism, GoalRightFailingContexts, GoalFailingContexts,
        Goal0, Goal, !DetInfo),

    % Finally combine the results computed above.
    det_conjunction_detism(HeadDetism, TailDetism, Detism),
    !:ConjFailingContexts = GoalFailingContexts ++ !.ConjFailingContexts.

%---------------------------------------------------------------------------%

:- pred det_infer_par_conj(instmap::in, soln_context::in, maybe(pess_info)::in,
    hlds_goal_info::in, determinism::out,
    list(failing_context)::in, list(failing_context)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    det_info::in, det_info::out) is det.

det_infer_par_conj(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        GoalInfo, Detism, RightFailingContexts, GoalFailingContexts,
        Goals0, Goals, !DetInfo) :-
    det_infer_par_conj_goals(InstMap0, SolnContext,
        MaybePromiseEqvSolutionSets, Detism,
        RightFailingContexts, [], GoalFailingContexts,
        Goals0, Goals, !DetInfo),
    determinism_components(Detism, CanFail, MaxSoln),
    ( if
        CanFail = cannot_fail,
        MaxSoln \= at_most_many
    then
        true
    else
        Context = goal_info_get_context(GoalInfo),
        (
            CanFail = can_fail,
            First = "Error: parallel conjunct may fail."
        ;
            CanFail = cannot_fail,
            (
                MaxSoln = at_most_many,
                First = "Error: parallel conjunct may have multiple solutions."
            ;
                ( MaxSoln = at_most_zero
                ; MaxSoln = at_most_one
                ; MaxSoln = at_most_many_cc
                ),
                unexpected($pred,
                    "strange determinism error for parallel conjunction")
            )
        ),
        Rest = "The current implementation supports only "
            ++ "single-solution non-failing parallel conjunctions.",
        Pieces = [words(First), words(Rest), nl],
        % The switch context should be irrelevant to the problem.
        SwitchContexts = [],
        det_diagnose_conj(InstMap0, SwitchContexts, detism_det, Goals,
            GoalMsgGroups, !DetInfo),
        sort_error_msg_groups(GoalMsgGroups, SortedGoalMsgGroups),
        SortedGoalMsgs = flatten_error_msg_groups(SortedGoalMsgGroups),
        Spec = error_spec($pred, severity_error, phase_detism_check,
            [msg(Context, Pieces)] ++ SortedGoalMsgs),
        det_info_add_error_spec(Spec, !DetInfo)
    ).

:- pred det_infer_par_conj_goals(instmap::in, soln_context::in,
    maybe(pess_info)::in, determinism::out, list(failing_context)::in,
    list(failing_context)::in, list(failing_context)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    det_info::in, det_info::out) is det.

det_infer_par_conj_goals(_InstMap0, _SolnContext, _MaybePromiseEqvSolutionSets,
        detism_det, _RightFailingContexts, !ConjFailingContexts,
        [], [], !DetInfo).
det_infer_par_conj_goals(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        Detism, RightFailingContexts, !ConjFailingContexts,
        [Goal0 | Goals0], [Goal | Goals], !DetInfo) :-
    det_infer_goal(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        HeadDetism, RightFailingContexts, GoalFailingContexts,
        Goal0, Goal, !DetInfo),
    determinism_components(HeadDetism, HeadCanFail, HeadMaxSolns),

    det_infer_par_conj_goals(InstMap0, SolnContext,
        MaybePromiseEqvSolutionSets, TailDetism,
        RightFailingContexts, !ConjFailingContexts, Goals0, Goals, !DetInfo),
    determinism_components(TailDetism, TailCanFail, TailMaxSolns),

    det_conjunction_maxsoln(HeadMaxSolns, TailMaxSolns, MaxSolns),
    det_conjunction_canfail(HeadCanFail, TailCanFail, CanFail),
    determinism_components(Detism, CanFail, MaxSolns),
    !:ConjFailingContexts = GoalFailingContexts ++ !.ConjFailingContexts.

%---------------------------------------------------------------------------%

:- pred det_infer_disj(instmap::in, soln_context::in,
    maybe(pess_info)::in, hlds_goal_info::in, determinism::out,
    list(failing_context)::in, list(failing_context)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    det_info::in, det_info::out) is det.

det_infer_disj(InstMap0, SolnContext, MaybePromiseEqvSolutionSets, GoalInfo,
        Detism, RightFailingContexts, GoalFailingContexts,
        Goals0, Goals, !DetInfo) :-
    det_infer_disj_goals(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        can_fail, at_most_zero, Detism,
        RightFailingContexts, [], GoalFailingContexts0,
        Goals0, Goals, !DetInfo),
    (
        Goals = [],
        Context = goal_info_get_context(GoalInfo),
        FailingContext = failing_context(Context, fail_goal),
        GoalFailingContexts = [FailingContext | GoalFailingContexts0]
    ;
        Goals = [_ | _],
        GoalFailingContexts = GoalFailingContexts0
    ).

:- pred det_infer_disj_goals(instmap::in, soln_context::in,
    maybe(pess_info)::in, can_fail::in, soln_count::in, determinism::out,
    list(failing_context)::in,
    list(failing_context)::in, list(failing_context)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    det_info::in, det_info::out) is det.

det_infer_disj_goals(_InstMap0, _SolnContext, _MaybePromiseEqvSolutionSets,
        CanFail, MaxSolns, Detism, _RightFailingContexts, !DisjFailingContexts,
        [], [], !DetInfo) :-
    determinism_components(Detism, CanFail, MaxSolns).
det_infer_disj_goals(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        !.CanFail, !.MaxSolns, Detism,
        RightFailingContexts, !DisjFailingContexts,
        [Goal0 | Goals0], [Goal | Goals],  !DetInfo) :-
    det_infer_goal(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        FirstDetism, RightFailingContexts, GoalFailingContexts,
        Goal0, Goal, !DetInfo),
    determinism_components(FirstDetism, FirstCanFail, FirstMaxSolns),
    Goal = hlds_goal(_, GoalInfo),
    % If a disjunct cannot succeed but is marked with the
    % preserve_backtrack_into feature, treat it as being able to succeed
    % when computing the max number of solutions of the disjunction as a
    % whole, *provided* that some earlier disjuct could succeed. The idea
    % is that ( marked failure ; det ) should be treated as det, since all
    % backtracking is local within it, while disjunctions of the form
    % ( det ; marked failure ) should be treated as multi, since we want
    % to be able to backtrack to the second disjunct from *outside*
    % the disjunction. This is useful for program transformation that want
    % to get control on exits to and redos into model_non procedures.
    % Deep profiling is one such transformation.
    ( if
        !.MaxSolns \= at_most_zero,
        FirstMaxSolns = at_most_zero,
        goal_info_has_feature(GoalInfo, feature_preserve_backtrack_into)
    then
        AdjFirstMaxSolns = at_most_one
    else
        AdjFirstMaxSolns = FirstMaxSolns
    ),
    det_disjunction_canfail(!.CanFail, FirstCanFail, !:CanFail),
    det_disjunction_maxsoln(!.MaxSolns, AdjFirstMaxSolns, !:MaxSolns),
    % In single-solution contexts, convert at_most_many to at_most_many_cc.
    ( if
        SolnContext = first_soln,
        !.MaxSolns = at_most_many
    then
        !:MaxSolns = at_most_many_cc
    else
        true
    ),
    det_infer_disj_goals(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        !.CanFail, !.MaxSolns, Detism,
        RightFailingContexts, !DisjFailingContexts, Goals0, Goals, !DetInfo),
    !:DisjFailingContexts = GoalFailingContexts ++ !.DisjFailingContexts.

%---------------------------------------------------------------------------%

:- pred det_infer_switch(instmap::in, soln_context::in, maybe(pess_info)::in,
    hlds_goal_info::in, prog_var::in, can_fail::in, determinism::out,
    list(failing_context)::in, list(failing_context)::out,
    list(case)::in, list(case)::out,
    det_info::in, det_info::out) is det.

det_infer_switch(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        GoalInfo, Var, SwitchCanFail, Detism,
        RightFailingContexts, GoalFailingContexts, Cases0, Cases, !DetInfo) :-
    % The determinism of a switch is the worst of the determinism of each
    % of the cases. Also, if only a subset of the constructors are handled,
    % then it is semideterministic or worse - this is determined
    % in switch_detection.m and handled via the SwitchCanFail field.

    det_infer_switch_cases(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        Var, cannot_fail, at_most_zero, CasesDetism,
        RightFailingContexts, [], GoalFailingContexts0,
        Cases0, Cases, !DetInfo),
    determinism_components(CasesDetism, CasesCanFail, CasesSolns),
    % The switch variable tests are in a first_soln context if and only
    % if the switch goal as a whole was in a first_soln context and the
    % cases cannot fail.
    ( if
        CasesCanFail = cannot_fail,
        SolnContext = first_soln
    then
        SwitchSolnContext = first_soln
    else
        SwitchSolnContext = all_solns
    ),
    ExaminesRep = yes,
    det_check_for_noncanonical_type(Var, ExaminesRep, SwitchCanFail,
        SwitchSolnContext, GoalFailingContexts0, RightFailingContexts,
        GoalInfo, ccuc_switch, SwitchSolns, !DetInfo),
    det_conjunction_canfail(SwitchCanFail, CasesCanFail, CanFail),
    det_conjunction_maxsoln(SwitchSolns, CasesSolns, NumSolns),
    determinism_components(Detism, CanFail, NumSolns),
    (
        SwitchCanFail = can_fail,
        SwitchContext = goal_info_get_context(GoalInfo),
        FailingContext = failing_context(SwitchContext,
            incomplete_switch(Var)),
        GoalFailingContexts = [FailingContext | GoalFailingContexts0]
    ;
        SwitchCanFail = cannot_fail,
        GoalFailingContexts = GoalFailingContexts0
    ).

:- pred det_infer_switch_cases(instmap::in, soln_context::in,
    maybe(pess_info)::in, prog_var::in, can_fail::in, soln_count::in,
    determinism::out, list(failing_context)::in,
    list(failing_context)::in, list(failing_context)::out,
    list(case)::in, list(case)::out, det_info::in, det_info::out) is det.

det_infer_switch_cases(_InstMap0, _SolnContext, _MaybePromiseEqvSolutionSets,
        _Var, CanFail, MaxSolns, Detism,
        _RightFailingContexts, !SwitchFailingContexts, [], [], !DetInfo) :-
    determinism_components(Detism, CanFail, MaxSolns).
det_infer_switch_cases(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        Var, !.CanFail, !.MaxSolns, Detism,
        RightFailingContexts, !SwitchFailingContexts,
        [Case0 | Cases0], [Case | Cases], !DetInfo) :-
    % Technically, we should update the instmap to reflect the knowledge that
    % the var is bound to this particular constructor, but we wouldn't use
    % that information here anyway, so we don't bother.
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    det_info_get_module_info(!.DetInfo, ModuleInfo0),
    det_info_get_var_table(!.DetInfo, VarTable),
    lookup_var_type(VarTable, Var, VarType),
    bind_var_to_functors(Var, VarType, MainConsId, OtherConsIds,
        InstMap0, InstMap1, ModuleInfo0, ModuleInfo),
    det_info_set_module_info(ModuleInfo, !DetInfo),
    trace [compiletime(flag("debug-det-analysis-progress")), io(!IO)] (
        get_det_debug_output_stream(!.DetInfo, DebugStream, !IO),
        io.write_string(DebugStream, "inferring switch case for ", !IO),
        io.write(DebugStream, Var, !IO),
        io.write_string(DebugStream, " with main cons id ", !IO),
        io.write_line(DebugStream, MainConsId, !IO)
    ),
    det_infer_goal(InstMap1, SolnContext, MaybePromiseEqvSolutionSets,
        FirstDetism, RightFailingContexts, GoalFailingContexts,
        Goal0, Goal, !DetInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    determinism_components(FirstDetism, FirstCanFail, FirstMaxSolns),
    det_switch_canfail(!.CanFail, FirstCanFail, !:CanFail),
    det_switch_maxsoln(!.MaxSolns, FirstMaxSolns, !:MaxSolns),
    det_infer_switch_cases(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        Var, !.CanFail, !.MaxSolns, Detism,
        RightFailingContexts, !SwitchFailingContexts, Cases0, Cases, !DetInfo),
    !:SwitchFailingContexts = GoalFailingContexts ++ !.SwitchFailingContexts.

%---------------------------------------------------------------------------%

:- pred det_infer_if_then_else(instmap::in, soln_context::in,
    maybe(pess_info)::in, determinism::out,
    list(failing_context)::in, list(failing_context)::out,
    hlds_goal::in, hlds_goal::out, hlds_goal::in, hlds_goal::out,
    hlds_goal::in, hlds_goal::out, det_info::in, det_info::out) is det.

det_infer_if_then_else(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        Detism, RightFailingContexts, GoalFailingContexts,
        Cond0, Cond, Then0, Then, Else0, Else, !DetInfo) :-
    % We process the goal right-to-left, doing the `then' before the
    % condition of the if-then-else, so that we can propagate the
    % SolnContext correctly.

    % First process the `then' part.
    trace [compiletime(flag("debug-det-analysis-progress")), io(!IO)] (
        get_det_debug_output_stream(!.DetInfo, DebugStream, !IO),
        io.write_string(DebugStream, "inferring condition\n", !IO)
    ),
    apply_goal_instmap_delta(Cond0, InstMap0, InstMap1),
    det_infer_goal(InstMap1, SolnContext, MaybePromiseEqvSolutionSets,
        ThenDetism, RightFailingContexts, ThenFailingContexts,
        Then0, Then, !DetInfo),
    determinism_components(ThenDetism, ThenCanFail, ThenMaxSoln),

    % Next, work out the right soln_context to use for the condition.
    % The condition is in a first_soln context if and only if the goal as
    % a whole was in a first_soln context and the `then' part cannot fail.
    ( if
        ThenCanFail = cannot_fail,
        SolnContext = first_soln
    then
        CondSolnContext = first_soln
    else
        CondSolnContext = all_solns
    ),
    % Process the `condition' part,
    trace [compiletime(flag("debug-det-analysis-progress")), io(!IO)] (
        get_det_debug_output_stream(!.DetInfo, DebugStream, !IO),
        io.write_string(DebugStream, "inferring then-part\n", !IO)
    ),
    det_infer_goal(InstMap0, CondSolnContext, MaybePromiseEqvSolutionSets,
        CondDetism,
        ThenFailingContexts ++ RightFailingContexts, _CondFailingContexts,
        Cond0, Cond, !DetInfo),
    determinism_components(CondDetism, CondCanFail, CondMaxSoln),

    % Process the `else' part.
    trace [compiletime(flag("debug-det-analysis-progress")), io(!IO)] (
        get_det_debug_output_stream(!.DetInfo, DebugStream, !IO),
        io.write_string(DebugStream, "inferring else-part\n", !IO)
    ),
    det_infer_goal(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        ElseDetism, RightFailingContexts, ElseFailingContexts,
        Else0, Else, !DetInfo),
    determinism_components(ElseDetism, ElseCanFail, ElseMaxSoln),

    % Finally combine the results from the three parts.
    (
        CondCanFail = cannot_fail,
        % "if A then B else C" is equivalent to "A, B" if A cannot fail.
        det_conjunction_detism(CondDetism, ThenDetism, Detism)
    ;
        CondCanFail = can_fail,
        (
            CondMaxSoln = at_most_zero,
            % "if A then B else C" is equivalent to "not A, C"
            % if A cannot succeed.
            det_negation_det(CondDetism, MaybeNegDetism),
            (
                MaybeNegDetism = no,
                unexpected($pred,
                    "cannot find determinism of negated condition")
            ;
                MaybeNegDetism = yes(NegDetism)
            ),
            det_conjunction_detism(NegDetism, ElseDetism, Detism)
        ;
            ( CondMaxSoln = at_most_one
            ; CondMaxSoln = at_most_many
            ; CondMaxSoln = at_most_many_cc
            ),
            det_conjunction_maxsoln(CondMaxSoln, ThenMaxSoln, CTMaxSoln),
            det_switch_maxsoln(CTMaxSoln, ElseMaxSoln, MaxSoln),
            det_switch_canfail(ThenCanFail, ElseCanFail, CanFail),
            determinism_components(Detism, CanFail, MaxSoln)
        )
    ),
    % Failing contexts in the condition are ignored, since they can't lead
    % to failure of the if-then-else as a whole without one or more failing
    % contexts in the then part or the else part.
    GoalFailingContexts = ThenFailingContexts ++ ElseFailingContexts.

%---------------------------------------------------------------------------%

:- pred det_infer_negation(instmap::in, maybe(pess_info)::in,
    hlds_goal_info::in, determinism::out, list(failing_context)::out,
    hlds_goal::in, hlds_goal::out, det_info::in, det_info::out) is det.

det_infer_negation(InstMap0, MaybePromiseEqvSolutionSets, GoalInfo, Detism,
        GoalFailingContexts, Goal0, Goal, !DetInfo) :-
    % Negations are almost always semideterministic. It is an error for
    % a negation to further instantiate any non-local variable. Such errors
    % will be reported by the mode analysis.
    %
    % Question: should we warn about the negation of goals that either
    % cannot succeed or cannot fail?
    % Answer: yes, probably, but it's not a high priority.
    det_infer_goal(InstMap0, first_soln, MaybePromiseEqvSolutionSets,
        NegDetism, [], _NegatedFailingContexts, Goal0, Goal, !DetInfo),
    det_negation_det(NegDetism, MaybeDetism),
    (
        MaybeDetism = no,
        unexpected($pred, "inappropriate determinism inside a negation")
    ;
        MaybeDetism = yes(Detism)
    ),
    determinism_components(Detism, CanFail, _),
    (
        CanFail = can_fail,
        Context = goal_info_get_context(GoalInfo),
        GoalFailingContexts = [failing_context(Context, negated_goal)]
    ;
        CanFail = cannot_fail,
        GoalFailingContexts = []
    ).

%---------------------------------------------------------------------------%

:- pred det_infer_scope(instmap::in, soln_context::in, maybe(pess_info)::in,
    hlds_goal_info::in, scope_reason::in, determinism::out,
    list(failing_context)::in, list(failing_context)::out,
    hlds_goal::in, hlds_goal::out, det_info::in, det_info::out) is det.

det_infer_scope(InstMap0, SolnContext, MaybePromiseEqvSolutionSets0,
        GoalInfo, Reason, Detism, RightFailingContexts, GoalFailingContexts,
        Goal0, Goal, !DetInfo) :-
    % Existential quantification may require a cut to throw away solutions,
    % but we cannot rely on explicit quantification to detect this.
    % Therefore cuts are handled in det_infer_goal.
    (
        Reason = promise_solutions(Vars, Kind),
        det_info_get_var_table(!.DetInfo, VarTable),
        Context = goal_info_get_context(GoalInfo),
        (
            Kind = equivalent_solutions,
            SolnContextToUse = first_soln,
            MaybePromiseEqvSolutionSets = MaybePromiseEqvSolutionSets0
        ;
            Kind = equivalent_solution_sets,
            SolnContextToUse = SolnContext,
            (
                MaybePromiseEqvSolutionSets0 = no,
                MaybePromiseEqvSolutionSets = yes(pess_info(Vars, Context))
            ;
                MaybePromiseEqvSolutionSets0 = yes(PESSInfo),
                PESSInfo = pess_info(OuterVars, OuterContext),
                NestedPieces = [words("Warning: this"),
                    quote("promise_equivalent_solution_sets"),
                    words("scope is")] ++
                    color_as_incorrect([words("nested")]) ++
                    [words("inside another."), nl],
                NestedOuterPieces = [words("This is the outer"),
                    quote("promise_equivalent_solution_sets"),
                    words("scope."), nl],
                NestedSeverity = severity_warning(warn_dodgy_simple_code),
                NestedSpec = error_spec($pred,
                    NestedSeverity, phase_detism_check,
                    [msg(Context, NestedPieces),
                    msg(OuterContext, NestedOuterPieces)]),
                det_info_add_error_spec(NestedSpec, !DetInfo),
                AllVars = set_of_var.list_to_set(OuterVars ++ Vars),
                MaybePromiseEqvSolutionSets =
                    yes(pess_info(set_of_var.to_sorted_list(AllVars),
                        OuterContext))
            )
        ;
            Kind = equivalent_solution_sets_arbitrary,
            (
                MaybePromiseEqvSolutionSets0 = no,
                ArbitraryPieces = [words("Error: this"),
                    quote("arbitrary"), words("scope is")] ++
                    color_as_incorrect([words("not nested")]) ++
                    [words("inside a"),
                    quote("promise_equivalent_solution_sets"),
                    words("scope."), nl],
                ArbitrarySpec = spec($pred, severity_error,
                    phase_detism_check, Context, ArbitraryPieces),
                det_info_add_error_spec(ArbitrarySpec, !DetInfo)
            ;
                MaybePromiseEqvSolutionSets0 = yes(pess_info(OldVars,
                    PromiseContext)),
                OverlapVars = set_of_var.intersect(
                    set_of_var.list_to_set(OldVars),
                    set_of_var.list_to_set(Vars)),
                OverlapVarList = set_of_var.to_sorted_list(OverlapVars),
                (
                    OverlapVarList = []
                ;
                    OverlapVarList = [_HeadOverlapVar | TailOverlapVars],
                    OverlapVarPieces = list.map(
                        var_in_table_to_quote_piece(VarTable), OverlapVarList),
                    OverlapVarDotPieces = piece_list_to_color_pieces(
                        color_subject, "and", [suffix(".")], OverlapVarPieces),
                    (
                        TailOverlapVars = [],
                        OverlapVarStr = "the variable"
                    ;
                        TailOverlapVars = [_ | _],
                        OverlapVarStr = "the following variables:"
                    ),
                    OverlapPieces = [words("Error: this"), quote("arbitrary"),
                        words("scope and the"),
                        quote("promise_equivalent_solution_sets"),
                        words("scope it is nested inside")] ++
                        color_as_incorrect([words("overlap")]) ++
                        [words("on"), words(OverlapVarStr)] ++
                        OverlapVarDotPieces ++ [nl],
                    OverlapPromisePieces = [words("This is the outer"),
                        quote("promise_equivalent_solution_sets"),
                        words("scope."), nl],
                    OverlapSpec = error_spec($pred, severity_error,
                        phase_detism_check,
                        [msg(Context, OverlapPieces),
                        msg(PromiseContext, OverlapPromisePieces)]),
                    det_info_add_error_spec(OverlapSpec, !DetInfo)
                )
            ),
            MaybePromiseEqvSolutionSets = no,
            SolnContextToUse = first_soln
        ),
        InstmapDelta = goal_info_get_instmap_delta(GoalInfo),
        instmap_delta_changed_vars(InstmapDelta, ChangedVars),
        det_info_get_module_info(!.DetInfo, ModuleInfo),
        % BoundVars must include both vars whose inst has changed and vars
        % with inst any which may have been further constrained by the goal.
        set_of_var.divide(
            var_is_ground_in_instmap(ModuleInfo, VarTable, InstMap0),
            ChangedVars, _GroundAtStartVars, GroundBoundVars),
        NonLocalVars = goal_info_get_nonlocals(GoalInfo),
        AnyBoundVars = set_of_var.filter(
            var_is_any_in_instmap(ModuleInfo, InstMap0),
            NonLocalVars),
        BoundVars0 = set_of_var.union(GroundBoundVars, AnyBoundVars),
        BoundVars = remove_typeinfo_vars_from_set_of_var(VarTable, BoundVars0),

        % Which vars were bound inside the scope but not listed
        % in the promise_equivalent_solution{s,_sets} or arbitrary scope?
        set_of_var.difference(BoundVars, set_of_var.list_to_set(Vars),
            MissingVars),
        MissingVarList = set_of_var.to_sorted_list(MissingVars),
        (
            MissingVarList = []
        ;
            MissingVarList = [_HeadMissingVar | TailMissingVars],
            MissingVarPieces = list.map(var_in_table_to_quote_piece(VarTable),
                MissingVarList),
            MissingVarDotPieces = piece_list_to_color_pieces(color_subject,
                "and", [suffix(".")], MissingVarPieces),
            MissingKindStr = promise_solutions_kind_str(Kind),
            (
                TailMissingVars = [],
                MissingListStr = "a variable that is not listed:"
            ;
                TailMissingVars = [_ | _],
                MissingListStr = "some variables that are not listed:"
            ),
            ( if
                set_of_var.member(MissingVars, MissingVar),
                set_of_var.member(AnyBoundVars, MissingVar)
            then
                Binds = "may constrain"
            else
                Binds = "binds"
            ),
            MissingPieces = [words("Error: the"), quote(MissingKindStr),
                words("goal")] ++
                color_as_incorrect([words(Binds), words(MissingListStr)]) ++
                MissingVarDotPieces ++ [nl],
            MissingSpec = spec($pred, severity_error, phase_detism_check,
                Context, MissingPieces),
            det_info_add_error_spec(MissingSpec, !DetInfo)
        ),
        % Which vars were listed in the promise_equivalent_solutions
        % but not bound inside the scope?
        set_of_var.difference(set_of_var.list_to_set(Vars),
            BoundVars, ExtraVars),
        det_info_get_pess_extra_vars(!.DetInfo, IgnoreExtraVars),
        ExtraVarList = set_of_var.to_sorted_list(ExtraVars),
        (
            ExtraVarList = []
        ;
            ExtraVarList = [_HeadExtraVar | TailExtraVars],
            ExtraVarPieces =
                list.map(var_in_table_to_quote_piece(VarTable), ExtraVarList),
            (
                IgnoreExtraVars = pess_extra_vars_ignore
            ;
                IgnoreExtraVars = pess_extra_vars_report,
                ExtraVarDotPieces = piece_list_to_color_pieces(color_subject,
                    "and",  [suffix(".")], ExtraVarPieces),
                ExtraKindStr = promise_solutions_kind_str(Kind),
                (
                    TailExtraVars = [],
                    ExtraListStr = "an extra variable:"
                ;
                    TailExtraVars = [_ | _],
                    ExtraListStr = "some extra variables:"
                ),
                ExtraPieces = [words("Error: the"), quote(ExtraKindStr),
                    words("goal")] ++
                    color_as_incorrect([words("lists"),
                        words(ExtraListStr)]) ++
                    ExtraVarDotPieces ++ [nl],
                ExtraSpec = spec($pred, severity_error, phase_detism_check,
                    Context, ExtraPieces),
                det_info_add_error_spec(ExtraSpec, !DetInfo)
            )
        ),
        det_infer_goal(InstMap0, SolnContextToUse, MaybePromiseEqvSolutionSets,
            Detism, RightFailingContexts, GoalFailingContexts,
            Goal0, Goal, !DetInfo)
    ;
        Reason = trace_goal(_, _, _, _, _),
        det_infer_goal(InstMap0, SolnContext, MaybePromiseEqvSolutionSets0,
            Detism0, RightFailingContexts, GoalFailingContexts,
            Goal0, Goal, !DetInfo),
        ( if
            % Since the trace goal may not be enabled, it would be incorrect
            % to say that it ALWAYS aborts. That is why we convert a detism
            % of detism_erroneous inside the scope to detism_det outside the
            % scope.
            (
                Detism0 = detism_det,
                Detism1 = detism_det
            ;
                Detism0 = detism_cc_multi,
                Detism1 = detism_cc_multi
            ;
                Detism0 = detism_erroneous,
                Detism1 = detism_det
            )
        then
            Detism = Detism1
        else
            Detism = Detism0,
            Context = goal_info_get_context(GoalInfo),
            DetismStr = determinism_to_string(Detism),
            Pieces = [words("Error: trace goal has determinism")] ++
                color_as_incorrect([quote(DetismStr), suffix(",")]) ++
                [words("it should be either")] ++
                color_as_correct([words("det")]) ++ [words("or")] ++
                color_as_correct([words("cc_multi.")]) ++ [nl],
            Spec = spec($pred, severity_error, phase_detism_check,
                Context, Pieces),
            det_info_add_error_spec(Spec, !DetInfo)
        )
    ;
        ( Reason = exist_quant(_, _)
        ; Reason = disable_warnings(_, _)
        ; Reason = promise_purity(_)
        ; Reason = commit(_)
        ; Reason = barrier(_)
        ),
        det_infer_goal(InstMap0, SolnContext, MaybePromiseEqvSolutionSets0,
            Detism, RightFailingContexts, GoalFailingContexts,
            Goal0, Goal, !DetInfo)
    ;
        ( Reason = require_detism(_)
        ; Reason = require_complete_switch(_)
        ; Reason = require_switch_arms_detism(_, _)
        ),
        det_info_set_has_req_scope(!DetInfo),
        det_infer_goal(InstMap0, SolnContext, MaybePromiseEqvSolutionSets0,
            Detism, RightFailingContexts, GoalFailingContexts,
            Goal0, Goal, !DetInfo)
    ;
        Reason = loop_control(_, _, _),
        det_infer_goal(InstMap0, SolnContext, MaybePromiseEqvSolutionSets0,
            Detism, RightFailingContexts, GoalFailingContexts,
            Goal0, Goal, !DetInfo),
        (
            ( Detism = detism_det
            ; Detism = detism_cc_multi
            )
        ;
            ( Detism = detism_semi
            ; Detism = detism_multi
            ; Detism = detism_non
            ; Detism = detism_cc_non
            ; Detism = detism_failure
            % Note: One day we should make exceptions in parallel
            % conjunctions work.
            ; Detism = detism_erroneous
            ),
            % Since loop control structures are generated only by the
            % compiler it is reasonable to abort here.
            unexpected($pred, "Loop control scope with strange determinism")
        )
    ;
        Reason = from_ground_term(_, FromGroundTermKind),
        (
            FromGroundTermKind = from_ground_term_construct,
            Goal = Goal0,
            Detism = detism_det,
            GoalFailingContexts = []
        ;
            ( FromGroundTermKind = from_ground_term_deconstruct
            ; FromGroundTermKind = from_ground_term_other
            ),
            det_infer_goal(InstMap0, SolnContext, MaybePromiseEqvSolutionSets0,
                Detism, RightFailingContexts, GoalFailingContexts,
                Goal0, Goal, !DetInfo)
        ;
            FromGroundTermKind = from_ground_term_initial,
            unexpected($pred, "from_ground_term_initial")
        )
    ).

    % Return a printable representation of the given promise_solutions_kind.
    %
:- func promise_solutions_kind_str(promise_solutions_kind) = string.

promise_solutions_kind_str(equivalent_solutions)
    = "promise_equivalent_solutions".
promise_solutions_kind_str(equivalent_solution_sets)
    = "promise_equivalent_solution_sets".
promise_solutions_kind_str(equivalent_solution_sets_arbitrary)
    = "arbitrary".

%---------------------------------------------------------------------------%

:- pred det_infer_atomic(instmap::in, soln_context::in, maybe(pess_info)::in,
    list(failing_context)::in, determinism::out,
    hlds_goal::in, hlds_goal::out, list(hlds_goal)::in, list(hlds_goal)::out,
    det_info::in, det_info::out) is det.

det_infer_atomic(InstMap0, SolnContext, MaybePromiseEqvSolutionSets0,
        RightFailingContexts, Detism,
        MainGoal0, MainGoal, OrElseGoals0, OrElseGoals, !DetInfo) :-
    det_infer_atomic_goal(InstMap0, SolnContext, MaybePromiseEqvSolutionSets0,
        RightFailingContexts, MainDetism, MainGoal0, MainGoal, !DetInfo),
    (
        OrElseGoals0 = [],
        OrElseGoals = [],
        Detism = MainDetism
    ;
        OrElseGoals0 = [_ | _],
        determinism_components(MainDetism, MainCanFail, MainMaxSolns),
        det_infer_orelse_goals(InstMap0, SolnContext,
            MaybePromiseEqvSolutionSets0, RightFailingContexts,
            MainCanFail, CanFail, MainMaxSolns, MaxSolns0,
            OrElseGoals0, OrElseGoals, !DetInfo),
        (
            MaxSolns0 = at_most_zero,
            MaxSolns = at_most_zero
        ;
            MaxSolns0 = at_most_one,
            % The final solution is given by the main goal or one of the
            % orelse goals; whichever succeeds first. This effectively makes
            % the atomic scope commit to the first of several possible
            % solutions.
            MaxSolns = at_most_many_cc
        ;
            MaxSolns0 = at_most_many_cc,
            MaxSolns = at_most_many_cc
        ;
            MaxSolns0 = at_most_many,
            MaxSolns = at_most_many
        ),
        determinism_components(Detism, CanFail, MaxSolns)
    ).

:- pred det_infer_atomic_goal(instmap::in, soln_context::in,
    maybe(pess_info)::in, list(failing_context)::in, determinism::out,
    hlds_goal::in, hlds_goal::out, det_info::in, det_info::out) is det.

det_infer_atomic_goal(InstMap0, SolnContext, MaybePromiseEqvSolutionSets0,
        RightFailingContexts, Detism, Goal0, Goal, !DetInfo) :-
    det_infer_goal(InstMap0, SolnContext, MaybePromiseEqvSolutionSets0, Detism,
        RightFailingContexts, GoalFailingContexts, Goal0, Goal, !DetInfo),
    (
        ( Detism = detism_det
        ; Detism = detism_cc_multi
        ; Detism = detism_erroneous
        ),
        % XXX STM Detism = detism_cc_multi            % <== TMP
        expect(unify(GoalFailingContexts, []), $pred,
            "GoalFailingContexts != []")
    ;
        ( Detism = detism_semi
        ; Detism = detism_multi
        ; Detism = detism_non
        ; Detism = detism_cc_non
        ; Detism = detism_failure
        ),
        Goal0 = hlds_goal(_, GoalInfo0),
        Context = goal_info_get_context(GoalInfo0),
        DetismStr = determinism_to_string(Detism),
        Pieces = [words("Error: atomic goal has determinism"),
            quote(DetismStr), suffix(","),
            words("should be det or cc_multi."), nl],
        Spec = spec($pred, severity_error, phase_detism_check,
            Context, Pieces),
        det_info_add_error_spec(Spec, !DetInfo)
    ).

:- pred det_infer_orelse_goals(instmap::in, soln_context::in,
    maybe(pess_info)::in, list(failing_context)::in,
    can_fail::in, can_fail::out, soln_count::in, soln_count::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    det_info::in, det_info::out) is det.

det_infer_orelse_goals(_InstMap0, _SolnContext, _MaybePromiseEqvSolutionSets,
        _RightFailingContexts, !CanFail, !MaxSolns, [], [], !DetInfo).
det_infer_orelse_goals(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        RightFailingContexts, !CanFail, !MaxSolns,
        [Goal0 | Goals0], [Goal | Goals], !DetInfo) :-
    det_infer_atomic_goal(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        RightFailingContexts, FirstDetism, Goal0, Goal, !DetInfo),
    determinism_components(FirstDetism, FirstCanFail, FirstMaxSolns),
    det_switch_canfail(!.CanFail, FirstCanFail, !:CanFail),
    det_switch_maxsoln(!.MaxSolns, FirstMaxSolns, !:MaxSolns),
    det_infer_orelse_goals(InstMap0, SolnContext, MaybePromiseEqvSolutionSets,
        RightFailingContexts, !CanFail, !MaxSolns, Goals0, Goals, !DetInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Predicates needed to process more than one kind of goal.
%

:- type cc_unify_context
    --->    ccuc_unify(unify_context)
    ;       ccuc_switch.

:- pred det_check_for_noncanonical_type(prog_var::in, bool::in, can_fail::in,
    soln_context::in, list(failing_context)::in, list(failing_context)::in,
    hlds_goal_info::in, cc_unify_context::in, soln_count::out,
    det_info::in, det_info::out) is det.

det_check_for_noncanonical_type(Var, ExaminesRepresentation, CanFail,
        SolnContext, FailingContextsA, FailingContextsB, GoalInfo, GoalContext,
        NumSolns, !DetInfo) :-
    ( if
        % Check for unifications that attempt to examine the representation
        % of a type that does not have a single representation for each
        % abstract value.
        ExaminesRepresentation = yes,
        det_info_get_var_table(!.DetInfo, VarTable),
        lookup_var_type(VarTable, Var, Type),
        det_type_has_user_defined_equality_pred(!.DetInfo, Type)
    then
        (
            CanFail = can_fail,
            Context = goal_info_get_context(GoalInfo),
            (
                GoalContext = ccuc_switch,
                VarPiece = var_in_table_to_quote_piece(VarTable, Var),
                Pieces0 = [words("In switch on variable"), VarPiece,
                    suffix(":"), nl]
            ;
                GoalContext = ccuc_unify(UnifyContext),
                unify_context_to_pieces(UnifyContext, _LastContextWord,
                    [], Pieces0)
            ),
            Pieces1 = [lower_case_next_if_not_first, words("Error:")] ++
                color_as_subject([words("unification for non-canonical type"),
                    qual_top_ctor_of_type(Type)]) ++
                [words("is")] ++
                color_as_incorrect([words("not guaranteed to succeed.")]) ++
                [nl],
            VerbosePieces = noncanon_unify_verbose_preamble ++
                [words("The success of this unification might depend on"),
                words("the choice of concrete representation."),
                words("Figuring out whether there is a solution"),
                words("to this unification")] ++
                noncanon_unify_verbose_would_require,
            Spec = error_spec($pred, severity_error, phase_detism_check,
                [simple_msg(Context,
                    [always(Pieces0 ++ Pieces1),
                    verbose_only(verbose_once, VerbosePieces)])]),
            det_info_add_error_spec(Spec, !DetInfo)
        ;
            CanFail = cannot_fail,
            (
                SolnContext = all_solns,
                Context = goal_info_get_context(GoalInfo),
                (
                    GoalContext = ccuc_switch,
                    VarPiece = var_in_table_to_quote_piece(VarTable, Var),
                    Pieces0 = [words("In switch on variable"), VarPiece,
                        suffix(":"), nl]
                ;
                    GoalContext = ccuc_unify(UnifyContext),
                    unify_context_first_to_pieces(is_first, _,
                        UnifyContext, _LastContextWord, [], Pieces0)
                ),
                Pieces1 = [lower_case_next_if_not_first, words("Error:")] ++
                    color_as_subject(
                        [words("unification for non-canonical type"),
                        qual_top_ctor_of_type(Type)]) ++
                    [words("occurs in a context that")] ++
                    color_as_incorrect(
                        [words("requires all solutions.")]) ++
                    [nl],
                VerbosePieces = noncanon_unify_verbose_preamble ++
                    [words("The results of this unification might depend on"),
                    words("the choice of concrete representation."),
                    words("Finding all possible solutions"),
                    words("to this unification")] ++
                    noncanon_unify_verbose_would_require,
                det_info_get_module_info(!.DetInfo, ModuleInfo),
                ContextMsgs = failing_contexts_description(ModuleInfo,
                    VarTable, FailingContextsA ++ FailingContextsB),
                Spec = error_spec($pred, severity_error, phase_detism_check,
                    [simple_msg(Context,
                        [always(Pieces0 ++ Pieces1),
                        verbose_only(verbose_once, VerbosePieces)])]
                    ++ ContextMsgs),
                det_info_add_error_spec(Spec, !DetInfo)
            ;
                SolnContext = first_soln
            )
        ),
        (
            SolnContext = first_soln,
            NumSolns = at_most_many_cc
        ;
            SolnContext = all_solns,
            NumSolns = at_most_many
        )
    else
        NumSolns = at_most_one
    ).

%---------------------%

    % Return true iff the principal type constructor of the given type
    % has user-defined equality.
    %
:- pred det_type_has_user_defined_equality_pred(det_info::in,
    mer_type::in) is semidet.

det_type_has_user_defined_equality_pred(DetInfo, Type) :-
    det_info_get_module_info(DetInfo, ModuleInfo),
    type_has_user_defined_equality_pred(ModuleInfo, Type, _).

%---------------------%

    % Return yes iff the results of the specified unification might depend
    % on the concrete representation of the abstract values involved.
    %
:- pred det_infer_unify_examines_rep(unification::in, bool::out) is det.

det_infer_unify_examines_rep(assign(_, _), no).
det_infer_unify_examines_rep(construct(_, _, _, _, _, _, _), no).
det_infer_unify_examines_rep(deconstruct(_, _, _, _, _, _), yes).
det_infer_unify_examines_rep(simple_test(_, _), yes).
    % Some complicated modes of complicated unifications _do_
    % examine the representation...
    % but we will catch those by reporting errors in the
    % compiler-generated code for the complicated unification.
det_infer_unify_examines_rep(complicated_unify(_, _, _), no).

%---------------------%

    % Deconstruction unifications cannot fail if the type only has one
    % constructor, or if the variable is known to be already bound
    % to the appropriate functor.
    %
    % This is handled (modulo bugs) by modes.m, which sets the appropriate
    % field in the deconstruct(...) to can_fail for those deconstruction
    % unifications which might fail. But switch_detection.m may set it back
    % to cannot_fail again, if it moves the functor test into a switch instead.
    %
:- pred det_infer_unify_canfail(unification::in, can_fail::out) is det.

det_infer_unify_canfail(deconstruct(_, _, _, _, CanFail, _), CanFail).
det_infer_unify_canfail(assign(_, _), cannot_fail).
det_infer_unify_canfail(construct(_, _, _, _, _, _, _), cannot_fail).
det_infer_unify_canfail(simple_test(_, _), can_fail).
det_infer_unify_canfail(complicated_unify(_, CanFail, _), CanFail).

%---------------------%

:- func noncanon_unify_verbose_preamble = list(format_piece).

noncanon_unify_verbose_preamble =
    [words("Since the type has a user-defined equality predicate,"),
    words("I must presume that there is more than one possible concrete"),
    words("representation for each abstract value of this type.")].

:- func noncanon_unify_verbose_would_require = list(format_piece).

noncanon_unify_verbose_would_require =
    [words("would require backtracking over all possible representations,"),
    words("but I am not going to do that implicitly."),
    words("(If that is really what you want, you must do it explicitly.)"),
    nl].

%---------------------------------------------------------------------------%

    % Describe the given list of failing contexts.
    %
:- func failing_contexts_description(module_info, var_table,
    list(failing_context)) = list(error_msg).

failing_contexts_description(ModuleInfo, VarTable, FailingContexts) =
    list.map(failing_context_description(ModuleInfo, VarTable),
        FailingContexts).

:- func failing_context_description(module_info, var_table,
    failing_context) = error_msg.

failing_context_description(ModuleInfo, VarTable, FailingContext) = Msg :-
    FailingContext = failing_context(Context, FailingGoal),
    (
        FailingGoal = incomplete_switch(Var),
        VarPiece = var_in_table_to_quote_piece(VarTable, Var),
        Pieces = [words("The")] ++
            color_as_subject([words("switch on"), VarPiece]) ++
            [words("is")] ++
            color_as_incorrect([words("incomplete.")]) ++ [nl]
    ;
        FailingGoal = fail_goal,
        Pieces = color_as_subject([words("Fail goal")]) ++
            color_as_incorrect([words("can fail.")]) ++ [nl]
    ;
        FailingGoal = test_goal(VarA, VarB),
        VarPieceA = var_in_table_to_quote_piece(VarTable, VarA),
        VarPieceB = var_in_table_to_quote_piece(VarTable, VarB),
        Pieces = [words("Unification of")] ++ color_as_subject([VarPieceA]) ++
            [words("and")] ++ color_as_subject([VarPieceB]) ++
            color_as_incorrect([words("can fail.")]) ++ [nl]
    ;
        FailingGoal = deconstruct_goal(Var, ConsId),
        VarPiece = var_in_table_to_quote_piece(VarTable, Var),
        Pieces = [words("Unification of")] ++ color_as_subject([VarPiece]) ++
            [words("with")] ++
            color_as_subject([qual_cons_id_and_maybe_arity(ConsId)]) ++
            color_as_incorrect([words("can fail.")]) ++ [nl]
    ;
        FailingGoal = call_goal(PredId, _ProcId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        Name = pred_info_name(PredInfo),
        Pieces = [words("Call to")] ++ color_as_subject([fixed(Name)]) ++
            color_as_incorrect([words("can fail.")]) ++ [nl]
    ;
        FailingGoal = generic_call_goal(GenericCall),
        VarNameSrc = vns_var_table(VarTable),
        GenericCallPieces =
            generic_call_to_pieces(print_ho_var_name, VarNameSrc, GenericCall),
        Pieces = color_as_subject([upper_case_next | GenericCallPieces]) ++
            color_as_incorrect([words("can fail.")]) ++ [nl]
    ;
        FailingGoal = negated_goal,
        Pieces = color_as_subject([words("Negated goal")]) ++
            color_as_incorrect([words("can fail.")]) ++ [nl]
    ),
    Msg = msg(Context, Pieces).

%---------------------------------------------------------------------------%

:- pred get_det_debug_output_stream(det_info::in, io.text_output_stream::out,
    io::di, io::uo) is det.

get_det_debug_output_stream(DetInfo, DebugStream, !IO) :-
    det_info_get_module_info(DetInfo, ModuleInfo),
    get_debug_output_stream(ModuleInfo, DebugStream, !IO).

%---------------------------------------------------------------------------%
:- end_module check_hlds.det_infer_goal.
%---------------------------------------------------------------------------%
