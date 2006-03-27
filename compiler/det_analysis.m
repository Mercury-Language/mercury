%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: det_analysis.m - the determinism analysis pass.
% Main authors: conway, fjh, zs.

% This pass has three components:
%
% - Segregate the procedures into those that have determinism declarations,
%   and those that don't.
%
% - A step of performing a local inference pass on each procedure
%   without a determinism declaration is iterated until a fixpoint is reached.
%
% - A checking step is performed on all the procedures that have determinism
%   declarations to ensure that they are at least as deterministic as their
%   declaration. This uses a form of the local inference pass.
%
% If we are to avoid global inference for predicates with declarations, then
% it must be an error, not just a warning, if the determinism checking step
% detects that the determinism annotation was wrong.  If we were to issue just
% a warning, then we would have to override the determinism annotation, and
% that would force us to re-check the inferred determinism for all calling
% predicates.
%
% Alternately, we could leave it as a warning, but then we would have to
% _make_ the predicate deterministic (or semideterministic) by inserting
% run-time checking code which calls error/1 if the predicate really isn't
% deterministic (semideterministic).

% Determinism has three components:
%
%   whether a goal can fail
%   whether a goal has more than one possible solution
%   whether a goal occurs in a context where only the first solution
%       is required
%
% The first two components are synthesized attributes: they are inferred
% bottom-up. The last component is an inherited attribute: it is propagated
% top-down.

%-----------------------------------------------------------------------------%

:- module check_hlds.det_analysis.
:- interface.

:- import_module check_hlds.det_report.
:- import_module check_hlds.det_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module libs.globals.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module std_util.

%-----------------------------------------------------------------------------%

    % Perform determinism inference for local predicates with no determinism
    % declarations, and determinism checking for all other predicates.
    %
:- pred determinism_pass(module_info::in, module_info::out,
    io::di, io::uo) is det.

    % Check the determinism of a single procedure (only works if the
    % determinism of the procedures it calls has already been inferred).
    %
:- pred determinism_check_proc(proc_id::in, pred_id::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

    % Infer the determinism of a procedure.
    %
:- pred det_infer_proc(pred_id::in, proc_id::in, module_info::in,
    module_info::out, globals::in, determinism::out, determinism::out,
    list(context_det_msg)::out) is det.

:- type pess_info   % short for promise_equivalent_solution_sets_info
    --->    pess_info(prog_vars, prog_context).

    % Infers the determinism of `Goal0' and returns this in `Detism'.
    % It annotates the goal and all its subgoals with their determinism
    % and returns the annotated goal in `Goal'.
    %
:- pred det_infer_goal(hlds_goal::in, hlds_goal::out, instmap::in,
    soln_context::in, list(failing_context)::in, maybe(pess_info)::in,
    det_info::in, determinism::out,
    list(failing_context)::out, list(context_det_msg)::out) is det.

    % Work out how many solutions are needed for a given determinism.
    %
:- pred det_get_soln_context(determinism::in, soln_context::out) is det.

:- type soln_context
    --->    all_solns
    ;       first_soln.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.modecheck_call.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.purity.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_out.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module libs.compiler_util.
:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.

:- import_module assoc_list.
:- import_module bool.
:- import_module map.
:- import_module set.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

determinism_pass(!ModuleInfo, !IO) :-
    determinism_declarations(!.ModuleInfo, DeclaredProcs,
        UndeclaredProcs, NoInferProcs),
    list.foldl(set_non_inferred_proc_determinism, NoInferProcs, !ModuleInfo),
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    globals.io_lookup_bool_option(debug_det, Debug, !IO),
    (
        UndeclaredProcs = []
    ;
        UndeclaredProcs = [_ | _],
        maybe_write_string(Verbose, "% Doing determinism inference...\n", !IO),
        global_inference_pass(!ModuleInfo, UndeclaredProcs, Debug, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ),
    maybe_write_string(Verbose, "% Doing determinism checking...\n", !IO),
    global_final_pass(!ModuleInfo, DeclaredProcs, Debug, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO).

determinism_check_proc(ProcId, PredId, !ModuleInfo, !IO) :-
    globals.io_lookup_bool_option(debug_det, Debug, !IO),
    global_final_pass(!ModuleInfo, [proc(PredId, ProcId)], Debug, !IO).

%-----------------------------------------------------------------------------%

:- pred global_inference_pass(module_info::in, module_info::out,
    pred_proc_list::in, bool::in, io::di, io::uo) is det.

    % Iterate until a fixpoint is reached. This can be expensive if a module
    % has many predicates with undeclared determinisms. If this ever becomes
    % a problem, we should switch to doing iterations only on strongly
    % connected components of the dependency graph.
    %
global_inference_pass(!ModuleInfo, ProcList, Debug, !IO) :-
    global_inference_single_pass(ProcList, Debug, !ModuleInfo, [], Msgs,
        unchanged, Changed, !IO),
    maybe_write_string(Debug, "% Inference pass complete\n", !IO),
    (
        Changed = changed,
        global_inference_pass(!ModuleInfo, ProcList, Debug, !IO)
    ;
        Changed = unchanged,
        % We have arrived at a fixpoint. Therefore all the messages we have
        % are based on the final determinisms of all procedures, which means
        % it is safe to print them.
        det_report_and_handle_msgs(Msgs, !ModuleInfo, !IO)
    ).

:- pred global_inference_single_pass(pred_proc_list::in, bool::in,
    module_info::in, module_info::out,
    list(context_det_msg)::in, list(context_det_msg)::out,
    maybe_changed::in, maybe_changed::out, io::di, io::uo) is det.

global_inference_single_pass([], _, !ModuleInfo, !Msgs, !Changed, !IO).
global_inference_single_pass([proc(PredId, ProcId) | PredProcs], Debug,
        !ModuleInfo, !Msgs, !Changed, !IO) :-
    globals.io_get_globals(Globals, !IO),
    det_infer_proc(PredId, ProcId, !ModuleInfo, Globals, OldDetism, NewDetism,
        ProcMsgs),
    ( NewDetism = OldDetism ->
        ChangeStr = "old"
    ;
        ChangeStr = "new",
        !:Changed = changed
    ),
    (
        Debug = yes,
        io.write_string("% Inferred " ++ ChangeStr ++ " detism ", !IO),
        mercury_output_det(NewDetism, !IO),
        io.write_string(" for ", !IO),
        hlds_out.write_pred_proc_id(!.ModuleInfo, PredId, ProcId, !IO),
        io.write_string("\n", !IO)
    ;
        Debug = no
    ),
    list.append(ProcMsgs, !Msgs),
    global_inference_single_pass(PredProcs, Debug, !ModuleInfo, !Msgs,
        !Changed, !IO).

:- pred global_final_pass(module_info::in, module_info::out,
    pred_proc_list::in, bool::in, io::di, io::uo) is det.

global_final_pass(!ModuleInfo, ProcList, Debug, !IO) :-
    global_inference_single_pass(ProcList, Debug, !ModuleInfo, [], Msgs,
        unchanged, _, !IO),
    det_report_and_handle_msgs(Msgs, !ModuleInfo, !IO),
    global_checking_pass(ProcList, !ModuleInfo, !IO).

%-----------------------------------------------------------------------------%

det_infer_proc(PredId, ProcId, !ModuleInfo, Globals, OldDetism, NewDetism,
        !:Msgs) :-

    % Get the proc_info structure for this procedure.
    module_info_preds(!.ModuleInfo, Preds0),
    map.lookup(Preds0, PredId, Pred0),
    pred_info_get_procedures(Pred0, Procs0),
    map.lookup(Procs0, ProcId, Proc0),

    % Remember the old inferred determinism of this procedure.
    proc_info_get_inferred_determinism(Proc0, OldDetism),

    % Work out whether or not the procedure occurs in a single-solution
    % context. Currently we only assume so if the predicate has an explicit
    % determinism declaration that says so.
    det_get_soln_context(OldDetism, OldInferredSolnContext),
    proc_info_get_declared_determinism(Proc0, MaybeDeclaredDetism),
    (
        MaybeDeclaredDetism = yes(DeclaredDetism),
        det_get_soln_context(DeclaredDetism, DeclaredSolnContext)
    ;
        MaybeDeclaredDetism = no,
        DeclaredSolnContext = all_solns
    ),
    (
        ( DeclaredSolnContext = first_soln
        ; OldInferredSolnContext = first_soln
        )
    ->
        SolnContext = first_soln
    ;
        SolnContext = all_solns
    ),

    % Infer the determinism of the goal.
    proc_info_get_goal(Proc0, Goal0),
    proc_info_get_initial_instmap(Proc0, !.ModuleInfo, InstMap0),
    proc_info_get_vartypes(Proc0, VarTypes),
    det_info_init(!.ModuleInfo, VarTypes, PredId, ProcId, Globals, DetInfo),
    det_infer_goal(Goal0, Goal, InstMap0, SolnContext, [], no, DetInfo,
        InferDetism, _, !:Msgs),

    % Take the worst of the old and inferred detisms. This is needed to prevent
    % loops on p :- not(p), at least if the initial assumed detism is det.
    % This may also be needed to ensure that we don't change the interface
    % determinism of procedures, if we are re-running determinism analysis.

    determinism_components(OldDetism, OldCanFail, OldMaxSoln),
    determinism_components(InferDetism, InferCanFail, InferMaxSoln),
    det_switch_canfail(OldCanFail, InferCanFail, CanFail),
    det_switch_maxsoln(OldMaxSoln, InferMaxSoln, MaxSoln),
    determinism_components(TentativeDetism, CanFail, MaxSoln),

    % Now see if the evaluation model can change the detism.
    proc_info_get_eval_method(Proc0, EvalMethod),
    NewDetism = eval_method_change_determinism(EvalMethod, TentativeDetism),
    (
        proc_info_has_io_state_pair(!.ModuleInfo, Proc0, _InArg, _OutArg),
        (
            MaybeDeclaredDetism = yes(ToBeCheckedDetism)
        ;
            MaybeDeclaredDetism = no,
            ToBeCheckedDetism = NewDetism
        ),
        determinism_to_code_model(ToBeCheckedDetism, ToBeCheckedCodeModel),
        ToBeCheckedCodeModel \= model_det
    ->
        proc_info_get_context(Proc0, ProcContext),
        IOStateMsg = has_io_state_but_not_det(PredId, ProcId),
        IOStateContextMsg = context_det_msg(ProcContext, IOStateMsg),
        !:Msgs = [IOStateContextMsg | !.Msgs]
    ;
        true
    ),

    % Check to make sure that if this procedure is exported to C via a
    % pragma export declaration then the determinism is not multi or nondet
    % - pragma exported procs that have been declared to have these
    % determinisms should have been picked up in make_hlds, so this is just
    % to catch those whose determinisms need to be inferred.
    module_info_get_pragma_exported_procs(!.ModuleInfo, ExportedProcs),
    (
        list.member(pragma_exported_proc(PredId, ProcId, _, _), ExportedProcs),
        ( NewDetism = multidet
        ; NewDetism = nondet
        )
    ->
        (
            get_exported_proc_context(ExportedProcs, PredId, ProcId,
                PragmaContext)
        ->
            ExportMsg = export_model_non_proc(PredId, ProcId, NewDetism),
            ExportContextMsg = context_det_msg(PragmaContext, ExportMsg),
            list.cons(ExportContextMsg, !Msgs)
        ;
            unexpected(this_file,
                "Cannot find proc in table of pragma exported procs")
        )
    ;
        true
    ),

    % Save the newly inferred information.
    proc_info_set_goal(Goal, Proc0, Proc1),
    proc_info_set_inferred_determinism(NewDetism, Proc1, Proc),

    % Put back the new proc_info structure.
    map.det_update(Procs0, ProcId, Proc, Procs),
    pred_info_set_procedures(Procs, Pred0, Pred),
    map.det_update(Preds0, PredId, Pred, Preds),
    module_info_set_preds(Preds, !ModuleInfo).

:- pred get_exported_proc_context(list(pragma_exported_proc)::in,
    pred_id::in, proc_id::in, prog_context::out) is semidet.

get_exported_proc_context([Proc | Procs], PredId, ProcId, Context) :-
    ( Proc = pragma_exported_proc(PredId, ProcId, _, Context0) ->
        Context = Context0
    ;
        get_exported_proc_context(Procs, PredId, ProcId, Context)
    ).

%-----------------------------------------------------------------------------%

det_infer_goal(Goal0 - GoalInfo0, Goal - GoalInfo, InstMap0, !.SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo, Detism,
        GoalFailingContexts, !:Msgs) :-
    goal_info_get_nonlocals(GoalInfo0, NonLocalVars),
    goal_info_get_instmap_delta(GoalInfo0, InstmapDelta),

    % If a pure or semipure goal has no output variables, then the goal
    % is in a single-solution context.
    (
        det_no_output_vars(NonLocalVars, InstMap0, InstmapDelta, DetInfo),
        (
            goal_info_is_impure(GoalInfo0)
        =>
            goal_info_has_feature(GoalInfo0, not_impure_for_determinism)
        )
    ->
        AddPruning = yes,
        !:SolnContext = first_soln
    ;
        AddPruning = no
    ),
    (
        Goal0 = scope(ScopeReason, _),
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
    ->
        Prune = yes
    ;
        Prune = AddPruning
    ),

    det_infer_goal_2(Goal0, Goal1, GoalInfo0, InstMap0, !.SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
        InternalDetism0, GoalFailingContexts, !:Msgs),

    determinism_components(InternalDetism0, InternalCanFail, InternalSolns0),
    (
        % If mode analysis notices that a goal cannot succeed,
        % then determinism analysis should notice this too.

        instmap_delta_is_unreachable(InstmapDelta)
    ->
        InternalSolns = at_most_zero
    ;
        InternalSolns = InternalSolns0
    ),
    (
        ( InternalSolns = at_most_many
        ; InternalSolns = at_most_many_cc
        ),
        Prune = yes
    ->
        Solns = at_most_one
    ;
        % If a goal with multiple solutions occurs in a single-solution
        % context, then we will need to do pruning.

        InternalSolns = at_most_many,
        !.SolnContext = first_soln
    ->
        Solns = at_most_many_cc
    ;
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
    (
        % If-then-elses that are det or semidet may nevertheless contain nondet
        % or multidet conditions. If this happens, the if-then-else must be put
        % inside a `scope' to appease the code generator. (Both the MLDS and
        % LLDS back-ends rely on this.)

        Goal1 = if_then_else(_, _ - CondInfo, _, _),
        goal_info_get_determinism(CondInfo, CondDetism),
        determinism_components(CondDetism, _, at_most_many),
        Solns \= at_most_many
    ->
        FinalInternalSolns = at_most_many
    ;
        % Conjunctions that cannot produce solutions may nevertheless contain
        % nondet and multidet goals. If this happens, the conjunction is put
        % inside a scope goal to appease the code generator.

        Goal1 = conj(plain_conj, ConjGoals),
        Solns = at_most_zero,
        some [ConjGoalInfo] (
            list.member(_ - ConjGoalInfo, ConjGoals),
            goal_info_get_determinism(ConjGoalInfo, ConjGoalDetism),
            determinism_components(ConjGoalDetism, _, at_most_many)
        )
    ->
        FinalInternalSolns = at_most_many
    ;
        FinalInternalSolns = InternalSolns
    ),
    determinism_components(FinalInternalDetism, InternalCanFail,
        FinalInternalSolns),

    % See how we should introduce the commit operator, if one is needed.
    (
        % Do we need a commit?
        Detism \= FinalInternalDetism,

        % Disjunctions, we want to use a semidet or cc_nondet disjunction
        % which avoids creating a choice point at all, rather than wrapping
        % a some [] around a nondet disj, which would create a choice point
        % and then prune it.
        Goal1 \= disj(_),

        % Do we already have a commit?
        Goal1 \= scope(_, _)
    ->
        % A commit is needed - we must introduce an explicit `commit' so that
        % the code generator knows to insert the appropriate code for pruning.
        goal_info_set_determinism(FinalInternalDetism, GoalInfo0, InnerInfo),
        Goal = scope(commit(dont_force_pruning), Goal1 - InnerInfo)
    ;
        % Either no commit is needed, or a `scope' is already present.
        Goal = Goal1
    ).

:- func promise_eqv_solutions_kind_prunes(promise_solutions_kind) = bool.

promise_eqv_solutions_kind_prunes(equivalent_solutions) = yes.
promise_eqv_solutions_kind_prunes(equivalent_solution_sets) = no.
promise_eqv_solutions_kind_prunes(equivalent_solution_sets_arbitrary) = yes.

%-----------------------------------------------------------------------------%

:- pred det_infer_goal_2(hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, instmap::in, soln_context::in,
    list(failing_context)::in, maybe(pess_info)::in, det_info::in,
    determinism::out, list(failing_context)::out, list(context_det_msg)::out)
    is det.

det_infer_goal_2(GoalExpr0, GoalExpr, GoalInfo, InstMap0, SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo, Detism,
        GoalFailingContexts, !:Msgs) :-
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            % The determinism of a conjunction is the worst case of the
            % determinism of the goals of that conjuction.
            det_infer_conj(Goals0, Goals, InstMap0, SolnContext,
                RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
                Detism, [], GoalFailingContexts, !:Msgs)
        ;
            ConjType = parallel_conj,
            det_infer_par_conj(Goals0, Goals, GoalInfo, InstMap0, SolnContext,
                RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
                Detism, GoalFailingContexts, !:Msgs)
        ),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        det_infer_disj(Goals0, Goals, GoalInfo, InstMap0, SolnContext,
            RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
            Detism, GoalFailingContexts, !:Msgs),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, SwitchCanFail, Cases0),
        det_infer_switch(Var, SwitchCanFail, Cases0, Cases, GoalInfo, InstMap0,
            SolnContext, RightFailingContexts, MaybePromiseEqvSolutionSets,
            DetInfo, Detism, GoalFailingContexts, !:Msgs),
        GoalExpr = switch(Var, SwitchCanFail, Cases)
    ;
        GoalExpr0 = call(PredId, ProcId0, Args, Builtin, UnifyContext, Name),
        det_infer_call(PredId, ProcId0, ProcId, GoalInfo, SolnContext,
            RightFailingContexts, DetInfo,
            Detism, GoalFailingContexts, !:Msgs),
        GoalExpr = call(PredId, ProcId, Args, Builtin, UnifyContext, Name)
    ;
        GoalExpr0 = generic_call(GenericCall, _ArgVars, _Modes, CallDetism),
        det_infer_generic_call(GenericCall, CallDetism, GoalInfo, SolnContext,
            RightFailingContexts, DetInfo,
            Detism, GoalFailingContexts, !:Msgs),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = unify(LHS, RHS0, Mode, Unify, UnifyContext),
        det_infer_unify(LHS, RHS0, Unify, UnifyContext, RHS, GoalInfo,
            InstMap0, SolnContext, RightFailingContexts, DetInfo, Detism,
            GoalFailingContexts, !:Msgs),
        GoalExpr = unify(LHS, RHS, Mode, Unify, UnifyContext)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        det_infer_if_then_else(Cond0, Cond, Then0, Then, Else0, Else,
            InstMap0, SolnContext, RightFailingContexts,
            MaybePromiseEqvSolutionSets, DetInfo, Detism,
            GoalFailingContexts, !:Msgs),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = not(Goal0),
        det_infer_not(Goal0, Goal, GoalInfo, InstMap0,
            MaybePromiseEqvSolutionSets, DetInfo, Detism,
            GoalFailingContexts, !:Msgs),
        GoalExpr = not(Goal)
    ;
        GoalExpr0 = scope(Reason, Goal0),
        det_infer_scope(Reason, Goal0, Goal, GoalInfo, InstMap0, SolnContext,
            RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
            Detism, GoalFailingContexts, !:Msgs),
        GoalExpr = scope(Reason, Goal)
    ;
        GoalExpr0 = foreign_proc(Attributes, PredId, ProcId, _Args, _ExtraArgs,
            PragmaCode),
        det_infer_foreign_proc(Attributes, PredId, ProcId, PragmaCode,
            GoalInfo, SolnContext, RightFailingContexts, DetInfo, Detism,
            GoalFailingContexts, !:Msgs),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected(this_file, "det_infer_goal_2: unexpected shorthand")
    ).

%-----------------------------------------------------------------------------%

:- pred det_infer_conj(list(hlds_goal)::in, list(hlds_goal)::out, instmap::in,
    soln_context::in, list(failing_context)::in, maybe(pess_info)::in,
    det_info::in, determinism::out, list(failing_context)::in,
    list(failing_context)::out, list(context_det_msg)::out) is det.

det_infer_conj([], [], _InstMap0, _SolnContext, _RightFailingContexts,
        _MaybePromiseEqvSolutionSets, _DetInfo, det, !ConjFailingContexts, []).
det_infer_conj([Goal0 | Goals0], [Goal | Goals], InstMap0, SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo, Detism,
        !ConjFailingContexts, Msgs) :-
    % We should look to see when we get to a not_reached point
    % and optimize away the remaining elements of the conjunction.
    % But that optimization is done in the code generator anyway.

    % We infer the determinisms right-to-left, so that we can propagate
    % the SolnContext properly.

    % First, process the second and subsequent conjuncts.
    update_instmap(Goal0, InstMap0, InstMap1),
    det_infer_conj(Goals0, Goals, InstMap1, SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
        TailDetism, !ConjFailingContexts, TailMsgs),
    determinism_components(TailDetism, TailCanFail, _TailMaxSolns),

    % Next, work out whether the first conjunct is in a first_soln context
    % or not. We obviously need all its solutions if we need all the solutions
    % of the conjunction. However, even if we need only the first solution
    % of the conjunction, we may need to generate more than one solution
    % of the first conjunct if the later conjuncts may possibly fail.
    (
        TailCanFail = cannot_fail,
        SolnContext = first_soln
    ->
        HeadSolnContext = first_soln
    ;
        HeadSolnContext = all_solns
    ),
    % Process the first conjunct.
    det_infer_goal(Goal0, Goal, InstMap0, HeadSolnContext,
        !.ConjFailingContexts ++ RightFailingContexts,
        MaybePromiseEqvSolutionSets, DetInfo, HeadDetism,
        GoalFailingContexts, HeadMsgs),

    % Finally combine the results computed above.
    det_conjunction_detism(HeadDetism, TailDetism, Detism),
    !:ConjFailingContexts = GoalFailingContexts ++ !.ConjFailingContexts,
    Msgs = HeadMsgs ++ TailMsgs.

:- pred det_infer_par_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    hlds_goal_info::in, instmap::in, soln_context::in,
    list(failing_context)::in, maybe(pess_info)::in, det_info::in,
    determinism::out, list(failing_context)::out, list(context_det_msg)::out)
    is det.

det_infer_par_conj(Goals0, Goals, GoalInfo, InstMap0, SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
        Detism, GoalFailingContexts, !:Msgs) :-
    det_infer_par_conj_goals(Goals0, Goals, InstMap0, SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
        Detism, [], GoalFailingContexts, !:Msgs),
    (
        determinism_components(Detism, CanFail, Solns),
        CanFail = cannot_fail,
        Solns \= at_most_many
    ->
        true
    ;
        goal_info_get_context(GoalInfo, Context),
        det_info_get_pred_id(DetInfo, PredId),
        det_info_get_proc_id(DetInfo, ProcId),
        Msg = par_conj_not_det(Detism, PredId, ProcId, GoalInfo, Goals),
        ContextMsg = context_det_msg(Context, Msg),
        !:Msgs = [ContextMsg | !.Msgs]
    ).

:- pred det_infer_par_conj_goals(list(hlds_goal)::in, list(hlds_goal)::out,
    instmap::in, soln_context::in, list(failing_context)::in,
    maybe(pess_info)::in, det_info::in, determinism::out,
    list(failing_context)::in, list(failing_context)::out,
    list(context_det_msg)::out) is det.

det_infer_par_conj_goals([], [], _InstMap0, _SolnContext,
        _RightFailingContexts, _MaybePromiseEqvSolutionSets, _DetInfo,
        det, !ConjFailingContexts, []).
det_infer_par_conj_goals([Goal0 | Goals0], [Goal | Goals], InstMap0,
        SolnContext, RightFailingContexts, MaybePromiseEqvSolutionSets,
        DetInfo, Detism, !ConjFailingContexts, Msgs) :-
    det_infer_goal(Goal0, Goal, InstMap0, SolnContext, RightFailingContexts,
        MaybePromiseEqvSolutionSets, DetInfo, HeadDetism, GoalFailingContexts,
        HeadMsgs),
    determinism_components(HeadDetism, HeadCanFail, HeadMaxSolns),

    det_infer_par_conj_goals(Goals0, Goals, InstMap0, SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
        TailDetism, !ConjFailingContexts, TailMsgs),
    determinism_components(TailDetism, TailCanFail, TailMaxSolns),

    det_conjunction_maxsoln(HeadMaxSolns, TailMaxSolns, MaxSolns),
    det_conjunction_canfail(HeadCanFail, TailCanFail, CanFail),
    determinism_components(Detism, CanFail, MaxSolns),
    !:ConjFailingContexts = GoalFailingContexts ++ !.ConjFailingContexts,
    Msgs = HeadMsgs ++ TailMsgs.

:- pred det_infer_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    hlds_goal_info::in, instmap::in, soln_context::in,
    list(failing_context)::in, maybe(pess_info)::in, det_info::in,
    determinism::out, list(failing_context)::out, list(context_det_msg)::out)
    is det.

det_infer_disj(Goals0, Goals, GoalInfo, InstMap0, SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
        Detism, GoalFailingContexts, !:Msgs) :-
    det_infer_disj_goals(Goals0, Goals, InstMap0, SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
        can_fail, at_most_zero, Detism, [], GoalFailingContexts0, !:Msgs),
    (
        Goals = [],
        goal_info_get_context(GoalInfo, Context),
        GoalFailingContexts = [Context - fail_goal | GoalFailingContexts0]
    ;
        Goals = [_ | _],
        GoalFailingContexts = GoalFailingContexts0
    ).

:- pred det_infer_disj_goals(list(hlds_goal)::in, list(hlds_goal)::out,
    instmap::in, soln_context::in, list(failing_context)::in,
    maybe(pess_info)::in, det_info::in, can_fail::in, soln_count::in,
    determinism::out, list(failing_context)::in, list(failing_context)::out,
    list(context_det_msg)::out) is det.

det_infer_disj_goals([], [], _InstMap0, _SolnContext, _RightFailingContexts,
        _MaybePromiseEqvSolutionSets, _DetInfo, CanFail, MaxSolns, Detism,
        !DisjFailingContexts, []) :-
    determinism_components(Detism, CanFail, MaxSolns).
det_infer_disj_goals([Goal0 | Goals0], [Goal | Goals], InstMap0, SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
        !.CanFail, !.MaxSolns, Detism, !DisjFailingContexts, Msgs) :-
    det_infer_goal(Goal0, Goal, InstMap0, SolnContext, RightFailingContexts,
        MaybePromiseEqvSolutionSets, DetInfo, FirstDetism, GoalFailingContexts,
        FirstMsgs),
    determinism_components(FirstDetism, FirstCanFail, FirstMaxSolns),
    Goal = _ - GoalInfo,
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
    (
        !.MaxSolns \= at_most_zero,
        FirstMaxSolns = at_most_zero,
        goal_info_has_feature(GoalInfo, preserve_backtrack_into)
    ->
        AdjFirstMaxSolns = at_most_one
    ;
        AdjFirstMaxSolns = FirstMaxSolns
    ),
    det_disjunction_canfail(!.CanFail, FirstCanFail, !:CanFail),
    det_disjunction_maxsoln(!.MaxSolns, AdjFirstMaxSolns, !:MaxSolns),
    % In single-solution contexts, convert at_most_many to at_most_many_cc.
    (
        SolnContext = first_soln,
        !.MaxSolns = at_most_many
    ->
        !:MaxSolns = at_most_many_cc
    ;
        true
    ),
    det_infer_disj_goals(Goals0, Goals, InstMap0, SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
        !.CanFail, !.MaxSolns, Detism, !DisjFailingContexts, LaterMsgs),
    !:DisjFailingContexts = GoalFailingContexts ++ !.DisjFailingContexts,
    Msgs = FirstMsgs ++ LaterMsgs.

%-----------------------------------------------------------------------------%

:- pred det_infer_switch(prog_var::in, can_fail::in,
    list(case)::in, list(case)::out,
    hlds_goal_info::in, instmap::in, soln_context::in,
    list(failing_context)::in, maybe(pess_info)::in, det_info::in,
    determinism::out, list(failing_context)::out, list(context_det_msg)::out)
    is det.

det_infer_switch(Var, SwitchCanFail, Cases0, Cases, GoalInfo, InstMap0,
        SolnContext, RightFailingContexts, MaybePromiseEqvSolutionSets,
        DetInfo, Detism, GoalFailingContexts, !:Msgs) :-
    % The determinism of a switch is the worst of the determinism of each
    % of the cases. Also, if only a subset of the constructors are handled,
    % then it is semideterministic or worse - this is determined
    % in switch_detection.m and handled via the SwitchCanFail field.

    det_infer_switch_cases(Cases0, Cases, InstMap0, SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
        cannot_fail, at_most_zero, CasesDetism, [], GoalFailingContexts0,
        !:Msgs),
    determinism_components(CasesDetism, CasesCanFail, CasesSolns),
    % The switch variable tests are in a first_soln context if and only
    % if the switch goal as a whole was in a first_soln context and the
    % cases cannot fail.
    (
        CasesCanFail = cannot_fail,
        SolnContext = first_soln
    ->
        SwitchSolnContext = first_soln
    ;
        SwitchSolnContext = all_solns
    ),
    ExaminesRep = yes,
    det_check_for_noncanonical_type(Var, ExaminesRep, SwitchCanFail,
        SwitchSolnContext, GoalFailingContexts0, RightFailingContexts,
        GoalInfo, switch, DetInfo, SwitchSolns, !Msgs),
    det_conjunction_canfail(SwitchCanFail, CasesCanFail, CanFail),
    det_conjunction_maxsoln(SwitchSolns, CasesSolns, NumSolns),
    determinism_components(Detism, CanFail, NumSolns),
    (
        SwitchCanFail = can_fail,
        goal_info_get_context(GoalInfo, SwitchContext),
        GoalFailingContexts = [SwitchContext - incomplete_switch(Var) |
            GoalFailingContexts0]
    ;
        SwitchCanFail = cannot_fail,
        GoalFailingContexts = GoalFailingContexts0
    ).

:- pred det_infer_switch_cases(list(case)::in, list(case)::out, instmap::in,
    soln_context::in, list(failing_context)::in, maybe(pess_info)::in,
    det_info::in, can_fail::in, soln_count::in, determinism::out,
    list(failing_context)::in, list(failing_context)::out,
    list(context_det_msg)::out) is det.

det_infer_switch_cases([], [], _InstMap0, _SolnContext, _RightFailingContexts,
        _MaybePromiseEqvSolutionSets, _DetInfo, CanFail, MaxSolns,
        Detism, !SwitchFailingContexts, []) :-
    determinism_components(Detism, CanFail, MaxSolns).
det_infer_switch_cases([Case0 | Cases0], [Case | Cases], InstMap0, SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
        !.CanFail, !.MaxSolns, Detism, !SwitchFailingContexts, Msgs) :-
    % Technically, we should update the instmap to reflect the knowledge that
    % the var is bound to this particular constructor, but we wouldn't use
    % that information here anyway, so we don't bother.
    Case0 = case(ConsId, Goal0),
    det_infer_goal(Goal0, Goal, InstMap0, SolnContext, RightFailingContexts,
        MaybePromiseEqvSolutionSets, DetInfo, FirstDetism, GoalFailingContexts,
        FirstMsgs),
    Case = case(ConsId, Goal),
    determinism_components(FirstDetism, FirstCanFail, FirstMaxSolns),
    det_switch_canfail(!.CanFail, FirstCanFail, !:CanFail),
    det_switch_maxsoln(!.MaxSolns, FirstMaxSolns, !:MaxSolns),
    det_infer_switch_cases(Cases0, Cases, InstMap0, SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo,
        !.CanFail, !.MaxSolns, Detism, !SwitchFailingContexts, LaterMsgs),
    !:SwitchFailingContexts = GoalFailingContexts ++ !.SwitchFailingContexts,
    Msgs = FirstMsgs ++ LaterMsgs.

%-----------------------------------------------------------------------------%

:- pred det_infer_call(pred_id::in, proc_id::in, proc_id::out,
    hlds_goal_info::in, soln_context::in,
    list(failing_context)::in, det_info::in, determinism::out,
    list(failing_context)::out, list(context_det_msg)::out) is det.

det_infer_call(PredId, ProcId0, ProcId, GoalInfo, SolnContext,
        RightFailingContexts, DetInfo, Detism, GoalFailingContexts, !:Msgs) :-
    % For calls, just look up the determinism entry associated with
    % the called predicate.
    % This is the point at which annotations start changing
    % when we iterate to fixpoint for global determinism inference.
    det_lookup_detism(DetInfo, PredId, ProcId0, Detism0),

    % Make sure we don't try to call a committed-choice pred
    % from a non-committed-choice context.
    determinism_components(Detism0, CanFail, NumSolns),
    (
        NumSolns = at_most_many_cc,
        SolnContext = all_solns
    ->
        (
            det_find_matching_non_cc_mode(DetInfo, PredId, ProcId0,
                ProcIdPrime)
        ->
            ProcId = ProcIdPrime,
            !:Msgs = [],
            determinism_components(Detism, CanFail, at_most_many)
        ;
            goal_info_get_context(GoalInfo, GoalContext),
            det_get_proc_info(DetInfo, ProcInfo),
            proc_info_get_varset(ProcInfo, VarSet),
            Msg = cc_pred_in_wrong_context(GoalInfo, Detism0,
                PredId, ProcId0, VarSet, RightFailingContexts),
            ContextMsg = context_det_msg(GoalContext, Msg),
            !:Msgs = [ContextMsg],
            ProcId = ProcId0,
            % Code elsewhere relies on the assumption that
            % SolnContext = all_solns => NumSolns \= at_most_many_cc,
            % so we need to enforce that here.
            determinism_components(Detism, CanFail, at_most_many)
        )
    ;
        !:Msgs = [],
        ProcId = ProcId0,
        Detism = Detism0
    ),
    (
        CanFail = can_fail,
        goal_info_get_context(GoalInfo, Context),
        GoalFailingContexts = [Context - call_goal(PredId, ProcId)]
    ;
        CanFail = cannot_fail,
        GoalFailingContexts = []
    ).

:- pred det_infer_generic_call(generic_call::in, determinism::in,
    hlds_goal_info::in, soln_context::in,
    list(failing_context)::in, det_info::in, determinism::out,
    list(failing_context)::out, list(context_det_msg)::out) is det.

det_infer_generic_call(GenericCall, CallDetism,
        GoalInfo, SolnContext, RightFailingContexts, DetInfo,
        Detism, GoalFailingContexts, !:Msgs) :-
    determinism_components(CallDetism, CanFail, NumSolns),
    goal_info_get_context(GoalInfo, Context),
    (
        NumSolns = at_most_many_cc,
        SolnContext = all_solns
    ->
        % This error can only occur for higher-order calls.
        % Class method calls are only introduced by polymorphism.
        det_get_proc_info(DetInfo, ProcInfo),
        proc_info_get_varset(ProcInfo, VarSet),
        Msg = higher_order_cc_pred_in_wrong_context(GoalInfo, CallDetism,
            VarSet, RightFailingContexts),
        ContextMsg = context_det_msg(Context, Msg),
        !:Msgs = [ContextMsg],
        % Code elsewhere relies on the assumption that
        % SolnContext = all_soln => NumSolns \= at_most_many_cc,
        % so we need to enforce that here.
        determinism_components(Detism, CanFail, at_most_many)
    ;
        !:Msgs = [],
        Detism = CallDetism
    ),
    (
        CanFail = can_fail,
        GoalFailingContexts = [Context - generic_call_goal(GenericCall)]
    ;
        CanFail = cannot_fail,
        GoalFailingContexts = []
    ).

:- pred det_infer_foreign_proc(pragma_foreign_proc_attributes::in,
    pred_id::in, proc_id::in, pragma_foreign_code_impl::in,
    hlds_goal_info::in, soln_context::in,
    list(failing_context)::in, det_info::in, determinism::out,
    list(failing_context)::out, list(context_det_msg)::out) is det.

det_infer_foreign_proc(Attributes, PredId, ProcId, PragmaCode,
        GoalInfo, SolnContext, RightFailingContexts, DetInfo,
        Detism, GoalFailingContexts, !:Msgs) :-
    % Foreign_procs are handled in the same way as predicate calls.

    det_info_get_module_info(DetInfo, ModuleInfo),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
    (
        MaybeDetism = yes(Detism0),
        determinism_components(Detism0, CanFail, NumSolns0),
        (
            may_throw_exception(Attributes) = will_not_throw_exception,
            Detism0 = erroneous
        ->
            proc_info_get_context(ProcInfo, ProcContext),
            WillNotThrowMsg = will_not_throw_with_erroneous(PredId, ProcId),
            WillNotThrowContextMsg =
                context_det_msg(ProcContext, WillNotThrowMsg),
            !:Msgs = [WillNotThrowContextMsg]
        ;
            !:Msgs = []
        ),
        ( PragmaCode = nondet(_, _, _, _, _, _, _, _, _) ->
            % Foreign_procs codes of this form can have more than one
            % solution.
            NumSolns1 = at_most_many
        ;
            NumSolns1 = NumSolns0
        ),
        (
            NumSolns1 = at_most_many_cc,
            SolnContext = all_solns
        ->
            goal_info_get_context(GoalInfo, GoalContext),
            proc_info_get_varset(ProcInfo, VarSet),
            WrongContextMsg = cc_pred_in_wrong_context(GoalInfo, Detism0,
                PredId, ProcId, VarSet, RightFailingContexts),
            WrongContextContextMsg = context_det_msg(GoalContext,
                WrongContextMsg),
            !:Msgs = [WrongContextContextMsg | !.Msgs],
            NumSolns = at_most_many
        ;
            NumSolns = NumSolns1
        ),
        determinism_components(Detism, CanFail, NumSolns),
        (
            CanFail = can_fail,
            goal_info_get_context(GoalInfo, Context),
            GoalFailingContexts = [Context - call_goal(PredId, ProcId)]
        ;
            CanFail = cannot_fail,
            GoalFailingContexts = []
        )
    ;
        MaybeDetism = no,
        proc_info_get_context(ProcInfo, Context),
        Msg = pragma_c_code_without_det_decl(PredId, ProcId),
        ContextMsg = context_det_msg(Context, Msg),
        !:Msgs = [ContextMsg],
        Detism = erroneous,
        GoalFailingContexts = []
    ).

%-----------------------------------------------------------------------------%

:- pred det_infer_unify(prog_var::in, unify_rhs::in,
    unification::in, unify_context::in, unify_rhs::out,
    hlds_goal_info::in, instmap::in, soln_context::in,
    list(failing_context)::in, det_info::in, determinism::out,
    list(failing_context)::out, list(context_det_msg)::out) is det.

det_infer_unify(LHS, RHS0, Unify, UnifyContext, RHS, GoalInfo, InstMap0,
        SolnContext, RightFailingContexts, DetInfo, Detism,
        GoalFailingContexts, !:Msgs) :-
    % Unifications are either deterministic or semideterministic.
    (
        RHS0 = lambda_goal(Purity, PredOrFunc, EvalMethod, NonLocalVars,
            Vars, Modes, LambdaDeclaredDet, Goal0)
    ->
        ( determinism_components(LambdaDeclaredDet, _, at_most_many_cc) ->
            LambdaSolnContext = first_soln
        ;
            LambdaSolnContext = all_solns
        ),
        det_info_get_module_info(DetInfo, ModuleInfo),
        instmap.pre_lambda_update(ModuleInfo, Vars, Modes,
            InstMap0, InstMap1),
        det_infer_goal(Goal0, Goal, InstMap1, LambdaSolnContext, [],
            no, DetInfo, LambdaInferredDet, _LambdaFailingContexts, GoalMsgs),
        det_check_lambda(LambdaDeclaredDet, LambdaInferredDet,
            Goal, GoalInfo, DetInfo, CheckLambdaMsgs),
        list.append(GoalMsgs, CheckLambdaMsgs, !:Msgs),
        RHS = lambda_goal(Purity, PredOrFunc, EvalMethod, NonLocalVars,
            Vars, Modes, LambdaDeclaredDet, Goal)
    ;
        RHS = RHS0,
        !:Msgs = []
    ),
    det_infer_unify_canfail(Unify, UnifyCanFail),
    det_infer_unify_examines_rep(Unify, ExaminesRepresentation),
    det_check_for_noncanonical_type(LHS, ExaminesRepresentation,
        UnifyCanFail, SolnContext, RightFailingContexts, [], GoalInfo,
        unify(UnifyContext), DetInfo, UnifyNumSolns, !Msgs),
    determinism_components(Detism, UnifyCanFail, UnifyNumSolns),
    (
        UnifyCanFail = can_fail,
        goal_info_get_context(GoalInfo, Context),
        (
            Unify = construct(_, _, _, _, _, _, _),
            unexpected(this_file, "can_fail construct")
        ;
            Unify = assign(_, _),
            unexpected(this_file, "can_fail assign")
        ;
            Unify = complicated_unify(_, _, _),
            ( RHS = var(RHSVar) ->
                GoalFailingContexts = [Context - test_goal(LHS, RHSVar)]
            ;
                unexpected(this_file, "complicated_unify but no var")
            )
        ;
            Unify = deconstruct(Var, ConsId, _, _, _, _),
            GoalFailingContexts = [Context - deconstruct_goal(Var, ConsId)]
        ;
            Unify = simple_test(Var1, Var2),
            GoalFailingContexts = [Context - test_goal(Var1, Var2)]
        )
    ;
        UnifyCanFail = cannot_fail,
        GoalFailingContexts = []
    ).

%-----------------------------------------------------------------------------%

:- pred det_infer_if_then_else(hlds_goal::in, hlds_goal::out,
    hlds_goal::in, hlds_goal::out, hlds_goal::in, hlds_goal::out,
    instmap::in, soln_context::in, list(failing_context)::in,
    maybe(pess_info)::in, det_info::in, determinism::out,
    list(failing_context)::out, list(context_det_msg)::out) is det.

det_infer_if_then_else(Cond0, Cond, Then0, Then, Else0, Else, InstMap0,
        SolnContext, RightFailingContexts, MaybePromiseEqvSolutionSets,
        DetInfo, Detism, GoalFailingContexts, !:Msgs) :-
    % We process the goal right-to-left, doing the `then' before the
    % condition of the if-then-else, so that we can propagate the
    % SolnContext correctly.

    % First process the `then' part
    update_instmap(Cond0, InstMap0, InstMap1),
    det_infer_goal(Then0, Then, InstMap1, SolnContext, RightFailingContexts,
        MaybePromiseEqvSolutionSets, DetInfo, ThenDetism, ThenFailingContexts,
        ThenMsgs),
    determinism_components(ThenDetism, ThenCanFail, ThenMaxSoln),

    % Next, work out the right soln_context to use for the condition.
    % The condition is in a first_soln context if and only if the goal as
    % a whole was in a first_soln context and the `then' part cannot fail.
    (
        ThenCanFail = cannot_fail,
        SolnContext = first_soln
    ->
        CondSolnContext = first_soln
    ;
        CondSolnContext = all_solns
    ),
    % Process the `condition' part
    det_infer_goal(Cond0, Cond, InstMap0, CondSolnContext,
        ThenFailingContexts ++ RightFailingContexts,
        MaybePromiseEqvSolutionSets, DetInfo,
        CondDetism, _CondFailingContexts, CondMsgs),
    determinism_components(CondDetism, CondCanFail, CondMaxSoln),

    % Process the `else' part
    det_infer_goal(Else0, Else, InstMap0, SolnContext, RightFailingContexts,
        MaybePromiseEqvSolutionSets, DetInfo, ElseDetism, ElseFailingContexts,
        ElseMsgs),
    determinism_components(ElseDetism, ElseCanFail, ElseMaxSoln),

    % Finally combine the results from the three parts.
    ( CondCanFail = cannot_fail ->
        % A -> B ; C is equivalent to A, B if A cannot fail
        det_conjunction_detism(CondDetism, ThenDetism, Detism)
    ; CondMaxSoln = at_most_zero ->
        % A -> B ; C is equivalent to ~A, C if A cannot succeed
        det_negation_det(CondDetism, MaybeNegDetism),
        (
            MaybeNegDetism = no,
            unexpected(this_file,
                "cannot find determinism of negated condition")
        ;
            MaybeNegDetism = yes(NegDetism)
        ),
        det_conjunction_detism(NegDetism, ElseDetism, Detism)
    ;
        det_conjunction_maxsoln(CondMaxSoln, ThenMaxSoln, CTMaxSoln),
        det_switch_maxsoln(CTMaxSoln, ElseMaxSoln, MaxSoln),
        det_switch_canfail(ThenCanFail, ElseCanFail, CanFail),
        determinism_components(Detism, CanFail, MaxSoln)
    ),
    % Failing contexts in the condition are ignored, since they can't lead
    % to failure of the if-then-else as a whole without one or more failing
    % contexts in the then part or the else part.
    GoalFailingContexts = ThenFailingContexts ++ ElseFailingContexts,
    !:Msgs = CondMsgs ++ ThenMsgs ++ ElseMsgs.

:- pred det_infer_not(hlds_goal::in, hlds_goal::out, hlds_goal_info::in,
    instmap::in, maybe(pess_info)::in, det_info::in, determinism::out,
    list(failing_context)::out, list(context_det_msg)::out) is det.

det_infer_not(Goal0, Goal, GoalInfo, InstMap0, MaybePromiseEqvSolutionSets,
        DetInfo, Detism, GoalFailingContexts, !:Msgs) :-
    % Negations are almost always semideterministic. It is an error for
    % a negation to further instantiate any non-local variable. Such errors
    % will be reported by the mode analysis.
    %
    % Question: should we warn about the negation of goals that either
    % cannot succeed or cannot fail?
    % Answer: yes, probably, but it's not a high priority.
    det_infer_goal(Goal0, Goal, InstMap0, first_soln, [],
        MaybePromiseEqvSolutionSets, DetInfo, NegDetism, _NegatedGoalCanFail,
        !:Msgs),
    det_negation_det(NegDetism, MaybeDetism),
    (
        MaybeDetism = no,
        unexpected(this_file,
            "inappropriate determinism inside a negation")
    ;
        MaybeDetism = yes(Detism)
    ),
    determinism_components(Detism, CanFail, _),
    (
        CanFail = can_fail,
        goal_info_get_context(GoalInfo, Context),
        GoalFailingContexts = [Context - negated_goal]
    ;
        CanFail = cannot_fail,
        GoalFailingContexts = []
    ).

%-----------------------------------------------------------------------------%

:- pred det_infer_scope(scope_reason::in, hlds_goal::in, hlds_goal::out,
    hlds_goal_info::in, instmap::in, soln_context::in,
    list(failing_context)::in, maybe(pess_info)::in, det_info::in,
    determinism::out, list(failing_context)::out, list(context_det_msg)::out)
    is det.

det_infer_scope(Reason, Goal0, Goal, GoalInfo, InstMap0, SolnContext,
        RightFailingContexts, MaybePromiseEqvSolutionSets0, DetInfo, Detism,
        GoalFailingContexts, !:Msgs) :-
    % Existential quantification may require a cut to throw away solutions,
    % but we cannot rely on explicit quantification to detect this.
    % Therefore cuts are handled in det_infer_goal.
    ( Reason = promise_solutions(Vars, Kind) ->
        det_get_proc_info(DetInfo, ProcInfo),
        proc_info_get_varset(ProcInfo, VarSet),

        goal_info_get_context(GoalInfo, Context),
        (
            Kind = equivalent_solutions,
            SolnContextToUse = first_soln,
            MaybePromiseEqvSolutionSets = MaybePromiseEqvSolutionSets0,
            PromiseMsgs = []
        ;
            Kind = equivalent_solution_sets,
            SolnContextToUse = SolnContext,
            (
                MaybePromiseEqvSolutionSets0 = no,
                MaybePromiseEqvSolutionSets = yes(pess_info(Vars, Context)),
                PromiseMsgs = []
            ;
                MaybePromiseEqvSolutionSets0 = yes(pess_info(OldVars,
                    OldContext)),
                PromiseMsg = nested_promise_eqv_solution_sets(OldContext),
                PromiseScopeMsg = context_det_msg(Context, PromiseMsg),
                PromiseMsgs = [PromiseScopeMsg],
                AllVars = set.union(list_to_set(OldVars), list_to_set(Vars)),
                MaybePromiseEqvSolutionSets =
                    yes(pess_info(to_sorted_list(AllVars), OldContext))
            )
        ;
            Kind = equivalent_solution_sets_arbitrary,
            (
                MaybePromiseEqvSolutionSets0 = no,
                PromiseMsg = arbitrary_without_promise,
                PromiseScopeMsg = context_det_msg(Context, PromiseMsg),
                PromiseMsgs = [PromiseScopeMsg]
            ;
                MaybePromiseEqvSolutionSets0 = yes(pess_info(OldVars,
                    OldContext)),
                IntersectVars = set.intersect(list_to_set(OldVars),
                    list_to_set(Vars)),
                ( set.empty(IntersectVars) ->
                    PromiseMsgs = []
                ;
                    PromiseMsg = arbitrary_promise_overlap(OldContext,
                        VarSet, IntersectVars),
                    PromiseScopeMsg = context_det_msg(Context, PromiseMsg),
                    PromiseMsgs = [PromiseScopeMsg]
                )
            ),
            MaybePromiseEqvSolutionSets = no,
            SolnContextToUse = first_soln
        ),
        goal_info_get_instmap_delta(GoalInfo, InstmapDelta),
        instmap_delta_changed_vars(InstmapDelta, ChangedVars),
        det_info_get_module_info(DetInfo, ModuleInfo),
        set.divide(var_is_ground_in_instmap(ModuleInfo, InstMap0),
            ChangedVars, _GroundAtStartVars, BoundVars),

        % Which vars were bound inside the scope but not listed
        % in the promise_equivalent_solution{s,_sets} or arbitrary scope?
        set.difference(BoundVars, set.list_to_set(Vars), BugVars),
        ( set.empty(BugVars) ->
            ScopeMsgs1 = []
        ;
            ScopeMsg1 = promise_solutions_missing_vars(Kind, VarSet, BugVars),
            ContextScopeMsg1 = context_det_msg(Context, ScopeMsg1),
            ScopeMsgs1 = [ContextScopeMsg1]
        ),
        % Which vars were listed in the promise_equivalent_solutions
        % but not bound inside the scope?
        set.difference(set.list_to_set(Vars), BoundVars, ExtraVars),
        ( set.empty(ExtraVars) ->
            ScopeMsgs2 = []
        ;
            ScopeMsg2 = promise_solutions_extra_vars(Kind, VarSet, ExtraVars),
            ContextScopeMsg2 = context_det_msg(Context, ScopeMsg2),
            ScopeMsgs2 = [ContextScopeMsg2]
        ),
        ScopeMsgs = ScopeMsgs1 ++ ScopeMsgs2
    ;
        SolnContextToUse = SolnContext,
        MaybePromiseEqvSolutionSets = MaybePromiseEqvSolutionSets0,
        PromiseMsgs = [],
        ScopeMsgs = []
    ),
    det_infer_goal(Goal0, Goal, InstMap0, SolnContextToUse,
        RightFailingContexts, MaybePromiseEqvSolutionSets, DetInfo, Detism,
        GoalFailingContexts, SubMsgs),
    !:Msgs = PromiseMsgs ++ SubMsgs ++ ScopeMsgs.

%-----------------------------------------------------------------------------%

    % det_find_matching_non_cc_mode(DetInfo, PredId, ProcId0, ProcId):
    %
    % Search for a mode of the given predicate that is identical to the mode
    % ProcId0, except that its determinism is non-cc whereas ProcId0's detism
    % is cc. Let ProcId be the first such mode.
    %
:- pred det_find_matching_non_cc_mode(det_info::in, pred_id::in, proc_id::in,
    proc_id::out) is semidet.

det_find_matching_non_cc_mode(DetInfo, PredId, !ProcId) :-
    det_info_get_module_info(DetInfo, ModuleInfo),
    module_info_preds(ModuleInfo, PredTable),
    map.lookup(PredTable, PredId, PredInfo),
    pred_info_get_procedures(PredInfo, ProcTable),
    map.to_assoc_list(ProcTable, ProcList),
    det_find_matching_non_cc_mode_2(ProcList, ModuleInfo, PredInfo, !ProcId).

:- pred det_find_matching_non_cc_mode_2(assoc_list(proc_id, proc_info)::in,
    module_info::in, pred_info::in, proc_id::in, proc_id::out) is semidet.

det_find_matching_non_cc_mode_2([TestProcId - ProcInfo | Rest],
        ModuleInfo, PredInfo, !ProcId) :-
    (
        TestProcId \= !.ProcId,
        proc_info_interface_determinism(ProcInfo, Detism),
        determinism_components(Detism, _CanFail, MaxSoln),
        MaxSoln = at_most_many,
        modes_are_identical_bar_cc(!.ProcId, TestProcId, PredInfo, ModuleInfo)
    ->
        !:ProcId = TestProcId
    ;
        det_find_matching_non_cc_mode_2(Rest, ModuleInfo, PredInfo, !ProcId)
    ).

%-----------------------------------------------------------------------------%

:- pred det_check_for_noncanonical_type(prog_var::in, bool::in, can_fail::in,
    soln_context::in, list(failing_context)::in, list(failing_context)::in,
    hlds_goal_info::in, cc_unify_context::in, det_info::in, soln_count::out,
    list(context_det_msg)::in, list(context_det_msg)::out) is det.

det_check_for_noncanonical_type(Var, ExaminesRepresentation, CanFail,
        SolnContext, FailingContextsA, FailingContextsB, GoalInfo, GoalContext,
        DetInfo, NumSolns, !Msgs) :-
    (
        % Check for unifications that attempt to examine the representation
        % of a type that does not have a single representation for each
        % abstract value.

        ExaminesRepresentation = yes,
        det_get_proc_info(DetInfo, ProcInfo),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        map.lookup(VarTypes, Var, Type),
        det_type_has_user_defined_equality_pred(DetInfo, Type)
    ->
        ( CanFail = can_fail ->
            goal_info_get_context(GoalInfo, Context),
            proc_info_get_varset(ProcInfo, VarSet),
            Msg = cc_unify_can_fail(GoalInfo, Var, Type, VarSet, GoalContext),
            ContextMsg = context_det_msg(Context, Msg),
            !:Msgs = [ContextMsg | !.Msgs]
        ; SolnContext = all_solns ->
            goal_info_get_context(GoalInfo, Context),
            proc_info_get_varset(ProcInfo, VarSet),
            Msg = cc_unify_in_wrong_context(GoalInfo, Var, Type, VarSet,
                GoalContext, FailingContextsA ++ FailingContextsB),
            ContextMsg = context_det_msg(Context, Msg),
            !:Msgs = [ContextMsg | !.Msgs]
        ;
            true
        ),
        (
            SolnContext = first_soln,
            NumSolns = at_most_many_cc
        ;
            SolnContext = all_solns,
            NumSolns = at_most_many
        )
    ;
        NumSolns = at_most_one
    ).

    % Return true iff the principal type constructor of the given type
    % has user-defined equality.
    %
:- pred det_type_has_user_defined_equality_pred(det_info::in,
    mer_type::in) is semidet.

det_type_has_user_defined_equality_pred(DetInfo, Type) :-
    det_info_get_module_info(DetInfo, ModuleInfo),
    type_has_user_defined_equality_pred(ModuleInfo, Type, _).

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

%-----------------------------------------------------------------------------%

det_get_soln_context(DeclaredDetism, SolnContext) :-
    ( determinism_components(DeclaredDetism, _, at_most_many_cc) ->
        SolnContext = first_soln
    ;
        SolnContext = all_solns
    ).

%-----------------------------------------------------------------------------%

    % Determinism_declarations takes a module_info as input and returns
    % two lists of procedure ids, the first being those with determinism
    % declarations, and the second being those without.
    %
:- pred determinism_declarations(module_info::in, pred_proc_list::out,
    pred_proc_list::out, pred_proc_list::out) is det.

determinism_declarations(ModuleInfo, DeclaredProcs,
        UndeclaredProcs, NoInferProcs) :-
    get_all_pred_procs(ModuleInfo, PredProcs),
    segregate_procs(ModuleInfo, PredProcs, DeclaredProcs,
        UndeclaredProcs, NoInferProcs).

    % Get_all_pred_procs takes a module_info and returns a list of all
    % the procedure ids for that module (except class methods, which
    % do not need to be checked since we generate the code ourselves).
    %
:- pred get_all_pred_procs(module_info::in, pred_proc_list::out) is det.

get_all_pred_procs(ModuleInfo, PredProcs) :-
    module_info_predids(ModuleInfo, PredIds),
    module_info_preds(ModuleInfo, Preds),
    get_all_pred_procs_2(Preds, PredIds, [], PredProcs).

:- pred get_all_pred_procs_2(pred_table::in, list(pred_id)::in,
    pred_proc_list::in, pred_proc_list::out) is det.

get_all_pred_procs_2(_Preds, [], !PredProcs).
get_all_pred_procs_2(Preds, [PredId | PredIds], !PredProcs) :-
    map.lookup(Preds, PredId, Pred),
    ProcIds = pred_info_procids(Pred),
    fold_pred_modes(PredId, ProcIds, !PredProcs),
    get_all_pred_procs_2(Preds, PredIds, !PredProcs).

:- pred fold_pred_modes(pred_id::in, list(proc_id)::in, pred_proc_list::in,
    pred_proc_list::out) is det.

fold_pred_modes(_PredId, [], !PredProcs).
fold_pred_modes(PredId, [ProcId | ProcIds], !PredProcs) :-
    !:PredProcs = [proc(PredId, ProcId) | !.PredProcs],
    fold_pred_modes(PredId, ProcIds, !PredProcs).

    % segregate_procs(ModuleInfo, PredProcs,
    %   DeclaredProcs, UndeclaredProcs, NoInferProcs):
    %
    % The predicate partitions the pred_proc_ids in PredProcs into three
    % categories:
    %
    % - DeclaredProcs holds the procedures that have declarations that need
    %   to be checked.
    %
    % - UndeclaredProcs holds the procedures that don't have declarations
    %   whose determinism needs to be inferred.
    %
    % - NoInferProcs holds the procedures whose determinism is already
    %   known, and which should not be processed further.
    %
:- pred segregate_procs(module_info::in, pred_proc_list::in,
    pred_proc_list::out, pred_proc_list::out, pred_proc_list::out) is det.

segregate_procs(ModuleInfo, PredProcs, DeclaredProcs, UndeclaredProcs,
        NoInferProcs) :-
    segregate_procs_2(ModuleInfo, PredProcs, [], DeclaredProcs,
        [], UndeclaredProcs, [], NoInferProcs).

:- pred segregate_procs_2(module_info::in, pred_proc_list::in,
    pred_proc_list::in, pred_proc_list::out,
    pred_proc_list::in, pred_proc_list::out,
    pred_proc_list::in, pred_proc_list::out) is det.

segregate_procs_2(_ModuleInfo, [], !DeclaredProcs,
        !UndeclaredProcs, !NoInferProcs).
segregate_procs_2(ModuleInfo, [PredProcId | PredProcIds],
        !DeclaredProcs, !UndeclaredProcs, !NoInferProcs) :-
    PredProcId = proc(PredId, ProcId),
    module_info_preds(ModuleInfo, Preds),
    map.lookup(Preds, PredId, Pred),
    (
        (
            pred_info_is_imported(Pred)
        ;
            pred_info_is_pseudo_imported(Pred),
            hlds_pred.in_in_unification_proc_id(ProcId)
        ;
            pred_info_get_markers(Pred, Markers),
            check_marker(Markers, class_method)
        )
    ->
        !:NoInferProcs = [PredProcId | !.NoInferProcs]
    ;
        pred_info_get_procedures(Pred, Procs),
        map.lookup(Procs, ProcId, Proc),
        proc_info_get_declared_determinism(Proc, MaybeDetism),
        (
            MaybeDetism = no,
            !:UndeclaredProcs = [PredProcId | !.UndeclaredProcs]
        ;
            MaybeDetism = yes(_),
            !:DeclaredProcs = [PredProcId | !.DeclaredProcs]
        )
    ),
    segregate_procs_2(ModuleInfo, PredProcIds, !DeclaredProcs,
        !UndeclaredProcs, !NoInferProcs).

    % We can't infer a tighter determinism for imported procedures or for
    % class methods, so set the inferred determinism to be the same as the
    % declared determinism. This can't be done easily during make_hlds since
    % inter-module optimization means that the import_status of procedures
    % isn't determined until after all items are processed.
    %
:- pred set_non_inferred_proc_determinism(pred_proc_id::in,
    module_info::in, module_info::out) is det.

set_non_inferred_proc_determinism(proc(PredId, ProcId), !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_procedures(PredInfo0, Procs0),
    map.lookup(Procs0, ProcId, ProcInfo0),
    proc_info_get_declared_determinism(ProcInfo0, MaybeDet),
    (
        MaybeDet = yes(Det),
        proc_info_set_inferred_determinism(Det, ProcInfo0, ProcInfo),
        map.det_update(Procs0, ProcId, ProcInfo, Procs),
        pred_info_set_procedures(Procs, PredInfo0, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ;
        MaybeDet = no
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "det_analysis.m".

%-----------------------------------------------------------------------------%
:- end_module det_analysis.
%-----------------------------------------------------------------------------%
