%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: modes.m.
% Main author: fjh.
%
% This module contains the top level of the code for mode checking and mode
% inference.  It uses code in the subsidiary modules mode_info, delay_info,
% inst_match, mode_errors, and mode_util.
%
% Basically what this module does is to traverse the HLDS, performing
% mode-checking or mode inference on each predicate.  For each procedure, it
% will reorder the procedure body if necessary, annotate each sub_goal with
% its mode, and check that the procedure body is mode-correct,
% This pass also determines whether or not unifications can fail.  It
% also converts unifications with higher-order predicate terms into
% unifications with lambda expressions.
%
% The input to this pass must be type-correct and in superhomogeneous form.
%
% This pass does not check that `unique' modes are not used in contexts
% which might require backtracking - that is done by unique_modes.m.
% N.B. Changes here may also require changes to unique_modes.m!

% IMPLEMENTATION DOCUMENTATION
% How does it all work?  Well, mode checking/inference is basically a
% process of abstract interpretation.  To perform this abstract
% interpretation on a procedure body, we need to know the initial insts of
% the arguments; then we can abstractly interpret the goal to compute the
% final insts.  For mode checking, we then just compare the inferred final
% insts with the declared final insts, and that's about all there is to it.
%
% For mode inference, it's a little bit trickier.  When we see a call to a
% predicate for which the modes weren't declared, we first check whether the
% call matches any of the modes we've already inferred.  If not, we create a
% new mode for the predicate, with the initial insts set to a "normalised"
% version of the insts of the call arguments.  For a first approximation, we
% set the final insts to `not_reached'.  What this means is that we don't
% yet have any information about what the final insts will be.  We then keep
% iterating mode inference passes until we reach a fixpoint.
%
% To mode-analyse a procedure:
%   1.  Initialize the insts of the head variables.
%   2.  Mode-analyse the goal.
%   3.  a.  If we're doing mode-checking:
%           Check that the final insts of the head variables
%           matches that specified in the mode declaration
%       b.  If we're doing mode-inference:
%           Normalise the final insts of the head variables,
%           record the newly inferred normalised final insts
%           in the proc_info, and check whether they changed
%           (so that we know when we've reached the fixpoint).
%
% To mode-analyse a goal:
% If goal is
%   (a) a disjunction
%       Mode-analyse the sub-goals;
%       check that the final insts of all the non-local
%       variables are the same for all the sub-goals.
%   (b) a conjunction
%       Attempt to schedule each sub-goal.  If a sub-goal can
%       be scheduled, then schedule it, otherwise delay it.
%       Continue with the remaining sub-goals until there are
%       no goals left.  Every time a variable gets bound,
%       see whether we should wake up a delayed goal,
%       and if so, wake it up next time we get back to
%       the conjunction.  If there are still delayed goals
%       hanging around at the end of the conjunction,
%       report a mode error.
%   (c) a negation
%       Mode-check the sub-goal.
%       Check that the sub-goal does not further instantiate
%       any non-local variables.  (Actually, rather than
%       doing this check after we mode-analyse the subgoal,
%       we instead "lock" the non-local variables, and
%       disallow binding of locked variables.)
%   (d) a unification
%       Check that the unification doesn't attempt to unify
%       two free variables (or in general two free sub-terms)
%       unless one of them is dead.  Split unifications
%       up if necessary to avoid complicated sub-unifications.
%       We also figure out at this point whether or not each
%       unification can fail.
%   (e) a predicate call
%       Check that there is a mode declaration for the
%       predicate which matches the current instantiation of
%       the arguments.  (Also handle calls to implied modes.)
%       If the called predicate is one for which we must infer
%       the modes, then create a new mode for the called predicate
%       whose initial insts are the result of normalising
%       the current inst of the arguments.
%   (f) an if-then-else
%       Attempt to schedule the condition.  If successful,
%       then check that it doesn't further instantiate any
%       non-local variables, mode-check the `then' part
%       and the `else' part, and then check that the final
%       insts match.  (Perhaps also think about expanding
%       if-then-elses so that they can be run backwards,
%       if the condition can't be scheduled?)
%
% To attempt to schedule a goal, first mode-check the goal.  If mode-checking
% succeeds, then scheduling succeeds.  If mode-checking would report
% an error due to the binding of a local variable, then scheduling
% fails.  (If mode-checking would report an error due to the binding of
% a *local* variable, we could report the error right away --
% but this idea has not yet been implemented.)
%
% Note that the notion of liveness used here is different to that
% used in liveness.m and the code generator.  Here, we consider
% a variable live if its value will be used later on in the computation.
%
% XXX we ought to allow unification of free with free even when both
%     *variables* are live, if one of the particular *sub-nodes* is
%     dead (causes problems handling e.g. `list__same_length').
%
% XXX we ought to break unifications into "micro-unifications", because
%     some code can't be scheduled without splitting up unifications.
%     For example, `p(X) :- X = f(A, B), B is A + 1.', where
%     p is declared as `:- mode p(bound(f(ground,free))->ground).'.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module check_hlds__modes.

:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__instmap.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module std_util.

    % modecheck(HLDS0, HLDS, UnsafeToContinue):
    %
    % Perform mode inference and checking for a whole module.
    % UnsafeToContinue = yes means that mode inference was halted
    % prematurely, due to an error, and that we should therefore
    % not perform determinism-checking, because we might get
    % internal errors.
    %
:- pred modecheck(module_info::in, module_info::out, bool::out,
    io::di, io::uo) is det.

    % Mode-check or unique-mode-check the code of all the predicates
    % in a module.
    %
:- pred check_pred_modes(how_to_check_goal::in, may_change_called_proc::in,
    module_info::in, module_info::out, bool::out, io::di, io::uo) is det.

    % Mode-check or unique-mode-check the code for single predicate.
    %
:- pred modecheck_pred_mode(pred_id::in, pred_info::in, how_to_check_goal::in,
    may_change_called_proc::in, module_info::in, module_info::out,
    int::out, io::di, io::uo) is det.

    % Mode-check the code for the given predicate in a given mode.
    % Returns the number of errs found and a bool `Changed'
    % which is true iff another pass of fixpoint analysis may be needed.
    %
:- pred modecheck_proc(proc_id::in, pred_id::in,
    module_info::in, module_info::out, int::out, bool::out,
    io::di, io::uo) is det.

    % Mode-check or unique-mode-check the code for the given predicate
    % in a given mode.
    % Returns the number of errs found and a bool `Changed'
    % which is true iff another pass of fixpoint analysis may be needed.
    %
:- pred modecheck_proc_general(proc_id::in, pred_id::in, how_to_check_goal::in,
    may_change_called_proc::in, module_info::in, module_info::out,
    int::out, bool::out, io::di, io::uo) is det.

    % Mode-check the code for the given predicate in the given mode.
    %
:- pred modecheck_proc_info(proc_id::in, pred_id::in,
    module_info::in, module_info::out, proc_info::in, proc_info::out,
    int::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

% The following predicates are used by unique_modes.m.

:- import_module check_hlds__mode_info.

    % Modecheck a unification.

    % Given a list of variables, and a list of livenesses,
    % select the live variables.
    %
:- pred get_live_vars(list(prog_var)::in, list(is_live)::in,
    list(prog_var)::out) is det.

    % Calculate the argument number offset that needs to be passed to
    % modecheck_var_list_is_live, modecheck_var_has_inst_list, and
    % modecheck_set_var_inst_list.  This offset number is calculated
    % so that real arguments get positive argument numbers and
    % type_info arguments get argument numbers less than or equal to 0.
    %
:- pred compute_arg_offset(pred_info::in, int::out) is det.

    % Given a list of variables and a list of expected liveness, ensure
    % that the inst of each variable satisfies the corresponding expected
    % liveness.  If the bool argument is `yes', then require an exact
    % match.
    %
:- pred modecheck_var_list_is_live(list(prog_var)::in, list(is_live)::in,
    bool::in, int::in, mode_info::in, mode_info::out) is det.

    % Given a list of variables and a list of initial insts, ensure
    % that the inst of each variable matches the corresponding initial
    % inst.  If the bool argument is `yes', then we require an exact
    % match (using inst_matches_final), otherwise we allow the var
    % to be more instantiated than the inst (using inst_matches_initial).
    %
:- pred modecheck_var_has_inst_list(list(prog_var)::in, list(mer_inst)::in,
    bool::in, int::in, inst_var_sub::out,
    mode_info::in, mode_info::out) is det.

    % modecheck_set_var_inst(Var, Inst, MaybeUInst, ModeInfo0, ModeInfo):
    %
    % Assign the given Inst to the given Var, after checking that it is
    % okay to do so.  If the inst to be assigned is the result of an
    % abstract unification then the MaybeUInst argument should be the
    % initial inst of the _other_ side of the unification. This allows
    % more precise (i.e. less conservative) checking in the case that
    % Inst contains `any' components and Var is locked (i.e. is a
    % nonlocal variable in a negated context). Where the inst is not
    % the result of an abstract unification then MaybeUInst should be `no'.
    %
:- pred modecheck_set_var_inst(prog_var::in, mer_inst::in, maybe(mer_inst)::in,
    mode_info::in, mode_info::out) is det.

:- pred modecheck_set_var_inst_list(list(prog_var)::in, list(mer_inst)::in,
    list(mer_inst)::in, int::in, list(prog_var)::out, extra_goals::out,
    mode_info::in, mode_info::out) is det.

    % Check that the final insts of the head vars of a lambda
    % goal matches their expected insts.
    %
:- pred modecheck_lambda_final_insts(list(prog_var)::in, list(mer_inst)::in,
    hlds_goal::in, hlds_goal::out, mode_info::in, mode_info::out) is det.

:- pred mode_info_add_goals_live_vars(list(hlds_goal)::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_remove_goals_live_vars(list(hlds_goal)::in,
    mode_info::in, mode_info::out) is det.

    % modecheck_functor_test(ConsId, Var):
    %
    % Update the instmap to reflect the fact that Var was bound to ConsId.
    % This is used for the functor tests in `switch' statements.
    %
:- pred modecheck_functor_test(prog_var::in, cons_id::in,
    mode_info::in, mode_info::out) is det.

    % compute_goal_instmap_delta(InstMap0, Goal,
    %   GoalInfo0, GoalInfo, ModeInfo0, ModeInfo):
    %
    % Work out the instmap_delta for a goal from
    % the instmaps before and after the goal.
    %
:- pred compute_goal_instmap_delta(instmap::in, hlds_goal_expr::in,
    hlds_goal_info::in, hlds_goal_info::out,
    mode_info::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%

% The following predicates are used by modecheck_unify.m.

:- pred modecheck_goal(hlds_goal::in, hlds_goal::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

    % Mode-check a single goal-expression.
    %
:- pred modecheck_goal_expr(hlds_goal_expr::in, hlds_goal_info::in,
    hlds_goal_expr::out, mode_info::in, mode_info::out,
    io::di, io::uo) is det.

:- type extra_goals
    --->    no_extra_goals
    ;       extra_goals(
                extra_before_main   :: list(hlds_goal),
                                    % goals to insert before the main goal
                extra_after_main    :: list(hlds_goal)
                                    % goals to append after the main goal
            ).

:- type after_goals
    --->    no_after_goals
    ;       after_goals(
                after_instmap       :: instmap,
                                    % instmap at end of main goal
                after_goals         :: list(hlds_goal)
                                    % goals to append after the main goal
            ).

    % Append_extra_goals inserts adds some goals to the
    % list of goals to insert before/after the main goal.
    %
:- pred append_extra_goals(extra_goals::in, extra_goals::in,
    extra_goals::out) is det.

    % Handle_extra_goals combines MainGoal and ExtraGoals into a single
    % hlds_goal_expr, rerunning mode analysis on the entire
    % conjunction if ExtraGoals is not empty.
    %
:- pred handle_extra_goals(hlds_goal_expr::in, extra_goals::in,
    hlds_goal_info::in, list(prog_var)::in, list(prog_var)::in,
    instmap::in, hlds_goal_expr::out, mode_info::in, mode_info::out,
    io::di, io::uo) is det.

:- pred mode_context_to_unify_context(mode_info::in, mode_context::in,
    unify_context::out) is det.

    % Construct a call to initialise a free solver type variable.
    %
:- pred construct_initialisation_call(prog_var::in, mer_type::in, mer_inst::in,
    prog_context::in, maybe(call_unify_context)::in,
    hlds_goal::out, mode_info::in, mode_info::out) is det.

    % Construct a list of initialisation calls.
    %
:- pred construct_initialisation_calls(list(prog_var)::in,
    list(hlds_goal)::out, mode_info::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__clause_to_proc.
:- import_module check_hlds__delay_info.
:- import_module check_hlds__inst_match.
:- import_module check_hlds__inst_util.
:- import_module check_hlds__modecheck_call.
:- import_module check_hlds__modecheck_unify.
:- import_module check_hlds__mode_debug.
:- import_module check_hlds__mode_errors.
:- import_module check_hlds__mode_info.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__polymorphism.
:- import_module check_hlds__purity.
:- import_module check_hlds__typecheck.
:- import_module check_hlds__type_util.
:- import_module check_hlds__unify_proc.
:- import_module check_hlds__unique_modes.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_out.
:- import_module hlds__instmap.
:- import_module hlds__make_hlds.
:- import_module hlds__passes_aux.
:- import_module hlds__quantification.
:- import_module hlds__special_pred.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__error_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__module_qual.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_type.

:- import_module assoc_list.
:- import_module bag.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

modecheck(!Module, UnsafeToContinue, !IO) :-
    globals__io_lookup_bool_option(statistics, Statistics, !IO),
    globals__io_lookup_bool_option(verbose, Verbose, !IO),

    maybe_write_string(Verbose, "% Mode-checking clauses...\n", !IO),
    check_pred_modes(check_modes, may_change_called_proc, !Module,
        UnsafeToContinue, !IO),
    maybe_report_stats(Statistics, !IO).

%-----------------------------------------------------------------------------%

    % Mode-check the code for all the predicates in a module.

check_pred_modes(WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, UnsafeToContinue, !IO) :-
    module_info_predids(!.ModuleInfo, PredIds),
    globals__io_lookup_int_option(mode_inference_iteration_limit,
        MaxIterations, !IO),
    modecheck_to_fixpoint(PredIds, MaxIterations, WhatToCheck,
        MayChangeCalledProc, !ModuleInfo, UnsafeToContinue, !IO),
    (
        WhatToCheck = check_unique_modes,
        write_mode_inference_messages(PredIds, yes, !.ModuleInfo, !IO),
        check_eval_methods(!ModuleInfo, !IO)
    ;
        WhatToCheck = check_modes,
        (
            UnsafeToContinue = yes,
            write_mode_inference_messages(PredIds, no, !.ModuleInfo, !IO)
        ;
            UnsafeToContinue = no
        )
    ).

    % Iterate over the list of pred_ids in a module.
    %
:- pred modecheck_to_fixpoint(list(pred_id)::in, int::in,
    how_to_check_goal::in, may_change_called_proc::in,
    module_info::in, module_info::out, bool::out, io::di, io::uo) is det.

modecheck_to_fixpoint(PredIds, MaxIterations, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, UnsafeToContinue, !IO) :-
    % Save the old procedure bodies so that we can restore them for the
    % next pass.
    module_info_preds(!.ModuleInfo, OldPredTable0),

    % Analyze everything which has the "can-process" flag set to `yes'.
    list__foldl4(maybe_modecheck_pred(WhatToCheck, MayChangeCalledProc),
        PredIds, !ModuleInfo, no, Changed1, 0, NumErrors, !IO),

    % Analyze the procedures whose "can-process" flag was no;
    % those procedures were inserted into the unify requests queue.
    modecheck_queued_procs(WhatToCheck, OldPredTable0, OldPredTable,
        !ModuleInfo, Changed2, !IO),
    io__get_exit_status(ExitStatus, !IO),

    bool__or(Changed1, Changed2, Changed),

    % Stop if we have reached a fixpoint or found any errors.
    ( ( Changed = no ; NumErrors > 0 ; ExitStatus \= 0 ) ->
        UnsafeToContinue = Changed
    ;
        % Stop if we have exceeded the iteration limit.
        ( MaxIterations =< 1 ->
            report_max_iterations_exceeded(!IO),
            UnsafeToContinue = yes
        ;
            globals__io_lookup_bool_option(debug_modes, DebugModes, !IO),
            (
                DebugModes = yes,
                write_mode_inference_messages(PredIds, no, !.ModuleInfo, !IO)
            ;
                DebugModes = no
            ),

            %
            % Mode analysis may have modified the procedure bodies,
            % since it does some optimizations such as deleting unreachable
            % code. But since we didn't reach a fixpoint yet, the mode
            % information is not yet correct, and so those optimizations
            % will have been done based on incomplete information, and so
            % they may produce incorrect results. We thus need to restore
            % the old procedure bodies.
            %

            (
                WhatToCheck = check_modes,
                % Restore the proc_info goals from the clauses in the
                % pred_info.  Reintroduce exists_cast goals, since these
                % do not appear in the clauses.
                copy_module_clauses_to_procs(PredIds, !ModuleInfo),
                introduce_exists_casts(PredIds, !ModuleInfo)
            ;
                WhatToCheck = check_unique_modes,
                % Restore the proc_info goals from the
                % proc_infos in the old module_info.
                copy_pred_bodies(OldPredTable, PredIds, !ModuleInfo)
            ),

            MaxIterations1 = MaxIterations - 1,
            modecheck_to_fixpoint(PredIds, MaxIterations1, WhatToCheck,
                MayChangeCalledProc, !ModuleInfo, UnsafeToContinue, !IO)
        )
    ).

:- pred report_max_iterations_exceeded(io::di, io::uo) is det.

report_max_iterations_exceeded(!IO) :-
    io__set_exit_status(1, !IO),
    io__write_strings([
       "Mode analysis iteration limit exceeded.\n",
       "You may need to declare the modes explicitly, or use the\n",
       "`--mode-inference-iteration-limit' option to increase the limit.\n"
    ], !IO),
    globals__io_lookup_int_option(mode_inference_iteration_limit,
        MaxIterations, !IO),
    io__format("(The current limit is %d iterations.)\n",
        [i(MaxIterations)], !IO).

    % copy_pred_bodies(OldPredTable, ProcId, ModuleInfo0, ModuleInfo):
    %
    % Copy the procedure bodies for all procedures of the specified PredIds
    % from OldPredTable into ModuleInfo0, giving ModuleInfo.
    %
:- pred copy_pred_bodies(pred_table::in, list(pred_id)::in,
    module_info::in, module_info::out) is det.

copy_pred_bodies(OldPredTable, PredIds, !ModuleInfo) :-
    module_info_preds(!.ModuleInfo, PredTable0),
    list__foldl(copy_pred_body(OldPredTable), PredIds, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo).

    % copy_pred_body(OldPredTable, ProcId, PredTable0, PredTable):
    %
    % Copy the procedure bodies for all procedures of the specified PredId
    % from OldPredTable into PredTable0, giving PredTable.
    %
:- pred copy_pred_body(pred_table::in, pred_id::in,
    pred_table::in, pred_table::out) is det.

copy_pred_body(OldPredTable, PredId, PredTable0, PredTable) :-
    map__lookup(PredTable0, PredId, PredInfo0),
    (
        % don't copy type class methods, because their
        % proc_infos are generated already mode-correct,
        % and because copying from the clauses_info doesn't
        % work for them.
        pred_info_get_markers(PredInfo0, Markers),
        check_marker(Markers, class_method)
    ->
        PredTable = PredTable0
    ;
        pred_info_procedures(PredInfo0, ProcTable0),
        map__lookup(OldPredTable, PredId, OldPredInfo),
        pred_info_procedures(OldPredInfo, OldProcTable),
        map__keys(OldProcTable, OldProcIds),
        list__foldl(copy_proc_body(OldProcTable), OldProcIds,
            ProcTable0, ProcTable),
        pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
        map__set(PredTable0, PredId, PredInfo, PredTable)
    ).

    % copy_proc_body(OldProcTable, ProcId, ProcTable0, ProcTable):
    %
    % Copy the body of the specified ProcId from OldProcTable
    % into ProcTable0, giving ProcTable.
    %
:- pred copy_proc_body(proc_table::in, proc_id::in,
    proc_table::in, proc_table::out) is det.

copy_proc_body(OldProcTable, ProcId, ProcTable0, ProcTable) :-
    map__lookup(OldProcTable, ProcId, OldProcInfo),
    proc_info_goal(OldProcInfo, OldProcBody),
    map__lookup(ProcTable0, ProcId, ProcInfo0),
    proc_info_set_goal(OldProcBody, ProcInfo0, ProcInfo),
    map__set(ProcTable0, ProcId, ProcInfo, ProcTable).

:- func should_modecheck_pred(pred_info) = bool.

should_modecheck_pred(PredInfo) = ShouldModeCheck :-
    (
        (
            %
            % don't modecheck imported predicates
            %
            ( pred_info_is_imported(PredInfo)
            ; pred_info_is_pseudo_imported(PredInfo)
            )
        ;
            %
            % don't modecheck class methods, because they are generated
            % already mode-correct and with correct instmap deltas.
            %
            pred_info_get_markers(PredInfo, PredMarkers),
            check_marker(PredMarkers, class_method)
        )
    ->
        ShouldModeCheck = no
    ;
        ShouldModeCheck = yes
    ).

:- pred maybe_modecheck_pred(how_to_check_goal::in, may_change_called_proc::in,
    pred_id::in, module_info::in, module_info::out, bool::in, bool::out,
    int::in, int::out, io::di, io::uo) is det.

maybe_modecheck_pred(WhatToCheck, MayChangeCalledProc, PredId,
        !ModuleInfo, !Changed, !NumErrors, !IO) :-
    module_info_preds(!.ModuleInfo, Preds0),
    map__lookup(Preds0, PredId, PredInfo0),
    ShouldModeCheck = should_modecheck_pred(PredInfo0),
    (
        ShouldModeCheck = no
    ;
        ShouldModeCheck = yes,
        write_modes_progress_message(PredId, PredInfo0, !.ModuleInfo,
            WhatToCheck, !IO),
        modecheck_pred_mode_2(PredId, PredInfo0, WhatToCheck,
            MayChangeCalledProc, !ModuleInfo, !Changed, ErrsInThisPred, !IO),
        ( ErrsInThisPred = 0 ->
            true
        ;
            module_info_get_num_errors(!.ModuleInfo, ModNumErrors0),
            ModNumErrors1 = ModNumErrors0 + ErrsInThisPred,
            module_info_set_num_errors(ModNumErrors1, !ModuleInfo),
            module_info_remove_predid(PredId, !ModuleInfo)
        ),
        !:NumErrors = !.NumErrors + ErrsInThisPred,
        globals__io_lookup_bool_option(detailed_statistics, Statistics, !IO),
        maybe_report_stats(Statistics, !IO)
    ).

:- pred write_modes_progress_message(pred_id::in, pred_info::in,
    module_info::in, how_to_check_goal::in, io::di, io::uo) is det.

write_modes_progress_message(PredId, PredInfo, ModuleInfo, WhatToCheck, !IO) :-
    pred_info_get_markers(PredInfo, Markers),
    ( check_marker(Markers, infer_modes) ->
        (
            WhatToCheck = check_modes,
            write_pred_progress_message("% Mode-analysing ",
                PredId, ModuleInfo, !IO)
        ;
            WhatToCheck = check_unique_modes,
            write_pred_progress_message("% Unique-mode-analysing ",
                PredId, ModuleInfo, !IO)
        )
    ;
        (
            WhatToCheck = check_modes,
            write_pred_progress_message("% Mode-checking ",
                PredId, ModuleInfo, !IO)
        ;
            WhatToCheck = check_unique_modes,
            write_pred_progress_message("% Unique-mode-checking ",
                PredId, ModuleInfo, !IO)
        )
    ).

%-----------------------------------------------------------------------------%

    % Mode-check the code for single predicate.
    %
modecheck_pred_mode(PredId, PredInfo0, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, NumErrors, !IO) :-
    modecheck_pred_mode_2(PredId, PredInfo0, WhatToCheck,
        MayChangeCalledProc, !ModuleInfo, no, _, NumErrors, !IO).

:- pred modecheck_pred_mode_2(pred_id::in, pred_info::in,
    how_to_check_goal::in, may_change_called_proc::in,
    module_info::in, module_info::out, bool::in, bool::out, int::out,
    io::di, io::uo) is det.

modecheck_pred_mode_2(PredId, PredInfo0, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !Changed, NumErrors, !IO) :-
    (
        WhatToCheck = check_modes,
        pred_info_procedures(PredInfo0, ProcTable),
        (
            some [ProcInfo] (
                map__member(ProcTable, _ProcId, ProcInfo),
                proc_info_maybe_declared_argmodes(ProcInfo, yes(_))
            )
        ->
            % there was at least one declared mode for this
            % procedure
            true
        ;
            % there were no declared modes for this procedure
            maybe_report_error_no_modes(PredId, PredInfo0, !.ModuleInfo, !IO)
        )
    ;
        WhatToCheck = check_unique_modes
    ),
    % Note that we use pred_info_procids rather than pred_info_all_procids
    % here, which means that we don't process modes that have already been
    % inferred as invalid.
    ProcIds = pred_info_procids(PredInfo0),
    modecheck_procs(ProcIds, PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !Changed, 0, NumErrors, !IO).

    % Iterate over the list of modes for a predicate.
    %
:- pred modecheck_procs(list(proc_id)::in, pred_id::in, how_to_check_goal::in,
    may_change_called_proc::in, module_info::in, module_info::out,
    bool::in, bool::out, int::in, int::out, io::di, io::uo) is det.

modecheck_procs([], _PredId, _, _, !ModuleInfo, !Changed, !Errs, !IO).
modecheck_procs([ProcId | ProcIds], PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !Changed, !Errs, !IO) :-
    % Mode-check that mode of the predicate.
    maybe_modecheck_proc(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !Changed, NumErrors, !IO),
    !:Errs = !.Errs + NumErrors,
    % Recursively process the remaining modes.
    modecheck_procs(ProcIds, PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !Changed, !Errs, !IO).

%-----------------------------------------------------------------------------%

    % Mode-check the code for predicate in a given mode.
    %
modecheck_proc(ProcId, PredId, !ModuleInfo, NumErrors, Changed, !IO) :-
    modecheck_proc_general(ProcId, PredId, check_modes, may_change_called_proc,
        !ModuleInfo, NumErrors, Changed, !IO).

modecheck_proc_general(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, NumErrors, Changed, !IO) :-
    maybe_modecheck_proc(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, no, Changed, NumErrors, !IO).

:- pred maybe_modecheck_proc(proc_id::in, pred_id::in, how_to_check_goal::in,
    may_change_called_proc::in, module_info::in, module_info::out,
    bool::in, bool::out, int::out, io::di, io::uo) is det.

maybe_modecheck_proc(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !Changed, NumErrors, !IO) :-
        % get the proc_info from the module_info
    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        _PredInfo0, ProcInfo0),
    ( proc_info_can_process(ProcInfo0, no) ->
        NumErrors = 0
    ;
            % modecheck it
        do_modecheck_proc(ProcId, PredId, WhatToCheck,
            MayChangeCalledProc, !ModuleInfo, ProcInfo0, ProcInfo,
            !Changed, NumErrors, !IO),

            % save the proc_info back in the module_info
        module_info_preds(!.ModuleInfo, Preds1),
        map__lookup(Preds1, PredId, PredInfo1),
        pred_info_procedures(PredInfo1, Procs1),
        map__set(Procs1, ProcId, ProcInfo, Procs),
        pred_info_set_procedures(Procs, PredInfo1, PredInfo),
        map__set(Preds1, PredId, PredInfo, Preds),
        module_info_set_preds(Preds, !ModuleInfo)
    ).

modecheck_proc_info(ProcId, PredId, !ModuleInfo, !ProcInfo, NumErrors, !IO) :-
    WhatToCheck = check_modes,
    do_modecheck_proc(ProcId, PredId, WhatToCheck, may_change_called_proc,
        !ModuleInfo, !ProcInfo, no, _Changed, NumErrors, !IO).

:- pred do_modecheck_proc(proc_id::in, pred_id::in, how_to_check_goal::in,
    may_change_called_proc::in, module_info::in, module_info::out,
    proc_info::in, proc_info::out, bool::in, bool::out, int::out,
    io::di, io::uo) is det.

do_modecheck_proc(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !ProcInfo, !Changed, NumErrors, !IO) :-
        % extract the useful fields in the proc_info
    proc_info_headvars(!.ProcInfo, HeadVars),
    proc_info_argmodes(!.ProcInfo, ArgModes0),
    proc_info_arglives(!.ProcInfo, !.ModuleInfo, ArgLives0),
    proc_info_goal(!.ProcInfo, Body0),

    % We use the context of the first clause, unless there weren't any clauses
    % at all, in which case we use the context of the mode declaration.
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    pred_info_clauses_info(PredInfo, ClausesInfo),
    clauses_info_clauses_only(ClausesInfo, ClauseList),
    (
        ClauseList = [FirstClause | _],
        FirstClause = clause(_, _, _, Context)
    ;
        ClauseList = [],
        proc_info_context(!.ProcInfo, Context)
    ),

    % Modecheck the body. First set the initial instantiation of the head
    % arguments, then modecheck the body, and then check that the final
    % instantiation matches that in the mode declaration.

    some [!ModeInfo] (
            % Construct the initial instmap.
        mode_list_get_initial_insts(!.ModuleInfo, ArgModes0, ArgInitialInsts),
        assoc_list__from_corresponding_lists(HeadVars, ArgInitialInsts,
            InstAL),
        instmap__from_assoc_list(InstAL, InstMap0),

            % Construct the initial set of live vars:
            % initially, only the non-clobbered head variables are live.
        get_live_vars(HeadVars, ArgLives0, LiveVarsList),
        set__list_to_set(LiveVarsList, LiveVars),

            % Initialize the mode info.
        mode_info_init(!.ModuleInfo, PredId, ProcId, Context, LiveVars,
            InstMap0, WhatToCheck, MayChangeCalledProc, !:ModeInfo),
        mode_info_set_changed_flag(!.Changed, !ModeInfo),

        pred_info_get_markers(PredInfo, Markers),
        ( check_marker(Markers, infer_modes) ->
            InferModes = yes
        ;
            InferModes = no
        ),
        mode_list_get_final_insts(!.ModuleInfo, ArgModes0, ArgFinalInsts0),

        (
            InferModes = no,
            check_marker(Markers, mode_check_clauses),
            (
                Body0 = disj(Disjuncts0) - BodyGoalInfo0,
                Disjuncts0 = [_ | _],
                ClausesForm0 = clause_disj(Disjuncts0)
            ;
                Body0 = switch(SwitchVar0, CanFail0, Cases0) - BodyGoalInfo0,
                Cases0 = [_ | _],
                ClausesForm0 = clause_switch(SwitchVar0, CanFail0, Cases0)
            ),
            goal_info_get_nonlocals(BodyGoalInfo0, BodyNonLocals),
            mode_info_get_var_types(!.ModeInfo, VarTypes0),
            SolverNonLocals = list__filter(
                is_solver_var(VarTypes0, !.ModuleInfo),
                set__to_sorted_list(BodyNonLocals)),
            SolverNonLocals = []
        ->
            goal_info_get_context(BodyGoalInfo0, BodyContext),
            term__context_init(EmptyContext),
            ( BodyContext = EmptyContext ->
                true
            ;
                mode_info_set_context(BodyContext, !ModeInfo)
            ),

            % Modecheck each clause of the procedure body separately.
            (
                WhatToCheck = check_modes,
                (
                    ClausesForm0 = clause_disj(Disjuncts1),
                    Disjuncts2 = flatten_disjs(Disjuncts1),
                    list__map_foldl2(
                        modecheck_clause_disj(HeadVars, InstMap0,
                            ArgFinalInsts0),
                        Disjuncts2, Disjuncts, !ModeInfo, !IO),
                    NewGoalExpr = disj(Disjuncts)
                ;
                    ClausesForm0 = clause_switch(SwitchVar, CanFail, Cases1),
                    list__map_foldl2(
                        modecheck_clause_switch(HeadVars, InstMap0,
                            ArgFinalInsts0, SwitchVar),
                        Cases1, Cases, !ModeInfo, !IO),
                    NewGoalExpr = switch(SwitchVar, CanFail, Cases)
                )
            ;
                WhatToCheck = check_unique_modes,
                mode_info_get_nondet_live_vars(!.ModeInfo, NondetLiveVars0),
                goal_info_get_determinism(BodyGoalInfo0, Detism),
                goal_info_get_nonlocals(BodyGoalInfo0, NonLocals),
                ( determinism_components(Detism, _, at_most_many) ->
                    true
                ;
                    mode_info_set_nondet_live_vars(bag__init, !ModeInfo)
                ),
                (
                    ClausesForm0 = clause_disj(Disjuncts1),
                    Disjuncts2 = flatten_disjs(Disjuncts1),
                    ( determinism_components(Detism, _, at_most_many) ->
                        mode_info_add_live_vars(NonLocals, !ModeInfo),
                        make_all_nondet_live_vars_mostly_uniq(!ModeInfo),
                        mode_info_remove_live_vars(NonLocals, !ModeInfo)
                    ;
                        true
                    ),
                    list__map_foldl2(
                        unique_modecheck_clause_disj(HeadVars, InstMap0,
                            ArgFinalInsts0, Detism, NonLocals,
                            NondetLiveVars0),
                        Disjuncts2, Disjuncts, !ModeInfo, !IO),
                    NewGoalExpr = disj(Disjuncts)
                ;
                    ClausesForm0 = clause_switch(SwitchVar, CanFail, Cases1),
                    list__map_foldl2(
                        unique_modecheck_clause_switch(HeadVars, InstMap0,
                            ArgFinalInsts0, SwitchVar),
                        Cases1, Cases, !ModeInfo, !IO),
                    NewGoalExpr = switch(SwitchVar, CanFail, Cases)
                )
            ),

            % Manufacture an instmap_delta for the disjunction as a whole.
            assoc_list__from_corresponding_lists(HeadVars, ArgFinalInsts0,
                HeadVarFinalInsts),
            instmap__from_assoc_list(HeadVarFinalInsts, FinalInstMap),
            compute_instmap_delta(InstMap0, FinalInstMap, BodyNonLocals,
                DeltaInstMap),
            goal_info_set_instmap_delta(DeltaInstMap,
                BodyGoalInfo0, BodyGoalInfo),
            Body = NewGoalExpr - BodyGoalInfo,
            ArgFinalInsts = ArgFinalInsts0
        ;
            % Modecheck the procedure body as a single goal.
            (
                WhatToCheck = check_modes,
                modecheck_goal(Body0, Body1, !ModeInfo, !IO)
            ;
                WhatToCheck = check_unique_modes,
                unique_modes__check_goal(Body0, Body1, !ModeInfo, !IO)
            ),

            % Check that final insts match those specified in the
            % mode declaration.
            modecheck_final_insts(HeadVars, InferModes, ArgFinalInsts0,
                ArgFinalInsts, Body1, Body, !ModeInfo)
        ),

        (
            InferModes = yes,
            % For inferred predicates, we don't report the error(s) here;
            % instead we just save them in the proc_info, thus marking that
            % procedure as invalid. Uncommenting the next call is sometimes
            % handy for debugging:
            % report_mode_errors(!ModeInfo),
            mode_info_get_errors(!.ModeInfo, ModeErrors),
            !:ProcInfo = !.ProcInfo ^ mode_errors := ModeErrors,
            NumErrors = 0
        ;
            InferModes = no,
            % Report any errors we found.
            report_mode_errors(!ModeInfo, !IO),
            mode_info_get_num_errors(!.ModeInfo, NumErrors),
            report_mode_warnings(!ModeInfo, !IO)
        ),
        % Save away the results.
        inst_lists_to_mode_list(ArgInitialInsts, ArgFinalInsts, ArgModes),
        mode_info_get_changed_flag(!.ModeInfo, !:Changed),
        mode_info_get_module_info(!.ModeInfo, !:ModuleInfo),
        mode_info_get_varset(!.ModeInfo, VarSet),
        % VarTypes may be the same as VarTypes0, since mode checking can
        % add new variables (e.g. when handling calls in implied modes).
        mode_info_get_var_types(!.ModeInfo, VarTypes),
        mode_info_get_need_to_requantify(!.ModeInfo, NeedToRequantify),
        proc_info_set_goal(Body, !ProcInfo),
        proc_info_set_varset(VarSet, !ProcInfo),
        proc_info_set_vartypes(VarTypes, !ProcInfo),
        proc_info_set_argmodes(ArgModes, !ProcInfo),
        (
            NeedToRequantify = no
        ;
            NeedToRequantify = yes,
            requantify_proc(!ProcInfo)
        )
    ).

%-----------------------------------------------------------------------------%

:- type clause_form
    --->    clause_disj(list(hlds_goal))
    ;       clause_switch(prog_var, can_fail, list(case)).

:- pred modecheck_clause_disj(list(prog_var)::in, instmap::in,
    list(mer_inst)::in, hlds_goal::in, hlds_goal::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

modecheck_clause_disj(HeadVars, InstMap0, ArgFinalInsts0, Disjunct0, Disjunct,
        !ModeInfo, !IO) :-
    mode_info_set_instmap(InstMap0, !ModeInfo),
    modecheck_goal(Disjunct0, Disjunct1, !ModeInfo, !IO),

    % Check that final insts match those specified in the mode declaration.
    modecheck_final_insts(HeadVars, no, ArgFinalInsts0,
        _ArgFinalInsts, Disjunct1, Disjunct, !ModeInfo).

:- pred modecheck_clause_switch(list(prog_var)::in, instmap::in,
    list(mer_inst)::in, prog_var::in, case::in, case::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

modecheck_clause_switch(HeadVars, InstMap0, ArgFinalInsts0, Var, Case0, Case,
        !ModeInfo, !IO) :-
    Case0 = case(ConsId, Goal0),
    mode_info_set_instmap(InstMap0, !ModeInfo),

    modecheck_functor_test(Var, ConsId, !ModeInfo),

    % Modecheck this case (if it is reachable).
    mode_info_get_instmap(!.ModeInfo, InstMap1),
    ( instmap__is_reachable(InstMap1) ->
        modecheck_goal(Goal0, Goal1, !ModeInfo, !IO),
        mode_info_get_instmap(!.ModeInfo, InstMap)
    ;
        % We should not mode-analyse the goal, since it is unreachable.
        % Instead we optimize the goal away, so that later passes
        % won't complain about it not having mode information.
        true_goal(Goal1),
        InstMap = InstMap1
    ),

    % Don't lose the information added by the functor test above.
    fixup_switch_var(Var, InstMap0, InstMap, Goal1, Goal2),

    % Check that final insts match those specified in the mode declaration.
    modecheck_final_insts(HeadVars, no, ArgFinalInsts0,
        _ArgFinalInsts, Goal2, Goal, !ModeInfo),
    Case = case(ConsId, Goal).

:- pred unique_modecheck_clause_disj(list(prog_var)::in, instmap::in,
    list(mer_inst)::in, determinism::in, set(prog_var)::in, bag(prog_var)::in,
    hlds_goal::in, hlds_goal::out, mode_info::in, mode_info::out,
    io::di, io::uo) is det.

unique_modecheck_clause_disj(HeadVars, InstMap0, ArgFinalInsts0, DisjDetism,
        DisjNonLocals, NondetLiveVars0, Disjunct0, Disjunct, !ModeInfo, !IO) :-
    mode_info_set_instmap(InstMap0, !ModeInfo),
    mode_info_set_nondet_live_vars(NondetLiveVars0, !ModeInfo),
    unique_modes__prepare_for_disjunct(Disjunct0, DisjDetism, DisjNonLocals,
        !ModeInfo),
    unique_modes__check_goal(Disjunct0, Disjunct1, !ModeInfo, !IO),

    % Check that final insts match those specified in the mode declaration.
    modecheck_final_insts(HeadVars, no, ArgFinalInsts0,
        _ArgFinalInsts, Disjunct1, Disjunct, !ModeInfo).

:- pred unique_modecheck_clause_switch(list(prog_var)::in, instmap::in,
    list(mer_inst)::in, prog_var::in, case::in, case::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

unique_modecheck_clause_switch(HeadVars, InstMap0, ArgFinalInsts0, Var,
        Case0, Case, !ModeInfo, !IO) :-
    Case0 = case(ConsId, Goal0),
    mode_info_set_instmap(InstMap0, !ModeInfo),

    modecheck_functor_test(Var, ConsId, !ModeInfo),

    mode_info_get_instmap(!.ModeInfo, InstMap1),
    ( instmap__is_reachable(InstMap1) ->
        unique_modes__check_goal(Goal0, Goal1, !ModeInfo, !IO)
    ;
        % We should not mode-analyse the goal, since it is unreachable.
        % Instead we optimize the goal away, so that later passes
        % won't complain about it not having mode information.
        true_goal(Goal1)
    ),

    % Don't lose the information added by the functor test above.
    mode_info_get_instmap(!.ModeInfo, InstMap),
    fixup_switch_var(Var, InstMap0, InstMap, Goal1, Goal2),

    % Check that final insts match those specified in the mode declaration.
    modecheck_final_insts(HeadVars, no, ArgFinalInsts0, _ArgFinalInsts,
        Goal2, Goal, !ModeInfo),
    Case = case(ConsId, Goal).

%-----------------------------------------------------------------------------%

    % Modecheck_final_insts for a lambda expression.
    %
modecheck_lambda_final_insts(HeadVars, ArgFinalInsts, !Goal, !ModeInfo) :-
        % for lambda expressions, modes must always be
        % declared, we never infer them.
    InferModes = no,
    modecheck_final_insts(HeadVars, InferModes, ArgFinalInsts,
        _NewFinalInsts, !Goal, !ModeInfo).

:- pred modecheck_final_insts(list(prog_var)::in, bool::in,
    list(mer_inst)::in, list(mer_inst)::out, hlds_goal::in, hlds_goal::out,
    mode_info::in, mode_info::out) is det.

    % Check that the final insts of the head vars match their expected insts.
    %
modecheck_final_insts(HeadVars, InferModes, FinalInsts0, FinalInsts,
        Body0, Body, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_errors(!.ModeInfo, Errors),
    % If there were any mode errors, use an unreachable instmap.
    % This ensures that we don't get unwanted flow-on errors.
    % This is not strictly necessary, since we only report the
    % first mode error anyway, and the resulting FinalInsts
    % will not be used; but it improves the readability of the
    % rejected modes.
    (
        Errors = [_ | _],
        % If there were any mode errors, something must have
        % changed, since if the procedure had mode errors
        % in a previous pass then it wouldn't have been
        % processed at all in this pass.
        Changed0 = yes,
        instmap__init_unreachable(InstMap)
    ;
        Errors = [],
        Changed0 = no,
        mode_info_get_instmap(!.ModeInfo, InstMap)
    ),
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    instmap__lookup_vars(HeadVars, InstMap, VarFinalInsts1),
    map__apply_to_list(HeadVars, VarTypes, ArgTypes),
    (
        InferModes = yes,
        normalise_insts(ModuleInfo, ArgTypes, VarFinalInsts1, VarFinalInsts2),
        %
        % make sure we set the final insts of any variables which
        % we assumed were dead to `clobbered'.
        %
        mode_info_get_preds(!.ModeInfo, Preds),
        mode_info_get_predid(!.ModeInfo, PredId),
        map__lookup(Preds, PredId, PredInfo),
        pred_info_procedures(PredInfo, Procs),
        mode_info_get_procid(!.ModeInfo, ProcId),
        map__lookup(Procs, ProcId, ProcInfo),
        proc_info_arglives(ProcInfo, ModuleInfo, ArgLives),
        maybe_clobber_insts(VarFinalInsts2, ArgLives, FinalInsts),
        check_final_insts(HeadVars, FinalInsts0, FinalInsts, InferModes, 1,
            ModuleInfo, Body0, Body, no, Changed1, !ModeInfo),
        mode_info_get_changed_flag(!.ModeInfo, Changed2),
        bool__or_list([Changed0, Changed1, Changed2], Changed),
        mode_info_set_changed_flag(Changed, !ModeInfo)
    ;
        InferModes = no,
        check_final_insts(HeadVars, FinalInsts0, VarFinalInsts1,
            InferModes, 1, ModuleInfo, Body0, Body, no, _Changed1, !ModeInfo),
        FinalInsts = FinalInsts0
    ).

:- pred maybe_clobber_insts(list(mer_inst)::in, list(is_live)::in,
    list(mer_inst)::out) is det.

maybe_clobber_insts([], [_ | _], _) :-
    error("maybe_clobber_insts: length mismatch").
maybe_clobber_insts([_ | _], [], _) :-
    error("maybe_clobber_insts: length mismatch").
maybe_clobber_insts([], [], []).
maybe_clobber_insts([Inst0 | Insts0], [IsLive | IsLives], [Inst | Insts]) :-
    ( IsLive = dead ->
        Inst = ground(clobbered, none)
    ;
        Inst = Inst0
    ),
    maybe_clobber_insts(Insts0, IsLives, Insts).

:- pred check_final_insts(list(prog_var)::in,
    list(mer_inst)::in, list(mer_inst)::in,
    bool::in, int::in, module_info::in, hlds_goal::in, hlds_goal::out,
    bool::in, bool::out, mode_info::in, mode_info::out) is det.

check_final_insts(Vars, Insts, VarInsts, InferModes, ArgNum, ModuleInfo,
        !Goal, !Changed, !ModeInfo) :-
    (
        Vars = [],
        Insts = [],
        VarInsts = []
    ->
        true
    ;
        Vars = [Var | VarsTail],
        Insts = [Inst | InstsTail],
        VarInsts = [VarInst | VarInstsTail]
    ->
        mode_info_get_var_types(!.ModeInfo, VarTypes),
        map__lookup(VarTypes, Var, Type),
        (
            inst_matches_final(VarInst, Inst, Type, ModuleInfo)
        ->
            true
        ;
            !:Changed = yes,
            (
                % If this is a solver type with inst `free'
                % that should have inst `any' then insert
                % a call to the appropriate initialisation
                % predicate.
                %
                inst_match__inst_is_free(ModuleInfo, VarInst),
                inst_match__inst_is_any(ModuleInfo, Inst),
                type_util__type_is_solver_type(ModuleInfo, Type)
            ->
                prepend_initialisation_call(Var, Type, VarInst, !Goal,
                    !ModeInfo)
            ;
                % If we're inferring the mode, then don't
                % report an error, just set changed to yes
                % to make sure that we will do another
                % fixpoint pass.
                InferModes = yes
            ->
                true
            ;
                % XXX this might need to be reconsidered now
                % we have unique modes
                ( inst_matches_initial(VarInst, Inst, Type, ModuleInfo) ->
                    Reason = too_instantiated
                ; inst_matches_initial(Inst, VarInst, Type, ModuleInfo) ->
                    Reason = not_instantiated_enough
                ;
                    % I don't think this can happen.
                    % But just in case...
                    Reason = wrongly_instantiated
                ),
                set__init(WaitingVars),
                mode_info_error(WaitingVars,
                    mode_error_final_inst(ArgNum, Var, VarInst, Inst, Reason),
                    !ModeInfo)
            )
        ),
        check_final_insts(VarsTail, InstsTail, VarInstsTail,
            InferModes, ArgNum + 1, ModuleInfo, !Goal, !Changed, !ModeInfo)
    ;
        error("check_final_insts: length mismatch")
    ).

%-----------------------------------------------------------------------------%

:- pred prepend_initialisation_call(prog_var::in, mer_type::in, mer_inst::in,
    hlds_goal::in, hlds_goal::out, mode_info::in, mode_info::out) is det.

prepend_initialisation_call(Var, VarType, InitialInst, Goal0, Goal,
        !ModeInfo) :-
    Goal0   = _GoalExpr0 - GoalInfo0,
    hlds_goal__goal_info_get_context(GoalInfo0, Context),
    construct_initialisation_call(Var, VarType, InitialInst, Context,
        no /* CallUnifyContext */, InitVarGoal, !ModeInfo),
    goal_to_conj_list(Goal0, ConjList0),
    conj_list_to_goal([InitVarGoal | ConjList0], GoalInfo0, Goal).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% Modecheck a goal by abstractly interpreting it, as explained
% at the top of this file.

% Note: any changes here may need to be duplicated in unique_modes.m.

% Input-output: InstMap - Stored in the ModeInfo, which is passed as an
%             argument pair
%       DelayInfo - Stored in the ModeInfo
%       Goal    - Passed as an argument pair
% Input only:   Symbol tables   (constant)
%           - Stored in the ModuleInfo which is in the ModeInfo
%       Context Info    (changing as we go along the clause)
%           - Stored in the ModeInfo
% Output only:  Error Message(s)
%           - Output directly to stdout.

modecheck_goal(Goal0 - GoalInfo0, Goal - GoalInfo, !ModeInfo, !IO) :-
        %
        % store the current context in the mode_info
        %
    goal_info_get_context(GoalInfo0, Context),
    term__context_init(EmptyContext),
    ( Context = EmptyContext ->
        true
    ;
        mode_info_set_context(Context, !ModeInfo)
    ),
        %
        % modecheck the goal, and then store the changes in
        % instantiation of the vars in the delta_instmap
        % in the goal's goal_info.
        %
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    modecheck_goal_expr(Goal0, GoalInfo0, Goal, !ModeInfo, !IO),
    compute_goal_instmap_delta(InstMap0, Goal, GoalInfo0, GoalInfo, !ModeInfo).

compute_goal_instmap_delta(InstMap0, Goal, !GoalInfo, !ModeInfo) :-
    ( Goal = conj([]) ->
        %
        % When modecheck_unify.m replaces a unification with a
        % dead variable with `true', make sure the instmap_delta
        % of the goal is empty. The code generator and
        % mode_util__recompute_instmap_delta can be confused
        % by references to the dead variable in the instmap_delta,
        % resulting in calls to error/1.
        %
        instmap_delta_init_reachable(DeltaInstMap),
        mode_info_set_instmap(InstMap0, !ModeInfo)
    ;
        goal_info_get_nonlocals(!.GoalInfo, NonLocals),
        mode_info_get_instmap(!.ModeInfo, InstMap),
        compute_instmap_delta(InstMap0, InstMap, NonLocals, DeltaInstMap)
    ),
    goal_info_set_instmap_delta(DeltaInstMap, !GoalInfo).

modecheck_goal_expr(conj(List0), GoalInfo0, Goal, !ModeInfo, !IO) :-
    mode_checkpoint(enter, "conj", !ModeInfo, !IO),
    (
        List0 = [],         % for efficiency, optimize common case
        Goal = conj([])
    ;
        List0 = [_ | _],
        modecheck_conj_list(List0, List, !ModeInfo, !IO),
        conj_list_to_goal(List, GoalInfo0, Goal - _GoalInfo)
    ),
    mode_checkpoint(exit, "conj", !ModeInfo, !IO).

    % To modecheck a parallel conjunction, we modecheck each
    % conjunct independently (just like for disjunctions).
    % To make sure that we don't try to bind a variable more than
    % once (by binding it in more than one conjunct), we maintain a
    % datastructure that keeps track of three things:
    %
    % - the set of variables that are nonlocal to the conjuncts
    %   (which may be a superset of the nonlocals of the par_conj
    %   as a whole);
    % - the set of nonlocal variables that have been bound in the
    %   current conjunct; and
    % - the set of variables that were bound in previous conjuncts.
    %
    % When binding a variable, we check that it wasn't in the set of
    % variables bound in other conjuncts, and we add it to the set of
    % variables bound in this conjunct.
    %
    % At the end of the conjunct, we add the set of variables bound in
    % this conjunct to the set of variables bound in previous conjuncts
    % and set the set of variables bound in the current conjunct to
    % empty.
    %
    % A stack of these structures is maintained to handle nested parallel
    % conjunctions properly.
    %
modecheck_goal_expr(par_conj(List0), GoalInfo0, par_conj(List), !ModeInfo,
        !IO) :-
    mode_checkpoint(enter, "par_conj", !ModeInfo, !IO),
    goal_info_get_nonlocals(GoalInfo0, NonLocals),
    modecheck_par_conj_list(List0, List, NonLocals, InstMapNonlocalList,
        !ModeInfo, !IO),
    instmap__unify(NonLocals, InstMapNonlocalList, !ModeInfo),
    mode_checkpoint(exit, "par_conj", !ModeInfo, !IO).

modecheck_goal_expr(disj(Disjs0), GoalInfo0, Goal, !ModeInfo, !IO) :-
    mode_checkpoint(enter, "disj", !ModeInfo, !IO),
    (
        Disjs0 = [],    % for efficiency, optimize common case
        Goal = disj(Disjs0),
        instmap__init_unreachable(InstMap),
        mode_info_set_instmap(InstMap, !ModeInfo)
    ;
        % If you modify this code, you may also need to modify
        % modecheck_clause_disj or the code that calls it.

        Disjs0 = [_ | _],
        goal_info_get_nonlocals(GoalInfo0, NonLocals),
        modecheck_disj_list(Disjs0, Disjs1, InstMapList0, !ModeInfo, !IO),
        mode_info_get_var_types(!.ModeInfo, VarTypes),
        handle_solver_vars_in_disjs(set__to_sorted_list(NonLocals),
            VarTypes, Disjs1, Disjs2, InstMapList0, InstMapList, !ModeInfo),
        Disjs = flatten_disjs(Disjs2),
        instmap__merge(NonLocals, InstMapList, disj, !ModeInfo),
        disj_list_to_goal(Disjs, GoalInfo0, Goal - _GoalInfo)
    ),
    mode_checkpoint(exit, "disj", !ModeInfo, !IO).

modecheck_goal_expr(if_then_else(Vars, Cond0, Then0, Else0), GoalInfo0, Goal,
        !ModeInfo, !IO) :-
    mode_checkpoint(enter, "if-then-else", !ModeInfo, !IO),
    goal_info_get_nonlocals(GoalInfo0, NonLocals),
    goal_get_nonlocals(Then0, ThenVars),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    %
    % We need to lock the non-local variables, to ensure
    % that the condition of the if-then-else does not bind them.
    %
    mode_info_lock_vars(if_then_else, NonLocals, !ModeInfo),
    mode_info_add_live_vars(ThenVars, !ModeInfo),
    modecheck_goal(Cond0, Cond, !ModeInfo, !IO),
    mode_info_get_instmap(!.ModeInfo, InstMapCond),
    mode_info_remove_live_vars(ThenVars, !ModeInfo),
    mode_info_unlock_vars(if_then_else, NonLocals, !ModeInfo),
    ( instmap__is_reachable(InstMapCond) ->
        modecheck_goal(Then0, Then1, !ModeInfo, !IO),
        mode_info_get_instmap(!.ModeInfo, InstMapThen1)
    ;
        % We should not mode-analyse the goal, since it is unreachable.
        % Instead we optimize the goal away, so that later passes
        % won't complain about it not having mode information.
        true_goal(Then1),
        InstMapThen1 = InstMapCond
    ),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    modecheck_goal(Else0, Else1, !ModeInfo, !IO),
    mode_info_get_instmap(!.ModeInfo, InstMapElse1),
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    handle_solver_vars_in_ite(set__to_sorted_list(NonLocals), VarTypes,
        Then1, Then, Else1, Else,
        InstMapThen1, InstMapThen, InstMapElse1, InstMapElse, !ModeInfo),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    instmap__merge(NonLocals, [InstMapThen, InstMapElse], if_then_else,
        !ModeInfo),
    Goal = if_then_else(Vars, Cond, Then, Else),
    mode_checkpoint(exit, "if-then-else", !ModeInfo, !IO).

modecheck_goal_expr(not(SubGoal0), GoalInfo0, not(SubGoal), !ModeInfo, !IO) :-
    mode_checkpoint(enter, "not", !ModeInfo, !IO),
    goal_info_get_nonlocals(GoalInfo0, NonLocals),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    %
    % when analyzing a negated goal, nothing is forward-live
    % (live on forward executution after that goal), because
    % if the goal succeeds then execution will immediately backtrack.
    % So we need to set the live variables set to empty here.
    % This allows those variables to be backtrackably
    % destructively updated.  (If you try to do non-backtrackable
    % destructive update on such a variable, it will be caught
    % later on by unique_modes.m.)
    %
    mode_info_get_live_vars(!.ModeInfo, LiveVars0),
    mode_info_set_live_vars(bag__init, !ModeInfo),
    %
    % We need to lock the non-local variables, to ensure
    % that the negation does not bind them.
    %
    mode_info_lock_vars(negation, NonLocals, !ModeInfo),
    modecheck_goal(SubGoal0, SubGoal, !ModeInfo, !IO),
    mode_info_set_live_vars(LiveVars0, !ModeInfo),
    mode_info_unlock_vars(negation, NonLocals, !ModeInfo),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    mode_checkpoint(exit, "not", !ModeInfo, !IO).

modecheck_goal_expr(scope(Reason, SubGoal0), _GoalInfo, GoalExpr,
        !ModeInfo, !IO) :-
    ( Reason = from_ground_term(TermVar) ->
        % The original goal does no quantification, so deleting
        % the `scope' is OK, and it is necessary for avoiding
        % bad performance in later compiler phases, such as
        % simplification. This deletion undoes the insertion
        % done in the base case of unravel_unification in make_hlds.m.
        (
            mode_info_get_instmap(!.ModeInfo, InstMap0),
            instmap__lookup_var(InstMap0, TermVar, InstOfVar),
            InstOfVar = free,
            SubGoal0 = conj([UnifyTermGoal | UnifyArgGoals])
                - SubGoalInfo,
            % If TermVar created by an impure unification, which is
            % possible for solver types, it is possible for
            % UnifyTermGoal to contain a unification other than
            % one involving TermVar.
            UnifyTermGoal = unify(TermVar, _, _, _, _) - _
        ->
            % UnifyTerm unifies TermVar with the arguments created
            % by UnifyArgs. Since TermVar is now free and the
            % argument variables haven't been encountered yet,
            % UnifyTerm cannot succeed until *after* the argument
            % variables become ground.
            %
            % Putting UnifyTerm after UnifyArgs here is much more
            % efficient than letting the usual more ordering
            % algorithm delay it repeatedly.

            list__reverse([UnifyTermGoal | UnifyArgGoals], RevConj),
            RevSubGoal0 = conj(RevConj) - SubGoalInfo,
            mode_checkpoint(enter, "ground scope", !ModeInfo, !IO),
            modecheck_goal(RevSubGoal0, SubGoal, !ModeInfo, !IO),
            mode_checkpoint(exit, "ground scope", !ModeInfo, !IO),

            SubGoal = GoalExpr - _
        ;
            mode_checkpoint(enter, "scope", !ModeInfo, !IO),
            modecheck_goal(SubGoal0, SubGoal, !ModeInfo, !IO),
            mode_checkpoint(exit, "scope", !ModeInfo, !IO),
            SubGoal = GoalExpr - _
        )
    ;
        mode_checkpoint(enter, "scope", !ModeInfo, !IO),
        modecheck_goal(SubGoal0, SubGoal, !ModeInfo, !IO),
        mode_checkpoint(exit, "scope", !ModeInfo, !IO),
        GoalExpr = scope(Reason, SubGoal)
    ).

modecheck_goal_expr(call(PredId, ProcId0, Args0, _, Context, PredName),
        GoalInfo0, Goal, !ModeInfo, !IO) :-
    mdbcomp__prim_data__sym_name_to_string(PredName, PredNameString),
    string__append("call ", PredNameString, CallString),
    mode_checkpoint(enter, CallString, !ModeInfo, !IO),

    mode_info_get_call_id(!.ModeInfo, PredId, CallId),
    mode_info_set_call_context(call(call(CallId)), !ModeInfo),

    mode_info_get_instmap(!.ModeInfo, InstMap0),
    DeterminismKnown = no,
    modecheck_call_pred(PredId, DeterminismKnown, ProcId0, ProcId,
        Args0, Args, ExtraGoals, !ModeInfo),

    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_predid(!.ModeInfo, CallerPredId),
    Builtin = builtin_state(ModuleInfo, CallerPredId, PredId, ProcId),
    Call = call(PredId, ProcId, Args, Builtin, Context, PredName),
    handle_extra_goals(Call, ExtraGoals, GoalInfo0, Args0, Args,
        InstMap0, Goal, !ModeInfo, !IO),

    mode_info_unset_call_context(!ModeInfo),
    mode_checkpoint(exit, CallString, !ModeInfo, !IO).

modecheck_goal_expr(generic_call(GenericCall, Args0, Modes0, _),
        GoalInfo0, Goal, !ModeInfo, !IO) :-
    mode_checkpoint(enter, "generic_call", !ModeInfo, !IO),
    mode_info_get_instmap(!.ModeInfo, InstMap0),

    hlds_goal__generic_call_id(GenericCall, CallId),
    mode_info_set_call_context(call(CallId), !ModeInfo),
    (
        GenericCall = higher_order(PredVar, _, PredOrFunc, _),
        modecheck_higher_order_call(PredOrFunc, PredVar,
            Args0, Args, Modes, Det, ExtraGoals, !ModeInfo),
        AllArgs0 = [PredVar | Args0],
        AllArgs = [PredVar | Args]
    ;
        % Class method calls are added by polymorphism.m.
        % XXX We should probably fill this in so that
        % rerunning mode analysis works on code with typeclasses.
        GenericCall = class_method(_, _, _, _),
        error("modecheck_goal_expr: class_method_call")
    ;
        GenericCall = cast(_CastType),
        (
            goal_info_has_feature(GoalInfo0, keep_constant_binding),
            mode_info_get_instmap(!.ModeInfo, InstMap),
            (
                Args0 = [Arg1Prime, _Arg2Prime],
                Modes0 = [Mode1Prime, Mode2Prime]
            ->
                Arg1 = Arg1Prime,
                Mode1 = Mode1Prime,
                Mode2 = Mode2Prime
            ;
                error("modecheck_goal_expr: bad cast")
            ),
            Mode1 = in_mode,
            Mode2 = out_mode,
            instmap__lookup_var(InstMap, Arg1, Inst1),
            Inst1 = bound(Unique, [functor(ConsId, [])]),
            mode_info_get_module_info(!.ModeInfo, ModuleInfo),
            module_info_get_type_table(ModuleInfo, TypeTable),
            mode_info_get_var_types(!.ModeInfo, VarTypes),
            map__lookup(VarTypes, Arg1, ArgType1),
            type_to_ctor_and_args(ArgType1, ArgTypeCtor1, _),
            map__lookup(TypeTable, ArgTypeCtor1, CtorDefn),
            get_type_defn_body(CtorDefn, Body),
            ConsTagValues = Body ^ du_type_cons_tag_values,
            map__lookup(ConsTagValues, ConsId, ConsTag),
            ConsTag = shared_local_tag(_, LocalTag)
        ->
            BoundInst = functor(int_const(LocalTag), []),
            NewMode2 = (free -> bound(Unique, [BoundInst])),
            Modes = [Mode1, NewMode2]
        ;
            Modes = Modes0
        ),
        modecheck_builtin_cast(Modes, Args0, Args, Det, ExtraGoals, !ModeInfo),
        AllArgs0 = Args0,
        AllArgs = Args
    ;
        GenericCall = aditi_builtin(AditiBuiltin, UpdatedCallId),
        modecheck_aditi_builtin(AditiBuiltin, UpdatedCallId, Modes0,
            Args0, Args, Det, ExtraGoals, !ModeInfo),
        Modes = Modes0,
        AllArgs0 = Args0,
        AllArgs = Args
    ),

    Goal1 = generic_call(GenericCall, Args, Modes, Det),
    handle_extra_goals(Goal1, ExtraGoals, GoalInfo0, AllArgs0, AllArgs,
        InstMap0, Goal, !ModeInfo, !IO),

    mode_info_unset_call_context(!ModeInfo),
    mode_checkpoint(exit, "generic_call", !ModeInfo, !IO).

modecheck_goal_expr(unify(LHS0, RHS0, _, UnifyInfo0, UnifyContext), GoalInfo0,
        Goal, !ModeInfo, !IO) :-
    mode_checkpoint(enter, "unify", !ModeInfo, !IO),
    mode_info_set_call_context(unify(UnifyContext), !ModeInfo),
    modecheck_unification(LHS0, RHS0, UnifyInfo0, UnifyContext, GoalInfo0,
        Goal, !ModeInfo, !IO),
    mode_info_unset_call_context(!ModeInfo),
    mode_checkpoint(exit, "unify", !ModeInfo, !IO).

modecheck_goal_expr(switch(Var, CanFail, Cases0), GoalInfo0,
        switch(Var, CanFail, Cases), !ModeInfo, !IO) :-
    mode_checkpoint(enter, "switch", !ModeInfo, !IO),
    (
        Cases0 = [],
        Cases = [],
        instmap__init_unreachable(InstMap),
        mode_info_set_instmap(InstMap, !ModeInfo)
    ;
        % If you modify this code, you may also need to modify
        % modecheck_clause_switch or the code that calls it.

        Cases0 = [_ | _],
        goal_info_get_nonlocals(GoalInfo0, NonLocals),
        modecheck_case_list(Cases0, Var, Cases, InstMapList,
            !ModeInfo, !IO),
        instmap__merge(NonLocals, InstMapList, disj, !ModeInfo)
    ),
    mode_checkpoint(exit, "switch", !ModeInfo, !IO).

    % To modecheck a foreign_proc, we just modecheck the proc for
    % which it is the goal.
    %
modecheck_goal_expr(ForeignProc, GoalInfo, Goal, !ModeInfo, !IO) :-
    ForeignProc = foreign_proc(Attributes, PredId, ProcId0, Args0, ExtraArgs,
        PragmaCode),
    mode_checkpoint(enter, "pragma_foreign_code", !ModeInfo, !IO),
    mode_info_get_call_id(!.ModeInfo, PredId, CallId),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    DeterminismKnown = no,
    mode_info_set_call_context(call(call(CallId)), !ModeInfo),
    ArgVars0 = list__map(foreign_arg_var, Args0),
    modecheck_call_pred(PredId, DeterminismKnown, ProcId0, ProcId,
        ArgVars0, ArgVars, ExtraGoals, !ModeInfo),

    % zs: The assignment to Pragma looks wrong: instead of Args0,
    % I think we should use Args after the following call:
    % replace_foreign_arg_vars(Args0, ArgVars, Args)
    % or is there some reason why Args0 and Args would be the same?
    Pragma = foreign_proc(Attributes, PredId, ProcId, Args0, ExtraArgs,
        PragmaCode),
    handle_extra_goals(Pragma, ExtraGoals, GoalInfo, ArgVars0, ArgVars,
        InstMap0, Goal, !ModeInfo, !IO),

    mode_info_unset_call_context(!ModeInfo),
    mode_checkpoint(exit, "pragma_foreign_code", !ModeInfo, !IO).

modecheck_goal_expr(shorthand(_), _, _, !ModeInfo, !IO) :-
    % these should have been expanded out by now
    error("modecheck_goal_expr: unexpected shorthand").

append_extra_goals(no_extra_goals, ExtraGoals, ExtraGoals).
append_extra_goals(extra_goals(BeforeGoals, AfterGoals),
        no_extra_goals, extra_goals(BeforeGoals, AfterGoals)).
append_extra_goals(extra_goals(BeforeGoals0, AfterGoals0),
            extra_goals(BeforeGoals1, AfterGoals1),
            extra_goals(BeforeGoals, AfterGoals)) :-
    list__append(BeforeGoals0, BeforeGoals1, BeforeGoals),
    list__append(AfterGoals0, AfterGoals1, AfterGoals).

handle_extra_goals(MainGoal, no_extra_goals, _GoalInfo0, _Args0, _Args,
        _InstMap0, MainGoal, !ModeInfo, !IO).
handle_extra_goals(MainGoal, extra_goals(BeforeGoals0, AfterGoals0),
        GoalInfo0, Args0, Args, InstMap0, Goal, !ModeInfo, !IO) :-
    mode_info_get_errors(!.ModeInfo, Errors),
    (
        % There's no point adding extra goals if the code is
        % unreachable anyway.
        instmap__is_reachable(InstMap0),

        % If we recorded errors processing the goal, it will
        % have to be reprocessed anyway, so don't add the extra
        % goals now.
        Errors = []
    ->
        %
        % We need to be careful to update the delta-instmaps
        % correctly, using the appropriate instmaps:
        %
        %       % InstMapAtStart is here
        %    BeforeGoals,
        %       % we don't know the instmap here,
        %       % but as it happens we don't need it
        %    main goal,
        %       % InstMapAfterMain is here
        %    AfterGoals
        %       % InstMapAtEnd (from the ModeInfo) is here
        %

        % recompute the new set of non-local variables for the main goal
        goal_info_get_nonlocals(GoalInfo0, NonLocals0),
        set__list_to_set(Args0, OldArgVars),
        set__list_to_set(Args, NewArgVars),
        set__difference(NewArgVars, OldArgVars, IntroducedVars),
        set__union(NonLocals0, IntroducedVars, OutsideVars),
        set__intersect(OutsideVars, NewArgVars, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),

        % combine the main goal and the extra goals into a conjunction
        Goal0 = MainGoal - GoalInfo,
        goal_info_get_context(GoalInfo0, Context),
        handle_extra_goals_contexts(BeforeGoals0, Context, BeforeGoals),
        handle_extra_goals_contexts(AfterGoals0, Context, AfterGoals),
        list__append(BeforeGoals, [Goal0 | AfterGoals], GoalList0),

        mode_info_get_may_change_called_proc(!.ModeInfo, MayChangeCalledProc0),

        % Make sure we don't go into an infinite loop if
        % there is a bug in the code to add extra goals.
        mode_info_set_checking_extra_goals(yes, !ModeInfo),

        % We've already worked out which procedure should be called,
        % we don't need to do it again.
        mode_info_set_may_change_called_proc(may_not_change_called_proc,
            !ModeInfo),

        mode_info_set_instmap(InstMap0, !ModeInfo),

        % Recheck the goals to compute the instmap_deltas.
        %
        % This can fail even if the original check on the goal
        % succeeded in the case of a unification procedure which
        % binds a partially instantiated variable, because adding
        % the extra goals can make the partially instantiated
        % variables `live' after the main goal.
        % The other thing to beware of in this case is that delaying
        % must be disabled while processing the extra goals. If it
        % is not, the main unification will be delayed until after the
        % argument unifications, which turns them into assignments,
        % and we end up repeating the process forever.
        mode_info_add_goals_live_vars(GoalList0, !ModeInfo),
        modecheck_conj_list_no_delay(GoalList0, GoalList, !ModeInfo, !IO),
        Goal = conj(GoalList),
        mode_info_set_checking_extra_goals(no, !ModeInfo),
        mode_info_set_may_change_called_proc(MayChangeCalledProc0, !ModeInfo)
    ;
        Goal = MainGoal
    ).

:- pred handle_extra_goals_contexts(list(hlds_goal)::in, prog_context::in,
    list(hlds_goal)::out) is det.

handle_extra_goals_contexts([], _Context, []).
handle_extra_goals_contexts([Goal0 | Goals0], Context, [Goal | Goals]) :-
    Goal0 = Expr - GoalInfo0,
    Goal  = Expr - GoalInfo,
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    handle_extra_goals_contexts(Goals0, Context, Goals).

%-----------------------------------------------------------------------------%

    % Ensure that any non-local solver var that is initialised in
    % one disjunct is initialised in all disjuncts.
    %
:- pred handle_solver_vars_in_disjs(list(prog_var)::in,
    map(prog_var, mer_type)::in, list(hlds_goal)::in, list(hlds_goal)::out,
    list(instmap)::in, list(instmap)::out, mode_info::in, mode_info::out)
    is det.

handle_solver_vars_in_disjs(NonLocals, VarTypes, Disjs0, Disjs,
        InstMaps0, InstMaps, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    EnsureInitialised = solver_vars_that_must_be_initialised(NonLocals,
        VarTypes, ModuleInfo, InstMaps0),
    add_necessary_disj_init_calls(Disjs0, Disjs, InstMaps0, InstMaps,
        EnsureInitialised, !ModeInfo).

:- pred handle_solver_vars_in_ite(list(prog_var)::in,
    map(prog_var, mer_type)::in,
    hlds_goal::in, hlds_goal::out, hlds_goal::in, hlds_goal::out,
    instmap::in, instmap::out, instmap::in, instmap::out, mode_info::in,
    mode_info::out) is det.

handle_solver_vars_in_ite(NonLocals, VarTypes, Then0, Then, Else0, Else,
        ThenInstMap0, ThenInstMap, ElseInstMap0, ElseInstMap, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    EnsureInitialised = solver_vars_that_must_be_initialised(NonLocals,
        VarTypes, ModuleInfo, [ThenInstMap0, ElseInstMap0]),

    ThenVarsToInit = solver_vars_to_init(EnsureInitialised, ModuleInfo,
        ThenInstMap0),
    construct_initialisation_calls(ThenVarsToInit, ThenInitCalls, !ModeInfo),
    InitedThenVars = list_to_set(ThenVarsToInit),
    Then = append_init_calls_to_goal(InitedThenVars, ThenInitCalls, Then0),
    ThenInstMap = set_vars_to_inst_any(ThenVarsToInit, ThenInstMap0),

    ElseVarsToInit = solver_vars_to_init(EnsureInitialised, ModuleInfo,
        ElseInstMap0),
    construct_initialisation_calls(ElseVarsToInit, ElseInitCalls, !ModeInfo),
    InitedElseVars = list_to_set(ElseVarsToInit),
    Else = append_init_calls_to_goal(InitedElseVars, ElseInitCalls, Else0),
    ElseInstMap = set_vars_to_inst_any(ElseVarsToInit, ElseInstMap0).

:- func solver_vars_that_must_be_initialised(list(prog_var),
    vartypes, module_info, list(instmap)) = list(prog_var).

solver_vars_that_must_be_initialised(Vars, VarTypes, ModuleInfo, InstMaps) =
    list__filter(
        solver_var_must_be_initialised(VarTypes, ModuleInfo, InstMaps),
        Vars).

:- pred solver_var_must_be_initialised(vartypes::in, module_info::in,
    list(instmap)::in, prog_var::in) is semidet.

solver_var_must_be_initialised(VarTypes, ModuleInfo, InstMaps, Var) :-
    map__lookup(VarTypes, Var, VarType),
    type_util__type_is_solver_type(ModuleInfo, VarType),
    list__member(InstMap, InstMaps),
    instmap__lookup_var(InstMap, Var, Inst),
    not inst_match__inst_is_free(ModuleInfo, Inst).

:- pred is_solver_var(vartypes::in, module_info::in, prog_var::in) is semidet.

is_solver_var(VarTypes, ModuleInfo, Var) :-
    map__lookup(VarTypes, Var, VarType),
    type_util__type_is_solver_type(ModuleInfo, VarType).

:- pred add_necessary_disj_init_calls(list(hlds_goal)::in,
    list(hlds_goal)::out, list(instmap)::in, list(instmap)::out,
    list(prog_var)::in, mode_info::in, mode_info::out) is det.

add_necessary_disj_init_calls([], [], [], [], _EnsureInitialised, !ModeInfo).
add_necessary_disj_init_calls([], _, [_ | _], _, _, _, _) :-
    error("modes.add_necessary_init_calls: mismatched lists").
add_necessary_disj_init_calls([_ | _], _, [], _, _, _, _) :-
    error("modes.add_necessary_init_calls: mismatched lists").
add_necessary_disj_init_calls([Goal0 | Goals0], [Goal | Goals],
        [InstMap0 | InstMaps0], [InstMap | InstMaps],
        EnsureInitialised, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    VarsToInit = solver_vars_to_init(EnsureInitialised, ModuleInfo, InstMap0),
    construct_initialisation_calls(VarsToInit, InitCalls, !ModeInfo),
    InitedVars = list_to_set(VarsToInit),
    Goal = append_init_calls_to_goal(InitedVars, InitCalls, Goal0),
    InstMap = set_vars_to_inst_any(VarsToInit, InstMap0),
    add_necessary_disj_init_calls(Goals0, Goals, InstMaps0, InstMaps,
        EnsureInitialised, !ModeInfo).

:- func append_init_calls_to_goal(set(prog_var), list(hlds_goal), hlds_goal) =
        hlds_goal.

append_init_calls_to_goal(InitedVars, InitCalls, Goal0) = Goal :-
    Goal0 = GoalExpr0 - GoalInfo0,
    hlds_goal__goal_info_get_nonlocals(GoalInfo0, NonLocals0),
    NonLocals = set__union(InitedVars, NonLocals0),
    hlds_goal__goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
    ( GoalExpr0 = disj(Disjs0) ->
        Disjs = list__map(append_init_calls_to_goal(InitedVars, InitCalls),
                Disjs0),
        Goal  = disj(Disjs) - GoalInfo
    ;
        goal_to_conj_list(Goal0, Conjs),
        conj_list_to_goal(Conjs ++ InitCalls, GoalInfo, Goal)
    ).

:- func flatten_disjs(list(hlds_goal)) = list(hlds_goal).

flatten_disjs(Disjs) = list__foldr(flatten_disj, Disjs, []).

:- func flatten_disj(hlds_goal, list(hlds_goal)) = list(hlds_goal).

flatten_disj(Disj, Disjs0) = Disjs :-
    ( Disj = disj(Disjs1) - _GoalInfo ->
        Disjs = list__foldr(flatten_disj, Disjs1, Disjs0)
    ;
        Disjs = [Disj | Disjs0]
    ).

:- func solver_vars_to_init(list(prog_var), module_info, instmap) =
    list(prog_var).

solver_vars_to_init(Vars, ModuleInfo, InstMap) =
    list__filter(solver_var_to_init(ModuleInfo, InstMap), Vars).

:- pred solver_var_to_init(module_info::in, instmap::in, prog_var::in)
    is semidet.

solver_var_to_init(ModuleInfo, InstMap, Var) :-
    instmap__lookup_var(InstMap, Var, Inst),
    inst_match__inst_is_free(ModuleInfo, Inst).

:- func set_vars_to_inst_any(list(prog_var), instmap) = instmap.

set_vars_to_inst_any([], InstMap) = InstMap.
set_vars_to_inst_any([Var | Vars], InstMap0) = InstMap :-
    instmap__set(Var, any_inst, InstMap0, InstMap1),
    InstMap = set_vars_to_inst_any(Vars, InstMap1).

%-----------------------------------------------------------------------------%

    % Modecheck a conjunction without doing any reordering.
    % This is used by handle_extra_goals above.
    %
:- pred modecheck_conj_list_no_delay(list(hlds_goal)::in, list(hlds_goal)::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

modecheck_conj_list_no_delay([], [], !ModeInfo, !IO).
modecheck_conj_list_no_delay([Goal0 | Goals0], [Goal | Goals], !ModeInfo,
        !IO) :-
    goal_get_nonlocals(Goal0, NonLocals),
    mode_info_remove_live_vars(NonLocals, !ModeInfo),
    modecheck_goal(Goal0, Goal, !ModeInfo, !IO),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    ( instmap__is_unreachable(InstMap) ->
        % We should not mode-analyse the remaining goals, since they
        % are unreachable.  Instead we optimize them away, so that
        % later passes won't complain about them not having mode information.
        mode_info_remove_goals_live_vars(Goals0, !ModeInfo),
        Goals  = []
    ;
        modecheck_conj_list_no_delay(Goals0, Goals, !ModeInfo, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred modecheck_conj_list(list(hlds_goal)::in, list(hlds_goal)::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

modecheck_conj_list(Goals0, Goals, !ModeInfo, !IO) :-
    mode_info_get_errors(!.ModeInfo, OldErrors),
    mode_info_set_errors([], !ModeInfo),

    mode_info_get_may_initialise_solver_vars(MayInitEntryValue, !.ModeInfo),

    mode_info_get_delay_info(!.ModeInfo, DelayInfo0),
    delay_info__enter_conj(DelayInfo0, DelayInfo1),
    mode_info_set_delay_info(DelayInfo1, !ModeInfo),

    mode_info_get_live_vars(!.ModeInfo, LiveVars1),
    mode_info_add_goals_live_vars(Goals0, !ModeInfo),

        % Try to schedule goals without inserting any solver
        % initialisation calls by setting the mode_info flag
        % may_initialise_solver_vars to no.
    mode_info_set_may_initialise_solver_vars(no, !ModeInfo),

    modecheck_conj_list_2(Goals0, Goals1,
        [], RevImpurityErrors0, !ModeInfo, !IO),

    mode_info_get_delay_info(!.ModeInfo, DelayInfo2),
    delay_info__leave_conj(DelayInfo2, DelayedGoals0, DelayInfo3),
    mode_info_set_delay_info(DelayInfo3, !ModeInfo),

        % Otherwise try scheduling by inserting solver
        % initialisation calls where necessary.
    modecheck_delayed_solver_goals(Goals2, DelayedGoals0, DelayedGoals,
        RevImpurityErrors0, RevImpurityErrors, !ModeInfo, !IO),

    Goals = Goals1 ++ Goals2,

    mode_info_get_errors(!.ModeInfo, NewErrors),
    list__append(OldErrors, NewErrors, Errors),
    mode_info_set_errors(Errors, !ModeInfo),

    % We only report impurity errors if there were no other errors.
    (
        DelayedGoals = [],
        %
        % report all the impurity errors
        % (making sure we report the errors in the correct order)
        %
        list__reverse(RevImpurityErrors, ImpurityErrors),
        mode_info_get_errors(!.ModeInfo, Errors5),
        list__append(Errors5, ImpurityErrors, Errors6),
        mode_info_set_errors(Errors6, !ModeInfo)
    ;
        DelayedGoals = [FirstDelayedGoal | MoreDelayedGoals],
        % The variables in the delayed goals should not longer
        % be considered live (the conjunction itself will
        % delay, and its nonlocals will be made live).
        mode_info_set_live_vars(LiveVars1, !ModeInfo),
        (
            MoreDelayedGoals = [],
            FirstDelayedGoal = delayed_goal(_DVars, Error, _DGoal),
            mode_info_add_error(Error, !ModeInfo)
        ;
            MoreDelayedGoals = [_ | _],
            get_all_waiting_vars(DelayedGoals, Vars),
            mode_info_error(Vars,
                mode_error_conj(DelayedGoals, conj_floundered), !ModeInfo)
        )
    ),
        % Restore the value of the may_initialise_solver_vars flag.
    mode_info_set_may_initialise_solver_vars(MayInitEntryValue, !ModeInfo).

mode_info_add_goals_live_vars([], !ModeInfo).
mode_info_add_goals_live_vars([Goal | Goals], !ModeInfo) :-
    % We add the live vars for the goals in the goal list
    % in reverse order, because this ensures that in the
    % common case (where there is no delaying), when we come
    % to remove the live vars for the first goal
    % they will have been added last and will thus be
    % at the start of the list of live vars sets, which
    % makes them cheaper to remove.
    mode_info_add_goals_live_vars(Goals, !ModeInfo),
    (
        % Recurse into conjunctions, in case there are any conjunctions
        % that have not been flattened.
        Goal = conj(ConjGoals) - _
    ->
        mode_info_add_goals_live_vars(ConjGoals, !ModeInfo)
    ;
        goal_get_nonlocals(Goal, NonLocals),
        mode_info_add_live_vars(NonLocals, !ModeInfo)
    ).

mode_info_remove_goals_live_vars([], !ModeInfo).
mode_info_remove_goals_live_vars([Goal | Goals], !ModeInfo) :-
    (
        % Recurse into conjunctions, in case there are any conjunctions
        % that have not been flattened.
        Goal = conj(ConjGoals) - _
    ->
        mode_info_remove_goals_live_vars(ConjGoals, !ModeInfo)
    ;
        goal_get_nonlocals(Goal, NonLocals),
        mode_info_remove_live_vars(NonLocals, !ModeInfo)
    ),
    mode_info_remove_goals_live_vars(Goals, !ModeInfo).

:- type impurity_errors == list(mode_error_info).

    % Flatten conjunctions as we go.  Call modecheck_conj_list_3 to do
    % the actual scheduling.
    %
:- pred modecheck_conj_list_2(list(hlds_goal)::in, list(hlds_goal)::out,
    impurity_errors::in, impurity_errors::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

modecheck_conj_list_2([], [], !ImpurityErrors, !ModeInfo, !IO).
modecheck_conj_list_2([Goal0 | Goals0], Goals, !ImpurityErrors, !ModeInfo,
        !IO) :-
    (
        Goal0 = conj(ConjGoals) - _
    ->
        list__append(ConjGoals, Goals0, Goals1),
        modecheck_conj_list_2(Goals1, Goals, !ImpurityErrors, !ModeInfo, !IO)
    ;
        modecheck_conj_list_3(Goal0, Goals0, Goals, !ImpurityErrors,
            !ModeInfo, !IO)
    ).

:- pred modecheck_conj_list_3(hlds_goal::in, list(hlds_goal)::in,
    list(hlds_goal)::out, impurity_errors::in, impurity_errors::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

    % Schedule a conjunction.  If it's empty, then there is nothing to do.
    % For non-empty conjunctions, we attempt to schedule the first
    % goal in the conjunction.  If successful, we wakeup a newly
    % pending goal (if any), and if not, we delay the goal.  Then we
    % continue attempting to schedule all the rest of the goals.
    %
modecheck_conj_list_3(Goal0, Goals0, Goals, !ImpurityErrors, !ModeInfo, !IO) :-
    Goal0 = _GoalExpr - GoalInfo0,
    ( goal_info_is_impure(GoalInfo0) ->
        Impure = yes,
        check_for_impurity_error(Goal0, ScheduledSolverGoals,
            !ImpurityErrors, !ModeInfo, !IO)
    ;
        Impure = no,
        ScheduledSolverGoals = []
    ),

        % Hang onto the original instmap, delay_info, and live_vars
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    mode_info_get_delay_info(!.ModeInfo, DelayInfo0),

        % Modecheck the goal, noting first that the non-locals
        % which occur in the goal might not be live anymore.
    goal_get_nonlocals(Goal0, NonLocalVars),
    mode_info_remove_live_vars(NonLocalVars, !ModeInfo),
    modecheck_goal(Goal0, Goal, !ModeInfo, !IO),

        % Now see whether the goal was successfully scheduled.
        % If we didn't manage to schedule the goal, then we
        % restore the original instmap, delay_info and livevars
        % here, and delay the goal.
    mode_info_get_errors(!.ModeInfo, Errors),
    (
        Errors = [FirstErrorInfo | _],
        mode_info_set_errors([], !ModeInfo),
        mode_info_set_instmap(InstMap0, !ModeInfo),
        mode_info_add_live_vars(NonLocalVars, !ModeInfo),
        delay_info__delay_goal(DelayInfo0, FirstErrorInfo, Goal0, DelayInfo1),
        %  delaying an impure goal is an impurity error
        (
            Impure = yes,
            FirstErrorInfo = mode_error_info(Vars, _, _, _),
            ImpureError = mode_error_conj(
                [delayed_goal(Vars, FirstErrorInfo, Goal0)],
                goal_itself_was_impure),
            mode_info_get_context(!.ModeInfo, Context),
            mode_info_get_mode_context(!.ModeInfo, ModeContext),
            ImpureErrorInfo = mode_error_info(Vars, ImpureError,
                Context, ModeContext),
            !:ImpurityErrors = [ImpureErrorInfo | !.ImpurityErrors]
        ;
            Impure = no
        )
    ;
        Errors = [],
        mode_info_get_delay_info(!.ModeInfo, DelayInfo1)
    ),

        % Next, we attempt to wake up any pending goals,
        % and then continue scheduling the rest of the goal.
    delay_info__wakeup_goals(WokenGoals, DelayInfo1, DelayInfo),
    list__append(WokenGoals, Goals0, Goals1),
    (
        WokenGoals = []
    ;
        WokenGoals = [_],
        mode_checkpoint(wakeup, "goal", !ModeInfo, !IO)
    ;
        WokenGoals = [_, _ | _],
        mode_checkpoint(wakeup, "goals", !ModeInfo, !IO)
    ),
    mode_info_set_delay_info(DelayInfo, !ModeInfo),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    ( instmap__is_unreachable(InstMap) ->
        % We should not mode-analyse the remaining goals, since they
        % are unreachable.  Instead we optimize them away, so that
        % later passes won't complain about them not having mode
        % information.
        mode_info_remove_goals_live_vars(Goals1, !ModeInfo),
        Goals2  = []
    ;
        % The remaining goals may still need to be flattened.
        modecheck_conj_list_2(Goals1, Goals2, !ImpurityErrors, !ModeInfo, !IO)
    ),
    (
        Errors = [_ | _],
        % We delayed this goal -- it will be stored in the delay_info.
        Goals = ScheduledSolverGoals ++ Goals2
    ;
        Errors = [],
        % We successfully scheduled this goal, so insert it
        % in the list of successfully scheduled goals.
        % We flatten out conjunctions if we can. They can arise
        % when Goal0 was a scope(from_ground_term, _) goal.
        ( Goal = conj(SubGoals) - _ ->
            Goals = ScheduledSolverGoals ++ SubGoals ++ Goals2
        ;
            Goals = ScheduledSolverGoals ++ [Goal | Goals2]
        )
    ).

    % We may still have some unscheduled goals.  This may be because some
    % initialisation calls are needed to turn some solver type vars
    % from inst free to inst any.  This predicate attempts to schedule
    % such goals.
    %
:- pred modecheck_delayed_solver_goals(list(hlds_goal)::out,
    list(delayed_goal)::in, list(delayed_goal)::out,
    impurity_errors::in, impurity_errors::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

modecheck_delayed_solver_goals(Goals, DelayedGoals0, DelayedGoals,
        !ImpurityErrors, !ModeInfo, !IO) :-

        % Try to handle any unscheduled goals by inserting solver
        % initialisation calls, aiming for a deterministic
        % schedule.
        %
    modecheck_delayed_goals_try_det(DelayedGoals0, DelayedGoals1, Goals0,
        !ImpurityErrors, !ModeInfo, !IO),

        % Try to handle any unscheduled goals by inserting solver
        % initialisation calls, aiming for *any* workable schedule.
        %
    modecheck_delayed_goals_eager(DelayedGoals1, DelayedGoals, Goals1,
        !ImpurityErrors, !ModeInfo, !IO),
    Goals = Goals0 ++ Goals1.

    % We may still have some unscheduled goals.  This may be because some
    % initialisation calls are needed to turn some solver type vars
    % from inst free to inst any.  This pass attempts to identify a
    % minimal subset of such vars to initialise that will allow the
    % remaining goals to be scheduled in a deterministic fashion.
    %
    % This works as follows.  If a deterministic schedule exists for
    % the remaining goals, then each subgoal must also be deterministic.
    % Moreover, no call may employ an implied mode since these mean
    % introducing a semidet unification.  Therefore we only need to
    % consider det procs for calls, constructions for var/functor
    % unifications, and assignments for var/var unifications.
    %
    % If a consistent deterministic schedule exists then every
    % variable involved in the goals either
    % - has already been instantiated;
    % - will be instantiated by a single remaining subgoal;
    % - will not be instantiated by any remaining subgoal.
    % Variables in this last category that are solver type variables
    % should be initialised.  If all the variables that will remain
    % uninstantiated are in this last category then, after inserting
    % initialisation call, we should expect another attempt at
    % scheduling the remaining goals to succeed and produce a
    % deterministic result.
    %
    % XXX At some point we should extend this analysis to handle
    % disjunction, if-then-else goals, and negation.
    %
:- pred modecheck_delayed_goals_try_det(list(delayed_goal)::in,
    list(delayed_goal)::out, list(hlds_goal)::out,
    impurity_errors::in, impurity_errors::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

modecheck_delayed_goals_try_det(DelayedGoals0, DelayedGoals, Goals,
        !ImpurityErrors, !ModeInfo, !IO) :-
    (
            % There are no unscheduled goals, so we don't
            % need to do anything.
            %
        DelayedGoals0 = [],
        DelayedGoals  = [],
        Goals         = []
    ;
            % There are some unscheduled goals.  See if
            % allowing extra initialisation calls (for
            % a single goal) makes a difference.
            %
        DelayedGoals0 = [_ | _],

        (
                % Extract the HLDS goals from the delayed
                % goals.
                %
            Goals0 = list__map(hlds_goal_from_delayed_goal, DelayedGoals0),

                % Work out which vars are already
                % instantiated (i.e. have non-free insts).
                %
            mode_info_get_instmap(!.ModeInfo, InstMap),
            instmap__to_assoc_list(InstMap, VarInsts),
            NonFreeVars0 = set__list_to_set(
                non_free_vars_in_assoc_list(VarInsts)),

                % Find the set of vars whose instantiation should lead to
                % a deterministic schedule.
                %
            promise_equivalent_solutions [CandidateInitVars] (
                candidate_init_vars(!.ModeInfo, Goals0, NonFreeVars0,
                    CandidateInitVars)
            ),

                % And verify that all of these vars are
                % solver type vars (and can therefore be
                % initialised.)
                %
            mode_info_get_module_info(!.ModeInfo, ModuleInfo),
            mode_info_get_var_types(!.ModeInfo, VarTypes),
            all [Var] (
                set__member(Var, CandidateInitVars)
            =>
                (
                    map__lookup(VarTypes, Var, VarType),
                    type_util__type_is_solver_type(ModuleInfo, VarType)
                )
            )
        ->
                % Construct the inferred initialisation goals
                % and try scheduling again.
                %
            CandidateInitVarList = set__to_sorted_list(CandidateInitVars),
            construct_initialisation_calls(CandidateInitVarList,
                InitGoals, !ModeInfo),
            Goals1 = InitGoals ++ Goals0,

            mode_info_get_delay_info(!.ModeInfo, DelayInfo0),
            delay_info__enter_conj(DelayInfo0, DelayInfo1),
            mode_info_set_delay_info(DelayInfo1, !ModeInfo),

            mode_info_add_goals_live_vars(InitGoals, !ModeInfo),

            modecheck_conj_list_2(Goals1, Goals, !ImpurityErrors, !ModeInfo,
                !IO),

            mode_info_get_delay_info(!.ModeInfo, DelayInfo2),
            delay_info__leave_conj(DelayInfo2, DelayedGoals, DelayInfo3),
            mode_info_set_delay_info(DelayInfo3, !ModeInfo)
        ;
                % We couldn't identify a deterministic
                % solution.
            DelayedGoals = DelayedGoals0,
            Goals        = []
        )
    ).

construct_initialisation_calls([], [], !ModeInfo).
construct_initialisation_calls([Var | Vars], [Goal | Goals], !ModeInfo) :-
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    map__lookup(VarTypes, Var, VarType),
    InitialInst           = free,
    Context               = term__context_init,
    MaybeCallUnifyContext = no,
    construct_initialisation_call(Var, VarType, InitialInst, Context,
        MaybeCallUnifyContext, Goal, !ModeInfo),
    construct_initialisation_calls(Vars, Goals, !ModeInfo).

    % XXX will this catch synonyms for `free'?
    % N.B. This is perhaps the only time when `for' and `free'
    % can be juxtaposed grammatically :-)
    %
:- func non_free_vars_in_assoc_list(assoc_list(prog_var, mer_inst)) =
    list(prog_var).

non_free_vars_in_assoc_list([]) = [].
non_free_vars_in_assoc_list([Var - Inst | AssocList]) =
    ( ( Inst = free ; Inst = free(_) ) ->
        non_free_vars_in_assoc_list(AssocList)
    ;
        [Var | non_free_vars_in_assoc_list(AssocList)]
    ).

    % Find a set of vars that, if they were instantiated, might
    % lead to a deterministic scheduling of the given goals.
    %
    % This approximation is fairly crude: it only considers variables as
    % being free or non-free, rather than having detailed insts.
    %
    % XXX Does not completely handle negation, disjunction, if_then_else
    % goals, foreign_code, or var/lambda unifications.
    %
:- pred candidate_init_vars(mode_info::in, list(hlds_goal)::in,
    set(prog_var)::in, set(prog_var)::out) is cc_nondet.

candidate_init_vars(ModeInfo, Goals, NonFreeVars0, CandidateVars) :-
    CandidateVars0 = set__init,
    candidate_init_vars_2(ModeInfo, Goals, NonFreeVars0, NonFreeVars1,
        CandidateVars0, CandidateVars1),
    CandidateVars = set__difference(CandidateVars1, NonFreeVars1).

:- pred candidate_init_vars_2(mode_info::in, list(hlds_goal)::in,
    set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is nondet.

candidate_init_vars_2(ModeInfo, Goals, !NonFree, !CandidateVars) :-
    list__foldl2(candidate_init_vars_3(ModeInfo), Goals,
        !NonFree, !CandidateVars).

:- pred candidate_init_vars_3(mode_info::in, hlds_goal::in,
    set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is nondet.

candidate_init_vars_3(_ModeInfo, Goal, !NonFree, !CandidateVars) :-
        % A var/var unification.
        %
    Goal = unify(X, RHS, _, _, _) - _GoalInfo,
    RHS  = var(Y),
    (
        set__member(X, !.NonFree)
    ->
        not set__member(Y, !.NonFree),
            % It's an assignment from X to Y.
        !:NonFree = set__insert(!.NonFree, Y)
    ;
        set__member(Y, !.NonFree)
    ->
            % It's an assignment from Y to X.
        !:NonFree = set__insert(!.NonFree, X)
    ;
            % It's an assignment one way or the other.
        (
            !:NonFree       = set__insert(!.NonFree, X),
            !:CandidateVars = set__insert(!.CandidateVars, Y)
        ;
            !:NonFree       = set__insert(!.NonFree, Y),
            !:CandidateVars = set__insert(!.CandidateVars, X)
        )
    ).

candidate_init_vars_3(_ModeInfo, Goal, !NonFree, !CandidateVars) :-
        % A var/functor unification, which can only be deterministic
        % if it is a construction.
        %
    Goal = unify(X, RHS, _, _, _) - _GoalInfo,
    RHS  = functor(_, _, Args),
        % If this is a construction then X must be free.
    not set__member(X, !.NonFree),
        % But X becomes instantiated.
    !:NonFree = set__insert(!.NonFree, X),
        % And the Args are potential candidates for initialisation.
    !:CandidateVars = set__insert_list(!.CandidateVars, Args).

candidate_init_vars_3(_ModeInfo, Goal, !NonFree, !CandidateVars) :-
        % A var/lambda unification, which can only be deterministic
        % if it is a construction.  The non-locals in the lambda are
        % *not* candidates for initialisation because that could
        % permit violations of referential transparency (executing
        % the lambda could otherwise further constrain a solver
        % variable that was not supplied as an argument).
        %
    Goal = unify(X, RHS, _, _, _) - _GoalInfo,
    RHS  = lambda_goal(_, _, _, _, _, _, _, _, _),
        % If this is a construction then X must be free.
    not set__member(X, !.NonFree),
        % But X becomes instantiated.
    !:NonFree = set__insert(!.NonFree, X).

candidate_init_vars_3(_ModeInfo, Goal, !NonFree, !CandidateVars) :-
        % Disjunctions are tricky, because we don't perform
        % switch analysis until after mode analysis.  So
        % here we assume that the disjunction is a det switch
        % and that we can ignore it for the purposes of identifying
        % candidate vars for initialisation.
    Goal = disj(_Goals) - _GoalInfo.

candidate_init_vars_3(ModeInfo, Goal, !NonFree, !CandidateVars) :-
        % We ignore the condition of an if-then-else goal,
        % other than to assume that it binds its non-solver-type
        % non-locals, but proceed on the assumption that the then
        % and else arms are det.  This isn't very accurate and may
        % need refinement.
        %
    Goal = if_then_else(_LocalVars, CondGoal, ThenGoal, ElseGoal) - _GoalInfo,

    CondGoal = _CondGoalExpr - CondGoalInfo,
    hlds_goal__goal_info_get_nonlocals(CondGoalInfo, NonLocals),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_var_types(ModeInfo, VarTypes),
    NonSolverNonLocals =
        set__filter(non_solver_var(ModuleInfo, VarTypes), NonLocals),
    !:NonFree = set__union(NonSolverNonLocals, !.NonFree),

    candidate_init_vars_3(ModeInfo, ThenGoal, !.NonFree, NonFreeThen,
        !CandidateVars),
    candidate_init_vars_3(ModeInfo, ElseGoal, !.NonFree, NonFreeElse,
        !CandidateVars),
    !:NonFree = set__union(NonFreeThen, NonFreeElse).

candidate_init_vars_3(ModeInfo, Goal, !NonFree, !CandidateVars) :-
        % A parallel conjunction.
        %
    Goal = par_conj(Goals) - _GoalInfo,
    candidate_init_vars_2(ModeInfo, Goals, !NonFree, !CandidateVars).

candidate_init_vars_3(ModeInfo, Goal0, !NonFree, !CandidateVars) :-
        % An existentially quantified goal.
        %
    Goal0 = scope(_, Goal) - _GoalInfo,
    candidate_init_vars_3(ModeInfo, Goal, !NonFree, !CandidateVars).

candidate_init_vars_3(ModeInfo, Goal, !NonFree, !CandidateVars) :-
        % A conjunction.
        %
    Goal = conj(Goals) - _GoalInfo,
    candidate_init_vars_2(ModeInfo, Goals, !NonFree, !CandidateVars).

candidate_init_vars_3(ModeInfo, Goal, !NonFree, !CandidateVars) :-
        % XXX Is the determinism field of a generic_call
        % valid at this point?  Determinism analysis is run after
        % mode analysis.
        %
        % We assume that generic calls are deterministic.
        % The modes field of higher_order calls is junk until
        % *after* mode analysis, hence we can't handle them here.
        %
    Goal = generic_call(Details, Args, ArgModes, _JunkDetism) - _GoalInfo,
    Details \= higher_order(_, _, _, _),
    candidate_init_vars_call(ModeInfo, Args, ArgModes,
        !NonFree, !CandidateVars).

candidate_init_vars_3(ModeInfo, Goal, !NonFree, !CandidateVars) :-
        % A call (at this point the ProcId is just a dummy value
        % since it isn't meaningful until the call is scheduled.)
        %
    Goal = call(PredId, _, Args, _, _, _) - _GoalInfo,
        % Find a deterministic proc for this call.
        %
    mode_info_get_preds(ModeInfo, Preds),
    map__lookup(Preds, PredId, PredInfo),
    pred_info_procedures(PredInfo, ProcTable),
    map__values(ProcTable, ProcInfos),
    list__member(ProcInfo, ProcInfos),
    proc_info_declared_determinism(ProcInfo, yes(DeclaredDetism)),
    ( DeclaredDetism = (det) ; DeclaredDetism = (cc_multidet) ),
        % Find the argument modes.
        %
    proc_info_argmodes(ProcInfo, ArgModes),
        % Process the call args.
        %
    candidate_init_vars_call(ModeInfo, Args, ArgModes,
        !NonFree, !CandidateVars).

    % Filter pred succeeding if a variable does not have a solver type.
    %
:- pred non_solver_var(module_info::in, vartypes::in, prog_var::in) is semidet.

non_solver_var(ModuleInfo, VarTypes, Var) :-
    VarType = VarTypes ^ det_elem(Var),
    not type_util__type_is_solver_type(ModuleInfo, VarType).

    % Update !NonFree and !CandidateVars given the args and modes for
    % a call.
    %
:- pred candidate_init_vars_call(mode_info::in,
    list(prog_var)::in, list(mer_mode)::in,
    set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is semidet.

candidate_init_vars_call(_ModeInfo, [], [], !NonFree, !CandidateVars).

candidate_init_vars_call(ModeInfo, [Arg | Args], [Mode | Modes],
        !NonFree, !CandidateVars) :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_get_insts_semidet(ModuleInfo, Mode, InitialInst, FinalInst),
    (
        InitialInst \= free,
        InitialInst \= free(_)
    ->
        % This arg is an input that needs instantiation.
        !:CandidateVars = set__insert(!.CandidateVars, Arg)
    ;
        % Otherwise this arg could be an output...
        FinalInst \= free,
        FinalInst \= free(_)
    ->
        % And it is.
        (
            not set__contains(!.NonFree, Arg)
        ->
            % This arg is instantiated on output.
            !:NonFree = set__insert(!.NonFree, Arg)
        ;
            % This arg appears in an implied mode.
            false
        )
    ;
        % This arg is unused.
        true
    ),
    candidate_init_vars_call(ModeInfo, Args, Modes, !NonFree, !CandidateVars).

    % We may still have some unscheduled goals.  This may be because some
    % initialisation calls are needed to turn some solver type vars
    % from inst free to inst any.  This pass tries to unblock the
    % remaining goals by conservatively inserting initialisation calls.
    % It is "eager" in the sense that as soon as it encounters a sub-goal
    % that may be unblocked this way it tries to do so.
    %
:- pred modecheck_delayed_goals_eager(list(delayed_goal)::in,
    list(delayed_goal)::out, list(hlds_goal)::out,
    impurity_errors::in, impurity_errors::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

modecheck_delayed_goals_eager(DelayedGoals0, DelayedGoals, Goals,
        !ImpurityErrors, !ModeInfo, !IO) :-
    (
            % There are no unscheduled goals, so we don't
            % need to do anything.
            %
        DelayedGoals0 = [],
        DelayedGoals  = [],
        Goals         = []
    ;
            % There are some unscheduled goals.  See if
            % allowing extra initialisation calls (for
            % a single goal) makes a difference.
            %
        DelayedGoals0 = [_ | _],

        Goals0 = list__map(hlds_goal_from_delayed_goal, DelayedGoals0),

        mode_info_get_delay_info(!.ModeInfo, DelayInfo0),
        delay_info__enter_conj(DelayInfo0, DelayInfo1),
        mode_info_set_delay_info(DelayInfo1, !ModeInfo),

        mode_info_set_may_initialise_solver_vars(yes, !ModeInfo),
        modecheck_conj_list_2(Goals0, Goals1, !ImpurityErrors,
            !ModeInfo, !IO),
        mode_info_set_may_initialise_solver_vars(no, !ModeInfo),

        mode_info_get_delay_info(!.ModeInfo, DelayInfo2),
        delay_info__leave_conj(DelayInfo2, DelayedGoals1, DelayInfo3),
        mode_info_set_delay_info(DelayInfo3, !ModeInfo),

            % See if we scheduled any goals.
            %
        (
            length(DelayedGoals1) < length(DelayedGoals0)
        ->
                % We scheduled some goals.  Keep going
                % until we flounder or succeed.
                %
            modecheck_delayed_goals_eager(DelayedGoals1, DelayedGoals,
                Goals2, !ImpurityErrors, !ModeInfo, !IO),
            Goals = Goals1 ++ Goals2
        ;
            DelayedGoals = DelayedGoals1,
            Goals = Goals1
        )
    ).

:- func hlds_goal_from_delayed_goal(delayed_goal) = hlds_goal.

hlds_goal_from_delayed_goal(delayed_goal(_WaitingVars, _ModeError, Goal)) =
    Goal.

    % Check whether there are any delayed goals (other than headvar
    % unifications) at the point where we are about to schedule an impure goal.
    % If so, that is an error. Headvar unifications are allowed to be delayed
    % because in the case of output arguments, they cannot be scheduled
    % until the variable value is known. If headvar unifications couldn't be
    % delayed past impure goals, impure predicates wouldn't be able to have
    % outputs! (Note that we first try to schedule any delayed solver goals
    % waiting for initialisation.)
    %
:- pred check_for_impurity_error(hlds_goal::in, list(hlds_goal)::out,
    impurity_errors::in, impurity_errors::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

check_for_impurity_error(Goal, Goals, !ImpurityErrors, !ModeInfo, !IO) :-
    mode_info_get_delay_info(!.ModeInfo, DelayInfo0),
    delay_info__leave_conj(DelayInfo0, DelayedGoals0, DelayInfo1),
    mode_info_set_delay_info(DelayInfo1, !ModeInfo),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_predid(!.ModeInfo, PredId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_clauses_info(PredInfo, ClausesInfo),
    clauses_info_headvars(ClausesInfo, HeadVars),
    filter_headvar_unification_goals(HeadVars, DelayedGoals0,
        HeadVarUnificationGoals, NonHeadVarUnificationGoals0),
    modecheck_delayed_solver_goals(Goals,
        NonHeadVarUnificationGoals0, NonHeadVarUnificationGoals,
        !ImpurityErrors, !ModeInfo, !IO),
    mode_info_get_delay_info(!.ModeInfo, DelayInfo2),
    delay_info__enter_conj(DelayInfo2, DelayInfo3),
    redelay_goals(HeadVarUnificationGoals, DelayInfo3, DelayInfo),
    mode_info_set_delay_info(DelayInfo, !ModeInfo),
    (
        NonHeadVarUnificationGoals = []
    ;
        NonHeadVarUnificationGoals = [_ | _],
        get_all_waiting_vars(NonHeadVarUnificationGoals, Vars),
        ModeError = mode_error_conj(NonHeadVarUnificationGoals,
            goals_followed_by_impure_goal(Goal)),
        mode_info_get_context(!.ModeInfo, Context),
        mode_info_get_mode_context(!.ModeInfo, ModeContext),
        ImpurityError = mode_error_info(Vars, ModeError, Context, ModeContext),
        !:ImpurityErrors = [ImpurityError | !.ImpurityErrors]
    ).

:- pred filter_headvar_unification_goals(list(prog_var)::in,
    list(delayed_goal)::in, list(delayed_goal)::out,
    list(delayed_goal)::out) is det.

filter_headvar_unification_goals(HeadVars, DelayedGoals,
        HeadVarUnificationGoals, NonHeadVarUnificationGoals) :-
    list__filter(is_headvar_unification_goal(HeadVars), DelayedGoals,
        HeadVarUnificationGoals, NonHeadVarUnificationGoals).

:- pred is_headvar_unification_goal(list(prog_var)::in, delayed_goal::in)
    is semidet.

is_headvar_unification_goal(HeadVars, delayed_goal(_, _, Goal - _)) :-
    Goal = unify(Var, RHS, _, _, _),
    (
        list__member(Var, HeadVars)
    ;
        RHS = var(OtherVar),
        list__member(OtherVar, HeadVars)
    ).

    % Given an association list of Vars - Goals,
    % combine all the Vars together into a single set.

:- pred get_all_waiting_vars(list(delayed_goal)::in, set(prog_var)::out)
    is det.

get_all_waiting_vars(DelayedGoals, Vars) :-
    get_all_waiting_vars_2(DelayedGoals, set__init, Vars).

:- pred get_all_waiting_vars_2(list(delayed_goal)::in,
    set(prog_var)::in, set(prog_var)::out) is det.

get_all_waiting_vars_2([], Vars, Vars).
get_all_waiting_vars_2([delayed_goal(Vars1, _, _) | Rest], Vars0, Vars) :-
    set__union(Vars0, Vars1, Vars2),
    get_all_waiting_vars_2(Rest, Vars2, Vars).

:- pred redelay_goals(list(delayed_goal)::in, delay_info::in, delay_info::out)
    is det.

redelay_goals([], DelayInfo, DelayInfo).

redelay_goals([DelayedGoal | DelayedGoals], DelayInfo0, DelayInfo) :-
    DelayedGoal = delayed_goal(_WaitingVars, ModeErrorInfo, Goal),
    delay_info__delay_goal(DelayInfo0, ModeErrorInfo, Goal, DelayInfo1),
    redelay_goals(DelayedGoals, DelayInfo1, DelayInfo).

%-----------------------------------------------------------------------------%

:- pred modecheck_disj_list(list(hlds_goal)::in, list(hlds_goal)::out,
    list(instmap)::out, mode_info::in, mode_info::out,
    io::di, io::uo) is det.

modecheck_disj_list([], [], [], !ModeInfo, !IO).
modecheck_disj_list([Goal0 | Goals0], [Goal | Goals], [InstMap | InstMaps],
        !ModeInfo, !IO) :-
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    modecheck_goal(Goal0, Goal, !ModeInfo, !IO),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    modecheck_disj_list(Goals0, Goals, InstMaps, !ModeInfo, !IO).

:- pred modecheck_case_list(list(case)::in, prog_var::in, list(case)::out,
    list(instmap)::out, mode_info::in, mode_info::out,
    io::di, io::uo) is det.

modecheck_case_list([], _Var, [], [], !ModeInfo, !IO).
modecheck_case_list([Case0 | Cases0], Var, [Case | Cases],
        [InstMap | InstMaps], !ModeInfo, !IO) :-
    Case0 = case(ConsId, Goal0),
    mode_info_get_instmap(!.ModeInfo, InstMap0),

    % Record the fact that Var was bound to ConsId in the
    % instmap before processing this case.
    modecheck_functor_test(Var, ConsId, !ModeInfo),

    % Modecheck this case (if it is reachable).
    mode_info_get_instmap(!.ModeInfo, InstMap1),
    ( instmap__is_reachable(InstMap1) ->
        modecheck_goal(Goal0, Goal1, !ModeInfo, !IO),
        mode_info_get_instmap(!.ModeInfo, InstMap)
    ;
        % We should not mode-analyse the goal, since it is unreachable.
        % Instead we optimize the goal away, so that later passes
        % won't complain about it not having mode information.
        true_goal(Goal1),
        InstMap = InstMap1
    ),

    % Don't lose the information added by the functor test above.
    fixup_switch_var(Var, InstMap0, InstMap, Goal1, Goal),
    Case = case(ConsId, Goal),

    mode_info_set_instmap(InstMap0, !ModeInfo),
    modecheck_case_list(Cases0, Var, Cases, InstMaps, !ModeInfo, !IO).

    % modecheck_functor_test(ConsId, Var):
    %
    % Update the instmap to reflect the fact that Var was bound to ConsId.
    % This is used for the functor tests in `switch' statements.
    %
modecheck_functor_test(Var, ConsId, !ModeInfo) :-
        % figure out the arity of this constructor,
        % _including_ any type-infos or typeclass-infos
        % inserted for existential data types.
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    map__lookup(VarTypes, Var, Type),
    AdjustedArity = cons_id_adjusted_arity(ModuleInfo, Type, ConsId),

        % record the fact that Var was bound to ConsId in the instmap
    list__duplicate(AdjustedArity, free, ArgInsts),
    modecheck_set_var_inst(Var, bound(unique, [functor(ConsId, ArgInsts)]),
        no, !ModeInfo).

%-----------------------------------------------------------------------------%

:- pred modecheck_par_conj_list(list(hlds_goal)::in, list(hlds_goal)::out,
    set(prog_var)::in, list(pair(instmap, set(prog_var)))::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

modecheck_par_conj_list([], [], _NonLocals, [], !ModeInfo, !IO).
modecheck_par_conj_list([Goal0 | Goals0], [Goal | Goals], NonLocals,
        [InstMap - GoalNonLocals | InstMaps], !ModeInfo, !IO) :-
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    Goal0 = _ - GoalInfo,
    goal_info_get_nonlocals(GoalInfo, GoalNonLocals),
    mode_info_get_parallel_vars(!.ModeInfo, PVars0),
    set__init(Bound0),
    mode_info_set_parallel_vars([NonLocals - Bound0 | PVars0], !ModeInfo),

    modecheck_goal(Goal0, Goal, !ModeInfo, !IO),
    mode_info_get_parallel_vars(!.ModeInfo, PVars1),
    (
        PVars1 = [_ - Bound1 | PVars2],
        (
            PVars2 = [OuterNonLocals - OuterBound0 | PVars3],
            set__intersect(OuterNonLocals, Bound1, Bound),
            set__union(OuterBound0, Bound, OuterBound),
            PVars = [OuterNonLocals - OuterBound | PVars3],
            mode_info_set_parallel_vars(PVars, !ModeInfo)
        ;
            PVars2 = [],
            mode_info_set_parallel_vars(PVars2, !ModeInfo)
        )
    ;
        PVars1 = [],
        error("lost parallel vars")
    ),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    mode_info_lock_vars(par_conj, Bound1, !ModeInfo),
    modecheck_par_conj_list(Goals0, Goals, NonLocals, InstMaps, !ModeInfo,
        !IO),
    mode_info_unlock_vars(par_conj, Bound1, !ModeInfo).

%-----------------------------------------------------------------------------%

    % Calculate the argument number offset that needs to be passed to
    % modecheck_var_list_is_live, modecheck_var_has_inst_list, and
    % modecheck_set_var_inst_list.  This offset number is calculated
    % so that real arguments get positive argument numbers and
    % type_info arguments get argument numbers less than or equal to 0.
    %
compute_arg_offset(PredInfo, ArgOffset) :-
    OrigArity = pred_info_orig_arity(PredInfo),
    pred_info_arg_types(PredInfo, ArgTypes),
    list__length(ArgTypes, CurrentArity),
    ArgOffset = OrigArity - CurrentArity.

%-----------------------------------------------------------------------------%

    % Given a list of variables and a list of expected livenesses,
    % ensure the liveness of each variable satisfies the corresponding
    % expected liveness.
    %
modecheck_var_list_is_live([_ | _], [], _, _, !ModeInfo) :-
    error("modecheck_var_list_is_live: length mismatch").
modecheck_var_list_is_live([], [_ | _], _, _, !ModeInfo) :-
    error("modecheck_var_list_is_live: length mismatch").
modecheck_var_list_is_live([], [], _NeedExactMatch, _ArgNum, !ModeInfo).
modecheck_var_list_is_live([Var | Vars], [IsLive | IsLives], NeedExactMatch,
        ArgNum0, !ModeInfo) :-
    ArgNum = ArgNum0 + 1,
    mode_info_set_call_arg_context(ArgNum, !ModeInfo),
    modecheck_var_is_live(Var, IsLive, NeedExactMatch, !ModeInfo),
    modecheck_var_list_is_live(Vars, IsLives, NeedExactMatch, ArgNum,
        !ModeInfo).

:- pred modecheck_var_is_live(prog_var::in, is_live::in, bool::in,
    mode_info::in, mode_info::out) is det.

    % `live' means possibly used later on, and
    % `dead' means definitely not used later on.
    % If you don't need an exact match, then
    % the only time you get an error is if you pass a variable
    % which is live to a predicate that expects the variable to
    % be dead; the predicate may use destructive update to clobber
    % the variable, so we must be sure that it is dead after the call.
    %
modecheck_var_is_live(VarId, ExpectedIsLive, NeedExactMatch, !ModeInfo) :-
    mode_info_var_is_live(!.ModeInfo, VarId, VarIsLive),
    (
        ( ExpectedIsLive = dead, VarIsLive = live
        ; NeedExactMatch = yes, VarIsLive \= ExpectedIsLive
        )
    ->
        set__singleton_set(WaitingVars, VarId),
        mode_info_error(WaitingVars, mode_error_var_is_live(VarId), !ModeInfo)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % Given a list of variables and a list of initial insts, ensure
    % that the inst of each variable matches the corresponding initial
    % inst.
    %
modecheck_var_has_inst_list(Vars, Insts, NeedEaxctMatch, ArgNum, Subst,
        !ModeInfo) :-
    modecheck_var_has_inst_list_2(Vars, Insts, NeedEaxctMatch, ArgNum,
        map__init, Subst, !ModeInfo).

:- pred modecheck_var_has_inst_list_2(list(prog_var)::in, list(mer_inst)::in,
    bool::in, int::in, inst_var_sub::in, inst_var_sub::out,
    mode_info::in, mode_info::out) is det.

modecheck_var_has_inst_list_2([_ | _], [], _, _, !Subst, !ModeInfo) :-
    error("modecheck_var_has_inst_list: length mismatch").
modecheck_var_has_inst_list_2([], [_ | _], _, _, !Subst, !ModeInfo) :-
    error("modecheck_var_has_inst_list: length mismatch").
modecheck_var_has_inst_list_2([], [], _Exact, _ArgNum, !Subst, !ModeInfo).
modecheck_var_has_inst_list_2([Var | Vars], [Inst | Insts], NeedExactMatch,
        ArgNum0, !Subst, !ModeInfo) :-
    ArgNum = ArgNum0 + 1,
    mode_info_set_call_arg_context(ArgNum, !ModeInfo),
    modecheck_var_has_inst(Var, Inst, NeedExactMatch, !Subst, !ModeInfo),
    modecheck_var_has_inst_list_2(Vars, Insts, NeedExactMatch, ArgNum,
        !Subst, !ModeInfo).

:- pred modecheck_var_has_inst(prog_var::in, mer_inst::in, bool::in,
    inst_var_sub::in, inst_var_sub::out,
    mode_info::in, mode_info::out) is det.

modecheck_var_has_inst(VarId, Inst, NeedExactMatch, !Subst, !ModeInfo) :-
    mode_info_get_instmap(!.ModeInfo, InstMap),
    instmap__lookup_var(InstMap, VarId, VarInst),
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    map__lookup(VarTypes, VarId, Type),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    (
        (
            NeedExactMatch = no,
            inst_matches_initial(VarInst, Inst, Type, ModuleInfo0,
                ModuleInfo, !Subst)
        ;
            NeedExactMatch = yes,
            inst_matches_initial_no_implied_modes(VarInst, Inst,
                Type, ModuleInfo0, ModuleInfo, !Subst)
        )
    ->
        mode_info_set_module_info(ModuleInfo, !ModeInfo)
    ;
        set__singleton_set(WaitingVars, VarId),
        mode_info_error(WaitingVars,
            mode_error_var_has_inst(VarId, VarInst, Inst), !ModeInfo)
    ).

%-----------------------------------------------------------------------------%

modecheck_set_var_inst_list(Vars0, InitialInsts, FinalInsts, ArgOffset,
        Vars, Goals, !ModeInfo) :-
    (
        modecheck_set_var_inst_list_2(Vars0, InitialInsts, FinalInsts,
            ArgOffset, Vars1, no_extra_goals, Goals1, !ModeInfo)
    ->
        Vars = Vars1,
        Goals = Goals1
    ;
        error("modecheck_set_var_inst_list: length mismatch")
    ).

:- pred modecheck_set_var_inst_list_2(list(prog_var)::in, list(mer_inst)::in,
    list(mer_inst)::in, int::in, list(prog_var)::out,
    extra_goals::in, extra_goals::out,
    mode_info::in, mode_info::out) is semidet.

modecheck_set_var_inst_list_2([], [], [], _, [], !ExtraGoals, !ModeInfo).
modecheck_set_var_inst_list_2([Var0 | Vars0], [InitialInst | InitialInsts],
        [FinalInst | FinalInsts], ArgNum0, [Var | Vars],
        !ExtraGoals, !ModeInfo) :-
    ArgNum = ArgNum0 + 1,
    mode_info_set_call_arg_context(ArgNum, !ModeInfo),
    modecheck_set_var_inst_call(Var0, InitialInst, FinalInst,
        Var, !ExtraGoals, !ModeInfo),
    modecheck_set_var_inst_list_2(Vars0, InitialInsts, FinalInsts, ArgNum,
        Vars, !ExtraGoals, !ModeInfo).

:- pred modecheck_set_var_inst_call(prog_var::in, mer_inst::in, mer_inst::in,
    prog_var::out, extra_goals::in, extra_goals::out,
    mode_info::in, mode_info::out) is det.

modecheck_set_var_inst_call(Var0, InitialInst, FinalInst, Var, !ExtraGoals,
        !ModeInfo) :-
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    ( instmap__is_reachable(InstMap0) ->
        % The new inst must be computed by unifying the
        % old inst and the proc's final inst
        instmap__lookup_var(InstMap0, Var0, VarInst0),
        handle_implied_mode(Var0, VarInst0, InitialInst, Var, !ExtraGoals,
            !ModeInfo),
        modecheck_set_var_inst(Var0, FinalInst, no, !ModeInfo),
        ( Var = Var0 ->
            true
        ;
            modecheck_set_var_inst(Var, FinalInst, no, !ModeInfo)
        )
    ;
        Var = Var0
    ).

    % Note that there are two versions of modecheck_set_var_inst,
    % one with arity 7 (suffixed with _call) and one with arity 5.
    % The former is used for predicate calls, where we may need
    % to introduce unifications to handle calls to implied modes.
    %
modecheck_set_var_inst(Var0, FinalInst, MaybeUInst, !ModeInfo) :-
    mode_info_get_parallel_vars(!.ModeInfo, PVars0),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    ( instmap__is_reachable(InstMap0) ->
        % The new inst must be computed by unifying the
        % old inst and the proc's final inst
        instmap__lookup_var(InstMap0, Var0, Inst0),
        mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
        (
            abstractly_unify_inst(dead, Inst0, FinalInst,
                fake_unify, UnifyInst, _Det, ModuleInfo0, ModuleInfo1)
        ->
            ModuleInfo = ModuleInfo1,
            Inst = UnifyInst
        ;
            error("modecheck_set_var_inst: unify_inst failed")
        ),
        mode_info_set_module_info(ModuleInfo, !ModeInfo),
        mode_info_get_var_types(!.ModeInfo, VarTypes),
        map__lookup(VarTypes, Var0, Type),
        (
            % if the top-level inst of the variable is not_reached,
            % then the instmap as a whole must be unreachable
            inst_expand(ModuleInfo, Inst, not_reached)
        ->
            instmap__init_unreachable(InstMap),
            mode_info_set_instmap(InstMap, !ModeInfo)
        ;
            % If we haven't added any information and
            % we haven't bound any part of the var, then
            % the only thing we can have done is lose uniqueness.
            inst_matches_initial(Inst0, Inst, Type, ModuleInfo)
        ->
            instmap__set(Var0, Inst, InstMap0, InstMap),
            mode_info_set_instmap(InstMap, !ModeInfo)
        ;
            % We must have either added some information,
            % lost some uniqueness, or bound part of the var.
            % The call to inst_matches_binding will succeed
            % only if we haven't bound any part of the var.
            \+ inst_matches_binding(Inst, Inst0, Type, ModuleInfo),

            % We've bound part of the var.  If the var was locked,
            % then we need to report an error...
            mode_info_var_is_locked(!.ModeInfo, Var0, Reason0),
            \+ (
                % ...unless the goal is a unification and the
                % var was unified with something no more
                % instantiated than itself.
                % This allows for the case of `any = free', for
                % example. The call to
                % inst_matches_binding, above will fail for the
                % var with mode `any >> any' however, it should
                % be allowed because it has only been unified
                % with a free variable.
                MaybeUInst = yes(UInst),
                inst_is_at_least_as_instantiated(Inst, UInst, Type,
                    ModuleInfo),
                inst_matches_binding_allow_any_any(Inst, Inst0, Type,
                    ModuleInfo)
            )
        ->
            set__singleton_set(WaitingVars, Var0),
            mode_info_error(WaitingVars,
                mode_error_bind_var(Reason0, Var0, Inst0, Inst), !ModeInfo)
        ;
            instmap__set(Var0, Inst, InstMap0, InstMap),
            mode_info_set_instmap(InstMap, !ModeInfo),
            mode_info_get_delay_info(!.ModeInfo, DelayInfo0),
            delay_info__bind_var(Var0, DelayInfo0, DelayInfo),
            mode_info_set_delay_info(DelayInfo, !ModeInfo)
        )
    ;
        true
    ),
    (
        PVars0 = []
    ;
        PVars0 = [NonLocals - Bound0 | PVars1],
        ( set__member(Var0, NonLocals) ->
            set__insert(Bound0, Var0, Bound),
            PVars = [NonLocals - Bound | PVars1]
        ;
            PVars = PVars0
        ),
        mode_info_set_parallel_vars(PVars, !ModeInfo)
    ).

    % If this was a call to an implied mode for that variable, then we need to
    % introduce a fresh variable.
    %
:- pred handle_implied_mode(prog_var::in, mer_inst::in, mer_inst::in,
    prog_var::out, extra_goals::in, extra_goals::out,
    mode_info::in, mode_info::out) is det.

handle_implied_mode(Var0, VarInst0, InitialInst0, Var, !ExtraGoals,
        !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    inst_expand(ModuleInfo0, InitialInst0, InitialInst),
    inst_expand(ModuleInfo0, VarInst0, VarInst1),

    mode_info_get_var_types(!.ModeInfo, VarTypes0),
    map__lookup(VarTypes0, Var0, VarType),
    (
        % If the initial inst of the variable matches_final
        % the initial inst specified in the pred's mode declaration,
        % then it's not a call to an implied mode, it's an exact
        % match with a genuine mode.
        inst_matches_initial_no_implied_modes(VarInst1, InitialInst,
            VarType, ModuleInfo0)
    ->
        Var = Var0
    ;
        % This is the implied mode case.
        % We do not yet handle implied modes for partially
        % instantiated vars, since that would require
        % doing a partially instantiated deep copy, and we
        % don't know how to do that yet.

        InitialInst = any(_),
        inst_is_free(ModuleInfo0, VarInst1)
    ->
        % This is the simple case of implied `any' modes,
        % where the declared mode was `any -> ...'
        % and the argument passed was `free'

        Var = Var0,

        % If the variable's type is not a solver type (in
        % which case inst `any' means the same as inst
        % `ground') then this is an implied mode that we
        % don't yet know how to handle.
        %
        % If the variable's type is a solver type then we need to
        % insert a call to the solver type's initialisation predicate.
        % (To avoid unnecessary complications, we avoid doing this if
        % there are any mode errors recorded at this point.)

        mode_info_get_context(!.ModeInfo, Context),
        mode_info_get_mode_context(!.ModeInfo, ModeContext),
        mode_context_to_unify_context(!.ModeInfo, ModeContext, UnifyContext),
        CallUnifyContext = yes(call_unify_context(Var, var(Var),
            UnifyContext)),
        (
            mode_info_get_errors(!.ModeInfo, ModeErrors),
            ModeErrors = [],
            mode_info_may_initialise_solver_vars(!.ModeInfo),
            type_util__type_is_solver_type(ModuleInfo0, VarType)
        ->
            % Create code to initialize the variable to inst `any',
            % by calling the solver type's initialisation predicate.
            insert_extra_initialisation_call(Var, VarType, InitialInst,
                Context, CallUnifyContext, !ExtraGoals, !ModeInfo)
        ;
            % If the type is a type variable, or isn't a solver type
            % then give up.
            set__singleton_set(WaitingVars, Var0),
            mode_info_error(WaitingVars,
                mode_error_implied_mode(Var0, VarInst0, InitialInst),
                !ModeInfo)
        )
    ;
        inst_is_bound(ModuleInfo0, InitialInst)
    ->
        % This is the case we can't handle.
        Var = Var0,
        set__singleton_set(WaitingVars, Var0),
        mode_info_error(WaitingVars,
            mode_error_implied_mode(Var0, VarInst0, InitialInst), !ModeInfo)
    ;
        % This is the simple case of implied modes,
        % where the declared mode was free -> ...

        % Introduce a new variable.
        mode_info_get_varset(!.ModeInfo, VarSet0),
        varset__new_var(VarSet0, Var, VarSet),
        map__set(VarTypes0, Var, VarType, VarTypes),
        mode_info_set_varset(VarSet, !ModeInfo),
        mode_info_set_var_types(VarTypes, !ModeInfo),

        % Construct the code to do the unification.
        modecheck_unify__create_var_var_unification(Var0, Var,
            VarType, !.ModeInfo, ExtraGoal),

        % Append the goals together in the appropriate order:
        % ExtraGoals0, then NewUnify.
        NewUnifyExtraGoal = extra_goals([], [ExtraGoal]),
        append_extra_goals(!.ExtraGoals, NewUnifyExtraGoal, !:ExtraGoals)
    ).

:- pred insert_extra_initialisation_call(prog_var::in, mer_type::in,
    mer_inst::in, prog_context::in, maybe(call_unify_context)::in,
    extra_goals::in, extra_goals::out, mode_info::in, mode_info::out) is det.

insert_extra_initialisation_call(Var, VarType, Inst, Context, CallUnifyContext,
        !ExtraGoals, !ModeInfo) :-

    construct_initialisation_call(Var, VarType, Inst, Context,
        CallUnifyContext, InitVarGoal, !ModeInfo),
    NewExtraGoal = extra_goals([InitVarGoal], []),
    append_extra_goals(!.ExtraGoals, NewExtraGoal, !:ExtraGoals).

construct_initialisation_call(Var, VarType, Inst, Context,
        MaybeCallUnifyContext, InitVarGoal, !ModeInfo) :-
    (
        type_to_ctor_and_args(VarType, TypeCtor, _TypeArgs),
        PredName = special_pred__special_pred_name(spec_pred_init, TypeCtor),
        (
            TypeCtor = qualified(ModuleName, _TypeName) - _Arity
        ;
            TypeCtor = unqualified(_TypeName) - _Arity,
            mode_info_get_module_info(!.ModeInfo, ModuleInfo),
            hlds_module__module_info_get_name(ModuleInfo, ModuleName)
        ),
        NonLocals = set__make_singleton_set(Var),
        InstmapDeltaAL = [Var - Inst],
        instmap_delta_from_assoc_list(InstmapDeltaAL, InstmapDelta),
        build_call(ModuleName, PredName, [Var], NonLocals,
            InstmapDelta, Context, MaybeCallUnifyContext,
            GoalExpr - GoalInfo, !ModeInfo)
    ->
        InitVarGoal = GoalExpr - GoalInfo
    ;
        error("modes.construct_initialisation_call")
    ).

:- pred build_call(module_name::in, string::in, list(prog_var)::in,
    set(prog_var)::in, instmap_delta::in, prog_context::in,
    maybe(call_unify_context)::in, hlds_goal::out,
    mode_info::in, mode_info::out) is semidet.

build_call(CalleeModuleName, CalleePredName, ArgVars, NonLocals, InstmapDelta,
        Context, CallUnifyContext, Goal, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),

        % Get the pred_info and proc_info for the procedure we are calling.
        %
    module_info_get_predicate_table(ModuleInfo0, PredicateTable),
    list__length(ArgVars, Arity),
    predicate_table_search_pred_m_n_a(PredicateTable, is_fully_qualified,
        CalleeModuleName, CalleePredName, Arity, [CalleePredId]),
    CalleeProcNo = 0, % first mode
    hlds_pred__proc_id_to_int(CalleeProcId, CalleeProcNo),
    module_info_pred_proc_info(ModuleInfo0, CalleePredId, CalleeProcId,
        CalleePredInfo, CalleeProcInfo),

        % Get the relevant information for the procedure we are transforming
        % (i.e., the caller).
        %
    mode_info_get_predid(!.ModeInfo, PredId),
    mode_info_get_procid(!.ModeInfo, ProcId),
    module_info_pred_proc_info(ModuleInfo0, PredId, ProcId, PredInfo0,
        ProcInfo0),

        % Create a poly_info for the caller.
        %
    polymorphism__create_poly_info(ModuleInfo0, PredInfo0, ProcInfo0,
        PolyInfo0),

        % Create a goal_info for the call.
        %
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo1),
    goal_info_set_nonlocals(NonLocals, GoalInfo1, GoalInfo2),
    goal_info_set_instmap_delta(InstmapDelta, GoalInfo2, GoalInfo),

        % Do the transformation for this call goal.
        %
    SymName = qualified(CalleeModuleName, CalleePredName),
    polymorphism__process_new_call(CalleePredInfo, CalleeProcInfo,
        CalleePredId, CalleeProcId, ArgVars, not_builtin, CallUnifyContext,
        SymName, GoalInfo, Goal, PolyInfo0, PolyInfo),

        % Update the information in the predicate table.
        %
    polymorphism__poly_info_extract(PolyInfo, PredInfo0, PredInfo,
        ProcInfo0, ProcInfo, ModuleInfo1),
    module_info_set_pred_proc_info(PredId, ProcId, PredInfo, ProcInfo,
        ModuleInfo1, ModuleInfo),

        % Update the information in the mode_info.
        %
    proc_info_varset(ProcInfo, VarSet),
    proc_info_vartypes(ProcInfo, VarTypes),
    mode_info_set_varset(VarSet, !ModeInfo),
    mode_info_set_var_types(VarTypes, !ModeInfo),
    mode_info_set_module_info(ModuleInfo, !ModeInfo).

%-----------------------------------------------------------------------------%

mode_context_to_unify_context(_, unify(UnifyContext, _), UnifyContext).
mode_context_to_unify_context(_, call(CallId, Arg),
        unify_context(call(CallId, Arg), [])).
mode_context_to_unify_context(_, uninitialized, _) :-
    error("mode_context_to_unify_context: uninitialized context").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Check that the evaluation method is OK for the given mode(s).
    % We also check the mode of main/2 here.
    %
:- pred check_eval_methods(module_info::in, module_info::out,
    io::di, io::uo) is det.

check_eval_methods(!ModuleInfo, !IO) :-
    module_info_predids(!.ModuleInfo, PredIds),
    pred_check_eval_methods(PredIds, !ModuleInfo, !IO).

:- pred pred_check_eval_methods(list(pred_id)::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

pred_check_eval_methods([], !ModuleInfo, !IO).
pred_check_eval_methods([PredId | Rest], !ModuleInfo, !IO) :-
    module_info_preds(!.ModuleInfo, Preds),
    map__lookup(Preds, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    proc_check_eval_methods(ProcIds, PredId, !ModuleInfo, !IO),
    pred_check_eval_methods(Rest, !ModuleInfo, !IO).

:- pred proc_check_eval_methods(list(proc_id)::in, pred_id::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

proc_check_eval_methods([], _, !ModuleInfo, !IO).
proc_check_eval_methods([ProcId | Rest], PredId, !ModuleInfo, !IO) :-
    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo),
    proc_info_eval_method(ProcInfo, EvalMethod),
    proc_info_argmodes(ProcInfo, Modes),
    (
        eval_method_requires_ground_args(EvalMethod) = yes,
        \+ only_fully_in_out_modes(Modes, !.ModuleInfo)
    ->
        report_eval_method_requires_ground_args(ProcInfo, !ModuleInfo, !IO)
    ;
        true
    ),
    (
        eval_method_destroys_uniqueness(EvalMethod) = yes,
        \+ only_nonunique_modes(Modes, !.ModuleInfo)
    ->
        report_eval_method_destroys_uniqueness(ProcInfo, !ModuleInfo, !IO)
    ;
        true
    ),
    (
        pred_info_name(PredInfo) = "main",
        pred_info_orig_arity(PredInfo) = 2,
        pred_info_is_exported(PredInfo),
        \+ check_mode_of_main(Modes, !.ModuleInfo)
    ->
        report_wrong_mode_for_main(ProcInfo, !ModuleInfo, !IO)
    ;
        true
    ),
    proc_check_eval_methods(Rest, PredId, !ModuleInfo, !IO).

:- pred only_fully_in_out_modes(list(mer_mode)::in, module_info::in)
    is semidet.

only_fully_in_out_modes([], _).
only_fully_in_out_modes([Mode | Rest], ModuleInfo) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    (
        inst_is_ground(ModuleInfo, InitialInst)
    ;
        inst_is_free(ModuleInfo, InitialInst),
        (
            inst_is_free(ModuleInfo, FinalInst)
        ;
            inst_is_ground(ModuleInfo, FinalInst)
        )
    ),
    only_fully_in_out_modes(Rest, ModuleInfo).

:- pred only_nonunique_modes(list(mer_mode)::in, module_info::in) is semidet.

only_nonunique_modes([], _).
only_nonunique_modes([Mode | Rest], ModuleInfo) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    inst_is_not_partly_unique(ModuleInfo, InitialInst),
    inst_is_not_partly_unique(ModuleInfo, FinalInst),
    only_nonunique_modes(Rest, ModuleInfo).

:- pred check_mode_of_main(list(mer_mode)::in, module_info::in) is semidet.

check_mode_of_main([Di, Uo], ModuleInfo) :-
    mode_get_insts(ModuleInfo, Di, DiInitialInst, DiFinalInst),
    mode_get_insts(ModuleInfo, Uo, UoInitialInst, UoFinalInst),
    %
    % Note that we hard-code these tests,
    % rather than using `inst_is_free', `inst_is_unique', etc.,
    % since for main/2 we're looking for an exact match
    % (modulo inst synonyms) with what the language reference
    % manual specifies, rather than looking for a particular
    % abstract property.
    %
    inst_expand(ModuleInfo, DiInitialInst, ground(unique, none)),
    inst_expand(ModuleInfo, DiFinalInst, ground(clobbered, none)),
    inst_expand(ModuleInfo, UoInitialInst, Free),
    ( Free = free ; Free = free(_Type) ),
    inst_expand(ModuleInfo, UoFinalInst, ground(unique, none)).

:- pred report_eval_method_requires_ground_args(proc_info::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

report_eval_method_requires_ground_args(ProcInfo, !ModuleInfo, !IO) :-
    proc_info_eval_method(ProcInfo, EvalMethod),
    proc_info_context(ProcInfo, Context),
    EvalMethodS = eval_method_to_one_string(EvalMethod),
    globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    Pieces1 = [words("Sorry, not implemented:"),
        fixed("`pragma " ++ EvalMethodS ++ "'"),
        words("declaration not allowed for procedure"),
        words("with partially instantiated modes.")],
    (
        VerboseErrors = yes,
        Pieces2 = [words("Tabling of predicates/functions"),
            words("with partially instantiated modes"),
            words("is not currently implemented.")]
    ;
        VerboseErrors = no,
        globals.io_set_extra_error_info(yes, !IO),
        Pieces2 = []
    ),
    write_error_pieces(Context, 0, Pieces1 ++ Pieces2, !IO),
    module_info_incr_errors(!ModuleInfo).

:- pred report_eval_method_destroys_uniqueness(proc_info::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

report_eval_method_destroys_uniqueness(ProcInfo, !ModuleInfo, !IO) :-
    proc_info_eval_method(ProcInfo, EvalMethod),
    proc_info_context(ProcInfo, Context),
    EvalMethodS = eval_method_to_one_string(EvalMethod),
    globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    Pieces1 = [words("Error:"),
        fixed("`pragma " ++ EvalMethodS ++ "'"),
        words("declaration not allowed for procedure"),
        words("with unique modes.")],
    (
        VerboseErrors = yes,
        Pieces2 = [words("Tabling of predicates/functions with unique modes"),
            words("is not allowed as this would lead to a copying"),
            words("of the unique arguments which would result"),
            words("in them no longer being unique.")]
    ;
        VerboseErrors = no,
        Pieces2 = []
    ),
    write_error_pieces(Context, 0, Pieces1 ++ Pieces2, !IO),
    module_info_incr_errors(!ModuleInfo).

:- pred report_wrong_mode_for_main(proc_info::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

report_wrong_mode_for_main(ProcInfo, !ModuleInfo, !IO) :-
    proc_info_context(ProcInfo, Context),
    Pieces = [words("Error: main/2 must have mode `(di, uo)'.")],
    write_error_pieces(Context, 0, Pieces, !IO),
    module_info_incr_errors(!ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Given a list of variables, and a list of livenesses,
    % select the live variables.
    %
get_live_vars([_ | _], [], _) :-
    error("get_live_vars: length mismatch").
get_live_vars([], [_ | _], _) :-
    error("get_live_vars: length mismatch").
get_live_vars([], [], []).
get_live_vars([Var | Vars], [IsLive | IsLives], LiveVars) :-
    (
        IsLive = live,
        LiveVars = [Var | LiveVars0]
    ;
        IsLive = dead,
        LiveVars = LiveVars0
    ),
    get_live_vars(Vars, IsLives, LiveVars0).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % XXX - At the moment we don't check for circular modes or insts.
    % (If they aren't used, the compiler will probably not detect the error;
    % if they are, it will probably go into an infinite loop).
    %
:- pred check_circular_modes(module_info::in, module_info::out,
    io::di, io::uo) is det.

check_circular_modes(!Module, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
