%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: modes.m.
% Main author: fjh.
%
% This module contains the top level of the code for mode checking and mode
% inference.
%
% Basically what this module does is to traverse the HLDS, performing
% mode-checking or mode inference on each predicate. For each procedure,
% it will reorder the procedure body if necessary, annotate each subgoal
% with its mode, and check that the procedure body is mode-correct,
% This pass also determines whether or not unifications can fail,
% and converts unifications with higher-order predicate terms into
% unifications with lambda expressions.
%
% The input to this pass must be type-correct and in superhomogeneous form.
%
% This pass does not check that `unique' modes are not used in contexts
% which might require backtracking - that is done by unique_modes.m.
% N.B. Changes here may also require changes to unique_modes.m!
%
% IMPLEMENTATION DOCUMENTATION
% How does it all work? Well, mode checking/inference is basically
% a process of abstract interpretation. To perform this abstract interpretation
% on a procedure body, we need to know the initial insts of the arguments;
% then we can abstractly interpret the goal to compute the final insts.
% For mode checking, we then just compare the inferred final insts
% with the declared final insts, and that is about all there is to it.
%
% For mode inference, it is a little bit trickier. When we see a call to
% a predicate for which the modes were not declared, we first check whether
% the call matches any of the modes we have already inferred. If not,
% we create a new mode for the predicate, with the initial insts
% set to a "normalised" version of the insts of the call arguments.
% For a first approximation, we set the final insts to `not_reached'.
% What this means is that we don't yet have any information about
% what the final insts will be. We then keep iterating mode inference passes
% until we reach a fixpoint.
%
% To mode-analyse a procedure:
%   1.  Initialize the insts of the head variables.
%   2.  Mode-analyse the goal.
%   3.  a.  If we are doing mode-checking:
%           Check that the final insts of the head variables
%           matches that specified in the mode declaration
%       b.  If we are doing mode-inference:
%           Normalise the final insts of the head variables,
%           record the newly inferred normalised final insts
%           in the proc_info, and check whether they changed
%           (so that we know when we have reached the fixpoint).
%
% How to mode-analyse a goal is documented at the top of modecheck_goal.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.modes.
:- interface.

:- import_module check_hlds.mode_info.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % modecheck_module(ProgressStream, !HLDS, SafeToContinue, Specs):
    %
    % Perform mode inference and checking for a whole module.
    %
    % SafeToContinue = unsafe_to_continue means that mode inference
    % was halted prematurely due to an error, and that we should therefore
    % not perform determinism-checking, because we might get internal errors.
    %
    % The outputs are in a tuple because our caller wants to be able to
    % benchmark mode analysis, and the benchmark predicates require exactly
    % one output argument.
    %
:- pred modecheck_module(io.text_output_stream::in,
    module_info::in, module_info::out,
    maybe_safe_to_continue::out, list(error_spec)::out) is det.

    % Mode-check or unique-mode-check the code of all the predicates
    % in a module.
    %
:- pred check_pred_modes(io.text_output_stream::in, how_to_check_goal::in,
    may_change_called_proc::in, module_info::in, module_info::out,
    maybe_safe_to_continue::out, list(error_spec)::out) is det.

    % Mode-check the code for the given predicate in a given mode.
    % Returns the number of errs found and a bool `Changed'
    % which is true iff another pass of fixpoint analysis may be needed.
    %
:- pred modecheck_proc(pred_id::in, proc_id::in,
    module_info::in, module_info::out, bool::out,
    list(error_spec)::out) is det.

    % Mode-check or unique-mode-check the code for the given predicate
    % in a given mode.
    % Returns the number of errs found and a bool `Changed'
    % which is true iff another pass of fixpoint analysis may be needed.
    %
:- pred modecheck_proc_general(how_to_check_goal::in,
    may_change_called_proc::in, pred_id::in, proc_id::in,
    module_info::in, module_info::out, bool::out,
    list(error_spec)::out) is det.

    % Check that the actual final insts of the head vars of a lambda goal
    % matches their expected final insts.
    %
:- pred modecheck_lambda_final_insts(list(prog_var)::in, list(mer_inst)::in,
    mode_info::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.clause_to_proc.
:- import_module check_hlds.cse_detection.
:- import_module check_hlds.delay_partial_inst.
:- import_module check_hlds.det_analysis.
:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.introduce_exists_casts.
:- import_module check_hlds.mode_debug.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.modecheck_goal.
:- import_module check_hlds.modecheck_util.
:- import_module check_hlds.proc_requests.
:- import_module check_hlds.switch_detection.
:- import_module check_hlds.type_util.
:- import_module check_hlds.unique_modes.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module parse_tree.write_error_spec.

:- import_module assoc_list.
:- import_module bag.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module queue.
:- import_module require.
:- import_module set_tree234.
:- import_module string.
:- import_module term_context.
:- import_module varset.

%-----------------------------------------------------------------------------%

modecheck_module(ProgressStream, !ModuleInfo, SafeToContinue, Specs) :-
    check_pred_modes(ProgressStream, check_modes, may_change_called_proc,
        !ModuleInfo, SafeToContinue, Specs).

%-----------------------------------------------------------------------------%

check_pred_modes(ProgressStream, WhatToCheck, MayChangeCalledProc, !ModuleInfo,
        SafeToContinue, !:Specs) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_int_option(Globals, mode_inference_iteration_limit,
        MaxIterations),
    modecheck_to_fixpoint(ProgressStream, PredIds, MaxIterations, WhatToCheck,
        MayChangeCalledProc, !ModuleInfo, SafeToContinue0, !:Specs),
    (
        WhatToCheck = check_unique_modes,
        report_mode_inference_messages_for_preds(!.ModuleInfo,
            include_detism_on_modes, PredIds, !Specs),
        module_check_eval_methods_and_main(!.ModuleInfo, !Specs),
        SafeToContinue = SafeToContinue0
    ;
        WhatToCheck = check_modes,
        (
            SafeToContinue0 = unsafe_to_continue,
            report_mode_inference_messages_for_preds(!.ModuleInfo,
                do_not_include_detism_on_modes, PredIds, !Specs),
            SafeToContinue = unsafe_to_continue
        ;
            SafeToContinue0 = safe_to_continue,
            globals.lookup_bool_option(Globals, delay_partial_instantiations,
                DelayPartialInstantiations),
            (
                DelayPartialInstantiations = yes,
                BeforeDPISafeToContinue = SafeToContinue0,
                BeforeDPISpecs = !.Specs,
                BeforeDPIModuleInfo = !.ModuleInfo,
                delay_partial_inst_preds(ProgressStream, PredIds, ChangedPreds,
                    !ModuleInfo),
                % --delay-partial-instantiations requires mode checking to be
                % run again.
                modecheck_to_fixpoint(ProgressStream, ChangedPreds,
                    MaxIterations, WhatToCheck, MayChangeCalledProc,
                    !.ModuleInfo, AfterDPIModuleInfo,
                    AfterDPISafeToContinue, AfterDPISpecs),
                MaybeBeforeDPISeverity =
                    worst_severity_in_specs(Globals, BeforeDPISpecs),
                MaybeAfterDPISeverity =
                    worst_severity_in_specs(Globals, AfterDPISpecs),
                % BeforeDPISpecs and AfterDPISpecs ought to be the same,
                % but in its current form, delay_partial_inst can also
                % INTRODUCE mode errors. This can happen in situations where
                % a predicate (such as the original version of the
                % get_feedback_data predicate in feedback.m in the
                % mdbcomp directory) REQUIRES being passed a partially
                % instantiated term.
                %
                % Ideally, we would apply the delay_partial_inst transformation
                % to the predicates where it does not cause problems, and
                % undo it in the predicates where it does. Unfortunately,
                % in the presence of mode inference, separating the two
                % categories is not easy, so if delay_partial_inst causes ANY
                % new problems, from ANY predicate, we undo ALL its updates.
                (
                    MaybeBeforeDPISeverity = no,
                    MaybeAfterDPISeverity = no,
                    % There is no difference; BeforeDPISpecs and AfterDPISpecs
                    % are equivalent. Pick one.
                    !:Specs = AfterDPISpecs,
                    !:ModuleInfo = AfterDPIModuleInfo,
                    SafeToContinue = AfterDPISafeToContinue
                ;
                    MaybeBeforeDPISeverity = no,
                    MaybeAfterDPISeverity = yes(_),
                    % delay_partial_inst introduced a problem. Undo its effect.
                    !:Specs = BeforeDPISpecs,
                    !:ModuleInfo = BeforeDPIModuleInfo,
                    SafeToContinue = BeforeDPISafeToContinue
                ;
                    MaybeBeforeDPISeverity = yes(_),
                    MaybeAfterDPISeverity = no,
                    % delay_partial_inst fixed a problem. Keep its effect.
                    !:Specs = AfterDPISpecs,
                    !:ModuleInfo = AfterDPIModuleInfo,
                    SafeToContinue = AfterDPISafeToContinue
                ;
                    MaybeBeforeDPISeverity = yes(BeforeDPISeverity),
                    MaybeAfterDPISeverity = yes(AfterDPISeverity),
                    WorstSeverity =
                        worst_severity(BeforeDPISeverity, AfterDPISeverity),
                    % We do not have a COUNT of the number of problems
                    % in either BeforeDPISpecs or AfterDPISpecs, so we
                    % cannot choose the one that reports fewer problems.
                    % However, to a large extent that does not matter,
                    % because what we actually want to minimize is the
                    % total COMPLEXITY of the mode problems we report
                    % to the user, and a single complex mode error can be
                    % as hard to understand as several simpler ones.
                    %
                    % If the delay_partial_inst transformation does not fix
                    % all the mode errors in the module (without necessarily
                    % fixing all the warnings), then we prefer to go with
                    % the untransformed version of the errors, since these
                    % should be easier to relate to the code as written.
                    ( if AfterDPISeverity = WorstSeverity then
                        !:Specs = BeforeDPISpecs,
                        !:ModuleInfo = BeforeDPIModuleInfo,
                        SafeToContinue = BeforeDPISafeToContinue
                    else
                        !:Specs = AfterDPISpecs,
                        !:ModuleInfo = AfterDPIModuleInfo,
                        SafeToContinue = AfterDPISafeToContinue
                    )
                )
            ;
                DelayPartialInstantiations = no,
                SafeToContinue = safe_to_continue
            )
        )
    ).

    % Iterate over the list of pred_ids in a module.
    % XXX Document this predicate rather better than that.
    % 
    % XXX I, zs, see three nontrivial problems with our current approach
    % to fixpoint iteration in the presence of predicates that have
    % no mode declarations.
    %
    % The first and most serious problem is that I see no attempt
    % to catch and diagnose a scenario that could lead to the compiler
    % generating incomplete code. This scenario goes like this:
    %
    % (a) Predicate A calls predicate B. Predicate B has no mode declaration,
    %     so we queue a request to add a mode to it, with the initial insts
    %     of its arguments being the insts of the corresponding arguments
    %     at the time of the call.
    %
    % (b) Mode inference of predicate B finds that there is no way
    %     to schedule the goals of the body of B with those initial insts.
    %     We record a mode error for this mode of B, making that mode of B
    %     invalid in the sense of proc_info_is_valid_mode. However:
    %
    %     (b1) Since B has marker_infer_modes, we don't add the B's mode error
    %          to the list of mode errors we intend to print, because a later
    %          iteration in the fixpoint could cure the error (e.g. by
    %          inferring a new mode for a predicate C that B calls).
    %
    %     (b2) The proc_id recorded for the call from A to B is *not*
    %          invalid_proc_id, but a *real* proc_id, that just happens
    %          to refer to an proc_info that is not valid. This means that
    %          we don't get a more error from A either.
    %
    %     While this situation would be extremely likely to change in the
    %     next iteration, I see no correctness argument that would guarantee
    %     would guarantee this. We could thus arrive at a fixpoint in which 
    %     a valid procedure in A would contain a call to an invalid procedure
    %     in B. Since we generate target language code for valid procedures
    %     but obviously not for invalid procedures, we would be generating
    %     a call to an undefined callee.
    %
    %     (Note that modecheck_call.m *does* ensure that if a call has
    %     invalid_proc_id as its proc_id, then we *will* generate an error
    %     when we are analysing the caller. That is distinct from the problem
    %     above, in which the callee procedure is invalid but its proc_id
    %     is *not* invalid_proc_id.)
    %
    % The second problem is that proc_infos created during mode inference
    % stick around (as proc_infos for which proc_info_is_valid_mode fails)
    % even if there are no calls to them from proc_infos that *are* valid.
    % For example, during mode inference of the mode_inference_reorder test
    % case in tests/general, many of the predicates that have no mode
    % declarations in the source code get two procedures created, of which
    % one ends up valid and one ends up invalid. As it happens, there are
    % dangling calls (i.e. the first problem above does not arise), because
    % all the calls to the invalid procedures are from *other* invalid
    % procedures. Having these invalid procedures hanging around for the
    % rest of the compiler invocation is pure overhead, since they
    % (a) are still in their predicates' proc tables, so their  memory
    % can't be garbage collected, and (b) all traversals of their predicates'
    % valid procedures have to step over them. And yet we can't just delete
    % all procedures that end up invalid from their predicates' proc table
    % at the end of mode analysis, since there *may* be references to them
    % from valid procedures (see the first problem above), and in that case
    % such deletion would probably lead to a compiler abort (e.g. when
    % the compiler wanted to look up some info about a deleted callee),
    % which is an even worse failure more than generating incomplete code.
    %
    % The third problem is that in the presence of intermodule optimization,
    % the predicate table may contains hundreds of predicates whose bodies
    % the compiler has access to, and whose bodies it therefore must modecheck.
    % The vast majority of these, the ones from .opt files, are known to be
    % mode correct, since if they weren't, their .opt file wouldn't have been
    % created in the first place. In the usual case, many if not most of
    % those .opt files will be from modules that do not import the module
    % currently being compiled, and whose contents thus cannot be affected
    % by any new modes we infer to the predicates in the current module.
    % Reanalysing such predicates on every iteration is also a waste of time,
    % *especially* given that it is also likely that many of those predicates
    % will end up not being called from anywhere at all during this compiler
    % invocation.
    %
:- pred modecheck_to_fixpoint(io.text_output_stream::in, list(pred_id)::in,
    int::in, how_to_check_goal::in, may_change_called_proc::in,
    module_info::in, module_info::out, maybe_safe_to_continue::out,
    list(error_spec)::out) is det.

modecheck_to_fixpoint(ProgressStream, PredIds, NumIterationsLeft, WhatToCheck,
        MayChangeCalledProc, !ModuleInfo, SafeToContinue, !:Specs) :-
    % Save the old procedure bodies, so that we can restore any procedure body 
    % for the next pass if necessary.
    module_info_get_pred_id_table(!.ModuleInfo, OldPredIdTable0),

    % Analyze every procedure whose "CanProcess" flag is `can_process_now'.
    list.foldl3(
        maybe_modecheck_pred(ProgressStream, WhatToCheck, MayChangeCalledProc),
        PredIds, !ModuleInfo, no, Changed1, [], !:Specs),

    % Analyze the procedures whose "CanProcess" flag was cannot_process_yet;
    % those procedures were inserted into the unify requests queue.
    modecheck_queued_procs(WhatToCheck, OldPredIdTable0, OldPredIdTable,
        !ModuleInfo, Changed1, Changed, !Specs),

    module_info_get_globals(!.ModuleInfo, Globals),
    ErrorsSoFar = contains_errors(Globals, !.Specs),
    (
        Changed = no,
        % Stop if we have reached a fixpoint.
        SafeToContinue = safe_to_continue
    ;
        Changed = yes,
        (
            ErrorsSoFar = yes,
            % Stop if we have found any errors.
            SafeToContinue = unsafe_to_continue
        ;
            ErrorsSoFar = no,
            ( if NumIterationsLeft =< 1 then
                % Stop if we have exceeded the iteration limit.
                MaxIterSpec = report_max_iterations_exceeded(!.ModuleInfo),
                !:Specs = [MaxIterSpec | !.Specs],
                SafeToContinue = unsafe_to_continue
            else
                % Otherwise, continue iterating.
                globals.lookup_bool_option(Globals, debug_modes, DebugModes),
                (
                    DebugModes = yes,
                    report_mode_inference_messages_for_preds(!.ModuleInfo,
                        do_not_include_detism_on_modes, PredIds,
                        [], InferenceSpecs),
                    trace [io(!IO)] (
                        module_info_get_name(!.ModuleInfo, ModuleName),
                        get_debug_output_stream(Globals, ModuleName,
                            DebugStream, !IO),
                        io.write_string(DebugStream,
                            "Inferences by current iteration:\n", !IO),
                        write_error_specs(DebugStream, Globals,
                            InferenceSpecs, !IO),
                        io.write_string(DebugStream,
                            "End of inferences.\n", !IO)
                    )
                ;
                    DebugModes = no
                ),

                % Mode analysis may have modified the procedure bodies,
                % since it does some optimizations such as deleting unreachable
                % code. But since we have not reached a fixpoint yet, the mode
                % information is not yet correct, and so those optimizations
                % will have been done based on incomplete information, and
                % therefore they may produce incorrect results. We thus
                % have to restore the old procedure bodies.

                (
                    WhatToCheck = check_modes,
                    % Restore the proc_info goals from the clauses in the
                    % pred_info. Reintroduce exists_cast goals, since these
                    % do not appear in the clauses.
                    copy_clauses_to_nonmethod_procs_for_preds_in_module_info(
                        PredIds, !ModuleInfo),
                    introduce_exists_casts(PredIds, !ModuleInfo)
                ;
                    WhatToCheck = check_unique_modes,
                    % Restore the proc_info goals from the
                    % proc_infos in the old module_info.
                    % XXX Why don't we do the same for check_modes?
                    copy_pred_bodies(OldPredIdTable, PredIds, !ModuleInfo)
                ),

                modecheck_to_fixpoint(ProgressStream, PredIds,
                    NumIterationsLeft - 1, WhatToCheck, MayChangeCalledProc,
                    !ModuleInfo, SafeToContinue, !:Specs)
            )
        )
    ).

:- func report_max_iterations_exceeded(module_info) = error_spec.

report_max_iterations_exceeded(ModuleInfo) = Spec :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_int_option(Globals, mode_inference_iteration_limit,
        MaxIterations),
    Pieces = [words("Mode analysis iteration limit exceeded."), nl,
        words("You may need to declare the modes explicitly"),
        words("or use the"), quote("--mode-inference-iteration-limit"),
        words("option to increase the limit."),
        words("(The current limit is"), int_fixed(MaxIterations),
        words("iterations.)"), nl],
    Spec = simplest_no_context_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Pieces).

    % copy_pred_bodies(OldPredIdTable, ProcId, !ModuleInfo):
    %
    % Copy the procedure bodies for all procedures of the specified PredIds
    % from OldPredIdTable into !ModuleInfo.
    %
:- pred copy_pred_bodies(pred_id_table::in, list(pred_id)::in,
    module_info::in, module_info::out) is det.

copy_pred_bodies(OldPredIdTable, PredIds, !ModuleInfo) :-
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    list.foldl(copy_pred_body(OldPredIdTable), PredIds,
        PredIdTable0, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

    % copy_pred_body(OldPredIdTable, ProcId, PredIdTable0, PredIdTable):
    %
    % Copy the procedure bodies for all procedures of the specified PredId
    % from OldPredIdTable into PredIdTable0, giving PredIdTable.
    %
:- pred copy_pred_body(pred_id_table::in, pred_id::in,
    pred_id_table::in, pred_id_table::out) is det.

copy_pred_body(OldPredIdTable, PredId, PredIdTable0, PredIdTable) :-
    map.lookup(PredIdTable0, PredId, PredInfo0),
    ( if
        % Don't copy type class methods, because their proc_infos are generated
        % already mode-correct, and because copying from the clauses_info
        % doesn't work for them.
        pred_info_get_markers(PredInfo0, Markers),
        check_marker(Markers, marker_class_method)
    then
        PredIdTable = PredIdTable0
    else
        pred_info_get_proc_table(PredInfo0, ProcTable0),
        map.lookup(OldPredIdTable, PredId, OldPredInfo),
        pred_info_get_proc_table(OldPredInfo, OldProcTable),
        map.keys(OldProcTable, OldProcIds),
        list.foldl(copy_proc_body(OldProcTable), OldProcIds,
            ProcTable0, ProcTable),
        pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
        map.det_update(PredId, PredInfo, PredIdTable0, PredIdTable)
    ).

    % copy_proc_body(OldProcTable, ProcId, !ProcTable):
    %
    % Copy the body of the specified ProcId from OldProcTable into !ProcTable.
    %
:- pred copy_proc_body(proc_table::in, proc_id::in,
    proc_table::in, proc_table::out) is det.

copy_proc_body(OldProcTable, ProcId, !ProcTable) :-
    map.lookup(OldProcTable, ProcId, OldProcInfo),
    proc_info_get_goal(OldProcInfo, OldProcBody),
    map.lookup(!.ProcTable, ProcId, ProcInfo0),
    proc_info_set_goal(OldProcBody, ProcInfo0, ProcInfo),
    map.det_update(ProcId, ProcInfo, !ProcTable).

:- func should_modecheck_pred(pred_info) = bool.

should_modecheck_pred(PredInfo) = ShouldModeCheck :-
    ( if
        (
            % Don't modecheck imported predicates.
            ( pred_info_is_imported(PredInfo)
            ; pred_info_is_pseudo_imported(PredInfo)
            )
        ;
            % Don't modecheck class methods, because they are generated
            % already mode-correct and with correct instmap deltas.
            pred_info_get_markers(PredInfo, PredMarkers),
            check_marker(PredMarkers, marker_class_method)
        )
    then
        ShouldModeCheck = no
    else
        ShouldModeCheck = yes
    ).

:- pred maybe_modecheck_pred(io.text_output_stream::in, how_to_check_goal::in,
    may_change_called_proc::in, pred_id::in, module_info::in, module_info::out,
    bool::in, bool::out, list(error_spec)::in, list(error_spec)::out) is det.

maybe_modecheck_pred(ProgressStream, WhatToCheck, MayChangeCalledProc, PredId,
        !ModuleInfo, !Changed, !Specs) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    ShouldModeCheck = should_modecheck_pred(PredInfo0),
    (
        ShouldModeCheck = no
    ;
        ShouldModeCheck = yes,
        trace [io(!IO)] (
            maybe_write_modes_progress_message(ProgressStream, !.ModuleInfo,
                WhatToCheck, PredId, PredInfo0, !IO)
        ),
        do_modecheck_pred(PredId, PredInfo0, WhatToCheck,
            MayChangeCalledProc, !ModuleInfo, !Changed,
            ThisPredDeclSpecs, ThisPredProcSpecs),
        !:Specs = ThisPredDeclSpecs ++ ThisPredProcSpecs ++ !.Specs,
        % The lack of a mode declaration for the predicate is not a reason
        % to stop mode inference on the predicate. That is why we check for
        % errors only in ThisPredProcSpecs, not in ThisPredDeclSpecs.
        module_info_get_globals(!.ModuleInfo, Globals),
        ContainsError = contains_errors(Globals, ThisPredProcSpecs),
        (
            ContainsError = yes,
            module_info_make_pred_id_invalid(PredId, !ModuleInfo)
        ;
            ContainsError = no
        ),

        globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
        trace [io(!IO)] (
            maybe_report_stats(ProgressStream, Statistics, !IO)
        )
    ).

:- pred maybe_write_modes_progress_message(io.text_output_stream::in,
    module_info::in, how_to_check_goal::in, pred_id::in, pred_info::in,
    io::di, io::uo) is det.

maybe_write_modes_progress_message(ProgressStream, ModuleInfo, WhatToCheck,
        PredId, PredInfo, !IO) :-
    pred_info_get_markers(PredInfo, Markers),
    ( if check_marker(Markers, marker_infer_modes) then
        (
            WhatToCheck = check_modes,
            Msg = "Mode-analysing"
        ;
            WhatToCheck = check_unique_modes,
            Msg = "Unique-mode-analysing"
        )
    else
        (
            WhatToCheck = check_modes,
            Msg = "Mode-checking"
        ;
            WhatToCheck = check_unique_modes,
            Msg = "Unique-mode-checking"
        )
    ),
    maybe_write_pred_progress_message(ProgressStream, ModuleInfo, Msg,
        PredId, !IO).

%-----------------------------------------------------------------------------%

:- pred do_modecheck_pred(pred_id::in, pred_info::in,
    how_to_check_goal::in, may_change_called_proc::in,
    module_info::in, module_info::out, bool::in, bool::out,
    list(error_spec)::out, list(error_spec)::out) is det.

do_modecheck_pred(PredId, PredInfo0, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !Changed, DeclSpecs, ProcSpecs) :-
    (
        WhatToCheck = check_modes,
        pred_info_get_proc_table(PredInfo0, ProcTable),
        ( if
            some [ProcInfo] (
                map.member(ProcTable, _ProcId, ProcInfo),
                proc_info_get_maybe_declared_argmodes(ProcInfo, yes(_))
            )
        then
            % There was at least one declared mode for this procedure.
            DeclSpecs = []
        else
            % There were no declared modes for this procedure.
            DeclSpecs = maybe_report_error_no_modes(!.ModuleInfo, PredId,
                PredInfo0)
        )
    ;
        WhatToCheck = check_unique_modes,
        DeclSpecs = []
    ),
    % Note that we use pred_info_valid_procids, rather than
    % pred_info_all_procids here, which means that we don't process modes
    % that have already been inferred as invalid.
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    ProcIds = pred_info_valid_procids(PredInfo0),
    maybe_modecheck_procs(WhatToCheck, MayChangeCalledProc, PredId, ProcTable0,
        ProcIds, !ModuleInfo, !Changed, init_error_spec_accumulator, SpecsAcc),
    ProcSpecs = error_spec_accumulator_to_list(SpecsAcc).

    % Return an error for a predicate with no mode declarations
    % unless mode inference is enabled and the predicate is local.
    %
:- func maybe_report_error_no_modes(module_info, pred_id, pred_info)
    = list(error_spec).

maybe_report_error_no_modes(ModuleInfo, PredId, PredInfo) = Specs :-
    pred_info_get_status(PredInfo, PredStatus),
    % XXX STATUS
    ( if PredStatus = pred_status(status_local) then
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, infer_modes, InferModesOpt),
        (
            InferModesOpt = yes,
            Specs = []
        ;
            InferModesOpt = no,
            pred_info_get_markers(PredInfo, Markers),
            ( if check_marker(Markers, marker_no_pred_decl) then
                % Generate an error_spec that prints nothing.
                % While we don't want the user to see the error message,
                % we need the severity_error to stop the compiler
                % from proceeding to process this predicate further.
                % For example, to determinism analysis, where it could
                % generate misleading errors about the determinism declaration
                % (added implicitly by the compiler) being wrong.
                % There is no risk of the compilation failing without
                % *any* error indication, since we generated an error message
                % when we added the marker.
                Msgs = []
            else
                PredDesc = describe_one_pred_name(ModuleInfo,
                    should_not_module_qualify, PredId),
                MainPieces = [words("Error: no mode declaration for")] ++
                    PredDesc ++ [suffix("."), nl],
                VerbosePieces =
                    [words("(Use"), quote("--infer-modes"),
                    words("to enable mode inference.)"), nl],
                Msgs =
                    [simple_msg(Context,
                        [always(MainPieces),
                        verbose_only(verbose_once, VerbosePieces)])]
            ),
            pred_info_get_context(PredInfo, Context),
            Spec = error_spec($pred, severity_error,
                phase_mode_check(report_in_any_mode), Msgs),
            Specs = [Spec]
        )
    else
        pred_info_get_context(PredInfo, Context),
        Pieces = [words("Error: no mode declaration for exported")] ++
            describe_one_pred_name(ModuleInfo, should_module_qualify, PredId)
            ++ [suffix("."), nl],
        Spec = simplest_spec($pred, severity_error,
            phase_mode_check(report_in_any_mode), Context, Pieces),
        Specs = [Spec]
    ).

%-----------------------------------------------------------------------------%

    % Iterate over the list of modes for a predicate.
    %
:- pred maybe_modecheck_procs(how_to_check_goal::in,
    may_change_called_proc::in, pred_id::in, proc_table::in, list(proc_id)::in,
    module_info::in, module_info::out, bool::in, bool::out,
    error_spec_accumulator::in, error_spec_accumulator::out) is det.

maybe_modecheck_procs(_, _, _, _, [], !ModuleInfo, !Changed, !Specs).
maybe_modecheck_procs(WhatToCheck, MayChangeCalledProc, PredId, ProcTable0,
        [ProcId | ProcIds], !ModuleInfo, !Changed, !SpecsAcc) :-
    map.lookup(ProcTable0, ProcId, ProcInfo0),
    maybe_modecheck_proc(WhatToCheck, MayChangeCalledProc,
        PredId, ProcId, ProcInfo0, !ModuleInfo, !Changed, ProcSpecs),
    accumulate_error_specs_for_proc(ProcSpecs, !SpecsAcc),
    maybe_modecheck_procs(WhatToCheck, MayChangeCalledProc, PredId, ProcTable0,
        ProcIds, !ModuleInfo, !Changed, !SpecsAcc).

%-----------------------------------------------------------------------------%

modecheck_proc(PredId, ProcId, !ModuleInfo, Changed, Specs) :-
    modecheck_proc_general(check_modes, may_change_called_proc,
        PredId, ProcId, !ModuleInfo, Changed, Specs).

modecheck_proc_general(WhatToCheck, MayChangeCalledProc, PredId, ProcId,
        !ModuleInfo, Changed, Specs) :-
    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        _PredInfo, ProcInfo),
    maybe_modecheck_proc(WhatToCheck, MayChangeCalledProc,
        PredId, ProcId, ProcInfo, !ModuleInfo, no, Changed, Specs).

:- pred maybe_modecheck_proc(how_to_check_goal::in, may_change_called_proc::in,
    pred_id::in, proc_id::in, proc_info::in,
    module_info::in, module_info::out, bool::in, bool::out,
    list(error_spec)::out) is det.

maybe_modecheck_proc(WhatToCheck, MayChangeCalledProc,
        PredId, ProcId, ProcInfo0, !ModuleInfo, !Changed, Specs) :-
    proc_info_get_can_process(ProcInfo0, CanProcess),
    (
        CanProcess = cannot_process_yet,
        Specs = []
    ;
        CanProcess = can_process_now,
        definitely_modecheck_proc(WhatToCheck, MayChangeCalledProc,
            PredId, ProcId, ProcInfo0, !ModuleInfo, !Changed, Specs)
    ).

:- pred definitely_modecheck_proc(how_to_check_goal::in,
    may_change_called_proc::in, pred_id::in, proc_id::in, proc_info::in,
    module_info::in, module_info::out, bool::in, bool::out,
    list(error_spec)::out) is det.

definitely_modecheck_proc(WhatToCheck, MayChangeCalledProc,
        PredId, ProcId, ProcInfo0, !ModuleInfo, !Changed, Specs) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    do_modecheck_proc(WhatToCheck, MayChangeCalledProc,
        PredId, PredInfo0, ProcId, ProcInfo0, ProcInfo, ClausesInfo,
        !ModuleInfo, !Changed, Specs),

    % We get the pred_info from the ModuleInfo *again*, because
    % while we are doing mode inference on one procedure of a predicate,
    % we can add new mode declarations to that predicate. If we didn't
    % refetch the pred_info, we would be implicitly undoing the addition
    % of those new entries to the predicate's proc table.
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo1),
    pred_info_get_proc_table(PredInfo1, ProcMap1),
    map.det_update(ProcId, ProcInfo, ProcMap1, ProcMap),
    pred_info_set_proc_table(ProcMap, PredInfo1, PredInfo2),
    pred_info_set_clauses_info(ClausesInfo, PredInfo2, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- type maybe_infer_modes
    --->    do_not_infer_modes
    ;       do_infer_modes.

:- type maybe_unify_pred
    --->    is_not_unify_pred
    ;       is_unify_pred.

:- pred do_modecheck_proc(how_to_check_goal::in, may_change_called_proc::in,
    pred_id::in, pred_info::in, proc_id::in, proc_info::in, proc_info::out,
    clauses_info::out, module_info::in, module_info::out,
    bool::in, bool::out, list(error_spec)::out) is det.

do_modecheck_proc(WhatToCheck, MayChangeCalledProc,
        PredId, PredInfo0, ProcId, !ProcInfo, ClausesInfo,
        !ModuleInfo, !Changed, ErrorAndWarningSpecs) :-
    pred_info_get_markers(PredInfo0, Markers),
    ( if check_marker(Markers, marker_infer_modes) then
        InferModes = do_infer_modes
    else
        InferModes = do_not_infer_modes
    ),
    ( if is_unify_pred(PredInfo0) then
        IsUnifyPred = is_unify_pred
    else
        IsUnifyPred = is_not_unify_pred
    ),
    pred_info_get_origin(PredInfo0, Origin),

    % We use the context of the first clause, unless there weren't any clauses
    % at all, in which case we use the context of the mode declaration.
    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    clauses_info_clauses(Clauses, _ItemNumbers, ClausesInfo0, ClausesInfo),
    (
        Clauses = [FirstClause | _],
        Context = FirstClause ^ clause_context
    ;
        Clauses = [],
        proc_info_get_context(!.ProcInfo, Context)
    ),

    % Extract the useful fields in the proc_info.
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_argmodes(!.ProcInfo, ArgModes0),
    proc_info_arglives(!.ModuleInfo, !.ProcInfo, ArgLives0),
    proc_info_get_goal(!.ProcInfo, Body0),

    % Modecheck the body. First set the initial instantiation of the head
    % arguments, then modecheck the body, and then check that the final
    % instantiation matches that in the mode declaration.

    some [!ModeInfo] (
        % Construct the initial instmap.
        mode_list_get_initial_insts(!.ModuleInfo, ArgModes0, ArgInitialInsts),
        assoc_list.from_corresponding_lists(HeadVars, ArgInitialInsts, InstAL),
        InstMap0 = instmap_from_assoc_list(InstAL),

        % Construct the initial set of live vars:
        % initially, only the non-clobbered head variables are live.
        get_live_vars(HeadVars, ArgLives0, LiveVarsList),
        set_of_var.list_to_set(LiveVarsList, LiveVars),

        get_constrained_inst_vars(!.ModuleInfo, ArgModes0, HeadInstVars),

        % Initialize the mode info.
        mode_info_init(!.ModuleInfo, PredId, ProcId, Context, LiveVars,
            HeadInstVars, InstMap0, WhatToCheck, MayChangeCalledProc,
            !:ModeInfo),
        mode_info_set_changed_flag(!.Changed, !ModeInfo),

        mode_list_get_final_insts(!.ModuleInfo, ArgModes0, ArgFinalInsts0),

        modecheck_proc_body(!.ModuleInfo, WhatToCheck, InferModes, IsUnifyPred,
            Markers, PredId, ProcId, Body0, Body, HeadVars,
            InstMap0, ArgFinalInsts0, ArgFinalInsts, !ModeInfo),

        mode_info_get_errors(!.ModeInfo, ModeErrors),
        (
            InferModes = do_infer_modes,
            % For inferred predicates, we don't report the error(s) here;
            % instead we just save them in the proc_info, thus marking that
            % procedure as invalid.
            proc_info_set_mode_errors(ModeErrors, !ProcInfo),
            ErrorAndWarningSpecs = []
        ;
            InferModes = do_not_infer_modes,
            ( if Origin = origin_compiler(made_for_mutable(_, _, _)) then
                % The only mode error that may occur in the automatically
                % generated auxiliary predicates for a mutable is an
                % invalid inst occurring in a mode, and we report a specific
                % error message for that. Giving another, less direct
                % description of the problem here would be confusing.
                ErrorAndWarningSpecs = []
            else
                AllErrorSpecs = list.map(mode_error_info_to_spec(!.ModeInfo),
                    ModeErrors),
                (
                    AllErrorSpecs = [ErrorSpec | _],
                    % We only return the first error, because
                    % (1) there could be a large number of mode errors;
                    % (2) most of the errors after the first are usually
                    %     "avalanche" errors caused by previous errors; and
                    % (3) the first is virtually always enough to diagnose
                    %     the problem, and if not, it is enough to diagnose
                    %     *one* problem, after whose fix we will report
                    %     another error.
                    ErrorSpecs = [ErrorSpec],
                    proc_info_get_statevar_warnings(!.ProcInfo,
                        StateVarWarningSpecs)
                ;
                    AllErrorSpecs = [],
                    ErrorSpecs = [],
                    % If there were no errors, then ignore the informational
                    % messages generated by the state variable transformation.
                    StateVarWarningSpecs = []
                ),
                mode_info_get_warnings(!.ModeInfo, ModeWarnings),
                WarningSpecs = list.map(mode_warning_info_to_spec(!.ModeInfo),
                    ModeWarnings),
                ErrorAndWarningSpecs = ErrorSpecs ++ WarningSpecs ++
                    StateVarWarningSpecs
            )
        ),

        % Save away the results.
        inst_lists_to_mode_list(ArgInitialInsts, ArgFinalInsts, ArgModes),
        mode_info_get_changed_flag(!.ModeInfo, !:Changed),
        mode_info_get_module_info(!.ModeInfo, !:ModuleInfo),
        % VarTable may differ from VarTable0, since mode checking can
        % add new variables (e.g. when handling calls in implied modes).
        mode_info_get_var_table(!.ModeInfo, VarTable),
        mode_info_get_need_to_requantify(!.ModeInfo, NeedToRequantify),
        proc_info_set_goal(Body, !ProcInfo),
        proc_info_set_var_table(VarTable, !ProcInfo),
        proc_info_set_argmodes(ArgModes, !ProcInfo),
        (
            NeedToRequantify = do_not_need_to_requantify
        ;
            NeedToRequantify = need_to_requantify,
            requantify_proc_general(ord_nl_maybe_lambda, !ProcInfo)
        )
    ).

:- pred modecheck_proc_body(module_info::in, how_to_check_goal::in,
    maybe_infer_modes::in, maybe_unify_pred::in, pred_markers::in,
    pred_id::in, proc_id::in, hlds_goal::in, hlds_goal::out,
    list(prog_var)::in, instmap::in, list(mer_inst)::in, list(mer_inst)::out,
    mode_info::in, mode_info::out) is det.

modecheck_proc_body(ModuleInfo, WhatToCheck, InferModes, IsUnifyPred, Markers,
        PredId, ProcId, Body0, Body, HeadVars, InstMap0,
        ArgFinalInsts0, ArgFinalInsts, ModeInfo0, ModeInfo) :-
    do_modecheck_proc_body(ModuleInfo, WhatToCheck, InferModes, IsUnifyPred,
        Markers, PredId, ProcId, Body0, Body1, HeadVars, InstMap0,
        ArgFinalInsts0, ArgFinalInsts1, ModeInfo0, ModeInfo1),
    mode_info_get_errors(ModeInfo1, ModeErrors1),
    (
        ModeErrors1 = [],
        Body = Body1,
        ArgFinalInsts = ArgFinalInsts1,
        ModeInfo = ModeInfo1
    ;
        ModeErrors1 = [_ | _],
        mode_info_get_had_from_ground_term(ModeInfo1, HadFromGroundTerm),
        (
            HadFromGroundTerm = had_from_ground_term_scope,
            % The error could have been due a ground term that we marked down
            % as ground instead of unique. We therefore try again from the
            % beginning, but this time, we tell the code that handles
            % from_ground_term scopes to create unique terms.
            %
            % Note that this may be overkill. Even if e.g. the procedure has
            % three from_ground_term_construct scopes, only one of which needs
            % to be unique for mode analysis to succeed, we will call copy
            % after all three. Fixing this would require a significantly more
            % complicated approach.
            mode_info_set_make_ground_terms_unique(make_ground_terms_unique,
                ModeInfo0, ModeInfo2),
            do_modecheck_proc_body(ModuleInfo, WhatToCheck, InferModes,
                IsUnifyPred, Markers, PredId, ProcId, Body0, Body, HeadVars,
                InstMap0, ArgFinalInsts0, ArgFinalInsts, ModeInfo2, ModeInfo)
        ;
            HadFromGroundTerm = did_not_have_from_ground_term_scope,
            % The error could not have been due a ground term, so the results
            % of the first analysis must stand.
            Body = Body1,
            ArgFinalInsts = ArgFinalInsts1,
            ModeInfo = ModeInfo1
        )
    ).

:- pred do_modecheck_proc_body(module_info::in, how_to_check_goal::in,
    maybe_infer_modes::in, maybe_unify_pred::in, pred_markers::in,
    pred_id::in, proc_id::in, hlds_goal::in, hlds_goal::out,
    list(prog_var)::in, instmap::in, list(mer_inst)::in, list(mer_inst)::out,
    mode_info::in, mode_info::out) is det.

do_modecheck_proc_body(ModuleInfo, WhatToCheck, InferModes, IsUnifyPred,
        Markers, PredId, ProcId, Body0, Body, HeadVars, InstMap0,
        ArgFinalInsts0, ArgFinalInsts, !ModeInfo) :-
    string.format("procedure_%d_%d",
        [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))],
        CheckpointMsg),
    ( if
        InferModes = do_not_infer_modes,
        check_marker(Markers, marker_mode_check_clauses),
        Body0 = hlds_goal(BodyGoalExpr0, BodyGoalInfo0),
        (
            BodyGoalExpr0 = disj(Disjuncts0),
            Disjuncts0 = [_ | _],
            ClausesForm0 = clause_disj(Disjuncts0)
        ;
            BodyGoalExpr0 = switch(SwitchVar0, CanFail0, Cases0),
            Cases0 = [_ | _],
            ClausesForm0 = clause_switch(SwitchVar0, CanFail0, Cases0)
        ),
        BodyNonLocals = goal_info_get_nonlocals(BodyGoalInfo0),
        mode_info_get_var_table(!.ModeInfo, VarTable0),
        SolverNonLocals = list.filter(
            var_is_or_may_contain_solver_type(ModuleInfo, VarTable0),
            set_of_var.to_sorted_list(BodyNonLocals)),
        SolverNonLocals = []
    then
        BodyContext = goal_info_get_context(BodyGoalInfo0),
        ( if is_dummy_context(BodyContext) then
            true
        else
            mode_info_set_context(BodyContext, !ModeInfo)
        ),

        % Modecheck each clause of the procedure body separately.
        (
            WhatToCheck = check_modes,
            (
                ClausesForm0 = clause_disj(Disjuncts1),
                flatten_disj(Disjuncts1, Disjuncts2),
                list.map_foldl(
                    modecheck_clause_disj(CheckpointMsg, HeadVars,
                        InstMap0, ArgFinalInsts0),
                    Disjuncts2, Disjuncts, !ModeInfo),
                NewGoalExpr = disj(Disjuncts)
            ;
                ClausesForm0 = clause_switch(SwitchVar, CanFail, Cases1),
                list.map_foldl(
                    modecheck_clause_switch(CheckpointMsg, HeadVars,
                        InstMap0, ArgFinalInsts0, SwitchVar),
                    Cases1, Cases, !ModeInfo),
                NewGoalExpr = switch(SwitchVar, CanFail, Cases)
            )
        ;
            WhatToCheck = check_unique_modes,
            mode_info_get_nondet_live_vars(!.ModeInfo, NondetLiveVars0),
            Detism = goal_info_get_determinism(BodyGoalInfo0),
            NonLocals = goal_info_get_nonlocals(BodyGoalInfo0),
            determinism_components(Detism, _, SolnCount),
            (
                SolnCount = at_most_many
            ;
                ( SolnCount = at_most_zero
                ; SolnCount = at_most_one
                ; SolnCount = at_most_many_cc
                ),
                mode_info_set_nondet_live_vars(bag.init, !ModeInfo)
            ),
            (
                ClausesForm0 = clause_disj(Disjuncts1),
                flatten_disj(Disjuncts1, Disjuncts2),
                (
                    SolnCount = at_most_many,
                    mode_info_add_live_vars(NonLocals, !ModeInfo),
                    make_all_nondet_live_vars_mostly_uniq(!ModeInfo),
                    mode_info_remove_live_vars(NonLocals, !ModeInfo)
                ;
                    ( SolnCount = at_most_zero
                    ; SolnCount = at_most_one
                    ; SolnCount = at_most_many_cc
                    )
                ),
                list.map_foldl(
                    unique_modecheck_clause_disj(CheckpointMsg, HeadVars,
                        InstMap0, ArgFinalInsts0, Detism, NonLocals,
                        NondetLiveVars0),
                    Disjuncts2, Disjuncts, !ModeInfo),
                NewGoalExpr = disj(Disjuncts)
            ;
                ClausesForm0 = clause_switch(SwitchVar, CanFail, Cases1),
                list.map_foldl(
                    unique_modecheck_clause_switch(CheckpointMsg, HeadVars,
                        InstMap0, ArgFinalInsts0, SwitchVar),
                    Cases1, Cases, !ModeInfo),
                NewGoalExpr = switch(SwitchVar, CanFail, Cases)
            )
        ),

        % Manufacture an instmap_delta for the disjunction as a whole.
        assoc_list.from_corresponding_lists(HeadVars, ArgFinalInsts0,
            HeadVarFinalInsts),
        FinalInstMap = instmap_from_assoc_list(HeadVarFinalInsts),
        compute_instmap_delta(InstMap0, FinalInstMap, BodyNonLocals,
            DeltaInstMap),
        goal_info_set_instmap_delta(DeltaInstMap, BodyGoalInfo0, BodyGoalInfo),
        Body = hlds_goal(NewGoalExpr, BodyGoalInfo),
        ArgFinalInsts = ArgFinalInsts0
    else
        % Modecheck the procedure body as a single goal.
        mode_checkpoint(enter, CheckpointMsg, !ModeInfo),
        (
            WhatToCheck = check_modes,
            modecheck_goal(Body0, Body, !ModeInfo)
        ;
            WhatToCheck = check_unique_modes,
            unique_modes_check_goal(Body0, Body, !ModeInfo)
        ),
        mode_checkpoint(exit, CheckpointMsg, !ModeInfo),

        % Check that final insts match those specified in the mode declaration.
        (
            IsUnifyPred = is_not_unify_pred,
            GroundMatchesBound = ground_matches_bound_if_complete
        ;
            IsUnifyPred = is_unify_pred,
            GroundMatchesBound = ground_matches_bound_always
        ),
        modecheck_final_insts_gmb(InferModes, GroundMatchesBound,
            HeadVars, ArgFinalInsts0, ArgFinalInsts, !ModeInfo)
    ).

%-----------------------------------------------------------------------------%

    % Do mode analysis of the queued procedures. If the first argument is
    % `unique_mode_check', then also go on and do full determinism analysis
    % and unique mode analysis on them as well. The pred_id_table arguments
    % are used to store copies of the procedure bodies before unique mode
    % analysis, so that we can restore them before doing the next analysis
    % pass.
    %
:- pred modecheck_queued_procs(how_to_check_goal::in,
    pred_id_table::in, pred_id_table::out, module_info::in, module_info::out,
    bool::in, bool::out, list(error_spec)::in, list(error_spec)::out) is det.

modecheck_queued_procs(HowToCheckGoal, !OldPredIdTable, !ModuleInfo,
        !Changed, !Specs) :-
    module_info_get_proc_requests(!.ModuleInfo, Requests0),
    get_req_queue(Requests0, RequestQueue0),
    ( if queue.get(PredProcId, RequestQueue0, RequestQueue1) then
        set_req_queue(RequestQueue1, Requests0, Requests1),
        module_info_set_proc_requests(Requests1, !ModuleInfo),

        % Check that the procedure is valid before we attempt to do
        % mode analysis on it. This check is necessary to avoid
        % internal errors caused by
        % (a) doing mode analysis on type-incorrect code, and
        % (b) doing mode inference on predicates that have higher order
        % arguments.

        PredProcId = proc(PredId, _ProcId),
        module_info_get_valid_pred_id_set(!.ModuleInfo, ValidPredIds),
        ( if set_tree234.member(PredId, ValidPredIds) then
            trace [io(!IO)] (
                queued_proc_progress_message(!.ModuleInfo, PredProcId,
                    HowToCheckGoal, !IO)
            ),
            modecheck_queued_proc(HowToCheckGoal, PredProcId,
                !OldPredIdTable, !ModuleInfo, HeadChanged, HeadSpecs),
            bool.or(HeadChanged, !Changed),
            !:Specs = HeadSpecs ++ !.Specs
        else
            true
        ),
        disable_warning [suspicious_recursion] (
            modecheck_queued_procs(HowToCheckGoal, !OldPredIdTable,
                !ModuleInfo, !Changed, !Specs)
        )
    else
        true
    ).

:- pred queued_proc_progress_message(module_info::in, pred_proc_id::in,
    how_to_check_goal::in, io::di, io::uo) is det.

queued_proc_progress_message(ModuleInfo, PredProcId, HowToCheckGoal, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        module_info_get_name(ModuleInfo, ModuleName),
        get_progress_output_stream(Globals, ModuleName, ProgressStream, !IO),
        ProcStr = pred_proc_id_to_user_string(ModuleInfo, PredProcId),
        (
            HowToCheckGoal = check_modes,
            io.format(ProgressStream, "%% Mode-analysing %s\n",
                [s(ProcStr)], !IO)
        ;
            HowToCheckGoal = check_unique_modes,
            io.format(ProgressStream, "%% Analysing unique modes for\n%% %s",
                [s(ProcStr)], !IO)
        )
    ;
        VeryVerbose = no
    ).

:- pred modecheck_queued_proc(how_to_check_goal::in, pred_proc_id::in,
    pred_id_table::in, pred_id_table::out, module_info::in, module_info::out,
    bool::out, list(error_spec)::out) is det.

modecheck_queued_proc(HowToCheckGoal, PredProcId, !OldPredIdTable, !ModuleInfo,
        !:Changed, Specs) :-
    PredProcId = proc(PredId, ProcId),

    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_proc_info(PredInfo0, ProcId, ProcInfo0),

    % Mark the procedure as ready to be processed.
    proc_info_set_can_process(can_process_now, ProcInfo0, ProcInfo1),

    pred_info_set_proc_info(ProcId, ProcInfo1, PredInfo0, PredInfo1),
    module_info_set_pred_info(PredId, PredInfo1, !ModuleInfo),

    % Modecheck the procedure.
    definitely_modecheck_proc(check_modes, may_change_called_proc,
        PredId, ProcId, ProcInfo1, !ModuleInfo, no, !:Changed, ModeSpecs),

    module_info_get_globals(!.ModuleInfo, Globals),
    ModeErrors = contains_errors(Globals, ModeSpecs),
    (
        ModeErrors = yes,
        module_info_make_pred_id_invalid(PredId, !ModuleInfo),
        Specs = ModeSpecs
    ;
        ModeErrors = no,
        (
            HowToCheckGoal = check_unique_modes,

            module_info_pred_info(!.ModuleInfo, PredId, PredInfo2),
            pred_info_proc_info(PredInfo2, ProcId, ProcInfo2),

            SwitchDetectInfo = init_switch_detect_info(!.ModuleInfo),
            detect_switches_in_proc(SwitchDetectInfo, ProcInfo2, ProcInfo3),

            pred_info_set_proc_info(ProcId, ProcInfo3, PredInfo2, PredInfo3),
            module_info_set_pred_info(PredId, PredInfo3, !ModuleInfo),

            detect_cse_in_proc(maybe.no, PredId, ProcId, !ModuleInfo),
            determinism_check_proc(PredId, ProcId, !ModuleInfo, DetismSpecs),
            expect(unify(DetismSpecs, []), $pred, "found detism error"),
            save_proc_info(!.ModuleInfo, ProcId, PredId, !OldPredIdTable),
            unique_modes_check_proc(PredId, ProcId, !ModuleInfo,
                NewChanged, UniqueSpecs),
            bool.or(NewChanged, !Changed),
            Specs = ModeSpecs ++ UniqueSpecs
        ;
            HowToCheckGoal = check_modes,
            Specs = ModeSpecs
        )
    ).

    % Save a copy of the proc info for the specified procedure in
    % !OldProcTable.
    %
:- pred save_proc_info(module_info::in, proc_id::in, pred_id::in,
    pred_id_table::in, pred_id_table::out) is det.

save_proc_info(ModuleInfo, ProcId, PredId, !OldPredIdTable) :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        _PredInfo, ProcInfo),
    map.lookup(!.OldPredIdTable, PredId, OldPredInfo0),
    pred_info_get_proc_table(OldPredInfo0, OldProcTable0),
    map.set(ProcId, ProcInfo, OldProcTable0, OldProcTable),
    pred_info_set_proc_table(OldProcTable, OldPredInfo0, OldPredInfo),
    map.det_update(PredId, OldPredInfo, !OldPredIdTable).

%-----------------------------------------------------------------------------%

:- type clause_form
    --->    clause_disj(list(hlds_goal))
    ;       clause_switch(prog_var, can_fail, list(case)).

:- pred modecheck_clause_disj(string::in, list(prog_var)::in, instmap::in,
    list(mer_inst)::in, hlds_goal::in, hlds_goal::out,
    mode_info::in, mode_info::out) is det.

modecheck_clause_disj(CheckpointMsg, HeadVars, InstMap0, ArgFinalInsts0,
        Disjunct0, Disjunct, !ModeInfo) :-
    mode_checkpoint(enter, CheckpointMsg, !ModeInfo),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    modecheck_goal(Disjunct0, Disjunct, !ModeInfo),
    mode_checkpoint(exit, CheckpointMsg, !ModeInfo),

    % Check that final insts match those specified in the mode declaration.
    modecheck_final_insts(do_not_infer_modes, HeadVars,
        ArgFinalInsts0, _ArgFinalInsts, !ModeInfo).

:- pred modecheck_clause_switch(string::in, list(prog_var)::in, instmap::in,
    list(mer_inst)::in, prog_var::in, case::in, case::out,
    mode_info::in, mode_info::out) is det.

modecheck_clause_switch(CheckpointMsg, HeadVars, InstMap0, ArgFinalInsts0,
        Var, Case0, Case, !ModeInfo) :-
    mode_checkpoint(enter, CheckpointMsg, !ModeInfo),
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    mode_info_set_instmap(InstMap0, !ModeInfo),

    modecheck_functors_test(Var, MainConsId, OtherConsIds, !ModeInfo),

    % Modecheck this case (if it is reachable).
    mode_info_get_instmap(!.ModeInfo, InstMap1),
    ( if instmap_is_reachable(InstMap1) then
        modecheck_goal(Goal0, Goal1, !ModeInfo),
        mode_info_get_instmap(!.ModeInfo, InstMap)
    else
        % We should not mode-analyse the goal, since it is unreachable.
        % Instead we optimize the goal away, so that later passes
        % won't complain about it not having mode information.
        Goal1 = true_goal,
        InstMap = InstMap1
    ),

    % Don't lose the information added by the functor test above.
    fixup_instmap_switch_var(Var, InstMap0, InstMap, Goal1, Goal),
    mode_checkpoint(exit, CheckpointMsg, !ModeInfo),

    % Check that final insts match those specified in the mode declaration.
    modecheck_final_insts(do_not_infer_modes, HeadVars,
        ArgFinalInsts0, _ArgFinalInsts, !ModeInfo),
    Case = case(MainConsId, OtherConsIds, Goal).

:- pred unique_modecheck_clause_disj(string::in, list(prog_var)::in,
    instmap::in, list(mer_inst)::in, determinism::in, set_of_progvar::in,
    bag(prog_var)::in, hlds_goal::in, hlds_goal::out,
    mode_info::in, mode_info::out) is det.

unique_modecheck_clause_disj(CheckpointMsg, HeadVars, InstMap0, ArgFinalInsts0,
        DisjDetism, DisjNonLocals, NondetLiveVars0,
        Disjunct0, Disjunct, !ModeInfo) :-
    mode_checkpoint(enter, CheckpointMsg, !ModeInfo),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    mode_info_set_nondet_live_vars(NondetLiveVars0, !ModeInfo),
    unique_modes.prepare_for_disjunct(Disjunct0, DisjDetism, DisjNonLocals,
        !ModeInfo),
    unique_modes_check_goal(Disjunct0, Disjunct, !ModeInfo),
    mode_checkpoint(exit, CheckpointMsg, !ModeInfo),

    % Check that final insts match those specified in the mode declaration.
    modecheck_final_insts(do_not_infer_modes, HeadVars,
        ArgFinalInsts0, _ArgFinalInsts, !ModeInfo).

:- pred unique_modecheck_clause_switch(string::in, list(prog_var)::in,
    instmap::in, list(mer_inst)::in, prog_var::in, case::in, case::out,
    mode_info::in, mode_info::out) is det.

unique_modecheck_clause_switch(CheckpointMsg, HeadVars, InstMap0,
        ArgFinalInsts0, Var, Case0, Case, !ModeInfo) :-
    mode_checkpoint(enter, CheckpointMsg, !ModeInfo),
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    mode_info_set_instmap(InstMap0, !ModeInfo),

    modecheck_functors_test(Var, MainConsId, OtherConsIds, !ModeInfo),

    mode_info_get_instmap(!.ModeInfo, InstMap1),
    ( if instmap_is_reachable(InstMap1) then
        unique_modes_check_goal(Goal0, Goal1, !ModeInfo)
    else
        % We should not mode-analyse the goal, since it is unreachable.
        % Instead we optimize the goal away, so that later passes
        % won't complain about it not having mode information.
        Goal1 = true_goal
    ),

    % Don't lose the information added by the functor test above.
    mode_info_get_instmap(!.ModeInfo, InstMap),
    fixup_instmap_switch_var(Var, InstMap0, InstMap, Goal1, Goal),
    mode_checkpoint(exit, CheckpointMsg, !ModeInfo),

    % Check that final insts match those specified in the mode declaration.
    modecheck_final_insts(do_not_infer_modes, HeadVars,
        ArgFinalInsts0, _ArgFinalInsts, !ModeInfo),
    Case = case(MainConsId, OtherConsIds, Goal).

%-----------------------------------------------------------------------------%

modecheck_lambda_final_insts(HeadVars, ArgFinalInsts, !ModeInfo) :-
    % This is modecheck_final_insts for a lambda expression.
    %
    % For lambda expressions, modes must always be declared;
    % we never infer them.
    modecheck_final_insts(do_not_infer_modes, HeadVars,
        ArgFinalInsts, _NewFinalInsts, !ModeInfo).

    % Check that the final insts of the head vars match their expected insts.
    %
:- pred modecheck_final_insts(maybe_infer_modes::in, list(prog_var)::in,
    list(mer_inst)::in, list(mer_inst)::out,
    mode_info::in, mode_info::out) is det.

modecheck_final_insts(InferModes, HeadVars, !FinalInsts, !ModeInfo) :-
    modecheck_final_insts_gmb(InferModes, ground_matches_bound_if_complete,
        HeadVars, !FinalInsts, !ModeInfo).

:- pred modecheck_final_insts_gmb(maybe_infer_modes::in,
    ground_matches_bound::in, list(prog_var)::in,
    list(mer_inst)::in, list(mer_inst)::out,
    mode_info::in, mode_info::out) is det.

modecheck_final_insts_gmb(InferModes, GroundMatchesBound,
        HeadVars, FinalInsts0, FinalInsts, !ModeInfo) :-
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
        % If there were any mode errors, something must have changed, since
        % if the procedure had mode errors in a previous pass, then it
        % wouldn't have been processed at all in this pass.
        Changed0 = yes,
        instmap.init_unreachable(InstMap)
    ;
        Errors = [],
        Changed0 = no,
        mode_info_get_instmap(!.ModeInfo, InstMap)
    ),
    mode_info_get_var_table(!.ModeInfo, VarTable),
    instmap_lookup_vars(InstMap, HeadVars, VarFinalInsts0),
    lookup_var_types(VarTable, HeadVars, ArgTypes),
    (
        InferModes = do_infer_modes,
        % Make sure we set the final insts of any variables which
        % we assumed were dead to `clobbered'.
        mode_info_get_pred_id(!.ModeInfo, PredId),
        mode_info_get_proc_id(!.ModeInfo, ProcId),
        module_info_proc_info(ModuleInfo, PredId, ProcId, ProcInfo),
        proc_info_arglives(ModuleInfo, ProcInfo, ArgLives),
        normalise_insts(ModuleInfo, ArgTypes, VarFinalInsts0, VarFinalInsts1),
        maybe_clobber_insts(VarFinalInsts1, ArgLives, VarFinalInsts2),
        check_final_insts(InferModes, GroundMatchesBound, HeadVars,
            VarFinalInsts2, FinalInsts0, 1, no, Changed1, !ModeInfo),
        FinalInsts = VarFinalInsts2,
        mode_info_get_changed_flag(!.ModeInfo, Changed2),
        bool.or_list([Changed0, Changed1, Changed2], Changed),
        mode_info_set_changed_flag(Changed, !ModeInfo)
    ;
        InferModes = do_not_infer_modes,
        check_final_insts(InferModes, GroundMatchesBound, HeadVars,
            VarFinalInsts0, FinalInsts0, 1, no, _Changed1, !ModeInfo),
        FinalInsts = FinalInsts0
    ).

:- pred maybe_clobber_insts(list(mer_inst)::in, list(is_live)::in,
    list(mer_inst)::out) is det.

maybe_clobber_insts([], [], []).
maybe_clobber_insts([], [_ | _], _) :-
    unexpected($pred, "length mismatch").
maybe_clobber_insts([_ | _], [], _) :-
    unexpected($pred, "length mismatch").
maybe_clobber_insts([Inst0 | Insts0], [IsLive | IsLives], [Inst | Insts]) :-
    (
        IsLive = is_dead,
        Inst = ground(clobbered, none_or_default_func)
    ;
        IsLive = is_live,
        Inst = Inst0
    ),
    maybe_clobber_insts(Insts0, IsLives, Insts).

:- pred check_final_insts(maybe_infer_modes::in, ground_matches_bound::in,
    list(prog_var)::in, list(mer_inst)::in, list(mer_inst)::in, int::in,
    bool::in, bool::out, mode_info::in, mode_info::out) is det.

check_final_insts(InferModes, GroundMatchesBound,
        Vars, VarInsts, ExpectedInsts, ArgNum, !Changed, !ModeInfo) :-
    ( if
        Vars = [],
        VarInsts = [],
        ExpectedInsts = []
    then
        true
    else if
        Vars = [HeadVar | TailVars],
        VarInsts = [HeadVarInst | TailVarInsts],
        ExpectedInsts = [HeadExpectedInst | TailExpectedInsts]
    then
        check_final_inst(InferModes, GroundMatchesBound,
            HeadVar, HeadVarInst, HeadExpectedInst, ArgNum,
            !Changed, !ModeInfo),
        check_final_insts(InferModes, GroundMatchesBound,
            TailVars, TailVarInsts, TailExpectedInsts, ArgNum + 1,
            !Changed, !ModeInfo)
    else
        unexpected($pred, "length mismatch")
    ).

:- pred check_final_inst(maybe_infer_modes::in, ground_matches_bound::in,
    prog_var::in, mer_inst::in, mer_inst::in, int::in,
    bool::in, bool::out, mode_info::in, mode_info::out) is det.

check_final_inst(InferModes, GroundMatchesBound,
        Var, VarInst, ExpectedInst, ArgNum, !Changed, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_var_table(!.ModeInfo, VarTable),
    lookup_var_type(VarTable, Var, Type),
    ( if
        inst_matches_final_gmb(ModuleInfo, GroundMatchesBound, Type,
            VarInst, ExpectedInst)
    then
        true
    else
        !:Changed = yes,
        (
            % If we are inferring the mode, then don't report an error,
            % just set changed to yes to make sure that we will do
            % another fixpoint pass.
            InferModes = do_infer_modes
        ;
            InferModes = do_not_infer_modes,
            % XXX This might need to be reconsidered now we have
            % unique modes.
            ( if
                inst_matches_initial(ModuleInfo, Type, VarInst, ExpectedInst)
            then
                Reason = too_instantiated
            else if
                % The only reason why VarInst is not good enough
                % if the expected Inst is simply `ground' is that
                % it is not instantiated enough. Unfortunately,
                % we need to test separately for this, because the call
                % to inst_matches_initial below can fail, even if
                % Inst is `ground', because VarInst contains parts
                % that are too unique, or because it does not cover
                % all the function symbols in Type.
                % This is a side effect of having an inst representation
                % that entangles uniqueness information and which-functor
                % information with information about how bound a variable
                % is. In the extremely common case that Inst is `ground',
                % we need only the latter, but we can't get it by itself.
                ( ExpectedInst = ground(shared, none_or_default_func)
                ; inst_matches_initial(ModuleInfo, Type, ExpectedInst, VarInst)
                )
            then
                Reason = not_instantiated_enough
            else
                % I don't think this can happen. But just in case...
                Reason = wrongly_instantiated
            ),
            set_of_var.init(WaitingVars),
            ModeError = mode_error_unexpected_final_inst(ArgNum, Var,
                VarInst, ExpectedInst, Reason),
            mode_info_error(WaitingVars, ModeError, !ModeInfo)
        )
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Check that the evaluation method is OK for the given mode(s).
    % We also check the mode of main/2 here.
    %
:- pred module_check_eval_methods_and_main(module_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

module_check_eval_methods_and_main(ModuleInfo, !Specs) :-
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    pred_check_eval_methods_and_main(ModuleInfo, PredIds, !Specs).

:- pred pred_check_eval_methods_and_main(module_info::in, list(pred_id)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

pred_check_eval_methods_and_main(_, [], !Specs).
pred_check_eval_methods_and_main(ModuleInfo, [PredId | PredIds], !Specs) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_procids(PredInfo),
    proc_check_eval_methods_and_main(ModuleInfo, PredInfo, ProcIds, !Specs),
    pred_check_eval_methods_and_main(ModuleInfo, PredIds, !Specs).

:- pred proc_check_eval_methods_and_main(module_info::in, pred_info::in,
    list(proc_id)::in, list(error_spec)::in, list(error_spec)::out) is det.

proc_check_eval_methods_and_main(_, _, [], !Specs).
proc_check_eval_methods_and_main(ModuleInfo, PredInfo, [ProcId | ProcIds],
        !Specs) :-
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.lookup(ProcTable, ProcId, ProcInfo),
    proc_info_get_eval_method(ProcInfo, EvalMethod),
    proc_info_get_argmodes(ProcInfo, Modes),
    (
        EvalMethod = eval_normal
    ;
        EvalMethod = eval_tabled(TabledMethod),
        ( if only_fully_in_out_modes(ModuleInfo, Modes) then
            true
        else
            % All tabled methods require ground arguments.
            GroundArgsSpec = report_eval_method_requires_ground_args(ProcInfo,
                TabledMethod),
            !:Specs = [GroundArgsSpec | !.Specs]
        ),
        ( if
            tabled_eval_method_destroys_uniqueness(TabledMethod) = yes,
            not only_nonunique_modes(ModuleInfo, Modes)
        then
            UniquenessSpec =
                report_eval_method_destroys_uniqueness(ProcInfo, TabledMethod),
            !:Specs = [UniquenessSpec | !.Specs]
        else
            true
        )
    ),
    ( if
        pred_info_name(PredInfo) = "main",
        pred_info_orig_arity(PredInfo) = 2,
        pred_info_is_exported(PredInfo),
        not modes_are_valid_for_main(ModuleInfo, Modes)
    then
        MainSpec = report_wrong_mode_for_main(ProcInfo),
        !:Specs = [MainSpec | !.Specs]
    else
        true
    ),
    proc_check_eval_methods_and_main(ModuleInfo, PredInfo, ProcIds, !Specs).

:- pred only_fully_in_out_modes(module_info::in, list(mer_mode)::in)
    is semidet.

only_fully_in_out_modes(_, []).
only_fully_in_out_modes(ModuleInfo, [Mode | Modes]) :-
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
    only_fully_in_out_modes(ModuleInfo, Modes).

:- pred only_nonunique_modes(module_info::in, list(mer_mode)::in) is semidet.

only_nonunique_modes(_, []).
only_nonunique_modes(ModuleInfo, [Mode | Modes]) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    inst_is_not_partly_unique(ModuleInfo, InitialInst),
    inst_is_not_partly_unique(ModuleInfo, FinalInst),
    only_nonunique_modes(ModuleInfo, Modes).

:- pred modes_are_valid_for_main(module_info::in, list(mer_mode)::in)
    is semidet.

modes_are_valid_for_main(ModuleInfo, [Di, Uo]) :-
    mode_get_insts(ModuleInfo, Di, DiInitialInst, DiFinalInst),
    mode_get_insts(ModuleInfo, Uo, UoInitialInst, UoFinalInst),
    % Note that we hard-code these tests, rather than using `inst_is_free',
    % `inst_is_unique', etc., since for main/2, we are looking for
    % an exact match (modulo inst synonyms) with what the language reference
    % manual specifies, rather than looking for a particular abstract property.
    Unique = ground(unique, none_or_default_func),
    Clobbered = ground(clobbered, none_or_default_func),
    inst_expand(ModuleInfo, DiInitialInst, Unique),
    inst_expand(ModuleInfo, DiFinalInst, Clobbered),
    inst_expand(ModuleInfo, UoInitialInst, Free),
    ( Free = free ; Free = free(_Type) ),
    inst_expand(ModuleInfo, UoFinalInst, Unique).

:- func report_eval_method_requires_ground_args(proc_info, tabled_eval_method)
    = error_spec.

report_eval_method_requires_ground_args(ProcInfo, TabledMethod) = Spec :-
    proc_info_get_context(ProcInfo, Context),
    TabledMethodStr = tabled_eval_method_to_pragma_name(TabledMethod),
    MainPieces = [words("Sorry, not implemented:"),
        pragma_decl(TabledMethodStr),
        words("declaration not allowed for procedure"),
        words("with partially instantiated modes."), nl],
    VerbosePieces = [words("Tabling of predicates/functions"),
        words("with partially instantiated modes"),
        words("is not currently implemented."), nl],
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(verbose_once, VerbosePieces)]),
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), [Msg]).

:- func report_eval_method_destroys_uniqueness(proc_info, tabled_eval_method)
    = error_spec.

report_eval_method_destroys_uniqueness(ProcInfo, TabledMethod) = Spec :-
    proc_info_get_context(ProcInfo, Context),
    TabledMethodStr = tabled_eval_method_to_pragma_name(TabledMethod),
    MainPieces = [words("Error:"),
        pragma_decl(TabledMethodStr), words("declaration"),
        words("not allowed for procedure with unique modes."), nl],
    VerbosePieces =
        [words("Tabling of predicates/functions with unique modes"),
        words("is not allowed, as tabling requires copying arguments,"),
        words("which would destroy their uniqueness."), nl],
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(verbose_once, VerbosePieces)]),
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), [Msg]).

:- func report_wrong_mode_for_main(proc_info) = error_spec.

report_wrong_mode_for_main(ProcInfo) = Spec :-
    proc_info_get_context(ProcInfo, Context),
    Pieces = [words("Error:"),
        unqual_sym_name_arity(sym_name_arity(unqualified("main"), 2)),
        words("must have mode"), quote("(di, uo)"), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Pieces).

%-----------------------------------------------------------------------------%

:- type include_detism_on_modes
    --->    include_detism_on_modes
    ;       do_not_include_detism_on_modes.

    % Generate the inferred `mode' declarations for a list of pred_ids.
    % The include_detism_on_modes argument indicates whether or not
    % to write out determinism annotations on the modes. (It should only
    % be set to `include_detism_on_modes' _after_ determinism analysis.)
    %
:- pred report_mode_inference_messages_for_preds(module_info::in,
    include_detism_on_modes::in, list(pred_id)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_mode_inference_messages_for_preds(_, _, [], !Specs).
report_mode_inference_messages_for_preds(ModuleInfo, OutputDetism,
        [PredId | PredIds], !Specs) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    ( if check_marker(Markers, marker_infer_modes) then
        ProcIds = pred_info_all_procids(PredInfo),
        pred_info_get_proc_table(PredInfo, Procs),
        report_mode_inference_messages_for_procs(ModuleInfo, OutputDetism,
            PredInfo, Procs, ProcIds, !Specs)
    else
        true
    ),
    report_mode_inference_messages_for_preds(ModuleInfo, OutputDetism,
        PredIds, !Specs).

    % Generate the inferred `mode' declarations for a list of proc_ids.
    %
:- pred report_mode_inference_messages_for_procs(module_info::in,
    include_detism_on_modes::in, pred_info::in, proc_table::in,
    list(proc_id)::in, list(error_spec)::in, list(error_spec)::out) is det.

report_mode_inference_messages_for_procs(_, _, _, _, [], !Specs).
report_mode_inference_messages_for_procs(ModuleInfo, OutputDetism,
        PredInfo, Procs, [ProcId | ProcIds], !Specs) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
    map.lookup(Procs, ProcId, ProcInfo),
    ( if
        (
            % We always output `Inferred :- mode ...'
            proc_info_is_valid_mode(ProcInfo)
        ;
            % We only output `REJECTED :- mode ...'
            % if --verbose-errors is enabled
            VerboseErrors = yes
        )
    then
        Spec = report_mode_inference_message(ModuleInfo, OutputDetism,
            PredInfo, ProcInfo),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    report_mode_inference_messages_for_procs(ModuleInfo,
        OutputDetism, PredInfo, Procs, ProcIds, !Specs).

    % Return a description of the inferred mode declaration for the given
    % predicate or function.
    %
:- func report_mode_inference_message(module_info, include_detism_on_modes,
    pred_info, proc_info) = error_spec.

report_mode_inference_message(ModuleInfo, OutputDetism, PredInfo, ProcInfo)
        = Spec :-
    PredName = pred_info_name(PredInfo),
    Name = unqualified(PredName),
    pred_info_get_context(PredInfo, Context),
    PredArity = pred_info_orig_arity(PredInfo),
    some [!ArgModes, !MaybeDet] (
        proc_info_get_argmodes(ProcInfo, !:ArgModes),

        % We need to strip off the extra type_info arguments inserted at the
        % front by polymorphism.m - we only want the last `PredArity' of them.
        %
        list.length(!.ArgModes, NumArgModes),
        NumToDrop = NumArgModes - PredArity,
        ( if list.drop(NumToDrop, !ArgModes) then
            true
        else
            unexpected($pred, "list.drop failed")
        ),

        varset.init(VarSet),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        (
            OutputDetism = include_detism_on_modes,
            proc_info_get_inferred_determinism(ProcInfo, Detism),
            !:MaybeDet = yes(Detism)
        ;
            OutputDetism = do_not_include_detism_on_modes,
            !:MaybeDet = no
        ),
        ( if proc_info_is_valid_mode(ProcInfo) then
            Verb = "Inferred"
        else
            Verb = "REJECTED",
            % Replace the final insts with dummy insts '...', since they
            % won't be valid anyway -- they are just the results of whatever
            % partial inference we did before detecting the error.
            mode_list_get_initial_insts(ModuleInfo, !.ArgModes, InitialInsts),
            DummyInst = defined_inst(user_inst(unqualified("..."), [])),
            list.duplicate(PredArity, DummyInst, FinalInsts),
            !:ArgModes = list.map(func(I - F) = from_to_mode(I, F),
                assoc_list.from_corresponding_lists(InitialInsts, FinalInsts)),
            % Likewise delete the determinism.
            !:MaybeDet = no
        ),
        strip_module_names_from_mode_list(strip_builtin_module_name,
            !ArgModes),
        (
            PredOrFunc = pf_predicate,
            MaybeWithInst = maybe.no,
            Detail = mercury_pred_mode_decl_to_string(output_debug, VarSet,
                Name, !.ArgModes, MaybeWithInst, !.MaybeDet)
        ;
            PredOrFunc = pf_function,
            pred_args_to_func_args(!.ArgModes, FuncArgModes, RetMode),
            Detail = mercury_func_mode_decl_to_string(output_debug, VarSet,
                Name, FuncArgModes, RetMode, !.MaybeDet)
        ),
        Pieces = [words(Verb), words(Detail), nl],
        Spec = conditional_spec($pred, inform_inferred_modes, yes,
            severity_informational, phase_mode_check(report_in_any_mode),
            [simplest_msg(Context, Pieces)])
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.modes.
%-----------------------------------------------------------------------------%
