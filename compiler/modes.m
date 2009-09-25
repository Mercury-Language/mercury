%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2009 The University of Melbourne.
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
%
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
% How to mode-analyse a goal is documented at the top of modecheck_goal.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.modes.
:- interface.

:- import_module check_hlds.mode_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type modes_safe_to_continue
    --->    modes_safe_to_continue
    ;       modes_unsafe_to_continue.

    % modecheck_module(!.HLDS, {!:HLDS, SafeToContinue, Specs}):
    %
    % Perform mode inference and checking for a whole module.
    %
    % SafeToContinue = modes_unsafe_to_continue means that mode inference
    % was halted prematurely due to an error, and that we should therefore
    % not perform determinism-checking, because we might get internal errors.
    %
    % The outputs are in a tuple because our caller wants to be able to
    % benchmark mode analysis, and the benchmark predicates require exactly
    % one output argument.
    %
:- pred modecheck_module(module_info::in,
    {module_info, modes_safe_to_continue, list(error_spec)}::out) is det.

    % Mode-check or unique-mode-check the code of all the predicates
    % in a module.
    %
:- pred check_pred_modes(how_to_check_goal::in, may_change_called_proc::in,
    module_info::in, module_info::out, modes_safe_to_continue::out,
    list(error_spec)::out) is det.

    % Mode-check the code for the given predicate in a given mode.
    % Returns the number of errs found and a bool `Changed'
    % which is true iff another pass of fixpoint analysis may be needed.
    %
:- pred modecheck_proc(proc_id::in, pred_id::in,
    module_info::in, module_info::out, list(error_spec)::out,
    bool::out) is det.

    % Mode-check or unique-mode-check the code for the given predicate
    % in a given mode.
    % Returns the number of errs found and a bool `Changed'
    % which is true iff another pass of fixpoint analysis may be needed.
    %
:- pred modecheck_proc_general(proc_id::in, pred_id::in, how_to_check_goal::in,
    may_change_called_proc::in, module_info::in, module_info::out,
    list(error_spec)::out, bool::out) is det.

    % Check that the final insts of the head vars of a lambda goal
    % matches their expected insts.
    %
:- pred modecheck_lambda_final_insts(list(prog_var)::in, list(mer_inst)::in,
    hlds_goal::in, hlds_goal::out, mode_info::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.clause_to_proc.
:- import_module check_hlds.cse_detection.
:- import_module check_hlds.delay_partial_inst.
:- import_module check_hlds.det_analysis.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.modecheck_goal.
:- import_module check_hlds.modecheck_util.
:- import_module check_hlds.switch_detection.
:- import_module check_hlds.type_util.
:- import_module check_hlds.unify_proc.
:- import_module check_hlds.unique_modes.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_out.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.

:- import_module assoc_list.
:- import_module bag.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module queue.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module term.

%-----------------------------------------------------------------------------%

modecheck_module(ModuleInfo0, {ModuleInfo, SafeToContinue, Specs}) :-
    module_info_get_globals(ModuleInfo0, Globals),
    trace [io(!IO)] (
        globals.lookup_bool_option(Globals, verbose, Verbose),
        maybe_write_string(Verbose, "% Mode-checking clauses...\n", !IO)
    ),
    check_pred_modes(check_modes, may_change_called_proc,
        ModuleInfo0, ModuleInfo, SafeToContinue, Specs),
    trace [io(!IO)] (
        globals.lookup_bool_option(Globals, statistics, Statistics),
        maybe_report_stats(Statistics, !IO)
    ).

%-----------------------------------------------------------------------------%

check_pred_modes(WhatToCheck, MayChangeCalledProc, !ModuleInfo,
        SafeToContinue, !:Specs) :-
    module_info_predids(PredIds, !ModuleInfo),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_int_option(Globals, mode_inference_iteration_limit,
        MaxIterations),
    modecheck_to_fixpoint(PredIds, MaxIterations, WhatToCheck,
        MayChangeCalledProc, !ModuleInfo, SafeToContinue0, !:Specs),
    (
        WhatToCheck = check_unique_modes,
        InferenceSpecs = report_mode_inference_messages(!.ModuleInfo,
            include_detism_on_modes, PredIds),
        !:Specs = InferenceSpecs ++ !.Specs,
        check_eval_methods(!ModuleInfo, !Specs),
        SafeToContinue = SafeToContinue0
    ;
        WhatToCheck = check_modes,
        (
            SafeToContinue0 = modes_unsafe_to_continue,
            InferenceSpecs = report_mode_inference_messages(!.ModuleInfo,
                do_not_include_detism_on_modes, PredIds),
            !:Specs = InferenceSpecs ++ !.Specs,
            SafeToContinue = modes_unsafe_to_continue
        ;
            SafeToContinue0 = modes_safe_to_continue,
            globals.lookup_bool_option(Globals, delay_partial_instantiations,
                DelayPartialInstantiations),
            (
                DelayPartialInstantiations = yes,
                delay_partial_inst_preds(PredIds, ChangedPreds, !ModuleInfo),
                % --delay-partial-instantiations requires mode checking to be
                % run again.
                modecheck_to_fixpoint(ChangedPreds, MaxIterations, WhatToCheck,
                    MayChangeCalledProc, !ModuleInfo,
                    SafeToContinue, FixpointSpecs),
                % !.Specs and FixpointSpecs are two sets of warning messages
                % about the program, with !.Specs being derived from the
                % version before delay_partial_inst_preds, and FixpointSpecs
                % being derived from the version after. They ought to be the
                % same, but just in case they are not, we do not want to
                % confuse users by reporting the same problem twice.
                % Which version we pick is a question of taste. !.Specs
                % is more closely related to the compiler's original expansion
                % of the program, but FixpointSpecs may be more closely related
                % to the expansion intended by the programmer.
                !:Specs = FixpointSpecs
            ;
                DelayPartialInstantiations = no,
                SafeToContinue = modes_safe_to_continue
            )
        )
    ).

    % Iterate over the list of pred_ids in a module.
    %
:- pred modecheck_to_fixpoint(list(pred_id)::in, int::in,
    how_to_check_goal::in, may_change_called_proc::in,
    module_info::in, module_info::out, modes_safe_to_continue::out,
    list(error_spec)::out) is det.

modecheck_to_fixpoint(PredIds, MaxIterations, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, SafeToContinue, !:Specs) :-
    % Save the old procedure bodies so that we can restore them for the
    % next pass.
    module_info_preds(!.ModuleInfo, OldPredTable0),

    % Analyze everything which has the "can-process" flag set to `yes'.
    list.foldl3(maybe_modecheck_pred(WhatToCheck, MayChangeCalledProc),
        PredIds, !ModuleInfo, no, Changed1, [], !:Specs),

    % Analyze the procedures whose "can-process" flag was no;
    % those procedures were inserted into the unify requests queue.
    modecheck_queued_procs(WhatToCheck, OldPredTable0, OldPredTable,
        !ModuleInfo, Changed2, QueuedSpecs),
    !:Specs = QueuedSpecs ++ !.Specs,
    bool.or(Changed1, Changed2, Changed),

    module_info_get_globals(!.ModuleInfo, Globals),
    ErrorsSoFar = contains_errors(Globals, !.Specs),
    (
        Changed = no,
        % Stop if we have reached a fixpoint.
        SafeToContinue = modes_safe_to_continue
    ;
        Changed = yes,
        (
            ErrorsSoFar = yes,
            % Stop if we have found any errors.
            SafeToContinue = modes_unsafe_to_continue
        ;
            ErrorsSoFar = no,
            ( MaxIterations =< 1 ->
                % Stop if we have exceeded the iteration limit.
                MaxIterSpec = report_max_iterations_exceeded(!.ModuleInfo),
                !:Specs = [MaxIterSpec | !.Specs],
                SafeToContinue = modes_unsafe_to_continue
            ;
                % Otherwise, continue iterating.
                globals.lookup_bool_option(Globals, debug_modes, DebugModes),
                (
                    DebugModes = yes,
                    InferenceSpecs =
                        report_mode_inference_messages(!.ModuleInfo,
                            do_not_include_detism_on_modes, PredIds),
                    trace [io(!IO)] (
                        io.write_string("Inferences by current iteration:\n",
                            !IO),
                        write_error_specs(InferenceSpecs, Globals,
                            0, _NumWarnings, 0, _NumErrors, !IO),
                        io.write_string("End of inferences.\n", !IO)
                    )
                ;
                    DebugModes = no
                ),

                % Mode analysis may have modified the procedure bodies,
                % since it does some optimizations such as deleting unreachable
                % code. But since we didn't reach a fixpoint yet, the mode
                % information is not yet correct, and so those optimizations
                % will have been done based on incomplete information, and so
                % they may produce incorrect results. We thus need to restore
                % the old procedure bodies.

                (
                    WhatToCheck = check_modes,
                    % Restore the proc_info goals from the clauses in the
                    % pred_info. Reintroduce exists_cast goals, since these
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
                    MayChangeCalledProc, !ModuleInfo, SafeToContinue, !:Specs)
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
        words("or use the `--mode-inference-iteration-limit' option"),
        words("to increase the limit."),
        words("(The current limit is"), int_fixed(MaxIterations),
        words("iterations.)"), nl],
    Msg = error_msg(no, do_not_treat_as_first, 0, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [Msg]).

    % copy_pred_bodies(OldPredTable, ProcId, ModuleInfo0, ModuleInfo):
    %
    % Copy the procedure bodies for all procedures of the specified PredIds
    % from OldPredTable into ModuleInfo0, giving ModuleInfo.
    %
:- pred copy_pred_bodies(pred_table::in, list(pred_id)::in,
    module_info::in, module_info::out) is det.

copy_pred_bodies(OldPredTable, PredIds, !ModuleInfo) :-
    module_info_preds(!.ModuleInfo, PredTable0),
    list.foldl(copy_pred_body(OldPredTable), PredIds, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo).

    % copy_pred_body(OldPredTable, ProcId, PredTable0, PredTable):
    %
    % Copy the procedure bodies for all procedures of the specified PredId
    % from OldPredTable into PredTable0, giving PredTable.
    %
:- pred copy_pred_body(pred_table::in, pred_id::in,
    pred_table::in, pred_table::out) is det.

copy_pred_body(OldPredTable, PredId, PredTable0, PredTable) :-
    map.lookup(PredTable0, PredId, PredInfo0),
    (
        % Don't copy type class methods, because their proc_infos are generated
        % already mode-correct, and because copying from the clauses_info
        % doesn't work for them.
        pred_info_get_markers(PredInfo0, Markers),
        check_marker(Markers, marker_class_method)
    ->
        PredTable = PredTable0
    ;
        pred_info_get_procedures(PredInfo0, ProcTable0),
        map.lookup(OldPredTable, PredId, OldPredInfo),
        pred_info_get_procedures(OldPredInfo, OldProcTable),
        map.keys(OldProcTable, OldProcIds),
        list.foldl(copy_proc_body(OldProcTable), OldProcIds,
            ProcTable0, ProcTable),
        pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
        map.set(PredTable0, PredId, PredInfo, PredTable)
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
    svmap.set(ProcId, ProcInfo, !ProcTable).

:- func should_modecheck_pred(pred_info) = bool.

should_modecheck_pred(PredInfo) = ShouldModeCheck :-
    (
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
    ->
        ShouldModeCheck = no
    ;
        ShouldModeCheck = yes
    ).

:- pred maybe_modecheck_pred(how_to_check_goal::in, may_change_called_proc::in,
    pred_id::in, module_info::in, module_info::out, bool::in, bool::out,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_modecheck_pred(WhatToCheck, MayChangeCalledProc, PredId,
        !ModuleInfo, !Changed, !Specs) :-
    module_info_preds(!.ModuleInfo, Preds0),
    map.lookup(Preds0, PredId, PredInfo0),
    ShouldModeCheck = should_modecheck_pred(PredInfo0),
    (
        ShouldModeCheck = no
    ;
        ShouldModeCheck = yes,
        trace [io(!IO)] (
            write_modes_progress_message(!.ModuleInfo, WhatToCheck,
                PredId, PredInfo0, !IO)
        ),
        modecheck_pred_mode_2(PredId, PredInfo0, WhatToCheck,
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
            module_info_remove_predid(PredId, !ModuleInfo)
        ;
            ContainsError = no
        ),

        globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
        trace [io(!IO)] (
            maybe_report_stats(Statistics, !IO)
        )
    ).

:- pred write_modes_progress_message(module_info::in, how_to_check_goal::in,
    pred_id::in, pred_info::in, io::di, io::uo) is det.

write_modes_progress_message(ModuleInfo, WhatToCheck, PredId, PredInfo, !IO) :-
    pred_info_get_markers(PredInfo, Markers),
    ( check_marker(Markers, marker_infer_modes) ->
        (
            WhatToCheck = check_modes,
            Msg = "% Mode-analysing "
        ;
            WhatToCheck = check_unique_modes,
            Msg = "% Unique-mode-analysing "
        )
    ;
        (
            WhatToCheck = check_modes,
            Msg = "% Mode-checking "
        ;
            WhatToCheck = check_unique_modes,
            Msg = "% Unique-mode-checking "
        )
    ),
    write_pred_progress_message(Msg, PredId, ModuleInfo, !IO).

%-----------------------------------------------------------------------------%

:- pred modecheck_pred_mode_2(pred_id::in, pred_info::in,
    how_to_check_goal::in, may_change_called_proc::in,
    module_info::in, module_info::out, bool::in, bool::out,
    list(error_spec)::out, list(error_spec)::out) is det.

modecheck_pred_mode_2(PredId, PredInfo0, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !Changed, DeclSpecs, ProcSpecs) :-
    (
        WhatToCheck = check_modes,
        pred_info_get_procedures(PredInfo0, ProcTable),
        (
            some [ProcInfo] (
                map.member(ProcTable, _ProcId, ProcInfo),
                proc_info_get_maybe_declared_argmodes(ProcInfo, yes(_))
            )
        ->
            % There was at least one declared mode for this procedure.
            DeclSpecs = []
        ;
            % There were no declared modes for this procedure.
            DeclSpecs = maybe_report_error_no_modes(!.ModuleInfo, PredId,
                PredInfo0)
        )
    ;
        WhatToCheck = check_unique_modes,
        DeclSpecs = []
    ),
    % Note that we use pred_info_procids rather than pred_info_all_procids
    % here, which means that we don't process modes that have already been
    % inferred as invalid.
    ProcIds = pred_info_procids(PredInfo0),
    modecheck_procs(ProcIds, PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !Changed, init_error_spec_accumulator, SpecsAcc),
    ProcSpecs = error_spec_accumulator_to_list(SpecsAcc).

    % Iterate over the list of modes for a predicate.
    %
:- pred modecheck_procs(list(proc_id)::in, pred_id::in, how_to_check_goal::in,
    may_change_called_proc::in, module_info::in, module_info::out,
    bool::in, bool::out,
    error_spec_accumulator::in, error_spec_accumulator::out) is det.

modecheck_procs([], _PredId, _, _, !ModuleInfo, !Changed, !Specs).
modecheck_procs([ProcId | ProcIds], PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !Changed, !SpecsAcc) :-
    % Mode-check that mode of the predicate.
    maybe_modecheck_proc(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !Changed, ProcSpecs),
    accumulate_error_specs_for_proc(ProcSpecs, !SpecsAcc),
    % Recursively process the remaining modes.
    modecheck_procs(ProcIds, PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !Changed, !SpecsAcc).

%-----------------------------------------------------------------------------%

    % Mode-check the code for predicate in a given mode.
    %
modecheck_proc(ProcId, PredId, !ModuleInfo, Specs, Changed) :-
    modecheck_proc_general(ProcId, PredId, check_modes, may_change_called_proc,
        !ModuleInfo, Specs, Changed).

modecheck_proc_general(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, Specs, Changed) :-
    maybe_modecheck_proc(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, no, Changed, Specs).

:- pred maybe_modecheck_proc(proc_id::in, pred_id::in, how_to_check_goal::in,
    may_change_called_proc::in, module_info::in, module_info::out,
    bool::in, bool::out, list(error_spec)::out) is det.

maybe_modecheck_proc(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !Changed, Specs) :-
    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        _PredInfo0, ProcInfo0),
    proc_info_get_can_process(ProcInfo0, CanProcess),
    (
        CanProcess = no,
        Specs = []
    ;
        CanProcess = yes,
        do_modecheck_proc(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
            !ModuleInfo, ProcInfo0, ProcInfo, !Changed, Specs),
        module_info_preds(!.ModuleInfo, Preds1),
        map.lookup(Preds1, PredId, PredInfo1),
        pred_info_get_procedures(PredInfo1, Procs1),
        map.set(Procs1, ProcId, ProcInfo, Procs),
        pred_info_set_procedures(Procs, PredInfo1, PredInfo),
        map.set(Preds1, PredId, PredInfo, Preds),
        module_info_set_preds(Preds, !ModuleInfo)
    ).

:- pred do_modecheck_proc(proc_id::in, pred_id::in, how_to_check_goal::in,
    may_change_called_proc::in, module_info::in, module_info::out,
    proc_info::in, proc_info::out, bool::in, bool::out,
    list(error_spec)::out) is det.

do_modecheck_proc(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
        !ModuleInfo, !ProcInfo, !Changed, ErrorAndWarningSpecs) :-
    % Extract the useful fields in the proc_info.
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_argmodes(!.ProcInfo, ArgModes0),
    proc_info_arglives(!.ProcInfo, !.ModuleInfo, ArgLives0),
    proc_info_get_goal(!.ProcInfo, Body0),

    % We use the context of the first clause, unless there weren't any clauses
    % at all, in which case we use the context of the mode declaration.
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
    get_clause_list(ClausesRep, Clauses),
    (
        Clauses = [FirstClause | _],
        FirstClause = clause(_, _, _, Context)
    ;
        Clauses = [],
        proc_info_get_context(!.ProcInfo, Context)
    ),

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
        set.list_to_set(LiveVarsList, LiveVars),

        % Initialize the mode info.
        mode_info_init(!.ModuleInfo, PredId, ProcId, Context, LiveVars,
            InstMap0, WhatToCheck, MayChangeCalledProc, !:ModeInfo),
        mode_info_set_changed_flag(!.Changed, !ModeInfo),

        pred_info_get_markers(PredInfo, Markers),
        ( check_marker(Markers, marker_infer_modes) ->
            InferModes = yes
        ;
            InferModes = no
        ),
        mode_list_get_final_insts(!.ModuleInfo, ArgModes0, ArgFinalInsts0),

        modecheck_proc_body(!.ModuleInfo, WhatToCheck, InferModes,
            Markers, Body0, Body, HeadVars, InstMap0,
            ArgFinalInsts0, ArgFinalInsts, !ModeInfo),

        mode_info_get_errors(!.ModeInfo, ModeErrors),
        (
            InferModes = yes,
            % For inferred predicates, we don't report the error(s) here;
            % instead we just save them in the proc_info, thus marking that
            % procedure as invalid.
            !ProcInfo ^ mode_errors := ModeErrors,
            ErrorAndWarningSpecs = []
        ;
            InferModes = no,
            AllErrorSpecs = list.map(mode_error_info_to_spec(!.ModeInfo),
                ModeErrors),

            % We only return the first error, because there could be a
            % large number of mode errors and usually only one is needed to
            % diagnose the problem.

            (
                AllErrorSpecs = [ErrorSpec | _],
                ErrorSpecs = [ErrorSpec]
            ;
                AllErrorSpecs = [],
                ErrorSpecs = []
            ),
            mode_info_get_warnings(!.ModeInfo, ModeWarnings),
            WarningSpecs = list.map(mode_warning_info_to_spec(!.ModeInfo),
                ModeWarnings),
            ErrorAndWarningSpecs = ErrorSpecs ++ WarningSpecs
        ),

        % Save away the results.
        inst_lists_to_mode_list(ArgInitialInsts, ArgFinalInsts, ArgModes),
        mode_info_get_changed_flag(!.ModeInfo, !:Changed),
        mode_info_get_module_info(!.ModeInfo, !:ModuleInfo),
        mode_info_get_varset(!.ModeInfo, VarSet),
        % VarTypes may differ from VarTypes0, since mode checking can
        % add new variables (e.g. when handling calls in implied modes).
        mode_info_get_var_types(!.ModeInfo, VarTypes),
        mode_info_get_need_to_requantify(!.ModeInfo, NeedToRequantify),
        proc_info_set_goal(Body, !ProcInfo),
        proc_info_set_varset(VarSet, !ProcInfo),
        proc_info_set_vartypes(VarTypes, !ProcInfo),
        proc_info_set_argmodes(ArgModes, !ProcInfo),
        (
            NeedToRequantify = do_not_need_to_requantify
        ;
            NeedToRequantify = need_to_requantify,
            requantify_proc_general(ordinary_nonlocals_maybe_lambda, !ProcInfo)
        )
    ).

:- pred modecheck_proc_body(module_info::in, how_to_check_goal::in,
    bool::in, pred_markers::in, hlds_goal::in, hlds_goal::out,
    list(prog_var)::in, instmap::in, list(mer_inst)::in, list(mer_inst)::out,
    mode_info::in, mode_info::out) is det.

modecheck_proc_body(ModuleInfo, WhatToCheck, InferModes, Markers,
        Body0, Body, HeadVars, InstMap0, ArgFinalInsts0, ArgFinalInsts,
        ModeInfo0, ModeInfo) :-
    do_modecheck_proc_body(ModuleInfo, WhatToCheck, InferModes, Markers,
        Body0, Body1, HeadVars, InstMap0, ArgFinalInsts0, ArgFinalInsts1,
        ModeInfo0, ModeInfo1),
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
                Markers, Body0, Body, HeadVars, InstMap0,
                ArgFinalInsts0, ArgFinalInsts, ModeInfo2, ModeInfo)
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
    bool::in, pred_markers::in, hlds_goal::in, hlds_goal::out,
    list(prog_var)::in, instmap::in, list(mer_inst)::in, list(mer_inst)::out,
    mode_info::in, mode_info::out) is det.

do_modecheck_proc_body(ModuleInfo, WhatToCheck, InferModes, Markers,
        Body0, Body, HeadVars, InstMap0, ArgFinalInsts0, ArgFinalInsts,
        !ModeInfo) :-
    (
        InferModes = no,
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
        mode_info_get_var_types(!.ModeInfo, VarTypes0),
        SolverNonLocals = list.filter(is_solver_var(VarTypes0, ModuleInfo),
            set.to_sorted_list(BodyNonLocals)),
        SolverNonLocals = []
    ->
        BodyContext = goal_info_get_context(BodyGoalInfo0),
        term.context_init(EmptyContext),
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
                list.map_foldl(
                    modecheck_clause_disj(HeadVars, InstMap0, ArgFinalInsts0),
                    Disjuncts2, Disjuncts, !ModeInfo),
                NewGoalExpr = disj(Disjuncts)
            ;
                ClausesForm0 = clause_switch(SwitchVar, CanFail, Cases1),
                list.map_foldl(
                    modecheck_clause_switch(HeadVars, InstMap0,
                        ArgFinalInsts0, SwitchVar),
                    Cases1, Cases, !ModeInfo),
                NewGoalExpr = switch(SwitchVar, CanFail, Cases)
            )
        ;
            WhatToCheck = check_unique_modes,
            mode_info_get_nondet_live_vars(!.ModeInfo, NondetLiveVars0),
            Detism = goal_info_get_determinism(BodyGoalInfo0),
            NonLocals = goal_info_get_nonlocals(BodyGoalInfo0),
            ( determinism_components(Detism, _, at_most_many) ->
                true
            ;
                mode_info_set_nondet_live_vars(bag.init, !ModeInfo)
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
                list.map_foldl(
                    unique_modecheck_clause_disj(HeadVars, InstMap0,
                        ArgFinalInsts0, Detism, NonLocals, NondetLiveVars0),
                    Disjuncts2, Disjuncts, !ModeInfo),
                NewGoalExpr = disj(Disjuncts)
            ;
                ClausesForm0 = clause_switch(SwitchVar, CanFail, Cases1),
                list.map_foldl(
                    unique_modecheck_clause_switch(HeadVars, InstMap0,
                        ArgFinalInsts0, SwitchVar),
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
    ;
        % Modecheck the procedure body as a single goal.
        (
            WhatToCheck = check_modes,
            modecheck_goal(Body0, Body1, !ModeInfo)
        ;
            WhatToCheck = check_unique_modes,
            unique_modes_check_goal(Body0, Body1, !ModeInfo)
        ),

        % Check that final insts match those specified in the mode declaration.
        modecheck_final_insts(HeadVars, InferModes,
            ArgFinalInsts0, ArgFinalInsts, Body1, Body, !ModeInfo)
    ).

%-----------------------------------------------------------------------------%

    % Do mode analysis of the queued procedures. If the first argument is
    % `unique_mode_check', then also go on and do full determinism analysis
    % and unique mode analysis on them as well. The pred_table arguments
    % are used to store copies of the procedure bodies before unique mode
    % analysis, so that we can restore them before doing the next analysis
    % pass.
    %
:- pred modecheck_queued_procs(how_to_check_goal::in,
    pred_table::in, pred_table::out, module_info::in, module_info::out,
    bool::out, list(error_spec)::out) is det.

modecheck_queued_procs(HowToCheckGoal, !OldPredTable, !ModuleInfo, Changed,
        Specs) :-
    module_info_get_proc_requests(!.ModuleInfo, Requests0),
    get_req_queue(Requests0, RequestQueue0),
    ( queue.get(RequestQueue0, PredProcId, RequestQueue1) ->
        set_req_queue(RequestQueue1, Requests0, Requests1),
        module_info_set_proc_requests(Requests1, !ModuleInfo),

        % Check that the procedure is valid (i.e. type-correct), before
        % we attempt to do mode analysis on it. This check is necessary
        % to avoid internal errors caused by doing mode analysis on
        % type-incorrect code.
        % XXX inefficient! This is O(N*M).

        PredProcId = proc(PredId, _ProcId),
        module_info_predids(ValidPredIds, !ModuleInfo),
        ( list.member(PredId, ValidPredIds) ->
            trace [io(!IO)] (
                queued_proc_progress_message(!.ModuleInfo, PredProcId,
                    HowToCheckGoal, !IO)
            ),
            modecheck_queued_proc(HowToCheckGoal, PredProcId,
                !OldPredTable, !ModuleInfo, HeadChanged, HeadSpecs)
        ;
            HeadSpecs = [],
            HeadChanged = no
        ),
        modecheck_queued_procs(HowToCheckGoal, !OldPredTable, !ModuleInfo,
            TailChanged, TailSpecs),
        bool.or(HeadChanged, TailChanged, Changed),
        Specs = HeadSpecs ++ TailSpecs
    ;
        Changed = no,
        Specs = []
    ).

:- pred queued_proc_progress_message(module_info::in, pred_proc_id::in,
    how_to_check_goal::in, io::di, io::uo) is det.

queued_proc_progress_message(ModuleInfo, PredProcId, HowToCheckGoal, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        (
            HowToCheckGoal = check_modes,
            io.write_string("% Mode-analyzing ", !IO)
        ;
            HowToCheckGoal = check_unique_modes,
            io.write_string("% Analyzing modes, determinism, " ++
                "and unique-modes for\n% ", !IO)
        ),
        hlds_out.write_pred_proc_id(ModuleInfo, PredProcId, !IO),
        io.write_string("\n", !IO)
%       /*****
%       mode_list_get_initial_insts(Modes, ModuleInfo1,
%           InitialInsts),
%       io.write_string("% Initial insts: `", !IO),
%       varset.init(InstVarSet),
%       mercury_output_inst_list(InitialInsts, InstVarSet, !IO),
%       io.write_string("'\n", !IO)
%       *****/
    ;
        VeryVerbose = no
    ).

:- pred modecheck_queued_proc(how_to_check_goal::in, pred_proc_id::in,
    pred_table::in, pred_table::out, module_info::in, module_info::out,
    bool::out, list(error_spec)::out) is det.

modecheck_queued_proc(HowToCheckGoal, PredProcId, !OldPredTable, !ModuleInfo,
        !:Changed, Specs) :-
    % Mark the procedure as ready to be processed.
    PredProcId = proc(PredId, ProcId),
    module_info_preds(!.ModuleInfo, Preds0),
    map.lookup(Preds0, PredId, PredInfo0),
    pred_info_get_procedures(PredInfo0, Procs0),
    map.lookup(Procs0, ProcId, ProcInfo0),
    proc_info_set_can_process(yes, ProcInfo0, ProcInfo1),
    map.det_update(Procs0, ProcId, ProcInfo1, Procs1),
    pred_info_set_procedures(Procs1, PredInfo0, PredInfo1),
    map.det_update(Preds0, PredId, PredInfo1, Preds1),
    module_info_set_preds(Preds1, !ModuleInfo),

    % Modecheck the procedure.
    modecheck_proc(ProcId, PredId, !ModuleInfo, ModeSpecs, !:Changed),

    module_info_get_globals(!.ModuleInfo, Globals),
    ModeErrors = contains_errors(Globals, ModeSpecs),
    (
        ModeErrors = yes,
        module_info_remove_predid(PredId, !ModuleInfo),
        Specs = ModeSpecs
    ;
        ModeErrors = no,
        (
            HowToCheckGoal = check_unique_modes,
            detect_switches_in_proc(ProcId, PredId, !ModuleInfo),
            detect_cse_in_proc(ProcId, PredId, !ModuleInfo),
            determinism_check_proc(ProcId, PredId, !ModuleInfo, DetismSpecs),
            expect(unify(DetismSpecs, []), this_file,
                "modecheck_queued_proc: found detism error"),
            save_proc_info(ProcId, PredId, !.ModuleInfo, !OldPredTable),
            unique_modes_check_proc(ProcId, PredId, !ModuleInfo,
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
:- pred save_proc_info(proc_id::in, pred_id::in, module_info::in,
    pred_table::in, pred_table::out) is det.

save_proc_info(ProcId, PredId, ModuleInfo, !OldPredTable) :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        _PredInfo, ProcInfo),
    map.lookup(!.OldPredTable, PredId, OldPredInfo0),
    pred_info_get_procedures(OldPredInfo0, OldProcTable0),
    map.set(OldProcTable0, ProcId, ProcInfo, OldProcTable),
    pred_info_set_procedures(OldProcTable, OldPredInfo0, OldPredInfo),
    map.det_update(!.OldPredTable, PredId, OldPredInfo, !:OldPredTable).

%-----------------------------------------------------------------------------%

:- type clause_form
    --->    clause_disj(list(hlds_goal))
    ;       clause_switch(prog_var, can_fail, list(case)).

:- pred modecheck_clause_disj(list(prog_var)::in, instmap::in,
    list(mer_inst)::in, hlds_goal::in, hlds_goal::out,
    mode_info::in, mode_info::out) is det.

modecheck_clause_disj(HeadVars, InstMap0, ArgFinalInsts0, Disjunct0, Disjunct,
        !ModeInfo) :-
    mode_info_set_instmap(InstMap0, !ModeInfo),
    modecheck_goal(Disjunct0, Disjunct1, !ModeInfo),

    % Check that final insts match those specified in the mode declaration.
    modecheck_final_insts(HeadVars, no, ArgFinalInsts0,
        _ArgFinalInsts, Disjunct1, Disjunct, !ModeInfo).

:- pred modecheck_clause_switch(list(prog_var)::in, instmap::in,
    list(mer_inst)::in, prog_var::in, case::in, case::out,
    mode_info::in, mode_info::out) is det.

modecheck_clause_switch(HeadVars, InstMap0, ArgFinalInsts0, Var, Case0, Case,
        !ModeInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    mode_info_set_instmap(InstMap0, !ModeInfo),

    modecheck_functors_test(Var, MainConsId, OtherConsIds, !ModeInfo),

    % Modecheck this case (if it is reachable).
    mode_info_get_instmap(!.ModeInfo, InstMap1),
    ( instmap_is_reachable(InstMap1) ->
        modecheck_goal(Goal0, Goal1, !ModeInfo),
        mode_info_get_instmap(!.ModeInfo, InstMap)
    ;
        % We should not mode-analyse the goal, since it is unreachable.
        % Instead we optimize the goal away, so that later passes
        % won't complain about it not having mode information.
        Goal1 = true_goal,
        InstMap = InstMap1
    ),

    % Don't lose the information added by the functor test above.
    fixup_instmap_switch_var(Var, InstMap0, InstMap, Goal1, Goal2),

    % Check that final insts match those specified in the mode declaration.
    modecheck_final_insts(HeadVars, no, ArgFinalInsts0,
        _ArgFinalInsts, Goal2, Goal, !ModeInfo),
    Case = case(MainConsId, OtherConsIds, Goal).

:- pred unique_modecheck_clause_disj(list(prog_var)::in, instmap::in,
    list(mer_inst)::in, determinism::in, set(prog_var)::in, bag(prog_var)::in,
    hlds_goal::in, hlds_goal::out, mode_info::in, mode_info::out) is det.

unique_modecheck_clause_disj(HeadVars, InstMap0, ArgFinalInsts0, DisjDetism,
        DisjNonLocals, NondetLiveVars0, Disjunct0, Disjunct, !ModeInfo) :-
    mode_info_set_instmap(InstMap0, !ModeInfo),
    mode_info_set_nondet_live_vars(NondetLiveVars0, !ModeInfo),
    unique_modes.prepare_for_disjunct(Disjunct0, DisjDetism, DisjNonLocals,
        !ModeInfo),
    unique_modes_check_goal(Disjunct0, Disjunct1, !ModeInfo),

    % Check that final insts match those specified in the mode declaration.
    modecheck_final_insts(HeadVars, no, ArgFinalInsts0,
        _ArgFinalInsts, Disjunct1, Disjunct, !ModeInfo).

:- pred unique_modecheck_clause_switch(list(prog_var)::in, instmap::in,
    list(mer_inst)::in, prog_var::in, case::in, case::out,
    mode_info::in, mode_info::out) is det.

unique_modecheck_clause_switch(HeadVars, InstMap0, ArgFinalInsts0, Var,
        Case0, Case, !ModeInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    mode_info_set_instmap(InstMap0, !ModeInfo),

    modecheck_functors_test(Var, MainConsId, OtherConsIds, !ModeInfo),

    mode_info_get_instmap(!.ModeInfo, InstMap1),
    ( instmap_is_reachable(InstMap1) ->
        unique_modes_check_goal(Goal0, Goal1, !ModeInfo)
    ;
        % We should not mode-analyse the goal, since it is unreachable.
        % Instead we optimize the goal away, so that later passes
        % won't complain about it not having mode information.
        Goal1 = true_goal
    ),

    % Don't lose the information added by the functor test above.
    mode_info_get_instmap(!.ModeInfo, InstMap),
    fixup_instmap_switch_var(Var, InstMap0, InstMap, Goal1, Goal2),

    % Check that final insts match those specified in the mode declaration.
    modecheck_final_insts(HeadVars, no, ArgFinalInsts0, _ArgFinalInsts,
        Goal2, Goal, !ModeInfo),
    Case = case(MainConsId, OtherConsIds, Goal).

%-----------------------------------------------------------------------------%

    % This is modecheck_final_insts for a lambda expression.
    %
modecheck_lambda_final_insts(HeadVars, ArgFinalInsts, !Goal, !ModeInfo) :-
    % For lambda expressions, modes must always be declared;
    % we never infer them.
    InferModes = no,
    modecheck_final_insts(HeadVars, InferModes, ArgFinalInsts,
        _NewFinalInsts, !Goal, !ModeInfo).

    % Check that the final insts of the head vars match their expected insts.
    %
:- pred modecheck_final_insts(list(prog_var)::in, bool::in,
    list(mer_inst)::in, list(mer_inst)::out, hlds_goal::in, hlds_goal::out,
    mode_info::in, mode_info::out) is det.

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
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    instmap_lookup_vars(InstMap, HeadVars, VarFinalInsts1),
    map.apply_to_list(HeadVars, VarTypes, ArgTypes),
    (
        InferModes = yes,
        normalise_insts(ModuleInfo, ArgTypes, VarFinalInsts1, VarFinalInsts2),

        % Make sure we set the final insts of any variables which
        % we assumed were dead to `clobbered'.

        mode_info_get_pred_id(!.ModeInfo, PredId),
        mode_info_get_proc_id(!.ModeInfo, ProcId),
        module_info_proc_info(ModuleInfo, PredId, ProcId, ProcInfo),
        proc_info_arglives(ProcInfo, ModuleInfo, ArgLives),
        maybe_clobber_insts(VarFinalInsts2, ArgLives, FinalInsts),
        check_final_insts(HeadVars, FinalInsts0, FinalInsts, InferModes, 1,
            ModuleInfo, Body0, Body, no, Changed1, !ModeInfo),
        mode_info_get_changed_flag(!.ModeInfo, Changed2),
        bool.or_list([Changed0, Changed1, Changed2], Changed),
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
    unexpected(this_file, "maybe_clobber_insts: length mismatch").
maybe_clobber_insts([_ | _], [], _) :-
    unexpected(this_file, "maybe_clobber_insts: length mismatch").
maybe_clobber_insts([], [], []).
maybe_clobber_insts([Inst0 | Insts0], [IsLive | IsLives], [Inst | Insts]) :-
    (
        IsLive = is_dead,
        Inst = ground(clobbered, none)
    ;
        IsLive = is_live,
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
        map.lookup(VarTypes, Var, Type),
        (
            inst_matches_final_typed(VarInst, Inst, Type, ModuleInfo)
        ->
            true
        ;
            !:Changed = yes,
            (
                % Insert a call to the appropriate solver type initialisation
                % predicate if:
                %
                % (a) this is a solver type with inst `free' that should
                %     have inst `any'.
                %
                % (b) this is a solver type that allows automatic
                %     initialisation.
                %
                % (c) the option `--solver-type-auto-init' is enabled.
                %
                inst_match.inst_is_free(ModuleInfo, VarInst),
                inst_match.inst_is_any(ModuleInfo, Inst),
                type_is_solver_type_with_auto_init(ModuleInfo, Type),
                mode_info_solver_init_is_supported(!.ModeInfo)
            ->
                prepend_initialisation_call(Var, Type, VarInst, !Goal,
                    !ModeInfo)
            ;
                (
                    % If we're inferring the mode, then don't report an error,
                    % just set changed to yes to make sure that we will do
                    % another fixpoint pass.
                    InferModes = yes
                ;
                    InferModes = no,
                    % XXX This might need to be reconsidered now we have
                    % unique modes.
                    ( inst_matches_initial(VarInst, Inst, Type, ModuleInfo) ->
                        Reason = too_instantiated
                    ; inst_matches_initial(Inst, VarInst, Type, ModuleInfo) ->
                        Reason = not_instantiated_enough
                    ;
                        % I don't think this can happen. But just in case...
                        Reason = wrongly_instantiated
                    ),
                    set.init(WaitingVars),
                    ModeError = mode_error_final_inst(ArgNum, Var, VarInst,
                        Inst, Reason),
                    mode_info_error(WaitingVars, ModeError, !ModeInfo)
                )
            )
        ),
        check_final_insts(VarsTail, InstsTail, VarInstsTail,
            InferModes, ArgNum + 1, ModuleInfo, !Goal, !Changed, !ModeInfo)
    ;
        unexpected(this_file, "check_final_insts: length mismatch")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Check that the evaluation method is OK for the given mode(s).
    % We also check the mode of main/2 here.
    %
:- pred check_eval_methods(module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_eval_methods(!ModuleInfo, !Specs) :-
    module_info_predids(PredIds, !ModuleInfo),
    pred_check_eval_methods(!.ModuleInfo, PredIds, !Specs).

:- pred pred_check_eval_methods(module_info::in, list(pred_id)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

pred_check_eval_methods(_, [], !Specs).
pred_check_eval_methods(ModuleInfo, [PredId | PredIds], !Specs) :-
    module_info_preds(ModuleInfo, Preds),
    map.lookup(Preds, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    proc_check_eval_methods(ModuleInfo, PredId, ProcIds, !Specs),
    pred_check_eval_methods(ModuleInfo, PredIds, !Specs).

:- pred proc_check_eval_methods(module_info::in, pred_id::in,
    list(proc_id)::in, list(error_spec)::in, list(error_spec)::out) is det.

proc_check_eval_methods(_, _, [], !Specs).
proc_check_eval_methods(ModuleInfo, PredId, [ProcId | ProcIds], !Specs) :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    proc_info_get_eval_method(ProcInfo, EvalMethod),
    proc_info_get_argmodes(ProcInfo, Modes),
    (
        eval_method_requires_ground_args(EvalMethod) = yes,
        \+ only_fully_in_out_modes(Modes, ModuleInfo)
    ->
        GroundArgsSpec = report_eval_method_requires_ground_args(ProcInfo),
        !:Specs = [GroundArgsSpec | !.Specs]
    ;
        true
    ),
    (
        eval_method_destroys_uniqueness(EvalMethod) = yes,
        \+ only_nonunique_modes(Modes, ModuleInfo)
    ->
        UniquenessSpec = report_eval_method_destroys_uniqueness(ProcInfo),
        !:Specs = [UniquenessSpec | !.Specs]
    ;
        true
    ),
    (
        pred_info_name(PredInfo) = "main",
        pred_info_orig_arity(PredInfo) = 2,
        pred_info_is_exported(PredInfo),
        \+ check_mode_of_main(Modes, ModuleInfo)
    ->
        MainSpec = report_wrong_mode_for_main(ProcInfo),
        !:Specs = [MainSpec | !.Specs]
    ;
        true
    ),
    proc_check_eval_methods(ModuleInfo, PredId, ProcIds, !Specs).

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

:- func report_eval_method_requires_ground_args(proc_info) = error_spec.

report_eval_method_requires_ground_args(ProcInfo) = Spec :-
    proc_info_get_eval_method(ProcInfo, EvalMethod),
    proc_info_get_context(ProcInfo, Context),
    EvalMethodS = eval_method_to_string(EvalMethod),
    MainPieces = [words("Sorry, not implemented:"),
        fixed("`pragma " ++ EvalMethodS ++ "'"),
        words("declaration not allowed for procedure"),
        words("with partially instantiated modes."), nl],
    VerbosePieces = [words("Tabling of predicates/functions"),
        words("with partially instantiated modes"),
        words("is not currently implemented."), nl],
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [Msg]).

:- func report_eval_method_destroys_uniqueness(proc_info) = error_spec.

report_eval_method_destroys_uniqueness(ProcInfo) = Spec :-
    proc_info_get_eval_method(ProcInfo, EvalMethod),
    proc_info_get_context(ProcInfo, Context),
    EvalMethodS = eval_method_to_string(EvalMethod),
    MainPieces = [words("Error:"),
        fixed("`pragma " ++ EvalMethodS ++ "'"),
        words("declaration not allowed for procedure"),
        words("with unique modes."), nl],
    VerbosePieces =
        [words("Tabling of predicates/functions with unique modes"),
        words("is not allowed as this would lead to a copying"),
        words("of the unique arguments which would result"),
        words("in them no longer being unique."), nl],
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [Msg]).

:- func report_wrong_mode_for_main(proc_info) = error_spec.

report_wrong_mode_for_main(ProcInfo) = Spec :-
    proc_info_get_context(ProcInfo, Context),
    Pieces = [words("Error: main/2 must have mode `(di, uo)'."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [Msg]).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "modes.m".

%-----------------------------------------------------------------------------%
