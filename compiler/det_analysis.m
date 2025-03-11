%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: det_analysis.m - the determinism analysis pass.
% Main authors: conway, fjh, zs.
%
% This pass has three components.
%
% - We partition the procedures that need determinism analysis into
%   the procedures that have determinism declarations (call these
%   DeclaredProcs), and the procedures that don't (call these UndeclaredProcs).
%   (Procedures imported from other modules do not need determinism analysis,
%   since we have their declarations and do *not* have their definitions.
%   And some procedures created by the compiler already have their determinism
%   information filled in.)
%
% - We perform a fixpoint iteration on the procedures in UndeclaredProcs.
%   Each iteration of this fixpoint process infers the determinism of
%   all these procedures, assuming that the declared determinisms of the
%   DeclaredProcs and the currently recorded inferred determinisms of the
%   UndeclaredProcs are all correct. If these assumptions are all correct,
%   this will compute the same determinism for all the UndeclaredProcs
%   as their currently recorded inferred determinisms. This is the fixpoint,
%   since any further iterations would get the same result.
%
%   The inferred determinism fields of the proc_infos of UndeclaredProcs
%   initially contain "erroneous", the determinism that makes the most
%   assertions about the number of the solutions of the procedure.
%   (The possible assertions are "has at least one solution",
%   "has at most one solution" and "has at most zero solutions".)
%   Each iteration before we reach the fixpoint will show one or more
%   of these tentative assertions to be unjustified, and we then delete
%   these assertions from their recorded inferred determinism.
%   Since we have a finite number of assertions (three) for each procedure,
%   and each iteration before the fixpoint will delete at least one,
%   the fixpoint iteration is guaranteed to terminate.
%
% - We then infer the determinism of all the DeclaredProcs, and report
%   any results that are not at least as deterministic as their declarations.
%
%---------------------------------------------------------------------------%

:- module check_hlds.det_analysis.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % Perform determinism inference for local predicates with no determinism
    % declarations, and determinism checking for all other predicates.
    %
:- pred determinism_pass(io.text_output_stream::in,
    list(error_spec)::out, module_info::in, module_info::out) is det.

    % Check the determinism of a single procedure. Works only if the
    % determinisms of the procedures it calls have already been inferred.
    %
:- pred determinism_check_proc(io.text_output_stream::in,
    pred_id::in, proc_id::in, list(error_spec)::out,
    module_info::in, module_info::out) is det.

    % Infer the determinism of a procedure.
    %
:- pred det_infer_proc_ignore_msgs(io.text_output_stream::in,
    pred_id::in, proc_id::in, module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_infer_goal.
:- import_module check_hlds.det_report.
:- import_module check_hlds.det_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module libs.options.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

determinism_pass(ProgressStream, Specs, !ModuleInfo) :-
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    module_info_get_valid_pred_ids(!.ModuleInfo, ValidPredIds0),
    determinism_declarations(PredIdTable0, ValidPredIds0,
        DeclaredProcs, UndeclaredProcs, CompGenProcs, ImportedProcs),
    list.foldl(set_non_inferred_proc_determinism, CompGenProcs, !ModuleInfo),
    list.foldl(set_non_inferred_proc_determinism, ImportedProcs, !ModuleInfo),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, debug_det, Debug),
    (
        UndeclaredProcs = [],
        InferenceSpecs = []
    ;
        UndeclaredProcs = [_ | _],
        trace [io(!IO)] (
            maybe_write_string(ProgressStream, Verbose,
                "% Doing determinism inference...\n", !IO)
        ),
        determinism_inference_to_fixpoint(ProgressStream, Debug,
            UndeclaredProcs, InferenceSpecs, !ModuleInfo),
        trace [io(!IO)] (
            maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO)
        )
    ),
    trace [io(!IO)] (
        maybe_write_string(ProgressStream, Verbose,
            "% Doing determinism checking...\n", !IO)
    ),
    determinism_final_pass(ProgressStream, Debug,
        DeclaredProcs, UndeclaredProcs, ImportedProcs,
        FinalSpecs, !ModuleInfo),
    Specs = InferenceSpecs ++ FinalSpecs,
    trace [io(!IO)] (
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO)
    ).

determinism_check_proc(ProgressStream, PredId, ProcId, !:Specs, !ModuleInfo) :-
    % Does for one procedure what determinism_final_pass does
    % for all determinism-checked procedures.
    PredProcId = proc(PredId, ProcId),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_det, Debug),
    det_infer_proc(ProgressStream, Debug, proc(PredId, ProcId),
        [], !:Specs, unchanged, _, !ModuleInfo),
    check_determinism_of_proc(ProgressStream, PredProcId, !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred determinism_inference_to_fixpoint(io.text_output_stream::in, bool::in,
    list(pred_proc_id)::in, list(error_spec)::out,
    module_info::in, module_info::out) is det.

determinism_inference_to_fixpoint(ProgressStream, Debug, PredProcIds, Specs,
        !ModuleInfo) :-
    % Iterate until a fixpoint is reached. This can be expensive if a module
    % has many predicates with undeclared determinisms. If this ever becomes
    % a problem, we should switch to doing iterations only on strongly
    % connected components of the dependency graph.
    determinism_inference_one_pass(ProgressStream, Debug, PredProcIds,
        [], Specs1, unchanged, Changed, !ModuleInfo),
    trace [io(!IO)] (
        maybe_write_string(ProgressStream, Debug,
            "% Inference pass complete\n", !IO)
    ),
    (
        Changed = changed,
        % We have not yet arrived at a fixpoint. Therefore the messages in
        % Specs1 are based on possibly non-final determinisms of some
        % procedures, which means that it is NOT safe to return them
        % to be printed. Instead, we will compute them again from more
        % up-to-date determinism information.
        disable_warning [suspicious_recursion] (
            determinism_inference_to_fixpoint(ProgressStream, Debug,
                PredProcIds, Specs, !ModuleInfo)
        )
    ;
        Changed = unchanged,
        % We have arrived at a fixpoint. Therefore all the messages we have
        % are based on the final determinisms of all procedures, which means
        % it is safe to return them to be printed.
        Specs = Specs1
    ).

:- pred determinism_inference_one_pass(io.text_output_stream::in, bool::in,
    list(pred_proc_id)::in, list(error_spec)::in, list(error_spec)::out,
    maybe_changed::in, maybe_changed::out,
    module_info::in, module_info::out) is det.

determinism_inference_one_pass(_, _, [], !Specs, !Changed, !ModuleInfo).
determinism_inference_one_pass(ProgressStream, Debug,
        [PredProcId | PredProcIds], !Specs, !Changed, !ModuleInfo) :-
    det_infer_proc(ProgressStream, Debug, PredProcId,
        !Specs, !Changed, !ModuleInfo),
    determinism_inference_one_pass(ProgressStream, Debug, PredProcIds,
        !Specs, !Changed, !ModuleInfo).

:- pred determinism_final_pass(io.text_output_stream::in, bool::in,
    list(pred_proc_id)::in, list(pred_proc_id)::in, list(pred_proc_id)::in,
    list(error_spec)::out, module_info::in, module_info::out) is det.

determinism_final_pass(ProgressStream, Debug,
        DeclaredProcs, UndeclaredProcs, ImportedProcs, !:Specs, !ModuleInfo) :-
    % We have already iterated determinism_inference_one_pass to a fixpoint
    % on the undeclared procs.
    determinism_inference_one_pass(ProgressStream, Debug, DeclaredProcs,
        [], !:Specs, unchanged, _, !ModuleInfo),
    % This is the second, checking pass.
    check_determinism_of_procs(ProgressStream, DeclaredProcs,
        !ModuleInfo, !Specs),
    check_determinism_of_procs(ProgressStream, UndeclaredProcs,
        !ModuleInfo, !Specs),
    check_determinism_of_imported_procs(ProgressStream, !.ModuleInfo,
        ImportedProcs, !Specs).

%---------------------------------------------------------------------------%

det_infer_proc_ignore_msgs(ProgressStream, PredId, ProcId, !ModuleInfo) :-
    det_infer_proc(ProgressStream, no, proc(PredId, ProcId),
        [], _Specs, unchanged, _, !ModuleInfo).

:- pred det_infer_proc(io.text_output_stream::in, bool::in, pred_proc_id::in,
    list(error_spec)::in, list(error_spec)::out,
    maybe_changed::in, maybe_changed::out,
    module_info::in, module_info::out) is det.

det_infer_proc(ProgressStream, Debug, PredProcId,
        !Specs, !Changed, !ModuleInfo) :-
    % Get the proc_info structure for this procedure.
    PredProcId = proc(PredId, ProcId),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_proc_info(PredInfo0, ProcId, ProcInfo0),

    % Remember the old inferred determinism of this procedure.
    proc_info_get_inferred_determinism(ProcInfo0, OldDetism),

    % Work out whether or not the procedure occurs in a single-solution
    % context. Currently we only assume so if the predicate has an explicit
    % determinism declaration that says so.
    det_get_soln_context(OldDetism, OldInferredSolnContext),
    proc_info_get_declared_determinism(ProcInfo0, MaybeDeclaredDetism),
    (
        MaybeDeclaredDetism = yes(DeclaredDetism),
        det_get_soln_context(DeclaredDetism, DeclaredSolnContext)
    ;
        MaybeDeclaredDetism = no,
        DeclaredSolnContext = all_solns
    ),
    ( if
        ( DeclaredSolnContext = first_soln
        ; OldInferredSolnContext = first_soln
        )
    then
        SolnContext = first_soln
    else
        SolnContext = all_solns
    ),

    trace [compiletime(flag("debug-det-analysis-progress")), io(!IO)] (
        PredIdInt = pred_id_to_int(PredId),
        ProcIdInt = proc_id_to_int(ProcId),
        io.format(ProgressStream, "inferring predicate %d proc %d\n",
            [i(PredIdInt), i(ProcIdInt)], !IO)
    ),

    % Infer the determinism of the goal.
    proc_info_get_goal(ProcInfo0, Goal0),
    proc_info_get_initial_instmap(!.ModuleInfo, ProcInfo0, InstMap0),
    proc_info_get_var_table(ProcInfo0, VarTable),
    det_info_init(!.ModuleInfo, PredProcId, VarTable, pess_extra_vars_report,
        !.Specs, DetInfo0),
    det_infer_proc_goal(InstMap0, SolnContext, InferDetism,
        Goal0, Goal, DetInfo0, DetInfo),
    det_info_get_module_info(DetInfo, !:ModuleInfo),
    det_info_get_error_specs(DetInfo, !:Specs),

    % Take the worst of the old and inferred detisms. This is needed to prevent
    % loops on p :- not(p), at least if the initial assumed detism is det.
    % This may also be needed to ensure that we don't change the interface
    % determinism of procedures, if we are re-running determinism analysis.
    determinism_components(OldDetism, OldCanFail, OldMaxSoln),
    determinism_components(InferDetism, InferCanFail, InferMaxSoln),
    det_switch_canfail(OldCanFail, InferCanFail, CanFail),
    det_switch_maxsoln(OldMaxSoln, InferMaxSoln, MaxSoln),
    determinism_components(TentativeDetism, CanFail, MaxSoln),

    % Apply the effect of the evaluation model (if any).
    proc_info_get_eval_method(ProcInfo0, EvalMethod),
    NewDetism = eval_method_change_determinism(EvalMethod, TentativeDetism),

    % Save the newly inferred information in the proc_info and pred_info,
    % and put those updated structures back into the module_info.
    proc_info_set_goal(Goal, ProcInfo0, ProcInfo1),
    proc_info_set_inferred_determinism(NewDetism, ProcInfo1, ProcInfo),
    pred_info_set_proc_info(ProcId, ProcInfo, PredInfo0, PredInfo1),
    record_det_info_markers(DetInfo, PredInfo1, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo),

    maybe_record_change_print_inferred(!.ModuleInfo, Debug, PredProcId,
        OldDetism, NewDetism, !Changed).

    % Return the change a given evaluation method can do to a given
    % determinism.
    %
:- func eval_method_change_determinism(eval_method, determinism) = determinism.

eval_method_change_determinism(eval_normal, Detism) = Detism.
eval_method_change_determinism(eval_tabled(TabledMethoed), Detism)  =
    tabled_eval_method_change_determinism(TabledMethoed, Detism).

:- func tabled_eval_method_change_determinism(tabled_eval_method, determinism)
    = determinism.

tabled_eval_method_change_determinism(tabled_loop_check, Detism) = Detism.
tabled_eval_method_change_determinism(tabled_io(_, _), Detism) = Detism.
tabled_eval_method_change_determinism(tabled_memo(_), Detism) = Detism.
tabled_eval_method_change_determinism(tabled_minimal(_), Detism0) = Detism :-
    det_conjunction_detism(detism_semi, Detism0, Detism).

%---------------------%

:- pred record_det_info_markers(det_info::in,
    pred_info::in, pred_info::out) is det.

record_det_info_markers(DetInfo, !PredInfo) :-
    det_info_get_has_format_call(DetInfo, HasFormatCalls),
    det_info_get_has_req_scope(DetInfo, HasRequireScope),
    det_info_get_has_incomplete_switch(DetInfo, HasIncompleteSwitch),
    some [!Markers] (
        pred_info_get_markers(!.PredInfo, !:Markers),
        (
            HasFormatCalls = does_not_contain_format_call
        ;
            HasFormatCalls = contains_format_call,
            add_marker(marker_has_format_call, !Markers)
        ),
        (
            HasRequireScope = does_not_contain_require_scope
        ;
            HasRequireScope = contains_require_scope,
            add_marker(marker_has_require_scope, !Markers)
        ),
        (
            HasIncompleteSwitch = does_not_contain_incomplete_switch
        ;
            HasIncompleteSwitch = contains_incomplete_switch,
            add_marker(marker_has_incomplete_switch, !Markers)
        ),
        pred_info_set_markers(!.Markers, !PredInfo)
    ).

%---------------------%

:- pred maybe_record_change_print_inferred(module_info::in, bool::in,
    pred_proc_id::in, determinism::in, determinism::in,
    maybe_changed::in, maybe_changed::out) is det.

maybe_record_change_print_inferred(ModuleInfo, Debug, PredProcId,
        OldDetism, NewDetism, !Changed) :-
    ( if NewDetism = OldDetism then
        ChangeStr = "old"
    else
        ChangeStr = "new",
        !:Changed = changed
    ),
    (
        Debug = yes,
        trace [io(!IO)] (
            get_debug_output_stream(ModuleInfo,  DebugStream, !IO),
            NewDetismStr = mercury_det_to_string(NewDetism),
            ProcStr = pred_proc_id_to_user_string(ModuleInfo, PredProcId),
            io.format(DebugStream, "%% Inferred %s detism %s for %s\n",
                [s(ChangeStr), s(NewDetismStr), s(ProcStr)], !IO)
        )
    ;
        Debug = no
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Determinism_declarations takes a module_info as input and returns
    % four lists of procedure ids.
    %
    % - DeclaredProcs holds the local user-written procedures
    %   that have declarations that need to be checked.
    %
    % - UndeclaredProcs holds the local user-written procedures
    %   that don't have declarations, whose determinism needs to be inferred.
    %
    % - CompGenProcs holds the local compiler-generated procedures.
    %   Their determinism is already known, and they are known to be correct.
    %
    % - ImportedProcs hold the nonlocal procedures, whose determinism
    %   *should* be included in their mode declarations in the .int/.int2 file.
    %   We perform all the checks on the declarations of procedures that
    %   we perform on the declarations of DeclaredProcs. We don't perform
    %   checks on them that require access to the procedure's body goal,
    %   since we (in the absence of inter-module optimization, at least)
    %   we don't have access to those.
    %
:- pred determinism_declarations(pred_id_table::in, list(pred_id)::in,
    list(pred_proc_id)::out, list(pred_proc_id)::out,
    list(pred_proc_id)::out, list(pred_proc_id)::out) is det.

determinism_declarations(PredIdTable, PredIds,
        DeclaredProcs, UndeclaredProcs, CompGenProcs, ImportedProcs) :-
    determinism_declarations_preds(PredIdTable, PredIds,
        [], DeclaredProcs, [], UndeclaredProcs,
        [], CompGenProcs, [], ImportedProcs).

:- pred determinism_declarations_preds(pred_id_table::in, list(pred_id)::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

determinism_declarations_preds(_PredIdTable, [],
        !DeclaredProcs, !UndeclaredProcs, !CompGenProcs, !ImportedProcs).
determinism_declarations_preds(PredIdTable, [PredId | PredIds],
        !DeclaredProcs, !UndeclaredProcs, !CompGenProcs, !ImportedProcs) :-
    map.lookup(PredIdTable, PredId, PredInfo),
    ProcIds = pred_info_all_procids(PredInfo),
    determinism_declarations_procs(PredId, PredInfo, ProcIds,
        !DeclaredProcs, !UndeclaredProcs, !CompGenProcs, !ImportedProcs),
    determinism_declarations_preds(PredIdTable, PredIds,
        !DeclaredProcs, !UndeclaredProcs, !CompGenProcs, !ImportedProcs).

:- pred determinism_declarations_procs(pred_id::in, pred_info::in,
    list(proc_id)::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

determinism_declarations_procs(_PredId, _PredInfo, [],
        !DeclaredProcs, !UndeclaredProcs, !CompGenProcs, !ImportedProcs).
determinism_declarations_procs(PredId, PredInfo, [ProcId | ProcIds],
        !DeclaredProcs, !UndeclaredProcs, !CompGenProcs, !ImportedProcs) :-
    PredProcId = proc(PredId, ProcId),
    ( if
        % Imported predicates need to be checked, but that will happen
        % when their defining module is compiled.
        pred_info_is_imported(PredInfo)
    then
        !:ImportedProcs = [PredProcId | !.ImportedProcs]
    else if
        % Since we generate the code of <in,in> unifications and class methods
        % ourselves, they do not need to be checked.
        (
            pred_info_is_pseudo_imported(PredInfo),
            hlds_pred.in_in_unification_proc_id(ProcId)
        ;
            pred_info_get_markers(PredInfo, Markers),
            marker_is_present(Markers, marker_class_method)
        )
    then
        !:CompGenProcs = [PredProcId | !.CompGenProcs]
    else
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.lookup(ProcTable, ProcId, ProcInfo),
        proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
        (
            MaybeDetism = no,
            !:UndeclaredProcs = [PredProcId | !.UndeclaredProcs]
        ;
            MaybeDetism = yes(_),
            !:DeclaredProcs = [PredProcId | !.DeclaredProcs]
        )
    ),
    determinism_declarations_procs(PredId, PredInfo, ProcIds,
        !DeclaredProcs, !UndeclaredProcs, !CompGenProcs, !ImportedProcs).

    % We can't infer a tighter determinism for imported procedures or for
    % class methods, so set the inferred determinism to be the same as the
    % declared determinism. This can't be done easily during the make_hlds
    % pass, since inter-module optimization means that the import_status
    % of procedures isn't determined until after all items are processed.
    % XXX Is this still true?
    %
:- pred set_non_inferred_proc_determinism(pred_proc_id::in,
    module_info::in, module_info::out) is det.

set_non_inferred_proc_determinism(proc(PredId, ProcId), !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, Procs0),
    map.lookup(Procs0, ProcId, ProcInfo0),
    proc_info_get_declared_determinism(ProcInfo0, MaybeDet),
    (
        MaybeDet = yes(Det),
        proc_info_set_inferred_determinism(Det, ProcInfo0, ProcInfo),
        map.det_update(ProcId, ProcInfo, Procs0, Procs),
        pred_info_set_proc_table(Procs, PredInfo0, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ;
        MaybeDet = no
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.det_analysis.
%---------------------------------------------------------------------------%
