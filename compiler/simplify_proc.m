%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: simplify_proc.m.
%
% This module handles top level invocations of simplification.
%
% Most such invocations simplify the body of a procedure, or the bodies
% of all the procedures in a predicate. However, in some cases some other
% compiler passes (such as deforestation or partial evaluation) want to
% simplify a goal that is not the body of a procedure.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_proc.
:- interface.

:- import_module check_hlds.simplify.simplify_tasks.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % Simplify all the given procedures of the given predicate.
    % Add any resulting messages to the error spec accumulator.
    %
    % Used by mercury_compiler_front_end.m when doing compilation pass-by-pass.
    %
:- pred simplify_pred_procs(io.text_output_stream::in,
    simplify_tasks::in, pred_id::in, list(proc_id)::in,
    pred_info::in, pred_info::out, module_info::in, module_info::out,
    error_spec_accumulator::in, error_spec_accumulator::out) is det.

    % Simplify the given procedure. Throw away any resulting error messages.
    %
    % Used by compiler passes after the front end that need (or maybe just
    % want) to eliminate unnecessary parts of the procedure.
    %
:- pred simplify_proc(maybe(io.text_output_stream)::in,
    io.text_output_stream::in, simplify_tasks::in, pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

    % simplify_goal_update_vars_in_proc(ProgressStream, SimplifyTasks,
    %   PredId, ProcId, InstMap0, CostDelta, !Goal, !ProcInfo, !ModuleInfo):
    %
    % Perform the specified simplification tasks on !Goal, which should be
    % part of the procedure identified by PredId and ProcId. InstMap0
    % should be the instmap immediately before !.Goal.
    %
    % We may update !ModuleInfo during the course of updating instmaps
    % to reflect any changes made to the code. If the modifications to !Goal
    % add any new variables, add these to !ProcInfo.
    %
    % !.Goal does NOT need to be the entire body of the procedure it appears
    % in; it can be just a part of it. This is why we return the updated goal
    % in !:Goal, not in !:ProcInfo.
    %
    % Used by partial evaluation.
    %
:- pred simplify_goal_update_vars_in_proc(io.text_output_stream::in,
    simplify_tasks::in, pred_id::in, proc_id::in, instmap::in, int::out,
    hlds_goal::in, hlds_goal::out, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_infer_goal.
:- import_module check_hlds.det_util.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module check_hlds.simplify.common.
:- import_module check_hlds.simplify.format_call.
:- import_module check_hlds.simplify.mark_trace_goals.
:- import_module check_hlds.simplify.simplify_goal.
:- import_module check_hlds.simplify.simplify_info.
:- import_module check_hlds.simplify.split_switch_arms.
:- import_module hlds.code_model.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.
:- import_module transform_hlds.direct_arg_in_out.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module set.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

simplify_pred_procs(_, _, _, [], !PredInfo, !ModuleInfo, !Specs).
simplify_pred_procs(ProgressStream, SimplifyTasks, PredId,
        [ProcId | ProcIds], !PredInfo, !ModuleInfo, !Specs) :-
    simplify_pred_proc(ProgressStream, SimplifyTasks, PredId, ProcId,
        !PredInfo, !ModuleInfo, !Specs),
    simplify_pred_procs(ProgressStream, SimplifyTasks, PredId, ProcIds,
        !PredInfo, !ModuleInfo, !Specs).

:- pred simplify_pred_proc(io.text_output_stream::in, simplify_tasks::in,
    pred_id::in, proc_id::in, pred_info::in, pred_info::out,
    module_info::in, module_info::out,
    error_spec_accumulator::in, error_spec_accumulator::out) is det.

simplify_pred_proc(ProgressStream, SimplifyTasks, PredId, ProcId,
        !PredInfo, !ModuleInfo, !Specs) :-
    % XXX It is strange that simplify_proc prints progress messages,
    % but simplify_pred_proc does not.
    pred_info_get_proc_table(!.PredInfo, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),
    simplify_proc_return_msgs(ProgressStream, SimplifyTasks, PredId, ProcId,
        ProcSpecs, ProcInfo0, ProcInfo, !ModuleInfo),
    % This is ugly, but we want to avoid running the dependent parallel
    % conjunction pass on predicates and even modules that do not contain
    % parallel conjunctions (nearly all of them). Since simplification
    % is always done, we use it to mark modules and procedures containing
    % parallel conjunctions.
    proc_info_get_has_parallel_conj(ProcInfo, HasParallelConj),
    (
        HasParallelConj = has_parallel_conj,
        module_info_set_has_parallel_conj(!ModuleInfo)
    ;
        HasParallelConj = has_no_parallel_conj
    ),
    proc_info_get_has_user_event(ProcInfo, HasUserEvent),
    (
        HasUserEvent = has_user_event,
        module_info_set_has_user_event(!ModuleInfo)
    ;
        HasUserEvent = has_no_user_event
    ),
    map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
    pred_info_set_proc_table(ProcTable, !PredInfo),
    accumulate_error_specs_for_proc(ProcSpecs, !Specs).

simplify_proc(MaybeProgressStream, ProgressStream, SimplifyTasks,
        PredId, ProcId, !ProcInfo, !ModuleInfo)  :-
    trace [io(!IO)] (
        (
            MaybeProgressStream = no
        ;
            MaybeProgressStream = yes(Stream),
            maybe_write_pred_progress_message(Stream, !.ModuleInfo,
                "Simplifying", PredId, !IO)
        )
    ),
    simplify_proc_return_msgs(ProgressStream, SimplifyTasks, PredId, ProcId,
        _, !ProcInfo, !ModuleInfo).

simplify_goal_update_vars_in_proc(ProgressStream, SimplifyTasks,
        PredId, ProcId, InstMap0, CostDelta, !Goal, !ProcInfo, !ModuleInfo) :-
    simplify_info_init(ProgressStream, !.ModuleInfo, PredId, ProcId,
        !.ProcInfo, SimplifyTasks, SimplifyInfo0),
    % The nested context we construct is probably a lie; we don't actually
    % know whether we are inside a goal duplicated for a switch, or a lambda,
    % or a model_non procedure. However, this should be ok. The first three
    % fields of the nested context are used for deciding what warnings and
    % errors to generate, and we are not interested in those, while the fourth
    % is there to support an optimization that we explicitly disallow
    % below by passing do_not_allow_splitting_switch_arms.
    InsideDuplForSwitch = no,
    ProcIsModelNon = no,
    NumEnclosingBarriers = 0u,
    SwitchArmContext = [],
    NestedContext0 = simplify_nested_context(InsideDuplForSwitch,
        ProcIsModelNon, NumEnclosingBarriers, SwitchArmContext),
    % Passing do_not_allow_splitting_switch_arms here is conservative.
    simplify_top_level_goal(NestedContext0, InstMap0,
        do_not_allow_splitting_switch_arms, !Goal,
        SimplifyInfo0, SimplifyInfo),

    simplify_info_get_module_info(SimplifyInfo, !:ModuleInfo),

    simplify_info_get_var_table(SimplifyInfo, VarTable),
    simplify_info_get_rtti_varmaps(SimplifyInfo, RttiVarMaps),
    proc_info_set_var_table(VarTable, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),

    simplify_info_get_cost_delta(SimplifyInfo, CostDelta).

%-----------------------------------------------------------------------------%

    % Simplify the given procedure. Return the resulting error messages.
    %
:- pred simplify_proc_return_msgs(io.text_output_stream::in,
    simplify_tasks::in, pred_id::in, proc_id::in, list(error_spec)::out,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

simplify_proc_return_msgs(ProgressStream, SimplifyTasks0, PredId, ProcId,
        !:Specs, !ProcInfo, !ModuleInfo) :-
    simplify_proc_maybe_vary_parameters(!.ModuleInfo, PredId, !.ProcInfo,
        SimplifyTasks0, SimplifyTasks),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_markers(PredInfo0, Markers0),
    ( if marker_is_present(Markers0, marker_mode_check_clauses) then
        simplify_proc_maybe_mark_modecheck_clauses(!ProcInfo)
    else
        true
    ),

    % We must invoke analyze_and_optimize_format_calls before
    % simplify_top_level_goal, for two reasons.
    %
    % First, excess assignment optimization may delete some of the
    % unifications that build the format strings or values,
    % which means that the goal it generates may not contain the
    % information that analyze_and_optimize_format_calls needs to avoid
    % spurious messages about unknown format strings or values.
    %
    % Second, analyze_and_optimize_format_calls generates nested
    % conjunctions, which simplify_top_level_goal can eliminate.
    %
    % We therefore get determinism analysis to mark the procedure
    % if its body contains any calls relevant to format_calls.m.

    ( if
        marker_is_present(Markers0, marker_has_format_call),
        SimplifyTasks ^ do_invoke_format_call = invoke_format_call
    then
        (
            SimplifyTasks ^ do_warn_implicit_streams =
                do_not_warn_implicit_streams,
            ImplicitStreamWarnings = do_not_generate_implicit_stream_warnings
        ;
            SimplifyTasks ^ do_warn_implicit_streams = warn_implicit_streams,
            ImplicitStreamWarnings = generate_implicit_stream_warnings
        ),
        simplify_proc_analyze_and_format_calls(!ModuleInfo, PredId, PredInfo0,
            ProcId, !ProcInfo, ImplicitStreamWarnings, FormatSpecs)
    else
        % Either there are no format calls to check, or we don't want to
        % optimize them and would ignore the added messages anyway.
        FormatSpecs = []
    ),

    simplify_info_init(ProgressStream, !.ModuleInfo, PredId, ProcId,
        !.ProcInfo, SimplifyTasks, Info0),

    InsideDuplForSwitch = no,
    CodeModel = proc_info_interface_code_model(!.ProcInfo),
    (
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        ProcIsModelNon = no
    ;
        CodeModel = model_non,
        ProcIsModelNon = yes(imp_whole_proc)
    ),
    NumEnclosingBarriers = 0u,
    SwitchArmContext = [],
    NestedContext0 = simplify_nested_context(InsideDuplForSwitch,
        ProcIsModelNon, NumEnclosingBarriers, SwitchArmContext),
    proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InstMap0),

    proc_info_get_goal(!.ProcInfo, Goal0),
    simplify_top_level_goal(NestedContext0, InstMap0,
        allow_splitting_switch_arms, Goal0, Goal, Info0, Info),
    proc_info_set_goal(Goal, !ProcInfo),

    simplify_info_get_var_table(Info, VarTable0),
    simplify_info_get_rtti_varmaps(Info, RttiVarMaps),
    simplify_info_get_elim_vars(Info, ElimVarsLists0),
    % We sort the lists basically on the number of the first variable.
    list.sort(ElimVarsLists0, ElimVarsLists),
    list.condense(ElimVarsLists, ElimVars),
    delete_var_entries(ElimVars, VarTable0, VarTable1),
    simplify_info_get_module_info(Info, !:ModuleInfo),
    % We only eliminate vars that cannot occur in RttiVarMaps.
    ( if simplify_do_after_front_end(Info) then
        proc_info_get_var_name_remap(!.ProcInfo, VarNameRemap),
        RenameVar =
            ( pred(V::in, N::in, VT0::in, VT::out) is det :-
                lookup_var_entry(VT0, V, E0),
                E = E0 ^ vte_name := N,
                update_var_entry(V, E, VT0, VT)
            ),
        map.foldl(RenameVar, VarNameRemap, VarTable1, VarTable),
        proc_info_set_var_name_remap(map.init, !ProcInfo),

        proc_info_get_headvars(!.ProcInfo, HeadVars),
        proc_info_get_argmodes(!.ProcInfo, ArgModes),
        find_and_record_any_direct_arg_in_out_posns(PredId, ProcId, VarTable,
            HeadVars, ArgModes, !ModuleInfo)
    else
        VarTable = VarTable1
    ),
    proc_info_set_var_table(VarTable, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),

    simplify_info_get_has_parallel_conj(Info, HasParallelConj),
    proc_info_set_has_parallel_conj(HasParallelConj, !ProcInfo),

    simplify_info_get_has_user_event(Info, HasUserEvent),
    proc_info_set_has_user_event(HasUserEvent, !ProcInfo),

    simplify_info_get_deleted_call_callees(Info, CurDeletedCallCallees),
    proc_info_get_deleted_call_callees(!.ProcInfo, DeletedCallCallees0),
    set.union(CurDeletedCallCallees, DeletedCallCallees0, DeletedCallCallees),
    proc_info_set_deleted_call_callees(DeletedCallCallees, !ProcInfo),

    simplify_info_get_error_specs(Info, !:Specs),
    !:Specs = FormatSpecs ++ !.Specs,
    simplify_proc_maybe_warn_attribute_conflict(!.ModuleInfo, PredId,
        !.ProcInfo, !Specs),

    pred_info_get_status(PredInfo0, Status),
    IsDefinedHere = pred_status_defined_in_this_module(Status),
    (
        IsDefinedHere = no,
        % Don't generate any warnings or even errors if the predicate isn't
        % defined here; any such messages will be generated when we compile
        % the module the predicate comes from.
        !:Specs = []
    ;
        IsDefinedHere = yes
    ).

:- pred simplify_proc_maybe_vary_parameters(module_info::in, pred_id::in,
    proc_info::in, simplify_tasks::in, simplify_tasks::out) is det.

simplify_proc_maybe_vary_parameters(ModuleInfo, PredId, ProcInfo,
        !SimplifyTasks) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_string_option(Globals, debug_common_struct_preds,
        DebugCommonStructPreds),
    ( if DebugCommonStructPreds = "" then
        TurnOffCommonStructByRequest = no
    else
        CommonStructPredIdStrs =
            string.split_at_char(',', DebugCommonStructPreds),
        ( if
            list.map(string.to_int, CommonStructPredIdStrs,
                CommonStructPredIdInts)
        then
            PredIdInt = pred_id_to_int(PredId),
            ( if list.member(PredIdInt, CommonStructPredIdInts) then
                TurnOffCommonStructByRequest = no
            else
                TurnOffCommonStructByRequest = yes
            )
        else
            TurnOffCommonStructByRequest = no
        )
    ),
    proc_info_get_var_table(ProcInfo, VarTable0),
    var_table_count(VarTable0, NumVars),
    ( if
        ( TurnOffCommonStructByRequest = yes
        ; NumVars > turn_off_common_struct_threshold
        )
    then
        !SimplifyTasks ^ do_opt_common_structs := do_not_opt_common_structs
    else
        true
    ).

    % If we have too many variables, common_struct used to take so long that
    % either the compiler runs out of memory, or the user runs out of patience.
    % In such cases, the fact that we would generate better code if the
    % compilation finished is therefore of limited interest.
    %
    % However, since this limit was first imposed, we have optimized
    % the compiler's infrastructure for such things, e.g. by using much more
    % compact representations for sets of variables, which permit much faster
    % operations on them. These changes do not eliminate the danger described
    % above completely, but they do raise the threshold at which they can
    % appear.
    %
    % As of 2020 october 11, the code of convert_options_to_globals in
    % handle_options.m has just shy of 9,000 variables at the time of the
    % first simplify pass (HLDS dump stage 65). The compiler can handle that
    % easily, so the setting below allows for some growth.
    %
:- func turn_off_common_struct_threshold = int.

turn_off_common_struct_threshold = 12000.

:- pred simplify_proc_maybe_mark_modecheck_clauses(
    proc_info::in, proc_info::out) is det.

simplify_proc_maybe_mark_modecheck_clauses(!ProcInfo) :-
    proc_info_get_goal(!.ProcInfo, Goal0),
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ( if
        ( GoalExpr0 = disj(_)
        ; GoalExpr0 = switch(_, _, _)
        )
    then
        goal_info_add_feature(feature_mode_check_clauses_goal,
            GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr0, GoalInfo),
        proc_info_set_goal(Goal, !ProcInfo)
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- pred simplify_proc_analyze_and_format_calls(
    module_info::in, module_info::out, pred_id::in, pred_info::in,
    proc_id::in, proc_info::in, proc_info::out,
    maybe_generate_implicit_stream_warnings::in, list(error_spec)::out) is det.

simplify_proc_analyze_and_format_calls(!ModuleInfo, PredId, PredInfo0,
        ProcId, !ProcInfo, ImplicitStreamWarnings, FormatSpecs) :-
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_var_table(!.ProcInfo, VarTable0),
    analyze_and_optimize_format_calls(!.ModuleInfo, PredInfo0, !.ProcInfo,
        ImplicitStreamWarnings, Goal0, MaybeGoal, FormatSpecs,
        VarTable0, VarTable),
    (
        MaybeGoal = yes(Goal),
        proc_info_set_goal(Goal, !ProcInfo),
        proc_info_set_var_table(VarTable, !ProcInfo),

        % The goals we replace format calls with are created with the
        % correct nonlocals, but analyze_and_optimize_format_calls can
        % take code for building a list of string.poly_types out of one
        % scope (e.g. the condition of an if-then-else) and replace it
        % with code to build the string directly in another scope
        % (such as the then part of that if-then-else, if that is where
        % the format call is). This can leave variables missing from
        % the nonlocal fields of the original scopes. And since
        % instmap_deltas are restricted to the goal's nonlocals,
        % they need to be recomputed as well.
        requantify_proc_general(ord_nl_maybe_lambda, !ProcInfo),
        recompute_instmap_delta_proc(no_recomp_atomics,
            !ProcInfo, !ModuleInfo),

        % Put the new proc_info back into !ModuleInfo, since some of the
        % following code could otherwise find obsolete information in there.
        pred_info_set_proc_info(ProcId, !.ProcInfo, PredInfo0, PredInfo1),

        % Remove the has_format_call marker from the pred_info before
        % putting it back, since any optimizable format calls will already
        % have been optimized. Since currently there is no program
        % transformation that inserts calls to these predicates,
        % there is no point in trying to optimize format_calls again later.
        pred_info_get_markers(PredInfo1, Markers1),
        remove_marker(marker_has_format_call, Markers1, Markers),
        pred_info_set_markers(Markers, PredInfo1, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ;
        MaybeGoal = no
        % There should not be any updates to the var_table,
        % but even if there are, throw them away, since they apply to a version
        % of the goal that we will not be using.
    ).

%-----------------------------------------------------------------------------%

:- pred simplify_proc_maybe_warn_attribute_conflict(module_info::in,
    pred_id::in, proc_info::in, list(error_spec)::in, list(error_spec)::out)
    is det.

simplify_proc_maybe_warn_attribute_conflict(ModuleInfo, PredId, ProcInfo,
        !Specs) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),

    % The alternate goal by definition cannot be a call_foreign_proc.
    proc_info_get_goal(ProcInfo, Goal),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    ( if GoalExpr = call_foreign_proc(Attributes, _, _, _, _, _, _) then
        Context = goal_info_get_context(GoalInfo),
        MaybeMayDuplicate = get_may_duplicate(Attributes),
        (
            MaybeMayDuplicate = yes(MayDuplicate),
            maybe_warn_about_may_duplicate_attributes(MayDuplicate, Markers,
                Context, !Specs)
        ;
            MaybeMayDuplicate = no
        ),
        MaybeMayExportBody = get_may_export_body(Attributes),
        (
            MaybeMayExportBody = yes(MayExportBody),
            maybe_warn_about_may_export_body_attribute(MayExportBody, Markers,
                Context, !Specs)
        ;
            MaybeMayExportBody = no
        )
    else
        true
    ).

:- pred maybe_warn_about_may_duplicate_attributes(proc_may_duplicate::in,
    pred_markers::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_warn_about_may_duplicate_attributes(MayDuplicate, Markers, Context,
        !Specs) :-
    (
        MayDuplicate = proc_may_duplicate,
        ( if marker_is_present(Markers, marker_user_marked_no_inline) then
            AttrPieces = [quote("may_duplicate"), words("attribute")],
            PragmaPieces = [pragma_decl("no_inline"), words("declaration")],
            Pieces = [words("Error: the")] ++
                color_as_inconsistent(AttrPieces) ++
                [words("on the foreign_proc is")] ++
                color_as_incorrect([words("not compatible")]) ++
                [words("with the")] ++
                color_as_inconsistent(PragmaPieces) ++
                [words("on the predicate."), nl],
            Spec = spec($pred, severity_error,
                phase_simplify(report_in_any_mode), Context, Pieces),
            !:Specs = [Spec | !.Specs]
        else
            true
        )
    ;
        MayDuplicate = proc_may_not_duplicate,
        ( if marker_is_present(Markers, marker_user_marked_inline) then
            AttrPieces = [quote("may_not_duplicate"), words("attribute")],
            PragmaPieces = [pragma_decl("inline"), words("declaration")],
            Pieces = [words("Error: the")] ++
                color_as_inconsistent(AttrPieces) ++
                [words("on the foreign_proc is")] ++
                color_as_incorrect([words("not compatible")]) ++
                [words("with the")] ++
                color_as_inconsistent(PragmaPieces) ++
                [words("on the predicate."), nl],
            Spec = spec($pred, severity_error,
                phase_simplify(report_in_any_mode), Context, Pieces),
            !:Specs = [Spec | !.Specs]
        else
            true
        )
    ).

:- pred maybe_warn_about_may_export_body_attribute(proc_may_export_body::in,
    pred_markers::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_warn_about_may_export_body_attribute(MayExportBody, Markers, Context,
        !Specs) :-
    (
        MayExportBody = proc_may_export_body,
        ( if marker_is_present(Markers, marker_user_marked_no_inline) then
            AttrPieces = [quote("may_export_body"), words("attribute")],
            PragmaPieces = [pragma_decl("inline"), words("declaration")],
            Pieces = [words("Error: the")] ++
                color_as_inconsistent(AttrPieces) ++
                [words("on the foreign_proc is")] ++
                color_as_incorrect([words("not compatible")]) ++
                [words("with the")] ++
                color_as_inconsistent(PragmaPieces) ++
                [words("on the predicate."), nl],
            Spec = spec($pred, severity_error,
                phase_simplify(report_in_any_mode), Context, Pieces),
            !:Specs = [Spec | !.Specs]
        else
            true
        )
    ;
        MayExportBody = proc_may_not_export_body
        % Inlining is allowed within the same target file.
    ).

%-----------------------------------------------------------------------------%

:- type maybe_allow_splitting_switch_arms
    --->    do_not_allow_splitting_switch_arms
    ;       allow_splitting_switch_arms.

:- pred simplify_top_level_goal(simplify_nested_context::in, instmap::in,
    maybe_allow_splitting_switch_arms::in, hlds_goal::in, hlds_goal::out,
    simplify_info::in, simplify_info::out) is det.

simplify_top_level_goal(NestedContext0, InstMap0, AllowSplitSwitchArms,
        !Goal, !Info) :-
    % Simplification is done in two main passes, which is we have two calls
    % to do_simplify_top_level_goal below. The first pass performs common
    % structure and duplicate call elimination. The second pass performs
    % excess assignment elimination, and cleans up the code after the
    % first pass.
    %
    % Two passes are required because some parts of the first pass can
    % obsolete existing nonlocals sets and instmap deltas, while some parts
    % of the second pass may need this information to be up-to-date.
    % Therefore we have to be prepared to recompute this information
    % between the two passes.
    %
    % After the two main passes that both invoke do_simplify_top_level_goal,
    % we have some optional other passes as well.
    some [!SimplifyTasks] (
        simplify_info_get_simplify_tasks(!.Info, !:SimplifyTasks),
        OriginalSimplifyTasks = !.SimplifyTasks,
        ( if
            ( !.SimplifyTasks ^ do_opt_common_structs = opt_common_structs
            ; !.SimplifyTasks ^ do_opt_const_structs = opt_const_structs
            ; !.SimplifyTasks ^ do_opt_duplicate_calls = opt_dup_calls
            )
        then
            !SimplifyTasks ^ do_mark_code_model_changes
                := do_not_mark_code_model_changes,
            !SimplifyTasks ^ do_excess_assign := do_not_elim_excess_assigns,
            simplify_info_set_simplify_tasks(!.SimplifyTasks, !Info),
            % PASS 1.
            do_simplify_top_level_goal(NestedContext0, InstMap0, !Goal, !Info),

            !:SimplifyTasks = OriginalSimplifyTasks,

            % Disable the generation of error, warning and info messages
            % for the second pass.
            % - We can do this because any warnings we want to generate
            %   will have been generated by the first pass.
            % - And we *must* do this, because the excess assign transformation
            %   in pass 2 may replace a variable (say X) with a different
            %   variable (say Y). So if in pass 1, we generated e.g. a message
            %   involving X, then in pass 2 we would generate a version
            %   of that same message, but referring to Y instead of X,
            %   which is very likely to confuse the reader.
            simplify_info_set_allow_messages(do_not_allow_messages, !Info),

            % If requested, these tasks were done in pass 1. Repeating them
            % in pass 2 would serve no purpose, since we do nothing in pass 1
            % that would generate new occurrences of the situations
            % that these tasks seek to optimize.
            !SimplifyTasks ^ do_opt_common_structs :=
                do_not_opt_common_structs,
            !SimplifyTasks ^ do_opt_const_structs := do_not_opt_const_structs,
            !SimplifyTasks ^ do_opt_duplicate_calls := do_not_opt_dup_calls,
            simplify_info_reinit(!.SimplifyTasks, !Info)
        else
            % We have not been asked to perform PASS 1.
            true
        ),
        % PASS 2.
        % In this pass, do excess assignment elimination and
        % some cleaning up after the common structure pass.
        do_simplify_top_level_goal(NestedContext0, InstMap0, !Goal, !Info),

        % OPTIONAL PASS 3.
        simplify_info_get_switch_arms_to_split(!.Info, ToSplitArms),
        ( if
            set.is_non_empty(ToSplitArms),
            OriginalSimplifyTasks ^ do_switch_split_arms = split_switch_arms,
            AllowSplitSwitchArms = allow_splitting_switch_arms
        then
            simplify_info_get_tvarset(!.Info, TVarSet),
            simplify_info_get_inst_varset(!.Info, InstVarSet),
            trace [compile_time(flag("split_switch_arms")),
                runtime(env("SPLIT_SWITCH_ARMS")),
                io(!IO)]
            (
                io.stderr_stream(StdErr, !IO),
                io.write_string(StdErr, "BEFORE split_switch_arms\n", !IO),
                simplify_info_get_module_info(!.Info, ModuleInfo),
                simplify_info_get_var_table(!.Info, VarTable),
                dump_goal_nl(StdErr, ModuleInfo, vns_var_table(VarTable),
                    TVarSet, InstVarSet, !.Goal, !IO)
            ),
            split_switch_arms_in_goal(ToSplitArms, !Goal),
            trace [compile_time(flag("split_switch_arms")),
                runtime(env("SPLIT_SWITCH_ARMS")),
                io(!IO)]
            (
                io.stderr_stream(StdErr, !IO),
                io.write_string(StdErr, "AFTER split_switch_arms\n", !IO),
                simplify_info_get_module_info(!.Info, ModuleInfo),
                simplify_info_get_var_table(!.Info, VarTable),
                dump_goal_nl(StdErr, ModuleInfo, vns_var_table(VarTable),
                    TVarSet, InstVarSet, !.Goal, !IO)
            ),

            !.Goal = hlds_goal(_, GoalInfo0),
            simplify_info_set_rerun_quant_instmap_delta(!Info),
            simplify_info_set_rerun_det(!Info),
            maybe_recompute_fields_after_top_level_goal(GoalInfo0, InstMap0,
                !Goal, !Info)
        else
            true
        ),

        % OPTIONAL PASS 4.
        ( if
            simplify_do_after_front_end(!.Info),
            simplify_info_get_found_contains_trace(!.Info, yes)
        then
            set_goal_contains_trace_features_in_goal(!Goal, _ContainsTraceGoal,
                map.init, _LastNonTraceGoal, [], TraceSpecs),
            % Note that we ignore the setting of do_not_allow_messages above,
            % since it applies only to pass 2.
            simplify_info_get_error_specs(!.Info, Specs0),
            Specs = TraceSpecs ++ Specs0,
            simplify_info_set_error_specs(Specs, !Info)
        else
            true
        )
        % We do not have to put OriginalSimplifyTasks back into !:Info
        % at the end, because neither of our two callers will look at
        % the tasks field of the !:Info value we return. We could return
        % just the fields of !:Info they *do* look at, but there are
        % many of them.
    ).

:- pred do_simplify_top_level_goal(simplify_nested_context::in, instmap::in,
    hlds_goal::in, hlds_goal::out,
    simplify_info::in, simplify_info::out) is det.

do_simplify_top_level_goal(NestedContext0, InstMap0, !Goal, !Info) :-
    !.Goal = hlds_goal(_, GoalInfo0),
    simplify_info_get_simplify_tasks(!.Info, SimplifyTasks),
    Common0 = common_info_init(SimplifyTasks),
    simplify_goal(!Goal, NestedContext0, InstMap0, Common0, _Common, !Info),
    maybe_recompute_fields_after_top_level_goal(GoalInfo0, InstMap0,
        !Goal, !Info).

:- pred maybe_recompute_fields_after_top_level_goal(hlds_goal_info::in,
    instmap::in, hlds_goal::in, hlds_goal::out,
    simplify_info::in, simplify_info::out) is det.

maybe_recompute_fields_after_top_level_goal(GoalInfo0, InstMap0,
        !Goal, !Info) :-
    simplify_info_get_rerun_quant_instmap_delta(!.Info, RerunQuantDelta),
    (
        RerunQuantDelta = rerun_quant_instmap_deltas,
        NonLocals = goal_info_get_nonlocals(GoalInfo0),
        some [!ModuleInfo, !VarTable, !RttiVarMaps] (
            simplify_info_get_tvarset(!.Info, TVarSet),
            simplify_info_get_inst_varset(!.Info, InstVarSet),
            simplify_info_get_var_table(!.Info, !:VarTable),
            trace [compile_time(flag("simplify_recompute_after")), io(!IO)] (
                io.stderr_stream(StdErr, !IO),
                set_of_var.to_sorted_list(NonLocals, NonLocalsList),
                VarNameSrc = vns_var_table(!.VarTable),
                NonLocalsStr = mercury_vars_to_string_src(VarNameSrc,
                    print_name_and_num, NonLocalsList),

                io.write_string(StdErr, "\nBEFORE QUANTIFY\n\n", !IO),
                io.format(StdErr, "NONLOCALS: %s\n", [s(NonLocalsStr)], !IO),
                simplify_info_get_module_info(!.Info, TraceModuleInfo),
                dump_goal_nl(StdErr, TraceModuleInfo, VarNameSrc,
                    TVarSet, InstVarSet, !.Goal, !IO)
            ),
            simplify_info_get_rtti_varmaps(!.Info, !:RttiVarMaps),
            implicitly_quantify_goal_general(ord_nl_maybe_lambda, NonLocals, _,
                !Goal, !VarTable, !RttiVarMaps),
            trace [compile_time(flag("simplify_recompute_after")), io(!IO)] (
                io.stderr_stream(StdErr, !IO),
                io.write_string(StdErr, "\nAFTER QUANTIFY\n\n", !IO),
                simplify_info_get_module_info(!.Info, TraceModuleInfo),
                VarNameSrc = vns_var_table(!.VarTable),
                dump_goal_nl(StdErr, TraceModuleInfo, VarNameSrc,
                    TVarSet, InstVarSet, !.Goal, !IO)
            ),

            simplify_info_set_var_table(!.VarTable, !Info),
            simplify_info_set_rtti_varmaps(!.RttiVarMaps, !Info),

            % Always recompute instmap_deltas for atomic goals - this is safer
            % in the case where unused variables should no longer be included
            % in the instmap_delta for a goal.
            simplify_info_get_module_info(!.Info, !:ModuleInfo),
            recompute_instmap_delta(recomp_atomics, !.VarTable, InstVarSet,
                InstMap0, !Goal, !ModuleInfo),
            simplify_info_set_module_info(!.ModuleInfo, !Info),

            trace [compile_time(flag("simplify_recompute_after")), io(!IO)] (
                io.stderr_stream(StdErr, !IO),
                io.write_string(StdErr, "\nAFTER INSTMAP DELTAS\n\n", !IO),
                simplify_info_get_module_info(!.Info, TraceModuleInfo),
                VarNameSrc = vns_var_table(!.VarTable),
                dump_goal_nl(StdErr, TraceModuleInfo, VarNameSrc,
                    TVarSet, InstVarSet, !.Goal, !IO)
            )
        )
    ;
        RerunQuantDelta = do_not_rerun_quant_instmap_deltas
    ),

    simplify_info_get_rerun_det(!.Info, RerunDet),
    (
        RerunDet = rerun_det,
        Detism = goal_info_get_determinism(GoalInfo0),
        det_get_soln_context(Detism, SolnContext),
        some [!ModuleInfo, !ProcInfo, !VarTable, !RttiVarMaps]
        (
            % det_infer_proc_goal looks up the proc_info in the module_info for
            % the var_table, so we have to put all the proc_info components
            % we have updated back in the proc_info, which we have to put back
            % in the module_info.
            simplify_info_get_module_info(!.Info, !:ModuleInfo),
            simplify_info_get_var_table(!.Info, !:VarTable),
            simplify_info_get_rtti_varmaps(!.Info, !:RttiVarMaps),
            simplify_info_get_pred_proc_id(!.Info, PredProcId),
            module_info_pred_proc_info(!.ModuleInfo, PredProcId,
                PredInfo, !:ProcInfo),
            proc_info_set_var_table(!.VarTable, !ProcInfo),
            proc_info_set_rtti_varmaps(!.RttiVarMaps, !ProcInfo),
            module_info_set_pred_proc_info(PredProcId,
                PredInfo, !.ProcInfo, !ModuleInfo),
            simplify_info_set_module_info(!.ModuleInfo, !Info),

            det_info_init(!.ModuleInfo, PredProcId, !.VarTable,
                pess_extra_vars_report, [], DetInfo0),
            det_infer_proc_goal(InstMap0, SolnContext, _Detism,
                !Goal, DetInfo0, DetInfo),
            det_info_get_module_info(DetInfo, !:ModuleInfo),
            det_info_get_var_table(DetInfo, !:VarTable),
            simplify_info_set_module_info(!.ModuleInfo, !Info),
            simplify_info_set_var_table(!.VarTable, !Info)
        )
    ;
        RerunDet = do_not_rerun_det
    ).
    % The call to simplify_info_reinit in our caller will reset !:Info
    % to do_not_rerun_quant_instmap_deltas and do_not_rerun_det before
    % the next pass, if there is a next pass.

%-----------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_proc.
%-----------------------------------------------------------------------------%
