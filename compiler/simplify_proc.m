%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
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
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Simplify all the given procedures of the given predicate.
    % Add any resulting messages to the error spec accumulator.
    %
    % Used by mercury_compiler_front_end.m when doing compilation pass-by-pass.
    %
:- pred simplify_pred_procs(simplify_tasks::in, pred_id::in,
    list(proc_id)::in, module_info::in, module_info::out,
    pred_info::in, pred_info::out,
    error_spec_accumulator::in, error_spec_accumulator::out) is det.

    % Simplify the given procedure. Throw away any resulting error messages.
    %
    % Used by compiler passes after the front end that need (or maybe just
    % want) to eliminate unnecessary parts of the procedure.
    %
:- pred simplify_proc(simplify_tasks::in, pred_id::in, proc_id::in,
    module_info::in, module_info::out, proc_info::in, proc_info::out) is det.

    % simplify_goal_update_vars_in_proc(SimplifyTasks, !ModuleInfo,
    %   PredId, ProcId, !ProcInfo, InstMap0, !Goal, CostDelta):
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
:- pred simplify_goal_update_vars_in_proc(simplify_tasks::in,
    module_info::in, module_info::out, pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, instmap::in, hlds_goal::in, hlds_goal::out,
    int::out) is det.

    % simplify_may_introduce_calls(ModuleName, PredName, Arity):
    %
    % Succeed if the simplify package may introduce calls to a predicate
    % or function with the given name. ModuleName should be a standard library
    % module.
    %
:- pred simplify_may_introduce_calls(string::in, string::in, arity::in)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_analysis.
:- import_module check_hlds.det_util.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.simplify.common.
:- import_module check_hlds.simplify.format_call.
:- import_module check_hlds.simplify.simplify_goal.
:- import_module check_hlds.simplify.simplify_info.
:- import_module hlds.code_model.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_data_foreign.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

simplify_pred_procs(_, _, [], !ModuleInfo, !PredInfo, !Specs).
simplify_pred_procs(SimplifyTasks, PredId, [ProcId | ProcIds], !ModuleInfo,
        !PredInfo, !Specs) :-
    simplify_pred_proc(SimplifyTasks, PredId, ProcId, !ModuleInfo,
        !PredInfo, !Specs),
    simplify_pred_procs(SimplifyTasks, PredId, ProcIds, !ModuleInfo,
        !PredInfo, !Specs).

:- pred simplify_pred_proc(simplify_tasks::in, pred_id::in, proc_id::in,
    module_info::in, module_info::out, pred_info::in, pred_info::out,
    error_spec_accumulator::in, error_spec_accumulator::out) is det.

simplify_pred_proc(SimplifyTasks, PredId, ProcId, !ModuleInfo,
        !PredInfo, !Specs) :-
    pred_info_get_proc_table(!.PredInfo, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),
    simplify_proc_return_msgs(SimplifyTasks, PredId, ProcId,
        !ModuleInfo, ProcInfo0, ProcInfo, ProcSpecs),
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

simplify_proc(SimplifyTasks, PredId, ProcId, !ModuleInfo, !ProcInfo)  :-
    trace [io(!IO)] (
        write_pred_progress_message("% Simplifying ", PredId, !.ModuleInfo,
            !IO)
    ),
    simplify_proc_return_msgs(SimplifyTasks, PredId, ProcId, !ModuleInfo,
        !ProcInfo, _).

simplify_goal_update_vars_in_proc(SimplifyTasks, !ModuleInfo,
        PredId, ProcId, !ProcInfo, InstMap0, !Goal, CostDelta) :-
    simplify_info_init(!.ModuleInfo, PredId, ProcId, !.ProcInfo,
        SimplifyTasks, SimplifyInfo0),
    % The nested context we construct is probably a lie; we don't actually
    % know whether we are inside a goal duplicated for a switch, or a lambda,
    % or a model_non procedure. However, this should be ok. The nested context
    % is used for deciding what warnings and errors to generate, and
    % we are not interested in those.
    InsideDuplForSwitch = no,
    NumEnclosingBarriers = 0u,
    ProcIsModelNon = no,
    NestedContext0 = simplify_nested_context(InsideDuplForSwitch,
        ProcIsModelNon, NumEnclosingBarriers),
    simplify_top_level_goal(!Goal, NestedContext0, InstMap0,
        SimplifyInfo0, SimplifyInfo),

    simplify_info_get_module_info(SimplifyInfo, !:ModuleInfo),

    simplify_info_get_varset(SimplifyInfo, VarSet),
    simplify_info_get_var_types(SimplifyInfo, VarTypes),
    simplify_info_get_rtti_varmaps(SimplifyInfo, RttiVarMaps),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),

    simplify_info_get_cost_delta(SimplifyInfo, CostDelta).

%-----------------------------------------------------------------------------%

    % Simplify the given procedure. Return the resulting error messages.
    %
:- pred simplify_proc_return_msgs(simplify_tasks::in, pred_id::in,
    proc_id::in, module_info::in, module_info::out,
    proc_info::in, proc_info::out, list(error_spec)::out) is det.

simplify_proc_return_msgs(SimplifyTasks0, PredId, ProcId, !ModuleInfo,
        !ProcInfo, !:Specs) :-
    simplify_proc_maybe_vary_parameters(!.ModuleInfo, PredId, !.ProcInfo,
        SimplifyTasks0, SimplifyTasks),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_markers(PredInfo0, Markers0),
    ( if check_marker(Markers0, marker_mode_check_clauses) then
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
        check_marker(Markers0, marker_has_format_call),
        SimplifyTasks ^ do_format_calls = yes
    then
        (
            SimplifyTasks ^ do_warn_implicit_stream_calls = no,
            ImplicitStreamWarnings = do_not_generate_implicit_stream_warnings
        ;
            SimplifyTasks ^ do_warn_implicit_stream_calls = yes,
            ImplicitStreamWarnings = generate_implicit_stream_warnings
        ),
        simplify_proc_analyze_and_format_calls(!ModuleInfo,
            ImplicitStreamWarnings, PredId, ProcId, FormatSpecs, !ProcInfo)
    else
        % Either there are no format calls to check, or we don't want to
        % optimize them and would ignore the added messages anyway.
        FormatSpecs = []
    ),

    simplify_info_init(!.ModuleInfo, PredId, ProcId, !.ProcInfo,
        SimplifyTasks, Info0),

    InsideDuplForSwitch = no,
    NumEnclosingBarriers = 0u,
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
    NestedContext0 = simplify_nested_context(InsideDuplForSwitch,
        ProcIsModelNon, NumEnclosingBarriers),
    proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InstMap0),

    proc_info_get_goal(!.ProcInfo, Goal0),
    simplify_top_level_goal(Goal0, Goal, NestedContext0, InstMap0,
        Info0, Info),
    proc_info_set_goal(Goal, !ProcInfo),

    simplify_info_get_varset(Info, VarSet0),
    simplify_info_get_var_types(Info, VarTypes1),
    simplify_info_get_rtti_varmaps(Info, RttiVarMaps),
    simplify_info_get_elim_vars(Info, ElimVarsLists0),
    % We sort the lists basically on the number of the first variable.
    list.sort(ElimVarsLists0, ElimVarsLists),
    list.condense(ElimVarsLists, ElimVars),
    varset.delete_sorted_vars(ElimVars, VarSet0, VarSet1),
    delete_sorted_var_types(ElimVars, VarTypes1, VarTypes),
    % We only eliminate vars that cannot occur in RttiVarMaps.
    ( if simplify_do_after_front_end(Info) then
        proc_info_get_var_name_remap(!.ProcInfo, VarNameRemap),
        map.foldl(varset.name_var, VarNameRemap, VarSet1, VarSet),
        proc_info_set_var_name_remap(map.init, !ProcInfo)
    else
        VarSet = VarSet0
    ),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),

    simplify_info_get_has_parallel_conj(Info, HasParallelConj),
    proc_info_set_has_parallel_conj(HasParallelConj, !ProcInfo),

    simplify_info_get_has_user_event(Info, HasUserEvent),
    proc_info_set_has_user_event(HasUserEvent, !ProcInfo),

    simplify_info_get_deleted_call_callees(Info, CurDeletedCallCallees),
    proc_info_get_deleted_call_callees(!.ProcInfo, DeletedCallCallees0),
    set.union(CurDeletedCallCallees, DeletedCallCallees0, DeletedCallCallees),
    proc_info_set_deleted_call_callees(DeletedCallCallees, !ProcInfo),

    simplify_info_get_module_info(Info, !:ModuleInfo),
    simplify_info_get_error_specs(Info, !:Specs),
    !:Specs = FormatSpecs ++ !.Specs,

    simplify_proc_maybe_warn_about_duplicates(!.ModuleInfo, PredId, !.ProcInfo,
        !Specs),

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
    proc_info_get_vartypes(ProcInfo, VarTypes0),
    vartypes_count(VarTypes0, NumVars),
    ( if NumVars > turn_off_common_struct_threshold then
        % If we have too many variables, common_struct takes so long that
        % either the compiler runs out of memory or the user runs out of
        % patience. The fact that we would generate better code if the
        % compilation finished is therefore of limited interest.
        !SimplifyTasks ^ do_common_struct := no
    else
        true
    ),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_string_option(Globals, common_struct_preds,
        CommonStructPreds),
    ( if CommonStructPreds = "" then
        true
    else
        CommonStructPredIdStrs = string.split_at_char(',', CommonStructPreds),
        ( if
            list.map(string.to_int, CommonStructPredIdStrs,
                CommonStructPredIdInts)
        then
            PredIdInt = pred_id_to_int(PredId),
            ( if list.member(PredIdInt, CommonStructPredIdInts) then
                true
            else
                !SimplifyTasks ^ do_common_struct := no
            )
        else
            true
        )
    ).

:- func turn_off_common_struct_threshold = int.

turn_off_common_struct_threshold = 1000.

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

:- pred simplify_proc_analyze_and_format_calls(
    module_info::in, module_info::out,
    maybe_generate_implicit_stream_warnings::in, pred_id::in, proc_id::in,
    list(error_spec)::out, proc_info::in, proc_info::out) is det.

simplify_proc_analyze_and_format_calls(!ModuleInfo, ImplicitStreamWarnings,
        PredId, ProcId, FormatSpecs, !ProcInfo) :-
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_varset(!.ProcInfo, VarSet0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    analyze_and_optimize_format_calls(!.ModuleInfo, ImplicitStreamWarnings,
        Goal0, MaybeGoal, FormatSpecs, VarSet0, VarSet, VarTypes0, VarTypes),
    (
        MaybeGoal = yes(Goal),
        proc_info_set_goal(Goal, !ProcInfo),
        proc_info_set_varset(VarSet, !ProcInfo),
        proc_info_set_vartypes(VarTypes, !ProcInfo),

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
        requantify_proc_general(ordinary_nonlocals_maybe_lambda, !ProcInfo),
        recompute_instmap_delta_proc(do_not_recompute_atomic_instmap_deltas,
            !ProcInfo, !ModuleInfo),

        % Put the new proc_info back into !ModuleInfo, since some of the
        % following code could otherwise find obsolete information in there.

        % Remove the has_format_call marker from the pred_info before
        % putting it back, since any optimizable format calls will already
        % have been optimized. Since currently there is no program
        % transformation that inserts calls to these predicates,
        % there is no point in invoking find_format_call again later.

        module_info_get_preds(!.ModuleInfo, PredTable0),
        map.lookup(PredTable0, PredId, PredInfo0),
        pred_info_get_proc_table(PredInfo0, ProcTable0),
        map.det_update(ProcId, !.ProcInfo, ProcTable0, ProcTable),
        pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo1),

        pred_info_get_markers(PredInfo1, Markers1),
        remove_marker(marker_has_format_call, Markers1, Markers),
        pred_info_set_markers(Markers, PredInfo1, PredInfo),

        map.det_update(PredId, PredInfo, PredTable0, PredTable),
        module_info_set_preds(PredTable, !ModuleInfo)
    ;
        MaybeGoal = no
        % There should not be any updates to the varset and the vartypes,
        % but even if there are, throw them away, since they apply to a version
        % of the goal that we will not be using.
    ).

:- pred simplify_proc_maybe_warn_about_duplicates(module_info::in, pred_id::in,
    proc_info::in, list(error_spec)::in, list(error_spec)::out) is det.

simplify_proc_maybe_warn_about_duplicates(ModuleInfo, PredId, ProcInfo,
        !Specs) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),

    % The alternate goal by definition cannot be a call_foreign_proc.
    proc_info_get_goal(ProcInfo, Goal),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    ( if
        GoalExpr = call_foreign_proc(Attributes, _, _, _, _, _, _),
        MaybeMayDuplicate = get_may_duplicate(Attributes),
        MaybeMayDuplicate = yes(MayDuplicate)
    then
        (
            MayDuplicate = proc_may_duplicate,
            ( if check_marker(Markers, marker_user_marked_no_inline) then
                Context = goal_info_get_context(GoalInfo),
                Pieces = [words("Error: the"), quote("may_duplicate"),
                    words("attribute on the foreign_proc contradicts the"),
                    quote("no_inline"), words("pragma on the predicate.")],
                Spec = simplest_spec($pred, severity_error,
                    phase_simplify(report_in_any_mode), Context, Pieces),
                !:Specs = [Spec | !.Specs]
            else
                true
            )
        ;
            MayDuplicate = proc_may_not_duplicate,
            ( if check_marker(Markers, marker_user_marked_inline) then
                Context = goal_info_get_context(GoalInfo),
                Pieces = [words("Error: the"), quote("may_not_duplicate"),
                    words("attribute on the foreign_proc contradicts the"),
                    quote("inline"), words("pragma on the predicate.")],
                Spec = simplest_spec($pred, severity_error,
                    phase_simplify(report_in_any_mode), Context, Pieces),
                !:Specs = [Spec | !.Specs]
            else
                true
            )
        )
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- pred simplify_top_level_goal(hlds_goal::in, hlds_goal::out,
    simplify_nested_context::in, instmap::in,
    simplify_info::in, simplify_info::out) is det.

simplify_top_level_goal(!Goal, NestedContext0, InstMap0, !Info) :-
    % Simplification is done in two passes. The first pass performs common
    % structure and duplicate call elimination. The second pass performs excess
    % assignment elimination and cleans up the code after the first pass.
    %
    % Two passes are required because the goal must be requantified after the
    % optimizations in common.m are run so that excess assignment elimination
    % works properly.

    some [!SimplifyTasks] (
        simplify_info_get_simplify_tasks(!.Info, !:SimplifyTasks),
        OriginalSimplifyTasks = !.SimplifyTasks,
        ( if
            ( simplify_do_common_struct(!.Info)
            ; simplify_do_opt_duplicate_calls(!.Info)
            )
        then
            !SimplifyTasks ^ do_mark_code_model_changes := no,
            !SimplifyTasks ^ do_excess_assign := no,
            simplify_info_set_simplify_tasks(!.SimplifyTasks, !Info),

            do_process_top_level_goal(!Goal, NestedContext0, InstMap0, !Info),

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

            % These tasks were done in pass 1. Repeating them in pass 2
            % would serve no purpose, since we do nothing in pass 1
            % that would generate new occurrences of the situations
            % that these tasks seek to optimize.
            !SimplifyTasks ^ do_common_struct := no,
            !SimplifyTasks ^ do_opt_duplicate_calls := no,
            simplify_info_reinit(!.SimplifyTasks, !Info)
        else
            true
        ),
        % On the second pass do excess assignment elimination and
        % some cleaning up after the common structure pass.
        do_process_top_level_goal(!Goal, NestedContext0, InstMap0, !Info),

        simplify_info_get_found_contains_trace(!.Info, FoundContainsTrace),
        (
            FoundContainsTrace = no
        ;
            FoundContainsTrace = yes,
            goal_contains_trace(!Goal, _)
        )
    ).

:- pred do_process_top_level_goal(hlds_goal::in, hlds_goal::out,
    simplify_nested_context::in, instmap::in,
    simplify_info::in, simplify_info::out) is det.

do_process_top_level_goal(!Goal, NestedContext0, InstMap0, !Info) :-
    !.Goal = hlds_goal(_, GoalInfo0),
    Detism = goal_info_get_determinism(GoalInfo0),
    NonLocals = goal_info_get_nonlocals(GoalInfo0),

    simplify_goal(!Goal, NestedContext0, InstMap0,
        common_info_init, _Common, !Info),

    simplify_info_get_should_requantify(!.Info, ShouldRequantify),
    (
        ShouldRequantify = yes,
        some [!VarSet, !VarTypes, !RttiVarMaps, !ModuleInfo] (
            simplify_info_get_varset(!.Info, !:VarSet),
            simplify_info_get_var_types(!.Info, !:VarTypes),
            simplify_info_get_rtti_varmaps(!.Info, !:RttiVarMaps),
            implicitly_quantify_goal_general(ordinary_nonlocals_maybe_lambda,
                NonLocals, _, !Goal, !VarSet, !VarTypes, !RttiVarMaps),

            simplify_info_set_varset(!.VarSet, !Info),
            simplify_info_set_var_types(!.VarTypes, !Info),
            simplify_info_set_rtti_varmaps(!.RttiVarMaps, !Info),

            % Always recompute instmap_deltas for atomic goals - this is safer
            % in the case where unused variables should no longer be included
            % in the instmap_delta for a goal.
            % In the alias branch this is necessary anyway.
            simplify_info_get_module_info(!.Info, !:ModuleInfo),
            simplify_info_get_inst_varset(!.Info, InstVarSet),
            recompute_instmap_delta(recompute_atomic_instmap_deltas, !Goal,
                !.VarTypes, InstVarSet, InstMap0, !ModuleInfo),
            simplify_info_set_module_info(!.ModuleInfo, !Info)
        )
    ;
        ShouldRequantify = no
    ),
    simplify_info_get_should_rerun_det(!.Info, ShouldRerunDet),
    (
        ShouldRerunDet = yes,
        some [!VarSet, !VarTypes, !RttiVarMaps, !ModuleInfo, !ProcInfo]
        (
            det_get_soln_context(Detism, SolnContext),

            % Det_infer_goal looks up the proc_info in the module_info
            % for the vartypes, so we'd better stick them back in the
            % module_info.
            simplify_info_get_module_info(!.Info, !:ModuleInfo),
            simplify_info_get_varset(!.Info, !:VarSet),
            simplify_info_get_var_types(!.Info, !:VarTypes),
            simplify_info_get_rtti_varmaps(!.Info, !:RttiVarMaps),
            simplify_info_get_pred_proc_id(!.Info, PredProcId),
            module_info_pred_proc_info(!.ModuleInfo, PredProcId,
                PredInfo, !:ProcInfo),
            proc_info_set_vartypes(!.VarTypes, !ProcInfo),
            proc_info_set_varset(!.VarSet, !ProcInfo),
            proc_info_set_rtti_varmaps(!.RttiVarMaps, !ProcInfo),
            module_info_set_pred_proc_info(PredProcId,
                PredInfo, !.ProcInfo, !ModuleInfo),
            simplify_info_set_module_info(!.ModuleInfo, !Info),

            det_info_init(!.ModuleInfo, PredProcId, !.VarSet, !.VarTypes,
                pess_extra_vars_report, [], DetInfo0),
            det_infer_goal(!Goal, InstMap0, SolnContext, [], no,
                _, _, DetInfo0, DetInfo),
            det_info_get_module_info(DetInfo, !:ModuleInfo),
            det_info_get_vartypes(DetInfo, !:VarTypes),
            simplify_info_set_module_info(!.ModuleInfo, !Info),
            simplify_info_set_var_types(!.VarTypes, !Info)
        )
    ;
        ShouldRerunDet = no
    ).

%-----------------------------------------------------------------------------%

:- pred goal_contains_trace(hlds_goal::in, hlds_goal::out,
    contains_trace_goal::out) is det.

goal_contains_trace(Goal0, Goal, ContainsTrace) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0,
        ContainsTrace = contains_no_trace_goal
    ;
        GoalExpr0 = conj(ConjType, SubGoals0),
        ContainsTrace0 = contains_no_trace_goal,
        goal_list_contains_trace_acc(SubGoals0, SubGoals,
            ContainsTrace0, ContainsTrace),
        GoalExpr = conj(ConjType, SubGoals)
    ;
        GoalExpr0 = disj(SubGoals0),
        ContainsTrace0 = contains_no_trace_goal,
        goal_list_contains_trace_acc(SubGoals0, SubGoals,
            ContainsTrace0, ContainsTrace),
        GoalExpr = disj(SubGoals)
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        ContainsTrace0 = contains_no_trace_goal,
        case_list_contains_trace_acc(Cases0, Cases,
            ContainsTrace0, ContainsTrace),
        GoalExpr = switch(SwitchVar, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        goal_contains_trace(Cond0, Cond, CondContainsTrace),
        goal_contains_trace(Then0, Then, ThenContainsTrace),
        goal_contains_trace(Else0, Else, ElseContainsTrace),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        ContainsTrace = worst_contains_trace(CondContainsTrace,
            worst_contains_trace(ThenContainsTrace, ElseContainsTrace))
    ;
        GoalExpr0 = negation(SubGoal0),
        goal_contains_trace(SubGoal0, SubGoal, ContainsTrace),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        (
            Reason = trace_goal(_, _, _, _, _),
            SubGoal = SubGoal0,
            ContainsTrace = contains_trace_goal
        ;
            Reason = from_ground_term(_, FGT),
            (
                ( FGT = from_ground_term_construct
                ; FGT = from_ground_term_deconstruct
                ),
                SubGoal = SubGoal0,
                ContainsTrace = contains_no_trace_goal
            ;
                ( FGT = from_ground_term_initial
                ; FGT = from_ground_term_other
                ),
                goal_contains_trace(SubGoal0, SubGoal, ContainsTrace)
            )
        ;
            ( Reason = disable_warnings(_, _)
            ; Reason = exist_quant(_)
            ; Reason = promise_solutions(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = loop_control(_, _, _)
            ),
            goal_contains_trace(SubGoal0, SubGoal, ContainsTrace)
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            goal_contains_trace(MainGoal0, MainGoal, MainContainsTrace),
            OrElseContainsTrace0 = contains_no_trace_goal,
            goal_list_contains_trace_acc(OrElseGoals0, OrElseGoals,
                OrElseContainsTrace0, OrElseContainsTrace),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners),
            GoalExpr = shorthand(ShortHand),
            ContainsTrace = worst_contains_trace(MainContainsTrace,
                OrElseContainsTrace)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            goal_contains_trace(SubGoal0, SubGoal, ContainsTrace),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal),
            GoalExpr = shorthand(ShortHand)
        ;
            ShortHand0 = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        )
    ),
    (
        ContainsTrace = contains_trace_goal,
        goal_info_add_feature(feature_contains_trace, GoalInfo0, GoalInfo)
    ;
        ContainsTrace = contains_no_trace_goal,
        goal_info_remove_feature(feature_contains_trace, GoalInfo0, GoalInfo)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred goal_list_contains_trace_acc(list(hlds_goal)::in, list(hlds_goal)::out,
    contains_trace_goal::in, contains_trace_goal::out) is det.

goal_list_contains_trace_acc([], [], !ContainsTrace).
goal_list_contains_trace_acc([Goal0 | Goals0], [Goal | Goals],
        !ContainsTrace) :-
    goal_contains_trace(Goal0, Goal, GoalContainsTrace),
    !:ContainsTrace = worst_contains_trace(GoalContainsTrace, !.ContainsTrace),
    goal_list_contains_trace_acc(Goals0, Goals, !ContainsTrace).

:- pred case_list_contains_trace_acc(list(case)::in, list(case)::out,
    contains_trace_goal::in, contains_trace_goal::out) is det.

case_list_contains_trace_acc([], [], !ContainsTrace).
case_list_contains_trace_acc([Case0 | Cases0], [Case | Cases],
        !ContainsTrace) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    goal_contains_trace(Goal0, Goal, GoalContainsTrace),
    Case = case(MainConsId, OtherConsIds, Goal),
    !:ContainsTrace = worst_contains_trace(GoalContainsTrace, !.ContainsTrace),
    case_list_contains_trace_acc(Cases0, Cases, !ContainsTrace).

%-----------------------------------------------------------------------------%

simplify_may_introduce_calls(ModuleName, PredName, _Arity) :-
    % For some reason, the compiler records the original arity of
    % int.unchecked_quotient as 3, not 2. Don't check the arities
    % until this is fixed.
    (
        ModuleName = "private_builtin",
        ( PredName = "builtin_compound_eq"
        ; PredName = "builtin_compound_lt"
        ; PredName = "state_var_copy"
        ; PredName = "builtin_int_lt"
        ; PredName = "builtin_int_gt"
        ; PredName = "builtin_int8_lt"
        ; PredName = "builtin_int8_gt"
        ; PredName = "builtin_int16_lt"
        ; PredName = "builtin_int16_gt"
        ; PredName = "builtin_int32_lt"
        ; PredName = "builtin_int32_gt"
        ; PredName = "builtin_int64_lt"
        ; PredName = "builtin_int64_gt"
        ; PredName = "builtin_uint_lt"
        ; PredName = "builtin_uint_gt"
        ; PredName = "builtin_uint8_lt"
        ; PredName = "builtin_uint8_gt"
        ; PredName = "builtin_uint16_lt"
        ; PredName = "builtin_uint16_gt"
        ; PredName = "builtin_uint32_lt"
        ; PredName = "builtin_uint32_gt"
        ; PredName = "builtin_uint64_lt"
        ; PredName = "builtin_uint64_gt"
        )
    ;
        ( ModuleName = "int"
        ; ModuleName = "uint"
        ; ModuleName = "int8"
        ; ModuleName = "uint8"
        ; ModuleName = "int16"
        ; ModuleName = "uint16"
        ; ModuleName = "int32"
        ; ModuleName = "uint32"
        ; ModuleName = "int64"
        ; ModuleName = "uint64"
        ),
        ( PredName = "*"
        ; PredName = "unchecked_quotient"
        ; PredName = "unchecked_rem"
        ; PredName = "unchecked_left_shift"
        ; PredName = "unchecked_right_shift"
        )
    ;
        ModuleName = "io",
        PredName = "write_string"
    ;
        ModuleName = "string",
        ( PredName = "int_to_string"
        ; PredName = "char_to_string"
        ; PredName = "float_to_string"
        ; PredName = "++"
        )
    ;
        ModuleName = "string.format",
        ( PredName = "format_char_component_nowidth"
        ; PredName = "format_char_component_width"
        ; PredName = "format_string_component_nowidth_noprec"
        ; PredName = "format_string_component_nowidth_prec"
        ; PredName = "format_string_component_width_noprec"
        ; PredName = "format_string_component_width_prec"
        ; PredName = "format_signed_int_component_nowidth_noprec"
        ; PredName = "format_signed_int_component_nowidth_prec"
        ; PredName = "format_signed_int_component_width_noprec"
        ; PredName = "format_signed_int_component_width_prec"
        ; PredName = "format_unsigned_int_component_nowidth_noprec"
        ; PredName = "format_unsigned_int_component_nowidth_prec"
        ; PredName = "format_unsigned_int_component_width_noprec"
        ; PredName = "format_unsigned_int_component_width_prec"
        ; PredName = "format_uint_component_nowidth_noprec"
        ; PredName = "format_uint_component_nowidth_prec"
        ; PredName = "format_uint_component_width_noprec"
        ; PredName = "format_uint_component_width_prec"
        ; PredName = "format_float_component_nowidth_noprec"
        ; PredName = "format_float_component_nowidth_prec"
        ; PredName = "format_float_component_width_noprec"
        ; PredName = "format_float_component_width_prec"
        )
    ;
        ModuleName = "stream",
        PredName = "put"
    ;
        ModuleName = "table_builtin",

        ( PredName = "table_lookup_insert_start_int"
        ; PredName = "table_lookup_insert_int"
        ; PredName = "table_lookup_insert_float"
        ; PredName = "table_lookup_insert_char"
        ; PredName = "table_lookup_insert_string"
        ; PredName = "table_lookup_insert_enum"
        ; PredName = "table_lookup_insert_foreign_enum"
        ; PredName = "table_lookup_insert_gen"
        ; PredName = "table_lookup_insert_addr"
        ; PredName = "table_lookup_insert_poly"
        ; PredName = "table_lookup_insert_poly_addr"
        ; PredName = "table_lookup_insert_typeinfo"
        ; PredName = "table_lookup_insert_typeclassinfo"

        ; PredName = "table_lookup_save_int_answer"
        ; PredName = "table_lookup_save_char_answer"
        ; PredName = "table_lookup_save_string_answer"
        ; PredName = "table_lookup_save_float_answer"
        ; PredName = "table_lookup_save_io_state_answer"
        ; PredName = "table_lookup_save_any_answer"

        ; PredName = "table_lookup_restore_int_answer"
        ; PredName = "table_lookup_restore_char_answer"
        ; PredName = "table_lookup_restore_string_answer"
        ; PredName = "table_lookup_restore_float_answer"
        ; PredName = "table_lookup_restore_io_state_answer"
        ; PredName = "table_lookup_restore_any_answer"

        ; PredName = "table_loop_setup"
        ; PredName = "table_loop_setup_shortcut"
        ; PredName = "table_loop_mark_as_inactive"
        ; PredName = "table_loop_mark_as_inactive_and_fail"
        ; PredName = "table_loop_mark_as_active_and_fail"

        ; PredName = "table_memo_det_setup"
        ; PredName = "table_memo_det_setup_shortcut"
        ; PredName = "table_memo_semi_setup"
        ; PredName = "table_memo_semi_setup_shortcut"
        ; PredName = "table_memo_non_setup"
        ; PredName = "table_memo_mark_as_failed"
        ; PredName = "table_memo_mark_as_succeeded"
        ; PredName = "table_memo_mark_as_incomplete"
        ; PredName = "table_memo_mark_as_active_and_fail"
        ; PredName = "table_memo_mark_as_complete_and_fail"
        ; PredName = "table_memo_create_answer_block"
        ; PredName = "table_memo_get_answer_block"
        ; PredName = "table_memo_non_get_answer_table"
        ; PredName = "table_memo_non_answer_is_not_duplicate"
        ; PredName = "table_memo_non_answer_is_not_duplicate_shortcut"
        ; PredName = "table_memo_return_all_answers_nondet"
        ; PredName = "table_memo_return_all_answers_multi"
        ; PredName = "table_memo_non_return_all_shortcut"

        ; PredName = "table_io_in_range"
        ; PredName = "table_io_has_occurred"
        ; PredName = "table_io_copy_io_state"
        ; PredName = "table_io_left_bracket_unitized_goal"
        ; PredName = "table_io_right_bracket_unitized_goal"

        ; PredName = "table_mm_setup"
        ; PredName = "table_mm_suspend_consumer"
        ; PredName = "table_mm_completion"
        ; PredName = "table_mm_get_answer_table"
        ; PredName = "table_mm_answer_is_not_duplicate"
        ; PredName = "table_mm_answer_is_not_duplicate_shortcut"
        ; PredName = "table_mm_create_answer_block"
        ; PredName = "table_mm_fill_answer_block_shortcut"
        ; PredName = "table_mm_return_all_nondet"
        ; PredName = "table_mm_return_all_multi"
        ; PredName = "table_mm_return_all_shortcut"

        ; PredName = "table_mmos_save_inputs"
        ; PredName = "table_mmos_setup_consumer"
        ; PredName = "table_mmos_answer_is_not_duplicate"
        ; PredName = "table_mmos_answer_is_not_duplicate_shortcut"
        ; PredName = "table_mmos_consume_next_answer_nondet"
        ; PredName = "table_mmos_consume_next_answer_multi"
        ; PredName = "table_mmos_restore_answers"
        ; PredName = "table_mmos_pickup_inputs"
        ; PredName = "table_mmos_create_answer_block"
        ; PredName = "table_mmos_return_answer"
        ; PredName = "table_mmos_completion"

        ; PredName = "table_error"
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_proc.
%-----------------------------------------------------------------------------%
