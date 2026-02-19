%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: unused_args.m.
% Main author: stayl.
%
% Detects and removes unused input arguments in procedures, especially
% type_infos.
%
% To enable the warnings use `--warn-unused-args'.
% To enable the optimisation use `--optimize-unused-args'.
%
% An argument is considered used if it (or any of its aliases) are
%   - used in a call to a predicate external to the current module
%   - used in a higher-order call
%   - used to instantiate an output variable
%   - involved in a simple test, switch or a semidet deconstruction
%   - used as an argument to another predicate in this module which is used.
%
% When using typeinfo liveness, the following variables are also
% considered used:
%   - a type-info (or part of a type-info) of a type parameter of the
%     type of a variable that is used (for example, if a variable
%     of type list(T) is used, then the type_info for T is used)
%
% The first step is to determine which arguments of which predicates are used
% locally to their predicate. For each unused argument, a set of other
% arguments that it depends on is built up.
%
% The next step is to iterate over the this map, checking for each unused
% argument whether any of the arguments it depends on has become used in the
% last iteration. Iterations are repeated until a fixpoint is reached.
%
% Warnings are then output. The warning message indicates which arguments are
% used in none of the modes of a predicate.
%
% The predicates are then fixed up. Unused variables and unifications are
% removed.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.unused_args.
:- interface.

:- import_module analysis.
:- import_module analysis.framework.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- type maybe_gather_pragma_unused_args
    --->    do_not_gather_pragma_unused_args
    ;       do_gather_pragma_unused_args.

:- type maybe_record_analysis_unused_args
    --->    do_not_record_analysis_unused_args
    ;       do_record_analysis_unused_args.

:- pred unused_args_process_module(maybe_gather_pragma_unused_args::in,
    maybe_record_analysis_unused_args::in,
    list(error_spec)::out, set(gen_pragma_unused_args_info)::out,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%
% Instances used by mmc_analysis.m.
%

:- type unused_args_func_info.
:- type unused_args_call.
:- type unused_args_answer.

:- instance analysis(unused_args_func_info, unused_args_call,
    unused_args_answer).

:- instance partial_order(unused_args_func_info, unused_args_call).
:- instance call_pattern(unused_args_func_info, unused_args_call).
:- instance to_term(unused_args_call).

:- instance partial_order(unused_args_func_info, unused_args_answer).
:- instance answer_pattern(unused_args_func_info, unused_args_answer).
:- instance to_term(unused_args_answer).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.operations.
:- import_module check_hlds.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_test.
:- import_module hlds.goal_refs.
:- import_module hlds.goal_vars.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module hlds.type_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_parse_tree.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module term_conversion.
:- import_module varset.

%---------------------------------------------------------------------------%

unused_args_process_module(GatherPragmas, RecordAnalysis,
        Specs, PragmaUnusedArgInfos, !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    init_global_var_usage_map(GlobalVarUsageMap0, FixpointPredProcIds,
        NewProcMap0, !ModuleInfo),
    % maybe_write_string(VeryVerbose, "% Finished initialisation.\n", !IO),

    unused_args_iterate_to_fixpoint(0, !.ModuleInfo, FixpointPredProcIds,
        GlobalVarUsageMap0, GlobalVarUsageMap),
    % maybe_write_string(VeryVerbose, "% Finished analysis.\n", !IO),

    map.init(UnusedArgInfo0),
    get_unused_arg_info(!.ModuleInfo, GlobalVarUsageMap, FixpointPredProcIds,
        UnusedArgInfo0, UnusedArgInfo),

    map.keys(UnusedArgInfo, PredProcIdsToFix),
    globals.lookup_bool_option(Globals, warn_unused_args, DoWarnBool),
    ( DoWarnBool = no,  DoWarn = do_not_warn_unused_args
    ; DoWarnBool = yes, DoWarn = do_warn_unused_args
    ),
    ( if
        ( DoWarn = do_warn_unused_args
        ; GatherPragmas = do_gather_pragma_unused_args
        )
    then
        set.init(WarnedPredIds0),
        gather_warnings_and_pragmas(!.ModuleInfo, UnusedArgInfo,
            DoWarn, GatherPragmas, PredProcIdsToFix, WarnedPredIds0,
            [], Specs, set.init, PragmaUnusedArgInfos)
    else
        Specs = [],
        set.init(PragmaUnusedArgInfos)
    ),
    (
        RecordAnalysis = do_record_analysis_unused_args,
        module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
        module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
        list.foldl(
            maybe_record_intermod_unused_args(!.ModuleInfo, UnusedArgInfo),
            PredIds, AnalysisInfo0, AnalysisInfo1),
        list.foldl(record_intermod_dependencies(!.ModuleInfo),
            FixpointPredProcIds, AnalysisInfo1, AnalysisInfo),
        module_info_set_analysis_info(AnalysisInfo, !ModuleInfo)
    ;
        RecordAnalysis = do_not_record_analysis_unused_args
    ),
    globals.get_opt_tuple(Globals, OptTuple),
    OptUnusedArgs = OptTuple ^ ot_opt_unused_args,
    (
        OptUnusedArgs = opt_unused_args,
        list.foldl2(unused_args_create_new_pred(UnusedArgInfo),
            PredProcIdsToFix, NewProcMap0, NewProcMap, !ModuleInfo),
        % maybe_write_string(VeryVerbose, "% Finished new preds.\n", !IO),
        delete_unused_args_in_module(VeryVerbose, GlobalVarUsageMap,
            FixpointPredProcIds, NewProcMap, !ModuleInfo),
        % maybe_write_string(VeryVerbose, "% Fixed up goals.\n", !IO),
        ( if map.is_empty(NewProcMap) then
            true
        else
            % The dependencies have changed, so any old dependency graph
            % is now invalid.
            module_info_clobber_dependency_info(!ModuleInfo)
        )
    ;
        OptUnusedArgs = do_not_opt_unused_args
    ).

%---------------------------------------------------------------------------%
%
% Initialisation section.
%

    % Set initial status of all args of local procs by examining the
    % module_info. PredProcList is the list of procedures to do the fixpoint
    % iteration over.
    %
    % The reason why we take the module_info as a read/write argument
    % instead of as a read-only argument is that we may need to update it
    % to record analysis results.
    %
:- pred init_global_var_usage_map(global_var_usage_map::out,
    list(pred_proc_id)::out, new_proc_map::out,
    module_info::in, module_info::out) is det.

init_global_var_usage_map(GlobalVarUsageMap, FixpointPredProcIds, NewProcMap,
        !ModuleInfo) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    init_global_var_usage_map_for_preds(PredIds, map.init, GlobalVarUsageMap,
        [], FixpointPredProcIds, map.init, NewProcMap, !ModuleInfo).

    % Setup args for the whole module.
    %
:- pred init_global_var_usage_map_for_preds(list(pred_id)::in,
    global_var_usage_map::in, global_var_usage_map::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    new_proc_map::in, new_proc_map::out,
    module_info::in, module_info::out) is det.

init_global_var_usage_map_for_preds([], !GlobalVarUsageMap,
        !FixpointPredProcIds, !OptProcs, !ModuleInfo).
init_global_var_usage_map_for_preds([PredId | PredIds], !GlobalVarUsageMap,
        !FixpointPredProcIds, !OptProcs, !ModuleInfo) :-
    maybe_init_global_var_usage_map_for_pred(PredId, !GlobalVarUsageMap,
        !FixpointPredProcIds, !OptProcs, !ModuleInfo),
    init_global_var_usage_map_for_preds(PredIds, !GlobalVarUsageMap,
        !FixpointPredProcIds, !OptProcs, !ModuleInfo).

    % Setup args for the given predicate if required.
    %
:- pred maybe_init_global_var_usage_map_for_pred(pred_id::in,
    global_var_usage_map::in, global_var_usage_map::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    new_proc_map::in, new_proc_map::out,
    module_info::in, module_info::out) is det.

maybe_init_global_var_usage_map_for_pred(PredId, !GlobalVarUsageMap,
        !FixpointPredProcIds, !OptProcs, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ( if
        % Check whether we must (or should) consider all arguments
        % of the predicate to be used.
        (
            % Builtins *do* use all their arguments.
            pred_info_is_builtin(PredInfo)
        ;
            % To avoid spurious warnings in their callers, we want to treat
            % stub procedures (those which originally had no clauses)
            % as if they use all of their arguments,
            pred_info_get_markers(PredInfo, Markers),
            marker_is_present(Markers, marker_stub)
        ;
            % The method of a class instance must have the exact same set
            % or arguments as the class method itself. We cannot delete
            % an argument, even an unused argument, from an instance method
            % without deleting that same argument from the class method,
            % which we cannot do unless that argument is unused in *all*
            % instances of the class, a condition we cannot check here.
            %
            % The above is why we cannot *replace* a class or instance method
            % procedure with their unused-arg-optimized clone. We *could* make
            % a class or instance method *forward* its job to such a clone,
            % but since class and instance methods cannot be recursive,
            % there would be no point.
            pred_info_get_origin(PredInfo, Origin),
            Origin = origin_user(OriginUser),
            ( OriginUser = user_made_class_method(_, _)
            ; OriginUser = user_made_instance_method(_, _)
            )
        )
    then
        true
    else
        pred_info_get_proc_table(PredInfo, ProcMap),
        map.foldl4(
            init_global_var_usage_map_entry_for_proc(PredId, PredInfo),
            ProcMap,
            !GlobalVarUsageMap, !FixpointPredProcIds, !OptProcs, !ModuleInfo)
    ).

    % Setup args for the procedure.
    %
    % XXX Document the meaning of the arguments.
    %
:- pred init_global_var_usage_map_entry_for_proc(pred_id::in, pred_info::in,
    proc_id::in, proc_info::in,
    global_var_usage_map::in, global_var_usage_map::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    new_proc_map::in, new_proc_map::out,
    module_info::in, module_info::out) is det.

init_global_var_usage_map_entry_for_proc(PredId, PredInfo, ProcId, ProcInfo,
        !GlobalVarUsageMap, !FixpointPredProcIds, !OptProcs, !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, intermodule_analysis, Intermod),
    ( if
        % Don't use the intermodule analysis info when we have the clauses
        % (opt_imported preds) since we may be able to do better with the
        % information in this module.
        Intermod = yes,
        pred_info_is_imported_not_external(PredInfo),
        not is_unify_index_or_compare_pred(PredInfo)
    then
        try_to_look_up_global_var_usage_map_entry_for_proc(PredId, PredInfo,
            ProcId, ProcInfo, !GlobalVarUsageMap, !OptProcs, !ModuleInfo)
    else if
        should_ignore_proc_unused_args(PredInfo, ProcId, ProcInfo)
    then
        true
    else
        proc_info_get_var_table(ProcInfo, VarTable),
        var_table_vars(VarTable, Vars),
        some [!LocalVarUsageMap] (
            map.init(!:LocalVarUsageMap),
            set_vars_to_unaliased_unused(Vars, !LocalVarUsageMap),
            record_output_args_as_used(!.ModuleInfo, ProcInfo,
                !LocalVarUsageMap),

            proc_interface_should_use_typeinfo_liveness(PredInfo, ProcId,
                Globals, TypeInfoLiveness),
            PredProcId = proc(PredId, ProcId),
            (
                TypeInfoLiveness = yes,
                proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
                setup_typeinfo_deps(PredProcId, VarTable, RttiVarMaps, Vars,
                    !LocalVarUsageMap)
            ;
                TypeInfoLiveness = no
            ),

            Info = unused_args_info(!.ModuleInfo, VarTable),
            proc_info_get_goal(ProcInfo, Goal),
            unused_args_traverse_goal(Info, Goal, !LocalVarUsageMap),
            map.det_insert(PredProcId, !.LocalVarUsageMap, !GlobalVarUsageMap)
        ),

        !:FixpointPredProcIds = [PredProcId | !.FixpointPredProcIds]
    ).

:- pred try_to_look_up_global_var_usage_map_entry_for_proc(
    pred_id::in, pred_info::in, proc_id::in, proc_info::in,
    global_var_usage_map::in, global_var_usage_map::out,
    new_proc_map::in, new_proc_map::out,
    module_info::in, module_info::out) is det.

try_to_look_up_global_var_usage_map_entry_for_proc(PredId, PredInfo,
        ProcId, ProcInfo, !GlobalVarUsageMap, !OptProcs, !ModuleInfo) :-
    PredModuleName = pred_info_module(PredInfo),
    pred_info_get_orig_arity(PredInfo, PredFormArity),
    FuncInfo = unused_args_func_info(PredFormArity),
    module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
    pred_info_proc_id_to_module_name_func_id(PredInfo, ProcId,
        ModuleId, FuncId),
    lookup_best_result(AnalysisInfo0, ModuleId, FuncId,
        FuncInfo, unused_args_call, MaybeBestResult),
    (
        MaybeBestResult = yes(analysis_result(_, BestAnswer, _)),
        BestAnswer = unused_args_answer(UnusedArgs),
        (
            UnusedArgs = [_ | _],
            proc_info_get_headvars(ProcInfo, HeadVars),
            list.map(list.det_index1(HeadVars), UnusedArgs, UnusedVars),
            set_vars_to_unaliased_unused(UnusedVars,
                map.init, LocalVarUsageMap),
            PredProcId = proc(PredId, ProcId),
            map.det_insert(PredProcId, LocalVarUsageMap, !GlobalVarUsageMap),
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.get_opt_tuple(Globals, OptTuple),
            OptUnusedArgs = OptTuple ^ ot_opt_unused_args,
            (
                OptUnusedArgs = opt_unused_args,
                make_imported_unused_args_pred_info(PredProcId, UnusedArgs,
                    !OptProcs, !ModuleInfo)
            ;
                OptUnusedArgs = do_not_opt_unused_args
            )
        ;
            UnusedArgs = []
        ),
        AnalysisInfo = AnalysisInfo0
    ;
        MaybeBestResult = no,
        record_request(analysis_name, PredModuleName, FuncId,
            unused_args_call, AnalysisInfo0, AnalysisInfo)
    ),
    module_info_set_analysis_info(AnalysisInfo, !ModuleInfo).

:- pred should_ignore_proc_unused_args(pred_info::in,
    proc_id::in, proc_info::in) is semidet.

should_ignore_proc_unused_args(PredInfo, ProcId, ProcInfo) :-
    (
        pred_info_is_imported(PredInfo)
    ;
        pred_info_is_pseudo_imported(PredInfo),
        hlds_pred.in_in_unification_proc_id(ProcId)
    ;
        % Unused argument optimization and tabling are
        % not compatible with each other.
        proc_info_get_eval_method(ProcInfo, EvalMethod),
        EvalMethod \= eval_normal
    ;
        proc_info_get_declared_determinism(ProcInfo,
            MaybeDeclaredDetism),
        proc_info_get_goal(ProcInfo, Goal),
        Goal = hlds_goal(_, GoalInfo),
        ActualDetism = goal_info_get_determinism(GoalInfo),
        % If there is a declared detism but the actual detism differs from it,
        % then replacing the Goal in ProcInfo with a forwarding call
        % to the clone of this procedure in which the unused args
        % have been eliminated can cause simplification to screw up.
        %
        % The scenario, as shown by Mantis bug #541, is the following.
        %
        % - A predicate is declared to be det (usually because it has to
        %   conform to an interface) but its body goal always throws
        %   an exception. Its actual determinism is therefore erroneous,
        %   and the body goal as a whole has "unreachable" as instmap delta.
        %
        % - The clone of this procedure which has the unused args deleted
        %   has the same declared determinism as the original, i.e. det.
        %
        % - Replacing the body with a call to the clone replaces
        %   an erroneous goal whose instmap_delta is unreachable
        %   with a det goal whose instmap_delta is unreachable.
        %
        %   It is this step that is at fault, for violating the invariant
        %   which says that a goal whose instmap_delta is "unreachable"
        %   should have determinism whose soln_count component is
        %   "at_most_zero", and vice versa.
        %
        % - The simplification pass we invoke just before code generation
        %   sees the contradiction. To make the program point as unreachable
        %   as the instmap_delta says it should be, it adds a "fail" goal
        %   after the call to the clone.
        %
        % - The code generator aborts when it finds "fail" in a det context.
        %
        % We could change the simplification pass to make the
        % supposed-to-be-unreachable end of the procedure body actually
        % unreachable by adding not "fail", but code that throws an exception.
        % However, there is no point in having unused args replacing code
        % that throws an exception with code that does a det call (whose body
        % throws an exception which we don't see because it is beyond
        % the predicate boundary), and then having simplification add code
        % to throw an exception afterward. It is much simpler not to allow
        % the unused arg transformation to replace the original exception
        % throwing code in the first place.
        (
            MaybeDeclaredDetism = yes(DeclaredDetism),
            DeclaredDetism \= ActualDetism
        ;
            determinism_components(ActualDetism, _CanFail, SolnCount),
            SolnCount = at_most_zero
        )
    ).

%---------------------%

:- pred set_vars_to_unaliased_unused(list(prog_var)::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

set_vars_to_unaliased_unused([], !LocalVarUsageMap).
set_vars_to_unaliased_unused([Var | Vars], !LocalVarUsageMap) :-
    set.init(AliasVars),
    set.init(AliasArgs),
    map.det_insert(Var, unused(AliasVars, AliasArgs), !LocalVarUsageMap),
    set_vars_to_unaliased_unused(Vars, !LocalVarUsageMap).

    % Record that all output arguments for a procedure as used.
    %
:- pred record_output_args_as_used(module_info::in, proc_info::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

record_output_args_as_used(ModuleInfo, ProcInfo, !LocalVarUsageMap) :-
    proc_info_instantiated_head_vars(ModuleInfo, ProcInfo,
        ChangedInstHeadVars),
    record_vars_as_used(ChangedInstHeadVars, !LocalVarUsageMap).

    % For each variable, ensure the typeinfos describing the type parameters
    % of the type of the variable depend on the head variable. For example,
    % if HeadVar1 has type list(T), then the type_info for T is used
    % if HeadVar1 is used.
    %
:- pred setup_typeinfo_deps(pred_proc_id::in, var_table::in, rtti_varmaps::in,
    list(prog_var)::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

setup_typeinfo_deps(_, _, _, [], !LocalVarUsageMap).
setup_typeinfo_deps(PredProcId, VarTable, RttiVarMaps, [Var | Vars],
        !LocalVarUsageMap) :-
    setup_typeinfo_dep(PredProcId, VarTable, RttiVarMaps, Var,
        !LocalVarUsageMap),
    setup_typeinfo_deps(PredProcId, VarTable, RttiVarMaps, Vars,
        !LocalVarUsageMap).

:- pred setup_typeinfo_dep(pred_proc_id::in, var_table::in, rtti_varmaps::in,
    prog_var::in, local_var_usage_map::in, local_var_usage_map::out) is det.

setup_typeinfo_dep(PredProcId, VarTable, RttiVarMaps, Var,
        !LocalVarUsageMap) :-
    lookup_var_type(VarTable, Var, Type),
    type_vars_in_type(Type, TVars),
    list.map(tvar_to_type_info_var(RttiVarMaps), TVars, TypeInfoVars),
    list.foldl(add_rev_arg_dep(Var, PredProcId), TypeInfoVars,
        !LocalVarUsageMap).

:- pred tvar_to_type_info_var(rtti_varmaps::in, tvar::in, prog_var::out)
    is det.

tvar_to_type_info_var(RttiVarMaps, TVar, TypeInfoVar) :-
    rtti_lookup_type_info_locn(RttiVarMaps, TVar, Locn),
    type_info_locn_var(Locn, TypeInfoVar).

%---------------------------------------------------------------------------%
%
% Traverse the procedure body, building up alias records as we go.
%

:- type unused_args_info
    --->    unused_args_info(
                unarg_module_info   :: module_info,
                unarg_var_table     :: var_table
            ).

:- pred unused_args_traverse_goal(unused_args_info::in, hlds_goal::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

unused_args_traverse_goal(Info, Goal, !LocalVarUsageMap) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = unify(LHS, RHS, _, Unify, _),
        unused_args_traverse_unify(Info, LHS, RHS, Unify, !LocalVarUsageMap)
    ;
        GoalExpr = plain_call(PredId, ProcId, CallArgVars, _, _, _),
        ModuleInfo = Info ^ unarg_module_info,
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
        proc_info_get_headvars(ProcInfo, CalleeHeadVars),
        CalleePredProcId = proc(PredId, ProcId),
        add_pred_call_arg_dep(CalleePredProcId, CallArgVars, CalleeHeadVars,
            !LocalVarUsageMap)
    ;
        GoalExpr = generic_call(GenericCall, CallArgVars, _, _, _),
        vars_in_generic_call(GenericCall, GenericCallArgVars),
        record_vars_as_used(GenericCallArgVars, !LocalVarUsageMap),
        record_vars_as_used(CallArgVars, !LocalVarUsageMap)
    ;
        GoalExpr = call_foreign_proc(_, _, _,
            ForeignArgs, ForeignExtraArgs, _, _),
        % Only arguments with names can be used in the foreign code.
        % The code in here should be kept in sync with the treatment
        % of foreign_procs in delete_unused_args_in_goal_expr:
        % any variable considered unused here should be renamed apart there.
        ArgIsUsed =
            ( pred(ForeignArg::in, Var::out) is semidet :-
                ForeignArg = foreign_arg(Var, MaybeNameAndMode, _, _),
                MaybeNameAndMode = yes(_)
            ),
        list.filter_map(ArgIsUsed, ForeignArgs ++ ForeignExtraArgs, UsedVars),
        record_vars_as_used(UsedVars, !LocalVarUsageMap)
    ;
        GoalExpr = conj(_ConjType, Goals),
        unused_args_traverse_goals(Info, Goals, !LocalVarUsageMap)
    ;
        GoalExpr = disj(Goals),
        unused_args_traverse_goals(Info, Goals, !LocalVarUsageMap)
    ;
        GoalExpr = switch(Var, _, Cases),
        record_var_as_used(Var, !LocalVarUsageMap),
        unused_args_traverse_cases(Info, Cases, !LocalVarUsageMap)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        unused_args_traverse_goal(Info, Cond, !LocalVarUsageMap),
        unused_args_traverse_goal(Info, Then, !LocalVarUsageMap),
        unused_args_traverse_goal(Info, Else, !LocalVarUsageMap)
    ;
        GoalExpr = negation(SubGoal),
        unused_args_traverse_goal(Info, SubGoal, !LocalVarUsageMap)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_TermVar, from_ground_term_construct)
        then
            % What we do here is what we would do for a construction
            % unification that binds TermVar to a constant, i.e. nothing.
            true
        else
            % XXX We could treat from_ground_term_deconstruct specially
            % as well.
            unused_args_traverse_goal(Info, SubGoal, !LocalVarUsageMap)
        )
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

:- pred unused_args_traverse_unify(unused_args_info::in,
    prog_var::in, unify_rhs::in, unification::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

unused_args_traverse_unify(Info, LHSVar, RHS, Unify, !LocalVarUsageMap) :-
    (
        Unify = simple_test(Var1, Var2),
        record_var_as_used(Var1, !LocalVarUsageMap),
        record_var_as_used(Var2, !LocalVarUsageMap)
    ;
        Unify = assign(Target, Source),
        ( if local_var_is_used(!.LocalVarUsageMap, Target) then
            % If Target is used to instantiate an output argument,
            % Source is used.
            record_var_as_used(Source, !LocalVarUsageMap)
        else
            add_aliases(Source, [Target], !LocalVarUsageMap)
        )
    ;
        Unify = construct(CellVar, _, ArgVars, _, _, _, _),
        expect(unify(CellVar, LHSVar), $pred, "LHSVar != CellVar"),
        ( if local_var_is_used(!.LocalVarUsageMap, CellVar) then
            record_vars_as_used(ArgVars, !LocalVarUsageMap)
        else
            add_construction_aliases(CellVar, ArgVars, !LocalVarUsageMap)
        )
    ;
        Unify = deconstruct(CellVar, _, ArgVars, ArgModes, CanFail, _),
        expect(unify(CellVar, LHSVar), $pred, "LHSVar != CellVar"),
        partition_deconstruct_args(Info, ArgVars, ArgModes,
            InputVars, OutputVars),
        % The deconstructed variable is used if any of the variables that
        % the deconstruction binds are used.
        add_aliases(CellVar, OutputVars, !LocalVarUsageMap),
        % Treat a deconstruction that further instantiates its left arg
        % as a partial construction.
        add_construction_aliases(CellVar, InputVars, !LocalVarUsageMap),
        (
            CanFail = can_fail,
            % A deconstruction that can_fail uses its left arg.
            record_var_as_used(CellVar, !LocalVarUsageMap)
        ;
            CanFail = cannot_fail
        )
    ;
        Unify = complicated_unify(_, _, _),
        % These should be transformed into calls by polymorphism.m.
        % This is here to cover the case where unused arguments is called
        % with --error-check-only and polymorphism has not been run.
        (
            RHS = rhs_var(RHSVar),
            record_var_as_used(RHSVar, !LocalVarUsageMap),
            record_var_as_used(LHSVar, !LocalVarUsageMap)
        ;
            ( RHS = rhs_functor(_, _, _)
            ; RHS = rhs_lambda_goal(_, _, _, _, _, _, _)
            ),
            unexpected($pred,
                "complicated unifications should only be var-var")
        )
    ).

    % Add PredProcId - CalleeArgVar as an alias for the corresponding
    % CallArgVar.
    %
:- pred add_pred_call_arg_dep(pred_proc_id::in,
    list(prog_var)::in, list(prog_var)::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

add_pred_call_arg_dep(PredProcId, CallArgVars, CalleeArgVars,
        !LocalVarUsageMap) :-
    (
        CallArgVars = [],
        CalleeArgVars = []
    ;
        CallArgVars = [],
        CalleeArgVars = [_ | _],
        unexpected($pred, "invalid call")
    ;
        CallArgVars = [_ | _],
        CalleeArgVars = [],
        unexpected($pred, "invalid call")
    ;
        CallArgVars = [HeadCallArgVar | TailCallArgVars],
        CalleeArgVars = [HeadCalleeArgVar | TailCalleeArgVars],
        add_arg_dep(HeadCallArgVar, PredProcId, HeadCalleeArgVar,
            !LocalVarUsageMap),
        add_pred_call_arg_dep(PredProcId, TailCallArgVars, TailCalleeArgVars,
            !LocalVarUsageMap)
    ).

    % Partition the arguments to a deconstruction into inputs
    % and outputs.
    %
:- pred partition_deconstruct_args(unused_args_info::in, list(prog_var)::in,
    list(unify_mode)::in, list(prog_var)::out, list(prog_var)::out) is det.

partition_deconstruct_args(Info, Vars, ArgModes, InputVars, OutputVars) :-
    (
        Vars = [],
        ArgModes = [],
        InputVars = [],
        OutputVars = []
    ;
        Vars = [],
        ArgModes = [_ | _],
        unexpected($pred, "mismatched lists")
    ;
        Vars = [_ | _],
        ArgModes = [],
        unexpected($pred, "mismatched lists")
    ;
        Vars = [HeadVar | TailVars],
        ArgModes = [HeadArgMode | TailArgModes],
        partition_deconstruct_args(Info, TailVars, TailArgModes,
            InputVarsTail, OutputVarsTail),

        HeadArgMode = unify_modes_li_lf_ri_rf(InitX, FinalX, InitY, FinalY),
        lookup_var_type(Info ^ unarg_var_table, HeadVar, HeadType),
        ModuleInfo = Info ^ unarg_module_info,

        % If the inst of the argument of the LHS is changed,
        % the argument is input.
        ( if inst_matches_binding(ModuleInfo, HeadType, InitX, FinalX) then
            InputVars = InputVarsTail
        else
            InputVars = [HeadVar | InputVarsTail]
        ),

        % If the inst of the argument of the RHS is changed,
        % the argument is output.
        ( if inst_matches_binding(ModuleInfo, HeadType, InitY, FinalY) then
            OutputVars = OutputVarsTail
        else
            OutputVars = [HeadVar | OutputVarsTail]
        )
    ).

:- pred unused_args_traverse_goals(unused_args_info::in, list(hlds_goal)::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

unused_args_traverse_goals(_, [], !LocalVarUsageMap).
unused_args_traverse_goals(Info, [Goal | Goals], !LocalVarUsageMap) :-
    unused_args_traverse_goal(Info, Goal, !LocalVarUsageMap),
    unused_args_traverse_goals(Info, Goals, !LocalVarUsageMap).

:- pred unused_args_traverse_cases(unused_args_info::in, list(case)::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

unused_args_traverse_cases(_, [], !LocalVarUsageMap).
unused_args_traverse_cases(Info, [Case | Cases], !LocalVarUsageMap) :-
    Case = case(_, _, Goal),
    unused_args_traverse_goal(Info, Goal, !LocalVarUsageMap),
    unused_args_traverse_cases(Info, Cases, !LocalVarUsageMap).

%---------------------------------------------------------------------------%
%
% Analysis section - do the fixpoint iteration.
%

    % Do a full iteration, check if anything changed, if so, repeat.
    %
:- pred unused_args_iterate_to_fixpoint(int::in, module_info::in,
    list(pred_proc_id)::in,
    global_var_usage_map::in, global_var_usage_map::out) is det.

unused_args_iterate_to_fixpoint(PassNum, ModuleInfo,
        LocalPredProcIds, !GlobalVarUsageMap) :-
    unused_args_single_pass(LocalPredProcIds, unchanged, Changed,
        !GlobalVarUsageMap),
    (
        Changed = changed,
        trace [compile_time(flag("unused_args_var_usage")), io(!IO)] (
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            io.format(DebugStream,
                "\nVARIABLE USAGE MAP AFTER PASS %d\n", [i(PassNum)], !IO),
            write_global_var_usage_map(DebugStream, ModuleInfo,
                !.GlobalVarUsageMap, !IO)
        ),
        unused_args_iterate_to_fixpoint(PassNum + 1, ModuleInfo,
            LocalPredProcIds, !GlobalVarUsageMap)
    ;
        Changed = unchanged
    ).

    % Check over all the procedures in a module.
    %
:- pred unused_args_single_pass(list(pred_proc_id)::in,
    maybe_changed::in, maybe_changed::out,
    global_var_usage_map::in, global_var_usage_map::out) is det.

unused_args_single_pass([], !Changed, !GlobalVarUsageMap).
unused_args_single_pass([PredProcId | PredProcIds],
        !Changed, !GlobalVarUsageMap) :-
    unused_args_check_proc(PredProcId, !Changed, !GlobalVarUsageMap),
    unused_args_single_pass(PredProcIds, !Changed, !GlobalVarUsageMap).

    % Check a single procedure.
    %
:- pred unused_args_check_proc(pred_proc_id::in,
    maybe_changed::in, maybe_changed::out,
    global_var_usage_map::in, global_var_usage_map::out) is det.

unused_args_check_proc(PredProcId, !Changed, !GlobalVarUsageMap) :-
    map.lookup(!.GlobalVarUsageMap, PredProcId, LocalVarUsageMap0),
    % NOTE: It would be nice to use map.map_foldl here, but that works
    % when the processing of each key-value pair involves *updating*
    % the value, whereas the job of unused_args_check_all_vars is to
    % *delete* whole pairs.
    %
    % Technically, we *could* fold over the initial version of
    % LocalVarUsageMap0 while producing the updated LocalVarUsageMap,
    % but this would be harder to maintain, due to the abstraction barrier
    % involved in calls to record_var_as_used.
    map.keys(LocalVarUsageMap0, Vars),
    unused_args_check_all_vars(!.GlobalVarUsageMap, Vars,
        unchanged, LocalChanged, LocalVarUsageMap0, LocalVarUsageMap),
    (
        LocalChanged = changed,
        map.det_update(PredProcId, LocalVarUsageMap, !GlobalVarUsageMap),
        !:Changed = changed
    ;
        LocalChanged = unchanged
    ).

    % Check each var of a procedure in turn.
    %
:- pred unused_args_check_all_vars(global_var_usage_map::in,
    list(prog_var)::in, maybe_changed::in, maybe_changed::out,
    local_var_usage_map::in, local_var_usage_map::out) is det.

unused_args_check_all_vars(_, [], !Changed, !LocalVarUsageMap).
unused_args_check_all_vars(GlobalVarUsageMap, [Var | Vars],
        !Changed, !LocalVarUsageMap) :-
    map.lookup(!.LocalVarUsageMap, Var, Usage0),
    Usage0 = unused(AliasVars0, AliasArgs0),
    ( if
        (
            % Are there any used procedure arguments that Var depends on?
            some [Argument] (
                set.member(Argument, AliasArgs0),
                Argument = arg_var_in_proc(PredProcId, ArgVar),
                proc_arg_var_is_used(GlobalVarUsageMap, PredProcId, ArgVar)
            )
        ;
            % Are there any used local variables that Var depends on?
            some [X] (
                set.member(X, AliasVars0),
                local_var_is_used(!.LocalVarUsageMap, X)
            )
        )
    then
        % Mark the current variable as used. Note that we update the same
        % data structure (!LocalVarUsageMap) as we test in the condition
        % above. This is OK because the order in which we mark variables
        % in !.LocalVarUsageMap as used does not matter; the iteration
        % performed by unused_args_iterate_to_fixpoint is guaranteed
        % to reach the same final result.
        record_var_as_used(Var, !LocalVarUsageMap),
        !:Changed = changed
    else
        true
    ),
    unused_args_check_all_vars(GlobalVarUsageMap, Vars,
        !Changed, !LocalVarUsageMap).

%---------------------------------------------------------------------------%

:- pred get_unused_arg_info(module_info::in, global_var_usage_map::in,
    list(pred_proc_id)::in, unused_arg_info::in, unused_arg_info::out) is det.

get_unused_arg_info(_, _, [], !UnusedArgInfo).
get_unused_arg_info(ModuleInfo, GlobalVarUsageMap, [PredProcId | PredProcIds],
        !UnusedArgInfo) :-
    PredProcId = proc(PredId, ProcId),
    map.lookup(GlobalVarUsageMap, PredProcId, LocalVarUsageMap),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    get_unused_arg_nums(LocalVarUsageMap, HeadVars, 1, UnusedArgs),
    map.det_insert(PredProcId, UnusedArgs, !UnusedArgInfo),
    get_unused_arg_info(ModuleInfo, GlobalVarUsageMap, PredProcIds,
        !UnusedArgInfo).

:- pred get_unused_arg_nums(local_var_usage_map::in, list(prog_var)::in,
    int::in, list(int)::out) is det.

get_unused_arg_nums(_, [], _, []).
get_unused_arg_nums(LocalVarUsageMap, [HeadVar | HeadVars], ArgNum,
        UnusedArgs) :-
    get_unused_arg_nums(LocalVarUsageMap, HeadVars, ArgNum + 1,
        UnusedArgsTail),
    ( if map.contains(LocalVarUsageMap, HeadVar) then
        UnusedArgs = [ArgNum | UnusedArgsTail]
    else
        UnusedArgs = UnusedArgsTail
    ).

%---------------------------------------------------------------------------%
%
% Fix up the module.
%

    % Information about predicates which have new predicates created for the
    % optimized version.
    %
:- type new_proc_map == map(pred_proc_id, new_proc_info).

    % New pred_id, proc_id, name, and the indices in the argument
    % vector of the arguments that have been removed.
:- type new_proc_info
    --->    new_proc_info(pred_id, proc_id, sym_name, list(int)).

    % Create a new predicate for each procedure which has unused arguments.
    % There are two reasons why we can't throw away the old procedure for
    % non-exported predicates. One is higher-order terms - we can't remove
    % arguments from them without changing their type, so they need the old
    % calling interface. The other is that the next proc_id for a predicate is
    % chosen based on the length of the list of proc_ids.
    %
:- pred unused_args_create_new_pred(unused_arg_info::in, pred_proc_id::in,
    new_proc_map::in, new_proc_map::out,
    module_info::in, module_info::out) is det.

unused_args_create_new_pred(UnusedArgInfo, OrigPredProcId,
        !NewProcMap, !ModuleInfo) :-
    map.lookup(UnusedArgInfo, OrigPredProcId, UnusedArgs),
    module_info_pred_proc_info(!.ModuleInfo, OrigPredProcId,
        OrigPredInfo, OrigProcInfo),
    PredModuleName = pred_info_module(OrigPredInfo),

    OrigPredProcId = proc(OrigPredId, ProcId),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, intermodule_analysis, Intermod),
    (
        Intermod = yes,
        module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),

        pred_info_proc_id_to_module_name_func_id(OrigPredInfo, ProcId,
            ModuleId, FuncId),
        analysis.operations.lookup_results(AnalysisInfo0, ModuleId, FuncId,
            IntermodResultsTriples : list(analysis_result(unused_args_call,
                unused_args_answer))),
        IntermodOldAnswers = list.map((func(R) = R ^ ar_answer),
            IntermodResultsTriples),

        pred_info_get_orig_arity(OrigPredInfo, PredFormArity),
        FuncInfo = unused_args_func_info(PredFormArity),
        Answer = unused_args_answer(UnusedArgs),

        FilterUnused =
            ( pred(VersionAnswer::in) is semidet :-
                VersionAnswer \= Answer,
                VersionAnswer \= unused_args_answer([]),
                more_precise_than(FuncInfo, Answer, VersionAnswer)
            ),
        IntermodOldArgLists = list.map(get_unused_args,
            list.filter(FilterUnused, IntermodOldAnswers))
    ;
        Intermod = no,
        IntermodResultsTriples = [],
        IntermodOldArgLists = []
    ),

    (
        UnusedArgs = []
    ;
        UnusedArgs = [_ | _],
        pred_info_get_status(OrigPredInfo, PredStatus0),
        ( if
            PredStatus0 = pred_status(status_opt_imported),
            IntermodResultsTriples = [_ | _],
            IntermodOldArgLists = []
        then
            % If this predicate is from a .opt file, and no more arguments
            % have been removed than in the original module, then leave the
            % import status as opt_imported so that dead_proc_elim will remove
            % it if no other optimization is performed on it.
            PredStatus = pred_status(status_opt_imported)
        else if
            pred_status_is_exported(PredStatus0) = yes
        then
            % This specialized version of the predicate will be declared
            % in the analysis file for this module so it must be exported.
            PredStatus = PredStatus0
        else
            PredStatus = pred_status(status_local)
        ),
        make_new_pred_info(!.ModuleInfo, UnusedArgs, PredStatus,
            OrigPredProcId, OrigPredInfo, NewPredInfo0),
        NewPredName = pred_info_name(NewPredInfo0),
        pred_info_get_proc_table(NewPredInfo0, NewProcs0),

        % Assign the old procedure to a new predicate, which will be fixed up
        % in delete_unused_args_in_module.
        % XXX Fixed up in what sense? And where within the call tree
        % of delete_unused_args_in_module?
        map.set(ProcId, OrigProcInfo, NewProcs0, NewProcs),
        pred_info_set_proc_table(NewProcs, NewPredInfo0, NewPredInfo),

        % Add the new proc to the pred table.
        module_info_get_predicate_table(!.ModuleInfo, PredTable0),
        predicate_table_insert(NewPredInfo, NewPredId, PredTable0, PredTable),
        module_info_set_predicate_table(PredTable, !ModuleInfo),

        % Add the new proc to the new_proc_map.
        PredSymName = qualified(PredModuleName, NewPredName),
        OrigToNew = new_proc_info(NewPredId, ProcId, PredSymName, UnusedArgs),
        map.det_insert(OrigPredProcId, OrigToNew, !NewProcMap),

        % Add a forwarding predicate with the original interface.
        create_call_goal(UnusedArgs, NewPredId, ProcId,
            PredModuleName, NewPredName, OrigProcInfo, ForwardingProcInfo),
        module_info_set_pred_proc_info(OrigPredId, ProcId, OrigPredInfo,
            ForwardingProcInfo, !ModuleInfo),

        % Add forwarding predicates for results produced in previous
        % compilations.
        % XXX This only works "once" due to the analysis framework now
        % discarding all but the best answer. If we compile this module again
        % without changing anything else, we won't remember to produce
        % the same forwarding predicates. If some callers refer to those
        % forwarding predicates, then linking will fail.
        list.foldl(
            make_intermod_proc(OrigPredId, NewPredId, ProcId, NewPredName,
                OrigPredInfo, OrigProcInfo, UnusedArgs),
            IntermodOldArgLists, !ModuleInfo)
    ).

:- pred make_intermod_proc(pred_id::in, pred_id::in, proc_id::in, string::in,
    pred_info::in, proc_info::in, list(int)::in, list(int)::in,
    module_info::in, module_info::out) is det.

make_intermod_proc(PredId, NewPredId, ProcId, NewPredName,
        OrigPredInfo, OrigProcInfo, UnusedArgs, UnusedArgs2, !ModuleInfo) :-
    % Add an exported predicate with the number of removed arguments promised
    % in the analysis file, which just calls the new predicate.
    make_new_pred_info(!.ModuleInfo, UnusedArgs2, pred_status(status_exported),
        proc(PredId, ProcId), OrigPredInfo, ExtraPredInfo0),
    PredModuleName = pred_info_module(OrigPredInfo),
    create_call_goal(UnusedArgs, NewPredId, ProcId,
        PredModuleName, NewPredName, OrigProcInfo, ExtraProc0),

    proc_info_get_headvars(OrigProcInfo, HeadVars0),
    proc_info_get_argmodes(OrigProcInfo, ArgModes0),
    remove_specified_positions(UnusedArgs2, HeadVars0, IntermodHeadVars),
    remove_specified_positions(UnusedArgs2, ArgModes0, IntermodArgModes),
    proc_info_set_headvars(IntermodHeadVars, ExtraProc0, ExtraProc1),
    proc_info_set_argmodes(IntermodArgModes, ExtraProc1, ExtraProc),

    pred_info_get_proc_table(ExtraPredInfo0, ExtraProcs0),
    map.set(ProcId, ExtraProc, ExtraProcs0, ExtraProcs),
    pred_info_set_proc_table(ExtraProcs, ExtraPredInfo0, ExtraPredInfo),

    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(ExtraPredInfo, _, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo).

:- pred make_new_pred_info(module_info::in, list(int)::in, pred_status::in,
    pred_proc_id::in, pred_info::in, pred_info::out) is det.

make_new_pred_info(_ModuleInfo, UnusedArgs, PredStatus, proc(PredId, ProcId),
        !PredInfo) :-
    PredModuleName = pred_info_module(!.PredInfo),
    Name0 = pred_info_name(!.PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(!.PredInfo),
    pred_info_get_arg_types(!.PredInfo, Tvars, ExistQVars, ArgTypes0),
    pred_info_get_origin(!.PredInfo, OrigOrigin),
    % Create a unique new pred name using the old proc_id.
    ( if
        string.prefix(Name0, "__"),
        % XXX The string __LambdaGoal__ is not being generated by lambda.m.
        % It *is* generated by modecheck_unify.m, but lambda.m is supposed
        % to override it.
        not string.prefix(Name0, "__LambdaGoal__")
    then
        ( if
            % Fix up special pred names.
            OrigOrigin = origin_compiler(made_for_uci(_SpecialId, TypeCtor))
        then
            type_ctor_module_name_arity(TypeCtor, TypeModule, TypeName,
                TypeArity),
            TypeModuleStr = sym_name_to_string_sep(TypeModule, "__"),
            string.format("%s_%s__%s_%d",
                [s(Name0), s(TypeModuleStr), s(TypeName), i(TypeArity)], Name1)
        else
            % The special predicate has already been specialised.
            Name1 = Name0
        )
    else
        Name1 = Name0
    ),
    % The mode number is included because we want to avoid the creation of
    % more than one predicate with the same name if more than one mode of
    % a predicate is specialized. Since the names of e.g. deep profiling
    % proc_static structures are derived from the names of predicates,
    % duplicate predicate names lead to duplicate global variable names
    % and hence to link errors.
    Transform = tn_unused_args(PredOrFunc, proc_id_to_int(ProcId), UnusedArgs),
    make_transformed_pred_name(Name1, Transform, TransformedName),
    PredFormArity = pred_info_pred_form_arity(!.PredInfo),
    pred_info_get_typevarset(!.PredInfo, TypeVars),
    remove_specified_positions(UnusedArgs, ArgTypes0, ArgTypes),
    pred_info_get_context(!.PredInfo, Context),
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo),
    pred_info_get_markers(!.PredInfo, Markers),
    pred_info_get_goal_type(!.PredInfo, GoalType),
    pred_info_get_class_context(!.PredInfo, ClassContext),
    pred_info_get_var_name_remap(!.PredInfo, VarNameRemap),

    % Since this pred_info isn't built until after the polymorphism
    % transformation is complete, we just use dummy maps for the class
    % constraints.
    map.init(Proofs),
    map.init(ConstraintMap),
    ProcTransform = proc_transform_unused_args(UnusedArgs),
    Origin = origin_proc_transform(ProcTransform, OrigOrigin, PredId, ProcId),
    CurUserDecl = maybe.no,
    pred_info_init(PredOrFunc, PredModuleName, TransformedName, PredFormArity,
        Context, Origin, PredStatus, CurUserDecl, GoalType, Markers, ArgTypes,
        Tvars, ExistQVars, ClassContext, Proofs, ConstraintMap,
        ClausesInfo, VarNameRemap, !:PredInfo),
    pred_info_set_typevarset(TypeVars, !PredInfo).

    % Replace the goal in the procedure with one to call the given
    % pred_id and proc_id.
    %
:- pred create_call_goal(list(int)::in,
    pred_id::in, proc_id::in, module_name::in, string::in,
    proc_info::in, proc_info::out) is det.

create_call_goal(UnusedArgs, NewPredId, NewProcId,
        PredModuleName, PredName, !OldProc) :-
    proc_info_get_headvars(!.OldProc, HeadVars),
    proc_info_get_goal(!.OldProc, Goal0),
    Goal0 = hlds_goal(_GoalExpr, GoalInfo0),

    % We must use the interface determinism for determining the determinism
    % of the version of the goal with its arguments removed, not the actual
    % determinism of the body is it may be more lax, which will lead to code
    % generation problems.
    proc_info_interface_determinism(!.OldProc, Determinism),
    goal_info_set_determinism(Determinism, GoalInfo0, GoalInfo1),

    proc_info_get_var_table(!.OldProc, VarTable0),
    set.list_to_set(HeadVars, NonLocals),
    lookup_var_entries(VarTable0, HeadVars, HeadVarEntries),
    var_table_from_corresponding_lists(HeadVars, HeadVarEntries, VarTable1),
    % The varset should probably be fixed up, but it shouldn't make
    % too much difference.
    proc_info_get_rtti_varmaps(!.OldProc, RttiVarMaps0),
    remove_specified_positions(UnusedArgs, HeadVars, NewHeadVars),
    GoalExpr = plain_call(NewPredId, NewProcId, NewHeadVars,
        not_builtin, no, qualified(PredModuleName, PredName)),
    Goal1 = hlds_goal(GoalExpr, GoalInfo1),
    implicitly_quantify_goal_general(ord_nl_no_lambda,
        set_to_bitset(NonLocals), _, Goal1, Goal,
        VarTable1, VarTable, RttiVarMaps0, RttiVarMaps),
    proc_info_set_goal(Goal, !OldProc),
    proc_info_set_var_table(VarTable, !OldProc),
    proc_info_set_rtti_varmaps(RttiVarMaps, !OldProc).

    % Create a pred_info for an imported pred with a pragma unused_args
    % in the .opt file.
    %
:- pred make_imported_unused_args_pred_info(pred_proc_id::in, list(int)::in,
    new_proc_map::in, new_proc_map::out,
    module_info::in, module_info::out) is det.

make_imported_unused_args_pred_info(OptProc, UnusedArgs, !NewProcMap,
        !ModuleInfo) :-
    OptProc = proc(PredId, ProcId),
    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        PredInfo0, ProcInfo0),
    make_new_pred_info(!.ModuleInfo, UnusedArgs,
        pred_status(status_imported(import_locn_interface)), OptProc,
        PredInfo0, NewPredInfo0),
    pred_info_get_proc_table(NewPredInfo0, NewProcs0),

    % Assign the old procedure to a new predicate.
    proc_info_get_headvars(ProcInfo0, HeadVars0),
    remove_specified_positions(UnusedArgs, HeadVars0, HeadVars),
    proc_info_set_headvars(HeadVars, ProcInfo0, ProcInfo1),
    proc_info_get_argmodes(ProcInfo1, ArgModes0),
    remove_specified_positions(UnusedArgs, ArgModes0, ArgModes),
    proc_info_set_argmodes(ArgModes, ProcInfo1, ProcInfo),
    map.set(ProcId, ProcInfo, NewProcs0, NewProcs),
    pred_info_set_proc_table(NewProcs, NewPredInfo0, NewPredInfo),

    % Add the new proc to the pred table.
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(NewPredInfo, NewPredId, PredTable0, PredTable1),
    module_info_set_predicate_table(PredTable1, !ModuleInfo),
    PredModuleName = pred_info_module(NewPredInfo),
    PredName = pred_info_name(NewPredInfo),
    PredSymName = qualified(PredModuleName, PredName),
    % Add the new proc to the new_proc_map.
    NewProcInfo = new_proc_info(NewPredId, ProcId, PredSymName, UnusedArgs),
    map.det_insert(OptProc, NewProcInfo, !NewProcMap).

%---------------------------------------------------------------------------%

:- pred delete_unused_args_in_module(bool::in, global_var_usage_map::in,
    list(pred_proc_id)::in, new_proc_map::in,
    module_info::in, module_info::out) is det.

delete_unused_args_in_module(VeryVerbose, GlobalVarUsageMap, PredProcIds,
        NewProcMap, !ModuleInfo) :-
    list.foldl(
        delete_unused_args_in_proc_msg(VeryVerbose, GlobalVarUsageMap,
            NewProcMap),
        PredProcIds, !ModuleInfo).

:- pred delete_unused_args_in_proc_msg(bool::in, global_var_usage_map::in,
    new_proc_map::in, pred_proc_id::in,
    module_info::in, module_info::out) is det.

delete_unused_args_in_proc_msg(VeryVerbose, GlobalVarUsageMap, NewProcMap,
        PredProcId, !ModuleInfo) :-
    (
        VeryVerbose = yes,
        trace [io(!IO)] (
            get_debug_output_stream(!.ModuleInfo, DebugStream, !IO),
            PredProcId = proc(PredId, ProcId),
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
            pred_info_get_name(PredInfo, Name),
            pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
            pred_info_get_orig_arity(PredInfo, PredFormArity),
            user_arity_pred_form_arity(PredOrFunc,
                user_arity(UserArityInt), PredFormArity),
            proc_id_to_int(ProcId, ProcInt),
            io.format(DebugStream, "%% Fixing up %s `%s/%d in mode %d\n",
                [s(pred_or_func_to_str(PredOrFunc)), s(Name),
                i(UserArityInt), i(ProcInt)], !IO)
        )
    ;
        VeryVerbose = no
    ),
    delete_unused_args_in_proc(GlobalVarUsageMap, PredProcId, NewProcMap,
        !ModuleInfo).

:- pred delete_unused_args_in_proc(global_var_usage_map::in, pred_proc_id::in,
    new_proc_map::in, module_info::in, module_info::out) is det.

delete_unused_args_in_proc(GlobalVarUsageMap, OldPredProcId, NewProcMap,
        !ModuleInfo) :-
    % Work out which proc we should be fixing up.
    ( if map.search(NewProcMap, OldPredProcId, NewProcInfo) then
        NewProcInfo = new_proc_info(PredId, ProcId, _, UnusedArgs)
    else
        OldPredProcId = proc(PredId, ProcId),
        UnusedArgs = []
    ),
    map.lookup(GlobalVarUsageMap, OldPredProcId, OldProcLocalVarUsageMap),
    map.keys(OldProcLocalVarUsageMap, UnusedVars),
    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        PredInfo0, ProcInfo0),
    proc_info_get_var_table(ProcInfo0, VarTable0),
    proc_info_get_headvars(ProcInfo0, HeadVars0),
    proc_info_get_argmodes(ProcInfo0, ArgModes0),
    proc_info_get_goal(ProcInfo0, Goal0),
    remove_specified_positions(UnusedArgs, HeadVars0, HeadVars),
    remove_specified_positions(UnusedArgs, ArgModes0, ArgModes),

    some [!ProcInfo, !Goal] (
        !:ProcInfo = ProcInfo0,
        !:Goal = Goal0,

        proc_info_set_headvars(HeadVars, !ProcInfo),
        proc_info_set_argmodes(ArgModes, !ProcInfo),

        % Remove unused vars from goal.
        % NOTE We should probably remove unused variables from the type map.
        DeleteInfo0 =
            delete_info(!.ModuleInfo, NewProcMap, UnusedVars, VarTable0),
        delete_unused_args_in_goal(!Goal, DeleteInfo0, DeleteInfo, Changed),
        DeleteInfo = delete_info(_, _, _, VarTable1),
        (
            Changed = changed,
            % If anything has changed, rerun quantification.
            NonLocals = set_of_var.list_to_set(HeadVars),
            proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
            implicitly_quantify_goal_general(ord_nl_no_lambda, NonLocals, _,
                !Goal, VarTable1, VarTable, RttiVarMaps0, RttiVarMaps),
            proc_info_set_goal(!.Goal, !ProcInfo),
            proc_info_set_var_table(VarTable, !ProcInfo),
            proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo)
        ;
            Changed = unchanged
        ),
        ProcInfo = !.ProcInfo
    ),
    pred_info_set_proc_info(ProcId, ProcInfo, PredInfo0, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- type delete_info
    --->    delete_info(
                delete_module_info      :: module_info,
                delete_new_proc_map     :: new_proc_map,
                delete_unused_vars      :: list(prog_var),
                delete_var_table        :: var_table
            ).

    % This is the important bit of the transformation.
    %
:- pred delete_unused_args_in_goal(hlds_goal::in, hlds_goal::out,
    delete_info::in, delete_info::out, maybe_changed::out) is det.

delete_unused_args_in_goal(Goal0, Goal, !Info, Changed) :-
    delete_unused_args_in_goal_expr(Goal0, Goal1, !Info, Changed),
    Goal1 = hlds_goal(GoalExpr1, GoalInfo1),
    (
        Changed = changed,
        UnusedVars = !.Info ^ delete_unused_vars,
        delete_unused_args_in_goal_info(UnusedVars, GoalInfo1, GoalInfo),
        Goal = hlds_goal(GoalExpr1, GoalInfo)
    ;
        Changed = unchanged,
        Goal = Goal0
    ).

:- pred delete_unused_args_in_goal_expr(hlds_goal::in, hlds_goal::out,
    delete_info::in, delete_info::out, maybe_changed::out) is det.

delete_unused_args_in_goal_expr(Goal0, Goal, !Info, Changed) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = unify(_Var, _RHS, _Mode, Unify, _Context),
        ModuleInfo = !.Info ^ delete_module_info,
        UnusedVars = !.Info ^ delete_unused_vars,
        ( if need_unify(ModuleInfo, UnusedVars, Unify, ChangedPrime) then
            Goal = Goal0,
            Changed = ChangedPrime
        else
            Goal = hlds_goal(true_goal_expr, GoalInfo0),
            Changed = changed
        )
    ;
        GoalExpr0 = plain_call(PredId, ProcId, ArgVars0, Builtin,
            UnifyContext, _SymName),
        NewProcMap = !.Info ^ delete_new_proc_map,
        ( if map.search(NewProcMap, proc(PredId, ProcId), NewProcInfo) then
            NewProcInfo = new_proc_info(NewPredId, NewProcId, NewSymName,
                UnusedArgs),
            Changed = changed,
            remove_specified_positions(UnusedArgs, ArgVars0, ArgVars),
            GoalExpr = plain_call(NewPredId, NewProcId, ArgVars, Builtin,
                UnifyContext, NewSymName),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        else
            Changed = unchanged,
            Goal = Goal0
        )
    ;
        GoalExpr0 = generic_call(_, _, _, _, _),
        Goal = Goal0,
        Changed = unchanged
    ;
        GoalExpr0 = call_foreign_proc(Attributes, PredId, ProcId,
            Args0, ExtraArgs0, MaybeTraceRuntimeCond, Impl),
        % The code in here should be kept in sync with the treatment of
        % foreign_procs in traverse_goal.
        Changed0 = unchanged,
        map.init(Subst0),
        list.map_foldl3(rename_apart_unused_foreign_arg,
            Args0, Args, Subst0, Subst1, !Info, Changed0, ArgsChanged),
        list.map_foldl3(rename_apart_unused_foreign_arg,
            ExtraArgs0, ExtraArgs, Subst1, Subst, !Info, ArgsChanged, Changed),
        GoalExpr = call_foreign_proc(Attributes, PredId, ProcId,
            Args, ExtraArgs, MaybeTraceRuntimeCond, Impl),
        rename_vars_in_goal_info(need_not_rename, Subst, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        delete_unused_args_in_conjuncts(Goals0, Goals, !Info,
            unchanged, Changed),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Goals0),
        delete_unused_args_in_disjuncts(Goals0, Goals, !Info,
            unchanged, Changed),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        delete_unused_args_in_cases(Cases0, Cases, !Info, unchanged, Changed),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(NegGoal0),
        delete_unused_args_in_goal(NegGoal0, NegGoal, !Info, Changed),
        GoalExpr = negation(NegGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        delete_unused_args_in_goal(Cond0, Cond, !Info, Changed1),
        delete_unused_args_in_goal(Then0, Then, !Info, Changed2),
        delete_unused_args_in_goal(Else0, Else, !Info, Changed3),
        Changed = maybe_util.or_list([Changed1, Changed2, Changed3]),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(TermVar, from_ground_term_construct)
        then
            UnusedVars = !.Info ^ delete_unused_vars,
            ( if list.member(TermVar, UnusedVars) then
                Goal = true_goal,
                % We don't change the set of unneeded variables.
                Changed = unchanged
            else
                Goal = Goal0,
                Changed = unchanged
            )
        else
            delete_unused_args_in_goal(SubGoal0, SubGoal, !Info, Changed),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

:- pred rename_apart_unused_foreign_arg(foreign_arg::in, foreign_arg::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out,
    delete_info::in, delete_info::out,
    maybe_changed::in, maybe_changed::out) is det.

rename_apart_unused_foreign_arg(Arg0, Arg, !Subst, !Info, !Changed) :-
    Arg0 = foreign_arg(OldVar, MaybeName, OrigType, BoxPolicy),
    (
        MaybeName = yes(_),
        Arg = Arg0
    ;
        MaybeName = no,
        VarTable0 = !.Info ^ delete_var_table,
        lookup_var_entry(VarTable0, OldVar, OldVarEntry),
        add_var_entry(OldVarEntry, NewVar, VarTable0, VarTable),
        !Info ^ delete_var_table := VarTable,

        % It is possible for an unnamed input argument to occur more than once
        % in the list of foreign_args.
        map.set(OldVar, NewVar, !Subst),
        Arg = foreign_arg(NewVar, MaybeName, OrigType, BoxPolicy),
        !:Changed = changed
    ).

    % Remove unused args in each conjunct, and delete the conjuncts
    % from which nothing is left.
    %
:- pred delete_unused_args_in_conjuncts(
    list(hlds_goal)::in, list(hlds_goal)::out,
    delete_info::in, delete_info::out,
    maybe_changed::in, maybe_changed::out) is det.

delete_unused_args_in_conjuncts([], [], !Info, !Changed).
delete_unused_args_in_conjuncts([Goal0 | Goals0], Goals, !Info, !Changed) :-
    delete_unused_args_in_goal(Goal0, Goal, !Info, LocalChanged),
    (
        LocalChanged = changed,
        !:Changed = changed
    ;
        LocalChanged = unchanged
    ),
    % Replacing a goal with true signals that it is no longer needed.
    ( if Goal = hlds_goal(true_goal_expr, _) then
        Goals = Goals1
    else
        Goals = [Goal | Goals1]
    ),
    delete_unused_args_in_conjuncts(Goals0, Goals1, !Info, !Changed).

    % We can't remove unused goals from the list of disjuncts as we do
    % for conjuncts, since that would change the determinism of the goal.
    %
:- pred delete_unused_args_in_disjuncts(
    list(hlds_goal)::in, list(hlds_goal)::out,
    delete_info::in, delete_info::out,
    maybe_changed::in, maybe_changed::out) is det.

delete_unused_args_in_disjuncts([], [], !Info, !Changed).
delete_unused_args_in_disjuncts([Goal0 | Goals0], [Goal | Goals],
        !Info, !Changed) :-
    delete_unused_args_in_goal(Goal0, Goal, !Info, LocalChanged),
    (
        LocalChanged = changed,
        !:Changed = changed
    ;
        LocalChanged = unchanged
    ),
    delete_unused_args_in_disjuncts(Goals0, Goals, !Info, !Changed).

:- pred delete_unused_args_in_cases(list(case)::in, list(case)::out,
    delete_info::in, delete_info::out,
    maybe_changed::in, maybe_changed::out) is det.

delete_unused_args_in_cases([], [], !Info, !Changed).
delete_unused_args_in_cases([Case0 | Cases0], [Case | Cases],
        !Info, !Changed) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    delete_unused_args_in_goal(Goal0, Goal, !Info, LocalChanged),
    Case = case(MainConsId, OtherConsIds, Goal),
    (
        LocalChanged = changed,
        !:Changed = changed
    ;
        LocalChanged = unchanged
    ),
    delete_unused_args_in_cases(Cases0, Cases, !Info, !Changed).

    % Fail if the unification is no longer needed.
    %
:- pred need_unify(module_info::in, list(prog_var)::in, unification::in,
    maybe_changed::out) is semidet.

need_unify(ModuleInfo, UnusedVars, Unify, Changed) :-
    (
        Unify = simple_test(_, _),
        % A simple test doesn't have any unused vars to fixup.
        Changed = unchanged
    ;
        % Target unused => we don't need the assignment
        % Source unused => Target unused
        Unify = assign(Target, _Source),
        not list.member(Target, UnusedVars),
        Changed = unchanged
    ;
        % LVar unused => we don't need the unification
        Unify = construct(LVar, _, _, _, _, _, _),
        not list.member(LVar, UnusedVars),
        Changed = unchanged
    ;
        Unify = deconstruct(LVar, _, ArgVars, ArgModes, CanFail, _CanCGC),
        not list.member(LVar, UnusedVars),
        (
            % Are any of the args unused?
            % If so, we need to fix up the goal_info.
            CanFail = cannot_fail,
            check_deconstruct_args(ModuleInfo, UnusedVars, ArgVars, ArgModes,
                no, Changed)
        ;
            CanFail = can_fail,
            Changed = unchanged
        )
    ;
        % These should have been transformed into calls by polymorphism.m.
        Unify = complicated_unify(_, _, _),
        unexpected($pred, "complicated unify")
    ).

    % Check if any of the arguments of a deconstruction are unused,
    % if so Changed will be yes and quantification will be rerun. Fails if
    % none of the arguments are used. Arguments which further instantiate
    % the deconstructed variable are ignored in this.
    %
:- pred check_deconstruct_args(module_info::in, list(prog_var)::in,
    list(prog_var)::in, list(unify_mode)::in, bool::in,
    maybe_changed::out) is semidet.

check_deconstruct_args(ModuleInfo, UnusedVars, Vars, ArgModes, !.SomeUsed,
        Changed) :-
    (
        Vars = [],
        ArgModes = [],
        !.SomeUsed = yes,
        Changed = unchanged
    ;
        Vars = [],
        ArgModes = [_ | _],
        unexpected($pred, "mismatched lists")
    ;
        Vars = [_ | _],
        ArgModes = [],
        unexpected($pred, "mismatched lists")
    ;
        Vars = [HeadVar | TailVars],
        ArgModes = [HeadArgMode | TailArgModes],
        ( if
            % XXX This test seems wrong to me. Why does it look at a mode
            % that is made up of *two initial* insts?
            HeadArgMode = unify_modes_li_lf_ri_rf(InitX, _, InitY, _),
            mode_is_output(ModuleInfo, from_to_mode(InitX, InitY)),
            list.member(HeadVar, UnusedVars)
        then
            check_deconstruct_args(ModuleInfo, UnusedVars,
                TailVars, TailArgModes, !.SomeUsed, _),
            Changed = changed
        else
            !:SomeUsed = yes,
            check_deconstruct_args(ModuleInfo, UnusedVars,
                TailVars, TailArgModes, !.SomeUsed, Changed)
        )
    ).

    % Remove unused vars from the instmap_delta, quantification fixes up
    % the rest.
    %
:- pred delete_unused_args_in_goal_info(list(prog_var)::in, hlds_goal_info::in,
    hlds_goal_info::out) is det.

delete_unused_args_in_goal_info(UnusedVars, !GoalInfo) :-
    InstMap0 = goal_info_get_instmap_delta(!.GoalInfo),
    instmap_delta_delete_vars(UnusedVars, InstMap0, InstMap),
    goal_info_set_instmap_delta(InstMap, !GoalInfo).

%---------------------------------------------------------------------------%

:- type maybe_warn_unused_args
    --->    do_not_warn_unused_args
    ;       do_warn_unused_args.

    % Except for type_infos, all args that are unused in one mode of a
    % predicate should be unused in all of the modes of a predicate, so we
    % only need to put out one warning for each predicate.
    %
:- pred gather_warnings_and_pragmas(module_info::in, unused_arg_info::in,
    maybe_warn_unused_args::in, maybe_gather_pragma_unused_args::in,
    list(pred_proc_id)::in, set(pred_id)::in,
    list(error_spec)::in, list(error_spec)::out,
    set(gen_pragma_unused_args_info)::in,
    set(gen_pragma_unused_args_info)::out) is det.

gather_warnings_and_pragmas(_, _, _, _, [], _,
        !Specs, !PragmaUnusedArgInfos).
gather_warnings_and_pragmas(ModuleInfo, UnusedArgInfo, DoWarn, DoPragma,
        [PredProcId | PredProcIds], !.WarnedPredIds,
        !Specs, !PragmaUnusedArgInfos) :-
    ( if map.search(UnusedArgInfo, PredProcId, UnusedArgs) then
        PredProcId = proc(PredId, ProcId) ,
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        ( if
            may_gather_warning_pragma_for_pred(ModuleInfo, PredId, PredInfo)
        then
            (
                DoWarn = do_not_warn_unused_args
            ;
                DoWarn = do_warn_unused_args,
                maybe_gather_warning(ModuleInfo, PredInfo, PredId, ProcId,
                    UnusedArgs, !WarnedPredIds, !Specs)
            ),
            (
                DoPragma = do_not_gather_pragma_unused_args
            ;
                DoPragma = do_gather_pragma_unused_args,
                maybe_gather_unused_args_pragma(PredInfo, ProcId, UnusedArgs,
                    !PragmaUnusedArgInfos)
            )
        else
            true
        )
    else
        true
    ),
    gather_warnings_and_pragmas(ModuleInfo, UnusedArgInfo, DoWarn, DoPragma,
        PredProcIds, !.WarnedPredIds, !Specs, !PragmaUnusedArgInfos).

:- pred may_gather_warning_pragma_for_pred(module_info::in,
    pred_id::in, pred_info::in) is semidet.

may_gather_warning_pragma_for_pred(ModuleInfo, PredId, PredInfo) :-
    ( if
        may_gather_warning_pragma_for_pred_old(ModuleInfo, PredId, PredInfo)
    then
        ( if may_gather_warning_pragma_for_pred_new(PredInfo) then
            true
        else
            unexpected($pred, "old succeeds, new fails")
        )
    else
        ( if may_gather_warning_pragma_for_pred_new(PredInfo) then
            unexpected($pred, "old fails, new succeeds")
        else
            fail
        )
    ).

:- pred may_gather_warning_pragma_for_pred_old(module_info::in,
    pred_id::in, pred_info::in) is semidet.

may_gather_warning_pragma_for_pred_old(ModuleInfo, PredId, PredInfo) :-
    not pred_info_is_imported(PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    PredStatus \= pred_status(status_opt_imported),

    % Don't warn about builtins that have unused arguments.
    not pred_info_is_builtin(PredInfo),
    not is_unify_index_or_compare_pred(PredInfo),

    % Don't warn about stubs for procedures with no clauses --
    % in that case, we *expect* none of the arguments to be used.
    pred_info_get_markers(PredInfo, Markers),
    not marker_is_present(Markers, marker_stub),

    % Don't warn about lambda expressions not using arguments.
    % (The warning message for these doesn't contain context,
    % so it's useless).
    Name = pred_info_name(PredInfo),
    not string.sub_string_search(Name, "__LambdaGoal__", _),

    % Don't warn for a specialized version.
    not (
        string.sub_string_search(Name, "__ho", Position),
        string.length(Name, Length),
        IdLen = Length - Position - 4,
        string.right(Name, IdLen, Id),
        string.to_int(Id, _)
    ),
    module_info_get_type_spec_tables(ModuleInfo, TypeSpecTables),
    TypeSpecTables = type_spec_tables(_, TypeSpecForcePreds, _, _),
    not set.member(PredId, TypeSpecForcePreds),

    % Don't warn for a loop-invariant hoisting-generated procedure.
    pred_info_get_origin(PredInfo, Origin),
    not (
        Origin = origin_proc_transform(proc_transform_loop_inv(_, _), _, _, _)
    ),

    % XXX We don't currently generate pragmas for the automatically
    % generated class instance methods because the compiler aborts
    % when trying to read them back in from the `.opt' files.
    not marker_is_present(Markers, marker_class_instance_method),
    not marker_is_present(Markers, marker_named_class_instance_method).

:- pred may_gather_warning_pragma_for_pred_new(pred_info::in) is semidet.

may_gather_warning_pragma_for_pred_new(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    % Previously, this test was effectively:
    % PredStatus \= pred_status(status_imported(_)),
    % PredStatus \= pred_status(status_opt_imported),
    % PredStatus \= pred_status(status_external(_)),
    % However, PredStatus cannot be status_abstract_imported,
    % and the status_pseudo_imported case is caught by
    % the test for made_for_uci below.
    pred_status_defined_in_this_module(PredStatus) = yes,

    pred_info_get_origin(PredInfo, Origin),
    require_complete_switch [Origin]
    (
        Origin = origin_user(UserMade),
        require_complete_switch [UserMade]
        (
            UserMade = user_made_pred(_, _, _),
            % Don't warn about builtins that have unused arguments.
            not pred_info_is_builtin(PredInfo)
        ;
            UserMade = user_made_lambda(_, _, _),
            % Don't warn about lambda expressions not using arguments.
            % (The warning message for these doesn't contain context,
            % so it is useless).
            % NOTE We *could* add any required context. However,
            % in some cases, people use lambdas as a shim between
            % their own code, and library predicates over whose argument
            % lists they have no control. In such cases, ignoring
            % an argument that the user code does not need but the
            % library predicate insists on supplying may be
            % the *whole point* of the lambda expression.
            %
            % XXX The above is nice reasoning, but line in the old version
            % of this test that is relevant here, namely
            %
            %   not string.sub_string_search(Name, "__LambdaGoal__", _),
            %
            % has NOT caused the test to fail since *1997*. Specifically,
            % since Tom's commit 2980b5947418ca8b0ed5312aa87d34b4c6ff5514,
            % which replaced __LambdaGoal__ in the names of the predicates
            % we construct for lambda expressions with IntroducedFrom__.
            true
        ;
            UserMade = user_made_class_method(_, _)
        ;
            UserMade = user_made_instance_method(_, _),
            % XXX We don't currently generate pragmas for the automatically
            % generated class instance methods because the compiler aborts
            % when trying to read them back in from the `.opt' files.
            %
            % I am also not sure whether we would *want* to generated warnings
            % about unused arguments in instance methods. If a class method
            % has an input that is needed by some instances but not others,
            % warning about the instances in the second category would not be
            % helpful, since the class methods needs the argument, and
            % the instance must conform to it.
            ( if
                ( marker_is_present(Markers, marker_class_instance_method)
                ; marker_is_present(Markers,
                    marker_named_class_instance_method)
                )
            then
                fail
            else
                unexpected($pred, "user_made_instance_method with marker")
            )
        ;
            UserMade = user_made_assertion(_, _, _)
            % XXX By construction, assertions should never have any
            % unused arguments, so trying to find them is a waste of time.
        ),

        % Don't warn about stubs for procedures with no clauses --
        % in that case, we *expect* none of the arguments to be used.
        %
        % XXX I (zs) am not sure whether typecheck.m can ever mark as stub
        % a predicate whose origin is not user_made_pred.
        % (There is a test filtering out predicates with marker_class_method,
        % but nothing similar for the other values of UserMade.)
        pred_info_get_markers(PredInfo, Markers),
        not marker_is_present(Markers, marker_stub)
    ;
        Origin = origin_compiler(CompilerMade),
        require_complete_switch [CompilerMade]
        (
            CompilerMade = made_for_uci(_, _),
            fail
        ;
            ( CompilerMade = made_for_deforestation(_, _)
            ; CompilerMade = made_for_solver_repn(_, _)
            ; CompilerMade = made_for_tabling(_, _)
            ; CompilerMade = made_for_mutable(_, _, _)
            ; CompilerMade = made_for_initialise(_, _)
            ; CompilerMade = made_for_finalise(_, _)
            )
            % XXX It is likely that some of these kinds of predicates
            % can never contain unused args, which means that
            % processing them is pointless.
        )
    ;
        Origin = origin_pred_transform(PredTransform, _, _),
        require_complete_switch [PredTransform]
        (
            PredTransform = pred_transform_pragma_type_spec(_),
            fail
        ;
            ( PredTransform = pred_transform_distance_granularity(_)
            ; PredTransform = pred_transform_table_generator
            ; PredTransform = pred_transform_ssdebug(_)
            ; PredTransform = pred_transform_structure_reuse
            )
        )
    ;
        Origin = origin_proc_transform(ProcTransform, _, _, _),
        require_complete_switch [ProcTransform]
        (
            ( ProcTransform = proc_transform_loop_inv(_, _)
            ; ProcTransform = proc_transform_higher_order_spec(_)
            ),
            fail
        ;
            ( ProcTransform = proc_transform_user_type_spec(_, _)
            ; ProcTransform = proc_transform_accumulator(_, _)
            ; ProcTransform = proc_transform_tuple(_, _)
            ; ProcTransform = proc_transform_untuple(_, _)
            ; ProcTransform = proc_transform_dep_par_conj(_)
            ; ProcTransform = proc_transform_par_loop_ctrl
            ; ProcTransform = proc_transform_lcmc(_, _)
            ; ProcTransform = proc_transform_stm_expansion
            ; ProcTransform = proc_transform_io_tabling
            ; ProcTransform = proc_transform_direct_arg_in_out
            )
            % XXX It is likely that some of these kinds of predicates
            % can never contain unused args, which means that
            % processing them is pointless.
        ;
            ProcTransform = proc_transform_unused_args(_),
            % These shouldn't have been created yet,
            % since we do not ever repeat the unused_args pass.
            unexpected($pred, "proc_transform_unused_args")
        )
    ).

:- pred maybe_gather_warning(module_info::in, pred_info::in,
    pred_id::in, proc_id::in, list(int)::in,
    set(pred_id)::in, set(pred_id)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_gather_warning(ModuleInfo, PredInfo, PredId, ProcId, UnusedArgs0,
        !WarnedPredIds, !Specs) :-
    ( if set.member(PredId, !.WarnedPredIds) then
        true
    else
        set.insert(PredId, !WarnedPredIds),
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.lookup(ProcTable, ProcId, Proc),
        pred_info_get_orig_arity(PredInfo, PredFormArity),
        proc_info_get_headvars(Proc, HeadVars),
        NumExtraArgs = num_extra_args(PredFormArity, HeadVars),
        % Strip off the extra type_info/typeclass_info arguments
        % inserted at the front by polymorphism.m.
        drop_poly_inserted_args(NumExtraArgs, UnusedArgs0, UnusedArgs),
        (
            UnusedArgs = [_ | _],
            Spec = report_unused_args(ModuleInfo, PredInfo, UnusedArgs),
            !:Specs = [Spec | !.Specs]
        ;
            UnusedArgs = []
        )
    ).

    % Adjust the argument numbers from how they look in an argument list
    % *with* the extra arguments inserted by polymorphism, to how they would
    % look without them. This means dropping the inserted argument
    % if they appear, and subtracting the number of inserted arguments
    % from the argument numbers of all the other arguments.
    %
:- pred drop_poly_inserted_args(int::in, list(int)::in, list(int)::out) is det.

drop_poly_inserted_args(_, [], []).
drop_poly_inserted_args(NumInserted, [HeadArgWith | TailArgsWith],
        ArgsWithout) :-
    drop_poly_inserted_args(NumInserted, TailArgsWith, TailArgsWithout),
    HeadArgWithout = HeadArgWith - NumInserted,
    ( if HeadArgWithout < 1 then
        ArgsWithout = TailArgsWithout
    else
        ArgsWithout = [HeadArgWithout | TailArgsWithout]
    ).

    % Warn about unused arguments in a predicate. We consider an argument
    % unused *only* if it is unused in *every* mode of the predicate.
    % We also never warn about arguments inserted by the polymorphism pass.
    %
    % The latter test is done by maybe_gather_warning with help from
    % drop_poly_inserted_args.
    %
    % XXX I (zs) would like to know where the first test is done,
    % since it is *not* done here.
    %
:- func report_unused_args(module_info, pred_info, list(int)) = error_spec.

report_unused_args(_ModuleInfo, PredInfo, UnusedArgs) = Spec :-
    list.length(UnusedArgs, NumArgs),
    pred_info_get_context(PredInfo, Context),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    pred_info_get_orig_arity(PredInfo, PredFormArity),
    user_arity_pred_form_arity(PredOrFunc,
        user_arity(UserArityInt), PredFormArity),
    SNA = sym_name_arity(qualified(ModuleName, PredName), UserArityInt),
    Pieces1 = [words("In"), fixed(pred_or_func_to_full_str(PredOrFunc)),
        qual_sym_name_arity(SNA), suffix(":"), nl, words("warning:")],
    UnusedArgNs = list.map(func(N) = int_fixed(N), UnusedArgs),
    UnusedArgPieces = piece_list_to_color_pieces(color_subject, "and", [],
        UnusedArgNs),
    ( if NumArgs = 1 then
        Pieces2 = [words("argument")] ++ UnusedArgPieces ++
            [words("is")] ++ color_as_incorrect([words("unused.")]) ++ [nl]
    else
        Pieces2 = [words("arguments")] ++ UnusedArgPieces ++
            [words("are")] ++ color_as_incorrect([words("unused.")]) ++ [nl]
    ),
    Spec = spec($pred, severity_warning(warn_requested_by_option),
        phase_code_gen, Context, Pieces1 ++ Pieces2).

:- pred maybe_gather_unused_args_pragma(pred_info::in, proc_id::in,
    list(int)::in,
    set(gen_pragma_unused_args_info)::in,
    set(gen_pragma_unused_args_info)::out) is det.

maybe_gather_unused_args_pragma(PredInfo, ProcId, UnusedArgs,
        !UnusedArgInfos) :-
    ( if
        ( pred_info_is_exported(PredInfo)
        ; pred_info_is_opt_exported(PredInfo)
        ; pred_info_is_exported_to_submodules(PredInfo)
        ),
        UnusedArgs = [_ | _]
    then
        ModuleName = pred_info_module(PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        PredName = pred_info_name(PredInfo),
        PredSymName = qualified(ModuleName, PredName),
        pred_info_get_orig_arity(PredInfo, PredFormArity),
        user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
        proc_id_to_int(ProcId, ModeNum),
        PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc, PredSymName,
            UserArity, ModeNum),
        % We can either collect a set of gen_pragma_unused_args
        % with dummy contexts and item sequence numbers now,
        % or we can collect PredNameArityPFMn/UnusedArgs pairs,
        % and add the dummy contexts and item sequence numbers to them
        % later. Both should work; this is marginally simpler to program.
        UnusedArgInfo = gen_pragma_unused_args_info(PredNameArityPFMn,
            UnusedArgs, dummy_context, item_no_seq_num),
        set.insert(UnusedArgInfo, !UnusedArgInfos)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_record_intermod_unused_args(module_info::in, unused_arg_info::in,
    pred_id::in, analysis_info::in, analysis_info::out) is det.

maybe_record_intermod_unused_args(ModuleInfo, UnusedArgInfo, PredId,
        !AnalysisInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_all_procids(PredInfo),
    list.foldl(
        maybe_record_intermod_unused_args_2(ModuleInfo, UnusedArgInfo,
            PredId, PredInfo),
        ProcIds, !AnalysisInfo).

:- pred maybe_record_intermod_unused_args_2(module_info::in,
    unused_arg_info::in, pred_id::in, pred_info::in, proc_id::in,
    analysis_info::in, analysis_info::out) is det.

maybe_record_intermod_unused_args_2(ModuleInfo, UnusedArgInfo,
        PredId, PredInfo, ProcId, !AnalysisInfo) :-
    ( if
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_index_or_compare_pred(PredInfo)
    then
        PPId = proc(PredId, ProcId),
        ( if map.search(UnusedArgInfo, PPId, UnusedArgs) then
            Answer = unused_args_answer(UnusedArgs)
        else
            Answer = unused_args_answer([])
        ),
        ppid_to_module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
        record_result(ModuleName, FuncId, unused_args_call, Answer, optimal,
            !AnalysisInfo)
    else
        true
    ).

%---------------------------------------------------------------------------%

    % If a procedure in this module calls a procedure from another module,
    % then we assume that this module depends on the analysis results of that
    % other procedure.
    %
    % This way of constructing the intermodule dependency graph is easier than
    % actually keeping track of which external analysis results we have used
    % in order to reach analysis results for this module.
    % It works because (1) we only have one type of call pattern so we don't
    % need to know which call patterns are used, and (2) we only record the
    % entire module as a dependency, so we don't have to know which exported
    % procedure is calling (directly or indirectly) which imported procedure.
    %
:- pred record_intermod_dependencies(module_info::in, pred_proc_id::in,
    analysis_info::in, analysis_info::out) is det.

record_intermod_dependencies(ModuleInfo, CallerPredProcId, !AnalysisInfo) :-
    module_info_pred_proc_info(ModuleInfo, CallerPredProcId,
        _CallerPredInfo, CallerProcInfo),
    proc_info_get_goal(CallerProcInfo, Goal),
    pred_proc_ids_called_from_goal(Goal, CalleePredProcIds),
    list.foldl(record_intermod_dependencies_2(ModuleInfo),
        CalleePredProcIds, !AnalysisInfo).

:- pred record_intermod_dependencies_2(module_info::in, pred_proc_id::in,
    analysis_info::in, analysis_info::out) is det.

record_intermod_dependencies_2(ModuleInfo, CalleePredProcId, !AnalysisInfo) :-
    CalleePredProcId = proc(CalleePredId, _),
    module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
    ( if
        pred_info_is_imported_not_external(CalleePredInfo),
        not is_unify_index_or_compare_pred(CalleePredInfo)
    then
        ppid_to_module_name_func_id(ModuleInfo, CalleePredProcId,
            CalleeModule, CalleeFuncId),
        Call = unused_args_call,
        Answer = _ : unused_args_answer,
        get_func_info(ModuleInfo, CalleeModule, CalleeFuncId, Call, Answer,
            FuncInfo),
        record_dependency(CalleeModule, CalleeFuncId, FuncInfo, Call, Answer,
            !AnalysisInfo)
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The types we use to track which arguments may possibly be unused,
% and the operations on them.
%

    % A collection of the local_var_usage_map structures of each procedure.
:- type global_var_usage_map == map(pred_proc_id, local_var_usage_map).

    % Values of this type map variables in a procedure that are
    % not yet known to be used to their aliases. When we find out that
    % either the variable, or one of its aliases, is used, we delete
    % the variable from the map. The absence of the variable from the map
    % implies that the variable is used.
    %
    % XXX Document exactly what set of variables ever get put into this map.
    % Is it just the variables representing the input args of the procedure
    % we are analyzing, or do other variables, such as those aliases,
    % get put in here as well?
:- type local_var_usage_map == map(prog_var, usage_info).

    % For each variable that is not yet known to be used, we record
    % the set of local variables, and the set of procedure arguments,
    % to which it has been aliased. We do this because if any of those
    % aliases turn out to be used, we cannot eliminate the argument
    % represented by the variable.
:- type usage_info
    --->    unused(
                % The set of aliased local variables.
                set(prog_var),

                % The set of aliased procedure argument variables.
                set(arg_var_in_proc)
            ).

    % We identify a specific argument of a procedure by storing ...
:- type arg_var_in_proc
    --->    arg_var_in_proc(
                % ... the identity of the procedure, and ...
                pred_proc_id,

                % ... the identity of the variable that represents that
                % argument in the list of head variables of that procedure
                % (as returned by proc_info_get_headvars). This means that
                % this prog_var is NOT in the varset of the procedure
                % whose whole BODY GOAL we are analyzing, but in the varset
                % of the procedure that is the CALLEE of the call we are
                % processing.
                %
                % Simon chose this representation over an argument number.
                % Both require a translation from the caller to the callee's
                % context: the prog_var representation requires it when an
                % arg_var_in_proc structure is created, while the argument
                % number representation requires it when they are used.
                % The prog_var representation looks simpler, but it is also
                % more error-prone, because the compiler cannot help detect
                % confusing a local variable for a variable in another
                % procedure, or vice versa.
                prog_var
            ).

%---------------------%
%
% Add variable aliases.
%

    % Add a list of aliases for a variable.
    %
:- pred add_aliases(prog_var::in, list(prog_var)::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

add_aliases(Var, Aliases, !LocalVarUsageMap) :-
    ( if map.search(!.LocalVarUsageMap, Var, VarInf0) then
        VarInf0 = unused(AliasVars0, AliasArgs),
        set.insert_list(Aliases, AliasVars0, AliasVars),
        VarInf = unused(AliasVars, AliasArgs),
        map.det_update(Var, VarInf, !LocalVarUsageMap)
    else
        true
    ).

    % Add Alias as an alias for all of Vars.
    %
:- pred add_construction_aliases(prog_var::in, list(prog_var)::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

add_construction_aliases(_, [], !LocalVarUsageMap).
add_construction_aliases(AliasVar, [Var | Vars], !LocalVarUsageMap) :-
    ( if map.search(!.LocalVarUsageMap, Var, VarInfo0) then
        VarInfo0 = unused(AliasVars0, AliasArgs),
        set.insert(AliasVar, AliasVars0, AliasVars),
        VarInfo = unused(AliasVars, AliasArgs),
        map.det_update(Var, VarInfo, !LocalVarUsageMap)
    else
        true
    ),
    add_construction_aliases(AliasVar, Vars, !LocalVarUsageMap).

%---------------------%
%
% Add procedure argument aliases.
%

:- pred add_arg_dep(prog_var::in, pred_proc_id::in, prog_var::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

add_arg_dep(Var, PredProcId, Arg, !LocalVarUsageMap) :-
    ( if map.search(!.LocalVarUsageMap, Var, VarUsage0) then
        VarUsage0 = unused(AliasVars, AliasArgs0),
        set.insert(arg_var_in_proc(PredProcId, Arg), AliasArgs0, AliasArgs),
        VarUsage = unused(AliasVars, AliasArgs),
        map.det_update(Var, VarUsage, !LocalVarUsageMap)
    else
        true
    ).

:- pred add_rev_arg_dep(prog_var::in, pred_proc_id::in, prog_var::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

add_rev_arg_dep(Var, PredProcId, Arg, !LocalVarUsageMap) :-
    add_arg_dep(Arg, PredProcId, Var, !LocalVarUsageMap).

%---------------------%
%
% Record that variables are used.
%

:- pred record_vars_as_used(list(prog_var)::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

record_vars_as_used(Vars, !LocalVarUsageMap) :-
    map.delete_list(Vars, !LocalVarUsageMap).

:- pred record_var_as_used(prog_var::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

record_var_as_used(Var, !LocalVarUsageMap) :-
    map.delete(Var, !LocalVarUsageMap).

%---------------------%
%
% Check whether a variable is used.
%

    % Succeed if and only if the given variable is definitely used.
    %
:- pred proc_arg_var_is_used(global_var_usage_map::in, pred_proc_id::in,
    prog_var::in) is semidet.

proc_arg_var_is_used(GlobalVarUsageMap, PredProcId, Var) :-
    not (
        map.search(GlobalVarUsageMap, PredProcId, LocalVarUsageMap),
        map.contains(LocalVarUsageMap, Var)
    ).

:- pred local_var_is_used(local_var_usage_map::in, prog_var::in) is semidet.

local_var_is_used(LocalVarUsageMap, Var) :-
    not map.contains(LocalVarUsageMap, Var).

%---------------------------------------------------------------------------%

:- pred remove_specified_positions(list(int)::in,
    list(T)::in, list(T)::out) is det.

remove_specified_positions(ArgNumsToRemove, !List) :-
    remove_specified_positions_loop(ArgNumsToRemove, 1, !List).

:- pred remove_specified_positions_loop(list(int)::in, int::in,
    list(T)::in, list(T)::out) is det.

remove_specified_positions_loop(_ArgNumsToRemove, _ArgNum,
        List0 @ [], List0).
remove_specified_positions_loop(ArgNumsToRemove, ArgNum,
        List0 @ [Head0 | Tail0], List) :-
    (
        ArgNumsToRemove = [],
        List = List0
    ;
        ArgNumsToRemove = [_ | _],
        remove_specified_positions_loop(ArgNumsToRemove, ArgNum + 1,
            Tail0, Tail),
        ( if list.member(ArgNum, ArgNumsToRemove) then
            List = Tail
        else
            List = [Head0 | Tail]
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Types and instances used by mmc_analysis.m.
%

:- type unused_args_func_info
    --->    unused_args_func_info(pred_form_arity).

:- type unused_args_call
    --->    unused_args_call.

:- type unused_args_answer
    --->    unused_args_answer(
                % The list of unused arguments is in sorted order.
                args    :: list(int)
            ).

:- func get_unused_args(unused_args_answer) = list(int).

get_unused_args(UnusedArgs) = UnusedArgs ^ args.

:- instance analysis(unused_args_func_info, unused_args_call,
    unused_args_answer) where
[
    analysis_name(_, _) = analysis_name,
    analysis_version_number(_, _) = 3,
    preferred_fixpoint_type(_, _) = least_fixpoint,
    bottom(unused_args_func_info(pred_form_arity(Arity)), _) =
        unused_args_answer(1 .. Arity),
    top(_, _) = unused_args_answer([]),
    (get_func_info(ModuleInfo, ModuleName, FuncId, _, _, FuncInfo) :-
        func_id_to_ppid(ModuleInfo, ModuleName, FuncId, proc(PredId, _)),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_orig_arity(PredInfo, PredFormArity),
        FuncInfo = unused_args_func_info(PredFormArity)
    )
].

:- func analysis_name = string.

analysis_name = "unused_args".

:- instance call_pattern(unused_args_func_info, unused_args_call) where [].
:- instance partial_order(unused_args_func_info, unused_args_call) where [
    ( more_precise_than(_, _, _) :-
        semidet_fail
    ),
    equivalent(_, Call, Call)
].

:- instance to_term(unused_args_call) where [
    ( to_term(unused_args_call) = Term :-
        Term = term.functor(atom("any"), [], dummy_context)
    ),
    ( from_term(Term, unused_args_call) :-
        Term = term.functor(atom("any"), [], _)
    )
].

:- instance answer_pattern(unused_args_func_info, unused_args_answer) where [].
:- instance partial_order(unused_args_func_info, unused_args_answer) where [
    (more_precise_than(_, Answer1, Answer2) :-
        Answer1 = unused_args_answer(Args1),
        Answer2 = unused_args_answer(Args2),
        set.subset(sorted_list_to_set(Args2), sorted_list_to_set(Args1))
    ),
    equivalent(_, Args, Args)
].

:- instance to_term(unused_args_answer) where [
    ( to_term(unused_args_answer(Args)) = Term :-
        type_to_term(Args, Term)
    ),
    ( from_term(Term, unused_args_answer(Args)) :-
        term_to_type(Term, Args)
    )
].


%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Predicates that can help debug the code of this module.
%

:- pred write_global_var_usage_map(io.text_output_stream::in, module_info::in,
    global_var_usage_map::in, io::di, io::uo) is det.

write_global_var_usage_map(Stream, ModuleInfo, GlobalVarUsageMap, !IO) :-
    map.foldl(write_local_var_usage_map(Stream, ModuleInfo),
        GlobalVarUsageMap, !IO).

:- pred write_local_var_usage_map(io.text_output_stream::in, module_info::in,
    pred_proc_id::in, local_var_usage_map::in, io::di, io::uo) is det.

write_local_var_usage_map(Stream, ModuleInfo, PredProcId,
        LocalVarUsageMap, !IO) :-
    PredProcIdStr = pred_proc_id_to_dev_string(ModuleInfo, PredProcId),
    io.format(Stream, "\n%s:\n", [s(PredProcIdStr)], !IO),
    map.to_assoc_list(LocalVarUsageMap, LocalVarUsages),
    module_info_proc_info(ModuleInfo, PredProcId, ProcInfo),
    proc_info_get_var_table(ProcInfo, VarTable),
    list.foldl2(
        write_var_usage_info(Stream, ModuleInfo, VarTable), LocalVarUsages,
        [], RevNoDependVars, !IO),
    list.reverse(RevNoDependVars, NoDependVars),
    (
        NoDependVars = []
    ;
        NoDependVars = [_ | _],
        NoDependVarsStr =
            mercury_vars_to_string(VarTable, print_name_and_num, NoDependVars),
        io.format(Stream, "nodepend vars: %s\n", [s(NoDependVarsStr)], !IO)
    ).

:- pred write_var_usage_info(io.text_output_stream::in, module_info::in,
    var_table::in, pair(prog_var, usage_info)::in,
    list(prog_var)::in, list(prog_var)::out, io::di, io::uo) is det.

write_var_usage_info(Stream, ModuleInfo, VarTable, Var - UsageInfo,
        !RevNoDependVars, !IO) :-
    UsageInfo = unused(Vars, Args),
    set.to_sorted_list(Vars, VarList),
    set.to_sorted_list(Args, ArgList),
    ( if VarList = [], ArgList = [] then
        !:RevNoDependVars = [Var | !.RevNoDependVars]
    else
        VarStr = mercury_var_to_string(VarTable, print_name_and_num, Var),
        io.format(Stream, "dependencies of %s:\n", [s(VarStr)], !IO),
        (
            VarList = []
        ;
            VarList = [_ | _],
            VarListStr =
                mercury_vars_to_string(VarTable, print_name_and_num, VarList),
            io.format(Stream, "on variables: %s\n", [s(VarListStr)], !IO)
        ),
        (
            ArgList = []
        ;
            ArgList = [_ | _],
            io.write_string(Stream, "on arguments:\n", !IO),
            list.foldl(write_arg_var_in_proc(Stream, ModuleInfo), ArgList, !IO)
        )
    ).

:- pred write_arg_var_in_proc(io.text_output_stream::in, module_info::in,
    arg_var_in_proc::in, io::di, io::uo) is det.

write_arg_var_in_proc(Stream, ModuleInfo, ArgVarInProc, !IO) :-
    ArgVarInProc = arg_var_in_proc(PredProcId, Var),
    PredProcIdStr = pred_proc_id_to_dev_string(ModuleInfo, PredProcId),
    module_info_proc_info(ModuleInfo, PredProcId, ProcInfo),
    proc_info_get_var_table(ProcInfo, VarTable),
    VarStr = mercury_var_to_string(VarTable, print_name_and_num, Var),
    io.format(Stream, "%s: %s\n", [s(PredProcIdStr), s(VarStr)], !IO).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.unused_args.
%---------------------------------------------------------------------------%
