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

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.
:- import_module transform_hlds.unused_args_warn_pragma.

:- import_module list.
:- import_module set.

:- pred unused_args_process_module(maybe_gather_pragma_unused_args::in,
    maybe_record_analysis_unused_args::in,
    list(error_spec)::out, set(gen_pragma_unused_args_info)::out,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module analysis.framework.
:- import_module analysis.operations.
:- import_module hlds.goal_vars.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.hlds_rtti.
:- import_module hlds.inst_match.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_name.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.mmc_analysis.
:- import_module transform_hlds.unused_args_analysis.
:- import_module transform_hlds.unused_args_base_ops.
:- import_module transform_hlds.unused_args_optimize.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

unused_args_process_module(GatherPragmas, RecordAnalysis,
        Specs, PragmaUnusedArgInfos, !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    init_global_var_usage_map(GlobalVarUsageMap0, FixpointPredProcIds,
        NewProcMap0, !ModuleInfo),
    % maybe_write_string(VeryVerbose, "% Finished initialisation.\n", !IO),

    record_required_vars_as_used_to_fixpoint(0, !.ModuleInfo,
        FixpointPredProcIds, GlobalVarUsageMap0, GlobalVarUsageMap),
    % maybe_write_string(VeryVerbose, "% Finished analysis.\n", !IO),

    map.init(ProcToUnusedArgsMap0),
    build_proc_to_unused_args_map(!.ModuleInfo, GlobalVarUsageMap,
        FixpointPredProcIds, ProcToUnusedArgsMap0, ProcToUnusedArgsMap),

    globals.lookup_bool_option(Globals, warn_unused_args, DoWarnBool),
    ( DoWarnBool = no,  DoWarn = do_not_warn_unused_args
    ; DoWarnBool = yes, DoWarn = do_warn_unused_args
    ),
    ( if
        ( DoWarn = do_warn_unused_args
        ; GatherPragmas = do_gather_pragma_unused_args
        )
    then
        gather_warnings_and_pragmas(!.ModuleInfo, ProcToUnusedArgsMap,
            DoWarn, GatherPragmas, Specs, PragmaUnusedArgInfos)
    else
        Specs = [],
        set.init(PragmaUnusedArgInfos)
    ),
    (
        RecordAnalysis = do_record_analysis_unused_args,
        record_analysis_unused_args(ProcToUnusedArgsMap, FixpointPredProcIds,
            !ModuleInfo)
    ;
        RecordAnalysis = do_not_record_analysis_unused_args
    ),
    globals.get_opt_tuple(Globals, OptTuple),
    OptUnusedArgs = OptTuple ^ ot_opt_unused_args,
    (
        OptUnusedArgs = opt_unused_args,
        optimize_unused_args(VeryVerbose, ProcToUnusedArgsMap,
            GlobalVarUsageMap, FixpointPredProcIds, NewProcMap0, !ModuleInfo)
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
            InitRequiredBy = required_by(set.init, set.init),
            init_requiring_vars_for_var(InitRequiredBy, Vars,
                !LocalVarUsageMap),
            record_output_args_as_used(!.ModuleInfo, ProcInfo,
                !LocalVarUsageMap),

            proc_interface_should_use_typeinfo_liveness(PredInfo, ProcId,
                Globals, TypeInfoLiveness),
            PredProcId = proc(PredId, ProcId),
            (
                TypeInfoLiveness = yes,
                proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
                require_typeinfo_liveness_for_vars(PredProcId, VarTable,
                    RttiVarMaps, Vars, !LocalVarUsageMap)
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
            InitRequiredBy = required_by(set.init, set.init),
            proc_info_get_headvars(ProcInfo, HeadVars),
            list.map(list.det_index1(HeadVars), UnusedArgs, UnusedVars),
            init_requiring_vars_for_var(InitRequiredBy, UnusedVars,
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

:- pred init_requiring_vars_for_var(required_by::in, list(prog_var)::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

init_requiring_vars_for_var(_, [], !LocalVarUsageMap).
init_requiring_vars_for_var(RequiredBy, [Var | Vars], !LocalVarUsageMap) :-
    map.det_insert(Var, RequiredBy, !LocalVarUsageMap),
    init_requiring_vars_for_var(RequiredBy, Vars, !LocalVarUsageMap).

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
:- pred require_typeinfo_liveness_for_vars(pred_proc_id::in, var_table::in,
    rtti_varmaps::in, list(prog_var)::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

require_typeinfo_liveness_for_vars(_, _, _, [], !LocalVarUsageMap).
require_typeinfo_liveness_for_vars(PredProcId, VarTable, RttiVarMaps,
        [Var | Vars], !LocalVarUsageMap) :-
    require_typeinfo_liveness_for_var(PredProcId, VarTable, RttiVarMaps,
        Var, !LocalVarUsageMap),
    require_typeinfo_liveness_for_vars(PredProcId, VarTable, RttiVarMaps,
        Vars, !LocalVarUsageMap).

:- pred require_typeinfo_liveness_for_var(pred_proc_id::in, var_table::in,
    rtti_varmaps::in, prog_var::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

require_typeinfo_liveness_for_var(PredProcId, VarTable, RttiVarMaps, Var,
        !LocalVarUsageMap) :-
    lookup_var_type(VarTable, Var, Type),
    type_vars_in_type(Type, TVars),
    list.map(tvar_to_type_info_var(RttiVarMaps), TVars, TypeInfoVars),
    ArgVarInProc = arg_var_in_proc(PredProcId, Var),
    local_vars_are_required_by_proc_arg(TypeInfoVars, ArgVarInProc,
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
        add_plain_call_arg_deps(CalleePredProcId, CallArgVars, CalleeHeadVars,
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
            local_var_is_required_by_local_vars(Source, [Target],
                !LocalVarUsageMap)
        )
    ;
        Unify = construct(CellVar, _, ArgVars, _, _, _, _),
        expect(unify(CellVar, LHSVar), $pred, "LHSVar != CellVar"),
        ( if local_var_is_used(!.LocalVarUsageMap, CellVar) then
            record_vars_as_used(ArgVars, !LocalVarUsageMap)
        else
            local_vars_are_required_by_local_var(ArgVars, CellVar,
                !LocalVarUsageMap)
        )
    ;
        Unify = deconstruct(CellVar, _, ArgVars, ArgModes, CanFail, _),
        expect(unify(CellVar, LHSVar), $pred, "LHSVar != CellVar"),
        partition_deconstruct_args(Info, ArgVars, ArgModes,
            InputVars, OutputVars),
        % The deconstructed variable is used if any of the variables that
        % the deconstruction binds are used.
        local_var_is_required_by_local_vars(CellVar, OutputVars,
            !LocalVarUsageMap),
        % Treat a deconstruction that further instantiates its left arg
        % as a partial construction.
        local_vars_are_required_by_local_var(InputVars, CellVar,
            !LocalVarUsageMap),
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
:- pred add_plain_call_arg_deps(pred_proc_id::in,
    list(prog_var)::in, list(prog_var)::in,
    local_var_usage_map::in, local_var_usage_map::out) is det.

add_plain_call_arg_deps(PredProcId, CallArgVars, CalleeArgVars,
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
        ArgVarInProc = arg_var_in_proc(PredProcId, HeadCalleeArgVar),
        local_var_is_required_by_proc_arg(HeadCallArgVar, ArgVarInProc,
            !LocalVarUsageMap),
        add_plain_call_arg_deps(PredProcId, TailCallArgVars, TailCalleeArgVars,
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
% Interprocedural analysis section.
%

    % Start by assuming that the only input arguments a procedure needs
    % are the ones used in its own procedure body. This denotes the
    % ideal situation, in the sense it has the most "unused" arguments
    % that can be optimized. However, this assumption is false in most cases,
    % because it does not account for discrepancies such as local variables
    % in procedure P1 that are needed in the body of procedure P2, to which
    % they are passed.
    %
    % This predicate does a top-down iteration to find the greatest fixpoint
    % of the operation that fixes such discrepancies. Each iteration
    % fixes the discrepancies implicit in its initial value of
    % !.GlobalVarUsageMap by marking more local variables is definitely used.
    % When we get to a fixpoint, by definition there will be no discrepancies
    % left, and the final GlobalVarUsageMap will record a variable as unused
    % if it can be truly deleted from the program without any ill effects.
    %
    % The reason why we compute the greatest fixpoint is because it allows us
    % to warn about and to optimize not just unused variables that are
    % local to their defining procedure, but also input arguments that
    % are seemingly used by being passed to recursive calls, possibly even
    % mutually recursive calls, but which are not used for anything else.
    %
    % One thing that we do NOT do, but we could consider doing,
    % is finding input arguments that are "used" not just by passing them
    % around as input to (possibly mutually) recursive calls, but also
    % to return them unchanged as output arguments. Such input arguments
    % should not be deleted on their own. They should always be deleted
    % together with the output argument they are returned as, and calls
    % from outside the clique of recursive procedures to inside must be
    % extended with code that copies the input argument to the unchanged
    % output argument. (This cannot be done if this caller is in another
    % module, so the compiler would need to create and export a shim
    % procedure to do this copying.)
    %
:- pred record_required_vars_as_used_to_fixpoint(int::in, module_info::in,
    list(pred_proc_id)::in,
    global_var_usage_map::in, global_var_usage_map::out) is det.

record_required_vars_as_used_to_fixpoint(PassNum, ModuleInfo,
        LocalPredProcIds, !GlobalVarUsageMap) :-
    % If we find any local variables in any procedures that
    %
    % - were not known to be definitely used, but
    % - were known to be required for the computation of some variable
    %   or procedure argument that is NOW known to be definitely used,
    %
    % then mark them as definitely used, and set !:Changed accordingly.
    record_required_vars_as_used_in_procs(LocalPredProcIds,
        unchanged, Changed, !GlobalVarUsageMap),
    (
        Changed = changed,
        % There are some new variables that were not known to be definitely
        % used BEFORE the call to record_required_vars_as_used_in_procs,
        % which are known to be definitely used NOW. This means that
        % any variable in any procedure that the computation of the values
        % of these require must *also* be marked as definitely used.
        %
        % Technically, we don't always need to reprocess *all* procedures
        % in LocalPredProcIds; the only ones we need to process are the
        % callers of procedures for which record_required_vars_as_used_in_proc
        % set !:Changed to "yes".
        %
        % However, keeping track of this set of procedures is likely to take
        % a nontrivial amount of time. Revisiting every procedure, even
        % the ones that we do not NEED to revisit, will likely be
        % just as fast, other than in cases of pathologically-slow convergence.
        trace [compile_time(flag("unused_args_var_usage")), io(!IO)] (
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            io.format(DebugStream,
                "\nVARIABLE USAGE MAP AFTER PASS %d\n", [i(PassNum)], !IO),
            write_global_var_usage_map(DebugStream, ModuleInfo,
                !.GlobalVarUsageMap, !IO)
        ),
        record_required_vars_as_used_to_fixpoint(PassNum + 1, ModuleInfo,
            LocalPredProcIds, !GlobalVarUsageMap)
    ;
        Changed = unchanged
    ).

:- pred record_required_vars_as_used_in_procs(list(pred_proc_id)::in,
    maybe_changed::in, maybe_changed::out,
    global_var_usage_map::in, global_var_usage_map::out) is det.

record_required_vars_as_used_in_procs([], !Changed, !GlobalVarUsageMap).
record_required_vars_as_used_in_procs([PredProcId | PredProcIds],
        !Changed, !GlobalVarUsageMap) :-
    record_required_vars_as_used_in_proc(PredProcId,
        !Changed, !GlobalVarUsageMap),
    record_required_vars_as_used_in_procs(PredProcIds,
        !Changed, !GlobalVarUsageMap).

    % If we find any local variables that
    %
    % - were not known to be definitely used, but
    % - were known to be required for the computation of some variable
    %   of procedure argument that is NOW known to be definitely used,
    %
    % then mark them as definitely used, and set !:Changed accordingly.
    %
:- pred record_required_vars_as_used_in_proc(pred_proc_id::in,
    maybe_changed::in, maybe_changed::out,
    global_var_usage_map::in, global_var_usage_map::out) is det.

record_required_vars_as_used_in_proc(PredProcId,
        !Changed, !GlobalVarUsageMap) :-
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
    record_required_vars_as_used(!.GlobalVarUsageMap, Vars,
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
:- pred record_required_vars_as_used(global_var_usage_map::in,
    list(prog_var)::in, maybe_changed::in, maybe_changed::out,
    local_var_usage_map::in, local_var_usage_map::out) is det.

record_required_vars_as_used(_, [], !Changed, !LocalVarUsageMap).
record_required_vars_as_used(GlobalVarUsageMap, [Var | Vars],
        !Changed, !LocalVarUsageMap) :-
    map.lookup(!.LocalVarUsageMap, Var, RequiredBy),
    RequiredBy = required_by(RequiringLocalVars0, RequiringProcArgs0),
    ( if
        (
            % Are there any used procedure arguments that Var depends on?
            some [Argument] (
                set.member(Argument, RequiringProcArgs0),
                Argument = arg_var_in_proc(PredProcId, ArgVar),
                proc_arg_var_is_used(GlobalVarUsageMap, PredProcId, ArgVar)
            )
        ;
            % Are there any used local variables that Var depends on?
            some [X] (
                set.member(X, RequiringLocalVars0),
                local_var_is_used(!.LocalVarUsageMap, X)
            )
        )
    then
        % Mark the current variable as used. Note that we update the same
        % data structure (!LocalVarUsageMap) as we test in the condition
        % above. This is OK because the order in which we mark variables
        % in !.LocalVarUsageMap as used does not matter; the iteration
        % performed by record_required_vars_as_used_to_fixpoint
        % is guaranteed to reach the same final result.
        record_var_as_used(Var, !LocalVarUsageMap),
        !:Changed = changed
    else
        true
    ),
    record_required_vars_as_used(GlobalVarUsageMap, Vars,
        !Changed, !LocalVarUsageMap).

%---------------------------------------------------------------------------%

:- pred build_proc_to_unused_args_map(module_info::in,
    global_var_usage_map::in, list(pred_proc_id)::in,
    proc_to_unused_args_map::in, proc_to_unused_args_map::out) is det.

build_proc_to_unused_args_map(_, _, [], !ProcToUnusedArgsMap).
build_proc_to_unused_args_map(ModuleInfo, GlobalVarUsageMap,
        [PredProcId | PredProcIds], !ProcToUnusedArgsMap) :-
    PredProcId = proc(PredId, ProcId),
    map.lookup(GlobalVarUsageMap, PredProcId, LocalVarUsageMap),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    get_unused_arg_nums(LocalVarUsageMap, HeadVars, 1, UnusedArgs),
    map.det_insert(PredProcId, UnusedArgs, !ProcToUnusedArgsMap),
    build_proc_to_unused_args_map(ModuleInfo, GlobalVarUsageMap,
        PredProcIds, !ProcToUnusedArgsMap).

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
:- end_module transform_hlds.unused_args.
%---------------------------------------------------------------------------%
