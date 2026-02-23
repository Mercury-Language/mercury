%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: unused_args_optimize.m.
%
% Given information about which procedures contain unused arguments,
%
% - create clones of those procedures,
%
% - delete from those clones the unused arguments, and all the code that
%   operate on them and on any unused local variables, and
%
% - redirect calls that used to be made to the originals of those clones
%   to call the clones instead, updationg the argument list accordingly.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.unused_args_optimize.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module transform_hlds.unused_args_base_ops.

:- import_module bool.
:- import_module list.
:- import_module map.

    % Information about procedures which have new predicates created for the
    % optimized version.
    %
:- type new_proc_map == map(pred_proc_id, new_proc_info).

    % New pred_id, proc_id, name, and the indices in the argument vector
    % of the arguments that have been removed.
:- type new_proc_info
    --->    new_proc_info(pred_id, proc_id, sym_name, list(int)).

    % optimize_unused_args(VeryVerbose, UnusedArgInfo, GlobalVarUsageMap,
    %   FixpointPredProcIds, NewProcMap0, !ModuleInfo)
    %
:- pred optimize_unused_args(bool::in, unused_arg_info::in,
    global_var_usage_map::in, list(pred_proc_id)::in, new_proc_map::in,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%

    % Create a pred_info for an imported pred with a pragma unused_args
    % in the .opt file.
    %
:- pred make_imported_unused_args_pred_info(pred_proc_id::in, list(int)::in,
    new_proc_map::in, new_proc_map::out,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module analysis.framework.
:- import_module analysis.operations.
:- import_module check_hlds.
:- import_module check_hlds.mode_test.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
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
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_parse_tree.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.mmc_analysis.
:- import_module transform_hlds.unused_args_analysis.

:- import_module io.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

optimize_unused_args(VeryVerbose, UnusedArgInfo, GlobalVarUsageMap,
        FixpointPredProcIds, NewProcMap0, !ModuleInfo) :-
    map.keys(UnusedArgInfo, PredProcIdsToFix),
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
    ).

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
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    (
        IntermodAnalysis = yes,
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
        IntermodAnalysis = no,
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
:- end_module transform_hlds.unused_args_optimize.
%---------------------------------------------------------------------------%
