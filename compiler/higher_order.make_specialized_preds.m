%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module transform_hlds.higher_order.make_specialized_preds.
:- interface.

:- import_module transform_hlds.higher_order.higher_order_global_info.

:- import_module maybe.
:- import_module io.

    % Process requests until there are no new requests to process.
    %
:- pred recursively_process_ho_spec_requests(maybe(io.text_output_stream)::in,
    higher_order_global_info::in, higher_order_global_info::out,
    io::di, io::uo) is det.

    % Process one lot of requests, returning requests for any
    % new specializations made possible by the first lot.
    %
:- pred process_ho_spec_requests(maybe(io.text_output_stream)::in,
    higher_order_global_info::in, higher_order_global_info::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.type_util.
:- import_module hlds.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module hlds.var_table_hlds.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.indent.
:- import_module libs.optimization_options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_unify.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.higher_order.specialize_calls.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.
:- import_module uint.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Predicates to process requests for specialization, and create any
% new predicates that are required.
%

recursively_process_ho_spec_requests(MaybeProgressStream, !GlobalInfo, !IO) :-
    ( if set.is_empty(!.GlobalInfo ^ hogi_requests) then
        true
    else
        process_ho_spec_requests(MaybeProgressStream, !GlobalInfo, !IO),
        recursively_process_ho_spec_requests(MaybeProgressStream,
            !GlobalInfo, !IO)
    ).

process_ho_spec_requests(MaybeProgressStream, !GlobalInfo, !IO) :-
    Requests0 = set.to_sorted_list(!.GlobalInfo ^ hogi_requests),
    !GlobalInfo ^ hogi_requests := set.init,
    list.foldl3(filter_request(MaybeProgressStream, !.GlobalInfo), Requests0,
        [], Requests, [], LoopRequests, !IO),
    (
        Requests = []
    ;
        Requests = [_ | _],
        some [!PredProcsToFix] (
            set.init(!:PredProcsToFix),
            maybe_create_new_ho_spec_preds(MaybeProgressStream, Requests,
                [], NewPredList, !PredProcsToFix, !GlobalInfo, !IO),
            list.foldl(check_loop_request(!.GlobalInfo), LoopRequests,
                !PredProcsToFix),
            set.to_sorted_list(!.PredProcsToFix, PredProcs)
        ),
        ho_fixup_specialized_versions(NewPredList, !GlobalInfo),
        ho_fixup_preds(PredProcs, !GlobalInfo),
        (
            NewPredList = [_ | _],
            % The dependencies may have changed, so the dependency graph
            % needs to rebuilt for inlining to work properly.
            ModuleInfo0 = !.GlobalInfo ^ hogi_module_info,
            module_info_clobber_dependency_info(ModuleInfo0, ModuleInfo),
            !GlobalInfo ^ hogi_module_info := ModuleInfo
        ;
            NewPredList = []
        )
    ).

%---------------------------------------------------------------------------%

    % Filter out requests for higher-order specialization for preds which are
    % too large. Maybe we could allow programmers to declare which predicates
    % they want specialized, as with inlining?
    %
    % Also filter out requests that would create specialized versions
    % of specialized versions, since for some fairly contrived examples
    % involving recursively building up lambda expressions,
    % this can create ridiculous numbers of versions.
    %
:- pred filter_request(maybe(io.text_output_stream)::in,
    higher_order_global_info::in, ho_request::in,
    list(ho_request)::in, list(ho_request)::out,
    list(ho_request)::in, list(ho_request)::out, io::di, io::uo) is det.

filter_request(MaybeProgressStream, Info, Request,
        !AcceptedRequests, !LoopRequests, !IO) :-
    ModuleInfo = Info ^ hogi_module_info,
    Request = ho_request(CallingPredProcId, CalledPredProcId, _, _, HOArgs,
        _, _, RequestKind, Context),
    CalledPredProcId = proc(CalledPredId, _),
    module_info_pred_info(ModuleInfo, CalledPredId, PredInfo),
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredFormArity = pred_info_pred_form_arity(PredInfo),
    pred_info_get_arg_types(PredInfo, Types),
    ActualArity = arg_list_arity(Types),
    (
        MaybeProgressStream = no
    ;
        MaybeProgressStream = yes(ProgressStream),
        write_request(ProgressStream, ModuleInfo, "Request for",
            qualified(PredModule, PredName), PredFormArity, ActualArity,
            no, HOArgs, Context, !IO)
    ),
    (
        RequestKind = user_type_spec,
        % Ignore the size limit for user specified specializations.
        maybe_write_string_to_stream(MaybeProgressStream,
            "%    request specialized (user-requested specialization)\n", !IO),
        list.cons(Request, !AcceptedRequests)
    ;
        RequestKind = non_user_type_spec,
        GoalSizeMap = Info ^ hogi_goal_size_map,
        ( if map.search(GoalSizeMap, CalledPredId, GoalSize0) then
            GoalSize = GoalSize0
        else
            % This can happen for a specialized version.
            GoalSize = 0
        ),
        ( if
            GoalSize > Info ^ hogi_params ^ param_size_limit
        then
            maybe_write_string_to_stream(MaybeProgressStream,
                "%    not specializing (goal too large).\n", !IO)
        else if
            higher_order_args_size(HOArgs) >
                Info ^ hogi_params ^ param_arg_limit
        then
            % If the arguments are too large, we can end up producing a
            % specialized version with massive numbers of arguments, because
            % all of the curried arguments are passed as separate arguments.
            % Without this extras/xml/xml.parse.chars.m takes forever to
            % compile.
            maybe_write_string_to_stream(MaybeProgressStream,
                "%    not specializing (args too large).\n", !IO)
        else if
            % To ensure termination of the specialization process, the depth
            % of the higher-order arguments must strictly decrease compared
            % to parents with the same original pred_proc_id.
            VersionInfoMap = Info ^ hogi_version_info,
            ( if
                map.search(VersionInfoMap, CalledPredProcId, CalledVersionInfo)
            then
                CalledVersionInfo = version_info(OrigPredProcId, _, _, _)
            else
                OrigPredProcId = CalledPredProcId
            ),
            map.search(VersionInfoMap, CallingPredProcId, CallingVersionInfo),
            CallingVersionInfo = version_info(_, _, _, ParentVersions),
            ArgDepth = higher_order_args_depth(HOArgs),
            some [ParentVersion] (
                list.member(ParentVersion, ParentVersions),
                ParentVersion = parent_version_info(OrigPredProcId,
                    OldArgDepth),
                ArgDepth >= OldArgDepth
            )
        then
            !:LoopRequests = [Request | !.LoopRequests],
            maybe_write_string_to_stream(MaybeProgressStream,
                "%    not specializing (recursive specialization).\n", !IO)
        else
            maybe_write_string_to_stream(MaybeProgressStream,
                "%    request specialized.\n", !IO),
            list.cons(Request, !AcceptedRequests)
        )
    ).

    % If we were not allowed to create a specialized version because the
    % loop check failed, check whether the version was created for another
    % request for which the loop check succeeded.
    %
:- pred check_loop_request(higher_order_global_info::in, ho_request::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

check_loop_request(Info, Request, !PredsToFix) :-
    CallingPredProcId = Request ^ rq_caller,
    CalledPredProcId = Request ^ rq_callee,
    ( if
        map.search(Info ^ hogi_new_pred_map, CalledPredProcId, SpecVersions0),
        some [Version] (
            set.member(Version, SpecVersions0),
            version_matches(Info ^ hogi_params, Info ^ hogi_module_info,
                Request, Version, _)
        )
    then
        set.insert(CallingPredProcId, !PredsToFix)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_create_new_ho_spec_preds(maybe(io.text_output_stream)::in,
    list(ho_request)::in, list(new_pred)::in, list(new_pred)::out,
    set(pred_proc_id)::in, set(pred_proc_id)::out,
    higher_order_global_info::in, higher_order_global_info::out,
    io::di, io::uo) is det.

maybe_create_new_ho_spec_preds(_, [],
        !NewPreds, !PredsToFix, !Info, !IO).
maybe_create_new_ho_spec_preds(MaybeProgressStream, [Request | Requests],
        !NewPreds, !PredsToFix, !Info, !IO) :-
    Request = ho_request(CallingPredProcId, CalledPredProcId,
        _, _, _, _, _, _, _),
    set.insert(CallingPredProcId, !PredsToFix),
    ( if
        % Check that we are not redoing the same pred.
        % SpecVersions0 are pred_proc_ids of the specialized versions
        % of the current pred.
        NewPredMap = !.Info ^ hogi_new_pred_map,
        map.search(NewPredMap, CalledPredProcId, SpecVersions0),
        set.member(Version, SpecVersions0),
        version_matches(!.Info ^ hogi_params, !.Info ^ hogi_module_info,
            Request, Version, _)
    then
        true
    else
        create_new_ho_spec_pred(MaybeProgressStream, Request, NewPred,
            !Info, !IO),
        !:NewPreds = [NewPred | !.NewPreds]
    ),
    maybe_create_new_ho_spec_preds(MaybeProgressStream, Requests,
        !NewPreds, !PredsToFix, !Info, !IO).

    % Here we create the pred_info for the new predicate.
    %
:- pred create_new_ho_spec_pred(maybe(io.text_output_stream)::in,
    ho_request::in, new_pred::out,
    higher_order_global_info::in, higher_order_global_info::out,
    io::di, io::uo) is det.

create_new_ho_spec_pred(MaybeProgressStream, Request, NewPred, !Info, !IO) :-
    Request = ho_request(CallerPPId, CalleePPId, CallArgsTypes,
        ExtraTypeInfoTVars, HOArgs, CallerTVarSet, TypeInfoLiveness,
        RequestKind, Context),
    CallerPPId = proc(CallerPredId, CallerProcId),
    ModuleInfo0 = !.Info ^ hogi_module_info,
    module_info_pred_proc_info(ModuleInfo0, CalleePPId, PredInfo0, ProcInfo0),

    Name0 = pred_info_name(PredInfo0),
    PredFormArity = pred_info_pred_form_arity(PredInfo0),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo0),
    PredModuleName = pred_info_module(PredInfo0),
    pred_info_get_arg_types(PredInfo0, ArgTVarSet, ExistQVars, Types),

    (
        RequestKind = user_type_spec,
        % If this is a user-guided type specialisation, the new name comes from
        % the name and mode number of the requesting predicate. The mode number
        % is included because we want to avoid the creation of more than one
        % predicate with the same name if more than one mode of a predicate
        % is specialized. Since the names of e.g. deep profiling proc_static
        % structures are derived from the names of predicates, duplicate
        % predicate names lead to duplicate global variable names and hence to
        % link errors.
        CallerPredName0 = predicate_name(ModuleInfo0, CallerPredId),

        % The higher_order_arg_order_version part is to avoid segmentation
        % faults or other errors when the order or number of extra arguments
        % changes. If the user does not recompile all affected code, the
        % program will not link.
        Transform = tn_user_type_spec(PredOrFunc, CallerPredId, CallerProcId,
            higher_order_arg_order_version),
        make_transformed_pred_name(CallerPredName0, Transform, SpecName),
        ProcTransform =
            proc_transform_user_type_spec(CallerPredId, CallerProcId),
        NewProcId = CallerProcId,
        % For exported predicates, the type specialization must be exported.
        % For opt_imported predicates, we only want to keep this version
        % if we do some other useful specialization on it.
        pred_info_get_status(PredInfo0, PredStatus)
    ;
        RequestKind = non_user_type_spec,
        NewProcId = hlds_pred.initial_proc_id,
        SeqNumCounter0 = !.Info ^ hogi_next_id,
        counter.allocate(SeqNum, SeqNumCounter0, SeqNumCounter),
        !Info ^ hogi_next_id := SeqNumCounter,
        Transform = tn_higher_order(PredOrFunc, SeqNum),
        make_transformed_pred_name(Name0, Transform, SpecName),
        ProcTransform = proc_transform_higher_order_spec(SeqNum),
        PredStatus = pred_status(status_local)
    ),

    (
        MaybeProgressStream = no
    ;
        MaybeProgressStream = yes(ProgressStream),
        ActualArity = arg_list_arity(Types),
        write_request(ProgressStream, ModuleInfo0, "Specializing",
            qualified(PredModuleName, Name0), PredFormArity, ActualArity,
            yes(SpecName), HOArgs, Context, !IO)
    ),

    pred_info_get_origin(PredInfo0, OrigOrigin),
    pred_info_get_typevarset(PredInfo0, TypeVarSet),
    pred_info_get_markers(PredInfo0, MarkerList),
    pred_info_get_goal_type(PredInfo0, GoalType),
    pred_info_get_class_context(PredInfo0, ClassContext),
    pred_info_get_var_name_remap(PredInfo0, VarNameRemap),

    InitTypes = cit_no_types(pred_form_arity(list.length(CallArgsTypes))),
    ItemNumbers = init_clause_item_numbers_comp_gen,
    clauses_info_init(pf_predicate, InitTypes, ItemNumbers, ClausesInfo0),
    varset.init(EmptyVarSet),
    vars_types_to_var_table(ModuleInfo0, EmptyVarSet, CallArgsTypes, VarTable),
    clauses_info_set_var_table(VarTable, ClausesInfo0, ClausesInfo),

    CalleePPId = proc(CalleePredId, CalleeProcId),
    Origin = origin_proc_transform(ProcTransform, OrigOrigin,
        CalleePredId, CalleeProcId),
    CurUserDecl = maybe.no,
    map.init(EmptyProofs),
    map.init(EmptyConstraintMap),
    pred_info_init(PredOrFunc, PredModuleName, SpecName, PredFormArity,
        Context, Origin, PredStatus, CurUserDecl, GoalType, MarkerList, Types,
        ArgTVarSet, ExistQVars, ClassContext, EmptyProofs, EmptyConstraintMap,
        ClausesInfo, VarNameRemap, NewPredInfo0),
    pred_info_set_typevarset(TypeVarSet, NewPredInfo0, NewPredInfo1),

    module_info_get_predicate_table(ModuleInfo0, PredTable0),
    predicate_table_insert(NewPredInfo1, NewPredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, ModuleInfo0, ModuleInfo1),

    !Info ^ hogi_module_info := ModuleInfo1,

    SpecSymName = qualified(PredModuleName, SpecName),
    NewPred = new_pred(proc(NewPredId, NewProcId), CalleePPId, CallerPPId,
        SpecSymName, HOArgs, CallArgsTypes, ExtraTypeInfoTVars, CallerTVarSet,
        TypeInfoLiveness, RequestKind),

    higher_order_add_new_pred(CalleePPId, NewPred, !Info),

    higher_order_create_new_proc(NewPred, ProcInfo0,
        NewPredInfo1, NewPredInfo, !Info),
    ModuleInfo2 = !.Info ^ hogi_module_info,
    module_info_set_pred_info(NewPredId, NewPredInfo, ModuleInfo2, ModuleInfo),
    !Info ^ hogi_module_info := ModuleInfo.

:- pred higher_order_add_new_pred(pred_proc_id::in, new_pred::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

higher_order_add_new_pred(CalleePPId, NewPred, !Info) :-
    NewPredMap0 = !.Info ^ hogi_new_pred_map,
    ( if map.search(NewPredMap0, CalleePPId, SpecVersions0) then
        set.insert(NewPred, SpecVersions0, SpecVersions),
        map.det_update(CalleePPId, SpecVersions, NewPredMap0, NewPredMap)
    else
        SpecVersions = set.make_singleton_set(NewPred),
        map.det_insert(CalleePPId, SpecVersions, NewPredMap0, NewPredMap)
    ),
    !Info ^ hogi_new_pred_map := NewPredMap.

%---------------------------------------------------------------------------%

:- pred ho_fixup_preds(list(pred_proc_id)::in, higher_order_global_info::in,
    higher_order_global_info::out) is det.

ho_fixup_preds(PredProcIds, !Info) :-
    Requests0 = !.Info ^ hogi_requests,
    list.foldl(ho_fixup_pred(need_not_recompute), PredProcIds, !Info),
    % Any additional requests must have already been denied.
    !Info ^ hogi_requests := Requests0.

:- pred ho_fixup_specialized_versions(list(new_pred)::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

ho_fixup_specialized_versions(NewPredList, !Info) :-
    NewPredProcIds = list.map(get_np_version_ppid, NewPredList),
    % Reprocess the goals to find any new specializations made
    % possible by the specializations performed in this pass.
    list.foldl(ho_fixup_pred(must_recompute), NewPredProcIds, !Info).

:- func get_np_version_ppid(new_pred) = pred_proc_id.

get_np_version_ppid(NewPred) = NewPred ^ np_version_ppid.

    % Fixup calls to specialized predicates.
    %
:- pred ho_fixup_pred(must_recompute::in, pred_proc_id::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

ho_fixup_pred(MustRecompute, proc(PredId, ProcId), !GlobalInfo) :-
    ho_traverse_proc(MustRecompute, PredId, ProcId, !GlobalInfo).

%---------------------------------------------------------------------------%

    % Build a proc_info for a specialized version.
    %
:- pred higher_order_create_new_proc(new_pred::in, proc_info::in,
    pred_info::in, pred_info::out,
    higher_order_global_info::in, higher_order_global_info::out) is det.

higher_order_create_new_proc(NewPred, !.NewProcInfo,
        !NewPredInfo, !GlobalInfo) :-
    ModuleInfo = !.GlobalInfo ^ hogi_module_info,

    NewPred = new_pred(NewPredProcId, OldPredProcId, CallerPredProcId, _Name,
        HOArgs0, CallArgsTypes0, ExtraTypeInfoTVars0, _, _, _),

    proc_info_get_headvars(!.NewProcInfo, HeadVars0),
    proc_info_get_argmodes(!.NewProcInfo, ArgModes0),
    pred_info_get_exist_quant_tvars(!.NewPredInfo, ExistQVars0),
    pred_info_get_typevarset(!.NewPredInfo, TypeVarSet0),
    pred_info_get_tvar_kind_map(!.NewPredInfo, KindMap0),
    pred_info_get_arg_types(!.NewPredInfo, OriginalArgTypes0),

    CallerPredProcId = proc(CallerPredId, _),
    module_info_pred_info(ModuleInfo, CallerPredId, CallerPredInfo),
    pred_info_get_typevarset(CallerPredInfo, CallerTypeVarSet),
    pred_info_get_univ_quant_tvars(CallerPredInfo, CallerHeadParams),

    % Specialize the types of the called procedure as for inlining.
    proc_info_get_var_table(!.NewProcInfo, VarTable0),
    tvarset_merge_renaming(CallerTypeVarSet, TypeVarSet0, TypeVarSet,
        TypeRenaming),
    apply_variable_renaming_to_tvar_kind_map(TypeRenaming, KindMap0, KindMap),
    apply_variable_renaming_to_var_table(TypeRenaming, VarTable0, VarTable1),
    apply_variable_renaming_to_type_list(TypeRenaming,
        OriginalArgTypes0, OriginalArgTypes1),

    % The real set of existentially quantified variables may be
    % smaller, but this is OK.
    apply_variable_renaming_to_tvar_list(TypeRenaming,
        ExistQVars0, ExistQVars1),

    assoc_list.keys_and_values(CallArgsTypes0, CallArgs, CallerArgTypes0),
    compute_caller_callee_type_substitution(OriginalArgTypes1, CallerArgTypes0,
        CallerHeadParams, ExistQVars1, TypeSubn),

    apply_rec_subst_to_tvar_list(KindMap, TypeSubn, ExistQVars1, ExistQTypes),
    list.filter_map(
        ( pred(ExistQType::in, ExistQVar::out) is semidet :-
            ExistQType = type_variable(ExistQVar, _)
        ), ExistQTypes, ExistQVars),

    apply_rec_subst_to_var_table(is_type_a_dummy(ModuleInfo), TypeSubn,
        VarTable1, VarTable2),
    apply_rec_subst_to_type_list(TypeSubn,
        OriginalArgTypes1, OriginalArgTypes),
    proc_info_set_var_table(VarTable2, !NewProcInfo),

    % XXX kind inference: we assume vars have kind `star'.
    prog_type.var_list_to_type_list(map.init, ExtraTypeInfoTVars0,
        ExtraTypeInfoTVarTypes0),
    ( if
        ( map.is_empty(TypeSubn)
        ; ExistQVars = []
        )
    then
        HOArgs = HOArgs0,
        ExtraTypeInfoTVarTypes = ExtraTypeInfoTVarTypes0,
        ExtraTypeInfoTVars = ExtraTypeInfoTVars0
    else
        % If there are existentially quantified variables in the callee
        % we may need to bind type variables in the caller.
        list.map(substitute_higher_order_arg(TypeSubn), HOArgs0, HOArgs),

        apply_rec_subst_to_type_list(TypeSubn, ExtraTypeInfoTVarTypes0,
            ExtraTypeInfoTVarTypes),
        % The substitution should never bind any of the type variables
        % for which extra type-infos are needed, otherwise it would not be
        % necessary to add them.
        ( if
            prog_type.type_list_to_var_list(ExtraTypeInfoTVarTypes,
                ExtraTypeInfoTVarsPrim)
        then
            ExtraTypeInfoTVars = ExtraTypeInfoTVarsPrim
        else
            unexpected($pred, "type var got bound")
        )
    ),

    % Add in the extra typeinfo vars.
    ExtraTypeInfoTypes =
        list.map(build_type_info_type, ExtraTypeInfoTVarTypes),
    proc_info_create_vars_from_types(ModuleInfo, ExtraTypeInfoTypes,
        ExtraTypeInfoVars, !NewProcInfo),

    % Add any extra type-infos or typeclass-infos we have added
    % to the typeinfo_varmap and typeclass_info_varmap.
    proc_info_get_rtti_varmaps(!.NewProcInfo, RttiVarMaps0),

    % The variable renaming does not rename variables in the callee.
    map.init(EmptyVarRenaming),

    % XXX1 See a XXX2 comment below about why this call is here.
    apply_substitutions_to_rtti_varmaps(TypeRenaming, TypeSubn,
        EmptyVarRenaming, RttiVarMaps0, RttiVarMaps1),

    % Add entries in the typeinfo_varmap for the extra type-infos.
    list.foldl_corresponding(rtti_det_insert_type_info_type,
        ExtraTypeInfoVars, ExtraTypeInfoTVarTypes,
        RttiVarMaps1, RttiVarMaps2),
    SetTypeInfoVarLocn =
        ( pred(TVar::in, Var::in, !.R::in, !:R::out) is det :-
            Locn = type_info(Var),
            rtti_set_type_info_locn(TVar, Locn, !R)
        ),
    list.foldl_corresponding(SetTypeInfoVarLocn,
        ExtraTypeInfoTVars, ExtraTypeInfoVars, RttiVarMaps2, RttiVarMaps),

    proc_info_set_rtti_varmaps(RttiVarMaps, !NewProcInfo),

    map.from_corresponding_lists(CallArgs, HeadVars0, VarRenaming0),

    % Construct the constant input closures within the goal
    % for the called procedure.
    map.init(KnownVarMap0),
    construct_higher_order_terms(ModuleInfo, HeadVars0, ExtraHeadVars,
        ArgModes0, ExtraArgModes, HOArgs, !NewProcInfo,
        VarRenaming0, _, KnownVarMap0, KnownVarMap, ConstGoals),

    % XXX2 The substitutions used to be applied to the typeclass_info_varmap
    % here rather than at the XXX above. Any new entries added in the code
    % between these two points should therefore be transformed as well?
    % The new entries come from HOArgs, which have already had TypeSubn
    % applied, but not TypeRenaming. Perhaps this is enough?

    % Record extra information about this version.
    VersionInfoMap0 = !.GlobalInfo ^ hogi_version_info,
    ArgsDepth = higher_order_args_depth(HOArgs),

    ( if map.search(VersionInfoMap0, OldPredProcId, OldProcVersionInfo) then
        OldProcVersionInfo = version_info(OrigPredProcId, _, _, _)
    else
        OrigPredProcId = OldPredProcId
    ),

    ( if map.search(VersionInfoMap0, CallerPredProcId, CallerVersionInfo) then
        CallerVersionInfo = version_info(_, _, _, CallerParentVersions)
    else
        CallerParentVersions = []
    ),
    ParentVersions = [parent_version_info(OrigPredProcId, ArgsDepth)
        | CallerParentVersions],

    VersionInfo = version_info(OrigPredProcId, ArgsDepth,
        KnownVarMap, ParentVersions),
    map.det_insert(NewPredProcId, VersionInfo,
        VersionInfoMap0, VersionInfoMap),
    !GlobalInfo ^ hogi_version_info := VersionInfoMap,

    % Fix up the argument vars, types and modes.
    in_mode(InMode),
    list.length(ExtraTypeInfoVars, NumTypeInfos),
    list.duplicate(NumTypeInfos, InMode, ExtraTypeInfoModes),

    remove_const_higher_order_args(1, HeadVars0, HOArgs, HeadVars1),
    remove_const_higher_order_args(1, ArgModes0, HOArgs, ArgModes1),
    list.condense([ExtraTypeInfoVars, ExtraHeadVars, HeadVars1], HeadVars),
    list.condense([ExtraTypeInfoModes, ExtraArgModes, ArgModes1], ArgModes),
    proc_info_set_headvars(HeadVars, !NewProcInfo),
    proc_info_set_argmodes(ArgModes, !NewProcInfo),

    proc_info_get_goal(!.NewProcInfo, Goal6),
    Goal6 = hlds_goal(_, GoalInfo6),
    goal_to_conj_list(Goal6, GoalList6),
    conj_list_to_goal(ConstGoals ++ GoalList6, GoalInfo6, Goal),
    proc_info_set_goal(Goal, !NewProcInfo),

    % Remove any imported structure sharing and reuse information for the
    % original procedure as they won't be (directly) applicable.
    proc_info_reset_imported_structure_sharing(!NewProcInfo),
    proc_info_reset_imported_structure_reuse(!NewProcInfo),

    proc_info_get_var_table(!.NewProcInfo, VarTable7),
    lookup_var_types(VarTable7, ExtraHeadVars, ExtraHeadVarTypes0),
    remove_const_higher_order_args(1, OriginalArgTypes,
        HOArgs, ModifiedOriginalArgTypes),
    list.condense([ExtraTypeInfoTypes, ExtraHeadVarTypes0,
        ModifiedOriginalArgTypes], ArgTypes),
    pred_info_set_arg_types(TypeVarSet, ExistQVars, ArgTypes, !NewPredInfo),
    pred_info_set_typevarset(TypeVarSet, !NewPredInfo),

    % The types of the headvars in the var_table map in the proc_info may be
    % more specific than the argument types returned by pred_info_argtypes
    % if the procedure body binds some existentially quantified type variables.
    % The types of the extra arguments added by construct_higher_order_terms
    % use the substitution computed based on the result
    % pred_info_get_arg_types. We may need to apply a substitution
    % to the types of the new variables in the var_table in the proc_info.
    %
    % XXX We should apply this substitution to the variable types in any
    % callers of this predicate, which may introduce other opportunities
    % for specialization.

    (
        ExistQVars = []
    ;
        ExistQVars = [_ | _],
        lookup_var_types(VarTable7, HeadVars0, OriginalHeadTypes),
        type_list_subsumes_det(OriginalArgTypes, OriginalHeadTypes,
            ExistentialSubn),
        apply_rec_subst_to_type_list(ExistentialSubn, ExtraHeadVarTypes0,
            ExtraHeadVarTypes),
        assoc_list.from_corresponding_lists(ExtraHeadVars,
            ExtraHeadVarTypes, ExtraHeadVarsAndTypes),
        list.foldl(update_var_types(ModuleInfo), ExtraHeadVarsAndTypes,
            VarTable7, VarTable8),
        proc_info_set_var_table(VarTable8, !NewProcInfo)
    ),

    % Find the new class context.
    proc_info_get_headvars(!.NewProcInfo, ArgVars),
    proc_info_get_rtti_varmaps(!.NewProcInfo, NewRttiVarMaps),
    list.map(rtti_varmaps_var_info(NewRttiVarMaps), ArgVars, ArgVarInfos),
    find_class_context(ModuleInfo, ArgVarInfos, ArgModes, [], [],
        ClassContext),
    pred_info_set_class_context(ClassContext, !NewPredInfo),

    NewPredProcId = proc(_, NewProcId),
    NewProcs = map.singleton(NewProcId, !.NewProcInfo),
    pred_info_set_proc_table(NewProcs, !NewPredInfo).

:- pred update_var_types(module_info::in, pair(prog_var, mer_type)::in,
    var_table::in, var_table::out) is det.

update_var_types(ModuleInfo, VarAndType, !VarTable) :-
    VarAndType = Var - Type,
    IsDummy = is_type_a_dummy(ModuleInfo, Type),
    lookup_var_entry(!.VarTable, Var, Entry0),
    Entry0 = vte(Name, _, _),
    Entry = vte(Name, Type, IsDummy),
    update_var_entry(Var, Entry, !VarTable).

%---------------------%

    % Take an original list of headvars and arg_modes and return these
    % with curried arguments added. The old higher-order arguments are
    % left in. They may be needed in calls which could not be
    % specialised. If not, unused_args.m can clean them up.
    %
    % Build the initial known_var_map which records higher-order and
    % type_info constants for a call to ho_traverse_proc_body.
    %
    % Build a var-var renaming from the requesting call's arguments to
    % the headvars of the specialized version.
    %
    % This predicate is recursively applied to all curried higher order
    % arguments of higher order arguments.
    %
    % Update higher_order_arg_order_version if the order or number of
    % the arguments for specialized versions changes.
    %
:- pred construct_higher_order_terms(module_info::in, list(prog_var)::in,
    list(prog_var)::out, list(mer_mode)::in, list(mer_mode)::out,
    list(higher_order_arg)::in, proc_info::in, proc_info::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out,
    known_var_map::in, known_var_map::out, list(hlds_goal)::out) is det.

construct_higher_order_terms(_, _, [], _, [], [], !ProcInfo, !Renaming,
        !KnownVarMap, []).
construct_higher_order_terms(ModuleInfo, HeadVars0, NewHeadVars, ArgModes0,
        NewArgModes, [HOArg | HOArgs], !ProcInfo, !Renaming,
        !KnownVarMap, ConstGoals) :-
    HOArg = higher_order_arg(ConsId, Index, NumArgs, CurriedArgs,
        CurriedArgTypes, CurriedArgRttiInfo, CurriedHOArgs, IsConst),

    list.det_index1(HeadVars0, Index, LVar),
    ( if ConsId = closure_cons(ShroudedPredProcId, _) then
        % Add the curried arguments to the procedure's argument list.
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
            CalledPredInfo, CalledProcInfo),
        PredOrFunc = pred_info_is_pred_or_func(CalledPredInfo),
        proc_info_get_argmodes(CalledProcInfo, CalledArgModes),
        list.det_split_list(NumArgs, CalledArgModes,
            CurriedArgModes1, NonCurriedArgModes),
        proc_info_interface_determinism(CalledProcInfo, ProcDetism),
        GroundInstInfo = higher_order(pred_inst_info(PredOrFunc,
            NonCurriedArgModes, arg_reg_types_unset, ProcDetism))
    else
        in_mode(InMode),
        GroundInstInfo = none_or_default_func,
        list.duplicate(NumArgs, InMode, CurriedArgModes1)
    ),

    proc_info_create_vars_from_types(ModuleInfo, CurriedArgTypes,
        CurriedHeadVars1, !ProcInfo),

    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
    list.foldl_corresponding(add_rtti_info, CurriedHeadVars1,
        CurriedArgRttiInfo, RttiVarMaps0, RttiVarMaps),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),

    (
        IsConst = no,
        % Make ho_traverse_proc_body pretend that the input higher-order
        % argument is built using the new arguments as its curried arguments.
        map.det_insert(LVar, known_const(ConsId, CurriedHeadVars1),
            !KnownVarMap)
    ;
        IsConst = yes
    ),

    assoc_list.from_corresponding_lists(CurriedArgs, CurriedHeadVars1,
        CurriedRenaming),
    list.foldl(
        ( pred(VarPair::in, !.Map::in, !:Map::out) is det :-
            VarPair = Var1 - Var2,
            map.set(Var1, Var2, !Map)
        ), CurriedRenaming, !Renaming),

    % Recursively construct the curried higher-order arguments.
    construct_higher_order_terms(ModuleInfo, CurriedHeadVars1,
        ExtraCurriedHeadVars, CurriedArgModes1, ExtraCurriedArgModes,
        CurriedHOArgs, !ProcInfo, !Renaming, !KnownVarMap,
        CurriedConstGoals),

    % Construct the rest of the higher-order arguments.
    construct_higher_order_terms(ModuleInfo, HeadVars0, NewHeadVars1,
        ArgModes0, NewArgModes1, HOArgs, !ProcInfo,
        !Renaming, !KnownVarMap, ConstGoals1),

    (
        IsConst = yes,
        % Build the constant inside the specialized version, so that
        % other constants which include it will be recognized as constant.
        ArgModes = list.map(mode_both_sides_to_unify_mode(ModuleInfo),
            CurriedArgModes1),
        set_of_var.list_to_set(CurriedHeadVars1, ConstNonLocals),
        ConstInst = ground(shared, GroundInstInfo),
        ConstInstMapDelta = instmap_delta_from_assoc_list([LVar - ConstInst]),
        goal_info_init(ConstNonLocals, ConstInstMapDelta, detism_det,
            purity_pure, ConstGoalInfo),
        RHS = rhs_functor(ConsId, is_not_exist_constr, CurriedHeadVars1),
        UnifyMode = unify_modes_li_lf_ri_rf(free, ConstInst,
            ConstInst, ConstInst),
        ConstGoalExpr = unify(LVar, RHS, UnifyMode,
            construct(LVar, ConsId, CurriedHeadVars1, ArgModes,
                construct_dynamically, cell_is_unique, no_construct_sub_info),
            unify_context(umc_explicit, [])),
        ConstGoal = hlds_goal(ConstGoalExpr, ConstGoalInfo),
        ConstGoals0 = CurriedConstGoals ++ [ConstGoal]
    ;
        IsConst = no,
        ConstGoals0 = CurriedConstGoals
    ),

    % Fix up the argument lists.
    remove_const_higher_order_args(1, CurriedHeadVars1,
        CurriedHOArgs, CurriedHeadVars),
    remove_const_higher_order_args(1, CurriedArgModes1,
        CurriedHOArgs, CurriedArgModes),
    NewHeadVars = CurriedHeadVars ++ ExtraCurriedHeadVars ++ NewHeadVars1,
    NewArgModes = CurriedArgModes ++ ExtraCurriedArgModes ++ NewArgModes1,
    ConstGoals = ConstGoals0 ++ ConstGoals1.

%---------------------%

    % Add any new type-infos or typeclass-infos to the rtti_varmaps.
    %
:- pred add_rtti_info(prog_var::in, rtti_var_info::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

add_rtti_info(Var, VarInfo, !RttiVarMaps) :-
    (
        VarInfo = type_info_var(TypeInfoType),
        rtti_det_insert_type_info_type(Var, TypeInfoType, !RttiVarMaps),
        ( if TypeInfoType = type_variable(TVar, _) then
            maybe_set_typeinfo_locn(TVar, type_info(Var), !RttiVarMaps)
        else
            true
        )
    ;
        VarInfo = typeclass_info_var(Constraint),
        ( if rtti_search_typeclass_info_var(!.RttiVarMaps, Constraint, _) then
            true
        else
            rtti_det_insert_typeclass_info_var(Constraint, Var, !RttiVarMaps),
            Constraint = constraint(_ClassName, ConstraintArgTypes),
            list.foldl2(update_type_info_locn(Var), ConstraintArgTypes, 1, _,
                !RttiVarMaps)
        )
    ;
        VarInfo = non_rtti_var
    ).

:- pred update_type_info_locn(prog_var::in, mer_type::in, int::in, int::out,
    rtti_varmaps::in, rtti_varmaps::out) is det.

update_type_info_locn(Var, ConstraintType, Index, Index + 1, !RttiVarMaps) :-
    (
        ConstraintType = type_variable(ConstraintTVar, _),
        maybe_set_typeinfo_locn(ConstraintTVar,
            typeclass_info(Var, Index), !RttiVarMaps)
    ;
        ( ConstraintType = defined_type(_, _, _)
        ; ConstraintType = builtin_type(_)
        ; ConstraintType = tuple_type(_, _)
        ; ConstraintType = higher_order_type(_, _, _, _, _)
        ; ConstraintType = apply_n_type(_, _, _)
        ; ConstraintType = kinded_type(_, _)
        )
    ).

:- pred maybe_set_typeinfo_locn(tvar::in, type_info_locn::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

maybe_set_typeinfo_locn(TVar, Locn, !RttiVarMaps) :-
    ( if rtti_search_type_info_locn(!.RttiVarMaps, TVar, _) then
        true
    else
        rtti_det_insert_type_info_locn(TVar, Locn, !RttiVarMaps)
    ).

:- func higher_order_arg_order_version = int.

higher_order_arg_order_version = 1.

%---------------------%

    % Substitute the types in a higher_order_arg.
    %
:- pred substitute_higher_order_arg(tsubst::in, higher_order_arg::in,
    higher_order_arg::out) is det.

substitute_higher_order_arg(Subn, !HOArg) :-
    CurriedArgTypes0 = !.HOArg ^ hoa_curry_type_in_caller,
    CurriedRttiTypes0 = !.HOArg ^ hoa_curry_rtti_type,
    CurriedHOArgs0 = !.HOArg ^ hoa_known_curry_args,
    apply_rec_subst_to_type_list(Subn, CurriedArgTypes0, CurriedArgTypes),
    list.map(substitute_rtti_var_info(Subn), CurriedRttiTypes0,
        CurriedRttiTypes),
    list.map(substitute_higher_order_arg(Subn), CurriedHOArgs0, CurriedHOArgs),
    !HOArg ^ hoa_curry_type_in_caller := CurriedArgTypes,
    !HOArg ^ hoa_curry_rtti_type := CurriedRttiTypes,
    !HOArg ^ hoa_known_curry_args := CurriedHOArgs.

:- pred substitute_rtti_var_info(tsubst::in,
    rtti_var_info::in, rtti_var_info::out) is det.

substitute_rtti_var_info(Subn, RttiVarInfo0, RttiVarInfo) :-
    (
        RttiVarInfo0 = type_info_var(Type0),
        apply_rec_subst_to_type(Subn, Type0, Type),
        RttiVarInfo = type_info_var(Type)
    ;
        RttiVarInfo0 = typeclass_info_var(Constraint0),
        apply_rec_subst_to_prog_constraint(Subn, Constraint0, Constraint),
        RttiVarInfo = typeclass_info_var(Constraint)
    ;
        RttiVarInfo0 = non_rtti_var,
        RttiVarInfo = non_rtti_var
    ).

%---------------------------------------------------------------------------%

    % Collect the list of prog_constraints from the list of argument types.
    % For universal constraints the typeclass_info is input, while
    % for existential constraints it is output.
    %
:- pred find_class_context(module_info::in, list(rtti_var_info)::in,
    list(mer_mode)::in, list(prog_constraint)::in, list(prog_constraint)::in,
    univ_exist_constraints::out) is det.

find_class_context(_, [], [], !.RevUniv, !.RevExist, Constraints) :-
    list.reverse(!.RevUniv, Univ),
    list.reverse(!.RevExist, Exist),
    Constraints = univ_exist_constraints(Univ, Exist).
find_class_context(_, [], [_ | _], _, _, _) :-
    unexpected($pred, "mismatched list length").
find_class_context(_, [_ | _], [], _, _, _) :-
    unexpected($pred, "mismatched list length").
find_class_context(ModuleInfo, [VarInfo | VarInfos], [Mode | Modes],
        !.RevUniv, !.RevExist, Constraints) :-
    (
        VarInfo = typeclass_info_var(Constraint),
        ( if mode_is_input(ModuleInfo, Mode) then
            maybe_add_constraint(Constraint, !RevUniv)
        else
            maybe_add_constraint(Constraint, !RevExist)
        )
    ;
        VarInfo = type_info_var(_)
    ;
        VarInfo = non_rtti_var
    ),
    find_class_context(ModuleInfo, VarInfos, Modes, !.RevUniv, !.RevExist,
        Constraints).

:- pred maybe_add_constraint(prog_constraint::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

maybe_add_constraint(Constraint, !RevConstraints) :-
    % Don't create duplicates.
    ( if list.member(Constraint, !.RevConstraints) then
        true
    else
        !:RevConstraints = [Constraint | !.RevConstraints]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Progress messages.
%

:- pred write_request(io.text_output_stream::in, module_info::in,
    string::in, sym_name::in, pred_form_arity::in, pred_form_arity::in,
    maybe(string)::in, list(higher_order_arg)::in, prog_context::in,
    io::di, io::uo) is det.

write_request(OutputStream, ModuleInfo, Msg,
        SymName, PredArity, ActualArity, MaybeNewName, HOArgs, Context, !IO) :-
    OldName = sym_name_to_string(SymName),
    PredArity = pred_form_arity(PredArityInt),
    ActualArity = pred_form_arity(ActualArityInt),
    io.write_string(OutputStream, "% ", !IO),
    parse_tree_out_misc.write_context(OutputStream, Context, !IO),
    io.format(OutputStream, "%s `%s'/%d",
        [s(Msg), s(OldName), i(PredArityInt)], !IO),
    (
        MaybeNewName = yes(NewName),
        io.format(OutputStream, " into %s", [s(NewName)], !IO)
    ;
        MaybeNewName = no
    ),
    io.write_string(OutputStream, " with higher-order arguments:\n", !IO),
    NumToDrop = ActualArityInt - PredArityInt,
    output_higher_order_args(OutputStream, ModuleInfo, NumToDrop, 0u,
        HOArgs, !IO).

:- pred output_higher_order_args(io.text_output_stream::in, module_info::in,
    int::in, indent::in, list(higher_order_arg)::in, io::di, io::uo) is det.

output_higher_order_args(_, _, _, _, [], !IO).
output_higher_order_args(OutputStream, ModuleInfo, NumToDrop, Indent,
        [HOArg | HOArgs], !IO) :-
    HOArg = higher_order_arg(ConsId, ArgNo, NumArgs, _, _, _,
        CurriedHOArgs, IsConst),
    Indent1Str = indent2_string(Indent + 1u),
    io.format(OutputStream, "%% %s", [s(Indent1Str)], !IO),
    (
        IsConst = yes,
        io.write_string(OutputStream, "const ", !IO)
    ;
        IsConst = no
    ),
    ( if ConsId = closure_cons(ShroudedPredProcId, _) then
        proc(PredId, _) = unshroud_pred_proc_id(ShroudedPredProcId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        Name = pred_info_name(PredInfo),
        pred_info_get_orig_arity(PredInfo, pred_form_arity(PredFormArityInt)),
        % Adjust message for type_infos.
        DeclaredArgNo = ArgNo - NumToDrop,
        io.format(OutputStream, "HeadVar__%d = `%s'/%d",
            [i(DeclaredArgNo), s(Name), i(PredFormArityInt)], !IO)
    else if ConsId = type_ctor_info_const(TypeModule, TypeName, TypeArity) then
        io.format(OutputStream, "type_ctor_info for `%s'/%d",
            [s(escaped_sym_name_to_string(qualified(TypeModule, TypeName))),
            i(TypeArity)], !IO)
    else if ConsId = base_typeclass_info_const(_, ClassId, _, _) then
        ClassId = class_id(ClassSymName, ClassArity),
        io.format(OutputStream, "base_typeclass_info for `%s'/%d",
            [s(escaped_sym_name_to_string(ClassSymName)), i(ClassArity)], !IO)
    else
        % XXX output the type.
        io.write_string(OutputStream, "type_info/typeclass_info", !IO)
    ),
    io.format(OutputStream, " with %d curried arguments", [i(NumArgs)], !IO),
    (
        CurriedHOArgs = [],
        io.nl(OutputStream, !IO)
    ;
        CurriedHOArgs = [_ | _],
        io.write_string(OutputStream, ":\n", !IO),
        output_higher_order_args(OutputStream, ModuleInfo, 0, Indent + 1u,
            CurriedHOArgs, !IO)
    ),
    output_higher_order_args(OutputStream, ModuleInfo, NumToDrop, Indent,
        HOArgs, !IO).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.higher_order.make_specialized_preds.
%---------------------------------------------------------------------------%
