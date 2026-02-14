%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.add_foreign_proc.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.make_hlds_types.
:- import_module hlds.status.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.

:- pred add_foreign_procs(io.text_output_stream::in,
    ims_list(item_foreign_proc_info)::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_foreign_proc(io.text_output_stream::in, item_mercury_status::in,
    pred_status::in, item_foreign_proc_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.vartypes.

:- import_module bag.
:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module uint.
:- import_module varset.

%---------------------------------------------------------------------------%

add_foreign_procs(_, [], !ModuleInfo, !Specs).
add_foreign_procs(ProgressStream, [ImsSubList | ImsSubLists],
        !ModuleInfo, !Specs) :-
    ImsSubList = ims_sub_list(ItemMercuryStatus, PragmaFPInfos),
    item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
    list.foldl2(
        add_foreign_proc(ProgressStream, ItemMercuryStatus, PredStatus),
        PragmaFPInfos, !ModuleInfo, !Specs),
    add_foreign_procs(ProgressStream, ImsSubLists, !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%

add_foreign_proc(ProgressStream, ItemMercuryStatus, PredStatus, FPInfo,
        !ModuleInfo, !Specs) :-
    FPInfo = item_foreign_proc_info(Attributes0, PredSymName, PredOrFunc,
        PragmaVars, ProgVarSet, _InstVarset, PragmaImpl, Context, SeqNum),
    PredFormArity = arg_list_arity(PragmaVars),
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, PredSymName, PredFormArity),

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        trace [io(!IO)] (
            IdStr = pf_sym_name_pred_form_arity_to_string(PFSymNameArity),
            io.format(ProgressStream,
                "%% Processing `:- pragma foreign_proc' for %s...\n",
                [s(IdStr)], !IO)
        )
    ;
        VeryVerbose = no
    ),

    % Lookup the pred_info for this pred, add the pragma to the proc_info
    % in the proc_table in the pred_info, and save the pred_info.
    some [!PredInfo] (
        add_implicit_pred_decl_if_needed(PFSymNameArity, PredStatus, Context,
            PredId, !ModuleInfo, !Specs),
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),

        % status_opt_imported preds are initially tagged as status_imported
        % and are tagged as status_opt_imported only if/when we see a clause
        % (including a `foreign_proc' clause) for them.
        ( if PredStatus = pred_status(status_opt_imported) then
            pred_info_set_status(pred_status(status_opt_imported), !PredInfo)
        else
            true
        ),

        record_foreign_proc_seq_num(SeqNum, Context, PredId,
            !PredInfo, !ModuleInfo),
        decide_actual_thread_safety(Globals, Attributes0, Attributes),
        compute_intended_proc_id(!.ModuleInfo, !.PredInfo,
            PFSymNameArity, PragmaVars, Attributes, Context, MaybeProcId),
        check_foreign_proc(!.ModuleInfo, !.PredInfo, PFSymNameArity,
            MaybeProcId, ProgVarSet, PragmaVars, Attributes, PragmaImpl,
            Context, AllowedToAdd, !.Specs, SpecsAfterCheck),
        ThisModule = pred_status_defined_in_this_module(PredStatus),
        (
            ThisModule = yes,
            !:Specs = SpecsAfterCheck
        ;
            ThisModule = no
        ),

        ( if
            AllowedToAdd = allowed_to_add_foreign_proc,
            MaybeProcId = ok(ProcId)
        then
            is_foreign_proc_for_this_backend(Globals, Attributes,
                ForThisBackend),
            (
                ForThisBackend = not_for_this_backend(RejectCause),
                handle_wrong_backend_foreign_proc(ItemMercuryStatus,
                    PredId, !.PredInfo, RejectCause, Context,
                    !ModuleInfo, !Specs)
            ;
                ForThisBackend = for_this_backend,
                add_nonimported_foreign_proc(PredId, !.PredInfo, ProcId,
                    PFSymNameArity, Attributes, PragmaVars,
                    PragmaImpl, Context, !ModuleInfo, !Specs)
            )
        else
            true
        )
    ).

%---------------------%

    % Lookup the pred declaration in the predicate table.
    % If it is not there, generate an error message, and insert
    % a dummy declaration for the predicate.
    %
:- pred add_implicit_pred_decl_if_needed(pf_sym_name_arity::in,
    pred_status::in, prog_context::in, pred_id::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_implicit_pred_decl_if_needed(PFSymNameArity, PredStatus, Context, PredId,
        !ModuleInfo, !Specs) :-
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, PredSymName, PredFormArity),
    det_sym_name_get_module_name_and_name(PredSymName,
        PredModuleName, PredName),
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_search_pf_fqm_n_a(PredTable0, PredOrFunc,
        PredModuleName, PredName, PredFormArity, MaybePredId),
    (
        MaybePredId = yes(PredId)
    ;
        MaybePredId = no,
        user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
        Origin = origin_user(user_made_pred(PredOrFunc,
            PredSymName, UserArity)),
        add_implicit_pred_decl_report_error(PredOrFunc, PredModuleName,
            PredName, PredFormArity, PredStatus, is_not_a_class_method,
            Context, Origin,
            [pragma_decl("foreign_proc"), words("declaration")],
            PredId, !ModuleInfo, !Specs)
    ).

    % Record the existence of this "clause".
    %
:- pred record_foreign_proc_seq_num(item_seq_num::in, prog_context::in,
    pred_id::in, pred_info::in, pred_info::out,
    module_info::in, module_info::out) is det.

record_foreign_proc_seq_num(SeqNum, Context, PredId, !PredInfo, !ModuleInfo) :-
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    ItemNumbers0 = ClausesInfo0 ^ cli_item_numbers,
    add_clause_item_number(SeqNum, Context, item_is_foreign_proc,
        ItemNumbers0, ItemNumbers),
    ClausesInfo = ClausesInfo0 ^ cli_item_numbers := ItemNumbers,
    pred_info_set_clauses_info(ClausesInfo, !PredInfo),
    module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo).

    % Replace any maybe_thread_safe foreign_proc attributes with
    % the actual thread safety attributes that we get from the
    % `--maybe-thread-safe' option.
    %
:- pred decide_actual_thread_safety(globals::in,
    foreign_proc_attributes::in, foreign_proc_attributes::out) is det.

decide_actual_thread_safety(Globals, Attributes0, Attributes) :-
    globals.get_maybe_thread_safe(Globals, MaybeThreadSafe),
    ThreadSafe = get_thread_safe(Attributes0),
    (
        ThreadSafe = proc_maybe_thread_safe,
        (
            MaybeThreadSafe = yes,
            set_thread_safe(proc_thread_safe, Attributes0, Attributes)
        ;
            MaybeThreadSafe = no,
            set_thread_safe(proc_not_thread_safe, Attributes0, Attributes)
        )
    ;
        ( ThreadSafe = proc_thread_safe
        ; ThreadSafe = proc_not_thread_safe
        ),
        Attributes = Attributes0
    ).

%---------------------%

:- type maybe_for_this_backend
    --->    for_this_backend
    ;       not_for_this_backend(wrong_backend_cause).

:- type wrong_backend_cause
    --->    wrong_lang(foreign_language, list(foreign_language))
    ;       right_lang_wrong_backend.

:- pred is_foreign_proc_for_this_backend(globals::in,
    foreign_proc_attributes::in, maybe_for_this_backend::out) is det.

is_foreign_proc_for_this_backend(Globals, Attributes, ForThisBackend) :-
    globals.get_backend_foreign_languages(Globals, BackendForeignLangs),
    PragmaForeignLanguage = get_foreign_language(Attributes),
    ( if list.member(PragmaForeignLanguage, BackendForeignLangs) then
        CurrentBackend = lookup_current_backend(Globals),
        MaybeForSpecificBackend = get_for_specific_backend(Attributes),
        ( if
            ( MaybeForSpecificBackend = no
            ; MaybeForSpecificBackend = yes(CurrentBackend)
            )
        then
            ForThisBackend = for_this_backend
        else
            RejectCause = right_lang_wrong_backend,
            ForThisBackend = not_for_this_backend(RejectCause)
        )
    else
        RejectCause = wrong_lang(PragmaForeignLanguage, BackendForeignLangs),
        ForThisBackend = not_for_this_backend(RejectCause)
    ).

%---------------------%

    % Add the foreign_proc to the list of "clauses" for this predicate,
    % if the procedure it is for actually exists.
    %
:- pred compute_intended_proc_id(module_info::in, pred_info::in,
    pf_sym_name_arity::in, list(pragma_var)::in, foreign_proc_attributes::in,
    prog_context::in, maybe_error(proc_id, error_spec)::out) is det.

compute_intended_proc_id(ModuleInfo, PredInfo, PFSymNameArity,
        PragmaVars, Attributes, Context, MaybeProcId) :-
    pred_info_get_proc_table(PredInfo, Procs),
    map.to_assoc_list(Procs, ExistingProcs),
    pragma_get_modes(PragmaVars, Modes),
    ( if
        % The inst variables for the foreign_proc declaration and
        % predmode declarations are from different varsets. We cannot just
        % unify the argument modes directly, because the representation
        % of the inst variables may be different. Instead, we need to allow
        % for a renaming between the inst variables in the argument modes
        % of the foreign_proc and those of the predmode declaration.
        %
        % XXX We should probably also check that each pair in the renaming
        % has the same name.
        get_procedure_matching_declmodes_with_renaming(ModuleInfo,
            ExistingProcs, Modes, ProcId)
    then
        MaybeProcId = ok(ProcId)
    else
        Spec = report_fproc_for_undeclared_mode(PFSymNameArity,
            Attributes, Context),
        MaybeProcId = error(Spec)
    ).

    % Add the foreign_proc to the list of "clauses" for this predicate,
    % if the procedure it is for actually exists.
    %
:- pred add_nonimported_foreign_proc(pred_id::in, pred_info::in, proc_id::in,
    pf_sym_name_arity::in, foreign_proc_attributes::in, list(pragma_var)::in,
    pragma_foreign_proc_impl::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_nonimported_foreign_proc(PredId, !.PredInfo, ProcId, PFSymNameArity,
        Attributes, PragmaVars, PragmaImpl, Context, !ModuleInfo, !Specs) :-
    PFSymNameArity =
        pf_sym_name_arity(PredOrFunc, PredSymName, _PredFormArity),
    det_sym_name_get_module_name_and_name(PredSymName,
        PredModuleName, PredName),
    pred_info_get_arg_types(!.PredInfo, ArgTypes),
    pred_info_get_purity(!.PredInfo, Purity),
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo1),
    clauses_info_add_foreign_proc(!.ModuleInfo, PredOrFunc,
        PredModuleName, PredName, PredId, ProcId, PragmaVars,
        ArgTypes, Purity, Attributes, Context, PragmaImpl,
        ClausesInfo1, ClausesInfo, !Specs),
    pred_info_set_clauses_info(ClausesInfo, !PredInfo),
    pred_info_update_goal_type(np_goal_type_foreign, !PredInfo),
    module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo).

%---------------------%

:- pred handle_wrong_backend_foreign_proc(item_mercury_status::in,
    pred_id::in, pred_info::in, wrong_backend_cause::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

handle_wrong_backend_foreign_proc(ItemMercuryStatus, PredId, !.PredInfo,
        WrongBackendCause, Context, !ModuleInfo, !Specs) :-
    % This foreign_proc is for the wrong language or the wrong backend.
    % This is ok when coming from a source file, which is allowed
    % to define a predicate by different foreign_procs for different
    % target languages and/or backends. This is not ok when coming
    % from a .opt file, which (if everything is set up properly)
    % we should be reading only if it has been generated for the
    % current grade.
    ( if ItemMercuryStatus =
        item_defined_in_other_module(item_import_opt_int)
    then
        report_bad_foreign_proc_in_dot_opt_file(WrongBackendCause, Context,
            !Specs)
    else
        true
    ),
    % XXX The next two calls seem redundant, and in most cases, they are.
    % However, deleting them results in the failure of the
    % hard_coded/foreign_type_2 test case, with this message:
    % In clause for function `x'/1:
    %   error: undefined symbol `foreign_type2.x'/1.
    %   There are `:- pragma foreign_type' declarations for type
    %   `foreign_type2.coord'/1, so it is treated as an abstract
    %   type in all predicates and functions which are not
    %   implemented for those foreign types.
    % The reason for this is the fact that convert_cons_defn
    % in typecheck.m checks whether the goal type is
    % goal_not_for_promise(np_goal_type_clause_and_foreign),
    % and it is these two calls that set up the "_and_foreign" part of that.
    %
    % XXX Originally, we executed these two calls only in the wrong_lang case,
    % and this was enough for fixing the foreign_type_2 test case. Whether
    % we need them in the right_lang_wrong_backend case is a guess,
    % which would need a dedicated test case to test. However, since
    % backend-specific external pragmas are intended only for implementors,
    % this is not an urgent matter.
    pred_info_update_goal_type(np_goal_type_foreign, !PredInfo),
    module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo).

%---------------------%

    % Add the pragma_foreign_proc goal to the clauses_info for this procedure.
    % To do so, we must also insert unifications between the variables in the
    % pragma foreign_proc declaration and the head vars of the pred.
    %
:- pred clauses_info_add_foreign_proc(module_info::in, pred_or_func::in,
    module_name::in, string::in, pred_id::in, proc_id::in,
    list(pragma_var)::in, list(mer_type)::in, purity::in,
    foreign_proc_attributes::in, prog_context::in,
    pragma_foreign_proc_impl::in, clauses_info::in, clauses_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

clauses_info_add_foreign_proc(ModuleInfo, PredOrFunc,
        PredModuleName, PredName, PredId, ProcId, PragmaVars, OrigArgTypes,
        Purity, Attributes0, Context, PragmaImpl, !ClausesInfo, !Specs) :-
    % Our caller should have already added this foreign_proc to ItemNumbers.
    !.ClausesInfo = clauses_info(VarSet0, ExplicitVarTypes,
        VarTable, RttiVarMaps, TVarNameMap, HeadVars, ClausesRep0,
        ItemNumbers, _HasForeignClauses, HadSyntaxError),

    % Currently we can override Mercury clauses with a foreign_proc right here,
    % which means that semantic analysis never sees those Mercury clauses.
    % Any errors in them thus do get picked not when they first arise, but
    % only when the code gets compiled for a target that requires their use.
    % XXX We should retain and check the Mercury clauses, and override them
    % with a more specific foreign language implementation only after semantic
    % analysis.
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    NewLang = get_foreign_language(Attributes0),
    PredFormArity = arg_list_arity(OrigArgTypes),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    AllProcIds = pred_info_all_procids(PredInfo),
    get_clause_list_for_replacement(ClausesRep0, Clauses0),
    add_foreign_proc_update_existing_clauses(Globals, PredOrFunc,
        PredModuleName, PredName, PredFormArity, Context, Target, NewLang,
        AllProcIds, ProcId, Overridden, Clauses0, Clauses1, !Specs),

    % We used have this code here, but as of 2022 feb 15, and almost certainly
    % for a long, long time before that, it effectively did nothing.
%   % If the foreign language is not one of the backend languages, we will
%   % have to generate an interface to it in a backend language.
%   globals.get_backend_foreign_languages(Globals, BackendForeignLanguages),
%   foreign.extrude_pragma_implementation(
%       PredOrFunc, PredModuleName, PredName, PragmaVars, Context, !ModuleInfo,
%       Attributes0, Attributes1, PragmaImpl0, PragmaImpl),

    % Build the foreign_proc.
    pragma_get_vars_and_var_infos(PragmaVars, ArgVars, ArgInfos),
    (
        Overridden = overridden_by_old_foreign_proc
    ;
        Overridden = not_overridden_by_old_foreign_proc,
        % Put the purity in the goal_info in case this foreign code is
        % inlined.
        goal_info_init_context_purity(Context, Purity, GoalInfo),
        % XXX ARGVEC - the foreign_args field in the hlds_goal_expr type
        % should also be a an proc_arg_vector rather than a list.
        HeadVarList = proc_arg_vector_to_list(HeadVars),
        make_foreign_args(HeadVarList, ArgInfos, OrigArgTypes, ForeignArgs),
        % Perform some renaming in any user annotated sharing information.
        maybe_rename_user_annotated_sharing_information(Globals,
            ArgVars, HeadVarList, OrigArgTypes, Attributes0, Attributes),
        ExtraArgs = [],
        MaybeTraceRuntimeCond = no,
        GoalExpr = call_foreign_proc(Attributes, PredId, ProcId,
            ForeignArgs, ExtraArgs, MaybeTraceRuntimeCond, PragmaImpl),
        HldsGoal0 = hlds_goal(GoalExpr, GoalInfo),
        % Foreign_procs cannot contain explicit variable type annotations.
        init_vartypes(EmptyExplicitVarTypes),
        rtti_varmaps_init(EmptyRttiVarmaps),
        implicitly_quantify_clause_body_general_vs(ord_nl_maybe_lambda,
            do_not_keep_quant_vars, HeadVarList, _Warnings,
            HldsGoal0, HldsGoal, VarSet0, VarSet,
            EmptyExplicitVarTypes, _, EmptyRttiVarmaps, _),
        Clause = clause(selected_modes([ProcId]), HldsGoal,
            impl_lang_foreign(NewLang), Context,
            [], init_unused_statevar_arg_map, clause_is_not_a_fact),
        Clauses = [Clause | Clauses1],
        set_clause_list(Clauses, ClausesRep),
        HasForeignClauses = some_foreign_lang_clauses,
        !:ClausesInfo = clauses_info(VarSet, ExplicitVarTypes,
            VarTable, RttiVarMaps, TVarNameMap, HeadVars, ClausesRep,
            ItemNumbers, HasForeignClauses, HadSyntaxError)
    ).

    % Rename any user annotated structure sharing information from the
    % variables (incl. type variables) in terms of which that information
    % is expressed, to the formal variables in terms of which the clause
    % is expressed.
    %
:- pred maybe_rename_user_annotated_sharing_information(globals::in,
    list(prog_var)::in, list(prog_var)::in, list(mer_type)::in,
    foreign_proc_attributes::in, foreign_proc_attributes::out)
    is det.

maybe_rename_user_annotated_sharing_information(Globals,
        ActualHeadVars, FormalHeadVars, FormalTypes, !Attributes):-
    globals.lookup_bool_option(Globals, structure_sharing_analysis,
        SharingAnalysis),
    (
        SharingAnalysis = no
    ;
        SharingAnalysis = yes,
        rename_user_annotated_sharing(ActualHeadVars, FormalHeadVars,
            FormalTypes, get_user_annotated_sharing(!.Attributes),
            FormalUserSharing),
        set_user_annotated_sharing(FormalUserSharing, !Attributes)
    ).

:- type overridden_by_old_foreign_proc
    --->    overridden_by_old_foreign_proc
    ;       not_overridden_by_old_foreign_proc.

:- pred add_foreign_proc_update_existing_clauses(globals::in, pred_or_func::in,
    module_name::in, string::in, pred_form_arity::in, prog_context::in,
    compilation_target::in, foreign_language::in,
    list(proc_id)::in, proc_id::in, overridden_by_old_foreign_proc::out,
    list(clause)::in, list(clause)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_foreign_proc_update_existing_clauses(Globals, PredOrFunc,
        PredModuleName, PredName, PredFormArity, NewContext, Target, NewLang,
        AllProcIds, NewClauseProcId, Overridden, Clauses0, Clauses, !Specs) :-
    (
        Clauses0 = [],
        Clauses = [],
        Overridden = not_overridden_by_old_foreign_proc
    ;
        Clauses0 = [FirstClause0 | LaterClauses0],
        add_foreign_proc_update_existing_clauses(Globals, PredOrFunc,
            PredModuleName, PredName, PredFormArity, NewContext, Target,
            NewLang, AllProcIds, NewClauseProcId, LaterOverridden,
            LaterClauses0, LaterClauses, !Specs),
        FirstClause0 = clause(ApplProcIds0, _Body, FirstClauseLang,
            FirstClauseContext, _StateVarWarnings, _UnusedSVarDescs,
            _MaybeFact),
        (
            FirstClauseLang = impl_lang_mercury,
            (
                ApplProcIds0 = all_modes,
                ProcIds0 = AllProcIds
            ;
                ApplProcIds0 = selected_modes(ProcIds0)
            ;
                ( ApplProcIds0 = unify_in_in_modes
                ; ApplProcIds0 = unify_non_in_in_modes
                ),
                unexpected($pred, "unify mode for user defined predicate")
            ),
            ( if list.delete_first(ProcIds0, NewClauseProcId, ProcIds) then
                (
                    ProcIds = [],
                    % This clause is totally overridden by the new
                    % foreign_proc, so delete it.
                    Clauses = LaterClauses
                ;
                    ProcIds = [_ | _],
                    % This clause is overridden by the new foreign_proc only
                    % in some modes, so mark it as being applicable only in the
                    % remaining modes.
                    FirstClause = FirstClause0 ^ clause_applicable_procs
                        := selected_modes(ProcIds),
                    Clauses = [FirstClause | LaterClauses]
                )
            else
                % This clause is not applicable to the mode of the new
                % foreign_proc, so leave it alone.
                Clauses = [FirstClause0 | LaterClauses]
            ),
            % A Mercury clause can never take precedence over a foreign_proc.
            Overridden = LaterOverridden
        ;
            FirstClauseLang = impl_lang_foreign(OldLang),
            (
                ApplProcIds0 = all_modes,
                unexpected($pred, "all_modes")
            ;
                ApplProcIds0 = selected_modes(ProcIds0)
            ;
                ( ApplProcIds0 = unify_in_in_modes
                ; ApplProcIds0 = unify_non_in_in_modes
                ),
                unexpected($pred, "unify modes")
            ),
            ( if list.delete_first(ProcIds0, NewClauseProcId, ProcIds) then
                PreferNewForeignLang = prefer_foreign_language(Globals, Target,
                    OldLang, NewLang),
                (
                    PreferNewForeignLang = yes,
                    (
                        ProcIds = [],
                        % The language of the new foreign_proc is preferred
                        % to the language of the old foreign_proc,
                        % so we should replace the old foreign_proc.
                        Clauses = LaterClauses,
                        Overridden = LaterOverridden
                    ;
                        ProcIds = [_ | _],
                        % The language of the new foreign_proc is preferred
                        % to the language of the old foreign_proc,
                        % but the old foreign_proc is still applicable
                        % in some modes, so we keep it in those modes.
                        %
                        % XXX This should not happen.
                        FirstClause = FirstClause0 ^ clause_applicable_procs
                            := selected_modes(ProcIds),
                        Clauses = [FirstClause | LaterClauses],
                        Overridden = LaterOverridden
                    ),
                    % Any later clause that overrides the new foreign_proc
                    % should have overridden this old foreign_proc as well.
                    expect(
                        unify(LaterOverridden,
                            not_overridden_by_old_foreign_proc),
                        $pred, "inconsistent old foreign_procs")
                ;
                    PreferNewForeignLang = no,
                    % We prefer the old foreign_proc to the new one,
                    % so keep the old one and tell our caller to ignore
                    % the new one.
                    Clauses = [FirstClause0 | LaterClauses],
                    Overridden = overridden_by_old_foreign_proc,

                    % However, if the old and the new foreign_procs are
                    % in the same language, then we emit an error message
                    % as well.
                    % XXX This won't detect multiple clauses in languages
                    % that are not supported by this backend, since we filter
                    % out foreign_procs in such languages way before we get
                    % here.
                    ( if OldLang = NewLang then
                        report_duplicate_foreign_proc(PredOrFunc,
                            PredModuleName, PredName, PredFormArity, OldLang,
                            FirstClauseContext, NewContext, !Specs)
                    else
                        true
                    )
                )
            else
                % This old foreign_proc is not overridden by the new one,
                % so leave it alone.
                Clauses = [FirstClause0 | LaterClauses],
                Overridden = LaterOverridden
            )
        )
    ).

%---------------------------------------------------------------------------%

:- type maybe_allowed_to_add_foreign_proc
    --->    allowed_to_add_foreign_proc
    ;       not_allowed_to_add_foreign_proc.

    % Perform all the checks on a foreign_proc that we can.
    %
:- pred check_foreign_proc(module_info::in, pred_info::in,
    pf_sym_name_arity::in, maybe_error(proc_id, error_spec)::in,
    prog_varset::in, list(pragma_var)::in, foreign_proc_attributes::in,
    pragma_foreign_proc_impl::in, prog_context::in,
    maybe_allowed_to_add_foreign_proc::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_foreign_proc(ModuleInfo, PredInfo, PFSymNameArity, MaybeProcId,
        ProgVarSet, PragmaVars, Attributes, PragmaImpl, Context,
        !:AllowedToAdd, !Specs) :-
    Lang = get_foreign_language(Attributes),
    report_if_fproc_is_for_imported_pred(PredInfo, Lang, Context,
        ImportedPredSpecs),
    report_if_fproc_is_for_builtin(ModuleInfo, PredInfo, Lang, Context,
        !:AllowedToAdd, !Specs),
    pragma_get_vars_and_var_infos(PragmaVars, ArgVars, _ArgInfos),
    check_foreign_proc_arg_list(PFSymNameArity, ProgVarSet, ArgVars,
        Lang, Context, ArgListSpecs),
    (
        ArgListSpecs = []
    ;
        ArgListSpecs = [_ | _],
        !:AllowedToAdd = not_allowed_to_add_foreign_proc,
        !:Specs = ArgListSpecs ++ !.Specs
    ),
    PragmaImpl = fp_impl_ordinary(Code, _),
    foreign_code_to_identifiers(Lang, Code, IdentifierList, CommentList),
    comments_to_identifiers(CommentList, CommentIdentifierList),
    set.list_to_set(IdentifierList, Identifiers),
    set.list_to_set(CommentIdentifierList, CommentIdentifiers),
    warn_singletons_in_pragma_foreign_proc(PredInfo, PFSymNameArity, Lang,
        PragmaVars, Identifiers, Context, !Specs),
    check_fp_body_for_return(PFSymNameArity, Lang, Identifiers, Context,
        !Specs),
    (
        MaybeProcId = ok(ProcId),
        % Without the ProcId, we cannot look up the procedure's determinism,
        % and the determinism specifies whether the code *should* contain
        % a reference to SUCCESS_INDICATOR.
        check_fp_body_for_success_indicator(PredInfo, PFSymNameArity, ProcId,
            Lang, Identifiers, Context, !Specs)
    ;
        MaybeProcId = error(ProcIdSpec),
        % Our caller checks for MaybeProcId = ok(_), but just to be sure ...
        !:AllowedToAdd = not_allowed_to_add_foreign_proc,
        !:Specs = [ProcIdSpec | !.Specs]
    ),
    check_foreign_proc_purity(PredInfo, Attributes, Lang, Context, !Specs),
    % We include CommentIdentifiers in the set of identifiers we pass to
    % check_typeinfos_for_existq_tvars, because without doing that,
    % we cannot pass the invalid/foreign_procs_exist_type test case.
    % The reason is simple. With user-provided arguments, programmers
    % can indicate that the argument is a singleton by adding an underscore
    % prefix. This is not possible with compiler-generated arguments.
    % For them, the only way to avoid the singleton warning is to mention
    % the variable name in actual code (which is cumbersome to arrange)
    % or in comments (which is much easier).
    set.union(Identifiers, CommentIdentifiers, AllIdentifiers),
    check_typeinfos_for_existq_tvars(PredInfo, Lang, AllIdentifiers, Context,
        !Specs),
    (
        ImportedPredSpecs = []
    ;
        ImportedPredSpecs = [_ | _],
        !:AllowedToAdd = not_allowed_to_add_foreign_proc,
        % Ignoring all the other diagnostics has one good and one bad effect.
        %
        % The good effect is the reduction of clutter.
        %
        % The bad effect is that it prevents programmers from fixing
        % the problems we *would* have reported *at the same* as moving
        % the foreign_proc to the module containing the predicate it is for.
        %
        % However, this kind of bug is so rare, it does not really matter
        % which choice we make here.
        !:Specs = ImportedPredSpecs
    ).

%---------------------------------------------------------------------------%

    % Don't allow definitions, whether clauses or foreign_procs,
    % for imported predicates/functions. Note that this applies to
    % *plain* imported predicates/functions, not the *opt-imported* ones.
    %
:- pred report_if_fproc_is_for_imported_pred(pred_info::in,
    foreign_language::in, prog_context::in, list(error_spec)::out) is det.

report_if_fproc_is_for_imported_pred(PredInfo, Lang, Context, Specs) :-
    ( if pred_info_is_imported(PredInfo) then
        LangStr = foreign_language_string(Lang),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        pred_info_get_sym_name(PredInfo, PredSymName),
        user_arity(UserArityInt) = pred_info_user_arity(PredInfo),
        SNA = sym_name_arity(PredSymName, UserArityInt),
        Pieces = [words("Error: this"), words(LangStr),
            words("foreign_proc is for")] ++
            color_as_subject([qual_sym_name_arity(SNA), suffix(",")]) ++
            [words("which is an")] ++
            color_as_incorrect([words("imported")]) ++
            [p_or_f(PredOrFunc), suffix("."),
            pragma_decl("foreign_proc"), words("declarations are allowed"),
            words("only for predicates and functions"),
            words("defined in the current module."), nl],
        Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
        Specs = [Spec]
    else
        Specs = []
    ).

%---------------------%

:- pred report_if_fproc_is_for_builtin(module_info::in, pred_info::in,
    foreign_language::in, prog_context::in,
    maybe_allowed_to_add_foreign_proc::out,
    list(error_spec)::in, list(error_spec)::out) is det.

report_if_fproc_is_for_builtin(ModuleInfo, PredInfo, Lang, Context,
        AllowedToAdd, !Specs) :-
    ( if pred_info_is_builtin(PredInfo) then
        AllowedToAdd = not_allowed_to_add_foreign_proc,
        % When bootstrapping a change that defines a builtin using
        % normal Mercury code, we need to disable the generation
        % of the error message, and just ignore the definition.
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, allow_defn_of_builtins,
            AllowDefnOfBuiltin),
        (
            AllowDefnOfBuiltin = no,
            LangStr = foreign_language_string(Lang),
            pred_info_get_sym_name(PredInfo, SymName),
            PredOrFunc = pred_info_is_pred_or_func(PredInfo),
            user_arity(UserArityInt) = pred_info_user_arity(PredInfo),
            SNA = sym_name_arity(SymName, UserArityInt),
            Pieces = [words("Error: this"), words(LangStr),
                words("foreign_proc is for the")] ++
                color_as_subject([qual_sym_name_arity(SNA)]) ++
                [p_or_f(PredOrFunc), suffix(","), words("which is a")] ++
                color_as_incorrect([words("builtin")]) ++ [words("operation."),
                words("Redefinitions of builtin operations are")] ++
                color_as_incorrect([words("not allowed.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        ;
            AllowDefnOfBuiltin = yes
        )
    else
        AllowedToAdd = allowed_to_add_foreign_proc
    ).

%---------------------%

:- func report_fproc_for_undeclared_mode(pf_sym_name_arity,
    foreign_proc_attributes, prog_context) = error_spec.

report_fproc_for_undeclared_mode(PFSymNameArity, Attributes, Context) = Spec :-
    Lang = get_foreign_language(Attributes),
    LangStr = foreign_language_string(Lang),
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, PredSymName, PredFormArity),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    UserArity = user_arity(UserArityInt),
    SNA = sym_name_arity(PredSymName, UserArityInt),
    Pieces = [words("Error:"), words(LangStr), words("foreign_proc for")] ++
        color_as_incorrect([words("undeclared mode")]) ++
        [words("of"), p_or_f(PredOrFunc)] ++
        color_as_subject([qual_sym_name_arity(SNA), suffix(".")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces).

%---------------------%

:- pred report_duplicate_foreign_proc(pred_or_func::in,
    module_name::in, string::in, pred_form_arity::in, foreign_language::in,
    prog_context::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_foreign_proc(PredOrFunc, PredModuleName, PredName,
        PredFormArity, Lang, FirstClauseContext, NewContext, !Specs) :-
    PredSymName = qualified(PredModuleName, PredName),
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, PredSymName, PredFormArity),
    LangStr = foreign_language_string(Lang),
    PiecesA = [words("Error:")] ++
        color_as_incorrect([words("duplicate"), words(LangStr),
            words("foreign_proc")]) ++
        [words("for this mode of"),
        unqual_pf_sym_name_pred_form_arity(PFSymNameArity), suffix("."), nl],
    PiecesB = [words("The first one was here."), nl],
    MsgA = msg(NewContext, PiecesA),
    MsgB = msg(FirstClauseContext, PiecesB),
    Spec = error_spec($pred, severity_error, phase_pt2h, [MsgA, MsgB]),
    !:Specs = [Spec | !.Specs].

%---------------------%

:- pred report_bad_foreign_proc_in_dot_opt_file(wrong_backend_cause::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

report_bad_foreign_proc_in_dot_opt_file(WrongBackendCause, Context, !Specs) :-
    (
        WrongBackendCause = wrong_lang(PragmaLang, BackendForeignLangs),
        PragmaLangStr = foreign_language_string(PragmaLang),
        FrontPieces = [words("Error: this foreign_proc is for")] ++
            color_as_incorrect([words(PragmaLangStr), suffix(",")]),
        (
            BackendForeignLangs = [],
            unexpected($pred, "BackendForeignLangs = []")
        ;
            BackendForeignLangs = [BackendForeignLang],
            BackendLangStr = foreign_language_string(BackendForeignLang),
            MainPieces = FrontPieces ++ [words("but the only language"),
                words("supported by the current backend is")] ++
                color_as_correct([words(BackendLangStr), suffix(".")]) ++
                [nl]
        ;
            BackendForeignLangs = [_, _ | _],
            BackendLangStrs =
                list.map(foreign_language_string, BackendForeignLangs),
            BackendLangsStr =
                fixed_list_to_color_pieces(color_correct, "and", [suffix(".")],
                    BackendLangStrs),
            MainPieces = FrontPieces ++ [words("which is not one of the"),
                words("languages supported by the current backend."),
                words("The supported languages are")] ++
                BackendLangsStr ++ [nl]
        )
    ;
        WrongBackendCause = right_lang_wrong_backend,
        MainPieces = [words("Error: this foreign_proc"),
            words("has a backend attribute that states that")] ++
            color_as_incorrect([words("it is not for the current grade.")]) ++
            [nl]
    ),
    Pieces = MainPieces ++ [words("This indicates that the .opt file"),
        words("was generated for a different grade."),
        words("You will need to rebuild this file"),
        words("for the current grade."), nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------%

    % Check that the purity of a predicate/function declaration agrees
    % with the (promised) purity of the foreign proc. We do not perform
    % this check if there is a promise_{pure,semipure} pragma for the
    % predicate/function, since in that case they will differ anyway.
    %
:- pred check_foreign_proc_purity(pred_info::in, foreign_proc_attributes::in,
    foreign_language::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_foreign_proc_purity(PredInfo, Attributes, Lang, Context, !Specs) :-
    pred_info_get_markers(PredInfo, PredMarkers),
    ( if
        ( marker_is_present(PredMarkers, marker_promised_pure)
        ; marker_is_present(PredMarkers, marker_promised_semipure)
        )
    then
        true
    else
        pred_info_get_purity(PredInfo, PredPurity),
        ForeignAttributePurity = get_purity(Attributes),
        ( if ForeignAttributePurity = PredPurity then
            true
        else
            LangStr = foreign_language_string(Lang),
            pred_info_get_pf_sym_name_arity(PredInfo, PFSymNameArity),
            PFSymNameArity =
                pf_sym_name_arity(PredOrFunc, _PredSymName, _PredFormArity),
            purity_name(ForeignAttributePurity, ForeignAttributePurityStr),
            purity_name(PredPurity, PredPurityStr),
            Pieces = [words("Error: this"), words(LangStr),
                words("foreign_proc for"),
                unqual_pf_sym_name_pred_form_arity(PFSymNameArity),
                words("has purity")] ++
                color_as_incorrect([words(ForeignAttributePurityStr),
                    suffix(",")]) ++
                [words("but that"), p_or_f(PredOrFunc),
                words("has been declared")] ++
                color_as_correct([words(PredPurityStr), suffix(".")]) ++ [nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ).

%---------------------%

:- pred check_typeinfos_for_existq_tvars(pred_info::in,
    foreign_language::in, set(string)::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_typeinfos_for_existq_tvars(PredInfo, Lang, Identifiers, Context,
        !Specs) :-
    pred_info_get_arg_types(PredInfo, TypeVarSet, ExistQVars, _ArgTypes),
    % NOTE This code will generate a separate error_spec for each
    % type_info variable that that does should occur, but does not occur,
    % in the foreign_proc. We *could* gather up their names and issue
    % just a single error_spec listing all their names, but this situation
    % is so rare that whether we generate separate or combined error_specs
    % does not matter.
    list.foldl2(
        check_typeinfo_for_existq_tvar(PredInfo, TypeVarSet, Lang,
            Identifiers, Context),
        ExistQVars, 1u, _N, !Specs).

:- pred check_typeinfo_for_existq_tvar(pred_info::in, tvarset::in,
    foreign_language::in, set(string)::in, prog_context::in, tvar::in,
    uint::in, uint::out, list(error_spec)::in, list(error_spec)::out) is det.

check_typeinfo_for_existq_tvar(PredInfo, TypeVarSet, Lang, Identifiers,
        Context, ExistQVar, !ExistQVarNum, !Specs) :-
    varset.lookup_name(TypeVarSet, ExistQVar, Name),
    OldVarName = "TypeInfo_for_" ++ Name,
    NewVarName = "TypeInfo_Out_" ++ string.uint_to_string(!.ExistQVarNum),
    !:ExistQVarNum = !.ExistQVarNum + 1u,
    ( if
        ( set.contains(Identifiers, OldVarName)
        ; set.contains(Identifiers, NewVarName)
        )
    then
        true
    else
        LangStr = foreign_language_string(Lang),
        Pieces = [words("Error: the code of the"), words(LangStr),
            words("foreign_proc for")] ++
            describe_one_pred_info_name(yes(color_subject),
                should_not_module_qualify, [], PredInfo) ++
            [words("should define the variable")] ++
            color_as_incorrect([quote(OldVarName), suffix(".")]) ++ [nl],
        Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------%

    % Check for arguments occurring more than once.
    %
:- pred check_foreign_proc_arg_list(pf_sym_name_arity::in, prog_varset::in,
    list(prog_var)::in, foreign_language::in, prog_context::in,
    list(error_spec)::out) is det.

check_foreign_proc_arg_list(PFSymNameArity, ProgVarSet, ArgVars, Lang, Context,
        Specs) :-
    bag.init(ArgVarBag0),
    bag.insert_list(ArgVars, ArgVarBag0, ArgVarBag),
    bag.to_list_only_duplicates(ArgVarBag, MultiplyOccurringArgVars),
    (
        MultiplyOccurringArgVars = [_ | _],
        LangStr = foreign_language_string(Lang),
        Pieces1 = [words("Error in"), words(LangStr),
            words("foreign_proc for"),
            unqual_pf_sym_name_pred_form_arity(PFSymNameArity),
            suffix(":"), nl],
        (
            MultiplyOccurringArgVars = [MultiplyOccurringArgVar],
            BadVarPiece =
                var_to_quote_piece(ProgVarSet, MultiplyOccurringArgVar),
            Pieces2 = [words("variable")] ++
                color_as_subject([BadVarPiece]) ++
                color_as_incorrect([words("occurs more than once")])
        ;
            MultiplyOccurringArgVars = [_, _ | _],
            BadVarPieces = list.map(var_to_quote_piece(ProgVarSet),
                MultiplyOccurringArgVars),
            BadVarsPieces = piece_list_to_color_pieces(color_subject, "and",
                [], BadVarPieces),
            Pieces2 = [words("variables")] ++ BadVarsPieces ++
                color_as_incorrect([words("each occur more than once")])
        ),
        Pieces3 = [words("in the argument list."), nl],
        Spec = spec($pred, severity_error, phase_pt2h,
            Context, Pieces1 ++ Pieces2 ++ Pieces3),
        Specs = [Spec]
    ;
        MultiplyOccurringArgVars = [],
        Specs = []
    ).

%---------------------%

    % warn_singletons_in_pragma_foreign_proc checks to see if each variable
    % is mentioned at least once in the foreign code fragments that ought to
    % mention it. If not, it gives a warning.
    %
    % (Note that for some foreign languages it might not be appropriate
    % to do this check, or you may need to add a transformation to map
    % Mercury variable names into identifiers for that foreign language).
    %
:- pred warn_singletons_in_pragma_foreign_proc(pred_info::in,
    pf_sym_name_arity::in, foreign_language::in, list(pragma_var)::in,
    set(string)::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_singletons_in_pragma_foreign_proc(PredInfo, PFSymNameArity,
        Lang, PragmaVars, Identifiers, Context, !Specs) :-
    pragma_get_var_infos(PragmaVars, ArgInfos),
    ArgNameModes =
        list.map(foreign_arg_name_mode_box_project_maybe_name_mode, ArgInfos),
    list.filter_map(var_is_unmentioned(Identifiers),
        ArgNameModes, UnmentionedVars),
    pred_info_get_markers(PredInfo, PredMarkers),
    ( if
        UnmentionedVars = [_ | _],
        not marker_is_present(PredMarkers, marker_fact_table_semantic_errors)
    then
        LangStr = foreign_language_string(Lang),
        variable_warning_start(UnmentionedVars, VarPieces, DoDoes),
        Pieces = [words("In the"), words(LangStr), words("foreign_proc for"),
            unqual_pf_sym_name_pred_form_arity(PFSymNameArity),
            suffix(":"), nl,
            words("warning:")] ++ VarPieces ++
            color_as_incorrect([words(DoDoes), words("not occur")]) ++
            [words("in the"), words(LangStr), words("code."), nl],
        Severity = severity_warning(warn_singleton_vars),
        Spec = spec($pred, Severity, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

:- pred var_is_unmentioned(set(string)::in, maybe(foreign_arg_name_mode)::in,
    string::out) is semidet.

var_is_unmentioned(Identifiers, MaybeArg, Name) :-
    MaybeArg = yes(foreign_arg_name_mode(Name, _Mode)),
    not string.prefix(Name, "_"),
    not set.member(Name, Identifiers).

:- pred variable_warning_start(list(string)::in, list(format_piece)::out,
    string::out) is det.

variable_warning_start(UnmentionedVars, Pieces, DoDoes) :-
    ( if UnmentionedVars = [Var] then
        Pieces = [words("variable")] ++ color_as_subject([quote(Var)]),
        DoDoes = "does"
    else
        Pieces = [words("variables")] ++
            quote_list_to_color_pieces(color_subject, "and", [],
                UnmentionedVars),
        DoDoes = "do"
    ).

%---------------------%

:- pred check_fp_body_for_success_indicator(pred_info::in,
    pf_sym_name_arity::in, proc_id::in, foreign_language::in,
    set(string)::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_fp_body_for_success_indicator(PredInfo, PFSymNameArity, ProcId, Lang,
        Identifiers, Context, !Specs) :-
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.lookup(ProcTable, ProcId, ProcInfo),
    proc_info_get_declared_determinism(ProcInfo, MaybeDeclDetism),
    (
        MaybeDeclDetism = yes(Detism),
        SuccIndStr = "SUCCESS_INDICATOR",
        (
            ( Detism = detism_det
            ; Detism = detism_cc_multi
            ; Detism = detism_erroneous
            ),
            ( if set.member(SuccIndStr, Identifiers) then
                LangStr = foreign_language_string(Lang),
                Pieces = [words("Warning: the"), fixed(LangStr),
                    words("code in the foreign_proc for"),
                    unqual_pf_sym_name_pred_form_arity(PFSymNameArity)] ++
                    color_as_inconsistent([words("may set"),
                        quote(SuccIndStr), suffix(",")]) ++
                    [words("but")] ++
                    color_as_inconsistent([words("it cannot fail.")]) ++ [nl],
                Severity = severity_warning(warn_suspicious_foreign_procs),
                Spec = spec($pred, Severity, phase_pt2h, Context, Pieces),
                !:Specs = [Spec | !.Specs]
            else
                true
            )
        ;
            ( Detism = detism_semi
            ; Detism = detism_cc_non
            ),
            ( if set.member(SuccIndStr, Identifiers) then
                true
            else
                LangStr = foreign_language_string(Lang),
                Pieces = [words("Warning: the"), fixed(LangStr),
                    words("code in the foreign_proc for"),
                    unqual_pf_sym_name_pred_form_arity(PFSymNameArity)] ++
                    color_as_inconsistent([words("does not appear to set"),
                        quote(SuccIndStr), suffix(",")]) ++
                    [words("but")] ++
                    color_as_inconsistent([words("it can fail.")]) ++ [nl],
                Severity = severity_warning(warn_suspicious_foreign_procs),
                Spec = spec($pred, Severity, phase_pt2h, Context, Pieces),
                !:Specs = [Spec | !.Specs]
            )
        ;
            ( Detism = detism_multi
            ; Detism = detism_non
            ; Detism = detism_failure
            )
        )
    ;
        MaybeDeclDetism = no
    ).

%---------------------%

    % Check to see if a foreign_proc body contains a return statement
    % (or whatever the foreign language equivalent is).
    %
:- pred check_fp_body_for_return(pf_sym_name_arity::in, foreign_language::in,
    set(string)::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_fp_body_for_return(PFSymNameArity, Lang, Identifiers, Context, !Specs) :-
    ( if set.member("return", Identifiers) then
        LangStr = foreign_language_string(Lang),
        PFSPiece = unqual_pf_sym_name_pred_form_arity(PFSymNameArity),
        Pieces = [words("Warning: the"), fixed(LangStr),
            words("code in the foreign_proc for")] ++
            color_as_subject([PFSPiece]) ++
            [words("may contain a")] ++
            color_as_incorrect([quote("return"), words("statement.")]) ++
            [nl],
        Severity = severity_warning(warn_suspicious_foreign_procs),
        Spec = spec($pred, Severity, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_foreign_proc.
%---------------------------------------------------------------------------%
