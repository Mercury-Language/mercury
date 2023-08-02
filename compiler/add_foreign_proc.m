%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_foreign_proc.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.make_hlds_types.
:- import_module hlds.status.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.

:- import_module list.

:- pred add_pragma_foreign_procs(ims_list(item_foreign_proc)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pragma_foreign_proc(item_mercury_status::in, pred_status::in,
    item_foreign_proc::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.add_pred.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.passes_aux.
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
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.vartypes.

:- import_module bag.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

add_pragma_foreign_procs([], !ModuleInfo, !Specs).
add_pragma_foreign_procs([ImsSubList | ImsSubLists], !ModuleInfo, !Specs) :-
    ImsSubList = ims_sub_list(ItemMercuryStatus, PragmaFPInfos),
    item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
    list.foldl2(add_pragma_foreign_proc(ItemMercuryStatus, PredStatus),
        PragmaFPInfos, !ModuleInfo, !Specs),
    add_pragma_foreign_procs(ImsSubLists, !ModuleInfo, !Specs).

%-----------------------------------------------------------------------------%

add_pragma_foreign_proc(ItemMercurystatus, PredStatus, PragmaFPInfo,
        !ModuleInfo, !Specs) :-
    PragmaFPInfo = item_pragma_info(FPInfo, Context, SeqNum),
    FPInfo = pragma_info_foreign_proc(Attributes0, PredSymName, PredOrFunc,
        PragmaVars, ProgVarSet, _InstVarset, PragmaImpl),
    (
        PredSymName = qualified(PredModuleName, PredName)
    ;
        PredSymName = unqualified(_),
        unexpected($pred, "unexpected PredSymName")
    ),
    PredFormArity = arg_list_arity(PragmaVars),
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, PredSymName, PredFormArity),

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        trace [io(!IO)] (
            IdStr = pf_sym_name_pred_form_arity_to_string(PFSymNameArity),
            get_progress_output_stream(!.ModuleInfo, ProgressStream, !IO),
            io.format(ProgressStream,
                "%% Processing `:- pragma foreign_proc' for %s...\n",
                [s(IdStr)], !IO)
        )
    ;
        VeryVerbose = no
    ),

    % Lookup the pred declaration in the predicate table.
    % If it is not there, print an error message and insert
    % a dummy declaration for the predicate.
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_lookup_pf_m_n_a(PredTable0, is_fully_qualified,
        PredOrFunc, PredModuleName, PredName, PredFormArity, PredIds),
    (
        PredIds = [],
        user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
        Origin = origin_user(user_made_pred(PredOrFunc,
            PredSymName, UserArity)),
        add_implicit_pred_decl_report_error(PredOrFunc, PredModuleName,
            PredName, PredFormArity, PredStatus, is_not_a_class_method,
            Context, Origin,
            [pragma_decl("foreign_proc"), words("declaration")],
            PredId, !ModuleInfo, !Specs)
    ;
        PredIds = [PredId]
    ;
        PredIds = [PredId, _ | _],
        % Any attempt to define more than one pred with the same PredOrFunc,
        % PredSymName and Arity should have been caught earlier, and an error
        % message generated. We continue so that we can try to find more
        % errors.
        AmbiPieces = [words("Error: ambiguous predicate name"),
            qual_pf_sym_name_pred_form_arity(PFSymNameArity), words("in"),
            quote("pragma foreign_proc"), suffix("."), nl],
        AmbiSpec = simplest_spec($pred, severity_error,
            phase_parse_tree_to_hlds, Context, AmbiPieces),
        !:Specs = [AmbiSpec | !.Specs]
    ),

    % Lookup the pred_info for this pred, add the pragma to the proc_info
    % in the proc_table in the pred_info, and save the pred_info.
    some [!PredInfo] (
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),

        % status_opt_imported preds are initially tagged as status_imported
        % and are tagged as status_opt_imported only if/when we see a clause
        % (including a `foreign_proc' clause) for them.
        ( if PredStatus = pred_status(status_opt_imported) then
            pred_info_set_status(pred_status(status_opt_imported), !PredInfo)
        else
            true
        ),

        % Record the existence of this "clause".
        pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
        ItemNumbers0 = ClausesInfo0 ^ cli_item_numbers,
        add_clause_item_number(SeqNum, Context, item_is_foreign_proc,
            ItemNumbers0, ItemNumbers),
        ClausesInfo1 = ClausesInfo0 ^ cli_item_numbers := ItemNumbers,
        pred_info_set_clauses_info(ClausesInfo1, !PredInfo),
        module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo),

        PredInfo0 = !.PredInfo,

        % Replace any maybe_thread_safe foreign_proc attributes with
        % the actual thread safety attributes which we get from the
        % `--maybe-thread-safe' option.
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
        ),

        CurrentBackend = lookup_current_backend(Globals),
        globals.get_backend_foreign_languages(Globals, BackendForeignLangs),
        PragmaForeignLanguage = get_foreign_language(Attributes),
        MaybeForSpecificBackend = get_for_specific_backend(Attributes),
        ( if
            MaybeForSpecificBackend = yes(SpecificBackend),
            SpecificBackend \= CurrentBackend
        then
            % Ignore this foreign_proc.
            % XXX Why are we treating this form of inapplicability
            % differently from the pragma being from a not-supported-on-
            % this-backend language?
            WarnIfOptInt = yes
        else if
            pred_info_is_imported(!.PredInfo)
        then
            Pieces = [words("Error:"), pragma_decl("foreign_proc"),
                words("declaration for imported"),
                qual_pf_sym_name_pred_form_arity(PFSymNameArity),
                suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
            !:Specs = [Spec | !.Specs],
            ( if list.member(PragmaForeignLanguage, BackendForeignLangs) then
                WarnIfOptInt = no
            else
                WarnIfOptInt = yes
            )
        else if
            % Don't add clauses for foreign languages other than the ones
            % we can generate code for.
            not list.member(PragmaForeignLanguage, BackendForeignLangs)
        then
            pred_info_update_goal_type(np_goal_type_foreign,
                PredInfo0, !:PredInfo),
            module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo),
            WarnIfOptInt = yes
        else
            % Add the pragma declaration to the proc_info for this procedure.
            pred_info_get_proc_table(!.PredInfo, Procs),
            map.to_assoc_list(Procs, ExistingProcs),
            pragma_get_modes(PragmaVars, Modes),
            ( if
                % The inst variables for the foreign_proc declaration
                % and predmode declarations are from different varsets.
                % We cannot just unify the argument modes directly because
                % the representation of the inst variables may be different.
                % Instead we need to allow for a renaming between the
                % inst variables in the argument modes of the foreign_proc
                % and those of the predmode declaration.
                %
                % XXX We should probably also check that each pair in
                % the renaming has the same name.
                get_procedure_matching_declmodes_with_renaming(!.ModuleInfo,
                    ExistingProcs, Modes, ProcId)
            then
                pred_info_get_arg_types(!.PredInfo, ArgTypes),
                pred_info_get_purity(!.PredInfo, Purity),
                pred_info_get_markers(!.PredInfo, Markers),
                clauses_info_add_pragma_foreign_proc(PredOrFunc,
                    PredModuleName, PredName, PredId, ProcId,
                    ProgVarSet, PragmaVars, ArgTypes,
                    Purity, Attributes, Markers, Context, PragmaImpl,
                    ClausesInfo1, ClausesInfo, !ModuleInfo, !Specs),
                pred_info_set_clauses_info(ClausesInfo, !PredInfo),
                pred_info_update_goal_type(np_goal_type_foreign, !PredInfo),

                module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo),
                pragma_get_var_infos(PragmaVars, ArgInfos),
                ArgNameModes = list.map(
                    foreign_arg_name_mode_box_project_maybe_name_mode,
                    ArgInfos),
                warn_singletons_in_pragma_foreign_proc(!.ModuleInfo,
                    PragmaImpl, PragmaForeignLanguage, ArgNameModes, Context,
                    PFSymNameArity, PredId, ProcId, !Specs)
            else
                Pieces = [words("Error:"),
                    pragma_decl("foreign_proc"), words("declaration"),
                    words("for undeclared mode of"),
                    qual_pf_sym_name_pred_form_arity(PFSymNameArity),
                    suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, Pieces),
                !:Specs = [Spec | !.Specs]
            ),
            WarnIfOptInt = no
        ),
        ( if
            WarnIfOptInt = yes,
            ItemMercurystatus =
                item_defined_in_other_module(item_import_opt_int)
        then
            ( if list.member(PragmaForeignLanguage, BackendForeignLangs) then
                PragmaLangStr = foreign_language_string(PragmaForeignLanguage),
                OptFrontPieces = [words("Error:"), pragma_decl("foreign_proc"),
                    words("declaration in a .opt file"),
                    words("for a foreign language,"),
                    words(PragmaLangStr), suffix(",")],
                (
                    BackendForeignLangs = [],
                    unexpected($pred, "BackendForeignLangs = []")
                ;
                    BackendForeignLangs = [BackendForeignLang],
                    BackendLangStr =
                        foreign_language_string(BackendForeignLang),
                    OptMainPieces = OptFrontPieces ++ [words("which differs"),
                        words("from the only language supported by"),
                        words("the current backend, which is"),
                        words(BackendLangStr), suffix("."), nl]
                ;
                    BackendForeignLangs = [_, _ | _],
                    BackendLangStrs = list.map(foreign_language_string,
                        BackendForeignLangs),
                    BackendLangsStr = list_to_pieces(BackendLangStrs),
                    OptMainPieces = OptFrontPieces ++
                        [words("which is not one of the languages"),
                        words("supported by the current backend,"),
                        words("which are")] ++ BackendLangsStr ++
                        [suffix("."), nl]
                )
            else
                OptMainPieces = [words("Error:"), pragma_decl("foreign_proc"),
                    words("declaration in a .opt file"),
                    words("whose backend attribute states that"),
                    words("it is not for the current grade."), nl]
            ),
            OptPieces = OptMainPieces ++
                [words("This indicates that the .opt file"),
                words("was generated for a different grade."),
                words("You will need to rebuild this file"),
                words("for the current grade."), nl],
            OptSpec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, OptPieces),
            !:Specs = [OptSpec | !.Specs]
        else
            true
        )
    ).

    % Add the pragma_foreign_proc goal to the clauses_info for this procedure.
    % To do so, we must also insert unifications between the variables in the
    % pragma foreign_proc declaration and the head vars of the pred. Also
    % return the hlds_goal.
    %
:- pred clauses_info_add_pragma_foreign_proc(pred_or_func::in,
    module_name::in, string::in, pred_id::in, proc_id::in,
    prog_varset::in, list(pragma_var)::in, list(mer_type)::in,
    purity::in, pragma_foreign_proc_attributes::in, pred_markers::in,
    prog_context::in, pragma_foreign_proc_impl::in,
    clauses_info::in, clauses_info::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

clauses_info_add_pragma_foreign_proc(PredOrFunc, PredModuleName, PredName,
        PredId, ProcId, VarSet, PragmaVars, OrigArgTypes,
        Purity, Attributes0, Markers, Context, PragmaImpl0,
        !ClausesInfo, !ModuleInfo, !Specs) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ( if pred_info_is_builtin(PredInfo) then
        % When bootstrapping a change that defines a builtin using
        % normal Mercury code, we need to disable the generation
        % of the error message, and just ignore the definition.
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, allow_defn_of_builtins,
            AllowDefnOfBuiltin),
        (
            AllowDefnOfBuiltin = no,
            Pieces = [words("Error: foreign_proc for builtin."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        ;
            AllowDefnOfBuiltin = yes
        )
    else
        AllProcIds = pred_info_all_procids(PredInfo),
        clauses_info_do_add_pragma_foreign_proc(PredOrFunc,
            PredModuleName, PredName, PredId, ProcId, AllProcIds,
            VarSet, PragmaVars, OrigArgTypes, Purity, Attributes0, Markers,
            Context, PragmaImpl0, !ClausesInfo, !ModuleInfo, !Specs)
    ).

:- pred clauses_info_do_add_pragma_foreign_proc(pred_or_func::in,
    module_name::in, string::in, pred_id::in, proc_id::in, list(proc_id)::in,
    prog_varset::in, list(pragma_var)::in, list(mer_type)::in,
    purity::in, pragma_foreign_proc_attributes::in, pred_markers::in,
    prog_context::in, pragma_foreign_proc_impl::in,
    clauses_info::in, clauses_info::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

clauses_info_do_add_pragma_foreign_proc(PredOrFunc, PredModuleName, PredName,
        PredId, ProcId, AllProcIds, PVarSet, PragmaVars, OrigArgTypes,
        Purity, Attributes0, Markers, Context, PragmaImpl,
        !ClausesInfo, !ModuleInfo, !Specs) :-
    % Our caller should have already added this foreign_proc to ItemNumbers.
    !.ClausesInfo = clauses_info(VarSet0, ExplicitVarTypes,
        VarTable, RttiVarMaps, TVarNameMap, HeadVars, ClausesRep0,
        ItemNumbers, _HasForeignClauses, HadSyntaxError),

    get_clause_list_for_replacement(ClausesRep0, Clauses0),

    % Currently we can override Mercury clauses with a foreign_proc right here,
    % which means that semantic analysis never sees those Mercury clauses.
    % Any errors in them thus do get picked not when they first arise, but
    % only when the code gets compiled for a target that requires their use.
    % XXX We should retain and check the Mercury clauses, and override them
    % with a more specific foreign language implementation only after semantic
    % analysis.
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    NewLang = get_foreign_language(Attributes0),
    PredFormArity = arg_list_arity(OrigArgTypes),
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

    % Check for arguments occurring more than once.
    pragma_get_vars_and_var_infos(PragmaVars, ArgVars, ArgInfos),
    bag.init(ArgVarBag0),
    bag.insert_list(ArgVars, ArgVarBag0, ArgVarBag),
    bag.to_assoc_list(ArgVarBag, ArgVarBagAssocList),
    list.filter_map(
        ( pred(ArgPair::in, Var::out) is semidet :-
            ArgPair = Var - Occurrences,
            Occurrences > 1
        ), ArgVarBagAssocList, MultiplyOccurringArgVars),
    (
        MultiplyOccurringArgVars = [_ | _],
        user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
        PredSymName = qualified(PredModuleName, PredName),
        PFSymNameArity =
            pred_pf_name_arity(PredOrFunc, PredSymName, UserArity),
        Pieces1 = [words("In"), pragma_decl("foreign_proc"),
            words("declaration for"),
            qual_pf_sym_name_user_arity(PFSymNameArity), suffix(":"), nl],
        (
            MultiplyOccurringArgVars = [MultiplyOccurringArgVar],
            BadVarStr = mercury_var_to_name_only_vs(PVarSet,
                MultiplyOccurringArgVar),
            Pieces2 = [words("error: variable"), quote(BadVarStr),
                words("occurs multiple times in the argument list."), nl]
        ;
            MultiplyOccurringArgVars = [_, _ | _],
            BadVarsStr = mercury_vars_to_name_only_vs(PVarSet,
                MultiplyOccurringArgVars),
            Pieces2 = [words("error: variables"), quote(BadVarsStr),
                words("occur multiple times in the argument list."), nl]
        ),
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces1 ++ Pieces2),
        !:Specs = [Spec | !.Specs]
    ;
        MultiplyOccurringArgVars = [],
        % Build the foreign_proc.
        %
        % Check that the purity of a predicate/function declaration agrees
        % with the (promised) purity of the foreign proc. We do not perform
        % this check if there is a promise_{pure,semipure} pragma for the
        % predicate/function, since in that case they will differ anyway.
        ( if
            ( check_marker(Markers, marker_promised_pure)
            ; check_marker(Markers, marker_promised_semipure)
            )
        then
            true
        else
            ForeignAttributePurity = get_purity(Attributes0),
            ( if ForeignAttributePurity = Purity then
                true
            else
                PredSymName = qualified(PredModuleName, PredName),
                user_arity_pred_form_arity(PredOrFunc, UserArity,
                    PredFormArity),
                PFSymNameArity =
                    pred_pf_name_arity(PredOrFunc, PredSymName, UserArity),
                purity_name(ForeignAttributePurity, ForeignAttributePurityStr),
                purity_name(Purity, PurityStr),
                Pieces = [words("Error: foreign clause for"),
                    unqual_pf_sym_name_user_arity(PFSymNameArity),
                    words("has purity"), words(ForeignAttributePurityStr),
                    words("but that"), p_or_f(PredOrFunc),
                    words("has been declared"), words(PurityStr),
                    suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, Pieces),
                !:Specs = [Spec | !.Specs]
            )
        ),
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
            make_foreign_args(HeadVarList, ArgInfos,
                OrigArgTypes, ForeignArgs),
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
                impl_lang_foreign(NewLang), Context, []),
            Clauses = [Clause | Clauses1],
            set_clause_list(Clauses, ClausesRep),
            HasForeignClauses = some_foreign_lang_clauses,
            !:ClausesInfo = clauses_info(VarSet, ExplicitVarTypes,
                VarTable, RttiVarMaps, TVarNameMap, HeadVars, ClausesRep,
                ItemNumbers, HasForeignClauses, HadSyntaxError)
        )
    ).

    % Rename any user annotated structure sharing information from the
    % variables (incl. type variables) in terms of which that information
    % is expressed, to the formal variables in terms of which the clause
    % is expressed.
    %
:- pred maybe_rename_user_annotated_sharing_information(globals::in,
    list(prog_var)::in, list(prog_var)::in, list(mer_type)::in,
    pragma_foreign_proc_attributes::in, pragma_foreign_proc_attributes::out)
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
        FirstClause0 = clause(ApplProcIds0, Body, FirstClauseLang,
            FirstClauseContext, StateVarWarnings),
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
                    FirstClause = clause(selected_modes(ProcIds), Body,
                        FirstClauseLang, FirstClauseContext, StateVarWarnings),
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
                        FirstClause = clause(selected_modes(ProcIds), Body,
                            FirstClauseLang, FirstClauseContext,
                            StateVarWarnings),
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
                        PredSymName = qualified(PredModuleName, PredName),
                        PFSymNameArity = pf_sym_name_arity(PredOrFunc,
                            PredSymName, PredFormArity),
                        OldLangStr = foreign_language_string(OldLang),
                        PiecesA = [words("Error: duplicate"),
                            pragma_decl("foreign_proc"), words("declaration"),
                            words("for this mode of"),
                            unqual_pf_sym_name_pred_form_arity(PFSymNameArity),
                            words("in"), words(OldLangStr), suffix("."), nl],
                        PiecesB = [words("The first one was here."), nl],
                        MsgA = simplest_msg(NewContext, PiecesA),
                        MsgB = error_msg(yes(FirstClauseContext),
                            always_treat_as_first, 0, [always(PiecesB)]),
                        Spec = error_spec($pred, severity_error,
                            phase_parse_tree_to_hlds, [MsgA, MsgB]),
                        !:Specs = [Spec | !.Specs]
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

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_foreign_proc.
%----------------------------------------------------------------------------%
