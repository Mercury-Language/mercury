%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pragma.add_pragma_type_spec.
:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.

:- import_module list.

:- pred add_pragma_type_spec(pragma_info_type_spec::in, term.context::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_args.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_hlds_error.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module parse_tree.vartypes.
:- import_module recompilation.

:- import_module assoc_list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module one_or_more.
:- import_module pair.
:- import_module set.
:- import_module varset.

%-----------------------------------------------------------------------------%

add_pragma_type_spec(TSInfo, Context, !ModuleInfo, !QualInfo, !Specs) :-
    TSInfo = pragma_info_type_spec(PFUMM, SymName, _, _, _, _),
    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    (
        (
            PFUMM = pfumm_predicate(ModesOrArity),
            PredOrFunc = pf_predicate
        ;
            PFUMM = pfumm_function(ModesOrArity),
            PredOrFunc = pf_function
        ),
        MaybePredOrFunc = yes(PredOrFunc),
        (
            ModesOrArity = moa_modes(Modes),
            PredFormArity = arg_list_arity(Modes),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
            MaybeModes = yes(Modes)
        ;
            ModesOrArity = moa_arity(UserArity),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
            MaybeModes = no
        ),
        predicate_table_lookup_pf_sym_arity(PredTable, is_fully_qualified,
            PredOrFunc, SymName, PredFormArity, PredIds),
        predicate_table_lookup_pf_sym(PredTable, is_fully_qualified,
            PredOrFunc, SymName, AllArityPredIds),
        UserArity = user_arity(UserArityInt)
    ;
        PFUMM = pfumm_unknown(UserArity),
        maybe_warn_about_pfumm_unknown(!.ModuleInfo, "type_spec",
            PFUMM, SymName, Context, !Specs),
        UserArity = user_arity(UserArityInt),
        MaybePredOrFunc = no,
        MaybeModes = no,
        predicate_table_lookup_sym_arity(PredTable, is_fully_qualified,
            SymName, UserArity, PredIds),
        predicate_table_lookup_sym(PredTable, is_fully_qualified,
            SymName, AllArityPredIds)
    ),
    (
        PredIds = [],
        module_info_get_pred_id_table(!.ModuleInfo, PredIdTable),
        find_user_arities_other_than(PredIdTable, AllArityPredIds, UserArity,
            OtherUserArities),
        OtherUserArityInts =
            list.map(project_user_arity_int, OtherUserArities),
        report_undefined_pred_or_func_error(MaybePredOrFunc, SymName,
            UserArityInt, OtherUserArityInts, Context,
            [pragma_decl("type_spec"), words("declaration")], !Specs)
    ;
        PredIds = [_ | _],
        list.foldl3(
            add_pragma_type_spec_for_pred(TSInfo, UserArity, MaybeModes,
                Context),
            PredIds, !ModuleInfo, !QualInfo, !Specs)
    ).

:- pred add_pragma_type_spec_for_pred(pragma_info_type_spec::in,
    user_arity::in, maybe(list(mer_mode))::in, prog_context::in, pred_id::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_type_spec_for_pred(TSInfo0, UserArity, MaybeArgModes, Context,
        PredId, !ModuleInfo, !QualInfo, !Specs) :-
    TSInfo0 = pragma_info_type_spec(PFUMM0, SymName, SpecModuleName, Subst,
        TVarSet0, ExpandedItems),
    UnqualName = unqualify_name(SymName),
    pfumm_to_maybe_pf_arity_maybe_modes(PFUMM0, MaybePredOrFunc0,
        _Arity, _MaybeModes),
    Transform = tn_pragma_type_spec(MaybePredOrFunc0, TVarSet0, Subst),
    make_transformed_pred_name(UnqualName, Transform, SpecName),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    handle_pragma_type_spec_subst(Context, Subst, PredInfo0, TVarSet0,
        MaybeSubstResult, !ModuleInfo),
    (
        MaybeSubstResult = ok5(TVarSet, Types, ExistQVars, ClassContext,
            RenamedSubst),
        pred_info_get_proc_table(PredInfo0, ProcTable0),
        handle_pragma_type_spec_modes(PredId, PredInfo0, TVarSet0,
            MaybeArgModes, Context, MaybeProcIds, ProcTable0, ProcTable1,
            !ModuleInfo, !Specs),
        % Remove any imported structure sharing and reuse information for the
        % original procedure as they won't be (directly) applicable.
        map.map_values_only(reset_imported_structure_sharing_reuse,
            ProcTable1, ProcTable),
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.get_opt_tuple(Globals, OptTuple),
        DoTypeSpec = OptTuple ^ ot_spec_types_user_guided,
        globals.lookup_bool_option(Globals, smart_recompilation, Smart),
        % XXX Should check whether smart recompilation has been disabled?
        ( if
            MaybeProcIds = yes(ProcIds),
            % Even if we aren't doing type specialization, we need to create
            % the interface procedures for local predicates to check the
            % type-class correctness of the requested specializations.
            %
            % If we are doing smart recompilation, we need to record the
            % pragmas even if we aren't doing type specialization, to avoid
            % problems with differing output for the recompilation tests
            % in debugging grades.

            ( DoTypeSpec = spec_types_user_guided
            ; not pred_info_is_imported(PredInfo0)
            ; Smart = yes
            )
        then
            % Build a clause to call the old predicate with the specified types
            % to force the specialization. For imported predicates this forces
            % the creation of the proper interface.
            %
            varset.init(ArgVarSet0),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
            PredFormArity = pred_form_arity(PredFormArityInt),
            make_n_fresh_vars("HeadVar__", PredFormArityInt, ArgVars,
                ArgVarSet0, ArgVarSet),

            goal_info_init(GoalInfo0),
            set_of_var.list_to_set(ArgVars, NonLocals),
            goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),
            goal_info_set_context(Context, GoalInfo1, GoalInfo),

            % We don't record the called predicate as used -- it is only used
            % if there is some other call. This call is only used to make
            % higher_order.m generate the interface for the type specialized
            % procedure, and will be removed by higher_order.m after that
            % is done.
            construct_pred_or_func_call(PredId, PredOrFunc, SymName, ArgVars,
                GoalInfo, Goal),
            Clause = clause(selected_modes(ProcIds), Goal, impl_lang_mercury,
                Context, []),
            % XXX We could use explicit type qualifications here for the
            % argument types, but explicit type qualification doesn't work
            % correctly with type inference due to a bug somewhere in
            % typecheck.m -- the explicitly declared types are not kept in
            % sync with the predicate's tvarset after the first pass of
            % type checking.
            % map.from_corresponding_lists(ArgVars, Types, ExplicitVarTypes0)
            init_vartypes(ExplicitVarTypes),
            init_var_table(VarTable),
            rtti_varmaps_init(RttiVarMaps),
            map.init(TVarNameMap),
            ArgsVec = proc_arg_vector_init(PredOrFunc, ArgVars),
            set_clause_list([Clause], ClausesRep),
            ItemNumbers = init_clause_item_numbers_comp_gen,
            Clauses = clauses_info(ArgVarSet, ExplicitVarTypes,
                VarTable, RttiVarMaps, TVarNameMap, ArgsVec, ClausesRep,
                ItemNumbers, no_foreign_lang_clauses, no_clause_syntax_errors),
            pred_info_get_markers(PredInfo0, Markers0),
            add_marker(marker_calls_are_fully_qualified, Markers0, Markers),
            map.init(Proofs),
            map.init(ConstraintMap),

            ( if pred_info_is_imported(PredInfo0) then
                PredStatus = pred_status(status_opt_imported)
            else
                pred_info_get_status(PredInfo0, PredStatus)
            ),

            pred_info_get_origin(PredInfo0, OrigOrigin),
            SubstDesc = one_or_more.map(subst_desc, Subst),
            Origin = origin_pred_transform(pred_transform_type_spec(SubstDesc),
                OrigOrigin, PredId),
            MaybeCurUserDecl = maybe.no,
            GoalType = goal_not_for_promise(np_goal_type_none),
            pred_info_get_var_name_remap(PredInfo0, VarNameRemap),
            pred_info_init(PredOrFunc, SpecModuleName, SpecName, PredFormArity,
                Context, Origin, PredStatus, MaybeCurUserDecl, GoalType,
                Markers, Types, TVarSet, ExistQVars, ClassContext, Proofs,
                ConstraintMap, Clauses, VarNameRemap, NewPredInfo0),
            pred_info_set_proc_table(ProcTable, NewPredInfo0, NewPredInfo),
            module_info_get_predicate_table(!.ModuleInfo, PredTable0),
            predicate_table_insert(NewPredInfo, NewPredId,
                PredTable0, PredTable),
            module_info_set_predicate_table(PredTable, !ModuleInfo),

            % Record the type specialisation in the module_info.
            module_info_get_type_spec_info(!.ModuleInfo, TypeSpecInfo0),
            TypeSpecInfo0 = type_spec_info(ProcsToSpec0,
                ForceVersions0, SpecMap0, PragmaMap0),
            list.map(
                ( pred(ProcId::in, PredProcId::out) is det :-
                    PredProcId = proc(PredId, ProcId)
                ), ProcIds, PredProcIds),
            set.insert_list(PredProcIds, ProcsToSpec0, ProcsToSpec),
            set.insert(NewPredId, ForceVersions0, ForceVersions),

            ( if PredStatus = pred_status(status_opt_imported) then
                % For imported predicates dead_proc_elim.m needs to know that
                % if the original predicate is used, the predicate to force
                % the production of the specialised interface is also used.
                multi_map.set(PredId, NewPredId, SpecMap0, SpecMap)
            else
                SpecMap = SpecMap0
            ),
            (
                MaybeArgModes = no,
                ModesOrArity = moa_arity(UserArity)
            ;
                MaybeArgModes = yes(ArgModes),
                ModesOrArity = moa_modes(ArgModes)
            ),
            PredOrFunc = pred_info_is_pred_or_func(PredInfo0),
            (
                PredOrFunc = pf_predicate,
                PFUMM = pfumm_predicate(ModesOrArity)
            ;
                PredOrFunc = pf_function,
                PFUMM = pfumm_function(ModesOrArity)
            ),
            TSInfo = pragma_info_type_spec(PFUMM, SymName, SpecModuleName,
                RenamedSubst, TVarSet, ExpandedItems),
            multi_map.set(PredId, TSInfo, PragmaMap0, PragmaMap),
            TypeSpecInfo = type_spec_info(ProcsToSpec, ForceVersions, SpecMap,
                PragmaMap),
            module_info_set_type_spec_info(TypeSpecInfo, !ModuleInfo),

            IsImported = pred_status_is_imported(PredStatus),
            (
                IsImported = yes,
                ItemType = pred_or_func_to_recomp_item_type(PredOrFunc),
                UserArity = user_arity(UserArityInt),
                ItemName = recomp_item_name(SymName, UserArityInt),
                ItemId = recomp_item_id(ItemType, ItemName),
                apply_to_recompilation_info(
                    recompilation.record_expanded_items(ItemId, ExpandedItems),
                    !QualInfo)
            ;
                IsImported = no
            )
        else
            true
        )
    ;
        MaybeSubstResult = error5(SubstSpecs),
        !:Specs = SubstSpecs ++ !.Specs
    ).

:- func subst_desc(pair(tvar, mer_type)) = pair(int, mer_type).

subst_desc(TVar - Type) = var_to_int(TVar) - Type.

    % Check that the type substitution for a `:- pragma type_spec'
    % declaration is valid.
    % A type substitution is invalid if:
    %   - it substitutes unknown type variables
    %   - it substitutes existentially quantified type variables
    % Type substitutions are also invalid if the replacement types are
    % not ground, however this is a (hopefully temporary) limitation
    % of the current implementation, so it only results in a warning.
    %
:- pred handle_pragma_type_spec_subst(prog_context::in,
    type_subst::in, pred_info::in, tvarset::in,
    maybe5(tvarset, list(mer_type), existq_tvars, prog_constraints,
        type_subst)::out,
    module_info::in, module_info::out) is det.

handle_pragma_type_spec_subst(Context, Subst, PredInfo0, TVarSet0,
        MaybeSubstResult, !ModuleInfo) :-
    SubstList = one_or_more_to_list(Subst),
    assoc_list.keys(SubstList, VarsToSub),
    find_duplicate_list_elements(VarsToSub, MultiSubstVars0),
    (
        MultiSubstVars0 = [_ | _],
        list.sort_and_remove_dups(MultiSubstVars0, MultiSubstVars),
        report_multiple_subst_vars(PredInfo0, Context, TVarSet0,
            MultiSubstVars, Spec),
        MaybeSubstResult = error5([Spec])
    ;
        MultiSubstVars0 = [],
        pred_info_get_typevarset(PredInfo0, CalledTVarSet),
        varset.create_name_var_map(CalledTVarSet, NameVarIndex0),
        list.filter(
            ( pred(Var::in) is semidet :-
                varset.lookup_name(TVarSet0, Var, VarName),
                not map.contains(NameVarIndex0, VarName)
            ), VarsToSub, UnknownVarsToSub),
        (
            UnknownVarsToSub = [],
            % Check that the substitution is not recursive.
            set.list_to_set(VarsToSub, VarsToSubSet),

            assoc_list.values(SubstList, SubstTypes0),
            type_vars_in_types(SubstTypes0, TVarsInSubstTypes0),
            set.list_to_set(TVarsInSubstTypes0, TVarsInSubstTypes),

            set.intersect(TVarsInSubstTypes, VarsToSubSet, RecSubstTVars0),
            set.to_sorted_list(RecSubstTVars0, RecSubstTVars),

            (
                RecSubstTVars = [],
                map.init(TVarRenaming0),
                list.append(VarsToSub, TVarsInSubstTypes0, VarsToReplace),

                get_new_tvars(VarsToReplace, TVarSet0, CalledTVarSet,
                    TVarSet, NameVarIndex0, _, TVarRenaming0, TVarRenaming),

                % Check that none of the existentially quantified variables
                % were substituted.
                map.apply_to_list(VarsToSub, TVarRenaming, RenamedVarsToSub),
                pred_info_get_exist_quant_tvars(PredInfo0, ExistQVars),
                list.filter(
                    ( pred(RenamedVar::in) is semidet :-
                        list.member(RenamedVar, ExistQVars)
                    ), RenamedVarsToSub, SubExistQVars),
                (
                    SubExistQVars = [],
                    apply_variable_renaming_to_type_list(TVarRenaming,
                        SubstTypes0, SubstTypes),
                    assoc_list.from_corresponding_lists(RenamedVarsToSub,
                        SubstTypes, SubAL),
                    map.from_assoc_list(SubAL, TypeSubst),

                    % Apply the substitution.
                    pred_info_get_arg_types(PredInfo0, Types0),
                    pred_info_get_class_context(PredInfo0, ClassContext0),
                    apply_rec_subst_to_type_list(TypeSubst, Types0, Types),
                    apply_rec_subst_to_prog_constraints(TypeSubst,
                        ClassContext0, ClassContext),
                    det_list_to_one_or_more(SubAL, RenamedSubst),
                    MaybeSubstResult = ok5(TVarSet, Types, ExistQVars,
                        ClassContext, RenamedSubst)
                ;
                    SubExistQVars = [_ | _],
                    report_subst_existq_tvars(PredInfo0, Context,
                        SubExistQVars, Spec),
                    MaybeSubstResult = error5([Spec])
                )
            ;
                RecSubstTVars = [_ | _],
                report_recursive_subst(PredInfo0, Context, TVarSet0,
                    RecSubstTVars, Spec),
                MaybeSubstResult = error5([Spec])
            )
        ;
            UnknownVarsToSub = [_ | _],
            report_unknown_vars_to_subst(PredInfo0, Context, TVarSet0,
                UnknownVarsToSub, Spec),
            MaybeSubstResult = error5([Spec])
        )
    ).

:- pred find_duplicate_list_elements(list(T)::in, list(T)::out) is det.

find_duplicate_list_elements([], []).
find_duplicate_list_elements([H | T], DupVars) :-
    find_duplicate_list_elements(T, DupVars0),
    ( if list.member(H, T) then
        DupVars = [H | DupVars0]
    else
        DupVars = DupVars0
    ).

:- pred report_subst_existq_tvars(pred_info::in, prog_context::in,
    list(tvar)::in, error_spec::out) is det.

report_subst_existq_tvars(PredInfo, Context, SubExistQVars, Spec) :-
    pred_info_get_typevarset(PredInfo, TVarSet),
    Pieces = pragma_type_spec_to_pieces(PredInfo) ++
        [words("error: the substitution includes"),
        words("the existentially quantified type")] ++
        report_variables(SubExistQVars, TVarSet) ++ [suffix(".")],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces).

:- pred report_recursive_subst(pred_info::in, prog_context::in, tvarset::in,
    list(tvar)::in, error_spec::out) is det.

report_recursive_subst(PredInfo, Context, TVarSet, RecursiveVars, Spec) :-
    Pieces = pragma_type_spec_to_pieces(PredInfo) ++
        [words("error:")] ++ report_variables(RecursiveVars, TVarSet) ++
        [words(choose_number(RecursiveVars, "occurs", "occur")),
        words("on both sides of the substitution.")],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces).

:- pred report_multiple_subst_vars(pred_info::in, prog_context::in,
    tvarset::in, list(tvar)::in, error_spec::out) is det.

report_multiple_subst_vars(PredInfo, Context, TVarSet, MultiSubstVars, Spec) :-
    Pieces = pragma_type_spec_to_pieces(PredInfo) ++
        [words("error:")] ++ report_variables(MultiSubstVars, TVarSet) ++
        [words(choose_number(MultiSubstVars, "has", "have")),
        words("multiple replacement types.")],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces).

:- pred report_unknown_vars_to_subst(pred_info::in, prog_context::in,
    tvarset::in, list(tvar)::in, error_spec::out) is det.

report_unknown_vars_to_subst(PredInfo, Context, TVarSet, UnknownVars, Spec) :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    (
        PredOrFunc = pf_predicate,
        Decl = "pred"
    ;
        PredOrFunc = pf_function,
        Decl = "func"
    ),
    Pieces = pragma_type_spec_to_pieces(PredInfo) ++
        [words("error:")] ++ report_variables(UnknownVars, TVarSet) ++
        [words(choose_number(UnknownVars, "does not", "do not")),
        words("occur in the"), decl(Decl), words("declaration.")],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces).

:- func pragma_type_spec_to_pieces(pred_info) = list(format_component).

pragma_type_spec_to_pieces(PredInfo) = Pieces :-
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    PredFormArity = pred_info_pred_form_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PFSymNameArity =
        pf_sym_name_arity(PredOrFunc, qualified(Module, Name), PredFormArity),
    Pieces = [words("In"), pragma_decl("type_spec"),
        words("declaration for"), qual_pf_sym_name_orig_arity(PFSymNameArity),
        suffix(":"), nl].

:- func report_variables(list(tvar), tvarset) = list(format_component).

report_variables(SubExistQVars, VarSet) =
    [words(choose_number(SubExistQVars, "variable", "variables")),
    quote(mercury_vars_to_name_only(VarSet, SubExistQVars))].

    % Check that the mode list for a `:- pragma type_spec' declaration
    % specifies a known procedure.
    %
:- pred handle_pragma_type_spec_modes(pred_id::in, pred_info::in,
    tvarset::in, maybe(list(mer_mode))::in, prog_context::in,
    maybe(list(proc_id))::out, proc_table::in, proc_table::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

handle_pragma_type_spec_modes(PredId, PredInfo, TVarSet, MaybeArgModes,
        Context, MaybeProcIds, !Procs, !ModuleInfo, !Specs) :-
    (
        MaybeArgModes = yes(ArgModes),
        map.to_assoc_list(!.Procs, ExistingProcs),
        ( if
            get_procedure_matching_argmodes(ExistingProcs, ArgModes,
                !.ModuleInfo, ProcId)
        then
            map.lookup(!.Procs, ProcId, ProcInfo),
            !:Procs = map.singleton(ProcId, ProcInfo),
            ProcIds = [ProcId],
            MaybeProcIds = yes(ProcIds)
        else
            varset.coerce(TVarSet, VarSet),
            DescPieces = [pragma_decl("type_spec"), words("declaration")],
            report_undeclared_mode_error(!.ModuleInfo, PredId, PredInfo,
                VarSet, ArgModes, DescPieces, Context, !Specs),
            MaybeProcIds = no
        )
    ;
        MaybeArgModes = no,
        map.keys(!.Procs, ProcIds),
        MaybeProcIds = yes(ProcIds)
    ).

:- pred reset_imported_structure_sharing_reuse(
    proc_info::in, proc_info::out) is det.

reset_imported_structure_sharing_reuse(!ProcInfo) :-
    proc_info_reset_imported_structure_sharing(!ProcInfo),
    proc_info_reset_imported_structure_reuse(!ProcInfo).

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pragma.add_pragma_type_spec.
%----------------------------------------------------------------------------%
