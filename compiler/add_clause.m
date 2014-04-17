%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2012,2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_clause.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.quantification.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred module_add_clause(prog_varset::in, pred_or_func::in, sym_name::in,
    list(prog_term)::in, goal::in, import_status::in, prog_context::in,
    maybe(int)::in, goal_type::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred clauses_info_add_clause(clause_applicable_modes::in, list(proc_id)::in,
    prog_varset::in, tvarset::in, list(prog_term)::in, goal::in,
    prog_context::in, maybe(int)::in, import_status::in, pred_or_func::in,
    arity::in, goal_type::in, hlds_goal::out, prog_varset::out, tvarset::out,
    clauses_info::in, clauses_info::out, list(quant_warning)::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.clause_to_proc.
:- import_module check_hlds.mode_errors.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.make_hlds.field_access.
:- import_module hlds.make_hlds.goal_expr_to_goal.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds.state_var.
:- import_module hlds.make_hlds.superhomogeneous.
:- import_module hlds.pred_table.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

module_add_clause(ClauseVarSet, PredOrFunc, PredName, Args0, Body, Status,
        Context, MaybeSeqNum, GoalType, !ModuleInfo, !QualInfo, !Specs) :-
    ( illegal_state_var_func_result(PredOrFunc, Args0, SVar) ->
        IllegalSVarResult = yes(SVar)
    ;
        IllegalSVarResult = no
    ),
    ArityAdjustment = ( if IllegalSVarResult = yes(_) then -1 else 0 ),
    expand_bang_states(Args0, Args),

    % Lookup the pred declaration in the predicate table.
    % (If it's not there, call maybe_undefined_pred_error and insert
    % an implicit declaration for the predicate.)
    module_info_get_name(!.ModuleInfo, ModuleName),
    list.length(Args, Arity0),
    Arity = Arity0 + ArityAdjustment,
    some [!PredInfo] (
        module_info_get_predicate_table(!.ModuleInfo, PredicateTable),
        predicate_table_lookup_pf_sym_arity(PredicateTable,
            is_fully_qualified, PredOrFunc, PredName, Arity, PredIds),
        ( if PredIds = [PredIdPrime] then
            MaybePredId = yes(PredIdPrime),
            ( GoalType = goal_type_promise(_) ->
                NameString = sym_name_to_string(PredName),
                string.format("%s %s %s (%s).\n",
                    [s("Attempted to introduce a predicate"),
                    s("for a promise with an identical"),
                    s("name to an existing predicate"),
                    s(NameString)], UnexpectedMsg),
                unexpected($module, $pred, UnexpectedMsg)
            ;
                true
            )
        else if unqualify_name(PredName) = ",", Arity = 2 then
            MaybePredId = no,
            Pieces = [words("Attempt to define a clause for `,'/2."),
                words("This is usually caused by"),
                words("inadvertently writing a period instead of a comma"),
                words("at the end of the preceding line."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        else
            % A promise will not have a corresponding pred declaration.
            ( GoalType = goal_type_promise(_) ->
                HeadVars = term.term_list_to_var_list(Args),
                preds_add_implicit_for_assertion(!ModuleInfo, ModuleName,
                    PredName, Arity, PredOrFunc, HeadVars, Status, Context,
                    NewPredId)
            ;
                preds_add_implicit_report_error(!ModuleInfo, ModuleName,
                    PredName, Arity, PredOrFunc, Status, no, Context,
                    origin_user(PredName), [words("clause")], NewPredId,
                    !Specs)
            ),
            MaybePredId = yes(NewPredId)
        ),
        (
            MaybePredId = yes(PredId),
            module_add_clause_2(ClauseVarSet, PredOrFunc, PredName, PredId,
                Args, Arity, ArityAdjustment, Body, Status, Context,
                MaybeSeqNum, GoalType, IllegalSVarResult,
                !ModuleInfo, !QualInfo, !Specs)
        ;
            MaybePredId = no
        )
    ).

:- pred module_add_clause_2(prog_varset::in, pred_or_func::in, sym_name::in,
    pred_id::in, list(prog_term)::in, int::in, int::in, goal::in,
    import_status::in, prog_context::in, maybe(int)::in, goal_type::in,
    maybe(prog_var)::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_clause_2(ClauseVarSet, PredOrFunc, PredName, PredId, Args,
        Arity, ArityAdjustment, Body, Status, Context, MaybeSeqNum, GoalType,
        IllegalSVarResult, !ModuleInfo, !QualInfo, !Specs) :-
    some [!PredInfo, !PredicateTable] (
        % Lookup the pred_info for this pred, add the clause to the
        % clauses_info in the pred_info, if there are no modes add an
        % `infer_modes' marker, and then save the pred_info.
        module_info_get_predicate_table(!.ModuleInfo, !:PredicateTable),
        predicate_table_get_preds(!.PredicateTable, Preds0),
        map.lookup(Preds0, PredId, !:PredInfo),

        trace [io(!IO)] (
            some [Globals] (
                module_info_get_globals(!.ModuleInfo, Globals),
                globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
                (
                    VeryVerbose = yes,
                    pred_info_get_clauses_info(!.PredInfo, MsgClauses),
                    NumClauses = num_clauses_in_clauses_rep(
                        MsgClauses ^ cli_rep),
                    io.format("%% Processing clause %d for ",
                        [i(NumClauses + 1)], !IO),
                    write_pred_or_func(PredOrFunc, !IO),
                    io.write_string(" `", !IO),
                    list.length(Args, PredArity0),
                    PredArity = PredArity0 + ArityAdjustment,
                    adjust_func_arity(PredOrFunc, OrigArity, PredArity),
                    prog_out.write_sym_name_and_arity(PredName/OrigArity, !IO),
                    io.write_string("'...\n", !IO)
                ;
                    VeryVerbose = no
                )
            )
        ),

        % Opt_imported preds are initially tagged as imported, and are tagged
        % as opt_imported only if/when we see a clause for them.
        ( Status = status_opt_imported ->
            pred_info_set_import_status(status_opt_imported, !PredInfo),
            pred_info_get_markers(!.PredInfo, InitMarkers0),
            add_marker(marker_calls_are_fully_qualified,
                InitMarkers0, InitMarkers),
            pred_info_set_markers(InitMarkers, !PredInfo)
        ;
            true
        ),
        (
            IllegalSVarResult = yes(StateVar),
            report_illegal_func_svar_result(Context, ClauseVarSet, StateVar,
                !Specs)
        ;
            IllegalSVarResult = no,
            (
                % User-supplied clauses for field access functions are not
                % allowed -- the clauses are always generated by the compiler.
                %
                PredOrFunc = pf_function,
                adjust_func_arity(pf_function, FuncArity, Arity),
                is_field_access_function_name(!.ModuleInfo, PredName,
                    FuncArity, _, _),

                % Don't report errors for clauses for field access
                % function clauses in `.opt' files.
                Status \= status_opt_imported
            ->
                CallId = simple_call_id(PredOrFunc, PredName, Arity),
                MainPieces = [
                    words("Error: clause for automatically generated"),
                    words("field access"), simple_call(CallId),
                    suffix("."), nl],
                VerbosePieces = [words("Clauses for field access functions"),
                    words("are automatically generated by the compiler."),
                    words("To supply your own definition for a field access"),
                    words("function, for example to check the input"),
                    words("to a field update, give the field"),
                    words("of the constructor a different name.")],
                Msg = simple_msg(Context,
                    [always(MainPieces), verbose_only(VerbosePieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            ;
                pred_info_is_builtin(!.PredInfo)
            ->
                % When bootstrapping a change that defines a builtin using
                % normal Mercury code, we need to disable the generation
                % of the error message, and just ignore the definition.
                some [Globals] (
                    module_info_get_globals(!.ModuleInfo, Globals),
                    globals.lookup_bool_option(Globals, allow_defn_of_builtins,
                        AllowDefnOfBuiltin)
                ),
                (
                    AllowDefnOfBuiltin = no,
                    Msg = simple_msg(Context,
                        [always([words("Error: clause for builtin.")])]),
                    Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                        [Msg]),
                    !:Specs = [Spec | !.Specs]
                ;
                    AllowDefnOfBuiltin = yes
                )
            ;
                pred_info_get_clauses_info(!.PredInfo, Clauses0),
                pred_info_get_typevarset(!.PredInfo, TVarSet0),
                maybe_add_default_func_mode(!PredInfo, _),
                select_applicable_modes(Args, ClauseVarSet, Status, Context,
                    PredId, !.PredInfo, ArgTerms, ProcIdsForThisClause,
                    AllProcIds, !ModuleInfo, !QualInfo, !Specs),
                clauses_info_add_clause(ProcIdsForThisClause, AllProcIds,
                    ClauseVarSet, TVarSet0, ArgTerms, Body,
                    Context, MaybeSeqNum, Status, PredOrFunc, Arity,
                    GoalType, Goal, VarSet, TVarSet, Clauses0, Clauses,
                    Warnings, !ModuleInfo, !QualInfo, !Specs),
                pred_info_set_clauses_info(Clauses, !PredInfo),
                ( GoalType = goal_type_promise(PromiseType) ->
                    pred_info_set_goal_type(goal_type_promise(PromiseType),
                        !PredInfo)
                ;
                    pred_info_update_goal_type(goal_type_clause, !PredInfo)
                ),
                pred_info_set_typevarset(TVarSet, !PredInfo),
                pred_info_get_arg_types(!.PredInfo, _ArgTVarSet, ExistQVars,
                    ArgTypes),
                pred_info_set_arg_types(TVarSet, ExistQVars, ArgTypes,
                    !PredInfo),

                % Check if there are still no modes for the predicate, and
                % if so, set the `infer_modes' marker for that predicate.
                % Predicates representing promises do not need mode inference.

                ProcIds = pred_info_all_procids(!.PredInfo),
                (
                    ProcIds = [],
                    GoalType \= goal_type_promise(_)
                ->
                    pred_info_get_markers(!.PredInfo, EndMarkers0),
                    add_marker(marker_infer_modes, EndMarkers0, EndMarkers),
                    pred_info_set_markers(EndMarkers, !PredInfo)
                ;
                    true
                ),
                map.det_update(PredId, !.PredInfo, Preds0, Preds),
                predicate_table_set_preds(Preds, !PredicateTable),
                module_info_set_predicate_table(!.PredicateTable, !ModuleInfo),
                ( Status = status_opt_imported ->
                    true
                ;
                    % Warn about singleton variables.
                    SimpleCallId = simple_call_id(PredOrFunc, PredName, Arity),
                    warn_singletons(!.ModuleInfo, SimpleCallId, VarSet, Goal,
                        !Specs),
                    % Warn about variables with overlapping scopes.
                    warn_overlap(Warnings, VarSet, SimpleCallId, !Specs)
                )
            )
        )
    ).

    % Extract the mode annotations (if any) from the clause arguments,
    % and determine which mode(s) this clause should apply to.
    %
:- pred select_applicable_modes(list(prog_term)::in, prog_varset::in,
    import_status::in, prog_context::in, pred_id::in, pred_info::in,
    list(prog_term)::out, clause_applicable_modes::out, list(proc_id)::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

select_applicable_modes(Args0, VarSet, Status, Context, PredId, PredInfo,
        Args, ApplProcIds, AllProcIds, !ModuleInfo, !QualInfo, !Specs) :-
    AllProcIds = pred_info_all_procids(PredInfo),
    get_mode_annotations(Args0, Args, empty, ModeAnnotations),
    (
        ModeAnnotations = modes(ModeList0),

        % The user specified some mode annotations on this clause.
        % First module-qualify the mode annotations. The annotations on
        % clauses from `.opt' files will already be fully module qualified.

        ( Status = status_opt_imported ->
            ModeList = ModeList0
        ;
            qual_info_get_mq_info(!.QualInfo, MQInfo0),
            qualify_clause_mode_list(ModeList0, ModeList, Context,
                MQInfo0, MQInfo, !Specs),
            qual_info_set_mq_info(MQInfo, !QualInfo)
        ),

        % Now find the procedure which matches these mode annotations.
        pred_info_get_procedures(PredInfo, Procs),
        map.to_assoc_list(Procs, ExistingProcs),
        (
            get_procedure_matching_declmodes_with_renaming(ExistingProcs,
                ModeList, !.ModuleInfo, ProcId)
        ->
            ApplProcIds = selected_modes([ProcId])
        ;
            undeclared_mode_error(ModeList, VarSet, PredId, PredInfo,
                !.ModuleInfo, Context, !Specs),
            % Apply the clause to all modes.
            % XXX Would it be better to apply it to none?
            ApplProcIds = selected_modes(AllProcIds)
        )
    ;
        ( ModeAnnotations = empty
        ; ModeAnnotations = none
        ),
        ( pred_info_pragma_goal_type(PredInfo) ->
            % We are only allowed to mix foreign procs and
            % mode specific clauses, so make this clause
            % mode specific but apply to all modes.
            ApplProcIds = selected_modes(AllProcIds)
        ;
            ApplProcIds = all_modes
        )
    ;
        ModeAnnotations = mixed,
        PredIdStr = pred_id_to_string(!.ModuleInfo, PredId),
        Pieces = [words("In clause for"), fixed(PredIdStr), suffix(":"), nl,
            words("syntax error: some but not all arguments"),
            words("have mode annotations."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs],

        % Apply the clause to all modes.
        % XXX Would it be better to apply it to none?
        ApplProcIds = selected_modes(AllProcIds)
    ).

:- pred undeclared_mode_error(list(mer_mode)::in, prog_varset::in,
    pred_id::in, pred_info::in, module_info::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

undeclared_mode_error(ModeList, VarSet, PredId, PredInfo, ModuleInfo, Context,
        !Specs) :-
    PredIdPieces = describe_one_pred_name(ModuleInfo,
        should_not_module_qualify, PredId),
    strip_builtin_qualifiers_from_mode_list(ModeList, StrippedModeList),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Name = pred_info_name(PredInfo),
    MaybeDet = no,
    SubDeclStr = mercury_mode_subdecl_to_string(PredOrFunc,
        varset.coerce(VarSet), unqualified(Name), StrippedModeList,
        MaybeDet, Context),

    MainPieces = [words("In clause for")] ++ PredIdPieces ++ [suffix(":"), nl,
        words("error: mode annotation specifies undeclared mode"),
        quote(SubDeclStr), suffix("."), nl],
    ProcIds = pred_info_all_procids(PredInfo),
    (
        ProcIds = [],
        VerbosePieces = [words("(There are no declared modes for this"),
            p_or_f(PredOrFunc), suffix(".)"), nl]
    ;
        ProcIds = [ProcIdsHead | ProcIdsTail],
        (
            ProcIdsTail = [],
            VerbosePieces = [words("The declared mode for this"),
                p_or_f(PredOrFunc), words("is:"),
                nl_indent_delta(1)] ++
                mode_decl_for_pred_info_to_pieces(PredInfo, ProcIdsHead) ++
                [nl_indent_delta(-1)]
        ;
            ProcIdsTail = [_ | _],
            VerbosePieces = [words("The declared modes for this"),
                p_or_f(PredOrFunc), words("are the following:"),
                nl_indent_delta(1)] ++
                component_list_to_line_pieces(
                    list.map(mode_decl_for_pred_info_to_pieces(PredInfo),
                        ProcIds),
                    []) ++
                [nl_indent_delta(-1)]
        )
    ),
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- func mode_decl_for_pred_info_to_pieces(pred_info, proc_id)
    = list(format_component).

mode_decl_for_pred_info_to_pieces(PredInfo, ProcId) =
    [words(":- mode"), words(mode_decl_to_string(ProcId, PredInfo)),
    suffix(".")].

    % Clauses can have mode annotations on them, to indicate that the
    % clause should only be used for particular modes of a predicate.
    % This type specifies the mode annotations on a clause.
:- type mode_annotations
    --->    empty   % No arguments.

    ;       none    % One or more arguments,
                    % each without any mode annotations.

    ;       modes(list(mer_mode))
                    % One or more arguments, each with a mode annotation.

    ;       mixed.  % Two or more arguments, including some with mode
                    % annotations and some without.  (This is not allowed.)

    % Extract the mode annotations (if any) from a list of arguments.
    %
:- pred get_mode_annotations(list(prog_term)::in, list(prog_term)::out,
    mode_annotations::in, mode_annotations::out) is det.

get_mode_annotations([], [], !Annotations).
get_mode_annotations([Arg0 | Args0], [Arg | Args], !Annotations) :-
    get_mode_annotation(Arg0, Arg, MaybeAnnotation),
    add_annotation(MaybeAnnotation, !Annotations),
    get_mode_annotations(Args0, Args, !Annotations).

:- pred add_annotation(maybe(mer_mode)::in,
    mode_annotations::in, mode_annotations::out) is det.

add_annotation(no,        empty, none).
add_annotation(yes(Mode), empty, modes([Mode])).
add_annotation(no,        modes(_ `with_type` list(mer_mode)), mixed).
add_annotation(yes(Mode), modes(Modes), modes(Modes ++ [Mode])).
add_annotation(no,        none, none).
add_annotation(yes(_),    none, mixed).
add_annotation(_,         mixed, mixed).

    % Extract the mode annotations (if any) from a single argument.
    %
:- pred get_mode_annotation(prog_term::in, prog_term::out,
    maybe(mer_mode)::out) is det.

get_mode_annotation(Arg0, Arg, MaybeAnnotation) :-
    (
        Arg0 = term.functor(term.atom("::"), [Arg1, ModeTerm], _),
        convert_mode(allow_constrained_inst_var, term.coerce(ModeTerm), Mode)
    ->
        Arg = Arg1,
        MaybeAnnotation = yes(Mode)
    ;
        Arg = Arg0,
        MaybeAnnotation = no
    ).

clauses_info_add_clause(ApplModeIds0, AllModeIds, CVarSet, TVarSet0,
        Args, Body, Context, MaybeSeqNum, Status, PredOrFunc, Arity,
        GoalType, Goal, VarSet, TVarSet, !ClausesInfo, QuantWarnings,
        !ModuleInfo, !QualInfo, !Specs) :-
    !.ClausesInfo = clauses_info(VarSet0, ExplicitVarTypes0,
        TVarNameMap0, InferredVarTypes, HeadVars, ClausesRep0, ItemNumbers0,
        RttiVarMaps, HasForeignClauses),
    IsEmpty = clause_list_is_empty(ClausesRep0),
    (
        IsEmpty = yes,
        % Create the mapping from type variable name, used to rename
        % type variables occurring in explicit type qualifications.
        % The version of this mapping stored in the clauses_info should
        % only contain type variables which occur in the argument types
        % of the predicate. Type variables which only occur in explicit type
        % qualifications are local to the clause in which they appear.
        varset.create_name_var_map(TVarSet0, TVarNameMap)
    ;
        IsEmpty = no,
        TVarNameMap = TVarNameMap0
    ),
    update_qual_info(TVarNameMap, TVarSet0, ExplicitVarTypes0, Status,
        !QualInfo),
    varset.merge_renaming(VarSet0, CVarSet, VarSet1, Renaming),
    add_clause_transform(Renaming, HeadVars, Args, Body, Context, PredOrFunc,
        Arity, GoalType, Goal0, VarSet1, VarSet,
        QuantWarnings, StateVarWarnings, StateVarErrors,
        !ModuleInfo, !QualInfo, !Specs),
    qual_info_get_tvarset(!.QualInfo, TVarSet),
    qual_info_get_found_syntax_error(!.QualInfo, FoundError),
    qual_info_set_found_syntax_error(no, !QualInfo),
    (
        ( FoundError = yes
        ; StateVarErrors = [_ | _]
        )
    ->
        % Don't insert clauses containing syntax errors into the clauses_info,
        % because doing that would cause typecheck.m to report spurious type
        % errors. Don't report singleton variable warnings if there were
        % syntax errors.
        !:Specs = StateVarErrors ++ !.Specs,
        Goal = true_goal
    ;
        Goal = Goal0,

        % If we have foreign clauses, we should only add this clause
        % for modes *not* covered by the foreign clauses.
        (
            HasForeignClauses = yes,
            get_clause_list_any_order(ClausesRep0, AnyOrderClauseList),
            ForeignModeIds = list.condense(list.filter_map(
                (func(C) = ProcIds is semidet :-
                    C ^ clause_lang = impl_lang_foreign(_),
                    ApplProcIds = C ^ clause_applicable_procs,
                    (
                        ApplProcIds = all_modes,
                        unexpected($module, $pred, "all_modes foreign_proc")
                    ;
                        ApplProcIds = selected_modes(ProcIds)
                    )
                ),
                AnyOrderClauseList)),
            (
                ApplModeIds0 = all_modes,
                ModeIds0 = AllModeIds
            ;
                ApplModeIds0 = selected_modes(ModeIds0)
            ),
            ModeIds = list.delete_elems(ModeIds0, ForeignModeIds),
            (
                ModeIds = [],
                ClausesRep = ClausesRep0
            ;
                ModeIds = [_ | _],
                ApplicableModeIds = selected_modes(ModeIds),
                Clause = clause(ApplicableModeIds, Goal, impl_lang_mercury,
                    Context, StateVarWarnings),
                add_clause(Clause, ClausesRep0, ClausesRep)
            )
        ;
            HasForeignClauses = no,
            Clause = clause(ApplModeIds0, Goal, impl_lang_mercury, Context,
                StateVarWarnings),
            add_clause(Clause, ClausesRep0, ClausesRep)
        ),
        qual_info_get_var_types(!.QualInfo, ExplicitVarTypes),
        add_clause_item_number(MaybeSeqNum, Context, item_is_clause,
            ItemNumbers0, ItemNumbers),
        !:ClausesInfo = clauses_info(VarSet, ExplicitVarTypes, TVarNameMap,
            InferredVarTypes, HeadVars, ClausesRep, ItemNumbers,
            RttiVarMaps, HasForeignClauses)
    ).

    % Args0 has already had !S arguments replaced by a !.S, !:S argument pair.
    %
:- pred add_clause_transform(prog_var_renaming::in,
    proc_arg_vector(prog_var)::in, list(prog_term)::in, goal::in,
    prog_context::in, pred_or_func::in, arity::in, goal_type::in,
    hlds_goal::out, prog_varset::in, prog_varset::out,
    list(quant_warning)::out, list(error_spec)::out, list(error_spec)::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_clause_transform(Renaming, HeadVars, Args0, ParseBody, Context, PredOrFunc,
        Arity, GoalType, Goal, !VarSet, QuantWarnings, StateVarWarnings,
        StateVarErrors, !ModuleInfo, !QualInfo, !Specs) :-
    some [!SInfo, !SVarState, !SVarStore] (
        HeadVarList = proc_arg_vector_to_list(HeadVars),
        rename_vars_in_term_list(need_not_rename, Renaming, Args0, Args1),
        svar_prepare_for_clause_head(Args1, Args, !VarSet, FinalSVarMap,
            !:SVarState, !:SVarStore, !Specs),
        InitialSVarState = !.SVarState,
        ( GoalType = goal_type_promise(_) ->
            HeadGoal = true_goal
        ;
            ArgContext = ac_head(PredOrFunc, Arity),
            HeadGoal0 = true_goal,
            insert_arg_unifications(HeadVarList, Args, Context, ArgContext,
                HeadGoal0, HeadGoal1, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            % The only pass that pays attention to the from_head feature,
            % switch_detection, only does so on kinds of hlds_goal_exprs
            % that do not occur in from_ground_term scopes, which we have
            % just marked as from_ground_term_construct. However, later
            % passes may convert some of the unifications inside these scopes
            % to calls, and switch detection *does* care about from_head
            % on calls, and it looks inside all scopes except those of the
            % from_ground_term_construct kind. Therefore any code that can be
            % executed between now and switch detection that converts a
            % from_ground_term_construct scope to another kind of scope
            % should attach any from_head feature present on the scope
            % to all its subgoals.
            attach_features_to_all_goals([feature_from_head],
                do_not_attach_in_from_ground_term, HeadGoal1, HeadGoal)
        ),
        transform_goal_expr_context_to_goal(loc_whole_goal, ParseBody,
            Renaming, BodyGoal, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),

        trace [compiletime(flag("debug-statevar-lambda")), io(!IO)] (
            io.write_string("\nCLAUSE HEAD\n", !IO),
            io.write_string("args before:\n", !IO),
            io.write_list(Args0, "\n", io.write, !IO),
            io.nl(!IO),
            io.write_string("args renamed:\n", !IO),
            io.write_list(Args1, "\n", io.write, !IO),
            io.nl(!IO),
            io.write_string("args after:\n", !IO),
            io.write_list(Args, "\n", io.write, !IO),
            io.nl(!IO),
            io.write_string("head vars:\n", !IO),
            io.write(HeadVarList, !IO),
            io.nl(!IO),
            io.write_string("arg unifies:\n", !IO),
            dump_goal(!.ModuleInfo, !.VarSet, HeadGoal, !IO),
            io.nl(!IO),
            io.write_string("clause body:\n", !IO),
            dump_goal(!.ModuleInfo, !.VarSet, BodyGoal, !IO),
            io.nl(!IO),
            some [FinalSVarList] (
                map.to_assoc_list(FinalSVarMap, FinalSVarList),
                io.write_string("FinalSVarMap:\n", !IO),
                io.write(FinalSVarList, !IO),
                io.nl(!IO)
            )
        ),

        FinalSVarState = !.SVarState,
        svar_finish_clause_body(Context, FinalSVarMap,
            [HeadGoal, BodyGoal], Goal0, InitialSVarState, FinalSVarState,
            !.SVarStore, StateVarWarnings, StateVarErrors),

        qual_info_get_var_types(!.QualInfo, VarTypes0),
        % The RTTI varmaps here are just a dummy value, because the real ones
        % are not introduced until polymorphism.
        rtti_varmaps_init(EmptyRttiVarmaps),
        % XXX It should be possible to exploit the fact that lambda expressions
        % are not yet recognized as such inside from_ground_term scopes.
        implicitly_quantify_clause_body_general(
            ordinary_nonlocals_maybe_lambda,
            HeadVarList, QuantWarnings, Goal0, Goal,
            !VarSet, VarTypes0, VarTypes, EmptyRttiVarmaps, _),
        qual_info_set_var_types(VarTypes, !QualInfo)
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_clause.
%-----------------------------------------------------------------------------%
