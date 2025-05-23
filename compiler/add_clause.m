%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_clause.
:- interface.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type clause_type
    --->    clause_not_for_promise
    ;       clause_for_promise(promise_type).

:- pred module_add_clause(io.text_output_stream::in, pred_status::in,
    clause_type::in, item_clause_info::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % This predicate is exported for use by instance_method_clauses.m.
    %
:- pred add_clause_to_clauses_info(clause_applicable_modes::in,
    list(proc_id)::in, pred_status::in, clause_type::in,
    pred_or_func::in, sym_name::in, list(prog_term)::in,
    prog_context::in, item_seq_num::in, goal::in, prog_varset::in,
    tvarset::in, tvarset::out, clauses_info::in, clauses_info::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.default_func_mode.
:- import_module hlds.goal_transform.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_goal.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.make_hlds.goal_expr_to_goal.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds.state_var.
:- import_module hlds.make_hlds.superhomogeneous.
:- import_module hlds.make_hlds_error.
:- import_module hlds.pre_quantification.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.module_qual.id_set.
:- import_module parse_tree.module_qual.qualify_items.
:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_tree_out_clause.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

module_add_clause(ProgressStream, PredStatus, ClauseType, ClauseInfo,
        !ModuleInfo, !QualInfo, !Specs) :-
    ClauseInfo = item_clause_info(PredOrFunc, PredSymName, ArgTerms0,
        ClauseVarSet, MaybeBodyGoal, Context, SeqNum),
    (
        PredSymName = qualified(PredModuleName, PredName)
    ;
        PredSymName = unqualified(_),
        % XXX The item_clause_info should encode this invariant, either
        % by recording PredModuleName and PredName separately, or by using
        % a qualified-only subtype of SymName.
        unexpected($pred, "PredSymName is unqualified")
    ),
    ( if
        illegal_state_var_func_result(PredOrFunc, ArgTerms0, SVar, SVarCtxt)
    then
        IllegalSVarResult = yes({SVar, SVarCtxt})
    else
        IllegalSVarResult = no
    ),
    expand_bang_state_pairs_in_terms(ArgTerms0, ArgTerms),

    % Lookup the pred declaration in the predicate table.
    % (If it is not there, call maybe_undefined_pred_error and insert
    % an implicit declaration for the predicate.)
    list.length(ArgTerms, Arity0),
    ( IllegalSVarResult = yes(_), Arity = Arity0 - 1
    ; IllegalSVarResult = no,     Arity = Arity0
    ),
    PredFormArity = pred_form_arity(Arity),
    some [!PredInfo] (
        module_info_get_predicate_table(!.ModuleInfo, PredicateTable),
        predicate_table_lookup_pf_sym_arity(PredicateTable, is_fully_qualified,
            PredOrFunc, PredSymName, PredFormArity, PredIds),
        ( if PredIds = [PredId] then
            add_clause_to_hlds(ProgressStream, PredStatus, ClauseType, PredId,
                PredOrFunc, PredSymName, ArgTerms, PredFormArity,
                ClauseVarSet, MaybeBodyGoal, Context, SeqNum,
                IllegalSVarResult, !ModuleInfo, !QualInfo, !Specs)
        else if PredName = ",", Arity = 2 then
            SNA = sym_name_arity(unqualified(","), 2),
            Pieces =
                [words("Error: attempt to define a clause for")] ++
                color_as_subject([unqual_sym_name_arity(SNA), suffix(".")]) ++
                [nl,
                words("This is usually caused by")] ++
                color_as_incorrect([words("inadvertently writing"),
                    words("a period instead of a comma")]) ++
                [words("at the end of the preceding line."), nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        else
            % A promise will not have a corresponding pred declaration.
            (
                ClauseType = clause_for_promise(_PromiseType),
                % add_promise in make_hlds_passes.m should have declared
                % this predicate before calling us to add this clause.
                unexpected($pred, "clause for undeclared promise")
            ;
                ClauseType = clause_not_for_promise,
                user_arity_pred_form_arity(PredOrFunc, UserArity,
                    PredFormArity),
                Origin = origin_user(user_made_pred(PredOrFunc,
                    PredSymName, UserArity)),
                add_implicit_pred_decl_report_error(PredOrFunc,
                    PredModuleName, PredName, PredFormArity, PredStatus,
                    is_not_a_class_method, Context, Origin,
                    [words("clause")], PredId, !ModuleInfo, !Specs)
            ),
            add_clause_to_hlds(ProgressStream, PredStatus, ClauseType, PredId,
                PredOrFunc, PredSymName, ArgTerms, PredFormArity,
                ClauseVarSet, MaybeBodyGoal, Context, SeqNum,
                IllegalSVarResult, !ModuleInfo, !QualInfo, !Specs)
        )
    ).

:- pred add_clause_to_hlds(io.text_output_stream::in,
    pred_status::in, clause_type::in, pred_id::in,
    pred_or_func::in, sym_name::in, list(prog_term)::in, pred_form_arity::in,
    prog_varset::in, maybe2(goal, list(warning_spec))::in, prog_context::in,
    item_seq_num::in, maybe({prog_var, prog_context})::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_clause_to_hlds(ProgressStream, PredStatus, ClauseType, PredId,
        PredOrFunc, PredSymName, MaybeAnnotatedArgTerms, PredFormArity,
        ClauseVarSet, MaybeBodyGoal, Context, SeqNum, IllegalSVarResult,
        !ModuleInfo, !QualInfo, !Specs) :-
    some [!PredInfo, !PredSpecs] (
        % Lookup the pred_info for this pred, add the clause to the
        % clauses_info in the pred_info, if there are no modes add an
        % `infer_modes' marker, and then save the pred_info.
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),

        trace [io(!IO)] (
            add_clause_progress_msg(ProgressStream, !.ModuleInfo, !.PredInfo,
                PredOrFunc, PredSymName, PredFormArity, !IO)
        ),

        % Opt_imported preds are initially tagged as imported, and are tagged
        % as opt_imported only if/when we see a clause for them.
        ( if PredStatus = pred_status(status_opt_imported) then
            pred_info_set_status(pred_status(status_opt_imported), !PredInfo),
            pred_info_get_markers(!.PredInfo, Markers0),
            add_marker(marker_calls_are_fully_qualified, Markers0, Markers),
            pred_info_set_markers(Markers, !PredInfo)
        else
            true
        ),

        !:PredSpecs = [],
        (
            IllegalSVarResult = yes({StateVar, StateVarContext}),
            ResultSpec = report_illegal_func_svar_result_raw(StateVarContext,
                ClauseVarSet, StateVar),
            !:PredSpecs = [ResultSpec | !.PredSpecs]
        ;
            IllegalSVarResult = no
        ),
        maybe_add_error_for_field_access_function(!.ModuleInfo, PredStatus,
            PredOrFunc, PredSymName, PredFormArity, Context, !PredSpecs),
        maybe_add_error_for_builtin(!.ModuleInfo, !.PredInfo,
            Context, !PredSpecs),
        maybe_add_default_func_mode(!.ModuleInfo, !PredInfo, _),
        (
            !.PredSpecs = [_ | _ ],
            !:Specs = !.PredSpecs ++
                get_any_errors_warnings2(MaybeBodyGoal) ++ !.Specs
        ;
            !.PredSpecs = [],
            (
                MaybeBodyGoal = error2(BodyGoalSpecs),
                !:Specs = BodyGoalSpecs ++ !.Specs,
                pred_info_get_clauses_info(!.PredInfo, Clauses0),
                Clauses = Clauses0 ^ cli_had_syntax_errors :=
                    some_clause_syntax_errors,
                pred_info_set_clauses_info(Clauses, !PredInfo)
            ;
                MaybeBodyGoal = ok2(BodyGoal, BodyGoalWarningSpecs),
                !:Specs = BodyGoalWarningSpecs ++ !.Specs,
                pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
                pred_info_get_typevarset(!.PredInfo, TVarSet0),
                select_applicable_modes(!.ModuleInfo, MaybeAnnotatedArgTerms,
                    ClauseVarSet, PredStatus, Context, PredId, !.PredInfo,
                    ArgTerms, ProcIdsForThisClause, AllProcIds,
                    !QualInfo, !Specs),
                add_clause_to_clauses_info(ProcIdsForThisClause, AllProcIds,
                    PredStatus, ClauseType, PredOrFunc, PredSymName,
                    ArgTerms, Context, SeqNum, BodyGoal, ClauseVarSet,
                    TVarSet0, TVarSet, ClausesInfo0, ClausesInfo,
                    !ModuleInfo, !QualInfo, !Specs),
                pred_info_set_clauses_info(ClausesInfo, !PredInfo),
                (
                    ClauseType = clause_for_promise(_PromiseType)
                    % We have already set the goal type.
                ;
                    ClauseType = clause_not_for_promise,
                    % We normally add all Mercury clauses before we add
                    % any foreign_procs, but just in case that changes
                    % in the future ...
                    pred_info_update_goal_type(np_goal_type_clause, !PredInfo)
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
                ( if
                    ProcIds = [],
                    ClauseType = clause_not_for_promise
                then
                    pred_info_get_markers(!.PredInfo, EndMarkers0),
                    add_marker(marker_infer_modes, EndMarkers0, EndMarkers),
                    pred_info_set_markers(EndMarkers, !PredInfo)
                else
                    true
                )
            ),
            module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo)
        )
    ).

%-----------------%

:- pred add_clause_progress_msg(io.text_output_stream::in, module_info::in,
    pred_info::in, pred_or_func::in, sym_name::in, pred_form_arity::in,
    io::di, io::uo) is det.

add_clause_progress_msg(ProgressStream, ModuleInfo, PredInfo,
        PredOrFunc, PredName, PredFormArity, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        pred_info_get_clauses_info(PredInfo, MsgClauses),
        NumClauses = num_clauses_in_clauses_rep(MsgClauses ^ cli_rep),
        PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
        user_arity_pred_form_arity(PredOrFunc, user_arity(Arity),
            PredFormArity),
        SNA = sym_name_arity(PredName, Arity),
        SNAStr = unescaped_sym_name_arity_to_string(SNA),
        io.format(ProgressStream, "%% Processing clause %d for %s `%s'...\n",
            [i(NumClauses + 1), s(PredOrFuncStr), s(SNAStr)], !IO)
    ;
        VeryVerbose = no
    ).

%-----------------%

:- pred maybe_add_error_for_field_access_function(module_info::in,
    pred_status::in, pred_or_func::in, sym_name::in, pred_form_arity::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

maybe_add_error_for_field_access_function(ModuleInfo, PredStatus,
        PredOrFunc, PredSymName, PredFormArity, Context, !Specs) :-
    ( if
        % User-supplied clauses for field access functions are not
        % allowed -- the clauses are always generated by the compiler.
        PredOrFunc = pf_function,
        user_arity_pred_form_arity(PredOrFunc,
            user_arity(UserArityInt), PredFormArity),
        is_field_access_function_name(ModuleInfo, PredSymName,
            UserArityInt, _, _),
        % Don't report errors for clauses for field access function clauses
        % in `.opt' files.
        PredStatus \= pred_status(status_opt_imported)
    then
        SNA = sym_name_arity(PredSymName, UserArityInt),
        FieldAccessMainPieces = [words("Error:")] ++
            color_as_incorrect([words("clause for automatically generated"),
                words("field access function")]) ++
            color_as_subject([unqual_sym_name_arity(SNA), suffix(".")]) ++
            [nl],
        FieldAccessVerbosePieces =
            [words("Clauses for field access functions"),
            words("are automatically generated by the compiler."),
            words("To supply your own definition for a field access"),
            words("function, for example to check the input"),
            words("to a field update, give the field"),
            words("of the constructor a different name."), nl],
        FieldAccessMsg = simple_msg(Context,
            [always(FieldAccessMainPieces),
            verbose_only(verbose_always, FieldAccessVerbosePieces)]),
        FieldAccessSpec = error_spec($pred, severity_error, phase_pt2h,
            [FieldAccessMsg]),
        !:Specs = [FieldAccessSpec | !.Specs]
    else
        true
    ).

:- pred maybe_add_error_for_builtin(module_info::in, pred_info::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

maybe_add_error_for_builtin(ModuleInfo, PredInfo, Context, !Specs) :-
    ( if pred_info_is_builtin(PredInfo) then
        % When bootstrapping a change that defines a builtin using
        % normal Mercury code, we need to disable the generation
        % of the error message, and just ignore the definition.
        some [Globals] (
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, allow_defn_of_builtins,
                AllowDefnOfBuiltin)
        ),
        (
            AllowDefnOfBuiltin = no,
            pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
            Pieces = [words("Error: clause for a builtin")] ++
                color_as_incorrect([p_or_f(PredOrFunc), suffix(".")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        ;
            AllowDefnOfBuiltin = yes
        )
    else
        true
    ).

%-----------------%

    % Extract the mode annotations (if any) from the clause arguments,
    % and determine which mode(s) this clause should apply to.
    %
:- pred select_applicable_modes(module_info::in, list(prog_term)::in,
    prog_varset::in, pred_status::in, prog_context::in,
    pred_id::in, pred_info::in, list(prog_term)::out,
    clause_applicable_modes::out, list(proc_id)::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

select_applicable_modes(ModuleInfo, MaybeAnnotatedArgTerms, VarSet,
        PredStatus, Context, PredId, PredInfo, ArgTerms,
        ApplProcIds, AllProcIds, !QualInfo, !Specs) :-
    AllProcIds = pred_info_all_procids(PredInfo),
    PredIdStr = pred_id_to_user_string(ModuleInfo, PredId),
    ContextPieces = cord.from_list([words("In the head of a clause for"),
        fixed(PredIdStr), suffix(":"), nl]),
    get_mode_annotations(VarSet, ContextPieces, 1, MaybeAnnotatedArgTerms,
        ArgTerms, ArgModes0, ArgsWithoutModes, ModeAnnotationSpecs),
    (
        ModeAnnotationSpecs = [_ | _],
        !:Specs = ModeAnnotationSpecs ++ !.Specs,
        % Apply the clause to all modes.
        % XXX Would it be better to apply it to none?
        ApplProcIds = selected_modes(AllProcIds)
    ;
        ModeAnnotationSpecs = [],
        (
            ArgModes0 = [],
            ( if pred_info_defn_has_foreign_proc(PredInfo) then
                % We are only allowed to mix foreign procs and
                % mode specific clauses, so make this clause
                % mode specific but apply to all modes.
                ApplProcIds = selected_modes(AllProcIds)
            else
                ApplProcIds = all_modes
            )
        ;
            ArgModes0 = [_ | _],
            ArgsWithoutModes = [],

            % The user specified some mode annotations on this clause.
            % First module-qualify the mode annotations. The annotations on
            % clauses from `.opt' files will already be fully module qualified.

            ( if PredStatus = pred_status(status_opt_imported) then
                ArgModes = ArgModes0
            else
                Exported =
                    pred_status_is_exported_to_non_submodules(PredStatus),
                (
                    Exported = yes,
                    InInt = mq_used_in_interface
                ;
                    Exported = no,
                    InInt = mq_not_used_in_interface
                ),
                qual_info_get_mq_info(!.QualInfo, MQInfo0),
                qualify_clause_mode_list(InInt, Context,
                    ArgModes0, ArgModes, MQInfo0, MQInfo, !Specs),
                qual_info_set_mq_info(MQInfo, !QualInfo)
            ),

            % Now find the procedure which matches these mode annotations.
            pred_info_get_proc_table(PredInfo, Procs),
            map.to_assoc_list(Procs, ExistingProcs),
            ( if
                get_procedure_matching_declmodes_with_renaming(ModuleInfo,
                    ExistingProcs, ArgModes, ProcId)
            then
                ApplProcIds = selected_modes([ProcId]),
                (
                    ExistingProcs = []
                    % A mode-specific clause for a predicate/function
                    % that has no modes is a bug (in the usual case where
                    % mode inference is not turned on), but it is a bug that
                    % should be reported elsewhere.
                ;
                    ExistingProcs = [_],
                    module_info_get_globals(ModuleInfo, Globals),
                    globals.lookup_bool_option(Globals,
                        warn_unneeded_mode_specific_clause, Warn),
                    (
                        Warn = yes,
                        PredDescPieces = describe_one_pred_info_name(
                            yes(color_subject), should_not_module_qualify,
                            [], PredInfo),
                        Pieces = [words("Warning: the")] ++ PredDescPieces ++
                            [words("has only one mode,"),
                            words("so there is")] ++
                            color_as_incorrect([words("no need to restrict"),
                                words("a clause for it to that mode.")]) ++
                            [nl],
                        Spec = spec($pred, severity_warning, phase_pt2h,
                            Context, Pieces),
                        !:Specs = [Spec | !.Specs]
                    ;
                        Warn = no
                    )
                ;
                    ExistingProcs = [_, _ | _]
                )
            else
                report_undeclared_mode_error(ModuleInfo, PredId, PredInfo,
                    VarSet, ArgModes, [words("clause")], Context, !Specs),
                % Apply the clause to all modes.
                % XXX Would it be better to apply it to none?
                ApplProcIds = selected_modes(AllProcIds)
            )
        ;
            ArgModes0 = [_ | _],
            ArgsWithoutModes = [_ | _],
            ArgsWithoutModePieces =
                list.map((func(N) = nth_fixed(N)), ArgsWithoutModes),
            Pieces = [words("In the head of a clause for"),
                fixed(PredIdStr), suffix(":"), nl,
                words("syntax error:")] ++
                color_as_incorrect([words("some but not all arguments"),
                    words("have mode annotations.")]) ++
                [words(choose_number(ArgsWithoutModes,
                    "The argument without a mode is the",
                    "The arguments without modes are the"))] ++
                piece_list_to_color_pieces(color_incorrect, "and",
                    [suffix(".")], ArgsWithoutModePieces) ++
                [nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
            !:Specs = [Spec | !.Specs],

            % Apply the clause to all modes.
            % XXX Would it be better to apply it to none?
            ApplProcIds = selected_modes(AllProcIds)
        )
    ).

%-----------------%

    % Extract the mode annotations (if any) from a list of arguments.
    %
:- pred get_mode_annotations(prog_varset::in, cord(format_piece)::in,
    int::in, list(prog_term)::in, list(prog_term)::out,
    list(mer_mode)::out, list(int)::out, list(error_spec)::out) is det.

get_mode_annotations(_, _, _, [], [], [], [], []).
get_mode_annotations(VarSet, ContextPieces, ArgNum, [MAArgTerm | MAArgTerms],
        [ArgTerm | ArgTerms], ArgModes, ArgsWithoutModes, Specs) :-
    get_mode_annotations(VarSet, ContextPieces, ArgNum + 1, MAArgTerms,
        ArgTerms, TailArgModes, TailArgsWithoutModes, TailSpecs),
    get_mode_annotation(VarSet, ContextPieces, ArgNum, MAArgTerm, ArgTerm,
        MaybeMaybeMode),
    (
        MaybeMaybeMode = ok1(MaybeMode),
        (
            MaybeMode = no,
            ArgModes = TailArgModes,
            ArgsWithoutModes = [ArgNum | TailArgsWithoutModes],
            Specs = TailSpecs
        ;
            MaybeMode = yes(ArgMode),
            ArgModes = [ArgMode | TailArgModes],
            ArgsWithoutModes = TailArgsWithoutModes,
            Specs = TailSpecs
        )
    ;
        MaybeMaybeMode = error1(MaybeModeSpecs),
        ArgModes = TailArgModes,
        ArgsWithoutModes = TailArgsWithoutModes,
        Specs = MaybeModeSpecs ++ TailSpecs
    ).

    % Extract the mode annotations (if any) from a single argument.
    %
:- pred get_mode_annotation(prog_varset::in, cord(format_piece)::in,
    int::in, prog_term::in, prog_term::out, maybe1(maybe(mer_mode))::out)
    is det.

get_mode_annotation(VarSet, ContextPieces, ArgNum, MaybeAnnotatedArgTerm,
        ArgTerm, MaybeMaybeAnnotation) :-
    ( if
        MaybeAnnotatedArgTerm = term.functor(term.atom("::"),
            [ArgTermPrime, ModeTerm], _)
    then
        ArgTerm = ArgTermPrime,

        ArgContextPieces = ContextPieces ++
            cord.from_list([words("in the"), nth_fixed(ArgNum),
            words("argument:"), nl]),
        varset.coerce(VarSet, GenVarSet),
        term.coerce(ModeTerm, GenModeTerm),
        parse_mode(allow_constrained_inst_var, GenVarSet, ArgContextPieces,
            GenModeTerm, MaybeMode),
        (
            MaybeMode = ok1(Mode),
            MaybeMaybeAnnotation = ok1(yes(Mode))
        ;
            MaybeMode = error1(Specs),
            MaybeMaybeAnnotation = error1(Specs)
        )
    else
        ArgTerm = MaybeAnnotatedArgTerm,
        MaybeMaybeAnnotation = ok1(no)
    ).

%-----------------------------------------------------------------------------%

add_clause_to_clauses_info(ApplModeIds0, AllModeIds, PredStatus, ClauseType,
        PredOrFunc, PredSymName, ArgTerms, Context, SeqNum, BodyGoal,
        ClauseVarSet, TVarSet0, TVarSet,
        !ClausesInfo, !ModuleInfo, !QualInfo, !Specs) :-
    !.ClausesInfo = clauses_info(VarSet0, ExplicitVarTypes0,
        VarTable0, RttiVarMaps0, TVarNameMap0, ArgVector, ClausesRep0,
        ItemNumbers0, HasForeignClauses0, HadSyntaxError0),
    HeadVars = proc_arg_vector_to_list(ArgVector),
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
    ( if PredStatus = pred_status(status_opt_imported) then
        MaybeOptImported = is_opt_imported
    else
        MaybeOptImported = is_not_opt_imported
    ),
    update_qual_info(TVarNameMap, TVarSet0, ExplicitVarTypes0,
        MaybeOptImported, !QualInfo),
    varset.merge_renaming(VarSet0, ClauseVarSet, VarSet1, Renaming),
    should_we_do_singleton_and_quant_warnings(!.ModuleInfo, PredStatus,
        !.ClausesInfo, ShouldWarn),
    % We need to keep quantified variables temporarily for use by the code
    % that warns about singletons, and then we want to delete those quantified
    % variables. If we won't try to generate any singleton variable warnings,
    % then there is no point in keeping those quantified variables.
    ( ShouldWarn = should_not_warn, KeepQuantVars = do_not_keep_quant_vars
    ; ShouldWarn = should_warn,     KeepQuantVars = keep_quant_vars
    ),
    % We want to find out if *this* clause has a syntax error, so forget
    % about any errors in previous clauses.
    qual_info_set_found_syntax_error(no, !QualInfo),
    add_clause_transform(KeepQuantVars, Renaming, PredOrFunc, PredSymName,
        HeadVars, ArgTerms, Context, ClauseType, BodyGoal, Goal0,
        VarSet1, VarSet2, QuantWarnings, StateVarWarnings, UnusedSVarArgMap,
        !ModuleInfo, !QualInfo, !Specs),
    qual_info_get_tvarset(!.QualInfo, TVarSet),
    qual_info_get_found_syntax_error(!.QualInfo, FoundSyntaxError),
    (
        FoundSyntaxError = yes,
        % Don't insert clauses containing syntax errors into the
        % clauses_info, because doing that would cause typecheck.m
        % to report spurious type errors. Don't report singleton variable
        % warnings if there were syntax errors.
        !ClausesInfo ^ cli_had_syntax_errors := some_clause_syntax_errors
    ;
        FoundSyntaxError = no,
        (
            ShouldWarn = should_not_warn,
            VarSet = VarSet2,
            Goal = Goal0
        ;
            ShouldWarn = should_warn,
            PredFormArity = arg_list_arity(HeadVars),
            WarnPFSymNameArity = pf_sym_name_arity(PredOrFunc, PredSymName,
                PredFormArity),
            warn_singletons_in_clause_body(!.ModuleInfo, WarnPFSymNameArity,
                VarSet2, Goal0, SeenQuant, !Specs),
            % Warn about variables with overlapping scopes.
            add_quant_warnings(!.ModuleInfo, WarnPFSymNameArity, VarSet,
                QuantWarnings, !Specs),
            (
                SeenQuant = have_not_seen_quant,
                % Even though we told the call to add_clause_transform above
                % to tell quantification.m to keep any quantified variables,
                % there were none to keep, so there is no point in trying
                % to delete them.
                VarSet = VarSet2,
                Goal = Goal0
            ;
                SeenQuant = have_seen_quant,
                % There *were* some to keep, so delete them.
                qual_info_get_explicit_var_types(!.QualInfo, QuantVarTypes0),
                rtti_varmaps_init(EmptyRttiVarmaps),
                implicitly_quantify_clause_body_general_vs(ord_nl_maybe_lambda,
                    do_not_keep_quant_vars, HeadVars, _QuantWarnings,
                    Goal0, Goal, VarSet2, VarSet,
                    QuantVarTypes0, QuantVarTypes, EmptyRttiVarmaps, _),
                qual_info_set_explicit_var_types(QuantVarTypes, !QualInfo)
            )
        ),
        ( if BodyGoal = true_expr(_) then
            MaybeFact = clause_is_a_fact
        else
            MaybeFact = clause_is_not_a_fact
        ),
        % If we have foreign clauses, we should only add this clause
        % for modes *not* covered by the foreign clauses.
        (
            HasForeignClauses0 = some_foreign_lang_clauses,
            get_clause_list(Clauses0, ClausesRep0, ClausesRep1),
            ForeignModeIds = list.condense(list.filter_map(
                ( func(C) = ProcIds is semidet :-
                    C ^ clause_lang = impl_lang_foreign(_),
                    ApplProcIds = C ^ clause_applicable_procs,
                    (
                        ApplProcIds = all_modes,
                        unexpected($pred, "all_modes foreign_proc")
                    ;
                        ApplProcIds = selected_modes(ProcIds)
                    ;
                        ( ApplProcIds = unify_in_in_modes
                        ; ApplProcIds = unify_non_in_in_modes
                        ),
                        unexpected($pred, "unify modes for foreign_proc")
                    )
                ),
                Clauses0)),
            (
                ApplModeIds0 = all_modes,
                ModeIds0 = AllModeIds
            ;
                ApplModeIds0 = selected_modes(ModeIds0)
            ;
                ( ApplModeIds0 = unify_in_in_modes
                ; ApplModeIds0 = unify_non_in_in_modes
                ),
                unexpected($pred, "unify modes for user defined predicate")
            ),
            ModeIds = list.delete_elems(ModeIds0, ForeignModeIds),
            (
                ModeIds = [],
                ClausesRep = ClausesRep1
            ;
                ModeIds = [_ | _],
                ApplicableModeIds = selected_modes(ModeIds),
                Clause = clause(ApplicableModeIds, Goal, impl_lang_mercury,
                    Context, StateVarWarnings, UnusedSVarArgMap, MaybeFact),
                add_clause(Clause, ClausesRep1, ClausesRep)
            )
        ;
            HasForeignClauses0 = no_foreign_lang_clauses,
            Clause = clause(ApplModeIds0, Goal, impl_lang_mercury, Context,
                StateVarWarnings, UnusedSVarArgMap, MaybeFact),
            add_clause(Clause, ClausesRep0, ClausesRep)
        ),
        qual_info_get_explicit_var_types(!.QualInfo, ExplicitVarTypes),
        add_clause_item_number(SeqNum, Context, item_is_clause,
            ItemNumbers0, ItemNumbers),
        !:ClausesInfo = clauses_info(VarSet, ExplicitVarTypes,
            VarTable0, RttiVarMaps0, TVarNameMap, ArgVector, ClausesRep,
            ItemNumbers, HasForeignClauses0, HadSyntaxError0)
    ).

%-----------------%

:- type maybe_should_warn
    --->    should_not_warn
    ;       should_warn.

:- pred should_we_do_singleton_and_quant_warnings(module_info::in,
    pred_status::in, clauses_info::in, maybe_should_warn::out) is det.

should_we_do_singleton_and_quant_warnings(ModuleInfo, PredStatus, ClausesInfo,
        ShouldWarn) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, warn_singleton_vars, WarnSingleton),
    globals.lookup_bool_option(Globals, warn_repeated_singleton_vars,
        WarnRepeatedSingleton),
    ( if
        (
            % Neither kind of singleton warning is enabled,
            % so there is no point in looking for singletons.
            WarnSingleton = no,
            WarnRepeatedSingleton = no
        ;
            % Any singleton warnings should be generated for the original code,
            % not for the copy in a .opt file.
            PredStatus = pred_status(status_opt_imported)
        ;
            % Part of the parser's recovery from syntax errors (e.g. when
            % they occur in lambda expressions' clause heads) may have
            % included not translating parts of the original term
            % into the parsed clause body, so any singleton warnings
            % we generate for such "truncated" clauses could be misleading.
            %
            % We could try to record the set of variables in the parts
            % of the original goal term that we don't include in the clause,
            % but (a) this is not trivial to do, and (b) the payoff is
            % questionable, because some of those variables could have been
            % the result of typos affecting a word that the programmer meant
            % to be something else.
            ClausesInfo ^ cli_had_syntax_errors = some_clause_syntax_errors
        )
    then
        ShouldWarn = should_not_warn
    else
        ShouldWarn = should_warn
    ).

%-----------------%

    % ArgTerms0 has already had !S arguments replaced by
    % !.S, !:S argument pairs.
    %
:- pred add_clause_transform(maybe_keep_quant_vars::in, prog_var_renaming::in,
    pred_or_func::in, sym_name::in, list(prog_var)::in,
    list(prog_term)::in, prog_context::in, clause_type::in,
    goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    list(quant_warning)::out, list(error_spec)::out,
    unused_statevar_arg_map::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_clause_transform(KeepQuantVars, Renaming, PredOrFunc, PredSymName,
        HeadVars, ArgTerms0, Context, ClauseType, ParseTreeBodyGoal, Goal,
        !VarSet, QuantWarnings, StateVarWarningSpecs, UnusedSVarArgMap,
        !ModuleInfo, !QualInfo, !Specs) :-
    some [!SVarState, !UrInfo] (
        rename_vars_in_term_list(need_not_rename, Renaming,
            ArgTerms0, ArgTerms1),
        svar_prepare_for_clause_head(!.ModuleInfo, !.QualInfo, !.VarSet,
            ArgTerms1, ArgTerms, FinalSVarMap, NewSVars,
            !:SVarState, !:UrInfo),
        InitialSVarState = !.SVarState,
        (
            ClauseType = clause_for_promise(_),
            HeadUnificationsGoal = true_goal
        ;
            ClauseType = clause_not_for_promise,
            PredFormArity = arg_list_arity(ArgTerms0),
            ArgContext = ac_head(PredOrFunc, PredFormArity),
            HeadUnificationsGoal0 = true_goal,
            pair_vars_with_terms(HeadVars, ArgTerms, HeadVarsArgTerms),
            insert_arg_unifications(HeadVarsArgTerms, Context, ArgContext,
                HeadUnificationsGoal0, HeadUnificationsGoal1,
                !SVarState, !UrInfo),
            % The only pass that pays attention to the from_head feature,
            % switch_detection, only does so on kinds of hlds_goal_exprs
            % that do not occur in from_ground_term scopes, which we have
            % just marked as from_ground_term_initial. Those scopes will be
            % converted to one of from_ground_term_{construct,deconstruct,
            % other} by mode analysis, if type analysis hasn't done it first.
            % Type analysis will do this if it finds that some of the
            % "unifications" inside these scopes are actually calls.
            % Switch detection *does* care about from_head features on calls,
            % and it looks inside all scopes except those of the
            % from_ground_term_construct kind. Therefore any code that can be
            % executed between now and switch detection that converts a
            % from_ground_term_initial or from_ground_term_construct scope
            % to another kind of scope should attach any from_head feature
            % present on the scope to all its subgoals.
            attach_features_to_all_goals([feature_from_head],
                do_not_attach_in_from_ground_term,
                HeadUnificationsGoal1, HeadUnificationsGoal)
        ),
        transform_parse_tree_goal_to_hlds(loc_whole_goal, Renaming,
            ParseTreeBodyGoal, BodyGoal, !SVarState, !UrInfo),
        FinalSVarState = !.SVarState,
        svar_finish_clause_body(Context, NewSVars, FinalSVarMap,
            InitialSVarState, FinalSVarState, HeadUnificationsGoal, BodyGoal,
            Goal0, StateVarWarningSpecs, UnusedSVarArgMap, !UrInfo),
        !.UrInfo = unravel_info(!:ModuleInfo, _FgtThreshold,
            !:QualInfo, !:VarSet, _SVarStore, UnravelSpecs),
        !:Specs = UnravelSpecs ++ !.Specs,

        trace [compiletime(flag("debug-statevar-lambda")), io(!IO)] (
            module_info_get_globals(!.ModuleInfo, Globals),
            module_info_get_name(!.ModuleInfo, ModuleName),
            globals.lookup_string_option(Globals, experiment, Experiment),
            PredName = unqualify_name(PredSymName),
            ( if PredName = Experiment then
                VarNameSrc = vns_varset(!.VarSet),
                varset.init(TVarSet),
                varset.init(InstVarSet),
                get_debug_output_stream(Globals, ModuleName, DebugStream, !IO),
                io.write_string(DebugStream, "\nCLAUSE HEAD\n", !IO),
                io.write_string(DebugStream, "\narg terms before:\n", !IO),
                list.foldl(io.write_line(DebugStream), ArgTerms0, !IO),
                io.write_string(DebugStream, "\narg terms renamed:\n", !IO),
                list.foldl(io.write_line(DebugStream), ArgTerms1, !IO),
                io.write_string(DebugStream, "\narg terms after:\n", !IO),
                list.foldl(io.write_line(DebugStream), ArgTerms, !IO),
                io.write_string(DebugStream, "\nhead vars:\n", !IO),
                io.write_line(DebugStream, HeadVars, !IO),
                io.write_string(DebugStream, "\narg unifies:\n", !IO),
                dump_goal_nl(DebugStream, !.ModuleInfo, VarNameSrc,
                    TVarSet, InstVarSet, HeadUnificationsGoal, !IO),
                io.write_string(DebugStream, "\nparse tree goal body:\n", !IO),
                mercury_format_goal(DebugStream, !.VarSet, 0u,
                    ParseTreeBodyGoal, !IO),
                io.write_string(DebugStream, "\nclause body:\n", !IO),
                dump_goal_nl(DebugStream, !.ModuleInfo, VarNameSrc,
                    TVarSet, InstVarSet, BodyGoal, !IO),
                map.to_assoc_list(FinalSVarMap, FinalSVarList),
                io.write_string(DebugStream, "\nFinalSVarMap:\n", !IO),
                io.write_line(DebugStream, FinalSVarList, !IO)
            else
                true
            )
        ),

        qual_info_get_found_trace_goal(!.QualInfo, FoundTraceGoal),
        (
            FoundTraceGoal = no,
            Goal1 = Goal0
        ;
            FoundTraceGoal = yes,
            separate_trace_goal_only_locals(Goal0, Goal1)
        ),
        qual_info_get_explicit_var_types(!.QualInfo, VarTypes0),
        % The RTTI varmaps here are just a dummy value, because the real ones
        % are not introduced until polymorphism.
        rtti_varmaps_init(EmptyRttiVarmaps),
        % XXX It should be possible to exploit the fact that lambda expressions
        % are not yet recognized as such inside from_ground_term scopes.
        implicitly_quantify_clause_body_general_vs(ord_nl_maybe_lambda,
            KeepQuantVars, HeadVars, QuantWarnings, Goal1, Goal,
            !VarSet, VarTypes0, VarTypes, EmptyRttiVarmaps, _),
        qual_info_set_explicit_var_types(VarTypes, !QualInfo)
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_clause.
%-----------------------------------------------------------------------------%
