%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pragma_tabling.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.status.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.

:- pred module_add_pragma_tabled(io.text_output_stream::in,
    impl_pragma_tabled_info::in, item_mercury_status::in, pred_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module check_hlds.
:- import_module check_hlds.mode_test.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.add_foreign_proc.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.make_hlds_error.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.
:- import_module ll_backend.rtti_out.           % XXX undesirable dependency
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.
:- import_module ml_backend.mlds.               % XXX undesirable dependency
:- import_module ml_backend.mlds_to_c_name.     % XXX undesirable dependency
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

module_add_pragma_tabled(ProgressStream, TabledInfo,
        ItemMercuryStatus, PredStatus, !ModuleInfo, !QualInfo, !Specs) :-
    TabledInfo = impl_pragma_tabled_info(TabledMethod, PredOrProcSpec,
        MaybeAttributes, Context, _SeqNum),
    PredOrProcSpec = pred_or_proc_pfumm_name(PFUMM, PredSymName),
    (
        PredSymName = qualified(PredModuleName, PredName)
    ;
        PredSymName = unqualified(_),
        unexpected($pred, "unqualified PredSymName")
    ),
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    (
        (
            PFUMM = pfumm_predicate(ModesOrArity),
            PredOrFunc = pf_predicate
        ;
            PFUMM = pfumm_function(ModesOrArity),
            PredOrFunc = pf_function
        ),
        (
            ModesOrArity = moa_modes(Modes),
            % The arity needed by predicate_table_lookup_pf_sym_arity
            % includes the return type for functions.
            PredFormArity = arg_list_arity(Modes),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity)
        ;
            ModesOrArity = moa_arity(UserArity),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity)
        ),
        % Lookup the pred or func declaration in the predicate table.
        % If it is not there, print an error message and insert
        % a dummy declaration for it.
        predicate_table_search_pf_fqm_n_a(PredicateTable0, PredOrFunc,
            PredModuleName, PredName, PredFormArity, MaybePredId),
        (
            MaybePredId = no,
            Origin = origin_user(user_made_pred(PredOrFunc,
                PredSymName, UserArity)),
            TabledMethodStr = tabled_eval_method_to_string(TabledMethod),
            DescPieces = [pragma_decl(TabledMethodStr), words("declaration")],
            add_implicit_pred_decl_report_error(PredOrFunc,
                PredModuleName, PredName, PredFormArity, PredStatus,
                is_not_a_class_method, Context, Origin, DescPieces,
                PredId, !ModuleInfo, !Specs),
            PredIds = [PredId]
        ;
            MaybePredId = yes(PredId),
            PredIds = [PredId]
        )
    ;
        PFUMM = pfumm_unknown(UserArity),
        TabledMethodStr = tabled_eval_method_to_string(TabledMethod),
        maybe_warn_about_pfumm_unknown(!.ModuleInfo, TabledMethodStr, PFUMM,
            PredSymName, Context, !Specs),
        predicate_table_lookup_m_n_a(PredicateTable0, is_fully_qualified,
            PredModuleName, PredName, UserArity, PredIds0),
        (
            PredIds0 = [],
            % XXX The pragma does not say whether the user intends to table
            % a predicate or a function, so adding a predicate here is
            % only a guess.
            Origin = origin_user(user_made_pred(pf_predicate,
                PredSymName, UserArity)),
            DescPieces = [pragma_decl(TabledMethodStr), words("declaration")],
            user_arity_pred_form_arity(pf_predicate, UserArity, PredFormArity),
            add_implicit_pred_decl_report_error(pf_predicate,
                PredModuleName, PredName, PredFormArity, PredStatus,
                is_not_a_class_method, Context, Origin,
                DescPieces, PredId, !ModuleInfo, !Specs),
            PredIds = [PredId]
        ;
            PredIds0 = [_ | _],
            PredIds = PredIds0
        )
    ),
    (
        MaybeAttributes = yes(Attributes),
        Statistics = Attributes ^ table_attr_statistics,
        AllowReset = Attributes ^ table_attr_allow_reset,
        (
            PredIds = [_]
        ;
            PredIds = [_, _ | _],
            UserArity = user_arity(UserArityInt),
            SNA = sym_name_arity(PredSymName, UserArityInt),
            (
                Statistics = table_gather_statistics,
                StatsPieces = [words("Error:")] ++
                    color_as_incorrect([words("cannot request statistics")]) ++
                    [words("for the ambiguous name")] ++
                    color_as_subject([qual_sym_name_arity(SNA),
                        suffix(",")]) ++
                    [words("since the compiler-generated"),
                    words("statistics predicate"),
                    words("would have an ambiguous name too."), nl],
                StatsSpec = spec($pred, severity_error, phase_pt2h,
                    Context, StatsPieces),
                !:Specs = [StatsSpec | !.Specs]
            ;
                Statistics = table_do_not_gather_statistics
            ),
            (
                AllowReset = table_allow_reset,
                ResetPieces = [words("Error:")] ++
                    color_as_incorrect(
                        [words("cannot request allow_reset")]) ++
                    [words("for the ambiguous name")] ++
                    color_as_subject([qual_sym_name_arity(SNA),
                        suffix(",")]) ++
                    [words("since the compiler-generated reset predicate"),
                    words("would have an ambiguous name too."), nl],
                ResetSpec = spec($pred, severity_error, phase_pt2h,
                    Context, ResetPieces),
                !:Specs = [ResetSpec | !.Specs]
            ;
                AllowReset = table_do_not_allow_reset
            )
        )
    ;
        MaybeAttributes = no
    ),
    list.foldl3(
        module_add_pragma_tabled_for_pred(ProgressStream, TabledMethod, PFUMM,
            PredModuleName, PredName, MaybeAttributes, Context,
            ItemMercuryStatus, PredStatus),
        PredIds, !ModuleInfo, !QualInfo, !Specs).

:- pred module_add_pragma_tabled_for_pred(io.text_output_stream::in,
    tabled_eval_method::in,
    pred_func_or_unknown_maybe_modes::in, module_name::in, string::in,
    maybe(table_attributes)::in, prog_context::in,
    item_mercury_status::in, pred_status::in, pred_id::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_pragma_tabled_for_pred(ProgressStream, TabledMethod0, PFUMM,
        PredModuleName, PredName, MaybeAttributes, Context,
        ItemMercuryStatus, PredStatus, PredId,
        !ModuleInfo, !QualInfo, !Specs) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    ( if TabledMethod0 = tabled_minimal(_) then
        globals.lookup_bool_option(Globals, use_minimal_model_own_stacks,
            OwnStacks),
        (
            OwnStacks = yes,
            TabledMethod = tabled_minimal(own_stacks_consumer)
        ;
            OwnStacks = no,
            TabledMethod = tabled_minimal(stack_copy)
        )
    else
        TabledMethod = TabledMethod0
    ),

    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pfumm_to_maybe_pf_arity_maybe_modes(PFUMM, MaybePredOrFunc, UserArity,
        MaybeModes),
    (
        MaybePredOrFunc = yes(PredOrFunc)
    ;
        MaybePredOrFunc = no,
        PredOrFunc = pred_info_is_pred_or_func(PredInfo0)
    ),

    PredSymName = qualified(PredModuleName, PredName),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    PFSymNameArity = pred_pf_name_arity(PredOrFunc, PredSymName, UserArity),

    TabledMethodStr = tabled_eval_method_to_string(TabledMethod),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        trace [io(!IO)] (
            IdStr = pf_sym_name_pred_form_arity_to_string(PredOrFunc,
                PredSymName, PredFormArity),
            io.format(ProgressStream,
                "%% Processing `:- pragma %s' for %s...\n",
                [s(TabledMethodStr), s(IdStr)], !IO)
        )
    ;
        VeryVerbose = no
    ),

    % Issue a warning if this predicate/function has a pragma inline
    % declaration. Tabled procedures cannot be inlined.
    pred_info_get_markers(PredInfo0, Markers),
    ( if
        marker_is_present(Markers, marker_user_marked_inline),
        globals.lookup_bool_option(Globals, warn_table_with_inline,
            WarnTableWithInline),
        WarnTableWithInline = yes
    then
        InlineWarningPieces = [words("Warning:")] ++
            color_as_subject([qual_pf_sym_name_user_arity(PFSymNameArity)]) ++
            [words("has both a")] ++
            color_as_inconsistent([pragma_decl(TabledMethodStr),
                words("declaration")]) ++
            [words("and a")] ++
            color_as_inconsistent([pragma_decl("inline"),
                words("declaration.")]) ++
            [nl,
            words("The inline pragma")] ++
            color_as_incorrect([words("will be ignored,")]) ++
            [words("because tabled predicates cannot be inlined."), nl,
            words("You can use the"), quote("--no-warn-table-with-inline"),
            words("option to suppress this warning."), nl],
        InlineWarningSpec = spec($pred, severity_warning, phase_pt2h,
            Context, InlineWarningPieces),
        !:Specs = [InlineWarningSpec | !.Specs]
    else
        true
    ),
    ( if pred_info_is_imported(PredInfo0) then
        UserArity = user_arity(UserArityInt),
        SNA = sym_name_arity(PredSymName, UserArityInt),
        Pieces = [words("Error: a"), pragma_decl(TabledMethodStr),
            words("declaration")] ++
            color_as_incorrect([words("may not specify")]) ++
            [words("that it is for an imported"), p_or_f(PredOrFunc),
            words("such as")] ++
            color_as_subject([qual_sym_name_arity(SNA), suffix(".")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        % Do we have to make sure the tabled preds are stratified?
        NeedsStrat = tabled_eval_method_needs_stratification(TabledMethod),
        (
            NeedsStrat = yes,
            module_info_get_must_be_stratified_preds(!.ModuleInfo,
                StratPredIds0),
            set.insert(PredId, StratPredIds0, StratPredIds),
            module_info_set_must_be_stratified_preds(StratPredIds, !ModuleInfo)
        ;
            NeedsStrat = no
        ),

        % Add the eval model to the proc_info for this procedure.
        pred_info_get_proc_table(PredInfo0, ProcTable0),
        (
            MaybeModes = yes(Modes),
            ( if
                get_procedure_matching_argmodes(!.ModuleInfo, ProcTable0,
                    Modes, ProcId, ProcInfo0)
            then
                set_eval_method_create_aux_preds(ProgressStream,
                    PredInfo0, PredOrFunc, PredModuleName, PredName, UserArity,
                    ProcId, ProcInfo0, is_single_proc, Context, TabledMethod,
                    MaybeAttributes, ItemMercuryStatus, PredStatus,
                    ProcTable0, ProcTable, !ModuleInfo, !QualInfo, !Specs),
                pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
                module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
            else
                UserArity = user_arity(UserArityInt),
                SNA = sym_name_arity(PredSymName, UserArityInt),
                Pieces = [words("Error:"),
                    pragma_decl(TabledMethodStr), words("declaration for")] ++
                    color_as_incorrect([words("undeclared mode")]) ++
                    [words("of"), p_or_f(PredOrFunc)] ++
                    color_as_subject([qual_sym_name_arity(SNA),
                        suffix(".")]) ++
                    [nl],
                Spec = spec($pred, severity_error, phase_pt2h,
                    Context, Pieces),
                !:Specs = [Spec | !.Specs]
            )
        ;
            MaybeModes = no,
            map.to_assoc_list(ProcTable0, ExistingProcs),
            (
                ExistingProcs = [],
                UserArity = user_arity(UserArityInt),
                SNA = sym_name_arity(PredSymName, UserArityInt),
                Pieces = [words("Error:"),
                    pragma_decl(TabledMethodStr), words("declaration"),
                    words("for the"), p_or_f(PredOrFunc)] ++
                    color_as_subject([qual_sym_name_arity(SNA),
                        suffix(",")]) ++
                    [words("which has")] ++
                    color_as_incorrect([words("no declared modes.")]) ++
                    [nl],
                Spec = spec($pred, severity_error, phase_pt2h,
                    Context, Pieces),
                !:Specs = [Spec | !.Specs]
            ;
                ExistingProcs = [_ | ExistingProcsTail],
                (
                    ExistingProcsTail = [],
                    SingleProc = is_single_proc
                ;
                    ExistingProcsTail = [_ | _],
                    SingleProc = is_not_single_proc
                ),
                set_eval_method_create_aux_preds_list(ProgressStream,
                    PredInfo0, PredOrFunc, PredModuleName, PredName, UserArity,
                    ExistingProcs, SingleProc, Context,
                    TabledMethod, MaybeAttributes,
                    ItemMercuryStatus, PredStatus, ProcTable0, ProcTable,
                    !ModuleInfo, !QualInfo, !Specs),
                pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
                module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
            )
        )
    ).

    % Return true if the given evaluation method requires a
    % stratification check.
    %
:- func tabled_eval_method_needs_stratification(tabled_eval_method) = bool.

tabled_eval_method_needs_stratification(TabledMethod) = NeedsStratification :-
    (
        ( TabledMethod = tabled_loop_check
        ; TabledMethod = tabled_memo(_)
        ; TabledMethod = tabled_io(_, _)
        ),
        NeedsStratification = no
    ;
        TabledMethod = tabled_minimal(_),
        NeedsStratification = yes
    ).

:- pred set_eval_method_create_aux_preds_list(io.text_output_stream::in,
    pred_info::in,
    pred_or_func::in, module_name::in, string::in, user_arity::in,
    assoc_list(proc_id, proc_info)::in, aux_tabling_maybe_single_proc::in,
    prog_context::in, tabled_eval_method::in, maybe(table_attributes)::in,
    item_mercury_status::in, pred_status::in,
    proc_table::in, proc_table::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

set_eval_method_create_aux_preds_list(_, _, _, _, _, _, [], _, _, _, _, _, _,
        !ProcTable, !ModuleInfo, !QualInfo, !Specs).
set_eval_method_create_aux_preds_list(ProgressStream, PredInfo, PredOrFunc,
        PredModuleName, PredName, UserArity,
        [ProcId - ProcInfo0 | ProcIdsInfos], SingleProc, Context,
        TabledMethod, MaybeAttributes, ItemMercuryStatus, PredStatus,
        !ProcTable, !ModuleInfo, !QualInfo, !Specs) :-
    set_eval_method_create_aux_preds(ProgressStream, PredInfo, PredOrFunc,
        PredModuleName, PredName, UserArity, ProcId, ProcInfo0, SingleProc,
        Context, TabledMethod, MaybeAttributes, ItemMercuryStatus, PredStatus,
        !ProcTable, !ModuleInfo, !QualInfo, !Specs),
    set_eval_method_create_aux_preds_list(ProgressStream, PredInfo, PredOrFunc,
        PredModuleName, PredName, UserArity, ProcIdsInfos, SingleProc,
        Context, TabledMethod, MaybeAttributes, ItemMercuryStatus, PredStatus,
        !ProcTable, !ModuleInfo, !QualInfo, !Specs).

:- pred set_eval_method_create_aux_preds(io.text_output_stream::in,
    pred_info::in, pred_or_func::in, module_name::in, string::in,
    user_arity::in, proc_id::in, proc_info::in,
    aux_tabling_maybe_single_proc::in, prog_context::in,
    tabled_eval_method::in, maybe(table_attributes)::in,
    item_mercury_status::in, pred_status::in,
    proc_table::in, proc_table::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

set_eval_method_create_aux_preds(ProgressStream, PredInfo, PredOrFunc,
        PredModuleName, PredName, UserArity, ProcId, ProcInfo0, SingleProc,
        Context, TabledMethod, MaybeAttributes, ItemMercuryStatus, PredStatus,
        !ProcTable, !ModuleInfo, !QualInfo, !Specs) :-
    proc_info_get_eval_method(ProcInfo0, OldEvalMethod),
    PredSymName = qualified(PredModuleName, PredName),
    PFSymNameArity = pred_pf_name_arity(PredOrFunc, PredSymName, UserArity),
    (
        OldEvalMethod = eval_normal,
        proc_info_get_maybe_declared_argmodes(ProcInfo0,
            MaybeDeclaredArgModes),
        (
            MaybeDeclaredArgModes = no,
            TabledMethodStr = tabled_eval_method_to_pragma_name(TabledMethod),
            UserArity = user_arity(UserArityInt),
            SNA = sym_name_arity(PredSymName, UserArityInt),
            Pieces = [words("Error:"),
                pragma_decl(TabledMethodStr), words("declaration for"),
                p_or_f(PredOrFunc)] ++
                color_as_subject([qual_sym_name_arity(SNA), suffix(",")]) ++
                [words("which has")] ++
                color_as_incorrect([words("no declared modes.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        ;
            MaybeDeclaredArgModes = yes(DeclaredArgModes),
            pred_info_get_arg_types(PredInfo, ArgTypes),
            assoc_list.from_corresponding_lists(ArgTypes, DeclaredArgModes,
                DeclaredArgTypesModes),
            (
                MaybeAttributes = yes(Attributes),
                Strictness = Attributes ^ table_attr_strictness,
                Statistics = Attributes ^ table_attr_statistics,
                AllowReset = Attributes ^ table_attr_allow_reset
            ;
                MaybeAttributes = no,
                Strictness = cts_all_strict,
                Statistics = table_do_not_gather_statistics,
                AllowReset = table_do_not_allow_reset
            ),
            (
                Strictness = cts_specified(MaybeArgMethods, _HiddenArgMethod),
                check_pred_args_against_tabling_methods(!.ModuleInfo, 1,
                    DeclaredArgTypesModes, MaybeArgMethods, ArgErrorPieces)
            ;
                ( Strictness = cts_all_strict
                ; Strictness = cts_all_fast_loose
                ),
                check_pred_args_against_tabling(!.ModuleInfo, 1,
                    DeclaredArgTypesModes, ArgErrorPieces)
            ),
            (
                ArgErrorPieces = []
            ;
                ArgErrorPieces = [_ | _],
                TabledMethodStr =
                    tabled_eval_method_to_pragma_name(TabledMethod),
                Pieces = [words("Error in"),
                    pragma_decl(TabledMethodStr), words("declaration for"),
                    qual_pf_sym_name_user_arity(PFSymNameArity),
                    suffix(":"), nl | ArgErrorPieces],
                Spec = spec($pred, severity_error, phase_pt2h,
                    Context, Pieces),
                !:Specs = [Spec | !.Specs]
            ),
            EvalMethod = eval_tabled(TabledMethod),
            proc_info_set_eval_method(EvalMethod, ProcInfo0, ProcInfo1),
            proc_info_set_table_attributes(MaybeAttributes,
                ProcInfo1, ProcInfo),
            map.det_update(ProcId, ProcInfo, !ProcTable),
            module_info_get_globals(!.ModuleInfo, Globals),
            current_grade_supports_tabling(Globals, TabledMethod,
                IsTablingSupported),
            % We create the statistics and reset predicates if requested
            % even in the presence of errors above, because if didn't do so,
            % later compiler passes would report errors at the sites where
            % these predicates are called.
            %
            % We create the reset and statistics predicates in all grades,
            % but they will do their intended jobs only in C grades.
            (
                Statistics = table_gather_statistics,
                create_tabling_statistics_pred(ProgressStream, PredOrFunc,
                    PredModuleName, PredName, UserArity, ProcId, SingleProc,
                    Context, IsTablingSupported, ItemMercuryStatus, PredStatus,
                    !ModuleInfo, !QualInfo, !Specs)
            ;
                Statistics = table_do_not_gather_statistics
            ),
            (
                AllowReset = table_allow_reset,
                create_tabling_reset_pred(ProgressStream, PredOrFunc,
                    PredModuleName, PredName, UserArity, ProcId, SingleProc,
                    Context, IsTablingSupported, ItemMercuryStatus, PredStatus,
                    !ModuleInfo, !QualInfo, !Specs)
            ;
                AllowReset = table_do_not_allow_reset
            )
        )
    ;
        OldEvalMethod = eval_tabled(OldTabledMethod),
        % We get here only if we have already processed a tabling pragma for
        % this procedure.
        TabledMethodStr = tabled_eval_method_to_pragma_name(TabledMethod),
        UserArity = user_arity(UserArityInt),
        SNA = sym_name_arity(PredSymName, UserArityInt),
        ( if OldTabledMethod = TabledMethod then
            Pieces = [words("Error:"), p_or_f(PredOrFunc),
                qual_sym_name_arity(SNA), words("has")] ++
                color_as_incorrect([words("duplicate"),
                    fixed(TabledMethodStr), words("pragmas")]) ++
                [words("specified."), nl]
        else
            OldTabledMethodStr =
                tabled_eval_method_to_pragma_name(OldTabledMethod),
            Pieces = [words("Error:"), p_or_f(PredOrFunc),
                qual_sym_name_arity(SNA), words("has")] ++
                color_as_incorrect([words("two conflicting"),
                    words("tabling pragmas specified,")]) ++
                color_as_inconsistent([fixed(OldTabledMethodStr)]) ++
                [words("and")] ++
                color_as_inconsistent([fixed(TabledMethodStr), suffix(".")])
                ++ [nl]
        ),
        Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred create_tabling_statistics_pred(io.text_output_stream::in,
    pred_or_func::in, module_name::in, string::in, user_arity::in,
    proc_id::in, aux_tabling_maybe_single_proc::in, prog_context::in,
    bool::in, item_mercury_status::in, pred_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

create_tabling_statistics_pred(ProgressStream, PredOrFunc,
        PredModuleName, PredName, UserArity, ProcId, SingleProc, Context,
        IsTablingSupported, ItemMercuryStatus, PredStatus,
        !ModuleInfo, !QualInfo, !Specs) :-
    Transform = tn_aux_tabling(PredOrFunc, UserArity, atpk_statistics,
        SingleProc, proc_id_to_int(ProcId)),
    make_transformed_pred_name(PredName, Transform, StatsPredName),
    StatsPredSymName = qualified(PredModuleName, StatsPredName),

    TableBuiltinModule = mercury_table_statistics_module,
    StatsTypeName = qualified(TableBuiltinModule, "proc_table_statistics"),
    StatsType = defined_type(StatsTypeName, [], kind_star),
    TypeAndModeArg1 = type_and_mode(StatsType, out_mode),
    TypeAndModeArg2 = type_and_mode(io_state_type, di_mode),
    TypeAndModeArg3 = type_and_mode(io_state_type, uo_mode),
    ArgTypesAndMaybeModes =
        types_and_modes([TypeAndModeArg1, TypeAndModeArg2, TypeAndModeArg3]),

    varset.init(TypeVarSet),
    varset.init(InstVarSet),
    ExistQVars = [],
    Constraints = univ_exist_constraints([], []),

    PredSymName = qualified(PredModuleName, PredName),
    PredSpec = pred_pf_name_arity(PredOrFunc, PredSymName, UserArity),
    Attrs = item_compiler_attributes(compiler_origin_tabling(PredSpec,
        tabling_aux_pred_stats)),
    MaybeAttrs = item_origin_compiler(Attrs),
    PredDecl = item_pred_decl_info(StatsPredSymName, pf_predicate,
        ArgTypesAndMaybeModes, maybe.no, maybe.no, yes(detism_det), MaybeAttrs,
        TypeVarSet, InstVarSet, ExistQVars, purity_pure, Constraints,
        Context, item_no_seq_num),
    module_add_pred_decl(ItemMercuryStatus, PredStatus, may_be_unqualified,
        PredDecl, _MaybePredProcId, !ModuleInfo, !Specs),

    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, PredSymName, PredFormArity),
    some [!Attrs, !VarSet] (
        varset.init(!:VarSet),
        varset.new_named_var("Stats", Stats, !VarSet),
        varset.new_named_var("IO0", IO0, !VarSet),
        varset.new_named_var("IO", IO, !VarSet),

        (
            IsTablingSupported = yes,
            Arg1 = pragma_var(Stats, "Stats", out_mode, bp_always_boxed),
            Arg2 = pragma_var(IO0, "_IO0", di_mode, bp_always_boxed),
            Arg3 = pragma_var(IO, "_IO", uo_mode, bp_always_boxed),

            % Currently, the only grades that support tabling target C.
            !:Attrs = default_attributes(lang_c),
            % It is easier to construct a complex Mercury structure if we are
            % allowed to use Mercury code to build it out of simple components
            % of primitive types.
            set_may_call_mercury(proc_may_call_mercury, !Attrs),
            set_thread_safe(proc_thread_safe, !Attrs),
            set_purity(purity_pure, !Attrs),
            set_may_duplicate(yes(proc_may_not_duplicate), !Attrs),

            Global = table_info_c_global_var_name(!.ModuleInfo, PFSymNameArity,
                ProcId),
            StatsCode = "MR_get_tabling_stats(&" ++ Global ++ ", &Stats);",
            StatsImpl = fp_impl_ordinary(StatsCode, yes(Context)),
            StatsFCInfo = item_foreign_proc_info(!.Attrs,
                StatsPredSymName, pf_predicate, [Arg1, Arg2, Arg3],
                !.VarSet, InstVarSet, StatsImpl, Context, item_no_seq_num),
            % XXX Should return this instead.
            add_foreign_proc(ProgressStream, ItemMercuryStatus, PredStatus,
                StatsFCInfo, !ModuleInfo, !Specs)
        ;
            IsTablingSupported = no,
            DummyStatsFuncSymName = qualified(mercury_table_statistics_module,
                "dummy_proc_table_statistics"),
            sym_name_to_term(Context, DummyStatsFuncSymName, [],
                DummyStatsFuncTerm),
            Args = [variable(Stats, Context),
                variable(IO0, Context), variable(IO, Context)],
            GetStatsExpr = unify_expr(Context,
                variable(Stats, Context), DummyStatsFuncTerm, purity_pure),
            UpdateIOExpr = unify_expr(Context,
                variable(IO0, Context), variable(IO, Context), purity_pure),
            GetStatsUpdateIOExpr = conj_expr(Context,
                GetStatsExpr, [UpdateIOExpr]),
            BodyExpr = promise_purity_expr(Context, purity_pure,
                GetStatsUpdateIOExpr),
            StatsClauseInfo = item_clause_info(pf_predicate, StatsPredSymName,
                Args, !.VarSet, ok2(BodyExpr, []), Context, item_no_seq_num),
            ClauseType = clause_not_for_promise,
            % XXX Should return this instead.
            module_add_clause(ProgressStream, PredStatus, ClauseType,
                StatsClauseInfo, !ModuleInfo, !QualInfo, !Specs)
        )
    ).

:- pred create_tabling_reset_pred(io.text_output_stream::in,
    pred_or_func::in, module_name::in, string::in, user_arity::in,
    proc_id::in, aux_tabling_maybe_single_proc::in, prog_context::in,
    bool::in, item_mercury_status::in, pred_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

create_tabling_reset_pred(ProgressStream, PredOrFunc, PredModuleName, PredName,
        UserArity, ProcId, SingleProc, Context, IsTablingSupported,
        ItemMercuryStatus, PredStatus, !ModuleInfo, !QualInfo, !Specs) :-
    Transform = tn_aux_tabling(PredOrFunc, UserArity, atpk_reset,
        SingleProc, proc_id_to_int(ProcId)),
    make_transformed_pred_name(PredName, Transform, ResetPredName),
    ResetPredSymName = qualified(PredModuleName, ResetPredName),

    TypeAndModeArg1 = type_and_mode(io_state_type, di_mode),
    TypeAndModeArg2 = type_and_mode(io_state_type, uo_mode),
    ArgTypesAndMaybeModes =
        types_and_modes([TypeAndModeArg1, TypeAndModeArg2]),
    varset.init(TypeVarSet),
    varset.init(InstVarSet),
    ExistQVars = [],
    Constraints = univ_exist_constraints([], []),

    PredSymName = qualified(PredModuleName, PredName),
    PredSpec = pred_pf_name_arity(PredOrFunc, PredSymName, UserArity),
    Attrs = item_compiler_attributes(compiler_origin_tabling(PredSpec,
        tabling_aux_pred_reset)),
    MaybeAttrs = item_origin_compiler(Attrs),
    PredDecl = item_pred_decl_info(ResetPredSymName, pf_predicate,
        ArgTypesAndMaybeModes, maybe.no, maybe.no, yes(detism_det), MaybeAttrs,
        TypeVarSet, InstVarSet, ExistQVars, purity_pure, Constraints,
        Context, item_no_seq_num),
    module_add_pred_decl(ItemMercuryStatus, PredStatus, may_be_unqualified,
        PredDecl, _MaybePredProcId, !ModuleInfo, !Specs),

    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, PredSymName, PredFormArity),
    some [!Attrs, !VarSet] (
        varset.init(!:VarSet),
        varset.new_named_var("IO0", IO0, !VarSet),
        varset.new_named_var("IO", IO, !VarSet),

        (
            IsTablingSupported = yes,
            Arg1 = pragma_var(IO0, "_IO0", di_mode, bp_always_boxed),
            Arg2 = pragma_var(IO, "_IO", uo_mode, bp_always_boxed),

            % Currently, the only grades that support tabling target C.
            !:Attrs = default_attributes(lang_c),
            set_may_call_mercury(proc_will_not_call_mercury, !Attrs),
            set_thread_safe(proc_thread_safe, !Attrs),
            set_purity(purity_pure, !Attrs),
            set_may_duplicate(yes(proc_may_not_duplicate), !Attrs),

            GlobalVarName = table_info_c_global_var_name(!.ModuleInfo,
                PFSymNameArity, ProcId),
            ResetCode = GlobalVarName ++ ".MR_pt_tablenode.MR_integer = 0;",
            ResetImpl = fp_impl_ordinary(ResetCode, yes(Context)),
            ResetFCInfo = item_foreign_proc_info(!.Attrs,
                ResetPredSymName, pf_predicate, [Arg1, Arg2],
                !.VarSet, InstVarSet, ResetImpl, Context, item_no_seq_num),
            % XXX Should return this instead.
            add_foreign_proc(ProgressStream, ItemMercuryStatus, PredStatus,
                ResetFCInfo, !ModuleInfo, !Specs)
        ;
            IsTablingSupported = no,
            Args = [variable(IO0, Context), variable(IO, Context)],
            BodyExpr = unify_expr(Context,
                variable(IO0, Context), variable(IO, Context), purity_pure),
            ResetClauseInfo = item_clause_info(pf_predicate, ResetPredSymName,
                Args, !.VarSet, ok2(BodyExpr, []), Context, item_no_seq_num),
            ClauseType = clause_not_for_promise,
            % XXX Should return this instead.
            module_add_clause(ProgressStream, PredStatus, ClauseType,
                ResetClauseInfo, !ModuleInfo, !QualInfo, !Specs)
        )
    ).

:- func table_info_c_global_var_name(module_info, pf_sym_name_arity, proc_id)
    = string.

table_info_c_global_var_name(ModuleInfo, PFSymNameArity, ProcId) = VarName :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    expect(unify(Target, target_c), $pred,
        "memo table statistics and reset are supported only for C"),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    % XXX We should generate a C global variable for a tabled predicate
    % only when generating code for the module that defines that predicate.
    % Shouldn't this mean that the module name in PredSymName is guaranteed
    % to be ModuleName?
    module_info_get_name(ModuleInfo, ModuleName),
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, PredSymName, PredFormArity),
    PredName = unqualify_name(PredSymName),
    (
        HighLevelCode = yes,
        MaybeModuleName = no,
        MLDS_PredLabel = mlds_user_pred_label(PredOrFunc, MaybeModuleName,
            PredName, PredFormArity),
        MLDS_ProcLabel = mlds_proc_label(MLDS_PredLabel, ProcId),
        VarName = tabling_struct_id_to_string(MLDS_ProcLabel, tabling_info)
    ;
        HighLevelCode = no,
        PredFormArity = pred_form_arity(PredFormArityInt),
        proc_id_to_int(ProcId, ProcIdInt),
        ProcLabel = ordinary_proc_label(ModuleName, PredOrFunc, ModuleName,
            PredName, PredFormArityInt, ProcIdInt),
        VarName = proc_tabling_info_var_name(ProcLabel)
    ).

:- func proc_tabling_info_var_name(proc_label) = string.

proc_tabling_info_var_name(ProcLabel) =
    tabling_struct_data_addr_string(ProcLabel, tabling_info).

:- pred check_pred_args_against_tabling_methods(module_info::in, int::in,
    assoc_list(mer_type, mer_mode)::in, list(maybe(arg_tabling_method))::in,
    list(format_piece)::out) is det.

check_pred_args_against_tabling_methods(_, _, [], [], []).
check_pred_args_against_tabling_methods(_, _, [], [_ | _], Pieces) :-
    Pieces = [words("too many argument tabling methods specified."), nl].
check_pred_args_against_tabling_methods(_, _, [_ | _], [], Pieces) :-
    Pieces = [words("not enough argument tabling methods specified."), nl].
check_pred_args_against_tabling_methods(ModuleInfo, ArgNum,
        [Type - Mode | TypesModes], [MaybeArgMethod | MaybeArgMethods],
        Pieces) :-
    % XXX We should check not just the boundedness of the argument, but also
    % whether it has any uniqueness annotation: tabling destroys uniqueness.
    ( if mode_is_fully_input(ModuleInfo, Type, Mode) then
        (
            MaybeArgMethod = yes(_),
            check_pred_args_against_tabling_methods(ModuleInfo, ArgNum + 1,
                TypesModes, MaybeArgMethods, Pieces)
        ;
            MaybeArgMethod = no,
            MethodStr = maybe_arg_tabling_method_to_string(MaybeArgMethod),
            % Don't allow a line break just before the argument number.
            Pieces = [words("in"),
                fixed("argument " ++ int_to_string(ArgNum)), suffix(":"),
                words("argument tabling method")] ++
                color_as_subject([quote(MethodStr)]) ++
                [words("is")] ++
                color_as_incorrect([words("not compatible with"),
                    words("input modes.")]) ++
                [nl]
        )
    else if mode_is_fully_output(ModuleInfo, Type, Mode) then
        (
            MaybeArgMethod = yes(_),
            MethodStr = maybe_arg_tabling_method_to_string(MaybeArgMethod),
            % Don't allow a line break just before the argument number.
            Pieces = [words("in"),
                fixed("argument " ++ int_to_string(ArgNum)), suffix(":"),
                words("argument tabling method")] ++
                color_as_subject([quote(MethodStr)]) ++
                [words("is")] ++
                color_as_incorrect([words("not compatible with"),
                    words("output modes.")]) ++
                [nl]
        ;
            MaybeArgMethod = no,
            check_pred_args_against_tabling_methods(ModuleInfo, ArgNum + 1,
                TypesModes, MaybeArgMethods, Pieces)
        )
    else
        Pieces = [fixed("argument " ++ int_to_string(ArgNum)),
            words("is neither input or output."), nl]
    ).

:- pred check_pred_args_against_tabling(module_info::in, int::in,
    assoc_list(mer_type, mer_mode)::in, list(format_piece)::out) is det.

check_pred_args_against_tabling(_, _, [], []).
check_pred_args_against_tabling(ModuleInfo, ArgNum, [Type - Mode | TypesModes],
        Pieces) :-
    ( if mode_is_fully_input(ModuleInfo, Type, Mode) then
        check_pred_args_against_tabling(ModuleInfo, ArgNum + 1,
            TypesModes, Pieces)
    else if mode_is_fully_output(ModuleInfo, Type, Mode) then
        check_pred_args_against_tabling(ModuleInfo, ArgNum + 1,
            TypesModes, Pieces)
    else
        % Don't allow a line break just before the argument number.
        Pieces = [fixed("argument " ++ int_to_string(ArgNum)),
            words("is neither input or output."), nl]
    ).

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pragma_tabling.
%----------------------------------------------------------------------------%
