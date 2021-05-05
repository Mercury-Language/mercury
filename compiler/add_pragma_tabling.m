%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pragma.add_pragma_tabling.
:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.

:- import_module list.

:- pred module_add_pragma_tabled(pragma_info_tabled::in, prog_context::in,
    pred_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module hlds.add_pred.
:- import_module hlds.code_model.
:- import_module hlds.hlds_code_util.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.add_foreign_proc.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.options.
:- import_module ll_backend.
:- import_module ll_backend.rtti_out.           % XXX undesirable dependency
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.
:- import_module ml_backend.mlds.               % XXX undesirable dependency
:- import_module ml_backend.mlds_to_c_name.     % XXX undesirable dependency
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

module_add_pragma_tabled(TabledInfo, Context, Status,
        !ModuleInfo, !QualInfo, !Specs) :-
    TabledInfo = pragma_info_tabled(EvalMethod, PredOrProcSpec,
        MaybeAttributes),
    PredOrProcSpec = pred_or_proc_pfumm_name(PFUMM, PredName),
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    EvalMethodStr = eval_method_to_string(EvalMethod),
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
            list.length(Modes, PredFormArityInt),
            user_arity_pred_form_arity(PredOrFunc, UserArity,
                pred_form_arity(PredFormArityInt))
        ;
            ModesOrArity = moa_arity(UserArity),
            user_arity_pred_form_arity(PredOrFunc, UserArity,
                pred_form_arity(PredFormArityInt))
        ),
        UserArity = user_arity(UserArityInt),
        % Lookup the pred or func declaration in the predicate table.
        % If it is not there, print an error message and insert
        % a dummy declaration for it.
        predicate_table_lookup_pf_sym_arity(PredicateTable0,
            is_fully_qualified, PredOrFunc, PredName, PredFormArityInt,
            PredIds0),
        (
            PredIds0 = [],
            module_info_get_name(!.ModuleInfo, ModuleName),
            DescPieces = [pragma_decl(EvalMethodStr), words("declaration")],
            preds_add_implicit_report_error(!ModuleInfo, ModuleName,
                PredName, PredFormArityInt, PredOrFunc, Status,
                is_not_a_class_method, Context, origin_user(PredName),
                DescPieces, PredId, !Specs),
            PredIds = [PredId]
        ;
            PredIds0 = [_ | _],
            PredIds = PredIds0
        )
    ;
        PFUMM = pfumm_unknown(UserArity),
        UserArity = user_arity(UserArityInt),
        predicate_table_lookup_sym_arity(PredicateTable0,
            is_fully_qualified, PredName, UserArityInt, PredIds0),
        (
            PredIds0 = [],
            module_info_get_name(!.ModuleInfo, ModuleName),
            DescPieces = [pragma_decl(EvalMethodStr), words("declaration")],
            % XXX The pragma does not say whether the user intends to table
            % a predicate or a function, so adding a predicate here is
            % only a guess.
            user_arity_pred_form_arity(pf_predicate, UserArity,
                pred_form_arity(PredFormArityInt)),
            preds_add_implicit_report_error(!ModuleInfo, ModuleName,
                PredName, PredFormArityInt, pf_predicate, Status,
                is_not_a_class_method, Context, origin_user(PredName),
                DescPieces, PredId, !Specs),
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
            (
                Statistics = table_gather_statistics,
                StatsPieces = [words("Error: cannot request statistics"),
                    words("for the ambiguous name"),
                    qual_sym_name_arity(
                        sym_name_arity(PredName, UserArityInt)),
                    suffix(","),
                    words("since the compiler-generated statistics predicate"),
                    words("would have an ambiguous name too."), nl],
                StatsSpec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, StatsPieces),
                !:Specs = [StatsSpec | !.Specs]
            ;
                Statistics = table_dont_gather_statistics
            ),
            (
                AllowReset = table_allow_reset,
                ResetPieces = [words("Error: cannot request allow_reset"),
                    words("for the ambiguous name"),
                    qual_sym_name_arity(
                        sym_name_arity(PredName, UserArityInt)),
                    suffix(","),
                    words("since the compiler-generated reset predicate"),
                    words("would have an ambiguous name too."), nl],
                ResetSpec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, ResetPieces),
                !:Specs = [ResetSpec | !.Specs]
            ;
                AllowReset = table_dont_allow_reset
            )
        )
    ;
        MaybeAttributes = no
    ),
    list.foldl3(
        module_add_pragma_tabled_for_pred(EvalMethod, PFUMM, PredName,
            MaybeAttributes, Context, Status),
        PredIds, !ModuleInfo, !QualInfo, !Specs).

:- pred module_add_pragma_tabled_for_pred(eval_method::in,
    pred_func_or_unknown_maybe_modes::in, sym_name::in,
    maybe(table_attributes)::in, prog_context::in, pred_status::in,
    pred_id::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_pragma_tabled_for_pred(EvalMethod0, PFUMM, PredName,
        MaybeAttributes, Context, PredStatus, PredId,
        !ModuleInfo, !QualInfo, !Specs) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    ( if EvalMethod0 = eval_minimal(_) then
        globals.lookup_bool_option(Globals, use_minimal_model_own_stacks,
            OwnStacks),
        (
            OwnStacks = yes,
            EvalMethod = eval_minimal(own_stacks_consumer)
        ;
            OwnStacks = no,
            EvalMethod = eval_minimal(stack_copy)
        )
    else
        EvalMethod = EvalMethod0
    ),

    module_info_get_predicate_table(!.ModuleInfo, PredicateTable),
    predicate_table_get_preds(PredicateTable, Preds),
    map.lookup(Preds, PredId, PredInfo0),
    pfumm_to_maybe_pf_arity_maybe_modes(PFUMM, MaybePredOrFunc, UserArity,
        MaybeModes),
    (
        MaybePredOrFunc = yes(PredOrFunc)
    ;
        MaybePredOrFunc = no,
        PredOrFunc = pred_info_is_pred_or_func(PredInfo0)
    ),

    % The places that later use PFSymNameArity all assume that Arity
    % includes the return value for functions, and then make the appropriate
    % correction for this, i.e. they take PredFormArityInt, even though
    % they want UserArity. It would be simpler to just pass UserArity to them,
    % but would require more extensive changes, including splitting
    % pf_sym_name_arity into two types, one for user_arity and one for
    % pred_form_arity.
    user_arity_pred_form_arity(PredOrFunc, UserArity,
        pred_form_arity(PredFormArityInt)),
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, PredName, PredFormArityInt),

    EvalMethodStr = eval_method_to_string(EvalMethod),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        trace [io(!IO)] (
            IdStr = pf_sym_name_orig_arity_to_string(PredOrFunc,
                PredName, PredFormArityInt),
            io.format("%% Processing `:- pragma %s' for %s...\n",
                [s(EvalMethodStr), s(IdStr)], !IO)
        )
    ;
        VeryVerbose = no
    ),

    % Issue a warning if this predicate/function has a pragma inline
    % declaration. Tabled procedures cannot be inlined.
    pred_info_get_markers(PredInfo0, Markers),
    ( if
        check_marker(Markers, marker_user_marked_inline),
        globals.lookup_bool_option(Globals, warn_table_with_inline,
            WarnTableWithInline),
        WarnTableWithInline = yes
    then
        InlineWarningPieces = [words("Warning: "),
            qual_pf_sym_name_orig_arity(PFSymNameArity), words("has a"),
            pragma_decl(EvalMethodStr), words("declaration but also has a"),
            pragma_decl("inline"), words("declaration."), nl,
            words("This inline pragma will be ignored"),
            words("since tabled predicates cannot be inlined."), nl,
            words("You can use the"), quote("--no-warn-table-with-inline"),
            words("option to suppress this warning."), nl],
        InlineWarningSpec = simplest_spec($pred, severity_warning,
            phase_parse_tree_to_hlds, Context, InlineWarningPieces),
        !:Specs = [InlineWarningSpec | !.Specs]
    else
        true
    ),
    ( if pred_info_is_imported(PredInfo0) then
        Pieces = [words("Error: "), pragma_decl(EvalMethodStr),
            words("declaration for imported"),
            qual_pf_sym_name_orig_arity(PFSymNameArity), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        % Do we have to make sure the tabled preds are stratified?
        NeedsStrat = eval_method_needs_stratification(EvalMethod),
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
        map.to_assoc_list(ProcTable0, ExistingProcs),
        (
            MaybeModes = yes(Modes),
            ( if
                get_procedure_matching_argmodes(ExistingProcs, Modes,
                    !.ModuleInfo, ProcId)
            then
                map.lookup(ProcTable0, ProcId, ProcInfo0),
                set_eval_method_create_aux_preds(ProcId, ProcInfo0,
                    Context, PFSymNameArity, yes, EvalMethod,
                    MaybeAttributes, PredStatus, ProcTable0, ProcTable,
                    !ModuleInfo, !QualInfo, !Specs),
                pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
                module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
            else
                Pieces = [words("Error:"),
                    pragma_decl(EvalMethodStr),
                    words("declaration for undeclared mode of"),
                    qual_pf_sym_name_orig_arity(PFSymNameArity),
                    suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, Pieces),
                !:Specs = [Spec | !.Specs]
            )
        ;
            MaybeModes = no,
            (
                ExistingProcs = [],
                Pieces = [words("Error: "),
                    pragma_decl(EvalMethodStr), words("declaration for"),
                    qual_pf_sym_name_orig_arity(PFSymNameArity),
                    words("with no declared modes."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, Pieces),
                !:Specs = [Spec | !.Specs]
            ;
                ExistingProcs = [_ | ExistingProcsTail],
                (
                    ExistingProcsTail = [],
                    SingleProc = yes
                ;
                    ExistingProcsTail = [_ | _],
                    SingleProc = no
                ),
                set_eval_method_create_aux_preds_list(ExistingProcs, Context,
                    PFSymNameArity, SingleProc, EvalMethod, MaybeAttributes,
                    PredStatus, ProcTable0, ProcTable,
                    !ModuleInfo, !QualInfo, !Specs),
                pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
                module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
            )
        )
    ).

:- pred set_eval_method_create_aux_preds_list(
    assoc_list(proc_id, proc_info)::in, prog_context::in,
    pf_sym_name_arity::in, bool::in, eval_method::in,
    maybe(table_attributes)::in, pred_status::in,
    proc_table::in, proc_table::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

set_eval_method_create_aux_preds_list([], _, _, _, _, _, _,
        !ProcTable, !ModuleInfo, !QualInfo, !Specs).
set_eval_method_create_aux_preds_list([ProcId - ProcInfo0 | Rest], Context,
        PFSymNameArity, SingleProc, EvalMethod, MaybeAttributes, PredStatus,
        !ProcTable, !ModuleInfo, !QualInfo, !Specs) :-
    set_eval_method_create_aux_preds(ProcId, ProcInfo0, Context,
        PFSymNameArity, SingleProc, EvalMethod, MaybeAttributes, PredStatus,
        !ProcTable, !ModuleInfo, !QualInfo, !Specs),
    set_eval_method_create_aux_preds_list(Rest, Context,
        PFSymNameArity, SingleProc, EvalMethod, MaybeAttributes, PredStatus,
        !ProcTable, !ModuleInfo, !QualInfo, !Specs).

:- pred set_eval_method_create_aux_preds(proc_id::in, proc_info::in,
    prog_context::in, pf_sym_name_arity::in, bool::in, eval_method::in,
    maybe(table_attributes)::in, pred_status::in,
    proc_table::in, proc_table::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

set_eval_method_create_aux_preds(ProcId, ProcInfo0, Context, PFSymNameArity,
        SingleProc, EvalMethod, MaybeAttributes, PredStatus,
        !ProcTable, !ModuleInfo, !QualInfo, !Specs) :-
    proc_info_get_eval_method(ProcInfo0, OldEvalMethod),
    % NOTE: We don't bother detecting multiple tabling pragmas
    % of the same type here.
    ( if OldEvalMethod = eval_normal then
        proc_info_get_maybe_declared_argmodes(ProcInfo0,
            MaybeDeclaredArgModes),
        (
            MaybeDeclaredArgModes = no,
            EvalMethodStr = eval_method_to_string(EvalMethod),
            Pieces = [words("Error:"),
                pragma_decl(EvalMethodStr), words("declaration for"),
                qual_pf_sym_name_orig_arity(PFSymNameArity),
                suffix(","), words("which has no declared modes."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        ;
            MaybeDeclaredArgModes = yes(DeclaredArgModes),
            (
                MaybeAttributes = yes(Attributes),
                Strictness = Attributes ^ table_attr_strictness,
                Statistics = Attributes ^ table_attr_statistics,
                AllowReset = Attributes ^ table_attr_allow_reset
            ;
                MaybeAttributes = no,
                Strictness = cts_all_strict,
                Statistics = table_dont_gather_statistics,
                AllowReset = table_dont_allow_reset
            ),
            (
                Strictness = cts_specified(MaybeArgMethods, _HiddenArgMethod),
                check_pred_args_against_tabling_methods(DeclaredArgModes,
                    MaybeArgMethods, !.ModuleInfo, 1, MaybeError)
            ;
                ( Strictness = cts_all_strict
                ; Strictness = cts_all_fast_loose
                ),
                check_pred_args_against_tabling(DeclaredArgModes, !.ModuleInfo,
                    1, MaybeError)
            ),
            (
                MaybeError = yes(ArgMsg - ErrorMsg),
                EvalMethodStr = eval_method_to_string(EvalMethod),
                Pieces = [words("Error in"),
                    pragma_decl(EvalMethodStr), words("declaration for"),
                    qual_pf_sym_name_orig_arity(PFSymNameArity),
                    suffix(":"), nl, fixed(ArgMsg), words(ErrorMsg), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, Pieces),
                !:Specs = [Spec | !.Specs]
            ;
                MaybeError = no
            ),
            proc_info_set_eval_method(EvalMethod, ProcInfo0, ProcInfo1),
            proc_info_set_table_attributes(MaybeAttributes,
                ProcInfo1, ProcInfo),
            map.det_update(ProcId, ProcInfo, !ProcTable),
            % We create the statistics and reset predicates if requested
            % even in the presence of errors above, because if didn't do so,
            % later compiler passes would report errors at the sites where
            % these predicates are called.
            %
            % We create the reset and statistics predicates in all grades,
            % but they will do their intended jobs only in C grades.
            (
                Statistics = table_gather_statistics,
                create_tabling_statistics_pred(ProcId, Context,
                    PFSymNameArity, SingleProc, PredStatus,
                    !ProcTable, !ModuleInfo, !QualInfo, !Specs)
            ;
                Statistics = table_dont_gather_statistics
            ),
            (
                AllowReset = table_allow_reset,
                create_tabling_reset_pred(ProcId, Context,
                    PFSymNameArity, SingleProc, PredStatus,
                    !ProcTable, !ModuleInfo, !QualInfo, !Specs)
            ;
                AllowReset = table_dont_allow_reset
            )
        )
    else
        % We get here only if we have already processed a tabling pragma for
        % this procedure.
        EvalMethodStr = eval_method_to_string(EvalMethod),
        ( if OldEvalMethod = EvalMethod then
            Pieces = [words("Error:"),
                qual_pf_sym_name_orig_arity(PFSymNameArity),
                words("has duplicate"), fixed(EvalMethodStr),
                words("pragmas specified."), nl]
        else
            OldEvalMethodStr = eval_method_to_string(OldEvalMethod),
            Pieces = [words("Error:"),
                qual_pf_sym_name_orig_arity(PFSymNameArity), words("has both"),
                fixed(OldEvalMethodStr), words("and"), fixed(EvalMethodStr),
                words("pragmas specified."),
                words("Only one kind of tabling pragma may be applied to it."),
                nl]
        ),
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred create_tabling_statistics_pred(proc_id::in, prog_context::in,
    pf_sym_name_arity::in, bool::in, pred_status::in,
    proc_table::in, proc_table::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

create_tabling_statistics_pred(ProcId, Context, PFSymNameArity, SingleProc,
        PredStatus, !ProcTable, !ModuleInfo, !QualInfo, !Specs) :-
    TableBuiltinModule = mercury_table_statistics_module,
    StatsPredSymName =
        tabling_stats_pred_name(PFSymNameArity, ProcId, SingleProc),
    StatsTypeName = qualified(TableBuiltinModule, "proc_table_statistics"),
    StatsType = defined_type(StatsTypeName, [], kind_star),
    TypeAndModeArg1 = type_and_mode(StatsType, out_mode),
    TypeAndModeArg2 = type_and_mode(io_state_type, di_mode),
    TypeAndModeArg3 = type_and_mode(io_state_type, uo_mode),
    ArgTypesAndModes = [TypeAndModeArg1, TypeAndModeArg2, TypeAndModeArg3],

    varset.init(TypeVarSet),
    varset.init(InstVarSet),
    ExistQVars = [],
    Constraints = constraints([], []),

    Attrs = item_compiler_attributes(compiler_origin_tabling(PFSymNameArity,
        tabling_aux_pred_stats)),
    MaybeAttrs = item_origin_compiler(Attrs),
    PredDecl = item_pred_decl_info(StatsPredSymName, pf_predicate,
        ArgTypesAndModes, maybe.no, maybe.no, yes(detism_det), MaybeAttrs,
        TypeVarSet, InstVarSet, ExistQVars, purity_pure, Constraints,
        Context, -1),
    module_add_pred_decl(PredStatus, may_be_unqualified, PredDecl,
        _MaybePredProcId, !ModuleInfo, !Specs),

    some [!Attrs, !VarSet] (
        varset.init(!:VarSet),
        varset.new_named_var("Stats", Stats, !VarSet),
        varset.new_named_var("IO0", IO0, !VarSet),
        varset.new_named_var("IO", IO, !VarSet),

        module_info_get_globals(!.ModuleInfo, Globals),
        current_grade_supports_tabling(Globals, IsTablingSupported),
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
            StatsPragmaFCInfo = pragma_info_foreign_proc(!.Attrs,
                StatsPredSymName, pf_predicate, [Arg1, Arg2, Arg3],
                !.VarSet, InstVarSet, StatsImpl),
            add_pragma_foreign_proc(StatsPragmaFCInfo, PredStatus, Context,
                yes(-1), !ModuleInfo, !Specs)
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
                GetStatsExpr, UpdateIOExpr),
            BodyExpr = promise_purity_expr(Context, purity_pure,
                GetStatsUpdateIOExpr),
            module_add_clause(!.VarSet, pf_predicate, StatsPredSymName, Args,
                ok2(BodyExpr, []), PredStatus, Context, no, goal_type_none,
                !ModuleInfo, !QualInfo, !Specs)
        )
    ).

:- pred create_tabling_reset_pred(proc_id::in, prog_context::in,
    pf_sym_name_arity::in, bool::in, pred_status::in,
    proc_table::in, proc_table::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

create_tabling_reset_pred(ProcId, Context, PFSymNameArity, SingleProc,
         PredStatus, !ProcTable, !ModuleInfo, !QualInfo, !Specs) :-
    ResetPredSymName = tabling_reset_pred_name(PFSymNameArity, ProcId,
        SingleProc),
    TypeAndModeArg1 = type_and_mode(io_state_type, di_mode),
    TypeAndModeArg2 = type_and_mode(io_state_type, uo_mode),
    ArgTypesAndModes = [TypeAndModeArg1, TypeAndModeArg2],
    varset.init(TypeVarSet),
    varset.init(InstVarSet),
    ExistQVars = [],
    Constraints = constraints([], []),

    Attrs = item_compiler_attributes(compiler_origin_tabling(PFSymNameArity,
        tabling_aux_pred_reset)),
    MaybeAttrs = item_origin_compiler(Attrs),
    PredDecl = item_pred_decl_info(ResetPredSymName, pf_predicate,
        ArgTypesAndModes, maybe.no, maybe.no, yes(detism_det), MaybeAttrs,
        TypeVarSet, InstVarSet, ExistQVars, purity_pure, Constraints,
        Context, -1),
    module_add_pred_decl(PredStatus, may_be_unqualified, PredDecl,
        _MaybePredProcId, !ModuleInfo, !Specs),

    some [!Attrs, !VarSet] (
        varset.init(!:VarSet),
        varset.new_named_var("IO0", IO0, !VarSet),
        varset.new_named_var("IO", IO, !VarSet),

        module_info_get_globals(!.ModuleInfo, Globals),
        current_grade_supports_tabling(Globals, IsTablingSupported),
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
            ResetPragmaFCInfo = pragma_info_foreign_proc(!.Attrs,
                ResetPredSymName, pf_predicate, [Arg1, Arg2],
                !.VarSet, InstVarSet, ResetImpl),
            add_pragma_foreign_proc(ResetPragmaFCInfo, PredStatus, Context,
                yes(-1), !ModuleInfo, !Specs)
        ;
            IsTablingSupported = no,
            Args = [variable(IO0, Context), variable(IO, Context)],
            BodyExpr = unify_expr(Context,
                variable(IO0, Context), variable(IO, Context), purity_pure),
            module_add_clause(!.VarSet, pf_predicate, ResetPredSymName, Args,
                ok2(BodyExpr, []), PredStatus, Context, no, goal_type_none,
                !ModuleInfo, !QualInfo, !Specs)
        )
    ).

:- func tabling_stats_pred_name(pf_sym_name_arity, proc_id, bool) = sym_name.

tabling_stats_pred_name(PFSymNameArity, ProcId, SingleProc) =
    tabling_pred_name("table_statistics_for", PFSymNameArity, ProcId,
        SingleProc).

:- func tabling_reset_pred_name(pf_sym_name_arity, proc_id, bool) = sym_name.

tabling_reset_pred_name(PFSymNameArity, ProcId, SingleProc) =
    tabling_pred_name("table_reset_for", PFSymNameArity, ProcId, SingleProc).

:- func tabling_pred_name(string, pf_sym_name_arity, proc_id, bool) = sym_name.

tabling_pred_name(Prefix, PFSymNameArity, ProcId, SingleProc) = NewSymName :-
    PFSymNameArity = pf_sym_name_arity(PorF, SymName, Arity0),
    (
        PorF = pf_predicate,
        Arity = Arity0
    ;
        PorF = pf_function,
        Arity = Arity0 - 1
    ),
    (
        SymName = qualified(ModuleName, Name),
        MaybeModuleName = yes(ModuleName)
    ;
        SymName = unqualified(Name),
        MaybeModuleName = no
    ),
    NewName0 = Prefix ++ "_" ++ Name ++ "_" ++ int_to_string(Arity),
    (
        SingleProc = yes,
        NewName = NewName0
    ;
        SingleProc = no,
        NewName = NewName0 ++ "_" ++ int_to_string(proc_id_to_int(ProcId))
    ),
    (
        MaybeModuleName = yes(ModuleNameAgain),
        NewSymName = qualified(ModuleNameAgain, NewName)
    ;
        MaybeModuleName = no,
        NewSymName = unqualified(NewName)
    ).

:- func table_info_c_global_var_name(module_info, pf_sym_name_arity, proc_id)
    = string.

table_info_c_global_var_name(ModuleInfo, PFSymNameArity, ProcId) = VarName :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    expect(unify(Target, target_c), $pred,
        "memo table statistics and reset are supported only for C"),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    module_info_get_name(ModuleInfo, ModuleName),
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, PredSymName, Arity),
    PredName = unqualify_name(PredSymName),
    (
        HighLevelCode = yes,
        MaybeModuleName = no,
        % We set CodeModel and NoReturnValue to dummy values because we cannot
        % do any better right now. The code that outputs the mlds_proc_label
        % of an mlds_tabling_ref should use mlds_std_tabling_proc_label to
        % set these fields to the same values.
        CodeModel = model_det,
        NoReturnValue = no,
        MLDS_PredLabel = mlds_user_pred_label(PredOrFunc, MaybeModuleName,
            PredName, Arity, CodeModel, NoReturnValue),
        MLDS_ProcLabel = mlds_proc_label(MLDS_PredLabel, ProcId),
        VarName = mlds_tabling_data_name(MLDS_ProcLabel, tabling_info)
    ;
        HighLevelCode = no,
        proc_id_to_int(ProcId, ProcIdInt),
        ProcLabel = ordinary_proc_label(ModuleName, PredOrFunc, ModuleName,
            PredName, Arity, ProcIdInt),
        VarName = proc_tabling_info_var_name(ProcLabel)
    ).

:- func proc_tabling_info_var_name(proc_label) = string.

proc_tabling_info_var_name(ProcLabel) =
    tabling_struct_data_addr_string(ProcLabel, tabling_info).

:- pred check_pred_args_against_tabling_methods(list(mer_mode)::in,
    list(maybe(arg_tabling_method))::in, module_info::in, int::in,
    maybe(pair(string))::out) is det.

check_pred_args_against_tabling_methods([], [], _, _, no).
check_pred_args_against_tabling_methods([], [_ | _], _, _, MaybeError) :-
    MaybeError = yes("too many argument tabling methods specified." - "").
check_pred_args_against_tabling_methods([_ | _], [], _, _, MaybeError) :-
    MaybeError = yes("not enough argument tabling methods specified." - "").
check_pred_args_against_tabling_methods([Mode | Modes],
        [MaybeArgMethod | MaybeArgMethods], ModuleInfo, ArgNum, MaybeError) :-
    % XXX We should check not just the boundedness of the argument, but also
    % whether it has any uniqueness annotation: tabling destroys uniqueness.
    ( if mode_is_fully_input(ModuleInfo, Mode) then
        (
            MaybeArgMethod = yes(_),
            check_pred_args_against_tabling_methods(Modes, MaybeArgMethods,
                ModuleInfo, ArgNum + 1, MaybeError)
        ;
            MaybeArgMethod = no,
            MaybeError = yes(("argument " ++ int_to_string(ArgNum) ++ ":") -
                ("argument tabling method `" ++
                maybe_arg_tabling_method_to_string(MaybeArgMethod) ++
                "' is not compatible with input modes."))
        )
    else if mode_is_fully_output(ModuleInfo, Mode) then
        (
            MaybeArgMethod = yes(_),
            MaybeError = yes(("argument " ++ int_to_string(ArgNum) ++ ":") -
                ("argument tabling method `" ++
                maybe_arg_tabling_method_to_string(MaybeArgMethod) ++
                "' is not compatible with output modes."))
        ;
            MaybeArgMethod = no,
            check_pred_args_against_tabling_methods(Modes, MaybeArgMethods,
                ModuleInfo, ArgNum + 1, MaybeError)
        )
    else
        MaybeError = yes(("argument " ++ int_to_string(ArgNum) ++ ":") -
            "is neither input or output.")
    ).

:- pred check_pred_args_against_tabling(list(mer_mode)::in, module_info::in,
    int::in, maybe(pair(string))::out) is det.

check_pred_args_against_tabling([], _, _, no).
check_pred_args_against_tabling([Mode | Modes], ModuleInfo, ArgNum,
        MaybeError) :-
    ( if mode_is_fully_input(ModuleInfo, Mode) then
        check_pred_args_against_tabling(Modes, ModuleInfo, ArgNum + 1,
            MaybeError)
    else if mode_is_fully_output(ModuleInfo, Mode) then
        check_pred_args_against_tabling(Modes, ModuleInfo, ArgNum + 1,
            MaybeError)
    else
        MaybeError = yes(("argument " ++ int_to_string(ArgNum)) -
            "is neither input or output.")
    ).

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pragma.add_pragma_tabling.
%----------------------------------------------------------------------------%
