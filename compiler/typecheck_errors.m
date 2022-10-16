%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_errors.m.
% Main author: fjh.
%
% This file contains predicates to report type errors.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_errors.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_info.
:- import_module hlds.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type arg_vector_kind
    --->    arg_vector_clause_head
    ;       arg_vector_plain_call_pred_id(pred_id)
    ;       arg_vector_plain_pred_call(sym_name_pred_form_arity)
    ;       arg_vector_generic_call(generic_call_id)
    ;       arg_vector_foreign_proc_call(pred_id)
    ;       arg_vector_event(string).

:- type var_vector_kind
    --->    var_vector_args(arg_vector_kind)
    ;       var_vector_cond_quant
    ;       var_vector_exist_quant
    ;       var_vector_promise_solutions
    ;       var_vector_switch_complete
    ;       var_vector_switch_arm_detism
    ;       var_vector_loop_control
    ;       var_vector_try_io
    ;       var_vector_atomic_output
    ;       var_vector_atomic_outer.

:- type type_error_goal_context
    --->    type_error_in_var_vector(
                % What kind of variable vector is it?
                teiav_kind                      :: var_vector_kind,

                % The argument number within that vector of variables.
                teiav_arg_num                   :: int
            )
    ;       type_error_in_unify(
                % The original source of the unification we are checking.
                teiu_unify_context              :: unify_context
            )
    ;       type_error_in_atomic_inner.

%---------------------------------------------------------------------------%

:- type cons_error
    --->    foreign_type_constructor(type_ctor, hlds_type_defn)
    ;       abstract_imported_type
    ;       invalid_field_update(sym_name, hlds_ctor_field_defn,
                tvarset, list(tvar))
    ;       new_on_non_existential_type(type_ctor).

%---------------------------------------------------------------------------%

:- func report_pred_call_error(type_error_clause_context, prog_context,
    sym_name_pred_form_arity) = error_spec.

:- func report_unknown_event_call_error(prog_context, string) = error_spec.

:- func report_event_args_mismatch(prog_context, string, list(mer_type),
    list(prog_var)) = error_spec.

:- func maybe_report_no_clauses(module_info, pred_id, pred_info)
    = list(error_spec).

:- func maybe_report_no_clauses_stub(module_info, pred_id, pred_info)
    = list(error_spec).

:- func report_warning_too_much_overloading(type_error_clause_context,
    prog_context, overloaded_symbol_map) = error_spec.

:- func report_error_too_much_overloading(type_error_clause_context,
    prog_context, overloaded_symbol_map) = error_spec.

:- func report_error_unify_var_var(typecheck_info, type_error_clause_context,
    unify_context, prog_context, prog_var, prog_var, type_assign_set)
    = error_spec.

:- func report_error_lambda_var(typecheck_info, type_error_clause_context,
    unify_context, prog_context, pred_or_func,
    lambda_eval_method, prog_var, list(prog_var), type_assign_set)
    = error_spec.

:- func report_error_functor_type(typecheck_info,
    unify_context, prog_context, prog_var,
    list(cons_type_info), cons_id, int, type_assign_set) = error_spec.

:- func report_error_functor_arg_types(typecheck_info,
    type_error_clause_context, unify_context, prog_context, prog_var,
    list(cons_type_info), cons_id, list(prog_var), args_type_assign_set)
    = error_spec.

:- type spec_and_maybe_actual_expected
    --->    spec_and_maybe_actual_expected(
                % A report of the type error.
                error_spec,

                % The actual and expected types involved in the type error,
                % if both are unambiguously known.
                maybe(actual_expected_types)
            ).

:- type actual_expected_types
    --->    actual_expected_types(
                % We put the pieces before the types, so that sorting lists
                % of actual_expected_types structures will *look* sorted,
                % provided the structure of the lists involved is similar.
                % (They should be, since they are generated by the same code.)
                actual_type_pieces      :: list(format_piece),
                actual_type             :: mer_type,
                expected_type_piece     :: list(format_piece),
                expected_type           :: mer_type,
                existq_tvars            :: list(tvar),
                expectation_source      :: maybe(args_type_assign_source)
            ).

:- func report_error_var(typecheck_info, type_error_goal_context,
    prog_context, prog_var, mer_type, type_assign_set)
    = spec_and_maybe_actual_expected.

:- type arg_vector_type_error
    --->    arg_vector_type_error(
                % The argument number in which the error occurred.
                int,

                % The variable at that argument position.
                prog_var,

                % The actual and expected types at that argument position.
                actual_expected_types
            ).

:- func report_arg_vector_type_errors(typecheck_info,
    type_error_clause_context, prog_context, arg_vector_kind, type_assign_set,
    list(arg_vector_type_error)) = error_spec.

:- func report_error_var_either_type(typecheck_info, type_error_clause_context,
    type_error_goal_context, prog_context, prog_var, mer_type, mer_type,
    type_assign_set) = error_spec.

:- func report_error_arg_var(typecheck_info, type_error_clause_context,
    type_error_goal_context, prog_context, prog_var, args_type_assign_set)
    = error_spec.

:- func report_error_undef_cons(type_error_clause_context,
    type_error_goal_context, prog_context, list(cons_error), cons_id, arity)
    = error_spec.

:- func report_ambiguity_error(type_error_clause_context, prog_context,
    overloaded_symbol_map, type_assign, type_assign) = error_spec.

:- func report_unsatisfiable_constraints(type_error_clause_context,
    prog_context, type_assign_set) = error_spec.

:- func report_missing_tvar_in_foreign_code(type_error_clause_context,
    prog_context, string) = error_spec.

:- func report_invalid_coerce_from_to(type_error_clause_context, prog_context,
    tvarset, mer_type, mer_type) = error_spec.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.error_type_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.var_db.
:- import_module parse_tree.vartypes.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

report_pred_call_error(ClauseContext, Context, SymNameArity) = Spec :-
    SymNameArity = sym_name_pred_form_arity(SymName, PredFormArity),
    ModuleInfo = ClauseContext ^ tecc_module_info,
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    PredMarkers = ClauseContext ^ tecc_pred_markers,
    IsFullyQualified = calls_are_fully_qualified(PredMarkers),
    predicate_table_lookup_pf_sym(PredicateTable, IsFullyQualified,
        pf_predicate, SymName, OtherIds),
    PFSymNameArity = pf_sym_name_arity(pf_predicate, SymName, PredFormArity),
    (
        OtherIds = [_ | _],
        predicate_table_get_pred_id_table(PredicateTable, PredIdTable),
        find_pred_arities(PredIdTable, OtherIds, PredFormArities),
        Spec = report_error_pred_num_args(ClauseContext, Context,
            PFSymNameArity, PredFormArities)
    ;
        OtherIds = [],
        report_error_undef_pred(ClauseContext, Context,
            PFSymNameArity, UndefMsg, MissingImportModules),
        predicate_table_lookup_pf_sym(PredicateTable,
            may_be_partially_qualified, pf_function, SymName, FuncOtherIds),
        (
            FuncOtherIds = [_ | _],
            KindMsg = report_error_func_instead_of_pred(Context),
            KindMsgs = [KindMsg]
        ;
            FuncOtherIds = [],
            KindMsgs = []
        ),
        PossibleModuleQuals =
            find_possible_pf_missing_module_qualifiers(PredicateTable,
                pf_predicate, SymName),
        set.list_to_set(PossibleModuleQuals, PossibleModuleQualsSet0),
        set.delete_list(MissingImportModules,
            PossibleModuleQualsSet0, PossibleModuleQualsSet),
        QualMsgs = report_any_missing_module_qualifiers(ClauseContext, Context,
            "predicate", PossibleModuleQualsSet),
        Msgs = [UndefMsg] ++ KindMsgs ++ QualMsgs,
        Spec = error_spec($pred, severity_error, phase_type_check, Msgs)
    ).

%---------------------%

:- func report_error_pred_num_args(type_error_clause_context, prog_context,
    pf_sym_name_arity, list(pred_form_arity)) = error_spec.

report_error_pred_num_args(ClauseContext, Context, PFSymNameArity,
        AllPredFormArities) = Spec :-
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, SymName, PredFormArity),
    PredFormArity = pred_form_arity(PredFormArityInt),
    AllPredFormArityInts =
        list.map(project_pred_form_arity_int, AllPredFormArities),
    MainPieces = in_clause_for_pieces(ClauseContext) ++
        [words("error:")] ++
        error_num_args_to_pieces(PredOrFunc, PredFormArityInt,
            AllPredFormArityInts) ++ [nl] ++
        [words("in call to"), p_or_f(PredOrFunc), qual_sym_name(SymName),
        suffix("."), nl],
    ( if
        % A call to process_options or to process_options_track in getopt_io
        % may appear in the source code either explicitly qualified,
        % or unqualified. If not explicitly qualified by the user,
        % it won't be qualified by the compiler either, due to
        % the arity mismatch.
        (
            SymName = unqualified(PredName),
            StdLibModuleName = "getopt_io"
        ;
            SymName = qualified(ModuleName, PredName),
            is_std_lib_module_name(ModuleName, StdLibModuleName),
            StdLibModuleName = "getopt_io"
        ),
        % We add SpecialPieces if these predicates are called
        % with (one of) their old arities.
        (
            PredName = "process_options",
            ( PredFormArityInt = 6 ; PredFormArityInt = 7 )
        ;
            PredName = "process_options_track",
            PredFormArityInt = 9
        )
    then
        SpecialPieces =
            [words("One possible reason for the error is that"),
            words("the predicate in the"), quote(StdLibModuleName),
            words("module that used to be named"), quote(PredName),
            words("has been renamed to"), quote(PredName ++ "_io"),
            suffix("."), nl]
    else
        SpecialPieces = []
    ),
    Spec = simplest_spec($pred, severity_error, phase_type_check,
        Context, MainPieces ++ SpecialPieces).

%---------------------%

:- pred report_error_undef_pred(type_error_clause_context::in,
    prog_context::in, pf_sym_name_arity::in,
    error_msg::out, list(module_name)::out) is det.

report_error_undef_pred(ClauseContext, Context, PFSymNameArity,
        Msg, MissingImportModules) :-
    PFSymNameArity =
        pf_sym_name_arity(_PredOrFunc, PredSymName, PredFormArity),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    InClauseForComponent = always(InClauseForPieces),
    PredFormArity = pred_form_arity(PredFormArityInt),
    ( if
        PredSymName = unqualified("->"),
        ( PredFormArityInt = 2 ; PredFormArityInt = 4 )
    then
        MainPieces = [words("error:"), quote("->"), words("without"),
            quote(";"), suffix("."), nl],
        MainComponent = always(MainPieces),
        VerbosePieces =
            [words("Note: the else part is not optional."), nl,
            words("Every if-then must have an else."), nl],
        VerboseComponent = verbose_only(verbose_once, VerbosePieces),
        Components = [MainComponent, VerboseComponent],
        MissingImportModules = []
    else if
        PredSymName = unqualified("else"),
        ( PredFormArityInt = 2 ; PredFormArityInt = 4 )
    then
        Components = [always([words("error: unmatched"), quote("else"),
            suffix("."), nl])],
        MissingImportModules = []
    else if
        PredSymName = unqualified("if"),
        ( PredFormArityInt = 2 ; PredFormArityInt = 4 )
    then
        Pieces = [words("error:"), quote("if"), words("without"),
            quote("then"), words("or"), quote("else"), suffix("."), nl],
        Components = [always(Pieces)],
        MissingImportModules = []
    else if
        PredSymName = unqualified("then"),
        ( PredFormArityInt = 2 ; PredFormArityInt = 4 )
    then
        MainPieces = [words("error:"), quote("then"), words("without"),
            quote("if"), words("or"), quote("else"), suffix("."), nl],
        MainComponent = always(MainPieces),
        VerbosePieces =
            [words("Note: the"), quote("else"), words("part is not optional."),
            nl, words("Every if-then must have an"),
            quote("else"), suffix("."), nl],
        VerboseComponent = verbose_only(verbose_once, VerbosePieces),
        Components = [MainComponent, VerboseComponent],
        MissingImportModules = []
    else if
        PredSymName = unqualified("apply"),
        PredFormArityInt >= 1
    then
        Components = report_apply_instead_of_pred,
        MissingImportModules = []
    else if
        PredSymName = unqualified(PurityString),
        PredFormArityInt = 1,
        ( PurityString = "impure" ; PurityString = "semipure" )
    then
        MainPieces = [words("error:"), quote(PurityString),
            words("marker in an inappropriate place."), nl],
        MainComponent = always(MainPieces),
        VerbosePieces =
            [words("Such markers only belong before predicate calls."), nl],
        VerboseComponent = verbose_only(verbose_once, VerbosePieces),
        Components = [MainComponent, VerboseComponent],
        MissingImportModules = []
    else if
        PredSymName = unqualified("some"),
        PredFormArityInt = 2
    then
        Pieces = [words("syntax error in existential quantification:"),
            words("first argument of"), quote("some"),
            words("should be a list of variables."), nl],
        Components = [always(Pieces)],
        MissingImportModules = []
    else
        MainPieces = [words("error: undefined"),
            qual_pf_sym_name_pred_form_arity(PFSymNameArity), suffix("."), nl],
        (
            PredSymName = qualified(ModuleQualifier, _),
            maybe_report_missing_import_addendum(ClauseContext,
                ModuleQualifier, AddeddumPices, MissingImportModules),
            OrdinaryPieces = MainPieces ++ AddeddumPices
        ;
            PredSymName = unqualified(_),
            OrdinaryPieces = MainPieces,
            MissingImportModules = []
        ),
        ( if
            % A call to process_options_se or to process_options_track_se
            % in getopt or getopt_io may appear in the source code either
            % explicitly qualified, or unqualified. If not explicitly
            % qualified by the user, it won't be qualified by the compiler
            % either, due to the wrong name.
            (
                PredSymName = unqualified(PredName)
            ;
                PredSymName = qualified(ModuleName, PredName),
                is_std_lib_module_name(ModuleName, StdLibModuleName),
                ( StdLibModuleName = "getopt"
                ; StdLibModuleName = "getopt_io"
                )
            ),
            % We add SpecialPieces if these predicates are called
            % with (one of) their old arities. (If they are called
            % with any other arity, then the caller didn't work
            % with the old contents of the getopt modules either.
            (
                PredName = "process_options_se",
                ( PredFormArityInt = 4 ; PredFormArityInt = 5 ;
                PredFormArityInt = 6 ; PredFormArityInt = 7 ),
                NewPredName = "process_options"
            ;
                PredName = "process_options_track_se",
                ( PredFormArityInt = 7 ; PredFormArityInt = 9 ),
                NewPredName = "process_options_track"
            )
        then
            SpecialPieces =
                [words("One possible reason for the error is that"),
                words("the predicate"), quote(PredName),
                words("in the Mercury standard library has been renamed to"),
                quote(NewPredName), suffix("."), nl]
        else
            SpecialPieces = []
        ),
        Components = [always(OrdinaryPieces ++ SpecialPieces)]
    ),
    Msg = simple_msg(Context, [InClauseForComponent | Components]).

:- func report_apply_instead_of_pred = list(error_msg_component).

report_apply_instead_of_pred = Components :-
    MainPieces = [words("error: the language construct"), quote("apply"),
        words("should be used as an expression, not as a goal."), nl],
    MainComponent = always(MainPieces),
    VerbosePieces =
        [words("Perhaps you forgot to add"), quote(" = ..."), suffix("?)"), nl,
        words("If you are trying to invoke a higher-order predicate,"),
        words("use"), quote("call"), suffix(","),
            words("not"), quote("apply"), suffix("."), nl,
        words("If you are trying to curry a higher-order function,"),
        words("use a forwarding function:"), nl,
        words("e.g. instead of "), quote("NewFunc = apply(OldFunc, X)"),
        words("use"), quote("NewFunc = my_apply(OldFunc, X)"),
        words("where"), quote("my_apply"), words("is defined"),
        words("with the appropriate arity, e.g."),
        quote("my_apply(Func, X, Y) :- apply(Func, X, Y)."), nl],
    VerboseComponent = verbose_only(verbose_always, VerbosePieces),
    Components = [MainComponent, VerboseComponent].

%---------------------%

:- func report_error_func_instead_of_pred(prog_context) = error_msg.

report_error_func_instead_of_pred(Context) = Msg :-
    Pieces = [words("(There is a *function* with that name, however."), nl,
        words("Perhaps you forgot to add"), quote(" = ..."), suffix("?)"), nl],
    Msg = simplest_msg(Context, Pieces).

%---------------------%

:- func find_possible_pf_missing_module_qualifiers(predicate_table,
    pred_or_func, sym_name) = list(module_name).

find_possible_pf_missing_module_qualifiers(PredicateTable,
        PredOrFunc, SymName) = ModuleNames :-
    predicate_table_lookup_pf_raw_name(PredicateTable, PredOrFunc,
        unqualify_name(SymName), PredIds),
    list.foldl(accumulate_matching_pf_module_names(PredicateTable, SymName),
        PredIds, [], ModuleNames).

:- pred accumulate_matching_pf_module_names(predicate_table::in, sym_name::in,
    pred_id::in, list(module_name)::in, list(module_name)::out) is det.

accumulate_matching_pf_module_names(PredicateTable, SymName, PredId,
        !ModuleNames) :-
    predicate_table_get_pred_id_table(PredicateTable, PredIdTable),
    map.lookup(PredIdTable, PredId, PredInfo),
    pred_info_get_module_name(PredInfo, ModuleName),
    (
        SymName = unqualified(_),
        !:ModuleNames = [ModuleName | !.ModuleNames]
    ;
        SymName = qualified(SymModuleName, _),
        ( if partial_sym_name_matches_full(SymModuleName, ModuleName) then
            !:ModuleNames = [ModuleName | !.ModuleNames]
        else
            true
        )
    ).

%---------------------------------------------------------------------------%

report_unknown_event_call_error(Context, EventName) = Spec :-
    Pieces = [words("Error: there is no event named"),
        quote(EventName), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_type_check,
        Context, Pieces).

%---------------------------------------------------------------------------%

report_event_args_mismatch(Context, EventName, EventArgTypes, Args) = Spec :-
    pred_form_arity(ActualArity) = arg_list_arity(Args),
    pred_form_arity(ExpectedArity) = arg_list_arity(EventArgTypes),
    Pieces = [words("Error:")] ++
        error_num_args_to_pieces(pf_predicate, ActualArity, [ExpectedArity]) ++
        [words("in event"), quote(EventName), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_type_check,
        Context, Pieces).

%---------------------------------------------------------------------------%

maybe_report_no_clauses(ModuleInfo, PredId, PredInfo) = Specs :-
    ShouldReport = should_report_no_clauses(ModuleInfo, PredInfo),
    (
        ShouldReport = yes,
        PredPieces = describe_one_pred_name(ModuleInfo,
            should_not_module_qualify, PredId),
        Pieces = [words("Error: no clauses for") | PredPieces] ++
            [suffix("."), nl],
        pred_info_get_context(PredInfo, Context),

        Spec = simplest_spec($pred, severity_error, phase_type_check,
            Context, Pieces),
        Specs = [Spec]
    ;
        ShouldReport = no,
        Specs = []
    ).

maybe_report_no_clauses_stub(ModuleInfo, PredId, PredInfo) = Specs :-
    ShouldReport = should_report_no_clauses(ModuleInfo, PredInfo),
    (
        ShouldReport = yes,
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, warn_stubs, WarnStubs),
        (
            WarnStubs = yes,
            PredPieces = describe_one_pred_name(ModuleInfo,
                should_not_module_qualify, PredId),
            Pieces = [words("Warning: no clauses for ") | PredPieces] ++
                [suffix("."), nl],
            pred_info_get_context(PredInfo, Context),
            Spec = simplest_spec($pred, severity_warning,
                phase_type_check, Context, Pieces),
            Specs = [Spec]
        ;
            WarnStubs = no,
            Specs = []
        )
    ;
        ShouldReport = no,
        Specs = []
    ).

:- func should_report_no_clauses(module_info, pred_info) = bool.

should_report_no_clauses(ModuleInfo, PredInfo) = ShouldReport :-
    module_info_get_int_bad_clauses(ModuleInfo, IntBadClauses),
    module_info_get_name(ModuleInfo, ModuleName),
    pred_info_get_module_name(PredInfo, PredModuleName),
    pred_info_get_name(PredInfo, PredName),
    UserArity = pred_info_user_arity(PredInfo),
    pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
    SymName = qualified(ModuleName, PredName),
    Id = pred_pf_name_arity(PredOrFunc, SymName, UserArity),
    pred_info_get_markers(PredInfo, PredMarkers),
    ( if
        ( set.contains(IntBadClauses, Id)
        ; check_marker(PredMarkers, marker_fact_table_semantic_errors)
        ; ModuleName \= PredModuleName
        )
    then
        ShouldReport = no
    else
        ShouldReport = yes
    ).

%---------------------------------------------------------------------------%

report_warning_too_much_overloading(ClauseContext, Context,
        OverloadedSymbolMap) = Spec :-
    Msgs = too_much_overloading_to_msgs(ClauseContext, Context,
        OverloadedSymbolMap, no),
    Spec = error_spec($pred, severity_warning, phase_type_check, Msgs).

report_error_too_much_overloading(ClauseContext, Context,
        OverloadedSymbolMap) = Spec :-
    Msgs = too_much_overloading_to_msgs(ClauseContext, Context,
        OverloadedSymbolMap, yes),
    Spec = error_spec($pred, severity_error, phase_type_check, Msgs).

:- func too_much_overloading_to_msgs(type_error_clause_context, prog_context,
    overloaded_symbol_map, bool) = list(error_msg).

too_much_overloading_to_msgs(ClauseContext, Context, OverloadedSymbolMap,
        IsError) = Msgs :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    (
        IsError = no,
        InitPieces = InClauseForPieces ++
            [words("warning: highly ambiguous overloading."), nl],
        InitComponent = always(InitPieces),

        VerbosePieces =
            [words("This may cause type-checking to be very slow."),
            words("It may also make your code difficult to understand."), nl],
        VerboseComponent = verbose_only(verbose_always, VerbosePieces)
    ;
        IsError = yes,
        InitPieces = InClauseForPieces ++
            [words("error: excessively ambiguous overloading."), nl],
        InitComponent = always(InitPieces),

        VerbosePieces =
            [words("This caused the type checker to exceed its limits."),
            words("It may also make your code difficult to understand."), nl],
        VerboseComponent = verbose_only(verbose_always, VerbosePieces)
    ),

    FirstMsg = simple_msg(Context, [InitComponent, VerboseComponent]),

    map.to_assoc_list(OverloadedSymbolMap, OverloadedSymbols),
    OverloadedSymbolsSortedContexts =
        assoc_list.map_values_only(sort_and_remove_dups, OverloadedSymbols),
    (
        OverloadedSymbolsSortedContexts = [],
        Msgs = [FirstMsg]
    ;
        (
            OverloadedSymbolsSortedContexts = [_ - Contexts],
            (
                Contexts = [],
                unexpected($pred, "no contexts")
            ;
                Contexts = [_],
                SecondPieces =
                    [words("The following symbol was overloaded"),
                    words("in the following context."), nl]
            ;
                Contexts = [_, _ | _],
                SecondPieces =
                    [words("The following symbol was overloaded"),
                    words("in the following contexts."), nl]
            )
        ;
            OverloadedSymbolsSortedContexts = [_, _ | _],
            SecondPieces =
                [words("The following symbols were overloaded"),
                words("in the following contexts."), nl]
        ),
        SecondMsg = simplest_msg(Context, SecondPieces),
        ModuleInfo = ClauseContext ^ tecc_module_info,
        DetailMsgsList = list.map(describe_overloaded_symbol(ModuleInfo),
            OverloadedSymbolsSortedContexts),
        list.condense(DetailMsgsList, DetailMsgs),
        Msgs = [FirstMsg, SecondMsg | DetailMsgs]
    ).

:- func describe_overloaded_symbol(module_info,
    pair(overloaded_symbol, list(prog_context))) = list(error_msg).

describe_overloaded_symbol(ModuleInfo, Symbol - SortedContexts) = Msgs :-
    (
        SortedContexts = [],
        unexpected($pred, "no context")
    ;
        SortedContexts = [FirstContext | LaterContexts],
        % We print a detailed message for the first context, but omit
        % repeating the list of possible matches for any later contexts.
        (
            Symbol = overloaded_pred(SymNamePredFormArity, PredIds),
            SymNamePredFormArity =
                sym_name_pred_form_arity(SymName, PredFormArity),
            PredFormArity = pred_form_arity(PredFormArityInt),
            SNA = sym_name_arity(SymName, PredFormArityInt),
            StartPieces = [words("The predicate symbol"),
                qual_sym_name_arity(SNA), suffix("."), nl,
                words("The possible matches are:"), nl_indent_delta(1)],
            PredIdPiecesList = list.map(
                describe_one_pred_name(ModuleInfo, should_module_qualify),
                PredIds),
            list.sort(PredIdPiecesList, SortedPredIdPiecesList),
            PredIdPieces =
                component_list_to_line_pieces(SortedPredIdPiecesList,
                    [suffix("."), nl]),
            FirstPieces = StartPieces ++ PredIdPieces,
            LaterPieces = [words("The predicate symbol"),
                qual_sym_name_arity(SNA), words("is also overloaded here."),
                nl]
        ;
            Symbol = overloaded_func(ConsId, Sources0),
            list.sort(Sources0, Sources),
            StartPieces = [words("The function symbol"),
                qual_cons_id_and_maybe_arity(ConsId), suffix("."), nl,
                words("The possible matches are:"), nl_indent_delta(1)],
            SourcePiecesList = list.map(
                describe_cons_type_info_source(ModuleInfo), Sources),
            list.sort(SourcePiecesList, SortedSourcePiecesList),
            SourcePieces =
                component_list_to_line_pieces(SortedSourcePiecesList,
                    [suffix("."), nl]),
            FirstPieces = StartPieces ++ SourcePieces,
            LaterPieces = [words("The function symbol"),
                qual_cons_id_and_maybe_arity(ConsId),
                words("is also overloaded here."), nl]
        ),
        FirstMsg = simplest_msg(FirstContext, FirstPieces),
        LaterMsgs = list.map(context_to_error_msg(LaterPieces), LaterContexts),
        Msgs = [FirstMsg | LaterMsgs]
    ).

:- func context_to_error_msg(list(format_piece), prog_context) = error_msg.

context_to_error_msg(Pieces, Context) = simplest_msg(Context, Pieces).

:- func describe_cons_type_info_source(module_info, cons_type_info_source)
    = list(format_piece).

describe_cons_type_info_source(ModuleInfo, Source) = Pieces :-
    (
        Source = source_type(TypeCtor, _ConsId),
        Pieces = [words("the type constructor"), qual_type_ctor(TypeCtor)]
    ;
        Source = source_builtin_type(TypeCtorName),
        Pieces = [words("the builtin type constructor"), quote(TypeCtorName)]
    ;
        Source = source_field_access(GetOrSet, TypeCtor,
            _ConsId, _FieldName),
        ( GetOrSet = get, GetOrSetStr = "get"
        ; GetOrSet = set, GetOrSetStr = "set"
        ),
        Pieces = [words("a"), quote(GetOrSetStr),
            words("field access function"),
            words("for the type constructor"), qual_type_ctor(TypeCtor)]
    ;
        Source = source_pred(PredId),
        Pieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
            PredId)
    ;
        Source = source_apply(ApplyOp),
        Pieces = [words("the builtin operator constructor"), quote(ApplyOp)]
    ).

:- func describe_args_type_assign_source(module_info, args_type_assign_source)
    = list(format_piece).

describe_args_type_assign_source(ModuleInfo, Source) = Pieces :-
    (
        Source = atas_pred(PredId),
        Pieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
            PredId)
    ;
        Source = atas_cons(ConsSource),
        (
            ConsSource = source_type(TypeCtor, ConsId),
            Pieces = [words("the functor"),
                unqual_cons_id_and_maybe_arity(ConsId),
                words("of the type constructor"), qual_type_ctor(TypeCtor)]
        ;
            ConsSource = source_builtin_type(TypeCtorName),
            Pieces = [words("the builtin type constructor"),
                quote(TypeCtorName)]
        ;
            ConsSource = source_field_access(GetOrSet, TypeCtor, ConsId,
                FieldName),
            ( GetOrSet = get, GetOrSetStr = "get"
            ; GetOrSet = set, GetOrSetStr = "set"
            ),
            Pieces = [words("the"), quote(GetOrSetStr),
                words("access function for the"), fixed(FieldName),
                words("field of the"), unqual_cons_id_and_maybe_arity(ConsId),
                words("function symbol of the type constructor"),
                qual_type_ctor(TypeCtor)]
        ;
            ConsSource = source_pred(PredId),
            Pieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
                PredId)
        ;
            ConsSource = source_apply(ApplyOp),
            Pieces = [words("the builtin operator constructor"),
                quote(ApplyOp)]
        )
    ;
        Source = atas_higher_order_call(_PredVar),
        % We can't print _PredVar without a varset.
        Pieces = []
    ;
        Source = atas_ensure_have_a_type,
        % These should not occur in errors at all.
        Pieces = []
    ).

%---------------------------------------------------------------------------%

report_error_unify_var_var(Info, ClauseContext, UnifyContext, Context,
        X, Y, TypeAssignSet) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    unify_context_to_pieces(UnifyContext, InClauseForPieces, ContextPieces),

    VarSet = ClauseContext ^ tecc_varset,
    get_inst_varset(ClauseContext, InstVarSet),
    MainPieces = [words("type error in unification of variable"),
        quote(mercury_var_to_name_only_vs(VarSet, X)), nl,
        words("and variable"),
        quote(mercury_var_to_name_only_vs(VarSet, Y)), suffix("."), nl,
        quote(mercury_var_to_name_only_vs(VarSet, X))] ++
        type_of_var_to_pieces(InstVarSet, TypeAssignSet,
            [suffix(",")], X) ++ [nl] ++
        [quote(mercury_var_to_name_only_vs(VarSet, Y))] ++
        type_of_var_to_pieces(InstVarSet, TypeAssignSet,
            [suffix(".")], Y) ++ [nl],
    type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent),
    Msg = simple_msg(Context,
        [always(ContextPieces), always(MainPieces), VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

%---------------------------------------------------------------------------%

report_error_lambda_var(Info, ClauseContext, UnifyContext, Context,
        PredOrFunc, _EvalMethod, Var, ArgVars, TypeAssignSet) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    unify_context_to_pieces(UnifyContext, InClauseForPieces, ContextPieces),

    VarSet = ClauseContext ^ tecc_varset,
    get_inst_varset(ClauseContext, InstVarSet),
    Pieces1 = [words("type error in unification of")] ++
        argument_name_to_pieces(VarSet, Var) ++ [nl],
    (
        PredOrFunc = pf_predicate,
        Pieces2 = [words("and"), prefix("pred("),
            words(mercury_vars_to_name_only_vs(VarSet, ArgVars)),
            suffix(")"), words(":- ...':"), nl]
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgVars, FuncArgs, RetVar),
        Pieces2 = [words("and"), prefix("func("),
            words(mercury_vars_to_name_only_vs(VarSet, FuncArgs)),
            suffix(")"), fixed("="),
            words(mercury_var_to_name_only_vs(VarSet, RetVar)),
            words(":- ...':"), nl]
    ),

    Pieces3 = argument_name_to_pieces(VarSet, Var) ++
        type_of_var_to_pieces(InstVarSet, TypeAssignSet, [suffix(",")], Var) ++
        [nl],

    (
        PredOrFunc = pf_predicate,
        (
            ArgVars = [],
            LambdaTypePieces = [words("pred")]
        ;
            ArgVars = [_ | _],
            list.length(ArgVars, NumArgVars),
            list.duplicate(NumArgVars - 1, ", _", Strings),
            JoinedString = string.join_list("", Strings),
            LambdaTypePieces = [words("pred(_" ++ JoinedString ++ ")")]
        )
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgVars, FuncArgVars, _),
        (
            FuncArgVars = [],
            LambdaTypePieces = [words("func = _")]
        ;
            FuncArgVars = [_ | _],
            list.length(FuncArgVars, NumArgVars),
            list.duplicate(NumArgVars - 1, ", _", Strings),
            JoinedString = string.join_list("", Strings),
            LambdaTypePieces = [words("func(_" ++ JoinedString ++ ") = _")]
        )
    ),
    Pieces4 = [words("lambda expression has type") | LambdaTypePieces] ++
        [suffix("."), nl],
    MainPieces = Pieces1 ++ Pieces2 ++ Pieces3 ++ Pieces4,

    type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent),
    Msg = simple_msg(Context,
        [always(ContextPieces), always(MainPieces), VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

%---------------------------------------------------------------------------%

report_error_functor_type(Info, UnifyContext, Context,
        Var, ConsDefnList, Functor, Arity, TypeAssignSet) = Spec :-
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    unify_context_to_pieces(UnifyContext, InClauseForPieces, ContextPieces),

    VarSet = ClauseContext ^ tecc_varset,
    get_inst_varset(ClauseContext, InstVarSet),
    MainPieces = [words("type error in unification of")] ++
        argument_name_to_pieces(VarSet, Var) ++ [nl, words("and")] ++
        functor_name_to_pieces(Functor, Arity) ++ [suffix("."), nl] ++

        argument_name_to_pieces(VarSet, Var) ++
        type_of_var_to_pieces(InstVarSet, TypeAssignSet, [suffix(",")], Var) ++
            [nl] ++

        functor_name_to_pieces(Functor, Arity) ++
        type_of_functor_to_pieces(InstVarSet, Functor, Arity, ConsDefnList,
            [suffix(".")]) ++
        [nl],

    ( if
        Functor = some_int_const(int_const(_)),
        get_all_transformed_type_stuffs(typestuff_to_type, TypeAssignSet,
            Var, TypesOfVar),
        list.any_true(expected_type_needs_int_constant_suffix, TypesOfVar)
    then
        NoSuffixIntegerPieces = nosuffix_integer_pieces
    else
        NoSuffixIntegerPieces = []
    ),

    type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent),
    AlwaysPieces = ContextPieces ++ MainPieces ++ NoSuffixIntegerPieces,
    Msg = simple_msg(Context, [always(AlwaysPieces), VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

%---------------------------------------------------------------------------%

report_error_functor_arg_types(Info, ClauseContext, UnifyContext, Context, Var,
        ConsDefnList, Functor, ArgVars, ArgsTypeAssignSet) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    unify_context_to_pieces(UnifyContext, InClauseForPieces, ContextPieces),

    ModuleInfo = ClauseContext ^ tecc_module_info,
    VarSet = ClauseContext ^ tecc_varset,
    get_inst_varset(ClauseContext, InstVarSet),
    strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor),
    StrippedFunctorStr = functor_cons_id_to_string(ModuleInfo,
        vns_varset(VarSet), print_name_only, StrippedFunctor, ArgVars),
    list.length(ArgVars, Arity),

    % If we have consistent information about the argument types,
    % we prefer to print an error message that mentions only the arguments
    % that may be in error.
    ConsArgTypesSet = list.map(get_expected_arg_types, ArgsTypeAssignSet),

    ( if
        list.all_same(ConsArgTypesSet),
        ConsArgTypesSet = [ConsArgTypes | _]
    then
        assoc_list.from_corresponding_lists(ArgVars, ConsArgTypes,
            ArgExpTypes),
        TypeAssigns = list.map(get_caller_arg_assign, ArgsTypeAssignSet),
        find_mismatched_args(do_not_add_quotes, InstVarSet, TypeAssigns,
            1, ArgExpTypes,
            [], RevSubsumesMismatches, [], RevNoSubsumeMismatches),
        % RevSubsumesMismatches will contain errors where the actual type
        % is e.g. list(T), while the expected type is list(some_actual_type).
        % Since the argument may be just list(T) because it is [],
        % we don't mention these arguments (which are likely to be red
        % herrings, i.e. not the actual cause of the problem) unless
        % there are no arguments whose possible actual types do not include
        % one that subsumes the expected type.
        (
            RevNoSubsumeMismatches = [_ | _],
            list.reverse(RevNoSubsumeMismatches, NoSubsumeMismatches),
            MaybeNumMismatches = yes(list.length(NoSubsumeMismatches)),
            ErrorPieces = mismatched_args_to_pieces(VarSet, Functor, yes,
                NoSubsumeMismatches)
        ;
            RevNoSubsumeMismatches = [],
            list.reverse(RevSubsumesMismatches, SubsumesMismatches),
            MaybeNumMismatches = yes(list.length(SubsumesMismatches)),
            ErrorPieces = mismatched_args_to_pieces(VarSet, Functor, yes,
                SubsumesMismatches)
        ),
        VerboseComponents = []
    else
        % XXX It should be possible to compute which arguments are
        % definitely OK, and which are suspect.
        MaybeNumMismatches = no,
        TypeAssignSet = convert_args_type_assign_set(ArgsTypeAssignSet),

        % For polymorphic data structures, the type of `Var' (the functor's
        % result type) can affect the valid types for the arguments.
        ( if
            % Could the type of the functor be polymorphic?
            some [ConsDefn] (
                list.member(ConsDefn, ConsDefnList),
                ConsDefn ^ cti_arg_types = [_ | _]
            )
        then
            % If so, print out the type of `Var'.
            ResultTypePieces = argument_name_to_pieces(VarSet, Var) ++
                type_of_var_to_pieces(InstVarSet, TypeAssignSet,
                    [suffix(",")], Var) ++
                [nl]
        else
            ResultTypePieces = []
        ),
        (
            ArgVars = [],
            AllTypesPieces =
                functor_name_to_pieces(Functor, Arity) ++
                type_of_functor_to_pieces(InstVarSet, Functor, Arity,
                    ConsDefnList, [suffix(".")]) ++
                [nl]
        ;
            ArgVars = [HeadArgVar | TailArgVars],
            AllTypesPieces =
                functor_name_to_pieces(Functor, Arity) ++
                type_of_functor_to_pieces(InstVarSet, Functor, Arity,
                    ConsDefnList, [suffix(",")]) ++
                types_of_vars_to_pieces(VarSet, InstVarSet, TypeAssignSet,
                    [suffix("."), nl], HeadArgVar, TailArgVars)
        ),
        ErrorPieces = ResultTypePieces ++ AllTypesPieces,
        type_assign_set_msg_to_verbose_component(Info, VarSet,
            TypeAssignSet, VerboseComponent),
        VerboseComponents = [VerboseComponent]
    ),
    (
        MaybeNumMismatches = no,
        Arguments = "argument(s)"
    ;
        MaybeNumMismatches = yes(NumMismatches),
        ( if NumMismatches = 1 then
            Arguments = "argument"
        else
            Arguments = "arguments"
        )
    ),
    VarAndTermPieces = [words("in unification of")] ++
        argument_name_to_pieces(VarSet, Var) ++ [nl, words("and term"),
        words_quote(StrippedFunctorStr), suffix(":"), nl,
        words("type error in"), words(Arguments), words("of")] ++
        functor_name_to_pieces(StrippedFunctor, Arity) ++ [suffix("."), nl],
    Msg = simple_msg(Context,
        [always(ContextPieces), always(VarAndTermPieces),
        always(ErrorPieces) | VerboseComponents]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

:- type mismatch_info
    --->    mismatch_info(
                % XXX We should report the context of the argument,
                % but unfortunately that information is not stored
                % in the HLDS.
                int,                % Argument number, starting from 1.
                prog_var,           % Variable in that position
                type_mismatch,      % The first mismatch for this arg.
                list(type_mismatch) % Later type mismatches for this arg.
            ).

:- type does_actual_subsume_expected
    --->    actual_does_not_subsume_expected
    ;       actual_subsumes_expected.

:- type type_mismatch_special
    --->    type_mismatch_special_getopt_error(string).

:- type type_mismatch
    --->    type_mismatch_exp_act(
                expected_type_desc  :: list(format_piece),
                actual_type_desc    :: list(format_piece),
                mismatch_subsumes   :: does_actual_subsume_expected,
                maybe_special       :: maybe(type_mismatch_special)
            ).

:- pred find_mismatched_args(maybe_add_quotes::in, inst_varset::in,
    type_assign_set::in, int::in, assoc_list(prog_var, mer_type)::in,
    list(mismatch_info)::in, list(mismatch_info)::out,
    list(mismatch_info)::in, list(mismatch_info)::out) is det.

find_mismatched_args(_, _, _, _, [],
        !RevSubsumesMismatches, !RevNoSubsumeMismatches).
find_mismatched_args(AddQuotes, InstVarSet, TypeAssignSet,
        CurArgNum, [Arg - ExpType | ArgExpTypes],
        !RevSubsumesMismatches, !RevNoSubsumeMismatches) :-
    % XXX When we get a test case in which the quadratic behavior of
    % get_all_type_stuffs_remove_dups is a performance issue, we should
    % try switching to get_all_type_stuffs without the remove_dups,
    % since the call to list.sort_and_remove_dups below should make it
    % semantically unnecessary.
    get_all_type_stuffs_remove_dups(TypeAssignSet, Arg, TypeStuffList),
    strip_builtin_qualifiers_from_type(ExpType, StrippedExpType),
    list.foldl2(
        substitute_types_check_match(AddQuotes, InstVarSet, StrippedExpType),
        TypeStuffList,
        [], TypeMismatches0, no_type_stuff_matches, DoesSomeTypeStuffMatch),
    (
        DoesSomeTypeStuffMatch = some_type_stuff_matches
        % It is possible some TypeStuff in TypeStuffList matches,
        % and some doesn't, so TypeMismatches0 may not be empty.
        % We could gather it and return it in a new accumulator,
        % to be printed if the final contents of both RevSubsumesMismatches
        % and RevNoSubsumeMismatches is empty. However, this should never
        % happen, since report_error_functor_arg_types should not be invoked
        % in the absence of a known mismatch in argument types.
    ;
        DoesSomeTypeStuffMatch = no_type_stuff_matches,
        list.sort_and_remove_dups(TypeMismatches0, TypeMismatches),
        (
            TypeMismatches = [],
            unexpected($pred, "no_type_stuff_matches but TypeMismatches = []")
        ;
            TypeMismatches = [HeadTypeMismatch | TailTypeMismatches],
            Mismatch = mismatch_info(CurArgNum, Arg, HeadTypeMismatch,
                TailTypeMismatches),
            ( if all_no_subsume_mismatches(TypeMismatches) then
                !:RevNoSubsumeMismatches =
                    [Mismatch | !.RevNoSubsumeMismatches]
            else
                !:RevSubsumesMismatches =
                    [Mismatch | !.RevSubsumesMismatches]
            )
        )
    ),
    find_mismatched_args(AddQuotes, InstVarSet, TypeAssignSet,
        CurArgNum + 1, ArgExpTypes,
        !RevSubsumesMismatches, !RevNoSubsumeMismatches).

:- type does_some_type_stuff_match
    --->    no_type_stuff_matches
    ;       some_type_stuff_matches.

:- pred substitute_types_check_match(maybe_add_quotes::in, inst_varset::in,
    mer_type::in, type_stuff::in,
    list(type_mismatch)::in, list(type_mismatch)::out,
    does_some_type_stuff_match::in, does_some_type_stuff_match::out) is det.

substitute_types_check_match(AddQuotes, InstVarSet, StrippedExpType, TypeStuff,
        !TypeMismatches, !DoesSomeTypeStuffMatch) :-
    TypeStuff = type_stuff(ArgType, TVarSet, TypeBindings, ExistQTVars),
    strip_builtin_qualifiers_from_type(ArgType, StrippedArgType),
    apply_rec_subst_to_type(TypeBindings, StrippedArgType, FullArgType),
    apply_rec_subst_to_type(TypeBindings, StrippedExpType, FullExpType),
    ( if
        (
            % There is no mismatch if the actual type of the argument
            % is the same as the expected type.
            identical_types(FullArgType, FullExpType)
        ;
            % There is no mismatch if the actual type of the argument
            % has no constraints on it.
            FullArgType = defined_type(unqualified("<any>"), [], _)
        )
    then
        !:DoesSomeTypeStuffMatch = some_type_stuff_matches
    else
        ( if type_subsumes(FullArgType, FullExpType, _Subst) then
            ActualSubsumesExpected = actual_subsumes_expected
        else
            ActualSubsumesExpected = actual_does_not_subsume_expected
        ),
        ExpectedPieces0 = type_to_pieces(TVarSet, InstVarSet, print_name_only,
            AddQuotes, ExistQTVars, FullExpType),
        ActualPieces0 = type_to_pieces(TVarSet, InstVarSet, print_name_only,
            AddQuotes, ExistQTVars, FullArgType),
        ( if ExpectedPieces0 = ActualPieces0 then
            ExpectedPieces = type_to_pieces(TVarSet, InstVarSet,
                print_name_and_num, AddQuotes, ExistQTVars, FullExpType),
            ActualPieces = type_to_pieces(TVarSet, InstVarSet,
                print_name_and_num, AddQuotes, ExistQTVars, FullArgType)
        else
            ExpectedPieces = ExpectedPieces0,
            ActualPieces = ActualPieces0
        ),
        ( if
            FullExpType = builtin_type(builtin_type_string),
            FullArgType = defined_type(ArgTypeCtorSymName, [_], kind_star),
            ArgTypeCtorSymName = qualified(ArgTypeModuleName, ArgTypeName),
            ArgTypeName = "option_error",
            is_std_lib_module_name(ArgTypeModuleName, StdLibModuleName),
            ( StdLibModuleName = "getopt"
            ; StdLibModuleName = "getopt_io"
            )
        then
            Special = type_mismatch_special_getopt_error(StdLibModuleName),
            MaybeSpecial = yes(Special)
        else
            MaybeSpecial = no
        ),
        TypeMismatch = type_mismatch_exp_act(ExpectedPieces, ActualPieces,
            ActualSubsumesExpected, MaybeSpecial),
        !:TypeMismatches = [TypeMismatch | !.TypeMismatches]
    ).

:- pred all_no_subsume_mismatches(list(type_mismatch)::in) is semidet.

all_no_subsume_mismatches([]).
all_no_subsume_mismatches([Mismatch | Mismatches]) :-
    Mismatch ^ mismatch_subsumes = actual_does_not_subsume_expected,
    all_no_subsume_mismatches(Mismatches).

:- func mismatched_args_to_pieces(prog_varset, cons_id, bool,
    list(mismatch_info)) = list(format_piece).

mismatched_args_to_pieces(_, _, _, []) = [].
mismatched_args_to_pieces(VarSet, Functor, First, [Mismatch | Mismatches])
        = Pieces :-
    Mismatch = mismatch_info(ArgNum, Var,
        HeadTypeMismatch, TailTypeMismatches),
    ( if
        % Handle higher-order syntax such as ''(F, A) specially:
        % output
        %   Functor (F) has type ...;
        %   argument 1 (A) has type ...
        % instead of
        %   Argument 1 (F) has type ...;
        %   argument 2 (A) has type ...
        Functor = cons(unqualified(""), Arity, _),
        Arity > 0
    then
        (
            First = yes,
            ArgNumPieces = [fixed("Functor")]
        ;
            First = no,
            ArgNumPieces = [fixed("Argument"), int_fixed(ArgNum - 1)]
        )
    else
        ArgNumPieces = [fixed("Argument"), int_fixed(ArgNum)]
    ),
    ( if varset.search_name(VarSet, Var, _) then
        VarNamePieces = [prefix("("),
            words(mercury_var_to_name_only_vs(VarSet, Var)),
            suffix(")")]
    else
        VarNamePieces = []
    ),
    HeadTypeMismatch =
        type_mismatch_exp_act(HeadExpectedTypePieces, HeadActualTypePieces,
            _ActualSubsumesExpected, _MaybeSpecial),
    ( if
        expected_types_match(HeadExpectedTypePieces, TailTypeMismatches,
            TailActualTypePieces)
    then
        (
            TailActualTypePieces = [],
            ErrorDescPieces = [words("has type"), nl_indent_delta(1)] ++
                HeadActualTypePieces ++ [suffix(","), nl_indent_delta(-1)] ++
                [words("expected type was"), nl_indent_delta(1)] ++
                HeadExpectedTypePieces ++ [suffix("."), nl_indent_delta(-1)]
        ;
            TailActualTypePieces =
                [SecondActualTypePieces | ThirdPlusActualTypePieces],
            ErrorDescPieces = [words("has type"), nl_indent_delta(1)] ++
                report_actual_types(HeadActualTypePieces,
                    SecondActualTypePieces, ThirdPlusActualTypePieces) ++
                [suffix(","), nl_indent_delta(-1)] ++
                [words("expected type was"), nl_indent_delta(1)] ++
                HeadExpectedTypePieces ++ [suffix("."), nl_indent_delta(-1)]
        )
    else
        AllMismatches = [HeadTypeMismatch | TailTypeMismatches],
        ErrorDescPieces =
            [words("has one of the following type mismatches."), nl] ++
            report_possible_expected_actual_types(1, AllMismatches) ++
        [suffix("."), nl]
    ),
    gather_special_type_mismatches([HeadTypeMismatch | TailTypeMismatches],
        SpecialMismatches),
    SpecialReasonPieces = report_special_type_mismatches(SpecialMismatches),

    ThisMismatchPieces = ArgNumPieces ++ VarNamePieces ++ ErrorDescPieces ++
        SpecialReasonPieces,

    (
        Mismatches = [],
        FollowingMismatchPieces = []
    ;
        Mismatches = [_ | _],
        FollowingMismatchPieces =
            mismatched_args_to_pieces(VarSet, Functor, no, Mismatches)
    ),
    Pieces = ThisMismatchPieces ++ FollowingMismatchPieces.

:- pred expected_types_match(list(format_piece)::in,
    list(type_mismatch)::in, list(list(format_piece))::out) is semidet.

expected_types_match(_ExpTypePieces, [], []).
expected_types_match(ExpTypePieces, [HeadMismatch | TailMismatches],
        [HeadActualTypePieces | TailActualTypePieces]) :-
    HeadMismatch =
        type_mismatch_exp_act(HeadExpTypePieces, HeadActualTypePieces,
            _ActualSubsumesExpected, _MaybeSpecial),
    ExpTypePieces = HeadExpTypePieces,
    expected_types_match(ExpTypePieces, TailMismatches, TailActualTypePieces).

:- func report_actual_types(list(format_piece),
    list(format_piece), list(list(format_piece))) =
    list(format_piece).

report_actual_types(FirstActualTypePieces, SecondActualTypePieces,
        ThirdPlusActualTypePieces) = Pieces :-
    (
        ThirdPlusActualTypePieces = [],
        Pieces =
            FirstActualTypePieces ++ [words("or")] ++ SecondActualTypePieces
    ;
        ThirdPlusActualTypePieces =
            [ThirdActualTypePieces | FourthPlusActualTypePieces],
        Pieces =
            FirstActualTypePieces ++ [suffix(",")] ++
            report_actual_types(SecondActualTypePieces, ThirdActualTypePieces,
                FourthPlusActualTypePieces)
    ).

:- func report_possible_expected_actual_types(int, list(type_mismatch)) =
    list(format_piece).

report_possible_expected_actual_types(_CurrPossNum, []) = [].
report_possible_expected_actual_types(CurrPossNum, [Mismatch | Mismatches])
        = Pieces :-
    Mismatch = type_mismatch_exp_act(ExpectedTypePieces, ActualTypePieces,
        _ActualSubsumesExpected, _MaybeSpecial),
    HeadPieces =
        [words("Possibility"), int_fixed(CurrPossNum), suffix(":")] ++
        [words("actual type")] ++ ActualTypePieces ++ [suffix(",")] ++
        [words("expected type")] ++ ExpectedTypePieces ++ [suffix("."), nl],
    TailPieces = report_possible_expected_actual_types(CurrPossNum + 1,
        Mismatches),
    Pieces = HeadPieces ++ TailPieces.

%---------------------%

:- pred gather_special_type_mismatches(list(type_mismatch)::in,
    set(type_mismatch_special)::out) is det.

gather_special_type_mismatches([], set.init).
gather_special_type_mismatches([Mismatch | Mismatches], !:Specials) :-
    gather_special_type_mismatches(Mismatches, !:Specials),
    Mismatch = type_mismatch_exp_act(_ExpectedTypePieces, _ActualTypePieces,
        _ActualSubsumesExpected, MaybeSpecial),
    (
        MaybeSpecial = no
    ;
        MaybeSpecial = yes(Special),
        set.insert(Special, !Specials)
    ).

:- func report_special_type_mismatches(set(type_mismatch_special))
    = list(format_piece).

report_special_type_mismatches(Specials) = Pieces :-
    report_special_type_mismatches_loop(is_first,
        set.to_sorted_list(Specials), Pieces).

:- pred report_special_type_mismatches_loop(is_first::in,
    list(type_mismatch_special)::in, list(format_piece)::out) is det.

report_special_type_mismatches_loop(_IsFirst, [], []).
report_special_type_mismatches_loop(IsFirst, [HeadSpecial | TailSpecials],
        Pieces) :-
    report_special_type_mismatches_loop(is_not_first, TailSpecials,
        TailPieces),
    HeadPieces = report_special_type_mismatch(IsFirst, HeadSpecial),
    Pieces = HeadPieces ++ TailPieces.

:- func report_special_type_mismatch(is_first, type_mismatch_special)
    = list(format_piece).

report_special_type_mismatch(IsFirst, MismatchSpecial) = Pieces :-
    (
        IsFirst = is_first,
        ReasonIsPieces =
            [words("One possible reason for the error is that")]
    ;
        IsFirst = is_not_first,
        ReasonIsPieces =
            [words("Another possible reason for the error is that")]
    ),
    (
        MismatchSpecial = type_mismatch_special_getopt_error(GetoptModule),
        Pieces = ReasonIsPieces ++
            [words("the signatures of the option processing predicates"),
            words("in the"), quote(GetoptModule), words("module"),
            words("have changed recently."),
            words("Errors are now returned in a structured form,"),
            words("which can be converted to a string by calling the"),
            quote("option_error_to_string"), words("function."), nl]
    ).

%---------------------------------------------------------------------------%

report_error_var(Info, GoalContext, Context, Var, Type, TypeAssignSet)
        = SpecAndMaybeActualExpected :-
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    GoalContextPieces = goal_context_to_pieces(ClauseContext, GoalContext),

    get_inst_varset(ClauseContext, InstVarSet),
    get_all_transformed_type_stuffs(
        type_stuff_to_actual_expected(do_not_add_quotes, InstVarSet, Type),
        TypeAssignSet, Var, ActualExpectedList0),
    list.sort_and_remove_dups(ActualExpectedList0, ActualExpectedList),

    TypeErrorPieces = [words("type error:")],
    VarSet = ClauseContext ^ tecc_varset,
    ( if ActualExpectedList = [ActualExpected] then
        MaybeActualExpected = yes(ActualExpected),
        ActualExpected = actual_expected_types(ActualPieces, ActualType,
            ExpectedPieces, ExpectedType, ExistQTVars, _Source),
        ActualExpectedPieces = argument_name_to_pieces(VarSet, Var) ++
            [words("has type"), nl_indent_delta(1)] ++
            ActualPieces ++ [suffix(","), nl_indent_delta(-1),
            words("expected type was"), nl_indent_delta(1)] ++
            ExpectedPieces ++ [suffix("."), nl_indent_delta(-1)],
        DiffPieces = type_diff_pieces([], ExistQTVars,
            ActualType, ExpectedType)
    else
        MaybeActualExpected = no,
        ModuleInfo = ClauseContext ^ tecc_module_info,
        ActualExpectedPieces = [words("type of")] ++
            argument_name_to_pieces(VarSet, Var) ++
            [words("does not match its expected type;"), nl] ++
            argument_name_to_pieces(VarSet, Var) ++
            [words("has overloaded actual/expected types {")] ++
            [nl_indent_delta(1)] ++
            actual_expected_types_list_to_pieces(ModuleInfo,
                ActualExpectedList) ++
            [nl_indent_delta(-1), fixed("}."), nl],
        DiffPieces = []
    ),
    typecheck_info_get_nosuffix_integer_vars(Info, NoSuffixIntegerVarSet),
    ( if
        set_tree234.contains(NoSuffixIntegerVarSet, Var),
        expected_type_needs_int_constant_suffix(Type)
    then
        NoSuffixIntegerPieces = nosuffix_integer_pieces
    else
        NoSuffixIntegerPieces = []
    ),

    type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent),
    Msg = simple_msg(Context,
        [always(InClauseForPieces), always(GoalContextPieces),
        always(TypeErrorPieces), always(ActualExpectedPieces),
        always(DiffPieces), always(NoSuffixIntegerPieces),
        VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]),
    SpecAndMaybeActualExpected =
        spec_and_maybe_actual_expected(Spec, MaybeActualExpected).

%---------------------------------------------------------------------------%

report_arg_vector_type_errors(Info, ClauseContext, Context, ArgVectorKind,
        TypeAssignSet, ArgVectorTypeErrors0) = Spec :-
    list.sort(ArgVectorTypeErrors0, ArgVectorTypeErrors),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    ArgVectorKindPieces =
        arg_vector_kind_to_pieces(ClauseContext, ArgVectorKind),
    VarSet = ClauseContext ^ tecc_varset,
    (
        ArgVectorTypeErrors =
            [HeadArgVectorTypeErrors | TailArgVectorTypeErrors]
    ;
        ArgVectorTypeErrors = [],
        unexpected($pred, "ArgVectorTypeErrors = []")
    ),
    arg_vector_type_errors_to_pieces(VarSet, ArgVectorTypeErrors,
        HeadArgVectorTypeErrors, TailArgVectorTypeErrors,
        ArgErrorPieces),
    type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent),
    Msg = simple_msg(Context,
        [always(InClauseForPieces), always(ArgVectorKindPieces),
        always(ArgErrorPieces), VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

:- pred arg_vector_type_errors_to_pieces(prog_varset::in,
    list(arg_vector_type_error)::in,
    arg_vector_type_error::in, list(arg_vector_type_error)::in,
    list(format_piece)::out) is det.

arg_vector_type_errors_to_pieces(VarSet, AllErrors, HeadError, TailErrors,
        Pieces) :-
    (
        TailErrors = [],
        SuffixPiece = suffix("."),
        TailPieces = []
    ;
        TailErrors = [HeadTailError | TailTailErrors],
        SuffixPiece = suffix(";"),
        arg_vector_type_errors_to_pieces(VarSet, AllErrors,
            HeadTailError, TailTailErrors, TailPieces)
    ),
    HeadError = arg_vector_type_error(ArgNum, Var, ActualExpected),
    ActualExpected = actual_expected_types(ActualPieces, _ActualType,
        ExpectedPieces, _ExpectedType, _ExistQTVars, _Source),
    find_possible_switched_positions(VarSet, ActualPieces, AllErrors,
        MismatchPieces),
    (
        MismatchPieces = [],
        NlMismatchSuffixPieces = [SuffixPiece, nl_indent_delta(-1)]
    ;
        MismatchPieces = [_ | _],
        NlMismatchSuffixPieces = [nl_indent_delta(-1)] ++
            MismatchPieces ++ [SuffixPiece]
    ),
    Pieces = [words("in argument"), int_fixed(ArgNum), suffix(":"),
        nl_indent_delta(1) |
        argument_name_to_pieces(VarSet, Var)] ++
        [words("has type"), nl_indent_delta(1)] ++
        ActualPieces ++ [suffix(","), nl_indent_delta(-1),
        words("expected type was"), nl_indent_delta(1)] ++
        ExpectedPieces ++ NlMismatchSuffixPieces ++
        [nl_indent_delta(-1) | TailPieces].

:- pred find_possible_switched_positions(prog_varset::in,
    list(format_piece)::in, list(arg_vector_type_error)::in,
    list(format_piece)::out) is det.

find_possible_switched_positions(VarSet, SearchActualPieces, AllErrors,
        Pieces) :-
    find_expecteds_matching_actual(VarSet, SearchActualPieces, AllErrors,
        MismatchPieces),
    (
        MismatchPieces = [],
        Pieces = []
    ;
        MismatchPieces = [_ | _],
        Pieces = [prefix("("),
            words("the actual type is the same as the expected type of")] ++
            MismatchPieces ++ [suffix(")")]
    ).

:- pred find_expecteds_matching_actual(prog_varset::in,
    list(format_piece)::in, list(arg_vector_type_error)::in,
    list(format_piece)::out) is det.

find_expecteds_matching_actual(_VarSet, _SearchActualPieces, [], []).
find_expecteds_matching_actual(VarSet, SearchActualPieces,
        [HeadError | TailErrors], MismatchPieces) :-
    find_expecteds_matching_actual(VarSet, SearchActualPieces, TailErrors,
        TailMismatchPieces),
    HeadError = arg_vector_type_error(ArgNum, Var, ActualExpected),
    ActualExpected = actual_expected_types(_ActualPieces, _ActualType,
        ExpectedPieces, _ExpectedType, _ExistQTVars, _Source),
    ( if SearchActualPieces = ExpectedPieces then
        ( if varset.search_name(VarSet, Var, _) then
            HeadMismatchPieces = [words("argument"), int_fixed(ArgNum),
                suffix(","), words("which is variable"),
                quote(mercury_var_to_name_only_vs(VarSet, Var))]
        else
            HeadMismatchPieces = [words("argument"), int_fixed(ArgNum)]
        ),
        (
            TailMismatchPieces = [],
            MismatchPieces = HeadMismatchPieces
        ;
            TailMismatchPieces = [_ | _],
            ConnectPieces = [suffix(","), words("and")],
            MismatchPieces = HeadMismatchPieces ++ ConnectPieces ++
                TailMismatchPieces
        )
    else
        MismatchPieces = TailMismatchPieces
    ).

%---------------------------------------------------------------------------%

report_error_var_either_type(Info, ClauseContext, GoalContext, Context,
        Var, TypeA, TypeB, TypeAssignSet) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    GoalContextPieces = goal_context_to_pieces(ClauseContext, GoalContext),

    % XXX When we get a test case in which the quadratic behavior of
    % get_all_type_stuffs_remove_dups is a performance issue, we should
    % try switching to get_all_type_stuffs without the remove_dups,
    % since the two calls to list.sort_and_remove_dups below make it
    % semantically unnecessary.
    get_inst_varset(ClauseContext, InstVarSet),
    get_all_type_stuffs_remove_dups(TypeAssignSet, Var, TypeStuffList),
    ActualExpectedListA0 = list.map(
        type_stuff_to_actual_expected(do_not_add_quotes, InstVarSet, TypeA),
        TypeStuffList),
    ActualExpectedListB0 = list.map(
        type_stuff_to_actual_expected(do_not_add_quotes, InstVarSet, TypeB),
        TypeStuffList),
    list.sort_and_remove_dups(ActualExpectedListA0, ActualExpectedListA),
    list.sort_and_remove_dups(ActualExpectedListB0, ActualExpectedListB),

    ErrorPieces = [words("type error:")],
    VarSet = ClauseContext ^ tecc_varset,
    ( if
        ActualExpectedListA = [ActualExpectedA],
        ActualExpectedListB = [ActualExpectedB]
    then
        ActualExpectedA = actual_expected_types(ActualPieces, _,
            ExpectedPiecesA, _, _, _),
        ActualExpectedB = actual_expected_types(_, _,
            ExpectedPiecesB, _, _, _),
        ActualExpectedPieces = argument_name_to_pieces(VarSet, Var) ++
            [words("has type")] ++ ActualPieces ++ [suffix(","), nl,
            words("expected type was either")] ++ ExpectedPiecesA ++
            [words("or")] ++ ExpectedPiecesB ++ [suffix("."), nl]
    else
        ModuleInfo = ClauseContext ^ tecc_module_info,
        ActualExpectedPieces = [words("type of")] ++
            argument_name_to_pieces(VarSet, Var) ++
            [words("does not match its expected type;"), nl] ++
            argument_name_to_pieces(VarSet, Var) ++
            [words("has overloaded actual/expected types {")] ++
            [nl_indent_delta(1)] ++
            actual_expected_types_list_to_pieces(ModuleInfo,
                ActualExpectedListA) ++
            [nl_indent_delta(-1), fixed("} or {"), nl_indent_delta(1)] ++
            actual_expected_types_list_to_pieces(ModuleInfo,
                ActualExpectedListB) ++
            [nl_indent_delta(-1), fixed("}."), nl]
    ),

    type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent),
    Msg = simple_msg(Context,
        [always(InClauseForPieces ++ GoalContextPieces),
        always(ErrorPieces ++ ActualExpectedPieces), VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

%---------------------------------------------------------------------------%

report_error_arg_var(Info, ClauseContext, GoalContext, Context, Var,
        ArgTypeAssignSet) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    GoalContextPieces = goal_context_to_pieces(ClauseContext, GoalContext),

    get_inst_varset(ClauseContext, InstVarSet),
    get_arg_type_stuffs(Var, ArgTypeAssignSet, ArgTypeStuffList),
    ActualExpectedList0 = list.map(
        arg_type_stuff_to_actual_expected(do_not_add_quotes, InstVarSet),
        ArgTypeStuffList),
    list.sort_and_remove_dups(ActualExpectedList0, ActualExpectedList),

    ErrorPieces = [words("type error:")],
    VarSet = ClauseContext ^ tecc_varset,
    ( if ActualExpectedList = [ActualExpected] then
        ActualExpected = actual_expected_types(ActualPieces, ActualType,
            ExpectedPieces, ExpectedType, ExistQTVars, _MaybeSource),
        ActualExpectedPieces = argument_name_to_pieces(VarSet, Var) ++
            [words("has type"), nl_indent_delta(1)] ++
            ActualPieces ++ [suffix(","), nl_indent_delta(-1),
            words("expected type was"), nl_indent_delta(1)] ++
            ExpectedPieces ++ [suffix("."), nl_indent_delta(-1)],
        DiffPieces = type_diff_pieces([], ExistQTVars,
            ActualType, ExpectedType)
    else
        ActualTypePieceLists = list.map((func(AE) = AE ^ actual_type_pieces),
            ActualExpectedList),
        ModuleInfo = ClauseContext ^ tecc_module_info,
        ( if
            all_same(ActualTypePieceLists),
            ActualTypePieceLists = [ActualTypePieces | _]
        then
            % If some elements of ActualExpectedList have substantive sources
            % and some don't, then don't print any of the sources, since
            % doing so would be confusing.
            ( if
                HasSource =
                    ( pred(AE::in) is semidet :-
                        MaybeSource = AE ^ expectation_source,
                        MaybeSource = yes(Source),
                        Source \= atas_ensure_have_a_type
                    ),
                list.all_true(HasSource, ActualExpectedList)
            then
                MaybePrintSource = print_expectation_source
            else
                MaybePrintSource = print_expected
            ),
            acc_expected_type_source_pieces(ModuleInfo, MaybePrintSource,
                ActualExpectedList, _, AllExpectedPieces),
            ActualExpectedPieces = [words("type of")] ++
                argument_name_to_pieces(VarSet, Var) ++
                [words("does not match its expected type;"), nl,
                words("its inferred type is"), nl_indent_delta(1)] ++
                ActualTypePieces ++ [suffix(","), nl_indent_delta(-1)] ++
                AllExpectedPieces
                % ZZZ
                % words("its expected types are {"), nl_indent_delta(1)] ++
                % AllExpectedPieces ++ [nl_indent_delta(-1), fixed("}."), nl]
        else
            ActualExpectedPieces = [words("type of")] ++
                argument_name_to_pieces(VarSet, Var) ++
                [words("does not match its expected type;"), nl] ++
                argument_name_to_pieces(VarSet, Var) ++
                [words("has overloaded actual/expected types {")] ++
                [nl_indent_delta(1)] ++
                actual_expected_types_list_to_pieces(ModuleInfo,
                    ActualExpectedList) ++
                [nl_indent_delta(-1), fixed("}."), nl]
        ),
        % Printing the diffs derives from *all* the elements of
        % ActualExpectedList would be more confusing than helpful.
        DiffPieces = []
    ),
    arg_type_assign_set_msg_to_verbose_component(Info, VarSet,
        ArgTypeAssignSet, VerboseComponent),
    Msg = simple_msg(Context,
        [always(InClauseForPieces ++ GoalContextPieces),
        always(ErrorPieces ++ ActualExpectedPieces ++ DiffPieces),
        VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

%---------------------------------------------------------------------------%

report_error_undef_cons(ClauseContext, GoalContext, Context,
        ConsErrors, Functor, Arity) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    GoalContextPieces = goal_context_to_pieces(ClauseContext, GoalContext),
    InitComp = always(InClauseForPieces ++ GoalContextPieces),
    % Check for some special cases, so that we can give clearer error messages.
    ( if
        Functor = cons(unqualified(FunctorName), FunctorArity, _),
        expect(unify(Arity, FunctorArity), $pred, "arity mismatch"),
        language_builtin_functor(FunctorName, FunctorArity)
    then
        language_builtin_functor_components(FunctorName, FunctorArity,
            FunctorComps),
        Spec = error_spec($pred, severity_error, phase_type_check,
            [simple_msg(Context, [InitComp | FunctorComps])])
    else if
        Functor = cons(unqualified(FunctorName), FunctorArity, _),
        expect(unify(Arity, FunctorArity), $pred, "arity mismatch"),
        syntax_functor_components(FunctorName, FunctorArity, FunctorComps)
    then
        Spec = error_spec($pred, severity_error, phase_type_check,
            [simple_msg(Context, [InitComp | FunctorComps])])
    else
        report_error_undef_cons_std(ClauseContext, Context, InitComp,
            ConsErrors, Functor, Arity, Spec)
    ).

%---------------------%

    % language_builtin_functor(Name, Arity) is true iff Name/Arity is the name
    % of a builtin language construct that should be used as a goal,
    % not as an expression.
    %
:- pred language_builtin_functor(string::in, arity::in) is semidet.

language_builtin_functor("=", 2).
language_builtin_functor("\\=", 2).
language_builtin_functor(",", 2).
language_builtin_functor(";", 2).
language_builtin_functor("\\+", 1).
language_builtin_functor("not", 1).
language_builtin_functor("<=>", 2).
language_builtin_functor("=>", 2).
language_builtin_functor("<=", 2).
language_builtin_functor("call", _).
language_builtin_functor("impure", 1).
language_builtin_functor("semipure", 1).
language_builtin_functor("all", 2).
language_builtin_functor("some", 2).

:- pred language_builtin_functor_components(string::in, arity::in,
    list(error_msg_component)::out) is det.

language_builtin_functor_components(Name, Arity, Components) :-
    MainPieces = [words("error: the language construct"),
        unqual_sym_name_arity(sym_name_arity(unqualified(Name), Arity)),
        words("should be used as a goal, not as an expression."), nl],
    VerbosePieces = [words("If you are trying to use a goal"),
        words("as a boolean function, you should write"),
        words_quote("if <goal> then yes else no"), words("instead."), nl],
    ( if Name = "call" then
        VerboseCallPieces =
            [words("If you are trying to invoke a higher-order function,"),
            words("you should use"), quote("apply"), suffix(","),
            words("not"), quote("call"), suffix("."), nl,
            words("If you are trying to curry a higher-order function,"),
            words("see the ""Creating higher-order terms"" section"),
            words("of the Mercury Language Reference Manual."), nl,
            words("If you really are trying to use"),
            quote("call"), words("as an expression"),
            words("and not as an application of the language builtin call/N,"),
            words("make sure that you have the arity correct,"),
            words("and that the functor"), quote("call"),
            words("is actually defined."),
            words("(If it is defined in a separate module,"),
            words("check that the module is correctly imported.)"), nl]
    else
        VerboseCallPieces = []
    ),
    Components = [always(MainPieces),
        verbose_only(verbose_always, VerbosePieces),
        verbose_only(verbose_once, VerboseCallPieces)].

%---------------------%

:- pred syntax_functor_components(string::in, arity::in,
    list(error_msg_component)::out) is semidet.

syntax_functor_components("else", 2, Components) :-
    Pieces = [words("error: unmatched"), quote("else"), suffix("."), nl],
    Components = [always(Pieces)].
syntax_functor_components("if", 2, Components) :-
    Pieces = [words("error:"), quote("if"), words("without"), quote("then"),
         words("or"), quote("else"), suffix("."), nl],
    Components = [always(Pieces)].
syntax_functor_components("then", 2, Components) :-
    Pieces1 = [words("error:"), quote("then"), words("without"),
        quote("if"), words("or"), quote("else"), suffix("."), nl],
    Pieces2 = [words("Note: the"), quote("else"),
        words("part is not optional."),
        words("Every if-then must have an"), quote("else"), suffix("."), nl],
    Components = [always(Pieces1), verbose_only(verbose_once, Pieces2)].
syntax_functor_components("->", 2, Components) :-
    Pieces1 = [words("error:"), quote("->"), words("without"),
        quote(";"), suffix("."), nl],
    Pieces2 = [words("Note: the else part is not optional."),
        words("Every if-then must have an else."), nl],
    Components = [always(Pieces1), verbose_only(verbose_once, Pieces2)].
syntax_functor_components("^", 2, Components) :-
    Pieces1 = [words("error: invalid use of field selection operator"),
        prefix("("), quote("^"), suffix(")."), nl],
    Pieces2 = [words("This is probably some kind of syntax error."),
        words("The field name must be an atom,"),
        words("not a variable or other term."), nl],
    Components = [always(Pieces1), verbose_only(verbose_always, Pieces2)].
syntax_functor_components(":=", 2, Components) :-
    Pieces1 = [words("error: invalid use of field update operator"),
        prefix("("), quote(":="), suffix(")."), nl],
    Pieces2 = [words("This is probably some kind of syntax error."), nl],
    Components = [always(Pieces1), verbose_only(verbose_always, Pieces2)].
syntax_functor_components(":-", 2, Components) :-
    Pieces = [words("syntax error in lambda expression"),
         prefix("("), quote(":-"), suffix(")."), nl],
    Components = [always(Pieces)].
syntax_functor_components("-->", 2, Components) :-
    Pieces = [words("syntax error in DCG lambda expression"),
        prefix("("), quote("-->"), suffix(")."), nl],
    Components = [always(Pieces)].
syntax_functor_components(".", 2, Components) :-
    Pieces = [words("error: the list constructor is now"),
        unqual_sym_name_arity(sym_name_arity(unqualified("[|]"), 2)),
        suffix(","), words("not"), quote("./2"),
        suffix("."), nl],
    Components = [always(Pieces)].
syntax_functor_components("!", 1, Components) :-
    Pieces1 = [words("error: invalid use of"), quote("!"),
        words("state variable operator."), nl],
    Pieces2 = [words("You probably meant to use"), quote("!."),
        words("or"), quote("!:"), suffix("."), nl],
    Components = [always(Pieces1), verbose_only(verbose_always, Pieces2)].

%---------------------%

:- pred report_error_undef_cons_std(type_error_clause_context::in,
    prog_context::in, error_msg_component::in, list(cons_error)::in,
    cons_id::in, arity::in, error_spec::out) is det.

report_error_undef_cons_std(ClauseContext, Context, InitComp, ConsErrors,
        Functor, Arity, Spec) :-
    ModuleInfo = ClauseContext ^ tecc_module_info,
    module_info_get_cons_table(ModuleInfo, ConsTable),
    ( if
        Functor = cons(Constructor, FunctorArity, _),
        expect(unify(Arity, FunctorArity), $pred, "arity mismatch"),

        return_cons_arities(ConsTable, Constructor, ConsArities),

        module_info_get_predicate_table(ModuleInfo, PredTable),
        predicate_table_lookup_sym(PredTable, may_be_partially_qualified,
            Constructor, PredIds),
        return_function_arities(ModuleInfo, PredIds, [], FuncArities),

        list.sort_and_remove_dups(ConsArities ++ FuncArities, AllArities),
        list.delete_all(AllArities, Arity, OtherArities),
        OtherArities = [_ | _]
    then
        FunctorPieces = wrong_arity_constructor_to_pieces(Constructor,
            Arity, OtherArities),
        FunctorComps = [always(FunctorPieces)],
        % The code that constructs QualMsgs below uses wording that
        % can be misleading in the presence of arity mismatches.
        QualMsgs = []
    else
        Pieces1 = [words("error: undefined symbol"),
            qual_cons_id_and_maybe_arity(Functor), suffix("."), nl],
        ( if
            Functor = cons(Constructor, _, _),
            Constructor = qualified(ModQual, _)
        then
            maybe_report_missing_import_addendum(ClauseContext, ModQual,
                Pieces2, MissingImportModules)
        else if
            Functor = cons(unqualified("[|]"), 2, _)
        then
            maybe_report_missing_import_addendum(ClauseContext,
                unqualified("list"), Pieces2, MissingImportModules)
        else
            Pieces2 = [],
            MissingImportModules = []
        ),
        FunctorComps = [always(Pieces1 ++ Pieces2)],
        ( if Functor = cons(FunctorName, _, _) then
            return_cons_defns_with_given_name(ConsTable,
                unqualify_name(FunctorName), ConsDefns),
            list.foldl(accumulate_matching_cons_module_names(FunctorName),
                ConsDefns, [], ConsModuleNames),
            module_info_get_predicate_table(ModuleInfo, PredicateTable),
            PredModuleNames =
                find_possible_pf_missing_module_qualifiers(PredicateTable,
                    pf_predicate, FunctorName),
            FuncModuleNames =
                find_possible_pf_missing_module_qualifiers(PredicateTable,
                    pf_function, FunctorName),
            ModuleNames =
                ConsModuleNames ++ PredModuleNames ++ FuncModuleNames,
            set.list_to_set(ModuleNames, ModuleNamesSet0),
            set.delete_list(MissingImportModules,
                ModuleNamesSet0, ModuleNamesSet),
            QualMsgs = report_any_missing_module_qualifiers(ClauseContext,
                Context, "symbol", ModuleNamesSet)
        else
            QualMsgs = []
        )
    ),
    (
        ConsErrors = [],
        ConsMsgs = []
    ;
        ConsErrors = [_ | _],
        ConsMsgLists = list.map(report_cons_error(Context), ConsErrors),
        list.condense(ConsMsgLists, ConsMsgs)
    ),
    Spec = error_spec($pred, severity_error, phase_type_check,
        [simple_msg(Context,
            [InitComp | FunctorComps]) | ConsMsgs] ++ QualMsgs).

:- pred return_function_arities(module_info::in, list(pred_id)::in,
    list(int)::in, list(int)::out) is det.

return_function_arities(_, [], !FuncArities).
return_function_arities(ModuleInfo, [PredId | PredIds], !FuncArities) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
    (
        PredOrFunc = pf_predicate
    ;
        PredOrFunc = pf_function,
        pred_info_get_orig_arity(PredInfo, OrigArity),
        adjust_func_arity(pf_function, FuncArity, OrigArity),
        !:FuncArities = [FuncArity | !.FuncArities]
    ),
    return_function_arities(ModuleInfo, PredIds, !FuncArities).

:- func wrong_arity_constructor_to_pieces(sym_name, arity, list(int))
    = list(format_piece).

wrong_arity_constructor_to_pieces(Name, Arity, ActualArities) = Pieces :-
    % Constructors' arities should be treated the same way as
    % predicates' ariries.
    NumArgsPieces =
        error_num_args_to_pieces(pf_predicate, Arity, ActualArities),
    Pieces = [words("error: ")] ++ NumArgsPieces ++
        [words("in use of constructor"), qual_sym_name(Name), suffix(".")].

:- pred accumulate_matching_cons_module_names(sym_name::in, hlds_cons_defn::in,
    list(module_name)::in, list(module_name)::out) is det.

accumulate_matching_cons_module_names(FunctorSymName, ConsDefn,
        !ModuleNames) :-
    type_ctor(TypeCtorSymName, _) = ConsDefn ^ cons_type_ctor,
    (
        TypeCtorSymName = unqualified(_)
        % There can be no problem with use_module replacing import_module
        % if the hlds_cons_defn is for a builtin data constructor.
    ;
        TypeCtorSymName = qualified(TypeCtorModuleName, _),
        FunctorName = unqualify_name(FunctorSymName),
        FullSymName = qualified(TypeCtorModuleName, FunctorName),
        ( if partial_sym_name_matches_full(FunctorSymName, FullSymName) then
            !:ModuleNames = [TypeCtorModuleName | !.ModuleNames]
        else
            true
        )
    ).

:- func report_cons_error(prog_context, cons_error) = list(error_msg).

report_cons_error(Context, ConsError) = Msgs :-
    (
        ConsError = foreign_type_constructor(TypeCtor, _),
        Pieces = [words("There are"),
            pragma_decl("foreign_type"), words("declarations"),
            words("for type"), qual_type_ctor(TypeCtor), suffix(","),
            words("so it is treated as an abstract type"),
            words("in all predicates and functions"),
            words("which are not implemented"),
            words("for those foreign types."), nl],
        Msgs = [simplest_msg(Context, Pieces)]
    ;
        ConsError = abstract_imported_type,
        % For `abstract_imported_type' errors, the "undefined symbol"
        % error written by `report_error_undef_cons' is sufficient so
        % we do not print an additional error message here.
        Msgs = []
    ;
        ConsError = invalid_field_update(FieldName, FieldDefn, TVarSet, TVars),
        FieldDefn = hlds_ctor_field_defn(DefnContext, _, _, ConsId, _),
        Pieces1 = [words("Field"), unqual_sym_name(FieldName),
            words("cannot be updated because"),
            words("the existentially quantified type")],
        (
            TVars = [],
            unexpected($pred, "no type variables")
        ;
            TVars = [TVar],
            TVarsStr = mercury_var_to_name_only_vs(TVarSet, TVar),
            Pieces2 = [words("variable"), quote(TVarsStr), words("occurs")]
        ;
            TVars = [_, _ | _],
            TVarsStr = mercury_vars_to_name_only_vs(TVarSet, TVars),
            Pieces2 = [words("variables"), quote(TVarsStr), words("occur")]
        ),
        Pieces3 = [words("in the types of field"), unqual_sym_name(FieldName),
            words("and some other field"),
            words("in definition of constructor"),
            qual_cons_id_and_maybe_arity(ConsId), suffix("."), nl],
        Pieces = Pieces1 ++ Pieces2 ++ Pieces3,
        Msgs = [simplest_msg(DefnContext, Pieces)]
    ;
        ConsError = new_on_non_existential_type(TypeCtor),
        Pieces = [words("Invalid use of"), quote("new"),
            words("on a constructor of type"), qual_type_ctor(TypeCtor),
            words("which is not existentially typed."), nl],
        Msgs = [simplest_msg(Context, Pieces)]
    ).

%---------------------------------------------------------------------------%

report_ambiguity_error(ClauseContext, Context, OverloadedSymbolMap,
        TypeAssign1, TypeAssign2) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    Pieces1 =
        [words("error: ambiguous overloading causes type ambiguity."), nl],
    VarSet = ClauseContext ^ tecc_varset,
    get_inst_varset(ClauseContext, InstVarSet),
    type_assign_get_var_types(TypeAssign1, VarTypes1),
    vartypes_vars(VarTypes1, Vars1),
    AmbiguityPieces = ambiguity_error_possibilities_to_pieces(add_quotes,
        VarSet, InstVarSet, Vars1, TypeAssign1, TypeAssign2),
    (
        AmbiguityPieces = [],
        Pieces2 = [],
        VerboseComponents = [],
        WarningMsgs = too_much_overloading_to_msgs(ClauseContext, Context,
            OverloadedSymbolMap, no)
    ;
        AmbiguityPieces = [_ | _],
        Pieces2 = [words("Possible type assignments include:"), nl
            | AmbiguityPieces],
        VerboseComponents =
            [verbose_only(verbose_once, add_qualifiers_reminder)],
        WarningMsgs = []
    ),

    MainMsg = simple_msg(Context,
        [always(InClauseForPieces ++ Pieces1 ++ Pieces2) | VerboseComponents]),
    Spec = error_spec($pred, severity_error, phase_type_check,
        [MainMsg | WarningMsgs]).

:- func add_qualifiers_reminder = list(format_piece).

add_qualifiers_reminder = [
    words("You will need to add an explicit type qualification"),
    words("to resolve the type ambiguity."),
    words("The way to add an explicit type qualification"),
    words("is to use \"with_type\"."),
    words("For details see the"), fixed("\"Explicit type qualification\""),
    words(" sub-section of the \"Data-terms\" section of the"),
    words("\"Syntax\" chapter of the Mercury language reference manual.")
].

:- func ambiguity_error_possibilities_to_pieces(maybe_add_quotes,
    prog_varset, inst_varset, list(prog_var), type_assign, type_assign)
    = list(format_piece).

ambiguity_error_possibilities_to_pieces(_AddQuotes, _VarSet, _InstVarSet,
        [], _TypeAssign1, _TypeAssign2) = [].
ambiguity_error_possibilities_to_pieces(AddQuotes, VarSet, InstVarSet,
        [Var | Vars], TypeAssign1, TypeAssign2) = Pieces :-
    type_assign_get_var_types(TypeAssign1, VarTypes1),
    type_assign_get_var_types(TypeAssign2, VarTypes2),
    type_assign_get_type_bindings(TypeAssign1, TypeBindings1),
    type_assign_get_type_bindings(TypeAssign2, TypeBindings2),
    type_assign_get_existq_tvars(TypeAssign1, ExistQTVars1),
    type_assign_get_existq_tvars(TypeAssign2, ExistQTVars2),
    ( if
        search_var_type(VarTypes1, Var, Type1),
        search_var_type(VarTypes2, Var, Type2),
        apply_rec_subst_to_type(TypeBindings1, Type1, T1),
        apply_rec_subst_to_type(TypeBindings2, Type2, T2),
        not identical_types(T1, T2)
    then
        type_assign_get_typevarset(TypeAssign1, TVarSet1),
        type_assign_get_typevarset(TypeAssign2, TVarSet2),
        UnnamedPiecesT1 = type_to_pieces(TVarSet1, InstVarSet,
            print_name_only, AddQuotes, ExistQTVars1, T1),
        UnnamedPiecesT2 = type_to_pieces(TVarSet2, InstVarSet,
            print_name_only, AddQuotes, ExistQTVars2, T2),
        ( if UnnamedPiecesT1 = UnnamedPiecesT2 then
            PiecesT1 = type_to_pieces(TVarSet1, InstVarSet,
                print_name_and_num, AddQuotes, ExistQTVars1, T1),
            PiecesT2 = type_to_pieces(TVarSet2, InstVarSet,
                print_name_and_num, AddQuotes, ExistQTVars2, T2)
        else
            PiecesT1 = UnnamedPiecesT1,
            PiecesT2 = UnnamedPiecesT2
        ),
        HeadPieces =
            [words(mercury_var_to_name_only_vs(VarSet, Var)), suffix(":")] ++
            [nl_indent_delta(1)] ++ PiecesT1 ++ [nl_indent_delta(-1)] ++
            [words("or")] ++
            [nl_indent_delta(1)] ++ PiecesT2 ++ [nl_indent_delta(-1)]
    else
        HeadPieces = []
    ),
    TailPieces = ambiguity_error_possibilities_to_pieces(AddQuotes,
        VarSet, InstVarSet, Vars, TypeAssign1, TypeAssign2),
    Pieces = HeadPieces ++ TailPieces.

    % Check whether two types are identical, i.e. whether they can be unified
    % without binding any type parameters.
    %
:- pred identical_types(mer_type::in, mer_type::in) is semidet.

identical_types(Type1, Type2) :-
    map.init(TypeSubst0),
    type_unify(Type1, Type2, [], TypeSubst0, TypeSubst),
    TypeSubst = TypeSubst0.

%---------------------------------------------------------------------------%

report_unsatisfiable_constraints(ClauseContext, Context, TypeAssignSet)
        = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    list.map_foldl(constraints_to_pieces, TypeAssignSet, ConstraintPieceLists,
        0, NumUnsatisfied),
    ( if NumUnsatisfied = 1 then
        Pieces1 = [words("unsatisfiable typeclass constraint:"), nl]
    else
        Pieces1 = [words("unsatisfiable typeclass constraints:"), nl]
    ),
    % XXX This won't be very pretty when there are multiple type_assigns.
    Pieces2 = component_list_to_line_pieces(ConstraintPieceLists,
        [suffix("."), nl]),
    Spec = simplest_spec($pred, severity_error, phase_type_check, Context,
        InClauseForPieces ++ Pieces1 ++ Pieces2).

:- pred constraints_to_pieces(type_assign::in, list(format_piece)::out,
    int::in, int::out) is det.

constraints_to_pieces(TypeAssign, Pieces, !NumUnsatisfied) :-
    type_assign_get_typeclass_constraints(TypeAssign, Constraints),
    UnprovenConstraints = Constraints ^ hcs_unproven,
    retrieve_prog_constraint_list(UnprovenConstraints,
        UnprovenProgConstraints0),

    type_assign_get_typevarset(TypeAssign, TVarSet),
    type_assign_get_type_bindings(TypeAssign, Bindings),
    apply_rec_subst_to_prog_constraint_list(Bindings,
        UnprovenProgConstraints0, UnprovenProgConstraints1),
    list.sort_and_remove_dups(UnprovenProgConstraints1,
        UnprovenProgConstraints),
    !:NumUnsatisfied = !.NumUnsatisfied + list.length(UnprovenProgConstraints),
    UnprovenProgConstraintStrings =
        list.map(mercury_constraint_to_string(TVarSet, print_name_only),
            UnprovenProgConstraints),
    UnprovenProgConstraintsPieces =
        list.map(wrap_quote, UnprovenProgConstraintStrings),
    Pieces = component_list_to_pieces("and", UnprovenProgConstraintsPieces).

:- func wrap_quote(string) = format_piece.

wrap_quote(Str) = quote(Str).

%---------------------------------------------------------------------------%

report_missing_tvar_in_foreign_code(ClauseContext, Context, VarName) = Spec :-
    ModuleInfo = ClauseContext ^ tecc_module_info,
    PredId = ClauseContext ^ tecc_pred_id,
    Pieces = [words("The foreign language code for") |
        describe_one_pred_name(ModuleInfo, should_module_qualify, PredId)] ++
        [words("should define the variable"), quote(VarName), suffix(".")],
    Spec = simplest_spec($pred, severity_error, phase_type_check,
        Context, Pieces).

%---------------------------------------------------------------------------%

report_invalid_coerce_from_to(ClauseContext, Context, TVarSet,
        FromType, ToType) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    FromTypeStr = mercury_type_to_string(TVarSet, print_num_only, FromType),
    ToTypeStr = mercury_type_to_string(TVarSet, print_num_only, ToType),
    Pieces = [words("cannot coerce from"), quote(FromTypeStr),
        words("to"), quote(ToTypeStr), suffix("."), nl],
    Msg = simplest_msg(Context, InClauseForPieces ++ Pieces),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The rest of this module contains utility predicates and functions
% for use by the code above.
%

:- func types_of_vars_to_pieces(prog_varset, inst_varset, type_assign_set,
    list(format_piece), prog_var, list(prog_var)) = list(format_piece).

types_of_vars_to_pieces(VarSet, InstVarSet, TypeAssignSet, FinalPieces,
        HeadVar, TailVars) = Pieces :-
    (
        TailVars = [],
        Pieces =
            argument_name_to_pieces(VarSet, HeadVar) ++
            type_of_var_to_pieces(InstVarSet, TypeAssignSet,
                FinalPieces, HeadVar)
    ;
        TailVars = [HeadTailVar | TailTailVars],
        Pieces =
            argument_name_to_pieces(VarSet, HeadVar) ++
            type_of_var_to_pieces(InstVarSet, TypeAssignSet,
                [suffix(","), nl], HeadVar) ++
            types_of_vars_to_pieces(VarSet, InstVarSet, TypeAssignSet,
                FinalPieces, HeadTailVar, TailTailVars)
    ).

:- func argument_name_to_pieces(prog_varset, prog_var)
    = list(format_piece).

argument_name_to_pieces(VarSet, Var) = Pieces :-
    ( if varset.search_name(VarSet, Var, _) then
        Pieces = [words("variable"),
            quote(mercury_var_to_name_only_vs(VarSet, Var))]
    else
        Pieces = [words("argument")]
    ).

:- func functor_name_to_pieces(cons_id, arity) = list(format_piece).

functor_name_to_pieces(Functor, Arity) = Pieces :-
    strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor),
    ( if Arity = 0 then
        Piece1 = words("constant"),
        ( if Functor = cons(Name, _, _) then
            Piece2 = qual_sym_name(Name)
        else
            Piece2 = quote(cons_id_and_arity_to_string(StrippedFunctor))
        ),
        Pieces = [Piece1, Piece2]
    else if Functor = cons(unqualified(""), _, _) then
        Pieces = [words("higher-order term (with arity"),
            int_fixed(Arity - 1), suffix(")")]
    else
        Pieces = [words("functor"), qual_cons_id_and_maybe_arity(Functor)]
    ).

:- func type_of_var_to_pieces(inst_varset, type_assign_set,
    list(format_piece), prog_var) = list(format_piece).

type_of_var_to_pieces(InstVarSet, TypeAssignSet, SuffixPieces, Var) = Pieces :-
    get_all_transformed_type_stuffs(
        typestuff_to_pieces(do_not_add_quotes, InstVarSet),
        TypeAssignSet, Var, TypePiecesLists0),
    list.sort_and_remove_dups(TypePiecesLists0, TypePiecesLists),
    ( if TypePiecesLists = [TypePieces] then
        Pieces = [words("has type"), nl_indent_delta(1)] ++
            component_list_to_line_pieces([TypePieces],
                SuffixPieces ++ [nl_indent_delta(-1)])
    else
        Pieces = [words("has overloaded type {"), nl_indent_delta(1)] ++
            component_list_to_line_pieces(TypePiecesLists,
                [nl_indent_delta(-1)]) ++
            [words("}")] ++ SuffixPieces
    ).

:- func type_of_functor_to_pieces(inst_varset, cons_id, arity,
    list(cons_type_info), list(format_piece)) = list(format_piece).

type_of_functor_to_pieces(InstVarSet, Functor, Arity, ConsDefnList,
        SuffixPieces) = Pieces :-
    ( if ConsDefnList = [SingleDefn] then
        ConsTypePieces = cons_type_to_pieces(InstVarSet, SingleDefn, Functor),
        ( if Arity = 0 then
            Pieces = [words("has type"), nl_indent_delta(1)] ++
                ConsTypePieces ++ SuffixPieces ++ [nl_indent_delta(-1)]
        else
            Pieces = [words("has type"), nl_indent_delta(1)] ++
                ConsTypePieces ++ SuffixPieces ++ [nl_indent_delta(-1)]
        )
    else
        ConsTypeListPieces =
            cons_type_list_to_pieces(InstVarSet, ConsDefnList, Functor, Arity),
        Pieces = [words("has overloaded type {"), nl_indent_delta(1)] ++
            ConsTypeListPieces ++ [nl_indent_delta(-1)] ++
            [fixed("}")] ++ SuffixPieces ++ [nl]
    ).

    % Return a description of the given data constructor's argument types.
    %
    % The caller should ensure that these pieces are indented one or two levels
    % to separate them from surrounding material.
    %
:- func cons_type_to_pieces(inst_varset, cons_type_info, cons_id)
    = list(format_piece).

cons_type_to_pieces(InstVarSet, ConsInfo, Functor) = Pieces :-
    ConsInfo = cons_type_info(TVarSet, ExistQVars, ConsType, ArgTypes, _, _),
    (
        ArgTypes = [_ | _],
        ( if Functor = cons(SymName, _Arity, _) then
            % What we construct in Type is not really a type: it is a
            % function symbol applied to a list of argument types. However
            % *syntactically*, it looks like a type, and we already have
            % code to print types, so we take a shortcut.
            Type = defined_type(SymName, ArgTypes, kind_star),
            ArgPieces =
                type_to_pieces(TVarSet, InstVarSet,
                    print_name_only, do_not_add_quotes, ExistQVars, Type) ++
                [suffix(":")]
        else
            unexpected($pred, "invalid cons_id")
        )
    ;
        ArgTypes = [],
        ArgPieces = []
    ),
    Pieces = ArgPieces ++
        type_to_pieces(TVarSet, InstVarSet, print_name_only, do_not_add_quotes,
            ExistQVars, ConsType).

    % Return a description of the  argument types of the given list of
    % data constructors.
    %
    % The caller should ensure that these pieces are indented one or two levels
    % to separate them from surrounding material.
    %
:- func cons_type_list_to_pieces(inst_varset, list(cons_type_info), cons_id,
    int) = list(format_piece).

cons_type_list_to_pieces(_, [], _, _) = [].
cons_type_list_to_pieces(InstVarSet, [ConsDefn | ConsDefns], Functor, Arity)
        = Pieces :-
    ThisPieces = cons_type_to_pieces(InstVarSet, ConsDefn, Functor),
    (
        ConsDefns = [],
        Pieces = ThisPieces
    ;
        ConsDefns = [_ | _],
        ( if Arity = 0 then
            ConnectPieces = [suffix(",")]
        else
            ConnectPieces = [suffix(","), nl]
        ),
        TailPieces = cons_type_list_to_pieces(InstVarSet, ConsDefns, Functor,
            Arity),
        Pieces = ThisPieces ++ ConnectPieces ++ TailPieces
    ).

    % Return a description of the current set of type assignments.
    %
    % Since this description can be very large and unwieldy, containing
    % much irrelevant information as well as (hopefully) one or two useful
    % pieces of information, it is intended to be used only with
    % --verbose-errors.
    %
:- func type_assign_set_msg_to_pieces(module_info, prog_varset,
    type_assign_set) = list(format_piece).

type_assign_set_msg_to_pieces(ModuleInfo, VarSet, TypeAssignSet) = Pieces :-
    ( if TypeAssignSet = [_] then
        FirstWords = "The partial type assignment was:",
        MaybeSeq = no
    else
        FirstWords = "The possible partial type assignments were:",
        MaybeSeq = yes(1)
    ),
    list.sort(TypeAssignSet, SortedTypeAssignSet),
    LaterPieces = type_assign_set_to_pieces(ModuleInfo, VarSet,
        SortedTypeAssignSet, MaybeSeq),
    Pieces = [words(FirstWords), nl_indent_delta(1) | LaterPieces] ++
        [nl_indent_delta(-1)].

    % Return a description of the current set of type assignments.
    %
    % Since this description can be very large and unwieldy, containing
    % much irrelevant information as well as (hopefully) one or two useful
    % pieces of information, it is intended to be used only with
    % --verbose-errors.
    %
:- func args_type_assign_set_msg_to_pieces(module_info, prog_varset,
    args_type_assign_set) = list(format_piece).

args_type_assign_set_msg_to_pieces(ModuleInfo, VarSet, ArgTypeAssignSet)
        = Pieces :-
    ( if ArgTypeAssignSet = [_] then
        FirstWords = "The partial type assignment was:",
        MaybeSeq = no
    else
        FirstWords = "The possible partial type assignments were:",
        MaybeSeq = yes(1)
    ),
    list.sort(ArgTypeAssignSet, SortedArgTypeAssignSet),
    LaterPieces = args_type_assign_set_to_pieces(ModuleInfo, VarSet,
        SortedArgTypeAssignSet, MaybeSeq),
    Pieces = [words(FirstWords), nl_indent_delta(1) | LaterPieces] ++
        [nl_indent_delta(-1)].

:- func type_stuff_to_actual_expected(maybe_add_quotes, inst_varset,
    mer_type, type_stuff) = actual_expected_types.

type_stuff_to_actual_expected(AddQuotes, InstVarSet, ExpectedType,
        VarTypeStuff) = ActualExpected :-
    VarTypeStuff = type_stuff(VarType, TVarSet, TypeBinding, ExistQTVars),
    strip_builtin_qualifiers_from_type(VarType, StrippedVarType),
    strip_builtin_qualifiers_from_type(ExpectedType, StrippedExpectedType),
    ActualPieces0 = bound_type_to_pieces(print_name_only, AddQuotes,
        TVarSet, InstVarSet, TypeBinding, ExistQTVars, StrippedVarType),
    ExpectedPieces0 = bound_type_to_pieces(print_name_only, AddQuotes,
        TVarSet, InstVarSet, TypeBinding, ExistQTVars, StrippedExpectedType),
    ( if ActualPieces0 = ExpectedPieces0 then
        ActualPieces = bound_type_to_pieces(print_name_and_num, AddQuotes,
            TVarSet, InstVarSet, TypeBinding, ExistQTVars, VarType),
        ExpectedPieces = bound_type_to_pieces(print_name_and_num, AddQuotes,
            TVarSet, InstVarSet, TypeBinding, ExistQTVars, ExpectedType),
        ActualExpected = actual_expected_types(ActualPieces, VarType,
            ExpectedPieces, ExpectedType, ExistQTVars, no)
    else
        ActualExpected = actual_expected_types(ActualPieces0, VarType,
            ExpectedPieces0, ExpectedType, ExistQTVars, no)
    ).

:- func arg_type_stuff_to_actual_expected(maybe_add_quotes, inst_varset,
    arg_type_stuff) = actual_expected_types.

arg_type_stuff_to_actual_expected(AddQuotes, InstVarSet, ArgTypeStuff)
        = ActualExpected :-
    ArgTypeStuff = arg_type_stuff(VarType, Source, ExpectedType,
        TVarSet, ExistQTVars),
    strip_builtin_qualifiers_from_type(VarType, StrippedVarType),
    strip_builtin_qualifiers_from_type(ExpectedType, StrippedExpectedType),
    ActualPieces0 = type_to_pieces(TVarSet, InstVarSet, print_name_only,
        AddQuotes, ExistQTVars, StrippedVarType),
    ExpectedPieces0 = type_to_pieces(TVarSet, InstVarSet, print_name_only,
        AddQuotes, ExistQTVars, StrippedExpectedType),
    ( if ActualPieces0 = ExpectedPieces0 then
        ActualPieces = type_to_pieces(TVarSet, InstVarSet,
            print_name_and_num, AddQuotes, ExistQTVars, VarType),
        ExpectedPieces = type_to_pieces(TVarSet, InstVarSet,
            print_name_and_num, AddQuotes, ExistQTVars, ExpectedType),
        ActualExpected = actual_expected_types(ActualPieces, VarType,
            ExpectedPieces, ExpectedType, ExistQTVars, yes(Source))
    else
        ActualExpected = actual_expected_types(ActualPieces0, VarType,
            ExpectedPieces0, ExpectedType, ExistQTVars, yes(Source))
    ).

:- func actual_expected_types_list_to_pieces(module_info,
    list(actual_expected_types)) = list(format_piece).

actual_expected_types_list_to_pieces(ModuleInfo, ActualExpectedList)
        = Pieces :-
    % XXX Printing all the actual types and then all the expected types
    % seems to me (zs) to be a bad idea. If different elements of
    % ActualExpectedList cause the printing of different actual types,
    % then it would seem to make sense to put each of those actual types
    % next to the expected type from the *same* element.
    %
    % Without this, printing the source of the expectation for each
    % expected type would be meaningless.
    %
    % XXX The part of the message before the pieces we return here
    % talk about inferred/expected types *in that order*, so printing
    % the expected types before the inferred ones here seems strange.
    list.foldl(acc_expected_type_pieces(ModuleInfo, print_expected),
        ActualExpectedList, [], ExpectedPieces),
    ActualPieces = list.map(actual_types_to_pieces, ActualExpectedList),
    Pieces =
        component_list_to_line_pieces(ExpectedPieces ++ ActualPieces, [nl]).

:- type maybe_print_expectation_source
    --->    print_expected
    ;       print_expectation_source.

:- pred acc_expected_type_pieces(module_info::in,
    maybe_print_expectation_source::in, actual_expected_types::in,
    list(list(format_piece))::in, list(list(format_piece))::out) is det.

acc_expected_type_pieces(ModuleInfo, MaybePrintSource, ActualExpected,
        !TaggedPieceLists) :-
    ActualExpected = actual_expected_types(_ActualPieces, _ActualType,
        ExpectedPieces, _ExpectedType, _ExistQTVars, MaybeSource),
    ( if
        MaybePrintSource = print_expectation_source,
        MaybeSource = yes(Source)
    then
        SourcePieces = describe_args_type_assign_source(ModuleInfo, Source),
        % We add a newline after the "(expected by ...):" text for two reasons:
        %
        % - because SourcePieces is likely to take up a large chunk
        %   of the line anyway, and
        % - because this (or something very similar) is needed to ensure
        %   that the different expected type pieces line up exactly with
        %   (a) the inferred type pieces, and (b) each other.
        TaggedPieces = [words("the type expected by") | SourcePieces] ++
            [words("is:"), nl | ExpectedPieces]
    else
        TaggedPieces = [words("(expected)") | ExpectedPieces]
    ),
    ( if list.member(TaggedPieces, !.TaggedPieceLists) then
        true
    else
        !:TaggedPieceLists = !.TaggedPieceLists ++ [TaggedPieces]
    ).

:- pred acc_expected_type_source_pieces(module_info::in,
    maybe_print_expectation_source::in, list(actual_expected_types)::in,
    set(pair(list(format_piece)))::out, list(format_piece)::out) is det.

acc_expected_type_source_pieces(_, _, [], set.init, []).
acc_expected_type_source_pieces(ModuleInfo, MaybePrintSource,
        [ActualExpected | ActualExpecteds],
        SourceExpectedPairs, AllTaggedPieces) :-
    acc_expected_type_source_pieces(ModuleInfo, MaybePrintSource,
        ActualExpecteds, TailSourceExpectedPairs, TailTaggedPieces),
    ActualExpected = actual_expected_types(_ActualPieces, _ActualType,
        ExpectedPieces, _ExpectedType, _ExistQTVars, MaybeSource),
    (
        TailTaggedPieces = [],
        CommaOrPeriod = "."
    ;
        TailTaggedPieces = [_ | _],
        CommaOrPeriod = ","
    ),
    ( if
        MaybePrintSource = print_expectation_source,
        MaybeSource = yes(Source)
    then
        SourcePieces = describe_args_type_assign_source(ModuleInfo, Source),
        % We add a newline after the "(expected by ...):" text for two reasons:
        %
        % - because SourcePieces is likely to take up a large chunk
        %   of the line anyway, and
        % - because this (or something very similar) is needed to ensure
        %   that the different expected type pieces line up exactly with
        %   (a) the inferred type pieces, and (b) each other.
        TaggedPieces = [words("the type expected by") | SourcePieces] ++
            [words("is:"), nl_indent_delta(1) | ExpectedPieces] ++
            [suffix(CommaOrPeriod), nl_indent_delta(-1)]
    else
        SourcePieces = [],
        TaggedPieces = [words("one expected type is:"),
            nl_indent_delta(1) | ExpectedPieces] ++
            [suffix(CommaOrPeriod), nl_indent_delta(-1)]
    ),
    % We can't test whether we have printed a SourcePieces/ExpectedPieces
    % pair by testing TailTaggedPieceLists due to the possibility of false
    % negatives due to a comma vs period mismatch, so we test it directly.
    SourceExpectedPair = SourcePieces - ExpectedPieces,
    ( if set.member(SourceExpectedPair, TailSourceExpectedPairs) then
        SourceExpectedPairs = TailSourceExpectedPairs,
        AllTaggedPieces = TailTaggedPieces
    else
        set.insert(SourceExpectedPair,
            TailSourceExpectedPairs, SourceExpectedPairs),
        AllTaggedPieces = TaggedPieces ++ TailTaggedPieces
    ).

:- func actual_types_to_pieces(actual_expected_types) = list(format_piece).

actual_types_to_pieces(ActualExpected) = Pieces :-
    ActualExpected = actual_expected_types(ActualPieces, _ActualType,
        _ExpectedPieces, _ExpectedType, _ExistQTVars, _Source),
    Pieces = [words("(inferred)") | ActualPieces].

:- func bound_type_to_pieces(var_name_print, maybe_add_quotes,
    tvarset, inst_varset, tsubst, list(tvar), mer_type)
    = list(format_piece).

bound_type_to_pieces(VarNamePrint, AddQuotes, TVarSet, InstVarSet,
        TypeBindings, ExistQTVars, Type0) = Pieces :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type),
    Pieces = type_to_pieces(TVarSet, InstVarSet, VarNamePrint, AddQuotes,
        ExistQTVars, Type).

%---------------------------------------------------------------------------%

:- func type_assign_set_to_pieces(module_info, prog_varset, type_assign_set,
    maybe(int)) = list(format_piece).

type_assign_set_to_pieces(_, _, [], _) = [].
type_assign_set_to_pieces(ModuleInfo, VarSet, [TypeAssign | TypeAssigns],
        MaybeSeq) =
    type_assign_to_pieces(ModuleInfo, VarSet, TypeAssign, no, MaybeSeq) ++
    type_assign_set_to_pieces(ModuleInfo, VarSet, TypeAssigns,
        inc_maybe_seq(MaybeSeq)).

:- func args_type_assign_set_to_pieces(module_info, prog_varset,
    args_type_assign_set, maybe(int)) = list(format_piece).

args_type_assign_set_to_pieces(_, _, [], _) = [].
args_type_assign_set_to_pieces(ModuleInfo, VarSet,
        [ArgTypeAssign | ArgTypeAssigns], MaybeSeq) = Pieces :-
    % XXX Why does this simply pick the TypeAssign part of the ArgTypeAssign,
    % instead of invoking convert_args_type_assign?
    ArgTypeAssign = args_type_assign(TypeAssign, _ArgTypes, _Constraints,
        Source),
    Pieces =
        type_assign_to_pieces(ModuleInfo, VarSet, TypeAssign, yes(Source),
            MaybeSeq) ++
        args_type_assign_set_to_pieces(ModuleInfo, VarSet, ArgTypeAssigns,
            inc_maybe_seq(MaybeSeq)).

%---------------------%

:- func type_assign_to_pieces(module_info, prog_varset, type_assign,
    maybe(args_type_assign_source), maybe(int)) = list(format_piece).

type_assign_to_pieces(ModuleInfo, VarSet, TypeAssign, MaybeSource, MaybeSeq)
        = Pieces :-
    (
        MaybeSeq = yes(N),
        ( if
            MaybeSource = yes(Source),
            SourcePieces0 =
                describe_args_type_assign_source(ModuleInfo, Source),
            SourcePieces0 = [_ | _]
        then
            SourcePieces = [suffix(","), words("derived from") | SourcePieces0]
        else
            SourcePieces = []
        ),
        SeqPieces0 = [words("Type assignment"), int_fixed(N)] ++
            SourcePieces ++ [suffix(":"), nl],
        ( if N > 1 then
            SeqPieces = [blank_line | SeqPieces0]
        else
            SeqPieces = SeqPieces0
        )
    ;
        MaybeSeq = no,
        SeqPieces = []
    ),
    type_assign_get_existq_tvars(TypeAssign, ExistQTVars),
    type_assign_get_var_types(TypeAssign, VarTypes),
    type_assign_get_typeclass_constraints(TypeAssign, Constraints),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TypeVarSet),
    vartypes_vars(VarTypes, Vars),
    (
        ExistQTVars = [],
        HeadPieces = []
    ;
        ExistQTVars = [_ | _],
        VarsStr = mercury_vars_to_string_vs(TypeVarSet, varnums, ExistQTVars),
        HeadPieces = [words("some [" ++ VarsStr ++ "]"), nl]
    ),
    TypePieces = type_assign_types_to_pieces(VarSet, VarTypes, TypeVarSet,
        TypeBindings, no, Vars),
    ConstraintPieces = type_assign_hlds_constraints_to_pieces(Constraints,
        TypeBindings, TypeVarSet),
    Pieces = SeqPieces ++ HeadPieces ++ TypePieces ++ ConstraintPieces ++ [nl].

:- func type_assign_types_to_pieces(prog_varset, vartypes, tvarset,
    tsubst, bool, list(prog_var)) = list(format_piece).

type_assign_types_to_pieces(_, _, _, _, FoundOne, []) = Pieces :-
    (
        FoundOne = no,
        Pieces = [words("(No variables were assigned a type)")]
    ;
        FoundOne = yes,
        Pieces = []
    ).
type_assign_types_to_pieces(VarSet, VarTypes, TypeVarSet, TypeBindings,
        FoundOne, [Var | Vars]) = Pieces :-
    ( if search_var_type(VarTypes, Var, Type) then
        (
            FoundOne = yes,
            PrefixPieces = [nl]
        ;
            FoundOne = no,
            PrefixPieces = []
        ),
        VarStr = mercury_var_to_string_vs(VarSet, varnums, Var),
        TypeStr = type_with_bindings_to_string(Type, TypeVarSet, TypeBindings),
        AssignPieces = [fixed(VarStr), suffix(":"), words(TypeStr)],
        TailPieces = type_assign_types_to_pieces(VarSet, VarTypes,
            TypeVarSet, TypeBindings, yes, Vars),
        Pieces = PrefixPieces ++ AssignPieces ++ TailPieces
    else
        Pieces = type_assign_types_to_pieces(VarSet, VarTypes,
            TypeVarSet, TypeBindings, FoundOne, Vars)
    ).

:- func type_with_bindings_to_string(mer_type, tvarset, tsubst) = string.

type_with_bindings_to_string(Type0, TypeVarSet, TypeBindings) = Str :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_builtin_qualifiers_from_type(Type1, Type),
    Str = mercury_type_to_string(TypeVarSet, print_name_only, Type).

:- func type_assign_hlds_constraints_to_pieces(hlds_constraints,
    tsubst, tvarset) = list(format_piece).

type_assign_hlds_constraints_to_pieces(Constraints, TypeBindings, TypeVarSet)
        = Pieces1 ++ Pieces2 :-
    Constraints =
        hlds_constraints(ConstraintsToProve, AssumedConstraints, _, _),
    PiecesList1 = type_assign_constraints_to_pieces_list("&",
        AssumedConstraints, TypeBindings, TypeVarSet, no),
    PiecesList2 = type_assign_constraints_to_pieces_list("<=",
        ConstraintsToProve, TypeBindings, TypeVarSet, no),
    Pieces1 = component_list_to_line_pieces(PiecesList1, [nl]),
    Pieces2 = component_list_to_line_pieces(PiecesList2, [nl]).

:- func type_assign_constraints_to_pieces_list(string, list(hlds_constraint),
    tsubst, tvarset, bool) = list(list(format_piece)).

type_assign_constraints_to_pieces_list(_, [], _, _, _) = [].
type_assign_constraints_to_pieces_list(Operator, [Constraint | Constraints],
        TypeBindings, TypeVarSet, FoundOne) = [ThisPieces] ++ TailPieceLists :-
    (
        FoundOne = no,
        Prefix = Operator ++ " "
    ;
        FoundOne = yes,
        Prefix = "   "
    ),
    apply_rec_subst_to_constraint(TypeBindings, Constraint, BoundConstraint),
    retrieve_prog_constraint(BoundConstraint, ProgConstraint),
    ThisPieces = [fixed(Prefix ++
        mercury_constraint_to_string(TypeVarSet, print_name_only,
            ProgConstraint))],
    TailPieceLists = type_assign_constraints_to_pieces_list(Operator,
        Constraints, TypeBindings, TypeVarSet, yes).

:- func inc_maybe_seq(maybe(int)) = maybe(int).

inc_maybe_seq(no) = no.
inc_maybe_seq(yes(N)) = yes(N + 1).

:- func varnums = var_name_print.

varnums = print_name_and_num.

%---------------------------------------------------------------------------%

:- pred maybe_report_missing_import_addendum(type_error_clause_context::in,
    module_name::in, list(format_piece)::out, list(module_name)::out)
    is det.

maybe_report_missing_import_addendum(ClauseContext, ModuleQualifier,
        Pieces, MissingImportModules) :-
    % First check if this module wasn't imported.
    ModuleInfo = ClauseContext ^ tecc_module_info,
    module_info_get_visible_modules(ModuleInfo, VisibleModules),

    set.filter(partial_sym_name_matches_full(ModuleQualifier),
        VisibleModules, MatchingVisibleModules),
    ( if set.is_empty(MatchingVisibleModules) then
        % The module qualifier does not match any of the visible modules,
        % so we report that the module has not been imported.
        Pieces = [nl, words("(The module"), qual_sym_name(ModuleQualifier),
            words("has not been imported.)"), nl],
        MissingImportModules = [ModuleQualifier]
    else
        % The module qualifier matches one or more of the visible modules.
        % But maybe the user forgot to import the parent module(s) of that
        % module...
        find_unimported_ancestors(VisibleModules, MatchingVisibleModules,
            UnimportedAncestorsSet),
        set.to_sorted_list(UnimportedAncestorsSet, UnimportedAncestors),
        (
            UnimportedAncestors = [_ | _],
            Pieces = report_unimported_ancestors(UnimportedAncestors),
            % Since ModuleQualifier has unimported parents, its own import,
            % if any, has no effect.
            ModuleQualifierList = sym_name_to_list(ModuleQualifier),
            AddAncestor =
                ( func(AncestorMN) = MN :-
                    AncestorMNList = sym_name_to_list(AncestorMN),
                    FullList = AncestorMNList ++ ModuleQualifierList,
                    det_list_to_sym_name(FullList, MN)
                ),
            MissingImportModules =
                [ModuleQualifier | list.map(AddAncestor, UnimportedAncestors)]
        ;
            UnimportedAncestors = [],
            Pieces = [],
            MissingImportModules = []
        )
    ).

    % Return all the ancestors of modules in MatchingModuleNames
    % which are not imported (i.e. not visible).
    %
:- pred find_unimported_ancestors(set(module_name)::in, set(module_name)::in,
    set(module_name)::out) is det.

find_unimported_ancestors(VisibleModules, MatchingModuleNames,
        UnimportedAncestors) :-
    MatchingModuleNamesAncestorSets =
        set.map(get_ancestors_set, MatchingModuleNames),
    set.power_union(MatchingModuleNamesAncestorSets,
        MatchingModuleNamesAncestors),
    set.difference(MatchingModuleNamesAncestors, VisibleModules,
        UnimportedAncestors).

:- func report_unimported_ancestors(list(module_name))
    = list(format_piece).

report_unimported_ancestors(UnimportedAncestors) = Pieces :-
    UnimportedAncestorDescs = list.map(describe_sym_name, UnimportedAncestors),
    AllUnimportedAncestors = list_to_pieces(UnimportedAncestorDescs),
    ( if AllUnimportedAncestors = [_] then
        Pieces = [words("(The possible parent module")] ++
            AllUnimportedAncestors ++ [words("has not been imported.)"), nl]
    else
        Pieces = [words("(The possible parent modules")] ++
            AllUnimportedAncestors ++ [words("have not been imported.)"), nl]
    ).

%---------------------------------------------------------------------------%

:- func report_any_missing_module_qualifiers(type_error_clause_context,
    prog_context, string, set(module_name)) = list(error_msg).

report_any_missing_module_qualifiers(ClauseContext, Context,
        ItemName, ModuleNamesSet0) = Msgs :-
    % We take a set of module names instead of a list, because we want
    % to report the name of each module just once, even if it defines
    % more than one entity with the name that got the error.
    % (Our caller can put the module name into the set more than once:
    % once for each such entity.)

    % For entities defined in the current module and in its ancestors,
    % access via use_module vs import_module is irrelevant.
    ModuleInfo = ClauseContext ^ tecc_module_info,
    module_info_get_name(ModuleInfo, ModuleName),
    set.delete_list([ModuleName | get_ancestors(ModuleName)],
        ModuleNamesSet0, ModuleNamesSet1),
    % Users are not supposed to know about private_builtin.m at all.
    set.delete(mercury_private_builtin_module,
        ModuleNamesSet1, ModuleNamesSet),
    set.to_sorted_list(ModuleNamesSet, ModuleNames),
    (
        ModuleNames = [],
        Msgs = []
    ;
        ModuleNames = [HeadModuleName | TailModuleNames],
        (
            TailModuleNames = [],
            MainPieces = [words("That"), words(ItemName), words("is defined"),
                words("in module"), qual_sym_name(HeadModuleName), suffix(","),
                words("which does not have an"),
                decl("import_module"), words("declaration."), nl]
        ;
            TailModuleNames = [_ | _],
            ModuleNamePieces =
                list.map(func(MN) = qual_sym_name(MN), ModuleNames),
            ModuleNamesPieces =
                component_list_to_pieces("and", ModuleNamePieces),
            MainPieces = [words("That"), words(ItemName), words("is defined"),
                words("in modules")] ++ ModuleNamesPieces ++ [suffix(","),
                words("none of which have"),
                decl("import_module"), words("declarations."), nl]
        ),
        MainMsg = simplest_msg(Context, MainPieces),
        VerbosePieces = [words("Note that symbols defined in modules"),
            words("accessed via"), decl("use_module"), words("declarations"),
            words("must always be fully module qualified."), nl],
        VerboseMsg = simple_msg(Context,
            [verbose_only(verbose_once, VerbosePieces)]),
        Msgs = [MainMsg, VerboseMsg]
    ).

%---------------------------------------------------------------------------%

:- func goal_context_to_pieces(type_error_clause_context,
    type_error_goal_context) = list(format_piece).

goal_context_to_pieces(ClauseContext, GoalContext) = Pieces :-
    (
        GoalContext = type_error_in_var_vector(VarVectorKind, ArgNum),
        (
            VarVectorKind = var_vector_args(ArgVectorKind),
            (
                ArgVectorKind = arg_vector_clause_head,
                Pieces = [words("in argument"),
                    invis_order_default_end(ArgNum),
                    int_fixed(ArgNum), words("of the clause head:"), nl]
            ;
                (
                    ArgVectorKind =
                        arg_vector_plain_pred_call(SymNamePredFormArity),
                    SymNamePredFormArity =
                        sym_name_pred_form_arity(SymName, PredFormArity),
                    PFSymNameArity = pf_sym_name_arity(pf_predicate, SymName,
                        PredFormArity),
                    CallId = plain_call_id(PFSymNameArity)
                ;
                    ArgVectorKind = arg_vector_plain_call_pred_id(PredId),
                    ModuleInfo = ClauseContext ^ tecc_module_info,
                    module_info_pred_info(ModuleInfo, PredId, PredInfo),
                    pred_info_get_pf_sym_name_arity(PredInfo, PFSymNameArity),
                    CallId = plain_call_id(PFSymNameArity)
                ;
                    ArgVectorKind = arg_vector_generic_call(GenericId),
                    CallId = generic_call_id(GenericId)
                ),
                PredMarkers = ClauseContext ^ tecc_pred_markers,
                Pieces = [words("in"),
                    words(call_arg_id_to_string(CallId, ArgNum, PredMarkers)),
                    suffix(":"), nl]
            ;
                ArgVectorKind = arg_vector_foreign_proc_call(_PredId),
                % During typechecking, call_foreign_proc goals can occur
                % only in foreign clauses derived from foreign_proc
                % "clauses". Any error we report for them is for the
                % foreign_proc as a whole, not for a "call" to the
                % foreign_proc.
                %
                % Our caller will prefix the Pieces we return here
                % with the identification of the predicate or function
                % whose foreign_proc the error is for, which is all
                % the context that the error needs. So there is nothing
                % useful we can add here.
                Pieces = []
            ;
                ArgVectorKind = arg_vector_event(EventName),
                Pieces = [words("in argument"),
                    invis_order_default_end(ArgNum), int_fixed(ArgNum),
                    words("of event"), fixed(EventName), suffix(":"), nl]
            )
        ;
            VarVectorKind = var_vector_cond_quant,
            Pieces = [words("in the"), invis_order_default_end(ArgNum),
                nth_fixed(ArgNum),
                words("quantified variable in if-then-else condition:"), nl]
        ;
            VarVectorKind = var_vector_exist_quant,
            Pieces = [words("in the"), invis_order_default_end(ArgNum),
                nth_fixed(ArgNum),
                words("variable of quantification scope:"), nl]
        ;
            VarVectorKind = var_vector_promise_solutions,
            Pieces = [words("in the"), invis_order_default_end(ArgNum),
                nth_fixed(ArgNum),
                words("variable of promise_solutions scope:"), nl]
        ;
            VarVectorKind = var_vector_switch_complete,
            Pieces = [words("in the"),
                words("variable of require_switch_complete scope:"), nl]
        ;
            VarVectorKind = var_vector_switch_arm_detism,
            Pieces = [words("in the"),
                words("variable of require_switch_arm_detism scope:"), nl]
        ;
            VarVectorKind = var_vector_loop_control,
            Pieces = [words("in the"), invis_order_default_end(ArgNum),
                nth_fixed(ArgNum),
                words("variable of loop control scope:"), nl]
        ;
            VarVectorKind = var_vector_try_io,
            ( if ArgNum = 1 then
                Pieces = [invis_order_default_end(1),
                    words("in initial I/O state variable of try goal:"), nl]
            else if ArgNum = 2 then
                Pieces = [invis_order_default_end(2),
                    words("in final I/O state variable of try goal:"), nl]
            else
                unexpected($pred, "try io variable not arg 1 or 2")
            )
        ;
            VarVectorKind = var_vector_atomic_output,
            Pieces = [words("in the"), invis_order_default_end(ArgNum),
                nth_fixed(ArgNum), words("output variable of atomic goal:"),
                nl]
        ;
            VarVectorKind = var_vector_atomic_outer,
            ( if ArgNum = 1 then
                Pieces = [invis_order_default_end(1),
                    words("in the first outer variable"),
                    words("of atomic goal:"), nl]
            else if ArgNum = 2 then
                Pieces = [invis_order_default_end(2),
                    words("in the second outer variable"),
                    words("of atomic goal:"), nl]
            else
                unexpected($pred, "outer variable not arg 1 or 2")
            )
        )
    ;
        GoalContext = type_error_in_unify(UnifyContext),
        unify_context_to_pieces(UnifyContext, [], Pieces)
    ;
        GoalContext = type_error_in_atomic_inner,
        Pieces = [words("in inner variable of atomic goal:"), nl]
    ).

:- func arg_vector_kind_to_pieces(type_error_clause_context, arg_vector_kind)
    = list(format_piece).

arg_vector_kind_to_pieces(ClauseContext, ArgVectorKind) = Pieces :-
    (
        ArgVectorKind = arg_vector_clause_head,
        Pieces = [words("in arguments of the clause head:"), nl]
    ;
        (
            ArgVectorKind = arg_vector_plain_pred_call(SymNamePredFormArity),
            SymNamePredFormArity =
                sym_name_pred_form_arity(SymName, PredFormArity),
            PFSymNameArity =
                pf_sym_name_arity(pf_predicate, SymName, PredFormArity),
            CallId = plain_call_id(PFSymNameArity)
        ;
            ArgVectorKind = arg_vector_plain_call_pred_id(PredId),
            ModuleInfo = ClauseContext ^ tecc_module_info,
            module_info_pred_info(ModuleInfo, PredId, PredInfo),
            pred_info_get_pf_sym_name_arity(PredInfo, PFSymNameArity),
            CallId = plain_call_id(PFSymNameArity)
        ;
            ArgVectorKind = arg_vector_generic_call(GenericId),
            CallId = generic_call_id(GenericId)
        ),
        PredMarkers = ClauseContext ^ tecc_pred_markers,
        Pieces = [words("in"),
            words(call_arg_id_to_string(CallId, -1, PredMarkers)),
            suffix(":"), nl]
    ;
        ArgVectorKind = arg_vector_foreign_proc_call(_PredId),
        unexpected($pred, "arg_vector_foreign_proc_call")
    ;
        ArgVectorKind = arg_vector_event(EventName),
        Pieces = [words("in arguments of event"), fixed(EventName),
            suffix(":"), nl]
    ).

    % This function generates the preamble (initial part of) all type error
    % messages, giving the name of the predicate or function in which the error
    % occurred.
    %
:- func in_clause_for_pieces(type_error_clause_context) =
    list(format_piece).

in_clause_for_pieces(ClauseContext) = Pieces :-
    ModuleInfo = ClauseContext ^ tecc_module_info,
    PredId = ClauseContext ^ tecc_pred_id,
    PredIdPieces = describe_one_pred_name(ModuleInfo,
        should_not_module_qualify, PredId),
    Pieces = [words("In clause for") | PredIdPieces] ++ [suffix(":"), nl].

    % error_num_args_to_pieces(PredOrFunc, Arity, CorrectArities):
    %
    % Return a description for the error message
    % "wrong number of arguments (<Arity>; should be <CorrectArities>)",
    % adjusting `Arity' and `CorrectArities' if `MaybePredOrFunc' is
    % `yes(function)'.
    %
:- func error_num_args_to_pieces(pred_or_func, int, list(int)) =
    list(format_piece).

error_num_args_to_pieces(PredOrFunc, Arity0, Arities0) = Pieces :-
    (
        PredOrFunc = pf_predicate,
        Arity = Arity0,
        Arities = Arities0
    ;
        PredOrFunc = pf_function,
        % Adjust arities for functions.
        adjust_func_arity(pf_function, Arity, Arity0),
        ReverseAdjust =
            ( pred(OtherArity0::in, OtherArity::out) is det :-
                adjust_func_arity(pf_function, OtherArity, OtherArity0)
            ),
        list.map(ReverseAdjust, Arities0, Arities)
    ),
    RightAritiesPieces = error_right_num_args_to_pieces(Arities),
    Pieces = [words("wrong number of arguments ("),
        suffix(int_to_string(Arity)), suffix(";"),
        words("should be") | RightAritiesPieces] ++ [suffix(")")].

:- func error_right_num_args_to_pieces(list(int)) = list(format_piece).

error_right_num_args_to_pieces([]) = [].
error_right_num_args_to_pieces([Arity | Arities]) = Pieces :-
    TailPieces = error_right_num_args_to_pieces(Arities),
    ArityPiece = fixed(int_to_string(Arity)),
    (
        Arities = [],
        Pieces = [ArityPiece | TailPieces]
    ;
        Arities = [_],
        Pieces = [ArityPiece, words("or") | TailPieces]
    ;
        Arities = [_, _ | _],
        Pieces = [ArityPiece, suffix(",") | TailPieces]
    ).

%---------------------------------------------------------------------------%

:- type type_stuff
    --->    type_stuff(
                type_stuff_base_type            :: mer_type,
                type_stuff_tvarset              :: tvarset,
                type_stuff_binding              :: tsubst,
                type_stuff_existq_tvars         :: list(tvar)
            ).

    % Given a type assignment set and a variable, return the list of possible
    % different types for the variable, removing all duplicates.
    % The check for duplicates makes this algorithm O(N^2), which can be
    % a problem. In addition, the equality test unifications done by
    % list.member compare type_stuffs starting with the base_type field,
    % which (in the extremely limited deep profiling sample consisting
    % of just one run that motivated this comment) always compares equal,
    % unlike the second and third fields of type_stuff.
    %
:- pred get_all_type_stuffs_remove_dups(type_assign_set::in, prog_var::in,
    list(type_stuff)::out) is det.

get_all_type_stuffs_remove_dups([], _Var, []).
get_all_type_stuffs_remove_dups([TypeAssign | TypeAssigns], Var, TypeStuffs) :-
    get_all_type_stuffs_remove_dups(TypeAssigns, Var, TailTypeStuffs),
    get_type_stuff(TypeAssign, Var, TypeStuff),
    ( if list.member(TypeStuff, TailTypeStuffs) then
        TypeStuffs = TailTypeStuffs
    else
        TypeStuffs = [TypeStuff | TailTypeStuffs]
    ).

    % Given a type assignment set and a variable, return the list of possible
    % different types for the variable. The returned list may contain
    % duplicates.
    %
:- pred get_all_type_stuffs(type_assign_set::in, prog_var::in,
    list(type_stuff)::out) is det.
% Some XXX comments above describe scenarios in which this code
% could be needed.
:- pragma consider_used(pred(get_all_type_stuffs/3)).

get_all_type_stuffs([], _Var, []).
get_all_type_stuffs([TypeAssign | TypeAssigns], Var,
        [TypeStuff | TypeStuffs]) :-
    get_type_stuff(TypeAssign, Var, TypeStuff),
    get_all_type_stuffs(TypeAssigns, Var, TypeStuffs).

    % Given a type assignment set and a variable, return the result of
    % applying the given function to the list of the possible different types
    % for the variable. The returned list may contain duplicates.
    %
    % We *could* eliminate duplicates here piecemeal as they are generated,
    % as get_all_type_stuffs_remove_dups does, but that is an quadratic
    % algorithm, and our callers typically call list.sort_and_remove_dups
    % on the result, which removes duplicates at a linear cost over the
    % usually O(N log N) cost of the sorting itself.
    %
    % However, the much bigger win is that each result is typically
    % smaller than the type_stuff it is derived from, because
    %
    % - the tvarsets in type_stuffs are often big, while
    % - the result is a type in some form, whose size is typcally small.
    %
    % And if the results are smaller than the type_stuffs, then comparing
    % should be faster as well.
    %
:- pred get_all_transformed_type_stuffs((func(type_stuff) = T)::in,
    type_assign_set::in, prog_var::in, list(T)::out) is det.

get_all_transformed_type_stuffs(_TransformFunc, [], _Var, []).
get_all_transformed_type_stuffs(TransformFunc, [TypeAssign | TypeAssigns], Var,
        [Result | Results]) :-
    get_type_stuff(TypeAssign, Var, TypeStuff),
    Result = TransformFunc(TypeStuff),
    get_all_transformed_type_stuffs(TransformFunc, TypeAssigns, Var, Results).

    % Given a type assignment and a variable, return information about
    % the type of that variable in that type assignment.
    %
:- pred get_type_stuff(type_assign::in, prog_var::in, type_stuff::out) is det.

get_type_stuff(TypeAssign, Var, TypeStuff) :-
    type_assign_get_existq_tvars(TypeAssign, ExistQTVars),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TVarSet),
    type_assign_get_var_types(TypeAssign, VarTypes),
    ( if search_var_type(VarTypes, Var, Type0) then
        Type = Type0
    else
        % This shouldn't happen - how can a variable which has not yet been
        % assigned a type variable fail to have the correct type?
        Type = defined_type(unqualified("<any>"), [], kind_star)
    ),
    TypeStuff = type_stuff(Type, TVarSet, TypeBindings, ExistQTVars).

:- func typestuff_to_type(type_stuff) = mer_type.

typestuff_to_type(TypeStuff) = Type :-
    TypeStuff = type_stuff(Type0, _TypeVarSet, TypeBindings, _ExistQTVars),
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_builtin_qualifiers_from_type(Type1, Type).

:- func typestuff_to_pieces(maybe_add_quotes, inst_varset, type_stuff)
    = list(format_piece).

typestuff_to_pieces(AddQuotes, InstVarSet, TypeStuff) = Pieces :-
    Type = typestuff_to_type(TypeStuff),
    TypeStuff = type_stuff(_Type0, TypeVarSet, _TypeBindings, ExistQTVars),
    Pieces = type_to_pieces(TypeVarSet, InstVarSet, print_name_only, AddQuotes,
        ExistQTVars, Type).

:- type arg_type_stuff
    --->    arg_type_stuff(
                arg_type_stuff_var_type             :: mer_type,
                arg_type_stuff_source               :: args_type_assign_source,
                arg_type_stuff_arg_type             :: mer_type,
                arg_type_stuff_tvarset              :: tvarset,
                arg_type_stuff_existq_tvars         :: list(tvar)
            ).

    % Given a variable and an arg type assignment set, return the list of
    % the possible different types for the variable and the argument.
    %
:- pred get_arg_type_stuffs(prog_var::in, args_type_assign_set::in,
    list(arg_type_stuff)::out) is det.

get_arg_type_stuffs(_Var, [], []).
get_arg_type_stuffs(Var, [ArgTypeAssign | ArgTypeAssigns], ArgTypeStuffs) :-
    get_arg_type_stuffs(Var, ArgTypeAssigns, TailArgTypeStuffs),
    get_arg_type_stuff(Var, ArgTypeAssign, TailArgTypeStuffs, ArgTypeStuffs).

:- pred get_arg_type_stuff(prog_var::in, args_type_assign::in,
    list(arg_type_stuff)::in, list(arg_type_stuff)::out) is det.
:- pragma inline(pred(get_arg_type_stuff/4)).

get_arg_type_stuff(Var, ArgTypeAssign, TailArgTypeStuffs, ArgTypeStuffs) :-
    ArgTypeAssign = args_type_assign(TypeAssign, ArgTypes, _, Source),
    type_assign_get_var_types(TypeAssign, VarTypes),
    ( if search_var_type(VarTypes, Var, VarType0) then
        VarType = VarType0
    else
        % This shouldn't happen - how can a variable which has
        % not yet been assigned a type variable fail to have
        % the correct type?
        VarType = defined_type(unqualified("<any>"), [], kind_star)
    ),
    % XXX document me
    list.det_index0(ArgTypes, 0, ArgType),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    apply_rec_subst_to_type(TypeBindings, VarType, RecSubstVarType),
    apply_rec_subst_to_type(TypeBindings, ArgType, RecSubstArgType),
    type_assign_get_typevarset(TypeAssign, TVarSet),
    type_assign_get_existq_tvars(TypeAssign, ExistQTVars),
    ArgTypeStuff = arg_type_stuff(RecSubstVarType, Source, RecSubstArgType,
        TVarSet, ExistQTVars),
    ( if list.member(ArgTypeStuff, TailArgTypeStuffs) then
        ArgTypeStuffs = TailArgTypeStuffs
    else
        ArgTypeStuffs = [ArgTypeStuff | TailArgTypeStuffs]
    ).

%---------------------------------------------------------------------------%

:- pred expected_type_needs_int_constant_suffix(mer_type::in) is semidet.

expected_type_needs_int_constant_suffix(Type) :-
    Type = builtin_type(BuiltinType),
    BuiltinType = builtin_type_int(BuiltinTypeInt),
    BuiltinTypeInt \= int_type_int.

:- func nosuffix_integer_pieces = list(format_piece).

nosuffix_integer_pieces = Pieces :-
    Pieces = [words("A integer constant that consists only of digits"),
        words("is always of type"), quote("int"), suffix("."),
        words("Unsigned integer constants of the default size"),
        words("should have the suffix"), quote("u"), suffix(";"),
        words("constants of sized integer types should have"),
        words("an"), quote("i8"), suffix(","), quote("i16"), suffix(","),
        quote("i32"), words("or"), quote("i64"), words("suffix"),
        words("if they are signed, and"),
        words("an"), quote("u8"), suffix(","), quote("u16"), suffix(","),
        quote("u32"), words("or"), quote("u64"), words("suffix"),
        words("if they are unsigned."), nl].

%---------------------------------------------------------------------------%
%
% Converting a type assign set to a part of an error_spec can take
% a *very* long time if the type assign set is very big, which it can be
% in large predicates with many ambiguously typed variables.
% Yet in the common cases handled by the two predicates below,
% the result of the conversion is needed only if verbose_errors is set.
% The code below ensures that we incur the cost of the conversion
% only if we have to.
%
% We *always* include a verbose_only component in the result,
% because this ensures that the compiler output includes the line
%   For more information, recompile with `-E'.
% at the end.
%

:- pred type_assign_set_msg_to_verbose_component(typecheck_info::in,
    prog_varset::in, type_assign_set::in, error_msg_component::out) is det.

type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent) :-
    typecheck_info_get_verbose_errors(Info, VerboseErrors),
    (
        VerboseErrors = no,
        VerbosePieces = []
    ;
        VerboseErrors = yes,
        typecheck_info_get_module_info(Info, ModuleInfo),
        VerbosePieces = type_assign_set_msg_to_pieces(ModuleInfo, VarSet,
            TypeAssignSet)
    ),
    VerboseComponent = verbose_only(verbose_always, VerbosePieces).

:- pred arg_type_assign_set_msg_to_verbose_component(typecheck_info::in,
    prog_varset::in, args_type_assign_set::in, error_msg_component::out)
    is det.

arg_type_assign_set_msg_to_verbose_component(Info, VarSet,
        ArgTypeAssignSet, VerboseComponent) :-
    typecheck_info_get_verbose_errors(Info, VerboseErrors),
    (
        VerboseErrors = no,
        VerbosePieces = []
    ;
        VerboseErrors = yes,
        typecheck_info_get_module_info(Info, ModuleInfo),
        VerbosePieces = args_type_assign_set_msg_to_pieces(ModuleInfo, VarSet,
            ArgTypeAssignSet)
    ),
    VerboseComponent = verbose_only(verbose_always, VerbosePieces).

%---------------------------------------------------------------------------%

:- func type_diff_pieces(list(format_piece), list(tvar),
    mer_type, mer_type) = list(format_piece).

type_diff_pieces(ContextPieces, ExistQTVars, ActualType0, ExpectedType0)
        = DiffPieces :-
    ActualType = strip_kind_annotation(ActualType0),
    ExpectedType = strip_kind_annotation(ExpectedType0),
    ( if
        ActualType = ExpectedType
    then
        % There are no differences.
        DiffPieces = []
    else if
        % There is at least one difference.
        %
        % First, look for differences in the types of arguments.
        % If we can pinpoint some argument(s) as being different, we want
        % to report *this*, rather than the difference in the whole types.
        require_complete_switch [ActualType]
        (
            ( ActualType = type_variable(_, _)
            ; ActualType = builtin_type(_)
            ),
            fail
        ;
            ActualType = defined_type(TypeSymName, ActualArgTypes, _),
            ExpectedType = defined_type(TypeSymName, ExpectedArgTypes, _),
            ActualArgTypes \= ExpectedArgTypes,
            DiffPiecesPrime = arg_type_list_diff_pieces(ContextPieces,
                [words("type constructor"), unqual_sym_name(TypeSymName)],
                ExistQTVars, ActualArgTypes, ExpectedArgTypes)
        ;
            ActualType = tuple_type(ActualArgTypes, _),
            ExpectedType = tuple_type(ExpectedArgTypes, _),
            ActualArgTypes \= ExpectedArgTypes,
            DiffPiecesPrime = arg_type_list_diff_pieces(ContextPieces,
                [words("the tuple type constructor")],
                ExistQTVars, ActualArgTypes, ExpectedArgTypes)
        ;
            ActualType = apply_n_type(TVar, ActualArgTypes, _),
            ExpectedType = apply_n_type(TVar, ExpectedArgTypes, _),
            ActualArgTypes \= ExpectedArgTypes,
            DiffPiecesPrime = arg_type_list_diff_pieces(ContextPieces,
                [words("apply_n type constructor")],
                ExistQTVars, ActualArgTypes, ExpectedArgTypes)
        ;
            ActualType = higher_order_type(ActualPorF, ActualArgTypes,
                ActualInstInfo, ActualPurity, _ActualLambdaEvalMethod),
            ExpectedType = higher_order_type(ExpectedPorF, ExpectedArgTypes,
                ExpectedInstInfo, ExpectedPurity, _ExpectedLambdaEvalMethod),
            % There is only one lambda_eval_method, so that field
            % cannot contain a difference.
            DiffPiecesPrime = higher_order_diff_pieces(ContextPieces,
                ExistQTVars, ActualPorF, ExpectedPorF,
                ActualArgTypes, ExpectedArgTypes,
                ActualInstInfo, ExpectedInstInfo, ActualPurity, ExpectedPurity)
        ),
        DiffPiecesPrime = [_ | _]
    then
        DiffPieces = DiffPiecesPrime
    else if
        ExpectedType = type_variable(ExpectedTVar, _),
        list.member(ExpectedTVar, ExistQTVars),
        ActualType \= type_variable(_, _)
    then
        DiffPieces = wrap_diff_pieces(ContextPieces,
            [words("The context requires a specific type, but this is"),
            words("not allowed for existentially typed arguments.")])
    else
        % There is at least one difference, but either there are
        % no arguments, or there are no differences in them. In this case,
        % the difference must be at the position that ContextPieces points to.
        (
            ContextPieces = [],
            % ContextPieces points to the top level. Our caller printing out
            % the whole of both the expected and actual types will make
            % this apparent.
            DiffPieces = []
        ;
            ContextPieces = [_ | _],
            % In this case, the difference must be at the position that
            % ContextPieces points to. We could report this fact, but
            % what text would be more useful than annoying?
            DiffPieces = []
            % DiffPieces = wrap_diff_pieces(ContextPieces,
            %     [words("A difference occurs here.")])
        )
    ).

    % Return a description of any differences between the given
    % actual and expected argument types. The caller need not have ensured
    % that the two lists are the same length; we detect and report any
    % length mismatches.
    %
:- func arg_type_list_diff_pieces(list(format_piece),
    list(format_piece), list(tvar), list(mer_type), list(mer_type))
    = list(format_piece).

arg_type_list_diff_pieces(ContextPieces, TypeCtorPieces, ExistQTVars,
        ActualArgTypes, ExpectedArgTypes) = DiffPieces :-
    list.length(ActualArgTypes, ActualNumArgs),
    list.length(ExpectedArgTypes, ExpectedNumArgs),
    ( if ActualArgTypes = ExpectedArgTypes then
        DiffPieces = []
    else if ActualNumArgs = ExpectedNumArgs then
        DiffPieces = arg_type_list_diff_pieces_loop(ContextPieces,
            TypeCtorPieces, ExistQTVars, 1, ActualArgTypes, ExpectedArgTypes)
    else
        DiffPieces = wrap_diff_pieces(ContextPieces,
            [words("Arity mismatch for")] ++ TypeCtorPieces ++ [suffix(":"),
            words("expected"), int_name(ExpectedNumArgs), words("arguments,"),
            words("got"), int_name(ActualNumArgs), suffix(".")])
    ).

:- func higher_order_diff_pieces(list(format_piece), list(tvar),
    pred_or_func, pred_or_func, list(mer_type), list(mer_type),
    ho_inst_info, ho_inst_info, purity, purity) = list(format_piece).

higher_order_diff_pieces(ContextPieces, ExistQTVars, ActualPorF, ExpectedPorF,
        ActualArgTypes, ExpectedArgTypes, ActualInstInfo, ExpectedInstInfo,
        ActualPurity, ExpectedPurity) = !:DiffPieces :-
    !:DiffPieces = [],
    ( if ActualPorF = ExpectedPorF then
        true
    else
        !:DiffPieces = !.DiffPieces ++ wrap_diff_pieces(ContextPieces,
            [words("Predicate vs function mismatch:"),
            words("expected a"), p_or_f(ExpectedPorF), suffix(","),
            words("got a"), p_or_f(ActualPorF), suffix(".")])
    ),
    ( if ActualPurity = ExpectedPurity then
        true
    else
        !:DiffPieces = !.DiffPieces ++ wrap_diff_pieces(ContextPieces,
            [words("Purity mismatch:"),
            words("expected"), a_purity_desc(ExpectedPurity),
            p_or_f(ExpectedPorF), suffix(","), words("got"),
            a_purity_desc(ActualPurity), p_or_f(ActualPorF), suffix(".")])
    ),
    ( if ActualArgTypes = ExpectedArgTypes then
        true
    else
        TypeCtorPieces = [lower_case_next_if_not_first,
            words("The"), p_or_f(ActualPorF)],
        !:DiffPieces = !.DiffPieces ++
            arg_type_list_diff_pieces(ContextPieces, TypeCtorPieces,
                ExistQTVars, ActualArgTypes, ExpectedArgTypes)
    ),
    ( if ActualInstInfo = ExpectedInstInfo then
        true
    else
        ( if
            ActualInstInfo = higher_order(ActualPredInstInfo),
            ExpectedInstInfo = higher_order(ExpectedPredInstInfo)
        then
            ActualPredInstInfo = pred_inst_info(ActualHOPorF,
                ActualArgModes, _ActualRegInfo, ActualDetism),
            ExpectedPredInstInfo = pred_inst_info(ExpectedHOPorF,
                ExpectedArgModes, _ExpectedRegInfo, ExpectedDetism),
            list.length(ActualArgTypes, ActualNumArgTypes),
            list.length(ExpectedArgTypes, ExpectedNumArgTypes),
            % This is guaranteed by the failure of the
            % "ActualArgTypes \= ExpectedNumArgTypes" test above.
            expect(unify(ActualNumArgTypes, ExpectedNumArgTypes), $pred,
                "ActualNumArgTypes != ExpectedNumArgTypes"),
            list.length(ActualArgModes, ActualNumArgModes),
            list.length(ExpectedArgModes, ExpectedNumArgModes),
            ( if ActualHOPorF = ActualPorF then
                true
            else
                !:DiffPieces = !.DiffPieces ++ wrap_diff_pieces(ContextPieces,
                    [words("Predicate vs function mismatch:"),
                    words("the actual type"),
                    words("is a"), p_or_f(ActualPorF), suffix(","),
                    words("but its mode says"),
                    words("it is a"), p_or_f(ActualHOPorF), suffix(".")])
            ),
            ( if ExpectedHOPorF = ExpectedPorF then
                true
            else
                !:DiffPieces = !.DiffPieces ++ wrap_diff_pieces(ContextPieces,
                    [ words("Predicate vs function mismatch:"),
                    words("the expected type"),
                    words("is a"), p_or_f(ActualPorF), suffix(","),
                    words("but its mode says"),
                    words("it is a"), p_or_f(ActualHOPorF), suffix(".")])
            ),
            ( if ActualNumArgTypes = ActualNumArgModes then
                true
            else
                adjust_func_arity(ActualPorF,
                    ActualTypeArity, ActualNumArgTypes),
                adjust_func_arity(ActualPorF,
                    ActualModeArity, ActualNumArgModes),
                !:DiffPieces = !.DiffPieces ++ wrap_diff_pieces(ContextPieces,
                    [words("Arity mismatch:"),
                    words("the actual"), p_or_f(ActualPorF),
                    words("type has"), int_name(ActualTypeArity),
                    words("arguments"), suffix(","),
                    words("but its mode information says it has"),
                    int_name(ActualModeArity), words("arguments.")])
            ),
            ( if ExpectedNumArgTypes = ExpectedNumArgModes then
                true
            else
                adjust_func_arity(ExpectedPorF,
                    ExpectedTypeArity, ExpectedNumArgTypes),
                adjust_func_arity(ExpectedPorF,
                    ExpectedModeArity, ExpectedNumArgModes),
                !:DiffPieces = !.DiffPieces ++ wrap_diff_pieces(ContextPieces,
                    [words("Arity mismatch:"),
                    words("the actual"), p_or_f(ExpectedPorF),
                    words("type has"), int_name(ExpectedTypeArity),
                    words("arguments"), suffix(","),
                    words("but its mode information says it has"),
                    int_name(ExpectedModeArity), words("arguments.")])
            ),
            ( if ActualArgModes = ExpectedArgModes then
                true
            else
                % We could try to report which argument(s), or even which
                % piece(s) of which argument(s), contain the difference ...
                !:DiffPieces = !.DiffPieces ++ wrap_diff_pieces(ContextPieces,
                    [words("Mode mismatch:"),
                    words("the actual and expected modes of the"),
                    p_or_f(ActualPorF), words("differ.")])
            ),
            ( if ActualDetism = ExpectedDetism then
                true
            else
                ActualDetismStr = determinism_to_string(ActualDetism),
                ExpectedDetismStr = determinism_to_string(ExpectedDetism),
                !:DiffPieces = !.DiffPieces ++ wrap_diff_pieces(ContextPieces,
                    [words("Determinism mismatch:"),
                    words("the actual"), p_or_f(ActualPorF), words("has"),
                    words("determinism"), words(ActualDetismStr), suffix(","),
                    words("but the expected determinism is"),
                    words(ExpectedDetismStr), suffix(".")])
            )
        else
            % XXX We could do better here, but as long as the compiler
            %
            % - does not allow mode and determinism information to appear
            %   in the *type* of higher order predicate and function arguments,
            %   and
            %
            % - does not *itself* propagate mode and determinism information
            %   to the types from any modes on such arguments, which
            %   it could do if that information was consistent among all
            %   the modes, which it trivially would be in the usual case
            %   of only one mode,
            %
            % any message we could print would be more about the compiler's
            % inadequacy than the programmer's :-(
            true
        )
    ).

    % Return a description of any differences between the given
    % actual and expected argument types. The caller should have ensured
    % that the two lists are the same length.
    %
:- func arg_type_list_diff_pieces_loop(list(format_piece),
    list(format_piece), list(tvar), int, list(mer_type), list(mer_type))
    = list(format_piece).

arg_type_list_diff_pieces_loop(_, _, _, _, [], []) = [].
arg_type_list_diff_pieces_loop(_, _, _, _, [], [_ | _]) = _ :-
    unexpected($pred, "list length misnatch").
arg_type_list_diff_pieces_loop(_, _, _, _, [_ | _], []) = _ :-
    unexpected($pred, "list length misnatch").
arg_type_list_diff_pieces_loop(ContextPieces, TypeCtorPieces,
        ExistQTVars, CurArgNum, [ActualArgType | ActualArgTypes],
        [ExpectedArgType | ExpectedArgTypes]) = DiffPieces :-
    TailDiffPieces = arg_type_list_diff_pieces_loop(ContextPieces,
        TypeCtorPieces, ExistQTVars, CurArgNum + 1,
        ActualArgTypes, ExpectedArgTypes),
    ( if ActualArgType = ExpectedArgType then
        DiffPieces = TailDiffPieces
    else
        ArgContextPieces = [treat_next_as_first | ContextPieces] ++
            [lower_case_next_if_not_first, words("In the"),
            nth_fixed(CurArgNum), words("argument of")] ++
            TypeCtorPieces ++ [suffix(":"), nl],
        HeadDiffPieces = type_diff_pieces(ArgContextPieces, ExistQTVars,
            ActualArgType, ExpectedArgType),
        DiffPieces = HeadDiffPieces ++ TailDiffPieces
    ).

:- func wrap_diff_pieces(list(format_piece), list(format_piece))
    = list(format_piece).

wrap_diff_pieces(ContextPieces, MismatchPieces) = DiffPieces :-
    (
        ContextPieces = [],
        DiffPieces = [treat_next_as_first] ++ MismatchPieces ++ [nl]
    ;
        ContextPieces = [_ | _],
        DiffPieces = [treat_next_as_first | ContextPieces] ++
            [nl_indent_delta(1), lower_case_next_if_not_first] ++
            MismatchPieces ++ [nl_indent_delta(-1)]
    ).

%---------------------------------------------------------------------------%

:- pred get_inst_varset(type_error_clause_context::in, inst_varset::out)
    is det.

get_inst_varset(ClauseContext, InstVarSet) :-
    % XXX Typechecking works on pred_infos, which do NOT have an inst_varset.
    % I (zs) don't know where the inst variables in any ho_inst_infos
    % in higher-order types come from, but I am *guessing* that it is
    % from the varset of the clause itself. I am not even sure whether
    % this matters, since I don't know whether ho_inst_infos can ever
    % be filled in before the end of typechecking.
    %
    % XXX Note that replacing the code below with "varset.init(InstVarSet)"
    % has no effect on a bootcheck, so it seems that the answer to the
    % question above is "no".
    ProgVarSet = ClauseContext ^ tecc_varset,
    varset.coerce(ProgVarSet, InstVarSet).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_errors.
%---------------------------------------------------------------------------%
