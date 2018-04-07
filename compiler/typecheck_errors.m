%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: typecheck_errors.m.
% Main author: fjh.
%
% This file contains predicates to report errors for typechecking.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.typecheck_errors.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_info.
:- import_module hlds.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type arg_vector_kind
    --->    arg_vector_clause_head
    ;       arg_vector_plain_call_pred_id(pred_id)
    ;       arg_vector_plain_call(simple_call_id)
    ;       arg_vector_generic_call(generic_call_id)
    ;       arg_vector_foreign_proc_call(pred_id)
    ;       arg_vector_event(string).

:- type var_vector_kind
    --->    var_vector_args(arg_vector_kind)
    ;       var_vector_cond_quant
    ;       var_vector_exist_quant
    ;       var_vector_promise_solutions
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

%-----------------------------------------------------------------------------%

:- type cons_error
    --->    foreign_type_constructor(type_ctor, hlds_type_defn)
    ;       abstract_imported_type
    ;       invalid_field_update(sym_name, hlds_ctor_field_defn,
                tvarset, list(tvar))
    ;       new_on_non_existential_type(type_ctor).

%-----------------------------------------------------------------------------%

:- func report_pred_call_error(type_error_clause_context, prog_context,
    simple_call_id) = error_spec.

:- func report_unknown_event_call_error(prog_context, string) = error_spec.

:- func report_event_args_mismatch(prog_context, string, list(mer_type),
    list(prog_var)) = error_spec.

:- func report_no_clauses(module_info, pred_id, pred_info) = error_spec.

:- func report_no_clauses_stub(module_info, pred_id, pred_info) = error_spec.

:- func report_non_contiguous_clauses(module_info, pred_id, pred_info,
    clause_item_number_region, clause_item_number_region,
    list(clause_item_number_region)) = error_spec.

:- func report_warning_too_much_overloading(type_error_clause_context,
    prog_context, overloaded_symbol_map) = error_spec.

:- func report_error_too_much_overloading(type_error_clause_context,
    prog_context, overloaded_symbol_map) = error_spec.

:- func report_error_unif_var_var(type_error_clause_context,
    unify_context, prog_context, prog_var, prog_var, type_assign_set)
    = error_spec.

:- func report_error_lambda_var(type_error_clause_context,
    unify_context, prog_context, pred_or_func,
    lambda_eval_method, prog_var, list(prog_var), type_assign_set)
    = error_spec.

:- func report_error_functor_type(type_error_clause_context,
    unify_context, prog_context, prog_var,
    list(cons_type_info), cons_id, int, type_assign_set) = error_spec.

:- func report_error_functor_arg_types(type_error_clause_context,
    unify_context, prog_context, prog_var,
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
                actual_type     :: list(format_component),
                expected_type   :: list(format_component)
            ).

:- func report_error_var(type_error_clause_context, type_error_goal_context,
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

:- func report_arg_vector_type_errors(type_error_clause_context,
    prog_context, arg_vector_kind, type_assign_set,
    list(arg_vector_type_error)) = error_spec.

:- func report_error_var_either_type(type_error_clause_context,
    type_error_goal_context, prog_context, prog_var, mer_type, mer_type,
    type_assign_set) = error_spec.

:- func report_error_arg_var(type_error_clause_context,
    type_error_goal_context, prog_context, prog_var, args_type_assign_set)
    = error_spec.

:- func report_error_undef_cons(type_error_clause_context,
    type_error_goal_context, prog_context, list(cons_error), cons_id, int)
    = error_spec.

:- func report_ambiguity_error(type_error_clause_context, prog_context,
    overloaded_symbol_map, type_assign, type_assign) = error_spec.

:- func report_unsatisfiable_constraints(type_error_clause_context,
    prog_context, type_assign_set) = error_spec.

:- func report_missing_tvar_in_foreign_code(type_error_clause_context,
    prog_context, string) = error_spec.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_class.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.pred_table.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.options.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_to_term.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

report_pred_call_error(ClauseContext, Context, PredCallId) = Spec :-
    PredCallId = simple_call_id(PredOrFunc, SymName, _Arity),
    ModuleInfo = ClauseContext ^ tecc_module_info,
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    PredMarkers = ClauseContext ^ tecc_pred_markers,
    IsFullyQualified = calls_are_fully_qualified(PredMarkers),
    predicate_table_lookup_pf_sym(PredicateTable, IsFullyQualified,
        PredOrFunc, SymName, OtherIds),
    (
        OtherIds = [_ | _],
        predicate_table_get_preds(PredicateTable, Preds),
        find_pred_arities(Preds, OtherIds, Arities),
        Spec = report_error_pred_num_args(ClauseContext, Context,
            PredCallId, Arities)
    ;
        OtherIds = [],
        UndefMsg = report_error_undef_pred(ClauseContext, Context, PredCallId),
        ( PredOrFunc = pf_predicate, SwitchedPredOrFunc = pf_function
        ; PredOrFunc = pf_function,  SwitchedPredOrFunc = pf_predicate
        ),
        predicate_table_lookup_pf_sym(PredicateTable, IsFullyQualified,
            SwitchedPredOrFunc, SymName, SwitchedOtherIds),
        (
            SwitchedOtherIds = [_ | _],
            KindMsg = report_error_func_instead_of_pred(Context,
                SwitchedPredOrFunc),
            Msgs = [UndefMsg, KindMsg]
        ;
            SwitchedOtherIds = [],
            Msgs = [UndefMsg]
        ),
        Spec = error_spec(severity_error, phase_type_check, Msgs)
    ).

:- func report_error_pred_num_args(type_error_clause_context, prog_context,
    simple_call_id, list(int)) = error_spec.

report_error_pred_num_args(ClauseContext, Context, SimpleCallId, Arities)
        = Spec :-
    SimpleCallId = simple_call_id(PredOrFunc, SymName, Arity),
    Pieces = in_clause_for_pieces(ClauseContext) ++
        [words("error:")] ++
        error_num_args_to_pieces(yes(PredOrFunc), Arity, Arities) ++ [nl] ++
        [words("in call to"), p_or_f(PredOrFunc), qual_sym_name(SymName),
        suffix("."), nl],
    Spec = error_spec(severity_error, phase_type_check,
        [simple_msg(Context, [always(Pieces)])]).

:- func report_error_func_instead_of_pred(prog_context, pred_or_func)
    = error_msg.

report_error_func_instead_of_pred(Context, PredOrFunc) = Msg :-
    (
        PredOrFunc = pf_function,
        Pieces = [words("(There is a"),
            prefix("*"), p_or_f(PredOrFunc), suffix("*"),
            words("with that name, however."), nl,
            words("Perhaps you forgot to add"),
            quote(" = ..."), suffix("?)"), nl]
    ;
        PredOrFunc = pf_predicate,
        Pieces = [words("(There is a"),
            prefix("*"), p_or_f(PredOrFunc), suffix("*"),
            words("with that name, however.)"), nl]
    ),
    Msg = simple_msg(Context, [always(Pieces)]).

:- func report_error_undef_pred(type_error_clause_context, prog_context,
    simple_call_id) = error_msg.

report_error_undef_pred(ClauseContext, Context, SimpleCallId) = Msg :-
    SimpleCallId = simple_call_id(_PredOrFunc, PredName, Arity),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    InClauseForComponent = always(InClauseForPieces),
    ( if
        PredName = unqualified("->"),
        ( Arity = 2 ; Arity = 4 )
    then
        MainPieces = [words("error:"), quote("->"), words("without"),
            quote(";"), suffix("."), nl],
        MainComponent = always(MainPieces),
        VerbosePieces =
            [words("Note: the else part is not optional."), nl,
            words("Every if-then must have an else."), nl],
        VerboseComponent = verbose_only(verbose_once, VerbosePieces),
        Components = [MainComponent, VerboseComponent]
    else if
        PredName = unqualified("else"),
        ( Arity = 2 ; Arity = 4 )
    then
        Components = [always([words("error: unmatched"), quote("else"),
            suffix("."), nl])]
    else if
        PredName = unqualified("if"),
        ( Arity = 2 ; Arity = 4 )
    then
        Pieces = [words("error:"), quote("if"), words("without"),
            quote("then"), words("or"), quote("else"), suffix("."), nl],
        Components = [always(Pieces)]
    else if
        PredName = unqualified("then"),
        ( Arity = 2 ; Arity = 4 )
    then
        MainPieces = [words("error:"), quote("then"), words("without"),
            quote("if"), words("or"), quote("else"), suffix("."), nl],
        MainComponent = always(MainPieces),
        VerbosePieces =
            [words("Note: the"), quote("else"), words("part is not optional."),
            nl, words("Every if-then must have an"),
            quote("else"), suffix("."), nl],
        VerboseComponent = verbose_only(verbose_once, VerbosePieces),
        Components = [MainComponent, VerboseComponent]
    else if
        PredName = unqualified("apply"),
        Arity >= 1
    then
        Components = report_apply_instead_of_pred
    else if
        PredName = unqualified(PurityString),
        Arity = 1,
        ( PurityString = "impure" ; PurityString = "semipure" )
    then
        MainPieces = [words("error:"), quote(PurityString),
            words("marker in an inappropriate place."), nl],
        MainComponent = always(MainPieces),
        VerbosePieces =
            [words("Such markers only belong before predicate calls."), nl],
        VerboseComponent = verbose_only(verbose_once, VerbosePieces),
        Components = [MainComponent, VerboseComponent]
    else if
        PredName = unqualified("some"),
        Arity = 2
    then
        Pieces = [words("syntax error in existential quantification:"),
            words("first argument of"), quote("some"),
            words("should be a list of variables."), nl],
        Components = [always(Pieces)]
    else
        MainPieces = [words("error: undefined"), simple_call(SimpleCallId)],
        (
            PredName = qualified(ModuleQualifier, _),
            Pieces = MainPieces ++
                maybe_report_missing_import_addendum(ClauseContext,
                    ModuleQualifier)
        ;
            PredName = unqualified(_),
            Pieces = MainPieces ++ [suffix("."), nl]
        ),
        Components = [always(Pieces)]
    ),
    Msg = simple_msg(Context, [InClauseForComponent | Components]).

:- func report_apply_instead_of_pred = list(error_msg_component).

report_apply_instead_of_pred = Components :-
    MainPieces = [words("error: the language construct"), quote("apply"),
        words("should be used as an expression, not as a goal."), nl],
    MainComponent = always(MainPieces),
    VerbosePieces =
        [words("Perhaps you forgot to add"), quote(" = ..."), suffix("?)"), nl,
        words("If you're trying to invoke a higher-order predicate,"),
        words("use"), quote("call"), suffix(","), words("not"),
            quote("apply"), suffix("."), nl,
        words("If you're trying to curry a higher-order function,"),
        words("use a forwarding function:"), nl,
        words("e.g. instead of "), quote("NewFunc = apply(OldFunc, X)"),
        words("use"), quote("NewFunc = my_apply(OldFunc, X)"),
        words("where"), quote("my_apply"), words("is defined"),
        words("with the appropriate arity, e.g."),
        quote("my_apply(Func, X, Y) :- apply(Func, X, Y).")],
    VerboseComponent = verbose_only(verbose_always, VerbosePieces),
    Components = [MainComponent, VerboseComponent].

%-----------------------------------------------------------------------------%

report_unknown_event_call_error(Context, EventName) = Spec :-
    Pieces = [words("Error: there is no event named"),
        quote(EventName), suffix(".")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

report_event_args_mismatch(Context, EventName, EventArgTypes, Args) = Spec :-
    Pieces =
        [words("Error:")] ++
        error_num_args_to_pieces(no, length(Args), [length(EventArgTypes)]) ++
        [words("in event"), quote(EventName), suffix(".")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

report_no_clauses(ModuleInfo, PredId, PredInfo) = Spec :-
    PredPieces = describe_one_pred_name(ModuleInfo, should_not_module_qualify,
        PredId),
    Pieces = [words("Error: no clauses for") | PredPieces] ++ [suffix(".")],
    pred_info_get_context(PredInfo, Context),
    % It is possible (and even likely) that the error that got the exit
    % status set was caused by a syntax error in a clause defining this
    % predicate or function. Reporting a missing clause could therefore
    % be redundant and misleading. Even if this predicate or function truly
    % has no clauses, this error would be caught once the other errors
    % (the ones leading to the exit status) are fixed by the programmer.
    %
    % However, right now we have no means to distinguish the case where the
    % exit status being set to nonzero was caused by an actual syntax error,
    % and the case where it was set by a no clauses error for another
    % predicate. Since we don't want to limit the number of predicates
    % without clauses we warn about in a single compiler invocation to one,
    % we choose (as the lesser of two evils) to always report the error.
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

report_no_clauses_stub(ModuleInfo, PredId, PredInfo) = Spec :-
    PredPieces = describe_one_pred_name(ModuleInfo, should_not_module_qualify,
        PredId),
    Pieces = [words("Warning: no clauses for ") | PredPieces] ++ [suffix(".")],
    pred_info_get_context(PredInfo, Context),
    Msg = simple_msg(Context,
        [option_is_set(warn_stubs, yes, [always(Pieces)])]),
    Severity = severity_conditional(warn_stubs, yes, severity_warning, no),
    Spec = error_spec(Severity, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

report_non_contiguous_clauses(ModuleInfo, PredId, PredInfo,
        FirstRegion, SecondRegion, LaterRegions) = Spec :-
    PredPieces = describe_one_pred_name(ModuleInfo, should_not_module_qualify,
        PredId),
    FrontPieces = [words("Warning: non-contiguous clauses for ") | PredPieces]
        ++ [suffix(".")],
    pred_info_get_context(PredInfo, Context),
    FrontMsg = simple_msg(Context, [always(FrontPieces)]),
    report_non_contiguous_clause_contexts(PredPieces, 1,
        FirstRegion, SecondRegion, LaterRegions, ContextMsgs),
    Msgs = [FrontMsg | ContextMsgs],
    Spec = error_spec(severity_warning, phase_type_check, Msgs).

:- pred report_non_contiguous_clause_contexts(list(format_component)::in,
    int::in, clause_item_number_region::in, clause_item_number_region::in,
    list(clause_item_number_region)::in, list(error_msg)::out) is det.

report_non_contiguous_clause_contexts(PredPieces, GapNumber,
        FirstRegion, SecondRegion, LaterRegions, Msgs) :-
    FirstRegion =
        clause_item_number_region(_FirstLowerNumber, _FirstUpperNumber,
        _FirstLowerContext, FirstUpperContext),
    SecondRegion =
        clause_item_number_region(_SecondLowerNumber, _SecondUpperNumber,
        SecondLowerContext, _SecondUpperContext),
    ( if
        GapNumber = 1,
        LaterRegions = []
    then
        % There is only one gap, so don't number it.
        GapPieces = []
    else
        GapPieces = [int_fixed(GapNumber)]
    ),
    % The wording here is chosen be non-confusing even if a clause has a gap
    % both before and after it, so that gaps both end and start at the context
    % of that clause. We could do better if we had separate contexts for the
    % start and the end of the clause, but we don't.
    FirstPieces = [words("Gap") | GapPieces] ++
        [words("in clauses of") | PredPieces] ++
        [words("starts after this clause."), nl],
    SecondPieces = [words("Gap") | GapPieces] ++
        [words("in clauses of") | PredPieces] ++
        [words("ends with this clause."), nl],
    FirstMsg = simple_msg(FirstUpperContext, [always(FirstPieces)]),
    SecondMsg = simple_msg(SecondLowerContext, [always(SecondPieces)]),
    (
        LaterRegions = [],
        Msgs = [FirstMsg, SecondMsg]
    ;
        LaterRegions = [FirstLaterRegion | LaterLaterRegions],
        report_non_contiguous_clause_contexts(PredPieces, GapNumber + 1,
            SecondRegion, FirstLaterRegion, LaterLaterRegions, LaterMsgs),
        Msgs = [FirstMsg, SecondMsg | LaterMsgs]
    ).

%-----------------------------------------------------------------------------%

report_warning_too_much_overloading(ClauseContext, Context,
        OverloadedSymbolMap) = Spec :-
    Msgs = too_much_overloading_to_msgs(ClauseContext, Context,
        OverloadedSymbolMap, no),
    Spec = error_spec(severity_warning, phase_type_check, Msgs).

report_error_too_much_overloading(ClauseContext, Context,
        OverloadedSymbolMap) = Spec :-
    Msgs = too_much_overloading_to_msgs(ClauseContext, Context,
        OverloadedSymbolMap, yes),
    Spec = error_spec(severity_error, phase_type_check, Msgs).

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
        SecondMsg = simple_msg(Context, [always(SecondPieces)]),
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
            Symbol = overloaded_pred(CallId, PredIds),
            StartPieces = [words("The predicate symbol"),
                simple_call(CallId), suffix("."), nl,
                words("The possible matches are:"), nl_indent_delta(1)],
            PredIdPiecesList = list.map(describe_one_pred_name(ModuleInfo,
                should_module_qualify), PredIds),
            PredIdPieces = component_list_to_line_pieces(PredIdPiecesList,
                [suffix(".")]),
            FirstPieces = StartPieces ++ PredIdPieces,
            LaterPieces = [words("The predicate symbol"),
                simple_call(CallId), words("is also overloaded here.")]
        ;
            Symbol = overloaded_func(ConsId, Sources0),
            list.sort(Sources0, Sources),
            StartPieces = [words("The function symbol"),
                qual_cons_id_and_maybe_arity(ConsId), suffix("."), nl,
                words("The possible matches are:"), nl_indent_delta(1)],
            SourcePiecesList = list.map(
                describe_cons_type_info_source(ModuleInfo), Sources),
            SourcePieces = component_list_to_line_pieces(SourcePiecesList,
                [suffix(".")]),
            FirstPieces = StartPieces ++ SourcePieces,
            LaterPieces = [words("The function symbol"),
                qual_cons_id_and_maybe_arity(ConsId),
                words("is also overloaded here.")]
        ),
        FirstMsg = simple_msg(FirstContext, [always(FirstPieces)]),
        LaterMsgs = list.map(context_to_error_msg(LaterPieces), LaterContexts),
        Msgs = [FirstMsg | LaterMsgs]
    ).

:- func context_to_error_msg(list(format_component), prog_context) = error_msg.

context_to_error_msg(Pieces, Context) = simple_msg(Context, [always(Pieces)]).

:- func describe_cons_type_info_source(module_info, cons_type_info_source)
    = list(format_component).

describe_cons_type_info_source(ModuleInfo, Source) = Pieces :-
    (
        Source = source_type(TypeCtor),
        TypeCtor = type_ctor(SymName, Arity),
        Pieces = [words("the type constructor"),
            qual_sym_name_and_arity(sym_name_arity(SymName, Arity))]
    ;
        Source = source_builtin_type(TypeCtorName),
        Pieces = [words("the builtin type constructor"), quote(TypeCtorName)]
    ;
        Source = source_get_field_access(TypeCtor),
        TypeCtor = type_ctor(SymName, Arity),
        Pieces = [words("a"), quote("get"), words("field access function"),
            words("for the type constructor"),
            qual_sym_name_and_arity(sym_name_arity(SymName, Arity))]
    ;
        Source = source_set_field_access(TypeCtor),
        TypeCtor = type_ctor(SymName, Arity),
        Pieces = [words("a"), quote("set"), quote("field access function"),
            words("for the type constructor"),
            qual_sym_name_and_arity(sym_name_arity(SymName, Arity))]
    ;
        Source = source_pred(PredId),
        Pieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
            PredId)
    ;
        Source = source_apply(ApplyOp),
        Pieces = [words("the builtin operator constructor"), quote(ApplyOp)]
    ).

%-----------------------------------------------------------------------------%

report_error_unif_var_var(ClauseContext, UnifyContext, Context,
        X, Y, TypeAssignSet) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    unify_context_to_pieces(UnifyContext, [], UnifyContextPieces),

    VarSet = ClauseContext ^ tecc_varset,
    MainPieces = [words("type error in unification of variable"),
        quote(mercury_var_to_name_only(VarSet, X)), nl,
        words("and variable"),
        quote(mercury_var_to_name_only(VarSet, Y)), suffix("."), nl,
        quote(mercury_var_to_name_only(VarSet, X))] ++
        type_of_var_to_pieces(TypeAssignSet, X) ++ [suffix(","), nl,
        quote(mercury_var_to_name_only(VarSet, Y))] ++
        type_of_var_to_pieces(TypeAssignSet, Y) ++ [suffix("."), nl],
    VerbosePieces = type_assign_set_msg_to_pieces(TypeAssignSet, VarSet),
    Msg = simple_msg(Context,
        [always(InClauseForPieces), always(UnifyContextPieces),
        always(MainPieces), verbose_only(verbose_always, VerbosePieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

report_error_lambda_var(ClauseContext, UnifyContext, Context,
        PredOrFunc, _EvalMethod, Var, ArgVars, TypeAssignSet) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    unify_context_to_pieces(UnifyContext, [], UnifyContextPieces),

    VarSet = ClauseContext ^ tecc_varset,
    Pieces1 = [words("type error in unification of")] ++
        argument_name_to_pieces(VarSet, Var) ++ [nl],
    (
        PredOrFunc = pf_predicate,
        Pieces2 = [words("and"), prefix("pred("),
            words(mercury_vars_to_name_only(VarSet, ArgVars)),
            suffix(")"), words(":- ...':"), nl]
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgVars, FuncArgs, RetVar),
        Pieces2 = [words("and"), prefix("func("),
            words(mercury_vars_to_name_only(VarSet, FuncArgs)),
            suffix(")"), fixed("="),
            words(mercury_var_to_name_only(VarSet, RetVar)),
            words(":- ...':"), nl]
    ),

    Pieces3 = argument_name_to_pieces(VarSet, Var) ++
        type_of_var_to_pieces(TypeAssignSet, Var) ++ [suffix(","), nl],

    LambdaExprStr = "lambda expression has type",
    Pieces4a = [words(LambdaExprStr)],
    (
        PredOrFunc = pf_predicate,
        (
            ArgVars = [],
            Pieces4b = [words("pred")]
        ;
            ArgVars = [_ | _],
            list.length(ArgVars, NumArgVars),
            list.duplicate(NumArgVars - 1, ", _", Strings),
            JoinedString = string.join_list("", Strings),
            Pieces4b = [words("pred(_" ++ JoinedString ++ ")")]
        )
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgVars, FuncArgVars, _),
        (
            FuncArgVars = [],
            Pieces4b = [words("func = _")]
        ;
            FuncArgVars = [_ | _],
            list.length(FuncArgVars, NumArgVars),
            list.duplicate(NumArgVars - 1, ", _", Strings),
            JoinedString = string.join_list("", Strings),
            Pieces4b = [words("func(_" ++ JoinedString ++ ") = _")]
        )
    ),
    Pieces4c = [suffix("."), nl],
    Pieces4 = Pieces4a ++ Pieces4b ++ Pieces4c,

    VerbosePieces = type_assign_set_msg_to_pieces(TypeAssignSet, VarSet),
    Msg = simple_msg(Context,
        [always(InClauseForPieces ++ UnifyContextPieces),
        always(Pieces1 ++ Pieces2 ++ Pieces3 ++ Pieces4),
        verbose_only(verbose_always, VerbosePieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

report_error_functor_type(ClauseContext, UnifyContext, Context,
        Var, ConsDefnList, Functor, Arity, TypeAssignSet) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    % XXX We could append UnifyContextPieces after InClauseForPieces
    % instead of after the empty list.
    unify_context_to_pieces(UnifyContext, [], UnifyContextPieces),

    VarSet = ClauseContext ^ tecc_varset,
    MainPieces = [words("type error in unification of")] ++
        argument_name_to_pieces(VarSet, Var) ++ [nl, words("and")] ++
        functor_name_to_pieces(Functor, Arity) ++ [suffix("."), nl] ++

        argument_name_to_pieces(VarSet, Var) ++
        type_of_var_to_pieces(TypeAssignSet, Var) ++ [suffix(","), nl] ++

        functor_name_to_pieces(Functor, Arity) ++
        type_of_functor_to_pieces(Functor, Arity, ConsDefnList) ++
        [suffix("."), nl],

    VerbosePieces = type_assign_set_msg_to_pieces(TypeAssignSet, VarSet),

    Msg = simple_msg(Context,
        [always(InClauseForPieces ++ UnifyContextPieces),
        always(MainPieces), verbose_only(verbose_always, VerbosePieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

report_error_functor_arg_types(ClauseContext, UnifyContext, Context, Var,
        ConsDefnList, Functor, Args, ArgsTypeAssignSet) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    unify_context_to_pieces(UnifyContext, [], UnifyContextPieces),

    ModuleInfo = ClauseContext ^ tecc_module_info,
    VarSet = ClauseContext ^ tecc_varset,
    strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor),
    StrippedFunctorStr = functor_cons_id_to_string(ModuleInfo, VarSet,
        print_name_only, StrippedFunctor, Args),
    list.length(Args, Arity),

    % If we have consistent information about the argument types,
    % we prefer to print an error message that mentions only the arguments
    % that may be in error.
    ConsArgTypesSet = list.map(get_callee_arg_types, ArgsTypeAssignSet),

    ( if
        list.all_same(ConsArgTypesSet),
        ConsArgTypesSet = [ConsArgTypes | _]
    then
        assoc_list.from_corresponding_lists(Args, ConsArgTypes, ArgExpTypes),
        TypeAssigns = list.map(get_caller_arg_assign, ArgsTypeAssignSet),
        find_mismatched_args(1, ArgExpTypes, TypeAssigns,
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
            ErrorPieces = mismatched_args_to_pieces(NoSubsumeMismatches, yes,
                VarSet, Functor)
        ;
            RevNoSubsumeMismatches = [],
            list.reverse(RevSubsumesMismatches, SubsumesMismatches),
            MaybeNumMismatches = yes(list.length(SubsumesMismatches)),
            ErrorPieces = mismatched_args_to_pieces(SubsumesMismatches, yes,
                VarSet, Functor)
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
            list.member(ConsDefn, ConsDefnList),
            ConsDefn ^ cti_arg_types = [_ | _]
        then
            % If so, print out the type of `Var'.
            ResultTypePieces = argument_name_to_pieces(VarSet, Var) ++
                type_of_var_to_pieces(TypeAssignSet, Var) ++
                [suffix(","), nl]
        else
            ResultTypePieces = []
        ),

        AllTypesPieces = functor_name_to_pieces(Functor, Arity) ++
            type_of_functor_to_pieces(Functor, Arity, ConsDefnList) ++
            types_of_vars_to_pieces(Args, VarSet, TypeAssignSet),
        ErrorPieces = ResultTypePieces ++ AllTypesPieces,

        VerbosePieces = type_assign_set_msg_to_pieces(TypeAssignSet, VarSet),
        VerboseComponents = [verbose_only(verbose_always, VerbosePieces)]
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
        [always(InClauseForPieces ++ UnifyContextPieces),
        always(VarAndTermPieces ++ ErrorPieces) | VerboseComponents]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

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

:- type type_mismatch
    --->    type_mismatch_exp_act(
                expected_type_desc  :: list(format_component),
                actual_type_desc    :: list(format_component),
                mismatch_subsumes   :: does_actual_subsume_expected
            ).

:- pred find_mismatched_args(int::in, assoc_list(prog_var, mer_type)::in,
    type_assign_set::in,
    list(mismatch_info)::in, list(mismatch_info)::out,
    list(mismatch_info)::in, list(mismatch_info)::out) is det.

find_mismatched_args(_, [], _,
        !RevSubsumesMismatches, !RevNoSubsumeMismatches).
find_mismatched_args(CurArgNum, [Arg - ExpType | ArgExpTypes], TypeAssignSet,
        !RevSubsumesMismatches, !RevNoSubsumeMismatches) :-
    get_type_stuff(TypeAssignSet, Arg, TypeStuffList),
    list.foldl2(substitute_types_check_match(ExpType), TypeStuffList,
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
    find_mismatched_args(CurArgNum + 1, ArgExpTypes, TypeAssignSet,
        !RevSubsumesMismatches, !RevNoSubsumeMismatches).

:- type does_some_type_stuff_match
    --->    no_type_stuff_matches
    ;       some_type_stuff_matches.

:- pred substitute_types_check_match(mer_type::in, type_stuff::in,
    list(type_mismatch)::in, list(type_mismatch)::out,
    does_some_type_stuff_match::in, does_some_type_stuff_match::out) is det.

substitute_types_check_match(ExpType, TypeStuff,
        !TypeMismatches, !DoesSomeTypeStuffMatch) :-
    TypeStuff = type_stuff(ArgType, TVarSet, TypeBindings, ExternalTypeParams),
    apply_rec_subst_to_type(TypeBindings, ArgType, FullArgType),
    apply_rec_subst_to_type(TypeBindings, ExpType, FullExpType),
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
        ExpectedPieces = type_to_pieces(add_quotes, FullExpType,
            TVarSet, ExternalTypeParams),
        ActualPieces = type_to_pieces(add_quotes, FullArgType,
            TVarSet, ExternalTypeParams),
        TypeMismatch = type_mismatch_exp_act(ExpectedPieces, ActualPieces,
            ActualSubsumesExpected),
        !:TypeMismatches = [TypeMismatch | !.TypeMismatches]
    ).

:- pred all_no_subsume_mismatches(list(type_mismatch)::in) is semidet.

all_no_subsume_mismatches([]).
all_no_subsume_mismatches([Mismatch | Mismatches]) :-
    Mismatch = type_mismatch_exp_act(_, _, actual_does_not_subsume_expected),
    all_no_subsume_mismatches(Mismatches).

:- func mismatched_args_to_pieces(list(mismatch_info), bool, prog_varset,
    cons_id) = list(format_component).

mismatched_args_to_pieces([], _, _, _) = [].
mismatched_args_to_pieces([Mismatch | Mismatches], First, VarSet, Functor)
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
            ArgNumPieces = [fixed("argument"), int_fixed(ArgNum - 1)]
        )
    else
        (
            First = yes,
            ArgNumPieces = [fixed("Argument"), int_fixed(ArgNum)]
        ;
            First = no,
            ArgNumPieces = [fixed("argument"), int_fixed(ArgNum)]
        )
    ),
    ( if varset.search_name(VarSet, Var, _) then
        VarNamePieces = [prefix("("),
            words(mercury_var_to_name_only(VarSet, Var)),
            suffix(")")]
    else
        VarNamePieces = []
    ),
    HeadTypeMismatch =
        type_mismatch_exp_act(HeadExpectedTypePieces, HeadActualTypePieces,
            _ActualSubsumesExpected),
    ( if
        expected_types_match(HeadExpectedTypePieces, TailTypeMismatches,
            TailActualTypePieces)
    then
        (
            TailActualTypePieces = [],
            ErrorDescPieces = [words("has type")] ++ HeadActualTypePieces ++
                [suffix(","), nl] ++
                [words("expected type was")] ++ HeadExpectedTypePieces
        ;
            TailActualTypePieces =
                [SecondActualTypePieces | ThirdPlusActualTypePieces],
            ErrorDescPieces = [words("has type")] ++
                report_actual_types(HeadActualTypePieces,
                    SecondActualTypePieces, ThirdPlusActualTypePieces) ++
                [suffix(","), nl] ++
                [words("expected type was")] ++ HeadExpectedTypePieces
        )
    else
        AllMismatches = [HeadTypeMismatch | TailTypeMismatches],
        ErrorDescPieces =
            [words("has one of the following type mismatches."), nl] ++
            report_possible_expected_actual_types(1, AllMismatches)
    ),

    (
        Mismatches = [],
        FollowingMismatchPieces = [suffix("."), nl]
    ;
        Mismatches = [_ | _],
        FollowingMismatchPieces = [suffix(";"), nl] ++
            mismatched_args_to_pieces(Mismatches, no, VarSet, Functor)
    ),
    Pieces = ArgNumPieces ++ VarNamePieces ++ ErrorDescPieces ++
        FollowingMismatchPieces.

:- pred expected_types_match(list(format_component)::in,
    list(type_mismatch)::in, list(list(format_component))::out) is semidet.

expected_types_match(_ExpTypePieces, [], []).
expected_types_match(ExpTypePieces, [HeadMismatch | TailMismatches],
        [HeadActualTypePieces | TailActualTypePieces]) :-
    HeadMismatch =
        type_mismatch_exp_act(HeadExpTypePieces, HeadActualTypePieces,
            _ActualSubsumesExpected),
    ExpTypePieces = HeadExpTypePieces,
    expected_types_match(ExpTypePieces, TailMismatches, TailActualTypePieces).

:- func report_actual_types(list(format_component),
    list(format_component), list(list(format_component))) =
    list(format_component).

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
    list(format_component).

report_possible_expected_actual_types(_CurrPossNum, []) = [].
report_possible_expected_actual_types(CurrPossNum, [Mismatch | Mismatches])
        = Pieces :-
    Mismatch = type_mismatch_exp_act(ExpectedTypePieces, ActualTypePieces,
        _ActualSubsumesExpected),
    HeadPieces =
        [words("Possibility"), int_fixed(CurrPossNum), suffix(":")] ++
        [words("actual type")] ++ ActualTypePieces ++ [suffix(",")] ++
        [words("expected type")] ++ ExpectedTypePieces ++ [suffix("."), nl],
    TailPieces = report_possible_expected_actual_types(CurrPossNum + 1,
        Mismatches),
    Pieces = HeadPieces ++ TailPieces.

%-----------------------------------------------------------------------------%

report_error_var(ClauseContext, GoalContext, Context, Var, Type,
        TypeAssignSet0) = SpecAndMaybeActualExpected :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    GoalContextPieces = goal_context_to_pieces(ClauseContext, GoalContext),

    get_type_stuff(TypeAssignSet0, Var, TypeStuffList),
    ActualExpectedList0 = list.map(type_stuff_to_actual_expected(Type),
        TypeStuffList),
    list.sort_and_remove_dups(ActualExpectedList0, ActualExpectedList),

    Pieces1 = [words("type error:")],
    VarSet = ClauseContext ^ tecc_varset,
    ( if ActualExpectedList = [ActualExpected] then
        MaybeActualExpected = yes(ActualExpected),
        ActualExpected = actual_expected_types(ActualPieces, ExpectedPieces),
        Pieces2 = argument_name_to_pieces(VarSet, Var) ++
            [words("has type")] ++ ActualPieces ++ [suffix(","), nl,
            words("expected type was")] ++ ExpectedPieces ++ [suffix("."), nl]
    else
        MaybeActualExpected = no,
        Pieces2 = [words("type of")] ++
            argument_name_to_pieces(VarSet, Var) ++
            [words("does not match its expected type;"), nl] ++
            argument_name_to_pieces(VarSet, Var) ++
            [words("has overloaded actual/expected types {")] ++
            [nl_indent_delta(1)] ++
            actual_expected_types_list_to_pieces(ActualExpectedList) ++
            [nl_indent_delta(-1), fixed("}."), nl]
    ),

    VerbosePieces = type_assign_set_msg_to_pieces(TypeAssignSet0, VarSet),
    Msg = simple_msg(Context,
        [always(InClauseForPieces), always(GoalContextPieces),
        always(Pieces1 ++ Pieces2),
        verbose_only(verbose_always, VerbosePieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]),
    SpecAndMaybeActualExpected =
        spec_and_maybe_actual_expected(Spec, MaybeActualExpected).

%-----------------------------------------------------------------------------%

report_arg_vector_type_errors(ClauseContext, Context, ArgVectorKind,
        TypeAssignSet0, ArgVectorTypeErrors0) = Spec :-
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
    VerbosePieces = type_assign_set_msg_to_pieces(TypeAssignSet0, VarSet),
    Msg = simple_msg(Context,
        [always(InClauseForPieces), always(ArgVectorKindPieces),
        always(ArgErrorPieces), verbose_only(verbose_always, VerbosePieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

:- pred arg_vector_type_errors_to_pieces(prog_varset::in,
    list(arg_vector_type_error)::in,
    arg_vector_type_error::in, list(arg_vector_type_error)::in,
    list(format_component)::out) is det.

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
    ActualExpected = actual_expected_types(ActualPieces, ExpectedPieces),
    find_possible_switched_positions(VarSet, ActualPieces, AllErrors,
        MismatchPieces),
    Pieces = [words("in argument"), int_fixed(ArgNum), suffix(":"),
        nl_indent_delta(1) |
        argument_name_to_pieces(VarSet, Var)] ++
        [words("has type")] ++ ActualPieces ++ [suffix(","), nl,
        words("expected type was")] ++ ExpectedPieces ++ MismatchPieces ++
        [SuffixPiece, nl_indent_delta(-1) | TailPieces].

:- pred find_possible_switched_positions(prog_varset::in,
    list(format_component)::in, list(arg_vector_type_error)::in,
    list(format_component)::out) is det.

find_possible_switched_positions(VarSet, SearchActualPieces, AllErrors,
        Pieces) :-
    find_expecteds_matching_actual(VarSet, SearchActualPieces, AllErrors,
        MismatchPieces),
    (
        MismatchPieces = [],
        Pieces = []
    ;
        MismatchPieces = [_ | _],
        Pieces = [nl, prefix("("),
            words("the actual type is the same as the expected type of")] ++
            MismatchPieces ++ [suffix(")")]
    ).

:- pred find_expecteds_matching_actual(prog_varset::in,
    list(format_component)::in, list(arg_vector_type_error)::in,
    list(format_component)::out) is det.

find_expecteds_matching_actual(_VarSet, _SearchActualPieces, [], []).
find_expecteds_matching_actual(VarSet, SearchActualPieces,
        [HeadError | TailErrors], MismatchPieces) :-
    find_expecteds_matching_actual(VarSet, SearchActualPieces, TailErrors,
        TailMismatchPieces),
    HeadError = arg_vector_type_error(ArgNum, Var, ActualExpected),
    ActualExpected = actual_expected_types(_ActualPieces, ExpectedPieces),
    ( if SearchActualPieces = ExpectedPieces then
        ( if varset.search_name(VarSet, Var, _) then
            HeadMismatchPieces = [words("argument"), int_fixed(ArgNum),
                suffix(","), words("which is variable"),
                quote(mercury_var_to_name_only(VarSet, Var))]
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

%-----------------------------------------------------------------------------%

report_error_var_either_type(ClauseContext, GoalContext, Context,
        Var, TypeA, TypeB, TypeAssignSet0) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    GoalContextPieces = goal_context_to_pieces(ClauseContext, GoalContext),

    get_type_stuff(TypeAssignSet0, Var, TypeStuffList),
    ActualExpectedListA0 = list.map(type_stuff_to_actual_expected(TypeA),
        TypeStuffList),
    ActualExpectedListB0 = list.map(type_stuff_to_actual_expected(TypeB),
        TypeStuffList),
    list.sort_and_remove_dups(ActualExpectedListA0, ActualExpectedListA),
    list.sort_and_remove_dups(ActualExpectedListB0, ActualExpectedListB),

    Pieces1 = [words("type error:")],
    VarSet = ClauseContext ^ tecc_varset,
    ( if
        ActualExpectedListA = [ActualExpectedA],
        ActualExpectedListB = [ActualExpectedB]
    then
        ActualExpectedA = actual_expected_types(ActualPieces, ExpectedPiecesA),
        ActualExpectedB = actual_expected_types(_, ExpectedPiecesB),
        Pieces2 = argument_name_to_pieces(VarSet, Var) ++
            [words("has type")] ++ ActualPieces ++ [suffix(","), nl,
            words("expected type was either")] ++ ExpectedPiecesA ++
            [words("or")] ++ ExpectedPiecesB ++ [suffix("."), nl]
    else
        Pieces2 = [words("type of")] ++
            argument_name_to_pieces(VarSet, Var) ++
            [words("does not match its expected type;"), nl] ++
            argument_name_to_pieces(VarSet, Var) ++
            [words("has overloaded actual/expected types {")] ++
            [nl_indent_delta(1)] ++
            actual_expected_types_list_to_pieces(ActualExpectedListA) ++
            [nl_indent_delta(-1), fixed("} or {"), nl_indent_delta(1)] ++
            actual_expected_types_list_to_pieces(ActualExpectedListB) ++
            [nl_indent_delta(-1), fixed("}."), nl]
    ),

    VerbosePieces = type_assign_set_msg_to_pieces(TypeAssignSet0, VarSet),
    Msg = simple_msg(Context,
        [always(InClauseForPieces ++ GoalContextPieces),
        always(Pieces1 ++ Pieces2),
        verbose_only(verbose_always, VerbosePieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

report_error_arg_var(ClauseContext, GoalContext, Context, Var,
        ArgTypeAssignSet0) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    GoalContextPieces = goal_context_to_pieces(ClauseContext, GoalContext),

    get_arg_type_stuff(ArgTypeAssignSet0, Var, ArgTypeStuffList),
    ActualExpectedList0 = list.map(arg_type_stuff_to_actual_expected,
        ArgTypeStuffList),
    list.sort_and_remove_dups(ActualExpectedList0, ActualExpectedList),

    Pieces1 = [words("type error:")],
    VarSet = ClauseContext ^ tecc_varset,
    ( if ActualExpectedList = [ActualExpected] then
        ActualExpected = actual_expected_types(ActualPieces, ExpectedPieces),
        Pieces2 = argument_name_to_pieces(VarSet, Var) ++
            [words("has type")] ++ ActualPieces ++ [suffix(","), nl,
            words("expected type was")] ++ ExpectedPieces ++ [suffix("."), nl]
    else
        Pieces2 = [words("type of")] ++
            argument_name_to_pieces(VarSet, Var) ++
            [words("does not match its expected type;"), nl] ++
            argument_name_to_pieces(VarSet, Var) ++
            [words("has overloaded actual/expected types {")] ++
            [nl_indent_delta(1)] ++
            actual_expected_types_list_to_pieces(ActualExpectedList) ++
            [nl_indent_delta(-1), fixed("}."), nl]
    ),

    VerbosePieces = args_type_assign_set_msg_to_pieces(ArgTypeAssignSet0,
        VarSet),
    Msg = simple_msg(Context,
        [always(InClauseForPieces ++ GoalContextPieces),
        always(Pieces1 ++ Pieces2),
        verbose_only(verbose_always, VerbosePieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

report_error_undef_cons(ClauseContext, GoalContext, Context,
        ConsErrors, Functor, Arity) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    GoalContextPieces = goal_context_to_pieces(ClauseContext, GoalContext),
    InitComp = always(InClauseForPieces ++ GoalContextPieces),

    % Check for some special cases, so that we can give clearer error messages.
    ( if
        Functor = cons(unqualified(FunctorName), FunctorArity, _),
        expect(unify(Arity, FunctorArity), $pred, "arity mismatch"),
        ( if
            language_builtin_functor_components(FunctorName, Arity,
                FunctorComps0)
        then
            FunctorComps1 = FunctorComps0
        else if
            syntax_functor_components(FunctorName, FunctorArity, FunctorComps0)
        then
            FunctorComps1 = FunctorComps0
        else
            fail
        )
    then
        FunctorComps = FunctorComps1,
        ReportConsErrors = no
    else if
        Functor = cons(Constructor, FunctorArity, _),
        expect(unify(Arity, FunctorArity), $pred, "arity mismatch"),
        ModuleInfo = ClauseContext ^ tecc_module_info,

        module_info_get_cons_table(ModuleInfo, ConsTable),
        return_cons_arities(ConsTable, Constructor, ConsArities),

        module_info_get_predicate_table(ModuleInfo, PredTable),
        predicate_table_lookup_sym(PredTable, may_be_partially_qualified,
            Constructor, PredIds),
        return_function_arities(ModuleInfo, PredIds, [], FuncArities),

        list.sort_and_remove_dups(ConsArities ++ FuncArities, AllArities),
        list.delete_all(AllArities, Arity, OtherArities),
        OtherArities = [_ | _]
    then
        FunctorPieces = wrong_arity_constructor_to_pieces(Constructor, Arity,
            OtherArities),
        FunctorComps = [always(FunctorPieces)],
        ReportConsErrors = yes
    else
        Pieces1 = [words("error: undefined symbol"),
            qual_cons_id_and_maybe_arity(Functor)],
        ( if
            Functor = cons(Constructor, _, _),
            Constructor = qualified(ModQual, _)
        then
            Pieces2 = maybe_report_missing_import_addendum(ClauseContext,
                ModQual)
        else if
            Functor = cons(unqualified("[|]"), 2, _)
        then
            Pieces2 = maybe_report_missing_import_addendum(ClauseContext,
                unqualified("list"))
        else
            Pieces2 = [suffix("."), nl]
        ),
        FunctorComps = [always(Pieces1 ++ Pieces2)],
        ReportConsErrors = yes
    ),
    ( if
        ReportConsErrors = yes,
        ConsErrors = [_ | _]
    then
        ConsMsgLists = list.map(report_cons_error(Context), ConsErrors),
        list.condense(ConsMsgLists, ConsMsgs)
    else
        ConsMsgs = []
    ),
    Spec = error_spec(severity_error, phase_type_check,
        [simple_msg(Context, [InitComp | FunctorComps]) | ConsMsgs]).

:- pred language_builtin_functor_components(string::in, arity::in,
    list(error_msg_component)::out) is semidet.

language_builtin_functor_components(Name, Arity, Components) :-
    language_builtin_functor(Name, Arity),
    MainPieces = [words("error: the language construct"),
        unqual_sym_name_and_arity(sym_name_arity(unqualified(Name), Arity)),
        words("should be used as a goal, not as an expression."), nl],
    VerbosePieces = [words("If you are trying to use a goal"),
        words("as a boolean function, you should write"),
        words_quote("if <goal> then yes else no"), words("instead."), nl],
    ( if Name = "call" then
        VerboseCallPieces =
            [words("If you are trying to invoke"),
            words("a higher-order function,"),
            words("you should use"), quote("apply"), suffix(","),
            words("not"), quote("call"), suffix("."), nl,
            words("If you're trying to curry"),
            words("a higher-order predicate,"),
            words("see the ""Creating higher-order terms"" section of"),
            words("the Mercury Language Reference Manual."), nl,
            words("If you really are trying to use"), quote("call"),
            words("as an expression and not as an application of"),
            words("the language builtin call/N, make sure that"),
            words("you have the arity correct, and that the functor"),
            quote("call"), words("is actually defined (if it is defined"),
            words("in a separate module, check that the module is"),
            words("correctly imported)."), nl]
    else
        VerboseCallPieces = []
    ),
    Components = [always(MainPieces),
        verbose_only(verbose_always, VerbosePieces),
        verbose_only(verbose_once, VerboseCallPieces)].

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
        unqual_sym_name_and_arity(sym_name_arity(unqualified("[|]"), 2)),
        suffix(","), words("not"), quote("./2"),
        suffix("."), nl],
    Components = [always(Pieces)].
syntax_functor_components("!", 1, Components) :-
    Pieces1 = [words("error: invalid use of"), quote("!"),
        words("state variable operator."), nl],
    Pieces2 = [words("You probably meant to use"), quote("!."),
        words("or"), quote("!:"), suffix("."), nl],
    Components = [always(Pieces1), verbose_only(verbose_always, Pieces2)].

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
    = list(format_component).

wrong_arity_constructor_to_pieces(Name, Arity, ActualArities) = Pieces :-
    MaybePredOrFunc = no,
    NumArgsPieces = error_num_args_to_pieces(MaybePredOrFunc, Arity,
        ActualArities),
    Pieces = [words("error: ")] ++ NumArgsPieces ++
        [words("in use of constructor"), qual_sym_name(Name), suffix(".")].

:- func report_cons_error(prog_context, cons_error) = list(error_msg).

report_cons_error(Context, ConsError) = Msgs :-
    (
        ConsError = foreign_type_constructor(TypeCtor, _),
        TypeCtor = type_ctor(TypeName, TypeArity),
        Pieces = [words("There are"),
            pragma_decl("foreign_type"),
            words("declarations for type"),
            qual_sym_name_and_arity(sym_name_arity(TypeName, TypeArity)),
            suffix(","),
            words("so it is treated as an abstract type"),
            words("in all predicates and functions"),
            words("which are not implemented"),
            words("for those foreign types."), nl],
        Msgs = [simple_msg(Context, [always(Pieces)])]
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
            TVarsStr = mercury_var_to_name_only(TVarSet, TVar),
            Pieces2 = [words("variable"), quote(TVarsStr), words("occurs")]
        ;
            TVars = [_, _ | _],
            TVarsStr = mercury_vars_to_name_only(TVarSet, TVars),
            Pieces2 = [words("variables"), quote(TVarsStr), words("occur")]
        ),
        Pieces3 = [words("in the types of field"), unqual_sym_name(FieldName),
            words("and some other field"),
            words("in definition of constructor"),
            qual_cons_id_and_maybe_arity(ConsId), suffix("."), nl],
        Pieces = Pieces1 ++ Pieces2 ++ Pieces3,
        Msgs = [simple_msg(DefnContext, [always(Pieces)])]
    ;
        ConsError = new_on_non_existential_type(TypeCtor),
        TypeCtor = type_ctor(TypeName, TypeArity),
        Pieces = [words("Invalid use of"), quote("new"),
            words("on a constructor of type"),
            qual_sym_name_and_arity(sym_name_arity(TypeName, TypeArity)),
            words("which is not existentially typed."), nl],
        Msgs = [simple_msg(Context, [always(Pieces)])]
    ).

%-----------------------------------------------------------------------------%

report_ambiguity_error(ClauseContext, Context, OverloadedSymbolMap,
        TypeAssign1, TypeAssign2) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    Pieces1 =
        [words("error: ambiguous overloading causes type ambiguity."), nl],
    VarSet = ClauseContext ^ tecc_varset,
    type_assign_get_var_types(TypeAssign1, VarTypes1),
    vartypes_vars(VarTypes1, Vars1),
    AmbiguityPieces = ambiguity_error_possibilities_to_pieces(Vars1, VarSet,
        TypeAssign1, TypeAssign2),
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
    Spec = error_spec(severity_error, phase_type_check,
        [MainMsg | WarningMsgs]).

:- func add_qualifiers_reminder = list(format_component).

add_qualifiers_reminder = [
    words("You will need to add an explicit type qualification"),
    words("to resolve the type ambiguity."),
    words("The way to add an explicit type qualification"),
    words("is to use \"with_type\"."),
    words("For details see the"),  fixed("\"Explicit type qualification\""),
    words(" sub-section of the \"Data-terms\" section of the"),
    words("\"Syntax\" chapter of the Mercury language reference manual.")
].

:- func ambiguity_error_possibilities_to_pieces(list(prog_var), prog_varset,
    type_assign, type_assign) = list(format_component).

ambiguity_error_possibilities_to_pieces([], _VarSet,
        _TypeAssign1, _TypeAssign2) = [].
ambiguity_error_possibilities_to_pieces([Var | Vars], VarSet,
        TypeAssign1, TypeAssign2) = Pieces :-
    type_assign_get_var_types(TypeAssign1, VarTypes1),
    type_assign_get_var_types(TypeAssign2, VarTypes2),
    type_assign_get_type_bindings(TypeAssign1, TypeBindings1),
    type_assign_get_type_bindings(TypeAssign2, TypeBindings2),
    type_assign_get_external_type_params(TypeAssign1, ExternalTypeParams1),
    type_assign_get_external_type_params(TypeAssign2, ExternalTypeParams2),
    ( if
        search_var_type(VarTypes1, Var, Type1),
        search_var_type(VarTypes2, Var, Type2),
        apply_rec_subst_to_type(TypeBindings1, Type1, T1),
        apply_rec_subst_to_type(TypeBindings2, Type2, T2),
        not identical_types(T1, T2)
    then
        type_assign_get_typevarset(TypeAssign1, TVarSet1),
        type_assign_get_typevarset(TypeAssign2, TVarSet2),
        HeadPieces =
            [words(mercury_var_to_name_only(VarSet, Var)), suffix(":")] ++
            type_to_pieces(add_quotes, T1, TVarSet1, ExternalTypeParams1) ++
            [words("or")] ++
            type_to_pieces(add_quotes, T2, TVarSet2, ExternalTypeParams2) ++
            [nl]
    else
        HeadPieces = []
    ),
    TailPieces = ambiguity_error_possibilities_to_pieces(Vars, VarSet,
        TypeAssign1, TypeAssign2),
    Pieces = HeadPieces ++ TailPieces.

    % Check whether two types are identical, i.e. whether they can be unified
    % without binding any type parameters.
    %
:- pred identical_types(mer_type::in, mer_type::in) is semidet.

identical_types(Type1, Type2) :-
    map.init(TypeSubst0),
    type_unify(Type1, Type2, [], TypeSubst0, TypeSubst),
    TypeSubst = TypeSubst0.

%-----------------------------------------------------------------------------%

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
    % XXX this won't be very pretty when there are multiple type_assigns.
    Pieces2 = component_list_to_line_pieces(ConstraintPieceLists,
        [suffix(".")]),

    Msg = simple_msg(Context,
        [always(InClauseForPieces ++ Pieces1 ++ Pieces2)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

:- pred constraints_to_pieces(type_assign::in, list(format_component)::out,
    int::in, int::out) is det.

constraints_to_pieces(TypeAssign, Pieces, !NumUnsatisfied) :-
    type_assign_get_typeclass_constraints(TypeAssign, Constraints),
    UnprovenConstraints = Constraints ^ hcs_unproven,
    retrieve_prog_constraint_list(UnprovenConstraints,
        UnprovenProgConstraints0),

    type_assign_get_typevarset(TypeAssign, VarSet),
    type_assign_get_type_bindings(TypeAssign, Bindings),
    apply_rec_subst_to_prog_constraint_list(Bindings,
        UnprovenProgConstraints0, UnprovenProgConstraints1),
    list.sort_and_remove_dups(UnprovenProgConstraints1,
        UnprovenProgConstraints),
    !:NumUnsatisfied = !.NumUnsatisfied + list.length(UnprovenProgConstraints),
    UnprovenProgConstraintStrings =
        list.map(mercury_constraint_to_string(VarSet),
            UnprovenProgConstraints),
    UnprovenProgConstraintsPieces =
        list.map(wrap_quote, UnprovenProgConstraintStrings),
    Pieces = component_list_to_pieces("and", UnprovenProgConstraintsPieces).

:- func wrap_quote(string) = format_component.

wrap_quote(Str) = quote(Str).

%-----------------------------------------------------------------------------%

report_missing_tvar_in_foreign_code(ClauseContext, Context, VarName) = Spec :-
    ModuleInfo = ClauseContext ^ tecc_module_info,
    PredId = ClauseContext ^ tecc_pred_id,
    Pieces = [words("The foreign language code for") |
        describe_one_pred_name(ModuleInfo, should_module_qualify, PredId)] ++
        [words("should define the variable"), quote(VarName), suffix(".")],
    Spec = error_spec(severity_error, phase_type_check,
        [simple_msg(Context, [always(Pieces)])]).

%-----------------------------------------------------------------------------%

:- func types_of_vars_to_pieces(list(prog_var), prog_varset, type_assign_set)
    = list(format_component).

types_of_vars_to_pieces([], _, _) = [suffix("."), nl].
types_of_vars_to_pieces([Var | Vars], VarSet, TypeAssignSet) =
    [suffix(","), nl] ++
    argument_name_to_pieces(VarSet, Var) ++
    type_of_var_to_pieces(TypeAssignSet, Var) ++
    types_of_vars_to_pieces(Vars, VarSet, TypeAssignSet).

:- func argument_name_to_pieces(prog_varset, prog_var)
    = list(format_component).

argument_name_to_pieces(VarSet, Var) = Pieces :-
    ( if varset.search_name(VarSet, Var, _) then
        Pieces = [words("variable"),
            quote(mercury_var_to_name_only(VarSet, Var))]
    else
        Pieces = [words("argument")]
    ).

:- func functor_name_to_pieces(cons_id, arity) = list(format_component).

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

:- func type_of_var_to_pieces(type_assign_set, prog_var)
    = list(format_component).

type_of_var_to_pieces(TypeAssignSet, Var) = Pieces :-
    get_type_stuff(TypeAssignSet, Var, TypeStuffList),
    TypeStrs0 = list.map(typestuff_to_typestr, TypeStuffList),
    list.sort_and_remove_dups(TypeStrs0, TypeStrs),
    ( if TypeStrs = [TypeStr] then
        Pieces = [words("has type"), words(add_quotes(TypeStr))]
    else
        Pieces = [words("has overloaded type {"), nl_indent_delta(2)] ++
            component_list_to_line_pieces(
                list.map(string_to_pieces, TypeStrs), []) ++
            [nl_indent_delta(-2), words("}")]
    ).

:- func type_of_functor_to_pieces(cons_id, int, list(cons_type_info))
    = list(format_component).

type_of_functor_to_pieces(Functor, Arity, ConsDefnList) = Pieces :-
    ( if ConsDefnList = [SingleDefn] then
        ( if Arity = 0 then
            SepPieces = []
        else
            SepPieces = [nl]
        ),
        ConsTypePieces = cons_type_to_pieces(SingleDefn, Functor),
        Pieces = [words("has type")] ++ SepPieces ++
            [prefix("`")] ++ ConsTypePieces ++ [suffix("'")]
    else
        ConsTypeListPieces =
            cons_type_list_to_pieces(ConsDefnList, Functor, Arity),
        Pieces = [words("has overloaded type {"), nl_indent_delta(1)] ++
            ConsTypeListPieces ++ [nl_indent_delta(-1), fixed("}")]
    ).

    % Return a description of the given data constructor's argument types.
    %
    % The caller should ensure that these pieces are indented one or two levels
    % to separate them from surrounding material.
    %
:- func cons_type_to_pieces(cons_type_info, cons_id) = list(format_component).

cons_type_to_pieces(ConsInfo, Functor) = Pieces :-
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
                type_to_pieces(do_not_add_quotes, Type, TVarSet, ExistQVars) ++
                [suffix(":")]
        else
            unexpected($pred, "invalid cons_id")
        )
    ;
        ArgTypes = [],
        ArgPieces = []
    ),
    Pieces = ArgPieces ++
        type_to_pieces(do_not_add_quotes, ConsType, TVarSet, ExistQVars).

    % Return a description of the  argument types of the given list of
    % data constructors.
    %
    % The caller should ensure that these pieces are indented one or two levels
    % to separate them from surrounding material.
    %
:- func cons_type_list_to_pieces(list(cons_type_info), cons_id, int)
    = list(format_component).

cons_type_list_to_pieces([], _, _) = [].
cons_type_list_to_pieces([ConsDefn | ConsDefns], Functor, Arity) = Pieces :-
    ThisPieces = cons_type_to_pieces(ConsDefn, Functor),
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
        TailPieces = cons_type_list_to_pieces(ConsDefns, Functor, Arity),
        Pieces = ThisPieces ++ ConnectPieces ++ TailPieces
    ).

    % Return a description of the current set of type assignments.
    %
    % Since this description can be very large and unwieldy, containing
    % much irrelevant information as well as (hopefully) one or two useful
    % pieces of information, it is intended to be used only with
    % --verbose-errors.
    %
:- func type_assign_set_msg_to_pieces(type_assign_set, prog_varset)
    = list(format_component).

type_assign_set_msg_to_pieces(TypeAssignSet0, VarSet) = Pieces :-
    ( if TypeAssignSet0 = [_] then
        FirstWords = "The partial type assignment was:",
        MaybeSeq = no
    else
        FirstWords = "The possible partial type assignments were:",
        MaybeSeq = yes(1)
    ),
    list.sort(TypeAssignSet0, TypeAssignSet),
    LaterPieces = type_assign_set_to_pieces(TypeAssignSet, MaybeSeq, VarSet),
    Pieces = [words(FirstWords), nl_indent_delta(1) | LaterPieces] ++
        [nl_indent_delta(-1)].

    % Return a description of the current set of type assignments.
    %
    % Since this description can be very large and unwieldy, containing
    % much irrelevant information as well as (hopefully) one or two useful
    % pieces of information, it is intended to be used only with
    % --verbose-errors.
    %
:- func args_type_assign_set_msg_to_pieces(args_type_assign_set, prog_varset)
    = list(format_component).

args_type_assign_set_msg_to_pieces(ArgTypeAssignSet0, VarSet) = Pieces :-
    ( if ArgTypeAssignSet0 = [_] then
        FirstWords = "The partial type assignment was:",
        MaybeSeq = no
    else
        FirstWords = "The possible partial type assignments were:",
        MaybeSeq = yes(1)
    ),
    list.sort(ArgTypeAssignSet0, ArgTypeAssignSet),
    LaterPieces = args_type_assign_set_to_pieces(ArgTypeAssignSet, MaybeSeq,
        VarSet),
    Pieces = [words(FirstWords), nl_indent_delta(1) | LaterPieces] ++
        [nl_indent_delta(-1)].

:- func type_stuff_to_actual_expected(mer_type, type_stuff)
    = actual_expected_types.

type_stuff_to_actual_expected(Type, VarTypeStuff) = ActualExpected :-
    VarTypeStuff = type_stuff(VarType, TVarSet, TypeBinding,
        ExternalTypeParams),
    ActualPieces = bound_type_to_pieces(VarType, TVarSet, TypeBinding,
        ExternalTypeParams),
    ExpectedPieces = bound_type_to_pieces(Type, TVarSet, TypeBinding,
        ExternalTypeParams),
    ActualExpected = actual_expected_types(ActualPieces, ExpectedPieces).

:- func arg_type_stuff_to_actual_expected(arg_type_stuff) =
    actual_expected_types.

arg_type_stuff_to_actual_expected(ArgTypeStuff) = ActualExpected :-
    ArgTypeStuff = arg_type_stuff(Type, VarType, TVarSet, ExternalTypeParams),
    ActualPieces = type_to_pieces(add_quotes, VarType, TVarSet,
        ExternalTypeParams),
    ExpectedPieces = type_to_pieces(add_quotes, Type, TVarSet,
        ExternalTypeParams),
    ActualExpected = actual_expected_types(ActualPieces, ExpectedPieces).

:- func actual_expected_types_list_to_pieces(list(actual_expected_types))
    = list(format_component).

actual_expected_types_list_to_pieces(ActualExpectedList) = Pieces :-
    ExpectedPieces = list.foldl(expected_types_to_pieces, ActualExpectedList,
        []),
    ActualPieces = list.map(actual_types_to_pieces, ActualExpectedList),
    Pieces = component_list_to_line_pieces(ExpectedPieces ++ ActualPieces, []).

:- func expected_types_to_pieces(actual_expected_types,
    list(list(format_component))) = list(list(format_component)).

expected_types_to_pieces(ActualExpected, Pieces0) = Pieces :-
    ActualExpected = actual_expected_types(_ActualPieces, ExpectedPieces),
    TaggedPieces = [words("(expected)") | ExpectedPieces],
    ( if list.member(TaggedPieces, Pieces0) then
        Pieces = Pieces0
    else
        Pieces = Pieces0 ++ [TaggedPieces]
    ).

:- func actual_types_to_pieces(actual_expected_types) = list(format_component).

actual_types_to_pieces(ActualExpected) = Pieces :-
    ActualExpected = actual_expected_types(ActualPieces, _ExpectedPieces),
    Pieces = [words("(inferred)") | ActualPieces].

:- func bound_type_to_pieces(mer_type, tvarset, tsubst, external_type_params)
    = list(format_component).

bound_type_to_pieces(Type0, TypeVarSet, TypeBindings, ExternalTypeParams)
        = Pieces :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type),
    Pieces = type_to_pieces(add_quotes, Type, TypeVarSet, ExternalTypeParams).

%-----------------------------------------------------------------------------%

:- func maybe_report_missing_import_addendum(type_error_clause_context,
    module_name) = list(format_component).

maybe_report_missing_import_addendum(ClauseContext, ModuleQualifier)
        = Pieces :-
    % First check if this module wasn't imported.
    ModuleInfo = ClauseContext ^ tecc_module_info,
    module_info_get_visible_modules(ModuleInfo, VisibleModules),

    set.filter(partial_sym_name_matches_full(ModuleQualifier),
        VisibleModules, MatchingVisibleModules),
    ( if set.is_empty(MatchingVisibleModules) then
        % The module qualifier does not match any of the visible modules,
        % so we report that the module has not been imported.
        Pieces = [nl, words("(the module"), qual_sym_name(ModuleQualifier),
            words("has not been imported)."), nl]
    else
        % The module qualifier matches one or more of the visible modules.
        % But maybe the user forgot to import the parent module(s) of that
        % module...
        solutions.solutions(
            get_unimported_parent(VisibleModules, MatchingVisibleModules),
            UnimportedParents),
        (
            UnimportedParents = [_ | _],
            Pieces = [nl | report_unimported_parents(UnimportedParents)]
        ;
            UnimportedParents = [],
            Pieces = [suffix("."), nl]
        )
    ).

    % Nondeterministically return all the possible parent modules
    % of a module in MatchingModuleNames which are not imported
    % (i.e. not visible).
    %
:- pred get_unimported_parent(set(module_name)::in, set(module_name)::in,
   module_name::out) is nondet.

get_unimported_parent(VisibleModules, MatchingModuleNames, UnimportedParent) :-
    set.member(MatchingModuleName, MatchingModuleNames),
    ParentModules = get_ancestors(MatchingModuleName),
    list.member(UnimportedParent, ParentModules),
    not set.contains(VisibleModules, UnimportedParent).

:- func report_unimported_parents(list(module_name)) = list(format_component).

report_unimported_parents(UnimportedParents) = Pieces :-
    UnimportedParentDescs = list.map(describe_sym_name, UnimportedParents),
    AllUnimportedParents = list_to_pieces(UnimportedParentDescs),
    ( if AllUnimportedParents = [_] then
        Pieces = [words("(the possible parent module")]
            ++ AllUnimportedParents ++ [words("has not been imported)."), nl]
    else
        Pieces = [words("(the possible parent modules")]
            ++ AllUnimportedParents ++ [words("have not been imported)."), nl]
    ).

%-----------------------------------------------------------------------------%

:- func goal_context_to_pieces(type_error_clause_context,
    type_error_goal_context) = list(format_component).

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
                    ArgVectorKind = arg_vector_plain_call(SimpleCallId),
                    CallId = plain_call_id(SimpleCallId)
                ;
                    ArgVectorKind = arg_vector_plain_call_pred_id(PredId),
                    ModuleInfo = ClauseContext ^ tecc_module_info,
                    module_info_pred_info(ModuleInfo, PredId, PredInfo),
                    pred_info_get_module_name(PredInfo, ModuleName),
                    pred_info_get_name(PredInfo, Name),
                    pred_info_get_orig_arity(PredInfo, Arity),
                    pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
                    SimpleCallId = simple_call_id(PredOrFunc,
                        qualified(ModuleName, Name), Arity),
                    CallId = plain_call_id(SimpleCallId)
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
                unexpected($pred, "arg_vector_foreign_proc_call")
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
    = list(format_component).

arg_vector_kind_to_pieces(ClauseContext, ArgVectorKind) = Pieces :-
    (
        ArgVectorKind = arg_vector_clause_head,
        Pieces = [words("in arguments of the clause head:"), nl]
    ;
        (
            ArgVectorKind = arg_vector_plain_call(SimpleCallId),
            CallId = plain_call_id(SimpleCallId)
        ;
            ArgVectorKind = arg_vector_plain_call_pred_id(PredId),
            ModuleInfo = ClauseContext ^ tecc_module_info,
            module_info_pred_info(ModuleInfo, PredId, PredInfo),
            pred_info_get_module_name(PredInfo, ModuleName),
            pred_info_get_name(PredInfo, Name),
            pred_info_get_orig_arity(PredInfo, Arity),
            pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
            SimpleCallId = simple_call_id(PredOrFunc,
                qualified(ModuleName, Name), Arity),
            CallId = plain_call_id(SimpleCallId)
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
    list(format_component).

in_clause_for_pieces(ClauseContext) = Pieces :-
    ModuleInfo = ClauseContext ^ tecc_module_info,
    PredId = ClauseContext ^ tecc_pred_id,
    PredIdPieces = describe_one_pred_name(ModuleInfo,
        should_not_module_qualify, PredId),
    Pieces = [words("In clause for") | PredIdPieces] ++ [suffix(":"), nl].

    % error_num_args_to_pieces(MaybePredOrFunc, Arity, CorrectArities):
    %
    % Return a description for the error message
    % "wrong number of arguments (<Arity>; should be <CorrectArities>)",
    % adjusting `Arity' and `CorrectArities' if `MaybePredOrFunc' is
    % `yes(function)'.
    %
:- func error_num_args_to_pieces(maybe(pred_or_func), int, list(int)) =
    list(format_component).

error_num_args_to_pieces(MaybePredOrFunc, Arity0, Arities0) = Pieces :-
    % Adjust arities for functions.
    ( if MaybePredOrFunc = yes(pf_function) then
        adjust_func_arity(pf_function, Arity, Arity0),
        ReverseAdjust =
            ( pred(OtherArity0::in, OtherArity::out) is det :-
                adjust_func_arity(pf_function, OtherArity, OtherArity0)
            ),
        list.map(ReverseAdjust, Arities0, Arities)
    else
        Arity = Arity0,
        Arities = Arities0
    ),
    RightAritiesPieces = error_right_num_args_to_pieces(Arities),
    Pieces = [words("wrong number of arguments ("),
        suffix(int_to_string(Arity)), suffix(";"),
        words("should be") | RightAritiesPieces] ++ [suffix(")")].

:- func error_right_num_args_to_pieces(list(int)) = list(format_component).

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

%-----------------------------------------------------------------------------%

:- type type_stuff
    --->    type_stuff(
                type_stuff_base_type            :: mer_type,
                type_stuff_tvarset              :: tvarset,
                type_stuff_binding              :: tsubst,
                type_stuff_external_type_params :: external_type_params
            ).

    % Given a type assignment set and a variable, return the list of possible
    % different types for the variable.
    %
:- pred get_type_stuff(type_assign_set::in, prog_var::in,
    list(type_stuff)::out) is det.

get_type_stuff([], _Var, []).
get_type_stuff([TypeAssign | TypeAssigns], Var, TypeStuffs) :-
    get_type_stuff(TypeAssigns, Var, TailTypeStuffs),
    type_assign_get_external_type_params(TypeAssign, ExternalTypeParams),
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
    TypeStuff = type_stuff(Type, TVarSet, TypeBindings, ExternalTypeParams),
    ( if list.member(TypeStuff, TailTypeStuffs) then
        TypeStuffs = TailTypeStuffs
    else
        TypeStuffs = [TypeStuff | TailTypeStuffs]
    ).

:- func typestuff_to_typestr(type_stuff) = string.

typestuff_to_typestr(TypeStuff) = TypeStr :-
    TypeStuff = type_stuff(Type0, TypeVarSet, TypeBindings,
        ExternalTypeParams),
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_builtin_qualifiers_from_type(Type1, Type),
    unparse_type(Type, Term0),
    list.map(term.coerce_var, ExternalTypeParams, ExistQVars),
    maybe_add_existential_quantifier(ExistQVars, Term0, Term),
    varset.coerce(TypeVarSet, VarSet),
    TypeStr = mercury_term_to_string(VarSet, print_name_only, Term).

:- type arg_type_stuff
    --->    arg_type_stuff(
                arg_type_stuff_arg_type             :: mer_type,
                arg_type_stuff_var_type             :: mer_type,
                arg_type_stuff_tvarset              :: tvarset,
                arg_type_stuff_external_type_params :: external_type_params
            ).

    % Given an arg type assignment set and a variable id, return the list of
    % possible different types for the argument and the variable.
    %
:- pred get_arg_type_stuff(args_type_assign_set::in, prog_var::in,
    list(arg_type_stuff)::out) is det.

get_arg_type_stuff([], _Var, []).
get_arg_type_stuff([ArgTypeAssign | ArgTypeAssigns], Var, ArgTypeStuffs) :-
    ArgTypeAssign = args_type_assign(TypeAssign, ArgTypes, _),
    get_arg_type_stuff(ArgTypeAssigns, Var, TailArgTypeStuffs),
    type_assign_get_external_type_params(TypeAssign, ExternalTypeParams),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TVarSet),
    type_assign_get_var_types(TypeAssign, VarTypes),
    ( if search_var_type(VarTypes, Var, VarType0) then
        VarType = VarType0
    else
        % This shouldn't happen - how can a variable which has
        % not yet been assigned a type variable fail to have
        % the correct type?
        VarType = defined_type(unqualified("<any>"), [], kind_star)
    ),
    list.det_index0(ArgTypes, 0, ArgType),
    apply_rec_subst_to_type(TypeBindings, ArgType, ArgType2),
    apply_rec_subst_to_type(TypeBindings, VarType, VarType2),
    ArgTypeStuff = arg_type_stuff(ArgType2, VarType2, TVarSet,
        ExternalTypeParams),
    ( if list.member(ArgTypeStuff, TailArgTypeStuffs) then
        ArgTypeStuffs = TailArgTypeStuffs
    else
        ArgTypeStuffs = [ArgTypeStuff | TailArgTypeStuffs]
    ).

    % Check if any of the variables in the term are existentially quantified
    % (occur in the first argument), and if so, add the appropriate
    % quantification to the term. Otherwise, return the term unchanged.
    %
:- pred maybe_add_existential_quantifier(list(var)::in, term::in, term::out)
    is det.

maybe_add_existential_quantifier(ExternalTypeParams, !Term) :-
    term.vars(!.Term, Vars),
    ExistQVars = set.to_sorted_list(set.intersect(
        set.list_to_set(ExternalTypeParams), set.list_to_set(Vars))),
    (
        ExistQVars = []
    ;
        ExistQVars = [_ | _],
        QTerm = make_list_term(ExistQVars),
        !:Term = term.functor(term.atom("some"), [QTerm, !.Term],
            term.context_init)
    ).

:- func make_list_term(list(var)) = term.

make_list_term([]) =
    term.functor(term.atom("[]"), [], term.context_init).
make_list_term([Var | Vars]) =
    term.functor(term.atom("[|]"),
        [term.variable(Var, context_init), make_list_term(Vars)],
        term.context_init).

%-----------------------------------------------------------------------------%

:- type maybe_add_quotes
    --->    do_not_add_quotes
    ;       add_quotes.

:- func type_to_pieces(maybe_add_quotes, mer_type, tvarset,
    external_type_params) = list(format_component).

type_to_pieces(MaybeAddQuotes, Type0, TVarSet, ExternalTypeParams) = Pieces :-
    strip_builtin_qualifiers_from_type(Type0, Type),
    unparse_type(Type, Term0),
    list.map(term.coerce_var, ExternalTypeParams, ExistQVars),
    maybe_add_existential_quantifier(ExistQVars, Term0, Term),
    varset.coerce(TVarSet, VarSet),
    TermPiece = words(mercury_term_to_string(VarSet, print_name_only, Term)),
    (
        MaybeAddQuotes = do_not_add_quotes,
        Pieces = [TermPiece]
    ;
        MaybeAddQuotes = add_quotes,
        Pieces = [prefix("`"), TermPiece, suffix("'")]
    ).

:- func string_to_pieces(string) = list(format_component).

string_to_pieces(Str) = [words(Str)].

%-----------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_errors.
%-----------------------------------------------------------------------------%
