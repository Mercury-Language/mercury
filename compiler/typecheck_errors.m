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

:- import_module check_hlds.typecheck_info.
:- import_module hlds.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type cons_error
    --->    foreign_type_constructor(type_ctor, hlds_type_defn)
    ;       abstract_imported_type
    ;       invalid_field_update(sym_name, hlds_ctor_field_defn,
                tvarset, list(tvar))
    ;       new_on_non_existential_type(type_ctor).

%-----------------------------------------------------------------------------%

:- func report_pred_call_error(typecheck_info, simple_call_id) = error_spec.

:- func report_unknown_event_call_error(typecheck_info, string) = error_spec.

:- func report_event_args_mismatch(typecheck_info, string, list(mer_type),
    list(prog_var)) = error_spec.

:- func report_no_clauses(module_info, pred_id, pred_info) = error_spec.

:- func report_no_clauses_stub(module_info, pred_id, pred_info) = error_spec.

:- func report_non_contiguous_clauses(module_info, pred_id, pred_info,
    clause_item_number_region, clause_item_number_region,
    list(clause_item_number_region)) = error_spec.

:- func report_warning_too_much_overloading(typecheck_info) = error_spec.

:- func report_error_too_much_overloading(typecheck_info) = error_spec.

:- func report_error_unif_var_var(typecheck_info, prog_var, prog_var,
    type_assign_set) = error_spec.

:- func report_error_lambda_var(typecheck_info, pred_or_func,
    lambda_eval_method, prog_var, list(prog_var), type_assign_set)
    = error_spec.

:- func report_error_functor_type(typecheck_info, prog_var,
    list(cons_type_info), cons_id, int, type_assign_set) = error_spec.

:- func report_error_functor_arg_types(typecheck_info, prog_var,
    list(cons_type_info), cons_id, list(prog_var), args_type_assign_set)
    = error_spec.

:- func report_error_var(typecheck_info, prog_var, mer_type, type_assign_set)
    = error_spec.

:- func report_error_var_either_type(typecheck_info, prog_var,
    mer_type, mer_type, type_assign_set) = error_spec.

:- func report_error_arg_var(typecheck_info, prog_var, args_type_assign_set)
    = error_spec.

:- func report_error_undef_cons(typecheck_info, list(cons_error), cons_id, int)
    = error_spec.

:- func report_ambiguity_error(typecheck_info, type_assign, type_assign)
    = error_spec.

:- func report_unsatisfiable_constraints(typecheck_info, type_assign_set)
    = error_spec.

:- func report_missing_tvar_in_foreign_code(typecheck_info, string)
    = error_spec.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.options.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

report_pred_call_error(Info, PredCallId) = Spec :-
    PredCallId = simple_call_id(PredOrFunc, SymName, _Arity),
    ModuleInfo = Info ^ tc_info_module_info,
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    typecheck_info_get_pred_markers(Info, PredMarkers),
    IsFullyQualified = calls_are_fully_qualified(PredMarkers),
    predicate_table_lookup_pf_sym(PredicateTable, IsFullyQualified,
        PredOrFunc, SymName, OtherIds),
    (
        OtherIds = [_ | _],
        predicate_table_get_preds(PredicateTable, Preds),
        typecheck_find_arities(Preds, OtherIds, Arities),
        Spec = report_error_pred_num_args(Info, PredCallId, Arities)
    ;
        OtherIds = [],
        UndefMsg = report_error_undef_pred(Info, PredCallId),
        ( PredOrFunc = pf_predicate, SwitchedPredOrFunc = pf_function
        ; PredOrFunc = pf_function,  SwitchedPredOrFunc = pf_predicate
        ),
        predicate_table_lookup_pf_sym(PredicateTable, IsFullyQualified,
            SwitchedPredOrFunc, SymName, SwitchedOtherIds),
        (
            SwitchedOtherIds = [_ | _],
            KindMsg = report_error_func_instead_of_pred(Info,
                SwitchedPredOrFunc),
            Msgs = [UndefMsg, KindMsg]
        ;
            SwitchedOtherIds = [],
            Msgs = [UndefMsg]
        ),
        Spec = error_spec(severity_error, phase_type_check, Msgs)
    ).

:- pred typecheck_find_arities(pred_table::in, list(pred_id)::in,
    list(int)::out) is det.

typecheck_find_arities(_, [], []).
typecheck_find_arities(Preds, [PredId | PredIds], [Arity | Arities]) :-
    map.lookup(Preds, PredId, PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    typecheck_find_arities(Preds, PredIds, Arities).

:- func report_error_pred_num_args(typecheck_info, simple_call_id,
    list(int)) = error_spec.

report_error_pred_num_args(Info, SimpleCallId, Arities) = Spec :-
    SimpleCallId = simple_call_id(PredOrFunc, SymName, Arity),
    Pieces = in_clause_for_pieces(Info) ++
        [words("error:")] ++
        error_num_args_to_pieces(yes(PredOrFunc), Arity, Arities) ++ [nl] ++
        [words("in call to"), p_or_f(PredOrFunc), sym_name(SymName),
        suffix("."), nl],
    Context = Info ^ tc_info_context,
    Spec = error_spec(severity_error, phase_type_check,
        [simple_msg(Context, [always(Pieces)])]).

:- func report_error_func_instead_of_pred(typecheck_info, pred_or_func)
    = error_msg.

report_error_func_instead_of_pred(Info, PredOrFunc) = Msg :-
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
    Context = Info ^ tc_info_context,
    Msg = simple_msg(Context, [always(Pieces)]).

:- func report_error_undef_pred(typecheck_info, simple_call_id) = error_msg.

report_error_undef_pred(Info, SimpleCallId) = Msg :-
    SimpleCallId = simple_call_id(_PredOrFunc, PredName, Arity),
    Context = Info ^ tc_info_context,
    InClauseForPieces = in_clause_for_pieces(Info),
    InClauseForComponent = always(InClauseForPieces),
    (
        PredName = unqualified("->"),
        ( Arity = 2 ; Arity = 4 )
    ->
        MainPieces = [words("error:"), quote("->"), words("without"),
            quote(";"), suffix("."), nl],
        MainComponent = always(MainPieces),
        VerbosePieces =
            [words("Note: the else part is not optional."), nl,
            words("Every if-then must have an else."), nl],
        VerboseComponent = verbose_only(VerbosePieces),
        Components = [MainComponent, VerboseComponent]
    ;
        PredName = unqualified("else"),
        ( Arity = 2 ; Arity = 4 )
    ->
        Components = [always([words("error: unmatched"), quote("else"),
            suffix("."), nl])]
    ;
        PredName = unqualified("if"),
        ( Arity = 2 ; Arity = 4 )
    ->
        Pieces = [words("error:"), quote("if"), words("without"),
            quote("then"), words("or"), quote("else"), suffix("."), nl],
        Components = [always(Pieces)]
    ;
        PredName = unqualified("then"),
        ( Arity = 2 ; Arity = 4 )
    ->
        MainPieces = [words("error:"), quote("then"), words("without"),
            quote("if"), words("or"), quote("else"), suffix("."), nl],
        MainComponent = always(MainPieces),
        VerbosePieces =
            [words("Note: the"), quote("else"), words("part is not optional."),
            nl, words("Every if-then must have an"),
            quote("else"), suffix("."), nl],
        VerboseComponent = verbose_only(VerbosePieces),
        Components = [MainComponent, VerboseComponent]
    ;
        PredName = unqualified("apply"),
        Arity >= 1
    ->
        Components = report_apply_instead_of_pred
    ;
        PredName = unqualified(PurityString),
        Arity = 1,
        ( PurityString = "impure" ; PurityString = "semipure" )
    ->
        MainPieces = [words("error:"), quote(PurityString),
            words("marker in an inappropriate place."), nl],
        MainComponent = always(MainPieces),
        VerbosePieces =
            [words("Such markers only belong before predicate calls."), nl],
        VerboseComponent = verbose_only(VerbosePieces),
        Components = [MainComponent, VerboseComponent]
    ;
        PredName = unqualified("some"),
        Arity = 2
    ->
        Pieces = [words("syntax error in existential quantification:"),
            words("first argument of"), quote("some"),
            words("should be a list of variables."), nl],
        Components = [always(Pieces)]
    ;
        MainPieces = [words("error: undefined"), simple_call(SimpleCallId)],
        (
            PredName = qualified(ModuleQualifier, _),
            Pieces = MainPieces ++
                maybe_report_missing_import_addendum(Info, ModuleQualifier)
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
    VerboseComponent = verbose_only(VerbosePieces),
    Components = [MainComponent, VerboseComponent].

%-----------------------------------------------------------------------------%

report_unknown_event_call_error(Info, EventName) = Spec :-
    Context = Info ^ tc_info_context,
    Pieces = [words("Error: there is no event named"),
        quote(EventName), suffix(".")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

report_event_args_mismatch(Info, EventName, EventArgTypes, Args) = Spec :-
    Context = Info ^ tc_info_context,
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
    (
        GapNumber = 1,
        LaterRegions = []
    ->
        % There is only one gap, so don't number it.
        GapPieces = []
    ;
        GapPieces = [int_fixed(GapNumber)]
    ),
    % The wording here is chosen be non-confusing even if a clause has a gap
    % both before and after it, so that gaps both end and start at the context
    % of that clause. We could do better if we had separate contexts for the
    % start and the end of the clause, but we don't.
    FirstPieces = [words("Gap") | GapPieces] ++
        [words("in clauses of") | PredPieces] ++
        [words("starts after this clause.")],
    SecondPieces = [words("Gap") | GapPieces] ++
        [words("in clauses of") | PredPieces] ++
        [words("ends with this clause.")],
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

report_warning_too_much_overloading(Info) = Spec :-
    Msgs = too_much_overloading_to_msgs(Info, no),
    Spec = error_spec(severity_warning, phase_type_check, Msgs).

report_error_too_much_overloading(Info) = Spec :-
    Msgs = too_much_overloading_to_msgs(Info, yes),
    Spec = error_spec(severity_error, phase_type_check, Msgs).

:- func too_much_overloading_to_msgs(typecheck_info, bool) = list(error_msg).

too_much_overloading_to_msgs(Info, IsError) = Msgs :-
    Context = Info ^ tc_info_context,
    (
        IsError = no,
        InitPieces = in_clause_for_pieces(Info) ++
            [words("warning: highly ambiguous overloading."), nl],
        InitComponent = always(InitPieces),

        VerbosePieces =
            [words("This may cause type-checking to be very slow."),
            words("It may also make your code difficult to understand."), nl],
        VerboseComponent = verbose_only(VerbosePieces)
    ;
        IsError = yes,
        InitPieces = in_clause_for_pieces(Info) ++
            [words("error: excessively ambiguous overloading."), nl],
        InitComponent = always(InitPieces),

        VerbosePieces =
            [words("This caused the type checker to exceed its limits."),
            words("It may also make your code difficult to understand."), nl],
        VerboseComponent = verbose_only(VerbosePieces)
    ),

    FirstMsg = simple_msg(Context, [InitComponent, VerboseComponent]),

    typecheck_info_get_overloaded_symbols(Info, OverloadedSymbolSet),
    map.to_assoc_list(OverloadedSymbolSet, OverloadedSymbols),
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
                unexpected($module, $pred, "no contexts")
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
        ModuleInfo = Info ^ tc_info_module_info,
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
        unexpected($module, $pred, "no context")
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
            ( ConsId = cons(SymName, Arity, _) ->
                ConsIdPiece = sym_name_and_arity(SymName / Arity)
            ;
                ConsIdPiece = fixed(cons_id_and_arity_to_string(ConsId))
            ),
            StartPieces = [words("The function symbol"), ConsIdPiece,
                suffix("."), nl,
                words("The possible matches are:"), nl_indent_delta(1)],
            SourcePiecesList = list.map(
                describe_cons_type_info_source(ModuleInfo), Sources),
            SourcePieces = component_list_to_line_pieces(SourcePiecesList,
                [suffix(".")]),
            FirstPieces = StartPieces ++ SourcePieces,
            LaterPieces = [words("The function symbol"), ConsIdPiece,
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
            sym_name_and_arity(SymName / Arity)]
    ;
        Source = source_builtin_type(TypeCtorName),
        Pieces = [words("the builtin type constructor"), quote(TypeCtorName)]
    ;
        Source = source_get_field_access(TypeCtor),
        TypeCtor = type_ctor(SymName, Arity),
        Pieces = [words("a"), quote("get"), words("field access function"),
            words("for the type constructor"),
            sym_name_and_arity(SymName / Arity)]
    ;
        Source = source_set_field_access(TypeCtor),
        TypeCtor = type_ctor(SymName, Arity),
        Pieces = [words("a"), quote("set"), quote("field access function"),
            words("for the type constructor"),
            sym_name_and_arity(SymName / Arity)]
    ;
        Source = source_pred(PredId),
        Pieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
            PredId)
    ;
        Source = source_apply(ApplyOp),
        Pieces = [words("the builtin operator constructor"), quote(ApplyOp)]
    ).

%-----------------------------------------------------------------------------%

report_error_unif_var_var(Info, X, Y, TypeAssignSet) = Spec :-
    Context = Info ^ tc_info_context,
    UnifyContext = Info ^ tc_info_unify_context,
    typecheck_info_get_varset(Info, VarSet),

    InClauseForPieces = in_clause_for_pieces(Info),
    unify_context_to_pieces(UnifyContext, [], UnifyContextPieces),

    MainPieces = [words("type error in unification of variable"),
        quote(mercury_var_to_string(VarSet, no, X)), nl,
        words("and variable"),
        quote(mercury_var_to_string(VarSet, no, Y)), suffix("."), nl,
        quote(mercury_var_to_string(VarSet, no, X))] ++
        type_of_var_to_pieces(TypeAssignSet, X) ++ [suffix(","), nl,
        quote(mercury_var_to_string(VarSet, no, Y))] ++
        type_of_var_to_pieces(TypeAssignSet, Y) ++ [suffix("."), nl],
    VerbosePieces = type_assign_set_msg_to_pieces(TypeAssignSet, VarSet),
    Msg = simple_msg(Context,
        [always(InClauseForPieces), always(UnifyContextPieces),
        always(MainPieces), verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

report_error_lambda_var(Info, PredOrFunc, _EvalMethod, Var, ArgVars,
        TypeAssignSet) = Spec :-
    Context = Info ^ tc_info_context,
    UnifyContext = Info ^ tc_info_unify_context,
    typecheck_info_get_varset(Info, VarSet),

    InClauseForPieces = in_clause_for_pieces(Info),
    unify_context_to_pieces(UnifyContext, [], UnifyContextPieces),

    Pieces1 = [words("type error in unification of")] ++
        argument_name_to_pieces(VarSet, Var) ++ [nl],
    (
        PredOrFunc = pf_predicate,
        Pieces2 = [words("and"), prefix("pred("),
            words(mercury_vars_to_string(VarSet, no, ArgVars)),
            suffix(")"), words(":- ...':"), nl]
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgVars, FuncArgs, RetVar),
        Pieces2 = [words("and"), prefix("func("),
            words(mercury_vars_to_string(VarSet, no, FuncArgs)),
            suffix(")"), fixed("="),
            words(mercury_var_to_string(VarSet, no, RetVar)),
            words(":- ...':"), nl]
    ),

    Pieces3 = argument_name_to_pieces(VarSet, Var) ++
        type_of_var_to_pieces(TypeAssignSet, Var) ++ [suffix(","), nl],

    LambdaExprStr = "lambda expression has type",
    Pieces4a = [words(LambdaExprStr), prefix("`")],
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
    Pieces4c = [suffix("'."), nl],
    Pieces4 = Pieces4a ++ Pieces4b ++ Pieces4c,

    VerbosePieces = type_assign_set_msg_to_pieces(TypeAssignSet, VarSet),
    Msg = simple_msg(Context,
        [always(InClauseForPieces ++ UnifyContextPieces),
        always(Pieces1 ++ Pieces2 ++ Pieces3 ++ Pieces4),
        verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

report_error_functor_type(Info, Var, ConsDefnList, Functor, Arity,
        TypeAssignSet) = Spec :-
    typecheck_info_get_varset(Info, VarSet),
    Context = Info ^ tc_info_context,
    UnifyContext = Info ^ tc_info_unify_context,

    InClauseForPieces = in_clause_for_pieces(Info),
    % XXX We could append UnifyContextPieces after InClauseForPieces
    % instead of after the empty list.
    unify_context_to_pieces(UnifyContext, [], UnifyContextPieces),

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
        always(MainPieces), verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

report_error_functor_arg_types(Info, Var, ConsDefnList, Functor, Args,
        ArgsTypeAssignSet) = Spec :-
    typecheck_info_get_varset(Info, VarSet),
    Context = Info ^ tc_info_context,
    UnifyContext = Info ^ tc_info_unify_context,
    ModuleInfo = Info ^ tc_info_module_info,
    list.length(Args, Arity),

    InClauseForPieces = in_clause_for_pieces(Info),
    unify_context_to_pieces(UnifyContext, [], UnifyContextPieces),

    strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor),
    StrippedFunctorStr = functor_cons_id_to_string(StrippedFunctor, Args,
        VarSet, ModuleInfo, no),

    Pieces1 = [words("in unification of")] ++
        argument_name_to_pieces(VarSet, Var) ++ [nl, words("and term"),
        quote(StrippedFunctorStr), suffix(":"), nl,
        words("type error in argument(s) of")] ++
        functor_name_to_pieces(StrippedFunctor, Arity) ++ [suffix("."), nl],

    ConsArgTypesSet = list.map(get_callee_arg_types, ArgsTypeAssignSet),

    % If we know the type of the function symbol, and each argument
    % also has at most one possible type, then we prefer to print an
    % error message that mentions the actual and expected types of the
    % arguments only for the arguments in which the two types differ.
    (
        list.all_same(ConsArgTypesSet),
        ConsArgTypesSet = [ConsArgTypes | _],
        assoc_list.from_corresponding_lists(Args, ConsArgTypes, ArgExpTypes),
        TypeAssigns = list.map(get_caller_arg_assign, ArgsTypeAssignSet),
        find_mismatched_args(ArgExpTypes, TypeAssigns, 1,
            SimpleMismatches, ComplexMismatches, AllMismatches),
        expect(list.is_not_empty(AllMismatches), $module, $pred,
            "no mismatches"),
        ComplexMismatches = []
    ->
        Pieces2 =
            mismatched_args_to_pieces(SimpleMismatches, yes, VarSet, Functor),
        VerboseComponents = []
    ;
        % XXX If we can compute AllMismatches, then we should use it to report
        % which arguments are OK, and which are suspect.

        TypeAssignSet = convert_args_type_assign_set(ArgsTypeAssignSet),

        % For polymorphic data structures, the type of `Var' (the functor's
        % result type) can affect the valid types for the arguments.
        (
            % Could the type of the functor be polymorphic?
            list.member(ConsDefn, ConsDefnList),
            ConsDefn ^ cti_arg_types = [_ | _]
        ->
            % If so, print out the type of `Var'.
            Pieces2a = argument_name_to_pieces(VarSet, Var) ++
                type_of_var_to_pieces(TypeAssignSet, Var) ++
                [suffix(","), nl]
        ;
            Pieces2a = []
        ),

        Pieces2b = functor_name_to_pieces(Functor, Arity) ++
            type_of_functor_to_pieces(Functor, Arity, ConsDefnList) ++
            types_of_vars_to_pieces(Args, VarSet, Info, TypeAssignSet),
        Pieces2 = Pieces2a ++ Pieces2b,

        VerbosePieces = type_assign_set_msg_to_pieces(TypeAssignSet, VarSet),
        VerboseComponents = [verbose_only(VerbosePieces)]
    ),

    Msg = simple_msg(Context,
        [always(InClauseForPieces ++ UnifyContextPieces),
        always(Pieces1 ++ Pieces2) | VerboseComponents]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

:- type mismatch_info
    --->    mismatch_info(
                int,        % argument number, starting from 1
                prog_var,   % variable in that position
                list(type_mismatch)
                    % list of possible type mismatches
            ).

:- type type_mismatch
    --->    type_mismatch(
                actual_type_desc    :: list(format_component),
                expected_type_desc  :: list(format_component)
            ).

:- pred find_mismatched_args(assoc_list(prog_var, mer_type)::in,
    type_assign_set::in, int::in, list(mismatch_info)::out,
    list(mismatch_info)::out, list(mismatch_info)::out) is det.

find_mismatched_args([], _, _, [], [], []).
find_mismatched_args([Arg - ExpType | ArgExpTypes], TypeAssignSet, ArgNum0,
        !:SimpleMismatches, !:ComplexMismatches, !:AllMismatches) :-
    ArgNum1 = ArgNum0 + 1,
    find_mismatched_args(ArgExpTypes, TypeAssignSet, ArgNum1,
        !:SimpleMismatches, !:ComplexMismatches, !:AllMismatches),
    get_type_stuff(TypeAssignSet, Arg, TypeStuffList),
    list.filter_map(substitute_types_check_match(ExpType), TypeStuffList,
        TypeMismatches0),
    list.sort_and_remove_dups(TypeMismatches0, TypeMismatches),
    (
        TypeMismatches = []
    ;
        TypeMismatches = [_],
        Mismatch = mismatch_info(ArgNum0, Arg, TypeMismatches),
        !:SimpleMismatches = [Mismatch | !.SimpleMismatches],
        !:AllMismatches = [Mismatch | !.AllMismatches]
    ;
        TypeMismatches = [_, _ | _],
        Mismatch = mismatch_info(ArgNum0, Arg, TypeMismatches),
        !:ComplexMismatches = [Mismatch | !.ComplexMismatches],
        !:AllMismatches = [Mismatch | !.AllMismatches]
    ).

:- pred substitute_types_check_match(mer_type::in, type_stuff::in,
    type_mismatch::out) is semidet.

substitute_types_check_match(ExpType, TypeStuff, TypeMismatch) :-
    TypeStuff = type_stuff(ArgType, TVarSet, TypeBindings, HeadTypeParams),
    apply_rec_subst_to_type(TypeBindings, ArgType, FullArgType),
    apply_rec_subst_to_type(TypeBindings, ExpType, FullExpType),
    (
        (
            % There is no mismatch if the actual type of the argument
            % is the same as the expected type.
            identical_types(FullArgType, FullExpType)
        ;
            % There is no mismatch if the actual type of the argument
            % has no constraints on it.
            FullArgType = defined_type(unqualified("<any>"), [], _)
        )
    ->
        fail
    ;
        ActualPieces = type_to_pieces(FullArgType, TVarSet, HeadTypeParams),
        ExpectedPieces = type_to_pieces(FullExpType, TVarSet, HeadTypeParams),
        TypeMismatch = type_mismatch(ActualPieces, ExpectedPieces)
    ).

:- func mismatched_args_to_pieces(list(mismatch_info), bool, prog_varset,
    cons_id) = list(format_component).

mismatched_args_to_pieces([], _, _, _) = [].
mismatched_args_to_pieces([Mismatch | Mismatches], First, VarSet, Functor)
        = Pieces :-
    Mismatch = mismatch_info(ArgNum, Var, TypeMismatches),
    ( TypeMismatches = [TypeMismatch] ->
        TypeMismatch = type_mismatch(ActTypePieces, ExpTypePieces)
    ;
        unexpected($module, $pred, "more than one type mismatch")
    ),
    (
        % Handle higher-order syntax such as ''(F, A) specially:
        % output
        %   Functor (F) has type ...;
        %   argument 1 (A) has type ...
        % instead of
        %   Argument 1 (F) has type ...;
        %   argument 2 (A) has type ...
        Functor = cons(unqualified(""), Arity, _),
        Arity > 0
    ->
        (
            First = yes,
            Pieces1 = [fixed("Functor")]
        ;
            First = no,
            Pieces1 = [fixed("argument"), int_fixed(ArgNum - 1)]
        )
    ;
        (
            First = yes,
            Pieces1 = [fixed("Argument"), int_fixed(ArgNum)]
        ;
            First = no,
            Pieces1 = [fixed("argument"), int_fixed(ArgNum)]
        )
    ),
    ( varset.search_name(VarSet, Var, _) ->
        Pieces2 = [prefix("("),
            words(mercury_var_to_string(VarSet, no, Var)),
            suffix(")")]
    ;
        Pieces2 = []
    ),
    Pieces3 = [words("has type"), prefix("`")] ++
        ActTypePieces ++
        [suffix("'"), suffix(","), nl] ++
        [words("expected type was"), prefix("`")] ++
        ExpTypePieces ++
        [suffix("'")],
    (
        Mismatches = [],
        Pieces4 = [suffix("."), nl]
    ;
        Mismatches = [_ | _],
        Pieces4 = [suffix(";"), nl] ++
            mismatched_args_to_pieces(Mismatches, no, VarSet, Functor)
    ),
    Pieces = Pieces1 ++ Pieces2 ++ Pieces3 ++ Pieces4.

%-----------------------------------------------------------------------------%

report_error_var(Info, Var, Type, TypeAssignSet0) = Spec :-
    typecheck_info_get_pred_markers(Info, PredMarkers),
    typecheck_info_get_called_predid(Info, CalledPredId),
    ArgNum = Info ^ tc_info_arg_num,
    Context = Info ^ tc_info_context,
    UnifyContext = Info ^ tc_info_unify_context,
    get_type_stuff(TypeAssignSet0, Var, TypeStuffList),
    typecheck_info_get_varset(Info, VarSet),

    InClauseForPieces = in_clause_for_pieces(Info),
    CallContextPieces = call_context_to_pieces(PredMarkers, CalledPredId,
        ArgNum, UnifyContext),

    ActualExpectedList0 = list.map(type_stuff_to_actual_expected(Type),
        TypeStuffList),
    list.sort_and_remove_dups(ActualExpectedList0, ActualExpectedList),

    Pieces1 = [words("type error:")],
    ( ActualExpectedList = [ActualExpected] ->
        ActualExpected = actual_expected_types(ActualPieces, ExpectedPieces),
        Pieces2 = argument_name_to_pieces(VarSet, Var) ++
            [words("has type"), prefix("`")] ++ ActualPieces ++
            [suffix("'"), suffix(","), nl,
            words("expected type was"), prefix("`")] ++ ExpectedPieces ++
            [suffix("'"), suffix("."), nl]
    ;
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
        [always(InClauseForPieces ++ CallContextPieces),
        always(Pieces1 ++ Pieces2),
        verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

report_error_var_either_type(Info, Var, TypeA, TypeB, TypeAssignSet0) = Spec :-
    typecheck_info_get_pred_markers(Info, PredMarkers),
    typecheck_info_get_called_predid(Info, CalledPredId),
    ArgNum = Info ^ tc_info_arg_num,
    Context = Info ^ tc_info_context,
    UnifyContext = Info ^ tc_info_unify_context,
    get_type_stuff(TypeAssignSet0, Var, TypeStuffList),
    typecheck_info_get_varset(Info, VarSet),

    InClauseForPieces = in_clause_for_pieces(Info),
    CallContextPieces = call_context_to_pieces(PredMarkers, CalledPredId,
        ArgNum, UnifyContext),

    ActualExpectedListA0 = list.map(type_stuff_to_actual_expected(TypeA),
        TypeStuffList),
    ActualExpectedListB0 = list.map(type_stuff_to_actual_expected(TypeB),
        TypeStuffList),
    list.sort_and_remove_dups(ActualExpectedListA0, ActualExpectedListA),
    list.sort_and_remove_dups(ActualExpectedListB0, ActualExpectedListB),

    Pieces1 = [words("type error:")],
    (
        ActualExpectedListA = [ActualExpectedA],
        ActualExpectedListB = [ActualExpectedB]
    ->
        ActualExpectedA = actual_expected_types(ActualPieces, ExpectedPiecesA),
        ActualExpectedB = actual_expected_types(_, ExpectedPiecesB),
        Pieces2 = argument_name_to_pieces(VarSet, Var) ++
            [words("has type"), prefix("`")] ++ ActualPieces ++
            [suffix("'"), suffix(","), nl,
            words("expected type was either"), prefix("`")] ++
            ExpectedPiecesA ++ [suffix("'"), words("or"), prefix("`")] ++
            ExpectedPiecesB ++ [suffix("'"), suffix("."), nl]
    ;
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
        [always(InClauseForPieces ++ CallContextPieces),
        always(Pieces1 ++ Pieces2),
        verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

report_error_arg_var(Info, Var, ArgTypeAssignSet0) = Spec :-
    typecheck_info_get_pred_markers(Info, PredMarkers),
    typecheck_info_get_called_predid(Info, CalledPredId),
    ArgNum = Info ^ tc_info_arg_num,
    Context = Info ^ tc_info_context,
    UnifyContext = Info ^ tc_info_unify_context,
    get_arg_type_stuff(ArgTypeAssignSet0, Var, ArgTypeStuffList),
    typecheck_info_get_varset(Info, VarSet),

    InClauseForPieces = in_clause_for_pieces(Info),
    CallContextPieces = call_context_to_pieces(PredMarkers, CalledPredId,
        ArgNum, UnifyContext),

    ActualExpectedList0 = list.map(arg_type_stuff_to_actual_expected,
        ArgTypeStuffList),
    list.sort_and_remove_dups(ActualExpectedList0, ActualExpectedList),

    Pieces1 = [words("type error:")],
    ( ActualExpectedList = [ActualExpected] ->
        ActualExpected = actual_expected_types(ActualPieces, ExpectedPieces),
        Pieces2 = argument_name_to_pieces(VarSet, Var) ++
            [words("has type"), prefix("`")] ++ ActualPieces ++
            [suffix("'"), suffix(","), nl,
            words("expected type was"), prefix("`")] ++ ExpectedPieces ++
            [suffix("'"), suffix("."), nl]
    ;
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
        [always(InClauseForPieces ++ CallContextPieces),
        always(Pieces1 ++ Pieces2),
        verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

report_error_undef_cons(Info, ConsErrors, Functor, Arity) = Spec :-
    typecheck_info_get_pred_markers(Info, PredMarkers),
    typecheck_info_get_called_predid(Info, CalledPredId),
    ArgNum = Info ^ tc_info_arg_num,
    Context = Info ^ tc_info_context,
    UnifyContext = Info ^ tc_info_unify_context,
    InClauseForPieces = in_clause_for_pieces(Info),
    CallContextPieces = call_context_to_pieces(PredMarkers, CalledPredId,
        ArgNum, UnifyContext),
    InitComp = always(InClauseForPieces ++ CallContextPieces),

    % Check for some special cases, so that we can give clearer error messages.
    (
        Functor = cons(unqualified(FunctorName), FunctorArity, _),
        expect(unify(Arity, FunctorArity), $module, $pred, "arity mismatch"),
        (
            language_builtin_functor_components(FunctorName, Arity,
                FunctorComps0)
        ->
            FunctorComps1 = FunctorComps0
        ;
            syntax_functor_components(FunctorName, FunctorArity, FunctorComps0)
        ->
            FunctorComps1 = FunctorComps0
        ;
            fail
        )
    ->
        FunctorComps = FunctorComps1,
        ReportConsErrors = no
    ;
        Functor = cons(Constructor, FunctorArity, _),
        expect(unify(Arity, FunctorArity), $module, $pred, "arity mismatch"),
        typecheck_info_get_ctors(Info, ConsTable),
        return_other_arities(ConsTable, Constructor, Arity, OtherArities),
        OtherArities = [_ | _]
    ->
        FunctorPieces = wrong_arity_constructor_to_pieces(Constructor, Arity,
            OtherArities),
        FunctorComps = [always(FunctorPieces)],
        ReportConsErrors = yes
    ;
        strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor),
        Pieces1 = [words("error: undefined symbol"),
            quote(cons_id_and_arity_to_string(StrippedFunctor))],
        (
            Functor = cons(Constructor, _, _),
            Constructor = qualified(ModQual, _)
        ->
            Pieces2 = maybe_report_missing_import_addendum(Info, ModQual)
        ;
            Functor = cons(unqualified("[|]"), 2, _)
        ->
            Pieces2 = maybe_report_missing_import_addendum(Info,
                unqualified("list"))
        ;
            Pieces2 = [suffix("."), nl]
        ),
        FunctorComps = [always(Pieces1 ++ Pieces2)],
        ReportConsErrors = yes
    ),
    (
        ReportConsErrors = yes,
        ConsErrors = [_ | _]
    ->
        ConsMsgLists = list.map(report_cons_error(Context), ConsErrors),
        list.condense(ConsMsgLists, ConsMsgs)
    ;
        ConsMsgs = []
    ),
    Spec = error_spec(severity_error, phase_type_check,
        [simple_msg(Context, [InitComp | FunctorComps]) | ConsMsgs]).

:- pred language_builtin_functor_components(string::in, arity::in,
    list(error_msg_component)::out) is semidet.

language_builtin_functor_components(Name, Arity, Components) :-
    language_builtin_functor(Name, Arity),
    MainPieces = [words("error: the language construct"),
        sym_name_and_arity(unqualified(Name) / Arity),
        words("should be used as a goal, not as an expression."), nl],
    VerbosePieces = [words("If you are trying to use a goal"),
        words("as a boolean function, you should write"),
        words("`if <goal> then yes else no' instead."), nl],
    ( Name = "call" ->
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
    ;
        VerboseCallPieces = []
    ),
    Components = [always(MainPieces),
        verbose_only(VerbosePieces ++ VerboseCallPieces)].

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
    Components = [always(Pieces1), verbose_only(Pieces2)].
syntax_functor_components("->", 2, Components) :-
    Pieces1 = [words("error:"), quote("->"), words("without"),
        quote(";"), suffix("."), nl],
    Pieces2 = [words("Note: the else part is not optional."),
        words("Every if-then must have an else."), nl],
    Components = [always(Pieces1), verbose_only(Pieces2)].
syntax_functor_components("^", 2, Components) :-
    Pieces1 = [words("error: invalid use of field selection operator"),
        prefix("("), quote("^"), suffix(")."), nl],
    Pieces2 = [words("This is probably some kind of syntax error."),
        words("The field name must be an atom,"),
        words("not a variable or other term."), nl],
    Components = [always(Pieces1), verbose_only(Pieces2)].
syntax_functor_components(":=", 2, Components) :-
    Pieces1 = [words("error: invalid use of field update operator"),
        prefix("("), quote(":="), suffix(")."), nl],
    Pieces2 = [words("This is probably some kind of syntax error."), nl],
    Components = [always(Pieces1), verbose_only(Pieces2)].
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
        sym_name_and_arity(unqualified("[|]") / 2),
        suffix(","), words("not"), quote("./2"),
        suffix("."), nl],
    Components = [always(Pieces)].
syntax_functor_components("!", 1, Components) :-
    Pieces1 = [words("error: invalid use of"), quote("!"),
        words("state variable operator."), nl],
    Pieces2 = [words("You probably meant to use"), quote("!."),
        words("or"), quote("!:"), suffix("."), nl],
    Components = [always(Pieces1), verbose_only(Pieces2)].

:- func wrong_arity_constructor_to_pieces(sym_name, arity, list(int))
    = list(format_component).

wrong_arity_constructor_to_pieces(Name, Arity, ActualArities) = Pieces :-
    MaybePredOrFunc = no,
    NumArgsPieces = error_num_args_to_pieces(MaybePredOrFunc, Arity,
        ActualArities),
    Pieces = [words("error: ")] ++ NumArgsPieces ++
        [words("in use of constructor"), sym_name(Name), suffix(".")].

:- func report_cons_error(prog_context, cons_error) = list(error_msg).

report_cons_error(Context, ConsError) = Msgs :-
    (
        ConsError = foreign_type_constructor(TypeCtor, _),
        TypeCtor = type_ctor(TypeName, TypeArity),
        Pieces = [words("There are"),
            pragma_decl("foreign_type"),
            words("declarations for type"),
            sym_name_and_arity(TypeName / TypeArity),
            suffix(","),
            words("so it is treated as an abstract type"),
            words("in all predicates and functions"),
            words("which are not implemented"),
            words("for those foreign types.")],
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
        Pieces1 = [words("Field"), sym_name(FieldName),
            words("cannot be updated because"),
            words("the existentially quantified type")],
        (
            TVars = [],
            unexpected($module, $pred, "no type variables")
        ;
            TVars = [TVar],
            TVarsStr = mercury_var_to_string(TVarSet, no, TVar),
            Pieces2 = [words("variable"), quote(TVarsStr), words("occurs")]
        ;
            TVars = [_, _ | _],
            TVarsStr = mercury_vars_to_string(TVarSet, no, TVars),
            Pieces2 = [words("variables"), quote(TVarsStr), words("occur")]
        ),
        ConsIdStr = cons_id_and_arity_to_string(ConsId),
        Pieces3 = [words("in the types of field"), sym_name(FieldName),
            words("and some other field"),
            words("in definition of constructor"),
            quote(ConsIdStr), suffix(".")],
        Pieces = Pieces1 ++ Pieces2 ++ Pieces3,
        Msgs = [simple_msg(DefnContext, [always(Pieces)])]
    ;
        ConsError = new_on_non_existential_type(TypeCtor),
        TypeCtor = type_ctor(TypeName, TypeArity),
        Pieces = [words("Invalid use of"), quote("new"),
            words("on a constructor of type"),
            sym_name_and_arity(TypeName / TypeArity),
            words("which is not existentially typed.")],
        Msgs = [simple_msg(Context, [always(Pieces)])]
    ).

%-----------------------------------------------------------------------------%

report_ambiguity_error(Info, TypeAssign1, TypeAssign2) = Spec :-
    InClauseForPieces = in_clause_for_pieces(Info),
    Pieces1 =
        [words("error: ambiguous overloading causes type ambiguity."), nl],
    typecheck_info_get_varset(Info, VarSet),
    type_assign_get_var_types(TypeAssign1, VarTypes1),
    vartypes_vars(VarTypes1, Vars1),
    AmbiguityPieces = ambiguity_error_possibilities_to_pieces(Vars1, VarSet,
        TypeAssign1, TypeAssign2),
    (
        AmbiguityPieces = [],
        Pieces2 = [],
        VerboseComponents = [],
        WarningMsgs = too_much_overloading_to_msgs(Info, no)
    ;
        AmbiguityPieces = [_ | _],
        Pieces2 = [words("Possible type assignments include:"), nl
            | AmbiguityPieces],
        VerboseComponents = [verbose_only(add_qualifiers_reminder)],
        WarningMsgs = []
    ),

    Context = Info ^ tc_info_context,
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
    type_assign_get_head_type_params(TypeAssign1, HeadTypeParams1),
    type_assign_get_head_type_params(TypeAssign2, HeadTypeParams2),
    (
        search_var_type(VarTypes1, Var, Type1),
        search_var_type(VarTypes2, Var, Type2),
        apply_rec_subst_to_type(TypeBindings1, Type1, T1),
        apply_rec_subst_to_type(TypeBindings2, Type2, T2),
        \+ identical_types(T1, T2)
    ->
        type_assign_get_typevarset(TypeAssign1, TVarSet1),
        type_assign_get_typevarset(TypeAssign2, TVarSet2),
        HeadPieces =
            [words(mercury_var_to_string(VarSet, no, Var)), suffix(":")] ++
            type_to_pieces(T1, TVarSet1, HeadTypeParams1) ++ [words("or")] ++
            type_to_pieces(T2, TVarSet2, HeadTypeParams2) ++ [nl]
    ;
        HeadPieces = []
    ),
    TailPieces = ambiguity_error_possibilities_to_pieces(Vars, VarSet,
        TypeAssign1, TypeAssign2),
    Pieces = HeadPieces ++ TailPieces.

%-----------------------------------------------------------------------------%

report_unsatisfiable_constraints(Info, TypeAssignSet) = Spec :-
    InClauseForPieces = in_clause_for_pieces(Info),
    list.map_foldl(constraints_to_pieces, TypeAssignSet, ConstraintPieceLists,
        0, NumUnsatisfied),
    ( NumUnsatisfied = 1 ->
        Pieces1 = [words("unsatisfiable typeclass constraint:"), nl]
    ;
        Pieces1 = [words("unsatisfiable typeclass constraints:"), nl]
    ),
    % XXX this won't be very pretty when there are multiple type_assigns.
    Pieces2 = component_list_to_line_pieces(ConstraintPieceLists,
        [suffix(".")]),

    Context = Info ^ tc_info_context,
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
    Pieces = component_list_to_pieces(UnprovenProgConstraintsPieces).

:- func wrap_quote(string) = format_component.

wrap_quote(Str) = quote(Str).

%-----------------------------------------------------------------------------%

report_missing_tvar_in_foreign_code(Info, VarName) = Spec :-
    ModuleInfo = Info ^ tc_info_module_info,
    Context = Info ^ tc_info_context,
    typecheck_info_get_predid(Info, PredId),
    Pieces = [words("The foreign language code for") |
        describe_one_pred_name(ModuleInfo, should_module_qualify, PredId)] ++
        [words("should define the variable"), quote(VarName), suffix(".")],
    Spec = error_spec(severity_error, phase_type_check,
        [simple_msg(Context, [always(Pieces)])]).

%-----------------------------------------------------------------------------%

:- func types_of_vars_to_pieces(list(prog_var), prog_varset,
    typecheck_info, type_assign_set) = list(format_component).

types_of_vars_to_pieces([], _, _, _) = [suffix("."), nl].
types_of_vars_to_pieces([Var | Vars], VarSet, Info, TypeAssignSet) =
    [suffix(","), nl] ++
    argument_name_to_pieces(VarSet, Var) ++
    type_of_var_to_pieces(TypeAssignSet, Var) ++
    types_of_vars_to_pieces(Vars, VarSet, Info, TypeAssignSet).

:- func argument_name_to_pieces(prog_varset, prog_var)
    = list(format_component).

argument_name_to_pieces(VarSet, Var) = Pieces :-
    ( varset.search_name(VarSet, Var, _) ->
        Pieces = [words("variable"),
            quote(mercury_var_to_string(VarSet, no, Var))]
    ;
        Pieces = [words("argument")]
    ).

:- func functor_name_to_pieces(cons_id, arity) = list(format_component).

functor_name_to_pieces(Functor, Arity) = Pieces :-
    strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor),
    ( Arity = 0 ->
        Piece1 = words("constant"),
        ( Functor = cons(Name, _, _) ->
            Piece2 = sym_name(Name)
        ;
            Piece2 = quote(cons_id_and_arity_to_string(StrippedFunctor))
        ),
        Pieces = [Piece1, Piece2]
    ; Functor = cons(unqualified(""), _, _) ->
        Pieces = [words("higher-order term (with arity"),
            int_fixed(Arity - 1), suffix(")")]
    ;
        Pieces = [words("functor"),
            quote(cons_id_and_arity_to_string(StrippedFunctor))]
    ).

:- func type_of_var_to_pieces(type_assign_set, prog_var)
    = list(format_component).

type_of_var_to_pieces(TypeAssignSet, Var) = Pieces :-
    get_type_stuff(TypeAssignSet, Var, TypeStuffList),
    TypeStrs0 = list.map(typestuff_to_typestr, TypeStuffList),
    list.sort_and_remove_dups(TypeStrs0, TypeStrs),
    ( TypeStrs = [TypeStr] ->
        Pieces = [words("has type"), words(add_quotes(TypeStr))]
    ;
        Pieces = [words("has overloaded type {"), nl_indent_delta(2)] ++
            types_list_to_pieces(TypeStrs) ++ [nl_indent_delta(-2), words("}")]
    ).

:- func type_of_functor_to_pieces(cons_id, int, list(cons_type_info))
    = list(format_component).

type_of_functor_to_pieces(Functor, Arity, ConsDefnList) = Pieces :-
    ( ConsDefnList = [SingleDefn] ->
        ( Arity \= 0 ->
            SepPieces = [nl]
        ;
            SepPieces = []
        ),
        ConsTypePieces = cons_type_to_pieces(SingleDefn, Functor),
        Pieces = [words("has type")] ++ SepPieces ++
            [prefix("`")] ++ ConsTypePieces ++ [suffix("'")]
    ;
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
        ( Functor = cons(SymName, _Arity, _) ->
            % What we construct in Type is not really a type: it is a
            % function symbol applied to a list of argument types. However
            % *syntactically*, it looks like a type, and we already have
            % code to print types, so we take a shortcut.
            Type = defined_type(SymName, ArgTypes, kind_star),
            ArgPieces = type_to_pieces(Type, TVarSet, ExistQVars) ++
                [suffix(":")]
        ;
            unexpected($module, $pred, "invalid cons_id")
        )
    ;
        ArgTypes = [],
        ArgPieces = []
    ),
    Pieces = ArgPieces ++ type_to_pieces(ConsType, TVarSet, ExistQVars).

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
        ( Arity = 0 ->
            ConnectPieces = [suffix(",")]
        ;
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
    ( TypeAssignSet0 = [_] ->
        FirstWords = "The partial type assignment was:",
        MaybeSeq = no
    ;
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
    ( ArgTypeAssignSet0 = [_] ->
        FirstWords = "The partial type assignment was:",
        MaybeSeq = no
    ;
        FirstWords = "The possible partial type assignments were:",
        MaybeSeq = yes(1)
    ),
    list.sort(ArgTypeAssignSet0, ArgTypeAssignSet),
    LaterPieces = args_type_assign_set_to_pieces(ArgTypeAssignSet, MaybeSeq,
        VarSet),
    Pieces = [words(FirstWords), nl_indent_delta(1) | LaterPieces] ++
        [nl_indent_delta(-1)].

:- func type_to_pieces(mer_type, tvarset, head_type_params)
    = list(format_component).

type_to_pieces(Type0, TVarSet, HeadTypeParams) = Pieces :-
    strip_builtin_qualifiers_from_type(Type0, Type),
    unparse_type(Type, Term0),
    list.map(term.coerce_var, HeadTypeParams, ExistQVars),
    maybe_add_existential_quantifier(ExistQVars, Term0, Term),
    varset.coerce(TVarSet, VarSet),
    Pieces = [words(mercury_term_to_string(VarSet, no, Term))].

    % Return a description of the given list of types.
    %
    % The caller should ensure that these pieces are indented one or two levels
    % to separate them from surrounding material.
    %
:- func types_list_to_pieces(list(string)) = list(format_component).

types_list_to_pieces(TypeStrs) =
    component_list_to_line_pieces(list.map(string_to_pieces, TypeStrs), []).

:- func string_to_pieces(string) = list(format_component).

string_to_pieces(Str) = [words(Str)].

:- type actual_expected_types
    --->    actual_expected_types(
                actual_type     :: list(format_component),
                expected_type   :: list(format_component)
            ).

:- func type_stuff_to_actual_expected(mer_type, type_stuff)
    = actual_expected_types.

type_stuff_to_actual_expected(Type, VarTypeStuff) = ActualExpected :-
    VarTypeStuff = type_stuff(VarType, TVarSet, TypeBinding, HeadTypeParams),
    ActualPieces =
        bound_type_to_pieces(VarType, TVarSet, TypeBinding, HeadTypeParams),
    ExpectedPieces =
        bound_type_to_pieces(Type, TVarSet, TypeBinding, HeadTypeParams),
    ActualExpected = actual_expected_types(ActualPieces, ExpectedPieces).

:- func arg_type_stuff_to_actual_expected(arg_type_stuff) =
    actual_expected_types.

arg_type_stuff_to_actual_expected(ArgTypeStuff) = ActualExpected :-
    ArgTypeStuff = arg_type_stuff(Type, VarType, TVarSet, HeadTypeParams),
    ActualPieces = type_to_pieces(VarType, TVarSet, HeadTypeParams),
    ExpectedPieces = type_to_pieces(Type, TVarSet, HeadTypeParams),
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
    ( list.member(TaggedPieces, Pieces0) ->
        Pieces = Pieces0
    ;
        Pieces = Pieces0 ++ [TaggedPieces]
    ).

:- func actual_types_to_pieces(actual_expected_types) = list(format_component).

actual_types_to_pieces(ActualExpected) = Pieces :-
    ActualExpected = actual_expected_types(ActualPieces, _ExpectedPieces),
    Pieces = [words("(inferred)") | ActualPieces].

:- func bound_type_to_pieces(mer_type, tvarset, tsubst, head_type_params)
    = list(format_component).

bound_type_to_pieces(Type0, TypeVarSet, TypeBindings, HeadTypeParams)
        = Pieces :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type),
    Pieces = type_to_pieces(Type, TypeVarSet, HeadTypeParams).

%-----------------------------------------------------------------------------%

:- func maybe_report_missing_import_addendum(typecheck_info, module_specifier)
    = list(format_component).

maybe_report_missing_import_addendum(Info, ModuleQualifier) = Pieces :-
    % First check if this module wasn't imported.
    ModuleInfo = Info ^ tc_info_module_info,
    (
        % If the module qualifier couldn't match any of the visible modules,
        % then we report that the module has not been imported.
        \+ (
            visible_module(VisibleModule, ModuleInfo),
            match_sym_name(ModuleQualifier, VisibleModule)
        )
    ->
        Pieces = [nl, words("(the module"), sym_name(ModuleQualifier),
            words("has not been imported)."), nl]
    ;
        % The module qualifier matches one or more of the visible modules.
        % But maybe the user forgot to import the parent module(s) of that
        % module...
        solutions.solutions(get_unimported_parent(ModuleQualifier,
            ModuleInfo), UnimportedParents),
        UnimportedParents = [_ | _]
    ->
        Pieces = [nl | report_unimported_parents(UnimportedParents)]
    ;
        Pieces = [suffix("."), nl]
    ).

    % Nondeterministically return all the possible parent modules which could
    % be parent modules of the given module qualifier, and which are not
    % imported.
    %
:- pred get_unimported_parent(module_name::in, module_info::in,
    module_name::out) is nondet.

get_unimported_parent(ModuleQualifier, ModuleInfo, UnimportedParent) :-
    visible_module(ModuleName, ModuleInfo),
    match_sym_name(ModuleQualifier, ModuleName),
    ParentModules = get_ancestors(ModuleName),
    list.member(UnimportedParent, ParentModules),
    \+ visible_module(UnimportedParent, ModuleInfo).

:- func report_unimported_parents(list(module_name)) = list(format_component).

report_unimported_parents(UnimportedParents) = Pieces :-
    UnimportedParentDescs = list.map(describe_sym_name, UnimportedParents),
    AllUnimportedParents = list_to_pieces(UnimportedParentDescs),
    ( AllUnimportedParents = [_] ->
        Pieces = [words("(the possible parent module")]
            ++ AllUnimportedParents ++ [words("has not been imported).")]
    ;
        Pieces = [words("(the possible parent modules")]
            ++ AllUnimportedParents ++ [words("have not been imported).")]
    ).

%-----------------------------------------------------------------------------%

:- func call_context_to_pieces(pred_markers, call_id, int, unify_context)
    = list(format_component).

call_context_to_pieces(PredMarkers, CallId, ArgNum, UnifyContext) = Pieces :-
    ( ArgNum = 0 ->
        unify_context_to_pieces(UnifyContext, [], Pieces)
    ;
        Pieces = [words("in"),
            words(call_arg_id_to_string(CallId, ArgNum, PredMarkers)),
            suffix(":"), nl]
    ).

    % This function generates the preamble (initial part of) all type error
    % messages, giving the name of the predicate or function in which the error
    % occurred.
    %
:- func in_clause_for_pieces(typecheck_info) = list(format_component).

in_clause_for_pieces(Info) = Pieces :-
    ModuleInfo = Info ^ tc_info_module_info,
    typecheck_info_get_predid(Info, PredId),
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
    ( MaybePredOrFunc = yes(pf_function) ->
        adjust_func_arity(pf_function, Arity, Arity0),
        ReverseAdjust =
            ( pred(OtherArity0::in, OtherArity::out) is det :-
                adjust_func_arity(pf_function, OtherArity, OtherArity0)
            ),
        list.map(ReverseAdjust, Arities0, Arities)
    ;
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

    % Check whether two types are identical, i.e. whether they can be unified
    % without binding any type parameters.
    %
:- pred identical_types(mer_type::in, mer_type::in) is semidet.

identical_types(Type1, Type2) :-
    map.init(TypeSubst0),
    type_unify(Type1, Type2, [], TypeSubst0, TypeSubst),
    TypeSubst = TypeSubst0.

%-----------------------------------------------------------------------------%

:- type type_stuff
    --->    type_stuff(
                type_stuff_base_type        :: mer_type,
                type_stuff_tvarset          :: tvarset,
                type_stuff_binding          :: tsubst,
                type_stuff_head_type_params :: head_type_params
            ).

    % Given a type assignment set and a variable, return the list of possible
    % different types for the variable.
    %
:- pred get_type_stuff(type_assign_set::in, prog_var::in,
    list(type_stuff)::out) is det.

get_type_stuff([], _Var, []).
get_type_stuff([TypeAssign | TypeAssigns], Var, TypeStuffs) :-
    get_type_stuff(TypeAssigns, Var, TailTypeStuffs),
    type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TVarSet),
    type_assign_get_var_types(TypeAssign, VarTypes),
    ( search_var_type(VarTypes, Var, Type0) ->
        Type = Type0
    ;
        % This shouldn't happen - how can a variable which has not yet been
        % assigned a type variable fail to have the correct type?
        Type = defined_type(unqualified("<any>"), [], kind_star)
    ),
    TypeStuff = type_stuff(Type, TVarSet, TypeBindings, HeadTypeParams),
    ( list.member(TypeStuff, TailTypeStuffs) ->
        TypeStuffs = TailTypeStuffs
    ;
        TypeStuffs = [TypeStuff | TailTypeStuffs]
    ).

:- func typestuff_to_typestr(type_stuff) = string.

typestuff_to_typestr(TypeStuff) = TypeStr :-
    TypeStuff = type_stuff(Type0, TypeVarSet, TypeBindings, HeadTypeParams),
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_builtin_qualifiers_from_type(Type1, Type),
    unparse_type(Type, Term0),
    list.map(term.coerce_var, HeadTypeParams, ExistQVars),
    maybe_add_existential_quantifier(ExistQVars, Term0, Term),
    varset.coerce(TypeVarSet, VarSet),
    TypeStr = mercury_term_to_string(VarSet, no, Term).

:- type arg_type_stuff
    --->    arg_type_stuff(
                arg_type_stuff_arg_type         :: mer_type,
                arg_type_stuff_var_type         :: mer_type,
                arg_type_stuff_tvarset          :: tvarset,
                arg_type_stuff_head_type_params :: head_type_params
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
    type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TVarSet),
    type_assign_get_var_types(TypeAssign, VarTypes),
    ( search_var_type(VarTypes, Var, VarType0) ->
        VarType = VarType0
    ;
        % This shouldn't happen - how can a variable which has
        % not yet been assigned a type variable fail to have
        % the correct type?
        VarType = defined_type(unqualified("<any>"), [], kind_star)
    ),
    list.det_index0(ArgTypes, 0, ArgType),
    apply_rec_subst_to_type(TypeBindings, ArgType, ArgType2),
    apply_rec_subst_to_type(TypeBindings, VarType, VarType2),
    ArgTypeStuff = arg_type_stuff(ArgType2, VarType2, TVarSet, HeadTypeParams),
    ( list.member(ArgTypeStuff, TailArgTypeStuffs) ->
        ArgTypeStuffs = TailArgTypeStuffs
    ;
        ArgTypeStuffs = [ArgTypeStuff | TailArgTypeStuffs]
    ).

    % Check if any of the variables in the term are existentially
    % quantified (occur in the first argument), and if so, add the
    % appropriate quantification to the term.  Otherwise return the term
    % unchanged.
    %
:- pred maybe_add_existential_quantifier(list(var)::in, term::in, term::out)
    is det.

maybe_add_existential_quantifier(HeadTypeParams, !Term) :-
    term.vars(!.Term, Vars),
    ExistQVars = set.to_sorted_list(set.intersect(
        set.list_to_set(HeadTypeParams), set.list_to_set(Vars))),
    (
        ExistQVars = []
    ;
        ExistQVars = [_ | _],
        QTerm = make_list_term(ExistQVars),
        !:Term = term.functor(term.atom("some"), [QTerm, !.Term],
            term.context_init)
    ).

:- func make_list_term(list(var)) = term.

make_list_term([]) = term.functor(term.atom("[]"), [], term.context_init).
make_list_term([Var | Vars]) = term.functor(term.atom("[|]"),
    [term.variable(Var, context_init), make_list_term(Vars)],
    term.context_init).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_errors.
%-----------------------------------------------------------------------------%
