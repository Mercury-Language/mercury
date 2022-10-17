%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_error_overload.m.
% Main author: fjh.
%
% This file contains predicates to report errors involving overloading.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_error_overload.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_info.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- func report_warning_too_much_overloading(type_error_clause_context,
    prog_context, overloaded_symbol_map) = error_spec.

:- func report_error_too_much_overloading(type_error_clause_context,
    prog_context, overloaded_symbol_map) = error_spec.

%---------------------------------------------------------------------------%

:- func report_ambiguity_error(type_error_clause_context, prog_context,
    overloaded_symbol_map, type_assign, type_assign) = error_spec.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.typecheck_error_util.
:- import_module hlds.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_type_util.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.vartypes.

:- import_module assoc_list.
:- import_module bool.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module term.
:- import_module varset.

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

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_error_overload.
%---------------------------------------------------------------------------%
