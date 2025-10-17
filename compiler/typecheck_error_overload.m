%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% Copyright (C) 2022-2025 The Mercury team.
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
    overloaded_symbol_map, type_assign, type_assign, list(type_assign))
    = error_spec.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.typecheck_error_util.
:- import_module hlds.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_type_util.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.vartypes.

:- import_module assoc_list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

report_warning_too_much_overloading(ClauseContext, Context,
        OverloadedSymbolMap) = Spec :-
    InitPieces = [words("warning: highly ambiguous overloading."), nl],
    VerbosePieces =
        [words("This may cause type-checking to be very slow."),
        words("It may also make your code difficult to understand."), nl],
    FirstMsg = create_first_msg(ClauseContext, Context,
        InitPieces, VerbosePieces),
    LaterMsgs = describe_overloaded_symbols(ClauseContext, Context,
        OverloadedSymbolMap),
    Spec = error_spec($pred, severity_warning(warn_typecheck_ambiguity_limit),
        phase_type_check, [FirstMsg | LaterMsgs]).

report_error_too_much_overloading(ClauseContext, Context,
        OverloadedSymbolMap) = Spec :-
    InitPieces = [words("error: excessively ambiguous overloading."), nl],
    VerbosePieces =
        [words("This caused the type checker to exceed its limits."),
        words("It may also make your code difficult to understand."), nl],
    FirstMsg = create_first_msg(ClauseContext, Context,
        InitPieces, VerbosePieces),
    LaterMsgs = describe_overloaded_symbols(ClauseContext, Context,
        OverloadedSymbolMap),
    Spec = error_spec($pred, severity_error,
        phase_type_check, [FirstMsg | LaterMsgs]).

%---------------------------------------------------------------------------%

:- func create_first_msg(type_error_clause_context, prog_context,
    list(format_piece), list(format_piece)) = error_msg.

create_first_msg(ClauseContext, Context, InitPieces,  VerbosePieces)
        = FirstMsg :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    InitComponent = always(InClauseForPieces ++ InitPieces),
    VerboseComponent = verbose_only(verbose_always, VerbosePieces),
    FirstMsg = simple_msg(Context, [InitComponent, VerboseComponent]).

%---------------------------------------------------------------------------%

report_ambiguity_error(ClauseContext, Context, OverloadedSymbolMap,
        TypeAssign1, TypeAssign2, TypeAssigns3plus) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    InitPieces = [words("error: unresolved type ambiguity."), nl],
    FirstMsg = simple_msg(Context, [always(InClauseForPieces ++ InitPieces)]),

    VarSet = ClauseContext ^ tecc_varset,
    get_inst_varset(ClauseContext, InstVarSet),
    type_assign_get_var_types(TypeAssign1, VarTypes1),
    vartypes_vars(VarTypes1, Vars1),
    AllTypeAssigns = [TypeAssign1, TypeAssign2 | TypeAssigns3plus],
    VarAssignPiecesList0 = list.map(
        var_ambiguity_to_pieces(VarSet, InstVarSet, AllTypeAssigns), Vars1),
    list.filter(list.is_non_empty, VarAssignPiecesList0, VarAssignPiecesList),
    (
        VarAssignPiecesList = [],
        LaterMsgs = describe_overloaded_symbols(ClauseContext, Context,
            OverloadedSymbolMap)
    ;
        VarAssignPiecesList = [_ | TailVarAssignPiecesList],
        list.condense(VarAssignPiecesList, VarAssignPieces),
        (
            TailVarAssignPiecesList = [],
            % We could have gere a singular version of the PreVarPieces in the
            % nonempty tail case, but that would be overkill. But we do want
            % to make clear that the entity we are reporting on is a variable,
            % which is why we have this prefix on the first and only
            % VarAssignPieceList.
            PreVarPieces = [words("The variable")]
        ;
            TailVarAssignPiecesList = [_ | _],
            PreVarPieces =
                [words("The following variables have ambiguous types."), nl]
        ),
        VarPieces = PreVarPieces ++ VarAssignPieces,
        VerboseComponent = verbose_only(verbose_once, add_qualifiers_reminder),
        VarMsg = simple_msg(Context, [always(VarPieces), VerboseComponent]),
        LaterMsgs = [VarMsg]
    ),
    Msgs = [FirstMsg | LaterMsgs],
    Spec = error_spec($pred, severity_error, phase_type_check, Msgs).

:- func var_ambiguity_to_pieces(prog_varset, inst_varset, list(type_assign),
    prog_var) = list(format_piece).

var_ambiguity_to_pieces(VarSet, InstVarSet, TypeAssigns, Var) = Pieces :-
    list.foldl2(gather_type_pieces_for_var_in_type_assign(InstVarSet, Var),
        TypeAssigns, set.init, NameOnlyPiecesSet, set.init, NameNumPiecesSet),
    NameNumPiecesList = set.to_sorted_list(NameNumPiecesSet),
    (
        NameNumPiecesList = [],
        % We have no info about Var (which I, zs, think should NOT happen).
        Pieces = []
    ;
        NameNumPiecesList = [_],
        % Var has an unambiguous type.
        Pieces = []
    ;
        NameNumPiecesList = [_, _ | _],
        % Var has an ambiguous type.
        NameOnlyPiecesList = set.to_sorted_list(NameOnlyPiecesSet),
        (
            NameOnlyPiecesList = [],
            % NameOnlyPiecesSet can be empty if and *only if*
            % NameNumPiecesSet is also empty.
            unexpected($pred, "NameOnlyPiecesList = []")
        ;
            NameOnlyPiecesList = [_],
            % Var has an ambiguous type, but this is apparent *only*
            % from NameNumPiecesList.
            PossibleTypePiecesList = NameNumPiecesList
        ;
            NameOnlyPiecesList = [_, _ | _],
            % Var has an ambiguous type, and this is apparent even from
            % just NameOnlyPiecesList.
            PossibleTypePiecesList = NameOnlyPiecesList
        ),
        VarPiece = var_to_quote_piece(VarSet, Var),
        ( if list.length(PossibleTypePiecesList) = 2 then
            EitherAny = "either"
        else
            EitherAny = "any"
        ),
        Pieces =
            % ZZZ [words("The variable")] ++
            color_as_subject([VarPiece]) ++
            [words("can have"), words(EitherAny),
            words("of the following types:"), nl_indent_delta(1)] ++
            pieces_list_to_color_line_pieces(color_hint, [suffix(".")],
                PossibleTypePiecesList) ++
            [nl_indent_delta(-1)]
    ).

:- pred gather_type_pieces_for_var_in_type_assign(inst_varset::in,
    prog_var::in, type_assign::in,
    set(list(format_piece))::in, set(list(format_piece))::out,
    set(list(format_piece))::in, set(list(format_piece))::out) is det.

gather_type_pieces_for_var_in_type_assign(InstVarSet, Var, TypeAssign,
        !NameOnlyPiecesSet, !NameNumPiecesSet) :-
    type_assign_get_var_types(TypeAssign, VarTypes),
    ( if search_var_type(VarTypes, Var, Type0) then
        type_assign_get_type_bindings(TypeAssign, TypeBindings),
        type_assign_get_existq_tvars(TypeAssign, ExistQTVars),
        type_assign_get_typevarset(TypeAssign, TVarSet),
        apply_rec_subst_to_type(TypeBindings, Type0, Type),
        NameOnlyPieces = type_to_pieces(TVarSet, InstVarSet,
            print_name_only, do_not_add_quotes, ExistQTVars, Type),
        NameNumPieces = type_to_pieces(TVarSet, InstVarSet,
            print_name_and_num, do_not_add_quotes, ExistQTVars, Type),
        set.insert(NameOnlyPieces, !NameOnlyPiecesSet),
        set.insert(NameNumPieces, !NameNumPiecesSet)
    else
        true
    ).

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

%---------------------------------------------------------------------------%

:- func describe_overloaded_symbols(type_error_clause_context, prog_context,
    overloaded_symbol_map) = list(error_msg).

describe_overloaded_symbols(ClauseContext, Context, OverloadedSymbolMap)
        = Msgs :-
    map.to_assoc_list(OverloadedSymbolMap, OverloadedSymbols),
    OverloadedSymbolsSortedContexts =
        assoc_list.map_values_only(sort_and_remove_dups, OverloadedSymbols),
    (
        OverloadedSymbolsSortedContexts = [],
        Msgs = []
    ;
        (
            OverloadedSymbolsSortedContexts = [_ - Contexts],
            (
                Contexts = [],
                unexpected($pred, "no contexts")
            ;
                Contexts = [_],
                WasOverloadedPieces =
                    [words("The following symbol was overloaded"),
                    words("in the following context."), nl]
            ;
                Contexts = [_, _ | _],
                WasOverloadedPieces =
                    [words("The following symbol was overloaded"),
                    words("in the following contexts."), nl]
            )
        ;
            OverloadedSymbolsSortedContexts = [_, _ | _],
            WasOverloadedPieces =
                [words("The following symbols were overloaded"),
                words("in the following contexts."), nl]
        ),
        WasOverloadedMsg = msg(Context, WasOverloadedPieces),
        ModuleInfo = ClauseContext ^ tecc_module_info,
        DetailMsgsList = list.map(describe_overloaded_symbol(ModuleInfo),
            OverloadedSymbolsSortedContexts),
        list.condense(DetailMsgsList, DetailMsgs),
        Msgs = [WasOverloadedMsg | DetailMsgs]
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
            SNAPiece = qual_sym_name_arity(SNA),
            StartPieces = [blank_line, words("The predicate symbol")] ++
                color_as_subject([SNAPiece, suffix(".")]) ++ [nl,
                words("The possible matches are:"), nl_indent_delta(1)],
            MakeItemPiecesFunc = describe_one_pred_name(ModuleInfo,
                yes(color_hint), should_module_qualify),
            construct_sorted_line_pieces(MakeItemPiecesFunc,
                PredIds, PredIdPieces),
            FirstPieces = StartPieces ++ PredIdPieces,
            LaterPieces = [words("That symbol"),
                words("is also overloaded here."), nl]
        ;
            Symbol = overloaded_func(ConsId, Sources),
            ConsIdPiece = qual_cons_id_and_maybe_arity(ConsId),
            StartPieces = [blank_line, words("The function symbol")] ++
                color_as_subject([ConsIdPiece, suffix(".")]) ++ [nl,
                words("The possible matches are:"), nl_indent_delta(1)],
            MakeItemPiecesFunc = describe_cons_type_info_source(ModuleInfo,
                yes(color_hint)),
            construct_sorted_line_pieces(MakeItemPiecesFunc,
                Sources, SourcePieces),
            FirstPieces = StartPieces ++ SourcePieces,
            LaterPieces = [words("That symbol"),
                words("is also overloaded here."), nl]
        ),
        FirstMsg = msg(FirstContext, FirstPieces),
        LaterMsgs = list.map(context_to_error_msg(LaterPieces), LaterContexts),
        Msgs = [FirstMsg | LaterMsgs]
    ).

:- func context_to_error_msg(list(format_piece), prog_context) = error_msg.

context_to_error_msg(Pieces, Context) = msg(Context, Pieces).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_error_overload.
%---------------------------------------------------------------------------%
