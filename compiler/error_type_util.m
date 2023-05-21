%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: error_type_util.m.
% Main author: zs.
%
% This module provides a way to format types nicely for error messages.
%
%---------------------------------------------------------------------------%

:- module parse_tree.error_type_util.
:- interface.

:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type maybe_add_quotes
    --->    do_not_add_quotes
    ;       add_quotes.

    % type_to_pieces(TVarSet, InstVarSet, VarNamePrint, MaybeAddQuotes,
    %   ExternalTypeParams, Type) = Pieces:
    %
    % Format Type for printing as part of an error message. Use TVarSet
    % as the source of the names of any type variables in the type, and
    % InstVarSet as the source of the names of any inst variables in any
    % higher order types. Put an existential quantifier in front of any type
    % that contains any of the type variables in ExternalTypeParams.
    %
:- func type_to_pieces(tvarset, inst_varset, var_name_print, maybe_add_quotes,
    list(tvar), mer_type) = list(format_piece).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_to_term.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

type_to_pieces(TVarSet, InstVarSet, VarNamePrint, MaybeAddQuotes,
        ExternalTypeParams, Type) = Pieces :-
    quote_pieces(MaybeAddQuotes, StartQuotePieces, EndQuotePieces),
    (
        ExternalTypeParams = [],
        % Optimize the common case.
        ExistQVars = []
    ;
        ExternalTypeParams = [_ | _],
        set_of_type_vars_in_type(Type, TypeVarsSet),
        set.list_to_set(ExternalTypeParams, ExternalTypeParamsSet),
        set.intersect(ExternalTypeParamsSet, TypeVarsSet, ExistQVarsSet),
        set.to_sorted_list(ExistQVarsSet, ExistQVars)
    ),
    % We switch on ExistQVars because *both* arms of the above switch
    % on ExternalTypeParams can generate ExistQVars = [].
    (
        ExistQVars = [],
        Pieces =
            StartQuotePieces ++
            type_pieces(TVarSet, InstVarSet, VarNamePrint,
                EndQuotePieces, Type)
    ;
        ExistQVars = [_ | _],
        ExistQVarStrs = list.map(
            mercury_var_to_string_vs(TVarSet, VarNamePrint),
            ExistQVars),
        ExistPieces = strict_list_to_pieces(ExistQVarStrs),
        ExistListPieces = [prefix("[")] ++ ExistPieces ++ [suffix("]")],
        % We wrap the parentheses around the quantified type
        % in prefix() and suffix() respectively because
        %
        % - these work the same as wrapping them in fixed()
        %   when FullPieces are used as is, including the newlines, and
        %
        % - they generate better looking output when our caller strips
        %   all the newlines out.
        Pieces = StartQuotePieces ++
            [fixed("some")] ++ ExistListPieces ++
            [left_paren_maybe_nl_inc("(", lp_plain)] ++
            type_pieces(TVarSet, InstVarSet, VarNamePrint, [], Type) ++
            [maybe_nl_dec_right_paren(")", rp_plain)] ++ EndQuotePieces
    ).

:- func type_pieces(tvarset, inst_varset, var_name_print,
    list(format_piece), mer_type) = list(format_piece).

type_pieces(TVarSet, InstVarSet, VarNamePrint, SuffixPieces, Type) = Pieces :-
    (
        Type = kinded_type(SubType, _Kind),
        Pieces = type_pieces(TVarSet, InstVarSet, VarNamePrint,
            SuffixPieces, SubType)
    ;
        Type = type_variable(TVar, _),
        TVarStr = mercury_var_to_string_vs(TVarSet, VarNamePrint, TVar),
        Pieces = [fixed(TVarStr) | SuffixPieces]
    ;
        Type = builtin_type(BuiltinType),
        builtin_type_name(BuiltinType, BuiltinTypeStr),
        Pieces = [fixed(BuiltinTypeStr) | SuffixPieces]
    ;
        (
            Type = defined_type(TypeCtorSymName0, ArgTypes, _),
            strip_builtin_qualifier_from_sym_name(TypeCtorSymName0,
                TypeCtorSymName),
            TypeCtorNameList0 = sym_name_to_list(TypeCtorSymName),
            TypeCtorNameList = list.map(maybe_quote_name, TypeCtorNameList0),
            TypeCtorStr = string.join_list(".", TypeCtorNameList),
            Const = TypeCtorStr,
            NonConstL = [fixed(TypeCtorStr),
                left_paren_maybe_nl_inc("(", lp_suffix)],
            NonConstR = [maybe_nl_dec_right_paren(")", rp_plain)]
        ;
            Type = tuple_type(ArgTypes, _),
            Const = "{}",
            NonConstL = [left_paren_maybe_nl_inc("{", lp_plain)],
            NonConstR = [maybe_nl_dec_right_paren("}", rp_plain)]
        ;
            Type = apply_n_type(TVar, ArgTypes, _),
            % XXX None of the test cases cover the output we generate
            % for apply_n_type, so I (zs) don't know whether this is ok.
            TVarStr = mercury_var_to_string_vs(TVarSet, VarNamePrint, TVar),
            Const = TVarStr,
            NonConstL = [fixed(TVarStr),
                left_paren_maybe_nl_inc("(", lp_suffix)],
            NonConstR = [maybe_nl_dec_right_paren(")", rp_plain)]
        ),
        (
            ArgTypes = [],
            Pieces = [fixed(Const) | SuffixPieces]
        ;
            ArgTypes = [_ | _],
            ArgTypePiecesList = list.map(
                type_pieces(TVarSet, InstVarSet, VarNamePrint, []),
                ArgTypes),
            Pieces = NonConstL ++
                component_list_to_line_pieces(ArgTypePiecesList, NonConstR) ++
                SuffixPieces
        )
    ;
        Type = higher_order_type(_, _, _, _, _),
        Pieces = higher_order_type_pieces(TVarSet, InstVarSet, VarNamePrint,
            SuffixPieces, Type)
    ).

:- func maybe_quote_name(string) = string.

maybe_quote_name(Name0) = Name :-
    FunctorGraphicChars = string_graphic_chars(Name0),
    (
        FunctorGraphicChars = no_graphic_chars,
        Name = Name0
    ;
        ( FunctorGraphicChars = some_graphic_chars
        ; FunctorGraphicChars = all_graphic_chars
        ),
        Name = add_quotes(Name0)
    ).

:- inst mer_type_higher_order for mer_type/0
    --->    higher_order_type(ground, ground, ground, ground, ground).

:- func higher_order_type_pieces(tvarset::in, inst_varset::in,
    var_name_print::in, list(format_piece)::in,
    mer_type::in(mer_type_higher_order))
    = (list(format_piece)::out) is det.

higher_order_type_pieces(TVarSet, InstVarSet, VarNamePrint, SuffixPieces,
        HigherOrderType) = Pieces :-
    HigherOrderType = higher_order_type(PorF, ArgTypes, HOInstInfo, Purity,
        _LambdaEvalMethod),
    ( Purity = purity_pure,     PurityPieces = []
    ; Purity = purity_semipure, PurityPieces = [words("semipure")]
    ; Purity = purity_impure,   PurityPieces = [words("impure")]
    ),
    (
        HOInstInfo = none_or_default_func,
        ArgPiecesList = list.map(
            type_pieces(TVarSet, InstVarSet, VarNamePrint, []),
            ArgTypes),
        FuncResultPrefixPieces = [],
        FuncResultSuffixPieces = [],
        DetismPieces = [],
        PorFMismatchPieces = [],
        ArityMismatchPieces = []
    ;
        HOInstInfo = higher_order(PredInstInfo),
        PorFStr = pred_or_func_to_full_str(PorF),
        PredInstInfo = pred_inst_info(HOPorF, ArgModes, _ArgRegs, Detism),
        ( if PorF = HOPorF then
            PorFMismatchPieces = []
        else
            HOPorFStr = pred_or_func_to_full_str(HOPorF),
            PorFMismatchPieces = [nl,
                words("The type says this is a"),
                words(PorFStr), suffix(","),
                words("but its mode says it is a"),
                words(HOPorFStr), suffix(".")]
        ),
        list.length(ArgTypes, NumArgTypes),
        list.length(ArgModes, NumArgModes),
        ( if NumArgTypes = NumArgModes then
            assoc_list.from_corresponding_lists(ArgTypes, ArgModes,
                ArgTypesModes),
            % If this higher order type is a function, then the type::mode
            % for the function result must be wrapped in parentheses.
            FuncResultPrefixPieces = [prefix("(")],
            FuncResultSuffixPieces = [suffix(")")],
            ArgPiecesList = list.map(
                type_and_mode_to_pieces(TVarSet, InstVarSet),
                ArgTypesModes),
            ArityMismatchPieces = []
        else
            ArgPiecesList = list.map(
                type_pieces(TVarSet, InstVarSet, VarNamePrint, []),
                ArgTypes),
            FuncResultPrefixPieces = [],
            FuncResultSuffixPieces = [],
            ArityMismatchPieces = [nl,
                words("The type says this"), words(PorFStr),
                words("has"), int_fixed(NumArgTypes), words("arguments,"),
                words("but its mode says it has"),
                int_fixed(NumArgModes), suffix(".")]
        ),
        DetismPieces = [words("is"), words(determinism_to_string(Detism))]
    ),
    % For predicates and functions that have arguments,
    % we wrap "pred(" and "func(" in prefix() and the closing ")" in suffix
    % because
    %
    % - these work the same as wrapping them in fixed()
    %   when the Pieces we generate are used as is, including the
    %   newlines, and
    %
    % - they generate better looking output when our caller strips
    %   all the newlines out of Pieces.
    (
        PorF = pf_predicate,
        (
            ArgPiecesList = [],
            PorFArgBlockPieces = [fixed("pred")]
        ;
            ArgPiecesList = [_ | _],
            PorFArgBlockPieces =
                [fixed("pred"), left_paren_maybe_nl_inc("(", lp_suffix)] ++
                component_list_to_line_pieces(ArgPiecesList,
                    [maybe_nl_dec_right_paren(")", rp_plain)])
        )
    ;
        PorF = pf_function,
        (
            ArgPiecesList = [],
            unexpected($pred, "function has no return value")
        ;
            ArgPiecesList = [ReturnValuePieces],
            PorFArgBlockPieces = [fixed("(func)"), fixed("=")] ++
                FuncResultPrefixPieces ++ ReturnValuePieces ++
                FuncResultSuffixPieces
        ;
            ArgPiecesList = [_, _ | _],
            list.det_split_last(ArgPiecesList,
                FuncArgPiecesList, ReturnValuePieces),
            PorFArgBlockPieces =
                [fixed("func"), left_paren_maybe_nl_inc("(", lp_suffix)] ++
                component_list_to_line_pieces(FuncArgPiecesList,
                    [maybe_nl_dec_right_paren(")", rp_plain)]) ++
                [fixed("=")] ++ FuncResultPrefixPieces ++
                ReturnValuePieces ++ FuncResultSuffixPieces
        )
    ),
    Pieces =
        PurityPieces ++ PorFArgBlockPieces ++ DetismPieces ++
        SuffixPieces ++
        PorFMismatchPieces ++ ArityMismatchPieces.

:- func type_and_mode_to_pieces(tvarset, inst_varset,
    pair(mer_type, mer_mode)) = list(format_piece).

type_and_mode_to_pieces(TVarSet, InstVarSet, Type - Mode) = Pieces :-
    TypePieces = type_pieces(TVarSet, InstVarSet, print_name_only, [], Type),
    ModeTerm0 = mode_to_term(output_mercury, Mode),
    term.coerce(ModeTerm0, ModeTerm),
    ModeTermStr =
        mercury_term_to_string_vs(InstVarSet, print_name_only, ModeTerm),
    Pieces = TypePieces ++ [fixed("::"), words(ModeTermStr)].

:- pred quote_pieces(maybe_add_quotes::in,
    list(format_piece)::out, list(format_piece)::out) is det.

quote_pieces(MaybeAddQuotes, StartQuotePieces, EndQuotePieces) :-
    (
        MaybeAddQuotes = do_not_add_quotes,
        StartQuotePieces = [],
        EndQuotePieces = []
    ;
        MaybeAddQuotes = add_quotes,
        StartQuotePieces = [prefix("`")],
        EndQuotePieces = [suffix("'")]
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.error_type_util.
%---------------------------------------------------------------------------%
