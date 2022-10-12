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
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_to_term.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.write_error_spec.

:- import_module assoc_list.
:- import_module int.
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
        FullPieces =
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
        FullPieces = StartQuotePieces ++
            [fixed("some")] ++ ExistListPieces ++ [prefix("(")] ++
            [nl_indent_delta(1)] ++
            type_pieces(TVarSet, InstVarSet, VarNamePrint, [], Type) ++
            [nl_indent_delta(-1)] ++
            [suffix(")")] ++ EndQuotePieces
    ),

    NoNlPieces = filter_out_newlines(FullPieces),
    NoNlStr = error_pieces_to_string(NoNlPieces),
    ( if string.count_codepoints(NoNlStr) < max_one_line_type_length then
        Pieces = NoNlPieces
    else
        Pieces = FullPieces
    ).

:- func max_one_line_type_length = int.

max_one_line_type_length = 40.

:- func type_pieces(tvarset, inst_varset, var_name_print,
    list(format_piece), mer_type) = list(format_piece).

type_pieces(TVarSet, InstVarSet, VarNamePrint, SuffixPieces, Type) = Pieces :-
    % XXX Should we test whether a version of Pieces that has its
    % newlines stripped from it is shorter than max_one_line_type_length?
    % XXX The ideal solution would be to return some representation
    % that would allow the code that converts error_specs to strings
    % to pick
    %
    % - the version without the newlines, if there is enough space
    %   left on the current line for it (using certain knowledge, not
    %   the guess represented by max_one_line_type_length), but
    % - falling back to the version with the newlines, if there is
    %   not enough space.
    %
    % However, the multi-phase approach we use for converting format_pieces
    % to strings does not make implementing the above approach straightforward.
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
        builtin_type_to_string(BuiltinType, BuiltinTypeStr),
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
            NonConstStart = TypeCtorStr ++ "(",
            NonConstEnd = ")"
        ;
            Type = tuple_type(ArgTypes, _),
            Const = "{}",
            NonConstStart = "{",
            NonConstEnd = "}"
        ;
            Type = apply_n_type(TVar, ArgTypes, _),
            % XXX None of the test cases cover the output we generate
            % for apply_n_type, so I (zs) don't know whether this is ok.
            TVarStr = mercury_var_to_string_vs(TVarSet, VarNamePrint, TVar),
            Const = TVarStr,
            NonConstStart = TVarStr ++ "(",
            NonConstEnd = ")"
        ),
        (
            ArgTypes = [],
            Pieces = [fixed(Const) | SuffixPieces]
        ;
            ArgTypes = [_ | _],
            ArgTypePiecesList = list.map(
                type_pieces(TVarSet, InstVarSet, VarNamePrint, []),
                ArgTypes),
            % We wrap NonConstStart and NonConstEnd in prefix() and suffix()
            % respectively because
            %
            % - these work the same as wrapping them in fixed()
            %   when the Pieces we generate are used as is, including the
            %   newlines, and
            %
            % - they generate better looking output when our caller strips
            %   all the newlines out of Pieces.
            Pieces = [prefix(NonConstStart), nl_indent_delta(1)] ++
                component_list_to_line_pieces(ArgTypePiecesList,
                    [nl_indent_delta(-1)]) ++
                [suffix(NonConstEnd) | SuffixPieces]
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
                [prefix("pred("), nl_indent_delta(1)] ++
                component_list_to_line_pieces(ArgPiecesList,
                    [nl_indent_delta(-1)]) ++
                [suffix(")")]
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
            PorFArgBlockPieces = [prefix("func("), nl_indent_delta(1)] ++
                component_list_to_line_pieces(FuncArgPiecesList,
                    [nl_indent_delta(-1)]) ++
                [suffix(")"), fixed("=")] ++ FuncResultPrefixPieces ++
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

:- func filter_out_newlines(list(format_piece)) = list(format_piece).

filter_out_newlines([]) = [].
filter_out_newlines([Piece | Pieces]) = FilteredPieces :-
    FilteredPiecesTail = filter_out_newlines(Pieces),
    ( if
        ( Piece = nl
        ; Piece = nl_indent_delta(_)
        )
    then
        FilteredPieces = FilteredPiecesTail
    else
        FilteredPieces = [Piece | FilteredPiecesTail]
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.error_type_util.
%---------------------------------------------------------------------------%
