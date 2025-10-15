%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_error_diff.m.
% Main author: zs.
%
% This module generates descriptions of the differences between actual and
% expected types, for use in diagnostics for type errors.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_error_diff.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.

    % type_diff_pieces(ExistQTVars, ActualType, ExpectedType) = DiffPieces.
    %
:- func type_diff_pieces(list(tvar), mer_type, mer_type) = list(format_piece).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.prog_type_test.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%

type_diff_pieces(ExistQTVars, ActualType0, ExpectedType0) = DiffPieces :-
    DiffPieces = generate_type_diff_pieces([], ExistQTVars, top_level,
        ActualType0, ExpectedType0).

%---------------------------------------------------------------------------%

:- type maybe_top_level
    --->    not_top_level
    ;       top_level.

:- func generate_type_diff_pieces(list(format_piece), list(tvar),
    maybe_top_level, mer_type, mer_type) = list(format_piece).

generate_type_diff_pieces(ContextPieces, ExistQTVars, MaybeTopLevel,
        ActualType0, ExpectedType0) = DiffPieces :-
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
            ActualType = defined_type(ActualTypeSymName,
                ActualArgTypes, _),
            ExpectedType = defined_type(ExpectedTypeSymName,
                ExpectedArgTypes, _),
            list.length(ActualArgTypes, ActualArity),
            list.length(ExpectedArgTypes, ExpectedArity),
            ActualTypeCtor = type_ctor(ActualTypeSymName, ActualArity),
            ExpectedTypeCtor = type_ctor(ExpectedTypeSymName, ExpectedArity),
            ( if ActualTypeCtor = ExpectedTypeCtor then
                % If ActualArgTypes = ExpectedArgTypes, then there is
                % no difference to report.
                ActualArgTypes \= ExpectedArgTypes,
                DiffPiecesPrime = arg_type_list_diff_pieces(ContextPieces,
                    [words("type constructor"),
                        unqual_type_ctor(ActualTypeCtor)],
                    ExistQTVars, ActualArgTypes, ExpectedArgTypes)
            else
                ( if ActualArity = ExpectedArity then
                    % Do not generate a diff if the difference is
                    % at the top level, because in that case, the diff
                    % would just duplicate the printing of the whole of
                    % the actual and expected types.
                    MaybeTopLevel = not_top_level,
                    ( if
                        ActualTypeSymName = qualified(CommonModuleName, _),
                        ExpectedTypeSymName = qualified(CommonModuleName, _)
                    then
                        ActualTCPiece = unqual_type_ctor(ActualTypeCtor),
                        ExpectedTCPiece = unqual_type_ctor(ExpectedTypeCtor)
                    else
                        ActualTCPiece = qual_type_ctor(ActualTypeCtor),
                        ExpectedTCPiece = qual_type_ctor(ExpectedTypeCtor)
                    ),
                    CausePieces =
                        [words("Argument type mismatch:"),
                        words("expected"), nl_indent_delta(1)] ++
                        color_as_correct([ExpectedTCPiece, suffix(",")]) ++
                        [nl_indent_delta(-1),
                        words("got"), nl_indent_delta(1)] ++
                        color_as_incorrect([ActualTCPiece, suffix(".")]) ++
                        [nl_indent_delta(-1)]
                else
                    % Since arity differences are easy to overlook,
                    % we want to point them out even at the top level.
                    TypeCtorPieces = [words("type constructor"),
                        unqual_sym_name(ActualTypeSymName)],
                    CausePieces = report_type_ctor_arity_mismatch(
                        TypeCtorPieces, ActualArity, ExpectedArity)
                ),
                DiffPiecesPrime = wrap_diff_pieces(ContextPieces, CausePieces)
            )
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
                ActualInstInfo, ActualPurity),
            ExpectedType = higher_order_type(ExpectedPorF, ExpectedArgTypes,
                ExpectedInstInfo, ExpectedPurity),
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

%---------------------------------------------------------------------------%

:- func report_type_ctor_arity_mismatch(list(format_piece), arity, arity)
    = list(format_piece).

report_type_ctor_arity_mismatch(TypeCtorPieces, ActualNumArgs, ExpectedNumArgs)
        = Pieces :-
    ( if ExpectedNumArgs = 1 then
        ArgumentS = "argument"
    else
        ArgumentS = "arguments"
    ),
    Pieces =
        [words("Arity mismatch for")] ++ TypeCtorPieces ++ [suffix(":"),
        words("expected")] ++
        color_as_correct([int_name(ExpectedNumArgs),
            words(ArgumentS), suffix(",")]) ++
        [words("got")] ++
        color_as_incorrect([int_name(ActualNumArgs), suffix(".")]).

%---------------------------------------------------------------------------%

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
            TypeCtorPieces, ExpectedNumArgs, ExistQTVars, 1,
            ActualArgTypes, ExpectedArgTypes)
    else
        CausePieces = report_type_ctor_arity_mismatch(TypeCtorPieces,
            ActualNumArgs, ExpectedNumArgs),
        DiffPieces = wrap_diff_pieces(ContextPieces, CausePieces)
    ).

    % Return a description of any differences between the given
    % actual and expected argument types. The caller should have ensured
    % that the two lists are the same length.
    %
:- func arg_type_list_diff_pieces_loop(list(format_piece),
    list(format_piece), arity, list(tvar), int, list(mer_type), list(mer_type))
    = list(format_piece).

arg_type_list_diff_pieces_loop(_, _, _, _, _, [], []) = [].
arg_type_list_diff_pieces_loop(_, _, _, _, _, [], [_ | _]) = _ :-
    unexpected($pred, "list length mismatch").
arg_type_list_diff_pieces_loop(_, _, _, _, _, [_ | _], []) = _ :-
    unexpected($pred, "list length mismatch").
arg_type_list_diff_pieces_loop(ContextPieces, TypeCtorPieces, TypeCtorArity,
        ExistQTVars, CurArgNum, [ActualArgType | ActualArgTypes],
        [ExpectedArgType | ExpectedArgTypes]) = DiffPieces :-
    TailDiffPieces = arg_type_list_diff_pieces_loop(ContextPieces,
        TypeCtorPieces, TypeCtorArity, ExistQTVars, CurArgNum + 1,
        ActualArgTypes, ExpectedArgTypes),
    ( if ActualArgType = ExpectedArgType then
        DiffPieces = TailDiffPieces
    else
        ( if TypeCtorArity = 1 then
            ArgNumOfPieces = [words("only argument of")]
        else
            ArgNumOfPieces = [nth_fixed(CurArgNum), words("argument of")]
        ),
        ArgContextPieces = [treat_next_as_first | ContextPieces] ++
            [lower_case_next_if_not_first, words("In the")] ++
            ArgNumOfPieces ++ TypeCtorPieces ++ [suffix(":"), nl],
        HeadDiffPieces = generate_type_diff_pieces(ArgContextPieces,
            ExistQTVars, not_top_level, ActualArgType, ExpectedArgType),
        DiffPieces = HeadDiffPieces ++ TailDiffPieces
    ).

%---------------------------------------------------------------------------%

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
        ExpActPredFuncCausePieces =
            [words("Predicate vs function mismatch: expected a")] ++
            color_as_correct([p_or_f(ExpectedPorF), suffix(",")]) ++
            [words("got a")] ++
            color_as_incorrect([p_or_f(ActualPorF), suffix(".")]),
        add_to_diff_pieces(ContextPieces, ExpActPredFuncCausePieces,
            !DiffPieces)
    ),
    ( if ActualPurity = ExpectedPurity then
        true
    else
        ExpActPurityCausePieces =
            [words("Purity mismatch: expected"),
            purity_desc_article(ExpectedPurity)] ++
            color_as_correct([purity_desc(ExpectedPurity),
                p_or_f(ExpectedPorF), suffix(",")]) ++
            [words("got"), purity_desc_article(ActualPurity)] ++
            color_as_incorrect([purity_desc(ActualPurity),
                p_or_f(ActualPorF), suffix(".")]),
        add_to_diff_pieces(ContextPieces, ExpActPurityCausePieces, !DiffPieces)
    ),
    ( if ActualArgTypes = ExpectedArgTypes then
        true
    else
        TypeCtorPieces = [lower_case_next_if_not_first,
            words("The"), p_or_f(ActualPorF)],
        ArgTypeCausePieces = arg_type_list_diff_pieces(ContextPieces,
            TypeCtorPieces, ExistQTVars, ActualArgTypes, ExpectedArgTypes),
        add_to_diff_pieces(ContextPieces, ArgTypeCausePieces, !DiffPieces)
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
            % The coloring pattern differs from the messages above
            % because this is a difference between either two sources of
            % actual information, or between two sources of expected
            % information, not between one actual and one expected source.
            ( if ActualHOPorF = ActualPorF then
                true
            else
                ActPredFuncTypeModeCausePieces =
                    color_as_incorrect(
                        [words("Predicate vs function mismatch:")]) ++
                    [words("the actual type is a")] ++
                    color_as_inconsistent([p_or_f(ActualPorF), suffix(",")]) ++
                    [words("but its mode says it is a")] ++
                    color_as_inconsistent([p_or_f(ActualHOPorF), suffix(".")]),
                add_to_diff_pieces(ContextPieces,
                    ActPredFuncTypeModeCausePieces, !DiffPieces)
            ),
            ( if ExpectedHOPorF = ExpectedPorF then
                true
            else
                ExpPredFuncTypeModeCausePieces =
                    color_as_incorrect(
                        [words("Predicate vs function mismatch:")]) ++
                    [words("the expected type is a")] ++
                    color_as_inconsistent([p_or_f(ActualPorF), suffix(",")]) ++
                    [words("but its mode says it is a")] ++
                    color_as_inconsistent([p_or_f(ActualHOPorF), suffix(".")]),
                add_to_diff_pieces(ContextPieces,
                    ExpPredFuncTypeModeCausePieces, !DiffPieces)
            ),
            list.length(ActualArgTypes, ActualNumArgTypes),
            list.length(ExpectedArgTypes, ExpectedNumArgTypes),
            list.length(ActualArgModes, ActualNumArgModes),
            list.length(ExpectedArgModes, ExpectedNumArgModes),
            ( if ActualNumArgTypes = ActualNumArgModes then
                true
            else
                adjust_func_arity(ActualPorF,
                    ActualTypeArity, ActualNumArgTypes),
                adjust_func_arity(ActualPorF,
                    ActualModeArity, ActualNumArgModes),
                ActArityCausePieces =
                    color_as_incorrect([words("Arity mismatch:")]) ++
                    [words("the actual"), p_or_f(ActualPorF),
                    words("type has")] ++
                    color_as_inconsistent([int_name(ActualTypeArity),
                        words("arguments"), suffix(",")]) ++
                    [words("but its mode information says it has")] ++
                    color_as_inconsistent([int_name(ActualModeArity),
                        words("arguments.")]),
                add_to_diff_pieces(ContextPieces, ActArityCausePieces,
                    !DiffPieces)
            ),
            ( if ExpectedNumArgTypes = ExpectedNumArgModes then
                true
            else
                adjust_func_arity(ExpectedPorF,
                    ExpectedTypeArity, ExpectedNumArgTypes),
                adjust_func_arity(ExpectedPorF,
                    ExpectedModeArity, ExpectedNumArgModes),
                ExpArityCausePieces =
                    color_as_incorrect([words("Arity mismatch:")]) ++
                    [words("the actual"), p_or_f(ExpectedPorF),
                    words("type has")] ++
                    color_as_inconsistent([int_name(ExpectedTypeArity),
                        words("arguments"), suffix(",")]) ++
                    [words("but its mode information says it has")] ++
                    color_as_inconsistent([int_name(ExpectedModeArity),
                        words("arguments.")]),
                add_to_diff_pieces(ContextPieces, ExpArityCausePieces,
                    !DiffPieces)
            ),
            ( if ActualArgModes = ExpectedArgModes then
                true
            else
                % We could try to report which argument(s), or even which
                % piece(s) of which argument(s), contain the difference ...
                ModeCausePieces =
                    color_as_incorrect([words("Mode mismatch:")]) ++
                    [words("the actual and expected modes of the"),
                    p_or_f(ActualPorF), words("differ.")],
                add_to_diff_pieces(ContextPieces, ModeCausePieces,
                    !DiffPieces)
            ),
            ( if ActualDetism = ExpectedDetism then
                true
            else
                ActualDetismStr = determinism_to_string(ActualDetism),
                ExpectedDetismStr = determinism_to_string(ExpectedDetism),
                DetismCausePieces =
                    [words("Determinism mismatch:"),
                    words("the actual"), p_or_f(ActualPorF), words("has"),
                    words("determinism")] ++
                    color_as_incorrect([words(ActualDetismStr),
                        suffix(",")]) ++
                    [words("but the expected determinism is")] ++
                    color_as_correct([words(ExpectedDetismStr),
                        suffix(".")]),
                add_to_diff_pieces(ContextPieces, DetismCausePieces,
                    !DiffPieces)
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

:- pred add_to_diff_pieces(list(format_piece)::in, list(format_piece)::in,
    list(format_piece)::in, list(format_piece)::out) is det.

add_to_diff_pieces(ContextPieces, MismatchPieces, !DiffPieces) :-
    NewDiffPieces = wrap_diff_pieces(ContextPieces, MismatchPieces),
    !:DiffPieces = !.DiffPieces ++ NewDiffPieces.

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
:- end_module check_hlds.typecheck_error_diff.
%---------------------------------------------------------------------------%
