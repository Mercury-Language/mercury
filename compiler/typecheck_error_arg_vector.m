%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_error_arg_vector.m.
%
% This modules reports type errors in argument vectors.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_error_arg_vector.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_error_type_assign.
:- import_module check_hlds.typecheck_error_util.
:- import_module check_hlds.typecheck_info.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- type arg_vector_type_error
    --->    arg_vector_type_error(
                % The argument number in which the error occurred.
                int,

                % The variable at that argument position.
                prog_var,

                % The actual and expected types at that argument position.
                actual_expected_types
            ).

:- func report_error_wrong_types_in_arg_vector(typecheck_info, prog_context,
    arg_vector_kind, type_assign_set, list(arg_vector_type_error))
    = error_spec.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.typecheck_error_builtin.
:- import_module hlds.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_term.

:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

report_error_wrong_types_in_arg_vector(Info, Context,
        ArgVectorKind, TypeAssignSet, ArgVectorTypeErrors0) = Spec :-
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    list.sort(ArgVectorTypeErrors0, ArgVectorTypeErrors),
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
        HeadArgVectorTypeErrors, TailArgVectorTypeErrors, ArgErrorPieces),
    ( if
        (
            ArgVectorKind = arg_vector_plain_pred_call(SymNamePredFormArity),
            SymNamePredFormArity =
                sym_name_pred_form_arity(SymName, PredFormArity)
        ;
            ArgVectorKind = arg_vector_plain_call_pred_id(PredId),
            ModuleInfo = ClauseContext ^ tecc_module_info,
            module_info_pred_info(ModuleInfo, PredId, PredInfo),
            pred_info_get_sym_name(PredInfo, SymName),
            PredFormArity = pred_info_pred_form_arity(PredInfo)
        ),
        is_int_pred_op(SymName, PredFormArity)
    then
        list.foldl(acc_builtin_types_of_arg_vector_type_error,
            ArgVectorTypeErrors, set.init, BuiltinTypes),
        InvisIntPieces =
            report_any_invisible_int_types(ClauseContext, BuiltinTypes)
    else
        InvisIntPieces = []
    ),
    type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent),
    Msg = simple_msg(Context,
        [always(InClauseForPieces), always(ArgVectorKindPieces),
        always(ArgErrorPieces ++ InvisIntPieces), VerboseComponent]),
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
        ExpectedPieces0, _ExpectedType, _ExistQTVars, _Source),
    find_possible_switched_positions(VarSet, ActualPieces, AllErrors,
        SwitchedPosPieces),
    (
        SwitchedPosPieces = [],
        ExpectedPieces = ExpectedPieces0 ++ [SuffixPiece],
        NlSwitchedPosPieces = [nl_indent_delta(-1)]
    ;
        SwitchedPosPieces = [_ | _],
        ExpectedPieces = ExpectedPieces0,
        NlSwitchedPosPieces = [nl_indent_delta(-1)] ++
            SwitchedPosPieces ++ [SuffixPiece]
    ),
    Pieces = [words("in argument"), int_fixed(ArgNum), suffix(":"),
        nl_indent_delta(1) |
        color_as_subject(argument_name_to_pieces_lc(VarSet, lcw_none, Var))] ++
        [words("has type"), nl_indent_delta(1)] ++
        color_as_incorrect(ActualPieces ++ [suffix(",")]) ++
            [nl_indent_delta(-1),
        words("expected type was"), nl_indent_delta(1)] ++
        color_as_correct(ExpectedPieces) ++ NlSwitchedPosPieces ++
        [nl_indent_delta(-1) | TailPieces].

:- pred find_possible_switched_positions(prog_varset::in,
    list(format_piece)::in, list(arg_vector_type_error)::in,
    list(format_piece)::out) is det.

find_possible_switched_positions(VarSet, SearchActualPieces, AllErrors,
        Pieces) :-
    find_expecteds_matching_actual(VarSet, SearchActualPieces, AllErrors,
        SwitchedPosPieces),
    (
        SwitchedPosPieces = [],
        Pieces = []
    ;
        SwitchedPosPieces = [_ | _],
        Pieces = [words("(the actual type is")] ++
            color_as_hint([words("the same as")]) ++
            [words("the expected type of")] ++
            SwitchedPosPieces ++ [suffix(")")]
    ).

:- pred find_expecteds_matching_actual(prog_varset::in,
    list(format_piece)::in, list(arg_vector_type_error)::in,
    list(format_piece)::out) is det.

find_expecteds_matching_actual(_VarSet, _SearchActualPieces, [], []).
find_expecteds_matching_actual(VarSet, SearchActualPieces,
        [HeadError | TailErrors], SwitchedPosPieces) :-
    find_expecteds_matching_actual(VarSet, SearchActualPieces, TailErrors,
        TailSwitchedPosPieces),
    HeadError = arg_vector_type_error(ArgNum, Var, ActualExpected),
    ActualExpected = actual_expected_types(_ActualPieces, _ActualType,
        ExpectedPieces, _ExpectedType, _ExistQTVars, _Source),
    ( if SearchActualPieces = ExpectedPieces then
        ( if varset.search_name(VarSet, Var, _) then
            HeadSwitchedPosPieces = [words("argument"), int_fixed(ArgNum),
                suffix(","), words("which is variable"),
                quote(mercury_var_to_name_only_vs(VarSet, Var))]
        else
            HeadSwitchedPosPieces = [words("argument"), int_fixed(ArgNum)]
        ),
        (
            TailSwitchedPosPieces = [],
            SwitchedPosPieces = HeadSwitchedPosPieces
        ;
            TailSwitchedPosPieces = [_ | _],
            ConnectPieces = [suffix(","), words("and")],
            SwitchedPosPieces = HeadSwitchedPosPieces ++ ConnectPieces ++
                TailSwitchedPosPieces
        )
    else
        SwitchedPosPieces = TailSwitchedPosPieces
    ).

%---------------------------------------------------------------------------%

:- pred acc_builtin_types_of_arg_vector_type_error(arg_vector_type_error::in,
    set(builtin_type)::in, set(builtin_type)::out) is det.

acc_builtin_types_of_arg_vector_type_error(Error, !BuiltinTypes) :-
    Error = arg_vector_type_error(_ArgNum, _Var, ActualExpected),
    ActualExpected = actual_expected_types(_ActualPieces, ActualType,
        _ExpectedPieces, ExpectedType, _ExistQTVars, _Source),
    acc_builtin_type(ActualType, !BuiltinTypes),
    acc_builtin_type(ExpectedType, !BuiltinTypes).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_error_arg_vector.
%---------------------------------------------------------------------------%
