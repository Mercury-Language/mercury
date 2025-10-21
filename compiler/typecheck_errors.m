%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
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
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

%---------------------------------------------------------------------------%

:- func report_unsatisfiable_constraints(type_error_clause_context,
    prog_context, type_assign_set) = error_spec.

:- func report_missing_tvar_in_foreign_code(type_error_clause_context,
    prog_context, string) = error_spec.

:- func report_invalid_coerce_from_to(type_error_clause_context, prog_context,
    prog_var, tvarset, mer_type, mer_type) = error_spec.

:- func report_unresolved_coerce_from_to(type_error_clause_context,
    prog_context, prog_var, tvarset, mer_type, mer_type) = error_spec.

:- func report_redundant_coerce(type_error_clause_context, prog_context,
    prog_var, tvarset, mer_type) = error_spec.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.typecheck_error_util.
:- import_module hlds.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_test.

:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

report_unsatisfiable_constraints(ClauseContext, Context, TypeAssignSet)
        = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    list.map(unproven_constraints_to_string_set, TypeAssignSet,
        UnprovenStrSets),
    % AlwaysUnprovenStrSet contains all the constraints
    % that are unproven in *all* type assigns.
    AlwaysUnprovenStrSet = set.intersect_list(UnprovenStrSets),
    SometimesUnprovenStrSet0 = set.union_list(UnprovenStrSets),
    % SometimesUnprovenStrSet contains all the constraints
    % that are unproven in *some but not all* type assigns.
    set.difference(SometimesUnprovenStrSet0,
        AlwaysUnprovenStrSet, SometimesUnprovenStrSet),
    set.to_sorted_list(AlwaysUnprovenStrSet, AlwaysUnprovenStrs),
    set.to_sorted_list(SometimesUnprovenStrSet, SometimesUnprovenStrs),
    AlwaysUnprovenPieceLists = list.map(wrap_quote, AlwaysUnprovenStrs),
    SometimesUnprovenPieceLists = list.map(wrap_quote, SometimesUnprovenStrs),
    ACS = choose_number(AlwaysUnprovenStrs, "constraint", "constraints"),
    SCS = choose_number(SometimesUnprovenStrs, "constraint", "constraints"),
    AIsAre = choose_number(AlwaysUnprovenStrs, "is", "are"),
    (
        AlwaysUnprovenPieceLists = [_ | _],
        SometimesUnprovenPieceLists = [_ | _],
        ErrorPieces =
            [words("error: the typeclass"), words(ACS),
            nl_indent_delta(1)] ++
            pieces_list_to_color_line_pieces(color_subject, [],
                AlwaysUnprovenPieceLists) ++
            [nl_indent_delta(-1),
            words(AIsAre)] ++
            color_as_incorrect([words("unsatisfiable,")]) ++
            [words("and depending on the chosen resolution"),
            words("of some type ambiguities,"),
            choose_number(SometimesUnprovenStrs,
                words("the constraint"), words("some of the constraints")),
            nl_indent_delta(1)] ++
            pieces_list_to_color_line_pieces(color_subject, [],
                SometimesUnprovenPieceLists) ++
            [nl_indent_delta(-1),
            words("may be")] ++
            color_as_incorrect([words("unsatisfiable")]) ++
            [words("as well."), nl]
    ;
        AlwaysUnprovenPieceLists = [_ | _],
        SometimesUnprovenPieceLists = [],
        ErrorPieces =
            [words("error: the typeclass"), words(ACS),
            nl_indent_delta(1)] ++
            pieces_list_to_color_line_pieces(color_subject, [],
                AlwaysUnprovenPieceLists) ++
            [nl_indent_delta(-1),
            words(AIsAre)] ++
            color_as_incorrect([words("unsatisfiable.")]) ++
            [nl]
    ;
        AlwaysUnprovenPieceLists = [],
        SometimesUnprovenPieceLists = [_ | _],
        ErrorPieces =
            [words("error: at least one the typeclass"), words(SCS),
            nl_indent_delta(1)] ++
            pieces_list_to_color_line_pieces(color_subject, [],
                SometimesUnprovenPieceLists) ++
            [nl_indent_delta(-1),
            words("is")] ++
            color_as_incorrect([words("unsatisfiable,")]) ++
            [words("but which one this is depends on the chosen resolution"),
            words("of some type ambiguities."), nl]
    ;
        AlwaysUnprovenPieceLists = [],
        SometimesUnprovenPieceLists = [],
        unexpected($pred, "no constraints seem to be unproven")
    ),
    Spec = spec($pred, severity_error, phase_type_check, Context,
        InClauseForPieces ++ ErrorPieces).

:- pred unproven_constraints_to_string_set(type_assign::in,
    set(string)::out) is det.

unproven_constraints_to_string_set(TypeAssign, UnprovenConstraintStrSet) :-
    type_assign_get_constraint_db(TypeAssign, ConstraintDb),
    UnprovenHldsConstraints = ConstraintDb ^ hcd_unproven,
    (
        UnprovenHldsConstraints = [],
        set.init(UnprovenConstraintStrSet)
    ;
        UnprovenHldsConstraints = [_ | _],
        retrieve_prog_constraint_list(UnprovenHldsConstraints,
            UnprovenConstraints0),

        type_assign_get_typevarset(TypeAssign, TVarSet),
        type_assign_get_type_bindings(TypeAssign, Bindings),
        apply_rec_subst_to_prog_constraints(Bindings,
            UnprovenConstraints0, UnprovenConstraints1),
        list.sort_and_remove_dups(UnprovenConstraints1,
            UnprovenConstraints),
        UnprovenConstraintStrs = list.map(
            mercury_constraint_to_string(TVarSet, print_name_only),
            UnprovenConstraints),
        set.list_to_set(UnprovenConstraintStrs, UnprovenConstraintStrSet)
    ).

:- func wrap_quote(string) = list(format_piece).

wrap_quote(Str) = [quote(Str)].

%---------------------------------------------------------------------------%

report_missing_tvar_in_foreign_code(ClauseContext, Context, VarName) = Spec :-
    ModuleInfo = ClauseContext ^ tecc_module_info,
    PredId = ClauseContext ^ tecc_pred_id,
    Pieces = [words("Error: the foreign language code for")] ++
        describe_one_pred_name(ModuleInfo, yes(color_subject),
            should_module_qualify, [], PredId) ++
        [words("should define the variable")] ++
        color_as_incorrect([quote(VarName), suffix(".")]) ++ [nl],
    Spec = spec($pred, severity_error, phase_type_check, Context, Pieces).

%---------------------------------------------------------------------------%

report_invalid_coerce_from_to(ClauseContext, Context, FromVar, TVarSet,
        FromType, ToType) = Spec :-
    % XXX TYPECHECK_ERRORS
    % This code can generate some less-than-helpful diagnostics.
    %
    % - For tests/invalid/coerce_unify_tvars.m and some others, it says that
    %   you cannot coerce from one anonymous type variable to another.
    %
    % In most cases, we will report that the coerced argument type is
    % unresolved. For the remaining cases, is there something we can report
    % that would be more helpful?
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    VarSet = ClauseContext ^ tecc_varset,
    FromVarStr = mercury_var_to_name_only_vs(VarSet, FromVar),
    FromTypeStr = mercury_type_to_string(TVarSet, print_num_only, FromType),
    ToTypeStr = mercury_type_to_string(TVarSet, print_num_only, ToType),
    OnlyDuPieces = [words("You can only coerce"),
        words("from one discriminated union type to another, and")],
    ( if FromTypeStr = ToTypeStr then
        describe_if_non_du_type(FromType, FromTypeNonDuPieces),
        (
            FromTypeNonDuPieces = [],
            % We shouldn't get here. FromType and ToType must be the same du
            % type, but a coercion from one du type to the same du type must be
            % type-correct. However, throwing an exception would only punish an
            % innocent user.
            CausePieces = []
        ;
            FromTypeNonDuPieces = [_ | _],
            CausePieces = OnlyDuPieces ++
                [quote(FromTypeStr), words("is a")] ++
                color_as_incorrect(FromTypeNonDuPieces ++ [suffix(".")])
        )
    else
        describe_if_non_du_type(FromType, FromTypeNonDuPieces),
        describe_if_non_du_type(ToType, ToTypeNonDuPieces),
        (
            FromTypeNonDuPieces = [],
            (
                ToTypeNonDuPieces = [],
                % Either FromTypeNonDuPieces or ToTypeNonDuPieces should be
                % nonempty, so we shouldn't get here. However, throwing
                % an exception would only punish an innocent user.
                CausePieces = []
            ;
                ToTypeNonDuPieces = [_ | _],
                CausePieces = OnlyDuPieces ++
                    [quote(ToTypeStr), words("is a")] ++
                    color_as_incorrect(ToTypeNonDuPieces ++ [suffix(".")])
            )
        ;
            FromTypeNonDuPieces = [_ | _],
            (
                ToTypeNonDuPieces = [],
                CausePieces = OnlyDuPieces ++
                    [quote(FromTypeStr), words("is a")] ++
                    color_as_incorrect(FromTypeNonDuPieces ++ [suffix(".")])
            ;
                ToTypeNonDuPieces = [_ | _],
                ( if FromTypeNonDuPieces = ToTypeNonDuPieces then
                    CausePieces = OnlyDuPieces ++
                        [quote(FromTypeStr), words("and"), quote(ToTypeStr),
                        words("are")] ++
                        color_as_incorrect(FromTypeNonDuPieces ++
                            [suffix("s.")])
                else
                    CausePieces = OnlyDuPieces ++
                        [quote(FromTypeStr), words("is a")] ++
                        color_as_incorrect(FromTypeNonDuPieces ++
                            [suffix(",")]) ++
                        [words("while"), quote(ToTypeStr), words("is a")] ++
                        color_as_incorrect(ToTypeNonDuPieces ++ [suffix(".")])
                )
            )
        )
    ),
    ( if strip_kind_annotation(FromType) = strip_kind_annotation(ToType) then
        RedundantPieces =
            [words("Also, the type conversion would be redundant anyway.")]
    else
        RedundantPieces = []
    ),
    ErrorPieces = [words("error: cannot coerce")] ++
        color_as_subject([quote(FromVarStr)]) ++ [words("from")] ++
        color_as_inconsistent([quote(FromTypeStr)]) ++ [words("to")] ++
        color_as_inconsistent([quote(ToTypeStr), suffix(".")]) ++ [nl] ++
        CausePieces ++ RedundantPieces ++ [nl],
    Spec = spec($pred, severity_error, phase_type_check, Context,
        InClauseForPieces ++ ErrorPieces).

report_unresolved_coerce_from_to(ClauseContext, Context, FromVar, TVarSet,
        FromType, ToType) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    VarSet = ClauseContext ^ tecc_varset,
    FromVarStr = mercury_var_to_name_only_vs(VarSet, FromVar),
    FromTypeStr = mercury_type_to_string(TVarSet, print_num_only, FromType),
    ToTypeStr = mercury_type_to_string(TVarSet, print_num_only, ToType),
    ErrorPieces = [words("error: the type of")] ++
        color_as_subject([quote(FromVarStr)]) ++
        [words("is")] ++ color_as_incorrect([words("unresolved;")]) ++
        [words("cannot coerce from"), quote(FromTypeStr), words("to"),
        quote(ToTypeStr), suffix("."), nl],
    Spec = spec($pred, severity_error, phase_type_check, Context,
        InClauseForPieces ++ ErrorPieces).

report_redundant_coerce(ClauseContext, Context, FromVar, TVarSet, FromType) =
        Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    VarSet = ClauseContext ^ tecc_varset,
    FromVarStr = mercury_var_to_name_only_vs(VarSet, FromVar),
    FromTypeStr = mercury_type_to_string(TVarSet, print_num_only, FromType),
    ErrorPieces = [words("warning: type conversion of")] ++
        color_as_subject([quote(FromVarStr)]) ++
        [words("from"), quote(FromTypeStr), words("to the same type is")] ++
        color_as_incorrect([words("redundant.")]) ++ [nl],
    Severity = severity_warning(warn_redundant_coerce),
    Spec = spec($pred, Severity, phase_type_check, Context,
        InClauseForPieces ++ ErrorPieces).

    % If the given type is du type, return the empty list. Otherwise,
    % return a description of what kind of non-du type it is.
    %
:- pred describe_if_non_du_type(mer_type::in, list(format_piece)::out) is det.

describe_if_non_du_type(Type, DescPieces) :-
    % Our caller can the article "a" in front of the text we return,
    % and the plural suffix "s" after the text.
    (
        Type = type_variable(_, _),
        DescPieces = [words("type variable")]
    ;
        Type = defined_type(_, _, _),
        DescPieces = []
    ;
        Type = builtin_type(_),
        DescPieces = [words("builtin type")]
    ;
        Type = tuple_type(_, _),
        DescPieces = [words("tuple type")]
    ;
        Type = higher_order_type(PorF, _, _, _),
        (
            PorF = pf_function,
            DescPieces = [words("function type")]
        ;
            PorF = pf_predicate,
            DescPieces = [words("predicate type")]
        )
    ;
        Type = apply_n_type(_, _, _),
        DescPieces = [words("function type")]
    ;
        Type = kinded_type(SubType, _),
        describe_if_non_du_type(SubType, DescPieces)
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_errors.
%---------------------------------------------------------------------------%
