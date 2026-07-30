%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
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

:- import_module list.

%---------------------------------------------------------------------------%

:- func report_unsatisfiable_constraints(type_error_clause_context,
    prog_context, type_assign_set) = diag_spec.

:- func report_invalid_coerce_from_to(type_error_clause_context, prog_context,
    prog_var, tvarset, mer_type, mer_type, list(coerce_fail)) = diag_spec.

:- func report_unresolved_coerce_from_to(type_error_clause_context,
    prog_context, prog_var, tvarset, mer_type, mer_type) = diag_spec.

:- func report_redundant_coerce(type_error_clause_context, prog_context,
    prog_var, tvarset, mer_type) = diag_spec.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.typecheck_error_util.
:- import_module check_hlds.typecheck_util.
:- import_module hlds.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_test.

:- import_module int.
:- import_module one_or_more.
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

report_invalid_coerce_from_to(ClauseContext, Context, FromVar, TVarSet,
        FromType0, ToType0, Fails0) = Spec :-
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

    delete_identical_qualifications(FromType0, ToType0, FromType, ToType),
    FromTypeStr = mercury_type_to_string(TVarSet, print_num_only, FromType),
    ToTypeStr = mercury_type_to_string(TVarSet, print_num_only, ToType),

    % The code of are_actual_param_type_pair_as_related_as_needed,
    % when given a pair of types in an argument position that does not
    % have to be invariant, tries out coercions in *both* directions.
    % This means that if are_actual_param_type_pair_as_related_as_needed
    % can return a specific coerce_fail, it can also return its mirror
    % image, meaning it can also return a coerce_fail that is identical
    % except for the exchange of roles between the from-type and the to-type.
    %
    % By imposing a standard order on each coerce_fails, we allow the
    % call to sort_and_remove_dups to replace each mirror image
    % with just coerce_fail.
    %
    % XXX are_actual_param_type_pair_as_related_as_needed could instead
    % just arbitrarily always return either
    % - the coerce_fails from the original comparison direction, or
    % - the coerce_fails from the reverse comparison direction.
    % Neither this nor that method seems universally superior.
    Fails1 = list.map(standardize_coerce_fail, Fails0),
    list.sort_and_remove_dups(Fails1, Fails),
    CausePieceLists = list.map(describe_coerce_fail(TVarSet), Fails),

    list.condense(CausePieceLists, CausePieces),
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

%---------------------------------------------------------------------------%

:- func standardize_coerce_fail(coerce_fail) = coerce_fail.

standardize_coerce_fail(Fail0) = Fail :-
    (
        Fail0 = different_base_types(_, _, _, _),
        Fail = Fail0
    ;
        Fail0 = nonground_type(_, _, _),
        Fail = Fail0
    ;
        Fail0 = different_type_categories(TypeTable, FromType, ToType),
        ( if compare((>), FromType, ToType) then
            Fail = different_type_categories(TypeTable, ToType, FromType)
        else
            Fail = Fail0
        )
    ;
        Fail0 = different_builtin_types(FromBuiltinType, ToBuiltinType),
        ( if compare((>), FromBuiltinType, ToBuiltinType) then
            Fail = different_builtin_types(ToBuiltinType, FromBuiltinType)
        else
            Fail = Fail0
        )
    ;
        Fail0 = different_tuple_arities(FromArity, ToArity),
        ( if FromArity > ToArity then
            Fail = different_tuple_arities(ToArity, FromArity)
        else
            Fail = Fail0
        )
    ;
        Fail0 = du_type_is_not_subtype(_),
        Fail = Fail0
    ;
        Fail0 = cannot_unify_type_vars(FromType, ToType),
        ( if compare((>), FromType, ToType) then
            Fail = cannot_unify_type_vars(ToType, FromType)
        else
            Fail = Fail0
        )
    ;
        Fail0 = non_du_type_ctor(FromType, FromTypeDesc, ToType, ToTypeDesc),
        ( if compare((>), FromTypeDesc, ToTypeDesc) then
            Fail = non_du_type_ctor(ToType, ToTypeDesc, FromType, FromTypeDesc)
        else
            Fail = Fail0
        )
    ;
        Fail0 = should_be_invariant_arg(BaseTypeCtor, ArgNum, Reason,
            FromType, ToType),
        ( if compare((>), FromType, ToType) then
            Fail = should_be_invariant_arg(BaseTypeCtor, ArgNum, Reason,
                ToType, FromType)
        else
            Fail = Fail0
        )
    ).

%---------------------------------------------------------------------------%

:- func describe_coerce_fail(tvarset, coerce_fail) = list(format_piece).

describe_coerce_fail(TVarSet, Fail) = Pieces :-
    % XXX Generate descriptions for ALL kinds of coerce failures.
    (
        Fail = different_base_types(FromType, FromBaseTypeCtor,
            ToType, ToBaseTypeCtor),
        Pieces = describe_coerce_fail_different_base_types(TVarSet,
            FromType, FromBaseTypeCtor, ToType, ToBaseTypeCtor)
    ;
        Fail = nonground_type(_, _, _),
        % This coerce_fail does not occur in our test suite.
        Pieces = []
    ;
        Fail = different_type_categories(TypeTable, FromType, ToType),
        Pieces = describe_coerce_fail_different_type_categories(TypeTable,
            FromType, ToType)
    ;
        Fail = different_builtin_types(FromBuiltinType, ToBuiltinType),
        Pieces = describe_coerce_fail_different_builtin_types(TVarSet,
            FromBuiltinType, ToBuiltinType)
    ;
        Fail = different_tuple_arities(FromArity, ToArity),
        Pieces = describe_coerce_fail_different_tuple_arities(
            FromArity, ToArity)
    ;
        Fail = du_type_is_not_subtype(TypeCtor),
        Pieces = describe_coerce_fail_du_type_is_not_subtype(TypeCtor)
    ;
        Fail = cannot_unify_type_vars(FromType, ToType),
        Pieces = describe_coerce_fail_cannot_unify_type_vars(TVarSet,
            FromType, ToType)
    ;
        Fail = non_du_type_ctor(FromType, FromTypeDesc, ToType, ToTypeDesc),
        Pieces = describe_coerce_fail_non_du_type_ctor(TVarSet,
            FromType, FromTypeDesc, ToType, ToTypeDesc)
    ;
        Fail = should_be_invariant_arg(BaseTypeCtor, ArgNum, Reason,
            FromType, ToType),
        Pieces = describe_coerce_fail_should_be_invariant_arg(TVarSet,
            BaseTypeCtor, ArgNum, Reason, FromType, ToType)
    ).

%---------------------%

:- func describe_coerce_fail_different_base_types(tvarset,
    mer_type, type_ctor, mer_type, type_ctor) = list(format_piece).

describe_coerce_fail_different_base_types(_TVarSet,
        _FromType, FromBaseTypeCtor, _ToType, ToBaseTypeCtor) = Pieces :-
    FromBaseTypeCtor = type_ctor(FromSymName, _),
    ToBaseTypeCtor = type_ctor(ToSymName, _),
    % Print the module qualifiers on the type_ctors only if it is relevant.
    ( if
        FromSymName = qualified(ModuleName, _),
        ToSymName = qualified(ModuleName, _)
    then
        FromBaseCtorPiece = unqual_type_ctor(FromBaseTypeCtor),
        ToBaseCtorPiece = unqual_type_ctor(ToBaseTypeCtor)
    else
        FromBaseCtorPiece = qual_type_ctor(FromBaseTypeCtor),
        ToBaseCtorPiece = qual_type_ctor(ToBaseTypeCtor)
    ),
    % The use of coerce-from and coerce-to terminology here works
    % because the comparison of base types is NOT subject to reversal
    % by are_actual_param_type_pair_as_related_as_needed, since it happens
    % above that predicate in the call tree.
    Pieces = [words("The base type constructor of the coerce-from type is")] ++
        color_as_inconsistent([FromBaseCtorPiece, suffix(",")]) ++
        [words("while for the coerce-to type it is")] ++
        color_as_inconsistent([ToBaseCtorPiece, suffix(".")]) ++
        [nl].
% XXX A possible alternative wording.
    % FromTypeStr = mercury_type_to_string(TVarSet, print_num_only, FromType),
    % ToTypeStr =   mercury_type_to_string(TVarSet, print_num_only, ToType),
%   Pieces = [words("You can coerce"),
%       words("from one discriminated union type to another"),
%       words("only if they have the same base type constructor,"),
%       words("meaning that following the chain of supertypes from both"),
%       % XXX Should this be included?
%       % words("the from-type and the to-type"),
%       words("ends up at the same type constructor."),
%       words("In this case, the base type constructor of the from-type")] ++
%       color_as_subject([words(FromTypeStr)]) ++
%       [words("is")] ++
%       color_as_inconsistent([FromBaseCtorPiece, suffix(",")]) ++
%       [words("while the base type constructor of the to-type")] ++
%       color_as_subject([words(ToTypeStr)]) ++
%       [words("is")] ++
%       color_as_inconsistent([ToBaseCtorPiece, suffix(".")]) ++
%       [nl].

%---------------------%

:- func describe_coerce_fail_different_type_categories(type_table,
    mer_type, mer_type) = list(format_piece).

describe_coerce_fail_different_type_categories(TypeTable,
        FromType, ToType) = Pieces :-
    classify_is_du_type(TypeTable, FromType, FromMaybeDuType),
    classify_is_du_type(TypeTable, ToType, ToMaybeDuType),
    (
        FromMaybeDuType = is_not_du_type(FromTypeDesc),
        ToMaybeDuType =   is_not_du_type(ToTypeDesc),
        Pieces =
            color_as_subject(
                [upper_case_next, words(FromTypeDesc), suffix("s")]) ++
            [words("and")] ++
            color_as_subject([words(ToTypeDesc), suffix("s")]) ++
            [words("cannot be either coerced from, or coerced to."), nl]
    ;
        FromMaybeDuType = is_not_du_type(FromTypeDesc),
        ToMaybeDuType =   is_du_type(_),
        Pieces =
            color_as_subject(
                [upper_case_next, words(FromTypeDesc), suffix("s")]) ++
            [words("cannot be either coerced from, or coerced to."), nl]
    ;
        FromMaybeDuType = is_du_type(_),
        ToMaybeDuType =   is_not_du_type(ToTypeDesc),
        Pieces =
            color_as_subject(
                [upper_case_next, words(ToTypeDesc), suffix("s")]) ++
            [words("cannot be either coerced from, or coerced to."), nl]
    ;
        FromMaybeDuType = is_du_type(_),
        ToMaybeDuType =   is_du_type(_),
        % We should generate a different coerce_fail for the situation
        % in which we now generate this one.
        Pieces = []
    ).

%---------------------%

:- func describe_coerce_fail_different_builtin_types(tvarset,
    builtin_type, builtin_type) = list(format_piece).

describe_coerce_fail_different_builtin_types(TVarSet,
        FromBuiltinType, ToBuiltinType) = Pieces :-
    FromType = builtin_type(FromBuiltinType),
    ToType =   builtin_type(ToBuiltinType),
    FromTypeStr = mercury_type_to_string(TVarSet, print_num_only, FromType),
    ToTypeStr =   mercury_type_to_string(TVarSet, print_num_only, ToType),
    Pieces = [words("Builtin types such as")] ++
        color_as_subject([words(FromTypeStr)]) ++ [words("and")] ++
        color_as_subject([words(ToTypeStr)]) ++
        [words("cannot be either coerced from, or coerced to."), nl].

%---------------------%

:- func describe_coerce_fail_different_tuple_arities(arity, arity)
    = list(format_piece).

describe_coerce_fail_different_tuple_arities(FromArity, ToArity) = Pieces :-
    Pieces = [words("You cannot coerce between a tuple type of")] ++
        color_as_inconsistent([words("arity"), int_fixed(FromArity)]) ++
        [words("and a tuple type of")] ++
        color_as_inconsistent([words("arity"), int_fixed(ToArity),
            suffix(".")]) ++
        [nl].

%---------------------%

:- func describe_coerce_fail_du_type_is_not_subtype(type_ctor)
    = list(format_piece).

describe_coerce_fail_du_type_is_not_subtype(TypeCtor) = Pieces :-
    Pieces = [qual_type_ctor(TypeCtor), words("is")] ++
        color_as_incorrect([words("not a subtype.")]) ++ [nl].

%---------------------%

:- func describe_coerce_fail_cannot_unify_type_vars(tvarset,
    mer_type, mer_type) = list(format_piece).

describe_coerce_fail_cannot_unify_type_vars(TVarSet, FromType, ToType)
        = Pieces :-
    FromTypeStr = mercury_type_to_string(TVarSet, print_num_only, FromType),
    ToTypeStr =   mercury_type_to_string(TVarSet, print_num_only, ToType),
    ( if FromType = type_variable(_, _) then
        ( if ToType = type_variable(_, _) then
            TypeVarOrVarsStr = "Unconstrained type variables",
            TVarPieces = color_as_subject([fixed(FromTypeStr)]) ++
                [words("and")] ++ color_as_subject([fixed(ToTypeStr)]),
            ItIsTheyAre = "they are"
        else
            TypeVarOrVarsStr = "An unconstrained type variable",
            TVarPieces = color_as_subject([fixed(FromTypeStr)]),
            ItIsTheyAre = "it is"
        )
    else
        ( if ToType = type_variable(_, _) then
            TypeVarOrVarsStr = "An unconstrained type variable",
            TVarPieces = color_as_subject([fixed(ToTypeStr)]),
            ItIsTheyAre = "it is"
        else
            unexpected($pred, "neither FromType nor ToType is a variable")
        )
    ),
    Pieces = [words(TypeVarOrVarsStr), words("such as")] ++ TVarPieces ++
        [words("cannot be either coerced from, or coerced to,"),
        words("because"), words(ItIsTheyAre), words("not known either"),
        words("to be equal to any type, or to be in a"),
        words("subtype relationship with any type."), nl].

%---------------------%

:- func describe_coerce_fail_non_du_type_ctor(tvarset,
    mer_type, string, mer_type, string) = list(format_piece).

describe_coerce_fail_non_du_type_ctor(TVarSet, FromType, FromTypeDesc,
        ToType, ToTypeDesc) = Pieces :-
    FromTypeStr = mercury_type_to_string(TVarSet, print_num_only, FromType),
    ToTypeStr = mercury_type_to_string(TVarSet, print_num_only, ToType),
    OnlyDuPieces = [words("You can only coerce"),
        words("from one discriminated union type to another, and")],
    describe_if_non_du_type(FromTypeDesc, FromTypeNonDuPieces),
    describe_if_non_du_type(ToTypeDesc, ToTypeNonDuPieces),
    (
        FromTypeNonDuPieces = [],
        (
            ToTypeNonDuPieces = [],
            % Either FromTypeNonDuPieces or ToTypeNonDuPieces should be
            % nonempty, so we shouldn't get here. However, throwing
            % an exception would only punish an innocent user.
            Pieces = []
        ;
            ToTypeNonDuPieces = [_ | _],
            Pieces = OnlyDuPieces ++
                [quote(ToTypeStr), words("is a")] ++
                color_as_incorrect(ToTypeNonDuPieces ++ [suffix(".")])
        )
    ;
        FromTypeNonDuPieces = [_ | _],
        (
            ToTypeNonDuPieces = [],
            Pieces = OnlyDuPieces ++
                [quote(FromTypeStr), words("is a")] ++
                color_as_incorrect(FromTypeNonDuPieces ++ [suffix(".")])
        ;
            ToTypeNonDuPieces = [_ | _],
            ( if FromTypeNonDuPieces = ToTypeNonDuPieces then
                ( if FromTypeStr = ToTypeStr then
                    Pieces = OnlyDuPieces ++
                        [quote(FromTypeStr), words("is a")] ++
                        color_as_incorrect(FromTypeNonDuPieces ++
                        [suffix(".")])
                else
                    Pieces = OnlyDuPieces ++
                        [quote(FromTypeStr), words("and"), quote(ToTypeStr),
                        words("are")] ++
                        color_as_incorrect(FromTypeNonDuPieces ++
                            [suffix("s.")])
                )
            else
                Pieces = OnlyDuPieces ++
                    [quote(FromTypeStr), words("is a")] ++
                    color_as_incorrect(FromTypeNonDuPieces ++
                        [suffix(",")]) ++
                    [words("while"), quote(ToTypeStr), words("is a")] ++
                    color_as_incorrect(ToTypeNonDuPieces ++ [suffix(".")])
            )
        )
    ).

:- pred describe_if_non_du_type(string::in, list(format_piece)::out) is det.

describe_if_non_du_type(NonDuDesc, DescPieces) :-
    ( if NonDuDesc = "" then
        DescPieces = []
    else
        DescPieces = [words(NonDuDesc)]
    ).

%---------------------%

:- func describe_coerce_fail_should_be_invariant_arg(tvarset, type_ctor, uint,
    invariant_reason, mer_type, mer_type) = list(format_piece).

describe_coerce_fail_should_be_invariant_arg(_TVarSet, BaseTypeCtor, ArgNum,
        Reason, _FromType, _ToType) = Pieces :-
    % XXX Should we print _FromType and _ToType?
    % In the usual case where BaseTypeCtor has a low arity, they will be
    % obvious from the names of the coerce-from and coerce-to types.
    (
        Reason = ir_higher_order,
        Pieces = [words("The type parameter that these types are bound to"),
            words("occurs in a higher order type."),
            words("Normally, it would be ok for the input arguments"),
            words("of higher order types to be co-variant,"),
            words("the Mercury type checker does not know"),
            words("which arguments are input."),
            words("It ensures soundness by requiring all arguments"),
            words("of higher order types to be invariant,"),
            words("meaning they must be the same in the"),
            words("coerced-from and coerced-to types.")]
    ;
        Reason = ir_base_type_ctor(OoMCtorArgPosns),
        BaseTypeCtor = type_ctor(_, BaseTypeCtorArity),
        ( if
            BaseTypeCtorArity = 1,
            ArgNum = 1u
        then
            ArgNumPieces = [words("only parameter")]
        else
            ArgNumPieces = [unth_fixed(ArgNum), words("parameter")]
        ),
        FrontPieces = [words("The")] ++ ArgNumPieces ++
            [words("of the type constructor"), unqual_type_ctor(BaseTypeCtor),
            words("must be")] ++ color_as_correct([words("invariant")]) ++
            [words("(meaning that it must be bound to the same type"),
            words("in the coerced-from and coerced-to types)"),
            words("because it occurs in")],
        OoMCtorArgPosns = one_or_more(HeadCtorArgPosn, TailCtorArgPosns),
        HeadCtorArgPosnPieces = ctor_arg_posn_to_pieces(HeadCtorArgPosn),
        (
            TailCtorArgPosns = [],
            Pieces = FrontPieces ++
                HeadCtorArgPosnPieces ++ [suffix("."), nl]
        ;
            TailCtorArgPosns = [_ | _],
            TailCtorArgPosnPieces =
                list.map(ctor_arg_posn_to_pieces, TailCtorArgPosns),
            CtorArgPosnPiecesLists =
                [HeadCtorArgPosnPieces | TailCtorArgPosnPieces],
            list.intersperse_list_last([[suffix(","), nl]],
                [[suffix(","), words("and"), nl]], CtorArgPosnPiecesLists,
                AllCtorArgPosnPiecesLists),
            list.condense(AllCtorArgPosnPiecesLists, AllCtorArgPosnPieces),
            Pieces = FrontPieces ++
                [nl_indent_delta(1)] ++
                AllCtorArgPosnPieces ++
                [suffix("."), nl_indent_delta(-1)]
        )
    ).

:- func ctor_arg_posn_to_pieces(ctor_arg_posn) = list(format_piece).

ctor_arg_posn_to_pieces(CtorArgPosn) = Pieces :-
    CtorArgPosn = ctor_arg_posn(DuOrTupleConsId, ArgNum, PosnReason),
    Pieces = [words("the type of the"),
        unth_fixed(ArgNum), words("argument of the"),
        unqual_cons_id_and_maybe_arity(coerce(DuOrTupleConsId)),
        words("data constructor, which")] ++
        posn_invariant_reason_to_pieces(PosnReason).

:- func posn_invariant_reason_to_pieces(posn_invariant_reason)
    = list(format_piece).

posn_invariant_reason_to_pieces(PosnReason) = Pieces :-
    % XXX These should be color_as_incorrect, but to be consistent,
    % we could need the color to include any comma suffix.
    (
        PosnReason = pir_du_nonrec(BaseTypeCtor, TypeCtor),
        ( if BaseTypeCtor = TypeCtor then
            Pieces = [words("applies the base type constructor"),
                unqual_type_ctor(BaseTypeCtor),
                words("to a different list of type parameters")]
        else
            % This should be a temporary limitation.
            Pieces = [words("has a type constructor other than"),
                unqual_type_ctor(BaseTypeCtor), suffix(","),
                words("namely"), unqual_type_ctor(TypeCtor)]
        )
    ;
        PosnReason = pir_foreign,
        Pieces = [words("is a foreign type")]
    ;
        PosnReason = pir_solver,
        Pieces = [words("is a solver type")]
    ;
        PosnReason = pir_abstract,
        Pieces = [words("is an abstract type")]
    ;
        PosnReason = pir_higher_order,
        Pieces = [words("is a higher order type")]
    ).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_errors.
%---------------------------------------------------------------------------%
