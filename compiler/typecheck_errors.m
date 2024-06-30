%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% Copyright (C) 2014-2024 The Mercury team.
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
:- import_module check_hlds.typecheck_error_type_assign.
:- import_module check_hlds.typecheck_error_util.
:- import_module check_hlds.typecheck_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- func report_unsatisfiable_constraints(type_error_clause_context,
    prog_context, type_assign_set) = error_spec.

:- func report_missing_tvar_in_foreign_code(type_error_clause_context,
    prog_context, string) = error_spec.

:- func report_invalid_coerce_from_to(type_error_clause_context, prog_context,
    tvarset, mer_type, mer_type) = error_spec.

:- func report_redundant_coerce(type_error_clause_context, prog_context,
    tvarset, mer_type) = error_spec.

%---------------------------------------------------------------------------%

:- func report_error_unify_var_var(typecheck_info, unify_context, prog_context,
    prog_var, prog_var, type_assign_set) = error_spec.

:- func report_error_unify_var_lambda(typecheck_info, unify_context,
    prog_context, pred_or_func, prog_var, list(prog_var), type_assign_set)
    = error_spec.

:- func report_error_unify_var_functor_result(typecheck_info,
    unify_context, prog_context, prog_var, list(cons_type_info), cons_id,
    int, type_assign_set) = error_spec.

:- func report_error_unify_var_functor_args(typecheck_info,
    unify_context, prog_context, prog_var, list(cons_type_info), cons_id,
    list(prog_var), args_type_assign_set) = error_spec.

%---------------------------------------------------------------------------%

    % report_error_var_has_wrong_type uses this type to return
    % not just an error_spec, but possibly also an actual_expected_types
    % structure to its caller, which may then give a list of these structures
    % to report_error_wrong_types_in_arg_vector.
    %
:- type spec_and_maybe_actual_expected
    --->    spec_and_maybe_actual_expected(
                % A report of the type error.
                error_spec,

                % The actual and expected types involved in the type error,
                % if both are unambiguously known.
                maybe(actual_expected_types)
            ).

% The two main difference between the next two functions is that
% - the first takes a type_assign_set and explicit specifies the expected
%   type, while
% - the second takes an args_type_assign_set, and gets a separate
%   expected types from each args_type_assign in the set.

:- func report_error_var_has_wrong_type(typecheck_info,
    type_error_goal_context, prog_context, prog_var, mer_type, type_assign_set)
    = spec_and_maybe_actual_expected.

:- func report_error_var_has_wrong_type_arg(typecheck_info,
    type_error_goal_context, prog_context, int, prog_var, args_type_assign_set)
    = error_spec.

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

:- import_module hlds.hlds_class.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_type_util.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_test.
:- import_module parse_tree.prog_type_unify.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.var_db.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
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
    type_assign_get_typeclass_constraints(TypeAssign, Constraints),
    UnprovenHldsConstraints = Constraints ^ hcs_unproven,
    (
        UnprovenHldsConstraints = [],
        set.init(UnprovenConstraintStrSet)
    ;
        UnprovenHldsConstraints = [_ | _],
        retrieve_prog_constraint_list(UnprovenHldsConstraints,
            UnprovenConstraints0),

        type_assign_get_typevarset(TypeAssign, TVarSet),
        type_assign_get_type_bindings(TypeAssign, Bindings),
        apply_rec_subst_to_prog_constraint_list(Bindings,
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

report_invalid_coerce_from_to(ClauseContext, Context, TVarSet,
        FromType, ToType) = Spec :-
    % XXX TYPECHECK_ERRORS
    % This code can generate some less-than-helpful diagnostics.
    %
    % - For tests/invalid/coerce_infer.m and some others, it says that
    %   you cannot coerce from one anonymous type variable to another.
    %
    % Is there something we can report that would be more helpful?
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
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
    ErrorPieces = [words("cannot coerce from")] ++
        color_as_inconsistent([quote(FromTypeStr)]) ++ [words("to")] ++
        color_as_inconsistent([quote(ToTypeStr), suffix(".")]) ++ [nl] ++
        CausePieces ++ [nl],
    Spec = spec($pred, severity_error, phase_type_check, Context,
        InClauseForPieces ++ ErrorPieces).

report_redundant_coerce(ClauseContext, Context, TVarSet, FromType) = Spec :-
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    FromTypeStr = mercury_type_to_string(TVarSet, print_num_only, FromType),
    ErrorPieces = [words("warning: type conversion from")] ++
        [quote(FromTypeStr), words("to the same type is")] ++
        color_as_incorrect([words("redundant.")]) ++ [nl],
    Severity = severity_conditional(warn_redundant_coerce, yes,
        severity_warning, no),
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
%---------------------------------------------------------------------------%

report_error_unify_var_var(Info, UnifyContext, Context, X, Y, TypeAssignSet)
        = Spec :-
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    unify_context_to_pieces(UnifyContext, _LastContextWord,
        InClauseForPieces, ContextPieces),

    VarSet = ClauseContext ^ tecc_varset,
    get_inst_varset(ClauseContext, InstVarSet),
    MaybeColor = yes(color_inconsistent),
    MainPieces = [words("type error in unification of variable"),
        quote(mercury_var_to_name_only_vs(VarSet, X)), words("and variable"),
        quote(mercury_var_to_name_only_vs(VarSet, Y)), suffix("."), nl] ++
        color_as_subject([quote(mercury_var_to_name_only_vs(VarSet, X))]) ++
        type_of_var_to_pieces(InstVarSet, MaybeColor,
            TypeAssignSet, [suffix(",")], X) ++ [nl] ++
        color_as_subject([quote(mercury_var_to_name_only_vs(VarSet, Y))]) ++
        type_of_var_to_pieces(InstVarSet, MaybeColor,
            TypeAssignSet, [suffix(".")], Y) ++ [nl],
    type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent),
    Msg = simple_msg(Context,
        [always(ContextPieces), always(MainPieces), VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

%---------------------------------------------------------------------------%

report_error_unify_var_lambda(Info, UnifyContext, Context, PredOrFunc,
        Var, ArgVars, TypeAssignSet) = Spec :-
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    unify_context_to_pieces(UnifyContext, LastContextWord,
        InClauseForPieces, ContextPieces),

    VarSet = ClauseContext ^ tecc_varset,
    VarNamePieces = argument_name_to_pieces_uc(VarSet, LastContextWord, Var),
    get_inst_varset(ClauseContext, InstVarSet),
    Pieces1 = [words("type error in unification of")] ++ VarNamePieces ++ [nl],
    (
        PredOrFunc = pf_predicate,
        Pieces2 = [words("and"), prefix("pred("),
            words(mercury_vars_to_name_only_vs(VarSet, ArgVars)),
            suffix(")"), words(":- ...':"), nl]
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgVars, FuncArgs, RetVar),
        Pieces2 = [words("and"), prefix("func("),
            words(mercury_vars_to_name_only_vs(VarSet, FuncArgs)),
            suffix(")"), fixed("="),
            words(mercury_var_to_name_only_vs(VarSet, RetVar)),
            words(":- ...':"), nl]
    ),

    % XXX There is no test case that tests the output of this function,
    % so it is hard to decide what should be colored.
    MaybeColor = maybe.no,
    Pieces3 = VarNamePieces ++
        type_of_var_to_pieces(InstVarSet, MaybeColor, TypeAssignSet,
            [suffix(",")], Var) ++
        [nl],

    (
        PredOrFunc = pf_predicate,
        (
            ArgVars = [],
            LambdaTypePieces = [words("pred")]
        ;
            ArgVars = [_ | _],
            list.length(ArgVars, NumArgVars),
            list.duplicate(NumArgVars, "_", Unders),
            CommaUnders = string.join_list(", ", Unders),
            LambdaTypePieces = [words("pred(_" ++ CommaUnders ++ ")")]
        )
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgVars, FuncArgVars, _),
        (
            FuncArgVars = [],
            LambdaTypePieces = [words("func = _")]
        ;
            FuncArgVars = [_ | _],
            list.length(FuncArgVars, NumArgVars),
            list.duplicate(NumArgVars, "_", Unders),
            CommaUnders = string.join_list(", ", Unders),
            LambdaTypePieces = [words("func(_" ++ CommaUnders ++ ") = _")]
        )
    ),
    Pieces4 = [words("lambda expression has type") | LambdaTypePieces] ++
        [suffix("."), nl],
    MainPieces = Pieces1 ++ Pieces2 ++ Pieces3 ++ Pieces4,

    type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent),
    Msg = simple_msg(Context,
        [always(ContextPieces), always(MainPieces), VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

%---------------------------------------------------------------------------%

report_error_unify_var_functor_result(Info, UnifyContext, Context,
        Var, ConsDefnList, Functor, Arity, TypeAssignSet) = Spec :-
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    unify_context_to_pieces(UnifyContext, LastContextWord,
        InClauseForPieces, ContextPieces),

    VarSet = ClauseContext ^ tecc_varset,
    get_inst_varset(ClauseContext, InstVarSet),

    VarTypePieces = type_of_var_to_pieces(InstVarSet,
        yes(color_incorrect), TypeAssignSet, [suffix(",")], Var),
    FunctorTypePieces = type_of_functor_to_pieces(InstVarSet,
        yes(color_correct), Functor, Arity, ConsDefnList, [suffix(".")]),
    % XXX TYPECHECK_ERRORS If all elements of TypeAssignSet agree on the type
    % of Var, and if ConsDefnList contains only a single element, then
    % we should be able to print the diff between the two types,
    % including pointers such as the arity we expected vs what we got.

    MainPieces = [words("type error in unification of")] ++
        argument_name_to_pieces_lc(VarSet, LastContextWord, Var) ++
        [nl, words("and")] ++
        functor_name_to_pieces(Functor, Arity) ++ [suffix("."), nl] ++

        color_as_subject(
            argument_name_to_pieces_uc(VarSet, LastContextWord, Var)) ++
        VarTypePieces ++ [nl] ++

        color_as_subject(functor_name_to_pieces(Functor, Arity)) ++
        FunctorTypePieces ++ [nl],

    ( if
        Functor = some_int_const(int_const(_)),
        get_all_transformed_type_stuffs(typestuff_to_type, TypeAssignSet,
            Var, TypesOfVar),
        list.any_true(expected_type_needs_int_constant_suffix, TypesOfVar)
    then
        NoSuffixIntegerPieces = nosuffix_integer_pieces
    else
        NoSuffixIntegerPieces = []
    ),

    ( if is_int_func_op(Functor) then
        acc_builtin_types_of_var(TypeAssignSet, Var, set.init, BuiltinTypes0),
        acc_builtin_types_in_cons_type_infos(ConsDefnList,
            BuiltinTypes0, BuiltinTypes),
        InvisIntPieces =
            report_any_invisible_int_types(ClauseContext, BuiltinTypes)
    else
        InvisIntPieces = []
    ),

    type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent),
    AlwaysPieces = ContextPieces ++ MainPieces ++
        NoSuffixIntegerPieces ++ InvisIntPieces,
    Msg = simple_msg(Context, [always(AlwaysPieces), VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

%---------------------------------------------------------------------------%

report_error_unify_var_functor_args(Info, UnifyContext, Context,
        Var, ConsDefnList, Functor, ArgVars, ArgsTypeAssignSet) = Spec :-
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    unify_context_to_pieces(UnifyContext, LastContextWord,
        InClauseForPieces, ContextPieces),

    ModuleInfo = ClauseContext ^ tecc_module_info,
    VarSet = ClauseContext ^ tecc_varset,
    get_inst_varset(ClauseContext, InstVarSet),
    strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor),
    StrippedFunctorStr = functor_cons_id_to_string(ModuleInfo,
        vns_varset(VarSet), print_name_only, StrippedFunctor, ArgVars),
    list.length(ArgVars, Arity),

    TypeAssignSet = convert_args_type_assign_set(ArgsTypeAssignSet),

    % If we have consistent information about the argument types,
    % we prefer to print an error message that mentions only the arguments
    % that may be in error.
    ConsArgTypesSet = list.map(get_expected_arg_types, ArgsTypeAssignSet),

    ( if
        list.all_same(ConsArgTypesSet),
        ConsArgTypesSet = [ConsArgTypes | _]
    then
        assoc_list.from_corresponding_lists(ArgVars, ConsArgTypes,
            ArgExpTypes),
        TypeAssigns = list.map(get_caller_arg_assign, ArgsTypeAssignSet),
        find_mismatched_args(do_not_add_quotes, InstVarSet, TypeAssigns,
            1, ArgExpTypes,
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
            list.reverse(RevNoSubsumeMismatches, Mismatches)
        ;
            RevNoSubsumeMismatches = [],
            list.reverse(RevSubsumesMismatches, Mismatches)
        ),
        categorize_mismatch_infos(Mismatches, CatMismatches0,
            map.init, ExpTVarCounts),
        list.negated_filter(hide_categorized_mismatch_info(ExpTVarCounts),
            CatMismatches0, CatMismatches),
        list.length(CatMismatches, NumCatMismatches),
        MaybeNumMismatches = yes(NumCatMismatches),
        ArgErrorPieces = mismatched_args_to_pieces(VarSet, Functor, is_first,
            CatMismatches),
        VerboseComponents = []
    else
        % XXX It should be possible to compute which arguments are
        % definitely OK, and which are suspect.
        MaybeNumMismatches = no,

        % For polymorphic data structures, the type of `Var' (the functor's
        % result type) can affect the valid types for the arguments.
        ( if
            % Could the type of the functor be polymorphic?
            some [ConsDefn] (
                list.member(ConsDefn, ConsDefnList),
                ConsDefn ^ cti_arg_types = [_ | _]
            )
        then
            % If so, print out the type of `Var'.
            % XXX Should this be in color, and if so, *which* color?
            ResultColor = maybe.no,
            ResultTypePieces =
                argument_name_to_pieces_uc(VarSet, LastContextWord, Var) ++
                type_of_var_to_pieces(InstVarSet, ResultColor, TypeAssignSet,
                    [suffix(",")], Var) ++
                [nl]
        else
            ResultTypePieces = []
        ),
        FunctorColor = yes(color_correct),
        (
            ArgVars = [],
            AllTypesPieces =
                functor_name_to_pieces(Functor, Arity) ++
                type_of_functor_to_pieces(InstVarSet, FunctorColor,
                    Functor, Arity, ConsDefnList, [suffix(".")]) ++
                [nl]
        ;
            ArgVars = [HeadArgVar | TailArgVars],
            VarColor = yes(color_incorrect),
            AllTypesPieces =
                functor_name_to_pieces(Functor, Arity) ++
                type_of_functor_to_pieces(InstVarSet, FunctorColor,
                    Functor, Arity, ConsDefnList, [suffix(",")]) ++
                types_of_vars_to_pieces(VarSet, InstVarSet, VarColor,
                    TypeAssignSet, [suffix("."), nl], HeadArgVar, TailArgVars)
        ),
        ArgErrorPieces = ResultTypePieces ++ AllTypesPieces,
        type_assign_set_msg_to_verbose_component(Info, VarSet,
            TypeAssignSet, VerboseComponent),
        VerboseComponents = [VerboseComponent]
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
        color_as_subject(
            argument_name_to_pieces_lc(VarSet, LastContextWord, Var)) ++
        [nl,
        words("and term")] ++
        color_as_subject([words_quote(StrippedFunctorStr), suffix(":")]) ++
            [nl,
        words("type error in"), words(Arguments), words("of")] ++
        functor_name_to_pieces(StrippedFunctor, Arity) ++ [suffix("."), nl],

    ( if is_int_func_op(Functor) then
        list.foldl(acc_builtin_types_of_var(TypeAssignSet), [Var | ArgVars],
            set.init, BuiltinTypes0),
        acc_builtin_types_in_cons_type_infos(ConsDefnList,
            BuiltinTypes0, BuiltinTypes),
        InvisIntTypePieces =
            report_any_invisible_int_types(ClauseContext, BuiltinTypes)
    else
        InvisIntTypePieces = []
    ),

    AlwaysPieces = ContextPieces ++ VarAndTermPieces ++ ArgErrorPieces ++
        InvisIntTypePieces,
    Msg = simple_msg(Context, [always(AlwaysPieces) | VerboseComponents]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

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

:- type categorized_mismatch_info
    --->    categorized_mismatch_info(
                int,                % Argument number, starting from 1.
                prog_var,           % Variable in that position
                mismatch_category,
                set(type_mismatch_special)
            ).

:- type mismatch_category
    --->    one_expected(
                one_expected_tvar   :: maybe(tvar),
                one_expected_pieces :: list(format_piece),
                one_actual_pieces   :: one_or_more(list(format_piece))
            )
    ;       several_expected(
                several_head        :: type_mismatch,
                several_tail        :: list(type_mismatch)
            ).

:- type actual_and_maybe_special
    --->    actual_and_maybe_special(
                list(format_piece),
                maybe(type_mismatch_special)
            ).

:- type does_actual_subsume_expected
    --->    actual_does_not_subsume_expected
    ;       actual_subsumes_expected.

:- type type_mismatch_special
    --->    type_mismatch_special_getopt_error(string).

:- type type_mismatch
    --->    type_mismatch_exp_act(
                expected_is_tvar    :: maybe(tvar),
                expected_type_desc  :: list(format_piece),
                actual_type_desc    :: list(format_piece),
                mismatch_subsumes   :: does_actual_subsume_expected,
                maybe_special       :: maybe(type_mismatch_special)
            ).

:- pred find_mismatched_args(maybe_add_quotes::in, inst_varset::in,
    type_assign_set::in, int::in, assoc_list(prog_var, mer_type)::in,
    list(mismatch_info)::in, list(mismatch_info)::out,
    list(mismatch_info)::in, list(mismatch_info)::out) is det.

find_mismatched_args(_, _, _, _, [],
        !RevSubsumesMismatches, !RevNoSubsumeMismatches).
find_mismatched_args(AddQuotes, InstVarSet, TypeAssignSet,
        CurArgNum, [Arg - ExpType | ArgExpTypes],
        !RevSubsumesMismatches, !RevNoSubsumeMismatches) :-
    % XXX When we get a test case in which the quadratic behavior of
    % get_all_type_stuffs_remove_dups is a performance issue, we should
    % try switching to get_all_type_stuffs without the remove_dups,
    % since the call to list.sort_and_remove_dups below should make it
    % semantically unnecessary.
    get_all_type_stuffs_remove_dups(TypeAssignSet, Arg, TypeStuffList),
    strip_module_names_from_type(strip_builtin_module_name, set_default_func,
        ExpType, StrippedExpType),
    list.foldl2(
        substitute_types_check_match(AddQuotes, InstVarSet, StrippedExpType),
        TypeStuffList,
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
    find_mismatched_args(AddQuotes, InstVarSet, TypeAssignSet,
        CurArgNum + 1, ArgExpTypes,
        !RevSubsumesMismatches, !RevNoSubsumeMismatches).

:- type does_some_type_stuff_match
    --->    no_type_stuff_matches
    ;       some_type_stuff_matches.

:- pred substitute_types_check_match(maybe_add_quotes::in, inst_varset::in,
    mer_type::in, type_stuff::in,
    list(type_mismatch)::in, list(type_mismatch)::out,
    does_some_type_stuff_match::in, does_some_type_stuff_match::out) is det.

substitute_types_check_match(AddQuotes, InstVarSet, StrippedExpType, TypeStuff,
        !TypeMismatches, !DoesSomeTypeStuffMatch) :-
    TypeStuff = type_stuff(ArgType, TVarSet, TypeBindings, ExistQTVars),
    strip_module_names_from_type(strip_builtin_module_name, set_default_func,
        ArgType, StrippedArgType),
    apply_rec_subst_to_type(TypeBindings, StrippedArgType, FullArgType),
    apply_rec_subst_to_type(TypeBindings, StrippedExpType, FullExpType),
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
        ExpectedPieces0 = type_to_pieces(TVarSet, InstVarSet, print_name_only,
            AddQuotes, ExistQTVars, FullExpType),
        ActualPieces0 = type_to_pieces(TVarSet, InstVarSet, print_name_only,
            AddQuotes, ExistQTVars, FullArgType),
        ( if ExpectedPieces0 = ActualPieces0 then
            ExpectedPieces = type_to_pieces(TVarSet, InstVarSet,
                print_name_and_num, AddQuotes, ExistQTVars, FullExpType),
            ActualPieces = type_to_pieces(TVarSet, InstVarSet,
                print_name_and_num, AddQuotes, ExistQTVars, FullArgType)
        else
            ExpectedPieces = ExpectedPieces0,
            ActualPieces = ActualPieces0
        ),
        ( if
            FullExpType = builtin_type(builtin_type_string),
            FullArgType = defined_type(ArgTypeCtorSymName, [_], kind_star),
            ArgTypeCtorSymName = qualified(ArgTypeModuleName, ArgTypeName),
            ArgTypeName = "option_error",
            is_std_lib_module_name(ArgTypeModuleName, StdLibModuleName),
            ( StdLibModuleName = "getopt"
            ; StdLibModuleName = "getopt_io"
            )
        then
            Special = type_mismatch_special_getopt_error(StdLibModuleName),
            MaybeSpecial = yes(Special)
        else
            MaybeSpecial = no
        ),
        ( if
            FullExpType = type_variable(ExpTVar, kind_star),
            not list.member(ExpTVar, ExistQTVars)
        then
            MaybeExpTVar = yes(ExpTVar)
        else
            MaybeExpTVar = no
        ),
        TypeMismatch = type_mismatch_exp_act(MaybeExpTVar, ExpectedPieces,
            ActualPieces, ActualSubsumesExpected, MaybeSpecial),
        !:TypeMismatches = [TypeMismatch | !.TypeMismatches]
    ).

:- pred all_no_subsume_mismatches(list(type_mismatch)::in) is semidet.

all_no_subsume_mismatches([]).
all_no_subsume_mismatches([Mismatch | Mismatches]) :-
    Mismatch ^ mismatch_subsumes = actual_does_not_subsume_expected,
    all_no_subsume_mismatches(Mismatches).

:- pred categorize_mismatch_infos(list(mismatch_info)::in,
    list(categorized_mismatch_info)::out,
    map(tvar, int)::in, map(tvar, int)::out) is det.

categorize_mismatch_infos([], [], !ExpTVarCounts).
categorize_mismatch_infos([Mismatch | Mismatches], [CatMI | CatMIs],
        !ExpTVarCounts) :-
    Mismatch = mismatch_info(ArgNum, Var,
        HeadTypeMismatch, TailTypeMismatches),
    HeadTypeMismatch =
        type_mismatch_exp_act(MaybeExpTVar,
            HeadExpectedPieces, HeadActualPieces,
            _ActualSubsumesExpected, _MaybeSpecial),
    ( if
        expected_types_all_same_return_actuals(HeadExpectedPieces,
            TailTypeMismatches, TailActualPieces)
    then
        MismatchCategory = one_expected(MaybeExpTVar, HeadExpectedPieces,
            one_or_more(HeadActualPieces, TailActualPieces)),
        (
            MaybeExpTVar = yes(ExpTVar),
            ( if map.search(!.ExpTVarCounts, ExpTVar, Count0) then
                map.det_update(ExpTVar, Count0 + 1, !ExpTVarCounts)
            else
                map.det_insert(ExpTVar, 1, !ExpTVarCounts)
            )
        ;
            MaybeExpTVar = no
        )
    else
        % NOTE Instead of returning all the mismatches unchanged,
        % we could group them by expected pieces. For example, if
        % [HeadTypeMismatch | TailTypeMismatches] had five elements, and
        % two had one value for the expected pieces and three another value,
        % then we could return a list containing two one_expected structures
        % (which should then be renamed).
        %
        % This would probably generate more useful output. However,
        % we almost never get errors for which this change would make
        % any difference, so it is not a priority, and we would need
        % a motivating example where it *would* make a difference to
        % help us decide on what the right formatting would be for such cases.
        MismatchCategory =
            several_expected(HeadTypeMismatch, TailTypeMismatches)
    ),
    gather_special_type_mismatches([HeadTypeMismatch | TailTypeMismatches],
        SpecialMismatches),
    CatMI = categorized_mismatch_info(ArgNum, Var, MismatchCategory,
        SpecialMismatches),
    categorize_mismatch_infos(Mismatches, CatMIs, !ExpTVarCounts).

    % Do not display for the user any message that says
    % "Argument has type <actual type>, expected type was <distinct typevar>",
    % since such mismatches are NOT type errors.
    %
:- pred hide_categorized_mismatch_info(map(tvar, int)::in,
    categorized_mismatch_info::in) is semidet.

hide_categorized_mismatch_info(ExpTVarCounts, CatMI) :-
    CatMI = categorized_mismatch_info(_ArgNum, _Var, MismatchCategory,
        _SpecialMismatches),
    MismatchCategory = one_expected(MaybeExpTVar, _ExpPieces, _ActPieces),
    MaybeExpTVar = yes(ExpTVar),
    map.lookup(ExpTVarCounts, ExpTVar, Count),
    Count < 2.

:- func mismatched_args_to_pieces(prog_varset, cons_id, is_first,
    list(categorized_mismatch_info)) = list(format_piece).

mismatched_args_to_pieces(_, _, _, []) = [].
mismatched_args_to_pieces(VarSet, Functor, First, [Mismatch | Mismatches])
        = Pieces :-
    Mismatch = categorized_mismatch_info(ArgNum, Var,
        MismatchCategory, SpecialMismatches),
    ( if
        % Handle higher-order syntax such as ''(F, A) specially:
        % output
        %   Functor (F) has type ...;
        %   argument 1 (A) has type ...
        % instead of
        %   Argument 1 (F) has type ...;
        %   argument 2 (A) has type ...
        Functor = du_data_ctor(du_ctor(unqualified(""), Arity, _)),
        Arity > 0
    then
        (
            First = is_first,
            ArgNumPieces = [fixed("Functor")]
        ;
            First = is_not_first,
            ArgNumPieces = [fixed("Argument"), int_fixed(ArgNum - 1)]
        )
    else
        ArgNumPieces = [fixed("Argument"), int_fixed(ArgNum)]
    ),
    ( if varset.search_name(VarSet, Var, _) then
        VarName = mercury_var_to_name_only_vs(VarSet, Var),
        VarNamePieces = [prefix("("), words(VarName), suffix(")")]
    else
        VarNamePieces = []
    ),
    (
        MismatchCategory =
            one_expected(_ExpType, ExpectedPieces, OoMActualPieces),
        OoMActualPieces = one_or_more(HeadActualPieces, TailActualPieces),
        ExpectedDotPieces = ExpectedPieces ++ [suffix(".")],
        (
            TailActualPieces = [],
            ActualCommaPieces = HeadActualPieces ++ [suffix(",")],
            ErrorDescPieces = [words("has type"), nl_indent_delta(1)] ++
                color_as_incorrect(ActualCommaPieces) ++
                    [nl_indent_delta(-1)] ++
                [words("expected type was"), nl_indent_delta(1)] ++
                color_as_correct(ExpectedDotPieces) ++
                    [nl_indent_delta(-1)]
        ;
            TailActualPieces =
                [SecondActualPieces | ThirdPlusActualPieces],
            ActualCommaPieces =
                report_actual_types(HeadActualPieces,
                    SecondActualPieces, ThirdPlusActualPieces) ++
                [suffix(",")],
            ErrorDescPieces = [words("has type"), nl_indent_delta(1)] ++
                color_as_incorrect(ActualCommaPieces) ++
                    [nl_indent_delta(-1)] ++
                [words("expected type was"), nl_indent_delta(1)] ++
                color_as_correct(ExpectedDotPieces) ++
                    [nl_indent_delta(-1)]
        )
    ;
        MismatchCategory =
            several_expected(HeadTypeMismatch, TailTypeMismatches),
        AllMismatches = [HeadTypeMismatch | TailTypeMismatches],
        ErrorDescPieces =
            [words("has one of the following type mismatches."), nl] ++
            report_possible_expected_actual_types(1, AllMismatches)
    ),
    SpecialReasonPieces = report_special_type_mismatches(SpecialMismatches),

    ThisMismatchPieces = color_as_subject(ArgNumPieces ++ VarNamePieces) ++
        ErrorDescPieces ++ SpecialReasonPieces,

    (
        Mismatches = [],
        FollowingMismatchPieces = []
    ;
        Mismatches = [_ | _],
        FollowingMismatchPieces = mismatched_args_to_pieces(VarSet,
            Functor, is_not_first, Mismatches)
    ),
    Pieces = ThisMismatchPieces ++ FollowingMismatchPieces.

:- pred expected_types_all_same_return_actuals(list(format_piece)::in,
    list(type_mismatch)::in, list(list(format_piece))::out) is semidet.

expected_types_all_same_return_actuals(_ExpTypePieces, [], []).
expected_types_all_same_return_actuals(ExpTypePieces,
        [HeadMismatch | TailMismatches],
        [HeadActualTypePieces | TailActualTypePieces]) :-
    HeadMismatch = type_mismatch_exp_act(_ExpType,
        HeadExpTypePieces, HeadActualTypePieces,
        _ActualSubsumesExpected, _MaybeSpecial),
    ExpTypePieces = HeadExpTypePieces,
    expected_types_all_same_return_actuals(ExpTypePieces,
        TailMismatches, TailActualTypePieces).

:- func report_actual_types(list(format_piece),
    list(format_piece), list(list(format_piece))) =
    list(format_piece).

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
    list(format_piece).

report_possible_expected_actual_types(_CurPossNum, []) = [].
report_possible_expected_actual_types(CurPossNum, [Mismatch | Mismatches])
        = Pieces :-
    Mismatch = type_mismatch_exp_act(_ExpType,
        ExpectedTypePieces, ActualTypePieces,
        _ActualSubsumesExpected, _MaybeSpecial),
    HeadPieces =
        [words("Possibility"), int_fixed(CurPossNum), suffix(":")] ++
        [words("actual type")] ++
            color_as_incorrect(ActualTypePieces ++ [suffix(",")]) ++
        [words("expected type")] ++
            color_as_correct(ExpectedTypePieces ++ [suffix("."), nl]),
    TailPieces = report_possible_expected_actual_types(CurPossNum + 1,
        Mismatches),
    Pieces = HeadPieces ++ TailPieces.

%---------------------%

:- pred gather_special_type_mismatches(list(type_mismatch)::in,
    set(type_mismatch_special)::out) is det.

gather_special_type_mismatches([], set.init).
gather_special_type_mismatches([Mismatch | Mismatches], !:Specials) :-
    gather_special_type_mismatches(Mismatches, !:Specials),
    Mismatch = type_mismatch_exp_act(_, _, _, _, MaybeSpecial),
    (
        MaybeSpecial = no
    ;
        MaybeSpecial = yes(Special),
        set.insert(Special, !Specials)
    ).

:- func report_special_type_mismatches(set(type_mismatch_special))
    = list(format_piece).

report_special_type_mismatches(Specials) = Pieces :-
    report_special_type_mismatches_loop(is_first,
        set.to_sorted_list(Specials), Pieces).

:- pred report_special_type_mismatches_loop(is_first::in,
    list(type_mismatch_special)::in, list(format_piece)::out) is det.

report_special_type_mismatches_loop(_IsFirst, [], []).
report_special_type_mismatches_loop(IsFirst, [HeadSpecial | TailSpecials],
        Pieces) :-
    report_special_type_mismatches_loop(is_not_first, TailSpecials,
        TailPieces),
    HeadPieces = report_special_type_mismatch(IsFirst, HeadSpecial),
    Pieces = HeadPieces ++ TailPieces.

:- func report_special_type_mismatch(is_first, type_mismatch_special)
    = list(format_piece).

report_special_type_mismatch(IsFirst, MismatchSpecial) = Pieces :-
    (
        IsFirst = is_first,
        ReasonIsPieces =
            [words("One possible reason for the error is that")]
    ;
        IsFirst = is_not_first,
        ReasonIsPieces =
            [words("Another possible reason for the error is that")]
    ),
    (
        MismatchSpecial = type_mismatch_special_getopt_error(GetoptModule),
        Pieces = ReasonIsPieces ++
            [words("the signatures of the option processing predicates"),
            words("in the"), quote(GetoptModule), words("module")] ++
            color_as_hint([words("have changed recently.")]) ++
            [words("Errors are now returned in a structured form,"),
            words("which can be converted to a string by calling the"),
            quote("option_error_to_string"), words("function."), nl]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

report_error_var_has_wrong_type(Info, GoalContext, Context, Var, Type,
        TypeAssignSet) = SpecAndMaybeActualExpected :-
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    GoalContextPieces = goal_context_to_pieces(ClauseContext, GoalContext),

    get_inst_varset(ClauseContext, InstVarSet),
    get_all_transformed_type_stuffs(
        type_stuff_to_actual_expected(do_not_add_quotes, InstVarSet, Type),
        TypeAssignSet, Var, ActualExpectedList0),
    list.sort_and_remove_dups(ActualExpectedList0, ActualExpectedList),
    report_actual_expected_types(ClauseContext, Var, ActualExpectedList,
        MaybeActualExpected, ActualExpectedPieces, DiffPieces),

    typecheck_info_get_nosuffix_integer_vars(Info, SetOfNoSuffixIntegerVars),
    ( if
        set_tree234.contains(SetOfNoSuffixIntegerVars, Var),
        expected_type_needs_int_constant_suffix(Type)
    then
        NoSuffixIntegerPieces = nosuffix_integer_pieces
    else
        NoSuffixIntegerPieces = []
    ),

    VarSet = ClauseContext ^ tecc_varset,
    type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent),
    Msg = simple_msg(Context,
        [always(InClauseForPieces), always(GoalContextPieces),
        always(ActualExpectedPieces), always(DiffPieces),
        always(NoSuffixIntegerPieces), VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]),
    SpecAndMaybeActualExpected =
        spec_and_maybe_actual_expected(Spec, MaybeActualExpected).

%---------------------------------------------------------------------------%

report_error_var_has_wrong_type_arg(Info, GoalContext, Context,
        ArgNum, Var, ArgTypeAssignSet) = Spec :-
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    GoalContextPieces = goal_context_to_pieces(ClauseContext, GoalContext),

    get_inst_varset(ClauseContext, InstVarSet),
    get_arg_type_stuffs(ArgNum, Var, ArgTypeAssignSet, ArgTypeStuffList),
    ActualExpectedList0 = list.map(
        arg_type_stuff_to_actual_expected(do_not_add_quotes, InstVarSet),
        ArgTypeStuffList),
    list.sort_and_remove_dups(ActualExpectedList0, ActualExpectedList),
    report_actual_expected_types(ClauseContext, Var, ActualExpectedList,
        _MaybeActualExpected, ActualExpectedPieces, DiffPieces),

    VarSet = ClauseContext ^ tecc_varset,
    arg_type_assign_set_msg_to_verbose_component(Info, VarSet,
        ArgTypeAssignSet, VerboseComponent),
    Msg = simple_msg(Context,
        [always(InClauseForPieces), always(GoalContextPieces),
        always(ActualExpectedPieces), always(DiffPieces),
        VerboseComponent]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

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
%---------------------------------------------------------------------------%
%
% The rest of this module contains utility predicates and functions
% for use by the code above.
%

:- pred expected_type_needs_int_constant_suffix(mer_type::in) is semidet.

expected_type_needs_int_constant_suffix(Type) :-
    Type = builtin_type(BuiltinType),
    BuiltinType = builtin_type_int(BuiltinTypeInt),
    BuiltinTypeInt \= int_type_int.

:- func nosuffix_integer_pieces = list(format_piece).

nosuffix_integer_pieces = Pieces :-
    Pieces = [words("A integer constant that consists only of digits is")] ++
        color_as_hint([words("always of type"), quote("int"), suffix(".")]) ++
        [words("Unsigned integer constants of the default size"),
        words("should have the suffix"), quote("u"), suffix(";"),
        words("constants of sized integer types should have"),
        words("an"), quote("i8"), suffix(","), quote("i16"), suffix(","),
        quote("i32"), words("or"), quote("i64"), words("suffix"),
        words("if they are signed, and"),
        words("an"), quote("u8"), suffix(","), quote("u16"), suffix(","),
        quote("u32"), words("or"), quote("u64"), words("suffix"),
        words("if they are unsigned."), nl].

%---------------------------------------------------------------------------%

    % Is the given function symbol the name of a function that is defined
    % (or could/should be defined) in each of int.m, uint.m,
    % and intN.m/uintN.m for N = {8,16,32,64}?
    %
:- pred is_int_func_op(cons_id::in) is semidet.

is_int_func_op(ConsId) :-
    ConsId = du_data_ctor(DuCtor),
    DuCtor = du_ctor(SymName, Arity, _TypeCtor),
    % We ignore the module name part of SymName, since it is very likely
    % that the error is precisely the fact that the module name is wrong.
    Name = unqualify_name(SymName),
    % This table lists the user arities of the named functions.
    ( Name = "abs",                     Arity = 1
    ; Name = "unchecked_abs",           Arity = 1
    ; Name = "nabs",                    Arity = 1

    ; Name = "max",                     Arity = 2
    ; Name = "min",                     Arity = 2

    ; Name = "+",                       (Arity = 1; Arity = 2)
    ; Name = "plus",                    Arity = 2
    ; Name = "-",                       (Arity = 1; Arity = 2)
    ; Name = "minus",                   Arity = 2
    ; Name = "*",                       Arity = 2
    ; Name = "times",                   Arity = 2
    ; Name = "/",                       Arity = 2
    ; Name = "//",                      Arity = 2
    ; Name = "div",                     Arity = 2
    ; Name = "unchecked_quotient",      Arity = 2
    ; Name = "mod",                     Arity = 2
    ; Name = "rem",                     Arity = 2
    ; Name = "unchecked_rem",           Arity = 2

    ; Name = "pow",                     Arity = 2
    ; Name = "log2",                    Arity = 1

    ; Name = "<<",                      Arity = 2
    ; Name = "unchecked_left_shift",    Arity = 2
    ; Name = ">>",                      Arity = 2
    ; Name = "unchecked_right_shift",   Arity = 2

    ; Name = "\\",                      Arity = 1
    ; Name = "/\\",                     Arity = 2
    ; Name = "\\/",                     Arity = 2
    ; Name = "xor",                     Arity = 2
    ).

    % Is the given string the name of a predicate that is defined
    % in each of int.m, uint.m, intN.m and uintN.m for N = {8,16,32,64}?
    %
:- pred is_int_pred_op(sym_name::in, pred_form_arity::in) is semidet.

is_int_pred_op(SymName, PredFormArity) :-
    % We ignore the module name part of SymName, since it is very likely
    % that the error is precisely the fact that the module name is wrong.
    Name = unqualify_name(SymName),
    PredFormArity = pred_form_arity(Arity),
    ( Name = "<",                       Arity = 2
    ; Name = ">",                       Arity = 2
    ; Name = "=<",                      Arity = 2
    ; Name = ">=",                      Arity = 2

    ; Name = "abs",                     Arity = 2

    ; Name = "max",                     Arity = 3
    ; Name = "min",                     Arity = 3

    ; Name = "pow",                     Arity = 3
    ; Name = "log2",                    Arity = 2
    ).

:- pred acc_builtin_types_of_var(list(type_assign)::in, prog_var::in,
    set(builtin_type)::in, set(builtin_type)::out) is det.

acc_builtin_types_of_var(TypeAssignSet, Var, !BuiltinTypes) :-
    get_all_type_stuffs_remove_dups(TypeAssignSet, Var, VarTypeStuffs),
    TypesOfVar = list.map(typestuff_to_type, VarTypeStuffs),
    list.foldl(acc_builtin_type, TypesOfVar, !BuiltinTypes).

:- pred acc_builtin_types_in_cons_type_infos(list(cons_type_info)::in,
    set(builtin_type)::in, set(builtin_type)::out) is det.

acc_builtin_types_in_cons_type_infos([], !BuiltinTypes).
acc_builtin_types_in_cons_type_infos([ConsTypeInfo | ConsTypeInfos],
        !BuiltinTypes) :-
    ConsTypeInfo = cons_type_info(_VarSet, _ExistQTVars, ResultType, ArgTypes,
        _Constraints, _Source),
    acc_builtin_type(ResultType, !BuiltinTypes),
    list.foldl(acc_builtin_type, ArgTypes, !BuiltinTypes),
    acc_builtin_types_in_cons_type_infos(ConsTypeInfos, !BuiltinTypes).

:- pred acc_builtin_types_of_arg_vector_type_error(arg_vector_type_error::in,
    set(builtin_type)::in, set(builtin_type)::out) is det.

acc_builtin_types_of_arg_vector_type_error(Error, !BuiltinTypes) :-
    Error = arg_vector_type_error(_ArgNum, _Var, ActualExpected),
    ActualExpected = actual_expected_types(_ActualPieces, ActualType,
        _ExpectedPieces, ExpectedType, _ExistQTVars, _Source),
    acc_builtin_type(ActualType, !BuiltinTypes),
    acc_builtin_type(ExpectedType, !BuiltinTypes).

:- pred acc_builtin_type(mer_type::in,
    set(builtin_type)::in, set(builtin_type)::out) is det.

acc_builtin_type(Type, !BuiltinTypes) :-
    (
        Type = builtin_type(BuiltinType),
        set.insert(BuiltinType, !BuiltinTypes)
    ;
        ( Type = type_variable(_, _)
        ; Type = defined_type(_, _, _)
        ; Type = tuple_type(_, _)
        ; Type = higher_order_type(_, _, _, _)
        ; Type = apply_n_type(_, _, _)
        ; Type = kinded_type(_, _)
        )
    ).

:- func report_any_invisible_int_types(type_error_clause_context,
    set(builtin_type)) = list(format_piece).

report_any_invisible_int_types(ClauseContext, BuiltinTypes) = Pieces :-
    set.filter_map(
        (pred(builtin_type_int(IntType)::in, IntType::out) is semidet),
        BuiltinTypes, IntTypes),
    ( if
        set.is_non_empty(IntTypes),
        ModuleInfo = ClauseContext ^ tecc_module_info,
        module_info_get_visible_modules(ModuleInfo, VisModules),
        set.filter_map(is_int_n_module, VisModules, VisIntTypes),
        set.difference(IntTypes, VisIntTypes, InvisIntTypes),
        set.to_sorted_list(InvisIntTypes, InvisIntTypesList),
        % Is there at least one integer type whose module we did not import?
        InvisIntTypesList = [HeadInvisIntType | TailInvisIntTypes]
    then
        % XXX The pieces we return denote the names of both types and modules.
        % Should we quote the type names? The module names? Currently,
        % we quote both, which is consistent, but that very consistency
        % makes it harder for readers to differentiate the two roles
        % of each name.
        IntTypeToModulePiece =
            ( func(IT) = quote(Str) :- int_type_module_name(IT, Str) ),
        HeadInvisIntTypePiece = IntTypeToModulePiece(HeadInvisIntType),
        TailInvisIntTypePieces =
            list.map(IntTypeToModulePiece, TailInvisIntTypes),
        (
            TailInvisIntTypePieces = [],
            Pieces = [words("Note that operations on values of type"),
                HeadInvisIntTypePiece, words("are available"),
                words("only if module")] ++
                color_as_hint([HeadInvisIntTypePiece,
                    words("is imported.")]) ++
                [nl]
        ;
            TailInvisIntTypePieces = [_ | _],
            InvisIntTypePieces =
                [HeadInvisIntTypePiece | TailInvisIntTypePieces],
            InvisIntTypeListPieces =
                piece_list_to_color_pieces(color_hint, "and", [],
                    InvisIntTypePieces),
            Pieces = [words("Note that operations on values of types") |
                InvisIntTypePieces] ++ [words("are available"),
                words("only if modules")] ++
                InvisIntTypeListPieces ++
                color_as_hint([words("are imported.")]) ++
                [nl]
        )
    else
        Pieces = []
    ).

:- pred is_int_n_module(module_name::in, int_type::out) is semidet.

is_int_n_module(ModuleSymName, IntType) :-
    ModuleSymName = unqualified(ModuleName),
    int_type_module_name(IntType, ModuleName).

%---------------------------------------------------------------------------%

:- func types_of_vars_to_pieces(prog_varset, inst_varset, maybe(color_name),
    type_assign_set, list(format_piece), prog_var, list(prog_var))
    = list(format_piece).

types_of_vars_to_pieces(VarSet, InstVarSet, MaybeColor, TypeAssignSet,
        FinalPieces, HeadVar, TailVars) = Pieces :-
    (
        TailVars = [],
        Pieces =
            argument_name_to_pieces_lc(VarSet, lcw_none, HeadVar) ++
            type_of_var_to_pieces(InstVarSet, MaybeColor, TypeAssignSet,
                FinalPieces, HeadVar)
    ;
        TailVars = [HeadTailVar | TailTailVars],
        Pieces =
            argument_name_to_pieces_lc(VarSet, lcw_none, HeadVar) ++
            type_of_var_to_pieces(InstVarSet, MaybeColor, TypeAssignSet,
                [suffix(","), nl], HeadVar) ++
            types_of_vars_to_pieces(VarSet, InstVarSet, MaybeColor,
                TypeAssignSet, FinalPieces, HeadTailVar, TailTailVars)
    ).

:- func argument_name_to_pieces_lc(prog_varset, last_context_word, prog_var)
    = list(format_piece).

argument_name_to_pieces_lc(VarSet, LastContextWord, Var) = Pieces :-
    ( if varset.search_name(VarSet, Var, _) then
        Pieces = [words("variable"),
            quote(mercury_var_to_name_only_vs(VarSet, Var))]
    else
        (
            ( LastContextWord = lcw_none
            ; LastContextWord = lcw_call
            ; LastContextWord = lcw_argument
            ),
            Pieces = [words("argument")]
        ;
            LastContextWord = lcw_result,
            Pieces = [words("result")]
        ;
            LastContextWord = lcw_element,
            Pieces = [words("element")]
        )
    ).

:- func argument_name_to_pieces_uc(prog_varset, last_context_word, prog_var)
    = list(format_piece).

argument_name_to_pieces_uc(VarSet, LastContextWord, Var) = Pieces :-
    ( if varset.search_name(VarSet, Var, _) then
        Pieces = [words("Variable"),
            quote(mercury_var_to_name_only_vs(VarSet, Var))]
    else
        (
            ( LastContextWord = lcw_none
            ; LastContextWord = lcw_call
            ; LastContextWord = lcw_argument
            ),
            Pieces = [words("Argument")]
        ;
            LastContextWord = lcw_result,
            Pieces = [words("Result")]
        ;
            LastContextWord = lcw_element,
            Pieces = [words("Element")]
        )
    ).

:- func functor_name_to_pieces(cons_id, arity) = list(format_piece).

functor_name_to_pieces(ConsId, Arity) = Pieces :-
    strip_builtin_qualifier_from_cons_id(ConsId, StrippedConsId),
    ( if Arity = 0 then
        Piece1 = words("constant"),
        ( if ConsId = du_data_ctor(du_ctor(SymName, _, _)) then
            Piece2 = qual_sym_name(SymName)
        else
            Piece2 = quote(cons_id_and_arity_to_string(StrippedConsId))
        ),
        Pieces = [Piece1, Piece2]
    else if ConsId = du_data_ctor(du_ctor(unqualified(""), _, _)) then
        Pieces = [words("higher-order term (with arity"),
            int_fixed(Arity - 1), suffix(")")]
    else
        Pieces = [words("functor"), qual_cons_id_and_maybe_arity(ConsId)]
    ).

:- func type_of_var_to_pieces(inst_varset, maybe(color_name), type_assign_set,
    list(format_piece), prog_var) = list(format_piece).

type_of_var_to_pieces(InstVarSet, MaybeColor, TypeAssignSet, SuffixPieces, Var)
        = Pieces :-
    get_all_transformed_type_stuffs(
        typestuff_to_pieces(do_not_add_quotes, InstVarSet),
        TypeAssignSet, Var, TypePiecesLists0),
    list.sort_and_remove_dups(TypePiecesLists0, TypePiecesLists),
    ( if TypePiecesLists = [_TypePieces] then
        Pieces = [words("has type"), nl_indent_delta(1)] ++
            maybe_color_pieces(MaybeColor,
                pieces_list_to_line_pieces(TypePiecesLists) ++
                SuffixPieces) ++
            [nl_indent_delta(-1)]
    else
        Pieces = [words("has overloaded type {"), nl_indent_delta(1)] ++
            maybe_color_pieces(MaybeColor,
                pieces_list_to_line_pieces(TypePiecesLists)) ++
                [nl_indent_delta(-1)] ++
            [words("}")] ++ SuffixPieces
    ).

:- func type_of_functor_to_pieces(inst_varset, maybe(color_name),
    cons_id, arity, list(cons_type_info), list(format_piece))
    = list(format_piece).

type_of_functor_to_pieces(InstVarSet, MaybeColor, Functor, Arity, ConsDefnList,
        SuffixPieces) = Pieces :-
    ( if ConsDefnList = [SingleDefn] then
        ConsTypePieces = cons_type_to_pieces(InstVarSet, SingleDefn, Functor),
        Pieces =
            [words("has type"),
                nl_indent_delta(1)] ++
            maybe_color_pieces(MaybeColor, ConsTypePieces ++ SuffixPieces) ++
                [nl_indent_delta(-1)]
    else
        ConsTypeListPieces =
            cons_type_list_to_pieces(InstVarSet, ConsDefnList, Functor, Arity),
        Pieces =
            [words("has overloaded type {"),
                nl_indent_delta(1)] ++
            maybe_color_pieces(MaybeColor, ConsTypeListPieces) ++
                [nl_indent_delta(-1)] ++
            [fixed("}")] ++ SuffixPieces ++ [nl]
    ).

    % Return a description of the given data constructor's argument types.
    %
    % The caller should ensure that these pieces are indented one or two levels
    % to separate them from surrounding material.
    %
:- func cons_type_to_pieces(inst_varset, cons_type_info, cons_id)
    = list(format_piece).

cons_type_to_pieces(InstVarSet, ConsInfo, Functor) = Pieces :-
    ConsInfo = cons_type_info(TVarSet, ExistQVars, ConsType, ArgTypes, _, _),
    (
        ArgTypes = [_ | _],
        ( if Functor = du_data_ctor(du_ctor(SymName, _Arity, _)) then
            % What we construct in Type is not really a type: it is a
            % function symbol applied to a list of argument types. However
            % *syntactically*, it looks like a type, and we already have
            % code to print types, so we take a shortcut.
            Type = defined_type(SymName, ArgTypes, kind_star),
            ArgPieces =
                type_to_pieces(TVarSet, InstVarSet,
                    print_name_only, do_not_add_quotes, ExistQVars, Type) ++
                [suffix(":")]
        else
            unexpected($pred, "invalid cons_id")
        )
    ;
        ArgTypes = [],
        ArgPieces = []
    ),
    Pieces = ArgPieces ++
        type_to_pieces(TVarSet, InstVarSet, print_name_only, do_not_add_quotes,
            ExistQVars, ConsType).

    % Return a description of the argument types of the given list of
    % data constructors.
    %
    % The caller should ensure that these pieces are indented one or two levels
    % to separate them from surrounding material.
    %
:- func cons_type_list_to_pieces(inst_varset, list(cons_type_info), cons_id,
    int) = list(format_piece).

cons_type_list_to_pieces(_, [], _, _) = [].
cons_type_list_to_pieces(InstVarSet, [ConsDefn | ConsDefns], Functor, Arity)
        = Pieces :-
    ThisPieces = cons_type_to_pieces(InstVarSet, ConsDefn, Functor),
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
        TailPieces = cons_type_list_to_pieces(InstVarSet, ConsDefns, Functor,
            Arity),
        Pieces = ThisPieces ++ ConnectPieces ++ TailPieces
    ).

%---------------------------------------------------------------------------%

:- pred report_actual_expected_types(type_error_clause_context::in,
    prog_var::in, list(actual_expected_types)::in,
    maybe(actual_expected_types)::out,
    list(format_piece)::out, list(format_piece)::out) is det.

report_actual_expected_types(ClauseContext, Var, ActualExpectedList,
        MaybeActualExpected, ActualExpectedPieces, DiffPieces) :-
    VarSet = ClauseContext ^ tecc_varset,
    TypeErrorPieces = [words("type error:")] ++
        color_as_subject(argument_name_to_pieces_lc(VarSet, lcw_none, Var)),
    is_actual_or_expected_single_type(ActualExpectedList,
        MaybeSingleActual, MaybeSingleExpected),
    % XXX TYPECHECK_ERRORS If both MaybeSingles are yes(), then
    % we could try to print the diff of the two types, though that would
    % probably help only if their outermost type_ctors are different.
    (
        MaybeSingleActual = yes(SingleActualPieces),
        % Technically, it is a *semi*colon, but ...
        ActualColonPieces0 = SingleActualPieces ++ [suffix(";")],
        ActualColonPieces = color_as_incorrect(ActualColonPieces0),
        ActualPartPieces =
            [words("has type"), nl_indent_delta(1)] ++
            ActualColonPieces ++ [nl_indent_delta(-1)]
    ;
        MaybeSingleActual = no,
        ActualPieceLists = list.map((func(AE) = AE ^ actual_type_pieces),
            ActualExpectedList),
        ActualColonPieces0 = pieces_list_to_line_pieces(ActualPieceLists) ++
            [suffix(";")],
        ActualColonPieces = color_as_incorrect(ActualColonPieces0),
        ActualPartPieces =
            [words("has one of the following inferred types:"),
                nl_indent_delta(1)] ++
            ActualColonPieces ++
                [nl_indent_delta(-1)]
    ),
    (
        MaybeSingleExpected = yes(SingleExpectedPieces),
        ExpectedDotPieces0 = SingleExpectedPieces ++ [suffix(".")],
        ExpectedDotPieces = color_as_correct(ExpectedDotPieces0),
        ExpectedPartPieces =
            [words("expected type was"), nl_indent_delta(1)] ++
            ExpectedDotPieces ++ [nl_indent_delta(-1)]
    ;
        MaybeSingleExpected = no,
        should_we_print_expectation_sources(ActualExpectedList,
            MaybePrintSource),
        (
            MaybePrintSource = do_not_print_expectation_source,
            ExpectedPieceLists = list.map(
                (func(AE) = AE ^ expected_type_pieces),
                ActualExpectedList),
            ExpectedDotPieces0 =
                pieces_list_to_line_pieces(ExpectedPieceLists) ++
                [suffix(".")],
            ExpectedDotPieces = color_as_correct(ExpectedDotPieces0),
            ExpectedPartPieces =
                [words("expected type was one of"), nl_indent_delta(1)] ++
                ExpectedDotPieces ++ [nl_indent_delta(-1)]
        ;
            MaybePrintSource = print_expectation_source,
            ModuleInfo = ClauseContext ^ tecc_module_info,
            acc_expected_type_source_pieces(ModuleInfo, ActualExpectedList,
                _, ExpectedPartPieces)
        )
    ),
    ActualExpectedPieces =
        TypeErrorPieces ++ ActualPartPieces ++ ExpectedPartPieces,
    ( if ActualExpectedList = [ActualExpected] then
        MaybeActualExpected = yes(ActualExpected),
        ActualExpected = actual_expected_types(_ActualPieces, ActualType,
            _ExpectedPieces, ExpectedType, ExistQTVars, _Source),
        DiffPieces = type_diff_pieces([], ExistQTVars,
            ActualType, ExpectedType)
    else
        MaybeActualExpected = no,
        % Printing the diffs derived from two or more elements of
        % ActualExpectedList would be more confusing than helpful.
        DiffPieces = []
    ).

:- pred is_actual_or_expected_single_type(list(actual_expected_types)::in,
    maybe(list(format_piece))::out, maybe(list(format_piece))::out) is det.

is_actual_or_expected_single_type([], no, no).
is_actual_or_expected_single_type([AE | AEs],
        MaybeSingleActual, MaybeSingleExpected) :-
    AE = actual_expected_types(ActualPieces, _, ExpectedPieces, _, _, _),
    is_actual_or_expected_single_type_loop(AEs,
        yes(ActualPieces), MaybeSingleActual,
        yes(ExpectedPieces), MaybeSingleExpected).

:- pred is_actual_or_expected_single_type_loop(list(actual_expected_types)::in,
    maybe(list(format_piece))::in, maybe(list(format_piece))::out,
    maybe(list(format_piece))::in, maybe(list(format_piece))::out) is det.

is_actual_or_expected_single_type_loop([],
        !MaybeSingleActual, !MaybeSingleExpected).
is_actual_or_expected_single_type_loop([AE | AEs],
        !MaybeSingleActual, !MaybeSingleExpected) :-
    AE = actual_expected_types(ActualPieces, _, ExpectedPieces, _, _, _),
    ( if !.MaybeSingleActual = yes(ActualPieces) then
        true
    else
        !:MaybeSingleActual = no
    ),
    ( if !.MaybeSingleExpected = yes(ExpectedPieces) then
        true
    else
        !:MaybeSingleExpected = no
    ),
    is_actual_or_expected_single_type_loop(AEs,
        !MaybeSingleActual, !MaybeSingleExpected).

%---------------------------------------------------------------------------%

:- type maybe_print_expectation_source
    --->    do_not_print_expectation_source
    ;       print_expectation_source.

:- pred should_we_print_expectation_sources(list(actual_expected_types)::in,
    maybe_print_expectation_source::out) is det.

should_we_print_expectation_sources(ActualExpectedList, MaybePrintSource) :-
    % If some elements of ActualExpectedList have substantive sources and
    % some don't, then don't print any of the sources, since doing so
    % would be confusing.
    HasSource =
        ( pred(AE::in) is semidet :-
            MaybeSource = AE ^ expectation_source,
            MaybeSource = yes(Source),
            Source \= atas_ensure_have_a_type
        ),
    ( if list.all_true(HasSource, ActualExpectedList) then
        MaybePrintSource = print_expectation_source
    else
        MaybePrintSource = do_not_print_expectation_source
    ).

:- pred acc_expected_type_source_pieces(module_info::in,
    list(actual_expected_types)::in,
    set(pair(list(format_piece)))::out, list(format_piece)::out) is det.

acc_expected_type_source_pieces(_, [], set.init, []).
acc_expected_type_source_pieces(ModuleInfo,
        [ActualExpected | ActualExpecteds],
        SourceExpectedPairs, TaggedPieces) :-
    acc_expected_type_source_pieces(ModuleInfo, ActualExpecteds,
        TailSourceExpectedPairs, TailTaggedPieces),
    ActualExpected = actual_expected_types(_ActualPieces, _ActualType,
        ExpectedPieces, _ExpectedType, _ExistQTVars, MaybeSource),
    (
        TailTaggedPieces = [],
        CommaOrPeriod = "."
    ;
        TailTaggedPieces = [_ | _],
        CommaOrPeriod = ","
    ),
    (
        MaybeSource = yes(Source),
        SourcePieces = describe_args_type_assign_source(ModuleInfo, Source),
        ExpectedCommaOrDotPieces0 = ExpectedPieces ++ [suffix(CommaOrPeriod)],
        ExpectedCommaOrDotPieces = color_as_correct(ExpectedCommaOrDotPieces0),
        % We add a newline after the "(expected by ...):" text for two reasons:
        %
        % - because SourcePieces is likely to take up a large chunk
        %   of the line anyway, and
        % - because this (or something very similar) is needed to ensure
        %   that the different expected type pieces line up exactly with
        %   (a) the inferred type pieces, and (b) each other.
        HeadTaggedPieces =
            [words("the type expected by") | SourcePieces] ++ [words("is:")] ++
                [nl_indent_delta(1)] ++
            ExpectedCommaOrDotPieces ++
                [nl_indent_delta(-1)]
    ;
        MaybeSource = no,
        % Our caller should invoke this predicate only if all
        % actual_expected_types have a meaningful source.
        unexpected($pred, "MaybeSource = no")
    ),
    % We can't test whether we have printed a SourcePieces/ExpectedPieces
    % pair by testing TailTaggedPieceLists, due to the possibility of false
    % negatives due to a comma vs period mismatch, so we test it directly.
    SourceExpectedPair = SourcePieces - ExpectedPieces,
    ( if set.member(SourceExpectedPair, TailSourceExpectedPairs) then
        SourceExpectedPairs = TailSourceExpectedPairs,
        TaggedPieces = TailTaggedPieces
    else
        set.insert(SourceExpectedPair,
            TailSourceExpectedPairs, SourceExpectedPairs),
        TaggedPieces = HeadTaggedPieces ++ TailTaggedPieces
    ).

%---------------------------------------------------------------------------%

:- func type_diff_pieces(list(format_piece), list(tvar),
    mer_type, mer_type) = list(format_piece).

type_diff_pieces(ContextPieces, ExistQTVars, ActualType0, ExpectedType0)
        = DiffPieces :-
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
            ActualType = defined_type(TypeSymName, ActualArgTypes, _),
            ExpectedType = defined_type(TypeSymName, ExpectedArgTypes, _),
            ActualArgTypes \= ExpectedArgTypes,
            DiffPiecesPrime = arg_type_list_diff_pieces(ContextPieces,
                [words("type constructor"), unqual_sym_name(TypeSymName)],
                ExistQTVars, ActualArgTypes, ExpectedArgTypes)
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
            % There is only one lambda_eval_method, so that field
            % cannot contain a difference.
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
            TypeCtorPieces, ExistQTVars, 1, ActualArgTypes, ExpectedArgTypes)
    else
        ( if ExpectedNumArgs = 1 then
            ArgumentS = "argument"
        else
            ArgumentS = "arguments"
        ),
        CausePieces =
            color_as_incorrect([words("Arity mismatch")]) ++
            [words("for")] ++ TypeCtorPieces ++ [suffix(":"),
            words("expected")] ++
            color_as_inconsistent([int_name(ExpectedNumArgs),
                words(ArgumentS), suffix(",")]) ++
            [words("got")] ++
            color_as_inconsistent([int_name(ActualNumArgs), suffix(".")]),
        DiffPieces = wrap_diff_pieces(ContextPieces, CausePieces)
    ).

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
            color_as_incorrect([words("Predicate vs function mismatch:")]) ++
            [words("expected a")] ++
            color_as_inconsistent([p_or_f(ExpectedPorF), suffix(",")]) ++
            [words("got a")] ++
            color_as_inconsistent([p_or_f(ActualPorF), suffix(".")]),
        add_to_diff_pieces(ContextPieces, ExpActPredFuncCausePieces,
            !DiffPieces)
    ),
    ( if ActualPurity = ExpectedPurity then
        true
    else
        ExpActPurityCausePieces =
            color_as_incorrect([words("Purity mismatch:")]) ++
            [words("expected"), purity_desc_article(ExpectedPurity)] ++
            color_as_inconsistent([purity_desc(ExpectedPurity),
                p_or_f(ExpectedPorF), suffix(",")]) ++
            [words("got"), purity_desc_article(ActualPurity)] ++
            color_as_inconsistent([purity_desc(ActualPurity),
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
            list.length(ActualArgTypes, ActualNumArgTypes),
            list.length(ExpectedArgTypes, ExpectedNumArgTypes),
            % This is guaranteed by the failure of the
            % "ActualArgTypes \= ExpectedNumArgTypes" test above.
            expect(unify(ActualNumArgTypes, ExpectedNumArgTypes), $pred,
                "ActualNumArgTypes != ExpectedNumArgTypes"),
            list.length(ActualArgModes, ActualNumArgModes),
            list.length(ExpectedArgModes, ExpectedNumArgModes),
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
                    color_as_incorrect([words("Determinism mismatch:")]) ++
                    [words("the actual"), p_or_f(ActualPorF), words("has"),
                    words("determinism")] ++
                    color_as_inconsistent([words(ActualDetismStr),
                        suffix(",")]) ++
                    [words("but the expected determinism is")] ++
                    color_as_inconsistent([words(ExpectedDetismStr),
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

    % Return a description of any differences between the given
    % actual and expected argument types. The caller should have ensured
    % that the two lists are the same length.
    %
:- func arg_type_list_diff_pieces_loop(list(format_piece),
    list(format_piece), list(tvar), int, list(mer_type), list(mer_type))
    = list(format_piece).

arg_type_list_diff_pieces_loop(_, _, _, _, [], []) = [].
arg_type_list_diff_pieces_loop(_, _, _, _, [], [_ | _]) = _ :-
    unexpected($pred, "list length mismatch").
arg_type_list_diff_pieces_loop(_, _, _, _, [_ | _], []) = _ :-
    unexpected($pred, "list length mismatch").
arg_type_list_diff_pieces_loop(ContextPieces, TypeCtorPieces,
        ExistQTVars, CurArgNum, [ActualArgType | ActualArgTypes],
        [ExpectedArgType | ExpectedArgTypes]) = DiffPieces :-
    TailDiffPieces = arg_type_list_diff_pieces_loop(ContextPieces,
        TypeCtorPieces, ExistQTVars, CurArgNum + 1,
        ActualArgTypes, ExpectedArgTypes),
    ( if ActualArgType = ExpectedArgType then
        DiffPieces = TailDiffPieces
    else
        ArgContextPieces = [treat_next_as_first | ContextPieces] ++
            [lower_case_next_if_not_first, words("In the"),
            nth_fixed(CurArgNum), words("argument of")] ++
            TypeCtorPieces ++ [suffix(":"), nl],
        HeadDiffPieces = type_diff_pieces(ArgContextPieces, ExistQTVars,
            ActualArgType, ExpectedArgType),
        DiffPieces = HeadDiffPieces ++ TailDiffPieces
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
:- end_module check_hlds.typecheck_errors.
%---------------------------------------------------------------------------%
