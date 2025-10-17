%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_error_wrong_type.m.
%
% This file contains predicates to report a specific kind of type error.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_error_wrong_type.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_error_type_assign.
:- import_module check_hlds.typecheck_error_util.
:- import_module check_hlds.typecheck_info.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module maybe.

%---------------------------------------------------------------------------%

    % report_error_var_has_wrong_type uses this type to return
    % not just an error_spec, but possibly also an actual_expected_types
    % structure to its caller, which may then pass that structure on
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

% The two main differences between the next two functions are that
% - the first takes a type_assign_set, and explicitly specifies
%   the expected type, while
% - the second takes an args_type_assign_set, and gets a separate
%   expected type from each args_type_assign in the set.

:- func report_error_var_has_wrong_type(typecheck_info,
    type_error_goal_context, prog_context, prog_var, mer_type, type_assign_set)
    = spec_and_maybe_actual_expected.

:- func report_error_var_has_wrong_type_arg(typecheck_info,
    type_error_goal_context, prog_context, int, prog_var, args_type_assign_set)
    = error_spec.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.typecheck_error_builtin.
:- import_module check_hlds.typecheck_error_diff.
:- import_module hlds.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.error_type_util.

:- import_module list.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

report_error_var_has_wrong_type(Info, GoalContext, Context, Var, ExpectedType,
        TypeAssignSet) = SpecAndMaybeActualExpected :-
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    InClauseForPieces = in_clause_for_pieces(ClauseContext),
    GoalContextPieces = goal_context_to_pieces(ClauseContext, GoalContext),

    get_inst_varset(ClauseContext, InstVarSet),
    get_all_transformed_type_stuffs(
        type_stuff_to_actual_expected(do_not_add_quotes, InstVarSet,
            ExpectedType),
        TypeAssignSet, Var, ActualExpectedList0),
    list.sort_and_remove_dups(ActualExpectedList0, ActualExpectedList),
    report_actual_expected_types(ClauseContext, Var, ActualExpectedList,
        MaybeActualExpected, ActualExpectedPieces, DiffPieces),

    typecheck_info_get_nosuffix_integer_vars(Info, SetOfNoSuffixIntegerVars),
    ( if
        set_tree234.contains(SetOfNoSuffixIntegerVars, Var),
        type_needs_int_constant_suffix(ExpectedType)
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
    print_actual_type_or_types(ActualExpectedList,
        MaybeSingleActual, ActualPartPieces),
    print_expected_type_or_types(ClauseContext, ActualExpectedList,
        MaybeSingleExpected, ExpectedPartPieces),
    ActualExpectedPieces =
        TypeErrorPieces ++ ActualPartPieces ++ ExpectedPartPieces,
    ( if ActualExpectedList = [ActualExpected] then
        MaybeActualExpected = yes(ActualExpected),
        ActualExpected = actual_expected_types(_ActualPieces, ActualType,
            _ExpectedPieces, ExpectedType, ExistQTVars, _Source),
        DiffPieces = type_diff_pieces(ExistQTVars, ActualType, ExpectedType)
    else
        MaybeActualExpected = no,
        % Printing the diffs derived from two or more elements of
        % ActualExpectedList would be more confusing than helpful.
        DiffPieces = []
    ).

%---------------------%

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

%---------------------%

:- pred print_actual_type_or_types(list(actual_expected_types)::in,
    maybe(list(format_piece))::in, list(format_piece)::out) is det.

print_actual_type_or_types(ActualExpectedList, MaybeSingleActual,
        ActualPartPieces) :-
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
    ).

:- pred print_expected_type_or_types(type_error_clause_context::in,
    list(actual_expected_types)::in, maybe(list(format_piece))::in,
    list(format_piece)::out) is det.

print_expected_type_or_types(ClauseContext, ActualExpectedList,
        MaybeSingleExpected, ExpectedPartPieces) :-
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
    ).

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
:- end_module check_hlds.typecheck_error_wrong_type.
%---------------------------------------------------------------------------%
