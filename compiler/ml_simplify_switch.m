%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001, 2003-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_simplify_switch.m
% Main author: fjh

% This module, which is invoked by the various parts of the MLDS code generator
% that generate switches, converts MLDS switches into computed gotos
% or if-then-else chains.

% We should eventually also handle lookup switches and binary search switches
% here too.

% The choice of which exactly which simplifications will get
% performed depends on the target (e.g. whether it understands
% switches) and the --prefer-switch option.

%-----------------------------------------------------------------------------%

:- module ml_backend.ml_simplify_switch.
:- interface.

:- import_module ml_backend.mlds.
:- import_module ml_backend.ml_code_util.

:- pred ml_simplify_switch(mlds_stmt::in, mlds_context::in,
    statement::out, ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ml_backend.ml_switch_gen.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module std_util.

%-----------------------------------------------------------------------------%

ml_simplify_switch(Stmt0, MLDS_Context, Statement, !Info) :-
    ml_gen_info_get_globals(!.Info, Globals),
    (
        % Convert dense int switches into computed gotos,
        % unless the target prefers switches.

        % Is this an int switch?
        Stmt0 = switch(Type, Rval, Range, Cases, Default),
        is_integral_type(Type),

        % Does the target want us to convert dense int switches
        % into computed gotos?
        target_supports_computed_goto(Globals),
        \+ (
            target_supports_int_switch(Globals),
            globals.lookup_bool_option(Globals, prefer_switch, yes)
        ),

        % Is the switch big enough?
        list.length(Cases, NumCases),
        globals.lookup_int_option(Globals, dense_switch_size, DenseSize),
        NumCases >= DenseSize,

        % ... and dense enough?
        globals.lookup_int_option(Globals, dense_switch_req_density,
            ReqDensity),
        is_dense_switch(Cases, ReqDensity)
    ->
        maybe_eliminate_default(Range, Cases, Default, ReqDensity,
            FirstVal, LastVal, NeedRangeCheck),
        generate_dense_switch(Cases, Default, FirstVal, LastVal,
            NeedRangeCheck, Type, Rval, MLDS_Context,
            Decls, Statements, !Info),
        Stmt = block(Decls, Statements),
        Statement = statement(Stmt, MLDS_Context)
    ;
        % Convert the remaining (sparse) int switches into if-then-else chains,
        % unless the target prefers switches.

        Stmt0 = switch(Type, Rval, _Range, Cases, Default),
        is_integral_type(Type),
        \+ (
            target_supports_int_switch(Globals),
            globals.lookup_bool_option(Globals, prefer_switch, yes)
        )
    ->
        Statement = ml_switch_to_if_else_chain(Cases, Default, Rval,
            MLDS_Context)
    ;
        % Optimize away trivial switches (these can occur e.g. with
        % --tags none, where the tag test always has only one reachable case)

        Stmt0 = switch(_Type, _Rval, _Range, Cases, Default),
        Cases = [SingleCase],
        Default = default_is_unreachable
    ->
        SingleCase = _MatchCondition - CaseStatement,
        Statement = CaseStatement
    ;
        Stmt = Stmt0,
        Statement = statement(Stmt, MLDS_Context)
    ).

:- pred is_integral_type(mlds_type::in) is semidet.

is_integral_type(mlds_native_int_type).
is_integral_type(mlds_native_char_type).
is_integral_type(mercury_type(_, type_cat_int, _)).
is_integral_type(mercury_type(_, type_cat_char, _)).
is_integral_type(mercury_type(_, type_cat_enum, _)).

:- pred is_dense_switch(list(mlds_switch_case)::in, int::in) is semidet.

is_dense_switch(Cases, ReqDensity) :-
    % Need at least two cases
    NumCases = list.length(Cases),
    NumCases > 2,

    % The switch needs to be dense enough.
    find_first_and_last_case(Cases, FirstCaseVal, LastCaseVal),
    CasesRange = LastCaseVal - FirstCaseVal + 1,
    Density = calc_density(NumCases, CasesRange),
    Density > ReqDensity.

    % For switches with a default, we normally need to check that
    % the variable is in range before we index into the jump table.
    % However, if the range of the type is sufficiently small,
    % we can make the jump table large enough to hold all
    % of the values for the type.
    %
:- pred maybe_eliminate_default(mlds_switch_range::in,
    list(mlds_switch_case)::in, mlds_switch_default::in, int::in,
    int::out, int::out, bool::out) is det.

maybe_eliminate_default(Range, Cases, Default, ReqDensity,
        FirstVal, LastVal, NeedRangeCheck) :-
    (
        Default \= default_is_unreachable,
        Range = range(Min, Max),
        TypeRange = Max - Min + 1,
        NumCases = list.length(Cases),
        NoDefaultDensity = calc_density(NumCases, TypeRange),
        NoDefaultDensity > ReqDensity
    ->
        NeedRangeCheck = no,
        FirstVal = Min,
        LastVal = Max
    ;
        ( Default = default_is_unreachable ->
            NeedRangeCheck = no
        ;
            NeedRangeCheck = yes
        ),
        find_first_and_last_case(Cases, FirstCaseVal, LastCaseVal),
        FirstVal = FirstCaseVal,
        LastVal = LastCaseVal
    ).

    % Calculate the percentage density given the range and the number of cases.
    %
:- func calc_density(int, int) = int.

calc_density(NumCases, Range) = Density :-
    Density = (NumCases * 100) // Range.

%-----------------------------------------------------------------------------%

    % Find the highest and lowest case values in a list of cases.
    %
:- pred find_first_and_last_case(list(mlds_switch_case)::in,
    int::out, int::out) is det.

find_first_and_last_case(Cases, Min, Max) :-
    list.foldl2(find_first_and_last_case_2, Cases, 0, Min, 0, Max).

:- pred find_first_and_last_case_2(mlds_switch_case::in,
    int::in, int::out, int::in, int::out) is det.

find_first_and_last_case_2(Case, !Min, !Max) :-
    Case = CaseConds - _CaseStatement,
    list.foldl2(find_first_and_last_case_3, CaseConds, !Min, !Max).

:- pred find_first_and_last_case_3(mlds_case_match_cond::in,
    int::in, int::out, int::in, int::out) is det.

find_first_and_last_case_3(match_value(Rval), !Min, !Max) :-
    (
        Rval = const(int_const(Val))
    ->
        int.min(Val, !Min),
        int.max(Val, !Max)
    ;
        unexpected(this_file, "find_first_and_last_case_3: non-int case")
    ).
find_first_and_last_case_3(match_range(MinRval, MaxRval),
        !Min, !Max) :-
    (
        MinRval = const(int_const(RvalMin)),
        MaxRval = const(int_const(RvalMax))
    ->
        int.min(RvalMin, !Min),
        int.max(RvalMax, !Max)
    ;
        unexpected(this_file, "find_first_and_last_case_3: non-int case")
    ).

%-----------------------------------------------------------------------------%

    % Generate code for a switch using a dense jump table.
    %
:- pred generate_dense_switch(list(mlds_switch_case)::in,
    mlds_switch_default::in, int::in, int::in, bool::in,
    mlds_type::in, mlds_rval::in, mlds_context::in,
    mlds_defns::out, statements::out,
    ml_gen_info::in, ml_gen_info::out) is det.

generate_dense_switch(Cases, Default, FirstVal, LastVal, NeedRangeCheck,
        _Type, Rval, MLDS_Context, Decls, Statements, !Info) :-
    % If the case values start at some number other than 0,
    % then subtract that number to give us a zero-based index.
    ( FirstVal = 0 ->
        Index = Rval
    ;
        Index = binop(int_sub, Rval, const(int_const(FirstVal)))
    ),

    % Now generate the jump table.
    ml_gen_new_label(EndLabel, !Info),
    map.init(CaseLabelsMap0),
    generate_cases(Cases, EndLabel, CaseLabelsMap0,
        CaseLabelsMap, CasesDecls, CasesCode, !Info),
    ml_gen_new_label(DefaultLabel, !Info),
    CaseLabels = get_case_labels(FirstVal, LastVal,
        CaseLabelsMap, DefaultLabel),
    DefaultLabelStatement = statement(label(DefaultLabel), MLDS_Context),
    (
        Default = default_is_unreachable,
        % We still need the label, in case we inserted references to it
        % into (unreachable) slots in the jump table.
        DefaultStatements = [DefaultLabelStatement]
    ;
        Default = default_do_nothing,
        DefaultStatements = [DefaultLabelStatement]
    ;
        Default = default_case(DefaultCase),
        DefaultStatements = [DefaultLabelStatement, DefaultCase]
    ),

    StartComment = statement(
        atomic(comment("switch (using dense jump table)")),
        MLDS_Context),
    DoJump = statement(computed_goto(Index, CaseLabels), MLDS_Context),
    EndLabelStatement = statement(label(EndLabel), MLDS_Context),
    EndComment = statement(atomic(comment("End of dense switch")),
        MLDS_Context),

    % We may need to check that the value of the variable lies within the
    % appropriate range.
    (
        NeedRangeCheck = yes,
        Difference = LastVal - FirstVal,
        InRange = binop(unsigned_le, Index, const(int_const(Difference))),
        Else = yes(statement(block([], DefaultStatements),
            MLDS_Context)),
        SwitchBody = statement(block([], [DoJump | CasesCode]),
            MLDS_Context),
        DoSwitch = statement(if_then_else(InRange, SwitchBody, Else),
            MLDS_Context),
        Statements = [StartComment, DoSwitch] ++
            [EndLabelStatement, EndComment]
    ;
        NeedRangeCheck = no,
        Statements = [StartComment, DoJump | CasesCode] ++
            DefaultStatements ++ [EndLabelStatement, EndComment]
    ),
    Decls = CasesDecls.

:- pred generate_cases(list(mlds_switch_case)::in, mlds_label::in,
    case_labels_map::in, case_labels_map::out,
    mlds_defns::out, statements::out,
    ml_gen_info::in, ml_gen_info::out) is det.

generate_cases([], _EndLabel, CaseLabelsMap, CaseLabelsMap, [], [], !Info).
generate_cases([Case | Cases], EndLabel, CaseLabelsMap0,
        CaseLabelsMap, Decls, Statements, !Info) :-
    generate_case(Case, EndLabel, CaseLabelsMap0, CaseLabelsMap1,
        CaseDecls, CaseStatements, !Info),
    generate_cases(Cases, EndLabel,
        CaseLabelsMap1, CaseLabelsMap,
        Decls1, Statements1, !Info),
    Decls = CaseDecls ++ Decls1,
    Statements = CaseStatements ++ Statements1.

    % This converts an MLDS switch case into code for a dense switch case,
    % by adding a label at the front and a `goto <EndLabel>' at the end.
    % It also inserts the label for this case into the CaseLabelsMap.
    %
:- pred generate_case(mlds_switch_case::in, mlds_label::in,
    case_labels_map::in, case_labels_map::out,
    mlds_defns::out, statements::out,
    ml_gen_info::in, ml_gen_info::out) is det.

generate_case(Case, EndLabel, CaseLabelsMap0, CaseLabelsMap,
        Decls, Statements, !Info) :-
    Case = MatchCondition - CaseStatement,
    ml_gen_new_label(ThisLabel, !Info),
    insert_cases_into_map(MatchCondition, ThisLabel,
        CaseLabelsMap0, CaseLabelsMap),
    CaseStatement = statement(_, MLDS_Context),
    LabelComment = statement(atomic(comment("case of dense switch")),
        MLDS_Context),
    LabelCode = statement(label(ThisLabel), MLDS_Context),
    JumpComment = statement(
        atomic(comment("branch to end of dense switch")),
        MLDS_Context),
    JumpCode = statement(goto(label(EndLabel)), MLDS_Context),
    Decls = [],
    Statements = [LabelComment, LabelCode, CaseStatement,
        JumpComment, JumpCode].

%-----------------------------------------------------------------------------%
%
% We build up a map which records which label should be used for
% each case value.

:- type case_labels_map == map(int, mlds_label).

:- pred insert_cases_into_map(mlds_case_match_conds::in, mlds_label::in,
    case_labels_map::in, case_labels_map::out) is det.

insert_cases_into_map([], _ThisLabel, !CaseLabelsMap).
insert_cases_into_map([Cond|Conds], ThisLabel, !CaseLabelsMap) :-
    insert_case_into_map(Cond, ThisLabel, !CaseLabelsMap),
    insert_cases_into_map(Conds, ThisLabel, !CaseLabelsMap).

:- pred insert_case_into_map(mlds_case_match_cond::in, mlds_label::in,
    case_labels_map::in, case_labels_map::out) is det.

insert_case_into_map(match_value(Rval), ThisLabel, !CaseLabelsMap) :-
    ( Rval = const(int_const(Val)) ->
        map.det_insert(!.CaseLabelsMap, Val, ThisLabel, !:CaseLabelsMap)
    ;
        unexpected(this_file, "insert_case_into_map: non-int case")
    ).
insert_case_into_map(match_range(MinRval, MaxRval), ThisLabel,
        !CaseLabelsMap) :-
    (
        MinRval = const(int_const(Min)),
        MaxRval = const(int_const(Max))
    ->
        insert_range_into_map(Min, Max, ThisLabel, !CaseLabelsMap)
    ;
        unexpected(this_file, "insert_case_into_map: non-int case")
    ).

:- pred insert_range_into_map(int::in, int::in, mlds_label::in,
    case_labels_map::in, case_labels_map::out) is det.

insert_range_into_map(Min, Max, ThisLabel, !CaseLabelsMap) :-
    ( Min > Max ->
        true
    ;
        map.det_insert(!.CaseLabelsMap, Min, ThisLabel, !:CaseLabelsMap),
        insert_range_into_map(Min + 1, Max, ThisLabel, !CaseLabelsMap)
    ).

%-----------------------------------------------------------------------------%

    % Given the starting and ending case values, the mapping from case values
    % to labels, and the default label to use for case values which aren't in
    % the map, this function returns the list of labels to use for the case
    % values.
    %
:- func get_case_labels(int, int, map(int, mlds_label), mlds_label)
    = list(mlds_label).

get_case_labels(ThisVal, LastVal, CaseLabelsMap, DefaultLabel) = CaseLabels :-
    ( ThisVal > LastVal ->
        CaseLabels = []
    ;
        ( map.search(CaseLabelsMap, ThisVal, CaseLabel0) ->
            CaseLabel = CaseLabel0
        ;
            CaseLabel = DefaultLabel
        ),
        CaseLabels1 = get_case_labels(ThisVal + 1, LastVal,
            CaseLabelsMap, DefaultLabel),
        CaseLabels = [CaseLabel | CaseLabels1]
    ).

%-----------------------------------------------------------------------------%

    % Convert an int switch to a chain of if-then-elses that test each case
    % in turn.
    %
:- func ml_switch_to_if_else_chain(mlds_switch_cases, mlds_switch_default,
    mlds_rval, mlds_context) = statement.

ml_switch_to_if_else_chain([], Default, _Rval, MLDS_Context) = Statement :-
    (
        Default = default_do_nothing,
        Statement = statement(block([],[]), MLDS_Context)
    ;
        Default = default_is_unreachable,
        Statement = statement(block([],[]), MLDS_Context)
    ;
        Default = default_case(Statement)
    ).
ml_switch_to_if_else_chain([Case | Cases], Default, SwitchRval, MLDS_Context) =
        Statement :-
    Case = MatchConditions - CaseStatement,
    (
        Cases = [],
        Default = default_is_unreachable
    ->
        Statement = CaseStatement
    ;
        CaseMatchedRval = ml_gen_case_match_conds(MatchConditions, SwitchRval),
        RestStatement = ml_switch_to_if_else_chain(Cases, Default, SwitchRval,
            MLDS_Context),
        IfStmt = if_then_else(CaseMatchedRval, CaseStatement,
            yes(RestStatement)),
        Statement = statement(IfStmt, MLDS_Context)
    ).

    % Generate an rval which will be true iff any of the specified list of
    % case conditions matches the specified rval (which must have integral
    % type).
    %
:- func ml_gen_case_match_conds(mlds_case_match_conds, mlds_rval) = mlds_rval.

ml_gen_case_match_conds([], _) = const(false).
ml_gen_case_match_conds([Cond], SwitchRval) =
    ml_gen_case_match_cond(Cond, SwitchRval).
ml_gen_case_match_conds([Cond1, Cond2 | Conds], SwitchRval) =
    binop(logical_or,
        ml_gen_case_match_cond(Cond1, SwitchRval),
        ml_gen_case_match_conds([Cond2 | Conds], SwitchRval)).

    % Generate an rval which will be true iff the specified case condition
    % matches the specified rval (which must have integral type).
    %
:- func ml_gen_case_match_cond(mlds_case_match_cond, mlds_rval) = mlds_rval.

ml_gen_case_match_cond(match_value(CaseRval), SwitchRval) =
    binop(eq, CaseRval, SwitchRval).
ml_gen_case_match_cond(match_range(MinRval, MaxRval), SwitchRval) =
    binop(logical_and,
        binop(int_gt, SwitchRval, MinRval),
        binop(int_le, SwitchRval, MaxRval)).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "ml_simplify_switch.m".

%-----------------------------------------------------------------------------%
