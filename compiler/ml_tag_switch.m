%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2003-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_tag_switch.m.
% Author: fjh.
%
% Generate switches based on primary and secondary tags, for the MLDS
% back-end.
%
%-----------------------------------------------------------------------------%

:- module ml_backend.ml_tag_switch.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Generate efficient indexing code for tag based switches.
    %
:- pred ml_generate_tag_switch(list(tagged_case)::in, prog_var::in,
    code_model::in, can_fail::in, prog_context::in, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.rtti.
:- import_module backend_libs.switch_util.
:- import_module hlds.hlds_data.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_simplify_switch.
:- import_module ml_backend.ml_switch_gen.
:- import_module ml_backend.ml_unify_gen.
:- import_module ml_backend.ml_util.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module unit.

:- type code_map == map(int, maybe_code).

:- type maybe_code
    --->    immediate(statement)
    ;       generate(hlds_goal).

%-----------------------------------------------------------------------------%

ml_generate_tag_switch(TaggedCases, Var, CodeModel, CanFail, Context,
        Statements, !Info) :-
    % Generate the rval for the primary tag.
    ml_gen_var(!.Info, Var, VarLval),
    VarRval = ml_lval(VarLval),
    PTagRval = ml_unop(std_unop(tag), VarRval),

    % Group the cases based on primary tag value, find out how many
    % constructors share each primary tag value, and sort the cases so that
    % the most frequently occurring primary tag values come first.

    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_variable_type(!.Info, Var, Type),
    get_ptag_counts(Type, ModuleInfo, MaxPrimary, PtagCountMap),
    group_cases_by_ptag(TaggedCases, gen_tagged_case_code(CodeModel),
        map.init, CodeMap, unit, _, !Info, _CaseNumPtagsMap, PtagCaseMap),
    order_ptags_by_count(PtagCountMap, PtagCaseMap, PtagCaseList),
    % The code generation scheme that we use below can duplicate the code of a
    % case if the representations of the cons_ids of that case use more than
    % one primary tag. (We generate one copy of the code for each such primary
    % tag.)
    %
    % If generating code for a target language that does not allow the code of
    % a case to be duplicated, we could adopt a code generation scheme
    % like the following pseudocode:
    %
    % index = primary_tag(Var) * max_secondary_tag_value
    % if primary_tag(Var) indicates that Var has secondary_tag
    %   index += secondary_tag(Var)
    % switch on index using the direct mapped scheme
    %
    % For such targets, you would want to employ this code generation scheme,
    % which is not (yet) implemented, if any value in CaseNumPtagsMap is a set
    % with more than one element.

    % Generate the switch on the primary tag.
    gen_ptag_cases(PtagCaseList, CodeMap, Var, CanFail, CodeModel,
        PtagCountMap, Context, PtagCases0, !Info),
    list.sort(PtagCases0, PtagCases),
    ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info),

    % Package up the results into a switch statement.
    Range = mlds_switch_range(0, MaxPrimary),
    SwitchStmt0 = ml_stmt_switch(mlds_native_int_type, PTagRval, Range,
        PtagCases, Default),
    MLDS_Context = mlds_make_context(Context),
    ml_simplify_switch(SwitchStmt0, MLDS_Context, SwitchStatement, !Info),
    Statements = [SwitchStatement].

:- pred gen_tagged_case_code(code_model::in, tagged_case::in, int::out,
    code_map::in, code_map::out, unit::in, unit::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_tagged_case_code(CodeModel, TaggedCase, CaseNum, !CodeMap, !Unit,
        Info0, Info) :-
    TaggedCase = tagged_case(_MainTaggedConsId, OtherTaggedConsIds,
        CaseNum, Goal),
    ml_gen_goal_as_branch_block(CodeModel, Goal, Statement, Info0, Info1),
    % Do not allow the generated code to be literally duplicated if it contains
    % labels. Rather, we will regenerate the code at every point it is required
    % so that the labels are unique.
    (
        OtherTaggedConsIds = [_ | _],
        statement_contains_label(Statement)
    ->
        MaybeCode = generate(Goal),
        Info = Info0
    ;
        MaybeCode = immediate(Statement),
        Info = Info1
    ),
    map.det_insert(CaseNum, MaybeCode, !CodeMap).

:- pred statement_contains_label(statement::in) is semidet.

statement_contains_label(Statement) :-
    statement_contains_statement(Statement, Label),
    Label = statement(ml_stmt_label(_), _).

:- pred lookup_code_map(code_map::in, int::in, code_model::in, statement::out,
    ml_gen_info::in, ml_gen_info::out) is det.

lookup_code_map(CodeMap, CaseNum, CodeModel, Statement, !Info) :-
    map.lookup(CodeMap, CaseNum, MaybeCode),
    (
        MaybeCode = immediate(Statement)
    ;
        MaybeCode = generate(Goal),
        ml_gen_goal_as_branch_block(CodeModel, Goal, Statement, !Info)
    ).

:- type is_a_case_split_between_ptags
    --->    no_case_is_split_between_ptags
    ;       some_case_is_split_between_ptags.

:- pred find_any_split_cases(case_num_ptags_map::in,
    is_a_case_split_between_ptags::out) is det.

find_any_split_cases(CaseNumPtagsMap, IsAnyCaseSplit) :-
    map.foldl(find_any_split_cases_2, CaseNumPtagsMap,
        no_case_is_split_between_ptags, IsAnyCaseSplit).

:- pred find_any_split_cases_2(int::in, set(int)::in,
    is_a_case_split_between_ptags::in, is_a_case_split_between_ptags::out)
    is det.

find_any_split_cases_2(_CaseNum, Ptags, !IsAnyCaseSplit) :-
    ( set.is_singleton(Ptags, _OnlyPtag) ->
        true
    ;
        !:IsAnyCaseSplit = some_case_is_split_between_ptags
    ).

%-----------------------------------------------------------------------------%

:- pred gen_ptag_cases(ptag_case_group_list(int)::in, code_map::in,
    prog_var::in, can_fail::in, code_model::in, ptag_count_map::in,
    prog_context::in, list(mlds_switch_case)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_ptag_cases([], _, _, _, _, _, _, [], !Info).
gen_ptag_cases([Ptag | Ptags], CodeMap, Var, CanFail, CodeModel,
        PtagCountMap, Context, [MLDS_Case | MLDS_Cases], !Info) :-
    gen_ptag_case(Ptag, CodeMap, Var, CanFail, CodeModel,
        PtagCountMap, Context, MLDS_Case, !Info),
    gen_ptag_cases(Ptags, CodeMap, Var, CanFail, CodeModel,
        PtagCountMap, Context, MLDS_Cases, !Info).

:- pred gen_ptag_case(ptag_case_group_entry(int)::in, code_map::in,
    prog_var::in, can_fail::in, code_model::in, ptag_count_map::in,
    prog_context::in, mlds_switch_case::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_ptag_case(PtagCase, CodeMap, Var, CanFail, CodeModel, PtagCountMap,
        Context, MLDS_Case, !Info) :-
    PtagCase = ptag_case_group_entry(MainPtag, OtherPtags,
        ptag_case(SecTagLocn, GoalMap)),
    map.lookup(PtagCountMap, MainPtag, CountInfo),
    CountInfo = SecTagLocn1 - MaxSecondary,
    expect(unify(SecTagLocn, SecTagLocn1), $module, $pred,
        "secondary tag locations differ"),
    map.to_assoc_list(GoalMap, GoalList),
    (
        ( SecTagLocn = sectag_none
        ; SecTagLocn = sectag_none_direct_arg
        ),
        % There is no secondary tag, so there is no switch on it.
        (
            GoalList = [],
            unexpected($module, $pred, "no goal for non-shared tag")
        ;
            GoalList = [_Stag - CaseNum],
            lookup_code_map(CodeMap, CaseNum, CodeModel, Statement, !Info)
        ;
            GoalList = [_, _ | _],
            unexpected($module, $pred, "more than one goal for non-shared tag")
        )
    ;
        ( SecTagLocn = sectag_local
        ; SecTagLocn = sectag_remote
        ),
        expect(unify(OtherPtags, []), $module, $pred,
            ">1 ptag with secondary tag"),
        (
            CanFail = cannot_fail,
            CaseCanFail = cannot_fail
        ;
            CanFail = can_fail,
            (
                list.length(GoalList, GoalCount),
                FullGoalCount = MaxSecondary + 1,
                FullGoalCount = GoalCount
            ->
                CaseCanFail = cannot_fail
            ;
                CaseCanFail = can_fail
            )
        ),
        group_stag_cases(GoalList, GroupedGoalList),
        (
            GroupedGoalList = [CaseNum - _Stags],
            CaseCanFail = cannot_fail
        ->
            % There is only one possible matching goal, so we don't need
            % to switch on it. This can happen if the other functor symbols
            % that share this primary tag are ruled out by the initial inst
            % of the switched-on variable.
            lookup_code_map(CodeMap, CaseNum, CodeModel, Statement, !Info)
        ;
            gen_stag_switch(GroupedGoalList, CodeMap, MainPtag, SecTagLocn,
                Var, CodeModel, CaseCanFail, Context, Statement, !Info)
        )
    ),
    MainPtagMatch = make_ptag_match(MainPtag),
    OtherPtagMatches = list.map(make_ptag_match, OtherPtags),
    MLDS_Case = mlds_switch_case(MainPtagMatch, OtherPtagMatches, Statement).

:- func make_ptag_match(tag_bits) = mlds_case_match_cond.

make_ptag_match(Ptag) = match_value(ml_const(mlconst_int(Ptag))).

%-----------------------------------------------------------------------------%

    % A list of secondary tag values that all have the same code.
    % The list is stored in reversed form.
    %
:- type stags
    --->    stags(int, list(int)).

    % Maps case numbers (each of which identifies the code of one switch arm)
    % to the secondary tags that share that code.
    %
:- type stag_rev_map == map(int, stags).

:- pred group_stag_cases(stag_goal_list(int)::in,
    assoc_list(int, stags)::out) is det.

group_stag_cases(Goals, GroupedGoals) :-
    build_stag_rev_map(Goals, map.init, RevMap),
    map.to_assoc_list(RevMap, GroupedGoals).

:- pred build_stag_rev_map(stag_goal_list(int)::in,
    stag_rev_map::in, stag_rev_map::out) is det.

build_stag_rev_map([], !RevMap).
build_stag_rev_map([Entry | Entries], !RevMap) :-
    Entry = Stag - CaseNum,
    ( map.search(!.RevMap, CaseNum, OldEntry) ->
        OldEntry = stags(OldFirstStag, OldLaterStags),
        NewEntry = stags(OldFirstStag, [Stag | OldLaterStags]),
        map.det_update(CaseNum, NewEntry, !RevMap)
    ;
        NewEntry = stags(Stag, []),
        map.det_insert(CaseNum, NewEntry, !RevMap)
    ),
    build_stag_rev_map(Entries, !RevMap).

%-----------------------------------------------------------------------------%

:- pred gen_stag_switch(assoc_list(int, stags)::in,
    code_map::in, int::in, sectag_locn::in, prog_var::in,
    code_model::in, can_fail::in, prog_context::in, statement::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_stag_switch(Cases, CodeMap, PrimaryTag, StagLocn, Var, CodeModel,
        CanFail, Context, Statement, !Info) :-
    % Generate the rval for the secondary tag.
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_variable_type(!.Info, Var, VarType),
    ml_gen_var(!.Info, Var, VarLval),
    VarRval = ml_lval(VarLval),
    (
        StagLocn = sectag_local,
        STagRval = ml_unop(std_unop(unmkbody), VarRval)
    ;
        StagLocn = sectag_remote,
        STagRval = ml_gen_secondary_tag_rval(ModuleInfo, PrimaryTag, VarType,
            VarRval)
    ;
        ( StagLocn = sectag_none
        ; StagLocn = sectag_none_direct_arg
        ),
        unexpected($module, $pred, "no stag")
    ),

    % Generate the switch on the secondary tag.
    gen_stag_cases(Cases, CodeMap, CodeModel, StagCases0, !Info),
    list.sort(StagCases0, StagCases),
    ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info),

    % Package up the results into a switch statement.
    Range = mlds_switch_range_unknown, % XXX could do better
    SwitchStmt = ml_stmt_switch(mlds_native_int_type, STagRval, Range,
        StagCases, Default),
    MLDS_Context = mlds_make_context(Context),
    ml_simplify_switch(SwitchStmt, MLDS_Context, Statement, !Info).

%-----------------------------------------------------------------------------%

:- pred gen_stag_cases(assoc_list(int, stags)::in,
    code_map::in, code_model::in,
    list(mlds_switch_case)::out, ml_gen_info::in, ml_gen_info::out) is det.

gen_stag_cases([], _, _, [], !Info).
gen_stag_cases([Group | Groups], CodeMap, CodeModel, [Case | Cases], !Info) :-
    gen_stag_case(Group, CodeMap, CodeModel, Case, !Info),
    gen_stag_cases(Groups, CodeMap, CodeModel, Cases, !Info).

:- pred gen_stag_case(pair(int, stags)::in, code_map::in, code_model::in,
    mlds_switch_case::out, ml_gen_info::in, ml_gen_info::out) is det.

gen_stag_case(Group, CodeMap, CodeModel, MLDS_Case, !Info) :-
    Group = CaseNum - Stags,
    Stags = stags(FirstStag, RevLaterStags),
    list.reverse(RevLaterStags, LaterStags),
    FirstMatch = make_match_value(FirstStag),
    LaterMatches = list.map(make_match_value, LaterStags),
    lookup_code_map(CodeMap, CaseNum, CodeModel, Statement, !Info),
    MLDS_Case = mlds_switch_case(FirstMatch, LaterMatches, Statement).

:- func make_match_value(int) = mlds_case_match_cond.

make_match_value(Stag) = match_value(ml_const(mlconst_int(Stag))).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.ml_tag_switch.
%-----------------------------------------------------------------------------%
