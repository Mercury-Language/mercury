%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2003-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_tag_switch.m.
% Author: fjh.
%
% Generate switches based on primary and secondary tags, for the MLDS
% back-end.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_tag_switch.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % Generate efficient indexing code for tag based switches.
    %
:- pred ml_generate_tag_switch(list(tagged_case)::in, prog_var::in,
    code_model::in, can_fail::in, packed_args_map::in, prog_context::in,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.rtti.
:- import_module backend_libs.switch_util.
:- import_module hlds.hlds_data.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_simplify_switch.
:- import_module ml_backend.ml_switch_gen.
:- import_module ml_backend.ml_unify_gen_util.
:- import_module ml_backend.ml_util.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module uint8.
:- import_module unit.

:- type code_map == map(case_id, maybe_code).

:- type maybe_code
    --->    immediate(mlds_stmt)
    ;       generate(packed_args_map, hlds_goal).

%---------------------------------------------------------------------------%

ml_generate_tag_switch(TaggedCases, Var, CodeModel, CanFail,
        EntryPackedArgsMap, Context, Stmts, !Info) :-
    % Generate the rval for the primary tag.
    ml_gen_var(!.Info, Var, VarLval),
    VarRval = ml_lval(VarLval),
    PtagRval = ml_unop(tag, VarRval),

    % Group the cases based on primary tag value, find out how many
    % constructors share each primary tag value, and sort the cases so that
    % the most frequently occurring primary tag values come first.

    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_variable_type(!.Info, Var, Type),
    get_ptag_counts(Type, ModuleInfo, MaxPrimary, PtagCountMap),
    group_cases_by_ptag(TaggedCases,
        gen_tagged_case_code(CodeModel, EntryPackedArgsMap),
        map.init, CodeMap, unit, _, !Info, _CaseIdPtagsMap, PtagCaseMap),
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
    % which is not (yet) implemented, if any value in CaseIdPtagsMap is a set
    % with more than one element. One could test for that condition with the
    % (currently unused) find_any_split_cases predicate.

    % Generate the switch on the primary tag.
    gen_ptag_cases(PtagCaseList, CodeMap, Var, CanFail, CodeModel,
        PtagCountMap, Context, PtagCases0, !Info),
    list.sort(PtagCases0, PtagCases),
    ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info),

    % Package up the results into a switch statement.
    Range = mlds_switch_range(0, uint8.cast_to_int(MaxPrimary)),
    SwitchStmt0 = ml_stmt_switch(mlds_native_int_type, PtagRval, Range,
        PtagCases, Default, Context),
    ml_simplify_switch(SwitchStmt0, SwitchStmt, !Info),
    Stmts = [SwitchStmt].

:- pred gen_tagged_case_code(code_model::in, packed_args_map::in,
    tagged_case::in, case_id::out, code_map::in, code_map::out,
    unit::in, unit::out, ml_gen_info::in, ml_gen_info::out) is det.

gen_tagged_case_code(CodeModel, EntryPackedArgsMap, TaggedCase, CaseId,
        !CodeMap, !Unit, Info0, Info) :-
    ml_gen_info_set_packed_args_map(EntryPackedArgsMap, Info0, Info1),
    TaggedCase = tagged_case(_MainTaggedConsId, OtherTaggedConsIds,
        CaseId, Goal),
    ml_gen_goal_as_branch_block(CodeModel, Goal, Stmt, Info1, Info2),
    % Do not allow the generated code to be literally duplicated if it contains
    % labels. Rather, we will regenerate the code at every point it is required
    % so that the labels are unique.
    ( if
        OtherTaggedConsIds = [_ | _],
        statement_contains_label(Stmt)
    then
        MaybeCode = generate(EntryPackedArgsMap, Goal),
        Info = Info1
    else
        MaybeCode = immediate(Stmt),
        Info = Info2
    ),
    map.det_insert(CaseId, MaybeCode, !CodeMap).

:- pred statement_contains_label(mlds_stmt::in) is semidet.

statement_contains_label(Stmt) :-
    statement_is_or_contains_statement(Stmt, SubStmt),
    SubStmt = ml_stmt_label(_, _).

:- type is_a_case_split_between_ptags
    --->    no_case_is_split_between_ptags
    ;       some_case_is_split_between_ptags.

:- pred find_any_split_cases(case_id_ptags_map::in,
    is_a_case_split_between_ptags::out) is det.
:- pragma consider_used(find_any_split_cases/2).

find_any_split_cases(CaseIdPtagsMap, IsAnyCaseSplit) :-
    map.foldl(find_any_split_cases_2, CaseIdPtagsMap,
        no_case_is_split_between_ptags, IsAnyCaseSplit).

:- pred find_any_split_cases_2(case_id::in, set(ptag)::in,
    is_a_case_split_between_ptags::in, is_a_case_split_between_ptags::out)
    is det.

find_any_split_cases_2(_CaseId, Ptags, !IsAnyCaseSplit) :-
    ( if set.is_singleton(Ptags, _OnlyPtag) then
        true
    else
        !:IsAnyCaseSplit = some_case_is_split_between_ptags
    ).

%---------------------------------------------------------------------------%

:- pred gen_ptag_cases(ptag_case_group_list(case_id)::in, code_map::in,
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

:- pred gen_ptag_case(ptag_case_group_entry(case_id)::in, code_map::in,
    prog_var::in, can_fail::in, code_model::in, ptag_count_map::in,
    prog_context::in, mlds_switch_case::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_ptag_case(PtagCase, CodeMap, Var, CanFail, CodeModel, PtagCountMap,
        Context, MLDS_Case, !Info) :-
    PtagCase = ptag_case_group_entry(MainPtag, OtherPtags,
        ptag_case(SecTagLocn, GoalMap)),
    map.lookup(PtagCountMap, MainPtag, CountInfo),
    CountInfo = SecTagLocn1 - MaxSecondary,
    expect(unify(SecTagLocn, SecTagLocn1), $pred,
        "secondary tag locations differ"),
    map.to_assoc_list(GoalMap, GoalList),
    (
        ( SecTagLocn = sectag_none
        ; SecTagLocn = sectag_none_direct_arg
        ),
        % There is no secondary tag, so there is no switch on it.
        (
            GoalList = [],
            unexpected($pred, "no goal for non-shared tag")
        ;
            GoalList = [_Stag - CaseId],
            lookup_code_map(CodeMap, CaseId, CodeModel, Stmt, !Info)
        ;
            GoalList = [_, _ | _],
            unexpected($pred, "more than one goal for non-shared tag")
        )
    ;
        ( SecTagLocn = sectag_local_rest_of_word
        ; SecTagLocn = sectag_local_bits(_, _)
        ; SecTagLocn = sectag_remote
        ),
        expect(unify(OtherPtags, []), $pred, ">1 ptag with secondary tag"),
        (
            CanFail = cannot_fail,
            CaseCanFail = cannot_fail
        ;
            CanFail = can_fail,
            ( if
                list.length(GoalList, GoalCount),
                FullGoalCount = MaxSecondary + 1,
                FullGoalCount = GoalCount
            then
                CaseCanFail = cannot_fail
            else
                CaseCanFail = can_fail
            )
        ),
        group_stag_cases(GoalList, GroupedGoalList),
        ( if
            GroupedGoalList = [CaseId - _Stags],
            CaseCanFail = cannot_fail
        then
            % There is only one possible matching goal, so we don't need
            % to switch on it. This can happen if the other functor symbols
            % that share this primary tag are ruled out by the initial inst
            % of the switched-on variable.
            lookup_code_map(CodeMap, CaseId, CodeModel, Stmt, !Info)
        else
            gen_stag_switch(GroupedGoalList, CodeMap, MainPtag, SecTagLocn,
                Var, CodeModel, CaseCanFail, Context, Stmt, !Info)
        )
    ),
    MainPtagMatch = make_ptag_match(MainPtag),
    OtherPtagMatches = list.map(make_ptag_match, OtherPtags),
    MLDS_Case = mlds_switch_case(MainPtagMatch, OtherPtagMatches, Stmt).

:- func make_ptag_match(ptag) = mlds_case_match_cond.

make_ptag_match(Ptag) = Cond :-
    Ptag = ptag(PtagUint8),
    Cond = match_value(ml_const(mlconst_int(uint8.cast_to_int(PtagUint8)))).

:- pred lookup_code_map(code_map::in, case_id::in, code_model::in,
    mlds_stmt::out, ml_gen_info::in, ml_gen_info::out) is det.

lookup_code_map(CodeMap, CaseId, CodeModel, Stmt, !Info) :-
    map.lookup(CodeMap, CaseId, MaybeCode),
    (
        MaybeCode = immediate(Stmt)
    ;
        MaybeCode = generate(EntryPackedArgsMap, Goal),
        ml_gen_info_set_packed_args_map(EntryPackedArgsMap, !Info),
        ml_gen_goal_as_branch_block(CodeModel, Goal, Stmt, !Info)
    ).

%---------------------------------------------------------------------------%

    % A list of secondary tag values that all have the same code.
    % The list is stored in reversed form.
    %
:- type stags
    --->    stags(int, list(int)).

    % Maps case numbers (each of which identifies the code of one switch arm)
    % to the secondary tags that share that code.
    %
:- type stag_rev_map == map(case_id, stags).

:- pred group_stag_cases(stag_goal_list(case_id)::in,
    assoc_list(case_id, stags)::out) is det.

group_stag_cases(Goals, GroupedGoals) :-
    build_stag_rev_map(Goals, map.init, RevMap),
    map.to_assoc_list(RevMap, GroupedGoals).

:- pred build_stag_rev_map(stag_goal_list(case_id)::in,
    stag_rev_map::in, stag_rev_map::out) is det.

build_stag_rev_map([], !RevMap).
build_stag_rev_map([Entry | Entries], !RevMap) :-
    Entry = Stag - CaseId,
    ( if map.search(!.RevMap, CaseId, OldEntry) then
        OldEntry = stags(OldFirstStag, OldLaterStags),
        NewEntry = stags(OldFirstStag, [Stag | OldLaterStags]),
        map.det_update(CaseId, NewEntry, !RevMap)
    else
        NewEntry = stags(Stag, []),
        map.det_insert(CaseId, NewEntry, !RevMap)
    ),
    build_stag_rev_map(Entries, !RevMap).

%---------------------------------------------------------------------------%

:- pred gen_stag_switch(assoc_list(case_id, stags)::in,
    code_map::in, ptag::in, sectag_locn::in, prog_var::in,
    code_model::in, can_fail::in, prog_context::in, mlds_stmt::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_stag_switch(Cases, CodeMap, Ptag, StagLocn, Var, CodeModel,
        CanFail, Context, Stmt, !Info) :-
    % Generate the rval for the secondary tag.
    ml_variable_type(!.Info, Var, VarType),
    ml_gen_var(!.Info, Var, VarLval),
    VarRval = ml_lval(VarLval),
    (
        StagLocn = sectag_local_rest_of_word,
        StagRval = ml_unop(unmkbody, VarRval)
    ;
        StagLocn = sectag_local_bits(_, Mask),
        StagRval = ml_binop(bitwise_and(int_type_uint),
            ml_unop(unmkbody, VarRval),
            ml_const(mlconst_uint(Mask)))
    ;
        StagLocn = sectag_remote,
        ml_gen_secondary_tag_rval(!.Info, VarType, VarRval,
            Ptag, StagRval)
    ;
        ( StagLocn = sectag_none
        ; StagLocn = sectag_none_direct_arg
        ),
        unexpected($pred, "no stag")
    ),

    % Generate the switch on the secondary tag.
    gen_stag_cases(Cases, CodeMap, CodeModel, StagCases0, !Info),
    list.sort(StagCases0, StagCases),
    ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info),

    % Package up the results into a switch statement.
    Range = mlds_switch_range_unknown, % XXX could do better
    SwitchStmt = ml_stmt_switch(mlds_native_int_type, StagRval, Range,
        StagCases, Default, Context),
    ml_simplify_switch(SwitchStmt, Stmt, !Info).

%---------------------------------------------------------------------------%

:- pred gen_stag_cases(assoc_list(case_id, stags)::in,
    code_map::in, code_model::in, list(mlds_switch_case)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_stag_cases([], _, _, [], !Info).
gen_stag_cases([Group | Groups], CodeMap, CodeModel, [Case | Cases], !Info) :-
    gen_stag_case(Group, CodeMap, CodeModel, Case, !Info),
    gen_stag_cases(Groups, CodeMap, CodeModel, Cases, !Info).

:- pred gen_stag_case(pair(case_id, stags)::in,
    code_map::in, code_model::in, mlds_switch_case::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_stag_case(Group, CodeMap, CodeModel, MLDS_Case, !Info) :-
    Group = CaseId - Stags,
    Stags = stags(FirstStag, RevLaterStags),
    list.reverse(RevLaterStags, LaterStags),
    FirstMatch = make_match_value(FirstStag),
    LaterMatches = list.map(make_match_value, LaterStags),
    lookup_code_map(CodeMap, CaseId, CodeModel, Stmt, !Info),
    MLDS_Case = mlds_switch_case(FirstMatch, LaterMatches, Stmt).

:- func make_match_value(int) = mlds_case_match_cond.

make_match_value(Stag) = match_value(ml_const(mlconst_int(Stag))).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_tag_switch.
%---------------------------------------------------------------------------%
