%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2003-2009 The University of Melbourne.
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
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.mlds.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Generate efficient indexing code for tag based switches.
    %
:- pred ml_generate_tag_switch(list(tagged_case)::in, prog_var::in,
    code_model::in, can_fail::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.rtti.
:- import_module backend_libs.switch_util.
:- import_module hlds.hlds_data.
:- import_module libs.compiler_util.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_simplify_switch.
:- import_module ml_backend.ml_switch_gen.
:- import_module ml_backend.ml_unify_gen.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module unit.

%-----------------------------------------------------------------------------%

ml_generate_tag_switch(TaggedCases, Var, CodeModel, CanFail, Context,
        Decls, Statements, !Info) :-
    % Generate the rval for the primary tag.
    ml_gen_var(!.Info, Var, VarLval),
    VarRval = ml_lval(VarLval),
    PTagRval = ml_unop(std_unop(tag), VarRval),

    % Group the cases based on primary tag value, find out how many
    % constructors share each primary tag value, and sort the cases so that
    % the most frequently occurring primary tag values come first.

    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_variable_type(!.Info, Var, Type),
    switch_util.get_ptag_counts(Type, ModuleInfo, MaxPrimary, PtagCountMap),
    map.to_assoc_list(PtagCountMap, PtagCountList),
    map.init(PtagCaseMap0),
    switch_util.group_cases_by_ptag(TaggedCases,
        represent_tagged_case_by_itself, unit, _, unit, _, unit, _,
        PtagCaseMap0, PtagCaseMap),
    switch_util.order_ptags_by_count(PtagCountList, PtagCaseMap,
        PtagCaseList),

    % Generate the switch on the primary tag.
    gen_ptag_cases(PtagCaseList, Var, CanFail, CodeModel,
        PtagCountMap, Context, MLDS_Cases, !Info),
    ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info),

    % Package up the results into a switch statement.
    Range = mlds_switch_range(0, MaxPrimary),
    SwitchStmt0 = ml_stmt_switch(mlds_native_int_type, PTagRval, Range,
        MLDS_Cases, Default),
    MLDS_Context = mlds_make_context(Context),
    ml_simplify_switch(SwitchStmt0, MLDS_Context, SwitchStatement, !Info),
    Decls = [],
    Statements = [SwitchStatement].

:- pred gen_ptag_cases(ptag_case_list(tagged_case)::in, prog_var::in,
    can_fail::in, code_model::in, ptag_count_map::in,
    prog_context::in, list(mlds_switch_case)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_ptag_cases([], _, _, _, _, _, [], !Info).
gen_ptag_cases([Case | Cases], Var, CanFail, CodeModel,
        PtagCountMap, Context, [MLDS_Case | MLDS_Cases], !Info) :-
    gen_ptag_case(Case, Var, CanFail, CodeModel,
        PtagCountMap, Context, MLDS_Case, !Info),
    gen_ptag_cases(Cases, Var, CanFail, CodeModel,
        PtagCountMap, Context, MLDS_Cases, !Info).

:- pred gen_ptag_case(pair(tag_bits, ptag_case(tagged_case))::in,
    prog_var::in, can_fail::in, code_model::in, ptag_count_map::in,
    prog_context::in, mlds_switch_case::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_ptag_case(Case, Var, CanFail, CodeModel, PtagCountMap, Context, MLDS_Case,
        !Info) :-
    Case = PrimaryTag - ptag_case(SecTagLocn, GoalMap),
    map.lookup(PtagCountMap, PrimaryTag, CountInfo),
    CountInfo = SecTagLocn1 - MaxSecondary,
    expect(unify(SecTagLocn, SecTagLocn1), this_file,
        "ml_tag_switch.m: secondary tag locations differ"),
    map.to_assoc_list(GoalMap, GoalList),
    (
        SecTagLocn = sectag_none,
        % There is no secondary tag, so there is no switch on it.
        (
            GoalList = [],
            unexpected(this_file, "no goal for non-shared tag")
        ;
            GoalList = [_Stag - TaggedCase],
            TaggedCase = tagged_case(_MainTaggedConsId, _OtherTaggedConsIds,
                Goal),
            ml_gen_goal_as_block(CodeModel, Goal, Statement, !Info)
        ;
            GoalList = [_, _ | _],
            unexpected(this_file, "more than one goal for non-shared tag")
        )
    ;
        ( SecTagLocn = sectag_local
        ; SecTagLocn = sectag_remote
        ),
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
        (
            GoalList = [_Stag - TaggedCase],
            CaseCanFail = cannot_fail
        ->
            TaggedCase = tagged_case(_MainTaggedConsId, _OtherTaggedConsIds,
                Goal),
            % There is only one possible matching goal,
            % so we don't need to switch on it.
            ml_gen_goal_as_block(CodeModel, Goal, Statement, !Info)
        ;
            gen_stag_switch(GoalList, PrimaryTag, SecTagLocn,
                Var, CodeModel, CaseCanFail, Context, Statement, !Info)
        )
    ),
    PrimaryTagRval = ml_const(mlconst_int(PrimaryTag)),
    MLDS_Case = mlds_switch_case([match_value(PrimaryTagRval)], Statement).

:- pred gen_stag_switch(stag_goal_list(tagged_case)::in, int::in,
    sectag_locn::in, prog_var::in, code_model::in, can_fail::in,
    prog_context::in, statement::out, ml_gen_info::in, ml_gen_info::out)
    is det.

gen_stag_switch(Cases, PrimaryTag, StagLocn, Var, CodeModel, CanFail, Context,
        Statement, !Info) :-
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
        STagRval = ml_gen_secondary_tag_rval(PrimaryTag, VarType, ModuleInfo,
            VarRval)
    ;
        StagLocn = sectag_none,
        unexpected(this_file, "gen_stag_switch: no stag")
    ),

    % Generate the switch on the secondary tag.
    gen_stag_cases(Cases, CodeModel, MLDS_Cases, !Info),
    ml_switch_generate_default(CanFail, CodeModel, Context, Default, !Info),

    % Package up the results into a switch statement.
    Range = mlds_switch_range_unknown, % XXX could do better
    SwitchStmt = ml_stmt_switch(mlds_native_int_type, STagRval, Range,
        MLDS_Cases, Default),
    MLDS_Context = mlds_make_context(Context),
    ml_simplify_switch(SwitchStmt, MLDS_Context, Statement, !Info).

:- pred gen_stag_cases(stag_goal_list(tagged_case)::in, code_model::in,
    list(mlds_switch_case)::out, ml_gen_info::in, ml_gen_info::out) is det.

gen_stag_cases([], _, [], !Info).
gen_stag_cases([Case | Cases], CodeModel, [MLDS_Case | MLDS_Cases], !Info) :-
    gen_stag_case(Case, CodeModel, MLDS_Case, !Info),
    gen_stag_cases(Cases, CodeModel, MLDS_Cases, !Info).

:- pred gen_stag_case(pair(tag_bits, tagged_case)::in,
    code_model::in, mlds_switch_case::out,
    ml_gen_info::in, ml_gen_info::out) is det.

gen_stag_case(Case, CodeModel, MLDS_Case, !Info) :-
    Case = Stag - tagged_case(_MainTaggedConsId, _OtherTaggedConsIds, Goal),
    StagRval = ml_const(mlconst_int(Stag)),
    ml_gen_goal_as_block(CodeModel, Goal, Statement, !Info),
    MLDS_Case = mlds_switch_case([match_value(StagRval)], Statement).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "ml_tag_switch.m".

%-----------------------------------------------------------------------------%
