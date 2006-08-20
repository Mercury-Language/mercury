%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: switch_gen.m.
% Authors: conway, fjh, zs.
% 
% This module handles the generation of code for switches, which are
% disjunctions that do not require backtracking.  Switches are detected
% in switch_detection.m.  This is the module that determines what
% sort of indexing to use for each switch and then actually generates the
% code.
%
% Currently the following forms of indexing are used:
%
% For switches on atomic data types (int, char, enums),
% if the cases are not sparse, we use the value of the switch variable
% to index into a jump table.
%
% If all the alternative goals for a switch on an atomic data type
% contain only construction unifications of constants, then we generate
% a dense lookup table (an array) for each output variable of the switch,
% rather than a dense jump table, so that executing the switch becomes
% a matter of doing an array index for each output variable - avoiding
% the branch overhead of the jump-table.
%
% For switches on discriminated union types, we generate code that does
% indexing first on the primary tag, and then on the secondary tag (if
% the primary tag is shared between several function symbols). The
% indexing code for switches on both primary and secondary tags can be
% in the form of a try-me-else chain, a try chain, a dense jump table
% or a binary search.
%
% For switches on strings, we lookup the address to jump to in a hash table,
% using open addressing to resolve hash collisions.
%
% For all other cases (or if the --smart-indexing option was disabled),
% we just generate a chain of if-then-elses.
% 
%-----------------------------------------------------------------------------%

:- module ll_backend.switch_gen.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred generate_switch(code_model::in, prog_var::in, can_fail::in,
    list(case)::in, hlds_goal_info::in, code_tree::out,
    code_info::in, code_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.switch_util.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_llds.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.dense_switch.
:- import_module ll_backend.lookup_switch.
:- import_module ll_backend.string_switch.
:- import_module ll_backend.tag_switch.
:- import_module ll_backend.trace_gen.
:- import_module ll_backend.unify_gen.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

generate_switch(CodeModel, CaseVar, CanFail, Cases, GoalInfo, Code, !CI) :-
    % Choose which method to use to generate the switch.
    % CanFail says whether the switch covers all cases.

    goal_info_get_store_map(GoalInfo, StoreMap),
    SwitchCategory = determine_category(!.CI, CaseVar),
    code_info.get_next_label(EndLabel, !CI),
    lookup_tags(!.CI, Cases, CaseVar, TaggedCases0),
    list.sort_and_remove_dups(TaggedCases0, TaggedCases),
    code_info.get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, smart_indexing, Indexing),
    (
        % Check for a switch on a type whose representation
        % uses reserved addresses.
        list.member(Case, TaggedCases),
        Case = case(_Priority, Tag, _ConsId, _Goal),
        (
            Tag = reserved_address_tag(_)
        ;
            Tag = shared_with_reserved_addresses_tag(_, _)
        )
    ->
        % XXX This may be be inefficient in some cases.
        generate_all_cases(TaggedCases, CaseVar, CodeModel, CanFail, GoalInfo,
            EndLabel, no, MaybeEnd, Code, !CI)
    ;
        Indexing = yes,
        SwitchCategory = atomic_switch,
        code_info.get_maybe_trace_info(!.CI, MaybeTraceInfo),
        MaybeTraceInfo = no,
        list.length(TaggedCases, NumCases),
        globals.lookup_int_option(Globals, lookup_switch_size, LookupSize),
        NumCases >= LookupSize,
        globals.lookup_int_option(Globals, lookup_switch_req_density,
            ReqDensity),
        is_lookup_switch(CaseVar, TaggedCases, GoalInfo, CanFail, ReqDensity,
            StoreMap, no, MaybeEndPrime, CodeModel, LookupSwitchInfo, !CI)
    ->
        MaybeEnd = MaybeEndPrime,
        generate_lookup_switch(CaseVar, StoreMap, no, LookupSwitchInfo, Code,
            !CI)
    ;
        Indexing = yes,
        SwitchCategory = atomic_switch,
        list.length(TaggedCases, NumCases),
        globals.lookup_int_option(Globals, dense_switch_size, DenseSize),
        NumCases >= DenseSize,
        globals.lookup_int_option(Globals, dense_switch_req_density,
            ReqDensity),
        dense_switch.is_dense_switch(!.CI, CaseVar, TaggedCases,
            CanFail, ReqDensity, FirstVal, LastVal, CanFail1)
    ->
        generate_dense_switch(TaggedCases, FirstVal, LastVal, CaseVar,
            CodeModel, CanFail1, GoalInfo, EndLabel, no, MaybeEnd, Code, !CI)
    ;
        Indexing = yes,
        SwitchCategory = string_switch,
        list.length(TaggedCases, NumCases),
        globals.lookup_int_option(Globals, string_switch_size, StringSize),
        NumCases >= StringSize
    ->
        generate_string_switch(TaggedCases, CaseVar, CodeModel, CanFail,
            GoalInfo, EndLabel, no, MaybeEnd, Code, !CI)
    ;
        Indexing = yes,
        SwitchCategory = tag_switch,
        list.length(TaggedCases, NumCases),
        globals.lookup_int_option(Globals, tag_switch_size, TagSize),
        NumCases >= TagSize
    ->
        generate_tag_switch(TaggedCases, CaseVar, CodeModel, CanFail,
            GoalInfo, EndLabel, no, MaybeEnd, Code, !CI)
    ;
        % To generate a switch, first we flush the variable on whose tag
        % we are going to switch, then we generate the cases for the switch.
        generate_all_cases(TaggedCases, CaseVar, CodeModel, CanFail, GoalInfo,
            EndLabel, no, MaybeEnd, Code, !CI)
    ),
    code_info.after_all_branches(StoreMap, MaybeEnd, !CI).

%-----------------------------------------------------------------------------%

    % We categorize switches according to whether the value being switched on
    % is an atomic type, a string, or something more complicated.
    %
:- func determine_category(code_info, prog_var) = switch_category.

determine_category(CI, CaseVar) = SwitchCategory :-
    Type = code_info.variable_type(CI, CaseVar),
    code_info.get_module_info(CI, ModuleInfo),
    classify_type(ModuleInfo, Type) = TypeCategory,
    SwitchCategory = switch_util.type_cat_to_switch_cat(TypeCategory).

%-----------------------------------------------------------------------------%

:- pred lookup_tags(code_info::in, list(case)::in, prog_var::in,
    cases_list::out) is det.

lookup_tags(_, [], _, []).
lookup_tags(CI, [Case | Cases], Var, [TaggedCase | TaggedCases]) :-
    Case = case(ConsId, Goal),
    Tag = code_info.cons_id_to_tag(CI, Var, ConsId),
    Priority = switch_util.switch_priority(Tag),
    TaggedCase = case(Priority, Tag, ConsId, Goal),
    lookup_tags(CI, Cases, Var, TaggedCases).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Generate a switch as a chain of if-then-elses.
    %
    % To generate a case for a switch we generate
    % code to do a tag-test and fall through to the next case in
    % the event of failure.
    %
    % Each case except the last consists of
    %
    %   a tag test, jumping to the next case if it fails
    %   the goal for that case
    %   code to move variables to where the store map says they
    %       ought to be
    %   a branch to the end of the switch.
    %
    % For the last case, if the switch covers all cases that can occur,
    % we don't need to generate the tag test, and we never need to
    % generate the branch to the end of the switch.
    %
    % After the last case, we put the end-of-switch label which other
    % cases branch to after their case goals.
    %
    % In the important special case of a det switch with two cases,
    % we try to find out which case will be executed more frequently,
    % and put that one first. This minimizes the number of pipeline
    % breaks caused by taken branches.
    %
:- pred generate_all_cases(list(extended_case)::in, prog_var::in,
    code_model::in, can_fail::in, hlds_goal_info::in, label::in,
    branch_end::in, branch_end::out, code_tree::out,
    code_info::in, code_info::out) is det.

generate_all_cases(Cases0, Var, CodeModel, CanFail, GoalInfo, EndLabel,
        !MaybeEnd, Code, !CI) :-
    code_info.produce_variable(Var, VarCode, _Rval, !CI),
    (
        CodeModel = model_det,
        CanFail = cannot_fail,
        Cases0 = [Case1, Case2],
        Case1 = case(_, _, _, Goal1),
        Case2 = case(_, _, _, Goal2)
    ->
        code_info.get_pred_id(!.CI, PredId),
        code_info.get_proc_id(!.CI, ProcId),
        count_recursive_calls(Goal1, PredId, ProcId, Min1, Max1),
        count_recursive_calls(Goal2, PredId, ProcId, Min2, Max2),
        (
            Max1 = 0,   % Goal1 is a base case
            Min2 = 1    % Goal2 is probably singly recursive
        ->
            Cases = [Case2, Case1]
        ;
            Max2 = 0,   % Goal2 is a base case
            Min1 > 1    % Goal1 is at least doubly recursive
        ->
            Cases = [Case2, Case1]
        ;
            Cases = Cases0
        )
    ;
        Cases = Cases0
    ),
    generate_cases(Cases, Var, CodeModel, CanFail, GoalInfo, EndLabel,
        !MaybeEnd, CasesCode, !CI),
    Code = tree(VarCode, CasesCode).

:- pred generate_cases(list(extended_case)::in, prog_var::in,
    code_model::in, can_fail::in, hlds_goal_info::in, label::in,
    branch_end::in, branch_end::out, code_tree::out,
    code_info::in, code_info::out) is det.

generate_cases([], _Var, _CodeModel, CanFail, _GoalInfo, EndLabel, !MaybeEnd,
        Code, !CI) :-
    (
        CanFail = can_fail,
        % At the end of a locally semidet switch, we fail because we came
        % across a tag which was not covered by one of the cases. It is
        % followed by the end of switch label to which the cases branch.
        code_info.generate_failure(FailCode, !CI)
    ;
        CanFail = cannot_fail,
        FailCode = empty
    ),
    EndCode = node([label(EndLabel) - "end of switch"]),
    Code = tree(FailCode, EndCode).

generate_cases([case(_, _, Cons, Goal) | Cases], Var, CodeModel, CanFail,
        SwitchGoalInfo, EndLabel, !MaybeEnd, CasesCode, !CI) :-
    code_info.remember_position(!.CI, BranchStart),
    goal_info_get_store_map(SwitchGoalInfo, StoreMap),
    (
        ( Cases = [_|_]
        ; CanFail = can_fail
        )
    ->
        unify_gen.generate_tag_test(Var, Cons, branch_on_failure, NextLabel,
            TestCode, !CI),
        maybe_generate_internal_event_code(Goal, SwitchGoalInfo, TraceCode,
            !CI),
        code_gen.generate_goal(CodeModel, Goal, GoalCode, !CI),
        code_info.generate_branch_end(StoreMap, !MaybeEnd, SaveCode, !CI),
        ElseCode = node([
            goto(label(EndLabel)) - "skip to the end of the switch",
            label(NextLabel) - "next case"
        ]),
        ThisCaseCode = tree_list([TestCode, TraceCode, GoalCode, SaveCode,
             ElseCode])
    ;
        maybe_generate_internal_event_code(Goal, SwitchGoalInfo, TraceCode,
            !CI),
        code_gen.generate_goal(CodeModel, Goal, GoalCode, !CI),
        code_info.generate_branch_end(StoreMap, !MaybeEnd, SaveCode, !CI),
        ThisCaseCode = tree_list([TraceCode, GoalCode, SaveCode])
    ),
    code_info.reset_to_position(BranchStart, !CI),
    generate_cases(Cases, Var, CodeModel, CanFail,
        SwitchGoalInfo, EndLabel, !MaybeEnd, OtherCasesCode, !CI),
    CasesCode = tree(ThisCaseCode, OtherCasesCode).

%-----------------------------------------------------------------------------%
