%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: switch_gen.m.
% Authors: conway, fjh, zs.
%
% This module determines how we should generate code for a switch, primarily
% by deciding what sort of indexing, if any, we should use.
% NOTE The code here is quite similar to the code in ml_switch_gen.m,
% which does the same thing for the MLDS back-end. Any changes here
% probably also require similar changes there.
%
% The following describes the different forms of indexing that we can use.
%
% 1 For switches on atomic data types (int, char, enums), we can use two
%   smart indexing strategies.
%
%   a)  If all the switch arms contain only construction unifications of
%       constants, then we generate a dense lookup table (an array) in which
%       we look up the values of the output variables.
%       Implemented by lookup_switch.m.
%
%   b)  If the cases are not sparse, we use a computed_goto.
%       Implemented by dense_switch.m.
%
% 2 For switches on strings, we can use four smart indexing strategies,
%   which are the possible combinations of two possible implementations
%   of each of two aspects of the switch.
%
%   The first aspect is the implementation of the lookup.
%
%   a)  One basic implementation approach is the use of a hash table with
%       open addressing. Since the contents of the hash table is fixed,
%       the open addressing can select buckets that are not the home bucket
%       of any string in the table. And if we know that no two strings in
%       the table share the same home address, we can dispense with open
%       addressing altogether.
%
%   b)  The other basic implementation approach is the use of binary search.
%       We generate a table containing all the strings in the switch cases in
%       order, and search it using binary search.
%
%   The second aspect is whether we use a lookup table. If all the switch arms
%   contain only construction unifications of constants, then we extend each
%   row in either the hash table or the binary search table with extra columns
%   containing the values of the output variables.
%
%   All these indexing strategies are implemented by string_switch.m, with
%   some help from utility predicates in lookup_switch.m.
%
% 3 For switches on discriminated union types, we generate code that does
%   indexing first on the primary tag, and then on the secondary tag (if
%   the primary tag is shared between several function symbols). The
%   indexing code for switches on both primary and secondary tags can be
%   in the form of a try-me-else chain, a try chain, a dense jump table
%   or a binary search.
%   Implemented by tag_switch.m.
%
%   XXX We should implement lookup switches on secondary tags, and (if the
%   switched-on type does not use any secondary tags) on primary tags as well.
%
% 4 For switches on floats, we could generate code that does binary search.
%   However, this is not yet implemented.
%
% If we cannot apply any of the above smart indexing strategies, or if the
% --smart-indexing option was disabled, then this module just generates
% a chain of if-then-elses.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.switch_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred generate_switch(code_model::in, prog_var::in, can_fail::in,
    list(case)::in, hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.switch_util.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.dense_switch.
:- import_module ll_backend.lookup_switch.
:- import_module ll_backend.string_switch.
:- import_module ll_backend.tag_switch.
:- import_module ll_backend.trace_gen.
:- import_module ll_backend.unify_gen_test.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

generate_switch(CodeModel, SwitchVar, CanFail, Cases, GoalInfo, Code,
        !CI, !CLD) :-
    % Choose which method to use to generate the switch.
    % CanFail says whether the switch covers all cases.

    goal_info_get_store_map(GoalInfo, StoreMap),
    get_next_label(EndLabel, !CI),
    get_module_info(!.CI, ModuleInfo),
    SwitchVarType = variable_type(!.CI, SwitchVar),
    tag_cases(ModuleInfo, SwitchVarType, Cases, TaggedCases0,
        MaybeIntSwitchInfo),
    list.sort_and_remove_dups(TaggedCases0, TaggedCases),

    SwitchVarName = variable_name(!.CI, SwitchVar),
    produce_variable(SwitchVar, SwitchVarCode, SwitchVarRval, !.CI, !CLD),

    find_switch_category(ModuleInfo, SwitchVarType, SwitchCategory,
        MayUseSmartIndexing),
    (
        MayUseSmartIndexing = may_not_use_smart_indexing,
        order_and_generate_cases(TaggedCases, SwitchVarRval, SwitchVarType,
            SwitchVarName, CodeModel, CanFail, GoalInfo, EndLabel, MaybeEnd,
            SwitchCode, !CI, !.CLD)
    ;
        MayUseSmartIndexing = may_use_smart_indexing,
        module_info_get_globals(ModuleInfo, Globals),
        (
            ( SwitchCategory = atomic_switch
            ; SwitchCategory = int64_switch
            ),
            generate_atomic_or_int64_switch(ModuleInfo, Globals,
                CodeModel, CanFail, MaybeIntSwitchInfo, SwitchVarName,
                SwitchVarType, SwitchVarRval, TaggedCases, GoalInfo,
                EndLabel, MaybeEnd, SwitchCode, !CI, !.CLD)
        ;
            SwitchCategory = string_switch,
            generate_string_switch(Globals, CodeModel, CanFail,
                SwitchVarName, SwitchVarType, SwitchVarRval, TaggedCases,
                GoalInfo, EndLabel, MaybeEnd, SwitchCode, !CI, !.CLD)
        ;
            SwitchCategory = tag_switch,
            num_cons_ids_in_tagged_cases(TaggedCases, NumConsIds, NumArms),
            globals.lookup_int_option(Globals, tag_switch_size, TagSize),
            ( if NumConsIds >= TagSize, NumArms > 1 then
                generate_tag_switch(TaggedCases, SwitchVarRval, SwitchVarType,
                    SwitchVarName, CodeModel, CanFail, GoalInfo, EndLabel,
                    no, MaybeEnd, SwitchCode, !CI, !.CLD)
            else
                order_and_generate_cases(TaggedCases, SwitchVarRval,
                    SwitchVarType, SwitchVarName, CodeModel, CanFail,
                    GoalInfo, EndLabel, MaybeEnd, SwitchCode, !CI, !.CLD)
            )
        ;
            SwitchCategory = float_switch,
            order_and_generate_cases(TaggedCases, SwitchVarRval, SwitchVarType,
                SwitchVarName, CodeModel, CanFail, GoalInfo, EndLabel,
                MaybeEnd, SwitchCode, !CI, !.CLD)
        )
    ),
    Code = SwitchVarCode ++ SwitchCode,
    after_all_branches(StoreMap, MaybeEnd, !.CI, !:CLD).

:- pred generate_atomic_or_int64_switch(module_info::in, globals::in,
    code_model::in, can_fail::in, maybe_int_switch_info::in,
    string::in, mer_type::in, rval::in, list(tagged_case)::in,
    hlds_goal_info::in, label::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_atomic_or_int64_switch(ModuleInfo, Globals, CodeModel, CanFail,
        MaybeIntSwitchInfo, SwitchVarName, SwitchVarType, SwitchVarRval,
        TaggedCases, GoalInfo, EndLabel, MaybeEnd, SwitchCode, !CI, !.CLD) :-
    num_cons_ids_in_tagged_cases(TaggedCases, NumConsIds, NumArms),
    ( if
        MaybeIntSwitchInfo = int_switch(IntSwitchInfo),
        IntSwitchInfo = int_switch_info(LowerLimit, UpperLimit, NumValues),
        % Since lookup switches rely on static ground terms to work
        % efficiently, there is no point in using a lookup switch
        % if static ground terms are not enabled. Well, actually,
        % it is possible that they might be a win in some
        % circumstances, but it would take a pretty complex heuristic
        % to get it right, so, lets just use a simple one - no static
        % ground terms, no lookup switch.
        globals.lookup_bool_option(Globals, static_ground_cells, yes),

        % Lookup switches do not generate trace events.
        get_maybe_trace_info(!.CI, MaybeTraceInfo),
        MaybeTraceInfo = no,

        globals.lookup_int_option(Globals, lookup_switch_size,
            LookupSize),
        NumConsIds >= LookupSize,
        NumArms > 1,
        globals.lookup_int_option(Globals, lookup_switch_req_density,
            ReqDensity),
        filter_out_failing_cases_if_needed(CodeModel,
            TaggedCases, FilteredTaggedCases,
            CanFail, FilteredCanFail),
        find_int_lookup_switch_params(ModuleInfo, SwitchVarType,
            FilteredCanFail, LowerLimit, UpperLimit, NumValues,
            ReqDensity, NeedBitVecCheck, NeedRangeCheck,
            FirstVal, LastVal),
        remember_position(!.CLD, BranchStart),
        goal_info_get_store_map(GoalInfo, StoreMap),
        is_lookup_switch(BranchStart, get_int_tag, FilteredTaggedCases,
            GoalInfo, StoreMap, no, MaybeEnd1, LookupSwitchInfo, !CI)
    then
        % We update MaybeEnd1 to MaybeEnd to account for the possible
        % reservation of temp slots for nondet switches.
        reset_to_position(BranchStart, !.CI, !:CLD),
        generate_int_lookup_switch(SwitchVarRval, LookupSwitchInfo,
            EndLabel, StoreMap, FirstVal, LastVal,
            NeedBitVecCheck, NeedRangeCheck,
            MaybeEnd1, MaybeEnd, SwitchCode, !CI, !.CLD)
    else if
        MaybeIntSwitchInfo = int_switch(IntSwitchInfo),
        IntSwitchInfo = int_switch_info(LowerLimit, UpperLimit, NumValues),
        globals.lookup_int_option(Globals, dense_switch_size,
            DenseSize),
        NumConsIds >= DenseSize,
        NumArms > 1,
        globals.lookup_int_option(Globals, dense_switch_req_density,
            ReqDensity),
        tagged_case_list_is_dense_switch(!.CI, SwitchVarType,
            TaggedCases, LowerLimit, UpperLimit, NumValues,
            ReqDensity, CanFail, DenseSwitchInfo)
    then
        generate_dense_switch(TaggedCases, SwitchVarRval,
            SwitchVarName, CodeModel, GoalInfo, DenseSwitchInfo,
            EndLabel, no, MaybeEnd, SwitchCode, !CI, !.CLD)
    else
        order_and_generate_cases(TaggedCases, SwitchVarRval,
            SwitchVarType, SwitchVarName, CodeModel, CanFail,
            GoalInfo, EndLabel, MaybeEnd, SwitchCode, !CI, !.CLD)
    ).

:- pred generate_string_switch(globals::in, code_model::in, can_fail::in,
    string::in, mer_type::in, rval::in, list(tagged_case)::in,
    hlds_goal_info::in, label::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_string_switch(Globals, CodeModel, CanFail,
        SwitchVarName, SwitchVarType, SwitchVarRval,
        TaggedCases, GoalInfo, EndLabel, MaybeEnd, SwitchCode, !CI, !.CLD) :-
    goal_info_get_store_map(GoalInfo, StoreMap),
    filter_out_failing_cases_if_needed(CodeModel,
        TaggedCases, FilteredTaggedCases, CanFail, FilteredCanFail),
    num_cons_ids_in_tagged_cases(FilteredTaggedCases,
        NumConsIds, NumArms),
    ( if NumArms > 1 then
        globals.lookup_int_option(Globals, string_hash_switch_size,
            StringHashSwitchSize),
        globals.lookup_int_option(Globals, string_binary_switch_size,
            StringBinarySwitchSize),
        ( if NumConsIds >= StringHashSwitchSize then
            ( if
                remember_position(!.CLD, BranchStart),
                is_lookup_switch(BranchStart, get_string_tag,
                    FilteredTaggedCases, GoalInfo, StoreMap,
                    no, MaybeEnd1, LookupSwitchInfo, !CI)
            then
                % We update MaybeEnd1 to MaybeEnd to account for the
                % possible reservation of temp slots for nondet
                % switches.
                reset_to_position(BranchStart, !.CI, !:CLD),
                generate_string_hash_lookup_switch(SwitchVarRval,
                    LookupSwitchInfo, FilteredCanFail, EndLabel,
                    StoreMap, MaybeEnd1, MaybeEnd, SwitchCode,
                    !CI, !.CLD)
            else
                generate_string_hash_switch(TaggedCases, SwitchVarRval,
                    SwitchVarName, CodeModel, CanFail, GoalInfo,
                    EndLabel, MaybeEnd, SwitchCode,
                    !CI, !.CLD)
            )
        else if NumConsIds >= StringBinarySwitchSize then
            ( if
                remember_position(!.CLD, BranchStart),
                is_lookup_switch(BranchStart, get_string_tag,
                    FilteredTaggedCases, GoalInfo, StoreMap,
                    no, MaybeEnd1, LookupSwitchInfo, !CI)
            then
                % We update MaybeEnd1 to MaybeEnd to account for the
                % possible reservation of temp slots for nondet
                % switches.
                reset_to_position(BranchStart, !.CI, !:CLD),
                generate_string_binary_lookup_switch(SwitchVarRval,
                    LookupSwitchInfo, FilteredCanFail, EndLabel,
                    StoreMap, MaybeEnd1, MaybeEnd, SwitchCode,
                    !CI, !.CLD)
            else
                generate_string_binary_switch(TaggedCases,
                    SwitchVarRval, SwitchVarName, CodeModel, CanFail,
                    GoalInfo, EndLabel, MaybeEnd, SwitchCode,
                    !CI, !.CLD)
            )
        else
            order_and_generate_cases(TaggedCases, SwitchVarRval,
                SwitchVarType, SwitchVarName, CodeModel, CanFail,
                GoalInfo, EndLabel, MaybeEnd, SwitchCode, !CI, !.CLD)
        )
    else
        order_and_generate_cases(TaggedCases, SwitchVarRval,
            SwitchVarType, SwitchVarName, CodeModel, CanFail,
            GoalInfo, EndLabel, MaybeEnd, SwitchCode, !CI, !.CLD)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Generate a switch as a chain of if-then-elses.
    %
    % To generate a case for a switch, we generate code to do a tag-test,
    % and fall through to the next case in the event of failure.
    %
    % Each case except the last consists of
    %
    % - a tag test, jumping to the next case if it fails;
    % - the goal for that case;
    % - code to move variables to where the store map says they ought to be;
    % - a branch to the end of the switch.
    %
    % For the last case, if the switch covers all cases that can occur,
    % we don't need to generate the tag test, and we never need to generate
    % the branch to the end of the switch.
    %
    % After the last case, we put the end-of-switch label which other cases
    % branch to after their case goals.
    %
    % In the important special case of a det switch with two cases,
    % we try to find out which case will be executed more frequently,
    % and put that one first. This minimizes the number of pipeline
    % breaks caused by taken branches.
    %
:- pred order_and_generate_cases(list(tagged_case)::in, rval::in, mer_type::in,
    string::in, code_model::in, can_fail::in, hlds_goal_info::in, label::in,
    branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

order_and_generate_cases(TaggedCases, VarRval, VarType, VarName, CodeModel,
        CanFail, GoalInfo, EndLabel, MaybeEnd, Code, !CI, !.CLD) :-
    order_cases(TaggedCases, OrderedTaggedCases, CodeModel, CanFail, !.CI),
    type_to_ctor_det(VarType, TypeCtor),
    get_module_info(!.CI, ModuleInfo),
    module_info_get_type_table(ModuleInfo, TypeTable),
    ( if search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) then
        get_type_defn_body(TypeDefn, TypeBody),
        CheaperTagTest = get_maybe_cheaper_tag_test(TypeBody)
    else
        CheaperTagTest = no_cheaper_tag_test
    ),
    remember_position(!.CLD, BranchStart),
    generate_if_then_else_chain_cases(BranchStart, OrderedTaggedCases,
        VarRval, VarType, VarName, CheaperTagTest, CodeModel, CanFail,
        GoalInfo, EndLabel, no, MaybeEnd, Code, !CI).

:- pred order_cases(list(tagged_case)::in, list(tagged_case)::out,
    code_model::in, can_fail::in, code_info::in) is det.

order_cases(Cases0, Cases, CodeModel, CanFail, CI) :-
    % We do ordering here based on three considerations.
    %
    % - We try to put cases that can succeed before ones that cannot, since
    %   cases that cannot succeed clearly won't be executed frequently.
    %
    % - If the recursion structure of the predicate is sufficiently simple that
    %   we can make a good guess at which case will be executed more
    %   frequently, we try to put the frequent case first.
    %
    % - We try to put cheap-to-execute tests first; for arms with more than one
    %   cons_id, we sum the costs of their tests. The main aim of this is to
    %   reduce the average cost at runtime. For cannot_fail switches, putting
    %   the most expensive-to-test case last has the additional benefit that
    %   we don't ever need to execute that test, since the failure of all the
    %   previous ones guarantees that it could not fail. This should be
    %   especially useful for switches in which many cons_ids share a single
    %   arm.
    %
    % Each consideration is implemented by its own predicate, which calls the
    % predicate of the next consideration to decide ties. The predicates for
    % the three considerations are
    %
    % - order_cases (this predicate),
    % - order_recursive_cases,
    % - order_tag_test_cost
    %
    % respectively.

    separate_cannot_succeed_cases(Cases0, CanSucceedCases, CannotSucceedCases),
    (
        CannotSucceedCases = [],
        order_recursive_cases(Cases0, Cases, CodeModel, CanFail, CI)
    ;
        CannotSucceedCases = [_ | _],
        % There is no point in calling order_recursive_cases in this situation.
        Cases = CanSucceedCases ++ CannotSucceedCases
    ).

:- pred separate_cannot_succeed_cases(list(tagged_case)::in,
    list(tagged_case)::out, list(tagged_case)::out) is det.

separate_cannot_succeed_cases([], [], []).
separate_cannot_succeed_cases([Case | Cases],
        CanSucceedCases, CannotSucceedCases) :-
    separate_cannot_succeed_cases(Cases,
        CanSucceedCases1, CannotSucceedCases1),
    Case = tagged_case(_, _, _, Goal),
    Goal = hlds_goal(_, GoalInfo),
    Detism = goal_info_get_determinism(GoalInfo),
    determinism_components(Detism, _CanFail, SolnCount),
    (
        ( SolnCount = at_most_one
        ; SolnCount = at_most_many_cc
        ; SolnCount = at_most_many
        ),
        CanSucceedCases = [Case | CanSucceedCases1],
        CannotSucceedCases = CannotSucceedCases1
    ;
        SolnCount = at_most_zero,
        CanSucceedCases = CanSucceedCases1,
        CannotSucceedCases = [Case | CannotSucceedCases1]
    ).

%-----------------------------------------------------------------------------%

:- pred order_recursive_cases(list(tagged_case)::in, list(tagged_case)::out,
    code_model::in, can_fail::in, code_info::in) is det.

order_recursive_cases(Cases0, Cases, CodeModel, CanFail, CI) :-
    ( if
        CodeModel = model_det,
        CanFail = cannot_fail,
        Cases0 = [Case1, Case2],
        Case1 = tagged_case(_, _, _, Goal1),
        Case2 = tagged_case(_, _, _, Goal2)
    then
        get_module_info(CI, ModuleInfo),
        module_info_get_globals(ModuleInfo, Globals),
        get_pred_id(CI, PredId),
        get_proc_id(CI, ProcId),
        count_recursive_calls(Goal1, PredId, ProcId, Min1, Max1),
        count_recursive_calls(Goal2, PredId, ProcId, Min2, Max2),
        ( if
            ( if
                Max1 = 0,   % Goal1 is a base case
                Min2 = 1    % Goal2 is probably singly recursive
            then
                BaseCase = Case1,
                SingleRecCase = Case2
            else if
                Max2 = 0,   % Goal2 is a base case
                Min1 = 1    % Goal1 is probably singly recursive
            then
                BaseCase = Case2,
                SingleRecCase = Case1
            else
                fail
            )
        then
            globals.lookup_bool_option(Globals, switch_single_rec_base_first,
                SingleRecBaseFirst),
            (
                SingleRecBaseFirst = yes,
                Cases = [SingleRecCase, BaseCase]
            ;
                SingleRecBaseFirst = no,
                Cases = [BaseCase, SingleRecCase]
            )
        else if
            ( if
                Max1 = 0,   % Goal1 is a base case
                Min2 > 1    % Goal2 is at least doubly recursive
            then
                BaseCase = Case1,
                MultiRecCase = Case2
            else if
                Max2 = 0,   % Goal2 is a base case
                Min1 > 1    % Goal1 is at least doubly recursive
            then
                BaseCase = Case2,
                MultiRecCase = Case1
            else
                fail
            )
        then
            globals.lookup_bool_option(Globals, switch_multi_rec_base_first,
                MultiRecBaseFirst),
            (
                MultiRecBaseFirst = yes,
                Cases = [BaseCase, MultiRecCase]
            ;
                MultiRecBaseFirst = no,
                Cases = [MultiRecCase, BaseCase]
            )
        else
            order_tag_test_cost(Cases0, Cases)
        )
    else
        order_tag_test_cost(Cases0, Cases)
    ).

%-----------------------------------------------------------------------------%

:- pred order_tag_test_cost(list(tagged_case)::in, list(tagged_case)::out)
    is det.

order_tag_test_cost(Cases0, Cases) :-
    CostedCases = list.map(estimate_cost_of_case_test, Cases0),
    list.sort(CostedCases, SortedCostedCases),
    assoc_list.values(SortedCostedCases, Cases).

:- func estimate_cost_of_case_test(tagged_case) = pair(int, tagged_case).

estimate_cost_of_case_test(TaggedCase) = Cost - TaggedCase :-
    TaggedCase = tagged_case(MainTaggedConsId, OtherTaggedConsIds, _, _),
    MainTag = project_tagged_cons_id_tag(MainTaggedConsId),
    MainCost = estimate_switch_tag_test_cost(MainTag),
    OtherTags = list.map(project_tagged_cons_id_tag, OtherTaggedConsIds),
    OtherCosts = list.map(estimate_switch_tag_test_cost, OtherTags),
    Cost = list.foldl(int.plus, [MainCost | OtherCosts], 0).

%-----------------------------------------------------------------------------%

:- pred generate_if_then_else_chain_cases(position_info::in,
    list(tagged_case)::in, rval::in, mer_type::in, string::in,
    maybe_cheaper_tag_test::in, code_model::in, can_fail::in,
    hlds_goal_info::in, label::in, branch_end::in, branch_end::out,
    llds_code::out, code_info::in, code_info::out) is det.

generate_if_then_else_chain_cases(BranchStart, Cases, VarRval, VarType,
        VarName, CheaperTagTest, CodeModel, CanFail, SwitchGoalInfo, EndLabel,
        !MaybeEnd, Code, !CI) :-
    (
        Cases = [HeadCase | TailCases],
        HeadCase = tagged_case(MainTaggedConsId, OtherTaggedConsIds, _, Goal),
        goal_info_get_store_map(SwitchGoalInfo, StoreMap),
        ( if
            ( TailCases = [_ | _]
            ; CanFail = can_fail
            )
        then
            generate_test_var_has_one_tagged_cons_id(VarRval, VarName,
                MainTaggedConsId, OtherTaggedConsIds, CheaperTagTest,
                branch_on_failure, NextLabel, TestCode, !CI),
            ElseCode = from_list([
                llds_instr(goto(code_label(EndLabel)),
                    "skip to the end of the switch on " ++ VarName),
                llds_instr(label(NextLabel), "next case")
            ])
        else
            % When debugging code generator output, we need a way to tell
            % which case's code is next. We normally hang this comment
            % on the test, but in this case there is no test.
            project_cons_name_and_tag(MainTaggedConsId, MainConsName, _),
            list.map2(project_cons_name_and_tag, OtherTaggedConsIds,
                OtherConsNames, _),
            Comment = case_comment(VarName, MainConsName, OtherConsNames),
            TestCode = singleton(
                llds_instr(comment(Comment), "")
            ),
            ElseCode = empty
        ),

        some [!CLD] (
            reset_to_position(BranchStart, !.CI, !:CLD),
            maybe_generate_internal_event_code(Goal, SwitchGoalInfo, TraceCode,
                !CI, !CLD),
            generate_goal(CodeModel, Goal, GoalCode, !CI, !CLD),
            generate_branch_end(StoreMap, !MaybeEnd, SaveCode, !.CI, !.CLD)
        ),
        HeadCaseCode = TestCode ++ TraceCode ++ GoalCode ++ SaveCode ++
            ElseCode,
        generate_if_then_else_chain_cases(BranchStart, TailCases,
            VarRval, VarType, VarName, CheaperTagTest, CodeModel, CanFail,
            SwitchGoalInfo, EndLabel, !MaybeEnd, TailCasesCode, !CI),
        Code = HeadCaseCode ++ TailCasesCode
    ;
        Cases = [],
        (
            CanFail = can_fail,
            % At the end of a locally semidet switch, we fail because we came
            % across a tag which was not covered by one of the cases. It is
            % followed by the end of switch label to which the cases branch.
            some [!CLD] (
                reset_to_position(BranchStart, !.CI, !:CLD),
                generate_failure(FailCode, !CI, !.CLD)
            )
        ;
            CanFail = cannot_fail,
            FailCode = empty
        ),
        EndCode = singleton(
            llds_instr(label(EndLabel),
                "end of the switch on " ++ VarName)
        ),
        Code = FailCode ++ EndCode
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.switch_gen.
%-----------------------------------------------------------------------------%
