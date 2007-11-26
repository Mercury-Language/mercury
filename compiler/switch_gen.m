%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2007 The University of Melbourne.
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
% For switches on atomic data types (int, char, enums), if the cases are not
% sparse, we use the value of the switch variable to index into a jump table.
%
% If all the alternative goals for a switch on an atomic data type
% contain only construction unifications of constants, then we generate
% a dense lookup table (an array) for the output variables of the switch,
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
% For switches on strings, we look up the address to jump to in a hash table,
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
:- import_module hlds.hlds_module.
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
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.

%-----------------------------------------------------------------------------%

generate_switch(CodeModel, CaseVar, CanFail, Cases, GoalInfo, Code, !CI) :-
    % Choose which method to use to generate the switch.
    % CanFail says whether the switch covers all cases.

    goal_info_get_store_map(GoalInfo, StoreMap),
    get_next_label(EndLabel, !CI),
    lookup_tags(!.CI, Cases, CaseVar, TaggedCases0),
    list.sort_and_remove_dups(TaggedCases0, TaggedCases),
    get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, smart_indexing, Indexing),

    CaseVarType = variable_type(!.CI, CaseVar),
    type_to_ctor_det(CaseVarType, CaseVarTypeCtor),
    get_module_info(!.CI, ModuleInfo),
    TypeCategory = classify_type(ModuleInfo, CaseVarType),
    SwitchCategory = switch_util.type_cat_to_switch_cat(TypeCategory),
    (
        (
            Indexing = no
        ;
            module_info_get_type_table(ModuleInfo, TypeTable),
            % The search will fail for builtin types.
            map.search(TypeTable, CaseVarTypeCtor, CaseVarTypeDefn),
            hlds_data.get_type_defn_body(CaseVarTypeDefn, CaseVarTypeBody),
            CaseVarTypeBody ^ du_type_reserved_addr = uses_reserved_address
        )
    ->
        order_and_generate_cases(TaggedCases, CaseVar, CodeModel, CanFail,
            GoalInfo, EndLabel, no, MaybeEnd, Code, !CI)
    ;
        (
            SwitchCategory = atomic_switch,
            list.length(TaggedCases, NumCases),
            (
                get_maybe_trace_info(!.CI, MaybeTraceInfo),
                MaybeTraceInfo = no,
                globals.lookup_int_option(Globals, lookup_switch_size,
                    LookupSize),
                NumCases >= LookupSize,
                globals.lookup_int_option(Globals, lookup_switch_req_density,
                    ReqDensity),
                is_lookup_switch(CaseVar, TaggedCases, GoalInfo, CanFail,
                    ReqDensity, StoreMap, no, MaybeEndPrime, CodeModel,
                    LookupSwitchInfo, !CI)
            ->
                MaybeEnd = MaybeEndPrime,
                generate_lookup_switch(CaseVar, StoreMap, no, LookupSwitchInfo,
                    Code, !CI)
            ;
                globals.lookup_int_option(Globals, dense_switch_size,
                    DenseSize),
                NumCases >= DenseSize,
                globals.lookup_int_option(Globals, dense_switch_req_density,
                    ReqDensity),
                cases_list_is_dense_switch(!.CI, CaseVar, TaggedCases, CanFail,
                    ReqDensity, FirstVal, LastVal, CanFail1)
            ->
                generate_dense_switch(TaggedCases, FirstVal, LastVal, CaseVar,
                    CodeModel, CanFail1, GoalInfo, EndLabel, no, MaybeEnd,
                    Code, !CI)
            ;
                order_and_generate_cases(TaggedCases, CaseVar, CodeModel,
                    CanFail, GoalInfo, EndLabel, no, MaybeEnd, Code, !CI)
            )
        ;
            SwitchCategory = string_switch,
            list.length(TaggedCases, NumCases),
            globals.lookup_int_option(Globals, string_switch_size, StringSize),
            ( NumCases >= StringSize ->
                generate_string_switch(TaggedCases, CaseVar, CodeModel,
                    CanFail, GoalInfo, EndLabel, no, MaybeEnd, Code, !CI)
            ;
                order_and_generate_cases(TaggedCases, CaseVar, CodeModel,
                    CanFail, GoalInfo, EndLabel, no, MaybeEnd, Code, !CI)
            )
        ;
            SwitchCategory = tag_switch,
            list.length(TaggedCases, NumCases),
            globals.lookup_int_option(Globals, tag_switch_size, TagSize),
            ( NumCases >= TagSize ->
                generate_tag_switch(TaggedCases, CaseVar, CodeModel, CanFail,
                    GoalInfo, EndLabel, no, MaybeEnd, Code, !CI)
            ;
                order_and_generate_cases(TaggedCases, CaseVar, CodeModel,
                    CanFail, GoalInfo, EndLabel, no, MaybeEnd, Code, !CI)
            )
        ;
            SwitchCategory = other_switch,
            order_and_generate_cases(TaggedCases, CaseVar, CodeModel,
                CanFail, GoalInfo, EndLabel, no, MaybeEnd, Code, !CI)
        )
    ),
    after_all_branches(StoreMap, MaybeEnd, !CI).

%-----------------------------------------------------------------------------%

    % We categorize switches according to whether the value being switched on
    % is an atomic type, a string, or something more complicated.
    %
:- func determine_switch_category(code_info, prog_var) = switch_category.

determine_switch_category(CI, CaseVar) = SwitchCategory :-
    Type = variable_type(CI, CaseVar),
    get_module_info(CI, ModuleInfo),
    classify_type(ModuleInfo, Type) = TypeCategory,
    SwitchCategory = switch_util.type_cat_to_switch_cat(TypeCategory).

%-----------------------------------------------------------------------------%

:- pred lookup_tags(code_info::in, list(case)::in, prog_var::in,
    cases_list::out) is det.

lookup_tags(_, [], _, []).
lookup_tags(CI, [Case | Cases], Var, [TaggedCase | TaggedCases]) :-
    Case = case(ConsId, Goal),
    Tag = cons_id_to_tag_for_var(CI, Var, ConsId),
    Priority = switch_util.switch_priority(Tag),
    TaggedCase = extended_case(Priority, Tag, ConsId, Goal),
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
    %   code to move variables to where the store map says they ought to be
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
:- pred order_and_generate_cases(list(extended_case)::in, prog_var::in,
    code_model::in, can_fail::in, hlds_goal_info::in, label::in,
    branch_end::in, branch_end::out, code_tree::out,
    code_info::in, code_info::out) is det.

order_and_generate_cases(Cases0, Var, CodeModel, CanFail, GoalInfo, EndLabel,
        !MaybeEnd, Code, !CI) :-
    % XXX We should use _VarRval below; we shouldn't produce the variable
    % again.
    produce_variable(Var, VarCode, _VarRval, !CI),
    VarType = variable_type(!.CI, Var),
    order_cases(Cases0, Cases, VarType, CodeModel, CanFail, !.CI),
    generate_if_then_else_chain_cases(Cases, Var, CodeModel, CanFail, GoalInfo,
        EndLabel, !MaybeEnd, CasesCode, !CI),
    Code = tree(VarCode, CasesCode).

:- pred order_cases(list(extended_case)::in, list(extended_case)::out,
    mer_type::in, code_model::in, can_fail::in, code_info::in) is det.

order_cases(Cases0, Cases, VarType, CodeModel, CanFail, CI) :-
    % We do ordering here based on three out of four considerations.
    %
    % - We try to put tests against reserved addresses first, so later cases
    %   can assume those tests have already been done.
    % - We try to put cases that can succeed before ones that cannot, since
    %   cases that cannot succeed clearly won't be executed frequently.
    % - If the recursion structure of the predicate is sufficiently simple that
    %   we can make a good guess at which case will be executed more
    %   frequently, we try to put the frequent case first.
    % - We try to put cheap-to-execute tests first.
    %
    % order_cases acts on the first consideration. order_cannot_succeed_cases
    % acts on the second and indirectly (by calling order_recursive_cases) the
    % third.
    %
    % The fourth consideration has already been acted upon when the switch
    % priorities were put into each extended case, and the list of cases sorted
    % on that priority. That is why we take care not to upset the existing
    % order except when one of the first three considerations dictate a need
    % to do so.

    (
        search_type_defn(CI, VarType, VarTypeDefn),
        get_type_defn_body(VarTypeDefn, VarTypeDefnBody),
        VarTypeDefnBody ^ du_type_reserved_addr = uses_reserved_address
    ->
        separate_reserved_address_cases(Cases0,
            ReservedAddrCases0, NonReservedAddrCases0),
        order_cannot_succeed_cases(ReservedAddrCases0, ReservedAddrCases,
            CodeModel, CanFail, CI),
        order_cannot_succeed_cases(NonReservedAddrCases0, NonReservedAddrCases,
            CodeModel, CanFail, CI),
        Cases = ReservedAddrCases ++ NonReservedAddrCases
    ;
        % The type is either not a discriminated union type (e.g. in int or
        % string), or it is a discriminated union type that does not use
        % reserved addresses.
        order_cannot_succeed_cases(Cases0, Cases, CodeModel, CanFail, CI)
    ).

:- pred separate_reserved_address_cases(list(extended_case)::in,
    list(extended_case)::out, list(extended_case)::out) is det.

separate_reserved_address_cases([], [], []).
separate_reserved_address_cases([Case | Cases],
        ReservedAddrCases, NonReservedAddrCases) :-
    separate_reserved_address_cases(Cases,
        ReservedAddrCases1, NonReservedAddrCases1),
    Case = extended_case(_, ConsTag, _, _),
    (
        ConsTag = reserved_address_tag(_),
        ReservedAddrCases = [Case | ReservedAddrCases1],
        NonReservedAddrCases = NonReservedAddrCases1
    ;
        ( ConsTag = no_tag
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = float_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = int_tag(_)
        ; ConsTag = pred_closure_tag(_, _, _)
        ; ConsTag = shared_local_tag(_, _)
        ; ConsTag = shared_remote_tag(_, _)
        ; ConsTag = shared_with_reserved_addresses_tag(_, _)
        ; ConsTag = single_functor_tag
        ; ConsTag = string_tag(_)
        ; ConsTag = table_io_decl_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = unshared_tag(_)
        ),
        ReservedAddrCases = ReservedAddrCases1,
        NonReservedAddrCases = [Case | NonReservedAddrCases1]
    ).

:- pred order_cannot_succeed_cases(
    list(extended_case)::in, list(extended_case)::out,
    code_model::in, can_fail::in, code_info::in) is det.

order_cannot_succeed_cases(Cases0, Cases, CodeModel, CanFail, CI) :-
    separate_cannot_succeed_cases(Cases0, CanSucceedCases, CannotSucceedCases),
    (
        CannotSucceedCases = [],
        order_recursive_cases(Cases0, Cases, CodeModel, CanFail, CI)
    ;
        CannotSucceedCases = [_ | _],
        % There is no point in calling order_recursive_cases in this situation.
        Cases = CanSucceedCases ++ CannotSucceedCases
    ).

:- pred separate_cannot_succeed_cases(list(extended_case)::in,
    list(extended_case)::out, list(extended_case)::out) is det.

separate_cannot_succeed_cases([], [], []).
separate_cannot_succeed_cases([Case | Cases],
        CanSucceedCases, CannotSucceedCases) :-
    separate_cannot_succeed_cases(Cases,
        CanSucceedCases1, CannotSucceedCases1),
    Case = extended_case(_, _, _, Goal),
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

:- pred order_recursive_cases(
    list(extended_case)::in, list(extended_case)::out,
    code_model::in, can_fail::in, code_info::in) is det.

order_recursive_cases(Cases0, Cases, CodeModel, CanFail, CI) :-
    (
        CodeModel = model_det,
        CanFail = cannot_fail,
        Cases0 = [Case1, Case2],
        Case1 = extended_case(_, _, _, Goal1),
        Case2 = extended_case(_, _, _, Goal2)
    ->
        get_module_info(CI, ModuleInfo),
        module_info_get_globals(ModuleInfo, Globals),
        get_pred_id(CI, PredId),
        get_proc_id(CI, ProcId),
        count_recursive_calls(Goal1, PredId, ProcId, Min1, Max1),
        count_recursive_calls(Goal2, PredId, ProcId, Min2, Max2),
        (
            (
                Max1 = 0,   % Goal1 is a base case
                Min2 = 1    % Goal2 is probably singly recursive
            ->
                BaseCase = Case1,
                SingleRecCase = Case2
            ;
                Max2 = 0,   % Goal2 is a base case
                Min1 = 1    % Goal1 is probably singly recursive
            ->
                BaseCase = Case2,
                SingleRecCase = Case1
            ;
                fail
            )
        ->
            globals.lookup_bool_option(Globals, switch_single_rec_base_first,
                SingleRecBaseFirst),
            (
                SingleRecBaseFirst = yes,
                Cases = [SingleRecCase, BaseCase]
            ;
                SingleRecBaseFirst = no,
                Cases = [BaseCase, SingleRecCase]
            )
        ;
            (
                Max1 = 0,   % Goal1 is a base case
                Min2 > 1    % Goal2 is at least doubly recursive
            ->
                BaseCase = Case1,
                MultiRecCase = Case2
            ;
                Max2 = 0,   % Goal2 is a base case
                Min1 > 1    % Goal1 is at least doubly recursive
            ->
                BaseCase = Case2,
                MultiRecCase = Case1
            ;
                fail
            )
        ->
            globals.lookup_bool_option(Globals, switch_multi_rec_base_first,
                MultiRecBaseFirst),
            (
                MultiRecBaseFirst = yes,
                Cases = [BaseCase, MultiRecCase]
            ;
                MultiRecBaseFirst = no,
                Cases = [MultiRecCase, BaseCase]
            )
        ;
            Cases = Cases0
        )
    ;
        Cases = Cases0
    ).

%-----------------------------------------------------------------------------%

:- pred generate_if_then_else_chain_cases(list(extended_case)::in,
    prog_var::in, code_model::in, can_fail::in, hlds_goal_info::in, label::in,
    branch_end::in, branch_end::out, code_tree::out,
    code_info::in, code_info::out) is det.

generate_if_then_else_chain_cases(Cases, Var, CodeModel, CanFail,
        SwitchGoalInfo, EndLabel, !MaybeEnd, Code, !CI) :-
    (
        Cases = [HeadCase | TailCases],
        HeadCase = extended_case(_, _, Cons, Goal),
        remember_position(!.CI, BranchStart),
        goal_info_get_store_map(SwitchGoalInfo, StoreMap),
        (
            ( TailCases = [_ | _]
            ; CanFail = can_fail
            )
        ->
            unify_gen.generate_tag_test(Var, Cons, branch_on_failure,
                NextLabel, TestCode, !CI),
            maybe_generate_internal_event_code(Goal, SwitchGoalInfo, TraceCode,
                !CI),
            code_gen.generate_goal(CodeModel, Goal, GoalCode, !CI),
            generate_branch_end(StoreMap, !MaybeEnd, SaveCode, !CI),
            ElseCode = node([
                llds_instr(goto(code_label(EndLabel)),
                    "skip to the end of the switch"),
                llds_instr(label(NextLabel), "next case")
            ]),
            HeadCaseCode = tree_list([TestCode, TraceCode, GoalCode, SaveCode,
                 ElseCode])
        ;
            maybe_generate_internal_event_code(Goal, SwitchGoalInfo, TraceCode,
                !CI),
            code_gen.generate_goal(CodeModel, Goal, GoalCode, !CI),
            generate_branch_end(StoreMap, !MaybeEnd, SaveCode, !CI),
            HeadCaseCode = tree_list([TraceCode, GoalCode, SaveCode])
        ),
        reset_to_position(BranchStart, !CI),
        generate_if_then_else_chain_cases(TailCases, Var, CodeModel, CanFail,
            SwitchGoalInfo, EndLabel, !MaybeEnd, TailCasesCode, !CI),
        Code = tree(HeadCaseCode, TailCasesCode)
    ;
        Cases = [],
        (
            CanFail = can_fail,
            % At the end of a locally semidet switch, we fail because we came
            % across a tag which was not covered by one of the cases. It is
            % followed by the end of switch label to which the cases branch.
            generate_failure(FailCode, !CI)
        ;
            CanFail = cannot_fail,
            FailCode = empty
        ),
        EndCode = node([llds_instr(label(EndLabel), "end of switch")]),
        Code = tree(FailCode, EndCode)
    ).

%-----------------------------------------------------------------------------%
