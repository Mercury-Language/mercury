%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2000,2002-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: tag_switch.m.
% Author: zs.

% Generate switches based on primary and secondary tags.

%-----------------------------------------------------------------------------%

:- module ll_backend.tag_switch.
:- interface.

:- import_module backend_libs.switch_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Generate intelligent indexing code for tag based switches.
    %
:- pred generate_tag_switch(list(extended_case)::in, prog_var::in,
    code_model::in, can_fail::in, hlds_goal_info::in, label::in,
    branch_end::in, branch_end::out, code_tree::out,
    code_info::in, code_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_pred.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.trace.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

    % The idea is to generate two-level switches, first on the primary
    % tag and then on the secondary tag. Since more than one function
    % symbol can be eliminated by a failed primary tag test, this reduces
    % the expected the number of comparisons required before finding the
    % code corresponding to the actual value of the switch variable.
    % We also get a speedup compared to non-tag switches by extracting
    % the primary and secondary tags once instead of repeatedly for
    % each functor test.
    %
    % We have four methods we can use for generating the code for the
    % switches on both primary and secondary tags.
    %
    % 1. try-me-else chains have the form
    %
    %       if (tag(var) != tag1) goto L1
    %       code for tag1
    %       goto end
    %   L1: if (tag(var) != tag2) goto L2
    %       code for tag2
    %       goto end
    %   L2: ...
    %   Ln: code for last possible tag value (or failure)
    %       goto end
    %
    % 2. try chains have the form
    %
    %       if (tag(var) == tag1) goto L1
    %       if (tag(var) == tag2) goto L2
    %       ...
    %       code for last possible tag value (or failure)
    %       goto end
    %   L1: code for tag1
    %       goto end
    %   L2: code for tag2
    %       goto end
    %       ...
    %
    % 3. jump tables have the form
    %
    %       goto tag(var) of L1, L2, ...
    %   L1: code for tag1
    %       goto end
    %   L2: code for tag2
    %       goto end
    %       ...
    %
    % 4. binary search switches have the form
    %
    %       if (tag(var)) > 1) goto L23
    %       if (tag(var)) != 0) goto L1
    %       code for tag 0
    %       goto end
    %   L1: code for tag 1
    %       goto end
    %   L23:    if (tag(var)) != 2) goto L3
    %       code for tag 2
    %       goto end
    %   L3: code for tag 3
    %       goto end
    %
    % Note that for a det switch with two tag values, try-me-else chains
    % and try chains are equivalent.
    %
    % Which method is best depends on the number of possible tag values,
    % on the costs of taken/untaken branches and table lookups on the given
    % architecture, and on the frequency with which the various
    % alternatives are taken.
    %
    % While the first two are in principle known at compile time,
    % the third is not. Nevertheless, for switches on primary tags
    % we can use the heuristic that the more secondary tags assigned to
    % a primary tag, the more likely that the switch variable will have
    % that primary tag at runtime.
    %
    % Try chains are good for switches with small numbers of alternatives
    % on architectures where untaken branches are cheaper than taken
    % branches.
    %
    % Try-me-else chains are good for switches with very small numbers of
    % alternatives on architectures where taken branches are cheaper than
    % untaken branches (which are rare these days).
    %
    % Jump tables are good for switches with large numbers of alternatives.
    % The cost of jumping through a jump table is relatively high, since
    % it involves a memory access and an indirect branch (which most
    % current architectures do not handle well), but this cost is
    % independent of the number of alternatives.
    %
    % Binary search switches are good for switches where the number of
    % alternatives is large enough for the reduced expected number of
    % branches executed to overcome the extra overhead of the subtraction
    % required for some conditional branches (compared to try chains
    % and try-me-else chains), but not large enough to make the
    % expected cost of the expected number of comparisons exceed the
    % expected cost of a jump table lookup and dispatch.

    % For try-me-else chains, we want tag1 to be the most frequent case,
    % tag 2 the next most frequent case, etc.
    %
    % For det try chains, we want the last tag value to be the most
    % frequent case, since it can be reached without taken jumps.
    % We want tag1 to be the next most frequent, tag2 the next most
    % frequent after that, etc.
    %
    % For semidet try chains, there is no last possible tag value (the
    % code for failure occupies its position), so we want tag1 to be
    % the most frequent case, tag 2 the next most frequent case, etc.
    %
    % For jump tables, the position of the labels in the computed goto
    % must conform to their numerical value. The order of the code
    % fragments does not really matter, although the last has a slight
    % edge in that no goto is needed to reach the code following the
    % switch. If there is no code following the switch (which happens
    % very frequently), then even this advantage is nullified.
    %
    % For binary search switches, we want the case of the most frequently
    % occurring tag to be the first, since this code is reached with no
    % taken branches and ends with an unconditional branch, whereas
    % reaching the code of the other cases requires at least one taken
    % *conditional* branch. In general, at each binary decision we
    % want the more frequently reached cases to be in the half that
    % immediately follows the if statement implementing the decision.

:- type switch_method
    --->    try_me_else_chain
    ;       try_chain
    ;       jump_table
    ;       binary_search.

generate_tag_switch(Cases, Var, CodeModel, CanFail, SwitchGoalInfo, EndLabel,
        !MaybeEnd, Code, !CI) :-
    % Group the cases based on primary tag value and find out how many
    % constructors share each primary tag value.

    code_info.get_module_info(!.CI, ModuleInfo),
    code_info.get_proc_info(!.CI, ProcInfo),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.lookup(VarTypes, Var, Type),
    switch_util.get_ptag_counts(Type, ModuleInfo,
        MaxPrimary, PtagCountMap),
    map.to_assoc_list(PtagCountMap, PtagCountList),
    map.init(PtagCaseMap0),
    switch_util.group_cases_by_ptag(Cases, PtagCaseMap0, PtagCaseMap),

    map.count(PtagCaseMap, PtagsUsed),
    code_info.get_globals(!.CI, Globals),
    globals.lookup_int_option(Globals, dense_switch_size,
        DenseSwitchSize),
    globals.lookup_int_option(Globals, try_switch_size,
        TrySwitchSize),
    globals.lookup_int_option(Globals, binary_switch_size,
        BinarySwitchSize),
    ( PtagsUsed >= DenseSwitchSize ->
        PrimaryMethod = jump_table
    ; PtagsUsed >= BinarySwitchSize ->
        PrimaryMethod = binary_search
    ; PtagsUsed >= TrySwitchSize ->
        PrimaryMethod = try_chain
    ;
        PrimaryMethod = try_me_else_chain
    ),

    % We get a register for holding the tag. The tag is needed only
    % by the switch, and no other code gets control between producing
    % the tag value and all uses of it, so we can release the register
    % for use by the code of the various cases.

    % We forgo using the register if the primary tag is needed only once,
    % or if the "register" we get is likely to be slower than
    % recomputing the tag from scratch.

    code_info.produce_variable_in_reg(Var, VarCode, VarLval, !CI),
    VarRval = lval(VarLval),
    code_info.acquire_reg(r, PtagReg, !CI),
    code_info.release_reg(PtagReg, !CI),
    (
        PrimaryMethod \= jump_table,
        PtagsUsed >= 2,
        globals.lookup_int_option(Globals, num_real_r_regs, NumRealRegs),
        (
            NumRealRegs = 0
        ;
            ( PtagReg = reg(r, PtagRegNo) ->
                PtagRegNo =< NumRealRegs
            ;
                unexpected(this_file, "improper reg in tag switch")
            )
        )
    ->
        PtagCode = node([
            assign(PtagReg, unop(tag, VarRval)) - "compute tag to switch on"
        ]),
        PtagRval = lval(PtagReg)
    ;
        PtagCode = empty,
        PtagRval = unop(tag, VarRval)
    ),

    % We generate FailCode and EndCode here because the last case within
    % a primary tag may not be the last case overall.

    code_info.get_next_label(FailLabel, !CI),
    FailLabelCode = node([label(FailLabel) - "switch has failed"]),
    (
        CanFail = cannot_fail,
        FailCode = node([goto(do_not_reached) - "oh-oh, det switch failed"])
    ;
        CanFail = can_fail,
        code_info.generate_failure(FailCode, !CI)
    ),
    LabelledFailCode = tree(FailLabelCode, FailCode),

    EndCode = node([label(EndLabel) - "end of tag switch"]),

    (
        PrimaryMethod = binary_search,
        switch_util.order_ptags_by_value(0, MaxPrimary, PtagCaseMap,
            PtagCaseList),
        generate_primary_binary_search(PtagCaseList, 0, MaxPrimary, PtagRval,
            VarRval, CodeModel, CanFail, SwitchGoalInfo, EndLabel, FailLabel,
            PtagCountMap, !MaybeEnd, CasesCode, !CI)
    ;
        PrimaryMethod = jump_table,
        switch_util.order_ptags_by_value(0, MaxPrimary, PtagCaseMap,
            PtagCaseList),
        generate_primary_jump_table(PtagCaseList, 0, MaxPrimary, VarRval,
            CodeModel, SwitchGoalInfo, EndLabel, FailLabel, PtagCountMap,
            !MaybeEnd, Labels, TableCode, !CI),
        SwitchCode = node([
            computed_goto(PtagRval, Labels) - "switch on primary tag"
        ]),
        CasesCode = tree(SwitchCode, TableCode)
    ;
        PrimaryMethod = try_chain,
        switch_util.order_ptags_by_count(PtagCountList, PtagCaseMap,
            PtagCaseList0),
        (
            CanFail = cannot_fail,
            PtagCaseList0 = [MostFreqCase | OtherCases]
        ->
            list.append(OtherCases, [MostFreqCase], PtagCaseList)
        ;
            PtagCaseList = PtagCaseList0
        ),
        generate_primary_try_chain(PtagCaseList, PtagRval, VarRval, CodeModel,
            CanFail, SwitchGoalInfo, EndLabel, FailLabel, PtagCountMap,
            empty, empty, !MaybeEnd, CasesCode, !CI)
    ;
        PrimaryMethod = try_me_else_chain,
        switch_util.order_ptags_by_count(PtagCountList, PtagCaseMap,
            PtagCaseList),
        generate_primary_try_me_else_chain(PtagCaseList, PtagRval, VarRval,
            CodeModel, CanFail, SwitchGoalInfo, EndLabel, FailLabel,
            PtagCountMap, !MaybeEnd, CasesCode, !CI)
    ),
    Code = tree_list([VarCode, PtagCode, CasesCode, LabelledFailCode,
        EndCode]).

%-----------------------------------------------------------------------------%

    % Generate a switch on a primary tag value using a try-me-else chain.
    %
:- pred generate_primary_try_me_else_chain(ptag_case_list::in,
    rval::in, rval::in, code_model::in, can_fail::in, hlds_goal_info::in,
    label::in, label::in, ptag_count_map::in,
    branch_end::in, branch_end::out, code_tree::out,
    code_info::in, code_info::out) is det.

generate_primary_try_me_else_chain([], _, _, _, _, _, _, _, _, _, _, _, !CI) :-
    unexpected(this_file, "generate_primary_try_me_else_chain: empty switch").
generate_primary_try_me_else_chain([PtagGroup | PtagGroups], TagRval, VarRval,
        CodeModel, CanFail, SwitchGoalInfo, EndLabel, FailLabel, PtagCountMap,
        !MaybeEnd, Code, !CI) :-
    PtagGroup = Primary - ptag_case(StagLoc, StagGoalMap),
    map.lookup(PtagCountMap, Primary, CountInfo),
    CountInfo = StagLoc1 - MaxSecondary,
    expect(unify(StagLoc, StagLoc1), this_file,
        "generate_primary_try_me_else_chain: secondary tag locations differ"),
    (
        ( PtagGroups = [_ | _]
        ; CanFail = can_fail
        )
    ->
        code_info.remember_position(!.CI, BranchStart),
        code_info.get_next_label(ElseLabel, !CI),
        TestRval = binop(ne, TagRval,
            unop(mktag, const(int_const(Primary)))),
        TestCode = node([
            if_val(TestRval, label(ElseLabel)) - "test primary tag only"
        ]),
        generate_primary_tag_code(StagGoalMap, Primary, MaxSecondary, StagLoc,
            VarRval, CodeModel, SwitchGoalInfo, EndLabel, FailLabel, !MaybeEnd,
            TagCode, !CI),
        ElseCode = node([label(ElseLabel) - "handle next primary tag"]),
        ThisTagCode = tree_list([TestCode, TagCode, ElseCode]),
        (
            PtagGroups = [_ | _],
            code_info.reset_to_position(BranchStart, !CI),
            generate_primary_try_me_else_chain(PtagGroups, TagRval, VarRval,
                CodeModel, CanFail, SwitchGoalInfo, EndLabel, FailLabel,
                PtagCountMap, !MaybeEnd, OtherTagsCode, !CI),
            Code = tree(ThisTagCode, OtherTagsCode)
        ;
            PtagGroups = [],
            % FailLabel ought to be the next label anyway,
            % so this goto will be optimized away (unless the
            % layout of the failcode in the caller changes).
            FailCode = node([
                goto(label(FailLabel)) -
                    "primary tag with no code to handle it"
            ]),
            Code = tree(ThisTagCode, FailCode)
        )
    ;
        generate_primary_tag_code(StagGoalMap, Primary, MaxSecondary, StagLoc,
            VarRval, CodeModel, SwitchGoalInfo, EndLabel, FailLabel, !MaybeEnd,
            Code, !CI)
    ).

%-----------------------------------------------------------------------------%

    % Generate a switch on a primary tag value using a try chain.
    %
:- pred generate_primary_try_chain(ptag_case_list::in,
    rval::in, rval::in, code_model::in, can_fail::in, hlds_goal_info::in,
    label::in, label::in, ptag_count_map::in, code_tree::in, code_tree::in,
    branch_end::in, branch_end::out, code_tree::out,
    code_info::in, code_info::out) is det.

generate_primary_try_chain([], _, _, _, _, _, _, _, _, _, _, _, _, _, !CI) :-
     unexpected(this_file, "empty list in generate_primary_try_chain").
generate_primary_try_chain([PtagGroup | PtagGroups], TagRval, VarRval,
        CodeModel, CanFail, SwitchGoalInfo, EndLabel, FailLabel, PtagCountMap,
        PrevTests0, PrevCases0, !MaybeEnd, Code, !CI) :-
    PtagGroup = Primary - ptag_case(StagLoc, StagGoalMap),
    map.lookup(PtagCountMap, Primary, CountInfo),
    CountInfo = StagLoc1 - MaxSecondary,
    expect(unify(StagLoc, StagLoc1), this_file,
        "secondary tag locations differ in generate_primary_try_chain"),
    (
        ( PtagGroups = [_ | _]
        ; CanFail = can_fail
        )
    ->
        code_info.remember_position(!.CI, BranchStart),
        code_info.get_next_label(ThisPtagLabel, !CI),
        TestRval = binop(eq, TagRval,
            unop(mktag, const(int_const(Primary)))),
        TestCode = node([
            if_val(TestRval, label(ThisPtagLabel)) - "test primary tag only"
        ]),
        LabelCode = node([label(ThisPtagLabel) - "this primary tag"]),
        generate_primary_tag_code(StagGoalMap, Primary, MaxSecondary, StagLoc,
            VarRval, CodeModel, SwitchGoalInfo, EndLabel, FailLabel, !MaybeEnd,
            TagCode, !CI),
        PrevTests = tree(PrevTests0, TestCode),
        PrevCases = tree(tree(LabelCode, TagCode), PrevCases0),
        (
            PtagGroups = [_ | _],
            code_info.reset_to_position(BranchStart, !CI),
            generate_primary_try_chain(PtagGroups, TagRval, VarRval, CodeModel,
                CanFail, SwitchGoalInfo, EndLabel, FailLabel, PtagCountMap,
                PrevTests, PrevCases, !MaybeEnd, Code, !CI)
        ;
            PtagGroups = [],
            FailCode = node([
                goto(label(FailLabel)) -
                    "primary tag with no code to handle it"
            ]),
            Code = tree(PrevTests, tree(FailCode, PrevCases))
        )
    ;
        Comment = node([comment("fallthrough to last tag value") - ""]),
        generate_primary_tag_code(StagGoalMap, Primary, MaxSecondary, StagLoc,
            VarRval, CodeModel, SwitchGoalInfo, EndLabel, FailLabel, !MaybeEnd,
            TagCode, !CI),
        Code = tree_list([PrevTests0, Comment, TagCode, PrevCases0])
    ).

%-----------------------------------------------------------------------------%

    % Generate the cases for a primary tag using a dense jump table
    % that has an entry for all possible primary tag values.
    %
:- pred generate_primary_jump_table(ptag_case_list::in, int::in,
    int::in, rval::in, code_model::in, hlds_goal_info::in,
    label::in, label::in, ptag_count_map::in,
    branch_end::in, branch_end::out, list(label)::out, code_tree::out,
    code_info::in, code_info::out) is det.

generate_primary_jump_table(PtagGroups, CurPrimary, MaxPrimary, VarRval,
        CodeModel, SwitchGoalInfo, EndLabel, FailLabel, PtagCountMap,
        !MaybeEnd, Labels, Code, !CI) :-
    ( CurPrimary > MaxPrimary ->
        (
            PtagGroups = []
        ;
            PtagGroups = [_ | _],
            unexpected(this_file,
                "generate_primary_jump_table: " ++
                "caselist not empty when reaching limiting primary tag")
        ),
        Labels = [],
        Code = empty
    ;
        NextPrimary = CurPrimary + 1,
        ( PtagGroups = [CurPrimary - PrimaryInfo | PtagGroups1] ->
            PrimaryInfo = ptag_case(StagLoc, StagGoalMap),
            map.lookup(PtagCountMap, CurPrimary, CountInfo),
            CountInfo = StagLoc1 - MaxSecondary,
            expect(unify(StagLoc, StagLoc1), this_file,
                "secondary tag locations differ " ++
                "in generate_primary_jump_table"),
            code_info.get_next_label(NewLabel, !CI),
            LabelCode = node([
                label(NewLabel) - "start of a case in primary tag switch"
            ]),
            (
                PtagGroups1 = [],
                generate_primary_tag_code(StagGoalMap, CurPrimary,
                    MaxSecondary, StagLoc, VarRval, CodeModel, SwitchGoalInfo,
                    EndLabel, FailLabel, !MaybeEnd, ThisTagCode, !CI)
            ;
                PtagGroups1 = [_ | _],
                code_info.remember_position(!.CI, BranchStart),
                generate_primary_tag_code(StagGoalMap, CurPrimary,
                    MaxSecondary, StagLoc, VarRval, CodeModel, SwitchGoalInfo,
                    EndLabel, FailLabel, !MaybeEnd, ThisTagCode, !CI),
                code_info.reset_to_position(BranchStart, !CI)
            ),
            generate_primary_jump_table(PtagGroups1, NextPrimary, MaxPrimary,
                VarRval, CodeModel, SwitchGoalInfo, EndLabel, FailLabel,
                PtagCountMap, !MaybeEnd, OtherLabels, OtherCode, !CI),
            Labels = [NewLabel | OtherLabels],
            Code = tree_list([LabelCode, ThisTagCode, OtherCode])
        ;
            generate_primary_jump_table(PtagGroups, NextPrimary, MaxPrimary,
                VarRval, CodeModel, SwitchGoalInfo, EndLabel, FailLabel,
                PtagCountMap, !MaybeEnd, OtherLabels, Code, !CI),
            Labels = [FailLabel | OtherLabels]
        )
    ).

%-----------------------------------------------------------------------------%

    % Generate the cases for a primary tag using a binary search.
    % This invocation looks after primary tag values in the range
    % MinPtag to MaxPtag (including both boundary values).
    %
:- pred generate_primary_binary_search(ptag_case_list::in, int::in,
    int::in, rval::in, rval::in, code_model::in, can_fail::in,
    hlds_goal_info::in, label::in, label::in, ptag_count_map::in,
    branch_end::in, branch_end::out, code_tree::out,
    code_info::in, code_info::out) is det.

generate_primary_binary_search(PtagGroups, MinPtag, MaxPtag, PtagRval, VarRval,
        CodeModel, CanFail, SwitchGoalInfo, EndLabel, FailLabel, PtagCountMap,
        !MaybeEnd, Code, !CI) :-
    ( MinPtag = MaxPtag ->
        CurPrimary = MinPtag,
        (
            PtagGroups = [],
            % There is no code for this tag.
            (
                CanFail = can_fail,
                string.int_to_string(CurPrimary, PtagStr),
                string.append("no code for ptag ", PtagStr, Comment),
                Code = node([goto(label(FailLabel)) - Comment])
            ;
                CanFail = cannot_fail,
                Code = empty
            )
        ;
            PtagGroups = [CurPrimaryPrime - PrimaryInfo],
            expect(unify(CurPrimary, CurPrimaryPrime), this_file,
                "generate_primary_binary_search: cur_primary mismatch"),
            PrimaryInfo = ptag_case(StagLoc, StagGoalMap),
            map.lookup(PtagCountMap, CurPrimary, CountInfo),
            CountInfo = StagLoc1 - MaxSecondary,
            expect(unify(StagLoc, StagLoc1), this_file,
                "secondary tag locations differ " ++
                "in generate_primary_jump_table"),
            generate_primary_tag_code(StagGoalMap, CurPrimary, MaxSecondary,
                StagLoc, VarRval, CodeModel, SwitchGoalInfo,
                EndLabel, FailLabel, !MaybeEnd, Code, !CI)
        ;
            PtagGroups = [_, _ | _],
            unexpected(this_file,
                "caselist not singleton or empty when binary search ends")
        )
    ;
        LowRangeEnd = (MinPtag + MaxPtag) // 2,
        HighRangeStart = LowRangeEnd + 1,
        InLowGroup = (pred(PtagGroup::in) is semidet :-
            PtagGroup = Ptag - _,
            Ptag =< LowRangeEnd
        ),
        list.filter(InLowGroup, PtagGroups, LowGroups, HighGroups),
        code_info.get_next_label(NewLabel, !CI),
        string.int_to_string(MinPtag, LowStartStr),
        string.int_to_string(LowRangeEnd, LowEndStr),
        string.int_to_string(HighRangeStart, HighStartStr),
        string.int_to_string(MaxPtag, HighEndStr),
        string.append_list(["fallthrough for ptags ",
            LowStartStr, " to ", LowEndStr], IfComment),
        string.append_list(["code for ptags ", HighStartStr,
            " to ", HighEndStr], LabelComment),
        LowRangeEndConst = const(int_const(LowRangeEnd)),
        TestRval = binop(int_gt, PtagRval, LowRangeEndConst),
        IfCode = node([if_val(TestRval, label(NewLabel)) - IfComment]),
        LabelCode = node([label(NewLabel) - LabelComment]),

        code_info.remember_position(!.CI, BranchStart),
        generate_primary_binary_search(LowGroups, MinPtag, LowRangeEnd,
            PtagRval, VarRval, CodeModel, CanFail, SwitchGoalInfo,
            EndLabel, FailLabel, PtagCountMap, !MaybeEnd, LowRangeCode, !CI),
        code_info.reset_to_position(BranchStart, !CI),
        generate_primary_binary_search(HighGroups, HighRangeStart, MaxPtag,
            PtagRval, VarRval, CodeModel, CanFail, SwitchGoalInfo,
            EndLabel, FailLabel, PtagCountMap, !MaybeEnd, HighRangeCode, !CI),

        Code = tree_list([IfCode, LowRangeCode, LabelCode, HighRangeCode])
    ).

%-----------------------------------------------------------------------------%

    % Generate the code corresponding to a primary tag.
    % If this primary tag has secondary tags, decide whether we should
    % use a jump table to implement the secondary switch.
    %
:- pred generate_primary_tag_code(stag_goal_map::in, tag_bits::in,
    int::in, stag_loc::in, rval::in, code_model::in, hlds_goal_info::in,
    label::in, label::in, branch_end::in, branch_end::out, code_tree::out,
    code_info::in, code_info::out) is det.

generate_primary_tag_code(GoalMap, Primary, MaxSecondary, StagLoc, Rval,
        CodeModel, SwitchGoalInfo, EndLabel, FailLabel, !MaybeEnd, Code,
        !CI) :-
    map.to_assoc_list(GoalMap, GoalList),
    (
        StagLoc = none
    ->
        % There is no secondary tag, so there is no switch on it
        ( GoalList = [-1 - stag_goal(ConsId, Goal)] ->
            Comment = "case " ++ cons_id_to_string(ConsId),
            CommentCode = node([comment(Comment) - ""]),
            trace.maybe_generate_internal_event_code(Goal, SwitchGoalInfo,
                TraceCode, !CI),
            code_gen.generate_goal(CodeModel, Goal, GoalCode, !CI),
            goal_info_get_store_map(SwitchGoalInfo, StoreMap),
            code_info.generate_branch_end(StoreMap, !MaybeEnd, SaveCode, !CI),
            GotoCode = node([
                goto(label(EndLabel)) - "skip to end of primary tag switch"
            ]),
            Code = tree_list([CommentCode, TraceCode, GoalCode, SaveCode,
                GotoCode])
        ; GoalList = [] ->
            unexpected(this_file, "no goal for non-shared tag")
        ;
            unexpected(this_file, "more than one goal for non-shared tag")
        )
    ;
        % There is a secondary tag, so figure out how to switch on it
        code_info.get_globals(!.CI, Globals),
        globals.lookup_int_option(Globals, dense_switch_size,
            DenseSwitchSize),
        globals.lookup_int_option(Globals, binary_switch_size,
            BinarySwitchSize),
        globals.lookup_int_option(Globals, try_switch_size, TrySwitchSize),
        ( MaxSecondary >= DenseSwitchSize ->
            SecondaryMethod = jump_table
        ; MaxSecondary >= BinarySwitchSize ->
            SecondaryMethod = binary_search
        ; MaxSecondary >= TrySwitchSize ->
            SecondaryMethod = try_chain
        ;
            SecondaryMethod = try_me_else_chain
        ),

        ( StagLoc = remote ->
            OrigStagRval = lval(field(yes(Primary), Rval,
                const(int_const(0)))),
            Comment = "compute remote sec tag to switch on"
        ;
            OrigStagRval = unop(unmkbody, Rval),
            Comment = "compute local sec tag to switch on"
        ),

        code_info.acquire_reg(r, StagReg, !CI),
        code_info.release_reg(StagReg, !CI),
        (
            SecondaryMethod \= jump_table,
            MaxSecondary >= 2,
            globals.lookup_int_option(Globals, num_real_r_regs, NumRealRegs),
            (
                NumRealRegs = 0
            ;
                ( StagReg = reg(r, StagRegNo) ->
                    StagRegNo =< NumRealRegs
                ;
                    unexpected(this_file, "improper reg in tag switch")
                )
            )
        ->
            StagCode = node([assign(StagReg, OrigStagRval) - Comment]),
            StagRval = lval(StagReg)
        ;
            StagCode = empty,
            StagRval = OrigStagRval
        ),
        (
            list.length(GoalList, GoalCount),
            FullGoalCount = MaxSecondary + 1,
            FullGoalCount = GoalCount
        ->
            CanFail = cannot_fail
        ;
            CanFail = can_fail
        ),

        (
            SecondaryMethod = jump_table,
            generate_secondary_jump_table(GoalList, 0, MaxSecondary, CodeModel,
                SwitchGoalInfo, EndLabel, FailLabel, !MaybeEnd, Labels,
                CasesCode, !CI),
            SwitchCode = node([
                computed_goto(StagRval, Labels) - "switch on secondary tag"
            ]),
            Code = tree(SwitchCode, CasesCode)
        ;
            SecondaryMethod = binary_search,
            generate_secondary_binary_search(GoalList, 0, MaxSecondary,
                StagRval, CodeModel, CanFail, SwitchGoalInfo,
                EndLabel, FailLabel, !MaybeEnd, Code, !CI)
        ;
            SecondaryMethod = try_chain,
            generate_secondary_try_chain(GoalList, StagRval, CodeModel,
                CanFail, SwitchGoalInfo, EndLabel, FailLabel, empty, empty,
                !MaybeEnd, Codes, !CI),
            Code = tree(StagCode, Codes)
        ;
            SecondaryMethod = try_me_else_chain,
            generate_secondary_try_me_else_chain(GoalList, StagRval, CodeModel,
                CanFail, SwitchGoalInfo, EndLabel, FailLabel, !MaybeEnd,
                Codes, !CI),
            Code = tree(StagCode, Codes)
        )
    ).

%-----------------------------------------------------------------------------%

    % Generate a switch on a secondary tag value using a try-me-else chain.
    %
:- pred generate_secondary_try_me_else_chain(stag_goal_list::in,
    rval::in, code_model::in, can_fail::in, hlds_goal_info::in,
    label::in, label::in, branch_end::in, branch_end::out, code_tree::out,
    code_info::in, code_info::out) is det.

generate_secondary_try_me_else_chain([], _, _, _, _, _, _, _, _, _, !CI) :-
    unexpected(this_file,
        "generate_secondary_try_me_else_chain: empty switch").
generate_secondary_try_me_else_chain([Case0 | Cases0], StagRval, CodeModel,
        CanFail, SwitchGoalInfo, EndLabel, FailLabel, !MaybeEnd, Code, !CI) :-
    Case0 = Secondary - stag_goal(ConsId, Goal),
    Comment = "case " ++ cons_id_to_string(ConsId),
    CommentCode = node([comment(Comment) - ""]),
    goal_info_get_store_map(SwitchGoalInfo, StoreMap),
    (
        ( Cases0 = [_ | _]
        ; CanFail = can_fail
        )
    ->
        code_info.remember_position(!.CI, BranchStart),
        code_info.get_next_label(ElseLabel, !CI),
        TestCode = node([
            if_val(binop(ne, StagRval, const(int_const(Secondary))),
                label(ElseLabel))
                - "test remote sec tag only"
        ]),
        trace.maybe_generate_internal_event_code(Goal, SwitchGoalInfo,
            TraceCode, !CI),
        code_gen.generate_goal(CodeModel, Goal, GoalCode, !CI),
        code_info.generate_branch_end(StoreMap, !MaybeEnd,
            SaveCode, !CI),
        GotoLabelCode = node([
            goto(label(EndLabel)) - "skip to end of secondary tag switch",
            label(ElseLabel) - "handle next secondary tag"
        ]),
        ThisCode = tree_list([TestCode, CommentCode, TraceCode, GoalCode,
            SaveCode, GotoLabelCode]),
        (
            Cases0 = [_ | _],
            code_info.reset_to_position(BranchStart, !CI),
            generate_secondary_try_me_else_chain(Cases0, StagRval, CodeModel,
                CanFail, SwitchGoalInfo, EndLabel, FailLabel, !MaybeEnd,
                OtherCode, !CI),
            Code = tree(ThisCode, OtherCode)
        ;
            Cases0 = [],
            FailCode = node([
                goto(label(FailLabel)) - "secondary tag does not match"
            ]),
            Code = tree(ThisCode, FailCode)
        )
    ;
        trace.maybe_generate_internal_event_code(Goal, SwitchGoalInfo,
            TraceCode, !CI),
        code_gen.generate_goal(CodeModel, Goal, GoalCode, !CI),
        code_info.generate_branch_end(StoreMap, !MaybeEnd, SaveCode, !CI),
        GotoCode = node([
            goto(label(EndLabel)) - "skip to end of secondary tag switch"
        ]),
        Code = tree_list([CommentCode, TraceCode, GoalCode, SaveCode,
            GotoCode])
    ).

%-----------------------------------------------------------------------------%

    % Generate a switch on a secondary tag value using a try chain.
    %
:- pred generate_secondary_try_chain(stag_goal_list::in, rval::in,
    code_model::in, can_fail::in, hlds_goal_info::in, label::in, label::in,
    code_tree::in, code_tree::in, branch_end::in, branch_end::out,
    code_tree::out, code_info::in, code_info::out) is det.

generate_secondary_try_chain([], _, _, _, _, _, _, _, _, _, _, _, !CI) :-
    unexpected(this_file, "generate_secondary_try_chain: empty switch").
generate_secondary_try_chain([Case0 | Cases0], StagRval, CodeModel, CanFail,
        SwitchGoalInfo, EndLabel, FailLabel, PrevTests0, PrevCases0, !MaybeEnd,
        Code, !CI) :-
    Case0 = Secondary - stag_goal(ConsId, Goal),
    Comment = "case " ++ cons_id_to_string(ConsId),
    goal_info_get_store_map(SwitchGoalInfo, StoreMap),
    (
        ( Cases0 = [_ | _]
        ; CanFail = can_fail
        )
    ->
        code_info.remember_position(!.CI, BranchStart),
        code_info.get_next_label(ThisStagLabel, !CI),
        TestCode = node([
            if_val(binop(eq, StagRval, const(int_const(Secondary))),
                label(ThisStagLabel))
                - ("test remote sec tag only for " ++ Comment)
        ]),
        LabelCode = node([
            label(ThisStagLabel)
                - ("handle next secondary tag for " ++ Comment)
        ]),
        trace.maybe_generate_internal_event_code(Goal, SwitchGoalInfo,
            TraceCode, !CI),
        code_gen.generate_goal(CodeModel, Goal, GoalCode, !CI),
        code_info.generate_branch_end(StoreMap, !MaybeEnd, SaveCode, !CI),
        GotoCode = node([
            goto(label(EndLabel)) - "skip to end of secondary tag switch"
        ]),
        ThisCode = tree_list([LabelCode, TraceCode, GoalCode, SaveCode,
            GotoCode]),
        PrevTests = tree(PrevTests0, TestCode),
        PrevCases = tree(ThisCode, PrevCases0),
        (
            Cases0 = [_ | _],
            code_info.reset_to_position(BranchStart, !CI),
            generate_secondary_try_chain(Cases0, StagRval, CodeModel, CanFail,
                SwitchGoalInfo, EndLabel, FailLabel, PrevTests, PrevCases,
                !MaybeEnd, Code, !CI)
        ;
            Cases0 = [],
            FailCode = node([
                goto(label(FailLabel)) -
                    "secondary tag with no code to handle it"
            ]),
            Code = tree(PrevTests, tree(FailCode, PrevCases))
        )
    ;
        CommentCode = node([comment(Comment) - ""]),
        trace.maybe_generate_internal_event_code(Goal, SwitchGoalInfo,
            TraceCode, !CI),
        code_gen.generate_goal(CodeModel, Goal, GoalCode, !CI),
        code_info.generate_branch_end(StoreMap, !MaybeEnd, SaveCode, !CI),
        GotoCode = node([
            goto(label(EndLabel)) - "skip to end of secondary tag switch"
        ]),
        Code = tree_list([PrevTests0, CommentCode, TraceCode, GoalCode,
            SaveCode, GotoCode, PrevCases0])
    ).

%-----------------------------------------------------------------------------%

    % Generate the cases for a primary tag using a dense jump table
    % that has an entry for all possible secondary tag values.
    %
:- pred generate_secondary_jump_table(stag_goal_list::in, int::in,
    int::in, code_model::in, hlds_goal_info::in, label::in, label::in,
    branch_end::in, branch_end::out, list(label)::out, code_tree::out,
    code_info::in, code_info::out) is det.

generate_secondary_jump_table(CaseList, CurSecondary, MaxSecondary, CodeModel,
        SwitchGoalInfo, EndLabel, FailLabel, !MaybeEnd, Labels, Code, !CI) :-
    ( CurSecondary > MaxSecondary ->
        expect(unify(CaseList, []), this_file,
            "caselist not empty when reaching limiting secondary tag"),
        Labels = [],
        Code = empty
    ;
        NextSecondary = CurSecondary + 1,
        ( CaseList = [CurSecondary - stag_goal(ConsId, Goal) | CaseList1] ->
            Comment = "case " ++ cons_id_to_string(ConsId),
            code_info.get_next_label(NewLabel, !CI),
            LabelCode = node([
                label(NewLabel) -
                    ("start of " ++ Comment ++ " in secondary tag switch")
            ]),
            code_info.remember_position(!.CI, BranchStart),
            trace.maybe_generate_internal_event_code(Goal, SwitchGoalInfo,
                TraceCode, !CI),
            code_gen.generate_goal(CodeModel, Goal, GoalCode, !CI),
            goal_info_get_store_map(SwitchGoalInfo, StoreMap),
            code_info.generate_branch_end(StoreMap, !MaybeEnd, SaveCode, !CI),
            (
                CaseList1 = []
            ;
                CaseList1 = [_ | _],
                code_info.reset_to_position(BranchStart, !CI)
            ),
            GotoCode = node([
                goto(label(EndLabel)) - "branch to end of tag switch"
            ]),
            generate_secondary_jump_table(CaseList1, NextSecondary,
                MaxSecondary, CodeModel, SwitchGoalInfo, EndLabel, FailLabel,
                !MaybeEnd, OtherLabels, OtherCode, !CI),
            Labels = [NewLabel | OtherLabels],
            Code = tree_list([LabelCode, TraceCode, GoalCode, SaveCode, 
                GotoCode, OtherCode])
        ;
            generate_secondary_jump_table(CaseList,
                NextSecondary, MaxSecondary, CodeModel, SwitchGoalInfo,
                EndLabel, FailLabel, !MaybeEnd, OtherLabels, Code, !CI),
            Labels = [FailLabel | OtherLabels]
        )
    ).

%-----------------------------------------------------------------------------%

    % Generate the cases for a secondary tag using a binary search.
    % This invocation looks after secondary tag values in the range
    % MinPtag to MaxPtag (including both boundary values).
    %
:- pred generate_secondary_binary_search(stag_goal_list::in,
    int::in, int::in, rval::in, code_model::in, can_fail::in,
    hlds_goal_info::in, label::in, label::in,
    branch_end::in, branch_end::out, code_tree::out,
    code_info::in, code_info::out) is det.

generate_secondary_binary_search(StagGoals, MinStag, MaxStag, StagRval,
        CodeModel, CanFail, SwitchGoalInfo, EndLabel, FailLabel, !MaybeEnd,
        Code, !CI) :-
    ( MinStag = MaxStag ->
        CurSec = MinStag,
        (
            StagGoals = [],
            % There is no code for this tag.
            (
                CanFail = can_fail,
                string.int_to_string(CurSec, StagStr),
                string.append("no code for ptag ", StagStr, Comment),
                Code = node([goto(label(FailLabel)) - Comment])
            ;
                CanFail = cannot_fail,
                Code = empty
            )
        ;
            StagGoals = [CurSecPrime - stag_goal(ConsId, Goal)],
            Comment = "case " ++ cons_id_to_string(ConsId),
            CommentCode = node([comment(Comment) - ""]),
            expect(unify(CurSec, CurSecPrime), this_file,
                "generate_secondary_binary_search: cur_secondary mismatch"),
            trace.maybe_generate_internal_event_code(Goal, SwitchGoalInfo,
                TraceCode, !CI),
            code_gen.generate_goal(CodeModel, Goal, GoalCode, !CI),
            goal_info_get_store_map(SwitchGoalInfo, StoreMap),
            code_info.generate_branch_end(StoreMap, !MaybeEnd, SaveCode, !CI),
            Code = tree_list([CommentCode, TraceCode, GoalCode, SaveCode])
        ;
            StagGoals = [_, _ | _],
            unexpected(this_file,
                "generate_secondary_binary_search: " ++
                "goallist not singleton or empty when binary search ends")
        )
    ;
        LowRangeEnd = (MinStag + MaxStag) // 2,
        HighRangeStart = LowRangeEnd + 1,
        InLowGroup = (pred(StagGoal::in) is semidet :-
            StagGoal = Stag - _,
            Stag =< LowRangeEnd
        ),
        list.filter(InLowGroup, StagGoals, LowGoals, HighGoals),
        code_info.get_next_label(NewLabel, !CI),
        string.int_to_string(MinStag, LowStartStr),
        string.int_to_string(LowRangeEnd, LowEndStr),
        string.int_to_string(HighRangeStart, HighStartStr),
        string.int_to_string(MaxStag, HighEndStr),
        string.append_list(["fallthrough for stags ",
            LowStartStr, " to ", LowEndStr], IfComment),
        string.append_list(["code for stags ", HighStartStr,
            " to ", HighEndStr], LabelComment),
        LowRangeEndConst = const(int_const(LowRangeEnd)),
        TestRval = binop(int_gt, StagRval, LowRangeEndConst),
        IfCode = node([if_val(TestRval, label(NewLabel)) - IfComment]),
        LabelCode = node([label(NewLabel) - LabelComment ]),

        code_info.remember_position(!.CI, BranchStart),
        generate_secondary_binary_search(LowGoals, MinStag, LowRangeEnd,
            StagRval, CodeModel, CanFail, SwitchGoalInfo, EndLabel, FailLabel,
            !MaybeEnd, LowRangeCode, !CI),
        code_info.reset_to_position(BranchStart, !CI),
        generate_secondary_binary_search(HighGoals, HighRangeStart, MaxStag,
            StagRval, CodeModel, CanFail, SwitchGoalInfo, EndLabel, FailLabel,
            !MaybeEnd, HighRangeCode, !CI),

        Code = tree_list([IfCode, LowRangeCode, LabelCode, HighRangeCode])
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "tag_switch.m".

%-----------------------------------------------------------------------------%
