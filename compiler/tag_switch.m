%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2000,2002-2007, 2009-2011 The University of Melbourne.
% Copyright (C) 2015-2018, 2020, 2024-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: tag_switch.m.
% Author: zs.
%
% Generate switches based on primary and secondary tags.
%
%---------------------------------------------------------------------------%

:- module ll_backend.tag_switch.
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

%---------------------------------------------------------------------------%

    % Generate intelligent indexing code for tag based switches.
    %
:- pred generate_tag_switch(rval::in, mer_type::in, string::in,
    list(tagged_case)::in, code_model::in, can_fail::in, hlds_goal_info::in,
    label::in, branch_end::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.tag_switch_util.
:- import_module hlds.hlds_llds.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module ll_backend.code_util.
:- import_module ll_backend.switch_case.

:- import_module assoc_list.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module uint.
:- import_module uint8.
:- import_module unit.

%---------------------------------------------------------------------------%

    % The idea is to generate two-level switches, first on the primary
    % tag and then (if the primary tag is shared) on the secondary tag.
    % Since more than one function symbol can be eliminated by a
    % failed primary tag test, this reduces the expected the number
    % of comparisons required before finding the code corresponding to
    % the actual value of the switch variable.
    %
    % We also get a speedup compared to non-tag switches by extracting
    % the primary and secondary tags just once, instead of repeatedly for
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
    % Which method is best depends
    % - on the number of possible tag values,
    % - on the costs of taken/untaken branches and table lookups on the given
    %   architecture, and
    % - on the frequency with which the various alternatives are taken.
    %
    % While the first two can be known at compile time (at least in principle),
    % the third is not (at least not without feedback from a profiler).
    % Nevertheless, for switches on primary tags we can use the heuristic
    % that the more secondary tags assigned to a primary tag, the more likely
    % it is that the switch variable will have that primary tag at runtime.
    %
    % Try-me-else chains are good for switches with very small numbers of
    % alternatives on architectures where taken branches are cheaper than
    % untaken branches (which are rare these days).
    %
    % Try chains are good for switches with small numbers of alternatives
    % on architectures where untaken branches are cheaper than taken
    % branches.
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
    % tag2 the next most frequent case, etc.
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

%---------------------------------------------------------------------------%

generate_tag_switch(VarRval, VarType, VarName, TaggedCases, CodeModel, CanFail,
        SwitchGoalInfo, EndLabel, !MaybeEnd, Code, !CI, CLD0) :-
    % We get registers for holding the primary and (if needed) the secondary
    % tag. The tags are needed only by the switch, and no other code gets
    % control between producing the tag values and all their uses, so
    % we can immediately release the registers for use by the code of
    % the various cases.
    %
    % We need to get and release the registers before we generate the code
    % of the switch arms, since the set of free registers will in general be
    % different before and after that action.
    %
    % We forgo using the primary tag register if the primary tag is needed
    % only once, or if the "register" we get is likely to be slower than
    % recomputing the tag from scratch.
    some [!CLD] (
        !:CLD = CLD0,
        acquire_reg(reg_r, PtagReg, !CLD),
        acquire_reg(reg_r, SectagReg, !CLD),
        release_reg(PtagReg, !CLD),
        release_reg(SectagReg, !CLD),
        remember_position(!.CLD, BranchStart)
    ),

    % Group the cases based on primary tag value and find out how many
    % constructors share each primary tag value.
    get_module_info(!.CI, ModuleInfo),
    % get_ptag_counts(ModuleInfo, VarType, MaxPtagUint8, PtagCountMap),
    Params = represent_params(VarName, SwitchGoalInfo, CodeModel, BranchStart,
        EndLabel),
    group_cases_by_ptag(ModuleInfo, represent_tagged_case_for_llds(Params),
        VarType, TaggedCases, map.init, CaseLabelMap0, !MaybeEnd, !CI, unit, _,
        PtagGroups0, CaseRepGoalMap, NumPtagsUsed, MaxPtagUint8),

    get_globals(!.CI, Globals),
    Method = choose_switch_method(Globals, NumPtagsUsed),
    compute_ptag_rval(Globals, VarRval, PtagReg, NumPtagsUsed, Method,
        PtagRval, PtagRvalCode),

    % We must generate the failure code in the context in which
    % none of the switch arms have been executed yet.
    (
        CanFail = cannot_fail,
        MaybeFailLabel = no,
        FailCode = empty
    ;
        CanFail = can_fail,
        get_next_label(FailLabel, !CI),
        MaybeFailLabel = yes(FailLabel),
        FailLabelCode = singleton(
            llds_instr(label(FailLabel), "switch has failed")
        ),
        some [!CLD] (
            reset_to_position(BranchStart, !.CI, !:CLD),
            generate_failure(FailureCode, !.CI, !.CLD)
        ),
        FailCode = FailLabelCode ++ FailureCode
    ),

    (
        Method = try_me_else_chain,
        order_ptag_groups_by_count(CaseRepGoalMap, PtagGroups0, PtagGroups),
        list.det_head_tail(PtagGroups, HeadPtagGroup, TailPtagGroups),
        generate_primary_try_me_else_chain(VarRval, PtagRval,
            SectagReg, MaybeFailLabel, HeadPtagGroup, TailPtagGroups,
            CasesCode, CaseLabelMap0, CaseLabelMap1, !CI),
        add_not_yet_included_cases(RemainingCasesCode,
            CaseLabelMap1, _CaseLabelMap)
    ;
        Method = try_chain,
        order_ptag_groups_by_count(CaseRepGoalMap, PtagGroups0, PtagGroups1),
        ( if
            CanFail = cannot_fail,
            PtagGroups1 = [MostFreqGroup | OtherGroups]
        then
            % NOTE See the end of the comment on put_an_expensive_test_last
            % for the reason for this reordering.
            PtagGroups = OtherGroups ++ [MostFreqGroup]
        else
            PtagGroups = PtagGroups1
        ),
        list.det_head_tail(PtagGroups, HeadPtagGroup, TailPtagGroups),
        generate_primary_try_chain(VarRval, PtagRval, SectagReg,
            MaybeFailLabel, HeadPtagGroup, TailPtagGroups,
            empty, empty, CasesCode, CaseLabelMap0, CaseLabelMap1, !CI),
        add_not_yet_included_cases(RemainingCasesCode,
            CaseLabelMap1, _CaseLabelMap)
    ;
        Method = jump_table,
        order_ptag_specific_groups_by_value(PtagGroups0, PtagGroups),
        % Generate the code for all the cases now, even though we will add
        % the resulting code at the end, so that we don't intersperse the
        % code of the cases (RemainingCasesCode) with TableCode, which
        % will contain the code for any needed switches on secondary tags.
        % (Ptags that don't have an associated secondary tag won't have
        % any code in TableCode.)
        add_not_yet_included_cases(RemainingCasesCode,
            CaseLabelMap0, CaseLabelMap),
        generate_primary_jump_table(CaseLabelMap, VarRval, SectagReg,
            MaybeFailLabel, PtagGroups, 0u8, MaxPtagUint8,
            TargetMaybeLabels, TableCode, !CI),
        MaxPtag = uint8.cast_to_int(MaxPtagUint8),
        SwitchCode = singleton(
            llds_instr(
                computed_goto(PtagRval, yes(MaxPtag), TargetMaybeLabels),
                "switch on ptag")
        ),
        CasesCode = SwitchCode ++ TableCode
    ;
        Method = binary_search,
        order_ptag_specific_groups_by_value(PtagGroups0, PtagGroups),
        generate_primary_binary_search(VarRval, PtagRval, SectagReg,
            MaybeFailLabel, PtagGroups, 0u8, MaxPtagUint8,
            CasesCode, CaseLabelMap0, CaseLabelMap1, !CI),
        add_not_yet_included_cases(RemainingCasesCode,
            CaseLabelMap1, _CaseLabelMap)
    ),
    EndCode = singleton(
        llds_instr(label(EndLabel), "end of tag switch")
    ),
    Code = PtagRvalCode ++ CasesCode ++ RemainingCasesCode ++
        FailCode ++ EndCode.

:- pred compute_ptag_rval(globals::in, rval::in, lval::in, int::in,
    switch_method::in, rval::out, llds_code::out) is det.

compute_ptag_rval(Globals, VarRval, PtagReg, NumPtagsUsed,
        Method, PtagRval, PtagRvalCode) :-
    AccessCount = switch_method_tag_access_count(Method),
    ( if
        AccessCount = more_than_one_access,
        NumPtagsUsed >= 2,
        globals.lookup_int_option(Globals, num_real_r_regs, NumRealRegs),
        (
            NumRealRegs = 0
        ;
            ( if PtagReg = reg(reg_r, PtagRegNum) then
                PtagRegNum =< NumRealRegs
            else
                unexpected($pred, "improper reg in tag switch")
            )
        )
    then
        PtagRval = lval(PtagReg),
        PtagRvalCode = singleton(
            llds_instr(assign(PtagReg, unop(tag, VarRval)),
                "compute tag to switch on")
        )
    else
        PtagRval = unop(tag, VarRval),
        PtagRvalCode = empty
    ).

%---------------------------------------------------------------------------%
%
% try-me-else chain switches on ptags.
%

    % Generate a switch on a primary tag value using a try-me-else chain.
    %
:- pred generate_primary_try_me_else_chain(rval::in, rval::in, lval::in,
    maybe(label)::in,
    ptag_case_group(label)::in, list(ptag_case_group(label))::in,
    llds_code::out, case_label_map::in, case_label_map::out,
    code_info::in, code_info::out) is det.

generate_primary_try_me_else_chain(VarRval, PtagRval, SectagReg,
        MaybeFailLabel, HeadPtagGroup, TailPtagGroups, Code,
        !CaseLabelMap, !CI) :-
    (
        TailPtagGroups = [HeadTailPtagGroup | TailTailPtagGroups],
        generate_primary_try_me_else_chain_group(VarRval, PtagRval, SectagReg,
            MaybeFailLabel, HeadPtagGroup, HeadGroupCode, !CaseLabelMap, !CI),
        generate_primary_try_me_else_chain(VarRval, PtagRval, SectagReg,
            MaybeFailLabel, HeadTailPtagGroup, TailTailPtagGroups,
            TailGroupsCode, !CaseLabelMap, !CI),
        Code = HeadGroupCode ++ TailGroupsCode
    ;
        TailPtagGroups = [],
        (
            MaybeFailLabel = yes(FailLabel),
            generate_primary_try_me_else_chain_group(VarRval, PtagRval,
                SectagReg, MaybeFailLabel, HeadPtagGroup, HeadGroupCode,
                !CaseLabelMap, !CI),
            % FailLabel ought to be the next label anyway, so this goto
            % will be optimized away (unless the layout of the failcode
            % in the caller changes).
            FailCode = singleton(
                llds_instr(goto(code_label(FailLabel)),
                    "ptag with no code to handle it")
            ),
            Code = HeadGroupCode ++ FailCode
        ;
            MaybeFailLabel = no,
            generate_ptag_group_code(VarRval, SectagReg, MaybeFailLabel,
                HeadPtagGroup, Code, !CaseLabelMap, !CI)
        )
    ).

:- pred generate_primary_try_me_else_chain_group(rval::in, rval::in, lval::in,
    maybe(label)::in, ptag_case_group(label)::in, llds_code::out,
    case_label_map::in, case_label_map::out,
    code_info::in, code_info::out) is det.

generate_primary_try_me_else_chain_group(VarRval, PtagRval, SectagReg,
        MaybeFailLabel, PtagGroup, Code, !CaseLabelMap, !CI) :-
    get_next_label(ElseLabel, !CI),
    test_ptag_is_in_case_group(PtagRval, PtagGroup, IsApplicableTestRval),
    negate_rval(IsApplicableTestRval, IsNotApplicableRval),
    TestCode = singleton(
        llds_instr(if_val(IsNotApplicableRval, code_label(ElseLabel)),
            "test ptag(s) only")
    ),
    generate_ptag_group_code(VarRval, SectagReg, MaybeFailLabel,
        PtagGroup, CaseCode, !CaseLabelMap, !CI),
    ElseCode = singleton(
        llds_instr(label(ElseLabel), "handle next ptag")
    ),
    Code = TestCode ++ CaseCode ++ ElseCode.

%---------------------------------------------------------------------------%
%
% Try chain switches on ptags.
%

    % Generate a switch on a primary tag value using a try chain.
    %
:- pred generate_primary_try_chain(rval::in, rval::in, lval::in,
    maybe(label)::in,
    ptag_case_group(label)::in, list(ptag_case_group(label))::in,
    llds_code::in, llds_code::in, llds_code::out,
    case_label_map::in, case_label_map::out,
    code_info::in, code_info::out) is det.

generate_primary_try_chain(VarRval, PtagRval, SectagReg, MaybeFailLabel,
        HeadPtagGroup, TailPtagGroups, !.TryChainCode, !.GroupsCode, Code,
        !CaseLabelMap, !CI) :-
    (
        TailPtagGroups = [HeadTailPtagGroup | TailTailPtagGroups],
        generate_primary_try_chain_case(VarRval, PtagRval, SectagReg,
            MaybeFailLabel, HeadPtagGroup, !TryChainCode, !GroupsCode,
            !CaseLabelMap, !CI),
        generate_primary_try_chain(VarRval, PtagRval, SectagReg,
            MaybeFailLabel, HeadTailPtagGroup, TailTailPtagGroups,
            !.TryChainCode, !.GroupsCode, Code, !CaseLabelMap, !CI)
    ;
        TailPtagGroups = [],
        (
            MaybeFailLabel = yes(FailLabel),
            generate_primary_try_chain_case(VarRval, PtagRval, SectagReg,
                MaybeFailLabel, HeadPtagGroup,
                !TryChainCode, !GroupsCode, !CaseLabelMap, !CI),
            FailCode = singleton(
                llds_instr(goto(code_label(FailLabel)),
                    "ptag with no code to handle it")
            ),
            Code = !.TryChainCode ++ FailCode ++ !.GroupsCode
        ;
            MaybeFailLabel = no,
            make_ptag_comment("fallthrough to last ptag value: ",
                HeadPtagGroup, Comment),
            CommentCode = singleton(
                llds_instr(comment(Comment), "")
            ),
            generate_ptag_group_code(VarRval, SectagReg, MaybeFailLabel,
                HeadPtagGroup, GroupCode, !CaseLabelMap, !CI),
            Code = !.TryChainCode ++ CommentCode ++ GroupCode ++ !.GroupsCode
        )
    ).

:- pred generate_primary_try_chain_case(rval::in, rval::in, lval::in,
    maybe(label)::in, ptag_case_group(label)::in,
    llds_code::in, llds_code::out, llds_code::in, llds_code::out,
    case_label_map::in, case_label_map::out,
    code_info::in, code_info::out) is det.

generate_primary_try_chain_case(VarRval, PtagRval, SectagReg, MaybeFailLabel,
        PtagGroup, !TryChainCode, !GroupsCode, !CaseLabelMap, !CI) :-
    get_next_label(ThisGroupLabel, !CI),
    test_ptag_is_in_case_group(PtagRval, PtagGroup, IsApplicableTestRval),
    TestCode = singleton(
        llds_instr(if_val(IsApplicableTestRval, code_label(ThisGroupLabel)),
            "test ptag only")
    ),
    make_ptag_comment("ptag value(s): ", PtagGroup, Comment),
    LabelCode = singleton(
        llds_instr(label(ThisGroupLabel), Comment)
    ),
    generate_ptag_group_code(VarRval, SectagReg, MaybeFailLabel,
        PtagGroup, GroupCode, !CaseLabelMap, !CI),
    LabelledGroupCode = LabelCode ++ GroupCode,
    !:TryChainCode = !.TryChainCode ++ TestCode,
    !:GroupsCode = LabelledGroupCode ++ !.GroupsCode.

%---------------------------------------------------------------------------%
%
% Infrastructure needed for both try-me-else and try chain switches on ptags.
%

:- pred test_ptag_is_in_case_group(rval::in, ptag_case_group(label)::in,
    rval::out) is det.

test_ptag_is_in_case_group(PtagRval, PtagGroup, TestRval) :-
    (
        PtagGroup = one_or_more_whole_ptags(WholeInfo),
        % Note: OtherPtags may be [] here too.
        WholeInfo = whole_ptags_info(MainPtag, OtherPtags, _, _)
    ;
        PtagGroup = one_shared_ptag(SharedInfo),
        SharedInfo = shared_ptag_info(MainPtag, _, _, _, _, _, _),
        OtherPtags = []
    ),
    test_ptag_is_in_set(PtagRval, MainPtag, OtherPtags, TestRval).

%---------------------------------------------------------------------------%
%
% Jump table switches on ptags.
%

    % Generate the cases for a primary tag using a dense jump table
    % that has an entry for all possible primary tag values.
    %
:- pred generate_primary_jump_table(case_label_map::in, rval::in, lval::in,
    maybe(label)::in, list(single_ptag_case(label))::in, uint8::in, uint8::in,
    list(maybe(label))::out, llds_code::out,
    code_info::in, code_info::out) is det.

generate_primary_jump_table(CaseLabelMap, VarRval, SectagReg, MaybeFailLabel,
        SinglePtagGroups, CurPtagUint8, MaxPtagUint8,
        TargetMaybeLabels, Code, !CI) :-
    ( if CurPtagUint8 > MaxPtagUint8 then
        TargetMaybeLabels = [],
        Code = empty
    else
        NextPtagUint8 = CurPtagUint8 + 1u8,
        ( if
            SinglePtagGroups = [SinglePtagGroup | TailSinglePtagGroups],
            (
                SinglePtagGroup = one_or_more_whole_ptags(WholeInfo0),
                WholeInfo0 = whole_ptags_info(MainPtag, _, _, _)
            ;
                SinglePtagGroup = one_shared_ptag(SharedInfo0),
                SharedInfo0 = shared_ptag_info(MainPtag, _, _, _, _, _, _)
            ),
            MainPtag = ptag(CurPtagUint8)
        then
            (
                SinglePtagGroup = one_or_more_whole_ptags(WholeInfo),
                WholeInfo = whole_ptags_info(_, _, _, Label),
                HeadMaybeTargetLabel = yes(Label),
                HeadEntryCode = empty
            ;
                SinglePtagGroup = one_shared_ptag(_),
                get_next_label(ThisPtagLabel, !CI),
                Comment = "start of a shared ptag in ptag jump table: " ++
                    string.uint8_to_string(CurPtagUint8),
                LabelCode = singleton(
                    llds_instr(label(ThisPtagLabel), Comment)
                ),
                PtagGroup = coerce(SinglePtagGroup),
                % Our caller has already generated the code for all the labels
                % in CaseLabelMap.
                generate_ptag_group_code(VarRval, SectagReg, MaybeFailLabel,
                    PtagGroup, HeadEntryCode0, CaseLabelMap, _, !CI),
                HeadMaybeTargetLabel = yes(ThisPtagLabel),
                HeadEntryCode = LabelCode ++ HeadEntryCode0
            ),
            NextSinglePtagGroups = TailSinglePtagGroups
        else
            HeadMaybeTargetLabel = MaybeFailLabel,
            HeadEntryCode = empty,
            NextSinglePtagGroups = SinglePtagGroups
        ),
        generate_primary_jump_table(CaseLabelMap, VarRval, SectagReg,
            MaybeFailLabel, NextSinglePtagGroups, NextPtagUint8, MaxPtagUint8,
            TailTargetMaybeLabels, TailEntriesCode, !CI),
        TargetMaybeLabels = [HeadMaybeTargetLabel | TailTargetMaybeLabels],
        Code = HeadEntryCode ++ TailEntriesCode
    ).

:- func ptag_case_group_main_ptag(ptag_case_group(CaseRep)) = ptag.

ptag_case_group_main_ptag(PtagGroup) = MainPtag :-
    (
        PtagGroup = one_or_more_whole_ptags(WholeInfo),
        WholeInfo = whole_ptags_info(MainPtag, _, _, _)
    ;
        PtagGroup = one_shared_ptag(SharedInfo),
        SharedInfo = shared_ptag_info(MainPtag, _, _, _, _, _, _)
    ).

%---------------------------------------------------------------------------%
%
% Binary search switches on ptags.
%

    % Generate the cases for a primary tag using a binary search.
    % This invocation looks after primary tag values in the range
    % MinPtag to MaxPtag (including both boundary values).
    %
:- pred generate_primary_binary_search(rval::in, rval::in, lval::in,
    maybe(label)::in, list(single_ptag_case(label))::in,
    uint8::in, uint8::in, llds_code::out,
    case_label_map::in, case_label_map::out,
    code_info::in, code_info::out) is det.

generate_primary_binary_search(VarRval, PtagRval, SectagReg,
        MaybeFailLabel, SinglePtagGroups, MinPtag, MaxPtag, Code,
        !CaseLabelMap, !CI) :-
    ( if MinPtag = MaxPtag then
        CurPtagUint8 = MinPtag,
        (
            SinglePtagGroups = [],
            % There is no code for this tag.
            (
                MaybeFailLabel = yes(FailLabel),
                PtagStr = string.uint8_to_string(CurPtagUint8),
                Comment = "no code for ptag " ++ PtagStr,
                Code = singleton(
                    llds_instr(goto(code_label(FailLabel)), Comment)
                )
            ;
                MaybeFailLabel = no,
                % The switch is cannot_fail, which means this case cannot
                % happen at runtime.
                Code = empty
            )
        ;
            SinglePtagGroups = [SinglePtagGroup],
            PtagGroup = coerce(SinglePtagGroup),
            MainPtag = ptag_case_group_main_ptag(PtagGroup),
            expect(unify(ptag(CurPtagUint8), MainPtag), $pred,
                "cur_primary mismatch"),
            generate_ptag_group_code(VarRval, SectagReg, MaybeFailLabel,
                PtagGroup, Code, !CaseLabelMap, !CI)
        ;
            SinglePtagGroups = [_, _ | _],
            unexpected($pred,
                "ptag groups not singleton or empty when binary search ends")
        )
    else
        LoRangeMax = (MinPtag + MaxPtag) // 2u8,
        EqHiRangeMin = LoRangeMax + 1u8,
        InLoGroup =
            ( pred(SPG::in) is semidet :-
                ptag(MainPtagUint8) = ptag_case_group_main_ptag(coerce(SPG)),
                MainPtagUint8 =< LoRangeMax
            ),
        list.filter(InLoGroup, SinglePtagGroups, LoGroups, EqHiGroups),
        get_next_label(EqHiLabel, !CI),
        string.format("fallthrough for ptags %u to %u",
            [u8(MinPtag), u8(LoRangeMax)], IfLoComment),
        string.format("code for ptags %u to %u",
            [u8(EqHiRangeMin), u8(MaxPtag)], EqHiLabelComment),
        % XXX ARG_PACK We should do the comparison on uint8s, not ints.
        LoRangeMaxConst = const(llconst_int(uint8.cast_to_int(LoRangeMax))),
        TestRval = binop(int_cmp(int_type_int, gt), PtagRval, LoRangeMaxConst),
        IfLoCode = singleton(
            llds_instr(if_val(TestRval, code_label(EqHiLabel)), IfLoComment)
        ),
        EqHiLabelCode = singleton(
            llds_instr(label(EqHiLabel), EqHiLabelComment)
        ),

        generate_primary_binary_search(VarRval, PtagRval, SectagReg,
            MaybeFailLabel, LoGroups, MinPtag, LoRangeMax,
            LoRangeCode, !CaseLabelMap, !CI),
        generate_primary_binary_search(VarRval, PtagRval, SectagReg,
            MaybeFailLabel, EqHiGroups, EqHiRangeMin, MaxPtag,
            EqHiRangeCode, !CaseLabelMap, !CI),
        Code = IfLoCode ++ LoRangeCode ++ EqHiLabelCode ++ EqHiRangeCode
    ).

%---------------------------------------------------------------------------%
%
% Infrastructure needed for all switch methods on ptags.
%

    % Generate the code corresponding to a primary tag.
    %
:- pred generate_ptag_group_code(rval::in, lval::in, maybe(label)::in,
    ptag_case_group(label)::in, llds_code::out,
    case_label_map::in, case_label_map::out,
    code_info::in, code_info::out) is det.

generate_ptag_group_code(VarRval, SectagReg, MaybeFailLabel,
        PtagGroup, Code, !CaseLabelMap, !CI) :-
    (
        PtagGroup = one_or_more_whole_ptags(WholeInfo),
        WholeInfo = whole_ptags_info(_MainPtag, _OtherPtags, _NF, CaseLabel),
        % There is no secondary tag, so there is no switch on it.
        generate_case_code_or_jump(CaseLabel, Code, !CaseLabelMap)
    ;
        PtagGroup = one_shared_ptag(SharedInfo),
        generate_secondary_switch(VarRval, SectagReg, MaybeFailLabel,
            SharedInfo, Code, !CaseLabelMap, !CI)
    ).

    % Generate the switch on the secondary tag.
    %
:- pred generate_secondary_switch(rval::in, lval::in, maybe(label)::in,
    shared_ptag_info(label)::in, llds_code::out,
    case_label_map::in, case_label_map::out,
    code_info::in, code_info::out) is det.

generate_secondary_switch(VarRval, SectagReg, MaybeFailLabel,
        SharedInfo, Code, !CaseLabelMap, !CI) :-
    SharedInfo = shared_ptag_info(_Ptag, _SharedSectagLocn, MaxSectag,
        SectagSwitchComplete, _NF, SectagToLabelMap, LabelToSectagsMap),
    get_globals(!.CI, Globals),
    MaxSectagInt = uint.cast_to_int(MaxSectag),
    Method = choose_switch_method(Globals, MaxSectagInt + 1),
    compute_sectag_rval(Globals, VarRval, SectagReg, SharedInfo,
        Method, SectagRval, SectagRvalCode),
    (
        SectagSwitchComplete = complete_switch,
        MaybeSecFailLabel = no
    ;
        SectagSwitchComplete = incomplete_switch,
        (
            MaybeFailLabel = yes(FailLabel),
            MaybeSecFailLabel = yes(FailLabel)
        ;
            MaybeFailLabel = no,
            % This can happen when
            %
            % - the switch on the secondary tag is missing some sectag values
            %   (which is why SectagSwitchCanFail = complete_switch), but
            %
            % - the inst of the switched-on variable at entry to the switch
            %   says that the switched-on variable cannot be bound to the
            %   function symbols corresponding to the missing sectags
            %   (which is why it is possible for MaybeFailLabel to be "no").
            MaybeSecFailLabel = no
        )
    ),
    (
        ( Method = try_me_else_chain
        ; Method = try_chain
        ),
        globals.get_word_size(Globals, WordSize),
        map.to_sorted_assoc_list(LabelToSectagsMap, LabelToSectagsAL),
        list.map(compute_sectag_case_test_rval(WordSize, SectagRval),
            LabelToSectagsAL, Cases0),
        (
            MaybeSecFailLabel = yes(_),
            list.det_head_tail(Cases0, HeadCase, TailCases)
        ;
            MaybeSecFailLabel = no,
            (
                Cases0 = [],
                unexpected($pred, "Cases0 = []")
            ;
                Cases0 = [Case1],
                HeadCase = Case1,
                TailCases = []
            ;
                Cases0 = [Case1, Case2 | Case3plus],
                put_an_expensive_test_last(Case1, Case2, Case3plus,
                    cord.init, CaseCord),
                Cases = cord.list(CaseCord),
                list.det_head_tail(Cases, HeadCase, TailCases)
            )
        ),
        (
            Method = try_me_else_chain,
            generate_secondary_try_me_else_chain(MaybeSecFailLabel,
                HeadCase, TailCases, CasesCode, !CaseLabelMap, !CI)
        ;
            Method = try_chain,
            generate_secondary_try_chain(MaybeSecFailLabel,
                HeadCase, TailCases, empty, CasesCode, !CaseLabelMap)
        )
    ;
        Method = jump_table,
        map.to_sorted_assoc_list(SectagToLabelMap, SectagToLabelAL),
        generate_secondary_jump_table(MaybeSecFailLabel, SectagToLabelAL,
            0u, MaxSectag, TargetMaybeLabels),
        CasesCode = singleton(
            llds_instr(
                computed_goto(SectagRval, yes(MaxSectagInt),
                    TargetMaybeLabels),
                "switch on secondary tag")
        )
    ;
        Method = binary_search,
        map.to_sorted_assoc_list(SectagToLabelMap, SectagToLabelAL),
        generate_secondary_binary_search(SectagRval, MaybeSecFailLabel,
            SectagToLabelAL, 0u, MaxSectag, CasesCode, !CaseLabelMap, !CI)
    ),
    Code = SectagRvalCode ++ CasesCode.

:- pred compute_sectag_rval(globals::in, rval::in, lval::in,
    shared_ptag_info(label)::in, switch_method::in,
    rval::out, llds_code::out) is det.

compute_sectag_rval(Globals, VarRval, SectagReg, SharedInfo, Method,
        SectagRval, SectagRvalCode) :-
    SharedInfo = shared_ptag_info(Ptag, SharedSectagLocn, MaxSectag,
        _, _, _, _),
    (
        SharedSectagLocn = sectag_remote_word,
        ZeroOffset = const(llconst_int(0)),
        OrigSectagRval = lval(field(yes(Ptag), VarRval, ZeroOffset)),
        Comment = "compute remote word sec tag to switch on"
    ;
        SharedSectagLocn = sectag_remote_bits(_NumBits, Mask),
        ZeroOffset = const(llconst_int(0)),
        SectagWordRval = lval(field(yes(Ptag), VarRval, ZeroOffset)),
        OrigSectagRval = binop(bitwise_and(int_type_uint),
            SectagWordRval, const(llconst_uint(Mask))),
        Comment = "compute remote sec tag bits to switch on"
    ;
        SharedSectagLocn = sectag_local_rest_of_word,
        OrigSectagRval = unop(unmkbody, VarRval),
        Comment = "compute local rest-of-word sec tag to switch on"
    ;
        SharedSectagLocn = sectag_local_bits(_NumBits, Mask),
        OrigSectagRval = binop(bitwise_and(int_type_uint),
            unop(unmkbody, VarRval), const(llconst_uint(Mask))),
        Comment = "compute local sec tag bits to switch on"
    ),
    AccessCount = switch_method_tag_access_count(Method),
    ( if
        AccessCount = more_than_one_access,
        MaxSectag >= 2u,
        globals.lookup_int_option(Globals, num_real_r_regs, NumRealRegs),
        (
            NumRealRegs = 0
        ;
            ( if SectagReg = reg(reg_r, SectagRegNum) then
                SectagRegNum =< NumRealRegs
            else
                unexpected($pred, "improper reg in tag switch")
            )
        )
    then
        SectagRval = lval(SectagReg),
        SectagRvalCode = singleton(
            llds_instr(assign(SectagReg, OrigSectagRval), Comment)
        )
    else
        SectagRval = OrigSectagRval,
        SectagRvalCode = empty
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Try-me-else chain switches on sectags.
%

    % Generate a switch on a secondary tag value using a try-me-else chain.
    %
:- pred generate_secondary_try_me_else_chain(maybe(label)::in,
    sectag_case_with_test::in, list(sectag_case_with_test)::in,
    llds_code::out, case_label_map::in, case_label_map::out,
    code_info::in, code_info::out) is det.

generate_secondary_try_me_else_chain(MaybeFailLabel, HeadCase, TailCases,
        Code, !CaseLabelMap, !CI) :-
    (
        TailCases = [HeadTailCase | TailTailCases],
        generate_secondary_try_me_else_chain_case(HeadCase, HeadCode,
            !CaseLabelMap, !CI),
        generate_secondary_try_me_else_chain(MaybeFailLabel,
            HeadTailCase, TailTailCases, TailCode, !CaseLabelMap, !CI),
        Code = HeadCode ++ TailCode
    ;
        TailCases = [],
        (
            MaybeFailLabel = yes(FailLabel),
            generate_secondary_try_me_else_chain_case(HeadCase, HeadCode,
                !CaseLabelMap, !CI),
            FailCode = singleton(
                llds_instr(goto(code_label(FailLabel)),
                    "secondary tag does not match any case")
            ),
            Code = HeadCode ++ FailCode
        ;
            MaybeFailLabel = no,
            HeadCase = sectag_case_with_test(HeadCaseLabel, _, _, _, _),
            generate_case_code_or_jump(HeadCaseLabel, Code, !CaseLabelMap)
        )
    ).

:- pred generate_secondary_try_me_else_chain_case(sectag_case_with_test::in,
    llds_code::out, case_label_map::in, case_label_map::out,
    code_info::in, code_info::out) is det.

generate_secondary_try_me_else_chain_case(Case, Code, !CaseLabelMap, !CI) :-
    Case = sectag_case_with_test(CaseLabel, OoMSectags, _,
        IsApplicableTestRval, _),
    generate_case_code_or_jump(CaseLabel, CaseCode, !CaseLabelMap),
    % ZZZ XXX Optimize what we generate when CaseCode = goto(CaseLabel).
    get_next_label(ElseLabel, !CI),
    negate_rval(IsApplicableTestRval, IsNotApplicableTestRval),
    SectagStrs =
        list.map(string.uint_to_string, one_or_more_to_list(OoMSectags)),
    SectagsStr = string.join_list(", ", SectagStrs),
    string.format("case for sectags %s", [s(SectagsStr)], CaseCommentStr),
    TestCode = cord.from_list([
        llds_instr(
            if_val(IsNotApplicableTestRval, code_label(ElseLabel)),
            "test sec tag only"),
        llds_instr(comment(CaseCommentStr), "")
    ]),
    ElseLabelCode = singleton(
        llds_instr(label(ElseLabel), "handle next secondary tag")
    ),
    Code = TestCode ++ CaseCode ++ ElseLabelCode.

%---------------------------------------------------------------------------%
%
% Try chain switches on sectags.
%

    % Generate a switch on a secondary tag value using a try chain.
    %
:- pred generate_secondary_try_chain(maybe(label)::in,
    sectag_case_with_test::in, list(sectag_case_with_test)::in,
    llds_code::in, llds_code::out,
    case_label_map::in, case_label_map::out) is det.

generate_secondary_try_chain(MaybeFailLabel, HeadCase, TailCases,
        !.TryChainCode, Code, !CaseLabelMap) :-
    (
        TailCases = [HeadTailCase | TailTailCases],
        generate_secondary_try_chain_case(!.CaseLabelMap, HeadCase,
            !TryChainCode),
        generate_secondary_try_chain(MaybeFailLabel,
            HeadTailCase, TailTailCases, !.TryChainCode, Code, !CaseLabelMap)
    ;
        TailCases = [],
        (
            MaybeFailLabel = yes(FailLabel),
            generate_secondary_try_chain_case(!.CaseLabelMap, HeadCase,
                !TryChainCode),
            FailCode = singleton(
                llds_instr(goto(code_label(FailLabel)),
                    "secondary tag with no code to handle it")
            ),
            Code = !.TryChainCode ++ FailCode
        ;
            MaybeFailLabel = no,
            HeadCase = sectag_case_with_test(HeadCaseLabel, _, _, _, _),
            generate_case_code_or_jump(HeadCaseLabel, HeadCode, !CaseLabelMap),
            Code = !.TryChainCode ++ HeadCode
        )
    ).

:- pred generate_secondary_try_chain_case(case_label_map::in,
    sectag_case_with_test::in, llds_code::in, llds_code::out) is det.

generate_secondary_try_chain_case(CaseLabelMap, Case, !TryChainCode) :-
    Case = sectag_case_with_test(CaseLabel, _, _, IsApplicableTestRval, _),
    map.lookup(CaseLabelMap, CaseLabel, CaseInfo0),
    CaseInfo0 = case_label_info(Comment, _CaseCode, _CaseGenerated),
    TestCode = singleton(
        llds_instr(
            if_val(IsApplicableTestRval, code_label(CaseLabel)),
            "test sec tag only for " ++ Comment)
    ),
    !:TryChainCode = !.TryChainCode ++ TestCode.

%---------------------------------------------------------------------------%
%
% Jump table switches on sectags.
%

    % Generate the cases for a switch on the secondary tag value using
    % a dense jump table that has an entry for all the possible values.
    %
:- pred generate_secondary_jump_table(maybe(label)::in,
    sectag_goal_list(label)::in, uint::in, uint::in,
    list(maybe(label))::out) is det.

generate_secondary_jump_table(MaybeFailLabel, Cases, CurSectag, MaxSectag,
        TargetMaybeLabels) :-
    ( if CurSectag > MaxSectag then
        expect(unify(Cases, []), $pred,
            "Cases not empty when reaching limiting secondary tag"),
        TargetMaybeLabels = []
    else
        NextSectag = CurSectag + 1u,
        ( if Cases = [CurSectag - CaseLabel | TailCases] then
            generate_secondary_jump_table(MaybeFailLabel, TailCases,
                NextSectag, MaxSectag, TailTargetMaybeLabels),
            TargetMaybeLabels = [yes(CaseLabel) | TailTargetMaybeLabels]
        else
            generate_secondary_jump_table(MaybeFailLabel, Cases,
                NextSectag, MaxSectag, TailTargetMaybeLabels),
            TargetMaybeLabels = [MaybeFailLabel | TailTargetMaybeLabels]
        )
    ).

%---------------------------------------------------------------------------%
%
% Binary search switches on sectags.
%

    % Generate the cases for a secondary tag using a binary search.
    % This invocation looks after secondary tag values in the range
    % MinPtag to MaxPtag (including both boundary values).
    %
:- pred generate_secondary_binary_search(rval::in, maybe(label)::in,
    sectag_goal_list(label)::in, uint::in, uint::in, llds_code::out,
    case_label_map::in, case_label_map::out,
    code_info::in, code_info::out) is det.

generate_secondary_binary_search(SectagRval, MaybeFailLabel,
        SectagGoals, MinSectag, MaxSectag, Code, !CaseLabelMap, !CI) :-
    ( if MinSectag = MaxSectag then
        CurSectag = MinSectag,
        (
            SectagGoals = [],
            % There is no code for this tag.
            (
                MaybeFailLabel = yes(FailLabel),
                CurSectagStr = string.uint_to_string(CurSectag),
                Comment = "no code for ptag " ++ CurSectagStr,
                Code = singleton(
                    llds_instr(goto(code_label(FailLabel)), Comment)
                )
            ;
                MaybeFailLabel = no,
                Code = empty
            )
        ;
            SectagGoals = [CurSectagPrime - CaseLabel],
            expect(unify(CurSectag, CurSectagPrime), $pred,
                "cur sectag mismatch"),
            generate_case_code_or_jump(CaseLabel, Code, !CaseLabelMap)
        ;
            SectagGoals = [_, _ | _],
            unexpected($pred,
                "SectagGoals not singleton or empty when binary search ends")
        )
    else
        LoRangeMax = (MinSectag + MaxSectag) // 2u,
        EqHiRangeMin = LoRangeMax + 1u,
        InLoGroup =
            ( pred(SectagGoal::in) is semidet :-
                SectagGoal = Sectag - _,
                Sectag =< LoRangeMax
            ),
        list.filter(InLoGroup, SectagGoals, LoGoals, EqHiGoals),
        get_next_label(NewLabel, !CI),
        string.format("fallthrough for sectags %u to %u",
            [u(MinSectag), u(LoRangeMax)], IfComment),
        string.format("code for sectags %u to %u",
            [u(EqHiRangeMin), u(MaxSectag)], LabelComment),
        LoRangeMaxConst = const(llconst_uint(LoRangeMax)),
        TestRval = binop(int_cmp(int_type_int, gt),
            SectagRval, LoRangeMaxConst),
        IfCode = singleton(
            llds_instr(if_val(TestRval, code_label(NewLabel)), IfComment)
        ),
        LabelCode = singleton(
            llds_instr(label(NewLabel), LabelComment)
        ),

        generate_secondary_binary_search(SectagRval, MaybeFailLabel,
            LoGoals, MinSectag, LoRangeMax, LoRangeCode,
            !CaseLabelMap, !CI),
        generate_secondary_binary_search(SectagRval, MaybeFailLabel,
            EqHiGoals, EqHiRangeMin, MaxSectag, EqHiRangeCode,
            !CaseLabelMap, !CI),

        Code = IfCode ++ LoRangeCode ++ LabelCode ++ EqHiRangeCode
    ).

%---------------------------------------------------------------------------%
%
% Infrastructure needed for both try-me-else and try chain switches
% that test whether a tag (ptag or sectag) is in a given set.
%

    % This predicate is designed to try to reduce the cost of the last test
    % in a chain of tests. The scenario it is designed for is when we are
    % generating code for a switch on sectags that cannot fail. (This means
    % we have a case for every possible value of the secondary tag, even if
    % the tag switch as a whole can fail, which would have to be because
    % we don't have a case for a primary tag value)
    %
    % Obviously we have at least two cases, because if we had only one,
    % we wouldn't need a switch at all. In general, we have two or more.
    % If two cases, say A and B, have NumSectagsA and NumSectagsB sectag values
    % corresponding to them respectively, we prioritize getting to the code
    % of case A more cheaply than case B, simply because that minimizes
    % the expected *average* cost. This is why we order sectag groups
    % in descending order of number of sectags, leading to code structures
    % such as the try chain
    %
    %   if sectag is in Case A's set, goto code of case A
    %   if sectag is in Case B's set, goto code of case B
    %   if sectag is in Case C's set, goto code of case B
    %   ...
    %
    % where NumSectagsA >= NumSectagsB >= NumSectagsC >= ...
    %
    % (Try-me-else chains follow the same logic, but switch the role of
    % the branch away and the fallthrough.)
    %
    % If the switch on the sectag cannot fail, then the test on the last
    % case can be optimized away, since the failure of the previous tests
    % guarantees its success. However, our ordering of the cases guarantees
    % that the last case will correspond to at most as many sectags as
    % the next-to-last case, and (since the cost of the set membership test
    % *may* be higher for a larger set than for a smaller one), this means
    % that optimizing away the test for the last case may leave some
    % performance on the table. If indeed, the cost of the last case
    % (call it case F) is cheaper than the cost of the next-to-last case
    % (call it case E), then we don't want to generate code such as
    %
    %   ... code for previous cases ...
    %   if sectag is in case E's set, goto code of case E
    %   goto code of case F
    %
    % Instead, we want to generate code such as
    %
    %   ... code for previous cases ...
    %   if sectag is in case F's set, goto code of case F
    %   goto code of case E
    %
    % They both involve a test and a conditional branch. We do this
    % transformation only if the test for F is cheaper than the test for E,
    % and the cost of the conditional branch will depend on the performance
    % of the CPU's branch prediction mechanisms either way. If for some reason
    % it turned out that the structure that branches off to E is better
    % for performance, we can still get that effect by using code such as
    %
    %   ... code for previous cases ...
    %   if sectag is NOT in case F's set, goto code of case E
    %   goto code of case F
    %
    % Note that the extra negation exists only in this pseudo-code.
    % The actual code generated by neg_rval would replace each comparison
    % operation by its opposite (e.g. replacing "eq" with "ne"), and update
    % any connectives between the comparisons accordingly, keeping its cost
    % the same.
    %
    % For ptags, the scope for this optimization is smaller, since
    % the smaller set of possible values also constrains the number of cases.
    % The reordering to put the most frequent case last for try_chain switches
    % on ptags basically does what this predicate does, though
    %
    % - it does so *without* taking into account the number of ptags
    %   that share a case (which is understandable, since it was written
    %   before multi-cons-id switches were implemented), and
    %
    % - there is no similar logic for try-me-else chains on ptags.
    %
:- pred put_an_expensive_test_last(sectag_case_with_test::in,
    sectag_case_with_test::in, list(sectag_case_with_test)::in,
    cord(sectag_case_with_test)::in, cord(sectag_case_with_test)::out) is det.

put_an_expensive_test_last(Case1, Case2, Case3plus, !CaseCord) :-
    Case1 = sectag_case_with_test(_, _, NumSectags1, _, Cost1),
    Case2 = sectag_case_with_test(_, _, NumSectags2, _, Cost2),
    (
        Case3plus = [],
        % Case1 and Case2 are the two last cases. We want to put the one
        % with the more expensive test last, even if it has fewer sectags
        % (i.e. function symbols) than the other case.
        ( if Cost1 > Cost2 then
            cord.snoc(Case2, !CaseCord),
            cord.snoc(Case1, !CaseCord)
        else
            cord.snoc(Case1, !CaseCord),
            cord.snoc(Case2, !CaseCord)
        )
    ;
        Case3plus = [Case3 | Case4plus],
        % Case1 and Case2 are NOT the two last cases. In such cases,
        % it is more important to have the case with more sectags
        % (i.e. function symbols) first. Note that the list we are given
        % as input is sorted on the number of sectags in a descending order,
        % so Case2 cannot have more sectags than Case1.
        ( if NumSectags1 > NumSectags2 then
            cord.snoc(Case1, !CaseCord),
            put_an_expensive_test_last(Case2, Case3, Case4plus, !CaseCord)
        else
            % NumSectags1 must equal NumSectags2.
            ( if Cost1 > Cost2 then
                cord.snoc(Case2, !CaseCord),
                put_an_expensive_test_last(Case1, Case3, Case4plus, !CaseCord)
            else
                cord.snoc(Case1, !CaseCord),
                put_an_expensive_test_last(Case2, Case3, Case4plus, !CaseCord)
            )
        )
    ).

:- type sectag_case_with_test
    --->    sectag_case_with_test(
                % The label for the code we want to execute in
                % this arm of the switch.
                label,

                % The secondary tags for which we want to execute
                % this arm of the switch.
                one_or_more(uint),

                % The number of sectags in the previous field.
                int,

                % The test for the actual sectag having one of the values
                % in the second field.
                rval,

                % The cost of the test rval.
                int
            ).

:- pred compute_sectag_case_test_rval(word_size::in, rval::in,
    pair(label, one_or_more(uint))::in, sectag_case_with_test::out) is det.
 
compute_sectag_case_test_rval(WordSize, SectagRval, Case, CaseWithTestRval) :-
    Case = CaseLabel - OoMSectags,
    test_sectag_is_in_set(WordSize, SectagRval, OoMSectags,
        TestRval, TestRvalCost),
    NumSectags = one_or_more.length(OoMSectags),
    CaseWithTestRval = sectag_case_with_test(CaseLabel, OoMSectags,
        NumSectags, TestRval, TestRvalCost).

%---------------------%
%
% For ptags we know that all the sets in which we want to test for membership
% will be subsets of {0 .. 7}, which means that
%
% - the bitmap form of the set will always fit into one word, and
% - using the value of the ptag to index into this word will always be ok.
%
% Neither is true for secondary tags, because
%
% - testing a secondary tag for membership in the set {2, 5, 77, 80}
%   will require looking in two words even on 64 bit machines, and
%
% - testing a secondary tag for membership in the set {12, 15, 33, 37}
%   will require looking in either one or two words on 32 bit machines,
%   but arranging things so that we look in only one word requires
%   making the least significant bit of that word correspond to
%   a sectag value that is *not* zero, requiring a subtraction
%   of an initial offset from the actual sectag value.
%
% If we want to take advantage of the invariants applying to ptags (and we do),
% these differences require separate code for testing ptags vs testing sectags.
% We nevertheless group all that code together here, because this should make
% understanding and modifying this code simpler.
%
% A note about the mixed use of both signed and unsigned integers here.
%
% - The macros we use to get the ptag or sectag bits of a word were designed
%   before Mercury supported unsigned integers, and therefore they return
%   signed integers.
%
% - On the other hand, we want to be able to use bitmaps that all the bits
%   of a word, including the most-significant bit. If we used signed integer
%   operations on a bitmap that had the most-significant bit set, we would
%   be invoking undefined behavior, which would be bad, since I (zs) don't
%   want the C compiler to either optimize away the test rval entirely,
%   or to make demons fly out of my nose :-)
%
% The result is that we use unsigned integers to hold the bits being shifted,
% masked and tested, but signed integers (that are always non-negative)
% as the shift amounts.
%

:- pred test_ptag_is_in_set(rval::in, ptag::in, list(ptag)::in,
    rval::out) is det.

test_ptag_is_in_set(PtagRval, MainPtag, OtherPtags, TestRval) :-
    (
        OtherPtags = [],
        MainPtag = ptag(MainPtagUint8),
        TestRval = binop(int_cmp(int_type_int, eq), PtagRval,
            const(llconst_int(uint8.cast_to_int(MainPtagUint8))))
    ;
        OtherPtags = [_ | _],
        encode_ptags_as_bitmap_loop(MainPtag, OtherPtags, 0u, Bitmap),
        LeftShiftOp = unchecked_left_shift(int_type_uint, shift_by_int),
        SelectedBitMaskRval = binop(LeftShiftOp,
            const(llconst_uint(1u)), PtagRval),
        SelectedBitRval = binop(bitwise_and(int_type_uint),
            SelectedBitMaskRval, const(llconst_uint(Bitmap))),
        TestRval = binop(int_cmp(int_type_uint, ne),
            SelectedBitRval, const(llconst_uint(0u)))
    ).

:- pred encode_ptags_as_bitmap_loop(ptag::in, list(ptag)::in,
    uint::in, uint::out) is det.

encode_ptags_as_bitmap_loop(HeadPtag, TailPtags, !Bitmap) :-
    HeadPtag = ptag(HeadPtagUint8),
    !:Bitmap = !.Bitmap \/
        (1u `unchecked_left_ushift` uint8.cast_to_uint(HeadPtagUint8)),
    (
        TailPtags = []
    ;
        TailPtags = [HeadTailPtag | TailTailPtags],
        encode_ptags_as_bitmap_loop(HeadTailPtag, TailTailPtags, !Bitmap)
    ).

%---------------------%

    % test_sectag_is_in_set(WordSizeKind, SectagRval, OoMSectags,
    %   TestRval, TestRvalCost):
    %
    % Return in TestRval the code we want to use to test whether the value
    % currently in SectagRval is in the set represented by OoMSectags.
    % Return in TestRvalCost a measure of the cost of executing this test.
    %
:- pred test_sectag_is_in_set(word_size::in, rval::in, one_or_more(uint)::in,
    rval::out, int::out) is det.

test_sectag_is_in_set(WordSizeKind, SectagRval, OoMSectags,
        TestRval, TestRvalCost) :-
    OoMSectags = one_or_more(HeadSectag, TailSectags),
    (
        TailSectags = [],
        TestRval = make_sectag_eq_test(SectagRval, HeadSectag),
        TestRvalCost = cost_of_eq_test
    ;
        TailSectags = [HeadTailSectag | TailTailSectags],
        (
            TailTailSectags = [],
            HeadTestRval = make_sectag_eq_test(SectagRval, HeadSectag),
            HeadTailTestRval = make_sectag_eq_test(SectagRval, HeadTailSectag),
            TestRval = binop(logical_or, HeadTestRval, HeadTailTestRval),
            TestRvalCost = 2 * cost_of_eq_test + cost_of_logical_or
        ;
            TailTailSectags = [_ | _],
            BitmapWord0 = make_bitmap_word_starting_at(HeadSectag),
            % We set the word size to the minimum of
            % - the wordsize of the machine we are running on, and
            % - the wordsize of the machine we are compiling to, if different.
            ( WordSizeKind = word_size_32, WordSize = 32u
            ; WordSizeKind = word_size_64, WordSize = 64u
            ),
            encode_sectags_as_bitmaps_loop(WordSize,
                HeadTailSectag, TailTailSectags, BitmapWord0, OoMBitmaps),
            OoMBitmaps = one_or_more(HeadBitmap, TailBitmaps),
            test_sectag_is_in_bitmaps(WordSize, SectagRval,
                HeadBitmap, TailBitmaps, TestRval, TestRvalCost)
        )
    ).

:- type bitmap_word
    --->    bitmap_word(
                bw_start_offset     :: uint,
                bw_bitmap_word      :: uint,
                bw_values           :: cord(uint)
            ).

:- pred encode_sectags_as_bitmaps_loop(uint::in, uint::in, list(uint)::in,
    bitmap_word::in, one_or_more(bitmap_word)::out) is det.

encode_sectags_as_bitmaps_loop(WordSize, HeadN, TailNs,
        CurBitmapWord0, BitmapWords) :-
    CurBitmapWord0 = bitmap_word(StartOffset, Bitmap0, ValuesCord0),
    LocalOffset = HeadN - StartOffset,
    ( if LocalOffset =< WordSize then
        Bitmap1 = Bitmap0 \/ (1u `unchecked_left_ushift` LocalOffset),
        cord.snoc(HeadN, ValuesCord0, ValuesCord1),
        CurBitmapWord1 = bitmap_word(StartOffset, Bitmap1, ValuesCord1),
        (
            TailNs = [],
            BitmapWords = one_or_more(CurBitmapWord1, [])
        ;
            TailNs = [HeadTailN | TailTailNs],
            encode_sectags_as_bitmaps_loop(WordSize, HeadTailN, TailTailNs,
                CurBitmapWord1, BitmapWords)
        )
    else
        NextBitmapWord1 = make_bitmap_word_starting_at(HeadN),
        (
            TailNs = [],
            BitmapWords = one_or_more(CurBitmapWord0, [NextBitmapWord1])
        ;
            TailNs = [HeadTailN | TailTailNs],
            encode_sectags_as_bitmaps_loop(WordSize, HeadTailN, TailTailNs,
                NextBitmapWord1, TailBitmapWords),
            BitmapWords = one_or_more.cons(CurBitmapWord0, TailBitmapWords)
        )
    ).

:- func make_bitmap_word_starting_at(uint) = bitmap_word.

make_bitmap_word_starting_at(N) =
    bitmap_word(N, 1u, cord.singleton(N)).

:- pred test_sectag_is_in_bitmaps(uint::in, rval::in,
    bitmap_word::in, list(bitmap_word)::in, rval::out, int::out) is det.

test_sectag_is_in_bitmaps(WordSize, SectagRval, HeadBitmap, TailBitmaps,
        TestRval, TestRvalCost) :-
    HeadBitmap = bitmap_word(StartOffset, Bitmap0, ValuesCord),
    Values = cord.list(ValuesCord),
    % When we have just one sectag value, we just test for equality directly.
    %
    % For three or more sectag values, it is almost certainly cheaper
    % to do a subtraction, a shift, and bitwise and a test against zero
    % than it is to do three equality tests and two logical ORs.
    %
    % For two sectag values, the answer to the question "which approach
    % is faster" is reasonably likely to be both platform- and data-dependent.
    (
        Values = [],
        unexpected($pred, "Values = []")
    ;
        Values = [ValueA],
        HeadTestRval = make_sectag_eq_test(SectagRval, ValueA),
        HeadTestRvalCost = cost_of_eq_test
    ;
        Values = [ValueA, ValueB],
        TestA = make_sectag_eq_test(SectagRval, ValueA),
        TestB = make_sectag_eq_test(SectagRval, ValueB),
        HeadTestRval = binop(logical_or, TestA, TestB),
        HeadTestRvalCost = 2 + cost_of_eq_test + cost_of_logical_or
    ;
        Values = [_, _, _ | _],
        ( if StartOffset = 0u then
            Bitmap = Bitmap0,
            OffsetInWordRval = SectagRval,
            SubtractCost = 0
        else if list.det_last(Values, LastValue), LastValue < WordSize then
            % Avoid subtracting StartOffset from SectagRval, and compensate
            % by shifting all the bits in Bitmap to the left by StartOffset.
            Bitmap = Bitmap0 `unchecked_left_ushift` StartOffset,
            OffsetInWordRval = SectagRval,
            SubtractCost = 0
        else
            Bitmap = Bitmap0,
            SubOp = int_arith(int_type_uint, ao_sub),
            StartOffsetInt = uint.cast_to_int(StartOffset),
            OffsetInWordRval = binop(SubOp,
                SectagRval, const(llconst_int(StartOffsetInt))),
            SubtractCost = cost_of_subtract
        ),
        LeftShiftOp = unchecked_left_shift(int_type_uint, shift_by_int),
        SelectedBitMaskRval = binop(LeftShiftOp,
            const(llconst_uint(1u)), OffsetInWordRval),
        SelectedBitRval = binop(bitwise_and(int_type_uint),
            SelectedBitMaskRval, const(llconst_uint(Bitmap))),
        HeadTestRval = binop(int_cmp(int_type_uint, ne),
            SelectedBitRval, const(llconst_uint(0u))),
        HeadTestRvalCost = SubtractCost + cost_of_bitmap_test
    ),
    (
        TailBitmaps = [],
        TestRval = HeadTestRval,
        TestRvalCost = HeadTestRvalCost
    ;
        TailBitmaps = [HeadTailBitmap | TailTailBitmaps],
        test_sectag_is_in_bitmaps(WordSize, SectagRval,
            HeadTailBitmap, TailTailBitmaps, TailTestRval, TailTestRvalCost),
        TestRval = binop(logical_or, HeadTestRval, TailTestRval),
        TestRvalCost = HeadTestRvalCost + TailTestRvalCost
    ).

:- func cost_of_eq_test = int.
:- func cost_of_subtract = int.
:- func cost_of_bitmap_test = int.
:- func cost_of_logical_or = int.
:- pragma inline(func(cost_of_eq_test/0)).
:- pragma inline(func(cost_of_subtract/0)).
:- pragma inline(func(cost_of_bitmap_test/0)).
:- pragma inline(func(cost_of_logical_or/0)).

cost_of_eq_test = 1.
cost_of_subtract = 1.
cost_of_bitmap_test = 3.    % A shift, a bitwise AND, and a test for zero.
cost_of_logical_or = 1.

:- func make_sectag_eq_test(rval, uint) = rval.

make_sectag_eq_test(SectagRval, Sectag) = TestRval :-
    SectagInt = uint.cast_to_int(Sectag),
    TestRval = binop(int_cmp(int_type_int, eq),
        SectagRval, const(llconst_int(SectagInt))).

%---------------------------------------------------------------------------%
%
% General utility operations.
%

:- func choose_switch_method(globals, int) = switch_method.

choose_switch_method(Globals, NumAlternatives) = Method :-
    globals.get_opt_tuple(Globals, OptTuple),
    DenseSwitchSize = OptTuple ^ ot_dense_switch_size,
    TrySwitchSize = OptTuple ^ ot_try_switch_size,
    BinarySwitchSize = OptTuple ^ ot_binary_switch_size,
    % ZZZ revisit the defaults of these parameters
    ( if NumAlternatives >= DenseSwitchSize then
        Method = jump_table
    else if NumAlternatives >= BinarySwitchSize then
        Method = binary_search
    else if NumAlternatives >= TrySwitchSize then
        Method = try_chain
    else
        Method = try_me_else_chain
    ).

:- type tag_access_count
    --->    just_one_access
    ;       more_than_one_access.

    % Will the given method of implementing switches on tags
    % access the tag just once, or several times?
    %
:- func switch_method_tag_access_count(switch_method) = tag_access_count.

switch_method_tag_access_count(Method) = Count :-
    (
        Method = jump_table,
        Count = just_one_access
    ;
        ( Method = try_chain
        ; Method = try_me_else_chain
        ; Method = binary_search
        ),
        Count = more_than_one_access
    ).

:- pred make_ptag_comment(string::in, ptag_case_group(label)::in,
    string::out) is det.

make_ptag_comment(BaseStr, PtagGroup, Comment) :-
    (
        PtagGroup = one_or_more_whole_ptags(WholeInfo),
        % Note: OtherPtags may be [] here too.
        WholeInfo = whole_ptags_info(MainPtag, OtherPtags, _, _)
    ;
        PtagGroup = one_shared_ptag(SharedInfo),
        SharedInfo = shared_ptag_info(MainPtag, _, _, _, _, _, _),
        OtherPtags = []
    ),
    (
        OtherPtags = [],
        Comment = BaseStr ++ ptag_to_string(MainPtag)
    ;
        OtherPtags = [_ | _],
        Comment = BaseStr ++ ptag_to_string(MainPtag)
            ++ " (shared with " ++
            string.join_list(", ", list.map(ptag_to_string, OtherPtags))
            ++ ")"
    ).

:- func ptag_to_string(ptag) = string.

ptag_to_string(ptag(Ptag)) = string.uint8_to_string(Ptag).

%---------------------------------------------------------------------------%
:- end_module ll_backend.tag_switch.
%---------------------------------------------------------------------------%
