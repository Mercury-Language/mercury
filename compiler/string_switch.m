%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2007, 2009-2011 The University of Melbourne.
% Copyright (C) 2015, 2017-2018, 2020, 2022, 2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: string_switch.m.
% Authors: fjh, zs.
%
% For switches on strings, we can generate
%
% - a trie;
% - a hash table using open addressing to resolve hash conflicts; or
% - a sorted table for binary search.
%
% The hash table has a higher startup cost than binary search, but should use
% fewer comparisons, so it is preferred for bigger tables. The trie approach
% does not need any startup code and examines each character just once,
% so it does the least work, but it does do a hard-to-predict jump
% for each character in the trie (which usually *won't* be all the characters
% in the string being switched on).
%
% When the switch arms are general code, what we put into the hash table
% or binary search table for each case is the offset of the relevant arm
% in a computed_goto. The generated code would be faster (due to better
% locality) if we included the actual target address instead. Unfortunately,
% that would require two extensions to the LLDS. The first and relatively
% easy change would be a new LLDS instruction that represents a goto
% to an arbitrary rval (in this case, the rval taken from the selected
% table row). The second and substantially harder change would be making
% the internal labels of the switch arms actually storable in static data.
% We do not currently have any way to refer to internal labels from data,
% and optimizations that manipulate labels (such as frameopt, which can
% duplicate them, and dupelim, which can replace them with other labels)
% would have to be taught to reflect any changes they make in the global
% data. It is the last step that is the killer in terms of difficulty
% of implementation.
%
% One possible way around the problem would be to do the code generation
% and optimization as we do now, just recording a bit more information
% during code generation about which numbers in static data refer to
% which computed_gotos, and then, after all the optimizations are done,
% to go back and replace all the indicated numbers with the corresponding
% final labels.
%
% WARNING: the code here is quite similar to the code in ml_string_switch.m.
% Any changes here may require similar changes there, and vice versa.
%
%---------------------------------------------------------------------------%

:- module ll_backend.string_switch.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.
:- import_module ll_backend.lookup_switch.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------%

:- pred generate_string_trie_jump_switch(rval::in, string::in,
    list(tagged_case)::in, code_model::in, can_fail::in, hlds_goal_info::in,
    label::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

:- pred generate_string_trie_lookup_switch(rval::in,
    lookup_switch_info(string)::in, can_fail::in, label::in, abs_store_map::in,
    branch_end::out, llds_code::out, code_info::out) is det.

%---------------------%

:- pred generate_string_hash_jump_switch(rval::in, string::in,
    list(tagged_case)::in, code_model::in, can_fail::in, hlds_goal_info::in,
    label::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

:- pred generate_string_hash_lookup_switch(rval::in,
    lookup_switch_info(string)::in, can_fail::in, label::in, abs_store_map::in,
    branch_end::out, llds_code::out, code_info::out) is det.

%---------------------%

:- pred generate_string_binary_jump_switch(rval::in, string::in,
    list(tagged_case)::in, code_model::in, can_fail::in, hlds_goal_info::in,
    label::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

:- pred generate_string_binary_lookup_switch(rval::in,
    lookup_switch_info(string)::in, can_fail::in, label::in, abs_store_map::in,
    branch_end::out, llds_code::out, code_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.lookup_switch_util.
:- import_module backend_libs.string_encoding.
:- import_module backend_libs.string_switch_util.
:- import_module hlds.hlds_data.
:- import_module libs.
:- import_module libs.globals.
:- import_module ll_backend.code_util.
:- import_module ll_backend.lookup_util.
:- import_module ll_backend.switch_case.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_string_trie_jump_switch(VarRval, VarName, TaggedCases,
        CodeModel, CanFail, SwitchGoalInfo, EndLabel, MaybeEnd, Code,
        !CI, CLD) :-
    init_string_trie_switch_info_jump(CanFail, JumpInfo0, !CI, CLD),
    BranchStart = JumpInfo0 ^ stsij_branch_start,
    Params = represent_params(VarName, SwitchGoalInfo, CodeModel, BranchStart,
        EndLabel),
    CommentCode = singleton(
        llds_instr(comment("string trie jump switch"), "")
    ),

    % Generate code for the cases, and remember the label of each case.
    map.init(CaseIdToLabelMap0),
    map.init(CaseLabelMap0),
    represent_tagged_cases_in_string_trie_switch(Params, TaggedCases,
        CaseIdToLabelMap0, CaseIdToLabelMap, CaseLabelMap0, CaseLabelMap,
        no, MaybeEnd, !CI),
    JumpInfo = JumpInfo0 ^ stsij_case_id_to_label_map := CaseIdToLabelMap,

    Info = stsi_jump(JumpInfo),
    build_str_case_id_list(TaggedCases, _MaxCaseNum, StrsCaseIds),
    create_nested_trie_switch(Info, VarRval, StrsCaseIds, TrieCode, !CI),

    % Generate the code for the cases.
    add_not_yet_included_cases(CasesCode, CaseLabelMap, _),

    FailLabel = JumpInfo ^ stsij_fail_label,
    FailLabelCode = singleton(
        llds_instr(label(FailLabel), "fail label")
    ),
    FailCode = JumpInfo ^ stsij_fail_code,

    EndLabelCode = singleton(
        llds_instr(label(EndLabel), "end of string trie string")
    ),

    Code = CommentCode ++ TrieCode ++ CasesCode ++
        FailLabelCode ++ FailCode ++ EndLabelCode.

:- pred represent_tagged_cases_in_string_trie_switch(represent_params::in,
    list(tagged_case)::in, map(case_id, label)::in, map(case_id, label)::out,
    case_label_map::in, case_label_map::out,
    branch_end::in, branch_end::out, code_info::in, code_info::out) is det.

represent_tagged_cases_in_string_trie_switch(_, [],
        !CaseIdToLabelMap, !CaseLabelMap, !MaybeEnd, !CI).
represent_tagged_cases_in_string_trie_switch(Params,
        [TaggedCase | TaggedCases],
        !CaseIdToLabelMap, !CaseLabelMap, !MaybeEnd, !CI) :-
    represent_tagged_case_for_llds(Params, TaggedCase, Label,
        !CaseLabelMap, !MaybeEnd, !CI, unit, _),
    TaggedCase = tagged_case(_, _, CaseId, _),
    map.det_insert(CaseId, Label, !CaseIdToLabelMap),
    represent_tagged_cases_in_string_trie_switch(Params, TaggedCases,
        !CaseIdToLabelMap, !CaseLabelMap, !MaybeEnd, !CI).

%---------------------%

:- type code_unit_to_action
    --->    code_unit_to_action(int, code_unit_action).

:- type code_unit_action
    --->    action_nested_trie(label)
    ;       action_case(case_id).

:- pred create_nested_trie_switch(string_trie_switch_info::in, rval::in,
    assoc_list(string, case_id)::in, llds_code::out,
    code_info::in, code_info::out) is det.

create_nested_trie_switch(Info, VarRval, StrsCaseIds, TrieCode, !CI) :-
    get_encoding(Info, Encoding),
    create_trie(Encoding, StrsCaseIds, TopTrieNode),
    convert_trie_to_nested_switches(Info, VarRval, 0,
        TopTrieNode, TopTrieLabel, TrieCode0, !CI),
    ( if
        cord.head_tail(TrieCode0, HeadTrieCodeInstr, TailTrieCodeInstrs),
        HeadTrieCodeInstr = llds_instr(label(TopTrieLabel), _)
    then
        TrieCode = TailTrieCodeInstrs
    else
        unexpected($pred, "TrieCode0 does not start with TopTrieLabel")
    ).

:- pred convert_trie_to_nested_switches(string_trie_switch_info::in,
    rval::in, int::in, trie_node::in,
    label::out, llds_code::out, code_info::in, code_info::out) is det.

convert_trie_to_nested_switches(Info, VarRval, NumMatched, TrieNode,
        TrieNodeLabel, Code, !CI) :-
    get_next_label(TrieNodeLabel, !CI),
    (
        TrieNode = trie_leaf(RevMatchedCodeUnits, NotYetMatchedCodeUnits,
            CaseId),
        list.reverse(RevMatchedCodeUnits, MatchedCodeUnits),
        AllCodeUnits = MatchedCodeUnits ++ NotYetMatchedCodeUnits,
        list.length(MatchedCodeUnits, NumMatchedCodeUnits),
        expect(unify(NumMatchedCodeUnits, NumMatched), $pred,
            "NumevMatchedCodeUnits != NumMatched"),
        get_encoding(Info, Encoding),
        det_from_code_unit_list_in_encoding_allow_ill_formed(Encoding,
            AllCodeUnits, EndStr),
        NodeComment = "AllCodeUnits " ++ string.string(AllCodeUnits),
        CondRval = binop(offset_str_eq(NumMatched, no_size),
            VarRval, const(llconst_string(EndStr))),
        generate_trie_case_or_fall_through(Info, CondRval, CaseId,
            CaseRepCode, !CI),
        generate_trie_goto_fail_code(Info, GotoFailCode),
        TrieNodeLabelCode = singleton(
            llds_instr(label(TrieNodeLabel), NodeComment)
        ),
        Code = TrieNodeLabelCode ++ CaseRepCode ++ GotoFailCode
    ;
        TrieNode = trie_choice(RevMatchedCodeUnits, _ChoiceMap, MaybeEnd),
        list.length(RevMatchedCodeUnits, NumRevMatchedCodeUnits),
        expect(unify(NumRevMatchedCodeUnits, NumMatched), $pred,
            "NumRevMatchedCodeUnits != NumMatched"),

        get_code_unit_reg(Info, CodeUnitRegLval),
        GetCurCodeUnitRval = binop(string_unsafe_index_code_unit,
            VarRval, const(llconst_int(NumMatched))),
        LabelCode = singleton(
            llds_instr(label(TrieNodeLabel), "")
        ),
        SetCodeUnitCode = singleton(
            llds_instr(assign(CodeUnitRegLval, GetCurCodeUnitRval), "")
        ),
        CodeUnitRval = lval(CodeUnitRegLval),
        generate_trie_goto_fail_code(Info, GotoFailCode),

        chase_any_stick_in_trie(TrieNode, ChoicePairs,
            StickCodeUnits, TrieNodeAfterStick),
        (
            StickCodeUnits = [_, _ | _],
            list.length(StickCodeUnits, NumStickCodeUnits),
            CmpOp = offset_str_eq(NumMatched, size(NumStickCodeUnits)),
            list.reverse(RevMatchedCodeUnits, MatchedCodeUnits),
            get_encoding(Info, Encoding),
            MatchedStickCodeUnits = MatchedCodeUnits ++ StickCodeUnits,
            det_from_code_unit_list_in_encoding_allow_ill_formed(Encoding,
                MatchedStickCodeUnits, MatchedStickStr),
            TestComment =
                "MatchedCodeUnits " ++ string.string(MatchedCodeUnits) ++
                " StickCodeUnits " ++ string.string(StickCodeUnits),
            TestRval = binop(CmpOp,
                VarRval, const(llconst_string(MatchedStickStr))),
            convert_trie_to_nested_switches(Info, VarRval,
                NumMatched + NumStickCodeUnits,
                TrieNodeAfterStick, TrieNodeLabelAfterStick,
                CodeAfterStick, !CI),
            TrieNodeCodeAddrAfterStick = code_label(TrieNodeLabelAfterStick),
            TestCode = singleton(
                llds_instr(if_val(TestRval, TrieNodeCodeAddrAfterStick),
                    TestComment)
            ),
            TestChainCode = TestCode ++ GotoFailCode,
            Code = LabelCode ++ TestChainCode ++ CodeAfterStick
        ;
            ( StickCodeUnits = []
            ; StickCodeUnits = [_]
            ),
            convert_trie_choices_to_nested_switches(Info, VarRval,
                NumMatched + 1, ChoicePairs,
                cord.init, NestedTrieInfosCord, 0, NumActions0,
                empty, NestedTrieCode, !CI),
            NestedTrieInfos0 = cord.list(NestedTrieInfosCord),
            (
                MaybeEnd = no,
                NestedTrieInfos = NestedTrieInfos0,
                NumActions = NumActions0
            ;
                MaybeEnd = yes(EndCaseId),
                EndNestedTrieInfo =
                    code_unit_to_action(0, action_case(EndCaseId)),
                NestedTrieInfos = [EndNestedTrieInfo | NestedTrieInfos0],
                NumActions = NumActions0 + 1
            ),
            ( if NumActions =< 3 then
                generate_nested_trie_try_chain(Info, CodeUnitRval,
                    NestedTrieInfos, empty, TestCode, !CI)
            else
                generate_nested_trie_binary_search(Info, CodeUnitRval,
                    NumActions, NestedTrieInfos, TestCode, !CI)
            ),
            Code = LabelCode ++ SetCodeUnitCode ++ TestCode ++ NestedTrieCode
        )
    ).

:- pred convert_trie_choices_to_nested_switches(string_trie_switch_info::in,
    rval::in, int::in, assoc_list(int, trie_node)::in,
    cord(code_unit_to_action)::in, cord(code_unit_to_action)::out,
    int::in, int::out, llds_code::in, llds_code::out,
    code_info::in, code_info::out) is det.

convert_trie_choices_to_nested_switches(_, _, _, [],
        !CodeUnitToActionsCord, !NumActions, !NestedTrieCode, !CI).
convert_trie_choices_to_nested_switches(Info, VarRval,
        NumMatched, [Choice | Choices],
        !CodeUnitToActionsCord, !NumActions, !NestedTrieCode, !CI) :-
    Choice = CodeUnit - TrieNode,
    convert_trie_to_nested_switches(Info, VarRval, NumMatched,
        TrieNode, TrieNodeLabel, TrieNodeCode, !CI),
    CodeUnitToAction =
        code_unit_to_action(CodeUnit, action_nested_trie(TrieNodeLabel)),
    cord.snoc(CodeUnitToAction, !CodeUnitToActionsCord),
    !:NumActions = !.NumActions + 1,
    !:NestedTrieCode = !.NestedTrieCode ++ TrieNodeCode,
    convert_trie_choices_to_nested_switches(Info, VarRval, NumMatched, Choices,
        !CodeUnitToActionsCord, !NumActions, !NestedTrieCode, !CI).

%---------------------%

:- pred generate_nested_trie_try_chain(string_trie_switch_info::in, rval::in,
    list(code_unit_to_action)::in, llds_code::in, llds_code::out,
    code_info::in, code_info::out) is det.

generate_nested_trie_try_chain(Info, _, [], !TryChainCode, !CI) :-
    generate_trie_goto_fail_code(Info, GotoFailCode),
    !:TryChainCode = !.TryChainCode ++ GotoFailCode.
generate_nested_trie_try_chain(Info, CodeUnitRval,
        [CodeUnitToAction | CodeUnitToActions], !TryChainCode, !CI) :-
    CodeUnitToAction = code_unit_to_action(CodeUnit, Action),
    CondRval = binop(eq(int_type_int),
        CodeUnitRval, const(llconst_int(CodeUnit))),
    (
        Action = action_nested_trie(NestedTrieNodeLabel),
        TestCodeUnitCode = singleton(
            llds_instr(if_val(CondRval, code_label(NestedTrieNodeLabel)), "")
        )
    ;
        Action = action_case(CaseId),
        generate_trie_case_or_fall_through(Info, CondRval, CaseId,
            TestCodeUnitCode, !CI)
    ),
    !:TryChainCode = !.TryChainCode ++ TestCodeUnitCode,
    generate_nested_trie_try_chain(Info, CodeUnitRval,
        CodeUnitToActions, !TryChainCode, !CI).

%---------------------%

:- pred generate_nested_trie_binary_search(string_trie_switch_info::in,
    rval::in, int::in, list(code_unit_to_action)::in, llds_code::out,
    code_info::in, code_info::out) is det.

generate_nested_trie_binary_search(Info, CodeUnitRval,
        NumActions, CodeUnitToActions, TestCode, !CI) :-
    ( if NumActions =< 3 then
        generate_nested_trie_try_chain(Info, CodeUnitRval,
            CodeUnitToActions, empty, TestCode, !CI)
    else
        NumActionsR = NumActions / 2,
        NumActionsL = NumActions - NumActionsR,
        list.det_split_list(NumActionsL, CodeUnitToActions,
            CodeUnitToActionsL, CodeUnitToActionsR),
        list.det_head(CodeUnitToActionsR, HeadCodeUnitToActions),
        HeadCodeUnitToActions = code_unit_to_action(LeastCodeUnitR, _),
        code_info.get_next_label(LabelR, !CI),

        TestRvalLR = binop(int_ge(int_type_int),
            CodeUnitRval, const(llconst_int(LeastCodeUnitR))),
        CommentLR = "binary search on code unit",
        TestCodeLR = singleton(
            llds_instr(if_val(TestRvalLR, code_label(LabelR)), CommentLR)
        ),
        generate_nested_trie_binary_search(Info, CodeUnitRval,
            NumActionsL, CodeUnitToActionsL, TestCodeL, !CI),
        LabelCodeR = singleton(
            llds_instr(label(LabelR), "")
        ),
        generate_nested_trie_binary_search(Info, CodeUnitRval,
            NumActionsR, CodeUnitToActionsR, TestCodeR, !CI),
        TestCode = TestCodeLR ++ TestCodeL ++ LabelCodeR ++ TestCodeR
    ).

%---------------------------------------------------------------------------%

:- type string_trie_switch_info_jump
    --->    string_trie_switch_info_jump(
                stsij_encoding              :: string_encoding,
                stsij_case_id_to_label_map  :: map(case_id, label),
                stsij_branch_start          :: position_info,
                stsij_code_unit_reg         :: lval,
                stsij_fail_label            :: label,
                stsij_fail_code             :: llds_code,
                stsij_goto_fail_code        :: llds_code
            ).

:- pred  init_string_trie_switch_info_jump(can_fail::in,
    string_trie_switch_info_jump::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

init_string_trie_switch_info_jump(CanFail, Info, !CI, !.CLD) :-
    Encoding = target_string_encoding(target_c),

    % See the comment about acquire_reg/release_reg in the
    % init_string_binary_switch_info predicate below.
    acquire_reg(reg_r, CodeUnitReg, !CLD),
    release_reg(CodeUnitReg, !CLD),

    remember_position(!.CLD, BranchStart),

    % We generate a fail label even if CanFail = cannot_fail, so that
    % we have somewhere to go to throw an exception at runtime.
    get_next_label(FailLabel, !CI),
    % We must generate the failure code in the context in which
    % none of the switch arms have been executed yet.
    generate_string_switch_fail(CanFail, FailCode, !CI, !.CLD),

    GotoFailCode = singleton(
        llds_instr(goto(code_label(FailLabel)), "no match; goto fail")
    ),

    % The code that creates the case_id to label map needs the
    % other fields of the string_trie_switch_info_jump structure as its inputs,
    % so this field will be filled in with *real* data by our caller.
    map.init(CaseIdToLabelMap0),
    Info = string_trie_switch_info_jump(Encoding, CaseIdToLabelMap0,
        BranchStart, CodeUnitReg, FailLabel, FailCode, GotoFailCode).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_string_trie_lookup_switch(VarRval, LookupSwitchInfo, CanFail,
        EndLabel, StoreMap, !:MaybeEnd, Code, !:CI) :-
    LookupSwitchInfo = lookup_switch_info(KeyToCaseIdMap, CaseConsts,
        OutVars, OutTypes, Liveness, !:MaybeEnd, !:CI, CLD),
    init_string_trie_switch_info_lookup(CanFail, LookupInfo, !CI, CLD),
    CaseNumRegLval = LookupInfo ^ stsil_case_id_reg,
    InitCaseNumRegCode = singleton(
        llds_instr(assign(CaseNumRegLval, const(llconst_int(-1))),
            "initialize case id to invalid")
    ),
    Info = stsi_lookup(LookupInfo),
    map.to_sorted_assoc_list(KeyToCaseIdMap, StrsCaseIds),
    create_nested_trie_switch(Info, VarRval, StrsCaseIds, TrieCode, !CI),
    AfterLabel = LookupInfo ^ stsil_after_label,
    AfterLabelCode = singleton(
        llds_instr(label(AfterLabel), "after the trie search")
    ),
    SetCaseNumCode = InitCaseNumRegCode ++ TrieCode ++ AfterLabelCode,
    % We must generate the failure code in the context in which
    % none of the switch arms have been executed yet.
    (
        CanFail = cannot_fail,
        SetAndCheckCaseNumCode = SetCaseNumCode
    ;
        CanFail = can_fail,
        get_next_label(NonFailLabel, !CI),
        CaseNumIsValid = binop(int_ge(int_type_int),
            lval(CaseNumRegLval), const(llconst_int(0))),
        TestForFailCode = singleton(
            llds_instr(if_val(CaseNumIsValid, code_label(NonFailLabel)),
                "branch around fail code")
        ),
        FailCode = LookupInfo ^ stsil_fail_code,
        NonFailLabelCode = singleton(
            llds_instr(label(NonFailLabel), "non-fail label")
        ),
        SetAndCheckCaseNumCode = SetCaseNumCode ++
            TestForFailCode ++ FailCode ++ NonFailLabelCode
    ),

    (
        CaseConsts = all_one_soln(CaseIdToValuesMap),
        map.to_assoc_list(CaseIdToValuesMap, CaseIdToValuesAL),
        generate_string_trie_simple_lookup_switch(LookupInfo, CaseIdToValuesAL,
            OutVars, OutTypes, Liveness, EndLabel, StoreMap,
            SetAndCheckCaseNumCode, Code, !MaybeEnd, !CI, CLD)
    ;
        CaseConsts = some_several_solns(_, _),
        unexpected($pred, "some_several_solns")
    ).

%---------------------------------------------------------------------------%

:- pred generate_string_trie_simple_lookup_switch(
    string_trie_switch_info_lookup::in,
    assoc_list(case_id, list(rval))::in, list(prog_var)::in,
    list(llds_type)::in, set_of_progvar::in,
    label::in, abs_store_map::in, llds_code::in, llds_code::out,
    branch_end::in, branch_end::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_string_trie_simple_lookup_switch(LookupInfo, CaseValues,
        OutVars, OutTypes, Liveness, EndLabel, StoreMap,
        SetAndCheckCaseNumCode, Code, !MaybeEnd, !CI, !.CLD) :-
    (
        OutVars = [],
        SetBaseRegCode = empty
    ;
        OutVars = [_ | _],
        RowElemTypes = OutTypes,

        construct_string_trie_simple_lookup_vector(CaseValues, 0,
            cord.init, VectorRvalsCord),
        VectorRvals = cord.list(VectorRvalsCord),
        add_vector_static_cell(RowElemTypes, VectorRvals, VectorAddr, !CI),
        VectorAddrRval = const(llconst_data_addr(VectorAddr, no)),

        % Since we release BaseReg only after the call to generate_branch_end,
        % we must make sure that generate_branch_end won't want to
        % overwrite BaseReg.
        acquire_reg_not_in_storemap(StoreMap, reg_r, BaseRegLval, !CLD),

        % Generate code to look up each of the variables in OutVars
        % in its slot in the table row RowStartReg.
        %
        % Here, we just set up the pointer to the right row; the code that
        % picks up the values of OutVars from that row is generated by
        % the combination of record_offset_assigns (which updates
        % the code generator state !CLD to tell it where OutVars are)
        % and set_liveness_and_end_branch (which generates the code
        % to pick up their values from there).
        CaseIdRegLval = LookupInfo ^ stsil_case_id_reg,
        list.length(OutVars, NumColumns),
        SetBaseRegCode = singleton(
            llds_instr(
                assign(BaseRegLval,
                    mem_addr(heap_ref(VectorAddrRval, yes(ptag(0u8)),
                        binop(int_mul(int_type_int),
                            lval(CaseIdRegLval),
                            const(llconst_int(NumColumns)))))),
                "set up base reg")
        ),
        NumPrevColumns = 0,
        record_offset_assigns(OutVars, NumPrevColumns, BaseRegLval, !.CI, !CLD)
    ),

    % We keep track of what variables are supposed to be live at the end
    % of cases. We have to do this explicitly because generating a `fail'
    % slot last would yield the wrong liveness.
    set_liveness_and_end_branch(StoreMap, Liveness, !MaybeEnd, BranchEndCode,
        !.CLD),

    EndLabelCode = singleton(
        llds_instr(label(EndLabel),
            "end of simple trie string lookup switch")
    ),
    MatchCode = SetBaseRegCode ++ BranchEndCode ++ EndLabelCode,
    Code = SetAndCheckCaseNumCode ++ MatchCode.

:- pred construct_string_trie_simple_lookup_vector(
    assoc_list(case_id, list(rval))::in, int::in,
    cord(list(rval))::in, cord(list(rval))::out) is det.

construct_string_trie_simple_lookup_vector([], _, !RowsCord).
construct_string_trie_simple_lookup_vector([CaseValues | CasesValues], RowNum,
        !RowsCord) :-
    CaseValues = CaseId - OutVarRvals,
    CaseId = case_id(CaseIdNum),
    expect(unify(RowNum, CaseIdNum), $pred, "RowNum != CaseIdNum"),
    Row = OutVarRvals,
    cord.snoc(Row, !RowsCord),
    construct_string_trie_simple_lookup_vector(CasesValues, RowNum + 1,
        !RowsCord).

%---------------------%

:- type string_trie_switch_info_lookup
    --->    string_trie_switch_info_lookup(
                stsil_encoding              :: string_encoding,
                stsil_branch_start          :: position_info,
                stsil_code_unit_reg         :: lval,
                stsil_case_id_reg           :: lval,
                stsil_after_label           :: label,
                stsil_after_code_addr       :: code_addr,
                stsil_goto_after_code       :: llds_code,
                stsil_fail_code             :: llds_code
            ).

:- pred  init_string_trie_switch_info_lookup(can_fail::in,
    string_trie_switch_info_lookup::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

init_string_trie_switch_info_lookup(CanFail, Info, !CI, !.CLD) :-
    Encoding = target_string_encoding(target_c),

    % See the comment about acquire_reg/release_reg in the
    % init_string_binary_switch_info predicate below.
    acquire_reg(reg_r, CodeUnitReg, !CLD),
    acquire_reg(reg_r, CaseIdReg, !CLD),
    release_reg(CodeUnitReg, !CLD),
    release_reg(CaseIdReg, !CLD),

    remember_position(!.CLD, BranchStart),

    get_next_label(AfterLabel, !CI),
    AfterCodeAddr = code_label(AfterLabel),

    GotoAfterCode = singleton(
        llds_instr(goto(AfterCodeAddr), "go to the code after the trie")
    ),

    (
        CanFail = cannot_fail,
        FailCode = empty
    ;
        CanFail = can_fail,
        generate_failure(FailCode, !CI, !.CLD)
    ),

    Info = string_trie_switch_info_lookup(Encoding, BranchStart,
        CodeUnitReg, CaseIdReg, AfterLabel, AfterCodeAddr, GotoAfterCode,
        FailCode).

%---------------------------------------------------------------------------%

:- type string_trie_switch_info
    --->    stsi_jump(string_trie_switch_info_jump)
    ;       stsi_lookup(string_trie_switch_info_lookup).

%---------------------%

:- pred generate_trie_case_or_fall_through(string_trie_switch_info::in,
    rval::in, case_id::in, llds_code::out, code_info::in, code_info::out)
    is det.

generate_trie_case_or_fall_through(Info, CondRval, CaseId, CaseCode, !CI) :-
    (
        Info = stsi_jump(JumpInfo),
        JumpInfo = string_trie_switch_info_jump(_Encoding, CaseIdToLabelMap,
            _CodeUnitRegLval, _BranchStart, _FailLabel, _FailCode,
            _GotoFailCode),
        map.lookup(CaseIdToLabelMap, CaseId, CaseLabel),
        CaseCodeAddr = code_label(CaseLabel),
        CaseCode = singleton(
            llds_instr(if_val(CondRval, CaseCodeAddr), "if match; goto case")
        )
        % If the test fails, fall through
    ;
        Info = stsi_lookup(LookupInfo),
        get_next_label(FallThroughLabel, !CI),
        CaseIdRegLval = LookupInfo ^ stsil_case_id_reg,
        AfterCodeAddr = LookupInfo ^ stsil_after_code_addr,
        negate_rval(CondRval, NegCondRval),
        CaseId = case_id(CaseIdNum),
        CaseIdRval = const(llconst_int(CaseIdNum)),
        CaseCode = from_list([
            llds_instr(if_val(NegCondRval, code_label(FallThroughLabel)),
                "if not match; fall through"),
            llds_instr(assign(CaseIdRegLval, CaseIdRval), "assign CaseId"),
            llds_instr(goto(AfterCodeAddr), "match; goto end"),
            llds_instr(label(FallThroughLabel), "fall through label")
        ])
    ).

:- pred generate_trie_goto_fail_code(string_trie_switch_info::in,
    llds_code::out) is det.

generate_trie_goto_fail_code(Info, GotoFailCode) :-
    (
        Info = stsi_jump(JumpInfo),
        GotoFailCode = JumpInfo ^ stsij_goto_fail_code
    ;
        Info = stsi_lookup(LookupInfo),
        AfterCodeAddr = LookupInfo ^ stsil_after_code_addr,
        GotoFailCode = singleton(
            llds_instr(goto(AfterCodeAddr),
                "no match; goto after with no case id set")
        )
    ).

:- pred get_encoding(string_trie_switch_info::in, string_encoding::out) is det.

get_encoding(Info, Encoding) :-
    (
        Info = stsi_jump(JumpInfo),
        Encoding = JumpInfo ^ stsij_encoding
    ;
        Info = stsi_lookup(LookupInfo),
        Encoding = LookupInfo ^ stsil_encoding
    ).

:- pred get_code_unit_reg(string_trie_switch_info::in, lval::out) is det.

get_code_unit_reg(Info, CodeUnitReg) :-
    (
        Info = stsi_jump(JumpInfo),
        CodeUnitReg = JumpInfo ^ stsij_code_unit_reg
    ;
        Info = stsi_lookup(LookupInfo),
        CodeUnitReg = LookupInfo ^ stsil_code_unit_reg
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_string_hash_jump_switch(VarRval, VarName, TaggedCases,
        CodeModel, CanFail, SwitchGoalInfo, EndLabel, MaybeEnd, Code,
        !CI, CLD) :-
    init_string_hash_switch_info(CanFail, HashSwitchInfo, !CI, CLD),
    BranchStart = HashSwitchInfo ^ shsi_branch_start,
    Params = represent_params(VarName, SwitchGoalInfo, CodeModel, BranchStart,
        EndLabel),

    % Generate code for the cases, and remember the label of each case.
    map.init(CaseLabelMap0),
    represent_tagged_cases_in_string_hash_switch(Params, TaggedCases,
        [], StrsLabels, CaseLabelMap0, CaseLabelMap, no, MaybeEnd, !CI),

    % Compute the hash table.
    construct_string_hash_cases(StrsLabels, allow_doubling,
        TableSize, HashSlotsMap, HashOp, NumCollisions),
    HashMask = TableSize - 1,

    % Generate the data structures for the hash table.
    FailLabel = HashSwitchInfo ^ shsi_fail_label,
    construct_string_hash_jump_vectors(0, TableSize, HashSlotsMap, FailLabel,
        NumCollisions, cord.init, TableRowsCord, cord.init, MaybeTargetsCord),
    TableRows = cord.list(TableRowsCord),
    MaybeTargets = cord.list(MaybeTargetsCord),

    % Generate the code for the hash table lookup.
    ( if NumCollisions = 0 then
        NumColumns = 1,
        RowElemTypes = [lt_string],
        ArrayElemTypes = [scalar_elem_string]
    else
        NumColumns = 2,
        RowElemTypes = [lt_string, lt_int(int_type_int)],
        ArrayElemTypes = [scalar_elem_string, scalar_elem_int]
    ),
    add_vector_static_cell(RowElemTypes, TableRows, TableAddr, !CI),
    ArrayElemType = array_elem_struct(ArrayElemTypes),
    TableAddrRval = const(llconst_data_addr(TableAddr, no)),

    SlotReg = HashSwitchInfo ^ shsi_slot_reg,
    MatchCode = from_list([
        % See the comment at the top of the module about why we use
        % a computed_goto here.
        llds_instr(computed_goto(lval(SlotReg), MaybeTargets),
            "jump to the corresponding code")
    ]),

    % Generate the code for the cases.
    add_not_yet_included_cases(CasesCode, CaseLabelMap, _),
    % Put it all together.
    generate_string_hash_switch_search(HashSwitchInfo, VarRval, TableAddrRval,
        ArrayElemType, NumColumns, HashOp, HashMask, NumCollisions,
        EndLabel, "jump", CasesCode, MatchCode, Code).

:- pred construct_string_hash_jump_vectors(int::in, int::in,
    map(int, string_hash_slot(label))::in, label::in, int::in,
    cord(list(rval))::in, cord(list(rval))::out,
    cord(maybe(label))::in, cord(maybe(label))::out) is det.

construct_string_hash_jump_vectors(Slot, TableSize, HashSlotMap, FailLabel,
        NumCollisions, !TableRowsCord, !MaybeTargetsCord) :-
    ( if Slot = TableSize then
        true
    else
        ( if map.search(HashSlotMap, Slot, SlotInfo) then
            SlotInfo = string_hash_slot(String, Next, CaseLabel),
            NextSlotRval = const(llconst_int(Next)),
            StringRval = const(llconst_string(String)),
            Target = CaseLabel
        else
            StringRval = const(llconst_int(0)),
            NextSlotRval = const(llconst_int(-2)),
            Target = FailLabel
        ),
        ( if NumCollisions = 0 then
            TableRow = [StringRval]
        else
            TableRow = [StringRval, NextSlotRval]
        ),
        cord.snoc(TableRow, !TableRowsCord),
        cord.snoc(yes(Target), !MaybeTargetsCord),
        construct_string_hash_jump_vectors(Slot + 1, TableSize, HashSlotMap,
            FailLabel, NumCollisions, !TableRowsCord, !MaybeTargetsCord)
    ).

:- pred represent_tagged_cases_in_string_hash_switch(represent_params::in,
    list(tagged_case)::in,
    assoc_list(string, label)::in, assoc_list(string, label)::out,
    case_label_map::in, case_label_map::out,
    branch_end::in, branch_end::out, code_info::in, code_info::out) is det.

represent_tagged_cases_in_string_hash_switch(_, [],
        !StrsLabels, !CaseLabelMap, !MaybeEnd, !CI).
represent_tagged_cases_in_string_hash_switch(Params,
        [TaggedCase | TaggedCases],
        !StrsLabels, !CaseLabelMap, !MaybeEnd, !CI) :-
    represent_tagged_case_for_llds(Params, TaggedCase, Label,
        !CaseLabelMap, !MaybeEnd, !CI, unit, _),
    TaggedCase = tagged_case(MainTaggedConsId, OtherTaggedConsIds, _, _),
    record_label_for_string(Label, MainTaggedConsId, !StrsLabels),
    list.foldl(record_label_for_string(Label), OtherTaggedConsIds,
        !StrsLabels),
    represent_tagged_cases_in_string_hash_switch(Params, TaggedCases,
        !StrsLabels, !CaseLabelMap, !MaybeEnd, !CI).

:- pred record_label_for_string(label::in, tagged_cons_id::in,
    assoc_list(string, label)::in, assoc_list(string, label)::out) is det.

record_label_for_string(Label, TaggedConsId, !StrsLabels) :-
    TaggedConsId = tagged_cons_id(_ConsId, Tag),
    ( if Tag = string_tag(String) then
        !:StrsLabels = [String - Label | !.StrsLabels]
    else
        unexpected($pred, "non-string tag")
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_string_hash_lookup_switch(VarRval, LookupSwitchInfo,
        CanFail, EndLabel, StoreMap, !:MaybeEnd, Code, !:CI) :-
    LookupSwitchInfo = lookup_switch_info(KeyToCaseIdMap, CaseConsts,
        OutVars, OutTypes, Liveness, !:MaybeEnd, !:CI, CLD),
    (
        CaseConsts = all_one_soln(CaseIdToValuesMap),
        compose_maps(KeyToCaseIdMap, CaseIdToValuesMap, KeyToValuesMap),
        map.to_assoc_list(KeyToValuesMap, KeyValuesAL),
        generate_string_hash_simple_lookup_switch(VarRval, KeyValuesAL,
            OutVars, OutTypes, Liveness, CanFail, EndLabel, StoreMap,
            Code, !MaybeEnd, !CI, CLD)
    ;
        CaseConsts = some_several_solns(CaseIdToSolnsMap,
            case_consts_several_llds(ResumeVars, GoalsMayModifyTrail)),
        compose_maps(KeyToCaseIdMap, CaseIdToSolnsMap, KeyToSolnsMap),
        map.to_assoc_list(KeyToSolnsMap, KeySolnsAL),
        generate_string_hash_several_soln_lookup_switch(VarRval, KeySolnsAL,
            ResumeVars, GoalsMayModifyTrail, OutVars, OutTypes, Liveness,
            CanFail, EndLabel, StoreMap, Code, !MaybeEnd, !CI, CLD)
    ).

%---------------------------------------------------------------------------%

:- pred generate_string_hash_simple_lookup_switch(rval::in,
    assoc_list(string, list(rval))::in, list(prog_var)::in,
    list(llds_type)::in, set_of_progvar::in,
    can_fail::in, label::in, abs_store_map::in,
    llds_code::out, branch_end::in, branch_end::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_string_hash_simple_lookup_switch(VarRval, CaseValues,
        OutVars, OutTypes, Liveness, CanFail, EndLabel, StoreMap,
        Code, !MaybeEnd, !CI, !.CLD) :-
    % This predicate, generate_string_hash_several_soln_lookup_switch,
    % and generate_string_hash_lookup_switch do similar tasks using
    % similar code, so if you need to update one, you probably need to
    % update them all.
    init_string_hash_switch_info(CanFail, HashSwitchInfo, !CI, !.CLD),

    % Compute the hash table.
    construct_string_hash_cases(CaseValues, allow_doubling, TableSize,
        HashSlotsMap, HashOp, NumCollisions),
    HashMask = TableSize - 1,

    list.length(OutVars, NumOutVars),
    % For the LLDS backend, array indexing ops don't need the element types,
    % so it is ok to lie for OutElemTypes.
    list.duplicate(NumOutVars, scalar_elem_generic, OutElemTypes),
    DummyOutRvals = list.map(default_value_for_type, OutTypes),
    ( if NumCollisions = 0 then
        NumPrevColumns = 1,
        NumColumns = 1 + NumOutVars,
        ArrayElemTypes = [scalar_elem_string | OutElemTypes],
        RowElemTypes = [lt_string | OutTypes]
    else
        NumPrevColumns = 2,
        NumColumns = 2 + NumOutVars,
        ArrayElemTypes = [scalar_elem_string, scalar_elem_int | OutElemTypes],
        RowElemTypes = [lt_string, lt_int(int_type_int) | OutTypes]
    ),
    ArrayElemType = array_elem_struct(ArrayElemTypes),

    % Generate the static lookup table for this switch.
    construct_string_hash_simple_lookup_vector(0, TableSize, HashSlotsMap,
        NumCollisions, DummyOutRvals, cord.init, VectorRvalsCord),
    VectorRvals = cord.list(VectorRvalsCord),
    add_vector_static_cell(RowElemTypes, VectorRvals, VectorAddr, !CI),
    VectorAddrRval = const(llconst_data_addr(VectorAddr, no)),

    (
        OutVars = [],
        SetBaseRegCode = empty
    ;
        OutVars = [_ | _],
        % Since we release BaseReg only after the call to generate_branch_end,
        % we must make sure that generate_branch_end won't want to
        % overwrite BaseReg.
        acquire_reg_not_in_storemap(StoreMap, reg_r, BaseReg, !CLD),

        % Generate code to look up each of the variables in OutVars
        % in its slot in the table row RowStartReg.
        %
        % Here, we just set up the pointer to the right row; the code that
        % picks up the values of OutVars from that row is generated by
        % the combination of record_offset_assigns (which updates
        % the code generator state !CLD to tell it where OutVars are)
        % and set_liveness_and_end_branch (which generates the code
        % to pick up their values from there).
        RowStartReg = HashSwitchInfo ^ shsi_row_start_reg,
        SetBaseRegCode = singleton(
            llds_instr(
                assign(BaseReg,
                    mem_addr(heap_ref(VectorAddrRval, yes(ptag(0u8)),
                        lval(RowStartReg)))),
                "set up base reg")
        ),
        record_offset_assigns(OutVars, NumPrevColumns, BaseReg, !.CI, !CLD)
    ),

    % We keep track of what variables are supposed to be live at the end
    % of cases. We have to do this explicitly because generating a `fail'
    % slot last would yield the wrong liveness.
    set_liveness_and_end_branch(StoreMap, Liveness, !MaybeEnd, BranchEndCode,
        !.CLD),

    GotoEndLabelCode = singleton(
        llds_instr(goto(code_label(EndLabel)),
            "go to end of simple hash string lookup switch")
    ),
    MatchCode = SetBaseRegCode ++ BranchEndCode ++ GotoEndLabelCode,
    generate_string_hash_switch_search(HashSwitchInfo, VarRval,
        VectorAddrRval, ArrayElemType, NumColumns, HashOp, HashMask,
        NumCollisions, EndLabel, "single soln lookup", empty, MatchCode, Code).

:- pred construct_string_hash_simple_lookup_vector(int::in, int::in,
    map(int, string_hash_slot(list(rval)))::in, int::in, list(rval)::in,
    cord(list(rval))::in, cord(list(rval))::out) is det.

construct_string_hash_simple_lookup_vector(Slot, TableSize, HashSlotMap,
        NumCollisions, DummyOutRvals, !RowsCord) :-
    ( if Slot = TableSize then
        true
    else
        ( if map.search(HashSlotMap, Slot, SlotInfo) then
            SlotInfo = string_hash_slot(String, Next, OutVarRvals),
            NextSlotRval = const(llconst_int(Next)),
            StringRval = const(llconst_string(String))
        else
            StringRval = const(llconst_int(0)),
            NextSlotRval = const(llconst_int(-2)),
            OutVarRvals = DummyOutRvals
        ),
        ( if NumCollisions = 0 then
            Row = [StringRval | OutVarRvals]
        else
            Row = [StringRval, NextSlotRval | OutVarRvals]
        ),
        cord.snoc(Row, !RowsCord),
        construct_string_hash_simple_lookup_vector(Slot + 1, TableSize,
            HashSlotMap, NumCollisions, DummyOutRvals, !RowsCord)
    ).

%---------------------------------------------------------------------------%

:- pred generate_string_hash_several_soln_lookup_switch(rval::in,
    assoc_list(string, soln_consts(rval))::in, set_of_progvar::in, bool::in,
    list(prog_var)::in, list(llds_type)::in, set_of_progvar::in,
    can_fail::in, label::in, abs_store_map::in,
    llds_code::out, branch_end::in, branch_end::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_string_hash_several_soln_lookup_switch(VarRval, CaseSolns,
        ResumeVars, GoalsMayModifyTrail, OutVars, OutTypes, Liveness,
        CanFail, EndLabel, StoreMap, Code, !MaybeEnd, !CI, !.CLD) :-
    % This predicate, generate_string_hash_simple_lookup_switch,
    % and generate_string_hash_lookup_switch do similar tasks using
    % similar code, so if you need to update one, you probably need to
    % update them all.
    init_string_hash_switch_info(CanFail, HashSwitchInfo, !CI, !.CLD),

    % Compute the hash table.
    construct_string_hash_cases(CaseSolns, allow_doubling, TableSize,
        HashSlotsMap, HashOp, NumCollisions),
    HashMask = TableSize - 1,

    list.length(OutVars, NumOutVars),
    % For the LLDS backend, array indexing ops don't need the element types,
    % so it is ok to lie for OutElemTypes.
    list.duplicate(NumOutVars, scalar_elem_generic, OutElemTypes),
    ( if NumCollisions = 0 then
        NumColumns = 3 + NumOutVars,
        NumPrevColumns = 1,
        ArrayElemTypes = [scalar_elem_string,
            scalar_elem_int, scalar_elem_int | OutElemTypes],
        MainRowTypes = [lt_string, lt_int(int_type_int),
            lt_int(int_type_int) | OutTypes]
    else
        NumColumns = 4 + NumOutVars,
        NumPrevColumns = 2,
        ArrayElemTypes = [scalar_elem_string, scalar_elem_int,
            scalar_elem_int, scalar_elem_int | OutElemTypes],
        MainRowTypes = [lt_string, lt_int(int_type_int), lt_int(int_type_int),
            lt_int(int_type_int) | OutTypes]
    ),
    ArrayElemType = array_elem_struct(ArrayElemTypes),

    % If there are no output variables, then how can the individual solutions
    % differ from each other?
    expect_not(unify(OutVars, []), $pred, "no OutVars"),
    (
        GoalsMayModifyTrail = yes,
        get_emit_trail_ops(!.CI, EmitTrailOps),
        AddTrailOps = EmitTrailOps
    ;
        GoalsMayModifyTrail = no,
        AddTrailOps = do_not_add_trail_ops
    ),

    % Generate the static lookup table for this switch.
    InitLaterSolnRowNumber = 1,
    DummyOutRvals = list.map(default_value_for_type, OutTypes),
    LaterSolnArrayCord0 = singleton(DummyOutRvals),
    construct_string_hash_several_soln_lookup_vector(0, TableSize,
        HashSlotsMap, DummyOutRvals, NumOutVars, NumCollisions,
        cord.init, MainRowsCord, InitLaterSolnRowNumber,
        LaterSolnArrayCord0, LaterSolnArrayCord,
        0, OneSolnCaseCount, 0, SeveralSolnsCaseCount),
    MainRows = cord.list(MainRowsCord),
    LaterSolnArray = cord.list(LaterSolnArrayCord),

    list.sort([OneSolnCaseCount - kind_one_soln,
        SeveralSolnsCaseCount - kind_several_solns],
        AscendingSortedCountKinds),
    list.reverse(AscendingSortedCountKinds, DescendingSortedCountKinds),
    assoc_list.values(DescendingSortedCountKinds, DescendingSortedKinds),
    det_list_to_one_or_more(DescendingSortedKinds, OoMDescendingSortedKinds),

    add_vector_static_cell(MainRowTypes, MainRows, MainVectorAddr, !CI),
    MainVectorAddrRval = const(llconst_data_addr(MainVectorAddr, no)),
    add_vector_static_cell(OutTypes, LaterSolnArray, LaterVectorAddr, !CI),
    LaterVectorAddrRval = const(llconst_data_addr(LaterVectorAddr, no)),

    % Since we release BaseReg only after the calls to generate_branch_end,
    % we must make sure that generate_branch_end won't want to overwrite
    % BaseReg.
    acquire_reg_not_in_storemap(StoreMap, reg_r, BaseReg, !CLD),

    % Generate code to look up each of the variables in OutVars
    % in its slot in the table row RowStartReg.
    %
    % Here, we just set up the pointer to the right row; the code that
    % picks up the values of OutVars from that row is generated by
    % the combination of record_offset_assigns (which updates
    % the code generator state !CLD to tell it where OutVars are)
    % and set_liveness_and_end_branch (which generates the code
    % to pick up their values from there). Both are invoked by code inside
    % generate_code_for_all_kinds.
    RowStartReg = HashSwitchInfo ^ shsi_row_start_reg,
    SetBaseRegCode = singleton(
        llds_instr(
            assign(BaseReg,
                mem_addr(heap_ref(MainVectorAddrRval, yes(ptag(0u8)),
                    lval(RowStartReg)))),
            "set up base reg")
    ),
    generate_code_for_all_kinds(OoMDescendingSortedKinds, NumPrevColumns,
        OutVars, ResumeVars, EndLabel, StoreMap, Liveness, AddTrailOps,
        BaseReg, LaterVectorAddrRval, LookupResultsCode,
        !MaybeEnd, !CI, !.CLD),
    MatchCode = SetBaseRegCode ++ LookupResultsCode,
    generate_string_hash_switch_search(HashSwitchInfo, VarRval,
        MainVectorAddrRval, ArrayElemType, NumColumns, HashOp, HashMask,
        NumCollisions, EndLabel, "multi soln lookup", empty, MatchCode, Code).

:- pred construct_string_hash_several_soln_lookup_vector(int::in, int::in,
    map(int, string_hash_slot(soln_consts(rval)))::in, list(rval)::in,
    int::in, int::in, cord(list(rval))::in, cord(list(rval))::out,
    int::in, cord(list(rval))::in, cord(list(rval))::out,
    int::in, int::out, int::in, int::out) is det.

construct_string_hash_several_soln_lookup_vector(Slot, TableSize, HashSlotMap,
        DummyOutRvals, NumOutVars, NumCollisions,
        !MainRowsCord, !.LaterNextRow, !LaterSolnArray,
        !OneSolnCaseCount, !SeveralSolnsCaseCount) :-
    ( if Slot = TableSize then
        true
    else
        ( if map.search(HashSlotMap, Slot, SlotInfo) then
            SlotInfo = string_hash_slot(String, Next, Soln),
            StringRval = const(llconst_string(String)),
            NextSlotRval = const(llconst_int(Next)),
            (
                Soln = one_soln(OutVarRvals),
                !:OneSolnCaseCount = !.OneSolnCaseCount + 1,
                ZeroRval = const(llconst_int(0)),
                % The first ZeroRval means there is exactly one solution for
                % this case; the second ZeroRval is a dummy that won't be
                % referenced.
                MainRowTail = [ZeroRval, ZeroRval | OutVarRvals],
                ( if NumCollisions = 0 then
                    MainRow = [StringRval | MainRowTail]
                else
                    MainRow = [StringRval, NextSlotRval | MainRowTail]
                )
            ;
                Soln = several_solns(FirstSolnRvals, LaterSolns),
                !:SeveralSolnsCaseCount = !.SeveralSolnsCaseCount + 1,
                list.length(LaterSolns, NumLaterSolns),
                FirstRowOffset = !.LaterNextRow * NumOutVars,
                LastRowOffset = (!.LaterNextRow + NumLaterSolns - 1)
                    * NumOutVars,
                FirstRowRval = const(llconst_int(FirstRowOffset)),
                LastRowRval = const(llconst_int(LastRowOffset)),
                MainRowTail = [FirstRowRval, LastRowRval | FirstSolnRvals],
                ( if NumCollisions = 0 then
                    MainRow = [StringRval | MainRowTail]
                else
                    MainRow = [StringRval, NextSlotRval | MainRowTail]
                ),
                !:LaterNextRow = !.LaterNextRow + NumLaterSolns,
                !:LaterSolnArray = !.LaterSolnArray ++ from_list(LaterSolns)
            )
        else
            % The zero in the StringRval slot means that this bucket is empty.
            StringRval = const(llconst_int(0)),
            NextSlotRval = const(llconst_int(-2)),
            ZeroRval = const(llconst_int(0)),
            MainRowTail = [ZeroRval, ZeroRval | DummyOutRvals],
            ( if NumCollisions = 0 then
                MainRow = [StringRval | MainRowTail]
            else
                MainRow = [StringRval, NextSlotRval | MainRowTail]
            )
        ),
        cord.snoc(MainRow, !MainRowsCord),
        construct_string_hash_several_soln_lookup_vector(Slot + 1, TableSize,
            HashSlotMap, DummyOutRvals, NumOutVars, NumCollisions,
            !MainRowsCord, !.LaterNextRow, !LaterSolnArray,
            !OneSolnCaseCount, !SeveralSolnsCaseCount)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type string_hash_switch_info
    --->    string_hash_switch_info(
                shsi_slot_reg           :: lval,
                shsi_row_start_reg      :: lval,
                shsi_string_reg         :: lval,

                shsi_loop_start_label   :: label,
                shsi_no_match_label     :: label,
                shsi_fail_label         :: label,

                shsi_branch_start       :: position_info,
                shsi_fail_code          :: llds_code
            ).

:- pred  init_string_hash_switch_info(can_fail::in,
    string_hash_switch_info::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

init_string_hash_switch_info(CanFail, Info, !CI, !.CLD) :-
    % See the comment about acquire_reg/release_reg in the
    % init_string_binary_switch_info predicate below.
    acquire_reg(reg_r, SlotReg, !CLD),
    acquire_reg(reg_r, RowStartReg, !CLD),
    acquire_reg(reg_r, StringReg, !CLD),
    release_reg(SlotReg, !CLD),
    release_reg(RowStartReg, !CLD),
    release_reg(StringReg, !CLD),

    get_next_label(LoopStartLabel, !CI),
    get_next_label(FailLabel, !CI),
    get_next_label(NoMatchLabel, !CI),

    % We must generate the failure code in the context in which
    % none of the switch arms have been executed yet.
    remember_position(!.CLD, BranchStart),
    generate_string_switch_fail(CanFail, FailCode, !CI, !.CLD),

    Info = string_hash_switch_info(SlotReg, RowStartReg, StringReg,
        LoopStartLabel, NoMatchLabel, FailLabel, BranchStart, FailCode).

:- pred generate_string_hash_switch_search(string_hash_switch_info::in,
    rval::in, rval::in, array_elem_type::in, int::in, unary_op::in, int::in,
    int::in, label::in, string::in, llds_code::in, llds_code::in,
    llds_code::out) is det.

generate_string_hash_switch_search(Info, VarRval, TableAddrRval,
        ArrayElemType, NumColumns, HashOp, HashMask, NumCollisions,
        EndLabel, SwitchKindStr, CasesCode, MatchCode, Code) :-
    SlotReg = Info ^ shsi_slot_reg,
    RowStartReg = Info ^ shsi_row_start_reg,
    StringReg = Info ^ shsi_string_reg,
    LoopStartLabel = Info ^ shsi_loop_start_label,
    NoMatchLabel = Info ^ shsi_no_match_label,
    FailLabel = Info ^ shsi_fail_label,
    FailCode = Info ^ shsi_fail_code,

    ( if NumCollisions = 0 then
        ( if NumColumns = 1 then
            BaseReg = SlotReg,
            MultiplyInstrs = []
        else
            BaseReg = RowStartReg,
            MultiplyInstrs = [
                llds_instr(
                    assign(RowStartReg,
                        binop(int_mul(int_type_int),
                            lval(SlotReg), const(llconst_int(NumColumns)))),
                    "find the start of the row")
            ]
        ),
        HashSearchCode =
            from_list([
                llds_instr(
                    assign(SlotReg,
                        binop(bitwise_and(int_type_int),
                            unop(HashOp, VarRval),
                            const(llconst_int(HashMask)))),
                    "compute the hash value of the input string") |
                MultiplyInstrs]) ++
            from_list([
                llds_instr(
                    assign(StringReg,
                        binop(array_index(ArrayElemType),
                            TableAddrRval, lval(BaseReg))),
                    "lookup the string for this hash slot"),
                llds_instr(
                    if_val(
                        binop(logical_or,
                            binop(eq(int_type_int),
                                lval(StringReg), const(llconst_int(0))),
                            binop(str_ne, lval(StringReg), VarRval)),
                    code_label(FailLabel)),
                    "did we find a match? nofulljump")
            ]) ++
            MatchCode ++
            from_list([
                llds_instr(label(FailLabel),
                    "handle the failure of the table search")
            ])
    else
        HashSearchCode =
            from_list([
                llds_instr(
                    assign(SlotReg,
                        binop(bitwise_and(int_type_int),
                            unop(HashOp, VarRval),
                            const(llconst_int(HashMask)))),
                    "compute the hash value of the input string"),
                llds_instr(label(LoopStartLabel),
                    "begin hash chain loop, nofulljump"),
                llds_instr(
                    assign(RowStartReg,
                        binop(int_mul(int_type_int),
                            lval(SlotReg), const(llconst_int(NumColumns)))),
                    "find the start of the row"),
                llds_instr(
                    assign(StringReg,
                        binop(array_index(ArrayElemType),
                            TableAddrRval, lval(RowStartReg))),
                    "lookup the string for this hash slot"),
                llds_instr(
                    if_val(
                        binop(logical_or,
                            binop(eq(int_type_int),
                                lval(StringReg), const(llconst_int(0))),
                            binop(str_ne, lval(StringReg), VarRval)),
                        code_label(NoMatchLabel)),
                    "did we find a match? nofulljump")
            ]) ++
            MatchCode ++
            from_list([
                llds_instr(label(NoMatchLabel),
                    "no match yet, nofulljump"),
                llds_instr(
                    assign(SlotReg,
                        binop(array_index(ArrayElemType),
                            TableAddrRval,
                            binop(int_add(int_type_int),
                                lval(RowStartReg), const(llconst_int(1))))),
                    "get next slot in hash chain"),
                llds_instr(
                    if_val(
                        binop(int_ge(int_type_int),
                            lval(SlotReg), const(llconst_int(0))),
                        code_label(LoopStartLabel)),
                    "if not at the end of the chain, keep searching"),
                llds_instr(label(FailLabel),
                    "handle the failure of the table search")
            ])
    ),
    string.format("string hash %s switch",
        [s(SwitchKindStr)], StartComment),
    string.format("end of string hash %s switch",
        [s(SwitchKindStr)], EndLabelComment),
    StartCommentCode = singleton(
        llds_instr(comment(StartComment), "")
    ),
    EndLabelCode = singleton(
        llds_instr(label(EndLabel), EndLabelComment)
    ),
    Code = StartCommentCode ++ HashSearchCode ++ FailCode ++
        CasesCode ++ EndLabelCode.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_string_binary_jump_switch(VarRval, VarName, TaggedCases,
        CodeModel, CanFail, SwitchGoalInfo, EndLabel, MaybeEnd, Code,
        !CI, CLD) :-
    init_string_binary_switch_info(CanFail, BinarySwitchInfo, !CI, CLD),
    BranchStart = BinarySwitchInfo ^ sbsi_branch_start,
    Params = represent_params(VarName, SwitchGoalInfo, CodeModel, BranchStart,
        EndLabel),
    CommentCode = singleton(
        llds_instr(comment("string binary jump switch"), "")
    ),

    % Compute and generate the binary search table.
    map.init(CaseLabelMap0),
    string_binary_cases(TaggedCases, represent_tagged_case_for_llds(Params),
        CaseLabelMap0, CaseLabelMap, no, MaybeEnd, !CI, unit, _, SortedTable),

    gen_string_binary_jump_slots(SortedTable,
        cord.init, TableRowsCord, cord.init, TargetsCord,
        0, TableSize),
    TableRows = cord.list(TableRowsCord),
    Targets = cord.list(TargetsCord),
    NumColumns = 2,
    RowElemTypes = [lt_string, lt_int(int_type_int)],
    add_vector_static_cell(RowElemTypes, TableRows, TableAddr, !CI),
    ArrayElemTypes = [scalar_elem_string, scalar_elem_int],
    ArrayElemType = array_elem_struct(ArrayElemTypes),
    TableAddrRval = const(llconst_data_addr(TableAddr, no)),

    generate_string_binary_switch_search(BinarySwitchInfo,
        VarRval, TableAddrRval, ArrayElemType, TableSize, NumColumns,
        BinarySearchCode),

    MidReg = BinarySwitchInfo ^ sbsi_mid_reg,
    % See the comment at the top about why we use a computed_goto here.
    ComputedGotoCode = singleton(
        llds_instr(
            computed_goto(
                binop(array_index(ArrayElemType),
                    TableAddrRval,
                    binop(int_add(int_type_int),
                        binop(int_mul(int_type_int),
                            lval(MidReg),
                            const(llconst_int(NumColumns))),
                        const(llconst_int(1)))),
                Targets),
            "jump to the matching case")
    ),

    % Generate the code for the cases.
    add_not_yet_included_cases(CasesCode, CaseLabelMap, _),
    EndLabelCode = singleton(
        llds_instr(label(EndLabel), "end of binary string switch")
    ),

    Code = CommentCode ++ BinarySearchCode ++ ComputedGotoCode ++
        CasesCode ++ EndLabelCode.

:- pred gen_string_binary_jump_slots(assoc_list(string, label)::in,
    cord(list(rval))::in, cord(list(rval))::out,
    cord(maybe(label))::in, cord(maybe(label))::out,
    int::in, int::out) is det.

gen_string_binary_jump_slots([], !TableRowsCord, !MaybeTargetsCord, !CurIndex).
gen_string_binary_jump_slots([Str - Label | StrLabels],
        !TableRowsCord, !MaybeTargetsCord, !CurIndex) :-
    Row = [const(llconst_string(Str)), const(llconst_int(!.CurIndex))],
    cord.snoc(Row, !TableRowsCord),
    cord.snoc(yes(Label), !MaybeTargetsCord),
    !:CurIndex = !.CurIndex + 1,
    gen_string_binary_jump_slots(StrLabels,
        !TableRowsCord, !MaybeTargetsCord, !CurIndex).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_string_binary_lookup_switch(VarRval, LookupSwitchInfo,
        CanFail, EndLabel, StoreMap, !:MaybeEnd, Code, !:CI) :-
    LookupSwitchInfo = lookup_switch_info(KeyToCaseIdMap, CaseConsts,
        OutVars, OutTypes, Liveness, !:MaybeEnd, !:CI, CLD),
    (
        CaseConsts = all_one_soln(CaseIdToValuesMap),
        compose_maps(KeyToCaseIdMap, CaseIdToValuesMap, KeyToValuesMap),
        map.to_assoc_list(KeyToValuesMap, KeyValuesAL),
        generate_string_binary_simple_lookup_switch(VarRval, KeyValuesAL,
            OutVars, OutTypes, Liveness, CanFail, EndLabel, StoreMap,
            Code, !MaybeEnd, !CI, CLD)
    ;
        CaseConsts = some_several_solns(CaseIdToSolnsMap,
            case_consts_several_llds(ResumeVars, GoalsMayModifyTrail)),
        compose_maps(KeyToCaseIdMap, CaseIdToSolnsMap, KeyToSolnsMap),
        map.to_assoc_list(KeyToSolnsMap, KeySolnsAL),
        generate_string_binary_several_soln_lookup_switch(VarRval, KeySolnsAL,
            ResumeVars, GoalsMayModifyTrail, OutVars, OutTypes, Liveness,
            CanFail, EndLabel, StoreMap, Code, !MaybeEnd, !CI, CLD)
    ).

%---------------------------------------------------------------------------%

:- pred generate_string_binary_simple_lookup_switch(rval::in,
    assoc_list(string, list(rval))::in, list(prog_var)::in,
    list(llds_type)::in, set_of_progvar::in,
    can_fail::in, label::in, abs_store_map::in,
    llds_code::out, branch_end::in, branch_end::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_string_binary_simple_lookup_switch(VarRval, CaseValues,
        OutVars, OutTypes, Liveness, CanFail, EndLabel, StoreMap,
        Code, !MaybeEnd, !CI, !.CLD) :-
    % This predicate, generate_string_binary_several_soln_lookup_switch,
    % and generate_string_binary_lookup_switch do similar tasks using
    % similar code, so if you need to update one, you probably need to
    % update them all.

    init_string_binary_switch_info(CanFail, BinarySwitchInfo, !CI, !.CLD),
    CommentCode = singleton(
        llds_instr(comment("string binary simple lookup switch"), "")
    ),

    list.length(CaseValues, TableSize),
    list.length(OutVars, NumOutVars),
    NumColumns = 1 + NumOutVars,
    % For the LLDS backend, array indexing ops don't need the element types,
    % so it is ok to lie here.
    list.duplicate(NumOutVars, scalar_elem_generic, OutElemTypes),
    ArrayElemTypes = [scalar_elem_string | OutElemTypes],
    ArrayElemType = array_elem_struct(ArrayElemTypes),

    % Generate the static lookup table for this switch.
    construct_string_binary_simple_lookup_vector(CaseValues,
        cord.init, VectorRvalsCord),
    VectorRvals = cord.list(VectorRvalsCord),
    RowElemTypes = [lt_string | OutTypes],
    add_vector_static_cell(RowElemTypes, VectorRvals, VectorAddr, !CI),
    VectorAddrRval = const(llconst_data_addr(VectorAddr, no)),

    generate_string_binary_switch_search(BinarySwitchInfo, VarRval,
        VectorAddrRval, ArrayElemType, TableSize, NumColumns,
        BinarySearchCode),
    (
        OutVars = [],
        SetBaseRegCode = empty
    ;
        OutVars = [_ | _],
        % Since we release BaseReg only after the call to
        % generate_branch_end, we must make sure that generate_branch_end
        % won't want to overwrite BaseReg.
        acquire_reg_not_in_storemap(StoreMap, reg_r, BaseReg, !CLD),

        % Generate code to look up each of the variables in OutVars
        % in its slot in the table row MidReg.
        %
        % Here, we just set up the pointer to the right row; the code that
        % picks up the values of OutVars from that row is generated by
        % the combination of record_offset_assigns (which updates
        % the code generator state !CLD to tell it where OutVars are)
        % and set_liveness_and_end_branch (which generates the code
        % to pick up their values from there).
        MidReg = BinarySwitchInfo ^ sbsi_mid_reg,
        SetBaseRegCode = singleton(
            llds_instr(
                assign(BaseReg,
                    mem_addr(
                        heap_ref(VectorAddrRval, yes(ptag(0u8)),
                            binop(int_mul(int_type_int),
                                lval(MidReg),
                                const(llconst_int(NumColumns)))))),
                "set up base reg")
        ),
        record_offset_assigns(OutVars, 1, BaseReg, !.CI, !CLD)
    ),

    % We keep track of what variables are supposed to be live at the end
    % of cases. We have to do this explicitly because generating a `fail'
    % slot last would yield the wrong liveness.
    set_liveness_and_end_branch(StoreMap, Liveness, no, _MaybeEnd,
        BranchEndCode, !.CLD),
    EndLabelCode = singleton(
        llds_instr(label(EndLabel),
            "end of string binary single soln lookup switch")
    ),
    Code = CommentCode ++ BinarySearchCode ++ SetBaseRegCode ++
        BranchEndCode ++ EndLabelCode.

:- pred construct_string_binary_simple_lookup_vector(
    assoc_list(string, list(rval))::in,
    cord(list(rval))::in, cord(list(rval))::out) is det.

construct_string_binary_simple_lookup_vector([], !RowsCord).
construct_string_binary_simple_lookup_vector([Str - OutRvals | StrsOutRvals],
        !RowsCord) :-
    RowRvals = [const(llconst_string(Str)) | OutRvals],
    cord.snoc(RowRvals, !RowsCord),
    construct_string_binary_simple_lookup_vector(StrsOutRvals, !RowsCord).

%---------------------------------------------------------------------------%

:- pred generate_string_binary_several_soln_lookup_switch(rval::in,
    assoc_list(string, soln_consts(rval))::in, set_of_progvar::in, bool::in,
    list(prog_var)::in, list(llds_type)::in, set_of_progvar::in,
    can_fail::in, label::in, abs_store_map::in,
    llds_code::out, branch_end::in, branch_end::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_string_binary_several_soln_lookup_switch(VarRval, CaseSolns,
        ResumeVars, GoalsMayModifyTrail, OutVars, OutTypes, Liveness,
        CanFail, EndLabel, StoreMap, Code, !MaybeEnd, !CI, !.CLD) :-
    % This predicate, generate_string_binary_simple_lookup_switch,
    % and generate_string_binary_lookup_switch do similar tasks using
    % similar code, so if you need to update one, you probably need to
    % update them all.

    init_string_binary_switch_info(CanFail, BinarySwitchInfo, !CI, !.CLD),
    CommentCode = singleton(
        llds_instr(comment("string binary multi soln lookup switch"), "")
    ),

    list.length(CaseSolns, MainTableSize),
    list.length(OutVars, NumOutVars),
    % For the LLDS backend, array indexing ops don't need the element types,
    % so it is ok to lie here.
    list.duplicate(NumOutVars, scalar_elem_generic, OutElemTypes),
    ArrayElemTypes =
        [scalar_elem_string, scalar_elem_int, scalar_elem_int | OutElemTypes],
    ArrayElemType = array_elem_struct(ArrayElemTypes),

    % If there are no output variables, then how can the individual solutions
    % differ from each other?
    expect_not(unify(OutVars, []), $pred, "no OutVars"),
    (
        GoalsMayModifyTrail = yes,
        get_emit_trail_ops(!.CI, EmitTrailOps),
        AddTrailOps = EmitTrailOps
    ;
        GoalsMayModifyTrail = no,
        AddTrailOps = do_not_add_trail_ops
    ),

    % Now generate the static cells into which we do the lookups of the values
    % of the output variables, if there are any.
    %
    % We put a dummy row at the start of the later solns table, so that
    % a zero in the "later solns start row" column of the main table can mean
    % "no later solutions".
    InitLaterSolnRowNumber = 1,
    DummyLaterSolnRow = list.map(default_value_for_type, OutTypes),
    LaterSolnArrayCord0 = singleton(DummyLaterSolnRow),
    construct_string_binary_several_soln_lookup_vector(CaseSolns,
        NumOutVars, cord.init, MainRowsCord,
        InitLaterSolnRowNumber, LaterSolnArrayCord0, LaterSolnArrayCord,
        0, OneSolnCaseCount, 0, SeveralSolnsCaseCount),
    MainRows = cord.list(MainRowsCord),
    LaterSolnArray = cord.list(LaterSolnArrayCord),

    list.sort([OneSolnCaseCount - kind_one_soln,
        SeveralSolnsCaseCount - kind_several_solns],
        AscendingSortedCountKinds),
    list.reverse(AscendingSortedCountKinds, DescendingSortedCountKinds),
    assoc_list.values(DescendingSortedCountKinds, DescendingSortedKinds),
    det_list_to_one_or_more(DescendingSortedKinds, OoMDescendingSortedKinds),

    MainRowTypes =
        [lt_string, lt_int(int_type_int), lt_int(int_type_int) | OutTypes],
    list.length(MainRowTypes, MainNumColumns),
    add_vector_static_cell(MainRowTypes, MainRows, MainVectorAddr, !CI),
    MainVectorAddrRval = const(llconst_data_addr(MainVectorAddr, no)),
    add_vector_static_cell(OutTypes, LaterSolnArray, LaterVectorAddr, !CI),
    LaterVectorAddrRval = const(llconst_data_addr(LaterVectorAddr, no)),

    % Since we release BaseReg only after the calls to generate_branch_end,
    % we must make sure that generate_branch_end won't want to overwrite
    % BaseReg.
    acquire_reg_not_in_storemap(StoreMap, reg_r, BaseReg, !CLD),
    generate_string_binary_switch_search(BinarySwitchInfo, VarRval,
        MainVectorAddrRval, ArrayElemType, MainTableSize, MainNumColumns,
        BinarySearchCode),

    % Generate code to look up each of the variables in OutVars
    % in its slot in the table row MidReg.
    %
    % Here, we just set up the pointer to the right row; the code that
    % picks up the values of OutVars from that row is generated by
    % the combination of record_offset_assigns (which updates
    % the code generator state !CLD to tell it where OutVars are)
    % and set_liveness_and_end_branch (which generates the code
    % to pick up their values from there). Both are invoked by code inside
    % generate_code_for_all_kinds.
    MidReg = BinarySwitchInfo ^ sbsi_mid_reg,
    SetBaseRegCode = singleton(
        llds_instr(
            assign(BaseReg,
                mem_addr(
                    heap_ref(MainVectorAddrRval, yes(ptag(0u8)),
                        binop(int_mul(int_type_int),
                            lval(MidReg),
                            const(llconst_int(MainNumColumns)))))),
            "set up base reg")
    ),

    generate_code_for_all_kinds(OoMDescendingSortedKinds, 1, OutVars,
        ResumeVars, EndLabel, StoreMap, Liveness, AddTrailOps, BaseReg,
        LaterVectorAddrRval, LookupResultsCode, !MaybeEnd, !CI, !.CLD),
    EndLabelCode = singleton(
        llds_instr(label(EndLabel),
            "end of string binary multi solns lookup switch")
    ),
    Code = CommentCode ++ BinarySearchCode ++ SetBaseRegCode ++
        LookupResultsCode ++ EndLabelCode.

:- pred construct_string_binary_several_soln_lookup_vector(
    assoc_list(string, soln_consts(rval))::in, int::in,
    cord(list(rval))::in, cord(list(rval))::out,
    int::in, cord(list(rval))::in, cord(list(rval))::out,
    int::in, int::out, int::in, int::out) is det.

construct_string_binary_several_soln_lookup_vector([],
        _NumOutVars, !MainRowsCord, _LaterNextRow, !LaterSolnArray,
        !OneSolnCaseCount, !SeveralSolnCaseCount).
construct_string_binary_several_soln_lookup_vector([Str - Soln | StrSolns],
        NumOutVars, !MainRowsCord, !.LaterNextRow, !LaterSolnArray,
        !OneSolnCaseCount, !SeveralSolnsCaseCount) :-
    StrRval = const(llconst_string(Str)),
    (
        Soln = one_soln(OutRvals),
        !:OneSolnCaseCount = !.OneSolnCaseCount + 1,
        ZeroRval = const(llconst_int(0)),
        % The first ZeroRval means there is exactly one solution for this case;
        % the second ZeroRval is a dummy that won't be referenced.
        MainRow = [StrRval, ZeroRval, ZeroRval | OutRvals]
    ;
        Soln = several_solns(FirstSolnRvals, LaterSolns),
        !:SeveralSolnsCaseCount = !.SeveralSolnsCaseCount + 1,
        list.length(LaterSolns, NumLaterSolns),
        FirstRowOffset = !.LaterNextRow * NumOutVars,
        LastRowOffset = (!.LaterNextRow + NumLaterSolns - 1) * NumOutVars,
        FirstRowRval = const(llconst_int(FirstRowOffset)),
        LastRowRval = const(llconst_int(LastRowOffset)),
        MainRow = [StrRval, FirstRowRval, LastRowRval | FirstSolnRvals],
        !:LaterNextRow = !.LaterNextRow + NumLaterSolns,
        !:LaterSolnArray = !.LaterSolnArray ++ from_list(LaterSolns)
    ),
    cord.snoc(MainRow, !MainRowsCord),
    construct_string_binary_several_soln_lookup_vector(StrSolns, NumOutVars,
        !MainRowsCord, !.LaterNextRow, !LaterSolnArray,
        !OneSolnCaseCount, !SeveralSolnsCaseCount).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type string_binary_switch_info
    --->    string_binary_switch_info(
                sbsi_lo_reg             :: lval,
                sbsi_hi_reg             :: lval,
                sbsi_mid_reg            :: lval,
                sbsi_result_reg         :: lval,

                sbsi_loop_start_label   :: label,
                sbsi_gt_eq_label        :: label,
                sbsi_eq_label           :: label,
                sbsi_fail_label         :: label,

                sbsi_branch_start       :: position_info,
                sbsi_fail_code          :: llds_code
            ).

:- pred init_string_binary_switch_info(can_fail::in,
    string_binary_switch_info::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

init_string_binary_switch_info(CanFail, Info, !CI, !.CLD) :-
    % We get the registers we use as working storage in the implementation
    % of binary search, and in the implementation of trie and hash switches
    % above. We get them now, before we generate the code of the switch arms,
    % because the set of free registers will in general be different
    % before and after that action. However, it is safe to release them
    % immediately, even though we haven't yet generated all the code
    % which uses them, because with all three implementation methods, that
    % code will *only* be executed before the code for the cases.
    % Releasing the registers early allows the code we generate for the cases
    % to make use of them.
    acquire_reg(reg_r, LoReg, !CLD),
    acquire_reg(reg_r, HiReg, !CLD),
    acquire_reg(reg_r, MidReg, !CLD),
    acquire_reg(reg_r, ResultReg, !CLD),
    release_reg(LoReg, !CLD),
    release_reg(HiReg, !CLD),
    release_reg(MidReg, !CLD),
    release_reg(ResultReg, !CLD),

    get_next_label(LoopStartLabel, !CI),
    get_next_label(GtEqLabel, !CI),
    get_next_label(EqLabel, !CI),
    get_next_label(FailLabel, !CI),

    % We must generate the failure code in the context in which
    % none of the switch arms have been executed yet.
    remember_position(!.CLD, BranchStart),
    generate_string_switch_fail(CanFail, FailCode, !CI, !.CLD),

    Info = string_binary_switch_info(LoReg, HiReg, MidReg, ResultReg,
        LoopStartLabel, GtEqLabel, EqLabel, FailLabel, BranchStart, FailCode).

    % Generate code for the binary search. This code will execute FailCode
    % if the key is not in the table, and will fall through if it is, leaving
    % the index of the matching row in the register specified by
    % Info ^ sbsi_mid_reg.
    %
:- pred generate_string_binary_switch_search(string_binary_switch_info::in,
    rval::in, rval::in, array_elem_type::in, int::in, int::in,
    llds_code::out) is det.

generate_string_binary_switch_search(Info, VarRval, TableAddrRval,
        ArrayElemType, TableSize, NumColumns, Code) :-
    Info = string_binary_switch_info(LoReg, HiReg, MidReg, ResultReg,
        LoopStartLabel, GtEqLabel, EqLabel, FailLabel, _BranchStart, FailCode),

    MaxIndex = TableSize - 1,
    Code =
        from_list([
            llds_instr(assign(LoReg, const(llconst_int(0))), ""),
            llds_instr(assign(HiReg, const(llconst_int(MaxIndex))), ""),

% LoopStartLabel
            llds_instr(label(LoopStartLabel),
                "begin table search loop, nofulljump"),
            llds_instr(
                if_val(
                    binop(int_gt(int_type_int), lval(LoReg), lval(HiReg)),
                    code_label(FailLabel)),
                "have we searched all of the table?"),
            llds_instr(
                assign(MidReg,
                    binop(int_div(int_type_int),
                        binop(int_add(int_type_int), lval(LoReg), lval(HiReg)),
                        const(llconst_int(2)))), ""),
            llds_instr(
                assign(ResultReg,
                    binop(str_cmp,
                        VarRval,
                        binop(array_index(ArrayElemType),
                            TableAddrRval,
                            binop(int_mul(int_type_int),
                                lval(MidReg),
                                const(llconst_int(NumColumns)))))),
                "compare with the middle element"),
            llds_instr(
                if_val(
                    binop(int_ge(int_type_int),
                        lval(ResultReg), const(llconst_int(0))),
                    code_label(GtEqLabel)),
                "branch away unless key is in lo half"),
            llds_instr(
                assign(HiReg,
                    binop(int_sub(int_type_int),
                        lval(MidReg), const(llconst_int(1)))),
                ""),
            llds_instr(goto(code_label(LoopStartLabel)),
                "go back to search the remaining lo half"),

% GtEqLabel
            llds_instr(label(GtEqLabel), "nofulljump"),
            llds_instr(
                if_val(
                    binop(int_le(int_type_int),
                        lval(ResultReg), const(llconst_int(0))),
                    code_label(EqLabel)),
                "branch away unless key is in hi half"),
            llds_instr(
                assign(LoReg,
                    binop(int_add(int_type_int),
                        lval(MidReg), const(llconst_int(1)))),
                ""),
            llds_instr(goto(code_label(LoopStartLabel)),
                "go back to search the remaining hi half"),

% FailLabel
            llds_instr(label(FailLabel),
                "handle the failure of the table search")
        ]) ++
        FailCode ++
% EqLabel
        singleton(
            llds_instr(label(EqLabel), "we found the key")
        ).

%---------------------------------------------------------------------------%

:- pred generate_string_switch_fail(can_fail::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_string_switch_fail(CanFail, FailCode, !CI, !.CLD) :-
    (
        CanFail = can_fail,
        generate_failure(FailCode, !CI, !.CLD)
    ;
        CanFail = cannot_fail,
        FailCode = singleton(
            llds_instr(
                comment("unreachable; fail code in cannot_fail switch"), "")
        )
    ).

%---------------------------------------------------------------------------%
:- end_module ll_backend.string_switch.
%---------------------------------------------------------------------------%
