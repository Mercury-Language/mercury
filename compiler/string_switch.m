%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: string_switch.m.
% Authors: fjh, zs.
%
% For switches on strings, we can generate either
% - a hash table using open addressing to resolve hash conflicts, or
% - a sorted table for binary search.
%
% The hash table has a higher startup cost, but should use fewer comparisons,
% so it is preferred for bigger tables.
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
% of implementation. One possible way around the problem would be to do
% the code generation and optimization as we do now, just recording a bit
% more information during code generation about which numbers in static data
% refer to which computed_gotos, and then, after all the optimizations are
% done, to go back and replace all the indicated numbers with the corresponding
% final labels.
%
%-----------------------------------------------------------------------------%

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

:- pred generate_string_hash_switch(list(tagged_case)::in, rval::in,
    string::in, code_model::in, can_fail::in, hlds_goal_info::in, label::in,
    branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

:- pred generate_string_hash_lookup_switch(rval::in,
    lookup_switch_info(string)::in, can_fail::in, label::in, abs_store_map::in,
    branch_end::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

:- pred generate_string_binary_switch(list(tagged_case)::in, rval::in,
    string::in, code_model::in, can_fail::in, hlds_goal_info::in, label::in,
    branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

:- pred generate_string_binary_lookup_switch(rval::in,
    lookup_switch_info(string)::in, can_fail::in, label::in, abs_store_map::in,
    branch_end::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.switch_util.
:- import_module hlds.hlds_data.
:- import_module ll_backend.lookup_util.
:- import_module ll_backend.switch_case.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module std_util.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

generate_string_hash_switch(Cases, VarRval, VarName, CodeModel, CanFail,
        SwitchGoalInfo, EndLabel, MaybeEnd, Code, !CI, CLD) :-
    init_string_hash_switch_info(CanFail, HashSwitchInfo, !CI, CLD),
    BranchStart = HashSwitchInfo ^ shsi_branch_start,
    Params = represent_params(VarName, SwitchGoalInfo, CodeModel, BranchStart,
        EndLabel),
    CommentCode = singleton(
        llds_instr(comment("string hash jump switch"), "")
    ),

    % Generate code for the cases, and remember the label of each case.
    map.init(CaseLabelMap0),
    represent_tagged_cases_in_string_switch(Params, Cases, StrsLabels,
        CaseLabelMap0, CaseLabelMap, no, MaybeEnd, !CI),

    % Compute the hash table.
    construct_string_hash_cases(StrsLabels, allow_doubling,
        TableSize, HashSlotsMap, HashOp, NumCollisions),
    HashMask = TableSize - 1,

    % Generate the data structures for the hash table.
    FailLabel = HashSwitchInfo ^ shsi_fail_label,
    construct_string_hash_jump_vectors(0, TableSize, HashSlotsMap, FailLabel,
        NumCollisions, [], RevTableRows, [], RevTargets),
    list.reverse(RevTableRows, TableRows),
    list.reverse(RevTargets, Targets),

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
        % See the comment at the top about why we use a computed_goto here.
        llds_instr(computed_goto(lval(SlotReg), Targets),
            "jump to the corresponding code")
    ]),

    generate_string_hash_switch_search(HashSwitchInfo, VarRval, TableAddrRval,
        ArrayElemType, NumColumns, HashOp, HashMask, NumCollisions,
        MatchCode, HashLookupCode),

    % Generate the code for the cases.
    map.foldl(add_remaining_case, CaseLabelMap, empty, CasesCode),
    EndLabelCode = singleton(
        llds_instr(label(EndLabel), "end of hashed string switch")
    ),

    Code = CommentCode ++ HashLookupCode ++ CasesCode ++ EndLabelCode.

:- pred construct_string_hash_jump_vectors(int::in, int::in,
    map(int, string_hash_slot(label))::in, label::in, int::in,
    list(list(rval))::in, list(list(rval))::out,
    list(maybe(label))::in, list(maybe(label))::out) is det.

construct_string_hash_jump_vectors(Slot, TableSize, HashSlotMap, FailLabel,
        NumCollisions, !RevTableRows, !RevMaybeTargets) :-
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
        !:RevTableRows = [TableRow | !.RevTableRows],
        !:RevMaybeTargets = [yes(Target) | !.RevMaybeTargets],
        construct_string_hash_jump_vectors(Slot + 1, TableSize, HashSlotMap,
            FailLabel, NumCollisions, !RevTableRows, !RevMaybeTargets)
    ).

:- pred represent_tagged_cases_in_string_switch(represent_params::in,
    list(tagged_case)::in, assoc_list(string, label)::out,
    case_label_map::in, case_label_map::out,
    branch_end::in, branch_end::out, code_info::in, code_info::out) is det.

represent_tagged_cases_in_string_switch(_, [], [],
        !CaseLabelMap, !MaybeEnd, !CI).
represent_tagged_cases_in_string_switch(Params, [Case | Cases], !:StrsLabels,
        !CaseLabelMap, !MaybeEnd, !CI) :-
    represent_tagged_case_for_llds(Params, Case, Label,
        !CaseLabelMap, !MaybeEnd, !CI),
    represent_tagged_cases_in_string_switch(Params, Cases, !:StrsLabels,
        !CaseLabelMap, !MaybeEnd, !CI),
    Case = tagged_case(MainTaggedConsId, OtherTaggedConsIds, _, _),
    add_to_strs_labels(Label, MainTaggedConsId, !StrsLabels),
    list.foldl(add_to_strs_labels(Label), OtherTaggedConsIds, !StrsLabels).

:- pred add_to_strs_labels(label::in, tagged_cons_id::in,
    assoc_list(string, label)::in, assoc_list(string, label)::out) is det.

add_to_strs_labels(Label, TaggedConsId, !StrsLabels) :-
    TaggedConsId = tagged_cons_id(_ConsId, Tag),
    ( if Tag = string_tag(String) then
        !:StrsLabels = [String - Label | !.StrsLabels]
    else
        unexpected($pred, "non-string tag")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

generate_string_hash_lookup_switch(VarRval, LookupSwitchInfo,
        CanFail, EndLabel, StoreMap, !MaybeEnd, Code, !CI, CLD) :-
    LookupSwitchInfo = lookup_switch_info(CaseConsts, OutVars, OutTypes,
        Liveness),
    (
        CaseConsts = all_one_soln(CaseValueMap),
        map.to_assoc_list(CaseValueMap, CaseValues),
        generate_string_hash_simple_lookup_switch(VarRval, CaseValues,
            OutVars, OutTypes, Liveness, CanFail, EndLabel, StoreMap,
            !MaybeEnd, Code, !CI, CLD)
    ;
        CaseConsts = some_several_solns(CaseSolnMap,
            case_consts_several_llds(ResumeVars, GoalsMayModifyTrail)),
        map.to_assoc_list(CaseSolnMap, CaseSolns),
        generate_string_hash_several_soln_lookup_switch(VarRval, CaseSolns,
            ResumeVars, GoalsMayModifyTrail, OutVars, OutTypes, Liveness,
            CanFail, EndLabel, StoreMap, !MaybeEnd, Code, !CI, CLD)
    ).

%-----------------------------------------------------------------------------%

:- pred generate_string_hash_simple_lookup_switch(rval::in,
    assoc_list(string, list(rval))::in, list(prog_var)::in,
    list(llds_type)::in, set_of_progvar::in,
    can_fail::in, label::in, abs_store_map::in,
    branch_end::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_string_hash_simple_lookup_switch(VarRval, CaseValues,
        OutVars, OutTypes, Liveness, CanFail, EndLabel, StoreMap,
        !MaybeEnd, Code, !CI, !.CLD) :-
    % This predicate, generate_string_hash_several_soln_lookup_switch,
    % and generate_string_hash_lookup_switch do similar tasks using
    % similar code, so if you need to update one, you probably need to
    % update them all.

    init_string_hash_switch_info(CanFail, HashSwitchInfo, !CI, !.CLD),
    CommentCode = singleton(
        llds_instr(comment("string hash simple lookup switch"), "")
    ),

    % Compute the hash table.
    construct_string_hash_cases(CaseValues, allow_doubling,
        TableSize, HashSlotsMap, HashOp, NumCollisions),
    HashMask = TableSize - 1,

    list.length(OutVars, NumOutVars),
    % For the LLDS backend, array indexing ops don't need the element
    % types, so it is ok to lie for OutElemTypes.
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
        NumCollisions, DummyOutRvals, [], RevVectorRvals),
    list.reverse(RevVectorRvals, VectorRvals),
    add_vector_static_cell(RowElemTypes, VectorRvals, VectorAddr, !CI),
    VectorAddrRval = const(llconst_data_addr(VectorAddr, no)),

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
        % in its slot in the table row RowStartReg. Most of the change is done
        % by generate_offset_assigns associating each var with the relevant
        % field in !CLD.
        RowStartReg = HashSwitchInfo ^ shsi_row_start_reg,
        SetBaseRegCode = singleton(
            llds_instr(assign(BaseReg,
                mem_addr(heap_ref(VectorAddrRval, yes(ptag(0u8)),
                    lval(RowStartReg)))),
                "set up base reg")
        ),
        generate_offset_assigns(OutVars, NumPrevColumns, BaseReg, !.CI, !CLD)
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
    generate_string_hash_switch_search(HashSwitchInfo,
        VarRval, VectorAddrRval, ArrayElemType, NumColumns, HashOp, HashMask,
        NumCollisions, MatchCode, HashSearchCode),

    EndLabelCode = singleton(
        llds_instr(label(EndLabel),
            "end of simple hash string lookup switch")
    ),

    Code = CommentCode ++ HashSearchCode ++ EndLabelCode.

:- pred construct_string_hash_simple_lookup_vector(int::in, int::in,
    map(int, string_hash_slot(list(rval)))::in, int::in, list(rval)::in,
    list(list(rval))::in, list(list(rval))::out) is det.

construct_string_hash_simple_lookup_vector(Slot, TableSize, HashSlotMap,
        NumCollisions, DummyOutRvals, !RevRows) :-
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
        !:RevRows = [Row | !.RevRows],
        construct_string_hash_simple_lookup_vector(Slot + 1, TableSize,
            HashSlotMap, NumCollisions, DummyOutRvals, !RevRows)
    ).

%-----------------------------------------------------------------------------%

:- pred generate_string_hash_several_soln_lookup_switch(rval::in,
    assoc_list(string, soln_consts(rval))::in, set_of_progvar::in, bool::in,
    list(prog_var)::in, list(llds_type)::in, set_of_progvar::in,
    can_fail::in, label::in, abs_store_map::in,
    branch_end::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_string_hash_several_soln_lookup_switch(VarRval, CaseSolns,
        ResumeVars, GoalsMayModifyTrail, OutVars, OutTypes, Liveness,
        CanFail, EndLabel, StoreMap, !MaybeEnd, Code, !CI, !.CLD) :-
    % This predicate, generate_string_hash_simple_lookup_switch,
    % and generate_string_hash_lookup_switch do similar tasks using
    % similar code, so if you need to update one, you probably need to
    % update them all.

    init_string_hash_switch_info(CanFail, HashSwitchInfo, !CI, !.CLD),
    CommentCode = singleton(
        llds_instr(comment("string hash several soln lookup switch"), "")
    ),

    % Compute the hash table.
    construct_string_hash_cases(CaseSolns, allow_doubling, TableSize,
        HashSlotsMap, HashOp, NumCollisions),
    HashMask = TableSize - 1,

    list.length(OutVars, NumOutVars),
    % For the LLDS backend, array indexing ops don't need the element
    % types, so it is ok to lie for OutElemTypes.
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
    expect(negate(unify(OutVars, [])), $pred, "no OutVars"),
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
        [], RevMainRows, InitLaterSolnRowNumber,
        LaterSolnArrayCord0, LaterSolnArrayCord,
        0, OneSolnCaseCount, 0, SeveralSolnsCaseCount),
    list.reverse(RevMainRows, MainRows),
    LaterSolnArray = cord.list(LaterSolnArrayCord),

    list.sort([OneSolnCaseCount - kind_one_soln,
        SeveralSolnsCaseCount - kind_several_solns],
        AscendingSortedCountKinds),
    list.reverse(AscendingSortedCountKinds, DescendingSortedCountKinds),
    assoc_list.values(DescendingSortedCountKinds, DescendingSortedKinds),

    add_vector_static_cell(MainRowTypes, MainRows, MainVectorAddr, !CI),
    MainVectorAddrRval = const(llconst_data_addr(MainVectorAddr, no)),
    add_vector_static_cell(OutTypes, LaterSolnArray, LaterVectorAddr, !CI),
    LaterVectorAddrRval = const(llconst_data_addr(LaterVectorAddr, no)),

    % Since we release BaseReg only after the calls to generate_branch_end,
    % we must make sure that generate_branch_end won't want to overwrite
    % BaseReg.
    acquire_reg_not_in_storemap(StoreMap, reg_r, BaseReg, !CLD),

    % Generate code to look up each of the variables in OutVars
    % in its slot in the table row RowStartReg. Most of the change is done
    % by generate_offset_assigns associating each var with the relevant
    % field in !CI.
    RowStartReg = HashSwitchInfo ^ shsi_row_start_reg,
    SetBaseRegCode = singleton(
        llds_instr(assign(BaseReg,
            mem_addr(heap_ref(MainVectorAddrRval, yes(ptag(0u8)),
                lval(RowStartReg)))),
            "set up base reg")
    ),
    generate_code_for_all_kinds(DescendingSortedKinds, NumPrevColumns,
        OutVars, ResumeVars, EndLabel, StoreMap, Liveness, AddTrailOps,
        BaseReg, LaterVectorAddrRval, !MaybeEnd, LookupResultsCode,
        !CI, !.CLD),
    MatchCode = SetBaseRegCode ++ LookupResultsCode,

    generate_string_hash_switch_search(HashSwitchInfo,
        VarRval, MainVectorAddrRval, ArrayElemType, NumColumns,
        HashOp, HashMask, NumCollisions, MatchCode, HashSearchCode),
    EndLabelCode = singleton(
        llds_instr(label(EndLabel),
            "end of simple hash string lookup switch")
    ),
    Code = CommentCode ++ HashSearchCode ++ EndLabelCode.

:- pred construct_string_hash_several_soln_lookup_vector(int::in, int::in,
    map(int, string_hash_slot(soln_consts(rval)))::in, list(rval)::in,
    int::in, int::in, list(list(rval))::in, list(list(rval))::out,
    int::in, cord(list(rval))::in, cord(list(rval))::out,
    int::in, int::out, int::in, int::out) is det.

construct_string_hash_several_soln_lookup_vector(Slot, TableSize, HashSlotMap,
        DummyOutRvals, NumOutVars, NumCollisions,
        !RevMainRows, !.LaterNextRow, !LaterSolnArray,
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
        !:RevMainRows = [MainRow | !.RevMainRows],
        construct_string_hash_several_soln_lookup_vector(Slot + 1, TableSize,
            HashSlotMap, DummyOutRvals, NumOutVars, NumCollisions,
            !RevMainRows, !.LaterNextRow, !LaterSolnArray,
            !OneSolnCaseCount, !SeveralSolnsCaseCount)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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
    % We get the registers we use as working storage in the hash table lookup
    % code now, before we generate the code of the switch arms, since the set
    % of free registers will in general be different before and after that
    % action. However, it is safe to release them immediately, even though
    % we haven't yet generated all the code which uses them, because that
    % code will *only* be executed before the code for the cases, and because
    % that code is generated manually below. Releasing the registers early
    % allows the code of the cases to make use of them.

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
    int::in, llds_code::in, llds_code::out) is det.

generate_string_hash_switch_search(Info, VarRval, TableAddrRval,
        ArrayElemType, NumColumns, HashOp, HashMask, NumCollisions,
        MatchCode, Code) :-
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
                llds_instr(assign(RowStartReg,
                    binop(int_mul(int_type_int), lval(SlotReg),
                        const(llconst_int(NumColumns)))),
                    "find the start of the row")
            ]
        ),
        Code = from_list([
            llds_instr(assign(SlotReg,
                binop(bitwise_and(int_type_int), unop(HashOp, VarRval),
                    const(llconst_int(HashMask)))),
                "compute the hash value of the input string") |
            MultiplyInstrs]) ++
        from_list([
            llds_instr(assign(StringReg,
                binop(array_index(ArrayElemType), TableAddrRval,
                    lval(BaseReg))),
                "lookup the string for this hash slot"),
            llds_instr(if_val(
                    binop(logical_or,
                        binop(eq(int_type_int), lval(StringReg),
                            const(llconst_int(0))),
                        binop(str_ne, lval(StringReg), VarRval)),
                code_label(FailLabel)),
                "did we find a match? nofulljump")
        ]) ++ MatchCode ++ from_list([
            llds_instr(label(FailLabel),
                "handle the failure of the table search")
        ]) ++ FailCode
    else
        Code = from_list([
            llds_instr(assign(SlotReg,
                binop(bitwise_and(int_type_int), unop(HashOp, VarRval),
                    const(llconst_int(HashMask)))),
                "compute the hash value of the input string"),
            llds_instr(label(LoopStartLabel),
                "begin hash chain loop, nofulljump"),
            llds_instr(assign(RowStartReg,
                binop(int_mul(int_type_int), lval(SlotReg),
                    const(llconst_int(NumColumns)))),
                "find the start of the row"),
            llds_instr(assign(StringReg,
                binop(array_index(ArrayElemType), TableAddrRval,
                    lval(RowStartReg))),
                "lookup the string for this hash slot"),
            llds_instr(if_val(
                    binop(logical_or,
                        binop(eq(int_type_int), lval(StringReg),
                            const(llconst_int(0))),
                        binop(str_ne, lval(StringReg), VarRval)),
                code_label(NoMatchLabel)),
                "did we find a match? nofulljump")
        ]) ++ MatchCode ++ from_list([
            llds_instr(label(NoMatchLabel),
                "no match yet, nofulljump"),
            llds_instr(assign(SlotReg,
                binop(array_index(ArrayElemType), TableAddrRval,
                    binop(int_add(int_type_int), lval(RowStartReg),
                        const(llconst_int(1))))),
                "get next slot in hash chain"),
            llds_instr(
                if_val(binop(int_ge(int_type_int), lval(SlotReg),
                        const(llconst_int(0))),
                    code_label(LoopStartLabel)),
                "if we have not reached the end of the chain, keep searching"),
            llds_instr(label(FailLabel),
                "handle the failure of the table search")
        ]) ++ FailCode
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

generate_string_binary_switch(Cases, VarRval, VarName, CodeModel, CanFail,
        SwitchGoalInfo, EndLabel, MaybeEnd, Code, !CI, CLD) :-
    init_string_binary_switch_info(CanFail, BinarySwitchInfo, !CI, CLD),
    BranchStart = BinarySwitchInfo ^ sbsi_branch_start,
    Params = represent_params(VarName, SwitchGoalInfo, CodeModel, BranchStart,
        EndLabel),
    CommentCode = singleton(
        llds_instr(comment("string binary jump switch"), "")
    ),

    % Compute and generate the binary search table.
    map.init(CaseLabelMap0),
    switch_util.string_binary_cases(Cases,
        represent_tagged_case_for_llds(Params),
        CaseLabelMap0, CaseLabelMap, no, MaybeEnd, !CI, SortedTable),

    gen_string_binary_jump_slots(SortedTable, [], RevTableRows, [], RevTargets,
        0, TableSize),
    list.reverse(RevTableRows, TableRows),
    list.reverse(RevTargets, Targets),
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
        llds_instr(computed_goto(
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
    map.foldl(add_remaining_case, CaseLabelMap, empty, CasesCode),
    EndLabelCode = singleton(
        llds_instr(label(EndLabel), "end of binary string switch")
    ),

    Code = CommentCode ++ BinarySearchCode ++ ComputedGotoCode ++
        CasesCode ++ EndLabelCode.

:- pred gen_string_binary_jump_slots(assoc_list(string, label)::in,
    list(list(rval))::in, list(list(rval))::out,
    list(maybe(label))::in, list(maybe(label))::out,
    int::in, int::out) is det.

gen_string_binary_jump_slots([], !RevTableRows, !RevTargets, !CurIndex).
gen_string_binary_jump_slots([Str - Label | StrLabels],
        !RevTableRows, !RevTargets, !CurIndex) :-
    Row = [const(llconst_string(Str)), const(llconst_int(!.CurIndex))],
    !:RevTableRows = [Row | !.RevTableRows],
    !:RevTargets = [yes(Label) | !.RevTargets],
    !:CurIndex = !.CurIndex + 1,
    gen_string_binary_jump_slots(StrLabels,
        !RevTableRows, !RevTargets, !CurIndex).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

generate_string_binary_lookup_switch(VarRval, LookupSwitchInfo,
        CanFail, EndLabel, StoreMap, !MaybeEnd, Code, !CI, CLD) :-
    LookupSwitchInfo = lookup_switch_info(CaseConsts, OutVars, OutTypes,
        Liveness),
    (
        CaseConsts = all_one_soln(CaseValueMap),
        map.to_assoc_list(CaseValueMap, CaseValues),
        generate_string_binary_simple_lookup_switch(VarRval, CaseValues,
            OutVars, OutTypes, Liveness, CanFail, EndLabel, StoreMap,
            !MaybeEnd, Code, !CI, CLD)
    ;
        CaseConsts = some_several_solns(CaseSolnMap,
            case_consts_several_llds(ResumeVars, GoalsMayModifyTrail)),
        map.to_assoc_list(CaseSolnMap, CaseSolns),
        generate_string_binary_several_soln_lookup_switch(VarRval, CaseSolns,
            ResumeVars, GoalsMayModifyTrail, OutVars, OutTypes, Liveness,
            CanFail, EndLabel, StoreMap, !MaybeEnd, Code, !CI, CLD)
    ).

%-----------------------------------------------------------------------------%

:- pred generate_string_binary_simple_lookup_switch(rval::in,
    assoc_list(string, list(rval))::in, list(prog_var)::in,
    list(llds_type)::in, set_of_progvar::in,
    can_fail::in, label::in, abs_store_map::in,
    branch_end::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_string_binary_simple_lookup_switch(VarRval, CaseValues,
        OutVars, OutTypes, Liveness, CanFail, EndLabel, StoreMap,
        !MaybeEnd, Code, !CI, !.CLD) :-
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
    % For the LLDS backend, array indexing ops don't need the element
    % types, so it is ok to lie here.
    list.duplicate(NumOutVars, scalar_elem_generic, OutElemTypes),
    ArrayElemTypes = [scalar_elem_string | OutElemTypes],
    ArrayElemType = array_elem_struct(ArrayElemTypes),

    % Generate the static lookup table for this switch.
    construct_string_binary_simple_lookup_vector(CaseValues,
        [], RevVectorRvals),
    list.reverse(RevVectorRvals, VectorRvals),
    RowElemTypes = [lt_string | OutTypes],
    add_vector_static_cell(RowElemTypes, VectorRvals, VectorAddr, !CI),
    VectorAddrRval = const(llconst_data_addr(VectorAddr, no)),

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
        % in its slot in the table row MidReg. Most of the change is done
        % by generate_offset_assigns associating each var with the relevant
        % field in !CLD.
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
        generate_offset_assigns(OutVars, 1, BaseReg, !.CI, !CLD)
    ),

    generate_string_binary_switch_search(BinarySwitchInfo,
        VarRval, VectorAddrRval, ArrayElemType, TableSize, NumColumns,
        BinarySearchCode),

    % We keep track of what variables are supposed to be live at the end
    % of cases. We have to do this explicitly because generating a `fail'
    % slot last would yield the wrong liveness.
    set_liveness_and_end_branch(StoreMap, Liveness, no, _MaybeEnd,
        BranchEndCode, !.CLD),

    EndLabelCode = singleton(
        llds_instr(label(EndLabel), "end of binary string switch")
    ),

    Code = CommentCode ++ BinarySearchCode ++ SetBaseRegCode ++
        BranchEndCode ++ EndLabelCode.

:- pred construct_string_binary_simple_lookup_vector(
    assoc_list(string, list(rval))::in,
    list(list(rval))::in, list(list(rval))::out) is det.

construct_string_binary_simple_lookup_vector([], !RevRows).
construct_string_binary_simple_lookup_vector([Str - OutRvals | Rest],
        !RevRows) :-
    RowRvals = [const(llconst_string(Str)) | OutRvals],
    !:RevRows = [RowRvals | !.RevRows],
    construct_string_binary_simple_lookup_vector(Rest, !RevRows).

%-----------------------------------------------------------------------------%

:- pred generate_string_binary_several_soln_lookup_switch(rval::in,
    assoc_list(string, soln_consts(rval))::in, set_of_progvar::in, bool::in,
    list(prog_var)::in, list(llds_type)::in, set_of_progvar::in,
    can_fail::in, label::in, abs_store_map::in,
    branch_end::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_string_binary_several_soln_lookup_switch(VarRval, CaseSolns,
        ResumeVars, GoalsMayModifyTrail, OutVars, OutTypes, Liveness,
        CanFail, EndLabel, StoreMap, !MaybeEnd, Code, !CI, !.CLD) :-
    % This predicate, generate_string_binary_simple_lookup_switch,
    % and generate_string_binary_lookup_switch do similar tasks using
    % similar code, so if you need to update one, you probably need to
    % update them all.

    init_string_binary_switch_info(CanFail, BinarySwitchInfo, !CI, !.CLD),
    CommentCode = singleton(
        llds_instr(comment("string binary several soln lookup switch"), "")
    ),

    list.length(CaseSolns, MainTableSize),
    list.length(OutVars, NumOutVars),
    % For the LLDS backend, array indexing ops don't need the element types,
    % so it is ok to lie here.
    list.duplicate(NumOutVars, scalar_elem_generic, OutElemTypes),
    ArrayElemTypes = [scalar_elem_string, scalar_elem_int, scalar_elem_int
        | OutElemTypes],
    ArrayElemType = array_elem_struct(ArrayElemTypes),

    % If there are no output variables, then how can the individual solutions
    % differ from each other?
    expect(negate(unify(OutVars, [])), $pred, "no OutVars"),
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
        NumOutVars, [], RevMainRows,
        InitLaterSolnRowNumber, LaterSolnArrayCord0, LaterSolnArrayCord,
        0, OneSolnCaseCount, 0, SeveralSolnsCaseCount),
    list.reverse(RevMainRows, MainRows),
    LaterSolnArray = cord.list(LaterSolnArrayCord),

    list.sort([OneSolnCaseCount - kind_one_soln,
        SeveralSolnsCaseCount - kind_several_solns],
        AscendingSortedCountKinds),
    list.reverse(AscendingSortedCountKinds, DescendingSortedCountKinds),
    assoc_list.values(DescendingSortedCountKinds, DescendingSortedKinds),

    MainRowTypes = [lt_string, lt_int(int_type_int),
        lt_int(int_type_int) | OutTypes],
    list.length(MainRowTypes, MainNumColumns),
    add_vector_static_cell(MainRowTypes, MainRows, MainVectorAddr, !CI),
    MainVectorAddrRval = const(llconst_data_addr(MainVectorAddr, no)),
    add_vector_static_cell(OutTypes, LaterSolnArray, LaterVectorAddr, !CI),
    LaterVectorAddrRval = const(llconst_data_addr(LaterVectorAddr, no)),

    % Since we release BaseReg only after the calls to generate_branch_end,
    % we must make sure that generate_branch_end won't want to overwrite
    % BaseReg.
    acquire_reg_not_in_storemap(StoreMap, reg_r, BaseReg, !CLD),
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
    generate_string_binary_switch_search(BinarySwitchInfo,
        VarRval, MainVectorAddrRval, ArrayElemType,
        MainTableSize, MainNumColumns, BinarySearchCode),

    generate_code_for_all_kinds(DescendingSortedKinds, 1, OutVars, ResumeVars,
        EndLabel, StoreMap, Liveness, AddTrailOps, BaseReg,
        LaterVectorAddrRval, !MaybeEnd, LookupResultsCode, !CI, !.CLD),
    EndLabelCode = singleton(
        llds_instr(label(EndLabel),
            "end of string binary several solns switch")
    ),
    Code = CommentCode ++ BinarySearchCode ++ SetBaseRegCode ++
        LookupResultsCode ++ EndLabelCode.

:- pred construct_string_binary_several_soln_lookup_vector(
    assoc_list(string, soln_consts(rval))::in, int::in,
    list(list(rval))::in, list(list(rval))::out,
    int::in, cord(list(rval))::in, cord(list(rval))::out,
    int::in, int::out, int::in, int::out) is det.

construct_string_binary_several_soln_lookup_vector([],
        _NumOutVars, !RevMainRows, _LaterNextRow, !LaterSolnArray,
        !OneSolnCaseCount, !SeveralSolnCaseCount).
construct_string_binary_several_soln_lookup_vector([Str - Soln | StrSolns],
        NumOutVars, !RevMainRows, !.LaterNextRow, !LaterSolnArray,
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
    !:RevMainRows = [MainRow | !.RevMainRows],
    construct_string_binary_several_soln_lookup_vector(StrSolns, NumOutVars,
        !RevMainRows, !.LaterNextRow, !LaterSolnArray,
        !OneSolnCaseCount, !SeveralSolnsCaseCount).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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
    % We get the registers we use as working storage in the hash table lookup
    % code now, before we generate the code of the switch arms, since the set
    % of free registers will in general be different before and after that
    % action. However, it is safe to release them immediately, even though
    % we haven't yet generated all the code which uses them, because that
    % code will *only* be executed before the code for the cases, and because
    % that code is generated manually below. Releasing the registers early
    % allows the code of the cases to make use of them.
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
    Code = from_list([
        llds_instr(assign(LoReg, const(llconst_int(0))), ""),
        llds_instr(assign(HiReg, const(llconst_int(MaxIndex))), ""),
        llds_instr(label(LoopStartLabel),
            "begin table search loop, nofulljump"),
        llds_instr(if_val(binop(
            int_gt(int_type_int), lval(LoReg), lval(HiReg)),
            code_label(FailLabel)),
            "have we searched all of the table?"),
        llds_instr(assign(MidReg,
            binop(int_div(int_type_int),
                binop(int_add(int_type_int), lval(LoReg), lval(HiReg)),
                const(llconst_int(2)))), ""),
        llds_instr(assign(ResultReg,
            binop(str_cmp,
                VarRval,
                binop(array_index(ArrayElemType),
                    TableAddrRval,
                        binop(int_mul(int_type_int),
                            lval(MidReg),
                            const(llconst_int(NumColumns)))))),
            "compare with the middle element"),

        llds_instr(if_val(
            binop(int_ge(int_type_int), lval(ResultReg),
                const(llconst_int(0))),
            code_label(GtEqLabel)),
            "branch away unless key is in lo half"),
        llds_instr(assign(HiReg,
            binop(int_sub(int_type_int), lval(MidReg),
                const(llconst_int(1)))),
            ""),
        llds_instr(goto(code_label(LoopStartLabel)),
            "go back to search the remaining lo half"),
        llds_instr(label(GtEqLabel), "nofulljump"),

        llds_instr(if_val(
            binop(int_le(int_type_int), lval(ResultReg),
                const(llconst_int(0))),
            code_label(EqLabel)),
            "branch away unless key is in hi half"),
        llds_instr(assign(LoReg,
            binop(int_add(int_type_int), lval(MidReg), const(llconst_int(1)))),
            ""),
        llds_instr(goto(code_label(LoopStartLabel)),
            "go back to search the remaining hi half"),
        llds_instr(label(FailLabel),
            "handle the failure of the table search")
    ]) ++
    FailCode ++
    singleton(
        llds_instr(label(EqLabel), "we found the key")
    ).

%-----------------------------------------------------------------------------%

:- pred generate_string_switch_fail(can_fail::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_string_switch_fail(CanFail, FailCode, !CI, !.CLD) :-
    (
        CanFail = can_fail,
        generate_failure(FailCode, !CI, !.CLD)
    ;
        CanFail = cannot_fail,
        FailCode = singleton(
            llds_instr(comment("unreachable"),
                "fail code in cannot_fail switch")
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.string_switch.
%-----------------------------------------------------------------------------%
