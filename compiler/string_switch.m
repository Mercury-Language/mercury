%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2007, 2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: string_switch.m.
% Author: fjh.
%
% For switches on strings, we generate a hash table using open addressing
% to resolve hash conflicts.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.string_switch.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.
:- import_module parse_tree.prog_data.

:- import_module list.

:- pred generate_string_switch(list(tagged_case)::in, rval::in, string::in,
    code_model::in, can_fail::in, hlds_goal_info::in, label::in,
    branch_end::in, branch_end::out, llds_code::out,
    code_info::in, code_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.switch_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module libs.compiler_util.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.switch_case.
:- import_module ll_backend.trace_gen.

:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

generate_string_switch(Cases, VarRval, VarName, CodeModel, _CanFail,
        SwitchGoalInfo, EndLabel, !MaybeEnd, Code, !CI) :-
    % We get the registers we use as working storage in the hash table lookup
    % code now, before we generate the code of the switch arms, since the set
    % of free registers will in general be different before and after that
    % action. However, it is safe to release them immediately, even though
    % we haven't yet generated all the code which uses them, because that
    % code will *only* be executed before the code for the cases, and because
    % that code is generated manually below. Releasing the registers early
    % allows the code of the cases to make use of them.

    acquire_reg(reg_r, SlotReg, !CI),
    acquire_reg(reg_r, StringReg, !CI),
    release_reg(SlotReg, !CI),
    release_reg(StringReg, !CI),

    get_next_label(LoopLabel, !CI),
    get_next_label(FailLabel, !CI),
    get_next_label(JumpLabel, !CI),

    % Determine how big to make the hash table. Currently we round the number
    % of cases up to the nearest power of two, and then double it.
    % This should hopefully ensure that we don't get too many hash collisions.

    list.length(Cases, NumCases),
    int.log2(NumCases, LogNumCases),
    int.pow(2, LogNumCases, RoundedNumCases),
    TableSize = 2 * RoundedNumCases,
    HashMask = TableSize - 1,

    remember_position(!.CI, BranchStart),
    Params = represent_params(VarName, SwitchGoalInfo, CodeModel, BranchStart,
        EndLabel),

    % Compute the hash table.
    map.init(CaseLabelMap0),
    switch_util.string_hash_cases(Cases, HashMask,
        represent_tagged_case_for_llds(Params),
        CaseLabelMap0, CaseLabelMap, !MaybeEnd, !CI, HashValsMap),
    map.to_assoc_list(HashValsMap, HashValsList),
    switch_util.calc_string_hash_slots(HashValsList, HashValsMap,
        HashSlotsMap),

     % We must generate the failure code in the context in which none of the
     % switch arms have been executed yet.
    reset_to_position(BranchStart, !CI),
    generate_failure(FailCode, !CI),

    % Generate the data structures for the hash table.
    gen_string_hash_slots(0, TableSize, HashSlotsMap, FailLabel,
        Strings, NextSlots, Targets),

    % Generate the code for the cases.
    map.foldl(add_remaining_case, CaseLabelMap, empty, CasesCode),
    EndLabelCode = singleton(
        llds_instr(label(EndLabel), "end of hashed string switch")
    ),

    % Generate the code for the hash table lookup.
    % XXX We should be using one vector cell, not two scalar cells.
    add_scalar_static_cell_natural_types(NextSlots, NextSlotsTableAddr, !CI),
    add_scalar_static_cell_natural_types(Strings, StringTableAddr, !CI),
    NextSlotsTable = const(llconst_data_addr(NextSlotsTableAddr, no)),
    StringTable = const(llconst_data_addr(StringTableAddr, no)),
    HashLookupCode = from_list([
        llds_instr(comment("hashed string switch"), ""),
        llds_instr(assign(SlotReg,
            binop(bitwise_and, unop(hash_string, VarRval),
                const(llconst_int(HashMask)))),
            "compute the hash value of the input string"),
        llds_instr(label(LoopLabel), "begin hash chain loop"),
        llds_instr(assign(StringReg,
            binop(array_index(elem_type_string),
                StringTable, lval(SlotReg))),
            "lookup the string for this hash slot"),
        llds_instr(if_val(binop(logical_and, lval(StringReg),
            binop(str_eq, lval(StringReg), VarRval)),
                code_label(JumpLabel)),
            "did we find a match?"),
        llds_instr(assign(SlotReg,
            binop(array_index(elem_type_int),
                NextSlotsTable, lval(SlotReg))),
            "not yet, so get next slot in hash chain"),
        llds_instr(
            if_val(binop(int_ge, lval(SlotReg), const(llconst_int(0))),
                code_label(LoopLabel)),
            "keep searching until we reach the end of the chain"),
        llds_instr(label(FailLabel), "no match, so fail")
    ]),

    JumpCode = from_list([
        llds_instr(label(JumpLabel), "we found a match"),
        llds_instr(computed_goto(lval(SlotReg), Targets),
            "jump to the corresponding code")
    ]),
    Code = HashLookupCode ++ FailCode ++ JumpCode ++ CasesCode ++
        EndLabelCode.

:- pred gen_string_hash_slots(int::in, int::in,
    map(int, string_hash_slot(label))::in, label::in,
    list(rval)::out, list(rval)::out, list(maybe(label))::out) is det.

gen_string_hash_slots(Slot, TableSize, HashSlotMap, FailLabel,
        Strings, NextSlots, Targets) :-
    ( Slot = TableSize ->
        Strings = [],
        NextSlots = [],
        Targets = []
    ;
        gen_string_hash_slot(Slot, HashSlotMap, FailLabel,
            String, NextSlot, Target),
        gen_string_hash_slots(Slot + 1, TableSize, HashSlotMap, FailLabel,
            TailStrings, TailNextSlots, TailTargets),
        Strings = [String | TailStrings],
        NextSlots = [NextSlot | TailNextSlots],
        Targets = [Target | TailTargets]
    ).

:- pred gen_string_hash_slot(int::in, map(int, string_hash_slot(label))::in,
    label::in, rval::out, rval::out, maybe(label)::out) is det.

gen_string_hash_slot(Slot, HashSlotMap, FailLabel,
        StringRval, NextSlotRval, Target) :-
    ( map.search(HashSlotMap, Slot, SlotInfo) ->
        SlotInfo = string_hash_slot(Next, String, CaseLabel),
        NextSlotRval = const(llconst_int(Next)),
        StringRval = const(llconst_string(String)),
        Target = yes(CaseLabel)
    ;
        StringRval = const(llconst_int(0)),
        NextSlotRval = const(llconst_int(-2)),
        Target = yes(FailLabel)
    ).

:- pred this_is_last_case(int::in, int::in,
    map(int, string_hash_slot(label))::in) is semidet.

this_is_last_case(Slot, TableSize, Table) :-
    Slot1 = Slot + 1,
    ( Slot1 >= TableSize ->
        true
    ;
        \+ map.contains(Table, Slot1),
        this_is_last_case(Slot1, TableSize, Table)
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "string_switch.m".

%-----------------------------------------------------------------------------%
