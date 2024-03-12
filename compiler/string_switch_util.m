%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: switch_util.m.
% Authors: fjh, zs.
%
% This module defines stuff for generating switches on strings
% that is shared between the MLDS and LLDS back-ends.
%
%---------------------------------------------------------------------------%

:- module backend_libs.string_switch_util.
:- interface.

:- import_module backend_libs.builtin_ops.
:- import_module hlds.
:- import_module hlds.hlds_goal.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module pair.

%---------------------------------------------------------------------------%
%
% Stuff for string hash switches.
%

:- type string_hash_slot(CaseRep)
    --->    string_hash_slot(string, int, CaseRep).

:- type table_size_upgrade
    --->    keep_first_size
    ;       allow_doubling.

    % construct_string_hash_cases(StrsData, AllowDouble,
    %   TableSize, HashMap, HashOp, NumCollisions):
    %
    % For a string switch, compute the hash value for each string in the
    % arms, and store the results as a map from hash values to case
    % representations.
    %
:- pred construct_string_hash_cases(assoc_list(string, CaseRep)::in,
    table_size_upgrade::in, int::out, map(int, string_hash_slot(CaseRep))::out,
    unary_op::out, int::out) is det.

%---------------------------------------------------------------------------%
%
% Stuff for string binary switches.
%

    % Given a list of cases, represent each case using the supplied predicate,
    % map each string to the representation of its corresponding case,
    % and return a sorted assoc_list version of that map.
    %
:- pred string_binary_cases(list(tagged_case)::in,
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB,
        StateC, StateC, StateD, StateD)
        ::in(pred(in, out, in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out,
    StateC::in, StateC::out, StateD::in, StateD::out,
    assoc_list(string, CaseRep)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module int.
:- import_module io.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Stuff for string hash switches.
%

construct_string_hash_cases(StrsDatas, Upgrade, TableSize,
        HashSlotsMap, HashOp, NumCollisions) :-
    % Determine how big to make the hash table. Currently we round the number
    % of strings up to the nearest power of two, and then double it.
    % If this yields a hash table without collisions, fine.
    % Otherwise, if our caller allows us, we see whether we can avoid
    % collisions if we double the table size again.

    list.length(StrsDatas, NumStrs),
    int.log2(NumStrs, LogNumStrs),
    int.pow(2, LogNumStrs, RoundedUpNumStrs),

    TableSizeA = 2 * RoundedUpNumStrs,
    % With this tablesize, the hash table load factor will be
    % between 0.25 and 0.5.
    HashMaskA = TableSizeA - 1,
    string_hash_cases(StrsDatas, HashMaskA,
        map.init, HashValsMap4A, map.init, HashValsMap5A,
        map.init, HashValsMap6A,
        0, NumCollisions4A, 0, NumCollisions5A, 0, NumCollisions6A),
    trace [compiletime(flag("hashcollisions")), io(!IO)] (
        io.stderr_stream(StdErr, !IO),
        io.format(StdErr, "string hash collisions A: %d %d %d\n",
            [i(NumCollisions4A), i(NumCollisions5A), i(NumCollisions6A)], !IO)
    ),
    ( if
        NumCollisions4A =< NumCollisions5A,
        NumCollisions4A =< NumCollisions6A
    then
        HashValsMapA = HashValsMap4A,
        HashOpA = hash_string4,
        NumCollisionsA = NumCollisions4A
    else if
        NumCollisions5A =< NumCollisions6A
    then
        HashValsMapA = HashValsMap5A,
        HashOpA = hash_string5,
        NumCollisionsA = NumCollisions5A
    else
        HashValsMapA = HashValsMap6A,
        HashOpA = hash_string6,
        NumCollisionsA = NumCollisions6A
    ),

    ( if
        ( NumCollisionsA = 0
        ; Upgrade = keep_first_size
        )
    then
        TableSize = TableSizeA,
        HashValsMap = HashValsMapA,
        HashOp = HashOpA,
        NumCollisions = NumCollisionsA
    else
        TableSizeB = 4 * RoundedUpNumStrs,
        % With this tablesize, the hash table load factor will be
        % between 0.125 and 0.25.
        HashMaskB = TableSizeB - 1,
        string_hash_cases(StrsDatas, HashMaskB,
            map.init, HashValsMap4B, map.init, HashValsMap5B,
            map.init, HashValsMap6B,
            0, NumCollisions4B, 0, NumCollisions5B, 0, NumCollisions6B),
        trace [compiletime(flag("hashcollisions")), io(!IO)] (
            io.stderr_stream(StdErr, !IO),
            io.format(StdErr, "string hash collisions B: %d %d %d\n",
                [i(NumCollisions4B), i(NumCollisions5B), i(NumCollisions6B)],
                !IO)
        ),
        ( if NumCollisions4B = 0 then
            TableSize = TableSizeB,
            HashValsMap = HashValsMap4B,
            HashOp = hash_string4,
            NumCollisions = NumCollisions4B
        else if NumCollisions5B = 0 then
            TableSize = TableSizeB,
            HashValsMap = HashValsMap5B,
            HashOp = hash_string5,
            NumCollisions = NumCollisions5B
        else if NumCollisions6B = 0 then
            TableSize = TableSizeB,
            HashValsMap = HashValsMap6B,
            HashOp = hash_string6,
            NumCollisions = NumCollisions6B
        else
            TableSize = TableSizeA,
            HashValsMap = HashValsMapA,
            HashOp = HashOpA,
            NumCollisions = NumCollisionsA
        ),
        trace [compiletime(flag("hashcollisions")), io(!IO)] (
            io.stderr_stream(StdErr, !IO),
            ( if NumCollisions = 0, NumCollisionsA > 0 then
                io.write_string(StdErr, "string hash IMPROVEMENT\n", !IO)
            else
                io.write_string(StdErr, "string hash NO IMPROVEMENT\n", !IO)
            )
        )
    ),
    map.to_assoc_list(HashValsMap, HashValsList),
    calc_string_hash_slots(TableSize, HashValsList, HashValsMap, HashSlotsMap).

%---------------------------------------------------------------------------%

:- pred string_hash_cases(assoc_list(string, CaseRep)::in, int::in,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::out,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::out,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::out,
    int::in, int::out, int::in, int::out, int::in, int::out) is det.

string_hash_cases([], _, !HashMap4, !HashMap5, !HashMap6,
        !NumCollisions4, !NumCollisions5, !NumCollisions6).
string_hash_cases([StrData | StrsDatas], HashMask,
        !HashMap4, !HashMap5, !HashMap6,
        !NumCollisions4, !NumCollisions5, !NumCollisions6) :-
    string_hash_case(StrData, HashMask,
        !HashMap4, !HashMap5, !HashMap6,
        !NumCollisions4, !NumCollisions5, !NumCollisions6),
    string_hash_cases(StrsDatas, HashMask,
        !HashMap4, !HashMap5, !HashMap6,
        !NumCollisions4, !NumCollisions5, !NumCollisions6).

:- pred string_hash_case(pair(string, CaseRep)::in, int::in,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::out,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::out,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::out,
    int::in, int::out, int::in, int::out, int::in, int::out) is det.

string_hash_case(StrCaseRep, HashMask,
        !HashMap4, !HashMap5, !HashMap6,
        !NumCollisions4, !NumCollisions5, !NumCollisions6) :-
    StrCaseRep = String - _CaseRep,
    HashVal4 = string.hash4(String) /\ HashMask,
    HashVal5 = string.hash5(String) /\ HashMask,
    HashVal6 = string.hash6(String) /\ HashMask,
    ( if map.search(!.HashMap4, HashVal4, OldEntries4) then
        map.det_update(HashVal4, [StrCaseRep | OldEntries4], !HashMap4),
        !:NumCollisions4 = !.NumCollisions4 + 1
    else
        map.det_insert(HashVal4, [StrCaseRep], !HashMap4)
    ),
    ( if map.search(!.HashMap5, HashVal5, OldEntries5) then
        map.det_update(HashVal5, [StrCaseRep | OldEntries5], !HashMap5),
        !:NumCollisions5 = !.NumCollisions5 + 1
    else
        map.det_insert(HashVal5, [StrCaseRep], !HashMap5)
    ),
    ( if map.search(!.HashMap6, HashVal6, OldEntries6) then
        map.det_update(HashVal6, [StrCaseRep | OldEntries6], !HashMap6),
        !:NumCollisions6 = !.NumCollisions6 + 1
    else
        map.det_insert(HashVal6, [StrCaseRep], !HashMap6)
    ).

%---------------------------------------------------------------------------%

    % calc_string_hash_slots(AssocList, HashMap, Map):
    %
    % For each (HashVal - Case) pair in AssocList, allocate a hash slot in Map
    % for the case. If the hash slot corresponding to HashVal is not already
    % used, then use that one. Otherwise, find the next spare slot (making sure
    % that we don't use slots which can be used for a direct match with the
    % hash value for one of the other cases), and use it instead.
    % Keep track of the hash chains as we do this.
    %
:- pred calc_string_hash_slots(int::in,
    assoc_list(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::out) is det.

calc_string_hash_slots(TableSize, HashValList, HashMap, SlotMap) :-
    trace [compile_time(flag("hash_slots")), io(!IO)] (
        io.stderr_stream(StdErr, !IO),
        io.write_string(StdErr, "CALCULATING HASH SLOTS START\n", !IO)
    ),
    calc_string_hash_slots_loop_over_hashes(HashValList, TableSize, HashMap,
        map.init, SlotMap, 0, _),
    trace [compile_time(flag("hash_slots")), io(!IO)] (
        io.stderr_stream(StdErr, !IO),
        io.write_string(StdErr, "CALCULATING HASH SLOTS END\n", !IO)
    ).

:- pred calc_string_hash_slots_loop_over_hashes(
    assoc_list(int, assoc_list(string, CaseRep))::in, int::in,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::out,
    int::in, int::out) is det.

calc_string_hash_slots_loop_over_hashes([], _, _, !SlotMap, !LastUsed).
calc_string_hash_slots_loop_over_hashes([HashVal - StringCaseReps | Rest],
        TableSize, HashMap, !SlotMap, !LastUsed) :-
    calc_string_hash_slots_loop_over_hash_strings(StringCaseReps, TableSize,
        HashVal, HashMap, !SlotMap, !LastUsed),
    calc_string_hash_slots_loop_over_hashes(Rest, TableSize,
        HashMap, !SlotMap, !LastUsed).

:- pred calc_string_hash_slots_loop_over_hash_strings(
    assoc_list(string, CaseRep)::in, int::in, int::in,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::out,
    int::in, int::out) is det.

calc_string_hash_slots_loop_over_hash_strings([],
        _TableSize, _HashVal, _HashMap, !SlotMap, !LastUsed).
calc_string_hash_slots_loop_over_hash_strings([StringCaseRep | StringCaseReps],
        TableSize, HashVal, HashMap, !SlotMap, !LastUsed) :-
    calc_string_hash_slots_loop_over_hash_strings(StringCaseReps,
        TableSize, HashVal, HashMap, !SlotMap, !LastUsed),
    StringCaseRep = String - CaseRep,
    NewSlot = string_hash_slot(String, -1, CaseRep),
    ( if map.contains(!.SlotMap, HashVal) then
        follow_hash_chain(!.SlotMap, HashVal, ChainEnd),
        next_free_hash_slot(!.SlotMap, HashMap, TableSize, !LastUsed),
        map.lookup(!.SlotMap, ChainEnd, ChainEndSlot0),
        ChainEndSlot0 = string_hash_slot(PrevString, _, PrevCaseRep),
        ChainEndSlot = string_hash_slot(PrevString, !.LastUsed, PrevCaseRep),
        map.det_update(ChainEnd, ChainEndSlot, !SlotMap),
        map.det_insert(!.LastUsed, NewSlot, !SlotMap),
        trace [compile_time(flag("hash_slots")), io(!IO)] (
            io.stderr_stream(StdErr, !IO),
            io.format(StdErr, "%s: home %d, remapped slot %d\n",
                [s(String), i(HashVal), i(!.LastUsed)], !IO)
        )
    else
        map.det_insert(HashVal, NewSlot, !SlotMap),
        trace [compile_time(flag("hash_slots")), io(!IO)] (
            io.stderr_stream(StdErr, !IO),
            io.format(StdErr, "%s: native slot %d\n",
                [s(String), i(HashVal)], !IO)
        )
    ).

:- pred follow_hash_chain(map(int, string_hash_slot(CaseRep))::in,
    int::in, int::out) is det.

follow_hash_chain(Map, Slot, LastSlot) :-
    map.lookup(Map, Slot, string_hash_slot(_, NextSlot, _)),
    ( if
        NextSlot >= 0,
        map.contains(Map, NextSlot)
    then
        follow_hash_chain(Map, NextSlot, LastSlot)
    else
        LastSlot = Slot
    ).

    % next_free_hash_slot(M, H_M, LastUsed, FreeSlot):
    %
    % Find the next available slot FreeSlot in the hash table which is not
    % already used (contained in Map) and which is not going to be used as a
    % primary slot (contained in HomeMap), starting at the slot after LastUsed.
    %
:- pred next_free_hash_slot(map(int, string_hash_slot(CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::in, int::in, int::in, int::out)
    is det.

next_free_hash_slot(Map, HomeMap, TableSize, LastUsed, FreeSlot) :-
    NextSlot = LastUsed + 1,
    expect(NextSlot < TableSize, $pred, "overflow"),
    ( if
        ( map.contains(Map, NextSlot)
        ; map.contains(HomeMap, NextSlot)
        )
    then
        next_free_hash_slot(Map, HomeMap, TableSize, NextSlot, FreeSlot)
    else
        FreeSlot = NextSlot
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Stuff for string binary switches.
%

string_binary_cases(TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, !StateD, SortedTable) :-
    string_binary_entries(TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, !StateD, [], UnsortedTable),
    list.sort(UnsortedTable, SortedTable).

:- pred string_binary_entries(list(tagged_case)::in,
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB,
        StateC, StateC, StateD, StateD)
        ::in(pred(in, out, in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out,
    StateC::in, StateC::out, StateD::in, StateD::out,
    assoc_list(string, CaseRep)::in, assoc_list(string, CaseRep)::out) is det.

string_binary_entries([], _,
        !StateA, !StateB, !StateC, !StateD, !UnsortedTable).
string_binary_entries([TaggedCase | TaggedCases], RepresentCase,
        !StateA, !StateB, !StateC, !StateD, !UnsortedTable) :-
    string_binary_entries(TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, !StateD, !UnsortedTable),
    RepresentCase(TaggedCase, CaseRep, !StateA, !StateB, !StateC, !StateD),
    TaggedCase = tagged_case(MainTaggedConsId, OtherTaggedConsIds, _, _),
    add_string_binary_entry(CaseRep, MainTaggedConsId, !UnsortedTable),
    list.foldl(add_string_binary_entry(CaseRep), OtherTaggedConsIds,
        !UnsortedTable).

:- pred add_string_binary_entry(CaseRep::in, tagged_cons_id::in,
    assoc_list(string, CaseRep)::in, assoc_list(string, CaseRep)::out) is det.

add_string_binary_entry(CaseRep, TaggedConsId, !UnsortedTable) :-
    TaggedConsId = tagged_cons_id(_ConsId, Tag),
    ( if Tag = string_tag(StringPrime) then
        String = StringPrime
    else
        unexpected($pred, "non-string case?")
    ),
    !:UnsortedTable = [String - CaseRep | !.UnsortedTable].

%---------------------------------------------------------------------------%
:- end_module backend_libs.string_switch_util.
%---------------------------------------------------------------------------%
