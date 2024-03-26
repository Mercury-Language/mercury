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
:- import_module maybe.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%
%
% Stuff for string trie switches.
%

:- type trie_node
    --->    trie_leaf(
                leaf_matched        :: list(int),
                % The already matched code units, in reverse order.

                leaf_unmatched      :: list(int),
                % The not-yet matched code units, in forward order.
                % Invariant: applying from_code_unit_list to
                % list.reverse(leaf_matched) ++ leaf_unmatched
                % should yield the original string.

                case_id
                % The case_id of the switch arm.
            )
    ;       trie_choice(
                choice_matched      :: list(int),
                % The already matched code units, in reverse order.

                choice_next_level   :: map(int, trie_node),
                % Maps the next code unit to the trie node reachable
                % through it.

                choice_end          :: maybe(case_id)
                % The case number of the switch arm whose string ends here,
                % if there is one.
            ).

    % create_trie(Encoding, TaggedCases, MaxCaseNum, TopTrieNode):
    %
    % Given a list of tagged cases, convert it to a trie that maps
    % each string in those cases to the id of its case, with each node
    % of the trie at depth N containing a branch on the code unit at offset N
    % of the string in the given encoding (if we consider the root node
    % to be at level 0).
    %
    % Returns also the highest case_id (in its integer form).
    %
:- pred create_trie(string_encoding::in, list(tagged_case)::in,
    int::out, trie_node::out) is det.

:- inst trie_choice for trie_node/0
    --->    trie_choice(ground, ground, ground).

:- pred chase_any_stick_in_trie(trie_node::in(trie_choice),
    assoc_list(int, trie_node)::out, list(int)::out, trie_node::out) is det.

%---------------------------------------------------------------------------%
%
% Stuff for both string trie switches and string hash switches.
%

    % build_str_case_id_list(TaggedCases, MaxCaseNum, StrCaseIds):
    %
    % Convert the list of cases, each of contains one or more strings,
    % into an assoc_list that maps each of those strings to its containing
    % case's case_id. Also return the highest case_id (in its integer form).
    %
    % NOTE It would be nice to change the type of the last argument
    % to a list of values of bespoke type, but
    %
    % - the value returned here is given by some callers to
    %   construct_string_hash_cases, which accepts
    %   "assoc_list(string, case_id)" as an instance of
    %   "assoc_list(string, CaseRep)", while
    %
    % - some *other* callers of construct_string_hash_cases pass values
    %   in that slot that use *other* types as the CaseRep type.
    %
    % This means that any such bespoke type would not be worthwhile, since
    % it would be almost as general as the pair type constructor.
    %   
:- pred build_str_case_id_list(list(tagged_case)::in,
    int::out, assoc_list(string, case_id)::out) is det.

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

:- import_module backend_libs.string_encoding.
:- import_module hlds.hlds_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module cord.
:- import_module int.
:- import_module io.
:- import_module require.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Stuff for string trie switches.
%

create_trie(Encoding, TaggedCases, MaxCaseNum, TopTrieNode) :-
    build_str_case_id_list(TaggedCases, MaxCaseNum, StrsCaseIds),
    % The order of StrsCaseIds does not matter; we will build the same trie
    % regardless of the order.
    (
        StrsCaseIds = [],
        TopTrieNode = trie_choice([], map.init, no)
    ;
        StrsCaseIds = [HeadStrCaseId | TailStrCaseIds],
        HeadStrCaseId = HeadStr - HeadCaseId,
        to_code_unit_list_in_encoding(Encoding, HeadStr, HeadStrCodeUnits),
        TopTrieNode1 = trie_leaf([], HeadStrCodeUnits, HeadCaseId),
        insert_cases_into_trie(Encoding, TailStrCaseIds, TopTrieNode1,
            TopTrieNode)
    ).

:- pred insert_cases_into_trie(string_encoding::in,
    assoc_list(string, case_id)::in, trie_node::in, trie_node::out) is det.

insert_cases_into_trie(_Encoding, [], !TrieNode).
insert_cases_into_trie(Encoding, [Case | Cases], !TrieNode) :-
    Case = Str - CaseId,
    to_code_unit_list_in_encoding(Encoding, Str, StrCodeUnits),
    insert_case_into_trie_node([], StrCodeUnits, CaseId, !TrieNode),
    insert_cases_into_trie(Encoding, Cases, !TrieNode).

:- pred insert_case_into_trie_node(list(int)::in, list(int)::in, case_id::in,
    trie_node::in, trie_node::out) is det.

insert_case_into_trie_node(InsertMatched, InsertNotYetMatched, InsertCaseId,
        TrieNode0, TrieNode) :-
    (
        TrieNode0 = trie_leaf(LeafMatched, LeafNotYetMatched, LeafCaseId),
        expect(unify(LeafMatched, InsertMatched), $pred, "LeafMatched didn't"),
        (
            LeafNotYetMatched = [],
            ChoiceMap0 = map.init,
            MaybeEnd0 = yes(LeafCaseId)
        ;
            LeafNotYetMatched = [LeafFirstCodeUnit | LeafLaterCodeUnits],
            NewLeaf = trie_leaf([LeafFirstCodeUnit | LeafMatched],
                LeafLaterCodeUnits, LeafCaseId),
            ChoiceMap0 = map.singleton(LeafFirstCodeUnit, NewLeaf),
            MaybeEnd0 = no
        )
    ;
        TrieNode0 = trie_choice(ChoiceMatched, ChoiceMap0, MaybeEnd0),
        expect(unify(ChoiceMatched, InsertMatched), $pred,
            "ChoiceMatched didn't")
    ),
    insert_case_into_trie_choice(InsertMatched, InsertNotYetMatched,
        InsertCaseId, ChoiceMap0, ChoiceMap, MaybeEnd0, MaybeEnd),
    TrieNode = trie_choice(InsertMatched, ChoiceMap, MaybeEnd).

:- pred insert_case_into_trie_choice(list(int)::in, list(int)::in, case_id::in,
    map(int, trie_node)::in, map(int, trie_node)::out,
    maybe(case_id)::in, maybe(case_id)::out) is det.

insert_case_into_trie_choice(InsertMatched, InsertNotYetMatched, InsertCaseId,
        ChoiceMap0, ChoiceMap, MaybeEnd0, MaybeEnd) :-
    (
        InsertNotYetMatched = [],
        ChoiceMap = ChoiceMap0,
        (
            MaybeEnd0 = no,
            MaybeEnd = yes(InsertCaseId)
        ;
            MaybeEnd0 = yes(_),
            % You can't have more than one occurrence of a string
            % as a cons_id in a switch.
            unexpected($pred, "two strings end at same trie node")
        )
    ;
        InsertNotYetMatched = [InsertFirstCodeUnit | InsertLaterCodeUnits],
        MaybeEnd = MaybeEnd0,
        ( if map.search(ChoiceMap0, InsertFirstCodeUnit, SubTrieNode0) then
            insert_case_into_trie_node([InsertFirstCodeUnit | InsertMatched],
                InsertLaterCodeUnits, InsertCaseId, SubTrieNode0, SubTrieNode),
            map.det_update(InsertFirstCodeUnit, SubTrieNode,
                ChoiceMap0, ChoiceMap)
        else
            SubTrieNode = trie_leaf([InsertFirstCodeUnit | InsertMatched],
                InsertLaterCodeUnits, InsertCaseId),
            map.det_insert(InsertFirstCodeUnit, SubTrieNode,
                ChoiceMap0, ChoiceMap)
        )
    ).

%---------------------------------------------------------------------------%

chase_any_stick_in_trie(TrieNode, ChoicePairs,
        StickCodeUnits, TrieNodeAfterStick) :-
    TrieNode = trie_choice(_, ChoiceMap, MaybeEnd),
    map.to_assoc_list(ChoiceMap, ChoicePairs),
    ( if
        ChoicePairs = [OneChoicePair],
        MaybeEnd = no
    then
        OneChoicePair = OneCodeUnit - OneSubTrieNode,
        (
            OneSubTrieNode = trie_leaf(_, _, _),
            StickCodeUnits = [],
            TrieNodeAfterStick = TrieNode
        ;
            OneSubTrieNode = trie_choice(_, _, _),
            chase_any_stick_in_trie(OneSubTrieNode, _SubChoicePairs,
                SubStickCodeUnits, TrieNodeAfterStick),
            StickCodeUnits = [OneCodeUnit | SubStickCodeUnits]
        )
    else
        StickCodeUnits = [],
        TrieNodeAfterStick = TrieNode
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Stuff for both string trie switches and string hash switches.
%

build_str_case_id_list(TaggedCases, MaxCaseNum, StrCaseIds) :-
    build_str_case_id_cord(TaggedCases, -1, MaxCaseNum,
        cord.init, StrCaseIdCord),
    StrCaseIds = cord.list(StrCaseIdCord).

    % Values of this type specify the identity of the case that applies
    % to a given string.
:- type string_case_id == pair(string, case_id).

:- pred build_str_case_id_cord(list(tagged_case)::in, int::in, int::out,
    cord(string_case_id)::in, cord(string_case_id)::out) is det.

build_str_case_id_cord([], !MaxCaseNum, !RevStrsCaseIds).
build_str_case_id_cord([TaggedCase | TaggedCases],
        !MaxCaseNum, !StrCaseIdCord) :-
    TaggedCase = tagged_case(MainTaggedConsId, OtherTaggedConsIds, CaseId, _),
    CaseId = case_id(CaseNum),
    int.max(CaseNum, !MaxCaseNum),
    add_to_strs_case_ids(CaseId, MainTaggedConsId, !StrCaseIdCord),
    list.foldl(add_to_strs_case_ids(CaseId),
        OtherTaggedConsIds, !StrCaseIdCord),
    build_str_case_id_cord(TaggedCases, !MaxCaseNum, !StrCaseIdCord).

:- pred add_to_strs_case_ids(case_id::in, tagged_cons_id::in,
    cord(string_case_id)::in, cord(string_case_id)::out) is det.

add_to_strs_case_ids(CaseId, TaggedConsId, !StrCaseIdCord) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    ( if ConsTag = string_tag(String) then
        cord.snoc(String - CaseId, !StrCaseIdCord)
    else
        unexpected($pred, "non-string tag")
    ).

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
