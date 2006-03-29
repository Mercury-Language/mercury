%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: switch_util.m.
% Author: fjh.

% This module defines stuff for generating switches that is shared
% between the MLDS and LLDS back-ends.

%-----------------------------------------------------------------------------%

:- module backend_libs.switch_util.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module pair.

%-----------------------------------------------------------------------------%
%
% Stuff for categorizing switches
%

% An extended_case is an HLDS case annotated with some additional info.
% The first (int) field is the priority, as computed by switch_priority/2.

:- type extended_case ---> case(int, cons_tag, cons_id, hlds_goal).
:- type cases_list == list(extended_case).

:- type switch_category
    --->    atomic_switch   % a switch on int/char/enum
    ;       string_switch
    ;       tag_switch
    ;       other_switch.

    % Convert a type category to a switch category.
:- func type_cat_to_switch_cat(type_category) = switch_category.

    % Return the priority of a constructor test.
    % A low number here indicates a high priority.
    % We prioritize the tag tests so that the cheapest
    % (most efficient) ones come first.
    %
:- func switch_priority(cons_tag) = int.

    % type_range(TypeCategory, Type, ModuleInfo, Min, Max):
    % Determine the range [Min..Max] of an atomic type.
    % Fail if the type isn't the sort of type that has a range
    % or if the type's range is too big to switch on (e.g. int).
    %
:- pred type_range(type_category::in, mer_type::in, module_info::in,
    int::out, int::out) is semidet.

%-----------------------------------------------------------------------------%
%
% Stuff for string hash switches
%

    % For a string switch, compute the hash value for each case in the list
    % of cases, and store the cases in a map from hash values to cases.
    %
:- pred string_hash_cases(cases_list::in, int::in, map(int, cases_list)::out)
    is det.

:- type hash_slot ---> hash_slot(extended_case, int).

    % calc_hash_slots(AssocList, HashMap, Map):
    %
    % For each (HashVal - Case) pair in AssocList, allocate a hash slot in Map
    % for the case. If the hash slot corresponding to HashVal is not already
    % used, then use that one. Otherwise, find the next spare slot (making sure
    % that we don't use slots which can be used for a direct match with the
    % hash value for one of the other cases), and use it instead.
    % Keep track of the hash chains as we do this.
    %
:- pred calc_hash_slots(assoc_list(int, cases_list)::in,
    map(int, cases_list)::in, map(int, hash_slot)::out) is det.

%-----------------------------------------------------------------------------%
%
% Stuff for tag switches
%

% Where is the secondary tag (if any) for this primary tag value.
:- type stag_loc    --->    none ; local ; remote.

% Map secondary tag values (-1 stands for none) to their goal.
:- type stag_goal ---> stag_goal(cons_id, hlds_goal).
:- type stag_goal_map   ==  map(int, stag_goal).
:- type stag_goal_list  ==  assoc_list(int, stag_goal).

% Map primary tag values to the set of their goals.
:- type ptag_case ---> ptag_case(stag_loc, stag_goal_map).
:- type ptag_case_map   ==  map(tag_bits, ptag_case).
:- type ptag_case_list  ==  assoc_list(tag_bits, ptag_case).

% Map primary tag values to the number of constructors sharing them.
:- type ptag_count_map  ==  map(tag_bits, pair(stag_loc, int)).
:- type ptag_count_list ==  assoc_list(tag_bits, pair(stag_loc, int)).

    % Group together all the cases that depend on the given variable
    % having the same primary tag value.
    %
:- pred group_cases_by_ptag(cases_list::in,
    ptag_case_map::in, ptag_case_map::out) is det.

    % Order the primary tags based on the number of secondary tags
    % associated with them, putting the ones with the most secondary tags
    % first. We use selection sort.
    % Note that it is not an error for a primary tag to have no case list;
    % this can happen in semidet switches, or in det switches
    % where the initial inst of the switch variable is a bound(...) inst
    % representing a subtype.
    %
:- pred order_ptags_by_count(ptag_count_list::in,
    ptag_case_map::in, ptag_case_list::out) is det.

    % order_ptags_by_value(FirstPtag, MaxPtag,
    %   PtagCaseMap0, PtagCaseList):
    % Order the primary tags based on their value, lowest value first.
    % We scan through the primary tags values from zero to maximum.
    % Note that it is not an error for a primary tag to have no case list,
    % since this can happen in semidet switches.
    %
:- pred order_ptags_by_value(int::in, int::in, ptag_case_map::in,
    ptag_case_list::out) is det.

    % Find out how many secondary tags share each primary tag
    % of the given variable.
    %
:- pred get_ptag_counts(mer_type::in, module_info::in,
    int::out, ptag_count_map::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module parse_tree.prog_type.

:- import_module char.
:- import_module int.
:- import_module string.
:- import_module svmap.

%-----------------------------------------------------------------------------%

string_hash_cases([], _, Map) :-
    map.init(Map).
string_hash_cases([Case | Cases], HashMask, Map) :-
    string_hash_cases(Cases, HashMask, Map0),
    ( Case = case(_, string_constant(String0), _, _) ->
        String = String0
    ;
        unexpected(this_file, "string_hash_cases: non-string case?")
    ),
    string.hash(String, HashVal0),
    HashVal = HashVal0 /\ HashMask,
    ( map.search(Map0, HashVal, CaseList0) ->
        map.det_update(Map0, HashVal, [Case | CaseList0], Map)
    ;
        map.det_insert(Map0, HashVal, [Case], Map)
    ).

calc_hash_slots(HashValList, HashMap, Map) :-
    calc_hash_slots_1(HashValList, HashMap, map.init, Map, 0, _).

:- pred calc_hash_slots_1(assoc_list(int, cases_list)::in,
    map(int, cases_list)::in,
    map(int, hash_slot)::in, map(int, hash_slot)::out,
    int::in, int::out) is det.

calc_hash_slots_1([], _, !Map, !LastUsed).
calc_hash_slots_1([HashVal - Cases | Rest], HashMap,
        !Map, !LastUsed) :-
    calc_hash_slots_2(Cases, HashVal, HashMap, !Map, !LastUsed),
    calc_hash_slots_1(Rest, HashMap, !Map, !LastUsed).

:- pred calc_hash_slots_2(cases_list::in, int::in,
    map(int, cases_list)::in,
    map(int, hash_slot)::in, map(int, hash_slot)::out,
    int::in, int::out) is det.

calc_hash_slots_2([], _HashVal, _HashMap, !Map, !LastUsed).
calc_hash_slots_2([Case | Cases], HashVal, HashMap, !Map, !LastUsed) :-
    calc_hash_slots_2(Cases, HashVal, HashMap, !Map, !LastUsed),
    ( map.contains(!.Map, HashVal) ->
        follow_hash_chain(!.Map, HashVal, ChainEnd),
        next_free_hash_slot(!.Map, HashMap, !LastUsed),
        map.lookup(!.Map, ChainEnd, hash_slot(PrevCase, _)),
        svmap.det_update(ChainEnd, hash_slot(PrevCase, !.LastUsed), !Map),
        svmap.det_insert(!.LastUsed, hash_slot(Case, -1), !Map)
    ;
        svmap.det_insert(HashVal, hash_slot(Case, -1), !Map)
    ).

:- pred follow_hash_chain(map(int, hash_slot)::in, int::in, int::out) is det.

follow_hash_chain(Map, Slot, LastSlot) :-
    map.lookup(Map, Slot, hash_slot(_, NextSlot)),
    (
        NextSlot >= 0,
        map.contains(Map, NextSlot)
    ->
        follow_hash_chain(Map, NextSlot, LastSlot)
    ;
        LastSlot = Slot
    ).

    % next_free_hash_slot(M, H_M, LastUsed, FreeSlot):
    %
    % Find the next available slot FreeSlot in the hash table which is not
    % already used (contained in M) and which is not going to be used a
    % primary slot (contained in H_M), starting at the slot after LastUsed.
    %
:- pred next_free_hash_slot(map(int, hash_slot)::in,
    map(int, cases_list)::in, int::in, int::out) is det.

next_free_hash_slot(Map, H_Map, LastUsed, FreeSlot) :-
    NextSlot = LastUsed + 1,
    (
        \+ map.contains(Map, NextSlot),
        \+ map.contains(H_Map, NextSlot)
    ->
        FreeSlot = NextSlot
    ;
        next_free_hash_slot(Map, H_Map, NextSlot, FreeSlot)
    ).

%-----------------------------------------------------------------------------%
%
% Stuff for categorizing switches
%

type_cat_to_switch_cat(type_cat_enum) = atomic_switch.
type_cat_to_switch_cat(type_cat_dummy) = _ :-
    % You can't have a switch without at least two arms.
    unexpected(this_file, "type_cat_to_switch_cat: dummy").
type_cat_to_switch_cat(type_cat_int) =  atomic_switch.
type_cat_to_switch_cat(type_cat_char) = atomic_switch.
type_cat_to_switch_cat(type_cat_float) = other_switch.
type_cat_to_switch_cat(type_cat_string) =  string_switch.
type_cat_to_switch_cat(type_cat_higher_order) = other_switch.
type_cat_to_switch_cat(type_cat_user_ctor) = tag_switch.
type_cat_to_switch_cat(type_cat_variable) = other_switch.
type_cat_to_switch_cat(type_cat_tuple) = other_switch.
type_cat_to_switch_cat(type_cat_void) = _ :-
    unexpected(this_file, "type_cat_to_switch_cat: void").
type_cat_to_switch_cat(type_cat_type_info) = _ :-
    unexpected(this_file, "type_cat_to_switch_cat: type_info").
type_cat_to_switch_cat(type_cat_type_ctor_info) = _ :-
    unexpected(this_file, "type_cat_to_switch_cat: type_ctor_info").
type_cat_to_switch_cat(type_cat_typeclass_info) = _ :-
    unexpected(this_file, "type_cat_to_switch_cat: typeclass_info").
type_cat_to_switch_cat(type_cat_base_typeclass_info) = _ :-
    unexpected(this_file, "type_cat_to_switch_cat: base_typeclass_info").

switch_priority(no_tag) = 0.       % should never occur
switch_priority(int_constant(_)) = 1.
switch_priority(reserved_address(_)) = 1.
switch_priority(shared_local_tag(_, _)) = 1.
switch_priority(single_functor) = 2.
switch_priority(unshared_tag(_)) = 2.
switch_priority(float_constant(_)) = 3.
switch_priority(shared_remote_tag(_, _)) = 4.
switch_priority(string_constant(_)) = 5.
switch_priority(shared_with_reserved_addresses(RAs, Tag)) =
    switch_priority(Tag) + list.length(RAs).
    % The following tags should all never occur in switches.
switch_priority(pred_closure_tag(_, _, _)) = 6.
switch_priority(type_ctor_info_constant(_, _, _)) = 6.
switch_priority(base_typeclass_info_constant(_, _, _)) = 6.
switch_priority(tabling_pointer_constant(_, _)) = 6.
switch_priority(deep_profiling_proc_layout_tag(_, _)) = 6.
switch_priority(table_io_decl_tag(_, _)) = 6.

type_range(type_cat_char, _, _, MinChar, MaxChar) :-
    % XXX the following code uses the host's character size,
    % not the target's, so it won't work if cross-compiling
    % to a machine with a different character size.
    % Note also that the code in dense_switch.m and the code
    % in lookup_switch.m assume that char.min_char_value is 0.
    char.min_char_value(MinChar),
    char.max_char_value(MaxChar).
type_range(type_cat_enum, Type, ModuleInfo, 0, MaxEnum) :-
    ( type_to_ctor_and_args(Type, TypeCtorPrime, _) ->
        TypeCtor = TypeCtorPrime
    ;
        unexpected(this_file, "dense_switch.type_range: invalid enum type?")
    ),
    module_info_get_type_table(ModuleInfo, TypeTable),
    map.lookup(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    ( ConsTable = TypeBody ^ du_type_cons_tag_values ->
        map.count(ConsTable, TypeRange),
        MaxEnum = TypeRange - 1
    ;
        unexpected(this_file, "type_range: enum type is not d.u. type?")
    ).

%-----------------------------------------------------------------------------%

get_ptag_counts(Type, ModuleInfo, MaxPrimary, PtagCountMap) :-
    ( type_to_ctor_and_args(Type, TypeCtorPrime, _) ->
        TypeCtor = TypeCtorPrime
    ;
        unexpected(this_file, "unknown type in get_ptag_counts")
    ),
    module_info_get_type_table(ModuleInfo, TypeTable),
    map.lookup(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, Body),
    ( ConsTable = Body ^ du_type_cons_tag_values ->
        map.to_assoc_list(ConsTable, ConsList),
        assoc_list.values(ConsList, TagList)
    ;
        unexpected(this_file, "non-du type in get_ptag_counts")
    ),
    map.init(PtagCountMap0),
    get_ptag_counts_2(TagList, -1, MaxPrimary, PtagCountMap0, PtagCountMap).

:- pred get_ptag_counts_2(list(cons_tag)::in, int::in, int::out,
    ptag_count_map::in, ptag_count_map::out) is det.

get_ptag_counts_2([], !Max, !PtagCountMap).
get_ptag_counts_2([ConsTag | TagList], !MaxPrimary, !PtagCountMap) :-
    (
        ( ConsTag = single_functor, Primary = 0
        ; ConsTag = unshared_tag(Primary)
        )
    ->
        int.max(Primary, !MaxPrimary),
        ( map.search(!.PtagCountMap, Primary, _) ->
            unexpected(this_file, "unshared tag is shared")
        ;
            map.det_insert(!.PtagCountMap, Primary, none - (-1),
                !:PtagCountMap)
        )
    ; ConsTag = shared_remote_tag(Primary, Secondary) ->
        int.max(Primary, !MaxPrimary),
        ( map.search(!.PtagCountMap, Primary, Target) ->
            Target = TagType - MaxSoFar,
            ( TagType = remote ->
                true
            ;
                unexpected(this_file, "remote tag is shared with non-remote")
            ),
            int.max(Secondary, MaxSoFar, Max),
            map.det_update(!.PtagCountMap, Primary, remote - Max,
                !:PtagCountMap)
        ;
            map.det_insert(!.PtagCountMap, Primary,
                remote - Secondary, !:PtagCountMap)
        )
    ; ConsTag = shared_local_tag(Primary, Secondary) ->
        int.max(Primary, !MaxPrimary),
        ( map.search(!.PtagCountMap, Primary, Target) ->
            Target = TagType - MaxSoFar,
            ( TagType = local ->
                true
            ;
                unexpected(this_file, "local tag is shared with non-local")
            ),
            int.max(Secondary, MaxSoFar, Max),
            map.det_update(!.PtagCountMap, Primary, local - Max,
                !:PtagCountMap)
        ;
            map.det_insert(!.PtagCountMap, Primary,
                local - Secondary, !:PtagCountMap)
        )
    ;
        unexpected(this_file, "non-du tag in get_ptag_counts_2")
    ),
    get_ptag_counts_2(TagList, !MaxPrimary, !PtagCountMap).

%-----------------------------------------------------------------------------%

    % Group together all the cases that depend on the given variable
    % having the same primary tag value.

group_cases_by_ptag([], !PtagCaseMap).
group_cases_by_ptag([Case0 | Cases0], !PtagCaseMap) :-
    Case0 = case(_Priority, Tag, ConsId, Goal),
    ConsIdGoal = stag_goal(ConsId, Goal),
    (
        ( Tag = single_functor, Primary = 0
        ; Tag = unshared_tag(Primary)
        )
    ->
        ( map.search(!.PtagCaseMap, Primary, _Group) ->
            unexpected(this_file, "unshared tag is shared")
        ;
            map.init(StagGoalMap0),
            map.det_insert(StagGoalMap0, -1, ConsIdGoal, StagGoalMap),
            svmap.det_insert(Primary, ptag_case(none, StagGoalMap),
                !PtagCaseMap)
        )
    ; Tag = shared_remote_tag(Primary, Secondary) ->
        ( map.search(!.PtagCaseMap, Primary, Group) ->
            Group = ptag_case(StagLoc, StagGoalMap0),
            expect(unify(StagLoc, remote), this_file,
                "remote tag is shared with non-remote"),
            map.det_insert(StagGoalMap0, Secondary, ConsIdGoal, StagGoalMap),
            svmap.det_update(Primary, ptag_case(remote, StagGoalMap),
                !PtagCaseMap)
        ;
            map.init(StagGoalMap0),
            map.det_insert(StagGoalMap0, Secondary, ConsIdGoal, StagGoalMap),
            svmap.det_insert(Primary, ptag_case(remote, StagGoalMap),
                !PtagCaseMap)
        )
    ; Tag = shared_local_tag(Primary, Secondary) ->
        ( map.search(!.PtagCaseMap, Primary, Group) ->
            Group = ptag_case(StagLoc, StagGoalMap0),
            expect(unify(StagLoc, local), this_file,
                "local tag is shared with non-local"),
            map.det_insert(StagGoalMap0, Secondary, ConsIdGoal, StagGoalMap),
            svmap.det_update(Primary, ptag_case(local, StagGoalMap),
                !PtagCaseMap)
        ;
            map.init(StagGoalMap0),
            map.det_insert(StagGoalMap0, Secondary, ConsIdGoal, StagGoalMap),
            svmap.det_insert(Primary, ptag_case(local, StagGoalMap),
                !PtagCaseMap)
        )
    ;
        unexpected(this_file, "non-du tag in group_cases_by_ptag")
    ),
    group_cases_by_ptag(Cases0, !PtagCaseMap).

%-----------------------------------------------------------------------------%

    % Order the primary tags based on the number of secondary tags
    % associated with them, putting the ones with the most secondary tags
    % first.
    % Note that it is not an error for a primary tag to have no case list;
    % this can happen in semidet switches, or in det switches
    % where the initial inst of the switch variable is a bound(...) inst
    % representing a subtype.
    %
    % We use selection sort.

order_ptags_by_count(PtagCountList0, PtagCaseMap0, PtagCaseList) :-
    ( select_frequent_ptag(PtagCountList0, Primary, _, PtagCountList1) ->
        ( map.search(PtagCaseMap0, Primary, PtagCase) ->
            map.delete(PtagCaseMap0, Primary, PtagCaseMap1),
            order_ptags_by_count(PtagCountList1, PtagCaseMap1, PtagCaseList1),
            PtagCaseList = [Primary - PtagCase | PtagCaseList1]
        ;
            order_ptags_by_count(PtagCountList1, PtagCaseMap0, PtagCaseList)
        )
    ;
        ( map.is_empty(PtagCaseMap0) ->
            PtagCaseList = []
        ;
            unexpected(this_file,
                "PtagCaseMap0 is not empty in order_ptags_by_count")
        )
    ).

    % Select the most frequently used primary tag based on the number of
    % secondary tags associated with it.
    %
:- pred select_frequent_ptag(ptag_count_list::in, tag_bits::out,
    int::out, ptag_count_list::out) is semidet.

select_frequent_ptag([PtagCount0 | PtagCountList1], Primary,
        Count, PtagCountList) :-
    PtagCount0 = Primary0 - (_ - Count0),
    (
        select_frequent_ptag(PtagCountList1, Primary1, Count1, PtagCountList2),
        Count1 > Count0
    ->
        Primary = Primary1,
        Count = Count1,
        PtagCountList = [PtagCount0 | PtagCountList2]
    ;
        Primary = Primary0,
        Count = Count0,
        PtagCountList = PtagCountList1
    ).

%-----------------------------------------------------------------------------%

    % Order the primary tags based on their value, lowest value first.
    % We scan through the primary tags values from zero to maximum.
    % Note that it is not an error for a primary tag to have no case list,
    % since this can happen in semidet switches.

order_ptags_by_value(Ptag, MaxPtag, PtagCaseMap0, PtagCaseList) :-
    ( MaxPtag >= Ptag ->
        NextPtag = Ptag + 1,
        ( map.search(PtagCaseMap0, Ptag, PtagCase) ->
            map.delete(PtagCaseMap0, Ptag, PtagCaseMap1),
            order_ptags_by_value(NextPtag, MaxPtag,
                PtagCaseMap1, PtagCaseList1),
            PtagCaseList = [Ptag - PtagCase | PtagCaseList1]
        ;
            order_ptags_by_value(NextPtag, MaxPtag, PtagCaseMap0, PtagCaseList)
        )
    ;
        ( map.is_empty(PtagCaseMap0) ->
            PtagCaseList = []
        ;
            unexpected(this_file,
                "PtagCaseMap0 is not empty in order_ptags_by_value")
        )
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "switch_util.m".

%-----------------------------------------------------------------------------%
