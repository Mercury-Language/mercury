%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: switch_util.m
% Author: fjh
%
% This module defines stuff for generating switches that is shared
% between the MLDS and LLDS back-ends.
%
%-----------------------------------------------------------------------------%

:- module switch_util.
:- interface.
:- import_module prog_data, hlds_goal, hlds_data, hlds_module, type_util.
:- import_module list, assoc_list, map, std_util.

%-----------------------------------------------------------------------------%
%
% Stuff for categorizing switches
%

% An extended_case is an HLDS case annotated with some additional info.
% The first (int) field is the priority, as computed by switch_priority/2.
:- type extended_case ---> case(int, cons_tag, cons_id, hlds_goal).
:- type cases_list == list(extended_case).

:- type switch_category
	--->	atomic_switch	% a switch on int/char/enum
	;	string_switch
	;	tag_switch
	;	other_switch.

:- pred switch_util__type_cat_to_switch_cat(builtin_type, switch_category).
:- mode switch_util__type_cat_to_switch_cat(in, out) is det.

	% Return the priority of a constructor test.
	% A low number here indicates a high priority.
	% We prioritize the tag tests so that the cheapest
	% (most efficient) ones come first.
	%
:- pred switch_util__switch_priority(cons_tag, int).
:- mode switch_util__switch_priority(in, out) is det.

	% switch_util__type_range(TypeCategory, Type, ModuleInfo, Min, Max):
	% Determine the range [Min..Max] of an atomic type.
	% Fail if the type isn't the sort of type that has a range
	% or if the type's range is too big to switch on (e.g. int).
	%
:- pred switch_util__type_range(builtin_type, type, module_info, int, int).
:- mode switch_util__type_range(in, in, in, out, out) is semidet.

%-----------------------------------------------------------------------------%
%
% Stuff for string hash switches
%

	% for a string switch, compute the hash value for each case
	% in the list of cases, and store the cases in a map
	% from hash values to cases.

:- pred switch_util__string_hash_cases(cases_list, int,
		map(int, cases_list)).
:- mode switch_util__string_hash_cases(in, in, out) is det.

	% switch_util__calc_hash_slots(AssocList, HashMap, Map) :-
	%	For each (HashVal - Case) pair in AssocList,
	%	allocate a hash slot in Map for the case.
	%	If the hash slot corresponding to HashVal is not
	%	already used, then use that one.  Otherwise, find
	%	the next spare slot (making sure that we don't
	%	use slots which can be used for a direct match with
	%	the hash value for one of the other cases), and
	%	use it instead.

:- type hash_slot ---> hash_slot(extended_case, int).

:- pred switch_util__calc_hash_slots(assoc_list(int, cases_list),
	map(int, cases_list), map(int, hash_slot)).
:- mode switch_util__calc_hash_slots(in, in, out) is det.

%-----------------------------------------------------------------------------%
%
% Stuff for tag switches
%

% where is the secondary tag (if any) for this primary tag value
:- type stag_loc	--->	none ; local ; remote.

% map secondary tag values (-1 stands for none) to their goal
:- type stag_goal_map	==	map(int, hlds_goal).
:- type stag_goal_list	==	assoc_list(int, hlds_goal).

% map primary tag values to the set of their goals
:- type ptag_case_map	==	map(tag_bits, pair(stag_loc, stag_goal_map)).
:- type ptag_case_list	==	assoc_list(tag_bits,
					pair(stag_loc, stag_goal_map)).

% map primary tag values to the number of constructors sharing them
:- type ptag_count_map	==	map(tag_bits, pair(stag_loc, int)).
:- type ptag_count_list ==	assoc_list(tag_bits, pair(stag_loc, int)).

	% Group together all the cases that depend on the given variable
	% having the same primary tag value.

:- pred switch_util__group_cases_by_ptag(cases_list,
		ptag_case_map, ptag_case_map).
:- mode switch_util__group_cases_by_ptag(in, in, out) is det.

	% Order the primary tags based on the number of secondary tags
	% associated with them, putting the ones with the most secondary tags
	% first. We use selection sort.
	% Note that it is not an error for a primary tag to have no case list;
	% this can happen in semidet switches, or in det switches
	% where the initial inst of the switch variable is a bound(...) inst
	% representing a subtype.

:- pred switch_util__order_ptags_by_count(ptag_count_list, ptag_case_map,
		ptag_case_list).
:- mode switch_util__order_ptags_by_count(in, in, out) is det.

	% switch_util__order_ptags_by_value(FirstPtag, MaxPtag,
	%	PtagCaseMap0, PtagCaseList):
	% Order the primary tags based on their value, lowest value first.
	% We scan through the primary tags values from zero to maximum.
	% Note that it is not an error for a primary tag to have no case list,
	% since this can happen in semidet switches.

:- pred switch_util__order_ptags_by_value(int, int, ptag_case_map,
		ptag_case_list).
:- mode switch_util__order_ptags_by_value(in, in, in, out) is det.

	% Find out how many secondary tags share each primary tag
	% of the given variable.

:- pred switch_util__get_ptag_counts(type, module_info, int,
		ptag_count_map).
:- mode switch_util__get_ptag_counts(in, in, out, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module char, int, string, require.

%-----------------------------------------------------------------------------%

switch_util__string_hash_cases([], _, Map) :-
	map__init(Map).
switch_util__string_hash_cases([Case | Cases], HashMask, Map) :-
	switch_util__string_hash_cases(Cases, HashMask, Map0),
	( Case = case(_, string_constant(String0), _, _) ->
		String = String0
	;
		error("switch_util__string_hash_cases: non-string case?")
	),
	string__hash(String, HashVal0),
	HashVal is HashVal0 /\ HashMask,
	( map__search(Map0, HashVal, CaseList0) ->
		map__det_update(Map0, HashVal, [Case | CaseList0], Map)
	;
		map__det_insert(Map0, HashVal, [Case], Map)
	).

	% switch_util__calc_hash_slots(AssocList, HashMap, Map) :-
	%	For each (HashVal - Case) pair in AssocList,
	%	allocate a hash slot in Map for the case, as follows.
	%	If the hash slot corresponding to HashVal is not
	%	already used, then use that one.  Otherwise, find
	%	the next spare slot (making sure that we don't
	%	use slots which can be used for a direct match with
	%	the hash value for one of the other cases), and
	%	use it instead.  Keep track of the hash chains
	%	as we do this.

switch_util__calc_hash_slots(HashValList, HashMap, Map) :-
	map__init(Map0),
	switch_util__calc_hash_slots_1(HashValList, HashMap, Map0, 0,
		Map, _).

:- pred switch_util__calc_hash_slots_1(assoc_list(int, cases_list),
	map(int, cases_list), map(int, hash_slot), int,
	map(int, hash_slot), int).
:- mode switch_util__calc_hash_slots_1(in, in, in, in, out, out) is det.

switch_util__calc_hash_slots_1([], _, Map, LastUsed, Map, LastUsed).
switch_util__calc_hash_slots_1([HashVal-Cases | Rest], HashMap, Map0,
		LastUsed0, Map, LastUsed) :-
	switch_util__calc_hash_slots_2(Cases, HashVal, HashMap, Map0,
		LastUsed0, Map1, LastUsed1),
	switch_util__calc_hash_slots_1(Rest, HashMap, Map1,
		LastUsed1, Map, LastUsed).

:- pred switch_util__calc_hash_slots_2(cases_list, int,
		map(int, cases_list), map(int, hash_slot), int,
		map(int, hash_slot), int).
:- mode switch_util__calc_hash_slots_2(in, in, in, in, in, out, out) is det.

switch_util__calc_hash_slots_2([], _HashVal, _HashMap, Map, LastUsed,
		Map, LastUsed).
switch_util__calc_hash_slots_2([Case | Cases], HashVal, HashMap, Map0,
		LastUsed0, Map, LastUsed) :-
	switch_util__calc_hash_slots_2(Cases, HashVal, HashMap, Map0,
		LastUsed0, Map1, LastUsed1),
	( map__contains(Map1, HashVal) ->
		switch_util__follow_hash_chain(Map1, HashVal, ChainEnd),
		switch_util__next_free_hash_slot(Map1, HashMap, LastUsed1,
			Next),
		map__lookup(Map1, ChainEnd, hash_slot(PrevCase, _)),
		map__det_update(Map1, ChainEnd, hash_slot(PrevCase, Next),
			Map2),
		map__det_insert(Map2, Next, hash_slot(Case, -1), Map),
		LastUsed = Next
	;
		map__det_insert(Map1, HashVal, hash_slot(Case, -1), Map),
		LastUsed = LastUsed1
	).

:- pred switch_util__follow_hash_chain(map(int, hash_slot), int, int).
:- mode switch_util__follow_hash_chain(in, in, out) is det.

switch_util__follow_hash_chain(Map, Slot, LastSlot) :-
	map__lookup(Map, Slot, hash_slot(_, NextSlot)),
	(
		NextSlot >= 0,
		map__contains(Map, NextSlot)
	->
		switch_util__follow_hash_chain(Map, NextSlot, LastSlot)
	;
		LastSlot = Slot
	).

	% next_free_hash_slot(M, H_M, LastUsed, FreeSlot) :-
	%	Find the next available slot FreeSlot in the hash table
	%	which is not already used (contained in M) and which is not
	%	going to be used a primary slot (contained in H_M),
	%	starting at the slot after LastUsed.

:- pred switch_util__next_free_hash_slot(map(int, hash_slot),
	map(int, cases_list), int, int).
:- mode switch_util__next_free_hash_slot(in, in, in, out) is det.

switch_util__next_free_hash_slot(Map, H_Map, LastUsed, FreeSlot) :-
	NextSlot is LastUsed + 1,
	(
		\+ map__contains(Map, NextSlot),
		\+ map__contains(H_Map, NextSlot)
	->
		FreeSlot = NextSlot
	;
		switch_util__next_free_hash_slot(Map, H_Map, NextSlot,
			FreeSlot)
	).

%-----------------------------------------------------------------------------%
%
% Stuff for categorizing switches
%

	% Convert a type category to a switch category
switch_util__type_cat_to_switch_cat(enum_type, atomic_switch).
switch_util__type_cat_to_switch_cat(int_type,  atomic_switch).
switch_util__type_cat_to_switch_cat(char_type, atomic_switch).
switch_util__type_cat_to_switch_cat(float_type, other_switch).
switch_util__type_cat_to_switch_cat(str_type,  string_switch).
switch_util__type_cat_to_switch_cat(pred_type, other_switch).
switch_util__type_cat_to_switch_cat(user_type, tag_switch).
switch_util__type_cat_to_switch_cat(polymorphic_type, other_switch).
switch_util__type_cat_to_switch_cat(tuple_type, other_switch).

	% Return the priority of a constructor test.
	% A low number here indicates a high priority.
	% We prioritize the tag tests so that the cheapest
	% (most efficient) ones come first.
	%
switch_util__switch_priority(no_tag, 0).		% should never occur
switch_util__switch_priority(int_constant(_), 1).
switch_util__switch_priority(reserved_address(_), 1).
switch_util__switch_priority(shared_local_tag(_, _), 1).
switch_util__switch_priority(unshared_tag(_), 2).
switch_util__switch_priority(float_constant(_), 3).
switch_util__switch_priority(shared_remote_tag(_, _), 4).
switch_util__switch_priority(string_constant(_), 5).
switch_util__switch_priority(shared_with_reserved_addresses(RAs, Tag), N) :-
	switch_util__switch_priority(Tag, N0),
	N = N0 + list__length(RAs).
	% The following tags should all never occur in switches.
switch_util__switch_priority(pred_closure_tag(_, _, _), 6).
switch_util__switch_priority(code_addr_constant(_, _), 6).
switch_util__switch_priority(type_ctor_info_constant(_, _, _), 6).
switch_util__switch_priority(base_typeclass_info_constant(_, _, _), 6).
switch_util__switch_priority(tabling_pointer_constant(_, _), 6).
switch_util__switch_priority(deep_profiling_proc_static_tag(_), 6).

	% Determine the range of an atomic type.
	% Fail if the type isn't the sort of type that has a range
	% or if the type's range is to big to switch on (e.g. int).
	%
switch_util__type_range(char_type, _, _, MinChar, MaxChar) :-
	% XXX the following code uses the host's character size,
	% not the target's, so it won't work if cross-compiling
	% to a machine with a different character size.
	% Note also that the code in dense_switch.m and the code
	% in lookup_switch.m assume that char__min_char_value is 0.
	char__min_char_value(MinChar),
	char__max_char_value(MaxChar).
switch_util__type_range(enum_type, Type, ModuleInfo, 0, MaxEnum) :-
	( type_to_type_id(Type, TypeId0, _) ->
		TypeId = TypeId0
	;
		error("dense_switch__type_range: invalid enum type?")
	),
	module_info_types(ModuleInfo, TypeTable),
	map__lookup(TypeTable, TypeId, TypeDefn),
	hlds_data__get_type_defn_body(TypeDefn, TypeBody),
	( TypeBody = du_type(_, ConsTable, _, _) ->
		map__count(ConsTable, TypeRange),
		MaxEnum = TypeRange - 1
	;
		error("dense_switch__type_range: enum type is not d.u. type?")
	).

%-----------------------------------------------------------------------------%

	% Find out how many secondary tags share each primary tag
	% of the given variable.

switch_util__get_ptag_counts(Type, ModuleInfo, MaxPrimary, PtagCountMap) :-
	( type_to_type_id(Type, TypeIdPrime, _) ->
		TypeId = TypeIdPrime
	;
		error("unknown type in switch_util__get_ptag_counts")
	),
	module_info_types(ModuleInfo, TypeTable),
	map__lookup(TypeTable, TypeId, TypeDefn),
	hlds_data__get_type_defn_body(TypeDefn, Body),
	( Body = du_type(_, ConsTable, _, _) ->
		map__to_assoc_list(ConsTable, ConsList),
		switch_util__cons_list_to_tag_list(ConsList, TagList)
	;
		error("non-du type in switch_util__get_ptag_counts")
	),
	map__init(PtagCountMap0),
	switch_util__get_ptag_counts_2(TagList, -1, MaxPrimary,
		PtagCountMap0, PtagCountMap).

:- pred switch_util__get_ptag_counts_2(list(cons_tag), int, int,
	ptag_count_map, ptag_count_map).
:- mode switch_util__get_ptag_counts_2(in, in, out, in, out) is det.

switch_util__get_ptag_counts_2([], Max, Max, PtagCountMap, PtagCountMap).
switch_util__get_ptag_counts_2([ConsTag | TagList], MaxPrimary0, MaxPrimary,
		PtagCountMap0, PtagCountMap) :-
	( ConsTag = unshared_tag(Primary) ->
		int__max(MaxPrimary0, Primary, MaxPrimary1),
		( map__search(PtagCountMap0, Primary, _) ->
			error("unshared tag is shared")
		;
			map__det_insert(PtagCountMap0, Primary, none - (-1),
				PtagCountMap1)
		)
	; ConsTag = shared_remote_tag(Primary, Secondary) ->
		int__max(MaxPrimary0, Primary, MaxPrimary1),
		( map__search(PtagCountMap0, Primary, Target) ->
			Target = TagType - MaxSoFar,
			( TagType = remote ->
				true
			;
				error("remote tag is shared with non-remote")
			),
			int__max(Secondary, MaxSoFar, Max),
			map__det_update(PtagCountMap0, Primary, remote - Max,
				PtagCountMap1)
		;
			map__det_insert(PtagCountMap0, Primary,
				remote - Secondary, PtagCountMap1)
		)
	; ConsTag = shared_local_tag(Primary, Secondary) ->
		int__max(MaxPrimary0, Primary, MaxPrimary1),
		( map__search(PtagCountMap0, Primary, Target) ->
			Target = TagType - MaxSoFar,
			( TagType = local ->
				true
			;
				error("local tag is shared with non-local")
			),
			int__max(Secondary, MaxSoFar, Max),
			map__det_update(PtagCountMap0, Primary, local - Max,
				PtagCountMap1)
		;
			map__det_insert(PtagCountMap0, Primary,
				local - Secondary, PtagCountMap1)
		)
	;
		error("non-du tag in switch_util__get_ptag_counts_2")
	),
	switch_util__get_ptag_counts_2(TagList, MaxPrimary1, MaxPrimary,
		PtagCountMap1, PtagCountMap).

%-----------------------------------------------------------------------------%

	% Group together all the cases that depend on the given variable
	% having the same primary tag value.

switch_util__group_cases_by_ptag([], PtagCaseMap, PtagCaseMap).
switch_util__group_cases_by_ptag([Case0 | Cases0], PtagCaseMap0, PtagCaseMap) :-
	Case0 = case(_Priority, Tag, _ConsId, Goal),
	( Tag = unshared_tag(Primary) ->
		( map__search(PtagCaseMap0, Primary, _Group) ->
			error("unshared tag is shared")
		;
			map__init(StagGoalMap0),
			map__det_insert(StagGoalMap0, -1, Goal, StagGoalMap),
			map__det_insert(PtagCaseMap0, Primary,
				none - StagGoalMap, PtagCaseMap1)
		)
	; Tag = shared_remote_tag(Primary, Secondary) ->
		( map__search(PtagCaseMap0, Primary, Group) ->
			Group = StagLoc - StagGoalMap0,
			( StagLoc = remote ->
				true
			;
				error("remote tag is shared with non-remote")
			),
			map__det_insert(StagGoalMap0, Secondary, Goal,
				StagGoalMap),
			map__det_update(PtagCaseMap0, Primary,
				remote - StagGoalMap, PtagCaseMap1)
		;
			map__init(StagGoalMap0),
			map__det_insert(StagGoalMap0, Secondary, Goal,
				StagGoalMap),
			map__det_insert(PtagCaseMap0, Primary,
				remote - StagGoalMap, PtagCaseMap1)
		)
	; Tag = shared_local_tag(Primary, Secondary) ->
		( map__search(PtagCaseMap0, Primary, Group) ->
			Group = StagLoc - StagGoalMap0,
			( StagLoc = local ->
				true
			;
				error("local tag is shared with non-local")
			),
			map__det_insert(StagGoalMap0, Secondary, Goal,
				StagGoalMap),
			map__det_update(PtagCaseMap0, Primary,
				local - StagGoalMap, PtagCaseMap1)
		;
			map__init(StagGoalMap0),
			map__det_insert(StagGoalMap0, Secondary, Goal,
				StagGoalMap),
			map__det_insert(PtagCaseMap0, Primary,
				local - StagGoalMap, PtagCaseMap1)
		)
	;
		error("non-du tag in switch_util__group_cases_by_ptag")
	),
	switch_util__group_cases_by_ptag(Cases0, PtagCaseMap1, PtagCaseMap).

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

switch_util__order_ptags_by_count(PtagCountList0, PtagCaseMap0, PtagCaseList) :-
	(
		switch_util__select_frequent_ptag(PtagCountList0,
			Primary, _, PtagCountList1)
	->
		( map__search(PtagCaseMap0, Primary, PtagCase) ->
			map__delete(PtagCaseMap0, Primary, PtagCaseMap1),
			switch_util__order_ptags_by_count(PtagCountList1,
				PtagCaseMap1, PtagCaseList1),
			PtagCaseList = [Primary - PtagCase | PtagCaseList1]
		;
			switch_util__order_ptags_by_count(PtagCountList1,
				PtagCaseMap0, PtagCaseList)
		)
	;
		( map__is_empty(PtagCaseMap0) ->
			PtagCaseList = []
		;
			error("PtagCaseMap0 is not empty in switch_util__order_ptags_by_count")
		)
	).

	% Select the most frequently used primary tag based on the number of
	% secondary tags associated with it.

:- pred switch_util__select_frequent_ptag(ptag_count_list, tag_bits, int,
	ptag_count_list).
:- mode switch_util__select_frequent_ptag(in, out, out, out) is semidet.

switch_util__select_frequent_ptag([PtagCount0 | PtagCountList1], Primary, Count,
		PtagCountList) :-
	PtagCount0 = Primary0 - (_ - Count0),
	(
		switch_util__select_frequent_ptag(PtagCountList1,
			Primary1, Count1, PtagCountList2),
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

switch_util__order_ptags_by_value(Ptag, MaxPtag, PtagCaseMap0, PtagCaseList) :-
	( MaxPtag >= Ptag ->
		NextPtag is Ptag + 1,
		( map__search(PtagCaseMap0, Ptag, PtagCase) ->
			map__delete(PtagCaseMap0, Ptag, PtagCaseMap1),
			switch_util__order_ptags_by_value(NextPtag, MaxPtag,
				PtagCaseMap1, PtagCaseList1),
			PtagCaseList = [Ptag - PtagCase | PtagCaseList1]
		;
			switch_util__order_ptags_by_value(NextPtag, MaxPtag,
				PtagCaseMap0, PtagCaseList)
		)
	;
		( map__is_empty(PtagCaseMap0) ->
			PtagCaseList = []
		;
			error("PtagCaseMap0 is not empty in order_ptags_by_value")
		)
	).

%-----------------------------------------------------------------------------%

:- pred switch_util__cons_list_to_tag_list(assoc_list(cons_id, cons_tag),
	list(cons_tag)).
:- mode switch_util__cons_list_to_tag_list(in, out) is det.

switch_util__cons_list_to_tag_list([], []).
switch_util__cons_list_to_tag_list([_ConsId - ConsTag | ConsList],
		[ConsTag | Tagslist]) :-
	switch_util__cons_list_to_tag_list(ConsList, Tagslist).

%-----------------------------------------------------------------------------%
