%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: group.m.
% main author: conway.
% stability: low.
%
% This module is probably not terribly useful, and it may not be supported
% in future releases.
%
% The `group' module provides a facility for handling a partitioned set.
% A group is a set of sets of elements, where each element is unique within
% the scope of the group. The module provides moderately efficient ways for
% manipulating groups and elements.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module group.

:- interface.

:- import_module set, list, assoc_list.

:- type group(T).

:- type group__key.

	% Create an empty group

:- pred group__init(group(T)).
:- mode group__init(out) is det.

	% Insert a set of elements into the group.

:- pred group__insert(group(T), set(T), group(T)).
:- mode group__insert(in, in, out) is det.

	% Given an element, get the set containing that element.

:- pred group__group(group(T), T, set(T)).
:- mode group__group(in, in, out) is det.

	% Convert the group to a set of sets.

:- pred group__to_set(group(T), set(set(T))).
:- mode group__to_set(in, out) is det.

:- pred group__sets_and_keys(group(T), assoc_list(set(T), group__key)).
:- mode group__sets_and_keys(in, out) is det.

	% Given an element, get the key for the group containing
	% that element.

:- pred group__group_key(group(T), T, group__key).
:- mode group__group_key(in, in, out) is det.

	% Given a group key, get the corresponding set of elements.

:- pred group__key_group(group(T), group__key, set(T)).
:- mode group__key_group(in, in, out) is det.

	% Remove a set from the group, and return the set.

:- pred group__remove_group(group(T), group__key, set(T), group(T)).
:- mode group__remove_group(in, in, out, out) is det.

	% Test to see if two elements are in the same set.

:- pred group__same_group(group(T), T, T).
:- mode group__same_group(in, in, in) is semidet.

:- pred group__largest_group_key(group(T), group__key).
:- mode group__largest_group_key(in, out) is det.

:- pred group__group_keys(group(T), list(group__key)).
:- mode group__group_keys(in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module map, int, require, std_util.

:- type group(T)	--->
		group(
			group__key,
			map(group__key, set(T)),
			map(T, group__key)
		).

:- type group__key	==	int.

group__init(G) :-
	map__init(Es),
	map__init(Ss),
	G = group(0, Es, Ss).

group__insert(G0, S, G) :-
	group__get_group_count(G0, C0),
	C is C0 + 1,
	group__get_sets(G0, Ss0),
	map__set(Ss0, C, S, Ss),
	group__get_elements(G0, Es0),
	set__to_sorted_list(S, SL),
	group__insert_elements(SL, C, Es0, Es),
	group__set_group_count(G0, C, G1),
	group__set_sets(G1, Ss, G2),
	group__set_elements(G2, Es, G).

:- pred group__insert_elements(list(T), group__key,
				map(T, group__key), map(T, group__key)).
:- mode group__insert_elements(in, in, in, out) is det.

group__insert_elements([], _GK, Es, Es).
group__insert_elements([I|Is], GK, Es0, Es) :-
	map__set(Es0, I, GK, Es1),
	group__insert_elements(Is, GK, Es1, Es).

group__group(G, E, S) :-
	group__get_elements(G, Es),
	map__lookup(Es, E, GK),
	group__get_sets(G, Ss),
	map__lookup(Ss, GK, S).

group__to_set(G, S) :-
	group__get_sets(G, SS),
	map__values(SS, S0),
	set__list_to_set(S0, S).

group__sets_and_keys(G, SKs) :-
	group__get_sets(G, SS),
	map__to_assoc_list(SS, SKs0),
	assoc_list__reverse_members(SKs0, SKs).

group__group_key(G, E, GK) :-
	group__get_elements(G, Es),
	map__lookup(Es, E, GK).

group__key_group(G, GK, S) :-
	group__get_sets(G, Ss),
	map__lookup(Ss, GK, S).

group__remove_group(G0, GK, S, G) :-
	group__get_sets(G0, Ss0),
	(
		map__remove(Ss0, GK, S1, Ss1)
	->
		S = S1,
		Ss = Ss1
	;
		error("map__remove unexpectedly failed.")
	),
	group__get_elements(G0, Es0),
	set__to_sorted_list(S, SL),
	group__remove_elements(SL, Es0, Es),
	group__set_sets(G0, Ss, G1),
	group__set_elements(G1, Es, G).

:- pred group__remove_elements(list(T), map(T, group__key), map(T, group__key)).
:- mode group__remove_elements(in, in, out) is det.

group__remove_elements([], Es, Es).
group__remove_elements([I|Is], Es0, Es) :-
	map__delete(Es0, I, Es1),
	group__remove_elements(Is, Es1, Es).

group__same_group(G, E0, E1) :-
	group__get_elements(G, Es),
	map__lookup(Es, E0, GK),
	map__lookup(Es, E1, GK).

group__largest_group_key(G, GK) :-
	group__get_sets(G, Ss),
	map__to_assoc_list(Ss, SL),
	group__largest_group_key_2(SL, 0, 0, GK).

:- pred group__largest_group_key_2(assoc_list(group__key, set(T)), int,
							group__key, group__key).
:- mode group__largest_group_key_2(in, in, in, out) is det.

group__largest_group_key_2([], _, GK, GK).
group__largest_group_key_2([GK0-S0|Ss], Sz0, GK1, GK) :-
	set__to_sorted_list(S0, S1),
	list__length(S1, Sz1),
	compare(R, Sz1, Sz0),
	(
		R = (>)
	->
		Sz = Sz1,
		GK2 = GK0
	;
		Sz = Sz0,
		GK2 = GK1
	),
	group__largest_group_key_2(Ss, Sz, GK2, GK).

%---------------------------------------------------------------------------%

group__group_keys(G, Ks) :-
	group__get_sets(G, Ss),
	map__keys(Ss, Ks).

%---------------------------------------------------------------------------%

:- pred group__get_group_count(group(T), int).
:- mode group__get_group_count(in, out) is det.

group__get_group_count(G, C) :-
	G = group(C, _, _).

:- pred group__get_sets(group(T), map(group__key, set(T))).
:- mode group__get_sets(in, out) is det.

group__get_sets(G, S) :-
	G = group(_, S, _).

:- pred group__get_elements(group(T), map(T, group__key)).
:- mode group__get_elements(in, out) is det.

group__get_elements(G, E) :-
	G = group(_, _, E).

:- pred group__set_group_count(group(T), int, group(T)).
:- mode group__set_group_count(in, in, out) is det.

group__set_group_count(G0, C, G) :-
	G0 = group(_, S, E),
	G = group(C, S, E).

:- pred group__set_sets(group(T), map(group__key, set(T)), group(T)).
:- mode group__set_sets(in, in, out) is det.

group__set_sets(G0, S, G) :-
	G0 = group(C, _, E),
	G = group(C, S, E).

:- pred group__set_elements(group(T), map(T, group__key), group(T)).
:- mode group__set_elements(in, in, out) is det.

group__set_elements(G0, E, G) :-
	G0 = group(C, S, _),
	G = group(C, S, E).

%---------------------------------------------------------------------------%
