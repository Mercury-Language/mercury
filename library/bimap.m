%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: bimap.m.
% Main author: conway.
% Stability: medium.
%
% This file provides a bijective map ADT.
% A map (also known as a dictionary or an associative array) is a collection
% of (Key,Data) pairs which allows you to look up any Data item given the
% Key.  A bimap also allows you to look up the Key given the Data.
%
% The implementation is a pair of maps.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module bimap.
:- interface.
:- import_module list, assoc_list.

%-----------------------------------------------------------------------------%

:- type bimap(_K, _V).

%-----------------------------------------------------------------------------%

	% Initialize an empty bimap.
:- pred bimap__init(bimap(_,_)).
:- mode bimap__init(out) is det.

	% Check whether a bimap is empty.
:- pred bimap__is_empty(bimap(_,_)).
:- mode bimap__is_empty(in) is semidet.

:- pred bimap__search(bimap(K,V), K, V).
:- mode bimap__search(in, in, out) is semidet.
:- mode bimap__search(in, out, in) is semidet.

:- pred bimap__lookup(bimap(K,V), K, V).
:- mode bimap__lookup(in, in, out) is det.

:- pred bimap__reverse_lookup(bimap(K,V), K, V).
:- mode bimap__reverse_lookup(in, out, in) is det.

:- pred bimap__insert(bimap(K,V), K, V, bimap(K,V)).
:- mode bimap__insert(in, in, in, out) is semidet.

:- pred bimap__set(bimap(K,V), K, V, bimap(K,V)).
:- mode bimap__set(in, in, in, out) is det.

	% Given a bimap, return a list of all the keys in the bimap
:- pred bimap__ordinates(bimap(K, _V), list(K)).
:- mode bimap__ordinates(in, out) is det.

	% Given a bimap, return a list of all the data values in the bimap
:- pred bimap__coordinates(bimap(_K, V), list(V)).
:- mode bimap__coordinates(in, out) is det.

	% convert a bimap to an association list
:- pred bimap__to_assoc_list(bimap(K,V), assoc_list(K,V)).
:- mode bimap__to_assoc_list(in, out) is det.

	% convert an association list to a bimap
:- pred bimap__from_assoc_list(assoc_list(K,V), bimap(K,V)).
:- mode bimap__from_assoc_list(in, out) is det.

/****
	% delete a key-value pair from a bimap
:- pred bimap__delete(bimap(K,V), K, V, bimap(K,V)).
:- mode bimap__delete(in, in, out, out) is det.
:- mode bimap__delete(in, out, in, out) is det.

:- pred bimap__from_corresponding_lists(list(K), list(V), bimap(K, V)).
:- mode bimap__from_corresponding_lists(in, in, out) is det.
****/

%-----------------------------------------------------------------------------%

:- import_module map.

:- type bimap(K,V)	--->	bimap(map(K,V), map(V, K)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

bimap__init(B) :-
	map__init(O),
	map__init(C),
	B = bimap(O, C).

bimap__is_empty(bimap(O,_C)) :-
	map__is_empty(O). % by inference == map__is_empty(C).

bimap__search(bimap(O, C), K, V) :-
	map__search(O, K, V),
	map__search(C, V, K).

bimap__lookup(bimap(O, _C), K, V) :-
	map__lookup(O, K, V).

bimap__reverse_lookup(bimap(_O, C), K, V) :-
	map__lookup(C, V, K).

bimap__insert(bimap(O0, C0), K, V, bimap(O, C)) :-
	map__insert(O0, K, V, O),
	map__insert(C0, V, K, C).
 
bimap__set(bimap(O0, C0), K, V, bimap(O, C)) :-
	map__set(O0, K, V, O),
	map__set(C0, V, K, C).
 
bimap__ordinates(bimap(O, _C), Os) :-
	map__keys(O, Os).

bimap__coordinates(bimap(_O, C), Cs) :-
	map__keys(C, Cs).

bimap__to_assoc_list(bimap(O, _C), L) :-
	map__to_assoc_list(O, L).

bimap__from_assoc_list(L, bimap(O, C)) :-
	map__from_assoc_list(L, O),
	assoc_list__reverse_members(L, L1),
	map__from_assoc_list(L1, C).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
