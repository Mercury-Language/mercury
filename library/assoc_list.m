%---------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 1999-2001, 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: assoc_list.m.
% Main authors: fjh, zs.
% Stability: medium to high.

% This file contains the definition of the type assoc_list(K, V)
% and some predicates which operate on those types.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module assoc_list.

:- interface.

:- import_module list, std_util.

%-----------------------------------------------------------------------------%

:- type assoc_list(K,V)	==	list(pair(K,V)).

:- type assoc_list(T)	==	list(pair(T,T)).

	% Swap the two sides of the pairs in each member of the list.
	%
:- pred assoc_list__reverse_members(assoc_list(K, V)::in,
	assoc_list(V, K)::out) is det.
:- func assoc_list__reverse_members(assoc_list(K, V)) = assoc_list(V, K).

	% Zip together two lists; abort if they are of different lengths.
	%
:- pred assoc_list__from_corresponding_lists(list(K)::in, list(V)::in,
	assoc_list(K,V)::out) is det.
:- func assoc_list__from_corresponding_lists(list(K), list(V))
	= assoc_list(K,V).

	% Return the first member of each pair.
	%
:- pred assoc_list__keys(assoc_list(K, V)::in, list(K)::out) is det.
:- func assoc_list__keys(assoc_list(K, V)) = list(K).

	% Return the second member of each pair.
	%
:- pred assoc_list__values(assoc_list(K, V)::in, list(V)::out) is det.
:- func assoc_list__values(assoc_list(K, V)) = list(V).

	% Return the two lists contain respectively the first and second member
	% of each pair in the assoc_list.
	%
:- pred assoc_list__keys_and_values(assoc_list(K, V)::in,
	list(K)::out, list(V)::out) is det.

	% Find the first element of the association list that matches
	% the given key, and return the associated value.
	%
:- pred assoc_list__search(assoc_list(K, V)::in, K::in, V::out) is semidet.

	% Find the first element of the association list that matches
	% the given key. Return the associated value, and the original
	% list with the selected element removed.
	%
:- pred assoc_list__remove(assoc_list(K, V)::in, K::in, V::out,
	assoc_list(K, V)::out) is semidet.

:- func assoc_list__map_values(func(K, V) = W, assoc_list(K, V))
	= assoc_list(K, W).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require, set, string.

assoc_list__reverse_members([], []).
assoc_list__reverse_members([K - V | KVs], [V - K | VKs]) :-
	assoc_list__reverse_members(KVs, VKs).

assoc_list__from_corresponding_lists(Ks, Vs, KVs) :-
	( assoc_list__from_corresponding_2(Ks, Vs, KVs0) ->
		KVs = KVs0
	;
		KeyType = type_name(type_of(Ks)),
		list__length(Ks, KeyLength),
		string__int_to_string(KeyLength, KeyLengthString),
		ValueType = type_name(type_of(Vs)),
		list__length(Vs, ValueLength),
		string__int_to_string(ValueLength, ValueLengthString),
		string__append_list(
			["assoc_list__from_corresponding_lists: " ++
				"lists have different lengths.\n",
			"\tKey list type: ",
			KeyType,
			"\n\tKey list length: ",
			KeyLengthString,
			"\n\tValue list type: ",
			ValueType,
			"\n\tValue list length: ",
			ValueLengthString
			],
			ErrorString),
		error(ErrorString)
	).

:- pred assoc_list__from_corresponding_2(list(K)::in, list(V)::in,
	assoc_list(K,V)::out) is semidet.

assoc_list__from_corresponding_2([], [], []).
assoc_list__from_corresponding_2([A | As], [B | Bs], [A - B | ABs]) :-
	assoc_list__from_corresponding_2(As, Bs, ABs).

assoc_list__keys([], []).
assoc_list__keys([K - _ | KVs], [K | Ks]) :-
	assoc_list__keys(KVs, Ks).

assoc_list__values([], []).
assoc_list__values([_ - V | KVs], [V | Vs]) :-
	assoc_list__values(KVs, Vs).

assoc_list__keys_and_values([], [], []).
assoc_list__keys_and_values([K - V | KVs], [K | Ks], [V | Vs]) :-
	assoc_list__keys_and_values(KVs, Ks, Vs).

assoc_list__search([K - V | KVs], Key, Value) :-
	( K = Key ->
		Value = V
	;
		assoc_list__search(KVs, Key, Value)
	).

assoc_list__remove([K - V | KVs], Key, Value, Rest) :-
	( K = Key ->
		Value = V,
		Rest = KVs
	;
		assoc_list__remove(KVs, Key, Value, Rest1),
		Rest = [K - V | Rest1]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
% 	Functional forms added.

assoc_list__reverse_members(AL1) = AL2 :-
	assoc_list__reverse_members(AL1, AL2).

assoc_list__from_corresponding_lists(Ks, Vs) = AL :-
	assoc_list__from_corresponding_lists(Ks, Vs, AL).

assoc_list__keys(AL) = Ks :-
	assoc_list__keys(AL, Ks).

assoc_list__values(AL) = Vs :-
	assoc_list__values(AL, Vs).

assoc_list__map_values(_F, []) = [].
assoc_list__map_values(F, [K - V0 | KVs0]) = [K - V | KVs] :-
	V = apply(F, K, V0),
	KVs = assoc_list__map_values(F, KVs0).
