%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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

:- pred assoc_list__reverse_members(assoc_list(K, V), assoc_list(V, K)).
:- mode assoc_list__reverse_members(in, out) is det.

	% Zip together two lists; abort if they are of different lengths.

:- pred assoc_list__from_corresponding_lists(list(K), list(V), assoc_list(K,V)).
:- mode assoc_list__from_corresponding_lists(in, in, out) is det.

	% Return the first member of each pair.

:- pred assoc_list__keys(assoc_list(K, V), list(K)).
:- mode assoc_list__keys(in, out) is det.

	% Return the second member of each pair.

:- pred assoc_list__values(assoc_list(K, V), list(V)).
:- mode assoc_list__values(in, out) is det.

	% Find the first element of the association list that matches
	% the given key, and return the associated value.

:- pred assoc_list__search(assoc_list(K, V), K, V).
:- mode assoc_list__search(in, in, out) is semidet.

	% Find the first element of the association list that matches
	% the given key. Return the associated value, and the original
	% list with the selected element removed.

:- pred assoc_list__remove(assoc_list(K, V), K, V,
	assoc_list(K, V)).
:- mode assoc_list__remove(in, in, out, out) is semidet.

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
			["assoc_list__from_corresponding_lists: lists have different lengths.\n",
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

:- pred assoc_list__from_corresponding_2(list(K), list(V), assoc_list(K,V)).
:- mode assoc_list__from_corresponding_2(in, in, out) is semidet.

assoc_list__from_corresponding_2([], [], []).
assoc_list__from_corresponding_2([A | As], [B | Bs], [A - B | ABs]) :-
	assoc_list__from_corresponding_2(As, Bs, ABs).

assoc_list__keys([], []).
assoc_list__keys([K - _ | KVs], [K | Ks]) :-
	assoc_list__keys(KVs, Ks).

assoc_list__values([], []).
assoc_list__values([_ - V | KVs], [V | Vs]) :-
	assoc_list__values(KVs, Vs).

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
