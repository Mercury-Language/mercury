%---------------------------------------------------------------------------%
% Copyright (C) 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: map.nu.nl.
% Main author: dgj.
%
% This file provides an implementation of map__lookup, for which the Mercury
% version calls type_of/1 if the lookup fails, but type_of/1 is not available
% from Prolog.
%
%-----------------------------------------------------------------------------%

:- map__lookup(_Map, K, _V) when K.     % required by bimap.m

map__lookup(Map, K, V) :-
	( tree234__search(Map, K, V1) ->
		V = V1
	;
		map__lookup_error(Map, K, V)
	).

% map__lookup_error is a separate predicate because it is handy
% to be able to set a spy point on it...
map__lookup_error(_Map, _K, _V) :-
	error("map__lookup: key not found").

map__det_insert(Map0, K, V, Map) :-
	( tree234__insert(Map0, K, V, Map1) ->
		Map = Map1
	;
		map__det_insert_error(Map0, K, V)
	).

% map__det_insert_error is a separate predicate because it is handy
% to be able to set a spy point on it...
map__det_insert_error(_Map, _K, _V) :-
	error("map__det_insert: key not found").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
