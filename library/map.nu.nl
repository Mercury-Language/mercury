%---------------------------------------------------------------------------%
% Copyright (C) 1997 University of Melbourne.
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
		error("map__lookup: key not found")
	).

