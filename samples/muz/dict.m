%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: dict.m
% main author: philip

:- module dict.

:- interface.

:- import_module word, ztype, list, assoc_list.

:- type dict.

:- func init = dict.

:- pred insert(ident::in, entry::in, dict::in, dict::out) is semidet.

:- pred search(ident::in, entry::out, dict::in) is semidet.

:- pred overlay(assoc_list(ident, entry)::in, dict::in, dict::out) is det.

:- pred delete_list(list(ident)::in, dict::in, dict::out) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module map.

:- type dict == map(ident, entry).

init = D :-
	map__init(D0),
%	I0 = powerIdent,  map__det_insert(D0, I0, powerEntry,     D1),
	D1 = D0,
	I1 = numIdent,    map__det_insert(D1, I1, givenEntry(I1), D2),
	I2 = stringIdent, map__det_insert(D2, I2, givenEntry(I2), D).

insert(Ident, Entry, D0, D) :- map__insert(D0, Ident, Entry, D).

search(Ident, Entry, D) :- map__search(D, Ident, Entry).

overlay(List, D0, D) :-
	map__from_assoc_list(List, Map), map__overlay(D0, Map, D).

delete_list(List, D0, D) :- map__delete_list(D0, List, D).
