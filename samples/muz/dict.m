%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1999, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: dict.m
% main author: philip

:- module dict.

:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module word.
:- import_module ztype.

:- type dict.

:- func init = dict.

:- pred insert(ident::in, entry::in, dict::in, dict::out) is semidet.

:- pred search(ident::in, entry::out, dict::in) is semidet.

:- pred overlay(assoc_list(ident, entry)::in, dict::in, dict::out) is det.

:- pred delete_list(list(ident)::in, dict::in, dict::out) is det.

:- implementation.

:- import_module map.

:- type dict == map(ident, entry).

init = D :-
    map.init(D0),
%   I0 = powerIdent,  map.det_insert(I0, powerEntry, D0, D1),
    D1 = D0,
    I1 = numIdent,
    map.det_insert(I1, givenEntry(I1), D1, D2),
    I2 = stringIdent,
    map.det_insert(I2, givenEntry(I2), D2, D).

insert(Ident, Entry, D0, D) :-
    map.insert(Ident, Entry, D0, D).

search(Ident, Entry, D) :-
    map.search(D, Ident, Entry).

overlay(List, D0, D) :-
    map.from_assoc_list(List, Map), map.overlay(D0, Map, D).

delete_list(List, D0, D) :-
    map.delete_list(List, D0, D).
