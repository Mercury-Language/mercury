%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2003, 2006 The University of Melbourne.
% Copyright (C) 2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% set_cc is an implementation of sets which uses compare_representation
% instead of builtin comparison, hence it is suitable for use with terms
% that don't have a canonical representation. It is implemented using
% tree234_cc; see that module for further discussion about the implications
% of using compare_representation.
%
% Author: Mark Brown (dougl)
%
%---------------------------------------------------------------------------%

:- module mdb.set_cc.

:- interface.
:- import_module bool.

:- type set_cc(T).

:- pred init(set_cc(T)::uo) is det.

:- pred is_empty(set_cc(T)::in) is semidet.

:- pred member(T::in, set_cc(T)::in, bool::out) is cc_multi.

:- pred insert(set_cc(T)::in, T::in, set_cc(T)::out) is cc_multi.

:- pred delete(set_cc(T)::in, T::in, set_cc(T)::out) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.tree234_cc.

:- import_module maybe.

:- type set_cc(T) == tree234_cc(T, unit).

init(S) :-
    tree234_cc.init(S).

is_empty(S) :-
    tree234_cc.is_empty(S).

member(T, S, Bool) :-
    tree234_cc.search(S, T, Maybe),
    (
        Maybe = yes(_),
        Bool = yes
    ;
        Maybe = no,
        Bool = no
    ).

insert(S0, T, S) :-
    tree234_cc.set(S0, T, unit, S).

delete(S0, T, S) :-
    tree234_cc.delete(S0, T, S).
