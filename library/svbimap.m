%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: svbimap.m.
%
% This file provides an interface to the 'bimap' ADT that is conducive to the
% use of state variable notation. The predicates here do the same thing as
% their counterparts in the bimap module; the only difference is the order
% of the arguments.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module svbimap.
:- interface.

:- import_module bimap.

%-----------------------------------------------------------------------------%

:- pred svbimap.insert(K::in, V::in, bimap(K, V)::in, bimap(K, V)::out)
    is semidet.

:- pred svbimap.det_insert(K::in, V::in, bimap(K, V)::in, bimap(K, V)::out)
    is det.

:- pred svbimap.set(K::in, V::in, bimap(K, V)::in, bimap(K, V)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

svbimap.insert(K, V, Bimap0, Bimap) :-
    bimap.insert(Bimap0, K, V, Bimap).

svbimap.det_insert(K, V, Bimap0, Bimap) :-
    bimap.det_insert(Bimap0, K, V, Bimap).

svbimap.set(K, V, Bimap0, Bimap) :-
    bimap.set(Bimap0, K, V, Bimap).
