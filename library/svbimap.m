%---------------------------------------------------------------------------%
% Copyright (C) 2004 The University of Melbourne.
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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module svbimap.

:- interface.

:- import_module bimap.

%-----------------------------------------------------------------------------%

:- pred svbimap__insert(K::in, V::in, bimap(K, V)::in, bimap(K, V)::out)
	is semidet.

:- pred svbimap__det_insert(K::in, V::in, bimap(K, V)::in, bimap(K, V)::out)
	is det.

:- pred svbimap__set(K::in, V::in, bimap(K, V)::in, bimap(K, V)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

svbimap__insert(K, V, Bimap0, Bimap) :-
	bimap__insert(Bimap0, K, V, Bimap).

svbimap__det_insert(K, V, Bimap0, Bimap) :-
	bimap__det_insert(Bimap0, K, V, Bimap).

svbimap__set(K, V, Bimap0, Bimap) :-
	bimap__set(Bimap0, K, V, Bimap).
