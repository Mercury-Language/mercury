%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2011 The University of Melbourne.
%
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: svlist.m.
% Authors: fjh, conway, trd, zs, philip, warwick, ...
% Stability: medium to high.
%
% This file provides an interface to the 'list' ADT that is conducive to the
% use of state variable notation. The predicates here do the same thing as
% their counterparts in the pqueue module; the only difference is the order
% of the arguments.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module svlist.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

    % svlist.delete(Elem, List, Remainder):
    %
    % True iff `Elem' occurs in `List', and `Remainder' is the result of
    % deleting one occurrence of `Elem' from `List'.
    %
:- pred svlist.delete(T, list(T), list(T)).
:- mode svlist.delete(in, in, in) is semidet.
:- mode svlist.delete(in, in, out) is nondet.
:- mode svlist.delete(out, in, out) is nondet.
:- mode svlist.delete(in, out, in) is multi.

    % svlist.delete_first(Elem, List0, List) is true iff Elem occurs in List0
    % and List is List0 with the first occurrence of Elem removed.
    %
:- pred svlist.delete_first(T::in, list(T)::in, list(T)::out) is semidet.

    % svlist.delete_elems(Elems, List0, List) is true iff List is List0 with
    % all occurrences of all elements of Elems removed.
    %
:- pred svlist.delete_elems(list(T)::in, list(T)::in, list(T)::out) is det.
    
    % svlist.replace(D, R, List0, List) is true iff List is List0
    % with an occurrence of D replaced with R.
    %
:- pred svlist.replace(T, T, list(T), list(T)).
:- mode svlist.replace(in, in, in, in) is semidet.
:- mode svlist.replace(in, in, in, out) is nondet.
    
    % svlist.replace_first(D, R, List0, List) is true iff List is List0
    % with the first occurrence of D replaced with R.
    %
:- pred svlist.replace_first(T::in, T::in, list(T)::in, list(T)::out)
    is semidet.

    % svlist.replace_all(D, R, List0, List) is true iff List is List0
    % with all occurrences of D replaced with R.
    %
:- pred svlist.replace_all(T::in, T::in, list(T)::in, list(T)::out) is det.
    
    % svlist.det_replace_nth(N, R, List0, List) is true iff List is List0
    % with Nth element replaced with R.
    % Aborts if N < 1 or if length of List0 < N.
    % (Position numbers start from 1.)
    %
:- pred svlist.det_replace_nth(int::in, T::in, list(T)::in, list(T)::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

svlist.delete(E, !List) :-
    list.delete(!.List, E, !:List).

svlist.delete_first(E, !List) :-
    list.delete_first(!.List, E, !:List).

svlist.delete_elems(Elems, !List) :-
    list.delete_elems(!.List, Elems, !:List).

svlist.replace(D, R, !List) :-
    list.replace(!.List, D, R, !:List).

svlist.replace_first(D, R, !List) :-
    list.replace_first(!.List, D, R, !:List).

svlist.replace_all(D, R, !List) :-
    list.replace_all(!.List, D, R, !:List).

svlist.det_replace_nth(N, R, !List) :-
    list.det_replace_nth(!.List, N, R, !:List).

%---------------------------------------------------------------------------%
:- end_module svlist.
%---------------------------------------------------------------------------%
