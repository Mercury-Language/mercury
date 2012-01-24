%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2011-2012 The University of Melbourne.
%
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: svpqueue.m.
% Main author: conway.
% Stability: high.
%
% This file provides an interface to the 'pqueue' ADT that is conducive to the
% use of state variable notation. The predicates here do the same thing as
% their counterparts in the pqueue module; the only difference is the order
% of the arguments.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module svpqueue.
:- interface.

:- import_module pqueue.

%---------------------------------------------------------------------------%
    
    % Insert a value V with key K into a priority queue
    % and return the new priority queue.
    %
:- pragma obsolete(svpqueue.insert/4).
:- pred svpqueue.insert(K::in, V::in, pqueue(K, V)::in, pqueue(K, V)::out)
    is det.
    
    % Remove the smallest item from the priority queue.
    % Fails if the priority queue is empty.
    %
:- pragma obsolete(svpqueue.remove/4).
:- pred svpqueue.remove(K::out, V::out, pqueue(K, V)::in, pqueue(K, V)::out)
    is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

svpqueue.insert(K, V, !PQ) :-
    pqueue.insert(K, V, !PQ).

svpqueue.remove(K, V, !PQ) :-
    pqueue.remove(K, V, !PQ).

%---------------------------------------------------------------------------%
:- end_module svpqueue.
%---------------------------------------------------------------------------%
