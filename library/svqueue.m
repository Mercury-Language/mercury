%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: svqueue.m.
% Author: zs.
% Stability: high.
% 
% This file provides an interface to the 'queue' ADT that is conducive to the
% use of state variable notation. The predicates here do the same thing as
% their counterparts in the queue module; the only difference is the order
% of the arguments.
% 
%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- module svqueue.
:- interface.

:- import_module list.
:- import_module queue.

    % `svqueue.put(Elem, Queue0, Queue)' is true iff `Queue' is
    % the queue which results from appending `Elem' onto the end
    % of `Queue0'.
    %
:- pred svqueue.put(T::in, queue(T)::in, queue(T)::out) is det.

    % `svqueue.put_list(Elems, Queue0, Queue)' is true iff `Queue'
    % is the queue which results from inserting the items in the
    % list `Elems' into `Queue0'.
    %
:- pred svqueue.put_list(list(T)::in, queue(T)::in, queue(T)::out) is det.

    % `svqueue.get(Elem, Queue0, Queue)' is true iff `Queue0' is
    % a non-empty queue whose first element is `Elem', and `Queue'
    % the queue which results from removing that element from
    % the front of `Queue0'.
    %
:- pred svqueue.get(T::out, queue(T)::in, queue(T)::out) is semidet.

    % `svqueue.delete_all(Elem, Queue0, Queue)' is true iff `Queue' is
    % the same queue as `Queue0' with all occurences of `Elem' removed
    % from it.
    %
:- pred svqueue.delete_all(T::in, queue(T)::in, queue(T)::out) is det.

    % `svqueue.put_on_front(Elem, Queue0, Queue)' pushes `Elem' on to
    % the front of `Queue0', giving `Queue'.
    %
:- pred svqueue.put_on_front(T::in, queue(T)::in, queue(T)::out) is det.

    % `svqueue.put_list_on_front(Queue0, Elems, Queue)' pushes `Elems'
    % on to the front of `Queue0', giving `Queue' (the Nth member
    % of `Elems' becomes the Nth member from the front of `Queue').
    %
:- pred svqueue.put_list_on_front(list(T)::in, queue(T)::in, queue(T)::out)
    is det.

    % `queue.get_from_back(Elem, Queue0, Queue)' removes `Elem' from
    % the back of `Queue0', giving `Queue'.
    %
:- pred svqueue.get_from_back(T::out, queue(T)::in, queue(T)::out) is semidet.

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- implementation.

svqueue.put(Elem, Queue0, Queue) :-
    queue.put(Queue0, Elem, Queue).

svqueue.put_list(List, Queue0, Queue) :-
    queue.put_list(Queue0, List, Queue).

svqueue.get(Elem, Queue0, Queue) :-
    queue.get(Queue0, Elem, Queue).

svqueue.delete_all(Elem, Queue0, Queue) :-
    queue.delete_all(Queue0, Elem, Queue).

svqueue.put_on_front(Elem, Queue0, Queue) :-
    queue.put_on_front(Queue0, Elem, Queue).

svqueue.put_list_on_front(Elems, Queue0, Queue) :-
    queue.put_list_on_front(Queue0, Elems, Queue).

svqueue.get_from_back(Elem, Queue0, Queue) :-
    queue.get_from_back(Queue0, Elem, Queue).
