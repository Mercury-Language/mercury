%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997-1999, 2003-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: queue.m.
% Main author: fjh.
% Stability: high.

% This file contains a `queue' ADT.
% A queue holds a sequence of values, and provides operations
% to insert values at the end of the queue (queue.put) and remove them from
% the front of the queue (queue.get).
%
% This implementation is in terms of a pair of lists.
% The put and get operations are amortized constant-time.

%--------------------------------------------------------------------------%

:- module queue.
:- interface.
:- import_module list.

:- type queue(T).

    % `queue.init(Queue)' is true iff `Queue' is an empty queue.
    %
:- pred queue.init(queue(T)::out) is det.
:- func queue.init = queue(T).

    % 'queue_equal(Q1, Q2)' is true iff Q1 and Q2 contain the same
    % elements in the same order.
    %
:- pred queue.equal(queue(T)::in, queue(T)::in) is semidet.

    % `queue.is_empty(Queue)' is true iff `Queue' is an empty queue.
    %
:- pred queue.is_empty(queue(T)::in) is semidet.

    % `queue.is_full(Queue)' is intended to be true iff `Queue' is a queue
    % whose capacity is exhausted. This implementation allows arbitrary-sized
    % queues, so queue.is_full always fails.
    %
:- pred queue.is_full(queue(T)::in) is semidet.

    % `queue.put(Queue0, Elem, Queue)' is true iff `Queue' is the queue
    % which results from appending `Elem' onto the end of `Queue0'.
    %
:- pred queue.put(queue(T)::in, T::in, queue(T)::out) is det.
:- func queue.put(queue(T), T) = queue(T).

    % `queue.put_list(Queue0, Elems, Queue)' is true iff `Queue' is the queue
    % which results from inserting the items in the list `Elems' into `Queue0'.
    %
:- pred queue.put_list(queue(T)::in, list(T)::in, queue(T)::out) is det.
:- func queue.put_list(queue(T), list(T)) = queue(T).

    % `queue.first(Queue, Elem)' is true iff `Queue' is a non-empty queue
    % whose first element is `Elem'.
    %
:- pred queue.first(queue(T)::in, T::out) is semidet.

    % `queue.get(Queue0, Elem, Queue)' is true iff `Queue0' is a non-empty
    % queue whose first element is `Elem', and `Queue' the queue which results
    % from removing that element from the front of `Queue0'.
    %
:- pred queue.get(queue(T)::in, T::out, queue(T)::out) is semidet.

    % `queue.length(Queue, Length)' is true iff `Queue' is a queue
    % containing `Length' elements.
    %
:- pred queue.length(queue(T)::in, int::out) is det.
:- func queue.length(queue(T)) = int.

    % `queue.list_to_queue(List, Queue)' is true iff `Queue' is a queue
    % containing the elements of List, with the first element of List at
    % the head of the queue.
    %
:- pred queue.list_to_queue(list(T)::in, queue(T)::out) is det.
:- func queue.list_to_queue(list(T)) = queue(T).

    % A synonym for queue.list_to_queue/1.
    %
:- func queue.from_list(list(T)) = queue(T).

    % `queue.to_list(Queue) = List' is the inverse of queue.from_list/1.
    %
:- func queue.to_list(queue(T)) = list(T).

    % `queue.delete_all(Queue0, Elem, Queue)' is true iff `Queue' is the same
    % queue as `Queue0' with all occurrences of `Elem' removed from it.
    %
:- pred queue.delete_all(queue(T)::in, T::in, queue(T)::out) is det.
:- func queue.delete_all(queue(T), T) = queue(T).

    % `queue.put_on_front(Queue0, Elem) = Queue' pushes `Elem' on to
    % the front of `Queue0', giving `Queue'.
    %
:- func queue.put_on_front(queue(T), T) = queue(T).
:- pred queue.put_on_front(queue(T)::in, T::in, queue(T)::out) is det.

    % `queue.put_list_on_front(Queue0, Elems) = Queue' pushes `Elems'
    % on to the front of `Queue0', giving `Queue' (the Nth member
    % of `Elems' becomes the Nth member from the front of `Queue').
    %
:- func queue.put_list_on_front(queue(T), list(T)) = queue(T).
:- pred queue.put_list_on_front(queue(T)::in, list(T)::in, queue(T)::out)
    is det.

    % `queue.get_from_back(Queue0, Elem, Queue)' removes `Elem' from
    % the back of `Queue0', giving `Queue'.
    %
:- pred queue.get_from_back(queue(T)::in, T::out, queue(T)::out) is semidet.

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module std_util.

    % This implementation is in terms of a pair of lists. We impose the
    % extra constraint that the `off' list is empty if and only if the queue
    % is empty.
:- type queue(T) == pair(list(T)).

queue.init([] - []).

queue.equal(On0 - Off0, On1 - Off1) :-
    list.reverse(On0, On0R),
    list.append(Off0, On0R, Q0),
    list.reverse(On1, On1R),
    list.append(Off1, On1R, Q1),
    Q0 = Q1.

queue.is_empty(_ - []).

queue.is_full(_) :-
    semidet_fail.

queue.put(On0 - Off0, Elem, On - Off) :-
    (
        Off0 = [],
        On = On0,
        Off = [Elem]
    ;
        Off0 = [_ | _],
        On = [Elem | On0],
        Off = Off0
    ).

queue.put_list(On0 - Off0, Xs, On - Off) :-
    (
        Off0 = [],
        On = On0,
        Off = Xs
    ;
        Off0 = [_ | _],
        Off = Off0,
        queue.put_list_2(Xs, On0, On)
    ).

:- pred queue.put_list_2(list(T)::in, list(T)::in, list(T)::out) is det.

queue.put_list_2([], On, On).
queue.put_list_2([X | Xs], On0, On) :-
    queue.put_list_2(Xs, [X | On0], On).

queue.first(_ - [Elem | _], Elem).

queue.get(On0 - [Elem | Off0], Elem, On - Off) :-
    (
        Off0 = [],
        list.reverse(On0, Off),
        On = []
    ;
        Off0 = [_ | _],
        On = On0,
        Off = Off0
    ).

queue.length(On - Off, Length) :-
    list.length(On, LengthOn),
    list.length(Off, LengthOff),
    Length = LengthOn + LengthOff.

queue.list_to_queue(List, [] - List).

queue.from_list(List) = [] - List.

queue.to_list(On - Off) = Off ++ list.reverse(On).

queue.delete_all(On0 - Off0, Elem, On - Off) :-
    list.delete_all(On0, Elem, On1),
    list.delete_all(Off0, Elem, Off1),
    (
        Off1 = [],
        list.reverse(On1, Off),
        On = []
    ;
        Off1 = [_ | _],
        On = On1,
        Off = Off1
    ).

queue.put_on_front(On - Off, Elem, On - [Elem | Off]).

queue.put_on_front(Queue0, Elem) = Queue :-
    queue.put_on_front(Queue0, Elem, Queue).

queue.put_list_on_front(On - Off, Elems, On - (Elems ++ Off)).

queue.put_list_on_front(Queue0, Elems) = Queue :-
    queue.put_list_on_front(Queue0, Elems, Queue).

queue.get_from_back(On0 - Off0, Elem, On - Off) :-
    (
        % The On list is non-empty and the last element in the queue
        % is the head of the On list.
        On0 = [Elem | On],
        Off = Off0
    ;
        % The On list is empty.
        On0 = [],
        (
            % The Off list contains a single element.
            Off0 = [Elem],
            On   = [],
            Off  = []
        ;
            % The Off list contains two or more elements. We split it in two
            % and take the head of the new On list as Elem.
            Off0 = [_, _ | _],
            N    = list.length(Off0),
            list.split_list(N / 2, Off0, Off, RevOn),
            [Elem | On] = list.reverse(RevOn)
        )
    ).

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
%   Function forms added.

queue.init = Q :-
    queue.init(Q).

queue.put(Q1, T) = Q2 :-
    queue.put(Q1, T, Q2).

queue.put_list(Q1, Xs) = Q2 :-
    queue.put_list(Q1, Xs, Q2).

queue.length(Q) = N :-
    queue.length(Q, N).

queue.list_to_queue(Xs) = Q :-
    queue.list_to_queue(Xs, Q).

queue.delete_all(Q1, T) = Q2 :-
    queue.delete_all(Q1, T, Q2).
