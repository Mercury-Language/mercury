%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997-1999, 2003-2006, 2011 The University of Melbourne.
% Copyright (C) 2014-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: queue.m.
% Main author: fjh.
% Stability: high.
%
% This file contains a `queue' ADT.
% A queue holds a sequence of values, and provides operations
% to insert values at the end of the queue (put) and remove them from
% the front of the queue (get).
%
% This implementation is in terms of a pair of lists.
% The put and get operations are amortized constant-time.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module queue.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

:- type queue(T).

    % `init(Queue)' is true iff `Queue' is an empty queue.
    %
:- func init = queue(T).
:- pred init(queue(T)::out) is det.

    % 'queue_equal(Q1, Q2)' is true iff Q1 and Q2 contain the same
    % elements in the same order.
    %
:- pred equal(queue(T)::in, queue(T)::in) is semidet.

    % `is_empty(Queue)' is true iff `Queue' is an empty queue.
    %
:- pred is_empty(queue(T)::in) is semidet.

    % `is_full(Queue)' is intended to be true iff `Queue' is a queue
    % whose capacity is exhausted. This implementation allows arbitrary-sized
    % queues, so is_full always fails.
    %
:- pred is_full(queue(T)::in) is semidet.

    % `put(Elem, Queue0, Queue)' is true iff `Queue' is the queue
    % which results from appending `Elem' onto the end of `Queue0'.
    %
:- func put(queue(T), T) = queue(T).
:- pred put(T::in, queue(T)::in, queue(T)::out) is det.

    % `put_list(Elems, Queue0, Queue)' is true iff `Queue' is the queue
    % which results from inserting the items in the list `Elems' into `Queue0'.
    %
:- func put_list(queue(T), list(T)) = queue(T).
:- pred put_list(list(T)::in, queue(T)::in, queue(T)::out) is det.

    % `first(Queue, Elem)' is true iff `Queue' is a non-empty queue
    % whose first element is `Elem'.
    %
:- pred first(queue(T)::in, T::out) is semidet.

    % `get(Elem, Queue0, Queue)' is true iff `Queue0' is a non-empty
    % queue whose first element is `Elem', and `Queue' the queue which results
    % from removing that element from the front of `Queue0'.
    %
:- pred get(T::out, queue(T)::in, queue(T)::out) is semidet.

    % `length(Queue, Length)' is true iff `Queue' is a queue
    % containing `Length' elements.
    %
:- func length(queue(T)) = int.
:- pred length(queue(T)::in, int::out) is det.

    % `list_to_queue(List, Queue)' is true iff `Queue' is a queue
    % containing the elements of List, with the first element of List at
    % the head of the queue.
    %
:- func list_to_queue(list(T)) = queue(T).
:- pred list_to_queue(list(T)::in, queue(T)::out) is det.

    % A synonym for list_to_queue/1.
    %
:- func from_list(list(T)) = queue(T).

    % `to_list(Queue) = List' is the inverse of from_list/1.
    %
:- func to_list(queue(T)) = list(T).

    % `delete_all(Elem, Queue0, Queue)' is true iff `Queue' is the same
    % queue as `Queue0' with all occurrences of `Elem' removed from it.
    %
:- func delete_all(queue(T), T) = queue(T).
:- pred delete_all(T::in, queue(T)::in, queue(T)::out) is det.

    % `put_on_front(Queue0, Elem) = Queue' pushes `Elem' on to
    % the front of `Queue0', giving `Queue'.
    %
:- func put_on_front(queue(T), T) = queue(T).
:- pred put_on_front(T::in, queue(T)::in, queue(T)::out) is det.

    % `put_list_on_front(Queue0, Elems) = Queue' pushes `Elems'
    % on to the front of `Queue0', giving `Queue' (the N'th member
    % of `Elems' becomes the N'th member from the front of `Queue').
    %
:- func put_list_on_front(queue(T), list(T)) = queue(T).
:- pred put_list_on_front(list(T)::in, queue(T)::in, queue(T)::out)
    is det.

    % `get_from_back(Elem, Queue0, Queue)' removes `Elem' from
    % the back of `Queue0', giving `Queue'.
    %
:- pred get_from_back(T::out, queue(T)::in, queue(T)::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%---------------------------------------------------------------------------%

    % This implementation is in terms of a pair of lists: the list of items
    % in the queue is given by off_list ++ reverse(on_list). The reason for
    % the names is that we generally get items off the off_list and put them
    % on the on_list. We impose the extra constraint that the off_list field
    % is empty if and only if the queue as a whole is empty.
    %
:- type queue(T)
    --->    queue(
                on_list  :: list(T),
                off_list :: list(T)
            ).

%---------------------------------------------------------------------------%

init = Q :-
    queue.init(Q).

init(queue([], [])).

equal(queue(OnA, OffA), queue(OnB, OffB)) :-
    QA = OffA ++ list.reverse(OnA),
    QB = OffB ++ list.reverse(OnB),
    QA = QB.

is_empty(queue(_, [])).

is_full(_) :-
    semidet_fail.

put(!.Q, T) = !:Q :-
    queue.put(T, !Q).

put(Elem, queue(On0, Off0), queue(On, Off)) :-
    (
        Off0 = [],
        On = On0,
        Off = [Elem]
    ;
        Off0 = [_ | _],
        On = [Elem | On0],
        Off = Off0
    ).

put_list(!.Q, Xs) = !:Q :-
    queue.put_list(Xs, !Q).

put_list(Xs, queue(On0, Off0), queue(On, Off)) :-
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

put_list_2([], On, On).
put_list_2([X | Xs], On0, On) :-
    queue.put_list_2(Xs, [X | On0], On).

first(queue(_, [Elem | _]), Elem).

get(Elem, queue(On0, [Elem | Off0]), queue(On, Off)) :-
    (
        Off0 = [],
        list.reverse(On0, Off),
        On = []
    ;
        Off0 = [_ | _],
        On = On0,
        Off = Off0
    ).

length(Q) = N :-
    queue.length(Q, N).

length(queue(On, Off), Length) :-
    list.length(On, LengthOn),
    list.length(Off, LengthOff),
    Length = LengthOn + LengthOff.

list_to_queue(Xs) = Q :-
    queue.list_to_queue(Xs, Q).

list_to_queue(List, queue([], List)).

from_list(List) = queue([], List).

to_list(queue(On, Off)) = Off ++ list.reverse(On).

delete_all(!.Q, T) = !:Q :-
    queue.delete_all(T, !Q).

delete_all(Elem ,queue(On0, Off0), queue(On, Off)) :-
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

put_on_front(!.Queue, Elem) = !:Queue :-
    queue.put_on_front(Elem, !Queue).

put_on_front(Elem, queue(On, Off), queue(On, [Elem | Off])).

put_list_on_front(!.Queue, Elems) = !:Queue :-
    queue.put_list_on_front(Elems, !Queue).

put_list_on_front(Elems, queue(On, Off), queue(On, Elems ++ Off)).

get_from_back(Elem, queue(On0, Off0), queue(On, Off)) :-
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

%---------------------------------------------------------------------------%
:- end_module queue.
%---------------------------------------------------------------------------%
