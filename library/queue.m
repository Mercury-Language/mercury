%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997-1999, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: queue.m.
% Main author: fjh.
% Stability: high.

% This file contains a `queue' ADT.
% A queue holds a sequence of values, and provides operations
% to insert values at the end of the queue (queue__put) and remove them from
% the front of the queue (queue__get).
%
% This implementation is in terms of a pair of lists.
% The put and get operations are amortized constant-time.

%--------------------------------------------------------------------------%

:- module queue.
:- interface.
:- import_module list.

:- type queue(T).

	% `queue__init(Queue)' is true iff `Queue' is an empty queue.

:- pred queue__init(queue(T)::out) is det.
:- func queue__init = queue(T).

	% 'queue_equal(Q1, Q2)' is true iff Q1 and Q2 contain the same
	% elements in the same order.

:- pred queue__equal(queue(T)::in, queue(T)::in) is semidet.

	% `queue__is_empty(Queue)' is true iff `Queue' is an empty queue.

:- pred queue__is_empty(queue(T)::in) is semidet.

	% `queue__is_full(Queue)' is intended to be true iff `Queue'
	% is a queue whose capacity is exhausted.  This
	% implementation allows arbitrary-sized queues, so queue__is_full
	% always fails.

:- pred queue__is_full(queue(T)::in) is semidet.

	% `queue__put(Queue0, Elem, Queue)' is true iff `Queue' is
	% the queue which results from appending `Elem' onto the end
	% of `Queue0'.

:- pred queue__put(queue(T)::in, T::in, queue(T)::out) is det.
:- func queue__put(queue(T), T) = queue(T).

	% `queue__put_list(Queue0, Elems, Queue)' is true iff `Queue'
	% is the queue which results from inserting the items in the
	% list `Elems' into `Queue0'.

:- pred queue__put_list(queue(T)::in, list(T)::in, queue(T)::out) is det.
:- func queue__put_list(queue(T), list(T)) = queue(T).

	% `queue__first(Queue, Elem)' is true iff `Queue' is a non-empty
	% queue whose first element is `Elem'.

:- pred queue__first(queue(T)::in, T::out) is semidet.

	% `queue__get(Queue0, Elem, Queue)' is true iff `Queue0' is
	% a non-empty queue whose first element is `Elem', and `Queue'
	% the queue which results from removing that element from
	% the front of `Queue0'.

:- pred queue__get(queue(T)::in, T::out, queue(T)::out) is semidet.

	% `queue__length(Queue, Length)' is true iff `Queue' is a queue
	% containing `Length' elements.

:- pred queue__length(queue(T)::in, int::out) is det.
:- func queue__length(queue(T)) = int.

	% `queue__list_to_queue(List, Queue)' is true iff `Queue' is a queue
	% containing the elements of List, with the first element of List at
	% the head of the queue.

:- pred queue__list_to_queue(list(T)::in, queue(T)::out) is det.
:- func queue__list_to_queue(list(T)) = queue(T).

	% A synonym for queue.list_to_queue/1.
	%
:- func queue__from_list(list(T)) = queue(T).

	% `queue__delete_all(Queue0, Elem, Queue)' is true iff `Queue' is
	% the same queue as `Queue0' with all occurences of `Elem' removed
	% from it.
:- pred queue__delete_all(queue(T)::in, T::in, queue(T)::out) is det.
:- func queue__delete_all(queue(T), T) = queue(T).

%--------------------------------------------------------------------------%

:- implementation.

:- import_module list, std_util, int.

% This implementation is in terms of a pair of lists.  We impose the
% extra constraint that the `off' list is empty if and only if the queue
% is empty.

:- type queue(T) == pair(list(T)).

queue__init([] - []).

queue__equal(On0 - Off0, On1 - Off1) :-
	list__reverse(On0, On0R),
	list__append(Off0, On0R, Q0),
	list__reverse(On1, On1R),
	list__append(Off1, On1R, Q1),
	Q0 = Q1.

queue__is_empty(_ - []).

queue__is_full(_) :-
	semidet_fail.

queue__put(On0 - Off0, Elem, On - Off) :-
	( Off0 = [] ->
		On = On0,
		Off = [Elem]
	;
		On = [Elem | On0],
		Off = Off0
	).

queue__put_list(On0 - Off0, Xs, On - Off) :-
	( Off0 = [] ->
		On = On0,
		Off = Xs
	;
		Off = Off0,
		queue__put_list_2(Xs, On0, On)
	).

:- pred queue__put_list_2(list(T)::in, list(T)::in, list(T)::out) is det.

queue__put_list_2([], On, On).
queue__put_list_2([X | Xs], On0, On) :-
	queue__put_list_2(Xs, [X | On0], On).

queue__first(_ - [Elem | _], Elem).

queue__get(On0 - [Elem | Off0], Elem, On - Off) :-
	( Off0 = [] ->
		list__reverse(On0, Off),
		On = []
	;
		On = On0,
		Off = Off0
	).

queue__length(On - Off, Length) :-
	list__length(On, LengthOn),
	list__length(Off, LengthOff),
	Length = LengthOn + LengthOff.

queue__list_to_queue(List, [] - List).

queue__from_list(List) = [] - List.

queue__delete_all(On0 - Off0, Elem, On - Off) :-
	list__delete_all(On0, Elem, On1),
	list__delete_all(Off0, Elem, Off1),
	( Off1 = [] ->
		list__reverse(On1, Off),
		On = []
	;
		On = On1,
		Off = Off1
	).

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
% 	Function forms added.

queue__init = Q :-
	queue__init(Q).

queue__put(Q1, T) = Q2 :-
	queue__put(Q1, T, Q2).

queue__put_list(Q1, Xs) = Q2 :-
	queue__put_list(Q1, Xs, Q2).

queue__length(Q) = N :-
	queue__length(Q, N).

queue__list_to_queue(Xs) = Q :-
	queue__list_to_queue(Xs, Q).

queue__delete_all(Q1, T) = Q2 :-
	queue__delete_all(Q1, T, Q2).
