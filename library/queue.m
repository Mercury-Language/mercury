%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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

:- pred queue__init(queue(T)).
:- mode queue__init(out) is det.

	% 'queue_equal(Q1, Q2)' is true iff Q1 and Q2 contain the same
	% elements in the same order.

:- pred queue__equal(queue(T), queue(T)).
:- mode queue__equal(in, in) is semidet.

	% `queue__is_empty(Queue)' is true iff `Queue' is an empty queue.

:- pred queue__is_empty(queue(T)).
:- mode queue__is_empty(in) is semidet.

	% `queue__is_full(Queue)' is intended to be true iff `Queue'
	% is a queue whose capacity is exhausted.  This
	% implementation allows arbitrary-sized queues, so queue__is_full
	% always fails.

:- pred queue__is_full(queue(T)).
:- mode queue__is_full(in) is semidet.

	% `queue__put(Queue0, Elem, Queue)' is true iff `Queue' is
	% the queue which results from appending `Elem' onto the end
	% of `Queue0'.

:- pred queue__put(queue(T), T, queue(T)).
:- mode queue__put(in, in, out) is det.

	% `queue__put_list(Queue0, Elems, Queue)' is true iff `Queue'
	% is the queue which results from inserting the items in the
	% list `Elems' into `Queue0'.

:- pred queue__put_list(queue(T), list(T), queue(T)).
:- mode queue__put_list(in, in, out) is det.

	% `queue__first(Queue, Elem)' is true iff `Queue' is a non-empty
	% queue whose first element is `Elem'.

:- pred queue__first(queue(T), T).
:- mode queue__first(in, out) is semidet.

	% `queue__get(Queue0, Elem, Queue)' is true iff `Queue0' is
	% a non-empty queue whose first element is `Elem', and `Queue'
	% the queue which results from removing that element from 
	% the front of `Queue0'.

:- pred queue__get(queue(T), T, queue(T)).
:- mode queue__get(in, out, out) is semidet.

	% `queue__length(Queue, Length)' is true iff `Queue' is a queue
	% containing `Length' elements.

:- pred queue__length(queue(T), int).
:- mode queue__length(in, out) is det.

	% `queue__list_to_queue(List, Queue)' is true iff `Queue' is a queue
	% containing the elements of List, with the first element of List at
	% the head of the queue.

:- pred queue__list_to_queue(list(T), queue(T)).
:- mode queue__list_to_queue(in, out) is det.

%--------------------------------------------------------------------------%

:- implementation.

:- import_module list, std_util, int.

:- type queue(T) == pair(list(T)).

queue__init([] - []).

queue__equal(On0 - Off0, On1 - Off1) :-
	list__reverse(On0, On0R),
	list__append(Off0, On0R, Q0),
	list__reverse(On1, On1R),
	list__append(Off1, On1R, Q1),
	Q0 = Q1.

queue__is_empty([] - []).

queue__is_full(_) :-
	semidet_fail.

queue__put(On - Off, Elem, [Elem | On] - Off).

queue__put_list(Q0, [], Q0).
queue__put_list(Q0, [X | Xs], Q1) :-
	queue__put(Q0, X, Q2),
	queue__put_list(Q2, Xs, Q1).

queue__first(On - Off, Elem) :-
	(	Off = [Elem | _]
	;	Off = [],
		% XXX efficiency could be improved
		list__reverse(On, NewOff),
		NewOff = [Elem | _]
	).

queue__get(On0 - Off0, Elem, On - Off) :-
	queue__get_2(On0, Off0, Elem, On, Off).

:- pred queue__get_2(list(T), list(T), T, list(T), list(T)).
:- mode queue__get_2(in, in, out, out, out) is semidet.

:- queue__get_2(_, X, _, _, _) when X.	% NU-Prolog indexing

queue__get_2(On, [Elem | Off], Elem, On, Off).
queue__get_2(On, [], Elem, [], Off) :-
	list__reverse(On, [Elem | Off]).

queue__length(On - Off, Length) :-
	list__length(On, LengthOn),
	list__length(Off, LengthOff),
	Length is LengthOn + LengthOff.

queue__list_to_queue(List, [] - List).

%--------------------------------------------------------------------------%
