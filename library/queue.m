%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

% File: queue.nl.
% Main author: fjh.

% This file contains a `queue' ADT.
% this implementation is in terms of a list pair.

%--------------------------------------------------------------------------%

:- module queue.
:- interface.
:- import_module int.

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
	% implement allows arbitrary-sized queues, so queue__is_full
	% always fails.

:- pred queue__is_full(queue(T)).
:- mode queue__is_full(in) is semidet.

	% `queue__put(Queue0, Elem, Queue)' is true iff `Queue' is
	% the queue which results from appending `Elem' onto the end
	% of `Queue0'.

:- pred queue__put(queue(T), T, queue(T)).
:- mode queue__put(in, in, out) is det.

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
:- mode queue__length(in, out).

%--------------------------------------------------------------------------%

:- implementation.

:- import_module list, std_util.

:- type queue(T) == pair(list(T), list(T)).

queue__init([] - []).

queue__equal(On0 - Off0, On1 - Off1) :-
	reverse(On0, On0R),
	append(Off0, On0R, Q0),
	reverse(On1, On1R),
	append(Off1, On1R, Q1),
	Q0 = Q1.

queue__is_empty([] - []).

queue__is_full(_) :- fail.

queue__put(On0 - Off, Elem, (Elem.On0) - Off).

queue__first(_On - (Elem._Off), Elem).

queue__get(On - (Elem.Off), Elem, On - Off).
queue__get(On - [], Elem, [] - Off) :-
	reverse(On, Elem.Off).

queue__length(On - Off, Depth) :-
	length(On, DOn),
	length(Off, DOff),
	Depth is DOn + DOff.

%--------------------------------------------------------------------------%
