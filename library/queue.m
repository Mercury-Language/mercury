%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

% File: queue.nl.
% Main author: fjh.

% This file contains a `queue' ADT.
% The current implementation is a naive version using a list.
% The predicate queue_put/3 is O(N) in the current implementation.

%--------------------------------------------------------------------------%

:- module queue.
:- interface.
:- import_module int.

:- type queue(T).

	% `queue__init(Queue)' is true iff `Queue' is an empty queue.

:- pred queue__init(queue(T)).
:- mode queue__init(out) is det.

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

:- import_module list.

:- type queue(T) == list(T).

queue__init([]).

queue__is_empty([]).

queue__is_full(_) :- fail.

queue__put(Queue0, Elem, Queue) :-
	append(Queue0, [Elem], Queue).

queue__first([Elem | _], Elem).

queue__get([Elem | Queue], Elem, Queue).

queue__length(Queue, Depth) :-
	length(Queue, Depth).

%--------------------------------------------------------------------------%
