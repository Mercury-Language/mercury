%---------------------------------------------------------------------------%
% Copyright (C) 2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: svqueue.m.
% Author: zs.
% Stability: high.

% This file provides an interface to the 'queue' ADT that is conducive to the
% use of state variable notation. The predicates here do the same thing as
% their counterparts in the queue module; the only difference is the order
% of the arguments.

%--------------------------------------------------------------------------%

:- module svqueue.

:- interface.

:- import_module list, queue.

	% `svqueue__put(Elem, Queue0, Queue)' is true iff `Queue' is
	% the queue which results from appending `Elem' onto the end
	% of `Queue0'.

:- pred svqueue__put(T::in, queue(T)::in, queue(T)::out) is det.

	% `svqueue__put_list(Elems, Queue0, Queue)' is true iff `Queue'
	% is the queue which results from inserting the items in the
	% list `Elems' into `Queue0'.

:- pred svqueue__put_list(list(T)::in, queue(T)::in, queue(T)::out) is det.

	% `svqueue__get(Elem, Queue0, Queue)' is true iff `Queue0' is
	% a non-empty queue whose first element is `Elem', and `Queue'
	% the queue which results from removing that element from
	% the front of `Queue0'.

:- pred svqueue__get(T::out, queue(T)::in, queue(T)::out) is semidet.

	% `svqueue__delete_all(Elem, Queue0, Queue)' is true iff `Queue' is
	% the same queue as `Queue0' with all occurences of `Elem' removed
	% from it.
:- pred svqueue__delete_all(T::in, queue(T)::in, queue(T)::out) is det.

%--------------------------------------------------------------------------%

:- implementation.

svqueue__put(Elem, Queue0, Queue) :-
	queue__put(Queue0, Elem, Queue).

svqueue__put_list(List, Queue0, Queue) :-
	queue__put_list(Queue0, List, Queue).

svqueue__get(Elem, Queue0, Queue) :-
	queue__get(Queue0, Elem, Queue).

svqueue__delete_all(Elem, Queue0, Queue) :-
	queue__delete_all(Queue0, Elem, Queue).
