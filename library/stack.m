%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: stack.m.
% Main author: fjh.
% Stability: high.

% This file contains a `stack' ADT.
% Stacks are implemented here using lists.

%--------------------------------------------------------------------------%

:- module stack.
:- interface.
:- import_module list.

:- type stack(_T).

	% `stack__init(Stack)' is true iff `Stack' is an empty stack.

:- pred stack__init(stack(_T)).
:- mode stack__init(out) is det.

	% `stack__is_empty(Stack)' is true iff `Stack' is an empty stack.

:- pred stack__is_empty(stack(_T)).
:- mode stack__is_empty(in) is semidet.

	% `stack__is_full(Stack)' is intended to be true iff `Stack'
	% is a stack whose capacity is exhausted.  This
	% implement allows arbitrary-sized stacks, so stack__is_full
	% always fails.

:- pred stack__is_full(stack(_T)).
:- mode stack__is_full(in) is semidet.

	% `stack__push(Stack0, Elem, Stack)' is true iff `Stack' is
	% the stack which results from pushing `Elem' onto the top
	% of `Stack0'.

:- pred stack__push(stack(T), T, stack(T)).
:- mode stack__push(in, in, out) is det.

	% `stack__push_list(Stack0, Elems, Stack)' is true iff `Stack' 
	% is the stack which results from pushing the elements of the
	% list `Elems' onto the top of `Stack0'.

:- pred stack__push_list(stack(T), list(T), stack(T)).
:- mode stack__push_list(in, in, out) is det.

	% `stack__top(Stack, Elem)' is true iff `Stack' is a non-empty
	% stack whose top element is `Elem'.

:- pred stack__top(stack(T), T).
:- mode stack__top(in, out) is semidet.

	% `stack__pop(Stack0, Elem, Stack)' is true iff `Stack0' is
	% a non-empty stack whose top element is `Elem', and `Stack'
	% the stack which results from popping `Elem' off `Stack0'.

:- pred stack__pop(stack(T), T, stack(T)).
:- mode stack__pop(in, out, out) is semidet.

	% `stack__pop_det' is like `stack__pop' except that it will
	% call error/1 rather than failing if given an empty stack.

:- pred stack__pop_det(stack(T), T, stack(T)).
:- mode stack__pop_det(in, out, out) is det.

	% `stack__depth(Stack, Depth)' is true iff `Stack' is a stack
	% containing `Depth' elements.

:- pred stack__depth(stack(_T), int).
:- mode stack__depth(in, out) is det.
:- mode stack__depth(in, in) is semidet. % implied

%--------------------------------------------------------------------------%

:- implementation.

:- import_module list, require, std_util.

:- type stack(T) == list(T).

stack__init([]).

stack__is_empty([]).

stack__is_full(_) :-
	semidet_fail.

stack__push(Stack, Elem, [Elem | Stack]).

stack__push_list(Stack, [], Stack).
stack__push_list(Stack0, [Elem | Elems], Stack1) :-
	stack__push(Stack0, Elem, Stack2),
	stack__push_list(Stack2, Elems, Stack1).

stack__top([Elem | _], Elem).

stack__pop([Elem | Stack], Elem, Stack).

stack__pop_det(Stack0, Elem, Stack) :-
	( Stack0 = [Elem1 | Stack1] ->
		Elem = Elem1,
		Stack = Stack1
	;
		error("stack__pop_det: pop from empty stack")
	).

stack__depth(Stack, Depth) :-
	list__length(Stack, Depth).

%--------------------------------------------------------------------------%
