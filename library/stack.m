%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

% File: stack.nl.
% Main author: fjh.

% This file contains a `stack' ADT.
% Stacks are implemented here using lists.

%--------------------------------------------------------------------------%

:- module stack.
:- interface.
:- import_module int.

:- type stack(T).

	% `stack__init(Stack)' is true iff `Stack' is an empty stack.

:- pred stack__init(stack(T)).
:- mode stack__init(out) is det.

	% `stack__is_empty(Stack)' is true iff `Stack' is an empty stack.

:- pred stack__is_empty(stack(T)).
:- mode stack__is_empty(in) is semidet.

	% `stack__is_full(Stack)' is intended to be true iff `Stack'
	% is a stack whose capacity is exhausted.  This
	% implement allows arbitrary-sized stacks, so stack__is_full
	% always fails.

:- pred stack__is_full(stack(T)).
:- mode stack__is_full(in) is semidet.

	% `stack__push(Stack0, Elem, Stack)' is true iff `Stack' is
	% the stack which results from pushing `Elem' onto the top
	% of `Stack0'.

:- pred stack__push(stack(T), T, stack(T)).
:- mode stack__push(in, in, out) is det.

	% `stack__top(Stack, Elem)' is true iff `Stack' is a non-empty
	% stack whose top element is `Elem'.

:- pred stack__top(stack(T), T).
:- mode stack__top(in, out) is semidet.

	% `stack__push(Stack0, Elem, Stack)' is true iff `Stack0' is
	% a non-empty stack whose top element is `Elem', and `Stack'
	% the stack which results from popping `Elem' off `Stack0'.

:- pred stack__pop(stack(T), T, stack(T)).
:- mode stack__pop(in, out, out) is semidet.

	% `stack__depth(Stack, Depth)' is true iff `Stack' is a stack
	% containing `Depth' elements.

:- pred stack__depth(stack(T), int).
:- mode stack__depth(in, out) is det.

%--------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type stack(T) == list(T).

stack__init([]).

stack__is_empty([]).

stack__is_full(_) :- fail.

stack__push(Stack, Elem, [Elem | Stack]).

stack__top([Elem | _], Elem).

stack__pop([Elem | Stack], Elem, Stack).

stack__depth(Stack, Depth) :-
	list__length(Stack, Depth).

%--------------------------------------------------------------------------%
