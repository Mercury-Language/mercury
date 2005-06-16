%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997-1999, 2005 The University of Melbourne.
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

:- type stack(T).

	% `stack__init(Stack)' is true iff `Stack' is an empty stack.
	%
:- pred stack__init(stack(T)::out) is det.
:- func stack__init = stack(T).

	% `stack__is_empty(Stack)' is true iff `Stack' is an empty stack.
	%
:- pred stack__is_empty(stack(T)::in) is semidet.

	% `stack__is_full(Stack)' is intended to be true iff `Stack'
	% is a stack whose capacity is exhausted.  This implementation
	% allows arbitrary-sized stacks, so stack__is_full always fails.
	%
:- pred stack__is_full(stack(T)::in) is semidet.

	% `stack__push(Stack0, Elem, Stack)' is true iff `Stack' is
	% the stack which results from pushing `Elem' onto the top
	% of `Stack0'.
	%
:- pred stack__push(stack(T)::in, T::in, stack(T)::out) is det.
:- func stack__push(stack(T), T) = stack(T).

	% `stack__push_list(Stack0, Elems, Stack)' is true iff `Stack' 
	% is the stack which results from pushing the elements of the
	% list `Elems' onto the top of `Stack0'.
	%
:- pred stack__push_list(stack(T)::in, list(T)::in, stack(T)::out) is det.
:- func stack__push_list(stack(T), list(T)) = stack(T).

	% `stack__top(Stack, Elem)' is true iff `Stack' is a non-empty
	% stack whose top element is `Elem'.
	%
:- pred stack__top(stack(T)::in, T::out) is semidet.

	% `stack__top_det' is like `stack__top' except that it will
	% call error/1 rather than failing if given an empty stack.
	%
:- pred stack__top_det(stack(T)::in, T::out) is det.
:- func stack__top_det(stack(T)) = T.
:- func stack__det_top(stack(T)) = T.

	% `stack__pop(Stack0, Elem, Stack)' is true iff `Stack0' is
	% a non-empty stack whose top element is `Elem', and `Stack'
	% the stack which results from popping `Elem' off `Stack0'.
	%
:- pred stack__pop(stack(T)::in, T::out, stack(T)::out) is semidet.

	% `stack__pop_det' is like `stack__pop' except that it will
	% call error/1 rather than failing if given an empty stack.
	%
:- pred stack__pop_det(stack(T)::in, T::out, stack(T)::out) is det.
:- pred stack__det_pop(stack(T)::in, T::out, stack(T)::out) is det.

	% `stack__depth(Stack, Depth)' is true iff `Stack' is a stack
	% containing `Depth' elements.
	%
:- pred stack__depth(stack(T)::in, int::out) is det.
:- func stack__depth(stack(T)) = int.

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module std_util.

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

stack__top_det(Stack, Elem) :-
	( Stack = [Elem1 | _] ->
		Elem = Elem1
	;
		error("stack__top_det: top of empty stack")
	).

stack__pop([Elem | Stack], Elem, Stack).

stack__pop_det(Stack0, Elem, Stack) :-
	( Stack0 = [Elem1 | Stack1] ->
		Elem = Elem1,
		Stack = Stack1
	;
		error("stack__pop_det: pop from empty stack")
	).

stack__det_pop(Stack0, Elem, Stack) :-
	stack__pop_det(Stack0, Elem, Stack).

stack__depth(Stack, Depth) :-
	list__length(Stack, Depth).

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
% 	Function forms added.

stack__init = S :-
	stack__init(S).

stack__push(S1, X) = S2 :-
	stack__push(S1, X, S2).

stack__push_list(S1, Xs) = S2 :-
	stack__push_list(S1, Xs, S2).

stack__top_det(S) = X :-
	stack__top_det(S, X).

stack__det_top(S) = X :-
	stack__top_det(S, X).

stack__depth(S) = N :-
	stack__depth(S, N).


