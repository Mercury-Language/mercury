%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997-1999, 2005-2006 The University of Melbourne.
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

%--------------------------------------------------------------------------%

:- type stack(T).

	% `stack.init(Stack)' is true iff `Stack' is an empty stack.
	%
:- pred stack.init(stack(T)::out) is det.
:- func stack.init = stack(T).

	% `stack.is_empty(Stack)' is true iff `Stack' is an empty stack.
	%
:- pred stack.is_empty(stack(T)::in) is semidet.

	% `stack.is_full(Stack)' is intended to be true iff `Stack'
	% is a stack whose capacity is exhausted.  This implementation
	% allows arbitrary-sized stacks, so stack.is_full always fails.
	%
:- pred stack.is_full(stack(T)::in) is semidet.

	% `stack.push(Stack0, Elem, Stack)' is true iff `Stack' is
	% the stack which results from pushing `Elem' onto the top
	% of `Stack0'.
	%
:- pred stack.push(stack(T)::in, T::in, stack(T)::out) is det.
:- func stack.push(stack(T), T) = stack(T).

	% `stack.push_list(Stack0, Elems, Stack)' is true iff `Stack'
	% is the stack which results from pushing the elements of the
	% list `Elems' onto the top of `Stack0'.
	%
:- pred stack.push_list(stack(T)::in, list(T)::in, stack(T)::out) is det.
:- func stack.push_list(stack(T), list(T)) = stack(T).

	% `stack.top(Stack, Elem)' is true iff `Stack' is a non-empty
	% stack whose top element is `Elem'.
	%
:- pred stack.top(stack(T)::in, T::out) is semidet.

	% `stack.top_det' is like `stack.top' except that it will
	% call error/1 rather than failing if given an empty stack.
	%
:- pred stack.top_det(stack(T)::in, T::out) is det.
:- func stack.top_det(stack(T)) = T.
:- func stack.det_top(stack(T)) = T.

	% `stack.pop(Stack0, Elem, Stack)' is true iff `Stack0' is
	% a non-empty stack whose top element is `Elem', and `Stack'
	% the stack which results from popping `Elem' off `Stack0'.
	%
:- pred stack.pop(stack(T)::in, T::out, stack(T)::out) is semidet.

	% `stack.pop_det' is like `stack.pop' except that it will
	% call error/1 rather than failing if given an empty stack.
	%
:- pred stack.pop_det(stack(T)::in, T::out, stack(T)::out) is det.
:- pred stack.det_pop(stack(T)::in, T::out, stack(T)::out) is det.

	% `stack.depth(Stack, Depth)' is true iff `Stack' is a stack
	% containing `Depth' elements.
	%
:- pred stack.depth(stack(T)::in, int::out) is det.
:- func stack.depth(stack(T)) = int.

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%--------------------------------------------------------------------------%

:- type stack(T) == list(T).

stack.init([]).

stack.is_empty([]).

stack.is_full(_) :-
	semidet_fail.

stack.push(Stack, Elem, [Elem | Stack]).

stack.push_list(Stack, [], Stack).
stack.push_list(Stack0, [Elem | Elems], Stack1) :-
	stack.push(Stack0, Elem, Stack2),
	stack.push_list(Stack2, Elems, Stack1).

stack.top([Elem | _], Elem).

stack.top_det(Stack, Elem) :-
	(
        Stack = [Elem | _]
	;
        Stack = [],
		error("stack.top_det: top of empty stack")
	).

stack.pop([Elem | Stack], Elem, Stack).

stack.pop_det(Stack0, Elem, Stack) :-
	(
        Stack0 = [Elem | Stack]
	;
        Stack0 = [],
		error("stack.pop_det: pop from empty stack")
	).

stack.det_pop(Stack0, Elem, Stack) :-
	stack.pop_det(Stack0, Elem, Stack).

stack.depth(Stack, Depth) :-
	list.length(Stack, Depth).

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
% 	Function forms added.

stack.init = S :-
	stack.init(S).

stack.push(S1, X) = S2 :-
	stack.push(S1, X, S2).

stack.push_list(S1, Xs) = S2 :-
	stack.push_list(S1, Xs, S2).

stack.top_det(S) = X :-
	stack.top_det(S, X).

stack.det_top(S) = X :-
	stack.top_det(S, X).

stack.depth(S) = N :-
	stack.depth(S, N).
