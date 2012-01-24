%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997-1999, 2005-2006, 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: stack.m.
% Main author: fjh.
% Stability: high.
% 
% This file contains a `stack' ADT.
% Stacks are implemented here using lists.
% 
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

	% `stack.push(Elem, Stack0, Stack)' is true iff `Stack' is
	% the stack which results from pushing `Elem' onto the top
	% of `Stack0'.
	%
:- pred stack.push(T::in, stack(T)::in, stack(T)::out) is det.
:- func stack.push(stack(T), T) = stack(T).

	% `stack.push_list(Elems, Stack0, Stack)' is true iff `Stack'
	% is the stack which results from pushing the elements of the
	% list `Elems' onto the top of `Stack0'.
	%
:- pred stack.push_list(list(T)::in, stack(T)::in, stack(T)::out) is det.
:- func stack.push_list(stack(T), list(T)) = stack(T).

	% `stack.top(Stack, Elem)' is true iff `Stack' is a non-empty
	% stack whose top element is `Elem'.
	%
:- pred stack.top(stack(T)::in, T::out) is semidet.

	% `stack.det_top' is like `stack.top' except that it will
	% call error/1 rather than failing if given an empty stack.
	%
:- pred stack.det_top(stack(T)::in, T::out) is det.
:- func stack.det_top(stack(T)) = T.

	% `stack.pop(Elem, Stack0, Stack)' is true iff `Stack0' is
	% a non-empty stack whose top element is `Elem', and `Stack'
	% the stack which results from popping `Elem' off `Stack0'.
	%
:- pred stack.pop(T::out, stack(T)::in, stack(T)::out) is semidet.

	% `stack.det_pop' is like `stack.pop' except that it will
	% call error/1 rather than failing if given an empty stack.
	%
:- pred stack.det_pop(T::out, stack(T)::in, stack(T)::out) is det.

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

:- type stack(T)
    --->    stack(list(T)).

stack.init = Stack :-
	stack.init(Stack).

stack.init(stack([])).

stack.is_empty(stack([])).

stack.is_full(_) :-
	semidet_fail.

stack.push(!.Stack, X) = !:Stack :-
	stack.push(X, !Stack).

stack.push(Elem, !Stack) :-
    !.Stack = stack(Elems),
    !:Stack = stack([Elem | Elems]).

stack.push_list(!.Stack, Xs) = !:Stack :-
	stack.push_list(Xs, !Stack).

stack.push_list([], !Stack).
stack.push_list([Elem | Elems], !Stack) :-
	stack.push(Elem, !Stack),
	stack.push_list(Elems, !Stack).

stack.top(stack([Elem | _]), Elem).

stack.det_top(Stack) = X :-
	stack.det_top(Stack, X).

stack.det_top(Stack, Elem) :-
	(
        Stack = stack([Elem | _])
	;
        Stack = stack([]),
		error("stack.det_top: top of empty stack")
	).

stack.pop(Elem, !Stack) :-
    !.Stack = stack([Elem | Elems]),
    !:Stack = stack(Elems).

stack.det_pop( Elem, !Stack) :-
	(
        !.Stack = stack([Elem | Elems]),
        !:Stack = stack(Elems)
	;
        !.Stack = stack([]),
		error("stack.det_pop: pop from empty stack")
	).

stack.depth(Stack) = Depth :-
	stack.depth(Stack, Depth).

stack.depth(Stack, Depth) :-
    Stack = stack(Elems),
	list.length(Elems, Depth).

%--------------------------------------------------------------------------%
:- end_module stack.
%--------------------------------------------------------------------------%
