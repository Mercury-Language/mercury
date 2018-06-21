%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997-1999, 2005-2006, 2011-2012 The University of Melbourne.
% Copyright (C) 2014-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: stack.m.
% Main author: fjh.
% Stability: high.
%
% This file contains a `stack' ADT.
% Stacks are implemented here using lists.
%
%---------------------------------------------------------------------------%

:- module stack.
:- interface.
:- import_module list.

%---------------------------------------------------------------------------%

:- type stack(T).

    % init = Stack:
    % init(Stack):
    %
    % True iff Stack is an empty stack.
    %
:- func init = stack(T).
:- pred init(stack(T)::out) is det.

    % is_empty(Stack):
    %
    % True iff Stack is an empty stack.
    %
:- pred is_empty(stack(T)::in) is semidet.

    % is_full(Stack):
    %
    % This is intended to be true iff Stack is a stack whose capacity
    % is exhausted. This implementation allows arbitrary-sized stacks,
    % so is_full always fails.
    %
:- pred is_full(stack(T)::in) is semidet.

    % push(Stack0, Elem) = Stack:
    % push(Elem, Stack0, Stack):
    %
    % True iff Stack is the stack which results from pushing Elem
    % onto the top of Stack0.
    %
:- func push(stack(T), T) = stack(T).
:- pred push(T::in, stack(T)::in, stack(T)::out) is det.

    % push_list(Stack0, Elems) = Stack:
    % push_list(Elems, Stack0, Stack):
    %
    % True iff Stack is the stack which results from pushing the elements of
    % the list Elems onto the top of Stack0.
    %
:- func push_list(stack(T), list(T)) = stack(T).
:- pred push_list(list(T)::in, stack(T)::in, stack(T)::out) is det.

    % top(Stack, Elem):
    %
    % True iff Stack is a non-empty stack whose top element is Elem.
    %
:- pred top(stack(T)::in, T::out) is semidet.

    % det_top is like top except that it will call error/1 rather than
    % failing if given an empty stack.
    %
:- func det_top(stack(T)) = T.
:- pred det_top(stack(T)::in, T::out) is det.

    % pop(Elem, Stack0, Stack):
    %
    % True iff Stack0 is a non-empty stack whose top element is Elem,
    % and Stack the stack which results from popping Elem off Stack0.
    %
:- pred pop(T::out, stack(T)::in, stack(T)::out) is semidet.

    % det_pop is like pop except that it will call error/1 rather than
    % failing if given an empty stack.
    %
:- pred det_pop(T::out, stack(T)::in, stack(T)::out) is det.

    % depth(Stack) = Depth:
    % depth(Stack, Depth):
    %
    % True iff Stack is a stack containing Depth elements.
    %
:- func depth(stack(T)) = int.
:- pred depth(stack(T)::in, int::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%---------------------------------------------------------------------------%

:- type stack(T)
    --->    stack(list(T)).

init = Stack :-
    stack.init(Stack).

init(stack([])).

is_empty(stack([])).

is_full(_) :-
    semidet_fail.

push(!.Stack, X) = !:Stack :-
    stack.push(X, !Stack).

push(Elem, !Stack) :-
    !.Stack = stack(Elems),
    !:Stack = stack([Elem | Elems]).

push_list(!.Stack, Xs) = !:Stack :-
    stack.push_list(Xs, !Stack).

push_list([], !Stack).
push_list([Elem | Elems], !Stack) :-
    stack.push(Elem, !Stack),
    stack.push_list(Elems, !Stack).

top(stack([Elem | _]), Elem).

det_top(Stack) = X :-
    stack.det_top(Stack, X).

det_top(Stack, Elem) :-
    (
        Stack = stack([Elem | _])
    ;
        Stack = stack([]),
        unexpected($pred, "top of empty stack")
    ).

pop(Elem, !Stack) :-
    !.Stack = stack([Elem | Elems]),
    !:Stack = stack(Elems).

det_pop( Elem, !Stack) :-
    (
        !.Stack = stack([Elem | Elems]),
        !:Stack = stack(Elems)
    ;
        !.Stack = stack([]),
        unexpected($pred, "pop from empty stack")
    ).

depth(Stack) = Depth :-
    stack.depth(Stack, Depth).

depth(Stack, Depth) :-
    Stack = stack(Elems),
    list.length(Elems, Depth).

%---------------------------------------------------------------------------%
:- end_module stack.
%---------------------------------------------------------------------------%
