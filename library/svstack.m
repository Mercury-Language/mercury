%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: stack.m.
% Main author: fjh.
% Stability: high.
%
% This file provides an interface to the 'stack' ADT that is conducive to the
% use of state variable notation. The predicates here do the same thing as
% their counterparts in the stack module; the only difference is the order of
% the arguments.
% 
%--------------------------------------------------------------------------%

:- module svstack.
:- interface.

:- import_module list.
:- import_module stack.

%--------------------------------------------------------------------------%

    % `svstack.push(Elem, Stack0, Stack)' is true iff `Stack' is
    % the stack which results from pushing `Elem' onto the top
    % of `Stack0'.
    %
:- pragma obsolete(svstack.push/3).
:- pred svstack.push(T::in, stack(T)::in, stack(T)::out) is det.

    % `svstack.push_list(Elems, Stack0, Stack)' is true iff `Stack'
    % is the stack which results from pushing the elements of the
    % list `Elems' onto the top of `Stack0'.
    %
:- pragma obsolete(svstack.push_list/3).
:- pred svstack.push_list(list(T)::in, stack(T)::in, stack(T)::out) is det.

    % `svstack.pop(Elem, Stack0, Stack)' is true iff `Stack0' is
    % a non-empty stack whose top element is `Elem', and `Stack'
    % the stack which results from popping `Elem' off `Stack0'.
    %
:- pragma obsolete(svstack.pop/3).
:- pred svstack.pop(T::out, stack(T)::in, stack(T)::out) is semidet.

    % `svstack.det_pop' is like `svstack.pop' except that it will
    % call error/1 rather than failing if given an empty stack.
    %
:- pragma obsolete(svstack.det_pop/3).
:- pred svstack.det_pop(T::out, stack(T)::in, stack(T)::out) is det.

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- implementation.

%--------------------------------------------------------------------------%

svstack.push(E, !S) :-
    stack.push(E, !S).    

svstack.push_list(Es, !S) :-
    stack.push_list(Es, !S).

svstack.pop(E, !S) :-
    stack.pop(E, !S).

svstack.det_pop(E, !S) :-
    stack.det_pop(E, !S).

%--------------------------------------------------------------------------%
:- end_module svstack.
%--------------------------------------------------------------------------%
