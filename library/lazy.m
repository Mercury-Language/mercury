%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999, 2006, 2009-2010 The University of Melbourne.
% Copyright (C) 2013-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: lazy.m.
% Main authors: fjh, pbone.
% Stability: medium.
%
% Provides support for optional explicit lazy evaluation.
%
% This module provides the data type lazy(T) and the functions `val',
% `delay', and `force', which can be used to emulate lazy evaluation.
%
% A field within a data structure can be made lazy by wrapping it within
% a lazy type. Or a lazy data structure can be implemented, for example:
%
% :- type lazy_list(T)
%     --->    lazy_list(
%                 lazy(list_cell(T))
%             ).
%
% :- type list_cell(T)
%     --->    cons(T, lazy_list(T))
%     ;       nil.
%
% Note that this makes every list cell lazy, whereas:
%
%   lazy(list(T))
%
% uses only one thunk for the entire list. And:
%
%   list(lazy(T))
%
% uses one thunk for every element, but the list's structure is not lazy.
%
%---------------------------------------------------------------------------%

:- module lazy.
:- interface.

    % A lazy(T) is a value of type T which will only be evaluated on demand.
    %
:- type lazy(T).

    % Convert a value from type T to lazy(T).
    %
:- func val(T) = lazy(T).

    % Construct a lazily-evaluated lazy(T) from a closure.
    %
:- func delay((func) = T) = lazy(T).

    % Force the evaluation of a lazy(T), and return the result as type T.
    % Note that if the type T may itself contain subterms of type lazy(T),
    % as is the case when T is a recursive type, those subterms will not be
    % evaluated -- force/1 only forces evaluation of the lazy/1 term at
    % the top level.
    %
    % A second call to force will not re-evaluate the lazy expression, it
    % will simply return T.
    %
:- func force(lazy(T)) = T.

    % Get the value of a lazy expression if it has already been made available
    % with force/1. This is useful as it can provide information without
    % incurring (much) cost.
    %
:- impure pred read_if_val(lazy(T)::in, T::out) is semidet.

    % Test lazy values for equality.
    %
:- pred equal_values(lazy(T)::in, lazy(T)::in) is semidet.

:- pred compare_values(comparison_result::uo, lazy(T)::in, lazy(T)::in) is det.

%---------------------------------------------------------------------------%
%
% The declarative semantics of the above constructs are given by the
% following equations:
%
%   val(X) = delay((func) = X).
%
%   force(delay(F)) = apply(F).
%
% The operational semantics satisfy the following:
%
% - val/1 and delay/1 both take O(1) time and use O(1) additional space.
%   In particular, delay/1 does not evaluate its argument using apply/1.
%
% - When force/1 is first called for a given term, it uses apply/1 to
%   evaluate the term, and then saves the result computed by destructively
%   modifying its argument; subsequent calls to force/1 on the same term
%   will return the same result.  So the time to evaluate force(X), where
%   `X = delay(F)', is O(the time to evaluate `apply(F)') for the first call,
%   and O(1) time for subsequent calls.
%
% - Equality on values of type lazy(T) is implemented by calling force/1
%   on both arguments and comparing the results.  So if X and Y have type
%   lazy(T), and both X and Y are ground, then the time to evaluate
%   `X = Y' is O(the time to evaluate `X1 = force(X)' + the time to evaluate
%   `Y1 = force(Y)' + the time to unify X1 and Y1).
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mutvar.

:- type lazy(T)
    --->    lazy(mutvar(lazy_state(T)))
    where equality is equal_values,
          comparison is compare_values.

    % Note that we use a user-defined equality predicate to ensure
    % that unifying two lazy(T) values will do the right thing.
    %
:- type lazy_state(T)
    --->    value(T)
    ;       closure((func) = T).

%---------------------------------------------------------------------------%

val(X) = lazy(Mutvar) :-
    promise_pure (
        impure new_mutvar(value(X), Mutvar)
    ).

delay(F) = lazy(Mutvar) :-
    promise_pure (
        impure new_mutvar(closure(F), Mutvar)
    ).

%---------------------------------------------------------------------------%

force(Lazy) = Value :-
    % The promise_equivalent_solutions scope is needed to tell the compiler
    % that force will return equal answers given arguments that are equal
    % but that have different representations.
    promise_equivalent_solutions [Mutvar] (
        Lazy = lazy(Mutvar)
    ),
    promise_pure (
        impure get_mutvar(Mutvar, State),
        (
            State = value(Value)
        ;
            State = closure(Thunk),
            Value = apply(Thunk),
            impure set_mutvar(Mutvar, value(Value))
        )
    ).

%---------------------------------------------------------------------------%

read_if_val(Lazy, Value) :-
    promise_equivalent_solutions [Mutvar] (
        Lazy = lazy(Mutvar)
    ),
    impure get_mutvar(Mutvar, State),
    State = value(Value).

%---------------------------------------------------------------------------%

equal_values(X, Y) :-
    force(X) = force(Y).

compare_values(R, X, Y) :-
    compare(R, force(X), force(Y)).

%---------------------------------------------------------------------------%
:- end_module lazy.
%---------------------------------------------------------------------------%
