%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module switches.

:- interface.

:- type t
    --->    a
    ;       b
    ;       c.
:- type f
    --->    f(t).

:- implementation.

    % Test basic first-argument indexing.
    %
:- pred first_arg(t::in, int::out) is det.

first_arg(a, 1).
first_arg(b, 2).
first_arg(c, 3).

    % Test second-argument indexing.
    %
:- pred second_arg(int::out, t::in) is det.

second_arg(1, a).
second_arg(2, b).
second_arg(3, c).

    % Test indexing of explicit disjunctions.
    %
:- pred explict_disj(t::in, int::out) is det.

explict_disj(X, Y) :-
    (
        X = a, Y = 1
    ;
        X = b, Y = 2
    ;
        X = c, Y = 3
    ).

    % Test chained indexing.
    %
:- pred chained(t::in, int::out) is det.

chained(X, Y) :-
    X = X1, X1 = a, Y = 1.
chained(X, Y) :-
    X = X2, X2 = b, Y = 2.
chained(X, Y) :-
    X = X3, X3 = c, Y = 3.

    % Test multi-level indexing.
    %
:- pred multi_level(f::in, int::out) is det.

multi_level(f(a), 1).
multi_level(f(b), 2).
multi_level(f(c), 3).
