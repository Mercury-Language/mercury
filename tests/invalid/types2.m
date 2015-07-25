%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module types2.
:- interface.

:- type t
    --->    a
    ;       f(t).

:- implementation.
:- pred p(pred).

:- pred q.

q :-
    p(q),
    zzzzzzzz,
    p(2),
    p.

r :-
    s.

a(X) :-
    b(X).

r :-
    r,
    a(0).

:- pred z.
z :-
    r,
    a(0).

:- pred foo.

foo :-
    bar(_).

:- pred bar(BarTypeParam).

bar(X) :-
    X = 0.

:- pred baz(BazTypeParam).

baz(X) :-
    bar(X).

:- type t(T1, T2).

:- pred bar2(t(Bar1, Bar2)).

:- pred baz2(t(Baz1, Baz2)).

baz2(X) :-
    bar2(X).

:- import_module string.
:- import_module int.

:- pred repeated_arity(int::in, int::in, int::out) is det.

repeated_arity(A, B, C) :-
    % Should report "wrong number of arguments (2; should be 3)",
    % not "wrong number of arguments (2; should be 3 or 3)"
    % as it used to (since there are two matches on the predicate name
    % "append" in scope, string.append and types2.append, both of arity 3).
    append(A, B),
    C = 42.

:- pred append(int::in, int::in, int::out) is det.

append(A, B, C) :-
    C = A + B.
