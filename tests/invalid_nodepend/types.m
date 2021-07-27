%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module types.

:- type t
    --->    a
    ;       a
    ;       f(t)
    ;       f(t).

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
