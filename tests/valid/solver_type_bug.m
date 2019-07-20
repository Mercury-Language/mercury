%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test.
% Previous versions of the Mercury compiler would
% incorrectly infer the determinism of p2 as semidet,
% resulting in a warning (and invalid code generation).

:- module solver_type_bug.
:- interface.

:- solver type foo.

:- pred eq_foo(foo::in(any), foo::in(any)) is semidet.

:- type bar
    --->    bar(foo).

:- pred p1(foo).
:- mode p1(in(any)) is nondet.

:- pred p2(bar).
:- mode p2(in(any)) is nondet.

:- implementation.

:- solver type foo
    where representation is int,
          ground         is ground,
          any            is ground,
          equality       is eq_foo.

eq_foo(X, Y) :-
    promise_pure (
        impure RX = 'representation of any foo/0'(X),
        impure RY = 'representation of any foo/0'(Y),
        RX = RY
    ).

p1(X) :-
    q1(X).

p2(X) :-
    q2(X).

:- pred q1(foo).
:- mode q1(in(any)) is nondet.
:- pragma external_pred(q1/1).

:- pred q2(bar).
:- mode q2(in(any)) is nondet.
:- pragma external_pred(q2/1).

:- pragma foreign_code("Erlang", "
q1_1_p_0(_, _) -> void.
q2_1_p_0(_, _) -> void.
").

:- pragma foreign_code("Java", "
    private static void q1_1_p_0(int a1, jmercury.runtime.MethodPtr a2, java.lang.Object a3) {}
    private static void q2_1_p_0(Bar_0 a1, jmercury.runtime.MethodPtr a2, java.lang.Object a3) {}
").
