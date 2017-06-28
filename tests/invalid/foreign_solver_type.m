%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_solver_type.

:- interface.

:- solver type foo
    where representation is int,
          ground         is ground,
          any            is ground,
          equality       is eq_foo.

:- solver type bar.

:- pragma foreign_type("C", foo, "MR_Integer").
:- pragma foreign_type("C", bar, "MR_Integer").

:- implementation.

:- pred init_foo(foo::out(any)) is det.

init_foo(foo(42)).

:- func foo(int::in) = (foo::out(any)) is det.

foo(N) = X :-
    promise_pure (
        impure X = 'representation to any foo/0'(N)
    ).

:- pred eq_foo(foo::in(any), foo::in(any)) is semidet.

eq_foo(X, Y) :-
    promise_pure (
        impure RX = 'representation of any foo/0'(X),
        impure RY = 'representation of any foo/0'(Y),
        RX = RY
    ).
