%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% solver_ite_inits.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Fri Mar 18 11:17:41 EST 2005
%
% Test that the compiler inserts solver variable initialisation calls
% at the ends of if-then-else branches if necessary to ensure that solver
% variables have compatible insts at the end of the if-then-else.
%
% This test is disabled, because automatic initialization of solver variables
% is no longer supported.
%
%---------------------------------------------------------------------------%

:- module solver_ite_inits.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%---------------------------------------------------------------------------%

main(!IO) :-
    write_foo(f(a), !IO),
    write_foo(f(b), !IO),
    write_foo(f(c), !IO).

:- solver type foo
    where   representation is int,
            initialisation is init,
            ground         is ground,
            any            is ground.

:- pred init(foo::oa) is det.
:- pragma promise_pure(init/1).

init(X) :-
    impure X = 'representation to any foo/0'(0).

:- func foo(int::in) = (foo::oa) is det.
:- pragma promise_pure(foo/1).

foo(N) = X :-
    impure X = 'representation to any foo/0'(N).

:- pred write_foo(foo::ia, io::di, io::uo) is det.
:- pragma promise_pure(write_foo/3).

write_foo(Foo, !IO) :-
    impure X = 'representation of any foo/0'(Foo),
    io.print_line(X, !IO).

:- type bar
    --->    a
    ;       b
    ;       c.

:- func f(bar::in) = (foo::oa) is det.

f(Bar) = Foo :-
    ( if Bar = a then true else Foo = foo(1) ).

%---------------------------------------------------------------------------%
