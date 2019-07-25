%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for bug #191:
% This program caused the following compiler abort in rotd-2011-03-22:
%
%    Software Error: code_gen.m: Unexpected: semidet model in det context
%
%---------------------------------------------------------------------------%

:- module bug191.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type foo
    --->    a(int)
    ;       b(string).

:- inst a
    ---> a(ground).

:- typeclass foo(T) where [pred baz(T::in, int::in, foo::out(a)) is det].
:- instance foo(int) where [pred(baz/3) is bar].

:- pred foo(foo::in(a), int::out) is det.
foo(a(I), I).

% should be bar(int::in, foo::out(a))
:- pred bar(int::in, int::in, foo::out) is det.
bar(_, _S, b("GOTCHA")).

main(!IO) :-
    baz(561, 42, F),
    foo(F, I),
    io.print(I, !IO),
    io.nl(!IO).
