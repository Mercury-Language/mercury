% Regression test for bug #191:
% This program caused the following compiler abort in rotd-2011-03-22:
%
%    Software Error: code_gen.m: Unexpected: semidet model in det context
%
:- module bug191.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type foo ---> a(int); b(string).

:- inst a ---> a(ground).

:- typeclass foo where [pred baz(int::in, foo::out(a)) is det].
:- instance foo where [pred(baz/2) is bar].

:- pred foo(foo::in(a), int::out) is det.
foo(a(I), I).

% should be bar(int::in, foo::out(a))
:- pred bar(int::in, foo::out) is det.
bar(_S, b("GOTCHA")).

main(!IO) :-
    baz(42, F),
    foo(F, I),
    print(I, !IO), nl(!IO).
