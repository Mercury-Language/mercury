%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. The compiler inlined foo/1 without inlining
% the correct determinism, and the result was a nondet unification.
% -- bromage 30/5/1997

:- module determinism_inlining.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module int.

main(!IO) :-
    foo(X),
    io.write_int(X, !IO),
    io.nl(!IO).

:- pred foo(int :: out) is multi.

foo(42).
