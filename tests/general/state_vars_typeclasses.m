%------------------------------------------------------------------------------%
% state_vars_typeclasses.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Fri May 31 14:28:03 EST 2002
% vim: ft=mercury ff=unix ts=4 sw=4 et wm=0 tw=0
%
%------------------------------------------------------------------------------%

:- module state_vars_typeclasses.

:- interface.

:- import_module io.


:- pred main(io::di, io::uo) is det.


:- implementation.

:- import_module int, string, list.


:- typeclass foo(T) where [ func f(T) = T, pred p(T::in, T::out) is det ].

:- instance foo(int) where [
    p(!X),
    f(!.X) = !:X + 1
].


main(!IO) :-
    p(1, A),
    B = f(1),
    format("p(1, %d).\nf(1) = %d.\n", [i(A), i(B)], !IO).
