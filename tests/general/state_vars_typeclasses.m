%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% state_vars_typeclasses.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Fri May 31 14:28:03 EST 2002
%---------------------------------------------------------------------------%

:- module state_vars_typeclasses.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

:- typeclass foo(T) where [
    func f(T) = T,
    pred p(T::in, T::out) is det
].

:- instance foo(int) where [
    p(!X),
    f(!.X) = !:X + 1
].

main(!IO) :-
    p(1, A),
    B = f(1),
    io.format("p(1, %d).\nf(1) = %d.\n", [i(A), i(B)], !IO).
