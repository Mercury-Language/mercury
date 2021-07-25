%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ite_2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module library_forwarding.

main(!IO) :-
    ite(a, 1, M),
    ite(b, 1, N),
    io.write_string("ite(a, 1, ", !IO),
    io.write_int(M, !IO),
    io.write_string(").\nite(b, 1, ", !IO),
    io.write_int(N, !IO),
    io.write_string(").\n\n", !IO).

:- pred ite(pred(int, int), int, int).
:- mode ite(in(pred(in, out) is semidet), in, out) is det.
:- mode ite(in(pred(in, out) is nondet), in, out) is multi.

ite(P, X, Y) :-
    ( if
        P(X, Z),
        Z > 1
    then
        Y = Z
    else
        c(X, Y)
    ).

:- pred a(int::in, int::out) is semidet.

a(1, 1).

:- pred b(int::in, int::out) is nondet.

b(1, 0).
b(1, 1).

:- pred c(int::in, int::out) is det.

c(X, X).
