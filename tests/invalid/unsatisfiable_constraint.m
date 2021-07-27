%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unsatisfiable_constraint.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module string.

main(!IO) :-
    io.write_string("test(1): ", !IO),
    ( if test(1) then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ),
    io.write_string("test(2): ", !IO),
    ( if test(2) then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- typeclass a(A, B, C, D) <= ((A -> B), (C -> D)) where [
    func f(A, C) = B,
    func g(A) = C
].

:- typeclass b(A, B) <= (A -> B) where [
    func h(A) = B,
    func ii = A
].

:- pred test(A) <= (a(A, B, C, D), b(B, C)).
:- mode test(in) is semidet.

test(A) :-
    B = g(A),
    C = h(B),       % unsatisfiable!
    f(A, C) = ii.

:- instance a(int, int, int, int) where [
    (f(M, N) = M + N),
    (g(M) = -M)
].

:- instance a(int, int, string, int) where [
    (f(M, S) = M + length(S)),
    (g(_) = "")
].

:- instance b(int, int) where [
    (h(N) = -N),
    (ii = 2)
].
