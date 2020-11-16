%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module complicated_fundeps.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module string.

main(!IO) :-
    write_string("test(1): ", !IO),
    ( if test(1) then
        write_string("yes\n", !IO)
    else
        write_string("no\n", !IO)
    ),
    write_string("test(2): ", !IO),
    ( if test(2) then
        write_string("yes\n", !IO)
    else
        write_string("no\n", !IO)
    ).

:- typeclass a(A, B, C, D) <= ((A -> B), (C -> D)) where [
    func f(A, C) = B,
    func g(A) = C
].

:- typeclass b(A, B) <= (A -> B) where [
    func h(B) = A,
    func ii = A
].

:- pred test(A) <= (a(A, B, C, D), b(B, C)).
:- mode test(in) is semidet.

test(A) :-
    C = g(A),
    B = h(C),
    f(A, C) = B.

:- instance a(int, int, int, int) where [
    (f(M, N) = M + N),
    (g(M) = M)
].

:- instance a(int, int, string, int) where [
    (f(M, S) = M + length(S)),
    (g(_) = "7")
].

:- instance b(int, int) where [
    (h(N) = N + 1),
    (ii = 2)
].
