% vim: ft=mercury ts=4 sw=4 et

:- module fib_par_plain.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

main(!IO) :-
    N = 46,
    fib(N, F),
    io.format("fib(%d) = %d\n", [i(N), i(F)], !IO).

:- pred fib(int::in, int::out) is det.

fib(N, F) :-
    ( if N < 0 then
        error("fib(N, _): N must be greater than 0")
    else if ( N = 0 ; N = 1 ) then
        F = 1
    else
        fib(N-1, FA),
        fib(N-2, FB),
        F = FA + FB
    ).

