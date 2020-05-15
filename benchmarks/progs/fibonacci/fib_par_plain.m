
:- module fib_par_plain.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list, string, require.

main(!IO) :-
    N = 46,
    fib(N, F),
    io.format("fib(%d) = %d\n", [i(N), i(F)], !IO).

:- pred fib(int::in, int::out) is det.

fib(N, F) :-
    ( N < 0 ->
        error("fib(N, _): N must be greater than 0")
    ; ( N = 0 ; N = 1 ) ->
        F = 1
    ;
        fib(N-1, FA),
        fib(N-2, FB),
        F = FA + FB
    ).

