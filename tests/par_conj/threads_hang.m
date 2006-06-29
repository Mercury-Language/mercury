% In low-level parallel grades, running with many threads could cause
% the program to deadlock.

:- module threads_hang.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    fib(11, F),
    io.write_int(F, !IO),
    io.nl(!IO).

:- pred fib(int::in, int::out) is det.

fib(N, F) :-
    (if N < 2 then
        F = 1
    else
        ( fib(N-1, F1)
        & fib(N-2, F2)
        ),
        F = F1 + F2
    ).
