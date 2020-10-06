%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for platforms with posix_spawn.
% The .exp2 file is for platforms without posix_spawn.
%
%---------------------------------------------------------------------------%

:- module list_cmd.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
    fib(5, N),
    io.write_int(N, !IO),
    io.nl(!IO).

:- pred fib(int::in, int::out) is det.

fib(N, F) :-
    ( if N < 2 then
        F = 1
    else
        fib(N - 1, F1),
        fib(N - 2, F2),
        F = F1 + F2
    ).
