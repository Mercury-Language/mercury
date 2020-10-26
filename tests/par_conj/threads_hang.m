%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% In low-level parallel grades, running with many threads could cause
% the program to deadlock.

:- module threads_hang.
:- interface.
:- import_module io.

:- impure pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    % Set a signal to go off if the program is taking too long.
    % The default SIGALRM handler will abort the program.
    impure alarm(10),

    fib(7, F),
    fib(6, G),
    io.write_int(F, !IO),
    io.nl(!IO),
    io.write_int(G, !IO),
    io.nl(!IO),
    io.flush_output(!IO).

:- pred fib(int::in, int::out) is det.

fib(N, F) :-
    ( if N < 2 then
        F = 1
    else
        ( fib(N-1, F1)
        & fib(N-2, F2)
        ),
        F = F1 + F2
    ).

:- pragma foreign_decl("C", "#include <unistd.h>").

:- impure pred alarm(int::in) is det.

:- pragma foreign_proc("C",
    alarm(Seconds::in),
    [will_not_call_mercury],
"
    alarm(Seconds);
").
