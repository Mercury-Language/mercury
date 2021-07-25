%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module func_call.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module library_forwarding.

main(!IO) :-
    io.write_int(fib(6), !IO),
    io.nl(!IO).

:- func fib(int) = int.

fib(N) =
    ( if N =< 1 then
        1
    else
        fib(N - 1) +
          fib(N - 3)    % Oops.
    ).
