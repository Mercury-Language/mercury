%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module catch_retry.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module exception.
:- import_module int.

main(!IO) :-
    p(1, R1),
    io.write_line(R1, !IO),
    p(2, R2),
    io.write_line(R2, !IO).

:- pred p(int::in, exception_result(int)::out) is cc_multi.

p(N, R) :-
    try(q(N), R).

:- pred q(int::in, int::out) is det.

q(N, M) :-
    ( if N > 1 then
        M = N
    else
        throw("q: bad input")
    ).
