%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ho5.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module exception.

main(!IO) :-
    try(p(1), R1),
    io.write_line(R1, !IO),
    try(p(2), R2),
    io.write_line(R2, !IO).

:- pred p(int::in, int::out) is det.

p(!N) :-
    q(!N),
    r(!N).

:- pred q(int::in, int::out) is det.

q(_, 0).

:- pred r(int::in, int::out) is det.

r(M, N) :-
    ( if M = 0 then
        throw(zero)
    else
        N = 0
    ).

:- func zero(int) = string.

zero(_) = "zero".
