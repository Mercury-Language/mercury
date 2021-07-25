%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module shallow.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module shallow_2.

main(!IO) :-
    test(p("t1", 5), P1),
    test(p("t2", 37), P2),
    test(q("t3", 2), Q1),
    test(q("t4", -1), Q2),
    test(r("t5", 3), R),
    io.write_int(P1, !IO),
    io.nl(!IO),
    io.write_int(P2, !IO),
    io.nl(!IO),
    io.write_int(Q1, !IO),
    io.nl(!IO),
    io.write_int(Q2, !IO),
    io.nl(!IO),
    io.write_int(R, !IO),
    io.nl(!IO).

:- pred test(pred(int)::in(pred(out) is det), int::out) is det.

test(P, N) :-
    P(N).
