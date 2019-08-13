%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 sts=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module rng1.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module rng.
:- import_module rng.marsaglia.
:- import_module rng.tausworthe.

main(!IO) :-
    io.write_string("marsaglia:\n", !IO),
    make_urng(marsaglia.init, RP1, RS1),
    test(20, RP1, RS1, _, !IO),

    io.write_string("\ntausworthe3:\n", !IO),
    tausworthe.init_t3(RP2, RS2),
    test(20, RP2, RS2, _, !IO),

    io.write_string("\ntausworthe4:\n", !IO),
    tausworthe.init_t4(RP3, RS3),
    test(20, RP3, RS3, _, !IO).

:- pred test(int, RP, RS, RS, io, io) <= urng(RP, RS).
:- mode test(in, in, di, uo, di, uo) is det.

test(Count, RP, !RS, !IO) :-
    ( if Count > 0 then
        urandom(RP, N, !RS),
        io.write_uint64(N, !IO),
        io.nl(!IO),
        test(Count - 1, RP, !RS, !IO)
    else
        true
    ).
