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
:- import_module rng.sfc.
:- import_module rng.tausworthe.

main(!IO) :-
    io.write_string("marsaglia:\n", !IO),
    make_urng(marsaglia.init, RPm, RSm),
    test(20, RPm, RSm, _, !IO),

    io.write_string("\nsfc16:\n", !IO),
    make_urng(sfc.init16, RPsfc16, RSsfc16),
    test(20, RPsfc16, RSsfc16, _, !IO),

    io.write_string("\nsfc32:\n", !IO),
    sfc.init32(RPsfc32, RSsfc32),
    test(20, RPsfc32, RSsfc32, _, !IO),

    io.write_string("\nsfc:\n", !IO),
    sfc.init(RPsfc, RSsfc),
    test(20, RPsfc, RSsfc, _, !IO),

    io.write_string("\ntausworthe3:\n", !IO),
    tausworthe.init_t3(RPt3, RSt3),
    test(20, RPt3, RSt3, _, !IO),

    io.write_string("\ntausworthe4:\n", !IO),
    tausworthe.init_t4(RPt4, RSt4),
    test(20, RPt4, RSt4, _, !IO).

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
