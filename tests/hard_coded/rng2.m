%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 sts=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module rng2.
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
    RNG1 = marsaglia.init,
    test(20, RNG1, _, !IO),

    io.write_string("\ntausworthe3:\n", !IO),
    tausworthe.init_t3(RP2, RS2),
    RNG2 = make_shared_rng(RP2, RS2),
    test(20, RNG2, _, !IO),

    io.write_string("\ntausworthe4:\n", !IO),
    tausworthe.init_t4(RP3, RS3),
    RNG3 = make_shared_rng(RP3, RS3),
    test(20, RNG3, _, !IO).

:- pred test(int, RNG, RNG, io, io) <= rng(RNG).
:- mode test(in, in, out, di, uo) is det.

test(Count, !RNG, !IO) :-
    ( if Count > 0 then
        random(N, !RNG),
        io.write_uint64(N, !IO),
        io.nl(!IO),
        test(Count - 1, !RNG, !IO)
    else
        true
    ).
