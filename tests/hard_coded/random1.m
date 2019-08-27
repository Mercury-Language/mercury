%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 sts=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module random1.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module random.
:- import_module random.sfc16.
:- import_module random.sfc32.
:- import_module random.sfc64.
:- import_module string.
:- import_module uint64.

main(!IO) :-
    io.write_string("sfc16:\n", !IO),
    make_urandom(sfc16.init, RPsfc16, RSsfc16),
    test(20, RPsfc16, RSsfc16, _, !IO),

    io.write_string("\nsfc32:\n", !IO),
    sfc32.init(RPsfc32, RSsfc32),
    test(20, RPsfc32, RSsfc32, _, !IO),

    io.write_string("\nsfc64:\n", !IO),
    sfc64.init(RPsfc64, RSsfc64),
    test(20, RPsfc64, RSsfc64, _, !IO).

:- pred test(int::in, P::in, S::di, S::uo, io::di, io::uo) is det
    <= urandom(P, S).

test(Count, RP, !RS, !IO) :-
    ( if Count > 0 then
        random.generate_uint64(RP, N, !RS),
        A = cast_to_int(N >> 32),
        B = cast_to_int(N /\ 0xffffffffu64),
        io.format("%08x%08x\n", [i(A), i(B)], !IO),
        test(Count - 1, RP, !RS, !IO)
    else
        true
    ).
