%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 sts=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module random3.
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
    make_io_random(sfc16.init, Msfc16, !IO),
    test(20, Msfc16, !IO),

    io.write_string("\nsfc32:\n", !IO),
    sfc32.init(Psfc32, Ssfc32),
    make_io_urandom(Psfc32, Ssfc32, Msfc32, !IO),
    test(20, Msfc32, !IO),

    io.write_string("\nsfc64:\n", !IO),
    sfc64.init(Psfc64, Ssfc64),
    make_io_urandom(Psfc64, Ssfc64, Msfc64, !IO),
    test(20, Msfc64, !IO).

:- pred test(int::in, M::in, io::di, io::uo) is det <= urandom(M, io).

test(Count, M, !IO) :-
    ( if Count > 0 then
        random.generate_uint64(M, N, !IO),
        A = cast_to_int(N >> 32),
        B = cast_to_int(N /\ 0xffffffffu64),
        io.format("%08x%08x\n", [i(A), i(B)], !IO),
        test(Count - 1, M, !IO)
    else
        true
    ).

