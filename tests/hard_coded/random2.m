%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 sts=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module random2.
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
    Rsfc16 = sfc16.init,
    test(20, Rsfc16, _, !IO),

    io.write_string("\nsfc32:\n", !IO),
    sfc32.init(Psfc32, Ssfc32),
    Rsfc32 = make_shared_random(Psfc32, Ssfc32),
    test(20, Rsfc32, _, !IO),

    io.write_string("\nsfc64:\n", !IO),
    sfc64.init(Psfc64, Ssfc64),
    Rsfc64 = make_shared_random(Psfc64, Ssfc64),
    test(20, Rsfc64, _, !IO).

:- pred test(int::in, R::in, R::out, io::di, io::uo) is det <= random(R).

test(Count, !R, !IO) :-
    ( if Count > 0 then
        random.gen_uint64(N, !R),
        A = cast_to_int(N >> 32),
        B = cast_to_int(N /\ 0xffffffffu64),
        io.format("%08x%08x\n", [i(A), i(B)], !IO),
        test(Count - 1, !R, !IO)
    else
        true
    ).

