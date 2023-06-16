%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test byte oriented lookups in bitmaps.

:- module bitmap_bytes.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bitmap.
:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    BM0 = bitmap.init(0),
    do_bitmap_test(BM0, [-1, 0, 1], !IO),
    io.nl(!IO),

    BM1 = bitmap.init(24),
    do_bitmap_test(BM1, [-1, 0, 1, 2, 3, 4], !IO),
    io.nl(!IO),

    % With partial final byte.
    BM2 = bitmap.init(17),
    do_bitmap_test(BM2, [-1, 0, 1, 2, 3], !IO),
    io.nl(!IO),

    BM3 = det_from_string("<24:FF00FF>"),
    do_bitmap_test(BM3, [-1, 0, 1, 2, 3], !IO),
    io.nl(!IO),

    BM4 = det_from_string("<16:F00F>"),
    do_bitmap_test(BM4, [-1, 0, 1, 2], !IO).

:- pred do_bitmap_test(bitmap::in, list(byte_index)::in,
    io::di, io::uo) is cc_multi.

do_bitmap_test(BM, Indexes, !IO) :-
    io.format("Bitmap: %s\n", [s(to_byte_string(BM))], !IO),
    list.foldl(test_byte_lookup(BM), Indexes, !IO).

:- pred test_byte_lookup(bitmap::in, byte_index::in,
    io::di, io::uo) is cc_multi.

test_byte_lookup(BM, Index, !IO) :-
    io.format("^ byte(%d): ", [i(Index)], !IO),
    ( try []
        Byte = BM ^ byte(Index)
    then
        io.write_int(Byte, !IO)
    catch bitmap_error(ByteError) ->
        io.write_string(ByteError, !IO)
    ),
    io.nl(!IO),
    io.format("get_uint(%d): ", [i(Index)], !IO),
    ( try []
        U8 = get_uint8(BM, Index)
    then
        io.write_uint8(U8, !IO)
    catch bitmap_error(U8Error) ->
        io.write_string(U8Error, !IO)
    ),
    io.nl(!IO).
