%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test array.shrink/3.
%
%---------------------------------------------------------------------------%

:- module array_shrink.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module exception.
:- import_module list.

:- type fruit
    --->    orange
    ;       lemon
    ;       apple
    ;       pear.

:- type color
    --->    color(uint8, uint8, uint8).

main(!IO) :-
    test_shrink([1, 2, 3, 4], -1, !IO), % Should raise exception.
    test_shrink([1, 2, 3, 4], 0, !IO),
    test_shrink([1, 2, 3, 4], 3, !IO),
    test_shrink([1, 2, 3, 4], 4, !IO),
    test_shrink([1, 2, 3, 4], 5, !IO),  % Should raise exception.
    test_shrink([1i8, 2i8, 3i8, 4i8], 3, !IO),
    test_shrink([1u8, 2u8, 3u8, 4u8], 3, !IO),
    test_shrink([1i16, 2i16, 3i16, 4i16], 3, !IO),
    test_shrink([1u16, 2u16, 3u16, 4u16], 3, !IO),
    test_shrink([1i32, 2i32, 3i32, 4i32], 3, !IO),
    test_shrink([1u32, 2u32, 3u32, 4u32], 3, !IO),
    test_shrink([1i64, 2i64, 3i64, 4i64], 3, !IO),
    test_shrink([1u64, 2u64, 3u64, 4u64], 3, !IO),
    test_shrink([1.0, 2.0, 3.0, 4.0], 3, !IO),
    test_shrink(["one", "two", "three", "four"], 3, !IO),
    test_shrink(['a', 'b', 'c', 'd'], 3, !IO),
    test_shrink([orange, lemon, apple, pear], 3, !IO),
    test_shrink([color(1u8, 1u8, 1u8), color(2u8, 2u8, 2u8),
        color(3u8, 3u8, 3u8), color(4u8, 4u8, 4u8)], 3, !IO),
    test_shrink([[1], [2, 2], [3, 3, 3], [4, 4, 4, 4]], 3, !IO).

:- pred test_shrink(list(T)::in, int::in, io::di, io::uo) is cc_multi.

test_shrink(Elems, Size, !IO) :-
    io.write_string("================\n", !IO),
    array.from_list(Elems, Array0),
    io.write_string("Array0 = ", !IO),
    io.write_line(Array0, !IO),
    io.write_string("Size = ", !IO),
    io.write_line(Size, !IO),
    ( try [] (
        array.shrink(Size, Array0, Array)
    ) then
        io.write_string("Array = ", !IO),
        io.write_line(Array, !IO)
    catch software_error(S) ->
        io.write_string("EXCEPTION: ", !IO),
        io.write_line(S, !IO)
    ).
