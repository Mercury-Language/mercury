%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test array.resize/3.
%
%---------------------------------------------------------------------------%

:- module array_resize.
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
    ;       pear
    ;       pineapple.

:- type color
    --->    color(uint8, uint8, uint8).

main(!IO) :-
    test_resize([1, 2, 3, 4], -1, 42, !IO), % Should raise exception.
    test_resize([1, 2, 3, 4], 0, 42, !IO),
    test_resize([1, 2, 3, 4], 3, 42, !IO),
    test_resize([1, 2, 3, 4], 4, 42, !IO),
    test_resize([1, 2, 3, 4], 5, 42, !IO),
    test_resize([1i8, 2i8, 3i8, 4i8], 3, 42i8, !IO),
    test_resize([1i8, 2i8, 3i8, 4i8], 6, 42i8, !IO),
    test_resize([1u8, 2u8, 3u8, 4u8], 3, 42u8, !IO),
    test_resize([1u8, 2u8, 3u8, 4u8], 6, 42u8, !IO),
    test_resize([1i16, 2i16, 3i16, 4i16], 3, 42i16, !IO),
    test_resize([1i16, 2i16, 3i16, 4i16], 6, 42i16, !IO),
    test_resize([1u16, 2u16, 3u16, 4u16], 3, 42u16, !IO),
    test_resize([1u16, 2u16, 3u16, 4u16], 6, 42u16, !IO),
    test_resize([1i32, 2i32, 3i32, 4i32], 3, 42i32, !IO),
    test_resize([1i32, 2i32, 3i32, 4i32], 6, 42i32, !IO),
    test_resize([1u32, 2u32, 3u32, 4u32], 3, 42u32, !IO),
    test_resize([1u32, 2u32, 3u32, 4u32], 6, 42u32, !IO),
    test_resize([1i64, 2i64, 3i64, 4i64], 3, 42i64, !IO),
    test_resize([1i64, 2i64, 3i64, 4i64], 6, 42i64, !IO),
    test_resize([1u64, 2u64, 3u64, 4u64], 3, 42u64, !IO),
    test_resize([1u64, 2u64, 3u64, 4u64], 6, 42u64, !IO),
    test_resize([1.0, 2.0, 3.0, 4.0], 3, 42.0, !IO),
    test_resize([1.0, 2.0, 3.0, 4.0], 36, 42.0, !IO),
    test_resize(["one", "two", "three", "four"], 3, "forty-two", !IO),
    test_resize([1.0, 2.0, 3.0, 4.0], 6, 42.0, !IO),
    test_resize(['a', 'b', 'c', 'd'], 3, 'X', !IO),
    test_resize(['a', 'b', 'c', 'd'], 6, 'X', !IO),
    test_resize([orange, lemon, apple, pear], 3, pineapple, !IO),
    test_resize([orange, lemon, apple, pear], 6, pineapple, !IO),
    test_resize([color(1u8, 1u8, 1u8), color(2u8, 2u8, 2u8),
        color(3u8, 3u8, 3u8), color(4u8, 4u8, 4u8)], 3, color(0u8, 0u8, 0u8),
        !IO),
    test_resize([color(1u8, 1u8, 1u8), color(2u8, 2u8, 2u8),
        color(3u8, 3u8, 3u8), color(4u8, 4u8, 4u8)], 6, color(0u8, 0u8, 0u8),
        !IO),
    test_resize([[1], [2, 2], [3, 3, 3], [4, 4, 4, 4]], 3, [42], !IO),
    test_resize([[1], [2, 2], [3, 3, 3], [4, 4, 4, 4]], 6, [42], !IO).

:- pred test_resize(list(T)::in, int::in, T::in, io::di, io::uo) is cc_multi.

test_resize(Elems, Size, Init, !IO) :-
    io.write_string("================\n", !IO),
    array.from_list(Elems, Array0),
    io.write_string("Array0 = ", !IO),
    io.write_line(Array0, !IO),
    io.write_string("Size = ", !IO),
    io.write_line(Size, !IO),
    io.write_string("Init = ", !IO),
    io.write_line(Init, !IO),
    ( try [] (
        array.resize(Size, Init, Array0, Array)
    ) then
        io.write_string("Array = ", !IO),
        io.write_line(Array, !IO)
    catch software_error(S) ->
        io.write_string("EXCEPTION: ", !IO),
        io.write_line(S, !IO)
    ).
