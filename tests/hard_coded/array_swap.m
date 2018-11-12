%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test array.swap/4.
%
%---------------------------------------------------------------------------%

:- module array_swap.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module exception.
:- import_module list.
:- import_module string.

:- type fruit
    --->    orange
    ;       lemon
    ;       apple
    ;       pear.

:- type color
    --->    color(uint8, uint8, uint8).

main(!IO) :-
    test_swap([1, 2, 3, 4], -1, 0, !IO), % Should raise exception.
    test_swap([1, 2, 3, 4], 2, -1, !IO), % Should raise exception.
    test_swap([1, 2, 3, 4], 0, 0, !IO),
    test_swap([1, 2, 3, 4], 1, 2, !IO),
    test_swap([1i8, 2i8, 3i8, 4i8], 0, 1, !IO),
    test_swap([1u8, 2u8, 3u8, 4u8], 0, 1, !IO),
    test_swap([1i16, 2i16, 3i16, 4i16], 0, 1, !IO),
    test_swap([1u16, 2u16, 3u16, 4u16], 0, 1, !IO),
    test_swap([1i32, 2i32, 3i32, 4i32], 0, 1, !IO),
    test_swap([1u32, 2u32, 3u32, 4u32], 0, 1, !IO),
    test_swap([1i64, 2i64, 3i64, 4i64], 0, 1, !IO),
    test_swap([1u64, 2u64, 3u64, 4u64], 0, 1, !IO),
    test_swap([1.0, 2.0, 3.0, 4.0], 0, 1, !IO),
    test_swap(["one", "two", "three", "four"], 0, 1, !IO),
    test_swap(['a', 'b', 'c', 'd'], 0, 1, !IO),
    test_swap([orange, lemon, apple, pear], 0, 1, !IO),
    test_swap([color(1u8, 1u8, 1u8), color(2u8, 2u8, 2u8),
        color(3u8, 3u8, 3u8), color(4u8, 4u8, 4u8)], 0, 1, !IO),
    test_swap([[1], [2, 2], [3, 3, 3], [4, 4, 4, 4]], 0, 1, !IO).

:- pred test_swap(list(T)::in, int::in, int::in, io::di, io::uo) is cc_multi.

test_swap(Elems, I, J, !IO) :-
    io.write_string("================\n", !IO),
    array.from_list(Elems, Array0),
    io.write_string("Array0 = ", !IO),
    io.write_line(Array0, !IO),
    io.format("Swap: %d <-> %d\n", [i(I), i(J)], !IO),
    ( try []
        array.swap(I, J, Array0, Array)
    then
        io.write_string("Array = ", !IO),
        io.write_line(Array, !IO)
    catch index_out_of_bounds(S) ->
        io.write_string("EXCEPTION: ", !IO),
        io.write_line(S, !IO)
    ).
