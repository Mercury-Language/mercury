%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test array.fill/3 and array.fill_range/5.
%

:- module array_fill.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module exception.
:- import_module list.

:- type dummy ---> dummy.

:- type fruit ---> apple ; orange ; lemon.

:- type color
    --->    color(
                red   :: uint8,
                green :: uint8,
                blue  :: uint8
            ).

main(!IO) :-
    test_fill(0, "foo", "bar", !IO),
    test_fill(1, "foo", "bar", !IO),
    test_fill(3, "foo", "bar", !IO),
    test_fill(3, 'a', 'b', !IO),
    test_fill(3, 561, -561, !IO),
    test_fill(3, 34u, 41u, !IO),
    test_fill(3, -128i8, 127i8, !IO),
    test_fill(3, 0u8, 255u8, !IO),
    test_fill(3, -32768i16, 32767i16, !IO),
    test_fill(3, 0u16, 65535u16, !IO),
    test_fill(3, -2147483648i32, 2147483647i32, !IO),
    test_fill(3, 0u32, 4294967295u32, !IO),
    test_fill(3, -9223372036854775808i64, 9223372036854775807i64, !IO),
    test_fill(3, 0u64, 18446744073709551615u64, !IO),
    test_fill(3, dummy, dummy, !IO),
    test_fill(3, orange, lemon, !IO),
    test_fill(3, [1, 2, 3], [4, 5, 6], !IO),
    test_fill(3, color(0u8, 0u8, 0u8), color(255u8, 255u8, 255u8), !IO),

    test_fill_range([1, 2, 3, 4, 5, 6], 561, 3, 1, !IO),
    test_fill_range([1, 2, 3, 4, 5, 6], 561, -1, 3, !IO),
    test_fill_range([1, 2, 3, 4, 5, 6], 561, 1, 8, !IO),
    test_fill_range([1, 2, 3, 4, 5, 6], 561, 1, 3, !IO),
    test_fill_range([], 561, 0, 0, !IO).

:- pred test_fill(int::in, T::in, T::in, io::di, io::uo) is det.

test_fill(Size, InitElem, FillElem, !IO) :-
    io.write_string("-------FILL--------\n", !IO),
    array.init(Size, InitElem, Array0),
    io.write_string("Array0 = ", !IO),
    io.write_line(Array0, !IO),
    io.write_string("Fill = ", !IO),
    io.write_line(FillElem, !IO),
    array.fill(FillElem, Array0, Array),
    io.write_string("Array = ", !IO),
    io.write(Array, !IO),
    io.nl(!IO).

:- pred test_fill_range(list(T)::in, T::in, int::in, int::in,
    io::di, io::uo) is cc_multi.

test_fill_range(InitElems, FillElem, Lo, Hi, !IO) :-
    io.write_string("-------FILL RANGE-------\n", !IO),
    Array0 = array.from_list(InitElems),
    io.write_string("Array0 = ", !IO),
    io.write_line(Array0, !IO),
    io.write_string("Lo = ", !IO),
    io.write_line(Lo, !IO),
    io.write_string("Hi = ", !IO),
    io.write_line(Hi, !IO),
    io.write_string("Fill = ", !IO),
    io.write_line(FillElem, !IO),
    ( try [] (
        array.fill_range(FillElem, Lo, Hi, Array0, Array)
    ) then
        io.write_string("Array = ", !IO),
        io.write_line(Array, !IO)
    catch index_out_of_bounds(S) ->
        io.write_string("INDEX-OUT-OF-BOUNDS: ", !IO),
        io.write_line(S, !IO)
    catch software_error(S) ->
        io.write_string("EXCEPTION: ", !IO),
        io.write_line(S, !IO)
    ).
