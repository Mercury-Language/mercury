%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test array.copy/2.
%

:- module array_copy.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module exception.
:- import_module list.

:- type fruit ---> apple ; lemon ; orange.

main(!IO) :-
    test_copy(['a', 'b', 'c'], !IO),
    test_copy([1, 2, 3], !IO),
    test_copy([1u, 2u, 3u], !IO),
    test_copy([1i8, 2i8, 3i8], !IO),
    test_copy([1u8, 2u8, 3u8], !IO),
    test_copy([1i16, 2i16, 3i16], !IO),
    test_copy([1u16, 2u16, 3u16], !IO),
    test_copy([1i32, 2i32, 3i32], !IO),
    test_copy([1u32, 2u32, 3u32], !IO),
    test_copy([1i64, 2i64, 3i64], !IO),
    test_copy([1u64, 2u64, 3u64], !IO),
    test_copy(["foo", "bar", "baaz"], !IO),
    test_copy([apple, lemon, orange], !IO),
    test_copy([[1], [2, 2], [3, 3,3]], !IO).

:- pred test_copy(list(T)::in, io::di, io::uo) is det.

test_copy(Elems, !IO) :-
    io.write_string("=====================\n", !IO),
    array.from_list(Elems, Array0),
    io.write_string("Array0 = ", !IO),
    io.write_line(Array0, !IO),
    array.copy(Array0, Array),
    io.write_string("Array = ", !IO),
    io.write(Array, !IO),
    io.nl(!IO).
