%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% This is a regression test for the bug fixed in commit 65f683e.  That bug was
% an abort due to an InvalidCastException in the C# grades.  The problem was
% that the C# implementation of ML_new_array omitted the special case handling
% of primitive unsigned integer types.  More generally, several of the array
% primitives (ML_new_array, ML_unsafe_new_array, ML_resize_array and
% ML_shrink_array), all need to implement the special case handling of
% primitive types, in both the C# and Java grades.  This test checks that
% special case handling for primitives is present for all of those operations.
%
%---------------------------------------------------------------------------%

:- module array_primitives.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module list.
:- import_module string.
:- import_module type_desc.

%---------------------------------------------------------------------------%

main(!IO) :-
    do_test(2i8, 3i8, !IO),
    do_test(2u8, 3u8, !IO),
    do_test(2i16, 3i16, !IO),
    do_test(2u16, 3u16, !IO),
    do_test(2i32, 3i32, !IO),
    do_test(2u32, 3u32, !IO),
    do_test(2i64, 3i64, !IO),
    do_test(2u64, 3u64, !IO),

    do_test(2, 3, !IO),
    do_test(2u, 3u, !IO),
    do_test(2.0, 3.0, !IO),

    do_test("foo", "bar", !IO),
    do_test([1, 2, 3], [4], !IO).

:- pred do_test(T::in, T::in, io::di, io::uo) is det.

do_test(E, F, !IO) :-
    TypeName = type_name(type_of(E)),

    io.format("*** Testing with element type: %s ****\n", [s(TypeName)], !IO),
    array.init(5, E, InitArray),      % Test ML_new_array
    io.write_string("init = ", !IO),
    io.write_line(InitArray, !IO),

    GenArray = array.generate(5, (func(_) = E)), % Test ML_unsafe_new_array
    io.write_string("generate = ", !IO),
    io.write_line(GenArray, !IO),

    array.resize(7, F, InitArray, ResizedArray), % Test ML_array_resize
    io.write_string("resize = ", !IO),
    io.write_line(ResizedArray, !IO),

    array.shrink(2, GenArray, ShrunkArray), % Test ML_shrink_array
    io.write_string("shrink = ", !IO),
    io.write_line(ShrunkArray, !IO),

    io.nl(!IO).

%---------------------------------------------------------------------------%
:- end_module array_primitives.
%---------------------------------------------------------------------------%
