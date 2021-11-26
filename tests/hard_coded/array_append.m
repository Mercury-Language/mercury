%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Test array append.
%---------------------------------------------------------------------------%

:- module array_append.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.

main(!IO) :-
    make_empty_array(EmptyA),
    make_empty_array(EmptyB),
    ArrayA = array([1, 2, 3]),
    ArrayB = array([4, 5, 6]),

    do_test(EmptyA, EmptyB, !IO),
    do_test(EmptyA, ArrayA, !IO),
    do_test(ArrayA, EmptyA, !IO),
    do_test(ArrayA, ArrayB, !IO).

:- pred do_test(array(T)::array_ui, array(T)::array_ui, io::di, io::uo) is det.

do_test(A, B, !IO) :-
    C = A `array.append` B,
    io.print(A, !IO),
    io.print(" `append` ", !IO),
    io.print(B, !IO),
    io.print(" = ", !IO),
    io.print_line(C, !IO).

%---------------------------------------------------------------------------%
:- end_module array_append.
%---------------------------------------------------------------------------%
