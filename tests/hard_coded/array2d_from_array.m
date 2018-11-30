%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test array2d.from_array/3.
%

:- module array2d_from_array.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module array2d.
:- import_module exception.
:- import_module list.

main(!IO) :-
    test_from_array([], -1, -1, !IO),
    test_from_array([], 0, -1, !IO),
    test_from_array([], -1, 0, !IO),
    test_from_array([], 2, 2, !IO),              % Too few elements.
    test_from_array([1, 2, 3, 4, 5], 2, 2, !IO), % Too many elements.
    test_from_array([], 0, 0, !IO),
    test_from_array([1], 1, 1, !IO),
    test_from_array([1, 2, 3, 4], 2, 2, !IO),
    test_from_array([1, 2, 3, 4, 5, 6], 2, 3, !IO).

:- pred test_from_array(list(int)::in, int::in, int::in,
    io::di, io::uo) is cc_multi.

test_from_array(Elems, M, N, !IO) :-
    io.write_string("------FROM ARRAY------\n", !IO),
    Array = array.from_list(Elems),
    io.write_string("Array = ", !IO),
    io.write_line(Array, !IO),
    io.write_string("M = ", !IO),
    io.write_line(M, !IO),
    io.write_string("N = ", !IO),
    io.write_line(N, !IO),
    ( try []
        Array2d = array2d.from_array(M, N, Array)
    then
        io.write_string("Array2d = ", !IO),
        io.write_line(Array2d, !IO)
    catch software_error(E) ->
        io.write_string("EXCEPTION: ", !IO),
        io.write_line(E, !IO)
    ).
