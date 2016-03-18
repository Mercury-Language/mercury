%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module write_array.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array.
:- import_module list.

main(!IO) :-
    list.foldl(run_test, [[], [1], [1, 2, 3]], !IO).

:- pred run_test(list(int)::in, io::di, io::uo) is det.

run_test(List, !IO) :-
    Array = array.from_list(List),
    io.write_string("Array: ", !IO),
    io.write_array(Array, ", ", io.write_int, !IO),
    io.nl(!IO),
    io.write_string("List: ", !IO),
    io.write_list(List, ", ", io.write_int, !IO),
    io.nl(!IO).
