%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_enum_test.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module foreign_enum_test_helper_1.
:- import_module list.

main(!IO) :-
    List = [flour, eggs, milk],
    io.write_string("The ingredients are ", !IO),
    io.write_line(List, !IO),
    io.write_string("My instrument is ", !IO),
    io.write_line(my_instrument, !IO).
