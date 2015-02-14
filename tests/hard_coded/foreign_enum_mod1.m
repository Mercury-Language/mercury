%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_enum_mod1.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module foreign_enum_mod2.
:- import_module list.

main(!IO) :-
    io.write_string("The ingredients are ", !IO),
    List = [flour, eggs, milk],
    io.write(List, !IO),
    io.nl(!IO),
    io.write_string("My instrument is ", !IO),
    io.write(my_instrument, !IO),
    io.nl(!IO).
