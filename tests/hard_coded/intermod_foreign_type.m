%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_foreign_type.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module intermod_foreign_type_helper_1.
:- import_module std_util.

main(!IO) :-
    C = new(4, 5),
    io.write_string("X:", !IO),
    io.write_int(x(C), !IO),
    io.nl(!IO),
    io.write_string("Y:", !IO),
    io.write_int(y(C), !IO),
    io.nl(!IO),
    io.write_line(coord(1, 2), !IO).

:- type coord2
    --->    coord2(int, int).

:- func coord(int, int) = coord2.

coord(X, Y) = coord2(X, Y).
