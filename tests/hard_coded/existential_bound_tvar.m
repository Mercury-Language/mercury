%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module existential_bound_tvar.

:- interface.

:- pred main(io::di, io::uo) is det.

:- import_module io.

:- implementation.

:- import_module list.
:- import_module type_desc.

main(!IO) :-
    blah(101, X),
    io.write_string("X: value = ", !IO),
    io.print_line(X, !IO),
    io.write_string("X: type = ", !IO),
    io.print_line(type_of(X), !IO),

    blah2(101, Y),
    io.write_string("Y: value = ", !IO),
    io.print_line(Y, !IO),
    io.write_string("Y: type = ", !IO),
    io.print_line(type_of(Y), !IO),

    ( if blah3([101], Z) then
        io.write_string("Z: value = ", !IO),
        io.print_line(Z, !IO),
        io.write_string("Z: type = ", !IO),
        io.print_line(type_of(Z), !IO)
    else
        io.write("ERROR\n", !IO)
    ).

:- some [T1] pred blah(T, T1).
:- mode blah(in, out) is det.

blah(X, X).

:- some [T1] pred blah2(T, T1).
:- mode blah2(in, out) is det.

blah2(X, [X]).

:- some [T1] pred blah3(list(T), T1).
:- mode blah3(in, out) is semidet.

blah3([X], X).
