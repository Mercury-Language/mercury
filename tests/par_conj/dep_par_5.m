%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% IO

:- module dep_par_5.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    (
        U = 1
    &
        io.write_int(U, !IO)
    ),
    io.nl(!IO).
