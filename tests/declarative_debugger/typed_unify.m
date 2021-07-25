%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module typed_unify.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module univ.

main(!IO) :-
    U = univ(1),
    ( if type_to_univ(I, U) then
        io.write_int(I, !IO)
    else
        true
    ),
    io.nl(!IO).
