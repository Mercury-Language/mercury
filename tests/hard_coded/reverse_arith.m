%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module reverse_arith.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    ( if X = 5, X = 2 * 2 then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).
