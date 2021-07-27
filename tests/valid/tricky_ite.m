%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tricky_ite.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module require.

main(!IO) :-
    ( if p(42, X) then
        error("blah"),
        io.write(X, !IO)
    else
        io.write_string("No.\n", !IO)
    ).

:- pred p(int::in, int::out) is nondet.

p(42, 1).
p(42, 2).
p(42, 3).
