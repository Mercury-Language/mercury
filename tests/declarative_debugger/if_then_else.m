%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module if_then_else.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    ite(0, X),
    io.write_string("ite(0, ", !IO),
    io.write_int(X, !IO),
    io.write_string(").\n", !IO),
    ite(1, Y),
    io.write_string("ite(1, ", !IO),
    io.write_int(Y, !IO),
    io.write_string(").\n", !IO).

:- pred ite(int::in, int::out) is det.

ite(A, B) :-
    ( if a(A) then
        b(B)
    else
        a(B)
    ).

:- pred a(int).
:- mode a(in) is semidet.
:- mode a(out) is det.

a(0).

:- pred b(int).
:- mode b(out) is det.

b(1).
