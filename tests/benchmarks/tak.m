%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tak.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module prolog.

main(!IO) :-
    tak(18, 12, 6, Out),
    io.write_int(Out, !IO),
    io.write_string("\n", !IO).

:- pred tak(int::in, int::in, int::in, int::out) is det.

tak(X, Y, Z, A) :-
    ( if X =< Y then
        Z = A
    else
        X1 = X - 1,
        tak(X1, Y, Z, A1),
        Y1 = Y - 1,
        tak(Y1, Z, X, A2),
        Z1 = Z - 1,
        tak(Z1, X, Y, A3),
        tak(A1, A2, A3, A)
    ).
