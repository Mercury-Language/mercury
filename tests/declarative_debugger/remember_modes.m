%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module remember_modes.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

:- pred p(int, int).
:- mode p(in, in) is semidet.
:- mode p(in, out) is semidet.
:- mode p(out, in) is semidet.
:- mode p(out, out) is det.

p(1, 2).

:- pred q(int::in, int::out, int::out, int::out, int::out) is semidet.

q(V, W, X, Y, Z) :-
    p(V, 2),
    p(W, 2),
    p(1, X),
    p(Y, Z).

main(!IO) :-
    ( if q(1, W, X, Y, Z) then
        io.write_line(W, !IO),
        io.write_line(X, !IO),
        io.write_line(Y, !IO),
        io.write_line(Z, !IO)
    else
        write_string("failed\n", !IO)
    ).
