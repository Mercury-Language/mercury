%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module neg_conj.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    ( if p(0) then
        io.write_string("yes.\n", !IO)
    else
        io.write_string("no.\n", !IO)
    ).

:- pred p(int::in) is semidet.

p(X) :-
    not (
        q(X, Y),
        r(Y)
    ).

:- pred q(int::in, int::out) is nondet.

q(0, 0).
q(0, 1).

:- pred r(int::in) is semidet.

r(Y) :-
    Y > 1.
