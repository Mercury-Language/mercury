%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module partial.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    p(X),
    io.write_line(X, !IO).

:- type t
    --->    t(
                a :: int,
                b :: int
            ).

:- pred p(t::out) is det.

p(X) :-
    a(A),
    b(B),
    X = t(A, _),
    X = t(_, B).

:- pred a(int::out) is det.

a(1).

:- pred b(int::out) is det.

b(2).
