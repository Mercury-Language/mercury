%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% A regression test.
% We test that the determination of possible cons_ids a cell can have
% is correct for branched goal structures, in this case the if_then_else.
%---------------------------------------------------------------------------%

:- module if_then_else.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type t
    --->    empty
    ;       one(int)
    ;       two(int, int).

main(!IO) :-
    X = two(1, 2),
    p(X, Y),
    io.write_line(Y, !IO).

:- pred p(t::in, t::out) is det.

p(X, Y) :-
    ( if X = empty then
        Y = X
    else
        (
            X = two(A, B),
            Y = two(B, A)
        ;
            X = one(A),
            Y = one(A)
        ;
            X = empty,
            Y = empty
        )
    ).
