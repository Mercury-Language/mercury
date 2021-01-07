%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module quantifier2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

:- pred testsum(list(int), int, int).
:- mode testsum(in, in, out) is semidet.

testsum([], I, 0) :-
    I > 0.
testsum([X | L], I, X + N1) :-
    testsum(L, I, N1).

:- pred foo(pred(int, int)).
:- mode foo(free >> (pred(in, out) is semidet)) is det.

foo(testsum([1, 2, 3])).

main(!IO) :-
    ( if
        P = (pred(I :: in, X :: out) is semidet :- I > 0, X = 6),
        foo(Q),
        J = 1,
        ( call(P, J, _X) <=> call(Q, J, _Y) )
    then
        io.print_line("yes", !IO)
    else
        io.print_line("no", !IO)
    ).
