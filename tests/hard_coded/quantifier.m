%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module quantifier.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main(!IO) :-
    ( if
        P = (pred(X :: out) is det :- X = 6),
        foo(Q),
        all [X] (call(P, X) <=> call(Q, X))
    then
        io.print_line("equivalent", !IO)
    else
        io.print_line("not equivalent", !IO)
    ).

:- pred sum(list(int), int).
:- mode sum(in, out) is det.

sum([], 0).
sum([X | L], X + N1) :-
    sum(L, N1).

:- pred foo(pred(int)).
:- mode foo(free >> (pred(out) is det)) is det.

foo(sum([1, 2, 3])).
