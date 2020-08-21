%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests that even though it is possible to introduce an accumulator for p,
% it would be counter productive because it makes the algorithm O(N^2).
%

:- module heuristic.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main(!IO) :-
    io.write_string("p: ", !IO),
    p([[1, 10, 100], [], [1, 2, 3], [6, 5, 4]], Length),
    io.write_line(Length, !IO).

:- pred p(list(list(T))::in, list(T)::out) is det.

p([], []).
p([X | Xs], L) :-
    p(Xs, L0),
    append(X, L0, L).
