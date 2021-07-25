%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module mode_inf.
:- interface.
:- use_module io.

:- pred main(io.io::di, io.io::uo) is det.

:- implementation.

:- use_module    array.
:- import_module int.
:- use_module    list.
:- import_module solutions.

main(!IO) :-
    solutions(
        ( pred(X::out) is multi :-
            do_some_stuff(X)
        ), L),
    list.foldl(io.write_line, L, !IO).

do_some_stuff(X) :-
    some_array_stuff(42, Y),
    some_backtracking_stuff(Y, X, _).

some_array_stuff(X, Y) :-
    array.init(40, 80, A0),
    array.set(37, X, A0, A1),
    array.lookup(A1, 37, Y).

some_backtracking_stuff(X, Y, Z) :- Y = X + 1, p(Z).
some_backtracking_stuff(X, Y, Z) :- Y = X + 2, p(Z).
some_backtracking_stuff(X, Y, Z) :- Y = X + 3, p(Z).

p(1).
p(2).
