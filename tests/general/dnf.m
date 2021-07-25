%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This tests whether the program as transformed by dnf still works.
% It does not (cannot) test whether dnf is applied, of course.

:- module dnf.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module std_util.

main(!IO) :-
    solutions(
        ( pred(Pair::out) is multi :-
            Pair = X - Y,
            q(X, Y)
        ), List),
    print_list(List, !IO).

:- pred print_list(list(pair(int))::in, io::di, io::uo) is det.

print_list([], !IO) :-
print_list([X - Y | XYs], !IO) :-
    io.format("X = %d, Y = %d\n", [i(X), i(Y)], !IO),
    print_list(XYs, !IO).

:- pred q(int::out, int::out) is multidet.

:- pragma(memo, q/2).

q(X, Y) :-
    ( X = 1 ; X = 2 ),
    ( Y = 41 ; Y = 42 ).
