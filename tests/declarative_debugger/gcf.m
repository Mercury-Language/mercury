%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module gcf.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module maybe.

main(!IO) :-
    ( if
        a(X),
        X > 15
    then
        R = yes(X)
    else
        R = no
    ),
    io.write_line(R, !IO).

:- pred a(int::out) is nondet.

a(X) :-
    g(Y),
    c(Y, X),
    f(X).

:- pred g(int::out) is multi.

g(1).
g(2).
g(3).
g(4).

:- pred c(int::in, int::out) is nondet.

c(2, 10).
c(2, 11).
c(2, 12).
c(3, 20).

:- pred f(int::in) is semidet.

f(X) :-
    X > 10.
