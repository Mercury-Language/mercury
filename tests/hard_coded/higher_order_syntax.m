%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module higher_order_syntax.

:- interface.
:- import_module io.
:- import_module list.

:- pred main(io::di, io::uo) is det.

:- func map_f(func(X) = Y, list(X)) = list(Y).
:- mode map_f(func(in) = out is det, in) = out is det.
:- mode map_f(func(in) = out is semidet, in) = out is semidet.

:- pred map_p(pred(X, Y), list(X), list(Y)).
:- mode map_p(pred(in, out) is det, in, out) is det.
:- mode map_p(pred(in, out) is semidet, in, out) is semidet.
:- mode map_p(pred(in, out) is multi, in, out) is multi.
:- mode map_p(pred(in, out) is nondet, in, out) is nondet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module std_util.

main(!IO) :-
    L1 = [1, 2, 3],
    L2 = map_f((func(X::in) = (Y::out) is det :- Y = 2*X), L1),
    map_p((pred(X::in, Y::out) is det :- Y = 2*X), L2, L3),
    L4 = map_f((func(X2) = Y2 :- Y2 = 5*X2), L3),
    L = map_f(func(X3) = 10*X3, L4),
    Foldit =
        ( pred(IO0::di, IO::uo) is det :-
            list.foldl(io.write_int, L, IO0, IO)
        ),
    Foldit(!IO),
    Write = io.write_string,
    Write("\n", !IO),
    ( if doit(semidet_succeed) then
        Write("Yes.\n", !IO)
    else
        Write("No.\n", !IO)
    ).

map_f(_, []) = [].
map_f(F, [H0 | T0]) = [F(H0) | map_f(F, T0)].

map_p(_, [], []).
map_p(P, [X | Xs], [Y | Ys]) :-
    P(X, Y),
    map_p(P, Xs, Ys).

:- pred doit(pred).
:- mode doit((pred) is semidet).
doit(P) :-
    P.
