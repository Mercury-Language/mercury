%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% File: higher_order.m.
% Author: fjh.
%
% Some very basic tests of higher-order predicates and lambda expressions.

:- module higher_order.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.
:- import_module string.

:- pred map(pred(T1, T2), list(T1), list(T2)).
:- mode map(pred(in, out) is det, in, out) is det.
:- mode map(pred(in, in) is semidet, in, in) is semidet.

map(_Pred, [], []).
map(Pred, [X | Xs], [Y | Ys]) :-
    call(Pred, X, Y),
    higher_order.map(Pred, Xs, Ys).

:- pred double(string::in, string::out) is det.

double(X, Y) :-
    string.append(X, X, Y).

main(!IO) :-
    higher_order.map(double, ["foo", "bar"], List),
    io.write_strings(List, !IO),
    io.write_string("\n", !IO),
    ( if
        higher_order.map(
            ( pred(X::in, Y::in) is semidet :-
                double(X, Y)
            ), ["ab"], ["abab"])
    then
        io.write_string("Yes\n", !IO)
    else
        io.write_string("Oops\n", !IO)
    ),
    ( if
        higher_order.map(
            ( pred(X::in, Y::in) is semidet :-
                double(X, Y)
            ), ["ab"], ["abracadabra"])
    then
        io.write_string("Oops\n", !IO)
    else
        io.write_string("No\n", !IO)
    ),
    ( if
        higher_order.map(
            ( pred(X::in, Y::in) is semidet :-
                double(X, Y)
            ), ["ab"], [])
    then
        io.write_string("Oops\n", !IO)
    else
        io.write_string("No\n", !IO)
    ).
