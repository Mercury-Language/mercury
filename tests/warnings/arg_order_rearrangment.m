%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% For accumulators to be introduced into r, R0 and [H] must have their
% order swapped. The compiler should warn the user that this may have
% performance implications (a good one in this case).
%
%---------------------------------------------------------------------------%

:- module arg_order_rearrangment.
:- interface.
:- import_module list.
:- pred r(list(T)::in, list(T)::out) is det.
:- implementation.

r([], []).
r([H | T], R) :-
    r(T, R0),
    app(R0, [H], R).

:- promise all [A, B, C, ABC]
    (
        (some [AB] (app(A, B, AB), app(AB, C, ABC)))
    <=>
        (some [BC] (app(B, C, BC), app(A, BC, ABC)))
    ).

:- pred app(list(T)::in, list(T)::in, list(T)::out) is det.

app([], Ys, Ys).
app([X | Xs], Ys, [X | Zs]) :-
    app(Xs, Ys, Zs).
