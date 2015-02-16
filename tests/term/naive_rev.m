%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module naive_rev.

:- interface.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- pred reverse(list(T)::in, list(T)::out) is det.

:- implementation.

reverse([X | Xs], Ys) :-
    reverse(Xs, Zs),
    app(Zs, [X], Ys).
reverse([], []).

:- pred app(list(T)::in, list(T)::in, list(T)::out).

app([X | Xs], Ys, [X | Zs]) :-
    app(Xs, Ys, Zs).
app([], Ys, Ys).
