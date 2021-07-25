%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module divide_and_query1.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.

:- type t
    --->    a
    ;       b
    ;       c.

:- type list(T)
    --->    []
    ;       [T | list(T)].

main(!IO) :-
    to_b([a, a, a, a, a, a, a, a, a, a], X),
    ( if abba([b, a, a, b]) then
        A = yes
    else
        A = no
    ),
    ( if abba([a, a, a, b]) then
        B = yes
    else
        B = no
    ),
    ( if abba([a, a, b, b]) then
        C = yes
    else
        C = no
    ),
    to_b2([c, c, c, c, c, c, c], Y),
    write(X, !IO),
    write(Y, !IO),
    write([A, B, C], !IO).

:- pred to_b(list(t)::in, list(t)::out) is det.

to_b([], []).
to_b([_ | T], [b | L]) :-
    to_b(T, L).

:- pred abba(list(t)::in) is semidet.

abba(L) :-
    abba_perm(L, [a, b, b, a]).

:- pred abba_perm(list(T)::in, list(T)::out) is multi.

abba_perm([], []).
abba_perm([X | Xs], Ys) :-
    abba_perm(Xs, Ys0),
    abba_delete(Ys, X, Ys0).

:- pred abba_delete(list(T)::out, T::in, list(T)::in) is multi.

abba_delete([X | L], X, L).
abba_delete([X | Xs], Y, [X | L]) :-
    abba_delete(Xs, Y, L).

:- pred to_b2(list(t)::in, list(t)::out) is det.

to_b2(L0, L) :-
    to_b(L0, L).
