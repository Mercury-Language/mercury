%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module match_impl.

:- interface.
:- import_module char.

:- pred match_aab(list(char)::in) is semidet.

:- import_module list.

:- implementation.

match_aab(Str) :-
    match([a, a, b], Str).

:- pred match(list(char)::in, list(char)::in) is semidet.

match(Pat, T) :-
    match1(Pat, T, Pat, T).

:- pred match1(list(char)::in, list(char)::in, list(char)::in,
    list(char)::in) is semidet.

match1([], _Ts, _P, _T).
match1([A | Ps], [B | Ts], P, [X | T]) :-
    ( if A = B then
        match1(Ps, Ts, P, T)
    else
            match1(P, Ts, P, [X | T])
    ).
