%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module contains.

:- interface.

:- import_module char.
:- import_module list.

:- pred contains_aab(list(char)::in) is semidet.

:- implementation.

contains_aab(Str) :-
    con(Str, [], [a, a, b]).

:- pred con(list(char)::in, list(char)::in, list(char)::in) is semidet.

con(_, _, []).
con([T | Rem_str], Prefix0, Suffix0) :-
    new(T, Prefix0, Suffix0, Prefix, Suffix),
    con(Rem_str, Prefix, Suffix).

:- pred new(char::in, list(char)::in, list(char)::in,
    list(char)::out, list(char)::out) is nondet.

new(T, Prefix0, [T2 | Suffix0], Prefix, Suffix) :-
    ( if T = T2 then
        app( Prefix0, [T], Prefix),
        Suffix = Suffix0
    else
        app(Prefix0, [T], Temp),
        app(Prefix , Rest, Prefix0),
        app(_, Prefix, Temp),
        app(Rest, [T2 | Suffix0], Suffix)
    ).

:- pred app(list(T), list(T), list(T)).
:- mode app(in, in, out) is det.
:- mode app(out, out, in) is multi.

app([], L, L).
app([X | Xs], Ys, [X | Zs]) :-
    app(Xs, Ys, Zs).
