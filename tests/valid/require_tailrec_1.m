%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the require tail recursion pragma with the
% --warn-non-tail-recursion-self option, and with pred()/func() wrappers
% around all name/arity pairs.
%

:- module require_tailrec_1.

:- interface.

:- import_module bool.
:- import_module int.
:- import_module list.

:- pred foldl1(pred(X, A, A), list(X), A, A).
:- mode foldl1(pred(in, in, out) is det, in, in, out) is det.

:- pred map1(pred(X, Y), list(X), list(Y)).
:- mode map1(pred(in, out) is det, in, out) is det.

:- func even1(int) = bool.
:- func odd1(int) = bool.

:- func even2(int) = bool.
:- func odd2(int) = bool.

:- func even3(int) = bool.
:- func odd3(int) = bool.

%---------------------------------------------------------------------------%

:- pred qsortapp(list(int)::in, list(int)::out) is det.

:- implementation.

% self tail recursive code with no pragma.
foldl1(_, [], !Acc).
foldl1(P, [X | Xs], !Acc) :-
    P(X, !Acc),
    foldl1(P, Xs, !Acc).

% self non-tail recursive code with reports disabled.
:- pragma disable_non_tail_recursion_reports(pred(map1/3)).
map1(_, [], []).
map1(P, [X | Xs], [Y | Ys]) :-
    P(X, Y),
    map1(P, Xs, Ys).

% mutual non-tail recursion without pragma.
even1(N) =
    ( if N = 0 then
        yes
    else
        odd1(N - 1)
    ).

odd1(N) =
    ( if N = 0 then
        no
    else
        bool.not(even1(N))
    ).

% mutual non-tail recursion with reports disabled.
:- pragma disable_non_tail_recursion_reports(func(odd2/1)).
even2(N) =
    ( if N = 0 then
        yes
    else
        odd2(N - 1)
    ).

:- pragma require_tail_recursion(func(odd3/1), [self_recursion_only]).
odd2(N) =
    ( if N = 0 then
        no
    else
        bool.not(even2(N))
    ).

even3(N) =
    ( if N = 0 then
        yes
    else
        odd3(N - 1)
    ).

odd3(N) =
    ( if N = 0 then
        no
    else
        bool.not(even3(N))
    ).

%---------------------------------------------------------------------------%

:- pragma disable_non_tail_recursion_reports(pred(qsortapp/2)).

qsortapp([], []).
qsortapp([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsortapp(Left0, Left),
    qsortapp(Right0, Right),
    append(Left, [Pivot | Right], List).

:- pred partition(int::in, list(int)::in, list(int)::in, list(int)::out,
    list(int)::in, list(int)::out) is det.
:- pragma require_tail_recursion(pred(partition/6)).

partition(_Pivot, [], Left, Left, Right, Right).
partition(Pivot, [H | T], Left0, Left, Right0, Right) :-
    ( if H < Pivot then
        partition(Pivot, T, [H | Left0], Left, Right0, Right)
    else
        partition(Pivot, T, Left0, Left, [H | Right0], Right)
    ).
