%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%
% Tests of `pragma require_tail_recursion' with
% `--warn-non-tail-recursion self'.
%
% The .exp file is for non-deep-profiling LLDS grades.
% The .exp3 file is for deep profiling LLDS grades.
% The .exp2 file is for MLDS grades.
%
%---------------------------------------------------------------------------%

:- module require_tailrec_2.

:- interface.

:- import_module bool.
:- import_module int.
:- import_module list.

:- pred map1(pred(X, Y), list(X), list(Y)).
:- mode map1(pred(in, out) is det, in, out) is det.

:- pred map2(pred(X, Y), list(X), list(Y)).
:- mode map2(pred(in, out) is det, in, out) is det.

:- func even1(int) = bool.
:- func odd1(int) = bool.

%---------------------------------------------------------------------------%

:- pred qsortapp_1(list(int)::in, list(int)::out) is det.
:- pred qsortapp_2(list(int)::in, list(int)::out) is det.
:- pred qsortapp_3(list(int)::in, list(int)::out) is det.
:- pred qsortapp_4(list(int)::in, list(int)::out) is det.
:- pred qsortapp_5(list(int)::in, list(int)::out) is det.
:- pred qsortapp_6(list(int)::in, list(int)::out) is det.

:- func cons(X, list(X)) = list(X).

%---------------------------------------------------------------------------%

:- implementation.

% self non-tail recursion with no pragma
map1(_, [], []).
map1(P, [X | Xs], [Y | Ys]) :-
    P(X, Y),
    map1(P, Xs, Ys).

% self non-tail recursion with self pragma
:- pragma require_tail_recursion(map2/3, [self_or_mutual_recursion]).
map2(_, [], []).
map2(P, [X | Xs], [Y | Ys]) :-
    P(X, Y),
    map2(P, Xs, Ys).

% mutual non-tail recursion with mutual pragma
:- pragma require_tail_recursion(even1/1, [self_or_mutual_recursion]).
even1(N) =
    ( if N = 0 then
        yes
    else
        bool.not(odd1(N))
    ).

% mutual tail recursion with mutual pragma, this does not raise an error.
:- pragma require_tail_recursion(odd1/1, [self_or_mutual_recursion]).
odd1(N) =
    ( if N = 0 then
        no
    else
        even1(N - 1)
    ).

%---------------------------------------------------------------------------%

:- pragma require_tail_recursion(qsortapp_1/2).

qsortapp_1([], []).
qsortapp_1([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsortapp_1(Left0, Left),
    qsortapp_1(Right0, Right),
    append(Left, [Pivot | Right], List).

:- pragma require_tail_recursion(qsortapp_2/2, []).

qsortapp_2([], []).
qsortapp_2([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsortapp_2(Left0, Left),
    qsortapp_2(Right0, Right),
    append(Left, [Pivot | Right], List).

:- pragma require_tail_recursion(qsortapp_3/2, [warn]).

qsortapp_3([], []).
qsortapp_3([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsortapp_3(Left0, Left),
    qsortapp_3(Right0, Right),
    append(Left, [Pivot | Right], List).

:- pragma require_tail_recursion(qsortapp_4/2, [error]).

qsortapp_4([], []).
qsortapp_4([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsortapp_4(Left0, Left),
    qsortapp_4(Right0, Right),
    append(Left, [Pivot | Right], List).

:- pragma require_tail_recursion(qsortapp_5/2, [self_recursion_only]).

qsortapp_5([], []).
qsortapp_5([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsortapp_5(Left0, Left),
    qsortapp_5(Right0, Right),
    append(Left, [Pivot | Right], List).

:- pragma require_tail_recursion(qsortapp_6/2, [self_or_mutual_recursion]).

qsortapp_6([], []).
qsortapp_6([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsortapp_6(Left0, Left),
    qsortapp_6(Right0, Right),
    append(Left, [Pivot | Right], List).

%---------------------------------------------------------------------------%

% Adding a tail recursion pragma to something that is not recursive is an
% error.
:- pragma require_tail_recursion(cons/2).
cons(X, Xs) = [X | Xs].

%---------------------------------------------------------------------------%

:- pred partition(int::in, list(int)::in, list(int)::in, list(int)::out,
    list(int)::in, list(int)::out) is det.

partition(_Pivot, [], Left, Left, Right, Right).
partition(Pivot, [H | T], Left0, Left, Right0, Right) :-
    ( if H < Pivot then
        partition(Pivot, T, [H | Left0], Left, Right0, Right)
    else
        partition(Pivot, T, Left0, Left, [H | Right0], Right)
    ).
