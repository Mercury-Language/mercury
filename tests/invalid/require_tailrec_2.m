%
% Require tail recursion pragma tests with --warn-non-tail-recursive
:- module require_tailrec_2.

:- interface.

:- import_module int.
:- import_module list.

:- pred qsortapp_1(list(int)::in, list(int)::out) is det.
:- pred qsortapp_2(list(int)::in, list(int)::out) is det.
:- pred qsortapp_3(list(int)::in, list(int)::out) is det.
:- pred qsortapp_4(list(int)::in, list(int)::out) is det.
:- pred qsortapp_5(list(int)::in, list(int)::out) is det.
:- pred qsortapp_6(list(int)::in, list(int)::out) is det.

:- func cons(X, list(X)) = list(X).

:- implementation.

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

%-----------------------------------------------------------------------%

% Adding a tail recursion pragma to something that is not recursive is an
% error.
:- pragma require_tail_recursion(cons/2).
cons(X, Xs) = [X | Xs].

%-----------------------------------------------------------------------%

:- pred partition(int::in, list(int)::in, list(int)::in, list(int)::out,
    list(int)::in, list(int)::out) is det.

partition(_Pivot, [], Left, Left, Right, Right).
partition(Pivot, [H | T], Left0, Left, Right0, Right) :-
    ( if H < Pivot then
        partition(Pivot, T, [H | Left0], Left, Right0, Right)
    else
        partition(Pivot, T, Left0, Left, [H | Right0], Right)
    ).

