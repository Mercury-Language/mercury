%
% Test the require tail recursion pragma with the
% --no-warn-non-tail-recursion option.  These tests do not raise an error,
% the tests that do raise errors are in invalid/
%

:- module require_tailrec_2.

:- interface.

:- import_module bool.
:- import_module int.
:- import_module list.

:- pred foldl1(pred(X, A, A), list(X), A, A).
:- mode foldl1(pred(in, in, out) is det, in, in, out) is det.

:- pred foldl2(pred(X, A, A), list(X), A, A).
:- mode foldl2(pred(in, in, out) is det, in, in, out) is det.

:- pred foldl3(pred(X, A, A), list(X), A, A).
:- mode foldl3(pred(in, in, out) is det, in, in, out) is det.

:- pred foldl4(pred(X, A, A), list(X), A, A).
:- mode foldl4(pred(in, in, out) is det, in, in, out) is det.

:- pred map1(pred(X, Y), list(X), list(Y)).
:- mode map1(pred(in, out) is det, in, out) is det.

:- pred map2(pred(X, Y), list(X), list(Y)).
:- mode map2(pred(in, out) is det, in, out) is det.

:- func even1(int) = bool.
:- func odd1(int) = bool.

:- func even2(int) = bool.
:- func odd2(int) = bool.

:- func even3(int) = bool.
:- func odd3(int) = bool.

:- func even4(int) = bool.
:- func odd4(int) = bool.

:- func even5(int) = bool.
:- func odd5(int) = bool.

:- func even6(int) = bool.
:- func odd6(int) = bool.

%---------------------------------------------------------------------------%

:- pred qsortapp(list(int)::in, list(int)::out) is det.

:- pred qsortapp_2(list(int)::in, list(int)::out) is det.

:- implementation.

% self tail recursive code with no pragma.
foldl1(_, [], !Acc).
foldl1(P, [X | Xs], !Acc) :-
    P(X, !Acc),
    foldl1(P, Xs, !Acc).

% self tail recursive code with none pragma.
:- pragma require_tail_recursion(foldl2/4, [none]).
foldl2(_, [], !Acc).
foldl2(P, [X | Xs], !Acc) :-
    P(X, !Acc),
    foldl2(P, Xs, !Acc).

% self tail recursive code with self pragma.
:- pragma require_tail_recursion(foldl3/4, [self_recursion_only]).
foldl3(_, [], !Acc).
foldl3(P, [X | Xs], !Acc) :-
    P(X, !Acc),
    foldl3(P, Xs, !Acc).

% self tail recursive code with mutual pragma.
:- pragma require_tail_recursion(foldl4/4, [self_or_mutual_recursion]).
foldl4(_, [], !Acc).
foldl4(P, [X | Xs], !Acc) :-
    P(X, !Acc),
    foldl4(P, Xs, !Acc).

% Self non-tail recursive code with no pragma
map1(_, [], []).
map1(P, [X | Xs], [Y | Ys]) :-
    P(X, Y),
    map1(P, Xs, Ys).

% Self non-tail recursive code with none pragma
:- pragma require_tail_recursion(map2/3, [none]).
map2(_, [], []).
map2(P, [X | Xs], [Y | Ys]) :-
    P(X, Y),
    map2(P, Xs, Ys).

% Mutual tail recursion with no pragma.
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
        even1(N - 1)
    ).

% Mutual tail recursion with none pragma.
:- pragma require_tail_recursion(even2/1, [none]).
even2(N) =
    ( if N = 0 then
        yes
    else
        odd2(N - 1)
    ).
:- pragma require_tail_recursion(odd2/1, [none]).
odd2(N) =
    ( if N = 0 then
        no
    else
        even2(N - 1)
    ).

% Mutual tail recursion with none pragma.
:- pragma require_tail_recursion(even3/1, [self_or_mutual_recursion]).
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
        even3(N - 1)
    ).

even4(N) =
    ( if N = 0 then
        yes
    else
        bool.not(odd4(N))
    ).
odd4(N) =
    ( if N = 0 then
        no
    else
        bool.not(even4(N))
    ).

:- pragma require_tail_recursion(even5/1, [none]).
even5(N) =
    ( if N = 0 then
        yes
    else
        bool.not(odd5(N))
    ).
odd5(N) =
    ( if N = 0 then
        no
    else
        bool.not(even5(N))
    ).

:- pragma require_tail_recursion(even6/1, [self_recursion_only]).
even6(N) =
    ( if N = 0 then
        yes
    else
        bool.not(odd6(N - 1))
    ).
:- pragma require_tail_recursion(odd6/1, [self_recursion_only]).
odd6(N) =
    ( if N = 0 then
        no
    else
        bool.not(even6(N - 1))
    ).

%---------------------------------------------------------------------------%

:- pragma require_tail_recursion(qsortapp/2, [none]).

qsortapp([], []).
qsortapp([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsortapp(Left0, Left),
    qsortapp(Right0, Right),
    append(Left, [Pivot | Right], List).

qsortapp_2([], []).
qsortapp_2([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsortapp_2(Left0, Left),
    qsortapp_2(Right0, Right),
    append(Left, [Pivot | Right], List).

:- pred partition(int::in, list(int)::in, list(int)::in, list(int)::out,
    list(int)::in, list(int)::out) is det.
:- pragma require_tail_recursion(partition/6).

partition(_Pivot, [], Left, Left, Right, Right).
partition(Pivot, [H | T], Left0, Left, Right0, Right) :-
    ( if H < Pivot then
        partition(Pivot, T, [H | Left0], Left, Right0, Right)
    else
        partition(Pivot, T, Left0, Left, [H | Right0], Right)
    ).

