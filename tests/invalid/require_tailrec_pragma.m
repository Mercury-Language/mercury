%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%
% The .exp  file is for LLDS grades that allow tail recursion.
% The .exp2 file is for MLDS grades that allow tail recursion.
% The .exp3 file is for grades without tail recursion (.debug, .profdeep).
%
% We export the predicates and functions whose diagnostics we wish to test
% because the code in mark_tail_calls.m that generates those diagnostics
% process only the procedures in the dependency graph, and that graph
% contains only the procedures that are reachable from the module interface.
%
%---------------------------------------------------------------------------%

:- module require_tailrec_pragma.

:- interface.

:- import_module bool.
:- import_module int.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred map1(pred(X, Y), list(X), list(Y)).
:- mode map1(pred(in, out) is det, in, out) is det.

:- pred map2(pred(X, Y), list(X), list(Y)).
:- mode map2(pred(in, out) is det, in, out) is det.

%---------------------------------------------------------------------------%

:- func even1(int) = bool.
:- func odd1(int) = bool.

:- func even2(int) = bool.
:- func odd2(int) = bool.

:- func even3(int) = bool.
:- func odd3(int) = bool.

%---------------------------------------------------------------------------%

:- pred qsort_1(list(int)::in, list(int)::out) is det.
:- pred qsort_2(list(int)::in, list(int)::out) is det.
:- pred qsort_3(list(int)::in, list(int)::out) is det.
:- pred qsort_4(list(int)::in, list(int)::out) is det.
:- pred qsort_5(list(int)::in, list(int)::out) is det.
:- pred qsort_6(list(int)::in, list(int)::out) is det.
:- pred qsort_7(list(int)::in, list(int)::out) is det.

%---------------------------------------------------------------------------%

:- func cons(X, list(X)) = list(X).

%---------------------------------------------------------------------------%
:- implementation.
%---------------------------------------------------------------------------%

    % Self non-tail recursion with self_recursion_only pragma.
    %
:- pragma require_tail_recursion(pred(map1/3), [self_recursion_only]).

map1(_, [], []).
map1(P, [X | Xs], [Y | Ys]) :-
    P(X, Y),
    map1(P, Xs, Ys).

    % Self non-tail recursion with self_or_mutual_recursion pragma.
    %
:- pragma require_tail_recursion(map2/3, [self_or_mutual_recursion]).

map2(_, [], []).
map2(P, [X | Xs], [Y | Ys]) :-
    P(X, Y),
    map2(P, Xs, Ys).

%---------------------------------------------------------------------------%
% NOTE Without the noinline pragmas below, the compiler could generate
% somewhat different diagnostic messages at higher optimization levels.
%---------------------------------------------------------------------------%

    % Mutual non-tail recursion with self_or_mutual_recursion pragma.
    %
:- pragma require_tail_recursion(func(even1/1), [self_or_mutual_recursion]).
:- pragma no_inline(even1/1).

even1(N) =
    ( if N = 0 then
        yes
    else
        bool.not(odd1(N))
    ).

    % Mutual tail recursion with self_or_mutual_recursion pragma.
    % This should get a diagnostic ONLY in MLDS grades.
    %
:- pragma require_tail_recursion(odd1/1, [self_or_mutual_recursion]).
:- pragma no_inline(odd1/1).

odd1(N) =
    ( if N = 0 then
        no
    else
        even1(N - 1)
    ).

%---------------------%

    % A repeat of even1/1, but asking for an error, not a warning.
    %
:- pragma require_tail_recursion(even2/1, [error, self_or_mutual_recursion]).
:- pragma no_inline(even2/1).

even2(N) =
    ( if N = 0 then
        yes
    else
        bool.not(odd2(N))
    ).

:- pragma no_inline(odd2/1).

odd2(N) =
    ( if N = 0 then
        no
    else
        even2(N - 1)
    ).

%---------------------%

    % A repeat of even1/1, but not specifying the severity.
    %
:- pragma require_tail_recursion(func(even3/1)).
:- pragma no_inline(even3/1).

even3(N) =
    ( if N = 0 then
        yes
    else
        bool.not(odd3(N))
    ).

:- pragma no_inline(odd3/1).

odd3(N) =
    ( if N = 0 then
        no
    else
        even3(N - 1)
    ).

%---------------------------------------------------------------------------%

    % Test the default operation of this pragma.
    %
:- pragma require_tail_recursion(qsort_1/2).

qsort_1([], []).
qsort_1([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsort_1(Left0, Left),
    qsort_1(Right0, Right),
    append(Left, [Pivot | Right], List).

    % Test the default operation of this pragma when the relevant call
    % is in a scope where the relevant warning is disabled.
    %
:- pragma require_tail_recursion(qsort_2/2, []).

qsort_2([], []).
qsort_2([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsort_2(Left0, Left),
    disable_warnings [non_tail_recursive_calls] qsort_2(Right0, Right),
    append(Left, [Pivot | Right], List).

    % Test the warn option.
    %
:- pragma require_tail_recursion(qsort_3/2, [warn]).

qsort_3([], []).
qsort_3([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsort_3(Left0, Left),
    qsort_3(Right0, Right),
    append(Left, [Pivot | Right], List).

    % Test the error option.
    %
:- pragma require_tail_recursion(qsort_4/2, [error]).

qsort_4([], []).
qsort_4([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsort_4(Left0, Left),
    qsort_4(Right0, Right),
    append(Left, [Pivot | Right], List).

    % Test (sort-of) the self_recursion_only option.
    %
:- pragma require_tail_recursion(qsort_5/2, [self_recursion_only]).

qsort_5([], []).
qsort_5([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsort_5(Left0, Left),
    qsort_5(Right0, Right),
    append(Left, [Pivot | Right], List).

    % Test (sort-of) the self_or_mutual_recursion option.
    %
:- pragma require_tail_recursion(qsort_6/2, [self_or_mutual_recursion]).

qsort_6([], []).
qsort_6([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsort_6(Left0, Left),
    qsort_6(Right0, Right),
    append(Left, [Pivot | Right], List).

    % Test the in_all_grades option.
    %
:- pragma require_tail_recursion(qsort_7/2,
    [error, in_all_grades, self_or_mutual_recursion]).

qsort_7([], []).
qsort_7([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsort_7(Left0, Left),
    qsort_7(Right0, Right),
    append(Left, [Pivot | Right], List).

    % Auxiliary predicate for the qsort_N predicates above.
    %
:- pred partition(int::in, list(int)::in, list(int)::in, list(int)::out,
    list(int)::in, list(int)::out) is det.

partition(_Pivot, [], Left, Left, Right, Right).
partition(Pivot, [H | T], Left0, Left, Right0, Right) :-
    ( if H < Pivot then
        partition(Pivot, T, [H | Left0], Left, Right0, Right)
    else
        partition(Pivot, T, Left0, Left, [H | Right0], Right)
    ).

%---------------------------------------------------------------------------%

% Adding this pragma to a non-recursive function is an error.
%
% This test case is expected to fail, generating useful diagnostics
% in grades that DO support tail recursion, we need it to also fail
% in grades that do NOT support tail recursion. The "in_all_grades" option
% is here to ensure just that.
:- pragma require_tail_recursion(func(cons/2), [in_all_grades, error]).

cons(X, Xs) = [X | Xs].

%---------------------------------------------------------------------------%
