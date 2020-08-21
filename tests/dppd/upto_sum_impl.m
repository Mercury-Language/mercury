%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module upto_sum_impl.

:- interface.

:- pred sumsquaresupto(int::in, int::out) is det.

:- type tree(T)
    --->    leaf(T)
    ;       branch(tree(T), tree(T)).

:- pred sumtrsquaretr(tree(int)::in, int::out) is det.

:- implementation.

:- import_module int.
:- import_module list.

sumsquaresupto(N, S) :-
   upto(1, N, Ns),
   squares(Ns, SONs),
   sum(SONs, S).

sumtrsquaretr(XT, S) :-
   squaretr(XT, SOXt),
   sumtr(SOXt, S).

:- pred upto(int::in, int::in, list(int)::out) is det.

upto(M, N, Ms) :-
    ( if M > N then
        Ms = []
    else
        M1 = M + 1,
        upto(M1, N, Ms1),
        Ms = [M | Ms1]
    ).

:- pred sum(list(int)::in, int::out) is det.

sum(Ns, S) :-
    sum1(Ns, 0, S).

:- pred sum1(list(int)::in, int::in, int::out) is det.

sum1([], S, S).
sum1([N | Ns], A, S) :-
    A1 = A + N,
    sum1(Ns, A1, S).

:- pred square(int::in, int::out) is det.

square(N, SON) :-
    SON = N * N.

:- pred squares(list(int)::in, list(int)::out) is det.

squares([], []).
squares([N | Ns], [SON | SONs]) :-
    square(N, SON),
    squares(Ns, SONs).

:- pred sumtr(tree(int)::in, int::out) is det.

sumtr(leaf(X), X).
sumtr(branch(Xt, Yt), S) :-
    sumtr(Xt, SX),
    sumtr(Yt, SY),
    S = SX + SY.

:- pred squaretr(tree(int)::in, tree(int)::out) is det.

squaretr(leaf(X), leaf(SOX)) :-
    square(X, SOX).
squaretr(branch(Xt, Yt), branch(SOXt, SOYt)) :-
    squaretr(Xt, SOXt),
    squaretr(Yt, SOYt).
