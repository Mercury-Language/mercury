%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Module `lazy_list' - this module defines a `lazy_list' type that
% is like a lazily-evaluated version of the `lazy_list' type.
%
% Main author: fjh.
% Stability: medium.
%
%---------------------------------------------------------------------------%

:- module lazy_list.
:- interface.

:- import_module list.

    % The definition of the type `lazy_list(T)':
    %   A lazy lazy_list is either an empty lazy_list, denoted `[]',
    %   or an element `Head' of type `T' followed by a tail `Tail'
    %   of type `lazy_list(T)', denoted `[Head | Tail]',
    %   or predicate `P' which when evaluated yields a lazy lazy_list,
    %   denoted `lazy(P)'.

:- type lazy_list(T)
    --->    []
    ;       [T | lazy_list(T)]
    ;       lazy(pred(lazy_list(T))).

:- inst lazy_list(I)
    --->    []
    ;       [I | lazy_list(I)]
    ;       lazy(pred(out(lazy_list(I))) is det).
:- inst lazy_list == lazy_list(ground).

    % `whnf' stands for "Weak Head Normal Form";
    % it means the top-level functor is evaluated (not lazy).
    % [Anyone got any ideas for a better name for this?]
:- inst whnf_lazy_list(I)
    --->    []
    ;       [I | lazy_list(I)].
:- inst whnf_lazy_list == whnf_lazy_list(ground).

    % `shnf' stands for "Spine Head Normal Form";
    % it means that the whole skeleton of the lazy_list is evaluated
    % (not lazy).
    % [Anyone got any ideas for a better name for this?]
:- inst shnf_lazy_list(I)
    --->    []
    ;       [I | shnf_lazy_list(I)].
:- inst shnf_lazy_list == shnf_lazy_list(ground).

:- inst empty_lazy_list
    --->    [].

:- inst nonempty_lazy_list(I)
    --->    [I | lazy_list(I)].
:- inst nonempty_lazy_list == nonempty_lazy_list(ground).

%---------------------------------------------------------------------------%

    % Convert a lazy_list to an ordinary list.
:- func lazy_list__to_list(lazy_list(T)) = list(T).
:- mode lazy_list__to_list(in(lazy_list)) = out is det.

    % Convert an ordinary lazy_list to a lazy_list.
:- func lazy_list__from_list(list(T)) = lazy_list(T).
:- mode lazy_list__from_list(in) = out(lazy_list) is det.

    % Check whether two lazy lists represent the same list.
:- pred lazy_list__equal(lazy_list(T), lazy_list(T)).
:- mode lazy_list__equal(in(lazy_list), in(lazy_list)) is semidet.

    % Extract the head of a lazy lazy_list
    % (but it's often better to use lazy_list__eval).
:- func lazy_list__head(lazy_list(T)) = T.
:- mode lazy_list__head(in(lazy_list)) = out is semidet.
:- mode lazy_list__head(in(nonempty_lazy_list)) = out is det.

    % Extract the tail of a lazy lazy_list
    % (but it's often better to use lazy_list__eval).
:- func lazy_list__tail(lazy_list(T)) = lazy_list(T).
:- mode lazy_list__tail(in(lazy_list)) = out(lazy_list) is semidet.
:- mode lazy_list__tail(in(nonempty_lazy_list)) = out(lazy_list) is det.

    % Evaluate the top-level functor of a lazy lazy_list
    % to either nil or cons.
:- func lazy_list__eval(lazy_list(T)) = lazy_list(T).
:- mode lazy_list__eval(in(lazy_list)) = out(whnf_lazy_list) is det.

%---------------------------------------------------------------------------%

    % Predicate versions of the above functions.
    % (This is just so you can use SICStus or NU-Prolog
    % for debugging; they will be obsolete when we
    % have a Mercury debugger that handles functions).

% :- pragma obsolete(lazy_list__to_list/2).
:- pred lazy_list__to_list(lazy_list(T), lazy_list(T)).
:- mode lazy_list__to_list(in(lazy_list), out) is det.

% :- pragma obsolete(lazy_list__from_list/2).
:- pred lazy_list__from_list(list(T), lazy_list(T)).
:- mode lazy_list__from_list(in, out(lazy_list)) is det.

% :- pragma obsolete(lazy_list__eval/2).
:- pred lazy_list__eval(lazy_list(T), lazy_list(T)).
:- mode lazy_list__eval(in(lazy_list), out(whnf_lazy_list)) is det.

%---------------------------------------------------------------------------%

    % A lazy_list version of the usual append predicate:
    % lazy_list__append(Start, End, List) is true iff
    % `List' is the result of concatenating `Start' and `End'.
    %
:- pred lazy_list__append(lazy_list(T), lazy_list(T), lazy_list(T)).
:- mode lazy_list__append(in(lazy_list), in(lazy_list), out(lazy_list)) is det.

    % lazy_list__merge(L1, L2, L):
    %   L is the result of merging L1 and L2.
    %   L1 and L2 must be sorted.
:- pred lazy_list__merge(lazy_list(T), lazy_list(T), lazy_list(T)).
:- mode lazy_list__merge(in(lazy_list), in(lazy_list), out(lazy_list)) is det.

    % lazy_list__merge_and_remove_dups(L1, L2, L):
    %   L is the result of merging L1 and L2 and eliminating
    %   any duplicates.
    %   L1 and L2 must be sorted.
:- pred lazy_list__merge_and_remove_dups(lazy_list(T), lazy_list(T),
                    lazy_list(T)).
:- mode lazy_list__merge_and_remove_dups(in(lazy_list), in(lazy_list),
        out(lazy_list)) is det.

    % lazy_list__remove_adjacent_dups(L0, L) :
    %   L is the result of replacing every sequence of duplicate
    %   elements in L0 with a single such element.
:- pred lazy_list__remove_adjacent_dups(lazy_list(T), lazy_list(T)).
:- mode lazy_list__remove_adjacent_dups(in(lazy_list), out(lazy_list)) is det.

    % lazy_list__member(Elem, List) :
    %   True iff `List' contains `Elem'.
:- pred lazy_list__member(T, lazy_list(T)).
:- mode lazy_list__member(in, in(lazy_list)) is semidet.
:- mode lazy_list__member(out, in(nonempty_lazy_list)) is multi.
:- mode lazy_list__member(out, in(lazy_list)) is nondet.

/******************
NOT YET IMPLEMENTED
    % lazy_list__split_list(Len, List, Start, End):
    %   splits `List' into a prefix `Start' of length `Len',
    %   and a remainder `End'.
    %   See also: lazy_list__take, lazy_list__drop.
    %
:- pred lazy_list__split_list(int, lazy_list(T), lazy_list(T), lazy_list(T)).
:- mode lazy_list__split_list(in, in, out, out) is semidet.

    % lazy_list__take(Len, List, Start):
    %   `Start' is the first `Len' elements of `List'.
    %   See also: lazy_list__split_list.
    %
:- pred lazy_list__take(int, lazy_list(T), lazy_list(T)).
:- mode lazy_list__take(in, in, out) is semidet.

    % lazy_list__drop(Len, List, End):
    %   `End' is the remainder of `List' after removing the
    %   first `Len' elements.
    %   See also: lazy_list__split_list.
    %
:- pred lazy_list__drop(int, lazy_list(T), lazy_list(T)).
:- mode lazy_list__drop(in, in, out) is semidet.

    % lazy_list__insert(Elem, List0, List):
    %   `List' is the result of inserting `Elem' somewhere in `List0'.
    %   Same as `lazy_list__delete(List, Elem, List0)'.
    %
:- pred lazy_list__insert(T, lazy_list(T), lazy_list(T)).
:- mode lazy_list__insert(in, out(lazy_list), in(lazy_list)) is nondet.
:- mode lazy_list__insert(in, out(lazy_list), in(lazy_list)) is nondet.
:- mode lazy_list__insert(out, out, in) is nondet.
:- mode lazy_list__insert(in, in, out) is multi.

NOT YET IMPLEMENTED
******************/

    % lazy_list__delete(List, Elem, Remainder):
    %   True iff `Elem' occurs in `List', and
    %   `Remainder' is the result of deleting one occurrence of
    %   `Elem' from `List'.
    %
:- pred lazy_list__delete(lazy_list(T), T, lazy_list(T)).
:- mode lazy_list__delete(in(lazy_list), in, out(lazy_list)) is nondet.
% :- mode lazy_list__delete(in(lazy_list), out, out(lazy_list)) is nondet.

    % lazy_list__delete_first(List0, Elem, List) is true iff Elem
    % occurs in List0 and List is List0 with the first occurence of Elem
    % removed
    %
:- pred lazy_list__delete_first(lazy_list(T), T, lazy_list(T)).
:- mode lazy_list__delete_first(in(lazy_list), in, out(lazy_list)) is semidet.

    % lazy_list__delete_all(List0, Elem, List) is true iff List
    % is List0 with all occurences of Elem removed
    %
:- pred lazy_list__delete_all(lazy_list(T), T, lazy_list(T)).
:- mode lazy_list__delete_all(in(lazy_list), in, out(lazy_list)) is det.

    % lazy_list__delete_elems(List0, Elems, List) is true iff List is
    % List0 with all occurences of all elements of Elems removed
    %
:- pred lazy_list__delete_elems(lazy_list(T), list(T), lazy_list(T)).
:- mode lazy_list__delete_elems(in(lazy_list), in, out(lazy_list)) is det.

/******************
NOT YET IMPLEMENTED

    % lazy_list__replace(lazy_list0, D, R, List) is true iff List is List0
    % with an occurence of D replaced with R.
    %
:- pred lazy_list__replace(lazy_list(T), T, T, lazy_list(T)).
:- mode lazy_list__replace(in, in, in, in) is semidet.
:- mode lazy_list__replace(in, in, in, out) is nondet.

    % lazy_list__replace_first(List0, D, R, List) is true iff List is List0
    % with the first occurence of D replaced with R.
    %
:- pred lazy_list__replace_first(lazy_list(T), T, T, lazy_list(T)).
:- mode lazy_list__replace_first(in, in, in, out) is semidet.

    % lazy_list__replace_all(List0, D, R, List) is true iff List is List0
    % with all occurences of D replaced with R.
    %
:- pred lazy_list__replace_all(lazy_list(T), T, T, lazy_list(T)).
:- mode lazy_list__replace_all(in, in, in, out) is det.

    %   True iff `List' is a permutation of `List0'.
    %
:- pred lazy_list__perm(lazy_list(T), lazy_list(T)).
:- mode lazy_list__perm(in, out) is nondet.

    % lazy_list__duplicate(Count, Elem, List) is true iff List is a list
    % containing Count duplicate copies of Elem.
    %
:- pred lazy_list__duplicate(int, T, lazy_list(T)).
:- mode lazy_list__duplicate(in, in, out) is det.

    % lazy_list__chunk(List, ChunkSize, Chunks):
    %   Takes a list `List' and breaks it into a list of lists `Chunks',
    %   such that the length of each lazy_list in `Chunks' is at most
    %   `ChunkSize.  (More precisely, the length of each list in
    %   `Chunks' other than the last one is exactly `ChunkSize',
    %   and the length of the last list in `Chunks' is between one
    %   and `ChunkSize'.)
    %
:- pred lazy_list__chunk(lazy_list(T), int, lazy_list(lazy_list(T))).
:- mode lazy_list__chunk(in, in, out) is det.

NOT YET IMPLEMENTED
******************/

    % lazy_list__zip(ListA, ListB, List):
    %   List is the result of alternating the elements
    %   of ListA and ListB.  When one of the lists goes to empty,
    %   the remainder of the nonempty list is appended.
    %
:- pred lazy_list__zip(lazy_list(T), lazy_list(T), lazy_list(T)).
:- mode lazy_list__zip(in(lazy_list), in(lazy_list), out(lazy_list)) is det.

    % lazy_list__condense(ListOflazy_lists, List):
    %   `List' is the result of concatenating all the
    %   elements of `ListOflazy_lists'.
    %
:- pred lazy_list__condense(lazy_list(lazy_list(T)), lazy_list(T)).
:- mode lazy_list__condense(in(lazy_list(lazy_list)), out(lazy_list)) is det.

%---------------------------------------------------------------------------%
%
% The following group of predicates use higher-order terms to simplify
% various list processing tasks. They implement pretty much standard
% sorts of operations provided by standard libraries for functional languages.

    % lazy_list__map(T, L, M) uses the closure T
    % to transform the elements of L into the elements of L.
:- pred lazy_list__map(pred(X, Y), lazy_list(X), lazy_list(Y)).
:- mode lazy_list__map(pred(in, out) is det,
        in(lazy_list), out(lazy_list)) is det.

    % lazy_list__filter(Pred, List, TrueList) takes a closure with one
    % input argument and for each member of List `X', calls the closure.
    % Iff call(Pred, X) is true, then X is included in TrueList.
:- pred lazy_list__filter(pred(X), lazy_list(X), lazy_list(X)).
:- mode lazy_list__filter(pred(in) is semidet, in(lazy_list), out(lazy_list))
    is det.

/******************
NOT YET IMPLEMENTED
    % lazy_list__merge(Compare, As, Bs, Sorted) is true iff Sorted is a
    % lazy_list containing the elements of As and Bs in the order implied
    % by their sorted merge. The ordering of elements is defined by
    % the higher order comparison predicate Compare.
:- pred lazy_list__merge(pred(X, X, comparison_result),
            lazy_list(X), lazy_list(X), lazy_list(X)).
:- mode lazy_list__merge(pred(in, in, out) is det,
            in(lazy_list), in(lazy_list), out(lazy_list)) is det.
NOT YET IMPLEMENTED
******************/

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%---------------------------------------------------------------------------%

lazy_list__equal(LazyL1, LazyL2) :-
    lazy_list__to_list(LazyL1, L),
    lazy_list__to_list(LazyL2, L).

% to_list -- function version
lazy_list__to_list([]) = [].
lazy_list__to_list([X | Xs]) = [X | lazy_list__to_list(Xs)].
lazy_list__to_list(lazy(P)) = lazy_list__to_list(Xs) :- P(Xs).

% to_list -- predicate version
lazy_list__to_list([], []).
lazy_list__to_list([X | Xs], [X | Ys]) :-
    lazy_list__to_list(Xs, Ys).
lazy_list__to_list(lazy(P), Ys) :-
    P(Xs),
    lazy_list__to_list(Xs, Ys).

% from_list -- function version
lazy_list__from_list([]) = [].
lazy_list__from_list([X | Xs]) = [X | lazy_list__from_list(Xs)].

% from_list -- predicate version
lazy_list__from_list([], []).
lazy_list__from_list([X | Xs], [X | Ys]) :-
    lazy_list__from_list(Xs, Ys).

% eval -- function version
lazy_list__eval([]) = [].
lazy_list__eval([X | Xs]) = [X | Xs].
lazy_list__eval(lazy(P)) = lazy_list__eval(Xs) :- P(Xs).

% eval -- predicate version
lazy_list__eval([], []).
lazy_list__eval([X | Xs], [X | Xs]).
lazy_list__eval(lazy(P), Ys) :- P(Xs), lazy_list__eval(Xs, Ys).

lazy_list__head([X | _]) = X.
lazy_list__head(lazy(P)) = lazy_list__head(Xs) :- P(Xs).

lazy_list__tail([_ | Ys]) = Ys.
lazy_list__tail(lazy(P)) = lazy_list__tail(Xs) :- P(Xs).

%---------------------------------------------------------------------------%

lazy_list__append([], Ys, Ys).
lazy_list__append([X | Xs], Ys, [X | Zs]) :-
    lazy_list__append(Xs, Ys, Zs).
lazy_list__append(lazy(P0), Ys, lazy(P)) :-
    P = (pred(Zs::out) is det :- P0(Xs), lazy_list__append(Xs, Ys, Zs)).

%---------------------------------------------------------------------------%

lazy_list__condense([], []).
lazy_list__condense([L | Ls], R) :-
    lazy_list__condense(Ls, R1),
    lazy_list__append(L, R1, R).
lazy_list__condense(lazy(P0), lazy(P)) :-
    P = (pred(L::out) is det :- P0(L0), lazy_list__condense(L0, L)).

%---------------------------------------------------------------------------%

/* NYI
lazy_list__insert(Elem, List0, List) :-
    lazy_list__delete(List, Elem, List0).
*/

%---------------------------------------------------------------------------%

lazy_list__delete([X | L], X, L).
lazy_list__delete([X | Xs], Y, [X | L]) :-
    lazy_list__delete(Xs, Y, L).
lazy_list__delete(lazy(P0), Elem, L) :-
    P0(L0),
    lazy_list__delete(L0, Elem, L).

lazy_list__delete_first([X | Xs], Y, Zs) :-
    ( X = Y ->
        Zs = Xs
    ;
        Zs = [X | Zs1],
        lazy_list__delete_first(Xs, Y, Zs1)
    ).
lazy_list__delete_first(lazy(P0), Elem, L) :-
    P0(L0),
    lazy_list__delete_first(L0, Elem, L).

lazy_list__delete_all([], _, []).
lazy_list__delete_all([X | Xs], Y, Zs) :-
    ( X = Y ->
        lazy_list__delete_all(Xs, Y, Zs)
    ;
        Zs = [X | Zs1],
        lazy_list__delete_all(Xs, Y, Zs1)
    ).
lazy_list__delete_all(lazy(P0), Elem, lazy(P)) :-
    P = (pred(L::out) is det :-
        P0(L0), lazy_list__delete_all(L0, Elem, L)).

lazy_list__delete_elems(Xs, [], Xs).
lazy_list__delete_elems(Xs, [E | Es], Zs) :-
    lazy_list__delete_all(Xs, E, Ys),
    lazy_list__delete_elems(Ys, Es, Zs).

%---------------------------------------------------------------------------%

/******************
NOT YET IMPLEMENTED
lazy_list__replace([X | L], X, Z, [Z | L]).
lazy_list__replace([X | Xs], Y, Z, [X | L]) :-
    lazy_list__replace(Xs, Y, Z, L).

lazy_list__replace_first([X | Xs], Y, Z, lazy_list) :-
    (
        X = Y
    ->
        lazy_list = [Z | Xs]
    ;
        lazy_list = [X | L1],
        lazy_list__replace_first(Xs, Y, Z, L1)
    ).

lazy_list__replace_all([], _, _, []).
lazy_list__replace_all([X | Xs], Y, Z, L) :-
    ( X = Y ->
        L = [Z | L0],
        lazy_list__replace_all(Xs, Y, Z, L0)
    ;
        L = [X | L0],
        lazy_list__replace_all(Xs, Y, Z, L0)
    ).
NOT YET IMPLEMENTED
******************/

%---------------------------------------------------------------------------%

lazy_list__member(X, [X | _]).
lazy_list__member(X, [_ | Xs]) :-
    lazy_list__member(X, Xs).
lazy_list__member(X, lazy(P)) :-
    P(Xs), lazy_list__member(X, Xs).

%---------------------------------------------------------------------------%

lazy_list__merge([], L, L).
lazy_list__merge([X | Xs], [], [X | Xs]).
lazy_list__merge([X | Xs], [Y | Ys], [Z | Zs]) :-
    ( compare(<, X, Y) ->
        Z = X,
        lazy_list__merge(Xs, [Y | Ys], Zs)
    ;
        Z = Y,
        lazy_list__merge([X | Xs], Ys, Zs)
    ).
lazy_list__merge([X | Xs], lazy(YsP), lazy(ZsP)) :-
    ZsP = (pred(Zs::out) is det :-
            YsP(Ys),
            lazy_list__merge([X | Xs], Ys, Zs)
        ).
lazy_list__merge(lazy(XsP), Ys, Zs) :-
    XsP(Xs),
    lazy_list__merge(Xs, Ys, Zs).

lazy_list__merge_and_remove_dups([], L, L).
lazy_list__merge_and_remove_dups([X | Xs], [], [X | Xs]).
lazy_list__merge_and_remove_dups([X | Xs], [Y | Ys], L) :-
    compare(Res, X, Y),
    ( Res = (<) ->
        L = [X | Zs],
        lazy_list__merge_and_remove_dups(Xs, [Y | Ys], Zs)
    ; Res = (>) ->
        L = [Y | Zs],
        lazy_list__merge_and_remove_dups([X | Xs], Ys, Zs)
    ;
        lazy_list__merge_and_remove_dups(Xs, [Y | Ys], L)
    ).
lazy_list__merge_and_remove_dups([X | Xs], lazy(P), Zs) :-
    P(Ys),
    lazy_list__merge_and_remove_dups([X | Xs], Ys, Zs).
lazy_list__merge_and_remove_dups(lazy(P), Ys, Zs) :-
    P(Xs),
    lazy_list__merge_and_remove_dups(Xs, Ys, Zs).

%---------------------------------------------------------------------------%

lazy_list__remove_adjacent_dups([], []).
lazy_list__remove_adjacent_dups([X | Xs], L) :-
    lazy_list__remove_adjacent_dups_2(Xs, X, L).
lazy_list__remove_adjacent_dups(lazy(P0), lazy(P)) :-
    P = ( pred(L::out) is det :-
        P0(L0),
        lazy_list__remove_adjacent_dups(L0, L)
    ).

:- pred lazy_list__remove_adjacent_dups_2(lazy_list(T), T, lazy_list(T)).
:- mode lazy_list__remove_adjacent_dups_2(in(lazy_list), in, out(lazy_list))
    is det.

lazy_list__remove_adjacent_dups_2([], X, [X]).
lazy_list__remove_adjacent_dups_2([X1 | Xs], X0, L) :-
    (X0 = X1 ->
        lazy_list__remove_adjacent_dups_2(Xs, X1, L)
    ;
        L = [X0 | L0],
        lazy_list__remove_adjacent_dups_2(Xs, X1, L0)
    ).
lazy_list__remove_adjacent_dups_2(lazy(P0), X, lazy(P)) :-
    P = (pred(L::out) is det :-
            P0(L0),
            lazy_list__remove_adjacent_dups_2(L0, X, L)
        ).

%---------------------------------------------------------------------------%

lazy_list__zip([], Bs, Bs).
lazy_list__zip([A | As], Bs, [A | Cs]) :-
    lazy_list__zip2(As, Bs, Cs).
lazy_list__zip(lazy(P0), Bs, lazy(P)) :-
    P = (pred(Cs::out) is det :- P0(As), lazy_list__zip(As, Bs, Cs)).

:- pred lazy_list__zip2(lazy_list(T), lazy_list(T), lazy_list(T)).
:- mode lazy_list__zip2(in(lazy_list), in(lazy_list), out(lazy_list)) is det.

lazy_list__zip2(As, [], As).
lazy_list__zip2(As, [B | Bs], [B | Cs]) :-
    lazy_list__zip(As, Bs, Cs).
lazy_list__zip2(As, lazy(P0), lazy(P)) :-
    P = (pred(Cs::out) is det :- P0(Bs), lazy_list__zip(As, Bs, Cs)).

%---------------------------------------------------------------------------%

/******************
NOT YET IMPLEMENTED
lazy_list__split_list(N, List, Start, End) :-
    ( N = 0 ->
        Start = [],
        End = lazy_list
    ;
        N > 0,
        N1 = N - 1,
        List = [Head | List1],
        Start = [Head | Start1],
        lazy_list__split_list(N1, List1, Start1, End)
    ).

lazy_list__take(N, As, Bs) :-
    (
        N > 0
    ->
        N1 = N - 1,
        As = [A | As1],
        Bs = [A | Bs1],
        lazy_list__take(N1, As1, Bs1)
    ;
        Bs = []
    ).

lazy_list__drop(N, As, Bs) :-
    (
        N > 0
    ->
        N1 = N - 1,
        As = [_ | Cs],
        lazy_list__drop(N1, Cs, Bs)
    ;
        As = Bs
    ).

%---------------------------------------------------------------------------%

lazy_list__duplicate(N, X, L) :-
    ( N > 0 ->
        N1 = N - 1,
        L = [X | L1],
        lazy_list__duplicate(N1, X, L1)
    ;
        L = []
    ).

%---------------------------------------------------------------------------%

lazy_list__chunk(List, ChunkSize, ListOfSmallLists) :-
    lazy_list__chunk_2(List, ChunkSize, [], ChunkSize, ListOfSmallLists).

:- pred lazy_list__chunk_2(lazy_list(T), int, lazy_list(T), int,
            lazy_list(lazy_list(T))).
:- mode lazy_list__chunk_2(in, in, in, in, out) is det.

lazy_list__chunk_2([], _ChunkSize, List0, _N, Lists) :-
    ( List0 = [] ->
        Lists = []
    ;
        lazy_list__reverse(List0, List),
        Lists = [List]
    ).
lazy_list__chunk_2([X | Xs], ChunkSize, List0, N, Lists) :-
    ( N > 1 ->
        N1 = N - 1,
        lazy_list__chunk_2(Xs, ChunkSize, [X | List0], N1, Lists)
    ;
        lazy_list__reverse([X | List0], List),
        lazy_lists = [List | Lists1],
        lazy_list__chunk_2(Xs, ChunkSize, [], ChunkSize, Lists1)
    ).

%---------------------------------------------------------------------------%

lazy_list__perm([], []).
lazy_list__perm([X | Xs], Ys) :-
    lazy_list__perm(Xs, Ys0),
    lazy_list__insert(X, Ys0, Ys).

%---------------------------------------------------------------------------%

lazy_list__sublazy_list([], _).
lazy_list__sublazy_list([SH | ST], [FH | FT]) :-
    ( SH = FH ->
        lazy_list__sublazy_list(ST, FT)
    ;
        lazy_list__sublazy_list([SH | ST], FT)
    ).

NOT YET IMPLEMENTED
******************/

%---------------------------------------------------------------------------%

lazy_list__map(_, [],  []).
lazy_list__map(P, [H0 | T0], [H | T]) :-
    call(P, H0, H),
    lazy_list__map(P, T0, T).
lazy_list__map(As, lazy(P0), lazy(P)) :-
    P = (pred(Cs::out) is det :- P0(Bs), lazy_list__map(As, Bs, Cs)).

lazy_list__filter(_, [],  []).
lazy_list__filter(P, [H | T], lazy(Pred)) :-
    lazy_list__filter(P, T, L1),
    Pred = (
        pred(L::out) is det :-
            ( P(H) ->
                L = [H | L1]
            ;
                L = L1
            )
        ).
lazy_list__filter(P, lazy(ListP), lazy(TrueP)) :-
    ListP(L0),
    TrueP = (pred(L::out) is det :- lazy_list__filter(P, L0, L)).

/* NYI
lazy_list__merge(_P, [], L, L).
lazy_list__merge(_P, [X | Xs], [], [X | Xs]).
lazy_list__merge(P, [H1 | T1], [H2 | T2], L) :-
    call(P, H1, H2, C),
    (
        C = (<),
        L = [H1 | T],
        lazy_list__merge(P, T1, [H2 | T2], T)
    ;
        C = (=),
        L = [H1, H2 | T],
        lazy_list__merge(P, T1, T2, T)
    ;
        C = (>),
        L = [H2 | T],
        lazy_list__merge(P, [H1 | T1], T2, T)
    ).
NYI */

%---------------------------------------------------------------------------%
