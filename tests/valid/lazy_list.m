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
    %
    % A lazy lazy_list is either an empty lazy_list, denoted `[]',
    % or an element `Head' of type `T' followed by a tail `Tail'
    % of type `lazy_list(T)', denoted `[Head | Tail]',
    % or predicate `P' which when evaluated yields a lazy lazy_list,
    % denoted `lazy(P)'.

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
:- func to_list(lazy_list(T)) = list(T).
:- mode to_list(in(lazy_list)) = out is det.

    % Convert an ordinary lazy_list to a lazy_list.
:- func from_list(list(T)) = lazy_list(T).
:- mode from_list(in) = out(lazy_list) is det.

    % Check whether two lazy lists represent the same list.
:- pred equal(lazy_list(T), lazy_list(T)).
:- mode equal(in(lazy_list), in(lazy_list)) is semidet.

    % Extract the head of a lazy lazy_list
    % (but it's often better to use eval).
:- func head(lazy_list(T)) = T.
:- mode head(in(lazy_list)) = out is semidet.
:- mode head(in(nonempty_lazy_list)) = out is det.

    % Extract the tail of a lazy lazy_list
    % (but it's often better to use eval).
:- func tail(lazy_list(T)) = lazy_list(T).
:- mode tail(in(lazy_list)) = out(lazy_list) is semidet.
:- mode tail(in(nonempty_lazy_list)) = out(lazy_list) is det.

    % Evaluate the top-level functor of a lazy lazy_list
    % to either nil or cons.
:- func eval(lazy_list(T)) = lazy_list(T).
:- mode eval(in(lazy_list)) = out(whnf_lazy_list) is det.

%---------------------------------------------------------------------------%

    % Predicate versions of the above functions.
    % (This is just so you can use SICStus or NU-Prolog
    % for debugging; they will be obsolete when we
    % have a Mercury debugger that handles functions).

% :- pragma obsolete(to_list/2).
:- pred to_list(lazy_list(T), lazy_list(T)).
:- mode to_list(in(lazy_list), out) is det.

% :- pragma obsolete(from_list/2).
:- pred from_list(list(T), lazy_list(T)).
:- mode from_list(in, out(lazy_list)) is det.

% :- pragma obsolete(eval/2).
:- pred eval(lazy_list(T), lazy_list(T)).
:- mode eval(in(lazy_list), out(whnf_lazy_list)) is det.

%---------------------------------------------------------------------------%

    % A lazy_list version of the usual append predicate:
    % append(Start, End, List) is true iff
    % `List' is the result of concatenating `Start' and `End'.
    %
:- pred append(lazy_list(T), lazy_list(T), lazy_list(T)).
:- mode append(in(lazy_list), in(lazy_list), out(lazy_list)) is det.

    % merge(L1, L2, L):
    %   L is the result of merging L1 and L2.
    %   L1 and L2 must be sorted.
:- pred merge(lazy_list(T), lazy_list(T), lazy_list(T)).
:- mode merge(in(lazy_list), in(lazy_list), out(lazy_list)) is det.

    % merge_and_remove_dups(L1, L2, L):
    %   L is the result of merging L1 and L2 and eliminating
    %   any duplicates.
    %   L1 and L2 must be sorted.
:- pred merge_and_remove_dups(lazy_list(T), lazy_list(T), lazy_list(T)).
:- mode merge_and_remove_dups(in(lazy_list), in(lazy_list), out(lazy_list))
    is det.

    % remove_adjacent_dups(L0, L) :
    %   L is the result of replacing every sequence of duplicate
    %   elements in L0 with a single such element.
:- pred remove_adjacent_dups(lazy_list(T), lazy_list(T)).
:- mode remove_adjacent_dups(in(lazy_list), out(lazy_list)) is det.

    % member(Elem, List) :
    %   True iff `List' contains `Elem'.
:- pred member(T, lazy_list(T)).
:- mode member(in, in(lazy_list)) is semidet.
:- mode member(out, in(nonempty_lazy_list)) is multi.
:- mode member(out, in(lazy_list)) is nondet.

/******************
NOT YET IMPLEMENTED
    % split_list(Len, List, Start, End):
    %   splits `List' into a prefix `Start' of length `Len',
    %   and a remainder `End'.
    %   See also: take, drop.
    %
:- pred split_list(int, lazy_list(T), lazy_list(T), lazy_list(T)).
:- mode split_list(in, in, out, out) is semidet.

    % take(Len, List, Start):
    %   `Start' is the first `Len' elements of `List'.
    %   See also: split_list.
    %
:- pred take(int, lazy_list(T), lazy_list(T)).
:- mode take(in, in, out) is semidet.

    % drop(Len, List, End):
    %   `End' is the remainder of `List' after removing the
    %   first `Len' elements.
    %   See also: split_list.
    %
:- pred drop(int, lazy_list(T), lazy_list(T)).
:- mode drop(in, in, out) is semidet.

    % insert(Elem, List0, List):
    %   `List' is the result of inserting `Elem' somewhere in `List0'.
    %   Same as `delete(List, Elem, List0)'.
    %
:- pred insert(T, lazy_list(T), lazy_list(T)).
:- mode insert(in, out(lazy_list), in(lazy_list)) is nondet.
:- mode insert(in, out(lazy_list), in(lazy_list)) is nondet.
:- mode insert(out, out, in) is nondet.
:- mode insert(in, in, out) is multi.

NOT YET IMPLEMENTED
******************/

    % delete(List, Elem, Remainder):
    %   True iff `Elem' occurs in `List', and
    %   `Remainder' is the result of deleting one occurrence of
    %   `Elem' from `List'.
    %
:- pred delete(lazy_list(T), T, lazy_list(T)).
:- mode delete(in(lazy_list), in, out(lazy_list)) is nondet.
% :- mode delete(in(lazy_list), out, out(lazy_list)) is nondet.

    % delete_first(List0, Elem, List) is true iff Elem
    % occurs in List0 and List is List0 with the first occurence of Elem
    % removed
    %
:- pred delete_first(lazy_list(T), T, lazy_list(T)).
:- mode delete_first(in(lazy_list), in, out(lazy_list)) is semidet.

    % delete_all(List0, Elem, List) is true iff List
    % is List0 with all occurences of Elem removed
    %
:- pred delete_all(lazy_list(T), T, lazy_list(T)).
:- mode delete_all(in(lazy_list), in, out(lazy_list)) is det.

    % delete_elems(List0, Elems, List) is true iff List is
    % List0 with all occurences of all elements of Elems removed
    %
:- pred delete_elems(lazy_list(T), list(T), lazy_list(T)).
:- mode delete_elems(in(lazy_list), in, out(lazy_list)) is det.

/******************
NOT YET IMPLEMENTED

    % replace(lazy_list0, D, R, List) is true iff List is List0
    % with an occurence of D replaced with R.
    %
:- pred replace(lazy_list(T), T, T, lazy_list(T)).
:- mode replace(in, in, in, in) is semidet.
:- mode replace(in, in, in, out) is nondet.

    % replace_first(List0, D, R, List) is true iff List is List0
    % with the first occurence of D replaced with R.
    %
:- pred replace_first(lazy_list(T), T, T, lazy_list(T)).
:- mode replace_first(in, in, in, out) is semidet.

    % (List0, D, R, List) is true iff List is List0
    % with all occurences of D replaced with R.
    %
:- pred replace_all(lazy_list(T), T, T, lazy_list(T)).
:- mode replace_all(in, in, in, out) is det.

    %   True iff `List' is a permutation of `List0'.
    %
:- pred perm(lazy_list(T), lazy_list(T)).
:- mode perm(in, out) is nondet.

    % duplicate(Count, Elem, List) is true iff List is a list
    % containing Count duplicate copies of Elem.
    %
:- pred duplicate(int, T, lazy_list(T)).
:- mode duplicate(in, in, out) is det.

    % chunk(List, ChunkSize, Chunks):
    %   Takes a list `List' and breaks it into a list of lists `Chunks',
    %   such that the length of each lazy_list in `Chunks' is at most
    %   `ChunkSize.  (More precisely, the length of each list in
    %   `Chunks' other than the last one is exactly `ChunkSize',
    %   and the length of the last list in `Chunks' is between one
    %   and `ChunkSize'.)
    %
:- pred chunk(lazy_list(T), int, lazy_list(lazy_list(T))).
:- mode chunk(in, in, out) is det.

NOT YET IMPLEMENTED
******************/

    % zip(ListA, ListB, List):
    %   List is the result of alternating the elements
    %   of ListA and ListB.  When one of the lists goes to empty,
    %   the remainder of the nonempty list is appended.
    %
:- pred zip(lazy_list(T), lazy_list(T), lazy_list(T)).
:- mode zip(in(lazy_list), in(lazy_list), out(lazy_list)) is det.

    % condense(ListOflazy_lists, List):
    %   `List' is the result of concatenating all the
    %   elements of `ListOflazy_lists'.
    %
:- pred condense(lazy_list(lazy_list(T)), lazy_list(T)).
:- mode condense(in(lazy_list(lazy_list)), out(lazy_list)) is det.

%---------------------------------------------------------------------------%
%
% The following group of predicates use higher-order terms to simplify
% various list processing tasks. They implement pretty much standard
% sorts of operations provided by standard libraries for functional languages.

    % map(T, L, M) uses the closure T
    % to transform the elements of L into the elements of L.
:- pred map(pred(X, Y), lazy_list(X), lazy_list(Y)).
:- mode map(pred(in, out) is det, in(lazy_list), out(lazy_list)) is det.

    % filter(Pred, List, TrueList) takes a closure with one
    % input argument and for each member of List `X', calls the closure.
    % Iff call(Pred, X) is true, then X is included in TrueList.
:- pred filter(pred(X), lazy_list(X), lazy_list(X)).
:- mode filter(pred(in) is semidet, in(lazy_list), out(lazy_list)) is det.

/******************
NOT YET IMPLEMENTED
    % merge(Compare, As, Bs, Sorted) is true iff Sorted is a
    % lazy_list containing the elements of As and Bs in the order implied
    % by their sorted merge. The ordering of elements is defined by
    % the higher order comparison predicate Compare.
:- pred merge(pred(X, X, comparison_result),
            lazy_list(X), lazy_list(X), lazy_list(X)).
:- mode merge(pred(in, in, out) is det,
            in(lazy_list), in(lazy_list), out(lazy_list)) is det.
NOT YET IMPLEMENTED
******************/

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%---------------------------------------------------------------------------%

equal(LazyL1, LazyL2) :-
    to_list(LazyL1, L),
    to_list(LazyL2, L).

% to_list -- function version
to_list([]) = [].
to_list([X | Xs]) = [X | to_list(Xs)].
to_list(lazy(P)) = to_list(Xs) :- P(Xs).

% to_list -- predicate version
to_list([], []).
to_list([X | Xs], [X | Ys]) :-
    to_list(Xs, Ys).
to_list(lazy(P), Ys) :-
    P(Xs),
    to_list(Xs, Ys).

% from_list -- function version
from_list([]) = [].
from_list([X | Xs]) = [X | from_list(Xs)].

% from_list -- predicate version
from_list([], []).
from_list([X | Xs], [X | Ys]) :-
    from_list(Xs, Ys).

% eval -- function version
eval([]) = [].
eval([X | Xs]) = [X | Xs].
eval(lazy(P)) = eval(Xs) :- P(Xs).

% eval -- predicate version
eval([], []).
eval([X | Xs], [X | Xs]).
eval(lazy(P), Ys) :- P(Xs), eval(Xs, Ys).

head([X | _]) = X.
head(lazy(P)) = head(Xs) :- P(Xs).

tail([_ | Ys]) = Ys.
tail(lazy(P)) = tail(Xs) :- P(Xs).

%---------------------------------------------------------------------------%

append([], Ys, Ys).
append([X | Xs], Ys, [X | Zs]) :-
    append(Xs, Ys, Zs).
append(lazy(P0), Ys, lazy(P)) :-
    P = (pred(Zs::out) is det :- P0(Xs), append(Xs, Ys, Zs)).

%---------------------------------------------------------------------------%

condense([], []).
condense([L | Ls], R) :-
    condense(Ls, R1),
    append(L, R1, R).
condense(lazy(P0), lazy(P)) :-
    P = (pred(L::out) is det :- P0(L0), condense(L0, L)).

%---------------------------------------------------------------------------%

/* NYI
insert(Elem, List0, List) :-
    delete(List, Elem, List0).
*/

%---------------------------------------------------------------------------%

delete([X | L], X, L).
delete([X | Xs], Y, [X | L]) :-
    delete(Xs, Y, L).
delete(lazy(P0), Elem, L) :-
    P0(L0),
    delete(L0, Elem, L).

delete_first([X | Xs], Y, Zs) :-
    ( if X = Y then
        Zs = Xs
    else
        Zs = [X | Zs1],
        delete_first(Xs, Y, Zs1)
    ).
delete_first(lazy(P0), Elem, L) :-
    P0(L0),
    delete_first(L0, Elem, L).

delete_all([], _, []).
delete_all([X | Xs], Y, Zs) :-
    ( if X = Y then
        delete_all(Xs, Y, Zs)
    else
        Zs = [X | Zs1],
        delete_all(Xs, Y, Zs1)
    ).
delete_all(lazy(P0), Elem, lazy(P)) :-
    P =
        ( pred(L::out) is det :-
            P0(L0), delete_all(L0, Elem, L)
        ).

delete_elems(Xs, [], Xs).
delete_elems(Xs, [E | Es], Zs) :-
    delete_all(Xs, E, Ys),
    delete_elems(Ys, Es, Zs).

%---------------------------------------------------------------------------%

/******************
NOT YET IMPLEMENTED
replace([X | L], X, Z, [Z | L]).
replace([X | Xs], Y, Z, [X | L]) :-
    replace(Xs, Y, Z, L).

replace_first([X | Xs], Y, Z, lazy_list) :-
    ( if X = Y then
        lazy_list = [Z | Xs]
    ;
        lazy_list = [X | L1],
        replace_first(Xs, Y, Z, L1)
    ).

replace_all([], _, _, []).
replace_all([X | Xs], Y, Z, L) :-
    ( if X = Y then
        L = [Z | L0],
        replace_all(Xs, Y, Z, L0)
    ;
        L = [X | L0],
        replace_all(Xs, Y, Z, L0)
    ).
NOT YET IMPLEMENTED
******************/

%---------------------------------------------------------------------------%

member(X, [X | _]).
member(X, [_ | Xs]) :-
    member(X, Xs).
member(X, lazy(P)) :-
    P(Xs), member(X, Xs).

%---------------------------------------------------------------------------%

merge([], L, L).
merge([X | Xs], [], [X | Xs]).
merge([X | Xs], [Y | Ys], [Z | Zs]) :-
    ( if compare(<, X, Y) then
        Z = X,
        merge(Xs, [Y | Ys], Zs)
    else
        Z = Y,
        merge([X | Xs], Ys, Zs)
    ).
merge([X | Xs], lazy(YsP), lazy(ZsP)) :-
    ZsP =
        ( pred(Zs::out) is det :-
            YsP(Ys),
            merge([X | Xs], Ys, Zs)
        ).
merge(lazy(XsP), Ys, Zs) :-
    XsP(Xs),
    merge(Xs, Ys, Zs).

merge_and_remove_dups([], L, L).
merge_and_remove_dups([X | Xs], [], [X | Xs]).
merge_and_remove_dups([X | Xs], [Y | Ys], L) :-
    compare(Res, X, Y),
    ( if Res = (<) then
        L = [X | Zs],
        merge_and_remove_dups(Xs, [Y | Ys], Zs)
    else if Res = (>) then
        L = [Y | Zs],
        merge_and_remove_dups([X | Xs], Ys, Zs)
    else
        merge_and_remove_dups(Xs, [Y | Ys], L)
    ).
merge_and_remove_dups([X | Xs], lazy(P), Zs) :-
    P(Ys),
    merge_and_remove_dups([X | Xs], Ys, Zs).
merge_and_remove_dups(lazy(P), Ys, Zs) :-
    P(Xs),
    merge_and_remove_dups(Xs, Ys, Zs).

%---------------------------------------------------------------------------%

remove_adjacent_dups([], []).
remove_adjacent_dups([X | Xs], L) :-
    remove_adjacent_dups_2(Xs, X, L).
remove_adjacent_dups(lazy(P0), lazy(P)) :-
    P =
        ( pred(L::out) is det :-
            P0(L0),
            remove_adjacent_dups(L0, L)
        ).

:- pred remove_adjacent_dups_2(lazy_list(T), T, lazy_list(T)).
:- mode remove_adjacent_dups_2(in(lazy_list), in, out(lazy_list))
    is det.

remove_adjacent_dups_2([], X, [X]).
remove_adjacent_dups_2([X1 | Xs], X0, L) :-
    ( if X0 = X1 then
        remove_adjacent_dups_2(Xs, X1, L)
    else
        L = [X0 | L0],
        remove_adjacent_dups_2(Xs, X1, L0)
    ).
remove_adjacent_dups_2(lazy(P0), X, lazy(P)) :-
    P =
        ( pred(L::out) is det :-
            P0(L0),
            remove_adjacent_dups_2(L0, X, L)
        ).

%---------------------------------------------------------------------------%

zip([], Bs, Bs).
zip([A | As], Bs, [A | Cs]) :-
    zip2(As, Bs, Cs).
zip(lazy(P0), Bs, lazy(P)) :-
    P = (pred(Cs::out) is det :- P0(As), zip(As, Bs, Cs)).

:- pred zip2(lazy_list(T), lazy_list(T), lazy_list(T)).
:- mode zip2(in(lazy_list), in(lazy_list), out(lazy_list)) is det.

zip2(As, [], As).
zip2(As, [B | Bs], [B | Cs]) :-
    zip(As, Bs, Cs).
zip2(As, lazy(P0), lazy(P)) :-
    P = (pred(Cs::out) is det :- P0(Bs), zip(As, Bs, Cs)).

%---------------------------------------------------------------------------%

/******************
NOT YET IMPLEMENTED
split_list(N, List, Start, End) :-
    ( if N = 0 then
        Start = [],
        End = lazy_list
    else
        N > 0,
        N1 = N - 1,
        List = [Head | List1],
        Start = [Head | Start1],
        split_list(N1, List1, Start1, End)
    ).

take(N, As, Bs) :-
    ( if N > 0 then
        N1 = N - 1,
        As = [A | As1],
        Bs = [A | Bs1],
        take(N1, As1, Bs1)
    else
        Bs = []
    ).

drop(N, As, Bs) :-
    ( if N > 0 then
        N1 = N - 1,
        As = [_ | Cs],
        drop(N1, Cs, Bs)
    else
        As = Bs
    ).

%---------------------------------------------------------------------------%

duplicate(N, X, L) :-
    ( if N > 0 then
        N1 = N - 1,
        L = [X | L1],
        duplicate(N1, X, L1)
    else
        L = []
    ).

%---------------------------------------------------------------------------%

chunk(List, ChunkSize, ListOfSmallLists) :-
    chunk_2(List, ChunkSize, [], ChunkSize, ListOfSmallLists).

:- pred chunk_2(lazy_list(T), int, lazy_list(T), int, lazy_list(lazy_list(T))).
:- mode chunk_2(in, in, in, in, out) is det.

chunk_2([], _ChunkSize, List0, _N, Lists) :-
    ( if List0 = [] then
        Lists = []
    else
        reverse(List0, List),
        Lists = [List]
    ).
chunk_2([X | Xs], ChunkSize, List0, N, Lists) :-
    ( if N > 1 then
        N1 = N - 1,
        chunk_2(Xs, ChunkSize, [X | List0], N1, Lists)
    else
        reverse([X | List0], List),
        lazy_lists = [List | Lists1],
        chunk_2(Xs, ChunkSize, [], ChunkSize, Lists1)
    ).

%---------------------------------------------------------------------------%

perm([], []).
perm([X | Xs], Ys) :-
    perm(Xs, Ys0),
    insert(X, Ys0, Ys).

%---------------------------------------------------------------------------%

sublazy_list([], _).
sublazy_list([SH | ST], [FH | FT]) :-
    ( if SH = FH then
        sublazy_list(ST, FT)
    else
        sublazy_list([SH | ST], FT)
    ).

NOT YET IMPLEMENTED
******************/

%---------------------------------------------------------------------------%

map(_, [],  []).
map(P, [H0 | T0], [H | T]) :-
    call(P, H0, H),
    map(P, T0, T).
map(As, lazy(P0), lazy(P)) :-
    P = (pred(Cs::out) is det :- P0(Bs), map(As, Bs, Cs)).

filter(_, [],  []).
filter(P, [H | T], lazy(Pred)) :-
    filter(P, T, L1),
    Pred =
        ( pred(L::out) is det :-
            ( if P(H) then
                L = [H | L1]
            else
                L = L1
            )
        ).
filter(P, lazy(ListP), lazy(TrueP)) :-
    ListP(L0),
    TrueP = (pred(L::out) is det :- filter(P, L0, L)).

/* NYI
merge(_P, [], L, L).
merge(_P, [X | Xs], [], [X | Xs]).
merge(P, [H1 | T1], [H2 | T2], L) :-
    call(P, H1, H2, C),
    (
        C = (<),
        L = [H1 | T],
        merge(P, T1, [H2 | T2], T)
    ;
        C = (=),
        L = [H1, H2 | T],
        merge(P, T1, T2, T)
    ;
        C = (>),
        L = [H2 | T],
        merge(P, [H1 | T1], T2, T)
    ).
NYI */

%---------------------------------------------------------------------------%
