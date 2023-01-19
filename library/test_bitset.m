%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2011-2012 The University of Melbourne.
% Copyright (C) 2014-2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Test operations on bitsets by comparing the output with the output
% from an ordinary set.
%
%---------------------------------------------------------------------------%

:- module test_bitset.

:- interface.

:- import_module enum.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- type test_bitset(T).

:- type bitset_error(T)
    --->    zero_argument(string,
                test_bitset(T))
    ;       one_argument(string,
                test_bitset(T), test_bitset(T))
    ;       two_arguments(string,
                test_bitset(T), test_bitset(T), test_bitset(T)).

%---------------------------------------------------------------------------%
%
% Initial creation of sets.
%

:- func init = test_bitset(T).
:- pred init(test_bitset(T)::out) is det.

:- func singleton_set(T) = test_bitset(T) <= uenum(T).
:- pred singleton_set(test_bitset(T)::out, T::in) is det <= uenum(T).

:- func make_singleton_set(T) = test_bitset(T) <= uenum(T).
:- pred make_singleton_set(test_bitset(T)::out, T::in) is det <= uenum(T).

%---------------------------------------------------------------------------%
%
% Emptiness and singleton-ness tests.
%

:- pred is_empty(test_bitset(T)::in) is semidet.

:- pred is_non_empty(test_bitset(T)::in) is semidet.

:- pred is_singleton(test_bitset(T)::in, T::out) is semidet <= uenum(T).

%---------------------------------------------------------------------------%
%
% Membership tests.
%

:- pred member(T, test_bitset(T)) <= uenum(T).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

:- pred contains(test_bitset(T)::in, T::in) is semidet <= uenum(T).

%---------------------------------------------------------------------------%
%
% Insertions and deletions.
%

:- pred insert(T::in, test_bitset(T)::in, test_bitset(T)::out)
    is det <= uenum(T).
:- pred insert_new(T::in, test_bitset(T)::in, test_bitset(T)::out)
    is semidet <= uenum(T).
:- pred insert_list(list(T)::in, test_bitset(T)::in, test_bitset(T)::out)
    is det <= uenum(T).

:- pred delete(T::in, test_bitset(T)::in, test_bitset(T)::out)
    is det <= uenum(T).
:- pred delete_list(list(T)::in, test_bitset(T)::in, test_bitset(T)::out)
    is det <= uenum(T).

:- pred remove(T::in, test_bitset(T)::in, test_bitset(T)::out)
    is semidet <= uenum(T).
:- pred remove_list(list(T)::in, test_bitset(T)::in, test_bitset(T)::out)
    is semidet <= uenum(T).
:- pred remove_least(T::out, test_bitset(T)::in, test_bitset(T)::out)
    is semidet <= uenum(T).

:- pred remove_leq(test_bitset(T)::in, T::in, test_bitset(T)::out)
    is det <= uenum(T).
:- pred remove_gt(test_bitset(T)::in, T::in, test_bitset(T)::out)
    is det <= uenum(T).

%---------------------------------------------------------------------------%
%
% Comparisons between sets.
%

:- pred equal(test_bitset(T)::in, test_bitset(T)::in) is semidet <= uenum(T).

:- pred subset(test_bitset(T)::in, test_bitset(T)::in) is semidet.

:- pred superset(test_bitset(T)::in, test_bitset(T)::in) is semidet.

%---------------------------------------------------------------------------%
%
% Operations on two or more sets.
%

:- func union(test_bitset(T), test_bitset(T)) = test_bitset(T) <= uenum(T).
:- pred union(test_bitset(T)::in,
    test_bitset(T)::in, test_bitset(T)::out) is det <= uenum(T).

:- func union_list(list(test_bitset(T))) = test_bitset(T) <= uenum(T).
:- pred union_list(list(test_bitset(T))::in, test_bitset(T)::out) is det
    <= uenum(T).

:- func intersect(test_bitset(T), test_bitset(T)) = test_bitset(T) <= uenum(T).
:- pred intersect(test_bitset(T)::in,
    test_bitset(T)::in, test_bitset(T)::out) is det <= uenum(T).

:- func intersect_list(list(test_bitset(T))) = test_bitset(T) <= uenum(T).
:- pred intersect_list(list(test_bitset(T))::in, test_bitset(T)::out) is det
    <= uenum(T).

:- func difference(test_bitset(T), test_bitset(T)) = test_bitset(T)
    <= uenum(T).
:- pred difference(test_bitset(T)::in,
    test_bitset(T)::in, test_bitset(T)::out) is det <= uenum(T).

%---------------------------------------------------------------------------%
%
% Operations that divide a set into two parts.
%

:- pred divide(pred(T)::in(pred(in) is semidet), test_bitset(T)::in,
    test_bitset(T)::out, test_bitset(T)::out) is det <= uenum(T).

:- pred divide_by_set(test_bitset(T)::in, test_bitset(T)::in,
    test_bitset(T)::out, test_bitset(T)::out) is det <= uenum(T).

%---------------------------------------------------------------------------%
%
% Converting lists to sets.
%

:- func list_to_set(list(T)) = test_bitset(T) <= uenum(T).
:- pred list_to_set(list(T)::in, test_bitset(T)::out) is det <= uenum(T).

:- func sorted_list_to_set(list(T)) = test_bitset(T) <= uenum(T).
:- pred sorted_list_to_set(list(T)::in, test_bitset(T)::out) is det
    <= uenum(T).

%---------------------------------------------------------------------------%
%
% Converting sets to lists.
%

:- func to_sorted_list(test_bitset(T)) = list(T) <= uenum(T).
:- pred to_sorted_list(test_bitset(T)::in, list(T)::out) is det <= uenum(T).

%---------------------------------------------------------------------------%
%
% Converting between different kinds of sets.
%

:- func set_to_bitset(set(T)) = test_bitset(T) <= uenum(T).
:- func from_set(set(T)) = test_bitset(T) <= uenum(T).

:- func bitset_to_set(test_bitset(T)) = set(T) <= uenum(T).
:- func to_set(test_bitset(T)) = set(T) <= uenum(T).

%---------------------------------------------------------------------------%
%
% Counting.
%

:- func count(test_bitset(T)) = int <= uenum(T).

%---------------------------------------------------------------------------%
%
% Standard higher order functions on collections.
%

    % all_true(Pred, Set) succeeds iff Pred(Element) succeeds
    % for all the elements of Set.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet), test_bitset(T)::in)
    is semidet <= uenum(T).

:- func filter(pred(T)::in(pred(in) is semidet), test_bitset(T)::in)
    = (test_bitset(T)::out) is det <= uenum(T).
:- pred filter(pred(T)::in(pred(in) is semidet),
    test_bitset(T)::in, test_bitset(T)::out, test_bitset(T)::out)
    is det <= uenum(T).

:- func foldl(func(T, Acc) = Acc, test_bitset(T), Acc) = Acc <= uenum(T).
:- mode foldl(func(in, in) = out is det, in, in) = out is det.

:- pred foldl(pred(T, Acc, Acc), test_bitset(T), Acc, Acc) <= uenum(T).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module maybe.
:- import_module require.
:- import_module set_ordlist.
:- import_module solutions.
:- import_module string.
:- import_module tree_bitset.
:- import_module uint.

:- type test_bitset(T)
    --->    tb(tree_bitset(T), set_ordlist(T)).

%---------------------------------------------------------------------------%

init = tb(tree_bitset.init, set_ordlist.init).

init(init).

singleton_set(A) =
    tb(tree_bitset.make_singleton_set(A), set_ordlist.make_singleton_set(A)).

singleton_set(test_bitset.singleton_set(A), A).

make_singleton_set(A) =
    tb(tree_bitset.make_singleton_set(A), set_ordlist.make_singleton_set(A)).

make_singleton_set(test_bitset.make_singleton_set(A), A).

%---------------------------------------------------------------------------%

is_empty(tb(A, B)) :-
    ( if tree_bitset.is_empty(A) then EmptyA = yes else EmptyA = no),
    ( if set_ordlist.is_empty(B) then EmptyB = yes else EmptyB = no),
    ( if EmptyA = EmptyB then
        EmptyA = yes
    else
        unexpected($pred, "failed")
    ).

is_non_empty(tb(A, B)) :-
    ( if tree_bitset.is_non_empty(A) then NonEmptyA = yes else NonEmptyA = no),
    ( if set_ordlist.is_non_empty(B) then NonEmptyB = yes else NonEmptyB = no),
    ( if NonEmptyA = NonEmptyB then
        NonEmptyA = yes
    else
        unexpected($pred, "failed")
    ).

is_singleton(tb(A, B), E) :-
    ( if tree_bitset.is_singleton(A, AE) then
        NonEmptyA = yes(AE)
    else
        NonEmptyA = no
    ),
    ( if set_ordlist.is_singleton(B, BE) then
        NonEmptyB = yes(BE)
    else
        NonEmptyB = no
    ),
    ( if NonEmptyA = NonEmptyB then
        NonEmptyA = yes(E)
    else
        unexpected($pred, "failed")
    ).

%---------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(pred(member/2)).

member(E::in, tb(SetA, SetB)::in) :-
    ( if tree_bitset.member(E, SetA) then InSetA = yes else InSetA = no),
    ( if set_ordlist.member(E, SetB) then InSetB = yes else InSetB = no),
    ( if InSetA = InSetB then
        InSetA = yes
    else
        unexpected($pred, "failed (in, in)")
    ).

member(E::out, tb(SetA, SetB)::in) :-
    PredA = (pred(EA::out) is nondet :- tree_bitset.member(EA, SetA)),
    PredB = (pred(EB::out) is nondet :- set_ordlist.member(EB, SetB)),
    solutions(PredA, SolnsA),
    solutions(PredB, SolnsB),
    ( if SolnsA = SolnsB then
        tree_bitset.member(E, SetA)
    else
        unexpected($pred, "failed (out, in)")
    ).

contains(tb(SetA, SetB), E) :-
    ( if tree_bitset.contains(SetA, E) then InSetA = yes else InSetA = no),
    ( if set_ordlist.contains(SetB, E) then InSetB = yes else InSetB = no),
    ( if InSetA = InSetB then
        InSetA = yes
    else
        unexpected($pred, "failed")
    ).

%---------------------------------------------------------------------------%

insert(E, tb(SetA0, SetB0), Result) :-
    tree_bitset.insert(E, SetA0, SetA),
    set_ordlist.insert(E, SetB0, SetB),
    check1("insert", tb(SetA0, SetB0), tb(SetA, SetB), Result).

insert_new(E, tb(SetA0, SetB0), Result) :-
    ( if tree_bitset.insert_new(E, SetA0, SetA) then
        ( if set_ordlist.insert_new(E, SetB0, SetB) then
            check1("insert", tb(SetA0, SetB0), tb(SetA, SetB), Result)
        else
            unexpected($pred, "success/fail in tree_bitset/set_ordlist")
        )
    else
        ( if set_ordlist.insert_new(E, SetB0, _SetB) then
            unexpected($pred, "fail/success in tree_bitset/set_ordlist")
        else
            % insert_new failed in both tree_bitset and set_ordlist.
            fail
        )
    ).

insert_list(Es, tb(SetA0, SetB0), Result) :-
    tree_bitset.insert_list(Es, SetA0, SetA),
    set_ordlist.insert_list(Es, SetB0, SetB),
    check1("insert_list", tb(SetA0, SetB0), tb(SetA, SetB), Result).

delete(E, tb(SetA0, SetB0), Result) :-
    tree_bitset.delete(E, SetA0, SetA),
    set_ordlist.delete(E, SetB0, SetB),
    check1("delete", tb(SetA0, SetB0), tb(SetA, SetB), Result).

delete_list(Es, tb(SetA0, SetB0), Result) :-
    tree_bitset.delete_list(Es, SetA0, SetA),
    set_ordlist.delete_list(Es, SetB0, SetB),
    check1("delete_list", tb(SetA0, SetB0), tb(SetA, SetB), Result).

remove(E, tb(SetA0, SetB0), Result) :-
    ( if tree_bitset.remove(E, SetA0, SetA1) then
        ( if set_ordlist.remove(E, SetB0, SetB1) then
            SetA = SetA1,
            SetB = SetB1,
            check1("remove", tb(SetA0, SetB0), tb(SetA, SetB), Result)
        else
            unexpected($pred, "unexpected success")
        )
    else if set_ordlist.remove(E, SetB0, _) then
        unexpected($pred, "unexpected failure")
    else
        fail
    ).

remove_list(Es, tb(SetA0, SetB0), Result) :-
    ( if tree_bitset.remove_list(Es, SetA0, SetA1) then
        ( if set_ordlist.remove_list(Es, SetB0, SetB1) then
            SetA = SetA1,
            SetB = SetB1,
            check1("remove_list", tb(SetA0, SetB0), tb(SetA, SetB), Result)
        else
            unexpected($pred, "unexpected success")
        )
    else if set_ordlist.remove_list(Es, SetB0, _) then
        unexpected($pred, "unexpected failure")
    else
        fail
    ).

remove_least(Least, tb(SetA0, SetB0), Result) :-
    ( if tree_bitset.remove_least(LeastA, SetA0, SetA1) then
        ( if set_ordlist.remove_least(LeastB, SetB0, SetB1) then
            ( if LeastA = LeastB then
                Least = LeastA,
                check1("remove_least", tb(SetA0, SetB0), tb(SetA1, SetB1),
                    Result)
            else
                unexpected($pred, "wrong least element")
            )
        else
            unexpected($pred, "should be no least value")
        )
    else if set_ordlist.remove_least(_, SetB0, _) then
        unexpected($pred, "failed")
    else
        fail
    ).

remove_leq(tb(SetA0, SetB0), Hurdle, Result) :-
    remove_leq(Hurdle, SetA0, SetA),
    RemoveLeq =
        ( pred(Item::in) is semidet :-
            Index = to_uint(Item),
            HurdleIndex = to_uint(Hurdle),
            not (Index =< HurdleIndex)
        ),
    set.filter(RemoveLeq, SetB0, SetB),
    check1("remove_leq", tb(SetA0, SetB0), tb(SetA, SetB), Result).

remove_gt(tb(SetA0, SetB0), Hurdle, Result) :-
    remove_gt(Hurdle, SetA0, SetA),
    RemoveGt =
        ( pred(Item::in) is semidet :-
            Index = to_uint(Item),
            HurdleIndex = to_uint(Hurdle),
            not (Index > HurdleIndex)
        ),
    set.filter(RemoveGt, SetB0, SetB),
    check1("remove_gt", tb(SetA0, SetB0), tb(SetA, SetB), Result).

%---------------------------------------------------------------------------%

equal(tb(SetA1, SetB1), tb(SetA2, SetB2)) :-
    ( if tree_bitset.equal(SetA1, SetA2) then EqualA = yes else EqualA = no),
    ( if set_ordlist.equal(SetB1, SetB2) then EqualB = yes else EqualB = no),
    ( if EqualA = EqualB then
        EqualA = yes
    else
        unexpected($pred, "failed")
    ).

subset(tb(SetA1, SetB1), tb(SetA2, SetB2)) :-
    ( if tree_bitset.subset(SetA1, SetA2) then
        ( if set_ordlist.subset(SetB1, SetB2) then
            true
        else
            unexpected($pred, "unexpected success")
        )
    else if set_ordlist.subset(SetB1, SetB2) then
        unexpected($pred, "unexpected failure")
    else
        fail
    ).

superset(tb(SetA1, SetB1), tb(SetA2, SetB2)) :-
    ( if tree_bitset.superset(SetA1, SetA2) then
        ( if set_ordlist.superset(SetB1, SetB2) then
            true
        else
            unexpected($pred, "unexpected success")
        )
    else if set_ordlist.superset(SetB1, SetB2) then
        unexpected($pred, "unexpected failure")
    else
        fail
    ).

%---------------------------------------------------------------------------%

union(tb(SetA1, SetB1), tb(SetA2, SetB2)) = Result :-
    tree_bitset.union(SetA1, SetA2, SetA),
    set_ordlist.union(SetB1, SetB2, SetB),
    check2("union", tb(SetA1, SetB1), tb(SetA2, SetB2), tb(SetA, SetB),
        Result).

union(A, B, test_bitset.union(A, B)).

union_list(SetsAB) = Result :-
    get_sets("union_list", SetsAB, SetsA, SetsB),
    SetA = tree_bitset.union_list(SetsA),
    SetB = set_ordlist.union_list(SetsB),
    check0("union_list", tb(SetA, SetB), Result).

union_list(Sets, test_bitset.union_list(Sets)).

intersect(tb(SetA1, SetB1), tb(SetA2, SetB2)) = Result :-
    tree_bitset.intersect(SetA1, SetA2, SetA),
    set_ordlist.intersect(SetB1, SetB2, SetB),
    check2("intersect", tb(SetA1, SetB1), tb(SetA2, SetB2), tb(SetA, SetB),
        Result).

intersect(A, B, test_bitset.intersect(A, B)).

intersect_list(SetsAB) = Result :-
    get_sets("intersect_list", SetsAB, SetsA, SetsB),
    SetA = tree_bitset.intersect_list(SetsA),
    SetB = set_ordlist.intersect_list(SetsB),
    check0("intersect_list", tb(SetA, SetB), Result).

intersect_list(Sets, test_bitset.intersect_list(Sets)).

difference(tb(SetA1, SetB1), tb(SetA2, SetB2)) = Result :-
    tree_bitset.difference(SetA1, SetA2, SetA),
    set_ordlist.difference(SetB1, SetB2, SetB),
    check2("difference", tb(SetA1, SetB1), tb(SetA2, SetB2), tb(SetA, SetB),
        Result).

difference(A, B, test_bitset.difference(A, B)).

%---------------------%

:- pred get_sets(string::in, list(test_bitset(T))::in,
    list(tree_bitset(T))::out, list(set_ordlist(T))::out) is det <= uenum(T).

get_sets(_, [], [], []).
get_sets(Op, [tb(SetA, SetB) | SetsAB], [SetA | SetsA], [SetB | SetsB]) :-
    tree_bitset.to_sorted_list(SetA, SetListA),
    set_ordlist.to_sorted_list(SetB, SetListB),
    ( if SetListA = SetListB then
        get_sets(Op, SetsAB, SetsA, SetsB)
    else
        unexpected($pred, "unequal sets in " ++ Op ++ " arg list")
    ).

%---------------------------------------------------------------------------%

divide(Pred, tb(SetA, SetB), ResultIn, ResultOut) :-
    tree_bitset.divide(Pred, SetA, InSetA, OutSetA),
    set_ordlist.divide(Pred, SetB, InSetB, OutSetB),

    tree_bitset.to_sorted_list(SetA, SetListA),
    set_ordlist.to_sorted_list(SetB, SetListB),
    tree_bitset.to_sorted_list(InSetA, InSetListA),
    set_ordlist.to_sorted_list(InSetB, InSetListB),
    tree_bitset.to_sorted_list(OutSetA, OutSetListA),
    set_ordlist.to_sorted_list(OutSetB, OutSetListB),
    ( if
        SetListA = SetListB,
        InSetListA = InSetListB,
        OutSetListA = OutSetListB
    then
        ResultIn = tb(InSetA, InSetB),
        ResultOut = tb(OutSetA, OutSetB)
    else
        unexpected($pred, "failed")
    ).

divide_by_set(tb(DivideBySetA, DivideBySetB), tb(SetA, SetB),
        ResultIn, ResultOut) :-
    tree_bitset.divide_by_set(DivideBySetA, SetA, InSetA, OutSetA),
    set_ordlist.divide_by_set(DivideBySetB, SetB, InSetB, OutSetB),

    tree_bitset.to_sorted_list(DivideBySetA, DivideBySetListA),
    set_ordlist.to_sorted_list(DivideBySetB, DivideBySetListB),
    tree_bitset.to_sorted_list(SetA, SetListA),
    set_ordlist.to_sorted_list(SetB, SetListB),
    tree_bitset.to_sorted_list(InSetA, InSetListA),
    set_ordlist.to_sorted_list(InSetB, InSetListB),
    tree_bitset.to_sorted_list(OutSetA, OutSetListA),
    set_ordlist.to_sorted_list(OutSetB, OutSetListB),
    ( if
        DivideBySetListA = DivideBySetListB,
        SetListA = SetListB,
        InSetListA = InSetListB,
        OutSetListA = OutSetListB
    then
        ResultIn = tb(InSetA, InSetB),
        ResultOut = tb(OutSetA, OutSetB)
    else
        unexpected($pred, "failed")
    ).

%---------------------------------------------------------------------------%

list_to_set(List) = Result :-
    check0("list_to_set",
        tb(tree_bitset.list_to_set(List), set_ordlist.list_to_set(List)),
        Result).

list_to_set(A, test_bitset.list_to_set(A)).

sorted_list_to_set(List) = Result :-
    check0("sorted_list_to_set",
        tb(tree_bitset.sorted_list_to_set(List),
            set_ordlist.sorted_list_to_set(List)),
        Result).

sorted_list_to_set(A, test_bitset.sorted_list_to_set(A)).

%---------------------------------------------------------------------------%

to_sorted_list(tb(A, B)) = List :-
    ListA = tree_bitset.to_sorted_list(A),
    ListB = set_ordlist.to_sorted_list(B),
    ( if ListA = ListB then
        List = ListB
    else
        unexpected($pred, "failed")
    ).

to_sorted_list(A, test_bitset.to_sorted_list(A)).

%---------------------------------------------------------------------------%

set_to_bitset(Set) = tb(A, B) :-
    set.to_sorted_list(Set, SortedList),
    tb(A, B) = test_bitset.sorted_list_to_set(SortedList).

from_set(Set) = set_to_bitset(Set).

bitset_to_set(tb(A, B)) = Set :-
    SortedList = test_bitset.to_sorted_list(tb(A, B)),
    set.sorted_list_to_set(SortedList, Set).

to_set(Set) = bitset_to_set(Set).

%---------------------------------------------------------------------------%

count(tb(SetA, SetB)) = Count :-
    CountA = tree_bitset.count(SetA),
    CountB = set_ordlist.count(SetB),
    ( if CountA = CountB then
        Count = CountA
    else
        unexpected($pred, "failed")
    ).

%---------------------------------------------------------------------------%

all_true(Pred, tb(SetA, SetB)) :-
    ( if tree_bitset.all_true(Pred, SetA) then
        ( if set_ordlist.all_true(Pred, SetB) then
            true
        else
            unexpected($pred, "tree_bitset but not set_ordlist")
        )
    else
        ( if set_ordlist.all_true(Pred, SetB) then
            unexpected($pred, "set_ordlist but not tree_bitset")
        else
            fail
        )
    ).

filter(Pred, tb(SetA, SetB)) = Result :-
    tree_bitset.to_sorted_list(SetA, SetListA),
    set_ordlist.to_sorted_list(SetB, SetListB),
    InSetA = tree_bitset.filter(Pred, SetA),
    InSetB = set_ordlist.filter(Pred, SetB),
    tree_bitset.to_sorted_list(InSetA, InSetListA),
    set_ordlist.to_sorted_list(InSetB, InSetListB),
    ( if SetListA = SetListB, InSetListA = InSetListB then
        Result = tb(InSetA, InSetB)
    else
        unexpected($pred, "failed")
    ).

filter(Pred, tb(SetA, SetB), ResultIn, ResultOut) :-
    tree_bitset.to_sorted_list(SetA, SetListA),
    set_ordlist.to_sorted_list(SetB, SetListB),
    tree_bitset.filter(Pred, SetA, InSetA, OutSetA),
    set_ordlist.filter(Pred, SetB, InSetB, OutSetB),
    tree_bitset.to_sorted_list(InSetA, InSetListA),
    set_ordlist.to_sorted_list(InSetB, InSetListB),
    tree_bitset.to_sorted_list(OutSetA, OutSetListA),
    set_ordlist.to_sorted_list(OutSetB, OutSetListB),
    ( if
        SetListA = SetListB,
        InSetListA = InSetListB,
        OutSetListA = OutSetListB
    then
        ResultIn = tb(InSetA, InSetB),
        ResultOut = tb(OutSetA, OutSetB)
    else
        unexpected($pred, "failed")
    ).

foldl(Pred, tb(SetA, SetB), Acc0) = Acc :-
    tree_bitset.to_sorted_list(SetA, SetListA),
    set_ordlist.to_sorted_list(SetB, SetListB),
    tree_bitset.foldl(Pred, SetA, Acc0) = AccA,
    set_ordlist.fold(Pred, SetB, Acc0) = AccB,
    ( if SetListA = SetListB, AccA = AccB then
        Acc = AccA
    else
        unexpected($pred, "failed")
    ).

foldl(Pred, tb(SetA, SetB), Acc0, Acc) :-
    tree_bitset.to_sorted_list(SetA, SetListA),
    set_ordlist.to_sorted_list(SetB, SetListB),
    tree_bitset.foldl(Pred, SetA, Acc0, AccA),
    set_ordlist.fold(Pred, SetB, Acc0, AccB),
    ( if SetListA = SetListB, AccA = AccB then
        Acc = AccA
    else
        unexpected($pred, "failed")
    ).

%---------------------------------------------------------------------------%
%
% The integrity test operations.
%

:- pred check0(string::in, test_bitset(T)::in, test_bitset(T)::out) is det
    <= uenum(T).

check0(Op, Tester, Result) :-
    Tester = tb(BitSet, Set),
    tree_bitset.to_sorted_list(BitSet, BitSetList),
    set_ordlist.to_sorted_list(Set, SetList),
    ( if BitSetList = SetList then
        Result = Tester
    else
        throw(zero_argument(Op, Tester))
    ).

:- pred check1(string::in, test_bitset(T)::in, test_bitset(T)::in,
    test_bitset(T)::out) is det <= uenum(T).

check1(Op, TesterA, Tester, Result) :-
    TesterA = tb(BitSetA, SetA),
    tree_bitset.to_sorted_list(BitSetA, BitSetListA),
    set_ordlist.to_sorted_list(SetA, SetListA),
    Tester = tb(BitSet, Set),
    tree_bitset.to_sorted_list(BitSet, BitSetList),
    set_ordlist.to_sorted_list(Set, SetList),
    ( if
        BitSetListA = SetListA,
        BitSetList = SetList
    then
        Result = Tester
    else
        throw(one_argument(Op, TesterA, Tester))
    ).

:- pred check2(string::in, test_bitset(T)::in, test_bitset(T)::in,
    test_bitset(T)::in, test_bitset(T)::out) is det <= uenum(T).

check2(Op, TesterA, TesterB, Tester, Result) :-
    TesterA = tb(BitSetA, SetA),
    tree_bitset.to_sorted_list(BitSetA, BitSetListA),
    set_ordlist.to_sorted_list(SetA, SetListA),
    TesterB = tb(BitSetB, SetB),
    tree_bitset.to_sorted_list(BitSetB, BitSetListB),
    set_ordlist.to_sorted_list(SetB, SetListB),
    Tester = tb(BitSet, Set),
    tree_bitset.to_sorted_list(BitSet, BitSetList),
    set_ordlist.to_sorted_list(Set, SetList),

    ( if
        BitSetListA = SetListA,
        BitSetListB = SetListB,
        BitSetList = SetList
    then
        Result = Tester
    else
        throw(two_arguments(Op, TesterA, TesterB, Tester))
    ).

%---------------------------------------------------------------------------%
:- end_module test_bitset.
%---------------------------------------------------------------------------%
