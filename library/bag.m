%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1999, 2003-2007, 2011 The University of Melbourne.
% Copyright (C) 2013-2015, 2017-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: bag.m.
% Main authors: conway, crs.
% Stability: medium.
%
% An implementation of multisets.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module bag.
:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- type bag(T).

%---------------------------------------------------------------------------%

    % Create an empty bag.
    %
:- func init = bag(T).
:- pred init(bag(T)::out) is det.

    % Create a bag containing the given item.
    %
:- func singleton(T) = bag(T).

    % Check whether a bag is empty.
    %
:- pred is_empty(bag(T)::in) is semidet.

%---------------------%

    % contains(Bag, X):
    %
    % Check whether Bag contains X.
    %
:- pred contains(bag(T)::in, T::in) is semidet.

    % count_value(Bag, X):
    %
    % Return how many occurrences of X Bag contains.
    % Return 0 if X is not in Bag.
    %
:- func count_value(bag(T), T) = int.
:- pred count_value(bag(T)::in, T::in, int::out) is det.

    % member(X, Bag):
    %
    % True iff Bag contains at least one occurrence of X.
    %
:- pred member(T::in, bag(T)::in) is semidet.

    % member(X, Bag, BagMinusX):
    %
    % Nondeterministically returns all values X from Bag, and the corresponding
    % bag after X has been removed. Duplicate values are returned as
    % many times as they occur in the Bag.
    % NOTE_TO_IMPLEMENTORS This means that if X occurs in Bag XN times,
    % NOTE_TO_IMPLEMENTORS we will return XN *identical* solutions for X,
    % NOTE_TO_IMPLEMENTORS and this will be so for *every* element of Bag.
    % NOTE_TO_IMPLEMENTORS This seems insane to me (zs).
    %
:- pred member(T::out, bag(T)::in, bag(T)::out) is nondet.

%---------------------%

    % Insert a particular value into a bag.
    %
:- func insert(bag(T), T) = bag(T).
:- pred insert(T::in, bag(T)::in, bag(T)::out) is det.

    % Insert a list of values into a bag.
    %
:- func insert_list(bag(T), list(T)) = bag(T).
:- pred insert_list(list(T)::in, bag(T)::in, bag(T)::out) is det.

    % Insert N copies of a particular value into a bag.
    % Fails if N < 0.
    %
:- pred insert_duplicates(int::in, T::in, bag(T)::in, bag(T)::out)
    is semidet.

    % As above, but throws an exception if N < 0.
    %
:- func det_insert_duplicates(bag(T), int, T) = bag(T).
:- pred det_insert_duplicates(int::in, T::in, bag(T)::in, bag(T)::out) is det.

    % Insert a set of values into a bag.
    %
:- func insert_set(bag(T), set(T)) = bag(T).
:- pred insert_set(set(T)::in, bag(T)::in, bag(T)::out) is det.

%---------------------%

    % Remove one occurrence of the smallest value from a bag.
    % Fails if the bag is empty.
    %
:- pred remove_smallest(T::out, bag(T)::in, bag(T)::out) is semidet.

    % Remove one occurrence of a particular value from a bag.
    % Fail if the item does not exist in the bag.
    %
:- pred remove(T::in, bag(T)::in, bag(T)::out) is semidet.

    % Remove one occurrence of a particular value from a bag.
    % Throw an exception if the item does not exist in the bag.
    %
:- func det_remove(bag(T), T) = bag(T).
:- pred det_remove(T::in, bag(T)::in, bag(T)::out) is det.

    % Remove a list of values from a bag. Duplicates are removed from the bag
    % the appropriate number of times. Fail if any of the items in the list
    % do not exist in the bag.
    %
    % This call is logically equivalent to:
    %
    %   remove_list(Bag0, RemoveList, Bag) :-
    %       from_list(RemoveList, RemoveBag),
    %       is_subbag(RemoveBag, Bag0),
    %       subtract(Bag0, RemoveBag, Bag).
    %
:- pred remove_list(list(T)::in, bag(T)::in, bag(T)::out) is semidet.

    % Remove a list of values from a bag. Duplicates are removed from the bag
    % the appropriate number of times. Throw an exception if any of the items
    % in the list do not exist in the bag.
    %
:- func det_remove_list(bag(T), list(T)) = bag(T).
:- pred det_remove_list(list(T)::in, bag(T)::in, bag(T)::out) is det.

    % Remove a set of values from a bag. Each value is removed once.
    % Fail if any of the items in the set do not exist in the bag.
    %
:- pred remove_set(set(T)::in, bag(T)::in, bag(T)::out) is semidet.

    % Remove a set of values from a bag. Each value is removed once.
    % Throw an exception if any of the items in the set do not exist in the
    % bag.
    %
:- func det_remove_set(bag(T), set(T)) = bag(T).
:- pred det_remove_set(set(T)::in, bag(T)::in, bag(T)::out) is det.

    % Delete one occurrence of a particular value from a bag.
    % If the key is not present, leave the bag unchanged.
    %
:- func delete(bag(T), T) = bag(T).
:- pred delete(T::in, bag(T)::in, bag(T)::out) is det.

    % Remove all occurrences of a particular value from a bag.
    % Fail if the item does not exist in the bag.
    %
:- pred remove_all(T::in, bag(T)::in, bag(T)::out) is semidet.

    % Delete all occurrences of a particular value from a bag.
    %
:- func delete_all(bag(T), T) = bag(T).
:- pred delete_all(T::in, bag(T)::in, bag(T)::out) is det.

%---------------------%

    % Make a bag from a list.
    %
:- func bag(list(T)) = bag(T).
:- func from_list(list(T)) = bag(T).
:- pred from_list(list(T)::in, bag(T)::out) is det.

    % Make a bag from a sorted list.
    %
:- func from_sorted_list(list(T)) = bag(T).
:- pred from_sorted_list(list(T)::in, bag(T)::out) is det.

    % Make a bag from a set.
    %
:- func from_set(set(T)) = bag(T).
:- pred from_set(set(T)::in, bag(T)::out) is det.

    % Given a bag, produce a sorted list containing all the values in the bag.
    % Each value will appear in the list the same number of times that it
    % appears in the bag.
    %
:- func to_list(bag(T)) = list(T).
:- pred to_list(bag(T)::in, list(T)::out) is det.

    % Given a bag, produce a sorted list containing all the values in the bag.
    % Each value will appear in the list once, with the associated integer
    % giving the number of times that it appears in the bag.
    %
:- func to_assoc_list(bag(T)) = assoc_list(T, int).
:- pred to_assoc_list(bag(T)::in, assoc_list(T, int)::out) is det.

    % Given a bag, produce a sorted list with no duplicates containing
    % all the values in the bag.
    %
:- func to_list_without_duplicates(bag(T)) = list(T).
:- pred to_list_without_duplicates(bag(T)::in, list(T)::out) is det.

    % Given a bag, produce a sorted list containing one copy each
    % of all the values that have *more* than one copy in the bag.
    %
:- func to_list_only_duplicates(bag(T)) = list(T).
:- pred to_list_only_duplicates(bag(T)::in, list(T)::out) is det.

    % Given a bag, produce a set containing all the values in the bag.
    % NOTE_TO_IMPLEMENTORS The _without_duplicates suffix is redundant.
    %
:- func to_set(bag(T)) = set(T).

%---------------------%

    % subtract(BagA, BagB, BagAmB):
    %
    % Subtracts BagB from BagA to produce BagAmB. Each element in BagB is
    % removed from BagA to produce BagAmB.
    %
    % An example:
    % subtract({1, 1, 2, 2, 3 }, {1, 1, 2, 3, 3, 3}, {2}).
    %
    % Use one of the subtract_small variants if BagB is expected to be
    % significantly smaller than BagA.
    %
:- func subtract(bag(T), bag(T)) = bag(T).
:- pred subtract(bag(T)::in, bag(T)::in, bag(T)::out) is det.
:- func subtract_small(bag(T), bag(T)) = bag(T).
:- pred subtract_small(bag(T)::in, bag(T)::in, bag(T)::out) is det.

    % least_upper_bound(BagA, BagB, BagAlubB):
    %
    % BagAlubB is the least upper bound of BagA and BagB.
    % It is the smallest bag that contains at least as many copies
    % of each element as BagA, and at least as many copies as BagB.
    % If an element X is present AXN in BagA and BXN times in BagB,
    % X will be present int.max(AXN, BXN) times in BagAlubB.
    %
    % An example:
    % least_upper_bound({1, 1, 2}, {2, 2, 3}, {1, 1, 2, 2, 3}).
    %
    % Use one of the least_upper_bound_small variants if BagB is expected
    % to be significantly smaller than BagA. (If BagA is expected to be
    % significantly smaller than BagB, then switch the operands around.)
    %
:- func least_upper_bound(bag(T), bag(T)) = bag(T).
:- pred least_upper_bound(bag(T)::in, bag(T)::in, bag(T)::out) is det.
:- func least_upper_bound_small(bag(T), bag(T)) = bag(T).
:- pred least_upper_bound_small(bag(T)::in, bag(T)::in, bag(T)::out) is det.

    % union(BagA, BagB, BagAuB):
    %
    % BagAuB is the union of BagA and BagB.
    %
    % An example:
    % e.g. {1, 1, 2, 2} U {2, 2, 3, 3} = {1, 1, 2, 2, 2, 2, 3, 3}.
    %
    % Use one of the union_small variants if BagB is expected to be
    % significantly smaller than BagA. (If BagA is expected to be
    % significantly smaller than BagB, then switch the operands around.)
    %
:- func union(bag(T), bag(T)) = bag(T).
:- pred union(bag(T)::in, bag(T)::in, bag(T)::out) is det.
:- func union_small(bag(T), bag(T)) = bag(T).
:- pred union_small(bag(T)::in, bag(T)::in, bag(T)::out) is det.

    % intersect(BagA, BagB, BagAuB):
    %
    % BagAiB is the intersection of BagA and BagB.
    %
    % An example:
    % intersect({1, 2, 2, 3, 3}, {2, 2, 3, 4}, {2, 2, 3}).
    %
    % Use one of the intersect_small variants if BagB is expected to be
    % significantly smaller than BagA. (If BagA is expected to be
    % significantly smaller than BagB, then switch the operands around.)
    %
:- func intersect(bag(T), bag(T)) = bag(T).
:- pred intersect(bag(T)::in, bag(T)::in, bag(T)::out) is det.
:- func intersect_small(bag(T), bag(T)) = bag(T).
:- pred intersect_small(bag(T)::in, bag(T)::in, bag(T)::out) is det.

    % Fails if there is no intersection between the 2 bags.
    % intersect(A, B) :- intersect(A, B, C), not is_empty(C).
    %
:- pred intersect(bag(T)::in, bag(T)::in) is semidet.

%---------------------%

    % Tests whether the first bag is a subbag of the second.
    % is_subbag(BagA, BagB) implies that every element in the BagA
    % is also in the BagB. If an element is in BagA multiple times,
    % it must be in BagB at least as many times.
    % e.g. is_subbag({1, 1, 2}, {1, 1, 2, 2, 3}).
    % e.g. is_subbag({1, 1, 2}, {1, 2, 3}) :- fail.
    %
:- pred is_subbag(bag(T)::in, bag(T)::in) is semidet.

    % Compares the two bags, and returns whether the first bag is a
    % subset (<), is equal (=), or is a superset (>) of the second.
    % Fails if the two bags are incomparable.
    %
    % Examples:
    % subset_compare(<, {apple, orange}, {apple, apple, orange}).
    % subset_compare(=, {apple, orange}, {apple, orange}).
    % subset_compare(>, {apple, apple, orange}, {apple, orange}).
    % subset_compare(_, {apple, apple}, {orange, orange}) :- fail.
    %
:- pred subset_compare(comparison_result::out, bag(T)::in, bag(T)::in)
    is semidet.

%---------------------%

    % Perform a traversal of the bag, applying an accumulator predicate
    % to each value - count pair.
    %
:- pred foldl(pred(T, int, A, A), bag(T), A, A).
:- mode foldl(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode foldl(in(pred(in, in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldl(in(pred(in, in, di, uo) is det), in, di, uo) is det.
:- mode foldl(in(pred(in, in, in, out) is semidet), in, in, out) is semidet.
:- mode foldl(in(pred(in, in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldl(in(pred(in, in, di, uo) is semidet), in, di, uo) is semidet.

    % As above, but with two accumulators.
    %
:- pred foldl2(pred(T, int, A, A, B, B), bag(T), A, A, B, B).
:- mode foldl2(in(pred(in, in, in, out, in, out) is det), in, in, out,
    in, out) is det.
:- mode foldl2(in(pred(in, in, in, out, mdi, muo) is det), in, in, out,
    mdi, muo) is det.
:- mode foldl2(in(pred(in, in, in, out, di, uo) is det), in, in, out,
    di, uo) is det.
:- mode foldl2(in(pred(in, in, in, out, in, out) is semidet), in, in, out,
    in, out) is semidet.
:- mode foldl2(in(pred(in, in, in, out, mdi, muo) is semidet), in, in, out,
    mdi, muo) is semidet.
:- mode foldl2(in(pred(in, in, in, out, di, uo) is semidet), in, in, out,
    di, uo) is semidet.

%---------------------%

    % Return the number of values in a bag.
    % If an element X is present N times, count it N times.
    %
:- func count(bag(T)) = int.

    % Return the number of unique values in a bag.
    % Even if an element X is present N times, count it just one.
    %
:- func count_unique(bag(T)) = int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

:- import_module term.      % for var/1.

:- pragma type_spec(pred(bag.insert/3), T = var(_)).
:- pragma type_spec(pred(bag.insert_list/3), T = var(_)).
:- pragma type_spec(pred(bag.insert_set/3), T = var(_)).

:- pragma type_spec(pred(bag.remove/3), T = var(_)).
:- pragma type_spec(pred(bag.remove_list/3), T = var(_)).
:- pragma type_spec(pred(bag.det_remove_list/3), T = var(_)).
:- pragma type_spec(pred(bag.det_remove_set/3), T = var(_)).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.

:- type bag(T)
    --->    bag(map(T, int)).

%---------------------------------------------------------------------------%

init = Bag :-
    bag.init(Bag).

init(bag(Map)) :-
    map.init(Map).

singleton(Item) = bag(Map) :-
    Map = map.singleton(Item, 1).

is_empty(bag(Map)) :-
    map.is_empty(Map).

%---------------------------------------------------------------------------%

contains(bag(Map), X) :-
    map.contains(Map, X).

count_value(Bag, X) = N :-
    bag.count_value(Bag, X, N).

count_value(bag(Map), X, N) :-
    ( if map.search(Map, X, NPrime) then
        N = NPrime
    else
        N = 0
    ).

member(X, bag(Map)) :-
    map.search(Map, X, _N).

member(X, !Bag) :-
    Xs = bag.to_list(!.Bag),
    list.member(X, Xs),
    bag.det_remove(X, !Bag).

%---------------------------------------------------------------------------%

insert(!.Bag, X) = !:Bag :-
    bag.insert(X, !Bag).

insert(Item, bag(!.Map), bag(!:Map)) :-
    ( if map.search(!.Map, Item, Count) then
        map.det_update(Item, Count + 1, !Map)
    else
        map.det_insert(Item, 1, !Map)
    ).

insert_list(!.Bag, Xs) = !:Bag :-
    bag.insert_list(Xs, !Bag).

insert_list([], !Bag).
insert_list([Item | Items], !Bag) :-
    bag.insert(Item, !Bag),
    bag.insert_list(Items, !Bag).

insert_duplicates(N, Item, bag(!.Map), bag(!:Map)) :-
    compare(CmpResult, N, 0),
    (
        CmpResult = (>),
        ( if map.search(!.Map, Item, Count) then
            map.det_update(Item, Count + N, !Map)
        else
            map.det_insert(Item, N, !Map)
        )
    ;
        CmpResult = (=)
    ;
        CmpResult = (<),
        fail
    ).

det_insert_duplicates(!.Bag, N, Item) = !:Bag :-
    det_insert_duplicates(N, Item, !Bag).

det_insert_duplicates(N, Item, !Bag) :-
    ( if insert_duplicates(N, Item, !Bag) then
        true
    else
        error($pred, "number of items is negative")
    ).

insert_set(!.Bag, Xs) = !:Bag :-
    bag.insert_set(Xs, !Bag).

insert_set(Set, !Bag) :-
    set.to_sorted_list(Set, List),
    % XXX We should exploit the sortedness of List.
    bag.insert_list(List, !Bag).

%---------------------------------------------------------------------------%

remove_smallest(X, bag(!.Map), bag(!:Map)) :-
    map.remove_smallest(X, N, !Map),
    ( if N > 1 then
        map.det_insert(X, N - 1, !Map)
    else
        true
    ).

remove(X, bag(!.Map), bag(!:Map)) :-
    map.search(!.Map, X, N),
    ( if N > 1 then
        map.det_update(X, N - 1, !Map)
    else
        map.delete(X, !Map)
    ).

det_remove(!.Bag, X) = !:Bag :-
    bag.det_remove(X, !Bag).

det_remove(X, !Bag) :-
    ( if bag.remove(X, !Bag) then
        true
    else
        unexpected($pred, "item not in bag")
    ).

remove_list([], !Bag).
remove_list([X | Xs], !Bag) :-
    bag.remove(X, !Bag),
    bag.remove_list(Xs, !Bag).

det_remove_list(!.Bag, Xs) = !:Bag :-
    bag.det_remove_list(Xs, !Bag).

det_remove_list(Xs, !Bag) :-
    ( if bag.remove_list(Xs, !Bag) then
        true
    else
        unexpected($pred, "some item not in bag")
    ).

remove_set(Set, !Bag) :-
    set.to_sorted_list(Set, Xs),
    % XXX We should exploit the sortedness of Xs.
    bag.remove_list(Xs, !Bag).

det_remove_set(!.Bag, Set) = !:Bag :-
    bag.det_remove_set(Set, !Bag).

det_remove_set(Set, !Bag) :-
    set.to_sorted_list(Set, Xs),
    % XXX We should exploit the sortedness of List.
    bag.det_remove_list(Xs, !Bag).

delete(!.Bag, X) = !:Bag :-
    bag.delete(X, !Bag).

delete(X, bag(!.Map), bag(!:Map)) :-
    ( if map.search(!.Map, X, N) then
        ( if N > 1 then
            map.det_update(X, N - 1, !Map)
        else
            map.delete(X, !Map)
        )
    else
        true
    ).

remove_all(X, bag(!.Map), bag(!:Map)) :-
    % This is semidet.
    map.remove(X, _N, !Map).

delete_all(!.Bag, X) = !:Bag :-
    bag.delete_all(X, !Bag).

delete_all(X, bag(!.Map), bag(!:Map)) :-
    % This is det.
    map.delete(X, !Map).

%---------------------------------------------------------------------------%

bag(Xs) = bag.from_list(Xs).

from_list(Xs) = Bag :-
    bag.from_list(Xs, Bag).

from_list(Xs, Bag) :-
    bag.init(Bag0),
    bag.insert_list(Xs, Bag0, Bag).

from_sorted_list(Xs) = Bag :-
    bag.from_sorted_list(Xs, Bag).

from_sorted_list(Xs, Bag) :-
    bag.init(Bag0),
    % XXX We should exploit the sortedness of Xs.
    bag.insert_list(Xs, Bag0, Bag).

from_set(Set) = Bag :-
    bag.from_set(Set, Bag).

from_set(Set, Bag) :-
    set.to_sorted_list(Set, Xs),
    bag.init(Bag0),
    % XXX We should exploit the sortedness of List.
    bag.insert_list(Xs, Bag0, Bag).

%---------------------------------------------------------------------------%

to_list(Bag) = Xs :-
    bag.to_list(Bag, Xs).

to_list(bag(Map), Xs) :-
    map.foldl(prepend_n_xs, Map, [], RevXs),
    list.reverse(RevXs, Xs).

:- pred prepend_n_xs(T::in, int::in, list(T)::in, list(T)::out) is det.

prepend_n_xs(X, N, !RevXs) :-
    ( if N =< 0 then
        true
    else
        !:RevXs = [X | !.RevXs],
        prepend_n_xs(X, N - 1, !RevXs)
    ).

to_assoc_list(Bag) = XNs :-
    bag.to_assoc_list(Bag, XNs).

to_assoc_list(bag(Map), XNs) :-
    map.to_assoc_list(Map, XNs).

to_list_without_duplicates(Bag) = Xs :-
    bag.to_list_without_duplicates(Bag, Xs).

to_list_without_duplicates(bag(Map), Xs) :-
    map.keys(Map, Xs).

to_list_only_duplicates(Bag) = Xs :-
    bag.to_list_only_duplicates(Bag, Xs).

to_list_only_duplicates(bag(Map), DupXs) :-
    map.to_assoc_list(Map, XNs),
    list.filter_map(is_duplicated, XNs, DupXs).

:- pred is_duplicated(pair(T, int)::in, T::out) is semidet.

is_duplicated(X - XN, X) :-
    XN > 1.

to_set(bag(Map)) = Set :-
    map.keys(Map, Xs),
    set.sorted_list_to_set(Xs, Set).

%---------------------------------------------------------------------------%

subtract(BagA, BagB) = BagAmB :-
    bag.subtract(BagA, BagB, BagAmB).

subtract(bag(MapA), bag(MapB), bag(MapAmB)) :-
    map.to_assoc_list(MapA, AXNs),
    map.to_assoc_list(MapB, BXNs),
    bag.subtract_loop(AXNs, BXNs, [], RevAmBXNs),
    map.from_rev_sorted_assoc_list(RevAmBXNs, MapAmB).

    % The specialized mode is for a recursive call.
    %
:- pred subtract_loop(assoc_list(T, int), assoc_list(T, int),
    assoc_list(T, int), assoc_list(T, int)).
:- mode subtract_loop(in, in(empty_list), in, out) is det.
:- mode subtract_loop(in, in, in, out) is det.

subtract_loop(AXNs, BXNs, !RevAmBXNs) :-
    (
        AXNs = []
        % There is nothing left to subtract.
    ;
        AXNs = [HeadAXN | TailAXNs],
        BXNs = [],
        !:RevAmBXNs = [HeadAXN | !.RevAmBXNs],
        bag.subtract_loop(TailAXNs, BXNs, !RevAmBXNs)
    ;
        AXNs = [HeadAXN | TailAXNs],
        BXNs = [HeadBXN | TailBXNs],
        HeadAXN = AX - AXN,
        HeadBXN = BX - BXN,
        compare(Res, AX, BX),
        (
            Res = (<),
            % There is nothing in BagB to subtract from AXN.
            !:RevAmBXNs = [HeadAXN | !.RevAmBXNs],
            bag.subtract_loop(TailAXNs, BXNs, !RevAmBXNs)
        ;
            Res = (=),
            XN = AXN - BXN,
            ( if XN > 0 then
                HeadXN = AX - XN,
                !:RevAmBXNs = [HeadXN | !.RevAmBXNs]
            else
                true
            ),
            bag.subtract_loop(TailAXNs, TailBXNs, !RevAmBXNs)
        ;
            Res = (>),
            % There is nothing in BagA to subtract BXN from.
            bag.subtract_loop(AXNs, TailBXNs, !RevAmBXNs)
        )
    ).

subtract_small(BagA, BagB) = BagAmB :-
    bag.subtract_small(BagA, BagB, BagAmB).

subtract_small(bag(MapA), bag(MapB), bag(MapAmB)) :-
    bag.subtract_small_loop(MapA, MapB, MapAmB).

:- pred subtract_small_loop(map(T, int)::in, map(T, int)::in, map(T, int)::out)
    is det.

subtract_small_loop(MapA, MapB, MapAmB) :-
    ( if map.remove_smallest(X, BXN, MapB, NextMapB) then
        ( if map.search(MapA, X, AXN) then
            XN = AXN - BXN,
            ( if XN > 0 then
                map.det_update(X, XN, MapA, NextMapA)
            else
                map.delete(X, MapA, NextMapA)
            )
        else
            NextMapA = MapA
        ),
        bag.subtract_small_loop(NextMapA, NextMapB, MapAmB)
    else
        MapAmB = MapA
    ).

%---------------------%

least_upper_bound(BagA, BagB) = BagAlubB :-
    bag.least_upper_bound(BagA, BagB, BagAlubB).

least_upper_bound(bag(MapA), bag(MapB), bag(MapAlubB)) :-
    map.to_assoc_list(MapA, AXNs),
    map.to_assoc_list(MapB, BXNs),
    bag.least_upper_bound_loop(AXNs, BXNs, [], RevAlubBXNs),
    map.from_rev_sorted_assoc_list(RevAlubBXNs, MapAlubB).

    % The specialized modes are for the recursive calls.
    %
:- pred least_upper_bound_loop(assoc_list(T, int), assoc_list(T, int),
    assoc_list(T, int), assoc_list(T, int)).
:- mode least_upper_bound_loop(in(empty_list), in, in, out) is det.
:- mode least_upper_bound_loop(in, in(empty_list), in, out) is det.
:- mode least_upper_bound_loop(in, in, in, out) is det.

least_upper_bound_loop(AXNs, BXNs, !RevAlubBXNs) :-
    (
        AXNs = [],
        BXNs = []
    ;
        AXNs = [],
        BXNs = [HeadBXN | TailBXNs],
        !:RevAlubBXNs = [HeadBXN | !.RevAlubBXNs],
        bag.least_upper_bound_loop(AXNs, TailBXNs, !RevAlubBXNs)
    ;
        AXNs = [HeadAXN | TailAXNs],
        BXNs = [],
        !:RevAlubBXNs = [HeadAXN | !.RevAlubBXNs],
        bag.least_upper_bound_loop(TailAXNs, BXNs, !RevAlubBXNs)
    ;
        AXNs = [HeadAXN | TailAXNs],
        BXNs = [HeadBXN | TailBXNs],
        HeadAXN = AX - AXN,
        HeadBXN = BX - BXN,
        compare(Res, AX, BX),
        (
            Res = (<),
            % There is nothing in BagB to match AX.
            !:RevAlubBXNs = [HeadAXN | !.RevAlubBXNs],
            bag.least_upper_bound_loop(TailAXNs, BXNs, !RevAlubBXNs)
        ;
            Res = (=),
            XN = int.max(AXN, BXN),
            HeadXN = AX - XN,
            !:RevAlubBXNs = [HeadXN | !.RevAlubBXNs],
            bag.least_upper_bound_loop(TailAXNs, TailBXNs, !RevAlubBXNs)
        ;
            Res = (>),
            % There is nothing in BagA to match BX.
            !:RevAlubBXNs = [HeadBXN | !.RevAlubBXNs],
            bag.least_upper_bound_loop(AXNs, TailBXNs, !RevAlubBXNs)
        )
    ).

least_upper_bound_small(BagA, BagB) = BagAlubB :-
    bag.least_upper_bound_small(BagA, BagB, BagAlubB).

least_upper_bound_small(bag(MapA), bag(MapB), bag(MapAlubB)) :-
    bag.least_upper_bound_small_loop(MapA, MapB, MapAlubB).

:- pred bag.least_upper_bound_small_loop(map(T, int)::in, map(T, int)::in,
    map(T, int)::out) is det.

least_upper_bound_small_loop(MapA, MapB, MapAlubB) :-
    ( if map.remove_smallest(X, BXN, MapB, NextMapB) then
        ( if map.search(MapA, X, AXN) then
            int.max(AXN, BXN, XN),
            map.det_update(X, XN, MapA, NextMapA)
        else
            map.det_insert(X, BXN, MapA, NextMapA)
        ),
        bag.least_upper_bound_small_loop(NextMapA, NextMapB, MapAlubB)
    else
        MapAlubB = MapA
    ).

%---------------------%

union(BagA, BagB) = BagAuB :-
    bag.union(BagA, BagB, BagAuB).

union(bag(MapA), bag(MapB), bag(MapAuB)) :-
    map.to_assoc_list(MapA, AXNs),
    map.to_assoc_list(MapB, BXNs),
    bag.union_loop(AXNs, BXNs, [], RevAuBXNs),
    map.from_rev_sorted_assoc_list(RevAuBXNs, MapAuB).

    % The specialized modes are for the recursive calls.
    %
:- pred union_loop(assoc_list(T, int), assoc_list(T, int),
    assoc_list(T, int), assoc_list(T, int)).
:- mode union_loop(in(empty_list), in, in, out) is det.
:- mode union_loop(in, in(empty_list), in, out) is det.
:- mode union_loop(in, in, in, out) is det.

union_loop(AXNs, BXNs, !RevAuBXNs) :-
    (
        AXNs = [],
        BXNs = []
    ;
        AXNs = [],
        BXNs = [HeadBXN | TailBXNs],
        !:RevAuBXNs = [HeadBXN | !.RevAuBXNs],
        bag.union_loop(AXNs, TailBXNs, !RevAuBXNs)
    ;
        AXNs = [HeadAXN | TailAXNs],
        BXNs = [],
        !:RevAuBXNs = [HeadAXN | !.RevAuBXNs],
        bag.union_loop(TailAXNs, BXNs, !RevAuBXNs)
    ;
        AXNs = [HeadAXN | TailAXNs],
        BXNs = [HeadBXN | TailBXNs],
        HeadAXN = AX - AXN,
        HeadBXN = BX - BXN,
        compare(Res, AX, BX),
        (
            Res = (<),
            % There is nothing in BagB to match AX.
            !:RevAuBXNs = [HeadAXN | !.RevAuBXNs],
            bag.union_loop(TailAXNs, BXNs, !RevAuBXNs)
        ;
            Res = (=),
            XN = AXN + BXN,
            HeadXN = AX - XN,
            !:RevAuBXNs = [HeadXN | !.RevAuBXNs],
            bag.union_loop(TailAXNs, TailBXNs, !RevAuBXNs)
        ;
            Res = (>),
            % There is nothing in BagA to match BX.
            !:RevAuBXNs = [HeadBXN | !.RevAuBXNs],
            bag.union_loop(AXNs, TailBXNs, !RevAuBXNs)
        )
    ).

union_small(BagA, BagB) = BagAuB :-
    bag.union_small(BagA, BagB, BagAuB).

union_small(bag(MapA), bag(MapB), bag(MapAuB)) :-
    bag.union_small_loop(MapA, MapB, MapAuB).

:- pred union_small_loop(map(T, int)::in, map(T, int)::in, map(T, int)::out)
    is det.

union_small_loop(MapA, MapB, MapAuB) :-
    ( if map.remove_smallest(X, BXN, MapB, NextMapB) then
        ( if map.search(MapA, X, AXN) then
            XN = AXN + BXN,
            map.det_update(X, XN, MapA, NextMapA)
        else
            map.det_insert(X, BXN, MapA, NextMapA)
        ),
        bag.union_small_loop(NextMapA, NextMapB, MapAuB)
    else
        MapAuB = MapA
    ).

%---------------------%

intersect(BagA, BagB) = BagAiB :-
    bag.intersect(BagA, BagB, BagAiB).

intersect(bag(MapA), bag(MapB), bag(MapAiB)) :-
    map.to_assoc_list(MapA, AXNs),
    map.to_assoc_list(MapB, BXNs),
    bag.intersect_loop(AXNs, BXNs, [], RevAiBXNs),
    map.from_rev_sorted_assoc_list(RevAiBXNs, MapAiB).

    % The specialized modes are for the recursive calls.
    %
:- pred intersect_loop(assoc_list(T, int), assoc_list(T, int),
    assoc_list(T, int), assoc_list(T, int)).
:- mode intersect_loop(in(empty_list), in, in, out) is det.
:- mode intersect_loop(in, in(empty_list), in, out) is det.
:- mode intersect_loop(in, in, in, out) is det.

intersect_loop(AXNs, BXNs, !RevAuBXNs) :-
    (
        AXNs = [],
        BXNs = []
    ;
        AXNs = [],
        BXNs = [_HeadBXN | TailBXNs],
        % ZZZ modes
        bag.intersect_loop(AXNs, TailBXNs, !RevAuBXNs)
    ;
        AXNs = [_HeadAXN | TailAXNs],
        BXNs = [],
        bag.intersect_loop(TailAXNs, BXNs, !RevAuBXNs)
    ;
        AXNs = [HeadAXN | TailAXNs],
        BXNs = [HeadBXN | TailBXNs],
        HeadAXN = AX - AXN,
        HeadBXN = BX - BXN,
        compare(Res, AX, BX),
        (
            Res = (<),
            % There is nothing in BagB to match AX.
            bag.intersect_loop(TailAXNs, BXNs, !RevAuBXNs)
        ;
            Res = (=),
            XN = int.min(AXN, BXN),
            HeadXN = AX - XN,
            !:RevAuBXNs = [HeadXN | !.RevAuBXNs],
            bag.intersect_loop(TailAXNs, TailBXNs, !RevAuBXNs)
        ;
            Res = (>),
            % There is nothing in BagA to match BX.
            bag.intersect_loop(AXNs, TailBXNs, !RevAuBXNs)
        )
    ).

intersect_small(BagA, BagB) = BagAiB :-
    bag.intersect_small(BagA, BagB, BagAiB).

intersect_small(bag(MapA), bag(MapB), bag(MapAiB)) :-
    map.init(MapAiB0),
    bag.intersect_small_loop(MapA, MapB, MapAiB0, MapAiB).

:- pred bag.intersect_small_loop(map(T, int)::in, map(T, int)::in,
    map(T, int)::in, map(T, int)::out) is det.

intersect_small_loop(MapA, MapB, !MapAiB) :-
    ( if map.remove_smallest(X, AXN, MapA, NextMapA) then
        ( if map.search(MapB, X, BXN) then
            int.min(AXN, BXN, XN),
            map.det_insert(X, XN, !MapAiB)
        else
            true
        ),
        bag.intersect_small_loop(NextMapA, MapB, !MapAiB)
    else
        true
    ).

intersect(bag(MapA), bag(MapB)) :-
    map.remove_smallest(X, _N, MapA, NextMapA),
    ( if map.contains(MapB, X) then
        true
    else
        bag.intersect(bag(NextMapA), bag(MapB))
    ).

%---------------------------------------------------------------------------%

is_subbag(BagA, BagB) :-
    bag.subset_compare(Res, BagA, BagB),
    ( Res = (<)
    ; Res = (=)
    ).

subset_compare(Res, bag(MapA), bag(MapB)) :-
    map.to_assoc_list(MapA, AXNs),
    map.to_assoc_list(MapB, BXNs),
    bag.subset_compare_loop(Res, AXNs, BXNs).

:- pred subset_compare_loop(comparison_result::out,
    assoc_list(T, int)::in, assoc_list(T, int)::in) is semidet.

subset_compare_loop(Res, AXNs, BXNs) :-
    % Go down both AXNs and BXNs until we find a difference.
    % If and when we find one, switch over to subset_compare_verify_le
    % to verify that the rest of two lists has differences only in
    % the same direction.
    (
        AXNs = [],
        BXNs = [],
        Res = (=)
    ;
        AXNs = [],
        BXNs = [_ | _],
        Res = (<)
    ;
        AXNs = [_ | _],
        BXNs = [],
        Res = (>)
    ;
        AXNs = [HeadAXN | TailAXNs],
        BXNs = [HeadBXN | TailBXNs],
        HeadAXN = AX - AXN,
        HeadBXN = BX - BXN,
        compare(XRes, AX, BX),
        (
            XRes = (<),
            % AXNs contains AX while BXNs does not.
            ( if bag.subset_compare_verify_le(BXNs, TailAXNs) then
                Res = (>)
            else
                fail
            )
        ;
            XRes = (=),
            compare(XNRes, AXN, BXN),
            (
                XNRes = (<),
                % BXNs contains more of AX=BX than AXNs.
                ( if bag.subset_compare_verify_le(TailAXNs, TailBXNs) then
                    Res = (<)
                else
                    fail
                )
            ;
                XNRes = (=),
                bag.subset_compare_loop(Res, TailAXNs, TailBXNs)
            ;
                XNRes = (>),
                % AXNs contains more of AX=BX than BXNs.
                ( if bag.subset_compare_verify_le(TailBXNs, TailAXNs) then
                    Res = (>)
                else
                    fail
                )
            )
        ;
            XRes = (>),
            % BXNs contains BX while AXNs does not.
            ( if bag.subset_compare_verify_le(AXNs, TailBXNs) then
                Res = (<)
            else
                fail
            )
        )
    ).

    % subset_compare_verify_le(AXNs, BXNs):
    %
    % Succeed iff AXNs represents a bag that is "less than or equal"
    % than the bag represented by BXNs.
    %
:- pred subset_compare_verify_le(
    assoc_list(T, int)::in, assoc_list(T, int)::in) is semidet.

subset_compare_verify_le(AXNs, BXNs) :-
    (
        AXNs = []
    ;
        AXNs = [HeadAXN | TailAXNs],
        (
            BXNs = [],
            fail
        ;
            BXNs = [HeadBXN | TailBXNs],
            HeadAXN = AX - AXN,
            HeadBXN = BX - BXN,
            compare(XRes, AX, BX),
            (
                XRes = (<),
                % AXNs contains AX while BXNs does not.
                fail
            ;
                XRes = (=),
                compare(XNRes, AXN, BXN),
                (
                    ( XNRes = (<)
                    ; XNRes = (=)
                    ),
                    bag.subset_compare_verify_le(TailAXNs, TailBXNs)
                ;
                    XNRes = (>),
                    % AXNs contains more of AX=BX than BXNs.
                    fail
                )
            ;
                XRes = (>),
                % BXNs contains BX while AXNs does not.
                bag.subset_compare_verify_le(AXNs, TailBXNs)
            )
        )
    ).

%---------------------------------------------------------------------------%

foldl(Pred, bag(Map), !Acc) :-
    map.foldl(Pred, Map, !Acc).

foldl2(Pred, bag(Map), !Acc1, !Acc2) :-
    map.foldl2(Pred, Map, !Acc1, !Acc2).

%---------------------------------------------------------------------------%

count(bag(Map)) = list.foldl(int.plus, map.values(Map), 0).

count_unique(bag(Map)) = map.count(Map).

%---------------------------------------------------------------------------%
:- end_module bag.
%---------------------------------------------------------------------------%
