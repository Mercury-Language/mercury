%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1999, 2003-2007, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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

    % Create an empty bag.
    %
:- func init = bag(T).
:- pred init(bag(T)::out) is det.

    % Return the number of values in a bag (including duplicate values).
    %
:- func count(bag(T)) = int.

    % Return the number of unique values in a bag, duplicate values are counted
    % only once.
    %
:- func count_unique(bag(T)) = int.

    % Insert a particular value in a bag.
    %
:- func insert(bag(T), T) = bag(T).
:- pred insert(T::in, bag(T)::in, bag(T)::out) is det.

    % Insert a list of values into a bag.
    %
:- func insert_list(bag(T), list(T)) = bag(T).
:- pred insert_list(list(T)::in, bag(T)::in, bag(T)::out) is det.

    % Insert a list of values into a bag.
    %
:- func insert_set(bag(T), set(T)) = bag(T).
:- pred insert_set(set(T)::in, bag(T)::in, bag(T)::out) is det.

    % member(Val, Bag):
    % True iff `Bag' contains at least one occurrence of `Val'.
    %
:- pred member(T::in, bag(T)::in) is semidet.

    % member(Val, Bag, Remainder):
    % Nondeterministically returns all values from Bag and the corresponding
    % bag after the value has been removed. Duplicate values are returned as
    % many times as they occur in the Bag.
    %
:- pred member(T::out, bag(T)::in, bag(T)::out) is nondet.

    % Make a bag from a list.
    %
:- func bag(list(T)) = bag(T).
:- func from_list(list(T)) = bag(T).
:- pred from_list(list(T)::in, bag(T)::out) is det.

    % Make a bag from a set.
    %
:- func from_set(set(T)) = bag(T).
:- pred from_set(set(T)::in, bag(T)::out) is det.

    % Make a bag from a sorted list.
    %
:- func from_sorted_list(list(T)) = bag(T).
:- pred from_sorted_list(list(T)::in, bag(T)::out) is det.

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

    % Given a bag, produce a set containing all the values in the bag.
    %
:- func to_set(bag(T)) = set(T).
:- func to_set_without_duplicates(bag(T)) = set(T).
:- pred to_set_without_duplicates(bag(T)::in, set(T)::out) is det.

    % Remove one occurrence of a particular value from a bag.
    % Fail if the item does not exist in the bag.
    %
:- pred remove(T::in, bag(T)::in, bag(T)::out) is semidet.

    % Remove one occurrence of a particular value from a bag.
    % Abort if the item does not exist in the bag.
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
    % the appropriate number of times.  Abort if any of the items in the list
    % do not exist in the bag.
    %
:- func det_remove_list(bag(T), list(T)) = bag(T).
:- pred det_remove_list(list(T)::in, bag(T)::in, bag(T)::out) is det.

    % Remove a set of values from a bag. Each value is removed once.
    % Fail if any of the items in the set do not exist in the bag.
    %
:- pred remove_set(set(T)::in, bag(T)::in, bag(T)::out) is semidet.

    % Remove a set of values from a bag. Each value is removed once.
    % Abort if any of the items in the set do not exist in the bag.
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

    % Check whether a bag contains a particular value.
    %
:- pred contains(bag(T)::in, T::in) is semidet.

    % Count how many occurrences of the value the bag contains.
    %
:- func count_value(bag(T), T) = int.
:- pred count_value(bag(T)::in, T::in, int::out) is det.

    % subtract(Bag0, SubBag, Bag):
    %
    % Subtracts SubBag from Bag0 to produce Bag. Each element in SubBag is
    % removed from Bag0 to produce Bag. If an element exists in SubBag,
    % but not in Bag, then that element is not removed. An example:
    % subtract({1, 1, 2, 2, 3 }, {1, 1, 2, 3, 3, 3}, {2}).
    %
:- func subtract(bag(T), bag(T)) = bag(T).
:- pred subtract(bag(T)::in, bag(T)::in, bag(T)::out) is det.

    % The third bag is the union of the first 2 bags,
    % e.g. {1, 1, 2, 2} U {2, 2, 3, 3} = {1, 1, 2, 2, 2, 2, 3, 3}.
    % If the two input bags are known to be unequal in size, then making
    % the first bag the larger bag will usually be more efficient.
    %
:- func union(bag(T), bag(T)) = bag(T).
:- pred union(bag(T)::in, bag(T)::in, bag(T)::out) is det.

    % The third bag is the intersection of the first 2 bags. Every element
    % in the third bag exists in both of the first 2 bags, e.g.
    % intersect({1, 2, 2, 3, 3}, {2, 2, 3, 4}, {2, 2, 3}).
    %
:- func intersect(bag(T), bag(T)) = bag(T).
:- pred intersect(bag(T)::in, bag(T)::in, bag(T)::out) is det.

    % Fails if there is no intersection between the 2 bags.
    % intersect(A, B) :- intersect(A, B, C), not is_empty(C).
    %
:- pred intersect(bag(T)::in, bag(T)::in) is semidet.

    % The third bag is the smallest bag that has both the first two bags
    % as subbags. If an element X is present N times in one of the first
    % two bags, X will be present at least N times in the third bag.
    % E.g. {1, 1, 2} upper_bound {2, 2, 3} = {1, 1, 2, 2, 3}.
    % If the two input bags are known to be unequal in size, then making
    % the first bag the larger bag will usually be more efficient.
    %
:- func least_upper_bound(bag(T), bag(T)) = bag(T).
:- pred least_upper_bound(bag(T)::in, bag(T)::in, bag(T)::out) is det.

    % Tests whether the first bag is a subbag of the second.
    % is_subbag(A, B) implies that every element in the bag A
    % is also in the bag B. If an element is in bag A multiple times,
    % it must be in bag B at least as many times.
    % e.g. is_subbag({1, 1, 2}, {1, 1, 2, 2, 3}).
    % e.g. is_subbag({1, 1, 2}, {1, 2, 3}) :- fail.
    %
:- pred is_subbag(bag(T)::in, bag(T)::in) is semidet.

    % Check whether a bag is empty.
    %
:- pred is_empty(bag(T)::in) is semidet.

    % Fails if the bag is empty.
    %
:- pred remove_smallest(T::out, bag(T)::in, bag(T)::out) is semidet.

    % Compares the two bags, and returns whether the first bag is a
    % subset (<), is equal (=), or is a superset (>) of the second.
    % subset_compare(<, {apple, orange}, {apple, apple, orange}).
    % subset_compare(=, {apple, orange}, {apple, orange}).
    % subset_compare(>, {apple, apple, orange}, {apple, orange}).
    % subset_compare(_, {apple, apple}, {orange, orange}) :- fail.
    %
:- pred subset_compare(comparison_result::out, bag(T)::in, bag(T)::in)
    is semidet.

    % Perform a traversal of the bag, applying an accumulator predicate
    % to each value - count pair.
    %
:- pred foldl(pred(T, int, A, A), bag(T), A, A).
:- mode foldl(pred(in, in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl(pred(in, in, di, uo) is semidet, in, di, uo) is semidet.

    % As above, but with two accumulators.
    %
:- pred foldl2(pred(T, int, A, A, B, B), bag(T), A, A, B, B).
:- mode foldl2(pred(in, in, in, out, in, out) is det, in, in, out,
    in, out) is det.
:- mode foldl2(pred(in, in, in, out, mdi, muo) is det, in, in, out,
    mdi, muo) is det.
:- mode foldl2(pred(in, in, in, out, di, uo) is det, in, in, out,
    di, uo) is det.
:- mode foldl2(pred(in, in, in, out, in, out) is semidet, in, in, out,
    in, out) is semidet.
:- mode foldl2(pred(in, in, in, out, mdi, muo) is semidet, in, in, out,
    mdi, muo) is semidet.
:- mode foldl2(pred(in, in, in, out, di, uo) is semidet, in, in, out,
    di, uo) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

:- import_module term.      % for var/1.

:- pragma type_spec(bag.insert/3, T = var(_)).
:- pragma type_spec(bag.insert_list/3, T = var(_)).
:- pragma type_spec(bag.insert_set/3, T = var(_)).

:- pragma type_spec(bag.remove/3, T = var(_)).
:- pragma type_spec(bag.remove_list/3, T = var(_)).
:- pragma type_spec(bag.det_remove_list/3, T = var(_)).
:- pragma type_spec(bag.det_remove_set/3, T = var(_)).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.

:- type bag(T)
    --->    bag(map(T, int)).

%---------------------------------------------------------------------------%

bag.init = B :-
    bag.init(B).

bag.init(bag(Bag)) :-
    map.init(Bag).

%---------------------------------------------------------------------------%

bag.count(bag(Bag)) = list.foldl(int.plus, map.values(Bag), 0).

%---------------------------------------------------------------------------%

bag.count_unique(bag(Bag)) = map.count(Bag).

%---------------------------------------------------------------------------%

bag.insert(!.B, X) = !:B :-
    bag.insert(X, !B).

bag.insert(Item, bag(!.Bag), bag(!:Bag)) :-
    ( map.search(!.Bag, Item, Count0) ->
        Count = Count0 + 1
    ;
        Count = 1
    ),
    map.set(Item, Count, !Bag).

%---------------------------------------------------------------------------%

bag.insert_list(B1, Xs) = B2 :-
    bag.insert_list(Xs, B1, B2).

bag.insert_list([], !Bag).
bag.insert_list([Item | Items], !Bag) :-
    bag.insert(Item, !Bag),
    bag.insert_list(Items, !Bag).

bag.insert_set(Set, !Bag) :-
    set.to_sorted_list(Set, List),
    % XXX We should exploit the sortedness of List.
    bag.insert_list(List, !Bag).

bag.member(M, bag(Bag)) :-
    map.search(Bag, M, _Occurrences).

bag.member(OutVal, InBag, OutBag) :-
    Vals = bag.to_list(InBag),
    list.member(OutVal, Vals),
    OutBag = bag.det_remove(InBag, OutVal).

bag.bag(Xs) = bag.from_list(Xs).

bag.from_list(Xs) = B :-
    bag.from_list(Xs, B).

bag.from_list(List, Bag) :-
    bag.init(Bag0),
    bag.insert_list(List, Bag0, Bag).

bag.insert_set(!.B, Xs) = !:B :-
    bag.insert_set(Xs, !B).

bag.from_set(Xs) = B :-
    bag.from_set(Xs, B).

bag.from_set(Set, Bag) :-
    set.to_sorted_list(Set, List),
    bag.init(Bag0),
    % XXX We should exploit the sortedness of List.
    bag.insert_list(List, Bag0, Bag).

bag.from_sorted_list(Xs) = B :-
    bag.from_sorted_list(Xs, B).

bag.from_sorted_list(List, Bag) :-
    bag.init(Bag0),
    % XXX We should exploit the sortedness of List.
    bag.insert_list(List, Bag0, Bag).

bag.to_list(B) = Xs :-
    bag.to_list(B, Xs).

bag.to_list(bag(Bag), List) :-
    map.to_assoc_list(Bag, AssocList),
    bag.to_list_2(AssocList, List).

:- pred bag.to_list_2(assoc_list(T, int)::in, list(T)::out) is det.

bag.to_list_2([], []).
bag.to_list_2([X - Int | Xs ], Out) :-
    ( Int =< 0 ->
        bag.to_list_2(Xs, Out)
    ;
        NewInt = Int - 1,
        bag.to_list_2([X - NewInt | Xs], Out0),
        Out = [X | Out0]
    ).

bag.to_assoc_list(B) = AL :-
    bag.to_assoc_list(B, AL).

bag.to_assoc_list(bag(Bag), AssocList) :-
    map.to_assoc_list(Bag, AssocList).

bag.to_list_without_duplicates(B) = Xs :-
    bag.to_list_without_duplicates(B, Xs).

bag.to_list_without_duplicates(bag(Bag), List) :-
    map.keys(Bag, List).

bag.to_set(B) = bag.to_set_without_duplicates(B).

bag.to_set_without_duplicates(B) = Xs :-
    bag.to_set_without_duplicates(B, Xs).

bag.to_set_without_duplicates(bag(Bag), Set) :-
    map.keys(Bag, List),
    set.sorted_list_to_set(List, Set).

%---------------------------------------------------------------------------%

bag.delete(B1, X) = B2 :-
    bag.delete(X, B1, B2).

bag.delete(Item, !Bag) :-
    ( bag.remove(Item, !.Bag, NewBag) ->
        !:Bag = NewBag 
    ;
        true
    ).

bag.remove(Item, bag(!.Bag), bag(!:Bag)) :-
    map.search(!.Bag, Item, Count0),
    ( Count0 > 1 ->
        Count = Count0 - 1,
        map.set(Item, Count, !Bag)
    ;
        map.delete(Item, !Bag)
    ).

bag.det_remove(B1, X) = B2 :-
    bag.det_remove(X, B1, B2).

bag.det_remove(Bag0, Item, Bag) :-
    ( bag.remove(Bag0, Item, Bag1) ->
        Bag = Bag1
    ;
        error("bag.det_remove: Missing item in bag.")
    ).

bag.det_remove_list(B1, Xs) = B2 :-
    bag.det_remove_list(Xs, B1, B2).

bag.remove_list([], !Bag).
bag.remove_list([X | Xs], !Bag) :-
    bag.remove(X, !Bag),
    bag.remove_list(Xs, !Bag).

bag.det_remove_list(List, !Bag) :-
    ( bag.remove_list(List, !Bag) ->
        true
    ;
        error("bag.det_remove_list: Missing item in bag.")
    ).

bag.remove_set(Set, !Bag) :-
    set.to_sorted_list(Set, List),
        % XXX We should exploit the sortedness of List.
    bag.remove_list(List, !Bag).

bag.det_remove_set(B1, Xs) = B2 :-
    bag.det_remove_set(Xs, B1, B2).

bag.det_remove_set(Set, !Bag) :-
    set.to_sorted_list(Set, List),
        % XXX We should exploit the sortedness of List.
    bag.det_remove_list(List, !Bag).

bag.remove_all(Item, bag(!.Bag), bag(!:Bag)) :- % semidet
    map.remove(Item, _Val, !Bag).

bag.delete_all(B1, X) = B2 :-
    bag.delete_all(X, B1, B2).

bag.delete_all(Item, bag(!.Bag), bag(!:Bag)) :- % det
    map.delete(Item, !Bag).

%---------------------------------------------------------------------------%

bag.contains(bag(Bag), Item) :-
    map.contains(Bag, Item).

%---------------------------------------------------------------------------%

bag.count_value(B, X) = N :-
    bag.count_value(B, X, N).

bag.count_value(bag(Bag), Item, Count) :-
    ( map.search(Bag, Item, Count0) ->
        Count = Count0
    ;
        Count = 0
    ).

%---------------------------------------------------------------------------%

bag.subtract(B1, B2) = B3 :-
    bag.subtract(B1, B2, B3).

bag.subtract(bag(Bag0), bag(SubBag), Bag) :-
    ( map.remove_smallest(SubKey, SubVal, SubBag, SubBag0) ->
        ( map.search(Bag0, SubKey, Val) ->
            NewVal = Val - SubVal,
            ( NewVal > 0 ->
                map.det_update(SubKey, NewVal, Bag0, Bag1)
            ;
                map.det_remove(SubKey, _Val, Bag0, Bag1)
            )
        ;
            Bag1 = Bag0
        ),
        bag.subtract(bag(Bag1), bag(SubBag0), Bag)
    ;
        Bag = bag(Bag0)
    ).

bag.union(B1, B2) = B3 :-
    bag.union(B1, B2, B3).

bag.union(bag(A), bag(B), Out) :-
    ( map.remove_smallest(Key, BVal, B, B0) ->
        ( map.search(A, Key, AVal) ->
            NewVal = AVal + BVal,
            map.det_update(Key, NewVal, A, A0)
        ;
            map.det_insert(Key, BVal, A, A0)
        ),
        bag.union(bag(A0), bag(B0), Out)
    ;
        Out = bag(A)
    ).

bag.intersect(B1, B2) = B3 :-
    bag.intersect(B1, B2, B3).

bag.intersect(A, B, Out) :-
    bag.init(Out0),
    bag.intersect_2(A, B, Out0, Out).

:- pred bag.intersect_2(bag(T)::in, bag(T)::in, bag(T)::in, bag(T)::out)
    is det.

bag.intersect_2(bag(A), bag(B), bag(Out0), Out) :-
    ( map.remove_smallest(Key, AVal, A, A0) ->
        ( map.search(B, Key, BVal) ->
            int.min(AVal, BVal, Val),
            map.det_insert(Key, Val, Out0, Out1)
        ;
            Out1 = Out0
        ),
        bag.intersect_2(bag(A0), bag(B), bag(Out1), Out)
    ;
        Out = bag(Out0)
    ).

bag.intersect(bag(A), bag(B)) :-
    map.remove_smallest(Key, _AVal, A, A0),
    ( map.contains(B, Key) ->
        true
    ;
        bag.intersect(bag(A0), bag(B))
    ).

bag.least_upper_bound(B1, B2) = B3 :-
    bag.least_upper_bound(B1, B2, B3).

bag.least_upper_bound(bag(A), bag(B), Out) :-
    ( map.remove_smallest(Key, BVal, B, B0) ->
        ( map.search(A, Key, AVal) ->
            int.max(AVal, BVal, NewVal),
            map.det_update(Key, NewVal, A, A0)
        ;
            map.det_insert(Key, BVal, A, A0)
        ),
        bag.least_upper_bound(bag(A0), bag(B0), Out)
    ;
        Out = bag(A)
    ).

%---------------------------------------------------------------------------%

bag.is_subbag(SubBag, BigBag) :-
    bag.subtract(SubBag, BigBag, SubBag0),
    bag.is_empty(SubBag0).

%---------------------------------------------------------------------------%

bag.is_empty(bag(Bag)) :-
    map.is_empty(Bag).

%---------------------------------------------------------------------------%

bag.remove_smallest(Item, bag(!.Bag), bag(!:Bag)) :-
    map.remove_smallest(Item, Val, !Bag),
    ( Val > 1 ->
        NewVal = Val - 1,
        map.det_insert(Item, NewVal, !Bag)
    ;
        true
    ).

    % compares the two bags, and returns whether the first bag is a
    % subset (<), is equal (=), or is a superset (>) of the second
    % bag.subset_compare(<, {apple, orange}, {apple, apple, orange}).
    % bag.subset_compare(=, {apple, orange}, {apple, orange}).
    % bag.subset_compare(>, {apple, apple, orange}, {apple, orange}).
    % bag.subset_compare(_, {apple, apple}, {orange, orange}) :- fail.
    % :- pred bag.subset_compare(comparison_result, bag(T), bag(T)).
    % :- mode bag.subset_compare(out, in, in) is semidet.
    %
bag.subset_compare(Res, bag(A), bag(B)) :-
    ( map.remove_smallest(Key, AVal, A, A0) ->
        ( map.remove(Key, BVal, B, B0) ->
            compare(ValRes, AVal, BVal),
            (
                ValRes = (>),
                bag.is_subbag(bag(B0), bag(A0)),
                Res = (>)
            ;
                ValRes = (=),
                bag.subset_compare(Res, bag(A0), bag(B0))
            ;
                ValRes = (<),
                bag.is_subbag(bag(A0), bag(B0)),
                Res = (<)
            )
        ;
            % B is empty, but A is not
            Res = (>)
        )
    ;
        % A is empty
        ( map.is_empty(B) ->
            Res = (=)
        ;
            Res = (<)
        )
    ).

%---------------------------------------------------------------------------%

bag.foldl(Pred, bag(Bag), !Acc) :-
    map.foldl(Pred, Bag, !Acc).

bag.foldl2(Pred, bag(Bag), !Acc1, !Acc2) :-
    map.foldl2(Pred, Bag, !Acc1, !Acc2).

%---------------------------------------------------------------------------%
:- end_module bag.
%---------------------------------------------------------------------------%
