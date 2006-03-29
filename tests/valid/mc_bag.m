%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1999, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: mc_bag.m
%   An implementation of multisets. This is a copy of the standard library
%   module bag, and provides a reasonably large test case for the
%   propagation solver approach to constraints based mode analysis to be
%   run on.
% main author: conway, crs.
% stability: medium
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mc_bag.

:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module set.

:- type mc_bag(T).

    % Create an empty mc_bag.
    %
:- pred mc_bag__init(mc_bag(T)::out) is det.
:- func mc_bag__init = mc_bag(T).

    % Insert a particular value in a mc_bag.
    %
:- pred mc_bag__insert(mc_bag(T)::in, T::in, mc_bag(T)::out) is det.
:- func mc_bag__insert(mc_bag(T), T) = mc_bag(T).

    % Insert a list of values into a mc_bag.
    %
:- pred mc_bag__insert_list(mc_bag(T)::in, list(T)::in, mc_bag(T)::out) is det.
:- func mc_bag__insert_list(mc_bag(T), list(T)) = mc_bag(T).

    % Insert a list of values into a mc_bag.
    %
:- pred mc_bag__insert_set(mc_bag(T)::in, set(T)::in, mc_bag(T)::out) is det.
:- func mc_bag__insert_set(mc_bag(T), set(T)) = mc_bag(T).

    % Make a mc_bag from a list.
    %
:- func mc_bag__mc_bag(list(T)) = mc_bag(T).
:- pred mc_bag__from_list(list(T)::in, mc_bag(T)::out) is det.
:- func mc_bag__from_list(list(T)) = mc_bag(T).

    % Make a mc_bag from a set.
    %
:- pred mc_bag__from_set(set(T)::in, mc_bag(T)::out) is det.
:- func mc_bag__from_set(set(T)) = mc_bag(T).

    % Given a mc_bag, produce a sorted list containing all the values in
    % the mc_bag.  Each value will appear in the list the same number of
    % times that it appears in the mc_bag.
    %
:- pred mc_bag__to_list(mc_bag(T)::in, list(T)::out) is det.
:- func mc_bag__to_list(mc_bag(T)) = list(T).

    % Given a mc_bag, produce a sorted list containing all the values in the mc_bag.
    % Each value will appear in the list once, with the associated integer
    % giving the number of times that it appears in the mc_bag.
    %
:- pred mc_bag__to_assoc_list(mc_bag(T)::in, assoc_list(T, int)::out) is det.
:- func mc_bag__to_assoc_list(mc_bag(T)) = assoc_list(T, int).

    % Given a mc_bag, produce a sorted list with no duplicates containing
    % all the values in the mc_bag.
    %
:- pred mc_bag__to_list_without_duplicates(mc_bag(T)::in, list(T)::out) is det.
:- func mc_bag__to_list_without_duplicates(mc_bag(T)) = list(T).

    % Given a mc_bag, produce a set containing all the values in the mc_bag.
    %
:- pred mc_bag__to_set_without_duplicates(mc_bag(T)::in, set(T)::out) is det.
:- func mc_bag__to_set_without_duplicates(mc_bag(T)) = set(T).
:- func mc_bag__to_set(mc_bag(T)) = set(T).

    % Remove one occurrence of a particular value from a mc_bag.
    % Fail if the item does not exist in the mc_bag.
    %
:- pred mc_bag__remove(mc_bag(T)::in, T::in, mc_bag(T)::out) is semidet.

    % Remove one occurrence of a particular value from a mc_bag.
    % Abort if the item does not exist in the mc_bag.
    %
:- pred mc_bag__det_remove(mc_bag(T)::in, T::in, mc_bag(T)::out) is det.
:- func mc_bag__det_remove(mc_bag(T), T) = mc_bag(T).

    % Remove a list of values from a mc_bag. Duplicates are removed from the mc_bag
    % the appropriate number of times. Fail if any of the items in the list
    % do not exist in the mc_bag.
    %
    % This call is logically equivalent to:
    %
    %   mc_bag__remove_list(Bag0, RemoveList, Bag) :-
    %       mc_bag__from_list(RemoveList, RemoveBag),
    %       mc_bag__is_submc_bag(RemoveBag, Bag0),
    %       mc_bag__subtract(Bag0, RemoveBag, Bag).
    %
:- pred mc_bag__remove_list(mc_bag(T)::in, list(T)::in, mc_bag(T)::out) is semidet.

    % Remove a list of values from a mc_bag. Duplicates are removed from the mc_bag
    % the appropriate number of times.  Abort if any of the items in the list
    % do not exist in the mc_bag.
    %
:- pred mc_bag__det_remove_list(mc_bag(T)::in, list(T)::in, mc_bag(T)::out) is det.
:- func mc_bag__det_remove_list(mc_bag(T), list(T)) = mc_bag(T).

    % Remove a set of values from a mc_bag. Each value is removed once.
    % Fail if any of the items in the set do not exist in the mc_bag.
    %
:- pred mc_bag__remove_set(mc_bag(T)::in, set(T)::in, mc_bag(T)::out) is semidet.

    % Remove a set of values from a mc_bag. Each value is removed once.
    % Abort if any of the items in the set do not exist in the mc_bag.
    %
:- pred mc_bag__det_remove_set(mc_bag(T)::in, set(T)::in, mc_bag(T)::out) is det.
:- func mc_bag__det_remove_set(mc_bag(T), set(T)) = mc_bag(T).

    % Delete one occurrence of a particular value from a mc_bag.
    % If the key is not present, leave the mc_bag unchanged.
    %
:- pred mc_bag__delete(mc_bag(T)::in, T::in, mc_bag(T)::out) is det.
:- func mc_bag__delete(mc_bag(T), T) = mc_bag(T).

    % Remove all occurrences of a particular value from a mc_bag.
    % Fail if the item does not exist in the mc_bag.
    %
:- pred mc_bag__remove_all(mc_bag(T)::in, T::in, mc_bag(T)::out) is semidet.

:- func mc_bag__delete_all(mc_bag(T), T) = mc_bag(T).

    % Delete all occurrences of a particular value from a mc_bag.
    %
:- pred mc_bag__delete_all(mc_bag(T)::in, T::in, mc_bag(T)::out) is det.

    % Check whether a mc_bag contains a particular value.
    %
:- pred mc_bag__contains(mc_bag(T)::in, T::in) is semidet.

    % Count how many occurrences of the value the mc_bag contains.
    %
:- pred mc_bag__count_value(mc_bag(T)::in, T::in, int::out) is det.
:- func mc_bag__count_value(mc_bag(T), T) = int.

    % mc_bag__subtract(Bag0, SubBag, Bag):
    %
    % Subtracts SubBag from Bag0 to produce Bag. Each element in SubBag is
    % removed from Bag0 to produce Bag. If an element exists in SubBag,
    % but not in Bag, then that element is not removed. An example:
    % mc_bag__subtract({1, 1, 2, 2, 3 }, {1, 1, 2, 3, 3, 3}, {2}).
    %
:- pred mc_bag__subtract(mc_bag(T)::in, mc_bag(T)::in, mc_bag(T)::out) is det.
:- func mc_bag__subtract(mc_bag(T), mc_bag(T)) = mc_bag(T).

    % The third mc_bag is the union of the first 2 mc_bags,
    % e.g. {1, 1, 2, 2} U {2, 2, 3, 3} = {1, 1, 2, 2, 2, 2, 3, 3}.
    % If the two input mc_bags are known to be unequal in size, then making
    % the first mc_bag the larger mc_bag will usually be more efficient.
    %
:- pred mc_bag__union(mc_bag(T)::in, mc_bag(T)::in, mc_bag(T)::out) is det.
:- func mc_bag__union(mc_bag(T), mc_bag(T)) = mc_bag(T).

    % The third mc_bag is the intersection of the first 2 mc_bags. Every element
    % in the third mc_bag exists in both of the first 2 mc_bags, e.g.
    % mc_bag__intersect({1, 2, 2, 3, 3}, {2, 2, 3, 4}, {2, 2, 3}).
    %
:- pred mc_bag__intersect(mc_bag(T)::in, mc_bag(T)::in, mc_bag(T)::out) is det.
:- func mc_bag__intersect(mc_bag(T), mc_bag(T)) = mc_bag(T).

    % Fails if there is no intersection between the 2 mc_bags.
    % mc_bag__intersect(A, B) :- mc_bag__intersect(A, B, C), not mc_bag__is_empty(C).
    %
:- pred mc_bag__intersect(mc_bag(T)::in, mc_bag(T)::in) is semidet.

    % The third mc_bag is the smallest mc_bag that has both the first two mc_bags
    % as submc_bags. If an element X is present N times in one of the first
    % two mc_bags, X will be present at least N times in the third mc_bag.
    % E.g. {1, 1, 2} upper_bound {2, 2, 3} = {1, 1, 2, 2, 3}.
    % If the two input mc_bags are known to be unequal in size, then making
    % the first mc_bag the larger mc_bag will usually be more efficient.
    %
:- pred mc_bag__least_upper_bound(mc_bag(T)::in, mc_bag(T)::in, mc_bag(T)::out) is det.
:- func mc_bag__least_upper_bound(mc_bag(T), mc_bag(T)) = mc_bag(T).

    % Tests whether the first mc_bag is a submc_bag of the second.
    % mc_bag__is_submc_bag(A, B) implies that every element in the mc_bag A
    % is also in the mc_bag B. If an element is in mc_bag A multiple times,
    % it must be in mc_bag B at least as many times.
    % e.g. mc_bag__is_submc_bag({1, 1, 2}, {1, 1, 2, 2, 3}).
    % e.g. mc_bag__is_submc_bag({1, 1, 2}, {1, 2, 3}) :- fail.
    %
:- pred mc_bag__is_submc_bag(mc_bag(T)::in, mc_bag(T)::in) is semidet.

    % Check whether a mc_bag is empty.
    %
:- pred mc_bag__is_empty(mc_bag(T)::in) is semidet.

    % Fails if the mc_bag is empty.
    %
:- pred mc_bag__remove_smallest(mc_bag(T)::in, T::out, mc_bag(T)::out) is semidet.

    % Compares the two mc_bags, and returns whether the first mc_bag is a
    % subset (<), is equal (=), or is a superset (>) of the second.
    % mc_bag__subset_compare(<, {apple, orange}, {apple, apple, orange}).
    % mc_bag__subset_compare(=, {apple, orange}, {apple, orange}).
    % mc_bag__subset_compare(>, {apple, apple, orange}, {apple, orange}).
    % mc_bag__subset_compare(_, {apple, apple}, {orange, orange}) :- fail.
    %
:- pred mc_bag__subset_compare(comparison_result::out, mc_bag(T)::in, mc_bag(T)::in)
    is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module map.
:- import_module require.
:- import_module std_util.

:- type mc_bag(T)      ==  map(T, int).

%---------------------------------------------------------------------------%

mc_bag__init(Bag) :-
    map__init(Bag).

%---------------------------------------------------------------------------%

mc_bag__insert(Bag0, Item, Bag) :-
    ( map__search(Bag0, Item, Count0) ->
        Count = Count0 + 1
    ;
        Count = 1
    ),
    map__set(Bag0, Item, Count, Bag).

%---------------------------------------------------------------------------%

mc_bag__insert_list(Bag, [], Bag).
mc_bag__insert_list(Bag0, [Item|Items], Bag) :-
    mc_bag__insert(Bag0, Item, Bag1),
    mc_bag__insert_list(Bag1, Items, Bag).

mc_bag__insert_set(Bag0, Set, Bag) :-
    set__to_sorted_list(Set, List),
    % XXX We should exploit the sortedness of List.
    mc_bag__insert_list(Bag0, List, Bag).

mc_bag__from_list(List, Bag) :-
    mc_bag__init(Bag0),
    mc_bag__insert_list(Bag0, List, Bag).

mc_bag__from_set(Set, Bag) :-
    set__to_sorted_list(Set, List),
    mc_bag__init(Bag0),
    % XXX We should exploit the sortedness of List.
    mc_bag__insert_list(Bag0, List, Bag).

mc_bag__to_list(Bag, List) :-
    map__to_assoc_list(Bag, AssocList),
    mc_bag__to_list_2(AssocList, List).

:- pred mc_bag__to_list_2(assoc_list(T, int)::in, list(T)::out) is det.

mc_bag__to_list_2([], []).
mc_bag__to_list_2([X - Int | Xs ], Out) :-
    ( Int =< 0 ->
        mc_bag__to_list_2(Xs, Out)
    ;
        NewInt = Int - 1,
        mc_bag__to_list_2([X - NewInt | Xs], Out0),
        Out = [X | Out0]
    ).

mc_bag__to_assoc_list(Bag, AssocList) :-
    map__to_assoc_list(Bag, AssocList).

mc_bag__to_list_without_duplicates(Bag, List) :-
    map__keys(Bag, List).

mc_bag__to_set_without_duplicates(Bag, Set) :-
    map__keys(Bag, List),
    set__sorted_list_to_set(List, Set).

%---------------------------------------------------------------------------%

mc_bag__delete(Bag0, Item, Bag) :-
    ( mc_bag__remove(Bag0, Item, Bag1) ->
        Bag = Bag1
    ;
        Bag = Bag0
    ).

mc_bag__remove(Bag0, Item, Bag) :-
    map__search(Bag0, Item, Count0),
    ( Count0 > 1 ->
        Count = Count0 - 1,
        map__set(Bag0, Item, Count, Bag)
    ;
        map__delete(Bag0, Item, Bag)
    ).

mc_bag__det_remove(Bag0, Item, Bag) :-
    ( mc_bag__remove(Bag0, Item, Bag1) ->
        Bag = Bag1
    ;
        error("mc_bag__det_remove: Missing item in mc_bag.")
    ).

mc_bag__remove_list(Bag, [], Bag).
mc_bag__remove_list(Bag0, [X | Xs], Bag) :-
    mc_bag__remove(Bag0, X, Bag1),
    mc_bag__remove_list(Bag1, Xs, Bag).

mc_bag__det_remove_list(Bag0, List, Bag) :-
    ( mc_bag__remove_list(Bag0, List, Bag1) ->
        Bag = Bag1
    ;
        error("mc_bag__det_remove_list: Missing item in mc_bag.")
    ).

mc_bag__remove_set(Bag0, Set, Bag) :-
    set__to_sorted_list(Set, List),
        % XXX We should exploit the sortedness of List.
    mc_bag__remove_list(Bag0, List, Bag).

mc_bag__det_remove_set(Bag0, Set, Bag) :-
    set__to_sorted_list(Set, List),
        % XXX We should exploit the sortedness of List.
    mc_bag__det_remove_list(Bag0, List, Bag).

mc_bag__remove_all(Bag0, Item, Bag) :-     % semidet
    map__remove(Bag0, Item, _Val, Bag).

mc_bag__delete_all(Bag0, Item, Bag) :- % det
    map__delete(Bag0, Item, Bag).

%---------------------------------------------------------------------------%

mc_bag__contains(Bag, Item) :-
    map__contains(Bag, Item).

%---------------------------------------------------------------------------%

mc_bag__count_value(Bag, Item, Count) :-
    ( map__search(Bag, Item, Count0) ->
        Count = Count0
    ;
        Count = 0
    ).

%---------------------------------------------------------------------------%

mc_bag__subtract(Bag0, SubBag, Bag) :-
    ( map__remove_smallest(SubBag, SubKey, SubVal, SubBag0) ->
        ( map__search(Bag0, SubKey, Val) ->
            NewVal = Val - SubVal,
            ( NewVal > 0 ->
                map__det_update(Bag0, SubKey, NewVal, Bag1)
            ;
                map__det_remove(Bag0, SubKey, _Val, Bag1)
            )
        ;
            Bag1 = Bag0
        ),
        mc_bag__subtract(Bag1, SubBag0, Bag)
    ;
        Bag = Bag0
    ).

mc_bag__union(A, B, Out) :-
    ( map__remove_smallest(B, Key, BVal, B0) ->
        ( map__search(A, Key, AVal) ->
            NewVal = AVal + BVal,
            map__det_update(A, Key, NewVal, A0)
        ;
            map__det_insert(A, Key, BVal, A0)
        ),
        mc_bag__union(A0, B0, Out)
    ;
        Out = A
    ).

mc_bag__intersect(A, B, Out) :-
    mc_bag__init(Out0),
    mc_bag__intersect_2(A, B, Out0, Out).

:- pred mc_bag__intersect_2(mc_bag(T)::in, mc_bag(T)::in, mc_bag(T)::in, mc_bag(T)::out)
    is det.

mc_bag__intersect_2(A, B, Out0, Out) :-
    ( map__remove_smallest(A, Key, AVal,A0) ->
        ( map__search(B, Key, BVal) ->
            int__min(AVal, BVal, Val),
            map__det_insert(Out0, Key, Val, Out1)
        ;
            Out1 = Out0
        ),
        mc_bag__intersect_2(A0, B, Out1, Out)
    ;
        Out = Out0
    ).

mc_bag__intersect(A, B) :-
    map__remove_smallest(A, Key, _AVal,A0),
    ( map__contains(B, Key) ->
        true
    ;
        mc_bag__intersect(A0, B)
    ).

mc_bag__least_upper_bound(A, B, Out) :-
    ( map__remove_smallest(B, Key, BVal, B0) ->
        ( map__search(A, Key, AVal) ->
            int__max(AVal, BVal, NewVal),
            map__det_update(A, Key, NewVal, A0)
        ;
            map__det_insert(A, Key, BVal, A0)
        ),
        mc_bag__least_upper_bound(A0, B0, Out)
    ;
        Out = A
    ).

%---------------------------------------------------------------------------%

mc_bag__is_submc_bag(SubBag, BigBag) :-
    mc_bag__subtract(SubBag, BigBag, SubBag0),
    mc_bag__is_empty(SubBag0).

%---------------------------------------------------------------------------%

mc_bag__is_empty(Bag) :-
    map__is_empty(Bag).

%---------------------------------------------------------------------------%

mc_bag__remove_smallest(Bag0, Item, Bag) :-
    map__remove_smallest(Bag0, Item, Val, Bag1),
    ( Val > 1 ->
        NewVal = Val - 1,
        map__det_insert(Bag1, Item, NewVal, Bag)
    ;
        Bag = Bag1
    ).

    % compares the two mc_bags, and returns whether the first mc_bag is a
    % subset (<), is equal (=), or is a superset (>) of the second
    % mc_bag__subset_compare(<, {apple, orange}, {apple, apple, orange}).
    % mc_bag__subset_compare(=, {apple, orange}, {apple, orange}).
    % mc_bag__subset_compare(>, {apple, apple, orange}, {apple, orange}).
    % mc_bag__subset_compare(_, {apple, apple}, {orange, orange}) :- fail.
    % :- pred mc_bag__subset_compare(comparison_result, mc_bag(T), mc_bag(T)).
    % :- mode mc_bag__subset_compare(out, in, in) is semidet.
    %
mc_bag__subset_compare(Res, A, B) :-
    ( map__remove_smallest(A, Key, AVal, A0) ->
        ( map__remove(B, Key, BVal, B0) ->
            compare(ValRes, AVal, BVal),
            (
                ValRes = (>),
                mc_bag__is_submc_bag(B0, A0),
                Res = (>)
            ;
                ValRes = (=),
                mc_bag__subset_compare(Res, A0, B0)
            ;
                ValRes = (<),
                mc_bag__is_submc_bag(A0, B0),
                Res = (<)
            )
        ;
            % B is empty, but A is not
            Res = (>)
        )
    ;
        % A is empty
        ( map__is_empty(B) ->
            Res = (=)
        ;
            Res = (<)
        )
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
%   Function forms added.

mc_bag__init = B :-
    mc_bag__init(B).

mc_bag__insert(B1, X) = B2 :-
    mc_bag__insert(B1, X, B2).

mc_bag__insert_list(B1, Xs) = B2 :-
    mc_bag__insert_list(B1, Xs, B2).

mc_bag__insert_set(B1, Xs) = B2 :-
    mc_bag__insert_set(B1, Xs, B2).

mc_bag__from_list(Xs) = B :-
    mc_bag__from_list(Xs, B).

mc_bag__from_set(Xs) = B :-
    mc_bag__from_set(Xs, B).

mc_bag__to_list(B) = Xs :-
    mc_bag__to_list(B, Xs).

mc_bag__to_assoc_list(B) = AL :-
    mc_bag__to_assoc_list(B, AL).

mc_bag__to_list_without_duplicates(B) = Xs :-
    mc_bag__to_list_without_duplicates(B, Xs).

mc_bag__to_set_without_duplicates(B) = Xs :-
    mc_bag__to_set_without_duplicates(B, Xs).

mc_bag__det_remove(B1, X) = B2 :-
    mc_bag__det_remove(B1, X, B2).

mc_bag__det_remove_list(B1, Xs) = B2 :-
    mc_bag__det_remove_list(B1, Xs, B2).

mc_bag__det_remove_set(B1, Xs) = B2 :-
    mc_bag__det_remove_set(B1, Xs, B2).

mc_bag__delete(B1, X) = B2 :-
    mc_bag__delete(B1, X, B2).

mc_bag__delete_all(B1, X) = B2 :-
    mc_bag__delete_all(B1, X, B2).

mc_bag__count_value(B, X) = N :-
    mc_bag__count_value(B, X, N).

mc_bag__subtract(B1, B2) = B3 :-
    mc_bag__subtract(B1, B2, B3).

mc_bag__union(B1, B2) = B3 :-
    mc_bag__union(B1, B2, B3).

mc_bag__intersect(B1, B2) = B3 :-
    mc_bag__intersect(B1, B2, B3).

mc_bag__least_upper_bound(B1, B2) = B3 :-
    mc_bag__least_upper_bound(B1, B2, B3).

mc_bag__mc_bag(Xs) = mc_bag__from_list(Xs).

mc_bag__to_set(B) = mc_bag__to_set_without_duplicates(B).
