%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% An implementation of multisets. This is a copy of the standard library
% module bag, and provides a reasonably large test case for the
% propagation solver approach to constraints based mode analysis to be
% run on.
%
%---------------------------------------------------------------------------%

:- module mc_bag.

:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module set.

:- type mc_bag(T).

    % Create an empty mc_bag.
    %
:- pred init(mc_bag(T)::out) is det.
:- func init = mc_bag(T).

    % Insert a particular value in a mc_bag.
    %
:- pred insert(mc_bag(T)::in, T::in, mc_bag(T)::out) is det.
:- func insert(mc_bag(T), T) = mc_bag(T).

    % Insert a list of values into a mc_bag.
    %
:- pred insert_list(mc_bag(T)::in, list(T)::in, mc_bag(T)::out) is det.
:- func insert_list(mc_bag(T), list(T)) = mc_bag(T).

    % Insert a list of values into a mc_bag.
    %
:- pred insert_set(mc_bag(T)::in, set(T)::in, mc_bag(T)::out) is det.
:- func insert_set(mc_bag(T), set(T)) = mc_bag(T).

    % Make a mc_bag from a list.
    %
:- func mc_bag(list(T)) = mc_bag(T).
:- pred from_list(list(T)::in, mc_bag(T)::out) is det.
:- func from_list(list(T)) = mc_bag(T).

    % Make a mc_bag from a set.
    %
:- pred from_set(set(T)::in, mc_bag(T)::out) is det.
:- func from_set(set(T)) = mc_bag(T).

    % Given a mc_bag, produce a sorted list containing all the values in
    % the mc_bag. Each value will appear in the list the same number of
    % times that it appears in the mc_bag.
    %
:- pred to_list(mc_bag(T)::in, list(T)::out) is det.
:- func to_list(mc_bag(T)) = list(T).

    % Given a mc_bag, produce a sorted list containing all the values
    % in the mc_bag.
    % Each value will appear in the list once, with the associated integer
    % giving the number of times that it appears in the mc_bag.
    %
:- pred to_assoc_list(mc_bag(T)::in, assoc_list(T, int)::out) is det.
:- func to_assoc_list(mc_bag(T)) = assoc_list(T, int).

    % Given a mc_bag, produce a sorted list with no duplicates containing
    % all the values in the mc_bag.
    %
:- pred to_list_without_duplicates(mc_bag(T)::in, list(T)::out) is det.
:- func to_list_without_duplicates(mc_bag(T)) = list(T).

    % Given a mc_bag, produce a set containing all the values in the mc_bag.
    %
:- pred to_set_without_duplicates(mc_bag(T)::in, set(T)::out) is det.
:- func to_set_without_duplicates(mc_bag(T)) = set(T).
:- func to_set(mc_bag(T)) = set(T).

    % Remove one occurrence of a particular value from a mc_bag.
    % Fail if the item does not exist in the mc_bag.
    %
:- pred remove(mc_bag(T)::in, T::in, mc_bag(T)::out) is semidet.

    % Remove one occurrence of a particular value from a mc_bag.
    % Abort if the item does not exist in the mc_bag.
    %
:- pred det_remove(mc_bag(T)::in, T::in, mc_bag(T)::out) is det.
:- func det_remove(mc_bag(T), T) = mc_bag(T).

    % Remove a list of values from a mc_bag. Duplicates are removed
    % from the mc_bag the appropriate number of times. Fail if
    % any of the items in the list do not exist in the mc_bag.
    %
    % This call is logically equivalent to:
    %
    %   remove_list(Bag0, RemoveList, Bag) :-
    %       from_list(RemoveList, RemoveBag),
    %       is_submc_bag(RemoveBag, Bag0),
    %       subtract(Bag0, RemoveBag, Bag).
    %
:- pred remove_list(mc_bag(T)::in, list(T)::in, mc_bag(T)::out) is semidet.

    % Remove a list of values from a mc_bag. Duplicates are removed
    % from the mc_bag the appropriate number of times. Abort if
    % any of the items in the list do not exist in the mc_bag.
    %
:- pred det_remove_list(mc_bag(T)::in, list(T)::in, mc_bag(T)::out) is det.
:- func det_remove_list(mc_bag(T), list(T)) = mc_bag(T).

    % Remove a set of values from a mc_bag. Each value is removed once.
    % Fail if any of the items in the set do not exist in the mc_bag.
    %
:- pred remove_set(mc_bag(T)::in, set(T)::in, mc_bag(T)::out)
    is semidet.

    % Remove a set of values from a mc_bag. Each value is removed once.
    % Abort if any of the items in the set do not exist in the mc_bag.
    %
:- pred det_remove_set(mc_bag(T)::in, set(T)::in, mc_bag(T)::out) is det.
:- func det_remove_set(mc_bag(T), set(T)) = mc_bag(T).

    % Delete one occurrence of a particular value from a mc_bag.
    % If the key is not present, leave the mc_bag unchanged.
    %
:- pred delete(mc_bag(T)::in, T::in, mc_bag(T)::out) is det.
:- func delete(mc_bag(T), T) = mc_bag(T).

    % Remove all occurrences of a particular value from a mc_bag.
    % Fail if the item does not exist in the mc_bag.
    %
:- pred remove_all(mc_bag(T)::in, T::in, mc_bag(T)::out) is semidet.

:- func delete_all(mc_bag(T), T) = mc_bag(T).

    % Delete all occurrences of a particular value from a mc_bag.
    %
:- pred delete_all(mc_bag(T)::in, T::in, mc_bag(T)::out) is det.

    % Check whether a mc_bag contains a particular value.
    %
:- pred contains(mc_bag(T)::in, T::in) is semidet.

    % Count how many occurrences of the value the mc_bag contains.
    %
:- pred count_value(mc_bag(T)::in, T::in, int::out) is det.
:- func count_value(mc_bag(T), T) = int.

    % subtract(Bag0, SubBag, Bag):
    %
    % Subtracts SubBag from Bag0 to produce Bag. Each element in SubBag is
    % removed from Bag0 to produce Bag. If an element exists in SubBag,
    % but not in Bag, then that element is not removed. An example:
    % subtract({1, 1, 2, 2, 3 }, {1, 1, 2, 3, 3, 3}, {2}).
    %
:- pred subtract(mc_bag(T)::in, mc_bag(T)::in, mc_bag(T)::out) is det.
:- func subtract(mc_bag(T), mc_bag(T)) = mc_bag(T).

    % The third mc_bag is the union of the first 2 mc_bags,
    % e.g. {1, 1, 2, 2} U {2, 2, 3, 3} = {1, 1, 2, 2, 2, 2, 3, 3}.
    % If the two input mc_bags are known to be unequal in size, then making
    % the first mc_bag the larger mc_bag will usually be more efficient.
    %
:- pred union(mc_bag(T)::in, mc_bag(T)::in, mc_bag(T)::out) is det.
:- func union(mc_bag(T), mc_bag(T)) = mc_bag(T).

    % The third mc_bag is the intersection of the first 2 mc_bags. Every element
    % in the third mc_bag exists in both of the first 2 mc_bags, e.g.
    % intersect({1, 2, 2, 3, 3}, {2, 2, 3, 4}, {2, 2, 3}).
    %
:- pred intersect(mc_bag(T)::in, mc_bag(T)::in, mc_bag(T)::out) is det.
:- func intersect(mc_bag(T), mc_bag(T)) = mc_bag(T).

    % Fails if there is no intersection between the 2 mc_bags.
    % intersect(A, B) :- intersect(A, B, C), not is_empty(C).
    %
:- pred intersect(mc_bag(T)::in, mc_bag(T)::in) is semidet.

    % The third mc_bag is the smallest mc_bag that has both the first two
    % mc_bags as submc_bags. If an element X is present N times in
    % one of the first two mc_bags, X will be present at least N times
    % in the third mc_bag.
    % E.g. {1, 1, 2} upper_bound {2, 2, 3} = {1, 1, 2, 2, 3}.
    % If the two input mc_bags are known to be unequal in size, then making
    % the first mc_bag the larger mc_bag will usually be more efficient.
    %
:- pred least_upper_bound(mc_bag(T)::in, mc_bag(T)::in, mc_bag(T)::out) is det.
:- func least_upper_bound(mc_bag(T), mc_bag(T)) = mc_bag(T).

    % Tests whether the first mc_bag is a submc_bag of the second.
    % is_submc_bag(A, B) implies that every element in the mc_bag A
    % is also in the mc_bag B. If an element is in mc_bag A multiple times,
    % it must be in mc_bag B at least as many times.
    % e.g. is_submc_bag({1, 1, 2}, {1, 1, 2, 2, 3}).
    % e.g. is_submc_bag({1, 1, 2}, {1, 2, 3}) :- fail.
    %
:- pred is_submc_bag(mc_bag(T)::in, mc_bag(T)::in) is semidet.

    % Check whether a mc_bag is empty.
    %
:- pred is_empty(mc_bag(T)::in) is semidet.

    % Fails if the mc_bag is empty.
    %
:- pred remove_smallest(mc_bag(T)::in, T::out, mc_bag(T)::out) is semidet.

    % Compares the two mc_bags, and returns whether the first mc_bag is a
    % subset (<), is equal (=), or is a superset (>) of the second.
    % subset_compare(<, {apple, orange}, {apple, apple, orange}).
    % subset_compare(=, {apple, orange}, {apple, orange}).
    % subset_compare(>, {apple, apple, orange}, {apple, orange}).
    % subset_compare(_, {apple, apple}, {orange, orange}) :- fail.
    %
:- pred subset_compare(comparison_result::out, mc_bag(T)::in, mc_bag(T)::in)
    is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module map.
:- import_module require.
:- import_module pair.

:- type mc_bag(T) ==  map(T, int).

%---------------------------------------------------------------------------%

init(Bag) :-
    map.init(Bag).

%---------------------------------------------------------------------------%

insert(Bag0, Item, Bag) :-
    ( if map.search(Bag0, Item, Count0) then
        Count = Count0 + 1
    else
        Count = 1
    ),
    map.set(Item, Count, Bag0, Bag).

%---------------------------------------------------------------------------%

insert_list(Bag, [], Bag).
insert_list(Bag0, [Item | Items], Bag) :-
    insert(Bag0, Item, Bag1),
    insert_list(Bag1, Items, Bag).

insert_set(Bag0, Set, Bag) :-
    set.to_sorted_list(Set, List),
    % XXX We should exploit the sortedness of List.
    insert_list(Bag0, List, Bag).

from_list(List, Bag) :-
    mc_bag.init(Bag0),
    mc_bag.insert_list(Bag0, List, Bag).

from_set(Set, Bag) :-
    set.to_sorted_list(Set, List),
    mc_bag.init(Bag0),
    % XXX We should exploit the sortedness of List.
    mc_bag.insert_list(Bag0, List, Bag).

to_list(Bag, List) :-
    map.to_assoc_list(Bag, AssocList),
    mc_bag.to_list_2(AssocList, List).

:- pred to_list_2(assoc_list(T, int)::in, list(T)::out) is det.

to_list_2([], []).
to_list_2([X - Int | Xs ], Out) :-
    ( if Int =< 0 then
        to_list_2(Xs, Out)
    else
        NewInt = Int - 1,
        to_list_2([X - NewInt | Xs], Out0),
        Out = [X | Out0]
    ).

to_assoc_list(Bag, AssocList) :-
    map.to_assoc_list(Bag, AssocList).

to_list_without_duplicates(Bag, List) :-
    map.keys(Bag, List).

to_set_without_duplicates(Bag, Set) :-
    map.keys(Bag, List),
    set.sorted_list_to_set(List, Set).

%---------------------------------------------------------------------------%

delete(Bag0, Item, Bag) :-
    ( if remove(Bag0, Item, Bag1) then
        Bag = Bag1
    else
        Bag = Bag0
    ).

remove(Bag0, Item, Bag) :-
    map.search(Bag0, Item, Count0),
    ( if Count0 > 1 then
        Count = Count0 - 1,
        map.set(Item, Count, Bag0, Bag)
    else
        map.delete(Item, Bag0, Bag)
    ).

det_remove(Bag0, Item, Bag) :-
    ( if remove(Bag0, Item, Bag1) then
        Bag = Bag1
    else
        error("det_remove: Missing item in mc_bag.")
    ).

remove_list(Bag, [], Bag).
remove_list(Bag0, [X | Xs], Bag) :-
    remove(Bag0, X, Bag1),
    remove_list(Bag1, Xs, Bag).

det_remove_list(Bag0, List, Bag) :-
    ( if remove_list(Bag0, List, Bag1) then
        Bag = Bag1
    else
        error("det_remove_list: Missing item in mc_bag.")
    ).

remove_set(Bag0, Set, Bag) :-
    set.to_sorted_list(Set, List),
        % XXX We should exploit the sortedness of List.
    remove_list(Bag0, List, Bag).

det_remove_set(Bag0, Set, Bag) :-
    set.to_sorted_list(Set, List),
        % XXX We should exploit the sortedness of List.
    det_remove_list(Bag0, List, Bag).

remove_all(Bag0, Item, Bag) :-     % semidet
    map.remove(Item, _Val, Bag0, Bag).

delete_all(Bag0, Item, Bag) :- % det
    map.delete(Item, Bag0, Bag).

%---------------------------------------------------------------------------%

contains(Bag, Item) :-
    map.contains(Bag, Item).

%---------------------------------------------------------------------------%

count_value(Bag, Item, Count) :-
    ( if map.search(Bag, Item, Count0) then
        Count = Count0
    else
        Count = 0
    ).

%---------------------------------------------------------------------------%

subtract(Bag0, SubBag, Bag) :-
    ( if map.remove_smallest(SubKey, SubVal, SubBag, SubBag0) then
        ( if map.search(Bag0, SubKey, Val) then
            NewVal = Val - SubVal,
            ( if NewVal > 0 then
                map.det_update(SubKey, NewVal, Bag0, Bag1)
            else
                map.det_remove(SubKey, _Val, Bag0, Bag1)
            )
        else
            Bag1 = Bag0
        ),
        subtract(Bag1, SubBag0, Bag)
    else
        Bag = Bag0
    ).

union(A, B, Out) :-
    ( if map.remove_smallest(Key, BVal, B, B0) then
        ( if map.search(A, Key, AVal) then
            NewVal = AVal + BVal,
            map.det_update(Key, NewVal, A, A0)
        else
            map.det_insert(Key, BVal, A, A0)
        ),
        union(A0, B0, Out)
    else
        Out = A
    ).

intersect(A, B, Out) :-
    mc_bag.init(Out0),
    mc_bag.intersect_2(A, B, Out0, Out).

:- pred intersect_2(mc_bag(T)::in, mc_bag(T)::in,
    mc_bag(T)::in, mc_bag(T)::out) is det.

intersect_2(A, B, Out0, Out) :-
    ( if map.remove_smallest(Key, AVal, A, A0) then
        ( if map.search(B, Key, BVal) then
            int.min(AVal, BVal, Val),
            map.det_insert(Key, Val, Out0, Out1)
        else
            Out1 = Out0
        ),
        mc_bag.intersect_2(A0, B, Out1, Out)
    else
        Out = Out0
    ).

intersect(A, B) :-
    map.remove_smallest(Key, _AVal, A, A0),
    ( if map.contains(B, Key) then
        true
    else
        mc_bag.intersect(A0, B)
    ).

least_upper_bound(A, B, Out) :-
    ( if map.remove_smallest(Key, BVal, B, B0) then
        ( if map.search(A, Key, AVal) then
            int.max(AVal, BVal, NewVal),
            map.det_update(Key, NewVal, A, A0)
        else
            map.det_insert(Key, BVal, A, A0)
        ),
        least_upper_bound(A0, B0, Out)
    else
        Out = A
    ).

%---------------------------------------------------------------------------%

is_submc_bag(SubBag, BigBag) :-
    mc_bag.subtract(SubBag, BigBag, SubBag0),
    mc_bag.is_empty(SubBag0).

%---------------------------------------------------------------------------%

is_empty(Bag) :-
    map.is_empty(Bag).

%---------------------------------------------------------------------------%

remove_smallest(Bag0, Item, Bag) :-
    map.remove_smallest(Item, Val, Bag0, Bag1),
    ( if Val > 1 then
        NewVal = Val - 1,
        map.det_insert(Item, NewVal, Bag1, Bag)
    else
        Bag = Bag1
    ).

    % compares the two mc_bags, and returns whether the first mc_bag is a
    % subset (<), is equal (=), or is a superset (>) of the second
    % subset_compare(<, {apple, orange}, {apple, apple, orange}).
    % subset_compare(=, {apple, orange}, {apple, orange}).
    % subset_compare(>, {apple, apple, orange}, {apple, orange}).
    % subset_compare(_, {apple, apple}, {orange, orange}) :- fail.
    % :- pred subset_compare(comparison_result, mc_bag(T), mc_bag(T)).
    % :- mode subset_compare(out, in, in) is semidet.
    %
subset_compare(Res, A, B) :-
    ( if map.remove_smallest(Key, AVal, A, A0) then
        ( if map.remove(Key, BVal, B, B0) then
            compare(ValRes, AVal, BVal),
            (
                ValRes = (>),
                is_submc_bag(B0, A0),
                Res = (>)
            ;
                ValRes = (=),
                subset_compare(Res, A0, B0)
            ;
                ValRes = (<),
                is_submc_bag(A0, B0),
                Res = (<)
            )
        else
            % B is empty, but A is not
            Res = (>)
        )
    else
        % A is empty
        ( if map.is_empty(B) then
            Res = (=)
        else
            Res = (<)
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
%   Function forms added.

init = B :-
    mc_bag.init(B).

insert(B1, X) = B2 :-
    mc_bag.insert(B1, X, B2).

insert_list(B1, Xs) = B2 :-
    mc_bag.insert_list(B1, Xs, B2).

insert_set(B1, Xs) = B2 :-
    mc_bag.insert_set(B1, Xs, B2).

from_list(Xs) = B :-
    mc_bag.from_list(Xs, B).

from_set(Xs) = B :-
    mc_bag.from_set(Xs, B).

to_list(B) = Xs :-
    mc_bag.to_list(B, Xs).

to_assoc_list(B) = AL :-
    mc_bag.to_assoc_list(B, AL).

to_list_without_duplicates(B) = Xs :-
    mc_bag.to_list_without_duplicates(B, Xs).

to_set_without_duplicates(B) = Xs :-
    mc_bag.to_set_without_duplicates(B, Xs).

det_remove(B1, X) = B2 :-
    mc_bag.det_remove(B1, X, B2).

det_remove_list(B1, Xs) = B2 :-
    mc_bag.det_remove_list(B1, Xs, B2).

det_remove_set(B1, Xs) = B2 :-
    mc_bag.det_remove_set(B1, Xs, B2).

delete(B1, X) = B2 :-
    mc_bag.delete(B1, X, B2).

delete_all(B1, X) = B2 :-
    mc_bag.delete_all(B1, X, B2).

count_value(B, X) = N :-
    mc_bag.count_value(B, X, N).

subtract(B1, B2) = B3 :-
    mc_bag.subtract(B1, B2, B3).

union(B1, B2) = B3 :-
    mc_bag.union(B1, B2, B3).

intersect(B1, B2) = B3 :-
    mc_bag.intersect(B1, B2, B3).

least_upper_bound(B1, B2) = B3 :-
    mc_bag.least_upper_bound(B1, B2, B3).

mc_bag(Xs) = mc_bag.from_list(Xs).

to_set(B) = mc_bag.to_set_without_duplicates(B).
