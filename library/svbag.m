%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: svbag.m
% 
% This file provides an interface to the 'bag' ADT that is conducive to the
% use of state variable notation. The predicates here do the same thing as
% their counterparts in the bag module; the only difference is the order of the
% arguments.
% 
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module svbag.
:- interface.

:- import_module bag.
:- import_module list.
:- import_module set.

    % Insert a particular value in a bag.
    %
:- pred svbag.insert(T::in, bag(T)::in, bag(T)::out) is det.

    % Insert a list of values into a bag.
    %
:- pred svbag.insert_list(list(T)::in, bag(T)::in, bag(T)::out) is det.

    % Insert a set of values into a bag.
    %
:- pred svbag.insert_set(set(T)::in, bag(T)::in, bag(T)::out) is det.

    % Remove one occurrence of a particular value from a bag.
    % Fail if the item does not exist in the bag.
    %
:- pred svbag.remove(T::in, bag(T)::in, bag(T)::out) is semidet.

    % Remove one occurrence of a particular value from a bag.
    % Abort if the item does not exist in the bag.
    %
:- pred svbag.det_remove(T::in, bag(T)::in, bag(T)::out) is det.

    % Remove a list of values from a bag.  Duplicates are removed
    % from the bag the appropriate number of times.  Fail if any
    % of the items in the list do not exist in the bag.
    %
    % This call is logically equivalent to:
    %
    %   svbag.remove_list(RemoveList, Bag0, Bag) :-
    %       bag.from_list(RemoveList, RemoveBag),
    %       bag.is_subbag(RemoveBag, Bag0),
    %       svbag.subtract(RemoveBag, Bag0, Bag).
    %
:- pred svbag.remove_list(list(T)::in, bag(T)::in, bag(T)::out) is semidet.

    % Remove a list of values from a bag.  Duplicates are removed
    % from the bag the appropriate number of times.  Abort if any
    % of the items in the list do not exist in the bag.
    %
:- pred svbag.det_remove_list(list(T)::in, bag(T)::in, bag(T)::out) is det.

    % Remove a set of values from a bag. Each value is removed once.
    % Fail if any of the items in the set do not exist in the bag.
    %
:- pred svbag.remove_set(set(T)::in, bag(T)::in, bag(T)::out) is semidet.

    % Remove a set of values from a bag. Each value is removed once.
    % Abort if any of the items in the set do not exist in the bag.
    %
:- pred svbag.det_remove_set(set(T)::in, bag(T)::in, bag(T)::out) is det.

    % Delete one occurrence of a particular value from a bag.
    % If the key is not present, leave the bag unchanged.
    %
:- pred svbag.delete(T::in, bag(T)::in, bag(T)::out) is det.

    % Remove all occurrences of a particular value from a bag.
    % Fail if the item does not exist in the bag.
    %
:- pred svbag.remove_all(T::in, bag(T)::in, bag(T)::out) is semidet.

    % Delete all occurrences of a particular value from a bag.
    %
:- pred svbag.delete_all(T::in, bag(T)::in, bag(T)::out) is det.

    % svbag.subtract(Bag0, SubBag, Bag):
    %
    % Subtracts SubBag from Bag0 to produce Bag.
    % Each element in SubBag is removed from Bag0 to produce Bag.
    % If an element exists in SubBag, but not in Bag, then that
    % element is not removed.
    % e.g. svbag.subtract({1, 1, 2, 2, 3 }, {1, 1, 2, 3, 3, 3}, {2}).
    %
:- pred svbag.subtract(bag(T)::in, bag(T)::in, bag(T)::out) is det.

    % Fails if the bag is empty.
    %
:- pred svbag.remove_smallest(T::out, bag(T)::in, bag(T)::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

svbag.insert(Item, Bag0, Bag) :-
    bag.insert(Bag0, Item, Bag).

svbag.insert_list(List, Bag0, Bag) :-
    bag.insert_list(Bag0, List, Bag).

svbag.insert_set(Set, Bag0, Bag) :-
    bag.insert_set(Bag0, Set, Bag).

svbag.remove(Item, Bag0, Bag) :-
    bag.remove(Bag0, Item, Bag).

svbag.det_remove(Item, Bag0, Bag) :-
    bag.det_remove(Bag0, Item, Bag).

svbag.remove_list(List, Bag0, Bag) :-
    bag.remove_list(Bag0, List, Bag).

svbag.det_remove_list(List, Bag0, Bag) :-
    bag.det_remove_list(Bag0, List, Bag).

svbag.remove_set(Set, Bag0, Bag) :-
    bag.remove_set(Bag0, Set, Bag).

svbag.det_remove_set(Set, Bag0, Bag) :-
    bag.det_remove_set(Bag0, Set, Bag).

svbag.delete(Item, Bag0, Bag) :-
    bag.delete(Bag0, Item, Bag).

svbag.remove_all(Item, Bag0, Bag) :-
    bag.remove_all(Bag0, Item, Bag).

svbag.delete_all(Item, Bag0, Bag) :-
    bag.delete_all(Bag0, Item, Bag).

svbag.subtract(SubBag, Bag0, Bag) :-
    bag.subtract(Bag0, SubBag, Bag).

svbag.remove_smallest(Item, Bag0, Bag) :-
    bag.remove_smallest(Bag0, Item, Bag).

%---------------------------------------------------------------------------%
