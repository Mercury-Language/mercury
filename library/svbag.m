%---------------------------------------------------------------------------%
% Copyright (C) 2004-2005 The University of Melbourne.
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
:- pred svbag__insert(T::in, bag(T)::in, bag(T)::out) is det.

	% Insert a list of values into a bag.
	%
:- pred svbag__insert_list(list(T)::in, bag(T)::in, bag(T)::out) is det.

	% Insert a set of values into a bag.
	%
:- pred svbag__insert_set(set(T)::in, bag(T)::in, bag(T)::out) is det.

	% Remove one occurrence of a particular value from a bag.
	% Fail if the item does not exist in the bag.
	%
:- pred svbag__remove(T::in, bag(T)::in, bag(T)::out) is semidet.

	% Remove one occurrence of a particular value from a bag.
	% Abort if the item does not exist in the bag.
	%
:- pred svbag__det_remove(T::in, bag(T)::in, bag(T)::out) is det.

	% Remove a list of values from a bag.  Duplicates are removed
	% from the bag the appropriate number of times.  Fail if any
	% of the items in the list do not exist in the bag.
	%
	% This call is logically equivalent to:
	%
	%	svbag__remove_list(RemoveList, Bag0, Bag) :-
	%		bag__from_list(RemoveList, RemoveBag),
	%		bag__is_subbag(RemoveBag, Bag0),
	%		svbag__subtract(RemoveBag, Bag0, Bag).
	%
:- pred svbag__remove_list(list(T)::in, bag(T)::in, bag(T)::out) is semidet.

	% Remove a list of values from a bag.  Duplicates are removed
	% from the bag the appropriate number of times.  Abort if any
	% of the items in the list do not exist in the bag.
	%
:- pred svbag__det_remove_list(list(T)::in, bag(T)::in, bag(T)::out) is det.

	% Remove a set of values from a bag. Each value is removed once.
	% Fail if any of the items in the set do not exist in the bag.
	% 
:- pred svbag__remove_set(set(T)::in, bag(T)::in, bag(T)::out) is semidet.

	% Remove a set of values from a bag. Each value is removed once.
	% Abort if any of the items in the set do not exist in the bag.
	% 
:- pred svbag__det_remove_set(set(T)::in, bag(T)::in, bag(T)::out) is det.

	% Delete one occurrence of a particular value from a bag.
	% If the key is not present, leave the bag unchanged.
	%
:- pred svbag__delete(T::in, bag(T)::in, bag(T)::out) is det.

	% Remove all occurrences of a particular value from a bag.
	% Fail if the item does not exist in the bag.
	%
:- pred svbag__remove_all(T::in, bag(T)::in, bag(T)::out) is semidet.

	% Delete all occurrences of a particular value from a bag.
	%
:- pred svbag__delete_all(T::in, bag(T)::in, bag(T)::out) is det.

	% svbag__subtract(Bag0, SubBag, Bag)
	% subtracts SubBag from Bag0 to produce Bag
	% each element in SubBag is removed from Bag0 to produce Bag.
	% If an element exists in SubBag, but not in Bag, then that
	% element is not removed.
	% e.g. svbag__subtract({1, 1, 2, 2, 3 }, {1, 1, 2, 3, 3, 3}, {2}).
	%
:- pred svbag__subtract(bag(T)::in, bag(T)::in, bag(T)::out) is det.

	% Fails if the bag is empty.
	%
:- pred svbag__remove_smallest(T::out, bag(T)::in, bag(T)::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

svbag__insert(Item, Bag0, Bag) :-
	bag__insert(Bag0, Item, Bag).

svbag__insert_list(List, Bag0, Bag) :-
	bag__insert_list(Bag0, List, Bag).

svbag__insert_set(Set, Bag0, Bag) :-
	bag__insert_set(Bag0, Set, Bag).

svbag__remove(Item, Bag0, Bag) :-
	bag__remove(Bag0, Item, Bag).

svbag__det_remove(Item, Bag0, Bag) :-
	bag__det_remove(Bag0, Item, Bag).

svbag__remove_list(List, Bag0, Bag) :-
	bag__remove_list(Bag0, List, Bag).

svbag__det_remove_list(List, Bag0, Bag) :-
	bag__det_remove_list(Bag0, List, Bag).

svbag__remove_set(Set, Bag0, Bag) :-
	bag__remove_set(Bag0, Set, Bag).

svbag__det_remove_set(Set, Bag0, Bag) :-
	bag__det_remove_set(Bag0, Set, Bag).

svbag__delete(Item, Bag0, Bag) :-
	bag__delete(Bag0, Item, Bag).

svbag__remove_all(Item, Bag0, Bag) :-
	bag__remove_all(Bag0, Item, Bag).

svbag__delete_all(Item, Bag0, Bag) :-
	bag__delete_all(Bag0, Item, Bag).

svbag__subtract(SubBag, Bag0, Bag) :-
	bag__subtract(Bag0, SubBag, Bag).

svbag__remove_smallest(Item, Bag0, Bag) :-
	bag__remove_smallest(Bag0, Item, Bag).

%---------------------------------------------------------------------------%
