%------------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 1999-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%

% File: set_bbbtree.m.
% Main authors: benyi.
% Stability: low.

% set_bbbtree - implements sets using bounded balanced binary trees.

%------------------------------------------------------------------------------%

:- module set_bbbtree.

:- interface.

:- import_module bool, list.

:- type set_bbbtree(T).


	% `set_bbbtree__init(Set)' returns an initialized empty set.

:- pred set_bbbtree__init(set_bbbtree(T)).
:- mode set_bbbtree__init(uo) is det.

:- func set_bbbtree__init = set_bbbtree(T).


        % `set_bbbtree__empty(Set) is true iff `Set' is contains no elements.

:- pred set_bbbtree__empty(set_bbbtree(T)).
:- mode set_bbbtree__empty(in) is semidet.


	% `set_bbbtree__size(Set, Size)' is true iff `Size' is the cardinality
	% of `Set'.

:- pred set_bbbtree__size(set_bbbtree(T), int).
:- mode set_bbbtree__size(in, out) is det.


	% `set_bbbtree__member(X, Set)' is true iff `X' is a member of `Set'.
	% O(lg n) for (in, in) and O(1) for (out, in).

:- pred set_bbbtree__member(T, set_bbbtree(T)).
:- mode set_bbbtree__member(in, in) is semidet.
:- mode set_bbbtree__member(out, in) is nondet.


	% `set_bbbtree__is_member(X, Set, Result)' is true iff `X' is a member
	% of `Set'.

:- pred set_bbbtree__is_member(T, set_bbbtree(T), bool).
:- mode set_bbbtree__is_member(in, in, out) is det.

	% `set_bbbtree__contains(Set, X)' is true iff `X' is a member of `Set'.
	% O(lg n).

:- pred set_bbbtree__contains(set_bbbtree(T), T).
:- mode set_bbbtree__contains(in, in) is semidet.


	% `set_bbbtree__least(Set, X)' is true iff `X' is smaller than all
	% the other members of `Set'.

:- pred set_bbbtree__least(set_bbbtree(T), T).
:- mode set_bbbtree__least(in, out) is semidet.
:- mode set_bbbtree__least(in, in) is semidet.


	% `set_bbbtree__largest(Set, X)' is true iff `X' is larger than all
	% the other members of `Set'.

:- pred set_bbbtree__largest(set_bbbtree(T), T).
:- mode set_bbbtree__largest(in, out) is semidet.
:- mode set_bbbtree__largest(in, in) is semidet.


	% `set_bbbtree__singleton_set(Set, X)' is true iff `Set' is the set
	% containing just the single element `X'.

:- pred set_bbbtree__singleton_set(set_bbbtree(T), T).
:- mode set_bbbtree__singleton_set(uo, di) is det.
:- mode set_bbbtree__singleton_set(in, out) is semidet.
:- mode set_bbbtree__singleton_set(in, in) is semidet.
:- mode set_bbbtree__singleton_set(out, in) is det.

:- func set_bbbtree__make_singleton_set(T) = set_bbbtree(T).


	% `set_bbbtree__equal(SetA, SetB)' is true iff `SetA' and `SetB'
	% contain the same elements.

:- pred set_bbbtree__equal(set_bbbtree(T), set_bbbtree(T)).
:- mode set_bbbtree__equal(in, in) is semidet.


	% `set_bbbtree__insert(Set0, X, Set)' is true iff `Set' is the union of
	% `Set0' and the set containing only `X'.

:- pred set_bbbtree__insert(set_bbbtree(T), T, set_bbbtree(T)).
:- mode set_bbbtree__insert(di, di, uo) is det.
:- mode set_bbbtree__insert(in, in, out) is det.

:- func set_bbbtree__insert(set_bbbtree(T), T) = set_bbbtree(T).


	% `set_bbbtree__insert_list(Set0, Xs, Set)' is true iff `Set' is
	% the union of `Set0' and the set containing only the members of `Xs'.

:- pred set_bbbtree__insert_list(set_bbbtree(T), list(T), set_bbbtree(T)).
% :- mode set_bbbtree__insert_list(di, di, uo) is det.
:- mode set_bbbtree__insert_list(in, in, out) is det.

:- func set_bbbtree__insert_list(set_bbbtree(T), list(T)) = set_bbbtree(T).


	% `set_bbbtree__delete(Set0, X, Set)' is true iff `Set' is the relative
	% complement of `Set0' and the set containing only `X', i.e.
	% if `Set' is the set which contains all the elements of `Set0'
	% except `X'.

:- pred set_bbbtree__delete(set_bbbtree(T), T, set_bbbtree(T)).
:- mode set_bbbtree__delete(di, in, uo) is det.
:- mode set_bbbtree__delete(in, in, out) is det.

:- func set_bbbtree__delete(set_bbbtree(T), T) = set_bbbtree(T).


	% `set_bbbtree__delete_list(Set0, Xs, Set)' is true iff `Set' is the
	% relative complement of `Set0' and the set containing only the members
	% of `Xs'.

:- pred set_bbbtree__delete_list(set_bbbtree(T), list(T), set_bbbtree(T)).
% :- mode set_bbbtree__delete_list(di, in, uo) is det.
:- mode set_bbbtree__delete_list(in, in, out) is det.

:- func set_bbbtree__delete_list(set_bbbtree(T), list(T)) = set_bbbtree(T).


	% `set_bbbtree__remove(Set0, X, Set)' is true iff `Set0' contains `X',
	% and `Set' is the relative complement of `Set0' and the set
	% containing only `X', i.e.  if `Set' is the set which contains
	% all the elements of `Set0' except `X'.

:- pred set_bbbtree__remove(set_bbbtree(T), T, set_bbbtree(T)).
:- mode set_bbbtree__remove(in, in, out) is semidet.


	% `set_bbbtree__remove_list(Set0, Xs, Set)' is true iff Xs does not
	% contain any duplicates, `Set0' contains every member of `Xs',
	% and `Set' is the relative complement of `Set0' and the set
	% containing only the members of `Xs'.

:- pred set_bbbtree__remove_list(set_bbbtree(T), list(T), set_bbbtree(T)).
:- mode set_bbbtree__remove_list(in, in, out) is semidet.


	% `set_bbbtree__remove_least(Set0, X, Set)' is true iff the union if
	% `X' and `Set' is `Set0' and `X' is smaller than all the elements of
	% `Set'.

:- pred set_bbbtree__remove_least(set_bbbtree(T), T, set_bbbtree(T)).
:- mode set_bbbtree__remove_least(in, out, out) is semidet.


	% `set_bbbtree__remove_largest(Set0, X, Set)' is true iff the union if
	% `X' and `Set' is `Set0' and `X' is larger than all the elements of
	% `Set'.

:- pred set_bbbtree__remove_largest(set_bbbtree(T), T, set_bbbtree(T)).
:- mode set_bbbtree__remove_largest(in, out, out) is semidet.


	% `set_bbbtree__list_to_set(List, Set)' is true iff `Set' is the set
	% containing only the members of `List'. O(n lg n)

:- pred set_bbbtree__list_to_set(list(T), set_bbbtree(T)).
% :- mode set_bbbtree__list_to_set(di, uo) is det.
:- mode set_bbbtree__list_to_set(in, out) is det.

:- func set_bbbtree__list_to_set(list(T)) = set_bbbtree(T).


	% `set_bbbtree__sorted_list_to_set(List, Set)' is true iff `Set' is the
	% set containing only the members of `List'.
	% `List' must be sorted. O(n).

:- pred set_bbbtree__sorted_list_to_set(list(T), set_bbbtree(T)).
% :- mode set_bbbtree__sorted_list_to_set(di, uo) is det.
:- mode set_bbbtree__sorted_list_to_set(in, out) is det.

:- func set_bbbtree__sorted_list_to_set(list(T)) = set_bbbtree(T).


	% `set_bbbtree__sorted_list_to_set_len(List, Set, N)' is true iff
	% `Set' is the set set containing only the members of `List' and `N'
	% is the length of the list. If the length of the list is already known
	% then a noticable speed improvement can be expected over
	% `set_bbbtree__sorted_list_to_set' as a significant cost involved
	% with `set_bbbtree__sorted_list_to_set' is the call to list__length.
	% `List' must be sorted. O(n).

:- pred set_bbbtree__sorted_list_to_set_len(list(T), set_bbbtree(T), int).
% :- mode set_bbbtree__sorted_list_to_set_len(di, uo, in) is det.
:- mode set_bbbtree__sorted_list_to_set_len(in, out, in) is det.


	% `set_bbbtree__to_sorted_list(Set, List)' is true iff `List' is the
	% list of all the members of `Set', in sorted order. O(n).

:- pred set_bbbtree__to_sorted_list(set_bbbtree(T), list(T)).
:- mode set_bbbtree__to_sorted_list(di, uo) is det.
:- mode set_bbbtree__to_sorted_list(in, out) is det.

:- func set_bbbtree__to_sorted_list(set_bbbtree(T)) = list(T).


	% `set_bbbtree__union(SetA, SetB, Set)' is true iff `Set' is the union
	% of `SetA' and `SetB'.

:- pred set_bbbtree__union(set_bbbtree(T), set_bbbtree(T), set_bbbtree(T)).
:- mode set_bbbtree__union(in, in, out) is det.

:- func set_bbbtree__union(set_bbbtree(T), set_bbbtree(T)) = set_bbbtree(T).


	% `set_bbbtree__union_list(Sets) = Set' is true iff `Set' is the union
	% of all the sets in `Sets'

:- func set_bbbtree__union_list(list(set_bbbtree(T))) = set_bbbtree(T).

	% `set_bbbtree__power_union(Sets, Set)' is true iff `Set' is the union
	% of all the sets in `Sets'

:- pred set_bbbtree__power_union(set_bbbtree(set_bbbtree(T)), set_bbbtree(T)).
:- mode set_bbbtree__power_union(in, out) is det.

:- func set_bbbtree__power_union(set_bbbtree(set_bbbtree(T))) = set_bbbtree(T).


	% `set_bbbtree__intersect(SetA, SetB, Set)' is true iff `Set' is the
	% intersection of `SetA' and `SetB'.

:- pred set_bbbtree__intersect(set_bbbtree(T), set_bbbtree(T),
					set_bbbtree(T)).
:- mode set_bbbtree__intersect(in, in, out) is det.

:- func set_bbbtree__intersect(set_bbbtree(T), set_bbbtree(T)) = set_bbbtree(T).


	% `set_bbbtree__power_intersect(Sets, Set) is true iff `Set' is the
	% intersection of the sets in `Sets'.

:- pred set_bbbtree__power_intersect(set_bbbtree(set_bbbtree(T)),
					set_bbbtree(T)).
:- mode set_bbbtree__power_intersect(in, out) is det.

:- func set_bbbtree__power_intersect(set_bbbtree(set_bbbtree(T)))
		= set_bbbtree(T).

	% `set_bbbtree__intersect_list(Sets) = Set is true iff `Set' is the
	% intersection of the sets in `Sets'.

:- func set_bbbtree__intersect_list(list(set_bbbtree(T))) = set_bbbtree(T).

	% `set_bbtree__difference(SetA, SetB, Set)' is true iff `Set' is the
	%  set containing all the elements of `SetA' except those that
	% occur in `SetB'.

:- pred set_bbbtree__difference(set_bbbtree(T), set_bbbtree(T),
					set_bbbtree(T)).
:- mode set_bbbtree__difference(in, in, out) is det.

:- func set_bbbtree__difference(set_bbbtree(T), set_bbbtree(T))
		= set_bbbtree(T).


	% `set_bbbtree__subset(SetA, SetB)' is true iff all the elements of
	% `SetA' are also elements of `SetB'.

:- pred set_bbbtree__subset(set_bbbtree(T), set_bbbtree(T)).
:- mode set_bbbtree__subset(in, in) is semidet.


	% `set_bbbtree__superset(SetA, SetB)' is true iff all the elements of
	% `SetB' are also elements of `SetA'.

:- pred set_bbbtree__superset(set_bbbtree(T), set_bbbtree(T)).
:- mode set_bbbtree__superset(in, in) is semidet.

:- func set_bbbtree__map(func(T1) = T2, set_bbbtree(T1)) = set_bbbtree(T2).

:- func set_bbbtree__filter_map(func(T1) = T2, set_bbbtree(T1))
		= set_bbbtree(T2).

:- mode set_bbbtree__filter_map(func(in) = out is semidet, in) = out is det.

:- func set_bbbtree__fold(func(T1, T2) = T2, set_bbbtree(T1), T2) = T2.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module int, require.

% Implementation based on "Functional Pearls: Efficient sets - a balancing act"
% by Stephen Adams, J. Functional Programming 3 (4): 553-561, Oct 1993.
%
% Note:	set_bbbtree__concat4 and not set_bbbtree__concat3 represents the
%	function concat3 mentioned in the report.
%
% Predicates with the suffix `_r' in the names are predicates that accept an
% integer as an additional last argument. This integer is a ratio that is
% used to measure how much larger one tree is allowed to be compared to the
% other before some rotations are performed to rebalance them. These predicates
% are currently not exported but it maybe useful to do so to allow users to
% influence the rebalancing process.
%
% NOTE:
% The size of trees are measured in terms of number of elements and not height.
% Also the fact that ratio is an integer seems counter intuitive but it should
% be realized that this property is true at all levels of the tree. Hence the
% default ratio of 5 will not allow the two trees to differ in height by more
% that a few units.
%
% BUGS:
% Due to the presence of bugs in the compiler concerning destructive input and
% unique modes some predicates and modes are commented out. Once the bugs are
% removed the modes should be uncommented. Although some commented out
% predicates can simply be uncommented and their hacked versions deleted
% others will need to be rewritten. Three examples are the _r versions of
% union, intersection and difference. They all make the calls
% set_bbbtree__split_lt followed by set_bbbtree__split_gt with the right
% tree. If the right tree is declared destructive input then either the
% compiler must be smart enough to call set_bbbtree__split_lt
% non-destructively followed by the call to set_bbbtree__split_gt
% destructively or some rewriting must be done.
%
% IMPROVEMENTS:
% Speed improvements may be possible by the implementation of specific
% predicates for predicates such as `set_bbbtree__equal' as opposed to the
% use of other set operations.
%
%------------------------------------------------------------------------------%

:- type set_bbbtree(T) --->
				empty
			;	tree(T, int, set_bbbtree(T), set_bbbtree(T)).


	% `set_bbbtree__def_ratio(Ratio)' returns the ratio that is used in
	% deciding whether two trees require re-balancing.

:- pred set_bbbtree__def_ratio(int).
:- mode set_bbbtree__def_ratio(uo) is det.

set_bbbtree__def_ratio(5).

%------------------------------------------------------------------------------%

set_bbbtree__init(empty).

%------------------------------------------------------------------------------%

set_bbbtree__empty(empty).

%------------------------------------------------------------------------------%

set_bbbtree__size(empty, 0).
set_bbbtree__size(tree(_V, N, _L, _R), N).

%------------------------------------------------------------------------------%

% set_bbbtree__member(X, empty) :- fail.

set_bbbtree__member(X, tree(V, _N, L, R)) :-
	compare(Result, X, V),
	(
		Result = (<),
		set_bbbtree__member(X, L)	% search left subtree
	;
		Result = (>),
		set_bbbtree__member(X, R)	% search right subtree
	;
		Result = (=),
		X = V
	).

set_bbbtree__contains(Set, X) :-
	set_bbbtree__member(X, Set).

%------------------------------------------------------------------------------%

set_bbbtree__is_member(X, Set, Result) :-
	(
		set_bbbtree__member(X, Set)
	->
		Result = yes
	;
		Result = no
	).

%------------------------------------------------------------------------------%

% set_bbbtree__least(empty, _) :- fail.

set_bbbtree__least(tree(V, _N, L, _R), X) :-
	(
			% found least element
		L = empty,
		X = V
	;
			% search further in left subtree
		L = tree(_V0, _N0, _L0, _R0),
		set_bbbtree__least(L, X)
	).

%------------------------------------------------------------------------------%

% set_bbbtree__largest(empty, _) :- fail.

set_bbbtree__largest(tree(V, _N, _L, R), X) :-
	(
			% found largest element
		R = empty,
		X = V
	;
			% search further in right subtree
		R = tree(_V0, _N0, _L0, _R0),
		set_bbbtree__largest(R, X)
	).

%------------------------------------------------------------------------------%

set_bbbtree__singleton_set(tree(V, 1, empty, empty), V).

%------------------------------------------------------------------------------%

set_bbbtree__equal(SetA, SetB) :-
	set_bbbtree__subset(SetA, SetB),
	set_bbbtree__subset(SetB, SetA).

%------------------------------------------------------------------------------%

% This is a hack to handle the bugs with unique and destructive input modes.
set_bbbtree__insert(Set0, X, Set) :-
	set_bbbtree__def_ratio(Ratio),
	set_bbbtree__insert_r(Set0, X, Set1, Ratio),
	unsafe_promise_unique(Set1, Set).

/* Uncomment this once destructive input and unique modes are fixed and detele
   the one above.
set_bbbtree__insert(Set0, X, Set) :-
	set_bbbtree__def_ratio(Ratio),
	set_bbbtree__insert_r(Set0, X, Set, Ratio).
*/

:- pred set_bbbtree__insert_r(set_bbbtree(T), T, set_bbbtree(T), int).
% :- mode set_bbbtree__insert_r(di, di, uo, in) is det.
:- mode set_bbbtree__insert_r(in, in, out, in) is det.

	% X was not in the set so make new node with X as the value
set_bbbtree__insert_r(empty, X, tree(X, 1, empty, empty), _Ratio).

set_bbbtree__insert_r(tree(V, N, L, R), X, Set, Ratio) :-
	compare(Result, X, V),
	(
			% insert X into left subtree and re-balance it
		Result = (<),
		set_bbbtree__insert_r(L, X, NewL, Ratio),
		set_bbbtree__balance(V, NewL, R, Set, Ratio)
	;
			% insert X into right subtree and re-balance it
		Result = (>),
		set_bbbtree__insert_r(R, X, NewR, Ratio),
		set_bbbtree__balance(V, L, NewR, Set, Ratio)
	;
			% X already in tree
		Result = (=),
		Set = tree(V, N, L, R)
	).

%------------------------------------------------------------------------------%

set_bbbtree__insert_list(Set0, List, Set) :-
	set_bbbtree__def_ratio(Ratio),
	set_bbbtree__insert_list_r(Set0, List, Set, Ratio).

:- pred set_bbbtree__insert_list_r(set_bbbtree(T), list(T), set_bbbtree(T),int).
% :- mode set_bbbtree__insert_list_r(di, di, uo, in) is det.
:- mode set_bbbtree__insert_list_r(in, in, out, in) is det.

set_bbbtree__insert_list_r(Set, [], Set, _Ratio).

set_bbbtree__insert_list_r(Set0, [X | Xs], Set, Ratio) :-
	set_bbbtree__insert_r(Set0, X, Set1, Ratio),
	set_bbbtree__insert_list_r(Set1, Xs, Set, Ratio).

%------------------------------------------------------------------------------%

/*
set_bbbtree__delete(empty, _X, empty).
set_bbbtree__delete(tree(V, N, L, R), X, Set) :-
	compare(Result, X, V),
	(
			% delete X from left subtree
		Result = (<),
		set_bbbtree__delete(L, X, NewL), % X in left tree
		NewN is N - 1,
		Set = tree(V, NewN, NewL, R)
	;
			% delete X from right subtree
		Result = (>),
		set_bbbtree__delete(R, X, NewR), % X in right tree
		NewN is N - 1,
		Set = tree(V, NewN, L, NewR)
	;
			% found X so just concatenate its two subtrees together
		Result = (=),
		set_bbbtree__concat3(L, R, Set)
	).
*/

/*
set_bbbtree__delete(Set0, X, Set) :-
	(
		set_bbbtree__remove(Set0, X, Set1)
	->
		Set = Set1
	;
		Set = Set0
	).
*/

set_bbbtree__delete(Set0, X, Set) :-
	(
		set_bbbtree__remove(Set0, X, Set1)
	->
		Set2 = Set1
	;
		Set2 = Set0
	),
	unsafe_promise_unique(Set2, Set).

%------------------------------------------------------------------------------%

set_bbbtree__delete_list(Set, [], Set).

set_bbbtree__delete_list(Set0, [X | Xs], Set) :-
	set_bbbtree__delete(Set0, X, Set1),
	set_bbbtree__delete_list(Set1, Xs, Set).

%------------------------------------------------------------------------------%

% set_bbbtree__remove(empty, X, _):- fail.

set_bbbtree__remove(tree(V, N, L, R), X, Set) :-
	compare(Result, X, V),
	(
			% remove X from left subtree
		Result = (<),
		set_bbbtree__remove(L, X, NewL), % X in left tree
		NewN is N - 1,
		Set = tree(V, NewN, NewL, R)
	;
			% remove X from right subtree
		Result = (>),
		set_bbbtree__remove(R, X, NewR), % X in right tree
		NewN is N - 1,
		Set = tree(V, NewN, L, NewR)
	;
			% found X so just concatenate its two subtrees together
		Result = (=),
		set_bbbtree__concat3(L, R, Set)
	).

%------------------------------------------------------------------------------%

set_bbbtree__remove_list(Set, [], Set).

set_bbbtree__remove_list(Set0, [X | Xs], Set) :-
	set_bbbtree__remove(Set0, X, Set1),
	set_bbbtree__remove_list(Set1, Xs, Set).

%------------------------------------------------------------------------------%

% The tree is not rebalanced as the removal of one element will not cause the
% tree to become much more unbalanced.

% set_bbbtree__remove_least(empty, X, _) :- fail.

set_bbbtree__remove_least(tree(V, N, L, R), X, Set) :-
	(
			% found the least element
		L = empty,
		X = V,
		Set = R
	;
			% search further in the left subtree
		L = tree(_V, _N, _L, _R),
		set_bbbtree__remove_least(L, X, NewL),
		NewN is N - 1,
		Set = tree(V, NewN, NewL, R)
	).

%------------------------------------------------------------------------------%

% The tree is not rebalanced as the removal of one element will not cause the
% tree to become much more unbalanced.

% set_bbbtree__remove_largest(empty, X, _) :- fail.

set_bbbtree__remove_largest(tree(V, N, L, R), X, Set) :-
	(
			% found the largest element
		R = empty,
		X = V,
		Set = L
	;
			% search further in the right subtree
		R = tree(_V, _N, _L, _R),
		set_bbbtree__remove_largest(R, X, NewR),
		NewN is N - 1,
		Set = tree(V, NewN, L, NewR)
	).

%------------------------------------------------------------------------------%

set_bbbtree__list_to_set(List, Set) :-
	set_bbbtree__def_ratio(Ratio),
	set_bbbtree__list_to_set_r(List, Set, Ratio).

:- pred set_bbbtree__list_to_set_r(list(T), set_bbbtree(T), int).
% :- mode set_bbbtree__list_to_set_r(di, uo, in) is det.
:- mode set_bbbtree__list_to_set_r(in, out, in) is det.

set_bbbtree__list_to_set_r(List, Set, Ratio) :-
	set_bbbtree__init(InitSet),
	set_bbbtree__insert_list_r(InitSet, List, Set, Ratio).

%------------------------------------------------------------------------------%

% The tree is created by first determining it's length. All lists of length
% N have the same structure. The root of of the tree is the N // 2 element
% of the list. Elements 1 to N // 2 - 1 make up the left subtree and elements
% N // 2 + 1 to N make up the right subtree. The structure, which is known
% due to the length of the list, is just created inorder while passing the
% list around in an accumulator and taking off elements as needed.
%
% The predicate set_bbbtree__sorted_list_to_set_len2 has been unfolded up to
% trees of size 3 so as to avoid recursive calls all the way to the leaves.
% This alone approximately halves execution time and stack usage.
% Unfolding further becomes a case of diminishing returns.
%
% Cases N = 3, N = 2 and N = 1 could safely be removed without change to
% computed results as long as the 3 in N > 3 is adjusted appropriately.

set_bbbtree__sorted_list_to_set(List, Set) :-
	list__length(List, N),
	set_bbbtree__sorted_list_to_set_len(List, Set, N).

set_bbbtree__sorted_list_to_set_len([], empty, _N).
set_bbbtree__sorted_list_to_set_len([ X | Xs ], Set, N) :-
	set_bbbtree__sorted_list_to_set_len2([ X | Xs ], RestOfList, N, Set),
	(
		RestOfList = []	    % The list should be exhaused on completion
	->
		true
	;
			% Should never happen. Here only to satify det checker
		error("set_bbbtree__sorted_list_to_set_r")
	).
	
:- pred set_bbbtree__sorted_list_to_set_len2(list(T), list(T), int,
								set_bbbtree(T)).
% :- mode set_bbbtree__sorted_list_to_set_len2(di, uo, in, uo) is det.
:- mode set_bbbtree__sorted_list_to_set_len2(in, out, in, out) is det.

set_bbbtree__sorted_list_to_set_len2(List, RestOfList, N, Set) :-
	(
		N > 3
	->
		NL is N//2,
		NR is N - NL - 1,
		set_bbbtree__sorted_list_to_set_len2(List, RestOfList0, NL, L),
		(
			RestOfList0 = [V | RestOfList1],
			set_bbbtree__sorted_list_to_set_len2(RestOfList1,
							RestOfList, NR, R),
			Set = tree(V, N, L, R)
		;
				% Should never occur.
				%Here only to satisfy det checker
			RestOfList0 = [],
			error("set_bbbtree__sorted_list_to_set_len2.1")
		)
	;
		N = 3
	->
		(
			List = [ X, Y, Z | RestOfList0 ]
		->
			RestOfList = RestOfList0,
			Set = tree(Y, N, tree(X, 1, empty, empty),
						tree(Z, 1, empty, empty))
		;
			% Should never occur.Here only to satisfy det checker
			error("set_bbbtree__sorted_list_to_set_len2.2")
		)
	;
		N = 2
	->
		(
			List = [ X, Y | RestOfList0 ]
		->
			RestOfList = RestOfList0,
			Set = tree(Y, N, tree(X, 1, empty, empty), empty)
		;
			% Should never occur. Here only to satisfy det checker
			error("set_bbbtree__sorted_list_to_set_len2.3")
		)
	;
		N = 1
	->
		(
			List = [ X | RestOfList0 ]
		->
			RestOfList = RestOfList0,
			Set = tree(X, N, empty, empty)
		;
			% Should never occur. Here only to satisfy det checker
			error("set_bbbtree__sorted_list_to_set_len2.4")
		)
	;
			% N = 0.
		RestOfList = List,
		Set = empty
	).

%------------------------------------------------------------------------------%

	% Flatten the tree by an accumulator based tree traversal
	% traversing the tree in a right-to-left post order manner. O(n).

set_bbbtree__to_sorted_list(Set, List) :-
	set_bbbtree__to_sorted_list2(Set, [], List).

:- pred set_bbbtree__to_sorted_list2(set_bbbtree(T), list(T), list(T)).
:- mode set_bbbtree__to_sorted_list2(di, di, uo) is det.
:- mode set_bbbtree__to_sorted_list2(in, in, out) is det.

set_bbbtree__to_sorted_list2(empty, List, List).

set_bbbtree__to_sorted_list2(tree(V, _N, L, R), Acc, List) :-
	set_bbbtree__to_sorted_list2(R, Acc, List0),
	set_bbbtree__to_sorted_list2(L, [V|List0], List).

%------------------------------------------------------------------------------%

% elem(x, A)  implies
%
% A union B =
%                   ( { elem(x, A) | x < a } union { elem(x, B) | x < a } )
%             union ( { a } )
%             union ( { elem(x, A) | x > a } union { elem(x, B) | x > a } )

set_bbbtree__union(SetA, SetB, Set) :-
	set_bbbtree__def_ratio(Ratio),
	set_bbbtree__union_r(SetA, SetB, Set, Ratio).

:- pred set_bbbtree__union_r(set_bbbtree(T),set_bbbtree(T),set_bbbtree(T),int).
% :- mode set_bbbtree__union_r(di, di, uo, in) is det.
:- mode set_bbbtree__union_r(in, in, out, in) is det.

set_bbbtree__union_r(empty, Set, Set, _Ratio).

set_bbbtree__union_r(tree(V, _N, LL, LR), R, Set, Ratio) :-
	set_bbbtree__split_lt(R, V, NewRL, Ratio),
	set_bbbtree__split_gt(R, V, NewRR, Ratio),
	set_bbbtree__union_r(LL, NewRL, LSet, Ratio),
	set_bbbtree__union_r(LR, NewRR, RSet, Ratio),
	set_bbbtree__concat4(LSet, RSet, V, Set, Ratio).

%------------------------------------------------------------------------------%

set_bbbtree__union_list(ListofSets) = 
	list__foldl(set_bbbtree__union, ListofSets, set_bbbtree__init).

%------------------------------------------------------------------------------%

% `set_bbbtree__power_union' is a divide and conquer algorithm. The power union
% of the two subtrees is determined and then unioned. Then the root set is
% unioned into the result. The choice to perform the power union of the two
% subtrees is due to the computational expense of union being proportional
% to the difference in height of the two subtrees. This way the two power
% union calls will likely create trees of similar sizes, and hence their
% union will be inexpensive. Unfortunately as the union grows it will
% increasing dwarf the tree that is the root node and hence this cost will
% increase, but this cannot be avoided.
 
set_bbbtree__power_union(Sets, Set) :-
	set_bbbtree__def_ratio(Ratio),
	set_bbbtree__power_union_r(Sets, Set, Ratio).

:- pred set_bbbtree__power_union_r(set_bbbtree(set_bbbtree(T)),
							set_bbbtree(T), int).
:- mode set_bbbtree__power_union_r(in, out, in) is det.

set_bbbtree__power_union_r(empty, empty, _Ratio).

set_bbbtree__power_union_r(tree(V, _N, L, R), Set, Ratio) :-
	set_bbbtree__power_union_r(L, LUnion, Ratio),
	set_bbbtree__power_union_r(R, RUnion, Ratio),
	set_bbbtree__union_r(LUnion, RUnion, Union, Ratio),
	set_bbbtree__union_r(V, Union, Set, Ratio).

%------------------------------------------------------------------------------%

% elem(x, A) and elem(x, B) implies
%
% A intersection B =
%                   ( { elem(x, A) | x < a } intersect { elem(x, B) | x < a } )
%             union ( { a } )
%             union ( { elem(x, A) | x > a } intersect { elem(x, B) | x > a } )

set_bbbtree__intersect(SetA, SetB, Set) :-
	set_bbbtree__def_ratio(Ratio),
	set_bbbtree__intersect_r(SetA, SetB, Set, Ratio).

:- pred set_bbbtree__intersect_r(set_bbbtree(T), set_bbbtree(T),
							set_bbbtree(T), int).
% :- mode set_bbbtree__intersect_r(di, di, uo, in) is det.
:- mode set_bbbtree__intersect_r(in, in, out, in) is det.

set_bbbtree__intersect_r(empty, _Set, empty, _Ratio).

set_bbbtree__intersect_r(tree(V, _N, LL, LR), R, Set, Ratio) :-
	set_bbbtree__split_lt(R, V, NewRL, Ratio),
	set_bbbtree__split_gt(R, V, NewRR, Ratio),
	set_bbbtree__intersect_r(LL, NewRL, LSet, Ratio),
	set_bbbtree__intersect_r(LR, NewRR, RSet, Ratio),
	(
		set_bbbtree__member(V, R)
	->
		set_bbbtree__concat4(LSet, RSet, V, Set, Ratio)
	;
		set_bbbtree__concat3(LSet, RSet, Set)
	).

%------------------------------------------------------------------------------%

% `set_bbbtree__power_intersect' is an accumulator based algorithm. Initially
% the accumulator is seeded with the tree at the root. Then the tree is travesed
% inorder and each tree at each node is respectively intersected with the
% accumulator. The aim of the algorithm is to rapidly reduce the size of the
% accumulator, possibly of the empty set in which case the call immediately
% returns.

set_bbbtree__power_intersect(Sets, Set) :-
	set_bbbtree__def_ratio(Ratio),
	set_bbbtree__power_intersect_r(Sets, Set, Ratio).

:- pred set_bbbtree__power_intersect_r(set_bbbtree(set_bbbtree(T)),
							set_bbbtree(T), int).
:- mode set_bbbtree__power_intersect_r(in, out, in) is det.

set_bbbtree__power_intersect_r(empty, empty, _Ratio).

set_bbbtree__power_intersect_r(tree(V, _N, L, R), Set, Ratio) :-
	set_bbbtree__power_intersect_r2(L, V, Intersection0, Ratio),
	set_bbbtree__power_intersect_r2(R, Intersection0, Set, Ratio).

:- pred set_bbbtree__power_intersect_r2(set_bbbtree(set_bbbtree(T)),
					set_bbbtree(T), set_bbbtree(T), int).
:- mode set_bbbtree__power_intersect_r2(in, in, out, in) is det.

set_bbbtree__power_intersect_r2(empty, empty, empty, _Ratio).

set_bbbtree__power_intersect_r2(empty, tree(_V, _N, _L, _R), empty, _Ratio).

set_bbbtree__power_intersect_r2(tree(_V, _N, _L, _R), empty, empty, _Ratio).

set_bbbtree__power_intersect_r2(tree(V, _N, L, R), tree(AccV, AccN, AccL, AccR),
								Set, Ratio) :-
	set_bbbtree__intersect_r(V, tree(AccV, AccN, AccL, AccR),
							Intersection0, Ratio),
	set_bbbtree__power_intersect_r2(L, Intersection0, Intersection1, Ratio),
	set_bbbtree__power_intersect_r2(R, Intersection1, Set, Ratio).

set_bbbtree__intersect_list([]) = set_bbbtree__init.
set_bbbtree__intersect_list([Set | Sets]) =
		set_bbbtree__intersect_list_r(Set, Sets, Ratio) :-
	set_bbbtree__def_ratio(Ratio).

:- func set_bbbtree__intersect_list_r(set_bbbtree(T), list(set_bbbtree(T)),
	int) = set_bbbtree(T).

set_bbbtree__intersect_list_r(Intersect, [], _Ratio) = Intersect.
set_bbbtree__intersect_list_r(Intersect0, [Set | Sets], Ratio) = 
		set_bbbtree__intersect_list_r(Intersect1, Sets, Ratio) :-
	set_bbbtree__intersect_r(Intersect0, Set, Intersect1, Ratio).

%------------------------------------------------------------------------------%

% elem(x, A) and not elem(x, B) implies
%
% A difference B =
%                   ( { elem(x, A) | x < a } difference { elem(x, B) | x < a } )
%             union ( { a } )
%             union ( { elem(x, A) | x > a } difference { elem(x, B) | x > a } )

set_bbbtree__difference(SetA, SetB, Set) :-
	set_bbbtree__def_ratio(Ratio),
	set_bbbtree__difference_r(SetA, SetB, Set, Ratio).

:- pred set_bbbtree__difference_r(set_bbbtree(T), set_bbbtree(T),
							set_bbbtree(T), int).
% :- mode set_bbbtree__difference_r(di, di, uo, in) is det.
:- mode set_bbbtree__difference_r(in, in, out, in) is det.

set_bbbtree__difference_r(empty, _Set, empty, _Ratio).

set_bbbtree__difference_r(tree(V, _N, LL, LR), R, Set, Ratio) :-
	set_bbbtree__split_lt(R, V, NewRL, Ratio),
	set_bbbtree__split_gt(R, V, NewRR, Ratio),
	set_bbbtree__difference_r(LL, NewRL, LSet, Ratio),
	set_bbbtree__difference_r(LR, NewRR, RSet, Ratio),
	(
		set_bbbtree__member(V, R)
	->
		set_bbbtree__concat3(LSet, RSet, Set)
	;
		set_bbbtree__concat4(LSet, RSet, V, Set, Ratio)
	).

%------------------------------------------------------------------------------%

set_bbbtree__subset(SetA, SetB) :-
	set_bbbtree__difference(SetA, SetB, Set),
	set_bbbtree__empty(Set).

%------------------------------------------------------------------------------%

set_bbbtree__superset(SetA, SetB) :-
	set_bbbtree__subset(SetB, SetA).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

	% given X, L and R create a new tree who's root is X,
	% left subtree is L and right subtree is R.

:- pred set_bbbtree__build_node(T, set_bbbtree(T), set_bbbtree(T),
								set_bbbtree(T)).
:- mode set_bbbtree__build_node(di, di, di, uo) is det.
:- mode set_bbbtree__build_node(in, in, in, out) is det.

set_bbbtree__build_node(X, L, R, Tree) :-
	set_bbbtree__size(L, LSize),
	set_bbbtree__size(R, RSize),
	N is 1 + LSize + RSize,
	Tree0 = tree(X, N, L, R),
	unsafe_promise_unique(Tree0, Tree).

%------------------------------------------------------------------------------%

	% Single rotation to the left.

	%     A                        B
	%   /   \                    /   \
	%  X     B      ----->      A     Z
	%      /   \              /   \
	%     Y     Z            X     Y

:- pred set_bbbtree__single_rot_l(T, set_bbbtree(T), set_bbbtree(T),
								set_bbbtree(T)).
:- mode set_bbbtree__single_rot_l(di, di, di, uo) is det.
:- mode set_bbbtree__single_rot_l(in, in, in, out) is det.

set_bbbtree__single_rot_l(_A, _X, empty, _Set) :-  % Should never occur. Here
	error("set_bbbtree__single_rot_l").	   % only to satisfy det checker

set_bbbtree__single_rot_l(A, X, tree(B, _N, Y, Z), Set) :-
	set_bbbtree__build_node(A, X, Y, A_X_and_Y),
	set_bbbtree__build_node(B, A_X_and_Y, Z, Set).

%------------------------------------------------------------------------------%

	% Single rotation to the right.

:- pred set_bbbtree__single_rot_r(T, set_bbbtree(T), set_bbbtree(T),
								set_bbbtree(T)).
:- mode set_bbbtree__single_rot_r(di, di, di, uo) is det.
:- mode set_bbbtree__single_rot_r(in, in, in, out) is det.

set_bbbtree__single_rot_r(_B, empty, _Z, _Set) :-  % Should never occur. Here
	error("set_bbbtree__single_rot_r").	   % only to satisfy det checker

set_bbbtree__single_rot_r(B, tree(A, _N, X, Y), Z, Set) :-
	set_bbbtree__build_node(B, Y, Z, B_Y_and_Z),
	set_bbbtree__build_node(A, X, B_Y_and_Z, Set).

%------------------------------------------------------------------------------%

	% Double rotation to the left.

	%        A
	%      /   \                         B
	%     X     C                      /   \
	%         /   \     ----->       A       C
	%        B     Z               /   \   /   \
	%      /   \                  X    Y1 Y2    Z
	%    Y1     Y2

:- pred set_bbbtree__double_rot_l(T, set_bbbtree(T), set_bbbtree(T),
								set_bbbtree(T)).
:- mode set_bbbtree__double_rot_l(di, di, di, uo) is det.
:- mode set_bbbtree__double_rot_l(in, in, in, out) is det.

set_bbbtree__double_rot_l(_A, _X, empty, _Set) :-  % Should never occur. Here
	error("set_bbbtree__double_rot_l.1").	   % only to satisfy det checker

set_bbbtree__double_rot_l(A, X, tree(C, _N0, Y, Z), Set) :-
	(
		Y = tree(B, _N1, Y1, Y2),
		set_bbbtree__build_node(A, X, Y1, A_X_and_Y1),
		set_bbbtree__build_node(C, Y2, Z, C_Y2_and_Z),
		set_bbbtree__build_node(B, A_X_and_Y1, C_Y2_and_Z, Set)
	;
			% Should never occur. Here only to satisfy det checker
		Y = empty,
		error("set_bbbtree__double_rot_l.2")
	).

%------------------------------------------------------------------------------%

	% Double rotation to the right.

:- pred set_bbbtree__double_rot_r(T, set_bbbtree(T), set_bbbtree(T),
								set_bbbtree(T)).
:- mode set_bbbtree__double_rot_r(di, di, di, uo) is det.
:- mode set_bbbtree__double_rot_r(in, in, in, out) is det.

set_bbbtree__double_rot_r(_B, empty, _Z, _Set) :-  % Should never occur. Here
	error("set_bbbtree__double_rot_r.1").	   % only to satisfy det checker

set_bbbtree__double_rot_r(C, tree(A, _N0, X, Y), Z, Set) :-
	(
		Y = tree(B, _N1, Y1, Y2),
		set_bbbtree__build_node(A, X, Y1, A_X_and_Y1),
		set_bbbtree__build_node(C, Y2, Z, C_Y2_and_Z),
		set_bbbtree__build_node(B, A_X_and_Y1, C_Y2_and_Z, Set)
	;
			% Should never occur. Here only to satisfy det checker
		Y = empty,
		error("set_bbbtree__double_rot_r.2")
	).

%------------------------------------------------------------------------------%

% Given two trees L and R, such that all the elements of L are less than those
% of R, and an element V that lies between the values of L and the values of R
% construct a new tree consisting of V, L and R that is balanced by the use of
% single and double left and right rotations.

:- pred set_bbbtree__balance(T, set_bbbtree(T), set_bbbtree(T),
							set_bbbtree(T), int).
% :- mode set_bbbtree__balance(di, di, di, uo, in) is det.
:- mode set_bbbtree__balance(in, in, in, out, in) is det.

set_bbbtree__balance(V, L, R, Set, Ratio) :-
	set_bbbtree__size(L, LSize),
	set_bbbtree__size(R, RSize),
	(
		Val is LSize + RSize,
		Val < 2
	->
			% The two trees are too small to bother rebalancing
		set_bbbtree__build_node(V, L, R, Set)
	;
		Val is Ratio * LSize,
		RSize > Val
	->
		(
			R = tree(_V0, _N0, RL, RR)
		->
			set_bbbtree__size(RL, RLSize),	% right side too big
			set_bbbtree__size(RR, RRSize),
			(
				RLSize < RRSize		
			->
				set_bbbtree__single_rot_l(V, L, R, Set)
			;
				set_bbbtree__double_rot_l(V, L, R, Set)
			)
		;
			% Should never occur. Here only to satisfy det checker
			error("set_bbbtree__balance.1")
		)
	;
		Val is Ratio * RSize,
		LSize > Val
	->
		(
			L = tree(_V1, _N1, LL, LR)
		->
			set_bbbtree__size(LL, LLSize),	% left side too big
			set_bbbtree__size(LR, LRSize),
			(
				LRSize < LLSize
			->
				set_bbbtree__single_rot_r(V, L, R, Set)
			;
				set_bbbtree__double_rot_r(V, L, R, Set)
			)
		;
			% Should never occur. Here only to satisfy det checker
			error("set_bbbtree__balance.2")
		)
	;
		set_bbbtree__build_node(V, L, R, Set)	% Already balanced
	).

%------------------------------------------------------------------------------%

% Given two trees concatenate them by removing the greates element from the left
% subtree if it is larger than the right subtree by a factor of ratio, else
% the smallest element from the right subtree, and make it the root along with
% the two remaining trees as its subtrees. No rebalancing is performed on the
% resultant tree. `set_bbbtree__concat3' is largely used for deletion of
% elements. This predicate should not be confused with the predicate `concat3'
% in the paper for that is `set_bbbtree__concat4'.

:- pred set_bbbtree__concat3(set_bbbtree(T),set_bbbtree(T),set_bbbtree(T)).
% :- mode set_bbbtree__concat3(di, di, uo) is det.
:- mode set_bbbtree__concat3(in, in, out) is det.

set_bbbtree__concat3(L, R, Set) :-
	set_bbbtree__size(L, LSize),
	(
		LSize = 0		% Left tree empty so just
	->				% return the right tree
		Set = R
	;
		set_bbbtree__size(R, RSize),
		(
			RSize = 0	% Right tree empty so just
		->			% just return the left tree
			Set = L
		;
			% If the left tree is the larger of the two then
			% remove its largest value and make it the root
			% of the new left and the right trees.
			% Otherwise remove the smallest value from the
			% right tree and make it the root of the left and
			% the new right trees.
			(
				LSize > RSize	
			->
				(
					set_bbbtree__remove_largest(L, X, NewL)
				->
					set_bbbtree__build_node(X, NewL, R, Set)
				;
						% Should never happen. Here only
						% to satisfy the det checker.
					error("set_bbbtree__concat3.1")
				)
			;
				(
					set_bbbtree__remove_least(R, X, NewR)
				->
					set_bbbtree__build_node(X, L, NewR, Set)
				;
						% Should never happen. Here only
						% to satisfy the det checker.
					error("set_bbbtree__concat3.2")
				)
			)
		)
	).

%------------------------------------------------------------------------------%

% Given two trees L and R, such that all the elements of L are less than those
% of R, and an element V that lies between the values of L and the values of R
% construct a new tree consisting of V and the elements of L and R.
% This predicate is the equivalent of concat3 in the paper.

:- pred set_bbbtree__concat4(set_bbbtree(T), set_bbbtree(T), T,
							set_bbbtree(T), int).
% :- mode set_bbbtree__concat4(di, di, di, uo, in) is det.
:- mode set_bbbtree__concat4(in, in, in, out, in) is det.

set_bbbtree__concat4(empty, R, V, Set, Ratio) :-
	set_bbbtree__insert_r(R, V, Set, Ratio).

set_bbbtree__concat4(tree(LV, LN, LL, LR), R, V, Set, Ratio) :-
	(
		R = empty,
		set_bbbtree__insert_r(tree(LV, LN, LL, LR), V, Set, Ratio)
	;
		R = tree(RV, RN, RL, RR),
		(
			Val is Ratio * LN,	% Right too big
			Val < RN
		->
			set_bbbtree__concat4(tree(LV,LN,LL,LR), RL, V,
								NewL, Ratio),
			set_bbbtree__balance(RV, NewL, RR, Set, Ratio)
		;
			Val is Ratio * RN,	% Left too big
			Val < LN
		->
			set_bbbtree__concat4(LR, tree(RV,RN,RL,RR), V,
								NewR, Ratio),
			set_bbbtree__balance(LV, LL, NewR, Set, Ratio)
		;
			set_bbbtree__build_node(V, tree(LV,LN,LL,LR), R, Set)
		)
	).

%------------------------------------------------------------------------------%

% Given a set return the subset that is less that X

:- pred set_bbbtree__split_lt(set_bbbtree(T), T, set_bbbtree(T), int).
:- mode set_bbbtree__split_lt(in, in, out, in) is det.

set_bbbtree__split_lt(empty, _X, empty, _Ratio).

set_bbbtree__split_lt(tree(V, _N, L, R), X, Set, Ratio) :-
	compare(Result, X, V),
	(
		Result = (<),
		set_bbbtree__split_lt(L, X, Set, Ratio)
	;
		Result = (>),
		set_bbbtree__split_lt(R, X, Set0, Ratio),
		set_bbbtree__concat4(L, Set0, V, Set, Ratio)
	;
		Result = (=),
		Set = L
	).

%------------------------------------------------------------------------------%

% Given a set return the subset of it that is greater that X

:- pred set_bbbtree__split_gt(set_bbbtree(T), T, set_bbbtree(T), int).
:- mode set_bbbtree__split_gt(in, in, out, in) is det.

set_bbbtree__split_gt(empty, _X, empty, _Ratio).

set_bbbtree__split_gt(tree(V, _N, L, R), X, Set, Ratio) :-
	compare(Result, X, V),
	(
		Result = (>),
		set_bbbtree__split_gt(R, X, Set, Ratio)
	;
		Result = (<),
		set_bbbtree__split_gt(L, X, Set0, Ratio),
		set_bbbtree__concat4(Set0, R, V, Set, Ratio)
	;
		Result = (=),
		Set = R
	).

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 24/04/99
%	Function forms added.

set_bbbtree__list_to_set(Xs) = S :-
	set_bbbtree__list_to_set(Xs, S).

set_bbbtree__sorted_list_to_set(Xs) = S :-
	set_bbbtree__sorted_list_to_set(Xs, S).

set_bbbtree__to_sorted_list(S) = Xs :-
	set_bbbtree__to_sorted_list(S, Xs).

set_bbbtree__init = S :-
	set_bbbtree__init(S).

set_bbbtree__make_singleton_set(T) = S :-
	set_bbbtree__singleton_set(S, T).

set_bbbtree__insert(S1, T) = S2 :-
	set_bbbtree__insert(S1, T, S2).

set_bbbtree__insert_list(S1, Xs) = S2 :-
	set_bbbtree__insert_list(S1, Xs, S2).

set_bbbtree__delete(S1, T) = S2 :-
	set_bbbtree__delete(S1, T, S2).

set_bbbtree__delete_list(S1, Xs) = S2 :-
	set_bbbtree__delete_list(S1, Xs, S2).

set_bbbtree__union(S1, S2) = S3 :-
	set_bbbtree__union(S1, S2, S3).

set_bbbtree__power_union(SS) = S :-
	set_bbbtree__power_union(SS, S).

set_bbbtree__intersect(S1, S2) = S3 :-
	set_bbbtree__intersect(S1, S2, S3).

set_bbbtree__power_intersect(SS) = S :-
	set_bbbtree__power_intersect(SS, S).

set_bbbtree__difference(S1, S2) = S3 :-
	set_bbbtree__difference(S1, S2, S3).

set_bbbtree__map(F, S1) = S2 :-
	S2 = set_bbbtree__list_to_set(list__map(F,
			set_bbbtree__to_sorted_list(S1))).

set_bbbtree__filter_map(PF, S1) = S2 :-
	S2 = set_bbbtree__list_to_set(list__filter_map(PF,
			set_bbbtree__to_sorted_list(S1))).

set_bbbtree__fold(F, S, A) = B :-
	B = list__foldl(F, set_bbbtree__to_sorted_list(S), A).

