% Regression tests for set_bbbtree.

% Author benyi.

:- module set_test.

:- interface.

:- import_module io.

:- pred main(io__state :: di, io__state :: uo) is det.


:- implementation.

:- import_module bool, list, string, set_bbbtree.


main -->
	{ set_bbbtree__init(Set0) },
	(
		{ set_bbbtree__empty(Set0) },
		{ set_bbbtree__size(Set0, 0) }
	->
		{ true }
	;
		io__write_string("set_bbbtree__empty/set_bbbtree__size failed on empty set.\n")
	),
	(
		{ set_bbbtree__is_member(5, Set0, no) }
	->
		{ true }
	;
		io__write_string("set_bbbtree__is_member should have failed on empty set\n")
	),
	{ set_bbbtree__insert(Set0, 5, Set10) },
	(
		{ set_bbbtree__is_member(5, Set10, yes) },
		{ set_bbbtree__is_member(6, Set10, no) },
		{ set_bbbtree__singleton_set(Set10, 5) },
		{ set_bbbtree__least(Set10, 5) },
		{ set_bbbtree__largest(Set10, 5) }
	->
		{ true }
	;
		io__write_string("set_bbbtree__is_member/set_bbbtree__singleton_set/set_bbbtree__least/set_bbbtree__largest failed\n")
	),
	{ set_bbbtree__delete(Set10, 5, Set20) },
	(
		{ set_bbbtree__empty(Set20) }
	->
		{ true }
	;
		io__write_string("set_bbbtree__empty failed as set was not empty\n")
	),
	{ set_bbbtree__delete(Set20, 5, Set30) },
	(
		{ set_bbbtree__empty(Set30) }
	->
		{ true }
	;
		io__write_string("set_bbbtree__empty failed as set was not empty\n")
	),

	{ set_bbbtree__delete_list(Set10,[0, 1, 2, 3, 4, 5, 6, 100000], Set40)},
	(
		{ set_bbbtree__empty(Set40) }
	->
		{ true }
	;
		io__write_string("set_bbbtree__empty failed as set was not empty\n")
	),
	{ set_bbbtree__list_to_set([-2, 10, 0, -1, 2, -2, -2, 0], Set50) },
	(
		{ set_bbbtree__size(Set50, 5) },
		{ set_bbbtree__remove_least(Set50, -2, TempSet) }
	->
		{ Set60 = TempSet },
		(
			{ set_bbbtree__is_member(-2, Set60, no) }
		->
			{ true }
		;
			io__write_string("set_bbbtree__is_member failed\n")
		),
		{ set_bbbtree__delete_list(Set60, [-2, -2, -2, 10], Set70) },
		(
			{ set_bbbtree__is_member(-2, Set70, no) },
			{ set_bbbtree__is_member(10, Set70, no) },
			{ set_bbbtree__is_member(10000, Set70, no) },
			{ set_bbbtree__is_member(0, Set70, yes) },
			{ set_bbbtree__largest(Set70, 2) },
			{ set_bbbtree__least(Set70, -1) }
		->
			{ true }
		;
			io__write_string("set_bbbtree__is_member/set_bbbtree__largest/set_bbbtree__least failed\n")
		)
	;
		io__write_string("set_bbbtree__size/set_bbbtree__remove_least failed\n")
	),

	{ set_bbbtree__list_to_set([4, -1, 0], Set80) },
	{ set_bbbtree__to_sorted_list(Set80, List) },
	(
		{ List = [-1, 0, 4] }
	->
		{ true }
	;
		io__write_string("conversion of list to set and back to list failed.\n")
	),

	{ set_bbbtree__list_to_set([1, 2, 3], Set90) },
	{ set_bbbtree__list_to_set([3, 4, 3], Set100) },
	{ set_bbbtree__list_to_set([1, 2, 3, 4], SetUnion) },
	{ set_bbbtree__list_to_set([3], SetIntersection) },
	{ set_bbbtree__list_to_set([1, 2], SetDifference) },
	{ set_bbbtree__union(Set90, Set100, SetUnion0) },
	(
		{ set_bbbtree__equal(SetUnion0, SetUnion) }
	->
		{ true }
	;
		io__write_string("set_bbbtree__union/set_bbbtree__equal failed\n")
	),
	{ set_bbbtree__intersect(Set90, Set100, SetIntersection0) },
	(
		{ set_bbbtree__equal(SetIntersection0, SetIntersection) }
	->
		{ true }
	;
		io__write_string("set_bbbtree__intersect/set_bbbtree__equal failed\n")
	),
	{ set_bbbtree__difference(Set90, Set100, SetDifference0) },
	(
		{ set_bbbtree__equal(SetDifference0, SetDifference) }
	->
		{ true }
	;
		io__write_string("set_bbbtree__difference/set_bbbtree__equal failed\n")
	),

	(
		{ set_bbbtree__subset(SetIntersection, Set90) },
		{ set_bbbtree__subset(SetIntersection, Set100) }
	->
		{ true }
	;
		io__write_string("set_bbbtree__subset failed\n")
	),

	{ set_bbbtree__init(Set110) },
	{ set_bbbtree__insert(Set110, Set90, Set120) },
	{ set_bbbtree__insert(Set120, Set100, Set130) },
	{ set_bbbtree__insert(Set130, SetIntersection, Set140) },
	{ set_bbbtree__insert(Set140, SetDifference, Set150) },
	{ set_bbbtree__power_union(Set150, SetUnion10) },
	(
		{ set_bbbtree__equal(SetUnion10, SetUnion) }
	->
		{ true }
	;
		io__write_string("set_bbbtree__power_union/set_bbbtree__equal failed\n")
	),
	{ set_bbbtree__power_intersect(Set150, SetIntersection10)},
	(
		{ set_bbbtree__equal(SetIntersection10, SetIntersection) }
	->
		{ true }
	;
		io__write_string("set_bbbtree__power_intersect/set_bbbtree__equal failed\n")
	).
