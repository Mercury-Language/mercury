%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: sparse_bitset.m.
% Author: stayl
% Stability: medium.
%
% This module provides an ADT for storing sets of integers.
% If the integers stored are closely grouped, a sparse_bitset
% is much more compact than the representation provided by set.m,
% and the operations will be much faster.
%
%
% Efficiency notes:
%
% A sparse bitset is represented as a sorted list of pairs of integers.
% For a pair `Offset - Bits', `Offset' is a multiple of `int__bits_per_int'.
% The bits of `Bits' describe which of the elements of the range
% `Offset' .. `Offset + bits_per_int - 1' are in the set.
% Pairs with the same value of `Offset' are merged.
% Pairs for which `Bits' is zero are removed.
%
% The values of `Offset' in the list need not be contiguous multiples
% of `bits_per_int', hence the name _sparse_ bitset.
%
% A sparse_bitset is suitable for storing sets of integers which
% can be represented using only a few `Offset - Bits' pairs.
% In the worst case, where the integers stored are not closely
% grouped, a sparse_bitset will take more memory than an
% ordinary set, but the operations should not be too much slower.
%
% In the asymptotic complexities of the operations below,
% `rep_size(Set)' is the number of pairs needed to represent `Set',
% and `card(Set)' is the number of elements in `Set'.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- module sparse_bitset.

:- interface.

:- import_module enum, list, term.

:- type sparse_bitset(T). % <= enum(T).

	% Return an empty set.
:- func init = sparse_bitset(T).

:- pred init(sparse_bitset(T)).
:- mode init(out) is det.

:- pred empty(sparse_bitset(T)).
:- mode empty(in) is semidet.
:- mode empty(out) is det.

	% `equal(SetA, SetB' is true iff `SetA' and `SetB'
	% contain the same elements.
	% Takes O(min(rep_size(SetA), rep_size(SetB))) time.
:- pred equal(sparse_bitset(T), sparse_bitset(T)).
:- mode equal(in, in) is semidet.

	% `list_to_set(List)' returns a set
	% containing only the members of `List'.
	% In the worst case this will take O(length(List)^2) time
	% and space. If the elements of the list are closely
	% grouped, it will be closer to O(length(List)).
:- func list_to_set(list(T)) = sparse_bitset(T) <= enum(T).

:- pred list_to_set(list(T), sparse_bitset(T)) <= enum(T).
:- mode list_to_set(in, out) is det.

	% `sorted_list_to_set(List)' returns a set containing
	% only the members of `List'.
	% `List' must be sorted.
	% Takes O(length(List)) time and space.
:- func sorted_list_to_set(list(T)) = sparse_bitset(T) <= enum(T).

:- pred sorted_list_to_set(list(T), sparse_bitset(T)) <= enum(T).
:- mode sorted_list_to_set(in, out) is det.

	% `to_sorted_list(Set, List)' returns a list
	% containing all the members of `Set', in sorted order.
	% Takes O(card(Set)) time and space.
:- func to_sorted_list(sparse_bitset(T)) = list(T) <= enum(T).

:- pred to_sorted_list(sparse_bitset(T), list(T)) <= enum(T).
:- mode to_sorted_list(in, out) is det.

	% `make_singleton_set(Elem)' returns a set
	% containing just the single element `Elem'.
:- func make_singleton_set(T) = sparse_bitset(T) <= enum(T).

	% Note: set.m contains the reverse mode of this predicate,
	% but it is difficult to implement both modes using
	% the representation in this module.
:- pred singleton_set(sparse_bitset(T), T) <= enum(T).
:- mode singleton_set(out, in) is det.

	% `subset(Subset, Set)' is true iff `Subset' is a subset of `Set'.
	% Same as `intersect(Set, Subset, Subset)', but may be more efficient.
:- pred subset(sparse_bitset(T), sparse_bitset(T)).
:- mode subset(in, in) is semidet.

	% `superset(Superset, Set)' is true iff `Superset' is a
	% superset of `Set'.
	% Same as `intersect(Superset, Set, Set)', but may be more efficient.
:- pred superset(sparse_bitset(T), sparse_bitset(T)).
:- mode superset(in, in) is semidet.

        % `contains(Set, X)' is true iff `X' is a member of `Set'.
	% Takes O(rep_size(Set)) time.
:- pred contains(sparse_bitset(T), T) <= enum(T).
:- mode contains(in, in) is semidet.

	% `insert(Set, X)' returns the union
	% of `Set' and the set containing only `X'.
	% Takes O(rep_size(Set)) time and space.
:- func insert(sparse_bitset(T), T) = sparse_bitset(T) <= enum(T).

:- pred insert(sparse_bitset(T), T, sparse_bitset(T)) <= enum(T).
:- mode insert(in, in, out) is det.

	% `insert_list(Set, X)' returns the union of `Set' and the set
	% containing only the members of `X'.
	% Same as `union(Set, list_to_set(X))', but may be more efficient.
:- func insert_list(sparse_bitset(T), list(T)) = sparse_bitset(T) <= enum(T).

:- pred insert_list(sparse_bitset(T), list(T), sparse_bitset(T)) <= enum(T).
:- mode insert_list(in, in, out) is det.

	% `delete(Set, X)' returns the difference
	% of `Set' and the set containing only `X'.
	% Takes O(rep_size(Set)) time and space.
:- func delete(sparse_bitset(T), T) = sparse_bitset(T) <= enum(T).

:- pred delete(sparse_bitset(T), T, sparse_bitset(T)) <= enum(T).
:- mode delete(in, in, out) is det.

	% `delete_list(Set, X)' returns the difference of `Set' and the set
	% containing only the members of `X'.
	% Same as `difference(Set, list_to_set(X))', but may be more efficient.
:- func delete_list(sparse_bitset(T), list(T)) = sparse_bitset(T) <= enum(T).

:- pred delete_list(sparse_bitset(T), list(T), sparse_bitset(T)) <= enum(T).
:- mode delete_list(in, in, out) is det.

	% `remove(Set, X)' returns the difference
	% of `Set' and the set containing only `X',
	% failing if `Set' does not contain `X'.
	% Takes O(rep_size(Set)) time and space.
:- func remove(sparse_bitset(T), T) = sparse_bitset(T) <= enum(T).
:- mode remove(in, in) = out is semidet.

:- pred remove(sparse_bitset(T), T, sparse_bitset(T)) <= enum(T).
:- mode remove(in, in, out) is semidet.

	% `remove_list(Set, X)' returns the difference of `Set'
	% and the set containing all the elements of `X',
	% failing if any element of `X' is not in `Set0'.
	% Same as
	%	`subset(list_to_set(X), Set), difference(Set, list_to_set(X))',
	% but may be more efficient.
:- func remove_list(sparse_bitset(T), list(T)) = sparse_bitset(T) <= enum(T).
:- mode remove_list(in, in) = out is semidet.

:- pred remove_list(sparse_bitset(T), list(T), sparse_bitset(T)) <= enum(T).
:- mode remove_list(in, in, out) is semidet.

	% `remove_least(Set0, X, Set)' is true iff `X' is the
	% least element in `Set0', and `Set' is the set which
	% contains all the elements of `Set0' except `X'.
	% Takes O(1) time and space.
:- pred remove_least(sparse_bitset(T), T, sparse_bitset(T)) <= enum(T).
:- mode remove_least(in, out, out) is semidet.

	% `union(SetA, SetB)' returns the union of `SetA' and `SetB'. 
	% The efficiency of the union operation is not sensitive
	% to the argument ordering.
	% Takes O(rep_size(SetA) + rep_size(SetB)) time and space.
:- func union(sparse_bitset(T), sparse_bitset(T)) = sparse_bitset(T).

:- pred union(sparse_bitset(T), sparse_bitset(T), sparse_bitset(T)).
:- mode union(in, in, out) is det.

	% `intersect(SetA, SetB)' returns the intersection of
	% `SetA' and `SetB'. The efficiency of the intersection
	% operation is not sensitive to the argument ordering.
	% Takes O(rep_size(SetA) + rep_size(SetB)) time and
	% O(min(rep_size(SetA)), rep_size(SetB)) space.
:- func intersect(sparse_bitset(T), sparse_bitset(T)) = sparse_bitset(T).

:- pred intersect(sparse_bitset(T), sparse_bitset(T), sparse_bitset(T)).
:- mode intersect(in, in, out) is det.

	% `difference(SetA, SetB)' returns the set containing all the
	% elements of `SetA' except those that occur in `SetB'.
	% Takes O(rep_size(SetA) + rep_size(SetB)) time and
	% O(rep_size(SetA)) space.
:- func difference(sparse_bitset(T), sparse_bitset(T)) = sparse_bitset(T).

:- pred difference(sparse_bitset(T), sparse_bitset(T), sparse_bitset(T)).
:- mode difference(in, in, out) is det.

	% `count(Set)' returns the number of elements in `Set'.
	% Takes O(card(Set)) time.
:- func count(sparse_bitset(T)) = int <= enum(T).

	% `foldl(Func, Set, Start)' calls Func with each element
	% of `Set' (in sorted order) and an accumulator
	% (with the initial value of `Start'), and returns
	% the final value.
	% Takes O(card(Set)) time.
:- func foldl(func(T, U) = U, sparse_bitset(T), U) = U <= enum(T).

	% `foldr(Func, Set, Start)' calls Func with each element
	% of `Set' (in reverse sorted order) and an accumulator
	% (with the initial value of `Start'), and returns
	% the final value.
	% Takes O(card(Set)) time.
:- func foldr(func(T, U) = U, sparse_bitset(T), U) = U <= enum(T).

%-----------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

:- pragma type_spec(list_to_set/1, T = var(_)).
:- pragma type_spec(list_to_set/1, T = int).

:- pragma type_spec(sorted_list_to_set/1, T = var(_)).
:- pragma type_spec(sorted_list_to_set/1, T = int).

:- pragma type_spec(to_sorted_list/1, T = var(_)).
:- pragma type_spec(to_sorted_list/1, T = int).

:- pragma type_spec(make_singleton_set/1, T = var(_)).
:- pragma type_spec(make_singleton_set/1, T = int).

:- pragma type_spec(contains/2, T = var(_)).
:- pragma type_spec(contains/2, T = int).

:- pragma type_spec(insert/2, T = var(_)).
:- pragma type_spec(insert/2, T = int).

:- pragma type_spec(insert_list/2, T = var(_)).
:- pragma type_spec(insert_list/2, T = int).

:- pragma type_spec(delete/2, T = var(_)).
:- pragma type_spec(delete/2, T = int).

:- pragma type_spec(delete_list/2, T = var(_)).
:- pragma type_spec(delete_list/2, T = int).

:- pragma type_spec(foldr/3, T = int).
:- pragma type_spec(foldr/3, T = var(_)).

:- pragma type_spec(foldl/3, T = int).
:- pragma type_spec(foldl/3, T = var(_)).

:- pragma type_spec(list_to_set/2, T = var(_)).
:- pragma type_spec(list_to_set/2, T = int).

:- pragma type_spec(sorted_list_to_set/2, T = var(_)).
:- pragma type_spec(sorted_list_to_set/2, T = int).

:- pragma type_spec(to_sorted_list/2, T = var(_)).
:- pragma type_spec(to_sorted_list/2, T = int).

:- pragma type_spec(singleton_set/2, T = var(_)).
:- pragma type_spec(singleton_set/2, T = int).

:- pragma type_spec(insert/3, T = var(_)).
:- pragma type_spec(insert/3, T = int).

:- pragma type_spec(insert_list/3, T = var(_)).
:- pragma type_spec(insert_list/3, T = int).

:- pragma type_spec(delete/3, T = var(_)).
:- pragma type_spec(delete/3, T = int).

:- pragma type_spec(delete_list/3, T = var(_)).
:- pragma type_spec(delete_list/3, T = int).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list, int, require, std_util.

	% The number of variables for most procedures
	% should fit into one or two words.
:- type sparse_bitset(T)	% <= enum(T)
	---> sparse_bitset(bitset_impl).

	% The list of elements, sorted on offset.
	% No two elements have the same offset.
:- type bitset_impl == list(bitset_elem).

	% Cells of this type should only be
	% constructed using make_bitset_elem/2.
:- type bitset_elem
	---> bitset_elem(
		offset :: int,	% multiple of bits_per_int
		bits :: int	% bits offset .. offset + bits_per_int - 1
				% The sparse_bitset operations all remove
				% elements of the list with a `bits'
				% field of zero.
	).

%-----------------------------------------------------------------------------%

init = sparse_bitset([]).

empty(init).

equal(X, X).

%-----------------------------------------------------------------------------%

to_sorted_list(Set) = foldr(func(Elem, Acc0) = [Elem | Acc0], Set, []).

%-----------------------------------------------------------------------------%

foldl(F, sparse_bitset(Set), Acc0) = foldl_2(F, Set, Acc0).

:- func foldl_2(func(T, U) = U, bitset_impl, U) = U <= enum(T).
:- pragma type_spec(foldl_2/3, T = int).
:- pragma type_spec(foldl_2/3, T = var(_)).

foldl_2(_, [], Acc) = Acc.
foldl_2(F, [H | T], Acc0) = Acc :-
	Acc1 = fold_bits(low_to_high, F, H ^ offset, H ^ bits, Acc0),
	Acc = foldl_2(F, T, Acc1).

foldr(F, sparse_bitset(Set), Acc0) = foldr_2(F, Set, Acc0).

:- func foldr_2(func(T, U) = U, bitset_impl, U) = U <= enum(T).
:- pragma type_spec(foldr_2/3, T = int).
:- pragma type_spec(foldr_2/3, T = var(_)).

	% We don't just use list__foldr here because the
	% overhead of allocating the closure for fold_bits
	% is significant for the compiler's runtime,
	% so it's best to avoid that even if
	% `--optimize-higher-order' is not set.
foldr_2(_, [], Acc) = Acc.
foldr_2(F, [H | T], Acc0) = Acc :-
	Acc1 = foldr_2(F, T, Acc0),
	Acc = fold_bits(high_to_low, F, H ^ offset, H ^ bits, Acc1).

:- func fold_bits(fold_direction, func(T, U) = U, int, int, U) = U <= enum(T).
:- pragma type_spec(fold_bits/5, T = int).
:- pragma type_spec(fold_bits/5, T = var(_)).

fold_bits(Dir, F, Offset, Bits, Acc0) = Acc :-
	Size = bits_per_int,
	Acc = fold_bits_2(Dir, F, Offset, Bits, Size, Acc0).

:- type fold_direction
	--->	low_to_high
	;	high_to_low
	.

	% Do a binary search for the 1 bits in an int.
:- func fold_bits_2(fold_direction, func(T, U) = U,
		int, int, int, U) = U <= enum(T).
:- pragma type_spec(fold_bits_2/6, T = int).
:- pragma type_spec(fold_bits_2/6, T = var(_)).

fold_bits_2(Dir, F, Offset, Bits, Size, Acc0) = Acc :-
	( Bits = 0 ->
		Acc = Acc0
	; Size = 1 ->
		( Elem = from_int(Offset) ->
			Acc = F(Elem, Acc0)
		;
			% We only apply `from_int/1' to integers returned
			% by `to_int/1', so it should never fail.
			error("sparse_bitset.m: `enum__from_int/1' failed")
		)
	;
		HalfSize = unchecked_right_shift(Size, 1),
		Mask = mask(HalfSize),
		
		% Extract the low-order half of the bits.
		LowBits = Mask /\ Bits,

		% Extract the high-order half of the bits.
		HighBits = Mask /\ unchecked_right_shift(Bits, HalfSize), 

		(
			Dir = low_to_high,
			Acc1 = fold_bits_2(Dir, F, Offset, LowBits,
					HalfSize, Acc0),
			Acc = fold_bits_2(Dir, F, Offset + HalfSize, HighBits,
					HalfSize, Acc1)
		;
			Dir = high_to_low,
			Acc1 = fold_bits_2(Dir, F, Offset + HalfSize, HighBits,
					HalfSize, Acc0),
			Acc = fold_bits_2(Dir, F, Offset, LowBits,
					HalfSize, Acc1)
		)
	).

%-----------------------------------------------------------------------------%

count(Set) = foldl((func(_, Acc) = Acc + 1), Set, 0).

%-----------------------------------------------------------------------------%

make_singleton_set(A) = insert(init, A).

insert(sparse_bitset(Set), Elem) =
		sparse_bitset(insert_2(Set, enum__to_int(Elem))).

:- func insert_2(bitset_impl, int) = bitset_impl.

insert_2([], Index) = [make_bitset_elem(Offset, Bits)] :-
	bits_for_index(Index, Offset, Bits).	
insert_2(Set0, Index) = Set :-
	Set0 = [Data0 | Rest],
	Offset0 = Data0 ^ offset,
	( Index < Offset0 ->
		bits_for_index(Index, Offset, Bits),
		Set = [make_bitset_elem(Offset, Bits) | Set0]
	; BitToSet = Index - Offset0, BitToSet < bits_per_int ->
		Bits0 = Data0 ^ bits,
		( get_bit(Bits0, BitToSet) \= 0 ->
			Set = Set0
		;
			Bits = set_bit(Bits0, BitToSet),
			Set = [make_bitset_elem(Offset0, Bits) | Rest]
		)
	;
		Set = [Data0 | insert_2(Rest, Index)]
	).

insert_list(Set, List) = union(list_to_set(List), Set).

%-----------------------------------------------------------------------------%

delete(Set, Elem) = difference(Set, insert(init, Elem)).
delete_list(Set, List) = difference(Set, list_to_set(List)).

remove(Set0, Elem) = Set :-
	contains(Set0, Elem),
	Set = delete(Set0, Elem).

remove_list(Set0, Elems) = Set :-
	list_to_set(Elems, ElemsSet),
	subset(ElemsSet, Set0),
	Set = difference(Set0, ElemsSet).

%-----------------------------------------------------------------------------%

remove_least(sparse_bitset(Set0), Elem, sparse_bitset(Set)) :-
	Set0 = [First | Rest],
	Bits0 = First ^ bits,
	Offset = First ^ offset,
	Bit = find_least_bit(Bits0),
	( Elem0 = from_int(Offset + Bit) ->
		Elem = Elem0
	;
		% We only apply `from_int/1' to integers returned
		% by `to_int/1', so it should never fail.
		error("sparse_bitset.m: `enum__from_int/1' failed")
	),
	Bits = clear_bit(Bits0, Bit),
	( Bits = 0 ->
		Set = Rest
	;
		Set = [make_bitset_elem(Offset, Bits) | Rest]
	).

:- func find_least_bit(int) = int.

find_least_bit(Bits0) = BitNum :-
	Size = bits_per_int,
	BitNum0 = 0,
	BitNum = find_least_bit_2(Bits0, Size, BitNum0).
	
:- func find_least_bit_2(int, int, int) = int.

find_least_bit_2(Bits0, Size, BitNum0) = BitNum :-
	( Size = 1 ->
		% We can't get here unless the bit is a 1 bit.
		BitNum = BitNum0
	;
		HalfSize = unchecked_right_shift(Size, 1),
		Mask = mask(HalfSize),
		
		LowBits = Bits0 /\ Mask,
		( LowBits \= 0 ->
			BitNum = find_least_bit_2(LowBits, HalfSize, BitNum0)
		;
			HighBits =
				Mask /\ unchecked_right_shift(Bits0, HalfSize),
			BitNum = find_least_bit_2(HighBits, HalfSize,
					BitNum0 + HalfSize)
		)
	).

%-----------------------------------------------------------------------------%

list_to_set(List) =
	sparse_bitset(list_to_set_2(List, [])).

	% Each pass over the input list selects out the elements which
	% belong in the same bitset_elem as the first element.
	% The assumption here is that the items in the input list
	% will have similar values, so that only a few passes
	% will be needed.
:- func list_to_set_2(list(T), bitset_impl) = bitset_impl <= enum(T).
:- pragma type_spec(list_to_set_2/2, T = var(_)).
:- pragma type_spec(list_to_set_2/2, T = int).

list_to_set_2([], List) = List.
list_to_set_2([H | T], List0) = List :-
	bits_for_index(enum__to_int(H), Offset, Bits0),
	list_to_set_3(T, Offset, Bits0, Bits, [], Rest),
	List1 = insert_bitset_elem(make_bitset_elem(Offset, Bits),
			List0),
	List = list_to_set_2(Rest, List1).

	% Go through the list picking out the elements
	% which belong in the same bitset_elem as the first
	% element, returning the uncollected elements.
:- pred list_to_set_3(list(T), int, int, int,
		list(T), list(T)) <= enum(T).
:- mode list_to_set_3(in, in, in, out, in, out) is det.
:- pragma type_spec(list_to_set_3/6, T = var(_)).
:- pragma type_spec(list_to_set_3/6, T = int).
		
list_to_set_3([], _, Bits, Bits, Rest, Rest).
list_to_set_3([H | T], Offset, Bits0, Bits, Rest0, Rest) :-
	BitToSet = enum__to_int(H) - Offset,
	( BitToSet >= 0, BitToSet < bits_per_int ->
		Bits2 = set_bit(Bits0, BitToSet), 
		Rest1 = Rest0
	;
		Bits2 = Bits0,
		Rest1 = [H | Rest0]
	),
	list_to_set_3(T, Offset, Bits2, Bits, Rest1, Rest).

	% The list of elements here is pretty much guaranteed
	% to be small, so use an insertion sort.
:- func insert_bitset_elem(bitset_elem, bitset_impl) = bitset_impl.

insert_bitset_elem(Data, []) = [Data].
insert_bitset_elem(Data0, [Data1 | Rest]) = List :-
	( Data0 ^ offset < Data1 ^ offset ->
		List = [Data0, Data1 | Rest]
	;
		List = [Data1 | insert_bitset_elem(Data0, Rest)]
	).

%-----------------------------------------------------------------------------%

sorted_list_to_set(L) = sparse_bitset(sorted_list_to_set_2(L)).

:- func sorted_list_to_set_2(list(T)) = bitset_impl <= enum(T).
:- pragma type_spec(sorted_list_to_set_2/1, T = var(_)).
:- pragma type_spec(sorted_list_to_set_2/1, T = int).

sorted_list_to_set_2([]) = [].
sorted_list_to_set_2([H | T]) = Set :-
	sorted_list_to_set_3(H, T, Offset, Bits, Set0),
	( Bits = 0 ->
		Set = Set0
	;
		Set = [make_bitset_elem(Offset, Bits) | Set0]
	).

:- pred sorted_list_to_set_3(T, list(T), int, int, bitset_impl) <= enum(T).
:- mode sorted_list_to_set_3(in, in, out, out, out) is det.
:- pragma type_spec(sorted_list_to_set_3/5, T = var(_)).
:- pragma type_spec(sorted_list_to_set_3/5, T = int).

sorted_list_to_set_3(Elem, [], Offset, Bits, []) :-
	bits_for_index(enum__to_int(Elem), Offset, Bits).
sorted_list_to_set_3(Elem1, [Elem2 | Elems], Offset, Bits, Rest) :-
	sorted_list_to_set_3(Elem2, Elems, Offset0, Bits0, Rest0),
	bits_for_index(enum__to_int(Elem1), Offset1, Bits1),
	( Offset1 = Offset0 ->
		Bits = Bits1 \/ Bits0,
		Offset = Offset1,
		Rest = Rest0
	;
		Rest = [make_bitset_elem(Offset0, Bits0) | Rest0],
		Offset = Offset1,
		Bits = Bits1
	).

%-----------------------------------------------------------------------------%

subset(Subset, Set) :- intersect(Set, Subset, Subset).

superset(Superset, Set) :- subset(Set, Superset).

%-----------------------------------------------------------------------------%

contains(sparse_bitset(Set), Elem) :-
	contains_2(Set, enum__to_int(Elem)).

:- pred contains_2(bitset_impl, int).
:- mode contains_2(in, in) is semidet.

contains_2([Data | Rest], Index) :-
	Offset = Data ^ offset,
	Index >= Offset,
	( Index < Offset + bits_per_int ->
		get_bit(Data ^ bits, Index - Offset) \= 0
	;		
		contains_2(Rest, Index)
	).

%-----------------------------------------------------------------------------%

:- func rest(bitset_impl::in(bound([ground | ground]))) =
		(bitset_impl::out) is det.
rest([_ | Rest]) = Rest.

union(sparse_bitset(Set1), sparse_bitset(Set2)) =
		sparse_bitset(union_2(Set1, Set2)).

:- func union_2(bitset_impl, bitset_impl) = bitset_impl.

union_2([], []) = [].
union_2([], B) = B :-
	B = [_ | _].
union_2(A, []) = A :-
	A = [_ | _].
union_2(Set1, Set2) = Set :-
	Set1 = [Data1 | _],
	Set2 = [Data2 | _],
	Offset1 = Data1 ^ offset,
	Offset2 = Data2 ^ offset,
	( Offset1 = Offset2 ->
		Elem = make_bitset_elem(Offset1,
				(Data1 ^ bits) \/ (Data2 ^ bits)),
		Set = [Elem | union_2(Set1 ^ rest, Set2 ^ rest)]
	; Offset1 < Offset2 ->
		Set = [Data1 | union_2(Set1 ^ rest, Set2)]
	;
		Set = [Data2 | union_2(Set1, Set2 ^ rest)]
	).

%-----------------------------------------------------------------------------%

intersect(sparse_bitset(Set1), sparse_bitset(Set2)) =
		sparse_bitset(intersect_2(Set1, Set2)).

:- func intersect_2(bitset_impl, bitset_impl) = bitset_impl.

intersect_2([], []) = [].
intersect_2([], B) = [] :-
	B = [_ | _].
intersect_2(A, []) = [] :-
	A = [_ | _].
intersect_2(Set1, Set2) = Set :-
	Set1 = [Data1 | _],
	Set2 = [Data2 | _],
	Offset1 = Data1 ^ offset,
	Offset2 = Data2 ^ offset,
	( Offset1 = Offset2 ->
		Bits = Data1 ^ bits /\ Data2 ^ bits,
		( Bits = 0 ->
			Set = intersect_2(Set1 ^ rest, Set2 ^ rest)
		;
			Set = [make_bitset_elem(Offset1, Bits) |
				intersect_2(Set1 ^ rest, Set2 ^ rest)]
		)
	; Offset1 < Offset2 ->
		Set = intersect_2(Set1 ^ rest, Set2)
	;
		Set = intersect_2(Set1, Set2 ^ rest)
	).

%-----------------------------------------------------------------------------%

difference(sparse_bitset(Set1), sparse_bitset(Set2)) =
		sparse_bitset(difference_2(Set1, Set2)).

:- func difference_2(bitset_impl, bitset_impl) = bitset_impl.

difference_2([], []) = [].
difference_2([], B) = [] :-
	B = [_|_].
difference_2(A, []) = A :-
	A = [_ | _].
difference_2(Set1, Set2) = Set :-
	Set1 = [Data1 | _],
	Set2 = [Data2 | _],
	Offset1 = Data1 ^ offset,
	Offset2 = Data2 ^ offset,
	( Offset1 = Offset2 ->
		Bits = (Data1 ^ bits) /\ \ (Data2 ^ bits),
		( Bits = 0 ->
			Set = difference_2(Set1 ^ rest, Set2 ^ rest)
		;
			Set = [make_bitset_elem(Offset1, Bits) |
				difference_2(Set1 ^ rest, Set2 ^ rest)]
		)
	; Offset1 < Offset2 ->
		Set = [Data1 | difference_2(Set1 ^ rest, Set2)]
	;
		Set = difference_2(Set1, Set2 ^ rest)
	).

%-----------------------------------------------------------------------------%

	% Return the offset of the element of a set
	% which should contain the given element,
	% and an int with the bit corresponding to
	% that element set.
:- pred bits_for_index(int, int, int).
:- mode bits_for_index(in, out, out) is det.
:- pragma inline(bits_for_index/3).

bits_for_index(Index, Offset, Bits) :-
	Offset = int__floor_to_multiple_of_bits_per_int(Index),
	BitToSet = Index - Offset,
	Bits = set_bit(0, BitToSet).

:- func get_bit(int, int) = int.
:- pragma inline(get_bit/2).

get_bit(Int, Bit) = Int /\ unchecked_left_shift(1, Bit).

:- func set_bit(int, int) = int.
:- pragma inline(set_bit/2).

set_bit(Int0, Bit) = Int0 \/ unchecked_left_shift(1, Bit).

:- func clear_bit(int, int) = int.
:- pragma inline(clear_bit/2).

clear_bit(Int0, Bit) = Int0 /\ \ unchecked_left_shift(1, Bit).

	% `mask(N)' returns a mask which can be `and'ed with an
	% integer to return the lower `N' bits of the integer.
	% `N' must be less than bits_per_int.
:- func mask(int) = int.
:- pragma inline(mask/1).

mask(N) = \ unchecked_left_shift(\ 0, N).

:- func make_bitset_elem(int, int) = bitset_elem.
:- pragma inline(make_bitset_elem/2).

%make_bitset_elem(A, B) = bitset_elem(A, B).

:- pragma foreign_decl("C", "
	#include ""mercury_heap.h""	/* for MR_incr_hp_atomic_msg() */
").

	% The bit pattern will often look like a pointer,
	% so allocate the pairs using GC_malloc_atomic()
	% to avoid unnecessary memory retention.
	% Doing this slows down the compiler by about 1%,
	% but in a library module it's better to be safe.
:- pragma foreign_proc("C", make_bitset_elem(A::in, B::in) = (Pair::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"{

#define ML_BITSET_TAG MR_FIRST_UNRESERVED_RAW_TAG

	MR_tag_incr_hp_atomic_msg(Pair, MR_mktag(ML_BITSET_TAG), 
			2, MR_PROC_LABEL, ""sparse_bitset:bitset_elem/0"");
	MR_field(MR_mktag(ML_BITSET_TAG), Pair, 0) = A;
	MR_field(MR_mktag(ML_BITSET_TAG), Pair, 1) = B;
}").

make_bitset_elem(Offset, Bits) = bitset_elem(Offset, Bits).

%-----------------------------------------------------------------------------%

init(init).

singleton_set(make_singleton_set(A), A).

insert(A, B, insert(A, B)).

insert_list(A, B, insert_list(A, B)).

delete(A, B, delete(A, B)).

delete_list(A, B, delete_list(A, B)).

remove(A, B, remove(A, B)).

remove_list(A, B, remove_list(A, B)).

list_to_set(A, list_to_set(A)).

to_sorted_list(A, to_sorted_list(A)).

sorted_list_to_set(A, sorted_list_to_set(A)).

union(A, B, union(A, B)).

intersect(A, B, intersect(A, B)).

difference(A, B, difference(A, B)).

%-----------------------------------------------------------------------------%
