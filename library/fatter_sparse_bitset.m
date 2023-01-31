%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2011-2012 The University of Melbourne.
% Copyright (C) 2014, 2016-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: fatter_sparse_bitset.m.
% Author: zs.
% Stability: medium.
%
% This module provides an abstract data type for storing sets of items
% that can each be represented by non-negative integers.
% If the integers being stored are closely grouped, a sparse_bitset
% will be much more compact than either the list-of-elements representations
% provided by set.m, set_ordlist.m, and set_unordlist.m, or the
% tree-of-elements representations provided by set_bbbtree.m, set_tree234.
% or set_ctree234.m.
%
% A sparse bitset is represented as a sorted list, with each element
% of this list containing two unsigned integers: Offset and Bits.
% Offset will always be a multiple of uint.ubits_per_uint, and
% the bits of Bits describe which of the elements of the range
% Offset .. (Offset + ubits_per_uint - 1) are in the set.
% The value of Bits must not be zero; any operation that would clear
% all the bits in Bits must also delete the whole list element.
% As one goes from the head towards the tail of the list, the offsets of
% the list elements must strictly increase.
%
% The values of Offset in the list need not be *contiguous* multiples
% of ubits_per_uint, hence the name *sparse* bitset.
%
% A sparse_bitset is suitable for storing sets of integers which
% can be represented using only a few Offset/Bits pairs.
% In the worst case, where the integers stored are not closely grouped,
% a sparse_bitset will take more memory than an ordinary set, but
% the operations should not be too much slower.
%
% In the asymptotic complexities of the operations below,
% `rep_size(Set)' is the number of Offset/Bits pairs needed to represent Set,
% and `card(Set)' is the cardinality of Set (i.e. its number of elements).
%
%---------------------------------------------------------------------------%
%
% The sparse_bitset, fat_sparse_bitset and fatter_sparse_bitset modules
% all use minor variations of the same data structure. To show the differences
% between them, we will use an example bitset containing four offset/bits
% pairs, with the offsets being 0, 64, 256 and 512; the values of the bits
% fields do not matter. We will assume that the word size is 64 bits.
%
% - The sparse_bitset module will store this set using eight memory cells,
%   all of them containing two words. Four cells will contain the offset/bits
%   pairs, and four will be the cons cells linking them together.
%
% - The fat_sparse_bitset module will store this set using four memory cells,
%   all of them containing three words: an offset, the corresponding bits,
%   and the pointer to the next cell.
%
% - The fatter_sparse_bitset module will store this set using three memory
%   cells, all of them containing four words: an offset, *two* words of
%   corresponding bits, and the pointer to the next cell.
%
% In each of the first two representations, the cells' bits fields
% will contain information about the presence/absence in the set of items
% 0-63, 64-127, 256-319, and 512-575 respectively.
% In the third representation, the cells' bits fields will contain information
% about the presence/absence in the set of items 0-127, 256-383, and 512-639
% respectively.
%
% The fat and fatter sparse_bitset representations have the advantage over
% the base sparse_bitset representation that processing the list requires
% following fewer pointers, since fetching one cell gets you the offset,
% the bits, and the address of the next cell all at once. This can be expected
% to result in fewer cache misses. However, the cons cell and the corresponding
% offset/bits cell in the sparse_bitset representation are very likely to be
% allocated together, which means that a good memory allocator may return
% two cells near each other for them.
%
% The fat_sparse_bitset implementation effectively replaces two two-word cells
% with a single three-word cell. This reduces initialization cost from writing
% four words to memory to writing three, but this is usually immaterial,
% because on todays processors, which are usually superscalar and have write
% buffers, those writes are effectively free. There is also no real advantage
% in memory usage, because most memory allocators will allocate a four-word
% block for a three-word request. This includes the Boehm-Demers-Weiser system
% usually used with Mercury.
%
% The fatter_sparse_bitset representation is intended to avoid this last
% problem with the fat_sparse_bitset representation, by repurposing the
% unused word allocated in each cell to represent up to another word's worth
% of items. In cases where this word contains at least one set bit, this
% is a worthwhile gain; in cases where this word is usually all zeroes,
% there will be no gain, but the (thanks to write buffers) the initialization
% cost will typically be negligible as well. There is the additional cost
% that each operation on the bits associated with an offset will either
% have to select which word of bits it applies to, or has to apply to both
% words of bits, but unless typical sets are so sparse that one word of bits
% in each cell is almost always empty, the reduction in the number of cells
% that have to be visited will more than compensate for those costs.
%
%---------------------------------------------------------------------------%

:- module fatter_sparse_bitset.
:- interface.

:- import_module enum.
:- import_module list.
:- import_module term.

:- use_module set.

%---------------------------------------------------------------------------%

:- type fatter_sparse_bitset(T). % <= uenum(T).

%---------------------------------------------------------------------------%
%
% Initial creation of sets.
%

    % Return an empty set.
    %
:- func init = fatter_sparse_bitset(T).
:- pred init(fatter_sparse_bitset(T)::out) is det.

    % Note: set.m contains the reverse mode of this predicate, but it is
    % difficult to implement both modes using the representation in this
    % module.
    %
:- pred singleton_set(fatter_sparse_bitset(T)::out, T::in) is det <= uenum(T).

    % make_singleton_set(Item) returns a set containing just the single Item.
    %
:- func make_singleton_set(T) = fatter_sparse_bitset(T) <= uenum(T).

%---------------------------------------------------------------------------%
%
% Emptiness and singleton-ness tests.
%

:- pred is_empty(fatter_sparse_bitset(T)::in) is semidet.

:- pred is_non_empty(fatter_sparse_bitset(T)::in) is semidet.

    % Is the given set a singleton, and if yes, what is the element?
    %
:- pred is_singleton(fatter_sparse_bitset(T)::in, T::out) is semidet
    <= uenum(T).

%---------------------------------------------------------------------------%
%
% Membership tests.
%

    % member(Item, Set) is true iff Item is a member of Set.
    % Takes O(rep_size(Set)) time.
    %
:- pred member(T, fatter_sparse_bitset(T)) <= uenum(T).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

    % contains(Set, Item) is true iff Item is a member of Set.
    % Takes O(rep_size(Set)) time.
    %
:- pred contains(fatter_sparse_bitset(T)::in, T::in) is semidet <= uenum(T).

%---------------------------------------------------------------------------%
%
% Insertions and deletions.
%

    % insert(Set, Item) returns the union of Set and the set containing
    % only Item. Takes O(rep_size(Set)) time and space.
    %
:- func insert(fatter_sparse_bitset(T), T) = fatter_sparse_bitset(T)
    <= uenum(T).
:- pred insert(T::in,
    fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::out) is det
    <= uenum(T).

    % insert_new(Item, Set0, Set) returns the union of Set0 and the set
    % containing only Item if Set0 does not already contain Item; if it does,
    % it fails. Takes O(rep_size(Set)) time and space.
    %
:- pred insert_new(T::in,
    fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::out) is semidet
    <= uenum(T).

    % insert_list(Set, Item) returns the union of Set and the set containing
    % only the members of Item. Same as `union(Set, list_to_set(Item))',
    % but may be more efficient.
    %
:- func insert_list(fatter_sparse_bitset(T), list(T)) = fatter_sparse_bitset(T)
    <= uenum(T).
:- pred insert_list(list(T)::in,
    fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::out) is det
    <= uenum(T).

%---------------------%

    % delete(Set, Item) returns the difference of Set and the set containing
    % only Item. Takes O(rep_size(Set)) time and space.
    %
:- func delete(fatter_sparse_bitset(T), T) = fatter_sparse_bitset(T)
    <= uenum(T).
:- pred delete(T::in,
    fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::out) is det
    <= uenum(T).

    % delete_list(Set, Item) returns the difference of Set and the set
    % containing only the members of Item. Same as
    % `difference(Set, list_to_set(Item))', but may be more efficient.
    %
:- func delete_list(fatter_sparse_bitset(T), list(T)) = fatter_sparse_bitset(T)
    <= uenum(T).
:- pred delete_list(list(T)::in,
    fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::out) is det
    <= uenum(T).

    % remove(Item, Set0, Set) returns in Set the difference of Set0
    % and the set containing only Item, failing if Set0 does not contain Item.
    % Takes O(rep_size(Set)) time and space.
    %
:- pred remove(T::in,
    fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::out) is semidet
    <= uenum(T).

    % remove_list(Item, Set0, Set) returns in Set the difference of Set0
    % and the set containing all the elements of Item, failing if any element
    % of Item is not in Set0. Same as `subset(list_to_set(Item), Set0),
    % difference(Set0, list_to_set(Item), Set)', but may be more efficient.
    %
:- pred remove_list(list(T)::in,
    fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::out) is semidet
    <= uenum(T).

    % remove_leq(Set, Item) returns Set with all elements less than or equal
    % to Item removed. In other words, it returns the set containing all the
    % elements of Set whose enum forms are greater than the enum form of Item.
    %
:- func remove_leq(fatter_sparse_bitset(T), T) = fatter_sparse_bitset(T)
    <= uenum(T).
:- pred remove_leq(T::in,
    fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::out) is det
    <= uenum(T).

    % remove_gt(Set, Item) returns Set with all elements greater than Item
    % removed. In other words, it returns the set containing all the elements
    % of Set whose enum forms are less than or equal to the enum form of Item.
    %
:- func remove_gt(fatter_sparse_bitset(T), T) = fatter_sparse_bitset(T)
    <= uenum(T).
:- pred remove_gt(T::in,
    fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::out) is det
    <= uenum(T).

    % remove_least(Set0, Item, Set) is true iff Item is the element
    % whose enum form is the smallest in Set0, and Set is the set
    % which contains all the elements of Set0 except Item. Takes O(1) time
    % and space.
    %
:- pred remove_least(T::out,
    fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::out) is semidet
    <= uenum(T).

%---------------------------------------------------------------------------%
%
% Comparisons between sets.
%

    % equal(SetA, SetB) is true iff SetA and SetB contain the same elements.
    % Takes O(min(rep_size(SetA), rep_size(SetB))) time.
    %
:- pred equal(fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::in)
    is semidet.

    % subset(Subset, Set) is true iff Subset is a subset of Set.
    % Same as `intersect(Set, Subset, Subset)', but may be more efficient.
    %
:- pred subset(fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::in)
    is semidet.

    % superset(Superset, Set) is true iff Superset is a superset of Set.
    % Same as `intersect(Superset, Set, Set)', but may be more efficient.
    %
:- pred superset(fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::in)
    is semidet.

%---------------------------------------------------------------------------%
%
% Operations on two or more sets.
%

    % union(SetA, SetB) returns the union of SetA and SetB. The
    % efficiency of the union operation is not sensitive to the argument
    % ordering. Takes O(rep_size(SetA) + rep_size(SetB)) time and space.
    %
:- func union(fatter_sparse_bitset(T), fatter_sparse_bitset(T))
    = fatter_sparse_bitset(T).
:- pred union(fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::in,
    fatter_sparse_bitset(T)::out) is det.

    % union_list(Sets, Set) returns the union of all the sets in Sets.
    %
:- func union_list(list(fatter_sparse_bitset(T))) = fatter_sparse_bitset(T).
:- pred union_list(list(fatter_sparse_bitset(T))::in,
    fatter_sparse_bitset(T)::out) is det.

    % intersect(SetA, SetB) returns the intersection of SetA and SetB.
    % The efficiency of the intersection operation is not sensitive to the
    % argument ordering. Takes O(rep_size(SetA) + rep_size(SetB)) time and
    % O(min(rep_size(SetA)), rep_size(SetB)) space.
    %
:- func intersect(fatter_sparse_bitset(T), fatter_sparse_bitset(T))
    = fatter_sparse_bitset(T).
:- pred intersect(fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::in,
    fatter_sparse_bitset(T)::out) is det.

    % intersect_list(Sets, Set) returns the intersection of all the sets
    % in Sets.
    %
:- func intersect_list(list(fatter_sparse_bitset(T))) =
    fatter_sparse_bitset(T).
:- pred intersect_list(list(fatter_sparse_bitset(T))::in,
    fatter_sparse_bitset(T)::out) is det.

    % difference(SetA, SetB) returns the set containing all the elements
    % of SetA except those that occur in SetB. Takes
    % O(rep_size(SetA) + rep_size(SetB)) time and O(rep_size(SetA)) space.
    %
:- func difference(fatter_sparse_bitset(T), fatter_sparse_bitset(T))
    = fatter_sparse_bitset(T).
:- pred difference(fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::in,
    fatter_sparse_bitset(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Operations that divide a set into two parts.
%

    % divide(Pred, Set, InPart, OutPart):
    % InPart consists of those elements of Set for which Pred succeeds;
    % OutPart consists of those elements of Set for which Pred fails.
    %
:- pred divide(pred(T)::in(pred(in) is semidet), fatter_sparse_bitset(T)::in,
    fatter_sparse_bitset(T)::out, fatter_sparse_bitset(T)::out) is det
    <= uenum(T).

    % divide_by_set(DivideBySet, Set, InPart, OutPart):
    % InPart consists of those elements of Set which are also in DivideBySet;
    % OutPart consists of those elements of Set which are not in DivideBySet.
    %
:- pred divide_by_set(fatter_sparse_bitset(T)::in, fatter_sparse_bitset(T)::in,
    fatter_sparse_bitset(T)::out, fatter_sparse_bitset(T)::out) is det
    <= uenum(T).

%---------------------------------------------------------------------------%
%
% Converting lists to sets.
%

    % list_to_set(List) returns a set containing only the members of List.
    % In the worst case this will take O(length(List)^2) time and space.
    % If the elements of the list are closely grouped, it will be closer
    % to O(length(List)).
    %
:- func list_to_set(list(T)) = fatter_sparse_bitset(T) <= uenum(T).
:- pred list_to_set(list(T)::in, fatter_sparse_bitset(T)::out) is det
    <= uenum(T).

    % sorted_list_to_set(List) returns a set containing only the members
    % of List. List must be sorted. Takes O(length(List)) time and space.
    %
:- func sorted_list_to_set(list(T)) = fatter_sparse_bitset(T) <= uenum(T).
:- pred sorted_list_to_set(list(T)::in, fatter_sparse_bitset(T)::out)
    is det <= uenum(T).

%---------------------------------------------------------------------------%
%
% Converting sets to lists.
%

    % to_sorted_list(Set) returns a list containing all the members of Set,
    % in sorted order. Takes O(card(Set)) time and space.
    %
:- func to_sorted_list(fatter_sparse_bitset(T)) = list(T) <= uenum(T).
:- pred to_sorted_list(fatter_sparse_bitset(T)::in, list(T)::out) is det
    <= uenum(T).

%---------------------------------------------------------------------------%
%
% Converting between different kinds of sets.
%

    % from_set(Set) returns a bitset containing only the members of Set.
    % Takes O(card(Set)) time and space.
    %
:- func from_set(set.set(T)) = fatter_sparse_bitset(T) <= uenum(T).

    % to_set(Set) returns a set.set containing all the members of Set,
    % Takes O(card(Set)) time and space.
    %
:- func to_set(fatter_sparse_bitset(T)) = set.set(T) <= uenum(T).

%---------------------------------------------------------------------------%
%
% Counting.
%

    % count(Set) returns the number of elements in Set.
    % Takes O(card(Set)) time.
    %
:- func count(fatter_sparse_bitset(T)) = int <= uenum(T).

%---------------------------------------------------------------------------%
%
% Standard higher order functions on collections.
%

    % all_true(Pred, Set) succeeds iff Pred(Element) succeeds
    % for all the elements of Set.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet), fatter_sparse_bitset(T)::in)
    is semidet <= uenum(T).

    % filter(Pred, Set) returns the elements of Set for which Pred succeeds.
    %
:- func filter(pred(T)::in(pred(in) is semidet), fatter_sparse_bitset(T)::in)
    = (fatter_sparse_bitset(T)::out) is det <= uenum(T).

    % filter(Pred, Set, TrueSet, FalseSet) returns the elements of Set
    % for which Pred succeeds, and those for which it fails.
    %
:- pred filter(pred(T)::in(pred(in) is semidet),
    fatter_sparse_bitset(T)::in,
    fatter_sparse_bitset(T)::out, fatter_sparse_bitset(T)::out) is det
    <= uenum(T).

    % foldl(Func, Set, Start) calls Func with each element of Set
    % (in sorted order) and an accumulator (with the initial value of Start),
    % and returns the final value. Takes O(card(Set)) time.
    %
:- func foldl(func(T, U) = U, fatter_sparse_bitset(T), U) = U <= uenum(T).

:- pred foldl(pred(T, U, U), fatter_sparse_bitset(T), U, U) <= uenum(T).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode foldl(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode foldl(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.
:- mode foldl(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.

:- pred foldl2(pred(T, U, U, V, V), fatter_sparse_bitset(T), U, U, V, V)
    <= uenum(T).
:- mode foldl2(pred(in, in, out, in, out) is det, in, in, out, in, out) is det.
:- mode foldl2(pred(in, in, out, mdi, muo) is det, in, in, out, mdi, muo)
    is det.
:- mode foldl2(pred(in, in, out, di, uo) is det, in, in, out, di, uo) is det.
:- mode foldl2(pred(in, di, uo, di, uo) is det, in, di, uo, di, uo) is det.
:- mode foldl2(pred(in, in, out, in, out) is semidet, in, in, out, in, out)
    is semidet.
:- mode foldl2(pred(in, in, out, mdi, muo) is semidet, in, in, out, mdi, muo)
    is semidet.
:- mode foldl2(pred(in, in, out, di, uo) is semidet, in, in, out, di, uo)
    is semidet.
:- mode foldl2(pred(in, in, out, in, out) is nondet, in, in, out, in, out)
    is nondet.
:- mode foldl2(pred(in, in, out, in, out) is cc_multi, in, in, out, in, out)
    is cc_multi.
:- mode foldl2(pred(in, in, out, di, uo) is cc_multi, in, in, out, di, uo)
    is cc_multi.
:- mode foldl2(pred(in, di, uo, di, uo) is cc_multi, in, di, uo, di, uo)
    is cc_multi.

    % foldr(Func, Set, Start) calls Func with each element of Set
    % (in reverse sorted order) and an accumulator (with the initial value
    % of Start), and returns the final value. Takes O(card(Set)) time.
    %
:- func foldr(func(T, U) = U, fatter_sparse_bitset(T), U) = U <= uenum(T).

:- pred foldr(pred(T, U, U), fatter_sparse_bitset(T), U, U) <= uenum(T).
:- mode foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode foldr(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldr(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldr(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldr(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode foldr(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode foldr(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.
:- mode foldr(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.

:- pred foldr2(pred(T, U, U, V, V), fatter_sparse_bitset(T), U, U, V, V)
    <= uenum(T).
:- mode foldr2(pred(in, in, out, in, out) is det, in, in, out, in, out) is det.
:- mode foldr2(pred(in, in, out, mdi, muo) is det, in, in, out, mdi, muo)
    is det.
:- mode foldr2(pred(in, in, out, di, uo) is det, in, in, out, di, uo) is det.
:- mode foldr2(pred(in, di, uo, di, uo) is det, in, di, uo, di, uo) is det.
:- mode foldr2(pred(in, in, out, in, out) is semidet, in, in, out, in, out)
    is semidet.
:- mode foldr2(pred(in, in, out, mdi, muo) is semidet, in, in, out, mdi, muo)
    is semidet.
:- mode foldr2(pred(in, in, out, di, uo) is semidet, in, in, out, di, uo)
    is semidet.
:- mode foldr2(pred(in, in, out, in, out) is nondet, in, in, out, in, out)
    is nondet.
:- mode foldr2(pred(in, di, uo, di, uo) is cc_multi, in, di, uo, di, uo)
    is cc_multi.
:- mode foldr2(pred(in, in, out, di, uo) is cc_multi, in, in, out, di, uo)
    is cc_multi.
:- mode foldr2(pred(in, in, out, in, out) is cc_multi, in, in, out, in, out)
    is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

:- pragma type_spec(pred(singleton_set/2), T = var(_)).
:- pragma type_spec(pred(singleton_set/2), T = uint).

:- pragma type_spec(func(make_singleton_set/1), T = var(_)).
:- pragma type_spec(func(make_singleton_set/1), T = uint).

:- pragma type_spec(pred(contains/2), T = var(_)).
:- pragma type_spec(pred(contains/2), T = uint).

:- pragma type_spec(func(insert/2), T = var(_)).
:- pragma type_spec(func(insert/2), T = uint).
:- pragma type_spec(pred(insert/3), T = var(_)).
:- pragma type_spec(pred(insert/3), T = uint).

:- pragma type_spec(func(insert_list/2), T = var(_)).
:- pragma type_spec(func(insert_list/2), T = uint).
:- pragma type_spec(pred(insert_list/3), T = var(_)).
:- pragma type_spec(pred(insert_list/3), T = uint).

:- pragma type_spec(func(delete/2), T = var(_)).
:- pragma type_spec(func(delete/2), T = uint).
:- pragma type_spec(pred(delete/3), T = var(_)).
:- pragma type_spec(pred(delete/3), T = uint).

:- pragma type_spec(func(delete_list/2), T = var(_)).
:- pragma type_spec(func(delete_list/2), T = uint).
:- pragma type_spec(pred(delete_list/3), T = var(_)).
:- pragma type_spec(pred(delete_list/3), T = uint).

:- pragma type_spec(func(list_to_set/1), T = var(_)).
:- pragma type_spec(func(list_to_set/1), T = uint).
:- pragma type_spec(pred(list_to_set/2), T = var(_)).
:- pragma type_spec(pred(list_to_set/2), T = uint).

:- pragma type_spec(func(sorted_list_to_set/1), T = var(_)).
:- pragma type_spec(func(sorted_list_to_set/1), T = uint).
:- pragma type_spec(pred(sorted_list_to_set/2), T = var(_)).
:- pragma type_spec(pred(sorted_list_to_set/2), T = uint).

:- pragma type_spec(func(to_sorted_list/1), T = var(_)).
:- pragma type_spec(func(to_sorted_list/1), T = uint).
:- pragma type_spec(pred(to_sorted_list/2), T = var(_)).
:- pragma type_spec(pred(to_sorted_list/2), T = uint).

:- pragma type_spec(func(to_set/1), T = var(_)).
:- pragma type_spec(func(to_set/1), T = uint).

:- pragma type_spec(func(from_set/1), T = var(_)).
:- pragma type_spec(func(from_set/1), T = uint).

:- pragma type_spec(func(foldl/3), T = uint).
:- pragma type_spec(func(foldl/3), T = var(_)).

:- pragma type_spec(pred(foldl/4), T = uint).
:- pragma type_spec(pred(foldl/4), T = var(_)).

:- pragma type_spec(func(foldr/3), T = uint).
:- pragma type_spec(func(foldr/3), T = var(_)).

:- pragma type_spec(pred(foldr/4), T = uint).
:- pragma type_spec(pred(foldr/4), T = var(_)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module uint.

%---------------------------------------------------------------------------%

:- type fatter_sparse_bitset(T)    % <= uenum(T)
    --->    fatter_sparse_bitset(bitset_elems).

    % The list of bitset_elems, sorted on offset in strictly ascending order.
    % Cells of this type should only be constructed using make_bitset_cons/3.
:- type bitset_elems
    --->    bitset_nil
    ;       bitset_cons(
                % This must be a multiple of 2 * ubits_per_uint.
                offset  :: uint,

                % Bit i of the bits_lo field, for i in [0, ubits_per_uint),
                % specifies whether the element at index
                % offset+i is in the set or not.
                %
                % Bit i of the bits_hi field, for i in [0, ubits_per_uint),
                % specifies whether the element at index
                % offset+ubits_per_uint+i is in the set or not.
                %
                % All fatter_sparse_bitset operations should remove
                % all elements of the list where both these fields are zero.
                bits_lo :: uint,
                bits_hi :: uint,

                % The rest of the set, whose offsets must all be strictly
                % larger than the offset in this cons cell.
                tail    :: bitset_elems
            ).

%---------------------------------------------------------------------------%

init = fatter_sparse_bitset(bitset_nil).

init(fatter_sparse_bitset(bitset_nil)).

singleton_set(make_singleton_set(A), A).

make_singleton_set(A) = insert(init, A).

%---------------------------------------------------------------------------%

is_empty(fatter_sparse_bitset(bitset_nil)).

is_non_empty(fatter_sparse_bitset(bitset_cons(_, _, _, _))).

is_singleton(Set, Item) :-
    Set = fatter_sparse_bitset(Elems),
    Elems = bitset_cons(Offset, BitsLo, BitsHi, bitset_nil),
    find_offsets_of_set_bits(Offset, ubits_per_uint, BitsLo,
        [], SetOffsetsLo),
    find_offsets_of_set_bits(Offset + ubits_per_uint, ubits_per_uint, BitsHi,
        SetOffsetsLo, SetOffsets),
    SetOffsets = [SetOffset],
    ( if from_uint(SetOffset, ItemPrime) then
        Item = ItemPrime
    else
        % We only apply from_uint/2 to integers returned
        % by to_uint/1, so it should never fail.
        unexpected($pred, "`enum.from_uint/2' failed")
    ).

    % find_offsets_of_set_bits(BitOffset, Size, Bits, !SetOffsets):
    %
    % Accumulate the offsets of the set bits in the given Bits,
    % whose size is Size bits and whose initial offset is BitOffset.
    % We do this via successive binary partitions, since this can skip
    % e.g. a byte's worth of clear bits without examining them one by one.
    %
:- pred find_offsets_of_set_bits(uint::in, uint::in, uint::in,
    list(uint)::in, list(uint)::out) is det.

find_offsets_of_set_bits(BitOffset, Size, Bits, !SetOffsets) :-
    ( if Bits = 0u then
        true
    else if Size = 1u then
        % If Bits were 0, we wouldn't have got here.
        !:SetOffsets = [BitOffset | !.SetOffsets]
    else
        HalfSize = unchecked_right_ushift(Size, 1u),
        Mask = mask(HalfSize),

        % Extract the low-order half of the bits.
        LowBits = Mask /\ Bits,

        % Extract the high-order half of the bits.
        HighBits = Mask /\ unchecked_right_ushift(Bits, HalfSize),

        find_offsets_of_set_bits(BitOffset, HalfSize, LowBits, !SetOffsets),
        find_offsets_of_set_bits(BitOffset + HalfSize, HalfSize, HighBits,
            !SetOffsets)
    ).

%---------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(pred(member/2)).

member(Item::in, Set::in) :-
    contains(Set, Item).
member(Item::out, fatter_sparse_bitset(Elems)::in) :-
    member_search_nodes(Index, Elems),
    ( if from_uint(Index, ItemPrime) then
        Item = ItemPrime
    else
        % We only apply from_uint/1 to integers returned
        % by to_uint/1, so it should never fail.
        unexpected($pred, "`enum.from_uint/2' failed")
    ).

:- pred member_search_nodes(uint::out, bitset_elems::in) is nondet.

member_search_nodes(Index, bitset_cons(Offset, BitsLo, BitsHi, Tail)) :-
    (
        member_search_one_node(Index, Offset, ubits_per_uint, BitsLo)
    ;
        member_search_one_node(Index, Offset + ubits_per_uint,
            ubits_per_uint, BitsHi)
    ;
        member_search_nodes(Index, Tail)
    ).

:- pred member_search_one_node(uint::out, uint::in, uint::in, uint::in)
    is nondet.

member_search_one_node(Index, Offset, Size, Bits) :-
    ( if Bits = 0u then
        fail
    else if Size = 1u then
        Index = Offset
    else
        HalfSize = unchecked_right_ushift(Size, 1u),
        Mask = mask(HalfSize),

        % Extract the low-order half of the bits.
        LowBits = Mask /\ Bits,

        % Extract the high-order half of the bits.
        HighBits = Mask /\ unchecked_right_ushift(Bits, HalfSize),

        ( member_search_one_node(Index, Offset, HalfSize, LowBits)
        ; member_search_one_node(Index, Offset + HalfSize, HalfSize, HighBits)
        )
    ).

%---------------------%

contains(fatter_sparse_bitset(Elems), Item) :-
    ItemIndex = enum.to_uint(Item),
    offset_and_bit_to_set_for_index(ItemIndex, ItemOffset, ItemBitToSet),
    contains_search_nodes(Elems, ItemOffset, ItemBitToSet).

:- pred contains_search_nodes(bitset_elems::in, uint::in, uint::in) is semidet.

contains_search_nodes(Elems, ItemOffset, ItemBitToSet) :-
    Elems = bitset_cons(Offset, BitsLo, BitsHi, Tail),
    ItemOffset >= Offset,
    ( if ItemOffset = Offset then
        get_bit(BitsLo, BitsHi, ItemBitToSet) \= 0u
    else
        contains_search_nodes(Tail, ItemOffset, ItemBitToSet)
    ).

%---------------------------------------------------------------------------%

insert(Set0, Item) = Set :-
    insert(Item, Set0, Set).

insert(Item, fatter_sparse_bitset(Elems0), fatter_sparse_bitset(Elems)) :-
    ItemIndex = enum.to_uint(Item),
    offset_and_bit_to_set_for_index(ItemIndex, ItemOffset, ItemBitToSet),
    insert_loop(ItemOffset, ItemBitToSet, Elems0, Elems).

:- pred insert_loop(uint::in, uint::in,
    bitset_elems::in, bitset_elems::out) is det.

insert_loop(ItemOffset, ItemBitToSet, Elems0, Elems) :-
    (
        Elems0 = bitset_nil,
        set_bit(ItemBitToSet, 0u, ItemBitsLo, 0u, ItemBitsHi),
        Elems =
            make_bitset_cons(ItemOffset, ItemBitsLo, ItemBitsHi, bitset_nil)
    ;
        Elems0 = bitset_cons(Offset0, BitsLo0, BitsHi0, Tail0),
        ( if ItemOffset < Offset0 then
            % The insertion is before the front node of Elems0.
            set_bit(ItemBitToSet, 0u, ItemBitsLo, 0u, ItemBitsHi),
            Elems =
                make_bitset_cons(ItemOffset, ItemBitsLo, ItemBitsHi, Elems0)
        else if ItemOffset = Offset0 then
            % The insertion is to the front node of Elems0.
            ( if get_bit(BitsLo0, BitsHi0, ItemBitToSet) = 0u then
                set_bit(ItemBitToSet, BitsLo0, BitsLo, BitsHi0, BitsHi),
                Elems = make_bitset_cons(Offset0, BitsLo, BitsHi, Tail0)
            else
                Elems = Elems0
            )
        else
            % The insertion is after the front node of Elems0.
            insert_loop(ItemOffset, ItemBitToSet, Tail0, Tail),
            Elems = make_bitset_cons(Offset0, BitsLo0, BitsHi0, Tail)
        )
    ).

%---------------------%

insert_new(Item, fatter_sparse_bitset(Elems0), fatter_sparse_bitset(Elems)) :-
    ItemIndex = enum.to_uint(Item),
    offset_and_bit_to_set_for_index(ItemIndex, ItemOffset, ItemBitToSet),
    insert_new_loop(ItemOffset, ItemBitToSet, Elems0, Elems).

:- pred insert_new_loop(uint::in, uint::in,
    bitset_elems::in, bitset_elems::out) is semidet.

insert_new_loop(ItemOffset, ItemBitToSet, Elems0, Elems) :-
    (
        Elems0 = bitset_nil,
        set_bit(ItemBitToSet, 0u, ItemBitsLo, 0u, ItemBitsHi),
        Elems =
            make_bitset_cons(ItemOffset, ItemBitsLo, ItemBitsHi, bitset_nil)
    ;
        Elems0 = bitset_cons(Offset0, BitsLo0, BitsHi0, Tail0),
        ( if ItemOffset < Offset0 then
            % The insertion is before the front node of Elems0.
            set_bit(ItemBitToSet, 0u, ItemBitsLo, 0u, ItemBitsHi),
            Elems =
                make_bitset_cons(ItemOffset, ItemBitsLo, ItemBitsHi, Elems0)
        else if ItemOffset = Offset0 then
            % The insertion is to the front node of Elems0.
            ( if get_bit(BitsLo0, BitsHi0, ItemBitToSet) = 0u then
                set_bit(ItemBitToSet, BitsLo0, BitsLo, BitsHi0, BitsHi),
                Elems = make_bitset_cons(Offset0, BitsLo, BitsHi, Tail0)
            else
                fail
            )
        else
            % The insertion is after the front node of Elems0.
            insert_new_loop(ItemOffset, ItemBitToSet, Tail0, Tail),
            Elems = make_bitset_cons(Offset0, BitsLo0, BitsHi0, Tail)
        )
    ).

%---------------------%

insert_list(Set0, List) = Set :-
    insert_list(List, Set0, Set).

insert_list(List, Set0, Set) :-
    union(list_to_set(List), Set0, Set).

%---------------------%

delete(Set0, Item) = Set :-
    delete(Item, Set0, Set).

delete(Item, !Set) :-
    difference(!.Set, make_singleton_set(Item), !:Set).

delete_list(Set0, Items) = Set :-
    delete_list(Items, Set0, Set).

delete_list(Items, !Set) :-
    difference(!.Set, list_to_set(Items), !:Set).

%---------------------%

remove(Item, !Set) :-
    contains(!.Set, Item),
    difference(!.Set, make_singleton_set(Item), !:Set).

remove_list(Items, !Set) :-
    list_to_set(Items, ItemsSet),
    subset(ItemsSet, !.Set),
    difference(!.Set, ItemsSet, !:Set).

%---------------------%

remove_leq(Set0, Item) = Set :-
    remove_leq(Item, Set0, Set).

remove_leq(Item, fatter_sparse_bitset(Elems0), fatter_sparse_bitset(Elems)) :-
    Index = enum.to_uint(Item),
    offset_and_bit_to_set_for_index(Index, IndexOffset, IndexBit),
    remove_leq_loop(IndexOffset, IndexBit, Elems0, Elems).

:- pred remove_leq_loop(uint::in, uint::in,
    bitset_elems::in, bitset_elems::out) is det.

remove_leq_loop(_, _, bitset_nil, bitset_nil).
remove_leq_loop(IndexOffset, IndexBit,
        Elems0 @ bitset_cons(Offset, BitsLo0, BitsHi0, Tail0), Elems) :-
    ( if Offset < IndexOffset then
        remove_leq_loop(IndexOffset, IndexBit, Tail0, Elems)
    else if Offset = IndexOffset then
        ( if IndexBit < ubits_per_uint then
            BitsLo = BitsLo0 /\ unchecked_left_ushift(\ 0u, IndexBit + 1u),
            BitsHi = BitsHi0,
            ( if BitsLo = 0u, BitsHi = 0u then
                Elems = Tail0
            else
                Elems = make_bitset_cons(Offset, BitsLo, BitsHi, Tail0)
            )
        else
            BitsLo = 0u,
            BitsHi = BitsHi0 /\
                unchecked_left_ushift(\ 0u, IndexBit - ubits_per_uint + 1u),
            ( if BitsHi = 0u then
                Elems = Tail0
            else
                Elems = make_bitset_cons(Offset, BitsLo, BitsHi, Tail0)
            )
        )
    else
        Elems = Elems0
    ).

%---------------------%

remove_gt(Set0, Item) = Set :-
    remove_gt(Item, Set0, Set).

remove_gt(Item, fatter_sparse_bitset(Elems0), fatter_sparse_bitset(Elems)) :-
    Index = enum.to_uint(Item),
    offset_and_bit_to_set_for_index(Index, IndexOffset, IndexBit),
    remove_gt_loop(IndexOffset, IndexBit, Elems0, Elems).

:- pred remove_gt_loop(uint::in, uint::in,
    bitset_elems::in, bitset_elems::out) is det.

remove_gt_loop(_, _, bitset_nil, bitset_nil).
remove_gt_loop(IndexOffset, IndexBit,
        bitset_cons(Offset, BitsLo0, BitsHi0, Tail0), Elems) :-
    ( if Offset < IndexOffset then
        remove_gt_loop(IndexOffset, IndexBit, Tail0, Tail),
        Elems = make_bitset_cons(Offset, BitsLo0, BitsHi0, Tail)
    else if Offset = IndexOffset then
        ( if IndexBit < ubits_per_uint then
            BitsLo = BitsLo0 /\ \ unchecked_left_ushift(\ 0u, IndexBit + 1u),
            BitsHi = 0u,
            ( if BitsLo = 0u then
                Elems = bitset_nil
            else
                Elems = make_bitset_cons(Offset, BitsLo, BitsHi, bitset_nil)
            )
        else
            BitsLo = BitsLo0,
            BitsHi = BitsHi0 /\
                \ unchecked_left_ushift(\ 0u, IndexBit - ubits_per_uint + 1u),
            ( if BitsLo = 0u, BitsHi = 0u then
                Elems = bitset_nil
            else
                Elems = make_bitset_cons(Offset, BitsLo, BitsHi, bitset_nil)
            )
        )
    else
        Elems = bitset_nil
    ).

%---------------------%

remove_least(Item, Set0, Set) :-
    Set0 = fatter_sparse_bitset(Elems0),
    Elems0 = bitset_cons(Offset, BitsLo0, BitsHi0, Tail0),
    ( if BitsLo0 = 0u then
        BitsLo = 0u,
        Bit = find_least_bit(BitsHi0),
        Item = det_from_uint(Offset + ubits_per_uint + Bit),
        clear_bit(Bit, BitsHi0, BitsHi)
    else
        Bit = find_least_bit(BitsLo0),
        Item = det_from_uint(Offset + Bit),
        clear_bit(Bit, BitsLo0, BitsLo),
        BitsHi = BitsHi0
    ),
    ( if BitsLo = 0u, BitsHi = 0u then
        Elems = Tail0
    else
        Elems = make_bitset_cons(Offset, BitsLo, BitsHi, Tail0)
    ),
    Set = fatter_sparse_bitset(Elems).

:- func find_least_bit(uint) = uint.

find_least_bit(Bits0) = BitNum :-
    Size = ubits_per_uint,
    BitNum0 = 0u,
    find_least_bit_loop(Bits0, Size, BitNum0, BitNum).

:- pred find_least_bit_loop(uint::in, uint::in, uint::in, uint::out) is det.

find_least_bit_loop(Bits0, Size, BitNum0, BitNum) :-
    ( if Size = 1u then
        % We can't get here unless the bit is a 1 bit.
        BitNum = BitNum0
    else
        HalfSize = unchecked_right_ushift(Size, 1u),
        Mask = mask(HalfSize),

        LowBits = Bits0 /\ Mask,
        ( if LowBits = 0u then
            HighBits = Mask /\ unchecked_right_ushift(Bits0, HalfSize),
            find_least_bit_loop(HighBits, HalfSize, BitNum0 + HalfSize, BitNum)
        else
            find_least_bit_loop(LowBits, HalfSize, BitNum0, BitNum)
        )
    ).

%---------------------------------------------------------------------------%

equal(X, X).

subset(Subset, Set) :-
    intersect(Set, Subset, Subset).

superset(Superset, Set) :-
    subset(Set, Superset).

%---------------------------------------------------------------------------%

union(SetA, SetB) = Set :-
    union(SetA, SetB, Set).

union(SetA, SetB, Set) :-
    SetA = fatter_sparse_bitset(ElemsA),
    SetB = fatter_sparse_bitset(ElemsB),
    union_loop(ElemsA, ElemsB, Elems),
    Set = fatter_sparse_bitset(Elems).

:- pred union_loop(bitset_elems::in, bitset_elems::in, bitset_elems::out)
    is det.

union_loop(bitset_nil, ElemsB, ElemsB).
union_loop(ElemsA @ bitset_cons(_, _, _, _), bitset_nil, ElemsA).
union_loop(ElemsA, ElemsB, Elems) :-
    ElemsA = bitset_cons(OffsetA, BitsLoA, BitsHiA, TailA),
    ElemsB = bitset_cons(OffsetB, BitsLoB, BitsHiB, TailB),
    ( if OffsetA = OffsetB then
        union_loop(TailA, TailB, Tail),
        BitsLo = BitsLoA \/ BitsLoB,
        BitsHi = BitsHiA \/ BitsHiB,
        Elems = make_bitset_cons(OffsetA, BitsLo, BitsHi, Tail)
    else if OffsetA < OffsetB then
        union_loop(TailA, ElemsB, Tail),
        Elems = make_bitset_cons(OffsetA, BitsLoA, BitsHiA, Tail)
    else
        union_loop(ElemsA, TailB, Tail),
        Elems = make_bitset_cons(OffsetB, BitsLoB, BitsHiB, Tail)
    ).

%---------------------%

union_list(Sets) = Set :-
    union_list(Sets, Set).

union_list([], fatter_sparse_bitset.init).
union_list([Set | Sets], Union) :-
    union_list_passes(Set, Sets, Union).

    % Union the full list of sets via a sequence of passes, where each pass
    % replaces each group of (up to) four adjacent sets with one set.
    %
    % We keep invoking union_list_pass until it yields only one set.
    %
    % The point of this approach is that unioning a large set with a small set
    % is often only slightly faster than unioning that large set with another
    % large set, yet it gets significantly less work done. This is because
    % the bitsets in a small set can be expected to be considerably sparser
    % that bitsets in large sets.
    %
    % We expect that this approach should yield performance closer to NlogN
    % than to N^2 when unioning a list of N sets.
    %
:- pred union_list_passes(fatter_sparse_bitset(T)::in,
    list(fatter_sparse_bitset(T))::in, fatter_sparse_bitset(T)::out) is det.

union_list_passes(Set1, Sets2plus, Union) :-
    union_list_pass(Set1, Sets2plus, HeadUnion, TailUnions),
    (
        TailUnions = [],
        Union = HeadUnion
    ;
        TailUnions = [_ | _],
        union_list_passes(HeadUnion, TailUnions, Union)
    ).

:- pred union_list_pass(fatter_sparse_bitset(T)::in,
    list(fatter_sparse_bitset(T))::in,
    fatter_sparse_bitset(T)::out, list(fatter_sparse_bitset(T))::out) is det.

union_list_pass(Set1, Sets2plus, HeadUnion, TailUnions) :-
    (
        Sets2plus = [],
        HeadUnion = Set1,
        TailUnions = []
    ;
        Sets2plus = [Set2],
        HeadUnion = union(Set1, Set2),
        TailUnions = []
    ;
        Sets2plus = [Set2, Set3],
        HeadUnion = union(Set1, union(Set2, Set3)),
        TailUnions = []
    ;
        Sets2plus = [Set2, Set3, Set4],
        HeadUnion = union(union(Set1, Set2), union(Set3, Set4)),
        TailUnions = []
    ;
        Sets2plus = [Set2, Set3, Set4, Set5],
        HeadUnion = union(
            union(Set1, Set2),
            union(Set3, union(Set4, Set5))),
        TailUnions = []
    ;
        Sets2plus = [Set2, Set3, Set4, Set5, Set6],
        HeadUnion = union(
            union(Set1, union(Set2, Set3)),
            union(Set4, union(Set5, Set6))),
        TailUnions = []
    ;
        Sets2plus = [Set2, Set3, Set4, Set5, Set6, Set7],
        HeadUnion = union(
            union(Set1, union(Set2, Set3)),
            union(union(Set4, Set5), union(Set6, Set7))),
        TailUnions = []
    ;
        Sets2plus = [Set2, Set3, Set4, Set5, Set6, Set7, Set8],
        HeadUnion = union(
            union(union(Set1, Set2), union(Set3, Set4)),
            union(union(Set5, Set6), union(Set7, Set8))),
        TailUnions = []
    ;
        Sets2plus = [Set2, Set3, Set4, Set5, Set6, Set7, Set8, Set9 |
            Sets10plus],
        HeadUnion = union(
            union(union(Set1, Set2), union(Set3, Set4)),
            union(union(Set5, Set6), union(Set7, Set8))),
        union_list_pass(Set9, Sets10plus, HeadTailUnion, TailTailUnions),
        TailUnions = [HeadTailUnion | TailTailUnions]
    ).

%---------------------%

intersect(SetA, SetB) = Set :-
    intersect(SetA, SetB, Set).

intersect(SetA, SetB, Set) :-
    SetA = fatter_sparse_bitset(ElemsA),
    SetB = fatter_sparse_bitset(ElemsB),
    intersect_loop(ElemsA, ElemsB, Elems),
    Set = fatter_sparse_bitset(Elems).

:- pred intersect_loop(bitset_elems::in, bitset_elems::in, bitset_elems::out)
    is det.

intersect_loop(bitset_nil, _ElemsB, bitset_nil).
intersect_loop(bitset_cons(_, _, _, _), bitset_nil, bitset_nil).
intersect_loop(ElemsA, ElemsB, Elems) :-
    ElemsA = bitset_cons(OffsetA, BitsLoA, BitsHiA, TailA),
    ElemsB = bitset_cons(OffsetB, BitsLoB, BitsHiB, TailB),
    ( if OffsetA = OffsetB then
        BitsLo = BitsLoA /\ BitsLoB,
        BitsHi = BitsHiA /\ BitsHiB,
        ( if BitsLo = 0u, BitsHi = 0u then
            intersect_loop(TailA, TailB, Elems)
        else
            intersect_loop(TailA, TailB, Tail),
            Elems = make_bitset_cons(OffsetA, BitsLo, BitsHi, Tail)
        )
    else if OffsetA < OffsetB then
        intersect_loop(TailA, ElemsB, Elems)
    else
        intersect_loop(ElemsA, TailB, Elems)
    ).

%---------------------%

intersect_list(Sets) = Set :-
    intersect_list(Sets, Set).

intersect_list([], fatter_sparse_bitset.init).
intersect_list([Set | Sets], Section) :-
    intersect_list_passes(Set, Sets, Section).

    % Intersect the full list of sets via a sequence of passes, where each pass
    % replaces each group of (up to) four adjacent sets with one set.
    %
    % We keep invoking intersect_list_pass until it yields only one set.
    %
    % The point of this approach is that intersecting a large set with
    % a small set is often only slightly faster than intersecting
    % that large set with another large set, yet it gets significantly
    % less work done. This is because the bitsets in a small set
    % can be expected to be considerably sparser that bitsets in large sets.
    %
    % We expect that this approach should yield performance closer to NlogN
    % than to N^2 when unioning a list of N sets.
    %
:- pred intersect_list_passes(fatter_sparse_bitset(T)::in,
    list(fatter_sparse_bitset(T))::in, fatter_sparse_bitset(T)::out) is det.

intersect_list_passes(Set1, Sets2plus, Section) :-
    intersect_list_pass(Set1, Sets2plus, HeadSection, TailSection),
    (
        TailSection = [],
        Section = HeadSection
    ;
        TailSection = [_ | _],
        intersect_list_passes(HeadSection, TailSection, Section)
    ).

:- pred intersect_list_pass(fatter_sparse_bitset(T)::in,
    list(fatter_sparse_bitset(T))::in,
    fatter_sparse_bitset(T)::out, list(fatter_sparse_bitset(T))::out) is det.

intersect_list_pass(Set1, Sets2plus, HeadSection, TailSections) :-
    (
        Sets2plus = [],
        HeadSection = Set1,
        TailSections = []
    ;
        Sets2plus = [Set2],
        HeadSection = intersect(Set1, Set2),
        TailSections = []
    ;
        Sets2plus = [Set2, Set3],
        HeadSection = intersect(Set1, intersect(Set2, Set3)),
        TailSections = []
    ;
        Sets2plus = [Set2, Set3, Set4],
        HeadSection = intersect(intersect(Set1, Set2), intersect(Set3, Set4)),
        TailSections = []
    ;
        Sets2plus = [Set2, Set3, Set4, Set5],
        HeadSection = intersect(
            intersect(Set1, Set2),
            intersect(Set3, intersect(Set4, Set5))),
        TailSections = []
    ;
        Sets2plus = [Set2, Set3, Set4, Set5, Set6],
        HeadSection = intersect(
            intersect(Set1, intersect(Set2, Set3)),
            intersect(Set4, intersect(Set5, Set6))),
        TailSections = []
    ;
        Sets2plus = [Set2, Set3, Set4, Set5, Set6, Set7],
        HeadSection = intersect(
            intersect(Set1, intersect(Set2, Set3)),
            intersect(intersect(Set4, Set5), intersect(Set6, Set7))),
        TailSections = []
    ;
        Sets2plus = [Set2, Set3, Set4, Set5, Set6, Set7, Set8],
        HeadSection = intersect(
            intersect(intersect(Set1, Set2), intersect(Set3, Set4)),
            intersect(intersect(Set5, Set6), intersect(Set7, Set8))),
        TailSections = []
    ;
        Sets2plus = [Set2, Set3, Set4, Set5, Set6, Set7, Set8, Set9 |
            Sets10plus],
        HeadSection = intersect(
            intersect(intersect(Set1, Set2), intersect(Set3, Set4)),
            intersect(intersect(Set5, Set6), intersect(Set7, Set8))),
        intersect_list_pass(Set9, Sets10plus,
            HeadTailSection, TailTailSections),
        TailSections = [HeadTailSection | TailTailSections]
    ).

%---------------------%

difference(SetA, SetB) = Set :-
    difference(SetA, SetB, Set).

difference(SetA, SetB, Set) :-
    SetA = fatter_sparse_bitset(ElemsA),
    SetB = fatter_sparse_bitset(ElemsB),
    difference_loop(ElemsA, ElemsB, Elems),
    Set = fatter_sparse_bitset(Elems).

:- pred difference_loop(bitset_elems::in, bitset_elems::in, bitset_elems::out)
    is det.

difference_loop(bitset_nil, _ElemsB, bitset_nil).
difference_loop(ElemsA @ bitset_cons(_, _, _, _), bitset_nil, ElemsA).
difference_loop(ElemsA, ElemsB, Elems) :-
    ElemsA = bitset_cons(OffsetA, BitsLoA, BitsHiA, TailA),
    ElemsB = bitset_cons(OffsetB, BitsLoB, BitsHiB, TailB),
    ( if OffsetA = OffsetB then
        BitsLo = BitsLoA /\ \ BitsLoB,
        BitsHi = BitsHiA /\ \ BitsHiB,
        ( if BitsLo = 0u, BitsHi = 0u then
            difference_loop(TailA, TailB, Elems)
        else
            difference_loop(TailA, TailB, Tail),
            Elems = make_bitset_cons(OffsetA, BitsLo, BitsHi, Tail)
        )
    else if OffsetA < OffsetB then
        difference_loop(TailA, ElemsB, Tail),
        Elems = make_bitset_cons(OffsetA, BitsLoA, BitsHiA, Tail)
    else
        difference_loop(ElemsA, TailB, Elems)
    ).

%---------------------------------------------------------------------------%

divide(Pred, Set, InSet, OutSet) :-
    Set = fatter_sparse_bitset(Nodes),
    divide_nodes(Pred, Nodes, InNodes, OutNodes),
    InSet = fatter_sparse_bitset(InNodes),
    OutSet = fatter_sparse_bitset(OutNodes).

:- pred divide_nodes(pred(T)::in(pred(in) is semidet),
    bitset_elems::in, bitset_elems::out, bitset_elems::out) is det <= uenum(T).

divide_nodes(_Pred, bitset_nil, bitset_nil, bitset_nil).
divide_nodes(Pred, bitset_cons(Offset, BitsLo, BitsHi, Tail),
        InNodes, OutNodes) :-
    divide_nodes(Pred, Tail, InNodesTail, OutNodesTail),
    divide_bits(Pred, Offset, 0u, BitsLo, ubits_per_uint,
        0u, InLo, 0u, OutLo),
    divide_bits(Pred, Offset + ubits_per_uint, 0u, BitsHi, ubits_per_uint,
        0u, InHi, 0u, OutHi),
    ( if InLo = 0u, InHi = 0u then
        InNodes = InNodesTail
    else
        InNodes = make_bitset_cons(Offset, InLo, InHi, InNodesTail)
    ),
    ( if OutLo = 0u, OutHi = 0u then
        OutNodes = OutNodesTail
    else
        OutNodes = make_bitset_cons(Offset, OutLo, OutHi, OutNodesTail)
    ).

    % Do a binary search for the 1 bits in an int.
    %
:- pred divide_bits(pred(T)::in(pred(in) is semidet),
    uint::in, uint::in, uint::in, uint::in,
    uint::in, uint::out, uint::in, uint::out) is det <= uenum(T).

divide_bits(Pred, BaseOffset, OffsetInWord, Bits, Size, !In, !Out) :-
    ( if Bits = 0u then
        true
    else if Size = 1u then
        Elem = det_from_uint(BaseOffset + OffsetInWord),
        OffsetBit = unchecked_left_ushift(1u, OffsetInWord),
        ( if Pred(Elem) then
            !:In = !.In \/ OffsetBit
        else
            !:Out = !.Out \/ OffsetBit
        )
    else
        HalfSize = unchecked_right_ushift(Size, 1u),
        Mask = mask(HalfSize),

        % Extract the low-order half of the bits.
        LowBits = Mask /\ Bits,

        % Extract the high-order half of the bits.
        HighBits = Mask /\ unchecked_right_ushift(Bits, HalfSize),

        divide_bits(Pred, BaseOffset, OffsetInWord,
            LowBits, HalfSize, !In, !Out),
        divide_bits(Pred, BaseOffset, OffsetInWord + HalfSize,
            HighBits, HalfSize, !In, !Out)
    ).

%---------------------%

divide_by_set(DivideBySet, Set, InSet, OutSet) :-
    DivideBySet = fatter_sparse_bitset(DivideByNodes),
    Set = fatter_sparse_bitset(Nodes),
    divide_nodes_by_set(DivideByNodes, Nodes, InNodes, OutNodes),
    InSet = fatter_sparse_bitset(InNodes),
    OutSet = fatter_sparse_bitset(OutNodes).

:- pred divide_nodes_by_set(bitset_elems::in, bitset_elems::in,
    bitset_elems::out, bitset_elems::out) is det.

divide_nodes_by_set(_DivideByNodes, bitset_nil, bitset_nil, bitset_nil).
divide_nodes_by_set(bitset_nil, Nodes @ bitset_cons(_, _, _, _),
        bitset_nil, Nodes).
divide_nodes_by_set(DivideByNodes, Nodes, InNodes, OutNodes) :-
    DivideByNodes = bitset_cons(DivideByOffset,
        DivideByBitsLo, DivideByBitsHi, DivideByNodesTail),
    Nodes = bitset_cons(Offset, BitsLo, BitsHi, NodesTail),
    ( if DivideByOffset < Offset then
        divide_nodes_by_set(DivideByNodesTail, Nodes, InNodes, OutNodes)
    else if DivideByOffset > Offset then
        divide_nodes_by_set(DivideByNodes, NodesTail, InNodes, OutNodesTail),
        OutNodes = make_bitset_cons(Offset, BitsLo, BitsHi, OutNodesTail)
    else
        divide_nodes_by_set(DivideByNodesTail, NodesTail,
            InNodesTail, OutNodesTail),
        divide_bits_by_set(DivideByBitsLo, ubits_per_uint, 0u, BitsLo,
            0u, InLo, 0u, OutLo),
        divide_bits_by_set(DivideByBitsHi, ubits_per_uint, 0u, BitsHi,
            0u, InHi, 0u, OutHi),
        ( if InLo = 0u, InHi = 0u then
            InNodes = InNodesTail
        else
            InNodes = make_bitset_cons(Offset, InLo, InHi, InNodesTail)
        ),
        ( if OutLo = 0u, OutHi = 0u then
            OutNodes = OutNodesTail
        else
            OutNodes = make_bitset_cons(Offset, OutLo, OutHi, OutNodesTail)
        )
    ).

    % divide_bits_by_set(DivideByBits, Size, Offset, Bits, !In, !Out):
    %
    % The least-significant Size bits of Bits were originally at offsets
    % Offset .. Offset+Size-1 in a word that represents one node of Set.
    %
    % For each 1 bit in this word in its original position,
    % - if the corresponding bit in DivideByBits is 1, set that bit in !In;
    % - if the corresponding bit in DivideByBits is 0, set that bit in !Out.
    % For each 0 bit in this word in its original position,
    % - do nothing, since the corresponding element is not in Set.
    %
    % By doing a binary search for the 1 bits in the original word in Set,
    % we hope to avoid having to individually test every bit of the word.
    % However, if most bits in the original word in Set are 1, then our
    % approach may well be slower than a simple iteration through all the bits
    % in that word would be.
    %
:- pred divide_bits_by_set(uint::in, uint::in, uint::in, uint::in,
    uint::in, uint::out, uint::in, uint::out) is det.

divide_bits_by_set(DivideByBits, Size, Offset, Bits, !In, !Out) :-
    ( if Bits = 0u then
        true
    else if Size = 1u then
        OffsetBit = unchecked_left_ushift(1u, Offset),
        ( if DivideByBits /\ OffsetBit = 0u then
            !:Out = !.Out \/ OffsetBit
        else
            !:In = !.In \/ OffsetBit
        )
    else
        HalfSize = unchecked_right_ushift(Size, 1u),
        % XXX We could pass around Mask as a parameter, updating it
        % on each recursive call. That may be cheaper than what we do now.
        Mask = mask(HalfSize),

        % Extract the low-order half of the bits.
        LowBits = Mask /\ Bits,

        % Extract the high-order half of the bits.
        HighBits = Mask /\ unchecked_right_ushift(Bits, HalfSize),

        divide_bits_by_set(DivideByBits, HalfSize, Offset, LowBits,
            !In, !Out),
        divide_bits_by_set(DivideByBits, HalfSize, Offset + HalfSize, HighBits,
            !In, !Out)
    ).

%---------------------------------------------------------------------------%

list_to_set(ItemList) = Set :-
    list_to_set(ItemList, Set).

list_to_set(ItemList, Set) :-
    % The algorithm we use is a modified version of natural merge sort.
    %
    % Unlike with the usual version of natural merge sort, the enum values
    % of the items in a run don't have to be strictly ascending, because
    % the order in which the bits of a given bitset_elem are set does not
    % matter. For the purposes of finding ascending runs, the defining
    % characteristic of a run is that the *offsets* of the bitset_elems
    % to which the enum values of the items belong should not decrease.
    % Likewise, the criterion for descending runs is that the offsets
    % should not increase.
    %
    % This means that the typical runs discovered by list_to_set_get_runs
    % can be expected to be longer than the runs of a conventional
    % natural merge sort, unless the number of bits set in each bitset_elem
    % in each run is at, or just above, one.
    list_to_set_get_runs(ItemList, [], Runs),
    % Once we have a list of runs, union them all together using the
    % usual implementation of union_list, which is effectively a merge sort.
    % It can be significantly faster than usual merge sort, because a single
    % logical OR operation can merge up to ubits_per_uint items, though again,
    % this advantage goes away if the number of bits set in each bitset_elem
    % in the final result is at, or just above, one.
    union_list(Runs, Set).

:- pred list_to_set_get_runs(list(T)::in,
    list(fatter_sparse_bitset(T))::in, list(fatter_sparse_bitset(T))::out)
    is det <= uenum(T).
:- pragma type_spec(pred(list_to_set_get_runs/3), T = var(_)).
:- pragma type_spec(pred(list_to_set_get_runs/3), T = uint).

list_to_set_get_runs([], !Runs).
list_to_set_get_runs([HeadItem | TailItems], !Runs) :-
    bits_for_index(enum.to_uint(HeadItem), Offset, BitsLo0, BitsHi0),
    list_to_set_get_run(Offset, BitsLo0, BitsHi0, TailItems, LeftOverItems,
        RunElems),
    Run = fatter_sparse_bitset(RunElems),
    !:Runs = [Run | !.Runs],
    list_to_set_get_runs(LeftOverItems, !Runs).

    % list_to_set_get_run(Offset0, Bits0, Items, LeftOverItems, RunElems):
    %
    % Find in Items an initial subsequence of either ascending or descending
    % bitset_elems, and return them, in ascending form, in RunElems.
    % Each bitset_elem consists of items whose enum form maps to
    % the same offset.
    %
    % Return up the bitset_elems in the run in RunElems.
    % Return the items beyond the run in LeftOverItems.
    %
    % This predicate is agnostic about the direction of the run,
    % because it directly handles only the first bitset_elem.
    % Once it runs out of items that map to this bitset_elem,
    % the algorithm is forced use the enum value of the next item
    % to choose a direction. According, we delegate getting the rest
    % of the run to one of list_to_set_get_{ascending,descending}_run.
    %
:- pred list_to_set_get_run(uint::in, uint::in, uint::in,
    list(T)::in, list(T)::out, bitset_elems::out) is det <= uenum(T).
:- pragma type_spec(pred(list_to_set_get_run/6), T = var(_)).
:- pragma type_spec(pred(list_to_set_get_run/6), T = uint).

list_to_set_get_run(Offset0, BitsLo0, BitsHi0, [], [], RunElems) :-
    RunElems = make_bitset_cons(Offset0, BitsLo0, BitsHi0, bitset_nil).
list_to_set_get_run(Offset0, BitsLo0, BitsHi0, [HeadItem | TailItems],
        LeftOverItems, RunElems) :-
    HeadItemIndex = enum.to_uint(HeadItem),
    offset_and_bit_to_set_for_index(HeadItemIndex,
        HeadItemOffset, HeadItemBitToSet),
    ( if Offset0 = HeadItemOffset then
        set_bit(HeadItemBitToSet, BitsLo0, BitsLo1, BitsHi0, BitsHi1),
        list_to_set_get_run(Offset0, BitsLo1, BitsHi1,
            TailItems, LeftOverItems, RunElems)
    else if Offset0 < HeadItemOffset then
        RevRunElems0 = make_bitset_cons(Offset0, BitsLo0, BitsHi0, bitset_nil),
        set_bit(HeadItemBitToSet, 0u, BitsLo1, 0u, BitsHi1),
        list_to_set_get_ascending_run(HeadItemOffset, BitsLo1, BitsHi1,
            TailItems, LeftOverItems, RevRunElems0, RevRunElems),
        reverse_bitset_elems_acc(RevRunElems, bitset_nil, RunElems)
    else
        RunElems0 = make_bitset_cons(Offset0, BitsLo0, BitsHi0, bitset_nil),
        set_bit(HeadItemBitToSet, 0u, BitsLo1, 0u, BitsHi1),
        list_to_set_get_descending_run(HeadItemOffset, BitsLo1, BitsHi1,
            TailItems, LeftOverItems, RunElems0, RunElems)
    ).

:- pred list_to_set_get_ascending_run(uint::in, uint::in, uint::in,
    list(T)::in, list(T)::out, bitset_elems::in, bitset_elems::out)
    is det <= uenum(T).
:- pragma type_spec(pred(list_to_set_get_ascending_run/7), T = var(_)).
:- pragma type_spec(pred(list_to_set_get_ascending_run/7), T = uint).

list_to_set_get_ascending_run(Offset0, BitsLo0, BitsHi0,
        [], [], !RevRunElems) :-
    !:RevRunElems = make_bitset_cons(Offset0, BitsLo0, BitsHi0, !.RevRunElems).
list_to_set_get_ascending_run(Offset0, BitsLo0, BitsHi0,
        Items @ [HeadItem | TailItems], LeftOverItems, !RevRunElems) :-
    HeadItemIndex = enum.to_uint(HeadItem),
    offset_and_bit_to_set_for_index(HeadItemIndex,
        HeadItemOffset, HeadItemBitToSet),
    ( if Offset0 = HeadItemOffset then
        set_bit(HeadItemBitToSet, BitsLo0, BitsLo1, BitsHi0, BitsHi1),
        list_to_set_get_ascending_run(Offset0, BitsLo1, BitsHi1,
            TailItems, LeftOverItems, !RevRunElems)
    else if Offset0 < HeadItemOffset then
        !:RevRunElems =
            make_bitset_cons(Offset0, BitsLo0, BitsHi0, !.RevRunElems),
        set_bit(HeadItemBitToSet, 0u, BitsLo1, 0u, BitsHi1),
        list_to_set_get_ascending_run(HeadItemOffset, BitsLo1, BitsHi1,
            TailItems, LeftOverItems, !RevRunElems)
    else
        !:RevRunElems =
            make_bitset_cons(Offset0, BitsLo0, BitsHi0, !.RevRunElems),
        LeftOverItems = Items
    ).

:- pred list_to_set_get_descending_run(uint::in, uint::in, uint::in,
    list(T)::in, list(T)::out, bitset_elems::in, bitset_elems::out)
    is det <= uenum(T).
:- pragma type_spec(pred(list_to_set_get_descending_run/7), T = var(_)).
:- pragma type_spec(pred(list_to_set_get_descending_run/7), T = uint).

list_to_set_get_descending_run(Offset0, BitsLo0, BitsHi0, [], [], !RunElems) :-
    !:RunElems = make_bitset_cons(Offset0, BitsLo0, BitsHi0, !.RunElems).
list_to_set_get_descending_run(Offset0, BitsLo0, BitsHi0,
        Items @ [HeadItem | TailItems], LeftOverItems, !RunElems) :-
    HeadItemIndex = enum.to_uint(HeadItem),
    offset_and_bit_to_set_for_index(HeadItemIndex,
        HeadItemOffset, HeadItemBitToSet),
    ( if Offset0 = HeadItemOffset then
        set_bit(HeadItemBitToSet, BitsLo0, BitsLo1, BitsHi0, BitsHi1),
        list_to_set_get_descending_run(Offset0, BitsLo1, BitsHi1,
            TailItems, LeftOverItems, !RunElems)
    else if Offset0 > HeadItemOffset then
        !:RunElems = make_bitset_cons(Offset0, BitsLo0, BitsHi0, !.RunElems),
        set_bit(HeadItemBitToSet, 0u, BitsLo1, 0u, BitsHi1),
        list_to_set_get_descending_run(HeadItemOffset, BitsLo1, BitsHi1,
            TailItems, LeftOverItems, !RunElems)
    else
        !:RunElems = make_bitset_cons(Offset0, BitsLo0, BitsHi0, !.RunElems),
        LeftOverItems = Items
    ).

    % reverse_bitset_elems_acc(Xs, Ys, Zs):
    %
    % Zs = reverse(Xs) ++ Ys.
    %
:- pred reverse_bitset_elems_acc(bitset_elems::in,
    bitset_elems::in, bitset_elems::out) is det.

reverse_bitset_elems_acc(bitset_nil, Ys, Ys).
reverse_bitset_elems_acc(bitset_cons(XOffset, XBitsLo, XBitsHi, Xs), Ys, Zs) :-
    reverse_bitset_elems_acc(Xs, bitset_cons(XOffset, XBitsLo, XBitsHi, Ys),
        Zs).

%---------------------%

sorted_list_to_set(SortedList) = Set :-
    sorted_list_to_set(SortedList, Set).

sorted_list_to_set(SortedList, fatter_sparse_bitset(Elems)) :-
    (
        SortedList = [],
        Elems = bitset_nil
    ;
        SortedList = [HeadItem | TailItems],
        sorted_list_to_set_loop(HeadItem, TailItems, Offset, BitsLo, BitsHi,
            Elems0),
        Elems = make_bitset_cons(Offset, BitsLo, BitsHi, Elems0)
    ).

    % The first two input arguments represent a nonempty list of items, which
    % must be sorted on their index values. We convert this list to a set.
    % But since we process the tail of the list before its head, we are
    % constantly adding items to the front of the list. We therefore return
    % the front bitset_elem in the resulting set as an unboxed
    % <offset,bits_lo,bits_hi> tuple. This way, we delay constructing a
    % bitset_elem to add to the front of the list until we have processed
    % all the items whose bits are part of that node. Note that the returned
    % values of BitsLo and BitsHi cannot *both* be zero.
    %
    % XXX The fact that the recursive call is not *tail* recursive
    % is a problem when working with very long lists. For those,
    % it may be better to build up the elem list in reverse,
    % and unreverse it at the end.
    %
:- pred sorted_list_to_set_loop(T::in, list(T)::in,
    uint::out, uint::out, uint::out, bitset_elems::out) is det <= uenum(T).
:- pragma type_spec(pred(sorted_list_to_set_loop/6), T = var(_)).
:- pragma type_spec(pred(sorted_list_to_set_loop/6), T = uint).

sorted_list_to_set_loop(Item1, [], Offset, BitsLo, BitsHi, bitset_nil) :-
    bits_for_index(enum.to_uint(Item1), Offset, BitsLo, BitsHi).
sorted_list_to_set_loop(Item1, [Item2 | Items], Offset, BitsLo, BitsHi,
        Tail) :-
    sorted_list_to_set_loop(Item2, Items, Offset0, BitsLo0, BitsHi0, Tail0),
    bits_for_index(enum.to_uint(Item1), Offset1, BitsLo1, BitsHi1),
    ( if Offset1 = Offset0 then
        Offset = Offset1,
        BitsLo = BitsLo1 \/ BitsLo0,
        BitsHi = BitsHi1 \/ BitsHi0,
        Tail = Tail0
    else
        Offset = Offset1,
        BitsLo = BitsLo1,
        BitsHi = BitsHi1,
        Tail = make_bitset_cons(Offset0, BitsLo0, BitsHi0, Tail0)
    ).

%---------------------%

to_sorted_list(Set) = SortedList :-
    to_sorted_list(Set, SortedList).

to_sorted_list(Set, SortedList) :-
    SortedList = foldr(func(Item, Acc0) = [Item | Acc0], Set, []).

%---------------------------------------------------------------------------%

from_set(Set) = sorted_list_to_set(set.to_sorted_list(Set)).

to_set(Set) = set.sorted_list_to_set(to_sorted_list(Set)).

%---------------------------------------------------------------------------%

count(Set) = fatter_sparse_bitset.foldl((func(_, Acc) = Acc + 1), Set, 0).

%---------------------------------------------------------------------------%

all_true(Pred, fatter_sparse_bitset(Elems)) :-
    all_true_node(Pred, Elems).

:- pred all_true_node(pred(T)::in(pred(in) is semidet), bitset_elems::in)
    is semidet <= uenum(T).
:- pragma type_spec(pred(all_true_node/2), T = uint).
:- pragma type_spec(pred(all_true_node/2), T = var(_)).

all_true_node(_, bitset_nil).
all_true_node(Pred, bitset_cons(Offset, BitsLo, BitsHi, Tail)) :-
    all_true_bits(Pred, Offset, BitsLo, ubits_per_uint),
    all_true_bits(Pred, Offset + ubits_per_uint, BitsHi, ubits_per_uint),
    all_true_node(Pred, Tail).

:- pred all_true_bits(pred(T)::in(pred(in) is semidet),
    uint::in, uint::in, uint::in) is semidet <= uenum(T).
:- pragma type_spec(pred(all_true_bits/4), T = uint).
:- pragma type_spec(pred(all_true_bits/4), T = var(_)).

all_true_bits(Pred, Offset, Bits, Size) :-
    ( if Bits = 0u then
        true
    else if Size = 1u then
        Item = det_from_uint(Offset),
        Pred(Item)
    else
        HalfSize = unchecked_right_ushift(Size, 1u),
        Mask = mask(HalfSize),

        % Extract the low-order half of the bits.
        LowBits = Mask /\ Bits,

        % Extract the high-order half of the bits.
        HighBits = Mask /\ unchecked_right_ushift(Bits, HalfSize),

        all_true_bits(Pred, Offset, LowBits, HalfSize),
        all_true_bits(Pred, Offset + HalfSize, HighBits, HalfSize)
    ).

%---------------------%

% XXX We should make these more efficient.

filter(Pred, Set) = TrueSet :-
    SortedList = to_sorted_list(Set),
    SortedTrueList = list.filter(Pred, SortedList),
    TrueSet = sorted_list_to_set(SortedTrueList).

filter(Pred, Set, TrueSet, FalseSet) :-
    SortedList = to_sorted_list(Set),
    list.filter(Pred, SortedList, SortedTrueList, SortedFalseList),
    TrueSet = sorted_list_to_set(SortedTrueList),
    FalseSet = sorted_list_to_set(SortedFalseList).

%---------------------%

foldl(Func, fatter_sparse_bitset(Elems), Acc0) = Acc :-
    do_foldl_pred(
        ( pred(E::in, Acc1::in, Acc2::out) is det :-
            Acc2 = Func(E, Acc1)
        ), Elems, Acc0, Acc).

foldl(Pred, fatter_sparse_bitset(Elems), !Acc) :-
    do_foldl_pred(Pred, Elems, !Acc).

:- pred do_foldl_pred(pred(T, U, U), bitset_elems, U, U) <= uenum(T).
:- mode do_foldl_pred(pred(in, in, out) is det, in, in, out) is det.
:- mode do_foldl_pred(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode do_foldl_pred(pred(in, di, uo) is det, in, di, uo) is det.
:- mode do_foldl_pred(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode do_foldl_pred(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode do_foldl_pred(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode do_foldl_pred(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode do_foldl_pred(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.
:- mode do_foldl_pred(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.

:- pragma type_spec(pred(do_foldl_pred/4), T = uint).
:- pragma type_spec(pred(do_foldl_pred/4), T = var(_)).

do_foldl_pred(_, bitset_nil, !Acc).
do_foldl_pred(Pred, bitset_cons(Offset, BitsLo, BitsHi, Tail), !Acc) :-
    fold_bits_low_to_high(Pred, Offset, BitsLo,
        ubits_per_uint, !Acc),
    fold_bits_low_to_high(Pred, Offset + ubits_per_uint, BitsHi,
        ubits_per_uint, !Acc),
    do_foldl_pred(Pred, Tail, !Acc).

%---------------------%

foldl2(Pred, fatter_sparse_bitset(Elems), !Acc1, !Acc2) :-
    do_foldl2_pred(Pred, Elems, !Acc1, !Acc2).

:- pred do_foldl2_pred(pred(T, U, U, V, V), bitset_elems, U, U, V, V)
    <= uenum(T).
:- mode do_foldl2_pred(pred(in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode do_foldl2_pred(pred(in, in, out, mdi, muo) is det,
    in, in, out, mdi, muo) is det.
:- mode do_foldl2_pred(pred(in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode do_foldl2_pred(pred(in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode do_foldl2_pred(pred(in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode do_foldl2_pred(pred(in, in, out, mdi, muo) is semidet,
    in, in, out, mdi, muo) is semidet.
:- mode do_foldl2_pred(pred(in, in, out, di, uo) is semidet,
    in, in, out, di, uo) is semidet.
:- mode do_foldl2_pred(pred(in, in, out, in, out) is nondet,
    in, in, out, in, out) is nondet.
:- mode do_foldl2_pred(pred(in, di, uo, di, uo) is cc_multi,
    in, di, uo, di, uo) is cc_multi.
:- mode do_foldl2_pred(pred(in, in, out, di, uo) is cc_multi,
    in, in, out, di, uo) is cc_multi.
:- mode do_foldl2_pred(pred(in, in, out, in, out) is cc_multi,
    in, in, out, in, out) is cc_multi.

:- pragma type_spec(pred(do_foldl2_pred/6), T = uint).
:- pragma type_spec(pred(do_foldl2_pred/6), T = var(_)).

do_foldl2_pred(_, bitset_nil, !Acc1, !Acc2).
do_foldl2_pred(Pred, bitset_cons(Offset, BitsLo, BitsHi, Tail),
        !Acc1, !Acc2) :-
    fold2_bits_low_to_high(Pred, Offset, BitsLo,
        ubits_per_uint, !Acc1, !Acc2),
    fold2_bits_low_to_high(Pred, Offset + ubits_per_uint, BitsHi,
        ubits_per_uint, !Acc1, !Acc2),
    do_foldl2_pred(Pred, Tail, !Acc1, !Acc2).

%---------------------%

foldr(Func, fatter_sparse_bitset(Elems), Acc0) = Acc :-
    do_foldr_pred(
        ( pred(E::in, Acc1::in, Acc2::out) is det :-
            Acc2 = Func(E, Acc1)
        ), Elems, Acc0, Acc).

foldr(Pred, fatter_sparse_bitset(Elems), !Acc) :-
    do_foldr_pred(Pred, Elems, !Acc).

:- pred do_foldr_pred(pred(T, U, U), bitset_elems, U, U) <= uenum(T).
:- mode do_foldr_pred(pred(in, in, out) is det, in, in, out) is det.
:- mode do_foldr_pred(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode do_foldr_pred(pred(in, di, uo) is det, in, di, uo) is det.
:- mode do_foldr_pred(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode do_foldr_pred(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode do_foldr_pred(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode do_foldr_pred(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode do_foldr_pred(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode do_foldr_pred(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.

:- pragma type_spec(pred(do_foldr_pred/4), T = uint).
:- pragma type_spec(pred(do_foldr_pred/4), T = var(_)).

    % We don't just use list.foldr here because the overhead of allocating
    % the closure for fold_bits is significant for the compiler's runtime,
    % so it is best to avoid that even if `--optimize-higher-order' is not set.
do_foldr_pred(_, bitset_nil, !Acc).
do_foldr_pred(Pred, bitset_cons(Offset, BitsLo, BitsHi, Tail), !Acc) :-
    do_foldr_pred(Pred, Tail, !Acc),
    fold_bits_high_to_low(Pred, Offset + ubits_per_uint,
        BitsHi, ubits_per_uint, !Acc),
    fold_bits_high_to_low(Pred, Offset,
        BitsLo, ubits_per_uint, !Acc).

%---------------------%

foldr2(Pred, fatter_sparse_bitset(Elems), !Acc1, !Acc2) :-
    do_foldr2_pred(Pred, Elems, !Acc1, !Acc2).

:- pred do_foldr2_pred(pred(T, U, U, V, V), bitset_elems, U, U, V, V)
    <= uenum(T).
:- mode do_foldr2_pred(pred(in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode do_foldr2_pred(pred(in, in, out, mdi, muo) is det,
    in, in, out, mdi, muo) is det.
:- mode do_foldr2_pred(pred(in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode do_foldr2_pred(pred(in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode do_foldr2_pred(pred(in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode do_foldr2_pred(pred(in, in, out, mdi, muo) is semidet,
    in, in, out, mdi, muo) is semidet.
:- mode do_foldr2_pred(pred(in, in, out, di, uo) is semidet,
    in, in, out, di, uo) is semidet.
:- mode do_foldr2_pred(pred(in, in, out, in, out) is nondet,
    in, in, out, in, out) is nondet.
:- mode do_foldr2_pred(pred(in, di, uo, di, uo) is cc_multi,
    in, di, uo, di, uo) is cc_multi.
:- mode do_foldr2_pred(pred(in, in, out, di, uo) is cc_multi,
    in, in, out, di, uo) is cc_multi.
:- mode do_foldr2_pred(pred(in, in, out, in, out) is cc_multi,
    in, in, out, in, out) is cc_multi.

:- pragma type_spec(pred(do_foldr2_pred/6), T = uint).
:- pragma type_spec(pred(do_foldr2_pred/6), T = var(_)).

    % We don't just use list.foldr here because the overhead of allocating
    % the closure for fold_bits is significant for the compiler's runtime,
    % so it's best to avoid that even if `--optimize-higher-order' is not set.
do_foldr2_pred(_, bitset_nil, !Acc1, !Acc2).
do_foldr2_pred(Pred, bitset_cons(Offset, BitsLo, BitsHi, Tail),
        !Acc1, !Acc2) :-
    do_foldr2_pred(Pred, Tail, !Acc1, !Acc2),
    fold2_bits_high_to_low(Pred, Offset + ubits_per_uint,
        BitsHi, ubits_per_uint, !Acc1, !Acc2),
    fold2_bits_high_to_low(Pred, Offset,
        BitsLo, ubits_per_uint, !Acc1, !Acc2).

%---------------------%

:- pred fold_bits_low_to_high(pred(T, U, U),
    uint, uint, uint, U, U) <= uenum(T).
:- mode fold_bits_low_to_high(pred(in, in, out) is det,
    in, in, in, in, out) is det.
:- mode fold_bits_low_to_high(pred(in, mdi, muo) is det,
    in, in, in, mdi, muo) is det.
:- mode fold_bits_low_to_high(pred(in, di, uo) is det,
    in, in, in, di, uo) is det.
:- mode fold_bits_low_to_high(pred(in, in, out) is semidet,
    in, in, in, in, out) is semidet.
:- mode fold_bits_low_to_high(pred(in, mdi, muo) is semidet,
    in, in, in, mdi, muo) is semidet.
:- mode fold_bits_low_to_high(pred(in, di, uo) is semidet,
    in, in, in, di, uo) is semidet.
:- mode fold_bits_low_to_high(pred(in, in, out) is nondet,
    in, in, in, in, out) is nondet.
:- mode fold_bits_low_to_high(pred(in, di, uo) is cc_multi,
    in, in, in, di, uo) is cc_multi.
:- mode fold_bits_low_to_high(pred(in, in, out) is cc_multi,
    in, in, in, in, out) is cc_multi.
:- pragma type_spec(pred(fold_bits_low_to_high/6), T = uint).
:- pragma type_spec(pred(fold_bits_low_to_high/6), T = var(_)).

fold_bits_low_to_high(Pred, Offset, Bits, Size, !Acc) :-
    ( if Bits = 0u then
        true
    else if Size = 1u then
        Item = det_from_uint(Offset),
        Pred(Item, !Acc)
    else
        HalfSize = unchecked_right_ushift(Size, 1u),
        Mask = mask(HalfSize),

        % Extract the low-order and high-order halves of the bits.
        LowBits = Mask /\ Bits,
        HighBits = Mask /\ unchecked_right_ushift(Bits, HalfSize),

        fold_bits_low_to_high(Pred, Offset,
            LowBits, HalfSize, !Acc),
        fold_bits_low_to_high(Pred, Offset + HalfSize,
            HighBits, HalfSize, !Acc)
    ).

:- pred fold_bits_high_to_low(pred(T, U, U),
    uint, uint, uint, U, U) <= uenum(T).
:- mode fold_bits_high_to_low(pred(in, in, out) is det,
    in, in, in, in, out) is det.
:- mode fold_bits_high_to_low(pred(in, mdi, muo) is det,
    in, in, in, mdi, muo) is det.
:- mode fold_bits_high_to_low(pred(in, di, uo) is det,
    in, in, in, di, uo) is det.
:- mode fold_bits_high_to_low(pred(in, in, out) is semidet,
    in, in, in, in, out) is semidet.
:- mode fold_bits_high_to_low(pred(in, mdi, muo) is semidet,
    in, in, in, mdi, muo) is semidet.
:- mode fold_bits_high_to_low(pred(in, di, uo) is semidet,
    in, in, in, di, uo) is semidet.
:- mode fold_bits_high_to_low(pred(in, in, out) is nondet,
    in, in, in, in, out) is nondet.
:- mode fold_bits_high_to_low(pred(in, di, uo) is cc_multi,
    in, in, in, di, uo) is cc_multi.
:- mode fold_bits_high_to_low(pred(in, in, out) is cc_multi,
    in, in, in, in, out) is cc_multi.
:- pragma type_spec(pred(fold_bits_high_to_low/6), T = uint).
:- pragma type_spec(pred(fold_bits_high_to_low/6), T = var(_)).

fold_bits_high_to_low(Pred, Offset, Bits, Size, !Acc) :-
    ( if Bits = 0u then
        true
    else if Size = 1u then
        Item = det_from_uint(Offset),
        Pred(Item, !Acc)
    else
        HalfSize = unchecked_right_ushift(Size, 1u),
        Mask = mask(HalfSize),

        % Extract the low-order and high-order halves of the bits.
        LowBits = Mask /\ Bits,
        HighBits = Mask /\ unchecked_right_ushift(Bits, HalfSize),

        fold_bits_high_to_low(Pred, Offset + HalfSize,
            HighBits, HalfSize, !Acc),
        fold_bits_high_to_low(Pred, Offset,
            LowBits, HalfSize, !Acc)
    ).

:- pred fold2_bits_low_to_high(pred(T, U, U, V, V),
    uint, uint, uint, U, U, V, V) <= uenum(T).
:- mode fold2_bits_low_to_high(pred(in, in, out, in, out) is det,
    in, in, in, in, out, in, out) is det.
:- mode fold2_bits_low_to_high(pred(in, in, out, mdi, muo) is det,
    in, in, in, in, out, mdi, muo) is det.
:- mode fold2_bits_low_to_high(pred(in, di, uo, di, uo) is det,
    in, in, in, di, uo, di, uo) is det.
:- mode fold2_bits_low_to_high(pred(in, in, out, di, uo) is det,
    in, in, in, in, out, di, uo) is det.
:- mode fold2_bits_low_to_high(pred(in, in, out, in, out) is semidet,
    in, in, in, in, out, in, out) is semidet.
:- mode fold2_bits_low_to_high(pred(in, in, out, mdi, muo) is semidet,
    in, in, in, in, out, mdi, muo) is semidet.
:- mode fold2_bits_low_to_high(pred(in, in, out, di, uo) is semidet,
    in, in, in, in, out, di, uo) is semidet.
:- mode fold2_bits_low_to_high(pred(in, in, out, in, out) is nondet,
    in, in, in, in, out, in, out) is nondet.
:- mode fold2_bits_low_to_high(pred(in, di, uo, di, uo) is cc_multi,
    in, in, in, di, uo, di, uo) is cc_multi.
:- mode fold2_bits_low_to_high(pred(in, in, out, di, uo) is cc_multi,
    in, in, in, in, out, di, uo) is cc_multi.
:- mode fold2_bits_low_to_high(pred(in, in, out, in, out) is cc_multi,
    in, in, in, in, out, in, out) is cc_multi.
:- pragma type_spec(pred(fold2_bits_low_to_high/8), T = uint).
:- pragma type_spec(pred(fold2_bits_low_to_high/8), T = var(_)).

fold2_bits_low_to_high(Pred, Offset, Bits, Size, !Acc1, !Acc2) :-
    ( if Bits = 0u then
        true
    else if Size = 1u then
        Item = det_from_uint(Offset),
        Pred(Item, !Acc1, !Acc2)
    else
        HalfSize = unchecked_right_ushift(Size, 1u),
        Mask = mask(HalfSize),

        % Extract the low-order and high-order halves of the bits.
        LowBits = Mask /\ Bits,
        HighBits = Mask /\ unchecked_right_ushift(Bits, HalfSize),

        fold2_bits_low_to_high(Pred, Offset, LowBits, HalfSize, !Acc1, !Acc2),
        fold2_bits_low_to_high(Pred, Offset + HalfSize, HighBits, HalfSize,
            !Acc1, !Acc2)
    ).

:- pred fold2_bits_high_to_low(pred(T, U, U, V, V),
    uint, uint, uint, U, U, V, V) <= uenum(T).
:- mode fold2_bits_high_to_low(pred(in, in, out, in, out) is det,
    in, in, in, in, out, in, out) is det.
:- mode fold2_bits_high_to_low(pred(in, in, out, mdi, muo) is det,
    in, in, in, in, out, mdi, muo) is det.
:- mode fold2_bits_high_to_low(pred(in, di, uo, di, uo) is det,
    in, in, in, di, uo, di, uo) is det.
:- mode fold2_bits_high_to_low(pred(in, in, out, di, uo) is det,
    in, in, in, in, out, di, uo) is det.
:- mode fold2_bits_high_to_low(pred(in, in, out, in, out) is semidet,
    in, in, in, in, out, in, out) is semidet.
:- mode fold2_bits_high_to_low(pred(in, in, out, mdi, muo) is semidet,
    in, in, in, in, out, mdi, muo) is semidet.
:- mode fold2_bits_high_to_low(pred(in, in, out, di, uo) is semidet,
    in, in, in, in, out, di, uo) is semidet.
:- mode fold2_bits_high_to_low(pred(in, in, out, in, out) is nondet,
    in, in, in, in, out, in, out) is nondet.
:- mode fold2_bits_high_to_low(pred(in, di, uo, di, uo) is cc_multi,
    in, in, in, di, uo, di, uo) is cc_multi.
:- mode fold2_bits_high_to_low(pred(in, in, out, di, uo) is cc_multi,
    in, in, in, in, out, di, uo) is cc_multi.
:- mode fold2_bits_high_to_low(pred(in, in, out, in, out) is cc_multi,
    in, in, in, in, out, in, out) is cc_multi.
:- pragma type_spec(pred(fold2_bits_high_to_low/8), T = uint).
:- pragma type_spec(pred(fold2_bits_high_to_low/8), T = var(_)).

fold2_bits_high_to_low(Pred, Offset, Bits, Size, !Acc1, !Acc2) :-
    ( if Bits = 0u then
        true
    else if Size = 1u then
        Item = det_from_uint(Offset),
        Pred(Item, !Acc1, !Acc2)
    else
        HalfSize = unchecked_right_ushift(Size, 1u),
        Mask = mask(HalfSize),

        % Extract the low-order and high-order halves of the bits.
        LowBits = Mask /\ Bits,
        HighBits = Mask /\ unchecked_right_ushift(Bits, HalfSize),

        fold2_bits_high_to_low(Pred, Offset + HalfSize, HighBits, HalfSize,
            !Acc1, !Acc2),
        fold2_bits_high_to_low(Pred, Offset, LowBits, HalfSize, !Acc1, !Acc2)
    ).

%---------------------------------------------------------------------------%
%
% Utility predicates and functions for the rest of the module above.
%

    % Return the offset of the element of a set which should contain the given
    % element, and a <lo, hi> pair of uints with the bit corresponding
    % to that element set.
    %
:- pred bits_for_index(uint::in, uint::out, uint::out, uint::out) is det.
:- pragma inline(pred(bits_for_index/4)).

bits_for_index(Index, Offset, BitsLo, BitsHi) :-
    Mask = unchecked_left_ushift(uint.ubits_per_uint, 1u) - 1u,
    Offset = Index /\ \ Mask,
    BitToSet = Index /\ Mask,
    set_bit(BitToSet, 0u, BitsLo, 0u, BitsHi).

:- pred offset_and_bit_to_set_for_index(uint::in, uint::out, uint::out) is det.
:- pragma inline(pred(offset_and_bit_to_set_for_index/3)).

offset_and_bit_to_set_for_index(Index, Offset, BitToSet) :-
    Mask = unchecked_left_ushift(uint.ubits_per_uint, 1u) - 1u,
    Offset = Index /\ \ Mask,
    BitToSet = Index /\ Mask.

:- func get_bit(uint, uint, uint) = uint.
:- pragma inline(func(get_bit/3)).

get_bit(BitsLo, BitsHi, Bit) = Value :-
    ( if Bit < ubits_per_uint then
        Value = BitsLo /\ unchecked_left_ushift(1u, Bit)
    else
        Value = BitsHi /\ unchecked_left_ushift(1u, Bit - ubits_per_uint)
    ).

:- pred set_bit(uint::in, uint::in, uint::out, uint::in, uint::out) is det.
:- pragma inline(pred(set_bit/5)).

set_bit(Bit, BitsLo0, BitsLo, BitsHi0, BitsHi) :-
    ( if Bit < ubits_per_uint then
        BitsLo = BitsLo0 \/ unchecked_left_ushift(1u, Bit),
        BitsHi = BitsHi0
    else
        BitsLo = BitsLo0,
        BitsHi = BitsHi0 \/ unchecked_left_ushift(1u, Bit - ubits_per_uint)
    ).

:- pred clear_bit(uint::in, uint::in, uint::out) is det.
:- pragma inline(pred(clear_bit/3)).

clear_bit(Bit, Bits0, Bits) :-
    Bits = Bits0 /\ \ unchecked_left_ushift(1u, Bit).

    % mask(N) returns a mask which can be `and'ed with a uint to return
    % the lower N bits of the uint. N must be less than ubits_per_uint.
    %
:- func mask(uint) = uint.
:- pragma inline(func(mask/1)).

mask(N) = \ unchecked_left_ushift(\ 0u, N).

:- func make_bitset_cons(uint, uint, uint, bitset_elems) = bitset_elems.
:- pragma inline(func(make_bitset_cons/4)).

make_bitset_cons(Offset, BitsLo, BitsHi, Tail) =
    bitset_cons(Offset, BitsLo, BitsHi, Tail).

%---------------------------------------------------------------------------%
:- end_module fatter_sparse_bitset.
%---------------------------------------------------------------------------%
