%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006, 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: tree_bitset.m.
% Author: zs, based on sparse_bitset.m by stayl.
% Stability: medium.
%
% This module provides an ADT for storing sets of non-negative integers.
% If the integers stored are closely grouped, a tree_bitset is more compact
% than the representation provided by set.m, and the operations will be much
% faster. Compared to sparse_bitset.m, the operations provided by this module
% for contains, union, intersection and difference can be expected to have
% lower asymptotic complexity (often logarithmic in the number of elements in
% the sets, rather than linear). The price for this is a representation that
% requires more memory, higher constant factors, and an additional factor
% representing the tree in the complexity of the operations that construct
% tree_bitsets. However, since the depth of the tree has a small upper bound
% for all sets of a practical size, we will fold this into the "higher
% constant factors" in the descriptions of the complexity of the individual
% operations below.
%
% All this means that using a tree_bitset in preference to a sparse_bitset
% is likely to be a good idea only when the sizes of the sets to be manipulated
% are quite big, or when worst-case performance is important.
%
% For the time being, this module can only handle items that map to nonnegative
% integers. This may change once unsigned integer operations are available.
%
%---------------------------------------------------------------------------%

:- module tree_bitset.
:- interface.

:- import_module enum.
:- import_module list.
:- import_module term.

:- use_module set.

%---------------------------------------------------------------------------%

:- type tree_bitset(T). % <= enum(T).

%---------------------------------------------------------------------------%
%
% Initial creation of sets.
%

    % Return an empty set.
    %
:- func init = tree_bitset(T).

    % `make_singleton_set(Elem)' returns a set containing just the single
    % element `Elem'.
    %
:- func make_singleton_set(T) = tree_bitset(T) <= enum(T).

%---------------------------------------------------------------------------%
%
% Emptiness and singleton-ness tests.
%

:- pred empty(tree_bitset(T)).
:- mode empty(in) is semidet.
:- mode empty(out) is det.
:- pragma obsolete(pred(empty/1), [init/0, is_empty/1]).

:- pred is_empty(tree_bitset(T)::in) is semidet.

:- pred is_non_empty(tree_bitset(T)::in) is semidet.

    % Is the given set a singleton, and if yes, what is the element?
    %
:- pred is_singleton(tree_bitset(T)::in, T::out) is semidet <= enum(T).

%---------------------------------------------------------------------------%
%
% Membership tests.
%

    % `member(X, Set)' is true iff `X' is a member of `Set'.
    % Takes O(card(Set)) time for the semidet mode.
    %
:- pred member(T, tree_bitset(T)) <= enum(T).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

    % `contains(Set, X)' is true iff `X' is a member of `Set'.
    % Takes O(log(card(Set))) time.
    %
:- pred contains(tree_bitset(T)::in, T::in) is semidet <= enum(T).

%---------------------------------------------------------------------------%
%
% Insertions and deletions.
%

    % `insert(Set, X)' returns the union of `Set' and the set containing
    % only `X'. Takes O(log(card(Set))) time and space.
    %
:- func insert(tree_bitset(T), T) = tree_bitset(T) <= enum(T).
:- pred insert(T::in, tree_bitset(T)::in, tree_bitset(T)::out)
    is det <= enum(T).

    % `insert_new(X, Set0, Set)' returns the union of `Set' and the set
    % containing only `X' is `Set0' does not contain 'X'; if it does, it fails.
    % Takes O(log(card(Set))) time and space.
    %
:- pred insert_new(T::in, tree_bitset(T)::in, tree_bitset(T)::out)
    is semidet <= enum(T).

    % `insert_list(Set, X)' returns the union of `Set' and the set containing
    % only the members of `X'. Same as `union(Set, list_to_set(X))', but may be
    % more efficient.
    %
:- func insert_list(tree_bitset(T), list(T)) = tree_bitset(T) <= enum(T).
:- pred insert_list(list(T)::in, tree_bitset(T)::in, tree_bitset(T)::out)
    is det <= enum(T).

%---------------------%

    % `delete(Set, X)' returns the difference of `Set' and the set containing
    % only `X'. Takes O(card(Set)) time and space.
    %
:- func delete(tree_bitset(T), T) = tree_bitset(T) <= enum(T).
:- pred delete(T::in, tree_bitset(T)::in, tree_bitset(T)::out)
    is det <= enum(T).

    % `delete_list(Set, X)' returns the difference of `Set' and the set
    % containing only the members of `X'. Same as
    % `difference(Set, list_to_set(X))', but may be more efficient.
    %
:- func delete_list(tree_bitset(T), list(T)) = tree_bitset(T) <= enum(T).
:- pred delete_list(list(T)::in, tree_bitset(T)::in, tree_bitset(T)::out)
    is det <= enum(T).

    % `remove(X, Set0, Set)' returns in `Set' the difference of `Set0'
    % and the set containing only `X', failing if `Set0' does not contain `X'.
    % Takes O(log(card(Set))) time and space.
    %
:- pred remove(T::in, tree_bitset(T)::in, tree_bitset(T)::out)
    is semidet <= enum(T).

    % `remove_list(X, Set0, Set)' returns in `Set' the difference of `Set0'
    % and the set containing all the elements of `X', failing if any element
    % of `X' is not in `Set0'. Same as `subset(list_to_set(X), Set0),
    % difference(Set0, list_to_set(X), Set)', but may be more efficient.
    %
:- pred remove_list(list(T)::in, tree_bitset(T)::in, tree_bitset(T)::out)
    is semidet <= enum(T).

    % `remove_leq(Set, X)' returns `Set' with all elements less than or equal
    % to `X' removed. In other words, it returns the set containing all the
    % elements of `Set' which are greater than `X'. Takes O(log(card(Set)))
    % time and space.
    %
:- func remove_leq(tree_bitset(T), T) = tree_bitset(T) <= enum(T).

    % `remove_gt(Set, X)' returns `Set' with all elements greater than `X'
    % removed. In other words, it returns the set containing all the elements
    % of `Set' which are less than or equal to `X'. Takes O(log(card(Set)))
    % time and space.
    %
:- func remove_gt(tree_bitset(T), T) = tree_bitset(T) <= enum(T).

    % `remove_least(Set0, X, Set)' is true iff `X' is the least element in
    % `Set0', and `Set' is the set which contains all the elements of `Set0'
    % except `X'. Takes O(1) time and space.
    %
:- pred remove_least(T::out, tree_bitset(T)::in, tree_bitset(T)::out)
    is semidet <= enum(T).

%---------------------------------------------------------------------------%
%
% Comparisons between sets.
%

    % `equal(SetA, SetB)' is true iff `SetA' and `SetB' contain the same
    % elements. Takes O(min(card(SetA), card(SetB))) time.
    %
:- pred equal(tree_bitset(T)::in, tree_bitset(T)::in) is semidet <= enum(T).

    % `subset(Subset, Set)' is true iff `Subset' is a subset of `Set'.
    % Same as `intersect(Set, Subset, Subset)', but may be more efficient.
    %
:- pred subset(tree_bitset(T)::in, tree_bitset(T)::in) is semidet.

    % `superset(Superset, Set)' is true iff `Superset' is a superset of `Set'.
    % Same as `intersect(Superset, Set, Set)', but may be more efficient.
    %
:- pred superset(tree_bitset(T)::in, tree_bitset(T)::in) is semidet.

%---------------------------------------------------------------------------%
%
% Operations on two or more sets.
%

    % `union(SetA, SetB)' returns the union of `SetA' and `SetB'. The
    % efficiency of the union operation is not sensitive to the argument
    % ordering. Takes somewhere between O(log(card(SetA)) + log(card(SetB)))
    % and O(card(SetA) + card(SetB)) time and space.
    %
:- func union(tree_bitset(T), tree_bitset(T)) = tree_bitset(T).
:- pred union(tree_bitset(T)::in, tree_bitset(T)::in, tree_bitset(T)::out)
    is det.

    % `union_list(Sets, Set)' returns the union of all the sets in Sets.
    %
:- func union_list(list(tree_bitset(T))) = tree_bitset(T).
:- pred union_list(list(tree_bitset(T))::in, tree_bitset(T)::out) is det.

    % `intersect(SetA, SetB)' returns the intersection of `SetA' and `SetB'.
    % The efficiency of the intersection operation is not sensitive to the
    % argument ordering. Takes somewhere between
    % O(log(card(SetA)) + log(card(SetB))) and O(card(SetA) + card(SetB)) time,
    % and O(min(card(SetA)), card(SetB)) space.
    %
:- func intersect(tree_bitset(T), tree_bitset(T)) = tree_bitset(T).
:- pred intersect(tree_bitset(T)::in, tree_bitset(T)::in, tree_bitset(T)::out)
    is det.

    % `intersect_list(Sets, Set)' returns the intersection of all the sets
    % in Sets.
    %
:- func intersect_list(list(tree_bitset(T))) = tree_bitset(T).
:- pred intersect_list(list(tree_bitset(T))::in, tree_bitset(T)::out) is det.

    % `difference(SetA, SetB)' returns the set containing all the elements
    % of `SetA' except those that occur in `SetB'. Takes somewhere between
    % O(log(card(SetA)) + log(card(SetB))) and O(card(SetA) + card(SetB)) time,
    % and O(card(SetA)) space.
    %
:- func difference(tree_bitset(T), tree_bitset(T)) = tree_bitset(T).
:- pred difference(tree_bitset(T)::in, tree_bitset(T)::in, tree_bitset(T)::out)
    is det.

%---------------------------------------------------------------------------%
%
% Operations that divide a set into two parts.
%

    % divide(Pred, Set, InPart, OutPart):
    % InPart consists of those elements of Set for which Pred succeeds;
    % OutPart consists of those elements of Set for which Pred fails.
    %
:- pred divide(pred(T)::in(pred(in) is semidet), tree_bitset(T)::in,
    tree_bitset(T)::out, tree_bitset(T)::out) is det <= enum(T).

    % divide_by_set(DivideBySet, Set, InPart, OutPart):
    % InPart consists of those elements of Set which are also in DivideBySet;
    % OutPart consists of those elements of Set which are not in DivideBySet.
    %
:- pred divide_by_set(tree_bitset(T)::in, tree_bitset(T)::in,
    tree_bitset(T)::out, tree_bitset(T)::out) is det <= enum(T).

%---------------------------------------------------------------------------%
%
% Converting lists to sets.
%

    % `list_to_set(List)' returns a set containing only the members of `List'.
    % Takes O(length(List)) time and space.
    %
:- func list_to_set(list(T)) = tree_bitset(T) <= enum(T).
:- pred list_to_set(list(T)::in, tree_bitset(T)::out) is det <= enum(T).

    % `sorted_list_to_set(List)' returns a set containing only the members
    % of `List'. `List' must be sorted. Takes O(length(List)) time and space.
    %
:- func sorted_list_to_set(list(T)) = tree_bitset(T) <= enum(T).
:- pred sorted_list_to_set(list(T)::in, tree_bitset(T)::out) is det <= enum(T).

%---------------------------------------------------------------------------%
%
% Converting sets to lists.
%

    % `to_sorted_list(Set)' returns a list containing all the members of `Set',
    % in sorted order. Takes O(card(Set)) time and space.
    %
:- func to_sorted_list(tree_bitset(T)) = list(T) <= enum(T).
:- pred to_sorted_list(tree_bitset(T)::in, list(T)::out) is det <= enum(T).

%---------------------------------------------------------------------------%
%
% Converting between different kinds of sets.
%

    % `from_set(Set)' returns a bitset containing only the members of `Set'.
    % Takes O(card(Set)) time and space.
    %
:- func from_set(set.set(T)) = tree_bitset(T) <= enum(T).

    % `to_sorted_list(Set)' returns a set.set containing all the members
    % of `Set', in sorted order. Takes O(card(Set)) time and space.
    %
:- func to_set(tree_bitset(T)) = set.set(T) <= enum(T).

%---------------------------------------------------------------------------%
%
% Counting.
%

    % `count(Set)' returns the number of elements in `Set'.
    % Takes O(card(Set)) time.
    %
:- func count(tree_bitset(T)) = int <= enum(T).

%---------------------------------------------------------------------------%
%
% Standard higher order functions on collections.
%

    % all_true(Pred, Set) succeeds iff Pred(Element) succeeds
    % for all the elements of Set.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet), tree_bitset(T)::in)
    is semidet <= enum(T).

    % `filter(Pred, Set) = TrueSet' returns the elements of Set for which
    % Pred succeeds.
    %
:- func filter(pred(T), tree_bitset(T)) = tree_bitset(T) <= enum(T).
:- mode filter(pred(in) is semidet, in) = out is det.

    % `filter(Pred, Set, TrueSet, FalseSet)' returns the elements of Set
    % for which Pred succeeds, and those for which it fails.
    %
:- pred filter(pred(T), tree_bitset(T), tree_bitset(T), tree_bitset(T))
    <= enum(T).
:- mode filter(pred(in) is semidet, in, out, out) is det.

    % `foldl(Func, Set, Start)' calls Func with each element of `Set'
    % (in sorted order) and an accumulator (with the initial value of `Start'),
    % and returns the final value. Takes O(card(Set)) time.
    %
:- func foldl(func(T, U) = U, tree_bitset(T), U) = U <= enum(T).

:- pred foldl(pred(T, U, U), tree_bitset(T), U, U) <= enum(T).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode foldl(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode foldl(pred(in, mdi, muo) is nondet, in, mdi, muo) is nondet.
:- mode foldl(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode foldl(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.

:- pred foldl2(pred(T, U, U, V, V), tree_bitset(T), U, U, V, V) <= enum(T).
:- mode foldl2(pred(in, di, uo, di, uo) is det, in, di, uo, di, uo) is det.
:- mode foldl2(pred(in, in, out, di, uo) is det, in, in, out, di, uo) is det.
:- mode foldl2(pred(in, in, out, in, out) is det, in, in, out, in, out) is det.
:- mode foldl2(pred(in, in, out, in, out) is semidet, in, in, out, in, out)
    is semidet.
:- mode foldl2(pred(in, in, out, in, out) is nondet, in, in, out, in, out)
    is nondet.
:- mode foldl2(pred(in, di, uo, di, uo) is cc_multi, in, di, uo, di, uo)
    is cc_multi.
:- mode foldl2(pred(in, in, out, di, uo) is cc_multi, in, in, out, di, uo)
    is cc_multi.
:- mode foldl2(pred(in, in, out, in, out) is cc_multi, in, in, out, in, out)
    is cc_multi.

    % `foldr(Func, Set, Start)' calls Func with each element of `Set'
    % (in reverse sorted order) and an accumulator (with the initial value
    % of `Start'), and returns the final value. Takes O(card(Set)) time.
    %
:- func foldr(func(T, U) = U, tree_bitset(T), U) = U <= enum(T).

:- pred foldr(pred(T, U, U), tree_bitset(T), U, U) <= enum(T).
:- mode foldr(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldr(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode foldr(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode foldr(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.

:- pred foldr2(pred(T, U, U, V, V), tree_bitset(T), U, U, V, V) <= enum(T).
:- mode foldr2(pred(in, di, uo, di, uo) is det, in, di, uo, di, uo) is det.
:- mode foldr2(pred(in, in, out, di, uo) is det, in, in, out, di, uo) is det.
:- mode foldr2(pred(in, in, out, in, out) is det, in, in, out, in, out) is det.
:- mode foldr2(pred(in, in, out, in, out) is semidet, in, in, out, in, out)
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

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

%---------------------------------------------------------------------------%

:- interface.

:- pragma type_spec(func(init/0), T = var(_)).
:- pragma type_spec(func(init/0), T = int).

:- pragma type_spec(func(make_singleton_set/1), T = var(_)).
:- pragma type_spec(func(make_singleton_set/1), T = int).

:- pragma type_spec(pred(member/2), T = var(_)).
:- pragma type_spec(pred(member/2), T = int).

:- pragma type_spec(pred(contains/2), T = var(_)).
:- pragma type_spec(pred(contains/2), T = int).

:- pragma type_spec(func(insert/2), T = var(_)).
:- pragma type_spec(func(insert/2), T = int).
:- pragma type_spec(pred(insert/3), T = var(_)).
:- pragma type_spec(pred(insert/3), T = int).

:- pragma type_spec(func(insert_list/2), T = var(_)).
:- pragma type_spec(func(insert_list/2), T = int).
:- pragma type_spec(pred(insert_list/3), T = var(_)).
:- pragma type_spec(pred(insert_list/3), T = int).

:- pragma type_spec(func(delete/2), T = var(_)).
:- pragma type_spec(func(delete/2), T = int).
:- pragma type_spec(pred(delete/3), T = var(_)).
:- pragma type_spec(pred(delete/3), T = int).

:- pragma type_spec(func(delete_list/2), T = var(_)).
:- pragma type_spec(func(delete_list/2), T = int).
:- pragma type_spec(pred(delete_list/3), T = var(_)).
:- pragma type_spec(pred(delete_list/3), T = int).

:- pragma type_spec(pred(equal/2), T = var(_)).
:- pragma type_spec(pred(equal/2), T = int).

:- pragma type_spec(pred(subset/2), T = var(_)).
:- pragma type_spec(pred(subset/2), T = int).

:- pragma type_spec(pred(superset/2), T = var(_)).
:- pragma type_spec(pred(superset/2), T = int).

:- pragma type_spec(func(list_to_set/1), T = var(_)).
:- pragma type_spec(func(list_to_set/1), T = int).
:- pragma type_spec(pred(list_to_set/2), T = var(_)).
:- pragma type_spec(pred(list_to_set/2), T = int).

:- pragma type_spec(func(sorted_list_to_set/1), T = var(_)).
:- pragma type_spec(func(sorted_list_to_set/1), T = int).
:- pragma type_spec(pred(sorted_list_to_set/2), T = var(_)).
:- pragma type_spec(pred(sorted_list_to_set/2), T = int).

:- pragma type_spec(func(to_sorted_list/1), T = var(_)).
:- pragma type_spec(func(to_sorted_list/1), T = int).
:- pragma type_spec(pred(to_sorted_list/2), T = var(_)).
:- pragma type_spec(pred(to_sorted_list/2), T = int).

:- pragma type_spec(func(from_set/1), T = var(_)).
:- pragma type_spec(func(from_set/1), T = int).

:- pragma type_spec(func(to_set/1), T = var(_)).
:- pragma type_spec(func(to_set/1), T = int).

:- pragma type_spec(pred(all_true/2), T = int).
:- pragma type_spec(pred(all_true/2), T = var(_)).

:- pragma type_spec(func(foldl/3), T = int).
:- pragma type_spec(func(foldl/3), T = var(_)).

:- pragma type_spec(pred(foldl/4), T = int).
:- pragma type_spec(pred(foldl/4), T = var(_)).

:- pragma type_spec(func(foldr/3), T = int).
:- pragma type_spec(func(foldr/3), T = var(_)).

:- pragma type_spec(pred(foldr/4), T = int).
:- pragma type_spec(pred(foldr/4), T = var(_)).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module uint.

% These are needed only for integrity checking.
:- import_module bool.
:- import_module maybe.
:- import_module pair.

%---------------------------------------------------------------------------%

    % We describe a set using a tree. The basic idea is the following.
    %
    % - Level 0 nodes are leaf nodes. A leaf node contains a bitmap of
    %   bits_per_int bits.
    %
    % - Level k > 0 nodes are interior nodes. An interior node of level k + 1
    %   has up to 2 ^ bits_per_level children, all of level k.
    %
    % - If a node at level k is isomorphic to a bitmap of b bits, then a node
    %   at level k + 1 is isomorphic to the bitmap of b * 2 ^ bits_per_level
    %   bits formed from the concatenation of its child nodes.
    %
    % - A node at level k, therefore, is isomorphic to a bitmap of
    %   m = bits_per_int * 2 ^ (k * bits_per_level) bits.
    %
    % - All the bitmaps are naturally aligned, so the first bit in the bitmap
    %   of m bits represented by a level k node will have an index that is
    %   a multiple of m.
    %
    % For leaf nodes, we store the index of the first bit it represents.
    % For interior nodes, we store the index of the first bit it represents,
    % and the first bit after the last bit it represents.
    %
    % Leaf nodes contain bitmaps directly. Given leaf_node(Offset, Bits),
    % the bits of `Bits' describe which of the elements of the range
    % `Offset' .. `Offset + bits_per_int - 1' are in the set.
    %
    % Interior nodes contain bitmaps only indirectly; they contain a list
    % of nodes one level down. For level 1 interior nodes, this means
    % a list of leaf nodes; for interior nodes of level k+1, this means
    % a list of interior nodes of level k.
    %
    % Invariants:
    %
    % - In every list of nodes, all the nodes in the list have the same level.
    %
    % - In every list of nodes, the list elements are sorted on offset, and
    %   no two elements have the same offset.
    %
    % - A list of nodes of level k must have the ranges of all its nodes
    %   contained within the range of a single level k+1 node.
    %
    % - If a node's range contains no items, the node must be deleted.
    %
    % - The top level list should not be a singleton, unless it consists
    %   of a single leaf node.
    %
    % These invariants ensure that every set of items has a unique
    % representation.
    %
    % Leaf node cells should only be constructed using make_leaf_node/2.

:- type tree_bitset(T)    % <= enum(T)
    --->    tree_bitset(node_list).

:- type node_list
    --->    leaf_list(
                leaf_nodes      :: list(leaf_node)
            )
    ;       interior_list(
                % Convenient but redundant; could be computed from the
                % init_offset and limit_offset fields of the nodes.
                level           :: int,

                interior_nodes  :: list(interior_node)
            ).

:- type leaf_node
    --->    leaf_node(
                % Must be a multiple of bits_per_int.
                leaf_offset     :: int,

                % bits offset .. offset + bits_per_int - 1
                % The tree_bitset operations all remove elements of the list
                % with a `bits' field of zero.
                leaf_bits       :: uint
            ).

:- type interior_node
    --->    interior_node(
                % Must be a multiple of
                % bits_per_int * 2 ^ (level * bits_per_level).
                init_offset     :: int,

                % limit_offset = init_offset +
                %   bits_per_int * 2 ^ (level * bits_per_level)
                limit_offset    :: int,

                components      :: node_list
            ).

:- func bits_per_level = int.

bits_per_level = 5.

%---------------------------------------------------------------------------%

:- func make_leaf_node(int, uint) = leaf_node.
:- pragma inline(func(make_leaf_node/2)).

make_leaf_node(Offset, Bits) = leaf_node(Offset, Bits).

%---------------------------------------------------------------------------%

:- func enum_to_index(T) = int <= enum(T).

enum_to_index(Elem) = Index :-
    Index = enum.to_int(Elem),
    trace [compile_time(flag("tree-bitset-checks"))] (
        expect((Index >= 0), $pred, "enums must map to nonnegative integers")
    ).

:- func index_to_enum(int) = T <= enum(T).

index_to_enum(Index) = Elem :-
    ( if Elem0 = enum.from_int(Index) then
        Elem = Elem0
    else
        % We only apply `from_int/1' to integers returned by `to_int/1',
        % so it should never fail.
        unexpected($pred, "`enum.from_int/1' failed")
    ).

%---------------------------------------------------------------------------%

% This function should be the only place in the module that adds the
% tree_bitset/1 wrapper around node lists, and therefore the only place
% that constructs terms that are semantically tree_bitsets. Invoking our
% integrity test from here should thus guarantee that we never return
% any malformed tree_bitsets.
%
% If you want to use the integrity checking version of wrap_tree_bitset,
% then you will need to compile this module with the flag
%
%           --trace-flag="tree-bitset-integrity"

:- func wrap_tree_bitset(node_list) = tree_bitset(T).
:- pragma inline(func(wrap_tree_bitset/1)).

wrap_tree_bitset(NodeList) = Set :-
    trace [compile_time(flag("tree-bitset-integrity"))] (
        MaybeBounds = no,
        Integrity = integrity(MaybeBounds, NodeList),
        (
            Integrity = yes
        ;
            Integrity = no,
            unexpected($pred, "integrity failed")
        )
    ),
    Set = tree_bitset(NodeList).

:- func integrity(maybe(pair(int)), node_list) = bool.

integrity(MaybeBounds, NodeList) = OK :-
    (
        NodeList = leaf_list(LeafNodes),
        (
            LeafNodes = [],
            (
                MaybeBounds = no,
                OK = yes
            ;
                MaybeBounds = yes(_),
                OK = no
            )
        ;
            LeafNodes = [LeafHead | _],
            range_of_parent_node(LeafHead ^ leaf_offset, 0,
                ParentInitOffset, ParentLimitOffset),
            (
                MaybeBounds = no,
                LimitOK = yes
            ;
                MaybeBounds = yes(Init - Limit),
                ( if
                    Init = ParentInitOffset,
                    Limit = ParentLimitOffset
                then
                    LimitOK = yes
                else
                    LimitOK = no
                )
            ),
            (
                LimitOK = no,
                OK = no
            ;
                LimitOK = yes,
                OK = integrity_leaf_nodes(LeafNodes,
                    ParentInitOffset, ParentLimitOffset)
            )
        )
    ;
        NodeList = interior_list(Level, InteriorNodes),
        (
            InteriorNodes = [],
            ListOK = no
        ;
            InteriorNodes = [IH | IT],
            (
                IT = [],
                (
                    MaybeBounds = no,
                    ListOK = no
                ;
                    MaybeBounds = yes(_),
                    ListOK = yes(IH)
                )
            ;
                IT = [_ | _],
                ListOK = yes(IH)
            )
        ),
        (
            ListOK = no,
            OK = no
        ;
            ListOK = yes(InteriorHead),
            range_of_parent_node(InteriorHead ^ init_offset, Level,
                ParentInitOffset, ParentLimitOffset),
            (
                MaybeBounds = no,
                LimitOK = yes
            ;
                MaybeBounds = yes(Init - Limit),
                ( if
                    Init = ParentInitOffset,
                    Limit = ParentLimitOffset
                then
                    LimitOK = yes
                else
                    LimitOK = no
                )
            ),
            (
                LimitOK = no,
                OK = no
            ;
                LimitOK = yes,
                OK = integrity_interior_nodes(InteriorNodes, Level,
                    ParentInitOffset, ParentLimitOffset)
            )
        )
    ).

:- func integrity_leaf_nodes(list(leaf_node), int, int) = bool.

integrity_leaf_nodes([], _Init, _Limit) = yes.
integrity_leaf_nodes([Head | Tail], Init, Limit) = OK :-
    Offset = Head ^ leaf_offset,
    ( if Offset rem bits_per_int > 0 then
        OK = no
    else if not (Init =< Offset, Offset + bits_per_int - 1 < Limit) then
        OK = no
    else
        OK = integrity_leaf_nodes(Tail, Init, Limit)
    ).

:- func integrity_interior_nodes(list(interior_node), int, int, int) = bool.

integrity_interior_nodes([], _Level, _Init, _Limit) = yes.
integrity_interior_nodes([Head | Tail], Level, Init, Limit) = OK :-
    Head = interior_node(NodeInit, NodeLimit, Components),
    CalcLimit = NodeInit +
        unchecked_left_shift(bits_per_int, Level * bits_per_level),
    ( if NodeInit rem bits_per_int > 0 then
        OK = no
    else if NodeLimit rem bits_per_int > 0 then
        OK = no
    else if NodeLimit \= CalcLimit then
        OK = no
    else if not (Init =< NodeInit, NodeLimit - 1 < Limit) then
        OK = no
    else
        (
            Components = leaf_list(LeafNodes),
            ( if Level = 1 then
                SubOK = integrity_leaf_nodes(LeafNodes, NodeInit, NodeLimit)
            else
                SubOK = no
            )
        ;
            Components = interior_list(CompLevel, InteriorNodes),
            ( if CompLevel = Level - 1 then
                SubOK = integrity_interior_nodes(InteriorNodes, CompLevel,
                    NodeInit, NodeLimit)
            else
                SubOK = no
            )
        ),
        (
            SubOK = no,
            OK = no
        ;
            SubOK = yes,
            OK = integrity_interior_nodes(Tail, Level, Init, Limit)
        )
    ).

%---------------------------------------------------------------------------%

:- pred range_of_parent_node(int::in, int::in, int::out, int::out) is det.

range_of_parent_node(NodeOffset, NodeLevel,
        ParentInitOffset, ParentLimitOffset) :-
    HigherLevel = NodeLevel + 1,
    ParentRangeSize = unchecked_left_shift(int.bits_per_int,
        HigherLevel * bits_per_level),
    ParentInitOffset = NodeOffset /\ \ (ParentRangeSize - 1),
    ParentLimitOffset = ParentInitOffset + ParentRangeSize.

:- pred expand_range(int::in, node_list::in, int::in, int::in, int::in,
    interior_node::out, int::out) is det.

expand_range(Index, SubNodes, CurLevel, CurInitOffset, CurLimitOffset,
        TopNode, TopLevel) :-
    trace [compile_time(flag("tree-bitset-integrity"))] (
        (
            Range = unchecked_left_shift(bits_per_int,
                CurLevel * bits_per_level),
            ( if CurLimitOffset - CurInitOffset = Range then
                true
            else
                unexpected($pred, "bad range for level")
            )
        ;
            true
        )
    ),
    CurNode = interior_node(CurInitOffset, CurLimitOffset, SubNodes),
    range_of_parent_node(CurInitOffset, CurLevel,
        ParentInitOffset, ParentLimitOffset),
    ( if
        ParentInitOffset =< Index,
        Index < ParentLimitOffset
    then
        TopNode = CurNode,
        TopLevel = CurLevel
    else
        expand_range(Index, interior_list(CurLevel, [CurNode]), CurLevel + 1,
            ParentInitOffset, ParentLimitOffset, TopNode, TopLevel)
    ).

:- pred raise_leaves_to_interior(leaf_node::in, list(leaf_node)::in,
    interior_node::out) is det.
:- pragma inline(pred(raise_leaves_to_interior/3)).

raise_leaves_to_interior(LeafNode, LeafNodes, InteriorNode) :-
    range_of_parent_node(LeafNode ^ leaf_offset, 0,
        ParentInitOffset, ParentLimitOffset),
    NodeList = leaf_list([LeafNode | LeafNodes]),
    InteriorNode = interior_node(ParentInitOffset, ParentLimitOffset,
        NodeList).

:- pred raise_leaf_to_level(int::in, leaf_node::in, interior_node::out) is det.
:- pragma inline(pred(raise_leaf_to_level/3)).

raise_leaf_to_level(TargetLevel, LeafNode, TopNode) :-
    raise_leaves_to_interior(LeafNode, [], ParentNode),
    raise_one_interior_to_level(TargetLevel, 1, ParentNode, TopNode).

:- pred raise_one_interior_to_level(int::in, int::in,
    interior_node::in, interior_node::out) is det.

raise_one_interior_to_level(TargetLevel, CurLevel, CurNode, TopNode) :-
    ( if CurLevel = TargetLevel then
        TopNode = CurNode
    else
        range_of_parent_node(CurNode ^ init_offset, CurLevel,
            ParentInitOffset, ParentLimitOffset),
        NodeList = interior_list(CurLevel, [CurNode]),
        ParentNode = interior_node(ParentInitOffset, ParentLimitOffset,
            NodeList),
        raise_one_interior_to_level(TargetLevel, CurLevel + 1,
            ParentNode, TopNode)
    ).

:- pred raise_interiors_to_level(int::in, int::in,
    interior_node::in, list(interior_node)::in,
    interior_node::out, list(interior_node)::out) is det.

raise_interiors_to_level(TargetLevel, CurLevel, CurNodesHead, CurNodesTail,
        TopNodesHead, TopNodesTail) :-
    ( if CurLevel = TargetLevel then
        TopNodesHead = CurNodesHead,
        TopNodesTail = CurNodesTail
    else
        range_of_parent_node(CurNodesHead ^ init_offset, CurLevel,
            ParentInitOffset, ParentLimitOffset),
        NodeList = interior_list(CurLevel, [CurNodesHead | CurNodesTail]),
        ParentNode = interior_node(ParentInitOffset, ParentLimitOffset,
            NodeList),
        raise_one_interior_to_level(TargetLevel, CurLevel + 1,
            ParentNode, TopNodesHead),
        TopNodesTail = []
    ).

:- pred raise_to_common_level(int::in,
    interior_node::in, list(interior_node)::in,
    interior_node::in, list(interior_node)::in,
    interior_node::out, list(interior_node)::out,
    interior_node::out, list(interior_node)::out,
    int::out) is det.

raise_to_common_level(CurLevel, HeadA, TailA, HeadB, TailB,
        TopHeadA, TopTailA, TopHeadB, TopTailB, TopLevel) :-
    range_of_parent_node(HeadA ^ init_offset, CurLevel,
        ParentInitOffsetA, ParentLimitOffsetA),
    range_of_parent_node(HeadB ^ init_offset, CurLevel,
        ParentInitOffsetB, ParentLimitOffsetB),
    ( if ParentInitOffsetA = ParentInitOffsetB then
        trace [compile_time(flag("tree-bitset-checks"))] (
            expect(unify(ParentLimitOffsetA, ParentLimitOffsetB),
                $pred, "limit mismatch")
        ),
        TopHeadA = HeadA,
        TopTailA = TailA,
        TopHeadB = HeadB,
        TopTailB = TailB,
        TopLevel = CurLevel
    else
        ComponentsA = interior_list(CurLevel, [HeadA | TailA]),
        ParentA = interior_node(ParentInitOffsetA, ParentLimitOffsetA,
            ComponentsA),
        ComponentsB = interior_list(CurLevel, [HeadB | TailB]),
        ParentB = interior_node(ParentInitOffsetB, ParentLimitOffsetB,
            ComponentsB),
        raise_to_common_level(CurLevel + 1, ParentA, [], ParentB, [],
            TopHeadA, TopTailA, TopHeadB, TopTailB, TopLevel)
    ).

:- pred prune_top_levels(node_list::in, node_list::out) is det.

prune_top_levels(List, PrunedList) :-
    (
        List = leaf_list(_),
        PrunedList = List
    ;
        List = interior_list(_, Nodes),
        (
            Nodes = [],
            % This can happen if e.g. we subtract a set from itself.
            PrunedList = leaf_list([])
        ;
            Nodes = [Node],
            prune_top_levels(Node ^ components, PrunedList)
        ;
            Nodes = [_, _ | _],
            PrunedList = List
        )
    ).

:- pred head_and_tail(list(interior_node)::in,
    interior_node::out, list(interior_node)::out) is det.

head_and_tail([], _, _) :-
    unexpected($pred, "empty list").
head_and_tail([Head | Tail], Head, Tail).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

init = wrap_tree_bitset(leaf_list([])).

make_singleton_set(A) = insert(init, A).

%---------------------------------------------------------------------------%

empty(init).

is_empty(init).

is_non_empty(Set) :-
    not is_empty(Set).

is_singleton(Set, Elem) :-
    Set = tree_bitset(List0),
    List0 = leaf_list([Leaf]),
    fold_bits(high_to_low, cons, Leaf ^ leaf_offset, Leaf ^ leaf_bits,
        bits_per_int, [], List),
    List = [Elem].

%---------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(pred(member/2)).

member(Elem::in, Set::in) :-
    contains(Set, Elem).
member(Elem::out, Set::in) :-
    Set = tree_bitset(NodeList),
    (
        NodeList = leaf_list(LeafNodes),
        leaflist_member(Index, LeafNodes)
    ;
        NodeList = interior_list(_, InteriorNodes),
        interiorlist_member(Index, InteriorNodes)
    ),
    Elem = index_to_enum(Index).

:- pred interiorlist_member(int::out, list(interior_node)::in) is nondet.

interiorlist_member(Index, [Elem | Elems]) :-
    (
        Components = Elem ^ components,
        (
            Components = leaf_list(LeafNodes),
            leaflist_member(Index, LeafNodes)
        ;
            Components = interior_list(_, InteriorNodes),
            interiorlist_member(Index, InteriorNodes)
        )
    ;
        interiorlist_member(Index, Elems)
    ).

:- pred leaflist_member(int::out, list(leaf_node)::in) is nondet.

leaflist_member(Index, [Elem | Elems]) :-
    (
        leafnode_member(Index, Elem ^ leaf_offset, bits_per_int,
            Elem ^ leaf_bits)
    ;
        leaflist_member(Index, Elems)
    ).

:- pred leafnode_member(int::out, int::in, int::in, uint::in) is nondet.

leafnode_member(Index, Offset, Size, Bits) :-
    ( if Bits = 0u then
        fail
    else if Size = 1 then
        Index = Offset
    else
        HalfSize = unchecked_right_shift(Size, 1),
        Mask = mask(HalfSize),

        % Extract the low-order half of the bits.
        LowBits = Mask /\ Bits,

        % Extract the high-order half of the bits.
        HighBits = Mask /\ unchecked_right_shift(Bits, HalfSize),

        ( leafnode_member(Index, Offset, HalfSize, LowBits)
        ; leafnode_member(Index, Offset + HalfSize, HalfSize, HighBits)
        )
    ).

%---------------------%

contains(Set, Elem) :-
    Set = tree_bitset(NodeList),
    Index = enum_to_index(Elem),
    (
        NodeList = leaf_list(LeafNodes),
        leaflist_contains(LeafNodes, Index)
    ;
        NodeList = interior_list(_, InteriorNodes),
        interiorlist_contains(InteriorNodes, Index)
    ).

:- pred leaflist_contains(list(leaf_node)::in, int::in) is semidet.

leaflist_contains([Head | Tail], Index) :-
    Offset = Head ^ leaf_offset,
    Index >= Offset,
    ( if Index < Offset + bits_per_int then
        get_bit(Head ^ leaf_bits, Index - Offset) \= 0u
    else
        leaflist_contains(Tail, Index)
    ).

:- pred interiorlist_contains(list(interior_node)::in, int::in) is semidet.

interiorlist_contains([Head | Tail], Index) :-
    Index >= Head ^ init_offset,
    ( if Index < Head ^ limit_offset then
        Components = Head ^ components,
        (
            Components = leaf_list(LeafNodes),
            leaflist_contains(LeafNodes, Index)
        ;
            Components = interior_list(_, InteriorNodes),
            interiorlist_contains(InteriorNodes, Index)
        )
    else
        interiorlist_contains(Tail, Index)
    ).

%---------------------------------------------------------------------------%

insert(Set0, Elem) = Set :-
    Set0 = tree_bitset(List0),
    Index = enum_to_index(Elem),
    (
        List0 = leaf_list(LeafList0),
        (
            LeafList0 = [],
            bits_for_index(Index, Offset, Bits),
            Set = wrap_tree_bitset(leaf_list([make_leaf_node(Offset, Bits)]))
        ;
            LeafList0 = [Leaf0 | _],
            range_of_parent_node(Leaf0 ^ leaf_offset, 0,
                ParentInitOffset, ParentLimitOffset),
            ( if
                ParentInitOffset =< Index,
                Index < ParentLimitOffset
            then
                leaflist_insert(Index, LeafList0, LeafList),
                Set = wrap_tree_bitset(leaf_list(LeafList))
            else
                expand_range(Index, List0, 1,
                    ParentInitOffset, ParentLimitOffset,
                    InteriorNode1, InteriorLevel1),
                interiorlist_insert(Index, InteriorLevel1,
                    [InteriorNode1], InteriorList),
                Set = wrap_tree_bitset(
                    interior_list(InteriorLevel1, InteriorList))
            )
        )
    ;
        List0 = interior_list(InteriorLevel, InteriorList0),
        (
            InteriorList0 = [],
            % This is a violation of our invariants.
            unexpected($pred, "insert into empty list of interior nodes")
        ;
            InteriorList0 = [InteriorNode0 | _],
            range_of_parent_node(InteriorNode0 ^ init_offset, InteriorLevel,
                ParentInitOffset, ParentLimitOffset),
            ( if
                ParentInitOffset =< Index,
                Index < ParentLimitOffset
            then
                interiorlist_insert(Index, InteriorLevel,
                    InteriorList0, InteriorList),
                Set = wrap_tree_bitset(
                    interior_list(InteriorLevel, InteriorList))
            else
                expand_range(Index, List0, InteriorLevel + 1,
                    ParentInitOffset, ParentLimitOffset,
                    InteriorNode1, InteriorLevel1),
                interiorlist_insert(Index, InteriorLevel1,
                    [InteriorNode1], InteriorList),
                Set = wrap_tree_bitset(
                    interior_list(InteriorLevel1, InteriorList))
            )
        )
    ).

insert(Elem, !Set) :-
    !:Set = insert(!.Set, Elem).

:- pred leaflist_insert(int::in, list(leaf_node)::in, list(leaf_node)::out)
    is det.

leaflist_insert(Index, [], Leaves) :-
    bits_for_index(Index, Offset, Bits),
    Leaves = [make_leaf_node(Offset, Bits)].
leaflist_insert(Index, Leaves0 @ [Head0 | Tail0], Leaves) :-
    Offset0 = Head0 ^ leaf_offset,
    ( if Index < Offset0 then
        bits_for_index(Index, Offset, Bits),
        Leaves = [make_leaf_node(Offset, Bits) | Leaves0]
    else if BitToSet = Index - Offset0, BitToSet < bits_per_int then
        Bits0 = Head0 ^ leaf_bits,
        ( if get_bit(Bits0, BitToSet) = 0u then
            Bits = set_bit(Bits0, BitToSet),
            Leaves = [make_leaf_node(Offset0, Bits) | Tail0]
        else
            Leaves = Leaves0
        )
    else
        leaflist_insert(Index, Tail0, Tail),
        Leaves = [Head0 | Tail]
    ).

:- pred interiorlist_insert(int::in, int::in,
    list(interior_node)::in, list(interior_node)::out) is det.

interiorlist_insert(Index, Level, [], Nodes) :-
    bits_for_index(Index, Offset, Bits),
    raise_leaf_to_level(Level, make_leaf_node(Offset, Bits), Node),
    Nodes = [Node].
interiorlist_insert(Index, Level, Nodes0 @ [Head0 | Tail0], Nodes) :-
    Offset0 = Head0 ^ init_offset,
    ( if Index < Offset0 then
        bits_for_index(Index, Offset, Bits),
        raise_leaf_to_level(Level, make_leaf_node(Offset, Bits), Node),
        Nodes = [Node | Nodes0]
    else if Head0 ^ init_offset =< Index, Index < Head0 ^ limit_offset then
        Components0 = Head0 ^ components,
        (
            Components0 = leaf_list(LeafList0),
            trace [compile_time(flag("tree-bitset-checks"))] (
                expect(unify(Level, 1), $pred,
                    "bad component list (leaf)")
            ),
            leaflist_insert(Index, LeafList0, LeafList),
            Components = leaf_list(LeafList)
        ;
            Components0 = interior_list(InteriorLevel, InteriorList0),
            trace [compile_time(flag("tree-bitset-checks"))] (
                expect(unify(InteriorLevel, Level - 1), $pred,
                    "bad component list (interior)")
            ),
            interiorlist_insert(Index, InteriorLevel,
                InteriorList0, InteriorList),
            Components = interior_list(InteriorLevel, InteriorList)
        ),
        Head = Head0 ^ components := Components,
        Nodes = [Head | Tail0]
    else
        interiorlist_insert(Index, Level, Tail0, Tail),
        Nodes = [Head0 | Tail]
    ).

%---------------------%

insert_new(Elem, Set0, Set) :-
    Set0 = tree_bitset(List0),
    Index = enum_to_index(Elem),
    (
        List0 = leaf_list(LeafList0),
        (
            LeafList0 = [],
            bits_for_index(Index, Offset, Bits),
            Set = wrap_tree_bitset(leaf_list([make_leaf_node(Offset, Bits)]))
        ;
            LeafList0 = [Leaf0 | _],
            range_of_parent_node(Leaf0 ^ leaf_offset, 0,
                ParentInitOffset, ParentLimitOffset),
            ( if
                ParentInitOffset =< Index,
                Index < ParentLimitOffset
            then
                leaflist_insert_new(Index, LeafList0, LeafList),
                Set = wrap_tree_bitset(leaf_list(LeafList))
            else
                expand_range(Index, List0, 1,
                    ParentInitOffset, ParentLimitOffset,
                    InteriorNode1, InteriorLevel1),
                interiorlist_insert_new(Index, InteriorLevel1,
                    [InteriorNode1], InteriorList),
                Set = wrap_tree_bitset(
                    interior_list(InteriorLevel1, InteriorList))
            )
        )
    ;
        List0 = interior_list(InteriorLevel, InteriorList0),
        (
            InteriorList0 = [],
            % This is a violation of our invariants.
            unexpected($pred, "insert_new into empty list of interior nodes")
        ;
            InteriorList0 = [InteriorNode0 | _],
            range_of_parent_node(InteriorNode0 ^ init_offset, InteriorLevel,
                ParentInitOffset, ParentLimitOffset),
            ( if
                ParentInitOffset =< Index,
                Index < ParentLimitOffset
            then
                interiorlist_insert_new(Index, InteriorLevel,
                    InteriorList0, InteriorList),
                Set = wrap_tree_bitset(
                    interior_list(InteriorLevel, InteriorList))
            else
                expand_range(Index, List0, InteriorLevel + 1,
                    ParentInitOffset, ParentLimitOffset,
                    InteriorNode1, InteriorLevel1),
                interiorlist_insert_new(Index, InteriorLevel1,
                    [InteriorNode1], InteriorList),
                Set = wrap_tree_bitset(
                    interior_list(InteriorLevel1, InteriorList))
            )
        )
    ).

:- pred leaflist_insert_new(int::in,
    list(leaf_node)::in, list(leaf_node)::out) is semidet.

leaflist_insert_new(Index, [], Leaves) :-
    bits_for_index(Index, Offset, Bits),
    Leaves = [make_leaf_node(Offset, Bits)].
leaflist_insert_new(Index, Leaves0 @ [Head0 | Tail0], Leaves) :-
    Offset0 = Head0 ^ leaf_offset,
    ( if Index < Offset0 then
        bits_for_index(Index, Offset, Bits),
        Leaves = [make_leaf_node(Offset, Bits) | Leaves0]
    else if BitToSet = Index - Offset0, BitToSet < bits_per_int then
        Bits0 = Head0 ^ leaf_bits,
        ( if get_bit(Bits0, BitToSet) = 0u then
            Bits = set_bit(Bits0, BitToSet),
            Leaves = [make_leaf_node(Offset0, Bits) | Tail0]
        else
            fail
        )
    else
        leaflist_insert_new(Index, Tail0, Tail),
        Leaves = [Head0 | Tail]
    ).

:- pred interiorlist_insert_new(int::in, int::in,
    list(interior_node)::in, list(interior_node)::out) is semidet.

interiorlist_insert_new(Index, Level, [], Nodes) :-
    bits_for_index(Index, Offset, Bits),
    raise_leaf_to_level(Level, make_leaf_node(Offset, Bits), Node),
    Nodes = [Node].
interiorlist_insert_new(Index, Level, Nodes0 @ [Head0 | Tail0], Nodes) :-
    Offset0 = Head0 ^ init_offset,
    ( if Index < Offset0 then
        bits_for_index(Index, Offset, Bits),
        raise_leaf_to_level(Level, make_leaf_node(Offset, Bits), Node),
        Nodes = [Node | Nodes0]
    else if Head0 ^ init_offset =< Index, Index < Head0 ^ limit_offset then
        Components0 = Head0 ^ components,
        (
            Components0 = leaf_list(LeafList0),
            trace [compile_time(flag("tree-bitset-checks"))] (
                expect(unify(Level, 1), $pred,
                    "bad component list (leaf)")
            ),
            leaflist_insert_new(Index, LeafList0, LeafList),
            Components = leaf_list(LeafList)
        ;
            Components0 = interior_list(InteriorLevel, InteriorList0),
            trace [compile_time(flag("tree-bitset-checks"))] (
                expect(unify(InteriorLevel, Level - 1), $pred,
                    "bad component list (interior)")
            ),
            interiorlist_insert_new(Index, InteriorLevel,
                InteriorList0, InteriorList),
            Components = interior_list(InteriorLevel, InteriorList)
        ),
        Head = Head0 ^ components := Components,
        Nodes = [Head | Tail0]
    else
        interiorlist_insert_new(Index, Level, Tail0, Tail),
        Nodes = [Head0 | Tail]
    ).

%---------------------%

insert_list(Set, List) = union(list_to_set(List), Set).

insert_list(Elems, !Set) :-
    !:Set = insert_list(!.Set, Elems).

%---------------------%

delete(Set0, Elem) = Set :-
    Set0 = tree_bitset(List0),
    Index = enum_to_index(Elem),
    (
        List0 = leaf_list(LeafNodes0),
        leaflist_delete(LeafNodes0, Index, LeafNodes),
        List = leaf_list(LeafNodes)
    ;
        List0 = interior_list(Level, InteriorNodes0),
        interiorlist_delete(InteriorNodes0, Index, InteriorNodes),
        List1 = interior_list(Level, InteriorNodes),
        prune_top_levels(List1, List)
    ),
    Set = wrap_tree_bitset(List).

delete(Elem, !Set) :-
    !:Set = delete(!.Set, Elem).

:- pred interiorlist_delete(list(interior_node)::in, int::in,
    list(interior_node)::out) is det.

interiorlist_delete([], _, []).
interiorlist_delete([Head0 | Tail0], Index, Result) :-
    ( if Head0 ^ limit_offset =< Index then
        interiorlist_delete(Tail0, Index, Tail),
        Result = [Head0 | Tail]
    else if Head0 ^ init_offset =< Index then
        Components0 = Head0 ^ components,
        (
            Components0 = leaf_list(LeafNodes0),
            leaflist_delete(LeafNodes0, Index, LeafNodes),
            (
                LeafNodes = [],
                Result = Tail0
            ;
                LeafNodes = [_ | _],
                Components = leaf_list(LeafNodes),
                Head = interior_node(
                    Head0 ^ init_offset, Head0 ^ limit_offset, Components),
                Result = [Head | Tail0]
            )
        ;
            Components0 = interior_list(Level, InteriorNodes0),
            interiorlist_delete(InteriorNodes0, Index, InteriorNodes),
            (
                InteriorNodes = [],
                Result = Tail0
            ;
                InteriorNodes = [_ | _],
                Components = interior_list(Level, InteriorNodes),
                Head = interior_node(
                    Head0 ^ init_offset, Head0 ^ limit_offset, Components),
                Result = [Head | Tail0]
            )
        )
    else
        Result = [Head0 | Tail0]
    ).

:- pred leaflist_delete(list(leaf_node)::in, int::in, list(leaf_node)::out)
    is det.

leaflist_delete([], _, []).
leaflist_delete([Head0 | Tail0], Index, Result) :-
    Offset = Head0 ^ leaf_offset,
    ( if Offset + bits_per_int =< Index then
        leaflist_delete(Tail0, Index, Tail),
        Result = [Head0 | Tail]
    else if Offset =< Index then
        Bits = clear_bit(Head0 ^ leaf_bits, Index - Offset),
        ( if Bits = 0u then
            Result = Tail0
        else
            Result = [make_leaf_node(Offset, Bits) | Tail0]
        )
    else
        Result = [Head0 | Tail0]
    ).

%---------------------%

delete_list(Set, List) = difference(Set, list_to_set(List)).

delete_list(Elems, !Set) :-
    !:Set = delete_list(!.Set, Elems).

%---------------------%

remove(Elem, !Set) :-
    contains(!.Set, Elem),
    !:Set = delete(!.Set, Elem).

remove_list(Elems, !Set) :-
    ElemsSet = list_to_set(Elems),
    subset(ElemsSet, !.Set),
    !:Set = difference(!.Set, ElemsSet).

%---------------------------------------------------------------------------%

remove_leq(Set0, Elem) = Set :-
    Set0 = tree_bitset(List0),
    Index = enum_to_index(Elem),
    (
        List0 = leaf_list(LeafNodes0),
        remove_leq_leaf(LeafNodes0, Index, LeafNodes),
        List = leaf_list(LeafNodes)
    ;
        List0 = interior_list(Level, InteriorNodes0),
        remove_leq_interior(InteriorNodes0, Index, InteriorNodes),
        List1 = interior_list(Level, InteriorNodes),
        prune_top_levels(List1, List)
    ),
    Set = wrap_tree_bitset(List).

:- pred remove_leq_interior(list(interior_node)::in, int::in,
    list(interior_node)::out) is det.

remove_leq_interior([], _, []).
remove_leq_interior([Head0 | Tail0], Index, Result) :-
    ( if Head0 ^ limit_offset =< Index then
        remove_leq_interior(Tail0, Index, Result)
    else if Head0 ^ init_offset =< Index then
        Components0 = Head0 ^ components,
        (
            Components0 = leaf_list(LeafNodes0),
            remove_leq_leaf(LeafNodes0, Index, LeafNodes),
            (
                LeafNodes = [],
                Result = Tail0
            ;
                LeafNodes = [_ | _],
                Components = leaf_list(LeafNodes),
                Head = interior_node(
                    Head0 ^ init_offset, Head0 ^ limit_offset, Components),
                Result = [Head | Tail0]
            )
        ;
            Components0 = interior_list(Level, InteriorNodes0),
            remove_leq_interior(InteriorNodes0, Index, InteriorNodes),
            (
                InteriorNodes = [],
                Result = Tail0
            ;
                InteriorNodes = [_ | _],
                Components = interior_list(Level, InteriorNodes),
                Head = interior_node(
                    Head0 ^ init_offset, Head0 ^ limit_offset, Components),
                Result = [Head | Tail0]
            )
        )
    else
        Result = [Head0 | Tail0]
    ).

:- pred remove_leq_leaf(list(leaf_node)::in, int::in, list(leaf_node)::out)
    is det.

remove_leq_leaf([], _, []).
remove_leq_leaf([Head0 | Tail0], Index, Result) :-
    Offset = Head0 ^ leaf_offset,
    ( if Offset + bits_per_int =< Index then
        remove_leq_leaf(Tail0, Index, Result)
    else if Offset =< Index then
        Bits = Head0 ^ leaf_bits /\
            unchecked_left_shift(\ 0u, Index - Offset + 1),
        ( if Bits = 0u then
            Result = Tail0
        else
            Result = [make_leaf_node(Offset, Bits) | Tail0]
        )
    else
        Result = [Head0 | Tail0]
    ).

%---------------------%

remove_gt(Set0, Elem) = Set :-
    Set0 = tree_bitset(List0),
    Index = enum_to_index(Elem),
    (
        List0 = leaf_list(LeafNodes0),
        remove_gt_leaf(LeafNodes0, Index, LeafNodes),
        List = leaf_list(LeafNodes)
    ;
        List0 = interior_list(Level, InteriorNodes0),
        remove_gt_interior(InteriorNodes0, Index, InteriorNodes),
        List1 = interior_list(Level, InteriorNodes),
        prune_top_levels(List1, List)
    ),
    Set = wrap_tree_bitset(List).

:- pred remove_gt_interior(list(interior_node)::in, int::in,
    list(interior_node)::out) is det.

remove_gt_interior([], _, []).
remove_gt_interior([Head0 | Tail0], Index, Result) :-
    ( if Head0 ^ limit_offset =< Index then
        remove_gt_interior(Tail0, Index, Tail),
        Result = [Head0 | Tail]
    else if Head0 ^ init_offset =< Index then
        Components0 = Head0 ^ components,
        (
            Components0 = leaf_list(LeafNodes0),
            remove_gt_leaf(LeafNodes0, Index, LeafNodes),
            (
                LeafNodes = [],
                Result = []
            ;
                LeafNodes = [_ | _],
                Components = leaf_list(LeafNodes),
                Head = interior_node(
                    Head0 ^ init_offset, Head0 ^ limit_offset, Components),
                Result = [Head]
            )
        ;
            Components0 = interior_list(Level, InteriorNodes0),
            remove_gt_interior(InteriorNodes0, Index, InteriorNodes),
            (
                InteriorNodes = [],
                Result = []
            ;
                InteriorNodes = [_ | _],
                Components = interior_list(Level, InteriorNodes),
                Head = interior_node(
                    Head0 ^ init_offset, Head0 ^ limit_offset, Components),
                Result = [Head]
            )
        )
    else
        Result = []
    ).

:- pred remove_gt_leaf(list(leaf_node)::in, int::in,
    list(leaf_node)::out) is det.

remove_gt_leaf([], _, []).
remove_gt_leaf([Head0 | Tail0], Index, Result) :-
    Offset = Head0 ^ leaf_offset,
    ( if Offset + bits_per_int - 1 =< Index then
        remove_gt_leaf(Tail0, Index, Tail),
        Result = [Head0 | Tail]
    else if Offset =< Index then
        ( if
            Bits = Head0 ^ leaf_bits /\
                \ unchecked_left_shift(\ 0u, Index - Offset + 1),
            Bits \= 0u
        then
            Result = [make_leaf_node(Offset, Bits)]
        else
            Result = []
        )
    else
        Result = []
    ).

%---------------------%

remove_least(Elem, Set0, Set) :-
    Set0 = tree_bitset(List0),
    (
        List0 = leaf_list(LeafNodes0),
        (
            LeafNodes0 = [],
            % There is no least element to remove.
            fail
        ;
            LeafNodes0 = [LeafHead | LeafTail],
            remove_least_leaf(LeafHead, LeafTail, Index, LeafNodes)
        ),
        List = leaf_list(LeafNodes)
    ;
        List0 = interior_list(Level, InteriorNodes0),
        (
            InteriorNodes0 = [],
            unexpected($pred, "empty InteriorNodes0")
        ;
            InteriorNodes0 = [InteriorHead | InteriorTail],
            remove_least_interior(InteriorHead, InteriorTail, Index,
                InteriorNodes)
        ),
        List1 = interior_list(Level, InteriorNodes),
        prune_top_levels(List1, List)
    ),
    Elem = index_to_enum(Index),
    Set = wrap_tree_bitset(List).

:- pred remove_least_interior(interior_node::in, list(interior_node)::in,
    int::out, list(interior_node)::out) is det.

remove_least_interior(Head0, Tail0, Index, Nodes) :-
    Components0 = Head0 ^ components,
    (
        Components0 = leaf_list(LeafNodes0),
        (
            LeafNodes0 = [],
            unexpected($pred, "empty LeafNodes0")
        ;
            LeafNodes0 = [LeafHead0 | LeafTail0],
            remove_least_leaf(LeafHead0, LeafTail0, Index, LeafNodes),
            (
                LeafNodes = [],
                Nodes = Tail0
            ;
                LeafNodes = [_ | _],
                Components = leaf_list(LeafNodes),
                Head = Head0 ^ components := Components,
                Nodes = [Head | Tail0]
            )
        )
    ;
        Components0 = interior_list(Level, InteriorNodes0),
        (
            InteriorNodes0 = [],
            unexpected($pred, "empty InteriorNodes0")
        ;
            InteriorNodes0 = [InteriorHead0 | InteriorTail0],
            remove_least_interior(InteriorHead0, InteriorTail0, Index,
                InteriorNodes),
            (
                InteriorNodes = [],
                Nodes = Tail0
            ;
                InteriorNodes = [_ | _],
                Components = interior_list(Level, InteriorNodes),
                Head = Head0 ^ components := Components,
                Nodes = [Head | Tail0]
            )
        )
    ).

:- pred remove_least_leaf(leaf_node::in, list(leaf_node)::in, int::out,
    list(leaf_node)::out) is det.

remove_least_leaf(Head0, Tail0, Index, Nodes) :-
    Bits0 = Head0 ^ leaf_bits,
    Offset = Head0 ^ leaf_offset,
    Bit = find_least_bit(Bits0),
    Bits = clear_bit(Bits0, Bit),
    Index = Offset + Bit,
    ( if Bits = 0u then
        Nodes = Tail0
    else
        Nodes = [make_leaf_node(Offset, Bits) | Tail0]
    ).

:- func find_least_bit(uint) = int.

find_least_bit(Bits0) = BitNum :-
    Size = bits_per_int,
    BitNum0 = 0,
    BitNum = find_least_bit_2(Bits0, Size, BitNum0).

:- func find_least_bit_2(uint, int, int) = int.

find_least_bit_2(Bits0, Size, BitNum0) = BitNum :-
    ( if Size = 1 then
        % We can't get here unless the bit is a 1 bit.
        BitNum = BitNum0
    else
        HalfSize = unchecked_right_shift(Size, 1),
        Mask = mask(HalfSize),

        LowBits = Bits0 /\ Mask,
        ( if LowBits = 0u then
            HighBits = Mask /\ unchecked_right_shift(Bits0, HalfSize),
            BitNum = find_least_bit_2(HighBits, HalfSize, BitNum0 + HalfSize)
        else
            BitNum = find_least_bit_2(LowBits, HalfSize, BitNum0)
        )
    ).

%---------------------------------------------------------------------------%

equal(SetA, SetB) :-
    trace [compile_time(flag("tree-bitset-integrity"))] (
        ( if
            to_sorted_list(SetA, ListA),
            to_sorted_list(SetB, ListB),
            (
                 SetA = SetB
            <=>
                 ListA = ListB
            )
         then
             true
        else
             unexpected($pred, "set and list equality differ")
        )
    ),
    SetA = SetB.

subset(Subset, Set) :-
    intersect(Set, Subset, Subset).

superset(Superset, Set) :-
    subset(Set, Superset).

%---------------------------------------------------------------------------%

union(SetA, SetB) = Set :-
    SetA = tree_bitset(ListA),
    SetB = tree_bitset(ListB),
    (
        ListA = leaf_list(LeafNodesA),
        ListB = leaf_list(LeafNodesB),
        (
            LeafNodesA = [],
            LeafNodesB = [],
            List = ListA  % or ListB
        ;
            LeafNodesA = [_ | _],
            LeafNodesB = [],
            List = ListA
        ;
            LeafNodesA = [],
            LeafNodesB = [_ | _],
            List = ListB
        ;
            LeafNodesA = [FirstNodeA | LaterNodesA],
            LeafNodesB = [FirstNodeB | LaterNodesB],
            range_of_parent_node(FirstNodeA ^ leaf_offset, 0,
                ParentInitOffsetA, ParentLimitOffsetA),
            range_of_parent_node(FirstNodeB ^ leaf_offset, 0,
                ParentInitOffsetB, ParentLimitOffsetB),
            ( if ParentInitOffsetA = ParentInitOffsetB then
                trace [compile_time(flag("tree-bitset-checks"))] (
                    expect(unify(ParentLimitOffsetA, ParentLimitOffsetB),
                        $pred, "limit mismatch")
                ),
                leaflist_union(LeafNodesA, LeafNodesB, LeafNodes),
                List = leaf_list(LeafNodes)
            else
                raise_leaves_to_interior(FirstNodeA, LaterNodesA,
                    InteriorNodeA),
                raise_leaves_to_interior(FirstNodeB, LaterNodesB,
                    InteriorNodeB),
                interiornode_union(1, InteriorNodeA, [],
                    1, InteriorNodeB, [], Level, InteriorNodes),
                List = interior_list(Level, InteriorNodes)
            )
        )
    ;
        ListA = leaf_list(LeafNodesA),
        ListB = interior_list(LevelB, InteriorNodesB),
        (
            LeafNodesA = [],
            List = ListB
        ;
            LeafNodesA = [FirstNodeA | LaterNodesA],
            raise_leaves_to_interior(FirstNodeA, LaterNodesA, InteriorNodeA),
            head_and_tail(InteriorNodesB, InteriorHeadB, InteriorTailB),
            interiornode_union(1, InteriorNodeA, [],
                LevelB, InteriorHeadB, InteriorTailB, Level, InteriorNodes),
            List = interior_list(Level, InteriorNodes)
        )
    ;
        ListA = interior_list(LevelA, InteriorNodesA),
        ListB = leaf_list(LeafNodesB),
        (
            LeafNodesB = [],
            List = ListA
        ;
            LeafNodesB = [FirstNodeB | LaterNodesB],
            raise_leaves_to_interior(FirstNodeB, LaterNodesB, InteriorNodeB),
            head_and_tail(InteriorNodesA, InteriorHeadA, InteriorTailA),
            interiornode_union(LevelA, InteriorHeadA, InteriorTailA,
                1, InteriorNodeB, [], Level, InteriorNodes),
            List = interior_list(Level, InteriorNodes)
        )
    ;
        ListA = interior_list(LevelA, InteriorNodesA),
        ListB = interior_list(LevelB, InteriorNodesB),
        head_and_tail(InteriorNodesA, InteriorHeadA, InteriorTailA),
        head_and_tail(InteriorNodesB, InteriorHeadB, InteriorTailB),
        interiornode_union(LevelA, InteriorHeadA, InteriorTailA,
            LevelB, InteriorHeadB, InteriorTailB,
            Level, InteriorNodes),
        List = interior_list(Level, InteriorNodes)
    ),
    Set = wrap_tree_bitset(List).

union(A, B, union(A, B)).

:- pred interiornode_union(
    int::in, interior_node::in, list(interior_node)::in,
    int::in, interior_node::in, list(interior_node)::in,
    int::out, list(interior_node)::out) is det.

interiornode_union(LevelA, HeadA, TailA, LevelB, HeadB, TailB, Level, List) :-
    int.max(LevelA, LevelB, LevelAB),
    raise_interiors_to_level(LevelAB, LevelA, HeadA, TailA,
        RaisedHeadA, RaisedTailA),
    raise_interiors_to_level(LevelAB, LevelB, HeadB, TailB,
        RaisedHeadB, RaisedTailB),
    raise_to_common_level(LevelAB,
        RaisedHeadA, RaisedTailA, RaisedHeadB, RaisedTailB,
        TopHeadA, TopTailA, TopHeadB, TopTailB, Level),
    interiorlist_union([TopHeadA | TopTailA], [TopHeadB | TopTailB], List).

:- pred leaflist_union(list(leaf_node)::in, list(leaf_node)::in,
    list(leaf_node)::out) is det.

leaflist_union([], [], []).
leaflist_union([], ListB @ [_ | _], ListB).
leaflist_union(ListA @ [_ | _], [], ListA).
leaflist_union(ListA @ [HeadA | TailA], ListB @ [HeadB | TailB], List) :-
    OffsetA = HeadA ^ leaf_offset,
    OffsetB = HeadB ^ leaf_offset,
    ( if OffsetA = OffsetB then
        Head = make_leaf_node(OffsetA,
            (HeadA ^ leaf_bits) \/ (HeadB ^ leaf_bits)),
        leaflist_union(TailA, TailB, Tail),
        List = [Head | Tail]
    else if OffsetA < OffsetB then
        leaflist_union(TailA, ListB, Tail),
        List = [HeadA | Tail]
    else
        leaflist_union(ListA, TailB, Tail),
        List = [HeadB | Tail]
    ).

:- pred interiorlist_union(list(interior_node)::in, list(interior_node)::in,
    list(interior_node)::out) is det.

interiorlist_union([], [], []).
interiorlist_union([], ListB @ [_ | _], ListB).
interiorlist_union(ListA @ [_ | _], [], ListA).
interiorlist_union(ListA @ [HeadA | TailA], ListB @ [HeadB | TailB], List) :-
    OffsetA = HeadA ^ init_offset,
    OffsetB = HeadB ^ init_offset,
    ( if OffsetA = OffsetB then
        ComponentsA = HeadA ^ components,
        ComponentsB = HeadB ^ components,
        (
            ComponentsA = leaf_list(LeafListA),
            ComponentsB = leaf_list(LeafListB),
            leaflist_union(LeafListA, LeafListB, LeafList),
            Components = leaf_list(LeafList),
            Head = interior_node(HeadA ^ init_offset, HeadA ^ limit_offset,
                Components)
        ;
            ComponentsA = leaf_list(_LeafListA),
            ComponentsB = interior_list(_LevelB, _InteriorListB),
            unexpected($pred, "inconsistent components")
        ;
            ComponentsA = interior_list(_LevelA, _InteriorListA),
            ComponentsB = leaf_list(_LeafListB),
            unexpected($pred, "inconsistent components")
        ;
            ComponentsA = interior_list(LevelA, InteriorListA),
            ComponentsB = interior_list(LevelB, InteriorListB),
            trace [compile_time(flag("tree-bitset-checks"))] (
                expect(unify(LevelA, LevelB), $pred, "inconsistent levels")
            ),
            interiorlist_union(InteriorListA, InteriorListB, InteriorList),
            Components = interior_list(LevelA, InteriorList),
            Head = interior_node(HeadA ^ init_offset, HeadA ^ limit_offset,
                Components)
        ),
        interiorlist_union(TailA, TailB, Tail),
        List = [Head | Tail]
    else if OffsetA < OffsetB then
        interiorlist_union(TailA, ListB, Tail),
        List = [HeadA | Tail]
    else
        interiorlist_union(ListA, TailB, Tail),
        List = [HeadB | Tail]
    ).

%---------------------%

union_list(Sets) = Set :-
    union_list(Sets, Set).

union_list([], tree_bitset.init).
union_list([Set], Set).
union_list(Sets @ [_, _ | _], Set) :-
    union_list_pass(Sets, [], MergedSets),
    union_list(MergedSets, Set).

    % Union adjacent pairs of sets, so that the resulting list has N sets
    % if the input list has 2N or 2N-1 sets.
    %
    % We keep invoking union_list_pass until it yields a list of only one set.
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
:- pred union_list_pass(list(tree_bitset(T))::in,
    list(tree_bitset(T))::in, list(tree_bitset(T))::out)
    is det.

union_list_pass([], !MergedSets).
union_list_pass([Set], !MergedSets) :-
    !:MergedSets = [Set | !.MergedSets].
union_list_pass([SetA, SetB | Sets0], !MergedSets) :-
    union(SetA, SetB, SetAB),
    !:MergedSets = [SetAB | !.MergedSets],
    union_list_pass(Sets0, !MergedSets).

%---------------------%

intersect(SetA, SetB) = Set :-
    SetA = tree_bitset(ListA),
    SetB = tree_bitset(ListB),
    (
        ListA = leaf_list(LeafNodesA),
        ListB = leaf_list(LeafNodesB),
        (
            LeafNodesA = [],
            LeafNodesB = [],
            List = ListA  % or ListB
        ;
            LeafNodesA = [_ | _],
            LeafNodesB = [],
            List = ListB
        ;
            LeafNodesA = [],
            LeafNodesB = [_ | _],
            List = ListA
        ;
            LeafNodesA = [FirstNodeA | _LaterNodesA],
            LeafNodesB = [FirstNodeB | _LaterNodesB],
            range_of_parent_node(FirstNodeA ^ leaf_offset, 0,
                ParentInitOffsetA, ParentLimitOffsetA),
            range_of_parent_node(FirstNodeB ^ leaf_offset, 0,
                ParentInitOffsetB, ParentLimitOffsetB),
            ( if ParentInitOffsetA = ParentInitOffsetB then
                trace [compile_time(flag("tree-bitset-checks"))] (
                    expect(unify(ParentLimitOffsetA, ParentLimitOffsetB),
                        $pred, "limit mismatch")
                ),
                leaflist_intersect(LeafNodesA, LeafNodesB, LeafNodes),
                List = leaf_list(LeafNodes)
            else
                % The ranges of the two sets do not overlap.
                List = leaf_list([])
            )
        )
    ;
        ListA = leaf_list(LeafNodesA),
        ListB = interior_list(LevelB, InteriorNodesB),
        (
            LeafNodesA = [],
            List = ListA
        ;
            LeafNodesA = [FirstNodeA | LaterNodesA],
            raise_leaves_to_interior(FirstNodeA, LaterNodesA, InteriorNodeA),
            descend_and_intersect(1, InteriorNodeA, LevelB, InteriorNodesB,
                List)
        )
    ;
        ListA = interior_list(LevelA, InteriorNodesA),
        ListB = leaf_list(LeafNodesB),
        (
            LeafNodesB = [],
            List = ListB
        ;
            LeafNodesB = [FirstNodeB | LaterNodesB],
            raise_leaves_to_interior(FirstNodeB, LaterNodesB, InteriorNodeB),
            descend_and_intersect(1, InteriorNodeB, LevelA, InteriorNodesA,
                List)
        )
    ;
        ListA = interior_list(LevelA, InteriorNodesA),
        ListB = interior_list(LevelB, InteriorNodesB),
        ( if LevelA = LevelB then
            interiorlist_intersect(InteriorNodesA, InteriorNodesB,
                InteriorNodes),
            List = interior_list(LevelA, InteriorNodes)
        else
            head_and_tail(InteriorNodesA, InteriorHeadA, InteriorTailA),
            head_and_tail(InteriorNodesB, InteriorHeadB, InteriorTailB),
            % Our basic approach of raising both operands to the same level
            % simplifies the code but searching the larger set for the range
            % of the smaller set and starting the operation there would be more
            % efficient in both time and space.
            interiornode_intersect(LevelA, InteriorHeadA, InteriorTailA,
                LevelB, InteriorHeadB, InteriorTailB, Level, InteriorNodes),
            List = interior_list(Level, InteriorNodes)
        )
    ),
    prune_top_levels(List, PrunedList),
    Set = wrap_tree_bitset(PrunedList).

intersect(A, B, intersect(A, B)).

:- pred leaflist_intersect(list(leaf_node)::in, list(leaf_node)::in,
    list(leaf_node)::out) is det.

leaflist_intersect([], [], []).
leaflist_intersect([], [_ | _], []).
leaflist_intersect([_ | _], [], []).
leaflist_intersect(ListA @ [HeadA | TailA], ListB @ [HeadB | TailB], List) :-
    OffsetA = HeadA ^ leaf_offset,
    OffsetB = HeadB ^ leaf_offset,
    ( if OffsetA = OffsetB then
        Bits = HeadA ^ leaf_bits /\ HeadB ^ leaf_bits,
        ( if Bits = 0u then
            leaflist_intersect(TailA, TailB, List)
        else
            Head = make_leaf_node(OffsetA, Bits),
            leaflist_intersect(TailA, TailB, Tail),
            List = [Head | Tail]
        )
    else if OffsetA < OffsetB then
        leaflist_intersect(TailA, ListB, List)
    else
        leaflist_intersect(ListA, TailB, List)
    ).

:- pred descend_and_intersect(int::in, interior_node::in,
    int::in, list(interior_node)::in, node_list::out) is det.

descend_and_intersect(_LevelA, _InteriorNodeA, _LevelB, [], List) :-
    List = leaf_list([]).
descend_and_intersect(LevelA, InteriorNodeA, LevelB, [HeadB | TailB], List) :-
    ( if
        HeadB ^ init_offset =< InteriorNodeA ^ init_offset,
        InteriorNodeA ^ limit_offset =< HeadB ^ limit_offset
    then
        ( if LevelA = LevelB then
            trace [compile_time(flag("tree-bitset-checks"))] (
                expect(
                    unify(InteriorNodeA ^ init_offset, HeadB ^ init_offset),
                    $pred, "inconsistent inits"),
                expect(
                    unify(InteriorNodeA ^ limit_offset, HeadB ^ limit_offset),
                    $pred, "inconsistent limits")
            ),
            ComponentsA = InteriorNodeA ^ components,
            ComponentsB = HeadB ^ components,
            (
                ComponentsA = leaf_list(LeafNodesA),
                ComponentsB = leaf_list(LeafNodesB),
                leaflist_intersect(LeafNodesA, LeafNodesB, LeafNodes),
                List = leaf_list(LeafNodes)
            ;
                ComponentsA = leaf_list(_),
                ComponentsB = interior_list(_, _),
                unexpected($pred, "inconsistent levels")
            ;
                ComponentsA = interior_list(_, _),
                ComponentsB = leaf_list(_),
                unexpected($pred, "inconsistent levels")
            ;
                ComponentsA = interior_list(_SubLevelA, InteriorNodesA),
                ComponentsB = interior_list(_SubLevelB, InteriorNodesB),
                interiorlist_intersect(InteriorNodesA, InteriorNodesB,
                    InteriorNodes),
                List = interior_list(LevelA, InteriorNodes)
            )
        else
            trace [compile_time(flag("tree-bitset-checks"))] (
                expect(LevelA < LevelB, $pred, "LevelA > LevelB")
            ),
            ComponentsB = HeadB ^ components,
            (
                ComponentsB = leaf_list(_),
                unexpected($pred, "bad ComponentsB")
            ;
                ComponentsB = interior_list(SubLevelB, InteriorNodesB),
                descend_and_intersect(LevelA, InteriorNodeA,
                    SubLevelB, InteriorNodesB, List)
            )
        )
    else
        descend_and_intersect(LevelA, InteriorNodeA, LevelB, TailB, List)
    ).

:- pred interiornode_intersect(
    int::in, interior_node::in, list(interior_node)::in,
    int::in, interior_node::in, list(interior_node)::in,
    int::out, list(interior_node)::out) is det.

interiornode_intersect(LevelA, HeadA, TailA, LevelB, HeadB, TailB,
        Level, List) :-
    int.max(LevelA, LevelB, LevelAB),
    raise_interiors_to_level(LevelAB, LevelA, HeadA, TailA,
        RaisedHeadA, RaisedTailA),
    raise_interiors_to_level(LevelAB, LevelB, HeadB, TailB,
        RaisedHeadB, RaisedTailB),
    raise_to_common_level(LevelAB,
        RaisedHeadA, RaisedTailA, RaisedHeadB, RaisedTailB,
        TopHeadA, TopTailA, TopHeadB, TopTailB, Level),
    interiorlist_intersect([TopHeadA | TopTailA], [TopHeadB | TopTailB], List).

:- pred interiorlist_intersect(
    list(interior_node)::in, list(interior_node)::in,
    list(interior_node)::out) is det.

interiorlist_intersect([], [], []).
interiorlist_intersect([], [_ | _], []).
interiorlist_intersect([_ | _], [], []).
interiorlist_intersect(ListA @ [HeadA | TailA], ListB @ [HeadB | TailB],
        List) :-
    OffsetA = HeadA ^ init_offset,
    OffsetB = HeadB ^ init_offset,
    ( if OffsetA = OffsetB then
        ComponentsA = HeadA ^ components,
        ComponentsB = HeadB ^ components,
        (
            ComponentsA = leaf_list(LeafNodesA),
            ComponentsB = leaf_list(LeafNodesB),
            leaflist_intersect(LeafNodesA, LeafNodesB, LeafNodes),
            (
                LeafNodes = [],
                interiorlist_intersect(TailA, TailB, List)
            ;
                LeafNodes = [_ | _],
                Components = leaf_list(LeafNodes),
                interiorlist_intersect(TailA, TailB, Tail),
                Head = interior_node(HeadA ^ init_offset, HeadA ^ limit_offset,
                    Components),
                List = [Head | Tail]
            )
        ;
            ComponentsA = interior_list(_LevelA, _InteriorNodesA),
            ComponentsB = leaf_list(_LeafNodesB),
            unexpected($pred, "inconsistent components")
        ;
            ComponentsB = interior_list(_LevelB, _InteriorNodesB),
            ComponentsA = leaf_list(_LeafNodesA),
            unexpected($pred, "inconsistent components")
        ;
            ComponentsA = interior_list(LevelA, InteriorNodesA),
            ComponentsB = interior_list(LevelB, InteriorNodesB),
            trace [compile_time(flag("tree-bitset-checks"))] (
                expect(unify(LevelA, LevelB), $pred, "inconsistent levels")
            ),
            interiorlist_intersect(InteriorNodesA, InteriorNodesB,
                InteriorNodes),
            (
                InteriorNodes = [],
                interiorlist_intersect(TailA, TailB, List)
            ;
                InteriorNodes = [_ | _],
                Components = interior_list(LevelA, InteriorNodes),
                interiorlist_intersect(TailA, TailB, Tail),
                Head = interior_node(HeadA ^ init_offset, HeadA ^ limit_offset,
                    Components),
                List = [Head | Tail]
            )
        )
    else if OffsetA < OffsetB then
        interiorlist_intersect(TailA, ListB, List)
    else
        interiorlist_intersect(ListA, TailB, List)
    ).

%---------------------%

intersect_list(Sets) = Set :-
    intersect_list(Sets, Set).

intersect_list([], tree_bitset.init).
intersect_list([Set], Set).
intersect_list(Sets @ [_, _ | _], Set) :-
    intersect_list_pass(Sets, [], MergedSets),
    intersect_list(MergedSets, Set).

    % Intersect adjacent pairs of sets, so that the resulting list has N sets
    % if the input list has 2N or 2N-1 sets.
    %
    % We keep invoking intersect_list_pass until it yields a list
    % of only one set.
    %
    % The point of this approach is that intersecting a large set with a small
    % set is often only slightly faster than intersecting that large set
    % with another large set, yet it gets significantly less work done.
    % This is because the bitsets in a small set can be expected to be
    % considerably sparser that bitsets in large sets.
    %
    % We expect that this approach should yield performance closer to NlogN
    % than to N^2 when intersecting a list of N sets.
    %
:- pred intersect_list_pass(list(tree_bitset(T))::in,
    list(tree_bitset(T))::in, list(tree_bitset(T))::out) is det.

intersect_list_pass([], !MergedSets).
intersect_list_pass([Set], !MergedSets) :-
    !:MergedSets = [Set | !.MergedSets].
intersect_list_pass([SetA, SetB | Sets0], !MergedSets) :-
    intersect(SetA, SetB, SetAB),
    !:MergedSets = [SetAB | !.MergedSets],
    intersect_list_pass(Sets0, !MergedSets).

%---------------------%

difference(SetA, SetB) = Set :-
    SetA = tree_bitset(ListA),
    SetB = tree_bitset(ListB),
    % Our basic approach of raising both operands to the same level simplifies
    % the code (by allowing the reuse of the basic pattern and the helper
    % predicates of the union predicate), but searching the larger set for the
    % range of the smaller set and starting the operation there would be more
    % efficient in both time and space.
    (
        ListA = leaf_list(LeafNodesA),
        ListB = leaf_list(LeafNodesB),
        (
            LeafNodesA = [],
            List = ListA
        ;
            LeafNodesA = [_ | _],
            LeafNodesB = [],
            List = ListA
        ;
            LeafNodesA = [FirstNodeA | _LaterNodesA],
            LeafNodesB = [FirstNodeB | _LaterNodesB],
            range_of_parent_node(FirstNodeA ^ leaf_offset, 0,
                ParentInitOffsetA, ParentLimitOffsetA),
            range_of_parent_node(FirstNodeB ^ leaf_offset, 0,
                ParentInitOffsetB, ParentLimitOffsetB),
            ( if ParentInitOffsetA = ParentInitOffsetB then
                trace [compile_time(flag("tree-bitset-checks"))] (
                    expect(unify(ParentLimitOffsetA, ParentLimitOffsetB),
                        $pred, "limit mismatch")
                ),
                leaflist_difference(LeafNodesA, LeafNodesB, LeafNodes),
                List = leaf_list(LeafNodes)
            else
                % The ranges of the two sets do not overlap.
                List = ListA
            )
        )
    ;
        ListA = leaf_list(LeafNodesA),
        ListB = interior_list(LevelB, InteriorNodesB),
        (
            LeafNodesA = [],
            List = ListA
        ;
            LeafNodesA = [FirstNodeA | _LaterNodesA],
            range_of_parent_node(FirstNodeA ^ leaf_offset, 0,
                ParentInitOffsetA, ParentLimitOffsetA),
            find_leaf_nodes_at_parent_offset(LevelB, InteriorNodesB,
                ParentInitOffsetA, ParentLimitOffsetA, LeafNodesB),
            leaflist_difference(LeafNodesA, LeafNodesB, LeafNodes),
            List = leaf_list(LeafNodes)
        )
    ;
        ListA = interior_list(LevelA, InteriorNodesA),
        ListB = leaf_list(LeafNodesB),
        (
            LeafNodesB = [],
            List = ListA
        ;
            LeafNodesB = [FirstNodeB | LaterNodesB],
            raise_leaves_to_interior(FirstNodeB, LaterNodesB, InteriorNodeB),
            descend_and_difference_one(LevelA, InteriorNodesA,
                1, InteriorNodeB, Level, InteriorNodes),
            List = interior_list(Level, InteriorNodes)
        )
    ;
        ListA = interior_list(LevelA, InteriorNodesA),
        ListB = interior_list(LevelB, InteriorNodesB),
        ( if LevelA > LevelB then
            head_and_tail(InteriorNodesB, InteriorHeadB, InteriorTailB),
            descend_and_difference_list(LevelA, InteriorNodesA,
                LevelB, InteriorHeadB, InteriorTailB, Level, InteriorNodes),
            List = interior_list(Level, InteriorNodes)
        else if LevelA = LevelB then
            head_and_tail(InteriorNodesA, InteriorHeadA, InteriorTailA),
            head_and_tail(InteriorNodesB, InteriorHeadB, InteriorTailB),
            interiornode_difference(LevelA, InteriorHeadA, InteriorTailA,
                LevelB, InteriorHeadB, InteriorTailB, Level, InteriorNodes),
            List = interior_list(Level, InteriorNodes)
        else
            % LevelA < LevelB
            head_and_tail(InteriorNodesA, InteriorHeadA, InteriorTailA),
            range_of_parent_node(InteriorHeadA ^ init_offset, LevelA,
                ParentInitOffsetA, ParentLimitOffsetA),
            ParentLevelA = LevelA + 1,
            % Find the list of nodes in B that are at LevelA, covering
            % the same range as A's parent node would cover. These are the
            % only nodes in B at Level A that InteriorNodesA can overlap with.
            find_interior_nodes_at_parent_offset(LevelB, InteriorNodesB,
                ParentLevelA, ParentInitOffsetA, ParentLimitOffsetA,
                SelectedNodesB),
            (
                SelectedNodesB = [],
                List = ListA
            ;
                SelectedNodesB = [SelectedHeadB | SelectedTailB],
                SelectedLevelB = LevelA,
                interiornode_difference(LevelA, InteriorHeadA, InteriorTailA,
                    SelectedLevelB, SelectedHeadB, SelectedTailB,
                    Level, InteriorNodes),
                List = interior_list(Level, InteriorNodes)
            )
        )
    ),
    prune_top_levels(List, PrunedList),
    Set = wrap_tree_bitset(PrunedList).

difference(A, B, difference(A, B)).

:- pred find_leaf_nodes_at_parent_offset(int::in, list(interior_node)::in,
    int::in, int::in, list(leaf_node)::out) is det.

find_leaf_nodes_at_parent_offset(_LevelB, [],
        _ParentInitOffsetA, _ParentLimitOffsetA, []).
find_leaf_nodes_at_parent_offset(LevelB, [HeadB | TailB],
        ParentInitOffsetA, ParentLimitOffsetA, LeafNodesB) :-
    ( if HeadB ^ init_offset > ParentInitOffsetA then
        % The leaf nodes in ListA cover at most one level 1 interior node's
        % span of bits. Call that the hypothetical level 1 interior node.
        % HeadB ^ init_offset should be a multiple of (a power of) that span,
        % so if it is greater than the initial offset of that hypothetical
        % node, then it should be greater than the final offset of that
        % hypothetical node as well. The limit offset is one bigger than
        % the final offset.
        trace [compile_time(flag("tree-bitset-checks"))] (
            ( if HeadB ^ init_offset >= ParentLimitOffsetA then
                true
            else
                unexpected($pred, "screwed-up offsets")
            )
        ),
        LeafNodesB = []
    else if ParentInitOffsetA < HeadB ^ limit_offset then
        % ListA's range is inside HeadB's range.
        HeadNodeListB = HeadB ^ components,
        (
            HeadNodeListB = leaf_list(HeadLeafNodesB),
            trace [compile_time(flag("tree-bitset-checks"))] (
                expect(unify(LevelB, 1), $pred, "LevelB != 1")
            ),
            LeafNodesB = HeadLeafNodesB
        ;
            HeadNodeListB = interior_list(HeadSubLevelB, HeadInteriorNodesB),
            trace [compile_time(flag("tree-bitset-checks"))] (
                expect_not(unify(LevelB, 1), $pred, "LevelB = 1"),
                expect(unify(HeadSubLevelB, LevelB - 1), $pred,
                    "HeadSubLevelB != LevelB - 1")
            ),
            find_leaf_nodes_at_parent_offset(HeadSubLevelB, HeadInteriorNodesB,
                ParentInitOffsetA, ParentLimitOffsetA, LeafNodesB)
        )
    else
        find_leaf_nodes_at_parent_offset(LevelB, TailB,
            ParentInitOffsetA, ParentLimitOffsetA, LeafNodesB)
    ).

:- pred find_interior_nodes_at_parent_offset(int::in, list(interior_node)::in,
    int::in, int::in, int::in, list(interior_node)::out) is det.

find_interior_nodes_at_parent_offset(_LevelB, [],
        _ParentLevelA, _ParentInitOffsetA, _ParentLimitOffsetA, []).
find_interior_nodes_at_parent_offset(LevelB, [HeadB | TailB],
        ParentLevelA, ParentInitOffsetA, ParentLimitOffsetA, NodesB) :-
    ( if LevelB > ParentLevelA then
        ( if HeadB ^ init_offset > ParentInitOffsetA then
            % ListA's range is before HeadB's range.
            % The nodes in ListA cover at most one level ParentLevelA interior
            % node's span of bits. Call that the hypothetical level
            % ParentLevelA interior node. HeadB ^ init_offset should be a
            % multiple of (a power of) that span, so if it is greater than
            % the initial offset of that hypothetical node, then it should be
            % greater than the final offset of that hypothetical node as well.
            % The limit offset is one bigger than the final offset.
            trace [compile_time(flag("tree-bitset-checks"))] (
                ( if HeadB ^ init_offset >= ParentLimitOffsetA then
                    true
                else
                    unexpected($pred, "screwed-up offsets")
                )
            ),
            NodesB = []
        else if ParentInitOffsetA < HeadB ^ limit_offset then
            % ListA's range is inside HeadB's range.
            HeadNodeListB = HeadB ^ components,
            (
                HeadNodeListB = leaf_list(_),
                unexpected($pred, "HeadNodeListB is a leaf list")
            ;
                HeadNodeListB = interior_list(HeadSubLevelB,
                    HeadInteriorNodesB),
                trace [compile_time(flag("tree-bitset-checks"))] (
                    expect_not(unify(LevelB, 1), $pred, "LevelB = 1"),
                    expect(unify(HeadSubLevelB, LevelB - 1), $pred,
                        "HeadSubLevelB != LevelB - 1")
                ),
                find_interior_nodes_at_parent_offset(HeadSubLevelB,
                    HeadInteriorNodesB,
                    ParentLevelA, ParentInitOffsetA, ParentLimitOffsetA,
                    NodesB)
            )
        else
            % ListA's range is after HeadB's range.
            find_interior_nodes_at_parent_offset(LevelB, TailB,
                ParentLevelA, ParentInitOffsetA, ParentLimitOffsetA, NodesB)
        )
    else
        trace [compile_time(flag("tree-bitset-checks"))] (
            expect(unify(ParentLevelA, LevelB), $pred,
                "ParentLevelA != LevelB")
        ),
        ( if HeadB ^ init_offset > ParentInitOffsetA then
            % ListA's range is before HeadB's range.
            % The nodes in ListA cover at most one level ParentLevelA interior
            % node's span of bits. Call that the hypothetical level
            % ParentLevelA interior node. HeadB ^ init_offset should be a
            % multiple of (a power of) that span, so if it is greater than
            % the initial offset of that hypothetical node, then it should be
            % greater than the final offset of that hypothetical node as well.
            % The limit offset is one bigger than the final offset.
            trace [compile_time(flag("tree-bitset-checks"))] (
                ( if HeadB ^ init_offset >= ParentLimitOffsetA then
                    true
                else
                    unexpected($pred, "screwed-up offsets")
                )
            ),
            NodesB = []
        else if HeadB ^ init_offset = ParentInitOffsetA then
            ComponentsB = HeadB ^ components,
            (
                ComponentsB = leaf_list(_),
                unexpected($pred, "leaf_list")
            ;
                ComponentsB = interior_list(_, NodesB)
            )
        else
            find_interior_nodes_at_parent_offset(LevelB, TailB,
                ParentLevelA, ParentInitOffsetA, ParentLimitOffsetA, NodesB)
        )
    ).

:- pred descend_and_difference_one(int::in, list(interior_node)::in,
    int::in, interior_node::in, int::out, list(interior_node)::out) is det.

descend_and_difference_one(LevelA, InteriorNodesA, LevelB, InteriorNodeB,
        Level, List) :-
    ( if LevelA > LevelB then
        (
            InteriorNodesA = [],
            Level = LevelA,
            List = []
        ;
            InteriorNodesA = [HeadA | TailA],
            ( if HeadA ^ limit_offset =< InteriorNodeB ^ init_offset then
                % All of the region covered by HeadA is before the region
                % covered by InteriorNodeB.
                descend_and_difference_one(LevelA, TailA,
                    LevelB, InteriorNodeB, LevelTail, ListTail),
                trace [compile_time(flag("tree-bitset-checks"))] (
                    expect(unify(LevelTail, LevelA), $pred,
                        "LevelTail != LevelA")
                ),
                Level = LevelA,
                List = [HeadA | ListTail]
            else if HeadA ^ init_offset =< InteriorNodeB ^ init_offset then
                % The region covered by HeadA contains the region
                % covered by InteriorNodeB.
                trace [compile_time(flag("tree-bitset-checks"))] (
                    ( if
                        InteriorNodeB ^ limit_offset =< HeadA ^ limit_offset
                    then
                        true
                    else
                        unexpected($pred, "weird region relationship")
                    )
                ),
                (
                    HeadA ^ components = leaf_list(_),
                    % LevelB is at least 1, LevelA is greater than LevelB,
                    % so stepping one level down from levelA should get us
                    % to at least level 1; level 0 cannot happen.
                    unexpected($pred, "HeadA ^ components is leaf_list")
                ;
                    HeadA ^ components = interior_list(HeadASubLevel,
                        HeadASubNodes)
                ),
                descend_and_difference_one(HeadASubLevel, HeadASubNodes,
                    LevelB, InteriorNodeB, LevelSub, ListSub),
                (
                    ListSub = [],
                    Level = LevelA,
                    List = TailA
                ;
                    ListSub = [ListSubHead | ListSubTail],
                    raise_interiors_to_level(LevelA, LevelSub,
                        ListSubHead, ListSubTail, RaisedHead, RaisedTail),
                    % We are here because LevelA > LevelB. By construction,
                    % LevelA > HeadASubLevel, and HeadASubLevel >= LevelSub.
                    % Since LevelA > LevelSub, when we raise ListSub to LevelA,
                    % all its nodes should have been gathered under a single
                    % LevelA interior node, RaisedHead.
                    trace [compile_time(flag("tree-bitset-checks"))] (
                        expect(unify(RaisedTail, []), $pred,
                            "RaisedTail != []")
                    ),
                    Level = LevelA,
                    List = [RaisedHead | TailA]
                )
            else
                % All of the region covered by HeadA is after the region
                % covered by InteriorNodeB, and therefore so are all the
                % regions covered by TailA.
                Level = LevelA,
                List = InteriorNodesA
            )
        )
    else if LevelA = LevelB then
        interiorlist_difference(InteriorNodesA, [InteriorNodeB], List),
        Level = LevelA
    else
        unexpected($pred, "LevelA < LevelB")
    ).

:- pred descend_and_difference_list(int::in, list(interior_node)::in,
    int::in, interior_node::in, list(interior_node)::in,
    int::out, list(interior_node)::out) is det.

descend_and_difference_list(LevelA, InteriorNodesA,
        LevelB, InteriorNodeB, InteriorNodesB, Level, List) :-
    ( if LevelA > LevelB then
        (
            InteriorNodesA = [],
            Level = LevelA,
            List = []
        ;
            InteriorNodesA = [HeadA | TailA],
            ( if HeadA ^ limit_offset =< InteriorNodeB ^ init_offset then
                % All of the region covered by HeadA is before the region
                % covered by [InteriorNodeB | InteriorNodesB].
                trace [compile_time(flag("tree-bitset-checks"))] (
                    list.det_last([InteriorNodeB | InteriorNodesB], LastB),
                    expect((HeadA ^ limit_offset =< LastB ^ init_offset),
                        $pred, "HeadA ^ limit_offset > LastB ^ init_offset")
                ),
                descend_and_difference_list(LevelA, TailA,
                    LevelB, InteriorNodeB, InteriorNodesB,
                    LevelTail, ListTail),
                trace [compile_time(flag("tree-bitset-checks"))] (
                    expect(unify(LevelTail, LevelA), $pred,
                        "LevelTail != LevelA")
                ),
                Level = LevelA,
                List = [HeadA | ListTail]
            else if HeadA ^ init_offset =< InteriorNodeB ^ init_offset then
                % The region covered by HeadA contains the region
                % covered by InteriorNodeB.
                trace [compile_time(flag("tree-bitset-checks"))] (
                    ( if
                        InteriorNodeB ^ limit_offset =< HeadA ^ limit_offset
                    then
                        true
                    else
                        unexpected($pred, "weird region relationship")
                    )
                ),
                (
                    HeadA ^ components = leaf_list(_),
                    % LevelB is at least 1, LevelA is greater than LevelB,
                    % so stepping one level down from levelA should get us
                    % to at least level 1; level 0 cannot happen.
                    unexpected($pred, "HeadA ^ components is leaf_list")
                ;
                    HeadA ^ components = interior_list(HeadASubLevel,
                        HeadASubNodes)
                ),
                descend_and_difference_list(HeadASubLevel, HeadASubNodes,
                    LevelB, InteriorNodeB, InteriorNodesB, LevelSub, ListSub),
                (
                    ListSub = [],
                    Level = LevelA,
                    List = TailA
                ;
                    ListSub = [ListSubHead | ListSubTail],
                    raise_interiors_to_level(LevelA, LevelSub,
                        ListSubHead, ListSubTail, RaisedHead, RaisedTail),
                    % We are here because LevelA > LevelB. By construction,
                    % LevelA > HeadASubLevel, and HeadASubLevel >= LevelSub.
                    % Since LevelA > LevelSub, when we raise ListSub to LevelA,
                    % all its nodes should have been gathered under a single
                    % LevelA interior node, RaisedHead.
                    trace [compile_time(flag("tree-bitset-checks"))] (
                        expect(unify(RaisedTail, []), $pred,
                            "RaisedTail != []")
                    ),
                    Level = LevelA,
                    List = [RaisedHead | TailA]
                )
            else
                % All of the region covered by HeadA is after the region
                % covered by InteriorNodeB, and therefore so are all the
                % regions covered by TailA.
                Level = LevelA,
                List = InteriorNodesA
            )
        )
    else if LevelA = LevelB then
        interiorlist_difference(InteriorNodesA,
            [InteriorNodeB | InteriorNodesB], List),
        Level = LevelA
    else
        unexpected($pred, "LevelA < LevelB")
    ).

:- pred interiornode_difference(
    int::in, interior_node::in, list(interior_node)::in,
    int::in, interior_node::in, list(interior_node)::in,
    int::out, list(interior_node)::out) is det.

interiornode_difference(LevelA, HeadA, TailA, LevelB, HeadB, TailB,
        Level, List) :-
    trace [compile_time(flag("tree-bitset-checks"))] (
        expect(unify(LevelA, LevelB), $pred, "level mismatch")
    ),
    range_of_parent_node(HeadA ^ init_offset, LevelA,
        ParentInitOffsetA, ParentLimitOffsetA),
    range_of_parent_node(HeadB ^ init_offset, LevelB,
        ParentInitOffsetB, ParentLimitOffsetB),
    ( if ParentInitOffsetA = ParentInitOffsetB then
        trace [compile_time(flag("tree-bitset-checks"))] (
            expect(unify(ParentLimitOffsetA, ParentLimitOffsetB), $pred,
                "limit mismatch")
        ),
        interiorlist_difference([HeadA | TailA], [HeadB | TailB], List),
        Level = LevelA
    else
        (
            TailA = [],
            List = [HeadA],
            Level = LevelA
        ;
            TailA = [HeadTailA | TailTailA],
            interiornode_difference(LevelA, HeadTailA, TailTailA,
                LevelA, HeadB, TailB, Level, Tail),
            trace [compile_time(flag("tree-bitset-checks"))] (
                expect(unify(LevelA, Level), $pred,
                    "final level mismatch")
            ),
            List = [HeadA | Tail]
        )
    ).

:- pred leaflist_difference(list(leaf_node)::in, list(leaf_node)::in,
    list(leaf_node)::out) is det.

leaflist_difference([], [], []).
leaflist_difference([], [_ | _], []).
leaflist_difference(ListA @ [_ | _], [], ListA).
leaflist_difference(ListA @ [HeadA | TailA], ListB @ [HeadB | TailB], List) :-
    OffsetA = HeadA ^ leaf_offset,
    OffsetB = HeadB ^ leaf_offset,
    ( if OffsetA = OffsetB then
        Bits = (HeadA ^ leaf_bits) /\ \ (HeadB ^ leaf_bits),
        ( if Bits = 0u then
            leaflist_difference(TailA, TailB, List)
        else
            Head = make_leaf_node(OffsetA, Bits),
            leaflist_difference(TailA, TailB, Tail),
            List = [Head | Tail]
        )
    else if OffsetA < OffsetB then
        leaflist_difference(TailA, ListB, Tail),
        List = [HeadA | Tail]
    else
        leaflist_difference(ListA, TailB, List)
    ).

:- pred interiorlist_difference(
    list(interior_node)::in, list(interior_node)::in,
    list(interior_node)::out) is det.

interiorlist_difference([], [], []).
interiorlist_difference([], [_ | _], []).
interiorlist_difference(ListA @ [_ | _], [], ListA).
interiorlist_difference(ListA @ [HeadA | TailA], ListB @ [HeadB | TailB],
        List) :-
    OffsetA = HeadA ^ init_offset,
    OffsetB = HeadB ^ init_offset,
    ( if OffsetA = OffsetB then
        ComponentsA = HeadA ^ components,
        ComponentsB = HeadB ^ components,
        (
            ComponentsA = leaf_list(LeafNodesA),
            ComponentsB = leaf_list(LeafNodesB),
            leaflist_difference(LeafNodesA, LeafNodesB, LeafNodes),
            (
                LeafNodes = [],
                interiorlist_difference(TailA, TailB, List)
            ;
                LeafNodes = [_ | _],
                Components = leaf_list(LeafNodes),
                interiorlist_difference(TailA, TailB, Tail),
                Head = interior_node(HeadA ^ init_offset, HeadA ^ limit_offset,
                    Components),
                List = [Head | Tail]
            )
        ;
            ComponentsA = interior_list(_LevelA, _InteriorNodesA),
            ComponentsB = leaf_list(_LeafNodesB),
            unexpected($pred, "inconsistent components")
        ;
            ComponentsB = interior_list(_LevelB, _InteriorNodesB),
            ComponentsA = leaf_list(_LeafNodesA),
            unexpected($pred, "inconsistent components")
        ;
            ComponentsA = interior_list(LevelA, InteriorNodesA),
            ComponentsB = interior_list(LevelB, InteriorNodesB),
            trace [compile_time(flag("tree-bitset-checks"))] (
                expect(unify(LevelA, LevelB), $pred, "inconsistent levels")
            ),
            interiorlist_difference(InteriorNodesA, InteriorNodesB,
                InteriorNodes),
            (
                InteriorNodes = [],
                interiorlist_difference(TailA, TailB, List)
            ;
                InteriorNodes = [_ | _],
                Components = interior_list(LevelA, InteriorNodes),
                interiorlist_difference(TailA, TailB, Tail),
                Head = interior_node(HeadA ^ init_offset, HeadA ^ limit_offset,
                    Components),
                List = [Head | Tail]
            )
        )
    else if OffsetA < OffsetB then
        interiorlist_difference(TailA, ListB, Tail),
        List = [HeadA | Tail]
    else
        interiorlist_difference(ListA, TailB, List)
    ).

%---------------------------------------------------------------------------%

divide(Pred, Set, InSet, OutSet) :-
    Set = tree_bitset(List),
    (
        List = leaf_list(LeafNodes),
        leaflist_divide(Pred, LeafNodes, InList, OutList),
        InSet = wrap_tree_bitset(leaf_list(InList)),
        OutSet = wrap_tree_bitset(leaf_list(OutList))
    ;
        List = interior_list(Level, InteriorNodes),
        interiornode_divide(Pred, InteriorNodes,
            InInteriorNodes, OutInteriorNodes),

        InList = interior_list(Level, InInteriorNodes),
        prune_top_levels(InList, PrunedInList),
        InSet = wrap_tree_bitset(PrunedInList),

        OutList = interior_list(Level, OutInteriorNodes),
        prune_top_levels(OutList, PrunedOutList),
        OutSet = wrap_tree_bitset(PrunedOutList)
    ).

:- pred leaflist_divide(pred(T)::in(pred(in) is semidet), list(leaf_node)::in,
    list(leaf_node)::out, list(leaf_node)::out) is det <= enum(T).

leaflist_divide(_Pred, [], [], []).
leaflist_divide(Pred, [Head | Tail], InList, OutList) :-
    leaflist_divide(Pred, Tail, InTail, OutTail),
    Head = leaf_node(Offset, Bits),
    leafnode_divide(Pred, Offset, 0, Bits, 0u, InBits, 0u, OutBits),
    ( if InBits = 0u then
        InList = InTail
    else
        InHead = make_leaf_node(Offset, InBits),
        InList = [InHead | InTail]
    ),
    ( if OutBits = 0u then
        OutList = OutTail
    else
        OutHead = make_leaf_node(Offset, OutBits),
        OutList = [OutHead | OutTail]
    ).

:- pred leafnode_divide(pred(T)::in(pred(in) is semidet), int::in, int::in,
    uint::in, uint::in, uint::out, uint::in, uint::out) is det <= enum(T).

leafnode_divide(Pred, Offset, WhichBit, Bits, !InBits, !OutBits) :-
    ( if WhichBit < bits_per_int then
        SelectedBit = get_bit(Bits, WhichBit),
        ( if SelectedBit = 0u then
            true
        else
            Elem = index_to_enum(Offset + WhichBit),
            ( if Pred(Elem) then
                !:InBits = set_bit(!.InBits, WhichBit)
            else
                !:OutBits = set_bit(!.OutBits, WhichBit)
            )
        ),
        leafnode_divide(Pred, Offset, WhichBit + 1, Bits, !InBits, !OutBits)
    else
        true
    ).

:- pred interiornode_divide(pred(T)::in(pred(in) is semidet),
    list(interior_node)::in,
    list(interior_node)::out, list(interior_node)::out) is det <= enum(T).

interiornode_divide(_Pred, [], [], []).
interiornode_divide(Pred, [Head | Tail], InNodes, OutNodes) :-
    interiornode_divide(Pred, Tail, InTail, OutTail),
    Head = interior_node(InitOffset, LimitOffset, SubNodes),
    (
        SubNodes = leaf_list(SubLeafNodes),
        leaflist_divide(Pred, SubLeafNodes, InLeafNodes, OutLeafNodes),
        (
            InLeafNodes = [],
            InNodes = InTail
        ;
            InLeafNodes = [_ | _],
            InHead = interior_node(InitOffset, LimitOffset,
                leaf_list(InLeafNodes)),
            InNodes = [InHead | InTail]
        ),
        (
            OutLeafNodes = [],
            OutNodes = OutTail
        ;
            OutLeafNodes = [_ | _],
            OutHead = interior_node(InitOffset, LimitOffset,
                leaf_list(OutLeafNodes)),
            OutNodes = [OutHead | OutTail]
        )
    ;
        SubNodes = interior_list(Level, SubInteriorNodes),
        interiornode_divide(Pred, SubInteriorNodes,
            InSubInteriorNodes, OutSubInteriorNodes),
        (
            InSubInteriorNodes = [],
            InNodes = InTail
        ;
            InSubInteriorNodes = [_ | _],
            InHead = interior_node(InitOffset, LimitOffset,
                interior_list(Level, InSubInteriorNodes)),
            InNodes = [InHead | InTail]
        ),
        (
            OutSubInteriorNodes = [],
            OutNodes = OutTail
        ;
            OutSubInteriorNodes = [_ | _],
            OutHead = interior_node(InitOffset, LimitOffset,
                interior_list(Level, OutSubInteriorNodes)),
            OutNodes = [OutHead | OutTail]
        )
    ).

%---------------------%

divide_by_set(DivideBySet, Set, InSet, OutSet) :-
    DivideBySet = tree_bitset(DivideByList),
    Set = tree_bitset(List),
    (
        DivideByList = leaf_list(DBLeafNodes),
        List = leaf_list(LeafNodes),
        leaflist_divide_by_set(DBLeafNodes, LeafNodes,
            InNodes, OutNodes),
        InList = leaf_list(InNodes),
        OutList = leaf_list(OutNodes),
        InSet = wrap_tree_bitset(InList),
        OutSet = wrap_tree_bitset(OutList)
    ;
        DivideByList = interior_list(DBLevel, DBNodes),
        List = leaf_list(LeafNodes),
        (
            LeafNodes = [],
            % The set we are dividing is empty, so both InSet and OutSet
            % must be empty too.
            InSet = Set,
            OutSet = Set
        ;
            LeafNodes = [leaf_node(FirstOffset, _) | _],
            range_of_parent_node(FirstOffset, 0, InitOffset, LimitOffset),
            head_and_tail(DBNodes, DBNodesHead, _),
            DBNodesHead = interior_node(DBFirstInitOffset, _, _),
            range_of_parent_node(DBFirstInitOffset, DBLevel,
                DBInitOffset, DBLimitOffset),
            ( if
                DBInitOffset =< InitOffset,
                InitOffset < DBLimitOffset
            then
                ( if
                    DBInitOffset < LimitOffset,
                    LimitOffset =< DBLimitOffset
                then
                    true
                else
                    unexpected($pred, "strange offsets")
                ),
                divide_by_set_descend_divide_by(DBLevel, DBNodes,
                    0, InitOffset, LimitOffset, List, InList0, OutList0),
                prune_top_levels(InList0, InList),
                prune_top_levels(OutList0, OutList),
                InSet = wrap_tree_bitset(InList),
                OutSet = wrap_tree_bitset(OutList)
            else
                % The ranges of the two sets do not overlap.
                InSet = wrap_tree_bitset(leaf_list([])),
                OutSet = Set
            )
        )
    ;
        DivideByList = leaf_list(_),
        List = interior_list(_, _),
        % XXX Should have specialized code here that traverses Set
        % just once. This will require something analogous to
        % divide_by_set_descend_divide_by, but descending List
        % instead of DivideByList.
        intersect(DivideBySet, Set, InSet),
        difference(Set, InSet, OutSet)
    ;
        DivideByList = interior_list(DBLevel, DBNodes),
        List = interior_list(Level, Nodes),
        ( if DBLevel = Level then
            interiorlist_divide_by_set(Level, DBNodes, Nodes,
                InNodes, OutNodes),
            (
                InNodes = [],
                InList = leaf_list([])
            ;
                InNodes = [_ | _],
                InList0 = interior_list(Level, InNodes),
                prune_top_levels(InList0, InList)
            ),
            (
                OutNodes = [],
                OutList = leaf_list([])
            ;
                OutNodes = [_ | _],
                OutList0 = interior_list(Level, OutNodes),
                prune_top_levels(OutList0, OutList)
            ),
            InSet = wrap_tree_bitset(InList),
            OutSet = wrap_tree_bitset(OutList)
        else if DBLevel > Level then
            head_and_tail(Nodes, NodesHead, _),
            NodesHead = interior_node(FirstInitOffset, _, _),
            range_of_parent_node(FirstInitOffset, Level,
                InitOffset, LimitOffset),
            divide_by_set_descend_divide_by(DBLevel, DBNodes,
                Level, InitOffset, LimitOffset, List, InList0, OutList0),
            prune_top_levels(InList0, InList),
            prune_top_levels(OutList0, OutList),
            InSet = wrap_tree_bitset(InList),
            OutSet = wrap_tree_bitset(OutList)
        else
            % XXX Should have specialized code here that traverses Set
            % just once. This will require something analogous to
            % divide_by_set_descend_divide_by, but descending List
            % instead of DivideByList.
            intersect(DivideBySet, Set, InSet),
            difference(Set, InSet, OutSet)
        )
    ).

:- pred divide_by_set_descend_divide_by(int::in,
    list(interior_node)::in, int::in, int::in, int::in, node_list::in,
    node_list::out, node_list::out) is det.

divide_by_set_descend_divide_by(DBLevel, DBNodes,
        Level, InitOffset, LimitOffset, List, InList, OutList) :-
    expect((DBLevel > Level), $pred, "not DBLevel > Level"),
    (
        DBNodes = [],
        % Every node in the original DivideByList is before List.
        InList = leaf_list([]),
        OutList = List
    ;
        DBNodes = [DBNodesHead | DBNodesTail],
        DBNodesHead = interior_node(DBHeadInitOffset, DBHeadLimitOffset,
            DBHeadComponents),
        ( if DBHeadLimitOffset =< InitOffset then
            % DBNodesHead is before List.
            divide_by_set_descend_divide_by(DBLevel, DBNodesTail,
                Level, InitOffset, LimitOffset, List, InList, OutList)
        else if LimitOffset =< DBHeadInitOffset then
            % DBNodesHead is after List, and every other
            % node in the original DivideByList is before List.
            InList = leaf_list([]),
            OutList = List
        else
            % The range of DBNodesHead contains the range of List.
            % Dividing List by DBNodesHead is thus the same as
            % dividing List by the original DivideBySet.
            (
                DBHeadComponents = leaf_list(DBHeadLeafNodes),
                expect(unify(DBLevel, 1), $pred, "DBLevel != 1"),
                expect(unify(Level, 0), $pred, "Level != 0"),
                % The other nodes in the original DivideByList are all
                % outside the range of List.
                (
                    List = leaf_list(Nodes),
                    leaflist_divide_by_set(DBHeadLeafNodes, Nodes,
                        InNodes, OutNodes),
                    InList = leaf_list(InNodes),
                    OutList = leaf_list(OutNodes)
                ;
                    List = interior_list(_, _),
                    % divide_by_set_descend_divide_by should have stopped
                    % recursing when it got to Level.
                    unexpected($pred, "List is not leaf_list")
                )
            ;
                DBHeadComponents = interior_list(DBSubLevel, DBSubNodes),
                expect(unify(DBLevel, DBSubLevel + 1), $pred,
                    "DBLevel != SubLevel + 1"),
                ( if DBSubLevel > Level then
                    divide_by_set_descend_divide_by(DBSubLevel, DBSubNodes,
                        Level, InitOffset, LimitOffset, List, InList, OutList)
                else if DBSubLevel = Level then
                    (
                        List = leaf_list(_),
                        % Since DBHeadComponents is an interior list,
                        % and List is at the same level, it should be
                        % an interior list too.
                        unexpected($pred, "List is leaf_list")
                    ;
                        List = interior_list(_, Nodes),
                        interiorlist_divide_by_set(Level,
                            DBSubNodes, Nodes, InNodes, OutNodes),
                        (
                            InNodes = [],
                            InList = leaf_list([])
                        ;
                            InNodes = [_ | _],
                            InList = interior_list(Level, InNodes)
                        ),
                        (
                            OutNodes = [],
                            OutList = leaf_list([])
                        ;
                            OutNodes = [_ | _],
                            OutList = interior_list(Level, OutNodes)
                        )
                    )
                else
                    unexpected($pred, "DBSubLevel < Level")
                )
            )
        )
    ).

:- pred interiorlist_divide_by_set(int::in,
    list(interior_node)::in, list(interior_node)::in,
    list(interior_node)::out, list(interior_node)::out) is det.

interiorlist_divide_by_set(_Level, _DBSubNodes, [], [], []).
interiorlist_divide_by_set(_Level, [], Nodes @ [_ | _], [], Nodes).
interiorlist_divide_by_set(Level, DBNodes @ [DBNodesHead | DBNodesTail],
        Nodes @ [NodesHead | NodesTail], InNodes, OutNodes) :-
    DBNodesHead = interior_node(DBInitOffset, DBLimitOffset, DBComponents),
    NodesHead = interior_node(InitOffset, LimitOffset, Components),
    % Since DBNodesHead and NodesHead are at the same level,
    % they cover the same region only if their initial and limit offsets
    % both match.
    ( if DBInitOffset = InitOffset then
        expect(unify(DBLimitOffset, LimitOffset), $pred,
            "DBLimitOffset != LimitOffset"),
        interiorlist_divide_by_set(Level, DBNodesTail, NodesTail,
            InNodesTail, OutNodesTail),
        (
            DBComponents = leaf_list(DBLeafNodes),
            Components = leaf_list(LeafNodes),
            leaflist_divide_by_set(DBLeafNodes, LeafNodes,
                InLeafNodes, OutLeafNodes),
            (
                InLeafNodes = [],
                InNodes = InNodesTail
            ;
                InLeafNodes = [_ | _],
                InNodesHead = interior_node(InitOffset, LimitOffset,
                    leaf_list(InLeafNodes)),
                InNodes = [InNodesHead | InNodesTail]
            ),
            (
                OutLeafNodes = [],
                OutNodes = OutNodesTail
            ;
                OutLeafNodes = [_ | _],
                OutNodesHead = interior_node(InitOffset, LimitOffset,
                    leaf_list(OutLeafNodes)),
                OutNodes = [OutNodesHead | OutNodesTail]
            )
        ;
            DBComponents = interior_list(_, _),
            Components = leaf_list(_),
            unexpected($pred, "DB interior vs leaf")
        ;
            DBComponents = leaf_list(_),
            Components = interior_list(_, _),
            unexpected($pred, "DB leaf vs interior")
        ;
            DBComponents = interior_list(DBSubLevel, DBSubNodes),
            Components = interior_list(SubLevel, SubNodes),
            expect(unify(DBSubLevel, SubLevel), $pred,
                "DBSubLevel != SubLevel"),
            expect(unify(SubLevel, Level - 1), $pred,
                "DBSubLevel != SubLevel"),
            interiorlist_divide_by_set(SubLevel, DBSubNodes, SubNodes,
                SubInNodes, SubOutNodes),
            (
                SubInNodes = [],
                InNodes = InNodesTail
            ;
                SubInNodes = [_ | _],
                InNodesHead = interior_node(InitOffset, LimitOffset,
                    interior_list(SubLevel, SubInNodes)),
                InNodes = [InNodesHead | InNodesTail]
            ),
            (
                SubOutNodes = [],
                OutNodes = OutNodesTail
            ;
                SubOutNodes = [_ | _],
                OutNodesHead = interior_node(InitOffset, LimitOffset,
                    interior_list(SubLevel, SubOutNodes)),
                OutNodes = [OutNodesHead | OutNodesTail]
            )
        )
    else if DBInitOffset < InitOffset then
        % DBNodesHead covers a region that is entirely before the region
        % covered by Nodes.
        interiorlist_divide_by_set(Level, DBNodesTail, Nodes,
            InNodes, OutNodes)
    else
        % NodesHead covers a region that is entirely before the region
        % covered by DBNodesHead. Therefore all the items in NodesHead
        % are outside DivideBySet.
        interiorlist_divide_by_set(Level, DBNodes, NodesTail,
            InNodes, OutNodesTail),
        OutNodes = [NodesHead | OutNodesTail]
    ).

:- pred leaflist_divide_by_set(list(leaf_node)::in, list(leaf_node)::in,
    list(leaf_node)::out, list(leaf_node)::out) is det.

leaflist_divide_by_set(_, [], [], []).
leaflist_divide_by_set([], List @ [_ | _], [], List).
leaflist_divide_by_set(DivideByList @ [DivideByHead | DivideByTail],
        List @ [ListHead | ListTail], InList, OutList) :-
    DivideByOffset = DivideByHead ^ leaf_offset,
    ListOffset = ListHead ^ leaf_offset,
    ( if DivideByOffset = ListOffset then
        ListHeadBits = ListHead ^ leaf_bits,
        DivideByHeadBits = DivideByHead ^ leaf_bits,
        InBits = ListHeadBits /\ DivideByHeadBits,
        OutBits = ListHeadBits /\ \ DivideByHeadBits,
        ( if InBits = 0u then
            ( if OutBits = 0u then
                leaflist_divide_by_set(DivideByTail, ListTail, InList, OutList)
            else
                NewOutNode = make_leaf_node(ListOffset, OutBits),
                leaflist_divide_by_set(DivideByTail, ListTail,
                    InList, OutTail),
                OutList = [NewOutNode | OutTail]
            )
        else
            NewInNode = make_leaf_node(ListOffset, InBits),
            ( if OutBits = 0u then
                leaflist_divide_by_set(DivideByTail, ListTail,
                    InTail, OutList),
                InList = [NewInNode | InTail]
            else
                NewOutNode = make_leaf_node(ListOffset, OutBits),
                leaflist_divide_by_set(DivideByTail, ListTail,
                    InTail, OutTail),
                InList = [NewInNode | InTail],
                OutList = [NewOutNode | OutTail]
            )
        )
    else if DivideByOffset < ListOffset then
        leaflist_divide_by_set(DivideByTail, List, InList, OutList)
    else
        leaflist_divide_by_set(DivideByList, ListTail, InList, OutTail),
        OutList = [ListHead | OutTail]
    ).

%     % Our basic approach of raising both operands to the same level
%     % simplifies the code (by allowing the reuse of the basic pattern
%     % and the helper predicates of the union predicate), but searching
%     % the larger set for the range of the smaller set and starting
%     % the operation there would be more efficient in both time and space.
%     (
%         DivideByList = leaf_list(DivideByLeafNodes),
%         List = leaf_list(LeafNodes),
%         (
%             LeafNodes = [],
%             InList = List,
%             OutList = List
%         ;
%             LeafNodes = [_ | _],
%             DivideByLeafNodes = [],
%             InList = [],
%             OutList = List
%         ;
%             LeafNodes = [FirstNode | _LaterNodes],
%             DivideByLeafNodes = [DivideByFirstNode | _DivideByLaterNodes],
%             range_of_parent_node(FirstNode ^ leaf_offset, 0,
%                 ParentInitOffset, ParentLimitOffset),
%             range_of_parent_node(DivideByFirstNode ^ leaf_offset, 0,
%                 DivideByParentInitOffset, DivideByParentLimitOffset),
%             ( if DivideByParentInitOffset = ParentInitOffset then
%                 expect(unify(DivideByParentLimitOffset, ParentLimitOffset),
%                     $pred, "limit mismatch"),
%                 leaflist_divide_by_set(DivideByLeafNodes, LeafNodes,
%                     InLeafNodes, OutLeafNodes),
%                 InList = leaf_list(InLeafNodes),
%                 OutList = leaf_list(OutLeafNodes)
%             else
%                 % The ranges of the two sets do not overlap.
%                 InList = [],
%                 OutList = List
%             )
%         )
%     ;
%         ListA = leaf_list(LeafNodesA),
%         ListB = interior_list(LevelB, InteriorNodesB),
%         (
%             LeafNodesA = [],
%             List = ListB
%         ;
%             LeafNodesA = [FirstNodeA | LaterNodesA],
%             raise_leaves_to_interior(FirstNodeA, LaterNodesA, InteriorNodeA),
%             head_and_tail(InteriorNodesB, InteriorHeadB, InteriorTailB),
%             interiornode_difference(1, InteriorNodeA, [],
%                 LevelB, InteriorHeadB, InteriorTailB, Level, InteriorNodes),
%             List = interior_list(Level, InteriorNodes)
%         )
%     ;
%         ListA = interior_list(LevelA, InteriorNodesA),
%         ListB = leaf_list(LeafNodesB),
%         (
%             LeafNodesB = [],
%             List = ListA
%         ;
%             LeafNodesB = [FirstNodeB | LaterNodesB],
%             raise_leaves_to_interior(FirstNodeB, LaterNodesB, InteriorNodeB),
%             head_and_tail(InteriorNodesA, InteriorHeadA, InteriorTailA),
%             interiornode_difference(LevelA, InteriorHeadA, InteriorTailA,
%                 1, InteriorNodeB, [], Level, InteriorNodes),
%             List = interior_list(Level, InteriorNodes)
%         )
%     ;
%         ListA = interior_list(LevelA, InteriorNodesA),
%         ListB = interior_list(LevelB, InteriorNodesB),
%         head_and_tail(InteriorNodesA, InteriorHeadA, InteriorTailA),
%         head_and_tail(InteriorNodesB, InteriorHeadB, InteriorTailB),
%         interiornode_difference(LevelA, InteriorHeadA, InteriorTailA,
%             LevelB, InteriorHeadB, InteriorTailB, Level, InteriorNodes),
%         List = interior_list(Level, InteriorNodes)
%     ),
%     prune_top_levels(List, PrunedList),
%     Set = wrap_tree_bitset(PrunedList).
%
% :- pred interiornode_difference(
%     int::in, interior_node::in, list(interior_node)::in,
%     int::in, interior_node::in, list(interior_node)::in,
%     int::out, list(interior_node)::out) is det.
%
% interiornode_difference(LevelA, HeadA, TailA, LevelB, HeadB, TailB,
%         Level, List) :-
%     ( if LevelA < LevelB then
%         range_of_parent_node(HeadA ^ init_offset, LevelA + 1,
%             ParentInitOffsetA, ParentLimitOffsetA),
%         ( if
%             find_containing_node(ParentInitOffsetA, ParentLimitOffsetA,
%                 [HeadB | TailB], ChosenB)
%         then
%             ComponentsB = ChosenB ^ components,
%             (
%                 ComponentsB = leaf_list(_),
%                 expect(unify(LevelA, 1),
%                     $pred, "bad leaf level"),
%                 interiorlist_difference([HeadA | TailA], [ChosenB], List),
%                 Level = LevelA
%             ;
%                 ComponentsB = interior_list(SubLevelB, SubNodesB),
%                 expect(unify(LevelB, SubLevelB + 1),
%                     $pred, "bad levels"),
%                 head_and_tail(SubNodesB, SubHeadB, SubTailB),
%                 interiornode_difference(LevelA, HeadA, TailA,
%                     SubLevelB, SubHeadB, SubTailB, Level, List)
%             )
%         else
%             Level = 1,
%             List = []
%         )
%     else
%         raise_interiors_to_level(LevelA, LevelB, HeadB, TailB,
%             RaisedHeadB, RaisedTailB),
%         range_of_parent_node(HeadA ^ init_offset, LevelA,
%             ParentInitOffsetA, ParentLimitOffsetA),
%         range_of_parent_node(RaisedHeadB ^ init_offset, LevelA,
%             ParentInitOffsetB, ParentLimitOffsetB),
%         ( if ParentInitOffsetA = ParentInitOffsetB then
%             expect(unify(ParentLimitOffsetA, ParentLimitOffsetB),
%                 $pred, "limit mismatch"),
%             interiorlist_difference([HeadA | TailA],
%                 [RaisedHeadB | RaisedTailB], List),
%             Level = LevelA
%         else
%             Level = 1,
%             List = []
%         )
%     ).
%
% :- pred find_containing_node(int::in, int::in, list(interior_node)::in,
%     interior_node::out) is semidet.
%
% find_containing_node(InitOffsetA, LimitOffsetA, [HeadB | TailB], ChosenB) :-
%     ( if
%         HeadB ^ init_offset =< InitOffsetA,
%         LimitOffsetA =< HeadB ^ limit_offset
%     then
%         ChosenB = HeadB
%     else
%         find_containing_node(InitOffsetA, LimitOffsetA, TailB, ChosenB)
%     ).
%
% :- pred leaflist_difference(list(leaf_node)::in, list(leaf_node)::in,
%     list(leaf_node)::out) is det.
%
% leaflist_difference([], [], []).
% leaflist_difference([], [_ | _], []).
% leaflist_difference(ListA @ [_ | _], [], ListA).
% leaflist_difference(ListA @ [HeadA | TailA], ListB @ [HeadB | TailB],
%         List) :-
%     OffsetA = HeadA ^ leaf_offset,
%     OffsetB = HeadB ^ leaf_offset,
%     ( if OffsetA = OffsetB then
%         Bits = (HeadA ^ leaf_bits) /\ \ (HeadB ^ leaf_bits),
%         ( if Bits = 0 then
%             leaflist_difference(TailA, TailB, List)
%         else
%             Head = make_leaf_node(OffsetA, Bits),
%             leaflist_difference(TailA, TailB, Tail),
%             List = [Head | Tail]
%         )
%     else if OffsetA < OffsetB then
%         leaflist_difference(TailA, ListB, Tail),
%         List = [HeadA | Tail]
%     else
%         leaflist_difference(ListA, TailB, List)
%     ).
%
% :- pred interiorlist_difference(
%     list(interior_node)::in, list(interior_node)::in,
%     list(interior_node)::out) is det.
%
% interiorlist_difference([], [], []).
% interiorlist_difference([], [_ | _], []).
% interiorlist_difference(ListA @ [_ | _], [], ListA).
% interiorlist_difference(ListA @ [HeadA | TailA], ListB @ [HeadB | TailB],
%         List) :-
%     OffsetA = HeadA ^ init_offset,
%     OffsetB = HeadB ^ init_offset,
%     ( if OffsetA = OffsetB tnen
%         ComponentsA = HeadA ^ components,
%         ComponentsB = HeadB ^ components,
%         (
%             ComponentsA = leaf_list(LeafNodesA),
%             ComponentsB = leaf_list(LeafNodesB),
%             leaflist_difference(LeafNodesA, LeafNodesB, LeafNodes),
%             (
%                 LeafNodes = [],
%                 interiorlist_difference(TailA, TailB, List)
%             ;
%                 LeafNodes = [_ | _],
%                 Components = leaf_list(LeafNodes),
%                 interiorlist_difference(TailA, TailB, Tail),
%                 Head = interior_node(HeadA ^ init_offset,
%                     HeadA ^ limit_offset, Components),
%                 List = [Head | Tail]
%             )
%         ;
%             ComponentsA = interior_list(_LevelA, _InteriorNodesA),
%             ComponentsB = leaf_list(_LeafNodesB),
%             error("tree_bitset.m: " ++
%                 "inconsistent components in interiorlist_difference")
%         ;
%             ComponentsB = interior_list(_LevelB, _InteriorNodesB),
%             ComponentsA = leaf_list(_LeafNodesA),
%             error("tree_bitset.m: " ++
%                 "inconsistent components in interiorlist_difference")
%         ;
%             ComponentsA = interior_list(LevelA, InteriorNodesA),
%             ComponentsB = interior_list(LevelB, InteriorNodesB),
%             expect(unify(LevelA, LevelB),
%                 "tree_bitset.m: " ++
%                 "inconsistent levels in interiorlist_difference"),
%             interiorlist_difference(InteriorNodesA, InteriorNodesB,
%                 InteriorNodes),
%             (
%                 InteriorNodes = [],
%                 interiorlist_difference(TailA, TailB, List)
%             ;
%                 InteriorNodes = [_ | _],
%                 Components = interior_list(LevelA, InteriorNodes),
%                 interiorlist_difference(TailA, TailB, Tail),
%                 Head = interior_node(HeadA ^ init_offset,
%                     HeadA ^ limit_offset, Components),
%                 List = [Head | Tail]
%             )
%         )
%     else if OffsetA < OffsetB then
%         interiorlist_difference(TailA, ListB, Tail),
%         List = [HeadA | Tail]
%     else
%         interiorlist_difference(ListA, TailB, List)
%     ).

%---------------------------------------------------------------------------%

list_to_set(List) = sorted_list_to_set(list.sort(List)).

list_to_set(List, Set) :-
    Set = list_to_set(List).

%---------------------%

sorted_list_to_set(Elems) = Set :-
    items_to_index(Elems, Indexes),
    % XXX We SHOULD sort Indexes. The fact that Elems is sorted
    % does not *necessarily* imply that Indexes is sorted.
    LeafNodes = sorted_list_to_leaf_nodes(Indexes),
    (
        LeafNodes = [],
        List = leaf_list([]),
        Set = wrap_tree_bitset(List)
    ;
        LeafNodes = [LeafHead | LeafTail],
        group_leaf_nodes(LeafHead, LeafTail, InteriorNodes0),
        (
            InteriorNodes0 = [],
            unexpected($pred, "empty InteriorNodes0")
        ;
            InteriorNodes0 = [InteriorNode],
            List = InteriorNode ^ components
        ;
            InteriorNodes0 = [_, _ | _],
            recursively_group_interior_nodes(1, InteriorNodes0, List)
        ),
        Set = wrap_tree_bitset(List)
    ).

sorted_list_to_set(List, Set) :-
    Set = sorted_list_to_set(List).

:- pred items_to_index(list(T)::in, list(int)::out) is det <= enum(T).
:- pragma type_spec(pred(items_to_index/2), T = var(_)).
:- pragma type_spec(pred(items_to_index/2), T = int).

items_to_index([], []).
items_to_index([ElemHead | ElemTail], [IndexHead | IndexTail]) :-
    IndexHead = enum_to_index(ElemHead),
    items_to_index(ElemTail, IndexTail).

:- func sorted_list_to_leaf_nodes(list(int)) = list(leaf_node).

sorted_list_to_leaf_nodes([]) = [].
sorted_list_to_leaf_nodes([Head | Tail]) = LeafNodes :-
    bits_for_index(Head, Offset, HeadBits),
    gather_bits_for_leaf(Tail, Offset, HeadBits, Bits, Remaining),
    sorted_list_to_leaf_nodes(Remaining) = LeafNodesTail,
    LeafNodes = [make_leaf_node(Offset, Bits) | LeafNodesTail].

:- pred gather_bits_for_leaf(list(int)::in, int::in, uint::in, uint::out,
    list(int)::out) is det.

gather_bits_for_leaf([], _Offset, !Bits, []).
gather_bits_for_leaf(List @ [Head | Tail], Offset, !Bits, Remaining) :-
    bits_for_index(Head, HeadOffset, HeadBits),
    ( if HeadOffset = Offset then
        !:Bits = !.Bits \/ HeadBits,
        gather_bits_for_leaf(Tail, Offset, !Bits, Remaining)
    else
        Remaining = List
    ).

:- pred group_leaf_nodes(leaf_node::in, list(leaf_node)::in,
    list(interior_node)::out) is det.

group_leaf_nodes(Head, Tail, ParentList) :-
    range_of_parent_node(Head ^ leaf_offset, 0,
        ParentInitOffset, ParentLimitOffset),
    group_leaf_nodes_in_range(ParentInitOffset, ParentLimitOffset, [Head],
        Tail, ParentHead, Remaining),
    (
        Remaining = [],
        ParentTail = []
    ;
        Remaining = [RemainingHead | RemainingTail],
        group_leaf_nodes(RemainingHead, RemainingTail, ParentTail)
    ),
    ParentList = [ParentHead | ParentTail].

:- pred group_leaf_nodes_in_range(int::in, int::in,
    list(leaf_node)::in, list(leaf_node)::in,
    interior_node::out, list(leaf_node)::out) is det.

group_leaf_nodes_in_range(ParentInitOffset, ParentLimitOffset, !.RevAcc,
        [], ParentNode, []) :-
    ParentNode = interior_node(ParentInitOffset, ParentLimitOffset,
        leaf_list(list.reverse(!.RevAcc))).
group_leaf_nodes_in_range(ParentInitOffset, ParentLimitOffset, !.RevAcc,
        [Head | Tail], ParentNode, Remaining) :-
    range_of_parent_node(Head ^ leaf_offset, 0,
        HeadParentInitOffset, HeadParentLimitOffset),
    ( if ParentInitOffset = HeadParentInitOffset then
        trace [compile_time(flag("tree-bitset-checks"))] (
            expect(unify(ParentLimitOffset, HeadParentLimitOffset),
                $pred, "limit mismatch")
        ),
        !:RevAcc = [Head | !.RevAcc],
        group_leaf_nodes_in_range(ParentInitOffset, ParentLimitOffset,
            !.RevAcc, Tail, ParentNode, Remaining)
    else
        ParentNode = interior_node(ParentInitOffset, ParentLimitOffset,
            leaf_list(list.reverse(!.RevAcc))),
        Remaining = [Head | Tail]
    ).

:- pred recursively_group_interior_nodes(int::in, list(interior_node)::in,
    node_list::out) is det.

recursively_group_interior_nodes(CurLevel, CurNodes, List) :-
    (
        CurNodes = [],
        unexpected($pred, "empty CurNodes")
    ;
        CurNodes = [CurNodesHead | CurNodesTail],
        (
            CurNodesTail = [],
            List = CurNodesHead ^ components
        ;
            CurNodesTail = [_ | _],
            group_interior_nodes(CurLevel, CurNodesHead, CurNodesTail,
                ParentNodes),
            recursively_group_interior_nodes(CurLevel + 1, ParentNodes, List)
        )
    ).

:- pred group_interior_nodes(int::in, interior_node::in,
    list(interior_node)::in, list(interior_node)::out) is det.

group_interior_nodes(Level, Head, Tail, ParentList) :-
    range_of_parent_node(Head ^ init_offset, Level,
        ParentInitOffset, ParentLimitOffset),
    group_interior_nodes_in_range(Level, ParentInitOffset, ParentLimitOffset,
        [Head], Tail, ParentHead, Remaining),
    (
        Remaining = [],
        ParentTail = []
    ;
        Remaining = [RemainingHead | RemainingTail],
        group_interior_nodes(Level, RemainingHead, RemainingTail, ParentTail)
    ),
    ParentList = [ParentHead | ParentTail].

:- pred group_interior_nodes_in_range(int::in, int::in, int::in,
    list(interior_node)::in, list(interior_node)::in,
    interior_node::out, list(interior_node)::out) is det.

group_interior_nodes_in_range(Level, ParentInitOffset, ParentLimitOffset,
        !.RevAcc, [], ParentNode, []) :-
    ParentNode = interior_node(ParentInitOffset, ParentLimitOffset,
        interior_list(Level, list.reverse(!.RevAcc))).
group_interior_nodes_in_range(Level, ParentInitOffset, ParentLimitOffset,
        !.RevAcc, [Head | Tail], ParentNode, Remaining) :-
    range_of_parent_node(Head ^ init_offset, Level,
        HeadParentInitOffset, HeadParentLimitOffset),
    ( if ParentInitOffset = HeadParentInitOffset then
        trace [compile_time(flag("tree-bitset-checks"))] (
            expect(unify(ParentLimitOffset, HeadParentLimitOffset),
                $pred, "limit mismatch")
        ),
        !:RevAcc = [Head | !.RevAcc],
        group_interior_nodes_in_range(Level,
            ParentInitOffset, ParentLimitOffset,
            !.RevAcc, Tail, ParentNode, Remaining)
    else
        ParentNode = interior_node(ParentInitOffset, ParentLimitOffset,
            interior_list(Level, list.reverse(!.RevAcc))),
        Remaining = [Head | Tail]
    ).

%---------------------%

to_sorted_list(Set) = foldr(list.cons, Set, []).

to_sorted_list(Set, List) :-
    List = to_sorted_list(Set).

%---------------------------------------------------------------------------%

from_set(Set) = sorted_list_to_set(set.to_sorted_list(Set)).

to_set(Set) = set.sorted_list_to_set(to_sorted_list(Set)).

%---------------------------------------------------------------------------%

count(Set) = foldl((func(_, Acc) = Acc + 1), Set, 0).

%---------------------------------------------------------------------------%

all_true(P, Set) :-
    Set = tree_bitset(List),
    (
        List = leaf_list(LeafNodes),
        leaf_all_true(P, LeafNodes)
    ;
        List = interior_list(_, InteriorNodes),
        interior_all_true(P, InteriorNodes)
    ).

:- pred interior_all_true(pred(T)::in(pred(in) is semidet),
    list(interior_node)::in) is semidet <= enum(T).
:- pragma type_spec(pred(interior_all_true/2), T = int).
:- pragma type_spec(pred(interior_all_true/2), T = var(_)).

interior_all_true(_P, []).
interior_all_true(P, [H | T]) :-
    Components = H ^ components,
    (
        Components = leaf_list(LeafNodes),
        leaf_all_true(P, LeafNodes)
    ;
        Components = interior_list(_, InteriorNodes),
        interior_all_true(P, InteriorNodes)
    ),
    interior_all_true(P, T).

:- pred leaf_all_true(pred(T)::in(pred(in) is semidet), list(leaf_node)::in)
    is semidet <= enum(T).
:- pragma type_spec(pred(leaf_all_true/2), T = int).
:- pragma type_spec(pred(leaf_all_true/2), T = var(_)).

leaf_all_true(_P, []).
leaf_all_true(P, [H | T]) :-
    all_true_bits(P, H ^ leaf_offset, H ^ leaf_bits, bits_per_int),
    leaf_all_true(P, T).

    % Do a binary search for the 1 bits in an int.
    %
:- pred all_true_bits(pred(T)::in(pred(in) is semidet),
    int::in, uint::in, int::in) is semidet <= enum(T).
:- pragma type_spec(pred(all_true_bits/4), T = int).
:- pragma type_spec(pred(all_true_bits/4), T = var(_)).

all_true_bits(P, Offset, Bits, Size) :-
    ( if Bits = 0u then
        true
    else if Size = 1 then
        Elem = index_to_enum(Offset),
        P(Elem)
    else
        HalfSize = unchecked_right_shift(Size, 1),
        Mask = mask(HalfSize),

        % Extract the low-order half of the bits.
        LowBits = Mask /\ Bits,

        % Extract the high-order half of the bits.
        HighBits = Mask /\ unchecked_right_shift(Bits, HalfSize),

        all_true_bits(P, Offset, LowBits, HalfSize),
        all_true_bits(P, Offset + HalfSize, HighBits, HalfSize)
    ).

%---------------------%

% XXX We should make these more efficient. At least, we could filter the bits
% in the leaf nodes, yielding a new list of leaf nodes, and we could put the
% interior nodes on top, just as we do in sorted_list_to_set.

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

foldl(F, Set, Acc0) = Acc :-
    P =
        ( pred(E::in, PAcc0::in, PAcc::out) is det :-
            PAcc = F(E, PAcc0)
        ),
    foldl(P, Set, Acc0, Acc).

foldl(P, Set, !Acc) :-
    Set = tree_bitset(List),
    (
        List = leaf_list(LeafNodes),
        leaf_foldl_pred(P, LeafNodes, !Acc)
    ;
        List = interior_list(_, InteriorNodes),
        do_foldl_pred(P, InteriorNodes, !Acc)
    ).

:- pred do_foldl_pred(pred(T, U, U), list(interior_node), U, U) <= enum(T).
:- mode do_foldl_pred(pred(in, in, out) is det, in, in, out) is det.
:- mode do_foldl_pred(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode do_foldl_pred(pred(in, di, uo) is det, in, di, uo) is det.
:- mode do_foldl_pred(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode do_foldl_pred(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode do_foldl_pred(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode do_foldl_pred(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode do_foldl_pred(pred(in, mdi, muo) is nondet, in, mdi, muo) is nondet.
:- mode do_foldl_pred(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.
:- mode do_foldl_pred(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- pragma type_spec(pred(do_foldl_pred/4), T = int).
:- pragma type_spec(pred(do_foldl_pred/4), T = var(_)).

do_foldl_pred(_, [], !Acc).
do_foldl_pred(P, [H | T], !Acc) :-
    Components = H ^ components,
    (
        Components = leaf_list(LeafNodes),
        leaf_foldl_pred(P, LeafNodes, !Acc)
    ;
        Components = interior_list(_, InteriorNodes),
        do_foldl_pred(P, InteriorNodes, !Acc)
    ),
    do_foldl_pred(P, T, !Acc).

:- pred leaf_foldl_pred(pred(T, U, U), list(leaf_node), U, U) <= enum(T).
:- mode leaf_foldl_pred(pred(in, in, out) is det, in, in, out) is det.
:- mode leaf_foldl_pred(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode leaf_foldl_pred(pred(in, di, uo) is det, in, di, uo) is det.
:- mode leaf_foldl_pred(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode leaf_foldl_pred(pred(in, mdi, muo) is semidet, in, mdi, muo)
    is semidet.
:- mode leaf_foldl_pred(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode leaf_foldl_pred(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode leaf_foldl_pred(pred(in, mdi, muo) is nondet, in, mdi, muo) is nondet.
:- mode leaf_foldl_pred(pred(in, in, out) is cc_multi, in, in, out)
    is cc_multi.
:- mode leaf_foldl_pred(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- pragma type_spec(pred(leaf_foldl_pred/4), T = int).
:- pragma type_spec(pred(leaf_foldl_pred/4), T = var(_)).

leaf_foldl_pred(_, [], !Acc).
leaf_foldl_pred(P, [H | T], !Acc) :-
    fold_bits(low_to_high, P, H ^ leaf_offset, H ^ leaf_bits, bits_per_int,
        !Acc),
    leaf_foldl_pred(P, T, !Acc).

%---------------------%

foldl2(P, Set, !AccA, !AccB) :-
    Set = tree_bitset(List),
    (
        List = leaf_list(LeafNodes),
        leaf_foldl2_pred(P, LeafNodes, !AccA, !AccB)
    ;
        List = interior_list(_, InteriorNodes),
        do_foldl2_pred(P, InteriorNodes, !AccA, !AccB)
    ).

:- pred do_foldl2_pred(pred(T, U, U, V, V), list(interior_node), U, U, V, V)
    <= enum(T).
:- mode do_foldl2_pred(pred(in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode do_foldl2_pred(pred(in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode do_foldl2_pred(pred(in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode do_foldl2_pred(pred(in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode do_foldl2_pred(pred(in, in, out, in, out) is nondet,
    in, in, out, in, out) is nondet.
:- mode do_foldl2_pred(pred(in, di, uo, di, uo) is cc_multi,
    in, di, uo, di, uo) is cc_multi.
:- mode do_foldl2_pred(pred(in, in, out, di, uo) is cc_multi,
    in, in, out, di, uo) is cc_multi.
:- mode do_foldl2_pred(pred(in, in, out, in, out) is cc_multi,
    in, in, out, in, out) is cc_multi.
:- pragma type_spec(pred(do_foldl2_pred/6), T = int).
:- pragma type_spec(pred(do_foldl2_pred/6), T = var(_)).

do_foldl2_pred(_, [], !AccA, !AccB).
do_foldl2_pred(P, [H | T], !AccA, !AccB) :-
    Components = H ^ components,
    (
        Components = leaf_list(LeafNodes),
        leaf_foldl2_pred(P, LeafNodes, !AccA, !AccB)
    ;
        Components = interior_list(_, InteriorNodes),
        do_foldl2_pred(P, InteriorNodes, !AccA, !AccB)
    ),
    do_foldl2_pred(P, T, !AccA, !AccB).

:- pred leaf_foldl2_pred(pred(T, U, U, V, V), list(leaf_node), U, U, V, V)
    <= enum(T).
:- mode leaf_foldl2_pred(pred(in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode leaf_foldl2_pred(pred(in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode leaf_foldl2_pred(pred(in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode leaf_foldl2_pred(pred(in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode leaf_foldl2_pred(pred(in, in, out, in, out) is nondet,
    in, in, out, in, out) is nondet.
:- mode leaf_foldl2_pred(pred(in, di, uo, di, uo) is cc_multi,
    in, di, uo, di, uo) is cc_multi.
:- mode leaf_foldl2_pred(pred(in, in, out, di, uo) is cc_multi,
    in, in, out, di, uo) is cc_multi.
:- mode leaf_foldl2_pred(pred(in, in, out, in, out) is cc_multi,
    in, in, out, in, out) is cc_multi.
:- pragma type_spec(pred(leaf_foldl2_pred/6), T = int).
:- pragma type_spec(pred(leaf_foldl2_pred/6), T = var(_)).

leaf_foldl2_pred(_, [], !AccA, !AccB).
leaf_foldl2_pred(P, [H | T], !AccA, !AccB) :-
    fold2_bits(low_to_high, P, H ^ leaf_offset, H ^ leaf_bits, bits_per_int,
        !AccA, !AccB),
    leaf_foldl2_pred(P, T, !AccA, !AccB).

%---------------------%

foldr(F, Set, Acc0) = Acc :-
    P =
        ( pred(E::in, PAcc0::in, PAcc::out) is det :-
            PAcc = F(E, PAcc0)
        ),
    foldr(P, Set, Acc0, Acc).

foldr(P, Set, !Acc) :-
    Set = tree_bitset(List),
    (
        List = leaf_list(LeafNodes),
        leaf_foldr_pred(P, LeafNodes, !Acc)
    ;
        List = interior_list(_, InteriorNodes),
        do_foldr_pred(P, InteriorNodes, !Acc)
    ).

:- pred do_foldr_pred(pred(T, U, U), list(interior_node), U, U) <= enum(T).
:- mode do_foldr_pred(pred(in, di, uo) is det, in, di, uo) is det.
:- mode do_foldr_pred(pred(in, in, out) is det, in, in, out) is det.
:- mode do_foldr_pred(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode do_foldr_pred(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode do_foldr_pred(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode do_foldr_pred(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.
:- pragma type_spec(pred(do_foldr_pred/4), T = int).
:- pragma type_spec(pred(do_foldr_pred/4), T = var(_)).

do_foldr_pred(_, [], !Acc).
do_foldr_pred(P, [H | T], !Acc) :-
    % We don't just use list.foldr here because the overhead of allocating
    % the closure for fold_bits is significant for the compiler's runtime,
    % so it is best to avoid that even if `--optimize-higher-order' is not set.
    do_foldr_pred(P, T, !Acc),
    Components = H ^ components,
    (
        Components = leaf_list(LeafNodes),
        leaf_foldr_pred(P, LeafNodes, !Acc)
    ;
        Components = interior_list(_, InteriorNodes),
        do_foldr_pred(P, InteriorNodes, !Acc)
    ).

:- pred leaf_foldr_pred(pred(T, U, U), list(leaf_node), U, U) <= enum(T).
:- mode leaf_foldr_pred(pred(in, di, uo) is det, in, di, uo) is det.
:- mode leaf_foldr_pred(pred(in, in, out) is det, in, in, out) is det.
:- mode leaf_foldr_pred(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode leaf_foldr_pred(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode leaf_foldr_pred(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode leaf_foldr_pred(pred(in, in, out) is cc_multi, in, in, out)
    is cc_multi.
:- pragma type_spec(pred(leaf_foldr_pred/4), T = int).
:- pragma type_spec(pred(leaf_foldr_pred/4), T = var(_)).

leaf_foldr_pred(_, [], !Acc).
leaf_foldr_pred(P, [H | T], !Acc) :-
    % We don't just use list.foldr here because the overhead of allocating
    % the closure for fold_bits is significant for the compiler's runtime,
    % so it is best to avoid that even if `--optimize-higher-order' is not set.
    leaf_foldr_pred(P, T, !Acc),
    fold_bits(high_to_low, P, H ^ leaf_offset, H ^ leaf_bits, bits_per_int,
        !Acc).

%---------------------%

foldr2(P, Set, !AccA, !AccB) :-
    Set = tree_bitset(List),
    (
        List = leaf_list(LeafNodes),
        leaf_foldr2_pred(P, LeafNodes, !AccA, !AccB)
    ;
        List = interior_list(_, InteriorNodes),
        do_foldr2_pred(P, InteriorNodes, !AccA, !AccB)
    ).

:- pred do_foldr2_pred(pred(T, U, U, V, V), list(interior_node), U, U, V, V)
    <= enum(T).
:- mode do_foldr2_pred(pred(in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode do_foldr2_pred(pred(in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode do_foldr2_pred(pred(in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode do_foldr2_pred(pred(in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode do_foldr2_pred(pred(in, in, out, in, out) is nondet,
    in, in, out, in, out) is nondet.
:- mode do_foldr2_pred(pred(in, di, uo, di, uo) is cc_multi,
    in, di, uo, di, uo) is cc_multi.
:- mode do_foldr2_pred(pred(in, in, out, di, uo) is cc_multi,
    in, in, out, di, uo) is cc_multi.
:- mode do_foldr2_pred(pred(in, in, out, in, out) is cc_multi,
    in, in, out, in, out) is cc_multi.
:- pragma type_spec(pred(do_foldr2_pred/6), T = int).
:- pragma type_spec(pred(do_foldr2_pred/6), T = var(_)).

do_foldr2_pred(_, [], !AccA, !AccB).
do_foldr2_pred(P, [H | T], !AccA, !AccB) :-
    % We don't just use list.foldr here because the overhead of allocating
    % the closure for fold_bits is significant for the compiler's runtime,
    % so it is best to avoid that even if `--optimize-higher-order' is not set.
    do_foldr2_pred(P, T, !AccA, !AccB),
    Components = H ^ components,
    (
        Components = leaf_list(LeafNodes),
        leaf_foldr2_pred(P, LeafNodes, !AccA, !AccB)
    ;
        Components = interior_list(_, InteriorNodes),
        do_foldr2_pred(P, InteriorNodes, !AccA, !AccB)
    ).

:- pred leaf_foldr2_pred(pred(T, U, U, V, V), list(leaf_node), U, U, V, V)
    <= enum(T).
:- mode leaf_foldr2_pred(pred(in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode leaf_foldr2_pred(pred(in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode leaf_foldr2_pred(pred(in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode leaf_foldr2_pred(pred(in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode leaf_foldr2_pred(pred(in, in, out, in, out) is nondet,
    in, in, out, in, out) is nondet.
:- mode leaf_foldr2_pred(pred(in, di, uo, di, uo) is cc_multi,
    in, di, uo, di, uo) is cc_multi.
:- mode leaf_foldr2_pred(pred(in, in, out, di, uo) is cc_multi,
    in, in, out, di, uo) is cc_multi.
:- mode leaf_foldr2_pred(pred(in, in, out, in, out) is cc_multi,
    in, in, out, in, out) is cc_multi.
:- pragma type_spec(pred(leaf_foldr2_pred/6), T = int).
:- pragma type_spec(pred(leaf_foldr2_pred/6), T = var(_)).

leaf_foldr2_pred(_, [], !AccA, !AccB).
leaf_foldr2_pred(P, [H | T], !AccA, !AccB) :-
    % We don't just use list.foldr here because the overhead of allocating
    % the closure for fold_bits is significant for the compiler's runtime,
    % so it is best to avoid that even if `--optimize-higher-order' is not set.
    leaf_foldr2_pred(P, T, !AccA, !AccB),
    fold2_bits(high_to_low, P, H ^ leaf_offset, H ^ leaf_bits, bits_per_int,
        !AccA, !AccB).

%---------------------%

:- type fold_direction
    --->    low_to_high
    ;       high_to_low.

    % Do a binary search for the 1 bits in an int.
    %
:- pred fold_bits(fold_direction, pred(T, U, U),
    int, uint, int, U, U) <= enum(T).
:- mode fold_bits(in, pred(in, in, out) is det,
    in, in, in, in, out) is det.
:- mode fold_bits(in, pred(in, mdi, muo) is det,
    in, in, in, mdi, muo) is det.
:- mode fold_bits(in, pred(in, di, uo) is det,
    in, in, in, di, uo) is det.
:- mode fold_bits(in, pred(in, in, out) is semidet,
    in, in, in, in, out) is semidet.
:- mode fold_bits(in, pred(in, mdi, muo) is semidet,
    in, in, in, mdi, muo) is semidet.
:- mode fold_bits(in, pred(in, di, uo) is semidet,
    in, in, in, di, uo) is semidet.
:- mode fold_bits(in, pred(in, in, out) is nondet,
    in, in, in, in, out) is nondet.
:- mode fold_bits(in, pred(in, mdi, muo) is nondet,
    in, in, in, mdi, muo) is nondet.
:- mode fold_bits(in, pred(in, in, out) is cc_multi,
    in, in, in, in, out) is cc_multi.
:- mode fold_bits(in, pred(in, di, uo) is cc_multi,
    in, in, in, di, uo) is cc_multi.
:- pragma type_spec(pred(fold_bits/7), T = int).
:- pragma type_spec(pred(fold_bits/7), T = var(_)).

fold_bits(Dir, P, Offset, Bits, Size, !Acc) :-
    ( if Bits = 0u then
        true
    else if Size = 1 then
        Elem = index_to_enum(Offset),
        P(Elem, !Acc)
    else
        HalfSize = unchecked_right_shift(Size, 1),
        Mask = mask(HalfSize),

        % Extract the low-order half of the bits.
        LowBits = Mask /\ Bits,

        % Extract the high-order half of the bits.
        HighBits = Mask /\ unchecked_right_shift(Bits, HalfSize),

        (
            Dir = low_to_high,
            fold_bits(Dir, P, Offset, LowBits, HalfSize, !Acc),
            fold_bits(Dir, P, Offset + HalfSize, HighBits, HalfSize, !Acc)
        ;
            Dir = high_to_low,
            fold_bits(Dir, P, Offset + HalfSize, HighBits, HalfSize, !Acc),
            fold_bits(Dir, P, Offset, LowBits, HalfSize, !Acc)
        )
    ).

:- pred fold2_bits(fold_direction, pred(T, U, U, V, V),
    int, uint, int, U, U, V, V) <= enum(T).
:- mode fold2_bits(in, pred(in, di, uo, di, uo) is det,
    in, in, in, di, uo, di, uo) is det.
:- mode fold2_bits(in, pred(in, in, out, di, uo) is det,
    in, in, in, in, out, di, uo) is det.
:- mode fold2_bits(in, pred(in, in, out, in, out) is det,
    in, in, in, in, out, in, out) is det.
:- mode fold2_bits(in, pred(in, in, out, in, out) is semidet,
    in, in, in, in, out, in, out) is semidet.
:- mode fold2_bits(in, pred(in, in, out, in, out) is nondet,
    in, in, in, in, out, in, out) is nondet.
:- mode fold2_bits(in, pred(in, di, uo, di, uo) is cc_multi,
    in, in, in, di, uo, di, uo) is cc_multi.
:- mode fold2_bits(in, pred(in, in, out, di, uo) is cc_multi,
    in, in, in, in, out, di, uo) is cc_multi.
:- mode fold2_bits(in, pred(in, in, out, in, out) is cc_multi,
    in, in, in, in, out, in, out) is cc_multi.
:- pragma type_spec(pred(fold2_bits/9), T = int).
:- pragma type_spec(pred(fold2_bits/9), T = var(_)).

fold2_bits(Dir, P, Offset, Bits, Size, !AccA, !AccB) :-
    ( if Bits = 0u then
        true
    else if Size = 1 then
        Elem = index_to_enum(Offset),
        P(Elem, !AccA, !AccB)
    else
        HalfSize = unchecked_right_shift(Size, 1),
        Mask = mask(HalfSize),

        % Extract the low-order half of the bits.
        LowBits = Mask /\ Bits,

        % Extract the high-order half of the bits.
        HighBits = Mask /\ unchecked_right_shift(Bits, HalfSize),

        (
            Dir = low_to_high,
            fold2_bits(Dir, P, Offset, LowBits, HalfSize, !AccA, !AccB),
            fold2_bits(Dir, P, Offset + HalfSize, HighBits, HalfSize,
                !AccA, !AccB)
        ;
            Dir = high_to_low,
            fold2_bits(Dir, P, Offset + HalfSize, HighBits, HalfSize,
                !AccA, !AccB),
            fold2_bits(Dir, P, Offset, LowBits, HalfSize, !AccA, !AccB)
        )
    ).

%---------------------------------------------------------------------------%
%
% Utility predicates and functions for the rest of the module above.
%

    % Return the offset of the element of a set which should contain the given
    % element, and an int with the bit corresponding to that element set.
    %
:- pred bits_for_index(int::in, int::out, uint::out) is det.
:- pragma inline(pred(bits_for_index/3)).

bits_for_index(Index, Offset, Bits) :-
    Offset = int.floor_to_multiple_of_bits_per_int(Index),
    BitToSet = Index - Offset,
    Bits = set_bit(0u, BitToSet).

:- func get_bit(uint, int) = uint.
:- pragma inline(func(get_bit/2)).

get_bit(Int, Bit) = Int /\ unchecked_left_shift(1u, Bit).

:- func set_bit(uint, int) = uint.
:- pragma inline(func(set_bit/2)).

set_bit(Int0, Bit) = Int0 \/ unchecked_left_shift(1u, Bit).

:- func clear_bit(uint, int) = uint.
:- pragma inline(func(clear_bit/2)).

clear_bit(Int0, Bit) = Int0 /\ \ unchecked_left_shift(1u, Bit).

    % `mask(N)' returns a mask which can be `and'ed with an integer to return
    % the lower `N' bits of the integer. `N' must be less than bits_per_int.
    %
:- func mask(int) = uint.
:- pragma inline(func(mask/1)).

mask(N) = \ unchecked_left_shift(\ 0u, N).

%---------------------------------------------------------------------------%
:- end_module tree_bitset.
%---------------------------------------------------------------------------%
