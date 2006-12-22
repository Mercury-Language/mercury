%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
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
% tree_bitsets. However, since the depth of the tree has a small upper bound,
% we will fold this into the "higher constant factors" in the descriptions of
% the complexity of the individual operations below.
%
% All this means that using a tree_bitset in preference to a sparse_bitset
% is likely to be a good idea only when the sizes of the sets to be manipulated
% are quite big, or when worst-case performance is important.
%
% For the time being, this module can only handle items that map to nonnegative
% integers. This may change once unsigned integer operations are available.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module tree_bitset.
:- interface.

:- import_module enum.
:- import_module list.
:- import_module term.

:- use_module set.

%-----------------------------------------------------------------------------%

:- type tree_bitset(T). % <= enum(T).

    % Return an empty set.
    %
:- func init = tree_bitset(T).

:- pred empty(tree_bitset(T)).
:- mode empty(in) is semidet.
:- mode empty(out) is det.

    % `equal(SetA, SetB)' is true iff `SetA' and `SetB' contain the same
    % elements. Takes O(min(card(SetA), card(SetB))) time.
    %
:- pred equal(tree_bitset(T)::in, tree_bitset(T)::in) is semidet <= enum(T).

    % `list_to_set(List)' returns a set containing only the members of `List'.
    % Takes O(length(List)) time and space.
    %
:- func list_to_set(list(T)) = tree_bitset(T) <= enum(T).

    % `sorted_list_to_set(List)' returns a set containing only the members
    % of `List'. `List' must be sorted. Takes O(length(List)) time and space.
    %
:- func sorted_list_to_set(list(T)) = tree_bitset(T) <= enum(T).

    % `from_set(Set)' returns a bitset containing only the members of `Set'.
    % Takes O(card(Set)) time and space.
    %
:- func from_set(set.set(T)) = tree_bitset(T) <= enum(T).

    % `to_sorted_list(Set)' returns a list containing all the members of `Set',
    % in sorted order. Takes O(card(Set)) time and space.
    %
:- func to_sorted_list(tree_bitset(T)) = list(T) <= enum(T).

    % `to_sorted_list(Set)' returns a set.set containing all the members
    % of `Set', in sorted order. Takes O(card(Set)) time and space.
    %
:- func to_set(tree_bitset(T)) = set.set(T) <= enum(T).

    % `make_singleton_set(Elem)' returns a set containing just the single
    % element `Elem'.
    %
:- func make_singleton_set(T) = tree_bitset(T) <= enum(T).

    % `subset(Subset, Set)' is true iff `Subset' is a subset of `Set'.
    % Same as `intersect(Set, Subset, Subset)', but may be more efficient.
    %
:- pred subset(tree_bitset(T)::in, tree_bitset(T)::in) is semidet.

    % `superset(Superset, Set)' is true iff `Superset' is a superset of `Set'.
    % Same as `intersect(Superset, Set, Set)', but may be more efficient.
    %
:- pred superset(tree_bitset(T)::in, tree_bitset(T)::in) is semidet.

    % `contains(Set, X)' is true iff `X' is a member of `Set'.
    % Takes O(log(card(Set))) time.
    %
:- pred contains(tree_bitset(T)::in, T::in) is semidet <= enum(T).

    % `member(Set, X)' is true iff `X' is a member of `Set'.
    % Takes O(card(Set)) time for the semidet mode.
    %
:- pred member(T, tree_bitset(T)) <= enum(T).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

    % `insert(Set, X)' returns the union of `Set' and the set containing
    % only `X'. Takes O(log(card(Set))) time and space.
    %
:- func insert(tree_bitset(T), T) = tree_bitset(T) <= enum(T).
:- pred insert(tree_bitset(T)::in, T::in, tree_bitset(T)::out)
    is det <= enum(T).

    % `insert_list(Set, X)' returns the union of `Set' and the set containing
    % only the members of `X'. Same as `union(Set, list_to_set(X))', but may be
    % more efficient.
    %
:- func insert_list(tree_bitset(T), list(T)) = tree_bitset(T) <= enum(T).
:- pred insert_list(tree_bitset(T)::in, list(T)::in, tree_bitset(T)::out)
    is det <= enum(T).

    % `delete(Set, X)' returns the difference of `Set' and the set containing
    % only `X'. Takes O(card(Set)) time and space.
    %
:- func delete(tree_bitset(T), T) = tree_bitset(T) <= enum(T).
:- pred delete(tree_bitset(T)::in, T::in, tree_bitset(T)::out)
    is det <= enum(T).

    % `delete_list(Set, X)' returns the difference of `Set' and the set
    % containing only the members of `X'. Same as
    % `difference(Set, list_to_set(X))', but may be more efficient.
    %
:- func delete_list(tree_bitset(T), list(T)) = tree_bitset(T) <= enum(T).
:- pred delete_list(tree_bitset(T)::in, list(T)::in, tree_bitset(T)::out)
    is det <= enum(T).

    % `remove(Set0, X, Set)' returns in `Set' the difference of `Set0'
    % and the set containing only `X', failing if `Set0' does not contain `X'.
    % Takes O(log(card(Set))) time and space.
    %
:- pred remove(tree_bitset(T)::in, T::in, tree_bitset(T)::out)
    is semidet <= enum(T).

    % `remove_list(Set0, X, Set)' returns in `Set' the difference of `Set0'
    % and the set containing all the elements of `X', failing if any element
    % of `X' is not in `Set0'. Same as `subset(list_to_set(X), Set0),
    % difference(Set0, list_to_set(X), Set)', but may be more efficient.
    %
:- pred remove_list(tree_bitset(T)::in, list(T)::in, tree_bitset(T)::out)
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
:- pred remove_least(tree_bitset(T)::in, T::out, tree_bitset(T)::out)
    is semidet <= enum(T).

    % `union(SetA, SetB)' returns the union of `SetA' and `SetB'. The
    % efficiency of the union operation is not sensitive to the argument
    % ordering. Takes somewhere between O(log(card(SetA)) + log(card(SetB)))
    % and O(card(SetA) + card(SetB)) time and space.
    %
:- func union(tree_bitset(T), tree_bitset(T)) = tree_bitset(T).
:- pred union(tree_bitset(T)::in, tree_bitset(T)::in, tree_bitset(T)::out)
    is det.

    % `intersect(SetA, SetB)' returns the intersection of `SetA' and `SetB'.
    % The efficiency of the intersection operation is not sensitive to the
    % argument ordering. Takes somewhere between
    % O(log(card(SetA)) + log(card(SetB))) and O(card(SetA) + card(SetB)) time,
    % and O(min(card(SetA)), card(SetB)) space.
    %
:- func intersect(tree_bitset(T), tree_bitset(T)) = tree_bitset(T).
:- pred intersect(tree_bitset(T)::in, tree_bitset(T)::in, tree_bitset(T)::out)
    is det.

    % `difference(SetA, SetB)' returns the set containing all the elements
    % of `SetA' except those that occur in `SetB'. Takes somewhere between
    % O(log(card(SetA)) + log(card(SetB))) and O(card(SetA) + card(SetB)) time,
    % and O(card(SetA)) space.
    %
:- func difference(tree_bitset(T), tree_bitset(T)) = tree_bitset(T).
:- pred difference(tree_bitset(T)::in, tree_bitset(T)::in, tree_bitset(T)::out)
    is det.

    % `count(Set)' returns the number of elements in `Set'.
    % Takes O(card(Set)) time.
    %
:- func count(tree_bitset(T)) = int <= enum(T).

    % `foldl(Func, Set, Start)' calls Func with each element of `Set'
    % (in sorted order) and an accumulator (with the initial value of `Start'),
    % and returns the final value. Takes O(card(Set)) time.
    %
:- func foldl(func(T, U) = U, tree_bitset(T), U) = U <= enum(T).

:- pred foldl(pred(T, U, U), tree_bitset(T), U, U) <= enum(T).
:- mode foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, in, out) is nondet, in, in, out) is nondet.
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

    % `filter(Pred, Set)' removes those elements from `Set' for which
    % `Pred' fails. In other words, it returns the set consisting of those
    % elements of `Set' for which `Pred' succeeds.
    %
:- func filter(pred(T), tree_bitset(T)) = tree_bitset(T) <= enum(T).
:- mode filter(pred(in) is semidet, in) = out is det.

%-----------------------------------------------------------------------------%
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

:- pragma type_spec(to_set/1, T = var(_)).
:- pragma type_spec(to_set/1, T = int).

:- pragma type_spec(from_set/1, T = var(_)).
:- pragma type_spec(from_set/1, T = int).

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

:- pragma type_spec(foldr/4, T = int).
:- pragma type_spec(foldr/4, T = var(_)).

:- pragma type_spec(foldl/3, T = int).
:- pragma type_spec(foldl/3, T = var(_)).

:- pragma type_spec(foldl/4, T = int).
:- pragma type_spec(foldl/4, T = var(_)).

:- pragma type_spec(list_to_set/1, T = var(_)).
:- pragma type_spec(list_to_set/1, T = int).

:- pragma type_spec(sorted_list_to_set/1, T = var(_)).
:- pragma type_spec(sorted_list_to_set/1, T = int).

:- pragma type_spec(to_sorted_list/1, T = var(_)).
:- pragma type_spec(to_sorted_list/1, T = int).

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

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.    % for ++ on strings in exceptions

% These are needed only for integrity checking.
:- import_module bool.
:- import_module maybe.
:- import_module pair.

%-----------------------------------------------------------------------------%

    % We describe a set using a tree. The basic idea is the following.
    %
    % - Level 0 nodes are leaf nodes. A leaf node contains a bitmap of
    %   bits_per_int bits.
    %
    % - Level k > 0 nodes are interior nodes. An interior node of level k + 1
    %   has up to 2 ^ bits_per_level children of level k.
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
    % of nodes one level down. (For level 1 interior nodes, this means
    % a list of leaf nodes; for interior nodes of level k+1, this means
    % a list of interior nodes of level k.)
    %
    % Invariants:
    %
    % - In every list of nodes, all the nodes in the list have the same level.
    %
    % - In every list of nodes, the list elements are sorted on offset, and
    %   no two elements have the same offset.
    %
    % - A list of nodes of level N must have the ranges of all its nodes
    %   contained within the range of a single level N+1 node.
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
                level           :: int,
                                % Convenient but redundant; could be computed
                                % from the init_offset and limit_offset fields
                                % of the nodes.
                interior_nodes  :: list(interior_node)
            ).

:- type leaf_node
    --->    leaf_node(
                leaf_offset     :: int,
                                % multiple of bits_per_int

                leaf_bits       :: int
                                % bits offset .. offset + bits_per_int - 1
                                % The tree_bitset operations all remove
                                % elements of the list with a `bits'
                                % field of zero.
            ).

:- type interior_node
    --->    interior_node(
                init_offset     :: int,
                                % multiple of
                                % bits_per_int * 2 ^ (level * bits_per_level)
                limit_offset    :: int,
                                % limit_offset = init_offset +
                                %   bits_per_int * 2 ^ (level * bits_per_level)
                components      :: node_list
            ).

:- func bits_per_level = int.

bits_per_level = 5.

%-----------------------------------------------------------------------------%

:- func make_leaf_node(int, int) = leaf_node.
:- pragma inline(make_leaf_node/2).

:- pragma foreign_decl("C", "
    #include ""mercury_heap.h"" /* for MR_tag_offset_incr_hp_atomic_msg() */
").

    % The bit pattern will often look like a pointer, so allocate the pairs
    % using GC_malloc_atomic() to avoid unnecessary memory retention. Doing
    % this slows down the compiler by about 1%, but in a library module it is
    % better to be safe.
:- pragma foreign_proc("C",
    make_leaf_node(A::in, B::in) = (Node::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
#define ML_BITSET_TAG MR_FIRST_UNRESERVED_RAW_TAG

    MR_tag_offset_incr_hp_atomic_msg(Node, MR_mktag(ML_BITSET_TAG),
        MR_SIZE_SLOT_SIZE, MR_SIZE_SLOT_SIZE + 2,
        MR_PROC_LABEL, ""tree_bitset:leaf_node/0"");
    MR_define_size_slot(MR_mktag(ML_BITSET_TAG), Node, 1);
    MR_field(MR_mktag(ML_BITSET_TAG), Node, 0) = A;
    MR_field(MR_mktag(ML_BITSET_TAG), Node, 1) = B;
}").

make_leaf_node(Offset, Bits) = leaf_node(Offset, Bits).

%-----------------------------------------------------------------------------%

:- func enum_to_index(T) = int <= enum(T).

enum_to_index(Elem) = Index :-
    Int = enum.to_int(Elem),
    ( Int < 0 ->
        error("tree_bitset.m: enums must map to nonnegative integers")
    ;
        Index = Int
    ).

:- func index_to_enum(int) = T <= enum(T).

index_to_enum(Index) = Elem :-
    ( Elem0 = enum.from_int(Index) ->
        Elem = Elem0
    ;
        % We only apply `from_int/1' to integers returned by `to_int/1',
        % so it should never fail.
        error("tree_bitset.m: `enum.from_int/1' failed")
    ).

%-----------------------------------------------------------------------------%

% This function is the only place in the module that adds the tree_bitset/1
% wrapper around node lists, and therefore the only place that constructs
% terms that are semantically tree_bitsets. Invoking our integrity test from
% here thus guarantees that we never return any malformed tree_bitsets.
%
% If you want to use the integrity checking version of wrap_tree_bitset,
% you'll probably also want to use the integrity checking version of equal,
% and enable the integrity checking code in expand_range. Search for the
% keyword CHECK_INTEGRITY.

:- func wrap_tree_bitset(node_list) = tree_bitset(T).
:- pragma inline(wrap_tree_bitset/1).

wrap_tree_bitset(NodeList) = Set :-
    Set = tree_bitset(NodeList).

% CHECK_INTEGRITY
% wrap_tree_bitset(NodeList) = Set :-
%     ( integrity(no, NodeList) = no ->
%         error("wrap_tree_bitset: integrity failed")
%     ;
%         Set = tree_bitset(NodeList)
%     ).

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
                (
                    Init = ParentInitOffset,
                    Limit = ParentLimitOffset
                ->
                    LimitOK = yes
                ;
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
                (
                    Init = ParentInitOffset,
                    Limit = ParentLimitOffset
                ->
                    LimitOK = yes
                ;
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
    ( Offset rem bits_per_int > 0 ->
        OK = no
    ; \+ (Init =< Offset, Offset + bits_per_int - 1 < Limit) ->
        OK = no
    ;
        OK = integrity_leaf_nodes(Tail, Init, Limit)
    ).

:- func integrity_interior_nodes(list(interior_node), int, int, int) = bool.

integrity_interior_nodes([], _Level, _Init, _Limit) = yes.
integrity_interior_nodes([Head | Tail], Level, Init, Limit) = OK :-
    Head = interior_node(NodeInit, NodeLimit, Components),
    CalcLimit = NodeInit +
        unchecked_left_shift(bits_per_int, Level * bits_per_level),
    ( NodeInit rem bits_per_int > 0 ->
        OK = no
    ; NodeLimit rem bits_per_int > 0 ->
        OK = no
    ; NodeLimit \= CalcLimit ->
        OK = no
    ; \+ (Init =< NodeInit, NodeLimit - 1 < Limit) ->
        OK = no
    ;
        (
            Components = leaf_list(LeafNodes),
            ( Level = 1 ->
                SubOK = integrity_leaf_nodes(LeafNodes, NodeInit, NodeLimit)
            ;
                SubOK = no
            )
        ;
            Components = interior_list(CompLevel, InteriorNodes),
            ( CompLevel = Level - 1 ->
                SubOK = integrity_interior_nodes(InteriorNodes, CompLevel,
                    NodeInit, NodeLimit)
            ;
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

%-----------------------------------------------------------------------------%

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
% CHECK_INTEGRITY
%   (
%       Range = unchecked_left_shift(bits_per_int, CurLevel * bits_per_level),
%       ( CurLimitOffset - CurInitOffset = Range ->
%           true
%       ;
%           error("tree_bitset.m: expand_range: bad range for level")
%       )
%   ;
%       true
%   ),
    CurNode = interior_node(CurInitOffset, CurLimitOffset, SubNodes),
    range_of_parent_node(CurInitOffset, CurLevel,
        ParentInitOffset, ParentLimitOffset),
    (
        ParentInitOffset =< Index,
        Index < ParentLimitOffset
    ->
        TopNode = CurNode,
        TopLevel = CurLevel
    ;
        expand_range(Index, interior_list(CurLevel, [CurNode]), CurLevel + 1,
            ParentInitOffset, ParentLimitOffset, TopNode, TopLevel)
    ).

:- pred raise_leaves_to_interior(leaf_node::in, list(leaf_node)::in,
    interior_node::out) is det.
:- pragma inline(raise_leaves_to_interior/3).

raise_leaves_to_interior(LeafNode, LeafNodes, InteriorNode) :-
    range_of_parent_node(LeafNode ^ leaf_offset, 0,
        ParentInitOffset, ParentLimitOffset),
    NodeList = leaf_list([LeafNode | LeafNodes]),
    InteriorNode = interior_node(ParentInitOffset, ParentLimitOffset,
        NodeList).

:- pred raise_leaf_to_level(int::in, leaf_node::in, interior_node::out)
    is det.
:- pragma inline(raise_leaf_to_level/3).

raise_leaf_to_level(TargetLevel, LeafNode, TopNode) :-
    raise_leaves_to_interior(LeafNode, [], ParentNode),
    raise_one_interior_to_level(TargetLevel, 1, ParentNode, TopNode).

:- pred raise_one_interior_to_level(int::in, int::in,
    interior_node::in, interior_node::out) is det.

raise_one_interior_to_level(TargetLevel, CurLevel, CurNode, TopNode) :-
    ( CurLevel = TargetLevel ->
        TopNode = CurNode
    ;
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
    ( CurLevel = TargetLevel ->
        TopNodesHead = CurNodesHead,
        TopNodesTail = CurNodesTail
    ;
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
    ( ParentInitOffsetA = ParentInitOffsetB ->
        require(unify(ParentLimitOffsetA, ParentLimitOffsetB),
            "tree_bitset.m: raise_to_common_level: limit mismatch"),
        TopHeadA = HeadA,
        TopTailA = TailA,
        TopHeadB = HeadB,
        TopTailB = TailB,
        TopLevel = CurLevel
    ;
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
    error("tree_bitset.m: empty list in head_and_tail").
head_and_tail([Head | Tail], Head, Tail).

%-----------------------------------------------------------------------------%

init = wrap_tree_bitset(leaf_list([])).

empty(init).

equal(SetA, SetB) :-
    SetA = SetB.

% CHECK_INTEGRITY
% equal(SetA, SetB) :-
%     (
%         to_sorted_list(SetA, ListA),
%         to_sorted_list(SetB, ListB),
%         (
%             SetA = SetB
%         <=>
%             ListA = ListB
%         )
%     ->
%         true
%     ;
%         error("tree_bitset.m: equal: set and list equality differ")
%     )
%     SetA = SetB.

%-----------------------------------------------------------------------------%

to_sorted_list(Set) = foldr(list.cons, Set, []).

to_set(Set) = set.sorted_list_to_set(to_sorted_list(Set)).

from_set(Set) = sorted_list_to_set(set.to_sorted_list(Set)).

%-----------------------------------------------------------------------------%

% XXX We should make this more efficient. At least, we could filter the bits in
% the leaf nodes, yielding a new list of leaf nodes, and we could put the
% interior nodes on top, just as we do in sorted_list_to_set.

filter(Pred, Set0) = Set :-
    SortedList0 = to_sorted_list(Set0),
    FilteredList = list.filter(Pred, SortedList0),
    Set = sorted_list_to_set(FilteredList).

%-----------------------------------------------------------------------------%

count(Set) = foldl((func(_, Acc) = Acc + 1), Set, 0).

%-----------------------------------------------------------------------------%

make_singleton_set(A) = insert(init, A).

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
            (
                ParentInitOffset =< Index,
                Index < ParentLimitOffset
            ->
                leaflist_insert(Index, LeafList0, LeafList),
                Set = wrap_tree_bitset(leaf_list(LeafList))
            ;
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
            error("tree_bitset.m :insert into empty list of interior nodes")
        ;
            InteriorList0 = [InteriorNode0 | _],
            range_of_parent_node(InteriorNode0 ^ init_offset, InteriorLevel,
                ParentInitOffset, ParentLimitOffset),
            (
                ParentInitOffset =< Index,
                Index < ParentLimitOffset
            ->
                interiorlist_insert(Index, InteriorLevel,
                    InteriorList0, InteriorList),
                Set = wrap_tree_bitset(
                    interior_list(InteriorLevel, InteriorList))
            ;
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

:- pred leaflist_insert(int::in, list(leaf_node)::in, list(leaf_node)::out)
    is det.

leaflist_insert(Index, [], Leaves) :-
    bits_for_index(Index, Offset, Bits),
    Leaves = [make_leaf_node(Offset, Bits)].
leaflist_insert(Index, Leaves0 @ [Head0 | Tail0], Leaves) :-
    Offset0 = Head0 ^ leaf_offset,
    ( Index < Offset0 ->
        bits_for_index(Index, Offset, Bits),
        Leaves = [make_leaf_node(Offset, Bits) | Leaves0]
    ; BitToSet = Index - Offset0, BitToSet < bits_per_int ->
        Bits0 = Head0 ^ leaf_bits,
        ( get_bit(Bits0, BitToSet) \= 0 ->
            Leaves = Leaves0
        ;
            Bits = set_bit(Bits0, BitToSet),
            Leaves = [make_leaf_node(Offset0, Bits) | Tail0]
        )
    ;
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
    ( Index < Offset0 ->
        bits_for_index(Index, Offset, Bits),
        raise_leaf_to_level(Level, make_leaf_node(Offset, Bits), Node),
        Nodes = [Node | Nodes0]
    ; Head0 ^ init_offset =< Index, Index < Head0 ^ limit_offset ->
        Components0 = Head0 ^ components,
        (
            Components0 = leaf_list(LeafList0),
            require(unify(Level, 1),
                "interiorlist_insert: bad component list (leaf)"),
            leaflist_insert(Index, LeafList0, LeafList),
            Components = leaf_list(LeafList)
        ;
            Components0 = interior_list(InteriorLevel, InteriorList0),
            require(unify(InteriorLevel, Level - 1),
                "interiorlist_insert: bad component list (interior)"),
            interiorlist_insert(Index, InteriorLevel,
                InteriorList0, InteriorList),
            Components = interior_list(InteriorLevel, InteriorList)
        ),
        Head = Head0 ^ components := Components,
        Nodes = [Head | Tail0]
    ;
        interiorlist_insert(Index, Level, Tail0, Tail),
        Nodes = [Head0 | Tail]
    ).

%-----------------------------------------------------------------------------%

insert_list(Set, List) = union(list_to_set(List), Set).

delete(Set, Elem) = difference(Set, insert(init, Elem)).

delete_list(Set, List) = difference(Set, list_to_set(List)).

remove(Set0, Elem, Set) :-
    contains(Set0, Elem),
    Set = delete(Set0, Elem).

remove_list(Set0, Elems, Set) :-
    ElemsSet = list_to_set(Elems),
    subset(ElemsSet, Set0),
    Set = difference(Set0, ElemsSet).

%-----------------------------------------------------------------------------%

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
    ( Head0 ^ limit_offset =< Index ->
        remove_leq_interior(Tail0, Index, Result)
    ; Head0 ^ init_offset =< Index ->
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
    ;
        Result = [Head0 | Tail0]
    ).

:- pred remove_leq_leaf(list(leaf_node)::in, int::in,
    list(leaf_node)::out) is det.

remove_leq_leaf([], _, []).
remove_leq_leaf([Head0 | Tail0], Index, Result) :-
    Offset = Head0 ^ leaf_offset,
    ( Offset + bits_per_int =< Index ->
        remove_leq_leaf(Tail0, Index, Result)
    ; Offset =< Index ->
        (
            Bits = Head0 ^ leaf_bits /\
                unchecked_left_shift(\ 0, Index - Offset + 1),
            Bits \= 0
        ->
            Result = [make_leaf_node(Offset, Bits) | Tail0]
        ;
            Result = Tail0
        )
    ;
        Result = [Head0 | Tail0]
    ).

%-----------------------------------------------------------------------------%

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
    ( Head0 ^ limit_offset =< Index ->
        remove_gt_interior(Tail0, Index, Tail),
        Result = [Head0 | Tail]
    ; Head0 ^ init_offset =< Index ->
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
    ;
        Result = []
    ).

:- pred remove_gt_leaf(list(leaf_node)::in, int::in,
    list(leaf_node)::out) is det.

remove_gt_leaf([], _, []).
remove_gt_leaf([Head0 | Tail0], Index, Result) :-
    Offset = Head0 ^ leaf_offset,
    ( Offset + bits_per_int - 1 =< Index ->
        remove_gt_leaf(Tail0, Index, Tail),
        Result = [Head0 | Tail]
    ; Offset =< Index ->
        (
            Bits = Head0 ^ leaf_bits /\
                \ unchecked_left_shift(\ 0, Index - Offset + 1),
            Bits \= 0
        ->
            Result = [make_leaf_node(Offset, Bits)]
        ;
            Result = []
        )
    ;
        Result = []
    ).

%-----------------------------------------------------------------------------%

remove_least(Set0, Elem, Set) :-
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
            error("tree_bitset.m: remove_least: empty InteriorNodes0")
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
            error("tree_bitset.m: remove_least_interior: empty LeafNodes0")
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
            error("tree_bitset.m: remove_least_interior: empty InteriorNodes0")
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
    ( Bits = 0 ->
        Nodes = Tail0
    ;
        Nodes = [make_leaf_node(Offset, Bits) | Tail0]
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
            HighBits = Mask /\ unchecked_right_shift(Bits0, HalfSize),
            BitNum = find_least_bit_2(HighBits, HalfSize, BitNum0 + HalfSize)
        )
    ).

%-----------------------------------------------------------------------------%

list_to_set(List) = sorted_list_to_set(list.sort(List)).

%-----------------------------------------------------------------------------%

sorted_list_to_set(Elems) = Set :-
    items_to_index(Elems, Indexes),
    % XXX
    % Should we sort Indexes? The fact that Elems is sorted
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
            error("tree_bitset.m: sorted_list_to_set: empty InteriorNodes0")
        ;
            InteriorNodes0 = [InteriorNode],
            List = InteriorNode ^ components
        ;
            InteriorNodes0 = [_, _ | _],
            recursively_group_interior_nodes(1, InteriorNodes0, List)
        ),
        Set = wrap_tree_bitset(List)
    ).

:- pred items_to_index(list(T)::in, list(int)::out) is det <= enum(T).
:- pragma type_spec(items_to_index/2, T = var(_)).
:- pragma type_spec(items_to_index/2, T = int).

items_to_index([], []).
items_to_index([ElemHead | ElemTail], [IndexHead | IndexTail]) :-
    IndexHead = enum_to_index(ElemHead),
    items_to_index(ElemTail, IndexTail).

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
    ( ParentInitOffset = HeadParentInitOffset ->
        require(unify(ParentLimitOffset, HeadParentLimitOffset),
            "tree_bitset.m: group_leaf_nodes_in_range: limit mismatch"),
        !:RevAcc = [Head | !.RevAcc],
        group_leaf_nodes_in_range(ParentInitOffset, ParentLimitOffset,
            !.RevAcc, Tail, ParentNode, Remaining)
    ;
        ParentNode = interior_node(ParentInitOffset, ParentLimitOffset,
            leaf_list(list.reverse(!.RevAcc))),
        Remaining = [Head | Tail]
    ).

:- pred recursively_group_interior_nodes(int::in, list(interior_node)::in,
    node_list::out) is det.

recursively_group_interior_nodes(CurLevel, CurNodes, List) :-
    (
        CurNodes = [],
        error("tree_bitset.m: recursively_group_interior_nodes: empty CurNodes")
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
    ( ParentInitOffset = HeadParentInitOffset ->
        require(unify(ParentLimitOffset, HeadParentLimitOffset),
            "tree_bitset.m: group_interior_nodes_in_range: limit mismatch"),
        !:RevAcc = [Head | !.RevAcc],
        group_interior_nodes_in_range(Level,
            ParentInitOffset, ParentLimitOffset,
            !.RevAcc, Tail, ParentNode, Remaining)
    ;
        ParentNode = interior_node(ParentInitOffset, ParentLimitOffset,
            interior_list(Level, list.reverse(!.RevAcc))),
        Remaining = [Head | Tail]
    ).

:- func sorted_list_to_leaf_nodes(list(int)) = list(leaf_node).

sorted_list_to_leaf_nodes([]) = [].
sorted_list_to_leaf_nodes([Head | Tail]) = LeafNodes :-
    bits_for_index(Head, Offset, HeadBits),
    gather_bits_for_leaf(Tail, Offset, HeadBits, Bits, Remaining),
    sorted_list_to_leaf_nodes(Remaining) = LeafNodesTail,
    LeafNodes = [make_leaf_node(Offset, Bits) | LeafNodesTail].

:- pred gather_bits_for_leaf(list(int)::in, int::in, int::in, int::out,
    list(int)::out) is det.

gather_bits_for_leaf([], _Offset, !Bits, []).
gather_bits_for_leaf(List @ [Head | Tail], Offset, !Bits, Remaining) :-
    bits_for_index(Head, HeadOffset, HeadBits),
    ( HeadOffset = Offset ->
        !:Bits = !.Bits \/ HeadBits,
        gather_bits_for_leaf(Tail, Offset, !Bits, Remaining)
    ;
        Remaining = List
    ).

%-----------------------------------------------------------------------------%

subset(Subset, Set) :-
    intersect(Set, Subset, Subset).

superset(Superset, Set) :-
    subset(Set, Superset).

%-----------------------------------------------------------------------------%

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
    ( Index < Offset + bits_per_int ->
        get_bit(Head ^ leaf_bits, Index - Offset) \= 0
    ;
        leaflist_contains(Tail, Index)
    ).

:- pred interiorlist_contains(list(interior_node)::in, int::in) is semidet.

interiorlist_contains([Head | Tail], Index) :-
    Index >= Head ^ init_offset,
    ( Index < Head ^ limit_offset ->
        Components = Head ^ components,
        (
            Components = leaf_list(LeafNodes),
            leaflist_contains(LeafNodes, Index)
        ;
            Components = interior_list(_, InteriorNodes),
            interiorlist_contains(InteriorNodes, Index)
        )
    ;
        interiorlist_contains(Tail, Index)
    ).

%-----------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(member/2).

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

:- pred leafnode_member(int::out, int::in, int::in, int::in) is nondet.

leafnode_member(Index, Offset, Size, Bits) :-
    ( Bits = 0 ->
        fail
    ; Size = 1 ->
        Index = Offset
    ;
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

%-----------------------------------------------------------------------------%

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
            ( ParentInitOffsetA = ParentInitOffsetB ->
                require(unify(ParentLimitOffsetA, ParentLimitOffsetB),
                    "tree_bitset.m: union: limit mismatch"),
                leaflist_union(LeafNodesA, LeafNodesB, LeafNodes),
                List = leaf_list(LeafNodes)
            ;
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
    ( OffsetA = OffsetB ->
        Head = make_leaf_node(OffsetA,
            (HeadA ^ leaf_bits) \/ (HeadB ^ leaf_bits)),
        leaflist_union(TailA, TailB, Tail),
        List = [Head | Tail]
    ; OffsetA < OffsetB ->
        leaflist_union(TailA, ListB, Tail),
        List = [HeadA | Tail]
    ;
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
    ( OffsetA = OffsetB ->
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
            error("tree_bitset.m: " ++
                "inconsistent components in interiorlist_union")
        ;
            ComponentsA = interior_list(_LevelA, _InteriorListA),
            ComponentsB = leaf_list(_LeafListB),
            error("tree_bitset.m: " ++
                "inconsistent components in interiorlist_union")
        ;
            ComponentsA = interior_list(LevelA, InteriorListA),
            ComponentsB = interior_list(LevelB, InteriorListB),
            require(unify(LevelA, LevelB),
                "tree_bitset.m: inconsistent levels in interiorlist_union"),
            interiorlist_union(InteriorListA, InteriorListB, InteriorList),
            Components = interior_list(LevelA, InteriorList),
            Head = interior_node(HeadA ^ init_offset, HeadA ^ limit_offset,
                Components)
        ),
        interiorlist_union(TailA, TailB, Tail),
        List = [Head | Tail]
    ; OffsetA < OffsetB ->
        interiorlist_union(TailA, ListB, Tail),
        List = [HeadA | Tail]
    ;
        interiorlist_union(ListA, TailB, Tail),
        List = [HeadB | Tail]
    ).

%-----------------------------------------------------------------------------%

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
            ( ParentInitOffsetA = ParentInitOffsetB ->
                require(unify(ParentLimitOffsetA, ParentLimitOffsetB),
                    "tree_bitset.m: intersect: limit mismatch"),
                leaflist_intersect(LeafNodesA, LeafNodesB, LeafNodes),
                List = leaf_list(LeafNodes)
            ;
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
        ( LevelA = LevelB ->
            interiorlist_intersect(InteriorNodesA, InteriorNodesB,
                InteriorNodes),
            List = interior_list(LevelA, InteriorNodes)
        ;
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

:- pred descend_and_intersect(int::in, interior_node::in,
    int::in, list(interior_node)::in, node_list::out) is det.

descend_and_intersect(_LevelA, _InteriorNodeA, _LevelB, [], List) :-
    List = leaf_list([]).
descend_and_intersect(LevelA, InteriorNodeA, LevelB, [HeadB | TailB], List) :-
    (
        HeadB ^ init_offset =< InteriorNodeA ^ init_offset,
        InteriorNodeA ^ limit_offset =< HeadB ^ limit_offset
    ->
        ( LevelA = LevelB ->
            require(unify(InteriorNodeA ^ init_offset, HeadB ^ init_offset),
                "tree_bitset.m: inconsistent inits in descend_and_intersect"),
            require(unify(InteriorNodeA ^ limit_offset, HeadB ^ limit_offset),
                "tree_bitset.m: inconsistent limits in descend_and_intersect"),
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
                error("tree_bitset.m: " ++
                    "inconsistent levels in descend_and_intersect")
            ;
                ComponentsA = interior_list(_, _),
                ComponentsB = leaf_list(_),
                error("tree_bitset.m: " ++
                    "inconsistent levels in descend_and_intersect")
            ;
                ComponentsA = interior_list(_SubLevelA, InteriorNodesA),
                ComponentsB = interior_list(_SubLevelB, InteriorNodesB),
                interiorlist_intersect(InteriorNodesA, InteriorNodesB,
                    InteriorNodes),
                List = interior_list(LevelA, InteriorNodes)
            )
        ;
            require(LevelA < LevelB,
                "tree_bitset.m: LevelA > LevelB in descend_and_intersect"),
            ComponentsB = HeadB ^ components,
            (
                ComponentsB = leaf_list(_),
                error("tree_bitset.m: " ++
                    "bad ComponentsB in descend_and_intersect")
            ;
                ComponentsB = interior_list(SubLevelB, InteriorNodesB),
                descend_and_intersect(LevelA, InteriorNodeA,
                    SubLevelB, InteriorNodesB, List)
            )
        )
    ;
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

:- pred leaflist_intersect(list(leaf_node)::in, list(leaf_node)::in,
    list(leaf_node)::out) is det.

leaflist_intersect([], [], []).
leaflist_intersect([], [_ | _], []).
leaflist_intersect([_ | _], [], []).
leaflist_intersect(ListA @ [HeadA | TailA], ListB @ [HeadB | TailB], List) :-
    OffsetA = HeadA ^ leaf_offset,
    OffsetB = HeadB ^ leaf_offset,
    ( OffsetA = OffsetB ->
        Bits = HeadA ^ leaf_bits /\ HeadB ^ leaf_bits,
        ( Bits = 0 ->
            leaflist_intersect(TailA, TailB, List)
        ;
            Head = make_leaf_node(OffsetA, Bits),
            leaflist_intersect(TailA, TailB, Tail),
            List = [Head | Tail]
        )
    ; OffsetA < OffsetB ->
        leaflist_intersect(TailA, ListB, List)
    ;
        leaflist_intersect(ListA, TailB, List)
    ).

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
    ( OffsetA = OffsetB ->
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
            error("tree_bitset.m: " ++
                "inconsistent components in interiorlist_intersect")
        ;
            ComponentsB = interior_list(_LevelB, _InteriorNodesB),
            ComponentsA = leaf_list(_LeafNodesA),
            error("tree_bitset.m: " ++
                "inconsistent components in interiorlist_intersect")
        ;
            ComponentsA = interior_list(LevelA, InteriorNodesA),
            ComponentsB = interior_list(LevelB, InteriorNodesB),
            require(unify(LevelA, LevelB),
                "tree_bitset.m: inconsistent levels in interiorlist_intersect"),
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
    ; OffsetA < OffsetB ->
        interiorlist_intersect(TailA, ListB, List)
    ;
        interiorlist_intersect(ListA, TailB, List)
    ).

%-----------------------------------------------------------------------------%

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
            ( ParentInitOffsetA = ParentInitOffsetB ->
                require(unify(ParentLimitOffsetA, ParentLimitOffsetB),
                    "tree_bitset.m: difference: limit mismatch"),
                leaflist_difference(LeafNodesA, LeafNodesB, LeafNodes),
                List = leaf_list(LeafNodes)
            ;
                % The ranges of the two sets do not overlap.
                List = ListA
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
            interiornode_difference(1, InteriorNodeA, [],
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
            interiornode_difference(LevelA, InteriorHeadA, InteriorTailA,
                1, InteriorNodeB, [], Level, InteriorNodes),
            List = interior_list(Level, InteriorNodes)
        )
    ;
        ListA = interior_list(LevelA, InteriorNodesA),
        ListB = interior_list(LevelB, InteriorNodesB),
        head_and_tail(InteriorNodesA, InteriorHeadA, InteriorTailA),
        head_and_tail(InteriorNodesB, InteriorHeadB, InteriorTailB),
        interiornode_difference(LevelA, InteriorHeadA, InteriorTailA,
            LevelB, InteriorHeadB, InteriorTailB, Level, InteriorNodes),
        List = interior_list(Level, InteriorNodes)
    ),
    prune_top_levels(List, PrunedList),
    Set = wrap_tree_bitset(PrunedList).

:- pred interiornode_difference(
    int::in, interior_node::in, list(interior_node)::in,
    int::in, interior_node::in, list(interior_node)::in,
    int::out, list(interior_node)::out) is det.

interiornode_difference(LevelA, HeadA, TailA, LevelB, HeadB, TailB,
        Level, List) :-
    ( LevelA < LevelB ->
        range_of_parent_node(HeadA ^ init_offset, LevelA + 1,
            ParentInitOffsetA, ParentLimitOffsetA),
        (
            find_containing_node(ParentInitOffsetA, ParentLimitOffsetA,
                [HeadB | TailB], ChosenB)
        ->
            ComponentsB = ChosenB ^ components,
            (
                ComponentsB = leaf_list(_),
                require(unify(LevelA, 1),
                    "tree_bitset.m: interiornode_difference: bad leaf level"),
                interiorlist_difference([HeadA | TailA], [ChosenB], List),
                Level = LevelA
            ;
                ComponentsB = interior_list(SubLevelB, SubNodesB),
                require(unify(LevelB, SubLevelB + 1),
                    "tree_bitset.m: interiornode_difference: bad levels"),
                head_and_tail(SubNodesB, SubHeadB, SubTailB),
                interiornode_difference(LevelA, HeadA, TailA,
                    SubLevelB, SubHeadB, SubTailB, Level, List)
            )
        ;
            Level = 1,
            List = []
        )
    ;
        raise_interiors_to_level(LevelA, LevelB, HeadB, TailB,
            RaisedHeadB, RaisedTailB),
        range_of_parent_node(HeadA ^ init_offset, LevelA,
            ParentInitOffsetA, ParentLimitOffsetA),
        range_of_parent_node(RaisedHeadB ^ init_offset, LevelA,
            ParentInitOffsetB, ParentLimitOffsetB),
        ( ParentInitOffsetA = ParentInitOffsetB ->
            require(unify(ParentLimitOffsetA, ParentLimitOffsetB),
                "tree_bitset.m: interiornode_difference: limit mismatch"),
            interiorlist_difference([HeadA | TailA],
                [RaisedHeadB | RaisedTailB], List),
            Level = LevelA
        ;
            Level = 1,
            List = []
        )
    ).

:- pred find_containing_node(int::in, int::in, list(interior_node)::in,
    interior_node::out) is semidet.

find_containing_node(InitOffsetA, LimitOffsetA, [HeadB | TailB], ChosenB) :-
    (
        HeadB ^ init_offset =< InitOffsetA,
        LimitOffsetA =< HeadB ^ limit_offset
    ->
        ChosenB = HeadB
    ;
        find_containing_node(InitOffsetA, LimitOffsetA, TailB, ChosenB)
    ).

:- pred leaflist_difference(list(leaf_node)::in, list(leaf_node)::in,
    list(leaf_node)::out) is det.

leaflist_difference([], [], []).
leaflist_difference([], [_ | _], []).
leaflist_difference(ListA @ [_ | _], [], ListA).
leaflist_difference(ListA @ [HeadA | TailA], ListB @ [HeadB | TailB], List) :-
    OffsetA = HeadA ^ leaf_offset,
    OffsetB = HeadB ^ leaf_offset,
    ( OffsetA = OffsetB ->
        Bits = (HeadA ^ leaf_bits) /\ \ (HeadB ^ leaf_bits),
        ( Bits = 0 ->
            leaflist_difference(TailA, TailB, List)
        ;
            Head = make_leaf_node(OffsetA, Bits),
            leaflist_difference(TailA, TailB, Tail),
            List = [Head | Tail]
        )
    ; OffsetA < OffsetB ->
        leaflist_difference(TailA, ListB, Tail),
        List = [HeadA | Tail]
    ;
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
    ( OffsetA = OffsetB ->
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
            error("tree_bitset.m: " ++
                "inconsistent components in interiorlist_difference")
        ;
            ComponentsB = interior_list(_LevelB, _InteriorNodesB),
            ComponentsA = leaf_list(_LeafNodesA),
            error("tree_bitset.m: " ++
                "inconsistent components in interiorlist_difference")
        ;
            ComponentsA = interior_list(LevelA, InteriorNodesA),
            ComponentsB = interior_list(LevelB, InteriorNodesB),
            require(unify(LevelA, LevelB),
                "tree_bitset.m: " ++
                "inconsistent levels in interiorlist_difference"),
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
    ; OffsetA < OffsetB ->
        interiorlist_difference(TailA, ListB, Tail),
        List = [HeadA | Tail]
    ;
        interiorlist_difference(ListA, TailB, List)
    ).

%-----------------------------------------------------------------------------%

    % Return the offset of the element of a set which should contain the given
    % element, and an int with the bit corresponding to that element set.
    %
:- pred bits_for_index(int::in, int::out, int::out) is det.
:- pragma inline(bits_for_index/3).

bits_for_index(Index, Offset, Bits) :-
    Offset = int.floor_to_multiple_of_bits_per_int(Index),
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

    % `mask(N)' returns a mask which can be `and'ed with an integer to return
    % the lower `N' bits of the integer. `N' must be less than bits_per_int.
    %
:- func mask(int) = int.
:- pragma inline(mask/1).

mask(N) = \ unchecked_left_shift(\ 0, N).

%-----------------------------------------------------------------------------%

insert(A, B, insert(A, B)).

insert_list(A, B, insert_list(A, B)).

delete(A, B, delete(A, B)).

delete_list(A, B, delete_list(A, B)).

union(A, B, union(A, B)).

intersect(A, B, intersect(A, B)).

difference(A, B, difference(A, B)).

%-----------------------------------------------------------------------------%

:- type fold_direction
    --->    low_to_high
    ;       high_to_low.

foldl(F, Set, Acc0) = Acc :-
    P = (pred(E::in, PAcc0::in, PAcc::out) is det :-
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

foldl2(P, Set, !AccA, !AccB) :-
    Set = tree_bitset(List),
    (
        List = leaf_list(LeafNodes),
        leaf_foldl2_pred(P, LeafNodes, !AccA, !AccB)
    ;
        List = interior_list(_, InteriorNodes),
        do_foldl2_pred(P, InteriorNodes, !AccA, !AccB)
    ).

:- pred do_foldl_pred(pred(T, U, U), list(interior_node), U, U) <= enum(T).
:- mode do_foldl_pred(pred(in, di, uo) is det, in, di, uo) is det.
:- mode do_foldl_pred(pred(in, in, out) is det, in, in, out) is det.
:- mode do_foldl_pred(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode do_foldl_pred(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode do_foldl_pred(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode do_foldl_pred(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.

:- pragma type_spec(do_foldl_pred/4, T = int).
:- pragma type_spec(do_foldl_pred/4, T = var(_)).

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
:- mode leaf_foldl_pred(pred(in, di, uo) is det, in, di, uo) is det.
:- mode leaf_foldl_pred(pred(in, in, out) is det, in, in, out) is det.
:- mode leaf_foldl_pred(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode leaf_foldl_pred(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode leaf_foldl_pred(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode leaf_foldl_pred(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.

:- pragma type_spec(leaf_foldl_pred/4, T = int).
:- pragma type_spec(leaf_foldl_pred/4, T = var(_)).

leaf_foldl_pred(_, [], !Acc).
leaf_foldl_pred(P, [H | T], !Acc) :-
    fold_bits(low_to_high, P, H ^ leaf_offset, H ^ leaf_bits, bits_per_int,
        !Acc),
    leaf_foldl_pred(P, T, !Acc).

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

:- pragma type_spec(do_foldl2_pred/6, T = int).
:- pragma type_spec(do_foldl2_pred/6, T = var(_)).

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

:- pragma type_spec(leaf_foldl2_pred/6, T = int).
:- pragma type_spec(leaf_foldl2_pred/6, T = var(_)).

leaf_foldl2_pred(_, [], !AccA, !AccB).
leaf_foldl2_pred(P, [H | T], !AccA, !AccB) :-
    fold2_bits(low_to_high, P, H ^ leaf_offset, H ^ leaf_bits, bits_per_int,
        !AccA, !AccB),
    leaf_foldl2_pred(P, T, !AccA, !AccB).

foldr(F, Set, Acc0) = Acc :-
    P = (pred(E::in, PAcc0::in, PAcc::out) is det :-
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

foldr2(P, Set, !AccA, !AccB) :-
    Set = tree_bitset(List),
    (
        List = leaf_list(LeafNodes),
        leaf_foldr2_pred(P, LeafNodes, !AccA, !AccB)
    ;
        List = interior_list(_, InteriorNodes),
        do_foldr2_pred(P, InteriorNodes, !AccA, !AccB)
    ).

:- pred do_foldr_pred(pred(T, U, U), list(interior_node), U, U) <= enum(T).
:- mode do_foldr_pred(pred(in, di, uo) is det, in, di, uo) is det.
:- mode do_foldr_pred(pred(in, in, out) is det, in, in, out) is det.
:- mode do_foldr_pred(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode do_foldr_pred(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode do_foldr_pred(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode do_foldr_pred(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.

:- pragma type_spec(do_foldr_pred/4, T = int).
:- pragma type_spec(do_foldr_pred/4, T = var(_)).

    % We don't just use list.foldr here because the overhead of allocating
    % the closure for fold_bits is significant for the compiler's runtime,
    % so it's best to avoid that even if `--optimize-higher-order' is not set.
do_foldr_pred(_, [], !Acc).
do_foldr_pred(P, [H | T], !Acc) :-
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
:- mode leaf_foldr_pred(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.

:- pragma type_spec(leaf_foldr_pred/4, T = int).
:- pragma type_spec(leaf_foldr_pred/4, T = var(_)).

    % We don't just use list.foldr here because the overhead of allocating
    % the closure for fold_bits is significant for the compiler's runtime,
    % so it's best to avoid that even if `--optimize-higher-order' is not set.
leaf_foldr_pred(_, [], !Acc).
leaf_foldr_pred(P, [H | T], !Acc) :-
    leaf_foldr_pred(P, T, !Acc),
    fold_bits(high_to_low, P, H ^ leaf_offset, H ^ leaf_bits, bits_per_int,
        !Acc).

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

:- pragma type_spec(do_foldr2_pred/6, T = int).
:- pragma type_spec(do_foldr2_pred/6, T = var(_)).

    % We don't just use list.foldr here because the overhead of allocating
    % the closure for fold_bits is significant for the compiler's runtime,
    % so it's best to avoid that even if `--optimize-higher-order' is not set.
do_foldr2_pred(_, [], !AccA, !AccB).
do_foldr2_pred(P, [H | T], !AccA, !AccB) :-
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

:- pragma type_spec(leaf_foldr2_pred/6, T = int).
:- pragma type_spec(leaf_foldr2_pred/6, T = var(_)).

    % We don't just use list.foldr here because the overhead of allocating
    % the closure for fold_bits is significant for the compiler's runtime,
    % so it's best to avoid that even if `--optimize-higher-order' is not set.
leaf_foldr2_pred(_, [], !AccA, !AccB).
leaf_foldr2_pred(P, [H | T], !AccA, !AccB) :-
    leaf_foldr2_pred(P, T, !AccA, !AccB),
    fold2_bits(high_to_low, P, H ^ leaf_offset, H ^ leaf_bits, bits_per_int,
        !AccA, !AccB).

    % Do a binary search for the 1 bits in an int.
    %
:- pred fold_bits(fold_direction, pred(T, U, U),
    int, int, int, U, U) <= enum(T).
:- mode fold_bits(in, pred(in, in, out) is det,
    in, in, in, in, out) is det.
:- mode fold_bits(in, pred(in, di, uo) is det,
    in, in, in, di, uo) is det.
:- mode fold_bits(in, pred(in, in, out) is semidet,
    in, in, in, in, out) is semidet.
:- mode fold_bits(in, pred(in, in, out) is nondet,
    in, in, in, in, out) is nondet.
:- mode fold_bits(in, pred(in, di, uo) is cc_multi,
    in, in, in, di, uo) is cc_multi.
:- mode fold_bits(in, pred(in, in, out) is cc_multi,
    in, in, in, in, out) is cc_multi.
:- pragma type_spec(fold_bits/7, T = int).
:- pragma type_spec(fold_bits/7, T = var(_)).

fold_bits(Dir, P, Offset, Bits, Size, !Acc) :-
    ( Bits = 0 ->
        true
    ; Size = 1 ->
        Elem = index_to_enum(Offset),
        P(Elem, !Acc)
    ;
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
    int, int, int, U, U, V, V) <= enum(T).
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
:- pragma type_spec(fold2_bits/9, T = int).
:- pragma type_spec(fold2_bits/9, T = var(_)).

fold2_bits(Dir, P, Offset, Bits, Size, !AccA, !AccB) :-
    ( Bits = 0 ->
        true
    ; Size = 1 ->
        Elem = index_to_enum(Offset),
        P(Elem, !AccA, !AccB)
    ;
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
