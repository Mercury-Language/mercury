%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2007, 2011-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: sparse_bitset.m.
% Author: stayl.
% Stability: medium.
%
% This module provides an ADT for storing sets of integers.
% If the integers stored are closely grouped, a sparse_bitset
% is much more compact than the representation provided by set.m,
% and the operations will be much faster.
%
% Efficiency notes:
%
% A sparse bitset is represented as a sorted list of pairs of integers.
% For a pair `Offset - Bits', `Offset' is a multiple of `int.bits_per_int'.
% The bits of `Bits' describe which of the elements of the range
% `Offset' .. `Offset + bits_per_int - 1' are in the set.
% Pairs with the same value of `Offset' are merged.
% Pairs in which `Bits' is zero are removed.
%
% The values of `Offset' in the list need not be *contiguous* multiples
% of `bits_per_int', hence the name *sparse* bitset.
%
% A sparse_bitset is suitable for storing sets of integers which
% can be represented using only a few `Offset - Bits' pairs.
% In the worst case, where the integers stored are not closely grouped,
% a sparse_bitset will take more memory than an ordinary set, but
% the operations should not be too much slower.
%
% In the asymptotic complexities of the operations below,
% `rep_size(Set)' is the number of pairs needed to represent `Set',
% and `card(Set)' is the number of elements in `Set'.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module sparse_bitset.
:- interface.

:- import_module enum.
:- import_module list.
:- import_module term.

:- use_module set.

%---------------------------------------------------------------------------%

:- type sparse_bitset(T). % <= enum(T).

    % `equal(SetA, SetB' is true iff `SetA' and `SetB' contain the same
    % elements. Takes O(min(rep_size(SetA), rep_size(SetB))) time.
    %
:- pred equal(sparse_bitset(T)::in, sparse_bitset(T)::in) is semidet.

%---------------------------------------------------------------------------%

    % Return an empty set.
    %
:- func init = sparse_bitset(T).
:- pred init(sparse_bitset(T)::out) is det.

:- pred empty(sparse_bitset(T)).
:- mode empty(in) is semidet.
:- mode empty(out) is det.

:- pred is_empty(sparse_bitset(T)::in) is semidet.

:- pred is_non_empty(sparse_bitset(T)::in) is semidet.

%---------------------%

    % `make_singleton_set(Elem)' returns a set containing just the single
    % element `Elem'.
    %
:- func make_singleton_set(T) = sparse_bitset(T) <= enum(T).

    % Note: set.m contains the reverse mode of this predicate, but it is
    % difficult to implement both modes using the representation in this
    % module.
    %
:- pred singleton_set(sparse_bitset(T)::out, T::in) is det <= enum(T).

    % Is the given set a singleton, and if yes, what is the element?
    %
:- pred is_singleton(sparse_bitset(T)::in, T::out) is semidet <= enum(T).

%---------------------------------------------------------------------------%

    % `contains(Set, X)' is true iff `X' is a member of `Set'.
    % Takes O(rep_size(Set)) time.
    %
:- pred contains(sparse_bitset(T)::in, T::in) is semidet <= enum(T).

    % `member(X, Set)' is true iff `X' is a member of `Set'.
    % Takes O(rep_size(Set)) time.
    %
:- pred member(T, sparse_bitset(T)) <= enum(T).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

%---------------------------------------------------------------------------%

    % `insert(Set, X)' returns the union of `Set' and the set containing
    % only `X'. Takes O(rep_size(Set)) time and space.
    %
:- func insert(sparse_bitset(T), T) = sparse_bitset(T) <= enum(T).
:- pred insert(T::in, sparse_bitset(T)::in, sparse_bitset(T)::out)
    is det <= enum(T).

    % `insert_new(X, Set0, Set)' returns the union of `Set' and the set
    % containing only `X' if `Set0' does not already contain `X'; if it does,
    % it fails. Takes O(rep_size(Set)) time and space.
    %
:- pred insert_new(T::in, sparse_bitset(T)::in, sparse_bitset(T)::out)
    is semidet <= enum(T).

    % `insert_list(Set, X)' returns the union of `Set' and the set containing
    % only the members of `X'. Same as `union(Set, list_to_set(X))', but may be
    % more efficient.
    %
:- func insert_list(sparse_bitset(T), list(T)) = sparse_bitset(T) <= enum(T).
:- pred insert_list(list(T)::in, sparse_bitset(T)::in, sparse_bitset(T)::out)
    is det <= enum(T).

%---------------------%

    % `delete(Set, X)' returns the difference of `Set' and the set containing
    % only `X'. Takes O(rep_size(Set)) time and space.
    %
:- func delete(sparse_bitset(T), T) = sparse_bitset(T) <= enum(T).
:- pred delete(T::in, sparse_bitset(T)::in, sparse_bitset(T)::out)
    is det <= enum(T).

    % `delete_list(Set, X)' returns the difference of `Set' and the set
    % containing only the members of `X'. Same as
    % `difference(Set, list_to_set(X))', but may be more efficient.
    %
:- func delete_list(sparse_bitset(T), list(T)) = sparse_bitset(T) <= enum(T).
:- pred delete_list(list(T)::in, sparse_bitset(T)::in, sparse_bitset(T)::out)
    is det <= enum(T).

    % `remove(X, Set0, Set)' returns in `Set' the difference of `Set0'
    % and the set containing only `X', failing if `Set0' does not contain `X'.
    % Takes O(rep_size(Set)) time and space.
    %
:- pred remove(T::in, sparse_bitset(T)::in, sparse_bitset(T)::out)
    is semidet <= enum(T).

    % `remove_list(X, Set0, Set)' returns in `Set' the difference of `Set0'
    % and the set containing all the elements of `X', failing if any element
    % of `X' is not in `Set0'. Same as `subset(list_to_set(X), Set0),
    % difference(Set0, list_to_set(X), Set)', but may be more efficient.
    %
:- pred remove_list(list(T)::in, sparse_bitset(T)::in, sparse_bitset(T)::out)
    is semidet <= enum(T).

    % `remove_leq(Set, X)' returns `Set' with all elements less than or equal
    % to `X' removed. In other words, it returns the set containing all the
    % elements of `Set' which are greater than `X'.
    %
:- func remove_leq(sparse_bitset(T), T) = sparse_bitset(T) <= enum(T).
:- pred remove_leq(T::in, sparse_bitset(T)::in, sparse_bitset(T)::out)
    is det <= enum(T).

    % `remove_gt(Set, X)' returns `Set' with all elements greater than `X'
    % removed. In other words, it returns the set containing all the elements
    % of `Set' which are less than or equal to `X'.
    %
:- func remove_gt(sparse_bitset(T), T) = sparse_bitset(T) <= enum(T).
:- pred remove_gt(T::in, sparse_bitset(T)::in, sparse_bitset(T)::out)
    is det <= enum(T).

    % `remove_least(Set0, X, Set)' is true iff `X' is the least element in
    % `Set0', and `Set' is the set which contains all the elements of `Set0'
    % except `X'. Takes O(1) time and space.
    %
:- pred remove_least(T::out, sparse_bitset(T)::in, sparse_bitset(T)::out)
    is semidet <= enum(T).

%---------------------------------------------------------------------------%

    % `list_to_set(List)' returns a set containing only the members of `List'.
    % In the worst case this will take O(length(List)^2) time and space.
    % If the elements of the list are closely grouped, it will be closer
    % to O(length(List)).
    %
:- func list_to_set(list(T)) = sparse_bitset(T) <= enum(T).
:- pred list_to_set(list(T)::in, sparse_bitset(T)::out) is det <= enum(T).

    % `sorted_list_to_set(List)' returns a set containing only the members
    % of `List'. `List' must be sorted. Takes O(length(List)) time and space.
    %
:- func sorted_list_to_set(list(T)) = sparse_bitset(T) <= enum(T).
:- pred sorted_list_to_set(list(T)::in, sparse_bitset(T)::out)
    is det <= enum(T).

    % `to_sorted_list(Set)' returns a list containing all the members of `Set',
    % in sorted order. Takes O(card(Set)) time and space.
    %
:- func to_sorted_list(sparse_bitset(T)) = list(T) <= enum(T).
:- pred to_sorted_list(sparse_bitset(T)::in, list(T)::out) is det <= enum(T).

%---------------------%

    % `from_set(Set)' returns a bitset containing only the members of `Set'.
    % Takes O(card(Set)) time and space.
    %
:- func from_set(set.set(T)) = sparse_bitset(T) <= enum(T).

    % `to_sorted_list(Set)' returns a set.set containing all the members
    % of `Set', in sorted order. Takes O(card(Set)) time and space.
    %
:- func to_set(sparse_bitset(T)) = set.set(T) <= enum(T).

%---------------------------------------------------------------------------%

    % `count(Set)' returns the number of elements in `Set'.
    % Takes O(card(Set)) time.
    %
:- func count(sparse_bitset(T)) = int <= enum(T).

    % `foldl(Func, Set, Start)' calls Func with each element of `Set'
    % (in sorted order) and an accumulator (with the initial value of `Start'),
    % and returns the final value. Takes O(card(Set)) time.
    %
:- func foldl(func(T, U) = U, sparse_bitset(T), U) = U <= enum(T).

:- pred foldl(pred(T, U, U), sparse_bitset(T), U, U) <= enum(T).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode foldl(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode foldl(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.
:- mode foldl(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.

:- pred foldl2(pred(T, U, U, V, V), sparse_bitset(T), U, U, V, V) <= enum(T).
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

    % `foldr(Func, Set, Start)' calls Func with each element of `Set'
    % (in reverse sorted order) and an accumulator (with the initial value
    % of `Start'), and returns the final value. Takes O(card(Set)) time.
    %
:- func foldr(func(T, U) = U, sparse_bitset(T), U) = U <= enum(T).

:- pred foldr(pred(T, U, U), sparse_bitset(T), U, U) <= enum(T).
:- mode foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode foldr(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldr(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldr(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldr(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode foldr(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode foldr(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.
:- mode foldr(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.

:- pred foldr2(pred(T, U, U, V, V), sparse_bitset(T), U, U, V, V) <= enum(T).
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

    % all_true(Pred, Set) succeeds iff Pred(Element) succeeds
    % for all the elements of Set.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet), sparse_bitset(T)::in)
    is semidet <= enum(T).

    % `filter(Pred, Set) = TrueSet' returns the elements of Set for which
    % Pred succeeds.
    %
:- func filter(pred(T), sparse_bitset(T)) = sparse_bitset(T) <= enum(T).
:- mode filter(pred(in) is semidet, in) = out is det.

    % `filter(Pred, Set, TrueSet, FalseSet)' returns the elements of Set
    % for which Pred succeeds, and those for which it fails.
    %
:- pred filter(pred(T), sparse_bitset(T), sparse_bitset(T), sparse_bitset(T))
    <= enum(T).
:- mode filter(pred(in) is semidet, in, out, out) is det.

%---------------------------------------------------------------------------%

    % `subset(Subset, Set)' is true iff `Subset' is a subset of `Set'.
    % Same as `intersect(Set, Subset, Subset)', but may be more efficient.
    %
:- pred subset(sparse_bitset(T)::in, sparse_bitset(T)::in) is semidet.

    % `superset(Superset, Set)' is true iff `Superset' is a superset of `Set'.
    % Same as `intersect(Superset, Set, Set)', but may be more efficient.
    %
:- pred superset(sparse_bitset(T)::in, sparse_bitset(T)::in) is semidet.

%---------------------------------------------------------------------------%

    % `union(SetA, SetB)' returns the union of `SetA' and `SetB'. The
    % efficiency of the union operation is not sensitive to the argument
    % ordering. Takes O(rep_size(SetA) + rep_size(SetB)) time and space.
    %
:- func union(sparse_bitset(T), sparse_bitset(T)) = sparse_bitset(T).
:- pred union(sparse_bitset(T)::in, sparse_bitset(T)::in,
    sparse_bitset(T)::out) is det.

    % `union_list(Sets, Set)' returns the union of all the sets in Sets.
    %
:- func union_list(list(sparse_bitset(T))) = sparse_bitset(T).
:- pred union_list(list(sparse_bitset(T))::in, sparse_bitset(T)::out) is det.

    % `intersect(SetA, SetB)' returns the intersection of `SetA' and `SetB'.
    % The efficiency of the intersection operation is not sensitive to the
    % argument ordering. Takes O(rep_size(SetA) + rep_size(SetB)) time and
    % O(min(rep_size(SetA)), rep_size(SetB)) space.
    %
:- func intersect(sparse_bitset(T), sparse_bitset(T)) = sparse_bitset(T).
:- pred intersect(sparse_bitset(T)::in, sparse_bitset(T)::in,
    sparse_bitset(T)::out) is det.

    % `intersect_list(Sets, Set)' returns the intersection of all the sets
    % in Sets.
    %
:- func intersect_list(list(sparse_bitset(T))) = sparse_bitset(T).
:- pred intersect_list(list(sparse_bitset(T))::in, sparse_bitset(T)::out)
    is det.

    % `difference(SetA, SetB)' returns the set containing all the elements
    % of `SetA' except those that occur in `SetB'. Takes
    % O(rep_size(SetA) + rep_size(SetB)) time and O(rep_size(SetA)) space.
    %
:- func difference(sparse_bitset(T), sparse_bitset(T)) = sparse_bitset(T).
:- pred difference(sparse_bitset(T)::in, sparse_bitset(T)::in,
    sparse_bitset(T)::out) is det.

    % divide(Pred, Set, InPart, OutPart):
    % InPart consists of those elements of Set for which Pred succeeds;
    % OutPart consists of those elements of Set for which Pred fails.
    %
:- pred divide(pred(T)::in(pred(in) is semidet), sparse_bitset(T)::in,
    sparse_bitset(T)::out, sparse_bitset(T)::out) is det <= enum(T).

    % divide_by_set(DivideBySet, Set, InPart, OutPart):
    % InPart consists of those elements of Set which are also in DivideBySet;
    % OutPart consists of those elements of Set which are not in DivideBySet.
    %
:- pred divide_by_set(sparse_bitset(T)::in, sparse_bitset(T)::in,
    sparse_bitset(T)::out, sparse_bitset(T)::out) is det <= enum(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module uint.

%---------------------------------------------------------------------------%

    % The number of variables for most procedures
    % should fit into one or two words.
:- type sparse_bitset(T)    % <= enum(T)
    --->    sparse_bitset(bitset_impl).

    % The list of elements, sorted on offset.
    % No two elements have the same offset.
:- type bitset_impl == list(bitset_elem).

    % Cells of this type should only be constructed using make_bitset_elem/2.
:- type bitset_elem
    --->    bitset_elem(
                % This must be a multiple of bits_per_int.
                offset  :: int,

                % bits offset .. offset + bits_per_int - 1
                % All sparse_bitset operations should remove all
                % elements of the list with a `bits' field of zero.
                bits    :: uint
            ).

%---------------------------------------------------------------------------%

equal(X, X).

%---------------------------------------------------------------------------%

init = sparse_bitset([]).

init(sparse_bitset([])).

empty(sparse_bitset([])).

is_empty(sparse_bitset([])).

is_non_empty(sparse_bitset([_ | _])).

%---------------------------------------------------------------------------%

make_singleton_set(A) = insert(init, A).

singleton_set(make_singleton_set(A), A).

is_singleton(sparse_bitset([Node]), Elem) :-
    Node = bitset_elem(Offset, Bits),
    count_bits(Offset, bits_per_int, Bits, [], SetOffsets),
    SetOffsets = [SetOffset],
    ( if ElemPrime = from_int(SetOffset) then
        Elem = ElemPrime
    else
        % We only apply `from_int/1' to integers returned
        % by `to_int/1', so it should never fail.
        unexpected($module, $pred, "`enum.from_int/1' failed")
    ).

    % Do a binary search for the 1 bits in an int.
    %
:- pred count_bits(int::in, int::in, uint::in,
    list(int)::in, list(int)::out) is det.

count_bits(BitOffset, Size, Bits, !SetOffsets) :-
    ( if Bits = 0u then
        true
    else if Size = 1 then
        % If Bits were 0, we wouldn't have got here.
        !:SetOffsets = [BitOffset | !.SetOffsets]
    else
        HalfSize = unchecked_right_shift(Size, 1),
        Mask = mask(HalfSize),

        % Extract the low-order half of the bits.
        LowBits = Mask /\ Bits,

        % Extract the high-order half of the bits.
        HighBits = Mask /\ unchecked_right_shift(Bits, HalfSize),

        count_bits(BitOffset, HalfSize, LowBits, !SetOffsets),
        count_bits(BitOffset + HalfSize, HalfSize, HighBits, !SetOffsets)
    ).

%---------------------------------------------------------------------------%

contains(sparse_bitset(Set), Elem) :-
    contains_search_nodes(Set, enum.to_int(Elem)).

:- pred contains_search_nodes(bitset_impl::in, int::in) is semidet.

contains_search_nodes([Data | Rest], Index) :-
    Offset = Data ^ offset,
    Index >= Offset,
    ( if Index < Offset + bits_per_int then
        get_bit(Data ^ bits, Index - Offset) \= 0u
    else
        contains_search_nodes(Rest, Index)
    ).

%---------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(member/2).

member(Elem::in, Set::in) :-
    contains(Set, Elem).
member(Elem::out, sparse_bitset(Set)::in) :-
    member_search_nodes(Index, Set),
    ( if Elem0 = from_int(Index) then
        Elem = Elem0
    else
        % We only apply `from_int/1' to integers returned
        % by `to_int/1', so it should never fail.
        unexpected($module, $pred, "`enum.from_int/1' failed")
    ).

:- pred member_search_nodes(int::out, bitset_impl::in) is nondet.

member_search_nodes(Index, [Elem | Elems]) :-
    ( member_search_one_node(Index, Elem ^ offset, bits_per_int, Elem ^ bits)
    ; member_search_nodes(Index, Elems)
    ).

:- pred member_search_one_node(int::out, int::in, int::in, uint::in) is nondet.

member_search_one_node(Index, Offset, Size, Bits) :-
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

        ( member_search_one_node(Index, Offset, HalfSize, LowBits)
        ; member_search_one_node(Index, Offset + HalfSize, HalfSize, HighBits)
        )
    ).

%---------------------------------------------------------------------------%

insert(Set0, Elem) = Set :-
    insert(Elem, Set0, Set).

insert(E, !Set) :-
    !.Set = sparse_bitset(Set0),
    insert_loop(enum.to_int(E), Set0, Set),
    !:Set = sparse_bitset(Set).

:- pred insert_loop(int::in, bitset_impl::in, bitset_impl::out) is det.

insert_loop(Index, Set0, Set) :-
    (
        Set0 = [],
        bits_for_index(Index, Offset, Bits),
        Set = [make_bitset_elem(Offset, Bits)]
    ;
        Set0 = [Data0 | SetTail0],
        Offset0 = Data0 ^ offset,
        ( if Index < Offset0 then
            bits_for_index(Index, Offset, Bits),
            Set = [make_bitset_elem(Offset, Bits) | Set0]
        else if BitToSet = Index - Offset0, BitToSet < bits_per_int then
            Bits0 = Data0 ^ bits,
            ( if get_bit(Bits0, BitToSet) \= 0u then
                Set = Set0
            else
                Bits = set_bit(Bits0, BitToSet),
                Set = [make_bitset_elem(Offset0, Bits) | SetTail0]
            )
        else
            insert_loop(Index, SetTail0, SetTail),
            Set = [Data0 | SetTail]
        )
    ).

%---------------------------------------------------------------------------%

insert_new(E, !Set) :-
    !.Set = sparse_bitset(Set0),
    insert_new_loop(enum.to_int(E), Set0, Set),
    !:Set = sparse_bitset(Set).

:- pred insert_new_loop(int::in, bitset_impl::in, bitset_impl::out)
    is semidet.

insert_new_loop(Index, Set0, Set) :-
    (
        Set0 = [],
        bits_for_index(Index, Offset, Bits),
        Set = [make_bitset_elem(Offset, Bits)]
    ;
        Set0 = [Data0 | SetTail0],
        Offset0 = Data0 ^ offset,
        ( if Index < Offset0 then
            bits_for_index(Index, Offset, Bits),
            Set = [make_bitset_elem(Offset, Bits) | Set0]
        else if BitToSet = Index - Offset0, BitToSet < bits_per_int then
            Bits0 = Data0 ^ bits,
            ( if get_bit(Bits0, BitToSet) \= 0u then
                fail
            else
                Bits = set_bit(Bits0, BitToSet),
                Set = [make_bitset_elem(Offset0, Bits) | SetTail0]
            )
        else
            insert_new_loop(Index, SetTail0, SetTail),
            Set = [Data0 | SetTail]
        )
    ).

%---------------------------------------------------------------------------%

insert_list(Set0, List) = Set :-
    insert_list(List, Set0, Set).

insert_list(List, Set0, Set) :-
    union(list_to_set(List), Set0, Set).

%---------------------------------------------------------------------------%

delete(Set, Elem) = difference(Set, insert(init, Elem)).

delete(E, !Set) :-
    !:Set = delete(!.Set, E).

delete_list(Set, List) = difference(Set, list_to_set(List)).

delete_list(List, !Set) :-
    !:Set = delete_list(!.Set, List).

%---------------------------------------------------------------------------%

remove(Elem, !Set) :-
    contains(!.Set, Elem),
    !:Set = delete(!.Set, Elem).

remove_list(Elems, !Set) :-
    list_to_set(Elems, ElemsSet),
    subset(ElemsSet, !.Set),
    !:Set = difference(!.Set, ElemsSet).

%---------------------------------------------------------------------------%

remove_leq(sparse_bitset(Set), Elem) =
    sparse_bitset(remove_leq_2(Set, enum.to_int(Elem))).

remove_leq(E, !Set) :-
    !:Set = remove_leq(!.Set, E).

:- func remove_leq_2(bitset_impl, int) = bitset_impl.

remove_leq_2([], _) = [].
remove_leq_2([Data | Rest], Index) = Result :-
    Offset = Data ^ offset,
    ( if Offset + bits_per_int =< Index then
        Result = remove_leq_2(Rest, Index)
    else if Offset =< Index then
        ( if
            Bits = Data ^ bits /\
                unchecked_left_shift(\ 0u, Index - Offset + 1),
            Bits \= 0u
        then
            Result = [make_bitset_elem(Offset, Bits) | Rest]
        else
            Result = Rest
        )
    else
        Result = [Data | Rest]
    ).

%---------------------------------------------------------------------------%

remove_gt(sparse_bitset(Set), Elem) =
    sparse_bitset(remove_gt_2(Set, enum.to_int(Elem))).

remove_gt(E, !Set) :-
    !:Set = remove_gt(!.Set, E).

:- func remove_gt_2(bitset_impl, int) = bitset_impl.

remove_gt_2([], _) = [].
remove_gt_2([Data | Rest], Index) = Result :-
    Offset = Data ^ offset,
    ( if Offset + bits_per_int - 1 =< Index then
        Result = [Data | remove_gt_2(Rest, Index)]
    else if Offset =< Index then
        ( if
            Bits = Data ^ bits /\
                \ unchecked_left_shift(\ 0u, Index - Offset + 1),
            Bits \= 0u
        then
            Result = [make_bitset_elem(Offset, Bits)]
        else
            Result = []
        )
    else
        Result = []
    ).

%---------------------------------------------------------------------------%

remove_least(Elem, sparse_bitset(Set0), sparse_bitset(Set)) :-
    Set0 = [First | Rest],
    Bits0 = First ^ bits,
    Offset = First ^ offset,
    Bit = find_least_bit(Bits0),
    ( if Elem0 = from_int(Offset + Bit) then
        Elem = Elem0
    else
        % We only apply `from_int/1' to integers returned
        % by `to_int/1', so it should never fail.
        unexpected($module, $pred, "`enum.from_int/1' failed")
    ),
    Bits = clear_bit(Bits0, Bit),
    ( if Bits = 0u then
        Set = Rest
    else
        Set = [make_bitset_elem(Offset, Bits) | Rest]
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
        ( if LowBits \= 0u then
            BitNum = find_least_bit_2(LowBits, HalfSize, BitNum0)
        else
            HighBits = Mask /\ unchecked_right_shift(Bits0, HalfSize),
            BitNum = find_least_bit_2(HighBits, HalfSize, BitNum0 + HalfSize)
        )
    ).

%---------------------------------------------------------------------------%

list_to_set(List) =
    sparse_bitset(list_to_set_2(List, [])).

list_to_set(A, list_to_set(A)).

    % Each pass over the input list selects out the elements which belong
    % in the same bitset_elem as the first element. The assumption here is that
    % the items in the input list will have similar values, so that only a few
    % passes will be needed.
    %
:- func list_to_set_2(list(T), bitset_impl) = bitset_impl <= enum(T).
:- pragma type_spec(list_to_set_2/2, T = var(_)).
:- pragma type_spec(list_to_set_2/2, T = int).

list_to_set_2([], List) = List.
list_to_set_2([H | T], List0) = List :-
    bits_for_index(enum.to_int(H), Offset, Bits0),
    list_to_set_3(T, Offset, Bits0, Bits, [], Rest),
    List1 = insert_bitset_elem(make_bitset_elem(Offset, Bits), List0),
    List = list_to_set_2(Rest, List1).

    % Go through the list picking out the elements which belong in the same
    % bitset_elem as the first element, returning the uncollected elements.
    %
:- pred list_to_set_3(list(T)::in, int::in, uint::in, uint::out,
    list(T)::in, list(T)::out) is det <= enum(T).
:- pragma type_spec(list_to_set_3/6, T = var(_)).
:- pragma type_spec(list_to_set_3/6, T = int).

list_to_set_3([], _, !Bits, !Rest).
list_to_set_3([H | T], Offset, !Bits, !Rest) :-
    BitToSet = enum.to_int(H) - Offset,
    ( if BitToSet >= 0, BitToSet < bits_per_int then
        !:Bits = set_bit(!.Bits, BitToSet)
    else
        !:Rest = [H | !.Rest]
    ),
    list_to_set_3(T, Offset, !Bits, !Rest).

    % The list of elements here is pretty much guaranteed to be small,
    % so use an insertion sort.
    %
:- func insert_bitset_elem(bitset_elem, bitset_impl) = bitset_impl.

insert_bitset_elem(Data, []) = [Data].
insert_bitset_elem(Data, [Head | Tail]) = List :-
    ( if Data ^ offset < Head ^ offset then
        List = [Data, Head | Tail]
    else
        List = [Head | insert_bitset_elem(Data, Tail)]
    ).

%---------------------------------------------------------------------------%

sorted_list_to_set(L) = sparse_bitset(sorted_list_to_set_2(L)).

sorted_list_to_set(A, sorted_list_to_set(A)).

:- func sorted_list_to_set_2(list(T)) = bitset_impl <= enum(T).
:- pragma type_spec(sorted_list_to_set_2/1, T = var(_)).
:- pragma type_spec(sorted_list_to_set_2/1, T = int).

sorted_list_to_set_2([]) = [].
sorted_list_to_set_2([H | T]) = Set :-
    sorted_list_to_set_3(H, T, Offset, Bits, Set0),
    ( if Bits = 0u then
        Set = Set0
    else
        Set = [make_bitset_elem(Offset, Bits) | Set0]
    ).

:- pred sorted_list_to_set_3(T::in, list(T)::in, int::out, uint::out,
    bitset_impl::out) is det <= enum(T).
:- pragma type_spec(sorted_list_to_set_3/5, T = var(_)).
:- pragma type_spec(sorted_list_to_set_3/5, T = int).

sorted_list_to_set_3(Elem, [], Offset, Bits, []) :-
    bits_for_index(enum.to_int(Elem), Offset, Bits).
sorted_list_to_set_3(Elem1, [Elem2 | Elems], Offset, Bits, Rest) :-
    sorted_list_to_set_3(Elem2, Elems, Offset0, Bits0, Rest0),
    bits_for_index(enum.to_int(Elem1), Offset1, Bits1),
    ( if Offset1 = Offset0 then
        Bits = Bits1 \/ Bits0,
        Offset = Offset1,
        Rest = Rest0
    else
        Rest = [make_bitset_elem(Offset0, Bits0) | Rest0],
        Offset = Offset1,
        Bits = Bits1
    ).

%---------------------------------------------------------------------------%

to_sorted_list(Set) = foldr(func(Elem, Acc0) = [Elem | Acc0], Set, []).

to_sorted_list(A, to_sorted_list(A)).

%---------------------------------------------------------------------------%

from_set(Set) = sorted_list_to_set(set.to_sorted_list(Set)).

to_set(Set) = set.sorted_list_to_set(to_sorted_list(Set)).

%---------------------------------------------------------------------------%

count(Set) = sparse_bitset.foldl((func(_, Acc) = Acc + 1), Set, 0).

%---------------------------------------------------------------------------%

:- type fold_direction
    --->    low_to_high
    ;       high_to_low.

foldl(F, sparse_bitset(Set), Acc0) = Acc :-
    do_foldl_pred(
        ( pred(E::in, Acc1::in, Acc2::out) is det :-
            Acc2 = F(E, Acc1)
        ), Set, Acc0, Acc).

foldl(P, sparse_bitset(Set), !Acc) :-
    do_foldl_pred(P, Set, !Acc).

foldl2(P, sparse_bitset(Set), !Acc1, !Acc2) :-
    do_foldl2_pred(P, Set, !Acc1, !Acc2).

:- pred do_foldl_pred(pred(T, U, U), bitset_impl, U, U) <= enum(T).
:- mode do_foldl_pred(pred(in, in, out) is det, in, in, out) is det.
:- mode do_foldl_pred(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode do_foldl_pred(pred(in, di, uo) is det, in, di, uo) is det.
:- mode do_foldl_pred(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode do_foldl_pred(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode do_foldl_pred(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode do_foldl_pred(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode do_foldl_pred(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.
:- mode do_foldl_pred(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.

:- pragma type_spec(do_foldl_pred/4, T = int).
:- pragma type_spec(do_foldl_pred/4, T = var(_)).

do_foldl_pred(_, [], !Acc).
do_foldl_pred(P, [H | T], !Acc) :-
    fold_bits(low_to_high, P, H ^ offset, H ^ bits, bits_per_int, !Acc),
    do_foldl_pred(P, T, !Acc).

:- pred do_foldl2_pred(pred(T, U, U, V, V), bitset_impl, U, U, V, V)
    <= enum(T).
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

:- pragma type_spec(do_foldl2_pred/6, T = int).
:- pragma type_spec(do_foldl2_pred/6, T = var(_)).

do_foldl2_pred(_, [], !Acc1, !Acc2).
do_foldl2_pred(P, [H | T], !Acc1, !Acc2) :-
    fold2_bits(low_to_high, P, H ^ offset, H ^ bits, bits_per_int,
        !Acc1, !Acc2),
    do_foldl2_pred(P, T, !Acc1, !Acc2).

foldr(F, sparse_bitset(Set), Acc0) = Acc :-
    do_foldr_pred(
        ( pred(E::in, Acc1::in, Acc2::out) is det :-
            Acc2 = F(E, Acc1)
        ), Set, Acc0, Acc).

foldr(P, sparse_bitset(Set), !Acc) :-
    do_foldr_pred(P, Set, !Acc).

foldr2(P, sparse_bitset(Set), !Acc1, !Acc2) :-
    do_foldr2_pred(P, Set, !Acc1, !Acc2).

:- pred do_foldr_pred(pred(T, U, U), bitset_impl, U, U) <= enum(T).
:- mode do_foldr_pred(pred(in, in, out) is det, in, in, out) is det.
:- mode do_foldr_pred(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode do_foldr_pred(pred(in, di, uo) is det, in, di, uo) is det.
:- mode do_foldr_pred(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode do_foldr_pred(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode do_foldr_pred(pred(in, di, uo) is semidet, in, di, uo) is semidet.
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
    fold_bits(high_to_low, P, H ^ offset, H ^ bits, bits_per_int, !Acc).

:- pred do_foldr2_pred(pred(T, U, U, V, V), bitset_impl, U, U, V, V)
    <= enum(T).
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

:- pragma type_spec(do_foldr2_pred/6, T = int).
:- pragma type_spec(do_foldr2_pred/6, T = var(_)).

    % We don't just use list.foldr here because the overhead of allocating
    % the closure for fold_bits is significant for the compiler's runtime,
    % so it's best to avoid that even if `--optimize-higher-order' is not set.
do_foldr2_pred(_, [], !Acc1, !Acc2).
do_foldr2_pred(P, [H | T], !Acc1, !Acc2) :-
    do_foldr2_pred(P, T, !Acc1, !Acc2),
    fold2_bits(high_to_low, P, H ^ offset, H ^ bits, bits_per_int,
        !Acc1, !Acc2).

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
:- mode fold_bits(in, pred(in, di, uo) is cc_multi,
    in, in, in, di, uo) is cc_multi.
:- mode fold_bits(in, pred(in, in, out) is cc_multi,
    in, in, in, in, out) is cc_multi.
:- pragma type_spec(fold_bits/7, T = int).
:- pragma type_spec(fold_bits/7, T = var(_)).

fold_bits(Dir, P, Offset, Bits, Size, !Acc) :-
    ( if Bits = 0u then
        true
    else if Size = 1 then
        ( if Elem = from_int(Offset) then
            P(Elem, !Acc)
        else
            % We only apply `from_int/1' to integers returned
            % by `to_int/1', so it should never fail.
            unexpected($module, $pred, "`enum.from_int/1' failed")
        )
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
:- mode fold2_bits(in, pred(in, in, out, in, out) is det,
    in, in, in, in, out, in, out) is det.
:- mode fold2_bits(in, pred(in, in, out, mdi, muo) is det,
    in, in, in, in, out, mdi, muo) is det.
:- mode fold2_bits(in, pred(in, di, uo, di, uo) is det,
    in, in, in, di, uo, di, uo) is det.
:- mode fold2_bits(in, pred(in, in, out, di, uo) is det,
    in, in, in, in, out, di, uo) is det.
:- mode fold2_bits(in, pred(in, in, out, in, out) is semidet,
    in, in, in, in, out, in, out) is semidet.
:- mode fold2_bits(in, pred(in, in, out, mdi, muo) is semidet,
    in, in, in, in, out, mdi, muo) is semidet.
:- mode fold2_bits(in, pred(in, in, out, di, uo) is semidet,
    in, in, in, in, out, di, uo) is semidet.
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

fold2_bits(Dir, P, Offset, Bits, Size, !Acc1, !Acc2) :-
    ( if Bits = 0u then
        true
    else if Size = 1 then
        ( if Elem = from_int(Offset) then
            P(Elem, !Acc1, !Acc2)
        else
            % We only apply `from_int/1' to integers returned
            % by `to_int/1', so it should never fail.
            unexpected($module, $pred, "`enum.from_int/1' failed")
        )
    else
        HalfSize = unchecked_right_shift(Size, 1),
        Mask = mask(HalfSize),

        % Extract the low-order half of the bits.
        LowBits = Mask /\ Bits,

        % Extract the high-order half of the bits.
        HighBits = Mask /\ unchecked_right_shift(Bits, HalfSize),

        (
            Dir = low_to_high,
            fold2_bits(Dir, P, Offset, LowBits, HalfSize, !Acc1, !Acc2),
            fold2_bits(Dir, P, Offset + HalfSize, HighBits, HalfSize,
                !Acc1, !Acc2)
        ;
            Dir = high_to_low,
            fold2_bits(Dir, P, Offset + HalfSize, HighBits, HalfSize,
                !Acc1, !Acc2),
            fold2_bits(Dir, P, Offset, LowBits, HalfSize, !Acc1, !Acc2)
        )
    ).

%---------------------------------------------------------------------------%

all_true(P, sparse_bitset(Set)) :-
    all_true_node(P, Set).

:- pred all_true_node(pred(T)::in(pred(in) is semidet), bitset_impl::in)
    is semidet <= enum(T).
:- pragma type_spec(all_true_node/2, T = int).
:- pragma type_spec(all_true_node/2, T = var(_)).

all_true_node(_, []).
all_true_node(P, [bitset_elem(Offset, Bits) | Rest]) :-
    all_true_bits(P, Offset, Bits, bits_per_int),
    all_true_node(P, Rest).

:- pred all_true_bits(pred(T)::in(pred(in) is semidet),
    int::in, uint::in, int::in) is semidet <= enum(T).
:- pragma type_spec(all_true_bits/4, T = int).
:- pragma type_spec(all_true_bits/4, T = var(_)).

all_true_bits(P, Offset, Bits, Size) :-
    ( if Bits = 0u then
        true
    else if Size = 1 then
        ( if Elem = from_int(Offset) then
            P(Elem)
        else
            % We only apply `from_int/1' to integers returned
            % by `to_int/1', so it should never fail.
            unexpected($module, $pred, "`enum.from_int/1' failed")
        )
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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

subset(Subset, Set) :-
    intersect(Set, Subset, Subset).

superset(Superset, Set) :-
    subset(Set, Superset).

%---------------------------------------------------------------------------%

union(sparse_bitset(Set1), sparse_bitset(Set2)) =
    sparse_bitset(union_2(Set1, Set2)).

union(A, B, union(A, B)).

:- func union_2(bitset_impl, bitset_impl) = bitset_impl.

union_2([], []) = [].
union_2([], B) = B :-
    B = [_ | _].
union_2(A, []) = A :-
    A = [_ | _].
union_2(Set1, Set2) = Set :-
    Set1 = [Data1 | SetTail1],
    Set2 = [Data2 | SetTail2],
    Offset1 = Data1 ^ offset,
    Offset2 = Data2 ^ offset,
    ( if Offset1 = Offset2 then
        Elem = make_bitset_elem(Offset1, (Data1 ^ bits) \/ (Data2 ^ bits)),
        Set = [Elem | union_2(SetTail1, SetTail2)]
    else if Offset1 < Offset2 then
        Set = [Data1 | union_2(SetTail1, Set2)]
    else
        Set = [Data2 | union_2(Set1, SetTail2)]
    ).

union_list(Sets) = Set :-
    union_list(Sets, Set).

union_list([], sparse_bitset.init).
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
:- pred union_list_pass(list(sparse_bitset(T))::in,
    list(sparse_bitset(T))::in, list(sparse_bitset(T))::out)
    is det.

union_list_pass([], !MergedSets).
union_list_pass([Set], !MergedSets) :-
    !:MergedSets = [Set | !.MergedSets].
union_list_pass([SetA, SetB | Sets0], !MergedSets) :-
    union(SetA, SetB, SetAB),
    !:MergedSets = [SetAB | !.MergedSets],
    union_list_pass(Sets0, !MergedSets).

%---------------------------------------------------------------------------%

intersect(sparse_bitset(Set1), sparse_bitset(Set2)) =
    sparse_bitset(intersect_2(Set1, Set2)).

intersect(A, B, intersect(A, B)).

:- func intersect_2(bitset_impl, bitset_impl) = bitset_impl.

intersect_2([], []) = [].
intersect_2([], B) = [] :-
    B = [_ | _].
intersect_2(A, []) = [] :-
    A = [_ | _].
intersect_2(Set1, Set2) = Set :-
    Set1 = [Data1 | SetTail1],
    Set2 = [Data2 | SetTail2],
    Offset1 = Data1 ^ offset,
    Offset2 = Data2 ^ offset,
    ( if Offset1 = Offset2 then
        Bits = Data1 ^ bits /\ Data2 ^ bits,
        ( if Bits = 0u then
            Set = intersect_2(SetTail1, SetTail2)
        else
            Set = [make_bitset_elem(Offset1, Bits) |
                intersect_2(SetTail1, SetTail2)]
        )
    else if Offset1 < Offset2 then
        Set = intersect_2(SetTail1, Set2)
    else
        Set = intersect_2(Set1, SetTail2)
    ).

intersect_list(Sets) = Set :-
    intersect_list(Sets, Set).

intersect_list([], sparse_bitset.init).
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
:- pred intersect_list_pass(list(sparse_bitset(T))::in,
    list(sparse_bitset(T))::in, list(sparse_bitset(T))::out) is det.

intersect_list_pass([], !MergedSets).
intersect_list_pass([Set], !MergedSets) :-
    !:MergedSets = [Set | !.MergedSets].
intersect_list_pass([SetA, SetB | Sets0], !MergedSets) :-
    intersect(SetA, SetB, SetAB),
    !:MergedSets = [SetAB | !.MergedSets],
    intersect_list_pass(Sets0, !MergedSets).

%---------------------------------------------------------------------------%

difference(sparse_bitset(Set1), sparse_bitset(Set2)) =
    sparse_bitset(difference_2(Set1, Set2)).

difference(A, B, difference(A, B)).

:- func difference_2(bitset_impl, bitset_impl) = bitset_impl.

difference_2([], []) = [].
difference_2([], B) = [] :-
    B = [_ | _].
difference_2(A, []) = A :-
    A = [_ | _].
difference_2(Set1, Set2) = Set :-
    Set1 = [Data1 | SetTail1],
    Set2 = [Data2 | SetTail2],
    Offset1 = Data1 ^ offset,
    Offset2 = Data2 ^ offset,
    ( if Offset1 = Offset2 then
        Bits = (Data1 ^ bits) /\ \ (Data2 ^ bits),
        ( if Bits = 0u then
            Set = difference_2(SetTail1, SetTail2)
        else
            Set = [make_bitset_elem(Offset1, Bits) |
                difference_2(SetTail1, SetTail2)]
        )
    else if Offset1 < Offset2 then
        Set = [Data1 | difference_2(SetTail1, Set2)]
    else
        Set = difference_2(Set1, SetTail2)
    ).

%---------------------------------------------------------------------------%

divide(Pred, Set, InSet, OutSet) :-
    Set = sparse_bitset(Nodes),
    divide_nodes(Pred, Nodes, InNodes, OutNodes),
    InSet = sparse_bitset(InNodes),
    OutSet = sparse_bitset(OutNodes).

:- pred divide_nodes(pred(T)::in(pred(in) is semidet),
    list(bitset_elem)::in, list(bitset_elem)::out, list(bitset_elem)::out)
    is det <= enum(T).

divide_nodes(_Pred, [], [], []).
divide_nodes(Pred, [Node | Nodes], InNodes, OutNodes) :-
    divide_nodes(Pred, Nodes, InNodesTail, OutNodesTail),
    Node = bitset_elem(Offset, Bits),
    divide_bits(Pred, Offset, 0, Bits, bits_per_int, 0u, In, 0u, Out),
    ( if In = 0u then
        InNodes = InNodesTail
    else
        InNodes = [make_bitset_elem(Offset, In) | InNodesTail]
    ),
    ( if Out = 0u then
        OutNodes = OutNodesTail
    else
        OutNodes = [make_bitset_elem(Offset, Out) | OutNodesTail]
    ).

    % Do a binary search for the 1 bits in an int.
    %
:- pred divide_bits(pred(T)::in(pred(in) is semidet),
    int::in, int::in, uint::in, int::in,
    uint::in, uint::out, uint::in, uint::out) is det <= enum(T).

divide_bits(P, BaseOffset, OffsetInWord, Bits, Size, !In, !Out) :-
    ( if Bits = 0u then
        true
    else if Size = 1 then
        ( if Elem = from_int(BaseOffset + OffsetInWord) then
            OffsetBit = unchecked_left_shift(1u, OffsetInWord),
            ( if P(Elem) then
                !:In = !.In \/ OffsetBit
            else
                !:Out = !.Out \/ OffsetBit
            )
        else
            % We only apply `from_int/1' to integers returned
            % by `to_int/1', so it should never fail.
            unexpected($module, $pred, "`enum.from_int/1' failed")
        )
    else
        HalfSize = unchecked_right_shift(Size, 1),
        Mask = mask(HalfSize),

        % Extract the low-order half of the bits.
        LowBits = Mask /\ Bits,

        % Extract the high-order half of the bits.
        HighBits = Mask /\ unchecked_right_shift(Bits, HalfSize),

        divide_bits(P, BaseOffset, OffsetInWord, LowBits, HalfSize,
            !In, !Out),
        divide_bits(P, BaseOffset, OffsetInWord + HalfSize, HighBits, HalfSize,
            !In, !Out)
    ).

divide_by_set(DivideBySet, Set, InSet, OutSet) :-
    DivideBySet = sparse_bitset(DivideByNodes),
    Set = sparse_bitset(Nodes),
    divide_nodes_by_set(DivideByNodes, Nodes, InNodes, OutNodes),
    InSet = sparse_bitset(InNodes),
    OutSet = sparse_bitset(OutNodes).

:- pred divide_nodes_by_set(list(bitset_elem)::in, list(bitset_elem)::in,
    list(bitset_elem)::out, list(bitset_elem)::out) is det.

divide_nodes_by_set(_DivideByNodes, [], [], []).
divide_nodes_by_set([], [Node | Nodes], [], [Node | Nodes]).
divide_nodes_by_set([DivideByNode | DivideByNodes], [Node | Nodes],
        InNodes, OutNodes) :-
    DivideByNode = bitset_elem(DivideByOffset, DivideByBits),
    Node = bitset_elem(Offset, Bits),
    ( if DivideByOffset < Offset then
        divide_nodes_by_set(DivideByNodes, [Node | Nodes], InNodes, OutNodes)
    else if DivideByOffset > Offset then
        divide_nodes_by_set([DivideByNode | DivideByNodes], Nodes,
            InNodes, OutNodesTail),
        OutNodes = [Node | OutNodesTail]
    else
        divide_nodes_by_set(DivideByNodes, Nodes, InNodesTail, OutNodesTail),
        divide_bits_by_set(DivideByBits, bits_per_int, 0, Bits,
            0u, In, 0u, Out),
        ( if In = 0u then
            InNodes = InNodesTail
        else
            InNodes = [make_bitset_elem(Offset, In) | InNodesTail]
        ),
        ( if Out = 0u then
            OutNodes = OutNodesTail
        else
            OutNodes = [make_bitset_elem(Offset, Out) | OutNodesTail]
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
:- pred divide_bits_by_set(uint::in,
    int::in, int::in, uint::in, uint::in, uint::out, uint::in, uint::out)
    is det.

divide_bits_by_set(DivideByBits, Size, Offset, Bits, !In, !Out) :-
    ( if Bits = 0u then
        true
    else if Size = 1 then
        OffsetBit = unchecked_left_shift(1u, Offset),
        ( if DivideByBits /\ OffsetBit = 0u then
            !:Out = !.Out \/ OffsetBit
        else
            !:In = !.In \/ OffsetBit
        )
    else
        HalfSize = unchecked_right_shift(Size, 1),
        % XXX We could pass around Mask as a parameter, updating it
        % on each recursive call. That may be cheaper than what we do now.
        Mask = mask(HalfSize),

        % Extract the low-order half of the bits.
        LowBits = Mask /\ Bits,

        % Extract the high-order half of the bits.
        HighBits = Mask /\ unchecked_right_shift(Bits, HalfSize),

        divide_bits_by_set(DivideByBits, HalfSize, Offset, LowBits,
            !In, !Out),
        divide_bits_by_set(DivideByBits, HalfSize, Offset + HalfSize, HighBits,
            !In, !Out)
    ).

%---------------------------------------------------------------------------%

    % Return the offset of the element of a set which should contain the given
    % element, and an int with the bit corresponding to that element set.
    %
:- pred bits_for_index(int::in, int::out, uint::out) is det.
:- pragma inline(bits_for_index/3).

bits_for_index(Index, Offset, Bits) :-
    Offset = int.floor_to_multiple_of_bits_per_int(Index),
    BitToSet = Index - Offset,
    Bits = set_bit(0u, BitToSet).

:- func get_bit(uint, int) = uint.
:- pragma inline(get_bit/2).

get_bit(Int, Bit) = Int /\ unchecked_left_shift(1u, Bit).

:- func set_bit(uint, int) = uint.
:- pragma inline(set_bit/2).

set_bit(Int0, Bit) = Int0 \/ unchecked_left_shift(1u, Bit).

:- func clear_bit(uint, int) = uint.
:- pragma inline(clear_bit/2).

clear_bit(Int0, Bit) = Int0 /\ \ unchecked_left_shift(1u, Bit).

    % `mask(N)' returns a mask which can be `and'ed with an integer to return
    % the lower `N' bits of the integer. `N' must be less than bits_per_int.
    %
:- func mask(int) = uint.
:- pragma inline(mask/1).

mask(N) = \ unchecked_left_shift(\ 0u, N).

:- func make_bitset_elem(int, uint) = bitset_elem.
:- pragma inline(make_bitset_elem/2).

make_bitset_elem(Offset, Bits) = bitset_elem(Offset, Bits).

%---------------------------------------------------------------------------%
:- end_module sparse_bitset.
%---------------------------------------------------------------------------%
