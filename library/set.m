%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997, 1999-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: set.m.
% Main authors: conway, fjh, benyi.
% Stability: high.
% 
% This module provides a set ADT.
% The implementation represents sets using ordered lists.
% This file just calls the equivalent predicates in set_ordlist.
% 
%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- module set.
:- interface.

:- import_module bool.
:- import_module list.

:- type set(T).

    % `set.init(Set)' is true iff `Set' is an empty set.
    %
:- pred set.init(set(T)::uo) is det.
:- func set.init = set(T).

    % `set.list_to_set(List, Set)' is true iff `Set' is the set
    % containing only the members of `List'.
    %
:- pred set.list_to_set(list(T)::in, set(T)::out) is det.
:- func set.list_to_set(list(T)) = set(T).

    % Synonyms for set.list_to_set/1.
    %
:- func set.from_list(list(T)) = set(T).
:- func set.set(list(T)) = set(T).

    % `set.sorted_list_to_set(List, Set)' is true iff `Set' is the set
    % containing only the members of `List'.  `List' must be sorted
    % and must not contain any duplicates.
    %
:- pred set.sorted_list_to_set(list(T)::in, set(T)::out) is det.
:- func set.sorted_list_to_set(list(T)) = set(T).

    % A synonym for set.sorted_list_to_set/1.
    %
:- func set.from_sorted_list(list(T)) = set(T).

    % `set.to_sorted_list(Set, List)' is true iff `List' is the list
    % of all the members of `Set', in sorted order without any
    % duplicates.
    %
:- pred set.to_sorted_list(set(T)::in, list(T)::out) is det.
:- func set.to_sorted_list(set(T)) = list(T).

    % `set.singleton_set(Set, Elem)' is true iff `Set' is the set
    % containing just the single element `Elem'.
    %
:- pred set.singleton_set(set(T), T).
:- mode set.singleton_set(in, out) is semidet.
:- mode set.singleton_set(out, in) is det.

:- func set.make_singleton_set(T) = set(T).

    % `set.equal(SetA, SetB)' is true iff
    % `SetA' and `SetB' contain the same elements.
    %
:- pred set.equal(set(T)::in, set(T)::in) is semidet.

:- pred set.empty(set(T)::in) is semidet.

:- pred set.non_empty(set(T)::in) is semidet.

    % `set.subset(SetA, SetB)' is true iff `SetA' is a subset of `SetB'.
    %
:- pred set.subset(set(T)::in, set(T)::in) is semidet.

    % `set.superset(SetA, SetB)' is true iff `SetA' is a
    % superset of `SetB'.
    %
:- pred set.superset(set(T)::in, set(T)::in) is semidet.

    % `set.member(X, Set)' is true iff `X' is a member of `Set'.
    %
:- pred set.member(T, set(T)).
:- mode set.member(in, in) is semidet.
:- mode set.member(out, in) is nondet.

    % `set_is_member(X, Set, Result)' returns
    % `Result = yes' iff `X' is a member of `Set'.
    %
:- pred set.is_member(T::in, set(T)::in, bool::out) is det.

    % `set.contains(Set, X)' is true iff `X' is a member of `Set'.
    %
:- pred set.contains(set(T)::in, T::in) is semidet.

    % `set.insert(Set0, X, Set)' is true iff `Set' is the union of
    % `Set0' and the set containing only `X'.
    %
:- pred set.insert(set(T)::in, T::in, set(T)::out) is det.

    % XXX rwab1: I think we should reverse the args. here for
    % higher order programming.
    %
:- func set.insert(set(T), T) = set(T).

    % `set.insert_list(Set0, Xs, Set)' is true iff `Set' is the union of
    % `Set0' and the set containing only the members of `Xs'.
    %
:- pred set.insert_list(set(T)::in, list(T)::in, set(T)::out) is det.

    % XXX rwab1: I think we should reverse the args. here for
    % higher order programming.
    %
:- func set.insert_list(set(T), list(T)) = set(T).

    % `set.delete(Set0, X, Set)' is true iff `Set' is the relative
    % complement of `Set0' and the set containing only `X', i.e.
    % if `Set' is the set which contains all the elements of `Set0'
    % except `X'.
    %
:- pred set.delete(set(T)::in, T::in, set(T)::out) is det.

    % XXX rwab1: I think we should reverse the args. here for
    % higher order programming.
    %
:- func set.delete(set(T), T) = set(T).

    % `set.delete_list(Set0, Xs, Set)' is true iff `Set' is the relative
    % complement of `Set0' and the set containing only the members of
    % `Xs'.
    %
:- pred set.delete_list(set(T)::in, list(T)::in, set(T)::out) is det.

    % XXX rwab1: I think we should reverse the args. here for
    % higher order programming.
    %
:- func set.delete_list(set(T), list(T)) = set(T).

    % `set.remove(Set0, X, Set)' is true iff `Set0' contains `X',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only `X', i.e.  if `Set' is the set which contains
    % all the elements of `Set0' except `X'.
    %
:- pred set.remove(set(T)::in, T::in, set(T)::out) is semidet.

    % `set.remove_list(Set0, Xs, Set)' is true iff `Xs' does not
    % contain any duplicates, `Set0' contains every member of `Xs',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only the members of `Xs'.
    %
:- pred set.remove_list(set(T)::in, list(T)::in, set(T)::out) is semidet.

    % `set.remove_least(Set0, Elem, Set)' is true iff
    % `Set0' is not empty, `Elem' is the smallest element in `Set0'
    % (with elements ordered using the standard ordering given
    % by compare/3), and `Set' is the set containing all the
    % elements of `Set0' except `Elem'.
    %
:- pred set.remove_least(set(T)::in, T::out, set(T)::out) is semidet.

    % `set_union(SetA, SetB, Set)' is true iff `Set' is the union of
    % `SetA' and `SetB'.  If the sets are known to be of different
    % sizes, then for efficiency make `SetA' the larger of the two.
    % (The current implementation using sorted lists with duplicates
    % removed is not sensitive to the ordering of the input arguments,
    % but other set implementations may be, so observing this convention
    % will make it less likely that you will encounter problems if
    % the implementation is changed.)
    %
:- pred set.union(set(T)::in, set(T)::in, set(T)::out) is det.
:- func set.union(set(T), set(T)) = set(T).

    % `set.union_list(A, B)' is true iff `B' is the union of
    % all the sets in `A'.
    %
:- func set.union_list(list(set(T))) = set(T).

    % `set.power_union(A, B)' is true iff `B' is the union of
    % all the sets in `A'.
    %
:- pred set.power_union(set(set(T))::in, set(T)::out) is det.
:- func set.power_union(set(set(T))) = set(T).

    % `set.intersect(SetA, SetB, Set)' is true iff `Set' is the
    % intersection of `SetA' and `SetB'. If the two sets are
    % known to be unequal in size, then making SetA be the larger
    % set will usually be more efficient.
    % (The current implementation, using sorted lists with duplicates
    % removed is not sensitive to the ordering of the input arguments
    % but other set implementations may be, so observing this convention
    % will make it less likely that you will encounter problems if
    % the implementation is changed.)
    %
:- pred set.intersect(set(T)::in, set(T)::in, set(T)::out) is det.
:- func set.intersect(set(T), set(T)) = set(T).

    % `set.power_intersect(A, B)' is true iff `B' is the intersection of
    % all the sets in `A'.
    %
:- pred set.power_intersect(set(set(T))::in, set(T)::out) is det.
:- func set.power_intersect(set(set(T))) = set(T).

    % `set.intersect_list(A, B)' is true iff `B' is the intersection of
    % all the sets in `A'.
    %
:- func set.intersect_list(list(set(T))) = set(T).

    % `set.difference(SetA, SetB, Set)' is true iff `Set' is the
    % set containing all the elements of `SetA' except those that
    % occur in `SetB'.
    %
:- pred set.difference(set(T)::in, set(T)::in, set(T)::out) is det.
:- func set.difference(set(T), set(T)) = set(T).

    % `set.count(Set, Count)' is true iff `Set' has `Count' elements.
    %
:- pred set.count(set(T)::in, int::out) is det.
:- func set.count(set(T)) = int.

    % Support for higher order set processing.

    % map(F, S) =
    %   list_to_set(list.map(F, to_sorted_list(S))).
    %
:- func set.map(func(T1) = T2, set(T1)) = set(T2).

    % set.map_fold(P, S0, S, A0, A) :-
    %   L0 = set.to_sorted_list(S0),
    %   list.map_foldl(P, L0, L, A0, A),
    %   S = set.list_to_set(L).
    %
:- pred set.map_fold(pred(T1, T2, T3, T3), set(T1), set(T2), T3, T3).
:- mode set.map_fold(pred(in, out, in, out) is det, in, out, in, out) is det.

    % set.filter(P, S) =
    %   sorted_list_to_set(list.filter(P, to_sorted_list(S))).
    %
:- func set.filter(pred(T1), set(T1)) = set(T1).
:- mode set.filter(pred(in) is semidet, in) = out is det.

    % set.filter_map(PF, S) =
    %   list_to_set(list.filter_map(PF, to_sorted_list(S))).
    %
:- func set.filter_map(func(T1) = T2, set(T1)) = set(T2).
:- mode set.filter_map(func(in) = out is semidet, in) = out is det.

    % set.fold(F, S, A) =
    %   list.foldl(F, to_sorted_list(S), A).
    %
:- func set.fold(func(T, A) = A, set(T), A) = A.

:- pred set.fold(pred(T, A, A), set(T), A, A).
:- mode set.fold(pred(in, di, uo) is det, in, di, uo) is det.
:- mode set.fold(pred(in, in, out) is det, in, in, out) is det.
:- mode set.fold(pred(in, in, out) is semidet, in, in, out) is semidet.

:- pred set.fold2(pred(T, A, A, B, B), set(T), A, A, B, B).
:- mode set.fold2(pred(in, in, out, di, uo) is det, in,
    in, out, di, uo) is det.
:- mode set.fold2(pred(in, in, out, in, out) is det, in,
    in, out, in, out) is det.
:- mode set.fold2(pred(in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.

:- pred set.fold3(pred(T, A, A, B, B, C, C), set(T), A, A, B, B, C, C).
:- mode set.fold3(pred(in, in, out, in, out, di, uo) is det, in,
    in, out, in, out, di, uo) is det.
:- mode set.fold3(pred(in, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out) is det.
:- mode set.fold3(pred(in, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out) is semidet.

:- pred set.fold4(pred(T, A, A, B, B, C, C, D, D), set(T), A, A, B, B,
        C, C, D, D).
:- mode set.fold4(pred(in, in, out, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out, in, out) is det.
:- mode set.fold4(pred(in, in, out, in, out, in, out, di, uo) is det, in,
    in, out, in, out, in, out, di, uo) is det.
:- mode set.fold4(pred(in, in, out, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out, in, out) is semidet.
    
    % set.divide(Pred, Set, TruePart, FalsePart):
    % TruePart consists of those elements of Set for which Pred succeeds;
    % FalsePart consists of those elements of Set for which Pred fails.
    %
:- pred set.divide(pred(T)::in(pred(in) is semidet), set(T)::in,
    set(T)::out, set(T)::out) is det.

    % set_divide_by_set(DivideBySet, Set, InPart, OutPart):
    % InPart consists of those elements of Set which are also in
    % DivideBySet; OutPart consists of those elements of which are
    % not in DivideBySet.
    %
:- pred set.divide_by_set(set(T)::in, set(T)::in, set(T)::out, set(T)::out)
    is det.

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

:- import_module term.  % for var/1.

:- pragma type_spec(set.list_to_set/2, T = var(_)).
:- pragma type_spec(set.list_to_set/1, T = var(_)).

:- pragma type_spec(set.member(in, in), T = var(_)).

:- pragma type_spec(set.contains(in, in), T = var(_)).

:- pragma type_spec(set.insert/3, T = var(_)).
:- pragma type_spec(set.insert/2, T = var(_)).

:- pragma type_spec(set.insert_list/3, T = var(_)).
:- pragma type_spec(set.insert_list/2, T = var(_)).

:- pragma type_spec(set.union/3, T = var(_)).
:- pragma type_spec(set.union/2, T = var(_)).

:- pragma type_spec(set.intersect/3, T = var(_)).
:- pragma type_spec(set.intersect/2, T = var(_)).

:- pragma type_spec(set.difference/3, T = var(_)).
:- pragma type_spec(set.difference/2, T = var(_)).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module set_ordlist.

:- type set(T) ==   set_ordlist(T).

set.list_to_set(List, Set) :-
    set_ordlist.list_to_set(List, Set).

set.from_list(List) = set_ordlist.from_list(List).

set.set(List) = set_ordlist.from_list(List).

set.sorted_list_to_set(List, Set) :-
    set_ordlist.sorted_list_to_set(List, Set).

set.from_sorted_list(List) = set_ordlist.from_sorted_list(List).

set.to_sorted_list(Set, List) :-
    set_ordlist.to_sorted_list(Set, List).

set.insert_list(Set0, List, Set) :-
    set_ordlist.insert_list(Set0, List, Set).

set.insert(Set0, X, Set) :-
    set_ordlist.insert(Set0, X, Set).

set.init(Set) :-
    set_ordlist.init(Set).

set.singleton_set(Set, X) :-
    set_ordlist.singleton_set(Set, X).

set.equal(SetA, SetB) :-
    set_ordlist.equal(SetA, SetB).

set.empty(Set) :-
    set_ordlist.empty(Set).

set.non_empty(Set) :-
    \+ set_ordlist.empty(Set).

set.subset(SetA, SetB) :-
    set_ordlist.subset(SetA, SetB).

set.superset(SetA, SetB) :-
    set_ordlist.superset(SetA, SetB).

:- pragma promise_equivalent_clauses(set.member/2).

set.member(X::in, Set::in) :-
    set_ordlist.is_member(X, Set, yes).
set.member(X::out, Set::in) :-
    set_ordlist.member(X, Set).

set.is_member(X, Set, Result) :-
    set_ordlist.is_member(X, Set, Result).

set.contains(Set, X) :-
    set_ordlist.contains(Set, X).

set.delete_list(Set0, List, Set) :-
    set_ordlist.delete_list(Set0, List, Set).

set.delete(Set0, X, Set) :-
    set_ordlist.delete(Set0, X, Set).

set.remove_list(Set0, List, Set) :-
    set_ordlist.remove_list(Set0, List, Set).

set.remove(Set0, X, Set) :-
    set_ordlist.remove(Set0, X, Set).

set.remove_least(Set0, X, Set) :-
    set_ordlist.remove_least(Set0, X, Set).

set.union(SetA, SetB, Set) :-
    set_ordlist.union(SetA, SetB, Set).

set.union_list(Sets) = set_ordlist.union_list(Sets).

set.power_union(Sets, Set) :-
    set_ordlist.power_union(Sets, Set).

set.intersect(SetA, SetB, Set) :-
    set_ordlist.intersect(SetA, SetB, Set).

set.power_intersect(Sets, Set) :-
    set_ordlist.power_intersect(Sets, Set).

set.intersect_list(Sets) = set_ordlist.intersect_list(Sets).

set.difference(SetA, SetB, Set) :-
    set_ordlist.difference(SetA, SetB, Set).

set.count(Set, Count) :-
    set_ordlist.count(Set, Count).

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 24/04/99
%   Function forms added.

set.list_to_set(Xs) = S :-
    set.list_to_set(Xs, S).

set.sorted_list_to_set(Xs) = S :-
    set.sorted_list_to_set(Xs, S).

set.to_sorted_list(S) = Xs :-
    set.to_sorted_list(S, Xs).

set.init = S :-
    set.init(S).

set.make_singleton_set(T) = S :-
    set.singleton_set(S, T).

set.insert(S1, T) = S2 :-
    set.insert(S1, T, S2).

set.insert_list(S1, Xs) = S2 :-
    set.insert_list(S1, Xs, S2).

set.delete(S1, T) = S2 :-
    set.delete(S1, T, S2).

set.delete_list(S1, Xs) = S2 :-
    set.delete_list(S1, Xs, S2).

set.union(S1, S2) = S3 :-
    set.union(S1, S2, S3).

set.power_union(SS) = S :-
    set.power_union(SS, S).

set.intersect(S1, S2) = S3 :-
    set.intersect(S1, S2, S3).

set.power_intersect(SS) = S :-
    set.power_intersect(SS, S).

set.difference(S1, S2) = S3 :-
    set.difference(S1, S2, S3).

set.count(S) = N :-
    set.count(S, N).

set.map(F, S1) = S2 :-
    S2 = set.list_to_set(list.map(F, set.to_sorted_list(S1))).

set.map_fold(P, S0, S, A0, A) :-
    L0 = set.to_sorted_list(S0),
    list.map_foldl(P, L0, L, A0, A),
    S = set.list_to_set(L).

set.filter(P, S1) = S2 :-
    S2 = set.sorted_list_to_set(list.filter(P, set.to_sorted_list(S1))).

set.filter_map(PF, S1) = S2 :-
    S2 = set.list_to_set(list.filter_map(PF, set.to_sorted_list(S1))).

set.fold(F, S, A) = B :-
    B = list.foldl(F, set.to_sorted_list(S), A).

set.fold(F, S, !A) :-
    list.foldl(F, set.to_sorted_list(S), !A).

set.fold2(F, S, !A, !B) :-
    list.foldl2(F, set.to_sorted_list(S), !A, !B).

set.fold3(F, S, !A, !B, !C) :-
    list.foldl3(F, set.to_sorted_list(S), !A, !B, !C).

set.fold4(F, S, !A, !B, !C, !D) :-
    list.foldl4(F, set.to_sorted_list(S), !A, !B, !C, !D).

set.divide(P, Set, TruePart, FalsePart) :-
    set_ordlist.divide(P, Set, TruePart, FalsePart).

set.divide_by_set(DivideBySet, Set, TruePart, FalsePart) :-
    set_ordlist.divide_by_set(DivideBySet, Set, TruePart, FalsePart).
