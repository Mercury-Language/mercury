%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2009 The University of Melbourne.
% Copyright (C) 2013-2016 Opturion Pty Ltd.
% Copyright (C) 2017-2019, 2022-2025 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: ranges.m.
% Authors: Mark Brown.
% Stability: medium.
%
% This module defines the ranges abstract type.
%
%---------------------------------------------------------------------------%

:- module ranges.
:- interface.

:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

    % Range lists represent sets of integers. Each contiguous block
    % of integers in the set is stored as a range which specifies
    % the bounds of the block, and these ranges are kept in a list-like
    % structure.
    %
:- type ranges.

%---------------------------------------------------------------------------%
%
% Initial creation of sets.
%

    % empty returns the empty set.
    %
:- func empty = ranges.

    % universe returns the largest set that can be handled by this module.
    % This is the set of integers (min_int + 1)..max_int. Note that min_int
    % cannot be represented in any set.
    %
:- func universe = ranges.

    % range(Lo, Hi) is the set of all integers from Lo to Hi both inclusive.
    %
:- func range(int, int) = ranges.

    % make_singleton_set(Elem) returns the set containing just the single
    % element Elem.
    %
:- func make_singleton_set(int) = ranges.

%---------------------------------------------------------------------------%
%
% Emptiness and other tests.
%

    % is_empty(Set) is true iff Set is the empty set.
    %
:- pred is_empty(ranges::in) is semidet.

    % is_non_empty(Set) is true iff Set is not the empty set.
    %
:- pred is_non_empty(ranges::in) is semidet.

    % is_contiguous(Set, Lo, Hi) is true iff Set is the set of all integers
    % from Lo to Hi, both inclusive.
    %
:- pred is_contiguous(ranges::in, int::out, int::out) is semidet.

    % is_singleton(Set, Elem) is true iff Set contains the single element Elem.
    %
:- pred is_singleton(ranges::in, int::out) is semidet.

%---------------------------------------------------------------------------%
%
% Membership tests.
%

    % member(X, Set) is true iff X is a member of Set.
    %
:- pred member(int::in, ranges::in) is semidet.

    % contains(Set, X) is true iff X is a member of Set.
    %
:- pred contains(ranges::in, int::in) is semidet.

    % nondet_member(X, Set):
    %
    % Nondeterministically produce each element in Set.
    % Each time this call succeeds, X will be bound to an element in Set.
    %
:- pred nondet_member(int::out, ranges::in) is nondet.

    % nondet_range_member(Lo, Hi, Set):
    %
    % Nondeterministically produce each range in Set.
    % Each time this call succeeds, Lo and Hi will be bound to
    % the smallest and largest integers respectively in a range in Set.
    %
:- pred nondet_range_member(int::out, int::out, ranges::in) is nondet.

    % Obsolete synonym for nondet_range_member/3.
    %
:- pred range_member(int::out, int::out, ranges::in) is nondet.
:- pragma obsolete(pred(range_member/3), [nondet_range_member/3]).

%---------------------------------------------------------------------------%
%
% Insertions and deletions.
%

    % insert(X, Set0, Set) is true iff Set is the union of Set0 and
    % the set containing only X.
    %
:- func insert(int, ranges) = ranges.
:- pred insert(int::in, ranges::in, ranges::out) is det.

    % insert_list(Xs, Set0, Set) is true iff Set is the union of Set0 and
    % the set containing only the members of Xs.
    %
:- func insert_list(list(int), ranges) = ranges.
:- pred insert_list(list(int)::in, ranges::in, ranges::out) is det.

    % delete(X, Set0, Set) is true iff Set is the relative complement
    % of Set0 and the set containing only X, i.e. if Set is the set
    % which contains all the elements of Set0 except X.
    %
:- func delete(int, ranges) = ranges.
:- pred delete(int::in, ranges::in, ranges::out) is det.
% NOTE_TO_IMPLEMENTORS XXX add semidet remove predicate,
% NOTE_TO_IMPLEMENTORS which fails if the item to delete is not in the set

    % delete_list(Xs, Set0, Set) is true iff Set is the relative complement
    % of Set0 and the set containing only the members of Xs.
    %
:- func delete_list(list(int), ranges) = ranges.
:- pred delete_list(list(int)::in, ranges::in, ranges::out) is det.

%---------------------------------------------------------------------------%
%
% Comparisons between sets.
%

    % subset(SetA, SetB) is true iff every value in SetA is in SetB.
    %
:- pred subset(ranges::in, ranges::in) is semidet.

    % disjoint(SetA, SetB) is true iff SetA and SetB have no values in common.
    %
:- pred disjoint(ranges::in, ranges::in) is semidet.

    % Compare the sets of integers given by the two ranges using lexicographic
    % ordering on the sorted set form.
    %
:- pred compare_lex(comparison_result::uo, ranges::in, ranges::in) is det.

%---------------------------------------------------------------------------%
%
% Operations on two or more sets.
%

    % union(SetA, SetB): return the set that contains all the integers in SetA
    % and SetB.
    %
:- func union(ranges, ranges) = ranges.
:- pred union(ranges::in, ranges::in, ranges::out) is det.

    % intersect(SetA, SetB): return the set that contains all the integers
    % in both SetA and SetB.
    %
:- func intersect(ranges, ranges) = ranges.
:- pred intersect(ranges::in, ranges::in, ranges::out) is det.

    % An obsolete synonym for intersect/2.
    %
:- func intersection(ranges, ranges) = ranges.
:- pragma obsolete(func(intersection/2), [intersect/2]).

    % difference(SetA, SetB): return the set that contains all of the integers
    % that are in SetA but not in SetB.
    %
:- func difference(ranges, ranges) = ranges.
:- pred difference(ranges::in, ranges::in, ranges::out) is det.

%---------------------------------------------------------------------------%
%
% Operations that divide a set into two parts.
%

    % split(Set, Lo, Hi, Rest) is true iff Lo..Hi is the first range
    % (i.e. the range containing the smallest integers) in Set, and
    % Rest is the set Set with this range removed.
    %
    % Fails if Set is empty.
    %
:- pred split(ranges::in, int::out, int::out, ranges::out) is semidet.

    % prune_to_next_non_member(Set0, Set, X0, X):
    %
    % Bind X to the smallest integer greater than or equal to X0
    % that is *not* in Set0, and bind Set to the set of integers in Set0
    % that are greater than X.
    % NOTE_TO_IMPLEMENTORS XXX This seems a strange predicate and name.
    %
:- pred prune_to_next_non_member(ranges::in, ranges::out,
    int::in, int::out) is det.

    % prune_to_prev_non_member(Set0, Set, X0, X):
    %
    % Bind X to the largest integer less than or equal to X0
    % that is *not* in Set0, and bind Set to the set of integers in Set0
    % that are less than X.
    % NOTE_TO_IMPLEMENTORS XXX This seems a strange predicate and name.
    %
:- pred prune_to_prev_non_member(ranges::in, ranges::out,
    int::in, int::out) is det.

%---------------------------------------------------------------------------%
%
% Converting lists and sets to ranges.
%

    % Convert from a list of integers.
    % NOTE_TO_IMPLEMENTORS XXX Should have list_to_ranges synonym.
    % NOTE_TO_IMPLEMENTORS XXX Should have sorted_list_to_ranges version.
    %
:- func from_list(list(int)) = ranges.

    % Convert from a set of integers.
    % NOTE_TO_IMPLEMENTORS XXX Should have set_to_ranges synonym.
    %
:- func from_set(set(int)) = ranges.

%---------------------------------------------------------------------------%
%
% Converting sets to lists.
%

    % Convert to a sorted list of integers.
    %
:- func to_sorted_list(ranges) = list(int).

%---------------------------------------------------------------------------%
%
% Counting.
%

    % Return the number of distinct integers that are in the set
    % (as opposed to the number of ranges).
    %
:- func count(ranges) = int.

    % A synonym for count/1.
    % NOTE_TO_IMPLEMENTORS XXX mark this obsolete and eventually remove it.
    %
:- func size(ranges) = int.

%---------------------------------------------------------------------------%
%
% Selecting individual elements from a set.
%

    % Returns the median value of the set. In the case of a tie,
    % returns the smaller of the two integers in the middle of the set.
    % Throws an exception if the set is empty.
    %
:- func median(ranges) = int.

    % least(Set, X) is true iff X is the smallest element of Set.
    % Fails if the set is empty.
    %
:- pred least(ranges::in, int::out) is semidet.

    % greatest(Set, X) is true iff X is the greatest element of Set.
    % Fails if the set is empty.
    %
:- pred greatest(ranges::in, int::out) is semidet.

    % next(Set, X0, X) is true iff X is the least element of Set
    % greater than X0.
    %
:- pred next(ranges::in, int::in, int::out) is semidet.

    % search_range(X, Set, Lo, Hi):
    %
    % If X is in Set, then succeed, setting Lo and Hi to the endpoints
    % of the range in which it is contained. If X is not in Set, fail.
    %
:- pred search_range(int::in, ranges::in, int::out, int::out) is semidet.

%---------------------------------------------------------------------------%
%
% Filtering elements in a set.
%

    % restrict_min(Min, Set): return the set that contains
    % all the integers in Set that are greater than or equal to Min.
    %
:- func restrict_min(int, ranges) = ranges.
:- pred restrict_min(int::in, ranges::in, ranges::out) is det.

    % restrict_max(Max, Set): return the set that contains
    % all the integers in Set that are less than or equal to Max.
    %
:- func restrict_max(int, ranges) = ranges.
:- pred restrict_max(int::in, ranges::in, ranges::out) is det.

    % restrict_range(Min, Max, Set): return the set that contains
    % all the integers X in Set that satisfy Min =< X =< Max.
    %
:- func restrict_range(int, int, ranges) = ranges.
:- pred restrict_range(int::in, int::in, ranges::in, ranges::out) is det.

%---------------------------------------------------------------------------%
%
% Transformations of a set.
% NOTE_TO_IMPLEMENTORS These names are uninformative
%

    % Negate all numbers: X in Set <=> -X in negate(Set)
    %
:- func negate(ranges) = ranges.

    % The sum of two ranges.
    %
:- func plus(ranges, ranges) = ranges.

    % Shift a range by a constant C.
    %
:- func shift(ranges, int) = ranges.

    % Dilate a range by a constant C.
    %
:- func dilation(ranges, int) = ranges.

    % Contract a range by a constant C.
    %
:- func contraction(ranges, int) = ranges.

%---------------------------------------------------------------------------%
%
% Standard higher-order functions on elements in a set.
%

:- pred foldl(pred(int, A, A), ranges, A, A).
:- mode foldl(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldl(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldl(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldl(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldl(in(pred(in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldl(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.

:- pred foldl2(pred(int, A, A, B, B), ranges, A, A, B, B).
:- mode foldl2(in(pred(in, in, out, in, out) is det), in, in, out,
    in, out) is det.
:- mode foldl2(in(pred(in, in, out, mdi, muo) is det), in, in, out,
    mdi, muo) is det.
:- mode foldl2(in(pred(in, in, out, di, uo) is det), in, in, out,
    di, uo) is det.
:- mode foldl2(in(pred(in, in, out, in, out) is semidet), in, in, out,
    in, out) is semidet.
:- mode foldl2(in(pred(in, in, out, mdi, muo) is semidet), in, in, out,
    mdi, muo) is semidet.
:- mode foldl2(in(pred(in, in, out, di, uo) is semidet), in, in, out,
    di, uo) is semidet.

:- pred foldl3(pred(int, A, A, B, B, C, C), ranges, A, A, B, B, C, C).
:- mode foldl3(in(pred(in, in, out, in, out, in, out) is det), in,
    in, out, in, out, in, out) is det.
:- mode foldl3(in(pred(in, in, out, in, out, mdi, muo) is det), in,
    in, out, in, out, mdi, muo) is det.
:- mode foldl3(in(pred(in, in, out, in, out, di, uo) is det), in,
    in, out, in, out, di, uo) is det.
:- mode foldl3(in(pred(in, in, out, in, out, di, uo) is semidet), in,
    in, out, in, out, di, uo) is semidet.

:- pred foldr(pred(int, A, A), ranges, A, A).
:- mode foldr(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldr(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldr(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldr(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldr(in(pred(in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldr(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.

%---------------------------------------------------------------------------%
%
% Standard higher-order functions on range endpoint pairs in set.
%

    % For each range, call the predicate, passing it the lower and
    % upper bound and threading through an accumulator.
    %
:- pred range_foldl(pred(int, int, A, A), ranges, A, A).
:- mode range_foldl(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode range_foldl(in(pred(in, in, mdi, muo) is det), in, mdi, muo) is det.
:- mode range_foldl(in(pred(in, in, di, uo) is det), in, di, uo) is det.
:- mode range_foldl(in(pred(in, in, in, out) is semidet), in, in, out)
    is semidet.
:- mode range_foldl(in(pred(in, in, mdi, muo) is semidet), in, mdi, muo)
    is semidet.
:- mode range_foldl(in(pred(in, in, di, uo) is semidet), in, di, uo)
    is semidet.

    % As above, but with two accumulators.
    %
:- pred range_foldl2(pred(int, int, A, A, B, B), ranges, A, A, B, B).
:- mode range_foldl2(in(pred(in, in, in, out, in, out) is det),
    in, in, out, in, out) is det.
:- mode range_foldl2(in(pred(in, in, in, out, mdi, muo) is det),
    in, in, out, mdi, muo) is det.
:- mode range_foldl2(in(pred(in, in, in, out, di, uo) is det),
    in, in, out, di, uo) is det.
:- mode range_foldl2(in(pred(in, in, in, out, in, out) is semidet),
    in, in, out, in, out) is semidet.
:- mode range_foldl2(in(pred(in, in, in, out, mdi, muo) is semidet),
    in, in, out, mdi, muo) is semidet.
:- mode range_foldl2(in(pred(in, in, in, out, di, uo) is semidet),
    in, in, out, di, uo) is semidet.

:- pred range_foldr(pred(int, int, A, A), ranges, A, A).
:- mode range_foldr(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode range_foldr(in(pred(in, in, mdi, muo) is det), in, mdi, muo) is det.
:- mode range_foldr(in(pred(in, in, di, uo) is det), in, di, uo) is det.
:- mode range_foldr(in(pred(in, in, in, out) is semidet), in, in, out)
    is semidet.
:- mode range_foldr(in(pred(in, in, mdi, muo) is semidet), in, mdi, muo)
    is semidet.
:- mode range_foldr(in(pred(in, in, di, uo) is semidet), in, di, uo)
    is semidet.

%---------------------------------------------------------------------------%
%
% C interface to ranges.
%
% This section describes the C interface to the ranges/0 type
% that is exported by this module.
%
% In C the ranges/0 type is represented by the ML_Ranges type.
% The following operations are exported and may be called from C or C++ code.
%
% ML_Ranges ML_ranges_empty(void)
%   Return the empty set.
%
% ML_Ranges ML_ranges_universe(void)
%   Return the set of integers from (min_int+1)..max_int.
%
% ML_Ranges ML_ranges_range(MR_Integer l, MR_Integer h)
%   Return the set of integers from `l' to `h' inclusive.
%
% int ML_ranges_is_empty(ML_Ranges r)
%   Return true iff `r` is the empty set.
%
% MR_Integer ML_ranges_count(ML_Ranges r)
%  Return the number of distinct integers in `r`.
%
% MR_Integer ML_ranges_size(ML_Ranges r)
%   Return the number of distinct integers in `r'.
%   (Synonym for ML_ranges_count).
%
% int ML_ranges_split(ML_Ranges d, MR_Integer *l, MR_Integer *h,
%       ML_Ranges *rest)
%   Return true if `d' is not the empty set, setting `l' and `h' to the
%   lower and upper bound of the first range in `d', and setting `rest'
%   to `d' with the first range removed.
%   Return false if `d' is the empty set.
%
% ML_Ranges ML_ranges_insert(MR_Integer i, ML_ranges r)
%   Return the ranges value that is the result of inserting
%   the integer `i' into the ranges value `r'.
%
%---------------------------------------------------------------------------%
%
% Java interface to ranges.
%
% This section describes the Java interface to the ranges/0 type that is
% exported by this module.
%
% In Java the ranges/0 type is represented by the ranges.Ranges_0 class.
% The following operations are exported as public static methods of the ranges
% module and may be called from Java code.
%
% ranges.Ranges_0 empty()
%   Return the empty set.
%
% ranges.Ranges_0 universe()
%   Return the set of integers from (min_int+1)..max_int.
%
% ranges.Ranges_0 range(int l, int, h)
%   Return the set of integers from `l' to `h' inclusive.
%
% boolean is_empty(ranges.Ranges_0 r)
%   Return true iff `r' is the empty set.
%
% int count(ranges.Ranges_0 r)
%   Return the number of distinct integers in `r'.
%
% int size(ranges.Ranges_0 r)
%   Return the number of distinct integers in `r'.
%   (Synonym for count).
%
% boolean split(ranges.Ranges_0 d,
%     jmercury.runtime.Ref<Integer> l,
%     jmercury.runtime.Ref<Integer> h,
%     jmercury.runtime.Ref<ranges.Ranges_0> rest)
%   Return true if `d' is not the empty set, setting `l' and `h' to the
%   lower and upper bound of the first range in `d', and setting `rest'
%   to `d' with the first range removed.
%   Return false if `d' is the empty set.
%
% ranges.Ranges_0 insert(int i, ranges.Ranges_0 r)
%   Return the ranges value that is the result of inserting the integer
%   `i' into the ranges value `r'.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

    % Values of this type represent finite sets of integers.
    % They are interpreted in the following way.
    %
    %     S[[ nil ]]                 = {}
    %     S[[ range(Lo, Hi, Rest) ]] = {N | Lo < N =< Hi} \/ S[[ Rest ]]
    %
    % For example, `range(1, 4, nil)' represents the set {2, 3, 4}.
    %
    % The invariants on this type are:
    %
    %   1) Each range must be non-empty, i.e. Lo must be strictly less than Hi.
    %   2) The ranges must not overlap or abut.
    %   3) The ranges must be in sorted order.
    %
    % The second and third invariants together require that for any value
    % `range(_, Hi1, range(Lo2, _, _)', we must have Hi1 < Lo2.
    %
    % These invariants ensure that the representation is canonical.
    %
    % Note that it is not possible to represent a set containing min_int.
    % Attempting to create such a set will result in an exception being thrown.
    %
:- type ranges
    --->    nil
    ;       range(int, int, ranges).

:- pragma foreign_decl("C",
"
typedef MR_Word ML_Ranges;
").

%---------------------------------------------------------------------------%

:- pragma foreign_export("C",    ranges.empty = out, "ML_ranges_empty").
:- pragma foreign_export("Java", ranges.empty = out, "empty").

empty = nil.

:- pragma foreign_export("C",    ranges.universe = out, "ML_ranges_universe").
:- pragma foreign_export("Java", ranges.universe = out, "universe").

universe = range(min_int, max_int, nil).

:- pragma foreign_export("C",
    ranges.range(in, in) = out, "ML_ranges_range").
:- pragma foreign_export("Java",
    ranges.range(in, in) = out, "range").

range(Lo, Hi) = Ranges :-
    ( if Lo = min_int then
        error($pred, "cannot represent min_int")
    else if Lo > Hi then
        Ranges = nil
    else
        Ranges = range(Lo - 1, Hi, nil)
    ).

make_singleton_set(Elem) = Ranges :-
    ( if Elem = min_int then
        error($pred, "cannot represent min_int")
    else
        Ranges = range(Elem - 1, Elem, nil)
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_export("C",    ranges.is_empty(in), "ML_ranges_is_empty").
:- pragma foreign_export("Java", ranges.is_empty(in), "is_empty").

is_empty(nil).

is_non_empty(range(_, _, _)).

is_contiguous(Range, Lo + 1, Hi) :-
    Range = range(Lo, Hi, nil).

is_singleton(range(N - 1, N, nil), N).

%---------------------------------------------------------------------------%

member(N, range(Lo, Hi, Tail)) :-
    (
        N > Lo,
        N =< Hi
    ;
        ranges.member(N, Tail)
    ).

contains(S, E) :-
    member(E, S).

nondet_member(N, As) :-
    nondet_range_member(Lo, Hi, As),
    int.nondet_int_in_range(Lo, Hi, N).

nondet_range_member(Lo, Hi, range(Lo0, Hi0, Tail)) :-
    (
        Lo = Lo0 + 1,
        Hi = Hi0
    ;
        nondet_range_member(Lo, Hi, Tail)
    ).

range_member(Lo, Hi, Ranges) :-
    nondet_range_member(Lo, Hi, Ranges).

%---------------------------------------------------------------------------%

:- pragma foreign_export("C",
    ranges.insert(in, in) = out, "ML_ranges_insert").
:- pragma foreign_export("Java",
    ranges.insert(in, in) = out, "insert").

insert(N, As) = union(As, make_singleton_set(N)).
insert(N, As, Bs) :-
    Bs = insert(N, As).

insert_list(Es, Set0) = Set :-
    insert_list(Es, Set0, Set).

insert_list([], !Set).
insert_list([E | Es], !Set) :-
    !:Set = insert(E, !.Set),
    insert_list(Es, !Set).

%---------------------%

delete(N, As) = difference(As, make_singleton_set(N)).
delete(N, As, Bs) :-
    Bs = delete(N, As).

delete_list(SetA, SetB) = Set:-
    delete_list(SetA, SetB, Set).

delete_list([], !Set).
delete_list([E | Es], !Set) :-
    !:Set = delete(E, !.Set),
    delete_list(Es, !Set).

%---------------------------------------------------------------------------%

subset(SetA, SetB) :-
    % XXX Should implement this more efficiently.
    ranges.difference(SetA, SetB) = nil.

disjoint(SetA, SetB) :-
    % XXX Should implement this more efficiently.
    ranges.intersect(SetA, SetB) = nil.

%---------------------------------------------------------------------------%

compare_lex(Result, A, B) :-
    (
        A = nil,
        B = nil,
        Result = (=)
    ;
        A = nil,
        B = range(_, _, _),
        Result = (<)
    ;
        A = range(_, _, _),
        B = nil,
        Result = (>)
    ;
        A = range(LBA, UBA, APrime),
        B = range(LBB, UBB, BPrime),
        % NOTE: when we unpack a range/3 constructor we must add one
        % to the first argument since that is the lowest value in that
        % subset.
        % XXX Why? Given that compare_lex is a symmetrical operation,
        % and the result of comparing X and Y is guaranteed to be the same
        % as comparing X+1 and Y+1, why bother?
        compare_lex_2(Result, LBA + 1, UBA, LBB + 1, UBB, APrime, BPrime)
    ).

:- pred compare_lex_2(comparison_result::uo, int::in, int::in,
    int::in, int::in, ranges::in, ranges::in) is det.

compare_lex_2(Result, !.LBA, !.UBA, !.LBB, !.UBB, !.NextA, !.NextB) :-
    compare(LBResult, !.LBA, !.LBB),
    (
        ( LBResult = (<)
        ; LBResult = (>)
        ),
        Result = LBResult
    ;
        LBResult = (=),
        compare(UBResult, !.UBA, !.UBB),
        (
            UBResult = (=),
            compare_lex(Result, !.NextA, !.NextB)
        ;
            ( UBResult = (<)
            ; UBResult = (>)
            ),
            ( if
                !.LBA = !.UBA,
                !.LBB = !.UBB
            then
                compare_lex(Result, !.NextA, !.NextB)
            else if
                !.LBA < !.UBA,
                !.LBB = !.UBB
            then
                !:LBA = !.LBA + 1,
                (
                    !.NextB = nil,
                    Result = (>)
                ;
                    !.NextB = range(!:LBB, !:UBB, !:NextB),
                    compare_lex_2(Result, !.LBA, !.UBA, !.LBB + 1, !.UBB,
                        !.NextA, !.NextB)
                )
            else if
                !.LBA = !.UBA,
                !.LBB < !.UBB
            then
                !:LBB = !.LBB + 1,
                (
                    !.NextA = nil,
                    Result = (<)
                ;
                    !.NextA = range(!:LBA, !:UBA, !:NextA),
                    compare_lex_2(Result, !.LBA + 1, !.UBA, !.LBB, !.UBB,
                        !.NextA, !.NextB)
                )
            else
                !:LBA = !.LBA + 1,
                !:LBB = !.LBB + 1,
                disable_warning [suspicious_recursion] (
                    compare_lex_2(Result, !.LBA, !.UBA, !.LBB, !.UBB,
                        !.NextA, !.NextB)
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

    % union(A, B) = A \/ B
    %
union(nil, Bs) = Bs.
union(As @ range(_, _, _), nil) = As.
union(As @ range(LA, UA, As0), Bs @ range(LB, UB, Bs0)) = Result :-
    compare(R, LA, LB),
    (
        R = (<),
        Result = n_diff_na_b(LA, UA, As0, Bs)
    ;
        R = (=),
        Result = n_int_na_nb(LA, UA, As0, UB, Bs0)
    ;
        R = (>),
        Result = n_diff_na_b(LB, UB, Bs0, As)
    ).

union(Xs, Ys, Union) :-
    Union = union(Xs, Ys).

    % n_union_a_nb(L, A, U, B) =
    %   {X | X > L} \ (A \/ ({Y | Y > U} \ B))
    %
    % assuming L < min(A), L < U and U < min(B).
    %
:- func n_union_a_nb(int, ranges, int, ranges) = ranges.

n_union_a_nb(L, nil, U, Bs) = range(L, U, Bs).
n_union_a_nb(L, As @ range(LA, UA, As0), UB, Bs0) = Result :-
    compare(R, LA, UB),
    (
        R = (<),
        Result = range(L, LA, diff_na_nb(UA, As0, UB, Bs0))
    ;
        R = (=),
        Result = range(L, LA, int_na_b(UA, As0, Bs0))
    ;
        R = (>),
        Result = range(L, UB, ranges.difference(Bs0, As))
    ).

    % n_union_na_b(L, U, A, B) =
    %   {X | X > L} \ (({Y | Y > U} \ A) \/ B)
    %
    % assuming L < U, U < min(A) and L < min(B).
    %
:- func n_union_na_b(int, int, ranges, ranges) = ranges.

n_union_na_b(L, U, As, nil) = range(L, U, As).
n_union_na_b(L, UA, As0, Bs @ range(LB, UB, Bs0)) = Result :-
    compare(R, UA, LB),
    (
        R = (<),
        Result = range(L, UA, ranges.difference(As0, Bs))
    ;
        R = (=),
        Result = range(L, UA, int_a_nb(As0, UB, Bs0))
    ;
        R = (>),
        Result = range(L, LB, diff_na_nb(UB, Bs0, UA, As0))
    ).

    % n_union_na_nb(L, UA, A, UB, B) =
    %   {X | X > L} \ (({Y | Y > UA} \ A) \/ ({Z | Z > UB} \ B))
    %
    % assuming L < UA, UA < min(A), L < UB and UB < min(B).
    %
:- func n_union_na_nb(int, int, ranges, int, ranges) = ranges.

n_union_na_nb(L, UA, As0, UB, Bs0) = Result :-
    compare(R, UA, UB),
    (
        R = (<),
        Result = range(L, UA, diff_a_nb(As0, UB, Bs0))
    ;
        R = (=),
        Result = range(L, UA, ranges.intersect(As0, Bs0))
    ;
        R = (>),
        Result = range(L, UB, diff_a_nb(Bs0, UA, As0))
    ).

%---------------------------------------------------------------------------%

    % intersect(A, B) = A /\ B
    %
intersect(nil, _) = nil.
intersect(range(_, _, _), nil) = nil.
intersect(As @ range(LA, UA, As0), Bs @ range(LB, UB, Bs0)) = Result :-
    compare(R, LA, LB),
    (
        R = (<),
        Result = diff_a_nb(Bs, UA, As0)
    ;
        R = (=),
        Result = n_union_na_nb(LA, UA, As0, UB, Bs0)
    ;
        R = (>),
        Result = diff_a_nb(As, UB, Bs0)
    ).

intersect(Xs, Ys, Intersection) :-
    Intersection = intersect(Xs, Ys).

intersection(Xs, Ys) =
    intersect(Xs, Ys).

    % int_na_b(U, A, B) = ({X | X > U} \ A) /\ B
    %
    % assuming U < min(A).
    %
:- func int_na_b(int, ranges, ranges) = ranges.

int_na_b(_, _, nil) = nil.
int_na_b(UA, As0, Bs @ range(LB, UB, Bs0)) = Result :-
    compare(R, UA, LB),
    (
        R = (<),
        Result = ranges.difference(Bs, As0)
    ;
        R = (=),
        Result = n_union_a_nb(UA, As0, UB, Bs0)
    ;
        R = (>),
        Result = diff_na_nb(UA, As0, UB, Bs0)
    ).

    % n_int_na_nb(L, UA, A, UB, B) =
    %   {X | X > L} (({Y | Y > UA} \ A) /\ ({Z | Z > UB} \ B))
    %
    % assuming L < UA, UA < min(A), L < UB and UB < min(B).
    %
:- func n_int_na_nb(int, int, ranges, int, ranges) = ranges.

n_int_na_nb(L, UA, As0, UB, Bs0) = Result :-
    compare(R, UA, UB),
    (
        R = (<),
        Result = n_diff_na_b(L, UB, Bs0, As0)
    ;
        R = (=),
        Result = range(L, UA, ranges.union(As0, Bs0))
    ;
        R = (>),
        Result = n_diff_na_b(L, UA, As0, Bs0)
    ).

    % int_a_nb(A, U, B) = A /\ ({X | X > U} \ B)
    %
    % assuming U < min(B).
    %
:- func int_a_nb(ranges, int, ranges) = ranges.

int_a_nb(nil, _, _) = nil.
int_a_nb(As @ range(LA, UA, As0), UB, Bs0) = Result :-
    compare(R, LA, UB),
    (
        R = (<),
        Result = diff_na_nb(UB, Bs0, UA, As0)
    ;
        R = (=),
        Result = n_union_na_b(LA, UA, As0, Bs0)
    ;
        R = (>),
        Result = ranges.difference(As, Bs0)
    ).

%---------------------------------------------------------------------------%

    % difference(A, B) = A \ B
    %
difference(nil, _) = nil.
difference(As @ range(_, _, _), nil) = As.
difference(As @ range(LA, UA, As0), Bs @ range(LB, UB, Bs0)) = Result :-
    compare(R, LA, LB),
    (
        R = (<),
        Result = n_union_na_b(LA, UA, As0, Bs)
    ;
        R = (=),
        Result = diff_na_nb(UB, Bs0, UA, As0)
    ;
        R = (>),
        Result = int_a_nb(As, UB, Bs0)
    ).

difference(Xs, Ys, Difference) :-
    Difference = difference(Xs, Ys).

    % n_diff_na_b(L, U, A, B) = {X | X > L} \ (({Y | Y > U} \ A) \ B)
    %
    % assuming L < U, U < min(A) and L < min(B).
    %
:- func n_diff_na_b(int, int, ranges, ranges) = ranges.

n_diff_na_b(L, U, As, nil) = range(L, U, As).
n_diff_na_b(L, UA, As0, Bs @ range(LB, UB, Bs0)) = Result :-
    compare(R, UA, LB),
    (
        R = (<),
        Result = range(L, UA, ranges.union(As0, Bs))
    ;
        R = (=),
        Result = n_diff_na_b(L, UB, Bs0, As0)
    ;
        R = (>),
        Result = n_int_na_nb(L, UA, As0, UB, Bs0)
    ).

    % diff_a_nb(A, U, B) = A \ ({X | X > U} \ B)
    %
    % assuming U < min(B).
    %
:- func diff_a_nb(ranges, int, ranges) = ranges.

diff_a_nb(nil, _, _) = nil.
diff_a_nb(As @ range(LA, UA, As0), UB, Bs0) = Result :-
    compare(R, LA, UB),
    (
        R = (<),
        Result = n_union_na_nb(LA, UA, As0, UB, Bs0)
    ;
        R = (=),
        Result = diff_a_nb(Bs0, UA, As0)
    ;
        R = (>),
        Result = ranges.intersect(As, Bs0)
    ).

    % diff_na_nb(UA, A, UB, B) = ({X | X > UA} \ A) \ ({Y | Y > UB} \ B)
    %
    % assuming UA < min(A) and UB < min(B).
    %
:- func diff_na_nb(int, ranges, int, ranges) = ranges.

diff_na_nb(UA, As0, UB, Bs0) = Result :-
    compare(R, UA, UB),
    (
        R = (<),
        Result = n_union_a_nb(UA, As0, UB, Bs0)
    ;
        R = (=),
        Result = ranges.difference(Bs0, As0)
    ;
        R = (>),
        Result = int_na_b(UA, As0, Bs0)
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_export("C",
    ranges.split(in, out, out, out), "ML_ranges_split").
:- pragma foreign_export("Java",
    ranges.split(in, out, out, out), "split").

split(range(Min1, Max, Rest), Min1 + 1, Max, Rest).

%---------------------------------------------------------------------------%

prune_to_next_non_member(nil, nil, !N).
prune_to_next_non_member(As @ range(LA, UA, As0), Result, !N) :-
    ( if !.N =< LA then
        Result = As
    else if !.N =< UA then
        !:N = UA + 1,
        Result = As0
    else
        prune_to_next_non_member(As0, Result, !N)
    ).

prune_to_prev_non_member(nil, nil, !N).
prune_to_prev_non_member(range(LA, UA, As0), Result, !N) :-
    ( if !.N =< LA then
        Result = nil
    else if !.N =< UA then
        !:N = LA,
        Result = nil
    else
        prune_to_prev_non_member(As0, Result0, !N),
        Result = range(LA, UA, Result0)
    ).

%---------------------------------------------------------------------------%

from_list(List) =
    list.foldl(ranges.insert, List, ranges.empty).

from_set(Set) =
    ranges.from_list(set.to_sorted_list(Set)).

%---------------------------------------------------------------------------%

to_sorted_list(nil) = [].
to_sorted_list(range(L, H, Rest)) =
    to_sorted_list_2(L, H, to_sorted_list(Rest)).

:- func to_sorted_list_2(int, int, list(int)) = list(int).

to_sorted_list_2(L, H, Ints) =
    ( if H = L then
        Ints
    else
        to_sorted_list_2(L, H - 1, [H | Ints])
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_export("C",    ranges.count(in) = out, "ML_ranges_count").
:- pragma foreign_export("Java", ranges.count(in) = out, "count").

count(nil) = 0.
count(range(L, U, Rest)) = (U - L) + size(Rest).

:- pragma foreign_export("C",    ranges.size(in) = out, "ML_ranges_size").
:- pragma foreign_export("Java", ranges.size(in) = out, "size").

size(Xs) = count(Xs).

%---------------------------------------------------------------------------%

median(As) = N :-
    Size = size(As),
    ( if Size > 0 then
        MiddleIndex = (Size + 1) / 2
    else
        error($pred, "empty set")
    ),
    N = element_index(As, MiddleIndex).

    % element_index(Intervals, I) returns the I'th largest value in the set
    % represented by Intervals (the least item in the set having index 1).
    %
:- func element_index(ranges, int) = int.

element_index(nil, _) =
    func_error($pred, "index out of range").
element_index(range(L, U, Rest), I) = N :-
    N0 = L + I,
    ( if N0 =< U then
        N = N0
    else
        N = element_index(Rest, N0 - U)
    ).

%---------------------------------------------------------------------------%

least(range(L, _, _), L + 1).

greatest(range(_, U0, As), U) :-
    greatest_loop(U0, As, U).

:- pred greatest_loop(int::in, ranges::in, int::out) is det.

greatest_loop(U, nil, U).
greatest_loop(_, range(_, U0, As), U) :-
    greatest_loop(U0, As, U).

next(range(L, U, As), N0, N) :-
    ( if N0 < U then
        N = int.max(L, N0) + 1
    else
        ranges.next(As, N0, N)
    ).

search_range(N, range(Lo0, Hi0, Rest), Lo, Hi) :-
    ( if
        N > Lo0,
        N =< Hi0
    then
        Lo = Lo0 + 1,
        Hi = Hi0
    else
        search_range(N, Rest, Lo, Hi)
    ).

%---------------------------------------------------------------------------%

restrict_min(_, nil) = nil.
restrict_min(Min, As0 @ range(L, U, As1)) = As :-
    ( if Min =< L then
        As = As0
    else if Min =< U then
        As = range(Min - 1, U, As1)
    else
        As = restrict_min(Min, As1)
    ).

restrict_min(Min, !Set) :-
    !:Set = restrict_min(Min, !.Set).

%---------------------%

restrict_max(_, nil) = nil.
restrict_max(Max, range(L, U, As0)) = As :-
    ( if Max =< L then
        As = nil
    else if Max =< U then
        As = range(L, Max, nil)
    else
        As = range(L, U, restrict_max(Max, As0))
    ).

restrict_max(Max, !Set) :-
    !:Set = restrict_max(Max, !.Set).

%---------------------%

restrict_range(Min, Max, As) =
    ranges.intersect(range(Min - 1, Max, nil), As).

restrict_range(Min, Max, !Set) :-
    !:Set = restrict_range(Min, Max, !.Set).

%---------------------------------------------------------------------------%

negate(As) = negate_aux(As, nil).

:- func negate_aux(ranges::in, ranges::in) = (ranges::out) is det.

negate_aux(nil, As) = As.
negate_aux(range(L, U, As), A) = negate_aux(As, range(-U-1, -L-1, A)).

%---------------------%

plus(nil, nil) = nil.
plus(nil, range(_,_,_)) = nil.
plus(range(_,_,_), nil) = nil.
plus(range(La, Ha, nil), range(L, H, nil)) = range(La + L + 1, Ha + H, nil).
plus(range(Lx0, Hx0, Xs0 @ range(Lx1, Hx1, Xs1)), range(L, H, nil)) = Result :-
    ( if Lx1 - Hx0 < H - L then
        Result = plus(range(Lx0, Hx1, Xs1), range(L, H, nil))
    else
        Result = range(Lx0 + L + 1, Hx0 + H, plus(Xs0, range(L, H, nil)))
    ).
plus(range(Lx, Hx, Xs), range(L, H, S @ range(_,_,_))) = Result :-
    A = plus(range(Lx, Hx, Xs), range(L, H, nil)),
    B = plus(range(Lx, Hx, Xs), S),
    Result = union(A,B).

%---------------------%

shift(nil, _) = nil.
shift(As @ range(L, H, As0), C) = Result :-
    ( if C = 0 then
        Result = As
    else
        Result = range(L + C, H + C, shift(As0, C))
    ).

%---------------------%

dilation(nil, _) = nil.
dilation(A @ range(_,_,_) , C) = Result :-
    ( if C < 0 then
        Result = dilation(negate(A), -C)
    else if C = 0 then
        Result = range(-1, 0, nil)
    else if C = 1 then
        Result = A
    else
        L = to_sorted_list(A),
        list.map(*(C), L) = L0,
        Result = from_list(L0)
    ).

%---------------------%

contraction(nil, _) = nil.
contraction(A @ range(L, H, As), C) = Result :-
    ( if C < 0 then
        Result = contraction(negate(A), -C)
    else if C = 0 then
        Result = range(-1, 0, nil)
    else if C = 1 then
        Result = A
    else
        L0 = div_up_xp(L + 1, C) - 1,
        H0 = div_down_xp(H, C),
        Result = contraction_0(L0, H0, As, C)
    ).

:- func contraction_0(int, int, ranges, int) = ranges.

contraction_0(L0, H0, nil, _) = range(L0, H0, nil).
contraction_0(L0, H0, range(L1, H1, As), C) = Result :-
    L1N = div_up_xp(L1 + 1, C) - 1,
    H1N = div_down_xp(H1, C),
    ( if H0 >= L1N then
        Result = contraction_0(L0, H1N, As, C)
    else
        Result = range(L0, H0, contraction_0(L1N, H1N, As, C))
    ).

%---------------------%

   % 0 < B. Round up.
   %
:- func div_up_xp(int::in, int::in) = (int::out) is det.

div_up_xp(A, B) = (A > 0 -> div_up_pp(A, B) ; div_up_np(A, B)).

    % 0 < A,B. Round up.
    %
:- func div_up_pp(int::in, int::in) = (int::out) is det.

div_up_pp(A, B) = int.unchecked_quotient(A + B - 1, B).

   % A < 0 < B. Round up.
   %
:- func div_up_np(int::in, int::in) = (int::out) is det.

div_up_np(A, B) = int.unchecked_quotient(A, B).

   % 0 < B. Round down.
   %
:- func div_down_xp(int::in, int::in) = (int::out) is det.

div_down_xp(A, B) = (A > 0 -> div_down_pp(A, B) ; div_down_np(A, B)).

    % 0 < A,B. Round down.
    %
:- func div_down_pp(int::in, int::in) = (int::out) is det.

div_down_pp(A, B) = int.unchecked_quotient(A, B).

    % A < 0 < B. Round down.
    %
:- func div_down_np(int::in, int::in) = (int::out) is det.

div_down_np(A, B) = int.unchecked_quotient(A - B + 1, B).

%---------------------------------------------------------------------------%

foldl(P, Ranges, !Acc) :-
    (
        Ranges = nil
    ;
        Ranges = range(L, U, Rest),
        int.fold_up(P, L + 1, U, !Acc),
        foldl(P, Rest, !Acc)
    ).

foldl2(P, Ranges, !Acc1, !Acc2) :-
    (
        Ranges = nil
    ;
        Ranges = range(L, U, Rest),
        int.fold_up2(P, L + 1, U, !Acc1, !Acc2),
        foldl2(P, Rest, !Acc1, !Acc2)
    ).

foldl3(P, Ranges, !Acc1, !Acc2, !Acc3) :-
    (
        Ranges = nil
    ;
        Ranges = range(L, U, Rest),
        int.fold_up3(P, L + 1, U, !Acc1, !Acc2, !Acc3),
        foldl3(P, Rest, !Acc1, !Acc2, !Acc3)
    ).

foldr(P, Ranges, !Acc) :-
    (
        Ranges = nil
    ;
        Ranges = range(L, H, Rest),
        foldr(P, Rest, !Acc),
        int.fold_down(P, L + 1, H, !Acc)
    ).

%---------------------------------------------------------------------------%

range_foldl(_, nil, !Acc).
range_foldl(P, range(L, U, Rest), !Acc) :-
    P(L + 1, U, !Acc),
    range_foldl(P, Rest, !Acc).

range_foldl2(_, nil, !Acc1, !Acc2).
range_foldl2(P, range(L, U, Rest), !Acc1, !Acc2) :-
    P(L + 1, U, !Acc1, !Acc2),
    range_foldl2(P, Rest, !Acc1, !Acc2).

range_foldr(_, nil, !Acc).
range_foldr(P, range(L, U, Rest), !Acc) :-
    range_foldr(P, Rest, !Acc),
    P(L + 1, U, !Acc).

%---------------------------------------------------------------------------%
:- end_module ranges.
%---------------------------------------------------------------------------%
