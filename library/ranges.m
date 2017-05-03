%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2009 The University of Melbourne.
% Copyright (C) 2013-2016 Opturion Pty Ltd.
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ranges.m.
% Authors: Mark Brown.
% Stability: medium.
%
% This module defines the ranges abstract type.
%
%-----------------------------------------------------------------------------%

:- module ranges.
:- interface.

:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

    % Range lists represent sets of integers. Each contiguous block
    % of integers in the set is stored as an range which specifies
    % the bounds of the block, and these ranges are kept in a list-like
    % structure.
    %
:- type ranges.

    % empty returns the empty set.
    %
:- func empty = ranges.

    % is_empty(Set):
    % Succeeds iff Set is the empty set.
    %
:- pred is_empty(ranges::in) is semidet.

    % is_non_empty(Set):
    % Succeeds iff Set is not the empty set.
    %
:- pred is_non_empty(ranges::in) is semidet.

    % universe returns the largest set that can be handled by this module.
    % This is the set of integers (min_int+1)..max_int. Note that min_int
    % cannot be represented in any set.
    %
:- func universe = ranges.

    % range(Min, Max) is the set of all integers from Min to Max inclusive.
    %
:- func range(int, int) = ranges.

    % split(D, L, H, Rest) is true iff L..H is the first range
    % in D, and Rest is the domain D with this range removed.
    %
:- pred split(ranges::in, int::out, int::out, ranges::out) is semidet.

    % is_contiguous(R, L, H) <=> split(R, L, H, empty):
    % Test if the set is a contiguous set of integers and return the endpoints
    % of this set if this is the case.
    %
:- pred is_contiguous(ranges::in, int::out, int::out) is semidet.

    % Add an integer to the set.
    %
:- func insert(int, ranges) = ranges.
:- pred insert(int::in, ranges::in, ranges::out) is det.

    % Delete an integer from the set.
    %
:- func delete(int, ranges) = ranges.

    % Return the number of distinct integers which are in the ranges
    % (as opposed to the number of ranges).
    %
:- func size(ranges) = int.

    % Returns the median value of the set. In case of a tie, returns
    % the lower of the two options.
    %
:- func median(ranges) = int.

    % least(A, N) is true iff N is the least element of A.
    %
:- pred least(ranges::in, int::out) is semidet.

    % greatest(A, N) is true iff N is the greatest element of A.
    %
:- pred greatest(ranges::in, int::out) is semidet.

    % next(A, N0, N) is true iff N is the least element of A greater
    % than N0.
    %
:- pred next(ranges::in, int::in, int::out) is semidet.

    % Test set membership.
    %
:- pred member(int::in, ranges::in) is semidet.

    % Nondeterministically produce each range.
    %
:- pred range_member(int::out, int::out, ranges::in) is nondet.

    % Nondeterministically produce each element.
    %
:- pred nondet_member(int::out, ranges::in) is nondet.

    % subset(A, B) is true iff every value in A is in B.
    %
:- pred subset(ranges::in, ranges::in) is semidet.

    % disjoint(A, B) is true iff A and B have no values in common.
    %
:- pred disjoint(ranges::in, ranges::in) is semidet.

    % union(A, B) contains all the integers in either A or B.
    %
:- func union(ranges, ranges) = ranges.

    % intersection(A, B) contains all the integers in both A and B.
    %
:- func intersection(ranges, ranges) = ranges.

    % difference(A, B) contains all the integers which are in A but
    % not in B.
    %
:- func difference(ranges, ranges) = ranges.

    % restrict_min(Min, A) contains all integers in A which are greater
    % than or equal to Min.
    %
:- func restrict_min(int, ranges) = ranges.

    % restrict_max(Max, A) contains all integers in A which are less than
    % or equal to Max.
    %
:- func restrict_max(int, ranges) = ranges.

    % restrict_range(Min, Max, A) contains all integers I in A which
    % satisfy Min =< I =< Max.
    %
:- func restrict_range(int, int, ranges) = ranges.

    % prune_to_next_non_member(A0, A, N0, N):
    %
    % N is the smallest integer larger than or equal to N0 which is not
    % in A0. A is the set A0 restricted to values greater than N.
    %
:- pred prune_to_next_non_member(ranges::in, ranges::out,
    int::in, int::out) is det.

    % prune_to_prev_non_member(A0, A, N0, N):
    %
    % N is the largest integer smaller than or equal to N0 which is not
    % in A0. A is the set A0 restricted to values less than N.
    %
:- pred prune_to_prev_non_member(ranges::in, ranges::out,
    int::in, int::out) is det.

    % Negate all numbers:  A in R  <=>  -A in negate(R)
    %
:- func negate(ranges) = ranges.

    % The sum of two ranges.
    %
:- func plus(ranges, ranges) = ranges.

    % Shift a range by const C.
    %
:- func shift(ranges, int) = ranges.

    % Dilate a range by const C.
    %
:- func dilation(ranges, int) = ranges.

    % Contract a range by const C.
    %
:- func contraction(ranges, int) = ranges.

%-----------------------------------------------------------------------------%

    % Convert to a sorted list of integers.
    %
:- func to_sorted_list(ranges) = list(int).

    % Convert from a list of integers.
    %
:- func from_list(list(int)) = ranges.

    % Convert from a set of integers.
    %
:- func from_set(set(int)) = ranges.

%-----------------------------------------------------------------------------%

    % Compare the sets of integers given by the two ranges using lexicographic
    % ordering on the sorted set form.
    %
:- pred compare_lex(comparison_result::uo, ranges::in, ranges::in) is det.

%-----------------------------------------------------------------------------%

:- pred foldl(pred(int, A, A), ranges, A, A).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl(pred(in, di, uo) is semidet, in, di, uo) is semidet.

:- pred foldl2(pred(int, A, A, B, B), ranges, A, A, B, B).
:- mode foldl2(pred(in, in, out, in, out) is det, in, in, out,
    in, out) is det.
:- mode foldl2(pred(in, in, out, mdi, muo) is det, in, in, out,
    mdi, muo) is det.
:- mode foldl2(pred(in, in, out, di, uo) is det, in, in, out,
    di, uo) is det.
:- mode foldl2(pred(in, in, out, in, out) is semidet, in, in, out,
    in, out) is semidet.
:- mode foldl2(pred(in, in, out, mdi, muo) is semidet, in, in, out,
    mdi, muo) is semidet.
:- mode foldl2(pred(in, in, out, di, uo) is semidet, in, in, out,
    di, uo) is semidet.

:- pred foldl3(pred(int, A, A, B, B, C, C), ranges, A, A, B, B, C, C).
:- mode foldl3(pred(in, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out) is det.
:- mode foldl3(pred(in, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, mdi, muo) is det.
:- mode foldl3(pred(in, in, out, in, out, di, uo) is det, in,
    in, out, in, out, di, uo) is det.
:- mode foldl3(pred(in, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, di, uo) is semidet.

:- pred foldr(pred(int, A, A), ranges, A, A).
:- mode foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode foldr(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldr(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldr(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldr(pred(in, di, uo) is semidet, in, di, uo) is semidet.

%-----------------------------------------------------------------------------%

    % For each range, call the predicate, passing it the lower and
    % upper bound and threading through an accumulator.
    %
:- pred range_foldl(pred(int, int, A, A), ranges, A, A).
:- mode range_foldl(pred(in, in, in, out) is det, in, in, out) is det.
:- mode range_foldl(pred(in, in, mdi, muo) is det, in, mdi, muo) is det.
:- mode range_foldl(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode range_foldl(pred(in, in, in, out) is semidet, in, in, out) is semidet.
:- mode range_foldl(pred(in, in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode range_foldl(pred(in, in, di, uo) is semidet, in, di, uo) is semidet.

    % As above, but with two accumulators.
    %
:- pred range_foldl2(pred(int, int, A, A, B, B), ranges, A, A, B, B).
:- mode range_foldl2(pred(in, in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode range_foldl2(pred(in, in, in, out, mdi, muo) is det,
    in, in, out, mdi, muo) is det.
:- mode range_foldl2(pred(in, in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode range_foldl2(pred(in, in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode range_foldl2(pred(in, in, in, out, mdi, muo) is semidet,
    in, in, out, mdi, muo) is semidet.
:- mode range_foldl2(pred(in, in, in, out, di, uo) is semidet,
    in, in, out, di, uo) is semidet.

:- pred range_foldr(pred(int, int, A, A), ranges, A, A).
:- mode range_foldr(pred(in, in, in, out) is det, in, in, out) is det.
:- mode range_foldr(pred(in, in, mdi, muo) is det, in, mdi, muo) is det.
:- mode range_foldr(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode range_foldr(pred(in, in, in, out) is semidet, in, in, out)
    is semidet.
:- mode range_foldr(pred(in, in, mdi, muo) is semidet, in, mdi, muo)
    is semidet.
:- mode range_foldr(pred(in, in, di, uo) is semidet, in, di, uo)
    is semidet.

%-----------------------------------------------------------------------------%
%
% C interface to ranges.
%

% This section describes the C interface to the ranges/0 type that is exported
% by this module.
%
% In C the ranges/0 type is represented by the ML_Ranges type.
% The following operations are exported and may be called from C or C++
% code.
%
% ML_Ranges ML_ranges_empty(void);
% Return the empty set.
%
% ML_Ranges ML_ranges_universe(void);
% Return the set of integers from (min_int+1)..max_int.
%
% int ML_ranges_is_empty(ML_Ranges ranges);
% Succeeds iff ranges is the empty set.
%
% MR_Integer ML_ranges_size(ML_Ranges ranges);
% Return the number of distinct integers in a range.a
%
% int ML_ranges_split(ML_Ranges d, MR_Integer *l, MR_Integer *h,
%       ML_Ranges *rest);
% If `d' is not the empty set, then set `l' and `h"'to the lower
% and upper bound of the first range in `d'; `rest' is set to `d'
% with the first range removed.
%
% ML_Ranges ML_ranges_insert(MR_Integer i, ML_ranges r);
% Return the ranges value that is the result of inserting the integer
% `i' into the ranges value `r'.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.

%-----------------------------------------------------------------------------%

    % Values of this type represent finite sets of integers.
    % They are interpreted in the following way.
    %
    %     S[[ nil ]]                        = {}
    %     S[[ range(L, H, Rest) ]]       = {N | L < N =< H} \/ S[[ Rest ]]
    %
    % For example, `range(1, 4, nil)' is interpreted as {2, 3, 4}.
    %
    % The invariants on this type are:
    %
    %   1) Each range must be non-empty (i.e., L < H).
    %   2) The ranges must not overlap or abut (i.e., for any
    %      value `range(_, H1, range(L2, _, _)' we must have H1 < L2).
    %   3) The ranges must be in sorted order.
    %
    % These invariants ensure that the representation is canonical.
    %
    % Note that it is not possible to represent a set containing min_int.
    % Attempting to create such a set will result in an exception being thrown.
    %
:- type ranges
    --->    nil
    ;       range(int, int, ranges).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "

typedef MR_Word ML_Ranges;

").

:- pragma foreign_export("C", ranges.empty = out, "ML_ranges_empty").

empty = nil.

:- pragma foreign_export("C", ranges.is_empty(in), "ML_ranges_is_empty").

is_empty(nil).

is_non_empty(range(_, _, _)).

:- pragma foreign_export("C", universe = out, "ML_ranges_universe").

universe = range(min_int, max_int, nil).

range(Min, Max) = Ranges :-
    ( if Min = min_int then
        throw("ranges.range: cannot represent min_int")
    else if Min > Max then
        Ranges = nil
    else
        Ranges = range(Min - 1, Max, nil)
    ).

:- pragma foreign_export("C", ranges.split(in, out, out, out),
    "ML_ranges_split").

split(range(Min1, Max, Rest), Min1 + 1, Max, Rest).

is_contiguous(Range, Min + 1, Max) :-
    Range = range(Min, Max, nil).

:- pragma foreign_export("C", ranges.insert(in, in) = out, "ML_ranges_insert").

insert(N, As) = union(As, range(N, N)).
insert(N, As, Bs) :- Bs = insert(N, As).

delete(N, As) = difference(As, range(N, N)).

%-----------------------------------------------------------------------------%

:- pragma foreign_export("C", size(in) = out, "ML_ranges_size").

size(nil) = 0.
size(range(L, U, Rest)) = (U - L) + size(Rest).

median(As) = N :-
    Size = size(As),
    ( if Size > 0 then
        MiddleIndex = (Size + 1) / 2
    else
        throw("ranges.median: empty set")
    ),
    N = element_index(As, MiddleIndex).

    % element_index(Intervals, I) returns the I'th largest value in the set
    % represented by Intervals (the least item in the set having index 1).
    %
:- func element_index(ranges, int) = int.

element_index(nil, _) =
    throw("ranges.element_index: index out of range").
element_index(range(L, U, Rest), I) = N :-
    N0 = L + I,
    ( if N0 =< U then
        N = N0
    else
        N = element_index(Rest, N0 - U)
    ).

%-----------------------------------------------------------------------------%

least(range(L, _, _), L + 1).

greatest(range(_, U0, As), U) :-
    greatest_2(U0, As, U).

:- pred greatest_2(int::in, ranges::in, int::out) is det.

greatest_2(U, nil, U).
greatest_2(_, range(_, U0, As), U) :-
    greatest_2(U0, As, U).

next(range(L, U, As), N0, N) :-
    ( if N0 < U then
        N = int.max(L, N0) + 1
    else
        ranges.next(As, N0, N)
    ).

%-----------------------------------------------------------------------------%

member(N, range(L, U, As)) :-
    (
        N > L,
        N =< U
    ;
        ranges.member(N, As)
    ).

range_member(L, U, range(A0, A1, As)) :-
    (
        L = A0 + 1,
        U = A1
    ;
        range_member(L, U, As)
    ).

nondet_member(N, As) :-
    range_member(L, U, As),
    int.nondet_int_in_range(L, U, N).

%-----------------------------------------------------------------------------%

subset(A, B) :-
    % XXX Should implement this more efficiently.
    ranges.difference(A, B) = nil.

disjoint(A, B) :-
    % XXX Should implement this more efficiently.
    ranges.intersection(A, B) = nil.

%-----------------------------------------------------------------------------%

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
        Result = range(L, UA, ranges.intersection(As0, Bs0))
    ;
        R = (>),
        Result = range(L, UB, diff_a_nb(Bs0, UA, As0))
    ).

    % intersection(A, B) = A /\ B
    %
intersection(nil, _) = nil.
intersection(range(_, _, _), nil) = nil.
intersection(As @ range(LA, UA, As0), Bs @ range(LB, UB, Bs0)) = Result :-
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
        Result = ranges.intersection(As, Bs0)
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

%-----------------------------------------------------------------------------%

restrict_min(_, nil) = nil.
restrict_min(Min, As0 @ range(L, U, As1)) = As :-
    ( if Min =< L then
        As = As0
    else if Min =< U then
        As = range(Min - 1, U, As1)
    else
        As = restrict_min(Min, As1)
    ).

restrict_max(_, nil) = nil.
restrict_max(Max, range(L, U, As0)) = As :-
    ( if Max =< L then
        As = nil
    else if Max =< U then
        As = range(L, Max, nil)
    else
        As = range(L, U, restrict_max(Max, As0))
    ).

restrict_range(Min, Max, As) =
    ranges.intersection(range(Min - 1, Max, nil), As).

%-----------------------------------------------------------------------------%

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

negate(As) = negate_aux(As, nil).

:- func negate_aux(ranges::in, ranges::in) = (ranges::out) is det.

negate_aux(nil, As) = As.
negate_aux(range(L, U, As), A) = negate_aux(As, range(-U-1, -L-1, A)).

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

shift(nil, _) = nil.
shift(As @ range(L, H, As0), C) = Result :-
    ( if C = 0 then
        Result = As
    else
        Result = range(L + C, H + C, shift(As0, C))
    ).

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

%-----------------------------------------------------------------------------%

to_sorted_list(nil) = [].
to_sorted_list(range(L, H, Rest)) =
    to_sorted_list_2(L, H, to_sorted_list(Rest)).

:- func to_sorted_list_2(int, int, list(int)) = list(int).

to_sorted_list_2(L, H, Ints) =
    ( if   H = L then
        Ints
    else
        to_sorted_list_2(L, H-1, [H | Ints])
    ).

from_list(List) =
    list.foldl(ranges.insert, List, ranges.empty).

from_set(Set) =
    ranges.from_list(set.to_sorted_list(Set)).

%-----------------------------------------------------------------------------%

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
                compare_lex_2(Result, !.LBA, !.UBA, !.LBB, !.UBB,
                    !.NextA, !.NextB)
            )
        )
    ).

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%
:- end_module ranges.
%-----------------------------------------------------------------------------%
