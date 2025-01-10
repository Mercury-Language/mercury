%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_ranges.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module ranges.
:- import_module set.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    % Initial creation of sets.
    test_construction(!IO),
    test_universe(!IO),

    % Emptiness and other tests
    test_unary_test_op(is_empty, "is_empty", !IO),
    test_unary_test_op(is_non_empty, "is_non_empty", !IO),
    test_is_contiguous(!IO),
    test_is_singleton(!IO),

    % Comparison
    test_comparison(!IO),

    % Membership tests
    test_membership(!IO),

    % Insertions and deletions.
    test_insert_and_delete(insert, "insert", !IO),
    test_insert_and_delete(delete, "delete", !IO),

    % Comparisons between sets.
    test_binary_test_op(subset, "subset", !IO),
    test_binary_test_op(disjoint, "disjoint", !IO),
    % compare_lex/3 is handled by test_comparison above.

    % Operations on two or more sets.
    test_binary_op(union, "union", !IO),
    test_binary_op(intersect, "intersect", !IO),
    test_binary_op(difference, "difference", !IO),

    % Operations that divide a set into two parts.
    test_split(!IO),
    test_prune(prune_to_next_non_member, "prune_to_next_non_member", !IO),
    test_prune(prune_to_prev_non_member, "prune_to_prev_non_member", !IO),

    % Converting lists and sets to ranges.
    test_from_list(!IO),
    test_from_set(!IO),

    % Converting sets to lists.
    test_to_sorted_list(!IO),

    % Counting.
    test_count(!IO),

    % Selecting individual elements from a set.
    test_median(!IO),
    test_least_greatest(least, "least", !IO),
    test_least_greatest(greatest, "greatest", !IO),
    test_next(!IO),
    test_search_range(!IO),

    % Filtering elements in a set.
    test_restrict_min(!IO),
    test_restrict_max(!IO),
    test_restrict_range(!IO),

    % Transformations of a set.
    test_negate(!IO),
    test_binary_op(plus, "plus", !IO),
    test_constant_op(shift, "shift", !IO),
    test_constant_op(dilation, "dilation", !IO),
    test_constant_op(contraction, "contraction", !IO),

    % Standard higher-order functions on elements in a set.
    test_foldl(!IO),
    test_foldl2(!IO),
    test_foldl3(!IO),
    test_foldr(!IO),

    % Standard higher-order functions on range endpoint pairs in set.
    test_range_foldl(!IO),
    test_range_foldl2(!IO),
    test_range_foldr(!IO).

%---------------------------------------------------------------------------%

:- pred test_construction(io::di, io::uo) is cc_multi.

test_construction(!IO) :-
    io.write_string("*** Test construction of ranges ***\n\n", !IO),
    test_empty(!IO),
    test_range(!IO),
    test_make_singleton_set(!IO).

:- pred test_empty(io::di, io::uo) is det.

test_empty(!IO) :-
    io.format("empty = %s\n", [r(empty)], !IO),
    io.nl(!IO).

:- pred test_range(io::di, io::uo) is cc_multi.

test_range(!IO) :-
    io.format("range(0, 0) = %s\n", [r(range(0, 0))], !IO),
    io.format("range(1, 0) = %s\n", [r(range(1, 0))], !IO),
    io.format("range(-1, 1) = %s\n", [r(range(-1, 1))], !IO),
    io.format("range(1, 3) = %s\n", [r(range(1, 3))], !IO),
    io.format("range(3, 1) = %s\n", [r(range(3, 1))], !IO),

    % Check that we cannot construct a set containing min_int.
    io.write_string("range(min_int) = ", !IO),
    ( try []
        Range = range(min_int, 3)
    then
        io.format("%s (FAIL)\n", [r(Range)], !IO)
    catch S ->
        S = software_error(Msg),
        io.format("<<exception: %s>>\n", [s(Msg)], !IO)
    catch_any Other ->
        throw(Other)
    ),
    io.nl(!IO).

:- pred test_make_singleton_set(io::di, io::uo) is cc_multi.

test_make_singleton_set(!IO) :-
    io.format("make_singleton_set(-1) = %s\n", [r(make_singleton_set(-1))], !IO),
    io.format("make_singleton_set(0) = %s\n", [r(make_singleton_set(0))], !IO),
    io.format("make_singleton_set(1) = %s\n", [r(make_singleton_set(1))], !IO),
    % Check that we cannot construct a singleton set containing min_int.
    io.write_string("make_singleton_set(min_int) = ", !IO),
    ( try []
        Range = make_singleton_set(min_int)
    then
        io.format("%s (FAIL)\n", [r(Range)], !IO)
    catch S ->
        S = software_error(Msg),
        io.format("<<exception: %s>>\n", [s(Msg)], !IO)
    catch_any Other ->
        throw(Other)
    ),
    io.format("make_singleton_set(max_int) = %s\n",
        [r(make_singleton_set(max_int))], !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred test_universe(io::di, io::uo) is det.

test_universe(!IO) :-
    io.write_string("*** Test universe/0 ***\n\n", !IO),
    Universe = ranges.universe,
    % We avoid writing the representation of the universe set to
    % to the output since will depend on the size of an int.
    ( if is_empty(Universe) then
        io.write_string("FAIL: ranges.universe is empty.\n", !IO)
    else
        io.write_string("PASS: ranges.universe is empty.\n", !IO)
    ),
    ( if is_contiguous(Universe, Lo, Hi) then
        io.write_string("PASS: ranges.universe is contiguous\n", !IO),
        ( if Lo = int.min_int + 1 then
            io.write_string(
                "PASS: ranges.universe lower bound is min_int + 1.\n", !IO)
        else
            io.format(
                "FAIL: range.universe lower bound is %d, not min_int + 1 (%d).\n",
                [i(Lo), i(min_int + 1)], !IO)
        ),
        ( if Hi = int.max_int then
            io.write_string("PASS: ranges.universe upper bound is max_int.\n",
                !IO)
        else
            io.format(
                "FAIL: ranges.universe upper bound is %d, not max_int (%d).\n",
                [i(Hi), i(max_int)], !IO)
        )
    else
        io.write_string("FAIL: ranges.universe is not contiguous.\n", !IO)
    ),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred test_unary_test_op(pred(ranges)::in(pred(in) is semidet),
    string::in, io::di, io::uo) is det.

test_unary_test_op(Pred, PredName, !IO) :-
    io.format("*** Test %s/1 ***\n\n", [s(PredName)], !IO),
    list.foldl(do_unary_test_op(Pred, PredName), test_ranges, !IO),
    io.nl(!IO).

:- pred do_unary_test_op(pred(ranges)::in(pred(in) is semidet),
    string::in, ranges::in, io::di, io::uo) is det.

do_unary_test_op(Pred, PredName, Arg, !IO) :-
    ( if Pred(Arg) then
        Result = "TRUE"
    else
        Result = "FAIL"
    ),
    io.format("%s(%s) ===> %s\n", [s(PredName), r(Arg), s(Result)], !IO).

%---------------------------------------------------------------------------%

:- pred test_is_contiguous(io::di, io::uo) is det.

test_is_contiguous(!IO) :-
    io.write_string("*** Test is_contiguous/3 ***\n\n", !IO),
    list.foldl(do_test_is_contiguous, test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_is_contiguous(ranges::in, io::di, io::uo) is det.

do_test_is_contiguous(Ranges, !IO) :-
    io.format("is_contiguous(%s) ===> ", [r(Ranges)], !IO),
    ( if is_contiguous(Ranges, Lo, Hi) then
        io.format("[%d, %d]\n", [i(Lo), i(Hi)], !IO)
    else
        io.write_string("FAIL\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_is_singleton(io::di, io::uo) is det.

test_is_singleton(!IO) :-
    io.write_string("*** Test is_singleton/2 ***\n\n", !IO),
    list.foldl(do_test_is_singleton, test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_is_singleton(ranges::in, io::di, io::uo) is det.

do_test_is_singleton(Ranges, !IO) :-
    io.format("is_singleton(%s) ===> ", [r(Ranges)], !IO),
    ( if is_singleton(Ranges, Elem) then
        io.format("%d\n", [i(Elem)], !IO)
    else
        io.write_string("FAIL\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_median(io::di, io::uo) is cc_multi.

test_median(!IO) :-
    io.write_string("*** Test median/1 ***\n\n", !IO),
    list.foldl(do_test_median, test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_median(ranges::in, io::di, io::uo) is cc_multi.

do_test_median(Ranges, !IO) :-
    io.format("median(%s) ===> ", [r(Ranges)], !IO),
    ( try []
        Median = ranges.median(Ranges)
    then
        io.format("%d\n", [i(Median)], !IO)
    catch S ->
        S = software_error(Msg),
        io.format("<<exception: %s>>\n", [s(Msg)], !IO)
    catch_any Other ->
        throw(Other)
    ).

%---------------------------------------------------------------------------%

:- pred test_least_greatest(
    pred(ranges, int)::in(pred(in, out) is semidet),
    string::in, io::di, io::uo) is det.

test_least_greatest(Pred, PredDesc, !IO) :-
    io.format("*** Test %s/2 ***\n\n", [s(PredDesc)], !IO),
    list.foldl(do_test_least_greatest(Pred, PredDesc), test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_least_greatest(
    pred(ranges, int)::in(pred(in, out) is semidet),
    string::in, ranges::in, io::di, io::uo) is det.

do_test_least_greatest(Pred, PredDesc, Ranges, !IO) :-
    io.format("%s(%s) ===> ", [s(PredDesc), r(Ranges)], !IO),
    ( if Pred(Ranges, Result) then
        io.format("%d\n", [i(Result)], !IO)
    else
        io.write_string("FAIL\n", !IO)
    ).
%---------------------------------------------------------------------------%

:- pred test_next(io::di, io::uo) is det.

test_next(!IO) :-
    io.write_string("*** Test next/3 ***\n\n", !IO),
    TestValues = [-11, -1, 0, 1, 11],
    list.foldl(do_test_next(TestValues), test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_next(list(int)::in, ranges::in, io::di, io::uo) is det.

do_test_next(TestValues, Ranges, !IO) :-
    list.foldl(do_test_next_2(Ranges), TestValues, !IO).

:- pred do_test_next_2(ranges::in, int::in, io::di, io::uo) is det.

do_test_next_2(Ranges, Value, !IO) :-
    io.format("next(%s, %d) ===> ", [r(Ranges), i(Value)], !IO),
    ( if ranges.next(Ranges, Value, Next) then
        io.format("%d\n", [i(Next)], !IO)
    else
        io.write_string("FAIL\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_search_range(io::di, io::uo) is det.

test_search_range(!IO) :-
    io.write_string("*** Test search_range/4 ***\n\n", !IO),
    TestValues = [-11, -1, 0, 1, 11, 1558, 1560, 8461, 95586, 100620, 100621],
    % The final case is one of the original regression tests for
    % this predicate.
    TestRanges = test_ranges ++ [
        range(1559, 8460) `union` range(43319, 48780) `union`
            range(93719, 100620)
    ],
    list.foldl(do_search_range(TestValues), TestRanges, !IO),
    io.nl(!IO).

:- pred do_search_range(list(int)::in, ranges::in, io::di, io::uo) is det.

do_search_range(TestValues, Ranges, !IO) :-
    list.foldl(do_test_search_range_2(Ranges), TestValues, !IO).

:- pred do_test_search_range_2(ranges::in, int::in, io::di, io::uo) is det.

do_test_search_range_2(Ranges, N, !IO) :-
    io.format("search_range(%d, %s) ==> ", [i(N), r(Ranges)], !IO),
    ( if ranges.search_range(N, Ranges, Lo, Hi) then
        io.format("(%d, %d).\n", [i(Lo), i(Hi)], !IO)
    else
        io.print_line("FAIL", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_comparison(io::di, io::uo) is det.

test_comparison(!IO) :-
    io.write_string("*** Test compare/3 and lex_compare/3 ***\n\n", !IO),

    do_comparison_test(empty, empty, !IO),

    do_comparison_test(empty, range(1, 1), !IO),
    do_comparison_test(range(1, 1), empty, !IO),

    do_comparison_test(range(1, 1) `union` range(3, 3), range(2, 2), !IO),
    do_comparison_test(range(2, 2), range(1, 1) `union` range(3, 3), !IO),

    do_comparison_test(range(1, 2), range(1, 1) `union` range(3, 3), !IO),
    do_comparison_test(range(1, 1) `union` range(3, 3), range(1, 2), !IO),

    do_comparison_test(range(1, 4), range(1, 1) `union` range(3, 3), !IO),
    do_comparison_test(range(1, 1) `union` range(3, 3), range(1, 4), !IO),

    do_comparison_test(range(1, 4), range(1, 1) `union` range(5, 5), !IO),
    do_comparison_test(range(1, 1) `union` range(5, 5), range(1, 4), !IO),

    do_comparison_test(range(1, 4), range(1, 5), !IO),
    do_comparison_test(range(1, 5), range(1, 4), !IO),

    io.nl(!IO).

:- pred do_comparison_test(ranges::in, ranges::in, io::di, io::uo) is det.

do_comparison_test(A, B, !IO) :-
   compare_lex(LexResult, A, B),
   compare(TermResult, A, B),
   io.format("compare_lex(%s, %s) ===> %s\n",
        [r(A), r(B), s(string(LexResult))], !IO),
   io.format("    compare(%s, %s) ===> %s\n",
       [r(A), r(B), s(string(TermResult))], !IO).

%---------------------------------------------------------------------------%

:- pred test_membership(io::di, io::uo) is det.

test_membership(!IO) :-
    io.write_string("*** Test member/2 ***\n\n", !IO),
    TestValues = [-11, -6 -1, 0, 1, 2, 3, 10, 11],
    list.foldl(do_test_member(TestValues), test_ranges, !IO),

    io.nl(!IO),

    io.write_string("*** Test nondet_range_member/2 ****\n\n", !IO),
    list.foldl(test_nondet_range_member, test_ranges, !IO),

    io.nl(!IO),

    io.write_string("*** Test nondet_member/2 ***\n\n", !IO),
    list.foldl(test_nondet_member, test_ranges, !IO),

    io.nl(!IO).

:- pred do_test_member(list(int)::in, ranges::in, io::di, io::uo) is det.

do_test_member(TestValues, Ranges, !IO) :-
    list.foldl(do_test_member_2(Ranges), TestValues, !IO).

:- pred do_test_member_2(ranges::in, int::in, io::di, io::uo) is det.

do_test_member_2(Ranges, Value, !IO) :-
    ( if ranges.member(Value, Ranges) then
        Result = "TRUE"
    else
        Result = "FAIL"
    ),
    io.format("member(%d, %s) ===> %s\n",
        [i(Value), r(Ranges), s(Result)], !IO).

:- pred test_nondet_range_member(ranges::in, io::di, io::uo) is det.

test_nondet_range_member(Ranges, !IO) :-
    Pred = (pred(V::out) is nondet :-
        nondet_range_member(Lo, Hi, Ranges),
        V = {Lo, Hi}
    ),
    solutions(Pred, Result),
    io.format("nondet_range_member(%s) ===> %s\n",
        [r(Ranges), s(string(Result))], !IO).

:- pred test_nondet_member(ranges::in, io::di, io::uo) is det.

test_nondet_member(Ranges, !IO) :-
    Pred = (pred(V::out) is nondet :-
        nondet_member(V, Ranges)
    ),
    solutions(Pred, Result),
    io.format("nondet_member(%s) ===> %s\n",
        [r(Ranges), s(string(Result))], !IO).

%---------------------------------------------------------------------------%

:- pred test_insert_and_delete((func(int, ranges) = ranges)::in, string::in,
    io::di, io::uo) is cc_multi.

test_insert_and_delete(Op, OpDesc, !IO) :-
    io.format("*** Test %s/2 ***\n\n", [s(OpDesc)], !IO),
    Values = [min_int, -1, 0, 1, 10, max_int],
    list.foldl(do_test_insert_and_delete(Op, OpDesc, Values),
        test_ranges, !IO),
    io.nl(!IO).

:-  pred do_test_insert_and_delete((func(int, ranges) = ranges)::in,
    string::in, list(int)::in, ranges::in, io::di, io::uo) is cc_multi.

do_test_insert_and_delete(Op, OpDesc, Values, Ranges, !IO) :-
    list.foldl(do_test_insert_and_delete_2(Op, OpDesc, Ranges), Values, !IO).

:-  pred do_test_insert_and_delete_2((func(int, ranges) = ranges)::in,
    string::in, ranges::in, int::in, io::di, io::uo) is cc_multi.

do_test_insert_and_delete_2(Op, OpDesc, Ranges, Value, !IO) :-
    io.format("%s(%s, %s) = ",
        [s(OpDesc), n(Value), r(Ranges)], !IO),
    ( try []
        Result = Op(Value, Ranges)
    then
        io.format("%s\n", [r(Result)], !IO)
    catch S ->
        S = software_error(Msg),
        io.format("<<exception: %s>>\n", [s(Msg)], !IO)
    catch_any Other ->
        throw(Other)
    ).

%---------------------------------------------------------------------------%

:- pred test_restrict_min(io::di, io::uo) is det.

test_restrict_min(!IO) :-
    io.write_string("*** Test restrict_min/2 ***\n\n", !IO),
    TestValues = [-11, -1, 0, 1, 3, 11],
    list.foldl(do_test_restrict_min(TestValues), test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_restrict_min(list(int)::in, ranges::in, io::di, io::uo) is det.

do_test_restrict_min(TestValues, Ranges, !IO) :-
    list.foldl(do_test_restrict_min_2(Ranges), TestValues, !IO).

:- pred do_test_restrict_min_2(ranges::in, int::in, io::di, io::uo) is det.

do_test_restrict_min_2(Ranges, Min, !IO) :-
    Restricted = restrict_min(Min, Ranges),
    io.format("restrict_min(%d, %s) = %s\n",
        [i(Min), r(Ranges), r(Restricted)], !IO).

%---------------------------------------------------------------------------%

:- pred test_restrict_max(io::di, io::uo) is det.

test_restrict_max(!IO) :-
    io.write_string("*** Test restrict_max/2 ***\n\n", !IO),
    TestValues = [-11, -1, 1, 3, 11],
    list.foldl(do_test_restrict_max(TestValues), test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_restrict_max(list(int)::in, ranges::in, io::di, io::uo) is det.

do_test_restrict_max(TestValues, Ranges, !IO) :-
    list.foldl(do_test_restrict_max_2(Ranges), TestValues, !IO).

:- pred do_test_restrict_max_2(ranges::in, int::in, io::di, io::uo) is det.

do_test_restrict_max_2(Ranges, Max, !IO) :-
    Restricted = restrict_max(Max, Ranges),
    io.format("restrict_max(%d, %s) = %s\n",
        [i(Max), r(Ranges), r(Restricted)], !IO).

%---------------------------------------------------------------------------%

:- pred test_restrict_range(io::di, io::uo) is det.

test_restrict_range(!IO) :-
    io.write_string("*** Test restrict_range/3 ***\n\n", !IO),
    TestMinMax = [
        {0, 0},
        {1, 3},
        {-2, 2}
    ],
    list.foldl(do_test_restrict_range(TestMinMax), test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_restrict_range(list({int, int})::in, ranges::in,
    io::di, io::uo) is det.

do_test_restrict_range(TestMinMax, Ranges, !IO) :-
    list.foldl(do_test_restrict_range_2(Ranges), TestMinMax, !IO).

:- pred do_test_restrict_range_2(ranges::in, {int, int}::in,
    io::di, io::uo) is det.

do_test_restrict_range_2(Ranges, {Min, Max}, !IO) :-
    Result = ranges.restrict_range(Min, Max, Ranges),
    io.format("restrict_range(%d, %d, %s) = %s\n",
        [i(Min), i(Max), r(Ranges), r(Result)], !IO).

%---------------------------------------------------------------------------%

:- pred test_binary_test_op(pred(ranges, ranges)::in(pred(in, in) is semidet),
    string::in, io::di, io::uo) is det.

test_binary_test_op(Pred, PredName, !IO) :-
    io.format("*** Test %s/2 ***\n\n", [s(PredName)], !IO),
    list.foldl(do_binary_test_op_1(Pred, PredName, test_ranges), test_ranges,
        !IO),
    io.nl(!IO).

:- pred do_binary_test_op_1(pred(ranges, ranges)::in(pred(in, in) is semidet),
    string::in, list(ranges)::in, ranges::in, io::di, io::uo) is det.

do_binary_test_op_1(Pred, PredName, SecondArgs, FirstArg, !IO) :-
    list.foldl(do_binary_test_op_2(Pred, PredName, FirstArg), SecondArgs, !IO).

:- pred do_binary_test_op_2(pred(ranges, ranges)::in(pred(in, in) is semidet),
    string::in, ranges::in, ranges::in, io::di, io::uo) is det.

do_binary_test_op_2(Pred, PredName, FirstArg, SecondArg, !IO) :-
    ( if Pred(FirstArg, SecondArg) then
        Result = "TRUE"
    else
        Result = "FAIL"
    ),
    io.format("%s(%s, %s) ===> %s\n",
        [s(PredName), r(FirstArg), r(SecondArg), s(Result)], !IO).

%---------------------------------------------------------------------------%

:- pred test_binary_op((func(ranges, ranges) = ranges)::in, string::in,
    io::di, io::uo) is det.

test_binary_op(Func, FuncName, !IO) :-
    io.format("*** Test %s/2 ***\n\n", [s(FuncName)], !IO),
    list.foldl(do_binary_set_op_1(Func, FuncName, test_ranges),
        test_ranges, !IO),
    io.nl(!IO).

:- pred do_binary_set_op_1((func(ranges, ranges) = ranges)::in, string::in,
    list(ranges)::in, ranges::in, io::di, io::uo) is det.

do_binary_set_op_1(Func, FuncName, SecondArgs, FirstArg, !IO) :-
    list.foldl(do_binary_set_op_2(Func, FuncName, FirstArg), SecondArgs, !IO).

:- pred do_binary_set_op_2((func(ranges, ranges) = ranges)::in, string::in,
    ranges::in, ranges::in, io::di, io::uo) is det.

do_binary_set_op_2(Func, FuncName, FirstArg, SecondArg, !IO) :-
    Result = Func(FirstArg, SecondArg),
    io.format("%s(%s, %s) = %s\n",
        [s(FuncName), r(FirstArg), r(SecondArg), r(Result)], !IO).

%---------------------------------------------------------------------------%

:- pred test_split(io::di, io::uo) is det.

test_split(!IO) :-
    io.write_string("*** test split/4 ***\n\n", !IO),
    list.foldl(do_test_split, test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_split(ranges::in, io::di, io::uo) is det.

do_test_split(Ranges, !IO) :-
    io.format("split(%s) ===> ", [r(Ranges)], !IO),
    ( if ranges.split(Ranges, Lo, Hi, Rest) then
        io.format("{Lo = %d, Hi = %d, Rest = %s}\n",
            [i(Lo), i(Hi), r(Rest)], !IO)
    else
        io.write_string("FAIL\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_prune(
    pred(ranges, ranges, int, int)::in(pred(in, out, in, out) is det),
    string::in, io::di, io::uo) is det.

test_prune(Pred, PredDesc, !IO) :-
    io.format("**** test %s/4 ***\n\n", [s(PredDesc)], !IO),
    TestValues = [-5, -1, 0, 1, 5],
    list.foldl(do_test_prune(Pred, PredDesc, TestValues), test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_prune(
    pred(ranges, ranges, int, int)::in(pred(in, out, in, out) is det),
    string::in, list(int)::in, ranges::in, io::di, io::uo) is det.

do_test_prune(Pred, PredDesc, TestValues, Ranges, !IO) :-
    list.foldl(do_test_prune_2(Pred, PredDesc, Ranges), TestValues, !IO).

:- pred do_test_prune_2(
    pred(ranges, ranges, int, int)::in(pred(in, out, in, out) is det),
    string::in, ranges::in, int::in, io::di, io::uo) is det.

do_test_prune_2(Pred, PredDesc, Ranges, Value, !IO) :-
    Pred(Ranges, RangesResult, Value, ValueResult),
    io.format("%s(%s, %s, %d, %d).\n",
        [s(PredDesc), r(Ranges), r(RangesResult), i(Value), i(ValueResult)],
        !IO).

%---------------------------------------------------------------------------%

:- pred test_from_list(io::di, io::uo) is cc_multi.

test_from_list(!IO) :-
    io.write_string("*** Test from_list/1 ***\n\n", !IO),
    TestLists = [
        [],
        [min_int],
        [-1],
        [0],
        [1],
        [1, 2, 3],
        [3, 2, 1],
        [3, 3, 2, 2, 1, 1],
        [-10, 1, 2, 3, 10],
        [1, 2, 3, 4, 5, 561, -1]
    ],
    list.foldl(do_test_from_list, TestLists, !IO),
    io.nl(!IO).

:- pred do_test_from_list(list(int)::in, io::di, io::uo) is cc_multi.

do_test_from_list(List, !IO) :-
    io.format("from_list(%s) = ", [li(List)], !IO),
    ( try []
        Ranges = ranges.from_list(List)
    then
        io.format("%s\n", [r(Ranges)], !IO)
    catch S ->
        S = software_error(Msg),
        io.format("<<exception: %s>>\n", [s(Msg)], !IO)
    catch_any Other ->
        throw(Other)
    ).

:- pred test_from_set(io::di, io::uo) is cc_multi.

test_from_set(!IO) :-
    io.write_string("*** Test from_set/1 ***\n\n", !IO),
    TestSets = [
        set.from_list([]),
        set.from_list([min_int]),
        set.from_list([-1]),
        set.from_list([0]),
        set.from_list([1]),
        set.from_list([1, 2, 3]),
        set.from_list([-10, 1, 2, 3, 10]),
        set.from_list([1, 2, 3, 4, 5, 561, -1])
    ],
    list.foldl(do_test_from_set, TestSets, !IO),
    io.nl(!IO).

:- pred do_test_from_set(set(int)::in, io::di, io::uo) is cc_multi.

do_test_from_set(Set, !IO) :-
    io.format("from_set(%s) = ", [si(Set)], !IO),
    ( try []
        Ranges = ranges.from_set(Set)
    then
        io.format("%s\n", [r(Ranges)], !IO)
    catch S ->
        S = software_error(Msg),
        io.format("<<exception: %s>>\n", [s(Msg)], !IO)
    catch_any Other ->
        throw(Other)
    ).

%---------------------------------------------------------------------------%

:- pred test_to_sorted_list(io::di, io::uo) is det.

test_to_sorted_list(!IO) :-
    io.write_string("*** Test to_sorted_list/1 ***\n\n", !IO),
    list.foldl(do_test_to_sorted_list, test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_to_sorted_list(ranges::in, io::di, io::uo) is det.

do_test_to_sorted_list(Ranges, !IO) :-
    List = ranges.to_sorted_list(Ranges),
    io.format("to_sorted_list(%s) = %s\n", [r(Ranges), s(string(List))], !IO).

%---------------------------------------------------------------------------%

:- pred test_count(io::di, io::uo) is det.

test_count(!IO) :-
    io.write_string("*** Test count/1 ***\n\n", !IO),
    list.foldl(do_test_count, test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_count(ranges::in, io::di, io::uo) is det.

do_test_count(Ranges, !IO) :-
    Size = ranges.count(Ranges),
    io.format("count(%s) = %d\n", [r(Ranges), i(Size)], !IO).

%---------------------------------------------------------------------------%

:- pred test_negate(io::di, io::uo) is det.

test_negate(!IO) :-
    io.write_string("*** Test negate/1 ***\n\n", !IO),
    list.foldl(do_test_negate, test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_negate(ranges::in, io::di, io::uo) is det.

do_test_negate(Ranges, !IO) :-
    Negation = ranges.negate(Ranges),
    io.format("negate(%s) = %s\n", [r(Ranges), r(Negation)], !IO).

%---------------------------------------------------------------------------%

:- pred test_constant_op((func(ranges, int) = ranges)::in, string::in,
    io::di, io::uo) is det.

test_constant_op(Func, FuncName, !IO) :-
    io.format("*** Test %s/2 ***\n\n", [s(FuncName)], !IO),
    Constants = [-2, -1, 0, -1, 2],
    list.foldl(test_constant_op_1(Func, FuncName, test_ranges),
        Constants, !IO),
    io.nl(!IO).

:- pred test_constant_op_1((func(ranges, int) = ranges)::in, string::in,
    list(ranges)::in, int::in, io::di, io::uo) is det.

test_constant_op_1(Func, FuncName, RangesList, Constant, !IO) :-
    list.foldl(test_constant_op_2(Func, FuncName, Constant),  RangesList, !IO).

:- pred test_constant_op_2((func(ranges, int) = ranges)::in, string::in,
    int::in, ranges::in, io::di, io::uo) is det.

test_constant_op_2(Func, FuncName, Constant, Ranges, !IO) :-
    Result = Func(Ranges, Constant),
    io.format("%s(%s, %d) = %s\n",
        [s(FuncName), r(Ranges), i(Constant), r(Result)], !IO).

%---------------------------------------------------------------------------%

:- pred test_foldl(io::di, io::uo) is det.

test_foldl(!IO) :-
    io.write_string("*** Test foldl/4 ***\n\n", !IO),
    list.foldl(do_test_foldl, test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_foldl(ranges::in, io::di, io::uo) is det.

do_test_foldl(Ranges, !IO) :-
    ranges.foldl(foldl_tester, Ranges, [], Acc),
    io.format("foldl(foldl_tester, %s, 0) ===> %s\n",
        [r(Ranges), s(string(Acc))], !IO).

:- pred foldl_tester(int::in, list(int)::in, list(int)::out) is det.

foldl_tester(E, Acc, [E | Acc]).

%---------------------------------------------------------------------------%

:- pred test_foldl2(io::di, io::uo) is det.

test_foldl2(!IO) :-
    io.write_string("*** Test foldl2/6 ***\n\n", !IO),
    list.foldl(do_test_foldl2, test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_foldl2(ranges::in, io::di, io::uo) is det.

do_test_foldl2(Ranges, !IO) :-
    ranges.foldl2(foldl2_tester, Ranges, 0, Acc1, 0, Acc2),
    io.format("foldl2(foldl2_tester, %s, 0) ===> {%d, %d}\n",
        [r(Ranges), i(Acc1), i(Acc2)], !IO).

:- pred foldl2_tester(int::in, int::in, int::out, int::in, int::out) is det.

foldl2_tester(E, Acc1, Acc1 + E, Acc2, Acc2 + 1).

%---------------------------------------------------------------------------%

:- pred test_foldl3(io::di, io::uo) is det.

test_foldl3(!IO) :-
    io.write_string("*** Test foldl3/8 ***\n\n", !IO),
    list.foldl(do_test_foldl3, test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_foldl3(ranges::in, io::di, io::uo) is det.

do_test_foldl3(Ranges, !IO) :-
    ranges.foldl3(foldl3_tester, Ranges, 0, Acc1, 0, Acc2, 0, Acc3),
    io.format("foldl3(foldl3_tester, %s, 0) ===> {%d, %d, %d}\n",
        [r(Ranges), i(Acc1), i(Acc2), i(Acc3)], !IO).

:- pred foldl3_tester(int::in, int::in, int::out, int::in, int::out,
    int::in, int::out) is det.

foldl3_tester(E, Acc1, Acc1 + E, Acc2, Acc2 + 1, Acc3, Acc3 - 1).

%---------------------------------------------------------------------------%

:- pred test_foldr(io::di, io::uo) is det.

test_foldr(!IO) :-
    io.write_string("*** Test foldr/4 ***\n\n", !IO),
    list.foldl(do_test_foldr, test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_foldr(ranges::in, io::di, io::uo) is det.

do_test_foldr(Ranges, !IO) :-
    ranges.foldr(foldr_tester, Ranges, [], Acc),
    io.format("foldr(foldr_tester, %s, 0) ===> %s\n",
        [r(Ranges), s(string(Acc))], !IO).

:- pred foldr_tester(int::in, list(int)::in, list(int)::out) is det.

foldr_tester(E, Acc, [E | Acc]).

%---------------------------------------------------------------------------%

:- pred test_range_foldl(io::di, io::uo) is det.

test_range_foldl(!IO) :-
    io.write_string("*** Test range_foldl/4 ***\n\n", !IO),
    list.foldl(do_test_range_foldl, test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_range_foldl(ranges::in, io::di, io::uo) is det.

do_test_range_foldl(Ranges, !IO) :-
    ranges.range_foldl(range_foldl_tester, Ranges, [], Acc),
    io.format("range_foldl(range_foldl_tester, %s, []) ===> %s\n",
        [r(Ranges), s(string(Acc))], !IO).

:- pred range_foldl_tester(int::in, int::in,
    list({int, int})::in, list({int, int})::out) is det.

range_foldl_tester(Lo, Hi, Acc, [{Lo, Hi} | Acc]).

%---------------------------------------------------------------------------%

:- pred test_range_foldl2(io::di, io::uo) is det.

test_range_foldl2(!IO) :-
    io.write_string("*** Test range_foldl/6 ***\n\n", !IO),
    list.foldl(do_test_range_foldl2, test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_range_foldl2(ranges::in, io::di, io::uo) is det.

do_test_range_foldl2(Ranges, !IO) :-
    ranges.range_foldl2(range_foldl2_tester, Ranges, [], Acc1, 0, Acc2),
    io.format("range_foldl2(range_foldl2_tester, %s, [], 0) ===> {%s, %d}\n",
        [r(Ranges), s(string(Acc1)), i(Acc2)], !IO).

:- pred range_foldl2_tester(int::in, int::in,
    list({int, int})::in, list({int, int})::out, int::in, int::out) is det.

range_foldl2_tester(Lo, Hi, Acc1, [{Lo, Hi} | Acc1], Acc2, Acc2 + 1).

%---------------------------------------------------------------------------%

:- pred test_range_foldr(io::di, io::uo) is det.

test_range_foldr(!IO) :-
    io.write_string("*** Test range_foldr/4 ***\n\n", !IO),
    list.foldl(do_test_range_foldr, test_ranges, !IO),
    io.nl(!IO).

:- pred do_test_range_foldr(ranges::in, io::di, io::uo) is det.

do_test_range_foldr(Ranges, !IO) :-
    ranges.range_foldr(range_foldr_tester, Ranges, [], Acc),
    io.format("range_foldr(range_foldr_tester, %s, []) ===> %s\n",
        [r(Ranges), s(string(Acc))], !IO).

:- pred range_foldr_tester(int::in, int::in,
    list({int, int})::in, list({int, int})::out) is det.

range_foldr_tester(Lo, Hi, Acc, [{Lo, Hi} | Acc]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- func r(ranges) = poly_type.

r(Ranges) = s(ranges_to_desc(Ranges)).

:- func ranges_to_desc(ranges) = string.

ranges_to_desc(Ranges) = Desc :-
    ranges.range_foldr(range_to_desc, Ranges, [], RangeDescs),
    Desc = "[" ++ join_list(", ", RangeDescs) ++ "]".

:- pred range_to_desc(int::in, int::in, list(string)::in, list(string)::out)
    is det.

range_to_desc(Lo, Hi, !Descs) :-
    ( if Lo = Hi then
        Desc = string.format("[%s]", [n(Lo)])
    else if Lo + 1 = Hi then
        Desc = string.format("[%s, %s]", [n(Lo), n(Hi)])
    else
        Desc = string.format("[%s .. %s]", [n(Lo), n(Hi)])
    ),
    !:Descs = [Desc | !.Descs].

    % Format an integer, returning a symbolic representation for {min,max}_int.
    %
:- func n(int) = poly_type.

n(N) = s(int_to_maybe_symbolic_string(N)).

    % Format a list of integers, returning symbolic representations for
    % {min,max}_int.
    %
:- func li(list(int)) = poly_type.

li(List) = s(ListStr) :-
    ListStrs = list.map(int_to_maybe_symbolic_string, List),
    ListStr = "[" ++ string.join_list(", ", ListStrs) ++ "]".

:- func si(set(int)) = poly_type.

si(Set) = s(SetStr) :-
    ElemStrs = list.map(int_to_maybe_symbolic_string, to_sorted_list(Set)),
    SetStr = "set([" ++ string.join_list(", ", ElemStrs) ++ "])".

:- func int_to_maybe_symbolic_string(int) = string.

int_to_maybe_symbolic_string(N) = Str :-
    ( if N = min_int then
        Str = "min_int"
    else if N = max_int then
        Str = "max_int"
    else
        string.int_to_string(N, Str)
    ).

%---------------------------------------------------------------------------%

:- func test_ranges = list(ranges).

test_ranges = [
    empty,

    range(-1, -1),
    range(0, 0),
    range(1, 1),

    range(-3, -1),
    range(-1, 1),
    range(1, 3),

    range(-10, -7) `union` range(-3, -1),
    range(1, 3) `union` range(7, 10),

    range(-10, -7) `union` range(-1, 1) `union` range(7, 10),
    range(-6, -3) `union` range(-1, 1) `union` range(3, 6),

    from_list([-1, 1, 3, 5, 7, 9])
].

%---------------------------------------------------------------------------%
:- end_module test_ranges.
%---------------------------------------------------------------------------%
