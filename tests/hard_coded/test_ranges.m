%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_ranges.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module ranges.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test_search_range(!IO),
    test_comparison(!IO),
    test_restrict_min(!IO).

%---------------------------------------------------------------------------%

:- pred test_search_range(io::di, io::uo) is det.

test_search_range(!IO) :-
    io.write_string("*** Test search_range/4 ***\n\n", !IO),

    EmptyRanges = ranges.empty : ranges,
    do_test_search_range(EmptyRanges, "Empty", 0, !IO),

    TestRanges = range(1559, 8460) `union` range(43319, 48780) `union`
        range(93719, 100620),
    TestValues = [-1, 0, 1558, 1560, 8461, 95586, 100620, 100621],
    list.foldl(do_test_search_range(TestRanges, "TestRanges"), TestValues,
        !IO),
    io.nl(!IO).

:- pred do_test_search_range(ranges::in, string::in, int::in,
    io::di, io::uo) is det.

do_test_search_range(Ranges, RangesDesc, N, !IO) :-
    io.format("search_range(%d, %s) ==> ", [i(N), s(RangesDesc)], !IO),
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
   ListA = ranges.to_sorted_list(A),
   ListB = ranges.to_sorted_list(B),
   StrA = string(ListA),
   StrB = string(ListB),
   StrLexResult = string(LexResult),
   StrTermResult = string(TermResult),
   io.format("compare_lex(%s, %s) ==> %s\n",
       [s(StrA), s(StrB), s(StrLexResult)], !IO),
   io.format("    compare(%s, %s) ==> %s\n",
       [s(StrA), s(StrB), s(StrTermResult)], !IO).

%---------------------------------------------------------------------------%

:- pred test_restrict_min(io::di, io::uo) is det.

test_restrict_min(!IO) :-
    io.write_string("*** Test restrict_min/2 ***\n\n", !IO),

    % Regression test for problem where restrict_min was not discarding an
    % entire subrange if it was below the minimum.
    R0 = empty,
    insert(1, R0, R1),
    insert(3, R1, R),
    Min = 3,
    Restricted = restrict_min(Min, R),
    StrR = string(ranges.to_sorted_list(R)),
    StrRestricted = string(ranges.to_sorted_list(Restricted)),
    io.format("restrict_min(%d, %s) ==> %s\n",
        [i(Min), s(StrR), s(StrRestricted)], !IO).

%---------------------------------------------------------------------------%
:- end_module test_ranges.
%---------------------------------------------------------------------------%
