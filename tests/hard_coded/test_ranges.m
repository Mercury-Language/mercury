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
    test_search_range(!IO).

%---------------------------------------------------------------------------%

:- pred test_search_range(io::di, io::uo) is det.

test_search_range(!IO) :-

    EmptyRanges = ranges.empty : ranges,
    do_test_search_range(EmptyRanges, "Empty", 0, !IO),

    TestRanges = range(1559, 8460) `union` range(43319, 48780) `union`
        range(93719, 100620),
    TestValues = [-1, 0, 1558, 1560, 8461, 95586, 100620, 100621],
    list.foldl(do_test_search_range(TestRanges, "TestRanges"), TestValues,
        !IO).

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
:- end_module test_ranges.
%---------------------------------------------------------------------------%
