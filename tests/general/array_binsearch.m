%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%
% Test the array.[approx_]binary_search predicates.
%
%-----------------------------------------------------------------------------%

:- module array_binsearch.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    SortedList = [1, 1, 1, 3, 3, 3, 5, 5, 7, 7, 8, 8, 10, 10, 11, 11,
        13, 13, 13, 16, 16, 19, 20, 20, 21, 21, 23, 27, 28, 28, 28, 29, 31],
    DistractedSortedList = [401, 1, 301, 403, 103, 303, 5, 5, 207, 307,
        408, 108, 210, 10, 411, 311,
        113, 213, 213, 316, 216, 319, 420, 220, 321, 121,
        323, 227, 28, 228, 28, 29, 231],
    Max = 32,
    run_undistracted_tests(SortedList, Max, !IO),
    run_distracted_tests(DistractedSortedList, Max, !IO).

:- func distraction_ordering(int, int) = comparison_result.

distraction_ordering(A, B) = R :-
    UndistractedA = A rem 100,
    UndistractedB = B rem 100,
    R = ordering(UndistractedA, UndistractedB).

%-----------------------------------------------------------------------------%

:- pred run_undistracted_tests(list(int)::in, int::in, io::di, io::uo) is det.

run_undistracted_tests(SortedList, Max, !IO) :-
    array.from_list(SortedList, Array),
    io.write_string("Undistracted tests\n", !IO),
    io.write(SortedList, !IO),
    io.nl(!IO),
    io.nl(!IO),
    undistracted_exact_test_loop(Array, 0, Max, !IO),
    undistracted_approx_test_loop(Array, 0, Max, !IO).

:- pred undistracted_exact_test_loop(array(int)::in, int::in, int::in,
    io::di, io::uo) is det.

undistracted_exact_test_loop(Array, Cur, Max, !IO) :-
    ( if Cur > Max then
        io.nl(!IO)
    else
        io.format("exact  search for %3d ", [i(Cur)], !IO),
        ( if array.binary_search(ordering, Array, Cur, Index) then
            ( if
                exact_verify_success(ordering, Array, Max, Cur, Index,
                    First, Last)
            then
                io.format("succeeds correctly   at %3d", [i(Index)], !IO),
                io.format(", first %3d, last %3d\n", [i(First), i(Last)], !IO)
            else
                io.format("succeeds INCORRECTLY at %3d\n", [i(Index)], !IO)
            )
        else
            ( if exact_verify_failure(ordering, Array, Max, Cur) then
                io.format("fails    correctly\n", [], !IO)
            else
                io.format("fails    INCORRECTLY\n", [], !IO)
            )
        ),
        undistracted_exact_test_loop(Array, Cur + 1, Max, !IO)
    ).

:- pred undistracted_approx_test_loop(array(int)::in, int::in, int::in,
    io::di, io::uo) is det.

undistracted_approx_test_loop(Array, Cur, Max, !IO) :-
    ( if Cur > Max then
        io.nl(!IO)
    else
        io.format("approx search for %3d ", [i(Cur)], !IO),
        ( if
            array.approx_binary_search(ordering, Array, Cur, Index)
        then
            ( if
                approx_verify_success(distraction_ordering, Array, Max, Cur,
                    Index, First, Last)
            then
                io.format("succeeds correctly   at %3d", [i(Index)], !IO),
                io.format(", first %3d, last %3d\n", [i(First), i(Last)], !IO)
            else
                io.format("succeeds INCORRECTLY at %3d\n", [i(Index)], !IO)
            )
        else
            ( if
                approx_verify_failure(distraction_ordering, Array, Max, Cur)
            then
                io.format("fails    correctly\n", [], !IO)
            else
                io.format("fails    INCORRECTLY\n", [], !IO)
            )
        ),
        undistracted_approx_test_loop(Array, Cur + 1, Max, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred run_distracted_tests(list(int)::in, int::in, io::di, io::uo) is det.

run_distracted_tests(SortedList, Max, !IO) :-
    array.from_list(SortedList, Array),
    io.write_string("Distracted tests\n", !IO),
    io.write(SortedList, !IO),
    io.nl(!IO),
    io.nl(!IO),
    distracted_exact_test_loop(Array, 0, 0, Max, !IO),
    distracted_exact_test_loop(Array, 1, 0, Max, !IO),
    distracted_exact_test_loop(Array, 2, 0, Max, !IO),
    distracted_exact_test_loop(Array, 3, 0, Max, !IO),
    distracted_exact_test_loop(Array, 4, 0, Max, !IO),
    distracted_approx_test_loop(Array, 0, 0, Max, !IO),
    distracted_approx_test_loop(Array, 1, 0, Max, !IO),
    distracted_approx_test_loop(Array, 2, 0, Max, !IO),
    distracted_approx_test_loop(Array, 3, 0, Max, !IO),
    distracted_approx_test_loop(Array, 4, 0, Max, !IO).

:- pred distracted_exact_test_loop(array(int)::in, int::in, int::in, int::in,
    io::di, io::uo) is det.

distracted_exact_test_loop(Array, Distraction, Cur, Max, !IO) :-
    ( if Cur > Max then
        io.nl(!IO)
    else
        DCur = Distraction * 100 + Cur,
        io.format("exact  search for %3d ", [i(DCur)], !IO),
        ( if
            array.binary_search(distraction_ordering, Array, DCur, Index)
        then
            ( if
                exact_verify_success(distraction_ordering, Array, Max, DCur,
                    Index, First, Last)
            then
                io.format("succeeds correctly   at %3d", [i(Index)], !IO),
                io.format(", first %3d, last %3d\n", [i(First), i(Last)], !IO)
            else
                io.format("succeeds INCORRECTLY at %3d\n", [i(Index)], !IO)
            )
        else
            ( if
                exact_verify_failure(distraction_ordering, Array, Max, DCur)
            then
                io.format("fails    correctly\n", [], !IO)
            else
                io.format("fails    INCORRECTLY\n", [], !IO)
            )
        ),
        distracted_exact_test_loop(Array, Distraction, Cur + 1, Max, !IO)
    ).

:- pred distracted_approx_test_loop(array(int)::in, int::in, int::in, int::in,
    io::di, io::uo) is det.

distracted_approx_test_loop(Array, Distraction, Cur, Max, !IO) :-
    ( if Cur > Max then
        io.nl(!IO)
    else
        DCur = Distraction * 100 + Cur,
        io.format("approx search for %3d ", [i(DCur)], !IO),
        ( if
            array.approx_binary_search(distraction_ordering, Array,
                DCur, Index)
        then
            ( if
                approx_verify_success(distraction_ordering, Array, Max, DCur,
                    Index, First, Last)
            then
                io.format("succeeds correctly   at %3d", [i(Index)], !IO),
                io.format(", first %3d, last %3d\n", [i(First), i(Last)], !IO)
            else
                io.format("succeeds INCORRECTLY at %3d\n", [i(Index)], !IO)
            )
        else
            ( if
                approx_verify_failure(distraction_ordering, Array, Max, DCur)
            then
                io.format("fails    correctly\n", [], !IO)
            else
                io.format("fails    INCORRECTLY\n", [], !IO)
            )
        ),
        distracted_approx_test_loop(Array, Distraction, Cur + 1, Max, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred exact_verify_success(comparison_func(int)::in, array(int)::in,
    int::in, int::in, int::in, int::out, int::out) is semidet.

exact_verify_success(Cmp, Array, Max, Value, FoundIndex,
        FirstIndex, LastIndex) :-
    array.lookup(Array, FoundIndex, ValueAtFoundIndex),
    Cmp(ValueAtFoundIndex, Value) = Res,
    Res = (=),
    require_det (
        exact_search_back(Cmp, Array, Value, FoundIndex, FirstIndex),
        exact_search_forward(Cmp, Array, Max, Value, FoundIndex, LastIndex)
    ).

:- pred exact_search_back(comparison_func(int)::in, array(int)::in,
    int::in, int::in, int::out) is det.

exact_search_back(Cmp, Array, Value, CurIndex, FirstIndex) :-
    % invariant: Array[CurIndex] = Value
    NextIndex = CurIndex - 1,
    ( if NextIndex >= 0 then
        array.lookup(Array, NextIndex, ValueAtNextIndex),
        ( if Cmp(ValueAtNextIndex, Value) = (=) then
            exact_search_back(Cmp, Array, Value, NextIndex, FirstIndex)
        else
            FirstIndex = CurIndex
        )
    else
        FirstIndex = CurIndex
    ).

:- pred exact_search_forward(comparison_func(int)::in, array(int)::in,
    int::in, int::in, int::in, int::out) is det.

exact_search_forward(Cmp, Array, Max, Value, CurIndex, LastIndex) :-
    % invariant: Array[CurIndex] = Value
    NextIndex = CurIndex + 1,
    ( if NextIndex < Max then
        array.lookup(Array, NextIndex, ValueAtNextIndex),
        ( if Cmp(ValueAtNextIndex, Value) = (=) then
            exact_search_forward(Cmp, Array, Max, Value, NextIndex, LastIndex)
        else
            LastIndex = CurIndex
        )
    else
        LastIndex = CurIndex
    ).

:- pred exact_verify_failure(comparison_func(int)::in, array(int)::in,
    int::in, int::in) is semidet.

exact_verify_failure(Cmp, Array, Max, Value) :-
    exact_verify_failure_loop(Cmp, Array, 0, Max, Value).

:- pred exact_verify_failure_loop(comparison_func(int)::in, array(int)::in,
    int::in, int::in, int::in) is semidet.

exact_verify_failure_loop(Cmp, Array, Cur, Max, Value) :-
    ( if Cur = Max then
        true
    else
        array.lookup(Array, Cur, CurValue),
        Cmp(CurValue, Value) \= (=),
        exact_verify_failure_loop(Cmp, Array, Cur + 1, Max, Value)
    ).

%-----------------------------------------------------------------------------%

:- pred approx_verify_success(comparison_func(int)::in, array(int)::in,
    int::in, int::in, int::in, int::out, int::out) is semidet.

approx_verify_success(Cmp, Array, Max, Value, FoundIndex,
        FirstIndex, LastIndex) :-
    array.lookup(Array, FoundIndex, ValueAtFoundIndex),
    Cmp(ValueAtFoundIndex, Value) = Res,
    (
        Res = (=),
        require_det (
            exact_search_back(Cmp, Array, Value, FoundIndex, FirstIndex),
            exact_search_forward(Cmp, Array, Max, Value, FoundIndex, LastIndex)
        )
    ;
        Res = (<),
        require_det (
            exact_search_back(Cmp, Array, ValueAtFoundIndex,
                FoundIndex, FirstIndex),
            exact_search_forward(Cmp, Array, Max, ValueAtFoundIndex,
                FoundIndex, LastIndex)
        ),
        FollowIndex = LastIndex + 1,
        ( if FollowIndex < Max then
            array.lookup(Array, FollowIndex, ValueAtFollowIndex),
            Cmp(Value, ValueAtFollowIndex) = (<)
        else
            true
        )
    ).

:- pred approx_verify_failure(comparison_func(int)::in, array(int)::in,
    int::in, int::in) is semidet.

approx_verify_failure(Cmp, Array, Max, Value) :-
    approx_verify_failure_loop(Cmp, Array, 0, Max, Value).

:- pred approx_verify_failure_loop(comparison_func(int)::in, array(int)::in,
    int::in, int::in, int::in) is semidet.

approx_verify_failure_loop(Cmp, Array, Cur, Max, Value) :-
    ( if Cur = Max then
        true
    else
        array.lookup(Array, Cur, CurValue),
        Cmp(CurValue, Value) = (>),
        approx_verify_failure_loop(Cmp, Array, Cur + 1, Max, Value)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
