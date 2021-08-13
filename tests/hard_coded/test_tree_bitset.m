%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_tree_bitset.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module test_bitset.

:- import_module bool.
:- import_module enum.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module random.
:- import_module random.sfc64.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

:- type maybe_double
    --->    do_not_double
    ;       do_double.

:- type maybe_write
    --->    do_not_write
    ;       do_write.

:- type which_test
    --->    test_count
    ;       test_foldl
    ;       test_union
    ;       test_intersection
    ;       test_difference
    ;       test_remove_least
    ;       test_delete
    ;       test_delete_list
    ;       test_divide_by_set
    ;       test_all.

%---------------------------------------------------------------------------%

:- func list1 = list(int).

list1 =
    [29, 28, 31, 22, 15, 32, 19, 58, 59, 36, 7, 39, 42,
    34, 25, 40, 59, 2, 19, 44, 47, 38].

:- func list2 = list(int).

list2 =
    [21, 52, 23, 18, 23, 56, 11, 46, 61, 4, 63, 54, 17, 64,
    13, 38, 37, 4, 39, 2, 57, 56, 37, 30, 51, 12, 49,
    58, 31, 48, 61, 42, 53, 44, 55, 14, 9, 40, 43, 50].

main(!IO) :-
    % This control makes it easy to test only a specific operation being
    % being worked on.
    WhichTest = test_all,

    SimpleTest = list1 - list2,
    StressTests = [
        % The values of X and Y on 32 bit machines (or 2X and 2Y on 64 bit
        % machines) are intended to end up in near the start and the end
        % of one interior node, while Z (or 2Z) ends up near the start of
        % the next interior node. These stress tests were derived from a bug
        % in the implementation of the difference operation. The bug was in
        % how the difference operation handled interior nodes at the same level
        % but not at the same starting address.

        [532, 32431] -      [32794],
        [1, 29424] -        [1, 2, 3, 35701],
        [1] -               [2, 35701],
        [101, 102] -        [1, 2, 3, 35699, 35700, 35701],
        [36696, 35702, 35703, 35705] -
                            [1, 2, 3, 33416, 334283],

        % These test the handling of empty sets, which several operations
        % handle with special case code.
        [] -                [2],
        [] -                [2, 35701],
        [2] -               [],
        [2, 35701] -        []
    ],

    % Run one lot of tests with known input lists,
    % to generate some visible output.
    run_test(do_not_double, do_write, WhichTest, SimpleTest, !IO),
    run_tests(do_double, do_write, WhichTest, StressTests, !IO),

    % Run some more tests with random input, checking the output against the
    % output of the corresponding predicates in set_ordlist.
    % XXX Ten runs with small input sets is not a rigourous test.
    Iterations = 10,
    List1Size = 20,
    List2Size = 40,
    sfc64.init(RNG, RS0),
    run_random_tests(RNG, Iterations, List1Size, List2Size, WhichTest,
        RS0, _, !IO).

%---------------------------------------------------------------------------%

:- pred run_random_tests(RNG::in, int::in, int::in, int::in, which_test::in,
    RS::di, RS::uo, io::di, io::uo) is det <= urandom(RNG, RS).

run_random_tests(RNG, Iterations, List1Size, List2Size, WhichTest,
        !RS, !IO) :-
    ( if Iterations = 0 then
        true
    else
        get_random_numbers(RNG, List1Size, [], List1, !RS),
        get_random_numbers(RNG, List2Size, [], List2, !RS),
        % We cannot write out the random tests, since we cannot anticipate
        % the random numbers in the .exp file.
        run_test(do_not_double, do_not_write, WhichTest, List1 - List2, !IO),

        run_random_tests(RNG, Iterations - 1, List1Size, List2Size, WhichTest,
            !RS, !IO)
    ).

:- pred get_random_numbers(RNG::in, int::in, list(int)::in, list(int)::out,
    RS::di, RS::uo) is det <= urandom(RNG, RS).

get_random_numbers(RNG, Num, !List, !RS) :-
    ( if Num = 0 then
        true
    else
        % 1048576 = 2^20
        random.uniform_int_in_range(RNG, 0, 1048576, RN, !RS),
        get_random_numbers(RNG, Num - 1, [RN | !.List], !:List, !RS)
    ).

%---------------------------------------------------------------------------%

:- pred run_tests(maybe_double::in, maybe_write::in, which_test::in,
    list(pair(list(int), list(int)))::in, io::di, io::uo) is det.

run_tests(_Double, _Write, _WhichTest, [], !IO).
run_tests(Double, Write, WhichTest, [Test | Tests], !IO) :-
    run_test(Double, Write, WhichTest, Test, !IO),
    run_tests(Double, Write, WhichTest, Tests, !IO).

:- pred run_test(maybe_double::in, maybe_write::in, which_test::in,
    pair(list(int), list(int))::in, io::di, io::uo) is det.

run_test(Double, Write, WhichTest, List1 - List2, !IO) :-
    do_run_test(Write, WhichTest, List1 - List2, !IO),
    (
        Double = do_not_double
    ;
        Double = do_double,
        DoubleList1 = list.map(double, List1),
        DoubleList2 = list.map(double, List2),
        do_run_test(Write, WhichTest, DoubleList1 - DoubleList2, !IO)
    ).

:- func double(int) = int.

double(X) = 2 * X.

%---------------------------------------------------------------------------%

:- pred do_run_test(maybe_write::in, which_test::in,
    pair(list(int), list(int))::in, io::di, io::uo) is det.

do_run_test(Write, WhichTest, List1 - List2, !IO) :-
    (
        Write = do_write,
        io.nl(!IO),
        io.write_string("List1: ", !IO),
        io.write_list(list.sort(List1), ", ", io.write_int, !IO),
        io.nl(!IO),
        io.write_string("List2: ", !IO),
        io.write_list(list.sort(List2), ", ", io.write_int, !IO),
        io.nl(!IO),
        io.nl(!IO)
    ;
        Write = do_not_write
    ),
    Set1 = test_bitset.list_to_set(List1),
    Set2 = test_bitset.list_to_set(List2),

    ( if
        ( WhichTest = test_count
        ; WhichTest = test_all
        )
    then
        io.write_string("testing count\n", !IO),
        Count1 = test_bitset.count(Set1),
        Count2 = test_bitset.count(Set2),
        (
            Write = do_write,
            io.format("count: %d %d\n", [i(Count1), i(Count2)], !IO)
        ;
            Write = do_not_write
        )
    else
        true
    ),

    ( if
        ( WhichTest = test_foldl
        ; WhichTest = test_all
        )
    then
        io.write_string("testing foldl\n", !IO),
        Sum = (func(Elem, Acc) = Elem + Acc),
        Result1 = test_bitset.foldl(Sum, Set1, 0),
        Result2 = test_bitset.foldl(Sum, Set2, 0),
        (
            Write = do_write,
            io.format("Sum of List1 = %d\n", [i(Result1)], !IO),
            io.format("Sum of List2 = %d\n", [i(Result2)], !IO)
        ;
            Write = do_not_write
        )
    else
        true
    ),

    ( if
        ( WhichTest = test_union
        ; WhichTest = test_all
        )
    then
        io.write_string("testing union\n", !IO),
        Union = test_bitset.union(Set1, Set2),
        maybe_write_bitset(Write, Union, !IO)
    else
        true
    ),

    ( if
        ( WhichTest = test_intersection
        ; WhichTest = test_all
        )
    then
        io.write_string("testing intersection\n", !IO),
        Intersection = test_bitset.intersect(Set1, Set2),
        maybe_write_bitset(Write, Intersection, !IO)
    else
        true
    ),

    ( if
        ( WhichTest = test_difference
        ; WhichTest = test_all
        )
    then
        io.write_string("testing difference\n", !IO),
        Difference = test_bitset.difference(Set1, Set2),
        maybe_write_bitset(Write, Difference, !IO)
    else
        true
    ),

    ( if
        ( WhichTest = test_remove_least
        ; WhichTest = test_all
        )
    then
        io.write_string("testing remove_least\n", !IO),
        ( if test_bitset.remove_least(Least, Set1, RemovedLeast) then
            (
                Write = do_write,
                io.write_int(Least, !IO),
                io.nl(!IO)
            ;
                Write = do_not_write
            ),
            maybe_write_bitset(Write, RemovedLeast, !IO)
        else
            (
                Write = do_write,
                io.write_string("call failed\n", !IO)
            ;
                Write = do_not_write
            )
        )
    else
        true
    ),

    ( if
        ( WhichTest = test_delete
        ; WhichTest = test_all
        )
    then
        io.write_string("testing delete\n", !IO),
        list.foldl(test_bitset.delete, List2, Set1, Delete2From1),
        maybe_write_bitset(Write, Delete2From1, !IO),

        list.foldl(test_bitset.delete, List1, Set1, Delete1From1),
        require(unify(Delete1From1, init), "Delete1From1 is not empty")
    else
        true
    ),

    ( if
        ( WhichTest = test_delete_list
        ; WhichTest = test_all
        )
    then
        io.write_string("testing delete_list\n", !IO),
        test_bitset.delete_list(List2, Set1, DeleteList2From1),
        maybe_write_bitset(Write, DeleteList2From1, !IO),

        test_bitset.delete_list(List1, Set1, DeleteList1From1),
        require(unify(DeleteList1From1, init), "DeleteList1From1 is not empty")
    else
        true
    ),

    ( if
        ( WhichTest = test_divide_by_set
        ; WhichTest = test_all
        )
    then
        io.write_string("testing divide_by_set\n", !IO),
        test_bitset.divide_by_set(Set1, Set2, InSet, OutSet),
        maybe_write_bitset(Write, InSet, !IO),
        maybe_write_bitset(Write, OutSet, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_write_bitset(maybe_write::in, test_bitset(int)::in,
    io::di, io::uo) is det.
:- pragma no_inline(pred(maybe_write_bitset/4)).

maybe_write_bitset(Write, Set, !IO) :-
    (
        Write = do_write,
        test_bitset.to_sorted_list(Set, List),
        io.write_line(List, !IO)
    ;
        Write = do_not_write
    ).

%---------------------------------------------------------------------------%
