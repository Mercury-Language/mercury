% vim: ts=4 sw=4 ft=mercury

:- module test_tree_bitset.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module tree_bitset_tester.

:- import_module bool.
:- import_module enum.
:- import_module int.
:- import_module list.
:- import_module random.
:- import_module require.

:- func list1 = list(int).

list1 = [29, 28, 31, 22, 15, 32, 19, 58, 59, 36, 7, 39, 42,
        34, 25, 40, 59, 2, 19, 44, 47, 38].

:- func list2 = list(int).

list2 = [21, 52, 23, 18, 23, 56, 11, 46, 61, 4, 63, 54, 17, 64,
        13, 38, 37, 4, 39, 2, 57, 56, 37, 30, 51, 12, 49,
        58, 31, 48, 61, 42, 53, 44, 55, 14, 9, 40, 43, 50].

main(!IO) :-
    % Run one lot of tests with known input lists,
    % to generate some visible output.
    Write = yes,
    run_test(Write, list1, list2, !IO),

    % Run some more tests with random input, checking
    % the output against that of set_ordlist.
    Iterations = 10,
    random.init(1, Supply),
    run_tests(Iterations, Supply, !IO).

:- pred run_tests(int::in, random.supply::mdi, io::di, io::uo) is det.

run_tests(Iterations, Supply0, !IO) :-
    ( Iterations = 0 ->
        true
    ;
        Num1 = 20,
        get_random_numbers(Num1, [], List1, Supply0, Supply1),
        Num2 = 40,
        get_random_numbers(Num2, [], List2, Supply1, Supply),
        
        Write = no,
        run_test(Write, List1, List2, !IO),

        run_tests(Iterations - 1, Supply, !IO)
    ).

:- pred get_random_numbers(int::in, list(int)::in, list(int)::out,
        random.supply::mdi, random.supply::muo) is det.

get_random_numbers(Num, List0, List, Supply0, Supply) :-
    ( Num = 0 ->
        List = List0,
        Supply = Supply0
    ;
        random.random(0, 256, RN, Supply0, Supply1),
        get_random_numbers(Num - 1, [RN | List0], List, Supply1, Supply)
    ).

:- pred run_test(bool::in, list(int)::in, list(int)::in,
	io::di, io::uo) is det.

run_test(Write, List1, List2, !IO) :-
    (
		Write = yes,
        io.write_string("List1: ", !IO),
        io.write_list(list.sort(List1), ", ", io.write_int, !IO),
        io.nl(!IO),
		io.nl(!IO),
        io.write_string("List2: ", !IO),
        io.write_list(list.sort(List2), ", ", io.write_int, !IO),
        io.nl(!IO),
		io.nl(!IO)
    ;
		Write = no
    ),
    Set1 = tree_bitset_tester.list_to_set(List1),
    Set2 = tree_bitset_tester.list_to_set(List2),

    io.write_string("testing count\n", !IO),
    Count1 = count(Set1),
    Count2 = count(Set2),
    (
		Write = yes,
        io.write_string("count: ", !IO),
        io.write_int(Count1, !IO),
        io.write_string(" ", !IO),
        io.write_int(Count2, !IO),
        io.nl(!IO)
    ;
		Write = no
    ),

    io.write_string("testing foldl\n", !IO),
    Sum = (func(Elem, Acc) = Elem + Acc),
    Result1 = foldl(Sum, Set1, 0),
    Result2 = foldl(Sum, Set2, 0),
    (
		Write = yes,
        io.write_string("Sum of List1 = ", !IO),
        io.write_int(Result1, !IO),
        io.nl(!IO),
        io.write_string("Sum of List2 = ", !IO),
        io.write_int(Result2, !IO),
        io.nl(!IO)
    ;
		Write = no
    ),

    io.write_string("testing union\n", !IO),
    Union = union(Set1, Set2),
    write_bitset_result(Write, Union, !IO),

    io.write_string("testing intersection\n", !IO),
    Intersection = intersect(Set1, Set2),
    write_bitset_result(Write, Intersection, !IO),

    io.write_string("testing difference\n", !IO),
    Difference = difference(Set1, Set2),
    write_bitset_result(Write, Difference, !IO),

    io.write_string("testing remove_least_element\n", !IO),
    ( remove_least(Set1, Least, RemovedLeast) ->
        (
			Write = yes,
            io.write_int(Least, !IO),
            io.nl(!IO)
        ;
			Write = no
        ),
        write_bitset_result(Write, RemovedLeast, !IO)
    ;
        error("remove_least failed")
    ),

    io.write_string("testing delete_list\n", !IO),
    Delete = delete_list(Set1, List2),
    write_bitset_result(Write, Delete, !IO),

    require(unify(delete_list(Set1, List1),
            init `with_type` tree_bitset_tester(int)),
        "delete_list_failed").

:- pred write_bitset_result(bool::in, bitset_tester(int)::in,
	io::di, io::uo) is det.
:- pragma no_inline(write_bitset_result/4).
    
write_bitset_result(Write, Set, !IO) :-
    ( 
		Write = yes,
        List `with_type` list(int) = to_sorted_list(Set),
        io.write(List, !IO),
        io.nl(!IO)
    ;
        Write = no
    ).
