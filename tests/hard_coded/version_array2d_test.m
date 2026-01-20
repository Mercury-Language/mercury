%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module version_array2d_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module uint.
:- import_module univ.
:- import_module version_array2d.

%---------------------------------------------------------------------------%

main(!IO) :-
    A1 = version_array2d([0 `..` 9, 100 `..` 109, 200 `..` 209]),
    % These two updates are overwritten in A2, but they are here to test that
    % the rollback rolls back to the correct version, meaning that changes
    % are applied in the correct order.
    version_array2d.set(1, 3, 333, A1, A10),
    version_array2d.uset(1u, 4u, 444, A10, A11),
    A2 = int.fold_up(func(I, A) = ( A ^ elem(1, I) := 20 - I ), 0, 9, A11),
    version_array2d.set(1, 5, 555, A2, A20),
    version_array2d.uset(1u, 6u, 666, A20, A22),
    % A21 forces an implicit rollback:
    A21 = int.fold_up(make_a21, 0, 9, A1),
    write_array("A1", A1, !IO),
    write_array("A2", A2, !IO),
    write_array("A22", A22, !IO),
    write_array("A21", A21, !IO),

    A3 = unsafe_rewind(A1),
    write_array("A3", A3, !IO),

    A4 = version_array2d.init(7, 7, 7),
    write_array("A4", A4, !IO),
    A5 = uresize(A4, 4u, 4u, 4),
    write_array("A5", A5, !IO),
    A6 = uresize(A5, 9u, 9u, 9),
    write_array("A6", A6, !IO),
    A7 = copy(A5),
    write_array("A7", A7, !IO),

    io.nl(!IO),
    io.write_string("ordering(A1, A2) = ", !IO),
    io.write_line(ordering(A1, A2), !IO),
    io.write_string("ordering(A2, A1) = ", !IO),
    io.write_line(ordering(A2, A1), !IO),
    io.nl(!IO),

    test_exception("misshapen init",
        ( (pred) is semidet :-
            _ = version_array2d([[0, 1, 2], [10, 11]])
        ), !IO),
    test_exception("out-of-bounds test 1",
        ( (pred) is semidet :-
            _ = A7 ^ elem(1, -1)
        ), !IO),
    test_exception("out-of-bounds test 2",
        ( (pred) is semidet :-
            _ : version_array2d(int) = A7 ^ elem(1, -1) := 2
        ), !IO),
    _ = A7 ^ elem(1, 1) := 1,
    test_exception("out-of-bounds test 3",
        ( (pred) is semidet :-
            _ = A7 ^ uelem(1u, 9u) := 2
        ), !IO),
    test_exception("out-of-bounds test 4",
        ( (pred) is semidet :-
            _ = A7 ^ elem(-1, 1)
        ), !IO),
    test_exception("out-of-bounds test 5",
        ( (pred) is semidet :-
            _ = A7 ^ uelem(4u, 4u)
        ), !IO).

:- func make_a21(int, version_array2d(int)) = version_array2d(int).

make_a21(I, A0) = A :-
    X = A0 ^ elem(1, I),
    ( if X `mod` 2 = 0 then
        A = A0 ^ elem(1, I) := X * 3
    else
        A = A0
    ).

:- pred write_array(string::in, version_array2d(int)::in,
    io::di, io::uo) is det.

write_array(Name, Array, !IO) :-
    bounds(Array, NumRowsI, NumColumnsI),
    ubounds(Array, NumRowsU, NumColumnsU),
    ( if
        uint.from_int(NumRowsI, NumRowsIU),
        uint.from_int(NumColumnsI, NumColumnsIU),
        NumRowsIU = NumRowsU,
        NumColumnsIU = NumColumnsU
    then
        RowStrs = to_rows(0, lists(Array)),
        io.format("\n%s: rows %d, columns %d\n",
            [s(Name), i(NumRowsI), i(NumColumnsI)], !IO),
        list.foldl(io.write_string, RowStrs, !IO)
    else
        io.format("\n%s: size mismatch %d/%d vs %u/%u\n",
            [s(Name), i(NumRowsI), i(NumColumnsI),
            u(NumRowsU), u(NumColumnsU)], !IO)
    ).

:- func to_rows(int, list(list(T))) = list(string).

to_rows(_, []) = [].
to_rows(RowNum, [Row | Rows]) = [Line | Lines] :-
    string.format("row %2d: %s\n", [i(RowNum), s(string(Row))], Line),
    Lines = to_rows(RowNum + 1, Rows).

:- pred test_exception(string::in, (pred)::in((pred) is semidet),
    io::di, io::uo) is cc_multi.

test_exception(TestName, Pred, !IO) :-
    try((pred({}::out) is semidet :- Pred), Result),
    (
        Result = succeeded(_),
        io.format("\n%s error: test succeeded, expected exception\n",
            [s(TestName ++ ":")], !IO)
    ;
        Result = failed,
        io.format("\n%s error: test failed, expected exception\n",
            [s(TestName ++ ":")], !IO)
    ;
        Result = exception(Exception),
        io.format("\n%s found exception as expected:\n%s\n",
            [s(TestName ++ ":"), s(string(univ_value(Exception)))], !IO)
    ).

%---------------------------------------------------------------------------%
