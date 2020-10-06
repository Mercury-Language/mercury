%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% version_array_test.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Fri Oct  8 15:00:25 EST 2004
%---------------------------------------------------------------------------%

:- module version_array_test.

:- interface.

:- import_module io.

:- pred main(io :: di, io :: uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module univ.
:- import_module version_array.

%---------------------------------------------------------------------------%

main(!IO) :-
    A0 = version_array.empty,
    A1 = version_array(0`..`20),
    % These two updates are overwritten, but they are here to help test that
    % the rollback rolls back to the correct version, that is that changes
    % are applied in the correct order.
    version_array.set(3, 333, A1, A10),
    version_array.set(4, 444, A10, A11),
    A2 = int.fold_up(func(I, A) = ( A ^ elem(I) := 20 - I ), 0, 20, A11),
    version_array.set(5, 555, A2, A20),
    version_array.set(6, 666, A20, A22),
    % A21 forces an implicit rollback:
    A21 = int.fold_up(make_a21, 0, 20, A1),
    io.write_string("ordering(A1, A0) = ", !IO),
    io.write(ordering(A1, A0), !IO),
    io.nl(!IO),
    io.write_string("ordering(A0, A1) = ", !IO),
    io.write(ordering(A0, A1), !IO),
    io.nl(!IO),
    io.write_string("ordering(A1, A2) = ", !IO),
    io.write(ordering(A1, A2), !IO),
    io.nl(!IO),
    io.write_string("ordering(A2, A1) = ", !IO),
    io.write(ordering(A2, A1), !IO),
    io.nl(!IO),
    write_array("A0", A0, !IO),
    write_array("A1", A1, !IO),
    write_array("A2", A2, !IO),
    write_array("A22", A22, !IO),
    write_array("A21", A21, !IO),
    A3 = unsafe_rewind(A1),
    write_array("A3", A3, !IO),
    A4 = version_array.init(7, 7),
    write_array("A4", A4, !IO),
    S4 = foldl(func(X, A) = X + A, A4, 0),
    io.format(" (sum %d)\n", [i(S4)], !IO),
    A5 = resize(A4, 4, 4),
    write_array("A5", A5, !IO),
    A6 = resize(A5, 9, 9),
    write_array("A6", A6, !IO),
    S6 = foldl(func(X, A) = X + A, A6, 0),
    io.format(" (sum %d)\n", [i(S6)], !IO),
    A7 = copy(A5),
    write_array("A7", A7, !IO),

    test_exception(
        ((pred) is semidet :-
            _ = A7 ^ elem(-1)
        ), !IO),
    test_exception(
        ((pred) is semidet :-
            _ : version_array(int) = A7 ^ elem(-1) := 2
        ), !IO),
    _ = A7 ^ elem(1) := 1,
    test_exception(
        ((pred) is semidet :-
            _ = A7 ^ elem(-1) := 2
        ), !IO),
    test_exception(
        ((pred) is semidet :-
            _ = A7 ^ elem(-1)
        ), !IO),
    test_exception(
        ((pred) is semidet :-
            _ = A7 ^ elem(4)
        ), !IO),

    A8 = version_array([2, 4, 6, 8]),
    io.write_string("all_true(int.even A8): ", !IO),
    ( if version_array.all_true(int.even, A8) then
        io.write_string("passed\n", !IO)
    else
        io.write_string("failed\n", !IO)
    ),

    io.write_string("all_false(int.odd, A8): ", !IO),
    ( if version_array.all_false(int.odd, A8) then
        io.write_string("passed\n", !IO)
    else
        io.write_string("failed\n", !IO)
    ),

    io.write_string("all_true(int.even, A0): ", !IO),
    ( if version_array.all_true(int.even, A0) then
        io.write_string("passed\n", !IO)
    else
        io.write_string("failed\n", !IO)
    ),

    io.write_string("all_false(int.even, A0): ", !IO),
    ( if version_array.all_false(int.even, A0) then
        io.write_string("passed\n", !IO)
    else
        io.write_string("failed\n", !IO)
    ),

    io.write_string("not all_true(int.odd, A8): ", !IO),
    ( if not version_array.all_true(int.odd, A8) then
        io.write_string("passed\n", !IO)
    else
        io.write_string("failed\n", !IO)
    ),

    io.write_string("not all_false(int.even, A8): ", !IO),
    ( if not version_array.all_false(int.even, A8) then
        io.write_string("passed\n", !IO)
    else
        io.write_string("failed\n", !IO)
    ),

    FromList1 = version_array.from_list([] : list(int)),
    write_array("from_list([])", FromList1, !IO),
    FromList2 = version_array.from_list([1]),
    write_array("from_list([1])", FromList2, !IO),
    FromList3 = version_array.from_list([1, 2]),
    write_array("from_list([1, 2])", FromList3, !IO),

    FromRevList1 = version_array.from_reverse_list([] : list(int)),
    write_array("from_reverse_list([])", FromRevList1, !IO),
    FromRevList2 = version_array.from_reverse_list([1]),
    write_array("from_reverse_list([1])", FromRevList2, !IO),
    FromRevList3 = version_array.from_reverse_list([2, 1]),
    write_array("from_reverse_list([2, 1])", FromRevList3, !IO).

:- pred test_exception((pred)::in((pred) is semidet),
    io::di, io::uo) is cc_multi.

test_exception(Pred, !IO) :-
    try((pred({}::out) is semidet :- Pred), Result),
    (
        Result = succeeded(_),
        io.write_string("Error: test succeeded, expected exception\n", !IO)
    ;
        Result = failed,
        io.write_string("Error: test failed, expected exception\n", !IO)
    ;
        Result = exception(Exception),
        io.write_string("Found exception as expected: ", !IO),
        io.write(univ_value(Exception), !IO),
        io.nl(!IO)
    ).

:- func make_a21(int, version_array(int)) = version_array(int).

make_a21(I, A0) = A :-
    X = A0 ^ elem(I),
    ( if X `mod` 2 = 0 then
        A = A0 ^ elem(I) := X * 3
    else
        A = A0
    ).

:- pred write_array(string::in, version_array(int)::in, io::di, io::uo) is det.

write_array(Name, Array, !IO) :-
    Size = size(Array),
    ArrayStr = string(to_list(Array)),
    io.format("%s: %s (size %d)\n", [s(Name), s(ArrayStr), i(Size)], !IO).

%---------------------------------------------------------------------------%
