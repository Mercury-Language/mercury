%-----------------------------------------------------------------------------%
% version_array_test.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Fri Oct  8 15:00:25 EST 2004
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
%-----------------------------------------------------------------------------%

:- module version_array_test.

:- interface.

:- import_module io.



:- pred main(io :: di, io :: uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception, int, list, string, univ, version_array.

%-----------------------------------------------------------------------------%

main(!IO) :-
    A0 = version_array.empty,
    A1 = version_array(0`..`9),
    A2 = int.fold_up(func(I, A) = ( A ^ elem(I) := 9 - I ), 0, 9, A1),
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
    io.write_list(to_list(A0), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A0))], !IO),
    io.write_list(to_list(A1), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A1))], !IO),
    io.write_list(to_list(A2), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A2))], !IO),
    A3 = unsafe_rewind(A1),
    io.write_list(to_list(A3), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A3))], !IO),
    A4 = version_array.init(7, 7),
    io.write_list(to_list(A4), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A4))], !IO),
    S4 = foldl(func(X, A) = X + A, A4, 0),
    io.format(" (sum %d)\n", [i(S4)], !IO),
    A5 = resize(A4, 4, 4),
    io.write_list(to_list(A5), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A5))], !IO),
    A6 = resize(A5, 9, 9),
    io.write_list(to_list(A6), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A6))], !IO),
    S6 = foldl(func(X, A) = X + A, A6, 0),
    io.format(" (sum %d)\n", [i(S6)], !IO),
    A7 = copy(A5),
    io.write_list(to_list(A7), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A7))], !IO),

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
    ( if version_array.all_true(int.even, A8)
    then io.write_string("passed\n", !IO)
    else io.write_string("failed\n", !IO)
    ),

    io.write_string("all_false(int.odd, A8): ", !IO),
    ( if version_array.all_false(int.odd, A8)
    then io.write_string("passed\n", !IO)
    else io.write_string("failed\n", !IO)
    ),

    io.write_string("all_true(int.even, A0): ", !IO),
    ( if version_array.all_true(int.even, A0)
    then io.write_string("passed\n", !IO)
    else io.write_string("failed\n", !IO)
    ),

    io.write_string("all_false(int.even, A0): ", !IO),
    ( if version_array.all_false(int.even, A0)
    then io.write_string("passed\n", !IO)
    else io.write_string("failed\n", !IO)
    ),

    io.write_string("not all_true(int.odd, A8): ", !IO),
    ( if not version_array.all_true(int.odd, A8)
    then io.write_string("passed\n", !IO)
    else io.write_string("failed\n", !IO)
    ),

    io.write_string("not all_false(int.even, A8): ", !IO),
    ( if not version_array.all_false(int.even, A8)
    then io.write_string("passed\n", !IO)
    else io.write_string("failed\n", !IO)
    ),

    true. 

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
