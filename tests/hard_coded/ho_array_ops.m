%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Tests of various higher-order array operations.
% XXX this should extended cover other ho array operations.
%
%---------------------------------------------------------------------------%

:- module ho_array_ops.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    do_test("foldl_corresponding (ok)", foldl_corresponding_ok, !IO),
    do_test("foldl_corresponding (empty)", foldl_corresponding_empty, !IO),
    do_test("foldl_corresponding (mismatch)", foldl_corresponding_mismatch,
        !IO),

    do_test("foldl2_corresponding (ok)", foldl2_corresponding_ok, !IO),
    do_test("foldl2_corresponding (empty)", foldl2_corresponding_empty, !IO),
    do_test("foldl2_corresponding (mismatch)", foldl2_corresponding_mismatch,
        !IO).

%---------------------------------------------------------------------------%

:- pred do_test(string::in, pred(io, io)::in(pred(di, uo) is det),
    io::di, io::uo) is cc_multi.

do_test(Desc, Pred, !IO) :-
    io.format("TESTING: %s\n", [s(Desc)], !IO),
    ( try [io(!IO)] (
        Pred(!IO)
    )
    then
        io.write_string("RESULT: OK\n", !IO)
    catch_any Excp ->
        io.write_string("RESULT: EXCEPTION: ", !IO),
        io.write_line(Excp, !IO)
    ),
    io.format("FINISHED TESTING: %s\n\n", [s(Desc)], !IO).

%---------------------------------------------------------------------------%
%
% Tests for array.foldl_corresponding/5.
%

:- pred foldl_corresponding_ok(io::di, io::uo) is det.

foldl_corresponding_ok(!IO) :-
    A = array.from_list([1, 2, 3, 4, 5]),
    B = array.from_list([2, 4, 6, 8, 10]),
    array.foldl_corresponding(print_corresponding, A, B, !IO).

:- pred foldl_corresponding_empty(io::di, io::uo) is det.

foldl_corresponding_empty(!IO) :-
    make_empty_array(A : array(int)),
    make_empty_array(B : array(int)),
    array.foldl_corresponding(print_corresponding, A, B, !IO).

:- pred foldl_corresponding_mismatch(io::di, io::uo) is det.

foldl_corresponding_mismatch(!IO) :-
    make_empty_array(A : array(int)),
    B = array.from_list([2, 4, 6, 8, 10]),
    array.foldl_corresponding(print_corresponding, A, B, !IO).

:- pred print_corresponding(int::in, int::in, io::di, io::uo) is det.

print_corresponding(A, B, !IO) :-
    io.format("%d - %d\n", [i(A), i(B)], !IO).

%---------------------------------------------------------------------------%

:- pred foldl2_corresponding_ok(io::di, io::uo) is det.

foldl2_corresponding_ok(!IO) :-
    A = array.from_list([1, 2, 3, 4, 5]),
    B = array.from_list([2, 4, 6, 8, 10]),
    array.foldl2_corresponding(print_and_sum_corresponding, A, B, 0, Sum,
        !IO),
    io.format("Sum is %d.\n", [i(Sum)], !IO).

:- pred foldl2_corresponding_empty(io::di, io::uo) is det.

foldl2_corresponding_empty(!IO) :-
    make_empty_array(A : array(int)),
    make_empty_array(B : array(int)),
    array.foldl2_corresponding(print_and_sum_corresponding, A, B, 0, Sum,
        !IO),
    io.format("Sum is %d.\n", [i(Sum)], !IO).

:- pred foldl2_corresponding_mismatch(io::di, io::uo) is det.

foldl2_corresponding_mismatch(!IO) :-
    make_empty_array(A : array(int)),
    B = array.from_list([2, 4, 6, 8, 10]),
    array.foldl2_corresponding(print_and_sum_corresponding, A, B, 0, Sum,
        !IO),
    io.format("Sum is %d.\n", [i(Sum)], !IO).

:- pred print_and_sum_corresponding(int::in, int::in, int::in, int::out,
    io::di, io::uo) is det.

print_and_sum_corresponding(A, B, !Sum, !IO) :-
    !:Sum = !.Sum + A + B,
    io.format("%d - %d\n", [i(A), i(B)], !IO).

%---------------------------------------------------------------------------%
:- end_module ho_array_ops.
%---------------------------------------------------------------------------%
