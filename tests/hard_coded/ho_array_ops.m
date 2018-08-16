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
    do_test("foldl (empty)", foldl_test([]), !IO),
    do_test("foldl (singleton)", foldl_test([1]), !IO),
    do_test("foldl (> 1)", foldl_test([1, 2, 3]), !IO),

    do_test("foldl2 (empty)", foldl2_test([]), !IO),
    do_test("foldl2 (singleton)", foldl2_test([1]), !IO),
    do_test("foldl2 (> 1)", foldl2_test([1, 2, 3]), !IO),

    do_test("foldr (empty)", foldr_test([]), !IO),
    do_test("foldr (singleton)", foldr_test([1]), !IO),
    do_test("foldr (> 1)", foldr_test([1, 2, 3]), !IO),

    do_test("foldr2 (empty)", foldr2_test([]), !IO),
    do_test("foldr2 (singleton)", foldr2_test([1]), !IO),
    do_test("foldr2 (> 1)", foldr2_test([1, 2, 3]), !IO),

    do_test("foldl_corresponding (ok)", foldl_corresponding_ok, !IO),
    do_test("foldl_corresponding (empty)", foldl_corresponding_empty, !IO),
    do_test("foldl_corresponding (mismatch)", foldl_corresponding_mismatch,
        !IO),

    do_test("foldl2_corresponding (ok)", foldl2_corresponding_ok, !IO),
    do_test("foldl2_corresponding (empty)", foldl2_corresponding_empty, !IO),
    do_test("foldl2_corresponding (mismatch)", foldl2_corresponding_mismatch,
        !IO),

    do_test("map (pred) (empty)", map_pred_test([]), !IO),
    do_test("map (pred) (singleton)", map_pred_test([1]), !IO),
    do_test("map (pred) (> 1)", map_pred_test([1, 2, 3]), !IO),

    do_test("map (func) (empty)", map_func_test([]), !IO),
    do_test("map (func) (singleton)", map_func_test([1]), !IO),
    do_test("map (func) (> 1)", map_func_test([1, 2, 3]), !IO),

    do_test("map_corresponding_foldl (ok)", map_corresponding_foldl_ok, !IO),
    do_test("map_corresponding_foldl (empty)", map_corresponding_foldl_empty,
        !IO),
    do_test("map_corresponding_foldl (mismatch)",
        map_corresponding_foldl_mismatch, !IO).

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
% Tests for foldl and foldr.
%

:- pred foldl_test(list(int)::in, io::di, io::uo) is det.

foldl_test(Elems, !IO) :-
    Array = array.from_list(Elems),
    array.foldl(write_elem, Array, !IO),
    io.nl(!IO).

:- pred foldr_test(list(int)::in, io::di, io::uo) is det.

foldr_test(Elems, !IO) :-
    Array = array.from_list(Elems),
    array.foldr(write_elem, Array, !IO),
    io.nl(!IO).

:- pred write_elem(int::in, io::di, io::uo) is det.

write_elem(X, !IO) :-
    io.write_int(X, !IO),
    io.write_string(" ", !IO).

%---------------------------------------------------------------------------%
%
% Tests for foldl2 and foldr2.
%

:- pred foldl2_test(list(int)::in, io::di, io::uo) is det.

foldl2_test(Elems, !IO) :-
    Array = array.from_list(Elems),
    array.foldl2(write_elem_and_sum, Array, 0, Sum, !IO),
    io.nl(!IO),
    io.format("The sum is %d\n", [i(Sum)], !IO).

:- pred foldr2_test(list(int)::in, io::di, io::uo) is det.

foldr2_test(Elems, !IO) :-
    Array = array.from_list(Elems),
    array.foldr2(write_elem_and_sum, Array, 0, Sum, !IO),
    io.nl(!IO),
    io.format("The sum is %d\n", [i(Sum)], !IO).

:- pred write_elem_and_sum(int::in, int::in, int::out, io::di, io::uo)
    is det.

write_elem_and_sum(X, !Sum, !IO) :-
    io.write_int(X, !IO),
    io.write_string(" ", !IO),
    !:Sum = !.Sum + X.

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

:- pred map_pred_test(list(int)::in, io::di, io::uo) is det.

map_pred_test(Elems, !IO) :-
    A = array.from_list(Elems),
    array.map(int_to_string, A, B),
    io.write_line(B, !IO).

:- pred map_func_test(list(int)::in, io::di, io::uo) is det.

map_func_test(Elems, !IO) :-
    A  = array.from_list(Elems),
    B : array(string) = array.map(int_to_string, A),
    io.write_line(B, !IO).

%---------------------------------------------------------------------------%

:- pred map_corresponding_foldl_ok(io::di, io::uo) is det.

map_corresponding_foldl_ok(!IO) :-
    A = array.from_list([1, 2, 3, 4, 5]),
    B = array.from_list([2, 4, 6, 8, 10]),
    array.map_corresponding_foldl(print_and_partial_sum, A, B, C, !IO),
    io.write_line(C, !IO),
    io.nl(!IO).

:- pred map_corresponding_foldl_empty(io::di, io::uo) is det.

map_corresponding_foldl_empty(!IO) :-
    make_empty_array(A),
    make_empty_array(B),
    array.map_corresponding_foldl(print_and_partial_sum, A, B, C, !IO),
    io.write_line(C, !IO),
    io.nl(!IO).

:- pred map_corresponding_foldl_mismatch(io::di, io::uo) is det.

map_corresponding_foldl_mismatch(!IO) :-
    A = array.from_list([1, 2, 3, 4, 5]),
    make_empty_array(B),
    array.map_corresponding_foldl(print_and_partial_sum, A, B, C, !IO),
    io.write_line(C, !IO),
    io.nl(!IO).

:- pred print_and_partial_sum(int::in, int::in, int::out, io::di, io::uo) is det.

print_and_partial_sum(A, B, C, !IO) :-
    C = A + B,
    io.format("%d + %d = %d\n", [i(A), i(B), i(C)], !IO).

%---------------------------------------------------------------------------%
:- end_module ho_array_ops.
%---------------------------------------------------------------------------%
