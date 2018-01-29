%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test arithmetic operations for signed 64-bit integers.

:- module arith_int64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int64.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_unop_test(int64.(+), "+", !IO),
    io.nl(!IO),
    run_unop_test(int64.(-), "-", !IO),
    io.nl(!IO),
    run_unop_test(int64.abs, "abs", !IO),
    io.nl(!IO),
    run_unop_test(int64.nabs, "nabs", !IO),
    io.nl(!IO),
    run_binop_test((func(X, Y) = X + Y), "+", !IO),
    io.nl(!IO),
    run_binop_test((func(X, Y) = X - Y), "-", !IO),
    io.nl(!IO),
    run_binop_test((func(X, Y) = X * Y), "*", !IO),
    io.nl(!IO),
    run_binop_test((func(X, Y) = X / Y), "/", !IO),
    io.nl(!IO),
    run_binop_test((func(X, Y) = X rem Y), "rem", !IO).

%---------------------------------------------------------------------------%

:- pred run_unop_test((func(int64) = int64)::in, string::in,
    io::di, io::uo) is cc_multi.

run_unop_test(UnOpFunc, Desc, !IO) :-
    io.format("*** Test unary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_unop_test_2(UnOpFunc, Desc), As, !IO).

:- pred run_unop_test_2((func(int64) = int64)::in, string::in, int64::in,
    io::di, io::uo) is cc_multi.

run_unop_test_2(UnOpFunc, Desc, A, !IO) :-
    ( try []
        Result0 = UnOpFunc(A)
    then
        ResultStr = int64_to_string(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s = %s\n",
        [s(Desc), s(int64_to_string(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- pred run_binop_test((func(int64, int64) = int64)::in, string::in,
    io::di, io::uo) is cc_multi.

run_binop_test(BinOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_binop_test_2(BinOpFunc, Desc, Bs), As, !IO).

:- pred run_binop_test_2((func(int64, int64) = int64)::in, string::in,
    list(int64)::in, int64::in, io::di, io::uo) is cc_multi.

run_binop_test_2(BinOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_binop_test_3(BinOpFunc, Desc, A), Bs, !IO).

:- pred run_binop_test_3((func(int64, int64) = int64)::in, string::in,
    int64::in, int64::in, io::di, io::uo) is cc_multi.

run_binop_test_3(BinOpFunc, Desc, A, B, !IO) :-
    ( try []
        Result0 = BinOpFunc(A, B)
    then
        ResultStr = int64_to_string(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s %s = %s\n",
        [s(int64_to_string(A)), s(Desc), s(int64_to_string(B)), s(ResultStr)],
        !IO).

:- func numbers = list(int64).

numbers = [
    -9_223_372_036_854_775_808_i64,
    -2_147_483_648_i64,
    -64_768_i64,
    -128_i64,
    0_i64,
    1_i64,
    2_i64,
    8_i64,
    10_i64,
    16_i64,
    127_i64,
    64_767_i64,
    2_147_483_647_i64,
    9_223_372_036_854_775_807_i64
].

%---------------------------------------------------------------------------%
:- end_module arith_int64.
%---------------------------------------------------------------------------%
