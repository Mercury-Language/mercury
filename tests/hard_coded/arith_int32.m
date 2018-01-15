%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test arithmetic operations for signed 32-bit integers.

:- module arith_int32.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int32.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_unop_test(int32.(+), "+", !IO),
    io.nl(!IO),
    run_unop_test(int32.(-), "-", !IO),
    io.nl(!IO),
    run_unop_test(int32.abs, "abs", !IO),
    io.nl(!IO),
    run_unop_test(int32.nabs, "nabs", !IO),
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

:- pred run_unop_test((func(int32) = int32)::in, string::in,
    io::di, io::uo) is cc_multi.

run_unop_test(UnOpFunc, Desc, !IO) :-
    io.format("*** Test unary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_unop_test_2(UnOpFunc, Desc), As, !IO).

:- pred run_unop_test_2((func(int32) = int32)::in, string::in, int32::in,
    io::di, io::uo) is cc_multi.

run_unop_test_2(UnOpFunc, Desc, A, !IO) :-
    ( try []
        Result0 = UnOpFunc(A)
    then
        ResultStr = int32_to_string(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s = %s\n",
        [s(Desc), s(int32_to_string(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- pred run_binop_test((func(int32, int32) = int32)::in, string::in,
    io::di, io::uo) is cc_multi.

run_binop_test(BinOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_binop_test_2(BinOpFunc, Desc, Bs), As, !IO).

:- pred run_binop_test_2((func(int32, int32) = int32)::in, string::in,
    list(int32)::in, int32::in, io::di, io::uo) is cc_multi.

run_binop_test_2(BinOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_binop_test_3(BinOpFunc, Desc, A), Bs, !IO).

:- pred run_binop_test_3((func(int32, int32) = int32)::in, string::in,
    int32::in, int32::in, io::di, io::uo) is cc_multi.

run_binop_test_3(BinOpFunc, Desc, A, B, !IO) :-
    ( try []
        Result0 = BinOpFunc(A, B)
    then
        ResultStr = int32_to_string(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s %s = %s\n",
        [s(int32_to_string(A)), s(Desc), s(int32_to_string(B)), s(ResultStr)],
        !IO).

:- func numbers = list(int32).

numbers = [
    -2_147_483_648_i32,
    -32_768_i32,
    -128_i32,
    0_i32,
    1_i32,
    2_i32,
    8_i32,
    10_i32,
    16_i32,
    127_i32,
    32_767_i32,
    2_147_483_647_i32
].

%---------------------------------------------------------------------------%
:- end_module arith_int32.
%---------------------------------------------------------------------------%
