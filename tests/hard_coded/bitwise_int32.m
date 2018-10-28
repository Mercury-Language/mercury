%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test bitwise operations for signed 32-bit integers.

:- module bitwise_int32.
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
    run_unop_test(int32.(\), "\\", !IO),
    io.nl(!IO),
    run_binop_test(int32.(/\), "/\\", !IO),
    io.nl(!IO),
    run_binop_test(int32.(\/), "\\/", !IO),
    io.nl(!IO),
    run_binop_test((func(X, Y) = int32.xor(X, Y)), "xor", !IO),
    io.nl(!IO),
    run_shift_test(int32.(>>), ">>", !IO),
    io.nl(!IO),
    run_shift_test(int32.(<<), "<<", !IO).

%---------------------------------------------------------------------------%

:- pred run_unop_test((func(int32) = int32)::in, string::in,
    io::di, io::uo) is cc_multi.

run_unop_test(UnOpFunc, Desc, !IO) :-
    io.format("*** Test unary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_unop_test_2(UnOpFunc, Desc), As, !IO).

:- pred run_unop_test_2((func(int32) = int32)::in, string::in,
    int32::in, io::di, io::uo) is cc_multi.

run_unop_test_2(UnOpFunc, Desc, A, !IO) :-
    ( try []
        Result0 = UnOpFunc(A)
    then
        ResultStr = to_binary_string_lz(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s =\n  %s\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO),
    io.nl(!IO).

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
        ResultStr = to_binary_string_lz(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s\n%s =\n%s\n",
        [s(to_binary_string_lz(A)), s(Desc),
        s(to_binary_string_lz(B)), s(ResultStr)], !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred run_shift_test((func(int32, int) = int32)::in, string::in,
    io::di, io::uo) is cc_multi.

run_shift_test(ShiftOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = shift_amounts,
    list.foldl(run_shift_test_2(ShiftOpFunc, Desc, Bs), As, !IO).

:- pred run_shift_test_2((func(int32, int) = int32)::in, string::in,
    list(int)::in, int32::in, io::di, io::uo) is cc_multi.

run_shift_test_2(ShiftOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_shift_test_3(ShiftOpFunc, Desc, A), Bs, !IO).

:- pred run_shift_test_3((func(int32, int) = int32)::in, string::in,
    int32::in, int::in, io::di, io::uo) is cc_multi.

run_shift_test_3(ShiftOpFunc, Desc, A, B, !IO) :-
    ( try []
        Result0 = ShiftOpFunc(A, B)
    then
        ResultStr = to_binary_string_lz(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s %d =\n%s\n",
        [s(to_binary_string_lz(A)), s(Desc), i(B), s(ResultStr)], !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

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
    16_i32,
    127_i32,
    32_767_i32,
    2_147_483_647_i32
].

:- func shift_amounts = list(int).

shift_amounts = [
    -1,
    0,
    1,
    2,
    3,
    4,
    8,
    16,
    24,
    31,
    32,
    36
].

%---------------------------------------------------------------------------%

:- func to_binary_string_lz(int32::in) = (string::uo) is det.

:- pragma foreign_proc("C",
    to_binary_string_lz(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i = 32;
    uint32_t U = I;

    MR_allocate_aligned_string_msg(S, 32, MR_ALLOC_ID);
    S[32] = '\\0';
    while (i > 0) {
        i--;
        S[i] = (U & 1) ? '1' : '0';
        U = U >> 1;
    }
").

:- pragma foreign_proc("C#",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = System.Convert.ToString(U, 2).PadLeft(32, '0');
").

:- pragma foreign_proc("Java",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.String.format(""%32s"",
        java.lang.Integer.toBinaryString(U)).replace(' ', '0');
").




%---------------------------------------------------------------------------%
:- end_module bitwise_int32.
%---------------------------------------------------------------------------%
