%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test bit twiddling operations for signed 64-bit integers.

:- module bit_twiddle_int64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int64.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_twiddle_test(int64.num_zeros, "num_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(int64.num_ones, "num_ones", !IO),
    io.nl(!IO),
    run_twiddle_test(int64.num_leading_zeros, "num_leading_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(int64.num_trailing_zeros, "num_trailing_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test_b(int64.reverse_bits, "reverse_bits", !IO),
    io.nl(!IO),
    run_twiddle_test_b(int64.reverse_bytes, "reverse_bytes", !IO).

%---------------------------------------------------------------------------%

:- pred run_twiddle_test((func(int64) = int)::in, string::in,
    io::di, io::uo) is det.

run_twiddle_test(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_2((func(int64) = int)::in, string::in,
    int64::in, io::di, io::uo) is det.

run_twiddle_test_2(Func, Desc, A, !IO) :-
    Result = Func(A),
    int_to_string(Result, ResultStr),
    io.format("%s(%s) = %s\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

% Test int64 -> int64 functions.

:- pred run_twiddle_test_b((func(int64) = int64)::in, string::in,
    io::di, io::uo) is det.

run_twiddle_test_b(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_b_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_b_2((func(int64) = int64)::in, string::in,
    int64::in, io::di, io::uo) is det.

run_twiddle_test_b_2(Func, Desc, A, !IO) :-
    Result = Func(A),
    ResultStr = to_binary_string_lz(Result),
    io.format("%s(%s) = %s\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(int64).

numbers = [
    -9_223_372_036_854_775_808_i64,
    -2_147_483_648_i64,
    -32_768_i64,
    -128_i64,
    0_i64,
    1_i64,
    2_i64,
    8_i64,
    10_i64,
    16_i64,
    127_i64,
    32_767_i64,
    2_147_483_647_i64,
    9_223_372_036_854_775_807_i64
].

%---------------------------------------------------------------------------%

:- func to_binary_string_lz(int64::in) = (string::uo) is det.

:- pragma foreign_proc("C",
    to_binary_string_lz(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i = 64;
    uint64_t U = I;

    MR_allocate_aligned_string_msg(S, 64, MR_ALLOC_ID);
    S[64] = '\\0';
    for (i = 63; i >= 0; i--) {
        S[i] = (U & 1) ? '1' : '0';
        U = U >> 1;
    }
").

:- pragma foreign_proc("C#",
    to_binary_string_lz(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = System.Convert.ToString(I, 2).PadLeft(64, '0');
").

:- pragma foreign_proc("Java",
    to_binary_string_lz(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.String.format(""%64s"",
        java.lang.Long.toBinaryString(I)).replace(' ', '0');
").

%---------------------------------------------------------------------------%
:- end_module bit_twiddle_int64.
%---------------------------------------------------------------------------%
