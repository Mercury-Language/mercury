%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test bit twiddling operations for unsigned 64-bit integers.

:- module bit_twiddle_uint64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint64.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_twiddle_test(uint64.num_zeros, "num_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(uint64.num_ones, "num_ones", !IO),
    io.nl(!IO),
    run_twiddle_test(uint64.num_leading_zeros, "num_leading_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(uint64.num_trailing_zeros, "num_trailing_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test_b(uint64.reverse_bits, "reverse_bits", !IO),
    io.nl(!IO),
    run_twiddle_test_b(uint64.reverse_bytes, "reverse_bytes", !IO).

%---------------------------------------------------------------------------%

:- pred run_twiddle_test((func(uint64) = int)::in, string::in,
    io::di, io::uo) is det.

run_twiddle_test(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_2((func(uint64) = int)::in, string::in,
    uint64::in, io::di, io::uo) is det.

run_twiddle_test_2(Func, Desc, A, !IO) :-
    Result = Func(A),
    int_to_string(Result, ResultStr),
    io.format("%s(%s) = %s\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

% Test uint64 -> uint64 functions.

:- pred run_twiddle_test_b((func(uint64) = uint64)::in, string::in,
    io::di, io::uo) is det.

run_twiddle_test_b(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_b_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_b_2((func(uint64) = uint64)::in, string::in,
    uint64::in, io::di, io::uo) is det.

run_twiddle_test_b_2(Func, Desc, A, !IO) :-
    Result = Func(A),
    ResultStr = to_binary_string_lz(Result),
    io.format("%s(%s) = %s\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(uint64).

numbers = [
    0_u64,
    1_u64,
    2_u64,
    8_u64,
    10_u64,
    16_u64,
    255_u64,
    65_535_u64,
    4_294_967_295_u64,
    18_446_744_073_709_551_615_u64
].

%---------------------------------------------------------------------------%

:- func to_binary_string_lz(uint64::in) = (string::uo) is det.

:- pragma foreign_proc("C",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i = 64;

    MR_allocate_aligned_string_msg(S, 64, MR_ALLOC_ID);
    S[64] = '\\0';
    for (i = 63; i >= 0; i--) {
        S[i] = (U & 1) ? '1' : '0';
        U = U >> 1;
    }
").

:- pragma foreign_proc("C#",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = System.Convert.ToString((long)U, 2).PadLeft(64, '0');
").

:- pragma foreign_proc("Java",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.String.format(""%64s"",
        java.lang.Long.toBinaryString(U)).replace(' ', '0');
").

%---------------------------------------------------------------------------%
:- end_module bit_twiddle_uint64.
%---------------------------------------------------------------------------%
