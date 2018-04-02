%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test bit twiddling operations for signed 8-bit integers.

:- module bit_twiddle_int8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int8.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_twiddle_test(int8.num_zeros, "num_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(int8.num_ones, "num_ones", !IO),
    io.nl(!IO),
    run_twiddle_test(int8.num_leading_zeros, "num_leading_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(int8.num_trailing_zeros, "num_trailing_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test_b(int8.reverse_bits, "reverse_bits", !IO).

%---------------------------------------------------------------------------%

:- pred run_twiddle_test((func(int8) = int)::in, string::in,
    io::di, io::uo) is det.

run_twiddle_test(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_2((func(int8) = int)::in, string::in,
    int8::in, io::di, io::uo) is det.

run_twiddle_test_2(Func, Desc, A, !IO) :-
    Result = Func(A),
    int_to_string(Result, ResultStr),
    io.format("%s(%s) = %s\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

% Test int8 -> int8 functions.

:- pred run_twiddle_test_b((func(int8) = int8)::in, string::in,
    io::di, io::uo) is det.

run_twiddle_test_b(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_b_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_b_2((func(int8) = int8)::in, string::in,
    int8::in, io::di, io::uo) is det.

run_twiddle_test_b_2(Func, Desc, A, !IO) :-
    Result = Func(A),
    ResultStr = to_binary_string_lz(Result),
    io.format("%s(%s) = %s\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(int8).

numbers = [
    -128_i8,
    -16_i8,
    -10_i8,
    -8_i8,
    -2_i8,
    -1_i8,
    0_i8,
    1_i8,
    2_i8,
    8_i8,
    10_i8,
    16_i8,
    127_i8
].

%---------------------------------------------------------------------------%

:- func to_binary_string_lz(int8::in) = (string::uo) is det.

:- pragma foreign_proc("C",
    to_binary_string_lz(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i = 8;
    uint8_t U = I;

    MR_allocate_aligned_string_msg(S, 8, MR_ALLOC_ID);
    S[8] = '\\0';
    for (i = 7; i >=0; i--) {
        S[i] = (U & 1) ? '1' : '0';
        U = U >> 1;
    }
").

:- pragma foreign_proc("C#",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = System.Convert.ToString((byte)U, 2).PadLeft(8, '0');
").

:- pragma foreign_proc("Java",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.String.format(""%8s"",
        java.lang.Integer.toBinaryString(U & 0xff)).replace(' ', '0');
").

%---------------------------------------------------------------------------%
:- end_module bit_twiddle_int8.
%---------------------------------------------------------------------------%
