%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test bit twiddling operations for unsigned 32-bit integers.

:- module bit_twiddle_uint32.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint32.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_twiddle_test(uint32.num_zeros, "num_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(uint32.num_ones, "num_ones", !IO),
    io.nl(!IO),
    run_twiddle_test(uint32.num_leading_zeros, "num_leading_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(uint32.num_trailing_zeros, "num_trailing_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test_b(uint32.reverse_bits, "reverse_bits", !IO),
    io.nl(!IO),
    run_twiddle_test_b(uint32.reverse_bytes, "reverse_bytes", !IO).

%---------------------------------------------------------------------------%

% Test uint32 -> int functions.

:- pred run_twiddle_test((func(uint32) = int)::in, string::in,
    io::di, io::uo) is det.

run_twiddle_test(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_2((func(uint32) = int)::in, string::in,
    uint32::in, io::di, io::uo) is det.

run_twiddle_test_2(Func, Desc, A, !IO) :-
    Result0 = Func(A),
    int_to_string(Result0, ResultStr),
    io.format("%s(%s) = %s\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

% Test uint32 -> uint32 functions.

:- pred run_twiddle_test_b((func(uint32) = uint32)::in, string::in,
    io::di, io::uo) is det.

run_twiddle_test_b(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_b_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_b_2((func(uint32) = uint32)::in, string::in,
    uint32::in, io::di, io::uo) is det.

run_twiddle_test_b_2(Func, Desc, A, !IO) :-
    Result0 = Func(A),
    ResultStr = to_binary_string_lz(Result0),
    io.format("%s(%s) = %s\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(uint32).

numbers = [
    0_u32,
    1_u32,
    2_u32,
    8_u32,
    10_u32,
    16_u32,
    255_u32,
    65_535_u32,
    4_294_967_295_u32
].

%---------------------------------------------------------------------------%

:- func to_binary_string_lz(uint32::in) = (string::uo) is det.

:- pragma foreign_proc("C",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i;

    MR_allocate_aligned_string_msg(S, 32, MR_ALLOC_ID);
    S[32] = '\\0';
    for (i = 31; i >= 0; i--) {
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
:- end_module bit_twiddle_uint32.
%---------------------------------------------------------------------------%
