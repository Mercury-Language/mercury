%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test bit twiddling operations for unsigned 16-bit integers.

:- module bit_twiddle_uint16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint16.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_twiddle_test(uint16.num_zeros, "num_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(uint16.num_ones, "num_ones", !IO),
    io.nl(!IO),
    run_twiddle_test(uint16.num_leading_zeros, "num_leading_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(uint16.num_trailing_zeros, "num_trailing_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test_b(uint16.reverse_bits, "reverse_bits", !IO),
    io.nl(!IO),
    run_twiddle_test_b(uint16.reverse_bytes, "reverse_bytes", !IO).

%---------------------------------------------------------------------------%

% Test uint16 -> int functions.

:- pred run_twiddle_test((func(uint16) = int)::in, string::in,
    io::di, io::uo) is det.

run_twiddle_test(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_2((func(uint16) = int)::in, string::in,
    uint16::in, io::di, io::uo) is det.

run_twiddle_test_2(Func, Desc, A, !IO) :-
    Result = Func(A),
    int_to_string(Result, ResultStr),
    io.format("%s(%s) = %s\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

% Test uint16 -> uint16 functions.

:- pred run_twiddle_test_b((func(uint16) = uint16)::in, string::in,
    io::di, io::uo) is det.

run_twiddle_test_b(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_b_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_b_2((func(uint16) = uint16)::in, string::in,
    uint16::in, io::di, io::uo) is det.

run_twiddle_test_b_2(Func, Desc, A, !IO) :-
    Result = Func(A),
    ResultStr = to_binary_string_lz(Result),
    io.format("%s(%s) = %s\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(uint16).

numbers = [
    0_u16,
    1_u16,
    2_u16,
    8_u16,
    16_u16,
    255_u16,
    65_535_u16
].

%---------------------------------------------------------------------------%

:- func to_binary_string_lz(uint16::in) = (string::uo) is det.

:- pragma foreign_proc("C",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i = 16;

    MR_allocate_aligned_string_msg(S, 16, MR_ALLOC_ID);
    S[16] = '\\0';
    while (i >= 0) {
        i--;
        S[i] = (U & 1) ? '1' : '0';
        U = U >> 1;
    }
").

:- pragma foreign_proc("C#",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = System.Convert.ToString(U, 2).PadLeft(16, '0');
").

:- pragma foreign_proc("Java",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.String.format(""%16s"",
        java.lang.Integer.toBinaryString(U & 0xffff)).replace(' ', '0');
").

%---------------------------------------------------------------------------%
:- end_module bit_twiddle_uint16.
%---------------------------------------------------------------------------%
