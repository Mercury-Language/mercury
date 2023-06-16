%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module rotate_uint8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module exception.
:- import_module list.
:- import_module string.
:- import_module uint8.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_test(rotate_left, unchecked_rotate_left, "rotate_left", !IO),
    run_test(rotate_right,unchecked_rotate_right,  "rotate_right", !IO).

%---------------------------------------------------------------------------%

:- pred run_test((func(uint8, uint) = uint8)::in,
    (func(uint8, uint) = uint8)::in,
    string::in, io::di, io::uo) is cc_multi.

run_test(CheckedFunc, UncheckedFunc, Desc, !IO) :-
    io.format("*** Test '%s' ***\n\n", [s(Desc)], !IO),
    Ns = numbers,
    Ds = distances,
    list.foldl(run_test_2(CheckedFunc, UncheckedFunc, Desc, Ds), Ns, !IO).

:- pred run_test_2((func(uint8, uint) = uint8)::in,
    (func(uint8, uint) = uint8)::in, string::in,
    list(uint)::in, uint8::in, io::di, io::uo) is cc_multi.

run_test_2(CheckedFunc, UncheckedFunc, Desc, Ds, N, !IO) :-
    list.foldl(run_test_3(CheckedFunc, UncheckedFunc, Desc, N), Ds, !IO).

:- pred run_test_3((func(uint8, uint) = uint8)::in,
    (func(uint8, uint) = uint8)::in, string::in, uint8::in,
    uint::in, io::di, io::uo) is cc_multi.

run_test_3(CheckedFunc, UncheckedFunc, Desc, N, D, !IO) :-
    do_eval(CheckedFunc, N, D, CheckedResult),
    io.format("          %s(%s, %u) = %s\n",
        [s(Desc), s(to_binary_string_lz(N)), u(D), s(CheckedResult)], !IO),
    do_eval(UncheckedFunc, N, D, UncheckedResult),
    io.format("unchecked_%s(%s, %u) = %s\n",
        [s(Desc), s(to_binary_string_lz(N)), u(D), s(UncheckedResult)], !IO),
    io.nl(!IO).

:- pred do_eval((func(uint8, uint) = uint8)::in, uint8::in, uint::in,
    string::out) is cc_multi.

do_eval(Func, N, D, ResultStr) :-
    ( try []
        Result0 = Func(N, D)
    then
        ResultStr = to_binary_string_lz(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ).

%---------------------------------------------------------------------------%

:- func numbers = list(uint8).

numbers = [
    1_u8,
    15_u8
].

:- func distances = list(uint).

distances = [
    0_u,
    1_u,
    2_u,
    7_u,
    8_u,
    15_u,
    16_u
].

%---------------------------------------------------------------------------%

:- func to_binary_string_lz(uint8::in) = (string::uo) is det.

:- pragma foreign_proc("C",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i;

    MR_allocate_aligned_string_msg(S, 8, MR_ALLOC_ID);
    S[8] = '\\0';
    for (i = 7; i >= 0; i--) {
        S[i] = (U & 1) ? '1' : '0';
        U = U >> 1;
    }
").

:- pragma foreign_proc("C#",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = System.Convert.ToString(U, 2).PadLeft(8, '0');
").

:- pragma foreign_proc("Java",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.String.format(""%8s"",
        java.lang.Integer.toBinaryString(U & 0xff)).replace(' ', '0');
").

%---------------------------------------------------------------------------%
:- end_module rotate_uint8.
%---------------------------------------------------------------------------%
