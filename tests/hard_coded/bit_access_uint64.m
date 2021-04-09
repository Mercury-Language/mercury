%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test look ups and setting of individual bits for uint64s.

:- module bit_access_uint64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint64.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_modify_test(uint64.set_bit, "set_bit", !IO),
    io.nl(!IO),
    run_modify_test(uint64.clear_bit, "clear_bit", !IO),
    io.nl(!IO),
    run_modify_test(uint64.flip_bit, "flip_bit", !IO),
    io.nl(!IO),
    run_value_test(uint64.bit_is_set, "bit_is_set", !IO),
    io.nl(!IO),
    run_value_test(uint64.bit_is_clear, "bit_is_clear", !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred run_modify_test((func(uint64, uint) = uint64)::in, string::in,
    io::di, io::uo) is cc_multi.

run_modify_test(Func, Desc, !IO) :-
    io.format("*** Test operation '%s' ***\n\n", [s(Desc)], !IO),
    list.foldl(run_modify_on_input(Func, Desc), modify_numbers, !IO).

:- pred run_modify_on_input((func(uint64, uint) = uint64)::in, string::in,
    uint64::in, io::di, io::uo) is cc_multi.

run_modify_on_input(Func, Desc, U, !IO) :-
    list.foldl(run_modify_on_input_at_index(Func, Desc, U), bit_indexes, !IO).

:- pred run_modify_on_input_at_index((func(uint64, uint) = uint64)::in, string::in,
    uint64::in, uint::in, io::di, io::uo) is cc_multi.

run_modify_on_input_at_index(Func, Desc, U, I, !IO) :-
    ( try []
        Result0 = Func(U, I)
    then
        ResultStr = to_binary_string_lz(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s(%s, %-2u) = %s\n",
        [s(Desc),
        s(to_binary_string_lz(U)),
        u(I),
        s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- pred run_value_test(pred(uint64, uint)::in(pred(in, in) is semidet),
    string::in, io::di, io::uo) is cc_multi.

run_value_test(Pred, Desc, !IO) :-
    io.format("*** Test operation '%s' ***\n\n", [s(Desc)], !IO),
    list.foldl(run_value_on_input(Pred, Desc), test_numbers, !IO).

:- pred run_value_on_input(pred(uint64, uint)::in(pred(in, in) is semidet),
    string::in, uint64::in, io::di, io::uo) is cc_multi.

run_value_on_input(Pred, Desc, U, !IO) :-
    list.foldl(run_value_on_input_at_index(Pred, Desc, U), bit_indexes, !IO).

:- pred run_value_on_input_at_index(pred(uint64, uint)::in(pred(in, in) is semidet),
    string::in, uint64::in, uint::in, io::di, io::uo) is cc_multi.

run_value_on_input_at_index(Pred, Desc, U, I, !IO) :-
    ( try []
        Pred(U, I)
    then
        ResultStr = "true"
    else
        ResultStr = "false"
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s(%s, %-2u) ==> %s\n",
        [s(Desc),
        s(to_binary_string_lz(U)),
        u(I),
        s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func modify_numbers = list(uint64).

modify_numbers = [
   0_u64,
   18_446_744_073_709_551_615_u64
].

:- func test_numbers = list(uint64).

test_numbers = [
    0_u64,
    1_u64,
    2_u64,
    8_u64,
    255_u64,
    4_294_967_295_u64,
    18_446_744_073_709_551_615_u64
].

:- func bit_indexes = list(uint).

bit_indexes = [
   0_u,
   1_u,
   2_u,
   31_u,
   63_u,
   64_u
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
:- end_module bit_access_uint64.
%---------------------------------------------------------------------------%

