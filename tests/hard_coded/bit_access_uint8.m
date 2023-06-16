%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test look ups and setting of individual bits for uint8s.

:- module bit_access_uint8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint8.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_modify_test(uint8.set_bit, "set_bit", !IO),
    io.nl(!IO),
    run_modify_test(uint8.clear_bit, "clear_bit", !IO),
    io.nl(!IO),
    run_modify_test(uint8.flip_bit, "flip_bit", !IO),
    io.nl(!IO),
    run_value_test(uint8.bit_is_set, "bit_is_set", !IO),
    io.nl(!IO),
    run_value_test(uint8.bit_is_clear, "bit_is_clear", !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred run_modify_test((func(uint8, uint) = uint8)::in,
    string::in, io::di, io::uo) is cc_multi.

run_modify_test(Func, Desc, !IO) :-
    io.format("*** Test operation '%s' ***\n\n", [s(Desc)], !IO),
    list.foldl(run_modify_on_input(Func, Desc), modify_numbers, !IO).

:- pred run_modify_on_input((func(uint8, uint) = uint8)::in,
    string::in, uint8::in, io::di, io::uo) is cc_multi.

run_modify_on_input(Func, Desc, U, !IO) :-
    list.foldl(run_modify_on_input_at_index(Func, Desc, U), bit_indexes, !IO).

:- pred run_modify_on_input_at_index((func(uint8, uint) = uint8)::in,
    string::in, uint8::in, uint::in, io::di, io::uo) is cc_multi.

run_modify_on_input_at_index(Func, Desc, U, I, !IO) :-
    ( try []
        Result0 = Func(U, I)
    then
        ResultStr = to_binary_string_lz(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s(%s, %u) = %s\n",
        [s(Desc),
        s(to_binary_string_lz(U)),
        u(I),
        s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- pred run_value_test(pred(uint8, uint)::in(pred(in, in) is semidet),
    string::in, io::di, io::uo) is cc_multi.

run_value_test(Pred, Desc, !IO) :-
    io.format("*** Test operation '%s' ***\n\n", [s(Desc)], !IO),
    list.foldl(run_value_on_input(Pred, Desc), test_numbers, !IO).

:- pred run_value_on_input(pred(uint8, uint)::in(pred(in, in) is semidet),
    string::in, uint8::in, io::di, io::uo) is cc_multi.

run_value_on_input(Pred, Desc, U, !IO) :-
    list.foldl(run_value_on_input_at_index(Pred, Desc, U), bit_indexes, !IO).

:- pred run_value_on_input_at_index(
    pred(uint8, uint)::in(pred(in, in) is semidet),
    string::in, uint8::in, uint::in, io::di, io::uo) is cc_multi.

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
    io.format("%s(%s, %u) ==> %s\n",
        [s(Desc),
        s(to_binary_string_lz(U)),
        u(I),
        s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func modify_numbers = list(uint8).

modify_numbers = [
   0_u8,
   255_u8
].

:- func test_numbers = list(uint8).

test_numbers = [
    0_u8,
    1_u8,
    2_u8,
    8_u8,
    127_u8,
    255_u8
].

:- func bit_indexes = list(uint).

bit_indexes = [
   0_u,
   1_u,
   2_u,
   3_u,
   4_u,
   5_u,
   6_u,
   7_u,
   8_u
].

%---------------------------------------------------------------------------%

:- func to_binary_string_lz(uint8::in) = (string::uo) is det.

:- pragma foreign_proc("C",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i = 8;

    MR_allocate_aligned_string_msg(S, 8, MR_ALLOC_ID);
    S[8] = '\\0';
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
:- end_module bit_access_uint8.
%---------------------------------------------------------------------------%

