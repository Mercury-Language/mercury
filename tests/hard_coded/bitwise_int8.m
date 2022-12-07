%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

% Test bitwise operations for signed 8-bit integers.

:- module bitwise_int8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int8.

:- import_module exception.
:- import_module list.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_unop_test(int8.(\), "\\", !IO),
    run_binop_test(int8.(/\), "/\\", !IO),
    run_binop_test(int8.(\/), "\\/", !IO),
    run_binop_test((func(X, Y) = int8.xor(X, Y)), "xor", !IO),
    run_shift_test(int8.(>>), ">>", int8.(>>u), ">>u", !IO),
    run_shift_test(int8.(<<), "<<", int8.(<<u), "<<u", !IO).

%---------------------------------------------------------------------------%

:- pred run_unop_test((func(int8) = int8)::in, string::in,
    io::di, io::uo) is cc_multi.

run_unop_test(UnOpFunc, Desc, !IO) :-
    io.format("*** Test unary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_unop_test_2(UnOpFunc, Desc), As, !IO).

:- pred run_unop_test_2((func(int8) = int8)::in, string::in,
    int8::in, io::di, io::uo) is cc_multi.

run_unop_test_2(UnOpFunc, Desc, A, !IO) :-
    ( try []
        Result0 = UnOpFunc(A)
    then
        ResultStr = to_binary_string_lz(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s =\n  %s\n\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- pred run_binop_test((func(int8, int8) = int8)::in, string::in,
    io::di, io::uo) is cc_multi.

run_binop_test(BinOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_binop_test_2(BinOpFunc, Desc, Bs), As, !IO).

:- pred run_binop_test_2((func(int8, int8) = int8)::in, string::in,
    list(int8)::in, int8::in, io::di, io::uo) is cc_multi.

run_binop_test_2(BinOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_binop_test_3(BinOpFunc, Desc, A), Bs, !IO).

:- pred run_binop_test_3((func(int8, int8) = int8)::in, string::in,
    int8::in, int8::in, io::di, io::uo) is cc_multi.

run_binop_test_3(BinOpFunc, Desc, A, B, !IO) :-
    ( try []
        Result0 = BinOpFunc(A, B)
    then
        ResultStr = to_binary_string_lz(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s\n%s =\n%s\n\n",
        [s(to_binary_string_lz(A)), s(Desc),
        s(to_binary_string_lz(B)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- pred run_shift_test(
    (func(int8, int) = int8)::in, string::in,
    (func(int8, uint) = int8)::in, string::in,
    io::di, io::uo) is cc_multi.

run_shift_test(ShiftOpFunc, Desc, UShiftOpFunc, UDesc, !IO) :-
    io.format("*** Test shift operations '%s' and '%s' ***\n\n",
        [s(Desc), s(UDesc)], !IO),
    As = numbers,
    Bs = shift_amounts,
    list.foldl(run_shift_test_2(ShiftOpFunc, Desc, UShiftOpFunc, UDesc, Bs),
        As, !IO).

:- pred run_shift_test_2(
    (func(int8, int) = int8)::in, string::in,
    (func(int8, uint) = int8)::in, string::in,
    list(int)::in, int8::in, io::di, io::uo) is cc_multi.

run_shift_test_2(ShiftOpFunc, Desc, UShiftOpFunc, UDesc, Bs, A, !IO) :-
    list.foldl(run_shift_test_3(ShiftOpFunc, Desc, UShiftOpFunc, UDesc, A),
        Bs, !IO).

:- pred run_shift_test_3(
    (func(int8, int) = int8)::in, string::in,
    (func(int8, uint) = int8)::in, string::in,
    int8::in, int::in, io::di, io::uo) is cc_multi.

run_shift_test_3(ShiftOpFunc, Desc, UShiftOpFunc, UDesc,
        Number, Amount, !IO) :-
    NumberStr = to_binary_string_lz(Number),
    ( try []
        Result0 = ShiftOpFunc(Number, Amount)
    then
        ResultStr = to_binary_string_lz(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s %d =\n%s\n\n",
        [s(NumberStr), s(Desc), i(Amount), s(ResultStr)], !IO),
    ( if uint.from_int(Amount, UAmount) then
        ( try []
            UResult0 = UShiftOpFunc(Number, UAmount)
        then
            UResultStr = to_binary_string_lz(UResult0)
        catch_any _ ->
            UResultStr = "<<exception>>"
        ),
        ( if UResultStr = ResultStr then
            true
        else
            io.format("%s vs %s difference:\n",
                [s(Desc), s(UDesc)], !IO),
            io.format("%s %s %u =\n%s\n\n",
                [s(NumberStr), s(UDesc), u(UAmount), s(UResultStr)], !IO)
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

:- func numbers = list(int8).

numbers = [
    -128_i8,
    0_i8,
    1_i8,
    2_i8,
    8_i8,
    10_i8,
    16_i8,
    127_i8
].

:- func shift_amounts = list(int).

shift_amounts = [
    -1,
    0,
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    16,
    17
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
:- end_module bitwise_int8.
%---------------------------------------------------------------------------%
