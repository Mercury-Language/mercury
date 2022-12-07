%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test bitwise operations for unsigned integers.
%
% The .exp file is for grades where uint is 64-bit.
% The .exp2 file is for grades where uint is 32-bit.
%
%---------------------------------------------------------------------------%

:- module bitwise_uint.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module uint.

:- import_module exception.
:- import_module list.
:- import_module require.
:- import_module string.

:- pragma foreign_import_module("C", uint). % For ML_BITS_PER_UINT.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_unop_test(uint.(\), "\\", !IO),
    run_binop_test(uint.(/\), "/\\", !IO),
    run_binop_test(uint.(\/), "\\/", !IO),
    run_binop_test(uint_xor_proxy, "xor", !IO),
    run_shift_test(uint.(>>), ">>", uint.(>>u), ">>u", !IO),
    run_shift_test(uint.(<<), "<<", uint.(<<u), "<<u", !IO).

:- func uint_xor_proxy(uint, uint) = uint.

uint_xor_proxy(A, B) = uint.xor(A, B).

%---------------------------------------------------------------------------%

:- pred run_unop_test((func(uint) = uint)::in, string::in,
    io::di, io::uo) is cc_multi.

run_unop_test(UnOpFunc, Desc, !IO) :-
    io.format("*** Test unary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_unop_test_2(UnOpFunc, Desc), As, !IO).

:- pred run_unop_test_2((func(uint) = uint)::in, string::in,
    uint::in, io::di, io::uo) is cc_multi.

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

:- pred run_binop_test((func(uint, uint) = uint)::in, string::in,
    io::di, io::uo) is cc_multi.

run_binop_test(BinOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_binop_test_2(BinOpFunc, Desc, Bs), As, !IO).

:- pred run_binop_test_2((func(uint, uint) = uint)::in, string::in,
    list(uint)::in, uint::in, io::di, io::uo) is cc_multi.

run_binop_test_2(BinOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_binop_test_3(BinOpFunc, Desc, A), Bs, !IO).

:- pred run_binop_test_3((func(uint, uint) = uint)::in, string::in,
    uint::in, uint::in, io::di, io::uo) is cc_multi.

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
    (func(uint, int) = uint)::in, string::in,
    (func(uint, uint) = uint)::in, string::in,
    io::di, io::uo) is cc_multi.

run_shift_test(ShiftOpFunc, Desc, UShiftOpFunc, UDesc, !IO) :-
    io.format("*** Test shift operations '%s' and '%s' ***\n\n",
        [s(Desc), s(UDesc)], !IO),
    As = numbers,
    Bs = shift_amounts,
    list.foldl(run_shift_test_2(ShiftOpFunc, Desc, UShiftOpFunc, UDesc, Bs),
        As, !IO).

:- pred run_shift_test_2(
    (func(uint, int) = uint)::in, string::in,
    (func(uint, uint) = uint)::in, string::in,
    list(int)::in, uint::in, io::di, io::uo) is cc_multi.

run_shift_test_2(ShiftOpFunc, Desc, UShiftOpFunc, UDesc, Bs, A, !IO) :-
    list.foldl(run_shift_test_3(ShiftOpFunc, Desc, UShiftOpFunc, UDesc, A),
        Bs, !IO).

:- pred run_shift_test_3(
    (func(uint, int) = uint)::in, string::in,
    (func(uint, uint) = uint)::in, string::in,
    uint::in, int::in, io::di, io::uo) is cc_multi.

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

:- func numbers = list(uint).

numbers = [
    0u,
    1u,
    2u,
    8u,
    10u,
    16u,
    max_uint
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
    bits_per_uint - 1,
    bits_per_uint
].

%---------------------------------------------------------------------------%

:- func to_binary_string_lz(uint::in) = (string::uo) is det.

:- pragma foreign_proc("C",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i = ML_BITS_PER_UINT;

    MR_allocate_aligned_string_msg(S, ML_BITS_PER_UINT, MR_ALLOC_ID);
    S[ML_BITS_PER_UINT] = '\\0';
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
:- end_module bitwise_uint.
%---------------------------------------------------------------------------%
