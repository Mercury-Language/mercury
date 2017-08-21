%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test arithmetic operations for unsigned 8-bit integers.

:- module arith_uint8.
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
    run_binop_test((func(X, Y) = X + Y), "+", !IO),
    io.nl(!IO),
    run_binop_test((func(X, Y) = X - Y), "-", !IO),
    io.nl(!IO),
    run_binop_test((func(X, Y) = X * Y), "*", !IO),
    io.nl(!IO),
    run_binop_test((func(X, Y) = X / Y), "/", !IO),
    io.nl(!IO),
    run_binop_test((func(X, Y) = X rem Y), "rem", !IO).

:- pred run_binop_test((func(uint8, uint8) = uint8)::in, string::in,
    io::di, io::uo) is cc_multi.

run_binop_test(BinOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_binop_test_2(BinOpFunc, Desc, Bs), As, !IO).

:- pred run_binop_test_2((func(uint8, uint8) = uint8)::in, string::in,
    list(uint8)::in, uint8::in, io::di, io::uo) is cc_multi.

run_binop_test_2(BinOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_binop_test_3(BinOpFunc, Desc, A), Bs, !IO).

:- pred run_binop_test_3((func(uint8, uint8) = uint8)::in, string::in,
    uint8::in, uint8::in, io::di, io::uo) is cc_multi.

run_binop_test_3(BinOpFunc, Desc, A, B, !IO) :-
    ( try []
        Result0 = BinOpFunc(A, B)
    then
        ResultStr = uint8_to_string(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s %s = %s\n",
        [s(uint8_to_string(A)), s(Desc), s(uint8_to_string(B)), s(ResultStr)], !IO).

:- func numbers = list(uint8).

numbers = [
    0u8,
    1u8,
    2u8,
    8u8,
    10u8,
    16u8,
    255u8
].

%---------------------------------------------------------------------------%
:- end_module arith_uint8.
%---------------------------------------------------------------------------%
