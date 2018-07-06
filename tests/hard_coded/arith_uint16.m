%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test arithmetic operations for unsigned 16-bit integers.

:- module arith_uint16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint16.

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

:- pred run_binop_test((func(uint16, uint16) = uint16)::in, string::in,
    io::di, io::uo) is cc_multi.

run_binop_test(BinOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_binop_test_2(BinOpFunc, Desc, Bs), As, !IO).

:- pred run_binop_test_2((func(uint16, uint16) = uint16)::in, string::in,
    list(uint16)::in, uint16::in, io::di, io::uo) is cc_multi.

run_binop_test_2(BinOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_binop_test_3(BinOpFunc, Desc, A), Bs, !IO).

:- pred run_binop_test_3((func(uint16, uint16) = uint16)::in, string::in,
    uint16::in, uint16::in, io::di, io::uo) is cc_multi.

run_binop_test_3(BinOpFunc, Desc, A, B, !IO) :-
    ( try []
        Result0 = BinOpFunc(A, B)
    then
        ResultStr = uint16_to_string(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s %s = %s\n",
        [s(uint16_to_string(A)), s(Desc), s(uint16_to_string(B)),
        s(ResultStr)], !IO).

:- func numbers = list(uint16).

numbers = [
    0_u16,
    1_u16,
    2_u16,
    8_u16,
    10_u16,
    16_u16,
    255_u16,
    65_535_u16
].

%---------------------------------------------------------------------------%
:- end_module arith_uint16.
%---------------------------------------------------------------------------%
