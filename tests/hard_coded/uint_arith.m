%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Test basic arithmetic operations for unsigned unsigned integers.
%
% The .exp file is for when uint is 64-bit.
% The .exp2 file is for when uint is 32-bit.
%
%---------------------------------------------------------------------------%

:- module uint_arith.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module list.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_binop_test((func(X, Y) = X + Y), "+", !IO),
    io.nl(!IO),
    run_binop_test((func(X, Y) = X - Y), "-", !IO),
    io.nl(!IO),
    run_binop_test((func(X, Y) = X * Y), "*", !IO),
    io.nl(!IO),
    run_binop_test(uint.(/), "/", !IO),
    io.nl(!IO),
    run_binop_test(uint.(rem), "rem", !IO).

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
        ResultStr = to_string(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s %s = %s\n",
        [s(to_string(A)), s(Desc), s(to_string(B)), s(ResultStr)], !IO).

:- func numbers = list(uint).

numbers = [
    0u,
    1u,
    2u,
    8u,
    10u,
    16u,
    32u,
    uint.max_uint
].

:- func to_string(uint) = string.

to_string(U) = uint_to_string(U) ++ "u".

%---------------------------------------------------------------------------%
:- end_module uint_arith.
%---------------------------------------------------------------------------%
