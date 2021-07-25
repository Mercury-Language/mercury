%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% base_string_to_int_test.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Thu Jul  1 16:12:57 EST 2004
%
%---------------------------------------------------------------------------%

:- module base_string_to_int_test.

:- interface.

:- import_module io.

:- pred main(io :: di, io :: uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test("123", !IO),
    test("+123", !IO),
    test("-123", !IO),
    test("", !IO),
    test("+", !IO),
    test("-", !IO),
    test("123abc", !IO),
    test("abc", !IO),
    test("+abc", !IO),
    test("-abc", !IO),
    test("Σ", !IO),
    test("-Σ", !IO),
    test("+Σ", !IO),
    test("123Σ", !IO),
    ( if int.bits_per_int = 32 then
        MinIntStr = "-2147483648"
    else if int.bits_per_int = 64 then
        MinIntStr = "-9223372036854775808"
    else
        error("unknown architecture")
    ),
    ( if string.base_string_to_int(10, MinIntStr, int.min_int) then
        io.write_string("min_int ok.\n", !IO)
    else
        io.write_string("min_int failed.\n", !IO)
    ).

:- pred test(string::in, io::di, io::uo) is det.

test(S, !IO) :-
    io.format("string.base_string_to_int(10, \"%s\", ", [s(S)], !IO),
    ( if string.base_string_to_int(10, S, N) then
        io.format("%d).\n", [i(N)], !IO)
    else
        io.format("_) failed.\n", [], !IO)
    ).

%---------------------------------------------------------------------------%
