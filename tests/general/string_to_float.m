%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% string_to_float.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Mon Feb 11 17:52:44 EST 2002
%---------------------------------------------------------------------------%
% The .exp2 file is for C# and Java, which print out "-0" without the sign.
%---------------------------------------------------------------------------%

:- module string_to_float.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test("1.23", !IO),
    test("x1.23", !IO),
    test("1.23x", !IO),
    test("x1.23x", !IO),
    test(" 1.23", !IO),
    test("1.23 ", !IO),
    test(" 1.23 ", !IO),
    test("1", !IO),
    test("-1", !IO),
    test("+1", !IO),
    test("0", !IO),
    test("+0", !IO),
    test("-0", !IO),
    test("-", !IO),
    test("+", !IO),
    test(" ", !IO),
    test("", !IO).

:- pred test(string::in, io::di, io::uo) is det.

test(Str, !IO) :-
    ( if string.to_float(Str, Float) then
        Result = string.format("= %f", [f(Float)])
    else
        Result = "FAILS"
    ),
    io.format("string__to_float(\"%s\") %s\n", [s(Str), s(Result)], !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
