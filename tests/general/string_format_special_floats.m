%------------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%------------------------------------------------------------------------------%

:- module string_format_special_floats.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module list.
:- import_module string.

%------------------------------------------------------------------------------%

main(!IO) :-
    Inf = (max + max),
    TestSpecs = ["%e", "%f", "%g", "%E", "%F", "%G"],
    io.write_string("Plus Infinity:\n", !IO),
    list.foldl(test_floats([Inf]), TestSpecs, !IO),
    io.write_string("Minus Infinity:\n", !IO),
    list.foldl(test_floats([-Inf]), TestSpecs, !IO),
    io.write_string("Not a number:\n", !IO),
    list.foldl(test_floats([0.0 * Inf]), TestSpecs, !IO).

:- pred test_floats(list(float)::in, string::in, io::di, io::uo) is det.

test_floats(Floats, FormatString, !IO) :-
    list.foldl(test_float(FormatString), Floats, !IO).

:- pred test_float(string::in, float::in, io::di, io::uo) is det.

test_float(FormatString, Float, !IO) :-
    FloatString = string.format(FormatString, [f(Float)]),
    io.format("%20s: %s\n", [s(FormatString), s(FloatString)], !IO).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
