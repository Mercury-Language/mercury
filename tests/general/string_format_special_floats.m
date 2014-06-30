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
    list.foldl(test_floats(is_plus_infinity, [Inf]), TestSpecs, !IO),
    io.write_string("Minus Infinity:\n", !IO),
    list.foldl(test_floats(is_minus_infinity, [-Inf]), TestSpecs, !IO),
    io.write_string("Not a number:\n", !IO),
    list.foldl(test_floats(is_nan, [0.0 * Inf]), TestSpecs, !IO).

:- pred test_floats(pred(string)::in(pred(in) is semidet), list(float)::in,
    string::in, io::di, io::uo) is det.

test_floats(IsValid, Floats, FormatString, !IO) :-
    list.foldl(test_float(FormatString, IsValid), Floats, !IO).

:- pred test_float(string::in, pred(string)::in(pred(in) is semidet),
    float::in, io::di, io::uo) is det.

test_float(FormatString, IsValid, Float, !IO) :-
    FloatString = string.format(FormatString, [f(Float)]),
    io.format("%20s: ", [s(FormatString)], !IO),
    ( IsValid(FloatString) ->
        io.write_string("success\n", !IO)
    ;
        io.write_string("failure '" ++ FloatString ++ "'\n", !IO)
    ).

:- pred is_plus_infinity(string::in) is semidet.

is_plus_infinity(String) :-
    LowerCaseString = string.to_lower(String),
    ( LowerCaseString = "infinity"
    ; LowerCaseString = "inf"
    ).

:- pred is_minus_infinity(string::in) is semidet.

is_minus_infinity(String) :-
    LowerCaseString = string.to_lower(String),
    ( LowerCaseString = "-infinity"
    ; LowerCaseString = "-inf"
    ).

:- pred is_nan(string::in) is semidet.

is_nan(String) :-
    LowerCaseString = string.to_lower(String),
    ( LowerCaseString = "nan"
    % XXX Actually, it makes no sense to put a minus sign on a NaN,
    %     since NaNs aren't signed.  However, the printf() function in
    %     some C libraries (in particular, the one for Solaris 2.7)
    %     do that.  Arguably that's a bug, but we can't do much about
    %     bugs in the Solaris C library, so we don't want to report a
    %     test case failure for that.  Hence we allow -NaN here.
    ; LowerCaseString = "-nan"
    ).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
