%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module format_call_warning.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module format_call_warning_helper_1.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    CurLevel = 6,
    Msg = "The record temperature is",
    TempA = 42.3,
    TempB = 43.2,

    % This call should get warnings for all four fs/vl pairs.
    maybe_log_msg(CurLevel, 1,
        "%%s: 5.2f", [f(TempA)], [f(TempB)],
        "%5.2f", [s(Msg), f(TempA)], [s(Msg), f(TempB)], !IO),

    % This call should get a warning for only the second fs/vl pair.
    maybe_log_msg(CurLevel, 1,
        "%5.2f", [f(TempA)], [f(TempB), s(Msg)],
        "%s: %5.2f", [s(Msg), f(TempA)], [s(Msg), f(TempB)], !IO),

    % This call should not get any warnings.
    maybe_log_msg(CurLevel, 1,
        "%5.2f", [f(TempA)], [f(TempB)],
        "%s: %5.2f", [s(Msg), f(TempA)], [s(Msg), f(TempB)], !IO),

    % This call should get a warning.
    maybe_log_simple_msg(CurLevel, 1, "%s: %5.2f", [s(Msg), s(Msg)], !IO),

    % This call should get a warning.
    maybe_log_imported_msg(CurLevel, 1, "%s: %5.2f",
        [f(TempA), f(TempB)], !IO).

%---------------------------------------------------------------------------%

:- pred maybe_log_msg(int::in, int::in,
    string::in, list(poly_type)::in, list(poly_type)::in,
    string::in, list(poly_type)::in, list(poly_type)::in,
    io::di, io::uo) is det.
:- pragma format_call(pred(maybe_log_msg/10),
    [format_string_values(3, 4), format_string_values(3, 5),
     format_string_values(6, 7), format_string_values(6, 8)]).

maybe_log_msg(CurLogLevel, MsgLevel,
        FormatStrA, ValuesListA1, ValuesListA2,
        FormatStrB, ValuesListB1, ValuesListB2, !IO) :-
    Diff = MsgLevel - CurLogLevel,
    % The first four calls to io.format below use <format string/values list>
    % pairs that are listed in the format_call pragma. This means that
    % callers of maybe_log_msg should have checked their compatibility,
    % which means that we *don't* want to warn about being unable to check
    % their compatibility *here*.
    ( if Diff >= 15 then
        io.format(FormatStrA, ValuesListA1, !IO)
    else if Diff >= 10 then
        io.format(FormatStrA, ValuesListA2, !IO)
    else if Diff >= 5 then
        io.format(FormatStrB, ValuesListB1, !IO)
    else if Diff >= 0 then
        io.format(FormatStrB, ValuesListB2, !IO)
    else
        % While ValuesListB2 is listed in the format_call pragma, the
        % format string does not come from there. Therefore the compiler
        % *should* generate a warning about being unable to check this call
        % due to having no idea about the value of ValuesListB2.
        io.format("%d", ValuesListB2, !IO)
    ).

:- pred maybe_log_simple_msg(int::in, int::in,
    string::in, list(poly_type)::in, io::di, io::uo) is det.
:- pragma format_call(pred(maybe_log_simple_msg/6),
    format_string_values(3, 4)).

maybe_log_simple_msg(CurLogLevel, MsgLevel,
        FormatStr, ValuesList, !IO) :-
    Diff = MsgLevel - CurLogLevel,
    ( if Diff >= 0 then
        io.format(FormatStr, ValuesList, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%
