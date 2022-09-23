%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module format_call_warning_helper.
:- interface.

:- import_module io.
:- import_module list.
:- import_module string.

:- pred maybe_log_imported_msg(int::in, int::in,
    string::in, list(poly_type)::in, io::di, io::uo) is det.
:- pragma format_call(pred(maybe_log_imported_msg/6),
    format_string_values(3, 4)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

maybe_log_imported_msg(CurLogLevel, MsgLevel,
        FormatStr, ValuesList, !IO) :-
    Diff = MsgLevel - CurLogLevel,
    ( if Diff >= 0 then
        io.format(FormatStr, ValuesList, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%
