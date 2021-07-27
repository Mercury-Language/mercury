%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The removal of automatic initialisation for solver types in
% rotd-2007-10-31 inadvertently weakened the scheduling of
% delayed goals such as the trace goal below.

:- module trace_goal_reorder.
:- interface.

:- import_module io.
:- import_module maybe.

:- pred read_deep_byte(maybe_error(int)::out,
    io::di, io::uo) is det.

:- implementation.

read_deep_byte(Res, !IO) :-
    read_byte(Res0, !IO),
    trace [compile_time(flag("debug_read_profdeep")), io(!IO)] (
        io.write_string("byte ", !IO),
        io.write_line(Res, !IO)
    ),
    (
        Res0 = ok(Byte),
        Res = ok(Byte)
    ;
        Res0 = eof,
        Res = error("unexpected end of file")
    ;
        Res0 = error(Err),
        io.error_message(Err, Msg),
        Res = error(Msg)
    ).
