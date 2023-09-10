%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% This is a regression test.
%
% Compiling this program with --intermod-opt used to yield:
%
% xml_read.opt:013: Syntax error at token 'event': unexpected token
% xml_read.opt:013:   at start of (sub)term.
% xml_read.opt:018: In clause for function `get_value'/1:
% xml_read.opt:018:   in function result term of clause head:
% xml_read.opt:018:   error: undefined symbol `xml_read.event'/1.
%
% This was caused by the direct_arg clause for the type xml_read_or_eof
% not escaping the operator "event".

:- module xml_event_read.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module xml_event_read_helper_1.

main(!IO) :-
    V = get_value("hello"),
    io.print_line(V, !IO).
