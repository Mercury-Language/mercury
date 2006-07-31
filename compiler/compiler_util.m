%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: compiler_util.
% Main author: zs.
%
% This module contains code that can be helpful in any compiler module.
%
%-----------------------------------------------------------------------------%

:- module libs.compiler_util.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

    % Call error/1 with a "Sorry, not implemented" message.
    %
    % Use this for features that should be implemented (or at least could be
    % implemented).
    %
:- func sorry(string, string) = _ is erroneous.
:- pred sorry(string::in, string::in) is erroneous.

    % unexpected(ModuleName, Message):
    %
    % Call error/1 with an "Unexpected" message.
    % Use this to handle cases which are not expected to arise (i.e. bugs).
    %
:- func unexpected(string, string) = _ is erroneous.
:- pred unexpected(string::in, string::in) is erroneous.
    
    % expect(Goal, ModuleName, Message):
    %
    % Call Goal, and call unexpected(ModuleName, Message) if Goal fails.
    %
:- pred expect((pred)::((pred) is semidet), string::in, string::in) is det.

    % Record the fact that a warning has been issued; set the exit status
    % to error if the `--halt-at-warn' option is set.
    %
:- pred record_warning(io::di, io::uo) is det.

    % Report a warning, and set the exit status to error if the
    % `--halt-at-warn' option is set.
    %
:- pred report_warning(string::in, io::di, io::uo) is det.

    % Report a warning to the specified stream, and set the exit status
    % to error if the --halt-at-warn option is set.
    %
:- pred report_warning(io.output_stream::in, string::in, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.globals.
:- import_module libs.options.

:- import_module bool.
:- import_module list.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

    % Call error/1 with a "Sorry, not implemented" message.
    %
sorry(Module, What) = _ :- sorry(Module, What).
sorry(Module, What) :-
    string.format("%s: Sorry, not implemented: %s",
        [s(Module), s(What)], ErrorMessage),
    error(ErrorMessage).

unexpected(Module, What) = _ :- unexpected(Module, What).
unexpected(Module, What) :-
    string.format("%s: Unexpected: %s", [s(Module), s(What)], ErrorMessage),
    error(ErrorMessage).

expect(Goal, Module, Message) :-
    ( Goal ->
        true
    ;
        unexpected(Module, Message)
    ).

record_warning(!IO) :-
    globals.io_lookup_bool_option(halt_at_warn, HaltAtWarn, !IO),
    (
        HaltAtWarn = yes,
        io.set_exit_status(1, !IO)
    ;
        HaltAtWarn = no
    ).

report_warning(Message, !IO) :-
    record_warning(!IO),
    io.write_string(Message, !IO).

report_warning(Stream, Message, !IO) :-
    record_warning(!IO),
    io.write_string(Stream, Message, !IO).

%-----------------------------------------------------------------------------%
:- end_module compiler_util.
%-----------------------------------------------------------------------------%
