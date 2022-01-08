%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997, 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: demangle_test.m
% Author: fjh
%
% Front-end to a Mercury symbol demangler. This is used to convert error
% messages from the linker back into a form that users can understand.
%
% BEWARE:
% The code here duplicates the functionality of util/mdemangle.c.
% Any changes here will require corresponding changes there.
%
% We might eventually replace util/mdemangle.c with this Mercury version.
%
%---------------------------------------------------------------------------%

:- module demangle_test.
:- interface.

:- import_module io.
:- pred main(state::di, state::uo) is det.

:- implementation.

:- import_module demangle.

:- import_module char.
:- import_module list.
:- import_module string.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (
        Args = [_ | _],

        % Invoke demangle/2 on each command line argument.
        list.map(demangle, Args, DemangledArgs),
        io.write_list(DemangledArgs, "\n", io.write_string, !IO),
        io.nl(!IO)
    ;
        Args = [],

        % Copy stdin to stdout, calling demangle/2 for every valid C identifier
        % in the input.
        demangle_stdin([], !IO)
    ).

:- pred demangle_stdin(list(char)::in, state::di, state::uo) is det.

demangle_stdin(RevChars, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = ok(Char),
        ( if char.is_alnum_or_underscore(Char) then
            demangle_stdin([Char | RevChars], !IO)
        else
            string.from_rev_char_list(RevChars, MangledName),
            demangle(MangledName, DemangledName),
            io.write_string(DemangledName, !IO),
            io.write_char(Char, !IO),
            demangle_stdin([], !IO)
        )
    ;
        Result = eof,
        string.from_rev_char_list(RevChars, MangledName),
        demangle(MangledName, DemangledName),
        io.write_string(DemangledName, !IO)
    ;
        Result = error(Error),
        io.error_message(Error, Message),
        io.input_stream_name(StreamName, !IO),
        io.progname("demangle_test", ProgName, !IO),
        io.write_strings([ProgName, ": ",
            "error reading input file `", StreamName, "': \n\t",
            Message, "\n"], !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module demangle_test.
%---------------------------------------------------------------------------%
