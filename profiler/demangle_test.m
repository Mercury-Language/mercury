%-----------------------------------------------------------------------------%
%
% Copyright (C) 1997, 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%
%-----------------------------------------------------------------------------%
%
% File: demangle_test.m
% Author: fjh
%
% Front-end to a mercury symbol demangler.
% This is used to convert error messages from the linker back
% into a form that users can understand.
%
% BEWARE:
% The code here duplicates the functionality of util/mdemangle.c.
% Any changes here will require corresponding changes there.
%
% We might eventually replace util/mdemangle.c with this Mercury version.
%
%-----------------------------------------------------------------------------%

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
	io__command_line_arguments(Args, !IO),
	(
		Args = [_ | _],
		%
		% invoke demangle/2 on each command line argument
		%
		list__map(demangle, Args, DemangledArgs),
		io__write_list(DemangledArgs, "\n", io__write_string, !IO),
		io__nl(!IO)
	;
		Args = [],
		%
		% copy stdin to stdout, calling demangle/2 for
		% every valid C identifier in the input
		%
		demangle_stdin([])
	).

:- pred demangle_stdin(list(char)::in, state::di, state::uo) is det.

demangle_stdin(RevChars, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = ok(Char),
		( char__is_alnum_or_underscore(Char) ->
			demangle_stdin([Char | RevChars], !IO)
		;
			string__from_rev_char_list(RevChars, MangledName),
			demangle(MangledName, DemangledName),
			io__write_string(DemangledName, !IO),
			io__write_char(Char, !IO),
			demangle_stdin([], !IO)
		)
	;
		Result = eof,
		string__from_rev_char_list(RevChars, MangledName),
		demangle(MangledName, DemangledName),
		io__write_string(DemangledName, !IO)
	;
		Result = error(Error),
		io__error_message(Error, Message),
		io__input_stream_name(StreamName, !IO),
		io__progname("demangle_test", ProgName, !IO),
		io__write_strings([ProgName, ": ",
			"error reading input file `", StreamName, "': \n\t",
			Message, "\n"], !IO)
	).

%-----------------------------------------------------------------------------%
