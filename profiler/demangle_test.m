%-----------------------------------------------------------------------------%
%
% Copyright (C) 1997 The University of Melbourne.
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
:- import_module list, char, string.

main -->
	io__command_line_arguments(Args),
	( { Args \= [] } ->
		%
		% invoke demangle/2 on each command line argument
		%
		{ list__map(demangle, Args, DemangledArgs) },
		io__write_list(DemangledArgs, "\n", io__write_string), io__nl
	;
		%
		% copy stdin to stdout, calling demangle/2 for
		% every valid C identifier in the input
		%
		demangle_stdin([])
	).

:- pred demangle_stdin(list(char)::in, state::di, state::uo) is det.
demangle_stdin(RevChars) -->
	io__read_char(Result),
	( { Result = ok(Char) },
		( { char__is_alnum_or_underscore(Char) } ->
			demangle_stdin([Char | RevChars])
		;
			{ string__from_rev_char_list(RevChars, MangledName) },
			{ demangle(MangledName, DemangledName) },
			io__write_string(DemangledName),
			io__write_char(Char),
			demangle_stdin([])
		)
	; { Result = eof },
		{ string__from_rev_char_list(RevChars, MangledName) },
		{ demangle(MangledName, DemangledName) },
		io__write_string(DemangledName)
	; { Result = error(Error) },
		{ io__error_message(Error, Message) },
		io__input_stream_name(StreamName),
		io__progname("demangle_test", ProgName),
		io__write_strings([ProgName, ": ",
			"error reading input file `", StreamName, "': \n\t",
			Message, "\n"])
	).

%-----------------------------------------------------------------------------%
