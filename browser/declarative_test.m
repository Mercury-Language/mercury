%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2000, 2003, 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_test.m
% Author: Mark Brown
%
% This module is a stand-alone version of the front end, suitable for
% testing.

:- module declarative_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module mdb.
:- import_module mdb.declarative_debugger.
:- import_module mdb.declarative_execution.

:- import_module list, std_util, map, require.

main -->
	process_arguments(MaybeFile),
	(
		{ MaybeFile = yes(File) }
	->
		load_trace_node_map(File, Map, Key),
		io__stdin_stream(StdIn),
		io__stdout_stream(StdOut),
		{ diagnoser_state_init(StdIn, StdOut, State) },
		diagnosis(Map, Key, Response, State, _),
		io__write_string("Diagnoser response:\n"),
		io__write(Response),
		io__nl
	;
		usage
	).

:- pred process_arguments(maybe(io__input_stream),
		io__state, io__state).
:- mode process_arguments(out, di, uo) is det.

process_arguments(MaybeFile) -->
	io__command_line_arguments(Args),
	(
		{ Args = [FileName] }
	->
		io__open_input(FileName, Res),
		(
			{ Res = ok(File) }
		->
			{ MaybeFile = yes(File) }
		;
			{ MaybeFile = no }
		)
	;
		{ MaybeFile = no }
	).

:- pred usage(io__state, io__state).
:- mode usage(di, uo) is det.

usage -->
	io__progname_base("declarative_test", Name),
	io__write_strings(["Usage: ", Name, " <filename>\n"]).

