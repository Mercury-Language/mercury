%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This file defines the top level predicates of the server process of the
% Mercury deep profiler. It is mostly concerned with option handling.

:- module mdprof_server.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module conf, interface, profile, read_profile, startup, server.
:- import_module array, bool, char, getopt, int, list, assoc_list.
:- import_module map, require, set, std_util, string, require.

:- type option
	--->	canonical_clique
	;	debug
	;	test
	;	test_dir
	;	test_fields
	;	timeout.

:- type options ---> options.
:- type option_table == (option_table(option)).

main -->
	io__stderr_stream(StdErr),
	io__report_stats,
	io__write_string(StdErr, "  Handling options...\n"),
	io__command_line_arguments(Args0),
	{ getopt__process_options(option_ops(short, long, defaults),
		Args0, Args, MaybeOptions) },
	(
		{ MaybeOptions = ok(Options) },
		( { Args = [] } ->
			io__set_exit_status(1),
			io__write_string(StdErr,
				"no data file name specified\n")
		;
			main2(Args, Options)
		)
	;
		{ MaybeOptions = error(Msg) },
		io__set_exit_status(1),
		io__format(StdErr, "error parsing options: %s\n", [s(Msg)])
	).

:- pred main2(list(string)::in, option_table::in,
	io__state::di, io__state::uo) is cc_multi.

main2(FileNames, Options) -->
	io__stderr_stream(StdErr),
	server_name(Machine),
	{ lookup_bool_option(Options, test, Test) },
	{ lookup_bool_option(Options, canonical_clique, CanonicalClique) },
	(
		{ Test = yes },
		read_and_startup(Machine, FileNames, CanonicalClique, Res),
		(
			{ Res = ok(Deep) },
			{ lookup_string_option(Options, test_dir, TestDir) },
			{ lookup_string_option(Options, test_fields,
				TestFields) },
			test_server(TestDir, Deep, TestFields)
		;
			{ Res = error(Error) },
			io__set_exit_status(1),
			io__format(StdErr,
				"error reading data file: %s\n",
				[s(Error)])
		)
	;
		{ Test = no },
		make_pipes(FileNames, IsOK),
		(
			{ IsOK = yes },
			read_and_startup(Machine, FileNames, CanonicalClique,
				Res),
			(
				{ Res = ok(Deep) },
				{ lookup_int_option(Options, timeout,
					TimeOut) },
				{ lookup_bool_option(Options, debug, Debug) },
				server(TimeOut, Debug, Deep)
			;
				{ Res = error(Error) },
				io__set_exit_status(1),
				io__format(StdErr,
					"error reading data file: %s\n",
					[s(Error)])
			)
		;
			{ IsOK = no },
			io__set_exit_status(1),
			io__write_string(StdErr,
				"could not make pipes to CGI script\n")
		)
	).

:- pred make_pipes(list(string)::in, bool::out, io__state::di, io__state::uo)
	is det.

make_pipes(FileNames, OK) -->
	( { FileNames = [FileName] } ->
		{ InputPipe = to_server_pipe_name(FileName) },
		{ OutputPipe = from_server_pipe_name(FileName) },
		{ MakeInputPipeCmd = make_pipe_cmd(InputPipe) },
		{ MakeOutputPipeCmd = make_pipe_cmd(OutputPipe) },
		io__call_system(MakeInputPipeCmd, InputRes),
		io__call_system(MakeOutputPipeCmd, OutputRes),
		{
			InputRes = ok(0),
			OutputRes = ok(0)
		->
			OK = yes
		;
			OK = no
		}
	;
		{ error("make_pipes: multiple filenames not yet implemented") }
	).

%-----------------------------------------------------------------------------%

:- pred short(char::in, option::out) is semidet.

short('c',	canonical_clique).
short('D',	test_dir).
short('F',	test_fields).
short('t',	timeout).
short('T',	test).

:- pred long(string::in, option::out) is semidet.

long("canonical-clique",canonical_clique).
long("debug",		debug).
long("test",		test).
long("test-dir",	test_dir).
long("test-fields",	test_fields).
long("timeout",		timeout).

:- pred defaults(option::out, option_data::out) is nondet.

defaults(Option, Data) :-
	semidet_succeed,
	defaults0(Option, Data).

:- pred defaults0(option::out, option_data::out) is multi.

defaults0(canonical_clique,	bool(no)).
defaults0(debug,		bool(no)).
defaults0(test,			bool(no)).
defaults0(test_dir,		string("deep_test")).
defaults0(test_fields,		string("pqw")).
defaults0(timeout,		int(30)).
