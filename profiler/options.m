%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: options.m.
% Main author: fjh.

% This defines the stuff necessary so that getopt.m
% can parse the command-line options.

%-----------------------------------------------------------------------------%

:- module options.
:- interface.
:- import_module bool, int, string, io, std_util, getopt.

:- type option	
	% Verbosity options
		--->	verbose
		;	very_verbose
	% Profiler options
		;	dynamic_cg
		;	call_graph
		;	countfile
		;	pairfile
		;	declfile
		;	libraryfile
	% Miscellaneous Options
		;	help.

:- type option_table	==	option_table(option).
		
:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_defaults(option::out, option_data::out) is nondet.
:- pred option_default(option::out, option_data::out) is multidet.

:- pred options_help(io__state::di, io__state::uo) is det.

% A couple of misc utilities

:- pred maybe_write_string(bool::input, string::input,
			io__state::di, io__state::uo) is det.
:- pred maybe_flush_output(bool::in, io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require.

option_defaults(Option, Default) :-
	semidet_succeed,
	option_default(Option, Default).

	% Verbosity Options
option_default(verbose,		bool(no)).
option_default(very_verbose,	bool(no)).

	% General profiler options
option_default(dynamic_cg,	bool(no)).
option_default(call_graph,	bool(no)).
option_default(countfile,	string("Prof.Counts")).
option_default(pairfile,	string("Prof.CallPair")).
option_default(declfile,	string("Prof.Decl")).
option_default(libraryfile,	string("")).

	% Miscellaneous Options
option_default(help,		bool(no)).


	% please keep this in alphabetic order
short_option('C',			countfile).
short_option('c',			call_graph).
short_option('d',			dynamic_cg).
short_option('D',			declfile).
short_option('h', 			help).
short_option('L', 			libraryfile).
short_option('P',			pairfile).
short_option('v', 			verbose).
short_option('V', 			very_verbose).

long_option("call-pair-file",		pairfile).
long_option("call-graph",		call_graph).
long_option("count-file",		countfile).
long_option("declaration-file",		declfile).
long_option("help",			help).
long_option("library-callgraph",	help).
long_option("use-dynamic",		dynamic_cg).
long_option("verbose",			verbose).
long_option("very-verbose",		very_verbose).


options_help -->
	io__write_string("\t-h, --help\n"),
	io__write_string("\t\tPrint this usage message.\n"),

	io__write_string("\nProfiler Options:\n"),
	io__write_string("\t-c, --call-graph\n"),
	io__write_string("\t\tInclude the call graph profile\n"),
	io__write_string("\t-d, --use-dynamic\n"),
	io__write_string("\t\tBuild the call graph dynamically.\n"),
	io__write_string("\t-C, --count-file\n"),
	io__write_string("\t\tName of the count file. Usually `Prof.Counts'.\n"),
	io__write_string("\t-D, --declaration-file\n"),
	io__write_string("\t\tName of the declaration file. Usually `Prof.Decl'.\n"),
	io__write_string("\t-P, --call-pair-file\n"),
	io__write_string("\t\tName of the call-pair file. Usually `Prof.CallPair'.\n"),
	io__write_string("\t-L, --library-callgraph\n"),
	io__write_string("\t\tName of the file which contains the callgraph for the library modules.\n"),


	io__write_string("\nVerbosity Options:\n"),
	io__write_string("\t-v, --verbose\n"),
	io__write_string("\t\tOutput progress messages at each stage.\n"),
	io__write_string("\t-V, --very_verbose\n"),
	io__write_string("\t\tOutput very verbose progress messages.\n").

%-----------------------------------------------------------------------------%

maybe_write_string(yes, String) --> io__write_string(String).
maybe_write_string(no, _) --> [].

maybe_flush_output(yes) --> io__flush_output.
maybe_flush_output(no) --> [].

:- end_module options.

%-----------------------------------------------------------------------------%
