%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 2000-2001, 2004 The University of Melbourne.
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
:- import_module bool, string, io, getopt.

:- type option	
	% Verbosity options
		--->	verbose
		;	very_verbose
	% Profiler options
		;	dynamic_cg
		;	call_graph
		;	profile
		;	profile_time
		;	profile_memory_words
		;	profile_memory_cells
		;	demangle
	% Filename options
		;	countfile
		;	pairfile
		;	declfile
		;	libraryfile
	% Miscellaneous Options
		;	help.

:- type option_table == option_table(option).
		
:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_defaults(option::out, option_data::out) is nondet.
:- pred option_default(option::out, option_data::out) is multidet.
:- pred special_handler(option::in, special_data::in,
       option_table::in, maybe_option_table(option)::out) is semidet.

:- pred options_help(io__state::di, io__state::uo) is det.

% A couple of misc utilities

:- pred maybe_write_string(bool::input, string::input, io::di, io::uo) is det.
:- pred maybe_flush_output(bool::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util, map.

option_defaults(Option, Default) :-
	semidet_succeed,
	option_default(Option, Default).

	% Verbosity Options
option_default(verbose,		bool(no)).
option_default(very_verbose,	bool(no)).

	% General profiler options
option_default(dynamic_cg,		bool(no)).
option_default(call_graph,		bool(no)).
option_default(profile,			string_special).
option_default(profile_time,		special).
option_default(profile_memory_words,	special).
option_default(profile_memory_cells,	special).
option_default(countfile,		string("Prof.Counts")).
option_default(pairfile,		string("Prof.CallPair")).
option_default(declfile,		string("Prof.Decl")).
option_default(libraryfile,		string("")).
option_default(demangle,		bool(yes)).

	% Miscellaneous Options
option_default(help,		bool(no)).


	% please keep this in alphabetic order
short_option('C',			countfile).
short_option('c',			call_graph).
short_option('d',			dynamic_cg).
short_option('D',			declfile).
short_option('h', 			help).
short_option('L', 			libraryfile).
short_option('m', 			profile_memory_words).
short_option('M', 			profile_memory_cells).
short_option('p',			profile).
short_option('P',			pairfile).
short_option('t', 			profile_time).
short_option('v', 			verbose).
short_option('V', 			very_verbose).

long_option("call-pair-file",		pairfile).
long_option("call-graph",		call_graph).
long_option("count-file",		countfile).
long_option("declaration-file",		declfile).
long_option("demangle",			demangle).
long_option("help",			help).
long_option("library-callgraph",	help).
long_option("profile",			profile).
long_option("profile-memory-words",	profile_memory_words).
long_option("profile-memory-cells",	profile_memory_cells).
long_option("profile-time",		profile_time).
long_option("use-dynamic",		dynamic_cg).
long_option("verbose",			verbose).
long_option("very-verbose",		very_verbose).

special_handler(profile, string(WhatToProfile), OptionTable0, Result) :-
	( valid_profile_option(WhatToProfile, CountFile) ->
		map__set(OptionTable0, countfile, string(CountFile),
			OptionTable),
		Result = ok(OptionTable)
	;
		Result = error("Invalid argument to `--profile' or `-p' option")
	).
special_handler(profile_memory_words, _, OptionTable0, ok(OptionTable)) :-
	map__set(OptionTable0, countfile, string("Prof.MemoryWords"),
		OptionTable).
special_handler(profile_memory_cells, _, OptionTable0, ok(OptionTable)) :-
	map__set(OptionTable0, countfile, string("Prof.MemoryCells"),
		OptionTable).
special_handler(profile_time, _, OptionTable0, ok(OptionTable)) :-
	map__set(OptionTable0, countfile, string("Prof.Counts"),
		OptionTable).

:- pred valid_profile_option(string::in, string::out) is semidet.

valid_profile_option("memory-words", "Prof.MemoryWords").
valid_profile_option("memory-cells", "Prof.MemoryCells").
valid_profile_option("time", "Prof.Counts").
	
% :- pred special_handler(option::in, special_data::in,
%       option_table::in, maybe_option_table::out) is semidet.

options_help -->
	io__write_string("\t-h, --help\n"),
	io__write_string("\t\tPrint this usage message.\n"),

	io__write_string("\nProfiler Options:\n"),
	io__write_string("\t-c, --call-graph\n"),
	io__write_string("\t\tInclude the call graph profile.\n"),
	io__write_string("\t-d, --use-dynamic\n"),
	io__write_string("\t\tBuild the call graph dynamically.\n"),
	io__write_string("\t-p, --profile {time, memory-words, memory-cells}\n"),
	io__write_string("\t\tSelect what to profile: time, amount of memory allocated, or\n"),
	io__write_string("\t\tnumber of memory allocations (regardless of size).\n"),
	io__write_string("\t-m\n"),
	io__write_string("\t\tSame as `--profile memory-words'\n"),
	io__write_string("\t-M\n"),
	io__write_string("\t\tSame as `--profile memory-cells'.\n"),
	io__write_string("\t-t\n"),
	io__write_string("\t\tSame as `--profile time'.\n"),
	io__write_string("\t--no-demangle\n"),
	io__write_string("\t\tOutput the mangled predicate and function names.\n"),

	io__write_string("\nFilename Options:\n"),
	io__write_string("\t-C <file>, --count-file <file>\n"),
	io__write_string("\t\tName of the count file. Usually `Prof.Counts',\n"),
	io__write_string("\t\t`Prof.MemoryWords', or `Prof.MemoryCells'.\n"),
	io__write_string("\t-D <file>, --declaration-file <file>\n"),
	io__write_string("\t\tName of the declaration file. Usually `Prof.Decl'.\n"),
	io__write_string("\t-P <file>, --call-pair-file <file>\n"),
	io__write_string("\t\tName of the call-pair file. Usually `Prof.CallPair'.\n"),
	io__write_string("\t-L <file>, --library-callgraph <file>\n"),
	io__write_string("\t\tName of the file which contains the call graph for\n"),
	io__write_string("\t\tthe library modules.\n"),

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

%-----------------------------------------------------------------------------%
:- end_module options.
%-----------------------------------------------------------------------------%
