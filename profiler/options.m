%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: options.m.
% Main author: fjh.

% This defines the stuff necessary so that getopt.m
% can parse the command-line options.
% When we implement higher-order preds, this and 
% getopt.m should be rewritten to use them.
% Currently the interface dependencies are very hairy.

% IMPORTANT NOTE: any changes to the options should be
% reflected in both the help message produced below,
% and in the Mercury User's Guide (../doc/user_guide.texi).

%-----------------------------------------------------------------------------%

:- module options.
:- interface.
:- import_module int, string, std_util, list, io, map, require.

:- type option_data	--->	bool(bool)
			;	int(int)
			;	string(string)
			;	accumulating(list(string)).

:- type option_table	==	map(option, option_data).
		
:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_defaults(list(pair(option, option_data))::out) is det.

:- pred options__lookup_bool_option(option_table, option, bool).
:- mode options__lookup_bool_option(in, in, out) is det.

:- pred options__lookup_int_option(option_table, option, int).
:- mode options__lookup_int_option(in, in, out) is det.

:- pred options__lookup_string_option(option_table, option, string).
:- mode options__lookup_string_option(in, in, out) is det.

:- pred options_help(io__state::di, io__state::uo) is det.

% A couple of misc utilities

:- pred maybe_write_string(bool::input, string::input,
			io__state::di, io__state::uo) is det.
:- pred maybe_flush_output(bool::in, io__state::di, io__state::uo) is det.

:- type option	
	% Verbosity options
		--->	verbose
		;	very_verbose
	% Miscellaneous Options
		;	help.

:- implementation.

	% I've split up option_defaults into several different clauses
	% purely in order to reduce compilation time/memory usage.

:- type option_category
	--->	verbosity_option
	;	miscellaneous_option.

option_defaults(OptionDefaults) :-
	option_defaults_2(verbosity_option, VerbosityOptions),
	option_defaults_2(miscellaneous_option, MiscellaneousOptions),
	list__condense([VerbosityOptions, MiscellaneousOptions], OptionDefaults).

:- pred option_defaults_2(option_category::in,
	list(pair(option, option_data))::out) is det.

option_defaults_2(verbosity_option, [
		% Verbosity Options
	verbose			-	bool(no),
	very_verbose		-	bool(no)
]).
option_defaults_2(miscellaneous_option, [
		% Miscellaneous Options
	help 			-	bool(no)
]).


	% please keep this in alphabetic order
short_option('h', 			help).
short_option('v', 			verbose).
short_option('V', 			very_verbose).

long_option("verbose",			verbose).
long_option("very-verbose",		very_verbose).
long_option("help",			help).


options_help -->
	io__write_string("\t-h, --help\n"),
	io__write_string("\t\tPrint this usage message.\n"),

	io__write_string("\nVerbosity Options:\n"),
	io__write_string("\t-v, --verbose\n"),
	io__write_string("\t\tOutput progress messages at each stage in the compilation.\n"),
	io__write_string("\t-V, --very_verbose\n"),
	io__write_string("\t\tOutput very verbose progress messages.\n").


maybe_write_string(yes, String) --> io__write_string(String).
maybe_write_string(no, _) --> [].

maybe_flush_output(yes) --> io__flush_output.
maybe_flush_output(no) --> [].

options__lookup_bool_option(OptionTable, Opt, Val) :-
	(
		map__lookup(OptionTable, Opt, bool(Val0))
	->
		Val = Val0
	;
		error("Expected bool option and didn't get one.")
	).

options__lookup_int_option(OptionTable, Opt, Val) :-
	(
		map__lookup(OptionTable, Opt, int(Val0))
	->
		Val = Val0
	;
		error("Expected int option and didn't get one.")
	).

options__lookup_string_option(OptionTable, Opt, Val) :-
	(
		map__lookup(OptionTable, Opt, string(Val0))
	->
		Val = Val0
	;
		error("Expected string option and didn't get one.")
	).

:- end_module options.

%-----------------------------------------------------------------------------%
