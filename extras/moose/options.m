%----------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%----------------------------------------------------------------------------%

:- module options.

:- interface.

:- import_module getopt, io, list, string.

:- type option
	--->	help
	;	verbose

		% Debugging options
	;	dump_action
	;	dump_first
	;	dump_follow
	;	dump_goto
	;	dump_items
	;	dump_rules

		% Output options
	.

:- type options == option_table(option).
:- type maybe_options == maybe_option_table(option).

:- pred parse_options(maybe_options, list(string), io__state, io__state).
:- mode parse_options(out, out, di, uo) is det.

:- implementation.

:- import_module bool, char, std_util.

parse_options(MOpts, Args, !IO) :-
	io__command_line_arguments(Args0, !IO),
	OptionOpts = option_ops(short, long, defaults),
	getopt__process_options(OptionOpts, Args0, Args, MOpts).

:- pred short(char, option).
:- mode short(in, out) is semidet.

short('h',	help).
short('v',	verbose).
short('a',	dump_action).
short('f',	dump_first).
short('F',	dump_follow).
short('g',	dump_goto).
short('i',	dump_items).
short('r',	dump_rules).

:- pred long(string, option).
:- mode long(in, out) is semidet.

long("help",		help).
long("verbose",		verbose).
long("dump-action",	dump_action).
long("dump-first",	dump_first).
long("dump-follow",	dump_follow).
long("dump-goto",	dump_goto).
long("dump-items",	dump_items).
long("dump-rules",	dump_rules).

:- pred defaults(option, option_data).
:- mode defaults(out, out) is nondet.

defaults(Opt, Data) :-
	semidet_succeed,
	defaults0(Opt, Data).

:- pred defaults0(option, option_data).
:- mode defaults0(out, out) is multi.

defaults0(help,		bool(no)).
defaults0(verbose,	bool(no)).
defaults0(dump_action,	bool(no)).
defaults0(dump_first,	bool(no)).
defaults0(dump_follow,	bool(no)).
defaults0(dump_goto,	bool(no)).
defaults0(dump_items,	bool(no)).
defaults0(dump_rules,	bool(no)).

