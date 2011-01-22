%----------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%----------------------------------------------------------------------------%

:- module options.
:- interface.

:- import_module getopt.
:- import_module io.
:- import_module list.
:- import_module string.

%----------------------------------------------------------------------------%

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

:- pred parse_options(maybe_options, list(string), io.state, io.state).
:- mode parse_options(out, out, di, uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%


:- implementation.

:- import_module bool.
:- import_module char.
:- import_module std_util.

%----------------------------------------------------------------------------%

parse_options(MOpts, Args, !IO) :-
	io.command_line_arguments(Args0, !IO),
	OptionOpts = option_ops_multi(short, long, defaults),
	getopt.process_options(OptionOpts, Args0, Args, MOpts).

:- pred short(char::in, option::out) is semidet.

short('h',	help).
short('v',	verbose).
short('a',	dump_action).
short('f',	dump_first).
short('F',	dump_follow).
short('g',	dump_goto).
short('i',	dump_items).
short('r',	dump_rules).

:- pred long(string::in, option::out) is semidet.

long("help",		help).
long("verbose",		verbose).
long("dump-action",	dump_action).
long("dump-first",	dump_first).
long("dump-follow",	dump_follow).
long("dump-goto",	dump_goto).
long("dump-items",	dump_items).
long("dump-rules",	dump_rules).

:- pred defaults(option::out, option_data::out) is multi.

defaults(help,		bool(no)).
defaults(verbose,	bool(no)).
defaults(dump_action,	bool(no)).
defaults(dump_first,	bool(no)).
defaults(dump_follow,	bool(no)).
defaults(dump_goto,	bool(no)).
defaults(dump_items,	bool(no)).
defaults(dump_rules,	bool(no)).

%----------------------------------------------------------------------------%
:- end_module options.
%----------------------------------------------------------------------------%
