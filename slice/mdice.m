%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Mercury dice tool.
% Main author: zs.
%
%-----------------------------------------------------------------------------%

:- module mdice.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp__slice_and_dice.

:- import_module getopt.
:- import_module list.

%-----------------------------------------------------------------------------%

main(!IO) :-
	io__command_line_arguments(Args0, !IO),
	OptionOps = option_ops_multi(short_option, long_option,
		option_default),
	getopt__process_options(OptionOps, Args0, Args, GetoptResult),
	(
		GetoptResult = ok(OptionTable),
		(
			Args = [],
			usage(!IO)
		;
			Args = [_],
			usage(!IO)
		;
			Args = [PassFileName, FailFileName],
			lookup_string_option(OptionTable, sort, SortStr),
			lookup_int_option(OptionTable, limit, Limit),
			lookup_string_option(OptionTable, modulename, Module),
			read_dice_to_string(PassFileName, FailFileName,
				SortStr, Limit, Module, DiceStr, Problem, !IO),
			( Problem = "" ->
				io__write_string(DiceStr, !IO)
			;
				io__write_string(Problem, !IO),
				io__nl(!IO),
				io__set_exit_status(1, !IO)
			)
		;
			Args = [_, _, _ | _],
			usage(!IO)
		)
	;
		GetoptResult = error(GetoptErrorMsg),
		io__write_string(GetoptErrorMsg, !IO),
		io__nl(!IO)
	).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
	io__write_strings(
		["Usage: mdice [-s sortspec] [-l N] [-m module] ",
		"passfile failfile\n"], !IO).

%-----------------------------------------------------------------------------%

:- type option	
	--->	sort
	;	limit
	;	modulename.

:- type option_table == option_table(option).
		
:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_default(option::out, option_data::out) is multi.

option_default(sort,		string("")).
option_default(limit,		int(100)).
option_default(modulename,	string("")).

short_option('s',		sort).
short_option('l',		limit).
short_option('m',		modulename).

long_option("sort",		sort).
long_option("limit",		limit).
long_option("module",		modulename).

%-----------------------------------------------------------------------------%
