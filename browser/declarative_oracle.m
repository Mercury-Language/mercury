%-----------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_debugger.m
% Author: Mark Brown
% Purpose:
%	This module implements the oracle for a Mercury declarative debugger.
% It is called by the front end of the declarative debugger to provide 
% information about the intended interpretation of the program being
% debugged.  The current implementation simply asks the user directly.
%

:- module declarative_oracle.
:- interface.
:- import_module bool, io.
:- import_module declarative_debugger.

	%
	% Query the oracle about the program being debugged.  The first
	% argument is a node in the evaluation tree, the second argument
	% is its validity in the intended interpreation.
	%
:- pred query_oracle(edt_node, bool, io__state, io__state).
:- mode query_oracle(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, char, require.

:- type debugger_command
	--->	yes
	;	no
	;	browse
	;	tree
	;	help
	;	unknown.


query_oracle(Node, Valid) -->
	write_node(Node),
	io__flush_output,
	get_command(Answer),
	(
		{ Answer = yes },
		{ Valid = yes }
	;
		{ Answer = no },
		{ Valid = no }
	;
		{ Answer = browse },
		io__write_string("Sorry, not implemented.\n"),
		query_oracle(Node, Valid)
	;
		{ Answer = tree },
		io__write_string("Sorry, not implemented.\n"),
		query_oracle(Node, Valid)
	;
		{ Answer = help },
		io__write_strings([
			"According to the intended interpretation",
			" of the program, answer one of:\n",
			"\ty\tyes\n",
			"\tn\tno\n",
%			"\tb\tbrowse the atom arguments (not yet)\n",
%			"\tt\tprint the evaluation tree (not yet)\n",
			"\th, ?\tthis help message\n"
		]),
		query_oracle(Node, Valid)
	;
		{ Answer = unknown },
		io__write_string("Unknown command, 'h' for help.\n"),
		query_oracle(Node, Valid)
	).


:- pred write_node(edt_node, io__state, io__state).
:- mode write_node(in, di, uo) is det.

write_node(wrong_answer(Name, Args)) -->
	write_atom(Name, Args),
	io__nl,
	io__write_string("Valid? ").


:- pred get_command(debugger_command, io__state, io__state).
:- mode get_command(out, di, uo) is det.

get_command(Command) -->
	io__read_line(Res),
	{ 
		Res = ok(Line)
	->
		(
			command_chars(Line, Command0)
		->
			Command = Command0
		;
			Command = unknown
		)
	;
		% XXX this should definitely be handled better.
		error("I/O error or EOF.\n")
	}.


:- pred command_chars(list(char), debugger_command).
:- mode command_chars(in, out) is semidet.

command_chars(['y' | _], yes).
command_chars(['n' | _], no).
command_chars(['b' | _], browse).
command_chars(['t' | _], tree).
command_chars(['h' | _], help).
command_chars(['?' | _], help).

