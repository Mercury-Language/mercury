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
:- import_module io.
:- import_module declarative_debugger.

	%
	% The oracle database.  This is threaded around the declarative
	% debugger, but currently stores no information.
	%
:- type oracle_data.

	%
	% Query the oracle about the program being debugged.  The first
	% argument is a node in the evaluation tree, the second argument
	% is its validity in the intended interpretation.
	%
:- pred query_oracle(edt_node, edt_truth, oracle_data, oracle_data,
		io__state, io__state).
:- mode query_oracle(in, out, in, out, di, uo) is det.

	%
	% Produce a new oracle state.
	%
:- pred oracle_data_init(oracle_data).
:- mode oracle_data_init(out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, list, char, require, std_util, string.
:- import_module util.

:- type oracle_data == unit.

:- type debugger_command
	--->	yes
	;	no
	;	browse
	;	tree
	;	help
	;	illegal_command.


oracle_data_init(unit).


query_oracle(Node, Valid, Oracle0, Oracle) -->
	query_user(Node, Answer),
	(
		{ Answer = yes },
		{ Valid = yes },
		{ Oracle = Oracle0 }
	;
		{ Answer = no },
		{ Valid = no },
		{ Oracle = Oracle0 }
	;
		{ Answer = browse },
		io__write_string("Sorry, not implemented.\n"),
		query_oracle(Node, Valid, Oracle0, Oracle)
	;
		{ Answer = tree },
		io__write_string("Sorry, not implemented.\n"),
		query_oracle(Node, Valid, Oracle0, Oracle)
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
		query_oracle(Node, Valid, Oracle0, Oracle)
	;
		{ Answer = illegal_command },
		io__write_string("Unknown command, 'h' for help.\n"),
		query_oracle(Node, Valid, Oracle0, Oracle)
	).


:- pred query_user(edt_node, debugger_command, io__state, io__state).
:- mode query_user(in, out, di, uo) is det.

query_user(Node, Answer) -->
	write_node(Node),
	io__nl,
	get_command("Valid? ", Answer).


:- pred get_command(string, debugger_command, io__state, io__state).
:- mode get_command(in, out, di, uo) is det.

get_command(Prompt, Command) -->
	util__trace_getline(Prompt, Result),
	( { Result = ok(String) },
		{ string__to_char_list(String, Line) },
		{
			command_chars(Line, Command0)
		->
			Command = Command0
		;
			Command = illegal_command
		}
	; { Result = error(Error) },
		{ io__error_message(Error, Msg) },
		io__write_string(Msg),
		io__nl,
		get_command(Prompt, Command)
	; { Result = eof },
		% XXX this should definitely be handled better.
		{ Command = illegal_command }
	).


:- pred command_chars(list(char), debugger_command).
:- mode command_chars(in, out) is semidet.

command_chars(['y' | _], yes).
command_chars(['n' | _], no).
command_chars(['b' | _], browse).
command_chars(['t' | _], tree).
command_chars(['h' | _], help).
command_chars(['?' | _], help).

