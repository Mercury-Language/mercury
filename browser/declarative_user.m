%-----------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_user.m
% Author: Mark Brown
% Purpose:
% 	This module performs all the user interaction of the front
% end of the declarative debugger.  It is responsible for displaying
% edt_nodes in a human-readable format, and for getting responses to
% debugger queries from the user.
%

:- module declarative_user.
:- interface.
:- import_module declarative_debugger, declarative_oracle.

	%
	% This predicate handles the interactive part of the declarative
	% debugging process.  The user is presented with an EDT node,
	% and is asked to respond about the truth of the node in the
	% intended interpretation.
	%

:- pred query_user(edt_node, oracle_answer, io__state, io__state).
:- mode query_user(in, out, di, uo) is det.

	%
	% Display the node in user readable form on the current
	% output stream.
	%
:- pred write_edt_node(edt_node, io__state, io__state).
:- mode write_edt_node(in, di, uo) is det.

	%
	% These are the responses the user can give.
	%
:- type user_response
	--->	yes			% The node is correct.
	;	no			% The node is incorrect.
	;	inadmissible		% The node is inadmissible.
	;	do_not_know		% The user has no answer.
	;	browse			% Browse the data before answering.
	;	tree			% Browse the EDT.
	;	help			% Request help before answering.
	;	illegal_command.	% None of the above.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module util.
:- import_module std_util, io, bool, list, char, string.

query_user(Node, Answer) -->
	write_edt_node(Node),
	io__nl,
	get_command("Valid? ", Command),
	(
		{ Command = yes },
		{ Answer = truth_value(yes) }
	;
		{ Command = no },
		{ Answer = truth_value(no) }
	;
		{ Command = inadmissible },
		io__write_string("Sorry, not implemented.\n"),
		query_user(Node, Answer)
	;
		{ Command = do_not_know },
		{ Answer = deferred(do_not_know) }
	;
		{ Command = browse },
		io__write_string("Sorry, not implemented.\n"),
		query_user(Node, Answer)
	;
		{ Command = tree },
		io__write_string("Sorry, not implemented.\n"),
		query_user(Node, Answer )
	;
		{ Command = help },
		io__write_strings([
			"According to the intended interpretation",
			" of the program, answer one of:\n",
			"\ty\tyes\n",
			"\tn\tno\n",
%			"\ti\tinadmissible (not yet)\n",
			"\td\tdon't know\n",
%			"\tb\tbrowse the atom arguments (not yet)\n",
%			"\tt\tprint the evaluation tree (not yet)\n",
			"\th, ?\tthis help message\n"
		]),
		query_user(Node, Answer )
	;
		{ Command = illegal_command },
		io__write_string("Unknown command, 'h' for help.\n"),
		query_user(Node, Answer)
	).


:- pred get_command(string, user_response, io__state, io__state).
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


:- pred command_chars(list(char), user_response).
:- mode command_chars(in, out) is semidet.

command_chars(['y' | _], yes).
command_chars(['n' | _], no).
command_chars(['i' | _], inadmissible).
command_chars(['d' | _], do_not_know).
command_chars(['b' | _], browse).
command_chars(['t' | _], tree).
command_chars(['h' | _], help).
command_chars(['?' | _], help).


write_edt_node(Node) -->
	{ Node = wrong_answer(Name, Args) },
	io__write_string(Name),
	(
		{ Args = [Arg1 | Args0] }
	->
		io__write_char('('),
		io__print(Arg1),
		write_args_rest(Args0),
		io__write_char(')')
	;
		[]
	).


:- pred write_args_rest(list(univ), io__state, io__state).
:- mode write_args_rest(in, di, uo) is det.

write_args_rest([]) -->
	[].
write_args_rest([Arg | Args]) -->
	io__write_string(", "),
	io__print(Arg),
	write_args_rest(Args).

