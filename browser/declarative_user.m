%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_user.m
% Author: Mark Brown
% Purpose:
% 	This module performs all the user interaction of the front
% end of the declarative debugger.  It is responsible for displaying
% questions and bugs in a human-readable format, and for getting
% responses to debugger queries from the user.
%

:- module mdb__declarative_user.
:- interface.
:- import_module mdb__declarative_debugger.
:- import_module list, io.

:- type user_response
	--->	user_answer(decl_answer)
	;	no_user_answer
	;	abort_diagnosis.

:- type user_state.

:- pred user_state_init(io__input_stream, io__output_stream, user_state).
:- mode user_state_init(in, in, out) is det.

	% This predicate handles the interactive part of the declarative
	% debugging process.  The user is presented with an EDT node,
	% and is asked to respond about the truth of the node in the
	% intended interpretation.
	%
:- pred query_user(list(decl_question), user_response, user_state, user_state,
		io__state, io__state).
:- mode query_user(in, out, in, out, di, uo) is det.

	% Confirm that the node found is indeed an e_bug or an i_bug.
	%
:- pred user_confirm_bug(decl_bug, decl_confirmation, user_state, user_state,
		io__state, io__state).
:- mode user_confirm_bug(in, out, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module mdb__declarative_execution, mdb__util.
:- import_module std_util, char, string, bool.

:- type user_state
	--->	user(
			instr	:: io__input_stream,
			outstr	:: io__output_stream
		).

user_state_init(InStr, OutStr, User) :-
	User = user(InStr, OutStr).

query_user(Nodes, Response, User0, User) -->
	query_user_2(Nodes, [], Response, User0, User).

:- pred query_user_2(list(decl_question), list(decl_question), user_response,
		user_state, user_state, io__state, io__state).
:- mode query_user_2(in, in, out, in, out, di, uo) is det.

query_user_2([], _, no_user_answer, User, User) -->
	[].
query_user_2([Node | Nodes], Skipped, Response, User0, User) -->
	write_decl_question(Node, User0),
	{ decl_question_prompt(Node, Question) },
	get_command(Question, Command, User0, User1),
	(
		{ Command = yes },
		{ Response = user_answer(Node - yes) },
		{ User = User1 }
	;
		{ Command = no },
		{ Response = user_answer(Node - no) },
		{ User = User1 }
	;
		{ Command = inadmissible },
		io__write_string("Sorry, not implemented,\n"),
		query_user_2([Node | Nodes], Skipped, Response, User1, User)
	;
		{ Command = skip },
		query_user_2(Nodes, [Node | Skipped], Response, User1, User)
	;
		{ Command = restart },
		{ reverse_and_append(Skipped, [Node | Nodes], Questions) },
		query_user_2(Questions, [], Response, User1, User)
	;
		{ Command = browse },
		browse_edt_node(Node, User1, User2),
		query_user_2([Node | Nodes], Skipped, Response, User2, User)
	;
		{ Command = abort },
		{ Response = abort_diagnosis },
		{ User = User1 }
	;
		{ Command = help },
		user_help_message(User1),
		query_user_2([Node | Nodes], Skipped, Response, User1, User)
	;
		{ Command = illegal_command },
		io__write_string("Unknown command, 'h' for help.\n"),
		query_user_2([Node | Nodes], Skipped, Response, User1, User)
	).

:- pred decl_question_prompt(decl_question, string).
:- mode decl_question_prompt(in, out) is det.

decl_question_prompt(wrong_answer(_), "Valid? ").
decl_question_prompt(missing_answer(_, _), "Complete? ").
decl_question_prompt(unexpected_exception(_, _), "Expected? ").

:- pred browse_edt_node(decl_question, user_state, user_state,
		io__state, io__state).
:- mode browse_edt_node(in, in, out, di, uo) is det.

browse_edt_node(_Node, User, User) -->
	io__write_string("Sorry, not implemented.\n").

:- pred browse_decl_bug(decl_bug, user_state, user_state,
		io__state, io__state).
:- mode browse_decl_bug(in, in, out, di, uo) is det.

browse_decl_bug(_Bug, User, User) -->
	io__write_string("Sorry, not implemented.\n").

	% Reverse the first argument and append the second to it.
	%
:- pred reverse_and_append(list(T), list(T), list(T)).
:- mode reverse_and_append(in, in, out) is det.

reverse_and_append([], Bs, Bs).
reverse_and_append([A | As], Bs, Cs) :-
	reverse_and_append(As, [A | Bs], Cs).

%-----------------------------------------------------------------------------%

:- type user_command
	--->	yes			% The node is correct.
	;	no			% The node is incorrect.
	;	inadmissible		% The node is inadmissible.
	;	skip			% The user has no answer.
	;	restart			% Ask the skipped questions again.
	;	browse			% Browse the data before answering.
	;	abort			% Abort this diagnosis session.
	;	help			% Request help before answering.
	;	illegal_command.	% None of the above.

:- pred user_help_message(user_state, io__state, io__state).
:- mode user_help_message(in, di, uo) is det.

user_help_message(User) -->
	io__write_strings(User^outstr, [
		"According to the intended interpretation of the program,",
		" answer one of:\n",
		"\ty\tyes\t\tthe node is correct\n",
		"\tn\tno\t\tthe node is incorrect\n",
%		"\ti\tinadmissible\tthe input arguments are out of range\n",
		"\ts\tskip\t\tskip this question\n",
		"\tr\trestart\t\task the skipped questions again\n",
%		"\tb\tbrowse\t\tbrowse the atom\n",
		"\ta\tabort\t\t",
			"abort this diagnosis session and return to mdb\n",
		"\th, ?\thelp\t\tthis help message\n"
	]).

:- pred user_confirm_bug_help(user_state, io__state, io__state).
:- mode user_confirm_bug_help(in, di, uo) is det.

user_confirm_bug_help(User) -->
	io__write_strings(User^outstr, [
		"Answer one of:\n",
		"\ty\tyes\t\tconfirm that the suspect is a bug\n",
		"\tn\tno\t\tdo not accept that the suspect is a bug\n",
%		"\tb\tbrowse\t\tbrowse the suspect\n",
		"\ta\tabort\t\t",
			"abort this diagnosis session and return to mdb\n",
		"\th, ?\thelp\t\tthis help message\n"
	]).

:- pred get_command(string, user_command, user_state, user_state,
		io__state, io__state).
:- mode get_command(in, out, in, out, di, uo) is det.

get_command(Prompt, Command, User, User) -->
	util__trace_getline(Prompt, Result, User^instr, User^outstr),
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
		io__write_string(User^outstr, Msg),
		io__nl(User^outstr),
		{ Command = abort }
	; { Result = eof },
		{ Command = abort }
	).

:- pred command_chars(list(char), user_command).
:- mode command_chars(in, out) is semidet.

command_chars(['y' | _], yes).
command_chars(['n' | _], no).
command_chars(['i' | _], inadmissible).
command_chars(['s' | _], skip).
command_chars(['r' | _], restart).
command_chars(['b' | _], browse).
command_chars(['a' | _], abort).
command_chars(['h' | _], help).
command_chars(['?' | _], help).

%-----------------------------------------------------------------------------%

user_confirm_bug(Bug, Response, User0, User) -->
	write_decl_bug(Bug, User0),
	get_command("Is this a bug? ", Command, User0, User1),
	(
		{ Command = yes }
	->
		{ Response = confirm_bug },
		{ User = User1 }
	;
		{ Command = no }
	->
		{ Response = overrule_bug },
		{ User = User1 }
	;
		{ Command = abort }
	->
		{ Response = abort_diagnosis },
		{ User = User1 }
	;
		{ Command = browse }
	->
		browse_decl_bug(Bug, User1, User2),
		user_confirm_bug(Bug, Response, User2, User)
	;
		user_confirm_bug_help(User1),
		user_confirm_bug(Bug, Response, User1, User)
	).

%-----------------------------------------------------------------------------%

	% Display the node in user readable form on the current
	% output stream.
	%
:- pred write_decl_question(decl_question, user_state, io__state, io__state).
:- mode write_decl_question(in, in, di, uo) is det.

write_decl_question(wrong_answer(Atom), User) -->
	write_decl_atom(User^outstr, "", Atom).
	
write_decl_question(missing_answer(Call, Solns), User) -->
	write_decl_atom(User^outstr, "Call ", Call),
	(
		{ Solns = [] }
	->
		io__write_string(User^outstr, "No solutions.\n")
	;
		io__write_string(User^outstr, "Solutions:\n"),
		list__foldl(write_decl_atom(User^outstr, "\t"), Solns)
	).

write_decl_question(unexpected_exception(Call, Exception), User) -->
	{ User = user(_, OutStr) },
	write_decl_atom(OutStr, "Call ", Call),
	io__write_string(OutStr, "Throws "),
	io__print(OutStr, Exception),
	io__nl(OutStr).

:- pred write_decl_bug(decl_bug, user_state, io__state, io__state).
:- mode write_decl_bug(in, in, di, uo) is det.

write_decl_bug(e_bug(EBug), User) -->
	(
		{ EBug = incorrect_contour(Atom, _, _) },
		io__write_string(User^outstr, "Found incorrect contour:\n"),
		write_decl_atom(User^outstr, "", Atom)
	;
		{ EBug = partially_uncovered_atom(Atom, _) },
		io__write_string(User^outstr,
				"Found partially uncovered atom:\n"),
		write_decl_atom(User^outstr, "", Atom)
	;
		{ EBug = unhandled_exception(Atom, Exception, _) },
		io__write_string(User^outstr, "Found unhandled exception:\n"),
		write_decl_atom(User^outstr, "", Atom),
		io__write(User^outstr, univ_value(Exception)),
		io__nl(User^outstr)
	).

write_decl_bug(i_bug(IBug), User) -->
	{ IBug = inadmissible_call(Parent, _, Call, _) },
	io__write_string(User^outstr, "Found inadmissible call:\n"),
	write_decl_atom(User^outstr, "Parent", Parent),
	write_decl_atom(User^outstr, "Call ", Call).

:- pred write_decl_atom(io__output_stream, string, decl_atom,
		io__state, io__state).
:- mode write_decl_atom(in, in, in, di, uo) is det.

write_decl_atom(OutStr, Indent, atom(Functor, Args)) -->
	io__write_string(OutStr, Indent),

		% XXX We should call the browser to print this.  But
		% that can wait until the browser has more flexible
		% term display facilities.
		%
	io__write_string(OutStr, Functor),
	(
		{ Args = [] }
	;
		{ Args = [Arg | Args0] },
		io__write_char(OutStr, '('),
		write_decl_atom_arg(OutStr, Arg),
		write_decl_atom_args(OutStr, Args0),
		io__write_char(OutStr, ')')
	),
	io__nl(OutStr).

:- pred write_decl_atom_args(io__output_stream, list(maybe(univ)),
		io__state, io__state).
:- mode write_decl_atom_args(in, in, di, uo) is det.

write_decl_atom_args(_, []) -->
	[].
write_decl_atom_args(OutStr, [Arg | Args]) -->
	io__write_string(OutStr, ", "),
	write_decl_atom_arg(OutStr, Arg),
	write_decl_atom_args(OutStr, Args).

:- pred write_decl_atom_arg(io__output_stream, maybe(univ),
		io__state, io__state).
:- mode write_decl_atom_arg(in, in, di, uo) is det.

write_decl_atom_arg(OutStr, yes(Arg)) -->
	io__print(OutStr, Arg).
write_decl_atom_arg(OutStr, no) -->
	io__write_char(OutStr, '_').

