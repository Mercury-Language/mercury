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
% edt_nodes in a human-readable format, and for getting responses to
% debugger queries from the user.
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

% :- pred confirm_user(list(decl_answer), user_response, user_state, user_state,
%		io__state, io__state).
% :- mode confirm_user(in, out, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module mdb__declarative_execution, mdb__util.
:- import_module std_util, char, string, bool.

:- type user_state
	--->	user(
			io__input_stream,
			io__output_stream
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

:- pred browse_edt_node(decl_question, user_state, user_state,
		io__state, io__state).
:- mode browse_edt_node(in, in, out, di, uo) is det.

browse_edt_node(_Node, User, User) -->
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

user_help_message(user(_, OutStr)) -->
	io__write_strings(OutStr, [
		"According to the intended interpretation of the program,",
		" answer one of:\n",
		"\ty\tyes\t\tthe node is correct\n",
		"\tn\tno\t\tthe node is incorrect\n",
%		"\ti\tinadmissible\tthe input arguments are out of range\n",
		"\ts\tskip\t\tskip this question\n",
		"\tr\trestart\t\task the skipped questions again\n",
%		"\tb\tbrowse\t\tbrowse the atom\n",
		"\ta\tabort\t\tabort this diagnosis session\n",
		"\th, ?\thelp\t\tthis help message\n"
	]).

:- pred get_command(string, user_command, user_state, user_state,
		io__state, io__state).
:- mode get_command(in, out, in, out, di, uo) is det.

get_command(Prompt, Command, User, User) -->
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

	% Display the node in user readable form on the current
	% output stream.
	%
:- pred write_decl_question(decl_question, user_state, io__state, io__state).
:- mode write_decl_question(in, in, di, uo) is det.

write_decl_question(wrong_answer(Atom), User) -->
	{ User = user(_, OutStr) },
	write_decl_atom(OutStr, "", Atom).
	
write_decl_question(missing_answer(Call, Solns), User) -->
	{ User = user(_, OutStr) },
	write_decl_atom(OutStr, "Call ", Call),
	io__write_string(OutStr, "Solutions:\n"),
	list__foldl(write_decl_atom(OutStr, "\t"), Solns).

:- pred write_decl_atom(io__output_stream, string, decl_atom,
		io__state, io__state).
:- mode write_decl_atom(in, in, in, di, uo) is det.

write_decl_atom(OutStr, Indent, Atom) -->
	io__write_string(OutStr, Indent),

		% XXX this looks horrible, but works for now.  We should
		% call the browser to print this.
		%
	io__write(OutStr, Atom),
	io__nl.

