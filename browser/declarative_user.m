%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2002 The University of Melbourne.
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
:- pred query_user(list(decl_question)::in, user_response::out,
	user_state::in, user_state::out, io__state::di, io__state::uo)
	is cc_multi.

	% Confirm that the node found is indeed an e_bug or an i_bug.
	%
:- pred user_confirm_bug(decl_bug::in, decl_confirmation::out,
	user_state::in, user_state::out, io__state::di, io__state::uo)
	is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module mdb__browser_info, mdb__browse, mdb__util.
:- import_module mdb__declarative_execution, mdb__program_representation.
:- import_module std_util, char, string, bool, int.

:- type user_state
	--->	user(
			instr	:: io__input_stream,
			outstr	:: io__output_stream,
			browser	:: browser_persistent_state
		).

user_state_init(InStr, OutStr, User) :-
	browser_info__init_persistent_state(Browser),
	User = user(InStr, OutStr, Browser).

%-----------------------------------------------------------------------------%

query_user(Nodes, Response, User0, User) -->
	query_user_2(Nodes, [], Response, User0, User).

:- pred query_user_2(list(decl_question)::in, list(decl_question)::in,
	user_response::out, user_state::in, user_state::out,
	io__state::di, io__state::uo) is cc_multi.

query_user_2([], _, no_user_answer, User, User) -->
	[].
query_user_2([Node | Nodes], Skipped, Response, User0, User) -->
	write_decl_question(Node, User0),
	{ decl_question_prompt(Node, Question) },
	get_command(Question, Command, User0, User1),
	(
		{ Command = yes },
		{ Response = user_answer(truth_value(Node, yes)) },
		{ User = User1 }
	;
		{ Command = no },
		{ Response = user_answer(truth_value(Node, no)) },
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
		{ Command = browse(Arg) },
		browse_edt_node(Node, Arg, MaybeMark, User1, User2),
		(
			{ MaybeMark = no },
			query_user_2([Node | Nodes], Skipped, Response, User2,
					User)
		;
			{ MaybeMark = yes(Mark) },
			{ Answer = suspicious_subterm(Node, Arg, Mark) },
			{ Response = user_answer(Answer) },
			{ User = User2 }
		)
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

:- pred browse_edt_node(decl_question::in, int::in, maybe(term_path)::out,
	user_state::in, user_state::out, io__state::di, io__state::uo)
	is cc_multi.

browse_edt_node(Node, ArgNum, MaybeMark, User0, User) -->
	{
		Node = wrong_answer(Atom)
	;
		Node = missing_answer(Atom, _)
	;
		Node = unexpected_exception(Atom, _)
	},
	browse_atom_argument(Atom, ArgNum, MaybeMark, User0, User).

:- pred browse_decl_bug(decl_bug::in, int::in, user_state::in, user_state::out,
	io__state::di, io__state::uo) is cc_multi.

browse_decl_bug(Bug, ArgNum, User0, User) -->
	{
		Bug = e_bug(EBug),
		(
			EBug = incorrect_contour(Atom, _, _)
		;
			EBug = partially_uncovered_atom(Atom, _)
		;
			EBug = unhandled_exception(Atom, _, _)
		)
	;
		Bug = i_bug(inadmissible_call(_, _, Atom, _))
	},
	browse_atom_argument(Atom, ArgNum, _, User0, User).

:- pred browse_atom_argument(decl_atom::in, int::in, maybe(term_path)::out,
	user_state::in, user_state::out, io__state::di, io__state::uo)
	is cc_multi.

browse_atom_argument(Atom, ArgNum, MaybeMark, User0, User) -->
	{ Atom = atom(_, _, Args) },
	(
		{ list__index1(Args, ArgNum, MaybeArg) },
		{ MaybeArg = yes(Arg) }
	->
		browse(univ_value(Arg), User0^instr, User0^outstr, MaybeDirs,
			User0^browser, Browser),
		{ maybe_convert_dirs_to_path(MaybeDirs, MaybeMark) },
		{ User = User0^browser := Browser }
	;
		io__write_string(User^outstr, "Invalid argument number\n"),
		{ MaybeMark = no },
		{ User = User0 }
	).

:- pred maybe_convert_dirs_to_path(maybe(list(dir)), maybe(term_path)).
:- mode maybe_convert_dirs_to_path(in, out) is det.

maybe_convert_dirs_to_path(no, no).
maybe_convert_dirs_to_path(yes(Dirs), yes(TermPath)) :-
	convert_dirs_to_term_path(Dirs, TermPath).

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
	;	browse(int)		% Browse the nth argument before
					% answering.
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
		"\tb <n>\tbrowse <n>\tbrowse the nth argument of the atom\n",
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
command_chars(['a' | _], abort).
command_chars(['h' | _], help).
command_chars(['?' | _], help).
command_chars(['b' | Line0], browse(Arg)) :-
	(
		Line0 = ['r','o','w','s','e' | Line1]
	->
		Line2 = Line1
	;
		Line2 = Line0
	),
	list__takewhile(char__is_whitespace, Line2, _, ArgChars),
	parse_integer(ArgChars, 0, Arg).

:- pred parse_integer(list(char), int, int).
:- mode parse_integer(in, in, out) is semidet.

parse_integer([], N, N).
parse_integer([D | Ds], N0, N) :-
	char__digit_to_int(D, I),
	parse_integer(Ds, N0 * 10 + I, N).

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
		{ Command = browse(Arg) }
	->
		browse_decl_bug(Bug, Arg, User1, User2),
		user_confirm_bug(Bug, Response, User2, User)
	;
		user_confirm_bug_help(User1),
		user_confirm_bug(Bug, Response, User1, User)
	).

%-----------------------------------------------------------------------------%

	% Display the node in user readable form on the current
	% output stream.
	%
:- pred write_decl_question(decl_question::in, user_state::in,
	io__state::di, io__state::uo) is cc_multi.

write_decl_question(wrong_answer(Atom), User) -->
	write_decl_atom(User, "", Atom).
	
write_decl_question(missing_answer(Call, Solns), User) -->
	write_decl_atom(User, "Call ", Call),
	(
		{ Solns = [] }
	->
		io__write_string(User^outstr, "No solutions.\n")
	;
		io__write_string(User^outstr, "Solutions:\n"),
		list__foldl(write_decl_atom(User, "\t"), Solns)
	).

write_decl_question(unexpected_exception(Call, Exception), User) -->
	write_decl_atom(User, "Call ", Call),
	io__write_string(User^outstr, "Throws "),
	io__print(User^outstr, Exception),
	io__nl(User^outstr).

:- pred write_decl_bug(decl_bug::in, user_state::in,
	io__state::di, io__state::uo) is cc_multi.

write_decl_bug(e_bug(EBug), User) -->
	(
		{ EBug = incorrect_contour(Atom, _, _) },
		io__write_string(User^outstr, "Found incorrect contour:\n"),
		write_decl_atom(User, "", Atom)
	;
		{ EBug = partially_uncovered_atom(Atom, _) },
		io__write_string(User^outstr,
				"Found partially uncovered atom:\n"),
		write_decl_atom(User, "", Atom)
	;
		{ EBug = unhandled_exception(Atom, Exception, _) },
		io__write_string(User^outstr, "Found unhandled exception:\n"),
		write_decl_atom(User, "", Atom),
		io__write(User^outstr, univ_value(Exception)),
		io__nl(User^outstr)
	).

write_decl_bug(i_bug(IBug), User) -->
	{ IBug = inadmissible_call(Parent, _, Call, _) },
	io__write_string(User^outstr, "Found inadmissible call:\n"),
	write_decl_atom(User, "Parent ", Parent),
	write_decl_atom(User, "Call ", Call).

:- pred write_decl_atom(user_state::in, string::in, decl_atom::in,
	io__state::di, io__state::uo) is cc_multi.

write_decl_atom(User, Indent, Atom) -->
	io__write_string(User^outstr, Indent),
		%
		% Check whether the atom is likely to fit on one line.
		% If it's not, then call the browser to print the term
		% to a limited depth.  If it is, then we prefer to print
		% it out directly so that all arguments are put on the
		% same line.
		%
	{ check_decl_atom_size(Indent, Atom, RemSize) },
	( { RemSize > 0 } ->
		write_decl_atom_direct(User^outstr, Atom)
	;
		write_decl_atom_limited(Atom, User)
	).

:- pred check_decl_atom_size(string, decl_atom, int).
:- mode check_decl_atom_size(in, in, out) is cc_multi.

check_decl_atom_size(Indent, atom(_, Functor, Args), RemSize) :-
	decl_atom_size_limit(RemSize0),
	string__length(Indent, I),
	string__length(Functor, F),
	P = 2,		% parentheses
	RemSize1 = RemSize0 - I - F - P,
	size_left_after_args(Args, RemSize1, RemSize).

:- pred size_left_after_args(list(maybe(univ)), int, int).
:- mode size_left_after_args(in, in, out) is cc_multi.

size_left_after_args([]) -->
	[].
size_left_after_args([yes(A) | As]) -->
	term_size_left_from_max(A),
	size_left_after_args(As).
size_left_after_args([no | As]) -->
	size_left_after_args(As).

:- pred decl_atom_size_limit(int).
:- mode decl_atom_size_limit(out) is det.

decl_atom_size_limit(79).

:- pred write_decl_atom_limited(decl_atom::in, user_state::in,
	io__state::di, io__state::uo) is cc_multi.

write_decl_atom_limited(atom(PredOrFunc, Functor, Args), User) -->
	write_decl_atom_category(User^outstr, PredOrFunc),
	io__write_string(User^outstr, Functor),
	io__nl(User^outstr),
	foldl(print_decl_atom_arg(User), Args).

:- pred write_decl_atom_category(io__output_stream, pred_or_func, io__state,
		io__state).
:- mode write_decl_atom_category(in, in, di, uo) is det.

write_decl_atom_category(OutStr, predicate) -->
	io__write_string(OutStr, "pred ").
write_decl_atom_category(OutStr, function) -->
	io__write_string(OutStr, "func ").

:- pred print_decl_atom_arg(user_state::in, maybe(univ)::in,
	io__state::di, io__state::uo) is cc_multi.

print_decl_atom_arg(User, yes(Arg)) -->
	io__write_string(User^outstr, "\t"),
	browse__print(univ_value(Arg), User^outstr, print_all, User^browser).
print_decl_atom_arg(User, no) -->
	io__write_string(User^outstr, "\t_\n").

:- pred write_decl_atom_direct(io__output_stream, decl_atom,
		io__state, io__state).
:- mode write_decl_atom_direct(in, in, di, uo) is det.

write_decl_atom_direct(OutStr, atom(PredOrFunc, Functor, Args)) -->
	io__write_string(OutStr, Functor),
	(
		{ Args = [] }
	;
		{ Args = [FirstArg | ArgsRest] },
		io__write_char(OutStr, '('),
		(
			{ PredOrFunc = predicate },
			io__write_list(OutStr, Args, ", ",
					write_decl_atom_arg(OutStr)),
			io__write_char(OutStr, ')')
		;
			{ PredOrFunc = function },
			{ get_inputs_and_result(FirstArg, ArgsRest, Inputs,
					Result) },
			io__write_list(OutStr, Inputs, ", ",
					write_decl_atom_arg(OutStr)),
			io__write_string(OutStr, ") = "),
			write_decl_atom_arg(OutStr, Result)
		)
	),
	io__nl(OutStr).

:- pred write_decl_atom_arg(io__output_stream, maybe(univ),
		io__state, io__state).
:- mode write_decl_atom_arg(in, in, di, uo) is det.

write_decl_atom_arg(OutStr, yes(Arg)) -->
	io__print(OutStr, Arg).
write_decl_atom_arg(OutStr, no) -->
	io__write_char(OutStr, '_').

:- pred get_inputs_and_result(T, list(T), list(T), T).
:- mode get_inputs_and_result(in, in, out, out) is det.

get_inputs_and_result(A, [], [], A).
get_inputs_and_result(A1, [A2 | As], [A1 | Inputs0], Result) :-
	get_inputs_and_result(A2, As, Inputs0, Result).

