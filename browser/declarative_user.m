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

:- type user_response(T)
	--->	user_answer(decl_question(T), decl_answer(T))
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
:- pred query_user(list(decl_question(T))::in, user_response(T)::out,
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
:- import_module std_util, char, string, bool, int, deconstruct.

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

query_user(Questions, Response, User0, User) -->
	query_user_2(Questions, [], Response, User0, User).

:- pred query_user_2(list(decl_question(T))::in, list(decl_question(T))::in,
	user_response(T)::out, user_state::in, user_state::out,
	io__state::di, io__state::uo) is cc_multi.

query_user_2([], _, no_user_answer, User, User) -->
	[].
query_user_2([Question | Questions], Skipped, Response, User0, User) -->
	write_decl_question(Question, User0),
	{ Node = get_decl_question_node(Question) },
	{ decl_question_prompt(Question, Prompt) },
	get_command(Prompt, Command, User0, User1),
	(
		{ Command = yes },
		{ Response = user_answer(Question, truth_value(Node, yes)) },
		{ User = User1 }
	;
		{ Command = no },
		{ Response = user_answer(Question, truth_value(Node, no)) },
		{ User = User1 }
	;
		{ Command = inadmissible },
		io__write_string("Sorry, not implemented,\n"),
		query_user_2([Question | Questions], Skipped, Response,
				User1, User)
	;
		{ Command = skip },
		query_user_2(Questions, [Question | Skipped], Response,
				User1, User)
	;
		{ Command = restart },
		{ reverse_and_append(Skipped, [Question | Questions],
				RestartedQuestions) },
		query_user(RestartedQuestions, Response, User1, User)
	;
		{ Command = browse(ArgNum) },
		browse_edt_node(Question, ArgNum, MaybeMark, User1, User2),
		(
			{ MaybeMark = no },
			query_user_2([Question | Questions], Skipped, Response,
					User2, User)
		;
			{ MaybeMark = yes(Mark) },
			{ Which = chosen_head_vars_presentation },
			{
				Which = only_user_headvars,
				ArgPos = user_head_var(ArgNum)
			;
				Which = all_headvars,
				ArgPos = any_head_var(ArgNum)
			},
			{ Answer = suspicious_subterm(Node, ArgPos, Mark) },
			{ Response = user_answer(Question, Answer) },
			{ User = User2 }
		)
	;
		{ Command = abort },
		{ Response = abort_diagnosis },
		{ User = User1 }
	;
		{ Command = help },
		user_help_message(User1),
		query_user_2([Question | Questions], Skipped, Response,
				User1, User)
	;
		{ Command = illegal_command },
		io__write_string("Unknown command, 'h' for help.\n"),
		query_user_2([Question | Questions], Skipped, Response,
				User1, User)
	).

:- pred decl_question_prompt(decl_question(T), string).
:- mode decl_question_prompt(in, out) is det.

decl_question_prompt(wrong_answer(_, _), "Valid? ").
decl_question_prompt(missing_answer(_, _, _), "Complete? ").
decl_question_prompt(unexpected_exception(_, _, _), "Expected? ").

:- pred browse_edt_node(decl_question(T)::in, int::in, maybe(term_path)::out,
	user_state::in, user_state::out, io__state::di, io__state::uo)
	is cc_multi.

browse_edt_node(Node, ArgNum, MaybeMark, User0, User) -->
	{
		Node = wrong_answer(_, Atom)
	;
		Node = missing_answer(_, Atom, _)
	;
		Node = unexpected_exception(_, Atom, _)
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
	{ Atom = atom(_, _, Args0) },
	{ maybe_filter_headvars(chosen_head_vars_presentation, Args0, Args) },
	(
		{ list__index1(Args, ArgNum, ArgInfo) },
		{ ArgInfo = arg_info(_, _, MaybeArg) },
		{ MaybeArg = yes(Arg) }
	->
		browse(univ_value(Arg), User0 ^ instr, User0 ^ outstr,
			MaybeDirs, User0 ^ browser, Browser),
		{ maybe_convert_dirs_to_path(MaybeDirs, MaybeMark) },
		{ User = User0 ^ browser := Browser }
	;
		io__write_string(User ^ outstr, "Invalid argument number\n"),
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
	io__write_strings(User ^ outstr, [
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
	io__write_strings(User ^ outstr, [
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
	util__trace_getline(Prompt, Result, User ^ instr, User ^ outstr),
	(
		{ Result = ok(String) },
		{ Words = string__words(char__is_whitespace, String) },
		{
			Words = [CmdWord | CmdArgs],
			cmd_handler(CmdWord, CmdHandler),
			CommandPrime = CmdHandler(CmdArgs)
		->
			Command = CommandPrime
		;
			Command = illegal_command
		}
	;
		{ Result = error(Error) },
		{ io__error_message(Error, Msg) },
		io__write_string(User ^ outstr, Msg),
		io__nl(User ^ outstr),
		{ Command = abort }
	;
		{ Result = eof },
		{ Command = abort }
	).

:- pred cmd_handler(string, func(list(string)) = user_command).
:- mode cmd_handler(in, out((func(in) = out is semidet))) is semidet.

cmd_handler("y",	one_word_cmd(yes)).
cmd_handler("yes",	one_word_cmd(yes)).
cmd_handler("n",	one_word_cmd(no)).
cmd_handler("no",	one_word_cmd(no)).
cmd_handler("in",	one_word_cmd(inadmissible)).
cmd_handler("inadmissible", one_word_cmd(inadmissible)).
cmd_handler("s",	one_word_cmd(skip)).
cmd_handler("skip",	one_word_cmd(skip)).
cmd_handler("r",	one_word_cmd(restart)).
cmd_handler("restart",	one_word_cmd(restart)).
cmd_handler("a",	one_word_cmd(abort)).
cmd_handler("abort",	one_word_cmd(abort)).
cmd_handler("?",	one_word_cmd(help)).
cmd_handler("h",	one_word_cmd(help)).
cmd_handler("help",	one_word_cmd(help)).
cmd_handler("b",	browse_cmd).
cmd_handler("browse",	browse_cmd).

:- func one_word_cmd(user_command::in, list(string)::in) = (user_command::out)
	is semidet.

one_word_cmd(Cmd, []) = Cmd.

:- func browse_cmd(list(string)::in) = (user_command::out) is semidet.

browse_cmd([Arg]) = browse(ArgNum) :-
	string__to_int(Arg, ArgNum).

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
:- pred write_decl_question(decl_question(T)::in, user_state::in,
	io__state::di, io__state::uo) is cc_multi.

write_decl_question(wrong_answer(_, Atom), User) -->
	write_decl_atom(User, "", Atom).
	
write_decl_question(missing_answer(_, Call, Solns), User) -->
	write_decl_atom(User, "Call ", Call),
	(
		{ Solns = [] }
	->
		io__write_string(User ^ outstr, "No solutions.\n")
	;
		io__write_string(User ^ outstr, "Solutions:\n"),
		list__foldl(write_decl_atom(User, "\t"), Solns)
	).

write_decl_question(unexpected_exception(_, Call, Exception), User) -->
	write_decl_atom(User, "Call ", Call),
	io__write_string(User ^ outstr, "Throws "),
	io__write(User ^ outstr, include_details_cc, univ_value(Exception)),
	io__nl(User ^ outstr).

:- pred write_decl_bug(decl_bug::in, user_state::in,
	io__state::di, io__state::uo) is cc_multi.

write_decl_bug(e_bug(EBug), User) -->
	(
		{ EBug = incorrect_contour(Atom, _, _) },
		io__write_string(User ^ outstr, "Found incorrect contour:\n"),
		write_decl_atom(User, "", Atom)
	;
		{ EBug = partially_uncovered_atom(Atom, _) },
		io__write_string(User ^ outstr,
				"Found partially uncovered atom:\n"),
		write_decl_atom(User, "", Atom)
	;
		{ EBug = unhandled_exception(Atom, Exception, _) },
		io__write_string(User ^ outstr, "Found unhandled exception:\n"),
		write_decl_atom(User, "", Atom),
		io__write(User ^ outstr, include_details_cc,
				univ_value(Exception)),
		io__nl(User ^ outstr)
	).

write_decl_bug(i_bug(IBug), User) -->
	{ IBug = inadmissible_call(Parent, _, Call, _) },
	io__write_string(User ^ outstr, "Found inadmissible call:\n"),
	write_decl_atom(User, "Parent ", Parent),
	write_decl_atom(User, "Call ", Call).

:- pred write_decl_atom(user_state::in, string::in, decl_atom::in,
	io__state::di, io__state::uo) is cc_multi.

write_decl_atom(User, Indent, Atom) -->
	io__write_string(User ^ outstr, Indent),
		%
		% Check whether the atom is likely to fit on one line.
		% If it's not, then call the browser to print the term
		% to a limited depth.  If it is, then we prefer to print
		% it out directly so that all arguments are put on the
		% same line.
		%
	{ Which = chosen_head_vars_presentation },
	{ check_decl_atom_size(Indent, Which, Atom, RemSize) },
	( { RemSize > 0 } ->
		write_decl_atom_direct(User ^ outstr, Atom, Which)
	;
		write_decl_atom_limited(User, Atom, Which)
	).

:- pred check_decl_atom_size(string::in, which_headvars::in, decl_atom::in,
	int::out) is cc_multi.

check_decl_atom_size(Indent, Which, atom(_, Functor, Args), RemSize) :-
	decl_atom_size_limit(RemSize0),
	string__length(Indent, I),
	string__length(Functor, F),
	P = 2,		% parentheses
	RemSize1 = RemSize0 - I - F - P,
	size_left_after_args(Args, Which, RemSize1, RemSize).

:- pred size_left_after_args(list(trace_atom_arg)::in, which_headvars::in,
	int::in, int::out) is cc_multi.

size_left_after_args([], _) -->
	[].
size_left_after_args([arg_info(UserVis, _, MaybeUniv) | Args], Which) -->
	(
		{ MaybeUniv = yes(Univ) },
		(
			{ Which = only_user_headvars },
			{ UserVis = no }
		->
			% This argument won't be printed.
			[]
		;
			term_size_left_from_max(Univ)
		)
	;
		{ MaybeUniv = no }
	),
	size_left_after_args(Args, Which).

:- pred decl_atom_size_limit(int).
:- mode decl_atom_size_limit(out) is det.

decl_atom_size_limit(79).

:- pred write_decl_atom_limited(user_state::in, decl_atom::in,
	which_headvars::in, io__state::di, io__state::uo) is cc_multi.

write_decl_atom_limited(User, atom(PredOrFunc, Functor, Args0), Which) -->
	write_decl_atom_category(User ^ outstr, PredOrFunc),
	io__write_string(User ^ outstr, Functor),
	io__nl(User ^ outstr),
	{ maybe_filter_headvars(Which, Args0, Args) },
	foldl(print_decl_atom_arg(User), Args).

:- pred write_decl_atom_category(io__output_stream::in, pred_or_func::in,
	io__state::di, io__state::uo) is det.

write_decl_atom_category(OutStr, predicate) -->
	io__write_string(OutStr, "pred ").
write_decl_atom_category(OutStr, function) -->
	io__write_string(OutStr, "func ").

:- pred print_decl_atom_arg(user_state::in, trace_atom_arg::in,
	io__state::di, io__state::uo) is cc_multi.

print_decl_atom_arg(User, arg_info(_, _, MaybeArg)) -->
	(
		{ MaybeArg = yes(Arg) },
		io__write_string(User ^ outstr, "\t"),
		browse__print(univ_value(Arg), User ^ outstr, print_all,
			User ^ browser)
	;
		{ MaybeArg = no },
		io__write_string(User ^ outstr, "\t_\n")
	).

:- pred write_decl_atom_direct(io__output_stream::in, decl_atom::in,
	which_headvars::in, io__state::di, io__state::uo) is cc_multi.

write_decl_atom_direct(OutStr, atom(PredOrFunc, Functor, Args0), Which) -->
	io__write_string(OutStr, Functor),
	{ maybe_filter_headvars(Which, Args0, Args) },
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

:- pred write_decl_atom_arg(io__output_stream, trace_atom_arg,
		io__state, io__state).
:- mode write_decl_atom_arg(in, in, di, uo) is cc_multi.

write_decl_atom_arg(OutStr, arg_info(_, _, MaybeArg)) -->
	(
		{ MaybeArg = yes(Arg) },
		io__write(OutStr, include_details_cc, univ_value(Arg))
	;
		{ MaybeArg = no },
		io__write_char(OutStr, '_')
	).

:- pred get_inputs_and_result(T, list(T), list(T), T).
:- mode get_inputs_and_result(in, in, out, out) is det.

get_inputs_and_result(A, [], [], A).
get_inputs_and_result(A1, [A2 | As], [A1 | Inputs0], Result) :-
	get_inputs_and_result(A2, As, Inputs0, Result).

%-----------------------------------------------------------------------------%
