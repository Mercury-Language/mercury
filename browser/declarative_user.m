%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2005 The University of Melbourne.
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

:- module mdb.declarative_user.

:- interface.

:- import_module mdb.browser_info.
:- import_module mdb.declarative_debugger.

:- import_module io.

:- type user_question(T)
	--->	plain_question(decl_question(T))
	;	question_with_default(decl_question(T), decl_truth).

:- type user_response(T)
	--->	user_answer(decl_question(T), decl_answer(T))
	;	trust_predicate(decl_question(T))
	;	trust_module(decl_question(T))
	;	exit_diagnosis(T)
	;	abort_diagnosis.

:- type user_state.

:- pred user_state_init(io.input_stream::in, io.output_stream::in, 
	browser_info.browser_persistent_state::in, user_state::out) is det.

	% This predicate handles the interactive part of the declarative
	% debugging process.  The user is presented with a question,
	% possibly with a default answer, and is asked to respond about the
	% truth of it in the intended interpretation.
	%
:- pred query_user(user_question(T)::in, user_response(T)::out,
	user_state::in, user_state::out, io::di, io::uo) is cc_multi.

	% Confirm that the node found is indeed an e_bug or an i_bug.
	%
:- pred user_confirm_bug(decl_bug::in, decl_confirmation::out,
	user_state::in, user_state::out, io::di, io::uo) is cc_multi.

	% Returns the state of the term browser.
	%
:- func get_browser_state(user_state) = browser_info.browser_persistent_state.

	% Sets the state of the term browser.
	%
:- pred set_browser_state(browser_info.browser_persistent_state::in, 
	user_state::in, user_state::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.browse.
:- import_module mdb.browser_term.
:- import_module mdb.io_action.
:- import_module mdb.util.
:- import_module mdb.declarative_execution.
:- import_module mdb.declarative_tree.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdb.parse.
:- import_module mdb.term_rep.

:- import_module std_util, char, string, bool, int, deconstruct, getopt, list.

:- type user_state
	--->	user(
			instr	:: io.input_stream,
			outstr	:: io.output_stream,
			browser	:: browser_persistent_state
		).

user_state_init(InStr, OutStr, Browser, user(InStr, OutStr, Browser)).

%-----------------------------------------------------------------------------%

query_user(UserQuestion, Response, !User, !IO) :-
	Question = get_decl_question(UserQuestion),
	write_decl_question(Question, !.User, !IO),
	user_question_prompt(UserQuestion, Prompt),
	get_command(Prompt, Command, !User, !IO),
	handle_command(Command, UserQuestion, Response, !User, 
		!IO).

:- pred handle_command(user_command::in, user_question(T)::in,
	user_response(T)::out, user_state::in, user_state::out, 
	io::di, io::uo) is cc_multi.

handle_command(yes, UserQuestion, Response, !User, !IO) :-
	Question = get_decl_question(UserQuestion),
	Node = get_decl_question_node(Question),
	Response = user_answer(Question, truth_value(Node, correct)).

handle_command(no, UserQuestion, Response, !User, !IO) :-
	Question = get_decl_question(UserQuestion),
	Node = get_decl_question_node(Question),
	Response = user_answer(Question, truth_value(Node, erroneous)).

handle_command(inadmissible, UserQuestion, Response, !User, !IO) :-
	Question = get_decl_question(UserQuestion),
	Node = get_decl_question_node(Question),
	Response = user_answer(Question, truth_value(Node, inadmissible)).

handle_command(skip, UserQuestion, Response, !User, !IO) :-
	Question = get_decl_question(UserQuestion),
	Node = get_decl_question_node(Question),
	Response = user_answer(Question, skip(Node)).

handle_command(browse_arg(MaybeArgNum), UserQuestion, Response, 
		!User, !IO) :-
	Question = get_decl_question(UserQuestion),
	edt_node_trace_atoms(Question, InitAtom, FinalAtom),
	(
		MaybeArgNum = yes(ArgNum),
		browse_atom_argument(InitAtom, FinalAtom, ArgNum, MaybeMark, 
			!User, !IO),
		(
			MaybeMark = no,
			query_user(UserQuestion, Response, 
				!User, !IO)
		;
			MaybeMark = yes(Mark),
			ArgPos = arg_num_to_arg_pos(ArgNum),
			Node = get_decl_question_node(Question),
			Answer = suspicious_subterm(Node, ArgPos, Mark),
			Response = user_answer(Question, Answer)
		)
	;
		MaybeArgNum = no,
		browse_atom(InitAtom, FinalAtom, MaybeMark, !User, !IO),
		(
			MaybeMark = no,
			query_user(UserQuestion, Response, 
				!User, !IO)
		;
			% If the user marks the predicate or function,
			% we make the call invalid.
			MaybeMark = yes([]),
			Node = get_decl_question_node(Question),
			Answer = truth_value(Node, erroneous),
			Response = user_answer(Question, Answer)
		;
			MaybeMark = yes([ArgNum | Mark]),
			ArgPos = arg_num_to_arg_pos(ArgNum),
			Node = get_decl_question_node(Question),
			Answer = suspicious_subterm(Node, ArgPos, Mark),
			Response = user_answer(Question, Answer)
		)
	).

handle_command(print_arg(From, To), UserQuestion, Response, 
		!User, !IO) :-
	Question = get_decl_question(UserQuestion),
	edt_node_trace_atoms(Question, _, TraceAtom),
	print_atom_arguments(TraceAtom, From, To, !.User, !IO),
	query_user(UserQuestion, Response, !User, !IO).

handle_command(set(MaybeOptionTable, Setting), UserQuestion, Response, !User,
		!IO) :-
	(
		MaybeOptionTable = ok(OptionTable),
		browser_info.set_param(no, OptionTable, Setting, 
			!.User ^ browser, Browser),
		!:User = !.User ^ browser := Browser
	;
		MaybeOptionTable = error(Msg),
		io.write_string(Msg++"\n", !IO)
	),
	query_user(UserQuestion, Response, !User, !IO).

handle_command(trust_predicate, UserQuestion, trust_predicate(Question), 
		!User, !IO) :-
	Question = get_decl_question(UserQuestion).

handle_command(trust_module, UserQuestion, trust_module(Question), 
		!User, !IO) :-
	Question = get_decl_question(UserQuestion).

handle_command(browse_io(ActionNum), UserQuestion, Response, 
		!User, !IO) :-
	Question = get_decl_question(UserQuestion),
	edt_node_io_actions(Question, IoActions),
	% We don't have code yet to trace a marked I/O action.
	browse_chosen_io_action(IoActions, ActionNum, _MaybeMark, !User, !IO),
	query_user(UserQuestion, Response, !User, !IO).

handle_command(print_io(From, To), UserQuestion, Response, 
		!User, !IO) :-
	Question = get_decl_question(UserQuestion),
	edt_node_io_actions(Question, IoActions),
	print_chosen_io_actions(IoActions, From, To, !.User, !IO),
	query_user(UserQuestion, Response, !User, !IO).

handle_command(pd, UserQuestion, Response, !User, !IO) :-
	Question = get_decl_question(UserQuestion),
	Node = get_decl_question_node(Question),
	Response = exit_diagnosis(Node).

handle_command(abort, _, Response, !User, !IO) :-
	Response = abort_diagnosis.

handle_command(help, UserQuestion, Response, !User, !IO) :-
	user_help_message(!.User, !IO),
	query_user(UserQuestion, Response, !User, !IO).

handle_command(empty_command, UserQuestion, Response, !User, 
		!IO) :-
	(
		UserQuestion = plain_question(_),
		Command = skip
	;
		UserQuestion = question_with_default(_, Truth),
		(
			Truth = correct,
			Command = yes
		;
			Truth = erroneous,
			Command = no
		;
			Truth = inadmissible,
			Command = inadmissible
		)
	),
	handle_command(Command, UserQuestion, Response, !User, 
		!IO).

handle_command(illegal_command, UserQuestion, Response, !User, 
		!IO) :-
	io.write_string("Unknown command, 'h' for help.\n", !IO),
	query_user(UserQuestion, Response, !User, !IO).

:- func arg_num_to_arg_pos(int) = arg_pos.

arg_num_to_arg_pos(ArgNum) = ArgPos :-
	Which = chosen_head_vars_presentation,
	(
		Which = only_user_headvars,
		ArgPos = user_head_var(ArgNum)
	;
		Which = all_headvars,
		ArgPos = any_head_var(ArgNum)
	).

:- func get_decl_question(user_question(T)) = decl_question(T).

get_decl_question(plain_question(Q)) = Q.
get_decl_question(question_with_default(Q, _)) = Q.

:- pred user_question_prompt(user_question(T)::in, string::out) is det.

user_question_prompt(plain_question(Question), Prompt) :-
	decl_question_prompt(Question, Prompt).

user_question_prompt(question_with_default(Question, DefaultTruth), Prompt) :-
	decl_question_prompt(Question, QuestionPrompt),
	default_prompt(DefaultTruth, DefaultPrompt),
	string.append(QuestionPrompt, DefaultPrompt, Prompt).

:- pred decl_question_prompt(decl_question(T)::in, string::out) is det.

decl_question_prompt(wrong_answer(_, _, _), "Valid? ").
decl_question_prompt(missing_answer(_, _, [_ | _]), "Complete? ").
decl_question_prompt(missing_answer(_, _, []), "Unsatisfiable? ").
decl_question_prompt(unexpected_exception(_, _, _), "Expected? ").

:- pred default_prompt(decl_truth::in, string::out) is det.

default_prompt(correct, "[yes] ").
default_prompt(erroneous, "[no] ").
default_prompt(inadmissible, "[inadmissible] ").

	% Find the initial and final atoms for a question.  For all 
	% questions besides wrong answer questions the initial and
	% final atoms will be the same.
	%
:- pred edt_node_trace_atoms(decl_question(T)::in, trace_atom::out,
	trace_atom::out) is det.

edt_node_trace_atoms(wrong_answer(_, InitDeclAtom, FinalDeclAtom),
	InitDeclAtom ^ init_atom, FinalDeclAtom ^ final_atom).
edt_node_trace_atoms(missing_answer(_, InitDeclAtom, _),
	InitDeclAtom ^ init_atom, InitDeclAtom ^ init_atom).
edt_node_trace_atoms(unexpected_exception(_, InitDeclAtom, _),
	InitDeclAtom ^ init_atom, InitDeclAtom ^ init_atom).

:- pred edt_node_io_actions(decl_question(T)::in, list(io_action)::out) is det.

edt_node_io_actions(wrong_answer(_, _, FinalDeclAtom),
	FinalDeclAtom ^ final_io_actions).
edt_node_io_actions(missing_answer(_, _, _), []).
edt_node_io_actions(unexpected_exception(_, _, _), []).

:- pred decl_bug_trace_atom(decl_bug::in, trace_atom::out, trace_atom::out) 
	is det.

decl_bug_trace_atom(e_bug(incorrect_contour(InitDeclAtom, FinalDeclAtom, _, 
	_)), InitDeclAtom ^ init_atom, FinalDeclAtom ^ final_atom).
decl_bug_trace_atom(e_bug(partially_uncovered_atom(InitDeclAtom, _)),
	InitDeclAtom ^ init_atom, InitDeclAtom ^ init_atom).
decl_bug_trace_atom(e_bug(unhandled_exception(InitDeclAtom, _, _)),
	InitDeclAtom ^ init_atom, InitDeclAtom ^ init_atom).
decl_bug_trace_atom(i_bug(inadmissible_call(_, _, InitDeclAtom, _)),
	InitDeclAtom ^ init_atom, InitDeclAtom ^ init_atom).

:- pred decl_bug_io_actions(decl_bug::in, list(io_action)::out) is det.

decl_bug_io_actions(e_bug(incorrect_contour(_, FinalDeclAtom, _, _)),
	FinalDeclAtom ^ final_io_actions).
decl_bug_io_actions(e_bug(partially_uncovered_atom(_, _)), []).
decl_bug_io_actions(e_bug(unhandled_exception(_, _, _)), []).
decl_bug_io_actions(i_bug(inadmissible_call(_, _, _, _)), []).

:- pred browse_chosen_io_action(list(io_action)::in, int::in,
	maybe(term_path)::out, user_state::in, user_state::out,
	io::di, io::uo) is cc_multi.

browse_chosen_io_action(IoActions, ActionNum, MaybeMark, User0, User, !IO) :-
	( list.index1(IoActions, ActionNum, IoAction) ->
		browse_io_action(IoAction, MaybeMark, User0, User, !IO)
	;
		io.write_string("No such IO action.\n", !IO),
		MaybeMark = no,
		User = User0
	).

:- pred print_chosen_io_actions(list(io_action)::in, int::in, int::in,
	user_state::in, io::di, io::uo) is cc_multi.

print_chosen_io_actions(Atom, From, To, User0, !IO) :-
	print_chosen_io_action(Atom, From, User0, OK, !IO),
	( OK = yes, From + 1 =< To ->
		print_chosen_io_actions(Atom, From + 1, To, User0, !IO)
	;
		true
	).

:- pred print_chosen_io_action(list(io_action)::in, int::in, user_state::in,
	bool::out, io::di, io::uo) is cc_multi.

print_chosen_io_action(IoActions, ActionNum, User0, OK, !IO) :-
	( list.index1(IoActions, ActionNum, IoAction) ->
		print_io_action(User0, IoAction, !IO),
		OK = yes
	;
		io.write_string("No such IO action.\n", !IO),
		OK = no
	).

:- pred browse_io_action(io_action::in, maybe(term_path)::out,
	user_state::in, user_state::out, io::di, io::uo) is cc_multi.

browse_io_action(IoAction, MaybeMark, !User, !IO) :-
	Term = io_action_to_browser_term(IoAction),
	browse_browser_term(Term, !.User ^ instr, !.User ^ outstr, no,
		MaybeDirs, !.User ^ browser, Browser, !IO),
	maybe_convert_dirs_to_path(MaybeDirs, MaybeMark),
	!:User = !.User ^ browser := Browser.

:- pred browse_decl_bug(decl_bug::in, maybe(int)::in, user_state::in,
	user_state::out, io::di, io::uo) is cc_multi.

browse_decl_bug(Bug, MaybeArgNum, !User, !IO) :-
	decl_bug_trace_atom(Bug, InitAtom, FinalAtom),
	(
		MaybeArgNum = yes(ArgNum),
		browse_atom_argument(InitAtom, FinalAtom, ArgNum, _, !User, 
			!IO)
	;
		MaybeArgNum = no,
		browse_atom(InitAtom, FinalAtom, _, !User, !IO)
	).

:- pred browse_atom_argument(trace_atom::in, trace_atom::in, int::in, 
	maybe(term_path)::out, user_state::in, user_state::out, 
	io::di, io::uo) is cc_multi.

browse_atom_argument(InitAtom, FinalAtom, ArgNum, MaybeMark, !User, !IO) :-
	FinalAtom = atom(_, Args0),
	maybe_filter_headvars(chosen_head_vars_presentation, Args0, Args),
	(
		list.index1(Args, ArgNum, ArgInfo),
		ArgInfo = arg_info(_, _, MaybeArg),
		MaybeArg = yes(ArgRep),
		term_rep.rep_to_univ(ArgRep, Arg)
	->
		browse_browser_term(univ_to_browser_term(Arg),
			!.User ^ instr, !.User ^ outstr,
			yes(get_subterm_mode_from_atoms_for_arg(ArgNum, 
				InitAtom, FinalAtom)),
			MaybeDirs, !.User ^ browser, Browser, !IO),
		maybe_convert_dirs_to_path(MaybeDirs, MaybeMark),
		!:User = !.User ^ browser := Browser
	;
		io.write_string(!.User ^ outstr, "Invalid argument number\n",
			!IO),
		MaybeMark = no
	).

:- pred browse_atom(trace_atom::in, trace_atom::in, maybe(term_path)::out,
	user_state::in, user_state::out, io::di, io::uo) is cc_multi.

browse_atom(InitAtom, FinalAtom, MaybeMark, !User, !IO) :-
	FinalAtom = atom(ProcLayout, Args),
	ProcLabel = get_proc_label_from_layout(ProcLayout),
	get_user_arg_values(Args, ArgValues),
	get_pred_attributes(ProcLabel, Module, Name, _, PredOrFunc),
	Function = pred_to_bool(unify(PredOrFunc,function)),
	sym_name_to_string(Module, ".", ModuleStr),
	BrowserTerm = synthetic_term_to_browser_term(ModuleStr ++ "." ++ Name, 
		ArgValues, Function),
	browse_browser_term(BrowserTerm, !.User ^ instr, !.User ^ outstr,
		yes(get_subterm_mode_from_atoms(InitAtom, FinalAtom)),
		MaybeDirs, !.User ^ browser, Browser, !IO),
	maybe_convert_dirs_to_path(MaybeDirs, MaybeMark),
	!:User = !.User ^ browser := Browser.

:- func get_subterm_mode_from_atoms(trace_atom, trace_atom, list(dir)) 
	= browser_term_mode.

get_subterm_mode_from_atoms(InitAtom, FinalAtom, Dirs) = Mode :-
	convert_dirs_to_term_path(Dirs, Path),
	(
		Path = [ArgNum | TermPath],
		ArgPos = arg_num_to_arg_pos(ArgNum),
		Mode = get_subterm_mode_from_atoms_and_term_path(InitAtom, 
			FinalAtom, ArgPos, TermPath)
	;
		Path = [],
		Mode = not_applicable
	).

:- func get_subterm_mode_from_atoms_and_term_path(trace_atom, trace_atom, 
	arg_pos, term_path) = browser_term_mode.

get_subterm_mode_from_atoms_and_term_path(InitAtom, FinalAtom, ArgPos, 
		TermPath) = Mode :-
	( trace_atom_subterm_is_ground(InitAtom, ArgPos, TermPath) ->
		Mode = input
	; trace_atom_subterm_is_ground(FinalAtom, ArgPos, TermPath) ->
		Mode = output
	;
		Mode = unbound
	).

:- func get_subterm_mode_from_atoms_for_arg(int, trace_atom, trace_atom, 
	list(dir)) = browser_term_mode.

get_subterm_mode_from_atoms_for_arg(ArgNum, InitAtom, FinalAtom, Dirs) 
		= Mode :-
	convert_dirs_to_term_path(Dirs, TermPath),
	ArgPos = arg_num_to_arg_pos(ArgNum),
	Mode = get_subterm_mode_from_atoms_and_term_path(InitAtom, FinalAtom,
		ArgPos, TermPath).

:- pred get_user_arg_values(list(trace_atom_arg)::in, list(univ)::out) is det.

get_user_arg_values([], []).
get_user_arg_values([arg_info(UserVisible, _, MaybeValue) | Args], Values) :-
	get_user_arg_values(Args, Values0),
	(
		UserVisible = yes
	->
		(
			MaybeValue = yes(ValueRep),
			term_rep.rep_to_univ(ValueRep, Value)
		;
			MaybeValue = no,
			Value = univ('_'`with_type`unbound)
		),
		Values = [Value | Values0]
	;
		Values = Values0
	).

:- pred print_atom_arguments(trace_atom::in, int::in, int::in, user_state::in,
	io::di, io::uo) is cc_multi.

print_atom_arguments(Atom, From, To, User, !IO) :-
	print_atom_argument(Atom, From, User, OK, !IO),
	(
		OK = yes,
		From + 1 =< To
	->
		print_atom_arguments(Atom, From + 1, To, User, !IO)
	;
		true
	).

:- pred print_atom_argument(trace_atom::in, int::in, user_state::in, bool::out,
	io::di, io::uo) is cc_multi.

print_atom_argument(Atom, ArgNum, User, OK, !IO) :-
	Atom = atom(_, Args0),
	maybe_filter_headvars(chosen_head_vars_presentation, Args0, Args),
	(
		list.index1(Args, ArgNum, ArgInfo),
		ArgInfo = arg_info(_, _, MaybeArg),
		MaybeArg = yes(ArgRep),
		term_rep.rep_to_univ(ArgRep, Arg)
	->
		print_browser_term(univ_to_browser_term(Arg), User ^ outstr,
			decl_caller_type, User ^ browser, !IO),
		OK = yes
	;
		io.write_string(User ^ outstr, "Invalid argument number\n",
			!IO),
		OK = no
	).

:- pred maybe_convert_dirs_to_path(maybe(list(dir))::in, 
	maybe(term_path)::out) is det.

maybe_convert_dirs_to_path(no, no).
maybe_convert_dirs_to_path(yes(Dirs), yes(TermPath)) :-
	convert_dirs_to_term_path(Dirs, TermPath).

	% Reverse the first argument and append the second to it.
	%
:- pred reverse_and_append(list(T)::in, list(T)::in, list(T)::out) is det.

reverse_and_append([], Bs, Bs).
reverse_and_append([A | As], Bs, Cs) :-
	reverse_and_append(As, [A | Bs], Cs).

%-----------------------------------------------------------------------------%

:- type user_command
	--->	yes			% The node is correct.
	;	no			% The node is incorrect.
	;	inadmissible		% The node is inadmissible.
	;	skip			% The user has no answer.
	;	browse_arg(maybe(int))	% Browse the nth argument before
					% answering.  Or browse the whole
					% predicate/function if the maybe is 
					% no.
	;	browse_io(int)		% Browse the nth IO action before
					% answering.
	;	print_arg(int, int)	% Print the nth to the mth arguments
					% before answering.
	;	print_io(int, int)	% Print the nth to the mth IO actions
					% before answering.
	;	pd			% Commence procedural debugging from
					% this point.
	;	set(maybe_option_table(setting_option), setting) 
					% Set a browser option.
	;	trust_predicate		% Trust the predicate being asked 
					% about.
	;	trust_module		% Trust the module being asked about.
	;	abort			% Abort this diagnosis session.
	;	help			% Request help before answering.
	;	empty_command		% User just pressed return.
	;	illegal_command.	% None of the above.

:- pred user_help_message(user_state::in, io::di, io::uo) is det.

user_help_message(User, !IO) :-
	io.write_strings(User ^ outstr, [
		"According to the intended interpretation of the program,",
		" answer one of:\n",
		"\ty\tyes\t\tthe node is correct\n",
		"\tn\tno\t\tthe node is incorrect\n",
		"\ti\tinadmissible\tthe input arguments are out of range\n",
		"\ts\tskip\t\tskip this question\n",
		"\tt [module]\ttrust [module]\tTrust the predicate/function ",
		"about\n",
		"\t\twhich the current question is being asked.  If the word\n",
		"\t\t`module' is given as an argument then trust all the\n",
		"\t\tpredicates/functions in the same module as the\n",
		"\t\tpredicate/function about which the current question is\n",
		"\t\tbeing asked.\n",
		"\tb [<n>]\tbrowse [<n>]\tbrowse the atom, or its nth argument\n",
		"\tb io <n>\tbrowse io <n>\tbrowse the atom's nth I/O action\n",
		"\tp <n>\tprint <n>\tprint the nth argument of the atom\n",
		"\tp <n-m>\tprint <n-m>\tprint the nth to the mth arguments of the atom\n",
		"\tp io <n>\tprint io <n>\tprint the atom's nth I/O action\n",
		"\tp io <n-m>\tprint io <n-m>\tprint the atom's nth to mth I/O actions\n",
		"\tset [-APBfpv] <param> <value>\t",
		"set a term browser parameter value\n",
		"\tpd\t\t\tcommence procedural debugging from this point\n",
		"\ta\tabort\t\t",
			"abort this diagnosis session and return to mdb\n",
		"\th, ?\thelp\t\tthis help message\n"
	], !IO).

:- pred user_confirm_bug_help(user_state::in, io::di, io::uo) is det.

user_confirm_bug_help(User, !IO) :-
	io.write_strings(User ^ outstr, [
		"Answer one of:\n",
		"\ty\tyes\t\tconfirm that the suspect is a bug\n",
		"\tn\tno\t\tdo not accept that the suspect is a bug\n",
%		"\tb\tbrowse\t\tbrowse the suspect\n",
		"\ta\tabort\t\t",
			"abort this diagnosis session and return to mdb\n",
		"\th, ?\thelp\t\tthis help message\n"
	], !IO).

:- pred get_command(string::in, user_command::out, 
	user_state::in, user_state::out, io::di, io::uo) is det.

get_command(Prompt, Command, User, User, !IO) :-
	util.trace_getline(Prompt, Result, User ^ instr, User ^ outstr, !IO),
	(
		Result = ok(String),
		Words = string.words(char.is_whitespace, String),
		(
			Words = [CmdWord | CmdArgs],
			(
				cmd_handler(CmdWord, CmdHandler),
				CommandPrime = CmdHandler(CmdArgs)
			->
				Command = CommandPrime
			;
				Command = illegal_command
			)
		;
			Words = [],
			Command = empty_command
		)
	;
		Result = error(Error),
		io.error_message(Error, Msg),
		io.write_string(User ^ outstr, Msg, !IO),
		io.nl(User ^ outstr, !IO),
		Command = abort
	;
		Result = eof,
		Command = abort
	).

:- pred cmd_handler(string::in, 
	(func(list(string)) = user_command)::out(func(in) = out is semidet))
	is semidet.

cmd_handler("y",	one_word_cmd(yes)).
cmd_handler("yes",	one_word_cmd(yes)).
cmd_handler("n",	one_word_cmd(no)).
cmd_handler("no",	one_word_cmd(no)).
cmd_handler("i",	one_word_cmd(inadmissible)).
cmd_handler("inadmissible", one_word_cmd(inadmissible)).
cmd_handler("s",	one_word_cmd(skip)).
cmd_handler("skip",	one_word_cmd(skip)).
cmd_handler("pd",	one_word_cmd(pd)).
cmd_handler("a",	one_word_cmd(abort)).
cmd_handler("abort",	one_word_cmd(abort)).
cmd_handler("?",	one_word_cmd(help)).
cmd_handler("h",	one_word_cmd(help)).
cmd_handler("help",	one_word_cmd(help)).
cmd_handler("b",	browse_arg_cmd).
cmd_handler("browse",	browse_arg_cmd).
cmd_handler("p",	print_arg_cmd).
cmd_handler("print",	print_arg_cmd).
cmd_handler("set",	set_arg_cmd).
cmd_handler("t",	trust_arg_cmd).
cmd_handler("trust",	trust_arg_cmd).

:- func one_word_cmd(user_command::in, list(string)::in) = (user_command::out)
	is semidet.

one_word_cmd(Cmd, []) = Cmd.

:- func browse_arg_cmd(list(string)::in) = (user_command::out) is semidet.

browse_arg_cmd([Arg]) = browse_arg(yes(ArgNum)) :-
	string.to_int(Arg, ArgNum).
browse_arg_cmd([]) = browse_arg(no).
browse_arg_cmd(["io", Arg]) = browse_io(ArgNum) :-
	string.to_int(Arg, ArgNum).

:- func print_arg_cmd(list(string)::in) = (user_command::out) is semidet.

print_arg_cmd([Arg]) = print_arg(From, To) :-
	string_to_range(Arg, From, To).
print_arg_cmd(["io", Arg]) = print_io(From, To) :-
	string_to_range(Arg, From, To).

:- pred string_to_range(string::in, int::out, int::out) is semidet.

:- func set_arg_cmd(list(string)::in) = (user_command::out) is semidet.

set_arg_cmd(ArgWords) = set(MaybeOptionTable, Setting) :-
	ArgWords \= [],
	parse.parse(["set" | ArgWords], set(MaybeOptionTable, Setting)).

:- func trust_arg_cmd(list(string)::in) = (user_command::out) is semidet.

trust_arg_cmd([]) = trust_predicate.
trust_arg_cmd(["module"]) = trust_module.

string_to_range(Arg, From, To) :-
	( string.to_int(Arg, Num) ->
		From = Num,
		To = Num
	;
		[FirstStr, SecondStr] = string.words(is_dash, Arg),
		string.to_int(FirstStr, First),
		string.to_int(SecondStr, Second),
		( First =< Second ->
			From = First,
			To = Second
		;
			From = Second,
			To = First
		)
	).

:- pred is_dash(char::in) is semidet.

is_dash('-').

%-----------------------------------------------------------------------------%

user_confirm_bug(Bug, Response, !User, !IO) :-
	write_decl_bug(Bug, !.User, !IO),
	get_command("Is this a bug? ", Command, !User, !IO),
	(
		Command = yes
	->
		Response = confirm_bug
	;
		Command = no
	->
		Response = overrule_bug
	;
		Command = abort
	->
		Response = abort_diagnosis
	;
		Command = browse_arg(MaybeArgNum)
	->
		browse_decl_bug(Bug, MaybeArgNum, !User, !IO),
		user_confirm_bug(Bug, Response, !User, !IO)
	;
		Command = browse_io(ActionNum)
	->
		decl_bug_io_actions(Bug, IoActions),
		browse_chosen_io_action(IoActions, ActionNum, _MaybeMark,
			!User, !IO),
		user_confirm_bug(Bug, Response, !User, !IO)
	;
		user_confirm_bug_help(!.User, !IO),
		user_confirm_bug(Bug, Response, !User, !IO)
	).

%-----------------------------------------------------------------------------%

	% Returns the caller type we want to use throughout the
	% declarative debugger.
:- func decl_caller_type = browse_caller_type.

decl_caller_type = print.

	% Display the node in user readable form on the current
	% output stream.
	%
:- pred write_decl_question(decl_question(T)::in, user_state::in,
	io::di, io::uo) is cc_multi.

write_decl_question(wrong_answer(_, _, Atom), User, !IO) :-
	write_decl_final_atom(User, "", decl_caller_type, Atom, !IO).
	
write_decl_question(missing_answer(_, Call, Solns), User, !IO) :-
	write_decl_init_atom(User, "Call ", decl_caller_type, Call, !IO),
	(
		Solns = []
	;
		Solns = [_ | _],
		io.write_string(User ^ outstr, "Solutions:\n", !IO),
		list.foldl(write_decl_final_atom(User, "\t", print_all), Solns,
			!IO)
	).

write_decl_question(unexpected_exception(_, Call, ExceptionRep), User, !IO) :-
	write_decl_init_atom(User, "Call ", decl_caller_type, Call, !IO),
	io.write_string(User ^ outstr, "Throws ", !IO),
	term_rep.rep_to_univ(ExceptionRep, Exception),
	io.write(User ^ outstr, include_details_cc, univ_value(Exception), 
		!IO),
	io.nl(User ^ outstr, !IO).

:- pred write_decl_bug(decl_bug::in, user_state::in, io::di, io::uo) 
	is cc_multi.

write_decl_bug(e_bug(EBug), User, !IO) :-
	(
		EBug = incorrect_contour(_, Atom, Contour, _),
		io.write_string(User ^ outstr, "Found incorrect contour:\n",
			!IO),
		io.write_list(Contour, "", write_decl_final_atom(User, "", 
			decl_caller_type), !IO),
		write_decl_final_atom(User, "", decl_caller_type, Atom, !IO)
	;
		EBug = partially_uncovered_atom(Atom, _),
		io.write_string(User ^ outstr,
			"Found partially uncovered atom:\n", !IO),
		write_decl_init_atom(User, "", decl_caller_type, Atom, !IO)
	;
		EBug = unhandled_exception(Atom, ExceptionRep, _),
		io.write_string(User ^ outstr, "Found unhandled exception:\n",
			!IO),
		write_decl_init_atom(User, "", decl_caller_type, Atom, !IO),
		term_rep.rep_to_univ(ExceptionRep, Exception),
		io.write(User ^ outstr, include_details_cc,
			univ_value(Exception), !IO),
		io.nl(User ^ outstr, !IO)
	).

write_decl_bug(i_bug(IBug), User, !IO) :-
	IBug = inadmissible_call(Parent, _, Call, _),
	io.write_string(User ^ outstr, "Found inadmissible call:\n", !IO),
	write_decl_atom(User, "Parent ", decl_caller_type, init(Parent), !IO),
	write_decl_atom(User, "Call ", decl_caller_type, init(Call), !IO).

:- pred write_decl_init_atom(user_state::in, string::in, 
	browse_caller_type::in, init_decl_atom::in, 
	io::di, io::uo) is cc_multi.

write_decl_init_atom(User, Indent, CallerType, InitAtom, !IO) :-
	write_decl_atom(User, Indent, CallerType, init(InitAtom), !IO).

:- pred write_decl_final_atom(user_state::in, string::in,
	browse_caller_type::in, final_decl_atom::in, 
	io::di, io::uo) is cc_multi.

write_decl_final_atom(User, Indent, CallerType, FinalAtom, !IO) :-
	write_decl_atom(User, Indent, CallerType, final(FinalAtom), !IO).

:- pred write_decl_atom(user_state::in, string::in, browse_caller_type::in,
	some_decl_atom::in, io::di, io::uo) is cc_multi.

write_decl_atom(User, Indent, CallerType, DeclAtom, !IO) :-
	io.write_string(User ^ outstr, Indent, !IO),
	unravel_decl_atom(DeclAtom, TraceAtom, IoActions),
	TraceAtom = atom(ProcLayout, Args0),
	ProcLabel = get_proc_label_from_layout(ProcLayout),
	get_pred_attributes(ProcLabel, _, Functor, _, PredOrFunc),
	Which = chosen_head_vars_presentation,
	maybe_filter_headvars(Which, Args0, Args1),
	list.map(trace_atom_arg_to_univ, Args1, Args),
		%
		% Call the term browser to print the atom (or part of it
		% up to a size limit) as a goal.
		%
	BrowserTerm = synthetic_term_to_browser_term(Functor, Args,
		is_function(PredOrFunc)),
	browse.print_browser_term(BrowserTerm, User ^ outstr, CallerType,
		User ^ browser, !IO),
	write_io_actions(User, IoActions, !IO).

:- pred trace_atom_arg_to_univ(trace_atom_arg::in, univ::out) is det.

trace_atom_arg_to_univ(TraceAtomArg, Univ) :-
	MaybeUniv = TraceAtomArg ^ arg_value,
	(
		MaybeUniv = yes(Rep),
		term_rep.rep_to_univ(Rep, Univ)
	;
		MaybeUniv = no,
		Univ = univ('_' `with_type` unbound)
	).

:- pred write_io_actions(user_state::in, list(io_action)::in, 
	io::di, io::uo) is cc_multi.

write_io_actions(User, IoActions, !IO) :-
	list.length(IoActions, NumIoActions),
	( NumIoActions = 0 ->
		true
	;
		( NumIoActions = 1 ->
			io.write_string(User ^ outstr, "1 io action:", !IO)
		;
			io.write_int(User ^ outstr, NumIoActions, !IO),
			io.write_string(User ^ outstr, " io actions:", !IO)
		),
 		NumPrinted = get_num_printed_io_actions(User ^ browser),
 		( NumIoActions =< NumPrinted ->
			io.nl(User ^ outstr, !IO),
			list.foldl(print_io_action(User), IoActions, !IO)
		;
			io.write_string(User ^ outstr, " too many to show",
				!IO),
			io.nl(User ^ outstr, !IO)
		)
	).

:- pred print_io_action(user_state::in, io_action::in, io::di, io::uo) 
	is cc_multi.

print_io_action(User, IoAction, !IO) :-
	Term = io_action_to_browser_term(IoAction),
	browse.print_browser_term(Term, User ^ outstr, print_all,
		User ^ browser, !IO).

%-----------------------------------------------------------------------------%

get_browser_state(User) = User ^ browser.

set_browser_state(Browser, !User) :-
	!:User = !.User ^ browser := Browser.

%-----------------------------------------------------------------------------%
