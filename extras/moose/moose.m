%----------------------------------------------------------------------------%
% Copyright (C) 1998-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%
% Original author: Tom Conway <conway@cs.mu.oz.au>
% Extensions: Ralph Becket <rafe@cs.mu.oz.au>
%
% There's scope for recoding much of this to use the more recent
% additions to the language, if anyone feels like something to do.
%
%----------------------------------------------------------------------------%

:- module moose.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module grammar, lalr, tables, check, mercury_syntax, misc, options.
:- import_module array, bool, getopt, int, list, map, require.
:- import_module set, std_util, string, term, term_io, varset.

main -->
	parse_options(MOptions, Args),
	(
		{ MOptions = ok(Options) },
		{ lookup_bool_option(Options, help, Help) },
		( { Help = yes } ->
			help
		;
			main2(Options, Args)
		)
	;
		{ MOptions = error(String) },
		stderr_stream(StdErr),
		write_string(StdErr, String),
		nl(StdErr)
	).

:- pred main2(options::in, list(string)::in, io__state::di, io__state::uo).
main2(_Options, []) -->
	stderr_stream(StdErr),
	write_string(StdErr, "no input files.\n"),
	help.
main2(Options, [Name0|Names]) -->
	{ figure_out_names(Name0, InName, OutName) },
	see(InName, Res0),
	(
		{ Res0 = ok },
		tell(OutName, Res1),
		(
			{ Res1 = ok },
			process(Options),
			told
		;
			{ Res1 = error(Err) },
			{ error_message(Err, Msg) },
			stderr_stream(StdErr),
			write_string(StdErr, Msg),
			nl(StdErr)
		)
	;
		{ Res0 = error(Err) },
		{ error_message(Err, Msg) },
		stderr_stream(StdErr),
		write_string(StdErr, Msg),
		nl(StdErr)
	),
	( { Names = [_|_] } ->
		main2(Options, Names)
	;
		[]
	).

:- pred figure_out_names(string::in, string::out, string::out) is det.
figure_out_names(Name0, InName, OutName) :-
	( string__remove_suffix(Name0, ".moo", Name1) ->
		Name = Name1
	;
		Name = Name0
	),
	string__append(Name, ".moo", InName),
	string__append(Name, ".m", OutName).

:- type whereami
	--->	(interface) ; (implementation) .

:- type parser
	--->	parser(
			whereami,
			nonterminal,	% Starting nonterminal.
			term,		% EOF token.
			string,		% Token type name.
			string,		% Naming prefix (unused).
			string,		% Parser state input mode.
			string		% Parser state output mode.
		).

:- pred process(options::in, io__state::di, io__state::uo) is det.

process(Options) -->
	{ lookup_bool_option(Options, verbose, Verbse) },
	( { Verbse = yes } -> report_stats ; [] ),
	read_module(Result),
	{ Result = module(Module, Errors) },
	(
		{ Errors = [_|_] },
		stderr_stream(StdErr),
		foldl((pred(Err::in, di, uo) is det -->
			{ Err = error(Msg, Line) },
			format(StdErr, "%d: %s\n", [i(Line), s(Msg)])
		), Errors)
	;
		{ Errors = [] },
		{ get_moose_elements(Module, [], Remainder0, (implementation),
			[], MParser, [], RuleDecls, [], ClauseList,
			[], XFormList) },
		(
			{ MParser = [] },
			stderr_stream(StdErr),
			write_string(StdErr, "error: no parse/6 declaration.\n")
		;
			{ MParser = [Parser] },
			{ reverse(Remainder0, Remainder) },
			process_2(Options, Remainder, Parser,
				RuleDecls, ClauseList, XFormList)
		;
			{ MParser = [_,_|_] },
			stderr_stream(StdErr),
			write_string(StdErr, "error: more than one parse/4 declaration.\n")
		)
	).

:- pred process_2(options, (module), parser, list(rule_decl), list(clause),
		list(xform), io__state, io__state).
:- mode process_2(in, in, in, in, in, in, di, uo) is det.

process_2(Options, Module, Parser, Decls0, Clauses0, XFormList) -->
	{ lookup_bool_option(Options, verbose, Verbse) },
	( { Verbse = yes } -> report_stats ; [] ),

	{ check_rule_decls(Decls0, Decls, DeclErrors) },
	foldl(write_error, DeclErrors),

	{ check_clauses(Clauses0, Decls, Clauses, ClauseErrors) },
	foldl(write_error, ClauseErrors),

	{ Parser = parser(WhereAmI, StartId, EndTerm, TokenType, _Prefix,
				InAtom, OutAtom) },

	{ check_useless(StartId, Clauses, Decls, UselessErrors) },
	foldl(write_error, UselessErrors),

	{ check_inf_derivations(Clauses, Decls, InfErrors) },
	foldl(write_error, InfErrors),

	(
		{ DeclErrors = [] },
		{ ClauseErrors = [] },
		{ UselessErrors = [] },
		{ InfErrors = [] }
	->
		write_module(nolines, Module), nl,
		{ lookup(Decls, StartId, StartDecl) },
		write_parser(WhereAmI, StartId, StartDecl, TokenType,
				InAtom, OutAtom),
		write_action_type_class(WhereAmI, XFormList, Decls,
					TokenType, InAtom, OutAtom),

		stderr_stream(StdErr),
		write_string(StdErr, "constructing grammar...\n"),

		{ map__init(Xfns0) },
		{ foldl((pred(XForm::in, Xf0::in, Xf::out) is det :-
			XForm = xform(XfNt, _),
			map__det_insert(Xf0, XfNt, XForm, Xf)
		), XFormList, Xfns0, XForms) },

		{ construct_grammar(StartId, Clauses, XForms, Grammar) },
		{ Grammar = grammar(Rules, _, Xfns, _, Index, First, _Follow) },
		{ reaching(Rules, First, Reaching) },

		write_string(StdErr, "constructing lr(0) items...\n"),
		{ lr0items(Rules, Reaching, C, Gotos) },
		write_string(StdErr, "determining lookaheads...\n"),
		lookaheads(C, Gotos, Rules, First, Index, Lookaheads),
		write_string(StdErr, "computing the action table...\n"),
		{ shifts(C, Rules, First, Reaching, Shifts) },
		{ actions(C, Rules, Lookaheads, Gotos, Shifts,
			States, ActionTable, ActionErrs) },
		foldl2((pred(Err::in, HasEs0::in, HasEs::out, di, uo) is det -->
			(
				{ Err = warning(Warning) },
				{ HasEs = HasEs0 },
				(
					{ Warning = shiftreduce(_S, Rp) },
					write_string(StdErr,
				"shift reduce conflict involving:\n\t"),
					write_rule(StdErr, Rp, Rules)
				)
			;
				{ Err = error(Error) },
				{ HasEs = yes },
				(
					{ Error = shiftshift(_, _) },
					write_string(StdErr,
						"shift shift error.\n")
				;
					{ Error = reducereduce(R0, R1) },
					write_string(StdErr,
				"reduce reduce conflict involving:\n\t"),
					write_rule(StdErr, R0, Rules),
					write_string(StdErr, "\t"),
					write_rule(StdErr, R1, Rules)
				;
					{ Error = misc(Ac1, Ac2) },
					write_string(StdErr,
				"misc conflict involving:\n\t"),
					write(StdErr, Ac1), 
					write_string(StdErr, "\n\t"),
					write(StdErr, Ac2), 
					write_string(StdErr, "\n")
				)
			)
		), ActionErrs, no, _HasErrors),
		write_action_table(ActionTable, TokenType, EndTerm),
		write_string(StdErr, "computing the goto table...\n"),
		{ gotos(C, States, Gotos, GotoTable) },
		write_goto_table(GotoTable, Decls),
		write_reductions(Rules, ActionTable, TokenType,
				InAtom, OutAtom, Xfns),
		[]
	;
		[]
	).

%------------------------------------------------------------------------------%

:- pred write_action_type_class(whereami, list(xform), rule_decls, 
	string, string, string, io__state, io__state).
:- mode write_action_type_class(in, in, in, in, in, in, di, uo) is det.

write_action_type_class(Where, XForms, Decls, TokenType, InAtom, OutAtom) -->
	( { Where = (interface) } ->
		write_string(":- interface.\n\n")
	;
		[]
	),
	io__format("\
:- typeclass parser_state(T) where [
	pred get_token(%s, T, T),
	mode get_token(out, %s, %s) is det,
	func unget_token(%s, T) = T,
	mode unget_token(in, %s) = %s is det\
",
		[s(TokenType), s(InAtom), s(OutAtom),
		 s(TokenType), s(InAtom), s(OutAtom)]
	),
	( { not XForms = [] } ->
		io__write_string(",\n")
	;
		[]
	),
	{ WriteIn = (pred(_Anything::in, di, uo) is det -->
		io__write_string("in"))
	},
	{ WriteXForm = (pred(XForm::in, di, uo) is det -->
		{ XForm = xform(NT, MethodName) },
		{ lookup(Decls, NT, RuleDecl) },
		{ RuleDecl = rule(_NT, Types, VarSet, _Context) },
		io__format("\tfunc %s(", [s(MethodName)]),
		io__write_list(Types, ", ", term_io__write_term(VarSet)),
		( { Types \= [] } -> io__write_string(", ") ; [] ),
		io__write_string("T) = T,\n"),

		io__format("\tmode %s(", [s(MethodName)]),
		io__write_list(Types, ", ", WriteIn),
		( { Types \= [] } -> io__write_string(", ") ; [] ),
		io__format("%s) = %s is det", [s(InAtom), s(OutAtom)])
		)
	},
	io__write_list(XForms, ",\n", WriteXForm),
	io__write_string("\n].\n"),
	( { Where = (interface) } ->
		write_string(":- implementation.\n\n")
	;
		[]
	).

%------------------------------------------------------------------------------%

:- pred write_rule(output_stream, int, rules, io__state, io__state).
:- mode write_rule(in, in, in, di, uo) is det.

write_rule(Stream, RN, Rules) -->
	{ lookup(Rules, RN, Rule) },
	write_int(Stream, RN), write_string(Stream, ": "),
	{ Rule = rule(NT, _, Syms, _, _, _, _) },
	write(Stream, NT),
	write_string(Stream, " ->\t"),
	write_syms(Stream, 0, 999, Syms),
	write_string(Stream, "\n").

:- pred write_syms(output_stream, int, int, symbols, io__state, io__state).
:- mode write_syms(in, in, in, in, di, uo) is det.

write_syms(Stream, N, Dot, Syms) -->
	( { N = Dot } ->
		write_string(Stream, ". ")
	;
		[]
	),
	{ array__max(Syms, Max) },
	( { N =< Max } ->
		{ lookup(Syms, N, Sym) },
		write(Stream, Sym),
		write_string(Stream, " "),
		write_syms(Stream, N + 1, Dot, Syms)
	;
		[]
	).

%------------------------------------------------------------------------------%

:- pred get_moose_elements((module), (module), (module), whereami,
		list(parser), list(parser), list(rule_decl), list(rule_decl),
		list(clause), list(clause), list(xform), list(xform)).
:- mode get_moose_elements(in, in, out, in, in, out, in, out, in, out,
		in, out) is det.

get_moose_elements([], Remainder, Remainder, _WhereAmI, MParser, MParser,
		RuleDecls, RuleDecls, Clauses, Clauses, Actions, Actions).
get_moose_elements([Element|Elements], Remainder0, Remainder, WhereAmI0,
		MParser0, MParser, RuleDecls0, RuleDecls, Clauses0, Clauses,
		Actions0, Actions) :-
	(
		Element = misc(ClauseTerm, ClauseVarSet),
		term_to_clause(ClauseTerm, ClauseVarSet, _, Clause)
	->
		WhereAmI = WhereAmI0,
		Remainder1 = Remainder0,
		MParser1 = MParser0,
		RuleDecls1 = RuleDecls0,
		Clauses1 = [Clause|Clauses0],
		Actions1 = Actions0
	;
		Element = misc(MiscTerm0, _),
		interface_term(MiscTerm0)
	->
		WhereAmI = (interface),
		Remainder1 = [Element|Remainder0],
		MParser1 = MParser0,
		RuleDecls1 = RuleDecls0,
		Clauses1 = Clauses0,
		Actions1 = Actions0
	;
		Element = misc(MiscTerm1, _),
		implementation_term(MiscTerm1)
	->
		WhereAmI = (implementation),
		Remainder1 = [Element|Remainder0],
		MParser1 = MParser0,
		RuleDecls1 = RuleDecls0,
		Clauses1 = Clauses0,
		Actions1 = Actions0
	;
		Element = misc(MiscTerm2, MiscVarSet2),
		rule_term(MiscTerm2, MiscVarSet2, RuleDecl)
	->
		WhereAmI = WhereAmI0,
		Remainder1 = Remainder0,
		MParser1 = MParser0,
		RuleDecls1 = [RuleDecl|RuleDecls0],
		Clauses1 = Clauses0,
		Actions1 = Actions0
	;
		Element = misc(MiscTerm3, MiscVarSet3),
		parser_term(MiscTerm3, MiscVarSet3, WhereAmI0, Parser)
	->
		WhereAmI = WhereAmI0,
		Remainder1 = Remainder0,
		MParser1 = [Parser|MParser0],
		RuleDecls1 = RuleDecls0,
		Clauses1 = Clauses0,
		Actions1 = Actions0
	;
		Element = misc(MiscTerm4, _),
		xform_term(MiscTerm4, XForm)
	->
		WhereAmI = WhereAmI0,
		Remainder1 = Remainder0,
		MParser1 = MParser0,
		RuleDecls1 = RuleDecls0,
		Clauses1 = Clauses0,
		Actions1 = [XForm|Actions0]
	;
		WhereAmI = WhereAmI0,
		Remainder1 = [Element|Remainder0],
		MParser1 = MParser0,
		RuleDecls1 = RuleDecls0,
		Clauses1 = Clauses0,
		Actions1 = Actions0
	),
	get_moose_elements(Elements, Remainder1, Remainder, WhereAmI,
		MParser1, MParser, RuleDecls1, RuleDecls, Clauses1, Clauses,
		Actions1, Actions).

:- pred interface_term(term::in) is semidet.
interface_term(functor(atom(":-"), [functor(atom("interface"), [], _)], _)).

:- pred implementation_term(term::in) is semidet.
implementation_term(functor(atom(":-"),
	[functor(atom("implementation"), [], _)], _)).

:- pred rule_term(term, varset, rule_decl).
:- mode rule_term(in, in, out) is semidet.

rule_term(functor(atom(":-"), [functor(atom("rule"), [RuleTerm], _)], _),
		VarSet, Decl) :-
	RuleTerm = functor(atom(Name), Args, Context),
	list__length(Args, Arity),
	Decl = rule(Name/Arity, Args, VarSet, Context).

:- pred parser_term(term, varset, whereami, parser).
:- mode parser_term(in, in, in, out) is semidet.

parser_term(functor(atom(":-"), [functor(atom("parse"), Args, _)], _),
		_VarSet, WhereAmI, Decl) :-
	Args = [StartIdTerm, TokTerm, EndTerm,
			PrefixTerm, InAtomTerm, OutAtomTerm],
	StartIdTerm = functor(atom("/"), [functor(atom(Name), [], _),
		functor(integer(Arity), _, _)], _),
	StartId = Name / Arity,
	TokTerm = functor(atom(TokAtom), [], _),
	PrefixTerm = functor(atom(PrefixAtom), [], _),
	InAtomTerm = functor(atom(InAtom), [], _),
	OutAtomTerm = functor(atom(OutAtom), [], _),
	Decl = parser(WhereAmI, StartId, EndTerm, TokAtom,
			PrefixAtom, InAtom, OutAtom).

:- pred xform_term(term, xform).
:- mode xform_term(in, out) is semidet.

xform_term(Term, XForm) :-
	Term = functor(atom(":-"), [
		functor(atom("action"), [
			functor(atom("/"), [
				functor(atom(Name), [], _),
				functor(integer(Arity), _, _)
			], _),
			functor(atom(Pred), [], _)
		], _)
	], _),
	XForm = xform(Name/Arity, Pred).

%------------------------------------------------------------------------------%

:- pred help(io__state, io__state).
:- mode help(di, uo) is det.

help -->
	stderr_stream(StdErr),
	write_string(StdErr, "\
usage: moose <options> file ...
	-h|--help		help
	-a|--dump-action	dump the action table
	-f|--dump-first		dump the FIRST sets
	-a|--dump-follow	dump the FOLLOW sets
	-a|--dump-goto		dump the goto table
	-a|--dump-items		dump the item sets
	-a|--dump-rules		dump the flattened rules
"	).

%------------------------------------------------------------------------------%

:- pred write_action_table(actiontable, string, term, io__state, io__state).
:- mode write_action_table(in, in, in, di, uo) is det.

write_action_table(Table, TT, End) -->
	io__format(":- inst state_no --->\n\t\t", []),
	io__write_list(map__keys(Table), "\n\t;\t", io__write_int),
	io__format(".\n:- inst state_nos == list_skel(state_no).\n\n", []),
	io__format("\
:- type parsing_action
	--->	shift
	;	reduce
	;	accept.

:- pred actions(int, %s, parsing_action, int).
:- mode actions(in(state_no), in, out, out(state_no)) is semidet.

",
		[s(TT)]
	),
	foldl((pred(State::in, StateActions::in, di,uo) is det -->
		{ format("0x%x", [i(State)], SS) },
		io__format("\
actions(%s, Tok, Action, Value) :-
	actions%s(Tok, Action, Value).

:- pred actions%s(%s, parsing_action, int).
:- mode actions%s(in, out, out(state_no)) is semidet.

",
			[s(SS), s(SS), s(SS), s(TT), s(SS)]
		),
		write_state_actions(SS, End, StateActions)
	), Table).

:- pred write_state_actions(string, term, (terminal -> action),
		io__state, io__state).
:- mode write_state_actions(in, in, in, di, uo) is det.

write_state_actions(SS, End, StateActions) -->
	{ format("actions%s", [s(SS)], Name) },
	foldl((pred(Terminal::in, Action::in, di, uo) is det -->
		{ terminal_to_term(Terminal, End, Token) },
		{ context_init(Ctxt) },
		{ Term = functor(atom(Name),
			[Token,
			functor(atom(Kind), [], Ctxt),
			functor(integer(Val), [], Ctxt)], Ctxt) },
		(
			{ Action = shift(Val) },
			{ Kind = "shift" }
		;
			{ Action = reduce(Val) },
			{ Kind = "reduce" }
		;
			{ Action = accept },
			{ Kind = "accept" },
			{ Val = 0 }
		),
		{ init(Varset) },
		term_io__write_term_nl(Varset, Term)
	), StateActions),
	nl.

:- pred terminal_to_term(terminal, term, term).
:- mode terminal_to_term(in, in, out) is det.

terminal_to_term(epsilon, _, _) :-
	error("terminal_to_term: unexpected epsilon").
terminal_to_term(Name/Arity, _, Term) :-
	init(V0),
	new_vars(V0, Arity, Vars, _),
	context_init(Ctxt),
	map((pred(Var::in, T::out) is det :-
		T = variable(Var)
	), Vars, Args),
	Term = functor(atom(Name), Args, Ctxt).
terminal_to_term(($), End, End).
terminal_to_term((*), _, _) :-
	error("terminal_to_term: unexpected hash").

%------------------------------------------------------------------------------%

:- pred write_goto_table(gototable, rule_decls, io__state, io__state).
:- mode write_goto_table(in, in, di, uo) is det.

write_goto_table(Table, DeclTable) -->
	{ values(DeclTable, Decls) },
	write_nonterminal_type(Decls),
	write_string("\
:- pred gotos(int, nonterminal, int).
:- mode gotos(in(state_no), in, out(state_no)) is semidet.

"	),
	foldl((pred(State::in, StateActions::in, di,uo) is det -->
		{ format("0x%x", [i(State)], SS) },
		io__format("\
gotos(%s, NT, NS) :-
	gotos%s(NT, NS).

:- pred gotos%s(nonterminal, int).
:- mode gotos%s(in, out) is semidet.

",
			[s(SS), s(SS), s(SS), s(SS)]
		),
		write_state_gotos(SS, StateActions)
	), Table).

:- pred write_nonterminal_type(list(rule_decl), io__state, io__state).
:- mode write_nonterminal_type(in, di, uo) is det.

write_nonterminal_type(Ds) -->
	{ map((pred(Decl::in, NTType::out) is det :-
		Decl = rule(NT, Args, _VS, TC),
		(
			NT = start,
			error("write_nonterminal_type: start!")
		;
			NT = Name/_Arity
		),
		NTType = functor(atom(Name), Args, TC)
	), Ds, NTTypes) },
	{ context_init(Ctxt) },
	{ init(Varset) },
	{ Type = disj(functor(atom("nonterminal"), [], Ctxt), NTTypes) },
	{ Element = type(Type, Varset) },
	write_element(nolines, Element),
	nl.

:- pred write_state_gotos(string, (nonterminal -> grammar__state),
		io__state, io__state).
:- mode write_state_gotos(in, in, di, uo) is det.

write_state_gotos(SS, StateActions) -->
	{ format("gotos%s", [s(SS)], Name) },
	foldl((pred(NT::in, NS::in, di, uo) is det -->
		{ nonterminal_to_term(NT, Token) },
		{ context_init(Ctxt) },
		{ Term = functor(atom(Name),
			[Token, functor(integer(NS), [], Ctxt)], Ctxt) },
		{ init(Varset) },
		term_io__write_term_nl(Varset, Term)
	), StateActions),
	nl.

:- pred nonterminal_to_term(nonterminal, term).
:- mode nonterminal_to_term(in, out) is det.

nonterminal_to_term(start, _) :-
	error("nonterminal_to_term: unexpected start").
nonterminal_to_term(Name/Arity, Term) :-
	init(V0),
	new_vars(V0, Arity, Vars, _),
	context_init(Ctxt),
	map((pred(Var::in, T::out) is det :-
		T = variable(Var)
	), Vars, Args),
	Term = functor(atom(Name), Args, Ctxt).

%------------------------------------------------------------------------------%

:- pred write_parser(whereami, nonterminal, rule_decl, string, string, string,
		io__state, io__state).
:- mode write_parser(in, in, in, in, in, in, di, uo) is det.

write_parser(Where, NT, Decl, TT, InAtom, OutAtom) -->
	(
		{ NT = StartName/StartArity }
	;
		{ NT = start },
		{ error("write_parser: start!") }
	),
	{ Decl = rule(_, DeclArgs, DeclVarset, DeclCtxt) },
	{ init(Varset0) },
	{ mkstartargs(StartArity, [], StartArgs, Varset0, Varset) },
	{ StartTerm = functor(atom(StartName), StartArgs, Ctxt) },
	{ context_init(Ctxt) },
	{ ParseResultType = type(disj(functor(atom("parse_result"), [], Ctxt),
		[OkayType, ErrorType]), DeclVarset) },
	{ OkayType = functor(atom(StartName), DeclArgs, DeclCtxt) },
	{ ErrorType = functor(atom("error"), [
		functor(atom("string"), [], Ctxt)], Ctxt) },
	( { Where = (interface) } ->
		write_string(":- interface.\n\n")
	;
		[]
	),
	write_element(nolines, ParseResultType),
	nl,
	io__format("\
:- import_module list.

:- pred parse(parse_result, P, P) <= parser_state(P).
:- mode parse(out, %s, %s) is det.

",
		[s(InAtom), s(OutAtom)]
	),
	( { Where = (interface) } ->
		write_string(":- implementation.\n\n")
	;
		[]
	),
	io__format("\
parse(Result, Toks0, Toks) :-
	parse(Toks0, Toks, [0], [], Result).

:- pred parse(P, P, statestack, symbolstack, parse_result) <= parser_state(P).
:- mode parse(%s, %s, in(state_nos), in, out) is det.

parse(Toks0, Toks, St0, Sy0, Res) :-
    (
        St0 = [S0|_],
        get_token(Tok, Toks0, Toks1),
        ( 
            actions(S0, Tok, What, Val)
        ->
            (
                What = shift,
                Sy1 = [t(Tok)|Sy0],
                St1 = [Val|St0],
                parse(Toks1, Toks, St1, Sy1, Res)
            ;
                What = reduce,
		Toks2 = unget_token(Tok, Toks1),
                reduce(Val, St0, St1, Sy0, Sy1, Toks2, Toks3),
                parse(Toks3, Toks, St1, Sy1, Res)
            ;
                What = accept,
                    ( Sy0 = [n(",
		[s(InAtom), s(OutAtom)]
        ),
        write_term(Varset, StartTerm),
        write_string(")] ->
                            Res = ("
	),
	write_term(Varset, StartTerm),
	write_string("),
                            Toks = Toks1
                    ;
                            error(""parse: internal accept error"")
                    )
                )
        ;
            Res = error(""parse error""),
	    Toks = unget_token(Tok, Toks1)
        )
    ;
        St0 = [],
        error(""parse: state stack underflow"")
    ).
"
	).

:- pred mkstartargs(int, list(term), list(term), varset, varset).
:- mode mkstartargs(in, in, out, in, out) is det.

mkstartargs(N, Ts0, Ts, VS0, VS) :-
	( N =< 0 ->
		Ts = Ts0,
		VS = VS0
	;
		format("V%d", [i(N)], VarName),
		new_named_var(VS0, VarName, Var, VS1),
		T = variable(Var),
		Ts1 = [T|Ts0],
		mkstartargs(N - 1, Ts1, Ts, VS1, VS)
	).

%------------------------------------------------------------------------------%

:- pred write_reductions(rules, actiontable, string, string, string, xforms,
		io__state, io__state).
:- mode write_reductions(in, in, in, in, in, in, di, uo) is det.

write_reductions(Rules, Table, TT, InAtom, OutAtom, Xfns) -->
	io__format("\
:- import_module require, std_util.

:- type statestack == list(int).
:- type symbolstack == list(stacksymbol).
:- type stacksymbol
	--->	n(nonterminal)
	;	t(%s).

",
		[s(TT)]
	),
	io__format("
:- pred reduce(int, statestack, statestack,
		symbolstack, symbolstack, P, P) <= parser_state(P).
:- mode reduce(in(state_no), in(state_nos), out(state_nos),
		in, out, %s, %s) is det.

reduce(RuleNum, States0, States, Symbols0, Symbols, Tokens0, Tokens) :-
	reduce0(RuleNum, States0, States1, Symbols0, Symbols1,
		Tokens0, Tokens1),
	(
		States1 = [State0|_States2],
		Symbols1 = [n(Non)|_],
		gotos(State0, Non, State1),
		States3 = [State1|States1]
	->
		States = States3,
		Symbols = Symbols1,
		Tokens = Tokens1
	;
		error(""reduce: reduction failed"")
	).

",
		[s(InAtom), s(OutAtom)]
	),
	io__format("\
:- pred reduce0(int, statestack, statestack,
		symbolstack, symbolstack, P, P) <= parser_state(P).
:- mode reduce0(in(state_no), in(state_nos), out(state_nos),
		in, out, %s, %s) is det.

",
		[s(InAtom), s(OutAtom)]
	),
	foldl((pred(Rn::in, Rule::in, di, uo) is det -->
		( { Rn = 0 } ->

		io__write_string("\
reduce0(0x0, _, _, _, _, _, _) :-
	reduce0_error(0x0).

"		)

		;

		{ RedName = format("reduce0x%x", [i(Rn)]) },
		{ RnS     = format("0x%x", [i(Rn)]) },
		io__format("\
reduce0(%s, S0, S, T0, T, U0, U) :-
	%s(S0, S, T0, T, U0, U).

:- pred %s(statestack, statestack, symbolstack, symbolstack,
		P, P) <= parser_state(P).
:- mode %s(in(state_nos), out(state_nos), in, out, %s, %s) is det.
",
			[s(RnS), s(RedName), s(RedName), s(RedName),
			 s(InAtom), s(OutAtom)]
		),
		{ Rule = rule(RNt, Head, _, Body, Actions, Varset0, _C) },
		{ new_named_var(Varset0, "M_St0", St0v, Varset1) },
		{ St0 = variable(St0v) },
		{ new_named_var(Varset1, "M_St1", St1v, Varset2) },
		{ St1 = variable(St1v) },
		{ new_named_var(Varset2, "M_Sy0", Sy0v, Varset3) },
		{ Sy0 = variable(Sy0v) },
		{ new_named_var(Varset3, "M_Sy1", Sy1v, Varset4) },
		{ Sy1 = variable(Sy1v) },
		{ new_named_var(Varset4, "M_RedRes", Resv, Varset5) },
		{ Res = variable(Resv) },
		{ ResS = functor(atom("n"), [variable(Resv)], Ctxt) },
		{ new_named_var(Varset5, "M_D", Dv, Varset6) },
		{ _D = variable(Dv) },
		{ new_named_var(Varset6, "M_S", Sv, Varset7) },
		{ _S = variable(Sv) },
		{ new_named_var(Varset7, "M_St", Stv, Varset8) },
		{ St = variable(Stv) },
		{ new_named_var(Varset8, "M_Sy", Syv, Varset9) },
		{ Sy = variable(Syv) },
		{ new_named_var(Varset9, "M_Ts0", Ts0v, Varset10) },
		{ Ts0 = variable(Ts0v) },
		{ new_named_var(Varset10, "M_Ts", Tsv, Varset11) },
		{ Ts = variable(Tsv) },
		{ context_init(Ctxt) },
		{ format("reduction 0x%x failed!", [i(Rn)], Err) },
		{ mkstacks(Body, St1, Sts, Sy1, Sys, Varset11, Varset12) },
		{ Cond = functor(atom(","), [
			functor(atom("="), [St0, Sts], Ctxt),
			functor(atom("="), [Sy0, Sys], Ctxt)
		], Ctxt) },
		{ Red = functor(atom("="), [Res, Head], Ctxt) },
		{ append(Actions, [Red], AllActions0) },
		{ reverse(AllActions0, AllActions) },
		{ ConsStack = functor(atom(","), [
			functor(atom("="), [Sy, functor(atom("[|]"),
				[ResS, Sy1], Ctxt)], Ctxt),
			functor(atom("="), [St, St1], Ctxt)], Ctxt) },
		{ mkactions(AllActions, ConsStack, Then0) },
		(
			{ search(Xfns, RNt, xform(_, XFormName)) },
			{ Head = functor(_, HeadArgs, _) }
		->
			{ append(HeadArgs, [Ts0], Then1Args) },
			{ XFTerm = functor(atom(XFormName), Then1Args, Ctxt) }
		;
			{ XFTerm = Ts0 }
		),
		{ Then1 = functor(atom("="), [Ts, XFTerm], Ctxt) },
		{ Then = functor(atom(","), [Then0, Then1], Ctxt) },
		{ BodyTerm = functor(atom(";"),[
			functor(atom("->"), [
				Cond,
				Then
			], Ctxt),
			functor(atom("error"),
				[functor(string(Err), [], Ctxt)],
				Ctxt
			)], Ctxt) },
		( { term_to_goal(BodyTerm, Goal0) } ->
			{ Goal = Goal0 }
		;
			{ error("write_reductions: failed to convert goal") }
		),
		{ Clause = clause(
			functor(atom(RedName),  [St0, St, Sy0, Sy, Ts0, Ts], Ctxt),
			Goal, Varset12) },
		write_element(lines, Clause),
		nl
		)
	), Rules),
	foldl((pred(State::in, _TerminalAction::in, di, uo) is det -->
			( if not { Rules `contains` State } then
				io__format("\
reduce0(0x%x, _, _, _, _, _, _) :-
	reduce0_error(0x%x).

",
					[i(State), i(State)]
				)
			)
		),
		Table
	),
	io__format("\
:- pred reduce0_error(int).
:- mode reduce0_error(in) is erroneous.

reduce0_error(State) :-
	error(string__format(""reduce in state 0x%%x"", [i(State)])).

",
		[]
	).

:- pred mkstacks(list(bodyterm), term, term, term, term, varset, varset).
:- mode mkstacks(in, in, out, in, out, in, out) is det.

mkstacks([], St, St, Sy, Sy, VS, VS).
mkstacks([E0|Es], St0, St, Sy0, Sy, VS0, VS) :-
	new_var(VS0, U, VS1),
	context_init(Ctxt),
	(
		E0 = terminal(ET),
		E = functor(atom("t"), [ET], Ctxt)
	;
		E0 = nonterminal(EN),
		E = functor(atom("n"), [EN], Ctxt)
	),
	Sy1 = functor(atom("[|]"), [E, Sy0], Ctxt),
	St1 = functor(atom("[|]"), [variable(U), St0], Ctxt),
	mkstacks(Es, St1, St, Sy1, Sy, VS1, VS).

:- pred mkactions(list(term), term, term).
:- mode mkactions(in, in, out) is det.

mkactions([], Term, Term).
mkactions([E|Es], Term0, Term) :-
	context_init(Ctxt),
	Term1 = functor(atom(","), [E, Term0], Ctxt),
	mkactions(Es, Term1, Term).

%------------------------------------------------------------------------------%

:- pred sub(string, list(pair(string)), string).
:- mode sub(in, in, out) is det.

sub(Orig, Subs, Final) :-
	foldl((pred(Sub::in, S0::in, S1::out) is det :-
		Sub = From - To,
		string__replace_all(S0, From, To, S1)
	), Subs, Orig, Final).

