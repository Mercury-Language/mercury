%----------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003, 2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%----------------------------------------------------------------------------%
%
% file: check.m
% main author: conway,	November 1998
%
% This module implements various checking predicates for checking the
% input to moose. It checks for the following things:
%	- duplicate rule declarations.
%	- declared rules with no productions.
%	- productions with no rule declaraion.
%	- nonterminals with no rule declaraion.
%	- productions that are not connected to the start rule.
%	- productions that have no finite derivations.
%
% Unfortunately, we don't do anything about these yet. We should attempt
% to correct these errors so that we can look for later errors.
%
%------------------------------------------------------------------------------%

:- module check.
:- interface.

:- import_module grammar.

:- import_module io.
:- import_module list.
:- import_module string.
:- import_module term.

%------------------------------------------------------------------------------%

:- type check.error
	--->	error(list(string), context).

:- pred check_rule_decls(list(rule_decl), rule_decls, list(check.error)).
:- mode check_rule_decls(in, out, out) is det.

:- pred check_clauses(list(clause), rule_decls, clauses, list(check.error)).
:- mode check_clauses(in, in, out, out) is det.

:- pred check_useless(nonterminal, clauses, rule_decls, list(check.error)).
:- mode check_useless(in, in, in, out) is det.

:- pred check_inf_derivations(clauses, rule_decls, list(check.error)).
:- mode check_inf_derivations(in, in, out) is det.

	% write an error message to stderr.
:- pred write_error(check.error, io.state, io.state).
:- mode write_error(in, di, uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module require.
:- import_module set.
:- import_module solutions.

%------------------------------------------------------------------------------%

check_rule_decls(DeclList, Decls, Errors) :-
	map.init(Decls0),
	check_rule_decls(DeclList, Decls0, Decls, Errors).

:- pred check_rule_decls(list(rule_decl), rule_decls, rule_decls,
		list(check.error)).
:- mode check_rule_decls(in, in, out, out) is det.

check_rule_decls([], !Decls, []).
check_rule_decls([Decl | DeclList], !Decls, Errors) :-
	Decl = rule(DeclId, _Args, _VarSet, DeclContext),
		% Look to see if we already have a declaration for this rule.
	( map.search(!.Decls, DeclId, PrevDecl) ->
		PrevDecl = rule(_, _, _, PrevDeclContext),
		id(DeclId, Name, Arity),
		string.format("The previous declaration for %s/%d is here.",
			[s(Name), i(Arity)], Msg0),
		Err0 = error([Msg0], PrevDeclContext),
		string.format("Duplicate declaration for %s/%d.",
			[s(Name), i(Arity)], Msg1),
		Err1 = error([Msg1], DeclContext),
		Errors = [Err0, Err1 | Errors0],
		check_rule_decls(DeclList, !Decls, Errors0)
	;
		map.set(DeclId, Decl, !Decls),
		check_rule_decls(DeclList, !Decls, Errors)
	).

%------------------------------------------------------------------------------%

check_clauses(ClauseList, Decls, Clauses, Errors) :-
	map.init(Clauses0),
	check_clauses0(ClauseList, Decls, Clauses0, Clauses, Errors0),

	map.keys(Decls, DeclIds),
	set.sorted_list_to_set(DeclIds, DeclSet),
	map.keys(Clauses, ClauseIds),
	set.sorted_list_to_set(ClauseIds, ClauseSet),
	NoDeclSet = ClauseSet `set.difference` DeclSet,
	NoClauseSet = DeclSet `set.difference` ClauseSet,

		% Productions that have no rule declaration.
	set.to_sorted_list(NoDeclSet, NoDeclList),
	list.map((pred(NoDeclId::in, NoDeclError::out) is det :-
		map.lookup(Clauses, NoDeclId, List),
		( List = [clause(_, _, _, NoDeclContext)|_] ->
			id(NoDeclId, NoDeclName, NoDeclArity),
			string.format("No rule declaration for %s/%d.",
				[s(NoDeclName), i(NoDeclArity)], NoDeclMsg),
			NoDeclError = error([NoDeclMsg], NoDeclContext)
		;
			error("check_clauses: no clause ids")
		)
	), NoDeclList, Errors1),

		% Rules that have no productions.
	set.to_sorted_list(NoClauseSet, NoClauseList),
	list.map((pred(NoClauseId::in, NoClauseError::out) is det :-
		map.lookup(Decls, NoClauseId, Decl),
		Decl = rule(_, _, _, NoClauseContext),
		id(NoClauseId, NoClauseName, NoClauseArity),
		string.format("No productions for %s/%d.",
			[s(NoClauseName), i(NoClauseArity)], NoClauseMsg),
		NoClauseError = error([NoClauseMsg], NoClauseContext)
	), NoClauseList, Errors2),

	list.condense([Errors0, Errors1, Errors2], Errors).

:- pred check_clauses0(list(clause), rule_decls, clauses, clauses,
		list(check.error)).
:- mode check_clauses0(in, in, in, out, out) is det.

check_clauses0([], _Decls, !Clauses, []).
check_clauses0([Clause | ClauseList], Decls, !Clauses, Errors) :-
	Clause = clause(Head, Prod, _, Context),
	Id = nonterminal(Head),
	( map.search(!.Clauses, Id, ClauseList0) ->
		list.append(ClauseList0, [Clause], ClauseList1)
	;
		ClauseList1 = [Clause]
	),
	map.set(Id, ClauseList1, !Clauses),

		% Look for used nonterminals that are not declared.
	solutions((pred(NonTermId::out) is nondet :-
			% XXX performance
		nonterminals(Prod, NonTermIds),
		list.member(NonTermId, NonTermIds),
		not contains(Decls, NonTermId)
	), UnDeclaredIds),
	list.map((pred(UnDeclaredId::in, UnDeclaredError::out) is det :-
		id(Id, CN, CA),
		id(UnDeclaredId, NN, NA),
		string.format("In production for %s/%d,", 
			[s(CN), i(CA)], Msg0),
		string.format("  the nonterminal %s/%d is undeclared.",
			[s(NN), i(NA)], Msg1),
		UnDeclaredError = error([Msg0, Msg1], Context)
	), UnDeclaredIds, Errors0),
	(
		Errors0 = [],
		check_clauses0(ClauseList, Decls, !Clauses, Errors)
	;
			% Not tail recursive, so only do it if we have to.
		Errors0 = [_|_],
		check_clauses0(ClauseList, Decls, !Clauses, Errors1),
		list.append(Errors0, Errors1, Errors)
	).

%------------------------------------------------------------------------------%

check_useless(Start, Clauses, Decls, Errors) :-
	StartSet = set.make_singleton_set(Start), 
	useful(StartSet, Clauses, StartSet, UsefulSet),
	map.keys(Clauses, AllIds),
	set.sorted_list_to_set(AllIds, AllSet),
	UselessSet = AllSet `set.difference` UsefulSet,
	set.to_sorted_list(UselessSet, UselessList),
	list.filter_map((pred(UselessId::in, Error::out) is semidet :-
			% Use search rather than lookup in case
			% it was an undeclared rule.
		map.search(Decls, UselessId, Decl),
		Decl = rule(_Id, _Args, _VarSet, Context),
		UselessId = Name / Arity,
		string.format("Grammar rule %s/%d is not used.", 
			[s(Name), i(Arity)], Msg),
		Error = error([Msg], Context)
	), UselessList, Errors).

	% Perform a fixpoint computation to find all the nonterminals
	% that are reachable from the start symbol.
:- pred useful(set(nonterminal), clauses, set(nonterminal), set(nonterminal)).
:- mode useful(in, in, in, out) is det.

useful(New0, Clauses, !Useful) :-
	( set.empty(New0) ->
		true
	;
		solutions_set((pred(UId::out) is nondet :-
			set.member(Id, New0),
			map.search(Clauses, Id, ClauseList),
			list.member(Clause, ClauseList),
			Clause = clause(_Head, Prod, _VarSet, _Context),
			nonterminal(UId, Prod)
		), NewSet),
		New1 = NewSet `set.difference` !.Useful,
		!:Useful = New1 `set.union`!.Useful,
		useful(New1, Clauses, !Useful)
	).

:- pred nonterminal(nonterminal, prod).
:- mode nonterminal(out, in) is nondet.

nonterminal(nonterminal(Term), nonterminal(Term)).
nonterminal(NonTerminal, (A, B)) :-
	(
		nonterminal(NonTerminal, A)
	;
		nonterminal(NonTerminal, B)
	).
nonterminal(NonTerminal, (A ; B)) :-
	(
		nonterminal(NonTerminal, A)
	;
		nonterminal(NonTerminal, B)
	).

%------------------------------------------------------------------------------%

check_inf_derivations(Clauses, Decls, Errors) :-
	map.keys(Clauses, AllIds),
	set.sorted_list_to_set(AllIds, InfSet0),
	set.init(FinSet0),
	finite(InfSet0, FinSet0, Clauses, InfSet),
	set.to_sorted_list(InfSet, InfList),
	list.filter_map((pred(InfId::in, Error::out) is semidet :-
			% Use search rather than lookup in case
			% it was an undeclared rule.
		map.search(Decls, InfId, Decl),
		Decl = rule(_Id, _Args, _VarSet, Context),
		InfId = Name / Arity,
		string.format("Rule %s/%d does not have any finite derivations.",
			[s(Name), i(Arity)], Msg),
		Error = error([Msg], Context)
	), InfList, Errors).

:- pred finite(set(nonterminal), set(nonterminal), clauses, set(nonterminal)).
:- mode finite(in, in, in, out) is det.

finite(!.Inf, Fin0, Clauses, !:Inf) :-
	solutions_set((pred(NewFinId::out) is nondet :-
		set.member(NewFinId, !.Inf),
			% search rather than lookup in case the nonterminal
			% doesn't have any clauses. This may lead to
			% spurious infinite derivations.
		map.search(Clauses, NewFinId, ClauseList),
		list.member(Clause, ClauseList),
		Clause = clause(_Head, Prod, _VarSet, _Context),
		nonterminals(Prod, NonTerms),
		(
			NonTerms = []
		;
			NonTerms = [_|_],
			all [NId] (
				list.member(NId, NonTerms) => 
					set.member(NId, Fin0)
			)
		)
	), NewFinSet),
	NewFin = NewFinSet `set.difference` Fin0,
	( set.empty(NewFin) ->
		true
	;
		!:Inf = !.Inf `set.difference` NewFin,
		Fin = Fin0 `set.union` NewFin,
		finite(!.Inf, Fin, Clauses, !:Inf)
	).

:- pred nonterminals(prod, list(nonterminal)).
:- mode nonterminals(in, out) is nondet.

nonterminals([], []).
nonterminals(terminal(_), []).
nonterminals(nonterminal(Term), [nonterminal(Term)]).
nonterminals((A, B), Syms) :-
	nonterminals(A, ASyms),
	nonterminals(B, BSyms),
	append(ASyms, BSyms, Syms).
nonterminals((A ; _B), Syms) :-
	nonterminals(A, Syms).
nonterminals((_A ; B), Syms) :-
	nonterminals(B, Syms).
nonterminals(action(_), []).

%------------------------------------------------------------------------------%

:- pred id(nonterminal, name, arity) is det.
:- mode id(in, out, out) is det.

id(Name/Arity, Name, Arity).
id(start, _, _) :-
	error("id: unexpected start").

%------------------------------------------------------------------------------%

write_error(error(MsgLines, Context), !IO) :-
	Context = term.context(File, Line),
	string.format("%s:%d: ", [s(File), i(Line)], ContextMsg),
	io.stderr_stream(StdErr, !IO),
	list.foldl((pred(Msg::in, !.IO::di, !:IO::uo) is det :-
		io.write_string(StdErr, ContextMsg, !IO),
		io.write_string(StdErr, Msg, !IO),
		io.nl(StdErr, !IO)
	), MsgLines, !IO).

%------------------------------------------------------------------------------%

