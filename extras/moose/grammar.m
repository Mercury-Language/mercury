%----------------------------------------------------------------------------%
% Copyright (C) 1998-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%----------------------------------------------------------------------------%
%
% file: grammar.m
% main author: conway,	November 1998
%
% This module defines the representation(s) of grammars that moose uses
% to construct parsers.
%
%------------------------------------------------------------------------------%

:- module grammar.

:- interface.

:- import_module misc.
:- import_module array, list, map, set, term, varset.

:- type grammar	
	--->	grammar(
			rules,
			clauses,
			xforms,
			int,		% Next nonterminal
			index,		% Rule index
			first,
			follow
		).

	% index maps from each nonterminal to the list (set) of normalized
	% rules for that nonterminal.
:- type index	== map(nonterminal, list(int)).

:- type clauses	== map(nonterminal, list(clause)).

:- type (clause)
	--->	clause(
			term,	% Head
			prod,	% body
			varset,
			context % Context of the `--->'
		).

:- type prod
	--->	terminal(term)
	;	nonterminal(term)
	;	( prod , prod )
	;	{ prod ;  prod }
	;	action(term)
	;	[]	% epsilon
	.

:- type name == string.
:- type arity == int.

:- type terminal
	--->	epsilon	% epsilon isn't really a terminal, but it avoids the
			% need for wrappers in the FIRST(alpha) situations.
	;	(name / arity)
	;	($)	% the special end-of-input symbol
	;	(*)	% the dummy symbol used for lookahead computation.
	.

:- type nonterminal
	--->	start	% S' - the distinguished start symbol. Will always
			% correspond to prodnum == 0.
	;	(name / arity)
	.

:- type symbol
	--->	terminal(terminal)
	;	nonterminal(nonterminal)
	.

:- type symbols	== array(symbol).

:- type bodyterm
	--->	terminal(term)
	;	nonterminal(term)
	.

:- type rule_decls == map(nonterminal, rule_decl).

:- type rule_decl
	--->	rule(
			nonterminal,	% Name/Arity
			list(term),	% types of the attributes
			varset,		% type variables of the attributes
			context		% context of the declaration.
		).

:- type rules	== (int -> (rule)).

:- type (rule)
	--->	rule(
			nonterminal,	% the nonterm that this rule belongs to
			term,		% Head
			symbols,	% Body
			list(bodyterm),	% NTs with their arguments
			list(term),	% Actions
			varset,
			context		% context from the clause.
		).

:- type xform
	--->	xform(
			nonterminal,
			string
		).

:- type xforms	==	(nonterminal -> xform).

:- type first	==	(nonterminal -> set(terminal)).

:- type follow	==	(nonterminal -> set(terminal)).

:- type state	== int.

:- type action
	--->	accept
	;	shift(int)
	;	reduce(int)
	.

:- type actiontable == (state -> terminal -> action).

:- type gototable == (state -> nonterminal -> state).

:- pred term_to_clause(term, varset, nonterminal, clause).
:- mode term_to_clause(in, in, out, out) is semidet.

:- pred add_clause(clauses, nonterminal, clause, clauses).
:- mode add_clause(in, in, in, out) is det.

:- pred construct_grammar(nonterminal, clauses, xforms, grammar).
:- mode construct_grammar(in, in, in, out) is det.

:- pred compute_first(rules, first).
:- mode compute_first(in, out) is det.

:- pred compute_follow(rules, nonterminal, terminal, first, follow).
:- mode compute_follow(in, in, in, in, out) is det.

	% Misc predicates.

:- func terminal(term) = terminal.
:- func nonterminal(term) = nonterminal.
:- func first(first, symbols, int) = set(terminal).

%------------------------------------------------------------------------------%

:- implementation.

:- import_module misc.
:- import_module bool, int, require, std_util, string.

%------------------------------------------------------------------------------%

%------------------------------------------------------------------------------%

term_to_clause(functor(Atom, Args, Context), VarSet, Id, Rule) :-
	Atom = atom("--->"),
	Args = [Head, Body],
	Head = functor(atom(Name), HeadArgs, _),
	list__length(HeadArgs, Arity),
	Id = Name/Arity,
	Rule = clause(Head, Prod, VarSet, Context),
	term_to_prod(Body, Prod).

:- pred term_to_prod(term, prod).
:- mode term_to_prod(in, out) is semidet.

term_to_prod(functor(Atom, Args, Ctxt), Prod) :-
	( Atom = atom(","), Args = [Arg1, Arg2] ->
		term_to_prod(Arg1, Left),
		term_to_prod(Arg2, Right),
		Prod = (Left, Right)
	; Atom = atom(";"), Args = [Arg1, Arg2] ->
		term_to_prod(Arg1, Left),
		term_to_prod(Arg2, Right),
		Prod = (Left; Right)
	; Atom = atom("{}"), Args = [Goal] ->
		Prod = action(Goal)
	; Atom = atom("{}"), Args = [Goal | Goals] ->
		foldl((pred(G::in, Left::in, (Left, action(G))::out) is det),
			Goals, action(Goal), Prod)
	; Atom = atom("[]"), Args = [] ->
		Prod = []
	; Atom = atom("[|]"), Args = [Head, Tail] ->
		terminals(Tail, terminal(Head), Prod)
	;
		Prod = nonterminal(functor(Atom, Args, Ctxt))
	).

:- pred terminals(term, prod, prod).
:- mode terminals(in, in, out) is semidet.

terminals(functor(Atom, Args, _), Prod0, Prod) :-
	( Atom = atom("[]"), Args = [] ->
		Prod = Prod0
	; Atom = atom("[|]"), Args = [Head, Tail] ->
		terminals(Tail, (Prod0, terminal(Head)), Prod)
	;
		fail
	).

%------------------------------------------------------------------------------%

add_clause(Clauses0, Id, Clause, Clauses) :-
	( map__search(Clauses0, Id, These0) ->
		These = [Clause|These0]
	;
		These = [Clause]
	),
	map__set(Clauses0, Id, These, Clauses).

%------------------------------------------------------------------------------%

construct_grammar(Start, AllClauses, XForms, Grammar) :-
	map__to_assoc_list(AllClauses, ClauseList),
	Nont0 = 1,
	start_rule(Start, StartRule),
	map__from_assoc_list([0 - StartRule], Rules0),
	map__init(Xfs0),
	map__init(ClauseIndex0),
	map__init(First0),
	map__init(Follow0),
	Grammar0 = grammar(Rules0, AllClauses, XForms, Nont0, ClauseIndex0,
		First0, Follow0),
	foldl(transform_clause_list, ClauseList, Grammar0, Grammar1),
	compute_first0(Grammar1, Grammar2),
	compute_follow0(Grammar2, Grammar3),
	Grammar3 = grammar(Rules3, AllClauses3, XForms3, Nont3, ClauseIndex3,
		First3, Follow3),
		
		% Keep the nonterminals in reverse sorted order
		% for efficient processing in lalr.m
	map__map_values((pred(_K::in, V0::in, V::out) is det :-
	    sort(V0, V1),
	    reverse(V1, V)
	), ClauseIndex3, ClauseIndex4),
	Grammar = grammar(Rules3, AllClauses3, XForms3, Nont3, ClauseIndex4,
		First3, Follow3).

:- pred start_rule(nonterminal, rule).
:- mode start_rule(in, out) is det.

start_rule(Id, Rule) :-
	(
		Id = Name/Arity
	;
		Id = start,
		error("epsilon start rule")
	),
	varset__init(VarSet0),
	varset__new_vars(VarSet0, Arity, Vars, VarSet1),
	foldl((pred(V::in, VS0::in, VS::out) is det :-
		var_to_int(V, I),
		format("V%d", [i(I)], N),
		varset__name_var(VS0, V, N, VS)
	), Vars, VarSet1, VarSet),
	term__var_list_to_term_list(Vars, Args),
	Context = context("foobie", 1),
	string__append(Name, "'", NewName),
	NewId = start,
	Head = functor(atom(NewName), Args, Context),
	Body = array([nonterminal(Id)]), 
	Body1 = [nonterminal(functor(atom(Name), Args, Context))],
	Rule = rule(NewId, Head, Body, Body1, [], VarSet, Context).

:- pred transform_clause_list(pair(nonterminal, list(clause)),
		grammar, grammar).
:- mode transform_clause_list(in, in, out) is det.

transform_clause_list(Id - Clauses, Grammar0, Grammar) :-
	foldl(transform_clause(Id), Clauses, Grammar0, Grammar).

:- pred transform_clause(nonterminal, clause, grammar, grammar).
:- mode transform_clause(in, in, in, out) is det.

transform_clause(Id, Clause, Grammar0, Grammar) :-
	Clause = clause(Head, Prod, Varset, Context),
	solutions(transform_prod(Prod), Bodies),
	foldl(add_rule(Id, Head, Varset, Context), Bodies, Grammar0, Grammar).

:- pred add_rule(nonterminal, term, varset, context,
		pair(list(bodyterm), list(term)), grammar, grammar).
:- mode add_rule(in, in, in, in, in, in, out) is det.

add_rule(Id, Head, Varset, Context, BodyTerms - Actions, Grammar0, Grammar) :-
	Grammar0 = grammar(Rules0, C, Xfs, Nont0, ClauseIndex0, F, L),
	map((pred(BodyTerm::in, BodyId::out) is det :-
		(
			BodyTerm = terminal(Term),
			( Term = functor(atom(Name), Args, _) ->
				length(Args, Arity),
				BId0 = Name/Arity
			;
				error("add_rule: bad body term")
			),
			BodyId = terminal(BId0)
		;
			BodyTerm = nonterminal(Term),
			( Term = functor(atom(Name), Args, _) ->
				length(Args, Arity),
				BId0 = Name/Arity
			;
				error("add_rule: bad body term")
			),
			BodyId = nonterminal(BId0)
		)
	), BodyTerms, BodyIds),
	Rule = rule(Id, Head, array(BodyIds), BodyTerms, Actions,
			Varset, Context),
	add_rule(Rules0, Nont0, Rule, Rules),
	Nont = Nont0 + 1,
	( map__search(ClauseIndex0, Id, Prods0) ->
		Prods = [Nont0|Prods0]
	;
		Prods = [Nont0]
	),
	map__set(ClauseIndex0, Id, Prods, ClauseIndex),
	Grammar = grammar(Rules, C, Xfs, Nont, ClauseIndex, F, L).

:- pred transform_prod(prod, pair(list(bodyterm), list(term))).
:- mode transform_prod(in, out) is multi.

transform_prod(terminal(Term), [terminal(Term)] - []).
transform_prod(nonterminal(Term), [nonterminal(Term)] - []).
transform_prod(action(Term), [] - [Term]).
transform_prod((ProdA, ProdB), Body - Actions) :-
	transform_prod(ProdA, BodyA - ActionsA),
	transform_prod(ProdB, BodyB - ActionsB),
	list__append(BodyA, BodyB, Body),
	list__append(ActionsA, ActionsB, Actions).
transform_prod((ProdA ; ProdB), Result) :-
	(
		transform_prod(ProdA, Result)
	;
		transform_prod(ProdB, Result)
	).
transform_prod([], [] - []).

terminal(Term) = Terminal :-
	(
		Term = functor(atom(Name), Args, _),
		length(Args, Arity)
	->
		Terminal = Name / Arity
	;
		error("terminal: bad term")
	).

nonterminal(Term) = Terminal :-
	(
		Term = functor(atom(Name), Args, _),
		length(Args, Arity)
	->
		Terminal = Name / Arity
	;
		error("nonterminal: bad term")
	).

%------------------------------------------------------------------------------%

	% The computation of the first sets is directly from
	% the dragon book.

:- pred compute_first0(grammar, grammar).
:- mode compute_first0(in, out) is det.

compute_first0(Grammar0, Grammar) :-
	Grammar0 = grammar(Rules, Clauses, Xfs, Nont, Index, _, Follow),
	compute_first(Rules, First),
	Grammar = grammar(Rules, Clauses, Xfs, Nont, Index, First, Follow).

:- type first_stuff
	--->	stuff(
			bool,		% Changed?
			list(nonterminal),	% Nonterminals
			rules,
			first
		).

compute_first(Rules, First) :-
	collect_nonterminals(Rules, Nonterminals),
	map__init(First0),
	Stuff0 = stuff(no, Nonterminals, Rules, First0),
	until((pred(Stuff1::in, Stuff3::out) is det :-
		Stuff1 = stuff(_, N1, R1, F1),
		Stuff2 = stuff(no, N1, R1, F1),
		foldl(compute_first, Rules, Stuff2, Stuff3)
	),
	(pred(StuffN::in) is semidet :-
		StuffN = stuff(no, _, _, _)
	), Stuff0, Stuff),
	Stuff = stuff(_, _, _, First).

:- pred compute_first(int, (rule), first_stuff, first_stuff).
:- mode compute_first(in, in, in, out) is det.

compute_first(_RuleNum, Rule, Stuff0, Stuff) :-
	Rule = rule(Id, _Head, Elems, _Body, _Actions, _Varset, _Context),
	array__max(Elems, Max),
	( Max >= 0 ->
			% If there are literals in the body of the
			% rule, then compute the first set that derives
			% from what we currently know...
		Stuff0 = stuff(_, _, _, TmpFirst),
		set__init(Emp),
		compute_first(0, Max, Elems, TmpFirst, Emp, ComputedFirst)
	;
			% There were no literals in the body of the rule,
			% so it was an epsilon rule.
		ComputedFirst = { epsilon }
	),
			% Add the computed first set to what we currently
			% know, noting whether or not anything has changed.
	Stuff0 = stuff(Ch0, Ns, Rs, First0),
	(
		search(First0, Id, ThisFirst0)
	->
		difference(ComputedFirst, ThisFirst0, NewFirst),
		union(ThisFirst0, NewFirst, ThisFirst),
		( empty(NewFirst) ->
			Ch1 = Ch0
		;
			Ch1 = yes
		)
	;
		ThisFirst = ComputedFirst,
		Ch1 = yes
	),
	set(First0, Id, ThisFirst, First1),
	Stuff = stuff(Ch1, Ns, Rs, First1).


		% Compute the first set directly from what we currently
		% know (using rule 3 on p189 of the dragon book):
		% iterate over the body until we get to
		%	- the end
		%	- an element about which we know nothing,
		%	- a terminal
		%	- a first set for a nonterminal that does not
		%		contain epsilon

:- pred compute_first(int, int, symbols, first, set(terminal), set(terminal)).
:- mode compute_first(in, in, in, in, in, out) is det.

compute_first(I, IMax, Elems, First, Set0, Set) :-
	( I =< IMax ->
		lookup(Elems, I, Elem),
		(
				% If we get to a terminal, then we add it
				% to the first set, and remove epsilon (if
				% it was there in the first place), since
				% this rule is certainly not nullable.
			Elem = terminal(Id),
			insert(Set0, Id, Set1),
			difference(Set1, { epsilon }, Set)
		;
			Elem = nonterminal(Id),
			( search(First, Id, Set1) ->
					% If we know some information about
					% the nonterminal, then add it to
					% what we already know. If it is
					% not nullable, then this rule is
					% not nullable, and we're done. If
					% it is nullable, then we look at
					% the next literal in the body.
				union(Set0, Set1, Set2),
				( member(epsilon, Set1) ->
					compute_first(I + 1, IMax, Elems, First,
						Set2, Set)
				;
					difference(Set2, { epsilon }, Set)
				)
			;
					% If we don't know anything about
					% this nonterminal, then stop here.
				Set = Set0
			)
		)
	;
		Set = Set0
	).

:- pred collect_terminals(rules, set(terminal)).
:- mode collect_terminals(in, out) is det.

collect_terminals(Rules, Terminals) :-
	foldl((pred(_RN::in, Rule::in, Ts0::in, Ts::out) is det :-
		Rule = rule(_Id, _Head, Elems, _, _, _, _),
		foldl((pred(Elem::in, Ts1::in, Ts2::out) is det :-
			(
				Elem = terminal(Id),
				Ts2 = [Id|Ts1]
			;
				Elem = nonterminal(_Id_),
				Ts2 = Ts1
			)
		), Elems, Ts0, Ts)
	), Rules, [], TerminalsList),
	set__list_to_set(TerminalsList, Terminals).

:- pred collect_nonterminals(rules, list(nonterminal)).
:- mode collect_nonterminals(in, out) is det.

collect_nonterminals(Rules, Nonterminals) :-
	foldl((pred(_RN ::in, Rule::in, Ts0::in, Ts::out) is det :-
		Rule = rule(Id, _Head, _Elems, _, _, _Varset, _Context),
		Ts = [Id|Ts0]
	), Rules, [], NonterminalsList),
	set__list_to_set(NonterminalsList, Nonterminals0),
	set__to_sorted_list(Nonterminals0, Nonterminals).

	% YYY This probably belongs in array.m
:- pred foldl(pred(T, U, U), array(T), U, U).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.

foldl(Closure, Array, Acc0, Acc) :-
	array__max(Array, Max),
	foldl(0, Max, Closure, Array, Acc0, Acc).

:- pred foldl(int, int, pred(T, U, U), array(T), U, U).
:- mode foldl(in, in, pred(in, in, out) is det, in, in, out) is det.

foldl(I, IMax, Closure, Array, Acc0, Acc) :-
	( I =< IMax ->
		lookup(Array, I, Elem),
		call(Closure, Elem, Acc0, Acc1),
		foldl(I + 1, IMax, Closure, Array, Acc1, Acc)
	;
		Acc = Acc0
	).

	% YYY This probably belongs in the library somewhere.
:- pred while(pred(T), pred(T, T), T, T).
:- mode while(pred(in) is semidet, pred(in, out) is det, in, out) is det.

while(Cond, Body, Acc0, Acc) :-
	( call(Cond, Acc0) ->
		call(Body, Acc0, Acc1),
		while(Cond, Body, Acc1, Acc)
	;
		Acc = Acc0
	).

	% YYY This probably belongs in the library somewhere.
:- pred until(pred(T, T), pred(T), T, T).
:- mode until(pred(in, out) is det, pred(in) is semidet, in, out) is det.

until(Body, Cond, Acc0, Acc) :-
	call(Body, Acc0, Acc1),
	( call(Cond, Acc1) ->
		Acc = Acc1
	;
		until(Body, Cond, Acc1, Acc)
	).

%------------------------------------------------------------------------------%

	% The computation of the follow sets is directly from
	% the dragon book.

:- pred compute_follow0(grammar, grammar).
:- mode compute_follow0(in, out) is det.

compute_follow0(Grammar0, Grammar) :-
	Grammar0 = grammar(Rules, Clauses, Xfs, Nont, Index, First, _),
	compute_follow(Rules, start, ($), First, Follow),
	Grammar = grammar(Rules, Clauses, Xfs, Nont, Index, First, Follow).

:- type follow_stuff
	--->	stuff(
			bool,		% Changed?
			list(nonterminal),	% Nonterminals
			rules,
			first,
			follow
		).

compute_follow(Rules, Start, EOF, First, Follow) :-
	map__init(Follow0),
		% Rule 1
	map__set(Follow0, Start, { EOF }, Follow1),
	collect_nonterminals(Rules, Ns),
	Stuff0 = stuff(no, Ns, Rules, First, Follow1),
	until((pred(Stuff1::in, Stuff3::out) is det :-
		Stuff1 = stuff(_, N1, R1, Fi1, Fo1),
		Stuff2 = stuff(no, N1, R1, Fi1, Fo1),
		foldl(compute_follow, Rules, Stuff2, Stuff3)
	),
	(pred(StuffN::in) is semidet :-
		StuffN = stuff(no, _, _, _, _)
	), Stuff0, Stuff),
	Stuff = stuff(_, _, _, _, Follow).

:- pred compute_follow(int, (rule), follow_stuff, follow_stuff).
:- mode compute_follow(in, in, in, out) is det.

compute_follow(_RuleNum, Rule, Stuff0, Stuff) :-
	Rule = rule(Id, _Head, Elems, _, _, _Varset, _Context),
	Stuff0 = stuff(_, _, _, First, _),
	array__max(Elems, Max),
		% Apply Rule 2
	compute_follow2(0, Max, First, Elems, Stuff0, Stuff1),
	compute_follow3(Max, First, Id, Elems, Stuff1, Stuff).

:- pred compute_follow2(int, int, first, symbols, follow_stuff, follow_stuff).
:- mode compute_follow2(in, in, in, in, in, out) is det.

compute_follow2(I, IMax, First, Elems, Stuff0, Stuff) :-
	( I =< IMax ->
		lookup(Elems, I, Elem),
		( Elem = nonterminal(Id) ->
			IdFollow0 = first(First, Elems, I + 1),
			difference(IdFollow0, { epsilon }, IdFollow),
			add_follow(Id, IdFollow, Stuff0, Stuff1)
		;
			Stuff1 = Stuff0
		),
		compute_follow2(I + 1, IMax, First, Elems, Stuff1, Stuff)
	;
		Stuff = Stuff0
	).

:- pred compute_follow3(int, first, nonterminal, symbols,
		follow_stuff, follow_stuff).
:- mode compute_follow3(in, in, in, in, in, out) is det.

compute_follow3(I, First, MyId, Elems, Stuff0, Stuff) :-
	( I >= 0 ->
		lookup(Elems, I, Elem),
		( Elem = nonterminal(Id) ->
			get_follow(MyId, MyFollow, Stuff0, _),
			add_follow(Id, MyFollow, Stuff0, Stuff1),
			lookup(First, Id, IdFirst),
			( member(epsilon, IdFirst) ->
				compute_follow3(I - 1, First, MyId, Elems,
					Stuff1, Stuff)
			;
				Stuff = Stuff1
			)
		;
			Stuff = Stuff0
		)
	;
		Stuff = Stuff0
	).

:- pred get_follow(nonterminal, set(terminal), follow_stuff, follow_stuff).
:- mode get_follow(in, out, in, out) is det.

get_follow(Id, IdFollow, Stuff, Stuff) :-
	Stuff = stuff(_, _, _, _, Follow),
	( search(Follow, Id, IdFollow0) ->
		IdFollow = IdFollow0
	;
		set__init(IdFollow)
	).

:- pred add_follow(nonterminal, set(terminal), follow_stuff, follow_stuff).
:- mode add_follow(in, in, in, out) is det.

add_follow(Id, IdFollow0, Stuff0, Stuff) :-
	Stuff0 = stuff(Ch0, Ns, Rs, Fs, Follow0),
	( search(Follow0, Id, OldFollow) ->
		difference(IdFollow0, OldFollow, NewFollow),
		( empty(NewFollow) ->
			IdFollow = OldFollow,
			Ch = Ch0
		;
			union(OldFollow, NewFollow, IdFollow),
			Ch = yes
		)
	;
		IdFollow = IdFollow0,
		Ch = yes
	),
	set(Follow0, Id, IdFollow, Follow),
	Stuff = stuff(Ch, Ns, Rs, Fs, Follow).

%------------------------------------------------------------------------------%

first(First, Elems, I) = FirstI :-
	array__max(Elems, Max),
	( I =< Max ->
		lookup(Elems, I, Elem),
		(
			Elem = terminal(Id),
			FirstI = { Id }
		;
			Elem = nonterminal(Id),
			lookup(First, Id, FirstI0),
			( member(epsilon, FirstI0) ->
				RestFirst = first(First, Elems, I+1),
				union(FirstI0, RestFirst, FirstI)
			;
				FirstI = FirstI0
			)
		)
	;
		FirstI = { epsilon }
	).

%------------------------------------------------------------------------------%

:- pred add_rule(rules, int, rule, rules).
:- mode add_rule(in, in, in, out) is det.

add_rule(Rules0, Num, Rule, Rules) :-
	set(Rules0, Num, Rule, Rules).

%------------------------------------------------------------------------------%
