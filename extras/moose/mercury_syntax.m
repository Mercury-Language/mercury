%----------------------------------------------------------------------------%
% Copyright (C) 1998-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%----------------------------------------------------------------------------%

:- module mercury_syntax.

:- interface.

:- import_module io, list, term, varset.

:- type (module) == list(element).

	% Element is a type for pieces of a mercury module.
:- type element
	--->	pred(term, varset)	% Pred declarations
	;	func(term, varset)	% Func declarations
	;	type(type, varset)	% Type declarations
	;	mode(term, varset)	% Mode declarations
					% (both predicate modes and new modes)
	;	inst(term, varset)	% Inst declarations
	;	clause(term, goal, varset)	% Program clauses
	;	dcg_clause(term, goal, varset)
					% DCG clauses
	;	class(term, varset)	% Class declarations
	;	instance(term, varset)	% Instance declarations
	;	misc(term, varset)	% Anything else
	.

:- type module_result
	--->	module(module, list(module_error)).

:- type module_error
	--->	error(string, int).

:- pred read_module(module_result, io__state, io__state).
:- mode read_module(out, di, uo) is det.

:- type lines
	--->	lines
	;	nolines
	.

:- pred write_element(lines, element, io__state, io__state).
:- mode write_element(in, in, di, uo) is det.

:- pred write_module(lines, (module), io__state, io__state).
:- mode write_module(in, in, di, uo) is det.

:- type (type)
	--->	abstr(term)
	;	equiv(term, term)
	;	disj(term, list(term))
	.

:- pred term_to_type(term, (type)).
:- mode term_to_type(in, out) is semidet.

:- type goal
	--->	conj(list(goal))
	;	disj(list(goal))
	;	ite(goal, goal, goal)
	;	call(term)
	;	(=(term, term, context))
	;	not(goal)
	;	exists(vars, goal)
	;	forall(vars, goal)
	% 	(goal => goal) % XXX conflicts with type classes
	;	(goal <= goal)
	;	(goal <=> goal)
	.

:- pred term_to_goal(term, goal).
:- mode term_to_goal(in, out) is semidet.

:- pred write_goal(varset, goal, io__state, io__state).
:- mode write_goal(in, in, di, uo) is det.

:- type vars == list(var).

:- implementation.

:- import_module int, require, std_util, string, term_io.

%------------------------------------------------------------------------------%

read_module(Result) -->
	read_module([], [], Result0),
	{ Result0 = module(Module0, Errors0) },
	{ reverse(Module0, Module) },
	{ reverse(Errors0, Errors) },
	{ Result = module(Module, Errors) }.

:- type element_result
	--->	element(element)
	;	eof
	;	error(string, int)
	.

:- pred read_module(module, list(module_error), module_result,
		io__state, io__state).
:- mode read_module(in, in, out, di, uo) is det.

read_module(Module0, Errors0, Result) -->
	read_element(Result0),
	(
		{ Result0 = eof },
		{ Result = module(Module0, Errors0) }
	;
		{ Result0 = element(Element) },
		read_module([Element|Module0], Errors0, Result)
	;
		{ Result0 = error(Msg, Line) },
		read_module(Module0, [error(Msg, Line)|Errors0], Result)
	).

:- pred read_element(element_result, io__state, io__state).
:- mode read_element(out, di, uo) is det.

read_element(Result) -->
	read_term(Result0),
	(
		{ Result0 = eof },
		{ Result = eof }
	;
		{ Result0 = error(Msg, Line) },
		{ Result = error(Msg, Line) }
	;
		{ Result0 = term(VarSet, Term) },
		( { classify(Term, VarSet, Element0) } ->
			{ Element = Element0 }
		;
			{ Element = misc(Term, VarSet) }
		),
		{ Result = element(Element) }
	).

:- pred classify(term, varset, element).
:- mode classify(in, in, out) is semidet.

classify(Term, VarSet, Element) :-
	Term = functor(atom(Atom), Args, _),
	( Atom = ":-" -> (
		Args = [functor(atom("pred"), [PredDecl], _)],
		Element = pred(PredDecl, VarSet)
	;
		Args = [functor(atom("func"), [FuncDecl], _)],
		Element = func(FuncDecl, VarSet)
	;
		Args = [functor(atom("mode"), [ModeDecl], _)],
		Element = mode(ModeDecl, VarSet)
	;
		Args = [functor(atom("type"), [TypeTerm], _)],
		( mercury_syntax__term_to_type(TypeTerm, TypeDecl) ->
			Element = type(TypeDecl, VarSet)
		;
			Element = misc(Term, VarSet)
		)
	;
		Args = [functor(atom("inst"), [InstDecl], _)],
		Element = inst(InstDecl, VarSet)
	;
		Args = [functor(atom("class"), [ClassDecl], _)],
		Element = class(ClassDecl, VarSet)
	;
		Args = [functor(atom("instance"), [InstanceDecl], _)],
		Element = instance(InstanceDecl, VarSet)
	;
		Args = [Head, Body],
		( term_to_goal(Body, Goal) ->
			Element = clause(Head, Goal, VarSet)
		;
			Element = misc(Term, VarSet)
		)
	) ; Atom = "-->" ->
		Args = [Head, Body],
		( term_to_goal(Body, Goal) ->
			Element = dcg_clause(Head, Goal, VarSet)
		;
			Element = misc(Term, VarSet)
		)
	;
		Element = misc(Term, VarSet)
	).

%------------------------------------------------------------------------------%

write_module(_Lines, []) --> [].
write_module(Lines, [Element|Module]) -->
	write_element(Lines, Element),
	nl,
	write_module(Lines, Module).

write_element(Lines, pred(PredDecl, VarSet)) -->
	{ cons_decl("pred", PredDecl, Term) },
	write_term(Lines, 0, VarSet, Term),
	dot_nl.

write_element(Lines, func(FuncDecl, VarSet)) -->
	{ cons_decl("func", FuncDecl, Term) },
	write_term(Lines, 0, VarSet, Term),
	dot_nl.

write_element(Lines, type(TypeDecl, VarSet)) -->
	(
		{ TypeDecl = abstr(AbstrTerm) },
		{ cons_decl("type", AbstrTerm, Term) },
		write_term(Lines, 0, VarSet, Term)
	;
		{ TypeDecl = equiv(Head, Body) },
		{ get_context(Head, Context) },
		{ EqivTerm = functor(atom("=="), [Head, Body], Context) },
		{ cons_decl("type", EqivTerm, Term) },
		write_term(Lines, 0, VarSet, Term)
	;
		{ TypeDecl = disj(Head, Body) },
		{ get_context(Head, Context) },
		{ cons_type_body(Body, BodyTerm) },
		{ DeclTerm = functor(atom("--->"), [Head, BodyTerm], Context) },
		{ cons_decl("type", DeclTerm, Term) },
		write_term(Lines, 0, VarSet, Term)
	),
	dot_nl.

write_element(Lines, mode(ModeDecl, VarSet)) -->
	{ cons_decl("mode", ModeDecl, Term) },
	write_term(Lines, 0, VarSet, Term),
	dot_nl.

write_element(Lines, inst(InstDecl, VarSet)) -->
	{ cons_decl("inst", InstDecl, Term) },
	write_term(Lines, 0, VarSet, Term),
	dot_nl.

write_element(Lines, class(ClassDecl, VarSet)) -->
	{ cons_decl("class", ClassDecl, Term) },
	write_term(Lines, 0, VarSet, Term),
	dot_nl.

write_element(Lines, instance(InstanceDecl, VarSet)) -->
	{ cons_decl("instance", InstanceDecl, Term) },
	write_term(Lines, 0, VarSet, Term),
	dot_nl.

write_element(Lines, misc(Term, VarSet)) -->
	write_term(Lines, 0, VarSet, Term),
	dot_nl.

write_element(Lines, clause(Head, Goal, VarSet)) -->
	write_term(Lines, 0, VarSet, Head),
	write_string(" :-\n"),
	write_goal(Lines, 1, normal, Goal, VarSet),
	dot_nl.

write_element(Lines, dcg_clause(Head, Goal, VarSet)) -->
	write_term(Lines, 0, VarSet, Head),
	write_string(" -->\n"),
	write_goal(Lines, 1, dcg, Goal, VarSet),
	dot_nl.

%------------------------------------------------------------------------------%

:- type goal_type
	--->	normal
	;	dcg
	.

:- pred write_goal_term(lines, int, goal_type, term, varset,
		io__state, io__state).
:- mode write_goal_term(in, in, in, in, in, di, uo) is det.

write_goal_term(Lines, Ind, Type, Term, VarSet) -->
	( { term_to_conj(Term, Conjuncts) } ->
		write_conjuncts(Lines, Ind, Type, Conjuncts, VarSet)
	; { term_to_ite(Term, IfThens, Else) } ->
		write_ite_terms(Lines, Ind, Type, IfThens, Else, VarSet)
	; { term_to_disj(Term, Disjuncts) } ->
		write_disjuncts(Lines, Ind, Type, Disjuncts, VarSet)
	;
		% Too bad if it is a quantifier, { Goal }, etc.
		% Also too bad if it contains a pred expression...
		% You can add pretty things here...
		write_term(Lines, Ind, VarSet, Term)
	).

:- pred term_to_conj(term, list(term)).
:- mode term_to_conj(in, out) is semidet.

term_to_conj(functor(atom(","), [Head,Term], _), [Head|Tail]) :-
	( term_to_conj(Term, Tail0) ->
		Tail = Tail0
	;
		Tail = [Term]
	).

:- pred term_to_disj(term, list(term)).
:- mode term_to_disj(in, out) is semidet.

term_to_disj(functor(atom(";"), [Head,Term], _), [Head|Tail]) :-
	( term_to_disj(Term, Tail0) ->
		Tail = Tail0
	;
		Tail = [Term]
	).

:- pred term_to_ite(term, list(pair(term)), term).
:- mode term_to_ite(in, out, out) is semidet.

term_to_ite(functor(atom(";"), [Head,Else0], _), [If - Then|Rest], Else) :-
	Head = functor(atom("->"), [If, Then], _),
	( term_to_ite(Else0, Rest0, Else1) ->
		Rest = Rest0,
		Else = Else1
	;
		Rest = [],
		Else = Else0
	).

%------------------------------------------------------------------------------%

:- pred write_conjuncts(lines, int, goal_type, list(term), varset,
		io__state, io__state).
:- mode write_conjuncts(in, in, in, in, in, di, uo) is det.

write_conjuncts(_Lines, Ind, Type, [], _VarSet) -->
	write_ind(Ind),
	(
		{ Type = normal },
		write_string("true")
	;
		{ Type = dcg },
		write_string("{ true }")
	).

write_conjuncts(Lines, Ind, Type, [Goal], VarSet) -->
	write_goal_term(Lines, Ind, Type, Goal, VarSet).

write_conjuncts(Lines, Ind, Type, [Goal|Goals], VarSet) -->
	{ Goals = [_|_] },
	write_goal_term(Lines, Ind, Type, Goal, VarSet),
	write_string(",\n"),
	write_conjuncts(Lines, Ind, Type, Goals, VarSet).

%------------------------------------------------------------------------------%

:- pred write_disjuncts(lines, int, goal_type, list(term), varset,
		io__state, io__state).
:- mode write_disjuncts(in, in, in, in, in, di, uo) is det.

write_disjuncts(Lines, Ind, Type, Goals, VarSet) -->
	write_ind(Ind),
	write_string("(\n"),
	write_disjuncts0(Lines, Ind, Type, Goals, VarSet), nl,
	write_ind(Ind),
	write_string(")").

:- pred write_disjuncts0(lines, int, goal_type, list(term), varset,
		io__state, io__state).
:- mode write_disjuncts0(in, in, in, in, in, di, uo) is det.

write_disjuncts0(_Lines, Ind, Type, [], _VarSet) -->
	write_ind(Ind),
	(
		{ Type = normal },
		write_string("fail")
	;
		{ Type = dcg },
		write_string("{ fail }")
	).

write_disjuncts0(Lines, Ind, Type, [Goal], VarSet) -->
	write_goal_term(Lines, Ind + 1, Type, Goal, VarSet), nl.

write_disjuncts0(Lines, Ind, Type, [Goal|Goals], VarSet) -->
	{ Goals = [_|_] },
	write_goal_term(Lines, Ind + 1, Type, Goal, VarSet), nl,
	write_ind(Ind),
	write_string(";\n"),
	write_disjuncts0(Lines, Ind, Type, Goals, VarSet).

%------------------------------------------------------------------------------%

:- pred write_ite_terms(lines, int, goal_type, list(pair(term)), term, varset,
		io__state, io__state).
:- mode write_ite_terms(in, in, in, in, in, in, di, uo) is det.

write_ite_terms(Lines, Ind, Type, IfThens, Else, VarSet) -->
	write_ind(Ind),
	write_string("(\n"),
	write_ite_terms0(Lines, Ind, Type, IfThens, VarSet),
	write_ind(Ind),
	write_string(";\n"),
	write_goal_term(Lines, Ind + 1, Type, Else, VarSet), nl,
	write_ind(Ind),
	write_string(")").

:- pred write_ite_terms0(lines, int, goal_type, list(pair(term)), varset,
		io__state, io__state).
:- mode write_ite_terms0(in, in, in, in, in, di, uo) is det.

write_ite_terms0(_Lines, _Ind, _Type, [], _VarSet) -->
	{ error("no if-thens") }.
write_ite_terms0(Lines, Ind, Type, [If - Then], VarSet) -->
	write_goal_term(Lines, Ind + 1, Type, If, VarSet), nl,
	write_ind(Ind),
	write_string("->\n"),
	write_goal_term(Lines, Ind + 1, Type, Then, VarSet), nl.
write_ite_terms0(Lines, Ind, Type, [If - Then|Rest], VarSet) -->
	{ Rest = [_|_] },
	write_goal_term(Lines, Ind + 1, Type, If, VarSet), nl,
	write_ind(Ind),
	write_string("->\n"),
	write_goal_term(Lines, Ind + 1, Type, Then, VarSet), nl,
	write_ind(Ind),
	write_string(";\n"),
	write_ite_terms0(Lines, Ind, Type, Rest, VarSet).

%------------------------------------------------------------------------------%

:- pred cons_decl(string, term, term).
:- mode cons_decl(in, in, out) is det.

cons_decl(Atom, DeclTerm, Term) :-
	get_context(DeclTerm, Context),
	Term = functor(atom(":-"),
		[functor(atom(Atom), [DeclTerm], Context)],
		Context).

:- pred get_context(term, context).
:- mode get_context(in, out) is det.

get_context(variable(_), Context) :-
	context_init(Context).
get_context(functor(_, _, Context), Context).

%------------------------------------------------------------------------------%

:- pred write_ind(int, io__state, io__state).
:- mode write_ind(in, di, uo) is det.

write_ind(N) -->
	( { N > 0 } ->
		write_string("    "),
		write_ind(N - 1)
	;
		[]
	).

:- pred dot_nl(io__state, io__state).
:- mode dot_nl(di, uo) is det.

dot_nl --> write_string(".\n").

:- pred write_term(lines, int, varset, term, io__state, io__state).
:- mode write_term(in, in, in, in, di, uo) is det.

write_term(lines, Ind, VarSet, Term) -->
	{ get_context(Term, context(File, Line)) },
	( { File = "", Line = 0 } ->
		[]
	;
		format("#%d\n", [i(Line)])
	),
	write_ind(Ind),
	write_term(VarSet, Term).
write_term(nolines, Ind, VarSet, Term) -->
	write_ind(Ind),
	write_term(VarSet, Term).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

term_to_type(functor(atom(Atom), Args, Context), Type) :-
	(
		Atom = "==",
		Args = [Head0, Body0]
	->
		Type = equiv(Head0, Body0)
	;
		Atom = "--->",
		Args = [Head1, Body1]
	->
		( term_to_disj(Body1, Terms0) ->
			Terms = Terms0
		;
			Terms = [Body1]
		),
		Type = disj(Head1, Terms)
	;
		Type = abstr(functor(atom(Atom), Args, Context))
	).

:- pred cons_type_body(list(term), term).
:- mode cons_type_body(in, out) is det.

cons_type_body([], _) :-
	error("cons_type_body: no disjuncts").
cons_type_body([E], E).
cons_type_body([E|Es], T) :-
	Es = [_|_],
	cons_type_body(Es, T0),
	get_context(E, Context),
	T = functor(atom(";"), [E, T0], Context).

%------------------------------------------------------------------------------%

term_to_goal(functor(atom(Atom), Args, Context), Goal) :-
	( term_to_goal0(Atom, Args, Context, Goal0) ->
		Goal = Goal0
	;
		Goal = call(functor(atom(Atom), Args, Context))
	).

:- pred term_to_goal0(string, list(term), context, goal).
:- mode term_to_goal0(in, in, in, out) is semidet.

term_to_goal0("true", [], _, conj([])).
term_to_goal0("fail", [], _, disj([])).

term_to_goal0(",", [A, B], _, conj([GoalA|Conj])) :-
	term_to_goal(A, GoalA),
	term_to_goal(B, GoalB),
	( GoalB = conj(Conj0) ->
		Conj = Conj0
	;
		Conj = [GoalB]
	).

term_to_goal0(";", [A, B], _, Goal) :-
	( A = functor(atom("->"), [IfTerm, ThenTerm], _) ->
		term_to_goal(IfTerm, If),
		term_to_goal(ThenTerm, Then),
		term_to_goal(B, Else),
		Goal = ite(If, Then, Else)
	;
		term_to_goal(A, GoalA),
		term_to_goal(B, GoalB),
		( GoalB = disj(Disj0) ->
			Goal = disj([GoalA|Disj0])
		;
			Goal = disj([GoalA, GoalB])
		)
	).

term_to_goal0("=", [A, B], Context, =(A, B, Context)).

term_to_goal0("not", [A], _, not(Goal)) :-
	term_to_goal(A, Goal).

term_to_goal0("\\+", [A], _, not(Goal)) :-
	term_to_goal(A, Goal).

term_to_goal0("some", [VarsTerm, GoalTerm], _, exists(Vars, Goal)) :-
	vars(VarsTerm, Vars0),
	sort_and_remove_dups(Vars0, Vars),
	term_to_goal(GoalTerm, Goal).

term_to_goal0("all", [VarsTerm, GoalTerm], _, forall(Vars, Goal)) :-
	vars(VarsTerm, Vars0),
	sort_and_remove_dups(Vars0, Vars),
	term_to_goal(GoalTerm, Goal).

/*
term_to_goal0("=>", [A, B], _, (GoalA => GoalB)) :-
	term_to_goal(A, GoalA),
	term_to_goal(B, GoalB).
*/

term_to_goal0("<=", [A, B], _, (GoalA <= GoalB)) :-
	term_to_goal(A, GoalA),
	term_to_goal(B, GoalB).

term_to_goal0("<=>", [A, B], _, (GoalA <=> GoalB)) :-
	term_to_goal(A, GoalA),
	term_to_goal(B, GoalB).

%------------------------------------------------------------------------------%

write_goal(VarSet, Goal) -->
	write_goal(nolines, 1, normal, Goal, VarSet).

:- pred write_goal(lines, int, goal_type, goal, varset, io__state, io__state).
:- mode write_goal(in, in, in, in, in, di, uo) is det.

write_goal(Lines, Ind, _GoalType, call(Term), VarSet) -->
	write_term(Lines, Ind, VarSet, Term).

write_goal(Lines, Ind, GoalType, =(LHS, RHS, Context), VarSet) -->
	{ UnifyTerm = functor(atom("="), [LHS, RHS], Context) },
	(
		{ GoalType = dcg },
		{ Term = functor(atom("{}"), [UnifyTerm], Context) }
	;
		{ GoalType = normal },
		{ Term = UnifyTerm }
	),
	write_term(Lines, Ind, VarSet, Term).

write_goal(Lines, Ind, GoalType, conj(Goals), VarSet) -->
	write_conj(Lines, Ind, GoalType, Goals, VarSet).

write_goal(Lines, Ind, GoalType, disj(Goals), VarSet) -->
	write_disj(Lines, Ind, GoalType, Goals, VarSet).

write_goal(Lines, Ind, GoalType, ite(If, Then, Else0), VarSet) -->
	{ collect_ite(Else0, IfThens0, Else) },
	write_ite(Lines, Ind, GoalType, [If - Then|IfThens0], Else, VarSet).

write_goal(Lines, Ind, GoalType, not(Goal), VarSet) -->
	write_ind(Ind),
	write_string("not (\n"),
	write_goal(Lines, Ind + 1, GoalType, Goal, VarSet), nl,
	write_ind(Ind),
	write_string(")").

write_goal(Lines, Ind, GoalType, exists(Vars, Goal), VarSet) -->
	write_ind(Ind),
	write_string("some ["),
	write_vars(Vars, VarSet),
	write_string("] (\n"),
	write_goal(Lines, Ind + 1, GoalType, Goal, VarSet), nl,
	write_ind(Ind),
	write_string(")").

write_goal(Lines, Ind, GoalType, forall(Vars, Goal), VarSet) -->
	write_ind(Ind),
	write_string("all ["),
	write_vars(Vars, VarSet),
	write_string("] (\n"),
	write_goal(Lines, Ind + 1, GoalType, Goal, VarSet), nl,
	write_ind(Ind),
	write_string(")").

/*
write_goal(Lines, Ind, GoalType, (A => B), VarSet) -->
	write_ind(Ind),
	write_string("((\n"),
	write_goal(Lines, Ind, GoalType, A, VarSet), nl,
	write_ind(Ind),
	write_string(") => (\n"),
	write_goal(Lines, Ind, GoalType, A, VarSet), nl,
	write_ind(Ind),
	write_string("))").
*/

write_goal(Lines, Ind, GoalType, (A <= B), VarSet) -->
	write_ind(Ind),
	write_string("((\n"),
	write_goal(Lines, Ind, GoalType, A, VarSet), nl,
	write_ind(Ind),
	write_string(") <= (\n"),
	write_goal(Lines, Ind, GoalType, B, VarSet), nl,
	write_ind(Ind),
	write_string("))").

write_goal(Lines, Ind, GoalType, (A <=> B), VarSet) -->
	write_ind(Ind),
	write_string("((\n"),
	write_goal(Lines, Ind, GoalType, A, VarSet), nl,
	write_ind(Ind),
	write_string(") <=> (\n"),
	write_goal(Lines, Ind, GoalType, B, VarSet), nl,
	write_ind(Ind),
	write_string("))").

%------------------------------------------------------------------------------%

:- pred write_conj(lines, int, goal_type, list(goal), varset,
		io__state, io__state).
:- mode write_conj(in, in, in, in, in, di, uo) is det.

write_conj(_Lines, Ind, Type, [], _VarSet) -->
	write_ind(Ind),
	(
		{ Type = normal },
		write_string("true")
	;
		{ Type = dcg },
		write_string("{ true }")
	).

write_conj(Lines, Ind, Type, [Goal], VarSet) -->
	write_goal(Lines, Ind, Type, Goal, VarSet).

write_conj(Lines, Ind, Type, [Goal|Goals], VarSet) -->
	{ Goals = [_|_] },
	write_goal(Lines, Ind, Type, Goal, VarSet),
	write_string(",\n"),
	write_conj(Lines, Ind, Type, Goals, VarSet).

%------------------------------------------------------------------------------%

:- pred write_disj(lines, int, goal_type, list(goal), varset,
		io__state, io__state).
:- mode write_disj(in, in, in, in, in, di, uo) is det.

write_disj(Lines, Ind, Type, Goals, VarSet) -->
	write_ind(Ind),
	write_string("(\n"),
	write_disj0(Lines, Ind, Type, Goals, VarSet), nl,
	write_ind(Ind),
	write_string(")").

:- pred write_disj0(lines, int, goal_type, list(goal), varset,
		io__state, io__state).
:- mode write_disj0(in, in, in, in, in, di, uo) is det.

write_disj0(_Lines, Ind, Type, [], _VarSet) -->
	write_ind(Ind + 1),
	(
		{ Type = normal },
		write_string("fail")
	;
		{ Type = dcg },
		write_string("{ fail }")
	).

write_disj0(Lines, Ind, Type, [Goal], VarSet) -->
	write_goal(Lines, Ind + 1, Type, Goal, VarSet), nl.

write_disj0(Lines, Ind, Type, [Goal|Goals], VarSet) -->
	{ Goals = [_|_] },
	write_goal(Lines, Ind + 1, Type, Goal, VarSet), nl,
	write_ind(Ind),
	write_string(";\n"),
	write_disj0(Lines, Ind, Type, Goals, VarSet).

%------------------------------------------------------------------------------%

:- pred collect_ite(goal, list(pair(goal)), goal).
:- mode collect_ite(in, out, out) is det.

collect_ite(Goal0, IfThens, Else) :-
	( Goal0 = ite(If, Then, Else0) ->
		IfThens = [If - Then|IfThens0],
		collect_ite(Else0, IfThens0, Else)
	;
		IfThens = [],
		Else = Goal0
	).

:- pred write_ite(lines, int, goal_type, list(pair(goal)), goal, varset,
		io__state, io__state).
:- mode write_ite(in, in, in, in, in, in, di, uo) is det.

write_ite(Lines, Ind, Type, IfThens, Else, VarSet) -->
	write_ind(Ind),
	write_string("(\n"),
	write_ite0(Lines, Ind, Type, IfThens, VarSet),
	write_ind(Ind),
	write_string(";\n"),
	write_goal(Lines, Ind + 1, Type, Else, VarSet), nl,
	write_ind(Ind),
	write_string(")").

:- pred write_ite0(lines, int, goal_type, list(pair(goal)), varset,
		io__state, io__state).
:- mode write_ite0(in, in, in, in, in, di, uo) is det.

write_ite0(_Lines, _Ind, _Type, [], _VarSet) -->
	{ error("no if-thens") }.
write_ite0(Lines, Ind, Type, [If - Then], VarSet) -->
	write_goal(Lines, Ind + 1, Type, If, VarSet), nl,
	write_ind(Ind),
	write_string("->\n"),
	write_goal(Lines, Ind + 1, Type, Then, VarSet), nl.
write_ite0(Lines, Ind, Type, [If - Then|Rest], VarSet) -->
	{ Rest = [_|_] },
	write_goal(Lines, Ind + 1, Type, If, VarSet), nl,
	write_ind(Ind),
	write_string("->\n"),
	write_goal(Lines, Ind + 1, Type, Then, VarSet), nl,
	write_ind(Ind),
	write_string(";\n"),
	write_ite0(Lines, Ind, Type, Rest, VarSet).

%------------------------------------------------------------------------------%

:- pred write_vars(vars, varset, io__state, io__state).
:- mode write_vars(in, in, di, uo) is det.

write_vars([], _) --> [].
write_vars([V], VarSet) -->
	write_variable(V, VarSet).
write_vars([V|Vs], VarSet) -->
	{ Vs = [_|_] },
	write_variable(V, VarSet),
	write_string(", "),
	write_vars(Vs, VarSet).

