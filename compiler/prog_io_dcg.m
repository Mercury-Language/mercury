%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2001, 2003-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_dcg.m.
% Main authors: fjh, zs.
%
% This module handles the parsing of clauses in Definite Clause Grammar
% notation.
%
% XXX This module performs no error checking.
% XXX It may be an idea to recode this as a state variable transformation:
% 	roughly		Head --> G1, G2, {G3}, G4.
% 	becomes		Head(!DCG) :- G1(!DCG), G2(!DCG), G3, G4(!DCG).

:- module parse_tree__prog_io_dcg.

:- interface.

:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_io_util.
:- import_module varset, term.

:- pred parse_dcg_clause(module_name::in, varset::in, term::in, term::in,
	prog_context::in, maybe_item_and_context::out) is det.

	% parse_dcg_pred_goal(GoalTerm, Goal,
	%	DCGVarInitial, DCGVarFinal, VarSet0, Varset)
	% parses `GoalTerm' and expands it as a DCG goal,
	% `VarSet0' is the initial varset, and `VarSet' is
	% the final varset. `DCGVarInitial' is the first DCG variable,
	% and `DCGVarFinal' is the final DCG variable.
:- pred parse_dcg_pred_goal(term::in, goal::out, prog_var::out, prog_var::out,
	prog_varset::in, prog_varset::out) is det.

:- implementation.

:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_io_goal.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_out.

:- import_module int, map, string, std_util, list, counter.

%-----------------------------------------------------------------------------%

parse_dcg_clause(ModuleName, VarSet0, DCG_Head, DCG_Body, DCG_Context,
		Result) :-
	varset__coerce(VarSet0, ProgVarSet0),
	new_dcg_var(ProgVarSet0, ProgVarSet1, counter__init(0), Counter0,
		DCG_0_Var),
	parse_dcg_goal(DCG_Body, Body, ProgVarSet1, ProgVarSet,
		Counter0, _Counter, DCG_0_Var, DCG_Var),
	parse_implicitly_qualified_term(ModuleName,
		DCG_Head, DCG_Body, "DCG clause head", HeadResult),
	process_dcg_clause(HeadResult, ProgVarSet, DCG_0_Var, DCG_Var,
		Body, R),
	add_context(R, DCG_Context, Result).

%-----------------------------------------------------------------------------%

parse_dcg_pred_goal(GoalTerm, Goal, DCGVar0, DCGVar, !VarSet) :-
	new_dcg_var(!VarSet, counter__init(0), Counter0, DCGVar0),
	parse_dcg_goal(GoalTerm, Goal, !VarSet, Counter0, _Counter,
		DCGVar0, DCGVar).

%-----------------------------------------------------------------------------%

	% Used to allocate fresh variables needed for the DCG expansion.

:- pred new_dcg_var(prog_varset::in, prog_varset::out,
	counter::in, counter::out, prog_var::out) is det.

new_dcg_var(!VarSet, !Counter, DCG_0_Var) :-
	counter__allocate(N, !Counter),
	string__int_to_string(N, StringN),
	string__append("DCG_", StringN, VarName),
	varset__new_var(!.VarSet, DCG_0_Var, !:VarSet),
	varset__name_var(!.VarSet, DCG_0_Var, VarName, !:VarSet).

%-----------------------------------------------------------------------------%

	% Expand a DCG goal.

:- pred parse_dcg_goal(term::in, goal::out, prog_varset::in, prog_varset::out,
	counter::in, counter::out, prog_var::in, prog_var::out) is det.

parse_dcg_goal(Term, Goal, !VarSet, !Counter, !Var) :-
	% first, figure out the context for the goal
	(
		Term = term__functor(_, _, Context)
	;
		Term = term__variable(_),
		term__context_init(Context)
	),
	% next, parse it
	(
		term__coerce(Term, ProgTerm),
		sym_name_and_args(ProgTerm, SymName, Args0)
	->
		% First check for the special cases:
		(
			SymName = unqualified(Functor),
			list__map(term__coerce, Args0, Args1),
			parse_dcg_goal_2(Functor, Args1, Context,
				Goal1, !VarSet, !Counter, !Var)
		->
			Goal = Goal1
		;
			% It's the ordinary case of non-terminal.
			% Create a fresh var as the DCG output var from this
			% goal, and append the DCG argument pair to the
			% non-terminal's argument list.
			new_dcg_var(!VarSet, !Counter, Var),
			list__append(Args0,
				[term__variable(!.Var),
					term__variable(Var)], Args),
			Goal = call(SymName, Args, pure) - Context,
			!:Var = Var
		)
	;
		% A call to a free variable, or to a number or string.
		% Just translate it into a call to call/3 - the typechecker
		% will catch calls to numbers and strings.
		new_dcg_var(!VarSet, !Counter, Var),
		term__coerce(Term, ProgTerm),
		Goal = call(unqualified("call"), [ProgTerm,
			term__variable(!.Var), term__variable(Var)],
			pure) - Context,
		!:Var = Var
	).

	% parse_dcg_goal_2(Functor, Args, Context, VarSet0, Counter0, Var0,
	%			Goal, VarSet, Counter, Var):
	% VarSet0/VarSet are an accumulator pair which we use to
	% allocate fresh DCG variables; Counter0 and Counter are a pair
	% we use to keep track of the number to give to the next DCG
	% variable (so that we can give it a semi-meaningful name "DCG_<N>"
	% for use in error messages, debugging, etc.).
	% Var0 and Var are an accumulator pair we use to keep track of
	% the current DCG variable.
	%
	% Since (A -> B) has different semantics in standard Prolog
	% (A -> B ; fail) than it does in NU-Prolog or Mercury (A -> B ; true),
	% for the moment we'll just disallow it.

:- pred parse_dcg_goal_2(string::in, list(term)::in, prog_context::in,
	goal::out, prog_varset::in, prog_varset::out,
	counter::in, counter::out, prog_var::in, prog_var::out) is semidet.

	% Ordinary goal inside { curly braces }.
parse_dcg_goal_2("{}", [G0 | Gs], Context, Goal, !VarSet, !Counter, !Var) :-
	% The parser treats '{}/N' terms as tuples, so we need
	% to undo the parsing of the argument conjunction here.
	list_to_conjunction(Context, G0, Gs, G),
	parse_goal(G, Goal, !VarSet).
parse_dcg_goal_2("impure", [G], _, Goal, !VarSet, !Counter, !Var) :-
	parse_dcg_goal_with_purity(G, (impure), Goal, !VarSet, !Counter, !Var).
parse_dcg_goal_2("semipure", [G], _, Goal, !VarSet, !Counter, !Var) :-
	parse_dcg_goal_with_purity(G, (semipure), Goal, !VarSet, !Counter,
		!Var).

	% Empty list - just unify the input and output DCG args.
parse_dcg_goal_2("[]", [], Context, Goal, !VarSet, !Counter, Var0, Var) :-
	new_dcg_var(!VarSet, !Counter, Var),
	Goal = unify(term__variable(Var0), term__variable(Var), pure)
		- Context.

	% Non-empty list of terminals.  Append the DCG output arg
	% as the new tail of the list, and unify the result with
	% the DCG input arg.
parse_dcg_goal_2("[|]", [X, Xs], Context, Goal, !VarSet, !Counter,
		Var0, Var) :-
	new_dcg_var(!VarSet, !Counter, Var),
	ConsTerm0 = term__functor(term__atom("[|]"), [X, Xs], Context),
	term__coerce(ConsTerm0, ConsTerm),
	term_list_append_term(ConsTerm, term__variable(Var), Term),
	Goal = unify(term__variable(Var0), Term, pure) - Context.

	% Call to '='/1 - unify argument with DCG input arg.
parse_dcg_goal_2("=", [A0], Context, Goal, !VarSet, !Counter, Var, Var) :-
	term__coerce(A0, A),
	Goal = unify(A, term__variable(Var), pure) - Context.

	% Call to ':='/1 - unify argument with DCG output arg.
parse_dcg_goal_2(":=", [A0], Context, Goal, !VarSet, !Counter, _Var0, Var) :-
	new_dcg_var(!VarSet, !Counter, Var),
	term__coerce(A0, A),
	Goal = unify(A, term__variable(Var), pure) - Context.

	% If-then (Prolog syntax).
	% We need to add an else part to unify the DCG args.

% /******
% parse_dcg_goal_2("->", [Cond0, Then0], Context, VarSet0, Counter0, Var0,
% 		Goal, VarSet, Counter, Var) :-
% 	parse_dcg_if_then(Cond0, Then0, Context, VarSet0, Counter0, Var0,
% 		SomeVars, StateVars, Cond, Then, VarSet, Counter, Var),
% 	( Var = Var0 ->
% 		Goal = if_then(SomeVars, StateVars, Cond, Then) - Context
% 	;
% 		Unify = unify(term__variable(Var), term__variable(Var0)),
% 		Goal = if_then_else(SomeVars, StateVars, Cond, Then,
% 					Unify - Context) - Context
% 	).
% ******/

	% If-then (NU-Prolog syntax).
parse_dcg_goal_2("if", [term__functor(term__atom("then"), [Cond0, Then0], _)],
		Context, Goal, !VarSet, !Counter, Var0, Var) :-
	parse_dcg_if_then(Cond0, Then0, Context, SomeVars, StateVars,
		Cond, Then, !VarSet, !Counter, Var0, Var),
	( Var = Var0 ->
		Goal = if_then(SomeVars, StateVars, Cond, Then) - Context
	;
		Unify = unify(term__variable(Var), term__variable(Var0), pure),
		Goal = if_then_else(SomeVars, StateVars, Cond, Then,
			Unify - Context) - Context
	).

	% Conjunction.
parse_dcg_goal_2(",", [A0, B0], Context, (A, B) - Context, !VarSet, !Counter,
		!Var) :-
	parse_dcg_goal(A0, A, !VarSet, !Counter, !Var),
	parse_dcg_goal(B0, B, !VarSet, !Counter, !Var).

parse_dcg_goal_2("&", [A0, B0], Context, (A & B) - Context,
		!VarSet, !Counter, !Var) :-
	parse_dcg_goal(A0, A, !VarSet, !Counter, !Var),
	parse_dcg_goal(B0, B, !VarSet, !Counter, !Var).

	% Disjunction or if-then-else (Prolog syntax).
parse_dcg_goal_2(";", [A0, B0], Context, Goal, !VarSet, !Counter, Var0, Var) :-
	(
		A0 = term__functor(term__atom("->"), [Cond0, Then0], _Context)
	->
		parse_dcg_if_then_else(Cond0, Then0, B0, Context, Goal,
			!VarSet, !Counter, Var0, Var)
	;
		parse_dcg_goal(A0, A1, !VarSet, !Counter, Var0, VarA),
		parse_dcg_goal(B0, B1, !VarSet, !Counter, Var0, VarB),
		( VarA = Var0, VarB = Var0 ->
			Var = Var0,
			Goal = (A1 ; B1) - Context
		; VarA = Var0 ->
			Var = VarB,
			Unify = unify(term__variable(Var),
				term__variable(VarA), pure),
			append_to_disjunct(A1, Unify, Context, A2),
			Goal = (A2 ; B1) - Context
		; VarB = Var0 ->
			Var = VarA,
			Unify = unify(term__variable(Var),
				term__variable(VarB), pure),
			append_to_disjunct(B1, Unify, Context, B2),
			Goal = (A1 ; B2) - Context
		;
			Var = VarB,
			prog_util__rename_in_goal(VarA, VarB, A1, A2),
			Goal = (A2 ; B1) - Context
		)
	).

	% If-then-else (NU-Prolog syntax).
parse_dcg_goal_2("else", [IF, Else0], _, Goal, !VarSet, !Counter, !Var) :-
	IF = term__functor(term__atom("if"),
		[term__functor(term__atom("then"), [Cond0, Then0], _)],
		Context),
	parse_dcg_if_then_else(Cond0, Then0, Else0, Context, Goal,
		!VarSet, !Counter, !Var).

	% Negation (NU-Prolog syntax).
parse_dcg_goal_2("not", [A0], Context, not(A) - Context,
		!VarSet, !Counter, Var0, Var0) :-
	parse_dcg_goal(A0, A, !VarSet, !Counter, Var0, _).

	% Negation (Prolog syntax).
parse_dcg_goal_2("\\+", [A0], Context, not(A) - Context,
		!VarSet, !Counter, Var0, Var0) :-
	parse_dcg_goal(A0, A, !VarSet, !Counter, Var0, _).

	% Universal quantification.
parse_dcg_goal_2("all", [QVars, A0], Context, GoalExpr - Context,
		!VarSet, !Counter, !Var) :-

		% Extract any state variables in the quantifier.
		%
	parse_quantifier_vars(QVars, StateVars0, Vars0),
	list__map(term__coerce_var, StateVars0, StateVars),
	list__map(term__coerce_var, Vars0, Vars),

	parse_dcg_goal(A0, A @ (GoalExprA - ContextA), !VarSet, !Counter,
		!Var),

	(
		Vars = [],    StateVars = [],
		GoalExpr = GoalExprA
	;
		Vars = [],    StateVars = [_|_],
		GoalExpr = all_state_vars(StateVars, A)
	;
		Vars = [_|_], StateVars = [],
		GoalExpr = all(Vars, A)
	;
		Vars = [_|_], StateVars = [_|_],
		GoalExpr = all(Vars, all_state_vars(StateVars, A) - ContextA)
	).

	% Existential quantification.
parse_dcg_goal_2("some", [QVars, A0], Context, GoalExpr - Context,
		!VarSet, !Counter, !Var) :-

		% Extract any state variables in the quantifier.
		%
	parse_quantifier_vars(QVars, StateVars0, Vars0),
	list__map(term__coerce_var, StateVars0, StateVars),
	list__map(term__coerce_var, Vars0, Vars),

	parse_dcg_goal(A0, A @ (GoalExprA - ContextA), !VarSet, !Counter,
		!Var),

	(
		Vars = [],    StateVars = [],
		GoalExpr = GoalExprA
	;
		Vars = [],    StateVars = [_|_],
		GoalExpr = some_state_vars(StateVars, A)
	;
		Vars = [_|_], StateVars = [],
		GoalExpr = some(Vars, A)
	;
		Vars = [_|_], StateVars = [_|_],
		GoalExpr = some(Vars, some_state_vars(StateVars, A) - ContextA)
	).

:- pred parse_dcg_goal_with_purity(term::in, purity::in, goal::out,
	prog_varset::in, prog_varset::out, counter::in, counter::out,
	prog_var::in, prog_var::out) is det.

parse_dcg_goal_with_purity(G, Purity, Goal, !VarSet, !Counter, !Var) :-
	parse_dcg_goal(G, Goal1, !VarSet, !Counter, !Var),
	( Goal1 = call(Pred, Args, pure) - Context ->
		Goal = call(Pred, Args, Purity) - Context
	; Goal1 = unify(ProgTerm1, ProgTerm2, pure) - Context ->
		Goal = unify(ProgTerm1, ProgTerm2, Purity) - Context
	;
		% Inappropriate placement of an impurity marker, so we treat
		% it like a predicate call.  typecheck.m prints out something
		% descriptive for these errors.
		Goal1 = _ - Context,
		purity_name(Purity, PurityString),
		term__coerce(G, G1),
		Goal = call(unqualified(PurityString), [G1], pure) - Context
	).

:- pred append_to_disjunct(goal::in, goal_expr::in, prog_context::in,
	goal::out) is det.

append_to_disjunct(Disjunct0, Goal, Context, Disjunct) :-
	( Disjunct0 = (A0 ; B0) - Context2 ->
		append_to_disjunct(A0, Goal, Context, A),
		append_to_disjunct(B0, Goal, Context, B),
		Disjunct = (A ; B) - Context2
	;
		Disjunct = (Disjunct0, Goal - Context) - Context
	).

:- pred parse_some_vars_dcg_goal(term::in, list(prog_var)::out,
	list(prog_var)::out, goal::out, prog_varset::in, prog_varset::out,
	counter::in, counter::out, prog_var::in, prog_var::out) is det.

parse_some_vars_dcg_goal(A0, SomeVars, StateVars, A, !VarSet, !Counter,
		!Var) :-
	( A0 = term__functor(term__atom("some"), [QVars0, A1], _Context) ->
		term__coerce(QVars0, QVars),
		( parse_quantifier_vars(QVars, StateVars0, SomeVars0) ->
			SomeVars = SomeVars0,
			StateVars = StateVars0
		;
				% XXX a hack because we do not do
				% error checking in this module.
			term__vars(QVars, SomeVars),
			StateVars = []
		),
		A2 = A1
	;
		SomeVars = [],
		StateVars = [],
		A2 = A0
	),
	parse_dcg_goal(A2, A, !VarSet, !Counter, !Var).

	% Parse the "if" and the "then" part of an if-then or an
	% if-then-else.
	% If the condition is a DCG goal, but then "then" part
	% is not, then we need to translate
	%	( a -> { b } ; c )
	% as
	%	( a(DCG_1, DCG_2) ->
	%		b,
	%		DCG_3 = DCG_2
	%	;
	%		c(DCG_1, DCG_3)
	%	)
	% rather than
	%	( a(DCG_1, DCG_2) ->
	%		b
	%	;
	%		c(DCG_1, DCG_2)
	%	)
	% so that the implicit quantification of DCG_2 is correct.

:- pred parse_dcg_if_then(term::in, term::in, prog_context::in,
	list(prog_var)::out, list(prog_var)::out, goal::out, goal::out,
	prog_varset::in, prog_varset::out, counter::in, counter::out,
	prog_var::in, prog_var::out) is det.

parse_dcg_if_then(Cond0, Then0, Context, SomeVars, StateVars, Cond, Then,
		!VarSet, !Counter, Var0, Var) :-
	parse_some_vars_dcg_goal(Cond0, SomeVars, StateVars, Cond,
		!VarSet, !Counter, Var0, Var1),
	parse_dcg_goal(Then0, Then1, !VarSet, !Counter, Var1, Var2),
	( Var0 \= Var1, Var1 = Var2 ->
		new_dcg_var(!VarSet, !Counter, Var),
		Unify = unify(term__variable(Var), term__variable(Var2), pure),
		Then = (Then1, Unify - Context) - Context
	;
		Then = Then1,
		Var = Var2
	).

:- pred parse_dcg_if_then_else(term::in, term::in, term::in, prog_context::in,
	goal::out, prog_varset::in, prog_varset::out,
	counter::in, counter::out, prog_var::in, prog_var::out) is det.

parse_dcg_if_then_else(Cond0, Then0, Else0, Context, Goal,
		!VarSet, !Counter, Var0, Var) :-
	parse_dcg_if_then(Cond0, Then0, Context, SomeVars, StateVars,
		Cond, Then1, !VarSet, !Counter, Var0, VarThen),
	parse_dcg_goal(Else0, Else1, !VarSet, !Counter, Var0, VarElse),
	( VarThen = Var0, VarElse = Var0 ->
		Var = Var0,
		Then = Then1,
		Else = Else1
	; VarThen = Var0 ->
		Var = VarElse,
		Unify = unify(term__variable(Var), term__variable(VarThen),
			pure),
		Then = (Then1, Unify - Context) - Context,
		Else = Else1
	; VarElse = Var0 ->
		Var = VarThen,
		Then = Then1,
		Unify = unify(term__variable(Var), term__variable(VarElse),
			pure),
		Else = (Else1, Unify - Context) - Context
	;
		% We prefer to substitute the then part since it is likely
		% to be smaller than the else part, since the else part may
		% have a deeply nested chain of if-then-elses.

		% parse_dcg_if_then guarantees that if VarThen \= Var0,
		% then the then part introduces a new DCG variable (i.e.
		% VarThen does not appear in the condition). We therefore
		% don't need to do the substitution in the condition.

		Var = VarElse,
		prog_util__rename_in_goal(VarThen, VarElse, Then1, Then),
		Else = Else1
	),
	Goal = if_then_else(SomeVars, StateVars, Cond, Then, Else) - Context.

	% term_list_append_term(ListTerm, Term, Result):
	% 	if ListTerm is a term representing a proper list,
	%	this predicate will append the term Term
	%	onto the end of the list

:- pred term_list_append_term(term(T)::in, term(T)::in, term(T)::out)
	is semidet.

term_list_append_term(List0, Term, List) :-
	( List0 = term__functor(term__atom("[]"), [], _Context) ->
		List = Term
	;
		List0 = term__functor(term__atom("[|]"),
			[Head, Tail0], Context2),
		List = term__functor(term__atom("[|]"),
			[Head, Tail], Context2),
		term_list_append_term(Tail0, Term, Tail)
	).

:- pred process_dcg_clause(maybe_functor::in, prog_varset::in, prog_var::in,
	prog_var::in, goal::in, maybe1(item)::out) is det.

process_dcg_clause(ok(Name, Args0), VarSet, Var0, Var, Body,
		ok(clause(VarSet, predicate, Name, Args, Body))) :-
	list__map(term__coerce, Args0, Args1),
	list__append(Args1, [term__variable(Var0), term__variable(Var)], Args).
process_dcg_clause(error(Message, Term), _, _, _, _, error(Message, Term)).
