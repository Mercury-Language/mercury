%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_goal.m.
% Main author: fjh.
%
% This module defines the predicates that parse goals.

:- module prog_io_goal.

:- interface.

:- import_module prog_data, hlds_data.
:- import_module list, term.

	% Convert a single term into a goal.
	%
:- pred parse_goal(term, prog_varset, goal, prog_varset).
:- mode parse_goal(in, in, out, out) is det.

	% Convert a term, possibly starting with `some [Vars]', into
	% a list of variables and a goal. (If the term doesn't start
	% with `some [Vars]', we return an empty list of variables.)
	% 
:- pred parse_some_vars_goal(term, prog_varset, list(prog_var),
		goal, prog_varset).
:- mode parse_some_vars_goal(in, in, out, out, out) is det.

	% parse_lambda_expression/3 converts the first argument to a lambda/2
	% expression into a list of variables, a list of their corresponding
	% modes, and a determinism.
	% The syntax of a lambda expression is
	%	`lambda([Var1::Mode1, ..., VarN::ModeN] is Det, Goal)'
	% but this predicate just parses the first argument, i.e. the
	% 	`[Var1::Mode1, ..., VarN::ModeN] is Det'
	% part.
	%
:- pred parse_lambda_expression(term, list(prog_term), list(mode),
		determinism).
:- mode parse_lambda_expression(in, out, out, out) is semidet.

	% parse_pred_expression/3 converts the first argument to a :-/2
	% higher-order pred expression into a list of variables, a list
	% of their corresponding modes, and a determinism.  This is just
	% a variant on parse_lambda_expression with a different syntax:
	% 	`(pred(Var1::Mode1, ..., VarN::ModeN) is Det :- Goal)'.
	%
:- pred parse_pred_expression(term, list(prog_term), list(mode),
		determinism).
:- mode parse_pred_expression(in, out, out, out) is semidet.

	% parse_dcg_pred_expression/3 converts the first argument to a -->/2
	% higher-order dcg pred expression into a list of variables, a list
	% of their corresponding modes and the two dcg argument modes, and a
	% determinism.
	% This is a variant of the higher-order pred syntax:
	%	`(pred(Var1::Mode1, ..., VarN::ModeN, DCG0Mode, DCGMode)
	%		is Det --> Goal)'.
	%
:- pred parse_dcg_pred_expression(term, list(prog_term),
		list(mode), determinism).
:- mode parse_dcg_pred_expression(in, out, out, out) is semidet.

	% parse_func_expression/3 converts the first argument to a :-/2
	% higher-order func expression into a list of variables, a list
	% of their corresponding modes, and a determinism.  The syntax
	% of a higher-order func expression is
	% 	`(func(Var1::Mode1, ..., VarN::ModeN) = (VarN1::ModeN1) is Det
	%		:- Goal)'.
	%
:- pred parse_func_expression(term, list(prog_term), list(mode),
		determinism).
:- mode parse_func_expression(in, out, out, out) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mode_util, purity, prog_io, prog_io_util, term_util.
:- import_module term.
:- import_module int, map, string, std_util.

	% Parse a goal.
	%
	% We could do some error-checking here, but all errors are picked up
	% in either the type-checker or parser anyway.

parse_goal(Term, VarSet0, Goal, VarSet) :-
	% first, get the goal context
	(
		Term = term__functor(_, _, Context)
	;
		Term = term__variable(_),
		term__context_init(Context)
	),
	% We just check if it matches the appropriate pattern
	% for one of the builtins.  If it doesn't match any of the
	% builtins, then it's just a predicate call.
	(
		% check for builtins...
		Term = term__functor(term__atom(Name), Args, Context),
		parse_goal_2(Name, Args, VarSet0, GoalExpr, VarSet1)
	->
		Goal = GoalExpr - Context,
		VarSet = VarSet1
	;
		% it's not a builtin
		term__coerce(Term, ArgsTerm),
		(
			% check for predicate calls
			sym_name_and_args(ArgsTerm, SymName, Args)
		->
			VarSet = VarSet0,
			Goal = call(SymName, Args, pure) - Context
		;
		% A call to a free variable, or to a number or string.
		% Just translate it into a call to call/1 - the typechecker
		% will catch calls to numbers and strings.
			Goal = call(unqualified("call"), [ArgsTerm], pure)
					- Context,
			VarSet = VarSet0
		)
	).

%-----------------------------------------------------------------------------%

:- pred parse_goal_2(string, list(term), prog_varset, goal_expr, prog_varset).
:- mode parse_goal_2(in, in, in, out, out) is semidet.
parse_goal_2("true", [], V, true, V).
parse_goal_2("fail", [], V, fail, V).
parse_goal_2("=", [A0, B0], V, unify(A, B), V) :-
	term__coerce(A0, A),
	term__coerce(B0, B).
/******
	Since (A -> B) has different semantics in standard Prolog
	(A -> B ; fail) than it does in NU-Prolog or Mercury (A -> B ; true),
	for the moment we'll just disallow it.
parse_goal_2("->", [A0, B0], V0, if_then(Vars, A, B), V) :-
	parse_some_vars_goal(A0, V0, Vars, A, V1),
	parse_goal(B0, V1, B, V).
******/
parse_goal_2(",", [A0, B0], V0, (A, B), V) :-
	parse_goal(A0, V0, A, V1),
	parse_goal(B0, V1, B, V).
parse_goal_2("&", [A0, B0], V0, (A & B), V) :-
	parse_goal(A0, V0, A, V1),
	parse_goal(B0, V1, B, V).
parse_goal_2(";", [A0, B0], V0, R, V) :-
	(
		A0 = term__functor(term__atom("->"), [X0, Y0], _Context)
	->
		parse_some_vars_goal(X0, V0, Vars, X, V1),
		parse_goal(Y0, V1, Y, V2),
		parse_goal(B0, V2, B, V),
		R = if_then_else(Vars, X, Y, B)
	;
		parse_goal(A0, V0, A, V1),
		parse_goal(B0, V1, B, V),
		R = (A;B)
	).
/****
	For consistency we also disallow if-then
parse_goal_2("if",
		[term__functor(term__atom("then"), [A0, B0], _)], V0,
		if_then(Vars, A, B), V) :-
	parse_some_vars_goal(A0, V0, Vars, A, V1),
	parse_goal(B0, V1, B, V).
****/
parse_goal_2("else", [
		    term__functor(term__atom("if"), [
			term__functor(term__atom("then"), [A0, B0], _)
		    ], _),
		    C0
		], V0,
		if_then_else(Vars, A, B, C), V) :-
	parse_some_vars_goal(A0, V0, Vars, A, V1),
	parse_goal(B0, V1, B, V2),
	parse_goal(C0, V2, C, V).
parse_goal_2("not", [A0], V0, not(A), V) :-
	parse_goal(A0, V0, A, V).
parse_goal_2("\\+", [A0], V0, not(A), V) :-
	parse_goal(A0, V0, A, V).
parse_goal_2("all", [Vars0, A0], V0, all(Vars, A), V):-
	term__coerce(Vars0, Vars1),
	term__vars(Vars1, Vars),
	parse_goal(A0, V0, A, V).

	% handle implication
parse_goal_2("<=", [A0, B0], V0, implies(B, A), V):-
	parse_goal(A0, V0, A, V1),
	parse_goal(B0, V1, B, V).

parse_goal_2("=>", [A0, B0], V0, implies(A, B), V):-
	parse_goal(A0, V0, A, V1),
	parse_goal(B0, V1, B, V).

	% handle equivalence
parse_goal_2("<=>", [A0, B0], V0, equivalent(A, B), V):-
	parse_goal(A0, V0, A, V1),
	parse_goal(B0, V1, B, V).

parse_goal_2("some", [Vars0, A0], V0, some(Vars, A), V):-
	parse_list_of_vars(Vars0, Vars1),
	list__map(term__coerce_var, Vars1, Vars),
	parse_goal(A0, V0, A, V).

	% The following is a temporary hack to handle `is' in
	% the parser - we ought to handle it in the code generation -
	% but then `is/2' itself is a bit of a hack
	%
parse_goal_2("is", [A0, B0], V, unify(A, B), V) :-
	term__coerce(A0, A),
	term__coerce(B0, B).
parse_goal_2("impure", [A0], V0, A, V) :-
	parse_goal_with_purity(A0, V0, (impure), A, V).
parse_goal_2("semipure", [A0], V0, A, V) :-
	parse_goal_with_purity(A0, V0, (semipure), A, V).


:- pred parse_goal_with_purity(term, prog_varset, purity, goal_expr,
		prog_varset).
:- mode parse_goal_with_purity(in, in, in, out, out) is det.

parse_goal_with_purity(A0, V0, Purity, A, V) :-
	parse_goal(A0, V0, A1, V),
	(   A1 = call(Pred, Args, pure) - _ ->
		A = call(Pred, Args, Purity)
	;
		% Inappropriate placement of an impurity marker, so we treat
		% it like a predicate call.  typecheck.m prints out something
		% descriptive for these errors.
		purity_name(Purity, PurityString),
		term__coerce(A0, A2),
		A = call(unqualified(PurityString), [A2], pure)
	).


%-----------------------------------------------------------------------------%

parse_some_vars_goal(A0, VarSet0, Vars, A, VarSet) :-
	( 
		A0 = term__functor(term__atom("some"), [Vars0, A1], _Context),
		parse_list_of_vars(Vars0, Vars1)
	->
		list__map(term__coerce_var, Vars1, Vars),
		parse_goal(A1, VarSet0, A, VarSet)
	;
		Vars = [],
		parse_goal(A0, VarSet0, A, VarSet)
	).

%-----------------------------------------------------------------------------%

parse_lambda_expression(LambdaExpressionTerm, Vars, Modes, Det) :-
	LambdaExpressionTerm = term__functor(term__atom("is"),
				[LambdaArgsTerm, DetTerm], _),
	DetTerm = term__functor(term__atom(DetString), [], _),
	standard_det(DetString, Det),
	parse_lambda_args(LambdaArgsTerm, Vars, Modes).

:- pred parse_lambda_args(term, list(prog_term), list(mode)).
:- mode parse_lambda_args(in, out, out) is semidet.

parse_lambda_args(Term, Vars, Modes) :-
	( Term = term__functor(term__atom("."), [Head, Tail], _Context) ->
		parse_lambda_arg(Head, Var, Mode),
		Vars = [Var | Vars1],
		Modes = [Mode | Modes1],
		parse_lambda_args(Tail, Vars1, Modes1)
	; Term = term__functor(term__atom("[]"), [], _) ->
		Vars = [],
		Modes = []
	;
		Vars = [Var],
		Modes = [Mode],
		parse_lambda_arg(Term, Var, Mode)
	).

:- pred parse_lambda_arg(term, prog_term, mode).
:- mode parse_lambda_arg(in, out, out) is semidet.

parse_lambda_arg(Term, VarTerm, Mode) :-
	Term = term__functor(term__atom("::"), [VarTerm0, ModeTerm], _),
	term__coerce(VarTerm0, VarTerm),
	convert_mode(ModeTerm, Mode).

%-----------------------------------------------------------------------------%

parse_pred_expression(PredTerm, Vars, Modes, Det) :-
	PredTerm = term__functor(term__atom("is"), [PredArgsTerm, DetTerm], _),
	DetTerm = term__functor(term__atom(DetString), [], _),
	standard_det(DetString, Det),
	PredArgsTerm = term__functor(term__atom("pred"), PredArgsList, _),
	parse_pred_expr_args(PredArgsList, Vars, Modes).

parse_dcg_pred_expression(PredTerm, Vars, Modes, Det) :-
	PredTerm = term__functor(term__atom("is"), [PredArgsTerm, DetTerm], _),
	DetTerm = term__functor(term__atom(DetString), [], _),
	standard_det(DetString, Det),
	PredArgsTerm = term__functor(term__atom("pred"), PredArgsList, _),
	parse_dcg_pred_expr_args(PredArgsList, Vars, Modes).

parse_func_expression(FuncTerm, Vars, Modes, Det) :-
	%
	% parse a func expression with specified modes and determinism
	%
	FuncTerm = term__functor(term__atom("is"), [EqTerm, DetTerm], _),
	EqTerm = term__functor(term__atom("="), [FuncArgsTerm, RetTerm], _),
	DetTerm = term__functor(term__atom(DetString), [], _),
	standard_det(DetString, Det),
	FuncArgsTerm = term__functor(term__atom("func"), FuncArgsList, _),
	parse_pred_expr_args(FuncArgsList, Vars0, Modes0),
	parse_lambda_arg(RetTerm, RetVar, RetMode),
	list__append(Vars0, [RetVar], Vars),
	list__append(Modes0, [RetMode], Modes).
parse_func_expression(FuncTerm, Vars, Modes, Det) :-
	%
	% parse a func expression with unspecified modes and determinism
	%
	FuncTerm = term__functor(term__atom("="), [FuncArgsTerm, RetVar], _),
	FuncArgsTerm = term__functor(term__atom("func"), Vars0, _),
	%
	% the argument modes default to `in',
	% the return mode defaults to `out',
	% and the determinism defaults to `det'.
	%
	in_mode(InMode),
	out_mode(OutMode),
	list__length(Vars0, NumVars),
	list__duplicate(NumVars, InMode, Modes0),
	RetMode = OutMode,
	Det = det,
	list__append(Modes0, [RetMode], Modes),
	list__append(Vars0, [RetVar], Vars1),
	list__map(term__coerce, Vars1, Vars).

:- pred parse_pred_expr_args(list(term), list(prog_term), list(mode)).
:- mode parse_pred_expr_args(in, out, out) is semidet.

parse_pred_expr_args([], [], []).
parse_pred_expr_args([Term|Terms], [Arg|Args], [Mode|Modes]) :-
	parse_lambda_arg(Term, Arg, Mode),
	parse_pred_expr_args(Terms, Args, Modes).

	% parse_dcg_pred_expr_args is like parse_pred_expr_args except
	% that the last two elements of the list are the modes of the
	% two dcg arguments.
:- pred parse_dcg_pred_expr_args(list(term), list(prog_term),
		list(mode)).
:- mode parse_dcg_pred_expr_args(in, out, out) is semidet.

parse_dcg_pred_expr_args([DCGModeTerm0, DCGModeTerm1], [],
		[DCGMode0, DCGMode1]) :-
	convert_mode(DCGModeTerm0, DCGMode0),
	convert_mode(DCGModeTerm1, DCGMode1).
parse_dcg_pred_expr_args([Term|Terms], [Arg|Args], [Mode|Modes]) :-
	Terms = [_, _|_],
	parse_lambda_arg(Term, Arg, Mode),
	parse_dcg_pred_expr_args(Terms, Args, Modes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
