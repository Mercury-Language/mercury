%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_goal.m.
% Main author: fjh.
%
% This module defines the predicates that parse goals.

:- module parse_tree__prog_io_goal.

:- interface.

:- import_module parse_tree__prog_data.

:- import_module list.
:- import_module term.

	% Convert a single term into a goal.
	%
:- pred parse_goal(term::in, goal::out, prog_varset::in, prog_varset::out)
	is det.

	% Convert a term, possibly starting with `some [Vars]', into
	% a list of the quantified variables, a list of quantified
	% state variables, and a goal. (If the term doesn't start
	% with `some [Vars]', we return empty lists of variables.)
	%
:- pred parse_some_vars_goal(term::in, list(prog_var)::out,
	list(prog_var)::out, goal::out, prog_varset::in, prog_varset::out)
	is det.

	% parse_pred_expression/3 converts the first argument to a :-/2
	% higher-order pred expression into a list of variables, a list
	% of their corresponding modes, and a determinism.
	%
:- pred parse_pred_expression(term::in, lambda_eval_method::out,
	list(prog_term)::out, list(mode)::out, determinism::out) is semidet.

	% parse_dcg_pred_expression/3 converts the first argument to a -->/2
	% higher-order dcg pred expression into a list of arguments, a list
	% of their corresponding modes and the two dcg argument modes, and a
	% determinism.
	% This is a variant of the higher-order pred syntax:
	%	`(pred(Var1::Mode1, ..., VarN::ModeN, DCG0Mode, DCGMode)
	%		is Det --> Goal)'.
	%
:- pred parse_dcg_pred_expression(term::in, lambda_eval_method::out,
	list(prog_term)::out, list(mode)::out, determinism::out) is semidet.

	% parse_func_expression/3 converts the first argument to a :-/2
	% higher-order func expression into a list of arguments, a list
	% of their corresponding modes, and a determinism.  The syntax
	% of a higher-order func expression is
	% 	`(func(Var1::Mode1, ..., VarN::ModeN) = (VarN1::ModeN1) is Det
	%		:- Goal)'
	% or
	% 	`(func(Var1, ..., VarN) = (VarN1) is Det :- Goal)'
	% 		where the modes are assumed to be `in' for the
	% 		function arguments and `out' for the result
	% or
	% 	`(func(Var1, ..., VarN) = (VarN1) :- Goal)'
	% 		where the modes are assumed as above, and the
	% 		determinism is assumed to be det
	% or
	% 	`(func(Var1, ..., VarN) = (VarN1). '
	%
:- pred parse_func_expression(term::in, lambda_eval_method::out,
	list(prog_term)::out, list(mode)::out, determinism::out) is semidet.

	% parse_lambda_eval_method/3 extracts the `aditi_bottom_up'
	% annotation (if any) from a pred expression and returns the
	% rest of the term.
	%
:- pred parse_lambda_eval_method(term(T)::in, lambda_eval_method::out,
	term(T)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_io_util.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_out.

:- import_module int.
:- import_module map.
:- import_module std_util.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

	% Parse a goal.
	%
	% We could do some error-checking here, but all errors are picked up
	% in either the type-checker or parser anyway.

parse_goal(Term, Goal, !VarSet) :-
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
		parse_goal_2(Name, Args, GoalExpr, !VarSet)
	->
		Goal = GoalExpr - Context
	;
		% it's not a builtin
		term__coerce(Term, ArgsTerm),
		(
			% check for predicate calls
			sym_name_and_args(ArgsTerm, SymName, Args)
		->
			Goal = call(SymName, Args, pure) - Context
		;
			% A call to a free variable, or to a number or string.
			% Just translate it into a call to call/1 - the
			% typechecker will catch calls to numbers and strings.
			Goal = call(unqualified("call"), [ArgsTerm], pure)
				- Context
		)
	).

%-----------------------------------------------------------------------------%

:- pred parse_goal_2(string::in, list(term)::in, goal_expr::out,
	prog_varset::in, prog_varset::out) is semidet.

	% Since (A -> B) has different semantics in standard Prolog
	% (A -> B ; fail) than it does in NU-Prolog or Mercury (A -> B ; true),
	% for the moment we'll just disallow it.
	% For consistency we also disallow if-then without the else.

parse_goal_2("true", [], true, !V).
parse_goal_2("fail", [], fail, !V).
parse_goal_2("=", [A0, B0], unify(A, B, pure), !V) :-
	term__coerce(A0, A),
	term__coerce(B0, B).
parse_goal_2(",", [A0, B0], (A, B), !V) :-
	parse_goal(A0, A, !V),
	parse_goal(B0, B, !V).
parse_goal_2("&", [A0, B0], (A & B), !V) :-
	parse_goal(A0, A, !V),
	parse_goal(B0, B, !V).
parse_goal_2(";", [A0, B0], R, !V) :-
	(
		A0 = term__functor(term__atom("->"), [X0, Y0], _Context)
	->
		parse_some_vars_goal(X0, Vars, StateVars, X, !V),
		parse_goal(Y0, Y, !V),
		parse_goal(B0, B, !V),
		R = if_then_else(Vars, StateVars, X, Y, B)
	;
		parse_goal(A0, A, !V),
		parse_goal(B0, B, !V),
		R = (A;B)
	).
parse_goal_2("else", [IF, C0], if_then_else(Vars, StateVars, A, B, C), !V) :-
	IF = term__functor(term__atom("if"),
		[term__functor(term__atom("then"), [A0, B0], _)], _),
	parse_some_vars_goal(A0, Vars, StateVars, A, !V),
	parse_goal(B0, B, !V),
	parse_goal(C0, C, !V).

parse_goal_2("not", [A0], not(A), !V) :-
	parse_goal(A0, A, !V).

parse_goal_2("\\+", [A0], not(A), !V) :-
	parse_goal(A0, A, !V).

parse_goal_2("all", [QVars, A0], GoalExpr, !V):-

		% Extract any state variables in the quantifier.
		%
	parse_quantifier_vars(QVars, StateVars0, Vars0),
	list__map(term__coerce_var, StateVars0, StateVars),
	list__map(term__coerce_var, Vars0, Vars),

	parse_goal(A0, A @ (GoalExprA - ContextA), !V),

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

	% handle implication
parse_goal_2("<=", [A0, B0], implies(B, A), !V):-
	parse_goal(A0, A, !V),
	parse_goal(B0, B, !V).

parse_goal_2("=>", [A0, B0], implies(A, B), !V):-
	parse_goal(A0, A, !V),
	parse_goal(B0, B, !V).

	% handle equivalence
parse_goal_2("<=>", [A0, B0], equivalent(A, B), !V):-
	parse_goal(A0, A, !V),
	parse_goal(B0, B, !V).

parse_goal_2("some", [QVars, A0], GoalExpr, !V):-

		% Extract any state variables in the quantifier.
		%
	parse_quantifier_vars(QVars, StateVars0, Vars0),
	list__map(term__coerce_var, StateVars0, StateVars),
	list__map(term__coerce_var, Vars0, Vars),

	parse_goal(A0, A @ (GoalExprA - ContextA), !V),
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

parse_goal_2("promise_equivalent_solutions", [OVars, A0], GoalExpr, !V):-
	parse_goal(A0, A, !V),
	parse_vars_and_state_vars(OVars, Vars0, DotSVars0, ColonSVars0),
	list__map(term__coerce_var, Vars0, Vars),
	list__map(term__coerce_var, DotSVars0, DotSVars),
	list__map(term__coerce_var, ColonSVars0, ColonSVars),
	GoalExpr = promise_equivalent_solutions(Vars, DotSVars, ColonSVars, A).

parse_goal_2("promise_pure", [A0], GoalExpr, !V):-
	parse_goal(A0, A, !V),
	GoalExpr = promise_purity(dont_make_implicit_promises, pure, A).

parse_goal_2("promise_semipure", [A0], GoalExpr, !V):-
	parse_goal(A0, A, !V),
	GoalExpr = promise_purity(dont_make_implicit_promises, semipure, A).

parse_goal_2("promise_impure", [A0], GoalExpr, !V):-
	parse_goal(A0, A, !V),
	GoalExpr = promise_purity(dont_make_implicit_promises, impure, A).

parse_goal_2("promise_pure_implicit", [A0], GoalExpr, !V):-
	parse_goal(A0, A, !V),
	GoalExpr = promise_purity(make_implicit_promises, pure, A).

parse_goal_2("promise_semipure_implicit", [A0], GoalExpr, !V):-
	parse_goal(A0, A, !V),
	GoalExpr = promise_purity(make_implicit_promises, semipure, A).

parse_goal_2("promise_impure_implicit", [A0], GoalExpr, !V):-
	parse_goal(A0, A, !V),
	GoalExpr = promise_purity(make_implicit_promises, impure, A).

	% The following is a temporary hack to handle `is' in
	% the parser - we ought to handle it in the code generation -
	% but then `is/2' itself is a bit of a hack
	%
parse_goal_2("is", [A0, B0], unify(A, B, pure), !V) :-
	term__coerce(A0, A),
	term__coerce(B0, B).
parse_goal_2("impure", [A0], A, !V) :-
	parse_goal_with_purity(A0, (impure), A, !V).
parse_goal_2("semipure", [A0], A, !V) :-
	parse_goal_with_purity(A0, (semipure), A, !V).

:- pred parse_goal_with_purity(term::in, purity::in, goal_expr::out,
	prog_varset::in, prog_varset::out) is det.

parse_goal_with_purity(A0, Purity, A, !V) :-
	parse_goal(A0, A1, !V),
	( A1 = call(Pred, Args, pure) - _ ->
		A = call(Pred, Args, Purity)
	; A1 = unify(ProgTerm1, ProgTerm2, pure) - _ ->
		A = unify(ProgTerm1, ProgTerm2, Purity)
	;
		% Inappropriate placement of an impurity marker, so we treat
		% it like a predicate call.  typecheck.m prints out something
		% descriptive for these errors.
		purity_name(Purity, PurityString),
		term__coerce(A0, A2),
		A = call(unqualified(PurityString), [A2], pure)
	).

%-----------------------------------------------------------------------------%

parse_some_vars_goal(A0, Vars, StateVars, A, !VarSet) :-
	(
		A0 = term__functor(term__atom("some"), [QVars, A1], _Context),
		parse_quantifier_vars(QVars, StateVars0, Vars0)
	->
		list__map(term__coerce_var, StateVars0, StateVars),
		list__map(term__coerce_var, Vars0,      Vars),
		parse_goal(A1, A, !VarSet)
	;
		Vars      = [],
		StateVars = [],
		parse_goal(A0, A, !VarSet)
	).

%-----------------------------------------------------------------------------%

:- pred parse_lambda_arg(term::in, prog_term::out, (mode)::out) is semidet.

parse_lambda_arg(Term, ArgTerm, Mode) :-
	Term = term__functor(term__atom("::"), [ArgTerm0, ModeTerm], _),
	term__coerce(ArgTerm0, ArgTerm),
	convert_mode(allow_constrained_inst_var, ModeTerm, Mode0),
	constrain_inst_vars_in_mode(Mode0, Mode).

%-----------------------------------------------------------------------------%
% 
% Code for parsing pred/func expressions
%

parse_pred_expression(PredTerm, EvalMethod, Args, Modes, Det) :-
	PredTerm = term__functor(term__atom("is"),
		[PredEvalArgsTerm, DetTerm], _),
	DetTerm = term__functor(term__atom(DetString), [], _),
	standard_det(DetString, Det),
	parse_lambda_eval_method(PredEvalArgsTerm, EvalMethod, PredArgsTerm),
	PredArgsTerm = term__functor(term__atom("pred"), PredArgsList, _),
	parse_pred_expr_args(PredArgsList, Args, Modes),
	inst_var_constraints_are_consistent_in_modes(Modes).

parse_dcg_pred_expression(PredTerm, EvalMethod, Args, Modes, Det) :-
	PredTerm = term__functor(term__atom("is"),
		[PredEvalArgsTerm, DetTerm], _),
	DetTerm = term__functor(term__atom(DetString), [], _),
	standard_det(DetString, Det),
	parse_lambda_eval_method(PredEvalArgsTerm, EvalMethod, PredArgsTerm),
	PredArgsTerm = term__functor(term__atom("pred"), PredArgsList, _),
	parse_dcg_pred_expr_args(PredArgsList, Args, Modes),
	inst_var_constraints_are_consistent_in_modes(Modes).

parse_func_expression(FuncTerm, EvalMethod, Args, Modes, Det) :-
	%
	% Parse a func expression with specified modes and determinism.
	%
	FuncTerm = term__functor(term__atom("is"), [EqTerm, DetTerm], _),
	EqTerm = term__functor(term__atom("="),
		[FuncEvalArgsTerm, RetTerm], _),
	DetTerm = term__functor(term__atom(DetString), [], _),
	standard_det(DetString, Det),
	parse_lambda_eval_method(FuncEvalArgsTerm, EvalMethod, FuncArgsTerm),
	FuncArgsTerm = term__functor(term__atom("func"), FuncArgsList, _),

	( parse_pred_expr_args(FuncArgsList, Args0, Modes0) ->
		parse_lambda_arg(RetTerm, RetArg, RetMode),
		list__append(Args0, [RetArg], Args),
		list__append(Modes0, [RetMode], Modes),
		inst_var_constraints_are_consistent_in_modes(Modes)
	;
		%
		% The argument modes default to `in',
		% the return mode defaults to `out'.
		%
		in_mode(InMode),
		out_mode(OutMode),
		list__length(FuncArgsList, NumArgs),
		list__duplicate(NumArgs, InMode, Modes0),
		RetMode = OutMode,
		list__append(Modes0, [RetMode], Modes),
		list__append(FuncArgsList, [RetTerm], Args1),
		list__map(term__coerce, Args1, Args)
	).

parse_func_expression(FuncTerm, EvalMethod, Args, Modes, Det) :-
	%
	% parse a func expression with unspecified modes and determinism
	%
	FuncTerm = term__functor(term__atom("="),
		[FuncEvalArgsTerm, RetTerm], _),
	parse_lambda_eval_method(FuncEvalArgsTerm, EvalMethod, FuncArgsTerm),
	FuncArgsTerm = term__functor(term__atom("func"), Args0, _),
	%
	% the argument modes default to `in',
	% the return mode defaults to `out',
	% and the determinism defaults to `det'.
	%
	in_mode(InMode),
	out_mode(OutMode),
	list__length(Args0, NumArgs),
	list__duplicate(NumArgs, InMode, Modes0),
	RetMode = OutMode,
	Det = det,
	list__append(Modes0, [RetMode], Modes),
	inst_var_constraints_are_consistent_in_modes(Modes),
	list__append(Args0, [RetTerm], Args1),
	list__map(term__coerce, Args1, Args).

parse_lambda_eval_method(Term0, EvalMethod, Term) :-
	( Term0 = term__functor(term__atom(MethodStr), [Term1], _) ->
		( MethodStr = "aditi_bottom_up" ->
			EvalMethod = (aditi_bottom_up),
			Term = Term1
		;
			EvalMethod = normal,
			Term = Term0
		)
	;
		EvalMethod = normal,
		Term = Term0
	).

:- pred parse_pred_expr_args(list(term)::in, list(prog_term)::out,
	list(mode)::out) is semidet.

parse_pred_expr_args([], [], []).
parse_pred_expr_args([Term|Terms], [Arg|Args], [Mode|Modes]) :-
	parse_lambda_arg(Term, Arg, Mode),
	parse_pred_expr_args(Terms, Args, Modes).

	% parse_dcg_pred_expr_args is like parse_pred_expr_args except
	% that the last two elements of the list are the modes of the
	% two dcg arguments.
:- pred parse_dcg_pred_expr_args(list(term)::in, list(prog_term)::out,
	list(mode)::out) is semidet.

parse_dcg_pred_expr_args([DCGModeTermA, DCGModeTermB], [],
		[DCGModeA, DCGModeB]) :-
	convert_mode(allow_constrained_inst_var, DCGModeTermA, DCGModeA0),
	convert_mode(allow_constrained_inst_var, DCGModeTermB, DCGModeB0),
	constrain_inst_vars_in_mode(DCGModeA0, DCGModeA),
	constrain_inst_vars_in_mode(DCGModeB0, DCGModeB).
parse_dcg_pred_expr_args([Term|Terms], [Arg|Args], [Mode|Modes]) :-
	Terms = [_, _|_],
	parse_lambda_arg(Term, Arg, Mode),
	parse_dcg_pred_expr_args(Terms, Args, Modes).

%-----------------------------------------------------------------------------%
