%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_util.m.
% Main author: fjh.
%
% This module defines the types used by prog_io and its subcontractors
% to return the results of parsing, and some utility predicates needed
% by several of prog_io's submodules.
%
% Most parsing predicates must check for errors. They return either the
% item(s) they were looking for, or an error indication.
%
% Most of the parsing predicates return a `maybe1(T)' or a `maybe2(T1, T2)',
% which will either be the `ok(ParseTree)' (or `ok(ParseTree1, ParseTree2)'),
% if the parse is successful, or `error(Message, Term)' if it is not.
% The `Term' there should be the term which is syntactically incorrect.

:- module parse_tree__prog_io_util.

:- interface.

:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.

:- import_module list, map, std_util, term.

:- type maybe2(T1, T2)
	--->	error(string, term)
	;	ok(T1, T2).

:- type maybe3(T1, T2, T3)
	--->	error(string, term)
	;	ok(T1, T2, T3).

:- type maybe1(T)	== 	maybe1(T, generic).
:- type maybe1(T, U)	--->	error(string, term(U))
			;	ok(T).

:- type maybe_functor	== 	maybe_functor(generic).
:- type maybe_functor(T) == 	maybe2(sym_name, list(term(T))).

	% ok(SymName, Args - MaybeFuncRetArg) ; error(Msg, Term).
:- type maybe_pred_or_func(T) == maybe2(sym_name, pair(list(T), maybe(T))).

:- type maybe_item_and_context
			==	maybe2(item, prog_context).

:- type var2tvar	==	map(var, tvar).

:- type var2pvar	==	map(var, prog_var).

:- type parser(T) == pred(term, maybe1(T)).
:- mode parser == (pred(in, out) is det).

:- pred add_context(maybe1(item)::in, prog_context::in,
	maybe_item_and_context::out) is det.

%
% Various predicates to parse small bits of syntax.
% These predicates simply fail if they encounter a syntax error.
%

:- pred parse_list_of_vars(term(T)::in, list(var(T))::out) is semidet.

	% Parse a list of quantified variables, splitting it into
	% state variables and ordinary logic variables, respectively.
	%
:- pred parse_quantifier_vars(term(T)::in, list(var(T))::out,
	list(var(T))::out) is semidet.

:- pred parse_name_and_arity(module_name::in, term(_T)::in,
	sym_name::out, arity::out) is semidet.

:- pred parse_name_and_arity(term(_T)::in, sym_name::out, arity::out)
	is semidet.

:- pred parse_pred_or_func_name_and_arity(module_name::in,
	term(_T)::in, pred_or_func::out, sym_name::out, arity::out) is semidet.

:- pred parse_pred_or_func_name_and_arity(term(_T)::in, pred_or_func::out,
	sym_name::out, arity::out) is semidet.

:- pred parse_pred_or_func_and_args(maybe(module_name)::in, term(_T)::in,
	term(_T)::in, string::in, maybe_pred_or_func(term(_T))::out) is det.

:- pred parse_pred_or_func_and_args(term(_T)::in, pred_or_func::out,
	sym_name::out, list(term(_T))::out) is semidet.

:- pred convert_type(term(T)::in, (type)::out) is det.

:- type allow_constrained_inst_var
	--->	allow_constrained_inst_var
	;	no_allow_constrained_inst_var.

:- pred convert_mode_list(allow_constrained_inst_var::in, list(term)::in,
	list(mode)::out) is semidet.

:- pred convert_mode(allow_constrained_inst_var::in, term::in, (mode)::out)
	is semidet.

:- pred convert_inst_list(allow_constrained_inst_var::in, list(term)::in,
	list(inst)::out) is semidet.

:- pred convert_inst(allow_constrained_inst_var::in, term::in, (inst)::out)
	is semidet.

:- pred standard_det(string::in, determinism::out) is semidet.

	% convert a "disjunction" (bunch of terms separated by ';'s) to a list

:- pred disjunction_to_list(term(T)::in, list(term(T))::out) is det.

	% convert a "conjunction" (bunch of terms separated by ','s) to a list

:- pred conjunction_to_list(term(T)::in, list(term(T))::out) is det.

	% list_to_conjunction(Context, First, Rest, Term).
	% convert a list to a "conjunction" (bunch of terms separated by ','s)

:- pred list_to_conjunction(prog_context::in, term(T)::in, list(term(T))::in,
	term(T)::out) is det.

	% convert a "sum" (bunch of terms separated by '+' operators) to a list

:- pred sum_to_list(term(T)::in, list(term(T))::out) is det.

	% Parse a comma-separated list (misleading described as
	% a "conjunction") of things.

:- pred parse_list(parser(T)::parser, term::in, maybe1(list(T))::out) is det.

:- pred map_parser(parser(T)::parser, list(term)::in, maybe1(list(T))::out)
	is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_io_goal.
:- import_module parse_tree__prog_util.

:- import_module bool, string, std_util, term, set.

add_context(error(M, T), _, error(M, T)).
add_context(ok(Item), Context, ok(Item, Context)).

parse_name_and_arity(ModuleName, PredAndArityTerm, SymName, Arity) :-
	PredAndArityTerm = term__functor(term__atom("/"),
		[PredNameTerm, ArityTerm], _),
	parse_implicitly_qualified_term(ModuleName,
		PredNameTerm, PredNameTerm, "", ok(SymName, [])),
	ArityTerm = term__functor(term__integer(Arity), [], _).

parse_name_and_arity(PredAndArityTerm, SymName, Arity) :-
	parse_name_and_arity(unqualified(""),
		PredAndArityTerm, SymName, Arity).

parse_pred_or_func_name_and_arity(ModuleName, PorFPredAndArityTerm,
		PredOrFunc, SymName, Arity) :-
	PorFPredAndArityTerm = term__functor(term__atom(PredOrFuncStr),
		Args, _),
	( PredOrFuncStr = "pred", PredOrFunc = predicate
	; PredOrFuncStr = "func", PredOrFunc = function
	),
	Args = [Arg],
	parse_name_and_arity(ModuleName, Arg, SymName, Arity).

parse_pred_or_func_name_and_arity(PorFPredAndArityTerm,
		PredOrFunc, SymName, Arity) :-
	parse_pred_or_func_name_and_arity(unqualified(""),
		PorFPredAndArityTerm, PredOrFunc, SymName, Arity).

parse_pred_or_func_and_args(Term, PredOrFunc, SymName, ArgTerms) :-
	parse_pred_or_func_and_args(no, Term, Term, "",
		ok(SymName, ArgTerms0 - MaybeRetTerm)),
	(
		MaybeRetTerm = yes(RetTerm),
		PredOrFunc = function,
		list__append(ArgTerms0, [RetTerm], ArgTerms)
	;
		MaybeRetTerm = no,
		PredOrFunc = predicate,
		ArgTerms = ArgTerms0
	).

parse_pred_or_func_and_args(MaybeModuleName, PredAndArgsTerm, ErrorTerm,
		Msg, PredAndArgsResult) :-
	(
		PredAndArgsTerm = term__functor(term__atom("="),
			[FuncAndArgsTerm, FuncResultTerm], _)
	->
		FunctorTerm = FuncAndArgsTerm,
		MaybeFuncResult = yes(FuncResultTerm)
	;
		FunctorTerm = PredAndArgsTerm,
		MaybeFuncResult = no
	),
	(
		MaybeModuleName = yes(ModuleName),
		parse_implicitly_qualified_term(ModuleName, FunctorTerm,
			ErrorTerm, Msg, Result)
	;
		MaybeModuleName = no,
		parse_qualified_term(FunctorTerm, ErrorTerm, Msg, Result)
	),
	(
		Result = ok(SymName, Args),
		PredAndArgsResult = ok(SymName, Args - MaybeFuncResult)
	;
		Result = error(ErrorMsg, Term),
		PredAndArgsResult = error(ErrorMsg, Term)
	).

parse_list_of_vars(term__functor(term__atom("[]"), [], _), []).
parse_list_of_vars(term__functor(term__atom("[|]"),
		[Head, Tail], _), [V | Vs]) :-
	Head = term__variable(V),
	parse_list_of_vars(Tail, Vs).

convert_type(T0, T) :-
	term__coerce(strip_prog_context(T0), T).

	% Strip out the prog_context fields, replacing them with empty
	% prog_context (as obtained by term__context_init/1)
	% in a type or list of types.
	%
	% This is necessary to allow maps indexed by class constraints.
	% Also, the version number computation for smart recompilation
	% relies on being able to unify program items, which won't
	% work if the types in the items contain context information.
:- func strip_prog_context(term(T)) = term(T).

strip_prog_context(term__variable(V)) = term__variable(V).
strip_prog_context(term__functor(F, As, _)) =
	term__functor(F,
		list__map(strip_prog_context, As),
		term__context_init).

convert_mode_list(_, [], []).
convert_mode_list(AllowConstrainedInstVar, [H0 | T0], [H | T]) :-
	convert_mode(AllowConstrainedInstVar, H0, H),
	convert_mode_list(AllowConstrainedInstVar, T0, T).

	%
	% The new operator for mode declarations is >>.
	% Previously we used ->, but this required a high-precedence
	% operator such as :: for the :- mode delcaration.
	%
	% Using >> allows us to use == for the :- mode declaration.
	%
	% Eventually we can stop supporting :: and -> in :- mode
	% declarations altogether.
	%
convert_mode(AllowConstrainedInstVar, Term, Mode) :-
	(
		(
			Term = term__functor(term__atom(">>"),
				[InstA, InstB], _)
		;
			Term = term__functor(term__atom("->"),
				[InstA, InstB], _)
		)
	->
		convert_inst(AllowConstrainedInstVar, InstA, ConvertedInstA),
		convert_inst(AllowConstrainedInstVar, InstB, ConvertedInstB),
		Mode = (ConvertedInstA -> ConvertedInstB)
	;
		% Handle higher-order predicate modes:
		% a mode of the form
		%	pred(<Mode1>, <Mode2>, ...) is <Det>
		% is an abbreviation for the inst mapping
		% 	(  pred(<Mode1>, <Mode2>, ...) is <Det>
		%	-> pred(<Mode1>, <Mode2>, ...) is <Det>
		%	)

		Term = term__functor(term__atom("is"), [PredTerm, DetTerm], _),
		PredTerm = term__functor(term__atom("pred"), ArgModesTerms, _)
	->
		DetTerm = term__functor(term__atom(DetString), [], _),
		standard_det(DetString, Detism),
		convert_mode_list(AllowConstrainedInstVar, ArgModesTerms,
			ArgModes),
		PredInstInfo = pred_inst_info(predicate, ArgModes, Detism),
		Inst = ground(shared, higher_order(PredInstInfo)),
		Mode = (Inst -> Inst)
	;
		% Handle higher-order function modes:
		% a mode of the form
		%	func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
		% is an abbreviation for the inst mapping
		% 	(  func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
		%	-> func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
		%	)

		Term = term__functor(term__atom("is"), [EqTerm, DetTerm], _),
		EqTerm = term__functor(term__atom("="),
			[FuncTerm, RetModeTerm], _),
		FuncTerm = term__functor(term__atom("func"), ArgModesTerms, _)
	->
		DetTerm = term__functor(term__atom(DetString), [], _),
		standard_det(DetString, Detism),
		convert_mode_list(AllowConstrainedInstVar, ArgModesTerms,
			ArgModes0),
		convert_mode(AllowConstrainedInstVar, RetModeTerm, RetMode),
		list__append(ArgModes0, [RetMode], ArgModes),
		FuncInstInfo = pred_inst_info(function, ArgModes, Detism),
		Inst = ground(shared, higher_order(FuncInstInfo)),
		Mode = (Inst -> Inst)
	;
		parse_qualified_term(Term, Term, "mode definition", R),
		R = ok(Name, Args),	% should improve error reporting
		convert_inst_list(AllowConstrainedInstVar, Args, ConvertedArgs),
		Mode = user_defined_mode(Name, ConvertedArgs)
	).

convert_inst_list(_, [], []).
convert_inst_list(AllowConstrainedInstVar, [H0 | T0], [H | T]) :-
	convert_inst(AllowConstrainedInstVar, H0, H),
	convert_inst_list(AllowConstrainedInstVar, T0, T).

convert_inst(_, term__variable(V0), inst_var(V)) :-
	term__coerce_var(V0, V).
convert_inst(AllowConstrainedInstVar, Term, Result) :-
	Term = term__functor(term__atom(Name), Args0, _Context),
	(
		convert_simple_builtin_inst(Name, Args0, Result0)
	->
		Result = Result0
	;
		% The syntax for a higher-order pred inst is
		%
		%	pred(<Mode1>, <Mode2>, ...) is <Detism>
		%
		% where <Mode1>, <Mode2>, ... are a list of modes,
		% and <Detism> is a determinism.

		Name = "is", Args0 = [PredTerm, DetTerm],
		PredTerm = term__functor(term__atom("pred"), ArgModesTerm, _)
	->
		DetTerm = term__functor(term__atom(DetString), [], _),
		standard_det(DetString, Detism),
		convert_mode_list(AllowConstrainedInstVar, ArgModesTerm,
			ArgModes),
		PredInst = pred_inst_info(predicate, ArgModes, Detism),
		Result = ground(shared, higher_order(PredInst))
	;

		% The syntax for a higher-order func inst is
		%
		%	func(<Mode1>, <Mode2>, ...) = <RetMode> is <Detism>
		%
		% where <Mode1>, <Mode2>, ... are a list of modes,
		% <RetMode> is a mode, and <Detism> is a determinism.

		Name = "is", Args0 = [EqTerm, DetTerm],
		EqTerm = term__functor(term__atom("="),
			[FuncTerm, RetModeTerm], _),
		FuncTerm = term__functor(term__atom("func"), ArgModesTerm, _)
	->
		DetTerm = term__functor(term__atom(DetString), [], _),
		standard_det(DetString, Detism),
		convert_mode_list(AllowConstrainedInstVar, ArgModesTerm,
			ArgModes0),
		convert_mode(AllowConstrainedInstVar, RetModeTerm, RetMode),
		list__append(ArgModes0, [RetMode], ArgModes),
		FuncInst = pred_inst_info(function, ArgModes, Detism),
		Result = ground(shared, higher_order(FuncInst))

	% `bound' insts
	; Name = "bound", Args0 = [Disj] ->
		parse_bound_inst_list(AllowConstrainedInstVar, Disj, shared,
			Result)
	% `bound_unique' is for backwards compatibility
	% - use `unique' instead
	; Name = "bound_unique", Args0 = [Disj] ->
		parse_bound_inst_list(AllowConstrainedInstVar, Disj, unique,
			Result)
	; Name = "unique", Args0 = [Disj] ->
		parse_bound_inst_list(AllowConstrainedInstVar, Disj, unique,
			Result)
	; Name = "mostly_unique", Args0 = [Disj] ->
		parse_bound_inst_list(AllowConstrainedInstVar, Disj,
			mostly_unique, Result)
	; Name = "=<", Args0 = [VarTerm, InstTerm] ->
		AllowConstrainedInstVar = allow_constrained_inst_var,
		VarTerm = term__variable(Var),
		% Do not allow nested constrained_inst_vars.
		convert_inst(no_allow_constrained_inst_var, InstTerm, Inst),
		Result = constrained_inst_vars(set__make_singleton_set(
			term__coerce_var(Var)), Inst)
	% anything else must be a user-defined inst
	;
		parse_qualified_term(Term, Term, "inst",
			ok(QualifiedName, Args1)),
		(
			mercury_public_builtin_module(BuiltinModule),
			sym_name_get_module_name(QualifiedName, unqualified(""),
				BuiltinModule),
			% If the term is qualified with the `builtin' module
			% then it may be one of the simple builtin insts.
			% We call convert_inst recursively to check for this.
			unqualify_name(QualifiedName, UnqualifiedName),
			convert_simple_builtin_inst(UnqualifiedName, Args1,
				Result0),

			% However, if the inst is a user_inst defined inside
			% the `builtin' module then we need to make sure it is
			% properly module-qualified.
			Result0 \= defined_inst(user_inst(_, _))
		->
			Result = Result0
		;
			convert_inst_list(AllowConstrainedInstVar, Args1, Args),
			Result = defined_inst(user_inst(QualifiedName, Args))
		)
	).

	% A "simple" builtin inst is one that has no arguments and no special
	% syntax.
:- pred convert_simple_builtin_inst(string::in, list(term)::in, (inst)::out)
	is semidet.

convert_simple_builtin_inst(Name, [], Inst) :-
	convert_simple_builtin_inst_2(Name, Inst).

:- pred convert_simple_builtin_inst_2(string::in, (inst)::out) is semidet.

	% `free' insts
convert_simple_builtin_inst_2("free", free).

	% `any' insts
convert_simple_builtin_inst_2("any",			any(shared)).
convert_simple_builtin_inst_2("unique_any",		any(unique)).
convert_simple_builtin_inst_2("mostly_unique_any",	any(mostly_unique)).
convert_simple_builtin_inst_2("clobbered_any",		any(clobbered)).
convert_simple_builtin_inst_2("mostly_clobbered_any",	any(mostly_clobbered)).

	% `ground' insts
convert_simple_builtin_inst_2("ground",		ground(shared, none)).
convert_simple_builtin_inst_2("unique",		ground(unique, none)).
convert_simple_builtin_inst_2("mostly_unique",	ground(mostly_unique, none)).
convert_simple_builtin_inst_2("clobbered",	ground(clobbered, none)).
convert_simple_builtin_inst_2("mostly_clobbered",
						ground(mostly_clobbered, none)).

	% `not_reached' inst
convert_simple_builtin_inst_2("not_reached", not_reached).

standard_det("det", det).
standard_det("cc_nondet", cc_nondet).
standard_det("cc_multi", cc_multidet).
standard_det("nondet", nondet).
standard_det("multi", multidet).
standard_det("multidet", multidet).
standard_det("semidet", semidet).
standard_det("erroneous", erroneous).
standard_det("failure", failure).

:- pred parse_bound_inst_list(allow_constrained_inst_var::in, term::in,
	uniqueness::in, (inst)::out) is semidet.

parse_bound_inst_list(AllowConstrainedInstVar, Disj, Uniqueness,
		bound(Uniqueness, Functors)) :-
	disjunction_to_list(Disj, List),
	convert_bound_inst_list(AllowConstrainedInstVar, List, Functors0),
	list__sort(Functors0, Functors),
	% check that the list doesn't specify the same functor twice
	\+ (
		list__append(_, SubList, Functors),
		SubList = [F1, F2 | _],
		F1 = functor(ConsId, _),
		F2 = functor(ConsId, _)
	).

:- pred convert_bound_inst_list(allow_constrained_inst_var::in, list(term)::in,
	list(bound_inst)::out) is semidet.

convert_bound_inst_list(_, [], []).
convert_bound_inst_list(AllowConstrainedInstVar, [H0 | T0], [H | T]) :-
	convert_bound_inst(AllowConstrainedInstVar, H0, H),
	convert_bound_inst_list(AllowConstrainedInstVar, T0, T).

:- pred convert_bound_inst(allow_constrained_inst_var::in, term::in,
	bound_inst::out) is semidet.

convert_bound_inst(AllowConstrainedInstVar, InstTerm, functor(ConsId, Args)) :-
	InstTerm = term__functor(Functor, Args0, _),
	( Functor = term__atom(_) ->
		parse_qualified_term(InstTerm, InstTerm, "inst",
			ok(SymName, Args1)),
		list__length(Args1, Arity),
		ConsId = cons(SymName, Arity)
	;
		Args1 = Args0,
		list__length(Args1, Arity),
		ConsId = make_functor_cons_id(Functor, Arity)
	),
	convert_inst_list(AllowConstrainedInstVar, Args1, Args).

disjunction_to_list(Term, List) :-
	binop_term_to_list(";", Term, List).

conjunction_to_list(Term, List) :-
	binop_term_to_list(",", Term, List).

list_to_conjunction(_, Term, [], Term).
list_to_conjunction(Context, First, [Second | Rest], Term) :-
	list_to_conjunction(Context, Second, Rest, Tail),
	Term = term__functor(term__atom(","), [First, Tail], Context).

sum_to_list(Term, List) :-
	binop_term_to_list("+", Term, List).

	% general predicate to convert terms separated by any specified
	% operator into a list

:- pred binop_term_to_list(string::in, term(T)::in, list(term(T))::out) is det.

binop_term_to_list(Op, Term, List) :-
	binop_term_to_list_2(Op, Term, [], List).

:- pred binop_term_to_list_2(string::in, term(T)::in, list(term(T))::in,
	list(term(T))::out) is det.

binop_term_to_list_2(Op, Term, !List) :-
	(
		Term = term__functor(term__atom(Op), [L, R], _Context)
	->
		binop_term_to_list_2(Op, R, !List),
		binop_term_to_list_2(Op, L, !List)
	;
		!:List = [Term | !.List]
	).

parse_list(Parser, Term, Result) :-
	conjunction_to_list(Term, List),
	map_parser(Parser, List, Result).

map_parser(_, [], ok([])).
map_parser(Parser, [X | Xs], Result) :-
	call(Parser, X, X_Result),
	map_parser(Parser, Xs, Xs_Result),
	combine_list_results(X_Result, Xs_Result, Result).

	% If a list of things contains multiple errors, then we only
	% report the first one.
:- pred combine_list_results(maybe1(T)::in, maybe1(list(T))::in,
	maybe1(list(T))::out) is det.

combine_list_results(error(Msg, Term), _, error(Msg, Term)).
combine_list_results(ok(_), error(Msg, Term), error(Msg, Term)).
combine_list_results(ok(X), ok(Xs), ok([X | Xs])).

%-----------------------------------------------------------------------------%

parse_quantifier_vars(functor(atom("[]"),  [],     _), [],  []).
parse_quantifier_vars(functor(atom("[|]"), [H, T], _), SVs, Vs) :-
	(
		H   = functor(atom("!"), [variable(SV)], _),
		SVs = [SV | SVs0],
		parse_quantifier_vars(T, SVs0, Vs)
	;
		H   = variable(V),
		Vs = [V | Vs0],
		parse_quantifier_vars(T, SVs, Vs0)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
