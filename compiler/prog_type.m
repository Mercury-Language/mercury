%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main author: fjh
%
% Utility predicates dealing with types that do not require access to the
% HLDS.  (The predicates that do are in type_util.m.)
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_type.

:- interface.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------------%

	% type_is_higher_order(Type, Purity, PredOrFunc, ArgTypes, EvalMeth):
	% succeeds iff Type is a higher-order predicate or function type with
	% the specified argument types (for functions, the return type is
	% appended to the end of the argument types), purity, and
	% evaluation method.
	% 
:- pred type_is_higher_order((type)::in, purity::out, pred_or_func::out,
 	lambda_eval_method::out, list(type)::out) is semidet.
	
	% Succeed if the given type is a tuple type, returning
	% the argument types.
	%
:- pred type_is_tuple((type)::in, list(type)::out) is semidet.
	
	% type_has_variable_arity_ctor(Type, TypeCtor, TypeArgs)
	% Check if the principal type constructor of Type is of variable arity.
	% If yes, return the type constructor as TypeCtor and its args as
	% TypeArgs. If not, fail.
	%
:- pred type_has_variable_arity_ctor((type)::in, type_ctor::out,
	list(type)::out) is semidet.

 	% Given a non-variable type, return its type-id and argument types.
	% 
:- pred type_to_ctor_and_args((type)::in, type_ctor::out, list(type)::out)
 	is semidet.
	
	% type_ctor_is_higher_order(TypeCtor, PredOrFunc) succeeds iff
	% TypeCtor is a higher-order predicate or function type.
	%
:- pred type_ctor_is_higher_order(type_ctor::in, purity::out, pred_or_func::out,
	lambda_eval_method::out) is semidet.

	% type_ctor_is_tuple(TypeCtor) succeeds iff TypeCtor is a tuple type.
	%
:- pred type_ctor_is_tuple(type_ctor::in) is semidet.

	% type_ctor_is_variable(TypeCtor) succeeds iff TypeCtor is a variable.
	%
:- pred type_ctor_is_variable(type_ctor::in) is semidet.
	
	% Given a variable type, return its type variable.
	%
:- pred prog_type.var(type, tvar).
:- mode prog_type.var(in, out) is semidet.
:- mode prog_type.var(out, in) is det.
	
	% Return a list of the type variables of a type.
	%
:- pred prog_type.vars((type)::in, list(tvar)::out) is det.
	
	% Given a type_ctor and a list of argument types,
	% construct a type.
	%
:- pred construct_type(type_ctor::in, list(type)::in, (type)::out) is det.

:- pred construct_higher_order_type(purity::in, pred_or_func::in,
	lambda_eval_method::in, list(type)::in, (type)::out) is det.

:- pred construct_higher_order_pred_type(purity::in, lambda_eval_method::in,
	list(type)::in, (type)::out) is det.

:- pred construct_higher_order_func_type(purity::in, lambda_eval_method::in,
	list(type)::in, (type)::in, (type)::out) is det.
	
	% Make error messages more readable by removing "builtin."
	% qualifiers.
	%
:- pred strip_builtin_qualifiers_from_type((type)::in, (type)::out) is det.

:- pred strip_builtin_qualifiers_from_type_list(list(type)::in,
	list(type)::out) is det.

%-----------------------------------------------------------------------------%
%
% Utility predicates dealing with typeclass constraints.
%

:- pred apply_rec_subst_to_prog_constraints(tsubst::in, prog_constraints::in,
	prog_constraints::out) is det.

:- pred apply_rec_subst_to_prog_constraint_list(tsubst::in,
	list(prog_constraint)::in, list(prog_constraint)::out) is det.

:- pred apply_rec_subst_to_prog_constraint(tsubst::in, prog_constraint::in,
	prog_constraint::out) is det.

:- pred apply_subst_to_prog_constraints(tsubst::in, prog_constraints::in,
	prog_constraints::out) is det.

:- pred apply_subst_to_prog_constraint_list(tsubst::in,
	list(prog_constraint)::in, list(prog_constraint)::out) is det.

:- pred apply_subst_to_prog_constraint(tsubst::in, prog_constraint::in,
	prog_constraint::out) is det.

:- pred apply_variable_renaming_to_prog_constraints(map(tvar, tvar)::in,
	prog_constraints::in, prog_constraints::out) is det.

:- pred apply_variable_renaming_to_prog_constraint_list(map(tvar, tvar)::in,
	list(prog_constraint)::in, list(prog_constraint)::out) is det.

:- pred apply_variable_renaming_to_prog_constraint(map(tvar, tvar)::in,
	prog_constraint::in, prog_constraint::out) is det.

	% constraint_list_get_tvars(Constraints, TVars):
	%	return the list of type variables contained in a
	%	list of constraints
	%
:- pred constraint_list_get_tvars(list(prog_constraint)::in, list(tvar)::out)
	is det.
	
	% constraint_get_tvars(Constraint, TVars):
	%	return the list of type variables contained in a constraint.
	%
:- pred constraint_get_tvars(prog_constraint::in, list(tvar)::out) is det.

:- pred get_unconstrained_tvars(list(tvar)::in, list(prog_constraint)::in,
	list(tvar)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module std_util.
:- import_module term.

%-----------------------------------------------------------------------------%

type_is_higher_order(Type, Purity, PredOrFunc, EvalMethod, PredArgTypes) :-
	(
		Type = term.functor(term.atom(PurityName), [BaseType], _),
		purity_name(Purity0, PurityName),
		type_is_higher_order_2(BaseType,
			PredOrFunc0, EvalMethod0, PredArgTypes0)
	->
		Purity = Purity0,
		PredOrFunc = PredOrFunc0,
		EvalMethod = EvalMethod0,
		PredArgTypes = PredArgTypes0
	;
		Purity = (pure),
		type_is_higher_order_2(Type,
			PredOrFunc, EvalMethod, PredArgTypes)
	).

% This parses a higher-order type without any purity indicator.
:- pred type_is_higher_order_2((type)::in, pred_or_func::out,
	lambda_eval_method::out, list(type)::out) is semidet.

type_is_higher_order_2(Type, PredOrFunc, EvalMethod, PredArgTypes) :-
	(
		Type = term.functor(term.atom("="),
			[FuncEvalAndArgs, FuncRetType], _)
	->
		get_lambda_eval_method_and_args("func", FuncEvalAndArgs,
			EvalMethod, FuncArgTypes),
		list.append(FuncArgTypes, [FuncRetType], PredArgTypes),
		PredOrFunc = function
	;
		get_lambda_eval_method_and_args("pred",
			Type, EvalMethod, PredArgTypes),
		PredOrFunc = predicate
	).

	% From the type of a lambda expression, work out how it should
	% be evaluated and extract the argument types.
:- pred get_lambda_eval_method_and_args(string::in, (type)::in,
	lambda_eval_method::out, list(type)::out) is semidet.

get_lambda_eval_method_and_args(PorFStr, Type0, EvalMethod, ArgTypes) :-
	Type0 = term.functor(term.atom(Functor), Args, _),
	( Functor = PorFStr ->
		EvalMethod = normal,
		ArgTypes = Args
	;
		Args = [Type1],
		Type1 = term.functor(term.atom(PorFStr), ArgTypes, _),
		Functor = "aditi_bottom_up",
		EvalMethod = (aditi_bottom_up)
	).

type_has_variable_arity_ctor(Type, TypeCtor, TypeArgs) :-
	(
		type_is_higher_order(Type, _Purity, PredOrFunc, _,
			TypeArgs0)
	->
		TypeArgs = TypeArgs0,
		PredOrFuncStr = prog_out.pred_or_func_to_str(PredOrFunc),
		TypeCtor = unqualified(PredOrFuncStr) - 0
	;
		type_is_tuple(Type, TypeArgs1)
	->
		TypeArgs = TypeArgs1,
		TypeCtor = unqualified("tuple") - 0
	;
		fail
	).

type_to_ctor_and_args(Type, SymName - Arity, Args) :-
	Type \= term.variable(_),

	% higher order types may have representations where
	% their arguments don't directly correspond to the
	% arguments of the term.
	(
		type_is_higher_order(Type, Purity, PredOrFunc,
			EvalMethod, PredArgTypes)
	->
		Args = PredArgTypes,
		list.length(Args, Arity0),
		adjust_func_arity(PredOrFunc, Arity, Arity0),
		(
			PredOrFunc = predicate,
			PorFStr = "pred"
		;
			PredOrFunc = function,
			PorFStr = "func"
		),
		SymName0 = unqualified(PorFStr),
		(
			EvalMethod = (aditi_bottom_up),
			insert_module_qualifier("aditi_bottom_up", SymName0,
				SymName1)
		;
			EvalMethod = normal,
			SymName1 = SymName0
		),
		(
			Purity = (pure),
			SymName = SymName1
		;
			Purity = (semipure),
			insert_module_qualifier("semipure", SymName1, SymName)
		;
			Purity = (impure),
			insert_module_qualifier("impure", SymName1, SymName)
		)
	;
		sym_name_and_args(Type, SymName, Args),

		% `private_builtin:constraint' is introduced by polymorphism,
		% and should only appear as the argument of a
		% `typeclass:info/1' type.
		% It behaves sort of like a type variable, so according to the
		% specification of `type_to_ctor_and_args', it should
		% cause failure. There isn't a definition in the type table.
		\+ (
			SymName = qualified(ModuleName, UnqualName),
			UnqualName = "constraint",
			mercury_private_builtin_module(PrivateBuiltin),
			ModuleName = PrivateBuiltin
		),
		list.length(Args, Arity)
	).

type_ctor_is_higher_order(SymName - _Arity, Purity, PredOrFunc, EvalMethod) :-
	get_purity_and_eval_method(SymName, Purity, EvalMethod, PorFStr),
	(
		PorFStr = "pred",
		PredOrFunc = predicate
	;
		PorFStr = "func",
		PredOrFunc = function
	).

:- pred get_purity_and_eval_method(sym_name::in, purity::out,
	lambda_eval_method::out, string::out) is semidet.

get_purity_and_eval_method(SymName, Purity, EvalMethod, PorFStr) :-
	(
		SymName = qualified(unqualified(Qualifier), PorFStr),
		(
			Qualifier = "aditi_bottom_up",
			EvalMethod = (aditi_bottom_up),
			Purity = (pure)
		;
			Qualifier = "impure",
			Purity = (impure),
			EvalMethod = normal
		;
			Qualifier = "semipure",
			Purity = (semipure),
			EvalMethod = normal
		)
	;
		SymName = unqualified(PorFStr),
		EvalMethod = normal,
		Purity = (pure)
	).

type_is_tuple(Type, ArgTypes) :-
	type_to_ctor_and_args(Type, TypeCtor, ArgTypes),
	type_ctor_is_tuple(TypeCtor).

type_ctor_is_tuple(unqualified("{}") - _).

type_ctor_is_variable(unqualified("") - _).

prog_type.var(term.variable(Var), Var).

prog_type.vars(Type, Tvars) :-
	term.vars(Type, Tvars).

construct_type(TypeCtor, Args, Type) :-
	(
		type_ctor_is_higher_order(TypeCtor, Purity, PredOrFunc,
			EvalMethod)
	->
		construct_higher_order_type(Purity, PredOrFunc, EvalMethod,
			Args, Type)
	;
		TypeCtor = SymName - _,
		construct_qualified_term(SymName, Args, Type)
	).

construct_higher_order_type(Purity, PredOrFunc, EvalMethod, ArgTypes, Type) :-
	(
		PredOrFunc = predicate,
		construct_higher_order_pred_type(Purity, EvalMethod, ArgTypes,
			Type)
	;
		PredOrFunc = function,
		pred_args_to_func_args(ArgTypes, FuncArgTypes, FuncRetType),
		construct_higher_order_func_type(Purity, EvalMethod,
			FuncArgTypes, FuncRetType, Type)
	).

construct_higher_order_pred_type(Purity, EvalMethod, ArgTypes, Type) :-
	construct_qualified_term(unqualified("pred"),
		ArgTypes, Type0),
	qualify_higher_order_type(EvalMethod, Type0, Type1),
	Type = add_purity_annotation(Purity, Type1).

construct_higher_order_func_type(Purity, EvalMethod, ArgTypes, RetType, Type) :-
	construct_qualified_term(unqualified("func"), ArgTypes, Type0),
	qualify_higher_order_type(EvalMethod, Type0, Type1),
	Type2 = term.functor(term.atom("="), [Type1, RetType],
			term.context_init),
	Type = add_purity_annotation(Purity, Type2).

:- func add_purity_annotation(purity, (type)) = (type).

add_purity_annotation(Purity, Type0) = Type :-
	(
		Purity = (pure),
		Type = Type0
	;
		Purity = (semipure),
		Type = term.functor(term.atom("semipure"), [Type0],
			term.context_init)
	;
		Purity = (impure),
		Type = term.functor(term.atom("impure"), [Type0],
			term.context_init)
	).

:- pred qualify_higher_order_type(lambda_eval_method::in, (type)::in,
	(type)::out) is det.

qualify_higher_order_type(normal, Type, Type).
qualify_higher_order_type((aditi_bottom_up), Type0,
	term.functor(term.atom("aditi_bottom_up"), [Type0], Context)) :-
	term.context_init(Context).

strip_builtin_qualifiers_from_type(Type0, Type) :-
	( type_to_ctor_and_args(Type0, TypeCtor0, Args0) ->
		strip_builtin_qualifiers_from_type_list(Args0, Args),
		TypeCtor0 = SymName0 - Arity,
		(
			SymName0 = qualified(Module, Name),
			mercury_public_builtin_module(Module)
		->
			SymName = unqualified(Name)
		;
			SymName = SymName0
		),
		construct_type(SymName - Arity, Args, Type)
	;
		Type = Type0
	).

strip_builtin_qualifiers_from_type_list(Types0, Types) :-
	list__map(strip_builtin_qualifiers_from_type, Types0, Types).

%-----------------------------------------------------------------------------%

apply_rec_subst_to_prog_constraints(Subst, Constraints0, Constraints) :-
	Constraints0 = constraints(UnivCs0, ExistCs0),
	apply_rec_subst_to_prog_constraint_list(Subst, UnivCs0, UnivCs),
	apply_rec_subst_to_prog_constraint_list(Subst, ExistCs0, ExistCs),
	Constraints = constraints(UnivCs, ExistCs).

apply_rec_subst_to_prog_constraint_list(Subst, !Constraints) :-
	list__map(apply_rec_subst_to_prog_constraint(Subst), !Constraints).

apply_rec_subst_to_prog_constraint(Subst, Constraint0, Constraint) :-
	Constraint0 = constraint(ClassName, Types0),
	term__apply_rec_substitution_to_list(Types0, Subst, Types),
	Constraint  = constraint(ClassName, Types).

apply_subst_to_prog_constraints(Subst,
		constraints(UniversalCs0, ExistentialCs0),
		constraints(UniversalCs, ExistentialCs)) :-
	apply_subst_to_prog_constraint_list(Subst, UniversalCs0, UniversalCs),
	apply_subst_to_prog_constraint_list(Subst, ExistentialCs0,
		ExistentialCs).

apply_subst_to_prog_constraint_list(Subst, !Constraints) :-
	list__map(apply_subst_to_prog_constraint(Subst), !Constraints).

apply_subst_to_prog_constraint(Subst, Constraint0, Constraint) :-
	Constraint0 = constraint(ClassName, Types0),
	term__apply_substitution_to_list(Types0, Subst, Types),
	Constraint  = constraint(ClassName, Types).

apply_variable_renaming_to_prog_constraints(Renaming, Constraints0,
		Constraints) :-
	Constraints0 = constraints(UnivConstraints0, ExistConstraints0),
	apply_variable_renaming_to_prog_constraint_list(Renaming,
		UnivConstraints0, UnivConstraints),
	apply_variable_renaming_to_prog_constraint_list(Renaming,
		ExistConstraints0, ExistConstraints),
	Constraints = constraints(UnivConstraints, ExistConstraints).

apply_variable_renaming_to_prog_constraint_list(Renaming, !Constraints) :-
	list.map(apply_variable_renaming_to_prog_constraint(Renaming),
		!Constraints).

apply_variable_renaming_to_prog_constraint(Renaming, !Constraint) :-
	!.Constraint = constraint(ClassName, ClassArgTypes0),
	term.apply_variable_renaming_to_list(ClassArgTypes0, Renaming,
		ClassArgTypes),
	!:Constraint = constraint(ClassName, ClassArgTypes).

constraint_list_get_tvars(Constraints, TVars) :-
	list.map(constraint_get_tvars, Constraints, TVarsList),
	list.condense(TVarsList, TVars).

constraint_get_tvars(constraint(_Name, Args), TVars) :-
	term.vars_list(Args, TVars).

get_unconstrained_tvars(Tvars, Constraints, Unconstrained) :-
	constraint_list_get_tvars(Constraints, ConstrainedTvars),
	list.delete_elems(Tvars, ConstrainedTvars, Unconstrained0),
	list.remove_dups(Unconstrained0, Unconstrained).

%-----------------------------------------------------------------------------%
:- end_module prog_type.
%-----------------------------------------------------------------------------%
