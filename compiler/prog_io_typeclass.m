%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2001 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_typeclass.m.
% Main authors: dgj.
%
% This module handles the parsing of typeclass declarations.
% Perhaps some of this should go into prog_io_util.m?

:- module prog_io_typeclass.

:- interface.

:- import_module prog_data, prog_io_util.
:- import_module list, varset, term.

	% parse a typeclass declaration. 
:- pred parse_typeclass(module_name, varset, list(term), maybe1(item)).
:- mode parse_typeclass(in, in, in, out) is semidet.

	% parse an instance declaration. 
:- pred parse_instance(module_name, varset, list(term), maybe1(item)).
:- mode parse_instance(in, in, in, out) is semidet.

	% parse a list of class constraints
:- pred parse_class_constraints(module_name, term,
		maybe1(list(class_constraint))).
:- mode parse_class_constraints(in, in, out) is det.

:- implementation.

:- import_module prog_io, prog_io_goal, prog_util, hlds_pred.
:- import_module term, varset.
:- import_module int, string, std_util, require, type_util, set.

parse_typeclass(ModuleName, VarSet, TypeClassTerm, Result) :-
		%XXX should return an error if we get more than one arg,
		%XXX rather than failing.
	TypeClassTerm = [Arg],
	(
		Arg = term__functor(term__atom("where"), [Name, Methods], _)
	->
		parse_non_empty_class(ModuleName, Name, Methods, VarSet,
			Result)
	;
		parse_class_name(ModuleName, Arg, VarSet, Result)
	).

:- pred parse_non_empty_class(module_name, term, term, varset, maybe1(item)).
:- mode parse_non_empty_class(in, in, in, in, out) is det.

parse_non_empty_class(ModuleName, Name, Methods, VarSet, Result) :-
	varset__coerce(VarSet, TVarSet),
	parse_class_methods(ModuleName, Methods, VarSet, ParsedMethods),
	(
		ParsedMethods = ok(MethodList),
		parse_class_name(ModuleName, Name, VarSet, ParsedNameAndVars),
		(
			ParsedNameAndVars = error(String, Term)
		->
			Result = error(String, Term)
		;
			ParsedNameAndVars = ok(typeclass(Constraints,
				NameString, Vars, _, _))
		->
			Result = ok(typeclass(Constraints, NameString, Vars,
				concrete(MethodList), TVarSet))
		;
				% if the item we get back isn't a typeclass,
				% something has gone wrong...
			error("prog_io_typeclass.m: item should be a typeclass")
		)
	;
		ParsedMethods = error(String, Term),
		Result = error(String, Term)
	).

:- pred parse_class_name(module_name, term, varset, maybe1(item)).
:- mode parse_class_name(in, in, in, out) is det.

parse_class_name(ModuleName, Arg, VarSet, Result) :-
	(
		Arg = term__functor(term__atom("<="), [Name, Constraints], _)
	->
		parse_constrained_class(ModuleName, Name, Constraints, VarSet,
			Result)
	;
		varset__coerce(VarSet, TVarSet),
		parse_unconstrained_class(ModuleName, Arg, TVarSet, Result)
	).

:- pred parse_constrained_class(module_name, term, term, varset, maybe1(item)).
:- mode parse_constrained_class(in, in, in, in, out) is det.

parse_constrained_class(ModuleName, Decl, Constraints, VarSet, Result) :-
	varset__coerce(VarSet, TVarSet),
	parse_superclass_constraints(ModuleName, Constraints,
		ParsedConstraints),
	(
		ParsedConstraints = ok(ConstraintList),
		parse_unconstrained_class(ModuleName, Decl, TVarSet, Result0),
		(
			Result0 = error(_, _)
		->
			Result = Result0
		;
			Result0 = ok(typeclass(_, Name, Vars, Interface, 
				VarSet0))
		->
			(
				%
				% check for type variables in the constraints
				% which do not occur in the type class
				% parameters 
				% 
				type_util__constraint_list_get_tvars(
					ConstraintList, ConstrainedVars),
				list__member(Var, ConstrainedVars),
				\+ list__member(Var, Vars)
			->
				Result = error(
"type variable in superclass constraint is not a parameter of this type class",
					Constraints)
			;
				Result = ok(typeclass(ConstraintList, Name,
					Vars, Interface, VarSet0))
			)
		;
				% if the item we get back isn't a typeclass,
				% something has gone wrong...
			error("prog_io_typeclass.m: item should be a typeclass")
		)
	;
		ParsedConstraints = error(String, Term),
		Result = error(String, Term)
	).

:- pred parse_superclass_constraints(module_name, term,
		maybe1(list(class_constraint))).
:- mode parse_superclass_constraints(in, in, out) is det.

parse_superclass_constraints(ModuleName, Constraints, Result) :-
	parse_simple_class_constraints(ModuleName, Constraints, 
		"constraints on class declaration may only constrain type variables, not compound types",
		Result).

:- pred parse_unconstrained_class(module_name, term, tvarset, maybe1(item)).
:- mode parse_unconstrained_class(in, in, in, out) is det.


parse_unconstrained_class(ModuleName, Name, TVarSet, Result) :-
	parse_implicitly_qualified_term(ModuleName,
		Name, Name, "typeclass declaration", MaybeClassName),
	(
		MaybeClassName = ok(ClassName, TermVars0),
		list__map(term__coerce, TermVars0, TermVars),
		(
			term__var_list_to_term_list(Vars, TermVars),
			list__sort_and_remove_dups(TermVars, SortedTermVars),
			list__length(SortedTermVars) =
				list__length(TermVars) `with_type` int
		->
			Result = ok(typeclass([], ClassName, Vars,
					abstract, TVarSet))
		;
			Result = error(
			"expected distinct variables as class parameters",
				Name)
		)
	;
		MaybeClassName = error(String, Term),
		Result = error(String, Term)
	).

:- pred parse_class_methods(module_name, term, varset,
		maybe1(list(class_method))).
:- mode parse_class_methods(in, in, in, out) is det.

parse_class_methods(ModuleName, Methods, VarSet, Result) :-
	(
		list_term_to_term_list(Methods, MethodList)
			% Convert the list of terms into a list of 
			% maybe1(class_method)s.
	->
		list__map(lambda([MethodTerm::in, Method::out] is det, 
			(
				% Turn the term into an item
			parse_decl(ModuleName, VarSet, MethodTerm, Item),
				% Turn the item into a class_method
			item_to_class_method(Item, MethodTerm, Method)
			)),
			MethodList,
			Interface),
		find_errors(Interface, Result)
	;
		Result = error("expected list of class methods", Methods)
	).

:- pred list_term_to_term_list(term, list(term)).
:- mode list_term_to_term_list(in, out) is semidet.

list_term_to_term_list(Methods, MethodList) :-
	(
		Methods = term__functor(term__atom("[|]"), [Head, Tail0], _),
		list_term_to_term_list(Tail0, Tail),
		MethodList = [Head|Tail]
	;
		Methods = term__functor(term__atom("[]"), [], _),
		MethodList = []
	).


:- pred item_to_class_method(maybe2(item, prog_context), term, 
	maybe1(class_method)).
:- mode item_to_class_method(in, in, out) is det.

item_to_class_method(error(String, Term), _, error(String, Term)).
item_to_class_method(ok(Item, Context), Term, Result) :-
	(
		Item = pred_or_func(A, B, C, D, E, F, G, H, I, J)
	->
		Result = ok(pred_or_func(A, B, C, D, E, F, G, H, I, J, Context))
	;
		Item = pred_or_func_mode(A, B, C, D, E, F)
	->
		Result = ok(pred_or_func_mode(A, B, C, D, E, F, Context))
	;
		Result = error("Only pred, func and mode declarations allowed in class interface", Term)
	).

	% from a list of maybe1s, search through until you find an error.
	% If an error is found, return it.
	% If no error is found, return ok(the original elements).
:- pred find_errors(list(maybe1(T)), maybe1(list(T))).
:- mode find_errors(in, out) is det.

find_errors([], ok([])).
find_errors([X|Xs], Result) :-
	(
		X = ok(Method),
		find_errors(Xs, Result0),
		(
			Result0 = ok(Methods),
			Result = ok([Method|Methods])
		;
			Result0 = error(String, Term),
			Result = error(String, Term)
		)
	;
		X = error(String, Term),
		Result = error(String, Term)
	).

%-----------------------------------------------------------------------------%

% Parse constraints on a pred or func declaration,
% or on an existentially quantified type definition.

parse_class_constraints(ModuleName, Constraints, Result) :-
	parse_simple_class_constraints(ModuleName, Constraints, 
		"sorry, not implemented: constraints may only constrain type variables, not compound types",
		Result).

% Parse constraints which can only constrain type variables

:- pred parse_simple_class_constraints(module_name, term, string,
		maybe1(list(class_constraint))).
:- mode parse_simple_class_constraints(in, in, in, out) is det.

parse_simple_class_constraints(ModuleName, Constraints, ErrorMessage,
		Result) :-
	parse_arbitrary_class_constraints(ModuleName, Constraints,
		ParsedConstraints),
	(
		ParsedConstraints = ok(ConstraintList),
		(
			list__member(Constraint, ConstraintList),
			Constraint = constraint(_, Types),
			list__member(Type, Types),
			\+ type_util__var(Type, _)
		->
			Result = error(ErrorMessage, Constraints)
		;
			Result = ParsedConstraints
		)
	;
		ParsedConstraints = error(_, _),
		Result = ParsedConstraints
	).

% Parse constraints which can constrain arbitrary types

:- pred parse_arbitrary_class_constraints(module_name, term,
		maybe1(list(class_constraint))).
:- mode parse_arbitrary_class_constraints(in, in, out) is det.

parse_arbitrary_class_constraints(ModuleName, Constraints, ParsedConstraints) :-
	conjunction_to_list(Constraints, ConstraintList),
	parse_class_constraint_list(ModuleName, ConstraintList, 
		ParsedConstraints).

:- pred parse_class_constraint_list(module_name, list(term),
		maybe1(list(class_constraint))).
:- mode parse_class_constraint_list(in, in, out) is det.

parse_class_constraint_list(_, [], ok([])).
parse_class_constraint_list(ModuleName, [C0|C0s], Result) :-
	parse_class_constraint(ModuleName, C0, Result0),
	(
		Result0 = ok(C),
		parse_class_constraint_list(ModuleName, C0s, Result1),
		(
			Result1 = ok(Cs),
			Result = ok([C|Cs])
		;
			Result1 = error(_, _),
			Result = Result1
		)
	;
		Result0 = error(String, Term),
		Result = error(String, Term)
	).

:- pred parse_class_constraint(module_name, term,
		maybe1(class_constraint)).
:- mode parse_class_constraint(in, in, out) is det.

parse_class_constraint(_ModuleName, Constraint, Result) :-
	(
		parse_qualified_term(Constraint, Constraint,
			"class constraint", ok(ClassName, Args0))
	->
		% we need to enforce the invariant that types in type class
		% constraints do not contain any info in their prog_context
		% fields
		list__map(convert_type, Args0, Args),
		Result = ok(constraint(ClassName, Args))
	;
		Result = error("expected atom as class name", Constraint)
	).

%-----------------------------------------------------------------------------%

parse_instance(ModuleName, VarSet, TypeClassTerm, Result) :-
		%XXX should return an error if we get more than one arg,
		%XXX rather than failing.
	TypeClassTerm = [Arg],
	varset__coerce(VarSet, TVarSet),
	(
		Arg = term__functor(term__atom("where"), [Name, Methods], _)
	->
		parse_non_empty_instance(ModuleName, Name, Methods, VarSet,
			TVarSet, Result)
	;
		parse_instance_name(ModuleName, Arg, TVarSet, Result)
	).

:- pred parse_instance_name(module_name, term, tvarset, maybe1(item)).
:- mode parse_instance_name(in, in, in, out) is det.

parse_instance_name(ModuleName, Arg, TVarSet, Result) :-
	(
		Arg = term__functor(term__atom("<="), [Name, Constraints], _)
	->
		parse_derived_instance(ModuleName, Name, Constraints,
			TVarSet, Result)
	;
		parse_underived_instance(ModuleName, Arg, TVarSet, Result)
	).

:- pred parse_derived_instance(module_name, term, term, tvarset, maybe1(item)).
:- mode parse_derived_instance(in, in, in, in, out) is det.

parse_derived_instance(ModuleName, Decl, Constraints, TVarSet,
		Result) :-
	parse_instance_constraints(ModuleName, Constraints, ParsedConstraints),
	(
		ParsedConstraints = ok(ConstraintList),
		parse_underived_instance(ModuleName, Decl, TVarSet,
			Result0),
		(
			Result0 = error(_, _)
		->
			Result = Result0
		;
			Result0 = ok(instance(_, Name, Types, Body, VarSet,
				ModName))
		->
			Result = ok(instance(ConstraintList, Name, Types, Body,
					VarSet, ModName))
		;
				% if the item we get back isn't an instance, 
				% something has gone wrong...
				% maybe we should use cleverer inst decls to
				% avoid this call to error
			error("prog_io_typeclass.m: item should be an instance")
		)
	;
		ParsedConstraints = error(String, Term),
		Result = error(String, Term)
	).

:- pred parse_instance_constraints(module_name, term,
		maybe1(list(class_constraint))).
:- mode parse_instance_constraints(in, in, out) is det.

parse_instance_constraints(ModuleName, Constraints, Result) :-
	parse_simple_class_constraints(ModuleName, Constraints,
		"constraints on instance declaration may only constrain type variables, not compound types",
		Result).

:- pred parse_underived_instance(module_name, term, tvarset, maybe1(item)).
:- mode parse_underived_instance(in, in, in, out) is det.

parse_underived_instance(ModuleName, Name, TVarSet, Result) :-
		% We don't give a default module name here since the instance
		% declaration could well be for a typeclass defined in another
		% module
	parse_qualified_term(Name, Name, "instance declaration",
		MaybeClassName),
	(
		MaybeClassName = ok(ClassName, TermTypes0),
			% check that the type in the name of the instance 
			% decl is a functor with vars as args
		list__map(convert_type, TermTypes0, TermTypes),
		IsFunctorAndVarArgs = lambda([Type::in] is semidet,
			(
					% Is the top level functor an atom?
				Type = term__functor(term__atom(Functor),
						Args, _),
				(
					Functor = ":"
				->
					Args = [_Module, Type1],
						% Is the top level functor an
						% atom?
					Type1 = term__functor(term__atom(_), 
							Args1, _),
						% Are all the args of the
						% functor variables?
					list__map(lambda([A::in, B::out] 
							is semidet, 
						type_util__var(A,B)), Args1, _)
				;
						% Are all the args of the
						% functor variables?
					list__map(lambda([A::in, B::out] 
							is semidet, 
						type_util__var(A,B)), Args, _)
				)
			)),
		list__filter(IsFunctorAndVarArgs, TermTypes, _,
			ErroneousTypes),
		(
			ErroneousTypes = [],
			Result = ok(instance([], ClassName,
				TermTypes, abstract, TVarSet, ModuleName))
		;
				% XXX We should report an error for _each_
				% XXX erroneous type
			ErroneousTypes = [E0|_Es],
			term__coerce(E0, E),
			Result = error("expected type in instance declaration to be a functor with variables as args", E)
		)
	;
		MaybeClassName = error(String, Term),
		Result = error(String, Term)
	).

:- pred parse_non_empty_instance(module_name, term, term, varset, tvarset,
		maybe1(item)).
:- mode parse_non_empty_instance(in, in, in, in, in, out) is det.

parse_non_empty_instance(ModuleName, Name, Methods, VarSet, TVarSet, Result) :-
	parse_instance_methods(ModuleName, Methods, VarSet, ParsedMethods),
	(
		ParsedMethods = ok(MethodList),
		parse_instance_name(ModuleName, Name, TVarSet,
			ParsedNameAndTypes),
		(
			ParsedNameAndTypes = error(String, Term)
		->
			Result = error(String, Term)
		;
			ParsedNameAndTypes = ok(instance(Constraints,
				NameString, Types, _, _, ModName))
		->
			Result0 = ok(instance(Constraints, NameString, Types,
				concrete(MethodList), TVarSet, ModName)),
			check_tvars_in_instance_constraint(Result0, Name,
				Result)
		;
				% if the item we get back isn't a typeclass,
				% something has gone wrong...
			error("prog_io_typeclass.m: item should be an instance")
		)
	;
		ParsedMethods = error(String, Term),
		Result = error(String, Term)
	).

:- pred check_tvars_in_instance_constraint(maybe1(item), term, maybe1(item)).
:- mode check_tvars_in_instance_constraint(in, in, out) is det.

check_tvars_in_instance_constraint(error(M,E), _, error(M, E)).
check_tvars_in_instance_constraint(ok(Item), InstanceTerm, Result) :-
	(
		Item = instance(Constraints, _Name, Types, _Methods, _TVarSet,
			_ModName)
	->
		%
		% check that all of the type variables in the constraints
		% on the instance declaration also occur in the type class
		% argument types in the instance declaration
		%
		( 
			type_util__constraint_list_get_tvars(Constraints,
				TVars),
			list__member(TVar, TVars),
			\+ term__contains_var_list(Types, TVar)
		->
			Result = error(
	"unbound type variable(s) in constraints on instance declaration",
				InstanceTerm)
		;
			Result = ok(Item)
		)
	;
		error("check_tvars_in_constraint: expecting instance item")
	).

:- pred parse_instance_methods(module_name, term, varset,
		maybe1(list(instance_method))).
:- mode parse_instance_methods(in, in, in, out) is det.

parse_instance_methods(ModuleName, Methods, VarSet, Result) :-
	(
		list_term_to_term_list(Methods, MethodList)
	->
			% Convert the list of terms into a list of 
			% maybe1(class_method)s.
		list__map(term_to_instance_method(ModuleName, VarSet),
			MethodList, Interface),
		find_errors(Interface, Result)
	;
		Result = error("expected list of instance methods", Methods)
	).

	% Turn the term into a method instance
:- pred term_to_instance_method(module_name, varset, term,
		maybe1(instance_method)).
:- mode term_to_instance_method(in, in, in, out) is det.

term_to_instance_method(_ModuleName, VarSet, MethodTerm, Result) :-
	(
		MethodTerm = term__functor(term__atom("is"), [ClassMethodTerm,
						InstanceMethod], TermContext)
	->
		(
			ClassMethodTerm = term__functor(term__atom("pred"),
				[term__functor(
					term__atom("/"), 
					[ClassMethod, Arity], 
					_)], 
				_)
		->
			(
				parse_qualified_term(ClassMethod,
					ClassMethod, "instance method", 
					ok(ClassMethodName, [])),
				Arity = term__functor(term__integer(ArityInt), 
					[], _),
				parse_qualified_term(InstanceMethod,
					InstanceMethod, "instance method",
					ok(InstanceMethodName, []))
			->
				Result = ok(instance_method(predicate,
					ClassMethodName,
					name(InstanceMethodName),
					ArityInt, TermContext))
			;
				Result = error(
				    "expected `pred(<Name> / <Arity>) is <InstanceMethod>'",
					MethodTerm)
			)
		;
			ClassMethodTerm = term__functor(term__atom("func"),
				[term__functor(
					term__atom("/"), 
					[ClassMethod, Arity], 
					_)], 
				_)
		->
			(
				parse_qualified_term(ClassMethod,
					ClassMethod, "instance method",
					ok(ClassMethodName, [])),
				Arity = term__functor(term__integer(ArityInt), 
					[], _),
				parse_qualified_term(InstanceMethod,
					InstanceMethod, "instance method",
					ok(InstanceMethodName, []))
			->
				Result = ok(instance_method(function,
					ClassMethodName,
					name(InstanceMethodName),
					ArityInt, TermContext))
			;
				Result = error(
				    "expected `func(<Name> / <Arity>) is <InstanceMethod>'",
					MethodTerm)
			)
		;
			Result = error(
				"expected `pred(<Name> / <Arity>) is <InstanceName>'",
				MethodTerm)
		)
	;
		% For the clauses in an instance declaration,
		% the default module name for the clause heads
		% is the module name of the class that this is an
		% instance declaration for, but we don't necessarily
		% know what module that is at this point, since the
		% class name hasn't been fully qualified yet.
		% So here we give the special module name ""
		% as the default, which means that there is no default.
		% (If the module qualifiers in the clauses don't match
		% the module name of the class, we will pick that up later,
		% in check_typeclass.m.)
		DefaultModuleName = unqualified(""),
		parse_item(DefaultModuleName, VarSet, MethodTerm, Result0),
		(
			Result0 = ok(Item, Context),
			Item = clause(_VarNames, PredOrFunc,
				ClassMethodName, HeadArgs,
				_ClauseBody)
		->
			adjust_func_arity(PredOrFunc, ArityInt,
					list__length(HeadArgs)),
			Result = ok(instance_method(PredOrFunc,
					ClassMethodName,
					clauses([Item]),
					ArityInt, Context))
		;
			Result0 = error(ErrorMsg, ErrorTerm)
		->
			Result = error(ErrorMsg, ErrorTerm)
		;
			% catch-all error message for a syntactically valid item
			% which is not a clause
			Result = error("expected clause or `pred(<Name> / <Arity>) is <InstanceName>' or `func(<Name> / <Arity>) is <InstanceName>')",
				MethodTerm)
		)
	).

