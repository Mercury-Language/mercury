%-----------------------------------------------------------------------------%
%
% Main author: fjh.
%
% This file contains a type-checker which I started writing a fair while
% ago.  It still needs quite a bit of work to integrate it with
% the rest of the stuff in this directory and to get it to actually work.
% 
% The predicates in this module are named as follows:
% 
% 	Predicates that type checking a particular language
% 	construct (goal, clause, etc.) are called typecheck_*.
% 	These will eventually have to iterate over every type
% 	assignment in the type assignment set.
% 
% 	Predicates that unify two things with respect to a
% 	single type assignment, as opposed to a type assignment set
% 	are called type_assign_*.
% 
% 	Access predicates for the typeinfo data structure are called
% 	typeinfo_*.
% 
% There are four sorts of types:
%
% 1) discriminated unions:
%	:- type tree(T) ---> nil ; t(tree(T), T, tree(T)).
%
% 2) undiscriminated unions (if rhs has a single type then the two
%	types have same structure but *different* name):
%	Basically just syntactic sugar for discriminated unions,
%	except that it also works for builtin types like int.
%
%		% an undiscriminate union
%		% same as type t3 ---> a ; b ; c ; d.
%	:- type t3 = t1 + t2.
%
%	:- type t1 ---> a ; b.
%	:- type t2 ---> c ; d.
%
%		% another undiscriminated union
%	:- type number = int + float.
%
% 3) equivalent types (treated identically, ie, same name.  Any number
%	of types can be equivalent; the *canonical* one is the one
%	which is not defined using ==):
%	:- type real == float.
%
% 4) builtin types
%	char, int, float, string
%	(plus types for higher-order preds, details to be worked out later).
%       These types have special syntax for constants.
%	There may be other types (short integers, complex numbers,
%	etc.) provided by the system, but they can just be part of the
%	standard library.
%
% Each predicate must have a `:- pred' declaration specifying the
% types of the arguments for that predicate.
% 
%-----------------------------------------------------------------------------%
%  TODO:
%
% 	XXX 	we should handle explicit type qualifications
% 		(and remove them here) but we don't do so yet
%
%	XXX	we should handle equivalence types
%
%-----------------------------------------------------------------------------%

:- import_module io, bag.

:- type type_assign_set	==	list(type_assignment).

:- type type_assignment	-->	type_assign(
					varset,		% var names
					map(var, type),	% var types
					varset,		% type names
					map(type_param, type)	% type bindings
				).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred typecheck(module_info, module_info, io__state, io__state).
:- mode typecheck(input, output, di, uo).

	% XXX need to pass FoundError to all steps

typecheck(Module0, FoundError, Module) -->
	io__write_string("Checking for undefined types...\n"),
	check_undefined_types(Module0, Module1),
	io__write_string("Checking for circularly defined types...\n"),
	check_circular_types(Module1, Module2),
	io__write_string("Type-checking clauses...\n"),
	check_pred_types(Module2, FoundError, Module).

%-----------------------------------------------------------------------------%
	
	% Type-check the code for all the predicates in a module.

:- pred check_pred_types(module_info, module_info, io__state, io__state).
:- mode check_pred_types(input, output, di, uo).

typecheck_pred_types(Module, Module) -->
	{ moduleinfo_predids(Module, PredIds) },
	check_pred_types_2(PredIds, Module).

%-----------------------------------------------------------------------------%

	% Iterate over the list of pred_ids in a module.

:- pred check_pred_types_2(list(pred_id), module_info, io__state, io__state).
:- mode check_pred_types_2(input, input, di, uo).

typecheck_pred_types_2([], _ModuleInfo) --> [].
typecheck_pred_types_2([PredId | PredIds], ModuleInfo) -->
	{ map__search(Preds, PredId, PredInfo) },
	{ predinfo_argtypes(PredInfo, TypeVarSet, ArgTypes) },
	{ predinfo_clauses(Clauses0) },
	typecheck_clause_list(Clauses0, PredId, TypeVarSet, ArgTypes,
		ModuleInfo, Clauses),
	{ predinfo_set_clauses(Clauses) },
	typecheck_pred_types_2(PredIds, ModuleInfo).

%-----------------------------------------------------------------------------%

	% Iterate over the list of clauses for a predicate.

:- pred typecheck_clause_list(list(clause), pred_id, varset, list(type),
			module_info, list(clause), io__state, io__state).
:- mode typecheck_clause_list(input, input, input, input, input, input, output,
			di, uo).

typecheck_clause_list([], _PredId, _TypeVarSet, _ArgTypes, _ModuleInfo, [])
		--> [].
typecheck_clause_list([Clause0|Clauses0], PredId, TypeVarSet, ArgTypes,
		ModuleInfo, [Clause|Clauses]) -->
	typecheck_clause(Clause0, PredId, TypeVarSet, ArgTypes, ModuleInfo,
		Clause),
	typecheck_clause_list(Clauses0, PredId, TypeVarSet, ArgTypes,
		ModuleInfo, Clauses).

%-----------------------------------------------------------------------------%

	% Type-check a single clause.

	% As we go through a clause, we determine the possible
	% type assignments for the clause.  A type assignment
	% is an assignment of a type to each variable in the
	% clause.
	%
	% Note that this may cause exponential time & space usage
	% in the presence of overloading of predicates and/or
	% functors.  This is a potentially very serious problem, but
	% there's no easy solution apparent.
	%
	% It would be more natural to use non-determinism to write
	% this code, and perhaps even more efficient.
	% But doing it deterministically would make bootstrapping more
	% difficult, and most importantly would make good error
	% messages very difficult.

:- pred typecheck_clause(clause, pred_id, varset, list(type), module_info,
		clause, io__state, io__state).
:- mode typecheck_clause(input, input, input, input, input, output, di, uo).

typecheck_clause(Clause0, PredId, TypeVarSet, ArgTypes, ModuleInfo, Clause,
		FoundError, IOState0, IOState) :-

		% initialize the typeinfo
		% XXX abstract clause/6

	Clause0 = clause(Modes, VarSet, _DummyVarTypes, HeadVars, Body,
			Context),
	typeinfo_init(IOState0, ModuleInfo, PredId, TypeVarSet, VarSet,
		TypeInfo0),

		% typecheck the clause - first the head unification, and
		% then the body

	typecheck_var_has_type_list(HeadVars, ArgTypes, TypeInfo0, TypeInfo1),
	typecheck_goal(Body0, TypeInfo1, TypeInfo),

		% finish up

	typeinfo_get_type_assign_set(TypeInfo, TypeAssignSet),
	typecheck_finish_up(TypeAssignSet, TypeInfo, VarTypes),
	typeinfo_get_io_state(TypeInfo, IOState),
	Clause = clause(Modes, VarSet, VarTypes, HeadVars, Body, Context).

	% At this stage, there are three possibilities.
	% There are either zero, one, or multiple type assignments
	% for the clause.  In the first case, we have already
	% issued an error message.  In the second case, the
	% clause is type-correct.  In the third case, we have to
	% issue an error message here.

	% XXX incomplete!!

typecheck_finish_up([], _TypeInfo, VarTypes).
typecheck_finish_up([VarTypes], _TypeInfo, VarTypes) :-
typecheck_finish_up([VarTypes1, VarTypes2 | _], _TypeInfo, VarTypes) :-
	report_ambiguity_error(VarTypes1, VarTypes2).

%-----------------------------------------------------------------------------%

:- pred typecheck_goal(hlds__goal, type_info, type_info).
:- mode typecheck_goal(input, typeinfo_di, typeinfo_uo).

typecheck_goal(Goal - _GoalInfo) -->
	typecheck_goal_2(Goal).

:- pred typecheck_goal_2(hlds__goal_expr, type_info, type_info).
:- mode typecheck_goal_2(input, typeinfo_di, typeinfo_uo).

typecheck_goal_2(true) --> [].
typecheck_goal_2(fail) --> [].
typecheck_goal_2(conj(List)) -->
	typecheck_goal_list(List).
typecheck_goal_2(disj(List)) -->
	typecheck_goal_list(List).
typecheck_goal_2(if_then_else(_Vs, A, B, C)) -->
	typecheck_goal(A),
	typecheck_goal(B),
	typecheck_goal(C).
typecheck_goal_2(not(_Vs, A)) -->
	typecheck_goal(A).
typecheck_goal_2(some(_Vs, G)) -->
	typecheck_goal(G).
typecheck_goal_2(all(_Vs, G)) -->
	typecheck_goal(G).
typecheck_goal_2(call(PredId, _Mode, Args, _Builtin)) -->
	typecheck_call_pred(PredId, Args).
typecheck_goal_2(unify(A, B, _Mode, _Info)) -->
	typecheck_unify(A, B).

%-----------------------------------------------------------------------------%

:- pred typecheck_goal_list(list(hlds__goal), type_info, type_info).
:- mode typecheck_goal_list(input, typeinfo_di, typeinfo_uo).

typecheck_goal_list([]) --> [].
typecheck_goal_list([Goal | Goals]) -->
	typecheck_goal(Goal),
	typecheck_goal_list(Goals).

%-----------------------------------------------------------------------------%

:- pred typecheck_call_pred(pred_id, list(term), type_info, type_info).
:- mode typecheck_call_pred(input, input, typeinfo_di, typeinfo_uo).

	% XXX we should handle overloading of predicates

typecheck_call_pred(PredId, Args, TypeInfo0, TypeInfo) :-
		% look up the called predicate's arg types
	typeinfo_moduleinfo(TypeInfo, ModuleInfo),
	moduleinfo_preds(ModuleInfo, Preds),
	map__search(Preds, PredId, PredInfo),
	predinfo_arg_types(PredInfo, PredTypeVarSet, PredArgTypes),
		% rename apart the type variables in called
		% predicate's arg types
	typeinfo_typevarset(TypeInfo0, TypeVarSet0),
	varset__merge(TypeVarSet0, PredTypeVarSet, TypeVarSet),
	typeinfo_set_typevarset(TypeInfo0, TypeVarSet, TypeInfo1),
		% unify the types of the call arguments with the
		% called predicates' arg types
	typecheck_term_has_type_list(TypeInfo1, Args, PredArgTypes, TypeInfo).

%-----------------------------------------------------------------------------%

	% Given a list of variables and a list of types, ensure
	% that each variable has the corresponding type.

:- pred typecheck_var_has_type_list(list(var), list(type), typeinfo, typeinfo).
:- mode typecheck_var_has_type_list(input, input, input, output).

typecheck_var_has_type_list([], []) --> [].
typecheck_var_has_type_list([Var|Vars], [Type|Types]) -->
	typecheck_var_has_type(Var, Type),
	typecheck_var_has_type_list(Vars, Types).

:- pred typecheck_var_has_type(var_id, type, type_info, type_info).
:- mode typecheck_var_has_type(input, input, typeinfo_di, typeinfo_uo).

typecheck_var_has_type(VarId, Type, TypeInfo0, TypeInfo) :-
	typeinfo_get_type_assign_set(TypeInfo0, TypeAssignSet0),
	typecheck_var_has_type_2(TypeAssignSet0, VarId, Type, TypeAssignSet),
	(if
		TypeAssignSet = [],
		(not TypeAssignSet0 = [])
	then
		typeinfo_get_iostate(TypeInfo0, IOState0),
		typeinfo_get_moduleinfo(TypeInfo0, ModuleInfo),
			% XXX should improve error reporting
		report_error(VarId, Type, ModuleInfo, IOState0, IOState),
		typeinfo_set_iostate(TypeInfo0, IOState, TypeInfo1),
		typeinfo_set_found_error(TypeInfo1, true, TypeInfo2),
		typeinfo_set_type_assign_set(TypeInfo2, TypeAssignSet, TypeInfo)
	else
		typeinfo_set_type_assign_set(TypeInfo0, TypeAssignSet0,
						TypeInfo).
	).

:- pred typecheck_var_has_type_2(type_assign_set, var_id, type,
				type_assign_set).
:- mode typecheck_var_has_type_2(input, input, input, output).

typecheck_var_has_type_2([], _, _, []).
typecheck_var_has_type_2([TypeAssign0 | TypeAssignSet0], VarId, Type,
		Result) :-
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	(if some [VarType]
		map__search(VarTypes0, VarId, VarType)
	then
		if some [TypeAssign]
			type_unify(TypeAssign0, VarType, Type, TypeAssign)
		then
			Result = [TypeAssign | TypeAssignSet])
		else
			Result = TypeAssignSet
		)
	else
		map__set(VarTypes0, VarId, Type, VarTypes),
		type_assign_set_vartypes(TypeAssign0, VarTypes, TypeAssign),
		Result = [TypeAssign | TypeAssignSet])
	),
	typeinfo_set_2(TypeAssignSet0, VarId, Type, TypeAssignSet).

%-----------------------------------------------------------------------------%
	
:- pred typecheck_term_has_type_list(list(term), list(type), type_info,
					type_info).
:- mode typecheck_term_has_type_list(input, input, input, typeinfo_di,
					typeinfo_uo).

typecheck_term_has_type_list([], []) --> [].
typecheck_term_has_type_list([Arg | Args], [Type | Types]) -->
	typecheck_term_has_type(Arg, Type),
	typecheck_term_has_type_list(Args, Types).

:- pred typecheck_term_has_type(term, type, type_info, type_info).
:- mode typecheck_term_has_type(input, input, input, typeinfo_di, typeinfo_uo).

typecheck_term_has_type(Term, Type, TypeInfo0, TypeInfo) :-
	XXX.

%-----------------------------------------------------------------------------%

	% Type check a unification.
	% Get the type assignment set from the type info and then just
	% iterate over all the possible type assignments.

:- pred typecheck_unification(term, term, type_info, type_info).
:- mode typecheck_unification(input, input, typeinfo_di, typeinfo_uo).

typecheck_unification(X, Y, TypeInfo0, TypeInfo) :-
	typeinfo_get_type_assign_set(TypeInfo0, TypeAssignSet0),
	typecheck_unification_2(TypeAssignSet0, X, Y, [], TypeAssignSet),
	typeinfo_set_type_assign_set(TypeInfo0, TypeAssignSet, TypeInfo).


	% iterate over all the possible type assignments.

:- pred typecheck_unification_2(type_assign_set, term, term, type_assign_set,
					type_assign_set).
:- mode typecheck_unification_2(input, input, input, input, output).

typecheck_unification_2([], _, _, TypeAssignSet, TypeAssignSet).
typecheck_unification_2([TypeAssign0 | TypeAssigns0], X, Y, TypeAssignSet0,
		TypeAssignSet) :-
	type_assign_unify_term(TypeAssign0, X, Y, TypeAssignSet0,
		TypeAssignSet1),
	typecheck_unification_2(TypeAssigns0, X, Y, TypeAssignSet1,
		TypeAssignSet).
	
%-----------------------------------------------------------------------------%

	% Type-check the unification of two terms,
	% and update the type assignment.
	% TypeAssign0 is the type assignment we are updating,
	% TypeAssignSet0 is an accumulator for the list of possible
	% type assignments so far, and TypeAssignSet is TypeAssignSet plus
	% any type assignment(s) resulting from TypeAssign0 and this
	% unification.

:- pred type_assign_unify_term(term, term, type_assign, type_assign_set,
					type_assign_set).
:- mode type_assign_unify_term(input, input, input, output).

type_assign_unify_term(term_variable(X), term_variable(Y), TypeAssign0,
		TypeAssignSet0, TypeAssignSet) :-
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	(if some [TypeX]
		map__search(VarTypes0, X, TypeX)
	then
		(if some [TypeY]
			map__search(VarTypes0, Y, TypeY)
		then
			% both X and Y already have types - just
			% unify their types
			(if some [TypeAssign]
				type_assign_unify_type(TypeAssign0, TypeX,
					TypeY, TypeAssign),
			then
				TypeAssignSet = [TypeAssign | TypeAssignSet0]
			else
				TypeAssignSet = TypeAssignSet0
			)
		else
			% Y is a fresh variable which hasn't been
			% assigned a type yet
			map__set(VarTypes0, Y, TypeX, VarTypes),
			type_assign_set_var_types(TypeAssign0, VarTypes,
				TypeAssign),
			TypeAssignSet = [TypeAssign | TypeAssignSet0].
	else
		(if some [TypeY]
			map__search(VarTypes0, Y, TypeY)
		then
			% X is a fresh variable which hasn't been
			% assigned a type yet
			map__set(VarTypes0, X, TypeY, VarTypes),
			type_assign_set_var_types(TypeAssign0, VarTypes,
				TypeAssign),
			TypeAssignSet = [TypeAssign | TypeAssignSet0]
		else
			% both X and Y are fresh variables -
			% introduce a fresh type variable to represent
			% their type
			type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
			varset_new_var(TypeVarSet0, TypeVar, Type, TypeVarSet),
			type_assign_set_typevarset(TypeAssign0, TypeVarSet,
				TypeAssign1),
			map__set(VarTypes0, X, term_variable(Type), VarTypes1),
			map__set(VarTypes1, Y, term_variable(Type), VarTypes),
			type_assign_set_var_types(TypeAssign1, VarTypes,
				TypeAssign),
			TypeAssignSet = [TypeAssign | TypeAssignSet0]
		)
	).

type_assign_unify_term(term_functor(F, As, _), term_variable(Y), TypeAssign0,
		TypeAssignSet0, TypeInfo, TypeAssignSet) :-
	length(As, Arity),
	typeinfo_get_ctors(TypeInfo, Ctors),
	(if some [ConsDefnList0]
		map__search(Ctors, cons(F, Arity), ConsDefnList)
	then
		ConsDefnList = ConsDefnList0
	else
		ConsDefnList = []
	),
	type_assign_unify_var_functor(ConsDefnList, Args, Y, TypeAssign0,
		TypeAssignSet0, TypeAssignSet).

type_assign_unify_term(term_variable(Y), term_functor(F, As, _), TypeAssign0,
		TypeAssignSet0, TypeInfo, TypeAssignSet) :-
	type_assign_unify_term(term_functor(F, As, _), term_variable(Y),
		TypeAssign0, TypeAssignSet0, TypeInfo, TypeAssignSet).
	
type_assign_unify_term(term_functor(FX, AsX, _), term_functor(FY, AsY, _),
		TypeAssign0, TypeAssignSet0, TypeInfo, TypeAssignSet) :-
	length(AsX, ArityX),
	length(AsY, ArityY),
	if
		FX = FY,
		ArityX = ArityY
	then
		% XXX should check that the functors are actually of
		% some type and that the types of the args matches what
		% it is supposed to be - instead we just check
		% that the types of the args are compatible with each other
		typecheck_unify_list(AsX, AsY, TypeAssign0, TypeAssignSet0,
			TypeInfo, TypeAssignSet)
	else
		
		
		XXX.
	

%-----------------------------------------------------------------------------%

	% Type-check the unification of a variable with a functor:
	% for each possible type of the constructor,
	% unify the type of the variable with the type of
	% the constructor and if this succeeds insert that
	% type assignment into the type assignment set.

:- pred type_assign_unify_var_functor(list(hlds__cons_defn), list(term),
		variable, type_assign, type_assign_set, type_assign_set).
:- mode type_assign_unify_var_functor(input, input, input, input, input,
		output).

type_assign_unify_var_functor([], _, _, _, TypeAssignSet, TypeAssignSet).
type_assign_unify_var_functor([ConsDefn | ConsDefns], Args, Y, TypeAssign0,
		TypeAssignSet0, TypeAssignSet) :-
	
	ConsDefn = hlds__cons_defn(ArgTypes, TypeId, _Context),

		% construct the type of this constructor
	type_asssign_get_typevarset(TypeAssign0, TypeVarSet0),
	type_id_to_type(TypeId, TypeVarSet0, ConsType, TypeVarSet),
	type_asssign_set_typevarset(TypeAssign0, TypeVarSet, TypeAssign1),

		% unify the type of Var with the type of the constructor
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	(if some [TypeY]
		map__search(VarTypes0, Y, TypeY)
	then
		type_unify_assign(TypeAssign1, ConsType, TypeY, TypeAssign2)
	else
		map__set(VarTypes0, Y, Type, VarTypes),
		type_assign_set_var_types(TypeAssign1, VarTypes, TypeAssign2)
	),

		% check that the types of the arguments matches the
		% specified arg types for this constructor
	type_unify_assign_list(Args, ArgTypes, TypeAssign2, TypeAssign3)
		% l(term), l(type), t_a, t_a

		XXX.


%-----------------------------------------------------------------------------%

	% Given a type id (such as list/1), construct a type term
	% (such as list(T)) corresponding to that type.

:- pred type_id_to_type(type_id, varset, type, varset).
:- mode type_id_to_type(input, input, output, output).

type_id_to_type(qualified(_Module, Name) - Arity, TypeVarSet0, Type,
		TypeVarSet) :-
	Const = term_atom(Name),
	make_n_fresh_var_terms(TypeVarSet0, Arity, TypeVarSet, TypeArgs),
	Type = term_functor(Const, TypeArgs).

type_id_to_type(unqualified(Name) - Arity, TypeVarSet0, Type, TypeVarSet) :-
	Const = term_atom(Name),
	make_n_fresh_var_terms(TypeVarSet0, Arity, TypeVarSet, TypeArgs),
	Type = term_functor(Const, TypeArgs).

%-----------------------------------------------------------------------------%

:- pred make_n_fresh_var_terms(varset, int, varset, list(term)).
:- mode make_n_fresh_var_terms(input, input, output, output).

make_n_fresh_var_terms(VarSet0, N, VarSet, VarTerms) :-
	(if
		N = 0
	then
		VarTerms = []
	else
		N1 is N - 1,
		varset_new_var(VarSet0, VarId, VarSet1),
		VarTerms = [term_variable(VarId) | TermVars1],
		make_n_fresh_var_terms(VarSet1, N1, VarSet, VarTerms1)
	).

%-----------------------------------------------------------------------------%

	% Unify (with occurs check) two types in a type assignment 
	% and update the type bindings.

:- pred type_assign_unify_type(type_assign, type, type, type_assign).
:- mode type_assign_unify_type(input, input, input, output).

type_assign_unify_type(TypeAssign0, X, Y, TypeAssign) :-
	type_assign_get_type_bindings(TypeAssign0, TypeBindings0),
	type_unify(X, Y, TypeBindings0, TypeBindings),
	type_assign_set_type_bindings(TypeAssign0, TypeBindings, TypeAssign).

%-----------------------------------------------------------------------------%

	% Unify (with occurs check) two types with respect to a type substition
	% and update the type bindings.
	% (Types are represented as terms, but we can't just use term__unify
	% because we need to handle equivalent types).

:- type_unify(X, Y, _, _) when X and Y.		% NU-Prolog indexing

:- pred type_unify(type, type, substition, substition).
:- mode type_unify(input, input, input, output).

type_unify(term_variable(X), term_variable(Y), Bindings0, Bindings) :-
	(if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	then
		(if some [TypeY]
			map__search(Bindings0, Y, BindingOfY)
		then
			% both X and Y already have bindings - just
			% unify the terms they are bound to
			type_unify(Bindings0, BindingOfX, BindingOfY, Bindings)
		else
			% Y is a variable which hasn't been bound yet
			not type_occurs(BindingOfX, Y, Bindings0),
			map__set(Bindings0, Y, BindingOfX, Bindings),
		)
	else
		(if some [TypeY]
			map__search(Bindings0, Y, BindingOfY)
		then
			% X is a variable which hasn't been bound yet
			not type_occurs(BindingOfY, X, Bindings0),
			map__set(Bindings0, X, BindingOfY, Bindings),
		else
			% both X and Y are unbound variables -
			% bind one to the other
			(if X = Y then
				true
			else
				map__set(Bindings0, X, term_variable(Y),
					Bindings)
			)
		)
	).

type_unify(term_variable(X), term_functor(F, As, C), Bindings0, Bindings) :-
	(if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	then
		type_unify(BindingOfX, term_functor(F, As, C), Bindings0,
			Bindings)
	else
		not type_occurs_list(As, X, Bindings0),
		map__set(Bindings0, X, term_functor(F, As, C), Bindings)
	).

type_unify(term_functor(F, As, C), term_variable(X), Bindings0, Bindings) :-
	(if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	then
		not type_occurs_list(As, X, Bindings0),
		type_unify(term_functor(F, As, C), BindingOfX, Bindings0,
			Bindings)
	else
		map__set(Bindings0, X, term_functor(F, As, C), Bindings)
	).

type_unify(term_functor(FX, AsX, _), term_functor(FY, AsY, _), Bindings0,
		Bindings) :-
	length(AsX, ArityX),
	length(AsY, ArityY),
	(if
		FX = FX,
		ArityX = ArityY
	then
		type_unify_list(AsX, AsY)
	else
		% XXX check if these types have been defined to be
		% equivalent using equivalence types
		fail	% XXX stub only!!!
	).

:- pred type_unify_list(list(type), list(type), substition, substition).
:- mode type_unify_list(input, input, input, output).

type_unify_list([], []) --> [].
type_unify_list([X | Xs], [Y | Ys]) -->
	type_unify(X, Y),
	type_unify_list(Xs, Ys).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% XXX - At the moment we don't check for circular types.
	% (If they aren't used, the compiler will probably not
	% detect the error; if they are, it will probably go into
	% an infinite loop).

:- pred check_circular_types(module_info, module_info, io__state, io__state).
:- mode check_circular_types(input, output, di, uo).

check_circular_types(Module0, Module) -->
	{ Module = Module0 }.

/**** JUNK
	{ moduleinfo_types(Module0, Types0 },
	{ map__keys(Types0, TypeIds) },
	check_circular_types_2(TypeIds, Types0, Types),
	{ moduleinfo_set_types(Module0, Types, Module) }.

check_circular_types_2([], Types, Types) --> [].
check_circular_types_2([TypeId | TypeIds], Types0, Types) -->

JUNK ****/
	

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Check for any possible undefined types.
	% XXX should we add a definition for undefined types?

:- pred check_undefined_types(module_info, io__state, io__state).
:- mode check_undefined_types(input, di, uo).
check_undefined_types(Module, Module) -->
	{ moduleinfo_types(Module, TypeDefns) },
	{ map__keys(TypeDefns, TypeIds) },
	find_undef_type_bodies(TypeIds, TypeDefns),
	{ moduleinfo_preds(Module, Preds) },
	{ map__keys(Preds, PredIds) },
	find_undef_pred_types(PredIds, Preds, TypeDefns).

	% Find any undefined types used in `:- pred' declarations.

:- pred find_undef_pred_types(list(pred_id), pred_table, type_table,
				io__state, io__state).
:- mode find_undef_pred_types(input, input, input, di, uo).

find_undef_pred_types([], _Preds, _TypeDefns) --> [].
find_undef_pred_types([PredId | PredIds], Preds, TypeDefns) -->
	{ map__search(Preds, PredId, PredDefn) },
	{ predinfo_arg_types(PredDefn, _VarSet, ArgTypes) },
	find_undef_type_list(ArgTypes, pred(PredId), TypeDefns),
	find_undef_pred_types(PredIds, Preds, TypeDefns).

	% Find any undefined types used in the bodies of other type
	% declarations.

:- pred find_undef_type_bodies(list(type_id), type_table, io__state, io__state).
:- mode find_undef_type_bodies(input, input, di, uo).

find_undef_type_bodies([], _) --> [].
find_undef_type_bodies([TypeId | TypeIds], TypeDefns) -->
	{ map__search(TypeDefns, TypeId, HLDS_TypeDefn) },
		% XXX abstract hlds__type_defn/4
	{ HLDS_TypeDefn = hlds__type_defn(_, _, TypeBody, _, _) },
	find_undef_type_body(TypeBody, type(TypeId), TypeDefns),
	find_undef_type_bodies(TypeIds, TypeDefns).

	% Find any undefined types used in the given type definition.

:- pred find_undef_type_body(hlds__type_body, context, type_table,
				io__state, io__state).
:- mode find_undef_type_body(input, input, input, di, uo).

find_undef_type_body(eqv_type(Type), Context, TypeDefns) -->
	find_undef_type(Type, Context, TypeDefns).
find_undef_type_body(uu_type(Types), Context, TypeDefns) -->
	find_undef_type_list(Types, Context, TypeDefns).
find_undef_type_body(du_type(Constructors), Context, TypeDefns) -->
	find_undef_type_du_body(Constructors, Context, TypeDefns).

	% Find any undefined types in a list of types.

:- pred find_undef_type_list(list(type), context, type_table,
				io__state, io__state).
:- mode find_undef_type_list(input, input, input, di, uo).

find_undef_type_list([], _Context, _TypeDefns) --> [].
find_undef_type_list([Type|Types], Context, TypeDefns) -->
	find_undef_type(Type, Context, TypeDefns),
	find_undef_type_list(Types, Context, TypeDefns).

	% Find any undefined types in a list of contructors
	% (the constructors for a discrimiated union type).

:- pred find_undef_type_du_body(list(constructor), context, type_table,
				io__state, io__state).
:- mode find_undef_type_du_body(input, input, input, di, uo).

find_undef_type_du_body([], _Context, _TypeDefns) --> [].
find_undef_type_du_body([Constructor | Constructors], Context, TypeDefns) -->
	{ Constructor = _Functor - ArgTypes },
	find_undef_type_list(ArgTypes, Context, TypeDefns),
	find_undef_type_du_body(Constructors, Context, TypeDefns).

	% Find any undefined types used in type.
	% The type itself may be undefined, and also
	% any type arguments may also be undefined.
	% (eg. the type `undef1(undef2, undef3)' should generate 3 errors.)

:- pred find_undef_type(type, context, type_table,
				io__state, io__state).
:- mode find_undef_type(input, input, input, di, uo).

find_undef_type(term_variable(_), _Context, _TypeDefns) --> [].
find_undef_type(term_functor(F, As, _), Context, TypeDefns) -->
	{ length(As, Arity) },
	{ make_type_id(F, Arity, TypeId) },
	(if
		{ not map__contains(TypeDefns, TypeId), 
		  not is_builtin_type(TypeId)
		}
	then
		write_undef_type(TypeId, Context)
	else
		[]
	),
	find_undef_type_list(As, Context, TypeDefns).

%-----------------------------------------------------------------------------%

	% Given a constant and an arity, return a type_id.
	% XXX this should take a name and an arity;
	% use of integers/floats/strings as type names should
	% be rejected by the parser in prog_io.nl, not here.

:- pred make_type_id(const, int, type_id).
:- mode make_type_id(input, input, output).

make_type_id(term_atom(Name), Arity, unqualified(Name) - Arity).
make_type_id(term_integer(_), _, "<error>" - 0) :-
	error("atom expected").
make_type_id(term_float(_), _, "<error>" - 0) :-
	error("atom expected").
make_type_id(term_string(_), _, "<error>" - 0) :-
	error("atom expected").

%-----------------------------------------------------------------------------%

:- type context ---> type(type_id) ; pred(pred_id).

	% Output an error message about an undefined type
	% in the specified context.

:- pred write_undef_type(type_id, context, io__state, io__state).
:- mode write_undef_type(input, context, di, uo).
write_undef_type(TypeId, Context) -->
	io__write_string("Undefined type "),
	write_type_id(TypeId),
	io__write_string(" used in definition of "),
	write_context(Context),
	io__write_string(".\n").

	% Output a description of the context where an undefined type was
	% used.

:- pred write_context(context, io__state, io__state).
:- mode write_context(input, di, uo).

write_context(pred(PredId)) -->
	io__write_string("predicate "),
	write_pred_id(PredId).
write_context(type(TypeId)) -->
	io__write_string("type "),
	write_type_id(TypeId).

%-----------------------------------------------------------------------------%

	% Predicates to output type_ids and pred_ids.
	% XXX type_ids should include the module.

:- pred write_type_id(type_id, io__state, io__state).
:- mode write_type_id(input, di, uo).

write_type_id(F - N) -->
	prog_out__write_sym_name(F),
	io__write_string("/"),
	io__write_int(N).

:- pred write_pred_id(pred_id, io__state, io__state).
:- mode write_pred_id(input, di, uo).

write_pred_id(PredId) -->
	{ predicate_module(PredId, Module) },
	{ predicate_name(PredId, Name) },
	{ predicate_arity(PredId, Arity) },
	io__write_string(Module),
	io__write_string(":"),
	io__write_string(Name),
	io__write_string("/"),
	io__write_int(Arity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type ok_type_decl 	--->	type(varset, type_defn, condition)
			;	pred(varset, term, condition).
:- type types = bag(ok_type_decl).

:- pred types_init(types).
types_init(Types) :-
	bag_init(Types).

	% given a type name, let Args be the type parameters for that type
	% and let Term be a valid type template for that type.
:- type type_template	--->	functor(term)
			;	type(term).
:- pred types_type_template(types, string, varset, list(variable),
				type_template, condition).
types_type_template(Types, Name, VarSet, Args, Template, Cond) :-
	bag_contains(Types, type(VarSet, TypeDecl, Cond)),
	types_type_template_2(TypeDecl, Name, Args, Template).
types_type_template(_, Name, VarSet, [], Template, true) :-
	builtin_type(Const, Name),
	Template = functor(term_functor(Const, [], _)), 
	varset_init(VarSet).

:- pred types_type_template_2(type_defn, string, list(variable), type_template).
types_type_template_2(du_type(Name, Args, RHS), Name, Args, functor(Term)) :-
	member(Term, RHS).
types_type_template_2(uu_type(Name, Args, RHS), Name, Args, type(Term)) :-
	member(Term, RHS).
types_type_template_2(eqv_type(Name, Args, Term), Name, Args, type(Term)).

	% Given a predicate name, look up the types of its arguments
:- pred types_pred_type(types, string, varset, list(term), condition).
types_pred_type(Types, Functor, VarSet, ArgTypes, Cond) :-
	bag_contains(Types, pred(VarSet, term_functor(Functor,ArgTypes,_), Cond)).

%-----------------------------------------------------------------------------%

:- mode types_lookup_type(input, input, output, output, output).

    % builtin types
types_lookup_type(_, term_functor(Const,[],_), BuiltInType, TypeVarSet, []) :-
	builtin_type(Const, BuiltInType),
	varset_init(TypeVarSet).

	% XXX possible efficiency problems

    % user-defined types
types_lookup_type(TypeDefns, term_functor(F, Args, _), TypeId, TVarSet,
		ArgTypes) :-
	map__member(TypeId - Defn, TypeDefns),		% nondeterministic
	Defn = hlds__typedefn(TVarSet, Params, _Body, Cond),
	Body = du_type(Constructors),			% may fail
	same_length(Args, ArgTypes),
	member(unqualified(F) - ArgTypes), Constructors).  % nondeterministic

%-----------------------------------------------------------------------------%

	% builtin_type(Term, Type)
	%	is true iff 'Term' is a constant of the builtin type 'Type'.

:- pred builtin_type(const, string).
:- mode builtin_type(input, output).

builtin_type(term_integer(_), "integer").
builtin_type(term_float(_), "float").
builtin_type(term_string(_), "string").
builtin_type(term_atom(String), "char") :-
	char_to_string(_, String).

	% is_builtin_type(TypeId)
	%	is true iff 'TypeId' is the type_id of a builting type

:- pred is_builtin_type(type_id).
:- mode is_builtin_type(input).

is_builtin_type(unqualified("integer") - 0).
is_builtin_type(unqualified("float") - 0).
is_builtin_type(unqualified("string") - 0).
is_builtin_type(unqualified("char") - 0).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The typeinfo data structure and access predicates.

:- type type_info 	--->	typeinfo(
					io__state,
					pred_table,
					type_table,
					cons_table,
					pred_id,
					bool,	% did we find any type errors?
					varset,	% type params
					type_assign_set,
				).

:- mode typeinfo_di = di.		% XXX
:- mode typeinfo_uo = uo.		% XXX

%-----------------------------------------------------------------------------%

:- pred typeinfo_init(io__state, module_info, pred_id, varset, varset,
			type_info).
:- mode typeinfo_init(di, input, input, input, input, typeinfo_uo).

typeinfo_init(IOState, ModuleInfo, PredId, TypeVarSet, VarSet, TypeInfo) :-
	moduleinfo_preds(ModuleInfo, Preds),
	moduleinfo_types(ModuleInfo, Types),
	moduleinfo_ctors(ModuleInfo, Ctors),
	map__init(TypeBindings),
	map__init(VarTypes),
	TypeInfo = typeinfo(
		IOState, Preds, Types, Ctors, PredId, false, TypeVarSet,
		[type_assign(VarSet, VarTypes, TypeVarSet, TypeBindings)]
	).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_ctors(type_info, cons_table).
:- mode typeinfo_get_ctors(input, output).

typeinfo_get_ctors(typeinfo(_,_,_,Ctors,_,_,_,_), Ctors).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_type_assign_set(type_info, type_assign_set).
:- mode typeinfo_get_type_assign_set(input, output).

typeinfo_get_type_assign_set(typeinfo(_,_,_,_,_,_,_,TypeAssignSet),
			TypeAssignSet).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_type_assign_set(type_info, type_assign_set, type_info).
:- mode typeinfo_set_type_assign_set(typeinfo_di, input, typeinfo_uo).

typeinfo_set_type_assign_set( typeinfo(A,B,C,D,E,F,G,_), TypeAssignSet,
			typeinfo(A,B,C,D,E,F,G,TypeAssignSet)).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_typevarset(type_info, varset).
:- mode typeinfo_get_typevarset(input, output).

typeinfo_get_typevarset(typeinfo(_,_,_,_,_,_,TypeVarSet,_), TypeVarSet).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_typevarset(type_info, varset, type_info).
:- mode typeinfo_set_typevarset(typeinfo_di, input, typeinfo_uo).

typeinfo_set_typevarset( typeinfo(A,B,C,D,E,F,_,H), TypeVarSet,
			typeinfo(A,B,C,D,E,F,TypeVarSet,H)).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_io_state(type_info, io__state).
:- mode typeinfo_get_io_state(typeinfo_d_io_state, uo).

typeinfo_get_io_state(typeinfo(IOState,_,_,_,_,_,_,_), IOState).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_io_state(type_info, io__state, type_info).
:- mode typeinfo_set_io_state(typeinfo_d_di, ui, typeinfo_uo).

typeinfo_set_io_state( typeinfo(A,B,C,D,E,F,G,_), TypeAssignSet,
			typeinfo(A,B,C,D,E,F,G,TypeAssignSet)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
