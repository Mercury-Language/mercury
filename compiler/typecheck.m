%-----------------------------------------------------------------------------%
%
% File: typecheck.nl.
% Main author: fjh.
%
% This file contains a type-checker which I started writing a fair while
% ago.  It still needs a bit of work to get it to actually work.
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
% Note that DCGS are used for THREE different purposes in this file:
%
%	1.  For accumulating io__states as usual.
%
%	2.  For accumulating type_infos, which contain:
%		- an io_state, which is modified if we need to
%		  to write out an error message
%		- various semi-global info which doesn't change,
%		  namely the pred_id and term__context of the clause
%		  we are type-checking
%		- a type_assign_set which stores the set of possible
%		  type assignments and is modified as we traverse through
%		  the clause
%	
%	3.  For accumulating type_assign_sets.  This is when we are
%	    type-checking a single atomic construct (unification or
%	    predicate), and we are iterating through all the
%	    possible existing type-assignments to accumulate a new
%	    type-assignment set.
%
% This can be a little confusing if you're not aware of it, so be
% careful to look at the pred declarations to see what each DCG predicate
% is actually accumulating.
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
%	There may be other types (short integers, complex numbers, rationals,
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
%	XXX	we should handle equivalence types here
%
%	XXX	we should handle overloading of predicates
%
%	XXX	improve error reporting
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module typecheck.
:- interface.
:- import_module hlds, io.

:- pred typecheck(module_info, module_info, bool, io__state, io__state).
:- mode typecheck(input, output, output, di, uo).

/*
	Formally, typecheck(Module0, Module, FoundError, IO0, IO) is
	intended to be true iff Module is Module0 annotated with the
	variable typings that result from the process of type-checking,
	FoundError is `yes' if Module0 contains any type errors and `no'
	otherwise, and IO is the io__state that results from IO0 after
	printing out appropriate error messages for the type errors in
	Module0, if any.

	Informally, typecheck(Module0, Module, FoundError, IO0, IO) 
	type-checks Module0 and annotates it with variable typings
	(returning the result in Module), prints out appropriate error
	messages, and sets FoundError to `yes' if it finds any errors
	and `no' otherwise.
*/

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, varset, prog_out, string.

%-----------------------------------------------------------------------------%

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

:- pred check_pred_types(module_info, module_info, bool, io__state, io__state).
:- mode check_pred_types(input, output, output, di, uo).

typecheck_pred_types(Module0, Module, FoundError) -->
	{ moduleinfo_predids(Module, PredIds) },
	check_pred_types_2(PredIds, Module0, Module, no, FoundError).

%-----------------------------------------------------------------------------%

	% Iterate over the list of pred_ids in a module.

:- pred check_pred_types_2(list(pred_id), module_info, module_info,
			bool, bool, io__state, io__state).
:- mode check_pred_types_2(input, input, input, output, output, di, uo).

typecheck_pred_types_2([], ModuleInfo, Error, ModuleInfo, Error) --> [].
typecheck_pred_types_2([PredId | PredIds], ModuleInfo0, Error0,
				ModuleInfo, Error) -->
	{ moduleinfo_preds(ModuleInfo0, Preds0) },
	{ map__search(Preds, PredId, PredInfo0) },
	{ predinfo_argtypes(PredInfo0, TypeVarSet, ArgTypes) },
	{ predinfo_clauses(PredInfo0, Clauses0) },
	typecheck_clause_list(Clauses0, PredId, TypeVarSet, ArgTypes,
		ModuleInfo0, Error0, Clauses, Error1),
	{ predinfo_set_clauses(PredInfo0, Clauses, PredInfo) },
	{ map__set(Preds0, PredId, Clauses, Preds) },
	{ moduleinfo_set_preds(ModuleInfo0, Preds, ModuleInfo1) },
	typecheck_pred_types_2(PredIds, ModuleInfo1, Error1, ModuleInfo, Error).

%-----------------------------------------------------------------------------%

	% Iterate over the list of clauses for a predicate.
	% 

:- pred typecheck_clause_list(list(clause), pred_id, tvarset, list(type),
		module_info, bool, list(clause), bool, io__state, io__state).
:- mode typecheck_clause_list(input, input, input, input, input, input, input,
			output, output, di, uo).

typecheck_clause_list([], _PredId, _TypeVarSet, _ArgTypes, _ModuleInfo, Error,
			Error, [])
		--> [].
typecheck_clause_list([Clause0|Clauses0], PredId, TypeVarSet, ArgTypes,
		ModuleInfo, Error0, [Clause|Clauses], Error) -->
	typecheck_clause(Clause0, PredId, TypeVarSet, ArgTypes, ModuleInfo,
		Error0, Clause, Error1),
	typecheck_clause_list(Clauses0, PredId, TypeVarSet, ArgTypes,
		ModuleInfo, Error1, Clauses, Error).

%-----------------------------------------------------------------------------%

	% Type-check a single clause.

	% As we go through a clause, we determine the possible
	% type assignments for the clause.  A type assignment
	% is an assignment of a type to each variable in the
	% clause.
	%
	% Note that this may cause exponential time & space usage
	% in the presence of overloading of predicates and/or
	% functors.  This is a potentially serious problem, but
	% there's no easy solution apparent.
	%
	% It would be more natural to use non-determinism to write
	% this code, and perhaps even more efficient.
	% But doing it deterministically would make bootstrapping more
	% difficult, and most importantly would make good error
	% messages very difficult.

:- pred typecheck_clause(clause, pred_id, tvarset, list(type), module_info,
		bool, clause, bool, io__state, io__state).
:- mode typecheck_clause(input, input, input, input, input, input,
		output, output, di, uo).

typecheck_clause(Clause0, PredId, TypeVarSet, ArgTypes, ModuleInfo, Error0,
		Clause, Error, IOState0, IOState) :-

		% initialize the typeinfo
		% XXX abstract clause/6

	Clause0 = clause(Modes, VarSet, _DummyVarTypes, HeadVars, Body,
			Context),
	typeinfo_init(IOState0, ModuleInfo, PredId, Context, TypeVarSet,
			VarSet, TypeInfo0),

		% typecheck the clause - first the head unification, and
		% then the body

	typecheck_var_has_type_list(HeadVars, ArgTypes, TypeInfo0, TypeInfo1),
	typecheck_goal(Body0, TypeInfo1, TypeInfo),

		% finish up

	typeinfo_get_type_assign_set(TypeInfo, TypeAssignSet),
	typeinfo_get_io_state(TypeInfo, IOState1),
	typecheck_finish_up(TypeAssignSet, TypeInfo, Error0, VarTypes, Error,
			IOState1, IOState),
	Clause = clause(Modes, VarSet, VarTypes, HeadVars, Body, Context).

	% At this stage, there are three possibilities.
	% There are either zero, one, or multiple type assignments
	% for the clause.  In the first case, we have already
	% issued an error message.  In the second case, the
	% clause is type-correct.  In the third case, we have to
	% issue an error message here.

:- pred typecheck_finish_up(type_assign_set, type_info, bool, map(var, type),
		bool, io__state, io__state).
:- mode typecheck_finish_up(input, input, input, output, output, di, uo).

typecheck_finish_up([], _TypeInfo, _Error, VarTypes, yes) -->
	{ map__init(VarTypes) }.
typecheck_finish_up([TypeAssign], _TypeInfo, Error, VarTypes, Error) -->
	{ type_assign_get_var_types(TypeAssign, VarTypes) }.
typecheck_finish_up([TypeAssign1, TypeAssign2 | _], TypeInfo, _Error,
		VarTypes1, yes) -->
	{ type_assign_get_var_types(TypeAssign1, VarTypes1) },
	report_ambiguity_error(TypeInfo, TypeAssign1, TypeAssign2).

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
	typecheck_unification(A, B).

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
	typeinfo_get_preds(TypeInfo, Preds),
	map__search(Preds, PredId, PredInfo),
	predinfo_arg_types(PredInfo, PredTypeVarSet, PredArgTypes),
		% rename apart the type variables in called
		% predicate's arg types
	typeinfo_get_typevarset(TypeInfo0, TypeVarSet0),
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
	typeinfo_get_varset(TypeInfo0, VarSet),
	typecheck_var_has_type_2(TypeAssignSet0, VarId, Type, TypeAssignSet),
	(if
		TypeAssignSet = [],
		(not TypeAssignSet0 = [])
	then
		typeinfo_get_io_state(TypeInfo0, IOState0),
		typeinfo_get_context(TypeInfo0, Context),
		typeinfo_get_predid(TypeInfo0, PredId),
		report_error_var(PredId, Context, VarSet, VarId, Type, 
					IOState0, IOState),
		typeinfo_set_io_state(TypeInfo0, IOState, TypeInfo1),
		typeinfo_set_found_error(TypeInfo1, true, TypeInfo2),
		typeinfo_set_type_assign_set(TypeInfo2, TypeAssignSet, TypeInfo)
	else
		typeinfo_set_type_assign_set(TypeInfo0, TypeAssignSet, TypeInfo)
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
		(if some [TypeAssign]
			type_unify(TypeAssign0, VarType, Type, TypeAssign)
		then
			Result = [TypeAssign | TypeAssignSet]
		else
			Result = TypeAssignSet
		)
	else
		map__set(VarTypes0, VarId, Type, VarTypes),
		type_assign_set_var_types(TypeAssign0, VarTypes, TypeAssign),
		Result = [TypeAssign | TypeAssignSet]
	),
	typecheck_var_has_type_2(TypeAssignSet0, VarId, Type, TypeAssignSet).

%-----------------------------------------------------------------------------%
	
:- pred typecheck_term_has_type_list(list(term), list(type), 
					type_info, type_info).
:- mode typecheck_term_has_type_list(input, input, input, 
					typeinfo_di, typeinfo_uo).

typecheck_term_has_type_list([], []) --> [].
typecheck_term_has_type_list([Arg | Args], [Type | Types]) -->
	typecheck_term_has_type(Arg, Type),
	typecheck_term_has_type_list(Args, Types).

:- pred typecheck_term_has_type(term, type, type_info, type_info).
:- mode typecheck_term_has_type(input, input, typeinfo_di, typeinfo_uo).

typecheck_term_has_type(term_variable(Var), Type, TypeInfo0, TypeInfo) :-
	typecheck_var_has_type(Var, Type, VarSet, TypeInfo0, TypeInfo).

typecheck_term_has_type(term_functor(F, As, C), Type, TypeInfo0, TypeInfo) :-
	length(As, Arity),
	typeinfo_get_ctor_list(TypeInfo, cons(F, Arity), ConsDefnList),
	typeinfo_get_type_assign_set(TypeInfo0, TypeAssignSet0),
	typecheck_cons_has_type(TypeAssignSet0, ConsDefnList, As, Type,  [],
		TypeAssignSet),
	(if
		TypeAssignSet = [],
		(not TypeAssignSet0 = [])
	then
		typeinfo_get_io_state(TypeInfo0, IOState0),
		typeinfo_get_context(TypeInfo0, Context),
		typeinfo_get_predid(TypeInfo0, PredId),
		report_error_cons(PredId, Context, F, Arity, Type,
				IOState0, IOState),
		typeinfo_set_io_state(TypeInfo0, IOState, TypeInfo1),
		typeinfo_set_found_error(TypeInfo1, true, TypeInfo2),
		typeinfo_set_type_assign_set(TypeInfo2, TypeAssignSet, TypeInfo)
	else
		typeinfo_set_type_assign_set(TypeInfo0, TypeAssignSet, TypeInfo)
	).

%-----------------------------------------------------------------------------%

	% Check that a constructor has the specified type
	% and that the arguments of the constructor have the appropriate
	% types for that constructor.
	% We do this by iterating over all the possible current
	% type assignments. 
	% For each possible current type assignment, we produce a
	% list of the possible resulting type assignments after
	% we have unified the type of this constructor with
	% the specified type.

:- pred typecheck_cons_has_type(type_assign_set, list(hlds__cons_defn),
		list(term), list(type), type_assign_set, type_assign_set).
:- mode typecheck_cons_has_type(input, input, input, input, input, output).

typecheck_cons_has_type([], _, _, _) --> [].
typecheck_cons_has_type([TypeAssign|TypeAssigns], ConsDefnList, Args, Type) -->
	type_assign_cons_has_type(ConsDefnList, TypeAssign, Args, Type),
	typecheck_cons_has_type(TypeAssigns, ConsDefnList, Args, Type).

%-----------------------------------------------------------------------------%

	% For each possible constructor which matches the
	% term (overloading means that there may be more than one),
	% if this constructor matches the specified type and
	% the types of it's arguments are ok, then add the resulting
	% type assignment to the type assignment set.

:- pred type_assign_cons_has_type(list(hlds__cons_defn), type_assign,
		list(term), list(type), type_assign_set, type_assign_set).
:- mode type_assign_cons_has_type(input, input, input, input, input, output).

type_assign_cons_has_type([], _TypeAssign0, _Args, _Type) --> [].
type_assign_cons_has_type([ConsDefn | ConsDefns], TypeAssign0, Args, Type) -->
	type_assign_cons_has_type_2(ConsDefn, TypeAssign0, Args, Type),
	type_assign_cons_has_type(ConsDefns, TypeAssign0, Args, Type).

:- pred type_assign_cons_has_type_2(hlds__cons_defn, type_assign,
		list(term), list(type), type_assign_set, type_assign_set).
:- mode type_assign_cons_has_type_2(input, input, input, input, input, output).

type_assign_cons_has_type_2(ConsDefn, TypeAssign0, Args, Type) -->
	{
	  ConsDefn = hlds__cons_defn(ArgTypes, TypeId, _Context),

		% construct the type of this constructor
	  type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
	  type_id_to_type(TypeId, TypeVarSet0, ConsType, TypeVarSet),
	  type_assign_set_typevarset(TypeAssign0, TypeVarSet, TypeAssign1)
	},
	(	{ type_assign_unify_type(TypeAssign1, ConsType, Type,
			TypeAssign2) }
	->
		type_assign_term_has_type_list(Args, ArgTypes, TypeAssign2)
	;
		% Don't add any new type assignments to the set
		[]
	).

:- pred type_assign_term_has_type_list(list(term), list(type), type_assign,
			type_assign_set, type_assign_set).
:- mode type_assign_term_has_type_list(input, input, input, input, output).

type_assign_term_has_type_list([], [], _) --> [].
type_assign_term_has_type_list([Arg | Args], [Type | Types], TypeAssign0) -->
	type_assign_term_has_type(Arg, Type, TypeAssign0),
	type_assign_term_has_type_list(Args, Types, TypeAssign0).
	
:- pred type_assign_term_has_type(term, type, type_assign,
			type_assign_set, type_assign_set).
:- mode type_assign_term_has_type(input, input, input, input, output).

/***** XXX Incomplete!!
type_assign_term_has_type(term_variable(V), Type, TypeAssign) -->
	
type_assign_term_has_type(term_variable(V), Type, TypeAssign
******/

%-----------------------------------------------------------------------------%

	% Type check a unification.
	% Get the type assignment set from the type info and then just
	% iterate over all the possible type assignments.

:- pred typecheck_unification(term, term, type_info, type_info).
:- mode typecheck_unification(input, input, typeinfo_di, typeinfo_uo).

typecheck_unification(X, Y, TypeInfo0, TypeInfo) :-
	typeinfo_get_type_assign_set(TypeInfo0, TypeAssignSet0),
	typecheck_unification_2(TypeAssignSet0, X, Y, [], TypeAssignSet),
		% XXX report an error!!
	typeinfo_set_type_assign_set(TypeInfo0, TypeAssignSet, TypeInfo).


	% iterate over all the possible type assignments.

:- pred typecheck_unification_2(type_assign_set, term, term, type_assign_set,
					type_assign_set).
:- mode typecheck_unification_2(input, input, input, input, output).

typecheck_unification_2([], _, _, TypeAssignSet, TypeAssignSet).
typecheck_unification_2([TypeAssign0 | TypeAssigns0], X, Y, TypeAssignSet0,
		TypeAssignSet) :-
	type_assign_unify_term(X, Y, TypeAssign0, TypeAssignSet0,
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
:- mode type_assign_unify_term(input, input, input, input, output).

:- type_assign_unify_term(T1, T2, _, _) when T1 and T2.	% NU-Prolog indexing

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
					TypeY, TypeAssign)
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
			TypeAssignSet = [TypeAssign | TypeAssignSet0]
		)
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
			varset__new_var(TypeVarSet0, TypeVar, Type, TypeVarSet),
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
		TypeAssignSet0, TypeAssignSet) :-
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
		TypeAssignSet0, TypeAssignSet) :-
	type_assign_unify_term(term_functor(F, As, _), term_variable(Y),
		TypeAssign0, TypeAssignSet0, TypeAssignSet).
	
type_assign_unify_term(term_functor(FX, AsX, _), term_functor(FY, AsY, _),
		TypeAssign0, TypeAssignSet0, TypeAssignSet) :-
	{ write('warning: unification of term with term\n') },
	TypeAssignSet = TypeAssignSet0.

	% XXX we should handle this properly

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
	type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
	type_id_to_type(TypeId, TypeVarSet0, ConsType, TypeVarSet),
	type_assign_set_typevarset(TypeAssign0, TypeVarSet, TypeAssign1),

		% unify the type of Var with the type of the constructor
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	(if some [TypeY]
		map__search(VarTypes0, Y, TypeY)
	then
		type_assign_unify_type(TypeAssign1, ConsType, TypeY,
					TypeAssign2)
	else
		map__set(VarTypes0, Y, Type, VarTypes),
		type_assign_set_var_types(TypeAssign1, VarTypes, TypeAssign2)
	),

		% check that the types of the arguments matches the
		% specified arg types for this constructor
	type_assign_term_has_type_list(Args, ArgTypes, TypeAssign2, 
		TypeAssignSet0, TypeAssignSet1),

		% recursively handle all the other possible constructors
		% that match this functor.
	type_assign_unify_var_functor(ConsDefns, Args, Y, TypeAssign0,
		TypeAssignSet1, TypeAssignSet).

%-----------------------------------------------------------------------------%

	% Given a type id (such as list/1), construct a type term
	% (such as list(T)) corresponding to that type.

:- pred type_id_to_type(type_id, tvarset, type, tvarset).
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
		varset__new_var(VarSet0, VarId, VarSet1),
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
			not term__occurs(BindingOfX, Y, Bindings0),
			map__set(Bindings0, Y, BindingOfX, Bindings)
		)
	else
		(if some [TypeY]
			map__search(Bindings0, Y, BindingOfY)
		then
			% X is a variable which hasn't been bound yet
			not term__occurs(BindingOfY, X, Bindings0),
			map__set(Bindings0, X, BindingOfY, Bindings)
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
		not term__occurs_list(As, X, Bindings0),
		map__set(Bindings0, X, term_functor(F, As, C), Bindings)
	).

type_unify(term_functor(F, As, C), term_variable(X), Bindings0, Bindings) :-
	(if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	then
		not term__occurs_list(As, X, Bindings0),
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
		type_unify_list(AsX, AsY, Bindings0, Bindings)
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

:- pred check_undefined_types(module_info, module_info, io__state, io__state).
:- mode check_undefined_types(input, output, di, uo).
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
		report_undef_type(TypeId, Context)
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

:- pred report_undef_type(type_id, context, io__state, io__state).
:- mode report_undef_type(input, context, di, uo).
report_undef_type(TypeId, Context) -->
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

	% builtin_type(Term, Type)
	%	is true iff 'Term' is a constant of the builtin type 'Type'.

:- pred builtin_type(const, string).
:- mode builtin_type(input, output).

builtin_type(term_integer(_), "integer").
builtin_type(term_float(_), "float").
builtin_type(term_string(_), "string").
builtin_type(term_atom(String), "char") :-
	string__char_to_string(_, String).

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

:- type tvarset		==	varset.

:- type type_info 	--->	typeinfo(
					io__state,
					pred_table,
					type_table,
					cons_table,
					pred_id,
					term__context,
					tvarset,	% type params
					varset,		% variables
					type_assign_set,
					bool	% did we find any type errors?
				).

	% The normal inst of a type_info struct: ground, with
	% the io_state and the struct itself unique, but with
	% multiple references allowed for the other parts.

:- inst uniq_type_info	=	bound_unique(
					typeinfo(
						ground_unique, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground
					)
				).

:- mode typeinfo_di :: uniq_type_info -> dead.
:- mode typeinfo_uo :: free -> uniq_type_info.

	% Some fiddly modes used when we want to extract
	% the io_state from a typeinfo struct and then put it back again.

:- inst type_info_no_io	=	bound_unique(
					typeinfo(
						dead, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground
					)
				).

:- mode typeinfo_get_io_state :: uniq_type_info -> type_info_no_io.
:- mode typeinfo_set_io_state :: type_info_no_io -> dead.

%-----------------------------------------------------------------------------%

:- pred typeinfo_init(io__state, module_info, pred_id, term__context,
			varset, varset, type_info).
:- mode typeinfo_init(di, input, input, input, input, input, typeinfo_uo).

typeinfo_init(IOState, ModuleInfo, PredId, Context, TypeVarSet, VarSet,
		TypeInfo) :-
	moduleinfo_preds(ModuleInfo, Preds),
	moduleinfo_types(ModuleInfo, Types),
	moduleinfo_ctors(ModuleInfo, Ctors),
	map__init(TypeBindings),
	map__init(VarTypes),
	TypeInfo = typeinfo(
		IOState, Preds, Types, Ctors, PredId, Context, TypeVarSet,
		VarSet, [type_assign(VarTypes, TypeVarSet, TypeBindings)],
		false
	).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_io_state(type_info, io__state).
:- mode typeinfo_get_io_state(typeinfo_get_io_state, uo).

typeinfo_get_io_state(typeinfo(IOState,_,_,_,_,_,_,_,_,_), IOState).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_io_state(type_info, io__state, type_info).
:- mode typeinfo_set_io_state(typeinfo_set_io_state, ui, typeinfo_uo).

typeinfo_set_io_state( typeinfo(_,B,C,D,E,F,G,H,I,J), IOState,
			typeinfo(IOState,B,C,D,E,F,G,H,I,J)).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_preds(type_info, pred_table).
:- mode typeinfo_get_preds(input, output).

typeinfo_get_preds(typeinfo(_,Preds,_,_,_,_,_,_,_,_), Preds).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_types(type_info, type_table).
:- mode typeinfo_get_types(input, output).

typeinfo_get_types(typeinfo(_,_,Types,_,_,_,_,_,_,_), Types).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_ctors(type_info, cons_table).
:- mode typeinfo_get_ctors(input, output).

typeinfo_get_ctors(typeinfo(_,_,_,Ctors,_,_,_,_,_,_), Ctors).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_predid(type_info, pred_id).
:- mode typeinfo_get_predid(input, output).

typeinfo_get_predid(typeinfo(_,_,_,_,PredId,_,_,_,_,_), PredId).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_context(type_info, term__context).
:- mode typeinfo_get_context(input, output).

typeinfo_get_context(typeinfo(_,_,_,_,Context,_,_,_,_,_), Context).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_type_assign_set(type_info, type_assign_set).
:- mode typeinfo_get_type_assign_set(input, output).

typeinfo_get_type_assign_set(typeinfo(_,_,_,_,_,_,_,_,TypeAssignSet,_),
			TypeAssignSet).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_type_assign_set(type_info, type_assign_set, type_info).
:- mode typeinfo_set_type_assign_set(typeinfo_di, input, typeinfo_uo).

typeinfo_set_type_assign_set( typeinfo(A,B,C,D,E,F,G,H,_,J), TypeAssignSet,
			typeinfo(A,B,C,D,E,F,G,H,TypeAssignSet,J)).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_varset(type_info, varset).
:- mode typeinfo_get_varset(input, output).

typeinfo_get_varset(typeinfo(_,_,_,_,_,_,_,VarSet,_,_), VarSet).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_typevarset(type_info, varset).
:- mode typeinfo_get_typevarset(input, output).

typeinfo_get_typevarset(typeinfo(_,_,_,_,_,_,TypeVarSet,_,_,_), TypeVarSet).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_typevarset(type_info, varset, type_info).
:- mode typeinfo_set_typevarset(typeinfo_di, input, typeinfo_uo).

typeinfo_set_typevarset( typeinfo(A,B,C,D,E,F,_,H,I,J), TypeVarSet,
			typeinfo(A,B,C,D,E,F,TypeVarSet,H,I,J)).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_found_error(type_info, bool, type_info).
:- mode typeinfo_set_found_error(typeinfo_di, input, typeinfo_uo).

typeinfo_set_found_error( typeinfo(A,B,C,D,E,F,G,H,I,_), FoundError,
			typeinfo(A,B,C,D,E,F,G,H,I,FoundError)).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_ctor_list(type_info, cons_id, list(hlds__cons_defn)).
:- mode typeinfo_get_ctor_list(input, input, output).

typeinfo_get_ctor_list(TypeInfo, ConsId, ConsDefnList) :-
	typeinfo_get_ctors(TypeInfo, Ctors),
	(if some [ConsDefnList0]
		map__search(Ctors, ConsId, ConsDefnList)
	then
		ConsDefnList = ConsDefnList0
	else
		ConsDefnList = []
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The type_assign and type_assign_set data structures.

:- type type_assign_set	==	list(type_assign).

:- type type_assign	--->	type_assign(
					map(var, type),		% var types
					tvarset,		% type names
					map(type_param, type)	% type bindings
				).

%-----------------------------------------------------------------------------%

	% Access predicates for the type_assign data structure.
	% Excruciatingly boring code.

:- pred type_assign_get_var_types(type_assign, map(var, type)).
:- mode type_assign_get_var_types(input, output).

type_assign_get_var_types(type_assign(VarTypes, _, _), VarTypes).

%-----------------------------------------------------------------------------%

:- pred type_assign_get_typevarset(type_assign, tvarset).
:- mode type_assign_get_typevarset(input, output).

type_assign_get_typevarset(type_assign(_, TypeVarSet, _), TypeVarSet).

%-----------------------------------------------------------------------------%

:- pred type_assign_get_type_bindings(type_assign, map(type_param, type)).
:- mode type_assign_get_type_bindings(input, output).

type_assign_get_type_bindings(type_assign(_, _, TypeBindings), TypeBindings).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_var_types(type_assign, map(var, type), type_assign).
:- mode type_assign_set_var_types(input, input, output).

type_assign_set_var_types(type_assign(_, B, C), VarTypes,
			type_assign(VarTypes, B, C)).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_typevarset(type_assign, tvarset, type_assign).
:- mode type_assign_set_typevarset(input, input, output).

type_assign_set_typevarset(type_assign(A, _, C), TypeVarSet,
			type_assign(A, TypeVarSet, C)).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_type_bindings(type_assign, map(type_param, type),
					type_assign).
:- mode type_assign_set_type_bindings(input, input, output).

type_assign_set_type_bindings(type_assign(A, B, _), TypeBindings,
			type_assign(A, B, TypeBindings)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred report_error_var(pred_id, term__context, varset, var, type, 
			io__state, io__state).
:- mode report_error_var(input, input, input, input, input, di, uo).

report_error_var(PredId, Context, VarSet, VarId, Type) -->
	write_context(Context),
	io__write_string("type error in clause for predicate `"),
	write_pred_id(PredId),
	io__write_string("':\n"),
	io__write_string("variable `"),
	write_var(VarId, VarSet),	% XXX
	io__write_string("has type `"),
	io__write_string("(XXX)"),
	io__write_string("', expected type was `"),
	write_type(Type),	% XXX
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- pred report_error_cons(pred_id, term__context, const, int, type, 
			io__state, io__state).
:- mode report_error_cons(input, input, input, input, input, di, uo).

report_error_cons(PredId, Context, Functor, Arity, Type) -->
	write_context(Context),
	io__write_string("type error in clause for predicate `"),
	write_pred_id(PredId),
	io__write_string("':\n"),
	io__write_string("constructor `"),
	io__write_constant(Functor),
	io__write_string("/"),
	io__write_string(Arity),
	io__write_string("' does not have type `"),
	write_type(Type),	% XXX
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- pred report_ambiguity_error(type_info, type_assign, type_assign,
				io__state, io__state).
:- mode report_ambiguity_error(input, input, input, di, uo).

report_ambiguity_error(TypeInfo, TypeAssign1, TypeAssign2) -->
	{ typeinfo_get_context(TypeInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("type ambiguity in clause for predicate "),
	{ typeinfo_get_predid(TypeInfo, PredId) },
	write_pred_id(PredId),
	io__write_string(".\n"),
	io__write_string("possible type assignments include:\n"),
	{ map__keys(VarTypes1, Vars1) },
	{ typeinfo_get_varset(TypeInfo, VarSet) },
	{ type_assign_get_var_types(TypeAssign1, VarTypes1) },
	{ type_assign_get_var_types(TypeAssign2, VarTypes2) },
	report_ambiguity_error_2(Vars1, VarSet, VarTypes1, VarTypes2).

:- pred report_ambiguity_error_2(list(var), varset, type_assign, type_assign,
				io__state, io__state).
:- mode report_ambiguity_error_2(input, input, input, input, di, uo).

report_ambiguity_error_2([], _VarSet, _VarTypes1, _VarTypes2) --> [].
report_ambiguity_error_2([V | Vs], VarSet, VarTypes1, VarTypes2) -->
	( {
		map__search(VarTypes1, V, T1),
		map__search(VarTypes2, V, T2),
		not (T1 = T2)
	} ->
		write_var(V, VarSet),		% XXX
		io__write_string(" :: "),
		write_type(T1),		% XXX should expand type substitutions
		io__write_string(" OR "),
		write_type(T2),		% XXX should expand type substitutions
		io__write_string("\n")
	),
	report_ambiguity_error_2(Vs, VarSet, VarTypes1, VarTypes2).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
