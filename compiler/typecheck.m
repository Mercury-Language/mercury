%-----------------------------------------------------------------------------%
%
% File: typecheck.nl.
% Main author: fjh.
%
% This file contains a type-checker.
% It still has a few bugs.
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
%		- various semi-global info which doesn't change often,
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
% 2) undiscriminated unions [NOT YET IMPLEMENTED].
%	If rhs has a single type then the two
%	types have same structure but *different* name.
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
%    undiscriminated unions are NOT YET IMPLEMENTED.
%
% 3) equivalent types (treated identically, ie, same name.  Any number
%	of types can be equivalent; the *canonical* one is the one
%	which is not defined using ==):
%	:- type real == float.
%
%    Currently references to equivalence types are expanded
%    in a separate pass by toplevel.nl.  It would be better
%    to avoid expanding them (and instead modify the type unification
%    algorithm to handle equivalent types) because this would
%    give better error messages.  However, this is not a high
%    priority.
%
% 4) builtin types
%	character, int, float, string
%	pred, pred(T), pred(T1, T2), pred(T1, T2, T3), ... [not yet implemented]
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
%		error reporting could still be improved:
%		- undefined symbols are only handled cleanly
%		  if they occur as predicate arguments.
%		- type errors in predicate calls should state
%		  which predicate call caused the error.
%		- errors in unification with head arguments should be handled
%		  specially; that `HeadVar', `HeadVars'', etc. is ugly.
%
%  Wish list:
% 	 	we should handle explicit type qualifications
% 		(and remove them here) but we don't do so yet
%
%		we should handle equivalence types here
%
%		we should handle overloading of predicates
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module typecheck.
:- interface.
:- import_module hlds, io, prog_io.

:- pred typecheck(module_info, module_info, bool, io__state, io__state).
:- mode typecheck(in, out, out, di, uo).

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
:- import_module list, map, string, require, std_util.
:- import_module varset, prog_util, prog_out, hlds_out.
:- import_module options, getopt, globals.

%-----------------------------------------------------------------------------%

	% XXX need to pass FoundError to all steps

typecheck(Module0, Module, FoundError) -->
	lookup_option(statistics, bool(Statistics)),
	lookup_option(verbose, bool(Verbose)),
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),
	maybe_report_stats(Statistics),

	maybe_write_string(Verbose, "% Checking for undefined types...\n"),
	check_undefined_types(Module0, Module1),
	maybe_report_stats(Statistics),

	maybe_write_string(Verbose,
		"% Checking for circular type definitions...\n"),
	check_circular_types(Module1, Module2),
	maybe_report_stats(Statistics),

	maybe_write_string(Verbose, "% Type-checking clauses...\n"),
	check_pred_types(Module2, Module, FoundError),
	maybe_report_stats(Statistics),

	io__set_output_stream(OldStream, _).

%-----------------------------------------------------------------------------%
	
	% Type-check the code for all the predicates in a module.

:- pred check_pred_types(module_info, module_info, bool, io__state, io__state).
:- mode check_pred_types(in, out, out, di, uo).

check_pred_types(Module0, Module, FoundError) -->
	{ moduleinfo_predids(Module0, PredIds) },
	typecheck_pred_types_2(PredIds, Module0, no, Module, FoundError).

%-----------------------------------------------------------------------------%

	% Iterate over the list of pred_ids in a module.

:- pred typecheck_pred_types_2(list(pred_id), module_info, bool,
			module_info, bool, io__state, io__state).
:- mode typecheck_pred_types_2(in, in, in, out, out, di, uo).

typecheck_pred_types_2([], ModuleInfo, Error, ModuleInfo, Error) --> [].
typecheck_pred_types_2([PredId | PredIds], ModuleInfo0, Error0,
				ModuleInfo, Error, IOState0, IOState) :-
	moduleinfo_preds(ModuleInfo0, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	predinfo_arg_types(PredInfo0, TypeVarSet, ArgTypes),
	predinfo_clauses_info(PredInfo0, ClausesInfo0),
	ClausesInfo0 = clauses_info(VarSet, VarTypes0, HeadVars, Clauses0),
	( Clauses0 = [] ->
		ModuleInfo1 = ModuleInfo0,
		Error1 = Error0,
		IOState1 = IOState0
	;
		write_progress_message(PredId, IOState0, IOState1),
		term__vars_list(ArgTypes, HeadTypeParams),
		typeinfo_init(IOState0, ModuleInfo0, PredId,
				TypeVarSet, VarSet, VarTypes0, HeadTypeParams,
				TypeInfo0),
		typeinfo_set_found_error(TypeInfo0, Error0, TypeInfo1),
		typecheck_clause_list(Clauses0, HeadVars, ArgTypes, Clauses,
				TypeInfo1, TypeInfo2),
		typeinfo_get_vartypes(TypeInfo2, VarTypes),
		ClausesInfo = clauses_info(VarSet, VarTypes, HeadVars, Clauses),
		predinfo_set_clauses_info(PredInfo0, ClausesInfo, PredInfo),
		map__set(Preds0, PredId, PredInfo, Preds),
		moduleinfo_set_preds(ModuleInfo0, Preds, ModuleInfo1),
		typeinfo_get_found_error(TypeInfo2, Error1),
		typeinfo_get_io_state(TypeInfo2, IOState1)
	),
	typecheck_pred_types_2(PredIds, ModuleInfo1, Error1, ModuleInfo, Error,
		IOState1, IOState).

:- pred write_progress_message(pred_id, io__state, io__state).
:- mode write_progress_message(in, di, uo) is det.

write_progress_message(PredId) -->
	lookup_option(very_verbose, bool(VeryVerbose)),
	( { VeryVerbose = yes } ->
		io__write_string("% Type-checking predicate "),
		hlds_out__write_pred_id(PredId),
		io__write_string("\n")
	;
		[]
	).

%-----------------------------------------------------------------------------%

	% Iterate over the list of clauses for a predicate.

:- pred typecheck_clause_list(list(clause), list(var), list(type), list(clause),
				type_info, type_info).
:- mode typecheck_clause_list(in, in, in, out, typeinfo_di, typeinfo_uo).

typecheck_clause_list([], _, _, []) --> [].
typecheck_clause_list([Clause0|Clauses0], HeadVars, ArgTypes,
			[Clause|Clauses]) -->
	typecheck_clause(Clause0, HeadVars, ArgTypes, Clause),
	typecheck_clause_list(Clauses0, HeadVars, ArgTypes, Clauses).

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
	% But doing it notdeterministically would make bootstrapping more
	% difficult, and most importantly would make good error
	% messages very difficult.

	% we should perhaps do manual garbage collection here

:- pred typecheck_clause(clause, list(var), list(type), clause,
			type_info, type_info).
:- mode typecheck_clause(in, in, in, out, typeinfo_di, typeinfo_uo).

typecheck_clause(Clause, HeadVars, ArgTypes, Clause) -->
		% XXX abstract clause/3
	{ Clause = clause(_Modes, Body, Context) },
	typeinfo_set_context(Context),
		% typecheck the clause - first the head unification, and
		% then the body
	typecheck_var_has_type_list(HeadVars, ArgTypes, 1),
	typecheck_goal(Body),
		% check for type ambiguities
	typecheck_finish_clause.

%-----------------------------------------------------------------------------%

	% If there are still multiple type assignments for the clause,
	% or if some of the type variables which occur only in the body
	% of the clause haven't been bound,
	% then we issue an error message here.

:- pred typecheck_finish_clause(type_info, type_info).
:- mode typecheck_finish_clause(typeinfo_di, typeinfo_uo).

typecheck_finish_clause(TypeInfo0, TypeInfo) :-
	typeinfo_get_type_assign_set(TypeInfo0, TypeAssignSet),
	( TypeAssignSet = [TypeAssign] ->
			% XXX we should only report an unresolved polymorphism
			% error if there weren't any other errors in the clause
		check_type_bindings(TypeAssign, TypeInfo0, TypeInfo)
	; TypeAssignSet = [TypeAssign1, TypeAssign2 | _] ->
			% XXX we should only report an ambiguity error if
			% there weren't any other errors in the clause
		typeinfo_set_found_error(TypeInfo0, yes, TypeInfo1),
		typeinfo_get_io_state(TypeInfo1, IOState0),
		report_ambiguity_error(TypeInfo1, TypeAssign1, TypeAssign2,
			IOState0, IOState),
		typeinfo_set_io_state(TypeInfo1, IOState, TypeInfo)
	;
		error("internal error in typechecker: no type-assignment"),
		TypeInfo = TypeInfo0
	).

	% Check that the all of the types which have been inferred
	% for the variables in the clause do not contain any unbound type
	% variables other than the HeadTypeParams.

:- pred check_type_bindings(type_assign, type_info, type_info).
:- mode check_type_bindings(in, typeinfo_di, typeinfo_uo).

check_type_bindings(TypeAssign, TypeInfo0, TypeInfo) :-
	typeinfo_get_head_type_params(TypeInfo0, HeadTypeParams),
	type_assign_get_type_bindings(TypeAssign, TypeBindings),
	type_assign_get_var_types(TypeAssign, VarTypes),
	map__values(VarTypes, Types),
	set__init(Set0),
	check_type_bindings_2(Types, TypeBindings, HeadTypeParams, Set0, Set),
	set__to_sorted_list(Set, ErrorVars),
	( ErrorVars = [] ->
		TypeInfo = TypeInfo0
	;
		type_assign_get_typevarset(TypeAssign, TVarSet),
		report_unresolved_type_error(ErrorVars, TVarSet, TypeInfo0,
			TypeInfo)
	).

:- pred check_type_bindings_2(list(term), tsubst, headtypes, set(var),
				set(var)).
:- mode check_type_bindings_2(in, in, in, in, out).

check_type_bindings_2([], _, _, Set, Set).
check_type_bindings_2([Type0 | Types], TypeBindings, HeadTypeParams, Set0,
			Set) :-
	term__apply_rec_substitution(Type0, TypeBindings, Type),
	term__vars(Type, TVars),
	set__list_to_set(TVars, TVarsSet0),
	set__remove_list(TVarsSet0, HeadTypeParams, TVarsSet1),
	set__union(Set0, TVarsSet1, Set1),
	check_type_bindings_2(Types, TypeBindings, HeadTypeParams, Set1, Set).

	% report an error: uninstantiated type parameter

:- pred report_unresolved_type_error(list(var), tvarset, type_info, type_info).
:- mode report_unresolved_type_error(in, in, typeinfo_di, typeinfo_uo).

report_unresolved_type_error(TVars, TVarSet, TypeInfo0, TypeInfo) :-
	typeinfo_get_io_state(TypeInfo0, IOState0),
	report_unresolved_type_error_2(TypeInfo0, TVars, TVarSet,
		IOState0, IOState),
	typeinfo_set_io_state(TypeInfo0, IOState, TypeInfo1),
	typeinfo_set_found_error(TypeInfo1, yes, TypeInfo).

:- pred report_unresolved_type_error_2(type_info, list(var), tvarset,
					io__state, io__state).
:- mode report_unresolved_type_error_2(typeinfo_no_io, in, in, di, uo).

report_unresolved_type_error_2(TypeInfo, TVars, TVarSet) -->
	write_typeinfo_context(TypeInfo),
	{ typeinfo_get_context(TypeInfo, Context) },
	io__write_string("  type error: unresolved polymorphism.\n"),
	prog_out__write_context(Context),
	io__write_string("  Unbound type vars were: "),
	write_type_var_list(TVars, TVarSet),
	io__write_string(".\n"),
	lookup_option(verbose_errors, bool(VerboseErrors)),
	( { VerboseErrors = yes } ->
		io__write_string("\tThe body of the clause contains a call to a polymorphic predicate,\n"),
		io__write_string("\tbut I can't determine which version should be called,\n"),
		io__write_string("\tbecause the type variables listed above didn't get bound.\n"),
			% XXX improve error message
		io__write_string("\t(I ought to tell you which call caused the error, but I'm afraid\n"),
		io__write_string("\tyou'll have to work it out yourself.  My apologies.)\n")
	;
		[]
	).

:- pred write_type_var_list(list(var), varset, io__state, io__state).
:- mode write_type_var_list(in, in, di, uo).

write_type_var_list([], _) -->
	io__write_string("<none>").
write_type_var_list([V|Vs], VarSet) -->
	io__write_variable(V, VarSet),
	write_type_var_list_2(Vs, VarSet).

:- pred write_type_var_list_2(list(var), varset, io__state, io__state).
:- mode write_type_var_list_2(in, in, di, uo).

write_type_var_list_2([], _) --> [].
write_type_var_list_2([V|Vs], VarSet) -->
	io__write_string(", "),
	io__write_variable(V, VarSet),
	write_type_var_list_2(Vs, VarSet).

%-----------------------------------------------------------------------------%

:- pred typecheck_goal(hlds__goal, type_info, type_info).
:- mode typecheck_goal(in, typeinfo_di, typeinfo_uo).

typecheck_goal(Goal - _GoalInfo, TypeInfo0, TypeInfo) :-
	% XXX prog_io.nl and make_hlds.nl don't set up the
	%     goalinfo context, so we have to just use the clause
	%     context
	% goalinfo_context(GoalInfo, Context),
	% typeinfo_set_context(Context, TypeInfo0, TypeInfo1),
	TypeInfo1 = TypeInfo0,
	typecheck_goal_2(Goal, TypeInfo1, TypeInfo).

:- pred typecheck_goal_2(hlds__goal_expr, type_info, type_info).
:- mode typecheck_goal_2(in, typeinfo_di, typeinfo_uo).

typecheck_goal_2(conj(List)) -->
	checkpoint("conj"),
	typecheck_goal_list(List).
typecheck_goal_2(disj(List)) -->
	checkpoint("disj"),
	typecheck_goal_list(List).
typecheck_goal_2(if_then_else(_Vs, A, B, C)) -->
	checkpoint("if"),
	typecheck_goal(A),
	checkpoint("then"),
	typecheck_goal(B),
	checkpoint("else"),
	typecheck_goal(C).
typecheck_goal_2(not(_Vs, A)) -->
	checkpoint("not"),
	typecheck_goal(A).
typecheck_goal_2(some(_Vs, G)) -->
	checkpoint("some"),
	typecheck_goal(G).
typecheck_goal_2(all(_Vs, G)) -->
	checkpoint("all"),
	typecheck_goal(G).
typecheck_goal_2(call(PredId, _Mode, Args, _Builtin)) -->
	checkpoint("call"),
	typecheck_call_pred(PredId, Args).
typecheck_goal_2(unify(A, B, _Mode, _Info, UnifyContext)) -->
	checkpoint("unify"),
	typeinfo_set_arg_num(0),
	typeinfo_set_unify_context(UnifyContext),
	typecheck_unification(A, B).

%-----------------------------------------------------------------------------%

:- pred typecheck_goal_list(list(hlds__goal), type_info, type_info).
:- mode typecheck_goal_list(in, typeinfo_di, typeinfo_uo).

typecheck_goal_list([]) --> [].
typecheck_goal_list([Goal | Goals]) -->
	typecheck_goal(Goal),
	typecheck_goal_list(Goals).

%-----------------------------------------------------------------------------%

:- pred typecheck_call_pred(pred_id, list(term), type_info, type_info).
:- mode typecheck_call_pred(in, in, typeinfo_di, typeinfo_uo).

	% WISHLIST - we should handle overloading of predicates

typecheck_call_pred(PredId, Args, TypeInfo0, TypeInfo) :-
		% look up the called predicate's arg types
	typeinfo_set_called_predid(TypeInfo0, PredId, TypeInfo1),
	typeinfo_get_preds(TypeInfo1, Preds),
	( % if some [PredInfo]
		map__search(Preds, PredId, PredInfo)
	->
		predinfo_arg_types(PredInfo, PredTypeVarSet, PredArgTypes0),

			% rename apart the type variables in called
			% predicate's arg types
			% (optimize for the common case of
			% a non-polymorphic predicate)
		( varset__is_empty(PredTypeVarSet) ->
			PredArgTypes = PredArgTypes0,
			TypeInfo2 = TypeInfo1
		;
			rename_apart(TypeInfo1, PredTypeVarSet, PredArgTypes0,
					TypeInfo2, PredArgTypes)
		),
			% unify the types of the call arguments with the
			% called predicates' arg types
		typecheck_term_has_type_list(Args, PredArgTypes, 0, TypeInfo2,
				TypeInfo)
	;
		typeinfo_get_io_state(TypeInfo1, IOState0),
		typeinfo_get_pred_name_index(TypeInfo0, PredNameIndex),
		predicate_name(PredId, PredName),
		( map__contains(PredNameIndex, PredName) ->
			report_error_pred_num_args(TypeInfo1, PredId,
				IOState0, IOState)
		;
			report_error_undef_pred(TypeInfo1, PredId,
				IOState0, IOState)
		),
		typeinfo_set_io_state(TypeInfo1, IOState, TypeInfo2),
		typeinfo_set_found_error(TypeInfo2, yes, TypeInfo)
	).

%-----------------------------------------------------------------------------%

	% Rename apart the type variables in called predicate's arg types.
	%
	% Each type_assign has it's own set of type variables, but these
	% are supposed to stay in synch with each other.  We need to
	% iterate over the set of type_assigns, but we check that
	% the resulting renamed apart list of predicate arg types
	% is the same for each type_assign (i.e. that the tvarsets
	% were indeed in synch).

:- pred rename_apart(type_info, tvarset, list(type), type_info, list(type)).
:- mode rename_apart(typeinfo_di, in, in, typeinfo_uo, out).

rename_apart(TypeInfo0, PredTypeVarSet, PredArgTypes0, TypeInfo, PredArgTypes)
		:-
	typeinfo_get_type_assign_set(TypeInfo0, TypeAssignSet0),
	( TypeAssignSet0 = [TypeAssign0 | TypeAssigns0] ->
			% process the first type_assign and get
			% the resulting PredArgTypes
		type_assign_rename_apart(TypeAssign0, PredTypeVarSet,
				PredArgTypes0, TypeAssign, PredArgTypes),
			% process the remaining type_assigns and check
			% that they produce matching PredArgTypes
		rename_apart_2(TypeAssigns0, PredTypeVarSet, PredArgTypes0,
				  TypeAssigns, PredArgTypes),
		TypeAssignSet = [TypeAssign | TypeAssigns],
		typeinfo_set_type_assign_set(TypeInfo0, TypeAssignSet, TypeInfo)
	;
		TypeInfo = TypeInfo0
	).

:- pred rename_apart_2(type_assign_set, tvarset, list(type),
			type_assign_set, list(type)).
:- mode rename_apart_2(in, in, in, out, in).

rename_apart_2([], _, _, [], _).
rename_apart_2([TypeAssign0 | TypeAssigns0], PredTypeVarSet, PredArgTypes0,
		[TypeAssign | TypeAssigns], PredArgTypes) :-
	type_assign_rename_apart(TypeAssign0, PredTypeVarSet, PredArgTypes0,
			TypeAssign, NewPredArgTypes),
	(PredArgTypes = NewPredArgTypes ->
		true
	;
		error("problem synchronizing type vars")
	),
	rename_apart_2(TypeAssigns0, PredTypeVarSet, PredArgTypes0,
			TypeAssigns, PredArgTypes).

:- pred type_assign_rename_apart(type_assign, tvarset, list(type),
			type_assign, list(type)).
:- mode type_assign_rename_apart(in, in, in, out, out).

type_assign_rename_apart(TypeAssign0, PredTypeVarSet, PredArgTypes0,
		TypeAssign, PredArgTypes) :-
	type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
	varset__merge(TypeVarSet0, PredTypeVarSet, PredArgTypes0,
			  TypeVarSet, PredArgTypes),
	type_assign_set_typevarset(TypeAssign0, TypeVarSet, TypeAssign).

%-----------------------------------------------------------------------------%

	% Given a list of variables and a list of types, ensure
	% that each variable has the corresponding type.

:- pred typecheck_var_has_type_list(list(var), list(type), int, type_info,
					type_info).
:- mode typecheck_var_has_type_list(in, in, in, in, out).

typecheck_var_has_type_list([], [], _) --> [].
typecheck_var_has_type_list([Var|Vars], [Type|Types], ArgNum) -->
	typeinfo_set_arg_num(ArgNum),
	typecheck_var_has_type(Var, Type),
	{ ArgNum1 is ArgNum + 1 },
	typecheck_var_has_type_list(Vars, Types, ArgNum1).

:- pred typecheck_var_has_type(var, type, type_info, type_info).
:- mode typecheck_var_has_type(in, in, typeinfo_di, typeinfo_uo).

typecheck_var_has_type(VarId, Type, TypeInfo0, TypeInfo) :-
	typeinfo_get_type_assign_set(TypeInfo0, TypeAssignSet0),
	typeinfo_get_head_type_params(TypeInfo0, HeadTypeParams),
	typecheck_var_has_type_2(TypeAssignSet0, HeadTypeParams, VarId, Type,
			[], TypeAssignSet),
	(
		TypeAssignSet = [],
		TypeAssignSet0 \= []
	->
		typeinfo_get_io_state(TypeInfo0, IOState0),
		report_error_var(TypeInfo0, VarId, 
				Type, TypeAssignSet0, IOState0, IOState),
		typeinfo_set_io_state(TypeInfo0, IOState, TypeInfo1),
		typeinfo_set_found_error(TypeInfo1, yes, TypeInfo)
	;
		typeinfo_set_type_assign_set(TypeInfo0, TypeAssignSet, TypeInfo)
	).

	% Given a type assignment set and a variable id,
	% return the list of possible different types for the variable.

:- type type_stuff ---> type_stuff(type, tvarset, tsubst).

:- pred get_type_stuff(type_assign_set, var, list(type_stuff)).
:- mode get_type_stuff(in, in, out).
get_type_stuff([], _VarId, []).
get_type_stuff([TypeAssign | TypeAssigns], VarId, L) :-
	get_type_stuff(TypeAssigns, VarId, L0),
	type_assign_get_type_bindings(TypeAssign, TypeBindings),
	type_assign_get_typevarset(TypeAssign, TVarSet),
	type_assign_get_var_types(TypeAssign, VarTypes),
	( %%% if some [Type0]
		map__search(VarTypes, VarId, Type0)
	->
		Type = Type0
	;
		% this shouldn't happen - how can a variable which has
		% not yet been assigned a type variable fail to have
		% the correct type?
		%%% error("problem in type unification")
		term__context_init(0, Context),
		Type = term_functor(term_atom("<any>"), [], Context)
	),
	TypeStuff = type_stuff(Type, TVarSet, TypeBindings),
	(
		member_chk(TypeStuff, L0)
	->
		L = L0
	;
		L = [TypeStuff | L0]
	).

:- type headtypes == list(var).

:- pred typecheck_var_has_type_2(type_assign_set, headtypes, var, type,
				type_assign_set, type_assign_set).
:- mode typecheck_var_has_type_2(in, in, in, in, in, out).

typecheck_var_has_type_2([], _, _, _) --> [].
typecheck_var_has_type_2([TypeAssign0 | TypeAssignSet0], HeadTypeParams, VarId,
		Type) -->
	type_assign_var_has_type(TypeAssign0, HeadTypeParams, VarId, Type),
	typecheck_var_has_type_2(TypeAssignSet0, HeadTypeParams, VarId, Type).

:- pred type_assign_var_has_type(type_assign, headtypes, var, type,
				type_assign_set, type_assign_set).
:- mode type_assign_var_has_type(in, in, in, in, in, out).

type_assign_var_has_type(TypeAssign0, HeadTypeParams, VarId, Type,
		TypeAssignSet0, TypeAssignSet) :-
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	( %%% if some [VarType]
		map__search(VarTypes0, VarId, VarType)
	->
		( %%% if some [TypeAssign1]
			type_assign_unify_type(TypeAssign0, HeadTypeParams,
				VarType, Type, TypeAssign1)
		->
			TypeAssignSet = [TypeAssign1 | TypeAssignSet0]
		;
			TypeAssignSet = TypeAssignSet0
		)
	;
		map__set(VarTypes0, VarId, Type, VarTypes),
		type_assign_set_var_types(TypeAssign0, VarTypes, TypeAssign),
		TypeAssignSet = [TypeAssign | TypeAssignSet0]
	).

%-----------------------------------------------------------------------------%
	
:- pred typecheck_term_has_type_list(list(term), list(type), int,
					type_info, type_info).
:- mode typecheck_term_has_type_list(in, in, in, typeinfo_di, typeinfo_uo).

typecheck_term_has_type_list([], [], _) --> [].
typecheck_term_has_type_list([Arg | Args], [Type | Types], N) -->
	{ N1 is N + 1 },
	typeinfo_set_arg_num(N1),
	typecheck_term_has_type(Arg, Type),
	typecheck_term_has_type_list(Args, Types, N1).

:- pred typecheck_term_has_type(term, type, type_info, type_info).
:- mode typecheck_term_has_type(in, in, typeinfo_di, typeinfo_uo).

typecheck_term_has_type(term_variable(Var), Type, TypeInfo0, TypeInfo) :-
	typecheck_var_has_type(Var, Type, TypeInfo0, TypeInfo).

typecheck_term_has_type(term_functor(F, As, _), Type, TypeInfo0, TypeInfo) :-
	length(As, Arity),
	typeinfo_get_ctor_list(TypeInfo0, F, Arity, ConsDefnList),
	( ConsDefnList = [] ->
	    typeinfo_get_io_state(TypeInfo0, IOState0),
	    report_error_undef_cons(TypeInfo0, F, Arity, IOState0, IOState),
	    typeinfo_set_io_state(TypeInfo0, IOState, TypeInfo1),
	    typeinfo_set_found_error(TypeInfo1, yes, TypeInfo)
	;
	    typeinfo_get_type_assign_set(TypeInfo0, TypeAssignSet0),
	    typecheck_cons_has_type(TypeAssignSet0, ConsDefnList, As, Type,
			TypeInfo0, [], TypeAssignSet),
	    (
		TypeAssignSet = [],
		TypeAssignSet0 \= []
	    ->
		typeinfo_get_io_state(TypeInfo0, IOState0),
		report_error_cons(TypeInfo0, F, As, Type, TypeAssignSet0,
					IOState0, IOState),
		typeinfo_set_io_state(TypeInfo0, IOState, TypeInfo1),
		typeinfo_set_found_error(TypeInfo1, yes, TypeInfo)
	    ;
		typeinfo_set_type_assign_set(TypeInfo0, TypeAssignSet, TypeInfo)
	    )
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

:- pred typecheck_cons_has_type(type_assign_set, list(cons_type_info),
		list(term), type, type_info, type_assign_set, type_assign_set).
:- mode typecheck_cons_has_type(in, in, in, in, typeinfo_ui, in, out).

typecheck_cons_has_type([], _, _, _, _) --> [].
typecheck_cons_has_type([TypeAssign|TypeAssigns], ConsDefnList, Args, Type, 
		TypeInfo) -->
	type_assign_cons_has_type(ConsDefnList, TypeAssign, Args, Type,
		TypeInfo),
	typecheck_cons_has_type(TypeAssigns, ConsDefnList, Args, Type,
		TypeInfo).

%-----------------------------------------------------------------------------%

	% For each possible constructor which matches the
	% term (overloading means that there may be more than one),
	% if this constructor matches the specified type and
	% the types of it's arguments are ok, then add the resulting
	% type assignment to the type assignment set.

:- pred type_assign_cons_has_type(list(cons_type_info), type_assign,
		list(term), type, type_info, type_assign_set, type_assign_set).
:- mode type_assign_cons_has_type(in, in, in, in, typeinfo_ui, in, out).

type_assign_cons_has_type([], _TypeAssign0, _Args, _Type, _TypeInfo) -->
	[].
type_assign_cons_has_type([ConsDefn | ConsDefns], TypeAssign0, Args, Type,
		TypeInfo) -->
	type_assign_cons_has_type_2(ConsDefn, TypeAssign0, Args, Type,
		TypeInfo),
	type_assign_cons_has_type(ConsDefns, TypeAssign0, Args, Type, TypeInfo).

:- pred type_assign_cons_has_type_2(cons_type_info, type_assign, list(term),
		type, type_info, type_assign_set, type_assign_set).
:- mode type_assign_cons_has_type_2(in, in, in, in, typeinfo_ui, in, out).

type_assign_cons_has_type_2(ConsDefn, TypeAssign0, Args, Type, TypeInfo,
		TypeAssignSet0, TypeAssignSet) :-

	get_cons_stuff(ConsDefn, TypeAssign0, TypeInfo,
			ConsType, ArgTypes, TypeAssign1),
	
	typeinfo_get_head_type_params(TypeInfo, HeadTypeParams),

	(
		type_assign_unify_type(TypeAssign1, HeadTypeParams,
			ConsType, Type, TypeAssign2)
	->
			% check the types of the arguments
		type_assign_term_has_type_list(Args, ArgTypes, TypeAssign2,
			TypeInfo, TypeAssignSet0, TypeAssignSet)
	;
		TypeAssignSet = TypeAssignSet0
	).

%-----------------------------------------------------------------------------%

	% type_assign_term_has_type_list(Terms, Types, TypeAssign, TypeInfo,
	%		TypeAssignSet0, TypeAssignSet):
	% 	Let TAs = { TA | TA is a an extension of TypeAssign
	%		    	 for which the types of the Terms unify with
	%		    	 their respective Types },
	% 	append(TAs, TypeAssignSet0, TypeAssignSet).

:- pred type_assign_term_has_type_list(list(term), list(type), type_assign,
			type_info, type_assign_set, type_assign_set).
:- mode type_assign_term_has_type_list(in, in, in, typeinfo_ui, in, out).

type_assign_term_has_type_list([], [], TypeAssign, _,
		TypeAssignSet, [TypeAssign|TypeAssignSet]).
type_assign_term_has_type_list([Arg | Args], [Type | Types], TypeAssign0,
		TypeInfo, TypeAssignSet0, TypeAssignSet) :-
	type_assign_term_has_type(Arg, Type, TypeAssign0, TypeInfo,
		[], TypeAssignSet1),
	type_assign_list_term_has_type_list(TypeAssignSet1,
		Args, Types, TypeInfo, TypeAssignSet0, TypeAssignSet).

	% type_assign_list_term_has_type_list(TAs, Terms, Types, 
	%		TypeInfo, TypeAssignSet0, TypeAssignSet):
	% 	Let TAs2 = { TA | TA is a an extension of a member of TAs
	%		    	  for which the types of the Terms unify with
	%		    	  their respective Types },
	% 	append(TAs, TypeAssignSet0, TypeAssignSet).

:- pred type_assign_list_term_has_type_list(type_assign_set, list(term),
		list(type), type_info, type_assign_set, type_assign_set).
:- mode type_assign_list_term_has_type_list(in, in, in, typeinfo_ui, in, out).

type_assign_list_term_has_type_list([], _, _, _) --> [].
type_assign_list_term_has_type_list([TA | TAs], Args, Types, TypeInfo) -->
	type_assign_term_has_type_list(Args, Types, TA, TypeInfo),
	type_assign_list_term_has_type_list(TAs, Args, Types, TypeInfo).
	
:- pred type_assign_term_has_type(term, type, type_assign,
			type_info, type_assign_set, type_assign_set).
:- mode type_assign_term_has_type(in, in, in, typeinfo_ui, in, out).

type_assign_term_has_type(term_variable(V), Type, TypeAssign, TypeInfo) -->
	{ typeinfo_get_head_type_params(TypeInfo, HeadTypeParams) },
	type_assign_var_has_type(TypeAssign, HeadTypeParams, V, Type).
type_assign_term_has_type(term_functor(F, Args, _Context), Type, TypeAssign,
		TypeInfo) -->
	{ length(Args, Arity) },
	{ typeinfo_get_ctor_list(TypeInfo, F, Arity, ConsDefnList) },
	type_assign_cons_has_type(ConsDefnList, TypeAssign, Args, Type,
		TypeInfo).

%-----------------------------------------------------------------------------%

	% used for debugging

:- pred checkpoint(string, type_info, type_info).
:- mode checkpoint(in, typeinfo_di, typeinfo_uo).

checkpoint(Msg, T0, T) :-
	typeinfo_get_io_state(T0, I0),
		% we should use a different option
	lookup_option(very_verbose, bool(DoCheckPoint), I0, I1),
	( DoCheckPoint = yes ->
		checkpoint_2(Msg, T0, I1, I)
	;	
		I = I1
	),
	typeinfo_set_io_state(T0, I, T).

:- pred checkpoint_2(string, type_info, io__state, io__state).
:- mode checkpoint_2(in, typeinfo_no_io, di, uo).

checkpoint_2(Msg, T0) -->
	io__write_string("At "),
	io__write_string(Msg),
	io__write_string(": "),
	lookup_option(statistics, bool(Statistics)),
	maybe_report_stats(Statistics),
	io__write_string("\n"),
	{ typeinfo_get_type_assign_set(T0, TypeAssignSet) },
	{ typeinfo_get_varset(T0, VarSet) },
	write_type_assign_set(TypeAssignSet, VarSet).

%-----------------------------------------------------------------------------%

	% Type check a unification.
	% Get the type assignment set from the type info and then just
	% iterate over all the possible type assignments.

:- pred typecheck_unification(term, term, type_info, type_info).
:- mode typecheck_unification(in, in, typeinfo_di, typeinfo_uo).

:- typecheck_unification(X, Y, _, _) when X and Y.

typecheck_unification(term_variable(X), term_variable(Y)) -->
	typecheck_unify_var_var(X, Y).
typecheck_unification(term_functor(F, As, C), term_variable(Y)) -->
	typecheck_unify_var_functor(Y, F, As, C).
typecheck_unification(term_variable(X), term_functor(F, As, C)) -->
	typecheck_unify_var_functor(X, F, As, C).
typecheck_unification(term_functor(_, _, _), term_functor(_, _, _)) -->
	{ error("not implemented: unification of term with term\n") }.

:- pred typecheck_unify_var_var(var, var, type_info, type_info).
:- mode typecheck_unify_var_var(in, in, typeinfo_di, typeinfo_uo).

typecheck_unify_var_var(X, Y, TypeInfo0, TypeInfo) :-
	typeinfo_get_type_assign_set(TypeInfo0, TypeAssignSet0),
	typecheck_unification_2(TypeAssignSet0, term_variable(X),
		term_variable(Y), TypeInfo0, [], TypeAssignSet),
	( TypeAssignSet = [], TypeAssignSet0 \= [] ->
		typeinfo_get_io_state(TypeInfo0, IOState0),
		report_error_unif_var_var(TypeInfo0, X, Y, TypeAssignSet0,
					IOState0, IOState1),
		typeinfo_set_io_state(TypeInfo0, IOState1, TypeInfo1),
		typeinfo_set_found_error(TypeInfo1, yes, TypeInfo)
	;
		typeinfo_set_type_assign_set(TypeInfo0, TypeAssignSet, TypeInfo)
	).

:- pred typecheck_unify_var_functor(var, const, list(term), term__context,
					type_info, type_info).
:- mode typecheck_unify_var_functor(in, in, in, in, di, uo).

typecheck_unify_var_functor(Var, Functor, Args, Context, TypeInfo9, TypeInfo) :-
	typeinfo_set_context(Context, TypeInfo9, TypeInfo0),
	length(Args, Arity),
	typeinfo_get_ctor_list(TypeInfo0, Functor, Arity, ConsDefnList),
	( ConsDefnList = [] ->
		typeinfo_get_io_state(TypeInfo0, IOState0),
		report_error_undef_cons(TypeInfo0, Functor, Arity, 
					IOState0, IOState1),
		typeinfo_set_io_state(TypeInfo0, IOState1, TypeInfo1),
		typeinfo_set_found_error(TypeInfo1, yes, TypeInfo)
	;
		typeinfo_get_type_assign_set(TypeInfo0, TypeAssignSet0),
		typecheck_unify_var_functor_2a(TypeAssignSet0,
			TypeInfo0, ConsDefnList, [], ConsTypeAssignSet),
								% XXX Args
		typecheck_unify_var_functor_3a( ConsTypeAssignSet, Var, Args,
			TypeInfo0, [], TypeAssignSet),
		( TypeAssignSet = [], TypeAssignSet0 \= [] ->
			typeinfo_get_io_state(TypeInfo0, IOState0),
			report_error_unif_var_functor(TypeInfo0,
				Var, Functor, Args, TypeAssignSet0,
				IOState0, IOState1),
			typeinfo_set_io_state(TypeInfo0, IOState1, TypeInfo1),
			typeinfo_set_found_error(TypeInfo1, yes, TypeInfo)
		;
			typeinfo_set_type_assign_set(TypeInfo0, TypeAssignSet,
				TypeInfo)
			% XXX typecheck_unify_args
		)
	).

	% typecheck_unify_var_functor_2a(TypeAssignSet, TypeInfo, ConsDefns):
	%	
	% Iterate over all the different possible type assignments.
	% For each type assignment in `TypeAssignSet', produce a pair 
	%
	%	TypeAssign - [cons_type(Type1, ArgTypes1), ...])
	%
	% where each `cons_type(Type, ArgTypes)' records one of the possible
	% types for the constructor in `ConsDefns', and where `TypeAssign' is
	% the type assignment renamed apart from the types of the constructors.

:- type cons_type ---> cons_type(type, list(type)).
:- type cons_type_set == list(cons_type).
:- type cons_type_assign_set == list(pair(type_assign, cons_type_set)).

:- pred typecheck_unify_var_functor_2a(type_assign_set,
				type_info, list(cons_type_info),
				cons_type_assign_set, cons_type_assign_set).
:- mode typecheck_unify_var_functor_2a(in, in, in, in, out).

	% Iterate over the type assign sets

typecheck_unify_var_functor_2a([], _, _) --> [].
typecheck_unify_var_functor_2a([TypeAssign0 | TypeAssigns], TypeInfo,
		ConsDefns) -->
	{ typecheck_unify_var_functor_2b(ConsDefns, TypeInfo,
		TypeAssign0, TypeAssign, [], ConsTypeAssignSet) },
	append([TypeAssign - ConsTypeAssignSet]),
	typecheck_unify_var_functor_2a(TypeAssigns, TypeInfo, ConsDefns).

	% Iterate over all the different cons defns.

:- pred typecheck_unify_var_functor_2b(list(cons_type_info), type_info,
				type_assign, type_assign,
				cons_type_set, cons_type_set).
:- mode typecheck_unify_var_functor_2b(in, in, in, out, in, out).

typecheck_unify_var_functor_2b([], _, TypeAssign, TypeAssign) --> [].
typecheck_unify_var_functor_2b([ConsDefn | ConsDefns], TypeInfo, TypeAssign0,
		TypeAssign)
		-->
	{ get_cons_stuff(ConsDefn, TypeAssign0, TypeInfo,
			ConsType, ArgTypes, TypeAssign1) },
	append([cons_type(ConsType, ArgTypes)]),
	typecheck_unify_var_functor_2b(ConsDefns, TypeInfo,
			TypeAssign1, TypeAssign).

	% typecheck_unify_var_functor_3a(ConsTypeAssignSet, Var, Args, ...):
	%
	% For each possible cons type assignment in `ConsTypeAssignSet',
	% for each possible constructor type and argument types,
	% check that the types of `Var' and `Args' matches these types.

:- pred typecheck_unify_var_functor_3a(cons_type_assign_set, var, list(term),
				type_info, type_assign_set, type_assign_set).
:- mode typecheck_unify_var_functor_3a(in, in, in, in, in, out).

typecheck_unify_var_functor_3a([], _, _, _) --> [].
typecheck_unify_var_functor_3a([TypeAssign - ConsTypes | ConsTypeAssigns],
		Var, Args, TypeInfo) -->
	typecheck_unify_var_functor_3b(ConsTypes, TypeAssign,
			Var, Args, TypeInfo),
	typecheck_unify_var_functor_3a(ConsTypeAssigns, Var, Args, TypeInfo).

	% typecheck_unify_var_functor_3b(ConsTypes, TypeAssign, Var, Args, ...):
	%
	% For each possible constructor type and argument types in `ConsTypes',
	% check that the types of `Var' and `Args' matches these types.

:- pred typecheck_unify_var_functor_3b(list(cons_type), type_assign,
				var, list(term), type_info,
				type_assign_set, type_assign_set).
:- mode typecheck_unify_var_functor_3b(in, in, in, in, in, in, out).

typecheck_unify_var_functor_3b([], _, _, _, _) --> [].
typecheck_unify_var_functor_3b([ConsType | ConsTypes],
			TypeAssign, Var, Args, TypeInfo) -->
	{ ConsType = cons_type(Type, ArgTypes) },
	type_assign_unify_var_functor_2(Type, ArgTypes,
			Args, Var, TypeAssign, TypeInfo),
	typecheck_unify_var_functor_3b(ConsTypes, TypeAssign,
			Var, Args, TypeInfo).


/******** OLD JUNK
:- pred typecheck_unification_old(term, term, type_info, type_info).
:- mode typecheck_unification_old(in, in, typeinfo_di, typeinfo_uo).

typecheck_unification_old(X, Y, TypeInfo0, TypeInfo) :-
	typeinfo_get_type_assign_set(TypeInfo0, TypeAssignSet0),
	typecheck_unification_2(TypeAssignSet0, X, Y, TypeInfo0,
		[], TypeAssignSet),
		% XXX report errors properly!!
	( TypeAssignSet = [], TypeAssignSet0 \= [] ->
		typeinfo_get_io_state(TypeInfo0, IOState0),
		report_error_unif(TypeInfo0, X, Y,
				TypeAssignSet0, IOState0, IOState1),
		typeinfo_set_io_state(TypeInfo0, IOState1, TypeInfo1),
		typeinfo_set_found_error(TypeInfo1, yes, TypeInfo)
	;
		typeinfo_set_type_assign_set(TypeInfo0, TypeAssignSet, TypeInfo)
	).
**************/

	% iterate over all the possible type assignments.

:- pred typecheck_unification_2(type_assign_set, term, term,
				type_info, type_assign_set, type_assign_set).
:- mode typecheck_unification_2(in, in, in, typeinfo_ui, in, out).

typecheck_unification_2([], _, _, _) --> [].
typecheck_unification_2([TypeAssign0 | TypeAssigns0], X, Y, TypeInfo) -->
	type_assign_unify_term(X, Y, TypeAssign0, TypeInfo),
	typecheck_unification_2(TypeAssigns0, X, Y, TypeInfo).
	
%-----------------------------------------------------------------------------%

	% Type-check the unification of two terms,
	% and update the type assignment.
	% TypeAssign0 is the type assignment we are updating,
	% TypeAssignSet0 is an accumulator for the list of possible
	% type assignments so far, and TypeAssignSet is TypeAssignSet plus
	% any type assignment(s) resulting from TypeAssign0 and this
	% unification.

:- pred type_assign_unify_term(term, term, type_assign, type_info,
				type_assign_set, type_assign_set).
:- mode type_assign_unify_term(in, in, in, typeinfo_ui, in, out).

	% NU-Prolog indexing
:- type_assign_unify_term(T1, T2, _, _, _, _) when T1 and T2.

type_assign_unify_term(term_variable(X), term_variable(Y), TypeAssign0,
		TypeInfo, TypeAssignSet0, TypeAssignSet) :-
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	(
		map__search(VarTypes0, X, TypeX)
	->
		(
			map__search(VarTypes0, Y, TypeY)
		->
			% both X and Y already have types - just
			% unify their types
			typeinfo_get_head_type_params(TypeInfo, HeadTypeParams),
			( 
				type_assign_unify_type(TypeAssign0,
					HeadTypeParams, TypeX, TypeY,
					TypeAssign3)
			->
				TypeAssignSet = [TypeAssign3 | TypeAssignSet0]
			;
				TypeAssignSet = TypeAssignSet0
			)
		;
			% Y is a fresh variable which hasn't been
			% assigned a type yet
			map__set(VarTypes0, Y, TypeX, VarTypes),
			type_assign_set_var_types(TypeAssign0, VarTypes,
				TypeAssign),
			TypeAssignSet = [TypeAssign | TypeAssignSet0]
		)
	;
		(
			map__search(VarTypes0, Y, TypeY)
		->
			% X is a fresh variable which hasn't been
			% assigned a type yet
			map__set(VarTypes0, X, TypeY, VarTypes),
			type_assign_set_var_types(TypeAssign0, VarTypes,
				TypeAssign),
			TypeAssignSet = [TypeAssign | TypeAssignSet0]
		;
			% both X and Y are fresh variables -
			% introduce a fresh type variable to represent
			% their type
			type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
			varset__new_var(TypeVarSet0, TypeVar, TypeVarSet),
			type_assign_set_typevarset(TypeAssign0, TypeVarSet,
				TypeAssign1),
			Type = term_variable(TypeVar),
			map__set(VarTypes0, X, Type, VarTypes1),
			map__set(VarTypes1, Y, Type, VarTypes),
			type_assign_set_var_types(TypeAssign1, VarTypes,
				TypeAssign),
			TypeAssignSet = [TypeAssign | TypeAssignSet0]
		)
	).

type_assign_unify_term(term_functor(Functor, Args, _), term_variable(Y),
		TypeAssign0, TypeInfo, TypeAssignSet0, TypeAssignSet) :-
	length(Args, Arity),
	typeinfo_get_ctor_list(TypeInfo, Functor, Arity, ConsDefnList),
	type_assign_unify_var_functor(ConsDefnList, Args, Y, TypeAssign0,
		TypeInfo, TypeAssignSet0, TypeAssignSet).

type_assign_unify_term(term_variable(Y), term_functor(F, As, _), TypeAssign0,
		TypeInfo, TypeAssignSet0, TypeAssignSet) :-
	type_assign_unify_term(term_functor(F, As, _), term_variable(Y),
		TypeAssign0, TypeInfo, TypeAssignSet0, TypeAssignSet).
	
type_assign_unify_term(term_functor(_, _, _), term_functor(_, _, _),
		_, _, TypeAssignSet, TypeAssignSet) :-
	    % We don't handle this, because it shouldn't occur
	    % if the code is in superhomogeneous form, and we plan to
	    % convert it to superhomogeneous form before doing type-checking.
	error("Unexpected unification of term with term\n").

%-----------------------------------------------------------------------------%

	% Type-check the unification of a variable with a functor:
	% for each possible type of the constructor,
	% unify the type of the variable with the type of
	% the constructor and if this succeeds insert that
	% type assignment into the type assignment set.

:- pred type_assign_unify_var_functor(list(cons_type_info), list(term),
		var, type_assign,
		type_info, type_assign_set, type_assign_set).
:- mode type_assign_unify_var_functor(in, in, in, in, typeinfo_ui, in, out).

	% loop over all the possible cons defns

type_assign_unify_var_functor([], _, _, _, _) --> [].
type_assign_unify_var_functor([ConsDefn | ConsDefns],
			Args, Var, TypeAssign0, TypeInfo) -->
	{ get_cons_stuff(ConsDefn, TypeAssign0, TypeInfo,
			ConsType, ArgTypes, TypeAssign1) },
	type_assign_unify_var_functor_2(ConsType, ArgTypes,
			Args, Var, TypeAssign1, TypeInfo),
	type_assign_unify_var_functor(ConsDefns,
			Args, Var, TypeAssign0, TypeInfo).


:- pred type_assign_unify_var_functor_2(type, list(type), list(term),
		var, type_assign, type_info,
		type_assign_set, type_assign_set).
:- mode type_assign_unify_var_functor_2(in, in, in, in, in, typeinfo_ui,
		in, out).

	% unify the type of the variable with the type of
	% the constructor and if this succeeds insert that
	% type assignment into the type assignment set.

type_assign_unify_var_functor_2(ConsType, ArgTypes, Args, Y, TypeAssign1,
		TypeInfo, TypeAssignSet0, TypeAssignSet) :-

		% unify the type of Var with the type of the constructor
	type_assign_get_var_types(TypeAssign1, VarTypes0),
	( %%% if some [TypeY]
		map__search(VarTypes0, Y, TypeY)
	->
		typeinfo_get_head_type_params(TypeInfo, HeadTypeParams),
		( %%% if some [TypeAssign2]
			type_assign_unify_type(TypeAssign1, HeadTypeParams,
					ConsType, TypeY, TypeAssign2)
		->
			% check that the types of the arguments matches the
			% specified arg types for this constructor
			type_assign_term_has_type_list(Args, ArgTypes,
				TypeAssign2, TypeInfo,
				TypeAssignSet0, TypeAssignSet)
		;
			% the top-level types didn't unify - no need to
			% check the types of the arguments, since this
			% type-assignment has already been rules out
			TypeAssignSet = TypeAssignSet0
		)
	;
		map__set(VarTypes0, Y, ConsType, VarTypes),
		type_assign_set_var_types(TypeAssign1, VarTypes, TypeAssign3),

			% check that the types of the arguments matches the
			% specified arg types for this constructor
		type_assign_term_has_type_list(Args, ArgTypes, TypeAssign3,
			TypeInfo, TypeAssignSet0, TypeAssignSet)
	).

%-----------------------------------------------------------------------------%

	% Given an cons_type_info, construct a type for the
	% constructor and a list of types of the arguments,
	% suitable renamed apart from the current type_assign's
	% typevarset.

:- pred get_cons_stuff(cons_type_info, type_assign, type_info,
			type, list(type), type_assign).
:- mode get_cons_stuff(in, in, in, out, out, out).

get_cons_stuff(ConsDefn, TypeAssign0, _TypeInfo, ConsType, ArgTypes,
			TypeAssign) :-

	ConsDefn = cons_type_info(ConsTypeVarSet, ConsType0, ArgTypes0),

	% Rename apart the type vars in the type of the constructor
	% and the types of it's arguments.
	% (Optimize the common case of a non-polymorphic type)

	ConsType0 = term_functor(_, ConsTypeParams, _),
	( ConsTypeParams = [] ->
		ConsType = ConsType0,
		ArgTypes = ArgTypes0,
		TypeAssign = TypeAssign0
	;
		type_assign_rename_apart(TypeAssign0, ConsTypeVarSet,
			[ConsType0 | ArgTypes0],
			TypeAssign, [ConsType | ArgTypes])
	).

%-----------------------------------------------------------------------------%

	% Unify (with occurs check) two types in a type assignment 
	% and update the type bindings.

:- pred type_assign_unify_type(type_assign, headtypes, type, type, type_assign).
:- mode type_assign_unify_type(in, in, in, in, out).

type_assign_unify_type(TypeAssign0, HeadTypeParams, X, Y, TypeAssign) :-
	type_assign_get_type_bindings(TypeAssign0, TypeBindings0),
	type_unify(X, Y, HeadTypeParams, TypeBindings0, TypeBindings),
	type_assign_set_type_bindings(TypeAssign0, TypeBindings, TypeAssign).

%-----------------------------------------------------------------------------%

	% Unify (with occurs check) two types with respect to a type
	% substitution and update the type bindings.
	% Types are represented as terms, but we can't just use term__unify
	% because we need to avoid binding any of the "head type params"
	% (the type variables that occur in the head of the clause),
	% and because one day we might want to handle equivalent types.

:- type_unify(X, Y, _, _, _) when X and Y.		% NU-Prolog indexing

:- pred type_unify(type, type, headtypes, substitution, substitution).
:- mode type_unify(in, in, in, in, out).

type_unify(term_variable(X), term_variable(Y), HeadTypeParams, Bindings0,
		Bindings) :-
	( member(Y, HeadTypeParams) ->
		type_unify_head_type_param(X, Y, HeadTypeParams,
			Bindings0, Bindings)
	; member(X, HeadTypeParams) ->
		type_unify_head_type_param(Y, X, HeadTypeParams,
			Bindings0, Bindings)
	; map__search(Bindings0, X, BindingOfX) ->
		( map__search(Bindings0, Y, BindingOfY) ->
			% both X and Y already have bindings - just
			% unify the types they are bound to
			type_unify(BindingOfX, BindingOfY, HeadTypeParams,
					Bindings0, Bindings)
		;
			term__apply_rec_substitution(BindingOfX,
				Bindings0, SubstBindingOfX),
			% Y is a type variable which hasn't been bound yet
			( SubstBindingOfX = term_variable(Y) ->
				Bindings = Bindings0
			;
				\+ term__occurs(SubstBindingOfX, Y, Bindings0),
				map__set(Bindings0, Y, SubstBindingOfX,
					Bindings)
			)
		)
	;
		( map__search(Bindings0, Y, BindingOfY) ->
			term__apply_rec_substitution(BindingOfY,
				Bindings0, SubstBindingOfY),
			% X is a type variable which hasn't been bound yet
			( SubstBindingOfY = term_variable(X) ->
				Bindings = Bindings0
			;
				\+ term__occurs(SubstBindingOfY, X, Bindings0),
				map__set(Bindings0, X, SubstBindingOfY,
					Bindings)
			)
		;
			% both X and Y are unbound type variables -
			% bind one to the other
			( X = Y ->
				Bindings = Bindings0
			; 
				map__set(Bindings0, X, term_variable(Y),
					Bindings)
			)
		)
	).

type_unify(term_variable(X), term_functor(F, As, C), HeadTypeParams, Bindings0,
		Bindings) :-
	( 
		map__search(Bindings0, X, BindingOfX)
	->
		type_unify(BindingOfX, term_functor(F, As, C), HeadTypeParams,
			Bindings0, Bindings)
	;
		\+ term__occurs_list(As, X, Bindings0),
		\+ member(X, HeadTypeParams),
		map__set(Bindings0, X, term_functor(F, As, C), Bindings)
	).

type_unify(term_functor(F, As, C), term_variable(X), HeadTypeParams, Bindings0,
		Bindings) :-
	( 
		map__search(Bindings0, X, BindingOfX)
	->
		type_unify(term_functor(F, As, C), BindingOfX, HeadTypeParams,
			Bindings0, Bindings)
	;
		\+ term__occurs_list(As, X, Bindings0),
		\+ member(X, HeadTypeParams),
		map__set(Bindings0, X, term_functor(F, As, C), Bindings)
	).

type_unify(term_functor(FX, AsX, _), term_functor(FY, AsY, _), HeadTypeParams,
		Bindings0, Bindings) :-
	length(AsX, ArityX),
	length(AsY, ArityY),
	(
		FX = FY,
		ArityX = ArityY
	->
		type_unify_list(AsX, AsY, HeadTypeParams, Bindings0, Bindings)
	;
		% YYY we should check here if these types have been defined
		% to be equivalent using equivalence types
		fail
	).

:- pred type_unify_list(list(type), list(type), headtypes, substitution,
			substitution).
:- mode type_unify_list(in, in, in, in, out).

type_unify_list([], [], _) --> [].
type_unify_list([X | Xs], [Y | Ys], HeadTypeParams) -->
	type_unify(X, Y, HeadTypeParams),
	type_unify_list(Xs, Ys, HeadTypeParams).

:- pred type_unify_head_type_param(var, var, headtypes, substitution,
				substitution).
:- mode type_unify_head_type_param(in, in, in, in, out).

type_unify_head_type_param(Var, HeadVar, HeadTypeParams, Bindings0,
		Bindings) :-
	( map__search(Bindings0, Var, BindingOfVar) ->
		BindingOfVar = term_variable(Var2),
		type_unify_head_type_param(Var2, HeadVar, HeadTypeParams,
			Bindings0, Bindings)
	;
		( Var = HeadVar ->
			Bindings = Bindings0
		;
			\+ member(Var, HeadTypeParams),
			map__set(Bindings0, Var, term_variable(HeadVar),
				Bindings)
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% XXX - At the moment we don't check for circular equivalence types.
	% (If they aren't used, the compiler will probably not
	% detect the error; if they are, it will probably go into
	% an infinite loop).

:- pred check_circular_types(module_info, module_info, io__state, io__state).
:- mode check_circular_types(in, out, di, uo).

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
:- mode check_undefined_types(in, out, di, uo).
check_undefined_types(Module, Module) -->
	{ moduleinfo_types(Module, TypeDefns) },
	{ map__keys(TypeDefns, TypeIds) },
	find_undef_type_bodies(TypeIds, TypeDefns),
	lookup_option(statistics, bool(Statistics)),
	maybe_report_stats(Statistics),
	{ moduleinfo_preds(Module, Preds) },
	{ moduleinfo_predids(Module, PredIds) },
	find_undef_pred_types(PredIds, Preds, TypeDefns).

	% Find any undefined types used in `:- pred' declarations.

:- pred find_undef_pred_types(list(pred_id), pred_table, type_table,
				io__state, io__state).
:- mode find_undef_pred_types(in, in, in, di, uo).

find_undef_pred_types([], _Preds, _TypeDefns) --> [].
find_undef_pred_types([PredId | PredIds], Preds, TypeDefns) -->
	{ map__lookup(Preds, PredId, PredDefn) },
	{ predinfo_arg_types(PredDefn, _VarSet, ArgTypes) },
	{ predinfo_context(PredDefn, Context) },
	find_undef_type_list(ArgTypes, pred(PredId) - Context, TypeDefns),
	find_undef_pred_types(PredIds, Preds, TypeDefns).

	% Find any undefined types used in the bodies of other type
	% declarations.

:- pred find_undef_type_bodies(list(type_id), type_table, io__state, io__state).
:- mode find_undef_type_bodies(in, in, di, uo).

find_undef_type_bodies([], _) --> [].
find_undef_type_bodies([TypeId | TypeIds], TypeDefns) -->
	{ map__lookup(TypeDefns, TypeId, HLDS_TypeDefn) },
		% XXX abstract hlds__type_defn/5
	{ HLDS_TypeDefn = hlds__type_defn(_, _, TypeBody, _, Context) },
	find_undef_type_body(TypeBody, type(TypeId) - Context, TypeDefns),
	find_undef_type_bodies(TypeIds, TypeDefns).

	% Find any undefined types used in the given type definition.

:- pred find_undef_type_body(hlds__type_body, error_context, type_table,
				io__state, io__state).
:- mode find_undef_type_body(in, in, in, di, uo).

find_undef_type_body(eqv_type(Type), ErrorContext, TypeDefns) -->
	find_undef_type(Type, ErrorContext, TypeDefns).
find_undef_type_body(uu_type(Types), ErrorContext, TypeDefns) -->
	find_undef_type_list(Types, ErrorContext, TypeDefns).
find_undef_type_body(du_type(Constructors), ErrorContext, TypeDefns) -->
	find_undef_type_du_body(Constructors, ErrorContext, TypeDefns).
find_undef_type_body(abstract_type, _ErrorContext, _TypeDefns) --> [].

	% Find any undefined types in a list of types.

:- pred find_undef_type_list(list(type), error_context, type_table,
				io__state, io__state).
:- mode find_undef_type_list(in, in, in, di, uo).

find_undef_type_list([], _ErrorContext, _TypeDefns) --> [].
find_undef_type_list([Type|Types], ErrorContext, TypeDefns) -->
	find_undef_type(Type, ErrorContext, TypeDefns),
	find_undef_type_list(Types, ErrorContext, TypeDefns).

	% Find any undefined types in a list of contructors
	% (the constructors for a discrimiated union type).

:- pred find_undef_type_du_body(list(constructor), error_context, type_table,
				io__state, io__state).
:- mode find_undef_type_du_body(in, in, in, di, uo).

find_undef_type_du_body([], _ErrorContext, _TypeDefns) --> [].
find_undef_type_du_body([Constructor | Constructors], ErrorContext,
		TypeDefns) -->
	{ Constructor = _Functor - ArgTypes },
	find_undef_type_list(ArgTypes, ErrorContext, TypeDefns),
	find_undef_type_du_body(Constructors, ErrorContext, TypeDefns).

	% Find any undefined types used in type.
	% The type itself may be undefined, and also
	% any type arguments may also be undefined.
	% (eg. the type `undef1(undef2, undef3)' should generate 3 errors.)

:- pred find_undef_type(type, error_context, type_table,
				io__state, io__state).
:- mode find_undef_type(in, in, in, di, uo).

find_undef_type(term_variable(_), _ErrorContext, _TypeDefns) --> [].
find_undef_type(term_functor(F, As, _), ErrorContext, TypeDefns) -->
	% Efficiency is very important here - this is the inner loop
	% of checking for undefined types.
	% The tests are ordered so as to maximize effiency.
	% Could efficiency be improved further?
	(
		{ is_builtin_atomic_type(TypeId) }
	->
		[]
	;
		{ length(As, Arity) },
		{ make_type_id(F, Arity, TypeId) },
		{ map__contains(TypeDefns, TypeId) }
	->
		[]
	;
		{ is_builtin_pred_type(TypeId) }
	->
		[]
	;
		report_undef_type(TypeId, ErrorContext)
	),
	find_undef_type_list(As, ErrorContext, TypeDefns).

%-----------------------------------------------------------------------------%

	% Given a constant and an arity, return a type_id.
	% XXX this should take a name and an arity;
	% use of integers/floats/strings as type names should
	% be rejected by the parser in prog_io.nl, not here.

:- pred make_type_id(const, int, type_id).
:- mode make_type_id(in, in, out).

make_type_id(term_atom(Name), Arity, unqualified(Name) - Arity).
make_type_id(term_integer(_), _, unqualified("<error>") - 0) :-
	error("atom expected").
make_type_id(term_float(_), _, unqualified("<error>") - 0) :-
	error("atom expected").
make_type_id(term_string(_), _, unqualified("<error>") - 0) :-
	error("atom expected").

%-----------------------------------------------------------------------------%

:- type error_context == pair(error_context_2, term__context).
:- type error_context_2 ---> type(type_id) ; pred(pred_id).

	% Output an error message about an undefined type
	% in the specified context.

:- pred report_undef_type(type_id, error_context, io__state, io__state).
:- mode report_undef_type(in, in, di, uo).

report_undef_type(TypeId, ErrorContext - Context) -->
	prog_out__write_context(Context),
	io__write_string("In definition of "),
	write_error_context(ErrorContext),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  error: undefined type "),
	hlds_out__write_type_id(TypeId),
	io__write_string(".\n").

	% Output a description of the context where an undefined type was
	% used.

:- pred write_error_context(error_context_2, io__state, io__state).
:- mode write_error_context(in, di, uo).

write_error_context(pred(PredId)) -->
	io__write_string("predicate "),
	hlds_out__write_pred_id(PredId).
write_error_context(type(TypeId)) -->
	io__write_string("type "),
	hlds_out__write_type_id(TypeId).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% builtin_atomic_type(Const, TypeName)
	%	If Const is a constant of a builtin atomic type,
	%	instantiates TypeName to the name of that type,
	%	otherwise fails.

:- pred builtin_atomic_type(const, string).
:- mode builtin_atomic_type(in, out) is semidet.

builtin_atomic_type(term_integer(_), "int").
builtin_atomic_type(term_float(_), "float").
builtin_atomic_type(term_string(_), "string").
builtin_atomic_type(term_atom(String), "character") :-
	string__char_to_string(_, String).

	% builtin_pred_type(TypeInfo, Functor, Arity, PredConsInfoList) :
	%	instantiates PredConsInfoList to the set of cons_type_info
	%	structures for each predicate with name `Functor' and arity
	%	greater than or equal to Arity.
	%
	%		PredTypeVarSet, PredTypeParams, ArgTypes) :-
	%	If Functor/Arity is a constant of a pred type,
	%	instantiates the output parameters, otherwise fails.
	%	For example, functor `map__search/1' has type `pred(K,V)'
	%	(hence PredTypeParams = [K,V]) and argument types [map(K,V)].

:- pred builtin_pred_type(type_info, const, int, list(cons_type_info)).
:- mode builtin_pred_type(typeinfo_ui, in, in, out)
	is semidet.

builtin_pred_type(TypeInfo, Functor, Arity, PredConsInfoList) :-
	Functor = term_atom(Name),
	typeinfo_get_preds(TypeInfo, PredTable),
	typeinfo_get_pred_name_index(TypeInfo, PredNameIndex),
	( map__search(PredNameIndex, Name, PredIdList) ->
		make_pred_cons_info_list(PredIdList, PredTable, Arity, [],
			PredConsInfoList)
	;
		PredConsInfoList = []
	).

:- pred make_pred_cons_info_list(list(pred_id), pred_table, int,
				list(cons_type_info), list(cons_type_info)).
:- mode make_pred_cons_info_list(in, in, in, in, out).

make_pred_cons_info_list([], _PredTable, _Arity, L, L).
make_pred_cons_info_list([PredId|PredIds], PredTable, Arity, L0, L) :-
	make_pred_cons_info(PredId, PredTable, Arity, L0, L1),
	make_pred_cons_info_list(PredIds, PredTable, Arity, L1, L).

:- type cons_type_info ---> cons_type_info(tvarset, type, list(type)).

:- pred make_pred_cons_info(pred_id, pred_table, int,
				list(cons_type_info), list(cons_type_info)).
:- mode make_pred_cons_info(in, in, in, in, out).

make_pred_cons_info(PredId, PredTable, FuncArity, L0, L) :-
	map__lookup(PredTable, PredId, PredInfo),
	predicate_arity(PredId, PredArity),
	(
		PredArity >= FuncArity
	->
		predinfo_arg_types(PredInfo, PredTypeVarSet, CompleteArgTypes),
		split_list(FuncArity, CompleteArgTypes,
			ArgTypes, PredTypeParams),
		term__context_init("<builtin>", 0, Context),
		PredType = term_functor(term_atom("pred"), PredTypeParams,
				Context),
		ConsInfo = cons_type_info(PredTypeVarSet, PredType, ArgTypes),
		L = [ConsInfo | L0]
	;
		L = L0
	).

	% is_builtin_atomic_type(TypeId)
	%	is true iff 'TypeId' is the type_id of a builtin atomic type

:- pred is_builtin_atomic_type(type_id).
:- mode is_builtin_atomic_type(in).

is_builtin_atomic_type(QualifiedName - 0) :-
	unqualify_name(QualifiedName, Name),
	is_builtin_atomic_type_2(Name).

:- pred is_builtin_atomic_type_2(string).
:- mode is_builtin_atomic_type_2(in).

:- is_builtin_atomic_type_2([]) when ever.
:- is_builtin_atomic_type_2([X|_]) when X.

is_builtin_atomic_type_2("int").
is_builtin_atomic_type_2("float").
is_builtin_atomic_type_2("string").
is_builtin_atomic_type_2("character").

	% is_builtin_pred_type(TypeId)
	%	is true iff 'TypeId' is the type_id of a builtin higher-order
	%	predicate type.

:- pred is_builtin_pred_type(type_id).
:- mode is_builtin_pred_type(in).

is_builtin_pred_type(QualifiedName - _Arity) :-
	unqualify_name(QualifiedName, Name),
	Name = "pred".

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The typeinfo data structure and access predicates.

:- type tvarset		==	varset.

:- type tsubst		==	map(var, type).

:- type type_info 
	--->	typeinfo(
				% The io state
			io__state, 

				% The global symbol tables
			module_info, 

				% The pred_id of the pred
				% being called (if any)
			pred_id,

				% The argument number within
				% a pred call 
			int,		

				% The pred we're checking
			pred_id,

				% The context of the goal
				% we're checking
			term__context,

				% The original source of the unification
				% we're checking
			unify_context,	

				% Variable names
			varset,	
				% This is the main piece of
				% information that we are
				% computing and which gets
				% updated as we go along
			type_assign_set,

				% did we find any type errors?
			bool,

				% Head type params
			headtypes
		).

	% The normal inst of a type_info struct: ground, with
	% the io_state and the struct itself unique, but with
	% multiple references allowed for the other parts.

:- inst uniq_type_info	=	bound_unique(
					typeinfo(
						ground_unique, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground
					)
				).

:- mode typeinfo_uo :: free -> uniq_type_info.
:- mode typeinfo_ui :: uniq_type_info -> uniq_type_info.
:- mode typeinfo_di :: uniq_type_info -> dead.

	% Some fiddly modes used when we want to extract
	% the io_state from a typeinfo struct and then put it back again.

:- inst typeinfo_no_io	=	bound_unique(
					typeinfo(
						dead, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground
					)
				).

:- mode typeinfo_get_io_state 	:: uniq_type_info -> typeinfo_no_io.
:- mode typeinfo_no_io 		:: typeinfo_no_io -> typeinfo_no_io.
:- mode typeinfo_set_io_state 	:: typeinfo_no_io -> dead.

%-----------------------------------------------------------------------------%

:- pred typeinfo_init(io__state, module_info, pred_id, varset,
			varset, map(var, type), headtypes, type_info).
:- mode typeinfo_init(di, in, in, in, in, in, in, typeinfo_uo).

typeinfo_init(IOState, ModuleInfo, PredId, TypeVarSet, VarSet,
		VarTypes, HeadTypeParams, TypeInfo) :-
	term__context_init(0, Context),
	map__init(TypeBindings),
	TypeInfo = typeinfo(
		IOState, ModuleInfo, PredId, 0, PredId, Context,
		unify_context(explicit, []),
		VarSet, [type_assign(VarTypes, TypeVarSet, TypeBindings)],
		no, HeadTypeParams
	).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_io_state(type_info, io__state).
:- mode typeinfo_get_io_state(typeinfo_get_io_state, uo).

typeinfo_get_io_state(typeinfo(IOState,_,_,_,_,_,_,_,_,_,_), IOState).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_io_state(type_info, io__state, type_info).
:- mode typeinfo_set_io_state(typeinfo_set_io_state, ui, typeinfo_uo).

typeinfo_set_io_state( typeinfo(_,B,C,D,E,F,G,H,I,J,K), IOState,
			typeinfo(IOState,B,C,D,E,F,G,H,I,J,K)).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_preds(type_info, pred_table).
:- mode typeinfo_get_preds(in, out).

typeinfo_get_preds(typeinfo(_,ModuleInfo,_,_,_,_,_,_,_,_,_), Preds) :-
	moduleinfo_preds(ModuleInfo, Preds).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_pred_name_index(type_info, map(string, list(pred_id))).
:- mode typeinfo_get_pred_name_index(in, out).

typeinfo_get_pred_name_index(typeinfo(_,ModuleInfo,_,_,_,_,_,_,_,_,_), 
		PredNameIndex) :-
	moduleinfo_pred_name_index(ModuleInfo, PredNameIndex).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_types(type_info, type_table).
:- mode typeinfo_get_types(in, out).

typeinfo_get_types(typeinfo(_,ModuleInfo,_,_,_,_,_,_,_,_,_), Types) :-
	moduleinfo_types(ModuleInfo, Types).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_ctors(type_info, cons_table).
:- mode typeinfo_get_ctors(in, out).

typeinfo_get_ctors(typeinfo(_,ModuleInfo,_,_,_,_,_,_,_,_,_), Ctors) :-
	moduleinfo_ctors(ModuleInfo, Ctors).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_called_predid(type_info, pred_id).
:- mode typeinfo_get_called_predid(in, out).

typeinfo_get_called_predid(typeinfo(_,_,PredId,_,_,_,_,_,_,_,_), PredId).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_called_predid(type_info, pred_id, type_info).
:- mode typeinfo_set_called_predid(typeinfo_di, in, typeinfo_uo).

typeinfo_set_called_predid(typeinfo(A,B,_     ,D,E,F,G,H,I,J,K), PredId,
			   typeinfo(A,B,PredId,D,E,F,G,H,I,J,K)).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_arg_num(type_info, int).
:- mode typeinfo_get_arg_num(in, out).

typeinfo_get_arg_num(typeinfo(_,_,_,ArgNum,_,_,_,_,_,_,_), ArgNum).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_arg_num(int, type_info, type_info).
:- mode typeinfo_set_arg_num(in, typeinfo_di, typeinfo_uo).

typeinfo_set_arg_num(ArgNum, typeinfo(A,B,C,_,     E,F,G,H,I,J,K),
			     typeinfo(A,B,C,ArgNum,E,F,G,H,I,J,K)).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_predid(type_info, pred_id).
:- mode typeinfo_get_predid(in, out).

typeinfo_get_predid(typeinfo(_,_,_,_,PredId,_,_,_,_,_,_), PredId).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_context(type_info, term__context).
:- mode typeinfo_get_context(in, out).

typeinfo_get_context(typeinfo(_,_,_,_,_,Context,_,_,_,_,_), Context).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_context(term__context, type_info, type_info).
:- mode typeinfo_set_context(in, typeinfo_di, typeinfo_uo).

typeinfo_set_context(Context, typeinfo(A,B,C,D,E,_,G,H,I,J,K),
			typeinfo(A,B,C,D,E,Context,G,H,I,J,K)).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_unify_context(type_info, unify_context).
:- mode typeinfo_get_unify_context(in, out).

typeinfo_get_unify_context(typeinfo(_,_,_,_,_,_,UnifyContext,_,_,_,_),
				UnifyContext).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_unify_context(unify_context, type_info, type_info).
:- mode typeinfo_set_unify_context(in, typeinfo_di, typeinfo_uo).

typeinfo_set_unify_context(UnifyContext, typeinfo(A,B,C,D,E,F,_,H,I,J,K),
			typeinfo(A,B,C,D,E,F,UnifyContext,H,I,J,K)).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_varset(type_info, varset).
:- mode typeinfo_get_varset(in, out).

typeinfo_get_varset(typeinfo(_,_,_,_,_,_,_,VarSet,_,_,_), VarSet).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_type_assign_set(type_info, type_assign_set).
:- mode typeinfo_get_type_assign_set(in, out).

typeinfo_get_type_assign_set(typeinfo(_,_,_,_,_,_,_,_,TypeAssignSet,_,_),
			TypeAssignSet).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_vartypes(type_info, map(var, type)).
:- mode typeinfo_get_vartypes(in, out).

typeinfo_get_vartypes(TypeInfo, VarTypes) :-
	typeinfo_get_type_assign_set(TypeInfo, TypeAssignSet),
	( TypeAssignSet = [TypeAssign | _] ->
		type_assign_get_var_types(TypeAssign, VarTypes)
	;
		error("internal error in typeinfo_get_vartypes")
	).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_type_assign_set(type_info, type_assign_set, type_info).
:- mode typeinfo_set_type_assign_set(typeinfo_di, in, typeinfo_uo).

typeinfo_set_type_assign_set( typeinfo(A,B,C,D,E,F,G,H,_,J,K), TypeAssignSet,
			typeinfo(A,B,C,D,E,F,G,H,TypeAssignSet,J,K)).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_found_error(type_info, bool).
:- mode typeinfo_get_found_error(typeinfo_ui, out).

typeinfo_get_found_error(typeinfo(_,_,_,_,_,_,_,_,_,FoundError,_), FoundError).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_found_error(type_info, bool, type_info).
:- mode typeinfo_set_found_error(typeinfo_di, in, typeinfo_uo).

typeinfo_set_found_error( typeinfo(A,B,C,D,E,F,G,H,I,_,K), FoundError,
			typeinfo(A,B,C,D,E,F,G,H,I,FoundError,K)).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_head_type_params(type_info, headtypes).
:- mode typeinfo_get_head_type_params(typeinfo_ui, out).

typeinfo_get_head_type_params( typeinfo(_,_,_,_,_,_,_,_,_,_,HeadTypeParams),
				HeadTypeParams).

%-----------------------------------------------------------------------------%

:- pred typeinfo_set_head_type_params(type_info, headtypes, type_info).
:- mode typeinfo_set_head_type_params(typeinfo_di, in, typeinfo_uo).

typeinfo_set_head_type_params( typeinfo(A,B,C,D,E,F,G,H,I,J,_), HeadTypeParams,
			typeinfo(A,B,C,D,E,F,G,H,I,J,HeadTypeParams)).

%-----------------------------------------------------------------------------%

:- pred typeinfo_get_ctor_list(type_info, const, int, list(cons_type_info)).
:- mode typeinfo_get_ctor_list(in, in, in, out).

typeinfo_get_ctor_list(TypeInfo, Functor, Arity, ConsInfoList) :-
	% Check if `Functor/Arity' has been defined as a constructor
	% in some discriminated union type(s).  This gives
	% us a list of possible cons_type_infos.
	typeinfo_get_ctors(TypeInfo, Ctors),
	(
		Functor = term_atom(Name),
		map__search(Ctors, cons(Name, Arity), HLDS_ConsDefnList)
	->
		convert_cons_defn_list(TypeInfo, HLDS_ConsDefnList,
			ConsInfoList0)
	;
		ConsInfoList0 = []
	),
	% Check if Functor is a constant of one of the builtin atomic
	% types (string, float, int, character).  If so, insert
	% the resulting cons_type_info at the start of the list.
	(
		Arity = 0,
		builtin_atomic_type(Functor, BuiltInTypeName)
	->
		term__context_init("<builtin>", 0, Context),
		ConsType = term_functor(term_atom(BuiltInTypeName), [],
				Context),
		varset__init(ConsTypeVarSet),
		ConsInfo = cons_type_info(ConsTypeVarSet, ConsType, []),
		ConsInfoList1 = [ConsInfo | ConsInfoList0]
	;
		ConsInfoList1 = ConsInfoList0
	),
	% Check if Functor is the name of a predicate which takes at least
	% Arity arguments.  If so, insert the resulting cons_type_info
	% at the start of the list.
	(
		builtin_pred_type(TypeInfo, Functor, Arity, PredConsInfoList)
	->
		append(ConsInfoList1, PredConsInfoList, ConsInfoList)
	;
		ConsInfoList = ConsInfoList1
	).

:- pred convert_cons_defn_list(type_info, list(hlds__cons_defn),
				list(cons_type_info)).
:- mode convert_cons_defn_list(typeinfo_ui, in, out).

:- convert_cons_defn_list(_, L, _) when L.	% NU-Prolog indexing.

convert_cons_defn_list(_TypeInfo, [], []).
convert_cons_defn_list(TypeInfo, [X|Xs], [Y|Ys]) :-
	convert_cons_defn(TypeInfo, X, Y),
	convert_cons_defn_list(TypeInfo, Xs, Ys).

:- pred convert_cons_defn(type_info, hlds__cons_defn, cons_type_info).
:- mode convert_cons_defn(typeinfo_ui, in, out).

convert_cons_defn(TypeInfo, HLDS_ConsDefn, ConsTypeInfo) :-
	HLDS_ConsDefn = hlds__cons_defn(ArgTypes, TypeId, Context),
	typeinfo_get_types(TypeInfo, Types),
	map__lookup(Types, TypeId, TypeDefn),
	TypeDefn = hlds__type_defn(ConsTypeVarSet, ConsTypeParams, _, _, _),
	TypeId = QualifiedName - _Arity,
	unqualify_name(QualifiedName, Name),
	ConsType = term_functor(term_atom(Name), ConsTypeParams, Context),
	ConsTypeInfo = cons_type_info(ConsTypeVarSet, ConsType, ArgTypes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The type_assign and type_assign_set data structures.

:- type type_assign_set	==	list(type_assign).

:- type type_assign	--->	type_assign(
					map(var, type),		% var types
					tvarset,		% type names
					tsubst			% type bindings
				).

%-----------------------------------------------------------------------------%

	% Access predicates for the type_assign data structure.
	% Excruciatingly boring code.

:- pred type_assign_get_var_types(type_assign, map(var, type)).
:- mode type_assign_get_var_types(in, out).

type_assign_get_var_types(type_assign(VarTypes, _, _), VarTypes).

%-----------------------------------------------------------------------------%

:- pred type_assign_get_typevarset(type_assign, tvarset).
:- mode type_assign_get_typevarset(in, out).

type_assign_get_typevarset(type_assign(_, TypeVarSet, _), TypeVarSet).

%-----------------------------------------------------------------------------%

:- pred type_assign_get_type_bindings(type_assign, tsubst).
:- mode type_assign_get_type_bindings(in, out).

type_assign_get_type_bindings(type_assign(_, _, TypeBindings), TypeBindings).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_var_types(type_assign, map(var, type), type_assign).
:- mode type_assign_set_var_types(in, in, out).

type_assign_set_var_types(type_assign(_, B, C), VarTypes,
			type_assign(VarTypes, B, C)).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_typevarset(type_assign, tvarset, type_assign).
:- mode type_assign_set_typevarset(in, in, out).

type_assign_set_typevarset(type_assign(A, _, C), TypeVarSet,
			type_assign(A, TypeVarSet, C)).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_type_bindings(type_assign, tsubst, type_assign).
:- mode type_assign_set_type_bindings(in, in, out).

type_assign_set_type_bindings(type_assign(A, B, _), TypeBindings,
			type_assign(A, B, TypeBindings)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The next section contains predicates for writing error diagnostics.

%-----------------------------------------------------------------------------%

/************** OLD JUNK
:- pred report_error_unif(type_info, term, term,
			type_assign_set, io__state, io__state).
:- mode report_error_unif(typeinfo_no_io, in, in, in, di, uo).

report_error_unif(TypeInfo, X, Y, TypeAssignSet) -->
	{ typeinfo_get_varset(TypeInfo, VarSet) },
	write_typeinfo_context(TypeInfo),
	io__write_string("  type error in unification of term `"),
	io__write_term(VarSet, X),
	io__write_string("' and term `"),
	io__write_term(VarSet, Y),
	io__write_string("'.\n"),
	write_type_assign_set_msg(TypeAssignSet, VarSet).
**********************/

:- pred report_error_unif_var_var(type_info, var, var, type_assign_set,
					io__state, io__state).
:- mode report_error_unif_var_var(typeinfo_no_io, in, in, in, di, uo).

report_error_unif_var_var(TypeInfo, X, Y, TypeAssignSet) -->

	{ typeinfo_get_context(TypeInfo, Context) },
	{ typeinfo_get_varset(TypeInfo, VarSet) },
	{ typeinfo_get_unify_context(TypeInfo, UnifyContext) },

	write_context_and_pred_id(TypeInfo),
	write_unify_context(UnifyContext, Context),

	io__write_string("  type error in unification of variable `"),
	io__write_variable(X, VarSet),
	io__write_string("' and variable `"),
	io__write_variable(Y, VarSet),
	io__write_string("'.\n"),

	io__write_string("\t`"),
	io__write_variable(X, VarSet),
	io__write_string("' "),
	write_type_of_var(TypeInfo, TypeAssignSet, X),
	io__write_string(",\n"),

	io__write_string("\t`"),
	io__write_variable(Y, VarSet),
	io__write_string("' "),
	write_type_of_var(TypeInfo, TypeAssignSet, Y),
	io__write_string(".\n"),

	write_type_assign_set_msg(TypeAssignSet, VarSet).

:- pred report_error_unif_var_functor(type_info, var, const, list(term),
					type_assign_set,
					io__state, io__state).
:- mode report_error_unif_var_functor(typeinfo_no_io, in, in, in, in, di, uo).

report_error_unif_var_functor(TypeInfo, Var, Functor, Args, TypeAssignSet) -->

	{ typeinfo_get_context(TypeInfo, Context) },
	{ typeinfo_get_varset(TypeInfo, VarSet) },
	{ typeinfo_get_unify_context(TypeInfo, UnifyContext) },

	write_context_and_pred_id(TypeInfo),
	write_unify_context(UnifyContext, Context),

	io__write_string("  type error in unification of "),
	write_argument_name(VarSet, Var),
	io__write_string(" and term `"),
	io__write_term(VarSet, term_functor(Functor, Args, Context)),
	io__write_string("'.\n"),

	io__write_string("\t("),
	write_argument_name(VarSet, Var),
	io__write_string(" "),
	write_type_of_var(TypeInfo, TypeAssignSet, Var),
	io__write_string(".)\n"),

	write_type_assign_set_msg(TypeAssignSet, VarSet).

:- pred write_argument_name(varset, var, io__state, io__state).
:- mode write_argument_name(in, in, di, uo).

write_argument_name(VarSet, VarId) -->
	( { varset__lookup_name(VarSet, VarId, _) } ->
		io__write_string("variable `"),
		io__write_variable(VarId, VarSet),
		io__write_string("'")
	;
		io__write_string("argument")
	).

:- pred write_unify_context(unify_context, term__context, io__state, io__state).
:- mode write_unify_context(in, in, di, uo).

write_unify_context(unify_context(MainContext, RevSubContexts), Context) -->
	prog_out__write_context(Context),
	write_unify_main_context(MainContext),
	{ reverse(RevSubContexts, SubContexts) },
	write_unify_sub_contexts(SubContexts, Context).

:- pred write_unify_main_context(unify_main_context, io__state, io__state).
:- mode write_unify_main_context(in, di, uo).

write_unify_main_context(explicit) -->
	[].
write_unify_main_context(head(ArgNum)) -->
	io__write_string("  in argument "),
	io__write_int(ArgNum),
	io__write_string(" of clause head:\n").
write_unify_main_context(call(PredId, ArgNum)) -->
	io__write_string("  in argument "),
	io__write_int(ArgNum),
	io__write_string(" of call to predicate `"),
	hlds_out__write_pred_id(PredId),
	io__write_string("':\n").

:- pred write_unify_sub_contexts(unify_sub_contexts, term__context,
				io__state, io__state).
:- mode write_unify_sub_contexts(in, in, di, uo).

write_unify_sub_contexts([], _) -->
	[].
write_unify_sub_contexts([ConsId - ArgNum | SubContexts], Context) -->
	prog_out__write_context(Context),
	io__write_string("  in argument "),
	io__write_int(ArgNum),
	io__write_string(" of functor `"),
	hlds_out__write_cons_id(ConsId),
	io__write_string("':\n"),
	write_unify_sub_contexts(SubContexts, Context).

:- pred write_type_of_var(type_info, type_assign_set, var,
				io__state, io__state).
:- mode write_type_of_var(typeinfo_no_io, in, in, di, uo).

write_type_of_var(_TypeInfo, TypeAssignSet, Var) -->
	{ get_type_stuff(TypeAssignSet, Var, TypeStuffList) },
	( { TypeStuffList = [SingleTypeStuff] } ->
		{ SingleTypeStuff = type_stuff(VType, TVarSet, TBinding) },
		io__write_string(" has type `"),
		write_type_b(VType, TVarSet, TBinding),
		io__write_string("'")
	;
		io__write_string(" has overloaded type { `"),
		write_type_stuff_list(TypeStuffList),
		io__write_string(" }")
	).

%-----------------------------------------------------------------------------%

:- pred write_type_assign_set_msg(type_assign_set, tvarset,
				io__state, io__state).
:- mode write_type_assign_set_msg(in, in, di, uo).

write_type_assign_set_msg(TypeAssignSet, VarSet) -->
	lookup_option(verbose_errors, bool(VerboseErrors)),
	( { VerboseErrors = yes } ->
		( { TypeAssignSet = [_] } ->
		    io__write_string("\tThe partial type assignment was:\n")
		;
		    io__write_string(
			"\tThe possible partial type assignments were:\n")
		),
		write_type_assign_set(TypeAssignSet, VarSet)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred write_type_assign_set(type_assign_set, tvarset, io__state, io__state).
:- mode write_type_assign_set(in, in, di, uo).

write_type_assign_set([], _) --> [].
write_type_assign_set([TypeAssign | TypeAssigns], VarSet) -->
	io__write_string("\t"),
	write_type_assign(TypeAssign, VarSet),
	io__write_string("\n"),
	write_type_assign_set(TypeAssigns, VarSet).

:- pred write_type_assign(type_assign, tvarset, io__state, io__state).
:- mode write_type_assign(in, in, di, uo).

write_type_assign(TypeAssign, VarSet) -->
	{
	  type_assign_get_var_types(TypeAssign, VarTypes),
	  type_assign_get_type_bindings(TypeAssign, TypeBindings),
	  type_assign_get_typevarset(TypeAssign, TypeVarSet),
	  map__keys(VarTypes, Vars)
	},
	write_type_assign_2(Vars, VarSet, VarTypes, TypeBindings, TypeVarSet,
			no),
	io__write_string("\n").

:- pred write_type_assign_2(list(var), varset, map(var, type),
			tsubst, tvarset, bool, io__state, io__state).
:- mode write_type_assign_2(in, in, in, in, in, in, di, uo).

write_type_assign_2([], _, _, _, _, FoundOne) -->
	( { FoundOne = no } ->
		io__write_string("(No variables were assigned a type)")
	;
		[]
	).

write_type_assign_2([Var | Vars], VarSet, VarTypes, TypeBindings, TypeVarSet,
			FoundOne) -->
	( 
		{ varset__lookup_name(VarSet, Var, _) },
		{ map__search(VarTypes, Var, Type) }
	->
		( { FoundOne = yes } ->
			io__write_string("\n\t")
		;
			[]
		),
		io__write_variable(Var, VarSet),
		io__write_string(" :: "),
		write_type_b(Type, TypeVarSet, TypeBindings),
		write_type_assign_2(Vars, VarSet, VarTypes, TypeBindings,
			TypeVarSet, yes)
	;
		write_type_assign_2(Vars, VarSet, VarTypes, TypeBindings,
			TypeVarSet, FoundOne)
	).

	% write_type_b writes out a type after applying the type bindings.

:- pred write_type_b(type, tvarset, tsubst, io__state, io__state).
:- mode write_type_b(in, in, in, di, uo).

write_type_b(Type, TypeVarSet, TypeBindings) -->
	{ term__apply_rec_substitution(Type, TypeBindings, Type2) },
	io__write_term(TypeVarSet, Type2).

%-----------------------------------------------------------------------------%

:- pred report_error_var(type_info, var, type, type_assign_set,
			io__state, io__state).
:- mode report_error_var(typeinfo_no_io, in, in, in, di, uo).

report_error_var(TypeInfo, VarId, Type, TypeAssignSet0) -->
	{ typeinfo_get_called_predid(TypeInfo, CalledPredId) },
	{ typeinfo_get_arg_num(TypeInfo, ArgNum) },
	{ typeinfo_get_context(TypeInfo, Context) },
	{ typeinfo_get_unify_context(TypeInfo, UnifyContext) },
	{ get_type_stuff(TypeAssignSet0, VarId, TypeStuffList) },
	{ typeinfo_get_varset(TypeInfo, VarSet) },
	write_context_and_pred_id(TypeInfo),
	write_call_context(Context, CalledPredId, ArgNum, UnifyContext),
	prog_out__write_context(Context),
	io__write_string("  type error: "),
	write_argument_name(VarSet, VarId),
	( { TypeStuffList = [SingleTypeStuff] } ->
		{ SingleTypeStuff = type_stuff(VType, TVarSet, TBinding) },
		io__write_string(" has type `"),
		write_type_b(VType, TVarSet, TBinding),
		io__write_string("',\n"),
		prog_out__write_context(Context),
		io__write_string("  expected type was `"),
		write_type_b(Type, TVarSet, TBinding),
		io__write_string("'.\n")
	;
		io__write_string(" has overloaded type { `"),
		write_type_stuff_list(TypeStuffList),
		io__write_string(" },\n"),
		prog_out__write_context(Context),
		lookup_option(verbose_errors, bool(VerboseErrors)),
		io__write_string("  which doesn't match the expected type.\n"),
		( { VerboseErrors = yes } ->
				% XXX improve error message: should output
				% the expected type.
			io__write_string("\t(I ought to tell you what the expected type was,\n"),
			io__write_string("\tbut it might be different for each overloading,\n"),
			io__write_string("\tso I won't try.  My apologies.)\n")
		;
			[]
		)
	),
	write_type_assign_set_msg(TypeAssignSet0, VarSet).

:- pred write_type_stuff_list(list(type_stuff), io__state, io__state).
:- mode write_type_stuff_list(in, di, uo).

write_type_stuff_list([]) --> [].
write_type_stuff_list([type_stuff(T, TVarSet, TBinding) | Ts]) -->
	write_type_b(T, TVarSet, TBinding),
	write_type_stuff_list_2(Ts).

:- pred write_type_stuff_list_2(list(type_stuff), io__state, io__state).
:- mode write_type_stuff_list_2(in, di, uo).

write_type_stuff_list_2([]) --> [].
write_type_stuff_list_2([type_stuff(T, TVarSet, TBinding) | Ts]) -->
	io__write_string(", "),
	write_type_b(T, TVarSet, TBinding),
	write_type_stuff_list_2(Ts).

%-----------------------------------------------------------------------------%

:- pred report_error_undef_pred(type_info,  pred_id, io__state, io__state).
:- mode report_error_undef_pred(typeinfo_no_io, in, di, uo).

report_error_undef_pred(TypeInfo, PredId) -->
	write_typeinfo_context(TypeInfo),
	io__write_string("  error: undefined predicate `"),
	hlds_out__write_pred_id(PredId),
	io__write_string("'.\n").

:- pred report_error_pred_num_args(type_info, pred_id, io__state, io__state).
:- mode report_error_pred_num_args(typeinfo_no_io, in, di, uo).

report_error_pred_num_args(TypeInfo, PredId) -->
	write_typeinfo_context(TypeInfo),
	io__write_string("  error: wrong number of arguments in call to pred `"),
	{ predicate_name(PredId, PredName) },
	io__write_string(PredName),
	io__write_string("'.\n").

:- pred report_error_undef_cons(type_info, const, int, io__state, io__state).
:- mode report_error_undef_cons(typeinfo_no_io, in, in, di, uo).

report_error_undef_cons(TypeInfo, Functor, Arity) -->
	{ typeinfo_get_called_predid(TypeInfo, CalledPredId) },
	{ typeinfo_get_arg_num(TypeInfo, ArgNum) },
	{ typeinfo_get_context(TypeInfo, Context) },
	{ typeinfo_get_unify_context(TypeInfo, UnifyContext) },
	write_context_and_pred_id(TypeInfo),
	write_call_context(Context, CalledPredId, ArgNum, UnifyContext),
	prog_out__write_context(Context),
	io__write_string("  error: undefined symbol `"),
	io__write_constant(Functor),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("'.\n").

:- pred write_call_context(term__context, pred_id, int, unify_context,
				io__state, io__state).
:- mode write_call_context(in, in, in, in, di, uo).

write_call_context(Context, PredId, ArgNum, UnifyContext) -->
	( { ArgNum = 0 } ->
		write_unify_context(UnifyContext, Context)
	;
		prog_out__write_context(Context),
		io__write_string("  in argument "),
		io__write_int(ArgNum),
		io__write_string(" of call to pred `"),
		hlds_out__write_pred_id(PredId),
		io__write_string("':\n")
	).

:- pred report_error_cons(type_info, const, list(term),
			type, type_assign_set, io__state, io__state).
:- mode report_error_cons(typeinfo_no_io, in, in, in, in, di, uo).

report_error_cons(TypeInfo, Functor, Args, Type, TypeAssignSet) -->
	{ typeinfo_get_called_predid(TypeInfo, CalledPredId) },
	{ typeinfo_get_arg_num(TypeInfo, ArgNum) },
	{ typeinfo_get_varset(TypeInfo, VarSet) },
	{ typeinfo_get_context(TypeInfo, Context) },
	{ typeinfo_get_unify_context(TypeInfo, UnifyContext) },
	write_context_and_pred_id(TypeInfo),
	write_call_context(Context, CalledPredId, ArgNum, UnifyContext),
	prog_out__write_context(Context),
	io__write_string("  type error: term `"),
	io__write_term(VarSet, term_functor(Functor, Args, Context)),
	io__write_string("' does not have type `"),
	write_type(Type),	% XXX type parameter names
	io__write_string("'.\n"),
	write_type_assign_set_msg(TypeAssignSet, VarSet).

%-----------------------------------------------------------------------------%

:- pred write_typeinfo_context(type_info, io__state, io__state).
:- mode write_typeinfo_context(typeinfo_no_io, di, uo).

write_typeinfo_context(TypeInfo) -->
	write_context_and_pred_id(TypeInfo),
	{ typeinfo_get_context(TypeInfo, Context) },
	prog_out__write_context(Context).

:- pred write_context_and_pred_id(type_info, io__state, io__state).
:- mode write_context_and_pred_id(typeinfo_no_io, di, uo).

write_context_and_pred_id(TypeInfo) -->
	{ typeinfo_get_context(TypeInfo, Context) },
	{ typeinfo_get_predid(TypeInfo, PredId) },
	prog_out__write_context(Context),
	io__write_string("In clause for predicate `"),
	hlds_out__write_pred_id(PredId),
	io__write_string("':\n").

%-----------------------------------------------------------------------------%

:- pred report_ambiguity_error(type_info, type_assign, type_assign,
				io__state, io__state).
:- mode report_ambiguity_error(in, in, in, di, uo).

report_ambiguity_error(TypeInfo, TypeAssign1, TypeAssign2) -->
	write_typeinfo_context(TypeInfo),
	io__write_string("  error: ambiguous overloading causes type ambiguity.\n"),
	io__write_string("\tPossible type assignments include:\n"),
	{ typeinfo_get_varset(TypeInfo, VarSet) },
	{ type_assign_get_var_types(TypeAssign1, VarTypes1) },
	{ map__keys(VarTypes1, Vars1) },
	report_ambiguity_error_2(Vars1, VarSet, TypeAssign1, TypeAssign2).

:- pred report_ambiguity_error_2(list(var), varset, type_assign, type_assign,
				io__state, io__state).
:- mode report_ambiguity_error_2(in, in, in, in, di, uo).

report_ambiguity_error_2([], _VarSet, _TypeAssign1, _TypeAssign2) --> [].
report_ambiguity_error_2([V | Vs], VarSet, TypeAssign1, TypeAssign2) -->
	{ type_assign_get_var_types(TypeAssign1, VarTypes1) },
	{ type_assign_get_var_types(TypeAssign2, VarTypes2) },
	( {
		map__search(VarTypes1, V, T1),
		map__search(VarTypes2, V, T2),
		not (T1 = T2)
	} ->
		io__write_string("\t"),
		io__write_variable(V, VarSet),
		io__write_string(" :: "),
		{ type_assign_get_typevarset(TypeAssign1, TVarSet1) },
		{ type_assign_get_type_bindings(TypeAssign1, TypeBindings1) },
		write_type_b(T1, TVarSet1, TypeBindings1),
		io__write_string(" or "),
		{ type_assign_get_typevarset(TypeAssign2, TVarSet2) },
		{ type_assign_get_type_bindings(TypeAssign2, TypeBindings2) },
		write_type_b(T2, TVarSet2, TypeBindings2),
		io__write_string("\n")
	;
		[]
	),
	report_ambiguity_error_2(Vs, VarSet, TypeAssign1, TypeAssign2).

:- pred write_type(type, io__state, io__state).
:- mode write_type(in, di, uo).

write_type(Type) -->
	{ varset__init(TVarSet) },	% XXX type parameter names
	io__write_term(TVarSet, Type).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
