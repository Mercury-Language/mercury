/******************************************************************************

% Main author: fjh.

This file contains a type-checker which I started writing a fair while
ago.  It still needs quite a bit of work to integrate it with
the rest of the stuff in this directory and to get it to actually work.

******************************************************************************/

:- import_module io, bag.

%-----------------------------------------------------------------------------%
%
% There are three sorts of types:
%
% 1) discriminated unions:
%	:- type tree(T) ---> nil ; t(tree(T), T, tree(T)).
%
% 2) undiscriminated unions (if rhs has a single type then the two
%	types have same structure but *different* name):
%	Basically just syntactic sugar for discriminated unions,
%	except that it also works for builtin types like int.
%
%	:- type t1 ---> a ; b.
%	:- type t2 ---> c ; d.
%
%		% same as type t3 ---> a ; b ; c ; d.
%	:- type t3 = t1 + t2.
%
%	:- type number = int + float.
%
% 3) equivalent types (treated identically, ie, same name.  Any number
%	of types can be equivalent; the *canonical* one is the one
%	which is not defined using ==):
%	:- type real == float.
%
% 4) pred declarations:
%	:- pred app(list(T), list(T), list(T)).
%
% builtin types: (these have special syntax for constants)
%	char, int, float, string
%	pred, pred(T), pred(T1, T2), pred(T1, T2, T3), ...
% system types:
%	module <system>: array(T), list(T).
%	module <io>: io__state.
%	module <lowlevel>: byte, word, ...
% generally useful types:
%	list(T) ---> [] | T.list(T).
%	string == list(char).
%	integer(Min,Max) == integer where (X : Min <= X, X <= Max).
%	real == float.
%	func(Dom,Ran) == pred(Dom,Ran)
%		where (Func : all [X] some [Y] Func(X,Y),
%			all [X,Y1,Y2] (Func(X,Y1),Func(X,Y2) => Y1 = Y2)).
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred typecheck(module_info, module_info, io__state, io__state).
:- mode typecheck(input, output, di, uo).

typecheck(Module0, Module) -->
	io__write_string("Checking for undefined types...\n"),
	check_undefined_types(Module0, Module1),
	{Module = Module1}.
	/****** NOT YET COMPLETED
	io__write_string("Checking for circularly defined types...\n"),
	check_circular_types(Module1, Module2),
	io__write_string("Type-checking clauses...\n"),
	check_pred_types(Module2, Module).
	*******/

%-----------------------------------------------------------------------------%
	
	% Type-check the code for all the predicates in a module.

:- pred check_pred_types(module_info, module_info, io__state, io__state).
:- mode check_pred_types(input, output, di, uo).

check_pred_types(Module, Module) -->
	{ moduleinfo_predids(Module, PredIds) },
	check_pred_types_2(PredIds, Module).

%-----------------------------------------------------------------------------%

	% Iterate over the list of pred_ids in a module.

:- pred check_pred_types_2(list(pred_id), module_info, io__state, io__state).
:- mode check_pred_types_2(input, input, di, uo).

check_pred_types_2([], _ModuleInfo) --> [].
check_pred_types_2([PredId | PredIds], ModuleInfo) -->
	{ map__search(Preds, PredId, PredInfo) },
	{ predinfo_argtypes(PredInfo, TypeVarSet, ArgTypes) },
	{ predinfo_clauses(Clauses0) },
	check_clause_list(Clauses0, PredId, TypeVarSet, ArgTypes, ModuleInfo,
				Clauses),
	{ predinfo_set_clauses(Clauses) },
	check_pred_types_2(PredIds, ModuleInfo).

%-----------------------------------------------------------------------------%

	% Iterate over the list of clauses for a predicate.

:- pred check_clause_list(list(clause), pred_id, varset, list(type),
			module_info, list(clause), io__state, io__state).
:- mode check_clause_list(input, input, input, input, input, input, output,
			di, uo).

check_clause_list([], _PredId, _TypeVarSet, _ArgTypes, _ModuleInfo, []) --> [].
check_clause_list([Clause0|Clauses0], PredId, TypeVarSet, ArgTypes, ModuleInfo,
		[Clause|Clauses]) -->
	check_clause(Clause0, PredId, TypeVarSet, ArgTypes, ModuleInfo, Clause),
	check_clause_list(Clauses0, PredId, TypeVarSet, ArgTypes, ModuleInfo,
			Clauses).

%-----------------------------------------------------------------------------%

	% Type-check a single clause.

:- pred check_clause(clause, pred_id, varset, list(type), module_info, clause,
			io__state, io__state).
:- mode check_clause(input, input, input, input, input, output, di, uo).

check_clause(Clause0, PredId, TypeVarSet, ArgTypes, ModuleInfo, Clause,
		IOState0, IOState) :-
	Clause0 = clause(Modes, VarSet, _DummyVarTypes, HeadVars, Body0),
	typeinfo_init(IOState0, ModuleInfo, PredId, TypeVarSet, VarSet,
		TypeInfo0),
	typeinfo_set_list(TypeInfo0, HeadVars, ArgTypes, TypeInfo1),
	check_body(Body0, Body, TypeInfo1, TypeInfo),
	typeinfo_get_var_types(TypeInfo, VarTypes),
	typeinfo_get_io_state(TypeInfo, IOState),
	Clause = clause(Modes, VarSet, VarTypes, HeadVars, Body).

%-----------------------------------------------------------------------------%

typeinfo_init(IOState, ModuleInfo, PredId, TypeVarSet, VarSet, TypeInfo) :-
	moduleinfo_preds(ModuleInfo, Preds),
	moduleinfo_types(ModuleInfo, Types),
	moduleinfo_ctors(ModuleInfo, Ctors),
	map__init(TypeBindings),
	map__init(VarTypes),
	TypeInfo = typecheck_info(IOState, Preds, Types, Ctors, PredId,
		[type_assign(VarSet, VarTypes, TypeVarSet, TypeBindings)]).

typeinfo_get_var_types(



:- type typecheck_info 	--->	typecheck_info(
					io__state,
					pred_table,
					type_table,
					cons_table,
					pred_id,
					type_assign_set
				).

:- type type_assign_set	==	list(type_assignment).

:- type type_assignment	-->	type_assign(
					varset,		% var names
					map(var, type),	% var types
					varset,		% type names
					map(type_param, type)	% type bindings
				).

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
	%
	% XXX - Todo:
	%	* we need a symbol table for constructors
	%	* need to resolve how to deal with overloading/ambiguity
	%	* need to bridge the gap between code above and
	%	* code below.

%-----------------------------------------------------------------------------%

:- pred set_types_list(list(var), list(type), var_types, var_types).
:- mode set_types_list(input, input, input, output).

typeset_set_list([], [], VarTypes, VarTypes).
typeset_set_list([Var|Vars], [Type|Types], VarTypes0, VarTypes) :-
	map__set(VarTypes0, Var, Type, VarTypes1),
	typeset_set_list(Vars, Types, VarTypes1, VarTypes).

%-----------------------------------------------------------------------------%

type_check_clause(clause(VarSet,PredName,Args,Body), GlobalTypes, Types) :-
	types_init(VarSet, GlobalTypes, Types0),
	type_check_clause_2(PredName, Args, Body, Types0, Types).

type_check_clause_2(PredName, Args, Body) -->
	type_check_call_pred(PredName,Args),
	type_check_goal(Body).

type_check_goal(Goal - _GoalInfo) -->
	type_check_goal_2(Goal).

type_check_goal_2(true) --> [].
type_check_goal_2(fail) --> [].
type_check_goal_2((A,B)) -->
	type_check_goal(A),
	type_check_goal(B).
type_check_goal_2((A;B)) -->
	type_check_goal(A),
	type_check_goal(B).
type_check_goal_2(if_then_else(_Vs,A,B,C)) -->
	type_check_goal(A),
	type_check_goal(B),
	type_check_goal(C).
type_check_goal_2(not(_Vs,A)) -->
	type_check_goal(A).
type_check_goal_2(some(_Vs,G)) -->
	type_check_goal(G).
type_check_goal_2(all(_Vs,G)) -->
	type_check_goal(G).
type_check_goal_2(call(PredId, _Mode, Args, _Builtin)) -->
	type_check_call_pred(PredId, Args).
type_check_goal_2(unify(A,B,_Info)) -->
	type_check_unify(A,B).

type_check_call_pred(PredId, Args, TypeCheckInfo0, TypeCheckInfo) :-
	get_pred_types(TypeCheckInfo, PredTypes),



type_check_unify(term_variable(X), term_variable(Y)) --> 
	type_unify(X, Y).
type_check_unify(term_functor(F, As, _), term_variable(Y)) --> 
	{ length(As, Arity) },
	lookup_functor_type(F, Arity, Type),

	

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
	(if { not map__contains(TypeDefns, TypeId) } then
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

%-----------------------------------------------------------------------------%
