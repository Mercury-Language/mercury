%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: type_util.m.
% Main author: fjh.

% This file provides some utility predicates which operate on types.
% It is used by various stages of the compilation after type-checking,
% include the mode checker and the code generator.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module type_util.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_data, prog_data.
:- import_module term.
:- import_module list, map.

%-----------------------------------------------------------------------------%

	% Succeed iff type is an "atomic" type - one which can be
	% unified using a simple_test rather than a complicated_unify.

:- pred type_is_atomic(type, module_info).
:- mode type_is_atomic(in, in) is semidet.

	% type_is_higher_order(Type, PredOrFunc, ArgTypes) succeeds iff
	% Type is a higher-order predicate or function type with the specified
	% argument types (for functions, the return type is appended to the
	% end of the argument types).

:- pred type_is_higher_order(type, pred_or_func, list(type)).
:- mode type_is_higher_order(in, out, out) is semidet.

	% type_id_is_higher_order(TypeId, PredOrFunc) succeeds iff
	% TypeId is a higher-order predicate or function type.

:- pred type_id_is_higher_order(type_id, pred_or_func).
:- mode type_id_is_higher_order(in, out) is semidet.

	% A test for types that are defined by hand (not including
	% the builtin types).  Don't generate base_type_*
	% for these types.

:- pred type_id_is_hand_defined(type_id).
:- mode type_id_is_hand_defined(in) is semidet.

	% Given a type, determine what sort of type it is.

:- pred classify_type(type, module_info, builtin_type).
:- mode classify_type(in, in, out) is det.

:- type builtin_type	--->	int_type
			;	char_type
			;	str_type
			;	float_type
			;	pred_type
			;	enum_type
			;	polymorphic_type
			;	user_type.

	% Given a non-variable type, return its type-id and argument types.

:- pred type_to_type_id(type, type_id, list(type)).
:- mode type_to_type_id(in, out, out) is semidet.

	% Given a variable type, return its type variable.
	
:- pred type_util__var(type, tvar).
:- mode type_util__var(in, out) is semidet.
:- mode type_util__var(out, in) is det.

	% Given a type_id, a list of argument types and maybe a context,
	% construct a type.

:- pred construct_type(type_id, list(type), (type)).
:- mode construct_type(in, in, out) is det.

:- pred construct_type(type_id, list(type), prog_context, (type)).
:- mode construct_type(in, in, in, out) is det.

	% Given a constant and an arity, return a type_id.
	% Fails if the constant is not an atom.

:- pred make_type_id(const, int, type_id).
:- mode make_type_id(in, in, out) is semidet.

	% Given a type_id, look up its module/name/arity

:- pred type_util__type_id_module(module_info, type_id, module_name).
:- mode type_util__type_id_module(in, in, out) is det.

:- pred type_util__type_id_name(module_info, type_id, string).
:- mode type_util__type_id_name(in, in, out) is det.

:- pred type_util__type_id_arity(module_info, type_id, arity).
:- mode type_util__type_id_arity(in, in, out) is det.

	% If the type is a du type, return the list of its constructors.

:- pred type_constructors(type, module_info, list(constructor)).
:- mode type_constructors(in, in, out) is semidet.

	% Work out the types of the arguments of a functor.
:- pred type_util__get_cons_id_arg_types(module_info::in, (type)::in,
		cons_id::in, list(type)::out) is det.

	% Given a list of constructors for a type,
	% check whether that type is a no_tag type
	% (i.e. one with only one constructor, and
	% whose one constructor has only one argument,
	% and which is not private_builtin:type_info/1),
	% and if so, return its constructor symbol and argument type.

:- pred type_is_no_tag_type(list(constructor), sym_name, type).
:- mode type_is_no_tag_type(in, out, out) is semidet.

	% Unify (with occurs check) two types with respect to a type
	% substitution and update the type bindings.
	% The third argument is a list of type variables which cannot
	% be bound (i.e. head type variables).

:- pred type_unify(type, type, list(tvar), tsubst, tsubst).
:- mode type_unify(in, in, in, in, out) is semidet.

:- pred type_unify_list(list(type), list(type), list(tvar), tsubst, tsubst).
:- mode type_unify_list(in, in, in, in, out) is semidet.

	% Return a list of the type variables of a type.

:- pred type_util__vars(type, list(tvar)).
:- mode type_util__vars(in, out) is det.

	% type_list_subsumes(TypesA, TypesB, Subst) succeeds iff the list
	% TypesA subsumes (is more general than) TypesB, producing a
	% type substitution which when applied to TypesA will give TypesB.

:- pred type_list_subsumes(list(type), list(type), tsubst).
:- mode type_list_subsumes(in, in, out) is semidet.

	% apply a type substitution (i.e. map from tvar -> type)
	% to all the types in a variable typing (i.e. map from var -> type).

:- pred apply_substitution_to_type_map(map(prog_var, type), tsubst,
		map(prog_var, type)).
:- mode apply_substitution_to_type_map(in, in, out) is det.

        % same thing as above, except for a recursive substitution
        % (i.e. we keep applying the substitution recursively until
        % there are no more changes).

:- pred apply_rec_substitution_to_type_map(map(prog_var, type), tsubst,
						 map(prog_var, type)).
:- mode apply_rec_substitution_to_type_map(in, in, out) is det.

	% Update a map from tvar to type_info_locn, using the type renaming
	% and substitution to rename tvars and a variable substitution to
	% rename vars. The type renaming is applied before the type
	% substitution.
	%
	% If tvar maps to a another type variable, we keep the new
	% variable, if it maps to a type, we remove it from the map.

:- pred apply_substitutions_to_var_map(map(tvar, type_info_locn), tsubst,
	map(tvar, type), map(prog_var, prog_var), map(tvar, type_info_locn)).
:- mode apply_substitutions_to_var_map(in, in, in, in, out) is det.

	% Update a map from class_constraint to var, using the type renaming
	% and substitution to rename tvars and a variable substition to
	% rename vars. The type renaming is applied before the type
	% substitution.

:- pred apply_substitutions_to_typeclass_var_map(
		map(class_constraint, prog_var), tsubst, map(tvar, type),
		map(prog_var, prog_var), map(class_constraint, prog_var)).
:- mode apply_substitutions_to_typeclass_var_map(in, in, in, in, out) is det.

:- pred apply_rec_subst_to_constraints(tsubst, class_constraints,
	class_constraints).
:- mode apply_rec_subst_to_constraints(in, in, out) is det.

:- pred apply_rec_subst_to_constraint_list(tsubst,
		list(class_constraint), list(class_constraint)).
:- mode apply_rec_subst_to_constraint_list(in, in, out) is det.

:- pred apply_rec_subst_to_constraint(tsubst, class_constraint,
	class_constraint).
:- mode apply_rec_subst_to_constraint(in, in, out) is det.

:- pred apply_subst_to_constraints(tsubst, class_constraints,
	class_constraints).
:- mode apply_subst_to_constraints(in, in, out) is det.

:- pred apply_subst_to_constraint_list(tsubst, list(class_constraint),
	list(class_constraint)).
:- mode apply_subst_to_constraint_list(in, in, out) is det.

:- pred apply_subst_to_constraint(tsubst, class_constraint,
	class_constraint).
:- mode apply_subst_to_constraint(in, in, out) is det.

:- pred apply_subst_to_constraint_proofs(tsubst, 
		map(class_constraint, constraint_proof),
		map(class_constraint, constraint_proof)).
:- mode apply_subst_to_constraint_proofs(in, in, out) is det.

:- pred apply_rec_subst_to_constraint_proofs(tsubst, 
	map(class_constraint, constraint_proof),
	map(class_constraint, constraint_proof)).
:- mode apply_rec_subst_to_constraint_proofs(in, in, out) is det.

:- pred apply_variable_renaming_to_constraints(map(tvar, tvar), 
	class_constraints, class_constraints).
:- mode apply_variable_renaming_to_constraints(in, in, out) is det.

:- pred apply_variable_renaming_to_constraint_list(map(tvar, tvar), 
	list(class_constraint), list(class_constraint)).
:- mode apply_variable_renaming_to_constraint_list(in, in, out) is det.

:- pred apply_variable_renaming_to_constraint(map(tvar, tvar), 
	class_constraint, class_constraint).
:- mode apply_variable_renaming_to_constraint(in, in, out) is det.

% Apply a renaming (partial map) to a list.
% Useful for applying a variable renaming to a list of variables.
:- pred apply_partial_map_to_list(list(T), map(T, T), list(T)).
:- mode apply_partial_map_to_list(in, in, out) is det.

% strip out the prog_context fields, replacing them with empty
% prog_context (as obtained by term__context_init/1)
% in a type or list of types
:- pred strip_prog_contexts(list(term(T))::in, list(term(T))::out) is det.
:- pred strip_prog_context(term(T)::in, term(T)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, require, std_util.
:- import_module prog_io, prog_io_goal, prog_util.

type_util__type_id_module(_ModuleInfo, TypeName - _Arity, ModuleName) :-
	sym_name_get_module_name(TypeName, unqualified(""), ModuleName).

type_util__type_id_name(_ModuleInfo, Name0 - _Arity, Name) :-
	unqualify_name(Name0, Name).

type_util__type_id_arity(_ModuleInfo, _Name - Arity, Arity).

type_is_atomic(Type, ModuleInfo) :-
	classify_type(Type, ModuleInfo, BuiltinType),
	BuiltinType \= polymorphic_type,
	BuiltinType \= pred_type,
	BuiltinType \= user_type.

type_util__var(term__variable(Var), Var).

type_id_is_hand_defined(qualified(PrivateBuiltin, "type_info") - 1) :-
	mercury_private_builtin_module(PrivateBuiltin).
type_id_is_hand_defined(qualified(PrivateBuiltin, "base_type_info") - 1) :-
	mercury_private_builtin_module(PrivateBuiltin).
type_id_is_hand_defined(qualified(PrivateBuiltin, "typeclass_info") - 1) :-
	mercury_private_builtin_module(PrivateBuiltin).
type_id_is_hand_defined(qualified(PrivateBuiltin, "base_typeclass_info") - 1) :-
	mercury_private_builtin_module(PrivateBuiltin).

%-----------------------------------------------------------------------------%

	% Given a type, determine what sort of type it is.

classify_type(VarType, ModuleInfo, Type) :-
	( type_to_type_id(VarType, TypeId, _) ->
		( TypeId = unqualified("character") - 0 ->
			Type = char_type
		; TypeId = unqualified("int") - 0 ->
			Type = int_type
		; TypeId = unqualified("float") - 0 ->
			Type = float_type
		; TypeId = unqualified("string") - 0 ->
			Type = str_type
		; type_id_is_higher_order(TypeId, _) ->
			Type = pred_type
		; type_id_is_enumeration(TypeId, ModuleInfo) ->
			Type = enum_type
		;
			Type = user_type
		)
	;
		Type = polymorphic_type
	).

type_is_higher_order(Type, PredOrFunc, PredArgTypes) :-
	(
		Type = term__functor(term__atom("pred"),
					PredArgTypes, _),
		PredOrFunc = predicate
	;
		Type = term__functor(term__atom("="),
				[term__functor(term__atom("func"),
					FuncArgTypes, _),
				 FuncRetType], _),
		list__append(FuncArgTypes, [FuncRetType], PredArgTypes),
		PredOrFunc = function
	).

type_id_is_higher_order(SymName - Arity, PredOrFunc) :-
	unqualify_name(SymName, TypeName),
	( 
		TypeName = "pred",
		PredOrFunc = predicate
	; 
		TypeName = "=",
		Arity = 2,
		PredOrFunc = function
	).

:- pred type_id_is_enumeration(type_id, module_info).
:- mode type_id_is_enumeration(in, in) is semidet.

type_id_is_enumeration(TypeId, ModuleInfo) :-
	module_info_types(ModuleInfo, TypeDefnTable),
	map__search(TypeDefnTable, TypeId, TypeDefn),
	hlds_data__get_type_defn_body(TypeDefn, TypeBody),
	TypeBody = du_type(_, _, IsEnum, _),
	IsEnum = yes.

type_to_type_id(Type, SymName - Arity, Args) :-
	sym_name_and_args(Type, SymName, Args1),

	% `private_builtin:constraint' is introduced by polymorphism, and
	% should only appear as the argument of a `typeclass:info/1' type.
	% It behaves sort of like a type variable, so according to the
	% specification of `type_to_type_id', it should cause failure.
	% There isn't a definition in the type table.
	mercury_private_builtin_module(PrivateBuiltin),
	SymName \= qualified(PrivateBuiltin, "constraint"),

	% higher order types may have representations where
	% their arguments don't directly correspond to the
	% arguments of the term.
	(
		type_is_higher_order(Type, _, PredArgTypes) 
	->
		Args = PredArgTypes,
		list__length(Args1, Arity)	% functions have arity 2, 
						% (they are =/2)
	;
		Args = Args1,
		list__length(Args, Arity)
	).

construct_type(TypeId, Args, Type) :-
	term__context_init(Context),
	construct_type(TypeId, Args, Context, Type).

construct_type(TypeId, Args, Context, Type) :-
	(
		type_id_is_higher_order(TypeId, PredOrFunc)
	->
		(
			PredOrFunc = predicate,
			NewArgs = Args
		;
			PredOrFunc = function,
			pred_args_to_func_args(Args, FuncArgTypes, FuncRetType),
			NewArgs = [term__functor(term__atom("func"),
						FuncArgTypes, Context),
					 FuncRetType]
		)
	;
		NewArgs = Args
	),
	TypeId = SymName - _,
	construct_qualified_term(SymName, NewArgs, Context, Type).

%-----------------------------------------------------------------------------%

	% Given a constant and an arity, return a type_id.
	% This really ought to take a name and an arity -
	% use of integers/floats/strings as type names should
	% be rejected by the parser in prog_io.m, not in module_qual.m.

make_type_id(term__atom(Name), Arity, unqualified(Name) - Arity).

%-----------------------------------------------------------------------------%

	% If the type is a du type, return the list of its constructors.

type_constructors(Type, ModuleInfo, Constructors) :-
	type_to_type_id(Type, TypeId, TypeArgs),
	module_info_types(ModuleInfo, TypeTable),
	map__search(TypeTable, TypeId, TypeDefn),
	hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),
	hlds_data__get_type_defn_body(TypeDefn, TypeBody),
	TypeBody = du_type(Constructors0, _, _, _),
	substitute_type_args(TypeParams, TypeArgs, Constructors0,
		Constructors).

%-----------------------------------------------------------------------------%

type_util__get_cons_id_arg_types(ModuleInfo, VarType, ConsId, ArgTypes) :-
	(
		type_to_type_id(VarType, TypeId, TypeArgs),
		module_info_ctors(ModuleInfo, Ctors),
		% will fail for builtin cons_ids.
		map__search(Ctors, ConsId, ConsDefns),
		CorrectCons = lambda([ConsDefn::in] is semidet, (
				ConsDefn = hlds_cons_defn(_, _, _, TypeId, _)
			)),
		list__filter(CorrectCons, ConsDefns,
			[hlds_cons_defn(_, _, ArgTypes0, _, _)]),
		ArgTypes0 \= []
	->
		module_info_types(ModuleInfo, Types),
		map__lookup(Types, TypeId, TypeDefn),
		hlds_data__get_type_defn_tparams(TypeDefn, TypeDefnParams),
		term__term_list_to_var_list(TypeDefnParams, TypeDefnVars),
		term__substitute_corresponding_list(TypeDefnVars, TypeArgs,
			ArgTypes0, ArgTypes)
	;
		ArgTypes = []
	).

%-----------------------------------------------------------------------------%

	% The checks for type_info and base_type_info
	% are needed because those types lie about their
	% arity; it might be cleaner to change that in
	% private_builtin.m, but that would cause some
	% bootstrapping difficulties.
	% It might be slightly better to check for private_builtin:type_info
	% etc. rather than just checking the unqualified type name,
	% but I found it difficult to verify that the constructors
	% would always be fully module-qualified at points where
	% type_is_no_tag_type/3 is called.

type_is_no_tag_type(Ctors, Ctor, Type) :-
	Ctors = [SingleCtor],
	SingleCtor = ctor(ExistQVars, _Constraints, Ctor, [_FieldName - Type]),
	ExistQVars = [],
	unqualify_name(Ctor, Name),
	Name \= "type_info",
	Name \= "base_type_info",
	Name \= "typeclass_info",
	Name \= "base_typeclass_info".

%-----------------------------------------------------------------------------%

	% Substitute the actual values of the type parameters
	% in list of constructors, for a particular instance of
	% a polymorphic type.

:- pred substitute_type_args(list(type_param), list(type),
				list(constructor), list(constructor)).
:- mode substitute_type_args(in, in, in, out) is det.

substitute_type_args(TypeParams0, TypeArgs, Constructors0, Constructors) :-
	( TypeParams0 = [] ->
		Constructors = Constructors0
	;
		term__term_list_to_var_list(TypeParams0, TypeParams),
		map__from_corresponding_lists(TypeParams, TypeArgs, Subst),
		substitute_type_args_2(Constructors0, Subst, Constructors)
	).

:- pred substitute_type_args_2(list(constructor), tsubst,
				list(constructor)).
:- mode substitute_type_args_2(in, in, out) is det.

substitute_type_args_2([], _, []).
substitute_type_args_2([Ctor0| Ctors0], Subst, [Ctor | Ctors]) :-
	% Note: prog_io.m ensures that the existentially quantified
	% variables, if any, are distinct from the parameters,
	% and that the (existential) constraints can only contain
	% existentially quantified variables, so there's
	% no need to worry about applying the substitution to
	% ExistQVars or Constraints
	Ctor0 = ctor(ExistQVars, Constraints, Name, Args0),
	Ctor = ctor(ExistQVars, Constraints, Name, Args),
	substitute_type_args_3(Args0, Subst, Args),
	substitute_type_args_2(Ctors0, Subst, Ctors).

:- pred substitute_type_args_3(list(constructor_arg), tsubst,
				list(constructor_arg)).
:- mode substitute_type_args_3(in, in, out) is det.

substitute_type_args_3([], _, []).
substitute_type_args_3([Name - Arg0 | Args0], Subst, [Name - Arg | Args]) :-
	term__apply_substitution(Arg0, Subst, Arg),
	substitute_type_args_3(Args0, Subst, Args).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Check whether TypesA subsumes TypesB, and if so return
	% a type substitution that will map from TypesA to TypesB.

type_list_subsumes(TypesA, TypesB, TypeSubst) :-
	%
	% TypesA subsumes TypesB iff TypesA can be unified with TypesB
	% without binding any of the type variables in TypesB.
	%
	term__vars_list(TypesB, TypesBVars),
	map__init(TypeSubst0),
	type_unify_list(TypesA, TypesB, TypesBVars, TypeSubst0, TypeSubst).

%-----------------------------------------------------------------------------%

	% Types are represented as terms, but we can't just use term__unify
	% because we need to avoid binding any of the "head type params"
	% (the type variables that occur in the head of the clause),
	% and because one day we might want to handle equivalent types.

type_unify(term__variable(X), term__variable(Y), HeadTypeParams,
		Bindings0, Bindings) :-
	( list__member(Y, HeadTypeParams) ->
		type_unify_head_type_param(X, Y, HeadTypeParams,
			Bindings0, Bindings)
	; list__member(X, HeadTypeParams) ->
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
			( SubstBindingOfX = term__variable(Y) ->
				Bindings = Bindings0
			;
				\+ term__occurs(SubstBindingOfX, Y,
					Bindings0),
				map__det_insert(Bindings0, Y, SubstBindingOfX,
					Bindings)
			)
		)
	;
		( map__search(Bindings0, Y, BindingOfY) ->
			term__apply_rec_substitution(BindingOfY,
				Bindings0, SubstBindingOfY),
			% X is a type variable which hasn't been bound yet
			( SubstBindingOfY = term__variable(X) ->
				Bindings = Bindings0
			;
				\+ term__occurs(SubstBindingOfY, X,
					Bindings0),
				map__det_insert(Bindings0, X, SubstBindingOfY,
					Bindings)
			)
		;
			% both X and Y are unbound type variables -
			% bind one to the other
			( X = Y ->
				Bindings = Bindings0
			; 
				map__det_insert(Bindings0, X,
					term__variable(Y), Bindings)
			)
		)
	).

type_unify(term__variable(X), term__functor(F, As, C), HeadTypeParams,
		Bindings0, Bindings) :-
	( 
		map__search(Bindings0, X, BindingOfX)
	->
		type_unify(BindingOfX, term__functor(F, As, C),
			HeadTypeParams, Bindings0, Bindings)
	;
		\+ term__occurs_list(As, X, Bindings0),
		\+ list__member(X, HeadTypeParams),
		map__det_insert(Bindings0, X, term__functor(F, As, C),
			Bindings)
	).

type_unify(term__functor(F, As, C), term__variable(X), HeadTypeParams,
		Bindings0, Bindings) :-
	( 
		map__search(Bindings0, X, BindingOfX)
	->
		type_unify(term__functor(F, As, C), BindingOfX,
			HeadTypeParams, Bindings0, Bindings)
	;
		\+ term__occurs_list(As, X, Bindings0),
		\+ list__member(X, HeadTypeParams),
		map__det_insert(Bindings0, X, term__functor(F, As, C),
			Bindings)
	).

type_unify(term__functor(FX, AsX, _CX), term__functor(FY, AsY, _CY),
		HeadTypeParams, Bindings0, Bindings) :-
	list__length(AsX, ArityX),
	list__length(AsY, ArityY),
	(
		FX = FY,
		ArityX = ArityY
	->
		type_unify_list(AsX, AsY, HeadTypeParams, Bindings0, Bindings)
	;
		fail
	).

	% XXX Instead of just failing if the functors' name/arity is different,
	% we should check here if these types have been defined
	% to be equivalent using equivalence types.  But this
	% is difficult because (1) it causes typevarset synchronization
	% problems, and (2) the relevant variables TypeInfo, TVarSet0, TVarSet
	% haven't been passed in to here.

/*******
	...
	;
		replace_eqv_type(FX, ArityX, AsX, EqvType)
	->
		type_unify(EqvType, term__functor(FY, AsY, CY),
			HeadTypeParams, Bindings0, Bindings)
	;
		replace_eqv_type(FY, ArityY, AsY, EqvType)
	->
		type_unify(term__functor(FX, AsX, CX), EqvType,
			HeadTypeParams, Bindings0, Bindings)
	;
		fail
	).

:- pred replace_eqv_type(const, int, list(type), type).
:- mode replace_eqv_type(in, in, in, out) is semidet.

replace_eqv_type(Functor, Arity, Args, EqvType) :-

	% XXX magically_obtain(TypeTable, TVarSet0, TVarSet)

	make_type_id(Functor, Arity, TypeId),
	map__search(TypeTable, TypeId, TypeDefn),
	TypeDefn = hlds_type_defn(TypeVarSet, TypeParams0,
			eqv_type(EqvType0), _Condition, Context, _Status),
	varset__merge(TVarSet0, TypeVarSet, [EqvType0 | TypeParams0],
			TVarSet, [EqvType1, TypeParams1]),
	type_param_to_var_list(TypeParams1, TypeParams),
	term__substitute_corresponding(EqvType1, TypeParams, AsX,
		EqvType).

******/

type_unify_list([], [], _) --> [].
type_unify_list([X | Xs], [Y | Ys], HeadTypeParams) -->
	type_unify(X, Y, HeadTypeParams),
	type_unify_list(Xs, Ys, HeadTypeParams).

:- pred type_unify_head_type_param(tvar, tvar, list(tvar), tsubst, tsubst).
:- mode type_unify_head_type_param(in, in, in, in, out) is semidet.

type_unify_head_type_param(Var, HeadVar, HeadTypeParams, Bindings0,
		Bindings) :-
	( map__search(Bindings0, Var, BindingOfVar) ->
		BindingOfVar = term__variable(Var2),
		type_unify_head_type_param(Var2, HeadVar, HeadTypeParams,
			Bindings0, Bindings)
	;
		( Var = HeadVar ->
			Bindings = Bindings0
		;
			\+ list__member(Var, HeadTypeParams),
			map__det_insert(Bindings0, Var,
				term__variable(HeadVar), Bindings)
		)
	).

%-----------------------------------------------------------------------------%

type_util__vars(Type, Tvars) :-
	term__vars(Type, Tvars).

%-----------------------------------------------------------------------------%

apply_substitution_to_type_map(VarTypes0, Subst, VarTypes) :-
	% optimize the common case of an empty type substitution
	( map__is_empty(Subst) ->
		VarTypes = VarTypes0
	;
		map__keys(VarTypes0, Vars),
		apply_substitution_to_type_map_2(Vars, VarTypes0, Subst,
			VarTypes)
	).

:- pred apply_substitution_to_type_map_2(list(prog_var)::in,
		map(prog_var, type)::in, tsubst::in, map(prog_var, type)::out)
		is det.

apply_substitution_to_type_map_2([], VarTypes, _Subst, VarTypes).
apply_substitution_to_type_map_2([Var | Vars], VarTypes0, Subst,
		VarTypes) :-
	map__lookup(VarTypes0, Var, VarType0),
	term__apply_substitution(VarType0, Subst, VarType),
	map__det_update(VarTypes0, Var, VarType, VarTypes1),
	apply_substitution_to_type_map_2(Vars, VarTypes1, Subst, VarTypes).

%-----------------------------------------------------------------------------%

apply_rec_substitution_to_type_map(VarTypes0, Subst, VarTypes) :-
	% optimize the common case of an empty type substitution
	( map__is_empty(Subst) ->
		VarTypes = VarTypes0
	;
		map__keys(VarTypes0, Vars),
		apply_rec_substitution_to_type_map_2(Vars, VarTypes0, Subst,
			VarTypes)
	).

:- pred apply_rec_substitution_to_type_map_2(list(prog_var)::in,
		map(prog_var, type)::in, tsubst::in, map(prog_var, type)::out)
		is det.

apply_rec_substitution_to_type_map_2([], VarTypes, _Subst, VarTypes).
apply_rec_substitution_to_type_map_2([Var | Vars], VarTypes0, Subst,
		VarTypes) :-
	map__lookup(VarTypes0, Var, VarType0),
	term__apply_rec_substitution(VarType0, Subst, VarType),
	map__det_update(VarTypes0, Var, VarType, VarTypes1),
	apply_rec_substitution_to_type_map_2(Vars, VarTypes1, Subst, VarTypes).

%-----------------------------------------------------------------------------%

apply_substitutions_to_var_map(VarMap0, TRenaming, TSubst, Subst, VarMap) :-
	% optimize the common case of empty substitutions
	(
		map__is_empty(Subst),
		map__is_empty(TSubst),
		map__is_empty(TRenaming)
	->
		VarMap = VarMap0
	;
		map__keys(VarMap0, TVars),
		map__init(NewVarMap),
		apply_substitutions_to_var_map_2(TVars, VarMap0,
			TRenaming, TSubst, Subst, NewVarMap, VarMap)
	).


:- pred apply_substitutions_to_var_map_2(list(tvar)::in, map(tvar,
		type_info_locn)::in, tsubst::in, map(tvar, type)::in,
		map(prog_var, prog_var)::in, map(tvar, type_info_locn)::in, 
		map(tvar, type_info_locn)::out) is det.

apply_substitutions_to_var_map_2([], _VarMap0, _, _, _, NewVarMap, NewVarMap).
apply_substitutions_to_var_map_2([TVar | TVars], VarMap0, TRenaming,
		TSubst, VarSubst, NewVarMap0, NewVarMap) :-
	map__lookup(VarMap0, TVar, Locn),
	type_info_locn_var(Locn, Var),
	
		% find the new var, if there is one
	( map__search(VarSubst, Var, NewVar0) ->
		NewVar = NewVar0
	;
		NewVar = Var
	),
	type_info_locn_set_var(Locn, NewVar, NewLocn),

		% find the new tvar, if there is one, otherwise just
		% create the old var as a type variable.
	(
		map__search(TRenaming, TVar, NewTVar0)
	->
		( NewTVar0 = term__variable(NewTVar1) ->
			NewTVar2 = NewTVar1
		;
			% varset__merge_subst only returns var->var mappings,
			% never var->term.
			error(
			"apply_substitution_to_var_map_2: weird type renaming")
		)
	; 
		% The variable wasn't renamed.
		NewTVar2 = TVar
	),

	term__apply_rec_substitution(term__variable(NewTVar2),
		TSubst, NewType),

		% if the tvar is still a variable, insert it into the
		% map with the new var.
	( type_util__var(NewType, NewTVar) ->
		% Don't abort if two old type variables
		% map to the same new type variable.
		map__set(NewVarMap0, NewTVar, NewLocn, NewVarMap1)
	;
		NewVarMap1 = NewVarMap0
	),
	apply_substitutions_to_var_map_2(TVars, VarMap0, TRenaming,
		TSubst, VarSubst, NewVarMap1, NewVarMap).

%-----------------------------------------------------------------------------%

apply_substitutions_to_typeclass_var_map(VarMap0,
		TRenaming, TSubst, Subst, VarMap) :-
	map__to_assoc_list(VarMap0, VarAL0),
	list__map(apply_substitutions_to_typeclass_var_map_2(TRenaming,
		TSubst, Subst), VarAL0, VarAL),
	map__from_assoc_list(VarAL, VarMap).

:- pred apply_substitutions_to_typeclass_var_map_2(tsubst, map(tvar, type),
		map(prog_var, prog_var), pair(class_constraint, prog_var),
		pair(class_constraint, prog_var)).
:- mode apply_substitutions_to_typeclass_var_map_2(in, in,
		in, in, out) is det.
	
apply_substitutions_to_typeclass_var_map_2(TRenaming, TSubst, VarRenaming,
		Constraint0 - Var0, Constraint - Var) :-
	apply_subst_to_constraint(TRenaming, Constraint0, Constraint1),
	apply_rec_subst_to_constraint(TSubst, Constraint1, Constraint),

	( map__search(VarRenaming, Var0, Var1) ->
		Var = Var1
	;
		Var = Var0
	).

%-----------------------------------------------------------------------------%

apply_rec_subst_to_constraints(Subst, Constraints0, Constraints) :-
	Constraints0 = constraints(UnivCs0, ExistCs0),
	apply_rec_subst_to_constraint_list(Subst, UnivCs0, UnivCs),
	apply_rec_subst_to_constraint_list(Subst, ExistCs0, ExistCs),
	Constraints = constraints(UnivCs, ExistCs).

apply_rec_subst_to_constraint_list(Subst, Constraints0, Constraints) :-
	list__map(apply_rec_subst_to_constraint(Subst), Constraints0,
		Constraints).

apply_rec_subst_to_constraint(Subst, Constraint0, Constraint) :-
	Constraint0 = constraint(ClassName, Types0),
	term__apply_rec_substitution_to_list(Types0, Subst, Types1),
	% we need to maintain the invariant that types in class constraints
	% do not have any information in their prog_context fields
	strip_prog_contexts(Types1, Types),
	Constraint  = constraint(ClassName, Types).

apply_subst_to_constraints(Subst,
		constraints(UniversalCs0, ExistentialCs0),
		constraints(UniversalCs, ExistentialCs)) :-
	apply_subst_to_constraint_list(Subst, UniversalCs0, UniversalCs),
	apply_subst_to_constraint_list(Subst, ExistentialCs0, ExistentialCs).

apply_subst_to_constraint_list(Subst, Constraints0, Constraints) :-
	list__map(apply_subst_to_constraint(Subst), Constraints0, Constraints).

apply_subst_to_constraint(Subst, Constraint0, Constraint) :-
	Constraint0 = constraint(ClassName, Types0),
	term__apply_substitution_to_list(Types0, Subst, Types),
	Constraint  = constraint(ClassName, Types).

apply_subst_to_constraint_proofs(Subst, Proofs0, Proofs) :-
	map__init(Empty),
	map__foldl(
		lambda([Constraint0::in, Proof0::in, Map0::in, Map::out] is det,
		(
			apply_subst_to_constraint(Subst, Constraint0,
				Constraint), 
			(
				Proof0 = apply_instance(_),
				Proof = Proof0
			;
				Proof0 = superclass(Super0),
				apply_subst_to_constraint(Subst, Super0, 
					Super),
				Proof = superclass(Super)
			),
			map__set(Map0, Constraint, Proof, Map)
		)),
	Proofs0, Empty, Proofs).

apply_rec_subst_to_constraint_proofs(Subst, Proofs0, Proofs) :-
	map__init(Empty),
	map__foldl(
		lambda([Constraint0::in, Proof0::in, Map0::in, Map::out] is det,
		(
			apply_rec_subst_to_constraint(Subst, Constraint0,
				Constraint), 
			(
				Proof0 = apply_instance(_),
				Proof = Proof0
			;
				Proof0 = superclass(Super0),
				apply_rec_subst_to_constraint(Subst, Super0, 
					Super),
				Proof = superclass(Super)
			),
			map__set(Map0, Constraint, Proof, Map)
		)),
	Proofs0, Empty, Proofs).

apply_variable_renaming_to_constraints(Renaming,
		constraints(UniversalCs0, ExistentialCs0),
		constraints(UniversalCs, ExistentialCs)) :-
	apply_variable_renaming_to_constraint_list(Renaming,
			UniversalCs0, UniversalCs),
	apply_variable_renaming_to_constraint_list(Renaming,
			ExistentialCs0, ExistentialCs).
	
apply_variable_renaming_to_constraint_list(Renaming, Constraints0,
		Constraints) :-
	list__map(apply_variable_renaming_to_constraint(Renaming),
			Constraints0, Constraints).

apply_variable_renaming_to_constraint(Renaming, Constraint0, Constraint) :-
	Constraint0 = constraint(ClassName, ClassArgTypes0),
	term__apply_variable_renaming_to_list(ClassArgTypes0,
		Renaming, ClassArgTypes),
	Constraint = constraint(ClassName, ClassArgTypes).

%-----------------------------------------------------------------------------%

apply_partial_map_to_list([], _PartialMap, []).
apply_partial_map_to_list([X|Xs], PartialMap, [Y|Ys]) :-
	( map__search(PartialMap, X, Y0) ->
		Y = Y0
	;
		Y = X
	),
	apply_partial_map_to_list(Xs, PartialMap, Ys).

%-----------------------------------------------------------------------------%

strip_prog_contexts(Terms, StrippedTerms) :-
	list__map(strip_prog_context, Terms, StrippedTerms).
	
strip_prog_context(term__variable(V), term__variable(V)).
strip_prog_context(term__functor(F, As0, _C0),
		term__functor(F, As, C)) :-
	term__context_init(C),
	strip_prog_contexts(As0, As).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
