%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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

%-----------------------------------------------------------------------------%

	% Succeed iff type is an "atomic" type - one which can be
	% unified using a simple_test rather than a complicated_unify.

:- pred type_is_atomic(type, module_info).
:- mode type_is_atomic(in, in) is semidet.

	% type_is_higher_order_type(Type, PredOrFunc, ArgTypes) succeeds iff
	% Type is a higher-order predicate or function type with the specified
	% argument types (for functions, the return type is appended to the
	% end of the argument types).

:- pred type_is_higher_order(type, pred_or_func, list(type)).
:- mode type_is_higher_order(in, out, out) is semidet.

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
			;	user_type(type).

	% Given a non-variable type, return it's type-id and argument types.

:- pred type_to_type_id(type, type_id, list(type)).
:- mode type_to_type_id(in, out, out) is semidet.

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

	% If the type is a du type, return the list of it's constructors.

:- pred type_constructors(type, module_info, list(constructor)).
:- mode type_constructors(in, in, out) is semidet.

	% Unify (with occurs check) two types with respect to a type
	% substitution and update the type bindings.
	% The third argument is a list of type variables which cannot
	% be bound (i.e. head type variables).

:- pred type_unify(type, type, list(tvar), tsubst, tsubst).
:- mode type_unify(in, in, in, in, out) is semidet.

:- pred type_unify_list(list(type), list(type), list(tvar), tsubst, tsubst).
:- mode type_unify_list(in, in, in, in, out) is semidet.

	% type_list_subsumes(TypesA, TypesB, Subst) succeeds iff the list
	% TypesA subsumes (is more general than) TypesB, producing a
	% type substitution which when applied to TypesA will give TypesB.

:- pred type_list_subsumes(list(type), list(type), tsubst).
:- mode type_list_subsumes(in, in, out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, list, term, require, map, std_util.
:- import_module prog_util.

type_util__type_id_module(_ModuleInfo, _TypeId, ModuleName) :-
	% XXX Module qualifiers not yet implemented
	ModuleName = "xxx".

type_util__type_id_name(_ModuleInfo, Name0 - _Arity, Name) :-
	unqualify_name(Name0, Name).

type_util__type_id_arity(_ModuleInfo, _Name - Arity, Arity).

type_is_atomic(Type, ModuleInfo) :-
	classify_type(Type, ModuleInfo, BuiltinType),
	BuiltinType \= polymorphic_type,
	BuiltinType \= pred_type,
	BuiltinType \= user_type(_).

%-----------------------------------------------------------------------------%

	% Given a type, determine what sort of type it is.

classify_type(VarType, ModuleInfo, Type) :-
	(
		VarType = term__variable(_)
	->
		Type = polymorphic_type
	;
		VarType = term__functor(term__atom("character"), [], _)
	->
		Type = char_type
	;
		VarType = term__functor(term__atom("int"), [], _)
	->
		Type = int_type
	;
		VarType = term__functor(term__atom("float"), [], _)
	->
		Type = float_type
	;
		VarType = term__functor(term__atom("string"), [], _)
	->
		Type = str_type
	;
		type_is_higher_order(VarType, _, _)
	->
		Type = pred_type
	;
		type_is_enumeration(VarType, ModuleInfo)
	->
		Type = enum_type
	;
		Type = user_type(VarType)
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

:- pred type_is_enumeration(type, module_info).
:- mode type_is_enumeration(in, in) is semidet.

type_is_enumeration(Type, ModuleInfo) :-
	type_to_type_id(Type, TypeId, _),
	module_info_types(ModuleInfo, TypeDefnTable),
	map__search(TypeDefnTable, TypeId, TypeDefn),
	TypeDefn = hlds__type_defn(_, _, TypeBody, _, _),
	TypeBody = du_type(_, _, IsEnum),
	IsEnum = yes.

type_to_type_id(term__functor(Name, Args, _), TypeId, Args) :-
	list__length(Args, Arity),
	make_type_id(Name, Arity, TypeId).

%-----------------------------------------------------------------------------%

	% Given a constant and an arity, return a type_id.
	% This really ought to take a name and an arity -
	% use of integers/floats/strings as type names should
	% be rejected by the parser in prog_io.m, not in undef_types.m.

make_type_id(term__atom(Name), Arity, unqualified(Name) - Arity).

%-----------------------------------------------------------------------------%

	% If the type is a du type, return the list of it's constructors.

type_constructors(Type, ModuleInfo, Constructors) :-
	type_to_type_id(Type, TypeId, TypeArgs),
	module_info_types(ModuleInfo, TypeTable),
	map__search(TypeTable, TypeId, TypeDefn),
	TypeDefn = hlds__type_defn(_, TypeParams, TypeBody, _, _),
	TypeBody = du_type(Constructors0, _, _),
	substitute_type_args(TypeParams, TypeArgs, Constructors0,
		Constructors).

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
		substitute_type_args_2(Constructors0, TypeParams, TypeArgs,
			Constructors)
	).

:- pred substitute_type_args_2(list(constructor), list(var), list(type),
				list(constructor)).
:- mode substitute_type_args_2(in, in, in, out) is det.

substitute_type_args_2([], _TypeParams, _TypeArgs, []).
substitute_type_args_2([Name - Args0 | Ctors0], TypeParams, TypeArgs,
		[Name - Args | Ctors]) :-
	term__substitute_corresponding_list(TypeParams, TypeArgs, Args0, Args),
	substitute_type_args_2(Ctors0, TypeParams, TypeArgs, Ctors).

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

:- type_unify(X, Y, _, _, _) when X and Y.		% NU-Prolog indexing

type_unify(term__variable(X), term__variable(Y), HeadTypeParams, Bindings0,
		Bindings) :-
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
			( SubstBindingOfY = term__variable(X) ->
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
				map__set(Bindings0, X, term__variable(Y),
					Bindings)
			)
		)
	).

type_unify(term__variable(X), term__functor(F, As, C), HeadTypeParams,
		Bindings0, Bindings) :-
	( 
		map__search(Bindings0, X, BindingOfX)
	->
		type_unify(BindingOfX, term__functor(F, As, C), HeadTypeParams,
			Bindings0, Bindings)
	;
		\+ term__occurs_list(As, X, Bindings0),
		\+ list__member(X, HeadTypeParams),
		map__set(Bindings0, X, term__functor(F, As, C), Bindings)
	).

type_unify(term__functor(F, As, C), term__variable(X), HeadTypeParams,
		Bindings0, Bindings) :-
	( 
		map__search(Bindings0, X, BindingOfX)
	->
		type_unify(term__functor(F, As, C), BindingOfX, HeadTypeParams,
			Bindings0, Bindings)
	;
		\+ term__occurs_list(As, X, Bindings0),
		\+ list__member(X, HeadTypeParams),
		map__set(Bindings0, X, term__functor(F, As, C), Bindings)
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
		type_unify(EqvType, term__functor(FY, AsY, CY), HeadTypeParams,
				Bindings0, Bindings)
	;
		replace_eqv_type(FY, ArityY, AsY, EqvType)
	->
		type_unify(term__functor(FX, AsX, CX), EqvType, HeadTypeParams,
				Bindings0, Bindings)
	;
		fail
	).

:- pred replace_eqv_type(const, int, list(type), type).
:- mode replace_eqv_type(in, in, in, out) is semidet.

replace_eqv_type(Functor, Arity, Args, EqvType) :-

	% XXX magically_obtain(TypeTable, TVarSet0, TVarSet)

	make_type_id(Functor, Arity, TypeId),
	map__search(TypeTable, TypeId, TypeDefn),
	TypeDefn = hlds__type_defn(TypeVarSet, TypeParams0,
			eqv_type(EqvType0), _Condition, Context),
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
			map__set(Bindings0, Var, term__variable(HeadVar),
				Bindings)
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
