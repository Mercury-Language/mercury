%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
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

:- module check_hlds__type_util.

:- interface.

:- import_module hlds__hlds_data.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module libs__globals.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module std_util.
:- import_module term.

%-----------------------------------------------------------------------------%

	% Succeed iff type is an "atomic" type - one which can be
	% unified using a simple_test rather than a complicated_unify.
	%
:- pred type_is_atomic((type)::in, module_info::in) is semidet.

:- pred type_ctor_is_atomic(type_ctor::in, module_info::in) is semidet.

	% The list of type_ctors which are builtins which do not have a
	% hlds_type_defn.
	%
:- func builtin_type_ctors_with_no_hlds_type_defn = list(type_ctor).

	% Obtain the type definition and type definition body respectively,
	% if known, for the principal type constructor of the given type.
	%
	% Fail if the given type is a type variable.
	%
:- pred type_util__type_to_type_defn(module_info::in, (type)::in,
		hlds_type_defn::out) is semidet.

:- pred type_util__type_to_type_defn_body(module_info::in, (type)::in,
		hlds_type_body::out) is semidet.

	% Succeed iff there was either a `where equality is <predname>' or a
	% `where comparison is <predname>' declaration for the principal type
	% constructor of the specified type, and return the ids of the declared
	% unify and/or comparison predicates. Note that even if the type
	% constructor has only a `where comparison is' clause, it effectively
	% has user-defined equality, two values being equal only if the
	% compare pred returns equal.
	%
	% If the type is a type variable and thus has no principal type
	% constructor, fail.
	%
:- pred type_has_user_defined_equality_pred(module_info::in, (type)::in,
	unify_compare::out) is semidet.

:- pred type_body_has_user_defined_equality_pred(module_info::in,
	hlds_type_body::in, unify_compare::out) is semidet.

	% Succeed iff the principal type constructor for the given type
	% is a solver type.
	%
	% If the type is a type variable and thus has no principal type
	% constructor, fail.
	%
:- pred type_util__type_is_solver_type(module_info::in, (type)::in) is semidet.

:- pred type_util__type_has_solver_type_details(module_info::in, (type)::in,
	solver_type_details::out) is semidet.

:- pred type_util__type_body_has_solver_type_details(module_info::in,
	hlds_type_body::in, solver_type_details::out) is semidet.

:- pred type_util__is_solver_type(module_info::in, (type)::in) is semidet.

:- pred type_body_is_solver_type(module_info::in, hlds_type_body::in)
	is semidet.

	% Succeeds iff one or more of the type constructors for a given
	% type is existentially quantified.
	%
:- pred type_util__is_existq_type(module_info::in, (type)::in) is semidet.

	% Certain types, e.g. io__state and store__store(S),
	% are just dummy types used to ensure logical semantics;
	% there is no need to actually pass them, and so when
	% importing or exporting procedures to/from C, we don't
	% include arguments with these types.
	%
:- pred type_util__is_dummy_argument_type((type)::in) is semidet.

:- pred type_util__constructors_are_dummy_argument_type(list(constructor)::in)
	is semidet.

:- pred type_is_io_state((type)::in) is semidet.

:- pred type_is_aditi_state((type)::in) is semidet.

:- pred type_ctor_is_array(type_ctor::in) is semidet.

	% Remove an `aditi:state' from the given list if one is present.
	%
:- pred type_util__remove_aditi_state(list(type)::in, list(T)::in,
	list(T)::out) is det.

	% A test for types that are defined in Mercury, but whose definitions
	% are `lies', i.e. they are not sufficiently accurate for RTTI
	% structures describing the types. Since the RTTI will be hand defined,
	% the compiler shouldn't generate RTTI for these types.
	%
:- pred type_ctor_has_hand_defined_rtti(type_ctor::in, hlds_type_body::in)
	is semidet.

	% A test for type_info-related types that are introduced by
	% polymorphism.m.  These need to be handled specially in certain
	% places.  For example, mode inference never infers unique modes
	% for these types, since it would not be useful, and since we
	% want to minimize the number of different modes that we infer.
	%
:- pred is_introduced_type_info_type((type)::in) is semidet.

:- pred is_introduced_type_info_type_ctor(type_ctor::in) is semidet.

:- func is_introduced_type_info_type_category(type_category) = bool.

	% Given a list of variables, return the permutation
	% of that list which has all the type_info-related variables
	% preceding the non-type_info-related variables (with the relative
	% order of variables within each group being the same as in the
	% original list).
	%
:- func put_typeinfo_vars_first(list(prog_var), vartypes) = list(prog_var).

	% In the forwards mode, this predicate checks for a "new " prefix
	% at the start of the functor name, and removes it if present;
	% it fails if there is no such prefix.
	% In the reverse mode, this predicate prepends such a prefix.
	% (These prefixes are used for construction unifications
	% with existentially typed functors.)
	%
:- pred remove_new_prefix(sym_name, sym_name).
:- mode remove_new_prefix(in, out) is semidet.
:- mode remove_new_prefix(out, in) is det.

	% Given a type, determine what category its principal constructor
	% falls into.
	%
:- func classify_type(module_info, type) = type_category.

	% Given a type_ctor, determine what sort it is.
	%
:- func classify_type_ctor(module_info, type_ctor) = type_category.

:- type type_category
	--->	int_type
	;	char_type
	;	str_type
	;	float_type
	;	higher_order_type
	;	tuple_type
	;	enum_type
	;	variable_type
	;	type_info_type
	;	type_ctor_info_type
	;	typeclass_info_type
	;	base_typeclass_info_type
	;	void_type
	;	user_ctor_type.

	% Construct builtin types.
:- func int_type = (type).
:- func string_type = (type).
:- func float_type = (type).
:- func char_type = (type).
:- func void_type = (type).
:- func c_pointer_type = (type).
:- func heap_pointer_type = (type).
:- func sample_type_info_type = (type).
:- func sample_typeclass_info_type = (type).
:- func comparison_result_type = (type).
:- func aditi_state_type = (type).

	% Construct the types of type_infos and type_ctor_infos.
	%
:- func type_info_type = (type).
:- func type_ctor_info_type = (type).

	% Given a constant and an arity, return a type_ctor.
	% Fails if the constant is not an atom.
	%
:- pred make_type_ctor(const::in, int::in, type_ctor::out) is semidet.

	% Given a type_ctor, look up its module/name/arity
	%
:- pred type_util__type_ctor_module(module_info::in, type_ctor::in,
	module_name::out) is det.

:- pred type_util__type_ctor_name(module_info::in, type_ctor::in, string::out)
	is det.

:- pred type_util__type_ctor_arity(module_info::in, type_ctor::in, arity::out)
	is det.

	% If the type is a du type or a tuple type,
	% return the list of its constructors.
	%
:- pred type_constructors((type)::in, module_info::in, list(constructor)::out)
	is semidet.

	% Given a type on which it is possible to have a complete switch,
	% return the number of alternatives. (It is possible to have a complete
	% switch on any du type and on the builtin type character. It is not
	% feasible to have a complete switch on the builtin types integer,
	% float, and switch. One cannot have a switch on an abstract type,
	% and equivalence types will have been expanded out by the time
	% we consider switches.)
	%
:- pred type_util__switch_type_num_functors(module_info::in, (type)::in,
	int::out) is semidet.

	% Work out the types of the arguments of a functor,
	% given the cons_id and type of the functor.
	% Aborts if the functor is existentially typed.
	% Note that this will substitute appropriate values for
	% any type variables in the functor's argument types,
	% to match their bindings in the functor's type.
	%
:- pred type_util__get_cons_id_arg_types(module_info::in, (type)::in,
	cons_id::in, list(type)::out) is det.

	% The same as type_util__get_cons_id_arg_types except that it
	% fails rather than aborting if the functor is existentially
	% typed.
	%
:- pred type_util__get_cons_id_non_existential_arg_types(module_info::in,
	(type)::in, cons_id::in, list(type)::out) is semidet.

	% The same as type_util__get_cons_id_arg_types except that the
	% cons_id is output non-deterministically.
	% The cons_id is not module-qualified.
	%
:- pred type_util__cons_id_arg_types(module_info::in, (type)::in,
	cons_id::out, list(type)::out) is nondet.

	% Given a type and a cons_id, look up the definitions of that
	% type and constructor. Aborts if the cons_id is not user-defined.
	% Note that this will NOT bind type variables in the
	% functor's argument types; they will be left unbound,
	% so the caller can find out the original types from the constructor
	% definition.  The caller must do that sustitution itself if required.
	%
:- pred type_util__get_type_and_cons_defn(module_info::in, (type)::in,
	cons_id::in, hlds_type_defn::out, hlds_cons_defn::out) is det.

	% Like type_util__get_type_and_cons_defn (above), except that it
	% only returns the definition of the constructor, not the type.
	%
:- pred type_util__get_cons_defn(module_info::in, type_ctor::in, cons_id::in,
	hlds_cons_defn::out) is semidet.

	% Module-qualify the cons_id using module information from the type.
	% The second output value is the cons_id required for use in insts which
	% can be different from that used in types for typeclass_info and
	% type_info.
	% The list(prog_var) is the list of arguments to the cons_id and is just
	% used for obtaining the arity for typeclass_info and type_info
	% cons_ids.
	%
:- pred qualify_cons_id((type)::in, list(prog_var)::in, cons_id::in,
	cons_id::out, cons_id::out) is det.

	% Given a type and a cons_id, look up the definition of that
	% constructor; if it is existentially typed, return its definition,
	% otherwise fail.
	% Note that this will NOT bind type variables in the
	% functor's argument types; they will be left unbound,
	% so the caller can find out the original types from the constructor
	% definition.  The caller must do that sustitution itself if required.
	%
:- pred type_util__get_existq_cons_defn(module_info::in, (type)::in,
	cons_id::in, ctor_defn::out) is semidet.

:- pred type_util__is_existq_cons(module_info::in, (type)::in, cons_id::in)
	is semidet.

	% This type is used to return information about a constructor
	% definition, extracted from the hlds_type_defn and hlds_cons_defn
	% data types.
	%
:- type ctor_defn
	--->	ctor_defn(
			tvarset,
			existq_tvars,
			tvar_kind_map,	% kinds of existq_tvars
			list(prog_constraint),	% existential constraints
			list(type),	% functor argument types
			(type)		% functor result type
		).

	% Check whether a type is a no_tag type
	% (i.e. one with only one constructor, and
	% whose one constructor has only one argument),
	% and if so, return its constructor symbol and argument type.
	%
:- pred type_is_no_tag_type(module_info::in, (type)::in, sym_name::out,
	(type)::out) is semidet.

	% Check whether some constructors are a no_tag type
	% (i.e. one with only one constructor, and
	% whose one constructor has only one argument),
	% and if so, return its constructor symbol, argument type,
	% and the argument's name (if it has one).
	%
	% This doesn't do any checks for options that might be set
	% (such as turning off no_tag_types).  If you want those checks
	% you should use type_is_no_tag_type/4, or if you really know
	% what you are doing, perform the checks yourself.
	%
:- pred type_constructors_are_no_tag_type(list(constructor)::in, sym_name::out,
	(type)::out, maybe(string)::out) is semidet.

	% Given a list of constructors for a type, check whether that
	% type is a private_builtin:type_info/n or similar type.
	%
:- pred type_constructors_are_type_info(list(constructor)::in) is semidet.

	% type_constructors_should_be_no_tag(Ctors, ReservedTag, Globals,
	%	FunctorName, FunctorArgType, MaybeFunctorArgName):
	%
	% Check whether some constructors are a no_tag type, and that this
	% is compatible with the ReservedTag setting for this type and
	% the grade options set in the globals.
	%
:- pred type_constructors_should_be_no_tag(list(constructor)::in, bool::in,
	globals::in, sym_name::out, (type)::out, maybe(string)::out)
	is semidet.

	% Unify (with occurs check) two types with respect to a type
	% substitution and update the type bindings.
	% The third argument is a list of type variables which cannot
	% be bound (i.e. head type variables).
	%
	% No kind checking is done, since it is assumed that kind errors
	% will be picked up elsewhere.
	%
:- pred type_unify((type)::in, (type)::in, list(tvar)::in, tsubst::in,
	tsubst::out) is semidet.

:- pred type_unify_list(list(type)::in, list(type)::in, list(tvar)::in,
	tsubst::in, tsubst::out) is semidet.
	
	% type_list_subsumes(TypesA, TypesB, Subst) succeeds iff the list
	% TypesA subsumes (is more general than) TypesB, producing a
	% type substitution which when applied to TypesA will give TypesB.
	%
:- pred type_list_subsumes(list(type)::in, list(type)::in, tsubst::out)
	is semidet.

	% This does the same as type_list_subsumes, but aborts instead of
	% failing.
:- pred type_list_subsumes_det(list(type)::in, list(type)::in, tsubst::out)
	is det.

	% arg_type_list_subsumes(TVarSet, ArgTypes,
	%	CalleeTVarSet, CalleeExistQVars, CalleeArgTypes).
	%
	% Check that the argument types of the called predicate,
	% function or constructor subsume the types of the
	% arguments of the call. This checks that none
	% of the existentially quantified type variables of
	% the callee are bound.
	%
:- pred arg_type_list_subsumes(tvarset::in, list(type)::in,
	tvarset::in, tvar_kind_map::in, existq_tvars::in,
	list(type)::in) is semidet.

	% apply a type substitution (i.e. map from tvar -> type)
	% to all the types in a variable typing (i.e. map from var -> type).
	%
:- pred apply_subst_to_type_map(tsubst::in, vartypes::in, vartypes::out)
	is det.

	% same thing as above, except for a recursive substitution
	% (i.e. we keep applying the substitution recursively until
	% there are no more changes).
	%
:- pred apply_rec_subst_to_type_map(tsubst::in, vartypes::in, vartypes::out)
	is det.

:- pred apply_variable_renaming_to_type_map(tvar_renaming::in,
	vartypes::in, vartypes::out) is det.

:- pred apply_rec_subst_to_constraints(tsubst::in, hlds_constraints::in,
	hlds_constraints::out) is det.

:- pred apply_rec_subst_to_constraint_list(tsubst::in,
	list(hlds_constraint)::in, list(hlds_constraint)::out) is det.

:- pred apply_rec_subst_to_constraint(tsubst::in, hlds_constraint::in,
	hlds_constraint::out) is det.

:- pred apply_subst_to_constraints(tsubst::in, hlds_constraints::in,
	hlds_constraints::out) is det.

:- pred apply_subst_to_constraint_list(tsubst::in, list(hlds_constraint)::in,
	list(hlds_constraint)::out) is det.

:- pred apply_subst_to_constraint(tsubst::in, hlds_constraint::in,
	hlds_constraint::out) is det.

:- pred apply_subst_to_constraint_proofs(tsubst::in,
	constraint_proof_map::in, constraint_proof_map::out) is det.

:- pred apply_rec_subst_to_constraint_proofs(tsubst::in,
	constraint_proof_map::in, constraint_proof_map::out) is det.

:- pred apply_subst_to_constraint_map(tsubst::in,
	constraint_map::in, constraint_map::out) is det.

:- pred apply_rec_subst_to_constraint_map(tsubst::in,
	constraint_map::in, constraint_map::out) is det.

:- pred apply_variable_renaming_to_constraints(tvar_renaming::in,
	hlds_constraints::in, hlds_constraints::out) is det.

:- pred apply_variable_renaming_to_constraint_list(tvar_renaming::in,
	list(hlds_constraint)::in, list(hlds_constraint)::out) is det.

:- pred apply_variable_renaming_to_constraint(tvar_renaming::in,
	hlds_constraint::in, hlds_constraint::out) is det.

:- pred apply_variable_renaming_to_constraint_proofs(tvar_renaming::in,
	constraint_proof_map::in, constraint_proof_map::out) is det.

:- pred apply_variable_renaming_to_constraint_map(tvar_renaming::in,
	constraint_map::in, constraint_map::out) is det.

	% Apply a renaming (partial map) to a list.
	% Useful for applying a variable renaming to a list of variables.
	%
:- pred apply_partial_map_to_list(list(T)::in, map(T, T)::in, list(T)::out)
	is det.

	% cons_id_adjusted_arity(ModuleInfo, Type, ConsId):
	%
	% Returns the number of arguments of specified constructor id,
	% adjusted to include the extra typeclassinfo and typeinfo
	% arguments inserted by polymorphism.m for existentially
	% typed constructors.
	%
:- func cons_id_adjusted_arity(module_info, type, cons_id) = int.

%-----------------------------------------------------------------------------%

	% If possible, get the argument types for the cons_id.
	% We need to pass in the arity rather than using the arity
	% from the cons_id because the arity in the cons_id will not
	% include any extra type_info arguments for existentially
	% quantified types.
	%
:- pred maybe_get_cons_id_arg_types(module_info::in, maybe(type)::in,
	cons_id::in, arity::in, list(maybe(type))::out) is det.

:- pred maybe_get_higher_order_arg_types(maybe(type)::in, arity::in,
	list(maybe(type))::out) is det.

:- type polymorphism_cell
	--->	type_info_cell(type_ctor)
	;	typeclass_info_cell.

:- func cell_cons_id(polymorphism_cell) = cons_id.

:- func cell_inst_cons_id(polymorphism_cell, int) = cons_id.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs__foreign.
:- import_module check_hlds__purity.
:- import_module hlds__hlds_out.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_io_goal.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module require.
:- import_module string.
:- import_module svmap.
:- import_module varset.

type_util__type_ctor_module(_ModuleInfo, TypeName - _Arity, ModuleName) :-
	sym_name_get_module_name(TypeName, unqualified(""), ModuleName).

type_util__type_ctor_name(_ModuleInfo, Name0 - _Arity, Name) :-
	unqualify_name(Name0, Name).

type_util__type_ctor_arity(_ModuleInfo, _Name - Arity, Arity).

type_is_atomic(Type, ModuleInfo) :-
	type_to_ctor_and_args(Type, TypeCtor, _),
	type_ctor_is_atomic(TypeCtor, ModuleInfo).

type_ctor_is_atomic(TypeCtor, ModuleInfo) :-
	TypeCategory = classify_type_ctor(ModuleInfo, TypeCtor),
	type_category_is_atomic(TypeCategory) = yes.

:- func type_category_is_atomic(type_category) = bool.

type_category_is_atomic(int_type) = yes.
type_category_is_atomic(char_type) = yes.
type_category_is_atomic(str_type) = yes.
type_category_is_atomic(float_type) = yes.
type_category_is_atomic(higher_order_type) = no.
type_category_is_atomic(tuple_type) = no.
type_category_is_atomic(enum_type) = yes.
type_category_is_atomic(variable_type) = no.
type_category_is_atomic(type_info_type) = no.
type_category_is_atomic(type_ctor_info_type) = no.
type_category_is_atomic(typeclass_info_type) = no.
type_category_is_atomic(base_typeclass_info_type) = no.
type_category_is_atomic(void_type) = yes.
type_category_is_atomic(user_ctor_type) = no.

type_ctor_is_array(qualified(unqualified("array"), "array") - 1).

type_ctor_has_hand_defined_rtti(Type, Body) :-
	Type = qualified(mercury_private_builtin_module, Name) - Arity,
	( Name = "type_info", Arity = 1
	; Name = "type_ctor_info", Arity = 1
	; Name = "typeclass_info", Arity = 1
	; Name = "base_typeclass_info", Arity = 1
	; Name = "zero_type_info", Arity = 0
	; Name = "zero_type_ctor_info", Arity = 0
	; Name = "zero_typeclass_info", Arity = 0
	; Name = "zero_base_typeclass_info", Arity = 0
	),
	\+ (	Body = du_type(_, _, _, _, _, yes(_))
	   ;	Body = foreign_type(_)
	   ;	Body = solver_type(_, _)
	   ).

is_introduced_type_info_type(Type) :-
	type_to_ctor_and_args(Type, TypeCtor, _),
	is_introduced_type_info_type_ctor(TypeCtor).

is_introduced_type_info_type_ctor(TypeCtor) :-
	TypeCtor = qualified(PrivateBuiltin, Name) - Arity,
	mercury_private_builtin_module(PrivateBuiltin),
	( Name = "type_info", Arity = 1
	; Name = "type_ctor_info", Arity = 1
	; Name = "typeclass_info", Arity = 1
	; Name = "base_typeclass_info", Arity = 1
	; Name = "zero_type_info", Arity = 0
	; Name = "zero_type_ctor_info", Arity = 0
	; Name = "zero_typeclass_info", Arity = 0
	; Name = "zero_base_typeclass_info", Arity = 0
	).

is_introduced_type_info_type_category(int_type) = no.
is_introduced_type_info_type_category(char_type) = no.
is_introduced_type_info_type_category(str_type) = no.
is_introduced_type_info_type_category(float_type) = no.
is_introduced_type_info_type_category(higher_order_type) = no.
is_introduced_type_info_type_category(tuple_type) = no.
is_introduced_type_info_type_category(enum_type) = no.
is_introduced_type_info_type_category(variable_type) = no.
is_introduced_type_info_type_category(type_info_type) = yes.
is_introduced_type_info_type_category(type_ctor_info_type) = yes.
is_introduced_type_info_type_category(typeclass_info_type) = yes.
is_introduced_type_info_type_category(base_typeclass_info_type) = yes.
is_introduced_type_info_type_category(void_type) = no.
is_introduced_type_info_type_category(user_ctor_type) = no.

put_typeinfo_vars_first(VarsList, VarTypes) =
		TypeInfoVarsList ++ NonTypeInfoVarsList :-
	list__filter((pred(Var::in) is semidet :-
			Type = map__lookup(VarTypes, Var),
			is_introduced_type_info_type(Type)),
		VarsList, TypeInfoVarsList, NonTypeInfoVarsList).

remove_new_prefix(unqualified(Name0), unqualified(Name)) :-
	string__append("new ", Name, Name0).
remove_new_prefix(qualified(Module, Name0), qualified(Module, Name)) :-
	string__append("new ", Name, Name0).

%-----------------------------------------------------------------------------%

	% Given a type, determine what sort of type it is.

classify_type(ModuleInfo, VarType) = TypeCategory :-
	( type_to_ctor_and_args(VarType, TypeCtor, _) ->
		TypeCategory = classify_type_ctor(ModuleInfo, TypeCtor)
	;
		TypeCategory = variable_type
	).

classify_type_ctor(ModuleInfo, TypeCtor) = TypeCategory :-
	PrivateBuiltin = mercury_private_builtin_module,
	( TypeCtor = unqualified("character") - 0 ->
		TypeCategory = char_type
	; TypeCtor = unqualified("int") - 0 ->
		TypeCategory = int_type
	; TypeCtor = unqualified("float") - 0 ->
		TypeCategory = float_type
	; TypeCtor = unqualified("string") - 0 ->
		TypeCategory = str_type
	; TypeCtor = unqualified("void") - 0 ->
		TypeCategory = void_type
	; TypeCtor = qualified(PrivateBuiltin, "type_info") - 1 ->
		TypeCategory = type_info_type
	; TypeCtor = qualified(PrivateBuiltin, "type_ctor_info") - 1 ->
		TypeCategory = type_ctor_info_type
	; TypeCtor = qualified(PrivateBuiltin, "typeclass_info") - 1 ->
		TypeCategory = typeclass_info_type
	; TypeCtor = qualified(PrivateBuiltin, "base_typeclass_info") - 1 ->
		TypeCategory = base_typeclass_info_type
	; TypeCtor = qualified(PrivateBuiltin, "zero_type_info") - 0 ->
		TypeCategory = type_info_type
	; TypeCtor = qualified(PrivateBuiltin, "zero_type_ctor_info") - 0 ->
		TypeCategory = type_ctor_info_type
	; TypeCtor = qualified(PrivateBuiltin, "zero_typeclass_info") - 0 ->
		TypeCategory = typeclass_info_type
	; TypeCtor = qualified(PrivateBuiltin, "zero_base_typeclass_info") - 0 ->
		TypeCategory = base_typeclass_info_type
	; type_ctor_is_higher_order(TypeCtor, _, _, _) ->
		TypeCategory = higher_order_type
	; type_ctor_is_tuple(TypeCtor) ->
		TypeCategory = tuple_type
	; type_ctor_is_enumeration(TypeCtor, ModuleInfo) ->
		TypeCategory = enum_type
	;
		TypeCategory = user_ctor_type
	).

type_has_user_defined_equality_pred(ModuleInfo, Type, UserEqComp) :-
	type_to_type_defn_body(ModuleInfo, Type, TypeBody),
	type_body_has_user_defined_equality_pred(ModuleInfo, TypeBody,
		UserEqComp).

type_body_has_user_defined_equality_pred(ModuleInfo, TypeBody, UserEqComp) :-
	module_info_globals(ModuleInfo, Globals),
	globals__get_target(Globals, Target),
	(
		TypeBody = du_type(_, _, _, _, _, _),
		(
			TypeBody ^ du_type_is_foreign_type =
				yes(ForeignTypeBody),
			have_foreign_type_for_backend(Target, ForeignTypeBody,
				yes)
		->
			UserEqComp =
			    foreign_type_body_has_user_defined_eq_comp_pred(
				ModuleInfo, ForeignTypeBody)
		;
			TypeBody ^ du_type_usereq = yes(UserEqComp)
		)
	;
		TypeBody = foreign_type(ForeignTypeBody),
		UserEqComp = foreign_type_body_has_user_defined_eq_comp_pred(
					ModuleInfo, ForeignTypeBody)
	;
		TypeBody = solver_type(_SolverTypeDetails, yes(UserEqComp))
	).


type_util__type_is_solver_type(ModuleInfo, Type) :-
	type_to_type_defn_body(ModuleInfo, Type, TypeBody),
	(
		TypeBody = solver_type(_, _)
	;
		TypeBody = abstract_type(solver_type)
	;
		TypeBody = eqv_type(EqvType),
		type_util__type_is_solver_type(ModuleInfo, EqvType)
	).


type_util__type_has_solver_type_details(ModuleInfo, Type, SolverTypeDetails) :-
	type_to_type_defn_body(ModuleInfo, Type, TypeBody),
	type_util__type_body_has_solver_type_details(ModuleInfo, TypeBody,
		SolverTypeDetails).


type_util__type_body_has_solver_type_details(_ModuleInfo,
		solver_type(SolverTypeDetails, _MaybeUserEqComp),
		SolverTypeDetails).
type_util__type_body_has_solver_type_details( ModuleInfo,
		eqv_type(Type),
		SolverTypeDetails) :-
	type_util__type_has_solver_type_details(ModuleInfo, Type,
		SolverTypeDetails).


type_util__type_to_type_defn(ModuleInfo, Type, TypeDefn) :-
	module_info_types(ModuleInfo, TypeTable),
	type_to_ctor_and_args(Type, TypeCtor, _TypeArgs),
	map__search(TypeTable, TypeCtor, TypeDefn).


type_util__type_to_type_defn_body(ModuleInfo, Type, TypeBody) :-
	type_to_type_defn(ModuleInfo, Type, TypeDefn),
	hlds_data__get_type_defn_body(TypeDefn, TypeBody).


	% XXX We can't assume that type variables refer to solver types
	% because otherwise the compiler will try to construct initialisation
	% forwarding predicates for exported abstract types defined to be
	% equivalent to a type variable parameter.  This, of course, will
	% lead to the compiler throwing an exception.  The correct solution
	% is to introduce a solver typeclass, but that's something for
	% another day.
	%
type_util__is_solver_type(ModuleInfo, Type) :-
		% type_to_type_defn_body will fail for builtin types such
		% as `int/0'.  Such types are not solver types so
		% type_util__is_solver_type fails too.  type_to_type_defn_body
		% also fails for type variables.
	type_to_type_defn_body(ModuleInfo, Type, TypeBody),
	type_body_is_solver_type(ModuleInfo, TypeBody).


	% Succeed if the type body is for a solver type.
type_util__type_body_is_solver_type(ModuleInfo, TypeBody) :-
	(
		TypeBody = solver_type(_, _)
	;
		TypeBody = abstract_type(solver_type)
	;
		TypeBody = eqv_type(Type),
		type_util__is_solver_type(ModuleInfo, Type)
	).

type_util__is_existq_type(Module, Type) :-
	type_constructors(Type, Module, Constructors),
	some [Constructor]
	(
		list.member(Constructor, Constructors),
		Constructor ^ cons_exist \= []
	).

	% Certain types, e.g. io__state and store__store(S),
	% are just dummy types used to ensure logical semantics;
	% there is no need to actually pass them, and so when
	% importing or exporting procedures to/from C, we don't
	% include arguments with these types.

type_util__is_dummy_argument_type(Type) :-
	( type_to_ctor_and_args(Type, TypeCtor, _) ->
		TypeCtor = CtorSymName - TypeArity,
		CtorSymName = qualified(unqualified(ModuleName), TypeName),
		type_util__is_dummy_argument_type_2(ModuleName, TypeName,
			TypeArity)
	;
		fail
	).

:- pred type_util__is_dummy_argument_type_2(string::in, string::in, arity::in)
	is semidet.

% The list of dummy types should be kept in sync with MR_is_dummy_type
% in trace/mercury_trace.c.
%
% XXX should we include aditi:state/0 in this list?
type_util__is_dummy_argument_type_2("io", "state", 0).    % io:state/0
type_util__is_dummy_argument_type_2("store", "store", 1). % store:store/1.

type_util__constructors_are_dummy_argument_type([Ctor]) :-
	Ctor = ctor([], [], qualified(unqualified("io"), "state"), [_]).
type_util__constructors_are_dummy_argument_type([Ctor]) :-
	Ctor = ctor([], [], qualified(unqualified("store"), "store"), [_]).

type_is_io_state(Type) :-
	type_to_ctor_and_args(Type, TypeCtor, []),
	TypeCtor = qualified(unqualified("io"), "state") - 0.

type_is_aditi_state(Type) :-
	type_to_ctor_and_args(Type, TypeCtor, []),
	TypeCtor = qualified(unqualified("aditi"), "state") - 0.

type_util__remove_aditi_state([], [], []).
type_util__remove_aditi_state([], [_|_], _) :-
	error("type_util__remove_aditi_state").
type_util__remove_aditi_state([_|_], [], _) :-
	error("type_util__remove_aditi_state").
type_util__remove_aditi_state([Type | Types], [Arg | Args0], Args) :-
	( type_is_aditi_state(Type) ->
		type_util__remove_aditi_state(Types, Args0, Args)
	;
		type_util__remove_aditi_state(Types, Args0, Args1),
		Args = [Arg | Args1]
	).

:- pred type_ctor_is_enumeration(type_ctor::in, module_info::in) is semidet.

type_ctor_is_enumeration(TypeCtor, ModuleInfo) :-
	module_info_types(ModuleInfo, TypeDefnTable),
	map__search(TypeDefnTable, TypeCtor, TypeDefn),
	hlds_data__get_type_defn_body(TypeDefn, TypeBody),
	TypeBody ^ du_type_is_enum = yes.

int_type = builtin(int).

string_type = builtin(string).

float_type = builtin(float).

char_type = builtin(character).

void_type = defined(unqualified("void"), [], star).

c_pointer_type = defined(Name, [], star) :-
	mercury_public_builtin_module(BuiltinModule),
	Name = qualified(BuiltinModule, "c_pointer").

heap_pointer_type = defined(Name, [], star) :-
	mercury_private_builtin_module(BuiltinModule),
	Name = qualified(BuiltinModule, "heap_pointer").

sample_type_info_type = defined(Name, [], star) :-
	mercury_private_builtin_module(BuiltinModule),
	Name = qualified(BuiltinModule, "sample_type_info").

sample_typeclass_info_type = defined(Name, [], star) :-
	mercury_private_builtin_module(BuiltinModule),
	Name = qualified(BuiltinModule, "sample_typeclass_info").

comparison_result_type = defined(Name, [], star) :-
	mercury_public_builtin_module(BuiltinModule),
	Name = qualified(BuiltinModule, "comparison_result").

type_info_type = defined(Name, [], star) :-
	mercury_private_builtin_module(BuiltinModule),
	Name = qualified(BuiltinModule, "zero_type_info").

type_ctor_info_type = defined(Name, [], star) :-
	mercury_private_builtin_module(BuiltinModule),
	Name = qualified(BuiltinModule, "zero_type_ctor_info").

aditi_state_type = defined(Name, [], star) :-
	aditi_public_builtin_module(BuiltinModule),
	Name = qualified(BuiltinModule, "state").

%-----------------------------------------------------------------------------%

	% Given a constant and an arity, return a type_ctor.
	% This really ought to take a name and an arity -
	% use of integers/floats/strings as type names should
	% be rejected by the parser in prog_io.m, not in module_qual.m.

make_type_ctor(term__atom(Name), Arity, unqualified(Name) - Arity).

%-----------------------------------------------------------------------------%

	% If the type is a du type, return the list of its constructors.

type_constructors(Type, ModuleInfo, Constructors) :-
	type_to_ctor_and_args(Type, TypeCtor, TypeArgs),
	( type_ctor_is_tuple(TypeCtor) ->
		% Tuples are never existentially typed.
		ExistQVars = [],
		ClassConstraints = [],
		CtorArgs = list__map((func(ArgType) = no - ArgType), TypeArgs),
		Constructors = [ctor(ExistQVars, ClassConstraints,
				unqualified("{}"), CtorArgs)]
	;
		module_info_types(ModuleInfo, TypeTable),
		map__search(TypeTable, TypeCtor, TypeDefn),
		hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),
		hlds_data__get_type_defn_body(TypeDefn, TypeBody),
		substitute_type_args(TypeParams, TypeArgs,
			TypeBody ^ du_type_ctors, Constructors)
	).

%-----------------------------------------------------------------------------%

type_util__switch_type_num_functors(ModuleInfo, Type, NumFunctors) :-
	type_to_ctor_and_args(Type, TypeCtor, _),
	( TypeCtor = unqualified("character") - 0 ->
		% XXX the following code uses the source machine's character
		% size, not the target's, so it won't work if cross-compiling
		% to a machine with a different size character.
		char__max_char_value(MaxChar),
		char__min_char_value(MinChar),
		NumFunctors = MaxChar - MinChar + 1
	; type_ctor_is_tuple(TypeCtor) ->
		NumFunctors = 1
	;
		module_info_types(ModuleInfo, TypeTable),
		map__search(TypeTable, TypeCtor, TypeDefn),
		hlds_data__get_type_defn_body(TypeDefn, TypeBody),
		map__count(TypeBody ^ du_type_cons_tag_values, NumFunctors)
	).

%-----------------------------------------------------------------------------%

type_util__get_cons_id_arg_types(ModuleInfo, Type, ConsId, ArgTypes) :-
	type_util__get_cons_id_arg_types_2(abort_on_exist_qvar,
		ModuleInfo, Type, ConsId, ArgTypes).

type_util__get_cons_id_non_existential_arg_types(ModuleInfo, Type, ConsId,
		ArgTypes) :-
	type_util__get_cons_id_arg_types_2(fail_on_exist_qvar,
		ModuleInfo, Type, ConsId, ArgTypes).

:- type exist_qvar_action
	--->	fail_on_exist_qvar
	;	abort_on_exist_qvar.

:- pred type_util__get_cons_id_arg_types_2(exist_qvar_action,
	module_info, (type), cons_id, list(type)).
:- mode type_util__get_cons_id_arg_types_2(in(bound(fail_on_exist_qvar)),
	in, in, in, out) is semidet.
:- mode type_util__get_cons_id_arg_types_2(in(bound(abort_on_exist_qvar)),
	in, in, in, out) is det.

type_util__get_cons_id_arg_types_2(EQVarAction, ModuleInfo, VarType, ConsId,
		ArgTypes) :-
	(
		type_to_ctor_and_args(VarType, TypeCtor, TypeArgs)
	->
		(
			% The argument types of a tuple cons_id are the
			% arguments of the tuple type.
			type_ctor_is_tuple(TypeCtor)
		->
			ArgTypes = TypeArgs
		;
			type_util__do_get_type_and_cons_defn(ModuleInfo,
				TypeCtor, ConsId, TypeDefn, ConsDefn),
			ConsDefn = hlds_cons_defn(ExistQVars0, _Constraints0,
				Args, _, _),
			Args \= []
		->
			hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),

			% XXX handle ExistQVars
			(
				ExistQVars0 = []
			;
				ExistQVars0 = [_ | _],
				(
					EQVarAction = abort_on_exist_qvar,
					error("get_cons_id_arg_types: " ++
						"existentially typed cons_id")
				;
					EQVarAction = fail_on_exist_qvar,
					fail
				)
			),

			map__from_corresponding_lists(TypeParams, TypeArgs,
				TSubst),
			assoc_list__values(Args, ArgTypes0),
			apply_subst_to_type_list(TSubst, ArgTypes0, ArgTypes)
		;
			ArgTypes = []
		)
	;
		ArgTypes = []
	).

type_util__cons_id_arg_types(ModuleInfo, VarType, ConsId, ArgTypes) :-
	type_to_ctor_and_args(VarType, TypeCtor, TypeArgs),
	module_info_types(ModuleInfo, Types),
	map__search(Types, TypeCtor, TypeDefn),
	hlds_data__get_type_defn_body(TypeDefn, TypeDefnBody),
	map__member(TypeDefnBody ^ du_type_cons_tag_values, ConsId, _),

	module_info_ctors(ModuleInfo, Ctors),
	map__lookup(Ctors, ConsId, ConsDefns),
	list__member(ConsDefn, ConsDefns),

	ConsDefn = hlds_cons_defn(ExistQVars0, _, Args, TypeCtor, _),

	% XXX handle ExistQVars
	ExistQVars0 = [],

	hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),

	map__from_corresponding_lists(TypeParams, TypeArgs, TSubst),
	assoc_list__values(Args, ArgTypes0),
	apply_subst_to_type_list(TSubst, ArgTypes0, ArgTypes).

type_util__is_existq_cons(ModuleInfo, VarType, ConsId) :-
	type_util__is_existq_cons(ModuleInfo, VarType, ConsId, _).

:- pred type_util__is_existq_cons(module_info::in,
		(type)::in, cons_id::in, hlds_cons_defn::out) is semidet.

type_util__is_existq_cons(ModuleInfo, VarType, ConsId, ConsDefn) :-
	type_to_ctor_and_args(VarType, TypeCtor, _),
	type_util__get_cons_defn(ModuleInfo, TypeCtor, ConsId, ConsDefn),
	ConsDefn = hlds_cons_defn(ExistQVars, _, _, _, _),
	ExistQVars \= [].

	% Given a type and a cons_id, look up the definition of that
	% constructor; if it is existentially typed, return its definition,
	% otherwise fail.
type_util__get_existq_cons_defn(ModuleInfo, VarType, ConsId, CtorDefn) :-
	type_util__is_existq_cons(ModuleInfo, VarType, ConsId, ConsDefn),
	ConsDefn = hlds_cons_defn(ExistQVars, Constraints, Args, _, _),
	assoc_list__values(Args, ArgTypes),
	module_info_types(ModuleInfo, Types),
	type_to_ctor_and_args(VarType, TypeCtor, _),
	map__lookup(Types, TypeCtor, TypeDefn),
	hlds_data__get_type_defn_tvarset(TypeDefn, TypeVarSet),
	hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),
	hlds_data__get_type_defn_kind_map(TypeDefn, KindMap),
	prog_type.var_list_to_type_list(KindMap, TypeParams, TypeCtorArgs),
	type_to_ctor_and_args(VarType, TypeCtor, _),
	construct_type(TypeCtor, TypeCtorArgs, RetType),
	CtorDefn = ctor_defn(TypeVarSet, ExistQVars, KindMap, Constraints,
		ArgTypes, RetType).

type_util__get_type_and_cons_defn(ModuleInfo, Type, ConsId,
		TypeDefn, ConsDefn) :-
	(
		type_to_ctor_and_args(Type, TypeCtor, _),
		type_util__do_get_type_and_cons_defn(ModuleInfo,
			TypeCtor, ConsId, TypeDefn0, ConsDefn0)
	->
		TypeDefn = TypeDefn0,
		ConsDefn = ConsDefn0
	;
		error("type_util__get_type_and_cons_defn")
	).

:- pred type_util__do_get_type_and_cons_defn(module_info::in,
		type_ctor::in, cons_id::in, hlds_type_defn::out,
		hlds_cons_defn::out) is semidet.

type_util__do_get_type_and_cons_defn(ModuleInfo, TypeCtor, ConsId,
		TypeDefn, ConsDefn) :-
	type_util__get_cons_defn(ModuleInfo, TypeCtor, ConsId, ConsDefn),
	module_info_types(ModuleInfo, Types),
	map__lookup(Types, TypeCtor, TypeDefn).

type_util__get_cons_defn(ModuleInfo, TypeCtor, ConsId, ConsDefn) :-
	module_info_ctors(ModuleInfo, Ctors),
	% will fail for builtin cons_ids.
	map__search(Ctors, ConsId, ConsDefns),
	MatchingCons = (pred(ThisConsDefn::in) is semidet :-
			ThisConsDefn = hlds_cons_defn(_, _, _, TypeCtor, _)
		),
	list__filter(MatchingCons, ConsDefns, [ConsDefn]).

%-----------------------------------------------------------------------------%

qualify_cons_id(Type, Args, ConsId0, ConsId, InstConsId) :-
	(
		ConsId0 = cons(Name0, OrigArity),
		type_to_ctor_and_args(Type, TypeCtor, _),
		TypeCtor = qualified(TypeModule, _) - _
	->
		unqualify_name(Name0, UnqualName),
		Name = qualified(TypeModule, UnqualName),
		ConsId = cons(Name, OrigArity),
		InstConsId = ConsId
	;
		ConsId0 = type_info_cell_constructor(CellCtor)
	->
		ConsId = ConsId0,
		InstConsId = cell_inst_cons_id(type_info_cell(CellCtor),
			list__length(Args))
	;
		ConsId0 = typeclass_info_cell_constructor
	->
		ConsId = typeclass_info_cell_constructor,
		InstConsId = cell_inst_cons_id(typeclass_info_cell,
			list__length(Args))
	;
		ConsId = ConsId0,
		InstConsId = ConsId
	).

%-----------------------------------------------------------------------------%

type_is_no_tag_type(ModuleInfo, Type, Ctor, ArgType) :-
	type_to_ctor_and_args(Type, TypeCtor, TypeArgs),
	module_info_no_tag_types(ModuleInfo, NoTagTypes),
	map__search(NoTagTypes, TypeCtor, NoTagType),
	NoTagType = no_tag_type(TypeParams, Ctor, ArgType0),
	( TypeParams = [] ->
		ArgType = ArgType0
	;
		map__from_corresponding_lists(TypeParams, TypeArgs, Subn),
		apply_subst_to_type(Subn, ArgType0, ArgType)
	).

type_constructors_are_no_tag_type(Ctors, Ctor, ArgType, MaybeArgName) :-
	type_is_single_ctor_single_arg(Ctors, Ctor, MaybeArgName0, ArgType),

	% We don't handle unary tuples as no_tag types --
	% they are rare enough that it's not worth
	% the implementation effort.
	Ctor \= unqualified("{}"),

	map_maybe(unqualify_name, MaybeArgName0, MaybeArgName).

type_constructors_are_type_info(Ctors) :-
	type_is_single_ctor_single_arg(Ctors, Ctor, _, _),
	ctor_is_type_info(Ctor).

:- pred ctor_is_type_info(sym_name::in) is semidet.

ctor_is_type_info(Ctor) :-
	unqualify_private_builtin(Ctor, Name),
	name_is_type_info(Name).

:- pred name_is_type_info(string::in) is semidet.

name_is_type_info("type_info").
name_is_type_info("type_ctor_info").
name_is_type_info("typeclass_info").
name_is_type_info("base_typeclass_info").
name_is_type_info("zero_type_info").
name_is_type_info("zero_type_ctor_info").
name_is_type_info("zero_typeclass_info").
name_is_type_info("zero_base_typeclass_info").

	% If the sym_name is in the private_builtin module, unqualify it,
	% otherwise fail.
	% All, user-defined types should be module-qualified by the
	% time this predicate is called, so we assume that any
	% unqualified names are in private_builtin.
:- pred unqualify_private_builtin(sym_name::in, string::out) is semidet.

unqualify_private_builtin(unqualified(Name), Name).
unqualify_private_builtin(qualified(ModuleName, Name), Name) :-
	mercury_private_builtin_module(ModuleName).

:- pred type_is_single_ctor_single_arg(list(constructor)::in, sym_name::out,
	maybe(ctor_field_name)::out, (type)::out) is semidet.

type_is_single_ctor_single_arg(Ctors, Ctor, MaybeArgName, ArgType) :-
	Ctors = [SingleCtor],
	SingleCtor = ctor(ExistQVars, _Constraints, Ctor,
		[MaybeArgName - ArgType]),
	ExistQVars = [].

%-----------------------------------------------------------------------------%

	% assign single functor of arity one a `no_tag' tag
	% (unless it is type_info/1 or we are reserving a tag,
	% or if it is one of the dummy types)
type_constructors_should_be_no_tag(Ctors, ReserveTagPragma, Globals,
		SingleFunc, SingleArg, MaybeArgName) :-
	type_constructors_are_no_tag_type(Ctors, SingleFunc, SingleArg,
		MaybeArgName),
	(
		ReserveTagPragma = no,
		globals__lookup_bool_option(Globals, reserve_tag, no),
		globals__lookup_bool_option(Globals, unboxed_no_tag_types, yes)
	;
			% Dummy types always need to be treated as no-tag types
			% as the low-level C back end just passes around
			% rubbish for them. When eg. using the debugger, it is
			% crucial that these values are treated as unboxed
			% c_pointers, not as tagged pointers to c_pointers
			% (otherwise the system winds up following a bogus
			% pointer).
		constructors_are_dummy_argument_type(Ctors)
	).

%-----------------------------------------------------------------------------%

	% Substitute the actual values of the type parameters
	% in list of constructors, for a particular instance of
	% a polymorphic type.

:- pred substitute_type_args(list(type_param)::in, list(type)::in,
	list(constructor)::in, list(constructor)::out) is det.

substitute_type_args(TypeParams, TypeArgs, Constructors0, Constructors) :-
	( TypeParams = [] ->
		Constructors = Constructors0
	;
		map__from_corresponding_lists(TypeParams, TypeArgs, Subst),
		substitute_type_args_2(Subst, Constructors0, Constructors)
	).

:- pred substitute_type_args_2(tsubst::in, list(constructor)::in,
	list(constructor)::out) is det.

substitute_type_args_2(_, [], []).
substitute_type_args_2(Subst, [Ctor0 | Ctors0], [Ctor | Ctors]) :-
	% Note: prog_io.m ensures that the existentially quantified
	% variables, if any, are distinct from the parameters,
	% and that the (existential) constraints can only contain
	% existentially quantified variables, so there's
	% no need to worry about applying the substitution to
	% ExistQVars or Constraints
	Ctor0 = ctor(ExistQVars, Constraints, Name, Args0),
	substitute_type_args_3(Subst, Args0, Args),
	substitute_type_args_2(Subst, Ctors0, Ctors),
	Ctor = ctor(ExistQVars, Constraints, Name, Args).

:- pred substitute_type_args_3(tsubst::in, list(constructor_arg)::in,
	list(constructor_arg)::out) is det.

substitute_type_args_3(_, [], []).
substitute_type_args_3(Subst, [Name - Arg0 | Args0], [Name - Arg | Args]) :-
	apply_subst_to_type(Subst, Arg0, Arg),
	substitute_type_args_3(Subst, Args0, Args).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Check whether TypesA subsumes TypesB, and if so return
	% a type substitution that will map from TypesA to TypesB.

type_list_subsumes(TypesA, TypesB, TypeSubst) :-
	%
	% TypesA subsumes TypesB iff TypesA can be unified with TypesB
	% without binding any of the type variables in TypesB.
	%
	prog_type__vars_list(TypesB, TypesBVars),
	map__init(TypeSubst0),
	type_unify_list(TypesA, TypesB, TypesBVars, TypeSubst0, TypeSubst).

type_list_subsumes_det(TypesA, TypesB, TypeSubst) :-
	( type_list_subsumes(TypesA, TypesB, TypeSubstPrime) ->
		TypeSubst = TypeSubstPrime
	;
		error("type_list_subsumes_det: type_list_subsumes failed")
	).

arg_type_list_subsumes(TVarSet, ActualArgTypes, CalleeTVarSet, PredKindMap,
		PredExistQVars, PredArgTypes) :-

	%
	% Rename the type variables in the callee's argument types.
	%
	tvarset_merge_renaming(TVarSet, CalleeTVarSet, _TVarSet1, Renaming),
	apply_variable_renaming_to_tvar_kind_map(Renaming, PredKindMap,
		ParentKindMap),
	apply_variable_renaming_to_type_list(Renaming, PredArgTypes,
		ParentArgTypes),
	apply_variable_renaming_to_tvar_list(Renaming, PredExistQVars,
		ParentExistQVars),

	%
	% Check that the types of the candidate predicate/function
	% subsume the actual argument types.
	% [This is the right thing to do even for calls to
	% existentially typed preds, because we're using the
	% type variables from the callee's pred decl (obtained
	% from the pred_info via pred_info_arg_types) not the types
	% inferred from the callee's clauses (and stored in the
	% clauses_info and proc_info) -- the latter
	% might not subsume the actual argument types.]
	%
	type_list_subsumes(ParentArgTypes, ActualArgTypes,
		ParentToActualSubst),

	%
	% Check that the type substitution did not bind any existentially
	% typed variables to non-ground types.
	%
	( ParentExistQVars = [] ->
		% Optimize common case.
		true
	;
		apply_rec_subst_to_tvar_list(ParentKindMap,
			ParentToActualSubst, ParentExistQVars,
			ActualExistQTypes),
		all [T] (list__member(T, ActualExistQTypes) =>
				T = variable(_, _))

		% It might make sense to also check that the type
		% substitution did not bind any existentially typed
		% variables to universally quantified type variables in
		% the caller's argument types.
	).

%-----------------------------------------------------------------------------%
%
% Type unification.
%

type_unify(X, Y, HeadTypeParams, !Bindings) :-
	(
		X = variable(VarX, _)
	->
		type_unify_var(VarX, Y, HeadTypeParams, !Bindings)
	;
		Y = variable(VarY, _)
	->
		type_unify_var(VarY, X, HeadTypeParams, !Bindings)
	;
		type_unify_nonvar(X, Y, HeadTypeParams, !Bindings)
	->
		true
	;
		% Some special cases are not handled above.  We handle them
		% separately here.
		type_unify_special(X, Y, HeadTypeParams, !Bindings)
	).

:- pred type_unify_var(tvar::in, (type)::in, list(tvar)::in,
	tsubst::in, tsubst::out) is semidet.

type_unify_var(VarX, TypeY, HeadTypeParams, !Bindings) :-
	(
		TypeY = variable(VarY, KindY)
	->
		type_unify_var_var(VarX, VarY, KindY, HeadTypeParams,
			!Bindings)
	;
		map.search(!.Bindings, VarX, BindingOfX)
	->
		% VarX has a binding.  Y is not a variable.
		type_unify(BindingOfX, TypeY, HeadTypeParams, !Bindings)
	;
		% VarX has no binding, so bind it to TypeY.
		\+ type_occurs(TypeY, VarX, !.Bindings),
		\+ list.member(VarX, HeadTypeParams),
		svmap.det_insert(VarX, TypeY, !Bindings)
	).

:- pred type_unify_var_var(tvar::in, tvar::in, kind::in, list(tvar)::in,
	tsubst::in, tsubst::out) is semidet.

type_unify_var_var(X, Y, Kind, HeadTypeParams, !Bindings) :-
	(
		list.member(Y, HeadTypeParams)
	->
		type_unify_head_type_param(X, Y, Kind, HeadTypeParams,
			!Bindings)
	;
		list.member(X, HeadTypeParams)
	->
		type_unify_head_type_param(Y, X, Kind, HeadTypeParams,
			!Bindings)
	;
		map.search(!.Bindings, X, BindingOfX)
	->
		(
			map.search(!.Bindings, Y, BindingOfY)
		->
			% Both X and Y already have bindings - just unify the
			% types they are bound to.
			type_unify(BindingOfX, BindingOfY, HeadTypeParams,
				!Bindings)
		;
			% Y hasn't been bound yet.
			apply_rec_subst_to_type(!.Bindings, BindingOfX,
				SubstBindingOfX),
			(
				SubstBindingOfX = variable(Y, _)
			->
				true
			;
				\+ type_occurs(SubstBindingOfX, Y, !.Bindings),
				svmap.det_insert(Y, SubstBindingOfX, !Bindings)
			)
		)
	;
		% Neither X nor Y is a head type param.  X had not been
		% bound yet.
		(
			map.search(!.Bindings, Y, BindingOfY)
		->
			apply_rec_subst_to_type(!.Bindings, BindingOfY,
				SubstBindingOfY),
			(
				SubstBindingOfY = variable(X, _)
			->
				true
			;
				\+ type_occurs(SubstBindingOfY, X, !.Bindings),
				svmap.det_insert(X, SubstBindingOfY, !Bindings)
			)
		;
			% Both X and Y are unbound type variables - bind one
			% to the other.
			(
				X = Y
			->
				true
			;
				svmap.det_insert(X, variable(Y, Kind),
					!Bindings)
			)
		)
	).

:- pred type_unify_head_type_param(tvar::in, tvar::in, kind::in,
	list(tvar)::in, tsubst::in, tsubst::out) is semidet.

type_unify_head_type_param(Var, HeadVar, Kind, HeadTypeParams, !Bindings) :-
	( map.search(!.Bindings, Var, BindingOfVar) ->
		BindingOfVar = variable(Var2, _),
		type_unify_head_type_param(Var2, HeadVar, Kind, HeadTypeParams,
			!Bindings)
	;
		( Var = HeadVar ->
			true
		;
			\+ list.member(Var, HeadTypeParams),
			svmap.det_insert(Var, variable(HeadVar, Kind),
				!Bindings)
		)
	).

	% Unify two types, neither of which are variables.  Two special cases
	% which are not handled here are apply_n types and kinded types.
	% Those are handled below.
	%
:- pred type_unify_nonvar((type)::in, (type)::in, list(tvar)::in,
	tsubst::in, tsubst::out) is semidet.

type_unify_nonvar(defined(SymName, ArgsX, _), defined(SymName, ArgsY, _),
		HeadTypeParams, !Bindings) :-
	% Instead of insisting that the names are equal and the arg lists
	% unify, we should consider attempting to expand equivalence types
	% first.  That would require the type table to be passed in to the
	% unification algorithm, though.
	type_unify_list(ArgsX, ArgsY, HeadTypeParams, !Bindings).
type_unify_nonvar(builtin(BuiltinType), builtin(BuiltinType), _, !Bindings).
type_unify_nonvar(higher_order(ArgsX, no, Purity, EvalMethod),
		higher_order(ArgsY, no, Purity, EvalMethod),
		HeadTypeParams, !Bindings) :-
	type_unify_list(ArgsX, ArgsY, HeadTypeParams, !Bindings).
type_unify_nonvar(higher_order(ArgsX, yes(RetX), Purity, EvalMethod),
		higher_order(ArgsY, yes(RetY), Purity, EvalMethod),
		HeadTypeParams, !Bindings) :-
	type_unify_list(ArgsX, ArgsY, HeadTypeParams, !Bindings),
	type_unify(RetX, RetY, HeadTypeParams, !Bindings).
type_unify_nonvar(tuple(ArgsX, _), tuple(ArgsY, _), HeadTypeParams,
		!Bindings) :-
	type_unify_list(ArgsX, ArgsY, HeadTypeParams, !Bindings).

	% Handle apply_n types and kinded types.
	%
:- pred type_unify_special((type)::in, (type)::in, list(tvar)::in,
	tsubst::in, tsubst::out) is semidet.

type_unify_special(X, Y, HeadTypeParams, !Bindings) :-
	(
		X = apply_n(VarX, ArgsX, _)
	->
		type_unify_apply(Y, VarX, ArgsX, HeadTypeParams, !Bindings)
	;
		Y = apply_n(VarY, ArgsY, _)
	->
		type_unify_apply(X, VarY, ArgsY, HeadTypeParams, !Bindings)
	;
		X = kinded(RawX, _)
	->
		(
			Y = kinded(RawY, _)
		->
			type_unify(RawX, RawY, HeadTypeParams, !Bindings)
		;
			type_unify(RawX, Y, HeadTypeParams, !Bindings)
		)
	;
		Y = kinded(RawY, _)
	->
		type_unify(X, RawY, HeadTypeParams, !Bindings)
	;
		fail
	).

	% The idea here is that we try to strip off arguments from Y starting
	% from the end and unify each with the corresponding argument of X.
	% If we reach an atomic type before the arguments run out then we
	% fail.  If we reach a variable before the arguments run out then we
	% unify it with what remains of the apply_n expression.  If we manage
	% to unify all of the arguments then we unify the apply_n variable
	% with what remains of the other expression.
	%
	% Note that Y is not a variable, since that case would have been
	% caught by type_unify.
	%
:- pred type_unify_apply((type)::in, tvar::in, list(type)::in, list(tvar)::in,
	tsubst::in, tsubst::out) is semidet.

type_unify_apply(defined(NameY, ArgsY0, KindY0), VarX, ArgsX, HeadTypeParams,
		!Bindings) :-
	type_unify_args(ArgsX, ArgsY0, ArgsY, KindY0, KindY, HeadTypeParams,
		!Bindings),
	type_unify_var(VarX, defined(NameY, ArgsY, KindY), HeadTypeParams,
		!Bindings).
type_unify_apply(Type @ builtin(_), VarX, [], HeadTypeParams, !Bindings) :-
	type_unify_var(VarX, Type, HeadTypeParams, !Bindings).
type_unify_apply(Type @ higher_order(_, _, _, _), VarX, [], HeadTypeParams,
		!Bindings) :-
	type_unify_var(VarX, Type, HeadTypeParams, !Bindings).
type_unify_apply(tuple(ArgsY0, KindY0), VarX, ArgsX, HeadTypeParams,
		!Bindings) :-
	type_unify_args(ArgsX, ArgsY0, ArgsY, KindY0, KindY, HeadTypeParams,
		!Bindings),
	type_unify_var(VarX, tuple(ArgsY, KindY), HeadTypeParams, !Bindings).
type_unify_apply(apply_n(VarY, ArgsY0, Kind0), VarX, ArgsX0, HeadTypeParams,
		!Bindings) :-
	list.length(ArgsX0, NArgsX0),
	list.length(ArgsY0, NArgsY0),
	compare(Result, NArgsX0, NArgsY0),
	(
		Result = (<),
		type_unify_args(ArgsX0, ArgsY0, ArgsY, Kind0, Kind,
			HeadTypeParams, !Bindings),
		type_unify_var(VarX, apply_n(VarY, ArgsY, Kind),
			HeadTypeParams, !Bindings)
	;
		Result = (=),
		% We know here that the list of remaining args will be empty.
		type_unify_args(ArgsX0, ArgsY0, _, Kind0, Kind, HeadTypeParams,
			!Bindings),
		type_unify_var_var(VarX, VarY, Kind, HeadTypeParams, !Bindings)
	;
		Result = (>),
		type_unify_args(ArgsY0, ArgsX0, ArgsX, Kind0, Kind,
			HeadTypeParams, !Bindings),
		type_unify_var(VarY, apply_n(VarX, ArgsX, Kind),
			HeadTypeParams, !Bindings)
	).
type_unify_apply(kinded(RawY, _), VarX, ArgsX, HeadTypeParams, !Bindings) :-
	type_unify_apply(RawY, VarX, ArgsX, HeadTypeParams, !Bindings).

:- pred type_unify_args(list(type)::in, list(type)::in, list(type)::out,
	kind::in, kind::out, list(tvar)::in, tsubst::in, tsubst::out)
	is semidet.

type_unify_args(ArgsX, ArgsY0, ArgsY, KindY0, KindY, HeadTypeParams,
		!Bindings) :-
	list.reverse(ArgsX, RevArgsX),
	list.reverse(ArgsY0, RevArgsY0),
	type_unify_rev_args(RevArgsX, RevArgsY0, RevArgsY, KindY0, KindY,
		HeadTypeParams, !Bindings),
	list.reverse(RevArgsY, ArgsY).

:- pred type_unify_rev_args(list(type)::in, list(type)::in, list(type)::out,
	kind::in, kind::out, list(tvar)::in, tsubst::in, tsubst::out)
	is semidet.

type_unify_rev_args([], ArgsY, ArgsY, KindY, KindY, _, !Bindings).
type_unify_rev_args([ArgX | ArgsX], [ArgY0 | ArgsY0], ArgsY, KindY0, KindY,
		HeadTypeParams, !Bindings) :-
	type_unify(ArgX, ArgY0, HeadTypeParams, !Bindings),
	KindY1 = arrow(get_type_kind(ArgY0), KindY0),
	type_unify_rev_args(ArgsX, ArgsY0, ArgsY, KindY1, KindY,
		HeadTypeParams, !Bindings).

type_unify_list([], [], _HeadTypeParams, !Bindings).
type_unify_list([X | Xs], [Y | Ys], HeadTypeParams, !Bindings) :-
	type_unify(X, Y, HeadTypeParams, !Bindings),
	type_unify_list(Xs, Ys, HeadTypeParams, !Bindings).

	% type_occurs(Type, Var, Subst) succeeds iff Type contains Var,
	% perhaps indirectly via the substitution.  (The variable must not
	% be mapped by the substitution.)
	%
:- pred type_occurs((type)::in, tvar::in, tsubst::in) is semidet.

type_occurs(variable(X, _), Y, Bindings) :-
	( X = Y ->
		true
	;
		map.search(Bindings, X, BindingOfX),
		type_occurs(BindingOfX, Y, Bindings)
	).
type_occurs(defined(_, Args, _), Y, Bindings) :-
	type_occurs_list(Args, Y, Bindings).
type_occurs(higher_order(Args, MaybeRet, _, _), Y, Bindings) :-
	(
		type_occurs_list(Args, Y, Bindings)
	;
		MaybeRet = yes(Ret),
		type_occurs(Ret, Y, Bindings)
	).
type_occurs(tuple(Args, _), Y, Bindings) :-
	type_occurs_list(Args, Y, Bindings).
type_occurs(apply_n(X, Args, _), Y, Bindings) :-
	(
		X = Y
	;
		type_occurs_list(Args, Y, Bindings)
	;
		map.search(Bindings, X, BindingOfX),
		type_occurs(BindingOfX, Y, Bindings)
	).
type_occurs(kinded(X, _), Y, Bindings) :-
	type_occurs(X, Y, Bindings).

:- pred type_occurs_list(list(type)::in, tvar::in, tsubst::in) is semidet.

type_occurs_list([X | Xs], Y,  Bindings) :-
	(
		type_occurs(X, Y, Bindings)
	;
		type_occurs_list(Xs, Y, Bindings)
	).

%-----------------------------------------------------------------------------%

apply_subst_to_type_map(Subst, !VarTypes) :-
	map__map_values(apply_subst_to_type_map_2(Subst), !VarTypes).

:- pred apply_subst_to_type_map_2(tsubst::in, prog_var::in,
	(type)::in, (type)::out) is det.

apply_subst_to_type_map_2(Subst, _, !Type) :-
	apply_subst_to_type(Subst, !Type).

apply_rec_subst_to_type_map(Subst, !VarTypes) :-
	map__map_values(apply_rec_subst_to_type_map_2(Subst), !VarTypes).

:- pred apply_rec_subst_to_type_map_2(tsubst::in, prog_var::in,
	(type)::in, (type)::out) is det.

apply_rec_subst_to_type_map_2(Subst, _, !Type) :-
	apply_rec_subst_to_type(Subst, !Type).

apply_variable_renaming_to_type_map(Renaming, !Map) :-
	map__map_values(apply_variable_renaming_to_type_map_2(Renaming), !Map).

:- pred apply_variable_renaming_to_type_map_2(tvar_renaming::in, prog_var::in,
	(type)::in, (type)::out) is det.

apply_variable_renaming_to_type_map_2(Renaming, _, !Type) :-
	apply_variable_renaming_to_type(Renaming, !Type).

%-----------------------------------------------------------------------------%

apply_rec_subst_to_constraints(Subst, !Constraints) :-
	!.Constraints = constraints(Unproven0, Assumed0, Redundant0),
	apply_rec_subst_to_constraint_list(Subst, Unproven0, Unproven),
	apply_rec_subst_to_constraint_list(Subst, Assumed0, Assumed),
	Pred = (pred(_::in, C0::in, C::out) is det :-
		apply_rec_subst_to_constraint_list(Subst, C0, C)
	),
	map.map_values(Pred, Redundant0, Redundant),
	!:Constraints = constraints(Unproven, Assumed, Redundant).

apply_rec_subst_to_constraint_list(Subst, !Constraints) :-
	list__map(apply_rec_subst_to_constraint(Subst), !Constraints).

apply_rec_subst_to_constraint(Subst, !Constraint) :-
	!.Constraint = constraint(Ids, Name, Types0),
	apply_rec_subst_to_type_list(Subst, Types0, Types),
	!:Constraint = constraint(Ids, Name, Types).

apply_subst_to_constraints(Subst, !Constraints) :-
	!.Constraints = constraints(Unproven0, Assumed0, Redundant0),
	apply_subst_to_constraint_list(Subst, Unproven0, Unproven),
	apply_subst_to_constraint_list(Subst, Assumed0, Assumed),
	Pred = (pred(_::in, C0::in, C::out) is det :-
		apply_subst_to_constraint_list(Subst, C0, C)
	),
	map.map_values(Pred, Redundant0, Redundant),
	!:Constraints = constraints(Unproven, Assumed, Redundant).

apply_subst_to_constraint_list(Subst, Constraints0, Constraints) :-
	list__map(apply_subst_to_constraint(Subst), Constraints0, Constraints).

apply_subst_to_constraint(Subst, Constraint0, Constraint) :-
	Constraint0 = constraint(Ids, ClassName, Types0),
	apply_subst_to_type_list(Subst, Types0, Types),
	Constraint  = constraint(Ids, ClassName, Types).

apply_subst_to_constraint_proofs(Subst, Proofs0, Proofs) :-
	map__foldl(apply_subst_to_constraint_proofs_2(Subst), Proofs0,
		map__init, Proofs).

:- pred apply_subst_to_constraint_proofs_2(tsubst::in,
	prog_constraint::in, constraint_proof::in,
	constraint_proof_map::in, constraint_proof_map::out) is det.

apply_subst_to_constraint_proofs_2(Subst, Constraint0, Proof0, Map0, Map) :-
	apply_subst_to_prog_constraint(Subst, Constraint0, Constraint),
	(
		Proof0 = apply_instance(_),
		Proof = Proof0
	;
		Proof0 = superclass(Super0),
		apply_subst_to_prog_constraint(Subst, Super0, Super),
		Proof = superclass(Super)
	),
	map__set(Map0, Constraint, Proof, Map).

apply_rec_subst_to_constraint_proofs(Subst, Proofs0, Proofs) :-
	map__foldl(apply_rec_subst_to_constraint_proofs_2(Subst), Proofs0,
		map__init, Proofs).

:- pred apply_rec_subst_to_constraint_proofs_2(tsubst::in,
	prog_constraint::in, constraint_proof::in,
	constraint_proof_map::in, constraint_proof_map::out) is det.

apply_rec_subst_to_constraint_proofs_2(Subst, Constraint0, Proof0, Map0, Map) :-
	apply_rec_subst_to_prog_constraint(Subst, Constraint0, Constraint),
	(
		Proof0 = apply_instance(_),
		Proof = Proof0
	;
		Proof0 = superclass(Super0),
		apply_rec_subst_to_prog_constraint(Subst, Super0, Super),
		Proof = superclass(Super)
	),
	map__set(Map0, Constraint, Proof, Map).

apply_subst_to_constraint_map(Subst, !ConstraintMap) :-
	map__map_values(apply_subst_to_constraint_map_2(Subst),
		!ConstraintMap).

:- pred apply_subst_to_constraint_map_2(tsubst::in, constraint_id::in,
	prog_constraint::in, prog_constraint::out) is det.

apply_subst_to_constraint_map_2(Subst, _Key, !Value) :-
	apply_subst_to_prog_constraint(Subst, !Value).

apply_rec_subst_to_constraint_map(Subst, !ConstraintMap) :-
	map__map_values(apply_rec_subst_to_constraint_map_2(Subst),
		!ConstraintMap).

:- pred apply_rec_subst_to_constraint_map_2(tsubst::in, constraint_id::in,
	prog_constraint::in, prog_constraint::out) is det.

apply_rec_subst_to_constraint_map_2(Subst, _Key, !Value) :-
	apply_rec_subst_to_prog_constraint(Subst, !Value).

apply_variable_renaming_to_constraints(Renaming, !Constraints) :-
	!.Constraints = constraints(Unproven0, Assumed0, Redundant0),
	apply_variable_renaming_to_constraint_list(Renaming, Unproven0,
		Unproven),
	apply_variable_renaming_to_constraint_list(Renaming, Assumed0,
		Assumed),
	Pred = (pred(_::in, C0::in, C::out) is det :-
		apply_variable_renaming_to_constraint_list(Renaming, C0, C)
	),
	map.map_values(Pred, Redundant0, Redundant),
	!:Constraints = constraints(Unproven, Assumed, Redundant).

apply_variable_renaming_to_constraint_list(Renaming, !Constraints) :-
	list__map(apply_variable_renaming_to_constraint(Renaming),
		!Constraints).

apply_variable_renaming_to_constraint(Renaming, Constraint0, Constraint) :-
	Constraint0 = constraint(Ids, ClassName, ClassArgTypes0),
	apply_variable_renaming_to_type_list(Renaming, ClassArgTypes0,
		ClassArgTypes),
	Constraint = constraint(Ids, ClassName, ClassArgTypes).

apply_variable_renaming_to_constraint_proofs(Renaming, Proofs0, Proofs) :-
	( map__is_empty(Proofs0) ->
		% Optimize the simple case.
		Proofs = Proofs0
	;
		map__keys(Proofs0, Keys0),
		map__values(Proofs0, Values0),
		apply_variable_renaming_to_prog_constraint_list(Renaming,
			Keys0, Keys),
		list__map(rename_constraint_proof(Renaming), Values0, Values),
		map__from_corresponding_lists(Keys, Values, Proofs)
	).

	% Apply a type variable renaming to a class constraint proof.
	%
:- pred rename_constraint_proof(tvar_renaming::in, constraint_proof::in,
	constraint_proof::out) is det.

rename_constraint_proof(_TSubst, apply_instance(Num), apply_instance(Num)).
rename_constraint_proof(TSubst, superclass(ClassConstraint0),
		superclass(ClassConstraint)) :-
	apply_variable_renaming_to_prog_constraint(TSubst, ClassConstraint0,
		ClassConstraint).

apply_variable_renaming_to_constraint_map(Renaming, !ConstraintMap) :-
	map__map_values(apply_variable_renaming_to_constraint_map_2(Renaming),
		!ConstraintMap).

:- pred apply_variable_renaming_to_constraint_map_2(tvar_renaming::in,
	constraint_id::in, prog_constraint::in, prog_constraint::out) is det.

apply_variable_renaming_to_constraint_map_2(Renaming, _Key, !Value) :-
	apply_variable_renaming_to_prog_constraint(Renaming, !Value).

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

cons_id_adjusted_arity(ModuleInfo, Type, ConsId) = AdjustedArity :-
		% figure out the arity of this constructor,
		% _including_ any type-infos or typeclass-infos
		% inserted for existential data types.
	ConsArity = cons_id_arity(ConsId),
	(
		type_util__get_existq_cons_defn(ModuleInfo, Type, ConsId,
			ConsDefn)
	->
		ConsDefn = ctor_defn(_TVarSet, ExistQTVars, _KindMap,
				Constraints, _ArgTypes, _ResultType),
		list__length(Constraints, NumTypeClassInfos),
		constraint_list_get_tvars(Constraints, ConstrainedTVars),
		list__delete_elems(ExistQTVars, ConstrainedTVars,
				UnconstrainedExistQTVars),
		list__length(UnconstrainedExistQTVars, NumTypeInfos),
		AdjustedArity = ConsArity + NumTypeClassInfos + NumTypeInfos
	;
		AdjustedArity = ConsArity
	).

%-----------------------------------------------------------------------------%

maybe_get_cons_id_arg_types(ModuleInfo, MaybeType, ConsId0, Arity,
		MaybeTypes) :-
	( ConsId0 = cons(_SymName, _) ->
		ConsId = ConsId0,
		(
			MaybeType = yes(Type),

			% XXX get_cons_id_non_existential_arg_types will fail
			% for ConsIds with existentially typed arguments.
			get_cons_id_non_existential_arg_types(ModuleInfo, Type,
				ConsId, Types),
			list__length(Types, Arity)
		->
			MaybeTypes = list__map(func(T) = yes(T), Types)
		;
			list__duplicate(Arity, no, MaybeTypes)
		)
	;
		MaybeTypes = []
	).

maybe_get_higher_order_arg_types(MaybeType, Arity, MaybeTypes) :-
	(
		MaybeType = yes(Type),
		type_is_higher_order(Type, _, _, _, Types)
	->
		MaybeTypes = list__map(func(T) = yes(T), Types)
	;
		list__duplicate(Arity, no, MaybeTypes)
	).

%-----------------------------------------------------------------------------%

cell_cons_id(type_info_cell(Ctor)) = type_info_cell_constructor(Ctor).
cell_cons_id(typeclass_info_cell) = typeclass_info_cell_constructor.

cell_inst_cons_id(Which, Arity) = InstConsId :-
	% Soon neither of these function symbols will exist,
	% even with fake arity, but they do not need to.
	(
		Which = type_info_cell(_),
		Symbol = "type_info"
	;
		Which = typeclass_info_cell,
		Symbol = "typeclass_info"
	),
	PrivateBuiltin = mercury_private_builtin_module,
	InstConsId = cons(qualified(PrivateBuiltin, Symbol), Arity).

%-----------------------------------------------------------------------------%

builtin_type_ctors_with_no_hlds_type_defn =
	[ qualified(mercury_public_builtin_module, "int") - 0,
	  qualified(mercury_public_builtin_module, "string") - 0,
	  qualified(mercury_public_builtin_module, "character") - 0,
	  qualified(mercury_public_builtin_module, "float") - 0,
	  qualified(mercury_public_builtin_module, "pred") - 0,
	  qualified(mercury_public_builtin_module, "func") - 0,
	  qualified(mercury_public_builtin_module, "void") - 0,
	  qualified(mercury_public_builtin_module, "tuple") - 0
	].

%-----------------------------------------------------------------------------%
