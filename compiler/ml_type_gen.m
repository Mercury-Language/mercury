%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_type_gen.m
% Main author: fjh

% MLDS type generation -- convert HLDS types to MLDS.

% For enumerations, we use a Java-style emulation: we convert them
% to classes with a single int member, plus a bunch of static (one_copy)
% const members for the different enumerations consts.
% 
% For discriminated unions, we create an MLDS base class type
% corresponding to the HLDS type, and we also create MLDS
% derived class types corresponding to each of the constructors
% which are defined from the base class type.
% For constructors which are represented as the addresses of
% specially reserved objects, we generate the static (one_copy)
% members for those objects.

%-----------------------------------------------------------------------------%

:- module ml_backend__ml_type_gen.
:- interface.
:- import_module parse_tree__prog_data, hlds__hlds_module, hlds__hlds_data.
:- import_module ml_backend__mlds.
:- import_module io.

	% Generate MLDS definitions for all the types in the HLDS.
	%
:- pred ml_gen_types(module_info, mlds__defns, io__state, io__state).
:- mode ml_gen_types(in, out, di, uo) is det.

	% Given an HLDS type_ctor, generate the MLDS class name and arity
	% for the corresponding MLDS type.
	%
:- pred ml_gen_type_name(type_ctor, mlds__class, arity).
:- mode ml_gen_type_name(in, out, out) is det.

	% Return the declaration flags appropriate for a type.
	%
:- func ml_gen_type_decl_flags = mlds__decl_flags.

	% Return the declaration flags appropriate for an enumeration constant.
	%
:- func ml_gen_enum_constant_decl_flags = mlds__decl_flags.
	
	% Return the declaration flags appropriate for a member variable.
	%
:- func ml_gen_member_decl_flags = mlds__decl_flags.

	% Return the declaration flags appropriate for a member of a class
	% that was transformed from a special predicate.  These differ 
	% from normal members in that their finality is `final'.
	%
:- func ml_gen_special_member_decl_flags = mlds__decl_flags.

	%
	% ml_uses_secondary_tag(ConsTagValues, Ctor, SecondaryTag):
	% Check if this constructor uses a secondary tag,
	% and if so, return the secondary tag value.
	%
:- pred ml_uses_secondary_tag(cons_tag_values, constructor, int).
:- mode ml_uses_secondary_tag(in, in, out) is semidet.

% A constructor is represented using the base class rather than a derived
% class if there is only a single functor, or if there is a single
% functor and some constants represented using reserved addresses.
:- pred ml_tag_uses_base_class(cons_tag::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module hlds__hlds_pred, parse_tree__prog_data.
:- import_module parse_tree__prog_util, check_hlds__type_util.
:- import_module check_hlds__polymorphism.
:- import_module ml_backend__ml_code_util, hlds__error_util.
:- import_module libs__globals, libs__options.

:- import_module bool, int, string, list, map, std_util, term, require.

ml_gen_types(ModuleInfo, MLDS_TypeDefns) -->
	globals__io_lookup_bool_option(highlevel_data, HighLevelData),
	( { HighLevelData = yes } ->
		{ module_info_types(ModuleInfo, TypeTable) },
		{ map__keys(TypeTable, TypeCtors) },
		{ list__foldl(ml_gen_type_defn(ModuleInfo, TypeTable),
			TypeCtors, [], MLDS_TypeDefns) }
	;
		{ MLDS_TypeDefns = [] }
	).

:- pred ml_gen_type_defn(module_info, type_table, type_ctor,
		mlds__defns, mlds__defns).
:- mode ml_gen_type_defn(in, in, in, in, out) is det.

ml_gen_type_defn(ModuleInfo, TypeTable, TypeCtor, MLDS_Defns0, MLDS_Defns) :-
	map__lookup(TypeTable, TypeCtor, TypeDefn),
	hlds_data__get_type_defn_status(TypeDefn, Status),
	( status_defined_in_this_module(Status, yes) ->
		hlds_data__get_type_defn_body(TypeDefn, TypeBody),
		ml_gen_type_2(TypeBody, ModuleInfo, TypeCtor, TypeDefn,
			MLDS_Defns0, MLDS_Defns)
	;
		MLDS_Defns = MLDS_Defns0
	).

:- pred ml_gen_type_2(hlds_type_body, module_info, type_ctor, hlds_type_defn,
		mlds__defns, mlds__defns).
:- mode ml_gen_type_2(in, in, in, in, in, out) is det.

ml_gen_type_2(abstract_type, _, _, _) --> [].
ml_gen_type_2(eqv_type(_EqvType), _, _, _) --> []. % XXX Fixme!
	% For a description of the problems with equivalence types,
	% see our BABEL'01 paper "Compiling Mercury to the .NET CLR".
ml_gen_type_2(du_type(Ctors, TagValues, IsEnum, MaybeEqualityPred),
		ModuleInfo, TypeCtor, TypeDefn) -->
	{ ml_gen_equality_members(MaybeEqualityPred, MaybeEqualityMembers) },
	( { IsEnum = yes } ->
		ml_gen_enum_type(TypeCtor, TypeDefn, Ctors, TagValues,
			MaybeEqualityMembers)
	;
		ml_gen_du_parent_type(ModuleInfo, TypeCtor, TypeDefn,
			Ctors, TagValues, MaybeEqualityMembers)
	).
	% XXX Fixme!  Same issues here as for eqv_type/1.
ml_gen_type_2(foreign_type(_, _, _), _, _, _) --> [].

%-----------------------------------------------------------------------------%
%
% Enumeration types.
%

	%
	% For each enumeration, we generate an MLDS type of the following form:
	%
	%	struct <ClassName> {
	%		static final const int <ctor1> = 0;
	%		static final const int <ctor2> = 1;
	%		...
	%		int value;
	%	};
	%
	% It is marked as an mlds__enum so that the MLDS -> target code
	% generator can treat it specially if need be (e.g. generating
	% a C enum rather than a class).
	%
:- pred ml_gen_enum_type(type_ctor, hlds_type_defn, list(constructor),
		cons_tag_values, mlds__defns, mlds__defns, mlds__defns).
:- mode ml_gen_enum_type(in, in, in, in, in, in, out) is det.

ml_gen_enum_type(TypeCtor, TypeDefn, Ctors, TagValues,
		MaybeEqualityMembers, MLDS_Defns0, MLDS_Defns) :-
	hlds_data__get_type_defn_context(TypeDefn, Context),
	MLDS_Context = mlds__make_context(Context),

	% generate the class name
	ml_gen_type_name(TypeCtor, qual(_, MLDS_ClassName), MLDS_ClassArity),

	% generate the class members
	ValueMember = ml_gen_enum_value_member(Context),
	EnumConstMembers = list__map(ml_gen_enum_constant(Context, TagValues),
		Ctors),
	Members = list__append(MaybeEqualityMembers,
		[ValueMember|EnumConstMembers]),

	% enums don't import or inherit anything
	Imports = [],
	Inherits = [],
	Implements = [],

	% put it all together
	MLDS_TypeName = type(MLDS_ClassName, MLDS_ClassArity),
	MLDS_TypeFlags = ml_gen_type_decl_flags,
	MLDS_TypeDefnBody = mlds__class(mlds__class_defn(mlds__enum,
		Imports, Inherits, Implements, [], Members)),
	MLDS_TypeDefn = mlds__defn(MLDS_TypeName, MLDS_Context, MLDS_TypeFlags,
		MLDS_TypeDefnBody),
	
	MLDS_Defns = [MLDS_TypeDefn | MLDS_Defns0].

:- func ml_gen_enum_value_member(prog_context) = mlds__defn.
ml_gen_enum_value_member(Context) =
	mlds__defn(data(var(mlds__var_name("value", no))),
		mlds__make_context(Context),
		ml_gen_member_decl_flags,
		mlds__data(mlds__native_int_type, no_initializer, no)).

:- func ml_gen_enum_constant(prog_context, cons_tag_values, constructor) =
	mlds__defn.

ml_gen_enum_constant(Context, ConsTagValues, Ctor) = MLDS_Defn :-
	%
	% figure out the value of this enumeration constant
	%
	Ctor = ctor(_ExistQTVars, _Constraints, Name, Args),
	list__length(Args, Arity),
	map__lookup(ConsTagValues, cons(Name, Arity), TagVal),
	( TagVal = int_constant(Int) ->
		ConstValue = const(int_const(Int))
	;
		error("ml_gen_enum_constant: enum constant needs int tag")
	),
	% sanity check
	require(unify(Arity, 0), "ml_gen_enum_constant: arity != []"),

	%
	% generate an MLDS definition for this enumeration constant.
	%
	unqualify_name(Name, UnqualifiedName),
	MLDS_Defn = mlds__defn(data(var(mlds__var_name(UnqualifiedName, no))),
		mlds__make_context(Context),
		ml_gen_enum_constant_decl_flags,
		mlds__data(mlds__native_int_type, init_obj(ConstValue), no)).

%-----------------------------------------------------------------------------%
%
% Discriminated union types.
%

	%
	% For each discriminated union type, we generate an MLDS type of the
	% following form:
	%
	%	static class <ClassName> {
	%	public:
	% #if some_but_not_all_ctors_use_secondary_tag
	%		/* A nested derived class for the secondary tag */
	%		static class tag_type : public <ClassName> {
	%		public:
	% #endif
	% #if some_ctors_use_secondary_tag
	%			int data_tag;
	%   #if 0
	%   /*
	%   ** XXX we don't yet bother with these;
	%   ** mlds_to_c.m doesn't support static members.
	%   */
	%			/* constants used for data_tag */
	%			static const int <ctor1> = 0;
	%			static const int <ctor2> = 1;
	%   #endif
	% #endif
	% #if some_but_not_all_ctors_use_secondary_tag
	%		};
	% #endif
	%		...
	%
	%		/*
	%		** Reserved objects and/or derived classes,
	%		** one for each constructor.
	%		**
	%		** Reserved objects are generated for any constructors
	%		** that use a `reserved_address(reserved_object(...))'
	% 		** representation.
	%		**
	%		** Derived classes are generated for any other
	%		** constructors; these are generated as nested classes
	%		** avoid name clashes.
	%		** These will derive either directly from
	%		** <ClassName> or from <ClassName>::tag_type
	%		** (which in turn derives from <ClassName>),
	%		** depending on whether they need a secondary
	%		** tag.  If all the ctors for a type need a
	%		** secondary tag, we put the secondary tag members
	%		** directly in the base class.
	%		*/
	%		*/
	% #if ctor1_uses_reserved_object
	%		static <ClassName> obj_<ctor1>;		
	% #else
	%		static class <ctor1> : public <ClassName> {
	%		public:
	%			/*
	%			** fields, one for each argument of this
	%			** constructor
	%			*/
	%			MR_Word F1;
	%			MR_Word F2;
	%			...
	%			/*
	%			** A constructor to initialize the fields
	%			*/
	%			<ctor1>(MR_Word F1, MR_Word F2, ...) {
	%				this->F1 = F1;
	%				this->F2 = F2;
	%				...
	%			}
	%		};
	% #endif
	%		static class <ctor2> : public <ClassName>::tag_type {
	%		public:
	%			...
	%		};
	%		...
	%
	%	};
	%
	%
	% If there is only one constructor which is not represented
	% as a reserved_object, then we don't generate a nested derived
	% class for that constructor, instead we just allocate the fields
	% in the base class.
	%
:- pred ml_gen_du_parent_type(module_info, type_ctor, hlds_type_defn,
		list(constructor), cons_tag_values, mlds__defns,
		mlds__defns, mlds__defns).
:- mode ml_gen_du_parent_type(in, in, in, in, in, in, in, out) is det.

ml_gen_du_parent_type(ModuleInfo, TypeCtor, TypeDefn, Ctors, TagValues,
		MaybeEqualityMembers, MLDS_Defns0, MLDS_Defns) :-
	hlds_data__get_type_defn_context(TypeDefn, Context),
	MLDS_Context = mlds__make_context(Context),

	% generate the class name
	ml_gen_type_name(TypeCtor, QualBaseClassName, BaseClassArity),
	BaseClassId = mlds__class_type(QualBaseClassName, BaseClassArity,
		mlds__class),
	QualBaseClassName = qual(BaseClassModuleName, BaseClassName),
	BaseClassQualifier = mlds__append_class_qualifier(
		BaseClassModuleName, BaseClassName, BaseClassArity),

	(
		%
		% If none of the constructors for this type need
		% a secondary tag, then we don't need the
		% members for the secondary tag.
		%
		\+ (some [Ctor] (
			list__member(Ctor, Ctors),
			ml_needs_secondary_tag(TagValues, Ctor)
		))
	->
		TagMembers = [],
		TagClassId = BaseClassId
	;
		%
		% Generate the members for the secondary tag.
		%
		TagDataMember = ml_gen_tag_member("data_tag", Context),
		TagConstMembers = [],
		% XXX we don't yet bother with these;
		% mlds_to_c.m doesn't support static (one_copy) members.
		%	TagConstMembers = list__condense(list__map(
		% 	    ml_gen_tag_constant(Context, TagValues), Ctors)),
		TagMembers0 = [TagDataMember | TagConstMembers],

		%
		% If all the constructors for this type need a
		% secondary tag, then we put the secondary tag members
		% directly in the base class, otherwise we put it in
		% a separate nested derived class.
		%
		(
			(all [Ctor] (
				list__member(Ctor, Ctors)
			=>
				ml_needs_secondary_tag(TagValues, Ctor)
			))
		->
			TagMembers = TagMembers0,
			TagClassId = BaseClassId
		;
			module_info_globals(ModuleInfo, Globals),
			globals__get_target(Globals, Target),
			ml_gen_secondary_tag_class(MLDS_Context,
				BaseClassQualifier, BaseClassId, TagMembers0,
				Target, TagTypeDefn, TagClassId),
			TagMembers = [TagTypeDefn]
		)
	),

	% generate the nested derived classes for the constructors,
	% or static (one_copy) member objects for constructors with
	% reserved_object representations,
	% or fields and a constructor method for the single_functor case.
	list__foldl2(ml_gen_du_ctor_member(ModuleInfo, BaseClassId,
		BaseClassQualifier, TagClassId, TypeDefn, TagValues),
		Ctors, [], CtorMembers, [], BaseClassCtorMethods),

	% the base class doesn't import or inherit anything
	Imports = [],
	Inherits = [],
	Implements = [],

	% put it all together
	Members = list__condense([MaybeEqualityMembers, TagMembers,
		CtorMembers]),
	MLDS_TypeName = type(BaseClassName, BaseClassArity),
	MLDS_TypeFlags = ml_gen_type_decl_flags,
	MLDS_TypeDefnBody = mlds__class(mlds__class_defn(mlds__class,
		Imports, Inherits, Implements, BaseClassCtorMethods, Members)),
	MLDS_TypeDefn = mlds__defn(MLDS_TypeName, MLDS_Context, MLDS_TypeFlags,
		MLDS_TypeDefnBody),
	
	MLDS_Defns = [MLDS_TypeDefn | MLDS_Defns0].

	%
	% Generate the declaration for the field that holds the secondary tag.
	%
:- func ml_gen_tag_member(string, prog_context) = mlds__defn.
ml_gen_tag_member(Name, Context) =
	mlds__defn(data(var(mlds__var_name(Name, no))),
		mlds__make_context(Context),
		ml_gen_member_decl_flags,
		mlds__data(mlds__native_int_type, no_initializer, no)).

:- func ml_gen_tag_constant(prog_context, cons_tag_values, constructor) =
	mlds__defns.

ml_gen_tag_constant(Context, ConsTagValues, Ctor) = MLDS_Defns :-
	%
	% Check if this constructor uses a secondary tag.
	%
	( ml_uses_secondary_tag(ConsTagValues, Ctor, SecondaryTag) ->
		%
		% Generate an MLDS definition for this secondary
		% tag constant.  We do this mainly for readability
		% and interoperability.  Note that we don't do the
		% same thing for primary tags, so this is most
		% useful in the `--tags none' case, where there
		% will be no primary tags.
		%
		Ctor = ctor(_ExistQTVars, _Constraints, Name, _Args),
		unqualify_name(Name, UnqualifiedName),
		ConstValue = const(int_const(SecondaryTag)),
		MLDS_Defn = mlds__defn(data(var(mlds__var_name(
				UnqualifiedName, no))),
			mlds__make_context(Context),
			ml_gen_enum_constant_decl_flags,
			mlds__data(mlds__native_int_type,
				init_obj(ConstValue), no)),
		MLDS_Defns = [MLDS_Defn]
	;
		MLDS_Defns = []
	).

	%
	% Check if this constructor's representation uses a secondary tag,
	% and if so, return the secondary tag value.
	% BEWARE that this is not the same as ml_needs_secondary_tag, below.
	%
ml_uses_secondary_tag(ConsTagValues, Ctor, SecondaryTag) :-
	TagVal = get_tagval(ConsTagValues, Ctor),
	get_secondary_tag(TagVal) = yes(SecondaryTag).

	%
	% Check if this constructor needs a secondary tag.
	% This is true if its representation uses a secondary
	% tag, obviously. But it is also true if its
	% representation is the address of a reserved object;
	% in that case, for some back-ends (e.g. C)
	% we need a field of some kind to ensure
	% that the reserved object had non-zero size,
	% which in turn is needed to ensure that its
	% address is distinct from any other reserved objects
	% for the same type.
	%
:- pred ml_needs_secondary_tag(cons_tag_values, constructor).
:- mode ml_needs_secondary_tag(in, in) is semidet.

ml_needs_secondary_tag(TagValues, Ctor) :-
	TagVal = get_tagval(TagValues, Ctor),
	( get_secondary_tag(TagVal) = yes(_)
	; tagval_is_reserved_addr(TagVal, reserved_object(_, _, _))
	).

	%
	% Check if this constructor is a constant whose
	% value is represented as a reserved address.
	%
:- pred ml_uses_reserved_addr(cons_tag_values, constructor, reserved_address).
:- mode ml_uses_reserved_addr(in, in, out) is semidet.

ml_uses_reserved_addr(ConsTagValues, Ctor, RA) :-
	TagVal = get_tagval(ConsTagValues, Ctor),
	tagval_is_reserved_addr(TagVal, RA).


:- pred tagval_is_reserved_addr(cons_tag::in, reserved_address::out)
	is semidet.

tagval_is_reserved_addr(reserved_address(RA), RA).
tagval_is_reserved_addr(shared_with_reserved_addresses(_, TagVal), RA) :-
	tagval_is_reserved_addr(TagVal, RA).

:- func get_tagval(cons_tag_values, constructor) = cons_tag.

get_tagval(ConsTagValues, Ctor) = TagVal :-
	Ctor = ctor(_ExistQTVars, _Constraints, Name, Args),
	list__length(Args, Arity),
	map__lookup(ConsTagValues, cons(Name, Arity), TagVal).

	%
	% Generate a definition for the class used for the secondary tag
	% type.  This is needed for discriminated unions for which some
	% but not all constructors use secondary tags.
	%
:- pred ml_gen_secondary_tag_class(mlds__context, mlds_module_name,
		mlds__class_id, mlds__defns, compilation_target,
		mlds__defn, mlds__class_id).
:- mode ml_gen_secondary_tag_class(in, in, in, in, in, out, out) is det.

ml_gen_secondary_tag_class(MLDS_Context, BaseClassQualifier, BaseClassId,
		Members, Target, MLDS_TypeDefn, SecondaryTagClassId) :-
	% Generate the class name for the secondary tag class.
	% Note: the secondary tag class is nested inside the
	% base class for this type.
	UnqualClassName = "tag_type",
	ClassName = qual(BaseClassQualifier, UnqualClassName),
	ClassArity = 0,
	SecondaryTagClassId = mlds__class_type(ClassName, ClassArity,
		mlds__class),

	% the secondary tag class inherits the base class for this type,
	% unless we're compiling to C -- in that case, we omit it,
	% since it is empty, and we don't want to include empty base
	% classes when compiling to C.
	Imports = [],
	( target_uses_empty_base_classes(Target) = yes ->
		Inherits = [BaseClassId]
	;
		Inherits = []
	),
	Implements = [],
	Ctors = [],

	% put it all together
	MLDS_TypeName = type(UnqualClassName, ClassArity),
	MLDS_TypeFlags = ml_gen_type_decl_flags,
	MLDS_TypeDefnBody = mlds__class(mlds__class_defn(mlds__class,
		Imports, Inherits, Implements, Ctors, Members)),
	MLDS_TypeDefn = mlds__defn(MLDS_TypeName, MLDS_Context, MLDS_TypeFlags,
		MLDS_TypeDefnBody).
	
	%
	% Generate definitions corresponding to
	% a constructor of a discriminated union type.
	% This will be one of the following:
	% - (in the usual case) a nested derived class definition
	% - (for reserved_object) a one_copy (static) member object
	% - (for the single_functor case) a bunch of fields and
	%   a constructor method.
	%
:- pred ml_gen_du_ctor_member(module_info, mlds__class_id, mlds_module_name,
		mlds__class_id, hlds_type_defn, cons_tag_values, constructor,
		mlds__defns, mlds__defns, mlds__defns, mlds__defns).
:- mode ml_gen_du_ctor_member(in, in, in, in, in, in, in, in, out, in, out)
		is det.

ml_gen_du_ctor_member(ModuleInfo, BaseClassId, BaseClassQualifier,
		SecondaryTagClassId, TypeDefn, ConsTagValues, Ctor,
		MLDS_Members0, MLDS_Members,
		MLDS_CtorMethods0, MLDS_CtorMethods) :-
	Ctor = ctor(ExistQTVars, Constraints, CtorName, Args),

	% XXX we should keep a context for the constructor,
	% but we don't, so we just use the context from the type.
	hlds_data__get_type_defn_context(TypeDefn, Context),
	MLDS_Context = mlds__make_context(Context),

	% generate the class name for this constructor
	unqualify_name(CtorName, UnqualCtorName),
	list__length(Args, CtorArity),

	TagVal = get_tagval(ConsTagValues, Ctor),
	( tagval_is_reserved_addr(TagVal, ReservedAddr) ->
		( ReservedAddr = reserved_object(_, _, _) ->
			%
			% Generate a reserved object for this constructor.
			% Note that we use the SecondaryTagClassId for the
			% type of this reserved object; we can't use the
			% BaseClassId because for some back-ends,
			% we need to ensure that the type used for the
			% reserved object has at least one data member,
			% to make sure that each reserved object gets a
			% distinct address.
			%
			MLDS_ReservedObjName = ml_format_reserved_object_name(
				UnqualCtorName, CtorArity),
			MLDS_ReservedObjDefn = ml_gen_static_const_defn(
				MLDS_ReservedObjName, SecondaryTagClassId,
				public, no_initializer, Context),
			MLDS_Members = [MLDS_ReservedObjDefn | MLDS_Members0]
		;
			% for reserved numeric addresses, we don't need
			% to generate any objects or types
			MLDS_Members = MLDS_Members0
		),
		MLDS_CtorMethods = MLDS_CtorMethods0
	;
		%
		% Generate the members for this constructor
		%

		% number any unnamed fields starting from 1
		ArgNum0 = 1,

		% generate class members for the type_infos and typeclass_infos
		% that hold information about existentially quantified
		% type variables and type class constraints.
		% Note that the order of fields is as follows:
		%	- first typeinfos (for unconstrained type variables)
		%	- then typeclassinfos (for class constraints)
		%	- finally the ordinary members
		( ExistQTVars = [] ->
			% optimize common case
			ExtraMembers = [],
			ArgNum2 = ArgNum0
		;
			constraint_list_get_tvars(Constraints,
				ConstrainedTVars),
			list__delete_elems(ExistQTVars, ConstrainedTVars,
				UnconstrainedTVars),
			list__map_foldl(
				ml_gen_type_info_member(ModuleInfo, Context),
				UnconstrainedTVars, TypeInfoMembers,
				ArgNum0, ArgNum1),
			list__map_foldl(ml_gen_typeclass_info_member(ModuleInfo,
				Context), Constraints, TypeClassInfoMembers,
				ArgNum1, ArgNum2),
			list__append(TypeInfoMembers, TypeClassInfoMembers,
				ExtraMembers)
		),

		% generate the class members for the ordinary fields
		% of this constructor
		list__map_foldl(ml_gen_du_ctor_field(ModuleInfo, Context),
			Args, OrdinaryMembers, ArgNum2, _ArgNum3),

		list__append(ExtraMembers, OrdinaryMembers, Members),

		% generate a constructor function to initialize the
		% fields, if needed (not all back-ends use constructor
		% functions)
		MaybeSecTagVal = get_secondary_tag(TagVal),
		module_info_globals(ModuleInfo, Globals),
		globals__get_target(Globals, Target),
		( target_uses_constructors(Target) = yes ->
			( ml_tag_uses_base_class(TagVal) ->
				CtorClassType = BaseClassId,
				CtorClassQualifier = BaseClassQualifier
			;
				CtorClassType = mlds__class_type(qual(
					BaseClassQualifier, UnqualCtorName),
					CtorArity, mlds__class),
				CtorClassQualifier =
				    mlds__append_class_qualifier(
					BaseClassQualifier, UnqualCtorName,
					CtorArity)
			),
			CtorFunction = gen_constructor_function(
				BaseClassId, CtorClassType, CtorClassQualifier,
				SecondaryTagClassId, MaybeSecTagVal, Members,
				MLDS_Context),
			% If this constructor is going to go in the base class,
			% then we may also need to generate an additional
			% zero-argument constructor, which is used to
			% construct the class that is used for reserved_objects
			(
				TagVal = shared_with_reserved_addresses(RAs,
					single_functor),
				some [RA] (
					list__member(RA, RAs),
					RA = reserved_object(_, _, _)
				),
				Members \= []
			->
				ZeroArgCtor = gen_constructor_function(
					BaseClassId, CtorClassType,
					CtorClassQualifier,
					SecondaryTagClassId, no, [],
					MLDS_Context),
				Ctors = [ZeroArgCtor, CtorFunction]
			;
				Ctors = [CtorFunction]
			)
		;
			Ctors = []
		),

		( ml_tag_uses_base_class(TagVal) ->
			% put the members for this constructor directly
			% in the base class
			MLDS_Members = Members ++ MLDS_Members0,
			MLDS_CtorMethods = Ctors ++ MLDS_CtorMethods0
		;
			%
			% Generate a nested derived class for this constructor,
			% and put the members for this constructor in that
			% class
			%

			% we inherit either the base class for this type,
			% or the secondary tag class, depending on whether
			% we need a secondary tag.  But when targetting C,
			% we want to omit empty base classes.  So if
			% targetting C, don't include any base class if
			% there is no secondary tag.
			( MaybeSecTagVal = yes(_) ->
				Inherits = [SecondaryTagClassId]
			; target_uses_empty_base_classes(Target) = yes ->
				Inherits = [BaseClassId]
			;
				Inherits = []
			),
			Imports = [],
			Implements = [],

			% put it all together
			MLDS_TypeName = type(UnqualCtorName, CtorArity),
			MLDS_TypeFlags = ml_gen_type_decl_flags,
			MLDS_TypeDefnBody = mlds__class(mlds__class_defn(
				mlds__class, Imports, Inherits, Implements,
				Ctors, Members)),
			MLDS_TypeDefn = mlds__defn(MLDS_TypeName, MLDS_Context,
				MLDS_TypeFlags, MLDS_TypeDefnBody),
			MLDS_Members = [MLDS_TypeDefn | MLDS_Members0],
			MLDS_CtorMethods = MLDS_CtorMethods0
		)
	).

% A constructor is represented using the base class rather than a derived
% class if there is only a single functor, or if there is a single
% functor and some constants represented using reserved addresses.
ml_tag_uses_base_class(single_functor).
ml_tag_uses_base_class(shared_with_reserved_addresses(_RAs, Tag)) :-
	ml_tag_uses_base_class(Tag).

:- func target_uses_constructors(compilation_target) = bool.
target_uses_constructors(c)	= no.
target_uses_constructors(il)	= yes.
target_uses_constructors(java)	= yes.
target_uses_constructors(asm)	= no.

:- func target_uses_empty_base_classes(compilation_target) = bool.
target_uses_empty_base_classes(c)	= no.
target_uses_empty_base_classes(il)	= yes.
target_uses_empty_base_classes(java)	= yes.
target_uses_empty_base_classes(asm)	= no.

:- func gen_constructor_function(mlds__class_id, mlds__type, mlds_module_name,
		mlds__class_id, maybe(int), mlds__defns, mlds__context) =
		mlds__defn.
gen_constructor_function(BaseClassId, ClassType, ClassQualifier,
		SecondaryTagClassId, MaybeTag, Members, Context) = CtorDefn :-
	Args = list__map(make_arg, Members),
	ReturnValues = [],

	InitMembers0 = list__map(gen_init_field(BaseClassId,
			ClassType, ClassQualifier), Members),
	(
		MaybeTag = yes(TagVal)
	->
		InitTag = gen_init_tag(ClassType, SecondaryTagClassId, TagVal,
			Context),
		InitMembers = [InitTag | InitMembers0]
	;
		InitMembers = InitMembers0
	),
	
	Stmt = mlds__statement(block([], InitMembers), Context),
	Attributes = [],

	Ctor = mlds__function(no, func_params(Args, ReturnValues),
			defined_here(Stmt), Attributes),
	CtorFlags = init_decl_flags(public, per_instance, non_virtual,
			overridable, modifiable, concrete),

		% Note that the name of constructor is
		% determined by the backend convention.
	CtorDefn = mlds__defn(export("<constructor>"), Context, CtorFlags,
			Ctor).

	% Get the name and type from the field definition,
	% for use as a constructor argument name and type.
:- func make_arg(mlds__defn) = mlds__argument is det.
make_arg(mlds__defn(Name, _Context, _Flags, Defn)) = Arg :-
	( Defn = data(Type, _Init, GC_TraceCode) ->
		Arg = mlds__argument(Name, Type, GC_TraceCode)
	;
		unexpected(this_file, "make_arg: non-data member")
	).

	% Generate "this-><fieldname> = <fieldname>;".
:- func gen_init_field(mlds__class_id, mlds__type, mlds_module_name, mlds__defn)
		= mlds__statement is det.
gen_init_field(BaseClassId, ClassType, ClassQualifier, Member) = Statement :-
	Member = mlds__defn(EntityName, Context, _Flags, Defn),
	( Defn = data(Type0, _Init, _GC_TraceCode) ->
		Type = Type0
	;
		unexpected(this_file, "gen_init_field: non-data member")
	),
	(
		EntityName = data(var(VarName0)),
		VarName0 = mlds__var_name(Name0, no)
	->
		Name = Name0,
		VarName = VarName0
	;
		unexpected(this_file, "gen_init_field: non-var member")
	),
	Param = mlds__lval(mlds__var(qual(ClassQualifier, VarName), Type)),
	Field = mlds__field(yes(0), self(ClassType),
			named_field(qual(ClassQualifier, Name),
				mlds__ptr_type(ClassType)),
				% XXX we should use ClassType rather than
				% BaseClassId here.  But doing so breaks the
				% IL back-end, because then the hack in
				% fixup_class_qualifiers doesn't work.
			Type, BaseClassId),
	Statement = mlds__statement(atomic(assign(Field, Param)), Context).

	% Generate "this->data_tag = <TagVal>;".
:- func gen_init_tag(mlds__type, mlds__class_id, int, mlds__context) =
		mlds__statement is det.
gen_init_tag(ClassType, SecondaryTagClassId, TagVal, Context) = Statement :-
	( SecondaryTagClassId = mlds__class_type(TagClass, TagArity, _) ->
		TagClass = qual(BaseClassQualifier, TagClassName),
		TagClassQualifier = mlds__append_class_qualifier(
				BaseClassQualifier, TagClassName, TagArity)
	;
		unexpected(this_file,
				"gen_init_tag: class_id should be a class")
	),
	Name = "data_tag",
	Type = mlds__native_int_type,
	Val = const(int_const(TagVal)),
	Field = mlds__field(yes(0), self(ClassType),
			named_field(qual(TagClassQualifier, Name),
				mlds__ptr_type(SecondaryTagClassId)),
			Type, ClassType),
	Statement = mlds__statement(atomic(assign(Field, Val)), Context).

:- pred ml_gen_typeclass_info_member(module_info, prog_context,
		class_constraint, mlds__defn, int, int).
:- mode ml_gen_typeclass_info_member(in, in, in, out, in, out) is det.

ml_gen_typeclass_info_member(ModuleInfo, Context, Constraint, MLDS_Defn,
		ArgNum0, ArgNum) :-
	polymorphism__build_typeclass_info_type(Constraint, Type),
	ml_gen_field(ModuleInfo, Context, no, Type, MLDS_Defn, ArgNum0, ArgNum).

:- pred ml_gen_type_info_member(module_info, prog_context, tvar, mlds__defn,
		int, int).
:- mode ml_gen_type_info_member(in, in, in, out, in, out) is det.

ml_gen_type_info_member(ModuleInfo, Context, TypeVar, MLDS_Defn,
		ArgNum0, ArgNum) :-
	polymorphism__build_type_info_type(term__variable(TypeVar), Type),
	ml_gen_field(ModuleInfo, Context, no, Type, MLDS_Defn, ArgNum0, ArgNum).

:- pred ml_gen_du_ctor_field(module_info, prog_context, constructor_arg,
		mlds__defn, int, int).
:- mode ml_gen_du_ctor_field(in, in, in, out, in, out) is det.

ml_gen_du_ctor_field(ModuleInfo, Context, MaybeFieldName - Type, MLDS_Defn,
		ArgNum0, ArgNum) :-
	ml_gen_field(ModuleInfo, Context, MaybeFieldName, Type, MLDS_Defn,
		ArgNum0, ArgNum).

:- pred ml_gen_field(module_info, prog_context, maybe(ctor_field_name),
		prog_type, mlds__defn, int, int).
:- mode ml_gen_field(in, in, in, in, out, in, out) is det.

ml_gen_field(ModuleInfo, Context, MaybeFieldName, Type, MLDS_Defn,
		ArgNum0, ArgNum) :-
	( ml_must_box_field_type(Type, ModuleInfo) ->
		MLDS_Type = mlds__generic_type
	;
		MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type)
	),
	FieldName = ml_gen_field_name(MaybeFieldName, ArgNum0),
	MLDS_Defn = ml_gen_mlds_field_decl(var(mlds__var_name(FieldName, no)),
		MLDS_Type, mlds__make_context(Context)),
	ArgNum = ArgNum0 + 1.


:- func ml_gen_mlds_field_decl(mlds__data_name, mlds__type, mlds__context)
	= mlds__defn.

ml_gen_mlds_field_decl(DataName, MLDS_Type, Context) = MLDS_Defn :- 
	Name = data(DataName),
	% We only need GC tracing code for top-level variables, not for fields
	GC_TraceCode = no,
	Defn = data(MLDS_Type, no_initializer, GC_TraceCode),
	DeclFlags = ml_gen_public_field_decl_flags,
	MLDS_Defn = mlds__defn(Name, Context, DeclFlags, Defn).

%-----------------------------------------------------------------------------%
%
% Miscellaneous helper routines.
%

ml_gen_type_name(Name - Arity, qual(MLDS_Module, TypeName), Arity) :-
	(
		Name = qualified(ModuleName, TypeName)
	;
		% builtin types like `int' may be still unqualified
		% at this point
		Name = unqualified(TypeName),
		mercury_public_builtin_module(ModuleName)
	),
	MLDS_Module = mercury_module_name_to_mlds(ModuleName).

	% For interoperability, we ought to generate an `==' member
	% for types which have a user-defined equality, if the target
	% language supports it (as do e.g. C++, Java).
:- pred ml_gen_equality_members(maybe(sym_name), list(mlds__defn)).
:- mode ml_gen_equality_members(in, out) is det.
ml_gen_equality_members(_, []).  % XXX generation of `==' members
				 % is not yet implemented.

%-----------------------------------------------------------------------------%
%
% Routines for generating declaration flags.
%

ml_gen_type_decl_flags = MLDS_DeclFlags :-
	% XXX are these right?
	Access = public,
	PerInstance = one_copy,
	Virtuality = non_virtual,
	Finality = overridable,
	Constness = modifiable,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).

ml_gen_member_decl_flags = MLDS_DeclFlags :-
	Access = public,
	PerInstance = per_instance,
	Virtuality = non_virtual,
	Finality = overridable,
	Constness = modifiable,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).

ml_gen_enum_constant_decl_flags = MLDS_DeclFlags :-
	Access = public,
	PerInstance = one_copy,
	Virtuality = non_virtual,
	Finality = final,
	Constness = const,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).

ml_gen_special_member_decl_flags = MLDS_DeclFlags :-
	Access = public,
	PerInstance = per_instance,
	Virtuality = non_virtual,
	Finality = final,
	Constness = const,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).


%-----------------------------------------------------------------------------%

:- func this_file = string.
this_file = "ml_type_gen.m".

:- end_module ml_type_gen.

%-----------------------------------------------------------------------------%
