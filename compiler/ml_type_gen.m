%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_type_gen.m
% Main author: fjh

% MLDS type generation -- convert HLDS types to MLDS.

% For enumerations, we use a Java-style emulation: we convert them
% to classes with a single int member, plus a bunch of static const
% members for the different enumerations consts.
% 
% For discriminated unions, we create an MLDS base class type
% corresponding to the HLDS type, and we also create MLDS
% derived class types corresponding to each of the constructors
% which are defined from the base class type.

%-----------------------------------------------------------------------------%

:- module ml_type_gen.
:- interface.
:- import_module prog_data, hlds_module, hlds_data, mlds.
:- import_module io.

	% Generate MLDS definitions for all the types in the HLDS.
	%
:- pred ml_gen_types(module_info, mlds__defns, io__state, io__state).
:- mode ml_gen_types(in, out, di, uo) is det.

	% Given an HLDS type_id, generate the MLDS class name and arity
	% for the corresponding MLDS type.
	%
:- pred ml_gen_type_name(type_id, mlds__class, arity).
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

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module hlds_pred, prog_data, prog_util, type_util, polymorphism.
:- import_module ml_code_util, error_util.
:- import_module globals, options.

:- import_module bool, int, string, list, map, std_util, term, require.

ml_gen_types(ModuleInfo, MLDS_TypeDefns) -->
	globals__io_lookup_bool_option(highlevel_data, HighLevelData),
	( { HighLevelData = yes } ->
		{ module_info_types(ModuleInfo, TypeTable) },
		{ map__keys(TypeTable, TypeIds) },
		{ list__foldl(ml_gen_type_defn(ModuleInfo, TypeTable),
			TypeIds, [], MLDS_TypeDefns) }
	;
		{ MLDS_TypeDefns = [] }
	).

:- pred ml_gen_type_defn(module_info, type_table, type_id,
		mlds__defns, mlds__defns).
:- mode ml_gen_type_defn(in, in, in, in, out) is det.

ml_gen_type_defn(ModuleInfo, TypeTable, TypeId, MLDS_Defns0, MLDS_Defns) :-
	map__lookup(TypeTable, TypeId, TypeDefn),
	hlds_data__get_type_defn_status(TypeDefn, Status),
	( status_defined_in_this_module(Status, yes) ->
		hlds_data__get_type_defn_body(TypeDefn, TypeBody),
		ml_gen_type_2(TypeBody, ModuleInfo, TypeId, TypeDefn,
			MLDS_Defns0, MLDS_Defns)
	;
		MLDS_Defns = MLDS_Defns0
	).

:- pred ml_gen_type_2(hlds_type_body, module_info, type_id, hlds_type_defn,
		mlds__defns, mlds__defns).
:- mode ml_gen_type_2(in, in, in, in, in, out) is det.

ml_gen_type_2(abstract_type, _, _, _) --> [].
ml_gen_type_2(eqv_type(_EqvType), _, _, _) --> []. % XXX Fixme!
ml_gen_type_2(uu_type(_), _, _, _) -->
	{ error("sorry, undiscriminated union types not implemented") }.
ml_gen_type_2(du_type(Ctors, TagValues, IsEnum, MaybeEqualityPred),
		ModuleInfo, TypeId, TypeDefn) -->
	{ ml_gen_equality_members(MaybeEqualityPred, MaybeEqualityMembers) },
	( { IsEnum = yes } ->
		ml_gen_enum_type(TypeId, TypeDefn, Ctors, TagValues,
			MaybeEqualityMembers)
	;
		ml_gen_du_parent_type(ModuleInfo, TypeId, TypeDefn,
			Ctors, TagValues, MaybeEqualityMembers)
	).

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
:- pred ml_gen_enum_type(type_id, hlds_type_defn, list(constructor),
		cons_tag_values, mlds__defns, mlds__defns, mlds__defns).
:- mode ml_gen_enum_type(in, in, in, in, in, in, out) is det.

ml_gen_enum_type(TypeId, TypeDefn, Ctors, TagValues,
		MaybeEqualityMembers, MLDS_Defns0, MLDS_Defns) :-
	hlds_data__get_type_defn_context(TypeDefn, Context),
	MLDS_Context = mlds__make_context(Context),

	% generate the class name
	ml_gen_type_name(TypeId, qual(_, MLDS_ClassName), MLDS_ClassArity),

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
		mlds__data(mlds__native_int_type, no_initializer)).

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
		mlds__data(mlds__native_int_type, init_obj(ConstValue))).

%-----------------------------------------------------------------------------%
%
% Discriminated union types.
%
% XXX we ought to optimize the case where there is only one alternative.
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
	%		/*
	%		** Derived classes, one for each constructor;
	%		** these are generated as nested classes to
	%		** avoid name clashes.
	%		** These will derive either directly from
	%		** <ClassName> or from <ClassName>::tag_type
	%		** (which in turn derives from <ClassName>),
	%		** depending on whether they need a secondary
	%		** tag.  If all the ctors for a type need a
	%		** secondary tag, we put the secondary tag members
	%		** directly in the base class.
	%		*/
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
	%		static class <ctor2> : public <ClassName>::tag_type {
	%		public:
	%			...
	%		};
	%		...
	%	};
	%
:- pred ml_gen_du_parent_type(module_info, type_id, hlds_type_defn,
		list(constructor), cons_tag_values, mlds__defns,
		mlds__defns, mlds__defns).
:- mode ml_gen_du_parent_type(in, in, in, in, in, in, in, out) is det.

ml_gen_du_parent_type(ModuleInfo, TypeId, TypeDefn, Ctors, TagValues,
		MaybeEqualityMembers, MLDS_Defns0, MLDS_Defns) :-
	hlds_data__get_type_defn_context(TypeDefn, Context),
	MLDS_Context = mlds__make_context(Context),

	% generate the class name
	ml_gen_type_name(TypeId, QualBaseClassName, BaseClassArity),
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
			ml_uses_secondary_tag(TagValues, Ctor, _)
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
		% mlds_to_c.m doesn't support static members.
		%	TagConstMembers = list__condense(list__map(
		% 		ml_gen_tag_constant(Context, TagValues), Ctors)),
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
				ml_uses_secondary_tag(TagValues, Ctor, _)
			))
		->
			TagMembers = TagMembers0,
			TagClassId = BaseClassId
		;
			ml_gen_secondary_tag_class(MLDS_Context,
				BaseClassQualifier, BaseClassId, TagMembers0,
				TagTypeDefn, TagClassId),
			TagMembers = [TagTypeDefn]
		)
	),

	% generate the nested derived classes for the constructors
	list__foldl(ml_gen_du_ctor_type(ModuleInfo, BaseClassId,
		BaseClassQualifier, TagClassId, TypeDefn, TagValues),
		Ctors, [], CtorMembers),

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
		Imports, Inherits, Implements, [], Members)),
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
		mlds__data(mlds__native_int_type, no_initializer)).

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
				init_obj(ConstValue))),
		MLDS_Defns = [MLDS_Defn]
	;
		MLDS_Defns = []
	).

	%
	% Check if this constructor uses a secondary tag,
	% and if so, return the secondary tag value.
	%
ml_uses_secondary_tag(ConsTagValues, Ctor, SecondaryTag) :-
	Ctor = ctor(_ExistQTVars, _Constraints, Name, Args),
	list__length(Args, Arity),
	map__lookup(ConsTagValues, cons(Name, Arity), TagVal),
	TagVal = shared_remote_tag(_PrimaryTag, SecondaryTag).

	%
	% Generate a definition for the class used for the secondary tag
	% type.  This is needed for discriminated unions for which some
	% but not all constructors use secondary tags.
	%
:- pred ml_gen_secondary_tag_class(mlds__context, mlds_module_name,
		mlds__class_id, mlds__defns, mlds__defn, mlds__class_id).
:- mode ml_gen_secondary_tag_class(in, in, in, in, out, out) is det.

ml_gen_secondary_tag_class(MLDS_Context, BaseClassQualifier, BaseClassId, Members,
		MLDS_TypeDefn, SecondaryTagClassId) :-
	% Generate the class name for the secondary tag class.
	% Note: the secondary tag class is nested inside the
	% base class for this type.
	UnqualClassName = "tag_type",
	ClassName = qual(BaseClassQualifier, UnqualClassName),
	ClassArity = 0,
	SecondaryTagClassId = mlds__class_type(ClassName, ClassArity,
		mlds__class),

	% the secondary tag class inherits the base class for this type
	Imports = [],
	Inherits = [BaseClassId],
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
	% Generate a definition for the class corresponding to
	% a constructor of a discriminated union type.
	%
:- pred ml_gen_du_ctor_type(module_info, mlds__class_id, mlds_module_name,
		mlds__class_id, hlds_type_defn, cons_tag_values, constructor,
		mlds__defns, mlds__defns).
:- mode ml_gen_du_ctor_type(in, in, in, in, in, in, in, in, out) is det.

ml_gen_du_ctor_type(ModuleInfo, BaseClassId, BaseClassQualifier,
		SecondaryTagClassId, TypeDefn, ConsTagValues, Ctor,
		MLDS_Defns0, MLDS_Defns) :-
	Ctor = ctor(ExistQTVars, Constraints, CtorName, Args),

	% XXX we should keep a context for the constructor,
	% but we don't, so we just use the context from the type.
	hlds_data__get_type_defn_context(TypeDefn, Context),
	MLDS_Context = mlds__make_context(Context),

	% generate the class name for this constructor
	unqualify_name(CtorName, CtorClassName),
	list__length(Args, CtorArity),

	% number any unnamed fields starting from 1
	ArgNum0 = 1,

	% generate class members for the type_infos and typeclass_infos
	% that hold information about existentially quantified
	% type variables and type class constraints
	( ExistQTVars = [] ->
		% optimize common case
		ExtraMembers = [],
		ArgNum2 = ArgNum0
	;
		list__map_foldl(ml_gen_typeclass_info_member(ModuleInfo,
			Context), Constraints, TypeClassInfoMembers,
			ArgNum0, ArgNum1),
		constraint_list_get_tvars(Constraints, ConstrainedTVars),
		list__delete_elems(ExistQTVars, ConstrainedTVars,
			UnconstrainedTVars),
		list__map_foldl(ml_gen_type_info_member(ModuleInfo, Context),
			UnconstrainedTVars, TypeInfoMembers,
			ArgNum1, ArgNum2),
		list__append(TypeClassInfoMembers, TypeInfoMembers,
			ExtraMembers)
	),

	% generate the class members for the ordinary fields
	% of this constructor
	list__map_foldl(ml_gen_du_ctor_member(ModuleInfo, Context),
		Args, OrdinaryMembers, ArgNum2, _ArgNum3),

	list__append(ExtraMembers, OrdinaryMembers, Members),

	% we inherit either the base class for this type,
	% or the secondary tag class, depending on whether
	% we need a secondary tag
	( ml_uses_secondary_tag(ConsTagValues, Ctor, TagVal) ->
		ParentClassId = SecondaryTagClassId,
		MaybeTagVal = yes(TagVal)
	;
		ParentClassId = BaseClassId,
		MaybeTagVal = no
	),
	Imports = [],
	Inherits = [ParentClassId],
	Implements = [],

	% generate a constructor function to initialize the fields
	%
	CtorClassType = mlds__class_type(qual(BaseClassQualifier, CtorClassName),
			CtorArity, mlds__class),
	CtorClassQualifier = mlds__append_class_qualifier(
			BaseClassQualifier, CtorClassName, CtorArity),
	CtorFunction = gen_constructor_function(BaseClassId, CtorClassType,
		CtorClassQualifier, SecondaryTagClassId, MaybeTagVal, Members,
		MLDS_Context),
	Ctors = [CtorFunction],

	% put it all together
	MLDS_TypeName = type(CtorClassName, CtorArity),
	MLDS_TypeFlags = ml_gen_type_decl_flags,
	MLDS_TypeDefnBody = mlds__class(mlds__class_defn(mlds__class,
		Imports, Inherits, Implements, Ctors, Members)),
	MLDS_TypeDefn = mlds__defn(MLDS_TypeName, MLDS_Context, MLDS_TypeFlags,
		MLDS_TypeDefnBody),
	
	MLDS_Defns = [MLDS_TypeDefn | MLDS_Defns0].

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
	CtorDefn = mlds__defn(export("<constructor>"), Context, CtorFlags, Ctor).

	% Get the name and type from the field definition,
	% for use as a constructor argument name and type.
:- func make_arg(mlds__defn) = pair(mlds__entity_name, mlds__type) is det.
make_arg(mlds__defn(Name, _Context, _Flags, Defn)) = Name - Type :-
	( Defn = data(Type0, _Init) ->
		Type = Type0
	;
		unexpected(this_file, "make_arg: non-data member")
	).

	% Generate "this-><fieldname> = <fieldname>;".
:- func gen_init_field(mlds__class_id, mlds__type, mlds_module_name, mlds__defn)
		= mlds__statement is det.
gen_init_field(BaseClassId, ClassType, ClassQualifier, Member) = Statement :-
	Member = mlds__defn(EntityName, Context, _Flags, Defn),
	( Defn = data(Type0, _Init) ->
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
		unexpected(this_file, "gen_init_tag: class_id should be a class")
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

:- pred ml_gen_du_ctor_member(module_info, prog_context, constructor_arg,
		mlds__defn, int, int).
:- mode ml_gen_du_ctor_member(in, in, in, out, in, out) is det.

ml_gen_du_ctor_member(ModuleInfo, Context, MaybeFieldName - Type, MLDS_Defn,
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
	Defn = data(MLDS_Type, no_initializer),
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
