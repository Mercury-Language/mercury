%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% rtti_to_mlds.m: convert RTTI data structures to MLDS.
% Authors: fjh, zs
%
% This module defines routines to convert from the back-end-independent
% RTTI data structures into MLDS definitions.
% The RTTI data structures are used for static data that is used
% for handling RTTI, polymorphism, and typeclasses.
%
%-----------------------------------------------------------------------------%

:- module ml_backend__rtti_to_mlds.
:- interface.
:- import_module hlds__hlds_module, backend_libs__rtti, ml_backend__mlds.
:- import_module list.

	% return a list of MLDS definitions for the given rtti_data list.
:- func rtti_data_list_to_mlds(module_info, list(rtti_data)) = mlds__defns.

:- implementation.
:- import_module parse_tree__prog_data, parse_tree__prog_data.
:- import_module parse_tree__prog_out, parse_tree__prog_util.
:- import_module hlds__hlds_data.
:- import_module check_hlds__type_util.
:- import_module backend_libs__foreign, backend_libs__type_ctor_info.
:- import_module backend_libs__pseudo_type_info.
:- import_module ml_backend__ml_code_util, ml_backend__ml_unify_gen.
:- import_module ml_backend__ml_closure_gen.
:- import_module bool, string, int, list, assoc_list, map.
:- import_module std_util, term, require.

rtti_data_list_to_mlds(ModuleInfo, RttiDatas) = MLDS_Defns :-
	RealRttiDatas = list__filter(real_rtti_data, RttiDatas),
	MLDS_DefnLists0 = list__map(rtti_data_to_mlds(ModuleInfo),
		RealRttiDatas),
	MLDS_Defns0 = list__condense(MLDS_DefnLists0),
	list__filter(mlds_defn_is_potentially_duplicated, MLDS_Defns0,
		MaybeDupDefns0, NoDupDefns),
	list__sort_and_remove_dups(MaybeDupDefns0, MaybeDupDefns),
	MLDS_Defns = list__append(MaybeDupDefns, NoDupDefns).

:- pred mlds_defn_is_potentially_duplicated(mlds__defn::in) is semidet.

mlds_defn_is_potentially_duplicated(MLDS_Defn) :-
	MLDS_Defn = mlds__defn(EntityName, _, _, _),
	EntityName = data(DataName),
	DataName = rtti(_, RttiName),
	( RttiName = type_info(_)
	; RttiName = pseudo_type_info(_)
	).

	% return a list of MLDS definitions for the given rtti_data.
:- func rtti_data_to_mlds(module_info, rtti_data) = mlds__defns.

rtti_data_to_mlds(ModuleInfo, RttiData) = MLDS_Defns :-
	( RttiData = pseudo_type_info(type_var(_)) ->
		% These just get represented as integers,
		% so we don't need to define them.
		% Also rtti_data_to_name/3 does not handle this case.
		MLDS_Defns = []
    	;
		%
		% Generate the name
		%
		(
			RttiData = base_typeclass_info(InstanceModule,
				ClassId, InstanceStr, _)
		->
			RttiName = base_typeclass_info(InstanceModule,
				ClassId, InstanceStr),
			Name = data(base_typeclass_info(ClassId, InstanceStr))
		;
			rtti_data_to_name(RttiData, RttiTypeCtor, RttiName),
			Name = data(rtti(RttiTypeCtor, RttiName))
		),

		gen_init_rtti_data_defn(RttiData, ModuleInfo, Initializer,
			ExtraDefns),
		rtti_entity_name_and_init_to_defn(Name, RttiName, Initializer,
			MLDS_Defn),
		MLDS_Defns = [MLDS_Defn | ExtraDefns]
	).

:- pred rtti_name_and_init_to_defn(rtti_type_ctor::in, rtti_name::in,
	mlds__initializer::in, mlds__defn::out) is det.

rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer, MLDS_Defn) :-
	Name = data(rtti(RttiTypeCtor, RttiName)),
	rtti_entity_name_and_init_to_defn(Name, RttiName, Initializer,
		MLDS_Defn).

:- pred rtti_entity_name_and_init_to_defn(mlds__entity_name::in, rtti_name::in,
	mlds__initializer::in, mlds__defn::out) is det.

rtti_entity_name_and_init_to_defn(Name, RttiName, Initializer, MLDS_Defn) :-
	%
	% Generate the context
	%
	% XXX the rtti_data ought to include a prog_context
	% (the context of the corresponding type or instance
	% definition)
	term__context_init(Context),
	MLDS_Context = mlds__make_context(Context),

	%
	% Generate the declaration flags
	%
	Exported = rtti_name_is_exported(RttiName),
	Flags = rtti_data_decl_flags(Exported),

	% The GC never needs to trace these definitions,
	% because they are static constants, and can point
	% only to other static constants, not to the heap.
	GC_TraceCode = no,

	%
	% Generate the declaration body,
	% i.e. the type and the initializer
	%
	MLDS_Type = rtti_type(RttiName),
	DefnBody = mlds__data(MLDS_Type, Initializer, GC_TraceCode),
	MLDS_Defn = mlds__defn(Name, MLDS_Context, Flags, DefnBody).

	% Return the declaration flags appropriate for an rtti_data.
	% Note that this must be the same as ml_static_const_decl_flags,
	% except for the access, so that ml_decl_is_static_const works.
	%
:- func rtti_data_decl_flags(bool) = mlds__decl_flags.

rtti_data_decl_flags(Exported) = MLDS_DeclFlags :-
	( Exported = yes ->
		Access = public
	;
		Access = private
	),
	PerInstance = one_copy,
	Virtuality = non_virtual,
	Finality = final,
	Constness = const,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).

%-----------------------------------------------------------------------------%

	% Return an MLDS initializer for the given RTTI definition
	% occurring in the given module.
:- pred gen_init_rtti_data_defn(rtti_data::in, module_info::in,
	mlds__initializer::out, list(mlds__defn)::out) is det.
gen_init_rtti_data_defn(RttiData, ModuleInfo, Init, ExtraDefns) :-
	RttiData = base_typeclass_info(_InstanceModule, _ClassId, _InstanceStr,
		BaseTypeClassInfo),
	BaseTypeClassInfo = base_typeclass_info(N1, N2, N3, N4, N5,
		Methods),
	NumExtra = BaseTypeClassInfo ^ num_extra,
	list__map_foldl(gen_init_method(ModuleInfo, NumExtra),
		Methods, MethodInitializers, [], ExtraDefns),
	Init = init_array([
		gen_init_boxed_int(N1),
		gen_init_boxed_int(N2),
		gen_init_boxed_int(N3),
		gen_init_boxed_int(N4),
		gen_init_boxed_int(N5)
		| MethodInitializers
	]).
gen_init_rtti_data_defn(RttiData, ModuleInfo, Init, SubDefns) :-
	RttiData = type_info(TypeInfo), 
	gen_type_info_defn(ModuleInfo, TypeInfo, Init, SubDefns).
gen_init_rtti_data_defn(RttiData, ModuleInfo, Init, SubDefns) :-
	RttiData = pseudo_type_info(PseudoTypeInfo), 
	gen_pseudo_type_info_defn(ModuleInfo, PseudoTypeInfo, Init, SubDefns).

gen_init_rtti_data_defn(RttiData, ModuleInfo, Init, SubDefns) :-
	RttiData = type_ctor_info(TypeCtorData), 
	TypeCtorData = type_ctor_data(Version, TypeModule, TypeName,
		TypeArity, UnifyUniv, CompareUniv, TypeCtorDetails),
	RttiTypeCtor = rtti_type_ctor(TypeModule, TypeName, TypeArity),
	prog_out__sym_name_to_string(TypeModule, TypeModuleName),
	NumPtags = type_ctor_details_num_ptags(TypeCtorDetails),
	NumFunctors = type_ctor_details_num_functors(TypeCtorDetails),
	gen_functors_layout_info(ModuleInfo, RttiTypeCtor, TypeCtorDetails,
		FunctorsInfo, LayoutInfo, SubDefns),
	Init = init_struct([
		gen_init_int(TypeArity),
		gen_init_int(Version),
		gen_init_int(NumPtags),
		gen_init_type_ctor_rep(TypeCtorData),
		gen_init_proc_id_from_univ(ModuleInfo, UnifyUniv),
		gen_init_proc_id_from_univ(ModuleInfo, CompareUniv),
		gen_init_string(TypeModuleName),
		gen_init_string(TypeName),
		% In the C back-end, these two "structs" are actually unions.
		% We need to use `init_struct' here so that the initializers
		% get enclosed in curly braces.
		init_struct([
			FunctorsInfo
		]),
		init_struct([
			LayoutInfo
		]),
		gen_init_int(NumFunctors)
			% These two are commented out while the corresponding
			% fields of the MR_TypeCtorInfo_Struct type are
			% commented out.
		% gen_init_maybe(gen_init_rtti_name(RttiTypeCtor),
		%	MaybeHashCons),
		% gen_init_proc_id_from_univ(ModuleInfo, PrettyprinterProc)
	]).

%-----------------------------------------------------------------------------%

:- pred gen_type_info_defn(module_info::in, rtti_type_info::in,
	mlds__initializer::out, list(mlds__defn)::out) is det.

gen_type_info_defn(_, plain_arity_zero_type_info(_), _, _) :-
	error("gen_type_info_defn: plain_arity_zero_type_info").
gen_type_info_defn(ModuleInfo, plain_type_info(RttiTypeCtor, ArgTypes),
		Init, SubDefns) :-
	ArgRttiDatas = list__map(type_info_to_rtti_data, ArgTypes),
	RealRttiDatas = list__filter(real_rtti_data, ArgRttiDatas),
	SubDefnLists = list__map(rtti_data_to_mlds(ModuleInfo), RealRttiDatas),
	SubDefns = list__condense(SubDefnLists),
	module_info_name(ModuleInfo, ModuleName),
	Init = init_struct([
		gen_init_rtti_name(ModuleName, RttiTypeCtor, type_ctor_info),
		gen_init_cast_rtti_datas_array(mlds__type_info_type,
			ModuleName, ArgRttiDatas)
	]).
gen_type_info_defn(ModuleInfo, var_arity_type_info(VarArityId, ArgTypes),
		Init, SubDefns) :-
	ArgRttiDatas = list__map(type_info_to_rtti_data, ArgTypes),
	RealRttiDatas = list__filter(real_rtti_data, ArgRttiDatas),
	SubDefnLists = list__map(rtti_data_to_mlds(ModuleInfo), RealRttiDatas),
	SubDefns = list__condense(SubDefnLists),
	RttiTypeCtor = var_arity_id_to_rtti_type_ctor(VarArityId),
	module_info_name(ModuleInfo, ModuleName),
	Init = init_struct([
		gen_init_rtti_name(ModuleName, RttiTypeCtor, type_ctor_info),
		gen_init_int(list__length(ArgTypes)),
		gen_init_cast_rtti_datas_array(mlds__type_info_type,
			ModuleName, ArgRttiDatas)
	]).

:- pred gen_pseudo_type_info_defn(module_info::in, rtti_pseudo_type_info::in,
	mlds__initializer::out, list(mlds__defn)::out) is det.

gen_pseudo_type_info_defn(_, plain_arity_zero_pseudo_type_info(_), _, _) :-
	error("gen_pseudo_type_info_defn: plain_arity_zero_pseudo_type_info").
gen_pseudo_type_info_defn(ModuleInfo, PseudoTypeInfo, Init, SubDefns) :-
	PseudoTypeInfo = plain_pseudo_type_info(RttiTypeCtor, ArgTypes),
	ArgRttiDatas = list__map(maybe_pseudo_type_info_to_rtti_data, ArgTypes),
	RealRttiDatas = list__filter(real_rtti_data, ArgRttiDatas),
	SubDefnLists = list__map(rtti_data_to_mlds(ModuleInfo), RealRttiDatas),
	SubDefns = list__condense(SubDefnLists),
	module_info_name(ModuleInfo, ModuleName),
	Init = init_struct([
		gen_init_rtti_name(ModuleName, RttiTypeCtor, type_ctor_info),
		gen_init_cast_rtti_datas_array(mlds__pseudo_type_info_type,
			ModuleName, ArgRttiDatas)
	]).
gen_pseudo_type_info_defn(ModuleInfo, PseudoTypeInfo, Init, SubDefns) :-
	PseudoTypeInfo = var_arity_pseudo_type_info(VarArityId, ArgTypes),
	ArgRttiDatas = list__map(maybe_pseudo_type_info_to_rtti_data, ArgTypes),
	RealRttiDatas = list__filter(real_rtti_data, ArgRttiDatas),
	SubDefnLists = list__map(rtti_data_to_mlds(ModuleInfo), RealRttiDatas),
	SubDefns = list__condense(SubDefnLists),
	RttiTypeCtor = var_arity_id_to_rtti_type_ctor(VarArityId),
	module_info_name(ModuleInfo, ModuleName),
	Init = init_struct([
		gen_init_rtti_name(ModuleName, RttiTypeCtor, type_ctor_info),
		gen_init_int(list__length(ArgTypes)),
		gen_init_cast_rtti_datas_array(mlds__pseudo_type_info_type,
			ModuleName, ArgRttiDatas)
	]).
gen_pseudo_type_info_defn(_, type_var(_), _, _) :-
	error("gen_pseudo_type_info_defn: type_var").

%-----------------------------------------------------------------------------%

:- pred gen_functors_layout_info(module_info::in, rtti_type_ctor::in,
	type_ctor_details::in, mlds__initializer::out, mlds__initializer::out,
	list(mlds__defn)::out) is det.

gen_functors_layout_info(ModuleInfo, RttiTypeCtor, TypeCtorDetails,
		FunctorInit, LayoutInit, Defns) :-
	module_info_name(ModuleInfo, ModuleName),
	(
		TypeCtorDetails = enum(_, EnumFunctors, EnumByValue,
			EnumByName),
		EnumFunctorDescs = list__map(
			gen_enum_functor_desc(ModuleInfo, RttiTypeCtor),
			EnumFunctors),
		ByValueDefn = gen_enum_value_ordered_table(ModuleInfo,
			RttiTypeCtor, EnumByValue),
		ByNameDefn = gen_enum_name_ordered_table(ModuleInfo,
			RttiTypeCtor, EnumByName),
		LayoutInit = gen_init_rtti_name(ModuleName, RttiTypeCtor,
			enum_value_ordered_table),
		FunctorInit = gen_init_rtti_name(ModuleName, RttiTypeCtor,
			enum_name_ordered_table),
		Defns = [ByValueDefn, ByNameDefn | EnumFunctorDescs]
	;
		TypeCtorDetails = du(_, DuFunctors, DuByPtag, DuByName),
		DuFunctorDefnLists = list__map(
			gen_du_functor_desc(ModuleInfo, RttiTypeCtor),
			DuFunctors),
		DuFunctorDefns = list__condense(DuFunctorDefnLists),
		ByPtagDefns = gen_du_ptag_ordered_table(ModuleInfo,
			RttiTypeCtor, DuByPtag),
		ByNameDefn = gen_du_name_ordered_table(ModuleInfo,
			RttiTypeCtor, DuByName),
		LayoutInit = gen_init_rtti_name(ModuleName, RttiTypeCtor,
			du_ptag_ordered_table),
		FunctorInit = gen_init_rtti_name(ModuleName, RttiTypeCtor,
			du_name_ordered_table),
		Defns = [ByNameDefn |
			list__append(ByPtagDefns, DuFunctorDefns)]
	;
		TypeCtorDetails = reserved(_, MaybeResFunctors, ResFunctors,
			DuByPtag, MaybeResByName),
		MaybeResFunctorDefnLists = list__map(
			gen_maybe_res_functor_desc(ModuleInfo, RttiTypeCtor),
			MaybeResFunctors),
		MaybeResFunctorDefns =
			list__condense(MaybeResFunctorDefnLists),
		ByValueDefns = gen_maybe_res_value_ordered_table(ModuleInfo,
			RttiTypeCtor, ResFunctors, DuByPtag),
		ByNameDefn = gen_maybe_res_name_ordered_table(ModuleInfo,
			RttiTypeCtor, MaybeResByName),
		LayoutInit = gen_init_rtti_name(ModuleName, RttiTypeCtor,
			res_value_ordered_table),
		FunctorInit = gen_init_rtti_name(ModuleName, RttiTypeCtor,
			res_name_ordered_table),
		Defns = [ByNameDefn |
			list__append(ByValueDefns, MaybeResFunctorDefns)]
	;
		TypeCtorDetails = notag(_, NotagFunctor),
		Defns = gen_notag_functor_desc(ModuleInfo,
			RttiTypeCtor, NotagFunctor),
		LayoutInit = gen_init_rtti_name(ModuleName, RttiTypeCtor,
			notag_functor_desc),
		FunctorInit = gen_init_rtti_name(ModuleName, RttiTypeCtor,
			notag_functor_desc)
	;
		TypeCtorDetails = eqv(EqvType),
		TypeRttiData = maybe_pseudo_type_info_to_rtti_data(EqvType),
		RealRttiDatas = list__filter(real_rtti_data, [TypeRttiData]),
		DefnsList = list__map(rtti_data_to_mlds(ModuleInfo),
			RealRttiDatas),
		Defns = list__condense(DefnsList),
		LayoutInit = gen_init_cast_rtti_data(
			mlds__pseudo_type_info_type, ModuleName, TypeRttiData),
			% The type is a lie, but a safe one.
		FunctorInit = gen_init_null_pointer(mlds__generic_type)
	;
		TypeCtorDetails = builtin(_),
		error("gen_functors_layout_info: builtin")
	;
		TypeCtorDetails = impl_artifact(_),
		error("gen_functors_layout_info: impl_artifact")
	;
		TypeCtorDetails = foreign,
		Defns = [],
		LayoutInit = gen_init_null_pointer(mlds__generic_type),
		FunctorInit = gen_init_null_pointer(mlds__generic_type)
	).

%-----------------------------------------------------------------------------%

:- func gen_enum_functor_desc(module_info, rtti_type_ctor, enum_functor)
	= mlds__defn.

gen_enum_functor_desc(_ModuleInfo, RttiTypeCtor, EnumFunctor) = MLDS_Defn :-
	EnumFunctor = enum_functor(FunctorName, Ordinal),
	Init = init_struct([
 		gen_init_string(FunctorName),
 		gen_init_int(Ordinal)
 	]),
	RttiName = enum_functor_desc(Ordinal),
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn).

:- func gen_notag_functor_desc(module_info, rtti_type_ctor, notag_functor)
	= list(mlds__defn).

gen_notag_functor_desc(ModuleInfo, RttiTypeCtor, NotagFunctorDesc)
		= MLDS_Defns :-
	NotagFunctorDesc = notag_functor(FunctorName, ArgType, MaybeArgName),
	module_info_name(ModuleInfo, ModuleName),
	ArgTypeRttiData = maybe_pseudo_type_info_to_rtti_data(ArgType),
	Init = init_struct([
		gen_init_string(FunctorName),
		gen_init_cast_rtti_data(mlds__pseudo_type_info_type,
			ModuleName, ArgTypeRttiData),
		gen_init_maybe(ml_string_type, gen_init_string, MaybeArgName)
	]),
	RttiName = notag_functor_desc,
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn),
	RealRttiDatas = list__filter(real_rtti_data, [ArgTypeRttiData]),
	SubDefnsList = list__map(rtti_data_to_mlds(ModuleInfo), RealRttiDatas),
	SubDefns = list__condense(SubDefnsList),
	MLDS_Defns = [MLDS_Defn | SubDefns].

:- func gen_du_functor_desc(module_info, rtti_type_ctor, du_functor)
	= list(mlds__defn).

gen_du_functor_desc(ModuleInfo, RttiTypeCtor, DuFunctor) = MLDS_Defns :-
	DuFunctor = du_functor(FunctorName, Arity, Ordinal, Rep, ArgInfos,
		MaybeExistInfo),
	ArgTypes = list__map(du_arg_info_type, ArgInfos),
	MaybeArgNames = list__map(du_arg_info_name, ArgInfos),
	ArgNames = list__filter_map(project_yes, MaybeArgNames),
	ContainsVarBitVector = compute_contains_var_bit_vector(ArgTypes),
	module_info_name(ModuleInfo, ModuleName),
	(
		ArgInfos = [_ | _],
		ArgTypeDefns = gen_field_types(ModuleInfo, RttiTypeCtor,
			Ordinal, ArgTypes),
		ArgTypeInit = gen_init_rtti_name(ModuleName, RttiTypeCtor,
			field_types(Ordinal))
	;
		ArgInfos = [],
		ArgTypeDefns = [],
		ArgTypeInit = gen_init_null_pointer(
			mlds__rtti_type(field_types(0)))
	),
	(
		ArgNames = [_ | _],
		ArgNameDefn = gen_field_names(ModuleInfo, RttiTypeCtor,
			Ordinal, MaybeArgNames),
		ArgNameDefns = [ArgNameDefn],
		ArgNameInit = gen_init_rtti_name(ModuleName, RttiTypeCtor,
			field_names(Ordinal))
	;
		ArgNames = [],
		ArgNameDefns = [],
		ArgNameInit = gen_init_null_pointer(
			mlds__rtti_type(field_names(0)))
	),
	(
		MaybeExistInfo = yes(ExistInfo),
		ExistInfoDefns = gen_exist_info(ModuleInfo, RttiTypeCtor,
			Ordinal, ExistInfo),
		ExistInfoInit = gen_init_rtti_name(ModuleName, RttiTypeCtor,
			exist_info(Ordinal))
	;
		MaybeExistInfo = no,
		ExistInfoDefns = [],
		ExistInfoInit = gen_init_null_pointer(
			mlds__rtti_type(exist_info(0)))
	),
	SubDefns = list__condense([ArgTypeDefns, ArgNameDefns,
		ExistInfoDefns]),
	(
		Rep = du_ll_rep(Ptag, SectagAndLocn)
	;
		Rep = du_hl_rep(_),
		error("output_du_functor_defn: du_hl_rep")
	),
	(
		SectagAndLocn = sectag_none,
		Locn = sectag_none,
		Stag = -1
	;
		SectagAndLocn = sectag_local(Stag),
		Locn = sectag_local
	;
		SectagAndLocn = sectag_remote(Stag),
		Locn = sectag_remote
	),
	Init = init_struct([
		gen_init_string(FunctorName),
		gen_init_int(Arity),
		gen_init_int(ContainsVarBitVector),
		gen_init_sectag_locn(Locn),
		gen_init_int(Ptag),
		gen_init_int(Stag),
		gen_init_int(Ordinal),
		ArgTypeInit,
		ArgNameInit,
		ExistInfoInit
	]),
	RttiName = du_functor_desc(Ordinal),
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn),
	MLDS_Defns = [MLDS_Defn | SubDefns].

:- func gen_res_addr_functor_desc(module_info, rtti_type_ctor,
	reserved_functor) = mlds__defn.

gen_res_addr_functor_desc(ModuleInfo, RttiTypeCtor, ResFunctor) = MLDS_Defn :-
	ResFunctor = reserved_functor(FunctorName, Ordinal, ReservedAddress),
	Init = init_struct([
		gen_init_string(FunctorName),
		gen_init_int(Ordinal),
		gen_init_reserved_address(ModuleInfo, ReservedAddress)
	]),
	RttiName = res_functor_desc(Ordinal),
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn).

:- func gen_maybe_res_functor_desc(module_info, rtti_type_ctor,
	maybe_reserved_functor) = list(mlds__defn).

gen_maybe_res_functor_desc(ModuleInfo, RttiTypeCtor, MaybeResFunctor)
		= MLDS_Defns :-
	(
		MaybeResFunctor = res_func(ResFunctor),
		MLDS_Defn = gen_res_addr_functor_desc(ModuleInfo, RttiTypeCtor,
			ResFunctor),
		MLDS_Defns = [MLDS_Defn]
	;
		MaybeResFunctor = du_func(DuFunctor),
		MLDS_Defns = gen_du_functor_desc(ModuleInfo, RttiTypeCtor,
			DuFunctor)
	).

%-----------------------------------------------------------------------------%

:- func gen_init_exist_locn(exist_typeinfo_locn) = mlds__initializer.

gen_init_exist_locn(plain_typeinfo(SlotInCell)) =
	init_struct([
		gen_init_int(SlotInCell),
		gen_init_int(-1)
	]).
gen_init_exist_locn(typeinfo_in_tci(SlotInCell, SlotInTci)) =
	init_struct([
		gen_init_int(SlotInCell),
		gen_init_int(SlotInTci)
	]).

:- func gen_exist_locns_array(module_info, rtti_type_ctor, int,
	list(exist_typeinfo_locn)) = mlds__defn.

gen_exist_locns_array(_ModuleInfo, RttiTypeCtor, Ordinal, Locns) = MLDS_Defn :-
 	Init = gen_init_array(gen_init_exist_locn, Locns),
	RttiName = exist_locns(Ordinal),
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn).

:- func gen_exist_info(module_info, rtti_type_ctor, int, exist_info)
	= list(mlds__defn).

gen_exist_info(ModuleInfo, RttiTypeCtor, Ordinal, ExistInfo) = MLDS_Defns :-
	ExistInfo = exist_info(Plain, InTci, Tci, Locns),
	module_info_name(ModuleInfo, ModuleName),
 	Init = init_struct([
 		gen_init_int(Plain),
 		gen_init_int(InTci),
 		gen_init_int(Tci),
 		gen_init_rtti_name(ModuleName, RttiTypeCtor,
			exist_locns(Ordinal))
 	]),
	RttiName = exist_info(Ordinal),
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn),
	Sub_Defn = gen_exist_locns_array(ModuleInfo, RttiTypeCtor, Ordinal,
		Locns),
	MLDS_Defns = [MLDS_Defn, Sub_Defn].

:- func gen_field_names(module_info, rtti_type_ctor, int, list(maybe(string)))
	= mlds__defn.

gen_field_names(_ModuleInfo, RttiTypeCtor, Ordinal, MaybeNames) = MLDS_Defn :-
	StrType = term__functor(term__atom("string"), [], context("", 0)),
	Init = gen_init_array(gen_init_maybe(
			mercury_type(StrType, str_type,
				non_foreign_type(StrType)),
			gen_init_string), MaybeNames),
	RttiName = field_names(Ordinal),
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn).

:- func gen_field_types(module_info, rtti_type_ctor, int,
	list(rtti_maybe_pseudo_type_info_or_self)) = list(mlds__defn).

gen_field_types(ModuleInfo, RttiTypeCtor, Ordinal, Types) = MLDS_Defns :-
	module_info_name(ModuleInfo, ModuleName),
	TypeRttiDatas = list__map(maybe_pseudo_type_info_or_self_to_rtti_data,
		Types),
	Init = gen_init_array(
		gen_init_cast_rtti_data(mlds__pseudo_type_info_type,
		ModuleName), TypeRttiDatas),
	RttiName = field_types(Ordinal),
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn),
	RealRttiDatas = list__filter(real_rtti_data, TypeRttiDatas),
	SubDefnsList = list__map(rtti_data_to_mlds(ModuleInfo), RealRttiDatas),
	SubDefns = list__condense(SubDefnsList),
	MLDS_Defns = [MLDS_Defn | SubDefns].

%-----------------------------------------------------------------------------%

:- func gen_enum_value_ordered_table(module_info, rtti_type_ctor,
	map(int, enum_functor)) = mlds__defn.

gen_enum_value_ordered_table(ModuleInfo, RttiTypeCtor, EnumByValue)
		= MLDS_Defn :-
	map__values(EnumByValue, Functors),
	module_info_name(ModuleInfo, ModuleName),
	FunctorRttiNames = list__map(enum_functor_rtti_name, Functors),
 	Init = gen_init_rtti_names_array(ModuleName, RttiTypeCtor,
		FunctorRttiNames),
	RttiName = enum_value_ordered_table,
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn).

:- func gen_enum_name_ordered_table(module_info, rtti_type_ctor,
	map(string, enum_functor)) = mlds__defn.

gen_enum_name_ordered_table(ModuleInfo, RttiTypeCtor, EnumByName)
		= MLDS_Defn :-
	map__values(EnumByName, Functors),
	module_info_name(ModuleInfo, ModuleName),
	FunctorRttiNames = list__map(enum_functor_rtti_name, Functors),
 	Init = gen_init_rtti_names_array(ModuleName, RttiTypeCtor,
		FunctorRttiNames),
	RttiName = enum_name_ordered_table,
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn).

:- func gen_du_ptag_ordered_table(module_info, rtti_type_ctor,
	map(int, sectag_table)) = list(mlds__defn).

gen_du_ptag_ordered_table(ModuleInfo, RttiTypeCtor, PtagMap) = MLDS_Defns :-
	module_info_name(ModuleInfo, ModuleName),
	map__to_assoc_list(PtagMap, PtagList),
	SubDefns = list__map(
		gen_du_stag_ordered_table(ModuleName, RttiTypeCtor), PtagList),
	( PtagList = [1 - _ | _] ->
			% Output a dummy ptag definition for the 
			% reserved tag first.
		PtagInitPrefix = [init_struct([
			gen_init_int(0),
			gen_init_builtin_const("MR_SECTAG_VARIABLE"),
			gen_init_null_pointer(
				mlds__rtti_type(du_stag_ordered_table(0)))
		])],
		FirstPtag = 1
	; PtagList = [0 - _ | _] ->
		PtagInitPrefix = [],
		FirstPtag = 0
	;
		error("gen_du_ptag_ordered_table: bad ptag list")
	),
	PtagInits = gen_du_ptag_ordered_table_body(ModuleName, RttiTypeCtor,
		PtagList, FirstPtag),
 	Init = init_array(list__append(PtagInitPrefix, PtagInits)),
	RttiName = du_ptag_ordered_table,
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn),
	MLDS_Defns = [MLDS_Defn | SubDefns].

:- func gen_du_ptag_ordered_table_body(module_name, rtti_type_ctor,
	assoc_list(int, sectag_table), int) = list(mlds__initializer).

gen_du_ptag_ordered_table_body(_, _, [], _) = [].
gen_du_ptag_ordered_table_body(ModuleName, RttiTypeCtor,
		[Ptag - SectagTable | PtagTail], CurPtag) = [Init | Inits] :-
	require(unify(Ptag, CurPtag),
		"gen_du_ptag_ordered_table_body: ptag mismatch"),
	SectagTable = sectag_table(SectagLocn, NumSharers, _SectagMap),
	Init = init_struct([
		gen_init_int(NumSharers),
		gen_init_sectag_locn(SectagLocn),
		gen_init_rtti_name(ModuleName, RttiTypeCtor,
			du_stag_ordered_table(Ptag))
	]),
	Inits = gen_du_ptag_ordered_table_body(ModuleName, RttiTypeCtor,
		PtagTail, CurPtag + 1).

:- func gen_du_stag_ordered_table(module_name, rtti_type_ctor,
	pair(int, sectag_table)) = mlds__defn.

gen_du_stag_ordered_table(ModuleName, RttiTypeCtor, Ptag - SectagTable)
		= MLDS_Defn :-
	SectagTable = sectag_table(_SectagLocn, _NumSharers, SectagMap),
	map__values(SectagMap, SectagFunctors),
	FunctorRttiNames = list__map(du_functor_rtti_name, SectagFunctors),
 	Init = gen_init_rtti_names_array(ModuleName, RttiTypeCtor,
		FunctorRttiNames),
	RttiName = du_stag_ordered_table(Ptag),
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn).

:- func gen_du_name_ordered_table(module_info, rtti_type_ctor,
	map(string, map(int, du_functor))) = mlds__defn.

gen_du_name_ordered_table(ModuleInfo, RttiTypeCtor, NameArityMap)
		= MLDS_Defn :-
	map__values(NameArityMap, ArityMaps),
	list__map(map__values, ArityMaps, FunctorLists),
	list__condense(FunctorLists, Functors),
	module_info_name(ModuleInfo, ModuleName),
	FunctorRttiNames = list__map(du_functor_rtti_name, Functors),
 	Init = gen_init_rtti_names_array(ModuleName, RttiTypeCtor,
		FunctorRttiNames),
	RttiName = du_name_ordered_table,
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn).

:- func gen_maybe_res_value_ordered_table(module_info, rtti_type_ctor,
	list(reserved_functor), map(int, sectag_table)) = list(mlds__defn).

gen_maybe_res_value_ordered_table(ModuleInfo, RttiTypeCtor, ResFunctors,
		DuByPtag) = MLDS_Defns :-
	ResFunctorReps = list__map(res_addr_rep, ResFunctors),
	list__filter(res_addr_is_numeric, ResFunctorReps,
		NumericResFunctorReps, SymbolicResFunctorReps),
	list__length(NumericResFunctorReps, NumNumericResFunctorReps),
	list__length(SymbolicResFunctorReps, NumSymbolicResFunctorReps),
	module_info_name(ModuleInfo, ModuleName),
	ResDefns = [gen_res_addr_functor_table(ModuleName, RttiTypeCtor,
		ResFunctors)],
	( NumSymbolicResFunctorReps = 0 ->
		ResAddrDefns = [],
		ResAddrInit = gen_init_null_pointer(mlds__generic_type)
	;
		ResAddrDefns = [gen_res_addrs_list(ModuleInfo, RttiTypeCtor,
			SymbolicResFunctorReps)],
		ResAddrInit = gen_init_rtti_name(ModuleName, RttiTypeCtor,
			res_addrs)
	),
	DuDefns = gen_du_ptag_ordered_table(ModuleInfo, RttiTypeCtor,
		DuByPtag),
	SubDefns = list__condense([ResDefns, ResAddrDefns, DuDefns]),
	Init = init_struct([
		gen_init_int(NumNumericResFunctorReps),
		gen_init_int(NumSymbolicResFunctorReps),
		ResAddrInit,
		gen_init_rtti_name(ModuleName, RttiTypeCtor,
			res_addr_functors),
		gen_init_rtti_name(ModuleName, RttiTypeCtor,
			du_ptag_ordered_table)
	]),
	RttiName = res_value_ordered_table,
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn),
	MLDS_Defns = [MLDS_Defn | SubDefns].

:- func gen_res_addr_functor_table(module_name, rtti_type_ctor,
	list(reserved_functor)) = mlds__defn.

gen_res_addr_functor_table(ModuleName, RttiTypeCtor, ResFunctors) = MLDS_Defn :-
	FunctorRttiNames = list__map(res_functor_rtti_name, ResFunctors),
 	Init = gen_init_rtti_names_array(ModuleName, RttiTypeCtor,
		FunctorRttiNames),
	RttiName = res_addr_functors,
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn).

:- func gen_res_addrs_list(module_info, rtti_type_ctor, list(reserved_address))
	= mlds__defn.

gen_res_addrs_list(ModuleInfo, RttiTypeCtor, ResAddrs) = MLDS_Defn :-
	Init = gen_init_array(gen_init_reserved_address(ModuleInfo), ResAddrs),
	RttiName = res_addrs,
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn).

:- func gen_maybe_res_name_ordered_table(module_info, rtti_type_ctor,
	map(string, map(int, maybe_reserved_functor))) = mlds__defn.

gen_maybe_res_name_ordered_table(ModuleInfo, RttiTypeCtor, NameArityMap)
		= MLDS_Defn :-
	map__values(NameArityMap, ArityMaps),
	list__map(map__values, ArityMaps, FunctorLists),
	list__condense(FunctorLists, Functors),
	module_info_name(ModuleInfo, ModuleName),
 	Init = gen_init_array(
		gen_maybe_res_name_ordered_table_element(ModuleName,
			RttiTypeCtor),
		Functors),
	RttiName = res_name_ordered_table,
	rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Init, MLDS_Defn).

:- func gen_maybe_res_name_ordered_table_element(module_name, rtti_type_ctor,
	maybe_reserved_functor) = mlds__initializer.

gen_maybe_res_name_ordered_table_element(ModuleName, RttiTypeCtor,
		MaybeResFunctor) = Init :-
	(
		MaybeResFunctor = res_func(ResFunctor),
		Name = ResFunctor ^ res_name,
		Init = init_struct([
			gen_init_builtin_const(Name),
			gen_init_int(0),
			gen_init_builtin_const("MR_TRUE"),
			gen_init_rtti_name(ModuleName, RttiTypeCtor,
				maybe_res_functor_rtti_name(MaybeResFunctor))
		])
	;
		MaybeResFunctor = du_func(DuFunctor),
		Name = DuFunctor ^ du_name,
		Init = init_struct([
			gen_init_builtin_const(Name),
			gen_init_int(DuFunctor ^ du_orig_arity),
			gen_init_builtin_const("MR_TRUE"),
			gen_init_rtti_name(ModuleName, RttiTypeCtor,
				maybe_res_functor_rtti_name(MaybeResFunctor))
		])
	).

%-----------------------------------------------------------------------------%

:- func gen_init_rtti_names_array(module_name, rtti_type_ctor,
		list(rtti_name)) = mlds__initializer.
gen_init_rtti_names_array(ModuleName, RttiTypeCtor, RttiNames) =
	gen_init_array(gen_init_rtti_name(ModuleName, RttiTypeCtor), RttiNames).

:- func gen_init_rtti_datas_array(module_name, list(rtti_data)) =
	mlds__initializer.
gen_init_rtti_datas_array(ModuleName, RttiDatas) =
	gen_init_array(gen_init_rtti_data(ModuleName), RttiDatas).

:- func gen_init_cast_rtti_datas_array(mlds__type, module_name,
		list(rtti_data)) = mlds__initializer.
gen_init_cast_rtti_datas_array(Type, ModuleName, RttiDatas) =
	gen_init_array(gen_init_cast_rtti_data(Type, ModuleName), RttiDatas).

	% Generate the MLDS initializer comprising the rtti_name
	% for a given rtti_data, converted to mlds__generic_type.
	% XXX we don't need to pass the module_name down to here
:- func gen_init_cast_rtti_data(mlds__type, module_name, rtti_data) =
	mlds__initializer.

gen_init_cast_rtti_data(DestType, ModuleName, RttiData) = Initializer :-
	(
		RttiData = pseudo_type_info(type_var(VarNum))
	->
		% rtti_data_to_name/3 does not handle this case
		SrcType = mlds__native_int_type,
		Initializer = init_obj(unop(gen_cast(SrcType, DestType),
			const(int_const(VarNum))))
	;
		RttiData = base_typeclass_info(InstanceModuleName, ClassId,
			InstanceString, _)
	->
		% rtti_data_to_name/3 does not handle this case
		SrcType = rtti_type(base_typeclass_info(InstanceModuleName,
			ClassId, InstanceString)),
		MLDS_ModuleName = mercury_module_name_to_mlds(
			InstanceModuleName),
		MLDS_DataName = base_typeclass_info(ClassId, InstanceString),
		DataAddr = data_addr(MLDS_ModuleName, MLDS_DataName),
		Rval = const(data_addr_const(DataAddr)),
		Initializer = init_obj(unop(gen_cast(SrcType, DestType),
			Rval))
	;
		rtti_data_to_name(RttiData, RttiTypeCtor, RttiName),
		Initializer = gen_init_cast_rtti_name(DestType,
			ModuleName, RttiTypeCtor, RttiName)
	).

	% currently casts only store the destination type
:- func gen_cast(mlds__type, mlds__type) = mlds__unary_op.
gen_cast(_SrcType, DestType) = cast(DestType).

	% Generate the MLDS initializer comprising the rtti_name
	% for a given rtti_data.
:- func gen_init_rtti_data(module_name, rtti_data) = mlds__initializer.

gen_init_rtti_data(ModuleName, RttiData) = Initializer :-
	rtti_data_to_name(RttiData, RttiTypeCtor, RttiName),
	Initializer = gen_init_rtti_name(ModuleName, RttiTypeCtor, RttiName).

	% Generate an MLDS initializer comprising just the
	% the rval for a given rtti_name
:- func gen_init_rtti_name(module_name, rtti_type_ctor, rtti_name) =
	mlds__initializer.

gen_init_rtti_name(ModuleName, RttiTypeCtor, RttiName) =
	init_obj(gen_rtti_name(ModuleName, RttiTypeCtor, RttiName)).

	% Generate the MLDS initializer comprising the rtti_name
	% for a given rtti_name, converted to the given type.
:- func gen_init_cast_rtti_name(mlds__type, module_name, rtti_type_ctor,
	rtti_name) = mlds__initializer.

gen_init_cast_rtti_name(DestType, ModuleName, RttiTypeCtor, RttiName) =
		Initializer :-
	SrcType = rtti_type(RttiName), 
	Initializer = init_obj(unop(gen_cast(SrcType, DestType),
		gen_rtti_name(ModuleName, RttiTypeCtor, RttiName))).

	% Generate the MLDS rval for an rtti_name.
:- func gen_rtti_name(module_name, rtti_type_ctor, rtti_name) = mlds__rval.

gen_rtti_name(ThisModuleName, RttiTypeCtor0, RttiName) = Rval :-
	%
	% Typeinfos are defined locally to each module.
	% Other kinds of RTTI data are defining in the module
	% corresponding to the type which they are for.
	%
	(
		(
			RttiName = type_info(TypeInfo),
			( TypeInfo = plain_type_info(_, _)
			; TypeInfo = var_arity_type_info(_, _)
			)
		;
			RttiName = pseudo_type_info(PseudoTypeInfo),
			( PseudoTypeInfo = plain_pseudo_type_info(_, _)
			; PseudoTypeInfo = var_arity_pseudo_type_info(_, _)
			)
		)
	->
		ModuleName = ThisModuleName,
		RttiTypeCtor = RttiTypeCtor0
	;
		RttiTypeCtor0 = rtti_type_ctor(RttiModuleName,
			RttiTypeName, RttiTypeArity),
		%
		% Although the builtin types `int', `float', etc. are treated
		% as part of the `builtin' module, for historical reasons they
		% don't have any qualifiers at this point, so we need to add
		% the `builtin' qualifier now.
		%
		( RttiModuleName = unqualified("") ->
			mercury_public_builtin_module(ModuleName),
			RttiTypeCtor = rtti_type_ctor(RttiModuleName,
				RttiTypeName, RttiTypeArity)
		;
			ModuleName = RttiModuleName,
			RttiTypeCtor = RttiTypeCtor0
		)
	),
	MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
	MLDS_DataName = rtti(RttiTypeCtor, RttiName),
	DataAddr = data_addr(MLDS_ModuleName, MLDS_DataName),
	Rval = const(data_addr_const(DataAddr)).

%-----------------------------------------------------------------------------%

:- pred gen_init_method(module_info, int, rtti_proc_label, mlds__initializer,
		list(mlds__defn), list(mlds__defn)).
:- mode gen_init_method(in, in, in, out, in, out) is det.

gen_init_method(ModuleInfo, NumExtra, RttiProcId, Init,
		ExtraDefns0, ExtraDefns) :-
	%
	% we can't store the address of the typeclass method directly in
	% the base_typeclass_info; instead, we need to generate
	% a wrapper function that extracts the NumExtra parameters
	% it needs from the typeclass_info, and store the address
	% of that wrapper function in the base_typeclass_info.
	%
	% Note that this means there are two levels of wrappers:
	% the wrapper that we generate here calls the
	% procedure introduced by check_typeclass.m,
	% and that in turn calls the user's procedure.
	% Hopefully the Mercury HLDS->HLDS inlining and/or
	% the target code compiler will be able to optimize this...
	%

	%
	% We start off by creating a fresh MLGenInfo here,
	% using the pred_id and proc_id of the wrapped procedure.
	% This requires considerable care.  We need to call
	% ml_gen_info_bump_func_label to ensure that the
	% function label allocated for the wrapper func
	% does not overlap with any function labels used
	% when generating code for the wrapped procedure.
	%
	PredId = RttiProcId ^ pred_id,
	ProcId = RttiProcId ^ proc_id,
	MLGenInfo0 = ml_gen_info_init(ModuleInfo, PredId, ProcId),
	ml_gen_info_bump_func_label(MLGenInfo0, MLGenInfo1),

	%
	% Now we can safely go ahead and generate the wrapper function
	%
	Offset = ml_typeclass_info_arg_offset,
	term__context_init(Context),
	ml_gen_closure_wrapper(PredId, ProcId, Offset, NumExtra,
		Context, WrapperFuncRval, WrapperFuncType,
		MLGenInfo1, MLGenInfo),
	ml_gen_info_get_extra_defns(MLGenInfo, ExtraDefns1),
	ExtraDefns = list__append(ExtraDefns1, ExtraDefns0),
	
	%
	% The initializer for the method field of the base_typeclass_info
	% is just the wrapper function's address, converted to
	% mlds__generic_type (by boxing).
	%
	Init = init_obj(unop(box(WrapperFuncType), WrapperFuncRval)).

:- func gen_init_proc_id(module_info, rtti_proc_label) = mlds__initializer.
gen_init_proc_id(ModuleInfo, RttiProcId) = Init :-
	%
	% construct an rval for the address of this procedure
	% (this is similar to ml_gen_proc_addr_rval)
	%
        ml_gen_pred_label_from_rtti(ModuleInfo, RttiProcId, PredLabel,
		PredModule),
	ProcId = RttiProcId ^ proc_id,
        QualifiedProcLabel = qual(PredModule, PredLabel - ProcId),
	Params = ml_gen_proc_params_from_rtti(ModuleInfo, RttiProcId),
	Signature = mlds__get_func_signature(Params),
	ProcAddrRval = const(code_addr_const(proc(QualifiedProcLabel, 
		Signature))),
	%
	% Convert the procedure address to a generic type.
	% We need to use a generic type because since the actual type
	% for the procedure will depend on how many type_info parameters
	% it takes, which will depend on the type's arity.
	%
        ProcAddrArg = unop(box(mlds__func_type(Params)), ProcAddrRval),
	Init = init_obj(ProcAddrArg).

:- func gen_init_proc_id_from_univ(module_info, univ) =
	mlds__initializer.

gen_init_proc_id_from_univ(ModuleInfo, ProcLabelUniv) = Init :-
	( univ_to_type(ProcLabelUniv, ProcLabel) ->
		Init = gen_init_proc_id(ModuleInfo, ProcLabel)
	;
		error("gen_init_proc_id_from_univ: cannot extract univ value")
	).

:- pred real_rtti_data(rtti_data::in) is semidet.

real_rtti_data(RttiData) :-
	\+ (
		(
			RttiData = type_info(TypeInfo),
			TypeInfo = plain_arity_zero_type_info(_)
		;
			RttiData = pseudo_type_info(PseudoTypeInfo),
			( PseudoTypeInfo = plain_arity_zero_pseudo_type_info(_)
			; PseudoTypeInfo = type_var(_)
			)
		)
	).

%-----------------------------------------------------------------------------%
%
% Conversion functions for builtin enumeration types.
%
% This handles sectag_locn and type_ctor_rep.
% The rvals generated are just named constants in
% the private_builtin module, which the Mercury
% runtime is expected to define.

:- func gen_init_sectag_locn(sectag_locn) = mlds__initializer.

gen_init_sectag_locn(Locn) = gen_init_builtin_const(Name) :-
	rtti__sectag_locn_to_string(Locn, Name).

:- func gen_init_type_ctor_rep(type_ctor_data) = mlds__initializer.

gen_init_type_ctor_rep(TypeCtorData) = gen_init_builtin_const(Name) :-
	rtti__type_ctor_rep_to_string(TypeCtorData, Name).

:- func gen_init_builtin_const(string) = mlds__initializer.

gen_init_builtin_const(Name) = init_obj(Rval) :-
        mercury_private_builtin_module(PrivateBuiltin),
	MLDS_Module = mercury_module_name_to_mlds(PrivateBuiltin),
	% XXX These are actually enumeration constants.
	% Perhaps we should be using an enumeration type here,
	% rather than `mlds__native_int_type'.
	Type = mlds__native_int_type,
	Rval = lval(var(qual(MLDS_Module, var_name(Name, no)), Type)).

%-----------------------------------------------------------------------------%
%
% Conversion functions for the basic types.
%
% This handles arrays, maybe, null pointers, strings, and ints.
%

:- func gen_init_array(func(T) = mlds__initializer, list(T)) =
	mlds__initializer.

gen_init_array(Conv, List) = init_array(list__map(Conv, List)).

:- func gen_init_maybe(mlds__type, func(T) = mlds__initializer, maybe(T)) =
	mlds__initializer.

gen_init_maybe(_Type, Conv, yes(X)) = Conv(X).
gen_init_maybe(Type, _Conv, no) = gen_init_null_pointer(Type).
	
:- func gen_init_null_pointer(mlds__type) = mlds__initializer.

gen_init_null_pointer(Type) =
	init_obj(mlds__unop(cast(mlds__generic_type), const(null(Type)))).

:- func gen_init_string(string) = mlds__initializer.

gen_init_string(String) = init_obj(const(string_const(String))).

:- func gen_init_int(int) = mlds__initializer.

gen_init_int(Int) = init_obj(const(int_const(Int))).

:- func gen_init_boxed_int(int) = mlds__initializer.

gen_init_boxed_int(Int) =
	init_obj(unop(box(mlds__native_int_type), const(int_const(Int)))).

:- func gen_init_reserved_address(module_info, reserved_address) =
	mlds__initializer.
	/* XXX using `mlds__generic_type' here is probably wrong */
gen_init_reserved_address(ModuleInfo, ReservedAddress) =
	init_obj(ml_gen_reserved_address(ModuleInfo, ReservedAddress,
		mlds__generic_type)).

%-----------------------------------------------------------------------------%
