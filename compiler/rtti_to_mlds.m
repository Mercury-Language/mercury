%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% rtti_to_mlds.m: convert RTTI data structures to MLDS.
% Author: fjh
%
% This module defines routines to convert from the back-end-independent
% RTTI data structures into MLDS definitions.
% The RTTI data structures are used for static data that is used
% for handling RTTI, polymorphism, and typeclasses.
%
%-----------------------------------------------------------------------------%

:- module rtti_to_mlds.
:- interface.
:- import_module hlds_module, rtti, mlds.
:- import_module list.

	% return a list of MLDS definitions for the given rtti_data list.
:- func rtti_data_list_to_mlds(module_info, list(rtti_data)) = mlds__defns.

	% Return a name, consisting only of alphabetic characters,
	% that would be suitable for the type name for the type
	% of the given rtti_name.  If rtti_name_has_array_type(Name) = yes,
	% then the name returned by mlds_rtti_type_name(Name) is the
	% array element type, otherwise it is the complete type.
:- func mlds_rtti_type_name(rtti_name) = string.

:- implementation.
:- import_module foreign, prog_data, hlds_data.
:- import_module pseudo_type_info, prog_util, prog_out, type_util.
:- import_module ml_code_util, ml_unify_gen.
:- import_module bool, list, std_util, string, term, require.

rtti_data_list_to_mlds(ModuleInfo, RttiDatas) =
	list__condense(list__map(rtti_data_to_mlds(ModuleInfo), RttiDatas)).

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
			rtti_data_to_name(RttiData, RttiTypeId, RttiName),
			Name = data(rtti(RttiTypeId, RttiName))
		),

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
		module_info_name(ModuleInfo, ModuleName),
		gen_init_rtti_data_defn(RttiData, ModuleName, ModuleInfo,
			Initializer, ExtraDefns),
		DefnBody = mlds__data(MLDS_Type, Initializer, GC_TraceCode),

		%
		% put it all together
		%
		MLDS_Defn = mlds__defn(Name, MLDS_Context, Flags, DefnBody),
		MLDS_Defns = [MLDS_Defn | ExtraDefns]
	).


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
:- pred gen_init_rtti_data_defn(rtti_data, module_name, module_info,
		mlds__initializer, list(mlds__defn)).
:- mode gen_init_rtti_data_defn(in, in, in, out, out) is det.

gen_init_rtti_data_defn(exist_locns(_RttiTypeId, _Ordinal, Locns), _, _,
		Init, []) :-
	Init = gen_init_array(gen_init_exist_locn, Locns).
gen_init_rtti_data_defn(exist_info(RttiTypeId, _Ordinal, Plain, InTci, Tci,
		Locns), ModuleName, _, Init, []) :-
	Init = init_struct([
		gen_init_int(Plain),
		gen_init_int(InTci),
		gen_init_int(Tci),
		gen_init_rtti_name(ModuleName, RttiTypeId, Locns)
	]).
gen_init_rtti_data_defn(field_names(_RttiTypeId, _Ordinal, MaybeNames), _, _,
		Init, []) :-
	StrType = term__functor(term__atom("string"), [], context("", 0)),
	Init = gen_init_array(gen_init_maybe(
			mercury_type(StrType, str_type,
				non_foreign_type(StrType)),
			gen_init_string), MaybeNames).
	
gen_init_rtti_data_defn(field_types(_RttiTypeId, _Ordinal, Types),
		ModuleName, _, Init, []) :-
	Init = gen_init_array(
		gen_init_cast_rtti_data(mlds__pseudo_type_info_type,
		ModuleName), Types).
gen_init_rtti_data_defn(reserved_addrs(_RttiTypeId, ReservedAddrs),
		_ModuleName, ModuleInfo, Init, []) :-
	Init = gen_init_array(gen_init_reserved_address(ModuleInfo),
		ReservedAddrs).
gen_init_rtti_data_defn(reserved_addr_functors(RttiTypeId,
			ReservedAddrFunctorDescs),
		ModuleName, _, Init, []) :-
	Init = gen_init_array(
		gen_init_rtti_name(ModuleName, RttiTypeId),
		ReservedAddrFunctorDescs).
gen_init_rtti_data_defn(enum_functor_desc(_RttiTypeId, FunctorName, Ordinal),
		_, _, Init, []) :-
	Init = init_struct([
		gen_init_string(FunctorName),
		gen_init_int(Ordinal)
	]).
gen_init_rtti_data_defn(notag_functor_desc(_RttiTypeId, FunctorName, ArgType,
		MaybeArgName), ModuleName, _, Init, []) :-
	Init = init_struct([
		gen_init_string(FunctorName),
		gen_init_cast_rtti_data(mlds__pseudo_type_info_type,
			ModuleName, ArgType),
		gen_init_maybe(ml_string_type, gen_init_string, MaybeArgName)
	]).
gen_init_rtti_data_defn(du_functor_desc(RttiTypeId, FunctorName, Ptag, Stag,
		Locn, Ordinal, Arity, ContainsVarBitVector, MaybeArgTypes,
		MaybeNames, MaybeExist), ModuleName, _, Init, []) :-
	Init = init_struct([
		gen_init_string(FunctorName),
		gen_init_int(Arity),
		gen_init_int(ContainsVarBitVector),
		gen_init_sectag_locn(Locn),
		gen_init_int(Ptag),
		gen_init_int(Stag),
		gen_init_int(Ordinal),
		gen_init_maybe(mlds__rtti_type(field_types(0)),
			gen_init_rtti_name(ModuleName, RttiTypeId),
			MaybeArgTypes),
		gen_init_maybe(mlds__rtti_type(field_names(0)),
			gen_init_rtti_name(ModuleName, RttiTypeId),
			MaybeNames),
		gen_init_maybe(mlds__rtti_type(exist_info(0)),
			gen_init_rtti_name(ModuleName, RttiTypeId),
			MaybeExist)
	]).
gen_init_rtti_data_defn(reserved_addr_functor_desc(_RttiTypeId, FunctorName, Ordinal,
		ReservedAddress), _, ModuleInfo, Init, []) :-
	Init = init_struct([
		gen_init_string(FunctorName),
		gen_init_int(Ordinal),
		gen_init_reserved_address(ModuleInfo, ReservedAddress)
	]).
gen_init_rtti_data_defn(enum_name_ordered_table(RttiTypeId, Functors),
		ModuleName, _, Init, []) :-
	Init = gen_init_rtti_names_array(ModuleName, RttiTypeId, Functors).
gen_init_rtti_data_defn(enum_value_ordered_table(RttiTypeId, Functors),
		ModuleName, _, Init, []) :-
	Init = gen_init_rtti_names_array(ModuleName, RttiTypeId, Functors).
gen_init_rtti_data_defn(du_name_ordered_table(RttiTypeId, Functors),
		ModuleName, _, Init, []) :-
	Init = gen_init_rtti_names_array(ModuleName, RttiTypeId, Functors).
gen_init_rtti_data_defn(du_stag_ordered_table(RttiTypeId, _Ptag, Sharers),
		ModuleName, _, Init, []) :-
	Init = gen_init_rtti_names_array(ModuleName, RttiTypeId, Sharers).
gen_init_rtti_data_defn(du_ptag_ordered_table(RttiTypeId, PtagLayouts),
		ModuleName, _, Init, []) :-
	Init = gen_init_array(gen_init_ptag_layout_defn(ModuleName, RttiTypeId),
		PtagLayouts).
gen_init_rtti_data_defn(reserved_addr_table(RttiTypeId,
		NumNumeric, NumSymbolic, ReservedAddrs, FunctorDescs, DuLayout),
		ModuleName, _, Init, []) :-
	Init = init_struct([
		gen_init_int(NumNumeric),
		gen_init_int(NumSymbolic),
		gen_init_rtti_name(ModuleName, RttiTypeId, ReservedAddrs),
		gen_init_rtti_name(ModuleName, RttiTypeId, FunctorDescs),
		gen_init_rtti_name(ModuleName, RttiTypeId, DuLayout)
	]).
gen_init_rtti_data_defn(type_ctor_info(RttiTypeId, UnifyProc, CompareProc,
		CtorRep, Version, NumPtags, NumFunctors, FunctorsInfo,
		LayoutInfo), ModuleName, ModuleInfo, Init, []) :-
	RttiTypeId = rtti_type_id(TypeModule, Type, TypeArity),
	prog_out__sym_name_to_string(TypeModule, TypeModuleName),
	Init = init_struct([
		gen_init_int(TypeArity),
		gen_init_int(Version),
		gen_init_type_ctor_rep(CtorRep),
		gen_init_int(NumPtags),
		gen_init_maybe_proc_id(ModuleInfo, UnifyProc),
		gen_init_maybe_proc_id(ModuleInfo, CompareProc),
		gen_init_string(TypeModuleName),
		gen_init_string(Type),
		% In the C back-end, these two "structs" are actually unions.
		% We need to use `init_struct' here so that the initializers
		% get enclosed in curly braces.
		init_struct([
			gen_init_functors_info(FunctorsInfo, ModuleName,
				RttiTypeId)
		]),
		init_struct([
			gen_init_layout_info(LayoutInfo, ModuleName, RttiTypeId)
		]),
		gen_init_int(NumFunctors)
			% These two are commented out while the corresponding
			% fields of the MR_TypeCtorInfo_Struct type are
			% commented out.
		% gen_init_maybe(gen_init_rtti_name(RttiTypeId),
		%	MaybeHashCons),
		% gen_init_maybe_proc_id(ModuleInfo, PrettyprinterProc)
	]).
gen_init_rtti_data_defn(base_typeclass_info(_InstanceModule, _ClassId,
		_InstanceStr, BaseTypeClassInfo), _ModuleName, ModuleInfo,
		Init, ExtraDefns) :-
	BaseTypeClassInfo = base_typeclass_info(N1, N2, N3, N4, N5,
		Methods),
	NumExtra = BaseTypeClassInfo^num_extra,
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
gen_init_rtti_data_defn(pseudo_type_info(Pseudo), ModuleName, _, Init, []) :-
	Init = gen_init_pseudo_type_info_defn(Pseudo, ModuleName).

:- func gen_init_functors_info(type_ctor_functors_info, module_name,
		rtti_type_id) = mlds__initializer.
gen_init_functors_info(enum_functors(EnumFunctorsInfo), ModuleName,
		RttiTypeId) =
	gen_init_cast_rtti_name(mlds__generic_type,
		ModuleName, RttiTypeId, EnumFunctorsInfo).
gen_init_functors_info(notag_functors(NotagFunctorsInfo), ModuleName,
		RttiTypeId) =
	gen_init_cast_rtti_name(mlds__generic_type,
		ModuleName, RttiTypeId, NotagFunctorsInfo).
gen_init_functors_info(du_functors(DuFunctorsInfo), ModuleName,
		RttiTypeId) =
	gen_init_cast_rtti_name(mlds__generic_type,
		ModuleName, RttiTypeId, DuFunctorsInfo).
gen_init_functors_info(no_functors, _, _) =
	gen_init_null_pointer(mlds__rtti_type(du_name_ordered_table)).

:- func gen_init_layout_info(type_ctor_layout_info, module_name,
		rtti_type_id) = mlds__initializer.

gen_init_layout_info(enum_layout(EnumLayoutInfo), ModuleName, RttiTypeId) =
	gen_init_cast_rtti_name(mlds__generic_type, ModuleName, RttiTypeId,
		EnumLayoutInfo).
gen_init_layout_info(notag_layout(NotagLayoutInfo), ModuleName, RttiTypeId) =
	gen_init_cast_rtti_name(mlds__generic_type, ModuleName, RttiTypeId,
		NotagLayoutInfo).
gen_init_layout_info(du_layout(DuLayoutInfo), ModuleName, RttiTypeId) =
	gen_init_cast_rtti_name(mlds__generic_type, ModuleName, RttiTypeId,
		DuLayoutInfo).
gen_init_layout_info(reserved_addr_layout(RaLayoutInfo), ModuleName, RttiTypeId) =
	gen_init_cast_rtti_name(mlds__generic_type, ModuleName, RttiTypeId,
		RaLayoutInfo).
gen_init_layout_info(equiv_layout(EquivTypeInfo), ModuleName, _RttiTypeId) =
	gen_init_cast_rtti_data(mlds__generic_type, ModuleName,
		EquivTypeInfo).
gen_init_layout_info(no_layout, _, _) =
	gen_init_null_pointer(mlds__rtti_type(du_ptag_ordered_table)).

:- func gen_init_maybe_proc_id(module_info, maybe(rtti_proc_label)) =
	mlds__initializer.

	% XXX the type here is a bit of a lie, but it is only used if we
	% generate a null constant, so it's pretty harmless right now. 
gen_init_maybe_proc_id(ModuleInfo, MaybeProcLabel) =
	gen_init_maybe(mlds__func_type(mlds__func_params([], [])),
		gen_init_proc_id(ModuleInfo), MaybeProcLabel).

:- func gen_init_pseudo_type_info_defn(pseudo_type_info, module_name) =
	mlds__initializer.

gen_init_pseudo_type_info_defn(type_var(_), _) = _ :-
	error("gen_init_pseudo_type_info_defn: type_var").
gen_init_pseudo_type_info_defn(type_ctor_info(_), _) = _ :-
	error("gen_init_pseudo_type_info_defn: type_ctor_info").
gen_init_pseudo_type_info_defn(type_info(RttiTypeId, ArgTypes), ModuleName) =
		Init :-
	ArgRttiDatas = list__map(func(P) = pseudo_type_info(P), ArgTypes),
	Init = init_struct([
		gen_init_rtti_name(ModuleName, RttiTypeId, type_ctor_info),
		gen_init_cast_rtti_datas_array(mlds__pseudo_type_info_type,
			ModuleName, ArgRttiDatas)
	]).
gen_init_pseudo_type_info_defn(higher_order_type_info(RttiTypeId,
		Arity, ArgTypes), ModuleName) = Init :-
	ArgRttiDatas = list__map(func(P) = pseudo_type_info(P), ArgTypes),
	Init = init_struct([
		gen_init_rtti_name(ModuleName, RttiTypeId, type_ctor_info),
		gen_init_int(Arity),
		gen_init_cast_rtti_datas_array(mlds__pseudo_type_info_type,
			ModuleName, ArgRttiDatas)
	]).

:- func gen_init_ptag_layout_defn(module_name, rtti_type_id, du_ptag_layout) =
	mlds__initializer.

gen_init_ptag_layout_defn(ModuleName, RttiTypeId, DuPtagLayout) = Init :-
	DuPtagLayout = du_ptag_layout(NumSharers, Locn, Descriptors) ,
	Init = init_struct([
		gen_init_int(NumSharers),
		gen_init_sectag_locn(Locn),
		gen_init_rtti_name(ModuleName, RttiTypeId, Descriptors)
	]).

%-----------------------------------------------------------------------------%

:- func gen_init_rtti_names_array(module_name, rtti_type_id,
		list(rtti_name)) = mlds__initializer.
gen_init_rtti_names_array(ModuleName, RttiTypeId, RttiNames) =
	gen_init_array(gen_init_rtti_name(ModuleName, RttiTypeId), RttiNames).

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
		rtti_data_to_name(RttiData, RttiTypeId, RttiName),
		Initializer = gen_init_cast_rtti_name(DestType,
			ModuleName, RttiTypeId, RttiName)
	).

	% currently casts only store the destination type
:- func gen_cast(mlds__type, mlds__type) = mlds__unary_op.
gen_cast(_SrcType, DestType) = cast(DestType).

	% Generate the MLDS initializer comprising the rtti_name
	% for a given rtti_data.
:- func gen_init_rtti_data(module_name, rtti_data) = mlds__initializer.

gen_init_rtti_data(ModuleName, RttiData) = Initializer :-
	rtti_data_to_name(RttiData, RttiTypeId, RttiName),
	Initializer = gen_init_rtti_name(ModuleName, RttiTypeId, RttiName).

	% Generate an MLDS initializer comprising just the
	% the rval for a given rtti_name
:- func gen_init_rtti_name(module_name, rtti_type_id, rtti_name) =
	mlds__initializer.

gen_init_rtti_name(ModuleName, RttiTypeId, RttiName) =
	init_obj(gen_rtti_name(ModuleName, RttiTypeId, RttiName)).

	% Generate the MLDS initializer comprising the rtti_name
	% for a given rtti_name, converted to the given type.
:- func gen_init_cast_rtti_name(mlds__type, module_name, rtti_type_id,
	rtti_name) = mlds__initializer.

gen_init_cast_rtti_name(DestType, ModuleName, RttiTypeId, RttiName) =
		Initializer :-
	SrcType = rtti_type(RttiName), 
	Initializer = init_obj(unop(gen_cast(SrcType, DestType),
		gen_rtti_name(ModuleName, RttiTypeId, RttiName))).

	% Generate the MLDS rval for an rtti_name.
:- func gen_rtti_name(module_name, rtti_type_id, rtti_name) = mlds__rval.

gen_rtti_name(ThisModuleName, RttiTypeId0, RttiName) = Rval :-
	%
	% Typeinfos are defined locally to each module.
	% Other kinds of RTTI data are defining in the module
	% corresponding to the type which they are for.
	%
	(
		RttiName = pseudo_type_info(PseudoTypeInfo),
		( PseudoTypeInfo = type_info(_, _)
		; PseudoTypeInfo = higher_order_type_info(_, _, _)
		)
	->
		ModuleName = ThisModuleName,
		RttiTypeId = RttiTypeId0
	;
		RttiTypeId0 = rtti_type_id(RttiModuleName,
			RttiTypeName, RttiTypeArity),
		%
		% Although the builtin types `int', `float', etc. are treated
		% as part of the `builtin' module, for historical reasons they
		% don't have any qualifiers at this point, so we need to add
		% the `builtin' qualifier now.
		%
		( RttiModuleName = unqualified("") ->
			mercury_public_builtin_module(ModuleName),
			RttiTypeId = rtti_type_id(RttiModuleName,
				RttiTypeName, RttiTypeArity)
		;
			ModuleName = RttiModuleName,
			RttiTypeId = RttiTypeId0
		)
	),
	MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
	MLDS_DataName = rtti(RttiTypeId, RttiName),
	DataAddr = data_addr(MLDS_ModuleName, MLDS_DataName),
	Rval = const(data_addr_const(DataAddr)).

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
	% of that wrapper function in the typeclass_info.
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
	PredId = RttiProcId^pred_id,
	ProcId = RttiProcId^proc_id,
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
	ProcId = RttiProcId^proc_id,
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

:- func gen_init_type_ctor_rep(type_ctor_rep) = mlds__initializer.
gen_init_type_ctor_rep(Rep) = gen_init_builtin_const(Name) :-
	rtti__type_ctor_rep_to_string(Rep, Name).

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

% the type names mentioned here should be defined in runtime/mercury.h
% (or in some header file that is included by that one)

mlds_rtti_type_name(exist_locns(_)) =		"DuExistLocn".
mlds_rtti_type_name(exist_info(_)) =		"DuExistInfo".
mlds_rtti_type_name(field_names(_)) =		"ConstString".
mlds_rtti_type_name(field_types(_)) =		"PseudoTypeInfo".
mlds_rtti_type_name(reserved_addrs) =		"ReservedAddrs".
mlds_rtti_type_name(reserved_addr_functors) =	"ReservedAddrFunctors".
mlds_rtti_type_name(enum_functor_desc(_)) =	"EnumFunctorDesc".
mlds_rtti_type_name(notag_functor_desc) =	"NotagFunctorDesc".
mlds_rtti_type_name(du_functor_desc(_)) =	"DuFunctorDesc".
mlds_rtti_type_name(reserved_addr_functor_desc(_)) = "ReservedAddrFunctorDesc".
mlds_rtti_type_name(enum_name_ordered_table) =	"EnumFunctorDescPtr".
mlds_rtti_type_name(enum_value_ordered_table) =	"EnumFunctorDescPtr".
mlds_rtti_type_name(du_name_ordered_table) =	"DuFunctorDescPtr".
mlds_rtti_type_name(du_stag_ordered_table(_)) =	"DuFunctorDescPtr".
mlds_rtti_type_name(du_ptag_ordered_table) =	"DuPtagLayout".
mlds_rtti_type_name(reserved_addr_table) =	"ReservedAddrTypeDesc".
mlds_rtti_type_name(type_ctor_info) =		"TypeCtorInfo_Struct".
mlds_rtti_type_name(base_typeclass_info(_, _, _)) = "BaseTypeclassInfo".
mlds_rtti_type_name(pseudo_type_info(Pseudo)) =
	mlds_pseudo_type_info_type_name(Pseudo).
mlds_rtti_type_name(type_hashcons_pointer) =	"TableNodePtrPtr".

:- func mlds_pseudo_type_info_type_name(pseudo_type_info) = string.

mlds_pseudo_type_info_type_name(type_var(_)) = _ :-
	% we use small integers to represent type_vars,
	% rather than pointers, so there is no pointed-to type
	error("mlds_rtti_type_name: type_var").
mlds_pseudo_type_info_type_name(type_ctor_info(_)) =
	"TypeCtorInfo_Struct".
mlds_pseudo_type_info_type_name(type_info(_TypeId, ArgTypes)) =
	string__format("FO_PseudoTypeInfo_Struct%d",
		[i(list__length(ArgTypes))]).
mlds_pseudo_type_info_type_name(higher_order_type_info(_TypeId, _Arity,
		ArgTypes)) =
	string__format("HO_PseudoTypeInfo_Struct%d",
		[i(list__length(ArgTypes))]).

%-----------------------------------------------------------------------------%
