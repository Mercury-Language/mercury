%-----------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
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
:- import_module rtti, mlds.
:- import_module list.

	% return a list of MLDS definitions for the given rtti_data list.
:- func rtti_data_list_to_mlds(list(rtti_data)) = mlds__defns.

	% return a name, consisting only of alphabetic characters,
	% that would be suitable for the type name for the type
	% of the given rtti_name.
:- func mlds_rtti_type_name(rtti_name) = string.

:- implementation.
:- import_module pseudo_type_info, ml_code_util, prog_util, prog_out.
:- import_module bool, list, std_util, string, term, require.

rtti_data_list_to_mlds(RttiDatas) =
	list__condense(list__map(rtti_data_to_mlds, RttiDatas)).

	% return a list of MLDS definitions for the given rtti_data.
:- func rtti_data_to_mlds(rtti_data) = mlds__defns.
rtti_data_to_mlds(RttiData) = MLDS_Defns :-
	( RttiData = pseudo_type_info(type_var(_)) ->
		% These just get represented as integers,
		% so we don't need to define them.
		% Also rtti_data_to_name/3 does not handle this case.
		MLDS_Defns = []
    	;
		%
		% Generate the name
		%
		rtti_data_to_name(RttiData, RttiTypeId, RttiName),
		Name = data(rtti(RttiTypeId, RttiName)),

		%
		% Generate the context
		%
		% XXX the rtti_data ought to include a prog_context
		% (the context of the corresponding type definition).
		term__context_init(Context),
		MLDS_Context = mlds__make_context(Context),

		%
		% Generate the declaration flags
		%
		Exported = rtti_name_is_exported(RttiName),
		Flags = rtti_data_decl_flags(Exported),

		%
		% Generate the declaration body,
		% i.e. the type and the initializer
		%
		MLDS_Type = rtti_type(RttiName),
		Initializer = gen_init_rtti_data_defn(RttiData),
		DefnBody = mlds__data(MLDS_Type, Initializer),

		%
		% put it all together
		%
		MLDS_Defn = mlds__defn(Name, MLDS_Context, Flags, DefnBody),
		MLDS_Defns = [MLDS_Defn]
	).


	% Return the declaration flags appropriate for an rtti_data.
	%
:- func rtti_data_decl_flags(bool) = mlds__decl_flags.
rtti_data_decl_flags(Exported) = MLDS_DeclFlags :-
	( Exported = yes ->
		Access = public
	;
		Access = private
	),
	PerInstance = per_instance,
	Virtuality = non_virtual,
	Finality = overridable,
	Constness = const,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).

%-----------------------------------------------------------------------------%

	% Return an MLDS initializer for the given RTTI definition.
:- func gen_init_rtti_data_defn(rtti_data) = mlds__initializer.

gen_init_rtti_data_defn(exist_locns(_RttiTypeId, _Ordinal, Locns)) =
	gen_init_array(gen_init_exist_locn, Locns).
gen_init_rtti_data_defn(exist_info(RttiTypeId, _Ordinal, Plain, InTci, Tci,
		Locns)) =
	init_struct([
		gen_init_int(Plain),
		gen_init_int(InTci),
		gen_init_int(Tci),
		gen_init_rtti_name(RttiTypeId, Locns)
	]).
gen_init_rtti_data_defn(field_names(_RttiTypeId, _Ordinal, MaybeNames)) =
	gen_init_array(gen_init_maybe(gen_init_string), MaybeNames).
gen_init_rtti_data_defn(field_types(_RttiTypeId, _Ordinal, Types)) =
	gen_init_array(gen_init_cast_rtti_data, Types).
gen_init_rtti_data_defn(enum_functor_desc(_RttiTypeId, FunctorName, Ordinal)) =
	init_struct([
		gen_init_string(FunctorName),
		gen_init_int(Ordinal)
	]).
gen_init_rtti_data_defn(notag_functor_desc(_RttiTypeId, FunctorName, ArgType)) =
	init_struct([
		gen_init_string(FunctorName),
		gen_init_cast_rtti_data(ArgType)
	]).
gen_init_rtti_data_defn(du_functor_desc(RttiTypeId, FunctorName, Ptag, Stag,
		Locn, Ordinal, Arity, ContainsVarBitVector, ArgTypes,
		MaybeNames, MaybeExist)) =
	init_struct([
		gen_init_string(FunctorName),
		gen_init_int(Arity),
		gen_init_int(ContainsVarBitVector),
		gen_init_sectag_locn(Locn),
		gen_init_int(Ptag),
		gen_init_int(Stag),
		gen_init_int(Ordinal),
		gen_init_rtti_name(RttiTypeId, ArgTypes),
		gen_init_maybe(gen_init_rtti_name(RttiTypeId), MaybeNames),
		gen_init_maybe(gen_init_rtti_name(RttiTypeId), MaybeExist)
	]).
gen_init_rtti_data_defn(enum_name_ordered_table(RttiTypeId, Functors)) =
	gen_init_rtti_names_array(RttiTypeId, Functors).
gen_init_rtti_data_defn(enum_value_ordered_table(RttiTypeId, Functors)) =
	gen_init_rtti_names_array(RttiTypeId, Functors).
gen_init_rtti_data_defn(du_name_ordered_table(RttiTypeId, Functors)) =
	gen_init_rtti_names_array(RttiTypeId, Functors).
gen_init_rtti_data_defn(du_stag_ordered_table(RttiTypeId, _Ptag, Sharers)) =
	gen_init_rtti_names_array(RttiTypeId, Sharers).
gen_init_rtti_data_defn(du_ptag_ordered_table(RttiTypeId, PtagLayouts)) =
	gen_init_array(gen_init_ptag_layout_defn(RttiTypeId), PtagLayouts).
gen_init_rtti_data_defn(type_ctor_info(RttiTypeId, UnifyProc, CompareProc,
		CtorRep, SolverProc, InitProc, Version, NumPtags, NumFunctors,
		FunctorsInfo, LayoutInfo, _MaybeHashCons,
		_PrettyprinterProc)) = Initializer :-
	RttiTypeId = rtti_type_id(Module, Type, TypeArity),
	prog_out__sym_name_to_string(Module, ModuleName),
	Initializer = init_struct([
		gen_init_int(TypeArity),
		gen_init_maybe_proc_id(UnifyProc),
		gen_init_maybe_proc_id(UnifyProc),
		gen_init_maybe_proc_id(CompareProc),
		gen_init_type_ctor_rep(CtorRep),
		gen_init_maybe_proc_id(SolverProc),
		gen_init_maybe_proc_id(InitProc),
		gen_init_string(ModuleName),
		gen_init_string(Type),
		gen_init_int(Version),
		gen_init_functors_info(FunctorsInfo, RttiTypeId),
		gen_init_layout_info(LayoutInfo, RttiTypeId),
		gen_init_int(NumFunctors),
		gen_init_int(NumPtags)
			% These two are commented out while the corresponding
			% fields of the MR_TypeCtorInfo_Struct type are
			% commented out.
		% gen_init_maybe(gen_init_rtti_name(RttiTypeId),
		%	MaybeHashCons),
		% gen_init_maybe_proc_id(PrettyprinterProc)
	]).
gen_init_rtti_data_defn(pseudo_type_info(Pseudo)) =
	gen_init_pseudo_type_info_defn(Pseudo).

:- func gen_init_functors_info(type_ctor_functors_info, rtti_type_id) =
	mlds__initializer.

gen_init_functors_info(enum_functors(EnumFunctorsInfo), RttiTypeId) =
	gen_init_cast_rtti_name(RttiTypeId, EnumFunctorsInfo).
gen_init_functors_info(notag_functors(NotagFunctorsInfo), RttiTypeId) =
	gen_init_cast_rtti_name(RttiTypeId, NotagFunctorsInfo).
gen_init_functors_info(du_functors(DuFunctorsInfo), RttiTypeId) =
	gen_init_cast_rtti_name(RttiTypeId, DuFunctorsInfo).
gen_init_functors_info(no_functors, _) =
	gen_init_null_pointer.

:- func gen_init_layout_info(type_ctor_layout_info, rtti_type_id) =
	mlds__initializer.

gen_init_layout_info(enum_layout(EnumLayoutInfo), RttiTypeId) =
	gen_init_cast_rtti_name(RttiTypeId, EnumLayoutInfo).
gen_init_layout_info(notag_layout(NotagLayoutInfo), RttiTypeId) =
	gen_init_cast_rtti_name(RttiTypeId, NotagLayoutInfo).
gen_init_layout_info(du_layout(DuLayoutInfo), RttiTypeId) =
	gen_init_cast_rtti_name(RttiTypeId, DuLayoutInfo).
gen_init_layout_info(equiv_layout(EquivTypeInfo), _RttiTypeId) =
	gen_init_cast_rtti_data(EquivTypeInfo).
gen_init_layout_info(no_layout, _RttiTypeId) =
	gen_init_null_pointer.

:- func gen_init_maybe_proc_id(maybe(rtti_proc_label)) = mlds__initializer.

gen_init_maybe_proc_id(MaybeProcLabel) =
	gen_init_maybe(gen_init_proc_id, MaybeProcLabel).

:- func gen_init_pseudo_type_info_defn(pseudo_type_info) = mlds__initializer.

gen_init_pseudo_type_info_defn(type_var(_)) = _ :-
	error("gen_init_pseudo_type_info_defn: type_var").
gen_init_pseudo_type_info_defn(type_ctor_info(_)) = _ :-
	error("gen_init_pseudo_type_info_defn: type_ctor_info").
gen_init_pseudo_type_info_defn(type_info(RttiTypeId, ArgTypes)) = Init :-
	ArgRttiDatas = list__map(func(P) = pseudo_type_info(P), ArgTypes),
	Init = init_struct([
		gen_init_rtti_name(RttiTypeId, type_ctor_info),
		gen_init_cast_rtti_datas_array(ArgRttiDatas)
	]).
gen_init_pseudo_type_info_defn(higher_order_type_info(RttiTypeId,
		Arity, ArgTypes)) = Init :-
	ArgRttiDatas = list__map(func(P) = pseudo_type_info(P), ArgTypes),
	Init = init_struct([
		gen_init_rtti_name(RttiTypeId, type_ctor_info),
		gen_init_int(Arity),
		gen_init_cast_rtti_datas_array(ArgRttiDatas)
	]).

:- func gen_init_ptag_layout_defn(rtti_type_id, du_ptag_layout) =
	mlds__initializer.

gen_init_ptag_layout_defn(RttiTypeId, DuPtagLayout) = Init :-
	DuPtagLayout = du_ptag_layout(NumSharers, Locn, Descriptors) ,
	Init = init_struct([
		gen_init_int(NumSharers),
		gen_init_sectag_locn(Locn),
		gen_init_rtti_name(RttiTypeId, Descriptors)
	]).

%-----------------------------------------------------------------------------%

:- func gen_init_rtti_names_array(rtti_type_id, list(rtti_name)) =
	mlds__initializer.
gen_init_rtti_names_array(RttiTypeId, RttiNames) =
	gen_init_array(gen_init_rtti_name(RttiTypeId), RttiNames).

:- func gen_init_rtti_datas_array(list(rtti_data)) = mlds__initializer.
gen_init_rtti_datas_array(RttiDatas) =
	gen_init_array(gen_init_rtti_data, RttiDatas).

:- func gen_init_cast_rtti_datas_array(list(rtti_data)) = mlds__initializer.
gen_init_cast_rtti_datas_array(RttiDatas) =
	gen_init_array(gen_init_cast_rtti_data, RttiDatas).

	% Generate the MLDS initializer comprising the rtti_name
	% for a given rtti_data, converted to mlds__generic_type.
:- func gen_init_cast_rtti_data(rtti_data) = mlds__initializer.

gen_init_cast_rtti_data(RttiData) = Initializer :-
	( RttiData = pseudo_type_info(type_var(VarNum)) ->
		% rtti_data_to_name/3 does not handle this case
		Initializer = init_obj(unop(box(mlds__native_int_type),
			const(int_const(VarNum))))
	;
		rtti_data_to_name(RttiData, RttiTypeId, RttiName),
		Initializer = gen_init_cast_rtti_name(RttiTypeId, RttiName)
	).

	% Generate the MLDS initializer comprising the rtti_name
	% for a given rtti_data.
:- func gen_init_rtti_data(rtti_data) = mlds__initializer.

gen_init_rtti_data(RttiData) = Initializer :-
	rtti_data_to_name(RttiData, RttiTypeId, RttiName),
	Initializer = gen_init_rtti_name(RttiTypeId, RttiName).

	% Generate an MLDS initializer comprising just the
	% the rval for a given rtti_name
:- func gen_init_rtti_name(rtti_type_id, rtti_name) = mlds__initializer.

gen_init_rtti_name(RttiTypeId, RttiName) =
	init_obj(gen_rtti_name(RttiTypeId, RttiName)).

	% Generate the MLDS initializer comprising the rtti_name
	% for a given rtti_name, converted to mlds__generic_type.
:- func gen_init_cast_rtti_name(rtti_type_id, rtti_name) = mlds__initializer.

gen_init_cast_rtti_name(RttiTypeId, RttiName) =
	init_obj(unop(box(rtti_type(RttiName)), 
		gen_rtti_name(RttiTypeId, RttiName))).

	% Generate the MLDS rval for an rtti_name.
:- func gen_rtti_name(rtti_type_id, rtti_name) = mlds__rval.

gen_rtti_name(RttiTypeId, RttiName) = Rval :-
	RttiTypeId = rtti_type_id(ModuleName, _Type, _TypeArity),
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

:- func gen_init_proc_id(rtti_proc_label) = mlds__initializer.
gen_init_proc_id(RttiProcId) = Init :-
	%
	% construct an rval for the address of this procedure
	% (this is similar to ml_gen_proc_addr_rval)
	%
        ml_gen_pred_label_from_rtti(RttiProcId, PredLabel, PredModule),
	ProcId = RttiProcId^proc_id,
        QualifiedProcLabel = qual(PredModule, PredLabel - ProcId),
	Params = ml_gen_proc_params_from_rtti(RttiProcId),
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
	Rval = lval(var(qual(MLDS_Module, Name))).

%-----------------------------------------------------------------------------%
%
% Conversion functions for the basic types.
%
% This handles arrays, maybe, null pointers, strings, and ints.
%

:- func gen_init_array(func(T) = mlds__initializer, list(T)) =
	mlds__initializer.

gen_init_array(Conv, List) = init_array(list__map(Conv, List)).

:- func gen_init_maybe(func(T) = mlds__initializer, maybe(T)) =
	mlds__initializer.

gen_init_maybe(Conv, yes(X)) = Conv(X).
gen_init_maybe(_, no) = gen_init_null_pointer.
	
:- func gen_init_null_pointer = mlds__initializer.

gen_init_null_pointer =
	% XXX the MLDS ought to have a null pointer constant
	init_obj(mlds__unop(cast(mlds__generic_type), const(int_const(0)))).

:- func gen_init_string(string) = mlds__initializer.

gen_init_string(String) = init_obj(const(string_const(String))).

:- func gen_init_int(int) = mlds__initializer.

gen_init_int(Int) = init_obj(const(int_const(Int))).

%-----------------------------------------------------------------------------%

mlds_rtti_type_name(exist_locns(_)) =		"DuExistLocnArray".
mlds_rtti_type_name(exist_info(_)) =		"DuExistInfo".
mlds_rtti_type_name(field_names(_)) =		"ConstStringArray".
mlds_rtti_type_name(field_types(_)) =		"PseudoTypeInfoArray".
mlds_rtti_type_name(enum_functor_desc(_)) =	"EnumFunctorDesc".
mlds_rtti_type_name(notag_functor_desc) =	"NotagFunctorDesc".
mlds_rtti_type_name(du_functor_desc(_)) =	"DuFunctorDesc".
mlds_rtti_type_name(enum_name_ordered_table) =	"EnumFunctorDescPtrArray".
mlds_rtti_type_name(enum_value_ordered_table) =	"EnumFunctorDescPtrArray".
mlds_rtti_type_name(du_name_ordered_table) =	"DuFunctorDescPtrArray".
mlds_rtti_type_name(du_stag_ordered_table(_)) =	"DuFunctorDescPtrArray".
mlds_rtti_type_name(du_ptag_ordered_table) =	"DuPtagLayoutArray".
mlds_rtti_type_name(type_ctor_info) =		"TypeCtorInfo_Struct".
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
