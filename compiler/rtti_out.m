%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This module contains code to output the RTTI data structures
% defined in rtti.m as C code.
%
% This module is part of the LLDS back-end.  The decl_set data type
% that it uses, which is defined in llds_out.m, represents a set of LLDS
% declarations, and thus depends on the LLDS.  Also the code to output
% code_addrs depends on the LLDS.
%
% The MLDS back-end does not use this module; instead it converts the RTTI
% data structures to MLDS (and then to C or Java, etc.).
%
% Main author: zs.
%
%-----------------------------------------------------------------------------%

:- module ll_backend__rtti_out.

:- interface.

:- import_module parse_tree__prog_data, hlds__hlds_data.
:- import_module backend_libs__rtti, ll_backend__llds_out.
:- import_module bool, io.

	% Output a C expression holding the address of the C name of
	% the specified rtti_data, preceded by the string in the first
	% argument (that string will usually be a C cast).
:- pred output_cast_addr_of_rtti_data(string::in, rtti_data::in,
	io__state::di, io__state::uo) is det.

	% Output a C expression holding the address of the C name of
	% the specified rtti_data.
:- pred output_addr_of_rtti_data(rtti_data::in, io__state::di, io__state::uo)
	is det.

	% Output a C declaration for the rtti_data.
:- pred output_rtti_data_decl(rtti_data::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

	% Output a C definition for the rtti_data.
:- pred output_rtti_data_defn(rtti_data::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

	% Output C code (e.g. a call to the MR_INIT_TYPE_CTOR_INFO() macro)
	% to initialize the rtti_data if necessary.
:- pred rtti_out__init_rtti_data_if_nec(rtti_data::in,
	io__state::di, io__state::uo) is det.

	% Output C code (e.g. a call to MR_register_type_ctor_info())
	% to register the rtti_data in the type tables, if it represents a data
	% structure that should be so registered. The bool should be the value
	% of the --split-c-files option; it governs whether the rtti_data is
	% declared in the generated code or not.
:- pred rtti_out__register_rtti_data_if_nec(rtti_data::in,
	bool::in, io__state::di, io__state::uo) is det.

	% Output the C name of the rtti_data specified by the given
	% rtti_type_ctor and rtti_name.
:- pred output_rtti_addr(rtti_type_ctor::in, rtti_name::in,
	io__state::di, io__state::uo) is det.

	% Output the C storage class, C type, and C name of the rtti_data 
	% specified by the given rtti_type_ctor and rtti_name,
	% for use in a declaration or definition.
	% The bool should be `yes' iff it is for a definition.
:- pred output_rtti_addr_storage_type_name(rtti_type_ctor::in, rtti_name::in,
	bool::in, io__state::di, io__state::uo) is det.

	% The same as output_rtti_addr_storage_type_name,
	% but for a base_typeclass_info.
:- pred output_base_typeclass_info_storage_type_name(module_name::in,
		class_id::in, string::in, bool::in,
		io__state::di, io__state::uo) is det.

:- implementation.

:- import_module parse_tree__prog_out.
:- import_module hlds__error_util.
:- import_module backend_libs__pseudo_type_info, backend_libs__type_ctor_info.
:- import_module backend_libs__c_util.
:- import_module ll_backend__llds, ll_backend__code_util.
:- import_module libs__options, libs__globals.
:- import_module int, string, list, assoc_list, map, require, std_util.

%-----------------------------------------------------------------------------%

output_rtti_data_defn(base_typeclass_info(InstanceModuleName, ClassId,
		InstanceString, BaseTypeClassInfo), DeclSet0, DeclSet) -->
	output_base_typeclass_info_defn(InstanceModuleName, ClassId,
		InstanceString, BaseTypeClassInfo, DeclSet0, DeclSet).
output_rtti_data_defn(type_info(TypeInfo), DeclSet0, DeclSet) -->
	output_type_info_defn(TypeInfo, DeclSet0, DeclSet).
output_rtti_data_defn(pseudo_type_info(PseudoTypeInfo), DeclSet0, DeclSet) -->
	output_pseudo_type_info_defn(PseudoTypeInfo, DeclSet0, DeclSet).
output_rtti_data_defn(type_ctor_info(TypeCtorData), DeclSet0, DeclSet) -->
	output_type_ctor_data_defn(TypeCtorData, DeclSet0, DeclSet).

%-----------------------------------------------------------------------------%

:- pred output_base_typeclass_info_defn(module_name::in, class_id::in,
	string::in, base_typeclass_info::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_base_typeclass_info_defn(InstanceModuleName, ClassId, InstanceString,
		base_typeclass_info(N1, N2, N3, N4, N5, Methods),
		DeclSet0, DeclSet) -->
	{ CodeAddrs = list__map(make_code_addr, Methods) },
	output_code_addrs_decls(CodeAddrs, "", "", 0, _, DeclSet0, DeclSet1),
	io__write_string("\n"),
	output_base_typeclass_info_storage_type_name(InstanceModuleName,
		ClassId, InstanceString, yes),
	% XXX It would be nice to avoid generating redundant declarations
	% of base_typeclass_infos, but currently we don't.
	{ DeclSet1 = DeclSet },
	io__write_string(" = {\n\t(MR_Code *) "),
	io__write_list([N1, N2, N3, N4, N5],
		",\n\t(MR_Code *) ", io__write_int),
	io__write_string(",\n\t"),
	io__write_list(CodeAddrs, ",\n\t", output_static_code_addr),
	io__write_string("\n};\n").

%-----------------------------------------------------------------------------%

:- pred output_maybe_pseudo_type_info_or_self_defn(
	rtti_maybe_pseudo_type_info_or_self::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_maybe_pseudo_type_info_or_self_defn(plain(TypeInfo),
		DeclSet0, DeclSet) -->
	output_type_info_defn(TypeInfo, DeclSet0, DeclSet).
output_maybe_pseudo_type_info_or_self_defn(pseudo(PseudoTypeInfo),
		DeclSet0, DeclSet) -->
	output_pseudo_type_info_defn(PseudoTypeInfo, DeclSet0, DeclSet).
output_maybe_pseudo_type_info_or_self_defn(self, DeclSet, DeclSet) --> [].

:- pred output_maybe_pseudo_type_info_defn(rtti_maybe_pseudo_type_info::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_maybe_pseudo_type_info_defn(plain(TypeInfo), DeclSet0, DeclSet) -->
	output_type_info_defn(TypeInfo, DeclSet0, DeclSet).
output_maybe_pseudo_type_info_defn(pseudo(PseudoTypeInfo), DeclSet0, DeclSet)
		-->
	output_pseudo_type_info_defn(PseudoTypeInfo, DeclSet0, DeclSet).

:- pred output_type_info_defn(rtti_type_info::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_type_info_defn(TypeInfo, DeclSet0, DeclSet) -->
	(
		{ rtti_data_to_name(type_info(TypeInfo),
			RttiTypeCtor, RttiName) },
		{ DataAddr = rtti_addr(RttiTypeCtor, RttiName) },
		{ decl_set_is_member(data_addr(DataAddr), DeclSet0) }
	->
		{ DeclSet = DeclSet0 }
	;
		do_output_type_info_defn(TypeInfo, DeclSet0, DeclSet)
	).

:- pred do_output_type_info_defn(rtti_type_info::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

do_output_type_info_defn(plain_arity_zero_type_info(_),
		DeclSet, DeclSet) --> [].
do_output_type_info_defn(TypeInfo, DeclSet0, DeclSet) -->
	{ TypeInfo = plain_type_info(RttiTypeCtor, Args) },
	{ TypeCtorRttiData = type_info(
		plain_arity_zero_type_info(RttiTypeCtor)) },
	{ ArgRttiDatas = list__map(type_info_to_rtti_data, Args) },
	output_type_ctor_and_arg_defns_and_decls(TypeCtorRttiData,
		ArgRttiDatas, DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		type_info(TypeInfo), DeclSet1, DeclSet),
	io__write_string(" = {\n\t&"),
	output_rtti_addr(RttiTypeCtor, type_ctor_info),
	io__write_string(",\n{"),
	output_cast_addr_of_rtti_datas("(MR_TypeInfo) ", ArgRttiDatas),
	io__write_string("}};\n").
do_output_type_info_defn(TypeInfo, DeclSet0, DeclSet) -->
	{ TypeInfo = var_arity_type_info(RttiVarArityId, Args) },
	{ RttiTypeCtor = var_arity_id_to_rtti_type_ctor(RttiVarArityId) },
	{ TypeCtorRttiData = type_info(
		plain_arity_zero_type_info(RttiTypeCtor)) },
	{ ArgRttiDatas = list__map(type_info_to_rtti_data, Args) },
	output_type_ctor_and_arg_defns_and_decls(TypeCtorRttiData,
		ArgRttiDatas, DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		type_info(TypeInfo), DeclSet1, DeclSet),
	io__write_string(" = {\n\t&"),
	output_rtti_addr(RttiTypeCtor, type_ctor_info),
	io__write_string(",\n\t"),
	{ list__length(Args, Arity) },
	io__write_int(Arity),
	io__write_string(",\n{"),
	output_cast_addr_of_rtti_datas("(MR_TypeInfo) ", ArgRttiDatas),
	io__write_string("}};\n").

:- pred output_pseudo_type_info_defn(rtti_pseudo_type_info::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_pseudo_type_info_defn(PseudoTypeInfo, DeclSet0, DeclSet) -->
	(
		{ PseudoTypeInfo = type_var(_) }
	->
		{ DeclSet = DeclSet0 }
	;
		{ rtti_data_to_name(pseudo_type_info(PseudoTypeInfo),
			RttiTypeCtor, RttiName) },
		{ DataAddr = rtti_addr(RttiTypeCtor, RttiName) },
		{ decl_set_is_member(data_addr(DataAddr), DeclSet0) }
	->
		{ DeclSet = DeclSet0 }
	;
		do_output_pseudo_type_info_defn(PseudoTypeInfo,
			DeclSet0, DeclSet)
	).

:- pred do_output_pseudo_type_info_defn(rtti_pseudo_type_info::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

do_output_pseudo_type_info_defn(plain_arity_zero_pseudo_type_info(_),
		DeclSet, DeclSet) --> [].
do_output_pseudo_type_info_defn(PseudoTypeInfo, DeclSet0, DeclSet) -->
	{ PseudoTypeInfo = plain_pseudo_type_info(RttiTypeCtor, Args) },
	{ TypeCtorRttiData = pseudo_type_info(
		plain_arity_zero_pseudo_type_info(RttiTypeCtor)) },
	{ ArgRttiDatas = list__map(maybe_pseudo_type_info_to_rtti_data, Args) },
	output_type_ctor_and_arg_defns_and_decls(TypeCtorRttiData,
		ArgRttiDatas, DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		pseudo_type_info(PseudoTypeInfo), DeclSet1, DeclSet),
	io__write_string(" = {\n\t&"),
	output_rtti_addr(RttiTypeCtor, type_ctor_info),
	io__write_string(",\n{"),
	output_cast_addr_of_rtti_datas("(MR_PseudoTypeInfo) ", ArgRttiDatas),
	io__write_string("}};\n").
do_output_pseudo_type_info_defn(PseudoTypeInfo, DeclSet0, DeclSet) -->
	{ PseudoTypeInfo = var_arity_pseudo_type_info(RttiVarArityId, Args) },
	{ RttiTypeCtor = var_arity_id_to_rtti_type_ctor(RttiVarArityId) },
	{ TypeCtorRttiData = pseudo_type_info(
		plain_arity_zero_pseudo_type_info(RttiTypeCtor)) },
	{ ArgRttiDatas = list__map(maybe_pseudo_type_info_to_rtti_data, Args) },
	output_type_ctor_and_arg_defns_and_decls(TypeCtorRttiData,
		ArgRttiDatas, DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		pseudo_type_info(PseudoTypeInfo), DeclSet1, DeclSet),
	io__write_string(" = {\n\t&"),
	output_rtti_addr(RttiTypeCtor, type_ctor_info),
	io__write_string(",\n\t"),
	{ list__length(Args, Arity) },
	io__write_int(Arity),
	io__write_string(",\n{"),
	output_cast_addr_of_rtti_datas("(MR_PseudoTypeInfo) ", ArgRttiDatas),
	io__write_string("}};\n").
do_output_pseudo_type_info_defn(type_var(_), DeclSet, DeclSet) --> [].

:- pred output_type_ctor_and_arg_defns_and_decls(rtti_data::in,
	list(rtti_data)::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_type_ctor_and_arg_defns_and_decls(TypeCtorRttiData, ArgRttiDatas,
		DeclSet0, DeclSet) -->
	output_rtti_data_decls(TypeCtorRttiData, "", "", 0, _,
		DeclSet0, DeclSet1),
	% We must output the definitions of the rtti_datas of the argument
	% typeinfos and/or pseudo-typeinfos, because they may contain other
	% typeinfos and/or pseudo-typeinfos nested within them. However,
	% zero arity typeinfos and pseudo-typeinfos have empty definitions,
	% yet the type_ctor_info they refer to still must be declared.
	% This is why both calls below are needed.
	list__foldl2(output_rtti_data_defn, ArgRttiDatas, DeclSet1, DeclSet2),
	output_rtti_datas_decls(ArgRttiDatas, "", "", 0, _,
		DeclSet2, DeclSet).

%-----------------------------------------------------------------------------%

:- pred output_type_ctor_data_defn(type_ctor_data::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_type_ctor_data_defn(TypeCtorData, DeclSet0, DeclSet) -->
	{ RttiTypeCtor = tcd_get_rtti_type_ctor(TypeCtorData) },
	{ TypeCtorData = type_ctor_data(Version, Module, TypeName, TypeArity,
		UnifyUniv, CompareUniv, TypeCtorDetails) },
	output_type_ctor_details_defn(RttiTypeCtor, TypeCtorDetails,
		MaybeFunctorsName, MaybeLayoutName, DeclSet0, DeclSet1),
	{ det_univ_to_type(UnifyUniv, UnifyProcLabel) },
	{ UnifyCodeAddr   = make_code_addr(UnifyProcLabel) },
	{ det_univ_to_type(CompareUniv, CompareProcLabel) },
	{ CompareCodeAddr = make_code_addr(CompareProcLabel) },
	{ CodeAddrs = [UnifyCodeAddr, CompareCodeAddr] },
	output_code_addrs_decls(CodeAddrs, "", "", 0, _, DeclSet1, DeclSet2),
	output_generic_rtti_data_defn_start(RttiTypeCtor, type_ctor_info,
		DeclSet2, DeclSet), io__write_string(" = {\n\t"),
	io__write_int(TypeArity),
	io__write_string(",\n\t"),
	io__write_int(Version),
	io__write_string(",\n\t"),
	io__write_int(type_ctor_details_num_ptags(TypeCtorDetails)),
	io__write_string(",\n\t"),
	{ rtti__type_ctor_rep_to_string(TypeCtorData, CtorRepStr) },
	io__write_string(CtorRepStr),
	io__write_string(",\n\t"),
	output_static_code_addr(UnifyCodeAddr),
	io__write_string(",\n\t"),
	output_static_code_addr(CompareCodeAddr),
	io__write_string(",\n\t"""),
	{ prog_out__sym_name_to_string(Module, ModuleName) },
	c_util__output_quoted_string(ModuleName),
	io__write_string(""",\n\t"""),
	c_util__output_quoted_string(TypeName),
	io__write_string(""",\n\t"),
	(
		{ MaybeFunctorsName = yes(FunctorsName) },
		io__write_string("{ (void *) &"),
		output_rtti_addr(RttiTypeCtor, FunctorsName),
		io__write_string(" }")
	;
		{ MaybeFunctorsName = no },
		io__write_string("{ 0 }")
	),
	io__write_string(",\n\t"),
	(
		{ MaybeLayoutName = yes(LayoutName) },
		io__write_string("{ (void *) &"),
		output_rtti_addr(RttiTypeCtor, LayoutName),
		io__write_string(" }")
	;
		{ MaybeLayoutName = no },
		io__write_string("{ 0 }")
	),
	io__write_string(",\n\t"),
	io__write_int(type_ctor_details_num_functors(TypeCtorDetails)),
% This code is commented out while the corresponding fields of the
% MR_TypeCtorInfo_Struct type are commented out.
%
%	io__write_string(",\n\t"),
%	(
%		{ MaybeHashCons = yes(HashConsDataAddr) },
%		io__write_string("&"),
%		output_rtti_addr(RttiTypeCtor, HashConsDataAddr)
%	;
%		{ MaybeHashCons = no },
%		io__write_string("NULL")
%	),
%	io__write_string(",\n\t"),
%	output_maybe_static_code_addr(Prettyprinter),
	io__write_string("\n};\n").

:- pred output_type_ctor_details_defn(rtti_type_ctor::in,
	type_ctor_details::in, maybe(rtti_name)::out, maybe(rtti_name)::out,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_type_ctor_details_defn(RttiTypeCtor, TypeCtorDetails,
		MaybeFunctorsName, MaybeLayoutName, DeclSet0, DeclSet) -->
	(
		{ TypeCtorDetails = enum(_, EnumFunctors, EnumByRep,
			EnumByName) },
		list__foldl2(output_enum_functor_defn(RttiTypeCtor),
			EnumFunctors, DeclSet0, DeclSet1),
		output_enum_value_ordered_table(RttiTypeCtor, EnumByRep,
			DeclSet1, DeclSet2),
		output_enum_name_ordered_table(RttiTypeCtor, EnumByName,
			DeclSet2, DeclSet),
		{ MaybeLayoutName = yes(enum_value_ordered_table) },
		{ MaybeFunctorsName = yes(enum_name_ordered_table) }
	;
		{ TypeCtorDetails = du(_, DuFunctors, DuByRep, DuByName) },
		list__foldl2(output_du_functor_defn(RttiTypeCtor), DuFunctors,
			DeclSet0, DeclSet1),
		output_du_ptag_ordered_table(RttiTypeCtor, DuByRep,
			DeclSet1, DeclSet2),
		output_du_name_ordered_table(RttiTypeCtor, DuByName,
			DeclSet2, DeclSet),
		{ MaybeLayoutName = yes(du_ptag_ordered_table) },
		{ MaybeFunctorsName = yes(du_name_ordered_table) }
	;
		{ TypeCtorDetails = reserved(_, MaybeResFunctors, ResFunctors,
			DuByRep, MaybeResByName) },
		list__foldl2(output_maybe_res_functor_defn(RttiTypeCtor),
			MaybeResFunctors, DeclSet0, DeclSet1),
		output_res_value_ordered_table(RttiTypeCtor, ResFunctors,
			DuByRep, DeclSet1, DeclSet2),
		output_res_name_ordered_table(RttiTypeCtor, MaybeResByName,
			DeclSet2, DeclSet),
		{ MaybeLayoutName = yes(res_value_ordered_table) },
		{ MaybeFunctorsName = yes(res_name_ordered_table) }
	;
		{ TypeCtorDetails = notag(_, NotagFunctor) },
		output_notag_functor_defn(RttiTypeCtor, NotagFunctor,
			DeclSet0, DeclSet),
		{ MaybeLayoutName = yes(notag_functor_desc) },
		{ MaybeFunctorsName = yes(notag_functor_desc) }
	;
		{ TypeCtorDetails = eqv(EqvType) },
		output_maybe_pseudo_type_info_defn(EqvType,
			DeclSet0, DeclSet1),
		{ TypeData = maybe_pseudo_type_info_to_rtti_data(EqvType) },
		output_rtti_data_decls(TypeData, "", "", 0, _,
			DeclSet1, DeclSet),
		{
			EqvType = plain(TypeInfo),
			LayoutName = type_info(TypeInfo)
		;
			EqvType = pseudo(PseudoTypeInfo),
			LayoutName = pseudo_type_info(PseudoTypeInfo)
		},
		{ MaybeLayoutName = yes(LayoutName) },
		{ MaybeFunctorsName = no }
	;
		{ TypeCtorDetails = builtin(_) }, { error("output_type_ctor_details_defn: builtin") }
	;
		{ TypeCtorDetails = impl_artifact(_) },
		{ error("output_type_ctor_details_defn: impl_artifact") }
	;
		{ TypeCtorDetails = foreign },
		{ DeclSet = DeclSet0 },
		{ MaybeLayoutName = no },
		{ MaybeFunctorsName = no }
	).

%-----------------------------------------------------------------------------%

:- pred output_enum_functor_defn(rtti_type_ctor::in, enum_functor::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_enum_functor_defn(RttiTypeCtor, EnumFunctor, DeclSet0, DeclSet) -->
	{ EnumFunctor = enum_functor(FunctorName, Ordinal) },
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		enum_functor_desc(Ordinal), DeclSet0, DeclSet),
	io__write_string(" = {\n\t"""),
	c_util__output_quoted_string(FunctorName),
	io__write_string(""",\n\t"),
	io__write_int(Ordinal),
	io__write_string("\n};\n").

:- pred output_notag_functor_defn(rtti_type_ctor::in, notag_functor::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_notag_functor_defn(RttiTypeCtor, NotagFunctor, DeclSet0, DeclSet) -->
	{ NotagFunctor = notag_functor(FunctorName, ArgType, MaybeArgName) },
	output_maybe_pseudo_type_info_defn(ArgType, DeclSet0, DeclSet1),
	{ ArgTypeData = maybe_pseudo_type_info_to_rtti_data(ArgType) },
	output_rtti_data_decls(ArgTypeData, "", "", 0, _,
		DeclSet1, DeclSet2),
	output_generic_rtti_data_defn_start(RttiTypeCtor, notag_functor_desc,
		DeclSet2, DeclSet),
	io__write_string(" = {\n\t"""),
	c_util__output_quoted_string(FunctorName),
	io__write_string(""",\n\t"),
	(
		{ ArgType = plain(ArgTypeInfo) },
		output_cast_addr_of_rtti_data("(MR_PseudoTypeInfo) ",
			type_info(ArgTypeInfo))
	;
		{ ArgType = pseudo(ArgPseudoTypeInfo) },
		% We need to cast the argument to MR_PseudoTypeInfo in case
		% it turns out to be a small integer, not a pointer.
		output_cast_addr_of_rtti_data("(MR_PseudoTypeInfo) ",
			pseudo_type_info(ArgPseudoTypeInfo))
	),
	io__write_string(",\n\t"),
	(
		{ MaybeArgName = yes(ArgName) },
		io__write_string(""""),
		io__write_string(ArgName),
		io__write_string("""")
	;
		{ MaybeArgName = no },
		io__write_string("NULL")
	),
	io__write_string("\n};\n").

:- pred output_du_functor_defn(rtti_type_ctor::in, du_functor::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_du_functor_defn(RttiTypeCtor, DuFunctor, DeclSet0, DeclSet) -->
	{ DuFunctor = du_functor(FunctorName, OrigArity, Ordinal, Rep,
		ArgInfos, MaybeExistInfo) },
	{ ArgTypes = list__map(du_arg_info_type, ArgInfos) },
	{ MaybeArgNames = list__map(du_arg_info_name, ArgInfos) },
	{ ArgNames = list__filter_map(project_yes, MaybeArgNames) },
	(
		{ ArgInfos = [_ | _] },
		output_du_arg_types(RttiTypeCtor, Ordinal, ArgTypes,
			DeclSet0, DeclSet1)
	;
		{ ArgInfos = [] },
		{ DeclSet1 = DeclSet0 }
	),
	(
		{ ArgNames = [_ | _] },
		output_du_arg_names(RttiTypeCtor, Ordinal, MaybeArgNames,
			DeclSet1, DeclSet2)
	;
		{ ArgNames = [] },
		{ DeclSet2 = DeclSet1 }
	),
	(
		{ MaybeExistInfo = yes(ExistInfo) },
		output_exist_info(RttiTypeCtor, Ordinal, ExistInfo,
			DeclSet2, DeclSet3)
	;
		{ MaybeExistInfo = no },
		{ DeclSet3 = DeclSet2 }
	),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		du_functor_desc(Ordinal), DeclSet3, DeclSet),
	io__write_string(" = {\n\t"""),
	c_util__output_quoted_string(FunctorName),
	io__write_string(""",\n\t"),
	io__write_int(OrigArity),
	io__write_string(",\n\t"),
	{ ContainsVarBitVector = compute_contains_var_bit_vector(ArgTypes) },
	io__write_int(ContainsVarBitVector),
	io__write_string(",\n\t"),
	{
		Rep = du_ll_rep(Ptag, SectagAndLocn)
	;
		Rep = du_hl_rep(_),
		error("output_du_functor_defn: du_hl_rep")
	},
	{
		SectagAndLocn = sectag_none,
		Locn = "MR_SECTAG_NONE",
		Stag = -1
	;
		SectagAndLocn = sectag_local(Stag),
		Locn = "MR_SECTAG_LOCAL"
	;
		SectagAndLocn = sectag_remote(Stag),
		Locn = "MR_SECTAG_REMOTE"
	},
	io__write_string(Locn),
	io__write_string(",\n\t"),
	io__write_int(Ptag),
	io__write_string(",\n\t"),
	io__write_int(Stag),
	io__write_string(",\n\t"),
	io__write_int(Ordinal),
	io__write_string(",\n\t"),
	io__write_string("(MR_PseudoTypeInfo *) "), % cast away const
	(
		{ ArgInfos = [_ | _] },
		output_addr_of_rtti_addr(RttiTypeCtor, field_types(Ordinal))
	;
		{ ArgInfos = [] },
		io__write_string("NULL")
	),
	io__write_string(",\n\t"),
	(
		{ ArgNames = [_ | _] },
		output_addr_of_rtti_addr(RttiTypeCtor, field_names(Ordinal))
	;
		{ ArgNames = [] },
		io__write_string("NULL")
	),
	io__write_string(",\n\t"),
	(
		{ MaybeExistInfo = yes(_) },
		output_addr_of_rtti_addr(RttiTypeCtor, exist_info(Ordinal))
	;
		{ MaybeExistInfo = no },
		io__write_string("NULL")
	),
	io__write_string("\n};\n").

:- pred output_res_functor_defn(rtti_type_ctor::in, reserved_functor::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_res_functor_defn(RttiTypeCtor, ResFunctor, DeclSet0, DeclSet) -->
	{ ResFunctor = reserved_functor(FunctorName, Ordinal, Rep) },
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		res_functor_desc(Ordinal), DeclSet0, DeclSet),
	io__write_string(" = {\n\t"""),
	c_util__output_quoted_string(FunctorName),
	io__write_string(""",\n\t"),
	io__write_int(Ordinal),
	io__write_string(",\n\t"),
	io__write_string("(void *) "),
	(
		{ Rep = null_pointer },
		io__write_string("NULL")
	;
		{ Rep = small_pointer(SmallPtr) },
		io__write_int(SmallPtr)
	;
		{ Rep = reserved_object(_, _, _) },
		{ error("output_res_functor_defn: reserved object") }
	),
	io__write_string("\n};\n").

:- pred output_maybe_res_functor_defn(rtti_type_ctor::in,
	maybe_reserved_functor::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_maybe_res_functor_defn(RttiTypeCtor, MaybeResFunctor, DeclSet0, DeclSet)
		-->
	(
		{ MaybeResFunctor = res_func(ResFunctor) },
		output_res_functor_defn(RttiTypeCtor, ResFunctor,
			DeclSet0, DeclSet)
	;
		{ MaybeResFunctor = du_func(DuFunctor) },
		output_du_functor_defn(RttiTypeCtor, DuFunctor,
			DeclSet0, DeclSet)
	).

%-----------------------------------------------------------------------------%

:- pred output_exist_locns_array(rtti_type_ctor::in, int::in,
	list(exist_typeinfo_locn)::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_exist_locns_array(RttiTypeCtor, Ordinal, Locns, DeclSet0, DeclSet) -->
	output_generic_rtti_data_defn_start(RttiTypeCtor, exist_locns(Ordinal),
		DeclSet0, DeclSet),
	(
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array if necessary.
		{ Locns = [] }
	->
		io__write_string("= { {0, 0} };\n")
	;
		io__write_string(" = {\n"),
		output_exist_locns(Locns),
		io__write_string("};\n")
	).

:- pred output_exist_info(rtti_type_ctor::in, int::in, exist_info::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_exist_info(RttiTypeCtor, Ordinal, ExistInfo, DeclSet0, DeclSet) -->
	{ ExistInfo = exist_info(Plain, InTci, Tci, Locns) }, 
	output_exist_locns_array(RttiTypeCtor, Ordinal, Locns,
		DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor, exist_info(Ordinal),
		DeclSet1, DeclSet),
	io__write_string(" = {\n\t"),
	io__write_int(Plain),
	io__write_string(",\n\t"),
	io__write_int(InTci),
	io__write_string(",\n\t"),
	io__write_int(Tci),
	io__write_string(",\n\t"),
	output_rtti_addr(RttiTypeCtor, exist_locns(Ordinal)),
	io__write_string("\n};\n").

:- pred output_du_arg_types(rtti_type_ctor::in, int::in,
	list(rtti_maybe_pseudo_type_info_or_self)::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_du_arg_types(RttiTypeCtor, Ordinal, ArgTypes, DeclSet0, DeclSet) -->
	list__foldl2(output_maybe_pseudo_type_info_or_self_defn, ArgTypes,
		DeclSet0, DeclSet1),
	{ ArgTypeDatas = list__map(maybe_pseudo_type_info_or_self_to_rtti_data,
		ArgTypes) },
	output_rtti_datas_decls(ArgTypeDatas, "", "", 0, _,
		DeclSet1, DeclSet2),
	output_generic_rtti_data_defn_start(RttiTypeCtor, field_types(Ordinal),
		DeclSet2, DeclSet),
	io__write_string(" = {\n"),
	{ require(list__is_not_empty(ArgTypes),
		"output_du_arg_types: empty list") },
	{ ArgRttiDatas = list__map(maybe_pseudo_type_info_or_self_to_rtti_data,
		ArgTypes) },
	output_cast_addr_of_rtti_datas("(MR_PseudoTypeInfo) ", ArgRttiDatas),
	io__write_string("};\n").

:- pred output_du_arg_names(rtti_type_ctor::in, int::in,
	list(maybe(string))::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_du_arg_names(RttiTypeCtor, Ordinal, MaybeNames, DeclSet0, DeclSet) -->
	output_generic_rtti_data_defn_start(RttiTypeCtor, field_names(Ordinal),
		DeclSet0, DeclSet),
	io__write_string(" = {\n"),
	{ require(list__is_not_empty(MaybeNames),
		"output_du_arg_names: empty list") },
	output_maybe_quoted_strings(MaybeNames),
	io__write_string("};\n").

%-----------------------------------------------------------------------------%

:- pred output_enum_value_ordered_table(rtti_type_ctor::in,
	map(int, enum_functor)::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_enum_value_ordered_table(RttiTypeCtor, FunctorMap, DeclSet0, DeclSet)
		-->
	{ Functors = map__values(FunctorMap) },
	{ FunctorRttiNames = list__map(enum_functor_rtti_name, Functors) },
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		enum_value_ordered_table, DeclSet0, DeclSet),
	io__write_string(" = {\n"),
	output_addr_of_rtti_addrs(RttiTypeCtor, FunctorRttiNames),
	io__write_string("};\n").

:- pred output_enum_name_ordered_table(rtti_type_ctor::in,
	map(string, enum_functor)::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_enum_name_ordered_table(RttiTypeCtor, FunctorMap, DeclSet0, DeclSet)
		-->
	{ Functors = map__values(FunctorMap) },
	{ FunctorRttiNames = list__map(enum_functor_rtti_name, Functors) },
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		enum_name_ordered_table, DeclSet0, DeclSet),
	io__write_string(" = {\n"),
	output_addr_of_rtti_addrs(RttiTypeCtor, FunctorRttiNames),
	io__write_string("};\n").

:- pred output_du_name_ordered_table(rtti_type_ctor::in,
	map(string, map(int, du_functor))::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_du_name_ordered_table(RttiTypeCtor, NameArityMap, DeclSet0, DeclSet) -->
	{ map__values(NameArityMap, ArityMaps) },
	{ list__map(map__values, ArityMaps, FunctorLists) },
	{ list__condense(FunctorLists, Functors) },
	{ FunctorRttiNames = list__map(du_functor_rtti_name, Functors) },
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		du_name_ordered_table, DeclSet0, DeclSet),
	io__write_string(" = {\n"),
	output_addr_of_rtti_addrs(RttiTypeCtor, FunctorRttiNames),
	io__write_string("};\n").

:- pred output_du_stag_ordered_table(rtti_type_ctor::in,
	pair(int, sectag_table)::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_du_stag_ordered_table(RttiTypeCtor, Ptag - SectagTable,
		DeclSet0, DeclSet) -->
	{ SectagTable = sectag_table(_SectagLocn, _NumSharers, SectagMap) },
	{ map__values(SectagMap, SectagFunctors) },
	{ FunctorNames = list__map(du_functor_rtti_name, SectagFunctors) },
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		du_stag_ordered_table(Ptag), DeclSet0, DeclSet),
	io__write_string(" = {\n"),
	output_addr_of_rtti_addrs(RttiTypeCtor, FunctorNames),
	io__write_string("\n};\n").

:- pred output_du_ptag_ordered_table(rtti_type_ctor::in,
	map(int, sectag_table)::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_du_ptag_ordered_table(RttiTypeCtor, PtagMap, DeclSet0, DeclSet) -->
	{ map__to_assoc_list(PtagMap, PtagList) },
	list__foldl2(output_du_stag_ordered_table(RttiTypeCtor), PtagList,
		DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		du_ptag_ordered_table, DeclSet1, DeclSet),
	io__write_string(" = {\n"),
	( { PtagList = [1 - _ | _] } ->
			% Output a dummy ptag definition for the 
			% reserved tag first.
		output_dummy_ptag_layout_defn,
		{ FirstPtag = 1 }
	; { PtagList = [0 - _ | _] } ->
		{ FirstPtag = 0 }
	;
		{ error("output_dummy_ptag_layout_defn: bad ptag list") }
	),
	output_du_ptag_ordered_table_body(RttiTypeCtor, PtagList, FirstPtag),
	io__write_string("\n};\n").

:- pred output_du_ptag_ordered_table_body(rtti_type_ctor::in,
	assoc_list(int, sectag_table)::in, int::in,
	io__state::di, io__state::uo) is det.

output_du_ptag_ordered_table_body(_RttiTypeCtor, [], _CurPtag) --> [].
output_du_ptag_ordered_table_body(RttiTypeCtor,
		[Ptag - SectagTable | PtagTail], CurPtag) -->
	{ require(unify(Ptag, CurPtag),
		"output_du_ptag_ordered_table_body: ptag mismatch") },
	{ SectagTable = sectag_table(SectagLocn, NumSharers, _SectagMap) },
	io__write_string("\t{ "),
	io__write_int(NumSharers),
	io__write_string(", "),
	{ rtti__sectag_locn_to_string(SectagLocn, LocnStr) },
	io__write_string(LocnStr),
	io__write_string(",\n\t"),
	output_rtti_addr(RttiTypeCtor, du_stag_ordered_table(Ptag)),
	( { PtagTail = [] } ->
		io__write_string(" }\n")
	;
		io__write_string(" },\n"),
		output_du_ptag_ordered_table_body(RttiTypeCtor, PtagTail,
			CurPtag + 1)
	).

	% Output a `dummy' ptag layout, for use by tags that aren't *real*
	% tags, such as the tag reserved when --reserve-tag is on.
	%
	% XXX Note that if one of these dummy ptag definitions is actually
	% accessed by the Mercury runtime, the result will be undefined.
	% This should be fixed by adding a MR_SECTAG_DUMMY and handling it
	% gracefully.
:- pred output_dummy_ptag_layout_defn(io__state::di, io__state::uo) is det.

output_dummy_ptag_layout_defn -->
	io__write_string("\t{ 0, MR_SECTAG_VARIABLE, NULL },\n").

:- pred output_res_addr_functors(rtti_type_ctor::in,
	reserved_functor::in, io__state::di, io__state::uo) is det.

output_res_addr_functors(RttiTypeCtor, ResFunctor) -->
	output_rtti_addr(RttiTypeCtor, res_functor_rtti_name(ResFunctor)),
	io__write_string(",\n").

:- pred output_res_value_ordered_table(rtti_type_ctor::in,
	list(reserved_functor)::in, map(int, sectag_table)::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_res_value_ordered_table(RttiTypeCtor, ResFunctors, DuPtagTable,
		DeclSet0, DeclSet) -->
	{ ResFunctorReps = list__map(res_addr_rep, ResFunctors) },
	{ list__filter(res_addr_is_numeric, ResFunctorReps,
		NumericResFunctorReps, SymbolicResFunctorReps) },
	{ list__length(NumericResFunctorReps, NumNumericResFunctorReps) },
	{ list__length(SymbolicResFunctorReps, NumSymbolicResFunctorReps) },
	{ require(unify(NumSymbolicResFunctorReps, 0),
		"output_res_value_ordered_table: symbolic functors") },

	output_generic_rtti_data_defn_start(RttiTypeCtor,
		res_addr_functors, DeclSet0, DeclSet1),
	io__write_string(" = {\n"),
	list__foldl(output_res_addr_functors(RttiTypeCtor), ResFunctors),
	io__write_string("};\n"),

	output_du_ptag_ordered_table(RttiTypeCtor, DuPtagTable,
		DeclSet1, DeclSet2),

	output_generic_rtti_data_defn_start(RttiTypeCtor,
		res_value_ordered_table, DeclSet2, DeclSet),
	io__write_string(" = {\n\t"""),
	io__write_int(NumNumericResFunctorReps),
	io__write_string(",\n\t"),
	io__write_int(NumSymbolicResFunctorReps),
	io__write_string(",\n\t"),
	io__write_string("NULL"),
	io__write_string(",\n\t"),
	output_rtti_addr(RttiTypeCtor, res_addr_functors),
	io__write_string(",\n\t"),
	output_rtti_addr(RttiTypeCtor, du_ptag_ordered_table),
	io__write_string("\n};\n").

:- pred output_res_name_ordered_table(rtti_type_ctor::in,
	map(string, map(int, maybe_reserved_functor))::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_res_name_ordered_table(RttiTypeCtor, NameArityMap, DeclSet0, DeclSet) -->
	{ map__values(NameArityMap, ArityMaps) },
	{ list__map(map__values, ArityMaps, FunctorLists) },
	{ list__condense(FunctorLists, Functors) },
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		res_name_ordered_table, DeclSet0, DeclSet),
	io__write_string(" = {\n\t"""),
	list__foldl(output_res_name_ordered_table_element(RttiTypeCtor),
		Functors),
	io__write_string("\n};\n").

:- pred output_res_name_ordered_table_element(rtti_type_ctor::in,
	maybe_reserved_functor::in, io__state::di, io__state::uo) is det.

output_res_name_ordered_table_element(RttiTypeCtor, MaybeResFunctor) -->
	io__write_string("\t{ """),
	(
		{ MaybeResFunctor = res_func(ResFunctor) },
		{ Name = ResFunctor ^ res_name },
		io__write_string(Name),
		io__write_string(""", "),
		io__write_string("0, "),
		io__write_string("MR_TRUE, ")
	;
		{ MaybeResFunctor = du_func(DuFunctor) },
		{ Name = DuFunctor ^ du_name },
		{ Arity = DuFunctor ^ du_orig_arity },
		io__write_string(Name),
		io__write_string(""", "),
		io__write_int(Arity),
		io__write_string(", "),
		io__write_string("MR_FALSE, ")
	),
	{ RttiName = maybe_res_functor_rtti_name(MaybeResFunctor) },
	output_rtti_addr(RttiTypeCtor, RttiName),
	io__write_string(" },\n").

%-----------------------------------------------------------------------------%

:- func make_code_addr(rtti_proc_label) = code_addr.

make_code_addr(ProcLabel) = CodeAddr :-
	code_util__make_entry_label_from_rtti(ProcLabel, no, CodeAddr).

:- pred output_reserved_address(reserved_address::in,
	io__state::di, io__state::uo) is det.

output_reserved_address(null_pointer) -->
	io__write_string("NULL").
output_reserved_address(small_pointer(Val)) -->
	io__write_string("(const void *) "),
	io__write_int(Val).
output_reserved_address(reserved_object(_, _, _)) -->
	% These should only be used for the MLDS back-end
	{ unexpected(this_file, "reserved_object") }.

%-----------------------------------------------------------------------------%

output_rtti_data_decl(RttiData, DeclSet0, DeclSet) -->
	( { RttiData = pseudo_type_info(type_var(_)) } ->
		% These just get represented as integers,
		% so we don't need to declare them.
		% Also rtti_data_to_name/3 does not handle this case.
		{ DeclSet = DeclSet0 }
	;
		{ RttiData = base_typeclass_info(InstanceModuleName, ClassId,
			InstanceStr, _) }
	->
		% rtti_data_to_name/3 does not handle this case
		output_base_typeclass_info_decl(InstanceModuleName, ClassId,
			InstanceStr, no, DeclSet0, DeclSet)
	;
		{ rtti_data_to_name(RttiData, RttiTypeCtor, RttiName) },
		output_generic_rtti_data_decl(RttiTypeCtor, RttiName,
			DeclSet0, DeclSet)
	).

:- pred output_base_typeclass_info_decl(module_name::in, class_id::in,
		string::in, bool::in, decl_set::in, decl_set::out,
		io__state::di, io__state::uo) is det.

output_base_typeclass_info_decl(InstanceModuleName, ClassId, InstanceStr,
		BeingDefined, DeclSet0, DeclSet) -->
	output_base_typeclass_info_storage_type_name(InstanceModuleName,
			ClassId, InstanceStr, BeingDefined),
	io__write_string(";\n"),
	% XXX It would be nice to avoid generating redundant declarations
	% of base_typeclass_infos, but currently we don't.
	{ DeclSet = DeclSet0 }.

output_base_typeclass_info_storage_type_name(InstanceModuleName, ClassId,
		InstanceStr, BeingDefined) -->
	output_rtti_name_storage_type_name(
		output_base_typeclass_info_name(ClassId, InstanceStr),
		base_typeclass_info(InstanceModuleName, ClassId, InstanceStr),
			BeingDefined).

%-----------------------------------------------------------------------------%

:- pred output_generic_rtti_data_decl(rtti_type_ctor::in, rtti_name::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_generic_rtti_data_decl(RttiTypeCtor, RttiName, DeclSet0, DeclSet) -->
	output_rtti_addr_storage_type_name(RttiTypeCtor, RttiName, no),
	io__write_string(";\n"),
	{ DataAddr = rtti_addr(RttiTypeCtor, RttiName) },
	{ decl_set_insert(DeclSet0, data_addr(DataAddr), DeclSet) }.

:- pred output_generic_rtti_data_defn_start(rtti_type_ctor::in, rtti_name::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_generic_rtti_data_defn_start(RttiTypeCtor, RttiName, DeclSet0, DeclSet)
		-->
	io__write_string("\n"),
	output_rtti_addr_storage_type_name(RttiTypeCtor, RttiName, yes),
	{ DataAddr = rtti_addr(RttiTypeCtor, RttiName) },
	{ decl_set_insert(DeclSet0, data_addr(DataAddr), DeclSet) }.

output_rtti_addr_storage_type_name(RttiTypeCtor, RttiName, BeingDefined) -->
	output_rtti_name_storage_type_name(
		output_rtti_addr(RttiTypeCtor, RttiName),
		RttiName, BeingDefined).

:- pred output_rtti_name_storage_type_name(
	pred(io__state, io__state)::pred(di, uo) is det,
	rtti_name::in, bool::in, io__state::di, io__state::uo) is det.

output_rtti_name_storage_type_name(OutputName, RttiName, BeingDefined) -->
	output_rtti_type_decl(RttiName),
	{ rtti_name_linkage(RttiName, Linkage) },
	globals__io_get_globals(Globals),
	{ LinkageStr = c_data_linkage_string(Globals, Linkage, yes,
		BeingDefined) },
	io__write_string(LinkageStr),

	{ InclCodeAddr = rtti_name_would_include_code_addr(RttiName) },
	{ c_data_const_string(Globals, InclCodeAddr, ConstStr) },
	io__write_string(ConstStr),

	{ rtti_name_c_type(RttiName, CType, IsArray) },
	c_util__output_quoted_string(CType),
	io__write_string(" "),
	OutputName,
	(
		{ IsArray = yes },
		io__write_string("[]")
	;
		{ IsArray = no }
	).

	% Each type_info and pseudo_type_info may have a different C type,
	% depending on what kind of type_info or pseudo_type_info it is,
	% and also on its arity. We need to declare that C type here.

:- pred output_rtti_type_decl(rtti_name::in, io__state::di, io__state::uo)
	is det.

output_rtti_type_decl(RttiName) -->
	(
		{ rtti_type_template_arity(RttiName, Arity) },
		{ Arity > max_always_declared_arity }
	->
		{ Template = 
"#ifndef MR_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY_%d_GUARD
#define MR_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY_%d_GUARD
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(%d);
#endif
"		},
		io__format(Template, [i(Arity), i(Arity), i(Arity)])
	;
		[]
	).

:- pred rtti_type_template_arity(rtti_name::in, int::out) is semidet.

rtti_type_template_arity(RttiName, NumArgTypes) :-
	RttiName = type_info(TypeInfo),
	(
		TypeInfo = plain_type_info(_, ArgTypes)
	;
		TypeInfo = var_arity_type_info(_, ArgTypes)
	),
	NumArgTypes = list__length(ArgTypes).
rtti_type_template_arity(RttiName, NumArgTypes) :-
	RttiName = pseudo_type_info(PseudoTypeInfo),
	(
		PseudoTypeInfo = plain_pseudo_type_info(_, ArgTypes)
	;
		PseudoTypeInfo = var_arity_pseudo_type_info(_, ArgTypes)
	),
	NumArgTypes = list__length(ArgTypes).

:- func max_always_declared_arity = int.

max_always_declared_arity = 20.

%-----------------------------------------------------------------------------%

rtti_out__init_rtti_data_if_nec(Data) -->
	(
		{ Data = type_ctor_info(TypeCtorData) }
	->
		{ RttiTypeCtor = tcd_get_rtti_type_ctor(TypeCtorData) },
		io__write_string("\tMR_INIT_TYPE_CTOR_INFO(\n\t\t"),
		output_rtti_addr(RttiTypeCtor, type_ctor_info),
		io__write_string(",\n\t\t"),
		{ RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, Arity) },
		{ llds_out__sym_name_mangle(ModuleName, ModuleNameString) },
		{ string__append(ModuleNameString, "__", UnderscoresModule) },
		( 
			{ string__append(UnderscoresModule, _, TypeName) } 
		->
			[]
		;
			io__write_string(UnderscoresModule)
		),
		{ llds_out__name_mangle(TypeName, MangledTypeName) },
		io__write_string(MangledTypeName),
		io__write_string("_"),
		io__write_int(Arity),
		io__write_string("_0);\n")
	;
		{ Data = base_typeclass_info(_ModName, ClassName, ClassArity,
			base_typeclass_info(_N1, _N2, _N3, _N4, _N5,
				Methods)) }
	->
		io__write_string("#ifndef MR_STATIC_CODE_ADDRESSES\n"),
			% the field number for the first method is 5,
			% since the methods are stored after N1 .. N5,
			% and fields are numbered from 0.
		{ FirstFieldNum = 5 },
		{ CodeAddrs = list__map(make_code_addr, Methods) },
		output_init_method_pointers(FirstFieldNum, CodeAddrs,
			ClassName, ClassArity),
		io__write_string("#endif /* MR_STATIC_CODE_ADDRESSES */\n")
	;
		[]
	).

rtti_out__register_rtti_data_if_nec(Data, SplitFiles) -->
	(
		{ Data = type_ctor_info(TypeCtorData) }
	->
		{ RttiTypeCtor = tcd_get_rtti_type_ctor(TypeCtorData) },
		(
			{ SplitFiles = yes },
			io__write_string("\t{\n\t"),
			output_rtti_addr_storage_type_name(RttiTypeCtor,
				type_ctor_info, no),
			io__write_string(
				";\n\tMR_register_type_ctor_info(\n\t\t&"),
			output_rtti_addr(RttiTypeCtor, type_ctor_info),
			io__write_string(");\n\t}\n")
		;
			{ SplitFiles = no },
			io__write_string(
				"\tMR_register_type_ctor_info(\n\t\t&"),
			output_rtti_addr(RttiTypeCtor, type_ctor_info),
			io__write_string(");\n")
		)
	;
		{ Data = base_typeclass_info(_InstanceModuleName, _ClassId,
			_InstanceString, _BaseTypeClassInfo) }
	->
		% XXX Registering base_typeclass_infos by themselves is not
		% enough. A base_typeclass_info doesn't say which types it
		% declares to be members of which typeclass, and for now
		% we don't even have any data structures in the runtime system
		% to describe such membership information.
		%
		% io__write_string("\tMR_register_base_typeclass_info(\n\t\t&"),
		% output_base_typeclass_info_storage_type_name(
		%	InstanceModuleName, ClassId, InstanceString, no),
		% io__write_string(");\n")
		[]
	;
		[]
	).

:- pred output_init_method_pointers(int, list(code_addr), class_id, string,
		io__state, io__state).
:- mode output_init_method_pointers(in, in, in, in, di, uo) is det.

output_init_method_pointers(_, [], _, _) --> [].
output_init_method_pointers(FieldNum, [Arg|Args], ClassId, InstanceStr) -->
	io__write_string("\t\t"),
	io__write_string("MR_field(MR_mktag(0), "),
	output_base_typeclass_info_name(ClassId, InstanceStr),
	io__format(", %d) =\n\t\t\t", [i(FieldNum)]),
	output_code_addr(Arg),
	io__write_string(";\n"),
	output_init_method_pointers(FieldNum + 1, Args, ClassId, InstanceStr).

%-----------------------------------------------------------------------------%

:- pred output_maybe_rtti_addrs_decls(rtti_type_ctor::in,
	list(maybe(rtti_name))::in, string::in, string::in, int::in, int::out,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_maybe_rtti_addrs_decls(_, [], _, _, N, N, DeclSet, DeclSet) --> [].
output_maybe_rtti_addrs_decls(RttiTypeCtor, [MaybeRttiName | RttiNames],
		FirstIndent, LaterIndent, N0, N, DeclSet0, DeclSet) -->
	(
		{ MaybeRttiName = yes(RttiName) },
		output_data_addr_decls(rtti_addr(RttiTypeCtor, RttiName),
			FirstIndent, LaterIndent, N0, N1, DeclSet0, DeclSet1)
	;
		{ MaybeRttiName = no },
		{ N1 = N0 },
		{ DeclSet1 = DeclSet0 }
	),
	output_maybe_rtti_addrs_decls(RttiTypeCtor, RttiNames,
		FirstIndent, LaterIndent, N1, N, DeclSet1, DeclSet).

:- pred output_rtti_datas_decls(list(rtti_data)::in,
	string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_rtti_datas_decls([], _, _, N, N, DeclSet, DeclSet) --> [].
output_rtti_datas_decls([RttiData | RttiDatas],
		FirstIndent, LaterIndent, N0, N, DeclSet0, DeclSet) -->
	output_rtti_data_decls(RttiData,
		FirstIndent, LaterIndent, N0, N1, DeclSet0, DeclSet1),
	output_rtti_datas_decls(RttiDatas,
		FirstIndent, LaterIndent, N1, N, DeclSet1, DeclSet).

:- pred output_rtti_addrs_decls(rtti_type_ctor::in, list(rtti_name)::in,
	string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_rtti_addrs_decls(_, [], _, _, N, N, DeclSet, DeclSet) --> [].
output_rtti_addrs_decls(RttiTypeCtor, [RttiName | RttiNames],
		FirstIndent, LaterIndent, N0, N, DeclSet0, DeclSet) -->
	output_data_addr_decls(rtti_addr(RttiTypeCtor, RttiName),
		FirstIndent, LaterIndent, N0, N1, DeclSet0, DeclSet1),
	output_rtti_addrs_decls(RttiTypeCtor, RttiNames,
		FirstIndent, LaterIndent, N1, N, DeclSet1, DeclSet).

:- pred output_rtti_data_decls(rtti_data::in,
	string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_rtti_data_decls(RttiData, FirstIndent, LaterIndent,
		N0, N, DeclSet0, DeclSet) -->
	( { RttiData = pseudo_type_info(type_var(_)) } ->
		% These just get represented as integers,
		% so we don't need to declare them.
		% Also rtti_data_to_name/3 does not handle this case.
		{ DeclSet = DeclSet0 },
		{ N = N0 }
	;
		{ RttiData = base_typeclass_info(InstanceModuleName, ClassId,
			InstanceStr, _) }
	->
		% rtti_data_to_name/3 does not handle this case,
		% so we need to handle it here
		output_base_typeclass_info_decl(InstanceModuleName, ClassId,
			InstanceStr, no, DeclSet0, DeclSet),
		{ N = N0 }
	;
		{ rtti_data_to_name(RttiData, RttiTypeCtor, RttiName) },
		output_rtti_addr_decls(RttiTypeCtor, RttiName,
			FirstIndent, LaterIndent, N0, N, DeclSet0, DeclSet)
	).

:- pred output_rtti_addr_decls(rtti_type_ctor::in, rtti_name::in,
	string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_rtti_addr_decls(RttiTypeCtor, RttiName, FirstIndent, LaterIndent,
		N0, N1, DeclSet0, DeclSet1) -->
	output_data_addr_decls(rtti_addr(RttiTypeCtor, RttiName),
		FirstIndent, LaterIndent, N0, N1, DeclSet0, DeclSet1).

:- pred output_addr_of_maybe_rtti_addr(rtti_type_ctor::in,
	maybe(rtti_name)::in, io__state::di, io__state::uo) is det.

output_addr_of_maybe_rtti_addr(RttiTypeCtor, MaybeRttiName) -->
	(
		{ MaybeRttiName = yes(RttiName) },
		output_addr_of_rtti_addr(RttiTypeCtor, RttiName)
	;
		{ MaybeRttiName = no },
		io__write_string("NULL")
	).

:- pred output_addr_of_maybe_rtti_addrs(rtti_type_ctor::in,
	list(maybe(rtti_name))::in, io__state::di, io__state::uo) is det.

output_addr_of_maybe_rtti_addrs(_, []) --> [].
output_addr_of_maybe_rtti_addrs(RttiTypeCtor,
		[MaybeRttiName | MaybeRttiNames]) -->
	io__write_string("\t"),
	io__write_list([MaybeRttiName | MaybeRttiNames], ",\n\t",
		output_addr_of_maybe_rtti_addr(RttiTypeCtor)),
	io__write_string("\n").

:- pred output_addr_of_rtti_addrs(rtti_type_ctor::in, list(rtti_name)::in,
	io__state::di, io__state::uo) is det.

output_addr_of_rtti_addrs(_, []) --> [].
output_addr_of_rtti_addrs(RttiTypeCtor, [RttiName | RttiNames]) -->
	io__write_string("\t"),
	io__write_list([RttiName | RttiNames], ",\n\t",
		output_addr_of_rtti_addr(RttiTypeCtor)),
	io__write_string("\n").

:- pred output_cast_addr_of_rtti_datas(string::in, list(rtti_data)::in,
	io__state::di, io__state::uo) is det.

output_cast_addr_of_rtti_datas(_, []) --> [].
output_cast_addr_of_rtti_datas(Cast, [RttiData | RttiDatas]) -->
	io__write_string("\t"),
	io__write_list([RttiData | RttiDatas], ",\n\t",
		output_cast_addr_of_rtti_data(Cast)),
	io__write_string("\n").

:- pred output_addr_of_rtti_datas(list(rtti_data)::in,
	io__state::di, io__state::uo) is det.

output_addr_of_rtti_datas([]) --> [].
output_addr_of_rtti_datas([RttiData | RttiDatas]) -->
	io__write_string("\t"),
	io__write_list([RttiData | RttiDatas], ",\n\t",
		output_addr_of_rtti_data),
	io__write_string("\n").

output_cast_addr_of_rtti_data(Cast, RttiData) -->
	io__write_string(Cast),
	output_addr_of_rtti_data(RttiData).

output_addr_of_rtti_data(RttiData) -->
	(
		{ RttiData = pseudo_type_info(type_var(VarNum)) }
	->
		% rtti_data_to_name/3 does not handle this case
		io__write_int(VarNum)
	;
		{ RttiData = base_typeclass_info(_InstanceModuleName, ClassId,
			InstanceStr, _) }
	->
		% rtti_data_to_name/3 does not handle this case
		output_base_typeclass_info_name(ClassId,
			InstanceStr)
	;
		{ rtti_data_to_name(RttiData, RttiTypeCtor, RttiName) },
		output_addr_of_rtti_addr(RttiTypeCtor, RttiName)
	).

:- pred output_addr_of_rtti_addr(rtti_type_ctor::in, rtti_name::in,
	io__state::di, io__state::uo) is det.

output_addr_of_rtti_addr(RttiTypeCtor, RttiName) -->
	%
	% If the RttiName is not an array, then
	% we need to use `&' to take its address
	%
	(
		{ rtti_name_has_array_type(RttiName) = yes }
	->
		[]
	;
		io__write_string("&")
	),
	output_rtti_addr(RttiTypeCtor, RttiName).

output_rtti_addr(RttiTypeCtor, RttiName) -->
	io__write_string(mercury_data_prefix),
	{ rtti__addr_to_string(RttiTypeCtor, RttiName, Str) },
	io__write_string(Str).

%-----------------------------------------------------------------------------%

:- pred output_maybe_quoted_string(maybe(string)::in,
	io__state::di, io__state::uo) is det.

output_maybe_quoted_string(MaybeName) -->
	(
		{ MaybeName = yes(Name) },
		io__write_string(""""),
		c_util__output_quoted_string(Name),
		io__write_string("""")
	;
		{ MaybeName = no },
		io__write_string("NULL")
	).

:- pred output_maybe_quoted_strings(list(maybe(string))::in,
	io__state::di, io__state::uo) is det.

output_maybe_quoted_strings(MaybeNames) -->
	io__write_string("\t"),
	io__write_list(MaybeNames, ",\n\t", output_maybe_quoted_string),
	io__write_string("\n").

%-----------------------------------------------------------------------------%

:- pred output_exist_locn(exist_typeinfo_locn::in,
	io__state::di, io__state::uo) is det.

output_exist_locn(Locn) -->
	(
		{ Locn = plain_typeinfo(SlotInCell) },
		io__write_string("{ "),
		io__write_int(SlotInCell),
		io__write_string(", -1 }")
	;
		{ Locn = typeinfo_in_tci(SlotInCell, SlotInTci) },
		io__write_string("{ "),
		io__write_int(SlotInCell),
		io__write_string(", "),
		io__write_int(SlotInTci),
		io__write_string(" }")
	).

:- pred output_exist_locns(list(exist_typeinfo_locn)::in,
	io__state::di, io__state::uo) is det.

output_exist_locns(Locns) -->
	io__write_string("\t"),
	io__write_list(Locns, ",\n\t", output_exist_locn),
	io__write_string("\n").

:- pred output_maybe_static_code_addr(maybe(code_addr)::in,
	io__state::di, io__state::uo) is det.

output_maybe_static_code_addr(yes(CodeAddr)) -->
	output_static_code_addr(CodeAddr).
output_maybe_static_code_addr(no) -->
	io__write_string("NULL").

:- pred output_static_code_addr(code_addr::in, io__state::di, io__state::uo)
	is det.
output_static_code_addr(CodeAddr) -->
	io__write_string("MR_MAYBE_STATIC_CODE("),
	output_code_addr(CodeAddr),
	io__write_string(")").

%-----------------------------------------------------------------------------%

:- pred rtti_name_linkage(rtti_name::in, linkage::out) is det.

rtti_name_linkage(RttiName, Linkage) :-
	(
			% ANSI/ISO C doesn't allow forward declarations
			% of static data with incomplete types (in this
			% case array types without an explicit array
			% size), so make the declarations extern.
		yes = rtti_name_has_array_type(RttiName)
	->
		Linkage = extern
	;
		Exported = rtti_name_is_exported(RttiName),
		( Exported = yes, Linkage = extern
		; Exported = no, Linkage = static
		)
        ).

%-----------------------------------------------------------------------------%

:- func this_file = string.
this_file = "rtti_out.m".

%-----------------------------------------------------------------------------%
