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

	% output a C expression holding the address of the C name of
	% the specified rtti_data
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

        % Return true iff the given type of RTTI data structure includes
	% code addresses.
:- func rtti_name_would_include_code_addr(rtti_name) = bool.

:- pred rtti_name_linkage(rtti_name::in, linkage::out) is det.

	% rtti_name_c_type(RttiName, Type, TypeSuffix):
	%	The type of the specified RttiName is given by Type
	%	and TypeSuffix, which are C code fragments suitable
	%	for use in a C declaration `<TypeName> foo <TypeSuffix>'.
	%	TypeSuffix will be "[]" if the given RttiName
	%	has an array type.
:- pred rtti_name_c_type(rtti_name::in, string::out, string::out) is det.

:- implementation.

:- import_module backend_libs__pseudo_type_info, ll_backend__code_util.
:- import_module ll_backend__llds, parse_tree__prog_out, backend_libs__c_util.
:- import_module hlds__error_util.
:- import_module libs__options, libs__globals.
:- import_module int, string, list, require, std_util.

%-----------------------------------------------------------------------------%

output_rtti_data_defn(exist_locns(RttiTypeCtor, Ordinal, Locns),
		DeclSet0, DeclSet) -->
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
output_rtti_data_defn(exist_info(RttiTypeCtor, Ordinal, Plain, InTci, Tci,
		Locns), DeclSet0, DeclSet) -->
	output_rtti_addr_decls(RttiTypeCtor, Locns, "", "", 0, _,
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
	output_rtti_addr(RttiTypeCtor, Locns),
	io__write_string("\n};\n").
output_rtti_data_defn(field_names(RttiTypeCtor, Ordinal, MaybeNames),
		DeclSet0, DeclSet) -->
	output_generic_rtti_data_defn_start(RttiTypeCtor, field_names(Ordinal),
		DeclSet0, DeclSet),
	(
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array if necessary.
		{ MaybeNames = [] }
	->
		io__write_string("= { "" };\n")
	;
		io__write_string(" = {\n"),
		output_maybe_quoted_strings(MaybeNames),
		io__write_string("};\n")
	).
output_rtti_data_defn(field_types(RttiTypeCtor, Ordinal, Types),
		DeclSet0, DeclSet) -->
	output_rtti_datas_decls(Types, "", "", 0, _, DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor, field_types(Ordinal),
		DeclSet1, DeclSet),
	(
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array if necessary.
		{ Types = [] }
	->
		io__write_string("= { NULL };\n")
	;
		io__write_string(" = {\n"),
		output_addr_of_rtti_datas(Types),
		io__write_string("};\n")
	).
output_rtti_data_defn(reserved_addrs(RttiTypeCtor, ReservedAddrs),
		DeclSet0, DeclSet) -->
	output_generic_rtti_data_defn_start(RttiTypeCtor, reserved_addrs,
		DeclSet0, DeclSet),
	(
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array if necessary.
		{ ReservedAddrs = [] }
	->
		io__write_string("= { NULL };\n")
	;
		io__write_string(" = {\n"),
		io__write_list(ReservedAddrs, ",\n\t", output_reserved_address),
		io__write_string("\n};\n")
	).
output_rtti_data_defn(reserved_addr_functors(RttiTypeCtor, FunctorDescs),
		DeclSet0, DeclSet) -->
	output_rtti_addrs_decls(RttiTypeCtor, FunctorDescs, "", "", 0, _,
		DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		reserved_addr_functors, DeclSet1, DeclSet),
	(
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array if necessary.
		{ FunctorDescs = [] }
	->
		io__write_string("= { NULL };\n")
	;
		io__write_string(" = {\n"),
		output_addr_of_rtti_addrs(RttiTypeCtor, FunctorDescs),
		io__write_string("};\n")
	).
output_rtti_data_defn(enum_functor_desc(RttiTypeCtor, FunctorName, Ordinal),
		DeclSet0, DeclSet) -->
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		enum_functor_desc(Ordinal), DeclSet0, DeclSet),
	io__write_string(" = {\n\t"""),
	c_util__output_quoted_string(FunctorName),
	io__write_string(""",\n\t"),
	io__write_int(Ordinal),
	io__write_string("\n};\n").
output_rtti_data_defn(notag_functor_desc(RttiTypeCtor, FunctorName, ArgType,
		MaybeArgName), DeclSet0, DeclSet) -->
	output_rtti_data_decls(ArgType, "", "", 0, _, DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor, notag_functor_desc,
		DeclSet1, DeclSet),
	io__write_string(" = {\n\t"""),
	c_util__output_quoted_string(FunctorName),
	io__write_string(""",\n\t "),
	output_addr_of_rtti_data(ArgType),
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
output_rtti_data_defn(du_functor_desc(RttiTypeCtor, FunctorName, Ptag, Stag,
		Locn, Ordinal, Arity, ContainsVarBitVector, MaybeArgTypes,
		MaybeNames, MaybeExist),
		DeclSet0, DeclSet) -->
	(
		{ MaybeArgTypes = yes(ArgTypes) },
		output_rtti_addr_decls(RttiTypeCtor, ArgTypes, "", "", 0, _,
			DeclSet0, DeclSet1)
	;
		{ MaybeArgTypes = no },
		{ DeclSet1 = DeclSet0 }
	),
	(
		{ MaybeNames = yes(NamesInfo1) },
		output_rtti_addr_decls(RttiTypeCtor, NamesInfo1, "", "",
			0, _, DeclSet1, DeclSet2)
	;
		{ MaybeNames = no },
		{ DeclSet2 = DeclSet1 }
	),
	(
		{ MaybeExist = yes(ExistInfo1) },
		output_rtti_addr_decls(RttiTypeCtor, ExistInfo1, "", "",
			0, _, DeclSet2, DeclSet3)
	;
		{ MaybeExist = no },
		{ DeclSet3 = DeclSet2 }
	),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		du_functor_desc(Ordinal), DeclSet3, DeclSet),
	io__write_string(" = {\n\t"""),
	c_util__output_quoted_string(FunctorName),
	io__write_string(""",\n\t"),
	io__write_int(Arity),
	io__write_string(",\n\t"),
	io__write_int(ContainsVarBitVector),
	io__write_string(",\n\t"),
	{ rtti__sectag_locn_to_string(Locn, LocnStr) },
	io__write_string(LocnStr),
	io__write_string(",\n\t"),
	io__write_int(Ptag),
	io__write_string(",\n\t"),
	io__write_int(Stag),
	io__write_string(",\n\t"),
	io__write_int(Ordinal),
	io__write_string(",\n\t"),
	io__write_string("(MR_PseudoTypeInfo *) "), % cast away const
	(
		{ MaybeArgTypes = yes(ArgTypes2) },
		output_addr_of_rtti_addr(RttiTypeCtor, ArgTypes2)
	;
		{ MaybeArgTypes = no },
		io__write_string("NULL")
	),
	io__write_string(",\n\t"),
	(
		{ MaybeNames = yes(NamesInfo2) },
		output_rtti_addr(RttiTypeCtor, NamesInfo2)
	;
		{ MaybeNames = no },
		io__write_string("NULL")
	),
	io__write_string(",\n\t"),
	(
		{ MaybeExist = yes(ExistInfo2) },
		output_addr_of_rtti_addr(RttiTypeCtor, ExistInfo2)
	;
		{ MaybeExist = no },
		io__write_string("NULL")
	),
	io__write_string("\n};\n").
output_rtti_data_defn(reserved_addr_functor_desc(RttiTypeCtor, FunctorName,
		Ordinal, ReservedAddr), DeclSet0, DeclSet) -->
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		reserved_addr_functor_desc(Ordinal), DeclSet0, DeclSet),
	io__write_string(" = {\n\t"""),
	c_util__output_quoted_string(FunctorName),
	io__write_string(""",\n\t"),
	io__write_int(Ordinal),
	io__write_string(",\n\t"),
	output_reserved_address(ReservedAddr),
	io__write_string("\n};\n").
output_rtti_data_defn(enum_name_ordered_table(RttiTypeCtor, Functors),
		DeclSet0, DeclSet) -->
	output_rtti_addrs_decls(RttiTypeCtor, Functors, "", "", 0, _,
		DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		enum_name_ordered_table, DeclSet1, DeclSet),
	io__write_string(" = {\n"),
	output_addr_of_rtti_addrs(RttiTypeCtor, Functors),
	io__write_string("};\n").
output_rtti_data_defn(enum_value_ordered_table(RttiTypeCtor, Functors),
		DeclSet0, DeclSet) -->
	output_rtti_addrs_decls(RttiTypeCtor, Functors, "", "", 0, _,
		DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		enum_value_ordered_table, DeclSet1, DeclSet),
	io__write_string(" = {\n"),
	output_addr_of_rtti_addrs(RttiTypeCtor, Functors),
	io__write_string("};\n").
output_rtti_data_defn(du_name_ordered_table(RttiTypeCtor, Functors),
		DeclSet0, DeclSet) -->
	output_rtti_addrs_decls(RttiTypeCtor, Functors, "", "", 0, _,
		DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		du_name_ordered_table, DeclSet1, DeclSet),
	io__write_string(" = {\n"),
	output_addr_of_rtti_addrs(RttiTypeCtor, Functors),
	io__write_string("};\n").
output_rtti_data_defn(du_stag_ordered_table(RttiTypeCtor, Ptag, Sharers),
		DeclSet0, DeclSet) -->
	output_rtti_addrs_decls(RttiTypeCtor, Sharers, "", "", 0, _,
		DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		du_stag_ordered_table(Ptag), DeclSet1, DeclSet),
	io__write_string(" = {\n"),
	output_addr_of_rtti_addrs(RttiTypeCtor, Sharers),
	io__write_string("\n};\n").
output_rtti_data_defn(du_ptag_ordered_table(RttiTypeCtor, PtagLayouts),
		DeclSet0, DeclSet) -->
	output_ptag_layout_decls(PtagLayouts, RttiTypeCtor,
		DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		du_ptag_ordered_table, DeclSet1, DeclSet),
	io__write_string(" = {\n"),
	globals__io_lookup_bool_option(reserve_tag, ReserveTag),
	(
		{ ReserveTag = yes }
	->
			% Output a dummy ptag definition for the 
			% reserved tag first
		output_dummy_ptag_layout_defn
	;
		[]
	),
	output_ptag_layout_defns(PtagLayouts, RttiTypeCtor),
	io__write_string("\n};\n").
output_rtti_data_defn(reserved_addr_table(RttiTypeCtor,
		NumNumericReservedAddrs, NumSymbolicReservedAddrs,
		SymbolicReservedAddrs, ReservedAddrFunctorDescs,
		DuFunctorLayout), DeclSet0, DeclSet) -->
	output_rtti_addrs_decls(RttiTypeCtor, [SymbolicReservedAddrs,
			DuFunctorLayout, ReservedAddrFunctorDescs],
			"", "", 0, _, DeclSet0, DeclSet1),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		reserved_addr_table, DeclSet1, DeclSet),
	io__write_string(" = {\n\t"),
	io__write_int(NumNumericReservedAddrs),
	io__write_string(",\n\t"),
	io__write_int(NumSymbolicReservedAddrs),
	io__write_string(",\n\t"),
	output_rtti_addr(RttiTypeCtor, SymbolicReservedAddrs),
	io__write_string(",\n\t"),
	output_rtti_addr(RttiTypeCtor, ReservedAddrFunctorDescs),
	io__write_string(",\n\t"),
	output_rtti_addr(RttiTypeCtor, DuFunctorLayout),
	io__write_string("\n};\n").
output_rtti_data_defn(type_ctor_info(RttiTypeCtor, Unify, Compare, CtorRep,
		Version, NumPtags, NumFunctors, FunctorsInfo, LayoutInfo),
		DeclSet0, DeclSet) -->
	{ UnifyCA   = make_maybe_code_addr(Unify) },
	{ CompareCA = make_maybe_code_addr(Compare) },
	{ MaybeCodeAddrs = [UnifyCA, CompareCA] },
	{ CodeAddrs = list__filter_map(func(yes(CA)) = CA is semidet,
		MaybeCodeAddrs) },
	output_code_addrs_decls(CodeAddrs, "", "", 0, _, DeclSet0, DeclSet1),
	output_functors_info_decl(RttiTypeCtor, FunctorsInfo,
		DeclSet1, DeclSet2),
	output_layout_info_decl(RttiTypeCtor, LayoutInfo, DeclSet2, DeclSet3),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		type_ctor_info, DeclSet3, DeclSet),
	io__write_string(" = {\n\t"),
	{ RttiTypeCtor = rtti_type_ctor(Module, Type, TypeArity) },
	io__write_int(TypeArity),
	io__write_string(",\n\t"),
	io__write_int(Version),
	io__write_string(",\n\t"),
	{ rtti__type_ctor_rep_to_string(CtorRep, CtorRepStr) },
	io__write_string(CtorRepStr),
	io__write_string(",\n\t"),
	io__write_int(NumPtags),
	io__write_string(",\n\t"),
	output_maybe_static_code_addr(UnifyCA),
	io__write_string(",\n\t"),
	output_maybe_static_code_addr(CompareCA),
	io__write_string(",\n\t"""),
	{ prog_out__sym_name_to_string(Module, ModuleName) },
	c_util__output_quoted_string(ModuleName),
	io__write_string(""",\n\t"""),
	c_util__output_quoted_string(Type),
	io__write_string(""",\n\t"),
	(
		{ FunctorsInfo = enum_functors(EnumFunctorsInfo) },
		io__write_string("{ (void *) "),
		output_rtti_addr(RttiTypeCtor, EnumFunctorsInfo),
		io__write_string(" }")
	;
		{ FunctorsInfo = notag_functors(NotagFunctorsInfo) },
		io__write_string("{ (void *) &"),
		output_rtti_addr(RttiTypeCtor, NotagFunctorsInfo),
		io__write_string(" }")
	;
		{ FunctorsInfo = du_functors(DuFunctorsInfo) },
		io__write_string("{ (void *) "),
		output_rtti_addr(RttiTypeCtor, DuFunctorsInfo),
		io__write_string(" }")
	;
		{ FunctorsInfo = no_functors },
		io__write_string("{ 0 }")
	),
	io__write_string(",\n\t"),
	(
		{ LayoutInfo = enum_layout(EnumLayoutInfo) },
		io__write_string("{ (void *) "),
		output_rtti_addr(RttiTypeCtor, EnumLayoutInfo),
		io__write_string(" }")
	;
		{ LayoutInfo = notag_layout(NotagLayoutInfo) },
		io__write_string("{ (void *) &"),
		output_rtti_addr(RttiTypeCtor, NotagLayoutInfo),
		io__write_string(" }")
	;
		{ LayoutInfo = du_layout(DuLayoutInfo) },
		io__write_string("{ (void *) "),
		output_rtti_addr(RttiTypeCtor, DuLayoutInfo),
		io__write_string(" }")
	;
		{ LayoutInfo = reserved_addr_layout(RaLayoutInfo) },
		io__write_string("{ (void *) &"),
		output_rtti_addr(RttiTypeCtor, RaLayoutInfo),
		io__write_string(" }")
	;
		{ LayoutInfo = equiv_layout(EquivTypeInfo) },
		io__write_string("{ (void *) "),
		output_addr_of_rtti_data(EquivTypeInfo),
		io__write_string(" }")
	;
		{ LayoutInfo = no_layout },
		io__write_string("{ 0 }")
	),
	io__write_string(",\n\t"),
	io__write_int(NumFunctors),
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
output_rtti_data_defn(base_typeclass_info(InstanceModuleName, ClassId,
		InstanceString, BaseTypeClassInfo), DeclSet0, DeclSet) -->
	output_base_typeclass_info_defn(InstanceModuleName, ClassId,
		InstanceString, BaseTypeClassInfo, DeclSet0, DeclSet).
output_rtti_data_defn(pseudo_type_info(Pseudo), DeclSet0, DeclSet) -->
	output_pseudo_type_info_defn(Pseudo, DeclSet0, DeclSet).

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

:- func make_maybe_code_addr(maybe(rtti_proc_label)) = maybe(code_addr).
make_maybe_code_addr(no) = no.
make_maybe_code_addr(yes(ProcLabel)) = yes(make_code_addr(ProcLabel)).

:- func make_code_addr(rtti_proc_label) = code_addr.
make_code_addr(ProcLabel) = CodeAddr :-
	code_util__make_entry_label_from_rtti(ProcLabel, no, CodeAddr).

:- pred output_pseudo_type_info_defn(pseudo_type_info, decl_set, decl_set,
		io__state, io__state).
:- mode output_pseudo_type_info_defn(in, in, out, di, uo) is det.

output_pseudo_type_info_defn(type_var(_), DeclSet, DeclSet) --> [].
output_pseudo_type_info_defn(type_ctor_info(_), DeclSet, DeclSet) --> [].
output_pseudo_type_info_defn(TypeInfo, DeclSet0, DeclSet) -->
	{ TypeInfo = type_info(RttiTypeCtor, ArgTypes) },
	{ TypeCtorRttiData = pseudo_type_info(type_ctor_info(RttiTypeCtor)) },
	{ ArgRttiDatas = list__map(func(P) = pseudo_type_info(P), ArgTypes) },
	output_rtti_data_decls(TypeCtorRttiData, "", "", 0, _, DeclSet0, DeclSet1),
	output_rtti_datas_decls(ArgRttiDatas, "", "", 0, _, DeclSet1, DeclSet2),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		pseudo_type_info(TypeInfo), DeclSet2, DeclSet),
	io__write_string(" = {\n\t&"),
	output_rtti_addr(RttiTypeCtor, type_ctor_info),
	io__write_string(",\n{"),
	output_addr_of_rtti_datas(ArgRttiDatas),
	io__write_string("}};\n").
output_pseudo_type_info_defn(HO_TypeInfo, DeclSet0, DeclSet) -->
	{ HO_TypeInfo = higher_order_type_info(RttiTypeCtor, Arity,
		ArgTypes) },
	{ TypeCtorRttiData = pseudo_type_info(type_ctor_info(RttiTypeCtor)) },
	{ ArgRttiDatas = list__map(func(P) = pseudo_type_info(P), ArgTypes) },
	output_rtti_data_decls(TypeCtorRttiData, "", "", 0, _,
		DeclSet0, DeclSet1),
	output_rtti_datas_decls(ArgRttiDatas, "", "", 0, _, DeclSet1, DeclSet2),
	output_generic_rtti_data_defn_start(RttiTypeCtor,
		pseudo_type_info(HO_TypeInfo), DeclSet2, DeclSet),
	io__write_string(" = {\n\t&"),
	output_rtti_addr(RttiTypeCtor, type_ctor_info),
	io__write_string(",\n\t"),
	io__write_int(Arity),
	io__write_string(",\n{"),
	output_addr_of_rtti_datas(ArgRttiDatas),
	io__write_string("}};\n").

:- pred output_functors_info_decl(rtti_type_ctor::in,
	type_ctor_functors_info::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_functors_info_decl(RttiTypeCtor, enum_functors(EnumFunctorsInfo),
		DeclSet0, DeclSet) -->
	output_generic_rtti_data_decl(RttiTypeCtor, EnumFunctorsInfo,
		DeclSet0, DeclSet).
output_functors_info_decl(RttiTypeCtor, notag_functors(NotagFunctorsInfo),
		DeclSet0, DeclSet) -->
	output_generic_rtti_data_decl(RttiTypeCtor, NotagFunctorsInfo,
		DeclSet0, DeclSet).
output_functors_info_decl(RttiTypeCtor, du_functors(DuFunctorsInfo),
		DeclSet0, DeclSet) -->
	output_generic_rtti_data_decl(RttiTypeCtor, DuFunctorsInfo,
		DeclSet0, DeclSet).
output_functors_info_decl(_RttiTypeCtor, no_functors, DeclSet, DeclSet) --> [].

:- pred output_layout_info_decl(rtti_type_ctor::in, type_ctor_layout_info::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_layout_info_decl(RttiTypeCtor, enum_layout(EnumLayoutInfo),
		DeclSet0, DeclSet) -->
	output_generic_rtti_data_decl(RttiTypeCtor, EnumLayoutInfo,
		DeclSet0, DeclSet).
output_layout_info_decl(RttiTypeCtor, notag_layout(NotagLayoutInfo),
		DeclSet0, DeclSet) -->
	output_generic_rtti_data_decl(RttiTypeCtor, NotagLayoutInfo,
		DeclSet0, DeclSet).
output_layout_info_decl(RttiTypeCtor, du_layout(DuLayoutInfo),
		DeclSet0, DeclSet) -->
	output_generic_rtti_data_decl(RttiTypeCtor, DuLayoutInfo,
		DeclSet0, DeclSet).
output_layout_info_decl(RttiTypeCtor, reserved_addr_layout(RaLayoutInfo),
		DeclSet0, DeclSet) -->
	output_generic_rtti_data_decl(RttiTypeCtor, RaLayoutInfo,
		DeclSet0, DeclSet).
output_layout_info_decl(_RttiTypeCtor, equiv_layout(EquivRttiData),
		DeclSet0, DeclSet) -->
	output_rtti_data_decl(EquivRttiData, DeclSet0, DeclSet).
output_layout_info_decl(_RttiTypeCtor, no_layout, DeclSet, DeclSet) --> [].

:- pred output_ptag_layout_decls(list(du_ptag_layout)::in, rtti_type_ctor::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_ptag_layout_decls([], _, DeclSet, DeclSet) --> [].
output_ptag_layout_decls([DuPtagLayout | DuPtagLayouts], RttiTypeCtor,
		DeclSet0, DeclSet) -->
	{ DuPtagLayout = du_ptag_layout(_, _, Descriptors) },
	output_rtti_addr_decls(RttiTypeCtor, Descriptors, "", "", 0, _,
		DeclSet0, DeclSet1),
	output_ptag_layout_decls(DuPtagLayouts, RttiTypeCtor,
		DeclSet1, DeclSet).

:- pred output_ptag_layout_defns(list(du_ptag_layout)::in, rtti_type_ctor::in,
	io__state::di, io__state::uo) is det.

output_ptag_layout_defns([], _) --> [].
output_ptag_layout_defns([DuPtagLayout | DuPtagLayouts], RttiTypeCtor) -->
	{ DuPtagLayout = du_ptag_layout(NumSharers, Locn, Descriptors) },
	io__write_string("\t{ "),
	io__write_int(NumSharers),
	io__write_string(", "),
	{ rtti__sectag_locn_to_string(Locn, LocnStr) },
	io__write_string(LocnStr),
	io__write_string(",\n\t"),
	output_rtti_addr(RttiTypeCtor, Descriptors),
	( { DuPtagLayouts = [] } ->
		io__write_string(" }\n")
	;
		io__write_string(" },\n")
	),
	output_ptag_layout_defns(DuPtagLayouts, RttiTypeCtor).

	% Output a `dummy' ptag layout, for use by tags that aren't *real*
	% tags, such as the tag reserved when --reserve-tag is on.
	%
	% XXX Note that if one of these dummy ptag definitions is actually
	% accessed by the Mercury runtime, or the construct/deconstruct
	% code in library/std_util.m, the result will be undefined.
	% This should be fixed by adding a MR_SECTAG_DUMMY and handling it
	% gracefully.
:- pred output_dummy_ptag_layout_defn(io__state::di, io__state::uo) is det.

output_dummy_ptag_layout_defn -->
	io__write_string("\t{ 0, MR_SECTAG_VARIABLE, NULL },\n").

%-----------------------------------------------------------------------------%

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
	{ c_data_linkage_string(Globals, Linkage, BeingDefined, LinkageStr) },
	io__write_string(LinkageStr),

	{ InclCodeAddr = rtti_name_would_include_code_addr(RttiName) },
	{ c_data_const_string(Globals, InclCodeAddr, ConstStr) },
	io__write_string(ConstStr),

	{ rtti_name_c_type(RttiName, CType, Suffix) },
	c_util__output_quoted_string(CType),
	io__write_string(" "),
	OutputName,
	io__write_string(Suffix).

:- pred output_rtti_type_decl(rtti_name::in, io__state::di, io__state::uo)
	is det.
output_rtti_type_decl(RttiName) -->
	(
		%
		% Each pseudo-type-info may have a different type,
		% depending on what kind of pseudo-type-info it is,
		% and also on its arity.
		% We need to declare that type here.
		%
		{
		  RttiName = pseudo_type_info(type_info(_, ArgTypes)),
		  TypeNameBase = "MR_FO_PseudoTypeInfo_Struct",
		  DefineType = "MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT"
		;
		  RttiName = pseudo_type_info(higher_order_type_info(_, _,
		  		ArgTypes)),
	 	  TypeNameBase = "MR_HO_PseudoTypeInfo_Struct",
		  DefineType = "MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT"
		}
	->
		{ NumArgTypes = list__length(ArgTypes) },
		{ Template = 
"#ifndef %s%d_GUARD
#define %s%d_GUARD
%s(%s%d, %d);
#endif
"		},
		io__format(Template, [
			s(TypeNameBase), i(NumArgTypes),
			s(TypeNameBase), i(NumArgTypes),
			s(DefineType), s(TypeNameBase),
			i(NumArgTypes), i(NumArgTypes)
		])
	;
		[]
	).

%-----------------------------------------------------------------------------%

rtti_out__init_rtti_data_if_nec(Data) -->
	(
		{ Data = type_ctor_info(RttiTypeCtor, _,_,_,_,_,_,_,_) }
	->
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
		{ Data = type_ctor_info(RttiTypeCtor, _,_,_,_,_,_,_,_) }
	->
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

:- pred output_addr_of_rtti_datas(list(rtti_data)::in,
	io__state::di, io__state::uo) is det.

output_addr_of_rtti_datas([]) --> [].
output_addr_of_rtti_datas([RttiData | RttiDatas]) -->
	io__write_string("\t"),
	io__write_list([RttiData | RttiDatas], ",\n\t",
		output_addr_of_rtti_data),
	io__write_string("\n").

output_addr_of_rtti_data(RttiData) -->
	( { RttiData = pseudo_type_info(type_var(VarNum)) } ->
		% rtti_data_to_name/3 does not handle this case
		io__write_string("(MR_PseudoTypeInfo) "),
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
	% The various different kinds of pseudotypeinfos
	% each have different types, but really we treat
	% them like a union rather than as separate types,
	% so here we need to cast all such constants to
	% a single type MR_PseudoTypeInfo.
	%
	(
		{ RttiName = pseudo_type_info(_) }
	->
		io__write_string("(MR_PseudoTypeInfo) ")
	;
		[]
	),
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

rtti_name_would_include_code_addr(exist_locns(_)) =               no.
rtti_name_would_include_code_addr(exist_info(_)) =                no.
rtti_name_would_include_code_addr(field_names(_)) =               no.
rtti_name_would_include_code_addr(field_types(_)) =               no.
rtti_name_would_include_code_addr(reserved_addrs) =               no.
rtti_name_would_include_code_addr(reserved_addr_functors) =       no.
rtti_name_would_include_code_addr(enum_functor_desc(_)) =         no.
rtti_name_would_include_code_addr(notag_functor_desc) =           no.
rtti_name_would_include_code_addr(du_functor_desc(_)) =           no.
rtti_name_would_include_code_addr(reserved_addr_functor_desc(_)) = no.
rtti_name_would_include_code_addr(enum_name_ordered_table) =      no.
rtti_name_would_include_code_addr(enum_value_ordered_table) =     no.
rtti_name_would_include_code_addr(du_name_ordered_table) =        no.
rtti_name_would_include_code_addr(du_stag_ordered_table(_)) =     no.
rtti_name_would_include_code_addr(du_ptag_ordered_table) =        no.
rtti_name_would_include_code_addr(reserved_addr_table) =          no.
rtti_name_would_include_code_addr(type_ctor_info) =               yes.
rtti_name_would_include_code_addr(base_typeclass_info(_, _, _)) = yes.
rtti_name_would_include_code_addr(pseudo_type_info(Pseudo)) =
		pseudo_type_info_would_incl_code_addr(Pseudo).
rtti_name_would_include_code_addr(type_hashcons_pointer) =        no.

:- func pseudo_type_info_would_incl_code_addr(pseudo_type_info) = bool.

pseudo_type_info_would_incl_code_addr(type_var(_))			= no.
pseudo_type_info_would_incl_code_addr(type_ctor_info(_))		= yes.
pseudo_type_info_would_incl_code_addr(type_info(_, _))			= no.
pseudo_type_info_would_incl_code_addr(higher_order_type_info(_, _, _))	= no.

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

rtti_name_c_type(exist_locns(_),           "MR_DuExistLocn", "[]").
rtti_name_c_type(exist_info(_),            "MR_DuExistInfo", "").
rtti_name_c_type(field_names(_),           "MR_ConstString", "[]").
rtti_name_c_type(field_types(_),           "MR_PseudoTypeInfo", "[]").
rtti_name_c_type(reserved_addrs,           "/* const */ void *", "[]").
rtti_name_c_type(reserved_addr_functors,   "MR_ReservedAddrFunctorDesc *",
						"[]").
rtti_name_c_type(enum_functor_desc(_),     "MR_EnumFunctorDesc", "").
rtti_name_c_type(notag_functor_desc,       "MR_NotagFunctorDesc", "").
rtti_name_c_type(du_functor_desc(_),       "MR_DuFunctorDesc", "").
rtti_name_c_type(reserved_addr_functor_desc(_), "MR_ReservedAddrFunctorDesc",
						"").
rtti_name_c_type(enum_name_ordered_table,  "MR_EnumFunctorDesc *", "[]").
rtti_name_c_type(enum_value_ordered_table, "MR_EnumFunctorDesc *", "[]").
rtti_name_c_type(du_name_ordered_table,    "MR_DuFunctorDesc *", "[]").
rtti_name_c_type(du_stag_ordered_table(_), "MR_DuFunctorDesc *", "[]").
rtti_name_c_type(du_ptag_ordered_table,    "MR_DuPtagLayout", "[]").
rtti_name_c_type(reserved_addr_table,      "MR_ReservedAddrTypeLayout", "").
rtti_name_c_type(type_ctor_info,           "struct MR_TypeCtorInfo_Struct",
						"").
rtti_name_c_type(base_typeclass_info(_, _, _), "MR_Code *", "[]").
rtti_name_c_type(pseudo_type_info(Pseudo), TypePrefix, TypeSuffix) :-
	pseudo_type_info_name_c_type(Pseudo, TypePrefix, TypeSuffix).
rtti_name_c_type(type_hashcons_pointer,    "union MR_TableNode_Union **", "").

:- pred pseudo_type_info_name_c_type(pseudo_type_info, string, string).
:- mode pseudo_type_info_name_c_type(in, out, out) is det.

pseudo_type_info_name_c_type(type_var(_), _, _) :-
	% we use small integers to represent type_vars,
	% rather than pointers, so there is no pointed-to type
	error("rtti_name_c_type: type_var").
pseudo_type_info_name_c_type(type_ctor_info(_),
		"struct MR_TypeCtorInfo_Struct", "").
pseudo_type_info_name_c_type(type_info(_TypeCtor, ArgTypes),
		TypeInfoStruct, "") :-
	TypeInfoStruct = string__format("struct MR_FO_PseudoTypeInfo_Struct%d",
		[i(list__length(ArgTypes))]).
pseudo_type_info_name_c_type(higher_order_type_info(_TypeCtor, _Arity,
		ArgTypes), TypeInfoStruct, "") :-
	TypeInfoStruct = string__format("struct MR_HO_PseudoTypeInfo_Struct%d",
		[i(list__length(ArgTypes))]).

%-----------------------------------------------------------------------------%

:- func this_file = string.
this_file = "rtti_out.m".

:- end_module rtti_out.

%-----------------------------------------------------------------------------%
