%---------------------------------------------------------------------------%
% Copyright (C) 1996-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: type_ctor_info.m.
% Authors: zs, trd.
%
% This module is responsible for the generation of the static type_ctor_info
% structures of the types defined by the current module. This includes the
% RTTI data structures that describe the representation of each type.
% These structures form the type_ctor_rep, type_num_functors, type_functors
% and type_layout fields of a type_ctor_info. This RTTI information is
% used for several purposes: examples include deep copy, tabling, and functor,
% arg and their cousins.
%
% Since it is possible for the type_ctor_info of a type local to the module
% not to be referred to anywhere in the module (and therefore, not to be
% referred to anywhere in the program), this module works in two stages.
% In the first stage, it inserts type_ctor_gen_info structures describing the
% type_ctor_infos of all the locally-defined types into the HLDS; some of
% these type_ctor_gen_infos are later eliminated by dead_proc_elim.m. The
% second stage then generates low-level descriptions of type_ctor_infos
% for LLDS (or later MLDS) from the surviving type_ctor_gen_infos.
%
% The representation we build is designed to be independent of whether
% the compiler is generating LLDS or MLDS. However, at the moment, we
% still generate some LLDS rvals to represent typeinfos and pseudotypeinfos.
% These `create' rvals are expected to be removed by llds_common.m to
% create static structures.
%
% The documentation of the data structures built in this module is in
% runtime/mercury_type_info.h; that file also contains a list of all
% the files that depend on these data structures.
%
% WARNING: if you change this module, you will probably need to also
% change ml_base_type_info.m, which does the same thing for the MLDS
% back-end.
%
%---------------------------------------------------------------------------%

:- module type_ctor_info.

:- interface.

:- import_module hlds_module, llds.
:- import_module list.

:- pred type_ctor_info__generate_hlds(module_info::in, module_info::out)
	is det.

:- pred type_ctor_info__generate_llds(module_info::in, module_info::out,
	list(comp_gen_c_data)::out) is det.

:- implementation.

:- import_module rtti, pseudo_type_info.
:- import_module hlds_data, hlds_pred, hlds_out.
:- import_module make_tags, prog_data, prog_util, prog_out.
:- import_module code_util, special_pred, type_util, globals, options.
:- import_module builtin_ops.

:- import_module bool, string, int, map, std_util, assoc_list, require.
:- import_module term.

%---------------------------------------------------------------------------%

type_ctor_info__generate_hlds(ModuleInfo0, ModuleInfo) :-
	module_info_name(ModuleInfo0, ModuleName),
	module_info_types(ModuleInfo0, TypeTable),
	map__keys(TypeTable, TypeIds),
	type_ctor_info__gen_type_ctor_gen_infos(TypeIds, TypeTable, ModuleName,
		ModuleInfo0, TypeCtorGenInfos),
	module_info_set_type_ctor_gen_infos(ModuleInfo0, TypeCtorGenInfos,
		ModuleInfo).

	% Given a list of the ids of all the types in the type table,
	% find the types defined in this module, and return a type_ctor_gen_info
	% for each.

:- pred type_ctor_info__gen_type_ctor_gen_infos(list(type_id)::in,
	type_table::in, module_name::in, module_info::in,
	list(type_ctor_gen_info)::out) is det.

type_ctor_info__gen_type_ctor_gen_infos([], _, _, _, []).
type_ctor_info__gen_type_ctor_gen_infos([TypeId | TypeIds], TypeTable,
		ModuleName, ModuleInfo, TypeCtorGenInfos) :-
	type_ctor_info__gen_type_ctor_gen_infos(TypeIds, TypeTable, ModuleName,
		ModuleInfo, TypeCtorGenInfos1),
	TypeId = SymName - TypeArity,
	(
		SymName = qualified(TypeModuleName, TypeName),
		( 
			TypeModuleName = ModuleName,
			map__lookup(TypeTable, TypeId, TypeDefn),
			hlds_data__get_type_defn_body(TypeDefn, TypeBody),
			TypeBody \= abstract_type,
			\+ type_id_has_hand_defined_rtti(TypeId)
		->
			type_ctor_info__gen_type_ctor_gen_info(TypeId,
				TypeName, TypeArity, TypeDefn,
				ModuleName, ModuleInfo, TypeCtorGenInfo),
			TypeCtorGenInfos = [TypeCtorGenInfo | TypeCtorGenInfos1]
		;
			TypeCtorGenInfos = TypeCtorGenInfos1
		)
	;
		SymName = unqualified(TypeName),
		string__append_list(["unqualified type ", TypeName,
			"found in type_ctor_info"], Msg),
		error(Msg)
	).

:- pred type_ctor_info__gen_type_ctor_gen_info(type_id::in, string::in,
	int::in, hlds_type_defn::in, module_name::in, module_info::in,
	type_ctor_gen_info::out) is det.

type_ctor_info__gen_type_ctor_gen_info(TypeId, TypeName, TypeArity, TypeDefn,
		ModuleName, ModuleInfo, TypeCtorGenInfo) :-
	hlds_data__get_type_defn_status(TypeDefn, Status),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, special_preds, SpecialPreds),
	(
		SpecialPreds = yes,
		module_info_get_special_pred_map(ModuleInfo, SpecMap),

		map__lookup(SpecMap, unify - TypeId, UnifyPredId),
		special_pred_mode_num(unify, UnifyProcInt),
		proc_id_to_int(UnifyProcId, UnifyProcInt),
		MaybeUnify = yes(proc(UnifyPredId, UnifyProcId)),

		map__lookup(SpecMap, index - TypeId, IndexPredId),
		special_pred_mode_num(index, IndexProcInt),
		proc_id_to_int(IndexProcId, IndexProcInt),
		MaybeIndex = yes(proc(IndexPredId, IndexProcId)),

		map__lookup(SpecMap, compare - TypeId, ComparePredId),
		special_pred_mode_num(compare, CompareProcInt),
		proc_id_to_int(CompareProcId, CompareProcInt),
		MaybeCompare = yes(proc(ComparePredId, CompareProcId))
	;
		SpecialPreds = no,
		MaybeUnify = no,
		MaybeIndex = no,
		MaybeCompare = no
	),
	TypeCtorGenInfo = type_ctor_gen_info(TypeId, ModuleName,
		TypeName, TypeArity, Status, TypeDefn,
		MaybeUnify, MaybeIndex, MaybeCompare,
		no, no, no).

%---------------------------------------------------------------------------%

type_ctor_info__generate_llds(ModuleInfo0, ModuleInfo, Tables) :-
	module_info_type_ctor_gen_infos(ModuleInfo0, TypeCtorGenInfos),
	type_ctor_info__construct_type_ctor_infos(TypeCtorGenInfos,
		ModuleInfo0, ModuleInfo, [], Dynamic0, [], Static0),
	list__map(llds__wrap_rtti_data, Dynamic0, Dynamic),
	list__map(llds__wrap_rtti_data, Static0, Static),
	list__append(Dynamic, Static, Tables).

:- pred type_ctor_info__construct_type_ctor_infos(
	list(type_ctor_gen_info)::in, module_info::in, module_info::out,
	list(rtti_data)::in, list(rtti_data)::out,
	list(rtti_data)::in, list(rtti_data)::out) is det.

type_ctor_info__construct_type_ctor_infos([], ModuleInfo, ModuleInfo,
		Dynamic, Dynamic, Static, Static).
type_ctor_info__construct_type_ctor_infos(
		[TypeCtorGenInfo | TypeCtorGenInfos], ModuleInfo0, ModuleInfo,
		Dynamic0, Dynamic, Static0, Static) :-
	type_ctor_info__construct_type_ctor_info(TypeCtorGenInfo,
		ModuleInfo0, ModuleInfo1, TypeCtorCModule, TypeCtorTables),
	Dynamic1 = [TypeCtorCModule | Dynamic0],
	list__append(TypeCtorTables, Static0, Static1),
	type_ctor_info__construct_type_ctor_infos(TypeCtorGenInfos,
		ModuleInfo1, ModuleInfo, Dynamic1, Dynamic, Static1, Static).

:- pred type_ctor_info__construct_type_ctor_info(type_ctor_gen_info::in,
	module_info::in, module_info::out,
	rtti_data::out, list(rtti_data)::out) is det.

type_ctor_info__construct_type_ctor_info(TypeCtorGenInfo,
		ModuleInfo0, ModuleInfo, TypeCtorData, TypeCtorTables) :-
	TypeCtorGenInfo = type_ctor_gen_info(_TypeId, ModuleName, TypeName,
		TypeArity, _Status, HldsDefn,
		MaybeUnify, MaybeIndex, MaybeCompare,
		MaybeSolver, MaybeInit, MaybePretty),
	type_ctor_info__make_pred_addr(MaybeUnify,   ModuleInfo, Unify),
	type_ctor_info__make_pred_addr(MaybeIndex,   ModuleInfo, Index),
	type_ctor_info__make_pred_addr(MaybeCompare, ModuleInfo, Compare),
	type_ctor_info__make_pred_addr(MaybeSolver,  ModuleInfo, Solver),
	type_ctor_info__make_pred_addr(MaybeInit,    ModuleInfo, Init),
	type_ctor_info__make_pred_addr(MaybePretty,  ModuleInfo, Pretty),

	module_info_globals(ModuleInfo0, Globals),
	globals__lookup_bool_option(Globals, type_layout, TypeLayoutOption),
	( TypeLayoutOption = yes ->
		type_ctor_info__gen_layout_info(ModuleName,
			TypeName, TypeArity, HldsDefn, ModuleInfo0, ModuleInfo,
			TypeCtorRep, NumFunctors, MaybeFunctors, MaybeLayout,
			NumPtags, TypeCtorTables)
	;
			% This is for measuring code size only; if this path
			% is ever taken, the resulting executable will not
			% work.
		TypeCtorRep = unknown,
		NumPtags = -1,
		NumFunctors = -1,
		MaybeFunctors = no_functors,
		MaybeLayout = no_layout,
		TypeCtorTables = [],
		ModuleInfo = ModuleInfo0
	),
	Version = type_ctor_info_rtti_version,
	RttiTypeId = rtti_type_id(ModuleName, TypeName, TypeArity),
	TypeCtorData = type_ctor_info(RttiTypeId, Unify, Index, Compare,
		TypeCtorRep, Solver, Init, Version, NumPtags, NumFunctors,
		MaybeFunctors, MaybeLayout, no, Pretty).

:- pred type_ctor_info__make_pred_addr(maybe(pred_proc_id)::in,
	module_info::in, maybe(code_addr)::out) is det.

type_ctor_info__make_pred_addr(no, _ModuleInfo, no).
type_ctor_info__make_pred_addr(yes(PredProcId), ModuleInfo, yes(PredAddr)) :-
	PredProcId = proc(PredId, ProcId),
	code_util__make_entry_label(ModuleInfo, PredId, ProcId, no, PredAddr).

%---------------------------------------------------------------------------%

	% The version of the RTTI data structures -- useful for bootstrapping.
	% If you write runtime code that checks this version number and
	% can at least handle the previous version of the data
	% structure, it makes it easier to bootstrap changes to the data
	% structures used for RTTI.
	%
	% This number should be kept in sync with MR_RTTI_VERSION in
	% runtime/mercury_type_info.h.  This means you need to update
	% the handwritten type_ctor_info structures and the code in the
	% runtime that uses RTTI to conform to whatever changes the new
	% version introduces.

:- func type_ctor_info_rtti_version = int.

type_ctor_info_rtti_version = 4.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Generate RTTI layout information for the named type.

:- pred type_ctor_info__gen_layout_info(module_name::in,
	string::in, int::in, hlds_type_defn::in,
	module_info::in, module_info::out, type_ctor_rep::out, int::out,
	type_ctor_functors_info::out, type_ctor_layout_info::out,
	int::out, list(rtti_data)::out) is det.

type_ctor_info__gen_layout_info(ModuleName, TypeName, TypeArity, HldsDefn,
		ModuleInfo0, ModuleInfo, TypeCtorRep, NumFunctors,
		FunctorsInfo, LayoutInfo, NumPtags, TypeTables) :-
	hlds_data__get_type_defn_body(HldsDefn, TypeBody),
	module_info_get_cell_count(ModuleInfo0, CellNumber0),
	(
		TypeBody = uu_type(_Alts),
		error("type_ctor_layout: sorry, undiscriminated union unimplemented\n")
	;
		TypeBody = abstract_type,
		TypeCtorRep = unknown,
		NumFunctors = -1,
		FunctorsInfo = no_functors,
		LayoutInfo = no_layout,
		TypeTables = [],
		CellNumber = CellNumber0,
		NumPtags = -1
	;
		TypeBody = eqv_type(Type),
		( term__is_ground(Type) ->
			TypeCtorRep = equiv(equiv_type_is_ground)
		;
			TypeCtorRep = equiv(equiv_type_is_not_ground)
		),
		NumFunctors = -1,
		UnivTvars = TypeArity,
			% There can be no existentially typed args to an
			% equivalence.
		ExistTvars = [],
		pseudo_type_info__construct_pseudo_type_info(Type,
			UnivTvars, ExistTvars, Rval, CellNumber0, CellNumber),
		FunctorsInfo = no_functors,
		LayoutInfo = equiv_layout(Rval),
		TypeTables = [],
		NumPtags = -1
	;
		TypeBody = du_type(Ctors, ConsTagMap, Enum, EqualityPred),
		(
			EqualityPred = yes(_),
			EqualityAxioms = user_defined
		;
			EqualityPred = no,
			EqualityAxioms = standard
		),
		list__length(Ctors, NumFunctors),
		RttiTypeId = rtti_type_id(ModuleName, TypeName, TypeArity),
		(
			Enum = yes,
			TypeCtorRep = enum(EqualityAxioms),
			type_ctor_info__make_enum_tables(Ctors, ConsTagMap,
				RttiTypeId, TypeTables,
				FunctorsInfo, LayoutInfo),
			CellNumber = CellNumber0,
			NumPtags = -1
		;
			Enum = no,
			( type_is_no_tag_type(Ctors, Name, ArgType) ->
				( term__is_ground(ArgType) ->
					Inst = equiv_type_is_ground
				;
					Inst = equiv_type_is_not_ground
				),
				TypeCtorRep = notag(EqualityAxioms, Inst),
				type_ctor_info__make_notag_tables(Name,
					ArgType, RttiTypeId,
					CellNumber0, CellNumber,
					TypeTables, FunctorsInfo, LayoutInfo),
				NumPtags = -1
			;
				module_info_globals(ModuleInfo0, Globals),
				globals__lookup_int_option(Globals,
					num_tag_bits, NumTagBits),
				int__pow(2, NumTagBits, NumTags),
				MaxPtag = NumTags - 1,
				TypeCtorRep = du(EqualityAxioms),
				type_ctor_info__make_du_tables(Ctors,
					ConsTagMap, MaxPtag, RttiTypeId,
					ModuleInfo0, CellNumber0, CellNumber,
					TypeTables, NumPtags,
					FunctorsInfo, LayoutInfo)
			)
		)
	),
	module_info_set_cell_count(ModuleInfo0, CellNumber, ModuleInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% Make the functor and notag tables for a notag type.

:- pred type_ctor_info__make_notag_tables(sym_name::in, (type)::in,
	rtti_type_id::in, int::in, int::out, list(rtti_data)::out,
	type_ctor_functors_info::out, type_ctor_layout_info::out) is det.

type_ctor_info__make_notag_tables(SymName, ArgType, RttiTypeId,
		CellNumber0, CellNumber,
		TypeTables, FunctorsInfo, LayoutInfo) :-
	unqualify_name(SymName, FunctorName),
	RttiTypeId = rtti_type_id(_, _, UnivTvars),
		% There can be no existentially typed args to the functor
		% in a notag type.
	ExistTvars = [],
	pseudo_type_info__construct_pseudo_type_info(ArgType,
		UnivTvars, ExistTvars, Rval, CellNumber0, CellNumber),
	FunctorDesc = notag_functor_desc(RttiTypeId, FunctorName, Rval),
	FunctorRttiName = notag_functor_desc,

	FunctorsInfo = notag_functors(FunctorRttiName),
	LayoutInfo = notag_layout(FunctorRttiName),
	TypeTables = [FunctorDesc].

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type name_sort_info == assoc_list(pair(string, int), rtti_name).

% Make the functor and notag tables for an enum type.

:- pred type_ctor_info__make_enum_tables(list(constructor)::in,
	cons_tag_values::in, rtti_type_id::in, list(rtti_data)::out,
	type_ctor_functors_info::out, type_ctor_layout_info::out) is det.

type_ctor_info__make_enum_tables(Ctors, ConsTagMap, RttiTypeId,
		TypeTables, FunctorInfo, LayoutInfo) :-
	type_ctor_info__make_enum_functor_tables(Ctors, 0, ConsTagMap,
		RttiTypeId, FunctorDescs, OrdinalOrderRttiNames, SortInfo0),
	list__sort(SortInfo0, SortInfo),
	assoc_list__values(SortInfo, NameOrderedRttiNames),

	NameOrderedTable = enum_name_ordered_table(RttiTypeId,
		NameOrderedRttiNames),
	NameOrderedTableRttiName = enum_name_ordered_table,
	FunctorInfo = enum_functors(NameOrderedTableRttiName),

	ValueOrderedTable = enum_value_ordered_table(RttiTypeId,
		OrdinalOrderRttiNames),
	ValueOrderedTableRttiName = enum_value_ordered_table,
	LayoutInfo = enum_layout(ValueOrderedTableRttiName),

	TypeTables = [NameOrderedTable, ValueOrderedTable | FunctorDescs].

% Create an enum_functor_desc structure for each functor in an enum type.
% The functors are given to us in ordinal order (since that's how the HLDS
% stored them), and that is how we return the list of rtti names of the
% enum_functor_desc structures; that way, it is directly usable in the type
% layout structure. We also return a structure that allows our caller to
% sort this list on functor name, which is how the type functors structure
% is constructed.

:- pred type_ctor_info__make_enum_functor_tables(list(constructor)::in,
	int::in, cons_tag_values::in, rtti_type_id::in,
	list(rtti_data)::out, list(rtti_name)::out,
	name_sort_info::out) is det.

type_ctor_info__make_enum_functor_tables([], _, _, _, [], [], []).
type_ctor_info__make_enum_functor_tables([Functor | Functors], NextOrdinal0,
		ConsTagMap, RttiTypeId,
		FunctorDescs, RttiNames, SortInfo) :-
	Functor = ctor(ExistTvars, Constraints, SymName, FunctorArgs),
	require(unify(ExistTvars, []),
		"existential arguments in functor in enum"),
	require(unify(Constraints, []),
		"class constraints on functor in enum"),
	list__length(FunctorArgs, Arity),
	require(unify(Arity, 0),
		"functor in enum has nonzero arity"),
	make_cons_id_from_qualified_sym_name(SymName, FunctorArgs, ConsId),
	map__lookup(ConsTagMap, ConsId, ConsTag),
	require(unify(ConsTag, int_constant(NextOrdinal0)),
		"mismatch on constant assigned to functor in enum"),
	unqualify_name(SymName, FunctorName),
	FunctorDesc = enum_functor_desc(RttiTypeId, FunctorName, NextOrdinal0),
	RttiName = enum_functor_desc(NextOrdinal0),
	FunctorSortInfo = (FunctorName - 0) - RttiName,
	type_ctor_info__make_enum_functor_tables(Functors, NextOrdinal0 + 1,
		ConsTagMap, RttiTypeId, FunctorDescs1, RttiNames1, SortInfo1),
	FunctorDescs = [FunctorDesc | FunctorDescs1],
	RttiNames = [RttiName | RttiNames1],
	SortInfo = [FunctorSortInfo | SortInfo1].

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type tag_map == map(int, pair(sectag_locn, map(int, rtti_name))).
:- type tag_list == assoc_list(int, pair(sectag_locn, map(int, rtti_name))).

% Make the functor and notag tables for a du type.

:- pred type_ctor_info__make_du_tables(list(constructor)::in,
	cons_tag_values::in, int::in, rtti_type_id::in, module_info::in,
	int::in, int::out, list(rtti_data)::out, int::out,
	type_ctor_functors_info::out, type_ctor_layout_info::out) is det.

type_ctor_info__make_du_tables(Ctors, ConsTagMap, MaxPtag, RttiTypeId,
		ModuleInfo, CellNumber0, CellNumber,
		TypeTables, NumPtags, FunctorInfo, LayoutInfo) :-
	map__init(TagMap0),
	type_ctor_info__make_du_functor_tables(Ctors, 0, ConsTagMap,
		RttiTypeId, ModuleInfo, CellNumber0, CellNumber,
		FunctorDescs, SortInfo0, TagMap0, TagMap),
	list__sort(SortInfo0, SortInfo),
	assoc_list__values(SortInfo, NameOrderedRttiNames),

	NameOrderedTable = du_name_ordered_table(RttiTypeId,
		NameOrderedRttiNames),
	NameOrderedTableRttiName = du_name_ordered_table,
	FunctorInfo = du_functors(NameOrderedTableRttiName),

	type_ctor_info__make_du_ptag_ordered_table(TagMap, MaxPtag,
		RttiTypeId, ValueOrderedTableRttiName, ValueOrderedTables,
		NumPtags),
	LayoutInfo = du_layout(ValueOrderedTableRttiName),
	list__append([NameOrderedTable | FunctorDescs], ValueOrderedTables,
		TypeTables).

% Create an enum_functor_desc structure for each functor in a du type.
% Besides returning a list of the rtti names of their du_functor_desc
% structures, we return two other items of information. The SortInfo
% enables our caller to sort these rtti names on functor name and then arity,
% which is how the type functors structure is constructed. The TagMap
% groups the rttis into groups depending on their primary tags; this is
% how the type layout structure is constructed.

:- pred type_ctor_info__make_du_functor_tables(list(constructor)::in,
	int::in, cons_tag_values::in, rtti_type_id::in, module_info::in,
	int::in, int::out, list(rtti_data)::out, name_sort_info::out,
	tag_map::in, tag_map::out) is det.

type_ctor_info__make_du_functor_tables([], _, _, _, _,
		CellNumber, CellNumber, [], [], TagMap, TagMap).
type_ctor_info__make_du_functor_tables([Functor | Functors], Ordinal,
		ConsTagMap, RttiTypeId, ModuleInfo, CellNumber0, CellNumber,
		Tables, SortInfo, TagMap0, TagMap) :-
	Functor = ctor(ExistTvars, Constraints, SymName, FunctorArgs),
	list__length(FunctorArgs, Arity),
	unqualify_name(SymName, FunctorName),
	RttiName = du_functor_desc(Ordinal),
	make_cons_id_from_qualified_sym_name(SymName, FunctorArgs, ConsId),
	map__lookup(ConsTagMap, ConsId, ConsTag),
	( ConsTag = unshared_tag(ConsPtag) ->
		Locn = sectag_none,
		Ptag = ConsPtag,
		Stag = 0,
		type_ctor_info__update_tag_info(Ptag, Stag, Locn, RttiName,
			TagMap0, TagMap1)
	; ConsTag = shared_local_tag(ConsPtag, ConsStag) ->
		Locn = sectag_local,
		Ptag = ConsPtag,
		Stag = ConsStag,
		type_ctor_info__update_tag_info(Ptag, Stag, Locn, RttiName,
			TagMap0, TagMap1)
	; ConsTag = shared_remote_tag(ConsPtag, ConsStag) ->
		Locn = sectag_remote,
		Ptag = ConsPtag,
		Stag = ConsStag,
		type_ctor_info__update_tag_info(Ptag, Stag, Locn, RttiName,
			TagMap0, TagMap1)
	;
		error("unexpected cons_tag for du function symbol")
	),

	type_ctor_info__generate_arg_info_tables(ModuleInfo,
		RttiTypeId, Ordinal, FunctorArgs, ExistTvars,
		CellNumber0, CellNumber1, MaybeArgNames,
		ArgPseudoTypeInfoVector, FieldTables, ContainsVarBitVector),
	( ExistTvars = [] ->
		MaybeExistInfo = no,
		ExistTables = []
	;
		module_info_classes(ModuleInfo, ClassTable),
		type_ctor_info__generate_type_info_locns(ExistTvars,
			Constraints, ClassTable, RttiTypeId, Ordinal,
			ExistInfo, ExistTables),
		MaybeExistInfo = yes(ExistInfo)
	),
	list__append(FieldTables, ExistTables, SubTables),
	FunctorDesc = du_functor_desc(RttiTypeId, FunctorName, Ptag, Stag,
		Locn, Ordinal, Arity, ContainsVarBitVector,
		ArgPseudoTypeInfoVector, MaybeArgNames, MaybeExistInfo),
	FunctorSortInfo = (FunctorName - Arity) - RttiName,
	type_ctor_info__make_du_functor_tables(Functors, Ordinal + 1,
		ConsTagMap, RttiTypeId, ModuleInfo, CellNumber1, CellNumber,
		Tables1, SortInfo1, TagMap1, TagMap),
	list__append([FunctorDesc | SubTables], Tables1, Tables),
	SortInfo = [FunctorSortInfo | SortInfo1].

% Generate the tables that describe the arguments of a functor. 

:- pred type_ctor_info__generate_arg_info_tables(module_info::in,
	rtti_type_id::in, int::in, list(constructor_arg)::in, existq_tvars::in,
	int::in, int::out, maybe(rtti_name)::out, rval::out,
	list(rtti_data)::out, int::out) is det.

type_ctor_info__generate_arg_info_tables(
		ModuleInfo, RttiTypeId, Ordinal,
		Args, ExistTvars, CellNumber0, CellNumber,
		MaybeFieldNamesRttiName, Vector, Tables,
		ContainsVarBitVector) :-
	RttiTypeId = rtti_type_id(_TypeModule, _TypeName, TypeArity),
	type_ctor_info__generate_arg_infos(Args, TypeArity, ExistTvars,
		ModuleInfo, CellNumber0, CellNumber1,
		MaybeArgNames, LldsTypes, MaybePseudoTypeInfos,
		0, 0, ContainsVarBitVector),
	list__filter((lambda([MaybeName::in] is semidet, MaybeName = yes(_))),
		MaybeArgNames, FieldNames),
	(
		FieldNames = [],
		MaybeFieldNamesRttiName = no,
		Tables = []
	;
		FieldNames = [_|_],
		FieldNameTable = field_names(RttiTypeId, Ordinal,
			MaybeArgNames),
		FieldNamesRttiName = field_names(Ordinal),
		MaybeFieldNamesRttiName = yes(FieldNamesRttiName),
		Tables = [FieldNameTable]
	),
	type_ctor_info__get_next_cell_number(CellNumber1, CN, CellNumber),
	Reuse = no,
	Vector = create(0, MaybePseudoTypeInfos, initial(LldsTypes, none),
		must_be_static, CN, "arg_types", Reuse).

% For each argument of a functor, return three items of information:
% its name (if any), a pseudotypeinfo describing its type (and the llds_type
% that describes the pseudotypeinfo), and an indication whether the type
% contains variables or not. The last item is encoded as an integer
% which contains a 1 bit in the position given by 1 << N if argument N's type
% contains variables (assuming that arguments are numbered starting from zero).
% The number of bits in the integer is given by contains_var_bit_vector_size;
% arguments beyond this limit do not contribute to this bit vector.

:- pred type_ctor_info__generate_arg_infos(list(constructor_arg)::in,
	int::in, existq_tvars::in, module_info::in, int::in, int::out,
	list(maybe(string))::out, initial_arg_types::out,
	list(maybe(rval))::out, int::in, int::in, int::out) is det.

type_ctor_info__generate_arg_infos([], _, _, _,
		CellNumber, CellNumber, [], [], [],
		_, ContainsVarBitVector, ContainsVarBitVector).
type_ctor_info__generate_arg_infos([MaybeArgSymName - ArgType | Args],
		NumUnivTvars, ExistTvars, ModuleInfo, CellNumber0, CellNumber,
		[MaybeArgName | MaybeArgNames],
		[1 - yes(LldsType) | LldsTypes],
		[yes(PseudoTypeInfo) | MaybePseudoTypeInfos],
		ArgNum, ContainsVarBitVector0, ContainsVarBitVector) :-
	(
		MaybeArgSymName = yes(SymName),
		unqualify_name(SymName, ArgName),
		MaybeArgName = yes(ArgName)
	;
		MaybeArgSymName = no,
		MaybeArgName = no
	),
	pseudo_type_info__construct_typed_pseudo_type_info(ArgType,
		NumUnivTvars, ExistTvars, PseudoTypeInfo, LldsType,
		CellNumber0, CellNumber1),
	( term__is_ground(ArgType) ->
		ContainsVarBitVector1 = ContainsVarBitVector0
	;
		( ArgNum >= contains_var_bit_vector_size - 1 ->
			BitNum = contains_var_bit_vector_size - 1
		;
			BitNum = ArgNum
		),
		ContainsVarBitVector1 = ContainsVarBitVector0 \/ (1 << BitNum)
	),
	type_ctor_info__generate_arg_infos(Args, NumUnivTvars,
		ExistTvars, ModuleInfo, CellNumber1, CellNumber,
		MaybeArgNames, LldsTypes, MaybePseudoTypeInfos,
		ArgNum + 1, ContainsVarBitVector1, ContainsVarBitVector).

% This function gives the size of the MR_du_functor_arg_type_contains_var
% field of the C type MR_DuFunctorDesc in bits.

:- func type_ctor_info__contains_var_bit_vector_size = int.

type_ctor_info__contains_var_bit_vector_size = 16.

% Construct the RTTI structures that record information about the locations
% of the typeinfos describing the types of the existentially typed arguments
% of a functor.

:- pred type_ctor_info__generate_type_info_locns(list(tvar)::in,
	list(class_constraint)::in, class_table::in, rtti_type_id::in, int::in,
	rtti_name::out, list(rtti_data)::out) is det.

type_ctor_info__generate_type_info_locns(ExistTvars, Constraints, ClassTable,
		RttiTypeId, Ordinal, exist_info(Ordinal),
		[ExistInfo, ExistLocns]) :-
	list__map((pred(C::in, Ts::out) is det :- C = constraint(_, Ts)),
		Constraints, ConstrainedTvars0),
	list__condense(ConstrainedTvars0, ConstrainedTvars1),
	term__vars_list(ConstrainedTvars1, ConstrainedTvars2),
	list__delete_elems(ExistTvars, ConstrainedTvars2, UnconstrainedTvars),
		% We do this to maintain the ordering of the type variables.
	list__delete_elems(ExistTvars, UnconstrainedTvars, ConstrainedTvars),
	map__init(LocnMap0),
	list__foldl2((pred(T::in, N0::in, N::out, Lm0::in, Lm::out) is det :-
			Locn = plain_typeinfo(N0),
			map__det_insert(Lm0, T, Locn, Lm),
			N = N0 + 1
		), UnconstrainedTvars, 0, TIsPlain, LocnMap0, LocnMap1),
	list__length(ExistTvars, AllTIs),
	TIsInTCIs = AllTIs - TIsPlain,
	list__foldl(
		find_type_info_index(Constraints, ClassTable, TIsPlain),
		ConstrainedTvars, LocnMap1, LocnMap),
	list__length(Constraints, TCIs),
	ExistInfo = exist_info(RttiTypeId, Ordinal,
		TIsPlain, TIsInTCIs, TCIs, exist_locns(Ordinal)),
	list__map((pred(Tvar::in, Locn::out) is det :-
		map__lookup(LocnMap, Tvar, Locn)),
		ExistTvars, Locns),
	ExistLocns = exist_locns(RttiTypeId, Ordinal, Locns).

:- pred find_type_info_index(list(class_constraint)::in, class_table::in,
	int::in, tvar::in, map(tvar, exist_typeinfo_locn)::in,
	map(tvar, exist_typeinfo_locn)::out) is det.

find_type_info_index(Constraints, ClassTable, StartSlot, Tvar,
		LocnMap0, LocnMap) :-
	first_matching_type_class_info(Constraints, Tvar,
		FirstConstraint, StartSlot, Slot, TypeInfoIndex),
	FirstConstraint = constraint(ClassName, Args),
	list__length(Args, ClassArity),
	map__lookup(ClassTable, class_id(ClassName, ClassArity), ClassDefn),
	ClassDefn = hlds_class_defn(_, SuperClasses, _, _, _, _, _),
	list__length(SuperClasses, NumSuperClasses),
	RealTypeInfoIndex = TypeInfoIndex + NumSuperClasses,
	Locn = typeinfo_in_tci(Slot, RealTypeInfoIndex),
	map__det_insert(LocnMap0, Tvar, Locn, LocnMap).

:- pred first_matching_type_class_info(list(class_constraint)::in, tvar::in,
	class_constraint::out, int::in, int::out, int::out) is det.

first_matching_type_class_info([], _, _, _, _, _) :-
	error("base_type_layout: constrained type info not found").
first_matching_type_class_info([C|Cs], Tvar, MatchingConstraint, N0, N,
		TypeInfoIndex) :-
	C = constraint(_, Ts),
	term__vars_list(Ts, TVs),
	( list__nth_member_search(TVs, Tvar, Index) ->
		N = N0,
		MatchingConstraint = C,
		TypeInfoIndex = Index
	;
		first_matching_type_class_info(Cs, Tvar, MatchingConstraint,
			N0 + 1, N, TypeInfoIndex)
	).

%---------------------------------------------------------------------------%

:- pred type_ctor_info__update_tag_info(int::in, int::in, sectag_locn::in,
	rtti_name::in, tag_map::in,  tag_map::out) is det.

type_ctor_info__update_tag_info(Ptag, Stag, Locn, RttiName, TagMap0, TagMap)
		:-
	( map__search(TagMap0, Ptag, OldLocn - OldSharerMap) ->
		( Locn = sectag_none ->
			error("unshared ptag shared after all")
		; OldLocn = Locn ->
			true
		;
			error("disagreement on sectag location for ptag")
		),
		map__det_insert(OldSharerMap, Stag, RttiName, NewSharerMap),
		map__det_update(TagMap0, Ptag, Locn - NewSharerMap, TagMap)
	;
		map__init(NewSharerMap0),
		map__det_insert(NewSharerMap0, Stag, RttiName, NewSharerMap),
		map__det_insert(TagMap0, Ptag, Locn - NewSharerMap, TagMap)
	).

:- pred type_ctor_info__make_du_ptag_ordered_table(tag_map::in, int::in,
	rtti_type_id::in, rtti_name::out, list(rtti_data)::out, int::out)
	is det.

type_ctor_info__make_du_ptag_ordered_table(TagMap, MaxPtagValue,
		RttiTypeId, PtagOrderedRttiName, Tables, NumPtags) :-
	map__to_assoc_list(TagMap, TagList),
	type_ctor_info__make_du_ptag_layouts(TagList, 0, MaxPtagValue,
		RttiTypeId, PtagLayouts, SubTables, NumPtags),
	PtagOrderedTable = du_ptag_ordered_table(RttiTypeId, PtagLayouts),
	PtagOrderedRttiName = du_ptag_ordered_table,
	Tables = [PtagOrderedTable | SubTables].

:- pred type_ctor_info__make_du_ptag_layouts(tag_list::in, int::in, int::in,
	rtti_type_id::in, list(du_ptag_layout)::out, list(rtti_data)::out,
	int::out) is det.

type_ctor_info__make_du_ptag_layouts(TagList0, CurPtag, MaxPtag,
		RttiTypeId, PtagLayouts, Tables, NumPtags) :-
	(
		TagList0 = [],
		PtagLayouts = [],
		Tables = [],
		NumPtags = CurPtag
	;
		TagList0 = [Ptag - (Locn - StagMap) | TagList],
		require(unify(CurPtag, Ptag),
			"missing ptag value in make_du_ptag_layout"),
		require(CurPtag =< MaxPtag,
			"ptag value exceeds maximum"),
		map__to_assoc_list(StagMap, StagList),
		list__length(StagList, StagListLength),
		type_ctor_info__make_du_stag_table(0, StagListLength - 1,
			StagList, StagRttiNames),
		StagOrderedTable = du_stag_ordered_table(RttiTypeId,
			Ptag, StagRttiNames),
		StagOrderedAddr = du_stag_ordered_table(Ptag),
		PtagLayout = du_ptag_layout(StagListLength, Locn,
			StagOrderedAddr),
		type_ctor_info__make_du_ptag_layouts(TagList,
			CurPtag + 1, MaxPtag, RttiTypeId,
			PtagLayouts1, Tables1, NumPtags),
		PtagLayouts = [PtagLayout | PtagLayouts1],
		Tables = [StagOrderedTable | Tables1]
	).

:- pred type_ctor_info__make_du_stag_table(int::in, int::in,
	assoc_list(int, rtti_name)::in, list(rtti_name)::out) is det.

type_ctor_info__make_du_stag_table(CurStag, MaxStag, TagList0,
		StagRttiNames) :-
	( CurStag =< MaxStag ->
		(
			TagList0 = [],
			error("short stag list in make_du_stag_table")
		;
			TagList0 = [Stag - RttiName | TagList],
			require(unify(CurStag, Stag),
				"missing stag value in make_du_stag_table")
		),
		type_ctor_info__make_du_stag_table(CurStag + 1, MaxStag,
			TagList, StagRttiNames1),
		StagRttiNames = [RttiName | StagRttiNames1]
	;
		require(unify(TagList0, []),
			"leftover stag values in make_du_stag_table"),
		StagRttiNames = []
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred type_ctor_info__get_next_cell_number(int::in, int::out, int::out)
	is det.

type_ctor_info__get_next_cell_number(CellNumber0, Next, CellNumber) :-
	CellNumber = CellNumber0 + 1,
	Next = CellNumber.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
