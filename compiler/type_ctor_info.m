%---------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
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
% second stage then generates lower-level RTTI descriptions of type_ctor_infos
% from the surviving type_ctor_gen_infos.  These can then be easily
% turned into either LLDS or MLDS.
%
% The documentation of the data structures built in this module is in
% runtime/mercury_type_info.h; that file also contains a list of all
% the files that depend on these data structures.
%
%---------------------------------------------------------------------------%

:- module backend_libs__type_ctor_info.

:- interface.

:- import_module hlds__hlds_module, backend_libs__rtti.
:- import_module list.

:- pred type_ctor_info__generate_hlds(module_info::in, module_info::out)
	is det.

:- pred type_ctor_info__generate_rtti(module_info::in, list(rtti_data)::out)
	is det.

:- implementation.

:- import_module backend_libs__rtti, backend_libs__pseudo_type_info.
:- import_module hlds__hlds_data, hlds__hlds_pred, hlds__hlds_out.
:- import_module hlds__make_tags, parse_tree__prog_data.
:- import_module parse_tree__prog_util, parse_tree__prog_out.
:- import_module ll_backend__code_util, hlds__special_pred.
:- import_module check_hlds__type_util, libs__globals, libs__options.
:- import_module backend_libs__builtin_ops, hlds__error_util.

:- import_module bool, string, int, map, std_util, assoc_list, require.
:- import_module term.

%---------------------------------------------------------------------------%

type_ctor_info__generate_hlds(ModuleInfo0, ModuleInfo) :-
	module_info_name(ModuleInfo0, ModuleName),
	module_info_types(ModuleInfo0, TypeTable),
	map__keys(TypeTable, TypeCtors),
	type_ctor_info__gen_type_ctor_gen_infos(TypeCtors, TypeTable,
		ModuleName, ModuleInfo0, TypeCtorGenInfos),
	module_info_set_type_ctor_gen_infos(ModuleInfo0, TypeCtorGenInfos,
		ModuleInfo).

	% Given a list of the ids of all the types in the type table,
	% find the types defined in this module, and return a type_ctor_gen_info
	% for each.

:- pred type_ctor_info__gen_type_ctor_gen_infos(list(type_ctor)::in,
	type_table::in, module_name::in, module_info::in,
	list(type_ctor_gen_info)::out) is det.

type_ctor_info__gen_type_ctor_gen_infos([], _, _, _, []).
type_ctor_info__gen_type_ctor_gen_infos([TypeCtor | TypeCtors], TypeTable,
		ModuleName, ModuleInfo, TypeCtorGenInfos) :-
	type_ctor_info__gen_type_ctor_gen_infos(TypeCtors, TypeTable,
		ModuleName, ModuleInfo, TypeCtorGenInfos1),
	TypeCtor = SymName - TypeArity,
	(
		SymName = qualified(TypeModuleName, TypeName),
		( 
			TypeModuleName = ModuleName,
			map__lookup(TypeTable, TypeCtor, TypeDefn),
			hlds_data__get_type_defn_body(TypeDefn, TypeBody),
			TypeBody \= abstract_type,
			\+ type_ctor_has_hand_defined_rtti(TypeCtor)
		->
			type_ctor_info__gen_type_ctor_gen_info(TypeCtor,
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

:- pred type_ctor_info__gen_type_ctor_gen_info(type_ctor::in, string::in,
	int::in, hlds_type_defn::in, module_name::in, module_info::in,
	type_ctor_gen_info::out) is det.

type_ctor_info__gen_type_ctor_gen_info(TypeCtor, TypeName, TypeArity, TypeDefn,
		ModuleName, ModuleInfo, TypeCtorGenInfo) :-
	hlds_data__get_type_defn_status(TypeDefn, Status),
	module_info_globals(ModuleInfo, Globals),
	module_info_get_special_pred_map(ModuleInfo, SpecMap),
	globals__lookup_bool_option(Globals, special_preds, SpecialPreds),
	(
		(
			SpecialPreds = yes
		;
			SpecialPreds = no,
			hlds_data__get_type_defn_body(TypeDefn, Body),
			Body = du_type(_, _, _, yes(_UserDefinedEquality), _)
		)
	->
		map__lookup(SpecMap, unify - TypeCtor, UnifyPredId),
		special_pred_mode_num(unify, UnifyProcInt),
		proc_id_to_int(UnifyProcId, UnifyProcInt),
		MaybeUnify = yes(proc(UnifyPredId, UnifyProcId)),

		map__lookup(SpecMap, compare - TypeCtor, ComparePredId),
		special_pred_mode_num(compare, CompareProcInt),
		proc_id_to_int(CompareProcId, CompareProcInt),
		MaybeCompare = yes(proc(ComparePredId, CompareProcId))
	;
		MaybeUnify = no,
		MaybeCompare = no
	),
	TypeCtorGenInfo = type_ctor_gen_info(TypeCtor, ModuleName, TypeName,
		TypeArity, Status, TypeDefn, MaybeUnify, MaybeCompare).

%---------------------------------------------------------------------------%

type_ctor_info__generate_rtti(ModuleInfo, Tables) :-
	module_info_type_ctor_gen_infos(ModuleInfo, TypeCtorGenInfos),
	type_ctor_info__construct_type_ctor_infos(TypeCtorGenInfos,
		ModuleInfo, [], Dynamic, [], Static0),
	% The same pseudo_type_info may be generated in several
	% places; we need to eliminate duplicates here, to avoid
	% duplicate definition errors in the generated C code.
	Static = list__remove_dups(Static0),
	list__append(Dynamic, Static, Tables).

:- pred type_ctor_info__construct_type_ctor_infos(
	list(type_ctor_gen_info)::in, module_info::in,
	list(rtti_data)::in, list(rtti_data)::out,
	list(rtti_data)::in, list(rtti_data)::out) is det.

type_ctor_info__construct_type_ctor_infos([], _ModuleInfo,
		Dynamic, Dynamic, Static, Static).
type_ctor_info__construct_type_ctor_infos(
		[TypeCtorGenInfo | TypeCtorGenInfos], ModuleInfo,
		Dynamic0, Dynamic, Static0, Static) :-
	type_ctor_info__construct_type_ctor_info(TypeCtorGenInfo,
		ModuleInfo, TypeCtorCModule, TypeCtorTables),
	Dynamic1 = [TypeCtorCModule | Dynamic0],
	list__append(TypeCtorTables, Static0, Static1),
	type_ctor_info__construct_type_ctor_infos(TypeCtorGenInfos,
		ModuleInfo, Dynamic1, Dynamic, Static1, Static).

:- pred type_ctor_info__construct_type_ctor_info(type_ctor_gen_info::in,
	module_info::in, rtti_data::out, list(rtti_data)::out) is det.

type_ctor_info__construct_type_ctor_info(TypeCtorGenInfo,
		ModuleInfo, TypeCtorData, TypeCtorTables) :-
	TypeCtorGenInfo = type_ctor_gen_info(_TypeCtor, ModuleName, TypeName,
		TypeArity, _Status, HldsDefn, MaybeUnify, MaybeCompare),
	type_ctor_info__make_proc_label(MaybeUnify,   ModuleInfo, Unify),
	type_ctor_info__make_proc_label(MaybeCompare, ModuleInfo, Compare),

	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, type_layout, TypeLayoutOption),
	( TypeLayoutOption = yes ->
		type_ctor_info__gen_layout_info(ModuleName,
			TypeName, TypeArity, HldsDefn, ModuleInfo,
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
		TypeCtorTables = []
	),
	Version = type_ctor_info_rtti_version,
	RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, TypeArity),
	TypeCtorData = type_ctor_info(RttiTypeCtor, Unify, Compare,
		TypeCtorRep, Version, NumPtags, NumFunctors,
		MaybeFunctors, MaybeLayout).

:- pred type_ctor_info__make_proc_label(maybe(pred_proc_id)::in,
	module_info::in, maybe(rtti_proc_label)::out) is det.

type_ctor_info__make_proc_label(no, _ModuleInfo, no).
type_ctor_info__make_proc_label(yes(PredProcId), ModuleInfo, yes(ProcLabel)) :-
	PredProcId = proc(PredId, ProcId),
	ProcLabel = rtti__make_proc_label(ModuleInfo, PredId, ProcId).

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

type_ctor_info_rtti_version = 7.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Generate RTTI layout information for the named type.

:- pred type_ctor_info__gen_layout_info(module_name::in,
	string::in, int::in, hlds_type_defn::in,
	module_info::in, type_ctor_rep::out, int::out,
	type_ctor_functors_info::out, type_ctor_layout_info::out,
	int::out, list(rtti_data)::out) is det.

type_ctor_info__gen_layout_info(ModuleName, TypeName, TypeArity, HldsDefn,
		ModuleInfo, TypeCtorRep, NumFunctors,
		FunctorsInfo, LayoutInfo, NumPtags, TypeTables) :-
	module_info_globals(ModuleInfo, Globals),
	hlds_data__get_type_defn_body(HldsDefn, TypeBody),
	(
		TypeBody = abstract_type,
		TypeCtorRep = unknown,
		NumFunctors = -1,
		FunctorsInfo = no_functors,
		LayoutInfo = no_layout,
		TypeTables = [],
		NumPtags = -1
	;
			% We treat foreign_types as equivalent to the
			% type builtin__c_pointer.
		TypeBody = foreign_type(_),
		gen_layout_info_eqv_type(c_pointer_type, TypeArity,
				TypeCtorRep, NumFunctors, FunctorsInfo,
				LayoutInfo, NumPtags, TypeTables)
	;
		TypeBody = eqv_type(Type),
		( term__is_ground(Type) ->
			TypeCtorRep = equiv(equiv_type_is_ground)
		;
			TypeCtorRep = equiv(equiv_type_is_not_ground)
		),
		NumFunctors = -1,
		FunctorsInfo = no_functors,
		UnivTvars = TypeArity,
			% There can be no existentially typed args to an
			% equivalence.
		ExistTvars = [],
		make_pseudo_type_info_and_tables(Type,
			UnivTvars, ExistTvars, PseudoTypeInfoRttiData,
			[], TypeTables),
		LayoutInfo = equiv_layout(PseudoTypeInfoRttiData),
		NumPtags = -1
	;
		TypeBody = du_type(Ctors, ConsTagMap, Enum, EqualityPred, _),
		(
			EqualityPred = yes(_),
			EqualityAxioms = user_defined
		;
			EqualityPred = no,
			EqualityAxioms = standard
		),
		list__length(Ctors, NumFunctors),
		globals__lookup_bool_option(Globals, reserve_tag, ReserveTag),
		RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, TypeArity),
		(
			Enum = yes,
			TypeCtorRep = enum(EqualityAxioms),
			type_ctor_info__make_enum_tables(Ctors, ConsTagMap,
				RttiTypeCtor, ReserveTag, TypeTables,
				FunctorsInfo, LayoutInfo),
			NumPtags = -1
		;
			Enum = no,
			(
				type_constructors_should_be_no_tag(Ctors, 
					Globals, Name, ArgType, MaybeArgName)
			->
				( term__is_ground(ArgType) ->
					Inst = equiv_type_is_ground
				;
					Inst = equiv_type_is_not_ground
				),
				TypeCtorRep = notag(EqualityAxioms, Inst),
				type_ctor_info__make_notag_tables(Name,
					ArgType, MaybeArgName, RttiTypeCtor,
					TypeTables, FunctorsInfo, LayoutInfo),
				NumPtags = -1
			;
				globals__lookup_int_option(Globals,
					num_tag_bits, NumTagBits),
				int__pow(2, NumTagBits, NumTags),
				MaxPtag = NumTags - 1,
				type_ctor_info__make_du_tables(Ctors,
					ConsTagMap, MaxPtag, RttiTypeCtor,
					EqualityAxioms, ModuleInfo,
					TypeTables, NumPtags,
					FunctorsInfo, LayoutInfo, TypeCtorRep)
			)
		)
	).

:- pred gen_layout_info_eqv_type((type)::in, int::in,
		type_ctor_rep::out, int::out, type_ctor_functors_info::out,
		type_ctor_layout_info::out, int::out,
		list(rtti_data)::out) is det.

gen_layout_info_eqv_type(Type, TypeArity,
		TypeCtorRep, NumFunctors, FunctorsInfo,
		LayoutInfo, NumPtags, TypeTables) :-
	( term__is_ground(Type) ->
		TypeCtorRep = equiv(equiv_type_is_ground)
	;
		TypeCtorRep = equiv(equiv_type_is_not_ground)
	),
	NumFunctors = -1,
	FunctorsInfo = no_functors,
	UnivTvars = TypeArity,
		% There can be no existentially typed args to an
		% equivalence.
	ExistTvars = [],
	make_pseudo_type_info_and_tables(Type,
		UnivTvars, ExistTvars, PseudoTypeInfoRttiData,
		[], TypeTables),
	LayoutInfo = equiv_layout(PseudoTypeInfoRttiData),
	NumPtags = -1.

% Construct an rtti_data for a pseudo_type_info,
% and also construct rtti_data definitions for all of the pseudo_type_infos
% that it references and prepend them to the given list of rtti_data tables.

:- pred make_pseudo_type_info_and_tables(type, int, existq_tvars, rtti_data,
		list(rtti_data), list(rtti_data)).
:- mode make_pseudo_type_info_and_tables(in, in, in, out, in, out) is det.

make_pseudo_type_info_and_tables(Type, UnivTvars, ExistTvars, RttiData,
		Tables0, Tables) :-
	pseudo_type_info__construct_pseudo_type_info(Type,
		UnivTvars, ExistTvars, PseudoTypeInfo),
	RttiData = pseudo_type_info(PseudoTypeInfo),
	make_pseudo_type_info_tables(PseudoTypeInfo,
		Tables0, Tables).

% Construct rtti_data definitions for all of the non-atomic subterms
% of a pseudo_type_info, and prepend them to the given
% list of rtti_data tables.

:- pred make_type_info_tables(rtti_type_info::in,
	list(rtti_data)::in, list(rtti_data)::out) is det.

make_type_info_tables(plain_arity_zero_type_info(_), Tables, Tables).
make_type_info_tables(PseudoTypeInfo, Tables0, Tables) :-
	PseudoTypeInfo = plain_type_info(_, Args),
	Tables1 = [type_info(PseudoTypeInfo) | Tables0],
	list__foldl(make_type_info_tables, Args, Tables1, Tables).
make_type_info_tables(PseudoTypeInfo, Tables0, Tables) :-
	PseudoTypeInfo = var_arity_type_info(_, Args),
	Tables1 = [type_info(PseudoTypeInfo) | Tables0],
	list__foldl(make_type_info_tables, Args, Tables1, Tables).

:- pred make_pseudo_type_info_tables(rtti_pseudo_type_info::in,
	list(rtti_data)::in, list(rtti_data)::out) is det.

make_pseudo_type_info_tables(plain_arity_zero_pseudo_type_info(_),
		Tables, Tables).
make_pseudo_type_info_tables(PseudoTypeInfo, Tables0, Tables) :-
	PseudoTypeInfo = plain_pseudo_type_info(_, Args),
	Tables1 = [pseudo_type_info(PseudoTypeInfo) | Tables0],
	list__foldl(make_maybe_pseudo_type_info_tables, Args,
		Tables1, Tables).
make_pseudo_type_info_tables(PseudoTypeInfo, Tables0, Tables) :-
	PseudoTypeInfo = var_arity_pseudo_type_info(_, Args),
	Tables1 = [pseudo_type_info(PseudoTypeInfo) | Tables0],
	list__foldl(make_maybe_pseudo_type_info_tables, Args,
		Tables1, Tables).
make_pseudo_type_info_tables(type_var(_), Tables, Tables).

:- pred make_maybe_pseudo_type_info_tables(rtti_maybe_pseudo_type_info::in,
	list(rtti_data)::in, list(rtti_data)::out) is det.

make_maybe_pseudo_type_info_tables(pseudo(PseudoTypeInfo), Tables0, Tables) :-
	make_pseudo_type_info_tables(PseudoTypeInfo, Tables0, Tables).
make_maybe_pseudo_type_info_tables(plain(TypeInfo), Tables0, Tables) :-
	make_type_info_tables(TypeInfo, Tables0, Tables).

:- pred make_maybe_pseudo_type_info_or_self_tables(
	rtti_maybe_pseudo_type_info_or_self::in,
	list(rtti_data)::in, list(rtti_data)::out) is det.

make_maybe_pseudo_type_info_or_self_tables(pseudo(PseudoTypeInfo),
		Tables0, Tables) :-
	make_pseudo_type_info_tables(PseudoTypeInfo, Tables0, Tables).
make_maybe_pseudo_type_info_or_self_tables(plain(TypeInfo), Tables0, Tables) :-
	make_type_info_tables(TypeInfo, Tables0, Tables).
make_maybe_pseudo_type_info_or_self_tables(self, Tables, Tables).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% Make the functor and layout tables for a notag type.

:- pred type_ctor_info__make_notag_tables(sym_name::in, (type)::in,
	maybe(string)::in, rtti_type_ctor::in, list(rtti_data)::out,
	type_ctor_functors_info::out, type_ctor_layout_info::out) is det.

type_ctor_info__make_notag_tables(SymName, ArgType, MaybeArgName, RttiTypeCtor,
		TypeTables, FunctorsInfo, LayoutInfo) :-
	unqualify_name(SymName, FunctorName),
	RttiTypeCtor = rtti_type_ctor(_, _, UnivTvars),
		% There can be no existentially typed args to the functor
		% in a notag type.
	ExistTvars = [],
	make_pseudo_type_info_and_tables(ArgType, UnivTvars, ExistTvars,
		RttiData, [], Tables0),
	FunctorDesc = notag_functor_desc(RttiTypeCtor, FunctorName, RttiData,
		MaybeArgName),
	FunctorRttiName = notag_functor_desc,

	FunctorsInfo = notag_functors(FunctorRttiName),
	LayoutInfo = notag_layout(FunctorRttiName),
	TypeTables = [FunctorDesc | Tables0].

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type name_sort_info == assoc_list(pair(string, int), rtti_name).

% Make the functor and layout tables for an enum type.

:- pred type_ctor_info__make_enum_tables(list(constructor)::in,
	cons_tag_values::in, rtti_type_ctor::in, bool::in, list(rtti_data)::out,
	type_ctor_functors_info::out, type_ctor_layout_info::out) is det.

type_ctor_info__make_enum_tables(Ctors, ConsTagMap, RttiTypeCtor, ReserveTag,
		TypeTables, FunctorInfo, LayoutInfo) :-
	(
		% If there are any existentially quantified type variables,
		% then the type will contain hidden fields holding the
		% type_infos and/or typeclass infos for those type variables,
		% so it won't be a single-argument type.

		ReserveTag = yes
	->
		unexpected("type_ctor_info", "enum in .rt grade")
	;
		InitTag = 0
	),
	type_ctor_info__make_enum_functor_tables(Ctors, InitTag, ConsTagMap,
		RttiTypeCtor, FunctorDescs, OrdinalOrderRttiNames, SortInfo0),
	list__sort(SortInfo0, SortInfo),
	assoc_list__values(SortInfo, NameOrderedRttiNames),

	NameOrderedTable = enum_name_ordered_table(RttiTypeCtor,
		NameOrderedRttiNames),
	NameOrderedTableRttiName = enum_name_ordered_table,
	FunctorInfo = enum_functors(NameOrderedTableRttiName),

	ValueOrderedTable = enum_value_ordered_table(RttiTypeCtor,
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
	int::in, cons_tag_values::in, rtti_type_ctor::in,
	list(rtti_data)::out, list(rtti_name)::out,
	name_sort_info::out) is det.

type_ctor_info__make_enum_functor_tables([], _, _, _, [], [], []).
type_ctor_info__make_enum_functor_tables([Functor | Functors], NextOrdinal0,
		ConsTagMap, RttiTypeCtor,
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
	FunctorDesc =
		enum_functor_desc(RttiTypeCtor, FunctorName, NextOrdinal0),
	RttiName = enum_functor_desc(NextOrdinal0),
	FunctorSortInfo = (FunctorName - 0) - RttiName,
	type_ctor_info__make_enum_functor_tables(Functors, NextOrdinal0 + 1,
		ConsTagMap, RttiTypeCtor, FunctorDescs1, RttiNames1,
		SortInfo1),
	FunctorDescs = [FunctorDesc | FunctorDescs1],
	RttiNames = [RttiName | RttiNames1],
	SortInfo = [FunctorSortInfo | SortInfo1].

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type tag_map == map(int, pair(sectag_locn, map(int, rtti_name))).
:- type tag_list == assoc_list(int, pair(sectag_locn, map(int, rtti_name))).

:- type reserved_addr_map == map(reserved_address, rtti_data).

% Make the functor and layout tables for a du type
% (including reserved_addr types).

:- pred type_ctor_info__make_du_tables(list(constructor)::in,
	cons_tag_values::in, int::in, rtti_type_ctor::in, equality_axioms::in,
	module_info::in, list(rtti_data)::out, int::out,
	type_ctor_functors_info::out, type_ctor_layout_info::out,
	type_ctor_rep::out) is det.

type_ctor_info__make_du_tables(Ctors, ConsTagMap, MaxPtag, RttiTypeCtor,
		EqualityAxioms, ModuleInfo, TypeTables, NumPtags, 
		FunctorInfo, LayoutInfo, TypeCtorRep) :-
	module_info_globals(ModuleInfo, Globals),
	(
		globals__lookup_bool_option(Globals, reserve_tag, yes)
	->
		InitTag = 1
	;
		InitTag = 0
	),
	map__init(TagMap0),
	map__init(ReservedAddrMap0),
	type_ctor_info__make_du_functor_tables(Ctors, InitTag, ConsTagMap,
		RttiTypeCtor, ModuleInfo,
		FunctorDescs, SortInfo0, TagMap0, TagMap,
		ReservedAddrMap0, ReservedAddrMap),
	list__sort(SortInfo0, SortInfo),
	assoc_list__values(SortInfo, NameOrderedRttiNames),

	NameOrderedTable = du_name_ordered_table(RttiTypeCtor,
		NameOrderedRttiNames),
	NameOrderedTableRttiName = du_name_ordered_table,
	FunctorInfo = du_functors(NameOrderedTableRttiName),

	type_ctor_info__make_du_ptag_ordered_table(TagMap, InitTag, MaxPtag,
		RttiTypeCtor, ValueOrderedTableRttiName, ValueOrderedTables,
		NumPtags),
	DuLayoutInfo = du_layout(ValueOrderedTableRttiName),
	list__append([NameOrderedTable | FunctorDescs], ValueOrderedTables,
		TypeTables0),
	( map__is_empty(ReservedAddrMap) ->
		TypeTables = TypeTables0,
		LayoutInfo = DuLayoutInfo,
		TypeCtorRep = du(EqualityAxioms)
	;
		type_ctor_info__make_reserved_addr_layout(RttiTypeCtor,
			ReservedAddrMap, ValueOrderedTableRttiName,
			RALayoutRttiName, RALayoutTables),
				% XXX does it matter what order they go in?
		TypeTables = RALayoutTables ++ TypeTables0,
		LayoutInfo = reserved_addr_layout(RALayoutRttiName),
		TypeCtorRep = reserved_addr(EqualityAxioms)
	).

:- pred type_ctor_info__make_reserved_addr_layout(rtti_type_ctor::in,
		reserved_addr_map::in, rtti_name::in,
		rtti_name::out, list(rtti_data)::out) is det.

type_ctor_info__make_reserved_addr_layout(RttiTypeCtor, ReservedAddrMap,
		DuTableRttiName, RALayoutRttiName, RALayoutTables) :-
	%
	% split the reserved addresses into numeric addresses (including null)
	% and symbolic addresses.
	%
	ReservedAddrAssocList = map__to_sorted_assoc_list(ReservedAddrMap),
	list__filter((pred(RA - _::in) is semidet :-
			RA = reserved_object(_, _, _)),
		ReservedAddrAssocList,
		SymbolicAddrAssocList, NumericAddrAssocList),

	%
	% fill in the tables pointed to by the reserved_addr_table
	%
	SymbolicAddrList = assoc_list__keys(SymbolicAddrAssocList),
	SymbolicAddrTable = reserved_addrs(RttiTypeCtor, SymbolicAddrList),
	ReservedAddrFunctorDescTables =
			assoc_list__values(ReservedAddrAssocList),
	ReservedAddrFunctorDescs = list__map(
		(func(RAFD) = Name :-
			rtti_data_to_name(RAFD, _RttiTypeCtor, Name)),
		ReservedAddrFunctorDescTables),
	ReservedAddrFunctorTable = reserved_addr_functors(
			RttiTypeCtor, ReservedAddrFunctorDescs),
	%
	% fill in the reserved_addr_table,
	% which describes the representation of this type
	%
	NumNumericReservedAddrs = list__length(NumericAddrAssocList),
	NumSymbolicReservedAddrs = list__length(SymbolicAddrAssocList),
	RALayoutTable = reserved_addr_table(RttiTypeCtor,
		NumNumericReservedAddrs,
		NumSymbolicReservedAddrs,
		reserved_addrs,
		reserved_addr_functors,
		DuTableRttiName),
	RALayoutRttiName = reserved_addr_table,
	
	% put it all together
	RALayoutTables = ReservedAddrFunctorDescTables ++
		[ReservedAddrFunctorTable, SymbolicAddrTable, RALayoutTable].

% Create a du_functor_desc structure for each functor in a du type.
% Besides returning a list of the rtti names of their du_functor_desc
% structures, we return two other items of information. The SortInfo
% enables our caller to sort these rtti names on functor name and then arity,
% which is how the type functors structure is constructed. The TagMap
% groups the rttis into groups depending on their primary tags; this is
% how the type layout structure is constructed.

:- type cons_representation
	--->	reserved_address(reserved_address)
	;	tagged_data(
			tag_bits,	% primary tag value
			sectag_locn,	% secondary tag location
			int		% secondary tag value
		).

:- pred type_ctor_info__make_du_functor_tables(list(constructor)::in,
	int::in, cons_tag_values::in, rtti_type_ctor::in, module_info::in,
	list(rtti_data)::out, name_sort_info::out,
	tag_map::in, tag_map::out,
	reserved_addr_map::in, reserved_addr_map::out) is det.

type_ctor_info__make_du_functor_tables([], _, _, _, _,
		[], [], TagMap, TagMap, RAMap, RAMap).
type_ctor_info__make_du_functor_tables([Functor | Functors], Ordinal,
		ConsTagMap, RttiTypeCtor, ModuleInfo,
		Tables, SortInfo, TagMap0, TagMap, RAMap0, RAMap) :-
	Functor = ctor(ExistTvars, Constraints, SymName, FunctorArgs),
	list__length(FunctorArgs, Arity),
	unqualify_name(SymName, FunctorName),
	RttiName = du_functor_desc(Ordinal),
	make_cons_id_from_qualified_sym_name(SymName, FunctorArgs, ConsId),
	map__lookup(ConsTagMap, ConsId, ConsTag),
	type_ctor_info__process_cons_tag(ConsTag, RttiName, ConsRep,
		TagMap0, TagMap1),
	(	
		ConsRep = tagged_data(Ptag, Locn, Stag),
		RAMap1 = RAMap0
	;
		ConsRep = reserved_address(RA),
		RAFunctorDesc = reserved_addr_functor_desc(RttiTypeCtor,
			FunctorName, Ordinal, RA),
		RAMap1 = map__det_insert(RAMap0, RA, RAFunctorDesc),
		% These three fields are not really used for
		% reserved_address const tags, but we need to fill
		% them in with something...
		Ptag = 0,
		Stag = 0,
		Locn = sectag_none
	),
	type_ctor_info__generate_arg_info_tables(ModuleInfo,
		RttiTypeCtor, Ordinal, FunctorArgs, ExistTvars,
		MaybeArgNames,
		ArgPseudoTypeInfoVector, FieldTables, ContainsVarBitVector),
	( ExistTvars = [] ->
		MaybeExistInfo = no,
		ExistTables = []
	;
		module_info_classes(ModuleInfo, ClassTable),
		type_ctor_info__generate_type_info_locns(ExistTvars,
			Constraints, ClassTable, RttiTypeCtor, Ordinal,
			ExistInfo, ExistTables),
		MaybeExistInfo = yes(ExistInfo)
	),
	list__append(FieldTables, ExistTables, SubTables),
	FunctorDesc = du_functor_desc(RttiTypeCtor, FunctorName, Ptag, Stag,
		Locn, Ordinal, Arity, ContainsVarBitVector,
		ArgPseudoTypeInfoVector, MaybeArgNames, MaybeExistInfo),
	FunctorSortInfo = (FunctorName - Arity) - RttiName,
	type_ctor_info__make_du_functor_tables(Functors, Ordinal + 1,
		ConsTagMap, RttiTypeCtor, ModuleInfo,
		Tables1, SortInfo1, TagMap1, TagMap, RAMap1, RAMap),
	list__append([FunctorDesc | SubTables], Tables1, Tables),
	SortInfo = [FunctorSortInfo | SortInfo1].

:- pred type_ctor_info__process_cons_tag(cons_tag::in, rtti_name::in,
		cons_representation::out, tag_map::in, tag_map::out) is det.

type_ctor_info__process_cons_tag(ConsTag, RttiName, ConsRep,
		TagMap0, TagMap) :-
	(
		( ConsTag = single_functor, ConsPtag = 0
		; ConsTag = unshared_tag(ConsPtag)
		)
	->
		Locn = sectag_none,
		Ptag = ConsPtag,
		Stag = 0,
		type_ctor_info__update_tag_info(Ptag, Stag, Locn, RttiName,
			TagMap0, TagMap),
		ConsRep = tagged_data(Ptag, Locn, Stag)
	; ConsTag = shared_local_tag(ConsPtag, ConsStag) ->
		Locn = sectag_local,
		Ptag = ConsPtag,
		Stag = ConsStag,
		type_ctor_info__update_tag_info(Ptag, Stag, Locn, RttiName,
			TagMap0, TagMap),
		ConsRep = tagged_data(Ptag, Locn, Stag)
	; ConsTag = shared_remote_tag(ConsPtag, ConsStag) ->
		Locn = sectag_remote,
		Ptag = ConsPtag,
		Stag = ConsStag,
		type_ctor_info__update_tag_info(Ptag, Stag, Locn, RttiName,
			TagMap0, TagMap),
		ConsRep = tagged_data(Ptag, Locn, Stag)
	; ConsTag = reserved_address(RA) ->
		ConsRep = reserved_address(RA),
		TagMap = TagMap0
	; ConsTag = shared_with_reserved_addresses(_RAs, ThisTag) ->
		% here we can just ignore the fact that this cons_tag is
		% shared with reserved addresses
		type_ctor_info__process_cons_tag(ThisTag, RttiName,
			ConsRep, TagMap0, TagMap)
	;
		unexpected(this_file, "cons_tag for du function symbol")
	).

% Generate the tables that describe the arguments of a functor. 

:- pred type_ctor_info__generate_arg_info_tables(module_info::in,
	rtti_type_ctor::in, int::in, list(constructor_arg)::in,
	existq_tvars::in, maybe(rtti_name)::out, maybe(rtti_name)::out,
	list(rtti_data)::out, int::out) is det.

type_ctor_info__generate_arg_info_tables(
		ModuleInfo, RttiTypeCtor, Ordinal, Args, ExistTvars,
		MaybeFieldNamesRttiName, MaybeFieldTypesRttiName, Tables,
		ContainsVarBitVector) :-
	RttiTypeCtor = rtti_type_ctor(_TypeModule, _TypeName, TypeArity),
	type_ctor_info__generate_arg_infos(Args, TypeArity, ExistTvars,
		ModuleInfo, MaybeArgNames, PseudoTypeInfos,
		0, 0, ContainsVarBitVector, [], Tables0),
	(
		PseudoTypeInfos = [],
		MaybeFieldTypesRttiName = no,
		Tables1 = Tables0
	;
		PseudoTypeInfos = [_|_],
		FieldTypesTable = field_types(RttiTypeCtor, Ordinal,
			PseudoTypeInfos),
		FieldTypesRttiName = field_types(Ordinal),
		MaybeFieldTypesRttiName = yes(FieldTypesRttiName),
		Tables1 = [FieldTypesTable | Tables0]
	),
	list__filter((lambda([MaybeName::in] is semidet, MaybeName = yes(_))),
		MaybeArgNames, FieldNames),
	(
		FieldNames = [],
		MaybeFieldNamesRttiName = no,
		Tables = Tables1
	;
		FieldNames = [_|_],
		FieldNameTable = field_names(RttiTypeCtor, Ordinal,
			MaybeArgNames),
		FieldNamesRttiName = field_names(Ordinal),
		MaybeFieldNamesRttiName = yes(FieldNamesRttiName),
		Tables = [FieldNameTable | Tables1]
	).

% For each argument of a functor, return three items of information:
% its name (if any), a rtti_data for the pseudotypeinfo describing
% its type, and an indication whether the type
% contains variables or not. The last item is encoded as an integer
% which contains a 1 bit in the position given by 1 << N if argument N's type
% contains variables (assuming that arguments are numbered starting from zero).
% The number of bits in the integer is given by contains_var_bit_vector_size;
% arguments beyond this limit do not contribute to this bit vector.

:- pred type_ctor_info__generate_arg_infos(list(constructor_arg)::in,
	int::in, existq_tvars::in, module_info::in, list(maybe(string))::out,
	list(rtti_data)::out, int::in, int::in, int::out,
	list(rtti_data)::in, list(rtti_data)::out) is det.

type_ctor_info__generate_arg_infos([], _, _, _, [], [],
		_, ContainsVarBitVector, ContainsVarBitVector, Tables, Tables).
type_ctor_info__generate_arg_infos([MaybeArgSymName - ArgType | Args],
		NumUnivTvars, ExistTvars, ModuleInfo,
		[MaybeArgName | MaybeArgNames], [RttiData | RttiDatas],
		ArgNum, ContainsVarBitVector0, ContainsVarBitVector,
		Tables0, Tables) :-
	(
		MaybeArgSymName = yes(SymName),
		unqualify_name(SymName, ArgName),
		MaybeArgName = yes(ArgName)
	;
		MaybeArgSymName = no,
		MaybeArgName = no
	),
	make_pseudo_type_info_and_tables(ArgType, NumUnivTvars, ExistTvars,
		RttiData, Tables0, Tables1),
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
		ExistTvars, ModuleInfo, MaybeArgNames, RttiDatas,
		ArgNum + 1, ContainsVarBitVector1, ContainsVarBitVector,
		Tables1, Tables).

% This function gives the size of the MR_du_functor_arg_type_contains_var
% field of the C type MR_DuFunctorDesc in bits.

:- func type_ctor_info__contains_var_bit_vector_size = int.

type_ctor_info__contains_var_bit_vector_size = 16.

% Construct the RTTI structures that record information about the locations
% of the typeinfos describing the types of the existentially typed arguments
% of a functor.

:- pred type_ctor_info__generate_type_info_locns(list(tvar)::in,
	list(class_constraint)::in, class_table::in, rtti_type_ctor::in,
	int::in, rtti_name::out, list(rtti_data)::out) is det.

type_ctor_info__generate_type_info_locns(ExistTvars, Constraints, ClassTable,
		RttiTypeCtor, Ordinal, exist_info(Ordinal),
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
	ExistInfo = exist_info(RttiTypeCtor, Ordinal,
		TIsPlain, TIsInTCIs, TCIs, exist_locns(Ordinal)),
	list__map((pred(Tvar::in, Locn::out) is det :-
		map__lookup(LocnMap, Tvar, Locn)),
		ExistTvars, Locns),
	ExistLocns = exist_locns(RttiTypeCtor, Ordinal, Locns).

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
	error("first_matching_type_class_info: not found").
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

:- pred type_ctor_info__make_du_ptag_ordered_table(tag_map::in, 
	int::in, int::in, rtti_type_ctor::in, 
	rtti_name::out, list(rtti_data)::out, int::out) is det.

type_ctor_info__make_du_ptag_ordered_table(TagMap, MinPtagValue, MaxPtagValue,
		RttiTypeCtor, PtagOrderedRttiName, Tables, NumPtags) :-
	map__to_assoc_list(TagMap, TagList),
	type_ctor_info__make_du_ptag_layouts(TagList, 
		MinPtagValue, MaxPtagValue, RttiTypeCtor, 
		PtagLayouts, SubTables, NumPtags),
	PtagOrderedTable = du_ptag_ordered_table(RttiTypeCtor, PtagLayouts),
	PtagOrderedRttiName = du_ptag_ordered_table,
	Tables = [PtagOrderedTable | SubTables].

:- pred type_ctor_info__make_du_ptag_layouts(tag_list::in, int::in, int::in,
	rtti_type_ctor::in, list(du_ptag_layout)::out, list(rtti_data)::out,
	int::out) is det.

type_ctor_info__make_du_ptag_layouts(TagList0, CurPtag, MaxPtag,
		RttiTypeCtor, PtagLayouts, Tables, NumPtags) :-
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
		StagOrderedTable = du_stag_ordered_table(RttiTypeCtor,
			Ptag, StagRttiNames),
		StagOrderedAddr = du_stag_ordered_table(Ptag),
		PtagLayout = du_ptag_layout(StagListLength, Locn,
			StagOrderedAddr),
		type_ctor_info__make_du_ptag_layouts(TagList,
			CurPtag + 1, MaxPtag, RttiTypeCtor,
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

:- func this_file = string.
this_file = "type_ctor_info.m".

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
