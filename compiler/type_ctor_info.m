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

	% Compute the "contains var" bit vector. The input is a list describing
	% the types of the arguments of a function symbol. The output is an
	% bit vector (represented as a 16 bit integer) in which each bit is set
	% if the type of the corresponding argument contains a type variable.
	% If the function symbol has more than 16 arguments, then the last bit
	% is true if any of the arguments after the 15th contain a type
	% variable in their type.

:- func compute_contains_var_bit_vector(
	list(rtti_maybe_pseudo_type_info_or_self)) = int.

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
		Unify = proc(UnifyPredId, UnifyProcId),

		map__lookup(SpecMap, compare - TypeCtor, ComparePredId),
		special_pred_mode_num(compare, CompareProcInt),
		proc_id_to_int(CompareProcId, CompareProcInt),
		Compare = proc(ComparePredId, CompareProcId)
	;
		module_info_get_predicate_table(ModuleInfo, PredTable),
		mercury_private_builtin_module(PrivateBuiltin),
		(
			predicate_table_search_pred_m_n_a(PredTable,
				PrivateBuiltin, "unused", 0, PredIds),
			PredIds = [PredId]
		->
			get_proc_id(ModuleInfo, PredId, ProcId),
			Unused = proc(PredId, ProcId),
			Unify = Unused,
			Compare = Unused
		;
			error("type_ctor_info__gen_type_ctor_gen_info: no unique unused predicate")
		)
	),
	TypeCtorGenInfo = type_ctor_gen_info(TypeCtor, ModuleName, TypeName,
		TypeArity, Status, TypeDefn, Unify, Compare).

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
		ModuleInfo, TypeCtorCModule),
	Dynamic1 = [TypeCtorCModule | Dynamic0],
	type_ctor_info__construct_type_ctor_infos(TypeCtorGenInfos,
		ModuleInfo, Dynamic1, Dynamic, Static0, Static).

	% Generate RTTI information for the given type.

:- pred type_ctor_info__construct_type_ctor_info(type_ctor_gen_info::in,
	module_info::in, rtti_data::out) is det.

type_ctor_info__construct_type_ctor_info(TypeCtorGenInfo, ModuleInfo,
		RttiData) :-
	TypeCtorGenInfo = type_ctor_gen_info(_TypeCtor, ModuleName, TypeName,
		TypeArity, _Status, HldsDefn, UnifyPredProcId,
		ComparePredProcId),
	type_ctor_info__make_proc_label(UnifyPredProcId, ModuleInfo,
		UnifyProcLabel),
	type_ctor_info__make_proc_label(ComparePredProcId, ModuleInfo,
		CompareProcLabel),
	type_to_univ(UnifyProcLabel, UnifyUniv),
	type_to_univ(CompareProcLabel, CompareUniv),
	module_info_globals(ModuleInfo, Globals),
	hlds_data__get_type_defn_body(HldsDefn, TypeBody),
	Version = type_ctor_info_rtti_version,
	(
		TypeBody = abstract_type,
		error("type_ctor_info__gen_type_ctor_data: abstract_type")
	;
		TypeBody = foreign_type(_),
		Details = foreign
	;
		TypeBody = eqv_type(Type),
			% There can be no existentially typed args to an
			% equivalence.
		UnivTvars = TypeArity,
		ExistTvars = [],
		pseudo_type_info__construct_maybe_pseudo_type_info(Type,
			UnivTvars, ExistTvars, MaybePseudoTypeInfo),
		Details = eqv(MaybePseudoTypeInfo)
	;
		TypeBody = du_type(Ctors, ConsTagMap, Enum, EqualityPred, _),
		(
			EqualityPred = yes(_),
			EqualityAxioms = user_defined
		;
			EqualityPred = no,
			EqualityAxioms = standard
		),
		globals__lookup_bool_option(Globals, reserve_tag, ReserveTag),
		(
			Enum = yes,
			type_ctor_info__make_enum_details(Ctors, ConsTagMap,
				ReserveTag, EqualityAxioms, Details)
		;
			Enum = no,
			(
				type_constructors_should_be_no_tag(Ctors, 
					Globals, Name, ArgType, MaybeArgName)
			->
				type_ctor_info__make_notag_details(TypeArity,
					Name, ArgType, MaybeArgName,
					EqualityAxioms, Details)
			;
				type_ctor_info__make_du_details(Ctors,
					ConsTagMap, TypeArity, EqualityAxioms,
					ModuleInfo, Details)
			)
		)
	),
	TypeCtorData = type_ctor_data(Version, ModuleName, TypeName, TypeArity,
		UnifyUniv, CompareUniv, Details),
	RttiData = type_ctor_info(TypeCtorData).

:- pred type_ctor_info__make_proc_label(pred_proc_id::in, module_info::in,
	rtti_proc_label::out) is det.

type_ctor_info__make_proc_label(PredProcId, ModuleInfo, ProcLabel) :-
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
	% the handwritten type_ctor_info structures (and the macros that
	% generate them) as well as the code in the runtime that uses RTTI
	% to conform to whatever changes the new version introduces.

:- func type_ctor_info_rtti_version = int.

type_ctor_info_rtti_version = 7.

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

:- pred type_ctor_info__make_notag_details(int::in, sym_name::in, (type)::in,
	maybe(string)::in, equality_axioms::in, type_ctor_details::out) is det.

type_ctor_info__make_notag_details(TypeArity, SymName, ArgType, MaybeArgName,
		EqualityAxioms, Details) :-
	unqualify_name(SymName, FunctorName),
	NumUnivTvars = TypeArity,
		% There can be no existentially typed args to the functor
		% in a notag type.
	ExistTvars = [],
	pseudo_type_info__construct_maybe_pseudo_type_info(ArgType,
		NumUnivTvars, ExistTvars, MaybePseudoTypeInfo),
	Functor = notag_functor(FunctorName, MaybePseudoTypeInfo,
		MaybeArgName),
	Details = notag(EqualityAxioms, Functor).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type name_sort_info == assoc_list(pair(string, int), rtti_name).

% Make the functor and layout tables for an enum type.

:- pred type_ctor_info__make_enum_details(list(constructor)::in,
	cons_tag_values::in, bool::in, equality_axioms::in,
	type_ctor_details::out) is det.

type_ctor_info__make_enum_details(Ctors, ConsTagMap, ReserveTag,
		EqualityAxioms, Details) :-
	( ReserveTag = yes ->
		unexpected("type_ctor_info", "enum in .rt grade")
	;
		true
	),
	type_ctor_info__make_enum_functors(Ctors, 0, ConsTagMap, EnumFunctors),
	ValueMap0 = map__init,
	NameMap0 = map__init,
	list__foldl2(type_ctor_info__make_enum_maps, EnumFunctors,
		ValueMap0, ValueMap, NameMap0, NameMap),
	Details = enum(EqualityAxioms, EnumFunctors, ValueMap, NameMap).

% Create an enum_functor structure for each functor in an enum type.
% The functors are given to us in ordinal order (since that's how the HLDS
% stored them), and that is how we return the list of rtti names of the
% enum_functor_desc structures; that way, it is directly usable in the type
% layout structure. We also return a structure that allows our caller to
% sort this list on functor name, which is how the type functors structure
% is constructed.

:- pred type_ctor_info__make_enum_functors(list(constructor)::in,
	int::in, cons_tag_values::in, list(enum_functor)::out) is det.

type_ctor_info__make_enum_functors([], _, _, []).
type_ctor_info__make_enum_functors([Functor | Functors], NextOrdinal0,
		ConsTagMap, [EnumFunctor | EnumFunctors]) :-
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
	EnumFunctor = enum_functor(FunctorName, NextOrdinal0),
	type_ctor_info__make_enum_functors(Functors, NextOrdinal0 + 1,
		ConsTagMap, EnumFunctors).

:- pred type_ctor_info__make_enum_maps(enum_functor::in,
	map(int, enum_functor)::in, map(int, enum_functor)::out,
	map(string, enum_functor)::in, map(string, enum_functor)::out) is det.

type_ctor_info__make_enum_maps(EnumFunctor, ValueMap0, ValueMap,
		NameMap0, NameMap) :-
	EnumFunctor = enum_functor(FunctorName, Ordinal),
	map__det_insert(ValueMap0, Ordinal, EnumFunctor, ValueMap),
	map__det_insert(NameMap0, FunctorName, EnumFunctor, NameMap).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type tag_map == map(int, pair(sectag_locn, map(int, rtti_name))).
:- type tag_list == assoc_list(int, pair(sectag_locn, map(int, rtti_name))).

:- type reserved_addr_map == map(reserved_address, rtti_data).

:- func is_du_functor(maybe_reserved_functor::in) = (du_functor::out)
	is semidet.

is_du_functor(du_func(DuFunctor)) = DuFunctor.

:- func is_reserved_functor(maybe_reserved_functor::in) =
	(reserved_functor::out) is semidet.

is_reserved_functor(res_func(ResFunctor)) = ResFunctor.

% Make the functor and layout tables for a du type
% (including reserved_addr types).

:- pred type_ctor_info__make_du_details(list(constructor)::in,
	cons_tag_values::in, int::in, equality_axioms::in, module_info::in,
	type_ctor_details::out) is det.

type_ctor_info__make_du_details(Ctors, ConsTagMap, TypeArity, EqualityAxioms,
		ModuleInfo, Details) :-
	type_ctor_info__make_maybe_res_functors(Ctors, 0, ConsTagMap,
		TypeArity, ModuleInfo, MaybeResFunctors),
	DuFunctors = list__filter_map(is_du_functor, MaybeResFunctors),
	ResFunctors = list__filter_map(is_reserved_functor, MaybeResFunctors),
	list__foldl(type_ctor_info__make_du_ptag_ordered_table, DuFunctors,
		map__init, DuPtagTable),
	( ResFunctors = [] ->
		list__foldl(type_ctor_info__make_du_name_ordered_table,
			DuFunctors, map__init, DuNameOrderedMap),
		Details = du(EqualityAxioms, DuFunctors, DuPtagTable,
			DuNameOrderedMap)
	;
		list__foldl(type_ctor_info__make_res_name_ordered_table,
			MaybeResFunctors, map__init, ResNameOrderedMap),
		Details = reserved(EqualityAxioms, MaybeResFunctors,
			ResFunctors, DuPtagTable, ResNameOrderedMap)
	).

% Create a du_functor_desc structure for each functor in a du type.
% Besides returning a list of the rtti names of their du_functor_desc
% structures, we return two other items of information. The SortInfo
% enables our caller to sort these rtti names on functor name and then arity,
% which is how the type functors structure is constructed. The TagMap
% groups the rttis into groups depending on their primary tags; this is
% how the type layout structure is constructed.

:- type maybe_reserved_rep
	--->	reserved_rep(
			reserved_address
		)
	;	du_rep(
			du_rep
		).

:- pred type_ctor_info__make_maybe_res_functors(list(constructor)::in,
	int::in, cons_tag_values::in, int::in, module_info::in,
	list(maybe_reserved_functor)::out) is det.

type_ctor_info__make_maybe_res_functors([], _, _, _, _, []).
type_ctor_info__make_maybe_res_functors([Functor | Functors], NextOrdinal,
		ConsTagMap, TypeArity, ModuleInfo,
		[MaybeResFunctor | MaybeResFunctors]) :-
	Functor = ctor(ExistTvars, Constraints, SymName, ConstructorArgs),
	list__length(ConstructorArgs, Arity),
	unqualify_name(SymName, FunctorName),
	make_cons_id_from_qualified_sym_name(SymName, ConstructorArgs, ConsId),
	map__lookup(ConsTagMap, ConsId, ConsTag),
	type_ctor_info__process_cons_tag(ConsTag, ConsRep),
	list__map(type_ctor_info__generate_du_arg_info(TypeArity, ExistTvars),
		ConstructorArgs, ArgInfos),
	( ExistTvars = [] ->
		MaybeExistInfo = no
	;
		module_info_classes(ModuleInfo, ClassTable),
		type_ctor_info__generate_exist_into(ExistTvars,
			Constraints, ClassTable, ExistInfo),
		MaybeExistInfo = yes(ExistInfo)
	),
	(	
		ConsRep = du_rep(DuRep),
		DuFunctor = du_functor(FunctorName, Arity, NextOrdinal,
			DuRep, ArgInfos, MaybeExistInfo),
		MaybeResFunctor = du_func(DuFunctor)
	;
		ConsRep = reserved_rep(ResRep),
		require(unify(Arity, 0),
			"type_ctor_info__make_maybe_res_functors: bad arity"),
		require(unify(ArgInfos, []),
			"type_ctor_info__make_maybe_res_functors: bad args"),
		require(unify(MaybeExistInfo, no),
			"type_ctor_info__make_maybe_res_functors: bad exist"),
		ResFunctor = reserved_functor(FunctorName, NextOrdinal,
			ResRep),
		MaybeResFunctor = res_func(ResFunctor)
	),
	type_ctor_info__make_maybe_res_functors(Functors, NextOrdinal + 1,
		ConsTagMap, TypeArity, ModuleInfo, MaybeResFunctors).

:- pred type_ctor_info__process_cons_tag(cons_tag::in, maybe_reserved_rep::out)
	is det.

type_ctor_info__process_cons_tag(ConsTag, ConsRep) :-
	(
		( ConsTag = single_functor, ConsPtag = 0
		; ConsTag = unshared_tag(ConsPtag)
		)
	->
		ConsRep = du_rep(du_ll_rep(ConsPtag, sectag_none))
	;
		ConsTag = shared_local_tag(ConsPtag, ConsStag)
	->
		ConsRep = du_rep(du_ll_rep(ConsPtag, sectag_local(ConsStag)))
	;
		ConsTag = shared_remote_tag(ConsPtag, ConsStag)
	->
		ConsRep = du_rep(du_ll_rep(ConsPtag, sectag_remote(ConsStag)))
	;
		ConsTag = reserved_address(ReservedAddr)
	->
		ConsRep = reserved_rep(ReservedAddr)
	;
		ConsTag = shared_with_reserved_addresses(_RAs, ThisTag)
	->
		% here we can just ignore the fact that this cons_tag is
		% shared with reserved addresses
		type_ctor_info__process_cons_tag(ThisTag, ConsRep)
	;
		unexpected(this_file, "bad cons_tag for du function symbol")
	).

:- pred type_ctor_info__generate_du_arg_info(int::in, existq_tvars::in,
	constructor_arg::in, du_arg_info::out) is det.

type_ctor_info__generate_du_arg_info(NumUnivTvars, ExistTvars, ConstructorArg,
		ArgInfo) :-
	ConstructorArg = MaybeArgSymName - ArgType,
	(
		MaybeArgSymName = yes(SymName),
		unqualify_name(SymName, ArgName),
		MaybeArgName = yes(ArgName)
	;
		MaybeArgSymName = no,
		MaybeArgName = no
	),
	% The C runtime cannot yet handle the "self" type representation,
	% so we do not generate it here.
	pseudo_type_info__construct_maybe_pseudo_type_info(ArgType,
		NumUnivTvars, ExistTvars, MaybePseudoTypeInfo),
	(
		MaybePseudoTypeInfo = plain(TypeInfo),
		MaybePseudoTypeInfoOrSelf = plain(TypeInfo)
		;
		MaybePseudoTypeInfo = pseudo(PseudoTypeInfo),
		MaybePseudoTypeInfoOrSelf = pseudo(PseudoTypeInfo)
	),
	ArgInfo = du_arg_info(MaybeArgName, MaybePseudoTypeInfoOrSelf).

% This function gives the size of the MR_du_functor_arg_type_contains_var
% field of the C type MR_DuFunctorDesc in bits.

:- func type_ctor_info__contains_var_bit_vector_size = int.

type_ctor_info__contains_var_bit_vector_size = 16.

% Construct the RTTI structures that record information about the locations
% of the typeinfos describing the types of the existentially typed arguments
% of a functor.

:- pred type_ctor_info__generate_exist_into(list(tvar)::in,
	list(class_constraint)::in, class_table::in, exist_info::out) is det.

type_ctor_info__generate_exist_into(ExistTvars, Constraints, ClassTable,
		ExistInfo) :-
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
	list__map((pred(Tvar::in, Locn::out) is det :-
		map__lookup(LocnMap, Tvar, Locn)),
		ExistTvars, ExistLocns),
	ExistInfo = exist_info(TIsPlain, TIsInTCIs, TCIs, ExistLocns).

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

:- pred type_ctor_info__make_du_ptag_ordered_table(du_functor::in,
	map(int, sectag_table)::in, map(int, sectag_table)::out) is det.

type_ctor_info__make_du_ptag_ordered_table(DuFunctor, PtagTable0, PtagTable) :-
	DuRep = DuFunctor ^ du_rep,
	(
		DuRep = du_ll_rep(Ptag, SectagAndLocn),
		(
			SectagAndLocn = sectag_none,
			SectagLocn = sectag_none,
			Sectag = 0
		;
			SectagAndLocn = sectag_local(Sectag),
			SectagLocn = sectag_local
		;
			SectagAndLocn = sectag_remote(Sectag),
			SectagLocn = sectag_remote
		),
		( map__search(PtagTable0, Ptag, SectagTable0) ->
			SectagTable0 = sectag_table(Locn0, NumSharers0,
				SectagMap0),
			require(unify(SectagLocn, Locn0),
				"type_ctor_info__make_du_ptag_ordered_table: sectag locn disagreement"),
			map__det_insert(SectagMap0, Sectag, DuFunctor,
				SectagMap),
			SectagTable = sectag_table(Locn0, NumSharers0 + 1,
				SectagMap),
			map__det_update(PtagTable0, Ptag, SectagTable,
				PtagTable)
		;
			SectagMap0 = map__init,
			map__det_insert(SectagMap0, Sectag, DuFunctor,
				SectagMap),
			SectagTable = sectag_table(SectagLocn, 1, SectagMap),
			map__det_insert(PtagTable0, Ptag, SectagTable,
				PtagTable)
		)
	;
		DuRep = du_hl_rep(_),
		error("type_ctor_info__make_du_ptag_ordered_table: du_hl_rep")
	).

:- pred type_ctor_info__make_du_name_ordered_table(du_functor::in,
	map(string, map(int, du_functor))::in,
	map(string, map(int, du_functor))::out) is det.

type_ctor_info__make_du_name_ordered_table(DuFunctor, NameTable0, NameTable) :-
	Name = DuFunctor ^ du_name,
	Arity = DuFunctor ^ du_orig_arity,
	( map__search(NameTable0, Name, NameMap0) ->
		map__det_insert(NameMap0, Arity, DuFunctor, NameMap),
		map__det_update(NameTable0, Name, NameMap, NameTable)
	;
		NameMap = map__det_insert(map__init, Arity, DuFunctor),
		map__det_insert(NameTable0, Name, NameMap, NameTable)
	).

:- pred type_ctor_info__make_res_name_ordered_table(maybe_reserved_functor::in,
	map(string, map(int, maybe_reserved_functor))::in,
	map(string, map(int, maybe_reserved_functor))::out) is det.

type_ctor_info__make_res_name_ordered_table(MaybeResFunctor,
		NameTable0, NameTable) :-
	(
		MaybeResFunctor = du_func(DuFunctor),
		Name = DuFunctor ^ du_name,
		Arity = DuFunctor ^ du_orig_arity
	;
		MaybeResFunctor = res_func(ResFunctor),
		Name = ResFunctor ^ res_name,
		Arity = 0
	),
	( map__search(NameTable0, Name, NameMap0) ->
		map__det_insert(NameMap0, Arity, MaybeResFunctor, NameMap),
		map__det_update(NameTable0, Name, NameMap, NameTable)
	;
		NameMap = map__det_insert(map__init, Arity, MaybeResFunctor),
		map__det_insert(NameTable0, Name, NameMap, NameTable)
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

compute_contains_var_bit_vector(ArgTypes) =
	compute_contains_var_bit_vector_2(ArgTypes, 0, 0).

:- func
compute_contains_var_bit_vector_2(
	list(rtti_maybe_pseudo_type_info_or_self), int, int) = int.

compute_contains_var_bit_vector_2([], _, Vector) = Vector.
compute_contains_var_bit_vector_2([ArgType | ArgTypes], ArgNum, Vector0) =
		Vector :-
	(
		ArgType = plain(_),
		Vector1 = Vector0
	;
		ArgType = pseudo(_),
		Vector1 = update_contains_var_bit_vector(Vector0, ArgNum)
	;
		ArgType = self,
		% The backend currently doesn't perform the optimization that
		% lets it avoid memory allocation on self types.
		Vector1 = update_contains_var_bit_vector(Vector0, ArgNum)
	),
	Vector = compute_contains_var_bit_vector_2(ArgTypes, ArgNum + 1,
		Vector1).

:- func update_contains_var_bit_vector(int, int) = int.

update_contains_var_bit_vector(Vector0, ArgNum) = Vector :-
	( ArgNum >= contains_var_bit_vector_size - 1 ->
		BitNum = contains_var_bit_vector_size - 1
	;
		BitNum = ArgNum
	),
	Vector = Vector0 \/ (1 << BitNum).

%---------------------------------------------------------------------------%

:- func this_file = string.
this_file = "type_ctor_info.m".

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
