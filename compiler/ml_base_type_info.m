%---------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module generates the MLDS code that defines the static constants
% that hold the type_ctor_info structures of the types defined by the
% current module.
%
% See polymorphism.m for a description of the various ways to represent
% type information, including a description of the type_ctor_info structures.
%
% WARNING: if you change this file, you will probably also need to
% modify base_type_info.m, which does the same thing for the LLDS back-end.
%
% Author: fjh.
%
%---------------------------------------------------------------------------%

:- module ml_base_type_info.

:- interface.

:- import_module hlds_module, mlds.

:- pred ml_base_type_info__generate_mlds(module_info, mlds__defns).
:- mode ml_base_type_info__generate_mlds(in, out) is det.

:- implementation.
:- import_module base_type_info, ml_code_util.

:- import_module base_typeclass_info.
:- import_module prog_data, prog_util, prog_out.
:- import_module hlds_data, hlds_pred, hlds_out.
:- import_module code_util, special_pred, type_util, globals, options.

:- import_module list.
:- import_module bool, string, map, std_util, require.

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
type_ctor_info_rtti_version = 3.

ml_base_type_info__generate_mlds(ModuleInfo, Defns) :-
	module_info_base_gen_infos(ModuleInfo, BaseGenInfos),
	ml_base_type_info__construct_type_ctor_infos(BaseGenInfos, ModuleInfo,
		Defns).
	/***
	% XXX type classes are not yet implemented in the MLDS back-end
	ml_base_typeclass_info__generate_mlds(ModuleInfo, Defns2),
		% XXX make this use an accumulator
	list__append(Defns1, Defns2, Defns).
	***/

:- pred ml_base_type_info__construct_type_ctor_infos(list(base_gen_info),
	module_info, mlds__defns).
:- mode ml_base_type_info__construct_type_ctor_infos(in, in, out) is det.

ml_base_type_info__construct_type_ctor_infos([], _, []).
ml_base_type_info__construct_type_ctor_infos([BaseGenInfo | BaseGenInfos],
		ModuleInfo, [Defn | Defns]) :-
	BaseGenInfo = base_gen_info(_TypeId, ModuleName, TypeName, TypeArity,
		Status, Elim, Procs, HLDS_TypeDefn),

	status_is_exported(Status, Exported),
	Flags = ml_gen_base_type_info_decl_flags(Exported),

	ml_base_type_info__construct_pred_addrs(Procs, Elim, ModuleInfo, 
		PredAddrArgs),
	ArityArg = const(int_const(TypeArity)),

	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, type_layout, TypeLayoutOption),
	(
		TypeLayoutOption = yes
	->
		ml_base_type_info__construct_type_ctor_representation(HLDS_TypeDefn,
			TypeCtorArg),
		/*****
		% XXX generation of the base_type_layout and base_type_functors
		% is not yet implemented for the MLDS back-end
		ml_base_type_info__construct_layout(ModuleInfo, TypeName,
			TypeArity, LayoutArg),
		ml_base_type_info__construct_functors(ModuleInfo, TypeName,
			TypeArity, FunctorsArg),
		******/
		LayoutArg = const(int_const(0)),
		FunctorsArg = const(int_const(0)),
		prog_out__sym_name_to_string(ModuleName, ModuleNameString),
		ModuleArg = const(string_const(ModuleNameString)),
		NameArg = const(string_const(TypeName)),
		VersionArg = const(int_const(type_ctor_info_rtti_version)),
		list__append(PredAddrArgs, [TypeCtorArg, FunctorsArg, LayoutArg,
			ModuleArg, NameArg, VersionArg], FinalArgs)
	;
		FinalArgs = PredAddrArgs
	),

	DataName = type_ctor(info, TypeName, TypeArity),
	hlds_data__get_type_defn_context(HLDS_TypeDefn, Context),
	MLDS_Context = mlds__make_context(Context),
	Initializer = [ArityArg | FinalArgs],
	MLDS_Type = mlds__base_type_info_type,
	DefnBody = mlds__data(MLDS_Type, yes(Initializer)),
	Defn = mlds__defn(data(DataName), MLDS_Context, Flags, DefnBody),

	ml_base_type_info__construct_type_ctor_infos(BaseGenInfos, ModuleInfo,
		Defns).

	% Return the declaration flags appropriate for a base_type_info.
	%
:- func ml_gen_base_type_info_decl_flags(bool) = mlds__decl_flags.
ml_gen_base_type_info_decl_flags(Exported) = MLDS_DeclFlags :-
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

:- pred	ml_base_type_info__construct_layout(module_info, string, int,
		mlds__rval).
:- mode	ml_base_type_info__construct_layout(in, in, in, out) is det.
ml_base_type_info__construct_layout(ModuleInfo, TypeName, TypeArity, Rval) :-
	module_info_name(ModuleInfo, ModuleName),
	MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
	Rval = const(data_addr_const(data_addr(MLDS_ModuleName, 
		type_ctor(layout, TypeName, TypeArity)))).

:- pred ml_base_type_info__construct_functors(module_info, string, int, 
	mlds__rval).
:- mode ml_base_type_info__construct_functors(in, in, in, out) is det.
ml_base_type_info__construct_functors(ModuleInfo, TypeName, TypeArity, Rval) :-
	module_info_name(ModuleInfo, ModuleName),
	MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
	Rval = const(data_addr_const(data_addr(MLDS_ModuleName, 
		type_ctor(functors, TypeName, TypeArity)))).

:- pred ml_base_type_info__construct_pred_addrs(list(pred_proc_id), maybe(int), 
	module_info, list(mlds__rval)).
:- mode ml_base_type_info__construct_pred_addrs(in, in, in, out) is det.

ml_base_type_info__construct_pred_addrs(Procs, Elim, ModuleInfo, PredAddrArgs) :-
	( 
		% dead_proc_elim has eliminated the procs, we
		% should just put some padding in.
	
		Elim = yes(ProcsLength)
	->
		/********
		module_info_globals(ModuleInfo, Globals),
			% If eliminated, make procs point to
			% private_builtin__unused.  (Or, if static code
			% addresses are not available, use NULL
			% pointers).
		(
			globals__have_static_code_addresses(Globals, yes)
		->
			hlds_pred__initial_proc_id(ProcId),
			mercury_private_builtin_module(MercuryBuiltin),
			PredAddrArg = const(code_addr_const(
				imported(proc(MercuryBuiltin, predicate,
					MercuryBuiltin, "unused", 0,
						ProcId))))
		;
			PredAddrArg = const(int_const(0))
		),
		*******/
		PredAddrArg = const(int_const(0)),
		list__duplicate(ProcsLength, PredAddrArg, PredAddrArgs)
	;
		ml_base_type_info__construct_pred_addrs_2(Procs, ModuleInfo, 
			PredAddrArgs)
	).

:- pred ml_base_type_info__construct_pred_addrs_2(list(pred_proc_id),
		module_info, list(mlds__rval)).
:- mode ml_base_type_info__construct_pred_addrs_2(in, in, out) is det.

ml_base_type_info__construct_pred_addrs_2([], _, []).
ml_base_type_info__construct_pred_addrs_2([proc(PredId, ProcId) | Procs],
		ModuleInfo, [ProcAddrArg | ProcAddrArgs]) :-
	%
	% construct an rval for the address of this procedure
	% (this is similar to ml_gen_proc_addr_rval)
	%
        ml_gen_pred_label(ModuleInfo, PredId, ProcId, PredLabel, PredModule),
        QualifiedProcLabel = qual(PredModule, PredLabel - ProcId),
        ProcAddrRval = const(code_addr_const(proc(QualifiedProcLabel))),
	%
	% Convert the procedure address to a generic type.
	% We need to use a generic type because since the actual type
	% for the procedure will depend on how many type_info parameters
	% it takes, which will depend on the type's arity.
	%
        PredParams = ml_gen_proc_params(ModuleInfo, PredId, ProcId),
        ProcAddrArg = unop(box(mlds__func_type(PredParams)), ProcAddrRval),
	%
	% recursively handle the remaining procedures
	%
	ml_base_type_info__construct_pred_addrs_2(Procs, ModuleInfo,
		ProcAddrArgs).


:- type type_ctor_representation 
	--->	enum
	;	enum_usereq
	;	du
	;	du_usereq
	;	notag
	;	notag_usereq
	;	equiv
	;	equiv_var
	;	int
	;	char
	;	float
	;	string
	;	(pred)
	;	univ
	;	void
	;	c_pointer
	;	typeinfo
	;	typeclassinfo
	;	array
	;	unknown.

:- pred ml_base_type_info__type_ctor_rep_to_int(type_ctor_representation::in,
	int::out) is det.
ml_base_type_info__type_ctor_rep_to_int(enum, 		0).
ml_base_type_info__type_ctor_rep_to_int(enum_usereq,	1).
ml_base_type_info__type_ctor_rep_to_int(du,		2).
ml_base_type_info__type_ctor_rep_to_int(du_usereq,	3).
ml_base_type_info__type_ctor_rep_to_int(notag,		4).
ml_base_type_info__type_ctor_rep_to_int(notag_usereq,	5).
ml_base_type_info__type_ctor_rep_to_int(equiv,		6).
ml_base_type_info__type_ctor_rep_to_int(equiv_var,	7).
ml_base_type_info__type_ctor_rep_to_int(int,		8).
ml_base_type_info__type_ctor_rep_to_int(char,		9).
ml_base_type_info__type_ctor_rep_to_int(float,	 	10).
ml_base_type_info__type_ctor_rep_to_int(string,		11).
ml_base_type_info__type_ctor_rep_to_int(pred,		12).
ml_base_type_info__type_ctor_rep_to_int(univ,		13).
ml_base_type_info__type_ctor_rep_to_int(void,		14).
ml_base_type_info__type_ctor_rep_to_int(c_pointer,	15).
ml_base_type_info__type_ctor_rep_to_int(typeinfo,	16).
ml_base_type_info__type_ctor_rep_to_int(typeclassinfo,	17).
ml_base_type_info__type_ctor_rep_to_int(array,		18).
ml_base_type_info__type_ctor_rep_to_int(unknown,	19).

:- pred ml_base_type_info__construct_type_ctor_representation(hlds_type_defn,
		mlds__rval).
:- mode ml_base_type_info__construct_type_ctor_representation(in, out) is det.

ml_base_type_info__construct_type_ctor_representation(HldsType, Rvals) :-
	hlds_data__get_type_defn_body(HldsType, TypeBody),
	(
		TypeBody = uu_type(_Alts),
		error("ml_base_type_info__construct_type_ctor_representation: sorry, undiscriminated union unimplemented\n")
	;
		TypeBody = eqv_type(_Type),
		TypeCtorRep = equiv
	;
		TypeBody = abstract_type,
		TypeCtorRep = unknown
	;
		TypeBody = du_type(Ctors, _ConsTagMap, Enum, EqualityPred),
		(
			Enum = yes,
			(
				EqualityPred = yes(_),
				TypeCtorRep = enum_usereq
			;
				EqualityPred = no,
				TypeCtorRep = enum
			)
		;
			Enum = no,
			( 
				type_is_no_tag_type(Ctors, _Name, _TypeArg)
			->
				(
					EqualityPred = yes(_),
					TypeCtorRep = notag_usereq
				;
					EqualityPred = no,
					TypeCtorRep = notag
				)
			;
				(
					EqualityPred = yes(_),
					TypeCtorRep = du_usereq
				;
					EqualityPred = no,
					TypeCtorRep = du
				)
			)
		)
	),
	ml_base_type_info__type_ctor_rep_to_int(TypeCtorRep, TypeCtorRepInt),
	Rvals = const(int_const(TypeCtorRepInt)).

