%---------------------------------------------------------------------------%
% Copyright (C) 1996-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module generates the LLDS code that defines global variables
% to hold the base_typeclass_info structures of the typeclass instances defined
% by the current module.
%
% See polymorphism.m for a description of the various ways to represent
% type information, including a description of the base_typeclass_info
% structures.
%
% Author: dgj.
%
%---------------------------------------------------------------------------%

:- module base_typeclass_info.

:- interface.

:- import_module hlds_module, list, llds, prog_data.

:- pred base_typeclass_info__generate_llds(module_info, list(comp_gen_c_data)).
:- mode base_typeclass_info__generate_llds(in, out) is det.

	% Given a list of types, mangle the names so into a string which
	% identifies them. The types must all have their top level functor
	% bound, with any arguments free variables.
:- pred base_typeclass_info__make_instance_string(list(type), string).
:- mode base_typeclass_info__make_instance_string(in, out) is det.

:- implementation.

:- import_module hlds_data, hlds_pred, hlds_out.
:- import_module code_util, globals, options.
:- import_module bool, string, map, std_util, require, assoc_list, term.

%---------------------------------------------------------------------------%

base_typeclass_info__generate_llds(ModuleInfo, CModules) :-
	module_info_name(ModuleInfo, ModuleName),
	module_info_instances(ModuleInfo, InstanceTable),
	map__to_assoc_list(InstanceTable, AllInstances),
	base_typeclass_info__gen_infos_for_classes(AllInstances, ModuleName,
		ModuleInfo, CModules).

:- pred base_typeclass_info__gen_infos_for_classes(assoc_list(class_id,
	list(hlds_instance_defn)), module_name, module_info,
	list(comp_gen_c_data)).
:- mode base_typeclass_info__gen_infos_for_classes(in, in, in, out) is det.

base_typeclass_info__gen_infos_for_classes([], _ModuleName, _ModuleInfo, []).
base_typeclass_info__gen_infos_for_classes([C|Cs], ModuleName, ModuleInfo, 
		CModules) :-
	base_typeclass_info__gen_infos_for_instance_list(C, ModuleName,
		ModuleInfo, CModules1),
	base_typeclass_info__gen_infos_for_classes(Cs, ModuleName,
		ModuleInfo, CModules2),
	% XXX make it use an accumulator
	list__append(CModules1, CModules2, CModules).

	% XXX make it use an accumulator
:- pred base_typeclass_info__gen_infos_for_instance_list(
	pair(class_id, list(hlds_instance_defn)), module_name, module_info,
	list(comp_gen_c_data)).
:- mode base_typeclass_info__gen_infos_for_instance_list(in, in, in, out) 
	is det.

base_typeclass_info__gen_infos_for_instance_list(_ - [], _, _, []).
base_typeclass_info__gen_infos_for_instance_list(ClassId - [InstanceDefn|Is], 
		ModuleName, ModuleInfo, CModules) :-
	base_typeclass_info__gen_infos_for_instance_list(ClassId - Is,
		ModuleName, ModuleInfo, CModules1),
	InstanceDefn = hlds_instance_defn(ImportStatus, _TermContext,
				InstanceConstraints, InstanceTypes, _Interface,
				PredProcIds, _Varset, _SuperClassProofs),
	(
			% Only make the base_typeclass_info if the instance
			% declaration originally came from _this_ module.
		status_defined_in_this_module(ImportStatus, yes)
	->

		base_typeclass_info__make_instance_string(InstanceTypes, 
			InstanceString),

		DataName = base_typeclass_info(ClassId, InstanceString),

		base_typeclass_info__gen_rvals_and_procs(PredProcIds,
			InstanceConstraints, ModuleInfo, Rvals0, Procs),

		/*
		base_typeclass_info__gen_superclass_rvals(ClassId, ModuleInfo,
			InstanceTypes, SuperClassRvals),

		list__append(Rvals0, SuperClassRvals, Rvals),
		*/
		Rvals = Rvals0,

			% XXX Need we always export it from the module?
			% (Note that linkage/2 in llds_out.m assumes
			% that we do.)
		Status = yes,

		CModule = comp_gen_c_data(ModuleName, DataName,
			Status, Rvals, Procs),
		CModules = [CModule | CModules1]
	;
			% The instance decl is from another module, so
			% we don't bother including it.
		CModules = CModules1
	).

%----------------------------------------------------------------------------%

:- pred base_typeclass_info__gen_rvals_and_procs(maybe(list(hlds_class_proc)),
	list(class_constraint), module_info, list(maybe(rval)),
	list(pred_proc_id)).
:- mode base_typeclass_info__gen_rvals_and_procs(in, in, in, out, out) is det.

base_typeclass_info__gen_rvals_and_procs(no, _, _, [], []) :-
	error("pred_proc_ids should have been filled in by check_typeclass.m").
base_typeclass_info__gen_rvals_and_procs(yes(PredProcIds0), Constraints,
		ModuleInfo, Rvals, PredProcIds) :-
	list__length(Constraints, InstanceArity),
	ArityArg = yes(const(int_const(InstanceArity))),
	ExtractPredProcId = lambda([HldsPredProc::in, PredProc::out] is det,
		(
			HldsPredProc = hlds_class_proc(PredId, ProcId),
			PredProc = proc(PredId, ProcId)
		)),
	list__map(ExtractPredProcId, PredProcIds0, PredProcIds),
	base_typeclass_info__construct_pred_addrs(PredProcIds, ModuleInfo,
		PredAddrArgs),
	Rvals = [ArityArg|PredAddrArgs].

:- pred base_typeclass_info__construct_pred_addrs(list(pred_proc_id),
	module_info, list(maybe(rval))).
:- mode base_typeclass_info__construct_pred_addrs(in, in, out) is det.

base_typeclass_info__construct_pred_addrs([], _, []).
base_typeclass_info__construct_pred_addrs([proc(PredId, ProcId) | Procs],
		ModuleInfo, [PredAddrArg | PredAddrArgs]) :-
	code_util__make_entry_label(ModuleInfo, PredId, ProcId, no, PredAddr),
	PredAddrArg = yes(const(code_addr_const(PredAddr))),
	base_typeclass_info__construct_pred_addrs(Procs, ModuleInfo,
		PredAddrArgs).

%----------------------------------------------------------------------------%

:- pred base_typeclass_info__gen_superclass_rvals(class_id, module_info, 
		list(type), list(maybe(rval))).
:- mode base_typeclass_info__gen_superclass_rvals(in, in, in, out) is det.

base_typeclass_info__gen_superclass_rvals(ClassId, ModuleInfo, InstanceTypes,
		SuperClassRvals) :-
	module_info_classes(ModuleInfo, ClassTable),
	map__lookup(ClassTable, ClassId, ClassDefn),
	ClassDefn = hlds_class_defn(SuperClassConstraints, ClassVars, _, _, _),
	map__from_corresponding_lists(ClassVars, InstanceTypes, VarToType),
	GetRval = lambda([Constraint::in, Rval::out] is det,
		(
			Constraint = constraint(ClassName, ClassTypes),
			list__length(ClassTypes, Arity),
			SuperClassId = class_id(ClassName, Arity),
			term__vars_list(ClassTypes, SuperClassVars), 
			map__apply_to_list(SuperClassVars, VarToType,
				UsedInstanceTypes),
			base_typeclass_info__make_instance_string(
				UsedInstanceTypes, InstanceString),

			DataName = base_typeclass_info(SuperClassId,
					InstanceString),
				% it doesn't matter which module the instance
				% decl comes from
			Module = unqualified("<unknown>"),
			DataAddr = data_addr(Module, DataName),
			Rval =  yes(const(data_addr_const(DataAddr)))
		)),
	list__map(GetRval, SuperClassConstraints, SuperClassRvals).
	
%----------------------------------------------------------------------------%
	% XXX this version of base_typeclass_info__make_instance_string 
	% handles non-qualified types, even though everything should be
	% qualified by now. Unfortunately, for some reason builtins are
	% not qualified. The version that aborts when given an unqualified
	% type is included at the end.

base_typeclass_info__make_instance_string(InstanceTypes, InstanceString) :-
	list__map(base_typeclass_info__type_to_string, 
		InstanceTypes, InstanceStrings),
	string__append_list(InstanceStrings, InstanceString).

:- pred base_typeclass_info__type_to_string(type, string).
:- mode base_typeclass_info__type_to_string(in, out) is det.

base_typeclass_info__type_to_string(Type, String) :-
	(
		Type = term__functor(Name, Args, _),
		(
			Name = term__atom(":"),
			Args = [ModuleName, TypeName]
		->
			base_typeclass_info__type_to_string(ModuleName,
				ModuleString),
			base_typeclass_info__type_to_string(TypeName,
				TypeString),
			string__append(ModuleString, TypeString, String)
		;
			Name = term__atom(NameString)
		->
			list__length(Args, Arity),
			string__int_to_string(Arity, ArityString),
			string__append_list([NameString, "_", ArityString, "_"],
				String)
		;
			error("instance functor not an atom")
		)
	;
		Type = term__variable(_),
		error("instance type should be a single functor with variables as args")
	).
	
/******************************* 
 *
 * This is the non-working version of base_typeclass_info__type_to_string,
 * enforces the rule that every type be qualified. Unfortunately this doesn't
 * seem to be the case

:- pred base_typeclass_info__type_to_string(type, string).
:- mode base_typeclass_info__type_to_string(in, out) is det.

base_typeclass_info__type_to_string(Type, String) :-
	(
		Type = term__functor(Name, Args, _),
		Name = term__atom(":"),
		Args = [ModuleName, TypeName]
	->
		base_typeclass_info__unqualified_type_to_string(ModuleName,
			ModuleString),
		base_typeclass_info__unqualified_type_to_string(TypeName,
			TypeString),
		string__append(ModuleString, TypeString, String)
	;
		error("type not qualified")
	).

:- pred base_typeclass_info__unqualified_type_to_string(type, string).
:- mode base_typeclass_info__unqualified_type_to_string(in, out) is det.

base_typeclass_info__unqualified_type_to_string(Type, String) :-
	(
		Type = term__functor(Name, Args, _),
		(
			Name = term__atom(NameString)
		->
			list__length(Args, Arity),
			string__int_to_string(Arity, ArityString),
			string__append_list([NameString, "_", ArityString, "_"],
				String)
		;
			error("instance functor not an atom")
		)
	;
		Type = term__variable(_),
		error("instance type should be a single functor with variables as args")
	).

**********************************/
	
%----------------------------------------------------------------------------%
