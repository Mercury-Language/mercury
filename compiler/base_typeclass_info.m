%---------------------------------------------------------------------------%
% Copyright (C) 1996-2000, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module generates the RTTI data for the global variables (or constants)
% that hold the base_typeclass_info structures of the typeclass instances
% defined by the current module.
%
% See notes/type_class_transformation.html for a description of the various 
% ways to represent type information, including a description of the
% base_typeclass_info structures.
%
% Author: dgj.
%
%---------------------------------------------------------------------------%

:- module backend_libs__base_typeclass_info.

:- interface.

:- import_module backend_libs__rtti.
:- import_module hlds__hlds_module.
:- import_module parse_tree__prog_data.

:- import_module list.

:- pred base_typeclass_info__generate_rtti(module_info, list(rtti_data)).
:- mode base_typeclass_info__generate_rtti(in, out) is det.

	% Given a list of types, mangle the names so into a string which
	% identifies them. The types must all have their top level functor
	% bound, with any arguments free variables.
:- pred base_typeclass_info__make_instance_string(list(type), string).
:- mode base_typeclass_info__make_instance_string(in, out) is det.

:- implementation.

:- import_module check_hlds__type_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_out.
:- import_module hlds__hlds_pred.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module ll_backend__code_util.
:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_out.

:- import_module bool, int, string, map, std_util, require, term, assoc_list.

%---------------------------------------------------------------------------%

base_typeclass_info__generate_rtti(ModuleInfo, RttiDataList) :-
	module_info_name(ModuleInfo, ModuleName),
	module_info_instances(ModuleInfo, InstanceTable),
	map__to_assoc_list(InstanceTable, AllInstances),
	base_typeclass_info__gen_infos_for_classes(AllInstances, ModuleName,
		ModuleInfo, RttiDataList).

:- pred base_typeclass_info__gen_infos_for_classes(assoc_list(class_id,
	list(hlds_instance_defn)), module_name, module_info,
	list(rtti_data)).
:- mode base_typeclass_info__gen_infos_for_classes(in, in, in, out) is det.

base_typeclass_info__gen_infos_for_classes([], _ModuleName, _ModuleInfo, []).
base_typeclass_info__gen_infos_for_classes([C|Cs], ModuleName, ModuleInfo, 
		RttiDataList) :-
	base_typeclass_info__gen_infos_for_instance_list(C, ModuleName,
		ModuleInfo, RttiDataList1),
	base_typeclass_info__gen_infos_for_classes(Cs, ModuleName,
		ModuleInfo, RttiDataList2),
	% XXX make it use an accumulator
	list__append(RttiDataList1, RttiDataList2, RttiDataList).

	% XXX make it use an accumulator
:- pred base_typeclass_info__gen_infos_for_instance_list(
	pair(class_id, list(hlds_instance_defn)), module_name, module_info,
	list(rtti_data)).
:- mode base_typeclass_info__gen_infos_for_instance_list(in, in, in, out) 
	is det.

base_typeclass_info__gen_infos_for_instance_list(_ - [], _, _, []).
base_typeclass_info__gen_infos_for_instance_list(ClassId - [InstanceDefn|Is], 
		ModuleName, ModuleInfo, RttiDataList) :-
	base_typeclass_info__gen_infos_for_instance_list(ClassId - Is,
		ModuleName, ModuleInfo, RttiDataList1),
	InstanceDefn = hlds_instance_defn(InstanceModule, ImportStatus,
			_TermContext, InstanceConstraints, InstanceTypes, Body,
			PredProcIds, _Varset, _SuperClassProofs),
	(
		Body = concrete(_),
			% Only make the base_typeclass_info if the instance
			% declaration originally came from _this_ module.
		status_defined_in_this_module(ImportStatus, yes)
	->
		base_typeclass_info__make_instance_string(InstanceTypes, 
			InstanceString),
		base_typeclass_info__gen_body(PredProcIds,
			InstanceTypes, InstanceConstraints, ModuleInfo, 
			ClassId, BaseTypeClassInfo),
		RttiData = base_typeclass_info(InstanceModule,
			ClassId, InstanceString, BaseTypeClassInfo),
		RttiDataList = [RttiData | RttiDataList1]
	;
			% The instance decl is from another module,
			% or is abstract, so we don't bother including it.
		RttiDataList = RttiDataList1
	).

%----------------------------------------------------------------------------%

:- pred base_typeclass_info__gen_body(maybe(list(hlds_class_proc)),
		list(type), list(class_constraint), module_info, class_id,
		base_typeclass_info).
:- mode base_typeclass_info__gen_body(in, in, in, in, in, out) is det.

base_typeclass_info__gen_body(no, _, _, _, _, _) :-
	error("pred_proc_ids should have been filled in by check_typeclass.m").
base_typeclass_info__gen_body(yes(PredProcIds0), Types, Constraints,
		ModuleInfo, ClassId, BaseTypeClassInfo) :-
	term__vars_list(Types, TypeVars),
	get_unconstrained_tvars(TypeVars, Constraints, Unconstrained),
	list__length(Constraints, NumConstraints),
	list__length(Unconstrained, NumUnconstrained),
	NumExtra = NumConstraints + NumUnconstrained,
	ExtractPredProcId = lambda([HldsPredProc::in, PredProc::out] is det,
		(
			HldsPredProc = hlds_class_proc(PredId, ProcId),
			PredProc = proc(PredId, ProcId)
		)),
	list__map(ExtractPredProcId, PredProcIds0, PredProcIds),
	base_typeclass_info__construct_proc_labels(PredProcIds, ModuleInfo,
		ProcLabels),
	base_typeclass_info__gen_superclass_count(ClassId, ModuleInfo,
			SuperClassCount, ClassArity),
	list__length(ProcLabels, NumMethods),
	BaseTypeClassInfo = base_typeclass_info(NumExtra, NumConstraints,
		SuperClassCount, ClassArity, NumMethods, ProcLabels).

:- pred base_typeclass_info__construct_proc_labels(list(pred_proc_id),
	module_info, list(rtti_proc_label)).
:- mode base_typeclass_info__construct_proc_labels(in, in, out) is det.

base_typeclass_info__construct_proc_labels([], _, []).
base_typeclass_info__construct_proc_labels([proc(PredId, ProcId) | Procs],
		ModuleInfo, [ProcLabel | ProcLabels]) :-
	ProcLabel = rtti__make_rtti_proc_label(ModuleInfo, PredId, ProcId),
	base_typeclass_info__construct_proc_labels(Procs, ModuleInfo,
		ProcLabels).

%----------------------------------------------------------------------------%

:- pred base_typeclass_info__gen_superclass_count(class_id, module_info, 
		int, int).
:- mode base_typeclass_info__gen_superclass_count(in, in, out, out) is det.

base_typeclass_info__gen_superclass_count(ClassId, ModuleInfo, 
		NumSuperClasses, ClassArity) :-
	module_info_classes(ModuleInfo, ClassTable),
	map__lookup(ClassTable, ClassId, ClassDefn),
	ClassDefn = hlds_class_defn(_, SuperClassConstraints, ClassVars,
			_, _, _, _),
	list__length(SuperClassConstraints, NumSuperClasses),
	list__length(ClassVars, ClassArity).

%----------------------------------------------------------------------------%

	% Note that for historical reasons, builtin types
	% are treated as being unqualified (`int') rather than
	% being qualified (`builtin:int') at this point.

base_typeclass_info__make_instance_string(InstanceTypes, InstanceString) :-
	list__map(base_typeclass_info__type_to_string, 
		InstanceTypes, InstanceStrings),
	string__append_list(InstanceStrings, InstanceString).

:- pred base_typeclass_info__type_to_string(type, string).
:- mode base_typeclass_info__type_to_string(in, out) is det.

base_typeclass_info__type_to_string(Type, String) :-
	( sym_name_and_args(Type, TypeName, TypeArgs) ->
		prog_out__sym_name_to_string(TypeName, "__", TypeNameString),
		list__length(TypeArgs, TypeArity),
		string__int_to_string(TypeArity, TypeArityString),
		string__append_list(
			[TypeNameString, "__arity", TypeArityString, "__"],
			String)
	;
		error("base_typeclass_info__type_to_string: invalid type")
	).
		
%----------------------------------------------------------------------------%
