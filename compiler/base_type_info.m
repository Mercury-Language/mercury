%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module generates the LLDS code that defines global variables
% to hold the base_type_info structures of the types defined by the
% current module.
%
% These global variables are needed only with when we are using the
% shared-one-or-two-cell way of representing type information.
% It is up to the caller to check this. (When using other representations,
% defining these global variables is harmless except for adding to
% compilation time and executable size.)
%
% See polymorphism.m for a description of the various ways to represent
% type information, including a description of the base_type_info structures.
%
% Author: zs.
%
%---------------------------------------------------------------------------%

:- module base_type_info.

:- interface.

:- import_module hlds_module.

:- pred base_type_info__generate_hlds(module_info, module_info).
:- mode base_type_info__generate_hlds(in, out) is det.

:- pred base_type_info__generate_llds(module_info, list(c_module)).
:- mode base_type_info__generate_llds(in, out) is det.

:- implementation.

:- import_module prog_data, hlds_data, hlds_pred, hlds_out.
:- import_module llds, code_util, globals, special_pred, options.
:- import_module bool, string, list, map, std_util, require.

%---------------------------------------------------------------------------%

base_type_info__generate_hlds(ModuleInfo0, ModuleInfo) :-
	module_info_name(ModuleInfo0, ModuleName),
	module_info_types(ModuleInfo0, TypeTable),
	map__keys(TypeTable, TypeIds),
	base_type_info__gen_base_gen_infos(TypeIds, TypeTable, ModuleName,
		ModuleInfo0, BaseGenInfos),
	module_info_set_base_gen_infos(ModuleInfo0, BaseGenInfos,
		ModuleInfo).

	% Given a list of the ids of all the types in the type table,
	% find the types defined in this module, and return a base_gen_info
	% for each.

:- pred base_type_info__gen_base_gen_infos(list(type_id), type_table, string,
	module_info, list(base_gen_info)).
:- mode base_type_info__gen_base_gen_infos(in, in, in, in, out) is det.

base_type_info__gen_base_gen_infos([], _, _, _, []).
base_type_info__gen_base_gen_infos([TypeId | TypeIds], TypeTable, ModuleName,
		ModuleInfo, BaseGenInfos) :-
	base_type_info__gen_base_gen_infos(TypeIds, TypeTable, ModuleName,
		ModuleInfo, BaseGenInfos1),
	TypeId = SymName - TypeArity,
	(
		SymName = qualified(TypeModuleName, TypeName),
		( TypeModuleName = ModuleName ->
			map__lookup(TypeTable, TypeId, TypeDefn),
			hlds_data__get_type_defn_status(TypeDefn, Status),
			special_pred_list(Specials),
			module_info_get_special_pred_map(ModuleInfo, SpecMap),
			base_type_info__gen_proc_list(Specials, SpecMap,
				TypeId, Procs),
			Info = base_gen_info(TypeId, ModuleName, TypeName,
				TypeArity, Status, no, Procs),
			BaseGenInfos = [Info | BaseGenInfos1]
		;
			BaseGenInfos = BaseGenInfos1
		)
	;
		SymName = unqualified(TypeName),
		string__append_list(["unqualified type ", TypeName,
			"found in base_type_info"], Msg),
		error(Msg)
	).

:- pred base_type_info__gen_proc_list(list(special_pred_id), special_pred_map,
	type_id, list(pred_proc_id)).
:- mode base_type_info__gen_proc_list(in, in, in, out) is det.

base_type_info__gen_proc_list([], _, _, []).
base_type_info__gen_proc_list([Special | Specials], SpecMap, TypeId,
		[proc(PredId, ProcId) | Procs]) :-
	map__lookup(SpecMap, Special - TypeId, PredId),
	special_pred_mode_num(Special, ProcId),
	base_type_info__gen_proc_list(Specials, SpecMap, TypeId, Procs).

%---------------------------------------------------------------------------%

base_type_info__generate_llds(ModuleInfo, CModules) :-
	module_info_base_gen_infos(ModuleInfo, BaseGenInfos),
	base_type_info__construct_base_type_infos(BaseGenInfos, ModuleInfo,
		CModules).

:- pred base_type_info__construct_base_type_infos(list(base_gen_info),
	module_info, list(c_module)).
:- mode base_type_info__construct_base_type_infos(in, in, out) is det.

base_type_info__construct_base_type_infos([], _, []).
base_type_info__construct_base_type_infos([BaseGenInfo | BaseGenInfos],
		ModuleInfo, [CModule | CModules]) :-
	BaseGenInfo = base_gen_info(_TypeId, ModuleName, TypeName, TypeArity,
		Status, Elim, Procs),
	base_type_info__construct_pred_addrs(Procs, Elim, ModuleInfo, 
		PredAddrArgs),
	ArityArg = yes(const(int_const(TypeArity))),
	( 	
		( Status = exported ; Status = abstract_exported )
	->
		Exported = yes
	;
		Exported = no
	),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, type_layout, TypeLayoutOption),
	(
		TypeLayoutOption = yes
	->
		base_type_info__construct_layout(ModuleInfo, TypeName,
			TypeArity, LayoutArg),
		list__append(PredAddrArgs, [LayoutArg], FinalArgs)
	;
		FinalArgs = PredAddrArgs
	),
	CModule = c_data(ModuleName, base_type_info(TypeName, TypeArity),
			Exported, [ArityArg | FinalArgs], Procs),
	base_type_info__construct_base_type_infos(BaseGenInfos, ModuleInfo,
		CModules).

:- pred	base_type_info__construct_layout(module_info, string, int, maybe(rval)).
:- mode	base_type_info__construct_layout(in, in, in, out) is det.
base_type_info__construct_layout(ModuleInfo, TypeName, TypeArity, Rval) :-
	module_info_name(ModuleInfo, ModuleName),
	Rval = yes(const(data_addr_const(data_addr(ModuleName, 
		base_type_layout(TypeName, TypeArity))))).

:- pred base_type_info__construct_pred_addrs(list(pred_proc_id), maybe(int), 
	module_info, list(maybe(rval))).
:- mode base_type_info__construct_pred_addrs(in, in, in, out) is det.

base_type_info__construct_pred_addrs(Procs, Elim, ModuleInfo, PredAddrArgs) :-
	( 
		% dead_proc_elim has eliminated the procs, we
		% should just put some padding in.
	
		Elim = yes(ProcsLength)
	->
		PredAddrArg = yes(const(code_addr_const(
			imported(proc("mercury_builtin", predicate,
				"mercury_builtin", "unused", 0, 0))))),
		list__duplicate(ProcsLength, PredAddrArg, PredAddrArgs)
	;
		base_type_info__construct_pred_addrs2(Procs, ModuleInfo, 
			PredAddrArgs)
	).

:- pred base_type_info__construct_pred_addrs2(list(pred_proc_id), module_info,
	list(maybe(rval))).
:- mode base_type_info__construct_pred_addrs2(in, in, out) is det.

base_type_info__construct_pred_addrs2([], _, []).
base_type_info__construct_pred_addrs2([proc(PredId, ProcId) | Procs],
		ModuleInfo, [PredAddrArg | PredAddrArgs]) :-
	code_util__make_entry_label(ModuleInfo, PredId, ProcId, no, PredAddr),
	PredAddrArg = yes(const(code_addr_const(PredAddr))),
	base_type_info__construct_pred_addrs2(Procs, ModuleInfo, PredAddrArgs).
