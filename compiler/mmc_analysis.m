%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2004 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: mmc_analysis.m
% Main author: stayl
%
% Specify Mercury compiler analyses to be used with the
% inter-module analysis framework.
%-----------------------------------------------------------------------------%

:- module transform_hlds__mmc_analysis.

:- interface.

:- import_module analysis.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.

:- type mmc ---> mmc.

:- instance compiler(mmc).

:- func module_name_to_module_id(module_name) = module_id.
:- func module_id_to_module_name(module_id) = module_name.

:- func pred_or_func_name_arity_to_func_id(pred_or_func,
	string, arity, proc_id) = func_id.

:- implementation.

:- import_module hlds__hlds_out.
:- import_module parse_tree__modules.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module transform_hlds__unused_args.

:- import_module bool, std_util, string.

:- instance compiler(mmc) where [
	compiler_name(mmc) = "mmc",

	analyses(mmc, "unused_args") =
		'new analysis_type'(
			unit1 `with_type` unit(unused_args_func_info),
			unit1 `with_type` unit(any_call),
			unit1 `with_type` unit(unused_args_answer)),

	module_id_to_file_name(mmc, ModuleId, Ext, FileName) -->
		module_name_to_file_name(module_id_to_module_name(ModuleId),
			Ext, yes, FileName)
].

module_name_to_module_id(ModuleName) = ModuleId :-
	sym_name_to_string(ModuleName, ModuleId).

module_id_to_module_name(ModuleId) = ModuleName :-
	string_to_sym_name(ModuleId, ".", ModuleName).

pred_or_func_name_arity_to_func_id(PredOrFunc, Name, Arity, ProcId) = FuncId :-
	FuncId0 = hlds_out__simple_call_id_to_string(PredOrFunc
		- unqualified(Name)/Arity),
	proc_id_to_int(ProcId, ProcInt),
	FuncId = FuncId0 ++ "-" ++ int_to_string(ProcInt).
