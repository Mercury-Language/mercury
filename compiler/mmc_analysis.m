%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2006 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mmc_analysis.m
% Main author: stayl

% Specify Mercury compiler analyses to be used with the inter-module analysis
% framework.

%-----------------------------------------------------------------------------%

:- module transform_hlds__mmc_analysis.
:- interface.

:- import_module analysis.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

%-----------------------------------------------------------------------------%

:- type mmc ---> mmc.

:- instance compiler(mmc).

:- func module_name_to_module_id(module_name) = module_id.
:- func module_id_to_module_name(module_id) = module_name.

:- func pred_or_func_name_arity_to_func_id(pred_or_func, string, arity,
    proc_id) = func_id.

:- pred module_id_func_id(module_info::in, pred_proc_id::in,
        module_id::out, func_id::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.modules.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module transform_hlds.exception_analysis.
:- import_module transform_hlds.trailing_analysis.
:- import_module transform_hlds.unused_args.

:- import_module bool.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%

:- instance compiler(mmc) where [
    compiler_name(mmc) = "mmc",

    analyses(mmc, "trail_usage") =
        'new analysis_type'(
            unit1 : unit(any_call),
            unit1 : unit(trailing_analysis_answer)),

    analyses(mmc, "exception_analysis") =
        'new analysis_type'(
            unit1 : unit(any_call),
            unit1 : unit(exception_analysis_answer)),

    analyses(mmc, "unused_args") =
        'new analysis_type'(
            unit1 : unit(unused_args_call),
            unit1 : unit(unused_args_answer)),

    module_id_to_file_name(mmc, ModuleId, Ext, FileName) -->
        module_name_to_file_name(module_id_to_module_name(ModuleId),
            Ext, yes, FileName)
].

module_name_to_module_id(ModuleName) = ModuleId :-
    sym_name_to_string(ModuleName, ModuleId).

module_id_to_module_name(ModuleId) = ModuleName :-
    string_to_sym_name(ModuleId, ".", ModuleName).

pred_or_func_name_arity_to_func_id(PredOrFunc, Name, Arity, ProcId) = FuncId :-
    FuncId0 = simple_call_id_to_string(PredOrFunc
        - unqualified(Name)/Arity),
    proc_id_to_int(ProcId, ProcInt),
    FuncId = FuncId0 ++ "-" ++ int_to_string(ProcInt).

module_id_func_id(ModuleInfo, proc(PredId, ProcId), ModuleId, FuncId) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    ModuleId = module_name_to_module_id(PredModule),
    FuncId = pred_or_func_name_arity_to_func_id(PredOrFunc,
        PredName, PredArity, ProcId).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
