%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2006, 2008-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mmc_analysis.m.
% Main author: stayl.
%
% Specify Mercury compiler analyses to be used with the inter-module analysis
% framework.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.mmc_analysis.
:- interface.

:- import_module analysis.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.sym_name.

%-----------------------------------------------------------------------------%

:- type mmc
    --->    mmc.

:- instance compiler(mmc).

:- pred module_name_func_id(module_info::in, pred_proc_id::in,
    module_name::out, func_id::out) is det.

:- pred module_name_func_id_from_pred_info(pred_info::in, proc_id::in,
    module_name::out, func_id::out) is det.

:- pred func_id_to_ppid(module_info::in, module_name::in,
    func_id::in, pred_proc_id::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module transform_hlds.ctgc.
:- import_module transform_hlds.ctgc.structure_sharing.
:- import_module transform_hlds.ctgc.structure_sharing.analysis.
:- import_module transform_hlds.ctgc.structure_reuse.
:- import_module transform_hlds.ctgc.structure_reuse.analysis.
:- import_module transform_hlds.exception_analysis.
:- import_module transform_hlds.tabling_analysis.
:- import_module transform_hlds.trailing_analysis.
:- import_module transform_hlds.unused_args.

:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module io.
:- import_module unit.

%-----------------------------------------------------------------------------%

:- instance compiler(mmc) where [
    compiler_name(mmc) = "mmc",

    analyses(mmc, Name, Analysis) :-
        (
            Name = "mm_tabling_analysis",
            Analysis = 'new analysis_type'(
                unit1 : unit(any_call),
                unit1 : unit(mm_tabling_analysis_answer))
        ;
            Name = "trail_usage",
            Analysis = 'new analysis_type'(
                unit1 : unit(any_call),
                unit1 : unit(trailing_analysis_answer))
        ;
            Name = "exception_analysis",
            Analysis = 'new analysis_type'(
                unit1 : unit(any_call),
                unit1 : unit(exception_analysis_answer))
        ;
            Name = "unused_args",
            Analysis = 'new analysis_type'(
                unit1 : unit(unused_args_call),
                unit1 : unit(unused_args_answer))
        ;
            Name = "structure_sharing",
            Analysis = 'new analysis_type'(
                unit1 : unit(structure_sharing_call),
                unit1 : unit(structure_sharing_answer))
        ;
            Name = "structure_reuse",
            Analysis = 'new analysis_type'(
                unit1 : unit(structure_reuse_call),
                unit1 : unit(structure_reuse_answer))
        ),

    module_name_to_read_file_name(mmc, Globals, ModuleName, Ext,
            MaybeFileName, !IO) :-
        mmc_module_name_to_read_file_name(Globals, ModuleName, Ext,
            MaybeFileName, !IO),

    module_name_to_write_file_name(mmc, Globals, ModuleName, Ext,
            FileName, !IO) :-
        mmc_module_name_to_write_file_name(Globals, ModuleName, Ext,
            FileName, !IO)
].

:- pred mmc_module_name_to_read_file_name(globals::in, module_name::in,
    string::in, maybe_error(string)::out, io::di, io::uo) is det.

mmc_module_name_to_read_file_name(Globals, ModuleName, Ext, MaybeFileName,
        !IO) :-
    module_name_to_search_file_name(Globals, Ext, ModuleName, FileName0, !IO),
    globals.lookup_accumulating_option(Globals, intermod_directories, Dirs),
    search_for_file(Dirs, FileName0, MaybeFileName, !IO).

:- pred mmc_module_name_to_write_file_name(globals::in, module_name::in,
    string::in, string::out, io::di, io::uo) is det.

mmc_module_name_to_write_file_name(Globals, ModuleName, Ext, FileName, !IO) :-
    module_name_to_file_name(Globals, do_create_dirs, Ext,
        ModuleName, FileName, !IO).

module_name_func_id(ModuleInfo, proc(PredId, ProcId), PredModule, FuncId) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    module_name_func_id_from_pred_info(PredInfo, ProcId, PredModule, FuncId).

module_name_func_id_from_pred_info(PredInfo, ProcId, PredModule, FuncId) :-
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    FuncId = func_id(PredOrFunc, PredName, PredArity, ProcId).

func_id_to_ppid(ModuleInfo, ModuleName, FuncId, PPId) :-
    FuncId = func_id(PredOrFunc, FuncName, Arity, ProcId),
    module_info_get_predicate_table(ModuleInfo, PredTable),
    predicate_table_lookup_pf_m_n_a(PredTable, is_fully_qualified,
        PredOrFunc, ModuleName, FuncName, Arity, PredIds),
    (
        PredIds = [],
        unexpected($pred, "no predicate")
    ;
        PredIds = [PredId],
        PPId = proc(PredId, ProcId)
    ;
        PredIds = [_, _ | _],
        unexpected($pred, "more than one predicate")
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.mmc_analysis.
%-----------------------------------------------------------------------------%
