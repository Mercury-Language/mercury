%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File rbmm.live_region_analysis.m.
% Main author: Quan Phan.
%
% This module implements the live region analysis, which uses execution paths
% with live variables to collect live regions at each program point.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.rbmm.live_region_analysis.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module transform_hlds.rbmm.points_to_info.
:- import_module transform_hlds.rbmm.region_liveness_info.

%---------------------------------------------------------------------------%

    % Collects live region sets.
    %
:- pred live_region_analysis(module_info::in, rpta_info_table::in,
    proc_pp_varset_table::in, proc_pp_varset_table::in,
    proc_pp_varset_table::in, proc_pp_region_set_table::out,
    proc_pp_region_set_table::out, proc_pp_region_set_table::out,
    proc_region_set_table::out, proc_region_set_table::out,
    proc_region_set_table::out, proc_region_set_table::out,
    proc_region_set_table::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.rbmm.points_to_graph.
:- import_module transform_hlds.smm_common.

:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module term.

%---------------------------------------------------------------------------%
%
% Live region analysis
%

% Compute for each procedure live region sets before and after all program
% points. As in live variable analysis we calculated the set of void
% variables after a program point, this analysis also computes the set of
% regions of those variables.
%
% Apart from that, it is convenient to also compute the inputR, outputR,
% localR, and the initial bornR and deadR for each procedure in this
% analysis.
%

live_region_analysis(ModuleInfo, RptaInfoTable, LVBeforeTable, LVAfterTable,
        VoidVarTable, LRBeforeTable, LRAfterTable, VoidVarRegionTable,
        InputRTable, OutputRTable, BornRTable, DeadRTable, LocalRTable) :-
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    map.init(LRBeforeTable0),
    map.init(LRAfterTable0),
    map.init(VoidVarRegionTable0),
    map.init(InputRTable0),
    map.init(OutputRTable0),
    map.init(BornRTable0),
    map.init(DeadRTable0),
    map.init(LocalRTable0),
    foldl8(
        live_region_analysis_pred(ModuleInfo, RptaInfoTable,
            LVBeforeTable, LVAfterTable, VoidVarTable),
        PredIds,
        LRBeforeTable0, LRBeforeTable,
        LRAfterTable0, LRAfterTable,
        VoidVarRegionTable0, VoidVarRegionTable,
        InputRTable0, InputRTable,
        OutputRTable0, OutputRTable,
        BornRTable0, BornRTable,
        DeadRTable0, DeadRTable,
        LocalRTable0, LocalRTable).

:- pred live_region_analysis_pred(module_info::in, rpta_info_table::in,
    proc_pp_varset_table::in, proc_pp_varset_table::in,
    proc_pp_varset_table::in, pred_id::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out,
    proc_region_set_table::in, proc_region_set_table::out,
    proc_region_set_table::in, proc_region_set_table::out,
    proc_region_set_table::in, proc_region_set_table::out,
    proc_region_set_table::in, proc_region_set_table::out,
    proc_region_set_table::in, proc_region_set_table::out) is det.

live_region_analysis_pred(ModuleInfo, RptaInfoTable, LVBeforeTable,
        LVAfterTable, VoidVarTable, PredId, !LRBeforeTable, !LRAfterTable,
        !VoidVarRegionTable, !InputRTable, !OutputRTable, !BornRTable,
        !DeadRTable, !LocalRTable) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_non_imported_procids(PredInfo),

    foldl8(
        live_region_analysis_proc(ModuleInfo, RptaInfoTable,
            LVBeforeTable, LVAfterTable, VoidVarTable, PredId),
        ProcIds,
        !LRBeforeTable, !LRAfterTable, !VoidVarRegionTable,
        !InputRTable, !OutputRTable, !BornRTable, !DeadRTable, !LocalRTable).

:- pred live_region_analysis_proc(module_info::in, rpta_info_table::in,
    proc_pp_varset_table::in, proc_pp_varset_table::in,
    proc_pp_varset_table::in, pred_id::in, proc_id::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out,
    proc_region_set_table::in, proc_region_set_table::out,
    proc_region_set_table::in, proc_region_set_table::out,
    proc_region_set_table::in, proc_region_set_table::out,
    proc_region_set_table::in, proc_region_set_table::out,
    proc_region_set_table::in, proc_region_set_table::out) is det.

live_region_analysis_proc(ModuleInfo, RptaInfoTable, LVBeforeTable,
        LVAfterTable, VoidVarTable, PredId, ProcId, !LRBeforeTable,
        !LRAfterTable, !VoidVarRegionTable, !InputRTable, !OutputRTable,
        !BornRTable, !DeadRTable, !LocalRTable) :-
    PPId = proc(PredId, ProcId),
    ( if some_are_special_preds([PPId], ModuleInfo) then
        % XXX: This action seems to be overkilled, it never goes in this
        % branch.
        % XXX: For the time being just ignore special predicates
        % such as __Unify__ and others or non-defined-in-module ones.
        % The latter ones should have been analysed when their
        % modules analysed and their tables will be integrated.
        % But it is not the case at the moment.
        true
    else
        % This test is just a cautious check.
        ( if
            RptaInfo = rpta_info_table_search_rpta_info(PPId, RptaInfoTable)
        then
            % Compute region sets.
            RptaInfo = rpta_info(Graph, _ALpha),
            module_info_proc_info(ModuleInfo, PPId, ProcInfo),
            find_input_output_args(ModuleInfo, ProcInfo, Inputs, Outputs),
            lv_to_lr(set.list_to_set(Inputs), Graph, ModuleInfo, ProcInfo,
                InputR),
            lv_to_lr(set.list_to_set(Outputs), Graph, ModuleInfo, ProcInfo,
                OutputR),
            map.set(PPId, InputR, !InputRTable),
            map.set(PPId, OutputR, !OutputRTable),
            % initial bornR
            set.difference(OutputR, InputR, BornR),
            map.set(PPId, BornR, !BornRTable),
            % initial deadR
            set.difference(InputR, OutputR, DeadR),
            map.set(PPId, DeadR, !DeadRTable),
            % localR
            Nodes = rptg_get_nodes_as_list(Graph),
            set.difference(
                set.difference(set.list_to_set(Nodes), InputR),
                OutputR, LocalR),
            map.set(PPId, LocalR, !LocalRTable),

            % Compute live region set at each program point
            map.lookup(LVBeforeTable, PPId, ProcLVBefore),
            map.foldl(
                proc_lv_to_proc_lr(Graph, ModuleInfo, ProcInfo),
                ProcLVBefore, map.init, ProcLRBefore),
            map.set(PPId, ProcLRBefore, !LRBeforeTable),

            map.lookup(LVAfterTable, PPId, ProcLVAfter),
            map.foldl(
                proc_lv_to_proc_lr(Graph, ModuleInfo, ProcInfo),
                ProcLVAfter, map.init, ProcLRAfter),
            map.set(PPId, ProcLRAfter, !LRAfterTable),

            map.lookup(VoidVarTable, PPId, ProcVoidVar),
            map.foldl(
                proc_lv_to_proc_lr(Graph, ModuleInfo, ProcInfo),
                ProcVoidVar, map.init, ProcVoidVarRegion),
            map.set(PPId, ProcVoidVarRegion, !VoidVarRegionTable)
        else
            unexpected($pred, "no rpta_info")
        )
    ).

:- pred proc_lv_to_proc_lr(rpt_graph::in, module_info::in, proc_info::in,
    program_point::in, variable_set::in, pp_region_set_table::in,
    pp_region_set_table::out) is det.

proc_lv_to_proc_lr(Graph, ModuleInfo, ProcInfo, ProgPoint, LVs, !ProcLRMap) :-
    lv_to_lr(LVs, Graph, ModuleInfo, ProcInfo, LRs),
    map.set(ProgPoint, LRs, !ProcLRMap).

    % From a set of live variables, derive the set of live regions.
    % A live region is defined to be reachable from a live variable
    % in the corresponding region points-to graph.
    %
:- pred lv_to_lr(variable_set::in, rpt_graph::in, module_info::in,
    proc_info::in, region_set::out) is det.

lv_to_lr(LVSet, Graph, ModuleInfo, ProcInfo, LRSet) :-
    ( if set.is_empty(LVSet) then
        set.init(LRSet)
    else
        % Collect reachable regions at this program point.
        set.init(LRSet0),
        set.fold(rptg_reach_from_a_variable(Graph, ModuleInfo, ProcInfo),
            LVSet, LRSet0, LRSet)
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.live_region_analysis.
%---------------------------------------------------------------------------%
