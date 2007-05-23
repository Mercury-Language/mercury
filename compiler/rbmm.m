%-----------------------------------------------------------------------------%
% Vim: ft=Mercury ts=4 sw=4
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: rbmm.m.
% Main author: quan.
%
% This is the main file for the region-based memory management package.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.rbmm.
:- interface.

:- include_module execution_path.
:- include_module interproc_region_lifetime.
:- include_module live_region_analysis.
:- include_module live_variable_analysis.
:- include_module points_to_analysis.
:- include_module points_to_graph.
:- include_module points_to_info.
:- include_module region_instruction.
:- include_module region_liveness_info.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- pred do_region_analysis(module_info::in, module_info::out) is det.

:- implementation.

:- import_module transform_hlds.rbmm.execution_path.
:- import_module transform_hlds.rbmm.interproc_region_lifetime.
:- import_module transform_hlds.rbmm.live_region_analysis.
:- import_module transform_hlds.rbmm.live_variable_analysis.
:- import_module transform_hlds.rbmm.points_to_analysis.
:- import_module transform_hlds.rbmm.region_instruction.

do_region_analysis(!ModuleInfo) :-
    region_points_to_analysis(RptaInfoTable, !ModuleInfo),
    execution_path_analysis(!.ModuleInfo, ExecPathTable),
    live_variable_analysis(!.ModuleInfo, ExecPathTable, LVBeforeTable, 
        LVAfterTable, VoidVarTable),
    live_region_analysis(!.ModuleInfo, RptaInfoTable, 
	LVBeforeTable, LVAfterTable, VoidVarTable,
	LRBeforeTable0, LRAfterTable0, VoidVarRegionTable0, 
	InputRTable, OutputRTable, BornRTable0, DeadRTable0, LocalRTable0),
    compute_interproc_region_lifetime(!.ModuleInfo, RptaInfoTable,
        ExecPathTable, LRBeforeTable0, LRAfterTable0, InputRTable,
	OutputRTable, ConstantRTable0, BornRTable0, BornRTable1,
	DeadRTable0, DeadRTable1),
    ignore_primitive_regions(!.ModuleInfo, RptaInfoTable, 
        BornRTable1, BornRTable, DeadRTable1, DeadRTable,
	ConstantRTable0, _ConstantRTable, LocalRTable0, LocalRTable,
	LRBeforeTable0, LRBeforeTable, LRAfterTable0, LRAfterTable,
	VoidVarRegionTable0, VoidVarRegionTable),
    transform(!.ModuleInfo, RptaInfoTable, ExecPathTable,
        LRBeforeTable, LRAfterTable, VoidVarRegionTable, BornRTable,
	DeadRTable, LocalRTable, _AnnotationTable).

:- end_module transform_hlds.rbmm.
