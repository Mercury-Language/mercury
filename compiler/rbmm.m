%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007, 2009-2010 The University of Melbourne.
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

:- include_module add_rbmm_goal_infos.
:- include_module condition_renaming.
:- include_module execution_path.
:- include_module interproc_region_lifetime.
:- include_module live_region_analysis.
:- include_module live_variable_analysis.
:- include_module points_to_analysis.
:- include_module points_to_graph.
:- include_module points_to_info.
:- include_module region_arguments.
:- include_module region_instruction.
:- include_module region_liveness_info.
:- include_module region_resurrection_renaming.
:- include_module region_transformation.

:- import_module hlds.
:- import_module hlds.hlds_module.

:- import_module io.
%-----------------------------------------------------------------------------%

:- pred do_region_analysis(module_info::in, module_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module transform_hlds.rbmm.add_rbmm_goal_infos.
:- import_module transform_hlds.rbmm.condition_renaming.
:- import_module transform_hlds.rbmm.execution_path.
:- import_module transform_hlds.rbmm.interproc_region_lifetime.
:- import_module transform_hlds.rbmm.live_region_analysis.
:- import_module transform_hlds.rbmm.live_variable_analysis.
:- import_module transform_hlds.rbmm.points_to_analysis.
:- import_module transform_hlds.rbmm.region_arguments.
:- import_module transform_hlds.rbmm.region_instruction.
:- import_module transform_hlds.rbmm.region_resurrection_renaming.
:- import_module transform_hlds.rbmm.region_transformation.

:- import_module map.
%-----------------------------------------------------------------------------%

do_region_analysis(!ModuleInfo, !IO) :-
    region_points_to_analysis(RptaInfoTable, !ModuleInfo),
    execution_path_analysis(!.ModuleInfo, ExecPathTable),
    live_variable_analysis(!.ModuleInfo, ExecPathTable, LVBeforeTable,
        LVAfterTable, VoidVarTable),
    live_region_analysis(!.ModuleInfo, RptaInfoTable,
        LVBeforeTable, LVAfterTable, VoidVarTable, LRBeforeTable0,
        LRAfterTable0, VoidVarRegionTable0, InputRTable, OutputRTable,
        BornRTable0, DeadRTable0, LocalRTable0),
    compute_interproc_region_lifetime(!.ModuleInfo, RptaInfoTable,
        ExecPathTable, LRBeforeTable0, LRAfterTable0, InputRTable,
        OutputRTable, ConstantRTable0, BornRTable0, BornRTable1,
        DeadRTable0, DeadRTable1),
    ignore_primitive_regions(!.ModuleInfo, RptaInfoTable,
        BornRTable1, BornRTable, DeadRTable1, DeadRTable,
        ConstantRTable0, ConstantRTable, LocalRTable0, LocalRTable,
        LRBeforeTable0, LRBeforeTable, LRAfterTable0, LRAfterTable,
        VoidVarRegionTable0, VoidVarRegionTable),
    introduce_region_instructions(!.ModuleInfo, RptaInfoTable,
        ExecPathTable, LRBeforeTable, LRAfterTable, VoidVarRegionTable,
        BornRTable, DeadRTable, LocalRTable,
        BecomeLiveTable, BecomeDeadBeforeTable, BecomeDeadAfterTable,
        RegionInstructionTable),

    record_region_arguments(!.ModuleInfo, RptaInfoTable,
        ConstantRTable, DeadRTable, BornRTable, FormalRegionArgTable,
        ActualRegionArgTable),

    % The region analysis treats region variables as if they are
    % imperative-style updatable variables. They may also have scopes
    % which are not valid in Mercury. In order for Mercury code to
    % manipulate regions we need to map these "region variables" on to
    % Mercury variables.
    % The calls below derive the necessary mapping to resolve the problem.
    compute_resurrection_paths(ExecPathTable, LRBeforeTable, LRAfterTable,
        BornRTable, DeadRTable, LocalRTable,
        BecomeLiveTable, BecomeDeadBeforeTable, BecomeDeadAfterTable,
        ResurrectionPathTable0),
    collect_join_points(ResurrectionPathTable0, ExecPathTable,
        JoinPointTable),
    collect_paths_containing_join_points(ExecPathTable, JoinPointTable,
        ResurrectionPathTable0, ResurrectionPathTable),
    collect_region_resurrection_renaming(BecomeLiveTable, LocalRTable,
        RptaInfoTable, ResurrectionPathTable, ResurrectionRenameTable),
    collect_renaming_and_annotation(ResurrectionRenameTable, JoinPointTable,
        LRBeforeTable, LRAfterTable, BornRTable, RptaInfoTable,
        ResurrectionPathTable, ExecPathTable, ResurRenamingAnnoTable,
        ResurRenamingTable),
    collect_non_local_and_in_cond_regions(!.ModuleInfo, RptaInfoTable,
        LRBeforeTable, LRAfterTable, ResurRenamingTable,
        ResurRenamingAnnoTable, LocalRegionsTable, InCondRegionsTable),
    collect_ite_renamed_regions(LocalRegionsTable, InCondRegionsTable,
        RenamedRegionsTable),
    collect_ite_renaming(!.ModuleInfo, RptaInfoTable, RenamedRegionsTable,
        IteRenamingTable0),
    collect_ite_annotation(RenamedRegionsTable, ExecPathTable,
        RptaInfoTable, IteRenamingTable0, IteRenamingTable,
        IteRenamingAnnoTable),
    region_transform(RptaInfoTable, FormalRegionArgTable,
        ActualRegionArgTable, ResurRenamingTable,
        IteRenamingTable, RegionInstructionTable, ResurRenamingAnnoTable,
        IteRenamingAnnoTable, map.init, NameToVarTable, !ModuleInfo),

    collect_rbmm_goal_info(RptaInfoTable, ActualRegionArgTable,
        ResurRenamingTable, IteRenamingTable, NameToVarTable, !ModuleInfo).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.
%-----------------------------------------------------------------------------%
