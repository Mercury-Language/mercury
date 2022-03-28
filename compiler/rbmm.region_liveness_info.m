%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File rbmm.region_liveness_info.m.
% Main author: Quan Phan.
%
% Defines the data structures used in several phases of the live region
% analysis.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.rbmm.region_liveness_info.

:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.rbmm.points_to_graph.
:- import_module transform_hlds.smm_common.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module set.

:- type execution_path == assoc_list(program_point, hlds_goal).
:- type execution_path_table == map(pred_proc_id, list(execution_path)).

%---------------------------------------------------------------------------%
%
% The part for program variables.
%

    % Represents a set of program variables.
    %
:- type variable_set == set(prog_var).

    % Represents the relation between a program point and a set of
    % variables. E.g., a map of this type can be the sets of live
    % variables before program points in a procedure.
    %
:- type pp_varset_table == map(program_point, variable_set).

    % Represents the relation between a procedure and sets of variables
    % associated with its program points.
    %
:- type proc_pp_varset_table == map(pred_proc_id, pp_varset_table).

    % Find input and output formal arguments of the procedure.
    %
:- pred find_input_output_args(module_info::in, proc_info::in,
    list(prog_var)::out, list(prog_var)::out) is det.

%---------------------------------------------------------------------------%
%
% The part for region/node
%

    % Represents a set of regions, e.g., deadR, bornR, ..., live regions
    % before and after a program point.
    %
:- type region_set == set(rptg_node).

:- pred region_set_equal(region_set::in, region_set::in) is semidet.

    % Represents the relation between a procedure and its interesting sets
    % of regions, such as deadR, bornR, constantR.
    %
:- type proc_region_set_table == map(pred_proc_id, region_set).

:- pred proc_region_set_table_equal(proc_region_set_table::in,
    proc_region_set_table::in) is semidet.

    % Represents the relation between a program point and its region set
    % e.g., live regions before the pp, live regions after the pp.
    %
:- type pp_region_set_table == map(program_point, region_set).

    % Represents the relation between a procedure and sets of regions
    % associated with its program points.
    %
:- type proc_pp_region_set_table == map(pred_proc_id, pp_region_set_table).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.arg_info.
:- import_module hlds.vartypes.

find_input_output_args(ModuleInfo, CalleeProcInfo, Inputs, Outputs) :-
    proc_info_get_headvars(CalleeProcInfo, ArgVars),
    proc_info_get_vartypes(CalleeProcInfo, VarTypes),
    proc_info_get_argmodes(CalleeProcInfo, ArgModes),
    arg_info.compute_in_and_out_vars(ModuleInfo, VarTypes, ArgVars, ArgModes,
        Inputs, Outputs).

region_set_equal(RegionSet1, RegionSet2) :-
    set.equal(RegionSet1, RegionSet2).

proc_region_set_table_equal(ProcRegionSetTable1, ProcRegionSetTable2) :-
    map.count(ProcRegionSetTable1, C1),
    map.count(ProcRegionSetTable2, C2),
    C1 = C2,

    map.keys(ProcRegionSetTable1, PredProcIds1),
    prst_equal_2(PredProcIds1, ProcRegionSetTable1, ProcRegionSetTable2).

:- pred prst_equal_2(list(pred_proc_id)::in, proc_region_set_table::in,
    proc_region_set_table::in) is semidet.

prst_equal_2([], _, _).
prst_equal_2([PPId | PPIds], PRST1, PRST2) :-
    map.search(PRST2, PPId, RS2),

    map.lookup(PRST1, PPId, RS1),
    set.equal(RS1, RS2),
    prst_equal_2(PPIds, PRST1, PRST2).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.region_liveness_info.
%---------------------------------------------------------------------------%
