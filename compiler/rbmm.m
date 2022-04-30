%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2007, 2009-2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: rbmm.m.
%
% This package implements region-based memory management.
%
%---------------------------------------------------------------------------%

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
:- include_module region_analysis.
:- include_module region_arguments.
:- include_module region_instruction.
:- include_module region_liveness_info.
:- include_module region_resurrection_renaming.
:- include_module region_transformation.

%---------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.
%---------------------------------------------------------------------------%
