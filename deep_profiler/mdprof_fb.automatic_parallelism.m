%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mdprof_fb.automatic_parallelism.m.
%
% This package holds the modules that generate recommendations to the compiler
% about what conjunctions to parallelize, and how.
%
%---------------------------------------------------------------------------%

:- module mdprof_fb.automatic_parallelism.
:- interface.

:- include_module mdprof_fb.automatic_parallelism.autopar_reports.
:- include_module mdprof_fb.automatic_parallelism.autopar_search_callgraph.

:- implementation.

:- include_module mdprof_fb.automatic_parallelism.autopar_annotate.
:- include_module mdprof_fb.automatic_parallelism.autopar_calc_overlap.
:- include_module mdprof_fb.automatic_parallelism.autopar_costs.
:- include_module mdprof_fb.automatic_parallelism.autopar_find_best_par.
:- include_module mdprof_fb.automatic_parallelism.autopar_search_goals.
:- include_module mdprof_fb.automatic_parallelism.autopar_types.
