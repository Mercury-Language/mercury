%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% check_hlds: semantic analysis and error checking
%	(the "front end" HLDS pass).
%

:- module check_hlds.
:- interface.

:- import_module hlds.
:- import_module mdbcomp.
:- import_module parse_tree.

% :- import_module check_hlds__type_analysis.
% :- import_module check_hlds__mode_analysis.

%-----------------------------------------------------------------------------%

% Type checking
%:- module type_analysis.
    :- include_module check_typeclass.
    :- include_module post_typecheck.
    :- include_module purity.
    :- include_module type_util.
    :- include_module typecheck.
%:- end_module type_analysis.

% Polymorphism transformation.
:- include_module clause_to_proc.
:- include_module polymorphism.

% Mode analysis
%:- module mode_analysis.
   :- include_module abstract_mode_constraints.
   :- include_module build_mode_constraints.
   :- include_module delay_info.
   :- include_module inst_match.
   :- include_module inst_util.
   :- include_module mode_constraint_robdd.
   :- include_module mode_constraints.
   :- include_module mode_debug.
   :- include_module mode_errors.
   :- include_module mode_info.
   :- include_module mode_ordering.
   :- include_module mode_util.
   :- include_module modecheck_call.
   :- include_module modecheck_unify.
   :- include_module modes.
   :- include_module prop_mode_constraints.
   :- include_module unify_proc.
   :- include_module unique_modes.
%:- end_module mode_analysis.

% Indexing and determinism analysis
:- include_module cse_detection.
:- include_module det_analysis.
:- include_module det_report.
:- include_module det_util.
:- include_module switch_detection.

% Stratification.
:- include_module stratify.

% Warnings about simple code
:- include_module common.
:- include_module simplify.

:- include_module goal_path.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module transform_hlds.	% for pd_cost, etc.

:- end_module check_hlds.

%-----------------------------------------------------------------------------%
