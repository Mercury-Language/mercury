%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% check_hlds: semantic analysis and error checking
%	(the "front end" HLDS pass).
%

:- module check_hlds.
:- interface.
:- import_module hlds, parse_tree, libs.
:- import_module backend_libs. % for base_typeclass_info, etc.
%:- import_module check_hlds__type_analysis, check_hlds__mode_analysis.

%-----------------------------------------------------------------------------%

% Type checking
%:- module type_analysis.
    :- include_module check_typeclass, typecheck, purity, post_typecheck.
    :- include_module type_util.
%:- end_module type_analysis.

% Polymorphism transformation.
:- include_module polymorphism.
:- include_module clause_to_proc.

% Mode analysis
%:- module mode_analysis.
   :- include_module modes, modecheck_unify, modecheck_call.
   :- include_module mode_info, delay_info, inst_match.
   :- include_module inst_util, mode_errors, mode_util, mode_debug.
   :- include_module unique_modes.
   :- include_module unify_proc.
%:- end_module mode_analysis.

% Indexing and determinism analysis
:- include_module switch_detection, cse_detection, det_analysis.
:- include_module det_report, det_util.

% Stratification.
:- include_module stratify.

% Warnings about simple code
:- include_module simplify.
:- include_module common.

:- include_module goal_path.

%-----------------------------------------------------------------------------%
          
:- implementation.
:- import_module transform_hlds. % for pd_cost, etc.
:- import_module ll_backend. % XXX for code_util, code_aux

:- end_module check_hlds.

%-----------------------------------------------------------------------------%
