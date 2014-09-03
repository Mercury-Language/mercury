%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006, 2009-2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% check_hlds: semantic analysis and error checking (the "front end" HLDS pass).

:- module check_hlds.
:- interface.

%-----------------------------------------------------------------------------%

% Type checking.
%:- module type_analysis.
    :- include_module check_typeclass.
    :- include_module post_typecheck.
    :- include_module purity.
    :- include_module type_constraints.
    :- include_module type_util.
    :- include_module typecheck.
    :- include_module typecheck_errors.
    :- include_module typecheck_info.
    :- include_module typeclasses.
%:- end_module type_analysis.

% Implementation-defined literals.
:- include_module implementation_defined_literals. 

% Polymorphism transformation.
:- include_module clause_to_proc.
:- include_module polymorphism.

% Mode analysis
%:- module mode_analysis.
   :- include_module delay_info.
   :- include_module delay_partial_inst.
   :- include_module inst_match.
   :- include_module inst_util.
   :- include_module mode_constraint_robdd.
   :- include_module mode_constraints.
   :- include_module mode_debug.
   :- include_module mode_errors.
   :- include_module mode_info.
   :- include_module mode_ordering.
   :- include_module mode_util.
   :- include_module modecheck_goal.
   :- include_module modecheck_conj.
   :- include_module modecheck_call.
   :- include_module modecheck_unify.
   :- include_module modecheck_util.
   :- include_module modes.
   :- include_module unify_proc.
   :- include_module unique_modes.

   % XXX This doesn't belong here but we don't know where it's home is at
   % the moment.
   %
   :- include_module abstract_mode_constraints.
   :- include_module build_mode_constraints.
   :- include_module mcsolver.
   :- include_module ordering_mode_constraints.
   :- include_module prop_mode_constraints.

%:- end_module mode_analysis.

% Indexing and determinism analysis.
:- include_module cse_detection.
:- include_module det_analysis.
:- include_module det_report.
:- include_module det_util.
:- include_module switch_detection.

% Stratification.
:- include_module stratify.

% Order independent state update pragmas.
:- include_module oisu_check.

% Expand try goals.
:- include_module try_expand.

% Warnings about simple code.
:- include_module simplify.

% Warnings about insts with no matching types
:- include_module inst_check.

% Warnings about unused imports.
:- include_module unused_imports.

% Output XML representation useful for documentation of module.
:- include_module xml_documentation.

%-----------------------------------------------------------------------------%
:- end_module check_hlds.
%-----------------------------------------------------------------------------%
