%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006, 2009-2010 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
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
    :- include_module check_for_missing_type_defns.
    :- include_module check_promise.
    :- include_module old_type_constraints.
    :- include_module post_typecheck.
    :- include_module pre_typecheck.
    :- include_module purity.
    :- include_module resolve_unify_functor.
    :- include_module type_constraints.
    :- include_module type_assign.
    :- include_module type_util.
    :- include_module typecheck.
    :- include_module typecheck_clauses.
    :- include_module typecheck_debug.
    :- include_module typecheck_errors.
    :- include_module typecheck_error_overload.
    :- include_module typecheck_error_type_assign.
    :- include_module typecheck_error_undef.
    :- include_module typecheck_error_util.
    :- include_module typecheck_info.
    :- include_module typecheck_msgs.
    :- include_module typeclasses.
    :- include_module types_into_modes.
%:- end_module type_analysis.

% Implementation-defined literals.
:- include_module implementation_defined_literals.

% Populating proc_infos from the clauses.
:- include_module clause_to_proc.

% Polymorphism transformation.
:- include_module introduce_exists_casts.
:- include_module polymorphism.
:- include_module polymorphism_clause.
:- include_module polymorphism_goal.
:- include_module polymorphism_info.
:- include_module polymorphism_lambda.
:- include_module polymorphism_post_copy.
:- include_module polymorphism_type_class_info.
:- include_module polymorphism_type_info.

% Preprocessing user insts in the inst table for mode analysis.
:- include_module inst_user.

% Mode analysis.
%:- module mode_analysis.
   % The current mode analysis system, which is based (quite loosely)
   % on the idea of abtract interpretation.
   :- include_module delay_info.
   :- include_module inst_abstract_unify.
   :- include_module inst_lookup.
   :- include_module inst_match.
   :- include_module inst_merge.
   :- include_module inst_mode_type_prop.
   :- include_module inst_test.
   :- include_module inst_util.
   :- include_module mode_comparison.
   :- include_module mode_debug.
   :- include_module mode_errors.
   :- include_module mode_info.
   :- include_module mode_test.
   :- include_module mode_top_functor.
   :- include_module mode_util.
   :- include_module modecheck_call.
   :- include_module modecheck_coerce.
   :- include_module modecheck_conj.
   :- include_module modecheck_goal.
   :- include_module modecheck_unify.
   :- include_module modecheck_util.
   :- include_module modes.
   :- include_module proc_requests.
   :- include_module recompute_instmap_deltas.
   :- include_module unify_proc.
   :- include_module unique_modes.

   % The following modules implement the first (robdd-based)
   % prototype constraint based mode checker, which was never completed.
   :- include_module mode_constraint_robdd.
   :- include_module mode_constraints.
   :- include_module mode_ordering.

   % The following modules implement the second (propagation-based)
   % prototype constraint based mode checker, which was never completed.
   :- include_module abstract_mode_constraints.
   :- include_module build_mode_constraints.
   :- include_module mcsolver.
   :- include_module ordering_mode_constraints.
   :- include_module prop_mode_constraints.

%:- end_module mode_analysis.

% A pass that can sometimes make the result of mode analysis
% look and behave better.
:- include_module delay_partial_inst.

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

% Check the information in format_call pragmas.
:- include_module check_pragma_format_call.

% Warnings about simple code.
:- include_module simplify.

% Warnings about insts with no matching types.
:- include_module inst_check.

% Warnings about unused imports.
:- include_module unused_imports.

% Warnings about bad programming style.
:- include_module style_checks.

% Output XML representation useful for documentation of module.
:- include_module xml_documentation.

%-----------------------------------------------------------------------------%
:- end_module check_hlds.
%-----------------------------------------------------------------------------%
