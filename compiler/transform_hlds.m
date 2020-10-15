%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2008, 2012 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% transform_hlds: High-level transformations that are independent
% of the choice of back-end (the "middle" HLDS pass).
%

:- module transform_hlds.
:- interface.

%-----------------------------------------------------------------------------%

:- include_module intermod.

:- include_module equiv_type_hlds.

:- include_module table_gen.

:- include_module complexity.

:- include_module (lambda).
:- include_module stm_expand.

:- include_module closure_analysis.

:- include_module termination.
   :- include_module term_pass1.
   :- include_module term_pass2.
   :- include_module term_traversal.
   :- include_module term_errors.
   :- include_module term_norm.
   :- include_module term_util.

:- include_module ssdebug.

:- include_module transform_hlds.ctgc.

:- include_module transform_hlds.rbmm.

:- include_module transform_hlds.smm_common.

:- include_module term_constr_main.
    :- include_module term_constr_initial.
        % Pass 1.
    :- include_module term_constr_build.
    :- include_module term_constr_fixpoint.
        % Pass 2.
    :- include_module term_constr_pass2.
        % Other bits.
    :- include_module term_constr_main_types.
    :- include_module term_constr_util.
    :- include_module term_constr_data.
    :- include_module term_constr_errors.

:- include_module post_term_analysis.
:- include_module exception_analysis.
:- include_module trailing_analysis.
:- include_module tabling_analysis.

% Mostly optimizations (HLDS -> HLDS)
:- include_module higher_order.
:- include_module inlining.
:- include_module deforest.
   :- include_module constraint.
   :- include_module pd_cost.
   :- include_module pd_debug.
   :- include_module pd_info.
   :- include_module pd_term.
   :- include_module pd_util.
:- include_module delay_construct.
:- include_module unused_args.
:- include_module unneeded_code.
:- include_module accumulator.
   :- include_module goal_store.
:- include_module dead_proc_elim.
:- include_module const_prop.
:- include_module loop_inv.
:- include_module size_prof.
:- include_module tupling.
:- include_module untupling.
:- include_module distance_granularity.
:- include_module granularity.
:- include_module dep_par_conj.
:- include_module parallel_to_plain_conj.
:- include_module implicit_parallelism.
:- include_module par_loop_control.
:- include_module lco.
:- include_module float_regs.

:- include_module mmc_analysis.

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.
%-----------------------------------------------------------------------------%
