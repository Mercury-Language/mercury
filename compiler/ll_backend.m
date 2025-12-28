%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002, 2003-2009, 2012 The University of Melbourne.
% Copyright (C) 2014-2015, 2018, 2022, 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains the low-level back-end
% (a.k.a. the LLDS back-end).
%

:- module ll_backend.
:- interface.

%-----------------------------------------------------------------------------%

% LLDS-specific pre-passes to transform or annotate the HLDS.
:- include_module deep_profiling.       % transform
:- include_module coverage_profiling.   % transform
:- include_module saved_vars.           % transform
:- include_module stack_opt.            % transform
:- include_module follow_code.          % transform
:- include_module liveness.             % annotate
:- include_module stack_alloc.          % annotate
    :- include_module live_vars.        % annotate
:- include_module store_alloc.          % annotate

% The LLDS data structure itself.
% :- module llds_data.
    :- include_module llds.
    :- include_module global_data.
    :- include_module prog_rep.
    :- include_module stack_layout.
% :- end_module llds_data.

% The HLDS->LLDS code generator.
:- include_module proc_gen.
    :- include_module unify_gen_construct.
    :- include_module call_gen.
    :- include_module continuation_info.

% An alternative HLDS->LLDS code generator for fact tables.
:- include_module fact_table_gen.

% LLDS->LLDS optimization passes.
:- include_module optimize.
    :- include_module dupproc.

% The LLDS->C output phase.
:- include_module transform_llds.
:- include_module llds_out.
:- include_module layout_out.
:- include_module rtti_out.

%-----------------------------------------------------------------------------%
:- implementation.
%-----------------------------------------------------------------------------%

% LLDS-specific pre-passes to transform or annotate the HLDS.
    :- include_module follow_vars.      % annotate

% The LLDS data structure itself.
% :- module llds_data.
    :- include_module code_util.
    :- include_module ll_pseudo_type_info.
    :- include_module layout.
    :- include_module prog_rep_tables.
% :- end_module llds_data.

% The HLDS->LLDS code generator.
    :- include_module code_gen.
    :- include_module unify_gen.
        :- include_module unify_gen_deconstruct.
        :- include_module unify_gen_test.
        :- include_module unify_gen_util.
        :- include_module closure_gen.
    :- include_module foreign_proc_gen.
    :- include_module ite_gen.
    :- include_module disj_gen.
    :- include_module switch_gen.
        :- include_module dense_switch.
        :- include_module lookup_switch.
        :- include_module string_switch.
        :- include_module tag_switch.
        :- include_module switch_case.
    :- include_module commit_gen.
    :- include_module par_conj_gen.
    :- include_module middle_rec.

    :- include_module code_info.
    :- include_module code_loc_dep.
    :- include_module var_locn.

    :- include_module exprn_aux.
    :- include_module lookup_util.
    :- include_module trace_gen.

% LLDS->LLDS optimization passes.
    :- include_module basic_block.
    :- include_module delay_slot.
    :- include_module dupelim.
    :- include_module frameopt.
    :- include_module jumpopt.
    :- include_module labelopt.
    :- include_module livemap.
    :- include_module opt_debug.
    :- include_module opt_util.
    :- include_module peephole.
    :- include_module reassign.
    :- include_module stdlabel.
    :- include_module use_local_vars.
    :- include_module wrap_blocks.

%-----------------------------------------------------------------------------%
:- end_module ll_backend.
%-----------------------------------------------------------------------------%
