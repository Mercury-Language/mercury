%-----------------------------------------------------------------------------%
% Copyright (C) 2002,2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains the low-level back-end
% (a.k.a. the LLDS back-end).
%
:- module ll_backend.
:- interface.

:- import_module aditi_backend.		% XXX for rl_file, used in llds_out.
:- import_module backend_libs.
:- import_module check_hlds. 		% is this needed?
:- import_module hlds.
:- import_module libs.
:- import_module parse_tree.
:- import_module transform_hlds.	% is this needed?

%-----------------------------------------------------------------------------%

% Pre-passes to transform or annotate the HLDS
% (XXX these are not listed in right order)
:- include_module saved_vars.	% transform
:- include_module stack_opt.	% transform
:- include_module stack_alloc.	% annotate
:- include_module arg_info.	% annotate
:- include_module liveness.	% annotate
:- include_module live_vars.	% annotate
:- include_module follow_code.	% transform
:- include_module follow_vars.	% annotate
:- include_module store_alloc.	% annotate
:- include_module deep_profiling. % transform

% The llds data structure itself
:- include_module llds.
:- include_module code_util. % XXX

% The HLDS->LLDS code generator.
:- include_module code_gen.
   :- include_module ite_gen.
   :- include_module call_gen.
   :- include_module disj_gen.
   :- include_module unify_gen.
   :- include_module commit_gen.
   :- include_module switch_gen.
      :- include_module dense_switch.
      :- include_module lookup_switch.
      :- include_module string_switch.
      :- include_module tag_switch.
   :- include_module pragma_c_gen.
   :- include_module par_conj_gen.
   :- include_module middle_rec.
   :- include_module trace.

   :- include_module code_info.
   :- include_module code_exprn.
   :- include_module exprn_aux.
   :- include_module code_aux. % XXX
   :- include_module continuation_info.
   :- include_module var_locn.

% An alternative HLDS->LLDS code generator for fact tables.
:- include_module fact_table.

%:- module llds_rtti.
   :- include_module ll_pseudo_type_info.
   :- include_module layout.
   :- include_module stack_layout.
   :- include_module prog_rep.
   :- include_module static_term.
%:- end_module llds_rtti.

% LLDS->LLDS optimization passes.
:- include_module optimize.
   :- include_module jumpopt.
   :- include_module dupelim.
   :- include_module frameopt.
   :- include_module delay_slot.
   :- include_module labelopt.
   :- include_module peephole.
   :- include_module reassign.
   :- include_module wrap_blocks.
   :- include_module use_local_vars.
%   :- include_module value_number.
%      :- include_module vn_block.
%      :- include_module vn_cost.
%      :- include_module vn_debug.
%      :- include_module vn_filter.
%      :- include_module vn_flush.
%      :- include_module vn_order.
%      :- include_module vn_temploc.
%      :- include_module vn_util.
%      :- include_module vn_verify.
%      :- include_module vn_type.
%      :- include_module vn_table.
   :- include_module llds_common.
   :- include_module livemap, basic_block, opt_util, opt_debug.
                
% The LLDS->C output phase.
:- include_module transform_llds.
:- include_module llds_out.
:- include_module layout_out.
:- include_module rtti_out.
  
:- end_module ll_backend.

%-----------------------------------------------------------------------------%
