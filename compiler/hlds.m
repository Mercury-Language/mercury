%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2010, 2012 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains the HLDS data structure, together with modules
% for creating and manipulating it.
%

:- module hlds.
:- interface.

%-----------------------------------------------------------------------------%

% The HLDS data structure itself.
:- include_module assertion.
:- include_module const_struct.
:- include_module hlds_args.
:- include_module hlds_clauses.
:- include_module hlds_data.
:- include_module hlds_goal.
:- include_module hlds_llds.
:- include_module hlds_module.
:- include_module hlds_pred.
:- include_module hlds_promise.
:- include_module hlds_rtti.
:- include_module inst_graph.
:- include_module instmap.
:- include_module pred_table.
:- include_module special_pred.
:- include_module status.
:- include_module vartypes.

% Modules for creating the HLDS.
:- include_module add_foreign_enum.
:- include_module add_pred.
:- include_module add_special_pred.
:- include_module du_type_layout.
:- include_module default_func_mode.
:- include_module hhf.
:- include_module make_hlds.
:- include_module make_hlds_error.
:- include_module pre_quantification.
:- include_module quantification.

% Modules for pretty-printing it.
:- include_module hlds_desc.
:- include_module hlds_out.

% Modules for handling errors.
:- include_module hlds_error_util.
:- include_module error_msg_inst.

% Miscellaneous utilities.
:- include_module arg_info.
:- include_module code_model.
:- include_module hlds_dependency_graph.
:- include_module from_ground_term_util.
:- include_module goal_form.
:- include_module goal_path.
:- include_module goal_util.
:- include_module headvar_names.
:- include_module hlds_defns.
:- include_module hlds_code_util.
:- include_module hlds_statistics.
:- include_module make_goal.
:- include_module mark_static_terms.
:- include_module mark_tail_calls.
:- include_module passes_aux.

%-----------------------------------------------------------------------------%
:- end_module hlds.
%-----------------------------------------------------------------------------%
