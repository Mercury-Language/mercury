%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2006, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make_hlds.m.
% Main author: fjh.
%
% This package converts an augmented compilation unit
% into the high level data structure defined in hlds.m.
%
% The augmented compilation unit contains the parse trees of
%
% - the module being compiled,
% - the parse trees of the (compiler generated) interface files of the
%   modules it imports, directly or indirectly, and possibly
% - the (also compiler generated) optimization files of those modules.
%
% All these parse trees contains lists of declarations and definitions
% of entities of various kinds. This package inserts those declarations
% and definitions into the HLDS after appropriate checks, e.g. for duplicates.
% It also converts Mercury clauses from the sort of concrete syntax tree
% constructed by the parser, to the abstract syntax tree used by every
% other part of the compiler. (For example, the parser represents the
% conjunction of three goals as `conj_expr(GoalA, conj_expr(GoalB, GoalC))',
% while the HLDS represents it as conj([GoalA, GoalB, GoalC]).)
% This transformation also converts clauses into superhomogenous form,
% and quantifies apart unrelated occurrences of the same variable name.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.
:- interface.

:- include_module instance_method_clauses.
:- include_module make_hlds_passes.
:- include_module make_hlds_types.
:- include_module qual_info.

:- implementation.

:- include_module add_class.
:- include_module add_clause.
:- include_module add_foreign_proc.
:- include_module add_mode.
:- include_module add_mutable_aux_preds.
:- include_module add_pragma.
:- include_module add_solver.
:- include_module add_type.
:- include_module field_access.
:- include_module goal_expr_to_goal.
:- include_module make_hlds_warn.
:- include_module state_var.
:- include_module superhomogeneous.

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.
%---------------------------------------------------------------------------%
