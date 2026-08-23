%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2013-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_pred.m.
% Main authors: fjh, conway.
%
% This module defines types that are specific to fields of pred_infos.
%
%---------------------------------------------------------------------------%

:- module hlds.pred_info_types.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_data_rare.

:- import_module bool.
:- import_module map.
:- import_module one_or_more.
:- import_module pair.

%---------------------------------------------------------------------------%

    % A predicate, and the goal inside it, may implement a promise declaration,
    % or it may be an ordinary predicate.
:- type goal_type
    --->    goal_not_for_promise(np_goal_type)
    ;       goal_for_promise(promise_type).

    % An ordinary non-promise predicate may be defined by Mercury clauses,
    % foreign procs, both, or neither. (The last is the recorded situation
    % when we have added the predicate's declaration to the HLDS but have not
    % processed any clauses or foreign procs just yet.)
    %
    % We use this information in two ways.
    %
    % First, intermodule optimization needs to know whether a predicate's
    % definition contains any foreign_procs, because if it does, then
    % it cannot append variable numbers after variable names for
    % disambiguation (in e.g. clause heads) since that would screw up
    % references to those variables in the foreign code.
    %
    % Second, purity.m has special handling for predicates that are defined
    % *only* by foreign procs.
    %
    % Therefore the compiler does make a distinction between how it handles
    % np_goal_type_foreign and np_goal_type_clause_and_foreign.
    %
    % As it happens, the compiler makes no distinction between how it handles
    % np_goal_type_none and np_goal_type_clause, with the obvious exception
    % that adding a foreign proc to the two results in no_goal_types that
    % *are* distinguishable.
:- type np_goal_type
    --->    np_goal_type_none
    ;       np_goal_type_clause
    ;       np_goal_type_foreign
    ;       np_goal_type_clause_and_foreign.

%---------------------%

    % This type is isomorphic to the module_section type, but
    % defining it here allows us not to depend on prog_parse_tree.m.
:- type decl_section
    --->    decl_interface
    ;       decl_implementation.

:- type maybe_predmode_decl
    --->    no_predmode_decl
    ;       predmode_decl.

:- type cur_user_decl_info
    --->    cur_user_decl_info(
                decl_section,
                maybe_predmode_decl,
                item_seq_num
            ).

%---------------------%

:- type format_call_info
    --->    format_call_info(
                % The context of the format_call pragma whose info
                % this field of the pred_info records. We use this
                % to generate more informative error messages in cases of
                % duplicate format_call pragmas.
                prog_context,

                % The <format string arg #, values list arg #> pairs
                % listed in that pragma.
                one_or_more(format_string_values)
            ).

%---------------------%

    % Mode information for the arguments of a procedure.
    % The first map gives the instantiation state on entry of the node
    % corresponding to the prog_var. The second map gives the instantiation
    % state on exit.
    %
:- type arg_modes_map == pair(map(prog_var, bool)).

%---------------------------------------------------------------------------%
:- end_module hlds.pred_info_types.
%---------------------------------------------------------------------------%
