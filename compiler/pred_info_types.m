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
% This module defines the part of the HLDS that deals with predicates
% and procedures.
%
%---------------------------------------------------------------------------%

:- module hlds.pred_info_types.
:- interface.

:- import_module hlds.hlds_llds.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_data_rare.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module map.
:- import_module one_or_more.
:- import_module pair.

%---------------------------------------------------------------------------%

:- type implementation_language
    --->    impl_lang_mercury
    ;       impl_lang_foreign(foreign_language).

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
    % First, intermod.m needs to know whether a predicate's definition
    % contains any foreign_procs, because if it does, then it cannot append
    % variable numbers after variable names for disambiguation, in e.g. clause
    % heads, since that would screw up references to those variables in the
    % foreign code.
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

    % NOTE: `liveness_info' records liveness in the sense used by code
    % generation. This is *not* the same thing as the notion of liveness
    % used by mode analysis!  See compiler/notes/glossary.html.
    %
:- type liveness_info == set_of_progvar.    % The live variables.

:- type arg_info
    --->    arg_info(
                arg_loc,                    % Stored location.
                top_functor_mode            % Mode of top functor.
            ).

    % The top_functor_mode specifies the mode of the top-level functor
    % of a term (excluding `no_tag' functors, since those have no
    % representation). It is used by the code generators when determining
    % how to pass the argument.
    %
    % For the LLDS back-end, top_in arguments are passed in registers,
    % and top_out values are returned in registers; top_unused values
    % are not passed at all, but they are treated as if they were top_out
    % for the purpose of assigning arguments to registers. (So e.g. if
    % a det procedure has three arguments with top_functor_modes top_out,
    % top_unused, and top_out respectively, the last argument will be
    % returned in register r3, not r2.)
    %
    % For the MLDS back-end, top_in values are passed as arguments.
    % Top_out values are normally passed by reference, except that
    %   - if the procedure is model_nondet, and the --nondet-copy-out option
    %     is set, top_out values are passed by value to the continuation
    %     function;
    %   - if the procedure is model_det or model_semi, and the
    %     --det-copy-out option is set, top_out arguments in the HLDS
    %     are mapped to (multiple) return values in the MLDS; and
    %   - if the HLDS function return value for a det function has mode
    %     `top_out', it is mapped to an MLDS return value.
    % top_unused arguments are not passed at all.
    %
:- type top_functor_mode
    --->    top_in
    ;       top_out
    ;       top_unused.

:- type arg_loc
    --->    reg(reg_type, int).

%---------------------%

    % This type is isomorphic to the module_section type, but defining it here
    % allows us not to depend on parse_tree.prog_item.m.
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

    % Mode information for the arguments of a procedure.
    % The first map gives the instantiation state on entry of the node
    % corresponding to the prog_var. The second map gives the instantiation
    % state on exit.
    %
:- type arg_modes_map == pair(map(prog_var, bool)).

%---------------------------------------------------------------------------%
:- end_module hlds.pred_info_types.
%---------------------------------------------------------------------------%
