%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997,2002-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: term_constr_main_types.m.
% Main author: juliensf.
%
% This module contains the definitions of the types that record the results
% of the constraint-based termination analysis in the HLDS.
%
%----------------------------------------------------------------------------%

:- module transform_hlds.term_constr_main_types.
:- interface.

:- import_module libs.
:- import_module libs.polyhedron.
:- import_module parse_tree.
:- import_module parse_tree.prog_data_pragma.
:- import_module transform_hlds.term_constr_data.
:- import_module transform_hlds.term_constr_errors.

:- import_module list.
:- import_module maybe.

%----------------------------------------------------------------------------%
%
% Types that define termination information about procedures.
%

    % This type is the interargument size relationships between
    % the arguments of a predicate.
    %
:- type constr_arg_size_info == polyhedron.

    % Whether or not a procedure is terminating and some idea of why
    % this is or is not the case.
    %
:- type constr_termination_info ==
    generic_termination_info(term_reason, list(term2_error)).

    % Why does the termination analyser think that a procedure is terminating?
    % This is useful for debugging purposes.
    %
:- type term_reason
    --->    term_reason_builtin
            % Procedure was a builtin.

    ;       term_reason_pragma_supplied
            % Procedure has pragma terminates decl.

    ;       term_reason_foreign_supplied
            % Procedure has foreign code attribute.

    ;       term_reason_import_supplied
            % This procedure was imported and its termination status
            % was read in from a .opt or .trans_opt file.

    ;       term_reason_analysis.
            % Termination info. was derived via analysis.

    % Whether a procedure may be involved in mutual recursion
    % across module boundaries.
    %
    % XXX Termination analysis of mutual recursion across module boundaries
    % NYI.
    %
:- type intermod_status
    --->    not_mutually_recursive
    ;       may_be_mutually_recursive.

%----------------------------------------------------------------------------%
%
% The 'termination2_info' structure.
%

% All the information needed by the termination analysis is stored in
% this structure. There is one such structure attached to every procedure
% in the module.

:- type termination2_info.

:- func term2_info_init = termination2_info.

:- func term2_info_get_size_var_map(termination2_info)
    = size_var_map.
:- func term2_info_get_import_success(termination2_info)
    = maybe(pragma_constr_arg_size_info).
:- func term2_info_get_import_failure(termination2_info)
    = maybe(pragma_constr_arg_size_info).
:- func term2_info_get_success_constrs(termination2_info)
    = maybe(constr_arg_size_info).
:- func term2_info_get_failure_constrs(termination2_info)
    = maybe(constr_arg_size_info).
:- func term2_info_get_abstract_rep(termination2_info)
    = maybe(abstract_proc).
:- func term2_info_get_term_status(termination2_info)
    = maybe(constr_termination_info).
:- func term2_info_get_intermod_status(termination2_info)
    = maybe(intermod_status).
:- func term2_info_get_head_vars(termination2_info)
    = list(size_var).

:- pred term2_info_set_size_var_map(size_var_map::in,
    termination2_info::in, termination2_info::out) is det.
:- pred term2_info_set_import_success(maybe(pragma_constr_arg_size_info)::in,
    termination2_info::in, termination2_info::out) is det.
:- pred term2_info_set_import_failure(maybe(pragma_constr_arg_size_info)::in,
    termination2_info::in, termination2_info::out) is det.
:- pred term2_info_set_success_constrs(maybe(constr_arg_size_info)::in,
    termination2_info::in, termination2_info::out) is det.
:- pred term2_info_set_failure_constrs(maybe(constr_arg_size_info)::in,
    termination2_info::in, termination2_info::out) is det.
:- pred term2_info_set_term_status(maybe(constr_termination_info)::in,
    termination2_info::in, termination2_info::out) is det.
:- pred term2_info_set_intermod_status(maybe(intermod_status)::in,
    termination2_info::in, termination2_info::out) is det.
:- pred term2_info_set_abstract_rep(maybe(abstract_proc)::in,
    termination2_info::in, termination2_info::out) is det.
:- pred term2_info_set_head_vars(list(size_var)::in,
    termination2_info::in, termination2_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.lp_rational.

:- import_module map.
:- import_module term.

%-----------------------------------------------------------------------------%
%
% The 'termination2_info' structure.
%

:- type termination2_info
    --->    term2_info(
                % Map between prog_vars and size_vars for this procedure.
                t2i_size_var_map        :: size_var_map,

                % These are the size variables that occur in argument
                % size constraints. For procedures that are imported
                % via a `.opt' or `.trans_opt' file, we set these during
                % the initial pass, for procedures in the module we are
                % analysing, pass 1 sets it.
                t2i_head_vars           :: size_vars,

                % Arg size info. imported from another module via a
                % `.opt' or `.trans_opt' file. Pass 0 needs to convert these
                % to the proper form. These particular fields are of no use
                % after that.
                t2i_import_success      :: maybe(pragma_constr_arg_size_info),
                t2i_import_failure      :: maybe(pragma_constr_arg_size_info),

                % The interargument size relationships
                % (expressed as convex constraints)
                % obtained during pass 1.
                t2i_success_constrs     :: maybe(constr_arg_size_info),

                % Failure constraints for predicates that can fail
                % (set by pass 1).
                t2i_failure_constrs     :: maybe(constr_arg_size_info),

                % The termination status of the procedure as determined
                % by pass 2.
                t2i_term_status         :: maybe(constr_termination_info),

                % Is this procedure (possibly) involved in mutual recursion
                % across module boundaries? Set by pass 1.
                t2i_intermod_status     :: maybe(intermod_status),

                % The abstract representation of this proc.
                % Set by term_constr_build.m.
                t2i_abstract_rep        :: maybe(abstract_proc)
            ).

term2_info_init = term2_info(map.init, [], no, no, no, no, no, no, no).

term2_info_get_size_var_map(Term2Info) = X :-
    X = Term2Info ^ t2i_size_var_map.
term2_info_get_import_success(Term2Info) = X :-
    X = Term2Info ^ t2i_import_success.
term2_info_get_import_failure(Term2Info) = X :-
    X = Term2Info ^ t2i_import_failure.
term2_info_get_success_constrs(Term2Info) = X :-
    X = Term2Info ^ t2i_success_constrs.
term2_info_get_failure_constrs(Term2Info) = X :-
    X = Term2Info ^ t2i_failure_constrs.
term2_info_get_abstract_rep(Term2Info) = X :-
    X = Term2Info ^ t2i_abstract_rep.
term2_info_get_term_status(Term2Info) = X :-
    X = Term2Info ^ t2i_term_status.
term2_info_get_intermod_status(Term2Info) = X :-
    X = Term2Info ^ t2i_intermod_status.
term2_info_get_head_vars(Term2Info) = X :-
    X = Term2Info ^ t2i_head_vars.

term2_info_set_size_var_map(X, !Term2Info) :-
    !Term2Info ^ t2i_size_var_map := X.
term2_info_set_import_success(X, !Term2Info) :-
    !Term2Info ^ t2i_import_success := X.
term2_info_set_import_failure(X, !Term2Info) :-
    !Term2Info ^ t2i_import_failure := X.
term2_info_set_success_constrs(X, !Term2Info) :-
    !Term2Info ^ t2i_success_constrs := X.
term2_info_set_failure_constrs(X, !Term2Info) :-
    !Term2Info ^ t2i_failure_constrs := X.
term2_info_set_term_status(X, !Term2Info) :-
    !Term2Info ^ t2i_term_status := X.
term2_info_set_intermod_status(X, !Term2Info) :-
    !Term2Info ^ t2i_intermod_status := X.
term2_info_set_abstract_rep(X, !Term2Info) :-
    !Term2Info ^ t2i_abstract_rep := X.
term2_info_set_head_vars(X, !Term2Info) :-
    !Term2Info ^ t2i_head_vars := X.

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.term_constr_main_types.
%-----------------------------------------------------------------------------%
