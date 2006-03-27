%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_clauses.m.
% Main authors: fjh, conway.
%
% This module defines the part of the HLDS that deals with clauses.
%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_clauses.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.

    % The clauses_info structure contains the clauses for a predicate
    % after conversion from the item_list by make_hlds.m.
    % Typechecking is performed on the clauses info, then the clauses
    % are copied to create the proc_info for each procedure.
    % After mode analysis the clauses and the procedure goals are not
    % guaranteed to be the same, and the clauses are only kept so that
    % the optimized goal can be compared with the original in HLDS dumps.
:- type clauses_info
    --->    clauses_info(
                varset                  :: prog_varset,

                explicit_vartypes       :: vartypes,
                                        % Variable types from explicit
                                        % qualifications.

                tvar_name_map           :: tvar_name_map,
                                        % Map from variable name to type
                                        % variable for the type variables
                                        % occurring in the argument types.
                                        % This is used to process explicit
                                        % type qualifications.

                vartypes                :: vartypes,
                                        % Variable types inferred by
                                        % typecheck.m.

                headvars                :: list(prog_var),
                                        % The head variables.

                clauses_rep             :: clauses_rep,

                clauses_rtti_varmaps    :: rtti_varmaps,
                                        % This field is computed by
                                        % polymorphism.m.

                have_foreign_clauses    :: bool
                                        % Do we have foreign language clauses?
        ).

:- pred clauses_info_init(int::in, clauses_info::out) is det.

:- pred clauses_info_init_for_assertion(prog_vars::in, clauses_info::out)
    is det.

:- type clauses_rep.

:- func init_clauses_rep = clauses_rep.

    % Returns yes iff the given clauses_rep represents the empty list of
    % clauses.
    %
:- func clause_list_is_empty(clauses_rep) = bool.

    % Adds the given clause to the end of the clause list.
    %
:- pred add_clause(clause::in, clauses_rep::in, clauses_rep::out) is det.

    % Get the list of clauses in the given clauses_rep in whatever order
    % happens to be efficient.
    %
:- pred get_clause_list_any_order(clauses_rep::in, list(clause)::out) is det.

    % Get the list of clauses in the given clauses_rep in program order.
    %
:- pred get_clause_list(clauses_rep::in, list(clause)::out) is det.

    % Set the list of clauses to the one given.
    %
:- pred set_clause_list(list(clause)::in, clauses_rep::out) is det.

:- pred clauses_info_get_varset(clauses_info::in, prog_varset::out) is det.

    % This partial map holds the types specified by any explicit
    % type qualifiers in the clauses.
    %
:- pred clauses_info_get_explicit_vartypes(clauses_info::in, vartypes::out) is det.

    % This map contains the types of all the variables, as inferred
    % by typecheck.m.
    %
:- pred clauses_info_get_vartypes(clauses_info::in, vartypes::out) is det.

:- pred clauses_info_get_rtti_varmaps(clauses_info::in, rtti_varmaps::out) is det.

:- pred clauses_info_get_headvars(clauses_info::in, list(prog_var)::out) is det.

:- pred clauses_info_get_clauses_rep(clauses_info::in, clauses_rep::out) is det.

    % Return the list of clauses in program order.
    %
:- pred clauses_info_clauses_only(clauses_info::in, list(clause)::out) is det.

    % Return the list of clauses in program order, and if necessary update
    % the cache of this info in the clauses_info.
    %
:- pred clauses_info_clauses(list(clause)::out,
    clauses_info::in, clauses_info::out) is det.

:- pred clauses_info_set_headvars(list(prog_var)::in,
    clauses_info::in, clauses_info::out) is det.

:- pred clauses_info_set_clauses(list(clause)::in,
    clauses_info::in, clauses_info::out) is det.

:- pred clauses_info_set_clauses_rep(clauses_rep::in,
    clauses_info::in, clauses_info::out) is det.

:- pred clauses_info_set_varset(prog_varset::in,
    clauses_info::in, clauses_info::out) is det.

    % This partial map holds the types specified by any explicit
    % type qualifiers in the clauses.
    %
:- pred clauses_info_set_explicit_vartypes(vartypes::in,
    clauses_info::in, clauses_info::out) is det.

    % This map contains the types of all the variables, as inferred
    % by typecheck.m.
    %
:- pred clauses_info_set_vartypes(vartypes::in,
    clauses_info::in, clauses_info::out) is det.

:- pred clauses_info_set_rtti_varmaps(rtti_varmaps::in,
    clauses_info::in, clauses_info::out) is det.

:- type clause
    --->    clause(
                applicable_procs    :: list(proc_id),
                                    % Modes for which this clause applies
                                    % ([] means it applies to all modes).

                clause_body         :: hlds_goal,
                clause_lang         :: implementation_language,
                clause_context      :: prog_context
            ).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.make_hlds.
:- import_module libs.compiler_util.
:- import_module libs.options.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module map.
:- import_module string.
:- import_module svmap.
:- import_module term.
:- import_module varset.

clauses_info_init(Arity, ClausesInfo) :-
    map.init(VarTypes),
    map.init(TVarNameMap),
    varset.init(VarSet0),
    make_n_fresh_vars("HeadVar__", Arity, HeadVars, VarSet0, VarSet),
    rtti_varmaps_init(RttiVarMaps),
    HasForeignClauses = no,
    set_clause_list([], ClausesRep),
    ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
        HeadVars, ClausesRep, RttiVarMaps, HasForeignClauses).

clauses_info_init_for_assertion(HeadVars, ClausesInfo) :-
    map.init(VarTypes),
    map.init(TVarNameMap),
    varset.init(VarSet),
    rtti_varmaps_init(RttiVarMaps),
    HasForeignClauses = no,
    set_clause_list([], ClausesRep),
    ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
        HeadVars, ClausesRep, RttiVarMaps, HasForeignClauses).

clauses_info_get_varset(CI, CI ^ varset).
clauses_info_get_explicit_vartypes(CI, CI ^ explicit_vartypes).
clauses_info_get_vartypes(CI, CI ^ vartypes).
clauses_info_get_headvars(CI, CI ^ headvars).
clauses_info_get_clauses_rep(CI, CI ^ clauses_rep).
clauses_info_get_rtti_varmaps(CI, CI ^ clauses_rtti_varmaps).

clauses_info_set_varset(X, CI, CI ^ varset := X).
clauses_info_set_explicit_vartypes(X, CI, CI ^ explicit_vartypes := X).
clauses_info_set_vartypes(X, CI, CI ^ vartypes := X).
clauses_info_set_headvars(X, CI, CI ^ headvars := X).
clauses_info_set_clauses(X, CI, CI ^ clauses_rep := forw(X)).
clauses_info_set_clauses_rep(X, CI, CI ^ clauses_rep := X).
clauses_info_set_rtti_varmaps(X, CI, CI ^ clauses_rtti_varmaps := X).

:- type clauses_rep
    --->    rev(list(clause))
    ;       forw(list(clause))
    ;       both(
                rev :: list(clause),
                forw :: list(clause)
            ).

init_clauses_rep = forw([]).

clause_list_is_empty(ClausesRep) = IsEmpty :-
    (
        ClausesRep = rev(List)
    ;
        ClausesRep = forw(List)
    ;
        ClausesRep = both(List, _)
    ),
    (
        List = [],
        IsEmpty = yes
    ;
        List = [_ | _],
        IsEmpty = no
    ).

get_clause_list_any_order(ClausesRep, Clauses) :-
    (
        ClausesRep = rev(Clauses)
    ;
        ClausesRep = forw(Clauses)
    ;
        ClausesRep = both(_, Clauses)
    ).

get_clause_list(ClausesRep, Clauses) :-
    (
        ClausesRep = rev(RevClauses),
        list.reverse(RevClauses, Clauses)
    ;
        ClausesRep = forw(Clauses)
    ;
        ClausesRep = both(_, Clauses)
    ).

set_clause_list(Clauses, forw(Clauses)).

clauses_info_clauses_only(CI, Clauses) :-
    ClausesRep = CI ^ clauses_rep,
    get_clause_list(ClausesRep, Clauses).

clauses_info_clauses(Clauses, !CI) :-
    ClausesRep = !.CI ^ clauses_rep,
    (
        ClausesRep = rev(RevClauses),
        list.reverse(RevClauses, Clauses),
        !:CI = !.CI ^ clauses_rep := both(RevClauses, Clauses)
    ;
        ClausesRep = forw(Clauses)
    ;
        ClausesRep = both(_, Clauses)
    ).

add_clause(Clause, !ClausesRep) :-
    % We keep the clause list in reverse order, to make it possible
    % to add other clauses without quadratic behavior.
    (
        !.ClausesRep = rev(RevClauses0),
        RevClauses = [Clause | RevClauses0],
        !:ClausesRep = rev(RevClauses)
    ;
        !.ClausesRep = forw(Clauses0),
        list.reverse(Clauses0, RevClauses0),
        RevClauses = [Clause | RevClauses0],
        !:ClausesRep = rev(RevClauses)
    ;
        !.ClausesRep = both(RevClauses0, _),
        RevClauses = [Clause | RevClauses0],
        !:ClausesRep = rev(RevClauses)
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "hlds_clauses.m".

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_clauses.
%-----------------------------------------------------------------------------%
