%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% Copyright (C) 2014-2021, 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: superhomogeneous_util.m.
%
% This module provides types and utility predicates that are used by
% superhomogeneous.m and superhomogeneous_lambda.m.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.superhomogeneous_util.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.state_var.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module cord.
:- import_module list.

%---------------------------------------------------------------------------%

:- type arg_context
    --->    ac_head(pred_or_func, pred_form_arity)
            % The arguments in the head of the clause.

    ;       ac_call(call_id)
            % The arguments in a call to a predicate.

    ;       ac_functor(            % The arguments in a functor.
                cons_id,
                unify_main_context,
                list(unify_sub_context)
            ).

    % A variable and a term it is to be unified with.
:- type unify_var_term
    --->    unify_var_term(prog_var, prog_term).

    % A variable and a term it is to be unified with, and information
    % about where the unification is taking place: the argument number
    % in a call, and the context of that argument.
:- type unify_var_term_num_context
    --->    unify_var_term_num_context(prog_var, prog_term, int, arg_context).

:- pred pair_vars_with_terms(list(prog_var)::in, list(prog_term)::in,
    list(unify_var_term)::out) is det.

%---------------------------------------------------------------------------%

    % make_fresh_arg_vars_subst_svars(Args, Vars, VarsArgs,
    %   !SVarState, !UrInfo):
    %
    % Ensure we have a distinct variable for each term in Args.
    % Return a list of these variables in Vars, and return lists of each Var
    % and Arg packaged together in VarsArgs. (Almost all of our callers
    % want both.)
    %
    % For each term in Args, if the term is a variable V which is distinct
    % from the variables already produced, then use just V as the distinct
    % variable paired with the term in VarsArgs. If it isn't, we pair the term
    % with a fresh variable we allocate from !VarSet.
    % XXX The use of "fresh" in the name of this predicate implies
    % that we never just use V, which is misleading.
    %
    % !:VarSet will be the varset resulting after all the necessary variables
    % have been allocated. If any of the Args is of the form !.S or !:S,
    % we do state var substitution for them. We need !SVarState for correct
    % state var references, and !UrInfo for the varset and for reporting
    % incorrect state var references.
    %
:- pred make_fresh_arg_vars_subst_svars(list(prog_term)::in,
    list(prog_var)::out, list(unify_var_term)::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

:- pred make_fresh_arg_var_no_svar(prog_term::in, prog_var::out,
    list(prog_var)::in, unravel_info::in, unravel_info::out) is det.

%---------------------------------------------------------------------------%

:- type maybe_fgti_var_size
    --->    not_fgti
    ;       fgti_var_size(prog_var, int).

:- type expansion
    --->    expansion(
                maybe_fgti_var_size,
                cord(hlds_goal)
            ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%

pair_vars_with_terms([], [], []).
pair_vars_with_terms([], [_ | _], _) :-
    unexpected($pred, "length mismatch").
pair_vars_with_terms([_ | _], [], _) :-
    unexpected($pred, "length mismatch").
pair_vars_with_terms([Var | Vars], [Term | Terms], [VarTerm | VarsTerms]) :-
    VarTerm = unify_var_term(Var, Term),
    pair_vars_with_terms(Vars, Terms, VarsTerms).

%---------------------------------------------------------------------------%

make_fresh_arg_vars_subst_svars(Args, Vars, VarsArgs, !SVarState, !UrInfo) :-
    % For efficiency, we construct `VarsArgs' backwards and then reverse it
    % to get the correct order.
    make_fresh_arg_vars_subst_svars_loop(Args, Vars, [], RevVarsArgs,
        !SVarState, !UrInfo),
    list.reverse(RevVarsArgs, VarsArgs).

:- pred make_fresh_arg_vars_subst_svars_loop(
    list(prog_term)::in, list(prog_var)::out,
    list(unify_var_term)::in, list(unify_var_term)::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

make_fresh_arg_vars_subst_svars_loop([], [],
        !RevVarsArgs, !SVarState, !UrInfo).
make_fresh_arg_vars_subst_svars_loop([Arg | Args], [Var | Vars],
        !RevVarsArgs, !SVarState, !UrInfo) :-
    make_fresh_arg_var_subst_svars(Arg, Var,
        !RevVarsArgs, !SVarState, !UrInfo),
    make_fresh_arg_vars_subst_svars_loop(Args, Vars,
        !RevVarsArgs, !SVarState, !UrInfo).

:- pred make_fresh_arg_var_subst_svars(prog_term::in, prog_var::out,
    list(unify_var_term)::in, list(unify_var_term)::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

make_fresh_arg_var_subst_svars(Arg0, Var, !RevVarsArgs, !SVarState, !UrInfo) :-
    replace_any_dot_colon_state_var_in_term(Arg0, Arg, !SVarState, !UrInfo),
    (
        Arg = term.variable(ArgVar, _),
        ( if have_seen_arg_var(!.RevVarsArgs, ArgVar) then
            % This is the second or later appearance of ArgVar
            % in the argument list.
            create_new_unravel_var(Var, !UrInfo)
        else
            Var = ArgVar
        )
    ;
        Arg = term.functor(_, _, _),
        create_new_unravel_var(Var, !UrInfo)
    ),
    !:RevVarsArgs = [unify_var_term(Var, Arg) | !.RevVarsArgs].

:- pred have_seen_arg_var(list(unify_var_term)::in, prog_var::in) is semidet.

have_seen_arg_var([RevUnifyVarTerm | RevUnifyVarTerms], ArgVar) :-
    RevUnifyVarTerm = unify_var_term(RevVar, _),
    ( if RevVar = ArgVar then
        true
    else
        have_seen_arg_var(RevUnifyVarTerms, ArgVar)
    ).

make_fresh_arg_var_no_svar(Arg, Var, Vars0, !UrInfo) :-
    ( if
        Arg = term.variable(ArgVar, _),
        not list.member(ArgVar, Vars0)
    then
        Var = ArgVar
    else
        create_new_unravel_var(Var, !UrInfo)
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.superhomogeneous_util.
%---------------------------------------------------------------------------%
