%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: superhomogeneous.m.
% Main author: fjh.
%
% This module performs the conversion of clause bodies
% to superhomogeneous form.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.superhomogeneous.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.make_hlds.state_var.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type arg_context
    --->    ac_head(pred_or_func, arity)
            % The arguments in the head of the clause.

    ;       ac_call(call_id)
            % The arguments in a call to a predicate.

    ;       ac_functor(            % The arguments in a functor.
                cons_id,
                unify_main_context,
                unify_sub_contexts
            ).

    % We count how many goals we insert in the course of a call to one of the
    % predicates below. We compute this count because we want to wrap a
    % from_ground_term scope only around goals where it saves us nontrivial
    % time (since the scope itself adds overhead).
:- type num_added_goals == int.

    % `insert_arg_unifications' takes a list of variables, a list of terms
    % to unify them with, and a goal, and inserts the appropriate unifications
    % onto the front of the goal. It calls `unravel_unification' to ensure that
    % each unification gets reduced to superhomogeneous form. It also gets
    % passed an `arg_context', which indicates where the terms came from.
    %
    % We never insert unifications of the form X = X.
    %
:- pred insert_arg_unifications(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in, hlds_goal::in, hlds_goal::out,
    num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred insert_arg_unifications_with_supplied_contexts(list(prog_var)::in,
    list(prog_term)::in, assoc_list(int, arg_context)::in, prog_context::in,
    hlds_goal::in, hlds_goal::out, num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % append_arg_unifications is the same as insert_arg_unifications,
    % except that the unifications are added after the goal rather
    % than before the goal.
    %
:- pred append_arg_unifications(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in, hlds_goal::in, hlds_goal::out,
    num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred unravel_unification(prog_term::in, prog_term::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, purity::in,
    hlds_goal::out, num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % make_fresh_arg_vars(Args, Vars, !VarSet, !SVarState, !Specs):
    %
    % Vars is a list of distinct variables corresponding to the terms in Args.
    % For each term in Args, if the term is a variable V which is distinct
    % from the variables already produced, then the corresponding variable
    % in Vars is just V, otherwise a fresh variable is allocated from !VarSet.
    % !:VarSet is the varset resulting after all the necessary variables
    % have been allocated. !SVarState and !Specs are required to handle
    % state variables.
    %
:- pred make_fresh_arg_vars(list(prog_term)::in, list(prog_var)::out,
    prog_varset::in, prog_varset::out, svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred make_fresh_arg_var(prog_term::in, prog_var::out, list(prog_var)::in,
    prog_varset::in, prog_varset::out, svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.field_access.
:- import_module hlds.make_hlds.goal_expr_to_goal.
:- import_module hlds.make_hlds.qual_info.
:- import_module libs.globals.  % for get_maybe_from_ground_term_threshold
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_io_dcg.
:- import_module parse_tree.prog_io_goal.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

insert_arg_unifications(HeadVars, Args0, Context, ArgContext, !Goal, NumAdded,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    do_insert_arg_unifications(HeadVars, Args0, Context, ArgContext, !Goal,
        get_maybe_from_ground_term_threshold, NumAdded,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).

insert_arg_unifications_with_supplied_contexts(ArgVars, ArgTerms0,
        ArgContexts, Context, !Goal, NumAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    do_insert_arg_unifications_with_supplied_contexts(ArgVars, ArgTerms0,
        ArgContexts, Context, !Goal,
        get_maybe_from_ground_term_threshold, NumAdded,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).

append_arg_unifications(HeadVars, Args0, Context, ArgContext, !Goal, NumAdded,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    do_append_arg_unifications(HeadVars, Args0, Context, ArgContext, !Goal,
        get_maybe_from_ground_term_threshold, NumAdded,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).

unravel_unification(LHS0, RHS0, Context, MainContext, SubContext, Purity,
        Goal, NumAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    do_unravel_unification(LHS0, RHS0, Context, MainContext, SubContext,
        Purity, Goal, get_maybe_from_ground_term_threshold, NumAdded,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).

%-----------------------------------------------------------------------------%

:- pred do_insert_arg_unifications(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in,
    hlds_goal::in, hlds_goal::out, maybe(int)::in, num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_insert_arg_unifications(HeadVars, Args0, Context, ArgContext,
        !Goal, MaybeThreshold, NumAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    (
        HeadVars = [],
        NumAdded = 0
    ;
        HeadVars = [_ | _],
        !.Goal = hlds_goal(_, GoalInfo0),
        svar_goal_to_conj_list(!.Goal, Goals0, !SVarStore),
        substitute_state_var_mappings(Args0, Args, !VarSet, !SVarState,
            !Specs),
        do_insert_arg_unifications_loop(HeadVars, Args, Context, ArgContext,
            1, Goals0, Goals, MaybeThreshold, 0, NumAdded,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        goal_info_set_context(Context, GoalInfo0, GoalInfo),
        conj_list_to_goal(Goals, GoalInfo, !:Goal)
    ).

:- pred do_insert_arg_unifications_loop(list(prog_var)::in,
    list(prog_term)::in, prog_context::in, arg_context::in, int::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    maybe(int)::in, num_added_goals::in, num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_insert_arg_unifications_loop([], [_ | _], _, _, _, _, _, _,
        !NumAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    unexpected($module, $pred, "length mismatch").
do_insert_arg_unifications_loop([_ | _], [], _, _, _, _, _, _,
        !NumAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    unexpected($module, $pred, "length mismatch").
do_insert_arg_unifications_loop([], [], _, _, _, !Goals, _,
        !NumAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs).
do_insert_arg_unifications_loop([Var | Vars], [Arg | Args],
        Context, ArgContext, ArgNum, !Goals, MaybeThreshold,
        !NumAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    do_arg_unification(Var, Arg, Context, ArgContext, ArgNum, ArgUnifyConj,
        MaybeThreshold, ArgAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),
    !:NumAdded = !.NumAdded + ArgAdded,
    (
        ArgUnifyConj = [],
        % Allow the recursive call to be tail recursive.
        do_insert_arg_unifications_loop(Vars, Args, Context, ArgContext,
            ArgNum + 1, !Goals, MaybeThreshold, !NumAdded,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs)
    ;
        ArgUnifyConj = [_ | _],
        do_insert_arg_unifications_loop(Vars, Args, Context, ArgContext,
            ArgNum + 1, !Goals, MaybeThreshold, !NumAdded,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        !:Goals = ArgUnifyConj ++ !.Goals
    ).

:- pred do_insert_arg_unifications_with_supplied_contexts(list(prog_var)::in,
    list(prog_term)::in, assoc_list(int, arg_context)::in, prog_context::in,
    hlds_goal::in, hlds_goal::out, maybe(int)::in, num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_insert_arg_unifications_with_supplied_contexts(ArgVars, ArgTerms0,
        ArgContexts, Context, !Goal, MaybeThreshold, NumAdded,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    (
        ArgVars = [],
        NumAdded = 0
    ;
        ArgVars = [_ | _],
        !.Goal = hlds_goal(_, GoalInfo0),
        svar_goal_to_conj_list(!.Goal, Goals0, !SVarStore),
        substitute_state_var_mappings(ArgTerms0, ArgTerms, !VarSet,
            !SVarState, !Specs),
        do_insert_arg_unifications_with_supplied_contexts_loop(ArgVars,
            ArgTerms, ArgContexts, Context, Goals0, Goals,
            MaybeThreshold, 0, NumAdded, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        goal_info_set_context(Context, GoalInfo0, GoalInfo),
        conj_list_to_goal(Goals, GoalInfo, !:Goal)
    ).

:- pred do_insert_arg_unifications_with_supplied_contexts_loop(
    list(prog_var)::in, list(prog_term)::in, assoc_list(int, arg_context)::in,
    prog_context::in, list(hlds_goal)::in, list(hlds_goal)::out,
    maybe(int)::in, num_added_goals::in, num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_insert_arg_unifications_with_supplied_contexts_loop(Vars, Terms,
        ArgContexts, Context, !Goals, MaybeThreshold, !NumAdded,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    (
        Vars = [],
        Terms = [],
        ArgContexts = []
    ->
        true
    ;
        Vars = [Var | VarsTail],
        Terms = [Term | TermsTail],
        ArgContexts = [ArgNumber - ArgContext | ArgContextsTail]
    ->
        do_arg_unification(Var, Term, Context, ArgContext, ArgNumber,
            ArgUnifyConj, MaybeThreshold, ArgAdded, !SVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs),
        !:NumAdded = !.NumAdded + ArgAdded,
        do_insert_arg_unifications_with_supplied_contexts_loop(VarsTail,
            TermsTail, ArgContextsTail, Context, !Goals, MaybeThreshold,
            !NumAdded, !SVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs),
        !:Goals = ArgUnifyConj ++ !.Goals
    ;
        unexpected($module, $pred, "length mismatch")
    ).

:- pred do_append_arg_unifications(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in,
    hlds_goal::in, hlds_goal::out, maybe(int)::in, num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_append_arg_unifications(HeadVars, Args0, Context, ArgContext, !Goal,
        MaybeThreshold, NumAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    (
        HeadVars = [],
        NumAdded = 0
    ;
        HeadVars = [_ | _],
        !.Goal = hlds_goal(_, GoalInfo),
        svar_goal_to_conj_list(!.Goal, Goals0, !SVarStore),
        substitute_state_var_mappings(Args0, Args, !VarSet, !SVarState,
            !Specs),
        do_append_arg_unifications_loop(HeadVars, Args, Context, ArgContext,
            1, Goals0, Goals, MaybeThreshold, 0, NumAdded,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        conj_list_to_goal(Goals, GoalInfo, !:Goal)
    ).

:- pred do_append_arg_unifications_loop(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in, int::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    maybe(int)::in, num_added_goals::in, num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_append_arg_unifications_loop([], [_ | _], _, _, _, _, _, _, !NumAdded,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    unexpected($module, $pred, "length mismatch").
do_append_arg_unifications_loop([_ | _], [], _, _, _, _, _, _, !NumAdded,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    unexpected($module, $pred, "length mismatch").
do_append_arg_unifications_loop([], [], _, _, _, !GoalList, _, !NumAdded,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).
do_append_arg_unifications_loop([Var | Vars], [Arg | Args],
        Context, ArgContext, ArgNum, !GoalList, MaybeThreshold, !NumAdded,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    do_arg_unification(Var, Arg, Context, ArgContext, ArgNum, ArgUnifyConj,
        MaybeThreshold, ArgAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),
    !:NumAdded = !.NumAdded + ArgAdded,
    !:GoalList = !.GoalList ++ ArgUnifyConj,
    do_append_arg_unifications_loop(Vars, Args, Context, ArgContext,
        ArgNum + 1, !GoalList, MaybeThreshold, !NumAdded,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).

:- pred do_arg_unification(prog_var::in, prog_term::in,
    prog_context::in, arg_context::in, int::in, list(hlds_goal)::out,
    maybe(int)::in, num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_arg_unification(Var, Arg, Context, ArgContext, ArgNum, ArgUnifyConj,
        MaybeThreshold, NumAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    ( Arg = term.variable(Var, _) ->
        % Skip unifications of the form `X = X'.
        ArgUnifyConj = [],
        NumAdded = 0
    ;
        arg_context_to_unify_context(ArgContext, ArgNum, UnifyMainContext,
            UnifySubContext),
        do_unravel_unification(term.variable(Var, Context), Arg, Context,
            UnifyMainContext, UnifySubContext, purity_pure, Goal,
            MaybeThreshold, NumAdded, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs),
        svar_goal_to_conj_list(Goal, ArgUnifyConj, !SVarStore)
    ).

%-----------------------------------------------------------------------------%

:- pred do_unravel_unification(prog_term::in, prog_term::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, purity::in,
    hlds_goal::out, maybe(int)::in, num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_unravel_unification(LHS0, RHS0, Context, MainContext, SubContext, Purity,
        Goal, MaybeThreshold, NumAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    substitute_state_var_mapping(LHS0, LHS, !VarSet, !SVarState, !Specs),
    substitute_state_var_mapping(RHS0, RHS, !VarSet, !SVarState, !Specs),
    classify_unravel_unification(LHS, RHS, Context, MainContext, SubContext,
        Purity, Goal0, NumAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs),
    (
        MaybeThreshold = yes(Threshold),
        NumAdded > Threshold,
        LHS = term.variable(LHSVar, _),
        ground_term(RHS)
    ->
        Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
        Kind = from_ground_term_initial,
        goal_info_set_nonlocals(set_of_var.make_singleton(LHSVar),
            GoalInfo0, GoalInfo),
        ( GoalExpr0 = conj(plain_conj, Conjuncts0) ->
            mark_nonlocals_in_ground_term_construct(Conjuncts0, Conjuncts),
            SubGoalExpr = conj(plain_conj, Conjuncts),
            SubGoal = hlds_goal(SubGoalExpr, GoalInfo),
            GoalExpr = scope(from_ground_term(LHSVar, Kind), SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo)
        ;
            % This can happen if we unravel a large ground term that happens
            % to be a lambda expression; the conjunction will then be *inside*
            % the rhs_lambda_goal.
            Goal = Goal0
        )
    ;
        Goal = Goal0
    ).

:- pred mark_nonlocals_in_ground_term_construct(
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

mark_nonlocals_in_ground_term_construct([], []).
mark_nonlocals_in_ground_term_construct([Goal0 | Goals0], [Goal | Goals]) :-
    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    (
        GoalExpr = unify(LHSVar, RHS, _, _, _),
        RHS = rhs_functor(_, _, RHSVars)
    ->
        set_of_var.list_to_set([LHSVar | RHSVars], NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        unexpected($module, $pred, "wrong shape goal")
    ),
    mark_nonlocals_in_ground_term_construct(Goals0, Goals).

:- pred classify_unravel_unification(prog_term::in, prog_term::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    purity::in, hlds_goal::out, num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_unravel_unification(TermX, TermY, Context, MainContext, SubContext,
        Purity, Goal, NumAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    (
        % `X = Y' needs no unravelling.
        TermX = term.variable(X, _),
        TermY = term.variable(Y, _),
        make_atomic_unification(X, rhs_var(Y), Context, MainContext,
            SubContext, Purity, Goal, !QualInfo),
        NumAdded = 0
    ;
        TermX = term.variable(X, _),
        TermY = term.functor(F, Args, FunctorContext),
        unravel_var_functor_unification(X, F, Args, FunctorContext,
            Context, MainContext, SubContext, Purity, Goal, NumAdded,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs)
    ;
        TermX = term.functor(F, Args, FunctorContext),
        TermY = term.variable(Y, _),
        unravel_var_functor_unification(Y, F, Args, FunctorContext,
            Context, MainContext, SubContext, Purity, Goal, NumAdded,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs)
    ;
        % If we find a unification of the form `f1(...) = f2(...)',
        % then we replace it with `Tmp = f1(...), Tmp = f2(...)',
        % and then process it according to the rules above.
        % Note that we can't simplify it yet, because we might simplify
        % away type errors.
        TermX = term.functor(_, _, _),
        TermY = term.functor(_, _, _),
        varset.new_var(TmpVar, !VarSet),
        do_unravel_unification(term.variable(TmpVar, Context), TermX,
            Context, MainContext, SubContext, Purity, GoalX, no, NumAddedX,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        do_unravel_unification(term.variable(TmpVar, Context), TermY,
            Context, MainContext, SubContext, Purity, GoalY, no, NumAddedY,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        svar_goal_to_conj_list(GoalX, ConjListX, !SVarStore),
        svar_goal_to_conj_list(GoalY, ConjListY, !SVarStore),
        ConjList = ConjListX ++ ConjListY,
        goal_info_init(GoalInfo),
        conj_list_to_goal(ConjList, GoalInfo, Goal),
        NumAdded = NumAddedX + NumAddedY
    ).

    % Given an unification of the form
    %   X = f(ArgTerm1, ArgTerm2, ArgTerm3)
    % we replace it with
    %   X = f(NewVar1, NewVar2, NewVar3),
    %   NewVar1 = ArgTerm1,
    %   NewVar2 = ArgTerm2,
    %   NewVar3 = ArgTerm3.
    % In the trivial case `X = c', no unravelling occurs.
    %
    % XXX We could do better on the error messages for lambda expressions
    % and field extraction and update expressions.
    %
:- pred unravel_var_functor_unification(prog_var::in, term.const::in,
    list(prog_term)::in, term.context::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    purity::in, hlds_goal::out, num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

unravel_var_functor_unification(X, F, Args1, FunctorContext,
        Context, MainContext, SubContext, Purity, Goal, NumAdded,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    substitute_state_var_mappings(Args1, Args, !VarSet, !SVarState, !Specs),
    (
        F = term.atom(Atom),
        maybe_unravel_special_var_functor_unification(X, Atom, Args,
            FunctorContext, Context, MainContext, SubContext, Purity,
            GoalPrime, NumAddedPrime, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ->
        Goal = GoalPrime,
        NumAdded = NumAddedPrime
    ;
        % Handle higher-order pred and func expressions.
        RHS = term.functor(F, Args, FunctorContext),
        parse_rule_term(Context, RHS, HeadTerm0, GoalTerm1),
        term.coerce(HeadTerm0, HeadTerm1),
        parse_purity_annotation(HeadTerm1, LambdaPurity, HeadTerm),
        (
            parse_pred_expression(HeadTerm, Groundness0, EvalMethod0, Vars0,
                Modes0, Det0)
        ->
            PredOrFunc = pf_predicate,
            EvalMethod = EvalMethod0,
            Groundness = Groundness0,
            Vars1 = Vars0,
            Modes1 = Modes0,
            Det1 = Det0
        ;
            parse_func_expression(HeadTerm, Groundness, EvalMethod, Vars1,
                Modes1, Det1),
            PredOrFunc = pf_function
        )
    ->
        qualify_lambda_mode_list_if_not_opt_imported(Modes1, Modes, Context,
            !QualInfo, !Specs),
        Det = Det1,
        term.coerce(GoalTerm1, GoalTerm),
        ContextPieces = [words("Error:")],
        parse_goal(GoalTerm, ContextPieces, MaybeParsedGoal, !VarSet),
        (
            MaybeParsedGoal = ok1(ParsedGoal),
            build_lambda_expression(X, Purity, LambdaPurity, Groundness,
                PredOrFunc, EvalMethod, Vars1, Modes, Det, ParsedGoal,
                Context, MainContext, SubContext, Goal, NumAdded,
                !.SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs)
        ;
            MaybeParsedGoal = error1(ParsedGoalSpecs),
            !:Specs = ParsedGoalSpecs ++ !.Specs,
            NumAdded = 0,
            Goal = true_goal
        )
    ;
        % Handle the usual case.
        % XXX Why do we use Args1 instead of Args here?
        RHS = term.functor(F, Args1, FunctorContext),
        ( try_parse_sym_name_and_args(RHS, FunctorName, FunctorArgsPrime) ->
            FunctorArgs = FunctorArgsPrime,
            list.length(FunctorArgs, Arity),
            ConsId = cons(FunctorName, Arity, cons_id_dummy_type_ctor)
        ;
            % float, int or string constant
            %   - any errors will be caught by typechecking
            list.length(Args, Arity),
            ConsId = make_functor_cons_id(F, Arity),
            FunctorArgs = Args
        ),
        (
            FunctorArgs = [],
            make_atomic_unification(X, rhs_functor(ConsId, no, []),
                Context, MainContext, SubContext, Purity, Goal0, !QualInfo),
            NumAdded = 1,
            Goal0 = hlds_goal(GoalExpr, GoalInfo0),
            goal_info_set_purity(Purity, GoalInfo0, GoalInfo),
            % We could wrap a from_ground_term(X) scope around Goal,
            % but there would be no gain from doing so, whereas the
            % increase would lead to a slight increase in memory and time
            % requirements.
            Goal = hlds_goal(GoalExpr, GoalInfo)
        ;
            FunctorArgs = [_ | _],
            make_fresh_arg_vars(FunctorArgs, HeadVars, !VarSet, !SVarState,
                !Specs),
            make_atomic_unification(X, rhs_functor(ConsId, no, HeadVars),
                Context, MainContext, SubContext, Purity, Goal0, !QualInfo),
            MainFunctorAdded = 1,
            ArgContext = ac_functor(ConsId, MainContext, SubContext),
            % Should this be insert_... rather than append_...?
            % No, because that causes efficiency problems
            % with type-checking :-(
            % But for impure unifications, we need to do this, because
            % mode reordering can't reorder around the functor unification.
            (
                Purity = purity_pure,
                do_append_arg_unifications(HeadVars, FunctorArgs,
                    FunctorContext, ArgContext, Goal0, Goal, no, ArgAdded,
                    !SVarState, !SVarStore, !VarSet,
                    !ModuleInfo, !QualInfo, !Specs)
            ;
                ( Purity = purity_semipure
                ; Purity = purity_impure
                ),
                Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
                goal_info_set_purity(Purity, GoalInfo0, GoalInfo1),
                Goal1 = hlds_goal(GoalExpr0, GoalInfo1),
                do_insert_arg_unifications(HeadVars, FunctorArgs,
                    FunctorContext, ArgContext, Goal1, Goal, no, ArgAdded,
                    !SVarState, !SVarStore, !VarSet,
                    !ModuleInfo, !QualInfo, !Specs)
            ),
            NumAdded = MainFunctorAdded + ArgAdded
        )
    ).

    % See whether Atom indicates a term with special syntax.
    %
:- pred maybe_unravel_special_var_functor_unification(prog_var::in,
    string::in, list(prog_term)::in, term.context::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    purity::in, hlds_goal::out, num_added_goals::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is semidet.

maybe_unravel_special_var_functor_unification(X, Atom, Args,
        FunctorContext, Context, MainContext, SubContext, Purity, Goal,
        NumAdded, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs)  :-
    % Switch on Atom.
    % XXX instead of failing if Atom has the wrong number of arguments or
    % if the arguments have the wrong shape, we should generate an error
    % message.
    (
        % Handle explicit type qualification.
        ( Atom = "with_type"
        ; Atom = ":"
        ),
        Args = [RVal, DeclType0],

        require_det (
            % DeclType0 is a prog_term, but it is really a type,
            % so we coerce it to a generic term before parsing it.
            term.coerce(DeclType0, DeclType1),
            ContextPieces = [words("In explicit type qualification:")],
            varset.coerce(!.VarSet, GenericVarSet),
            parse_type(DeclType1, GenericVarSet, ContextPieces,
                DeclTypeResult),
            (
                DeclTypeResult = ok1(DeclType),
                varset.coerce(!.VarSet, DeclVarSet),
                process_type_qualification(X, DeclType, DeclVarSet,
                    Context, !ModuleInfo, !QualInfo, !Specs)
            ;
                DeclTypeResult = error1(DeclTypeSpecs),
                % The varset is a prog_varset even though it contains
                % the names of type variables in ErrorTerm, which is
                % a generic term.
                !:Specs = DeclTypeSpecs ++ !.Specs
            ),
            do_unravel_unification(term.variable(X, Context), RVal,
                Context, MainContext, SubContext, Purity, Goal, no,
                NumAdded, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs)
        )
    ;
        % Handle unification expressions.
        Atom = "@",
        Args = [LVal, RVal],

        require_det (
            do_unravel_unification(term.variable(X, Context), LVal, Context,
                MainContext, SubContext, Purity, GoalL, no, NumAddedL,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            do_unravel_unification(term.variable(X, Context), RVal, Context,
                MainContext, SubContext, Purity, GoalR, no, NumAddedR,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            NumAdded = NumAddedL + NumAddedR,
            goal_info_init(GoalInfo),
            svar_goal_to_conj_list(GoalL, ConjListL, !SVarStore),
            svar_goal_to_conj_list(GoalR, ConjListR, !SVarStore),
            ConjList = ConjListL ++ ConjListR,
            conj_list_to_goal(ConjList, GoalInfo, Goal)
        )
    ;
        % Handle if-then-else expressions.
        (
            Atom = "else",
            Args = [CondThenTerm, ElseTerm],
            CondThenTerm = term.functor(term.atom("if"),
                [term.functor(term.atom("then"), [CondTerm0, ThenTerm], _)], _)
        ;
            Atom = ";",
            Args = [CondThenTerm, ElseTerm],
            CondThenTerm = term.functor(term.atom("->"),
                [CondTerm0, ThenTerm], _)
        ),

        require_det (
            term.coerce(CondTerm0, CondTerm),
            ContextPieces = [words("Error:")],
            parse_some_vars_goal(CondTerm, ContextPieces, MaybeVarsCond,
                !VarSet),
            (
                MaybeVarsCond = ok3(Vars, StateVars, CondParseTree),
                BeforeSVarState = !.SVarState,
                svar_prepare_for_local_state_vars(Context, !.VarSet, StateVars,
                    BeforeSVarState, BeforeInsideSVarState, !Specs),
                map.init(EmptySubst),
                transform_goal_expr_context_to_goal(loc_inside_atomic_goal,
                    CondParseTree, EmptySubst, CondGoal, CondAdded,
                    BeforeInsideSVarState, AfterCondInsideSVarState,
                    !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),

                do_unravel_unification(term.variable(X, Context), ThenTerm,
                    Context, MainContext, SubContext, Purity, ThenGoal0, no,
                    ThenAdded,
                    AfterCondInsideSVarState, AfterThenInsideSVarState,
                    !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),

                svar_finish_local_state_vars(StateVars, BeforeSVarState,
                    AfterThenInsideSVarState, AfterThenSVarState),

                do_unravel_unification(term.variable(X, Context), ElseTerm,
                    Context, MainContext, SubContext, Purity, ElseGoal0, no,
                    ElseAdded, BeforeSVarState, AfterElseSVarState,
                    !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),

                svar_finish_if_then_else(loc_inside_atomic_goal, Context,
                    StateVars, ThenGoal0, ThenGoal, ElseGoal0, ElseGoal,
                    BeforeSVarState, AfterCondInsideSVarState,
                    AfterThenSVarState, AfterElseSVarState, AfterITESVarState,
                    !VarSet, !SVarStore, !Specs),
                !:SVarState = AfterITESVarState,

                NumAdded = CondAdded + ThenAdded + ElseAdded,
                GoalExpr = if_then_else(StateVars ++ Vars,
                    CondGoal, ThenGoal, ElseGoal),
                goal_info_init(Context, GoalInfo),
                Goal = hlds_goal(GoalExpr, GoalInfo)
            ;
                MaybeVarsCond = error3(VarsCondSpecs),
                !:Specs = VarsCondSpecs ++ !.Specs,
                NumAdded = 0,
                Goal = true_goal
            )
        )
    ;
        % Handle field extraction expressions.
        Atom = "^",
        Args = [InputTerm, FieldNameTerm],
        maybe_parse_field_list(FieldNameTerm, !.VarSet, FieldNames),

        require_det (
            make_fresh_arg_var(InputTerm, InputTermVar, [],
                !VarSet, !SVarState, !Specs),
            expand_get_field_function_call(Context, MainContext, SubContext,
                FieldNames, X, InputTermVar, Purity, Functor, _,
                Goal0, CallAdded, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),

            ArgContext = ac_functor(Functor, MainContext, SubContext),
            do_insert_arg_unifications([InputTermVar], [InputTerm],
                FunctorContext, ArgContext, Goal0, Goal, no, ArgAdded,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            NumAdded = CallAdded + ArgAdded
        )
    ;
        % Handle field update expressions.
        Atom = ":=",
        Args = [FieldDescrTerm, FieldValueTerm],
        FieldDescrTerm = term.functor(term.atom("^"),
            [InputTerm, FieldNameTerm], _),
        maybe_parse_field_list(FieldNameTerm, !.VarSet, FieldNames),

        require_det (
            make_fresh_arg_var(InputTerm, InputTermVar, [],
                !VarSet, !SVarState, !Specs),
            make_fresh_arg_var(FieldValueTerm, FieldValueVar,
                [InputTermVar], !VarSet, !SVarState, !Specs),

            expand_set_field_function_call(Context, MainContext, SubContext,
                FieldNames, FieldValueVar, InputTermVar, X,
                Functor, InnerFunctor - FieldSubContext, Goal0, CallAdded,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),

            TermArgContext = ac_functor(Functor, MainContext, SubContext),
            TermArgNumber = 1,
            FieldArgContext = ac_functor(InnerFunctor, MainContext,
                FieldSubContext),
            FieldArgNumber = 2,
            ArgContexts = [TermArgNumber - TermArgContext,
                FieldArgNumber - FieldArgContext],
            do_insert_arg_unifications_with_supplied_contexts(
                [InputTermVar, FieldValueVar], [InputTerm, FieldValueTerm],
                ArgContexts, Context, Goal0, Goal, no, ArgAdded,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            NumAdded = CallAdded + ArgAdded
        )
    ;
        % Handle higher-order dcg pred expressions. They have the same
        % semantics as higher-order pred expressions, but have two extra
        % arguments, and the goal is expanded as a DCG goal.
        Atom = "-->",
        Args = [PredTerm0, GoalTerm0],
        term.coerce(PredTerm0, PredTerm1),
        parse_purity_annotation(PredTerm1, DCGLambdaPurity, PredTerm),
        parse_dcg_pred_expression(PredTerm, Groundness, EvalMethod, Vars0,
            Modes0, Det),

        require_det (
            qualify_lambda_mode_list_if_not_opt_imported(Modes0, Modes,
                Context, !QualInfo, !Specs),
            term.coerce(GoalTerm0, GoalTerm),
            ContextPieces = [words("Error:")],
            parse_dcg_pred_goal(GoalTerm, ContextPieces, MaybeParsedGoal,
                DCG0, DCGn, !VarSet),
            (
                MaybeParsedGoal = ok1(ParsedGoal),
                Vars1 = Vars0 ++
                    [term.variable(DCG0, Context),
                    term.variable(DCGn, Context)],
                build_lambda_expression(X, Purity, DCGLambdaPurity,
                    Groundness, pf_predicate, EvalMethod, Vars1, Modes, Det,
                    ParsedGoal, Context, MainContext, SubContext,
                    Goal0, NumAdded, !.SVarState, !SVarStore, !VarSet,
                    !ModuleInfo, !QualInfo, !Specs),
                Goal0 = hlds_goal(GoalExpr, GoalInfo0),
                goal_info_set_purity(Purity, GoalInfo0, GoalInfo),
                Goal = hlds_goal(GoalExpr, GoalInfo)
            ;
                MaybeParsedGoal = error1(ParsedGoalSpecs),
                !:Specs = ParsedGoalSpecs ++ !.Specs,
                NumAdded = 0,
                Goal = true_goal
            )
        )
    ).

:- pred qualify_lambda_mode_list_if_not_opt_imported(
    list(mer_mode)::in, list(mer_mode)::out, prog_context::in,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_lambda_mode_list_if_not_opt_imported(Modes0, Modes, Context,
        !QualInfo, !Specs) :-
    % The modes in `.opt' files are already fully module qualified.
    qual_info_get_import_status(!.QualInfo, ImportStatus),
    ( ImportStatus \= status_opt_imported ->
        qual_info_get_mq_info(!.QualInfo, MQInfo0),
        qualify_lambda_mode_list(Modes0, Modes, Context, MQInfo0, MQInfo,
            !Specs),
        qual_info_set_mq_info(MQInfo, !QualInfo)
    ;
        Modes = Modes0
    ).

%-----------------------------------------------------------------------------%
%
% Code for building lambda expressions.
%

:- pred build_lambda_expression(prog_var::in, purity::in, purity::in,
    ho_groundness::in, pred_or_func::in, lambda_eval_method::in,
    list(prog_term)::in, list(mer_mode)::in, determinism::in, goal::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    hlds_goal::out, num_added_goals::out,
    svar_state::in, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_lambda_expression(X, UnificationPurity, LambdaPurity, Groundness,
        PredOrFunc, EvalMethod, Args0, Modes, Det, ParsedGoal,
        Context, MainContext, SubContext, Goal, NumAdded,
        OutsideSVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    % In the parse tree, the lambda arguments can be any terms, but in the HLDS
    % they must be distinct variables. So we introduce fresh variables
    % for the lambda arguments, and add appropriate unifications.
    %
    % For example, we convert from:
    %
    %       X = (func(f(A, B), c) = D :- Body )
    %
    % to:
    %
    %       X = (func(H1, H2) = H3 :-
    %           some [A, B] (
    %               H1 = f(A, B),
    %               H2 = c,
    %               Body,
    %               H3 = D
    %       )
    %
    % Note that the quantification is important here. That's why we need
    % to introduce the explicit `some [...]'. Variables in the argument
    % positions are lambda-quantified, so when we move them to the body,
    % we need to make them explicitly existentially quantified to avoid
    % capturing any variables of the same name that occur outside this scope.
    %
    % Also, note that any introduced unifications that construct the output
    % arguments for the lambda expression, need to occur *after* the body
    % of the lambda expression. This is in case the body of the lambda
    % expression is impure, in which case the mode analyser cannot reorder
    % the unifications; this results in a mode error.
    %
    % XXX The mode analyser *should* be able to reorder such unifications,
    % especially ones that the compiler introduced itself.
    %
    % For predicates, all variables occurring in the lambda arguments are
    % locally quantified to the lambda goal. For functions, we need to
    % be careful because variables in arguments should similarly be quantified,
    % but variables in the function return value term (and not in the
    % arguments) should *not* be locally quantified.

    ( illegal_state_var_func_result(PredOrFunc, Args0, StateVar) ->
        report_illegal_func_svar_result(Context, !.VarSet, StateVar, !Specs),
        Goal = true_goal,
        NumAdded = 0
    ; lambda_args_contain_bang_state_var(Args0, StateVar) ->
        report_illegal_bang_svar_lambda_arg(Context, !.VarSet, StateVar,
            !Specs),
        Goal = true_goal,
        NumAdded = 0
    ;
        some [!SVarState] (
            svar_prepare_for_lambda_head(Context, Args0, Args, FinalSVarMap,
                OutsideSVarState, !:SVarState, !VarSet, !Specs),
            InitialSVarState = !.SVarState,

            % Create fresh variables, transform the goal to HLDS, and
            % add unifications with the fresh variables. We use varset.new_vars
            % rather than make_fresh_arg_vars, since for functions we need
            % to ensure that the variable corresponding to the function result
            % term is a new variable, to avoid the function result term
            % becoming lambda-quantified.

            list.length(Args, NumArgs),
            varset.new_vars(NumArgs, LambdaVars, !VarSet),

            % Partition the arguments (and their corresponding lambda vars)
            % into two sets: those that are not output, i.e. input and unused,
            % and those that are output.
            (
                partition_args_and_lambda_vars(!.ModuleInfo, Args, LambdaVars,
                    Modes, NonOutputArgs0, OutputArgs0, NonOutputLambdaVars0,
                    OutputLambdaVars0)
            ->
                NonOutputArgs       = NonOutputArgs0,
                OutputArgs          = OutputArgs0,
                NonOutputLambdaVars = NonOutputLambdaVars0,
                OutputLambdaVars    = OutputLambdaVars0
            ;
                unexpected($module, $pred, "mismatched lists")
            ),

            map.init(Substitution),
            ArgContext = ac_head(PredOrFunc, NumArgs),

            % Create the unifications that need to come before the body of the
            % lambda expression; those corresponding to args whose mode is
            % input or unused.
            HeadBefore0 = true_goal,
            insert_arg_unifications(NonOutputLambdaVars, NonOutputArgs,
                Context, ArgContext, HeadBefore0, HeadBefore, NonOutputAdded,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),

            transform_goal_expr_context_to_goal(loc_whole_goal, ParsedGoal,
                Substitution, Body, BodyAdded, !SVarState, !SVarStore,
                !VarSet, !ModuleInfo, !QualInfo, !Specs),

            % Create the unifications that need to come after the body of the
            % lambda expression; those corresponding to args whose mode is
            % output.
            HeadAfter0 = true_goal,
            insert_arg_unifications(OutputLambdaVars, OutputArgs,
                Context, ArgContext, HeadAfter0, HeadAfter, OutputAdded,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),

            NumAdded = NonOutputAdded + BodyAdded + OutputAdded,

            trace [compiletime(flag("debug-statevar-lambda")), io(!IO)] (
                io.write_string("\nLAMBDA EXPRESSION\n", !IO),
                io.write_string("args before:\n", !IO),
                io.write_list(Args0, "\n", io.write, !IO),
                io.nl(!IO),
                io.write_string("args after:\n", !IO),
                io.write_list(Args, "\n", io.write, !IO),
                io.nl(!IO),
                io.write_string("lambda arg vars:\n", !IO),
                io.write(LambdaVars, !IO),
                io.nl(!IO),
                io.write_string("lambda arg unifies before:\n", !IO),
                dump_goal(!.ModuleInfo, !.VarSet, HeadBefore, !IO),
                io.nl(!IO),
                io.write_string("lambda body:\n", !IO),
                dump_goal(!.ModuleInfo, !.VarSet, Body, !IO),
                io.nl(!IO),
                io.write_string("lambda arg unifies after:\n", !IO),
                dump_goal(!.ModuleInfo, !.VarSet, HeadAfter, !IO),
                io.nl(!IO),
                some [FinalSVarList] (
                    map.to_assoc_list(FinalSVarMap, FinalSVarList),
                    io.write_string("FinalSVarMap:\n", !IO),
                    io.write(FinalSVarList, !IO),
                    io.nl(!IO)
                )
            ),

            % Fix up any state variable unifications.
            FinalSVarState = !.SVarState,
            svar_finish_lambda_body(Context, FinalSVarMap,
                [HeadBefore, Body, HeadAfter], HLDS_Goal0,
                InitialSVarState, FinalSVarState, !SVarStore),

            % Figure out which variables we need to explicitly existentially
            % quantify.
            (
                PredOrFunc = pf_predicate,
                QuantifiedArgs = Args
            ;
                PredOrFunc = pf_function,
                pred_args_to_func_args(Args, QuantifiedArgs, _ReturnValTerm)
            ),
            term.vars_list(QuantifiedArgs, QuantifiedVars0),
            list.sort_and_remove_dups(QuantifiedVars0, QuantifiedVars),

            goal_info_init(Context, GoalInfo),
            HLDS_GoalExpr = scope(exist_quant(QuantifiedVars), HLDS_Goal0),
            HLDS_Goal = hlds_goal(HLDS_GoalExpr, GoalInfo),

            % We set the lambda nonlocals here to anything that could
            % possibly be nonlocal. Quantification will reduce this down
            % to the proper set of nonlocal arguments.
            some [!LambdaGoalVars] (
                goal_util.goal_vars(HLDS_Goal, !:LambdaGoalVars),
                set_of_var.delete_list(LambdaVars, !LambdaGoalVars),
                set_of_var.delete_list(QuantifiedVars, !LambdaGoalVars),
                LambdaNonLocals = set_of_var.to_sorted_list(!.LambdaGoalVars)
            ),

            LambdaRHS = rhs_lambda_goal(LambdaPurity, Groundness, PredOrFunc,
                EvalMethod, LambdaNonLocals, LambdaVars, Modes, Det, HLDS_Goal),
            make_atomic_unification(X, LambdaRHS, Context, MainContext,
                SubContext, UnificationPurity, Goal, !QualInfo)
        )
    ).

    % Partition the lists of arguments and variables into lists
    % of non-output and output arguments and variables.
    %
 :- pred partition_args_and_lambda_vars(module_info::in,
    list(prog_term)::in, list(prog_var)::in, list(mer_mode)::in,
    list(prog_term)::out, list(prog_term)::out,
    list(prog_var)::out, list(prog_var)::out) is semidet.

partition_args_and_lambda_vars(_, [], [], [], [], [], [], []).
partition_args_and_lambda_vars(ModuleInfo, [Arg | Args],
        [LambdaVar | LambdaVars], [Mode | Modes], InputArgs, OutputArgs,
        InputLambdaVars, OutputLambdaVars) :-
    partition_args_and_lambda_vars(ModuleInfo, Args, LambdaVars, Modes,
        InputArgs0, OutputArgs0, InputLambdaVars0, OutputLambdaVars0),

    % Calling mode_is_output/2 directly will cause the compiler to abort
    % if the mode is undefined, so we first check for this. If the mode
    % is undefined, it doesn't really matter which partitions we place
    % the arguments/lambda vars into because mode analysis will fail
    % anyway.

    ( mode_is_undefined(ModuleInfo, Mode) ->
        InputArgs        = [Arg | InputArgs0],
        OutputArgs       = OutputArgs0,
        InputLambdaVars  = [LambdaVar | InputLambdaVars0],
        OutputLambdaVars = OutputLambdaVars0
    ; mode_is_output(ModuleInfo, Mode) ->
        InputArgs        = InputArgs0,
        OutputArgs       = [Arg | OutputArgs0],
        InputLambdaVars  = InputLambdaVars0,
        OutputLambdaVars = [LambdaVar | OutputLambdaVars0]
    ;
        InputArgs        = [Arg | InputArgs0],
        OutputArgs       = OutputArgs0,
        InputLambdaVars  = [LambdaVar | InputLambdaVars0],
        OutputLambdaVars = OutputLambdaVars0
    ).

%-----------------------------------------------------------------------------%

:- pred ground_term(term(T)::in) is semidet.

ground_term(term.functor(_, Terms, _)) :-
    ground_terms(Terms).

:- pred ground_terms(list(term(T))::in) is semidet.

ground_terms([]).
ground_terms([Term | Terms]) :-
    ground_term(Term),
    ground_terms(Terms).

:- pred arg_context_to_unify_context(arg_context::in, int::in,
    unify_main_context::out, unify_sub_contexts::out) is det.

arg_context_to_unify_context(ArgContext, ArgNum, MainContext, SubContexts) :-
    (
        ArgContext = ac_head(PredOrFunc, Arity),
        ( PredOrFunc = pf_function, ArgNum = Arity ->
            % It's the function result term in the head.
            MainContext = umc_head_result
        ;
            % It's a head argument.
            MainContext = umc_head(ArgNum)
        ),
        SubContexts = []
    ;
        ArgContext = ac_call(PredId),
        MainContext = umc_call(PredId, ArgNum),
        SubContexts = []
    ;
        ArgContext = ac_functor(ConsId, MainContext, SubContexts0),
        SubContext = unify_sub_context(ConsId, ArgNum),
        SubContexts = [SubContext | SubContexts0]
    ).

%-----------------------------------------------------------------------------%

make_fresh_arg_vars(Args, Vars, !VarSet, !SVarState, !Specs) :-
    % For efficiency, we construct `Vars' backwards and then reverse it
    % to get the correct order.
    make_fresh_arg_vars_loop(Args, [], Vars1, !VarSet, !SVarState, !Specs),
    list.reverse(Vars1, Vars).

:- pred make_fresh_arg_vars_loop(list(prog_term)::in, list(prog_var)::in,
    list(prog_var)::out, prog_varset::in,prog_varset::out,
    svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

make_fresh_arg_vars_loop([], !RevVars, !VarSet, !SVarState, !Specs).
make_fresh_arg_vars_loop([Arg | Args], !RevVars, !VarSet, !SVarState,
        !Specs) :-
    make_fresh_arg_var(Arg, Var, !.RevVars, !VarSet, !SVarState, !Specs),
    !:RevVars = [Var | !.RevVars],
    make_fresh_arg_vars_loop(Args, !RevVars, !VarSet, !SVarState, !Specs).

make_fresh_arg_var(Arg0, Var, Vars0, !VarSet, !SVarState, !Specs) :-
    substitute_state_var_mapping(Arg0, Arg, !VarSet, !SVarState, !Specs),
    (
        Arg = term.variable(ArgVar, _),
        \+ list.member(ArgVar, Vars0)
    ->
        Var = ArgVar
    ;
        varset.new_var(Var, !VarSet)
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.superhomogeneous.
%-----------------------------------------------------------------------------%
