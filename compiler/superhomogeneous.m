%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: superhomogeneous.m.
% Main author: fjh.

% This module performs the conversion of clause bodies
% to superhomogeneous form.

%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.superhomogeneous.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.make_hlds.state_var.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type arg_context
    --->    head(pred_or_func, arity)
            % the arguments in the head of the clause

    ;       call(call_id)
            % the arguments in a call to a predicate

    ;       functor(            % the arguments in a functor
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
    num_added_goals::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

:- pred insert_arg_unifications_with_supplied_contexts(list(prog_var)::in,
    list(prog_term)::in, assoc_list(int, arg_context)::in, prog_context::in,
    hlds_goal::in, hlds_goal::out, num_added_goals::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

    % append_arg_unifications is the same as insert_arg_unifications,
    % except that the unifications are added after the goal rather
    % than before the goal.
    %
:- pred append_arg_unifications(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in, hlds_goal::in, hlds_goal::out,
    num_added_goals::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

:- pred unravel_unification(prog_term::in, prog_term::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, purity::in,
    hlds_goal::out, num_added_goals::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

    % make_fresh_arg_vars(Args, VarSet0, Vars, VarSet, !SInfo, !IO):
    %
    % `Vars' is a list of distinct variables corresponding to the terms
    % in `Args'. For each term in `Args', if the term is a variable V
    % which is distinct from the variables already produced, then the
    % corresponding variable in `Vars' is just V, otherwise a fresh variable
    % is allocated from `VarSet0'.  `VarSet' is the varset resulting after
    % all the necessary variables have been allocated. !SInfo and !IO
    % are required to handle state variables.
    %
:- pred make_fresh_arg_vars(list(prog_term)::in, list(prog_var)::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

:- pred make_fresh_arg_var(prog_term::in, prog_var::out, list(prog_var)::in,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module check_hlds.purity.
:- import_module hlds.goal_util.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.field_access.
:- import_module hlds.make_hlds.qual_info.
:- import_module libs.compiler_util.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_io_dcg.
:- import_module parse_tree.prog_io_goal.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module svvarset.
:- import_module svset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- func from_ground_term_scope_threshold = int.

from_ground_term_scope_threshold = 15.

insert_arg_unifications(HeadVars, Args0, Context, ArgContext, !Goal, NumAdded,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    do_insert_arg_unifications(HeadVars, Args0, Context, ArgContext, !Goal,
        yes(from_ground_term_scope_threshold), NumAdded,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO).

insert_arg_unifications_with_supplied_contexts(ArgVars, ArgTerms0,
        ArgContexts, Context, !Goal, NumAdded, !VarSet, !ModuleInfo, !QualInfo,
        !SInfo, !IO) :-
    do_insert_arg_unifications_with_supplied_contexts(ArgVars, ArgTerms0,
        ArgContexts, Context,
        !Goal, yes(from_ground_term_scope_threshold), NumAdded,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO).

append_arg_unifications(HeadVars, Args0, Context, ArgContext,
        !Goal, NumAdded, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    do_append_arg_unifications(HeadVars, Args0, Context, ArgContext, !Goal,
        yes(from_ground_term_scope_threshold), NumAdded,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO).

unravel_unification(LHS0, RHS0, Context, MainContext, SubContext, Purity,
        Goal, NumAdded, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    do_unravel_unification(LHS0, RHS0, Context, MainContext, SubContext,
        Purity, Goal, yes(from_ground_term_scope_threshold), NumAdded,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO).

%-----------------------------------------------------------------------------%

:- pred do_insert_arg_unifications(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in,
    hlds_goal::in, hlds_goal::out, maybe(int)::in, num_added_goals::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

do_insert_arg_unifications(HeadVars, Args0, Context, ArgContext,
        !Goal, MaybeThreshold, NumAdded, !VarSet, !ModuleInfo, !QualInfo,
        !SInfo, !IO) :-
    (
        HeadVars = [],
        NumAdded = 0
    ;
        HeadVars = [_ | _],
        !.Goal = _ - GoalInfo0,
        goal_to_conj_list(!.Goal, Goals0),
        substitute_state_var_mappings(Args0, Args, !VarSet, !SInfo, !IO),
        do_insert_arg_unifications_2(HeadVars, Args, Context, ArgContext,
            0, Goals0, Goals, MaybeThreshold, 0, NumAdded, !VarSet, !ModuleInfo,
            !QualInfo, !SInfo, !IO),
        goal_info_set_context(Context, GoalInfo0, GoalInfo),
        conj_list_to_goal(Goals, GoalInfo, !:Goal)
    ).

:- pred do_insert_arg_unifications_2(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in, int::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    maybe(int)::in, num_added_goals::in, num_added_goals::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

do_insert_arg_unifications_2([], [_ | _], _, _, _, _, _, _, !NumAdded, !VarSet,
        !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    unexpected(this_file, "do_insert_arg_unifications_2: length mismatch").
do_insert_arg_unifications_2([_ | _], [], _, _, _, _, _, _, !NumAdded, !VarSet,
        !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    unexpected(this_file, "do_insert_arg_unifications_2: length mismatch").
do_insert_arg_unifications_2([], [], _, _, _, !Goals, _, !NumAdded, !VarSet,
        !ModuleInfo, !QualInfo, !SInfo, !IO).
do_insert_arg_unifications_2([Var | Vars], [Arg | Args], Context, ArgContext,
        N0, !Goals, MaybeThreshold, !NumAdded, !VarSet, !ModuleInfo, !QualInfo,
        !SInfo, !IO) :-
    N1 = N0 + 1,
    do_insert_arg_unification(Var, Arg, Context, ArgContext, N1, ArgUnifyConj,
        MaybeThreshold, ArgAdded, !VarSet, !ModuleInfo, !QualInfo,
        !SInfo, !IO),
    !:NumAdded = !.NumAdded + ArgAdded,
    (
        ArgUnifyConj = [],
        % Allow the recursive call to be tail recursive.
        do_insert_arg_unifications_2(Vars, Args, Context, ArgContext, N1,
            !Goals, MaybeThreshold, !NumAdded, !VarSet, !ModuleInfo,
            !QualInfo, !SInfo, !IO)
    ;
        ArgUnifyConj = [_ | _],
        do_insert_arg_unifications_2(Vars, Args, Context, ArgContext, N1,
            !Goals, MaybeThreshold, !NumAdded, !VarSet, !ModuleInfo,
            !QualInfo, !SInfo, !IO),
        list.append(ArgUnifyConj, !.Goals, !:Goals)
    ).

:- pred do_insert_arg_unifications_with_supplied_contexts(list(prog_var)::in,
    list(prog_term)::in, assoc_list(int, arg_context)::in, prog_context::in,
    hlds_goal::in, hlds_goal::out, maybe(int)::in, num_added_goals::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

do_insert_arg_unifications_with_supplied_contexts(ArgVars, ArgTerms0,
        ArgContexts, Context, !Goal, MaybeThreshold, NumAdded,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    (
        ArgVars = [],
        NumAdded = 0
    ;
        ArgVars = [_ | _],
        !.Goal = _ - GoalInfo0,
        goal_to_conj_list(!.Goal, GoalList0),
        substitute_state_var_mappings(ArgTerms0, ArgTerms, !VarSet, !SInfo,
            !IO),
        do_insert_arg_unifications_with_supplied_contexts_2(ArgVars, ArgTerms,
            ArgContexts, Context, GoalList0, GoalList, MaybeThreshold,
            0, NumAdded, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        goal_info_set_context(Context, GoalInfo0, GoalInfo),
        conj_list_to_goal(GoalList, GoalInfo, !:Goal)
    ).

:- pred do_insert_arg_unifications_with_supplied_contexts_2(list(prog_var)::in,
    list(prog_term)::in, assoc_list(int, arg_context)::in, prog_context::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    maybe(int)::in, num_added_goals::in, num_added_goals::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

do_insert_arg_unifications_with_supplied_contexts_2(Vars, Terms, ArgContexts,
        Context, !Goals, MaybeThreshold, !NumAdded, !VarSet, !ModuleInfo,
        !QualInfo, !SInfo, !IO) :-
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
        do_insert_arg_unification(Var, Term, Context, ArgContext, ArgNumber,
            UnifyConj, MaybeThreshold, ArgAdded, !VarSet, !ModuleInfo,
            !QualInfo, !SInfo, !IO),
        !:NumAdded = !.NumAdded + ArgAdded,
        do_insert_arg_unifications_with_supplied_contexts_2(VarsTail,
            TermsTail, ArgContextsTail, Context, !Goals, MaybeThreshold,
            !NumAdded, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        list.append(UnifyConj, !.Goals, !:Goals)
    ;
        unexpected(this_file, "insert_arg_unifications_with_supplied_contexts")
    ).

:- pred do_insert_arg_unification(prog_var::in, prog_term::in, prog_context::in,
    arg_context::in, int::in, list(hlds_goal)::out, maybe(int)::in,
    num_added_goals::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

do_insert_arg_unification(Var, Arg, Context, ArgContext, N1, ArgUnifyConj,
        MaybeThreshold, NumAdded, !VarSet, !ModuleInfo, !QualInfo,
        !SInfo, !IO) :-
    ( Arg = term.variable(Var) ->
        % Skip unifications of the form `X = X'
        ArgUnifyConj = [],
        NumAdded = 0
    ;
        arg_context_to_unify_context(ArgContext, N1, UnifyMainContext,
            UnifySubContext),
        do_unravel_unification(term.variable(Var), Arg, Context,
            UnifyMainContext, UnifySubContext, purity_pure, Goal,
            MaybeThreshold, NumAdded, !VarSet, !ModuleInfo, !QualInfo,
            !SInfo, !IO),
        goal_to_conj_list(Goal, ArgUnifyConj)
    ).

:- pred do_append_arg_unifications(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in,
    hlds_goal::in, hlds_goal::out, maybe(int)::in, num_added_goals::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

do_append_arg_unifications(HeadVars, Args0, Context, ArgContext, !Goal,
        MaybeThreshold, NumAdded, !VarSet, !ModuleInfo, !QualInfo,
        !SInfo, !IO) :-
    (
        HeadVars = [],
        NumAdded = 0
    ;
        HeadVars = [_ | _],
        !.Goal = _ - GoalInfo,
        goal_to_conj_list(!.Goal, GoalList0),
        substitute_state_var_mappings(Args0, Args, !VarSet, !SInfo, !IO),
        do_append_arg_unifications_2(HeadVars, Args, Context, ArgContext,
            0, GoalList0, GoalList, MaybeThreshold, 0, NumAdded,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        conj_list_to_goal(GoalList, GoalInfo, !:Goal)
    ).

:- pred do_append_arg_unifications_2(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in, int::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    maybe(int)::in, num_added_goals::in, num_added_goals::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

do_append_arg_unifications_2([], [_ | _], _, _, _, _, _, _, !NumAdded,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    unexpected(this_file, "do_append_arg_unifications_2: length mismatch").
do_append_arg_unifications_2([_ | _], [], _, _, _, _, _, _, !NumAdded,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    unexpected(this_file, "do_append_arg_unifications_2: length mismatch").
do_append_arg_unifications_2([], [], _, _, _, !GoalList, _, !NumAdded,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO).
do_append_arg_unifications_2([Var | Vars], [Arg | Args], Context, ArgContext,
        N0, !GoalList, MaybeThreshold, !NumAdded, !VarSet, !ModuleInfo,
        !QualInfo, !SInfo, !IO) :-
    N1 = N0 + 1,
    do_append_arg_unification(Var, Arg, Context, ArgContext, N1, ConjList,
        MaybeThreshold, ArgAdded, !VarSet, !ModuleInfo, !QualInfo,
        !SInfo, !IO),
    !:NumAdded = !.NumAdded + ArgAdded,
    list.append(!.GoalList, ConjList, !:GoalList),
    do_append_arg_unifications_2(Vars, Args, Context, ArgContext, N1,
        !GoalList, MaybeThreshold, !NumAdded, !VarSet, !ModuleInfo,
        !QualInfo, !SInfo, !IO).

:- pred do_append_arg_unification(prog_var::in, prog_term::in,
    prog_context::in, arg_context::in, int::in, list(hlds_goal)::out,
    maybe(int)::in, num_added_goals::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

do_append_arg_unification(Var, Arg, Context, ArgContext, N1, ConjList,
        MaybeThreshold, NumAdded, !VarSet, !ModuleInfo, !QualInfo,
        !SInfo, !IO) :-
    ( Arg = term.variable(Var) ->
        % Skip unifications of the form `X = X'.
        ConjList = [],
        NumAdded = 0
    ;
        arg_context_to_unify_context(ArgContext, N1, UnifyMainContext,
            UnifySubContext),
        do_unravel_unification(term.variable(Var), Arg, Context,
            UnifyMainContext, UnifySubContext, purity_pure, Goal,
            MaybeThreshold, NumAdded, !VarSet, !ModuleInfo, !QualInfo,
            !SInfo, !IO),
        goal_to_conj_list(Goal, ConjList)
    ).

%-----------------------------------------------------------------------------%

:- pred do_unravel_unification(prog_term::in, prog_term::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, purity::in,
    hlds_goal::out, maybe(int)::in, num_added_goals::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

do_unravel_unification(LHS0, RHS0, Context, MainContext, SubContext, Purity,
        Goal, MaybeThreshold, NumAdded, !VarSet, !ModuleInfo, !QualInfo,
        !SInfo, !IO) :-
    substitute_state_var_mapping(LHS0, LHS, !VarSet, !SInfo, !IO),
    substitute_state_var_mapping(RHS0, RHS, !VarSet, !SInfo, !IO),
    classify_unravel_unification(LHS, RHS, Context, MainContext, SubContext,
        Purity, Goal0, NumAdded, !VarSet, !ModuleInfo, !QualInfo,
        !SInfo, !IO),
    (
        MaybeThreshold = yes(Threshold),
        NumAdded > Threshold,
        LHS = term.variable(X),
        ground_term(RHS)
    ->
        Goal0 = _ - GoalInfo,
        Goal = scope(from_ground_term(X), Goal0) - GoalInfo
    ;
        Goal = Goal0
    ).

:- pred classify_unravel_unification(prog_term::in, prog_term::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    purity::in, hlds_goal::out, num_added_goals::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

classify_unravel_unification(TermX, TermY, Context, MainContext, SubContext,
        Purity, Goal, NumAdded, !VarSet, !ModuleInfo, !QualInfo, !SInfo,
        !IO) :-
    (
        % `X = Y' needs no unravelling.
        TermX = term.variable(X),
        TermY = term.variable(Y),
        make_atomic_unification(X, var(Y), Context, MainContext, SubContext,
            Purity, Goal, !QualInfo),
        NumAdded = 0
    ;
        TermX = term.variable(X),
        TermY = term.functor(F, Args, FunctorContext),
        unravel_var_functor_unification(X, F, Args, FunctorContext,
            Context, MainContext, SubContext, Purity, Goal, NumAdded,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO)
    ;
        TermX = term.functor(F, Args, FunctorContext),
        TermY = term.variable(Y),
        unravel_var_functor_unification(Y, F, Args, FunctorContext,
            Context, MainContext, SubContext, Purity, Goal, NumAdded,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO)
    ;
        % If we find a unification of the form `f1(...) = f2(...)',
        % then we replace it with `Tmp = f1(...), Tmp = f2(...)',
        % and then process it according to the rules above.
        % Note that we can't simplify it yet, because we might simplify
        % away type errors.
        TermX = term.functor(_, _, _),
        TermY = term.functor(_, _, _),
        varset.new_var(!.VarSet, TmpVar, !:VarSet),
        do_unravel_unification(term.variable(TmpVar), TermX,
            Context, MainContext, SubContext, Purity, GoalX, no, NumAddedX,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        do_unravel_unification(term.variable(TmpVar), TermY,
            Context, MainContext, SubContext, Purity, GoalY, no, NumAddedY,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        goal_to_conj_list(GoalX, ConjListX),
        goal_to_conj_list(GoalY, ConjListY),
        ConjList = ConjListX ++ ConjListY,
        goal_info_init(GoalInfo),
        conj_list_to_goal(ConjList, GoalInfo, Goal),
        NumAdded = NumAddedX + NumAddedY
    ).

    % Given an unification of the form
    %   X = f(A1, A2, A3)
    % we replace it with
    %   X = f(NewVar1, NewVar2, NewVar3),
    %   NewVar1 = A1,
    %   NewVar2 = A2,
    %   NewVar3 = A3.
    % In the trivial case `X = c', no unravelling occurs.
    %
    % XXX We could do better on the error messages for lambda expressions
    % and field extraction and update expressions.
    %
:- pred unravel_var_functor_unification(prog_var::in, term.const::in,
    list(prog_term)::in, term.context::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    purity::in, hlds_goal::out, num_added_goals::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

unravel_var_functor_unification(X, F, Args1, FunctorContext,
        Context, MainContext, SubContext, Purity, Goal, NumAdded,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    substitute_state_var_mappings(Args1, Args, !VarSet, !SInfo, !IO),
    (
        % Handle explicit type qualification.
        (
            F = term.atom("with_type")
        ;
            F = term.atom(":")
        ),
        Args = [RVal, DeclType0]
    ->
        % DeclType0 is a prog_term, but it is really a type so we coerce it
        % to a generic term before parsing it.
        term.coerce(DeclType0, DeclType1),
        parse_type(DeclType1, DeclTypeResult),
        (
            DeclTypeResult = ok(DeclType),
            varset.coerce(!.VarSet, DeclVarSet),
            process_type_qualification(X, DeclType, DeclVarSet,
                Context, !ModuleInfo, !QualInfo, !IO)
        ;
            DeclTypeResult = error(Msg, ErrorTerm),
            % The varset is a prog_varset even though it contains the names
            % of type variables in ErrorTerm, which is a generic term.
            GenericVarSet = varset.coerce(!.VarSet),
            TermStr = mercury_term_to_string(ErrorTerm, GenericVarSet, no),
            Pieces = [words("In explicit type qualification:"),
                    words(Msg),
                    suffix(":"),
                    fixed("`" ++ TermStr ++ "'.")],
            write_error_pieces(Context, 0, Pieces, !IO),
            io.set_exit_status(1, !IO)
        ),
        do_unravel_unification(term.variable(X), RVal, Context, MainContext,
            SubContext, Purity, Goal, no, NumAdded, !VarSet, !ModuleInfo,
            !QualInfo, !SInfo, !IO)
    ;
        % Handle unification expressions.
        F = term.atom("@"),
        Args = [LVal, RVal]
    ->
        do_unravel_unification(term.variable(X), LVal, Context,
            MainContext, SubContext, Purity, Goal1, no, NumAdded1,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        do_unravel_unification(term.variable(X), RVal, Context,
            MainContext, SubContext, Purity, Goal2, no, NumAdded2,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        NumAdded = NumAdded1 + NumAdded2,
        goal_info_init(GoalInfo),
        goal_to_conj_list(Goal1, ConjList1),
        goal_to_conj_list(Goal2, ConjList2),
        list.append(ConjList1, ConjList2, ConjList),
        conj_list_to_goal(ConjList, GoalInfo, Goal)
    ;
        % Handle higher-order pred and func expressions.
        % XXX Why do we use Arg1 instead of Args here?
        RHS = term.functor(F, Args1, FunctorContext),
        parse_rule_term(Context, RHS, HeadTerm0, GoalTerm1),
        term.coerce(HeadTerm0, HeadTerm1),
        parse_purity_annotation(HeadTerm1, LambdaPurity, HeadTerm),
        ( parse_pred_expression(HeadTerm, EvalMethod0, Vars0, Modes0, Det0) ->
            PredOrFunc = predicate,
            EvalMethod = EvalMethod0,
            Vars1 = Vars0,
            Modes1 = Modes0,
            Det1 = Det0
        ;
            parse_func_expression(HeadTerm, EvalMethod, Vars1, Modes1, Det1),
            PredOrFunc = function
        )
    ->
        qualify_lambda_mode_list_if_not_opt_imported(Modes1, Modes, Context,
            !QualInfo, !IO),
        Det = Det1,
        term.coerce(GoalTerm1, GoalTerm),
        parse_goal(GoalTerm, ParsedGoal, !VarSet),
        build_lambda_expression(X, Purity, LambdaPurity, PredOrFunc,
            EvalMethod, Vars1, Modes, Det, ParsedGoal, Context, MainContext,
            SubContext, Goal, NumAdded, !VarSet, !ModuleInfo, !QualInfo,
            !.SInfo, !IO)
    ;
        % Handle higher-order dcg pred expressions. They have the same
        % semantics as higher-order pred expressions, but have two extra
        % arguments, and the goal is expanded as a DCG goal.
        F = term.atom("-->"),
        Args = [PredTerm0, GoalTerm0],
        term.coerce(PredTerm0, PredTerm1),
        parse_purity_annotation(PredTerm1, DCGLambdaPurity, PredTerm),
        parse_dcg_pred_expression(PredTerm, EvalMethod, Vars0, Modes0, Det)
    ->
        qualify_lambda_mode_list_if_not_opt_imported(Modes0, Modes, Context,
            !QualInfo, !IO),
        term.coerce(GoalTerm0, GoalTerm),
        parse_dcg_pred_goal(GoalTerm, ParsedGoal, DCG0, DCGn, !VarSet),
        Vars1 = Vars0 ++ [term.variable(DCG0), term.variable(DCGn)],
        build_lambda_expression(X, Purity, DCGLambdaPurity, predicate,
            EvalMethod, Vars1, Modes, Det, ParsedGoal, Context, MainContext,
            SubContext, Goal0, NumAdded, !VarSet, !ModuleInfo, !QualInfo,
            !.SInfo, !IO),
        Goal0 = GoalExpr - GoalInfo0,
        add_goal_info_purity_feature(Purity, GoalInfo0, GoalInfo),
        Goal = GoalExpr - GoalInfo
    ;
        % Handle if-then-else expressions
        (
            F = term.atom("else"),
            Args = [CondThenTerm, ElseTerm],
            CondThenTerm = term.functor(term.atom("if"),
                [term.functor(term.atom("then"), [CondTerm0, ThenTerm], _)], _)
        ;
            F = term.atom(";"),
            Args = [CondThenTerm, ElseTerm],
            CondThenTerm = term.functor(term.atom("->"),
                [CondTerm0, ThenTerm], _)
        ),
        term.coerce(CondTerm0, CondTerm),
        parse_some_vars_goal(CondTerm, Vars, StateVars, CondParseTree, !VarSet)
    ->
        BeforeSInfo = !.SInfo,
        prepare_for_if_then_else_expr(StateVars, !VarSet, !SInfo),

        map.init(EmptySubst),
        transform_goal(CondParseTree, EmptySubst, CondGoal, CondAdded, !VarSet,
            !ModuleInfo, !QualInfo, !SInfo, !IO),

        finish_if_then_else_expr_condition(BeforeSInfo, !SInfo),

        do_unravel_unification(term.variable(X), ThenTerm,
            Context, MainContext, SubContext, Purity, ThenGoal, no, ThenAdded,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),

        finish_if_then_else_expr_then_goal(StateVars, BeforeSInfo, !SInfo),

        do_unravel_unification(term.variable(X), ElseTerm,
            Context, MainContext, SubContext, Purity, ElseGoal, no, ElseAdded,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),

        NumAdded = CondAdded + ThenAdded + ElseAdded,
        GoalExpr = if_then_else(StateVars ++ Vars,
            CondGoal, ThenGoal, ElseGoal),
        goal_info_init(Context, GoalInfo),
        Goal = GoalExpr - GoalInfo
    ;
        % Handle field extraction expressions.
        F = term.atom("^"),
        Args = [InputTerm, FieldNameTerm],
        parse_field_list(FieldNameTerm, FieldNameResult),
        FieldNameResult = ok(FieldNames)
    ->
        make_fresh_arg_var(InputTerm, InputTermVar, [], !VarSet, !SInfo, !IO),
        expand_get_field_function_call(Context, MainContext, SubContext,
            FieldNames, X, InputTermVar, Purity, Functor, _, Goal0, CallAdded,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),

        ArgContext = functor(Functor, MainContext, SubContext),
        do_insert_arg_unifications([InputTermVar], [InputTerm],
            FunctorContext, ArgContext, Goal0, Goal, no, ArgAdded,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        NumAdded = CallAdded + ArgAdded
    ;
        % Handle field update expressions.
        F = term.atom(":="),
        Args = [FieldDescrTerm, FieldValueTerm],
        FieldDescrTerm = term.functor(term.atom("^"),
            [InputTerm, FieldNameTerm], _),
        parse_field_list(FieldNameTerm, FieldNameResult),
        FieldNameResult = ok(FieldNames)
    ->
        make_fresh_arg_var(InputTerm, InputTermVar, [], !VarSet, !SInfo, !IO),
        make_fresh_arg_var(FieldValueTerm, FieldValueVar, [InputTermVar],
            !VarSet, !SInfo, !IO),

        expand_set_field_function_call(Context, MainContext, SubContext,
            FieldNames, FieldValueVar, InputTermVar, X,
            Functor, InnerFunctor - FieldSubContext, Goal0, CallAdded,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),

        TermArgContext = functor(Functor, MainContext, SubContext),
        TermArgNumber = 1,
        FieldArgContext = functor(InnerFunctor, MainContext, FieldSubContext),
        FieldArgNumber = 2,
        ArgContexts = [TermArgNumber - TermArgContext,
            FieldArgNumber - FieldArgContext],
        do_insert_arg_unifications_with_supplied_contexts(
            [InputTermVar, FieldValueVar], [InputTerm, FieldValueTerm],
            ArgContexts, Context, Goal0, Goal, no, ArgAdded, !VarSet,
            !ModuleInfo, !QualInfo, !SInfo, !IO),
        NumAdded = CallAdded + ArgAdded
    ;
        % Handle the usual case.
        % XXX Why do we use Arg1 instead of Args here?
        RHS = term.functor(F, Args1, FunctorContext),
        parse_qualified_term(RHS, RHS, "", MaybeFunctor),
        (
            MaybeFunctor = ok(FunctorName, FunctorArgs),
            list.length(FunctorArgs, Arity),
            ConsId = cons(FunctorName, Arity)
        ;
            % float, int or string constant
            %   - any errors will be caught by typechecking
            MaybeFunctor = error(_, _),
            list.length(Args, Arity),
            ConsId = make_functor_cons_id(F, Arity),
            FunctorArgs = Args
        ),
        (
            FunctorArgs = [],
            make_atomic_unification(X, functor(ConsId, no, []), Context,
                MainContext, SubContext, Purity, Goal0, !QualInfo),
            NumAdded = 1,
            Goal0 = GoalExpr - GoalInfo0,
            add_goal_info_purity_feature(Purity, GoalInfo0, GoalInfo),
            % We could wrap a from_ground_term(X) scope around Goal,
            % but there would be no gain from doing so, whereas the
            % increase would lead to a slight increase in memory and time
            % requirements.
            Goal = GoalExpr - GoalInfo
        ;
            FunctorArgs = [_ | _],
            make_fresh_arg_vars(FunctorArgs, HeadVars, !VarSet, !SInfo, !IO),
            make_atomic_unification(X, functor(ConsId, no, HeadVars), Context,
                MainContext, SubContext, Purity, Goal0, !QualInfo),
            MainFunctorAdded = 1,
            ArgContext = functor(ConsId, MainContext, SubContext),
            % Should this be insert_... rather than append_...?
            % No, because that causes efficiency problems
            % with type-checking :-(
            % But for impure unifications, we need to do this, because
            % mode reordering can't reorder around the functor unification.
            ( Purity = purity_pure ->
                do_append_arg_unifications(HeadVars, FunctorArgs,
                    FunctorContext, ArgContext, Goal0, Goal, no, ArgAdded,
                    !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO)
            ;
                Goal0 = GoalExpr0 - GoalInfo0,
                add_goal_info_purity_feature(Purity, GoalInfo0, GoalInfo1),
                Goal1 = GoalExpr0 - GoalInfo1,
                do_insert_arg_unifications(HeadVars, FunctorArgs,
                    FunctorContext, ArgContext, Goal1, Goal, no, ArgAdded,
                    !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO)
            ),
            NumAdded = MainFunctorAdded + ArgAdded
        )
    ).

%-----------------------------------------------------------------------------%
%
% Code for building lambda expressions
%

:- pred build_lambda_expression(prog_var::in, purity::in, purity::in,
    pred_or_func::in, lambda_eval_method::in, list(prog_term)::in,
    list(mer_mode)::in, determinism::in, goal::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in,
    hlds_goal::out, num_added_goals::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, io::di, io::uo) is det.

build_lambda_expression(X, UnificationPurity, LambdaPurity, PredOrFunc,
        EvalMethod, Args0, Modes, Det, ParsedGoal,
        Context, MainContext, SubContext, Goal, NumAdded,
        !VarSet, !ModuleInfo, !QualInfo, !.SInfo, !IO) :-
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
    % Note that the quantification is important here.  That's why we need
    % to introduce the explicit `some [...]'. Variables in the argument
    % positions are lambda-quantified, so when we move them to the body,
    % we need to make them explicitly existentially quantified to avoid
    % capturing any variables of the same name that occur outside this scope.
    %
    % Also, note that any introduced unifications that construct the output
    % arguments for the lambda expression, need to occur *after*, the body
    % of the lambda expression. This is in case the body of the lambda
    % expression is impure, in which case the mode analyser cannot reorder
    % the unifications; this results in a mode error.
    %
    % XXX The mode analyser *should* be able to reorder such unifications,
    % especially ones that the compiler introduced itself.
    %
    % For predicates, all variables occurring in the lambda arguments are
    % locally quantified to the lambda goal.  For functions, we need to
    % be careful because variables in arguments should similarly be quantified,
    % but variables in the function return value term (and not in the
    % arguments) should *not* be locally quantified.
    %
    % Create fresh variables, transform the goal to HLDS, and add unifications
    % with the fresh variables. We use varset.new_vars rather than
    % make_fresh_arg_vars, since for functions we need to ensure that
    % the variable corresponding to the function result term is a new variable,
    % to avoid the function result term becoming lambda-quantified.

    ( illegal_state_var_func_result(PredOrFunc, Args0, StateVar) ->
        report_illegal_func_svar_result(Context, !.VarSet, StateVar, !IO),
        Goal = true_goal,
        NumAdded = 0
    ; lambda_args_contain_bang_state_var(Args0, StateVar) ->
        report_illegal_bang_svar_lambda_arg(Context, !.VarSet, StateVar, !IO),
        Goal = true_goal,
        NumAdded = 0
    ;
        prepare_for_lambda(!SInfo),
        substitute_state_var_mappings(Args0, Args, !VarSet, !SInfo, !IO),

        list.length(Args, NumArgs),
        svvarset.new_vars(NumArgs, LambdaVars, !VarSet),
        %
        % Partition the arguments (and their corresponding lambda variables)
        % into two sets: those that are not output, i.e. input and unused,
        % and those that are output.
        %
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
            unexpected(this_file,
                "mismatched lists in build_lambda_expression.")
        ),

        map.init(Substitution),
        ArgContext = head(PredOrFunc, NumArgs),
        %
        % Create the unifications that need to come before the body of
        % the lambda expression; those corresponding to args whose mode
        % is input or unused.
        %
        HeadBefore0 = true_goal,
        insert_arg_unifications(NonOutputLambdaVars, NonOutputArgs,
            Context, ArgContext, HeadBefore0, HeadBefore, NonOutputAdded,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        %
        % Create the unifications that need to come after the body of
        % the lambda expression; those corresponding to args whose mode
        % is output.
        %
        HeadAfter0 = true_goal,
        insert_arg_unifications(OutputLambdaVars, OutputArgs,
            Context, ArgContext, HeadAfter0, HeadAfter, OutputAdded,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),

        prepare_for_body(FinalSVarMap, !VarSet, !SInfo),

        transform_goal(ParsedGoal, Substitution, Body, BodyAdded,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        NumAdded = NonOutputAdded + OutputAdded + BodyAdded,

        %
        % Fix up any state variable unifications.
        %
        finish_goals(Context, FinalSVarMap, [HeadBefore, Body, HeadAfter],
            HLDS_Goal0, !.SInfo),
        %
        % Figure out which variables we need to explicitly existentially
        % quantify.
        %
        (
            PredOrFunc = predicate,
            QuantifiedArgs = Args
        ;
            PredOrFunc = function,
            pred_args_to_func_args(Args, QuantifiedArgs, _ReturnValTerm)
        ),
        term.vars_list(QuantifiedArgs, QuantifiedVars0),
        list.sort_and_remove_dups(QuantifiedVars0, QuantifiedVars),

        goal_info_init(Context, GoalInfo),
        HLDS_Goal = scope(exist_quant(QuantifiedVars), HLDS_Goal0) - GoalInfo,
        %
        % We set the lambda nonlocals here to anything that could
        % possibly be nonlocal.  Quantification will reduce this down to
        % the proper set of nonlocal arguments.
        %
        some [!LambdaGoalVars] (
            goal_util.goal_vars(HLDS_Goal, !:LambdaGoalVars),
            svset.delete_list(LambdaVars, !LambdaGoalVars),
            svset.delete_list(QuantifiedVars, !LambdaGoalVars),
            LambdaNonLocals = set.to_sorted_list(!.LambdaGoalVars)
        ),

        LambdaGoal = lambda_goal(LambdaPurity, PredOrFunc, EvalMethod,
            LambdaNonLocals, LambdaVars, Modes, Det, HLDS_Goal),
        make_atomic_unification(X, LambdaGoal, Context, MainContext,
            SubContext, UnificationPurity, Goal, !QualInfo)
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
        % the arguements/lambda vars into because mode analysis will fail
        % anyway.

        ( mode_is_undefined(ModuleInfo, Mode) ->
            InputArgs        = [Arg | InputArgs0],
            OutputArgs       = OutputArgs0,
            InputLambdaVars  = [LambdaVar | InputLambdaVars0],
            OutputLambdaVars = OutputLambdaVars0
        ;
            ( mode_is_output(ModuleInfo, Mode) ->
                InputArgs        = InputArgs0,
                OutputArgs       = [Arg | OutputArgs0],
                InputLambdaVars  = InputLambdaVars0,
                OutputLambdaVars = [LambdaVar | OutputLambdaVars0]
            ;
                InputArgs        = [Arg | InputArgs0],
                OutputArgs       = OutputArgs0,
                InputLambdaVars  = [LambdaVar | InputLambdaVars0],
                OutputLambdaVars = OutputLambdaVars0
            )
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

arg_context_to_unify_context(head(PredOrFunc, Arity), ArgNum,
        ArgContext, []) :-
    ( PredOrFunc = function, ArgNum = Arity ->
        % it's the function result term in the head
        ArgContext = head_result
    ;
        % it's a head argument
        ArgContext = head(ArgNum)
    ).
arg_context_to_unify_context(call(PredId), ArgNum, call(PredId, ArgNum), []).
arg_context_to_unify_context(functor(ConsId, MainContext, SubContexts), ArgNum,
    MainContext, [ConsId - ArgNum | SubContexts]).

%-----------------------------------------------------------------------------%

make_fresh_arg_vars(Args, Vars, !VarSet, !SInfo, !IO) :-
    % For efficiency, we construct `Vars' backwards and then reverse it
    % to get the correct order.
    make_fresh_arg_vars_2(Args, [], Vars1, !VarSet, !SInfo, !IO),
    list.reverse(Vars1, Vars).

:- pred make_fresh_arg_vars_2(list(prog_term)::in, list(prog_var)::in,
    list(prog_var)::out, prog_varset::in,prog_varset::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

make_fresh_arg_vars_2([], Vars, Vars, !VarSet, !SInfo, !IO).
make_fresh_arg_vars_2([Arg | Args], Vars0, Vars, !VarSet, !SInfo, !IO) :-
    make_fresh_arg_var(Arg, Var, Vars0, !VarSet, !SInfo, !IO),
    make_fresh_arg_vars_2(Args, [Var | Vars0], Vars, !VarSet, !SInfo, !IO).

make_fresh_arg_var(Arg0, Var, Vars0, !VarSet, !SInfo, !IO) :-
    substitute_state_var_mapping(Arg0, Arg, !VarSet, !SInfo, !IO),
    (
        Arg = term.variable(ArgVar),
        \+ list.member(ArgVar, Vars0)
    ->
        Var = ArgVar
    ;
        varset.new_var(!.VarSet, Var, !:VarSet)
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "superhomogeneous.m".

%-----------------------------------------------------------------------------%
:- end_module superhomogeneous.
%-----------------------------------------------------------------------------%
