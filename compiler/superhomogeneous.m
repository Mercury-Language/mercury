%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2012,2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: superhomogeneous.m.
% Main author of the original version of this module: fjh.
% Main author of the current version of this module: zs.
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
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred insert_arg_unifications_with_contexts(list(prog_var)::in,
    list(prog_term)::in, assoc_list(int, arg_context)::in, prog_context::in,
    hlds_goal::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred unravel_unification(prog_term::in, prog_term::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, purity::in,
    hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % make_fresh_arg_vars_subst_svars(Args, Vars, !VarSet, !SVarState, !Specs):
    %
    % Vars is a list of distinct variables corresponding to the terms in Args.
    % For each term in Args, if the term is a variable V which is distinct
    % from the variables already produced, then the corresponding variable
    % in Vars is just V, otherwise we allocate a fresh variable from !VarSet.
    % !:VarSet is the varset resulting after all the necessary variables
    % have been allocated. If any of the Args is of the form !.S or !:S,
    % we do state var substitution for them. We need !SVarState for correct
    % state var references, and !Specs for incorrect state var references.
    %
:- pred make_fresh_arg_vars_subst_svars(list(prog_term)::in,
    list(prog_var)::out,
    prog_varset::in, prog_varset::out, svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.from_ground_term_util.
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
:- import_module cord.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

insert_arg_unifications(XVars, XArgTerms0, Context, ArgContext, Goal0, Goal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    substitute_state_var_mappings(XArgTerms0, XArgTerms,
        !VarSet, !SVarState, !Specs),
    do_arg_unifications(XVars, XArgTerms, Context, ArgContext,
        construct_bottom_up, 1, Expansions,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    Goal0 = hlds_goal(_, GoalInfo0),
    insert_expansions_before_goal_top_not_fgti(GoalInfo0, Expansions,
        Goal0, Goal).

insert_arg_unifications_with_contexts(XVars, XArgTerms0, ArgContexts, Context,
        Goal0, Goal, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    substitute_state_var_mappings(XArgTerms0, XArgTerms,
        !VarSet, !SVarState, !Specs),
    do_arg_unifications_with_contexts(XVars, XArgTerms, ArgContexts, Context,
        construct_bottom_up, Expansions,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    Goal0 = hlds_goal(_, GoalInfo0),
    insert_expansions_before_goal_top_not_fgti(GoalInfo0, Expansions,
        Goal0, Goal).

unravel_unification(LHS0, RHS0, Context, MainContext, SubContext, Purity,
        Goal, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    (
        Purity = purity_pure,
        Order = deconstruct_top_down
    ;
        ( Purity = purity_semipure
        ; Purity = purity_impure
        ),
        Order = construct_bottom_up
    ),
    do_unravel_unification(LHS0, RHS0, Context, MainContext, SubContext,
        Purity, Order, Expansion,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    goal_info_init(Context, GoalInfo),
    expansion_to_goal_wrap_if_fgti(GoalInfo, Expansion, Goal).

%-----------------------------------------------------------------------------%

:- type maybe_fgti_var_size
    --->    not_fgti
    ;       fgti_var_size(prog_var, int).

:- type expansion
    --->    expansion(
                maybe_fgti_var_size,
                cord(hlds_goal)
            ).

%-----------------------------------------------------------------------------%

:- pred expansion_to_goal_wrap_if_fgti(hlds_goal_info::in, expansion::in,
    hlds_goal::out) is det.

expansion_to_goal_wrap_if_fgti(GoalInfo, Expansion, Goal) :-
    Expansion = expansion(MaybeFGTI, GoalCord),
    Goals = cord.list(GoalCord),
    (
        Goals = [],
        Goal = hlds_goal(true_goal_expr, GoalInfo)
    ;
        Goals = [Goal]
    ;
        Goals = [_, _ | _],
        (
            MaybeFGTI = fgti_var_size(TermVar, Size),
            get_maybe_from_ground_term_threshold = yes(Threshold),
            Size >= Threshold
        ->
            goal_info_set_nonlocals(set_of_var.make_singleton(TermVar),
                GoalInfo, MarkedGoalInfo),
            mark_nonlocals_in_ground_term_construct(Goals, MarkedGoals),
            ConjGoalExpr = conj(plain_conj, MarkedGoals),
            ConjGoal = hlds_goal(ConjGoalExpr, MarkedGoalInfo),
            Reason = from_ground_term(TermVar, from_ground_term_initial),
            ScopeGoalExpr = scope(Reason, ConjGoal),
            ScopeGoal = hlds_goal(ScopeGoalExpr, MarkedGoalInfo),
            Goal = ScopeGoal
        ;
            ConjGoalExpr = conj(plain_conj, Goals),
            ConjGoal = hlds_goal(ConjGoalExpr, GoalInfo),
            Goal = ConjGoal
        )
    ).

:- pred expansion_to_goal_cord_wrap_if_fgti(hlds_goal_info::in, expansion::in,
    cord(hlds_goal)::out) is det.

expansion_to_goal_cord_wrap_if_fgti(GoalInfo, Expansion,
        MaybeWrappedGoalCord) :-
    Expansion = expansion(MaybeFGTI, GoalCord),
    (
        MaybeFGTI = fgti_var_size(TermVar, Size),
        get_maybe_from_ground_term_threshold = yes(Threshold),
        Size >= Threshold
    ->
        Goals = cord.list(GoalCord),
        goal_info_set_nonlocals(set_of_var.make_singleton(TermVar),
            GoalInfo, MarkedGoalInfo),
        mark_nonlocals_in_ground_term_construct(Goals, MarkedGoals),
        ConjGoalExpr = conj(plain_conj, MarkedGoals),
        ConjGoal = hlds_goal(ConjGoalExpr, MarkedGoalInfo),
        Reason = from_ground_term(TermVar, from_ground_term_initial),
        ScopeGoalExpr = scope(Reason, ConjGoal),
        ScopeGoal = hlds_goal(ScopeGoalExpr, MarkedGoalInfo),
        MaybeWrappedGoalCord = cord.singleton(ScopeGoal)
    ;
        MaybeWrappedGoalCord = GoalCord
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

%-----------------------------------------------------------------------------%

:- pred insert_expansion_before_goal_top_not_fgti(hlds_goal_info::in,
    expansion::in, hlds_goal::in, hlds_goal::out) is det.

insert_expansion_before_goal_top_not_fgti(GoalInfo, Expansion, BaseGoal,
        Goal) :-
    goal_to_conj_list(BaseGoal, BaseGoals),
    expansion_to_goal_cord_wrap_if_fgti(GoalInfo, Expansion,
        ExpansionGoalCord),
    ExpansionGoals = cord.list(ExpansionGoalCord),
    conj_list_to_goal(ExpansionGoals ++ BaseGoals, GoalInfo, Goal).

:- pred insert_expansions_before_goal_top_not_fgti(hlds_goal_info::in,
    list(expansion)::in, hlds_goal::in, hlds_goal::out) is det.

insert_expansions_before_goal_top_not_fgti(GoalInfo, Expansions, BaseGoal,
        Goal) :-
    goal_to_conj_list(BaseGoal, BaseGoals),
    list.map(expansion_to_goal_cord_wrap_if_fgti(GoalInfo), Expansions,
        ExpansionGoalCords),
    ExpansionGoals = cord.cord_list_to_list(ExpansionGoalCords),
    conj_list_to_goal(ExpansionGoals ++ BaseGoals, GoalInfo, Goal).

:- pred append_expansions_after_goal_top_ftgi(hlds_goal_info::in, prog_var::in,
    hlds_goal::in, int::in, list(expansion)::in, expansion::out) is det.

append_expansions_after_goal_top_ftgi(GoalInfo, TermVar,
        BaseGoal, BaseGoalSize, ArgExpansions, Expansion) :-
    append_expansions_after_goal_top_ftgi_loop(ArgExpansions, yes, AllFGTI,
        BaseGoalSize, TotalSize),
    (
        AllFGTI = no,
        list.map(expansion_to_goal_cord_wrap_if_fgti(GoalInfo),
            ArgExpansions, ArgGoalCords),
        ArgGoalsCord = cord.cord_list_to_cord(ArgGoalCords),
        % XXX If BaseGoal can be a plain_conj, then we should expand it here.
        GoalCord = cord.cons(BaseGoal, ArgGoalsCord),
        Expansion = expansion(not_fgti, GoalCord)
    ;
        AllFGTI = yes,
        list.map(project_expansion_goals, ArgExpansions, ArgGoalCords),
        ArgGoalsCord = cord.cord_list_to_cord(ArgGoalCords),
        % XXX If BaseGoal can be a plain_conj, then we should expand it here.
        GoalCord = cord.cons(BaseGoal, ArgGoalsCord),
        Expansion = expansion(fgti_var_size(TermVar, TotalSize), GoalCord)
    ).

:- pred append_expansions_after_goal_top_ftgi_loop(list(expansion)::in,
    bool::in, bool::out, int::in, int::out) is det.

append_expansions_after_goal_top_ftgi_loop([], !AllFGTI, !TotalSize).
append_expansions_after_goal_top_ftgi_loop([Expansion | Expansions],
        !AllFGTI, !TotalSize) :-
    Expansion = expansion(MaybeFGTI, _),
    (
        MaybeFGTI = not_fgti,
        !:AllFGTI = no
    ;
        MaybeFGTI = fgti_var_size(_, Size),
        !:TotalSize = !.TotalSize + Size
    ),
    append_expansions_after_goal_top_ftgi_loop(Expansions,
        !AllFGTI, !TotalSize).

:- pred project_expansion_goals(expansion::in, cord(hlds_goal)::out)
    is det.

project_expansion_goals(expansion(_, GoalCord), GoalCord).

%-----------------------------------------------------------------------------%

:- pred do_arg_unifications(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in,
    goal_order::in, int::in, list(expansion)::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_arg_unifications([], [], _Context, _ArgContext,
        _Order, _ArgNum, [], !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs).
do_arg_unifications([], [_ | _], _Context, _ArgContext,
        _Order, _ArgNum, [], !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    unexpected($module, $pred, "length mismatch").
do_arg_unifications([_ | _], [], _Context, _ArgContext,
        _Order, _ArgNum, [], !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    unexpected($module, $pred, "length mismatch").
do_arg_unifications([XVar | XVars], [YTerm | YTerms], Context, ArgContext,
        Order, ArgNum, [Expansion | Expansions], !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    do_arg_unification(XVar, YTerm, Context, ArgContext, Order, ArgNum,
        Expansion, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),
    do_arg_unifications(XVars, YTerms, Context, ArgContext, Order, ArgNum + 1,
        Expansions, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs).

:- pred do_arg_unifications_with_fresh_vars(list(prog_term)::in,
    prog_context::in, arg_context::in, goal_order::in, int::in,
    list(prog_var)::in, list(prog_var)::out, list(expansion)::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_arg_unifications_with_fresh_vars([], _Context, _ArgContext,
        _Order, _ArgNum, _, [], [], !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs).
do_arg_unifications_with_fresh_vars([YTerm | YTerms], Context, ArgContext,
        Order, ArgNum, !.SeenXVars, [XVar | XVars], [Expansion | Expansions],
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    make_fresh_arg_var_no_svar(YTerm, XVar, !.SeenXVars, !VarSet),
    !:SeenXVars = [XVar | !.SeenXVars],
    do_arg_unification(XVar, YTerm, Context, ArgContext, Order,
        ArgNum, Expansion, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),
    do_arg_unifications_with_fresh_vars(YTerms, Context, ArgContext, Order,
        ArgNum + 1, !.SeenXVars, XVars, Expansions, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs).

:- pred do_arg_unifications_with_contexts(list(prog_var)::in,
    list(prog_term)::in, assoc_list(int, arg_context)::in,
    prog_context::in, goal_order::in, list(expansion)::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_arg_unifications_with_contexts(XVars, YTerms, ArgContexts,
        Context, Order, Expansions, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    (
        XVars = [],
        YTerms = [],
        ArgContexts = []
    ->
        Expansions = []
    ;
        XVars = [HeadXVar | TailXVars],
        YTerms = [HeadYTerm | TailYTerms],
        ArgContexts = [HeadArgNumber - HeadArgContext | TailArgContexts]
    ->
        do_arg_unification(HeadXVar, HeadYTerm, Context, HeadArgContext, Order,
            HeadArgNumber, HeadExpansion, !SVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs),
        do_arg_unifications_with_contexts(TailXVars, TailYTerms,
            TailArgContexts, Context, Order, TailExpansions,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        Expansions = [HeadExpansion | TailExpansions]
    ;
        unexpected($module, $pred, "length mismatch")
    ).

:- pred do_arg_unification(prog_var::in, prog_term::in,
    prog_context::in, arg_context::in,
    goal_order::in, int::in, expansion::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_arg_unification(XVar, YTerm, Context, ArgContext, Order, ArgNum,
        Expansion, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    % It is the caller's job to make sure that if needed, then both
    % XVar and the top level of YTerm have already been through
    % state var mapping expansion.
    (
        YTerm = term.variable(YVar, YVarContext),
        ( XVar = YVar ->
            % Skip unifications of the form `XVar = XVar'.
            GoalCord = cord.init
        ;
            arg_context_to_unify_context(ArgContext, ArgNum,
                MainContext, SubContext),
            make_atomic_unification(XVar, rhs_var(YVar), YVarContext,
                MainContext, SubContext, purity_pure, Goal, !QualInfo),
            GoalCord = cord.singleton(Goal)
        ),
        Expansion = expansion(not_fgti, GoalCord)
    ;
        YTerm = term.functor(YFunctor, YArgTerms, YFunctorContext),
        arg_context_to_unify_context(ArgContext, ArgNum,
            MainContext, SubContext),
        unravel_var_functor_unification(XVar, YFunctor, YArgTerms,
            YFunctorContext, Context, MainContext, SubContext, purity_pure,
            Order, Expansion, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ).

%-----------------------------------------------------------------------------%

:- pred do_unravel_unification(prog_term::in, prog_term::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    purity::in, goal_order::in, expansion::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_unravel_unification(LHS0, RHS0, Context, MainContext, SubContext,
        Purity, Order, Expansion, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    substitute_state_var_mapping(LHS0, LHS, !VarSet, !SVarState, !Specs),
    substitute_state_var_mapping(RHS0, RHS, !VarSet, !SVarState, !Specs),
    classify_unravel_unification(LHS, RHS,
        Context, MainContext, SubContext, Purity, Order, Expansion,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).

:- pred do_unravel_var_unification(prog_var::in, prog_term::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    purity::in, goal_order::in, expansion::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_unravel_var_unification(LHSVar, RHS0, Context, MainContext, SubContext,
        Purity, Order, Expansion, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    substitute_state_var_mapping(RHS0, RHS, !VarSet, !SVarState, !Specs),
    classify_unravel_var_unification(LHSVar, RHS,
        Context, MainContext, SubContext, Purity, Order, Expansion,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).

:- pred classify_unravel_unification(prog_term::in, prog_term::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    purity::in, goal_order::in, expansion::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_unravel_unification(XTerm, YTerm, Context, MainContext, SubContext,
        Purity, Order, Expansion, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    (
        % `X = Y' needs no unravelling.
        XTerm = term.variable(XVar, _),
        YTerm = term.variable(YVar, _),
        make_atomic_unification(XVar, rhs_var(YVar), Context, MainContext,
            SubContext, Purity, Goal, !QualInfo),
        Expansion = expansion(not_fgti, cord.singleton(Goal))
    ;
        XTerm = term.variable(XVar, _),
        YTerm = term.functor(YFunctor, YArgTerms, YFunctorContext),
        unravel_var_functor_unification(XVar, YFunctor, YArgTerms,
            YFunctorContext, Context, MainContext, SubContext,
            Purity, Order, Expansion, !SVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs)
    ;
        XTerm = term.functor(XFunctor, XArgTerms, XFunctorContext),
        YTerm = term.variable(YVar, _),
        unravel_var_functor_unification(YVar, XFunctor, XArgTerms,
            XFunctorContext, Context, MainContext, SubContext,
            Purity, Order, Expansion, !SVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs)
    ;
        % If we find a unification of the form `f1(...) = f2(...)',
        % then we replace it with `Tmp = f1(...), Tmp = f2(...)',
        % and then process it according to the rules above.
        % Note that we can't simplify it yet, e.g. by pairwise unifying
        % the args of XTerm and YTerm, because we might simplify away
        % type errors.
        XTerm = term.functor(XFunctor, XArgTerms, XFunctorContext),
        YTerm = term.functor(YFunctor, YArgTerms, YFunctorContext),
        varset.new_var(TmpVar, !VarSet),
        unravel_var_functor_unification(TmpVar, XFunctor, XArgTerms,
            XFunctorContext, Context, MainContext, SubContext,
            Purity, Order, ExpansionX,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        unravel_var_functor_unification(TmpVar, YFunctor, YArgTerms,
            YFunctorContext, Context, MainContext, SubContext,
            Purity, Order, ExpansionY,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        goal_info_init(Context, GoalInfo),
        expansion_to_goal_cord_wrap_if_fgti(GoalInfo, ExpansionX,
            MaybeWrappedGoalCordX),
        expansion_to_goal_cord_wrap_if_fgti(GoalInfo, ExpansionY,
            MaybeWrappedGoalCordY),
        GoalCord = MaybeWrappedGoalCordX ++ MaybeWrappedGoalCordY,
        Expansion = expansion(not_fgti, GoalCord)
    ).

:- pred classify_unravel_var_unification(prog_var::in, prog_term::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    purity::in, goal_order::in, expansion::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_unravel_var_unification(XVar, YTerm, Context, MainContext, SubContext,
        Purity, Order, Expansion, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs) :-
    (
        % `X = Y' needs no unravelling.
        YTerm = term.variable(YVar, _),
        make_atomic_unification(XVar, rhs_var(YVar), Context, MainContext,
            SubContext, Purity, Goal, !QualInfo),
        Expansion = expansion(not_fgti, cord.singleton(Goal))
    ;
        YTerm = term.functor(YFunctor, YArgTerms, YFunctorContext),
        unravel_var_functor_unification(XVar, YFunctor, YArgTerms,
            YFunctorContext, Context, MainContext, SubContext,
            Purity, Order, Expansion, !SVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs)
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
    purity::in, goal_order::in, expansion::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

unravel_var_functor_unification(XVar, YFunctor, YArgTerms0, YFunctorContext,
        Context, MainContext, SubContext, Purity, Order, Expansion,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    substitute_state_var_mappings(YArgTerms0, YArgTerms, !VarSet,
        !SVarState, !Specs),
    (
        YFunctor = term.atom(YAtom),
        maybe_unravel_special_var_functor_unification(XVar, YAtom, YArgTerms,
            YFunctorContext, Context, MainContext, SubContext, Purity,
            Order, ExpansionPrime, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    ->
        Expansion = ExpansionPrime
    ;
        % Handle higher-order pred and func expressions.
        RHS = term.functor(YFunctor, YArgTerms, YFunctorContext),
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
            build_lambda_expression(XVar, Purity, LambdaPurity, Groundness,
                PredOrFunc, EvalMethod, Vars1, Modes, Det, ParsedGoal,
                Context, MainContext, SubContext, Goal,
                !.SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            Expansion = expansion(not_fgti, cord.singleton(Goal))
        ;
            MaybeParsedGoal = error1(ParsedGoalSpecs),
            !:Specs = ParsedGoalSpecs ++ !.Specs,
            Expansion = expansion(not_fgti, cord.singleton(true_goal))
        )
    ;
        % Handle the usual case.
        (
            % The condition of this if-then-else is based on the logic of
            % try_parse_sym_name_and_args, but specialized to this location,
            % so that we can do state var expansion only if we need to.
            YFunctor = term.atom(FName),
            (
                FName = ".",
                YArgTerms = [ModuleTerm, NameArgsTerm]
            ->
                NameArgsTerm = term.functor(term.atom(Name), NameArgTerms, _),
                try_parse_symbol_name(ModuleTerm, Module),
                FunctorName = qualified(Module, Name),
                % We have done state variable name expansion at the top
                % level of Args, but not at the level of NameArgTerms.
                substitute_state_var_mappings(NameArgTerms,
                    MaybeQualifiedYArgTermsPrime, !VarSet, !SVarState, !Specs)
            ;
                FunctorName = string_to_sym_name_sep(FName, "__"),
                MaybeQualifiedYArgTermsPrime = YArgTerms
            )
        ->
            MaybeQualifiedYArgTerms = MaybeQualifiedYArgTermsPrime,
            list.length(MaybeQualifiedYArgTerms, Arity),
            ConsId = cons(FunctorName, Arity, cons_id_dummy_type_ctor)
        ;
            % float, int or string constant
            %   - any errors will be caught by typechecking
            list.length(YArgTerms, Arity),
            ConsId = make_functor_cons_id(YFunctor, Arity),
            MaybeQualifiedYArgTerms = YArgTerms
        ),
        % At this point, we have done state variable name expansion
        % at the top level of MaybeQualifiedYArgTerms.
        (
            MaybeQualifiedYArgTerms = [],
            make_atomic_unification(XVar, rhs_functor(ConsId, no, []),
                YFunctorContext, MainContext, SubContext, Purity, FunctorGoal,
                !QualInfo),
            goal_set_purity(Purity, FunctorGoal, Goal),
            Expansion = expansion(fgti_var_size(XVar, 1), cord.singleton(Goal))
        ;
            MaybeQualifiedYArgTerms = [_ | _],
            ArgContext = ac_functor(ConsId, MainContext, SubContext),
            (
                Purity = purity_pure,
                % If we can, we want to add the unifications for the arguments
                % AFTER the unification of the top level function symbol,
                % because otherwise we get efficiency problems during
                % type-checking :-(
                do_arg_unifications_with_fresh_vars(MaybeQualifiedYArgTerms,
                    YFunctorContext, ArgContext, deconstruct_top_down, 1,
                    [], YVars, ArgExpansions, !SVarState, !SVarStore, !VarSet,
                    !ModuleInfo, !QualInfo, !Specs),
                make_atomic_unification(XVar, rhs_functor(ConsId, no, YVars),
                    YFunctorContext, MainContext, SubContext, Purity,
                    FunctorGoal, !QualInfo),
                goal_info_init(Context, GoalInfo),
                append_expansions_after_goal_top_ftgi(GoalInfo, XVar,
                    FunctorGoal, 1, ArgExpansions, Expansion)
            ;
                ( Purity = purity_semipure
                ; Purity = purity_impure
                ),
                % For impure unifications, we need to put the unifications
                % for the arguments BEFORE the unification of the top level
                % function symbol, because mode reordering can't reorder
                % code around that unification.
                do_arg_unifications_with_fresh_vars(MaybeQualifiedYArgTerms,
                    YFunctorContext, ArgContext, construct_bottom_up, 1,
                    [], YVars, ArgExpansions, !SVarState, !SVarStore, !VarSet,
                    !ModuleInfo, !QualInfo, !Specs),
                make_atomic_unification(XVar, rhs_functor(ConsId, no, YVars),
                    YFunctorContext, MainContext, SubContext, Purity,
                    FunctorGoal, !QualInfo),
                goal_info_init(Context, GoalInfo),
                insert_expansions_before_goal_top_not_fgti(GoalInfo,
                    ArgExpansions, FunctorGoal, Goal0),
                goal_set_purity(Purity, Goal0, Goal),
                Expansion = expansion(not_fgti, cord.singleton(Goal))
            )
        )
    ).

    % See whether Atom indicates a term with special syntax.
    %
:- pred maybe_unravel_special_var_functor_unification(prog_var::in,
    string::in, list(prog_term)::in, term.context::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    purity::in, goal_order::in, expansion::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is semidet.

maybe_unravel_special_var_functor_unification(XVar, YAtom, YArgs,
        YFunctorContext, Context, MainContext, SubContext, Purity, Order,
        Expansion, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs)  :-
    % Switch on YAtom.
    % XXX instead of failing if YAtom has the wrong number of arguments or
    % if the arguments have the wrong shape, we should generate an error
    % message.
    (
        % Handle explicit type qualification.
        ( YAtom = "with_type"
        ; YAtom = ":"
        ),
        YArgs = [RVal, DeclType0],

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
                process_type_qualification(XVar, DeclType, DeclVarSet,
                    Context, !ModuleInfo, !QualInfo, !Specs)
            ;
                DeclTypeResult = error1(DeclTypeSpecs),
                % The varset is a prog_varset even though it contains
                % the names of type variables in ErrorTerm, which is
                % a generic term.
                !:Specs = DeclTypeSpecs ++ !.Specs
            ),
            do_unravel_var_unification(XVar, RVal,
                Context, MainContext, SubContext, Purity, Order, Expansion,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs)
        )
    ;
        % Handle unification expressions.
        YAtom = "@",
        YArgs = [LVal, RVal],

        require_det (
            do_unravel_var_unification(XVar, LVal, Context,
                MainContext, SubContext, Purity, Order, ExpansionL,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            do_unravel_var_unification(XVar, RVal, Context,
                MainContext, SubContext, Purity, Order, ExpansionR,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            ExpansionL = expansion(_, GoalCordL),
            ExpansionR = expansion(_, GoalCordR),
            Expansion = expansion(not_fgti, GoalCordL ++ GoalCordR)
        )
    ;
        % Handle if-then-else expressions.
        (
            YAtom = "else",
            YArgs = [CondThenTerm0, ElseTerm0],
            CondThenTerm0 = term.functor(term.atom("if"),
                [term.functor(term.atom("then"), [CondTerm0, ThenTerm0], _)],
                    _)
        ;
            YAtom = ";",
            YArgs = [CondThenTerm0, ElseTerm0],
            CondThenTerm0 = term.functor(term.atom("->"),
                [CondTerm0, ThenTerm0], _)
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
                    CondParseTree, EmptySubst, CondGoal,
                    BeforeInsideSVarState, AfterCondInsideSVarState,
                    !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),

                substitute_state_var_mapping(ThenTerm0, ThenTerm, !VarSet,
                    AfterCondInsideSVarState, AfterThenInsideSVarState0,
                    !Specs),
                classify_unravel_var_unification(XVar, ThenTerm,
                    Context, MainContext, SubContext,
                    Purity, Order, ThenExpansion,
                    AfterThenInsideSVarState0, AfterThenInsideSVarState,
                    !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
                goal_info_init(get_term_context(ThenTerm), ThenGoalInfo),
                expansion_to_goal_wrap_if_fgti(ThenGoalInfo,
                    ThenExpansion, ThenGoal0),

                svar_finish_local_state_vars(StateVars, BeforeSVarState,
                    AfterThenInsideSVarState, AfterThenSVarState),

                substitute_state_var_mapping(ElseTerm0, ElseTerm, !VarSet,
                    BeforeSVarState, AfterElseSVarState0, !Specs),
                classify_unravel_var_unification(XVar, ElseTerm,
                    Context, MainContext, SubContext,
                    Purity, Order, ElseExpansion,
                    AfterElseSVarState0, AfterElseSVarState,
                    !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
                goal_info_init(get_term_context(ElseTerm), ElseGoalInfo),
                expansion_to_goal_wrap_if_fgti(ElseGoalInfo,
                    ElseExpansion, ElseGoal0),

                svar_finish_if_then_else(loc_inside_atomic_goal, Context,
                    StateVars, ThenGoal0, ThenGoal, ElseGoal0, ElseGoal,
                    BeforeSVarState, AfterCondInsideSVarState,
                    AfterThenSVarState, AfterElseSVarState,
                    AfterITESVarState, !VarSet, !SVarStore, !Specs),
                !:SVarState = AfterITESVarState,

                GoalExpr = if_then_else(StateVars ++ Vars,
                    CondGoal, ThenGoal, ElseGoal),
                goal_info_init(Context, GoalInfo),
                Goal = hlds_goal(GoalExpr, GoalInfo),
                Expansion = expansion(not_fgti, cord.singleton(Goal))
            ;
                MaybeVarsCond = error3(VarsCondSpecs),
                !:Specs = VarsCondSpecs ++ !.Specs,
                Expansion = expansion(not_fgti, cord.singleton(true_goal))
            )
        )
    ;
        % Handle field extraction expressions.
        YAtom = "^",
        YArgs = [InputTerm0, FieldNameTerm],
        maybe_parse_field_list(FieldNameTerm, !.VarSet, FieldNames),

        require_det (
            substitute_state_var_mapping(InputTerm0, InputTerm, !VarSet,
                !SVarState, !Specs),
            make_fresh_arg_var_no_svar(InputTerm, InputTermVar, [], !VarSet),
            expand_get_field_function_call(Context, MainContext, SubContext,
                FieldNames, XVar, InputTermVar, Purity, Functor, _, GetGoal,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),

            ArgContext = ac_functor(Functor, MainContext, SubContext),
            do_arg_unification(InputTermVar, InputTerm,
                YFunctorContext, ArgContext, Order, 1, InputArgExpansion,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            goal_info_init(Context, GoalInfo),
            insert_expansion_before_goal_top_not_fgti(GoalInfo,
                InputArgExpansion, GetGoal, Goal),
            Expansion = expansion(not_fgti, cord.singleton(Goal))
        )
    ;
        % Handle field update expressions.
        YAtom = ":=",
        YArgs = [FieldDescrTerm, FieldValueTerm0],
        FieldDescrTerm = term.functor(term.atom("^"),
            [InputTerm0, FieldNameTerm], _),
        maybe_parse_field_list(FieldNameTerm, !.VarSet, FieldNames),

        require_det (
            substitute_state_var_mapping(InputTerm0, InputTerm,
                !VarSet, !SVarState, !Specs),
            make_fresh_arg_var_no_svar(InputTerm, InputTermVar, [], !VarSet),
            substitute_state_var_mapping(FieldValueTerm0, FieldValueTerm,
                !VarSet, !SVarState, !Specs),
            make_fresh_arg_var_no_svar(FieldValueTerm, FieldValueVar,
                [InputTermVar], !VarSet),

            expand_set_field_function_call(Context, MainContext, SubContext,
                FieldNames, FieldValueVar, InputTermVar, XVar,
                Functor, InnerFunctor - FieldSubContext, SetGoal,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),

            TermArgContext = ac_functor(Functor, MainContext, SubContext),
            TermArgNumber = 1,
            FieldArgContext = ac_functor(InnerFunctor, MainContext,
                FieldSubContext),
            FieldArgNumber = 2,
            ArgContexts = [TermArgNumber - TermArgContext,
                FieldArgNumber - FieldArgContext],
            do_arg_unifications_with_contexts([InputTermVar, FieldValueVar],
                [InputTerm, FieldValueTerm], ArgContexts, Context, Order,
                InputFieldArgExpansions, !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),

            goal_info_init(Context, GoalInfo),
            insert_expansions_before_goal_top_not_fgti(GoalInfo,
                InputFieldArgExpansions, SetGoal, Goal),
            Expansion = expansion(not_fgti, cord.singleton(Goal))
        )
    ;
        % Handle higher-order dcg pred expressions. They have the same
        % semantics as higher-order pred expressions, but have two extra
        % arguments, and the goal is expanded as a DCG goal.
        YAtom = "-->",
        YArgs = [PredTerm0, GoalTerm0],
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
                build_lambda_expression(XVar, Purity, DCGLambdaPurity,
                    Groundness, pf_predicate, EvalMethod, Vars1, Modes, Det,
                    ParsedGoal, Context, MainContext, SubContext,
                    Goal0, !.SVarState, !SVarStore, !VarSet,
                    !ModuleInfo, !QualInfo, !Specs),
                goal_set_purity(Purity, Goal0, Goal),
                Expansion = expansion(not_fgti, cord.singleton(Goal))
            ;
                MaybeParsedGoal = error1(ParsedGoalSpecs),
                !:Specs = ParsedGoalSpecs ++ !.Specs,
                Expansion = expansion(not_fgti, cord.singleton(true_goal))
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
    hlds_goal::out, svar_state::in, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_lambda_expression(X, UnificationPurity, LambdaPurity, Groundness,
        PredOrFunc, EvalMethod, Args0, Modes, Det, ParsedGoal,
        Context, MainContext, SubContext, Goal, OutsideSVarState,
        !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
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
        Goal = true_goal
    ; lambda_args_contain_bang_state_var(Args0, StateVar) ->
        report_illegal_bang_svar_lambda_arg(Context, !.VarSet, StateVar,
            !Specs),
        Goal = true_goal
    ;
        some [!SVarState] (
            svar_prepare_for_lambda_head(Context, Args0, Args, FinalSVarMap,
                OutsideSVarState, !:SVarState, !VarSet, !Specs),
            InitialSVarState = !.SVarState,

            % Create fresh variables, transform the goal to HLDS, and
            % add unifications with the fresh variables. We use varset.new_vars
            % rather than make_fresh_arg_vars_subst_svars, since for functions
            % we need to ensure that the variable corresponding to the function
            % result term is a new variable, to avoid the function result term
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
                Context, ArgContext, HeadBefore0, HeadBefore,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),

            transform_goal_expr_context_to_goal(loc_whole_goal, ParsedGoal,
                Substitution, Body, !SVarState, !SVarStore,
                !VarSet, !ModuleInfo, !QualInfo, !Specs),

            % Create the unifications that need to come after the body of the
            % lambda expression; those corresponding to args whose mode is
            % output.
            HeadAfter0 = true_goal,
            insert_arg_unifications(OutputLambdaVars, OutputArgs,
                Context, ArgContext, HeadAfter0, HeadAfter,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),

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
                EvalMethod, LambdaNonLocals, LambdaVars, Modes, Det,
                HLDS_Goal),
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

:- pred arg_context_to_unify_context(arg_context::in, int::in,
    unify_main_context::out, unify_sub_contexts::out) is det.

arg_context_to_unify_context(ArgContext, ArgNum, MainContext, SubContexts) :-
    (
        ArgContext = ac_head(PredOrFunc, Arity),
        ( PredOrFunc = pf_function, ArgNum = Arity ->
            % It is the function result term in the head.
            MainContext = umc_head_result
        ;
            % It is a head argument.
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

make_fresh_arg_vars_subst_svars(Args, Vars, !VarSet, !SVarState, !Specs) :-
    % For efficiency, we construct `Vars' backwards and then reverse it
    % to get the correct order.
    make_fresh_arg_vars_subst_svars_loop(Args, [], RevVars,
        !VarSet, !SVarState, !Specs),
    list.reverse(RevVars, Vars).

:- pred make_fresh_arg_vars_subst_svars_loop(list(prog_term)::in,
    list(prog_var)::in, list(prog_var)::out,
    prog_varset::in,prog_varset::out, svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

make_fresh_arg_vars_subst_svars_loop([], !RevVars,
        !VarSet, !SVarState, !Specs).
make_fresh_arg_vars_subst_svars_loop([Arg | Args], !RevVars,
        !VarSet, !SVarState, !Specs) :-
    make_fresh_arg_var_subst_svars(Arg, Var, !.RevVars, !VarSet,
        !SVarState, !Specs),
    !:RevVars = [Var | !.RevVars],
    make_fresh_arg_vars_subst_svars_loop(Args, !RevVars, !VarSet,
        !SVarState, !Specs).

:- pred make_fresh_arg_var_subst_svars(prog_term::in, prog_var::out,
    list(prog_var)::in,
    prog_varset::in, prog_varset::out, svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

make_fresh_arg_var_subst_svars(Arg0, Var, Vars0, !VarSet, !SVarState,
        !Specs) :-
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

    % make_fresh_arg_vars_no_svar(Args, Vars, !VarSet):
    %
    % Does the same job as make_fresh_arg_vars, but assumes that any references
    % to state variables at the top level of Args have already been expanded.
    %
:- pred make_fresh_arg_vars_no_svar(list(prog_term)::in, list(prog_var)::out,
    prog_varset::in, prog_varset::out) is det.

make_fresh_arg_vars_no_svar(Args, Vars, !VarSet) :-
    % For efficiency, we construct `Vars' backwards and then reverse it
    % to get the correct order.
    make_fresh_arg_vars_no_svar_loop(Args, [], RevVars, !VarSet),
    list.reverse(RevVars, Vars).

:- pred make_fresh_arg_vars_no_svar_loop(list(prog_term)::in,
    list(prog_var)::in, list(prog_var)::out,
    prog_varset::in,prog_varset::out) is det.

make_fresh_arg_vars_no_svar_loop([], !RevVars, !VarSet).
make_fresh_arg_vars_no_svar_loop([Arg | Args], !RevVars, !VarSet) :-
    make_fresh_arg_var_no_svar(Arg, Var, !.RevVars, !VarSet),
    !:RevVars = [Var | !.RevVars],
    make_fresh_arg_vars_no_svar_loop(Args, !RevVars, !VarSet).

:- pred make_fresh_arg_var_no_svar(prog_term::in, prog_var::out,
    list(prog_var)::in, prog_varset::in, prog_varset::out) is det.

make_fresh_arg_var_no_svar(Arg, Var, Vars0, !VarSet) :-
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
