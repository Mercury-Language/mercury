%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% Copyright (C) 2014-2021 The Mercury team.
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
% XXX The code in this module should follow a consistent naming convention
% to distinguish
%
% - variables that refer to goals in the term we are parsing from
% - variables that refer to goals in the HLDS we are building.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.superhomogeneous.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.make_hlds.state_var.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type arg_context
    --->    ac_head(pred_or_func, pred_form_arity)
            % The arguments in the head of the clause.

    ;       ac_call(call_id)
            % The arguments in a call to a predicate.

    ;       ac_functor(            % The arguments in a functor.
                cons_id,
                unify_main_context,
                unify_sub_contexts
            ).

    % A variable and a term it is to be unified with.
:- type unify_var_term
    --->    unify_var_term(prog_var, prog_term).

    % A variable and a term it is to be unified with, and information
    % about where the unification is taking place: the argument number
    % in a call, and the context of that argument.
:- type unify_var_term_num_context
    --->    unify_var_term_num_context(prog_var, prog_term, int, arg_context).

:- func unify_var_term_project_var(unify_var_term) = prog_var.

:- pred pair_vars_with_terms(list(prog_var)::in, list(prog_term)::in,
    list(unify_var_term)::out) is det.

%-----------------------------------------------------------------------------%

    % `insert_arg_unifications' takes a list of variables, a list of terms
    % to unify them with, and a goal, and inserts the appropriate unifications
    % onto the front of the goal. It calls `unravel_unification' to ensure that
    % each unification gets reduced to superhomogeneous form. It also gets
    % passed an `arg_context', which indicates where the terms came from.
    %
    % We never insert unifications of the form X = X.
    %
    % XXX We should have versions of these predicates that take the variables
    % and the terms to unify them as a single assoc_list, instead of taking
    % them as two separate lists whose lengths must be equal, but may not be.
    %
:- pred insert_arg_unifications(list(unify_var_term)::in,
    prog_context::in, arg_context::in, hlds_goal::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred insert_arg_unifications_with_contexts(
    list(unify_var_term_num_context)::in,
    prog_context::in, hlds_goal::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred unravel_unification(prog_term::in, prog_term::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, purity::in, hlds_goal::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % make_fresh_arg_vars_subst_svars(Args, Vars, VarsArgs,
    %   !VarSet, !SVarState, !Specs):
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
    % state var references, and !Specs for incorrect state var references.
    %
:- pred make_fresh_arg_vars_subst_svars(list(prog_term)::in,
    list(prog_var)::out, list(unify_var_term)::out,
    prog_varset::in, prog_varset::out, svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.mode_util.
:- import_module hlds.from_ground_term_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.make_goal.
:- import_module hlds.make_hlds.field_access.
:- import_module hlds.make_hlds.goal_expr_to_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.  % for get_maybe_from_ground_term_threshold
:- import_module libs.options.  % for warn_suspected_occurs_check_failure
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.parse_dcg_goal.
:- import_module parse_tree.parse_goal.
:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_type_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_db.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module integer.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_vars.
:- import_module varset.

%-----------------------------------------------------------------------------%

unify_var_term_project_var(unify_var_term(Var, _)) = Var.

pair_vars_with_terms([], [], []).
pair_vars_with_terms([], [_ | _], _) :-
    unexpected($pred, "length mismatch").
pair_vars_with_terms([_ | _], [], _) :-
    unexpected($pred, "length mismatch").
pair_vars_with_terms([Var | Vars], [Term | Terms], [VarTerm | VarsTerms]) :-
    VarTerm = unify_var_term(Var, Term),
    pair_vars_with_terms(Vars, Terms, VarsTerms).

%-----------------------------------------------------------------------------%

insert_arg_unifications(XVarsArgTerms0, Context, ArgContext, Goal0, Goal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    substitute_state_var_mappings_unify_var_term(XVarsArgTerms0, XVarsArgTerms,
        !VarSet, !SVarState, !Specs),
    map.init(AncestorVarMap),
    do_arg_unifications(XVarsArgTerms, Context, ArgContext,
        construct_bottom_up, 1, AncestorVarMap, Expansions,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    Goal0 = hlds_goal(_, GoalInfo0),
    insert_expansions_before_goal_top_not_fgti(GoalInfo0, Expansions,
        Goal0, Goal).

insert_arg_unifications_with_contexts(XVarsArgTermsArgNumsContexts0,
        Context, Goal0, Goal,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    substitute_state_var_mappings_unify_var_term_num_context(
        XVarsArgTermsArgNumsContexts0, XVarsArgTermsArgNumsContexts,
        !VarSet, !SVarState, !Specs),
    map.init(AncestorVarMap),
    do_arg_unifications_with_contexts(XVarsArgTermsArgNumsContexts,
        Context, construct_bottom_up, AncestorVarMap, Expansions,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    Goal0 = hlds_goal(_, GoalInfo0),
    insert_expansions_before_goal_top_not_fgti(GoalInfo0, Expansions,
        Goal0, Goal).

%-----------------------------------------------------------------------------%

:- pred substitute_state_var_mappings_unify_var_term(
    list(unify_var_term)::in, list(unify_var_term)::out,
    prog_varset::in, prog_varset::out,
    svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

substitute_state_var_mappings_unify_var_term([], [], !VarSet, !State, !Specs).
substitute_state_var_mappings_unify_var_term([UVT0 | UVTs0], [UVT | UVTs],
        !VarSet, !State, !Specs) :-
    UVT0 = unify_var_term(Var, Arg0),
    substitute_state_var_mapping(Arg0, Arg, !VarSet, !State, !Specs),
    UVT = unify_var_term(Var, Arg),
    substitute_state_var_mappings_unify_var_term(UVTs0, UVTs,
        !VarSet, !State, !Specs).

:- pred substitute_state_var_mappings_unify_var_term_num_context(
    list(unify_var_term_num_context)::in,
        list(unify_var_term_num_context)::out,
    prog_varset::in, prog_varset::out,
    svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

substitute_state_var_mappings_unify_var_term_num_context([], [],
        !VarSet, !State, !Specs).
substitute_state_var_mappings_unify_var_term_num_context(
        [UVTNC0 | UVTNCs0], [UVTNC | UVTNCs], !VarSet, !State, !Specs) :-
    UVTNC0 = unify_var_term_num_context(Var, Arg0, ArgNum, ArgContext),
    substitute_state_var_mapping(Arg0, Arg, !VarSet, !State, !Specs),
    UVTNC = unify_var_term_num_context(Var, Arg, ArgNum, ArgContext),
    substitute_state_var_mappings_unify_var_term_num_context(UVTNCs0, UVTNCs,
        !VarSet, !State, !Specs).

%-----------------------------------------------------------------------------%

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

:- type ancestor_var_map == map(prog_var, prog_context).

%-----------------------------------------------------------------------------%

:- pred expansion_to_goal_wrap_if_fgti(hlds_goal_info::in, expansion::in,
    hlds_goal::out) is det.

expansion_to_goal_wrap_if_fgti(GoalInfo, Expansion, Goal) :-
    Expansion = expansion(MaybeFGTI, ExpansionGoalCord),
    ExpansionGoals = cord.list(ExpansionGoalCord),
    (
        ExpansionGoals = [],
        Goal = hlds_goal(true_goal_expr, GoalInfo)
    ;
        ExpansionGoals = [ExpansionGoal0],
        ExpansionGoal0 = hlds_goal(ExpansionGoalExpr, ExpansionGoalInfo0),
        Context = goal_info_get_context(GoalInfo),
        goal_info_set_context(Context, ExpansionGoalInfo0, ExpansionGoalInfo),
        Goal = hlds_goal(ExpansionGoalExpr, ExpansionGoalInfo)
    ;
        ExpansionGoals = [_, _ | _],
        ( if
            MaybeFGTI = fgti_var_size(TermVar, Size),
            get_maybe_from_ground_term_threshold = yes(Threshold),
            Size >= Threshold
        then
            goal_info_set_nonlocals(set_of_var.make_singleton(TermVar),
                GoalInfo, MarkedGoalInfo),
            mark_nonlocals_in_ground_term_initial(ExpansionGoals, MarkedGoals),
            ConjGoalExpr = conj(plain_conj, MarkedGoals),
            ConjGoal = hlds_goal(ConjGoalExpr, MarkedGoalInfo),
            Reason = from_ground_term(TermVar, from_ground_term_initial),
            GoalExpr = scope(Reason, ConjGoal),
            Goal = hlds_goal(GoalExpr, MarkedGoalInfo)
        else
            GoalExpr = conj(plain_conj, ExpansionGoals),
            Goal = hlds_goal(GoalExpr, GoalInfo)
        )
    ).

:- pred expansion_to_goal_cord_wrap_if_fgti(hlds_goal_info::in, expansion::in,
    cord(hlds_goal)::out) is det.

expansion_to_goal_cord_wrap_if_fgti(GoalInfo, Expansion,
        MaybeWrappedGoalCord) :-
    Expansion = expansion(MaybeFGTI, GoalCord),
    ( if
        MaybeFGTI = fgti_var_size(TermVar, Size),
        get_maybe_from_ground_term_threshold = yes(Threshold),
        Size >= Threshold
    then
        Goals = cord.list(GoalCord),
        goal_info_set_nonlocals(set_of_var.make_singleton(TermVar),
            GoalInfo, MarkedGoalInfo),
        mark_nonlocals_in_ground_term_initial(Goals, MarkedGoals),
        ConjGoalExpr = conj(plain_conj, MarkedGoals),
        ConjGoal = hlds_goal(ConjGoalExpr, MarkedGoalInfo),
        Reason = from_ground_term(TermVar, from_ground_term_initial),
        ScopeGoalExpr = scope(Reason, ConjGoal),
        ScopeGoal = hlds_goal(ScopeGoalExpr, MarkedGoalInfo),
        MaybeWrappedGoalCord = cord.singleton(ScopeGoal)
    else
        MaybeWrappedGoalCord = GoalCord
    ).

:- pred mark_nonlocals_in_ground_term_initial(
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

mark_nonlocals_in_ground_term_initial([], []).
mark_nonlocals_in_ground_term_initial([Goal0 | Goals0], [Goal | Goals]) :-
    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    ( if
        GoalExpr = unify(LHSVar, RHS, _, _, _),
        RHS = rhs_functor(_, _, RHSVars)
    then
        set_of_var.list_to_set([LHSVar | RHSVars], NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    else
        unexpected($pred, "wrong shape goal")
    ),
    mark_nonlocals_in_ground_term_initial(Goals0, Goals).

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

:- pred do_arg_unifications(list(unify_var_term)::in,
    prog_context::in, arg_context::in,
    goal_order::in, int::in, ancestor_var_map::in, list(expansion)::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_arg_unifications([], _Context, _ArgContext, _Order, _ArgNum,
        _AncestorVarMap, [],
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).
do_arg_unifications([unify_var_term(XVar, YTerm) | XVarsYTerms],
        Context, ArgContext, Order, ArgNum,
        AncestorVarMap, [Expansion | Expansions],
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    do_arg_unification(XVar, YTerm, Context, ArgContext, Order, ArgNum,
        AncestorVarMap, Expansion,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    do_arg_unifications(XVarsYTerms, Context, ArgContext, Order, ArgNum + 1,
        AncestorVarMap, Expansions,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).

:- pred do_arg_unifications_with_fresh_vars(list(prog_term)::in,
    prog_context::in, arg_context::in, goal_order::in, int::in,
    list(prog_var)::in, ancestor_var_map::in,
    list(prog_var)::out, list(expansion)::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_arg_unifications_with_fresh_vars([], _Context, _ArgContext,
        _Order, _ArgNum, _SeenXVars, _AncestorVarMap, [], [],
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).
do_arg_unifications_with_fresh_vars([YTerm | YTerms], Context, ArgContext,
        Order, ArgNum, !.SeenXVars, AncestorVarMap,
        [XVar | XVars], [Expansion | Expansions],
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    make_fresh_arg_var_no_svar(YTerm, XVar, !.SeenXVars, !VarSet),
    !:SeenXVars = [XVar | !.SeenXVars],
    do_arg_unification(XVar, YTerm, Context, ArgContext, Order,
        ArgNum, AncestorVarMap, Expansion,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    do_arg_unifications_with_fresh_vars(YTerms, Context, ArgContext, Order,
        ArgNum + 1, !.SeenXVars, AncestorVarMap, XVars, Expansions,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).

:- pred do_arg_unifications_with_contexts(list(unify_var_term_num_context)::in,
    prog_context::in, goal_order::in, ancestor_var_map::in,
    list(expansion)::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_arg_unifications_with_contexts([], _Context, _Order, _AncestorVarMap, [],
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).
do_arg_unifications_with_contexts(
        [HeadXVarYTermArgContext | TailXVarsYTermsArgContexts],
        Context, Order, AncestorVarMap, Expansions,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    HeadXVarYTermArgContext = unify_var_term_num_context(HeadXVar,
        HeadYTerm, HeadArgNumber, HeadArgContext),
    do_arg_unification(HeadXVar, HeadYTerm, Context, HeadArgContext, Order,
        HeadArgNumber, AncestorVarMap, HeadExpansion, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs),
    do_arg_unifications_with_contexts(TailXVarsYTermsArgContexts,
        Context, Order, AncestorVarMap, TailExpansions,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
    Expansions = [HeadExpansion | TailExpansions].

:- pred do_arg_unification(prog_var::in, prog_term::in,
    prog_context::in, arg_context::in,
    goal_order::in, int::in, ancestor_var_map::in, expansion::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_arg_unification(XVar, YTerm, Context, ArgContext, Order, ArgNum,
        !.AncestorVarMap, Expansion, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    % It is the caller's job to make sure that if needed, then both
    % XVar and the top level of YTerm have already been through
    % state var mapping expansion.
    occurs_check(!.ModuleInfo, !.VarSet, !.AncestorVarMap, XVar, !Specs),
    (
        YTerm = term.variable(YVar, YVarContext),
        ( if XVar = YVar then
            % Skip unifications of the form `XVar = XVar'.
            GoalCord = cord.init
        else
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
            Order, !.AncestorVarMap, Expansion, !SVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs)
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
        Context, MainContext, SubContext, Purity, Order, map.init, Expansion,
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
        Context, MainContext, SubContext, Purity, Order, map.init, Expansion,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs).

:- pred classify_unravel_unification(prog_term::in, prog_term::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    purity::in, goal_order::in, ancestor_var_map::in, expansion::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_unravel_unification(XTerm, YTerm, Context, MainContext, SubContext,
        Purity, Order, !.AncestorVarMap, Expansion, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
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
            Purity, Order, !.AncestorVarMap, Expansion, !SVarState, !SVarStore,
            !VarSet, !ModuleInfo, !QualInfo, !Specs)
    ;
        XTerm = term.functor(XFunctor, XArgTerms, XFunctorContext),
        YTerm = term.variable(YVar, _),
        unravel_var_functor_unification(YVar, XFunctor, XArgTerms,
            XFunctorContext, Context, MainContext, SubContext,
            Purity, Order, !.AncestorVarMap, Expansion, !SVarState, !SVarStore,
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
        % TmpVar cannot occur in either XTerm or YTerm, so adding it
        % to !AncestorVarMap would not result in any hits, and would only
        % slow down lookups.
        unravel_var_functor_unification(TmpVar, XFunctor, XArgTerms,
            XFunctorContext, Context, MainContext, SubContext,
            Purity, Order, !.AncestorVarMap, ExpansionX,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
        unravel_var_functor_unification(TmpVar, YFunctor, YArgTerms,
            YFunctorContext, Context, MainContext, SubContext,
            Purity, Order, !.AncestorVarMap, ExpansionY,
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
    purity::in, goal_order::in, ancestor_var_map::in, expansion::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_unravel_var_unification(XVar, YTerm, Context, MainContext, SubContext,
        Purity, Order, AncestorVarMap, Expansion, !SVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    (
        % `X = Y' needs no unravelling.
        YTerm = term.variable(YVar, _),
        occurs_check(!.ModuleInfo, !.VarSet, AncestorVarMap, YVar, !Specs),
        make_atomic_unification(XVar, rhs_var(YVar), Context, MainContext,
            SubContext, Purity, Goal, !QualInfo),
        Expansion = expansion(not_fgti, cord.singleton(Goal))
    ;
        YTerm = term.functor(YFunctor, YArgTerms, YFunctorContext),
        unravel_var_functor_unification(XVar, YFunctor, YArgTerms,
            YFunctorContext, Context, MainContext, SubContext,
            Purity, Order, AncestorVarMap, Expansion, !SVarState, !SVarStore,
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
    purity::in, goal_order::in, ancestor_var_map::in, expansion::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

unravel_var_functor_unification(XVar, YFunctor, YArgTerms0, YFunctorContext,
        Context, MainContext, SubContext,
        Purity, Order, !.AncestorVarMap, Expansion,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    substitute_state_var_mappings(YArgTerms0, YArgTerms, !VarSet,
        !SVarState, !Specs),
    ( if
        YFunctor = term.atom(YAtom),
        maybe_unravel_special_var_functor_unification(XVar, YAtom, YArgTerms,
            YFunctorContext, Context, MainContext, SubContext, Purity,
            Order, ExpansionPrime, !SVarState, !SVarStore, !VarSet,
            !ModuleInfo, !QualInfo, !Specs)
    then
        Expansion = ExpansionPrime
    else
        % Handle the usual case.
        ( if
            % The condition of this if-then-else is based on the logic of
            % try_parse_sym_name_and_args, but specialized to this location,
            % so that we can do state var expansion only if we need to.
            YFunctor = term.atom(YAtom),
            ( if
                YAtom = ".",
                YArgTerms = [ModuleNameTerm, NameArgsTerm]
            then
                NameArgsTerm = term.functor(term.atom(Name), NameArgTerms, _),
                try_parse_symbol_name(ModuleNameTerm, ModuleName),
                FunctorName = qualified(ModuleName, Name),
                % We have done state variable name expansion at the top
                % level of Args, but not at the level of NameArgTerms.
                substitute_state_var_mappings(NameArgTerms,
                    MaybeQualifiedYArgTermsPrime, !VarSet, !SVarState, !Specs)
            else
                FunctorName = string_to_sym_name_sep(YAtom, "__"),
                MaybeQualifiedYArgTermsPrime = YArgTerms
            )
        then
            MaybeQualifiedYArgTerms = MaybeQualifiedYArgTermsPrime,
            list.length(MaybeQualifiedYArgTerms, Arity),
            ConsId = cons(FunctorName, Arity, cons_id_dummy_type_ctor)
        else
            % If YFunctor is a numeric or string constant, it *should*
            % have no arguments. If it nevertheless does, we still record
            % its arguments, and let the error be caught later during
            % typechecking.
            parse_ordinary_cons_id(!.VarSet, YFunctor, YArgTerms,
                YFunctorContext, ConsId, !Specs),
            MaybeQualifiedYArgTerms = YArgTerms
        ),
        build_var_cons_id_unification(XVar, ConsId, MaybeQualifiedYArgTerms,
            YFunctorContext, Context, MainContext, SubContext, Purity,
            !.AncestorVarMap, Expansion,
            !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs)
    ).

:- pred build_var_cons_id_unification(prog_var::in, cons_id::in,
    list(prog_term)::in, term.context::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, purity::in,
    ancestor_var_map::in, expansion::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_var_cons_id_unification(XVar, ConsId, MaybeQualifiedYArgTerms,
        YFunctorContext, Context, MainContext, SubContext, Purity,
        !.AncestorVarMap, Expansion,
        !SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    % Our caller has done state variable name expansion
    % at the top level of MaybeQualifiedYArgTerms.
    (
        MaybeQualifiedYArgTerms = [],
        RHS = rhs_functor(ConsId, is_not_exist_constr, []),
        make_atomic_unification(XVar, RHS, YFunctorContext,
            MainContext, SubContext, Purity, FunctorGoal, !QualInfo),
        goal_set_purity(Purity, FunctorGoal, Goal),
        Expansion = expansion(fgti_var_size(XVar, 1), cord.singleton(Goal))
    ;
        MaybeQualifiedYArgTerms = [_ | _],
        ArgContext = ac_functor(ConsId, MainContext, SubContext),
        maybe_add_to_ancestor_var_map(!.ModuleInfo, XVar, ConsId, Context,
            !AncestorVarMap),
        (
            Purity = purity_pure,
            % If we can, we want to add the unifications for the arguments
            % AFTER the unification of the top level function symbol,
            % because otherwise we get efficiency problems during
            % type-checking.
            do_arg_unifications_with_fresh_vars(MaybeQualifiedYArgTerms,
                YFunctorContext, ArgContext, deconstruct_top_down, 1,
                [], !.AncestorVarMap, YVars, ArgExpansions,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            RHS = rhs_functor(ConsId, is_not_exist_constr, YVars),
            make_atomic_unification(XVar, RHS, YFunctorContext,
                MainContext, SubContext, Purity, FunctorGoal, !QualInfo),
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
                [], !.AncestorVarMap, YVars, ArgExpansions,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),
            RHS = rhs_functor(ConsId, is_not_exist_constr, YVars),
            make_atomic_unification(XVar, RHS, YFunctorContext,
                MainContext, SubContext, Purity, FunctorGoal, !QualInfo),
            goal_info_init(Context, GoalInfo),
            insert_expansions_before_goal_top_not_fgti(GoalInfo,
                ArgExpansions, FunctorGoal, Goal0),
            goal_set_purity(Purity, Goal0, Goal),
            Expansion = expansion(not_fgti, cord.singleton(Goal))
        )
    ).

    % Add the variable on the left side of the var-functor unification
    % XVar = ConsId(...) to the ancestor var map *if* it can be part of
    % an occurs check violation we want to report.
    %
    % - The occurs check cannot be violated if ConsId is a constant.
    %
    % - If ConsId cannot actually be a data constructor, then this unification
    %   cannot be part of an occurs check violation we want to report.
    %   There are four possibilities:
    %
    %   1 ConsId(...) is a full application of a function, which returns
    %     a piece of data. Even if XVar occurs somewhere inside the
    %     arguments of ConsId, checking whether XVar is equal to the
    %     value computed from it is a perfectly legitimate test.
    %
    %   2 ConsId(...) is a partial application of a function or a predicate,
    %     and XVar's type is the higher order type matching the type
    %     of this closure. This case *would* be a perfectly legitimate
    %     equality test like case 1, were it not for the fact that unification
    %     of higher order values is not allowed (because it is an undecidable
    %     problem). This should therefore be detected as a type error.
    %
    %   3 ConsId(...) is a partial application of a function or a predicate,
    %     and XVar's type is not the higher order type matching the type
    %     of this closure. This is a more straightforward type error.
    %
    %   4 ConsId is not a function or a predicate. This is a straightforward
    %     "unknown function symbol" error.
    %
    %   In case 1, any warning about occurs check violation would be
    %   misleading. In cases 2, 3 and 4, it would be redundant, since they
    %   all involve an error which is not really about the occurs check.
    %
:- pred maybe_add_to_ancestor_var_map(module_info::in, prog_var::in,
    cons_id::in, prog_context::in,
    ancestor_var_map::in, ancestor_var_map::out) is det.

maybe_add_to_ancestor_var_map(ModuleInfo, XVar, ConsId, Context,
        !AncestorVarMap) :-
    ( if
        % The only two kinds of cons_ids that may (a) appear in user
        % written code, as opposed to compiler-generated code, and
        % (b) may have nonzero arities, are cons and tuple_cons.
        % However, the cons_ids of tuples are represented by tuple_cons
        % only *after* resolve_unify_functor.m has been run as part of
        % the post_typecheck pass. Until then, they have the form
        % recognized by the second disjunct.
        ConsId = cons(SymName, Arity, _TypeCtor),
        Arity > 0,
        (
            module_info_get_cons_table(ModuleInfo, ConsTable),
            is_known_data_cons(ConsTable, ConsId)
        ;
            SymName = unqualified("{}")
        )
    then
        map.search_insert(XVar, Context, _OldContext, !AncestorVarMap)
    else
        true
    ).

:- pred parse_ordinary_cons_id(prog_varset::in, term.const::in,
    list(prog_term)::in, term.context::in, cons_id::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_ordinary_cons_id(VarSet, Functor, ArgTerms, Context, ConsId, !Specs) :-
    (
        Functor = term.atom(Name),
        list.length(ArgTerms, Arity),
        ConsId = cons(unqualified(Name), Arity, cons_id_dummy_type_ctor)
    ;
        Functor = term.integer(Base, Integer, Signedness, Size),
%       expect(unify(ArgTerms, []), $pred,
%           "parse_simple_term has given an integer arguments"),
        parse_integer_cons_id(Base, Integer, Signedness, Size, Context,
            MaybeConsId),
        (
            MaybeConsId = ok1(ConsId)
        ;
            MaybeConsId = error1(ConsIdSpecs),
            % This is a dummy.
            ConsId = some_int_const(int_const(0)),
            !:Specs = ConsIdSpecs ++ !.Specs
        )
    ;
        Functor = term.float(Float),
%       expect(unify(ArgTerms, []), $pred,
%           "parse_simple_term has given a float arguments"),
        ConsId = float_const(Float)
    ;
        Functor = term.string(String),
%       expect(unify(ArgTerms, []), $pred,
%           "parse_simple_term has given a string arguments"),
        ConsId = string_const(String)
    ;
        Functor = term.implementation_defined(Name),
%       expect(unify(ArgTerms, []), $pred,
%           "parse_simple_term has given an implementation_defined arguments"),
        ( if
            ( Name = "line",   IDCKind = idc_line
            ; Name = "file",   IDCKind = idc_file
            ; Name = "module", IDCKind = idc_module
            ; Name = "pred",   IDCKind = idc_pred
            ; Name = "grade",  IDCKind = idc_grade
            )
        then
            ConsId = impl_defined_const(IDCKind)
        else
            ErrorTerm = functor(Functor, ArgTerms, Context),
            TermStr = describe_error_term(VarSet, ErrorTerm),
            Pieces = [words("Error:"),
                words("unexpected implementation defined literal"),
                quote(TermStr), suffix("."), nl,
                words("The only valid implementation defined literals are"),
                quote("$line"), suffix(","), quote("$file"), suffix(","),
                quote("$module"), suffix(","), quote("$pred"), words("and"),
                quote("$grade"), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
            !:Specs = [Spec | !.Specs],
            % This is a dummy.
            ConsId = impl_defined_const(idc_line)
        )
    ).

    % See whether YAtom indicates a term with special syntax.
    %
:- pred maybe_unravel_special_var_functor_unification(prog_var::in,
    string::in, list(prog_term)::in, term.context::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    purity::in, goal_order::in, expansion::out,
    svar_state::in, svar_state::out, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is semidet.

maybe_unravel_special_var_functor_unification(XVar, YAtom, YArgTerms,
        YFunctorContext, Context, MainContext, SubContext, Purity, Order,
        Expansion, !SVarState, !SVarStore, !VarSet,
        !ModuleInfo, !QualInfo, !Specs)  :-
    % Switch on YAtom.
    % We cannot require each of the switch arms to be det. One reason is that
    % some of the keywords we are looking for contain just one character,
    % and *could* be a reference to a character constant (since the lexer
    % doesn't distinguish between seeing e.g. ':' with and without the quotes).
    % However, each arm should wrap a require_det scope around all the goals
    % that we execute when we have decided that the term we are parsing *is*
    % in fact supposed to be the construct we are looking for.
    (
        % Handle explicit type qualification.
        ( YAtom = "with_type"
        ; YAtom = ":"
        ),
        ( if YArgTerms = [RValTerm, DeclTypeTerm0] then
            require_det (
                % DeclType0 is a prog_term, but it is really a type,
                % so we coerce it to a generic term before parsing it.
                term.coerce(DeclTypeTerm0, DeclTypeTerm1),
                ContextPieces =
                    cord.singleton(words("In explicit type qualification:")),
                varset.coerce(!.VarSet, GenericVarSet),
                parse_type(no_allow_ho_inst_info(wnhii_type_qual),
                    GenericVarSet, ContextPieces, DeclTypeTerm1,
                    DeclTypeResult),
                (
                    DeclTypeResult = ok1(DeclType),
                    varset.coerce(!.VarSet, DeclVarSet),
                    process_type_qualification(XVar, DeclType, DeclVarSet,
                        YFunctorContext, !ModuleInfo, !QualInfo, !Specs)
                ;
                    DeclTypeResult = error1(DeclTypeSpecs),
                    % The varset is a prog_varset even though it contains
                    % the names of type variables in ErrorTerm, which is
                    % a generic term.
                    !:Specs = DeclTypeSpecs ++ !.Specs
                ),
                do_unravel_var_unification(XVar, RValTerm,
                    Context, MainContext, SubContext, Purity, Order, Expansion,
                    !SVarState, !SVarStore, !VarSet,
                    !ModuleInfo, !QualInfo, !Specs)
            )
        else if YAtom = ":", YArgTerms = [] then
            % This may be the character ':'.
            fail
        else
            % The code below is disabled, as per the discussion on m-rev
            % that started on 2016 may 5.
            fail
%           Pieces = [words("Error: the type qualification operator"),
%               quote(YAtom), words("can be used only in expressions"),
%               words("of the form"), quote("<term> " ++ YAtom ++ " <type>"),
%               suffix("."), nl],
%           Msg = simple_msg(YFunctorContext, [always(Pieces)]),
%           Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
%           !:Specs = [Spec | !.Specs],
%           qual_info_set_found_syntax_error(yes, !QualInfo),
%           Expansion = expansion(not_fgti, cord.empty)
        )
    ;
        % Handle unification expressions.
        YAtom = "@",
        (
            YArgTerms = [],
            % This may be the character '@'.
            fail
        ;
            YArgTerms = [LVal, RVal],
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
            ( YArgTerms = [_]
            ; YArgTerms = [_, _, _ | _]
            ),
            % The code below is disabled, as per the discussion on m-rev
            % that started on 2016 may 5.
            fail
%           Pieces = [words("Error: the unification expression operator"),
%               quote(YAtom), words("can be used only in expressions"),
%               words("of the form"), quote("<term> " ++ YAtom ++ " <term>"),
%               suffix("."), nl],
%           Msg = simple_msg(YFunctorContext, [always(Pieces)]),
%           Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
%           !:Specs = [Spec | !.Specs],
%           qual_info_set_found_syntax_error(yes, !QualInfo),
%           Expansion = expansion(not_fgti, cord.empty)
        )
    ;
        % Handle coerce expressions.
        YAtom = "coerce",
        YArgTerms = [RValTerm0],
        require_det (
            (
                RValTerm0 = term.variable(RValTermVar, _),
                RValGoalCord = cord.empty
            ;
                RValTerm0 = term.functor(_, _, _),
                substitute_state_var_mapping(RValTerm0, RValTerm,
                    !VarSet, !SVarState, !Specs),
                make_fresh_arg_var_no_svar(RValTerm0, RValTermVar, [],
                    !VarSet),
                do_unravel_var_unification(RValTermVar, RValTerm, Context,
                    MainContext, SubContext, Purity, Order, RValTermExpansion,
                    !SVarState, !SVarStore, !VarSet,
                    !ModuleInfo, !QualInfo, !Specs),
                RValTermExpansion = expansion(_, RValGoalCord)
            ),
            CoerceGoalExpr = generic_call(cast(subtype_coerce),
                [RValTermVar, XVar], [in_mode, out_mode],
                arg_reg_types_unset, detism_det),
            goal_info_init(Context, CoerceGoalInfo),
            CoerceGoal = hlds_goal(CoerceGoalExpr, CoerceGoalInfo),
            CoerceGoalCord = cord.singleton(CoerceGoal),
            Expansion = expansion(not_fgti, RValGoalCord ++ CoerceGoalCord)
        )
    ;
        % Handle if-then-else expressions.
        (
            YAtom = "else",
            YArgTerms = [CondThenTerm0, ElseTerm0],
            CondThenTerm0 = term.functor(term.atom("if"),
                [term.functor(term.atom("then"), [CondTerm0, ThenTerm0], _)],
                    _)
        ;
            YAtom = ";",
            YArgTerms = [CondThenTerm0, ElseTerm0],
            CondThenTerm0 = term.functor(term.atom("->"),
                [CondTerm0, ThenTerm0], _)
        ),

        require_det (
            term.coerce(CondTerm0, CondTerm),
            ContextPieces = cord.init,
            parse_some_vars_goal(CondTerm, ContextPieces, MaybeVarsCond,
                !VarSet),
            (
                MaybeVarsCond =
                    ok4(Vars, StateVars, CondParseTree, CondWarningSpecs),
                !:Specs = CondWarningSpecs ++ !.Specs,
                BeforeSVarState = !.SVarState,
                svar_prepare_for_local_state_vars(Context, !.VarSet, StateVars,
                    BeforeSVarState, BeforeInsideSVarState, !Specs),
                map.init(EmptySubst),

                transform_parse_tree_goal_to_hlds(loc_inside_atomic_goal,
                    CondParseTree, EmptySubst, CondGoal,
                    BeforeInsideSVarState, AfterCondInsideSVarState,
                    !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),

                substitute_state_var_mapping(ThenTerm0, ThenTerm, !VarSet,
                    AfterCondInsideSVarState, AfterThenInsideSVarState0,
                    !Specs),
                map.init(AncestorVarMap),
                classify_unravel_var_unification(XVar, ThenTerm,
                    Context, MainContext, SubContext,
                    Purity, Order, AncestorVarMap, ThenExpansion,
                    AfterThenInsideSVarState0, AfterThenInsideSVarState,
                    !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
                goal_info_init(get_term_context(ThenTerm), ThenGoalInfo),
                expansion_to_goal_wrap_if_fgti(ThenGoalInfo,
                    ThenExpansion, ThenGoal0),

                module_info_get_globals(!.ModuleInfo, Globals),
                module_info_get_name(!.ModuleInfo, ModuleName),
                svar_finish_local_state_vars(Globals, ModuleName, StateVars,
                    BeforeSVarState, AfterThenInsideSVarState,
                    AfterThenSVarState),

                substitute_state_var_mapping(ElseTerm0, ElseTerm, !VarSet,
                    BeforeSVarState, AfterElseSVarState0, !Specs),
                classify_unravel_var_unification(XVar, ElseTerm,
                    Context, MainContext, SubContext,
                    Purity, Order, AncestorVarMap, ElseExpansion,
                    AfterElseSVarState0, AfterElseSVarState,
                    !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs),
                goal_info_init(get_term_context(ElseTerm), ElseGoalInfo),
                expansion_to_goal_wrap_if_fgti(ElseGoalInfo,
                    ElseExpansion, ElseGoal0),

                svar_finish_if_then_else(Globals, ModuleName,
                    loc_inside_atomic_goal, Context, StateVars,
                    ThenGoal0, ThenGoal, ElseGoal0, ElseGoal,
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
                MaybeVarsCond = error4(VarsCondSpecs),
                !:Specs = VarsCondSpecs ++ !.Specs,
                Expansion = expansion(not_fgti,
                    cord.singleton(true_goal_with_context(Context)))
            )
        )
    ;
        % Handle field extraction expressions.
        YAtom = "^",
        (
            YArgTerms = [],
            % This may be the character '^'.
            fail
        ;
            YArgTerms = [InputTerm0, FieldNameTerm],
            FieldNameContextPieces = [words("On the right hand side"),
                words("of the"), quote("^"), words("operator"),
                words("in a field selection expression:")],
            parse_field_list(FieldNameTerm, !.VarSet,
                FieldNameContextPieces, MaybeFieldNames),
            (
                MaybeFieldNames = ok1(FieldNames),
                require_det (
                    substitute_state_var_mapping(InputTerm0, InputTerm,
                        !VarSet, !SVarState, !Specs),
                    make_fresh_arg_var_no_svar(InputTerm, InputTermVar, [],
                        !VarSet),
                    expand_get_field_function_call(Context, MainContext,
                        SubContext, FieldNames, XVar, InputTermVar, Purity,
                        Functor, _, GetGoal, !SVarState, !SVarStore, !VarSet,
                        !ModuleInfo, !QualInfo, !Specs),

                    ArgContext = ac_functor(Functor, MainContext, SubContext),
                    map.init(AncestorVarMap),
                    do_arg_unification(InputTermVar, InputTerm,
                        YFunctorContext, ArgContext, Order,
                        1, AncestorVarMap, InputArgExpansion,
                        !SVarState, !SVarStore, !VarSet,
                        !ModuleInfo, !QualInfo, !Specs),
                    goal_info_init(Context, GoalInfo),
                    insert_expansion_before_goal_top_not_fgti(GoalInfo,
                        InputArgExpansion, GetGoal, Goal),
                    Expansion = expansion(not_fgti, cord.singleton(Goal))
                )
            ;
                MaybeFieldNames = error1(_FieldNamesSpecs),
                % The code below is disabled, as per the discussion
                % on m-rev that started on 2016 may 5.
                fail
%               !:Specs = FieldNamesSpecs ++ !.Specs,
%               qual_info_set_found_syntax_error(yes, !QualInfo),
%               Expansion = expansion(not_fgti, cord.empty)
            )
        ;
            ( YArgTerms = [_]
            ; YArgTerms = [_, _, _ | _]
            ),
            % The code below is disabled, as per the discussion on m-rev
            % that started on 2016 may 5.
            fail
%           Pieces = [words("Error: the field access operator"), quote(YAtom),
%               words("can be used only in expressions of the form"),
%               quote("<term> ^ <fieldname>"), suffix("."), nl],
%           Msg = simple_msg(YFunctorContext, [always(Pieces)]),
%           Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
%           !:Specs = [Spec | !.Specs],
%           qual_info_set_found_syntax_error(yes, !QualInfo),
%           Expansion = expansion(not_fgti, cord.empty)
        )
    ;
        % Handle field update expressions.
        YAtom = ":=",
        ( if
            YArgTerms = [FieldDescrTerm, FieldValueTerm0],
            FieldDescrTerm = term.functor(term.atom("^"),
                [InputTerm0, FieldNameTerm], _)
        then
            FieldNameContextPieces = [words("On the right hand side"),
                words("of the"), quote("^"), words("operator"),
                words("in a field update expression:")],
            parse_field_list(FieldNameTerm, !.VarSet,
                FieldNameContextPieces, MaybeFieldNames),
            (
                MaybeFieldNames = ok1(FieldNames),
                require_det (
                    substitute_state_var_mapping(InputTerm0, InputTerm,
                        !VarSet, !SVarState, !Specs),
                    make_fresh_arg_var_no_svar(InputTerm, InputTermVar, [],
                        !VarSet),
                    substitute_state_var_mapping(FieldValueTerm0,
                        FieldValueTerm, !VarSet, !SVarState, !Specs),
                    make_fresh_arg_var_no_svar(FieldValueTerm, FieldValueVar,
                        [InputTermVar], !VarSet),

                    expand_set_field_function_call(Context, MainContext,
                        SubContext, FieldNames, FieldValueVar,
                        InputTermVar, XVar,
                        Functor, InnerFunctor - FieldSubContext, SetGoal,
                        !SVarState, !SVarStore, !VarSet,
                        !ModuleInfo, !QualInfo, !Specs),

                    TermArgNumber = 1,
                    TermArgContext = ac_functor(Functor,
                        MainContext, SubContext),
                    InputVTNC = unify_var_term_num_context(InputTermVar,
                        InputTerm, TermArgNumber, TermArgContext),

                    FieldArgNumber = 2,
                    FieldArgContext = ac_functor(InnerFunctor, MainContext,
                        FieldSubContext),
                    FieldVTNC = unify_var_term_num_context(FieldValueVar,
                        FieldValueTerm, FieldArgNumber, FieldArgContext),

                    map.init(AncestorVarMap),
                    do_arg_unifications_with_contexts([InputVTNC, FieldVTNC],
                        Context, Order, AncestorVarMap,
                        InputFieldArgExpansions,
                        !SVarState, !SVarStore, !VarSet,
                        !ModuleInfo, !QualInfo, !Specs),

                    goal_info_init(Context, GoalInfo),
                    insert_expansions_before_goal_top_not_fgti(GoalInfo,
                        InputFieldArgExpansions, SetGoal, Goal),
                    Expansion = expansion(not_fgti, cord.singleton(Goal))
                )
            ;
                MaybeFieldNames = error1(_FieldNamesSpecs),
                % The code below is disabled, as per the discussion
                % on m-rev that started on 2016 may 5.
                fail
%               !:Specs = FieldNamesSpecs ++ !.Specs,
%               qual_info_set_found_syntax_error(yes, !QualInfo),
%               Expansion = expansion(not_fgti, cord.empty)
            )
        else
            % The code below is disabled, as per the discussion on m-rev
            % that started on 2016 may 5.
            fail
%           Pieces = [words("Error: the field update operator"), quote(YAtom),
%               words("can be used only in expressions of the form"),
%               quote("<term> ^ <fieldname> := <newfieldvalueterm>"),
%               suffix("."), nl],
%           Msg = simple_msg(YFunctorContext, [always(Pieces)]),
%           Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
%           !:Specs = [Spec | !.Specs],
%           qual_info_set_found_syntax_error(yes, !QualInfo),
%           Expansion = expansion(not_fgti, cord.empty)
        )
    ;
        (
            YAtom = ":-",
            LambdaBodyKind = lambda_body_ordinary
        ;
            YAtom = "-->",
            LambdaBodyKind = lambda_body_dcg
        ),
        % A lambda expression with a body goal. It may or may not have purity
        % marker.
        ( if YArgTerms = [PurityPFArgsDetTerm, BodyGoalTerm] then
            require_det(
                parse_lambda_expr(XVar, Purity,
                    Context, MainContext, SubContext,
                    PurityPFArgsDetTerm, yes({LambdaBodyKind, BodyGoalTerm}),
                    Expansion, !.SVarState, !SVarStore, !VarSet,
                    !ModuleInfo, !QualInfo, !Specs)
            )
        else
            HeadForm = "<lambda expression head> ",
            BodyForm = " <lambda expression body>",
            Form = HeadForm ++ YAtom ++ BodyForm,
            Pieces = [words("Error: the clause neck operator"), quote(YAtom),
                words("can be used only in expressions of the form"),
                quote(Form), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, YFunctorContext, Pieces),
            !:Specs = [Spec | !.Specs],
            qual_info_set_found_syntax_error(yes, !QualInfo),
            Expansion = expansion(not_fgti, cord.empty)
        )
%   ;
%       ( YAtom = "impure"
%       ; YAtom = "semipure"
%       ),
%       % This could be a lambda expression without a body goal
%       % but with a purity marker. However, since it could also be
%       % a marker in front of an ordinary, non-lambda unification,
%       % we cannot insist on parsing it as a language expression.
    ;
        YAtom = "is",
        % A lambda expression without a body goal or a purity marker,
        % but with a declared determinism.
        require_det (
            YTerm = term.functor(term.atom(YAtom), YArgTerms, YFunctorContext),
            parse_lambda_expr(XVar, Purity, Context, MainContext, SubContext,
                YTerm, no, Expansion, !.SVarState, !SVarStore,
                !VarSet, !ModuleInfo, !QualInfo, !Specs)
        )
    ;
        YAtom = "=",
        % A lambda expression without a body goal or a purity marker,
        % and without a declared determinism. This can happen only if
        % the lambda expression is a function, in which case its top functor
        % will be "=", and the top functor of the left operand of the "="
        % will be "func" or "any_func". (If it isn't, then we are looking at
        % a plain old unification that does NOT involve a lambda expression.)
        ( if
            YArgTerms = [FuncArgsTerm, _ReturnArgModeTerm],
            FuncArgsTerm = term.functor(term.atom(FuncTermFunctor), _, _),
            ( FuncTermFunctor = "func"
            ; FuncTermFunctor = "any_func"
            )
        then
            require_det (
                YTerm = term.functor(term.atom(YAtom), YArgTerms,
                    YFunctorContext),
                parse_lambda_expr(XVar, Purity,
                    Context, MainContext, SubContext, YTerm, no, Expansion,
                    !.SVarState, !SVarStore, !VarSet,
                    !ModuleInfo, !QualInfo, !Specs)
            )
        else
            fail
        )
    ).

%---------------------------------------------------------------------------%
%
% Code for parsing pred/func expressions.
%

:- type lambda_body_kind
    --->    lambda_body_ordinary
    ;       lambda_body_dcg.

:- pred parse_lambda_expr(prog_var::in, purity::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    prog_term::in, maybe({lambda_body_kind, prog_term})::in, expansion::out,
    svar_state::in, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_lambda_expr(XVar, Purity, Context, MainContext, SubContext,
        PurityPFArgsDetTerm, MaybeLambdaBody, Expansion,
        !.SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
    (
        MaybeLambdaBody = no,
        TrueGoal = true_expr(Context),
        MaybeBodyGoal = ok1(TrueGoal),
        MaybeDCGVars = no_dcg_vars
    ;
        MaybeLambdaBody = yes({LambdaBodyKind, BodyGoalTerm}),
        ContextPieces = cord.singleton(
            words("In the body of lambda expression:")),
        term.coerce(BodyGoalTerm, GenericBodyGoalTerm),
        (
            LambdaBodyKind = lambda_body_ordinary,
            parse_goal(GenericBodyGoalTerm, ContextPieces,
                MaybeBodyGoal0, !VarSet),
            MaybeDCGVars = no_dcg_vars
        ;
            LambdaBodyKind = lambda_body_dcg,
            parse_dcg_pred_goal(GenericBodyGoalTerm, ContextPieces,
                MaybeBodyGoal0, DCGVar0, DCGVarN, !VarSet),
            MaybeDCGVars = dcg_vars(DCGVar0, DCGVarN)
        ),
        (
            MaybeBodyGoal0 = ok2(BodyGoal, BodyGoalWarningSpecs),
            !:Specs = BodyGoalWarningSpecs ++ !.Specs,
            MaybeBodyGoal = ok1(BodyGoal)
        ;
            MaybeBodyGoal0 = error2(BodyGoalSpecs),
            MaybeBodyGoal = error1(BodyGoalSpecs)
        )
    ),
    parse_lambda_purity_pf_args_det_term(PurityPFArgsDetTerm, MaybeDCGVars,
        MaybeLambdaHead, !VarSet, !QualInfo),
    (
        MaybeLambdaHead = error1(LambdaHeadSpecs),
        !:Specs = LambdaHeadSpecs ++ !.Specs,
        qual_info_set_found_syntax_error(yes, !QualInfo),
        Expansion = expansion(not_fgti, cord.empty)
    ;
        MaybeLambdaHead = ok1(LambdaHead),
        build_lambda_expression(XVar, Purity, Context, MainContext, SubContext,
            LambdaHead, MaybeBodyGoal, Expansion,
            !.SVarState, !SVarStore, !VarSet, !ModuleInfo, !QualInfo, !Specs)
    ).

:- type maybe_dcg_vars
    --->    no_dcg_vars
    ;       dcg_vars(prog_var, prog_var).

:- pred parse_lambda_purity_pf_args_det_term(prog_term::in, maybe_dcg_vars::in,
    maybe1(lambda_head)::out, prog_varset::in, prog_varset::out,
    qual_info::in, qual_info::out) is det.

parse_lambda_purity_pf_args_det_term(PurityPFArgsDetTerm, MaybeDCGVars,
        MaybeLambdaHead, !VarSet, !QualInfo) :-
    term.coerce(PurityPFArgsDetTerm, GenericPurityPFArgsDetTerm),
    parse_purity_annotation(GenericPurityPFArgsDetTerm, LambdaPurity,
        PFArgsDetTerm),

%   A summary of the term structures that the two conditions of the nested
%   if-then-else below look for:
%
%   (
%       % Condition 1p:
%       PFArgsDetTerm = is(BeforeIsTerm, DetismTerm),
%       ( BeforeIsTerm = pred(...) ; BeforeIsTerm = any_pred(...) )
%   ;
%       % Condition 1f:
%       PFArgsDetTerm = is(BeforeIsTerm, DetismTerm),
%       BeforeIsTerm = "="(FuncArgsTerm, FuncRetTerm),
%       ( FuncArgsTerm = func(...) ; FuncArgsTerm = any_func(...) )
%   ;
%       % Condition 2f:
%       PFArgsDetTerm = "="(FuncArgsTerm, FuncRetTerm),
%       ( FuncArgsTerm = func(...) ; FuncArgsTerm = any_func(...) )
%   )

    ( if
        PFArgsDetTerm = term.functor(term.atom("is"),
            [BeforeIsTerm, DetismTerm], _),
        BeforeIsTerm = term.functor(term.atom(BeforeIsFunctor),
            BeforeIsArgTerms, Context),
        (
            % Condition 1p.
            (
                BeforeIsFunctor = "pred",
                Groundness = ho_ground
            ;
                BeforeIsFunctor = "any_pred",
                Groundness = ho_any
            ),
            ArgModeTerms0 = BeforeIsArgTerms,
            MaybeFuncRetArgModeTerm = no
        ;
            % Condition 1f.
            BeforeIsFunctor = "=",
            BeforeIsArgTerms = [FuncArgsTerm, FuncRetArgModeTerm0],
            FuncArgsTerm = term.functor(term.atom(FuncTermFunctor),
                ArgModeTerms0, _),
            (
                FuncTermFunctor = "func",
                Groundness = ho_ground
            ;
                FuncTermFunctor = "any_func",
                Groundness = ho_any
            ),
            MaybeFuncRetArgModeTerm = yes(FuncRetArgModeTerm0)
        )
    then
        parse_lambda_detism(!.VarSet, DetismTerm, MaybeDetism),
        (
            MaybeFuncRetArgModeTerm = no,
            PredOrFunc = pf_predicate,
            (
                MaybeDCGVars = no_dcg_vars,
                ArgModeTerms = ArgModeTerms0,
                parse_lambda_args_pred(Context, ArgModeTerms,
                    LambdaArgs, !VarSet, BadModeSpecs, SVarSpecs),
                LambdaHead = lambda_head(LambdaPurity, Groundness,
                    PredOrFunc, lambda_normal, LambdaArgs,
                    BadModeSpecs, SVarSpecs, MaybeDetism),
                MaybeLambdaHead = ok1(LambdaHead)
            ;
                MaybeDCGVars = dcg_vars(DCGVar0, DCGVarN),
                (
                    ( ArgModeTerms0 = []
                    ; ArgModeTerms0 = [_]
                    ),
                    Pieces = [words("Error: the head of a lambda expression"),
                        words("that is defined by a DCG clause"),
                        words("must have at least arguments."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_parse_tree_to_hlds, Context, Pieces),
                    MaybeLambdaHead =
                        error1([Spec | get_any_errors1(MaybeDetism)])
                ;
                    ArgModeTerms0 =
                        [ArgModeTerm1, ArgModeTerm2 | ArgModeTerms3plus],
                    split_last_two(
                        ArgModeTerm1, ArgModeTerm2, ArgModeTerms3plus,
                        NonDCGArgModeTerms, DCGModeTerm0, DCGModeTermN),
                    DCGContext0 = get_term_context(DCGModeTerm0),
                    DCGContextN = get_term_context(DCGModeTermN),
                    DCGVarTerm0 = term.variable(DCGVar0, DCGContext0),
                    DCGVarTermN = term.variable(DCGVarN, DCGContextN),
                    term.coerce(DCGVarTerm0, GenericDCGVarTerm0),
                    term.coerce(DCGVarTermN, GenericDCGVarTermN),
                    DCGArgModeTerm0 = term.functor(term.atom("::"),
                        [GenericDCGVarTerm0, DCGModeTerm0], DCGContext0),
                    DCGArgModeTermN = term.functor(term.atom("::"),
                        [GenericDCGVarTermN, DCGModeTermN], DCGContextN),
                    ArgModeTerms = NonDCGArgModeTerms ++
                        [DCGArgModeTerm0, DCGArgModeTermN],
                    parse_lambda_args_pred(Context, ArgModeTerms,
                        LambdaArgs, !VarSet, BadModeSpecs, SVarSpecs),
                    LambdaHead = lambda_head(LambdaPurity, Groundness,
                        PredOrFunc, lambda_normal, LambdaArgs,
                        BadModeSpecs, SVarSpecs, MaybeDetism),
                    MaybeLambdaHead = ok1(LambdaHead)
                )
            )
        ;
            MaybeFuncRetArgModeTerm = yes(FuncRetArgModeTerm),
            PredOrFunc = pf_function,
            (
                MaybeDCGVars = no_dcg_vars,
                parse_lambda_args_func(Context,
                    ArgModeTerms0, FuncRetArgModeTerm,
                    LambdaArgs, !VarSet, BadModeSpecs, SVarSpecs),
                LambdaHead = lambda_head(LambdaPurity, Groundness, PredOrFunc,
                    lambda_normal, LambdaArgs, BadModeSpecs, SVarSpecs,
                    MaybeDetism),
                MaybeLambdaHead = ok1(LambdaHead)
            ;
                MaybeDCGVars = dcg_vars(_, _),
                Pieces = [words("Error: DCG notation is not allowed"),
                    words("in clauses for functions."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, Pieces),
                MaybeLambdaHead = error1([Spec | get_any_errors1(MaybeDetism)])
            )
        )
    else if
        % Condition 2f.
        %
        % We are looking for the same term structure as condition 1b,
        % minus the outer "is detism" wrapper. This is why the structure
        % of this code, and the variable names, resemble condition 1b.
        PFArgsDetTerm = term.functor(term.atom(BeforeIsFunctor),
            BeforeIsArgTerms, Context),
        BeforeIsFunctor = "=",
        BeforeIsArgTerms = [FuncArgsTerm, FuncRetArgModeTerm],
        FuncArgsTerm = term.functor(term.atom(FuncTermFunctor),
            ArgModeTerms, _),
        (
            FuncTermFunctor = "func",
            Groundness = ho_ground
        ;
            FuncTermFunctor = "any_func",
            Groundness = ho_any
        )
    then
        PredOrFunc = pf_function,
        % XXX Should we require that ArgModeTerms and FuncRetArgModeTerm
        % *must* have no explicit mode annotations?
        (
            MaybeDCGVars = no_dcg_vars,
            parse_lambda_args_func(Context, ArgModeTerms, FuncRetArgModeTerm,
                LambdaArgs, !VarSet, BadModeSpecs, SVarSpecs),
            MaybeDetism = ok1(detism_det),
            LambdaHead = lambda_head(LambdaPurity, Groundness, PredOrFunc,
                lambda_normal, LambdaArgs, BadModeSpecs, SVarSpecs,
                MaybeDetism),
            MaybeLambdaHead = ok1(LambdaHead)
        ;
            MaybeDCGVars = dcg_vars(_, _),
            Pieces = [words("Error: DCG notation is not allowed"),
                words("in clauses for functions."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
            MaybeLambdaHead = error1([Spec])
        )
    else
        Pieces = [words("Error: the clause head part of a lambda expression"),
            words("should have one of the following forms:"),
            quote("pred(<args>) is <determinism>"), nl,
            quote("any_pred(<args>) is <determinism>"), nl,
            quote("func(<args>) = <retarg> is <determinism>"), nl,
            quote("any_func(<args>) = <retarg> is <determinism>"), nl,
            quote("func(<args>) = <retarg>"), nl,
            quote("any_func(<args>) = <retarg>"), suffix(","), nl,
            words("or one of those forms preceded by either"),
            quote("semipure"), words("or"), quote("impure"), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            get_term_context(PFArgsDetTerm), Pieces),
        qual_info_set_found_syntax_error(yes, !QualInfo),
        MaybeLambdaHead = error1([Spec])
    ).

:- pred split_last_two(T::in, T::in, list(T)::in, list(T)::out, T::out, T::out)
    is det.

split_last_two(Element1, Element2, Elements3plus, Main, LastButOne, Last) :-
    (
        Elements3plus = [],
        Main = [],
        LastButOne = Element1,
        Last = Element2
    ;
        Elements3plus = [Element3 | Elements4plus],
        split_last_two(Element2, Element3, Elements4plus, MainTail,
            LastButOne, Last),
        Main = [Element1 | MainTail]
    ).

%---------------------------------------------------------------------------%

:- pred parse_lambda_args_func(term.context::in, list(term)::in, term::in,
    list(lambda_arg)::out, prog_varset::in, prog_varset::out,
    list(error_spec)::out, list(error_spec)::out) is det.

parse_lambda_args_func(Context, ArgModeTerms, FuncRetArgModeTerm,
        LambdaArgs, !VarSet, !:BadModeSpecs, !:SVarSpecs) :-
    !:BadModeSpecs = [],
    !:SVarSpecs = [],
    parse_lambda_args(lambda_arg_ordinary,
        ArgModeTerms, OrdinaryLambdaArgs, 1, ResultArgNum,
        !VarSet, !BadModeSpecs, !SVarSpecs),
    parse_lambda_arg(lambda_arg_func_result,
        FuncRetArgModeTerm, FuncRetLambdaArg, ResultArgNum, _,
        !VarSet, !BadModeSpecs, !SVarSpecs),
    LambdaArgs = OrdinaryLambdaArgs ++ [FuncRetLambdaArg],
    classify_lambda_arg_modes_present_absent(LambdaArgs,
        PresentArgs, AbsentArgs),
    (
        AbsentArgs = []
        % All arguments have explicit mode annotations.
    ;
        AbsentArgs = [_ | _],
        (
            PresentArgs = []
            % No arguments have explicit mode annotations.
            % The argument modes that together constitute the default
            % function mode have already been filled in.
        ;
            PresentArgs = [_ | _],
            add_some_not_all_args_have_modes_error(Context, AbsentArgs,
                !BadModeSpecs)
        )
    ).

:- pred parse_lambda_args_pred(term.context::in, list(term)::in,
    list(lambda_arg)::out, prog_varset::in, prog_varset::out,
    list(error_spec)::out, list(error_spec)::out) is det.

parse_lambda_args_pred(Context, ArgModeTerms,
        LambdaArgs, !VarSet, !:BadModeSpecs, !:SVarSpecs) :-
    !:BadModeSpecs = [],
    !:SVarSpecs = [],
    parse_lambda_args(lambda_arg_ordinary, ArgModeTerms, LambdaArgs, 1, _,
        !VarSet, !BadModeSpecs, !SVarSpecs),
    classify_lambda_arg_modes_present_absent(LambdaArgs,
        PresentArgs, AbsentArgs),
    (
        AbsentArgs = []
        % All arguments have explicit mode annotations.
    ;
        AbsentArgs = [_ | _],
        (
            PresentArgs = [],
            add_pred_no_args_have_modes_error(Context, !BadModeSpecs)
        ;
            PresentArgs = [_ | _],
            add_some_not_all_args_have_modes_error(Context, AbsentArgs,
                !BadModeSpecs)
        )
    ).

:- pred classify_lambda_arg_modes_present_absent(list(lambda_arg)::in,
    list(lambda_arg)::out, list(lambda_arg)::out) is det.

classify_lambda_arg_modes_present_absent([], [], []).
classify_lambda_arg_modes_present_absent([LambdaArg | LambdaArgs],
        PresentArgs, AbsentArgs) :-
    classify_lambda_arg_modes_present_absent(LambdaArgs,
        PresentArgsTail, AbsentArgsTail),
    PresentOrAbsent = LambdaArg ^ la_arg_mode_presence,
    (
        PresentOrAbsent = lam_present,
        PresentArgs = [LambdaArg | PresentArgsTail],
        AbsentArgs = AbsentArgsTail
    ;
        PresentOrAbsent = lam_absent,
        PresentArgs = PresentArgsTail,
        AbsentArgs = [LambdaArg | AbsentArgsTail]
    ).

:- pred add_some_not_all_args_have_modes_error(prog_context::in,
    list(lambda_arg)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

add_some_not_all_args_have_modes_error(Context, _AbsentArgs, !Specs) :-
    % We could use _AbsentArgs to make the error message more detailed.
    Pieces = [words("Error: in head of lambda expression:"),
        words("some but not all arguments have modes."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred add_pred_no_args_have_modes_error(prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pred_no_args_have_modes_error(Context, !Specs) :-
    % We could use _AbsentArgs to make the error message more detailed.
    Pieces = [words("Error: in head of predicate lambda expression:"),
        words("none of the arguments have modes."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- type lambda_arg_kind
    --->    lambda_arg_ordinary
    ;       lambda_arg_func_result.

:- type lambda_arg_mode_presence
    --->    lam_absent
    ;       lam_present.

:- type lambda_arg
    --->    lambda_arg(
                la_arg_num              :: int,
                la_arg_term             :: prog_term,
                la_arg_var              :: prog_var,
                la_kind                 :: lambda_arg_kind,

                % If the lambda argument does not have a "::mode" annotation,
                % the la_arg_mode_presence field will contain lam_absent,
                % and the la_arg_mode field will contain the default mode
                % for the argument position ("in" for ordinary arguments,
                % "out" for function results).
                %
                % If the lambda argument does have a "::mode" annotation,
                % the la_arg_mode_presence field will contain lam_present.
                % If the mode annotation can be successfully parsed,
                % the la_arg_mode field will contain that mode.
                % If the mode annotation cannot be parsed, then
                % the la_arg_mode field will contain the default mode
                % for the argument position, as above, but the messages
                % descrbing the error will be added to !BadModeSpecs.
                la_arg_mode_presence    :: lambda_arg_mode_presence,
                la_arg_mode             :: mer_mode,

                % The context of the mode annotation, or if it is absent,
                % the context of the argument.
                la_arg_mode_context     :: prog_context
            ).

:- func project_lambda_arg_term(lambda_arg) = prog_term.

project_lambda_arg_term(LambdaArg) = ArgTerm :-
    ArgTerm = LambdaArg ^ la_arg_term.

:- func project_lambda_var(lambda_arg) = prog_var.

project_lambda_var(LambdaArg) = LambdaVar :-
    LambdaVar = LambdaArg ^ la_arg_var.

:- func project_lambda_arg_mode(lambda_arg) = mer_mode.

project_lambda_arg_mode(LambdaArg) = Mode :-
    Mode = LambdaArg ^ la_arg_mode.

:- func project_lambda_var_arg_mode(lambda_arg) = pair(prog_var, mer_mode).

project_lambda_var_arg_mode(LambdaArg) = LambdaVar - Mode :-
    LambdaVar = LambdaArg ^ la_arg_var,
    Mode = LambdaArg ^ la_arg_mode.

%---------------------------------------------------------------------------%

    % Parse a list of lambda argument terms, each which should be of the form
    % argterm::modeterm.
    %
:- pred parse_lambda_args(lambda_arg_kind::in,
    list(term)::in, list(lambda_arg)::out,
    int::in, int::out, prog_varset::in, prog_varset::out,
    list(error_spec)::in, list(error_spec)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_lambda_args(_Kind, [], [], !ArgNum, !VarSet, !BadModeSpecs, !SVarSpecs).
parse_lambda_args(Kind, [HeadArgModeTerm | TailArgModeTerms],
        [HeadLambdaArg | TailLambdaArgs],
        !ArgNum, !VarSet, !BadModeSpecs, !SVarSpecs) :-
    parse_lambda_arg(Kind, HeadArgModeTerm, HeadLambdaArg,
        !ArgNum, !VarSet, !BadModeSpecs, !SVarSpecs),
    parse_lambda_args(Kind, TailArgModeTerms, TailLambdaArgs,
        !ArgNum, !VarSet, !BadModeSpecs, !SVarSpecs).

:- pred parse_lambda_arg(lambda_arg_kind::in, term::in, lambda_arg::out,
    int::in, int::out, prog_varset::in, prog_varset::out,
    list(error_spec)::in, list(error_spec)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_lambda_arg(Kind, ArgModeTerm, LambdaArg, !ArgNum, !VarSet,
        !BadModeSpecs, !SVarSpecs) :-
    ( if
        ArgModeTerm = term.functor(term.atom("::"),
            [ArgTermPrime, ModeTerm], _)
    then
        ArgTerm = ArgTermPrime,
        PresentOrAbsent = lam_present,
        ModeContext = get_term_context(ModeTerm),
        ContextPieces = cord.from_list([words("In the"), nth_fixed(!.ArgNum),
            words("argument of the lambda expression:")]),
        varset.coerce(!.VarSet, GenericVarSet),
        parse_mode(allow_constrained_inst_var, GenericVarSet, ContextPieces,
            ModeTerm, MaybeMode0),
        (
            MaybeMode0 = ok1(Mode0),
            constrain_inst_vars_in_mode(Mode0, Mode)
        ;
            MaybeMode0 = error1(ModeSpecs),
            !:BadModeSpecs = ModeSpecs ++ !.BadModeSpecs,
            Mode = default_mode_for_lambda_arg(Kind)
        )
    else
        ArgTerm = ArgModeTerm,
        PresentOrAbsent = lam_absent,
        Mode = default_mode_for_lambda_arg(Kind),
        ModeContext = get_term_context(ArgModeTerm)
    ),

    term.coerce(ArgTerm, ProgArgTerm),
    % We currently do not allow !X to appear as a lambda head argument, though
    % we might later extend the syntax still further to accommodate this
    % using syntax such as !IO::(di, uo).
    ( if is_term_a_bang_state_pair(ProgArgTerm, StateVar, StateVarContext) then
        (
            Kind = lambda_arg_ordinary,
            report_illegal_bang_svar_lambda_arg(StateVarContext, !.VarSet,
                StateVar, !SVarSpecs)
        ;
            Kind = lambda_arg_func_result,
            report_illegal_func_svar_result(StateVarContext, !.VarSet,
                StateVar, !SVarSpecs)
        )
    else
        true
    ),
    % We always allocate a new variable for each lambda argument,
    % even if the argument term is already a variable (which is what
    % make_fresh_arg_vars_subst_svars does). This is because for functions,
    % we need to ensure that the variable corresponding to the function
    % result term is a new variable, to avoid the function result term
    % becoming lambda-quantified.
    LambdaVarName = "LambdaHeadVar__" ++ string.int_to_string(!.ArgNum),
    varset.new_named_var(LambdaVarName, LambdaVar, !VarSet),
    LambdaArg = lambda_arg(!.ArgNum, ProgArgTerm, LambdaVar, Kind,
        PresentOrAbsent, Mode, ModeContext),
    !:ArgNum = !.ArgNum + 1.

:- func default_mode_for_lambda_arg(lambda_arg_kind) = mer_mode.

default_mode_for_lambda_arg(Kind) = Mode :-
    (
        Kind = lambda_arg_ordinary,
        in_mode(Mode)
    ;
        Kind = lambda_arg_func_result,
        out_mode(Mode)
    ).

%-----------------------------------------------------------------------------%

:- pred parse_purity_annotation(term(T)::in, purity::out, term(T)::out) is det.

parse_purity_annotation(Term0, Purity, Term) :-
    ( if
        Term0 = term.functor(term.atom(PurityName), [Term1], _),
        purity_name(Purity0, PurityName)
    then
        Purity = Purity0,
        Term = Term1
    else
        Purity = purity_pure,
        Term = Term0
    ).

:- pred parse_lambda_detism(prog_varset::in, term::in,
    maybe1(determinism)::out) is det.

parse_lambda_detism(VarSet, DetismTerm, MaybeDetism) :-
    ( if
        DetismTerm = term.functor(term.atom(DetString), [], _),
        standard_det(DetString, Detism)
    then
        MaybeDetism = ok1(Detism)
    else
        varset.coerce(VarSet, GenericVarSet),
        TermStr = describe_error_term(GenericVarSet, DetismTerm),
        Pieces = [words("Error:"), words(TermStr),
            words("is not a valid determinism."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(DetismTerm), Pieces),
        MaybeDetism = error1([Spec])
    ).

%-----------------------------------------------------------------------------%
%
% Code for building lambda expressions.
%

:- type lambda_head
    --->    lambda_head(
                purity,
                ho_groundness,
                pred_or_func,
                lambda_eval_method,
                list(lambda_arg),
                list(error_spec),       % Errors about unparseable and/or
                                        % missing arg modes.
                list(error_spec),       % Errors about !X arguments.
                maybe1(determinism)     % The determinism of the lambda expr.
            ).

:- pred build_lambda_expression(prog_var::in, purity::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    lambda_head::in, maybe1(goal)::in, expansion::out,
    svar_state::in, svar_store::in, svar_store::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_lambda_expression(LHSVar, UnificationPurity,
        Context, MainContext, SubContext, LambdaHead, MaybeBodyGoal,
        Expansion, OutsideSVarState, !SVarStore,
        !VarSet, !ModuleInfo, !QualInfo, !Specs) :-
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
    % Note that the quantification is important here. That is why we need
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

    LambdaHead = lambda_head(LambdaPurity, Groundness, PredOrFunc,
        EvalMethod, LambdaArgs0, BadModeSpecs, SVarSpecs, MaybeDetism),
    qualify_lambda_arg_modes_if_not_opt_imported(LambdaArgs0, LambdaArgs1,
        Modes, !QualInfo, !Specs),
    inconsistent_constrained_inst_vars_in_modes(Modes, InconsistentVars),
    (
        InconsistentVars = []
    ;
        InconsistentVars = [_ | _],
        varset.coerce(!.VarSet, InstVarSet),
        InconsistentVarStrs = list.map(
            mercury_var_to_string_vs(InstVarSet, print_name_only),
            InconsistentVars),
        InconsistentVarPieces =
            [words("Error: the constraints on the inst"),
            words(choose_number(InconsistentVars, "variable", "variables")) |
            list_to_quoted_pieces(InconsistentVarStrs)] ++
            [words("are inconsistent."), nl],
        InconsistentVarSpec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, Context, InconsistentVarPieces),
        !:Specs = [InconsistentVarSpec | !.Specs]
    ),
    (
        MaybeDetism = ok1(Detism)
    ;
        MaybeDetism = error1(DetismSpecs),
        !:Specs = DetismSpecs ++ !.Specs,
        % Due to the error, this dummy value won't be used.
        Detism = detism_det
    ),
    (
        MaybeBodyGoal = ok1(BodyGoal)
    ;
        MaybeBodyGoal = error1(BodyGoalSpecs),
        !:Specs = BodyGoalSpecs ++ !.Specs,
        qual_info_set_found_syntax_error(yes, !QualInfo),
        % Due to the error, this dummy value won't be used.
        BodyGoal = true_expr(Context)
    ),

    ArgSpecs = BadModeSpecs ++ SVarSpecs,
    (
        ArgSpecs = [_ | _],
        !:Specs = ArgSpecs ++ !.Specs,
        qual_info_set_found_syntax_error(yes, !QualInfo),
        Goal = true_goal_with_context(Context)
    ;
        ArgSpecs = [],
        some [!SVarState] (
            ArgTerms1 = list.map(project_lambda_arg_term, LambdaArgs1),
            svar_prepare_for_lambda_head(Context, ArgTerms1, ArgTerms,
                FinalSVarMap, OutsideSVarState, !:SVarState, !VarSet, !Specs),
            InitialSVarState = !.SVarState,

            % Partition the arguments (and their corresponding lambda vars)
            % into two sets: those that are not output, i.e. input and unused,
            % and those that are output.
            %
            % The call to svar_prepare_for_lambda_head obsoletes the arg term
            % fields of LambdaArgs1, so we must pass the new arg terms
            % separately. We don't need to put them back into the lambda args,
            % since the lambda args won't be needed later.
            partition_args_and_lambda_vars(!.ModuleInfo, LambdaArgs1, ArgTerms,
                NonOutputLambdaVarsArgs, OutputLambdaVarsArgs),

            PredFormArity = arg_list_arity(ArgTerms),
            ArgContext = ac_head(PredOrFunc, PredFormArity),

            % Create the unifications that need to come before the body of the
            % lambda expression; those corresponding to args whose mode is
            % input or unused.
            HeadBefore0 = true_goal_with_context(Context),
            insert_arg_unifications(NonOutputLambdaVarsArgs,
                Context, ArgContext, HeadBefore0, HeadBefore,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),

            map.init(Substitution),
            transform_parse_tree_goal_to_hlds(loc_whole_goal, BodyGoal,
                Substitution, Body, !SVarState, !SVarStore,
                !VarSet, !ModuleInfo, !QualInfo, !Specs),

            % Create the unifications that need to come after the body of the
            % lambda expression; those corresponding to args whose mode is
            % output.
            HeadAfter0 = true_goal_with_context(Context),
            insert_arg_unifications(OutputLambdaVarsArgs,
                Context, ArgContext, HeadAfter0, HeadAfter,
                !SVarState, !SVarStore, !VarSet,
                !ModuleInfo, !QualInfo, !Specs),

            LambdaVarsModes =
                list.map(project_lambda_var_arg_mode, LambdaArgs1),
            LambdaVars =
                list.map(project_lambda_var, LambdaArgs1),

            trace [compiletime(flag("debug-statevar-lambda")), io(!IO)] (
                get_debug_output_stream(!.ModuleInfo, DebugStream, !IO),
                io.write_string(DebugStream, "\nLAMBDA EXPRESSION\n", !IO),
                io.write_string(DebugStream, "arg terms before:\n", !IO),
                list.foldl(io.write_line(DebugStream), ArgTerms1, !IO),
                io.write_string(DebugStream, "arg terms after:\n", !IO),
                list.foldl(io.write_line(DebugStream), ArgTerms, !IO),
                io.write_string(DebugStream, "lambda arg vars:\n", !IO),
                io.write_line(DebugStream, LambdaVars, !IO),
                io.write_string(DebugStream,
                    "lambda arg unifies before:\n", !IO),
                dump_goal_nl(DebugStream, !.ModuleInfo, vns_varset(!.VarSet),
                    HeadBefore, !IO),
                io.write_string(DebugStream, "lambda body:\n", !IO),
                dump_goal_nl(DebugStream, !.ModuleInfo, vns_varset(!.VarSet),
                    Body, !IO),
                io.write_string(DebugStream,
                    "lambda arg unifies after:\n", !IO),
                dump_goal_nl(DebugStream, !.ModuleInfo, vns_varset(!.VarSet),
                    HeadAfter, !IO),
                map.to_assoc_list(FinalSVarMap, FinalSVarList),
                io.write_string(DebugStream, "FinalSVarMap:\n", !IO),
                io.write_line(DebugStream, FinalSVarList, !IO)
            ),

            % Fix up any state variable unifications.
            FinalSVarState = !.SVarState,
            module_info_get_globals(!.ModuleInfo, Globals),
            module_info_get_name(!.ModuleInfo, ModuleName),
            svar_finish_lambda_body(Globals, ModuleName, Context, FinalSVarMap,
                [HeadBefore, Body, HeadAfter], HLDS_Goal0,
                InitialSVarState, FinalSVarState, !SVarStore),

            % Figure out which variables we need to explicitly existentially
            % quantify.
            (
                PredOrFunc = pf_predicate,
                QuantifiedArgTerms = ArgTerms
            ;
                PredOrFunc = pf_function,
                pred_args_to_func_args(ArgTerms, QuantifiedArgTerms,
                    _ReturnValTerm)
            ),
            term_vars.vars_in_terms(QuantifiedArgTerms, QuantifiedVars0),
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
                EvalMethod, LambdaNonLocals, LambdaVarsModes, Detism,
                HLDS_Goal),
            make_atomic_unification(LHSVar, LambdaRHS, Context, MainContext,
                SubContext, UnificationPurity, Goal, !QualInfo)
        )
    ),
    Expansion = expansion(not_fgti, cord.singleton(Goal)).

    % Partition the lists of arguments and variables into lists
    % of non-output and output arguments and variables.
    %
:- pred partition_args_and_lambda_vars(module_info::in,
    list(lambda_arg)::in, list(prog_term)::in,
    list(unify_var_term)::out, list(unify_var_term)::out) is det.

partition_args_and_lambda_vars(_, [], [], [], []).
partition_args_and_lambda_vars(_, [], [_ | _], _, _) :-
    unexpected($pred, "mismatched lists").
partition_args_and_lambda_vars(_, [_ | _], [], _, _) :-
    unexpected($pred, "mismatched lists").
partition_args_and_lambda_vars(ModuleInfo,
        [LambdaArg | LambdaArgs], [ArgTerm | ArgTerms],
        InputLambdaVarsArgTerms, OutputLambdaVarsArgTerms) :-
    partition_args_and_lambda_vars(ModuleInfo, LambdaArgs, ArgTerms,
        InputLambdaVarsArgTermsTail, OutputLambdaVarsArgTermsTail),

    LambdaArg = lambda_arg(_ArgNum, _SupersededArgTerm, LambdaVar,
        _Kind, _PresentOrAbsent, Mode, _ModeContext),
    LambdaVarArgTerm = unify_var_term(LambdaVar, ArgTerm),

    % If the mode is undefined, calling mode_is_output/2 directly would cause
    % the compiler to abort, so we don't want to do that.
    %
    % It does not really matter whether we consider an argument with an
    % undefined mode input or output, because mode analysis will fail anyway.
    % The code here is slightly simpler if we consider it input.
    ( if
        mode_is_defined(ModuleInfo, Mode),
        mode_is_output(ModuleInfo, Mode)
    then
        % defined and output
        InputLambdaVarsArgTerms  = InputLambdaVarsArgTermsTail,
        OutputLambdaVarsArgTerms =
            [LambdaVarArgTerm | OutputLambdaVarsArgTermsTail]
    else
        % undefined or (defined and not output)
        InputLambdaVarsArgTerms  =
            [LambdaVarArgTerm | InputLambdaVarsArgTermsTail],
        OutputLambdaVarsArgTerms = OutputLambdaVarsArgTermsTail
    ).

    % Succeeds iff the given mode is defined.
    %
:- pred mode_is_defined(module_info::in, mer_mode::in) is semidet.

mode_is_defined(ModuleInfo, Mode) :-
    mode_get_insts_semidet(ModuleInfo, Mode, _, _).

:- pred qualify_lambda_arg_modes_if_not_opt_imported(
    list(lambda_arg)::in, list(lambda_arg)::out, list(mer_mode)::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_lambda_arg_modes_if_not_opt_imported(LambdaArgs0, LambdaArgs,
        Modes, !QualInfo, !Specs) :-
    qual_info_get_maybe_opt_imported(!.QualInfo, MaybeOptImported),
    (
        MaybeOptImported = is_not_opt_imported,
        % Lambda expressions cannot appear in the interface of a module.
        InInt = mq_not_used_in_interface,
        qual_info_get_mq_info(!.QualInfo, MQInfo0),
        qualify_lambda_arg_modes(InInt, LambdaArgs0, LambdaArgs, Modes,
            MQInfo0, MQInfo, !Specs),
        qual_info_set_mq_info(MQInfo, !QualInfo)
    ;
        MaybeOptImported = is_opt_imported,
        % The modes in `.opt' files are already fully module qualified.
        LambdaArgs = LambdaArgs0,
        Modes = list.map(project_lambda_arg_mode, LambdaArgs)
    ).

:- pred qualify_lambda_arg_modes(mq_in_interface::in,
    list(lambda_arg)::in, list(lambda_arg)::out, list(mer_mode)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_lambda_arg_modes(_InInt, [], [], [], !MQInfo, !Specs).
qualify_lambda_arg_modes(InInt, [LambdaArg0 | LambdaArgs0],
        [LambdaArg | LambdaArgs], [Mode | Modes], !MQInfo, !Specs) :-
    LambdaArg0 = lambda_arg(ArgNum, ProgArgTerm, LambdaVar,
        Kind, PresentOrAbsent, Mode0, ModeContext),
    qualify_lambda_mode(InInt, ModeContext, Mode0, Mode, !MQInfo, !Specs),
    LambdaArg = lambda_arg(ArgNum, ProgArgTerm, LambdaVar,
        Kind, PresentOrAbsent, Mode, ModeContext),
    qualify_lambda_arg_modes(InInt, LambdaArgs0,
        LambdaArgs, Modes, !MQInfo, !Specs).

%-----------------------------------------------------------------------------%

:- pred arg_context_to_unify_context(arg_context::in, int::in,
    unify_main_context::out, unify_sub_contexts::out) is det.

arg_context_to_unify_context(ArgContext, ArgNum, MainContext, SubContexts) :-
    (
        ArgContext = ac_head(PredOrFunc, PredFormArity),
        ( if
            PredOrFunc = pf_function,
            PredFormArity = pred_form_arity(PredFormArityInt),
            ArgNum = PredFormArityInt
        then
            % It is the function result term in the head.
            MainContext = umc_head_result
        else
            % It is a non-function-result head argument.
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

make_fresh_arg_vars_subst_svars(Args, Vars, VarsArgs,
        !VarSet, !SVarState, !Specs) :-
    % For efficiency, we construct `VarsArgs' backwards and then reverse it
    % to get the correct order.
    make_fresh_arg_vars_subst_svars_loop(Args, Vars, [], RevVarsArgs,
        !VarSet, !SVarState, !Specs),
    list.reverse(RevVarsArgs, VarsArgs).

:- pred make_fresh_arg_vars_subst_svars_loop(
    list(prog_term)::in, list(prog_var)::out,
    list(unify_var_term)::in, list(unify_var_term)::out,
    prog_varset::in, prog_varset::out, svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

make_fresh_arg_vars_subst_svars_loop([], [],
        !RevVarsArgs, !VarSet, !SVarState, !Specs).
make_fresh_arg_vars_subst_svars_loop([Arg | Args], [Var | Vars],
        !RevVarsArgs, !VarSet, !SVarState, !Specs) :-
    make_fresh_arg_var_subst_svars(Arg, Var, !RevVarsArgs,
        !VarSet, !SVarState, !Specs),
    make_fresh_arg_vars_subst_svars_loop(Args, Vars,
        !RevVarsArgs, !VarSet, !SVarState, !Specs).

:- pred make_fresh_arg_var_subst_svars(prog_term::in, prog_var::out,
    list(unify_var_term)::in, list(unify_var_term)::out,
    prog_varset::in, prog_varset::out, svar_state::in, svar_state::out,
    list(error_spec)::in, list(error_spec)::out) is det.

make_fresh_arg_var_subst_svars(Arg0, Var, !RevVarsArgs,
        !VarSet, !SVarState, !Specs) :-
    substitute_state_var_mapping(Arg0, Arg, !VarSet, !SVarState, !Specs),
    (
        Arg = term.variable(ArgVar, _),
        ( if have_seen_arg_var(!.RevVarsArgs, ArgVar) then
            % This is the second or later appearance of ArgVar
            % in the argument list.
            varset.new_var(Var, !VarSet)
        else
            Var = ArgVar
        )
    ;
        Arg = term.functor(_, _, _),
        varset.new_var(Var, !VarSet)
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

%-----------------------------------------------------------------------------%

:- pred make_fresh_arg_var_no_svar(prog_term::in, prog_var::out,
    list(prog_var)::in, prog_varset::in, prog_varset::out) is det.

make_fresh_arg_var_no_svar(Arg, Var, Vars0, !VarSet) :-
    ( if
        Arg = term.variable(ArgVar, _),
        not list.member(ArgVar, Vars0)
    then
        Var = ArgVar
    else
        varset.new_var(Var, !VarSet)
    ).

%-----------------------------------------------------------------------------%

:- pred occurs_check(module_info::in, prog_varset::in, ancestor_var_map::in,
    prog_var::in, list(error_spec)::in, list(error_spec)::out) is det.

occurs_check(ModuleInfo, VarSet, AncestorVarMap, Var, !Specs) :-
    ( if map.search(AncestorVarMap, Var, AncestorContext) then
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals,
            warn_suspected_occurs_check_failure, WarnOccursCheck),
        (
            WarnOccursCheck = no
        ;
            WarnOccursCheck = yes,
            varset.lookup_name(VarSet, Var, VarName),
            Pieces = [words("Warning: the variable"), quote(VarName),
                words("is unified with a term containing itself."), nl],
            Spec = simplest_spec($pred, severity_warning,
                phase_parse_tree_to_hlds, AncestorContext, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    else
        true
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.superhomogeneous.
%-----------------------------------------------------------------------------%
