%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2008, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: constraint.m.
% Main author: stayl.
%
% The constraint propagation transformation attempts to improve the efficiency
% of a generate-and-test style program by statically scheduling constraints as
% early as possible, where a "constraint" is any pure goal which has no
% outputs, can fail, cannot loop and cannot throw an exception.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.constraint.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.

%-----------------------------------------------------------------------------%

:- type constraint_info.

    % propagate_constraints_in_goal pushes constraints left and inward within
    % a single goal. Specialized versions of procedures which are called with
    % constrained outputs are created by deforest.m. Goals which deforest.m
    % should try to propagate into calls are annotated with a `constraint'
    % goal feature.
    %
:- pred propagate_constraints_in_goal(hlds_goal::in, hlds_goal::out,
    constraint_info::in, constraint_info::out) is det.

:- pred constraint_info_init(module_info::in, vartypes::in, prog_varset::in,
    instmap::in, constraint_info::out) is det.

:- pred constraint_info_deconstruct(constraint_info::in, module_info::out,
    vartypes::out, prog_varset::out, bool::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_match.
:- import_module hlds.goal_form.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.set_of_var.

:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

propagate_constraints_in_goal(Goal0, Goal, !Info) :-
    % We need to strip off any existing constraint markers first.
    % Constraint markers are meant to indicate where a constraint is meant
    % to be attached to a call, and that deforest.m should consider creating
    % a specialized version for the call. If deforest.m rearranges the goal,
    % the constraints may not remain next to the call.
    strip_constraint_markers(Goal0, Goal1),
    propagate_goal([], Goal1, Goal, !Info).

:- pred propagate_goal(list(constraint)::in, hlds_goal::in, hlds_goal::out,
    constraint_info::in, constraint_info::out) is det.

propagate_goal(Constraints, Goal0, Goal, !Info) :-
    % We need to treat all single goals as conjunctions so that propagate_conj
    % can move the constraints to the left of the goal if that is allowed.
    Goal0 = hlds_goal(_, GoalInfo0),
    Features0 = goal_info_get_features(GoalInfo0),
    Context = goal_info_get_context(GoalInfo0),
    goal_to_conj_list(Goal0, Goals0),
    propagate_conj(Constraints, Goals0, Goals, !Info),
    goal_list_nonlocals(Goals, NonLocals),
    goal_list_instmap_delta(Goals, Delta),
    goal_list_determinism(Goals, ConjDetism),
    goal_list_purity(Goals, Purity),
    goal_info_init(NonLocals, Delta, ConjDetism, Purity, Context, GoalInfo1),
    goal_info_set_features(Features0, GoalInfo1, GoalInfo),
    conj_list_to_goal(Goals, GoalInfo, Goal).

:- pred propagate_conj_sub_goal(list(constraint)::in,
    hlds_goal::in, list(hlds_goal)::out,
    constraint_info::in, constraint_info::out) is det.

propagate_conj_sub_goal(Constraints, Goal0, Goals, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, _),
    HasSubGoals = goal_expr_has_subgoals(GoalExpr0),
    (
        HasSubGoals = does_not_have_subgoals
    ;
        HasSubGoals = has_subgoals,
        % If a non-empty list of constraints is pushed into a subgoal,
        % quantification, instmap_deltas and determinism need to be recomputed.
        constraint_info_update_changed(Constraints, !Info)
    ),
    InstMap0 = !.Info ^ constr_instmap,
    propagate_conj_sub_goal_2(Constraints, Goal0, Goals, !Info),
    !Info ^ constr_instmap := InstMap0.

:- pred propagate_conj_sub_goal_2(list(constraint)::in,
    hlds_goal::in, list(hlds_goal)::out,
    constraint_info::in, constraint_info::out) is det.

propagate_conj_sub_goal_2(Constraints, Goal0, FinalGoals, !Info) :-
    Goal0 = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            propagate_conj(Constraints, Goals0, Goals, !Info),
            FinalGoals = [hlds_goal(conj(ConjType, Goals), GoalInfo)]
        ;
            ConjType = parallel_conj,
            % We can't propagate constraints into parallel conjunctions because
            % parallel conjunctions must have determinism det. However, we can
            % propagate constraints *within* the goals of the conjunction.
            flatten_constraints(Constraints, MoreGoals),
            propagate_in_independent_goals([], Goals0, Goals, !Info),
            FinalGoals = [hlds_goal(conj(ConjType, Goals), GoalInfo) |
                MoreGoals]
        )
    ;
        GoalExpr = disj(Goals0),
        propagate_in_independent_goals(Constraints, Goals0, Goals, !Info),
        FinalGoals = [hlds_goal(disj(Goals), GoalInfo)]
    ;
        GoalExpr = switch(Var, CanFail, Cases0),
        propagate_cases(Var, Constraints, Cases0, Cases, !Info),
        FinalGoals = [hlds_goal(switch(Var, CanFail, Cases), GoalInfo)]
    ;
        GoalExpr = if_then_else(Vars, Cond0, Then0, Else0),
        InstMap0 = !.Info ^ constr_instmap,
        % We can't safely propagate constraints into the condition of an
        % if-then-else, because that would change the answers generated
        % by the procedure.
        propagate_goal([], Cond0, Cond, !Info),
        constraint_info_update_goal(Cond, !Info),
        propagate_goal(Constraints, Then0, Then, !Info),
        !Info ^ constr_instmap := InstMap0,
        propagate_goal(Constraints, Else0, Else, !Info),
        FinalGoals =
            [hlds_goal(if_then_else(Vars, Cond, Then, Else), GoalInfo)]
    ;
        GoalExpr = scope(Reason, SubGoal0),
        (
            Reason = disable_warnings(_, _),
            % NOTE: This assumes that compiler passes that generate the
            % warnings that could be disabled by this scope have all been run
            % BEFORE program transformations such as constraint propagation.
            % If they haven't been, then the transformations can hide warnings
            % about code by moving them into these scopes, or can caused them
            % to be generated when the user does not want them by moving
            % the warned-about code out of such scopes.
            propagate_goal(Constraints, SubGoal0, SubGoal, !Info),
            FinalGoals = [hlds_goal(scope(Reason, SubGoal), GoalInfo)]
        ;
            ( Reason = exist_quant(_)
            ; Reason = from_ground_term(_, from_ground_term_deconstruct)
            ; Reason = from_ground_term(_, from_ground_term_other)
            ),
            propagate_goal(Constraints, SubGoal0, SubGoal, !Info),
            FinalGoals = [hlds_goal(scope(Reason, SubGoal), GoalInfo)]
        ;
            ( Reason = promise_solutions(_, _)
            ; Reason = promise_purity(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            ; Reason = loop_control(_, _, _)
            ),
            % We can't safely propagate constraints into one of these scopes.
            % However, we can propagate constraints inside the scope goal.
            propagate_goal([], SubGoal0, SubGoal, !Info),
            flatten_constraints(Constraints, ConstraintGoals),
            FinalGoals = [hlds_goal(scope(Reason, SubGoal), GoalInfo) |
                ConstraintGoals]
        ;
            ( Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ; Reason = from_ground_term(_, from_ground_term_initial)
            ),
            % These scopes should have been deleted or converted by now.
            unexpected($pred, "unexpected scope")
        ;
            Reason = from_ground_term(_, from_ground_term_construct),
            % There is no point in either propagating constraints into these
            % scopes or propagating local constraints within these scopes.
            flatten_constraints(Constraints, ConstraintGoals),
            FinalGoals = [hlds_goal(GoalExpr, GoalInfo) | ConstraintGoals]
        )
    ;
        GoalExpr = negation(NegGoal0),
        % We can't safely propagate constraints into a negation,
        % because that would change the answers computed by the procedure.
        propagate_goal([], NegGoal0, NegGoal, !Info),
        flatten_constraints(Constraints, ConstraintGoals),
        FinalGoals = [hlds_goal(negation(NegGoal), GoalInfo) | ConstraintGoals]
    ;
        ( GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ),
        % Propagate_conj will move the constraints to the left of the call
        % or unification if that is possible, so nothing needs to be done here.
        flatten_constraints(Constraints, ConstraintGoals),
        FinalGoals = [hlds_goal(GoalExpr, GoalInfo) | ConstraintGoals]
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

%-----------------------------------------------------------------------------%

    % Put the constant constructions in front of the constraint.
    %
:- pred flatten_constraints(list(constraint)::in, list(hlds_goal)::out) is det.

flatten_constraints(Constraints0, Goals) :-
    list.map(constraint_goals_to_cord, Constraints0, GoalCords),
    Goals = cord_list_to_list(GoalCords).

:- pred constraint_goals_to_cord(constraint::in, cord(hlds_goal)::out) is det.

constraint_goals_to_cord(Constraint, GoalCord) :-
    Constraint = constraint(ConstraintGoal, _, _, ConstructGoals),
    GoalCord = cord.snoc(cord.from_list(ConstructGoals), ConstraintGoal).

%-----------------------------------------------------------------------------%

:- pred propagate_in_independent_goals(list(constraint)::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    constraint_info::in, constraint_info::out) is det.

propagate_in_independent_goals(_, [], [], !Info).
propagate_in_independent_goals(Constraints, [Goal0 | Goals0], [Goal | Goals],
        !Info) :-
    InstMap0 = !.Info ^ constr_instmap,
    propagate_goal(Constraints, Goal0, Goal, !Info),
    !Info ^ constr_instmap := InstMap0,
    propagate_in_independent_goals(Constraints, Goals0, Goals, !Info).

%-----------------------------------------------------------------------------%

:- pred propagate_cases(prog_var::in, list(constraint)::in,
    list(case)::in, list(case)::out,
    constraint_info::in, constraint_info::out) is det.

propagate_cases(_, _, [], [], !Info).
propagate_cases(Var, Constraints, [Case0 | Cases0], [Case | Cases], !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    InstMap0 = !.Info ^ constr_instmap,
    constraint_info_bind_var_to_functors(Var, MainConsId, OtherConsIds, !Info),
    propagate_goal(Constraints, Goal0, Goal, !Info),
    !Info ^ constr_instmap := InstMap0,
    Case = case(MainConsId, OtherConsIds, Goal),
    propagate_cases(Var, Constraints, Cases0, Cases, !Info).

%-----------------------------------------------------------------------------%

    % propagate_conj detects the constraints in a conjunction and moves them
    % to as early as possible in the list. We expend some effort to keep
    % the constraints in the same order as they are encountered, since this
    % should increase the likelihood of allowing us to fold recursive calls.
    %
:- pred propagate_conj(list(constraint)::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    constraint_info::in, constraint_info::out) is det.

propagate_conj(Constraints, Goals0, Goals, !Info) :-
    constraint_info_update_changed(Constraints, !Info),
    (
        Goals0 = [],
        flatten_constraints(Constraints, Goals)
    ;
        Goals0 = [Goal0 | GoalsTail0],
        ( if
            GoalsTail0 = [],
            Constraints = []
        then
            propagate_conj_sub_goal([], Goal0, Goals, !Info)
        else
            InstMap0 = !.Info ^ constr_instmap,
            ModuleInfo = !.Info ^ constr_module_info,
            VarTypes = !.Info ^ constr_vartypes,
            annotate_conj_output_vars(Goals0, ModuleInfo,
                VarTypes, InstMap0, [], RevAnnotatedGoals1),
            annotate_conj_constraints(ModuleInfo, RevAnnotatedGoals1,
                Constraints, [], AnnotatedGoals2, !Info),
            propagate_conj_constraints(AnnotatedGoals2, cord.init, GoalsCord,
                !Info),
            Goals = cord.list(GoalsCord)
        )
    ).

    % Annotate each conjunct with the variables it produces.
    %
:- pred annotate_conj_output_vars(list(hlds_goal)::in, module_info::in,
    vartypes::in, instmap::in, annotated_conj::in, annotated_conj::out) is det.

annotate_conj_output_vars([], _, _, _, !RevAnnotatedGoals).
annotate_conj_output_vars([Goal | Goals], ModuleInfo, VarTypes, InstMap0,
        !RevAnnotatedGoals) :-
    Goal = hlds_goal(_, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),

    apply_instmap_delta(InstMapDelta, InstMap0, InstMap),
    instmap_changed_vars(ModuleInfo, VarTypes, InstMap0, InstMap,
        ChangedVars0),

    instmap_vars_list(InstMap, InstMapVars),

    % Restrict the set of changed variables down to the set for which
    % the new inst is not an acceptable substitute for the old. This is done
    % to allow reordering of a goal which uses a variable with inst
    % `ground(shared, no)' with a constraint which just adds information,
    % changing the inst to `bound(shared, ...)'.

    InCompatible =
        ( pred(Var::in) is semidet :-
            instmap_lookup_var(InstMap0, Var, InstBefore),
            instmap_delta_search_var(InstMapDelta, Var, InstAfter),
            lookup_var_type(VarTypes, Var, Type),
            not inst_matches_initial(InstAfter, InstBefore, Type, ModuleInfo)
        ),
    IncompatibleInstVars = set_of_var.list_to_set(
        list.filter(InCompatible, InstMapVars)),

    % This will consider variables with inst `any' to be bound by the goal,
    % so goals which have non-locals with inst `any' will not be considered
    % to be constraints. XXX This is too conservative.

    Bound =
        ( pred(Var::in) is semidet :-
            instmap_lookup_var(InstMap0, Var, InstBefore),
            instmap_delta_search_var(InstMapDelta, Var, InstAfter),
            lookup_var_type(VarTypes, Var, Type),
            not inst_matches_binding(InstAfter, InstBefore, Type, ModuleInfo)
        ),
    BoundVars = set_of_var.list_to_set(list.filter(Bound, InstMapVars)),

    % Make sure that variables with inst `any' are placed in the changed vars
    % set. XXX This is too conservative, but avoids unexpected reorderings.

    set_of_var.union(ChangedVars0, BoundVars, ChangedVars),

    AnnotatedConjunct = annotated_conjunct(Goal, ChangedVars, BoundVars,
        IncompatibleInstVars),
    !:RevAnnotatedGoals = [AnnotatedConjunct | !.RevAnnotatedGoals],
    annotate_conj_output_vars(Goals, ModuleInfo, VarTypes, InstMap,
        !RevAnnotatedGoals).

%-----------------------------------------------------------------------------%

    % A conjunction in which each conjunct is annotated with the set of
    % variables whose instantiatedness it changes.
    %
:- type annotated_conj == list(annotated_conjunct).

:- type annotated_conjunct
    --->    annotated_conjunct(
                hlds_goal,

                % All variables returned by instmap_changed_vars.
                set_of_progvar,

                % All variables returned by instmap_changed_vars for
                % which inst_matches_binding(NewInst, OldInst) fails.
                set_of_progvar,

                % Variables returned by instmap_changed_vars
                % for which the new inst cannot be substituted
                % for the old as an input to a goal
                % (inst_matches_initial(NewInst, OldInst) fails).
                set_of_progvar
            ).

    % A constraint is a goal that may fail, has no outputs,
    % always terminates and will not throw an exception.
    %
:- type constraint
    --->    constraint(
                % The constraint itself.
                hlds_goal,

                % All variables returned by instmap_changed_vars.
                set_of_progvar,

                % Variables returned by instmap_changed_vars
                % for which the new inst cannot be substituted
                % for the old as an input to a goal
                % (inst_matches_initial(NewInst, OldInst) fails).
                set_of_progvar,

                % Goals to construct constants used by the constraint.
                % (as in X = 2, Y < X). These need to be propagated
                % with the constraint.
                list(hlds_goal)
            ).

    % Conjunction annotated with constraining goals.
    %
:- type constrained_conj == list(constrained_goal).
:- type constrained_goal
    --->    constrained_goal(hlds_goal, list(constraint)).

    % Pass backwards over the conjunction, annotating each conjunct
    % with the constraints that should be pushed into it.
    %
:- pred annotate_conj_constraints(module_info::in, annotated_conj::in,
    list(constraint)::in, constrained_conj::in, constrained_conj::out,
    constraint_info::in, constraint_info::out) is det.

annotate_conj_constraints(_, [], Constraints0,
        ConstrainedGoals0, ConstrainedGoals, !Info) :-
    flatten_constraints(Constraints0, Constraints1),
    list.map(add_empty_constraints, Constraints1, NewConstrainedGoals),
    ConstrainedGoals = NewConstrainedGoals ++ ConstrainedGoals0.
annotate_conj_constraints(ModuleInfo, [Conjunct | RevConjuncts0], Constraints0,
        ConstrainedGoals0, ConstrainedGoals, !Info) :-
    Conjunct = annotated_conjunct(Goal, ChangedVars, OutputVars,
        IncompatibleInstVars),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    CI_ModuleInfo0 = !.Info ^ constr_module_info,
    goal_can_loop_or_throw(Goal, GoalCanLoopOrThrow,
        CI_ModuleInfo0, CI_ModuleInfo),
    !Info ^ constr_module_info := CI_ModuleInfo,
    ( if
        % Propagate goals that can fail and have no output variables.
        % Propagating cc_nondet goals would be tricky, because we would
        % need to be careful about reordering the constraints (the cc_nondet
        % goal can't be moved before any goals which can fail).
        Detism = goal_info_get_determinism(GoalInfo),
        ( Detism = detism_semi
        ; Detism = detism_failure
        ),

        % XXX This is probably a bit too conservative. For example,
        % `any->any' moded non-locals are considered to be outputs.
        set_of_var.is_empty(OutputVars),

        % Don't propagate impure goals.
        goal_info_get_purity(GoalInfo) = purity_pure,

        % Only propagate goals that cannot loop or throw exceptions.
        GoalCanLoopOrThrow = cannot_loop_or_throw
    then
        % It is a constraint, so add it to the list of constraints
        % to be attached to goals earlier in the conjunction.
        ConstrainedGoals1 = ConstrainedGoals0,
        Constraint = constraint(hlds_goal(GoalExpr, GoalInfo), ChangedVars,
            IncompatibleInstVars, []),
        Constraints1 = [Constraint | Constraints0]
    else if
        % Look for a simple goal which some constraint depends on
        % which can be propagated backwards. This handles cases like
        % X = 2, Y < X. This should only be done for goals which result in
        % no execution at runtime, such as construction of static constants.
        % Currently we only allow constructions of zero arity constants.

        % Make a renamed copy of the goal, renaming within the constraint
        % as well, so that a copy of the constant doesn't need to be kept
        % on the stack.
        Goal = hlds_goal(unify(_, _, _, Unify, _), _),
        Unify = construct(ConstructVar, _, [], _, _, _, _)
    then
        ConstrainedGoals1 = [constrained_goal(Goal, []) | ConstrainedGoals0],
        add_constant_construction(ConstructVar, Goal,
            Constraints0, Constraints1, !Info),

        % If the constraint was the only use of the constant, the old goal
        % can be removed. We need to rerun quantification to work that out.
        !Info ^ constr_changed := yes
    else if
        % Prune away the constraints after a goal that cannot succeed
        % -- they can never be executed.
        Detism = goal_info_get_determinism(GoalInfo),
        determinism_components(Detism, _, at_most_zero)
    then
        constraint_info_update_changed(Constraints0, !Info),
        Constraints1 = [],
        ConstrainedGoals1 = [constrained_goal(Goal, []) | ConstrainedGoals0]
    else if
        % Don't propagate constraints into or past impure goals
        % or trace goals.
        Goal = hlds_goal(_, GoalInfo),
        ( goal_info_get_purity(GoalInfo) = purity_impure
        ; goal_info_has_feature(GoalInfo, feature_contains_trace)
        )
    then
        Constraints1 = [],
        flatten_constraints(Constraints0, ConstraintGoals),
        list.map(add_empty_constraints, [Goal | ConstraintGoals],
            NewConstrainedGoals),
        ConstrainedGoals1 = NewConstrainedGoals ++ ConstrainedGoals0
    else if
        % Don't move goals which can fail before a goal which can loop
        % or throw an exception if `--fully-strict' is set.
        not goal_cannot_loop_or_throw(ModuleInfo, Goal),
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, fully_strict, yes)
    then
        filter_dependent_constraints(NonLocals, ChangedVars,
            Constraints0, DependentConstraints, IndependentConstraints),
        flatten_constraints(IndependentConstraints,
            IndependentConstraintGoals),
        list.map(add_empty_constraints, IndependentConstraintGoals,
            NewConstrainedGoals),
        ConstrainedGoals1 = [attach_constraints(Goal, DependentConstraints)
            | NewConstrainedGoals] ++ ConstrainedGoals0,
        Constraints1 = []
    else
        filter_dependent_constraints(NonLocals, OutputVars,
            Constraints0, DependentConstraints, IndependentConstraints),
        Constraints1 = IndependentConstraints,
        ConstrainedGoals1 = [attach_constraints(Goal, DependentConstraints) |
            ConstrainedGoals0]
    ),
    annotate_conj_constraints(ModuleInfo, RevConjuncts0, Constraints1,
        ConstrainedGoals1, ConstrainedGoals, !Info).

:- pred add_empty_constraints(hlds_goal::in, constrained_goal::out) is det.

add_empty_constraints(Goal, constrained_goal(Goal, [])).

:- func attach_constraints(hlds_goal, list(constraint)) = constrained_goal.

attach_constraints(Goal, Constraints0) = constrained_goal(Goal, Constraints) :-
    ( if Goal = hlds_goal(plain_call(_, _, _, _, _, _), _) then
        list.map(add_constraint_feature_to_constraint,
            Constraints0, Constraints)
    else
        Constraints = Constraints0
    ).

:- pred add_constraint_feature_to_constraint(constraint::in, constraint::out)
    is det.

add_constraint_feature_to_constraint(Constraint0, Constraint) :-
    Constraint0 = constraint(Goal0, ChangedVars, IncompatibleInstVars,
        ConstructGoals0),
    add_constraint_feature_to_goal(Goal0, Goal),
    list.map(add_constraint_feature_to_goal, ConstructGoals0, ConstructGoals),
    Constraint = constraint(Goal, ChangedVars, IncompatibleInstVars,
        ConstructGoals).

:- pred add_constraint_feature_to_goal(hlds_goal::in, hlds_goal::out) is det.

add_constraint_feature_to_goal(Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    goal_info_add_feature(feature_constraint, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred add_constant_construction(prog_var::in, hlds_goal::in,
    list(constraint)::in, list(constraint)::out,
    constraint_info::in, constraint_info::out) is det.

add_constant_construction(_, _, [], [], !Info).
add_constant_construction(ConstructVar, Construct0,
        [Constraint0 | Constraints0], [Constraint | Constraints], !Info) :-
    Constraint0 = constraint(ConstraintGoal0, ChangedVars,
        IncompatibleInstVars, Constructs0),
    ( if
        ConstraintGoal0 = hlds_goal(_, ConstraintInfo),
        ConstraintNonLocals = goal_info_get_nonlocals(ConstraintInfo),
        set_of_var.member(ConstraintNonLocals, ConstructVar)
    then
        VarSet0 = !.Info ^ constr_varset,
        VarTypes0 = !.Info ^ constr_vartypes,
        varset.new_var(NewVar, VarSet0, VarSet),
        lookup_var_type(VarTypes0, ConstructVar, VarType),
        add_var_type(NewVar, VarType, VarTypes0, VarTypes),
        !Info ^ constr_varset := VarSet,
        !Info ^ constr_vartypes := VarTypes,
        Subn = map.singleton(ConstructVar, NewVar),
        rename_some_vars_in_goal(Subn, Construct0, Construct),
        Constructs = [Construct | Constructs0],
        rename_some_vars_in_goal(Subn, ConstraintGoal0, ConstraintGoal),
        Constraint = constraint(ConstraintGoal, ChangedVars,
            IncompatibleInstVars, Constructs)
    else
        Constraint = Constraint0
    ),
    add_constant_construction(ConstructVar, Construct0,
        Constraints0, Constraints, !Info).

%-----------------------------------------------------------------------------%

    % filter_dependent_constraints(GoalNonLocals, GoalOutputVars,
    %   Constraints, DependentConstraints, IndependentConstraints):
    %
    % Find all constraints which depend on the output variables of the
    % current goal in the conjunction being processed. The DependentConstraints
    % should be pushed into the current goal. The IndependentConstraints
    % should be moved to the left of the current goal, if the purity and
    % termination properties of the current goal allow that.
    %
:- pred filter_dependent_constraints(set_of_progvar::in, set_of_progvar::in,
    list(constraint)::in, list(constraint)::out, list(constraint)::out) is det.

filter_dependent_constraints(NonLocals, GoalOutputVars, Constraints,
        DependentConstraints, IndependentConstraints) :-
    filter_dependent_constraints_2(NonLocals, GoalOutputVars, Constraints,
        [], RevDependentConstraints, [], RevIndependentConstraints),
    list.reverse(RevDependentConstraints, DependentConstraints),
    list.reverse(RevIndependentConstraints, IndependentConstraints).

:- pred filter_dependent_constraints_2(set_of_progvar::in, set_of_progvar::in,
    list(constraint)::in,
    list(constraint)::in, list(constraint)::out,
    list(constraint)::in, list(constraint)::out) is det.

filter_dependent_constraints_2(_NonLocals, _OutputVars, [],
        !RevDependent, !RevIndependent).
filter_dependent_constraints_2(NonLocals, GoalOutputVars,
        [Constraint | Constraints], !RevDependent, !RevIndependent) :-
    Constraint = constraint(ConstraintGoal, _, IncompatibleInstVars, _),
    ConstraintGoal = hlds_goal(_, ConstraintGoalInfo),
    ConstraintNonLocals = goal_info_get_nonlocals(ConstraintGoalInfo),
    ( if
        (
            % A constraint is not independent of a goal if it uses
            % any of the output variables of that goal.
            set_of_var.intersect(ConstraintNonLocals, GoalOutputVars,
                OutputVarsUsedByConstraint),
            not set_of_var.is_empty(OutputVarsUsedByConstraint)
        ;
            % A constraint is not independent of a goal if it changes
            % the inst of a non-local of the goal in such a way that
            % the new inst is incompatible with the old inst (e.g. by
            % losing uniqueness).
            set_of_var.intersect(NonLocals, IncompatibleInstVars,
                IncompatibleInstVarsUsedByGoal),
            not set_of_var.is_empty(IncompatibleInstVarsUsedByGoal)
        ;
            % A constraint is not independent of a goal if it uses
            % any variables whose instantiatedness is changed
            % by any of the constraints already attached to the goal
            % (the dependent constraints will be attached to the goal
            % to be pushed into it by propagate_conj_sub_goal).
            list.member(EarlierConstraint, !.RevDependent),
            not can_reorder_constraints(EarlierConstraint, Constraint)
        )
    then
        !:RevDependent = [Constraint | !.RevDependent]
    else
        !:RevIndependent = [Constraint | !.RevIndependent]
    ),
    filter_dependent_constraints_2(NonLocals, GoalOutputVars, Constraints,
        !RevDependent, !RevIndependent).

%-----------------------------------------------------------------------------%

:- pred can_reorder_constraints(constraint::in, constraint::in) is semidet.

can_reorder_constraints(EarlierConstraint, Constraint) :-
    EarlierConstraint = constraint(_, EarlierChangedVars, _, _),
    Constraint = constraint(ConstraintGoal, _, _, _),
    ConstraintGoal = hlds_goal(_, ConstraintGoalInfo),
    ConstraintNonLocals = goal_info_get_nonlocals(ConstraintGoalInfo),
    set_of_var.intersect(EarlierChangedVars, ConstraintNonLocals,
        EarlierConstraintIntersection),
    set_of_var.is_empty(EarlierConstraintIntersection).

%-----------------------------------------------------------------------------%

    % Push the constraints into each conjunct.
    %
:- pred propagate_conj_constraints(constrained_conj::in,
    cord(hlds_goal)::in, cord(hlds_goal)::out,
    constraint_info::in, constraint_info::out) is det.

propagate_conj_constraints([], !GoalsCord, !Info).
propagate_conj_constraints([ConstrainedGoal0 | ConstrainedGoals0],
        !GoalsCord, !Info) :-
    ConstrainedGoal0 = constrained_goal(Goal, Constraints0),
    filter_complex_constraints(Constraints0,
        SimpleConstraints, ComplexConstraints0),
    propagate_conj_sub_goal(SimpleConstraints, Goal,
        GoalAndSimpleConstraints, !Info),
    constraint_info_update_goal(Goal, !Info),
    flatten_constraints(ComplexConstraints0, ComplexConstraints),
    !:GoalsCord = !.GoalsCord
        ++ cord.from_list(GoalAndSimpleConstraints)
        ++ cord.from_list(ComplexConstraints),
    propagate_conj_constraints(ConstrainedGoals0, !GoalsCord, !Info).

:- pred filter_complex_constraints(list(constraint)::in,
    list(constraint)::out, list(constraint)::out) is det.

filter_complex_constraints(Constraints,
        SimpleConstraints, ComplexConstraints) :-
    filter_complex_constraints_2(Constraints,
        [], RevSimpleConstraints, [], RevComplexConstraints),
    list.reverse(RevSimpleConstraints, SimpleConstraints),
    list.reverse(RevComplexConstraints, ComplexConstraints).

    % Don't attempt to push branched goals into other goals.
    %
:- pred filter_complex_constraints_2(list(constraint)::in,
    list(constraint)::in, list(constraint)::out,
    list(constraint)::in, list(constraint)::out) is det.

filter_complex_constraints_2([],
        !RevSimpleConstraints, !RevComplexConstraints).
filter_complex_constraints_2([Constraint | Constraints],
        !RevSimpleConstraints, !RevComplexConstraints) :-
    Constraint = constraint(ConstraintGoal, _, _, _),
    ( if
        goal_is_simple(ConstraintGoal),

        % Check whether this simple constraint can be reordered
        % with the complex constraints we have already found.
        (
            list.member(ComplexConstraint, !.RevComplexConstraints)
        =>
            can_reorder_constraints(ComplexConstraint, Constraint)
        )
    then
        !:RevSimpleConstraints = [Constraint | !.RevSimpleConstraints]
    else
        !:RevComplexConstraints = [Constraint | !.RevComplexConstraints]
    ),
    filter_complex_constraints_2(Constraints,
        !RevSimpleConstraints, !RevComplexConstraints).

:- pred goal_is_simple(hlds_goal::in) is semidet.

goal_is_simple(Goal) :-
    Goal = hlds_goal(GoalExpr, _),
    % XXX This code should be replaced with an explicit switch.
    (
        goal_expr_has_subgoals(GoalExpr) = does_not_have_subgoals
    ;
        GoalExpr = negation(SubGoal),
        goal_is_simple(SubGoal)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            true
        else
            goal_is_simple(SubGoal)
        )
    ).

%-----------------------------------------------------------------------------%

:- type constraint_info
    --->    constraint_info(
                constr_module_info  :: module_info,
                constr_vartypes     :: vartypes,
                constr_varset       :: prog_varset,
                constr_instmap      :: instmap,
                constr_changed      :: bool     % has anything changed.
            ).

constraint_info_init(ModuleInfo, VarTypes, VarSet, InstMap, ConstraintInfo) :-
    ConstraintInfo = constraint_info(ModuleInfo, VarTypes, VarSet,
        InstMap, no).

constraint_info_deconstruct(ConstraintInfo, ModuleInfo,
        VarTypes, VarSet, Changed) :-
    ConstraintInfo = constraint_info(ModuleInfo, VarTypes, VarSet, _, Changed).

:- pred constraint_info_update_goal(hlds_goal::in,
    constraint_info::in, constraint_info::out) is det.

constraint_info_update_goal(hlds_goal(_, GoalInfo), !Info) :-
    InstMap0 = !.Info ^ constr_instmap,
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstMapDelta, InstMap0, InstMap),
    !Info ^ constr_instmap := InstMap.

:- pred constraint_info_bind_var_to_functors(prog_var::in, cons_id::in,
    list(cons_id)::in, constraint_info::in, constraint_info::out) is det.

constraint_info_bind_var_to_functors(Var, MainConsId, OtherConsIds, !Info) :-
    InstMap0 = !.Info ^ constr_instmap,
    ModuleInfo0 = !.Info ^ constr_module_info,
    VarTypes = !.Info ^ constr_vartypes,
    lookup_var_type(VarTypes, Var, Type),
    bind_var_to_functors(Var, Type, MainConsId, OtherConsIds,
        InstMap0, InstMap, ModuleInfo0, ModuleInfo),
    !Info ^ constr_instmap := InstMap,
    !Info ^ constr_module_info := ModuleInfo.

    % If a non-empty list of constraints is pushed into a subgoal,
    % quantification, instmap_deltas and determinism need to be recomputed.
    %
:- pred constraint_info_update_changed(list(constraint)::in,
    constraint_info::in, constraint_info::out) is det.

constraint_info_update_changed(Constraints, !Info) :-
    (
        Constraints = []
    ;
        Constraints = [_ | _],
        !Info ^ constr_changed := yes
    ).

%-----------------------------------------------------------------------------%

    % Remove all `constraint' goal features from the goal_infos
    % of all subgoals of the given goal.
    %
:- pred strip_constraint_markers(hlds_goal::in, hlds_goal::out) is det.

strip_constraint_markers(Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    strip_constraint_markers_expr(GoalExpr0, GoalExpr),
    ( if goal_info_has_feature(GoalInfo0, feature_constraint) then
        goal_info_remove_feature(feature_constraint, GoalInfo0, GoalInfo)
    else
        GoalInfo = GoalInfo0
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred strip_constraint_markers_expr(hlds_goal_expr::in, hlds_goal_expr::out)
    is det.

strip_constraint_markers_expr(GoalExpr0, GoalExpr) :-
    (
        GoalExpr0 = conj(ConjType, SubGoals0),
        list.map(strip_constraint_markers, SubGoals0, SubGoals),
        GoalExpr = conj(ConjType, SubGoals)
    ;
        GoalExpr0 = disj(SubGoals0),
        list.map(strip_constraint_markers, SubGoals0, SubGoals),
        GoalExpr = disj(SubGoals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        list.map(strip_constraint_markers_case, Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        strip_constraint_markers(SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        strip_constraint_markers(SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        strip_constraint_markers(Cond0, Cond),
        strip_constraint_markers(Then0, Then),
        strip_constraint_markers(Else0, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        ( GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = shorthand(_)
        ),
        GoalExpr = GoalExpr0
    ).

:- pred strip_constraint_markers_case(case::in, case::out) is det.

strip_constraint_markers_case(Case0, Case) :-
    Case0 = case(MainConsId, OtherConsIds, SubGoal0),
    strip_constraint_markers(SubGoal0, SubGoal),
    Case = case(MainConsId, OtherConsIds, SubGoal).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.constraint.
%-----------------------------------------------------------------------------%
