%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: constraint.m
% Main author: stayl.
%
% The constraint propagation transformation attempts to improve
% the efficiency of a generate-and-test style program by statically
% scheduling constraints as early as possible, where a "constraint"
% is any pure goal which has no outputs, can fail and cannot loop.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds__constraint.

:- interface.

:- import_module hlds__hlds_goal, hlds__hlds_module, hlds__instmap.
:- import_module parse_tree__prog_data.
:- import_module bool, map.

:- type constraint_info.

	% constraint__propagate_constraints_in_goal pushes constraints
	% left and inward within a single goal. Specialized versions of
	% procedures which are called with constrained outputs are created
	% by deforest.m. Goals which deforest.m should try to propagate
	% into calls are annotated with a `constraint' goal feature.
:- pred constraint__propagate_constraints_in_goal(hlds_goal, hlds_goal,
		constraint_info, constraint_info).
:- mode constraint__propagate_constraints_in_goal(in, out, in, out) is det.

:- pred constraint_info_init(module_info, map(prog_var, type), prog_varset,
		instmap, constraint_info).
:- mode constraint_info_init(in, in, in, in, out) is det.

:- pred constraint_info_deconstruct(constraint_info, module_info,
		map(prog_var, type), prog_varset, bool).
:- mode constraint_info_deconstruct(in, out, out, out, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__goal_util, hlds__hlds_pred, hlds__hlds_module.
:- import_module hlds__hlds_data, hlds__passes_aux, hlds__goal_form.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__inst_match.
:- import_module check_hlds__purity.
:- import_module libs__options, libs__globals.

:- import_module assoc_list, list, require, set, std_util.
:- import_module string, term, varset.

%-----------------------------------------------------------------------------%

constraint__propagate_constraints_in_goal(Goal0, Goal) -->
	% We need to strip off any existing constraint markers first.
	% Constraint markers are meant to indicate where a constraint
	% is meant to be attached to a call, and that deforest.m should
	% consider creating a specialized version for the call.
	% If deforest.m rearranges the goal, the constraints may
	% not remain next to the call.
	{ Goal1 = strip_constraint_markers(Goal0) },
	constraint__propagate_goal(Goal1, [], Goal).

:- pred constraint__propagate_goal(hlds_goal, list(constraint),
		hlds_goal, constraint_info, constraint_info).
:- mode constraint__propagate_goal(in, in, out, in, out) is det.

constraint__propagate_goal(Goal0, Constraints, Goal) -->
	% We need to treat all single goals as conjunctions so that
	% constraint__propagate_conj can move the constraints to the
	% left of the goal if that is allowed.
	{ goal_to_conj_list(Goal0, Goals0) },
	constraint__propagate_conj(Goals0, Constraints, Goals),
	{ goal_list_nonlocals(Goals, NonLocals) },
	{ goal_list_instmap_delta(Goals, Delta) },
	{ goal_list_determinism(Goals, ConjDetism) },
	{ goal_info_init(NonLocals, Delta, ConjDetism, GoalInfo) },
	{ conj_list_to_goal(Goals, GoalInfo, Goal) }.

:- pred constraint__propagate_conj_sub_goal(hlds_goal, list(constraint),
		hlds_goals, constraint_info, constraint_info).
:- mode constraint__propagate_conj_sub_goal(in, in, out, 
		in, out) is det.

constraint__propagate_conj_sub_goal(Goal0, Constraints, Goals) -->
	{ Goal0 = GoalExpr0 - _ },
	( { goal_is_atomic(GoalExpr0) } ->
		[]
	;
		% If a non-empty list of constraints is pushed into a sub-goal,
		% quantification, instmap_deltas and determinism need to be
		% recomputed.
		constraint_info_update_changed(Constraints)
	),
	InstMap0 =^ instmap,
	constraint__propagate_conj_sub_goal_2(Goal0, Constraints, Goals),
	^ instmap := InstMap0.

:- pred constraint__propagate_conj_sub_goal_2(hlds_goal, list(constraint),
		list(hlds_goal), constraint_info, constraint_info).
:- mode constraint__propagate_conj_sub_goal_2(in, in, out, in, out) is det.

constraint__propagate_conj_sub_goal_2(conj(Goals0) - Info, Constraints, 
		[conj(Goals) - Info]) -->
	constraint__propagate_conj(Goals0, Constraints, Goals).

constraint__propagate_conj_sub_goal_2(disj(Goals0) - Info, Constraints,
		[disj(Goals) - Info]) -->
	constraint__propagate_disj(Goals0, Constraints, Goals).

constraint__propagate_conj_sub_goal_2(switch(Var, CanFail, Cases0) - Info,
		Constraints, [switch(Var, CanFail, Cases) - Info]) -->
	constraint__propagate_cases(Var, Cases0, Constraints, Cases).
	
constraint__propagate_conj_sub_goal_2(
		if_then_else(Vars, Cond0, Then0, Else0) - Info,
		Constraints, 
		[if_then_else(Vars, Cond, Then, Else) - Info]) -->
	InstMap0 =^ instmap,

	% We can't safely propagate constraints into 
	% the condition of an if-then-else, because that
	% would change the answers generated by the procedure.
	constraint__propagate_goal(Cond0, [], Cond),
	constraint_info_update_goal(Cond),
	constraint__propagate_goal(Then0, Constraints, Then),
	^ instmap := InstMap0,
	constraint__propagate_goal(Else0, Constraints, Else).

	% XXX propagate constraints into par_conjs -- this isn't
	% possible at the moment because par_conj goals must have
	% determinism det.
constraint__propagate_conj_sub_goal_2(par_conj(Goals0) - GoalInfo,
		Constraints0,
		[par_conj(Goals) - GoalInfo | Constraints]) -->
	% Propagate constraints within the goals of the conjunction.
	% constraint__propagate_disj treats its list of goals as
	% independent rather than specifically disjoint, so we can
	% use it to process a list of independent parallel conjuncts.
	constraint__propagate_disj(Goals0, [], Goals),
	{ constraint__flatten_constraints(Constraints0, Constraints) }.

constraint__propagate_conj_sub_goal_2(some(Vars, CanRemove, Goal0) - GoalInfo,
		Constraints, [some(Vars, CanRemove, Goal) - GoalInfo]) -->
	constraint__propagate_goal(Goal0, Constraints, Goal).

constraint__propagate_conj_sub_goal_2(not(NegGoal0) - GoalInfo, Constraints0,
		[not(NegGoal) - GoalInfo | Constraints]) -->
	% We can't safely propagate constraints into a negation,
	% because that would change the answers computed by the
	% procedure.
	constraint__propagate_goal(NegGoal0, [], NegGoal),
	{ constraint__flatten_constraints(Constraints0, Constraints) }.

constraint__propagate_conj_sub_goal_2(Goal, Constraints0,
		[Goal | Constraints]) -->
	% constraint__propagate_conj will move the constraints
	% to the left of the call if that is possible, so nothing
	% needs to be done here.
	{ Goal = call(_, _, _, _, _, _) - _ },
	{ constraint__flatten_constraints(Constraints0, Constraints) }.

constraint__propagate_conj_sub_goal_2(Goal, Constraints0,
		[Goal | Constraints]) -->
	{ Goal = generic_call(_, _, _, _) - _ },
	{ constraint__flatten_constraints(Constraints0, Constraints) }.

constraint__propagate_conj_sub_goal_2(Goal, Constraints0,
		[Goal | Constraints]) -->
	{ Goal = foreign_proc(_, _, _, _, _, _, _) - _ },
	{ constraint__flatten_constraints(Constraints0, Constraints) }.

constraint__propagate_conj_sub_goal_2(Goal, _, _) -->
	{ Goal = shorthand(_) - _ },
	{ error("constraint__propagate_conj_sub_goal_2: shorthand") }.

constraint__propagate_conj_sub_goal_2(Goal, Constraints0,
		[Goal | Constraints]) -->
	{ Goal = unify(_, _, _, _, _) - _ },
	{ constraint__flatten_constraints(Constraints0, Constraints) }.

%-----------------------------------------------------------------------------%

	% Put the constant constructions in front of the constraint.
:- pred constraint__flatten_constraints(list(constraint)::in,
		list(hlds_goal)::out) is det.

constraint__flatten_constraints(Constraints0, Goals) :-
	list__map(lambda([Constraint::in, Lists::out] is det, (
			Constraint = constraint(Goal, _, _, Constructs),
			Lists = [Constructs, [Goal]]
	)), Constraints0, GoalLists0),
	list__condense(GoalLists0, GoalLists),
	list__condense(GoalLists, Goals).

%-----------------------------------------------------------------------------%

:- pred constraint__propagate_disj(list(hlds_goal), list(constraint),
		list(hlds_goal), constraint_info, constraint_info).
:- mode constraint__propagate_disj(in, in, out, 
		in, out) is det.

constraint__propagate_disj([], _, []) --> [].
constraint__propagate_disj([Goal0 | Goals0], Constraints, [Goal | Goals]) -->
	InstMap0 =^ instmap,
	constraint__propagate_goal(Goal0, Constraints, Goal),
	^ instmap := InstMap0,
	constraint__propagate_disj(Goals0, Constraints, Goals).

%-----------------------------------------------------------------------------%

:- pred constraint__propagate_cases(prog_var, list(case), list(constraint), 
			list(case), constraint_info, constraint_info).
:- mode constraint__propagate_cases(in, in, in, out,
			in, out) is det.

constraint__propagate_cases(_, [], _, []) --> [].
constraint__propagate_cases(Var, [case(ConsId, Goal0) | Cases0], Constraints,
			[case(ConsId, Goal) | Cases]) -->
	InstMap0 =^ instmap,
	constraint_info_bind_var_to_functor(Var, ConsId),
	constraint__propagate_goal(Goal0, Constraints, Goal),
	^ instmap := InstMap0,
	constraint__propagate_cases(Var, Cases0, Constraints, Cases).

%-----------------------------------------------------------------------------%

	% constraint__propagate_conj detects the constraints in
	% a conjunction and moves them to as early as possible
	% in the list. Some effort is made to keep the constraints
	% in the same order as they are encountered to increase
	% the likelihood of folding recursive calls.
:- pred constraint__propagate_conj(list(hlds_goal), list(constraint), 
		list(hlds_goal), constraint_info, constraint_info).
:- mode constraint__propagate_conj(in, in, out,
		in, out) is det.

constraint__propagate_conj(Goals0, Constraints, Goals) -->
	constraint_info_update_changed(Constraints),
	( { Goals0 = [] } ->
		{ constraint__flatten_constraints(Constraints, Goals) }
	; { Goals0 = [Goal0], Constraints = [] } ->
		constraint__propagate_conj_sub_goal(Goal0, [], Goals)
	;
		InstMap0 =^ instmap,
		ModuleInfo =^ module_info,
		VarTypes =^ vartypes,
		{ constraint__annotate_conj_output_vars(Goals0, ModuleInfo,
			VarTypes, InstMap0, [], RevGoals1) },
		constraint__annotate_conj_constraints(ModuleInfo, RevGoals1, 
			Constraints, [], Goals2),
		constraint__propagate_conj_constraints(Goals2, [], Goals)
	).

	% Annotate each conjunct with the variables it produces.
:- pred constraint__annotate_conj_output_vars(list(hlds_goal), module_info,
		vartypes, instmap, annotated_conj, annotated_conj).
:- mode constraint__annotate_conj_output_vars(in, in, in, in, in, out) is det.

constraint__annotate_conj_output_vars([], _, _, _, RevGoals, RevGoals).
constraint__annotate_conj_output_vars([Goal | Goals], ModuleInfo, VarTypes,
		InstMap0, RevGoals0, RevGoals) :-
	Goal = _ - GoalInfo,
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),

	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap),
	instmap_changed_vars(InstMap0, InstMap, VarTypes,
		ModuleInfo, ChangedVars0),

	instmap__vars_list(InstMap, InstMapVars),

	% Restrict the set of changed variables down to the set
	% for which the new inst is not an acceptable subsitute
	% for the old. This is done to allow reordering of a goal which
	% uses a variable with inst `ground(shared, no)' with
	% a constraint which just adds information, changing the inst
	% to `bound(shared, ...)'.
	InCompatible = (pred(Var::in) is semidet :-
			instmap__lookup_var(InstMap0, Var, InstBefore),
			instmap_delta_search_var(InstMapDelta, Var, InstAfter),
			\+ inst_matches_initial(InstAfter, InstBefore,
				map__lookup(VarTypes, Var), ModuleInfo)
		),
	IncompatibleInstVars = set__list_to_set(
			list__filter(InCompatible, InstMapVars)),

	%
	% This will consider variables with inst `any' to be bound by
	% the goal, so goals which have non-locals with inst `any' will
	% not be considered to be constraints. XXX This is too conservative.
	%
	Bound = (pred(Var::in) is semidet :-
			instmap__lookup_var(InstMap0, Var, InstBefore),
			instmap_delta_search_var(InstMapDelta, Var, InstAfter),
			\+ inst_matches_binding(InstAfter, InstBefore,
				map__lookup(VarTypes, Var), ModuleInfo)
		),
	BoundVars = set__list_to_set(list__filter(Bound, InstMapVars)),

	% 
	% Make sure that variables with inst `any' are placed in
	% the changed vars set. XXX This is too conservative, but
	% avoids unexpected reorderings.
	%
	set__union(ChangedVars0, BoundVars, ChangedVars),

	AnnotatedConjunct = annotated_conjunct(Goal, ChangedVars, BoundVars,
				IncompatibleInstVars),
	constraint__annotate_conj_output_vars(Goals, ModuleInfo, VarTypes,
		InstMap, [AnnotatedConjunct | RevGoals0], RevGoals).

%-----------------------------------------------------------------------------%

	% Conjunction annotated with the variables each conjunct
	% changes the instantiatedness of.
:- type annotated_conj == list(annotated_conjunct).

:- type annotated_conjunct
	---> annotated_conjunct(
		hlds_goal,

			% All variables returned by instmap_changed_vars.
		set(prog_var),

			% All variables returned by instmap_changed_vars for
			% which inst_matches_binding(NewInst, OldInst) fails.
		set(prog_var),

			% Variables returned by instmap_changed_vars
			% for which the new inst cannot be substituted
			% for the old as an input to a goal
			% (inst_matches_initial(NewInst, OldInst) fails).
		set(prog_var)
	).

	% A constraint is a goal with no outputs which can fail and
	% always terminates.
:- type constraint
	---> constraint(
			% The constraint itself.
		hlds_goal,

			% All variables returned by instmap_changed_vars.
		set(prog_var),

			% Variables returned by instmap_changed_vars
			% for which the new inst cannot be substituted
			% for the old as an input to a goal
			% (inst_matches_initial(NewInst, OldInst) fails).
		set(prog_var),
		
			% Goals to construct constants used by the constraint.
			% (as in X = 2, Y < X). These need to be propagated
			% with the constraint.
		list(hlds_goal)
	).

	% Conjunction annotated with constraining goals.
:- type constrained_conj == assoc_list(hlds_goal, list(constraint)).

	% Pass backwards over the conjunction, annotating each conjunct
	% with the constraints that should be pushed into it.
:- pred constraint__annotate_conj_constraints(module_info, annotated_conj,
	list(constraint), constrained_conj, constrained_conj,
	constraint_info, constraint_info).
:- mode constraint__annotate_conj_constraints(in, in, in,
	in, out, in, out) is det.

constraint__annotate_conj_constraints(_, [], Constraints0, Goals0, Goals) -->
	{ constraint__flatten_constraints(Constraints0, Constraints1) },
	{ list__map(lambda([Goal::in, CnstrGoal::out] is det, (
			CnstrGoal = Goal - []
		)), Constraints1, Constraints) },
	{ list__append(Constraints, Goals0, Goals) }.
constraint__annotate_conj_constraints(ModuleInfo, 
		[Conjunct | RevConjuncts0],
		Constraints0, Goals0, Goals) -->
	{ Conjunct = annotated_conjunct(Goal, ChangedVars,
			OutputVars, IncompatibleInstVars) },
	{ Goal = GoalExpr - GoalInfo },
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	(
		% Propagate goals with no output variables which can fail.
		% Propagating cc_nondet goals would be tricky, because we
		% would need to be careful about reordering the constraints
		% (the cc_nondet goal can't be moved before any goals
		% which can fail).
		% 
		{ goal_info_get_determinism(GoalInfo, Detism) },
		{ Detism = semidet
		; Detism = failure
		},

		%
		% XXX This is probably a bit too conservative. For
		% example, `any->any' moded non-locals are considered
		% to be outputs.
		%
		{ set__empty(OutputVars) },

		% Don't propagate impure goals.
		{ goal_info_is_pure(GoalInfo) },

		% Don't propagate goals that can loop. 
		{ goal_cannot_loop(ModuleInfo, Goal) }
	->
		% It's a constraint, add it to the list of constraints
		% to be attached to goals earlier in the conjunction.
		{ Goals1 = Goals0 },
		{ Constraint = constraint(GoalExpr - GoalInfo,
				ChangedVars, IncompatibleInstVars, []) },
		{ Constraints1 = [Constraint | Constraints0] }
	;
		%
		% Look for a simple goal which some constraint depends
		% on which can be propagated backwards. This handles
		% cases like X = 2, Y < X. This should only be done for
		% goals which result in no execution at runtime, such
		% as construction of static constants. Currently we only
		% allow constructions of zero arity constants.
		% 
		% Make a renamed copy of the goal, renaming within
		% the constraint as well, so that a copy of the constant
		% doesn't need to be kept on the stack.
		%
		{ Goal = unify(_, _, _, Unify, _) - _ },
		{ Unify = construct(ConstructVar, _, [], _, _, _, _) }
	->
		{ Goals1 = [Goal - [] | Goals0] },
		constraint__add_constant_construction(ConstructVar, Goal,
			Constraints0, Constraints1),

		% If the constraint was the only use of the constant,
		% the old goal can be removed. We need to rerun
		% quantification to work that out.
		^ changed := yes
	;	
		% Prune away the constraints after a goal
		% which cannot succeed -- they can never be
		% executed.
		{ goal_info_get_determinism(GoalInfo, Detism) },
		{ determinism_components(Detism, _, at_most_zero) }
	->
		constraint_info_update_changed(Constraints0),
		{ Constraints1 = [] },
		{ Goals1 = [Goal - [] | Goals0] }	
	;
		% Don't propagate constraints into or past impure goals.
		{ Goal = _ - GoalInfo },
		{ goal_info_is_impure(GoalInfo) }
	->
		{ Constraints1 = [] },
		{ constraint__flatten_constraints(Constraints0,
			ConstraintGoals) },
		{ list__map(add_empty_constraints, [Goal | ConstraintGoals],
			GoalsAndConstraints) },
		{ list__append(GoalsAndConstraints, Goals0, Goals1) }
	;
		% Don't move goals which can fail before a goal which
		% can loop if `--fully-strict' is set.
		{ \+ goal_cannot_loop(ModuleInfo, Goal) },
		{ module_info_globals(ModuleInfo, Globals) },
		{ globals__lookup_bool_option(Globals, fully_strict, yes) }
	->
		{ constraint__filter_dependent_constraints(NonLocals,
			ChangedVars, Constraints0, DependentConstraints,
			IndependentConstraints) },
		{ constraint__flatten_constraints(IndependentConstraints,
			IndependentConstraintGoals) },
		{ list__map(add_empty_constraints, IndependentConstraintGoals,
			GoalsAndConstraints) },
		{ Goals1 = 
			[attach_constraints(Goal, DependentConstraints)
				| GoalsAndConstraints]
			++ Goals0 },
		{ Constraints1 = [] }
	;
		{ constraint__filter_dependent_constraints(NonLocals,
			OutputVars, Constraints0, DependentConstraints,
			IndependentConstraints) },
		{ Constraints1 = IndependentConstraints },
		{ Goals1 = [attach_constraints(Goal, DependentConstraints)
				| Goals0] }
	),
	constraint__annotate_conj_constraints(ModuleInfo, RevConjuncts0, 
		Constraints1, Goals1, Goals).

:- pred add_empty_constraints(hlds_goal::in,
		pair(hlds_goal, list(constraint))::out) is det.

add_empty_constraints(Goal, Goal - []).

:- func attach_constraints(hlds_goal, list(constraint)) =
		pair(hlds_goal, list(constraint)).

attach_constraints(Goal, Constraints0) = Goal - Constraints :-
        ( Goal = call(_, _, _, _, _, _) - _ ->
                Constraints = list__map(
                    (func(constraint(Goal0, B, C, Constructs0)) =
                        constraint(add_constraint_feature(Goal0), B, C,
                            list__map(add_constraint_feature, Constructs0))
                    ), Constraints0)
        ;
                Constraints = Constraints0
        ).

:- func add_constraint_feature(hlds_goal) = hlds_goal.

add_constraint_feature(Goal - GoalInfo0) = Goal - GoalInfo :-
	goal_info_add_feature(GoalInfo0, constraint, GoalInfo).

%-----------------------------------------------------------------------------%


:- pred constraint__add_constant_construction(prog_var::in, hlds_goal::in,
		list(constraint)::in, list(constraint)::out,
		constraint_info::in, constraint_info::out) is det.

constraint__add_constant_construction(_, _, [], []) --> [].
constraint__add_constant_construction(ConstructVar, Construct0,
		[Constraint0 | Constraints0],
		[Constraint | Constraints]) -->
	{ Constraint0 = constraint(ConstraintGoal0, ChangedVars,
				IncompatibleInstVars, Constructs0) },
	(
		{ ConstraintGoal0 = _ - ConstraintInfo },
		{ goal_info_get_nonlocals(ConstraintInfo,
			ConstraintNonLocals) },
		{ set__member(ConstructVar, ConstraintNonLocals) }
	->
		VarSet0 =^ varset,
		VarTypes0 =^ vartypes,
		{ varset__new_var(VarSet0, NewVar, VarSet) },
		{ map__lookup(VarTypes0, ConstructVar, VarType) },
		{ map__det_insert(VarTypes0, NewVar, VarType, VarTypes) },
		^ varset := VarSet,
		^ vartypes := VarTypes,
		{ map__from_assoc_list([ConstructVar - NewVar], Subn) },
		{ goal_util__rename_vars_in_goal(Construct0,
			Subn, Construct) },
		{ Constructs = [Construct | Constructs0] },
		{ goal_util__rename_vars_in_goal(ConstraintGoal0,
			Subn, ConstraintGoal) },
		{ Constraint = constraint(ConstraintGoal, ChangedVars,
				IncompatibleInstVars, Constructs) }
	;
		{ Constraint = Constraint0 }
	),
	constraint__add_constant_construction(ConstructVar, Construct0,
		Constraints0, Constraints). 

%-----------------------------------------------------------------------------%

	% constraints__filter_dependent_constraints(GoalNonLocals,
	%	GoalOutputVars, Constraints, DependentConstraints,
	% 	IndependentConstraints)
	%
	% Find all constraints which depend on the output variables
	% of the current goal in the conjunction being processed.
	% The DependentConstraints should be pushed into the current goal.
	% The IndependentConstraints should be moved to the left of
	% the current goal, if the purity and termination properties
	% of the current goal allow that.
:- pred constraint__filter_dependent_constraints(set(prog_var), set(prog_var),
		list(constraint), list(constraint), list(constraint)).
:- mode constraint__filter_dependent_constraints(in, in, in,
		out, out) is det.

constraint__filter_dependent_constraints(NonLocals, GoalOutputVars,
		Constraints, Dependent, Independent) :-
	constraint__filter_dependent_constraints(NonLocals, GoalOutputVars,
		Constraints, [], Dependent, [], Independent).

:- pred constraint__filter_dependent_constraints(set(prog_var), set(prog_var),
		list(constraint), list(constraint), list(constraint),
		list(constraint), list(constraint)).
:- mode constraint__filter_dependent_constraints(in, in, in,
		in, out, in, out) is det.

constraint__filter_dependent_constraints(_NonLocals, _OutputVars, [],
		RevDependent, Dependent, RevIndependent, Independent) :-
	list__reverse(RevDependent, Dependent),
	list__reverse(RevIndependent, Independent).
constraint__filter_dependent_constraints(NonLocals, GoalOutputVars,
		[Constraint | Constraints], Dependent0, Dependent,
		Independent0, Independent) :-
	Constraint = constraint(ConstraintGoal, _, IncompatibleInstVars, _),
	ConstraintGoal = _ - ConstraintGoalInfo,
	goal_info_get_nonlocals(ConstraintGoalInfo, ConstraintNonLocals),

	(
		(
			%
			% A constraint is not independent of a goal
			% if it uses any of the output variables
			% of that goal.
			%
			set__intersect(ConstraintNonLocals, GoalOutputVars,
				OutputVarsUsedByConstraint),
			\+ set__empty(OutputVarsUsedByConstraint)
		;
			%
			% A constraint is not independent of a goal
			% if it changes the inst of a non-local of the goal
			% in such a way that the new inst is incompatible
			% with the old inst (e.g. by losing uniqueness),
			%
			set__intersect(NonLocals, IncompatibleInstVars,
				IncompatibleInstVarsUsedByGoal),
			\+ set__empty(IncompatibleInstVarsUsedByGoal)
		;
			% 
			% A constraint is not independent of a goal if
			% it uses any variables whose instantiatedness is
			% changed by any the of the constraints already
			% attached to the goal (the dependent constraints
			% will be attached to the goal to be pushed into
			% it by constraint__propagate_conj_sub_goal).
			%
			list__member(EarlierConstraint, Dependent0),
			\+ can_reorder_constraints(EarlierConstraint,
				Constraint)
		)
	->
		Independent1 = Independent0,
		Dependent1 = [Constraint | Dependent0]
	;
		Independent1 = [Constraint | Independent0],
		Dependent1 = Dependent0
	),
	constraint__filter_dependent_constraints(NonLocals, GoalOutputVars,
		Constraints, Dependent1, Dependent, Independent1, Independent).

%-----------------------------------------------------------------------------%

:- pred can_reorder_constraints(constraint, constraint).
:- mode can_reorder_constraints(in, in) is semidet.

can_reorder_constraints(EarlierConstraint, Constraint) :-
	EarlierConstraint = constraint(_, EarlierChangedVars, _, _),
	Constraint = constraint(ConstraintGoal, _, _, _),
	ConstraintGoal = _ - ConstraintGoalInfo,
	goal_info_get_nonlocals(ConstraintGoalInfo, ConstraintNonLocals),
	set__intersect(EarlierChangedVars, ConstraintNonLocals,
			EarlierConstraintIntersection),
	set__empty(EarlierConstraintIntersection).
	
%-----------------------------------------------------------------------------%

	% Push the constraints into each conjunct.
:- pred constraint__propagate_conj_constraints(constrained_conj,
		list(hlds_goal), list(hlds_goal),
		constraint_info, constraint_info).
:- mode constraint__propagate_conj_constraints(in, in, out, in, out) is det.

constraint__propagate_conj_constraints([], RevGoals, Goals) --> 
	{ list__reverse(RevGoals, Goals) }.
constraint__propagate_conj_constraints([Goal0 - Constraints0 | Goals0],
		RevGoals0, RevGoals) -->
	{ constraint__filter_complex_constraints(Constraints0,
		SimpleConstraints, ComplexConstraints0) },
	constraint__propagate_conj_sub_goal(Goal0,
		SimpleConstraints, GoalList1),
	{ constraint__flatten_constraints(ComplexConstraints0,
		ComplexConstraints) },
	{ list__reverse(ComplexConstraints, RevComplexConstraints) },
	{ list__reverse(GoalList1, RevGoalList1) },
	{ list__condense([RevComplexConstraints, RevGoalList1, RevGoals0],
		RevGoals1) },
	constraint_info_update_goal(Goal0),
	constraint__propagate_conj_constraints(Goals0, RevGoals1, RevGoals).

:- pred constraint__filter_complex_constraints(list(constraint),
		list(constraint), list(constraint)).
:- mode constraint__filter_complex_constraints(in, out, out) is det.
		
constraint__filter_complex_constraints(Constraints,
		SimpleConstraints, ComplexConstraints) :-
	constraint__filter_complex_constraints(Constraints,
		[], SimpleConstraints, [], ComplexConstraints).

	% Don't attempt to push branched goals into other goals.
:- pred constraint__filter_complex_constraints(list(constraint),
		list(constraint), list(constraint),
		list(constraint), list(constraint)).
:- mode constraint__filter_complex_constraints(in, in, out, in, out) is det.

constraint__filter_complex_constraints([], SimpleConstraints,
		list__reverse(SimpleConstraints),
		ComplexConstraints, list__reverse(ComplexConstraints)).
constraint__filter_complex_constraints([Constraint | Constraints],
		SimpleConstraints0, SimpleConstraints,
		ComplexConstraints0, ComplexConstraints) :-
	Constraint = constraint(ConstraintGoal, _, _, _),
	(
		constraint__goal_is_simple(ConstraintGoal),

		%
		% Check whether this simple constraint can be reordered
		% with the complex constraints we've already found.
		%
		\+ (
			list__member(ComplexConstraint, ComplexConstraints0),
			\+ can_reorder_constraints(ComplexConstraint,
				Constraint)
		)
	->
		SimpleConstraints1 = [Constraint | SimpleConstraints0],
		ComplexConstraints1 = ComplexConstraints0
	;
		SimpleConstraints1 = SimpleConstraints0,
		ComplexConstraints1 = [Constraint | ComplexConstraints0]
	),
	constraint__filter_complex_constraints(Constraints, SimpleConstraints1,
		SimpleConstraints, ComplexConstraints1, ComplexConstraints).

:- pred constraint__goal_is_simple(hlds_goal).
:- mode constraint__goal_is_simple(in) is semidet.

constraint__goal_is_simple(Goal) :-
	Goal = GoalExpr - _,
	(
		goal_is_atomic(GoalExpr)
	;
		( GoalExpr = some(_, _, SubGoal)
		; GoalExpr = not(SubGoal)
		),
		constraint__goal_is_simple(SubGoal)
	).

%-----------------------------------------------------------------------------%

:- type constraint_info
	---> constraint_info(
		module_info :: module_info,
		vartypes :: map(prog_var, type),
		varset :: prog_varset,
		instmap :: instmap,
		changed :: bool			% has anything changed.
	).

constraint_info_init(ModuleInfo, VarTypes, VarSet, InstMap, ConstraintInfo) :-
	ConstraintInfo = constraint_info(ModuleInfo, VarTypes, VarSet,
		InstMap, no).

constraint_info_deconstruct(ConstraintInfo, ModuleInfo,
		VarTypes, VarSet, Changed) :-
	ConstraintInfo = constraint_info(ModuleInfo, VarTypes, VarSet,
		_, Changed).

:- pred constraint_info_update_goal(hlds_goal::in,
		constraint_info::in, constraint_info::out) is det.

constraint_info_update_goal(_ - GoalInfo) -->
	InstMap0 =^ instmap,
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	{ instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap) },
	^ instmap := InstMap.

:- pred constraint_info_bind_var_to_functor(prog_var::in, cons_id::in,
		constraint_info::in, constraint_info::out) is det.

constraint_info_bind_var_to_functor(Var, ConsId) -->
	InstMap0 =^ instmap,
	ModuleInfo0 =^ module_info,
	VarTypes =^ vartypes,
	{ map__lookup(VarTypes, Var, Type) },
	{ instmap__bind_var_to_functor(Var, Type, ConsId, InstMap0, InstMap,
		ModuleInfo0, ModuleInfo) },
	^ instmap := InstMap,
	^ module_info := ModuleInfo.

	% If a non-empty list of constraints is pushed into a sub-goal,
	% quantification, instmap_deltas and determinism need to be
	% recomputed.
:- pred constraint_info_update_changed(list(constraint)::in,
		constraint_info::in, constraint_info::out) is det.

constraint_info_update_changed(Constraints) -->
	( { Constraints = [] } ->
		[]
	;
		^ changed := yes
	).

%-----------------------------------------------------------------------------%

	% Remove all `constraint' goal features from the goal_infos
	% of all sub-goals of the given goal.
:- func strip_constraint_markers(hlds_goal) = hlds_goal.

strip_constraint_markers(Goal - GoalInfo0) =
		strip_constraint_markers_expr(Goal) - GoalInfo :-
	( goal_info_has_feature(GoalInfo0, constraint) ->
		goal_info_remove_feature(GoalInfo0, constraint, GoalInfo)
	;
		GoalInfo = GoalInfo0
	).

:- func strip_constraint_markers_expr(hlds_goal_expr) = hlds_goal_expr.

strip_constraint_markers_expr(conj(Goals)) =
		conj(list__map(strip_constraint_markers, Goals)).
strip_constraint_markers_expr(disj(Goals)) =
		disj(list__map(strip_constraint_markers, Goals)).
strip_constraint_markers_expr(switch(Var, CanFail, Cases0)) =
		switch(Var, CanFail, Cases) :-
	Cases = list__map(
		    (func(case(ConsId, Goal)) =
			case(ConsId, strip_constraint_markers(Goal))
		    ), Cases0).
strip_constraint_markers_expr(not(Goal)) =
		not(strip_constraint_markers(Goal)).
strip_constraint_markers_expr(some(Vars, Remove, Goal)) =
		some(Vars, Remove, strip_constraint_markers(Goal)).
strip_constraint_markers_expr(if_then_else(Vars, If, Then, Else)) =
		if_then_else(Vars, strip_constraint_markers(If),
			strip_constraint_markers(Then),
			strip_constraint_markers(Else)).
strip_constraint_markers_expr(par_conj(Goals)) =
		par_conj(list__map(strip_constraint_markers, Goals)).
strip_constraint_markers_expr(Goal) = Goal :-
	Goal = foreign_proc(_, _, _, _, _, _, _).
strip_constraint_markers_expr(Goal) = Goal :-
	Goal = generic_call(_, _, _, _).
strip_constraint_markers_expr(Goal) = Goal :-
	Goal = call(_, _, _, _, _, _).
strip_constraint_markers_expr(Goal) = Goal :-
	Goal = unify(_, _, _, _, _).
strip_constraint_markers_expr(Goal) = Goal :-
	Goal = shorthand(_).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
