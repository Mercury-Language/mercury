%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: quantification.m.
% Main authors: fjh, conway.

	% Make implicit quantification explicit, and rename apart
	% variables with the same name that appear in distinct scopes.
	% For the rules on implicit quantification, see the
	% Mercury language reference manual.
	%
	% Rather than making implicit quantification explicit by
	% inserting additional existential quantifiers in the form of
	% `some/2' goals, we instead record existential quantification
	% in the goal_info for each goal.  In fact we could (should?)
	% even delete any explicit existential quantifiers that were
	% present in the source code, since the information they convey
	% will be stored in the goal_info, although currently we don't
	% do that.
	% 
	% The important piece of information that later stages of the
	% compilation process want to know is "Does this goal bind any
	% of its non-local variables?".  So, rather than storing a list
	% of the variables which _are_ existentially quantified in the
	% goal_info, we store the set of variables which are _not_
	% quantified.

%-----------------------------------------------------------------------------%

:- module quantification.

:- interface.

:- import_module hlds_goal, hlds_pred, prog_data.
:- import_module list, set, term, varset.

:- pred implicitly_quantify_clause_body(list(var),
		hlds_goal, varset, map(var, type),
		hlds_goal, varset, map(var, type), list(quant_warning)).
:- mode implicitly_quantify_clause_body(in, in, in, in, out, out, out, out)
	is det.

:- pred implicitly_quantify_goal(hlds_goal, varset, map(var, type), set(var),
		hlds_goal, varset, map(var, type), list(quant_warning)).
:- mode implicitly_quantify_goal(in, in, in, in, out, out, out, out) is det.

:- pred requantify_proc(proc_info, proc_info) is det.
:- mode requantify_proc(in, out) is det.

	% We return a list of warnings back to make_hlds.m.
	% Currently the only thing we warn about is variables with
	% overlapping scopes.

:- type quant_warning
	--->	warn_overlap(list(var), term__context).

	% quantification__goal_vars(Goal, Vars):
	%	Vars is the set of variables that are free (unquantified)
	%	in Goal.
:- pred quantification__goal_vars(hlds_goal, set(var)).
:- mode quantification__goal_vars(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util, bool, map, goal_util, require.

	% The `outside vars', `lambda outside vars', and `quant vars'
	% fields are inputs; the `nonlocals' field is output; and
	% the `seen so far', the varset, the types, and the warnings fields
	% are threaded (i.e. both input and output).
	% We use the convention that the input fields are callee save,
	% and the outputs are caller save.
:- type quant_info
	--->	quant_info(
			set(var), % outside vars
			set(var), % quant vars
			set(var), % outside lambda vars
			set(var), % nonlocals
			set(var), % seen so far
			varset,
			map(var, type),
			list(quant_warning)
		).

	% `OutsideVars' are the variables that have occurred free outside
	% this goal, not counting occurrences in parallel goals
	% and not counting occurrences in lambda goals,
	% or which have been explicitly existentially quantified 
	% over a scope which includes the current goal in a negated context.
	% `QuantVars' are the variables not in `OutsideVars' 
	% that have been explicitly existentially quantified over a scope
	% which includes the current goal in a positive (non-negated) context.
	% `OutsideLambdaVars' are the variables
	% that have occurred free in a lambda
	% expression outside this goal, not counting occurrences in
	% parallel goals.
	%
	% For example, for
	%
	%	test :- some [X] (p(X) ; not q(X) ; r(X), s(X)).
	%
	% when processing `r(X), s(X)', OutsideVars will be [] and
	% QuantifiedVars will be [X]; when processing `r(X)',
	% OutsideVars will be [X] and QuantifiedVars will be [],
	% since now [X] has occured in a goal (`s(X)') outside of `r(X)'.
	% When processing `not q(X)', OutsideVars will be [] and
	% QuantifiedVars will be [X]; when processing `q(X)',
	% OutsideVars will be [X] and QuantifiedVars will be [],
	% since the quantification can't be pushed inside the negation.

%-----------------------------------------------------------------------------%

implicitly_quantify_clause_body(HeadVars, Goal0, Varset0, VarTypes0,
		Goal, Varset, VarTypes, Warnings) :-
	set__list_to_set(HeadVars, OutsideVars),
	implicitly_quantify_goal(Goal0, Varset0, VarTypes0,
			OutsideVars, Goal, Varset, VarTypes, Warnings).

requantify_proc(ProcInfo0, ProcInfo) :-
	proc_info_variables(ProcInfo0, Varset0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_headvars(ProcInfo0, HeadVars),
	proc_info_goal(ProcInfo0, Goal0),
	implicitly_quantify_clause_body(HeadVars, Goal0, Varset0, VarTypes0,
		Goal, Varset, VarTypes, _),
	proc_info_set_variables(ProcInfo0, Varset, ProcInfo1),
	proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo2),
	proc_info_set_goal(ProcInfo2, Goal, ProcInfo).

implicitly_quantify_goal(Goal0, Varset0, VarTypes0, OutsideVars,
				Goal, Varset, VarTypes, Warnings) :-
	quantification__init(OutsideVars, Varset0, VarTypes0, QuantInfo0),
	implicitly_quantify_goal(Goal0, Goal, QuantInfo0, QuantInfo),
	quantification__get_varset(Varset, QuantInfo, _),
	quantification__get_vartypes(VarTypes, QuantInfo, _),
	quantification__get_warnings(Warnings0, QuantInfo, _),
	list__reverse(Warnings0, Warnings).

:- pred implicitly_quantify_goal(hlds_goal, hlds_goal,
					quant_info, quant_info).
:- mode implicitly_quantify_goal(in, out, in, out) is det.

implicitly_quantify_goal(Goal0 - GoalInfo0, Goal - GoalInfo) -->
	quantification__get_seen(SeenVars),
	{ goal_info_get_context(GoalInfo0, Context) },
	implicitly_quantify_goal_2(Goal0, Context, Goal1),
	quantification__get_nonlocals(NonLocalVars),
	(
		% If there are any variables that are local to the goal
		% which we have come across before, then we rename them
		% apart.
		{ quantification__goal_vars(Goal0 - GoalInfo0, GoalVars0) },
		{ set__difference(GoalVars0, NonLocalVars, LocalVars) },
		{ set__intersect(SeenVars, LocalVars, RenameVars) },
		{ \+ set__empty(RenameVars) }
	->
		quantification__rename_apart(RenameVars, _, Goal1 - GoalInfo0,
				Goal - GoalInfo1)
	;
		{ Goal = Goal1 },
		{ GoalInfo1 = GoalInfo0 }
	),
	{ goal_info_set_nonlocals(GoalInfo1, NonLocalVars, GoalInfo) }.

:- pred implicitly_quantify_goal_2(hlds_goal_expr, term__context,
				hlds_goal_expr, quant_info, quant_info).
:- mode implicitly_quantify_goal_2(in, in, out, in, out) is det.

	% we retain explicit existential quantifiers in the source code,
	% even though they are redundant with the goal_info non_locals,
	% so that we can easily recalculate the goal_info non_locals
	% if necessary after program transformation.

implicitly_quantify_goal_2(some(Vars0, Goal0), Context, some(Vars, Goal)) -->
	quantification__get_outside(OutsideVars),
	quantification__get_lambda_outside(LambdaOutsideVars),
	quantification__get_quant_vars(QuantVars),
		% Rename apart all the quantified
		% variables that occur outside this goal.
	{ set__list_to_set(Vars0, QVars) },
	{ set__intersect(OutsideVars, QVars, RenameVars1) },
	{ set__intersect(LambdaOutsideVars, QVars, RenameVars2) },
	{ set__union(RenameVars1, RenameVars2, RenameVars) },
	(
		{ set__empty(RenameVars) }
	->
		{ Goal1 = Goal0 },
		{ Vars = Vars0 }
	;
		quantification__warn_overlapping_scope(RenameVars, Context),
		quantification__rename_apart(RenameVars, RenameMap,
			Goal0, Goal1),
		{ goal_util__rename_var_list(Vars0, no, RenameMap, Vars) }
	),
	quantification__update_seen_vars(QVars),
	{ set__insert_list(QuantVars, Vars, QuantVars1) },
	quantification__set_quant_vars(QuantVars1),
	implicitly_quantify_goal(Goal1, Goal),
	quantification__get_nonlocals(NonLocals0),
	{ set__delete_list(NonLocals0, Vars, NonLocals) },
	quantification__set_quant_vars(QuantVars),
	quantification__set_nonlocals(NonLocals).

implicitly_quantify_goal_2(conj(List0), _, conj(List)) -->
	implicitly_quantify_conj(List0, List).

implicitly_quantify_goal_2(disj(Goals0, SM), _, disj(Goals, SM)) -->
	implicitly_quantify_disj(Goals0, Goals).

implicitly_quantify_goal_2(switch(Var, Det, Cases0, SM), _,
					switch(Var, Det, Cases, SM)) -->
	implicitly_quantify_cases(Cases0, Cases),
		% The switch variable is guaranteed to be non-local to the
		% switch, since it has to be bound elsewhere, so we put it
		% in the nonlocals here.
	quantification__get_nonlocals(NonLocals0),
	{ set__insert(NonLocals0, Var, NonLocals) },
	quantification__set_nonlocals(NonLocals).

implicitly_quantify_goal_2(not(Goal0), _, not(Goal)) -->
		% quantified variables cannot be pushed inside a negation,
		% so we insert the quantified vars into the outside vars set,
		% and initialize the new quantified vars set to be empty
		% (the lambda outside vars remain unchanged)
	quantification__get_quant_vars(QuantVars),
	quantification__get_outside(OutsideVars),
	{ set__union(OutsideVars, QuantVars, OutsideVars1) },
	{ set__init(QuantVars1) },
	quantification__set_quant_vars(QuantVars1),
	quantification__set_outside(OutsideVars1),
	implicitly_quantify_goal(Goal0, Goal),
	quantification__set_outside(OutsideVars),
	quantification__set_quant_vars(QuantVars).

implicitly_quantify_goal_2(if_then_else(Vars0, Cond0, Then0, Else0, SM),
			Context, if_then_else(Vars, Cond, Then, Else, SM)) -->
	quantification__get_quant_vars(QuantVars),
	quantification__get_outside(OutsideVars),
	quantification__get_lambda_outside(LambdaOutsideVars),
	{ set__list_to_set(Vars0, QVars) },
		% Rename apart those variables that
		% are quantified to the cond and then
		% of the i-t-e that occur outside the
		% i-t-e.
	{ set__intersect(OutsideVars, QVars, RenameVars1) },
	{ set__intersect(LambdaOutsideVars, QVars, RenameVars2) },
	{ set__union(RenameVars1, RenameVars2, RenameVars) },
	(
		{ set__empty(RenameVars) }
	->
		{ Cond1 = Cond0 },
		{ Then1 = Then0 },
		{ Vars = Vars0 }
	;
		quantification__warn_overlapping_scope(RenameVars, Context),
		quantification__rename_apart(RenameVars, RenameMap,
						Cond0, Cond1),
		{ goal_util__rename_vars_in_goal(Then0, RenameMap, Then1) },
		{ goal_util__rename_var_list(Vars0, no, RenameMap, Vars) }
	),
	{ set__insert_list(QuantVars, Vars, QuantVars1) },
	{ quantification__goal_vars(Then1, VarsThen, LambdaVarsThen) },
	{ set__union(OutsideVars, VarsThen, OutsideVars1) },
	{ set__union(LambdaOutsideVars, LambdaVarsThen, LambdaOutsideVars1) },
	quantification__set_quant_vars(QuantVars1),
	quantification__set_outside(OutsideVars1),
	quantification__set_lambda_outside(LambdaOutsideVars1),
	quantification__update_seen_vars(QVars),
	implicitly_quantify_goal(Cond1, Cond),
	quantification__get_nonlocals(NonLocalsCond),
	{ set__union(OutsideVars, NonLocalsCond, OutsideVars2) },
	quantification__set_outside(OutsideVars2),
	quantification__set_lambda_outside(LambdaOutsideVars),
	implicitly_quantify_goal(Then1, Then),
	quantification__get_nonlocals(NonLocalsThen),
	quantification__set_outside(OutsideVars),
	quantification__set_quant_vars(QuantVars),
	implicitly_quantify_goal(Else0, Else),
	quantification__get_nonlocals(NonLocalsElse),
	{ set__union(NonLocalsCond, NonLocalsThen, NonLocalsIfThen) },
	{ set__union(NonLocalsIfThen, NonLocalsElse, NonLocalsIfThenElse) },
	{ set__intersect(NonLocalsIfThenElse, OutsideVars, NonLocals) },
	quantification__set_nonlocals(NonLocals).

implicitly_quantify_goal_2(call(A, B, HeadVars, D, E, F), _,
		call(A, B, HeadVars, D, E, F)) -->
	implicitly_quantify_atomic_goal(HeadVars).

implicitly_quantify_goal_2(higher_order_call(PredVar, ArgVars, C, D, E), _,
		higher_order_call(PredVar, ArgVars, C, D, E)) -->
	implicitly_quantify_atomic_goal([PredVar|ArgVars]).

implicitly_quantify_goal_2(unify(A, B0, X, Y, Z), Context,
		unify(A, B, X, Y, Z)) -->
	quantification__get_outside(OutsideVars),
	quantification__get_lambda_outside(LambdaOutsideVars),
	implicitly_quantify_unify_rhs(B0, Context, B),
	quantification__get_nonlocals(VarsB),
	{ set__insert(VarsB, A, GoalVars) },
	quantification__update_seen_vars(GoalVars),
	{ set__intersect(GoalVars, OutsideVars, NonLocalVars1) },
	{ set__intersect(GoalVars, LambdaOutsideVars, NonLocalVars2) },
	{ set__union(NonLocalVars1, NonLocalVars2, NonLocalVars) },
	quantification__set_nonlocals(NonLocalVars).

implicitly_quantify_goal_2(pragma_c_code(A,B,C,D,Vars,F,G,H), _,
		pragma_c_code(A,B,C,D,Vars,F,G,H)) --> 
	implicitly_quantify_atomic_goal(Vars).

:- pred implicitly_quantify_atomic_goal(list(var), quant_info, quant_info).
:- mode implicitly_quantify_atomic_goal(in, in, out) is det.

implicitly_quantify_atomic_goal(HeadVars) -->
	{ set__list_to_set(HeadVars, GoalVars) },
	quantification__update_seen_vars(GoalVars),
	quantification__get_outside(OutsideVars),
	quantification__get_lambda_outside(LambdaOutsideVars),
	{ set__intersect(GoalVars, OutsideVars, NonLocals1) },
	{ set__intersect(GoalVars, LambdaOutsideVars, NonLocals2) },
	{ set__union(NonLocals1, NonLocals2, NonLocals) },
	quantification__set_nonlocals(NonLocals).

:- pred implicitly_quantify_unify_rhs(unify_rhs, term__context, unify_rhs,
					quant_info, quant_info).
:- mode implicitly_quantify_unify_rhs(in, in, out, in, out) is det.

implicitly_quantify_unify_rhs(var(X), _, var(X)) -->
	{ set__singleton_set(Vars, X) },
	quantification__set_nonlocals(Vars).
implicitly_quantify_unify_rhs(functor(Functor, ArgVars), _,
				functor(Functor, ArgVars)) -->
	{ set__list_to_set(ArgVars, Vars) },
	quantification__set_nonlocals(Vars).
implicitly_quantify_unify_rhs(
		lambda_goal(PredOrFunc, LambdaVars0, Modes, Det, Goal0),
		Context,
		lambda_goal(PredOrFunc, LambdaVars, Modes, Det, Goal)) -->

	quantification__get_outside(OutsideVars0),
	{ set__list_to_set(LambdaVars0, QVars) },
		% Figure out which variables have overlapping scopes
		% because they occur outside the goal and are also
		% lambda-quantified vars.
	{ set__intersect(OutsideVars0, QVars, RenameVars0) },
	(
		{ set__empty(RenameVars0) }
	->
		[]
	;
		quantification__warn_overlapping_scope(RenameVars0, Context)
	),
		% We need to rename apart any of the lambda vars that
		% we have already seen, since they are new instances.
	quantification__get_seen(Seen0),
	{ set__intersect(Seen0, QVars, RenameVars1) },

	{ set__union(RenameVars0, RenameVars1, RenameVars) },
	quantification__rename_apart(RenameVars, RenameMap, Goal0, Goal1),
	{ goal_util__rename_var_list(LambdaVars0, no, RenameMap, LambdaVars) },

		% Quantified variables cannot be pushed inside a lambda goal,
		% so we insert the quantified vars into the outside vars set,
		% and initialize the new quantified vars set to be empty.
	quantification__get_quant_vars(QuantVars0),
	{ set__union(OutsideVars0, QuantVars0, OutsideVars1) },
		% Add the lambda vars as outside vars, since they are
		% outside of the lambda goal
	{ set__insert_list(OutsideVars1, LambdaVars, OutsideVars) },
	{ set__init(QuantVars) },
	quantification__set_outside(OutsideVars),
	quantification__set_quant_vars(QuantVars),
	implicitly_quantify_goal(Goal1, Goal),
	quantification__get_nonlocals(NonLocals0),
		% lambda-quantified variables are local
	{ set__delete_list(NonLocals0, LambdaVars, NonLocals) },
	quantification__set_quant_vars(QuantVars0),
	quantification__set_outside(OutsideVars0),
	quantification__set_nonlocals(NonLocals).

:- pred implicitly_quantify_conj(list(hlds_goal), list(hlds_goal), 
					quant_info, quant_info).
:- mode implicitly_quantify_conj(in, out, in, out) is det.

implicitly_quantify_conj(Goals0, Goals) -->
	{ get_vars(Goals0, FollowingVarsList) },
	implicitly_quantify_conj_2(Goals0, FollowingVarsList, Goals).

:- pred implicitly_quantify_conj_2(list(hlds_goal), list(pair(set(var))),
			list(hlds_goal), quant_info, quant_info).
:- mode implicitly_quantify_conj_2(in, in, out, in, out) is det.

implicitly_quantify_conj_2([], _, []) -->
	{ set__init(NonLocalVars) },
	quantification__set_nonlocals(NonLocalVars).
implicitly_quantify_conj_2([_|_], [], _, _, _) :-
	error("implicitly_quantify_conj_2: length mismatch").
implicitly_quantify_conj_2([Goal0 | Goals0],
		[FollowingVars - LambdaFollowingVars | FollowingVarsList],
			[Goal | Goals]) -->
	quantification__get_outside(OutsideVars),
	quantification__get_lambda_outside(LambdaOutsideVars),
	{ set__union(OutsideVars, FollowingVars, OutsideVars1) },
	{ set__union(LambdaOutsideVars, LambdaFollowingVars,
			LambdaOutsideVars1) },
	quantification__set_outside(OutsideVars1),
	quantification__set_lambda_outside(LambdaOutsideVars1),
	implicitly_quantify_goal(Goal0, Goal),
	quantification__get_nonlocals(NonLocalVars1),
	{ set__union(OutsideVars, NonLocalVars1, OutsideVars2) },
	quantification__set_outside(OutsideVars2),
	quantification__set_lambda_outside(LambdaOutsideVars),
	implicitly_quantify_conj_2(Goals0, FollowingVarsList,
				Goals),
	quantification__get_nonlocals(NonLocalVars2),
	{ set__union(NonLocalVars1, NonLocalVars2, NonLocalVarsConj) },
	{ set__intersect(NonLocalVarsConj, OutsideVars, NonLocalVars) },
	quantification__set_outside(OutsideVars),
	quantification__set_nonlocals(NonLocalVars).

:- pred implicitly_quantify_disj(list(hlds_goal), list(hlds_goal), 
					quant_info, quant_info).
:- mode implicitly_quantify_disj(in, out, in, out) is det.

implicitly_quantify_disj([], []) -->
	{ set__init(NonLocalVars) },
	quantification__set_nonlocals(NonLocalVars).
implicitly_quantify_disj([Goal0 | Goals0], [Goal | Goals]) -->
	implicitly_quantify_goal(Goal0, Goal),
	quantification__get_nonlocals(NonLocalVars0),
	implicitly_quantify_disj(Goals0, Goals),
	quantification__get_nonlocals(NonLocalVars1),
	{ set__union(NonLocalVars0, NonLocalVars1, NonLocalVars) },
	quantification__set_nonlocals(NonLocalVars).

:- pred implicitly_quantify_cases(list(case), list(case),
					quant_info, quant_info).
:- mode implicitly_quantify_cases(in, out, in, out) is det.

implicitly_quantify_cases([], []) -->
	{ set__init(NonLocalVars) },
	quantification__set_nonlocals(NonLocalVars).
implicitly_quantify_cases([case(Cons, Goal0) | Cases0],
				[case(Cons, Goal) | Cases]) -->
	implicitly_quantify_goal(Goal0, Goal),
	quantification__get_nonlocals(NonLocalVars0),
	implicitly_quantify_cases(Cases0, Cases),
	quantification__get_nonlocals(NonLocalVars1),
	{ set__union(NonLocalVars0, NonLocalVars1, NonLocalVars) },
	quantification__set_nonlocals(NonLocalVars).

%-----------------------------------------------------------------------------%

	% insert the given set of variables into the set of `seen' variables.

:- pred quantification__update_seen_vars(set(var), quant_info, quant_info).
:- mode quantification__update_seen_vars(in, in, out) is det.

quantification__update_seen_vars(NewVars) -->
	quantification__get_seen(SeenVars0),
	{ set__union(SeenVars0, NewVars, SeenVars) },
	quantification__set_seen(SeenVars).

%-----------------------------------------------------------------------------%

	% Given a list of goals, produce a corresponding list of
	% following variables, where the following variables
	% for each goal are those variables which occur free in any of the
	% following goals in the list.  The following variables
	% are divided into a pair of sets: the first set
	% contains following variables that occur not in lambda goals,
	% and the second contains following variables that
	% occur in lambda goals.

:- pred get_vars(list(hlds_goal), list(pair(set(var)))).
:- mode get_vars(in, out) is det.

get_vars([], []).
get_vars([_Goal | Goals], [Set - LambdaSet | SetPairs]) :-
	get_vars_2(Goals, Set, LambdaSet, SetPairs).

:- pred get_vars_2(list(hlds_goal), set(var), set(var), list(pair(set(var)))).
:- mode get_vars_2(in, out, out, out) is det.

get_vars_2([], Set, LambdaSet, []) :-
	set__init(Set),
	set__init(LambdaSet).
get_vars_2([Goal | Goals], Set, LambdaSet, SetPairList) :-
	get_vars_2(Goals, Set0, LambdaSet0, SetPairList0),
	quantification__goal_vars(Goal, Set1, LambdaSet1),
	set__union(Set0, Set1, Set),
	set__union(LambdaSet0, LambdaSet1, LambdaSet),
	SetPairList = [Set0 - LambdaSet0 | SetPairList0].

:- pred goal_list_vars_2(list(hlds_goal), set(var), set(var),
			set(var), set(var)).
:- mode goal_list_vars_2(in, in, in, out, out) is det.

goal_list_vars_2([], Set, LambdaSet, Set, LambdaSet).
goal_list_vars_2([Goal - _GoalInfo| Goals], Set0, LambdaSet0, Set, LambdaSet) :-
	quantification__goal_vars_2(Goal, Set0, LambdaSet0, Set1, LambdaSet1),
	goal_list_vars_2(Goals, Set1, LambdaSet1, Set, LambdaSet).

:- pred case_list_vars_2(list(case), set(var), set(var), set(var), set(var)).
:- mode case_list_vars_2(in, in, in, out, out) is det.

case_list_vars_2([], Set, LambdaSet, Set, LambdaSet).
case_list_vars_2([case(_Cons, Goal - _GoalInfo)| Cases], Set0, LambdaSet0,
			Set, LambdaSet) :-
	quantification__goal_vars_2(Goal, Set0, LambdaSet0, Set1, LambdaSet1),
	case_list_vars_2(Cases, Set1, LambdaSet1, Set, LambdaSet).

	% quantification__goal_vars(Goal, Vars):
	%	Vars is the set of variables that occur free (unquantified)
	%	in Goal.
quantification__goal_vars(Goal, BothSet) :-
	quantification__goal_vars(Goal, NonLambdaSet, LambdaSet),
	set__union(NonLambdaSet, LambdaSet, BothSet).

	% quantification__goal_vars(Goal, NonLambdaSet, LambdaSet):
	%	Set is the set of variables that occur free (unquantified)
	%	in Goal, not counting occurrences in lambda expressions.
	%	LambdaSet is the set of variables that occur free (unquantified)
	%	in lambda expressions in Goal.
:- pred quantification__goal_vars(hlds_goal, set(var), set(var)).
:- mode quantification__goal_vars(in, out, out) is det.

quantification__goal_vars(Goal - _GoalInfo, Set, LambdaSet) :-
	set__init(Set0),
	set__init(LambdaSet0),
	quantification__goal_vars_2(Goal, Set0, LambdaSet0, Set, LambdaSet).

:- pred quantification__goal_vars_2(hlds_goal_expr, set(var), set(var),
		set(var), set(var)).
:- mode quantification__goal_vars_2(in, in, in, out, out) is det.

quantification__goal_vars_2(unify(A, B, _, _, _), Set0, LambdaSet0,
		Set, LambdaSet) :-
	set__insert(Set0, A, Set1),
	quantification__unify_rhs_vars(B, Set1, LambdaSet0, Set, LambdaSet).

quantification__goal_vars_2(higher_order_call(PredVar, ArgVars, _, _, _),
		Set0, LambdaSet, Set, LambdaSet) :-
	set__insert_list(Set0, [PredVar | ArgVars], Set).

quantification__goal_vars_2(call(_, _, ArgVars, _, _, _), Set0, LambdaSet,
		Set, LambdaSet) :-
	set__insert_list(Set0, ArgVars, Set).

quantification__goal_vars_2(conj(Goals), Set0, LambdaSet0, Set, LambdaSet) :-
	goal_list_vars_2(Goals, Set0, LambdaSet0, Set, LambdaSet).

quantification__goal_vars_2(disj(Goals, _), Set0, LambdaSet0, Set, LambdaSet) :-
	goal_list_vars_2(Goals, Set0, LambdaSet0, Set, LambdaSet).

quantification__goal_vars_2(switch(Var, _Det, Cases, _), Set0, LambdaSet0,
		Set, LambdaSet) :-
	set__insert(Set0, Var, Set1),
	case_list_vars_2(Cases, Set1, LambdaSet0, Set, LambdaSet).

quantification__goal_vars_2(some(Vars, Goal), Set0, LambdaSet0,
		Set, LambdaSet) :-
	quantification__goal_vars(Goal, Set1, LambdaSet1),
	set__delete_list(Set1, Vars, Set2),
	set__delete_list(LambdaSet1, Vars, LambdaSet2),
	set__union(Set0, Set2, Set),
	set__union(LambdaSet0, LambdaSet2, LambdaSet).

quantification__goal_vars_2(not(Goal - _GoalInfo), Set0, LambdaSet0,
		Set, LambdaSet) :-
	quantification__goal_vars_2(Goal, Set0, LambdaSet0, Set, LambdaSet).

quantification__goal_vars_2(if_then_else(Vars, A, B, C, _), Set0, LambdaSet0,
		Set, LambdaSet) :-
	% This code does the following:
	%     Set = Set0 + ( (vars(A) + vars(B)) \ Vars ) + vars(C)
	% where `+' is set union and `\' is relative complement.
	quantification__goal_vars(A, Set1, LambdaSet1),
	quantification__goal_vars(B, Set2, LambdaSet2),
	set__union(Set1, Set2, Set3),
	set__union(LambdaSet1, LambdaSet2, LambdaSet3),
	set__delete_list(Set3, Vars, Set4),
	set__delete_list(LambdaSet3, Vars, LambdaSet4),
	set__union(Set0, Set4, Set5),
	set__union(LambdaSet0, LambdaSet4, LambdaSet5),
	quantification__goal_vars(C, Set6, LambdaSet6),
	set__union(Set5, Set6, Set),
	set__union(LambdaSet5, LambdaSet6, LambdaSet).

quantification__goal_vars_2(pragma_c_code(_, _, _, _, ArgVars, _, _, _),
		Set0, LambdaSet, Set, LambdaSet) :-
	set__insert_list(Set0, ArgVars, Set).

:- pred quantification__unify_rhs_vars(unify_rhs, set(var), set(var),
					set(var), set(var)).
:- mode quantification__unify_rhs_vars(in, in, in, out, out) is det.

quantification__unify_rhs_vars(var(X), Set0, LambdaSet, Set, LambdaSet) :-
	set__insert(Set0, X, Set).
quantification__unify_rhs_vars(functor(_Functor, ArgVars), Set0, LambdaSet,
		Set, LambdaSet) :-
	set__insert_list(Set0, ArgVars, Set).
quantification__unify_rhs_vars(lambda_goal(_PredOrFunc, LambdaVars, _Modes,
		_Detism, Goal), Set, LambdaSet0, Set, LambdaSet) :-
	quantification__goal_vars(Goal, GoalVars),
	set__delete_list(GoalVars, LambdaVars, GoalVars1),
	set__union(LambdaSet0, GoalVars1, LambdaSet).

%-----------------------------------------------------------------------------%

:- pred quantification__warn_overlapping_scope(set(var), term__context,
					quant_info, quant_info).
:- mode quantification__warn_overlapping_scope(in, in, in, out) is det.

quantification__warn_overlapping_scope(OverlapVars, Context) -->
	{ set__to_sorted_list(OverlapVars, Vars) },
	quantification__get_warnings(Warnings0),
	{ Warnings = [warn_overlap(Vars, Context) | Warnings0] },
	quantification__set_warnings(Warnings).

%-----------------------------------------------------------------------------%

% quantification__rename_apart(RenameSet, RenameMap, Goal0, Goal):
%	For each variable V in RenameSet, create a fresh variable V',
%	and insert the mapping V->V' into RenameMap.
%	Apply RenameMap to Goal0 giving Goal.

:- pred quantification__rename_apart(set(var), map(var, var),
				hlds_goal, hlds_goal, quant_info, quant_info).
:- mode quantification__rename_apart(in, out, in, out, in, out) is det.

quantification__rename_apart(RenameSet, RenameMap, Goal0, Goal) -->
	{ set__to_sorted_list(RenameSet, RenameList) },
	quantification__get_varset(Varset0),
	quantification__get_vartypes(VarTypes0),
	{ map__init(RenameMap0) },
	{ goal_util__create_variables(RenameList,
		Varset0, VarTypes0, RenameMap0, VarTypes0, Varset0,
			% ^ Accumulator		^ Reference ^Var names
		Varset, VarTypes, RenameMap) },
	{ goal_util__rename_vars_in_goal(Goal0, RenameMap, Goal) },
	quantification__set_varset(Varset),
	quantification__set_vartypes(VarTypes).
/****
	We don't need to add the newly created vars to the seen vars
	because we won't find them anywhere else in the enclosing goal.
	This is a performance improvement because it keeps the size of
	the seen var set down.
	quantification__get_seen(SeenVars0),
	{ map__values(RenameMap, NewVarsList) },
	{ set__insert_list(SeenVars0, NewVarsList, SeenVars) },
	quantification__set_seen(SeenVars).
***/

%-----------------------------------------------------------------------------%

:- pred quantification__init(set(var), varset, map(var, type), quant_info).
:- mode quantification__init(in, in, in, out) is det.

quantification__init(OutsideVars, Varset, VarTypes, QuantInfo) :-
	set__init(QuantVars),
	set__init(NonLocals),
	set__init(LambdaOutsideVars),
	Seen = OutsideVars,
	OverlapWarnings = [],
	QuantInfo = quant_info(OutsideVars, QuantVars, LambdaOutsideVars,
		NonLocals, Seen, Varset, VarTypes, OverlapWarnings).

:- pred quantification__get_outside(set(var), quant_info, quant_info).
:- mode quantification__get_outside(out, in, out) is det.

quantification__get_outside(A, Q, Q) :-
	Q = quant_info(A, _, _, _, _, _, _, _).

:- pred quantification__set_outside(set(var), quant_info, quant_info).
:- mode quantification__set_outside(in, in, out) is det.

quantification__set_outside(A, Q0, Q) :-
	Q0 = quant_info(_, B, C, D, E, F, G, H),
	Q  = quant_info(A, B, C, D, E, F, G, H).

:- pred quantification__get_quant_vars(set(var), quant_info, quant_info).
:- mode quantification__get_quant_vars(out, in, out) is det.

quantification__get_quant_vars(B, Q, Q) :-
	Q = quant_info(_, B, _, _, _, _, _, _).

:- pred quantification__set_quant_vars(set(var), quant_info, quant_info).
:- mode quantification__set_quant_vars(in, in, out) is det.

quantification__set_quant_vars(B, Q0, Q) :-
	Q0 = quant_info(A, _, C, D, E, F, G, H),
	Q  = quant_info(A, B, C, D, E, F, G, H).

:- pred quantification__get_lambda_outside(set(var), quant_info, quant_info).
:- mode quantification__get_lambda_outside(out, in, out) is det.

quantification__get_lambda_outside(C, Q, Q) :-
	Q  = quant_info(_, _, C, _, _, _, _, _).

:- pred quantification__set_lambda_outside(set(var), quant_info, quant_info).
:- mode quantification__set_lambda_outside(in, in, out) is det.

quantification__set_lambda_outside(C, Q0, Q) :-
	Q0 = quant_info(A, B, _, D, E, F, G, H),
	Q  = quant_info(A, B, C, D, E, F, G, H).

:- pred quantification__get_nonlocals(set(var), quant_info, quant_info).
:- mode quantification__get_nonlocals(out, in, out) is det.

quantification__get_nonlocals(D, Q, Q) :-
	Q  = quant_info(_, _, _, D, _, _, _, _).

:- pred quantification__set_nonlocals(set(var), quant_info, quant_info).
:- mode quantification__set_nonlocals(in, in, out) is det.

quantification__set_nonlocals(D, Q0, Q) :-
	Q0 = quant_info(A, B, C, _, E, F, G, H),
	Q  = quant_info(A, B, C, D, E, F, G, H).

:- pred quantification__get_seen(set(var), quant_info, quant_info).
:- mode quantification__get_seen(out, in, out) is det.

quantification__get_seen(E, Q, Q) :-
	Q  = quant_info(_, _, _, _, E, _, _, _).

:- pred quantification__set_seen(set(var), quant_info, quant_info).
:- mode quantification__set_seen(in, in, out) is det.

quantification__set_seen(E, Q0, Q) :-
	Q0 = quant_info(A, B, C, D, _, F, G, H),
	Q  = quant_info(A, B, C, D, E, F, G, H).

:- pred quantification__get_varset(varset, quant_info, quant_info).
:- mode quantification__get_varset(out, in, out) is det.

quantification__get_varset(F, Q, Q) :-
	Q  = quant_info(_, _, _, _, _, F, _, _).

:- pred quantification__set_varset(varset, quant_info, quant_info).
:- mode quantification__set_varset(in, in, out) is det.

quantification__set_varset(F, Q0, Q) :-
	Q0 = quant_info(A, B, C, D, E, _, G, H),
	Q  = quant_info(A, B, C, D, E, F, G, H).

:- pred quantification__get_vartypes(map(var, type), quant_info, quant_info).
:- mode quantification__get_vartypes(out, in, out) is det.

quantification__get_vartypes(G, Q, Q) :-
	Q  = quant_info(_, _, _, _, _, _, G, _).

:- pred quantification__set_vartypes(map(var, type), quant_info, quant_info).
:- mode quantification__set_vartypes(in, in, out) is det.

quantification__set_vartypes(G, Q0, Q) :-
	Q0 = quant_info(A, B, C, D, E, F, _, H),
	Q  = quant_info(A, B, C, D, E, F, G, H).

:- pred quantification__get_warnings(list(quant_warning),
					quant_info, quant_info).
:- mode quantification__get_warnings(out, in, out) is det.

quantification__get_warnings(H, Q, Q) :-
	Q  = quant_info(_, _, _, _, _, _, _, H).

:- pred quantification__set_warnings(list(quant_warning),
					quant_info, quant_info).
:- mode quantification__set_warnings(in, in, out) is det.

quantification__set_warnings(H, Q0, Q) :-
	Q0 = quant_info(A, B, C, D, E, F, G, _),
	Q  = quant_info(A, B, C, D, E, F, G, H).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
