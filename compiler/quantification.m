%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: quantification.m.
% Main author: fjh.

	% Make implicit quantification explicit.
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
	%
	% XXX we ought to rename variables with overlapping scopes
	% caused by explicit quantifiers here (and issue a warning
	% at the same time).

%-----------------------------------------------------------------------------%

:- module quantification.
:- interface.
:- import_module list, set, term, hlds.

:- pred implicitly_quantify_clause_body(list(var), hlds__goal, hlds__goal).
:- mode implicitly_quantify_clause_body(in, in, out) is det.

:- pred implicitly_quantify_goal(hlds__goal, set(var), hlds__goal).
:- mode implicitly_quantify_goal(in, in, out) is det.

:- pred goal_vars(hlds__goal, set(var)).
:- mode goal_vars(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util, require.

implicitly_quantify_clause_body(HeadVars, Goal0, Goal) :-
	set__list_to_set(HeadVars, OutsideVars),
	implicitly_quantify_goal(Goal0, OutsideVars, Goal).

implicitly_quantify_goal(Goal0, OutsideVars, Goal) :-
	set__init(QuantVars),
	implicitly_quantify_goal(Goal0, OutsideVars, QuantVars, Goal, _).

:- pred implicitly_quantify_goal(hlds__goal, set(var), set(var),
				hlds__goal, set(var)).
:- mode implicitly_quantify_goal(in, in, in, out, out) is det.

implicitly_quantify_goal(Goal0 - GoalInfo0, OutsideVars, QuantVars,
			Goal - GoalInfo, NonLocalVars) :-
	implicitly_quantify_goal_2(Goal0, OutsideVars, QuantVars,
			Goal, NonLocalVars),
	goal_info_set_nonlocals(GoalInfo0, NonLocalVars, GoalInfo).

	% `OutsideVars' are the variables that have occurred outside
	% this goal, not counting occurrences in quantifiers, and
	% `QuantVars' are the variables that have been explicitly
	% existentially quantified.  For example, for
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

:- pred implicitly_quantify_goal_2(hlds__goal_expr, set(var), set(var),
				   hlds__goal_expr, set(var)).
:- mode implicitly_quantify_goal_2(in, in, in, out, out) is det.

	% we retain explicit existential quantifiers in the source code,
	% even though they are redundant with the goal_info non_locals,
	% so that we can easily recalculate the goal_info non_locals
	% if necessary after program transformation.

implicitly_quantify_goal_2(some(Vars, Goal0), OutsideVars, QuantVars,
			   some(Vars, Goal), NonLocals) :-
	check_overlapping_scopes(Vars, OutsideVars, QuantVars),
	set__insert_list(QuantVars, Vars, QuantVars1),
	implicitly_quantify_goal(Goal0, OutsideVars, QuantVars1,
		Goal, NonLocals0),
	set__delete_list(NonLocals0, Vars, NonLocals).

implicitly_quantify_goal_2(conj(List0), OutsideVars, QuantVars,
			   conj(List), NonLocalVars) :-
	implicitly_quantify_conj(List0, OutsideVars, QuantVars,
				List, NonLocalVars).

implicitly_quantify_goal_2(disj(Goals0), OutsideVars, QuantVars,
			   disj(Goals), NonLocalVars) :-
	implicitly_quantify_disj(Goals0, OutsideVars, QuantVars,
				Goals, NonLocalVars).

implicitly_quantify_goal_2(switch(Var, Det, Cases0), OutsideVars, QuantVars,
			   switch(Var, Det, Cases), NonLocalVars) :-
	implicitly_quantify_cases(Cases0, OutsideVars, QuantVars,
				Cases, NonLocalVars).

implicitly_quantify_goal_2(not(Goal0), OutsideVars, QuantVars,
			   not(Goal), NonLocals) :-
	% quantified variables cannot be pushed inside a negation,
	% so we insert the quantified vars into the outside vars set,
	% and initialize the new quantified vars set to be empty
	set__union(OutsideVars, QuantVars, OutsideVars1),
	set__init(QuantVars1),
	implicitly_quantify_goal(Goal0, OutsideVars1, QuantVars1,
				Goal, NonLocals).

implicitly_quantify_goal_2(if_then_else(Vars, A0, B0, C0),
				OutsideVars, QuantVars,
		if_then_else(Vars, A, B, C), NonLocals) :-
	check_overlapping_scopes(Vars, OutsideVars, QuantVars),
	set__insert_list(QuantVars, Vars, QuantVars1),
	goal_vars(B0, VarsB),
	set__union(OutsideVars, VarsB, OutsideVars1),
	implicitly_quantify_goal(A0, OutsideVars1, QuantVars1, A, NonLocalsA),
	set__union(OutsideVars, NonLocalsA, OutsideVars2),
	implicitly_quantify_goal(B0, OutsideVars2, QuantVars1, B, NonLocalsB),
	implicitly_quantify_goal(C0, OutsideVars, QuantVars1, C, NonLocalsC),
	set__union(NonLocalsA, NonLocalsB, NonLocalsSuccess),
	set__union(NonLocalsSuccess, NonLocalsC, NonLocalsIfThenElse),
	set__intersect(NonLocalsIfThenElse, OutsideVars, NonLocals).

implicitly_quantify_goal_2(call(A, B, HeadVars, D, E, F, G),
		OutsideVars, _QuantVars,
		call(A, B, HeadVars, D, E, F, G), NonLocalVars) :-
	set__list_to_set(HeadVars, GoalVars),
	set__intersect(GoalVars, OutsideVars, NonLocalVars).

implicitly_quantify_goal_2(unify(A, B0, X, Y, Z),
			OutsideVars, QuantVars,
			unify(A, B, X, Y, Z), NonLocalVars) :-
	set__insert(OutsideVars, A, OutsideVars1),
	implicitly_quantify_unify_rhs(B0, OutsideVars1, QuantVars, B, VarsB),
	set__insert(VarsB, A, GoalVars),
	set__intersect(GoalVars, OutsideVars, NonLocalVars).

:- pred implicitly_quantify_unify_rhs(unify_rhs, set(var), set(var),
				 unify_rhs, set(var)).
:- mode implicitly_quantify_unify_rhs(in, in, in, out, out) is det.

implicitly_quantify_unify_rhs(var(X), _OutsideVars, _QuantVars, var(X), Vars) :-
	set__singleton_set(Vars, X).
implicitly_quantify_unify_rhs(functor(Functor, ArgVars),
				_OutsideVars, _QuantVars,
				functor(Functor, ArgVars), Vars) :-
	set__list_to_set(ArgVars, Vars).
implicitly_quantify_unify_rhs(lambda_goal(LambdaVars, Modes, Goal0),
				OutsideVars, QuantVars,
				lambda_goal(LambdaVars, Modes, Goal),
				NonLocals) :-
	% quantified variables cannot be pushed inside a lambda goal,
	% so we insert the quantified vars into the outside vars set,
	% and initialize the new quantified vars set to be empty
	set__union(OutsideVars, QuantVars, OutsideVars1),
	set__init(QuantVars1),
	% the lambda-quantified variables are outside the goal
	set__insert_list(OutsideVars1, LambdaVars, OutsideVars2),
	implicitly_quantify_goal(Goal0, OutsideVars2, QuantVars1,
			Goal, NonLocals0),
	% lambda-quantified variables are local
	set__delete_list(NonLocals0, LambdaVars, NonLocals).

:- pred implicitly_quantify_conj(list(hlds__goal), set(var), set(var),
				 list(hlds__goal), set(var)).
:- mode implicitly_quantify_conj(in, in, in, out, out) is det.

implicitly_quantify_conj(Goals0, OutsideVars, QuantVars, Goals, NonLocalVars) :-
	get_vars(Goals0, FollowingVarsList),
	implicitly_quantify_conj_2(Goals0, FollowingVarsList,
				OutsideVars, QuantVars,
				Goals, NonLocalVars).

:- pred implicitly_quantify_conj_2(list(hlds__goal), list(set(var)),
			set(var), set(var),
			list(hlds__goal), set(var)).
:- mode implicitly_quantify_conj_2(in, in, in, in, out, out) is det.

:- implicitly_quantify_conj_2(A, B, _, _, _, _) when A and B.

implicitly_quantify_conj_2([], _, _, _, [], NonLocalVars) :-
	set__init(NonLocalVars).
implicitly_quantify_conj_2([_|_], [], _, _, _, _) :-
	error("implicitly_quantify_conj_2: length mismatch").
implicitly_quantify_conj_2([Goal0 | Goals0],
			[FollowingVars | FollowingVarsList],
			OutsideVars, QuantVars,
			[Goal | Goals], NonLocalVars) :-
	set__union(OutsideVars, FollowingVars, OutsideVars1),
	implicitly_quantify_goal(Goal0, OutsideVars1, QuantVars,
		Goal, NonLocalVars1),
	set__union(OutsideVars, NonLocalVars1, OutsideVars2),
	implicitly_quantify_conj_2(Goals0, FollowingVarsList,
				OutsideVars2, QuantVars,
				Goals, NonLocalVars2),
	set__union(NonLocalVars1, NonLocalVars2, NonLocalVarsConj),
	set__intersect(NonLocalVarsConj, OutsideVars, NonLocalVars).

:- pred implicitly_quantify_disj(list(hlds__goal), set(var), set(var),
				 list(hlds__goal), set(var)).
:- mode implicitly_quantify_disj(in, in, in, out, out) is det.

implicitly_quantify_disj([], _, _, [], NonLocalVars) :-
	set__init(NonLocalVars).
implicitly_quantify_disj([Goal0 | Goals0], OutsideVars, QuantVars,
			[Goal | Goals], NonLocalVars) :-
	implicitly_quantify_goal(Goal0, OutsideVars, QuantVars,
			Goal, NonLocalVars0),
	implicitly_quantify_disj(Goals0, OutsideVars, QuantVars,
			Goals, NonLocalVars1),
	set__union(NonLocalVars0, NonLocalVars1, NonLocalVars).

:- pred implicitly_quantify_cases(list(case), set(var), set(var),
				 list(case), set(var)).
:- mode implicitly_quantify_cases(in, in, in, out, out) is det.

implicitly_quantify_cases([], _, _, [], NonLocalVars) :-
	set__init(NonLocalVars).
implicitly_quantify_cases([case(Cons, Goal0) | Cases0], OutsideVars, QuantVars,
			[case(Cons, Goal) | Cases], NonLocalVars) :-
	implicitly_quantify_goal(Goal0, OutsideVars, QuantVars,
		Goal, NonLocalVars0),
	implicitly_quantify_cases(Cases0, OutsideVars, QuantVars,
		Cases, NonLocalVars1),
	set__union(NonLocalVars0, NonLocalVars1, NonLocalVars).

%-----------------------------------------------------------------------------%

	% overlapping scopes are currently not implemented

:- pred check_overlapping_scopes(list(var), set(var), set(var)).
:- mode check_overlapping_scopes(in, in, in) is det.

check_overlapping_scopes(Vars, OutsideVars, QuantVars) :-
	(
		list__member(Var, Vars),
		(	set__member(Var, OutsideVars)
		;	set__member(Var, QuantVars)
		)
	->
		% XXX we should rename apart variables with overlapping
		% scopes
		error("implicitly_quantify_goal_2: sorry, not implemented: variable has overlapping scopes")
	;
		true
	).

%-----------------------------------------------------------------------------%

	% Given a list of goals, produce a corresponding list of sets
	% of following variables, where is the set of following variables
	% for each goal is those variables which occur in any of the
	% following goals in the list.

:- pred get_vars(list(hlds__goal), list(set(var))).
:- mode get_vars(in, out) is det.

get_vars([], []).
get_vars([_Goal|Goals], [Set|Sets]) :-
	get_vars_2(Goals, Set, Sets).

:- pred get_vars_2(list(hlds__goal), set(var), list(set(var))).
:- mode get_vars_2(in, out, out) is det.

get_vars_2([], Set, []) :-
	set__init(Set).
get_vars_2([Goal | Goals], Set, SetList) :-
	get_vars_2(Goals, Set0, SetList0),
	goal_vars(Goal, Set1),
	set__union(Set0, Set1, Set),
	SetList = [Set0 | SetList0].

	% `goal_list_vars(Goal, Vars)' is true iff 
	% `Vars' is the set of unquantified variables in Goal.

:- pred goal_list_vars(list(hlds__goal), set(var)).
:- mode goal_list_vars(in, out) is det.

goal_list_vars(Goals, S) :-	
	set__init(S0),
	goal_list_vars_2(Goals, S0, S).


:- pred goal_list_vars_2(list(hlds__goal), set(var), set(var)).
:- mode goal_list_vars_2(in, in, out) is det.

goal_list_vars_2([], Set, Set).
goal_list_vars_2([Goal - _GoalInfo| Goals], Set0, Set) :-
	goal_vars_2(Goal, Set0, Set1),
	goal_list_vars_2(Goals, Set1, Set).

:- pred case_list_vars_2(list(case), set(var), set(var)).
:- mode case_list_vars_2(in, in, out) is det.

case_list_vars_2([], Set, Set).
case_list_vars_2([case(_Cons, Goal - _GoalInfo)| Cases], Set0, Set) :-
	goal_vars_2(Goal, Set0, Set1),
	case_list_vars_2(Cases, Set1, Set).

goal_vars(Goal - _GoalInfo, Set) :-
	set__init(Set0),
	goal_vars_2(Goal, Set0, Set).

:- pred goal_vars_2(hlds__goal_expr, set(var), set(var)).
:- mode goal_vars_2(in, in, out) is det.

goal_vars_2(unify(A, B, _, _, _), Set0, Set) :-
	set__insert(Set0, A, Set1),
	unify_rhs_vars(B, Set1, Set).

goal_vars_2(call(_, _, ArgVars, _, _, _, _), Set0, Set) :-
	set__insert_list(Set0, ArgVars, Set).

goal_vars_2(conj(Goals), Set0, Set) :-
	goal_list_vars_2(Goals, Set0, Set).

goal_vars_2(disj(Goals), Set0, Set) :-
	goal_list_vars_2(Goals, Set0, Set).

goal_vars_2(switch(_Var, _Det, Cases), Set0, Set) :-
	case_list_vars_2(Cases, Set0, Set).

goal_vars_2(some(Vars, Goal), Set0, Set) :-
	goal_vars(Goal, Set1),
	set__delete_list(Set1, Vars, Set2),
	set__union(Set0, Set2, Set).

goal_vars_2(not(Goal - _GoalInfo), Set0, Set) :-
	goal_vars_2(Goal, Set0, Set).

goal_vars_2(if_then_else(Vars, A, B, C), Set0, Set) :-
		% This code does the following:
		%
		% Set = Set0 + ( (vars(A) + vars(B)) \ Vars ) + vars(C)
		%
		% where `+' is set union and `-' is relative complement.
		%
		% (It's times like this you wish you had a functional
		% notation ;-)
	goal_vars(A, Set1),
	goal_vars(B, Set2),
	set__union(Set1, Set2, Set3),
	set__delete_list(Set3, Vars, Set4),
	set__union(Set0, Set4, Set5),
	goal_vars(C, Set6),
	set__union(Set5, Set6, Set).

:- pred unify_rhs_vars(unify_rhs, set(var), set(var)).
:- mode unify_rhs_vars(in, in, out) is det.

unify_rhs_vars(var(X), Set0, Set) :-
	set__insert(Set0, X, Set).
unify_rhs_vars(functor(_Functor, ArgVars), Set0, Set) :-
	set__insert_list(Set0, ArgVars, Set).
unify_rhs_vars(lambda_goal(LambdaVars, _Modes, Goal), Set0, Set) :-
	goal_vars(Goal, GoalVars),
	set__delete_list(GoalVars, LambdaVars, GoalVars1),
	set__union(Set0, GoalVars1, Set).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
