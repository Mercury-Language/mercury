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
:- import_module list, set, term, hlds, prog_io.

:- pred implicitly_quantify_clause_body(list(var), hlds__goal, varset,
			map(var, type), hlds__goal, varset, map(var, type)).
:- mode implicitly_quantify_clause_body(in, in, in, in, out, out, out) is det.

:- pred implicitly_quantify_goal(hlds__goal, varset, map(var, type), set(var),
		hlds__goal, varset, map(var, type)).
:- mode implicitly_quantify_goal(in, in, in, in, out, out, out) is det.

:- pred goal_vars(hlds__goal, set(var)).
:- mode goal_vars(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util, map, goal_util, require.

	% The outside vars and quant vars are essentially inputs,
	% nonlocals output, and seen so far, the varset and the
	% types are threaded and thus input and output.
	% The input fields are callee save, and the outputs caller
	% save.
:- type quant_info	--->
		quant_info(
			set(var), % outside vars
			set(var), % quant vars
			set(var), % nonlocals
			set(var), % seen so far
			varset,
			map(var, type)
		).

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

implicitly_quantify_clause_body(HeadVars, Goal0, Varset0, VarTypes0,
		Goal, Varset, VarTypes) :-
	set__list_to_set(HeadVars, OutsideVars),
	implicitly_quantify_goal(Goal0, Varset0, VarTypes0,
			OutsideVars, Goal, Varset, VarTypes).

implicitly_quantify_goal(Goal0, Varset0, VarTypes0, OutsideVars,
					Goal, Varset, VarTypes) :-
	quantification__init(OutsideVars, Varset0, VarTypes0, QuantInfo0),
	implicitly_quantify_goal(Goal0, Goal, QuantInfo0, QuantInfo),
	quantification__get_varset(Varset, QuantInfo, _),
	quantification__get_vartypes(VarTypes, QuantInfo, _).

:- pred implicitly_quantify_goal(hlds__goal, hlds__goal,
					quant_info, quant_info).
:- mode implicitly_quantify_goal(in, out, in, out) is det.

implicitly_quantify_goal(Goal0 - GoalInfo0, Goal - GoalInfo) -->
	quantification__get_seen(SeenVars),
	{ set__init(Set0) },
	{ goal_vars_2(Goal0, Set0, GoalVars0) },
	implicitly_quantify_goal_2(Goal0, Goal1),
	quantification__get_nonlocals(NonLocalVars),
	(
		% If there are any variables that are local to the goal
		% which we have come across before, then we rename them
		% apart.
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

:- pred implicitly_quantify_goal_2(hlds__goal_expr, hlds__goal_expr,
					quant_info, quant_info).
:- mode implicitly_quantify_goal_2(in, out, in, out) is det.

	% we retain explicit existential quantifiers in the source code,
	% even though they are redundant with the goal_info non_locals,
	% so that we can easily recalculate the goal_info non_locals
	% if necessary after program transformation.

implicitly_quantify_goal_2(some(Vars, Goal0), some(Vars3, Goal)) -->
	quantification__get_outside(OutsideVars),
	quantification__get_quant_vars(QuantVars),
	quantification__get_seen(SeenVars0),
		% Rename apart all the quantified
		% variables that occur outside this goal.
	{ set__list_to_set(Vars, QVars) },
	{ set__intersect(OutsideVars, QVars, RenameVars) },
	(
		{ set__empty(RenameVars) }
	->
		{ Goal1 = Goal0 },
		{ Vars3 = Vars }
	;
		quantification__rename_apart(RenameVars, Vars1, Goal0, Goal1),
			% add to the renamed variables those
			% quantified variables that didn't get
			% renamed.
		{ set__difference(QVars, RenameVars, OtherVars) },
		{ set__union(OtherVars, Vars1, Vars2) },
		{ set__to_sorted_list(Vars2, Vars3) }
	),
	{ set__union(SeenVars0, QVars, SeenVars) },
	quantification__set_seen(SeenVars),
	{ set__insert_list(QuantVars, Vars, QuantVars1) },
	quantification__set_quant_vars(QuantVars1),
	implicitly_quantify_goal(Goal1, Goal),
	quantification__get_nonlocals(NonLocals0),
	{ set__delete_list(NonLocals0, Vars, NonLocals) },
	quantification__set_outside(OutsideVars),
	quantification__set_quant_vars(QuantVars),
	quantification__set_nonlocals(NonLocals).

implicitly_quantify_goal_2(conj(List0), conj(List)) -->
	implicitly_quantify_conj(List0, List).

implicitly_quantify_goal_2(disj(Goals0), disj(Goals)) -->
	implicitly_quantify_disj(Goals0, Goals).

implicitly_quantify_goal_2(switch(Var, Det, Cases0),
					switch(Var, Det, Cases)) -->
	implicitly_quantify_cases(Cases0, Cases).

implicitly_quantify_goal_2(not(Goal0), not(Goal)) -->
		% quantified variables cannot be pushed inside a negation,
		% so we insert the quantified vars into the outside vars set,
		% and initialize the new quantified vars set to be empty
	quantification__get_quant_vars(QuantVars),
	quantification__get_outside(OutsideVars),
	{ set__union(OutsideVars, QuantVars, OutsideVars1) },
	{ set__init(QuantVars1) },
	quantification__set_quant_vars(QuantVars1),
	quantification__set_outside(OutsideVars1),
	implicitly_quantify_goal(Goal0, Goal),
	quantification__set_outside(OutsideVars),
	quantification__set_quant_vars(QuantVars).

implicitly_quantify_goal_2(if_then_else(Vars, A0, B0, C0),
				if_then_else(Vars3, A, B, C)) -->
	quantification__get_quant_vars(QuantVars),
	quantification__get_outside(OutsideVars),
	{ set__list_to_set(Vars, QVars) },
		% Rename apart those variables that
		% are quantified to the cond and then
		% of the i-t-e that occur outside the
		% i-t-e.
	{ set__intersect(OutsideVars, QVars, RenameVars) },
	(
		{ set__empty(RenameVars) }
	->
		{ A1 = A0 },
		{ B1 = B0 },
		{ Vars3 = Vars }
	;
		quantification__rename_apart(RenameVars, Vars1, A0, A1),
		quantification__rename_apart(RenameVars, _, B0, B1),
		{ set__difference(QVars, RenameVars, OtherVars) },
		{ set__union(OtherVars, Vars1, Vars2) },
		{ set__to_sorted_list(Vars2, Vars3) }
	),
	{ set__insert_list(QuantVars, Vars, QuantVars1) },
	{ goal_vars(B1, VarsB) },
	{ set__union(OutsideVars, VarsB, OutsideVars1) },
	quantification__set_quant_vars(QuantVars1),
	quantification__set_outside(OutsideVars1),
	implicitly_quantify_goal(A1, A),
	quantification__get_nonlocals(NonLocalsA),
	{ set__union(OutsideVars, NonLocalsA, OutsideVars2) },
	quantification__set_outside(OutsideVars2),
	quantification__set_quant_vars(QuantVars1),
	implicitly_quantify_goal(B1, B),
	quantification__get_nonlocals(NonLocalsB),
	quantification__set_outside(OutsideVars),
	quantification__set_quant_vars(QuantVars),
	implicitly_quantify_goal(C0, C),
	quantification__get_nonlocals(NonLocalsC),
	quantification__set_outside(OutsideVars),
	quantification__set_quant_vars(QuantVars1),
	{ set__union(NonLocalsA, NonLocalsB, NonLocalsSuccess) },
	{ set__union(NonLocalsSuccess, NonLocalsC, NonLocalsIfThenElse) },
	{ set__intersect(NonLocalsIfThenElse, OutsideVars, NonLocals) },
	quantification__set_nonlocals(NonLocals).

implicitly_quantify_goal_2(call(A, B, HeadVars, D, E, F, G),
		call(A, B, HeadVars, D, E, F, G)) -->
	quantification__get_outside(OutsideVars),
	{ set__list_to_set(HeadVars, GoalVars) },
	quantification__get_seen(SeenVars0),
	{ set__union(SeenVars0, GoalVars, SeenVars) },
	quantification__set_seen(SeenVars),
	{ set__intersect(GoalVars, OutsideVars, NonLocals) },
	quantification__set_nonlocals(NonLocals).

implicitly_quantify_goal_2(unify(A, B0, X, Y, Z), unify(A, B, X, Y, Z)) -->
	quantification__get_outside(OutsideVars),
	implicitly_quantify_unify_rhs(B0, B),
	quantification__get_nonlocals(VarsB),
	{ set__insert(VarsB, A, GoalVars) },
	quantification__get_seen(SeenVars0),
	{ set__union(SeenVars0, GoalVars, SeenVars) },
	quantification__set_seen(SeenVars),
	{ set__intersect(GoalVars, OutsideVars, NonLocalVars) },
	quantification__set_nonlocals(NonLocalVars).

implicitly_quantify_goal_2(pragma_c_code(A,B,C,Vars,E), 
		pragma_c_code(A,B,C,Vars,E)) --> 
	quantification__get_outside(OutsideVars),
	{ set__list_to_set(Vars, GoalVars) },
	quantification__get_seen(SeenVars0),
	{ set__union(SeenVars0, GoalVars, SeenVars) },
	quantification__set_seen(SeenVars),
	{ set__intersect(GoalVars, OutsideVars, NonLocals) },
	quantification__set_nonlocals(NonLocals).

:- pred implicitly_quantify_unify_rhs(unify_rhs, unify_rhs,
					quant_info, quant_info).
:- mode implicitly_quantify_unify_rhs(in, out, in, out) is det.

implicitly_quantify_unify_rhs(var(X), var(X)) -->
	{ set__singleton_set(Vars, X) },
	quantification__set_nonlocals(Vars).
implicitly_quantify_unify_rhs(functor(Functor, ArgVars),
				functor(Functor, ArgVars)) -->
	{ set__list_to_set(ArgVars, Vars) },
	quantification__set_nonlocals(Vars).
implicitly_quantify_unify_rhs(lambda_goal(LambdaVars0, Modes, Det, Goal0),
				lambda_goal(LambdaVars, Modes, Det, Goal)) -->
	quantification__get_outside(OutsideVars0),
	{ set__list_to_set(LambdaVars0, QVars) },
	{ set__intersect(OutsideVars0, QVars, RenameVars) },
	(
		{ set__empty(RenameVars) }
	->
		{ Goal1 = Goal0 },
		{ LambdaVars = LambdaVars0 }
	;
		quantification__rename_apart(RenameVars, Vars1, Goal0, Goal1),
			% add to the renamed variables those
			% quantified variables that didn't get
			% renamed.
		{ set__difference(QVars, RenameVars, OtherVars) },
		{ set__union(OtherVars, Vars1, Vars2) },
		{ set__to_sorted_list(Vars2, LambdaVars) }
	),
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

:- pred implicitly_quantify_conj(list(hlds__goal), list(hlds__goal), 
					quant_info, quant_info).
:- mode implicitly_quantify_conj(in, out, in, out) is det.

implicitly_quantify_conj(Goals0, Goals) -->
	{ get_vars(Goals0, FollowingVarsList) },
	implicitly_quantify_conj_2(Goals0, FollowingVarsList, Goals).

:- pred implicitly_quantify_conj_2(list(hlds__goal), list(set(var)),
			list(hlds__goal), quant_info, quant_info).
:- mode implicitly_quantify_conj_2(in, in, out, in, out) is det.

implicitly_quantify_conj_2([], _, []) -->
	{ set__init(NonLocalVars) },
	quantification__set_nonlocals(NonLocalVars).
implicitly_quantify_conj_2([_|_], [], _, _, _) :-
	error("implicitly_quantify_conj_2: length mismatch").
implicitly_quantify_conj_2([Goal0 | Goals0],
			[FollowingVars | FollowingVarsList],
			[Goal | Goals]) -->
	quantification__get_outside(OutsideVars),
	quantification__get_quant_vars(QuantVars),
	{ set__union(OutsideVars, FollowingVars, OutsideVars1) },
	quantification__set_outside(OutsideVars1),
	implicitly_quantify_goal(Goal0, Goal),
	quantification__get_nonlocals(NonLocalVars1),
	{ set__union(OutsideVars, NonLocalVars1, OutsideVars2) },
	quantification__set_outside(OutsideVars2),
	implicitly_quantify_conj_2(Goals0, FollowingVarsList,
				Goals),
	quantification__get_nonlocals(NonLocalVars2),
	{ set__union(NonLocalVars1, NonLocalVars2, NonLocalVarsConj) },
	{ set__intersect(NonLocalVarsConj, OutsideVars, NonLocalVars) },
	quantification__set_quant_vars(QuantVars),
	quantification__set_outside(OutsideVars),
	quantification__set_nonlocals(NonLocalVars).

:- pred implicitly_quantify_disj(list(hlds__goal), list(hlds__goal), 
					quant_info, quant_info).
:- mode implicitly_quantify_disj(in, out, in, out) is det.

implicitly_quantify_disj([], []) -->
	{ set__init(NonLocalVars) },
	quantification__set_nonlocals(NonLocalVars).
implicitly_quantify_disj([Goal0 | Goals0], [Goal | Goals]) -->
	quantification__get_quant_vars(QuantVars),
	quantification__get_outside(OutsideVars),
	implicitly_quantify_goal(Goal0, Goal),
	quantification__get_nonlocals(NonLocalVars0),
	quantification__set_quant_vars(QuantVars),
	quantification__set_outside(OutsideVars),
	implicitly_quantify_disj(Goals0, Goals),
	quantification__get_nonlocals(NonLocalVars1),
	quantification__set_quant_vars(QuantVars),
	quantification__set_outside(OutsideVars),
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
	quantification__get_quant_vars(QuantVars),
	quantification__get_outside(OutsideVars),
	implicitly_quantify_goal(Goal0, Goal),
	quantification__get_nonlocals(NonLocalVars0),
	quantification__set_quant_vars(QuantVars),
	quantification__set_outside(OutsideVars),
	implicitly_quantify_cases(Cases0, Cases),
	quantification__get_nonlocals(NonLocalVars1),
	{ set__union(NonLocalVars0, NonLocalVars1, NonLocalVars) },
	quantification__set_quant_vars(QuantVars),
	quantification__set_outside(OutsideVars),
	quantification__set_nonlocals(NonLocalVars).

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

goal_vars_2(pragma_c_code(_, _, _, ArgVars, _), Set0, Set) :-
	set__insert_list(Set0, ArgVars, Set).

:- pred unify_rhs_vars(unify_rhs, set(var), set(var)).
:- mode unify_rhs_vars(in, in, out) is det.

unify_rhs_vars(var(X), Set0, Set) :-
	set__insert(Set0, X, Set).
unify_rhs_vars(functor(_Functor, ArgVars), Set0, Set) :-
	set__insert_list(Set0, ArgVars, Set).
unify_rhs_vars(lambda_goal(LambdaVars, _Modes, _Detism, Goal), Set0, Set) :-
	goal_vars(Goal, GoalVars),
	set__delete_list(GoalVars, LambdaVars, GoalVars1),
	set__union(Set0, GoalVars1, Set).

%-----------------------------------------------------------------------------%

:- pred quantification__rename_apart(set(var), set(var),
				hlds__goal, hlds__goal, quant_info, quant_info).
:- mode quantification__rename_apart(in, out, in, out, in, out) is det.

quantification__rename_apart(RenameSet, NewSet, Goal0, Goal) -->
	{ set__to_sorted_list(RenameSet, RenameList) },
	quantification__get_varset(Varset0),
	quantification__get_vartypes(VarTypes0),
	{ map__init(NewMap0) },
	{ goal_util__create_variables(RenameList, Varset0, VarTypes0, NewMap0,
		Varset, VarTypes, NewMap) },
	{ map__values(NewMap, NewList) },
	{ set__list_to_set(NewList, NewSet) },
	{ goal_util__rename_vars_in_goal(Goal0, NewMap, Goal) },
	quantification__set_varset(Varset),
	quantification__set_vartypes(VarTypes),
	quantification__get_seen(SeenVars0),
	{ set__union(SeenVars0, NewSet, SeenVars) },
	quantification__set_seen(SeenVars).

%-----------------------------------------------------------------------------%

:- pred quantification__init(set(var), varset, map(var, type), quant_info).
:- mode quantification__init(in, in, in, out) is det.

quantification__init(OutsideVars, Varset, VarTypes, QuantInfo) :-
	set__init(QuantVars),
	set__init(NonLocals),
	Seen = OutsideVars,
	QuantInfo = quant_info(OutsideVars, QuantVars, NonLocals, Seen, Varset,
		VarTypes).

:- pred quantification__get_outside(set(var), quant_info, quant_info).
:- mode quantification__get_outside(out, in, out) is det.

quantification__get_outside(A, Q, Q) :-
	Q = quant_info(A, _, _, _, _, _).

:- pred quantification__set_outside(set(var), quant_info, quant_info).
:- mode quantification__set_outside(in, in, out) is det.

quantification__set_outside(A, Q0, Q) :-
	Q0 = quant_info(_, B, C, D, E, F),
	Q  = quant_info(A, B, C, D, E, F).

:- pred quantification__get_quant_vars(set(var), quant_info, quant_info).
:- mode quantification__get_quant_vars(out, in, out) is det.

quantification__get_quant_vars(B, Q, Q) :-
	Q = quant_info(_, B, _, _, _, _).

:- pred quantification__set_quant_vars(set(var), quant_info, quant_info).
:- mode quantification__set_quant_vars(in, in, out) is det.

quantification__set_quant_vars(B, Q0, Q) :-
	Q0 = quant_info(A, _, C, D, E, F),
	Q  = quant_info(A, B, C, D, E, F).

:- pred quantification__get_nonlocals(set(var), quant_info, quant_info).
:- mode quantification__get_nonlocals(out, in, out) is det.

quantification__get_nonlocals(C, Q, Q) :-
	Q  = quant_info(_, _, C, _, _, _).

:- pred quantification__set_nonlocals(set(var), quant_info, quant_info).
:- mode quantification__set_nonlocals(in, in, out) is det.

quantification__set_nonlocals(C, Q0, Q) :-
	Q0 = quant_info(A, B, _, D, E, F),
	Q  = quant_info(A, B, C, D, E, F).

:- pred quantification__get_seen(set(var), quant_info, quant_info).
:- mode quantification__get_seen(out, in, out) is det.

quantification__get_seen(D, Q, Q) :-
	Q  = quant_info(_, _, _, D, _, _).

:- pred quantification__set_seen(set(var), quant_info, quant_info).
:- mode quantification__set_seen(in, in, out) is det.

quantification__set_seen(D, Q0, Q) :-
	Q0 = quant_info(A, B, C, _, E, F),
	Q  = quant_info(A, B, C, D, E, F).

:- pred quantification__get_varset(varset, quant_info, quant_info).
:- mode quantification__get_varset(out, in, out) is det.

quantification__get_varset(E, Q, Q) :-
	Q  = quant_info(_, _, _, _, E, _).

:- pred quantification__set_varset(varset, quant_info, quant_info).
:- mode quantification__set_varset(in, in, out) is det.

quantification__set_varset(E, Q0, Q) :-
	Q0 = quant_info(A, B, C, D, _, F),
	Q  = quant_info(A, B, C, D, E, F).

:- pred quantification__get_vartypes(map(var, type), quant_info, quant_info).
:- mode quantification__get_vartypes(out, in, out) is det.

quantification__get_vartypes(F, Q, Q) :-
	Q  = quant_info(_, _, _, _, _, F).

:- pred quantification__set_vartypes(map(var, type), quant_info, quant_info).
:- mode quantification__set_vartypes(in, in, out) is det.

quantification__set_vartypes(F, Q0, Q) :-
	Q0 = quant_info(A, B, C, D, E, _),
	Q  = quant_info(A, B, C, D, E, F).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
