%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: quantification.nl.
% Main author: fjh.

	% Make implicit quantification explicit.
	% For the rules on implicit quantification, see the
	% file compiler/notes/IMPLICIT_QUANTIFICATION.
	%
	% Rather than making implicit quantification explicit by
	% inserting additional existential quantifiers in the form of
	% `some/2' goals, we instead record existential quantification
	% in the goal_info for each goal.  In fact we could (should?)
	% even remove any explicit existential quantifiers that were
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

:- pred implicitly_quantify_goal(hlds__goal, set(var), hlds__goal, set(var)).
:- mode implicitly_quantify_goal(in, in, out, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util.

implicitly_quantify_clause_body(HeadVars, Goal0, Goal) :-
	set__list_to_set(HeadVars, Set),
	implicitly_quantify_goal(Goal0, Set, Goal, _).

implicitly_quantify_goal(Goal0 - GoalInfo0, OutsideVars,
			Goal - GoalInfo, NonLocalVars) :-
	implicitly_quantify_goal_2(Goal0, OutsideVars, Goal, NonLocalVars),
	goal_info_set_nonlocals(GoalInfo0, NonLocalVars, GoalInfo).

:- pred implicitly_quantify_goal_2(hlds__goal_expr, set(var),
				   hlds__goal_expr, set(var)).
:- mode implicitly_quantify_goal_2(in, in, out, out) is det.

implicitly_quantify_goal_2(some(Vars, Goal0), OutsideVars,
			   some(Vars, Goal), NonLocals) :-
	set__insert_list(OutsideVars, Vars, OutsideVars1),
	implicitly_quantify_goal(Goal0, OutsideVars1, Goal, NonLocals0),
	set__remove_list(NonLocals0, Vars, NonLocals).

/********
	% perhaps we should instead remove explicit quantifiers
	% as follows (and also for not/2):
implicitly_quantify_goal_2(some(Vars, Goal0), OutsideVars, Goal, NonLocals) :-
	set__remove_list(OutsideVars, Vars, OutsideVars1),
	implicitly_quantify_goal(Goal0, OutsideVars1, Goal1, NonLocals0),
	set__remove_list(NonLocals0, Vars, NonLocals),
	Goal1 = G - GoalInfo1,
	goal_info_set_nonlocals(GoalInfo1, NonLocals, GoalInfo).
	Goal = G - GoalInfo.
*********/

implicitly_quantify_goal_2(conj(List0), OutsideVars,
			   conj(List), NonLocalVars) :-
	implicitly_quantify_conj(List0, OutsideVars, List, NonLocalVars).

implicitly_quantify_goal_2(disj(Goals0), OutsideVars,
			   disj(Goals), NonLocalVars) :-
	implicitly_quantify_disj(Goals0, OutsideVars, Goals, NonLocalVars).

implicitly_quantify_goal_2(switch(Var, Cases0, F), OutsideVars,
			   switch(Var, Cases, F), NonLocalVars) :-
	implicitly_quantify_cases(Cases0, OutsideVars, Cases, NonLocalVars).

implicitly_quantify_goal_2(not(Vars, Goal0), OutsideVars,
		    not(Vars, Goal), NonLocals) :-
	set__insert_list(OutsideVars, Vars, OutsideVars1),
	implicitly_quantify_goal(Goal0, OutsideVars1, Goal, NonLocals0),
	set__remove_list(NonLocals0, Vars, NonLocals).

implicitly_quantify_goal_2(if_then_else(Vars, A0, B0, C0), OutsideVars,
		if_then_else(Vars, A, B, C), NonLocals) :-
	set__insert_list(OutsideVars, Vars, OutsideVars0),
	goal_vars(B0, VarsB),
	set__union(OutsideVars0, VarsB, OutsideVars1),
	implicitly_quantify_goal(A0, OutsideVars1, A, NonLocalsA),
	set__union(OutsideVars, NonLocalsA, OutsideVars2),
	implicitly_quantify_goal(B0, OutsideVars2, B, NonLocalsB),
	implicitly_quantify_goal(C0, OutsideVars, C, NonLocalsC),
	set__union(NonLocalsA, NonLocalsB, NonLocalsSuccess),
	set__union(NonLocalsSuccess, NonLocalsC, NonLocalsIfThenElse),
	set__intersect(NonLocalsIfThenElse, OutsideVars, NonLocals).

implicitly_quantify_goal_2(call(A, B, HeadArgs, D, E), OutsideVars,
		call(A, B, HeadArgs, D, E), NonLocalVars) :-
	term__vars_list(HeadArgs, HeadVars),
	set__list_to_set(HeadVars, GoalVars),
	set__intersect(GoalVars, OutsideVars, NonLocalVars).

implicitly_quantify_goal_2(unify(TermA, TermB, X, Y, Z), OutsideVars,
			unify(TermA, TermB, X, Y, Z), NonLocalVars) :-
	term__vars(TermA, VarsA),
	term__vars(TermB, VarsB),
	list__append(VarsA, VarsB, Vars),
	set__list_to_set(Vars, GoalVars),
	set__intersect(GoalVars, OutsideVars, NonLocalVars).

:- pred implicitly_quantify_conj(list(hlds__goal), set(var),
				 list(hlds__goal), set(var)).
:- mode implicitly_quantify_conj(in, in, out, out) is det.

implicitly_quantify_conj(Goals0, OutsideVars, Goals, NonLocalVars) :-
	get_vars(Goals0, FollowingVarsList),
	implicitly_quantify_conj_2(Goals0, FollowingVarsList, OutsideVars,
				Goals, NonLocalVars).

:- pred implicitly_quantify_conj_2(list(hlds__goal), list(set(var)), set(var),
			list(hlds__goal), set(var)).
:- mode implicitly_quantify_conj_2(in, in, in, out, out) is det.

implicitly_quantify_conj_2([], _, _, [], NonLocalVars) :-
	set__init(NonLocalVars).
implicitly_quantify_conj_2([Goal0 | Goals0],
			[FollowingVars | FollowingVarsList],
			OutsideVars,
			[Goal | Goals], NonLocalVars) :-
	set__union(OutsideVars, FollowingVars, OutsideVars1),
	implicitly_quantify_goal(Goal0, OutsideVars1, Goal, NonLocalVars1),
	set__union(OutsideVars, NonLocalVars1, OutsideVars2),
	implicitly_quantify_conj_2(Goals0, FollowingVarsList, OutsideVars2,
				Goals, NonLocalVars2),
	set__union(NonLocalVars1, NonLocalVars2, NonLocalVarsConj),
	set__intersect(NonLocalVarsConj, OutsideVars, NonLocalVars).

:- pred implicitly_quantify_disj(list(hlds__goal), set(var),
				 list(hlds__goal), set(var)).
:- mode implicitly_quantify_disj(in, in, out, out) is det.

implicitly_quantify_disj([], _, [], NonLocalVars) :-
	set__init(NonLocalVars).
implicitly_quantify_disj([Goal0 | Goals0], OutsideVars,
			[Goal | Goals], NonLocalVars) :-
	implicitly_quantify_goal(Goal0, OutsideVars, Goal, NonLocalVars0),
	implicitly_quantify_disj(Goals0, OutsideVars, Goals, NonLocalVars1),
	set__union(NonLocalVars0, NonLocalVars1, NonLocalVars).

:- pred implicitly_quantify_cases(list(case), set(var),
				 list(case), set(var)).
:- mode implicitly_quantify_cases(in, in, out, out) is det.

implicitly_quantify_cases([], _, [], NonLocalVars) :-
	set__init(NonLocalVars).
implicitly_quantify_cases([case(Cons, Goal0) | Cases0], OutsideVars,
			[case(Cons, Goal) | Cases], NonLocalVars) :-
	implicitly_quantify_goal(Goal0, OutsideVars, Goal, NonLocalVars0),
	implicitly_quantify_cases(Cases0, OutsideVars, Cases, NonLocalVars1),
	set__union(NonLocalVars0, NonLocalVars1, NonLocalVars).

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

:- pred goal_vars(hlds__goal, set(var)).
:- mode goal_vars(in, out) is det.

goal_vars(Goal - _GoalInfo, Set) :-
	set__init(Set0),
	goal_vars_2(Goal, Set0, Set).

:- pred goal_vars_2(hlds__goal_expr, set(var), set(var)).
:- mode goal_vars_2(in, in, out) is det.

goal_vars_2(unify(A, B, _, _, _), Set0, Set) :-
	term__vars(A, VarsA),
	set__insert_list(Set0, VarsA, Set1),
	term__vars(B, VarsB),
	set__insert_list(Set1, VarsB, Set).

goal_vars_2(call(_, _, Args, _, _), Set0, Set) :-
	term__vars_list(Args, Vars),
	set__insert_list(Set0, Vars, Set).

goal_vars_2(conj(Goals), Set0, Set) :-
	goal_list_vars_2(Goals, Set0, Set).

goal_vars_2(disj(Goals), Set0, Set) :-
	goal_list_vars_2(Goals, Set0, Set).

goal_vars_2(switch(_Var, Cases, _F), Set0, Set) :-
	case_list_vars_2(Cases, Set0, Set).

goal_vars_2(some(Vars, Goal), Set0, Set) :-
	goal_vars(Goal, Set1),
	set__remove_list(Set1, Vars, Set2),
	set__union(Set0, Set2, Set).

goal_vars_2(not(Vars, Goal), Set0, Set) :-
	goal_vars(Goal, Set1),
	set__remove_list(Set1, Vars, Set2),
	set__union(Set0, Set2, Set).

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
	set__remove_list(Set3, Vars, Set4),
	set__union(Set0, Set4, Set5),
	goal_vars(C, Set6),
	set__union(Set5, Set6, Set).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
