%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module traverses the HLDS, updating the `how_to_construct'
% field of construction unifications.  For each construction which
% can be done statically, i.e. whose arguments are all static,
% it replaces this field with `construct_statically'.
% This field is then used by the MLDS back-end to determine when it can
% generate static initialized constants rather than using
% new_object() MLDS statements.

% Main author: fjh.

:- module ml_backend__mark_static_terms.

:- interface.

:- import_module hlds__hlds_pred, hlds__hlds_module.

:- pred mark_static_terms(proc_info::in, module_info::in, proc_info::out)
	is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module parse_tree__prog_data, hlds__hlds_goal, hlds__hlds_data.
:- import_module map, list, bool.

%
% As we traverse the goal, we keep track of which variables are static at
% this point, and for each such variable, we keep information on how to
% construct it.
%
:- type static_info == map(prog_var, static_cons).

:- import_module hlds__hlds_goal.
:- import_module int, list, std_util, require.

mark_static_terms(Proc0, _ModuleInfo, Proc) :-
		% The ModuleInfo argument is there just for passes_aux
	proc_info_goal(Proc0, Goal0),
	map__init(StaticInfo0),
	goal_mark_static_terms(Goal0, Goal, StaticInfo0, _StaticInfo),
	proc_info_set_goal(Proc0, Goal, Proc).

:- pred goal_mark_static_terms(hlds_goal::in, hlds_goal::out,
		static_info::in, static_info::out) is det.

goal_mark_static_terms(GoalExpr0 - GoalInfo, GoalExpr - GoalInfo) -->
	goal_expr_mark_static_terms(GoalExpr0, GoalExpr).

:- pred goal_expr_mark_static_terms(hlds_goal_expr::in, hlds_goal_expr::out,
		static_info::in, static_info::out) is det.

goal_expr_mark_static_terms(conj(Goals0), conj(Goals), SI0, SI) :-
	conj_mark_static_terms(Goals0, Goals, SI0, SI).

goal_expr_mark_static_terms(par_conj(Goals0), par_conj(Goals),
		SI0, SI) :-
	% it's OK to treat parallel conjunctions as if they were
	% sequential here, since if we mark any variables as
	% static, the computation of those variables will be
	% done at compile time.
	conj_mark_static_terms(Goals0, Goals, SI0, SI).

goal_expr_mark_static_terms(disj(Goals0), disj(Goals), SI0, SI0) :-
	% we revert to the original static_info at the end of branched goals
	disj_mark_static_terms(Goals0, Goals, SI0).

goal_expr_mark_static_terms(switch(A, B, Cases0), switch(A, B, Cases),
		SI0, SI0) :-
	% we revert to the original static_info at the end of branched goals
	cases_mark_static_terms(Cases0, Cases, SI0).

goal_expr_mark_static_terms(not(Goal0), not(Goal), SI0, SI0) :-
	% we revert to the original static_info at the end of the negation
	goal_mark_static_terms(Goal0, Goal, SI0, _SI).

goal_expr_mark_static_terms(some(A, B, Goal0), some(A, B, Goal), SI0, SI) :-
	goal_mark_static_terms(Goal0, Goal, SI0, SI).

goal_expr_mark_static_terms(if_then_else(A, Cond0, Then0, Else0),
		if_then_else(A, Cond, Then, Else), SI0, SI0) :-
	% we run the Cond and the Then in sequence,
	% and we run the Else in parallel with that,
	% and then we throw away the static_infos we computed
	% and revert to the original static_info at the end,
	% since this was a branched goal.
	goal_mark_static_terms(Cond0, Cond, SI0, SI_Cond),
	goal_mark_static_terms(Then0, Then, SI_Cond, _SI_Then),
	goal_mark_static_terms(Else0, Else, SI0, _SI_Else).

goal_expr_mark_static_terms(call(A,B,C,D,E,F), call(A,B,C,D,E,F), SI, SI).

goal_expr_mark_static_terms(generic_call(A,B,C,D), generic_call(A,B,C,D),
	SI, SI).

goal_expr_mark_static_terms(unify(A,B,C, Unification0, E),
		unify(A,B,C, Unification, E), SI0, SI) :-
	unification_mark_static_terms(Unification0, Unification,
		SI0, SI).

goal_expr_mark_static_terms(foreign_proc(A,B,C,D,E,F,G),
		foreign_proc(A,B,C,D,E,F,G), SI, SI).

goal_expr_mark_static_terms(shorthand(_), _, _, _) :-
	% these should have been expanded out by now
	error("fill_expr_slots: unexpected shorthand").

:- pred conj_mark_static_terms(hlds_goals::in, hlds_goals::out,
		static_info::in, static_info::out) is det.
conj_mark_static_terms(Goals0, Goals) -->
	list__map_foldl(goal_mark_static_terms, Goals0, Goals).
	
:- pred disj_mark_static_terms(hlds_goals::in, hlds_goals::out,
		static_info::in) is det.
disj_mark_static_terms([], [], _).
disj_mark_static_terms([Goal0 | Goals0], [Goal | Goals], SI0) :-
	% we throw away the static_info obtained after each branch
	goal_mark_static_terms(Goal0, Goal, SI0, _SI),
	disj_mark_static_terms(Goals0, Goals, SI0).

:- pred cases_mark_static_terms(list(case)::in, list(case)::out,
		static_info::in) is det.
cases_mark_static_terms([], [], _SI0).
cases_mark_static_terms([Case0 | Cases0], [Case | Cases], SI0) :-
	Case0 = case(ConsId, Goal0),
	Case = case(ConsId, Goal),
	% we throw away the static_info obtained after each branch
	goal_mark_static_terms(Goal0, Goal, SI0, _SI),
	cases_mark_static_terms(Cases0, Cases, SI0).

:- pred unification_mark_static_terms(unification::in, unification::out,
		static_info::in, static_info::out) is det.
unification_mark_static_terms(Unification0, Unification,
		StaticVars0, StaticVars) :-
	(
		Unification0 = construct(Var, ConsId, ArgVars, D,
			HowToConstruct0, F, G),
		(
			% if all the arguments are static,
			% then the newly constructed variable
			% is static too
			CheckStaticArg = (pred(V::in, C::out) is semidet :-
				map__search(StaticVars0, V, C)),
			list__map(CheckStaticArg, ArgVars, StaticArgs)
		->
			HowToConstruct = construct_statically(StaticArgs),
			map__det_insert(StaticVars0, Var,
				static_cons(ConsId, ArgVars, StaticArgs),
				StaticVars)
		;
			HowToConstruct = HowToConstruct0,
			StaticVars = StaticVars0
		),
		( HowToConstruct = HowToConstruct0 ->
			% this is a minor optimization to improve the
			% efficiency of the compiler: don't bother
			% allocating memory if we don't need to
			Unification = Unification0
		;
			Unification = construct(Var, ConsId, ArgVars, D,
				HowToConstruct, F, G)
		)
	;
		Unification0 = deconstruct(_Var, _ConsId, _ArgVars, _UniModes,
			_CanFail, _CanCGC),
		Unification = Unification0,
		StaticVars = StaticVars0
/*****************
		(
			% if the variable being deconstructed is static,
			% and the deconstruction cannot fail,
			% then the newly extracted argument variables
			% are static too
			% (XXX is the "cannot fail" bit really necessary?)
			map__search(StaticVars0, Var, Data),
			CanFail = cannot_fail
		->
			XXX insert ArgVars into StaticVars0
		;
			StaticVars = StaticVars0
		)
*****************/
	;
		Unification0 = assign(TargetVar, SourceVar),
		Unification = Unification0,
		(
			% if the variable being assign from is static,
			% then the variable being assigned to is static too
			map__search(StaticVars0, SourceVar, Data)
		->
			map__det_insert(StaticVars0, TargetVar, Data,
				StaticVars)
		;
			StaticVars = StaticVars0
		)
	;
		Unification0 = simple_test(_, _),
		Unification = Unification0,
		StaticVars = StaticVars0
	;
		Unification0 = complicated_unify(_, _, _),
		Unification = Unification0,
		StaticVars = StaticVars0
	).

