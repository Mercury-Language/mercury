%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2004 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module looks after goal paths, which associate each goal
% with its position in a procedure definition,

% Main author: zs.

:- module check_hlds__goal_path.

:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.

:- import_module bool.

:- pred goal_path__fill_slots(module_info::in, proc_info::in, proc_info::out)
	is det.

	% Fill in the goal_paths for goals in the clauses_info of the predicate.
	% Clauses are given goal paths `disj(1)', ...,  `disj(N)'.  If the bool
	% argument is true then the goal paths are stored in a form where any
	% prefix consisting of `disj(_)', `neg', `exist(_)' and `ite_else'
	% components is removed.  This is used to optimise the constraint-based
	% mode analysis where the instantiatedness of a variable at such a goal
	% path is always equivalent to its instantiatedness at the parent goal
	% path.

:- pred goal_path__fill_slots_in_clauses(module_info::in, bool::in,
	pred_info::in, pred_info::out) is det.

:- pred goal_path__fill_slots_in_goal(hlds_goal::in, vartypes::in,
	module_info::in, hlds_goal::out) is det.

:- implementation.

:- import_module check_hlds__type_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module parse_tree__prog_data.

:- import_module char, int, list, map, std_util, require.

:- type slot_info
	--->	slot_info(
			vartypes			:: vartypes,
			module_info			:: module_info,
			omit_mode_equiv_prefix	 	:: bool
		).

goal_path__fill_slots(ModuleInfo, !Proc) :-
	proc_info_goal(!.Proc, Goal0),
	proc_info_vartypes(!.Proc, VarTypes),
	goal_path__fill_slots_in_goal(Goal0, VarTypes, ModuleInfo, Goal),
	proc_info_set_goal(Goal, !Proc).

goal_path__fill_slots_in_clauses(ModuleInfo, OmitModeEquivPrefix, !PredInfo) :-
	pred_info_clauses_info(!.PredInfo, ClausesInfo0),
	clauses_info_clauses(ClausesInfo0, Clauses0),
	clauses_info_vartypes(ClausesInfo0, VarTypes),
	SlotInfo = slot_info(VarTypes, ModuleInfo, OmitModeEquivPrefix),
	list__map_foldl(fill_slots_in_clause(SlotInfo), Clauses0, Clauses,
		1, _),
	clauses_info_set_clauses(Clauses, ClausesInfo0, ClausesInfo),
	pred_info_set_clauses_info(ClausesInfo, !PredInfo).

:- pred fill_slots_in_clause(slot_info::in, clause::in, clause::out,
	int::in, int::out) is det.

fill_slots_in_clause(SlotInfo, Clause0, Clause, ClauseNum, ClauseNum + 1) :-
	Clause0 = clause(ProcIds, Goal0, Lang, Context),
	fill_goal_slots(Goal0, [disj(ClauseNum)], SlotInfo, Goal),
	Clause = clause(ProcIds, Goal, Lang, Context).

goal_path__fill_slots_in_goal(Goal0, VarTypes, ModuleInfo, Goal) :-
	SlotInfo = slot_info(VarTypes, ModuleInfo, no),
	fill_goal_slots(Goal0, [], SlotInfo, Goal).

:- pred fill_goal_slots(hlds_goal::in, goal_path::in, slot_info::in,
	hlds_goal::out) is det.

fill_goal_slots(Expr0 - Info0, Path0, SlotInfo, Expr - Info) :-
	OmitModeEquivPrefix = SlotInfo ^ omit_mode_equiv_prefix,
	(
		OmitModeEquivPrefix = yes,
		list__takewhile(mode_equiv_step, Path0, _, Path)
	;
		OmitModeEquivPrefix = no,
		Path = Path0
	),
	goal_info_set_goal_path(Info0, Path, Info),
	fill_expr_slots(Expr0, Info, Path0, SlotInfo, Expr).

:- pred mode_equiv_step(goal_path_step::in) is semidet.

mode_equiv_step(Step) :-
	( Step = disj(_)
	; Step = neg
	; Step = exist(_)
	; Step = ite_else
	).

:- pred fill_expr_slots(hlds_goal_expr::in, hlds_goal_info::in, goal_path::in,
	slot_info::in, hlds_goal_expr::out) is det.

fill_expr_slots(conj(Goals0), _, Path0, SlotInfo, conj(Goals)) :-
	fill_conj_slots(Goals0, Path0, 0, SlotInfo, Goals).
fill_expr_slots(par_conj(Goals0), _, Path0, SlotInfo,
		par_conj(Goals)) :-
	fill_conj_slots(Goals0, Path0, 0, SlotInfo, Goals).
fill_expr_slots(disj(Goals0), _, Path0, SlotInfo, disj(Goals)) :-
	fill_disj_slots(Goals0, Path0, 0, SlotInfo, Goals).
fill_expr_slots(switch(Var, B, Cases0), _, Path0, SlotInfo,
		switch(Var, B, Cases)) :-
	VarTypes = SlotInfo ^ vartypes,
	ModuleInfo = SlotInfo ^ module_info,
	map__lookup(VarTypes, Var, Type),
	(
		type_util__switch_type_num_functors(ModuleInfo, Type,
			NumFunctors)
	->
		NumCases = NumFunctors
	;
		NumCases = -1
	),
	fill_switch_slots(Cases0, Path0, 0, NumCases, SlotInfo, Cases).
fill_expr_slots(not(Goal0), _, Path0, SlotInfo, not(Goal)) :-
	fill_goal_slots(Goal0, [neg | Path0], SlotInfo, Goal).
fill_expr_slots(some(A, B, Goal0), OuterInfo, Path0, SlotInfo,
		some(A, B, Goal)) :-
	Goal0 = _ - InnerInfo,
	goal_info_get_determinism(OuterInfo, OuterDetism),
	goal_info_get_determinism(InnerInfo, InnerDetism),
	( InnerDetism = OuterDetism ->
		MaybeCut = no_cut
	;
		MaybeCut = cut
	),
	fill_goal_slots(Goal0, [exist(MaybeCut) | Path0], SlotInfo, Goal).
fill_expr_slots(if_then_else(A, Cond0, Then0, Else0), _, Path0, SlotInfo,
		if_then_else(A, Cond, Then, Else)) :-
	fill_goal_slots(Cond0, [ite_cond | Path0], SlotInfo, Cond),
	fill_goal_slots(Then0, [ite_then | Path0], SlotInfo, Then),
	fill_goal_slots(Else0, [ite_else | Path0], SlotInfo, Else).
fill_expr_slots(Goal @ call(_, _, _, _, _, _), _, _, _, Goal).
fill_expr_slots(Goal @ generic_call(_, _, _, _), _, _, _, Goal).
fill_expr_slots(Goal @ unify(_, _, _, _, _), _, _, _, Goal).
fill_expr_slots(Goal @ foreign_proc(_, _, _, _, _, _), _, _, _, Goal).
fill_expr_slots(shorthand(_), _, _, _, _) :-
	% these should have been expanded out by now
	error("fill_expr_slots: unexpected shorthand").

:- pred fill_conj_slots(list(hlds_goal)::in, goal_path::in, int::in,
	slot_info::in, list(hlds_goal)::out) is det.

fill_conj_slots([], _, _, _, []).
fill_conj_slots([Goal0 | Goals0], Path0, N0, SlotInfo, [Goal | Goals]) :-
	N1 = N0 + 1,
	fill_goal_slots(Goal0, [conj(N1) | Path0], SlotInfo, Goal),
	fill_conj_slots(Goals0, Path0, N1, SlotInfo, Goals).

:- pred fill_disj_slots(list(hlds_goal)::in, goal_path::in, int::in,
	slot_info::in, list(hlds_goal)::out) is det.

fill_disj_slots([], _, _, _, []).
fill_disj_slots([Goal0 | Goals0], Path0, N0, SlotInfo, [Goal | Goals]) :-
	N1 = N0 + 1,
	fill_goal_slots(Goal0, [disj(N1) | Path0], SlotInfo, Goal),
	fill_disj_slots(Goals0, Path0, N1, SlotInfo, Goals).

:- pred fill_switch_slots(list(case)::in, goal_path::in, int::in, int::in,
	slot_info::in, list(case)::out) is det.

fill_switch_slots([], _, _, _, _, []).
fill_switch_slots([case(A, Goal0) | Cases0], Path0, N0, NumCases, SlotInfo,
		[case(A, Goal) | Cases]) :-
	N1 = N0 + 1,
	fill_goal_slots(Goal0, [switch(N1, NumCases) | Path0], SlotInfo, Goal),
	fill_switch_slots(Cases0, Path0, N1, NumCases, SlotInfo, Cases).
