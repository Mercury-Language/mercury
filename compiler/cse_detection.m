%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Common subexpression detection - hoist common subexpression goals out of
% branched structures. This can enable us to find more indexing opportunities
% and hence can make the code more deterministic.
% This code is switched on/off with the `--common-goal' option.
%
% Main author: zs.
% Much of the code is based on switch_detection.m by fjh.
%
%-----------------------------------------------------------------------------%

:- module cse_detection.

:- interface.

:- import_module hlds_module, hlds_pred, io.

:- pred detect_cse(module_info, module_info, io__state, io__state).
:- mode detect_cse(in, out, di, uo) is det.

:- pred detect_cse_in_proc(proc_id, pred_id, module_info, module_info,
			io__state, io__state).
% :- mode detect_cse_in_proc(in, in, di, uo, di, uo) is det.
:- mode detect_cse_in_proc(in, in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, hlds_data, options, globals, goal_util, hlds_out.
:- import_module modes, mode_util, make_hlds, quantification, instmap.
:- import_module prog_data, switch_detection, det_util.

:- import_module int, bool, list, map, set, std_util, require, term, varset.

%-----------------------------------------------------------------------------%

	% Traverse the module structure, calling `detect_cse_in_goal'
	% for each procedure body.

detect_cse(ModuleInfo0, ModuleInfo) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	detect_cse_in_preds(PredIds, ModuleInfo0, ModuleInfo).

:- pred detect_cse_in_preds(list(pred_id), module_info, module_info,
	io__state, io__state).
:- mode detect_cse_in_preds(in, in, out, di, uo) is det.

detect_cse_in_preds([], ModuleInfo, ModuleInfo) --> [].
detect_cse_in_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	detect_cse_in_pred(PredId, PredInfo, ModuleInfo0, ModuleInfo1),
	detect_cse_in_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred detect_cse_in_pred(pred_id, pred_info, module_info, module_info,
	io__state, io__state).
:- mode detect_cse_in_pred(in, in, in, out, di, uo) is det.

detect_cse_in_pred(PredId, PredInfo0, ModuleInfo0, ModuleInfo) -->
	{ pred_info_non_imported_procids(PredInfo0, ProcIds) },
	detect_cse_in_procs(ProcIds, PredId, ModuleInfo0, ModuleInfo).

:- pred detect_cse_in_procs(list(proc_id), pred_id, module_info, module_info,
			io__state, io__state).
% :- mode detect_cse_in_procs(in, in, di, uo, di, uo) is det.
:- mode detect_cse_in_procs(in, in, in, out, di, uo) is det.

detect_cse_in_procs([], _PredId, ModuleInfo, ModuleInfo) --> [].
detect_cse_in_procs([ProcId | ProcIds], PredId, ModuleInfo0, ModuleInfo) -->
	detect_cse_in_proc(ProcId, PredId, ModuleInfo0, ModuleInfo1),
	detect_cse_in_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

detect_cse_in_proc(ProcId, PredId, ModuleInfo0, ModuleInfo) -->
	{ detect_cse_in_proc_2(ProcId, PredId, Redo, ModuleInfo0,
		ModuleInfo1) },
	( { Redo = no } ->
		{ ModuleInfo = ModuleInfo1 }
	;
		globals__io_lookup_bool_option(very_verbose, VeryVerbose),
		( { VeryVerbose = yes } ->
			io__write_string("% Repeating mode check for "),
			hlds_out__write_pred_id(ModuleInfo1, PredId),
			io__write_string("\n")
		;
			[]
		),
		modecheck_proc(ProcId, PredId, ModuleInfo1, ModuleInfo2, Errs),
		{ Errs > 0 ->
			error("mode check fails when repeated")
		;
			true
		},
		( { VeryVerbose = yes } ->
			io__write_string("% Repeating switch detection for "),
			hlds_out__write_pred_id(ModuleInfo2, PredId),
			io__write_string("\n")
		;
			[]
		),
		{ detect_switches_in_proc(ProcId, PredId,
			ModuleInfo2, ModuleInfo3) },

		( { VeryVerbose = yes } ->
			io__write_string("% Repeating common deconstruction detection for "),
			hlds_out__write_pred_id(ModuleInfo0, PredId),
			io__write_string("\n")
		;
			[]
		),
		detect_cse_in_proc(ProcId, PredId, ModuleInfo3, ModuleInfo)
	).

:- type cse_info	--->	cse_info(varset, map(var, type), module_info).

:- pred detect_cse_in_proc_2(proc_id, pred_id, bool, module_info, module_info).
% :- mode detect_cse_in_proc_2(in, in, out, di, uo) is det.
:- mode detect_cse_in_proc_2(in, in, out, in, out) is det.

detect_cse_in_proc_2(ProcId, PredId, Redo, ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

		% To process each ProcInfo, we get the goal,
		% initialize the instmap based on the modes of the head vars,
		% and pass these to `detect_cse_in_goal'.

	proc_info_goal(ProcInfo0, Goal0),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0),
	proc_info_variables(ProcInfo0, Varset0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	CseInfo0 = cse_info(Varset0, VarTypes0, ModuleInfo0),
	detect_cse_in_goal(Goal0, InstMap0, CseInfo0, CseInfo, Redo, Goal1),

	(
		Redo = no,
		ModuleInfo = ModuleInfo0
	;
		Redo = yes,

		% ModuleInfo should not be changed by detect_cse_in_goal
		CseInfo = cse_info(Varset1, VarTypes1, _),
		proc_info_headvars(ProcInfo0, HeadVars),

		implicitly_quantify_clause_body(HeadVars, Goal1, Varset1,
			VarTypes1, Goal, Varset, VarTypes, _Warnings),

		proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
		proc_info_set_variables(ProcInfo1, Varset, ProcInfo2),
		proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo),

		map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
		pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
		map__det_update(PredTable0, PredId, PredInfo, PredTable),
		module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo)
	).

%-----------------------------------------------------------------------------%

	% Given a goal, and the instmap on entry to that goal,
	% find disjunctions that contain common subexpressions
	% and hoist these out of the disjunction. At the moment
	% we only look for cses that are deconstruction unifications.

:- pred detect_cse_in_goal(hlds_goal, instmap, cse_info, cse_info,
	bool, hlds_goal).
:- mode detect_cse_in_goal(in, in, in, out, out, out) is det.

detect_cse_in_goal(Goal0, InstMap0, CseInfo0, CseInfo, Redo, Goal) :-
	detect_cse_in_goal_1(Goal0, InstMap0, CseInfo0, CseInfo,
		Redo, Goal, _InstMap).

	% This version is the same as the above except that it returns
	% the resulting instmap on exit from the goal, which is
	% computed by applying the instmap delta specified in the
	% goal's goalinfo.

:- pred detect_cse_in_goal_1(hlds_goal, instmap, cse_info, cse_info, bool,
	hlds_goal, instmap).
:- mode detect_cse_in_goal_1(in, in, in, out, out, out, out) is det.

detect_cse_in_goal_1(Goal0 - GoalInfo, InstMap0, CseInfo0, CseInfo, Redo,
		Goal - GoalInfo, InstMap) :-
	detect_cse_in_goal_2(Goal0, GoalInfo, InstMap0, CseInfo0, CseInfo,
		Redo, Goal),
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap).

	% Here we process each of the different sorts of goals.

:- pred detect_cse_in_goal_2(hlds_goal_expr, hlds_goal_info, instmap,
	cse_info, cse_info, bool, hlds_goal_expr).
:- mode detect_cse_in_goal_2(in, in, in, in, out, out, out) is det.

detect_cse_in_goal_2(pragma_c_code(A,B,C,D,E,F,G,H), _, _, CseInfo, CseInfo,
	no, pragma_c_code(A,B,C,D,E,F,G,H)).

detect_cse_in_goal_2(higher_order_call(A,B,C,D,E), _, _, CseInfo, CseInfo, no,
	higher_order_call(A,B,C,D,E)).

detect_cse_in_goal_2(call(A,B,C,D,E,F), _, _, CseInfo, CseInfo, no,
	call(A,B,C,D,E,F)).

detect_cse_in_goal_2(unify(A,B0,C,D,E), _, InstMap0, CseInfo0, CseInfo, Redo,
		unify(A,B,C,D,E)) :-
	( B0 = lambda_goal(PredOrFunc, Vars, Modes, Det, Goal0) ->
		CseInfo0 = cse_info(_, _, ModuleInfo),
		instmap__pre_lambda_update(ModuleInfo, 
			Vars, Modes, InstMap0, InstMap),
		detect_cse_in_goal(Goal0, InstMap, CseInfo0, CseInfo, Redo,
			Goal),
		B = lambda_goal(PredOrFunc, Vars, Modes, Det, Goal)
	;
		B = B0,
		CseInfo = CseInfo0,
		Redo = no
	).

detect_cse_in_goal_2(not(Goal0), _GoalInfo, InstMap, CseInfo0, CseInfo,
		Redo, not(Goal)) :-
	detect_cse_in_goal(Goal0, InstMap, CseInfo0, CseInfo, Redo, Goal).

detect_cse_in_goal_2(some(Vars, Goal0), _GoalInfo, InstMap, CseInfo0, CseInfo,
		Redo, some(Vars, Goal)) :-
	detect_cse_in_goal(Goal0, InstMap, CseInfo0, CseInfo, Redo, Goal).

detect_cse_in_goal_2(conj(Goals0), _GoalInfo, InstMap, CseInfo0, CseInfo,
		Redo, conj(Goals)) :-
	detect_cse_in_conj(Goals0, InstMap, CseInfo0, CseInfo, Redo, Goals).

detect_cse_in_goal_2(disj(Goals0, SM), GoalInfo, InstMap, CseInfo0, CseInfo,
		Redo, Goal) :-
	( Goals0 = [] ->
		CseInfo = CseInfo0,
		Redo = no,
		Goal = disj([], SM)
	;
		goal_info_get_nonlocals(GoalInfo, NonLocals),
		set__to_sorted_list(NonLocals, NonLocalsList),
		detect_cse_in_disj(NonLocalsList, Goals0, GoalInfo,
			SM, InstMap, CseInfo0, CseInfo, Redo, Goal)
	).

detect_cse_in_goal_2(switch(Var, CanFail, Cases0, SM), GoalInfo, InstMap,
		CseInfo0, CseInfo, Redo, Goal) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	set__to_sorted_list(NonLocals, NonLocalsList),
	detect_cse_in_cases(NonLocalsList, Var, CanFail, Cases0, GoalInfo,
		SM, InstMap, CseInfo0, CseInfo, Redo, Goal).

detect_cse_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0, SM), GoalInfo,
		InstMap, CseInfo0, CseInfo, Redo, Goal) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	set__to_sorted_list(NonLocals, NonLocalsList),
	detect_cse_in_ite(NonLocalsList, Vars, Cond0, Then0, Else0, GoalInfo,
		SM, InstMap, CseInfo0, CseInfo, Redo, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_cse_in_conj(list(hlds_goal), instmap, cse_info, cse_info,
	bool, list(hlds_goal)).
:- mode detect_cse_in_conj(in, in, in, out, out, out) is det.

detect_cse_in_conj([], _InstMap, CseInfo, CseInfo, no, []).
detect_cse_in_conj([Goal0 | Goals0], InstMap0, CseInfo0, CseInfo,
		Redo, Goals) :-
	detect_cse_in_goal_1(Goal0, InstMap0, CseInfo0, CseInfo1, Redo1, Goal1,
		InstMap1),
	detect_cse_in_conj(Goals0, InstMap1, CseInfo1, CseInfo, Redo2, Goals1),
	( Goal1 = conj(ConjGoals) - _ ->
		list__append(ConjGoals, Goals1, Goals)
	;
		Goals = [Goal1 | Goals1]
	),
	bool__or(Redo1, Redo2, Redo).

%-----------------------------------------------------------------------------%

	% These are the interesting bits - we've found a non-empty branched
	% structure, and we've got a list of the non-local variables of that
	% structure. Now for each non-local variable, we check whether each
	% branch matches that variable against the same functor.

:- pred detect_cse_in_disj(list(var), list(hlds_goal), hlds_goal_info,
	store_map, instmap, cse_info, cse_info, bool, hlds_goal_expr).
:- mode detect_cse_in_disj(in, in, in, in, in, in, out, out, out) is det.

detect_cse_in_disj([], Goals0, _, SM, InstMap, CseInfo0, CseInfo,
		Redo, disj(Goals, SM)) :-
	detect_cse_in_disj_2(Goals0, InstMap, CseInfo0, CseInfo, Redo, Goals).
detect_cse_in_disj([Var | Vars], Goals0, GoalInfo0, SM, InstMap,
		CseInfo0, CseInfo, Redo, Goal) :-
	(
		instmap__lookup_var(InstMap, Var, VarInst0),
		CseInfo0 = cse_info(_, _, ModuleInfo),
		% XXX we only need inst_is_bound, but leave this as it is
		% until mode analysis can handle aliasing between free
		% variables.
		inst_is_ground_or_any(ModuleInfo, VarInst0),
		common_deconstruct(Goals0, Var, CseInfo0, CseInfo1,
			Unify, Goals)
	->
		CseInfo = CseInfo1,
		Goal = conj([Unify, disj(Goals, SM) - GoalInfo0]),
		Redo = yes
	;
		detect_cse_in_disj(Vars, Goals0, GoalInfo0, SM, InstMap,
			CseInfo0, CseInfo, Redo, Goal)
	).

:- pred detect_cse_in_disj_2(list(hlds_goal), instmap, cse_info, cse_info,
	bool, list(hlds_goal)).
:- mode detect_cse_in_disj_2(in, in, in, out, out, out) is det.

detect_cse_in_disj_2([], _InstMap, CseInfo, CseInfo, no, []).
detect_cse_in_disj_2([Goal0 | Goals0], InstMap0, CseInfo0, CseInfo, Redo,
		[Goal | Goals]) :-
	detect_cse_in_goal(Goal0, InstMap0, CseInfo0, CseInfo1, Redo1, Goal),
	detect_cse_in_disj_2(Goals0, InstMap0, CseInfo1, CseInfo, Redo2, Goals),
	bool__or(Redo1, Redo2, Redo).

:- pred detect_cse_in_cases(list(var), var, can_fail, list(case),
	hlds_goal_info, store_map, instmap, cse_info, cse_info, bool,
	hlds_goal_expr).
:- mode detect_cse_in_cases(in, in, in, in, in, in, in, in, out, out, out)
	is det.

detect_cse_in_cases([], SwitchVar, CanFail, Cases0, _GoalInfo, SM, InstMap,
		CseInfo0, CseInfo, Redo,
		switch(SwitchVar, CanFail, Cases, SM)) :-
	detect_cse_in_cases_2(Cases0, InstMap, CseInfo0, CseInfo, Redo, Cases).
detect_cse_in_cases([Var | Vars], SwitchVar, CanFail, Cases0, GoalInfo,
		SM, InstMap, CseInfo0, CseInfo, Redo, Goal) :-
	(
		Var \= SwitchVar,
		instmap__lookup_var(InstMap, Var, VarInst0),
		CseInfo0 = cse_info(_, _, ModuleInfo),
		% XXX we only need inst_is_bound, but leave this as it is
		% until mode analysis can handle aliasing between free
		% variables.
		inst_is_ground_or_any(ModuleInfo, VarInst0),
		common_deconstruct_cases(Cases0, Var, CseInfo0, CseInfo1,
			Unify, Cases)
	->
		CseInfo = CseInfo1,
		Goal = conj([Unify, switch(SwitchVar, CanFail, Cases, SM)
			- GoalInfo]),
		Redo = yes
	;
		detect_cse_in_cases(Vars, SwitchVar, CanFail, Cases0, GoalInfo,
			SM, InstMap, CseInfo0, CseInfo, Redo, Goal)
	).

:- pred detect_cse_in_cases_2(list(case), instmap, cse_info, cse_info,
	bool, list(case)).
:- mode detect_cse_in_cases_2(in, in, in, out, out, out) is det.

detect_cse_in_cases_2([], _, CseInfo, CseInfo, no, []).
detect_cse_in_cases_2([Case0 | Cases0], InstMap, CseInfo0, CseInfo, Redo,
		[Case | Cases]) :-
	Case0 = case(Functor, Goal0),
	detect_cse_in_goal(Goal0, InstMap, CseInfo0, CseInfo1, Redo1, Goal),
	Case = case(Functor, Goal),
	detect_cse_in_cases_2(Cases0, InstMap, CseInfo1, CseInfo, Redo2, Cases),
	bool__or(Redo1, Redo2, Redo).

:- pred detect_cse_in_ite(list(var), list(var),
	hlds_goal, hlds_goal, hlds_goal, hlds_goal_info,
	store_map, instmap, cse_info, cse_info, bool, hlds_goal_expr).
:- mode detect_cse_in_ite(in, in, in, in, in, in, in, in, in, out, out, out)
	is det.

detect_cse_in_ite([], IfVars, Cond0, Then0, Else0, _, SM, InstMap, CseInfo0,
		CseInfo, Redo, if_then_else(IfVars, Cond, Then, Else, SM)) :-
	detect_cse_in_ite_2(Cond0, Then0, Else0,
		InstMap, CseInfo0, CseInfo, Redo, Cond, Then, Else).
detect_cse_in_ite([Var | Vars], IfVars, Cond0, Then0, Else0, GoalInfo,
		SM, InstMap, CseInfo0, CseInfo, Redo, Goal) :-
	(
		CseInfo0 = cse_info(_, _, ModuleInfo),
		instmap__lookup_var(InstMap, Var, VarInst0),
		% XXX we only need inst_is_bound, but leave this as it is
		% until mode analysis can handle aliasing between free
		% variables.
		inst_is_ground_or_any(ModuleInfo, VarInst0),
		common_deconstruct([Then0, Else0], Var, CseInfo0, CseInfo1,
			Unify, Goals),
		Goals = [Then, Else]
	->
		CseInfo = CseInfo1,
		Goal = conj([Unify, if_then_else(IfVars, Cond0, Then, Else, SM)
			- GoalInfo]),
		Redo = yes
	;
		detect_cse_in_ite(Vars, IfVars, Cond0, Then0, Else0, GoalInfo,
			SM, InstMap, CseInfo0, CseInfo, Redo, Goal)
	).

:- pred detect_cse_in_ite_2(hlds_goal, hlds_goal, hlds_goal,
	instmap, cse_info, cse_info, bool, hlds_goal, hlds_goal, hlds_goal).
:- mode detect_cse_in_ite_2(in, in, in, in, in, out, out, out, out, out) is det.

detect_cse_in_ite_2(Cond0, Then0, Else0, InstMap0, CseInfo0, CseInfo, Redo,
		Cond, Then, Else) :-
	detect_cse_in_goal_1(Cond0, InstMap0, CseInfo0, CseInfo1, Redo1, Cond,
		InstMap1),
	detect_cse_in_goal(Then0, InstMap1, CseInfo1, CseInfo2, Redo2, Then),
	detect_cse_in_goal(Else0, InstMap0, CseInfo2, CseInfo, Redo3, Else),
	bool__or(Redo1, Redo2, Redo12),
	bool__or(Redo12, Redo3, Redo).

%-----------------------------------------------------------------------------%

% common_deconstruct(Goals0, Var, CseInfo0, CseInfo, Unify, Goals):
% input vars:
%	Goals0 is a list of parallel goals in a branched structure
%	(disjunction, if-then-else, or switch).
%	Var is the variable we are looking for a common deconstruction on.
%	CseInfo0 contains the original varset and type map.
% output vars:
%	CseInfo has a varset and a type map reflecting the new variables
%	we have introduced.
%	Goals is the modified version of Goals0 after the common deconstruction
%	has been hoisted out, with the new variables as the functor arguments.
%	Unify is the unification that was hoisted out.

:- pred common_deconstruct(list(hlds_goal), var, cse_info, cse_info,
	hlds_goal, list(hlds_goal)).
:- mode common_deconstruct(in, in, in, out, out, out) is semidet.

common_deconstruct(Goals0, Var, CseInfo0, CseInfo, Unify, Goals) :-
	common_deconstruct_2(Goals0, Var, no, CseInfo0, CseInfo,
		Goals, MaybeUnifyGoal),
	MaybeUnifyGoal = yes(Unify).

:- pred common_deconstruct_2(list(hlds_goal), var, maybe(hlds_goal),
	cse_info, cse_info, list(hlds_goal), maybe(hlds_goal)).
:- mode common_deconstruct_2(in, in, in, in, out, out, out) is semidet.

common_deconstruct_2([], _Var, MaybeUnify, CseInfo, CseInfo, [], MaybeUnify).
common_deconstruct_2([Goal0 | Goals0], Var, MaybeUnify0,
		CseInfo0, CseInfo, [Goal | Goals], MaybeUnify) :-
	goal_to_conj_list(Goal0, ConjList0),
	Goal0 = _ - GoalInfo,
	map__init(Substitution),
	find_bind_var_for_cse(ConjList0, Substitution, Var, MaybeUnify0,
		CseInfo0, CseInfo1, ConjList, _NewSubstitution, MaybeUnify1),
	MaybeUnify1 = yes(_),
	conj_list_to_goal(ConjList, GoalInfo, Goal),
	common_deconstruct_2(Goals0, Var, MaybeUnify1, CseInfo1, CseInfo,
		Goals, MaybeUnify).

%-----------------------------------------------------------------------------%

:- pred common_deconstruct_cases(list(case), var, cse_info, cse_info,
	hlds_goal, list(case)).
:- mode common_deconstruct_cases(in, in, in, out, out, out) is semidet.

common_deconstruct_cases(Cases0, Var, CseInfo0, CseInfo,
		Unify, Cases) :-
	common_deconstruct_cases_2(Cases0, Var, no, CseInfo0, CseInfo,
		Cases, MaybeUnifyGoal),
	MaybeUnifyGoal = yes(Unify).

:- pred common_deconstruct_cases_2(list(case), var, maybe(hlds_goal),
	cse_info, cse_info, list(case), maybe(hlds_goal)).
:- mode common_deconstruct_cases_2(in, in, in, in, out, out, out) is semidet.

common_deconstruct_cases_2([], _Var, MaybeUnify, CseInfo, CseInfo,
	[], MaybeUnify).
common_deconstruct_cases_2([case(ConsId, Goal0) | Cases0], Var, MaybeUnify0,
		CseInfo0, CseInfo, [case(ConsId, Goal) | Cases], MaybeUnify) :-
	goal_to_conj_list(Goal0, ConjList0),
	Goal0 = _ - GoalInfo,
	map__init(Substitution),
	find_bind_var_for_cse(ConjList0, Substitution, Var, MaybeUnify0,
		CseInfo0, CseInfo1, ConjList, _NewSubstitution, MaybeUnify1),
	MaybeUnify1 = yes(_),
	conj_list_to_goal(ConjList, GoalInfo, Goal),
	common_deconstruct_cases_2(Cases0, Var, MaybeUnify1, CseInfo1, CseInfo,
		Cases, MaybeUnify).

%-----------------------------------------------------------------------------%

	%	Searches through Goals0 looking for a deconstruction
	%	unification with `Var'.
	%
	%	If MaybeUnify0 is no, a unification with any functor
	%	is acceptable; if it is yes(Unify), only a unification
	%	involving the same variable and function symbol is OK.
	%
	%	If we do find an acceptable deconstruction, we replace it
	%	in the goal with pairwise equalities between the arguments
	%	of the functor in that unification and the arguments of the
	%	functor in Unify, where in Maybeunify = yes(Unify).
	%	If MaybeUnify0 was no, we have to create the variables in Unify.
	%
	%	If we do not find an acceptable deconstruction, we set
	%	MaybeUnify to no and set `Subst' to the substitution resulting
	%	from interpreting through the goal.

:- pred find_bind_var_for_cse(list(hlds_goal), substitution, var,
	maybe(hlds_goal), cse_info, cse_info, list(hlds_goal), 
	substitution, maybe(hlds_goal)).
:- mode find_bind_var_for_cse(in, in, in, in, in, out, out, out, out) is det.

find_bind_var_for_cse([], Substitution, _Var, _MaybeUnify0, CseInfo, CseInfo,
	[], Substitution, no).
find_bind_var_for_cse([GoalPair0 | Goals0], Substitution0, Var, MaybeUnify0,
		CseInfo0, CseInfo, Goals, Substitution, MaybeUnify) :-
	GoalPair0 = Goal0 - GoalInfo,
	( Goal0 = conj(SubGoals0) ->
		find_bind_var_for_cse(SubGoals0, Substitution0, Var,
			MaybeUnify0, CseInfo0, CseInfo1,
			SubGoals, Substitution1, MaybeUnify1),
		Goal = conj(SubGoals),
		( MaybeUnify1 = yes(_) ->
			Goals = [Goal - GoalInfo | Goals0],
			Substitution = Substitution1,
			MaybeUnify = MaybeUnify1,
			CseInfo = CseInfo1
		;
			find_bind_var_for_cse(Goals0, Substitution1, Var,
				MaybeUnify0, CseInfo1, CseInfo,
				Goals1, Substitution, MaybeUnify),
			Goals = [Goal0 - GoalInfo | Goals1]
		)
	; Goal0 = unify(A, B, _, UnifyInfo0, _) ->
		term__apply_rec_substitution(term__variable(Var),
			Substitution0, Term),
		(
			Term = term__variable(Var1),
			UnifyInfo0 = deconstruct(UnifyVar, _, _, _, _),
			term__apply_rec_substitution(term__variable(UnifyVar),
				Substitution0, term__variable(UnifyVar1)),
			Var1 = UnifyVar1,
			MaybeUnify0 = no
		->
			CseInfo0 = cse_info(Varset0, Typemap0, ModuleInfo),
			construct_common_unify(Var, Goal0 - GoalInfo, Goal,
				Varset0, Varset, Typemap0, Typemap,
				Replacements),
			CseInfo = cse_info(Varset, Typemap, ModuleInfo),
			MaybeUnify = yes(Goal),
			list__append(Replacements, Goals0, Goals),
			Substitution = Substitution0
		;
			Term = term__variable(Var1),
			UnifyInfo0 = deconstruct(UnifyVar, _, _, _, _),
			term__apply_rec_substitution(term__variable(UnifyVar),
				Substitution0, term__variable(UnifyVar1)),
			Var1 = UnifyVar1,
			UnifyInfo0 = deconstruct(_, _, _, _, _),
			MaybeUnify0 = yes(OldUnifyGoal),
			goal_info_get_context(GoalInfo, Context),
			find_similar_deconstruct(OldUnifyGoal, UnifyInfo0,
				Context, Replacements)
		->
			list__append(Replacements, Goals0, Goals),
			Substitution = Substitution0,
			CseInfo = CseInfo0,
			MaybeUnify = MaybeUnify0
		;
		%
		% if the variable was bound, but the deconstruction wasn't
		% similar, then stop searching
		%
			Term = term__functor(_, _, _)
		->
			Goals = [Goal0 - GoalInfo | Goals0],
			Substitution = Substitution0,
			CseInfo = CseInfo0,
			MaybeUnify = no
		;
			( interpret_unify(A, B, Substitution0, Substitution1) ->
				Substitution2 = Substitution1
			;
				% the unification must fail - just ignore it
				Substitution2 = Substitution0
			),
			find_bind_var_for_cse(Goals0, Substitution2, Var,
				MaybeUnify0, CseInfo0, CseInfo,
				Goals1, Substitution, MaybeUnify),
			Goals = [Goal0 - GoalInfo | Goals1]
		)
	;
		Goals = [Goal0 - GoalInfo | Goals0],
		Substitution = Substitution0,
		CseInfo = CseInfo0,
		MaybeUnify = no
	).

:- pred construct_common_unify(var, hlds_goal, hlds_goal, varset, varset,
	map(var, type), map(var, type), list(hlds_goal)).
:- mode construct_common_unify(in, in, out, in, out, in, out, out) is det.

construct_common_unify(Var, GoalExpr0 - GoalInfo, Goal, Varset0, Varset,
		Typemap0, Typemap, Replacements) :-
	(
		GoalExpr0 = unify(_, Term, Umode, Unif0, Ucontext),
		Unif0 = deconstruct(_, Consid, Args, Submodes, CanFail)
	->
		Unif = deconstruct(Var, Consid, Args, Submodes, CanFail),
		( Term = functor(_, _) ->
			GoalExpr1 = unify(Var, Term, Umode, Unif, Ucontext)
		;
			error("unexpected unify structure in construct_common_unify")
		),
		goal_info_get_context(GoalInfo, Context),
		create_parallel_subterms(Args, Context, Ucontext,
			Varset0, Varset, Typemap0, Typemap, Sub, Replacements),
		goal_util__rename_vars_in_goal(GoalExpr1 - GoalInfo, Sub, Goal)
	;
		error("unexpected goal in construct_common_unify")
	).

:- pred create_parallel_subterms(list(var), term__context, unify_context,
	varset, varset, map(var, type), map(var, type), map(var, var),
	list(hlds_goal)).
:- mode create_parallel_subterms(in, in, in, in, out, in, out, out, out) is det.

create_parallel_subterms([], _, _, Varset, Varset, Typemap, Typemap, Sub, []) :-
	map__init(Sub).
create_parallel_subterms([OFV | OFV0], Context, UnifyContext, Varset0, Varset,
		Typemap0, Typemap, Sub, Replacements) :-
	create_parallel_subterms(OFV0, Context, UnifyContext, Varset0, Varset1,
		Typemap0, Typemap1, Sub1, Replacements1),
	varset__new_var(Varset1, NFV, Varset),
	map__lookup(Typemap1, OFV, Type),
	map__det_insert(Typemap1, NFV, Type, Typemap),
	map__det_insert(Sub1, OFV, NFV, Sub),
	UnifyContext = unify_context(MainCtxt, SubCtxt),
	create_atomic_unification(OFV, var(NFV),
		Context, MainCtxt, SubCtxt, Goal),
	Replacements = [Goal | Replacements1].

%-----------------------------------------------------------------------------%

:- pred find_similar_deconstruct(hlds_goal, unification, term__context,
	list(hlds_goal)).
:- mode find_similar_deconstruct(in, in, in, out) is semidet.

find_similar_deconstruct(OldUnifyGoal, NewUnifyInfo, Context, Replacements) :-
	(
		OldUnifyGoal = unify(_OT1, _OT2, _OM, OldUnifyInfo, OC) - _,
		OldUnifyInfo = deconstruct(_OV, OF, OFV, _OUM, _OCF),
		NewUnifyInfo = deconstruct(_NV, NF, NFV, _NUM, _NCF)
	->
		OF = NF,
		list__length(OFV, OFVC),
		list__length(NFV, NFVC),
		OFVC = NFVC,
		pair_subterms(OFV, NFV, Context, OC, Replacements)
	;
		error("find_similar_deconstruct: non-deconstruct unify")
	).

:- pred pair_subterms(list(var), list(var), term__context, unify_context,
	list(hlds_goal)).
:- mode pair_subterms(in, in, in, in, out) is det.

pair_subterms(OFV0, NFV0, Context, UnifyContext, Replacements) :-
	(
		OFV0 = [OFV | OFV1],
		NFV0 = [NFV | NFV1]
	->
		pair_subterms(OFV1, NFV1, Context, UnifyContext, Replacements1),
		( OFV = NFV ->
			Replacements = Replacements1
		;
			UnifyContext = unify_context(MainCtxt, SubCtxt),
			create_atomic_unification(OFV, var(NFV),
				Context, MainCtxt, SubCtxt, Goal),
			Replacements = [Goal | Replacements1]
		)
	;
		OFV0 = [],
		NFV0 = []
	->
		Replacements = []
	;
		error("mismatched length lists in pair_subterms")
	).

%-----------------------------------------------------------------------------%
