%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2000 The University of Melbourne.
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

:- pred detect_cse(module_info::in, module_info::out,
	io__state::di, io__state::uo) is det.

:- pred detect_cse_in_proc(proc_id::in, pred_id::in,
	module_info::in, module_info::out, io__state::di, io__state::uo)
	is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, hlds_data, options, globals, goal_util, hlds_out.
:- import_module modes, mode_util, quantification, instmap.
:- import_module prog_data, switch_detection, det_util, inst_match.
:- import_module switch_detection, term, varset.

:- import_module int, bool, list, map, set, std_util, require.

%-----------------------------------------------------------------------------%

	% Traverse the module structure, calling `detect_cse_in_goal'
	% for each procedure body.

detect_cse(ModuleInfo0, ModuleInfo) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	detect_cse_in_preds(PredIds, ModuleInfo0, ModuleInfo).

:- pred detect_cse_in_preds(list(pred_id)::in,
	module_info::in, module_info::out, io__state::di, io__state::uo)
	is det.

detect_cse_in_preds([], ModuleInfo, ModuleInfo) --> [].
detect_cse_in_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	detect_cse_in_pred(PredId, PredInfo, ModuleInfo0, ModuleInfo1),
	detect_cse_in_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred detect_cse_in_pred(pred_id::in, pred_info::in,
	module_info::in, module_info::out, io__state::di, io__state::uo)
	is det.

detect_cse_in_pred(PredId, PredInfo0, ModuleInfo0, ModuleInfo) -->
	{ pred_info_non_imported_procids(PredInfo0, ProcIds) },
	detect_cse_in_procs(ProcIds, PredId, ModuleInfo0, ModuleInfo).

:- pred detect_cse_in_procs(list(proc_id)::in, pred_id::in,
	module_info::in, module_info::out, io__state::di, io__state::uo)
	is det.

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
		modecheck_proc(ProcId, PredId, ModuleInfo1,
				ModuleInfo2, Errs, _Changed),
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
			hlds_out__write_pred_id(ModuleInfo3, PredId),
			io__write_string("\n")
		;
			[]
		),
		detect_cse_in_proc(ProcId, PredId, ModuleInfo3, ModuleInfo)
	).

:- type cse_info
	--->	cse_info(
			varset			:: prog_varset,
			vartypes		:: vartypes,
			type_info_varmap	:: type_info_varmap,
			typeclass_info_varmap	:: typeclass_info_varmap,
			module_info		:: module_info
		).

:- pred detect_cse_in_proc_2(proc_id::in, pred_id::in, bool::out,
	module_info::in, module_info::out) is det.

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
	proc_info_varset(ProcInfo0, Varset0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_typeinfo_varmap(ProcInfo0, TypeInfoVarMap0),
	proc_info_typeclass_info_varmap(ProcInfo0, TypeClassInfoVarMap0),
	CseInfo0 = cse_info(Varset0, VarTypes0,
		TypeInfoVarMap0, TypeClassInfoVarMap0, ModuleInfo0),
	detect_cse_in_goal(Goal0, InstMap0, CseInfo0, CseInfo, Redo, Goal1),

	(
		Redo = no,
		ModuleInfo = ModuleInfo0
	;
		Redo = yes,

		% ModuleInfo should not be changed by detect_cse_in_goal
		CseInfo = cse_info(VarSet1, VarTypes1,
			TypeInfoVarMap, TypeClassInfoVarMap, _),
		proc_info_headvars(ProcInfo0, HeadVars),

		implicitly_quantify_clause_body(HeadVars, Goal1, VarSet1,
			VarTypes1, Goal, VarSet, VarTypes, _Warnings),

		proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
		proc_info_set_varset(ProcInfo1, VarSet, ProcInfo2),
		proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo3),
		proc_info_set_typeinfo_varmap(ProcInfo3,
			TypeInfoVarMap, ProcInfo4),
		proc_info_set_typeclass_info_varmap(ProcInfo4,
			TypeClassInfoVarMap, ProcInfo),

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

:- pred detect_cse_in_goal(hlds_goal::in, instmap::in, cse_info::in,
	cse_info::out, bool::out, hlds_goal::out) is det.

detect_cse_in_goal(Goal0, InstMap0, CseInfo0, CseInfo, Redo, Goal) :-
	detect_cse_in_goal_1(Goal0, InstMap0, CseInfo0, CseInfo,
		Redo, Goal, _InstMap).

	% This version is the same as the above except that it returns
	% the resulting instmap on exit from the goal, which is
	% computed by applying the instmap delta specified in the
	% goal's goalinfo.

:- pred detect_cse_in_goal_1(hlds_goal::in, instmap::in, cse_info::in,
	cse_info::out, bool::out, hlds_goal::out, instmap::out) is det.

detect_cse_in_goal_1(Goal0 - GoalInfo, InstMap0, CseInfo0, CseInfo, Redo,
		Goal - GoalInfo, InstMap) :-
	detect_cse_in_goal_2(Goal0, GoalInfo, InstMap0, CseInfo0, CseInfo,
		Redo, Goal),
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap).

	% Here we process each of the different sorts of goals.

:- pred detect_cse_in_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
	instmap::in, cse_info::in, cse_info::out, bool::out,
	hlds_goal_expr::out) is det.

detect_cse_in_goal_2(pragma_foreign_code(A,B,C,D,E,F,G), _, _, CseInfo,
	CseInfo, no, pragma_foreign_code(A,B,C,D,E,F,G)).

detect_cse_in_goal_2(generic_call(A,B,C,D), _, _, CseInfo, CseInfo,
	no, generic_call(A,B,C,D)).

detect_cse_in_goal_2(call(A,B,C,D,E,F), _, _, CseInfo, CseInfo, no,
	call(A,B,C,D,E,F)).

detect_cse_in_goal_2(unify(A,B0,C,D,E), _, InstMap0, CseInfo0, CseInfo, Redo,
		unify(A,B,C,D,E)) :-
	(
		B0 = lambda_goal(PredOrFunc, EvalMethod, FixModes,
			NonLocalVars, Vars, Modes, Det, Goal0)
	->
		ModuleInfo = CseInfo0 ^ module_info,
		instmap__pre_lambda_update(ModuleInfo,
			Vars, Modes, InstMap0, InstMap),
		detect_cse_in_goal(Goal0, InstMap, CseInfo0, CseInfo, Redo,
			Goal),
		B = lambda_goal(PredOrFunc, EvalMethod, FixModes,
			NonLocalVars, Vars, Modes, Det, Goal)
	;
		B = B0,
		CseInfo = CseInfo0,
		Redo = no
	).

detect_cse_in_goal_2(not(Goal0), _GoalInfo, InstMap, CseInfo0, CseInfo,
		Redo, not(Goal)) :-
	detect_cse_in_goal(Goal0, InstMap, CseInfo0, CseInfo, Redo, Goal).

detect_cse_in_goal_2(some(Vars, CanRemove, Goal0), _GoalInfo, InstMap,
		CseInfo0, CseInfo, Redo, some(Vars, CanRemove, Goal)) :-
	detect_cse_in_goal(Goal0, InstMap, CseInfo0, CseInfo, Redo, Goal).

detect_cse_in_goal_2(conj(Goals0), _GoalInfo, InstMap, CseInfo0, CseInfo,
		Redo, conj(Goals)) :-
	detect_cse_in_conj(Goals0, InstMap, CseInfo0, CseInfo, Redo, Goals).

detect_cse_in_goal_2(par_conj(Goals0, SM), _, InstMap, CseInfo0, CseInfo, Redo,
		par_conj(Goals, SM)) :-
	detect_cse_in_par_conj(Goals0, InstMap, CseInfo0, CseInfo,
		Redo, Goals).

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

detect_cse_in_goal_2(bi_implication(_, _), _, _, _, _, _, _) :-
	% these should have been expanded out by now
	error("detect_cse_in_goal_2: unexpected bi_implication").

%-----------------------------------------------------------------------------%

:- pred detect_cse_in_conj(list(hlds_goal)::in, instmap::in, cse_info::in,
	cse_info::out, bool::out, list(hlds_goal)::out) is det.

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

:- pred detect_cse_in_par_conj(list(hlds_goal)::in, instmap::in, cse_info::in,
	cse_info::out, bool::out, list(hlds_goal)::out) is det.

detect_cse_in_par_conj([], _InstMap, CseInfo, CseInfo, no, []).
detect_cse_in_par_conj([Goal0 | Goals0], InstMap0, CseInfo0, CseInfo,
		Redo, [Goal | Goals]) :-
	detect_cse_in_goal(Goal0, InstMap0, CseInfo0, CseInfo1, Redo1, Goal),
	detect_cse_in_par_conj(Goals0, InstMap0, CseInfo1, CseInfo,
		Redo2, Goals),
	bool__or(Redo1, Redo2, Redo).

%-----------------------------------------------------------------------------%

	% These are the interesting bits - we've found a non-empty branched
	% structure, and we've got a list of the non-local variables of that
	% structure. Now for each non-local variable, we check whether each
	% branch matches that variable against the same functor.

:- pred detect_cse_in_disj(list(prog_var)::in, list(hlds_goal)::in,
	hlds_goal_info::in, store_map::in, instmap::in, cse_info::in,
	cse_info::out, bool::out, hlds_goal_expr::out) is det.

detect_cse_in_disj([], Goals0, _, SM, InstMap, CseInfo0, CseInfo,
		Redo, disj(Goals, SM)) :-
	detect_cse_in_disj_2(Goals0, InstMap, CseInfo0, CseInfo, Redo, Goals).
detect_cse_in_disj([Var | Vars], Goals0, GoalInfo0, SM, InstMap,
		CseInfo0, CseInfo, Redo, Goal) :-
	(
		instmap__lookup_var(InstMap, Var, VarInst0),
		ModuleInfo = CseInfo0 ^ module_info,
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

:- pred detect_cse_in_disj_2(list(hlds_goal)::in, instmap::in, cse_info::in,
	cse_info::out, bool::out, list(hlds_goal)::out) is det.

detect_cse_in_disj_2([], _InstMap, CseInfo, CseInfo, no, []).
detect_cse_in_disj_2([Goal0 | Goals0], InstMap0, CseInfo0, CseInfo, Redo,
		[Goal | Goals]) :-
	detect_cse_in_goal(Goal0, InstMap0, CseInfo0, CseInfo1, Redo1, Goal),
	detect_cse_in_disj_2(Goals0, InstMap0, CseInfo1, CseInfo,
		Redo2, Goals),
	bool__or(Redo1, Redo2, Redo).

:- pred detect_cse_in_cases(list(prog_var)::in, prog_var::in, can_fail::in,
	list(case)::in, hlds_goal_info::in, store_map::in, instmap::in,
	cse_info::in, cse_info::out, bool::out, hlds_goal_expr::out) is det.

detect_cse_in_cases([], SwitchVar, CanFail, Cases0, _GoalInfo, SM, InstMap,
		CseInfo0, CseInfo, Redo,
		switch(SwitchVar, CanFail, Cases, SM)) :-
	detect_cse_in_cases_2(Cases0, InstMap, CseInfo0, CseInfo, Redo, Cases).
detect_cse_in_cases([Var | Vars], SwitchVar, CanFail, Cases0, GoalInfo,
		SM, InstMap, CseInfo0, CseInfo, Redo, Goal) :-
	(
		Var \= SwitchVar,
		instmap__lookup_var(InstMap, Var, VarInst0),
		ModuleInfo = CseInfo0 ^ module_info,
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

:- pred detect_cse_in_cases_2(list(case)::in, instmap::in, cse_info::in,
	cse_info::out, bool::out, list(case)::out) is det.

detect_cse_in_cases_2([], _, CseInfo, CseInfo, no, []).
detect_cse_in_cases_2([Case0 | Cases0], InstMap, CseInfo0, CseInfo, Redo,
		[Case | Cases]) :-
	Case0 = case(Functor, Goal0),
	detect_cse_in_goal(Goal0, InstMap, CseInfo0, CseInfo1, Redo1, Goal),
	Case = case(Functor, Goal),
	detect_cse_in_cases_2(Cases0, InstMap, CseInfo1, CseInfo,
		Redo2, Cases),
	bool__or(Redo1, Redo2, Redo).

:- pred detect_cse_in_ite(list(prog_var)::in, list(prog_var)::in,
	hlds_goal::in, hlds_goal::in, hlds_goal::in, hlds_goal_info::in,
	store_map::in, instmap::in, cse_info::in,
	cse_info::out, bool::out, hlds_goal_expr::out) is det.

detect_cse_in_ite([], IfVars, Cond0, Then0, Else0, _, SM, InstMap, CseInfo0,
		CseInfo, Redo, if_then_else(IfVars, Cond, Then, Else, SM)) :-
	detect_cse_in_ite_2(Cond0, Then0, Else0,
		InstMap, CseInfo0, CseInfo, Redo, Cond, Then, Else).
detect_cse_in_ite([Var | Vars], IfVars, Cond0, Then0, Else0, GoalInfo,
		SM, InstMap, CseInfo0, CseInfo, Redo, Goal) :-
	(
		ModuleInfo = CseInfo0 ^ module_info,
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

:- pred detect_cse_in_ite_2(hlds_goal::in, hlds_goal::in, hlds_goal::in,
	instmap::in, cse_info::in, cse_info::out, bool::out,
	hlds_goal::out, hlds_goal::out, hlds_goal::out) is det.

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

:- pred common_deconstruct(list(hlds_goal)::in, prog_var::in, cse_info::in,
	cse_info::out, hlds_goal::out, list(hlds_goal)::out) is semidet.

common_deconstruct(Goals0, Var, CseInfo0, CseInfo, Unify, Goals) :-
	common_deconstruct_2(Goals0, Var, before_candidate,
		have_candidate(Unify, yes), CseInfo0, CseInfo, Goals).

:- pred common_deconstruct_2(list(hlds_goal)::in, prog_var::in,
	cse_state::in, cse_state::out, cse_info::in, cse_info::out,
	list(hlds_goal)::out) is semidet.

common_deconstruct_2([], _Var, CseState, CseState, CseInfo, CseInfo, []).
common_deconstruct_2([Goal0 | Goals0], Var, CseState0, CseState,
		CseInfo0, CseInfo, [Goal | Goals]) :-
	find_bind_var(Var, find_bind_var_for_cse_in_deconstruct, Goal0, Goal,
		CseState0, CseState1, CseInfo0, CseInfo1),
	CseState1 = have_candidate(_, _),
	common_deconstruct_2(Goals0, Var, CseState1, CseState,
		CseInfo1, CseInfo, Goals).

%-----------------------------------------------------------------------------%

:- pred common_deconstruct_cases(list(case)::in, prog_var::in, cse_info::in,
	cse_info::out, hlds_goal::out, list(case)::out) is semidet.

common_deconstruct_cases(Cases0, Var, CseInfo0, CseInfo,
		Unify, Cases) :-
	common_deconstruct_cases_2(Cases0, Var, before_candidate,
		have_candidate(Unify, yes), CseInfo0, CseInfo, Cases).

:- pred common_deconstruct_cases_2(list(case)::in, prog_var::in,
	cse_state::in, cse_state::out, cse_info::in, cse_info::out,
	list(case)::out) is semidet.

common_deconstruct_cases_2([], _Var, CseState, CseState, CseInfo, CseInfo, []).
common_deconstruct_cases_2([case(ConsId, Goal0) | Cases0], Var,
		CseState0, CseState, CseInfo0, CseInfo,
		[case(ConsId, Goal) | Cases]) :-
	find_bind_var(Var, find_bind_var_for_cse_in_deconstruct, Goal0, Goal,
		CseState0, CseState1, CseInfo0, CseInfo1),
	CseState1 = have_candidate(_, _),
	common_deconstruct_cases_2(Cases0, Var, CseState1, CseState,
		CseInfo1, CseInfo, Cases).

%-----------------------------------------------------------------------------%

	% The hlds_goal is the common unification we are attemping to hoist.
	% The boolean states whether such a deconstruction has been seen in
	% another branch.
:- type cse_state
	--->	before_candidate
	;	have_candidate(
			goal		::	hlds_goal,
			seen_similar	::	bool
		)
	;	multiple_candidates.

:- pred find_bind_var_for_cse_in_deconstruct(prog_var::in, hlds_goal::in,
	list(hlds_goal)::out, cse_state::in, cse_state::out,
	cse_info::in, cse_info::out) is det.

find_bind_var_for_cse_in_deconstruct(Var, Goal0, Goals,
		CseState0, CseState, CseInfo0, CseInfo) :-
	(
		CseState0 = before_candidate,
		construct_common_unify(Var, Goal0, Goal, CseInfo0, CseInfo,
			Goals),
		CseState = have_candidate(Goal, no)
	;
		CseState0 = have_candidate(OldUnifyGoal, _),
		CseInfo = CseInfo0,
		Goal0 = _ - GoalInfo,
		goal_info_get_context(GoalInfo, Context),
		(
			find_similar_deconstruct(OldUnifyGoal,
				Goal0, Context, Goals0)
		->
			Goals = Goals0,
			CseState = have_candidate(OldUnifyGoal, yes)
		;
			Goals = [Goal0],
			CseState = multiple_candidates
		)
	;
		CseState0 = multiple_candidates,
		Goals = [Goal0],
		CseState = multiple_candidates,
		CseInfo = CseInfo0
	).

:- pred construct_common_unify(prog_var::in, hlds_goal::in, hlds_goal::out,
	cse_info::in, cse_info::out, list(hlds_goal)::out) is det.

construct_common_unify(Var, GoalExpr0 - GoalInfo, Goal, CseInfo0, CseInfo,
		Replacements) :-
	(
		GoalExpr0 = unify(_, Term, Umode, Unif0, Ucontext),
		Unif0 = deconstruct(_, Consid, Args, Submodes, CanFail, CanCGC)
	->
		Unif = deconstruct(Var, Consid, Args, Submodes, CanFail,
			CanCGC),
		( Term = functor(_, _) ->
			GoalExpr1 = unify(Var, Term, Umode, Unif, Ucontext)
		;
			error("non-functor unify in construct_common_unify")
		),
		goal_info_get_context(GoalInfo, Context),
		create_parallel_subterms(Args, Context, Ucontext,
			CseInfo0, CseInfo, Sub, Replacements),
		goal_util__rename_vars_in_goal(GoalExpr1 - GoalInfo, Sub, Goal)
	;
		error("non-unify goal in construct_common_unify")
	).

:- pred create_parallel_subterms(list(prog_var)::in, prog_context::in,
	unify_context::in, cse_info::in, cse_info::out,
	map(prog_var, prog_var)::out, list(hlds_goal)::out) is det.

create_parallel_subterms([], _, _, CseInfo, CseInfo, Sub, []) :-
	map__init(Sub).
create_parallel_subterms([OFV | OFV0], Context, UnifyContext,
		CseInfo0, CseInfo, Sub, Replacements) :-
	create_parallel_subterms(OFV0, Context, UnifyContext,
		CseInfo0, CseInfo1, Sub1, Replacements1),
	create_parallel_subterm(OFV, Context, UnifyContext,
		CseInfo1, CseInfo, Sub1, Sub, Goal),
	Replacements = [Goal | Replacements1].

:- pred create_parallel_subterm(prog_var::in, prog_context::in,
	unify_context::in, cse_info::in, cse_info::out,
	map(prog_var, prog_var)::in, map(prog_var, prog_var)::out,
	hlds_goal::out) is det.

create_parallel_subterm(OFV, Context, UnifyContext,
		CseInfo0, CseInfo, Sub0, Sub, Goal) :-
	VarSet0 = CseInfo0 ^ varset,
	VarTypes0 = CseInfo0 ^ vartypes,
	varset__new_var(VarSet0, NFV, VarSet),
	map__lookup(VarTypes0, OFV, Type),
	map__det_insert(VarTypes0, NFV, Type, VarTypes),
	map__det_insert(Sub0, OFV, NFV, Sub),
	UnifyContext = unify_context(MainCtxt, SubCtxt),
	create_atomic_unification(OFV, var(NFV),
		Context, MainCtxt, SubCtxt, Goal),
	CseInfo = (CseInfo0 ^ varset := VarSet) ^ vartypes := VarTypes.

%-----------------------------------------------------------------------------%

:- pred find_similar_deconstruct(hlds_goal::in, hlds_goal::in,
	prog_context::in, list(hlds_goal)::out) is semidet.

find_similar_deconstruct(OldUnifyGoal, NewUnifyGoal, Context, Replacements) :-
	(
		OldUnifyGoal = unify(_OT1, _OT2, _OM, OldUnifyInfo, OC) - _,
		OldUnifyInfo = deconstruct(_OV, OF, OFV, _OUM, _OCF, _OCGC),
		NewUnifyGoal = unify(_NT1, _NT2, _NM, NewUnifyInfo, _NC) - _,
		NewUnifyInfo = deconstruct(_NV, NF, NFV, _NUM, _NCF, _NCGC)
	->
		OF = NF,
		list__length(OFV, OFVC),
		list__length(NFV, NFVC),
		OFVC = NFVC,
		pair_subterms(OFV, NFV, Context, OC, Replacements)
	;
		error("find_similar_deconstruct: non-deconstruct unify")
	).

:- pred pair_subterms(list(prog_var)::in, list(prog_var)::in,
	prog_context::in, unify_context::in, list(hlds_goal)::out) is det.

pair_subterms(OFV0, NFV0, Context, UnifyContext, Replacements) :-
	(
		OFV0 = [OFV | OFV1],
		NFV0 = [NFV | NFV1]
	->
		pair_subterms(OFV1, NFV1, Context, UnifyContext,
			Replacements1),
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
