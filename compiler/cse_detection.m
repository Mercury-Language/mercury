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

:- import_module hlds, io.

:- pred detect_cse(module_info, module_info, io__state, io__state).
:- mode detect_cse(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list, map, set, std_util, require, term.
:- import_module options, globals.
:- import_module modes, mode_util, make_hlds, quantification, switch_detection.
:- import_module hlds_out.

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
	detect_cse_in_procs(ProcIds, PredId, no, Redo,
		ModuleInfo0, ModuleInfo1),
	(
		{ Redo = no },
		{ ModuleInfo = ModuleInfo1 }
	;
		{ Redo = yes },
		globals__io_lookup_bool_option(very_verbose, VeryVerbose),
		( { VeryVerbose = yes } ->
			io__write_string("% Repeating mode check for "),
			hlds_out__write_pred_id(ModuleInfo0, PredId),
			io__write_string("\n")
		;
			[]
		),
		{ module_info_preds(ModuleInfo1, PredTable1) },
		{ map__lookup(PredTable1, PredId, PredInfo1) },
		modecheck_pred_mode(PredId, PredInfo1, ModuleInfo1,
				ModuleInfo2, Errs),
		{ Errs > 0 ->
			error("mode check fails when repeated")
		;
			true
		},

		( { VeryVerbose = yes } ->
			io__write_string("% Repeating switch detection for "),
			hlds_out__write_pred_id(ModuleInfo0, PredId),
			io__write_string("\n")
		;
			[]
		),
		{ module_info_preds(ModuleInfo2, PredTable2) },
		{ map__lookup(PredTable2, PredId, PredInfo2) },
		detect_switches_in_pred(PredId, PredInfo2,
			ModuleInfo2, ModuleInfo3),

		( { VeryVerbose = yes } ->
			io__write_string("% Repeating common deconstruction detection for "),
			hlds_out__write_pred_id(ModuleInfo0, PredId),
			io__write_string("\n")
		;
			[]
		),
		{ module_info_preds(ModuleInfo3, PredTable3) },
		{ map__lookup(PredTable3, PredId, PredInfo3) },
		detect_cse_in_pred(PredId, PredInfo3, ModuleInfo3, ModuleInfo)
	).

:- pred detect_cse_in_procs(list(proc_id), pred_id, bool, bool,
	module_info, module_info, io__state, io__state).
% :- mode detect_cse_in_procs(in, in, in, out, di, uo, di, uo) is det.
:- mode detect_cse_in_procs(in, in, in, out, in, out, di, uo) is det.

detect_cse_in_procs([], _PredId, Redo, Redo, ModuleInfo, ModuleInfo,
	IOstate, IOstate).
detect_cse_in_procs([ProcId | ProcIds], PredId, Redo0, Redo,
		ModuleInfo0, ModuleInfo, IOstate0, IOstate) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

		% To process each ProcInfo, we get the goal,
		% initialize the instmap based on the modes of the head vars,
		% and pass these to `detect_cse_in_goal'.

	proc_info_goal(ProcInfo0, Goal0),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0),
	detect_cse_in_goal(Goal0, InstMap0, ModuleInfo0, Redo1, Goal1),

	(
		Redo1 = no,
		ModuleInfo1 = ModuleInfo0,
		IOstate1 = IOstate0
	;
		Redo1 = yes,
		proc_info_headvars(ProcInfo0, HeadVars),
		implicitly_quantify_clause_body(HeadVars, Goal1, Goal),

		proc_info_set_goal(ProcInfo0, Goal, ProcInfo),
		map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
		pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
		map__set(PredTable0, PredId, PredInfo, PredTable),
		module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),
		IOstate1 = IOstate0
	),

	bool__or(Redo0, Redo1, Redo2),
	detect_cse_in_procs(ProcIds, PredId, Redo2, Redo,
		ModuleInfo1, ModuleInfo, IOstate1, IOstate).

%-----------------------------------------------------------------------------%

	% Given a goal, and the instmap on entry to that goal,
	% find disjunctions that contain common subexpressions
	% and hoist these out of the disjunction. At the moment
	% we only look for cses that are deconstruction unifications.

:- pred detect_cse_in_goal(hlds__goal, instmap, module_info, bool, hlds__goal).
:- mode detect_cse_in_goal(in, in, in, out, out) is det.

detect_cse_in_goal(Goal0, InstMap0, ModuleInfo, Redo, Goal) :-
	detect_cse_in_goal_1(Goal0, InstMap0, ModuleInfo, Redo, Goal, _InstMap).

	% This version is the same as the above except that it returns
	% the resulting instmap on exit from the goal, which is
	% computed by applying the instmap delta specified in the
	% goal's goalinfo.

:- pred detect_cse_in_goal_1(hlds__goal, instmap, module_info, bool,
	hlds__goal, instmap).
:- mode detect_cse_in_goal_1(in, in, in, out, out, out) is det.

detect_cse_in_goal_1(Goal0 - GoalInfo, InstMap0, ModuleInfo, Redo,
		Goal - GoalInfo, InstMap) :-
	detect_cse_in_goal_2(Goal0, GoalInfo, InstMap0, ModuleInfo, Redo, Goal),
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	apply_instmap_delta(InstMap0, InstMapDelta, InstMap).

	% Here we process each of the different sorts of goals.

:- pred detect_cse_in_goal_2(hlds__goal_expr, hlds__goal_info, instmap,
	module_info, bool, hlds__goal_expr).
:- mode detect_cse_in_goal_2(in, in, in, in, out, out) is det.

detect_cse_in_goal_2(call(A,B,C,D,E,F,G), _, _, _, no, call(A,B,C,D,E,F,G)).

detect_cse_in_goal_2(unify(A,B,C,D,E), _, _, _, no, unify(A,B,C,D,E)).

detect_cse_in_goal_2(not(Goal0), _GoalInfo, InstMap, ModuleInfo,
		Redo, not(Goal)) :-
	detect_cse_in_goal(Goal0, InstMap, ModuleInfo, Redo, Goal).

detect_cse_in_goal_2(some(Vars, Goal0), _GoalInfo, InstMap, ModuleInfo,
		Redo, some(Vars, Goal)) :-
	detect_cse_in_goal(Goal0, InstMap, ModuleInfo, Redo, Goal).

detect_cse_in_goal_2(conj(Goals0), _GoalInfo, InstMap, ModuleInfo,
		Redo, conj(Goals)) :-
	detect_cse_in_conj(Goals0, InstMap, ModuleInfo, Redo, Goals).

detect_cse_in_goal_2(disj(Goals0), GoalInfo, InstMap, ModuleInfo, Redo, Goal) :-
	( Goals0 = [] ->
		Redo = no,
		Goal = disj([])
	;
		goal_info_get_nonlocals(GoalInfo, NonLocals),
		set__to_sorted_list(NonLocals, NonLocalsList),
		detect_cse_in_disj(NonLocalsList, Goals0, GoalInfo,
			InstMap, ModuleInfo, Redo, Goal)
	).

detect_cse_in_goal_2(switch(Var, CanFail, Cases0), GoalInfo, InstMap,
		ModuleInfo, Redo, Goal) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	set__to_sorted_list(NonLocals, NonLocalsList),
	detect_cse_in_cases(NonLocalsList, Var, CanFail, Cases0, GoalInfo,
		InstMap, ModuleInfo, Redo, Goal).

detect_cse_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), GoalInfo,
		InstMap, ModuleInfo, Redo, Goal) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	set__to_sorted_list(NonLocals, NonLocalsList),
	detect_cse_in_ite(NonLocalsList, Vars, Cond0, Then0, Else0, GoalInfo,
		InstMap, ModuleInfo, Redo, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_cse_in_conj(list(hlds__goal), instmap, module_info,
	bool, list(hlds__goal)).
:- mode detect_cse_in_conj(in, in, in, out, out) is det.

detect_cse_in_conj([], _InstMap, _ModuleInfo, no, []).
detect_cse_in_conj([Goal0 | Goals0], InstMap0, ModuleInfo, Redo, Goals) :-
	detect_cse_in_goal_1(Goal0, InstMap0, ModuleInfo, Redo1, Goal1,
		InstMap1),
	detect_cse_in_conj(Goals0, InstMap1, ModuleInfo, Redo2, Goals1),
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

:- pred detect_cse_in_disj(list(var), list(hlds__goal), hlds__goal_info,
	instmap, module_info, bool, hlds__goal_expr).
:- mode detect_cse_in_disj(in, in, in, in, in, out, out) is det.

detect_cse_in_disj([], Goals0, _, InstMap, ModuleInfo, Redo, disj(Goals)) :-
	detect_cse_in_disj_2(Goals0, InstMap, ModuleInfo, Redo, Goals).
detect_cse_in_disj([Var | Vars], Goals0, GoalInfo0, InstMap, ModuleInfo,
		Redo, Goal) :-
	(
		% XXX we only need inst_is_bound, but leave this as it is
		% until mode analysis can handle aliasing between variables.
		instmap_lookup_var(InstMap, Var, VarInst0),
		inst_is_ground(ModuleInfo, VarInst0),
		common_deconstruct(Goals0, Var, GoalInfo0, Unify, Goals)
	->
		Goal = conj([Unify, disj(Goals) - GoalInfo0]),
		Redo = yes
	;
		detect_cse_in_disj(Vars, Goals0, GoalInfo0, InstMap, ModuleInfo,
			Redo, Goal)
	).

:- pred detect_cse_in_disj_2(list(hlds__goal), instmap, module_info,
	bool, list(hlds__goal)).
:- mode detect_cse_in_disj_2(in, in, in, out, out) is det.

detect_cse_in_disj_2([], _InstMap, _ModuleInfo, no, []).
detect_cse_in_disj_2([Goal0 | Goals0], InstMap0, ModuleInfo, Redo,
		[Goal | Goals]) :-
	detect_cse_in_goal(Goal0, InstMap0, ModuleInfo, Redo1, Goal),
	detect_cse_in_disj_2(Goals0, InstMap0, ModuleInfo, Redo2, Goals),
	bool__or(Redo1, Redo2, Redo).

:- pred detect_cse_in_cases(list(var), var, can_fail, list(case),
	hlds__goal_info, instmap, module_info, bool, hlds__goal_expr).
:- mode detect_cse_in_cases(in, in, in, in, in, in, in, out, out) is det.

detect_cse_in_cases([], SwitchVar, CanFail, Cases0, _GoalInfo,
		InstMap, ModuleInfo, Redo, switch(SwitchVar, CanFail, Cases)) :-
	detect_cse_in_cases_2(Cases0, InstMap, ModuleInfo, Redo, Cases).
detect_cse_in_cases([Var | Vars], SwitchVar, CanFail, Cases0, GoalInfo,
		InstMap, ModuleInfo, Redo, Goal) :-
	(
		% XXX we only need inst_is_bound, but leave this as it is
		% until mode analysis can handle aliasing between variables.
		instmap_lookup_var(InstMap, Var, VarInst0),
		inst_is_ground(ModuleInfo, VarInst0),
		common_deconstruct_cases(Cases0, Var, GoalInfo, Unify, Cases)
	->
		Goal = conj([Unify, switch(SwitchVar, CanFail, Cases)
			- GoalInfo]),
		Redo = yes
	;
		detect_cse_in_cases(Vars, SwitchVar, CanFail, Cases0, GoalInfo,
			InstMap, ModuleInfo, Redo, Goal)
	).

:- pred detect_cse_in_cases_2(list(case), instmap, module_info,
	bool, list(case)).
:- mode detect_cse_in_cases_2(in, in, in, out, out) is det.

detect_cse_in_cases_2([], _, _, no, []).
detect_cse_in_cases_2([Case0 | Cases0], InstMap, ModuleInfo, Redo,
		[Case | Cases]) :-
	Case0 = case(Functor, Goal0),
	detect_cse_in_goal(Goal0, InstMap, ModuleInfo, Redo1, Goal),
	Case = case(Functor, Goal),
	detect_cse_in_cases_2(Cases0, InstMap, ModuleInfo, Redo2, Cases),
	bool__or(Redo1, Redo2, Redo).

:- pred detect_cse_in_ite(list(var), list(var),
	hlds__goal, hlds__goal, hlds__goal, hlds__goal_info,
	instmap, module_info, bool, hlds__goal_expr).
:- mode detect_cse_in_ite(in, in, in, in, in, in, in, in, out, out) is det.

detect_cse_in_ite([], IfVars, Cond0, Then0, Else0, _, InstMap, ModuleInfo,
		Redo, if_then_else(IfVars, Cond, Then, Else)) :-
	detect_cse_in_ite_2(Cond0, Then0, Else0,
		InstMap, ModuleInfo, Redo, Cond, Then, Else).
detect_cse_in_ite([Var | Vars], IfVars, Cond0, Then0, Else0, GoalInfo,
		InstMap, ModuleInfo, Redo, Goal) :-
	(
		% XXX we only need inst_is_bound, but leave this as it is
		% until mode analysis can handle aliasing between variables.
		instmap_lookup_var(InstMap, Var, VarInst0),
		inst_is_ground(ModuleInfo, VarInst0),
		common_deconstruct([Then0, Else0], Var, GoalInfo, Unify, Goals),
		Goals = [Then, Else]
	->
		Goal = conj([Unify, if_then_else(IfVars, Cond0, Then, Else)
			- GoalInfo]),
		Redo = yes
	;
		detect_cse_in_ite(Vars, IfVars, Cond0, Then0, Else0, GoalInfo,
			InstMap, ModuleInfo, Redo, Goal)
	).

:- pred detect_cse_in_ite_2(hlds__goal, hlds__goal, hlds__goal,
	instmap, module_info, bool, hlds__goal, hlds__goal, hlds__goal).
:- mode detect_cse_in_ite_2(in, in, in, in, in, out, out, out, out) is det.

detect_cse_in_ite_2(Cond0, Then0, Else0, InstMap0, ModuleInfo, Redo,
		Cond, Then, Else) :-
	detect_cse_in_goal_1(Cond0, InstMap0, ModuleInfo, Redo1, Cond,
		InstMap1),
	detect_cse_in_goal(Then0, InstMap1, ModuleInfo, Redo2, Then),
	detect_cse_in_goal(Else0, InstMap0, ModuleInfo, Redo3, Else),
	bool__or(Redo1, Redo2, Redo12),
	bool__or(Redo12, Redo3, Redo).

%-----------------------------------------------------------------------------%

:- pred common_deconstruct(list(hlds__goal), var, hlds__goal_info,
	hlds__goal, list(hlds__goal)).
:- mode common_deconstruct(in, in, in, out, out) is semidet.

common_deconstruct(Goals0, Var, GoalInfo0, Unify, Goals) :-
	common_deconstruct_2(Goals0, Var, no, Goals, MaybeUnifyGoal),
	MaybeUnifyGoal = yes(UnifyGoal),
	% XXX what is the proper GoalInfo here?
	Unify = UnifyGoal - GoalInfo0.

:- pred common_deconstruct_2(list(hlds__goal), var, maybe(hlds__goal_expr),
	list(hlds__goal), maybe(hlds__goal_expr)).
:- mode common_deconstruct_2(in, in, in, out, out) is semidet.

common_deconstruct_2([], _Var, MaybeUnify, [], MaybeUnify).
common_deconstruct_2([Goal0 | Goals0], Var, MaybeUnify0,
		[Goal | Goals], MaybeUnify) :-
	goal_to_conj_list(Goal0, ConjList0),
	Goal0 = _ - GoalInfo,
	map__init(Substitution),
	find_bind_var_for_cse(ConjList0, Substitution, Var, MaybeUnify0,
		ConjList, _NewSubstitution, MaybeUnify1),
	MaybeUnify1 = yes(_),
	conj_list_to_goal(ConjList, GoalInfo, Goal),
	common_deconstruct_2(Goals0, Var, MaybeUnify1, Goals, MaybeUnify).

%-----------------------------------------------------------------------------%

:- pred common_deconstruct_cases(list(case), var, hlds__goal_info,
	hlds__goal, list(case)).
:- mode common_deconstruct_cases(in, in, in, out, out) is semidet.

common_deconstruct_cases(Cases0, Var, GoalInfo0, Unify, Cases) :-
	common_deconstruct_cases_2(Cases0, Var, no, Cases, MaybeUnifyGoal),
	MaybeUnifyGoal = yes(UnifyGoal),
	% XXX what is the proper GoalInfo here?
	Unify = UnifyGoal - GoalInfo0.

:- pred common_deconstruct_cases_2(list(case), var, maybe(hlds__goal_expr),
	list(case), maybe(hlds__goal_expr)).
:- mode common_deconstruct_cases_2(in, in, in, out, out) is semidet.

common_deconstruct_cases_2([], _Var, MaybeUnify, [], MaybeUnify).
common_deconstruct_cases_2([case(ConsId, Goal0) | Cases0], Var, MaybeUnify0,
		[case(ConsId, Goal) | Cases], MaybeUnify) :-
	goal_to_conj_list(Goal0, ConjList0),
	Goal0 = _ - GoalInfo,
	map__init(Substitution),
	find_bind_var_for_cse(ConjList0, Substitution, Var, MaybeUnify0,
		ConjList, _NewSubstitution, MaybeUnify1),
	MaybeUnify1 = yes(_),
	conj_list_to_goal(ConjList, GoalInfo, Goal),
	common_deconstruct_cases_2(Cases0, Var, MaybeUnify1, Cases, MaybeUnify).

%-----------------------------------------------------------------------------%

	% find_bind_var_for_cse(Goals0, Subst0, Var, MaybeUnify0,
	%	Goals, Subst, MaybeUnify):
	%	Searches through Goals0 looking for a deconstruction
	%	unification with `Var'. If MaybeUnify is no, a unification
	%	with any functor is acceptable, and the unification will be
	%	removed from the goal and returned as MaybeUnify. If
	%	MaybeUnify is yes(_), and the unification being considered
	%	has the same functor, then the unification is replaced
	%	by pairwise equalities between the arguments of the
	%	functor in the two unifications, and MaybeUnify0 is
	%	returned as MaybeUnify. Otherwise, MaybeUnify is set to
	%	to no and `Subst' is the resulting substitution from
	%	interpreting through the goal.

:- pred find_bind_var_for_cse(list(hlds__goal), substitution, var,
	maybe(hlds__goal_expr), list(hlds__goal), substitution,
	maybe(hlds__goal_expr)).
:- mode find_bind_var_for_cse(in, in, in, in, out, out, out) is det.

find_bind_var_for_cse([], Substitution, _Var, _MaybeUnify0,
	[], Substitution, no).
find_bind_var_for_cse([Goal0 - GoalInfo | Goals0], Substitution0, Var,
		MaybeUnify0, Goals, Substitution, MaybeUnify) :-
	( Goal0 = conj(SubGoals0) ->
		find_bind_var_for_cse(SubGoals0, Substitution0, Var,
			MaybeUnify0, SubGoals, Substitution1, MaybeUnify1),
		Goal = conj(SubGoals),
		( MaybeUnify1 = yes(_) ->
			Goals = [Goal - GoalInfo | Goals0],
			Substitution = Substitution1,
			MaybeUnify = MaybeUnify1
		;
			find_bind_var_for_cse(Goals0, Substitution0, Var,
				MaybeUnify0, Goals1, Substitution, MaybeUnify),
			Goals = [Goal0 - GoalInfo | Goals1]
		)
	; Goal0 = unify(A, B, _, UnifyInfo0, _) ->
			 % otherwise abstractly interpret the unification
		( interpret_unify(A, B, Substitution0, Substitution1) ->
			Substitution2 = Substitution1
		;
			% the unification must fail - just ignore it
			Substitution2 = Substitution0
		),
			% check whether the var was bound
		term__apply_rec_substitution(term__variable(Var), Substitution2,
			Term),
		(
			Term = term__functor(_, _, _),
			UnifyInfo0 = deconstruct(_, _, _, _, _),
			MaybeUnify0 = no
		->
			% XXX bug: we should make sure that Goal0 refers to Var
			construct_common_unify(Var, Goal0, Goal),
			MaybeUnify = yes(Goal),
			Goals = Goals0,
			Substitution = Substitution2
		;
			Term = term__functor(_, _, _),
			UnifyInfo0 = deconstruct(_, _, _, _, _),
			MaybeUnify0 = yes(OldUnifyGoal),
			goal_info_context(GoalInfo, Context),
			find_similar_deconstruct(OldUnifyGoal, UnifyInfo0,
				Context, Replacements)
		->
			list__append(Replacements, Goals0, Goals),
			Substitution = Substitution2,
			MaybeUnify = MaybeUnify0
		;
			find_bind_var_for_cse(Goals0, Substitution2, Var,
				MaybeUnify0, Goals1, Substitution, MaybeUnify),
			Goals = [Goal0 - GoalInfo | Goals1]
		)
	;
		Goals = [Goal0 - GoalInfo | Goals0],
		Substitution = Substitution0,
		MaybeUnify = no
	).

:- pred construct_common_unify(var, hlds__goal_expr, hlds__goal_expr).
:- mode construct_common_unify(in, in, out) is det.

construct_common_unify(Var, Goal0, Goal) :-
	(
		Goal0 = unify(_, Term, Umode, Unif0, Ucontext),
		Unif0 = deconstruct(_, Consid, Args, Submodes, CanFail)
	->
		Unif = deconstruct(Var, Consid, Args, Submodes, CanFail),
		( Term = functor(_, _) ->
			Goal = unify(Var, Term, Umode, Unif, Ucontext)
		;
			error("unexpected unify structure in construct_common_unify")
		)
	;
		error("unexpected goal in construct_common_unify")
	).

:- pred find_similar_deconstruct(hlds__goal_expr, unification, term__context,
	list(hlds__goal)).
:- mode find_similar_deconstruct(in, in, in, out) is semidet.

find_similar_deconstruct(OldUnifyGoal, NewUnifyInfo, Context, Replacement) :-
	(
		OldUnifyGoal = unify(_OT1, _OT2, _OM, OldUnifyInfo, OC),
		OldUnifyInfo = deconstruct(_OV, OF, OFV, _OUM, _OCF),
		NewUnifyInfo = deconstruct(_NV, NF, NFV, _NUM, _NCF)
	->
		OF = NF,
		list__length(OFV, OFVC),
		list__length(NFV, NFVC),
		OFVC = NFVC,
		pair_subterms(OFV, NFV, Context, OC, Replacement)
	;
		error("goals should be deconstructions")
	).

:- pred pair_subterms(list(var), list(var),
			term__context, unify_context, list(hlds__goal)).
:- mode pair_subterms(in, in, in, in, out) is det.

pair_subterms(OFV0, NFV0, Context, UnifyContext, Replacement) :-
	(
		OFV0 = [OFV | OFV1],
		NFV0 = [NFV | NFV1]
	->
		pair_subterms(OFV1, NFV1, Context, UnifyContext, Replacement1),
		( OFV = NFV ->
			Replacement = Replacement1
		;
			UnifyContext = unify_context(MainCtxt, SubCtxt),

			create_atomic_unification(OFV, var(NFV),
					Context, MainCtxt, SubCtxt, Goal),
			Replacement = [Goal | Replacement1]
		)
	;
		OFV0 = [],
		NFV0 = []
	->
		Replacement = []
	;
		error("mismatched length lists in pair_subterms")
	).

%-----------------------------------------------------------------------------%
