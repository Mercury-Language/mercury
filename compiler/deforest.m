%-----------------------------------------------------------------------------%
% Copyright (C) 1998 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: deforest.m
% Main author: stayl.
%-----------------------------------------------------------------------------%
%
% Deforestation.
%
% A start on the documentation for this is in $CVSROOT/papers/deforest.
%
%-----------------------------------------------------------------------------%
:- module deforest.

:- interface.

:- import_module hlds_module.
:- import_module io.

:- pred deforestation(module_info::in, module_info::out,
		io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module pd_cost, pd_debug, pd_info, pd_term, pd_util.
:- import_module hlds_pred, hlds_goal, inlining, passes_aux.
:- import_module (inst), instmap, inst_match, simplify.
:- import_module dependency_graph, hlds_data, det_analysis, globals.
:- import_module mode_util, goal_util, prog_data, prog_util, purity.
:- import_module modes, mode_info, unique_modes, options, hlds_out.
:- import_module quantification.

:- import_module assoc_list, bool, getopt, int, list, map, require.
:- import_module set, std_util, string, term, varset.

deforestation(ModuleInfo0, ModuleInfo, IO0, IO) :-
	proc_arg_info_init(ProcArgInfo0),
	type_to_univ(ProcArgInfo0, UnivProcArgInfo0),

	% Find out which arguments of each procedure are switched on
	% at the top level or are constructed in a way which is
	% possibly deforestable.
	Task0 = update_module_cookie(deforest__get_branch_vars_proc,
			UnivProcArgInfo0),
	process_all_nonimported_procs(Task0, Task, 
		ModuleInfo0, ModuleInfo1, IO0, IO1),
	(
		Task = update_module_cookie(_, UnivProcArgInfo),
		univ_to_type(UnivProcArgInfo, ProcArgInfo1)
	->	
		ProcArgInfo = ProcArgInfo1
	;
		error("deforestation: passes_aux stuffed up")
	),

	% We process the module bottom-up to make estimation of the 
	% cost improvement of new versions a little more accurate and
	% also to avoid redoing optimizations.
	module_info_ensure_dependency_info(ModuleInfo1, ModuleInfo2),
	module_info_dependency_info(ModuleInfo2, DepInfo),
	hlds_dependency_info_get_dependency_ordering(DepInfo, DepOrdering),
	list__condense(DepOrdering, DepList),

	inst_table_init(InstTable),
	pd_info_init(ModuleInfo2, ProcArgInfo, InstTable, IO1, PdInfo0),
	pd_info_foldl(deforest__proc, DepList, PdInfo0, PdInfo1),
	pd_info_get_module_info(ModuleInfo3, PdInfo1, PdInfo),
	module_info_clobber_dependency_info(ModuleInfo3, ModuleInfo),
	pd_info_get_io_state(IO, PdInfo, _).

:- pred proc_arg_info_init(map(pred_proc_id, pd_proc_arg_info)::out) is det.

proc_arg_info_init(ProcArgInfo0) :-
	map__init(ProcArgInfo0).

:- pred deforest__get_branch_vars_proc(pred_id::in, proc_id::in, 
	proc_info::in, proc_info::out, univ::in, univ::out, 
	module_info::in, module_info::out) is det.

deforest__get_branch_vars_proc(PredId, ProcId, ProcInfo, ProcInfo, 
		UnivProcArgInfo0, UnivProcArgInfo, 
		ModuleInfo0, ModuleInfo) :-
	( univ_to_type(UnivProcArgInfo0, ProcArgInfo0) ->
		pd_util__get_branch_vars_proc(proc(PredId, ProcId), ProcInfo, 
			ProcArgInfo0, ProcArgInfo, ModuleInfo0, ModuleInfo),
		type_to_univ(ProcArgInfo, UnivProcArgInfo)
	;
		error("deforest__get_branch_vars_proc")
	).

:- pred deforest__proc(pred_proc_id::in, pd_info::pd_info_di, 
		pd_info::pd_info_uo) is det.

deforest__proc(PredProcId) -->
	deforest__proc(PredProcId, _, _).

:- pred deforest__proc(pred_proc_id::in, int::out, int::out,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__proc(proc(PredId, ProcId), CostDelta, SizeDelta) -->
	pd_info_get_module_info(ModuleInfo0),
	pd_info_get_io_state(IO0),
	{ write_proc_progress_message("% Deforesting ", 
		PredId, ProcId, ModuleInfo0, IO0, IO1) },
	pd_info_set_io_state(IO1),
	{ module_info_pred_proc_info(ModuleInfo0, PredId, ProcId, 
		PredInfo0, ProcInfo0) },
	pd_info_init_unfold_info(proc(PredId, ProcId), PredInfo0, ProcInfo0),
	{ proc_info_goal(ProcInfo0, Goal0) },
	{ proc_info_inst_table(ProcInfo0, InstTable0) },
	pd_info_set_inst_table(InstTable0),
	deforest__goal(Goal0, Goal1),
	pd_info_get_proc_info(ProcInfo1),
	{ proc_info_set_goal(ProcInfo1, Goal1, ProcInfo2) },
	pd_info_get_inst_table(InstTable1),
	{ proc_info_set_inst_table(ProcInfo2, InstTable1, ProcInfo3) },
	pd_info_get_changed(Changed),

	( { Changed = yes } ->
		pd_info_get_module_info(ModuleInfo2),
		{ requantify_proc(ProcInfo3, ProcInfo4) },
		{ proc_info_goal(ProcInfo4, Goal4) },
		{ proc_info_get_initial_instmap(ProcInfo4,
			ModuleInfo2, InstMap0) },
		{ proc_info_headvars(ProcInfo4, ArgVars) },
		{ proc_info_arglives(ProcInfo4, ModuleInfo2, ArgLives) },
		{ proc_info_vartypes(ProcInfo4, VarTypes) },
		{ proc_info_inst_table(ProcInfo4, InstTable4) },
		{ recompute_instmap_delta(ArgVars, ArgLives, VarTypes,
			Goal4, Goal, InstMap0, InstTable4, InstTable,
			_, ModuleInfo2, ModuleInfo3) },
		pd_info_set_module_info(ModuleInfo3),

		pd_info_get_pred_info(PredInfo),
		{ proc_info_set_goal(ProcInfo4, Goal, ProcInfo5) },
		{ proc_info_set_inst_table(ProcInfo5, InstTable, ProcInfo) },
		{ module_info_set_pred_proc_info(ModuleInfo3, PredId, ProcId,
			PredInfo, ProcInfo, ModuleInfo4) },

		pd_info_get_rerun_det(RerunDet),

		( { RerunDet = yes } ->
			% If the determinism of some sub-goals has changed,
			% then we re-run determinism analysis. As with
			% inlining.m, this avoids problems with inlining
			% erroneous procedures.
			pd_info_get_io_state(IO10),
			{ globals__io_get_globals(Globals, IO10, IO11) },
			pd_info_set_io_state(IO11),
			{ det_infer_proc(PredId, ProcId, ModuleInfo4,
				ModuleInfo5, Globals, _, _, _) }
		;
			{ ModuleInfo5 = ModuleInfo4 }
		),

		% Recompute the branch_info for the procedure.
		pd_info_get_proc_arg_info(ProcArgInfo0),
		{ pd_util__get_branch_vars_proc(proc(PredId, ProcId), ProcInfo, 
			ProcArgInfo0, ProcArgInfo, ModuleInfo5, ModuleInfo6) },
		pd_info_set_proc_arg_info(ProcArgInfo),
		pd_info_set_module_info(ModuleInfo6)
	;
		[]
	),		

	pd_info_get_module_info(ModuleInfo),
	pd_info_get_io_state(IO20),
	{ write_proc_progress_message("% Finished deforesting ", 
		PredId, ProcId, ModuleInfo, IO20, IO21) },
	pd_info_set_io_state(IO21),
	pd_info_get_cost_delta(CostDelta),
	pd_info_get_size_delta(SizeDelta),
	pd_info_unset_unfold_info.

:- pred deforest__goal(hlds_goal::in, hlds_goal::out, 
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__goal(conj(Goals0) - Info, conj(Goals) - Info) -->
	pd_info_get_instmap(InstMap0),
	deforest__partially_evaluate_conj_goals(Goals0, [], Goals1),
	pd_info_set_instmap(InstMap0),
	deforest__compute_goal_infos(Goals1, Goals2),
	pd_info_set_instmap(InstMap0),
	{ goal_info_get_nonlocals(Info, NonLocals) },
	deforest__conj(Goals2, NonLocals, [], Goals),
	pd_info_set_instmap(InstMap0).

	% XXX cannot deforest across parallel_conjunctions!
deforest__goal(par_conj(Goals, SM) - Info, par_conj(Goals, SM) - Info) --> [].

deforest__goal(disj(Goals0, SM) - Info, disj(Goals, SM) - Info) -->
	deforest__disj(Goals0, Goals).

deforest__goal(if_then_else(Vars, Cond0, Then0, Else0, SM) - Info, 
		if_then_else(Vars, Cond, Then, Else, SM) - Info) -->
	pd_info_get_instmap(InstMap0),
	deforest__goal(Cond0, Cond),
	pd_info_update_goal(Cond),
	deforest__goal(Then0, Then),
	pd_info_set_instmap(InstMap0),
	deforest__goal(Else0, Else),
	pd_info_set_instmap(InstMap0).
		
deforest__goal(switch(Var, CanFail, Cases0, SM) - Info,
		switch(Var, CanFail, Cases, SM) - Info) -->
	deforest__cases(Var, Cases0, Cases).

deforest__goal(Goal, Goal) -->
	{ Goal = pragma_c_code(_, _, _, _, _, _, _) - _ }.

deforest__goal(Goal, Goal) -->
	{ Goal = higher_order_call(_, _, _, _, _, _) - _ }.

deforest__goal(Goal, Goal) -->
	{ Goal = class_method_call(_, _, _, _, _, _) - _ }.

deforest__goal(not(Goal0) - Info, not(Goal) - Info) -->
	deforest__goal(Goal0, Goal).

deforest__goal(some(Vs, Goal0) - Info, some(Vs, Goal) - Info) -->
	deforest__goal(Goal0, Goal).

deforest__goal(Goal0, Goal) -->
	{ Goal0 = call(PredId, ProcId, Args, _, _, Name) - _ },
	deforest__call(PredId, ProcId, Args, Name, Goal0, Goal).
	
deforest__goal(Goal, Goal) -->
	{ Goal = unify(_, _, _, _, _) - _ }.

%-----------------------------------------------------------------------------%

:- pred deforest__disj(list(hlds_goal)::in, list(hlds_goal)::out,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__disj([], []) --> [].
deforest__disj([Goal0 | Goals0], [Goal | Goals]) -->
	pd_info_get_instmap(InstMap0),
	deforest__goal(Goal0, Goal),
	pd_info_set_instmap(InstMap0),
	deforest__disj(Goals0, Goals).

%-----------------------------------------------------------------------------%

:- pred deforest__cases(var::in, list(case)::in, list(case)::out,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__cases(_, [], []) --> [].
deforest__cases(Var, [case(ConsId, IMDelta, Goal0) | Cases0],
		[case(ConsId, IMDelta, Goal) | Cases]) -->
	% Bind Var to ConsId in the instmap before processing this case.
	pd_info_get_instmap(InstMap0),
	pd_info_apply_instmap_delta(IMDelta),
	deforest__goal(Goal0, Goal),
	pd_info_set_instmap(InstMap0),
	deforest__cases(Var, Cases0, Cases).

%-----------------------------------------------------------------------------%

	% Perform partial evaluation on the goals of a conjunction.
:- pred deforest__partially_evaluate_conj_goals(list(hlds_goal)::in,
		list(hlds_goal)::in, list(hlds_goal)::out,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__partially_evaluate_conj_goals([], RevGoals, Goals) -->
	{ list__reverse(RevGoals, Goals) }.
deforest__partially_evaluate_conj_goals([Goal0 | Goals0], RevGoals0, Goals) -->
	deforest__goal(Goal0, Goal1),
	pd_info_update_goal(Goal1),
	( { Goal1 = conj(Goals1) - _ } ->
		{ list__reverse(Goals1, RevGoals1) },
		{ list__append(RevGoals1, RevGoals0, RevGoals2) }
	;
		{ RevGoals2 = [Goal1 | RevGoals0] }
	),
	deforest__partially_evaluate_conj_goals(Goals0, RevGoals2, Goals).

%-----------------------------------------------------------------------------%

	% Compute the branch info for each goal in a conjunction.
:- pred deforest__compute_goal_infos(list(hlds_goal)::in, annotated_conj::out,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.	

deforest__compute_goal_infos([], []) --> [].
deforest__compute_goal_infos([Goal | Goals0], 
		[Goal - MaybeBranchInfo | Goals]) -->
	deforest__get_branch_vars_goal(Goal, MaybeBranchInfo),
	pd_info_update_goal(Goal),
	deforest__compute_goal_infos(Goals0, Goals).

:- pred deforest__get_branch_vars_goal(hlds_goal::in, 
		maybe(pd_branch_info(var))::out, pd_info::pd_info_di, 
		pd_info::pd_info_uo) is det.

deforest__get_branch_vars_goal(Goal, MaybeBranchInfo) -->
	{ Goal = GoalExpr - _ },
	( { goal_util__goal_is_branched(GoalExpr) } ->
		pd_util__get_branch_vars_goal(Goal, MaybeBranchInfo)
	; { GoalExpr = call(PredId, ProcId, Args, _, _, _) } ->
		pd_info_get_proc_arg_info(ProcBranchInfos),
		(
			{ map__search(ProcBranchInfos, 
				proc(PredId, ProcId), BranchInfo0) }
		->
			% Rename the branch_info for the called procedure
			% onto the argument variables.
			{ pd_util__convert_branch_info(BranchInfo0, 
				Args, BranchInfo) },
			{ MaybeBranchInfo = yes(BranchInfo) }
		;
			{ MaybeBranchInfo = no }
		)
	;
		{ MaybeBranchInfo = no }
	).

%-----------------------------------------------------------------------------%

:- type annotated_conj == assoc_list(hlds_goal, maybe(pd_branch_info(var))).

:- pred deforest__conj(annotated_conj::in, set(var)::in, list(hlds_goal)::in, 
	list(hlds_goal)::out, pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__conj([], _, RevGoals, Goals) --> 
	{ list__reverse(RevGoals, Goals) }.
deforest__conj([Goal0 - MaybeBranchInfo | Goals0], NonLocals, 
		RevGoals0, RevGoals) -->
	(
		% Look for a goal later in the conjunction to deforest with.
		{ MaybeBranchInfo = yes(GoalBranchInfo) },
		{ deforest__detect_deforestation(Goal0, GoalBranchInfo, 
			Goals0, Goals1, DeforestInfo) }
	->
		deforest__handle_deforestation(NonLocals, DeforestInfo, 
			RevGoals0, RevGoals1, Goals1, Goals2, Optimized),
		( { Optimized = yes } ->
			deforest__conj(Goals2, NonLocals, RevGoals1, RevGoals)
		;
			pd_info_update_goal(Goal0),
			{ RevGoals2 = [Goal0 | RevGoals0] },
			deforest__conj(Goals0, NonLocals, RevGoals2, RevGoals)
		)
	;	
		pd_info_update_goal(Goal0),
		{ RevGoals1 = [Goal0 | RevGoals0] },
		deforest__conj(Goals0, NonLocals, RevGoals1, RevGoals)
	).

%-----------------------------------------------------------------------------%

:- type deforest_info
	---> deforest_info(
		hlds_goal,		% earlier goal in conjunction
		pd_branch_info(var),	% branch_info for earlier goal
		list(hlds_goal),	% goals in between
		hlds_goal,		% later goal in conjunction
		pd_branch_info(var),	% branch_info for later goal
		set(int)		% branches for which there is
					% extra information about the second
					% goal, numbering starts at 1.
	).

	% Search backwards through the conjunction for the last
	% goal which contains extra information about the variable 
	% being switched on.
:- pred deforest__detect_deforestation(hlds_goal::in, pd_branch_info(var)::in, 
		annotated_conj::in, annotated_conj::out, 
		deforest_info::out) is semidet.

deforest__detect_deforestation(EarlierGoal, BranchInfo, 
		Goals0, Goals1, DeforestInfo) :-
	deforest__search_for_deforest_goal(EarlierGoal, BranchInfo, [], 
		Goals0, Goals1, DeforestInfo).
	
:- pred deforest__search_for_deforest_goal(hlds_goal::in,
		pd_branch_info(var)::in, annotated_conj::in,
		annotated_conj::in, annotated_conj::out,
		deforest_info::out) is semidet.

deforest__search_for_deforest_goal(EarlierGoal, EarlierBranchInfo, 
		RevBetweenGoals0, [Goal | Goals0], Goals, DeforestInfo) :-
	( 
		Goal = LaterGoal - yes(LaterBranchInfo),
		deforest__potential_deforestation(EarlierBranchInfo, 
			LaterBranchInfo, DeforestBranches)
	->
		list__reverse(RevBetweenGoals0, BetweenGoals1),
		assoc_list__keys(BetweenGoals1, BetweenGoals),
		Goals = Goals0,
		DeforestInfo = deforest_info(
				EarlierGoal, 
				EarlierBranchInfo,
				BetweenGoals, 
				LaterGoal,
				LaterBranchInfo,
				DeforestBranches
			)
	;
		deforest__search_for_deforest_goal(EarlierGoal, 
			EarlierBranchInfo, [Goal | RevBetweenGoals0], 
			Goals0, Goals, DeforestInfo)
	).

	% Look for a variable in the second branch_info for which
	% we have more information in the first than in the instmap.
	% Get the branches in the first goal which contain this extra
	% information.
:- pred deforest__potential_deforestation(pd_branch_info(var)::in, 
		pd_branch_info(var)::in, set(int)::out) is semidet.

deforest__potential_deforestation(Info1, Info2, DeforestBranches) :-
	Info1 = pd_branch_info(VarMap1, _, _),
	Info2 = pd_branch_info(_, LeftVars2, _),

	map__select(VarMap1, LeftVars2, VarMap),
	map__to_assoc_list(VarMap, VarAssoc),
	\+ map__is_empty(VarMap),

		% Work out which branches of the first goal should
		% contain unfolded versions of the second goal.
	GetBranches = 
	    lambda([VarInfo::in, Branches0::in, Branches::out] is det, (
		VarInfo = _ - Branches1,
		set__union(Branches0, Branches1, Branches)
	    )),
	set__init(DeforestBranches0),
	list__foldl(GetBranches, VarAssoc, 
		DeforestBranches0, DeforestBranches).

%-----------------------------------------------------------------------------%

	% Take the part of a conjunction found to have potential
	% for deforestation and attempt the optimization.
:- pred deforest__handle_deforestation(set(var)::in, deforest_info::in,
		list(hlds_goal)::in, list(hlds_goal)::out, 
		annotated_conj::in, annotated_conj::out, 
		bool::out, pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__handle_deforestation(NonLocals, DeforestInfo0, RevBeforeGoals0, 
		RevBeforeGoals, AfterGoals0, AfterGoals, Optimized) -->

	pd_info_get_instmap(InstMap0),
	pd_info_get_created_versions(CreatedVersions0),

	pd_info_get_depth(Depth0),
	pd_debug__message("checking for deforestation at depth %i\n", 
		[i(Depth0)]),

	deforest__reorder_conj(DeforestInfo0, DeforestInfo,
		BeforeIrrelevant, AfterIrrelevant),

	{ deforest__get_sub_conj_nonlocals(NonLocals, DeforestInfo, 
		RevBeforeGoals0, BeforeIrrelevant, AfterIrrelevant, 
		AfterGoals0, ConjNonLocals) },

		% Update the instmap.
	pd_info_foldl(pd_info_update_goal, BeforeIrrelevant),

	pd_info_get_pred_proc_id(CurrPredProcId),
	pd_info_get_parents(Parents0),
	pd_info_get_cost_delta(CostDelta0),
	pd_info_get_size_delta(SizeDelta0),
	pd_info_get_module_info(ModuleInfo),

	{ DeforestInfo = deforest_info(EarlierGoal, _, BetweenGoals, 
				LaterGoal, _, DeforestBranches) },

	deforest__should_try_deforestation(DeforestInfo, ShouldOptimize),
	( { ShouldOptimize = no } ->	
		{ Optimized0 = no },
		{ Goals = [] }
	;
		{ EarlierGoal = call(PredId1, _, _, _, _, _) - _ },
		{ LaterGoal = call(PredId2, _, _, _, _, _) - _ }
	->
		%
		% If both goals are calls create a new predicate
		% for the conjunction to be deforested and process it.
		%
		pd_info_get_module_info(ModuleInfo0),
		{ predicate_name(ModuleInfo0, PredId1, PredName1) },
		{ predicate_name(ModuleInfo0, PredId2, PredName2) },
		pd_debug__message("deforesting calls to %s and %s\n",
		[s(PredName1), s(PredName2)]),
		deforest__call_call(ConjNonLocals, DeforestInfo,
			Goal, Optimized0),
		{ Goals = [Goal] }
	;
		%
		% If the first goal is branched and the second goal is
		% a call, attempt to push the call into the branches.
		% Don't push a recursive call or a call to a predicate we
		% have already pushed into a switch, since it is difficult
		% to stop the process.
		%
		{ EarlierGoal = EarlierGoalExpr - _ },
		{ goal_util__goal_is_branched(EarlierGoalExpr) },
		{ LaterGoal = call(PredId, ProcId, _, _, _, _) - _ },
		{ PredProcId = proc(PredId, ProcId) },
		{ PredProcId \= CurrPredProcId },
		\+ { set__member(PredProcId, Parents0) }
	->
		{ predicate_name(ModuleInfo, PredId, CurrPredName) },
		pd_debug__message("Pushing call to %s into goal\n", 
			[s(CurrPredName)]),
		{ set__insert(Parents0, proc(PredId, ProcId), Parents) },
		pd_info_set_parents(Parents),
		deforest__push_goal_into_goal(ConjNonLocals, DeforestBranches, 
			EarlierGoal, BetweenGoals, LaterGoal, Goal),
		{ Goals = [Goal] },
		{ Optimized0 = yes }
	;
		% 
		% If both goals are branched, push the second into the
		% branches of the first.
		%
		{ EarlierGoal = EarlierGoalExpr - _ },
		{ LaterGoal = LaterGoalExpr - _ },
		{ goal_util__goal_is_branched(EarlierGoalExpr) },
		{ goal_util__goal_is_branched(LaterGoalExpr) }
	->
		pd_debug__message("Pushing goal into goal\n", []),
		deforest__push_goal_into_goal(ConjNonLocals, DeforestBranches,
			EarlierGoal, BetweenGoals, LaterGoal, Goal),
		{ Goals = [Goal] },
		{ goals_size([EarlierGoal | BetweenGoals], ConjSize1) },
		{ goal_size(LaterGoal, ConjSize2) },
		{ goal_size(Goal, NewSize) },
		{ SizeDiff = NewSize - ConjSize1 - ConjSize2 },
		pd_info_incr_size_delta(SizeDiff),
		{ Optimized0 = yes }
	;
		pd_debug__message("not optimizing\n", []),
		{ Goals = [] },
		{ Optimized0 = no }
	),
	deforest__check_improvement(Optimized0, 
		CostDelta0, SizeDelta0, Optimized),
	%
	% Clean up.
	%
	pd_info_set_depth(Depth0),
	pd_info_set_instmap(InstMap0),
	( { Optimized = no } ->

		% XXX currently this only attempts to deforest the
		% first goal with the first matching goal later in
		% the conjunction. If the deforestation failed,
		% other later goals should be tried.
		%
		% Return everything to the state it was in before
		% the attempted optimization.
		pd_info_set_cost_delta(CostDelta0),
		pd_info_set_size_delta(SizeDelta0),

		% Remove any versions which were created.
		pd_info_get_created_versions(CreatedVersions),
		{ set__difference(CreatedVersions, 
			CreatedVersions0, NewVersions0) },
		{ set__to_sorted_list(NewVersions0, NewVersions) },
		pd_info_foldl(pd_info__remove_version, NewVersions),

		% These will be restored properly in deforest__conj.
		{ RevBeforeGoals = RevBeforeGoals0 },
		{ AfterGoals = AfterGoals0 }
	; 
		% We want to reprocess the deforested goal to see
		% if it can be deforested with other goals later in
		% the conjunction.
		{ RevBeforeGoals = RevBeforeGoals0 },
		{ list__condense([BeforeIrrelevant, Goals, AfterIrrelevant],
			GoalsToProcess) },
		deforest__compute_goal_infos(GoalsToProcess, GoalsAndInfo),
		{ list__append(GoalsAndInfo, AfterGoals0, AfterGoals) },
		pd_info_set_instmap(InstMap0),
		pd_info_set_changed(yes),
		pd_info_set_rerun_det(yes)
	),
	pd_debug__message("finished deforestation at depth %i\n", [i(Depth0)]),
	pd_info_set_parents(Parents0).

	% Check whether deforestation is legal and worthwhile.
:- pred deforest__should_try_deforestation(deforest_info::in,
		bool::out, pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__should_try_deforestation(DeforestInfo, ShouldTry) -->

	{ DeforestInfo = deforest_info(EarlierGoal, EarlierBranchInfo,
				BetweenGoals, LaterGoal, _, _) },
	pd_info_lookup_option(deforestation_depth_limit, DepthLimitOpt),
	pd_info_get_depth(Depth0),
	{ Depth is Depth0 + 1 },
	pd_info_set_depth(Depth),

	pd_info_get_module_info(ModuleInfo),
	pd_info_lookup_option(fully_strict, FullyStrictOp),
	( 
		{ DepthLimitOpt = int(MaxDepth) },
		{ MaxDepth \= -1 }, 	% no depth limit set
		{ Depth0 >= MaxDepth }
	->
		% The depth limit was exceeded. This should not
		% occur too often in practice - the depth limit
		% is just a safety net.
		pd_debug__message("\n\n*****Depth limit exceeded*****\n\n", []),
		{ ShouldTry = no }
	;
		%
		% If some later goal depends on a variable such as an io__state
		% for which the construction cannot be reversed, recursive
		% folding will be impossible, so give up on the optimization.
		%
		{ EarlierBranchInfo = pd_branch_info(_, _, OpaqueVars) },
		{ list__member(OpaqueGoal, BetweenGoals)
		; OpaqueGoal = LaterGoal
		},
		{ OpaqueGoal = _ - OpaqueGoalInfo },
		{ goal_info_get_nonlocals(OpaqueGoalInfo, OpaqueNonLocals) },
		{ set__intersect(OpaqueNonLocals, OpaqueVars, 
			UsedOpaqueVars) },
		\+ { set__empty(UsedOpaqueVars) }
	->
		pd_debug__message("later goals depend on opaque vars\n", []),
		{ ShouldTry = no }
	;
		%
		% Don't optimize if that would require duplicating
		% branched goal structures.
		%
		\+ { deforest__is_simple_goal_list(BetweenGoals) }
	->
		pd_debug__message("between goals not simple enough\n", []),
		{ ShouldTry = no }
	;
		%
		% Give up if there are any impure goals involved.
		%
		( { list__member(ImpureGoal, BetweenGoals) }
		; { ImpureGoal = EarlierGoal }
		; { ImpureGoal = LaterGoal }
		),
		{ ImpureGoal = _ - ImpureGoalInfo },
		\+ { goal_info_is_pure(ImpureGoalInfo) }
	->
		pd_debug__message("goal list contains impure goal(s)\n", []),
		{ ShouldTry = no }
	;
		%
		% Check whether interleaving the execution of the goals could
		% alter the termination behaviour in a way which is
		% illegal according to the semantics options.
		%
		{ FullyStrictOp = bool(FullyStrict) },
		{ list__member(OtherGoal, BetweenGoals)
		; OtherGoal = LaterGoal
		},
		\+ { pd_util__reordering_maintains_termination(ModuleInfo,
			FullyStrict, EarlierGoal, OtherGoal) }
	->
		pd_debug__message("interleaving execution could change termination behaviour\n", []),
		{ ShouldTry = no }
	;
		{ ShouldTry = yes }
	).

	% Check that the code size increase is justified by the 
	% estimated performance increase. This should err towards 
	% allowing optimization - without any check at all the 
	% code size of the library only increases ~10%.
:- pred deforest__check_improvement(bool::in, int::in, int::in, bool::out,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__check_improvement(Optimized0, CostDelta0, SizeDelta0, Optimized) -->
	pd_info_get_cost_delta(CostDelta),
	pd_info_get_size_delta(SizeDelta),
	{ Improvement = CostDelta - CostDelta0 },
	{ SizeDifference = SizeDelta - SizeDelta0 },
	pd_info_lookup_option(deforestation_cost_factor, FactorOpt),
	( 
		{ Optimized0 = yes }, 
		{ FactorOpt = int(Factor) },
		{ deforest__check_deforestation_improvement(Factor,
			Improvement, SizeDifference) }
	->
		{ Optimized = yes },
		pd_debug__message("Enough improvement: cost(%i) size(%i)\n",
			[i(Improvement), i(SizeDifference)])
	;
		{ Optimized = no },
		pd_debug__message("Not enough improvement: cost(%i) size(%i)\n",
			[i(Improvement), i(SizeDifference)])
	).

%-----------------------------------------------------------------------------%

	% Attempt deforestation on a pair of calls.
:- pred deforest__call_call(set(var)::in, deforest_info::in, 
		hlds_goal::out, bool::out,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__call_call(ConjNonLocals, DeforestInfo, Goal, Optimized) -->
	{ DeforestInfo = deforest_info(EarlierGoal, _, 
			BetweenGoals, LaterGoal, _, _) },

	{ deforest__create_conj(EarlierGoal, BetweenGoals, 
		LaterGoal, ConjNonLocals, FoldGoal) },

	pd_info__search_version(FoldGoal, MaybeVersion),
	pd_info_get_parent_versions(Parents),
	(
		{ MaybeVersion = version(_, VersionPredProcId, 
			VersionInfo, Renaming, TypeRenaming) } 
	->
		% If we see an opportunity to fold, take it. 
		{ VersionPredProcId = proc(VersionPredId, _) },
		pd_info_get_module_info(ModuleInfo0),
		{ predicate_name(ModuleInfo0, VersionPredId, FoldPredName) },
		pd_debug__message("Folded with %s\n", [s(FoldPredName)]),
		( { set__member(VersionPredProcId, Parents) } ->
			{ pd_cost__recursive_fold(FoldCostDelta) }
		;
			{ pd_cost__fold(FoldCostDelta) }
		),
		pd_info_incr_cost_delta(FoldCostDelta),
		{ goals_size([EarlierGoal | BetweenGoals], NegSizeDelta) },
		{ SizeDelta is - NegSizeDelta },
		pd_info_incr_size_delta(SizeDelta),
		deforest__create_call_goal(VersionPredProcId, 
			VersionInfo, Renaming, TypeRenaming, Goal),
		{ Optimized = yes }
	;
		pd_info_get_global_term_info(TermInfo0),
		pd_info_get_parent_versions(ParentVersions0),

		pd_debug__do_io(io__write_string("Parents: ")),
		pd_debug__write(ParentVersions0),
		pd_debug__do_io(io__nl),

		pd_info_get_module_info(ModuleInfo),
		pd_info_get_versions(Versions),
		pd_info_get_instmap(InstMap),
		{ pd_term__global_check(ModuleInfo, EarlierGoal, BetweenGoals, 
			LaterGoal, InstMap, Versions, TermInfo0, 
			TermInfo, CheckResult) },
		( 
			{ CheckResult = ok(ProcPair, Size) },
			pd_debug__message("global termination check succeeded - creating new version\n", []),
			pd_info_set_global_term_info(TermInfo),
			{ RunModes = no },
			{ MaybeGeneralised = no },
			deforest__create_deforest_goal(EarlierGoal, 
				BetweenGoals, LaterGoal, FoldGoal,
				ConjNonLocals, RunModes, ProcPair, Size,
				MaybeGeneralised, Goal, Optimized)
		;
			{ CheckResult = possible_loop(ProcPair, Size, 
						CoveringPredProcId) },
			% The termination check found the same 
			% pair of end-points with the same length goal. 
			% If the goal matches the goal for the "covering"
			% predicate, perform a most specific 
			% generalisation on the insts then keep 
			% on going.
			deforest__try_generalisation(EarlierGoal,
				BetweenGoals, LaterGoal, FoldGoal,
				ConjNonLocals, ProcPair, Size, 
				CoveringPredProcId, Goal, Optimized)
		;
			{ CheckResult = loop },
			pd_debug__message("global termination check failed\n", []),
			{ Goal = LaterGoal },
			{ Optimized = no }
		),
		pd_info_set_global_term_info(TermInfo0)
	).

%-----------------------------------------------------------------------------%

	% Create a new procedure for a conjunction to be deforested, then
	% recursively process that procedure.
:- pred deforest__create_deforest_goal(hlds_goal::in, hlds_goals::in, 
	hlds_goal::in, hlds_goal::in, set(var)::in, bool::in,
	pair(pred_proc_id)::in, int::in, maybe(pred_proc_id)::in, 
	hlds_goal::out, bool::out, pd_info::pd_info_di, 
	pd_info::pd_info_uo) is det.

deforest__create_deforest_goal(EarlierGoal, BetweenGoals, LaterGoal,
		FoldGoal0, NonLocals, RunModes, ProcPair, Size, 
		MaybeGeneralised, CallGoal, Optimized) -->
	pd_info_get_module_info(ModuleInfo0),
	pd_info_lookup_option(deforestation_vars_threshold, VarsOpt),
	(
		{ EarlierGoal = call(PredId1, ProcId1, Args1, _, _, _) - _ },
		{ LaterGoal = call(PredId2, ProcId2, _, _, _, _) - _ },

		( 
				% no threshold set.
			{ VarsOpt = int(-1) }
		;		
			%
			% Check that we're not creating a procedure
			% with a massive number of variables. We assume
			% that all the variables in the first called 
			% goal are present in the final version. If the
			% number of variables in the first called goal
			% plus the number of variables in BetweenGoals
			% is less than --deforestation-vars-threshold, go 
			% ahead and optimize.
			%
			{ module_info_pred_proc_info(ModuleInfo0, 
				PredId1, ProcId1, _, CalledProcInfo1) },
			{ proc_info_goal(CalledProcInfo1, CalledGoal1) },
			{ goal_util__goal_vars(CalledGoal1, GoalVars1) },
			{ set__to_sorted_list(GoalVars1, GoalVarsList1) },
			{ set__init(GoalVars2) },
			{ goal_util__goals_goal_vars(BetweenGoals, 
				GoalVars2, GoalVars3) },
			{ set__to_sorted_list(GoalVars3, GoalVarsList3) },
					
			{ list__length(GoalVarsList1, NumVars1) },
			{ list__length(GoalVarsList3, NumVars3) },
			{ NumVars is NumVars1 + NumVars3 },
			{ VarsOpt = int(MaxVars) },
			{ NumVars < MaxVars }
		)
	->
		%
		% Create the goal for the new predicate,
		% unfolding the first call.
		%

		pd_info_get_instmap(InstMap0),

		pd_info_get_proc_info(ProcInfo0),
		pd_debug__message("unfolding first call\n", []),

		deforest__unfold_call(no, no, PredId1, ProcId1, Args1, 
			EarlierGoal, UnfoldedCall, _),
		{ deforest__create_conj(UnfoldedCall, BetweenGoals,
			LaterGoal, NonLocals, DeforestGoal0) },
		{ set__to_sorted_list(NonLocals, NonLocalsList) },

		( { RunModes = yes } ->

			%
			% If we did a generalisation step when creating this
			% version, we need to modecheck to propagate through
			% the new insts. If this causes mode errors, don't
			% create the new version. This can happen if a 
			% procedure expected an input to be bound to a 
			% particular functor but the extra information
			% was generalised away.
			%
			pd_debug__message("running modes on deforest goal\n", 
				[]),
			pd_util__unique_modecheck_goal(DeforestGoal0,
				DeforestGoal, Errors1),
			pd_util__unique_modecheck_goal(FoldGoal0, FoldGoal,
				Errors2),
			{ list__append(Errors1, Errors2, Errors) }
		;	
			{ DeforestGoal = DeforestGoal0 },
			{ FoldGoal = FoldGoal0 },
			{ Errors = [] }
		),
		( { Errors = [] } -> 

			%
			% Create the new version.
			%

			pd_info__define_new_pred(DeforestGoal, 
				PredProcId, CallGoal),
			{ PredProcId = proc(PredId, _) },

			pd_info_get_module_info(ModuleInfo),

			{ predicate_name(ModuleInfo, PredId, PredName) },
			pd_debug__message("\nCreated predicate %s\n", 
				[s(PredName)]),
			{ CalledPreds = [proc(PredId1, ProcId2),
					proc(PredId2, ProcId2)] },
			pd_info_get_parent_versions(Parents0),
			
			pd_info_get_proc_info(ProcInfo1),
			{ proc_info_vartypes(ProcInfo1, VarTypes) },
			{ map__apply_to_list(NonLocalsList, 
				VarTypes, ArgTypes) },
			{ VersionInfo = version_info(FoldGoal, CalledPreds,
				NonLocalsList, ArgTypes, InstMap0, 
				0, 0, Parents0, MaybeGeneralised) },
			pd_info_get_global_term_info(TermInfo0),
			{ pd_term__update_global_term_info(TermInfo0, ProcPair, 
				PredProcId, Size, TermInfo) },
			pd_info_set_global_term_info(TermInfo),
			{ set__insert_list(Parents0, 
				[PredProcId | CalledPreds], Parents) },
			pd_info_set_parent_versions(Parents),
			pd_info__register_version(PredProcId, VersionInfo),

			% Run deforestation on the new predicate 
			% to do the folding.
			pd_info_get_unfold_info(UnfoldInfo),
			deforest__proc(PredProcId, CostDelta, SizeDelta),
			pd_info_set_unfold_info(UnfoldInfo),
			pd_info_incr_cost_delta(CostDelta),
			pd_info_incr_size_delta(SizeDelta),
			pd_info_set_parent_versions(Parents0),
			pd_info_get_pred_proc_id(proc(CurrPredId, CurrProcId)),
			pd_info_get_io_state(IO0),
			{ write_proc_progress_message("% Back in ", 
				CurrPredId, CurrProcId, ModuleInfo, 
				IO0, IO) },
			pd_info_set_io_state(IO),
			{ Optimized = yes }
		;
			pd_debug__message("Generalisation produced mode errors\n", []),
			{ CallGoal = LaterGoal },
			{ Optimized = no }
		),

		% The varset and vartypes fields were increased when
		% we unfolded the first call, but all the new variables
		% are only used in the new version, so it is safe to
		% reset the proc_info.
		pd_info_set_proc_info(ProcInfo0),
		pd_info_set_instmap(InstMap0)
	;
		pd_debug__message("vars threshold exceeded\n", []),
		{ Optimized = no },
		{ CallGoal = LaterGoal }
	).
		
%-----------------------------------------------------------------------------%

	% Create a goal to call a newly created version.
:- pred deforest__create_call_goal(pred_proc_id::in, version_info::in, 
		map(var, var)::in, tsubst::in, hlds_goal::out, 
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__create_call_goal(proc(PredId, ProcId), VersionInfo,
		Renaming, TypeSubn, Goal) -->
	{ VersionInfo = version_info(_, _, OldArgs, _, _, _, _, _, _) },
	pd_info_get_module_info(ModuleInfo),
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		CalledPredInfo, _CalledProcInfo) },
	{ pred_info_arg_types(CalledPredInfo, CalledTVarSet, _CalledExistQVars,
			ArgTypes0) },


		% Rename the arguments in the version.
	pd_info_get_proc_info(ProcInfo0),
	pd_info_get_pred_info(PredInfo0),

	{ proc_info_vartypes(ProcInfo0, VarTypes0) },
	{ proc_info_varset(ProcInfo0, VarSet0) },
	{ pred_info_typevarset(PredInfo0, TVarSet0) },

		% Rename the argument types using the current pred's tvarset.
	{ varset__merge_subst(TVarSet0, CalledTVarSet,
		TVarSet, TypeRenaming) },
	{ pred_info_set_typevarset(PredInfo0, TVarSet, PredInfo) },	
	pd_info_set_pred_info(PredInfo),
	{ term__apply_substitution_to_list(ArgTypes0, 
		TypeRenaming, ArgTypes1) },

	{ deforest__create_deforest_call_args(OldArgs, ArgTypes1, Renaming, 
		TypeSubn, Args, VarSet0, VarSet, VarTypes0, VarTypes) },
	{ proc_info_set_vartypes(ProcInfo0, VarTypes, ProcInfo1) },
	{ proc_info_set_varset(ProcInfo1, VarSet, ProcInfo) },
	pd_info_set_proc_info(ProcInfo),

		% Compute a goal_info.
	% { proc_info_argmodes(CalledProcInfo, argument_modes(_, ArgModes)) },
	{ instmap_delta_init_reachable(InstMapDelta) },	% YYY
	{ proc_info_interface_determinism(ProcInfo, Detism) },
	{ set__list_to_set(Args, NonLocals) },
	{ goal_info_init(NonLocals, InstMapDelta, Detism, GoalInfo) },

	{ pred_info_module(CalledPredInfo, PredModule) },
	{ pred_info_name(CalledPredInfo, PredName) },
	{ Goal = call(PredId, ProcId, Args, not_builtin, no, 
			qualified(PredModule, PredName)) - GoalInfo }.
	
:- pred deforest__create_deforest_call_args(list(var)::in, list(type)::in,
	map(var, var)::in, substitution::in, list(var)::out, varset::in,
	varset::out, map(var, type)::in, map(var, type)::out) is det.
	
deforest__create_deforest_call_args([], [], _, _, [], 
		VarSet, VarSet, VarTypes, VarTypes).
deforest__create_deforest_call_args([], [_|_], _, _, _, _, _, _, _) :-
	error("deforest__create_deforest_call_args: length mismatch").
deforest__create_deforest_call_args([_|_], [], _, _, _, _, _, _, _) :-
	error("deforest__create_deforest_call_args: length mismatch").
deforest__create_deforest_call_args([OldArg | OldArgs], [ArgType | ArgTypes],
		Renaming, TypeSubn, [Arg | Args], VarSet0, VarSet, 
		VarTypes0, VarTypes) :-
	( map__search(Renaming, OldArg, Arg0) ->
		Arg = Arg0,
		VarSet1 = VarSet0,
		VarTypes1 = VarTypes0
	;
		% The variable is local to the call. Create a fresh variable.
		varset__new_var(VarSet0, Arg, VarSet1),
		term__apply_substitution(ArgType, TypeSubn, ArgType1),
		map__det_insert(VarTypes0, Arg, ArgType1, VarTypes1)
	),
	deforest__create_deforest_call_args(OldArgs, ArgTypes, Renaming, 
		TypeSubn, Args, VarSet1, VarSet, VarTypes1, VarTypes).

%-----------------------------------------------------------------------------%

	% Combine the two goals to be deforested and the 
	% goals in between into a conjunction.
:- pred deforest__create_conj(hlds_goal::in, list(hlds_goal)::in, 
		hlds_goal::in, set(var)::in, hlds_goal::out) is det.

deforest__create_conj(EarlierGoal, BetweenGoals, LaterGoal, 
			NonLocals, FoldGoal) :-
	list__append([EarlierGoal | BetweenGoals], [LaterGoal],
		DeforestConj),
	goal_list_determinism(DeforestConj, Detism),
	goal_list_instmap_delta(DeforestConj, InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, Detism, ConjInfo0),

	% Give the conjunction a context so that the generated predicate
	% name points to the location of the first goal.
	EarlierGoal = _ - EarlierGoalInfo,
	goal_info_get_context(EarlierGoalInfo, EarlierContext),
	goal_info_set_context(ConjInfo0, EarlierContext, ConjInfo),	
	FoldGoal = conj(DeforestConj) - ConjInfo.

%-----------------------------------------------------------------------------%

	% "Round-off" some of the extra information that caused the
	% termination check to fail and/or the insts of the versions
	% not to match in an attempt to achieve folding.
:- pred deforest__try_generalisation(hlds_goal::in, list(hlds_goal)::in,
		hlds_goal::in, hlds_goal::in, set(var)::in, 
		pair(pred_proc_id)::in, int::in, 
		pred_proc_id::in, hlds_goal::out, bool::out,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__try_generalisation(EarlierGoal, BetweenGoals, LaterGoal, FoldGoal, 
		ConjNonLocals, ProcPair, Size, 
		CoveringPredProcId, Goal, Optimized) -->

	pd_debug__message("trying generalisation\n", []),
	pd_info_get_versions(VersionIndex),
	{ map__lookup(VersionIndex, CoveringPredProcId, Version) },
	pd_info_get_module_info(ModuleInfo),
	{ Version = version_info(VersionGoal, _, VersionArgs, 
			VersionArgTypes, VersionInstMap, _, _, _, _) },
	pd_info_get_versions(Versions),
	pd_info_get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	(
		{ pd_util__goals_match(ModuleInfo, VersionGoal, VersionArgs, 
			VersionArgTypes, FoldGoal, VarTypes, Renaming, _) }
	->
		deforest__do_generalisation(VersionArgs, Renaming,
			VersionInstMap, EarlierGoal, BetweenGoals, 
			LaterGoal, FoldGoal, ConjNonLocals, ProcPair,
			Size, CoveringPredProcId, Goal, Optimized)
	;
		% If the earlier goal is a generalisation of another
		% version, try matching against that. This happens
		% when attempting two deforestations in a row and
		% the first deforestation required generalisation.
		{ proc_info_varset(ProcInfo, VarSet) },
		{ deforest__match_generalised_version(ModuleInfo,
			VersionGoal, VersionArgs, VersionArgTypes,
			EarlierGoal, BetweenGoals, LaterGoal, ConjNonLocals,
			VarSet, VarTypes, Versions, Renaming) }
	->
		pd_debug__message("matched with generalised version\n", []),
		deforest__do_generalisation(VersionArgs, Renaming,
			VersionInstMap, EarlierGoal, BetweenGoals, 
			LaterGoal, FoldGoal, ConjNonLocals, ProcPair, 
			Size, CoveringPredProcId, Goal, Optimized)
	;
		pd_debug__message("goals don't match\n", []),
		{ Goal = LaterGoal },
		{ Optimized = no }
	).	

:- pred deforest__do_generalisation(list(var)::in, map(var, var)::in,
	instmap::in, hlds_goal::in, list(hlds_goal)::in, hlds_goal::in, 
	hlds_goal::in, set(var)::in, pair(pred_proc_id)::in, int::in, 
	pred_proc_id::in, hlds_goal::out, bool::out, 
	pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__do_generalisation(VersionArgs, Renaming, VersionInstMap, EarlierGoal, 
		BetweenGoals, LaterGoal, FoldGoal, ConjNonLocals, 
		ProcPair, Size, Generalised, Goal, Optimized) -->
	pd_debug__message("goals match, trying MSG\n", []),
	pd_info_get_module_info(ModuleInfo),
	pd_info_get_instmap(InstMap0),
	pd_info_get_proc_info(ProcInfo),
	{ proc_info_inst_table(ProcInfo, InstTable) },
	{ instmap__lookup_vars(VersionArgs, VersionInstMap, 
		VersionInsts) },
	{ pd_util__inst_list_size(InstTable, ModuleInfo, VersionInsts, 
		VersionInstSizes) },
	{ set__to_sorted_list(ConjNonLocals, ConjNonLocalsList) },
	(
		% Check whether we can do a most specific 
		% generalisation of insts of the non-locals.
		{ deforest__try_MSG(InstTable, ModuleInfo, VersionInstMap, 
			VersionArgs, Renaming, InstMap0, InstMap) },
		{ instmap__lookup_vars(ConjNonLocalsList, InstMap,
			ArgInsts) },
		{ pd_util__inst_list_size(InstTable, ModuleInfo, ArgInsts,
			NewInstSizes) },
		{ NewInstSizes < VersionInstSizes }
	->	
		pd_debug__message("MSG succeeded", []),
		pd_info_set_instmap(InstMap),
		deforest__create_deforest_goal(EarlierGoal, BetweenGoals, 
			LaterGoal, FoldGoal, ConjNonLocals, yes, 
			ProcPair, Size, yes(Generalised),
			Goal, Optimized)
	;
		pd_debug__message("MSG failed\n", []),
		{ Goal = LaterGoal },
		{ Optimized = no }
	),
	pd_info_set_instmap(InstMap0).

:- pred deforest__try_MSG(inst_table::in, module_info::in, instmap::in,
	list(var)::in, map(var, var)::in, instmap::in, instmap::out) is semidet.

deforest__try_MSG(_, _, _, [], _, InstMap, InstMap).
deforest__try_MSG(InstTable, ModuleInfo, VersionInstMap,
		[VersionArg | VersionArgs], Renaming, InstMap0, InstMap) :-
	instmap__lookup_var(VersionInstMap, VersionArg, VersionInst),
	(
		map__search(Renaming, VersionArg, Arg),
		instmap__lookup_var(InstMap0, Arg, VarInst),
		inst_MSG(VersionInst, VersionInstMap, VarInst, InstMap0,
			InstTable, ModuleInfo, Inst) 
	->
		instmap__set(InstMap0, Arg, Inst, InstMap1)
	;
		InstMap1 = InstMap0
	),
	deforest__try_MSG(InstTable, ModuleInfo, VersionInstMap, VersionArgs,
		Renaming, InstMap1, InstMap).

%-----------------------------------------------------------------------------%

	% If the global termination check and generalisation failed and
	% the first goal in the conjunction to be specialised is a 
	% generalisation of another version, try matching and generalising
	% using that (non-generalised) version.
	%
 	% This predicate maps the call to the generalised predicate back 
	% onto the non-generalised version. This makes the goal match
	% with the previous conjunction, so the generalisation can be
	% reapplied to the entire conjunction.
	%
	% XXX this only undoes one level of generalisation.
:- pred deforest__match_generalised_version(module_info::in, 
		hlds_goal::in, list(var)::in, list(type)::in, hlds_goal::in, 
		list(hlds_goal)::in, hlds_goal::in, set(var)::in, 
		varset::in, map(var, type)::in, 
		version_index::in, map(var, var)::out) is semidet.

deforest__match_generalised_version(ModuleInfo, VersionGoal, VersionArgs, 
		VersionArgTypes, FirstGoal, BetweenGoals, LastGoal, 
		ConjNonLocals, VarSet0, VarTypes0, Versions, Renaming) :-

	FirstGoal = call(FirstPredId, FirstProcId, FirstArgs, _, _, _) - _,

	%
	% Look up the version which the first goal calls.
	%
	map__search(Versions, proc(FirstPredId, FirstProcId), 
		FirstVersionInfo),
	FirstVersionInfo = version_info(FirstVersionGoal, _, FirstVersionArgs, 
			_,_,_,_,_, MaybeNonGeneralisedVersion),
	MaybeNonGeneralisedVersion = yes(NonGeneralisedPredProcId),
	map__from_corresponding_lists(FirstVersionArgs, 
			FirstArgs, FirstRenaming0),

	goal_util__goal_vars(FirstVersionGoal, FirstVersionVars0),
	set__to_sorted_list(FirstVersionVars0, FirstVersionVars),

	module_info_pred_proc_info(ModuleInfo, FirstPredId, FirstProcId,
		_, FirstProcInfo),
	proc_info_varset(FirstProcInfo, FirstVersionVarSet),	
	proc_info_vartypes(FirstProcInfo, FirstVersionVarTypes),

	goal_util__create_variables(FirstVersionVars, VarSet0, VarTypes0,
		FirstRenaming0, FirstVersionVarTypes, FirstVersionVarSet,
		VarSet, VarTypes, FirstRenaming),
	goal_util__must_rename_vars_in_goal(FirstVersionGoal, FirstRenaming,
			RenamedFirstVersionGoal),

	% 
	% Look up the version which was generalised to create the version
	% which the first goal calls.
	%
	NonGeneralisedPredProcId = proc(NonGeneralisedPredId, 
					NonGeneralisedProcId),
	goal_to_conj_list(VersionGoal, VersionGoalList),
	VersionGoalList = [call(NonGeneralisedPredId, NonGeneralisedProcId,
				_, _, _, _) - _ | _],

	%
	% Find a renaming from the argument variables of the generalised
	% version to the version which was generalised. 
	%
	map__search(Versions, NonGeneralisedPredProcId, 
		NonGeneralisedVersion),
	NonGeneralisedVersion = version_info(NonGeneralisedGoal, _, 
		NonGeneralisedArgs, NonGeneralisedArgTypes,_,_,_,_,_),
	pd_util__goals_match(ModuleInfo, NonGeneralisedGoal, 
		NonGeneralisedArgs, NonGeneralisedArgTypes,
		RenamedFirstVersionGoal, VarTypes, GeneralRenaming, 
		TypeRenaming),

	module_info_pred_info(ModuleInfo, NonGeneralisedPredId, 
		NonGeneralisedPredInfo),
	pred_info_arg_types(NonGeneralisedPredInfo, NonGeneralisedArgTypes),

	deforest__create_deforest_call_args(NonGeneralisedArgs, 
		NonGeneralisedArgTypes, GeneralRenaming, TypeRenaming,
		NewArgs, VarSet, _, VarTypes, _),

	% Only fill in as much as pd_util__goals_match actually looks at.
	goal_info_init(GoalInfo),
	NonGeneralFirstGoal = call(NonGeneralisedPredId, 
		NonGeneralisedProcId, NewArgs, not_builtin, 
		no, unqualified("")) - GoalInfo,
	deforest__create_conj(NonGeneralFirstGoal, BetweenGoals, LastGoal, 
		ConjNonLocals, GoalToMatch),

	%
	% Check whether the entire conjunction matches.
	%
	pd_util__goals_match(ModuleInfo, VersionGoal, VersionArgs, 
		VersionArgTypes, GoalToMatch, VarTypes, Renaming, _).

%-----------------------------------------------------------------------------%

	% Work out the nonlocals of a sub-conjunction from the non-locals of
	% the entire conjunction and the goals before and after the 
	% sub-conjunction. This is needed to ensure that the temporary 
	% list in double_append is found to be local to the conjunction 
	% and can be removed.
:- pred deforest__get_sub_conj_nonlocals(set(var)::in, deforest_info::in,
		list(hlds_goal)::in, list(hlds_goal)::in, 
		list(hlds_goal)::in, annotated_conj::in, set(var)::out) is det.

deforest__get_sub_conj_nonlocals(NonLocals0, DeforestInfo, 
		RevBeforeGoals, BeforeIrrelevant, AfterIrrelevant, 
		AfterGoals0, SubConjNonLocals) :-

	DeforestInfo = deforest_info(EarlierGoal, _, BetweenGoals, 
				LaterGoal, _, _),

	AddGoalNonLocals =
	    lambda([Goal::in, Vars0::in, Vars::out] is det, (
		Goal = _ - GoalInfo,
		goal_info_get_nonlocals(GoalInfo, GoalNonLocals),
		set__union(Vars0, GoalNonLocals, Vars)
	    )),

	list__foldl(AddGoalNonLocals, RevBeforeGoals, NonLocals0, NonLocals1),
	list__foldl(AddGoalNonLocals, BeforeIrrelevant,
		NonLocals1, NonLocals2),
	list__foldl(AddGoalNonLocals, AfterIrrelevant, NonLocals2, NonLocals3),
	assoc_list__keys(AfterGoals0, AfterGoals),
	list__foldl(AddGoalNonLocals, AfterGoals, NonLocals3, NonLocals),

	set__init(SubConjNonLocals0),
	list__foldl(AddGoalNonLocals, [EarlierGoal | BetweenGoals], 
		SubConjNonLocals0, SubConjNonLocals1),
	call(AddGoalNonLocals, LaterGoal, SubConjNonLocals1, 
		SubConjNonLocals2),
	set__intersect(NonLocals, SubConjNonLocals2, SubConjNonLocals).

%-----------------------------------------------------------------------------%

	% Attempt to move irrelevant goals out of the conjunction.
	% This does a safe re-ordering that is guaranteed not to require
	% rescheduling of the conjunction, since it does not reorder goals
	% that depend on each other. 
	% We favor moving goals backward to avoid removing tail recursion.
:- pred deforest__reorder_conj(deforest_info::in, deforest_info::out,
		list(hlds_goal)::out, list(hlds_goal)::out, 
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__reorder_conj(DeforestInfo0, DeforestInfo, 
		BeforeIrrelevant, AfterIrrelevant) -->
	pd_debug__message("Reordering conjunction\n", []),
	{ DeforestInfo0 = deforest_info(EarlierGoal, EarlierBranchInfo, 
			BetweenGoals0, LaterGoal, LaterBranchInfo,
			DeforestBranches) },

	pd_info_get_module_info(ModuleInfo),
	pd_info_lookup_bool_option(fully_strict, FullyStrict),

	deforest__move_goals(deforest__can_move_goal_backward, ModuleInfo, 
		FullyStrict, BetweenGoals0, [], RevBetweenGoals1, EarlierGoal, 
		[], RevBeforeIrrelevant),

	deforest__move_goals(deforest__can_move_goal_forward,
		ModuleInfo, FullyStrict, RevBetweenGoals1, 
		[], BetweenGoals, LaterGoal, [], AfterIrrelevant),

	{ list__reverse(RevBeforeIrrelevant, BeforeIrrelevant) },
	{ DeforestInfo = deforest_info(EarlierGoal, EarlierBranchInfo, 
			BetweenGoals, LaterGoal, LaterBranchInfo,
			DeforestBranches) }.

:- pred deforest__move_goals(can_move::can_move, module_info::in, bool::in, 
		hlds_goals::in, hlds_goals::in, hlds_goals::out, 
		hlds_goal::in, hlds_goals::in, hlds_goals::out,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__move_goals(_, _, _, [], Between, Between, _, Moved, Moved) --> [].
deforest__move_goals(CanMove, ModuleInfo, FullyStrict, 
		[BetweenGoal | RevBetweenGoals0], BetweenGoals0, 
		BetweenGoals, EndGoal, MovedGoals0, MovedGoals) -->
	( 
		{ call(CanMove, ModuleInfo, FullyStrict, BetweenGoal, 
			[EndGoal | BetweenGoals0]) }
	->
		{ BetweenGoals1 = BetweenGoals0 },
		{ MovedGoals1 = [BetweenGoal | MovedGoals0] }
	;
		{ BetweenGoals1 = [BetweenGoal | BetweenGoals0] },
		{ MovedGoals1 = MovedGoals0 }
	),
	deforest__move_goals(CanMove, ModuleInfo, FullyStrict, 
		RevBetweenGoals0, BetweenGoals1, BetweenGoals, 
		EndGoal, MovedGoals1, MovedGoals).

:- type can_move == pred(module_info, bool, hlds_goal, hlds_goals).
:- mode can_move :: (pred(in, in, in, in) is semidet).

	% Check all goals occurring later in the conjunction to
	% see if they depend on the current goal. A goal
	% depends on the current goal if any of the non-locals
	% of the later goal have their instantiatedness changed
	% by the current goal.
:- pred deforest__can_move_goal_forward(module_info::in, bool::in,
		hlds_goal::in, list(hlds_goal)::in) is semidet.

deforest__can_move_goal_forward(ModuleInfo, FullyStrict, ThisGoal, Goals) :-
	\+ (
		list__member(LaterGoal, Goals),
		\+ pd_util__can_reorder_goals(ModuleInfo, FullyStrict, 
			ThisGoal, LaterGoal)
	).

	% Check all goals occurring earlier in the conjunction to
	% see if the current goal depends on them. 
:- pred deforest__can_move_goal_backward(module_info::in, bool::in,
		hlds_goal::in, list(hlds_goal)::in) is semidet.

deforest__can_move_goal_backward(ModuleInfo, FullyStrict, ThisGoal, Goals) :-
	\+ (
		list__member(EarlierGoal, Goals),
		\+ pd_util__can_reorder_goals(ModuleInfo, FullyStrict, 
			EarlierGoal, ThisGoal)
	).

%-----------------------------------------------------------------------------%

	% Tack the second goal and the goals in between onto the end
	% of each branch of the first goal, unfolding the second goal
	% in the branches which have extra information about the arguments.
:- pred deforest__push_goal_into_goal(set(var)::in, set(int)::in,
		hlds_goal::in, hlds_goals::in, hlds_goal::in, hlds_goal::out, 
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__push_goal_into_goal(NonLocals, DeforestInfo, EarlierGoal, 
		BetweenGoals, LaterGoal, Goal) -->
	pd_info_get_instmap(InstMap0),
	{ EarlierGoal = EarlierGoalExpr - _ },
	( { EarlierGoalExpr = switch(Var1, CanFail1, Cases1, SM) } ->
		{ set__insert(NonLocals, Var1, CaseNonLocals) },
		deforest__append_goal_to_cases(Var1, Cases1, BetweenGoals, 
			LaterGoal, CaseNonLocals, 1, DeforestInfo, Cases),
		{ GoalExpr = switch(Var1, CanFail1, Cases, SM) }
	; { EarlierGoalExpr = if_then_else(Vars, Cond, Then0, Else0, SM) } ->
		pd_info_update_goal(Cond),
		{ Cond = _ - CondInfo },
		{ goal_info_get_nonlocals(CondInfo, CondNonLocals) },
		{ set__union(CondNonLocals, NonLocals, ThenNonLocals) },
		deforest__append_goal(Then0, BetweenGoals,
			LaterGoal, ThenNonLocals, 1, DeforestInfo, Then),
		pd_info_set_instmap(InstMap0),
		deforest__append_goal(Else0, BetweenGoals,
			LaterGoal, NonLocals, 2, DeforestInfo, Else),
		{ GoalExpr = if_then_else(Vars, Cond, Then, Else, SM) }
	; { EarlierGoalExpr = disj(Disjuncts0, SM) } ->
		deforest__append_goal_to_disjuncts(Disjuncts0, BetweenGoals, 
			LaterGoal, NonLocals, 1, DeforestInfo, Disjuncts),
		{ GoalExpr = disj(Disjuncts, SM) }
	;
		{ error("deforest__push_goal_into_goal") }
	),
	pd_info_set_instmap(InstMap0),
	{ goal_list_instmap_delta([EarlierGoal | BetweenGoals], Delta0) },
	{ LaterGoal = _ - LaterInfo },
	{ goal_info_get_instmap_delta(LaterInfo, Delta1) },
	{ instmap_delta_apply_instmap_delta(Delta0, Delta1, Delta) },
	{ goal_list_determinism([EarlierGoal | BetweenGoals], Detism0) },
	{ goal_info_get_determinism(LaterInfo, Detism1) },
	{ det_conjunction_detism(Detism0, Detism1, Detism) },
	{ goal_info_init(NonLocals, Delta, Detism, GoalInfo) },
	{ Goal2 = GoalExpr - GoalInfo },

	pd_info_get_module_info(ModuleInfo),
	{ module_info_globals(ModuleInfo, Globals) },
	{ simplify__find_simplifications(no, Globals, Simplifications0) },

	% Be a bit more aggressive with common structure elimination.
	% This helps achieve folding in some cases.
	{ Simplifications = [extra_common_struct | Simplifications0] },
	pd_util__simplify_goal(Simplifications, Goal2, Goal3),
	pd_info_set_instmap(InstMap0),

	% Perform any folding which may now be possible.
	deforest__goal(Goal3, Goal),
	pd_info_set_instmap(InstMap0).

:- pred deforest__append_goal_to_disjuncts(hlds_goals::in, hlds_goals::in,
	hlds_goal::in, set(var)::in, int::in, set(int)::in, 
	hlds_goals::out, pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__append_goal_to_disjuncts([], _, _, _, _, _, []) --> [].
deforest__append_goal_to_disjuncts([Goal0 | Goals0], BetweenGoals, 
		GoalToAppend, NonLocals, CurrBranch, Branches, 
		[Goal | Goals]) -->
	pd_info_get_instmap(InstMap0),
	deforest__append_goal(Goal0, BetweenGoals, GoalToAppend,
		NonLocals, CurrBranch, Branches, Goal),
	{ NextBranch is CurrBranch + 1 },
	pd_info_set_instmap(InstMap0),
	deforest__append_goal_to_disjuncts(Goals0, BetweenGoals, GoalToAppend, 
		NonLocals, NextBranch, Branches, Goals).

:- pred deforest__append_goal_to_cases(var::in, list(case)::in, hlds_goals::in,
	hlds_goal::in, set(var)::in, int::in, set(int)::in,
	list(case)::out, pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__append_goal_to_cases(_, [], _, _, _, _, _, []) --> [].
deforest__append_goal_to_cases(Var, [case(ConsId, IMDelta, Goal0) | Cases0], 
		BetweenGoals, GoalToAppend, NonLocals, CurrCase, Branches, 
		[case(ConsId, IMDelta, Goal) | Cases]) -->
	pd_info_get_instmap(InstMap0),
	pd_info_apply_instmap_delta(IMDelta),
	deforest__append_goal(Goal0, BetweenGoals, 
		GoalToAppend, NonLocals, CurrCase, Branches, Goal),
	{ NextCase is CurrCase + 1 },
	pd_info_set_instmap(InstMap0),
	deforest__append_goal_to_cases(Var, Cases0, BetweenGoals, GoalToAppend,
		NonLocals, NextCase, Branches, Cases).

:- pred deforest__append_goal(hlds_goal::in, hlds_goals::in, 
	hlds_goal::in, set(var)::in, int::in, set(int)::in,
	hlds_goal::out, pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__append_goal(Goal0, BetweenGoals, GoalToAppend0,
		NonLocals0, CurrBranch, Branches, Goal) -->
	( { set__member(CurrBranch, Branches) } ->
		% Unfold the call.
		pd_info_get_instmap(InstMap0),
		pd_info_foldl(pd_info_update_goal, [Goal0 | BetweenGoals]),
		deforest__goal(GoalToAppend0, GoalToAppend),
		pd_info_set_instmap(InstMap0)
	;
		{ GoalToAppend = GoalToAppend0 }
	),
	{ goal_to_conj_list(Goal0, GoalList0) },
	{ goal_to_conj_list(GoalToAppend, GoalListToAppend) },
	{ list__condense([GoalList0, BetweenGoals, GoalListToAppend], Goals) },

	{ goal_list_nonlocals(Goals, SubNonLocals) },
	{ set__intersect(NonLocals0, SubNonLocals, NonLocals) },
	{ goal_list_instmap_delta(Goals, Delta) },
	{ goal_list_determinism(Goals, Detism) },
	{ goal_info_init(NonLocals, Delta, Detism, GoalInfo) }, 
	{ Goal = conj(Goals) - GoalInfo }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred deforest__call(pred_id::in, proc_id::in, list(var)::in, sym_name::in,
		hlds_goal::in, hlds_goal::out, 
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__call(PredId, ProcId, Args, SymName, Goal0, Goal) -->
	pd_info_get_proc_arg_info(ProcArgInfos),
	pd_info_get_module_info(ModuleInfo),
	pd_info_get_instmap(InstMap),
	{ unqualify_name(SymName, Name) },
	{ list__length(Args, Arity) },
	{ Goal0 = GoalExpr0 - GoalInfo0 },
	{ goal_info_get_context(GoalInfo0, Context) },

	pd_info_get_local_term_info(LocalTermInfo0),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_get_markers(PredInfo, Markers) },
	pd_info_get_proc_info(ProcInfo),
	{ proc_info_inst_table(ProcInfo, InstTable) },
	( 
		{ \+ check_marker(Markers, no_inline) },
		{ map__search(ProcArgInfos, proc(PredId, ProcId), 
			ProcArgInfo) },
		{ ProcArgInfo = pd_branch_info(_, LeftArgs, _) },
		{ set__member(LeftArg, LeftArgs) },
		{ list__index1_det(Args, LeftArg, Arg) },
		{ instmap__lookup_var(InstMap, Arg, ArgInst) },
		{ inst_is_bound_to_functors(ArgInst, InstMap, InstTable,
			ModuleInfo, [_]) }
	->
		pd_debug__message(Context, 
			"Found extra information for call to %s/%i\n", 
			[s(Name), i(Arity)]), 
		(
			{ pd_term__local_check(InstTable, ModuleInfo, Goal0,
				InstMap, LocalTermInfo0, LocalTermInfo) }
		->
			pd_debug__message("Local termination check succeeded\n",
				[]),
			pd_info_set_local_term_info(LocalTermInfo),
			deforest__unfold_call(yes, yes, PredId, ProcId, 
				Args, Goal0, Goal1, Optimized),
			( { Optimized = yes } ->
				deforest__goal(Goal1, Goal)
			;
				{ Goal = Goal1 }
			),
			pd_info_set_local_term_info(LocalTermInfo0)
		;
			pd_debug__message("Local termination check failed\n", 
				[]),
			{ Goal = GoalExpr0 - GoalInfo0 }
		)
	;
		pd_debug__message(Context, 
			"No extra information for call to %s/%i\n",
			[s(Name), i(Arity)]),
		{ Goal = Goal0 }
	).

:- pred deforest__unfold_call(bool::in, bool::in, pred_id::in, proc_id::in, 
		list(var)::in, hlds_goal::in, hlds_goal::out, bool::out,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

deforest__unfold_call(CheckImprovement, CheckVars, PredId, ProcId, Args, 
		Goal0, Goal, Optimized) -->
	pd_info_lookup_option(deforestation_vars_threshold, VarsOpt),
	pd_info_get_proc_info(ProcInfo0),
	{ proc_info_varset(ProcInfo0, VarSet0) },
	{ varset__vars(VarSet0, Vars) },
	{ list__length(Vars, NumVars) },
	( 
		%
		% Check that we haven't already got too many variables.
		%
		(
			{ CheckVars = no }
		;
			{ VarsOpt = int(-1) }
		;
			{ VarsOpt = int(MaxVars) },
			{ NumVars < MaxVars }
		)
	->
		pd_info_get_pred_info(PredInfo0),
		pd_info_get_module_info(ModuleInfo0),
		{ module_info_pred_proc_info(ModuleInfo0, 
			PredId, ProcId, CalledPredInfo, CalledProcInfo) },
		{ pred_info_typevarset(PredInfo0, TypeVarSet0) },
		{ pred_info_get_univ_quant_tvars(PredInfo0, UnivQVars) },
		{ proc_info_vartypes(ProcInfo0, VarTypes0) },
		{ proc_info_typeinfo_varmap(ProcInfo0, TypeInfoVarMap0) },
		{ inlining__do_inline_call(UnivQVars, Args, CalledPredInfo,
			CalledProcInfo, VarSet0, VarSet, VarTypes0, VarTypes,
			TypeVarSet0, TypeVarSet, TypeInfoVarMap0, 
			TypeInfoVarMap, Goal1) },
		{ pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo) },
		{ proc_info_set_varset(ProcInfo0, VarSet, ProcInfo1) },
		{ proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo2) },
		{ proc_info_set_typeinfo_varmap(ProcInfo2, TypeInfoVarMap, ProcInfo) },
		pd_info_set_pred_info(PredInfo),
		pd_info_set_proc_info(ProcInfo),

		{ pd_cost__goal(Goal1, OriginalCost) },
		pd_info_get_cost_delta(CostDelta0),
		pd_info_get_size_delta(SizeDelta0),
		pd_info_get_changed(Changed0),

			% update the quantification if not all the output
			% arguments are used.
		{ Goal1 = _ - GoalInfo1 },
		{ goal_info_get_nonlocals(GoalInfo1, NonLocals1) },
		{ set__list_to_set(Args, NonLocals) },
		( { \+ set__equal(NonLocals1, NonLocals) } ->
			pd_util__requantify_goal(Goal1, NonLocals, Goal2)
		;
			{ Goal2 = Goal1 }
		),

			% Push the extra information from the call 
			% through the goal.
		pd_debug__message("Running unique modes\n", []),
		{ proc_info_arglives(CalledProcInfo, ModuleInfo0, ArgLives) },
		{ get_live_vars(Args, ArgLives, LiveVars0) },
		{ set__list_to_set(LiveVars0, LiveVars1) },
		{ set__intersect(NonLocals, LiveVars1, LiveVars) },
		pd_util__unique_modecheck_goal(LiveVars, Goal2, Goal3, Errors),

		( { Errors = [] } ->
			{ Optimized0 = yes }	
		;
			% This can happen because common.m does not
			% maintain unique mode correctness. This should
			% eventually be fixed.
			{ Optimized0 = no }
		),

		pd_debug__message("Running simplify\n", []),
		pd_info_get_module_info(ModuleInfo),
		{ module_info_globals(ModuleInfo, Globals) },
		{ simplify__find_simplifications(no, Globals,
			Simplifications) },
		pd_util__simplify_goal(Simplifications, Goal3, Goal4),

		pd_info_get_cost_delta(CostDelta1),
		{ CostDelta is CostDelta1 - CostDelta0 },
		{ goal_size(Goal4, GoalSize) },
		{ pd_cost__call(CallCost) },
		{ SizeDelta = GoalSize - CallCost },
		pd_info_lookup_option(deforestation_cost_factor, FactorOpt),
		( 
			{ Optimized0 = yes },
			( 
				{ CheckImprovement = no }
			;
				{ CheckImprovement = yes },
				( { deforest__is_simple_goal(Goal3) } ->
					{ true }
				;
					{ FactorOpt = int(Factor) },
					{ deforest__check_improvement(Factor, 
						GoalSize, OriginalCost, 
						CostDelta) }
				)
			)
		->
			{ Goal = Goal4 },
			pd_debug__message("inlined: cost(%i) size(%i)\n", 
				[i(CostDelta), i(SizeDelta)]),
			pd_info_incr_size_delta(SizeDelta),
			pd_info_set_changed(yes),
			{ Goal0 = _ - GoalInfo0 },
			{ goal_info_get_determinism(GoalInfo0, Det0) },
			{ Goal = _ - GoalInfo },
			{ goal_info_get_determinism(GoalInfo, Det) },

			% Rerun determinism analysis later if
			% the determinism of any of the sub-goals
			% changes - this avoids problems with inlining
			% erroneous predicates.
			( { Det = Det0 } ->
				[]
			;
				pd_info_set_rerun_det(yes)
			),

			{ Optimized = yes }
		;
			pd_debug__message("not enough improvement - not inlining: cost(%i) size(%i)\n",
				[i(CostDelta), i(SizeDelta)]),
			pd_info_set_pred_info(PredInfo0),
			pd_info_set_proc_info(ProcInfo0),
			pd_info_set_size_delta(SizeDelta0),
			pd_info_set_cost_delta(CostDelta0),
			pd_info_set_changed(Changed0),
			{ Goal = Goal0 },
			{ Optimized = no }
		)
	;
		pd_debug__message("too many variables - not inlining\n", []),
		{ Goal = Goal0 },
		{ Optimized = no }
	).
	
%-----------------------------------------------------------------------------%

:- pred deforest__is_simple_goal_list(list(hlds_goal)::in) is semidet.

deforest__is_simple_goal_list([]).
deforest__is_simple_goal_list([Goal | Goals]) :-
	deforest__is_simple_goal(Goal),
	deforest__is_simple_goal_list(Goals).

:- pred deforest__is_simple_goal(hlds_goal::in) is semidet.

deforest__is_simple_goal(Goal - _) :-
	(
		goal_is_atomic(Goal)
	;
		Goal = not(Goal1),	
		% Handle a call or builtin + tests on the output.
		goal_to_conj_list(Goal1, GoalList1),
		deforest__is_simple_goal_list(GoalList1)
	).

%-----------------------------------------------------------------------------%

	% Very rough heuristics for checking improvement. This should lean
	% towards allowing optimizations.

:- pred deforest__check_improvement(int::in, int::in, 
		int::in, int::in) is semidet.

deforest__check_improvement(_Factor, Size, OriginalCost, CostDelta) :-

	( Size =< 5 ->
			% For small increases in size, 
			% accept any amount of optimization.
		CostDelta > 0
	;
		PercentChange is CostDelta * 100 // OriginalCost,
		PercentChange >= 5
	).

:- pred deforest__check_deforestation_improvement(int::in, 
		int::in, int::in) is semidet.

deforest__check_deforestation_improvement(Factor, CostDelta, SizeChange) :-
	( SizeChange =< 5 ->
			% For small increases in size, 
			% accept any amount of optimization.
		CostDelta > 0
	;
			% Accept the optimization if we save the equivalent 
			% of a heap increment per 3 extra atomic goals.
			% Note that folding is heavily rewarded by pd_cost.m, 
			% so this isn't very restrictive if a fold occurs.
		pd_cost__heap_incr(HeapCost),		
		ExpectedCostDelta is 1000 * HeapCost * SizeChange // 3,
		FudgedCostDelta is CostDelta * Factor,
		FudgedCostDelta >= ExpectedCostDelta
	).
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
