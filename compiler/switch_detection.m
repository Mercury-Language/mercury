%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Switch detection - when a disjunction contains disjuncts that unify the
% same input variable with different function symbols, replace (part of)
% the disjunction with a switch.
%
% Main author: fjh.
%
%-----------------------------------------------------------------------------%

:- module check_hlds__switch_detection.

:- interface.

:- import_module hlds__hlds_goal, hlds__hlds_module, hlds__hlds_pred.
:- import_module parse_tree__prog_data.
:- import_module bool, io, list.

:- pred detect_switches(module_info::in, module_info::out,
	io__state::di, io__state::uo) is det.

:- pred detect_switches_in_proc(proc_id::in, pred_id::in,
	module_info::in, module_info::out) is det.

	% find_bind_var(Var, ProcessUnify, Goal0, Goals, Subst0, Subst,
	%		Result0, Result, FoundDeconstruct):
	% 	Used by both switch_detection and cse_detection.
	%	Searches through `Goal0' looking for the first deconstruction
	%	unification with `Var' or an alias of `Var'.
	%	If a deconstruction unification of the variable is found,
	%	`ProcessUnify' is called to handle it and searching is stopped.
	%	If not, `Result' is set to `Result0'.
:- pred find_bind_var(prog_var::in,
	process_unify(Result, Info)::in(process_unify),
	hlds_goal::in, hlds_goal::out, Result::in, Result::out,
	Info::in, Info::out, bool::out) is det.

:- type process_unify(Result, Info) ==
	pred(prog_var, hlds_goal, list(hlds_goal), Result, Result, Info, Info).
:- inst process_unify = (pred(in, in, out, in, out, in, out) is det).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_goal, hlds__hlds_data, parse_tree__prog_data.
:- import_module hlds__instmap, check_hlds__inst_match.
:- import_module check_hlds__modes, check_hlds__mode_util.
:- import_module check_hlds__type_util, check_hlds__det_util.
:- import_module hlds__passes_aux, term.
:- import_module char, int, assoc_list, map, set, std_util, require.

%-----------------------------------------------------------------------------%

	% Traverse the module structure, calling `detect_switches_in_goal'
	% for each procedure body.

detect_switches(ModuleInfo0, ModuleInfo1) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	detect_switches_in_preds(PredIds, ModuleInfo0, ModuleInfo1).

:- pred detect_switches_in_preds(list(pred_id), module_info, module_info,
	io__state, io__state).
:- mode detect_switches_in_preds(in, in, out, di, uo) is det.

detect_switches_in_preds([], ModuleInfo, ModuleInfo) --> [].
detect_switches_in_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	detect_switches_in_pred(PredId, PredInfo, ModuleInfo0, ModuleInfo1),
	detect_switches_in_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred detect_switches_in_pred(pred_id, pred_info, module_info, module_info,
	io__state, io__state).
:- mode detect_switches_in_pred(in, in, in, out, di, uo) is det.

detect_switches_in_pred(PredId, PredInfo0, ModuleInfo0, ModuleInfo) -->
	{ pred_info_non_imported_procids(PredInfo0, ProcIds) },
	( { ProcIds \= [] } ->
		write_pred_progress_message("% Detecting switches in ", PredId,
			ModuleInfo0)
	;
		[]
	),
	{ detect_switches_in_procs(ProcIds, PredId, ModuleInfo0, ModuleInfo) }.

:- pred detect_switches_in_procs(list(proc_id), pred_id,
	module_info, module_info).
:- mode detect_switches_in_procs(in, in, in, out) is det.

detect_switches_in_procs([], _PredId, ModuleInfo, ModuleInfo).
detect_switches_in_procs([ProcId | ProcIds], PredId, ModuleInfo0, ModuleInfo) :-
	detect_switches_in_proc(ProcId, PredId, ModuleInfo0, ModuleInfo1),
	detect_switches_in_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

detect_switches_in_proc(ProcId, PredId, ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

		% To process each ProcInfo, we get the goal,
		% initialize the instmap based on the modes of the head vars,
		% and pass these to `detect_switches_in_goal'.
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_vartypes(ProcInfo0, VarTypes),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0),
	detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo0, Goal),

	proc_info_set_goal(ProcInfo0, Goal, ProcInfo),
	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__det_update(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo).

%-----------------------------------------------------------------------------%

	% Given a goal, and the instmap on entry to that goal,
	% replace disjunctions with switches whereever possible.

:- pred detect_switches_in_goal(hlds_goal, instmap, map(prog_var, type),
	module_info, hlds_goal).
:- mode detect_switches_in_goal(in, in, in, in, out) is det.

detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo, Goal) :-
	detect_switches_in_goal_1(Goal0, InstMap0, VarTypes, ModuleInfo,
		Goal, _InstMap).

	% This version is the same as the above except that it returns
	% the resulting instmap on exit from the goal, which is
	% computed by applying the instmap delta specified in the
	% goal's goalinfo.

:- pred detect_switches_in_goal_1(hlds_goal, instmap, map(prog_var, type),
	module_info, hlds_goal, instmap).
:- mode detect_switches_in_goal_1(in, in, in, in, out, out) is det.

detect_switches_in_goal_1(Goal0 - GoalInfo, InstMap0, VarTypes, ModuleInfo,
		Goal - GoalInfo, InstMap) :-
	detect_switches_in_goal_2(Goal0, GoalInfo, InstMap0,
		VarTypes, ModuleInfo, Goal),
	update_instmap(Goal0 - GoalInfo, InstMap0, InstMap).

	% Here we process each of the different sorts of goals.

:- pred detect_switches_in_goal_2(hlds_goal_expr, hlds_goal_info, instmap,
		map(prog_var, type), module_info, hlds_goal_expr).
:- mode detect_switches_in_goal_2(in, in, in, in, in, out) is det.

detect_switches_in_goal_2(disj(Goals0), GoalInfo, InstMap0,
		VarTypes, ModuleInfo, Goal) :-
	( Goals0 = [] ->
		Goal = disj([])
	;
		goal_info_get_nonlocals(GoalInfo, NonLocals),
		set__to_sorted_list(NonLocals, NonLocalsList),
		detect_switches_in_disj(NonLocalsList, Goals0, GoalInfo,
			InstMap0, VarTypes, NonLocalsList, ModuleInfo,
			[], Goal)
	).

detect_switches_in_goal_2(conj(Goals0), _GoalInfo, InstMap0,
		VarTypes, ModuleInfo, conj(Goals)) :-
	detect_switches_in_conj(Goals0, InstMap0, VarTypes, ModuleInfo, Goals).

detect_switches_in_goal_2(par_conj(Goals0), _GoalInfo, InstMap0,
		VarTypes, ModuleInfo, par_conj(Goals)) :-
	detect_switches_in_par_conj(Goals0, InstMap0, VarTypes,
		ModuleInfo, Goals).

detect_switches_in_goal_2(not(Goal0), _GoalInfo, InstMap0,
		VarTypes, ModuleInfo, not(Goal)) :-
	detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo, Goal).

detect_switches_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0),
		_GoalInfo, InstMap0, VarTypes, ModuleInfo,
		if_then_else(Vars, Cond, Then, Else)) :-
	detect_switches_in_goal_1(Cond0, InstMap0, VarTypes, ModuleInfo, Cond,
		InstMap1),
	detect_switches_in_goal(Then0, InstMap1, VarTypes, ModuleInfo, Then),
	detect_switches_in_goal(Else0, InstMap0, VarTypes, ModuleInfo, Else).

detect_switches_in_goal_2(some(Vars, CanRemove, Goal0), _GoalInfo, InstMap0,
		VarTypes, ModuleInfo, some(Vars, CanRemove, Goal)) :-
	detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo, Goal).

detect_switches_in_goal_2(generic_call(A,B,C,D), _, _, _, _,
		generic_call(A,B,C,D)).

detect_switches_in_goal_2(call(A,B,C,D,E,F), _, _, _, _,
		call(A,B,C,D,E,F)).

detect_switches_in_goal_2(unify(A,RHS0,C,D,E), __GoalInfo, InstMap0,
		VarTypes, ModuleInfo, unify(A,RHS,C,D,E)) :-
	(
		RHS0 = lambda_goal(PredOrFunc, EvalMethod, FixModes,
			NonLocals, Vars, Modes, Det, Goal0)
	->
		% we need to insert the initial insts for the lambda
		% variables in the instmap before processing the lambda goal
		instmap__pre_lambda_update(ModuleInfo, 
			Vars, Modes, InstMap0, InstMap1),
		detect_switches_in_goal(Goal0, InstMap1, VarTypes, ModuleInfo,
			Goal),
		RHS = lambda_goal(PredOrFunc, EvalMethod, FixModes,
			NonLocals, Vars, Modes, Det, Goal)
	;
		RHS = RHS0
	).

detect_switches_in_goal_2(switch(Var, CanFail, Cases0), _, InstMap,
		VarTypes, ModuleInfo, switch(Var, CanFail, Cases)) :-
	detect_switches_in_cases(Cases0, InstMap, VarTypes, ModuleInfo, Cases).

detect_switches_in_goal_2(foreign_proc(A,B,C,D,E,F,G), _, _, _, _,
		foreign_proc(A,B,C,D,E,F,G)).
detect_switches_in_goal_2(shorthand(_), _, _, _, _, _) :-
	% these should have been expanded out by now
	error("detect_switches_in_goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

	% This is the interesting bit - we've found a non-empty
	% disjunction, and we've got a list of the non-local variables
	% of that disjunction. Now for each non-local variable, we
	% check whether there is a partition of the disjuncts such that
	% each group of disjunctions can only succeed if the variable
	% is bound to a different functor.

:- type cases == map(cons_id, list(hlds_goal)).

:- type sorted_case_list == list(case).
	% the sorted_case_list should always be sorted on cons_id -
	% `delete_unreachable_cases' relies on this.

:- type again ---> again(prog_var, list(hlds_goal), sorted_case_list).

:- pred detect_switches_in_disj(list(prog_var), list(hlds_goal), hlds_goal_info,
	instmap, map(prog_var, type), list(prog_var), module_info,
	list(again), hlds_goal_expr).
:- mode detect_switches_in_disj(in, in, in, in, in, in, in, in, out) is det.

detect_switches_in_disj([Var | Vars], Goals0, GoalInfo, InstMap,
		VarTypes, AllVars, ModuleInfo, Again0, Goal) :-
	% can we do at least a partial switch on this variable?
	(
		instmap__lookup_var(InstMap, Var, VarInst0),
		inst_is_bound(ModuleInfo, VarInst0),
		partition_disj(Goals0, Var, GoalInfo, Left, CasesList)
	->
		%
		% A switch needs to have at least two cases.
		%
		% But, if there is a complete one-case switch
		% for a goal, we must leave it as a disjunction
		% rather than doing an incomplete switch on a
		% different variable, because otherwise we might
		% get determinism analysis wrong.  (The complete
		% one-case switch may be decomposable into other
		% complete sub-switches on the functor's arguments)
		%
		(
			% are there any disjuncts that are not part of the
			% switch?
			Left = []
		->
			( CasesList = [_, _ | _] ->
				cases_to_switch(CasesList, Var, VarTypes,
					GoalInfo, InstMap, ModuleInfo,
					Goal)
			;
				detect_sub_switches_in_disj(Goals0, InstMap,
					VarTypes, ModuleInfo, Goals),
				Goal = disj(Goals)
			)
		;
			% insert this switch into the list of incomplete
			% switches only if it has at least two cases
			%
			( CasesList = [_, _ | _] ->
				Again1 = [again(Var, Left, CasesList) | Again0]
			;
				Again1 = Again0
			),
			% try to find a switch
			detect_switches_in_disj(Vars, Goals0, GoalInfo,
				InstMap, VarTypes, AllVars, ModuleInfo,
				Again1, Goal)
		)
	;
		detect_switches_in_disj(Vars, Goals0, GoalInfo, InstMap,
			VarTypes, AllVars, ModuleInfo, Again0, Goal)
	).
detect_switches_in_disj([], Goals0, GoalInfo, InstMap,
		VarTypes, AllVars, ModuleInfo, AgainList0, disj(Goals)) :-
	(
		AgainList0 = [],
		detect_sub_switches_in_disj(Goals0, InstMap, VarTypes,
			ModuleInfo, Goals)
	;
		AgainList0 = [Again | AgainList1],
		select_best_switch(AgainList1, Again, BestAgain),
		BestAgain = again(Var, Left0, CasesList),
		cases_to_switch(CasesList, Var, VarTypes, GoalInfo, InstMap,
			ModuleInfo, SwitchGoal),
		detect_switches_in_disj(AllVars, Left0, GoalInfo, InstMap,
			VarTypes, AllVars, ModuleInfo, [], Left),
		goal_to_disj_list(Left - GoalInfo, LeftList),
		Goals = [SwitchGoal - GoalInfo | LeftList]
	).

:- pred select_best_switch(list(again), again, again).
:- mode select_best_switch(in, in, out) is det.

select_best_switch([], BestAgain, BestAgain).
select_best_switch([Again | AgainList], BestAgain0, BestAgain) :-
	(
		Again = again(_, _, CasesList),
		BestAgain0 = again(_, _, BestCasesList),
		list__length(CasesList, Length),
		list__length(BestCasesList, BestLength),
		Length < BestLength
	->
		BestAgain1 = BestAgain0
	;
		BestAgain1 = Again
	),
	select_best_switch(AgainList, BestAgain1, BestAgain).

:- pred detect_sub_switches_in_disj(list(hlds_goal), instmap,
		map(prog_var, type), module_info, list(hlds_goal)).
:- mode detect_sub_switches_in_disj(in, in, in, in, out) is det.

detect_sub_switches_in_disj([], _InstMap, _VarTypes, _ModuleInfo, []).
detect_sub_switches_in_disj([Goal0 | Goals0], InstMap, VarTypes, ModuleInfo,
		[Goal | Goals]) :-
	detect_switches_in_goal(Goal0, InstMap, VarTypes, ModuleInfo, Goal),
	detect_sub_switches_in_disj(Goals0, InstMap, VarTypes, ModuleInfo,
		Goals).

:- pred detect_switches_in_cases(list(case), instmap, map(prog_var, type),
		module_info, list(case)).
:- mode detect_switches_in_cases(in, in, in, in, out) is det.

detect_switches_in_cases([], _InstMap, _VarTypes, _ModuleInfo, []).
detect_switches_in_cases([Case0 | Cases0], InstMap, VarTypes, ModuleInfo,
		[Case | Cases]) :-
	Case0 = case(Functor, Goal0),
	detect_switches_in_goal(Goal0, InstMap, VarTypes, ModuleInfo, Goal),
	Case = case(Functor, Goal),
	detect_switches_in_cases(Cases0, InstMap, VarTypes, ModuleInfo, Cases).

:- pred detect_switches_in_par_conj(list(hlds_goal), instmap,
		map(prog_var, type), module_info, list(hlds_goal)).
:- mode detect_switches_in_par_conj(in, in, in, in, out) is det.

detect_switches_in_par_conj([], _InstMap, _VarTypes, _ModuleInfo, []).
detect_switches_in_par_conj([Goal0 | Goals0], InstMap, VarTypes, ModuleInfo,
		[Goal | Goals]) :-
	detect_switches_in_goal(Goal0, InstMap, VarTypes, ModuleInfo, Goal),
	detect_switches_in_par_conj(Goals0, InstMap, VarTypes,
		ModuleInfo, Goals).

:- pred detect_switches_in_conj(list(hlds_goal), instmap, map(prog_var, type),
	module_info, list(hlds_goal)).
:- mode detect_switches_in_conj(in, in, in, in, out) is det.

detect_switches_in_conj([], _InstMap, _VarTypes, _ModuleInfo, []).
detect_switches_in_conj([Goal0 | Goals0], InstMap0, VarTypes, ModuleInfo,
		[Goal | Goals]) :-
	detect_switches_in_goal_1(Goal0, InstMap0, VarTypes, ModuleInfo, Goal,
		InstMap1),
	detect_switches_in_conj(Goals0, InstMap1, VarTypes, ModuleInfo, Goals).

%-----------------------------------------------------------------------------%

	% partition_disj(Goals, Var, GoalInfo, VarTypes, ModuleInfo,
	%	Left, Cases):
	% Attempts to partition the disjunction `Goals' into a switch on `Var'.
	% If at least partially successful, returns the resulting `Cases', with
	% any disjunction goals not fitting into the switch in Left.

	% Given the list of goals in a disjunction, and an input variable
	% to switch on, we attempt to partition the goals into a switch.
	% For each constructor id, we record the list of disjuncts
	% which unify the variable with that constructor.
	% We partition the goals by abstractly interpreting the unifications
	% at the start of each disjunction, to build up a substitution.

:- pred partition_disj(list(hlds_goal), prog_var, hlds_goal_info,
		list(hlds_goal), sorted_case_list).
:- mode partition_disj(in, in, in, out, out) is semidet.

partition_disj(Goals0, Var, GoalInfo, Left, CasesList) :-
	map__init(Cases0),
	partition_disj_trial(Goals0, Var, [], Left, Cases0, Cases),
	map__to_assoc_list(Cases, CasesAssocList),
	CasesAssocList \= [], % there must be at least one case
	fix_case_list(CasesAssocList, GoalInfo, CasesList).

:- pred partition_disj_trial(list(hlds_goal), prog_var,
	list(hlds_goal), list(hlds_goal), cases, cases).
:- mode partition_disj_trial(in, in, in, out, in, out) is det.

partition_disj_trial([], _Var, Left, Left, Cases, Cases).
partition_disj_trial([Goal0 | Goals], Var, Left0, Left, Cases0, Cases) :-
	find_bind_var(Var, find_bind_var_for_switch_in_deconstruct,
		Goal0, Goal, no, MaybeFunctor, unit, _, _),
	(
		MaybeFunctor = yes(Functor),
		Left1 = Left0,
		( map__search(Cases0, Functor, DisjList0) ->
			DisjList1 = [Goal | DisjList0],
			map__det_update(Cases0, Functor, DisjList1, Cases1)
		;
			DisjList1 = [Goal],
			map__det_insert(Cases0, Functor, DisjList1, Cases1)
		)
	;
		MaybeFunctor = no,
		Left1 = [Goal0 | Left0],
		Cases1 = Cases0
	),
	partition_disj_trial(Goals, Var, Left1, Left, Cases1, Cases).

:- pred find_bind_var_for_switch_in_deconstruct(prog_var, hlds_goal,
		list(hlds_goal), maybe(cons_id), maybe(cons_id), unit, unit).
:- mode find_bind_var_for_switch_in_deconstruct(in, in, out,
		in, out, in, out) is det.

find_bind_var_for_switch_in_deconstruct(_UnifyVar, Goal0, Goals, 
		_Result0, Result, _, unit) :-
	(
		Goal0 = unify(A, B, C, UnifyInfo0, E) - GoalInfo,
		UnifyInfo0 = deconstruct(A, Functor, F, G, _, I)
	->
		Result = yes(Functor),
			% The deconstruction unification now becomes
			% deterministic, since the test will get
			% carried out in the switch.
		UnifyInfo = deconstruct(A, Functor, F, G,
			cannot_fail, I),
		Goals = [unify(A, B, C, UnifyInfo, E) - GoalInfo]
	;
		error("find_bind_var_for_switch_in_deconstruct")
	).

%-----------------------------------------------------------------------------%

find_bind_var(Var, ProcessUnify, Goal0, Goal,
		Result0, Result, Info0, Info, FoundDeconstruct) :-
	map__init(Substitution),
	find_bind_var(Var, ProcessUnify, Goal0, Goal, Substitution,
		_, Result0, Result, Info0, Info, DeconstructSearch),
	(
		DeconstructSearch = before_deconstruct,
		FoundDeconstruct = no
	;
		DeconstructSearch = found_deconstruct,
		FoundDeconstruct = yes
	;
		DeconstructSearch = given_up_search,
		FoundDeconstruct = no
	).

:- type deconstruct_search
	--->	before_deconstruct
	;	found_deconstruct
	;	given_up_search.

:- pred find_bind_var(prog_var::in,
	process_unify(Result, Info)::in(process_unify),
	hlds_goal::in, hlds_goal::out,
	prog_substitution::in, prog_substitution::out, Result::in, Result::out,
	Info::in, Info::out, deconstruct_search::out) is det.

find_bind_var(Var, ProcessUnify, Goal0 - GoalInfo, Goal,
		Substitution0, Substitution, Result0, Result, Info0, Info,
		FoundDeconstruct) :-
	( Goal0 = some(Vars, CanRemove, SubGoal0) ->
		find_bind_var(Var, ProcessUnify, SubGoal0, SubGoal,
			Substitution0, Substitution, Result0, Result,
			Info0, Info, FoundDeconstruct),
		Goal = some(Vars, CanRemove, SubGoal) - GoalInfo
	; Goal0 = conj(SubGoals0) ->
		conj_find_bind_var(Var, ProcessUnify, SubGoals0, SubGoals,
			Substitution0, Substitution, Result0, Result,
			Info0, Info, FoundDeconstruct),
		Goal = conj(SubGoals) - GoalInfo
	; Goal0 = unify(A, B, _, UnifyInfo0, _) ->
		(
			% check whether the unification is a deconstruction
			% unification on Var or a variable aliased to Var
			UnifyInfo0 = deconstruct(UnifyVar, _, _, _, _, _),
			term__apply_rec_substitution(
				term__variable(Var),
				Substitution0, term__variable(Var1)),
			term__apply_rec_substitution(
				term__variable(UnifyVar),
				Substitution0, term__variable(UnifyVar1)),
			Var1 = UnifyVar1
		->
			call(ProcessUnify, Var, Goal0 - GoalInfo, Goals,
				Result0, Result, Info0, Info),
			conj_list_to_goal(Goals, GoalInfo, Goal),
			FoundDeconstruct = found_deconstruct,
			Substitution = Substitution0
		;
			Goal = Goal0 - GoalInfo,
			FoundDeconstruct = before_deconstruct,
			% otherwise abstractly interpret the unification
			Result = Result0,
			Info = Info0,
			( interpret_unify(A, B, Substitution0, Substitution1) ->
				Substitution = Substitution1
			;
				% the unification must fail - just ignore it
				Substitution = Substitution0
			)
		)
	;
		Goal = Goal0 - GoalInfo,
		Substitution = Substitution0,
		Result = Result0,
		Info = Info0,
		FoundDeconstruct = given_up_search
	).

:- pred conj_find_bind_var(prog_var::in,
	process_unify(Result, Info)::in(process_unify), 
	list(hlds_goal)::in, list(hlds_goal)::out,
	prog_substitution::in, prog_substitution::out, Result::in, Result::out,
	Info::in, Info::out, deconstruct_search::out) is det.

conj_find_bind_var(_Var, _, [], [], Substitution, Substitution,
		Result, Result, Info, Info, before_deconstruct).
conj_find_bind_var(Var, ProcessUnify, [Goal0 | Goals0], [Goal | Goals],
		Substitution0, Substitution, Result0, Result,
		Info0, Info, FoundDeconstruct) :-
	find_bind_var(Var, ProcessUnify, Goal0, Goal, Substitution0,
		Substitution1, Result0, Result1,
		Info0, Info1, FoundDeconstruct1),
	( FoundDeconstruct1 = before_deconstruct ->
		conj_find_bind_var(Var, ProcessUnify, Goals0, Goals,
			Substitution1, Substitution, Result1, Result,
			Info1, Info, FoundDeconstruct)
	;
		FoundDeconstruct = FoundDeconstruct1,
		Goals = Goals0,
		Substitution = Substitution1,
		Result = Result1,
		Info = Info1
	).

%-----------------------------------------------------------------------------%

:- pred cases_to_switch(sorted_case_list, prog_var, map(prog_var, type),
		hlds_goal_info, instmap, module_info, hlds_goal_expr).
:- mode cases_to_switch(in, in, in, in, in, in, out) is det.

cases_to_switch(CasesList, Var, VarTypes, _GoalInfo, InstMap, ModuleInfo,
		Goal) :-
	instmap__lookup_var(InstMap, Var, VarInst),
	( inst_is_bound_to_functors(ModuleInfo, VarInst, Functors) ->
		functors_to_cons_ids(Functors, ConsIds0),
		list__sort(ConsIds0, ConsIds),
		delete_unreachable_cases(CasesList, ConsIds, CasesList1),
		( list__same_length(Functors, CasesList1) ->
			CanFail = cannot_fail
		;
			CanFail = can_fail
		)
	;
		map__lookup(VarTypes, Var, Type),
		CasesList1 = CasesList,
		( switch_covers_all_cases(CasesList1, Type, ModuleInfo) ->
			CanFail = cannot_fail
		;
			CanFail = can_fail
		)
	),
	detect_switches_in_cases(CasesList1, InstMap, VarTypes,
		ModuleInfo, Cases),

	% We turn switches with no arms into fail, since this avoids having
	% the code generator flush the control variable of the switch.
	% We can't easily eliminate switches with one arm, since the
	% code of the arm will have the unification between the variable
	% and the function symbol as det. The gain would be minimal to
	% nonexistent anyway.
	(
		Cases = [],
		Goal = disj([])
	;
		Cases = [_ | _],
		Goal = switch(Var, CanFail, Cases)
	).

	% check whether a switch handles all the possible
	% constants/functors for the type

:- pred switch_covers_all_cases(sorted_case_list, type, module_info).
:- mode switch_covers_all_cases(in, in, in) is semidet.

switch_covers_all_cases(CasesList, Type, ModuleInfo) :-
	type_util__switch_type_num_functors(ModuleInfo, Type, NumFunctors),
	list__length(CasesList, NumCases),
	NumCases = NumFunctors.

	% convert the assoc_list(cons_id, list(hlds_goal) back into
	% a plain list(case).

:- pred fix_case_list(assoc_list(cons_id, list(hlds_goal)), hlds_goal_info,
	list(case)).
:- mode fix_case_list(in, in, out) is det.

fix_case_list([], _, []).
fix_case_list([Functor - DisjList0 | Cases0], GoalInfo,
		[case(Functor, Goal) | Cases]) :-
		% We need to put the list back the right way around.
	list__reverse(DisjList0, DisjList),
	disj_list_to_goal(DisjList, GoalInfo, Goal),
	fix_case_list(Cases0, GoalInfo, Cases).

%-----------------------------------------------------------------------------%
