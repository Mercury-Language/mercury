%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: conway.

% Computes the `live_vars', i.e. which variables are either live across a
% call or live at the start of a disjunction, allocates a stack slot for
% each of these variables, and stores this information in the call_info
% structure in the proc_info.

%-----------------------------------------------------------------------------%

:- module live_vars.

:- interface.

:- import_module hlds, llds.

:- pred detect_live_vars(module_info, module_info).
:- mode detect_live_vars(in, out) is det.

:- pred detect_live_vars_in_proc(proc_info, module_info, proc_info).
:- mode detect_live_vars_in_proc(di, in, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list, map, set, std_util.
:- import_module mode_util, int, term, require.
:- import_module graph_colour.

%-----------------------------------------------------------------------------%

	% Traverse the module structure, calling `detect_live_vars_in_goal'
	% for each procedure body.

detect_live_vars(ModuleInfo0, ModuleInfo1) :-
	module_info_predids(ModuleInfo0, PredIds),
	detect_live_vars_in_preds(PredIds, ModuleInfo0, ModuleInfo1).

:- pred detect_live_vars_in_preds(list(pred_id), module_info, module_info).
:- mode detect_live_vars_in_preds(in, in, out) is det.

detect_live_vars_in_preds([], ModuleInfo, ModuleInfo).
detect_live_vars_in_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_non_imported_procids(PredInfo, ProcIds),
	detect_live_vars_in_procs(ProcIds, PredId, ModuleInfo0, ModuleInfo1),
	detect_live_vars_in_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred detect_live_vars_in_procs(list(proc_id), pred_id, module_info,
					module_info).
:- mode detect_live_vars_in_procs(in, in, in, out) is det.

detect_live_vars_in_procs([], _PredId, ModuleInfo, ModuleInfo).
detect_live_vars_in_procs([ProcId | ProcIds], PredId,
						ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	detect_live_vars_in_proc(ProcInfo0, ModuleInfo0, ProcInfo),

	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),

	detect_live_vars_in_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

detect_live_vars_in_proc(ProcInfo0, ModuleInfo, ProcInfo) :-
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_interface_code_model(ProcInfo0, CodeModel),

	detect_initial_live_vars(ProcInfo0, ModuleInfo, Liveness0),
	set__init(LiveSets0),
	detect_live_vars_in_goal(Goal0, Liveness0, LiveSets0,
		CodeModel, ModuleInfo, _Liveness, LiveSets),
	graph_colour__group_elements(LiveSets, ColourSets),
	set__to_sorted_list(ColourSets, ColourList),
	live_vars__allocate_call_info(ColourList, CodeModel, CallInfo),

	proc_info_set_call_info(ProcInfo0, CallInfo, ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The call_info structure (map(var, lval)) is threaded through the traversal
% of the goal. The liveness information is computed from the liveness
% delta annotations.

:- pred detect_live_vars_in_goal(hlds__goal, 
		liveness_info, set(set(var)), code_model, module_info,
		liveness_info, set(set(var))).
:- mode detect_live_vars_in_goal(in, in, in, in, in, out, out) is det.

detect_live_vars_in_goal(Goal0 - GoalInfo, Liveness0,
		LiveSets0, CodeModel, ModuleInfo,
			Liveness, LiveSets) :-
	goal_info_pre_delta_liveness(GoalInfo, PreDelta),
	PreDelta = PreBirths - PreDeaths,
	goal_info_post_delta_liveness(GoalInfo, PostDelta),
	PostDelta = PostBirths - PostDeaths,
	set__difference(Liveness0,  PreDeaths, Liveness1),
	set__union(Liveness1, PreBirths, Liveness2),

	(
		goal_is_atomic(Goal0)
	->
		set__difference(Liveness2, PostDeaths, Liveness3)
	;
		Liveness3 = Liveness2
	),

	detect_live_vars_in_goal_2(Goal0, Liveness3, LiveSets0,
		CodeModel, ModuleInfo, Liveness4, LiveSets1),

	(
		goal_is_atomic(Goal0)
	->
		Liveness5 = Liveness4
	;
		set__difference(Liveness4, PostDeaths, Liveness5)
	),

        set__union(Liveness5, PostBirths, Liveness),

		% Add extra interference for variables that become live
		% and variables that be come dead in this goal.
	(
		% goal_is_atomic(Goal0)
		fail
		% XXX this is not quite right
	->
		set__union(PreBirths, PostDeaths, ExtraInterference),
		set__insert(LiveSets1, ExtraInterference, LiveSets)
	;
		LiveSets = LiveSets1
	).

%-----------------------------------------------------------------------------%
	% Here we process each of the different sorts of goals.
	% `Liveness' is the set of live variables, i.e. vars which
	% have been referenced and may be referenced again (during
	% forward execution).
	% `LiveSets' is the interference graph, i.e. the set of sets
	% of variables which need to be on the stack at the same time.

:- pred detect_live_vars_in_goal_2(hlds__goal_expr, liveness_info,
			set(set(var)), code_model, module_info,
			liveness_info, set(set(var))).
:- mode detect_live_vars_in_goal_2(in, in, in, in, in, out, out) is det.

detect_live_vars_in_goal_2(conj(Goals0), Liveness0, LiveSets0,
		CodeModel, ModuleInfo, Liveness, LiveSets) :-
	detect_live_vars_in_conj(Goals0, Liveness0, LiveSets0,
		CodeModel, ModuleInfo, Liveness, LiveSets).

detect_live_vars_in_goal_2(disj(Goals0), Liveness0, LiveSets0,
		CodeModel, ModuleInfo, Liveness, LiveSets) :-
		% All the currently live variables need to be saved
		% on the stack at the start of a disjunction, since we
		% may need them on backtracking.  Therefore they need to
		% be on the stack at the same time, and hence we insert
		% them as an interference set into LiveSets.
	set__insert(LiveSets0, Liveness0, LiveSets1),
	detect_live_vars_in_disj(Goals0, Liveness0, LiveSets1,
		CodeModel, ModuleInfo, Liveness, LiveSets).

detect_live_vars_in_goal_2(switch(_Var, _Det, Cases0), Liveness0,
		LiveSets0, CodeModel, ModuleInfo,
			Liveness, LiveSets) :-
	detect_live_vars_in_cases(Cases0, Liveness0, LiveSets0,
		CodeModel, ModuleInfo, Liveness, LiveSets).

detect_live_vars_in_goal_2(if_then_else(_Vars, Cond0, Then0, Else0),
		Liveness0, LiveSets0, CodeModel,
			ModuleInfo, Liveness, LiveSets) :-
	set__insert(LiveSets0, Liveness0, LiveSets0A),
	detect_live_vars_in_goal(Cond0, Liveness0, LiveSets0A,
		CodeModel, ModuleInfo, Liveness1, LiveSets1),
	detect_live_vars_in_goal(Then0, Liveness1, LiveSets1,
		CodeModel, ModuleInfo, _Liveness2, LiveSets2),
	detect_live_vars_in_goal(Else0, Liveness0, LiveSets2,
		CodeModel, ModuleInfo, Liveness, LiveSets).

detect_live_vars_in_goal_2(not(Goal0), Liveness0, LiveSets0,
		CodeModel, ModuleInfo, Liveness, LiveSets) :-
	set__insert(LiveSets0, Liveness0, LiveSets1),
	detect_live_vars_in_goal(Goal0, Liveness0, LiveSets1,
		CodeModel, ModuleInfo, Liveness, LiveSets).

detect_live_vars_in_goal_2(some(_Vs, Goal0), Liveness0, LiveSets0,
		CodeModel, ModuleInfo, Liveness, LiveSets) :-
	detect_live_vars_in_goal(Goal0, Liveness0, LiveSets0,
		CodeModel, ModuleInfo, Liveness, LiveSets).

detect_live_vars_in_goal_2(
		call(PredId, ProcId, ArgVars, Builtin, _, _, _),
		Liveness, LiveSets0,
		_CodeModel, ModuleInfo, Liveness, LiveSets) :-
	(
		is_builtin__is_inline(Builtin)
	->
		LiveSets = LiveSets0
	;
		% The variables which need to be saved onto the stack
		% before the call are all the variables that are live
		% after the call, except for the output arguments produced
		% by the call, plus all the variables that are nondet
		% live at the call.
		find_output_vars(PredId, ProcId, ArgVars, ModuleInfo, OutVars),
		set__difference(Liveness, OutVars, LiveVars),
		set__insert(LiveSets0, LiveVars, LiveSets)
	).

detect_live_vars_in_goal_2(unify(_,_,_,D,_), Liveness, LiveSets0,
		_CodeModel, _ModuleInfo, Liveness, LiveSets) :-
	(
		D = complicated_unify(_, _, _)
	->
			% we have to save all live variables
			% across complicated unifications. 
		set__insert(LiveSets0, Liveness, LiveSets)
	;
		LiveSets = LiveSets0
	).

%-----------------------------------------------------------------------------%

:- pred detect_live_vars_in_conj(list(hlds__goal), liveness_info,
	set(set(var)), code_model, module_info, 
	liveness_info, set(set(var))).
:- mode detect_live_vars_in_conj(in, in, in, in, in, out, out) is det.

detect_live_vars_in_conj([], Liveness, LiveVars,
		_CodeModel, _ModuleInfo, Liveness, LiveVars).
detect_live_vars_in_conj([Goal0|Goals0], Liveness0, LiveVars0,
		CodeModel, ModuleInfo, Liveness, LiveVars) :-
	(
		Goal0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, unreachable)
	->
		detect_live_vars_in_goal(Goal0, Liveness0,
			LiveVars0, CodeModel, ModuleInfo,
				Liveness, LiveVars)
	;
		detect_live_vars_in_goal(Goal0, Liveness0,
			LiveVars0, CodeModel, ModuleInfo,
				Liveness1, LiveVars1),
		detect_live_vars_in_conj(Goals0, Liveness1,
			LiveVars1, CodeModel, ModuleInfo,
				Liveness, LiveVars)
	).

%-----------------------------------------------------------------------------%

:- pred detect_live_vars_in_disj(list(hlds__goal), liveness_info,
	set(set(var)), code_model, module_info, liveness_info,
	set(set(var))).
:- mode detect_live_vars_in_disj(in, in, in, in, in, out, out) is det.

detect_live_vars_in_disj([], Liveness, LiveSets,
		_CodeModel, _ModuleInfo, Liveness, LiveSets).
detect_live_vars_in_disj([Goal0|Goals0], Liveness0, LiveSets0,
		CodeModel, ModuleInfo, Liveness, LiveSets) :-
	detect_live_vars_in_goal(Goal0, Liveness0, LiveSets0,
		CodeModel,ModuleInfo, Liveness, LiveSets1),
	detect_live_vars_in_disj(Goals0, Liveness0, LiveSets1,
		CodeModel, ModuleInfo, _Liveness2, LiveSets).
	% set__union(Liveness1, Liveness2, Liveness).
	% This predicate call is unnecessary because the post-deaths and
	% pre-births sets *should* be taking care of everything.

%-----------------------------------------------------------------------------%

:- pred detect_live_vars_in_cases(list(case), liveness_info,
		set(set(var)), code_model, module_info,
		liveness_info, set(set(var))).
:- mode detect_live_vars_in_cases(in, in, in, in, in, out, out) is det.

detect_live_vars_in_cases([], Liveness, LiveSets,
		_CodeModel, _ModuleInfo, Liveness, LiveSets).
detect_live_vars_in_cases([case(_Cons, Goal0)|Goals0], Liveness0,
		LiveSets0, CodeModel, ModuleInfo,
			Liveness, LiveSets) :-
	detect_live_vars_in_goal(Goal0, Liveness0, LiveSets0,
		CodeModel, ModuleInfo, Liveness, LiveSets1),
	detect_live_vars_in_cases(Goals0, Liveness0, LiveSets1,
		CodeModel, ModuleInfo, _Liveness2, LiveSets).
	% set__union(Liveness1, Liveness2, Liveness).
	% This predicate call is unnecessary because the post-deaths and
	% pre-births sets *should* be taking care of everything.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_initial_live_vars(proc_info, module_info, set(var)).
:- mode detect_initial_live_vars(in, in, out) is det.

detect_initial_live_vars(ProcInfo, ModuleInfo, Liveness) :-
	proc_info_headvars(ProcInfo, Vars),
	proc_info_argmodes(ProcInfo, Args),
	assoc_list__from_corresponding_lists(Vars, Args, VarArgs),
	set__init(Liveness0),
	detect_initial_live_vars_2(VarArgs, ModuleInfo, Liveness0, Liveness).

:- pred detect_initial_live_vars_2(assoc_list(var,mode), module_info,
							set(var), set(var)).
:- mode detect_initial_live_vars_2(in, in, in, out) is det.

detect_initial_live_vars_2([], _ModuleInfo, Liveness, Liveness).
detect_initial_live_vars_2([V - M|VAs], ModuleInfo,
						Liveness0, Liveness) :-
	(
		mode_is_input(ModuleInfo, M)
	->
		set__insert(Liveness0, V, Liveness1)
	;
		Liveness1 = Liveness0
	),
	detect_initial_live_vars_2(VAs, ModuleInfo, Liveness1, Liveness).

%-----------------------------------------------------------------------------%

:- pred find_output_vars(pred_id, proc_id, list(var), module_info, set(var)).
:- mode find_output_vars(in, in, in, in, out) is det.

find_output_vars(PredId, ProcId, ArgVars, ModuleInfo, OutVars) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_arg_info(ProcInfo, ArgInfo),
	assoc_list__from_corresponding_lists(ArgVars, ArgInfo, ArgPairs),
	set__init(OutVars0),
	find_output_vars_2(ArgPairs, OutVars0, OutVars).

:- pred find_output_vars_2(assoc_list(var, arg_info), set(var), set(var)).
:- mode find_output_vars_2(in, in, out) is det.

find_output_vars_2([], OutVars, OutVars).
find_output_vars_2([Var - arg_info(_, Mode)|Rest], OutVars0, OutVars) :-
	(
		Mode = top_out
	->
		set__insert(OutVars0, Var, OutVars1)
	;
		OutVars1 = OutVars0
	),
	find_output_vars_2(Rest, OutVars1, OutVars).

%-----------------------------------------------------------------------------%

:- pred live_vars__allocate_call_info(list(set(var)), code_model,
	map(var, lval)).
:- mode live_vars__allocate_call_info(in, in, out) is det.

live_vars__allocate_call_info(ColourList, CodeModel, CallInfo) :-
	map__init(CallInfo0),
	(
		CodeModel = model_non
	->
		First = 0
	;
		First = 1
	),
	live_vars__allocate_call_info_2(ColourList, First, CodeModel,
							CallInfo0, CallInfo).

:- pred live_vars__allocate_call_info_2(list(set(var)), int, code_model,
						map(var, lval), map(var, lval)).
:- mode live_vars__allocate_call_info_2(in, in, in, in, out) is det.

live_vars__allocate_call_info_2([], _N, _CodeModel, CallInfo, CallInfo).
live_vars__allocate_call_info_2([Vars|VarSets], N0, CodeModel,
		CallInfo0, CallInfo) :-
	set__to_sorted_list(Vars, VarList),
	(
		CodeModel = model_non
	->
		Slot = framevar(N0)
	;
		Slot = stackvar(N0)
	),
	live_vars__allocate_call_info_3(VarList, Slot, CallInfo0, CallInfo1),
	N1 is N0 + 1,
	live_vars__allocate_call_info_2(VarSets, N1, CodeModel,
		CallInfo1, CallInfo).

:- pred live_vars__allocate_call_info_3(list(var), lval,
					map(var, lval), map(var, lval)).
:- mode live_vars__allocate_call_info_3(in, in, in, out) is det.

live_vars__allocate_call_info_3([], _Slot, CallInfo, CallInfo).
live_vars__allocate_call_info_3([Var|Vars], Slot, CallInfo0, CallInfo) :-
	map__set(CallInfo0, Var, Slot, CallInfo1),
	live_vars__allocate_call_info_3(Vars, Slot, CallInfo1, CallInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
