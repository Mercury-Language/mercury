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

:- import_module hlds_module, hlds_pred.

:- pred detect_live_vars_in_proc(proc_info, module_info, proc_info).
% :- mode detect_live_vars_in_proc(di, in, uo) is det.
:- mode detect_live_vars_in_proc(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module llds, arg_info, prog_data, hlds_goal, hlds_data, mode_util.
:- import_module code_aux, globals, graph_colour, instmap.
:- import_module list, map, set, std_util, assoc_list.
:- import_module int, term, require.

%-----------------------------------------------------------------------------%

detect_live_vars_in_proc(ProcInfo0, ModuleInfo, ProcInfo) :-
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_interface_code_model(ProcInfo0, CodeModel),

	detect_initial_live_vars(ProcInfo0, ModuleInfo, Liveness0),
	set__init(LiveSets0),
	detect_live_vars_in_goal(Goal0, Liveness0, LiveSets0,
		ModuleInfo, ProcInfo0, _Liveness, LiveSets),
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
		liveness_info, set(set(var)), module_info,
		proc_info, liveness_info, set(set(var))).
:- mode detect_live_vars_in_goal(in, in, in, in, in, out, out) is det.

detect_live_vars_in_goal(Goal0 - GoalInfo, Liveness0,
		LiveSets0, ModuleInfo, ProcInfo,
			Liveness, LiveSets) :-
	goal_info_pre_delta_liveness(GoalInfo, PreDelta),
	PreDelta = PreBirths - PreDeaths,
	goal_info_post_delta_liveness(GoalInfo, PostDelta),
	PostDelta = PostBirths - PostDeaths,
	set__difference(Liveness0,  PreDeaths, Liveness1),
	set__union(Liveness1, PreBirths, Liveness2),
	%
	% if the goal is atomic, we want to apply the postdeaths
	% before processing the goal, but if the goal is a compound
	% goal, then we want to apply them after processing it
	%
	(
		goal_is_atomic(Goal0)
	->
		set__difference(Liveness2, PostDeaths, Liveness3)
	;
		Liveness3 = Liveness2
	),

	goal_info_nondet_lives(GoalInfo, NondetLives),
	goal_info_get_code_model(GoalInfo, CodeModel),

	detect_live_vars_in_goal_2(Goal0, NondetLives, Liveness3, LiveSets0,
		CodeModel, ModuleInfo, ProcInfo, Liveness4, LiveSets1),

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
		% NB: `fail' is a conservative approximation
		% We could do better, but `goal_is_atomic' is not
		% quite right
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
	% `NondetLives' is the set of variables that may or may not be
	% `live' during the current forward execution but will become
	% live again on backtracking.

:- pred detect_live_vars_in_goal_2(hlds__goal_expr, set(var), liveness_info,
			set(set(var)), code_model, module_info, proc_info,
			liveness_info, set(set(var))).
:- mode detect_live_vars_in_goal_2(in, in, in, in, in, in, in, out, out) is det.

detect_live_vars_in_goal_2(conj(Goals0), _NondetLives, Liveness0, LiveSets0,
		_CodeModel, ModuleInfo, ProcInfo, Liveness, LiveSets) :-
	detect_live_vars_in_conj(Goals0, Liveness0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, LiveSets).

detect_live_vars_in_goal_2(disj(Goals0, _), NondetLives, Liveness0, LiveSets0,
		CodeModel, ModuleInfo, ProcInfo, Liveness, LiveSets) :-
	( CodeModel = model_non ->
		% All the currently live variables need to be saved
		% on the stack at the start of a nondet disjunction, since we
		% may need them on backtracking.  Therefore they need to
		% be on the stack at the same time, and hence we insert
		% them as an interference set into LiveSets.
		set__union(Liveness0, NondetLives, LiveVars),
		set__insert(LiveSets0, LiveVars, LiveSets1)
	;
		LiveSets1 = LiveSets0
	),
	detect_live_vars_in_disj(Goals0, Liveness0, LiveSets1,
		ModuleInfo, ProcInfo, Liveness, LiveSets).

detect_live_vars_in_goal_2(switch(_Var, _Det, Cases0, _), 
		_NondetLives, Liveness0, LiveSets0, _CodeModel,
			ModuleInfo, ProcInfo, Liveness, LiveSets) :-
	detect_live_vars_in_cases(Cases0, Liveness0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, LiveSets).

detect_live_vars_in_goal_2(if_then_else(_Vars, Cond0, Then0, Else0, _),
		NondetLives, Liveness0, LiveSets0, _CodeModel,
			ModuleInfo, ProcInfo, Liveness, LiveSets) :-
	set__union(Liveness0, NondetLives, LiveVars),
	( code_aux__contains_only_builtins(Cond0) ->
		LiveSets0A = LiveSets0
	;
		set__insert(LiveSets0, LiveVars, LiveSets0A)
	),
	detect_live_vars_in_goal(Cond0, Liveness0, LiveSets0A,
		ModuleInfo, ProcInfo, Liveness1, LiveSets1),
	detect_live_vars_in_goal(Then0, Liveness1, LiveSets1,
		ModuleInfo, ProcInfo, _Liveness2, LiveSets2),
	detect_live_vars_in_goal(Else0, Liveness0, LiveSets2,
		ModuleInfo, ProcInfo, Liveness, LiveSets).

detect_live_vars_in_goal_2(not(Goal0), NondetLives, Liveness0, LiveSets0,
		_CodeModel, ModuleInfo, ProcInfo, Liveness, LiveSets) :-
	set__union(Liveness0, NondetLives, LiveVars),
	set__insert(LiveSets0, LiveVars, LiveSets1),
	detect_live_vars_in_goal(Goal0, Liveness0, LiveSets1,
		ModuleInfo, ProcInfo, Liveness, LiveSets).

detect_live_vars_in_goal_2(some(_Vs, Goal0), _NondetLives, Liveness0, LiveSets0,
		_CodeModel, ModuleInfo, ProcInfo, Liveness, LiveSets) :-
	detect_live_vars_in_goal(Goal0, Liveness0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, LiveSets).

detect_live_vars_in_goal_2(higher_order_call(_PredVar, ArgVars, Types, Modes,
		Det),
		NondetLives, Liveness, LiveSets0,
		_CodeModel, ModuleInfo, ProcInfo, Liveness, LiveSets) :-
	% The variables which need to be saved onto the stack
	% before the call are all the variables that are live
	% after the call, except for the output arguments produced
	% by the call, plus all the variables that are nondet
	% live at the call.
	% To figure out which variables are output, we use the arg_info;
	% but it shouldn't matter which arg convention we're using,
	% so we can just pass convention `simple' to make_arg_infos.

	determinism_to_code_model(Det, CodeModel),
	make_arg_infos(simple, Types, Modes, CodeModel, ModuleInfo, ArgInfos),
	find_output_vars_from_arg_info(ArgVars, ArgInfos, OutVars),
	set__difference(Liveness, OutVars, LiveVars0),
	set__union(LiveVars0, NondetLives, LiveVars1),

	% Might need to add more live variables with accurate GC.

	live_vars__maybe_add_accurate_gc_typeinfos(ModuleInfo, ProcInfo, 
		OutVars, LiveVars1, LiveVars),

	set__insert(LiveSets0, LiveVars, LiveSets).


detect_live_vars_in_goal_2(
		call(PredId, ProcId, ArgVars, Builtin, _, _),
		NondetLives, Liveness, LiveSets0,
		_CodeModel, ModuleInfo, ProcInfo, Liveness, LiveSets) :-
	(
		hlds__is_builtin_is_inline(Builtin)
	->
		LiveSets = LiveSets0
	;
		% The variables which need to be saved onto the stack
		% before the call are all the variables that are live
		% after the call, except for the output arguments produced
		% by the call, plus all the variables that are nondet
		% live at the call.
		find_output_vars(PredId, ProcId, ArgVars, ModuleInfo, OutVars),
		set__difference(Liveness, OutVars, LiveVars0),
		set__union(LiveVars0, NondetLives, LiveVars1),

		% Might need to add more live variables with accurate GC.

		live_vars__maybe_add_accurate_gc_typeinfos(ModuleInfo, 
			ProcInfo, OutVars, LiveVars1, LiveVars),

		set__insert(LiveSets0, LiveVars, LiveSets)
	).

detect_live_vars_in_goal_2(unify(_,_,_,D,_), NondetLives, Liveness, LiveSets0,
		_CodeModel, _ModuleInfo, _ProcInfo, Liveness, LiveSets) :-
	(
		D = complicated_unify(_, _)
	->
			% we have to save all live variables
			% (and nondet-live variables)
			% across complicated unifications. 
		set__union(Liveness, NondetLives, LiveVars),
		set__insert(LiveSets0, LiveVars, LiveSets)
	;
		LiveSets = LiveSets0
	).

detect_live_vars_in_goal_2(
		pragma_c_code(_C_Code, IsRecursive, PredId, ProcId, Args,
				_ArgNameMap), 
		NondetLives, Liveness, LiveSets0, _Model, ModuleInfo, ProcInfo,
		Liveness, LiveSets) :-

	( IsRecursive = non_recursive ->
		% We don't need to save any variables onto the stack
		% before a pragma_c_code if we know that it is not going to call
		% back Mercury code, because C code won't clobber the registers.

		LiveSets = LiveSets0
	;
		% The variables which need to be saved onto the stack
		% before the c_code execution are all the variables that are
		% live after the c_code execution, except for the output
		% arguments produced by the c_code, plus all the variables
		% that are nondet live at the c_code.

		find_output_vars(PredId, ProcId, Args, ModuleInfo, OutVars),
		set__difference(Liveness, OutVars, LiveVars0),
		set__union(LiveVars0, NondetLives, LiveVars1),

		% Might need to add more live variables with accurate GC.

		live_vars__maybe_add_accurate_gc_typeinfos(ModuleInfo, 
			ProcInfo, OutVars, LiveVars1, LiveVars),

		set__insert(LiveSets0, LiveVars, LiveSets)
	).

%-----------------------------------------------------------------------------%

:- pred detect_live_vars_in_conj(list(hlds__goal), liveness_info,
	set(set(var)), module_info, proc_info,
	liveness_info, set(set(var))).
:- mode detect_live_vars_in_conj(in, in, in, in, in, out, out) is det.

detect_live_vars_in_conj([], Liveness, LiveVars,
		_ModuleInfo, _ProcInfo, Liveness, LiveVars).
detect_live_vars_in_conj([Goal0|Goals0], Liveness0, LiveVars0,
		ModuleInfo, ProcInfo, Liveness, LiveVars) :-
	(
		Goal0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
		instmap_delta_is_unreachable(InstMapDelta)
	->
		detect_live_vars_in_goal(Goal0, Liveness0,
			LiveVars0, ModuleInfo, ProcInfo,
				Liveness, LiveVars)
	;
		detect_live_vars_in_goal(Goal0, Liveness0,
			LiveVars0, ModuleInfo, ProcInfo,
				Liveness1, LiveVars1),
		detect_live_vars_in_conj(Goals0, Liveness1,
			LiveVars1, ModuleInfo, ProcInfo,
				Liveness, LiveVars)
	).

%-----------------------------------------------------------------------------%

:- pred detect_live_vars_in_disj(list(hlds__goal), liveness_info,
	set(set(var)), module_info, proc_info, liveness_info,
	set(set(var))).
:- mode detect_live_vars_in_disj(in, in, in, in, in, out, out) is det.

detect_live_vars_in_disj([], Liveness, LiveSets,
		_ModuleInfo, _ProcInfo, Liveness, LiveSets).
detect_live_vars_in_disj([Goal0|Goals0], Liveness0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, LiveSets) :-
	detect_live_vars_in_goal(Goal0, Liveness0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, LiveSets1),
	detect_live_vars_in_disj(Goals0, Liveness0, LiveSets1,
		ModuleInfo, ProcInfo, _Liveness2, LiveSets).
	% set__union(Liveness1, Liveness2, Liveness).
	% This predicate call is unnecessary because the post-deaths and
	% pre-births sets *should* be taking care of everything.

%-----------------------------------------------------------------------------%

:- pred detect_live_vars_in_cases(list(case), liveness_info,
		set(set(var)), module_info, proc_info,
		liveness_info, set(set(var))).
:- mode detect_live_vars_in_cases(in, in, in, in, in, out, out) is det.

detect_live_vars_in_cases([], Liveness, LiveSets,
		_ModuleInfo, _ProcInfo, Liveness, LiveSets).
detect_live_vars_in_cases([case(_Cons, Goal0)|Goals0], Liveness0,
		LiveSets0, ModuleInfo, ProcInfo,
			Liveness, LiveSets) :-
	detect_live_vars_in_goal(Goal0, Liveness0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, LiveSets1),
	detect_live_vars_in_cases(Goals0, Liveness0, LiveSets1,
		ModuleInfo, ProcInfo, _Liveness2, LiveSets).
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

:- pred detect_initial_live_vars_2(assoc_list(var, (mode)), module_info,
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


	% If doing accurate garbage collection, any typeinfos for 
	% output variables or live variables are also live. 
	% This is because if you want to collect, you need to 
	% know what shape the polymorphic args of the variables 
	% are, so you need the typeinfos to be present on the stack.

	% The live variables obviously need their typeinfos 
	% live, but the output variables also need their typeinfos
	% saved (otherwise we would throw out typeinfos and might
	% need one at a continuation point just after a call).

	% live_vars__maybe_add_accurate_gc_typeinfos takes a set of vars 
	% (output vars) and a set of live vars and if we 
	% are doing accurate GC, add the appropriate typeinfo variables to the 
	% set of variables. If not, return the live vars unchanged.

	% Make sure you get the output vars first, and the live vars second,
	% since this makes a significant difference to the output set of vars.

:- pred live_vars__maybe_add_accurate_gc_typeinfos(module_info, proc_info,
	set(var), set(var), set(var)).
:- mode live_vars__maybe_add_accurate_gc_typeinfos(in, in, in, in, out) is det.
live_vars__maybe_add_accurate_gc_typeinfos(ModuleInfo, ProcInfo, OutVars, 
	LiveVars1, LiveVars) :-
	module_info_globals(ModuleInfo, Globals),
	globals__get_gc_method(Globals, GC_Method),
	( 
		GC_Method = accurate
	->
		proc_info_get_used_typeinfos_setwise(ProcInfo, LiveVars1, 
			TypeInfoVarsLive),
		proc_info_get_used_typeinfos_setwise(ProcInfo, OutVars, 
			TypeInfoVarsOut),
		set__union(LiveVars1, TypeInfoVarsOut, LiveVars2),
		set__union(LiveVars2, TypeInfoVarsLive, LiveVars)
	;
		LiveVars = LiveVars1
	).

%-----------------------------------------------------------------------------%

:- pred find_output_vars(pred_id, proc_id, list(var), module_info, set(var)).
:- mode find_output_vars(in, in, in, in, out) is det.

find_output_vars(PredId, ProcId, ArgVars, ModuleInfo, OutVars) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_arg_info(ProcInfo, ArgInfo),
	find_output_vars_from_arg_info(ArgVars, ArgInfo, OutVars).

:- pred find_output_vars_from_arg_info(list(var), list(arg_info), set(var)).
:- mode find_output_vars_from_arg_info(in, in, out) is det.

find_output_vars_from_arg_info(ArgVars, ArgInfo, OutVars) :-
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
