%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File live_vars.m
%
% Main authors: conway, zs.
%
% This module allocates stack slots to the variables that need to be saved
% either across a call or across a goal that may fail.
%
% The jobs is done in two steps. First we traverse the predicate definition
% looking for sets of variables that must be saved on the stack at the same
% time. Then we use a graph colouring algorithm to find an allocation of
% stack slots (colours) to variables such that in each set of variables
% that must be saved at the same time, each variable has a different colour.

%-----------------------------------------------------------------------------%

:- module live_vars.

:- interface.

:- import_module hlds_module, hlds_pred.

:- pred allocate_stack_slots_in_proc(proc_info, module_info, proc_info).
% :- mode allocate_stack_slots_in_proc(di, in, uo) is det.
:- mode allocate_stack_slots_in_proc(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module llds, arg_info, prog_data, hlds_goal, hlds_data, mode_util.
:- import_module liveness, code_aux, globals, graph_colour, instmap.
:- import_module list, map, set, std_util, assoc_list.
:- import_module int, term, require.

%-----------------------------------------------------------------------------%

allocate_stack_slots_in_proc(ProcInfo0, ModuleInfo, ProcInfo) :-
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_interface_code_model(ProcInfo0, CodeModel),

	initial_liveness(ProcInfo0, ModuleInfo, Liveness0),
	set__init(LiveSets0),
	set__init(ResumeVars0),
	build_live_sets_in_goal(Goal0, Liveness0, ResumeVars0, LiveSets0,
		ModuleInfo, ProcInfo0, _Liveness, _ResumeVars, LiveSets),
	graph_colour__group_elements(LiveSets, ColourSets),
	set__to_sorted_list(ColourSets, ColourList),
	allocate_stack_slots(ColourList, CodeModel, StackSlots),

	proc_info_set_stack_slots(ProcInfo0, StackSlots, ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The stack_slots structure (map(var, lval)) is threaded through the traversal
% of the goal. The liveness information is computed from the liveness
% delta annotations.

:- pred build_live_sets_in_goal(hlds_goal, set(var), set(var), set(set(var)),
	module_info, proc_info, set(var), set(var), set(set(var))).
:- mode build_live_sets_in_goal(in, in, in, in, in, in, out, out, out) is det.

build_live_sets_in_goal(Goal0 - GoalInfo, Liveness0, ResumeVars0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, ResumeVars, LiveSets) :-
	goal_info_get_pre_births(GoalInfo, PreBirths),
	goal_info_get_pre_deaths(GoalInfo, PreDeaths),
	goal_info_get_post_births(GoalInfo, PostBirths),
	goal_info_get_post_deaths(GoalInfo, PostDeaths),

	set__difference(Liveness0, PreDeaths, Liveness1),
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

	goal_info_get_resume_point(GoalInfo, ResumePoint),
	(
		ResumePoint = no_resume_point,
		ResumeVars1 = ResumeVars0,
		LiveSets1 = LiveSets0
	;
		ResumePoint = resume_point(ResumePointVars, Locs),
		(
			Locs = orig_only,
			ResumeVars1 = ResumeVars0,
			LiveSets1 = LiveSets0
		;
			Locs = stack_only,
			set__union(ResumeVars0, ResumePointVars, ResumeVars1),
			set__insert(LiveSets0, ResumeVars1, LiveSets1)
		;
			Locs = orig_and_stack,
			set__union(ResumeVars0, ResumePointVars, ResumeVars1),
			set__insert(LiveSets0, ResumeVars1, LiveSets1)
		;
			Locs = stack_and_orig,
			set__union(ResumeVars0, ResumePointVars, ResumeVars1),
			set__insert(LiveSets0, ResumeVars1, LiveSets1)
		)
	),

	build_live_sets_in_goal_2(Goal0, Liveness3, ResumeVars1, LiveSets1,
		GoalInfo, ModuleInfo, ProcInfo,
		Liveness4, ResumeVars, LiveSets2),

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
/*******
		% goal_is_atomic(Goal0)
		fail
		% NB: `fail' is a conservative approximation
		% We could do better, but `goal_is_atomic' is not
		% quite right
	->
		set__union(PreBirths, PostDeaths, ExtraInterference),
		set__insert(LiveSets2, ExtraInterference, LiveSets)
	;
*******/
		LiveSets = LiveSets2
	).

%-----------------------------------------------------------------------------%

	% Here we process each of the different sorts of goals.
	% `Liveness' is the set of live variables, i.e. vars which
	% have been referenced and may be referenced again (during
	% forward execution).
	% `ResumeVars' is the set of variables that may or may not be
	% `live' during the current forward execution but will become
	% live again on backtracking.
	% `LiveSets' is the interference graph, i.e. the set of sets
	% of variables which need to be on the stack at the same time.

:- pred build_live_sets_in_goal_2(hlds_goal_expr, set(var), set(var),
	set(set(var)), hlds_goal_info, module_info, proc_info,
	set(var), set(var), set(set(var))).
:- mode build_live_sets_in_goal_2(in, in, in, in, in, in, in, out, out, out)
	is det.

build_live_sets_in_goal_2(conj(Goals0), Liveness0, ResumeVars0, LiveSets0,
		_, ModuleInfo, ProcInfo, Liveness, ResumeVars, LiveSets) :-
	build_live_sets_in_conj(Goals0, Liveness0, ResumeVars0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, ResumeVars, LiveSets).

build_live_sets_in_goal_2(disj(Goals0, _), Liveness0, ResumeVars0, LiveSets0,
		GoalInfo, ModuleInfo, ProcInfo, Liveness, ResumeVars, LiveSets)
		:-
	build_live_sets_in_disj(Goals0, Liveness0, ResumeVars0, LiveSets0,
		GoalInfo, ModuleInfo, ProcInfo, Liveness, ResumeVars, LiveSets).

build_live_sets_in_goal_2(switch(_, _, Cases0, _), Liveness0,
		ResumeVars0, LiveSets0, _, ModuleInfo, ProcInfo, Liveness,
		ResumeVars, LiveSets) :-
	build_live_sets_in_cases(Cases0, Liveness0, ResumeVars0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, ResumeVars, LiveSets).

build_live_sets_in_goal_2(if_then_else(_Vars, Cond0, Then0, Else0, _),
		Liveness0, ResumeVars0, LiveSets0, _, ModuleInfo, ProcInfo,
		Liveness, ResumeVars, LiveSets) :-
	build_live_sets_in_goal(Cond0, Liveness0, ResumeVars0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness1, ResumeVars1, LiveSets1),
	build_live_sets_in_goal(Then0, Liveness1, ResumeVars1, LiveSets1,
		ModuleInfo, ProcInfo, _Liveness2, ResumeVars2, LiveSets2),
	build_live_sets_in_goal(Else0, Liveness0, ResumeVars0, LiveSets2,
		ModuleInfo, ProcInfo, Liveness, ResumeVars3, LiveSets),
	set__union(ResumeVars2, ResumeVars3, ResumeVars).

build_live_sets_in_goal_2(not(Goal0), Liveness0, ResumeVars0, LiveSets0,
		_, ModuleInfo, ProcInfo, Liveness, ResumeVars0, LiveSets) :-
	build_live_sets_in_goal(Goal0, Liveness0, ResumeVars0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, _, LiveSets).

build_live_sets_in_goal_2(some(_Vs, Goal0), Liveness0, ResumeVars0, LiveSets0,
		GoalInfo, ModuleInfo, ProcInfo, Liveness, ResumeVars, LiveSets)
		:-
	build_live_sets_in_goal(Goal0, Liveness0, ResumeVars0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, ResumeVars1, LiveSets),

	% If the "some" goal cannot succeed more than once,
	% then execution cannot backtrack into the inner goal once control
	% has left it. Therefore the code following the "some" can reuse
	% any stack slots needed by resume points in the inner goal.

	goal_info_get_code_model(GoalInfo, CodeModel),
	( CodeModel = model_non ->
		ResumeVars = ResumeVars1
	;
		ResumeVars = ResumeVars0
	).

build_live_sets_in_goal_2(higher_order_call(_, ArgVars, Types, Modes, Det),
		Liveness, ResumeVars0, LiveSets0,
		GoalInfo, ModuleInfo, ProcInfo,
		Liveness, ResumeVars, LiveSets) :-
	% The variables which need to be saved onto the stack
	% before the call are all the variables that are live
	% after the call, except for the output arguments produced
	% by the call, plus all the variables that may be needed
	% at an enclosing resumption point.

	% To figure out which variables are output, we use the arg_info;
	% but it shouldn't matter which arg convention we're using,
	% so we can just pass convention `simple' to make_arg_infos.

	determinism_to_code_model(Det, CallModel),
	make_arg_infos(simple, Types, Modes, CallModel, ModuleInfo, ArgInfos),
	find_output_vars_from_arg_info(ArgVars, ArgInfos, OutVars),
	set__difference(Liveness, OutVars, InputLiveness),
	set__union(InputLiveness, ResumeVars0, StackVars0),

	% Might need to add more live variables with accurate GC.

	maybe_add_accurate_gc_typeinfos(ModuleInfo, ProcInfo,
		OutVars, StackVars0, StackVars),

	set__insert(LiveSets0, StackVars, LiveSets),

	% If this is a nondet call, then all the stack slots we need
	% must be protected against reuse in following code.

	goal_info_get_code_model(GoalInfo, CodeModel),
	( CodeModel = model_non ->
		ResumeVars = StackVars		% includes ResumeVars0
	;
		ResumeVars = ResumeVars0
	).

build_live_sets_in_goal_2(call(PredId, ProcId, ArgVars, BuiltinState, _, _),
		Liveness, ResumeVars0, LiveSets0,
		GoalInfo, ModuleInfo, ProcInfo,
		Liveness, ResumeVars, LiveSets) :-
	(
		BuiltinState = inline_builtin
	->
		ResumeVars = ResumeVars0,
		LiveSets = LiveSets0
	;
		% The variables which need to be saved onto the stack
		% before the call are all the variables that are live
		% after the call, except for the output arguments produced
		% by the call, plus all the variables that may be needed
		% at an enclosing resumption point.

		find_output_vars(PredId, ProcId, ArgVars, ModuleInfo, OutVars),
		set__difference(Liveness, OutVars, InputLiveness),
		set__union(InputLiveness, ResumeVars0, StackVars0),

		% Might need to add more live variables with accurate GC.

		maybe_add_accurate_gc_typeinfos(ModuleInfo,
			ProcInfo, OutVars, StackVars0, StackVars),

		set__insert(LiveSets0, StackVars, LiveSets),

		% If this is a nondet call, then all the stack slots we need
		% must be protected against reuse in following code.

		goal_info_get_code_model(GoalInfo, CodeModel),
		( CodeModel = model_non ->
			ResumeVars = StackVars		% includes ResumeVars0
		;
			ResumeVars = ResumeVars0
		)
	).

build_live_sets_in_goal_2(unify(_,_,_,D,_), Liveness, ResumeVars0, LiveSets0,
		_, _, _, Liveness, ResumeVars0, LiveSets) :-
	(
		D = complicated_unify(_, _)
	->
			% we have to save all live and protected variables
			% across complicated unifications.
		set__union(Liveness, ResumeVars0, LiveVars),
		set__insert(LiveSets0, LiveVars, LiveSets)
	;
		LiveSets = LiveSets0
	).

build_live_sets_in_goal_2(pragma_c_code(_, MayCallMercury, PredId, ProcId,
		Args, _, _, Extra), Liveness, ResumeVars0, LiveSets0,
		GoalInfo, ModuleInfo, ProcInfo,
		Liveness, ResumeVars, LiveSets) :-

	goal_info_get_code_model(GoalInfo, CodeModel),
	(
		CodeModel \= model_non,
		MayCallMercury = will_not_call_mercury
	->
		% We don't need to save any variables onto the stack
		% before a pragma_c_code if we know that it can't succeed
		% more than once and that it is not going to call back
		% Mercury code, because C code won't clobber the registers.

		ResumeVars = ResumeVars0,
		LiveSets = LiveSets0
	;
		% The variables which need to be saved onto the stack
		% before the call are all the variables that are live
		% after the call (except for the output arguments produced
		% by the call), plus any variables needed by a nondet
		% pragma to communication between incarnations, plus
		% all the variables that may be needed at an enclosing
		% resumption point.

		find_output_vars(PredId, ProcId, Args, ModuleInfo, OutVars),
		set__difference(Liveness, OutVars, InputLiveness),
		(
			Extra = none,
			StackVars0 = InputLiveness
		;
			Extra = extra_pragma_info(SavedVarNames, _),
			assoc_list__keys(SavedVarNames, SavedVars),
			set__insert_list(InputLiveness, SavedVars, StackVars0)
		),
		set__union(StackVars0, ResumeVars0, StackVars1),

		% Might need to add more live variables with accurate GC.

		maybe_add_accurate_gc_typeinfos(ModuleInfo,
			ProcInfo, OutVars, StackVars1, StackVars),

		set__insert(LiveSets0, StackVars, LiveSets),

		( CodeModel = model_non ->
			ResumeVars = StackVars		% includes ResumeVars0
		;
			ResumeVars = ResumeVars0
		)
	).

%-----------------------------------------------------------------------------%

:- pred build_live_sets_in_conj(list(hlds_goal), set(var), set(var),
	set(set(var)), module_info, proc_info, set(var), set(var),
	set(set(var))).
:- mode build_live_sets_in_conj(in, in, in, in, in, in, out, out, out) is det.

build_live_sets_in_conj([], Liveness, ResumeVars, LiveSets,
		_ModuleInfo, _ProcInfo, Liveness, ResumeVars, LiveSets).
build_live_sets_in_conj([Goal0 | Goals0], Liveness0, ResumeVars0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, ResumeVars, LiveSets) :-
	(
		Goal0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
		instmap_delta_is_unreachable(InstMapDelta)
	->
		build_live_sets_in_goal(Goal0,
			Liveness0, ResumeVars0, LiveSets0, ModuleInfo, ProcInfo,
			Liveness, ResumeVars, LiveSets)
	;
		build_live_sets_in_goal(Goal0,
			Liveness0, ResumeVars0, LiveSets0, ModuleInfo, ProcInfo,
			Liveness1, ResumeVars1, LiveSets1),
		build_live_sets_in_conj(Goals0,
			Liveness1, ResumeVars1, LiveSets1, ModuleInfo, ProcInfo,
			Liveness, ResumeVars, LiveSets)
	).

%-----------------------------------------------------------------------------%

:- pred build_live_sets_in_disj(list(hlds_goal), set(var), set(var),
	set(set(var)), hlds_goal_info, module_info, proc_info,
	set(var), set(var), set(set(var))).
:- mode build_live_sets_in_disj(in, in, in, in, in, in, in, out, out, out)
	is det.

build_live_sets_in_disj([], Liveness, ResumeVars, LiveSets, _, _, _,
		Liveness, ResumeVars, LiveSets).
build_live_sets_in_disj([Goal0 | Goals0], Liveness0, ResumeVars0, LiveSets0,
		GoalInfo, ModuleInfo, ProcInfo,
		Liveness, ResumeVars, LiveSets) :-
	build_live_sets_in_goal(Goal0, Liveness0, ResumeVars0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, ResumeVars1, LiveSets1),
	build_live_sets_in_disj(Goals0, Liveness0, ResumeVars0, LiveSets1,
		GoalInfo, ModuleInfo, ProcInfo,
		_Liveness2, ResumeVars2, LiveSets),
	goal_info_get_code_model(GoalInfo, CodeModel),
	( CodeModel = model_non ->
		set__union(ResumeVars1, ResumeVars2, ResumeVars3),
		goal_info_get_resume_point(GoalInfo, Resume),
		(
			Resume = resume_point(ResumePointVars, _),
			set__union(ResumeVars3, ResumePointVars, ResumeVars)
		;
			Resume = no_resume_point,
			ResumeVars = ResumeVars3
		)
	;
		ResumeVars = ResumeVars0
	).

%-----------------------------------------------------------------------------%

:- pred build_live_sets_in_cases(list(case), set(var), set(var),
	set(set(var)), module_info, proc_info, set(var), set(var),
	set(set(var))).
:- mode build_live_sets_in_cases(in, in, in, in, in, in, out, out, out) is det.

build_live_sets_in_cases([], Liveness, ResumeVars, LiveSets, _, _,
		Liveness, ResumeVars, LiveSets).
build_live_sets_in_cases([case(_Cons, Goal0) | Goals0],
		Liveness0, ResumeVars0, LiveSets0, ModuleInfo, ProcInfo,
		Liveness, ResumeVars, LiveSets) :-
	build_live_sets_in_goal(Goal0, Liveness0, ResumeVars0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, ResumeVars1, LiveSets1),
	build_live_sets_in_cases(Goals0, Liveness0, ResumeVars0, LiveSets1,
		ModuleInfo, ProcInfo, _Liveness2, ResumeVars2, LiveSets),
	set__union(ResumeVars1, ResumeVars2, ResumeVars).

%-----------------------------------------------------------------------------%
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

	% maybe_add_accurate_gc_typeinfos takes a set of vars
	% (output vars) and a set of live vars and if we
	% are doing accurate GC, add the appropriate typeinfo variables to the
	% set of variables. If not, return the live vars unchanged.

	% Make sure you get the output vars first, and the live vars second,
	% since this makes a significant difference to the output set of vars.

:- pred maybe_add_accurate_gc_typeinfos(module_info, proc_info,
	set(var), set(var), set(var)).
:- mode maybe_add_accurate_gc_typeinfos(in, in, in, in, out) is det.

maybe_add_accurate_gc_typeinfos(ModuleInfo, ProcInfo, OutVars,
	LiveVars1, LiveVars) :-
	module_info_globals(ModuleInfo, Globals),
	globals__get_gc_method(Globals, GC_Method),
	(
		GC_Method = accurate
	->
		proc_info_get_typeinfo_vars_setwise(ProcInfo, LiveVars1,
			TypeInfoVarsLive),
		proc_info_get_typeinfo_vars_setwise(ProcInfo, OutVars,
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
find_output_vars_2([Var - arg_info(_, Mode) | Rest], OutVars0, OutVars) :-
	(
		Mode = top_out
	->
		set__insert(OutVars0, Var, OutVars1)
	;
		OutVars1 = OutVars0
	),
	find_output_vars_2(Rest, OutVars1, OutVars).

%-----------------------------------------------------------------------------%

:- pred allocate_stack_slots(list(set(var)), code_model, stack_slots).
:- mode allocate_stack_slots(in, in, out) is det.

allocate_stack_slots(ColourList, CodeModel, StackSlots) :-
	map__init(StackSlots0),
	(
		CodeModel = model_non
	->
		First = 0
	;
		First = 1
	),
	allocate_stack_slots_2(ColourList, First, CodeModel,
		StackSlots0, StackSlots).

:- pred allocate_stack_slots_2(list(set(var)), int, code_model,
	stack_slots, stack_slots).
:- mode allocate_stack_slots_2(in, in, in, in, out) is det.

allocate_stack_slots_2([], _N, _CodeModel, StackSlots, StackSlots).
allocate_stack_slots_2([Vars | VarSets], N0, CodeModel,
		StackSlots0, StackSlots) :-
	set__to_sorted_list(Vars, VarList),
	(
		CodeModel = model_non
	->
		Slot = framevar(N0)
	;
		Slot = stackvar(N0)
	),
	allocate_same_stack_slot(VarList, Slot, StackSlots0, StackSlots1),
	N1 is N0 + 1,
	allocate_stack_slots_2(VarSets, N1, CodeModel,
		StackSlots1, StackSlots).

:- pred allocate_same_stack_slot(list(var), lval, stack_slots, stack_slots).
:- mode allocate_same_stack_slot(in, in, in, out) is det.

allocate_same_stack_slot([], _Slot, StackSlots, StackSlots).
allocate_same_stack_slot([Var | Vars], Slot, StackSlots0, StackSlots) :-
	map__det_insert(StackSlots0, Var, Slot, StackSlots1),
	allocate_same_stack_slot(Vars, Slot, StackSlots1, StackSlots).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
