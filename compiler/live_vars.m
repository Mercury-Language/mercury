%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1999 The University of Melbourne.
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

:- pred allocate_stack_slots_in_proc(proc_info, pred_id, module_info,
	proc_info).
:- mode allocate_stack_slots_in_proc(in, in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module llds, arg_info, prog_data, hlds_goal, hlds_data, mode_util.
:- import_module liveness, code_aux, globals, graph_colour, instmap, options.
:- import_module trace.
:- import_module list, map, set, std_util, assoc_list, bool.
:- import_module int, require.

%-----------------------------------------------------------------------------%

allocate_stack_slots_in_proc(ProcInfo0, PredId, ModuleInfo, ProcInfo) :-
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_interface_code_model(ProcInfo0, CodeModel),

	initial_liveness(ProcInfo0, PredId, ModuleInfo, Liveness0, _Refs),
	set__init(LiveSets0),
	module_info_globals(ModuleInfo, Globals),
	globals__get_trace_level(Globals, TraceLevel),
	( TraceLevel \= none ->
		trace__fail_vars(ModuleInfo, ProcInfo0, ResumeVars0),
		set__insert(LiveSets0, ResumeVars0, LiveSets1)
	;
		set__init(ResumeVars0),
		LiveSets1 = LiveSets0
	),
	trace__reserved_slots(ProcInfo0, Globals, NumReservedSlots),
	build_live_sets_in_goal(Goal0, Liveness0, ResumeVars0, LiveSets1,
		ModuleInfo, ProcInfo0, _Liveness, _ResumeVars, LiveSets),
	graph_colour__group_elements(LiveSets, ColourSets),
	set__to_sorted_list(ColourSets, ColourList),
	allocate_stack_slots(ColourList, CodeModel, NumReservedSlots,
		StackSlots),

	proc_info_set_stack_slots(ProcInfo0, StackSlots, ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The stack_slots structure (map(prog_var, lval)) is threaded through the
% traversal of the goal. The liveness information is computed from the liveness
% delta annotations.

:- pred build_live_sets_in_goal(hlds_goal, set(prog_var), set(prog_var),
	set(set(prog_var)), module_info, proc_info, set(prog_var),
	set(prog_var), set(set(prog_var))).
:- mode build_live_sets_in_goal(in, in, in, in, in, in, out, out, out) is det.

build_live_sets_in_goal(Goal0 - GoalInfo, Liveness0, ResumeVars0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, ResumeVars, LiveSets) :-
	% note: we must be careful to apply deaths before births
	goal_info_get_pre_deaths(GoalInfo, PreDeaths),
	goal_info_get_pre_births(GoalInfo, PreBirths),
	goal_info_get_post_deaths(GoalInfo, PostDeaths),
	goal_info_get_post_births(GoalInfo, PostBirths),

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
		semidet_fail
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

:- pred build_live_sets_in_goal_2(hlds_goal_expr, set(prog_var), set(prog_var),
	set(set(prog_var)), hlds_goal_info, module_info, proc_info,
	set(prog_var), set(prog_var), set(set(prog_var))).
:- mode build_live_sets_in_goal_2(in, in, in, in, in, in, in, out, out, out)
	is det.

build_live_sets_in_goal_2(conj(Goals0), Liveness0, ResumeVars0, LiveSets0,
		_, ModuleInfo, ProcInfo, Liveness, ResumeVars, LiveSets) :-
	build_live_sets_in_conj(Goals0, Liveness0, ResumeVars0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, ResumeVars, LiveSets).

build_live_sets_in_goal_2(par_conj(Goals0, _SM), Liveness0, ResumeVars0,
		LiveSets0, GoalInfo, ModuleInfo, ProcInfo, Liveness,
		ResumeVars, LiveSets) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	set__union(NonLocals, Liveness0, LiveSet),
		% We insert all the union of the live vars and the nonlocals.
		% Since each parallel conjunct may be run on a different
		% Mercury engine to the current engine, we must save all
		% the variables that are live or nonlocal to the parallel
		% conjunction. Nonlocal variables that are currently free, but
		% are bound inside one of the conjuncts need a stackslot
		% because they are passed out by reference to that stackslot.
	set__insert(LiveSets0, LiveSet, LiveSets1),
		% build_live_sets_in_disj treats its list of goals as a list
		% of independent goals, so we can use it for parallel conj's
		% too.
	build_live_sets_in_disj(Goals0, Liveness0, ResumeVars0, LiveSets1,
		GoalInfo, ModuleInfo, ProcInfo, Liveness, ResumeVars, LiveSets).

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

build_live_sets_in_goal_2(higher_order_call(_PredVar, ArgVars,
				Types, argument_modes(IKT, Modes), Det,
				_IsPredOrFunc),
		Liveness, ResumeVars0, LiveSets0,
		GoalInfo, ModuleInfo, ProcInfo,
		Liveness, ResumeVars, LiveSets) :-
	% The variables which need to be saved onto the stack
	% before the call are all the variables that are live
	% after the call, except for the output arguments produced
	% by the call, plus all the variables that may be needed
	% at an enclosing resumption point.

	determinism_to_code_model(Det, CallModel),
	instmap__init_reachable(BogusInstMap),
	make_arg_infos(Types, Modes, CallModel, BogusInstMap, IKT,
			ModuleInfo, ArgInfos),
	find_output_vars_from_arg_info(ArgVars, ArgInfos, OutVars),
	set__difference(Liveness, OutVars, InputLiveness),
	set__union(InputLiveness, ResumeVars0, StackVars0),

	% Might need to add more live variables with alternate liveness
	% calculation.

	maybe_add_alternate_liveness_typeinfos(ModuleInfo, ProcInfo,
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

	% Code duplication. Ulch.
build_live_sets_in_goal_2(class_method_call(_, _, ArgVars, Types, Modes, Det),
		Liveness, ResumeVars0, LiveSets0,
		GoalInfo, ModuleInfo, ProcInfo,
		Liveness, ResumeVars, LiveSets) :-
	% The variables which need to be saved onto the stack
	% before the call are all the variables that are live
	% after the call, except for the output arguments produced
	% by the call, plus all the variables that may be needed
	% at an enclosing resumption point.

	determinism_to_code_model(Det, CallModel),
	Modes = argument_modes(ArgInstTable, ArgModes),
	instmap__init_reachable(BogusInstMap),
	make_arg_infos(Types, ArgModes, CallModel, BogusInstMap,
			ArgInstTable, ModuleInfo, ArgInfos),
	find_output_vars_from_arg_info(ArgVars, ArgInfos, OutVars),
	set__difference(Liveness, OutVars, InputLiveness),
	set__union(InputLiveness, ResumeVars0, StackVars0),

	% Might need to add more live variables with alternate liveness
	% calculation.

	maybe_add_alternate_liveness_typeinfos(ModuleInfo, ProcInfo,
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

		% Might need to add more live variables with alternate
		% liveness calculation.

		maybe_add_alternate_liveness_typeinfos(ModuleInfo,
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
		D = complicated_unify(_, _, _)
	->
			% we have to save all live and protected variables
			% across complicated unifications.
		set__union(Liveness, ResumeVars0, LiveVars),
		set__insert(LiveSets0, LiveVars, LiveSets)
	;
		LiveSets = LiveSets0
	).

build_live_sets_in_goal_2(pragma_c_code(Attributes, PredId, ProcId,
		Args, _, _, _), Liveness, ResumeVars0, LiveSets0,
		GoalInfo, ModuleInfo, ProcInfo,
		Liveness, ResumeVars, LiveSets) :-

	goal_info_get_code_model(GoalInfo, CodeModel),
	(
		% We don't need to save any variables onto the stack
		% before a pragma_c_code if we know that it can't
		% succeed more than once and that it is not going
		% to call back Mercury code, because such pragma C code
		% won't clobber the registers.

		CodeModel \= model_non,
		may_call_mercury(Attributes, will_not_call_mercury)
	->
		ResumeVars = ResumeVars0,
		LiveSets = LiveSets0
	;
		% The variables which need to be saved onto the stack
		% before the call are all the variables that are live
		% after the call (except for the output arguments produced
		% by the call), plus all the variables that may be needed
		% at an enclosing resumption point.

		find_output_vars(PredId, ProcId, Args, ModuleInfo, OutVars),
		set__difference(Liveness, OutVars, InputLiveness),
		set__union(InputLiveness, ResumeVars0, StackVars0),

		% Might need to add more live variables with alternate
		% liveness calculation.

		maybe_add_alternate_liveness_typeinfos(ModuleInfo,
			ProcInfo, OutVars, StackVars0, StackVars),

		set__insert(LiveSets0, StackVars, LiveSets),

		( CodeModel = model_non ->
			ResumeVars = StackVars		% includes ResumeVars0
		;
			ResumeVars = ResumeVars0
		)
	).

%-----------------------------------------------------------------------------%

:- pred build_live_sets_in_conj(list(hlds_goal), set(prog_var), set(prog_var),
	set(set(prog_var)), module_info, proc_info, set(prog_var),
	set(prog_var), set(set(prog_var))).
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

	% build_live_sets_in_disj is used for both disjunctions and
	% parallel conjunctions.

:- pred build_live_sets_in_disj(list(hlds_goal), set(prog_var), set(prog_var),
	set(set(prog_var)), hlds_goal_info, module_info, proc_info,
	set(prog_var), set(prog_var), set(set(prog_var))).
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

:- pred build_live_sets_in_cases(list(case), set(prog_var), set(prog_var),
	set(set(prog_var)), module_info, proc_info, set(prog_var),
	set(prog_var), set(set(prog_var))).
:- mode build_live_sets_in_cases(in, in, in, in, in, in, out, out, out) is det.

build_live_sets_in_cases([], Liveness, ResumeVars, LiveSets, _, _,
		Liveness, ResumeVars, LiveSets).
build_live_sets_in_cases([case(_Cons, _IMDelta, Goal0) | Goals0],
		Liveness0, ResumeVars0, LiveSets0, ModuleInfo, ProcInfo,
		Liveness, ResumeVars, LiveSets) :-
	build_live_sets_in_goal(Goal0, Liveness0, ResumeVars0, LiveSets0,
		ModuleInfo, ProcInfo, Liveness, ResumeVars1, LiveSets1),
	build_live_sets_in_cases(Goals0, Liveness0, ResumeVars0, LiveSets1,
		ModuleInfo, ProcInfo, _Liveness2, ResumeVars2, LiveSets),
	set__union(ResumeVars1, ResumeVars2, ResumeVars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% If doing alternate liveness calculation, any typeinfos for
	% output variables or live variables are also live.
	% This is because if you want to examine the live data, you need to
	% know what shape the polymorphic args of the variables
	% are, so you need the typeinfos to be present on the stack.

	% The live variables obviously need their typeinfos
	% live, but the output variables also need their typeinfos
	% saved (otherwise we would throw out typeinfos and might
	% need one at a continuation point just after a call).

	% maybe_add_alternate_liveness_typeinfos takes a set of vars
	% (output vars) and a set of live vars and if we
	% are doing alternate liveness, adds the appropriate typeinfo
	% variables to the set of variables. If not, it returns the live
	% vars unchanged.

	% Make sure you get the output vars first, and the live vars second,
	% since this makes a significant difference to the output set of vars.

:- pred maybe_add_alternate_liveness_typeinfos(module_info, proc_info,
	set(prog_var), set(prog_var), set(prog_var)).
:- mode maybe_add_alternate_liveness_typeinfos(in, in, in, in, out) is det.

maybe_add_alternate_liveness_typeinfos(ModuleInfo, ProcInfo, OutVars,
		LiveVars1, LiveVars) :-
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, typeinfo_liveness,
		TypeinfoLiveness),
	(
		TypeinfoLiveness = yes
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

:- pred find_output_vars(pred_id, proc_id, list(prog_var), module_info,
		set(prog_var)).
:- mode find_output_vars(in, in, in, in, out) is det.

find_output_vars(PredId, ProcId, ArgVars, ModuleInfo, OutVars) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_arg_info(ProcInfo, ArgInfo),
	find_output_vars_from_arg_info(ArgVars, ArgInfo, OutVars).

:- pred find_output_vars_from_arg_info(list(prog_var), list(arg_info),
		set(prog_var)).
:- mode find_output_vars_from_arg_info(in, in, out) is det.

find_output_vars_from_arg_info(ArgVars, ArgInfo, OutVars) :-
	assoc_list__from_corresponding_lists(ArgVars, ArgInfo, ArgPairs),
	set__init(OutVars0),
	find_output_vars_2(ArgPairs, OutVars0, OutVars).

:- pred find_output_vars_2(assoc_list(prog_var, arg_info), set(prog_var),
		set(prog_var)).
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

:- pred allocate_stack_slots(list(set(prog_var)), code_model, int, stack_slots).
:- mode allocate_stack_slots(in, in, in, out) is det.

allocate_stack_slots(ColourList, CodeModel, NumReservedSlots, StackSlots) :-
	map__init(StackSlots0),
		% The reserved slots are referred to by fixed number
		% (e.g. framevar(1)) in trace__setup.
	FirstVarSlot is 1 + NumReservedSlots,
	allocate_stack_slots_2(ColourList, FirstVarSlot, CodeModel,
		StackSlots0, StackSlots).

:- pred allocate_stack_slots_2(list(set(prog_var)), int, code_model,
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

:- pred allocate_same_stack_slot(list(prog_var), lval, stack_slots,
		stack_slots).
:- mode allocate_same_stack_slot(in, in, in, out) is det.

allocate_same_stack_slot([], _Slot, StackSlots, StackSlots).
allocate_same_stack_slot([Var | Vars], Slot, StackSlots0, StackSlots) :-
	map__det_insert(StackSlots0, Var, Slot, StackSlots1),
	allocate_same_stack_slot(Vars, Slot, StackSlots1, StackSlots).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
