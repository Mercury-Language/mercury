%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001 The University of Melbourne.
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

:- module ll_backend__live_vars.

:- interface.

:- import_module hlds__hlds_module, hlds__hlds_pred.

:- pred allocate_stack_slots_in_proc(proc_info::in, pred_id::in,
	module_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Parse tree modules
:- import_module parse_tree__prog_data.

% HLDS modules
:- import_module hlds__hlds_goal, hlds__hlds_data, check_hlds__mode_util.
:- import_module hlds__instmap, ll_backend__code_aux.
:- import_module ll_backend__liveness.

% Modules shared between different back-ends.
:- import_module backend_libs__code_model.

% LLDS modules
:- import_module ll_backend__llds, ll_backend__arg_info, libs__trace_params.
:- import_module ll_backend__trace.

% Misc
:- import_module libs__globals, libs__options, libs__graph_colour.


% Standard library modules
:- import_module list, map, set, std_util, assoc_list, bool.
:- import_module int, require.

%-----------------------------------------------------------------------------%

:- type alloc_data
	--->	alloc_data(
			module_info		::	module_info,
			proc_info		::	proc_info,
			typeinfo_liveness	::	bool
		).

allocate_stack_slots_in_proc(ProcInfo0, PredId, ModuleInfo, ProcInfo) :-
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_interface_code_model(ProcInfo0, CodeModel),

	initial_liveness(ProcInfo0, PredId, ModuleInfo, Liveness0),
	set__init(LiveSets0),
	module_info_globals(ModuleInfo, Globals),
	globals__get_trace_level(Globals, TraceLevel),
	( trace_level_is_none(TraceLevel) = no ->
		trace__fail_vars(ModuleInfo, ProcInfo0, ResumeVars0),
		set__insert(LiveSets0, ResumeVars0, LiveSets1)
	;
		set__init(ResumeVars0),
		LiveSets1 = LiveSets0
	),
	trace__do_we_need_maxfr_slot(Globals, ProcInfo0, ProcInfo1),
	trace__reserved_slots(ModuleInfo, ProcInfo1, Globals, NumReservedSlots,
		MaybeReservedVarInfo),
	( MaybeReservedVarInfo = yes(ResVar - _) ->
		set__singleton_set(ResVarSet, ResVar),
		set__insert(LiveSets1, ResVarSet, LiveSets2)
	;
		LiveSets2 = LiveSets1
	),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	body_should_use_typeinfo_liveness(PredInfo, Globals, TypeInfoLiveness),
	AllocData = alloc_data(ModuleInfo, ProcInfo1, TypeInfoLiveness),
	set__init(NondetLiveness0),
	build_live_sets_in_goal(Goal0, Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets2, AllocData,
		_Liveness, _NondetLiveness, LiveSets),
	graph_colour__group_elements(LiveSets, ColourSets),
	set__to_sorted_list(ColourSets, ColourList),
	allocate_stack_slots(ColourList, CodeModel, NumReservedSlots,
		MaybeReservedVarInfo, StackSlots),

	proc_info_set_stack_slots(ProcInfo1, StackSlots, ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The stack_slots structure (map(prog_var, lval)) is threaded through the
% traversal of the goal. The liveness information is computed from the liveness
% delta annotations.

:- pred build_live_sets_in_goal(hlds_goal::in,
	set(prog_var)::in, set(prog_var)::in, set(prog_var)::in,
	set(set(prog_var))::in, alloc_data::in,
	set(prog_var)::out, set(prog_var)::out, set(set(prog_var))::out) is det.

build_live_sets_in_goal(Goal - GoalInfo, Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, AllocData,
		Liveness, NondetLiveness, LiveSets) :-
	goal_info_get_pre_deaths(GoalInfo, PreDeaths),
	goal_info_get_pre_births(GoalInfo, PreBirths),
	goal_info_get_post_deaths(GoalInfo, PostDeaths),
	goal_info_get_post_births(GoalInfo, PostBirths),

	% note: we must be careful to apply deaths before births
	set__difference(Liveness0, PreDeaths, Liveness1),
	set__union(Liveness1, PreBirths, Liveness2),

	%
	% if the goal is atomic, we want to apply the postdeaths
	% before processing the goal, but if the goal is a compound
	% goal, then we want to apply them after processing it
	%
	( goal_is_atomic(Goal) ->
		set__difference(Liveness2, PostDeaths, Liveness3)
	;
		Liveness3 = Liveness2
	),

	goal_info_get_resume_point(GoalInfo, ResumePoint),
	(
		ResumePoint = resume_point(ResumePointVars, Locs),
		resume_locs_include_stack(Locs, yes)
	->
		set__union(ResumeVars0, ResumePointVars, ResumeVars1),
		set__union(ResumeVars1, NondetLiveness0, InterferingVars),
		set__insert(LiveSets0, InterferingVars, LiveSets1)
	;
		ResumeVars1 = ResumeVars0,
		LiveSets1 = LiveSets0
	),

	build_live_sets_in_goal_2(Goal, Liveness3,
		NondetLiveness0, ResumeVars1, LiveSets1, GoalInfo, AllocData,
		Liveness4, NondetLiveness, LiveSets),

	( goal_is_atomic(Goal) ->
		Liveness5 = Liveness4
	;
		set__difference(Liveness4, PostDeaths, Liveness5)
	),

	set__union(Liveness5, PostBirths, Liveness).

:- pred resume_locs_include_stack(resume_locs::in, bool::out) is det.

resume_locs_include_stack(orig_only, no).
resume_locs_include_stack(stack_only, yes).
resume_locs_include_stack(orig_and_stack, yes).
resume_locs_include_stack(stack_and_orig, yes).

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

:- pred build_live_sets_in_goal_2(hlds_goal_expr::in,
	set(prog_var)::in, set(prog_var)::in, set(prog_var)::in,
	set(set(prog_var))::in, hlds_goal_info::in, alloc_data::in,
	set(prog_var)::out,
	set(prog_var)::out, set(set(prog_var))::out) is det.

build_live_sets_in_goal_2(conj(Goals), Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, _, AllocData,
		Liveness, NondetLiveness, LiveSets) :-
	build_live_sets_in_conj(Goals, Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, AllocData,
		Liveness, NondetLiveness, LiveSets).

build_live_sets_in_goal_2(par_conj(Goals, _SM),
		Liveness0, NondetLiveness0, ResumeVars0, LiveSets0,
		GoalInfo, AllocData, Liveness, NondetLiveness, LiveSets) :-
	goal_info_get_code_gen_nonlocals(GoalInfo, NonLocals),
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
		% too. XXX No, it doesn't.
	build_live_sets_in_disj(Goals, Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets1, GoalInfo, AllocData,
		Liveness, NondetLiveness, LiveSets).

build_live_sets_in_goal_2(disj(Goals, _SM), Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, GoalInfo, AllocData,
		Liveness, NondetLiveness, LiveSets) :-
	build_live_sets_in_disj(Goals, Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, GoalInfo, AllocData,
		Liveness, NondetLiveness1, LiveSets),
	(
		Goals = [First | _],
		First = _ - FirstGoalInfo,
		goal_info_get_resume_point(FirstGoalInfo, ResumePoint),
		(
			ResumePoint = resume_point(ResumeVars, Locs),
				% If we can backtrack into the disjunction,
				% we must protect the stack slots needed by
				% any of its resumption points from being
				% reused in the following code. The first
				% resumption point's vars include all the
				% vars needed by all the resumption points.
				%
				% Note that we must check the disjunction's
				% code model, not any disjuncts'; the
				% disjunction as a whole can be model_non
				% without any disjunct being model_non.
			(
				resume_locs_include_stack(Locs, yes),
				goal_info_get_code_model(GoalInfo, model_non)
			->
				set__union(NondetLiveness1, ResumeVars,
					NondetLiveness)
			;
				NondetLiveness = NondetLiveness1
			)
		;
			ResumePoint = no_resume_point,
				% We can get here if the disjunction is
				% not really a disjunction, because the first
				% alternative cannot fail and will be committed
				% to (e.g. in a first-solution context).
				% Simplification should eliminate such
				% disjunctions, replacing them with the first
				% disjunct, but until that is done, we
				% must handle them here.
			NondetLiveness = NondetLiveness1
		)
	;
		Goals = [],
		NondetLiveness = NondetLiveness1
	).

build_live_sets_in_goal_2(switch(_Var, _CanFail, Cases, _SM), Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, _, AllocData,
		Liveness, NondetLiveness, LiveSets) :-
	build_live_sets_in_cases(Cases, Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, AllocData,
		Liveness, NondetLiveness, LiveSets).

build_live_sets_in_goal_2(if_then_else(_Vars, Cond, Then, Else, _SM),
		Liveness0, NondetLiveness0, ResumeVars0, LiveSets0,
		_, AllocData, Liveness, NondetLiveness, LiveSets) :-
	build_live_sets_in_goal(Cond, Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, AllocData,
		Liveness1, NondetLivenessCond, LiveSets1),
	build_live_sets_in_goal(Then, Liveness1,
		NondetLivenessCond, ResumeVars0, LiveSets1, AllocData,
		_Liveness2, NondetLivenessThen, LiveSets2),
	build_live_sets_in_goal(Else, Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets2, AllocData,
		Liveness, NondetLivenessElse, LiveSets),
	set__union(NondetLivenessThen, NondetLivenessElse, NondetLiveness).

build_live_sets_in_goal_2(not(Goal), Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, _, AllocData,
		Liveness, NondetLiveness, LiveSets) :-
	build_live_sets_in_goal(Goal, Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, AllocData,
		Liveness, NondetLiveness, LiveSets).

build_live_sets_in_goal_2(some(_Vars, _CR, Goal), Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, GoalInfo, AllocData,
		Liveness, NondetLiveness, LiveSets) :-
	build_live_sets_in_goal(Goal, Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, AllocData,
		Liveness, NondetLiveness1, LiveSets),

	% If the "some" goal cannot succeed more than once,
	% then execution cannot backtrack into the inner goal once control
	% has left it. Therefore the code following the "some" can reuse
	% any stack slots needed by nondet code in the inner goal.

	goal_info_get_code_model(GoalInfo, CodeModel),
	( CodeModel = model_non ->
		NondetLiveness = NondetLiveness1
	;
		NondetLiveness = NondetLiveness0
	).

build_live_sets_in_goal_2(generic_call(_GenericCall, ArgVars, Modes, Det),
		Liveness, NondetLiveness0, ResumeVars0, LiveSets0,
		GoalInfo, AllocData, Liveness, NondetLiveness, LiveSets) :-

	determinism_to_code_model(Det, CallModel),
	ProcInfo = AllocData^proc_info,
	proc_info_vartypes(ProcInfo, VarTypes),
	map__apply_to_list(ArgVars, VarTypes, Types),
	ModuleInfo = AllocData^module_info,
	make_arg_infos(Types, Modes, CallModel, ModuleInfo, ArgInfos),
	find_output_vars_from_arg_info(ArgVars, ArgInfos, OutVars),

	build_live_sets_in_call(Liveness, NondetLiveness0, ResumeVars0,
		LiveSets0, OutVars, GoalInfo, AllocData, NondetLiveness,
		LiveSets).

build_live_sets_in_goal_2(call(PredId, ProcId, ArgVars, Builtin, _, _),
		Liveness, NondetLiveness0, ResumeVars0, LiveSets0,
		GoalInfo, AllocData, Liveness, NondetLiveness, LiveSets) :-
	( Builtin = inline_builtin ->
		NondetLiveness = NondetLiveness0,
		LiveSets = LiveSets0
	;
		ModuleInfo = AllocData^module_info,
		find_output_vars(PredId, ProcId, ArgVars, ModuleInfo, OutVars),
		build_live_sets_in_call(Liveness, NondetLiveness0,
			ResumeVars0, LiveSets0, OutVars, GoalInfo, AllocData,
			NondetLiveness, LiveSets)
	).

build_live_sets_in_goal_2(Goal, Liveness, NondetLiveness,
		_ResumeVars0, LiveSets, _GoalInfo, _,
		Liveness, NondetLiveness, LiveSets) :-
	Goal = unify(_, _, _, Unification, _),
	( Unification = complicated_unify(_, _, _) ->
		error("build_live_sets_in_goal_2: complicated_unify")
	;
		true
	).

build_live_sets_in_goal_2(foreign_proc(Attributes,
		PredId, ProcId, Args, _, _, _),
		Liveness, NondetLiveness0, ResumeVars0, LiveSets0,
		GoalInfo, AllocData,
		Liveness, NondetLiveness, LiveSets) :-
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
		NondetLiveness = NondetLiveness0,
		LiveSets = LiveSets0
	;
		% The variables which need to be saved onto the stack
		% before the call are all the variables that are live
		% after the call (except for the output arguments produced
		% by the call), plus all the variables that may be needed
		% at an enclosing resumption point.

		ModuleInfo = AllocData^module_info,
		find_output_vars(PredId, ProcId, Args, ModuleInfo, OutVars),
		build_live_sets_in_call(Liveness, NondetLiveness0,
			ResumeVars0, LiveSets0, OutVars, GoalInfo, AllocData,
			NondetLiveness, LiveSets)
	).

build_live_sets_in_goal_2(shorthand(_), _, _, _, _, _, _, _, _, _)
		:-
	% these should have been expanded out by now
	error("build_live_sets_in_goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

	% The variables which need to be saved onto the stack,
	% directly or indirectly, before a call or may_call_mercury
	% pragma_c_code are all the variables that are live after the goal
	% except for the output arguments produced by the goal, plus all the
	% variables that may be needed at an enclosing resumption point.

:- pred build_live_sets_in_call(set(prog_var)::in, set(prog_var)::in,
	set(prog_var)::in, set(set(prog_var))::in, set(prog_var)::in,
	hlds_goal_info::in, alloc_data::in, set(prog_var)::out,
	set(set(prog_var))::out) is det.

build_live_sets_in_call(Liveness, NondetLiveness0, ResumeVars0, LiveSets0,
		OutVars, GoalInfo, AllocData, NondetLiveness, LiveSets) :-

	set__difference(Liveness, OutVars, StackVars0),

	% Might need to add more live variables with alternate liveness
	% calculation.

	maybe_add_alternate_liveness_typeinfos(AllocData^proc_info,
		AllocData^typeinfo_liveness, OutVars, StackVars0, StackVars),

	set__union(ResumeVars0, NondetLiveness0, OtherStackDemands),
	set__union(StackVars, OtherStackDemands, InterferingVars),
	set__insert(LiveSets0, InterferingVars, LiveSets),

	% If this is a nondet call, then all the stack slots we need
	% must be protected against reuse in following code.

	goal_info_get_code_model(GoalInfo, CodeModel),
	( CodeModel = model_non ->
		set__union(NondetLiveness0, StackVars, NondetLiveness)
	;
		NondetLiveness = NondetLiveness0
	).

%-----------------------------------------------------------------------------%

:- pred build_live_sets_in_conj(list(hlds_goal)::in,
	set(prog_var)::in, set(prog_var)::in, set(prog_var)::in,
	set(set(prog_var))::in, alloc_data::in, set(prog_var)::out,
	set(prog_var)::out, set(set(prog_var))::out) is det.

build_live_sets_in_conj([], Liveness, NondetLiveness, _, LiveSets, _,
		Liveness, NondetLiveness, LiveSets).
build_live_sets_in_conj([Goal | Goals], Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, AllocData,
		Liveness, NondetLiveness, LiveSets) :-
	(
		Goal = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
		instmap_delta_is_unreachable(InstMapDelta)
	->
		build_live_sets_in_goal(Goal, Liveness0,
			NondetLiveness0, ResumeVars0, LiveSets0, AllocData,
			Liveness, NondetLiveness, LiveSets)
	;
		build_live_sets_in_goal(Goal, Liveness0,
			NondetLiveness0, ResumeVars0, LiveSets0, AllocData,
			Liveness1, NondetLiveness1, LiveSets1),
		build_live_sets_in_conj(Goals, Liveness1,
			NondetLiveness1, ResumeVars0, LiveSets1, AllocData,
			Liveness, NondetLiveness, LiveSets)
	).

%-----------------------------------------------------------------------------%

	% build_live_sets_in_disj is used for both disjunctions and
	% parallel conjunctions.

:- pred build_live_sets_in_disj(list(hlds_goal)::in,
	set(prog_var)::in, set(prog_var)::in, set(prog_var)::in,
	set(set(prog_var))::in, hlds_goal_info::in, alloc_data::in,
	set(prog_var)::out, set(prog_var)::out,
	set(set(prog_var))::out) is det.

build_live_sets_in_disj([], Liveness, NondetLiveness, _, LiveSets, _, _,
		Liveness, NondetLiveness, LiveSets).
build_live_sets_in_disj([Goal | Goals], Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, GoalInfo, AllocData,
		Liveness, NondetLiveness, LiveSets) :-
	build_live_sets_in_goal(Goal, Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, AllocData,
		Liveness, NondetLiveness1, LiveSets1),
	build_live_sets_in_disj(Goals, Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets1, GoalInfo, AllocData,
		_Liveness2, NondetLiveness2, LiveSets),
	goal_info_get_code_model(GoalInfo, CodeModel),
	( CodeModel = model_non ->
			% NondetLiveness should be a set of prog_var sets.
			% Insteading of taking the union of the NondetLive sets
			% at the ends of disjuncts, we should just keep them
			% in this set of sets.
		set__union(NondetLiveness1, NondetLiveness2, NondetLiveness3),
		goal_info_get_resume_point(GoalInfo, Resume),
		(
			Resume = resume_point(ResumePointVars, Locs),
			resume_locs_include_stack(Locs, yes)
		->
			set__union(NondetLiveness3, ResumePointVars,
				NondetLiveness)
		;
			NondetLiveness = NondetLiveness3
		)
	;
		NondetLiveness = NondetLiveness0
	).

%-----------------------------------------------------------------------------%

:- pred build_live_sets_in_cases(list(case)::in,
	set(prog_var)::in, set(prog_var)::in, set(prog_var)::in,
	set(set(prog_var))::in, alloc_data::in, set(prog_var)::out,
	set(prog_var)::out, set(set(prog_var))::out) is det.

build_live_sets_in_cases([], Liveness, NondetLiveness, _, LiveSets,
		_, Liveness, NondetLiveness, LiveSets).
build_live_sets_in_cases([case(_Cons, Goal) | Cases],
		Liveness0, NondetLiveness0, ResumeVars0, LiveSets0, AllocData,
		Liveness, NondetLiveness, LiveSets) :-
	build_live_sets_in_goal(Goal, Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets0, AllocData,
		Liveness, NondetLiveness1, LiveSets1),
	build_live_sets_in_cases(Cases, Liveness0,
		NondetLiveness0, ResumeVars0, LiveSets1, AllocData,
		_Liveness2, NondetLiveness2, LiveSets),
	set__union(NondetLiveness1, NondetLiveness2, NondetLiveness).

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

:- pred maybe_add_alternate_liveness_typeinfos(proc_info::in, bool::in,
	set(prog_var)::in, set(prog_var)::in, set(prog_var)::out) is det.

maybe_add_alternate_liveness_typeinfos(ProcInfo, TypeInfoLiveness, OutVars,
		LiveVars1, LiveVars) :-
	(
		TypeInfoLiveness = yes
	->
		proc_info_vartypes(ProcInfo, VarTypes),
		proc_info_typeinfo_varmap(ProcInfo, TVarMap),
		proc_info_get_typeinfo_vars(LiveVars1, VarTypes, TVarMap,
			TypeInfoVarsLive),
		proc_info_get_typeinfo_vars(OutVars, VarTypes, TVarMap,
			TypeInfoVarsOut),
		set__union(LiveVars1, TypeInfoVarsOut, LiveVars2),
		set__union(LiveVars2, TypeInfoVarsLive, LiveVars)
	;
		LiveVars = LiveVars1
	).

%-----------------------------------------------------------------------------%

:- pred find_output_vars(pred_id::in, proc_id::in, list(prog_var)::in,
	module_info::in, set(prog_var)::out) is det.

find_output_vars(PredId, ProcId, ArgVars, ModuleInfo, OutVars) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_arg_info(ProcInfo, ArgInfo),
	find_output_vars_from_arg_info(ArgVars, ArgInfo, OutVars).

:- pred find_output_vars_from_arg_info(list(prog_var)::in, list(arg_info)::in,
	set(prog_var)::out) is det.

find_output_vars_from_arg_info(ArgVars, ArgInfo, OutVars) :-
	assoc_list__from_corresponding_lists(ArgVars, ArgInfo, ArgPairs),
	set__init(OutVars0),
	find_output_vars_2(ArgPairs, OutVars0, OutVars).

:- pred find_output_vars_2(assoc_list(prog_var, arg_info)::in,
	set(prog_var)::in, set(prog_var)::out) is det.

find_output_vars_2([], OutVars, OutVars).
find_output_vars_2([Var - arg_info(_, Mode) | Rest], OutVars0, OutVars) :-
	( Mode = top_out ->
		set__insert(OutVars0, Var, OutVars1)
	;
		OutVars1 = OutVars0
	),
	find_output_vars_2(Rest, OutVars1, OutVars).

%-----------------------------------------------------------------------------%

:- pred allocate_stack_slots(list(set(prog_var))::in, code_model::in, int::in,
	maybe(pair(prog_var, int))::in, stack_slots::out) is det.

allocate_stack_slots(ColourList, CodeModel, NumReservedSlots,
		MaybeReservedVarInfo, StackSlots) :-
	map__init(StackSlots0),
		% The reserved slots are referred to by fixed number
		% (e.g. framevar(1)) in trace__setup.
	FirstVarSlot is 1 + NumReservedSlots,
	allocate_stack_slots_2(ColourList, CodeModel, FirstVarSlot,
		MaybeReservedVarInfo, StackSlots0, StackSlots).

:- pred allocate_stack_slots_2(list(set(prog_var))::in, code_model::in,
	int::in, maybe(pair(prog_var, int))::in,
	stack_slots::in, stack_slots::out) is det.

allocate_stack_slots_2([], _, _, _, StackSlots, StackSlots).
allocate_stack_slots_2([Vars | VarSets], CodeModel, N0, MaybeReservedVarInfo,
		StackSlots0, StackSlots) :-
	(
		MaybeReservedVarInfo = yes(ResVar - ResSlotNum),
		set__member(ResVar, Vars)
	->
		SlotNum = ResSlotNum,
		N1 = N0
	;
		SlotNum = N0,
		N1 = N0 + 1
	),
	( CodeModel = model_non ->
		Slot = framevar(SlotNum)
	;
		Slot = stackvar(SlotNum)
	),
	set__to_sorted_list(Vars, VarList),
	allocate_same_stack_slot(VarList, Slot, StackSlots0, StackSlots1),
	allocate_stack_slots_2(VarSets, CodeModel, N1, MaybeReservedVarInfo,
		StackSlots1, StackSlots).

:- pred allocate_same_stack_slot(list(prog_var)::in, lval::in, stack_slots::in,
	stack_slots::out) is det.

allocate_same_stack_slot([], _Slot, StackSlots, StackSlots).
allocate_same_stack_slot([Var | Vars], Slot, StackSlots0, StackSlots) :-
	map__det_insert(StackSlots0, Var, Slot, StackSlots1),
	allocate_same_stack_slot(Vars, Slot, StackSlots1, StackSlots).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
