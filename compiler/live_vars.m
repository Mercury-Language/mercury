%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File live_vars.m
%
% Main authors: conway, zs.
%
% This module finds out what variables need to be saved across calls,
% across goals that may fail, and in parallel conjunctions. It then does those
% things with that information. First, it attaches that information to the
% relevant goal as a LLDS-backend-specific annotation. Second, it invokes
% the relevant type class method of the allocator-specific data structure
% it is passed; the basic stack slot allocator and the optimizing stack slot
% allocator pass different instances of this type class.
%
%-----------------------------------------------------------------------------%

:- module ll_backend__live_vars.

:- interface.

% Parse tree modules
:- import_module parse_tree__prog_data.

% HLDS modules
:- import_module hlds__hlds_goal, hlds__hlds_pred, hlds__hlds_module.
:- import_module hlds__hlds_llds.

% Standard library modules
:- import_module bool, set.

:- type alloc_data
	--->	alloc_data(
			module_info		::	module_info,
			proc_info		::	proc_info,
			typeinfo_liveness	::	bool,
			opt_no_return_calls	::	bool
		).

:- typeclass stack_alloc_info(T) where [
	pred at_call_site(need_across_call::in, hlds_goal_info::in,
		T::in, T::out) is det,
	pred at_resume_site(need_in_resume::in, hlds_goal_info::in,
		T::in, T::out) is det,
	pred at_par_conj(need_in_par_conj::in, hlds_goal_info::in,
		T::in, T::out) is det
].

:- pred build_live_sets_in_goal(hlds_goal::in, T::in,
	set(prog_var)::in, set(prog_var)::in,
	set(prog_var)::in, alloc_data::in,
	hlds_goal::out, T::out, set(prog_var)::out, set(prog_var)::out)
	is det <= stack_alloc_info(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_goal, hlds__hlds_llds, hlds__hlds_data.
:- import_module hlds__instmap.
:- import_module check_hlds__mode_util.
:- import_module ll_backend__llds, ll_backend__arg_info.
:- import_module ll_backend__liveness, ll_backend__code_aux.
:- import_module backend_libs__code_model.

:- import_module int, list, assoc_list, map, std_util, require.

%-----------------------------------------------------------------------------%

% The stack_slots structure (map(prog_var, lval)) is threaded through the
% traversal of the goal. The liveness information is computed from the liveness
% delta annotations.

build_live_sets_in_goal(Goal0 - GoalInfo0, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		Goal - GoalInfo, StackAlloc, Liveness, NondetLiveness) :-
	goal_info_get_pre_deaths(GoalInfo0, PreDeaths),
	goal_info_get_pre_births(GoalInfo0, PreBirths),
	goal_info_get_post_deaths(GoalInfo0, PostDeaths),
	goal_info_get_post_births(GoalInfo0, PostBirths),

	% note: we must be careful to apply deaths before births
	set__difference(Liveness0, PreDeaths, Liveness1),
	set__union(Liveness1, PreBirths, Liveness2),

	%
	% If the goal is atomic, we want to apply the postdeaths
	% before processing the goal, but if the goal is a compound
	% goal, then we want to apply them after processing it.
	%
	( goal_is_atomic(Goal0) ->
		set__difference(Liveness2, PostDeaths, Liveness3)
	;
		Liveness3 = Liveness2
	),

	goal_info_get_resume_point(GoalInfo0, ResumePoint),
	(
		ResumePoint = no_resume_point,
		ResumeVars1 = ResumeVars0,
		GoalInfo1 = GoalInfo0,
		StackAlloc1 = StackAlloc0
	;
		ResumePoint = resume_point(ResumePointVars, Locs),
		( resume_locs_include_stack(Locs, yes) ->
			set__union(ResumeVars0, ResumePointVars, ResumeVars1),
			ResumeOnStack = yes
		;
			ResumeVars1 = ResumeVars0,
			ResumeOnStack = no
		),
		NeedInResume = need_in_resume(ResumeOnStack,
			ResumeVars1, NondetLiveness0),
		record_resume_site(NeedInResume, GoalInfo0, GoalInfo1,
			StackAlloc0, StackAlloc1)
	),

	build_live_sets_in_goal_2(Goal0, GoalInfo1, StackAlloc1,
		Liveness3, NondetLiveness0, ResumeVars1, AllocData,
		Goal, GoalInfo, StackAlloc, Liveness4, NondetLiveness),

	( goal_is_atomic(Goal0) ->
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
	% `SaveInfo' is the interference graph, i.e. the set of sets
	% of variables which need to be on the stack at the same time.

:- pred build_live_sets_in_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
	T::in, set(prog_var)::in, set(prog_var)::in,
	set(prog_var)::in, alloc_data::in,
	hlds_goal_expr::out, hlds_goal_info::out, T::out,
	set(prog_var)::out, set(prog_var)::out)
	is det <= stack_alloc_info(T).

build_live_sets_in_goal_2(conj(Goals0), GoalInfo, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		conj(Goals), GoalInfo, StackAlloc, Liveness, NondetLiveness) :-
	build_live_sets_in_conj(Goals0, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		Goals, StackAlloc, Liveness, NondetLiveness).

build_live_sets_in_goal_2(par_conj(Goals0), GoalInfo0, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		par_conj(Goals), GoalInfo, StackAlloc,
		Liveness, NondetLiveness) :-
	goal_info_get_code_gen_nonlocals(GoalInfo0, NonLocals),
	set__union(NonLocals, Liveness0, LiveSet),
		% Since each parallel conjunct may be run on a different
		% Mercury engine to the current engine, we must save all
		% the variables that are live or nonlocal to the parallel
		% conjunction. Nonlocal variables that are currently free, but
		% are bound inside one of the conjuncts need a stackslot
		% because they are passed out by reference to that stackslot.
	NeedInParConj = need_in_par_conj(LiveSet),
	record_par_conj(NeedInParConj, GoalInfo0, GoalInfo,
		StackAlloc0, StackAlloc1),
	build_live_sets_in_par_conj(Goals0, StackAlloc1,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		Goals, StackAlloc, Liveness, NondetLiveness).

build_live_sets_in_goal_2(disj(Goals0), GoalInfo, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		disj(Goals), GoalInfo, StackAlloc, Liveness, NondetLiveness) :-
	build_live_sets_in_disj(Goals0, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, GoalInfo, AllocData,
		Goals, StackAlloc, Liveness, NondetLiveness1),
	(
		Goals = [First | _],
		First = _ - FirstGoalInfo,
		goal_info_get_resume_point(FirstGoalInfo, ResumePoint),
		(
			ResumePoint = resume_point(ResumeVars, _Locs),
				% If we can backtrack into the disjunction,
				% we must protect the stack slots needed by
				% any of its resumption points from being
				% reused in the following code. The first
				% resumption point's vars include all the
				% vars needed by all the resumption points.
				% However, the first disjunct can be orig_only
				% while later disjuncts are include the stack.
				
				% Note that we must check the disjunction's
				% code model, not any disjuncts'; the
				% disjunction as a whole can be model_non
				% without any disjunct being model_non.
			(
				goal_info_get_code_model(GoalInfo, model_non),
				some [Disjunct] (
					list__member(Disjunct, Goals),
					Disjunct = _ - DisjunctGoalInfo,
					goal_info_get_resume_point(
						DisjunctGoalInfo,
						DisjunctResumePoint),
					DisjunctResumePoint =
						resume_point(_, Locs),
					resume_locs_include_stack(Locs, yes)
				)
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

build_live_sets_in_goal_2(switch(Var, CanFail, Cases0), GoalInfo, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		switch(Var, CanFail, Cases), GoalInfo, StackAlloc,
		Liveness, NondetLiveness) :-
	build_live_sets_in_cases(Cases0, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		Cases, StackAlloc, Liveness, NondetLiveness).

build_live_sets_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), GoalInfo,
		StackAlloc0, Liveness0, NondetLiveness0,
		ResumeVars0, AllocData,
		if_then_else(Vars, Cond, Then, Else), GoalInfo, StackAlloc,
		Liveness, NondetLiveness) :-
	build_live_sets_in_goal(Cond0, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		Cond, StackAlloc1, LivenessCond, NondetLivenessCond),
	build_live_sets_in_goal(Then0, StackAlloc1,
		LivenessCond, NondetLivenessCond, ResumeVars0, AllocData,
		Then, StackAlloc2, _LivenessThen, NondetLivenessThen),
	build_live_sets_in_goal(Else0, StackAlloc2,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		Else, StackAlloc, Liveness, NondetLivenessElse),
	set__union(NondetLivenessThen, NondetLivenessElse, NondetLiveness).

build_live_sets_in_goal_2(not(Goal0), GoalInfo, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		not(Goal), GoalInfo, StackAlloc, Liveness, NondetLiveness) :-
	build_live_sets_in_goal(Goal0, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		Goal, StackAlloc, Liveness, NondetLiveness).

build_live_sets_in_goal_2(some(Vars, CR, Goal0), GoalInfo, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		some(Vars, CR, Goal), GoalInfo, StackAlloc,
		Liveness, NondetLiveness) :-
	build_live_sets_in_goal(Goal0, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		Goal, StackAlloc, Liveness, NondetLiveness1),

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

build_live_sets_in_goal_2(Goal, GoalInfo0, StackAlloc0,
		Liveness, NondetLiveness0, ResumeVars0, AllocData,
		Goal, GoalInfo, StackAlloc, Liveness, NondetLiveness) :-
	Goal = generic_call(_GenericCall, ArgVars, Modes, _Det),
	ProcInfo = AllocData ^ proc_info,
	proc_info_vartypes(ProcInfo, VarTypes),
	map__apply_to_list(ArgVars, VarTypes, Types),
	ModuleInfo = AllocData ^ module_info,
	arg_info__partition_generic_call_args(ModuleInfo, ArgVars,
		Types, Modes, _InVars, OutVars, _UnusedVars),

	build_live_sets_in_call(OutVars, GoalInfo0, StackAlloc0,
		Liveness, NondetLiveness0, ResumeVars0, AllocData,
		GoalInfo, StackAlloc, NondetLiveness).

build_live_sets_in_goal_2(Goal, GoalInfo0, StackAlloc0,
		Liveness, NondetLiveness0, ResumeVars0, AllocData,
		Goal, GoalInfo, StackAlloc, Liveness, NondetLiveness) :-
	Goal = call(PredId, ProcId, ArgVars, Builtin, _, _),
	ModuleInfo = AllocData ^ module_info,
	CallerProcInfo = AllocData ^ proc_info,
	proc_info_vartypes(CallerProcInfo, VarTypes),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	arg_info__partition_proc_call_args(ProcInfo, VarTypes, ModuleInfo,
		ArgVars, _InVars, OutVars, _UnusedVars),
	( Builtin = inline_builtin ->
		NondetLiveness = NondetLiveness0,
		GoalInfo = GoalInfo0,
		StackAlloc = StackAlloc0
	;
		build_live_sets_in_call(OutVars, GoalInfo0, StackAlloc0,
			Liveness, NondetLiveness0, ResumeVars0, AllocData,
			GoalInfo, StackAlloc, NondetLiveness)
	).

build_live_sets_in_goal_2(Goal, GoalInfo, StackAlloc,
		Liveness, NondetLiveness, _ResumeVars0, _AllocData,
		Goal, GoalInfo, StackAlloc, Liveness, NondetLiveness) :-
	Goal = unify(_, _, _, Unification, _),
	( Unification = complicated_unify(_, _, _) ->
		error("build_live_sets_in_goal_2: complicated_unify")
	;
		true
	).

build_live_sets_in_goal_2(Goal, GoalInfo0, StackAlloc0,
		Liveness, NondetLiveness0, ResumeVars0, AllocData,
		Goal, GoalInfo, StackAlloc, Liveness, NondetLiveness) :-
	Goal = foreign_proc(Attributes, PredId, ProcId, ArgVars, _, _, _),
	ModuleInfo = AllocData ^ module_info,
	CallerProcInfo = AllocData ^ proc_info,
	proc_info_vartypes(CallerProcInfo, VarTypes),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	arg_info__partition_proc_call_args(ProcInfo, VarTypes, ModuleInfo,
		ArgVars, _InVars, OutVars, _UnusedVars),
	goal_info_get_code_model(GoalInfo0, CodeModel),
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
		GoalInfo = GoalInfo0,
		StackAlloc = StackAlloc0
	;
		% The variables which need to be saved onto the stack
		% before the call are all the variables that are live
		% after the call (except for the output arguments produced
		% by the call), plus all the variables that may be needed
		% at an enclosing resumption point.

		build_live_sets_in_call(OutVars, GoalInfo0, StackAlloc0,
			Liveness, NondetLiveness0, ResumeVars0, AllocData,
			GoalInfo, StackAlloc, NondetLiveness)
	).

build_live_sets_in_goal_2(shorthand(_), _,_,_,_,_,_,_,_,_,_,_) :-
	% these should have been expanded out by now
	error("build_live_sets_in_goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

	% The variables which need to be saved onto the stack,
	% directly or indirectly, before a call or may_call_mercury
	% pragma_c_code are all the variables that are live after the goal
	% except for the output arguments produced by the goal, plus all the
	% variables that may be needed at an enclosing resumption point.

:- pred build_live_sets_in_call(set(prog_var)::in, hlds_goal_info::in,
	T::in, set(prog_var)::in, set(prog_var)::in,
	set(prog_var)::in, alloc_data::in,
	hlds_goal_info::out, T::out, set(prog_var)::out)
	is det <= stack_alloc_info(T).

build_live_sets_in_call(OutVars, GoalInfo0, StackAlloc0,
		Liveness, NondetLiveness0, ResumeVars0, AllocData,
		GoalInfo, StackAlloc, NondetLiveness) :-

	set__difference(Liveness, OutVars, ForwardVars0),

	% Might need to add more live variables with typeinfo liveness
	% calculation.

	maybe_add_typeinfo_liveness(AllocData ^ proc_info,
		AllocData ^ typeinfo_liveness, OutVars,
		ForwardVars0, ForwardVars),

	goal_info_get_determinism(GoalInfo0, Detism),
	(
		Detism = erroneous,
		AllocData ^ opt_no_return_calls = yes
	->
		NeedAcrossCall = need_across_call(set__init, set__init,
			set__init)
	;
		NeedAcrossCall = need_across_call(ForwardVars, ResumeVars0,
			NondetLiveness0)
	),

	record_call_site(NeedAcrossCall, GoalInfo0, GoalInfo,
		StackAlloc0, StackAlloc),

	% If this is a nondet call, then all the stack slots we need
	% must be protected against reuse in following code.

	goal_info_get_code_model(GoalInfo, CodeModel),
	( CodeModel = model_non ->
		set__union(NondetLiveness0, ForwardVars, NondetLiveness)
	;
		NondetLiveness = NondetLiveness0
	).

%-----------------------------------------------------------------------------%

:- pred build_live_sets_in_conj(list(hlds_goal)::in, T::in,
	set(prog_var)::in, set(prog_var)::in,
	set(prog_var)::in, alloc_data::in,
	list(hlds_goal)::out, T::out, set(prog_var)::out, set(prog_var)::out)
	is det <= stack_alloc_info(T).

build_live_sets_in_conj([], StackAlloc, Liveness, NondetLiveness, _, _,
		[], StackAlloc, Liveness, NondetLiveness).
build_live_sets_in_conj([Goal0 | Goals0], StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		[Goal | Goals], StackAlloc, Liveness, NondetLiveness) :-
	(
		Goal0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
		instmap_delta_is_unreachable(InstMapDelta)
	->
		build_live_sets_in_goal(Goal0, StackAlloc0,
			Liveness0, NondetLiveness0, ResumeVars0, AllocData,
			Goal, StackAlloc, Liveness, NondetLiveness),
		Goals = Goals0 % XXX
	;
		build_live_sets_in_goal(Goal0, StackAlloc0,
			Liveness0, NondetLiveness0, ResumeVars0, AllocData,
			Goal, StackAlloc1, Liveness1, NondetLiveness1),
		build_live_sets_in_conj(Goals0, StackAlloc1,
			Liveness1, NondetLiveness1, ResumeVars0, AllocData,
			Goals, StackAlloc, Liveness, NondetLiveness)
	).

%-----------------------------------------------------------------------------%

:- pred build_live_sets_in_par_conj(list(hlds_goal)::in, T::in,
	set(prog_var)::in, set(prog_var)::in,
	set(prog_var)::in, alloc_data::in,
	list(hlds_goal)::out, T::out, set(prog_var)::out, set(prog_var)::out)
	is det <= stack_alloc_info(T).

build_live_sets_in_par_conj([], StackAlloc,
		Liveness, NondetLiveness, _, _,
		[], StackAlloc, Liveness, NondetLiveness).
build_live_sets_in_par_conj([Goal0 | Goals0], StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		[Goal | Goals], StackAlloc, Liveness, NondetLiveness) :-
	build_live_sets_in_goal(Goal0, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		Goal, StackAlloc1, Liveness1, NondetLiveness1),
	build_live_sets_in_par_conj(Goals0, StackAlloc1,
		Liveness1, NondetLiveness1, ResumeVars0, AllocData,
		Goals, StackAlloc, Liveness, NondetLiveness).

%-----------------------------------------------------------------------------%

:- pred build_live_sets_in_disj(list(hlds_goal)::in, T::in,
	set(prog_var)::in, set(prog_var)::in, set(prog_var)::in,
	hlds_goal_info::in, alloc_data::in,
	list(hlds_goal)::out, T::out,
	set(prog_var)::out, set(prog_var)::out)
	is det <= stack_alloc_info(T).

build_live_sets_in_disj([], StackAlloc, Liveness, NondetLiveness, _, _, _,
		[], StackAlloc, Liveness, NondetLiveness).
build_live_sets_in_disj([Goal0 | Goals0], StackAlloc0,
		Liveness0, NondetLiveness0,
		ResumeVars0, DisjGoalInfo, AllocData,
		[Goal | Goals], StackAlloc, Liveness, NondetLiveness) :-
	Goal = _ - GoalInfo,
	build_live_sets_in_goal(Goal0, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		Goal, StackAlloc1, Liveness, NondetLiveness1),
	build_live_sets_in_disj(Goals0, StackAlloc1,
		Liveness0, NondetLiveness0,
		ResumeVars0, DisjGoalInfo, AllocData,
		Goals, StackAlloc, _Liveness2, NondetLiveness2),
	goal_info_get_code_model(DisjGoalInfo, DisjCodeModel),
	( DisjCodeModel = model_non ->
			% NondetLiveness should be a set of prog_var sets.
			% Instead of taking the union of the NondetLive sets
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

:- pred build_live_sets_in_cases(list(case)::in, T::in,
	set(prog_var)::in, set(prog_var)::in,
	set(prog_var)::in, alloc_data::in,
	list(case)::out, T::out, set(prog_var)::out, set(prog_var)::out)
	is det <= stack_alloc_info(T).

build_live_sets_in_cases([], StackAlloc, Liveness, NondetLiveness, _, _,
		[], StackAlloc, Liveness, NondetLiveness).
build_live_sets_in_cases([case(Cons, Goal0) | Cases0], StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		[case(Cons, Goal) | Cases], StackAlloc,
		Liveness, NondetLiveness) :-
	build_live_sets_in_goal(Goal0, StackAlloc0,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		Goal, StackAlloc1, Liveness, NondetLiveness1),
	build_live_sets_in_cases(Cases0, StackAlloc1,
		Liveness0, NondetLiveness0, ResumeVars0, AllocData,
		Cases, StackAlloc, _Liveness2, NondetLiveness2),
	set__union(NondetLiveness1, NondetLiveness2, NondetLiveness).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% If doing typeinfo liveness calculation, any typeinfos for
	% output variables or live variables are also live.
	% This is because if you want to examine the live data, you need to
	% know what shape the polymorphic args of the variables
	% are, so you need the typeinfos to be present on the stack.

	% The live variables obviously need their typeinfos
	% live, but the output variables also need their typeinfos
	% saved (otherwise we would throw out typeinfos and might
	% need one at a continuation point just after a call).

	% maybe_add_typeinfo_liveness takes a set of vars (output vars) and
	% a set of live vars and if we are doing typeinfo liveness, adds the
	% appropriate typeinfo variables to the set of variables. If not,
	% it returns the live vars unchanged.

	% Make sure you get the output vars first, and the live vars second,
	% since this makes a significant difference to the output set of vars.

:- pred maybe_add_typeinfo_liveness(proc_info::in, bool::in,
	set(prog_var)::in, set(prog_var)::in, set(prog_var)::out) is det.

maybe_add_typeinfo_liveness(ProcInfo, TypeInfoLiveness, OutVars,
		LiveVars1, LiveVars) :-
	( TypeInfoLiveness = yes ->
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

:- pred record_call_site(need_across_call::in, hlds_goal_info::in,
	hlds_goal_info::out, T::in, T::out) is det <= stack_alloc_info(T).

record_call_site(NeedAcrossCall, GoalInfo0, GoalInfo,
		StackAlloc0, StackAlloc) :-
	goal_info_set_need_across_call(GoalInfo0, NeedAcrossCall, GoalInfo),
	at_call_site(NeedAcrossCall, GoalInfo, StackAlloc0, StackAlloc).

:- pred record_resume_site(need_in_resume::in, hlds_goal_info::in,
	hlds_goal_info::out, T::in, T::out) is det <= stack_alloc_info(T).

record_resume_site(NeedInResume, GoalInfo0, GoalInfo,
		StackAlloc0, StackAlloc) :-
	goal_info_set_need_in_resume(GoalInfo0, NeedInResume, GoalInfo),
	at_resume_site(NeedInResume, GoalInfo, StackAlloc0, StackAlloc).

:- pred record_par_conj(need_in_par_conj::in, hlds_goal_info::in,
	hlds_goal_info::out, T::in, T::out) is det <= stack_alloc_info(T).

record_par_conj(NeedInParConj, GoalInfo0, GoalInfo,
		StackAlloc0, StackAlloc) :-
	goal_info_set_need_in_par_conj(GoalInfo0, NeedInParConj, GoalInfo),
	at_par_conj(NeedInParConj, GoalInfo, StackAlloc0, StackAlloc).

%-----------------------------------------------------------------------------%
