%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: liveness.m
%
% Main authors: conway, zs, trd.
%
% This module traverses the goal for each procedure, and adds
% liveness annotations to the goal_info for each sub-goal.
% These annotations are the pre-birth set, the post-birth set,
% the pre-death set, the post-death set, and the resume_point field.
%
% Because it recomputes each of these annotations from scratch,
% it should be safe to call this module multiple times
% (although this has not been tested yet).
%
% Note - the concept of `liveness' here is different to that used in
% mode analysis. Mode analysis is concerned with the liveness of what
% is *pointed* to by a variable, for the purpose of avoiding and/or keeping
% track of aliasing and for structure re-use optimization, whereas here
% we are concerned with the liveness of the variable itself, for the
% purposes of optimizing stack slot and register usage.
% Variables have a lifetime: each variable is born, gets used, and then dies.
% To minimize stack slot and register usage, the birth should be
% as late as possible (but before the first possible use), and the
% death should be as early as possible (but after the last possible use).
%
% We compute liveness related information in three distinct passes.
%
% The first pass, detect_liveness_in_goal, finds the first value-giving
% occurrence of each variable on each computation path. Goals containing
% the first such occurrence of a variable include that variable in their
% pre-birth set. In branched structures, branches whose endpoint is not
% reachable include a post-birth set listing the variables that should
% have been born in that branch but haven't. Variables that shouldn't have
% been born but have been (in computation paths that cannot succeed)
% are included in the post-death set of the goal concerned.
%
% The second pass, detect_deadness_in_goal, finds the last occurrence
% of each variable on each computation path. Goals containing the last
% occurrence of a variable include that variable in their post-death
% set. In branched structures, branches in which a variable is not
% used at all include a pre-death set listing the variables that
% have died in parallel branches.
%
% The third pass, detect_resume_points_in_goal, finds goals that
% establish resume points and attaches to them a resume_point
% annotation listing the variables that may be referenced by the
% code at that resume point as well as the nature of the required
% entry labels.
%
% Typeinfo liveness calculation notes:
%
% When using accurate gc or execution tracing, liveness is computed
% slightly differently.  The runtime system needs access to the
% typeinfo variables of any variable that is live at a continuation.
% 
% Hence, the invariant needed for alternate liveness calculation:
% 	a variable holding a typeinfo must be live at any continuation
% 	where any variable whose type is described (in whole or in part)
% 	by that typeinfo is live.
%
% Typeinfos are introduced as either one of the head variables, or a new
% variable created by a goal in the procedure. If introduced as a head
% variable, initial_liveness will add it to the initial live set of
% variables -- no variable could be introduced earlier than the start of
% the goal. If introduced by a goal in the procedure, that goal must
% occur before any call that requires the typeinfo, so the variable will
% be born before the continuation after the call. So the typeinfo
% variables will always be born before any continuation where they are
% needed.
% 
% A typeinfo variable becomes dead after both the following conditions
% are true: 
%
% 	(1) The typeinfo variable is not used again (when it is no
% 	    longer part of the nonlocals)
%	(2) No other nonlocal variable's type is described by that typeinfo
%	    variable.
% 
% (1) happens without any changes to the liveness computation (it is
%     the normal condition for variables becoming dead). This more
%     conservative than what is required for the invariant, but is
%     required for code generation, so we should keep it ;-)
% (2) is implemented by adding the typeinfo variables for the types of the
%     nonlocals to the nonlocals for the purposes of computing liveness.
%
% So typeinfo variables will always be born before they are needed, and
% die only when no other variable needing them will be live, so the
% invariant holds.
%
% Quantification notes:
%
% If a variable is not live on entry to a goal, but the goal gives it a value,
% the code of this module assumes that
%
% (a) any parallel goals also give it a value, or
% (b) the variable is local to this goal and hence does not occur in parallel
%     goals.
%
% If a variable occurs in the nonlocal set of the goal, the code of this
% assumes that (b) is not true, and will therefore require (a) to be true.
% If some of the parallel goals cannot succeed, the first pass will include
% the variable in their post-birth sets.
%
% If a variable occurs in the nonlocal set of the goal, but is actually
% local to the goal, then any occurrence of that variable in the postbirth
% sets of parallel goals will lead to an inconsistency, because the variable
% will not die on those parallel paths, but will die on the path that
% actually gives a value to the variable.
%
% The nonlocal set of a goal is in general allowed to overapproximate the
% true set of nonlocal variables of the goal. Since this module requires
% *exact* information about nonlocals, it must recompute the nonlocal sets
% before starting.

%-----------------------------------------------------------------------------%

:- module liveness.

:- interface.

:- import_module hlds_module, hlds_pred, prog_data.
:- import_module set.

	% Add liveness annotations to the goal of the procedure.
	% This consists of the {pre,post}{birth,death} sets and
	% resume point information.

:- pred detect_liveness_proc(proc_info, pred_id, module_info, proc_info).
:- mode detect_liveness_proc(in, in, in, out) is det.

	% Return the set of variables live at the start of the procedure.

:- pred initial_liveness(proc_info, pred_id, module_info, set(prog_var)).
:- mode initial_liveness(in, in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, hlds_data, llds, quantification, (inst), instmap.
:- import_module hlds_out, mode_util, code_util, quantification, options.
:- import_module trace, globals, polymorphism, passes_aux, prog_util.
:- import_module term, varset.

:- import_module bool, map, std_util, list, assoc_list, require.
:- import_module string.

detect_liveness_proc(ProcInfo0, PredId, ModuleInfo, ProcInfo) :-
	module_info_globals(ModuleInfo, Globals),
	body_should_use_typeinfo_liveness(Globals, TypeInfoLiveness0),
	(
		pred_info_module(PredInfo, PredModule),
		( mercury_public_builtin_module(PredModule)
		; mercury_private_builtin_module(PredModule)
		)
	->
		TypeInfoLiveness1 = no
	;
		TypeInfoLiveness1 = TypeInfoLiveness0
	),
	requantify_proc(TypeInfoLiveness1, ProcInfo0, ProcInfo1),

	proc_info_goal(ProcInfo1, Goal0),
	proc_info_varset(ProcInfo1, Varset),
	proc_info_vartypes(ProcInfo1, VarTypes),
	proc_info_typeinfo_varmap(ProcInfo1, TVarMap),

	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_module(PredInfo, PredModule),
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, PredArity),
	(
		polymorphism__no_type_info_builtin(PredModule,
			PredName, PredArity)
	->
		TypeInfoLiveness = no
	;
		TypeInfoLiveness = TypeInfoLiveness0
	),
	live_info_init(ModuleInfo, TypeInfoLiveness, VarTypes, TVarMap, Varset,
		LiveInfo),

	initial_liveness(ProcInfo1, PredId, ModuleInfo, Liveness0),
	detect_liveness_in_goal(Goal0, Liveness0, LiveInfo,
		_, Goal1),

	initial_deadness(ProcInfo1, LiveInfo, ModuleInfo, Deadness0),
	detect_deadness_in_goal(Goal1, Deadness0, LiveInfo, _, Goal2),

	globals__get_trace_level(Globals, TraceLevel),
	( TraceLevel \= none ->
		trace__fail_vars(ModuleInfo, ProcInfo0, ResumeVars0)
	;
		set__init(ResumeVars0)
	),
	detect_resume_points_in_goal(Goal2, Liveness0, LiveInfo,
		ResumeVars0, Goal, _),

	proc_info_set_goal(ProcInfo1, Goal, ProcInfo2),
	proc_info_set_liveness_info(ProcInfo2, Liveness0, ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_goal(hlds_goal, set(prog_var), live_info,
		set(prog_var), hlds_goal).
:- mode detect_liveness_in_goal(in, in, in, out, out) is det.

detect_liveness_in_goal(Goal0 - GoalInfo0, Liveness0, LiveInfo,
		Liveness, Goal - GoalInfo) :-

		% work out which variables get born in this goal
	liveness__get_nonlocals_and_typeinfos(LiveInfo, GoalInfo0, NonLocals),
	set__difference(NonLocals, Liveness0, NewVarsSet),
	set__to_sorted_list(NewVarsSet, NewVarsList),
	goal_info_get_instmap_delta(GoalInfo0, InstMapDelta),
	set__init(Empty),
	( instmap_delta_is_unreachable(InstMapDelta) ->
		Births = Empty
	;
		set__init(Births0),
		find_value_giving_occurrences(NewVarsList, LiveInfo,
			InstMapDelta, Births0, Births)
	),
	set__union(Liveness0, Births, Liveness),
	(
		goal_is_atomic(Goal0)
	->
		PreDeaths = Empty,
		PreBirths = Births,
		PostDeaths = Empty,
		PostBirths = Empty,
		Goal = Goal0
	;
		PreDeaths = Empty,
		PreBirths = Empty,
		detect_liveness_in_goal_2(Goal0, Liveness0, NonLocals,
			LiveInfo, ActualLiveness, Goal),
		set__intersect(NonLocals, ActualLiveness, NonLocalLiveness),
		set__union(NonLocalLiveness, Liveness0, FinalLiveness),
		set__difference(FinalLiveness, Liveness, PostDeaths),
		set__difference(Liveness, FinalLiveness, PostBirths)
	),
		% We initialize all the fields in order to obliterate any
		% annotations left by a previous invocation of this module.
	goal_info_set_pre_deaths(GoalInfo0, PreDeaths, GoalInfo1),
	goal_info_set_pre_births(GoalInfo1, PreBirths, GoalInfo2),
	goal_info_set_post_deaths(GoalInfo2, PostDeaths, GoalInfo3),
	goal_info_set_post_births(GoalInfo3, PostBirths, GoalInfo4),
	goal_info_set_resume_point(GoalInfo4, no_resume_point, GoalInfo).

%-----------------------------------------------------------------------------%

	% Here we process each of the different sorts of goals.

:- pred detect_liveness_in_goal_2(hlds_goal_expr, set(prog_var), set(prog_var),
	live_info, set(prog_var), hlds_goal_expr).
:- mode detect_liveness_in_goal_2(in, in, in, in, out, out) is det.

detect_liveness_in_goal_2(conj(Goals0), Liveness0, _, LiveInfo,
		Liveness, conj(Goals)) :-
	detect_liveness_in_conj(Goals0, Liveness0, LiveInfo, Liveness, Goals).

detect_liveness_in_goal_2(par_conj(Goals0, SM), Liveness0, NonLocals, LiveInfo,
		Liveness, par_conj(Goals, SM)) :-
	set__init(Union0),
	detect_liveness_in_par_conj(Goals0, Liveness0, NonLocals, LiveInfo,
		Union0, Union, Goals),
	set__union(Liveness0, Union, Liveness).

detect_liveness_in_goal_2(disj(Goals0, SM), Liveness0, NonLocals, LiveInfo,
		Liveness, disj(Goals, SM)) :-
	set__init(Union0),
	detect_liveness_in_disj(Goals0, Liveness0, NonLocals, LiveInfo,
		Union0, Union, Goals),
	set__union(Liveness0, Union, Liveness).

detect_liveness_in_goal_2(switch(Var, Det, Cases0, SM), Liveness0, NonLocals,
		LiveInfo, Liveness, switch(Var, Det, Cases, SM)) :-
	detect_liveness_in_cases(Cases0, Liveness0, NonLocals, LiveInfo,
		Liveness0, Liveness, Cases).

detect_liveness_in_goal_2(not(Goal0), Liveness0, _, LiveInfo,
		Liveness, not(Goal)) :-
	detect_liveness_in_goal(Goal0, Liveness0, LiveInfo, Liveness, Goal).

detect_liveness_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0, SM),
		Liveness0, NonLocals, LiveInfo, Liveness,
		if_then_else(Vars, Cond, Then, Else, SM)) :-
	detect_liveness_in_goal(Cond0, Liveness0, LiveInfo, LivenessCond, Cond),

	%
	% If the condition cannot succeed, any variables which become live
	% in the else part should be put in the post-birth set of the then part
	% by add_liveness_after_goal, and the other sets should be empty.
	%
	Cond = _ - CondInfo,
	goal_info_get_instmap_delta(CondInfo, CondDelta),
	( instmap_delta_is_unreachable(CondDelta) ->
		LivenessThen = LivenessCond,
		Then1 = Then0
	;
		detect_liveness_in_goal(Then0, LivenessCond, LiveInfo,
			LivenessThen, Then1)
	),

	detect_liveness_in_goal(Else0, Liveness0, LiveInfo, LivenessElse,
		Else1),

	set__union(LivenessThen, LivenessElse, Liveness),
	set__intersect(Liveness, NonLocals, NonLocalLiveness),

	set__difference(NonLocalLiveness, LivenessThen, ResidueThen),
	set__difference(NonLocalLiveness, LivenessElse, ResidueElse),

	add_liveness_after_goal(Then1, ResidueThen, Then),
	add_liveness_after_goal(Else1, ResidueElse, Else).

detect_liveness_in_goal_2(some(Vars, CanRemove, Goal0), Liveness0, _, LiveInfo,
		Liveness, some(Vars, CanRemove, Goal)) :-
	detect_liveness_in_goal(Goal0, Liveness0, LiveInfo, Liveness, Goal).

detect_liveness_in_goal_2(generic_call(_,_,_,_), _, _, _, _, _) :-
	error("higher-order-call in detect_liveness_in_goal_2").

detect_liveness_in_goal_2(call(_,_,_,_,_,_), _, _, _, _, _) :-
	error("call in detect_liveness_in_goal_2").

detect_liveness_in_goal_2(unify(_,_,_,_,_), _, _, _, _, _) :-
	error("unify in detect_liveness_in_goal_2").

detect_liveness_in_goal_2(pragma_c_code(_,_,_,_,_,_,_), _, _, _, _, _) :-
	error("pragma_c_code in detect_liveness_in_goal_2").

detect_liveness_in_goal_2(bi_implication(_, _), _, _, _, _, _) :-
	error("bi_implication in detect_liveness_in_goal_2").

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_conj(list(hlds_goal), set(prog_var), live_info,
	set(prog_var), list(hlds_goal)).
:- mode detect_liveness_in_conj(in, in, in, out, out) is det.

detect_liveness_in_conj([], Liveness, _LiveInfo, Liveness, []).
detect_liveness_in_conj([Goal0 | Goals0], Liveness0, LiveInfo, Liveness,
		[Goal | Goals]) :-
	detect_liveness_in_goal(Goal0, Liveness0, LiveInfo, Liveness1, Goal),
	(
		Goal0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, InstmapDelta),
		instmap_delta_is_unreachable(InstmapDelta)
	->
		Goals = Goals0,
		Liveness = Liveness1
	;
		detect_liveness_in_conj(Goals0, Liveness1, LiveInfo,
			Liveness, Goals)
	).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_disj(list(hlds_goal), set(prog_var), set(prog_var),
		live_info, set(prog_var), set(prog_var), list(hlds_goal)).
:- mode detect_liveness_in_disj(in, in, in, in, in, out, out) is det.

detect_liveness_in_disj([], _Liveness, _NonLocals, _LiveInfo,
		Union, Union, []).
detect_liveness_in_disj([Goal0 | Goals0], Liveness, NonLocals, LiveInfo,
		Union0, Union, [Goal | Goals]) :-
	detect_liveness_in_goal(Goal0, Liveness, LiveInfo, Liveness1, Goal1),
	set__union(Union0, Liveness1, Union1),
	detect_liveness_in_disj(Goals0, Liveness, NonLocals, LiveInfo,
		Union1, Union, Goals),
	set__intersect(Union, NonLocals, NonLocalUnion),
	set__difference(NonLocalUnion, Liveness1, Residue),
	add_liveness_after_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_cases(list(case), set(prog_var), set(prog_var),
		live_info, set(prog_var), set(prog_var), list(case)).
:- mode detect_liveness_in_cases(in, in, in, in, in, out, out) is det.

detect_liveness_in_cases([], _Liveness, _NonLocals, _LiveInfo,
		Union, Union, []).
detect_liveness_in_cases([case(Cons, Goal0) | Goals0], Liveness, NonLocals,
		LiveInfo, Union0, Union, [case(Cons, Goal) | Goals]) :-
	detect_liveness_in_goal(Goal0, Liveness, LiveInfo, Liveness1, Goal1),
	set__union(Union0, Liveness1, Union1),
	detect_liveness_in_cases(Goals0, Liveness, NonLocals, LiveInfo,
		Union1, Union, Goals),
	set__intersect(Union, NonLocals, NonLocalUnion),
	set__difference(NonLocalUnion, Liveness1, Residue),
	add_liveness_after_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_par_conj(list(hlds_goal), set(prog_var),
		set(prog_var), live_info, set(prog_var), set(prog_var),
		list(hlds_goal)).
:- mode detect_liveness_in_par_conj(in, in, in, in, in, out, out) is det.

detect_liveness_in_par_conj([], _Liveness, _NonLocals, _LiveInfo,
		Union, Union, []).
detect_liveness_in_par_conj([Goal0 | Goals0], Liveness0, NonLocals, LiveInfo,
		Union0, Union, [Goal | Goals]) :-
	detect_liveness_in_goal(Goal0, Liveness0, LiveInfo, Liveness1, Goal1),
	set__union(Union0, Liveness1, Union1),
	detect_liveness_in_par_conj(Goals0, Liveness0, NonLocals, LiveInfo,
		Union1, Union, Goals),
	set__intersect(Union, NonLocals, NonLocalUnion),
	set__difference(NonLocalUnion, Liveness1, Residue),
	add_liveness_after_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_goal(hlds_goal, set(prog_var), live_info,
	set(prog_var), hlds_goal).
:- mode detect_deadness_in_goal(in, in, in, out, out) is det.

detect_deadness_in_goal(Goal0 - GoalInfo0, Deadness0, LiveInfo, Deadness,
		Goal - GoalInfo) :-
	goal_info_get_pre_deaths(GoalInfo0, PreDeaths0),
	goal_info_get_pre_births(GoalInfo0, PreBirths0),
	goal_info_get_post_deaths(GoalInfo0, PostDeaths0),
	goal_info_get_post_births(GoalInfo0, PostBirths0),

	set__difference(Deadness0, PostBirths0, Deadness1),
	set__union(Deadness1, PostDeaths0, Deadness2),

	liveness__get_nonlocals_and_typeinfos(LiveInfo, GoalInfo0, NonLocals),
	set__init(Empty),
	(
		goal_is_atomic(Goal0)
	->
		% 
		% The code below is slightly dodgy: the new postdeaths really
		% ought to be computed as the difference between the liveness
		% immediately before goal and the deadness immediately after
		% goal.  But we don't have liveness available here, and
		% computing it would be complicated and perhaps costly.  So
		% instead we use the non-locals.  The effect of this is that a
		% variable will die after its last occurence, even if it has
		% never been born.  (This can occur in the case of variables
		% with `free->free' modes, or for goals with determinism
		% erroneous.)  Thus the code generator must be willing to
		% handle that -- it is not considered an error for a variable
		% that is not yet live to die.  [If we ever wanted to
		% change this, the easiest thing to do would be to put
		% some extra code in the third pass (detect_resume_points)
		% to delete any such untimely deaths.]
		% 
		set__difference(NonLocals, Deadness2, NewPostDeaths),
		set__union(Deadness2, NewPostDeaths, Deadness3),
		Goal = Goal0
	;
		NewPostDeaths = Empty,
		detect_deadness_in_goal_2(Goal0, GoalInfo0, Deadness2,
			LiveInfo, Deadness3, Goal)
	),
	set__union(PostDeaths0, NewPostDeaths, PostDeaths),
	goal_info_set_post_deaths(GoalInfo0, PostDeaths, GoalInfo),

	set__difference(Deadness3, PreBirths0, Deadness4),
	set__union(Deadness4, PreDeaths0, Deadness).

	% Here we process each of the different sorts of goals.

:- pred detect_deadness_in_goal_2(hlds_goal_expr, hlds_goal_info,
	set(prog_var), live_info, set(prog_var), hlds_goal_expr).
:- mode detect_deadness_in_goal_2(in, in, in, in, out, out) is det.

detect_deadness_in_goal_2(conj(Goals0), _, Deadness0, LiveInfo,
		Deadness, conj(Goals)) :-
	detect_deadness_in_conj(Goals0, Deadness0, LiveInfo,
		Goals, Deadness).

detect_deadness_in_goal_2(par_conj(Goals0, SM), GoalInfo, Deadness0, LiveInfo,
		Deadness, par_conj(Goals, SM)) :-
	set__init(Union0),
	goal_info_get_code_gen_nonlocals(GoalInfo, NonLocals),
	detect_deadness_in_par_conj(Goals0, Deadness0, NonLocals,
		LiveInfo, Union0, Union, Goals),
	set__union(Union, Deadness0, Deadness).

detect_deadness_in_goal_2(disj(Goals0, SM), GoalInfo, Deadness0,
		LiveInfo, Deadness, disj(Goals, SM)) :-
	set__init(Union0),
	liveness__get_nonlocals_and_typeinfos(LiveInfo, GoalInfo, NonLocals),
	detect_deadness_in_disj(Goals0, Deadness0, NonLocals,
		LiveInfo, Union0, Deadness, Goals).

detect_deadness_in_goal_2(switch(Var, Det, Cases0, SM), GoalInfo, Deadness0,
		LiveInfo, Deadness, switch(Var, Det, Cases, SM)) :-
	set__init(Union0),
	liveness__get_nonlocals_and_typeinfos(LiveInfo, GoalInfo, NonLocals),
	detect_deadness_in_cases(Var, Cases0, Deadness0, NonLocals,
		LiveInfo, Union0, Deadness, Cases).

detect_deadness_in_goal_2(not(Goal0), _, Deadness0, LiveInfo,
		Deadness, not(Goal)) :-
	detect_deadness_in_goal(Goal0, Deadness0, LiveInfo, Deadness, Goal).

detect_deadness_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0, SM),
		GoalInfo, Deadness0, LiveInfo, Deadness,
		if_then_else(Vars, Cond, Then, Else, SM)) :-
	detect_deadness_in_goal(Else0, Deadness0, LiveInfo,
		DeadnessElse, Else1),
	detect_deadness_in_goal(Then0, Deadness0, LiveInfo,
		DeadnessThen, Then),
	detect_deadness_in_goal(Cond0, DeadnessThen, LiveInfo,
		DeadnessCond, Cond1),

	liveness__get_nonlocals_and_typeinfos(LiveInfo, GoalInfo, NonLocals),
	set__union(DeadnessCond, DeadnessElse, Deadness),
	set__intersect(Deadness, NonLocals, NonLocalDeadness),

	set__difference(NonLocalDeadness, DeadnessCond, ResidueCond),
	set__difference(NonLocalDeadness, DeadnessElse, ResidueElse),

	add_deadness_before_goal(Cond1, ResidueCond, Cond),
	add_deadness_before_goal(Else1, ResidueElse, Else).

detect_deadness_in_goal_2(some(Vars, CanRemove, Goal0), _, Deadness0, LiveInfo,
		Deadness, some(Vars, CanRemove, Goal)) :-
	detect_deadness_in_goal(Goal0, Deadness0, LiveInfo, Deadness, Goal).

detect_deadness_in_goal_2(generic_call(_,_,_,_), _, _, _, _, _) :-
	error("higher-order-call in detect_deadness_in_goal_2").

detect_deadness_in_goal_2(call(_,_,_,_,_,_), _, _, _, _, _) :-
	error("call in detect_deadness_in_goal_2").

detect_deadness_in_goal_2(unify(_,_,_,_,_), _, _, _, _, _) :-
	error("unify in detect_deadness_in_goal_2").

detect_deadness_in_goal_2(pragma_c_code(_,_,_,_,_,_,_), _, _, _, _, _) :-
	error("pragma_c_code in detect_deadness_in_goal_2").

detect_deadness_in_goal_2(bi_implication(_, _), _, _, _, _, _) :-
	error("bi_implication in detect_deadness_in_goal_2").

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_conj(list(hlds_goal), set(prog_var), live_info,
	list(hlds_goal), set(prog_var)).
:- mode detect_deadness_in_conj(in, in, in, out, out) is det.

detect_deadness_in_conj([], Deadness, _LiveInfo, [], Deadness).
detect_deadness_in_conj([Goal0 | Goals0], Deadness0, LiveInfo,
		[Goal | Goals], Deadness) :-
	(
		Goal0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, InstmapDelta),
		instmap_delta_is_unreachable(InstmapDelta)
	->
		Goals = Goals0,
		detect_deadness_in_goal(Goal0, Deadness0, LiveInfo,
			Deadness, Goal)
	;
		detect_deadness_in_conj(Goals0, Deadness0, LiveInfo,
			Goals, Deadness1),
		detect_deadness_in_goal(Goal0, Deadness1, LiveInfo,
			Deadness, Goal)
	).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_disj(list(hlds_goal), set(prog_var), set(prog_var),
	live_info, set(prog_var), set(prog_var), list(hlds_goal)).
:- mode detect_deadness_in_disj(in, in, in, in, in, out, out) is det.

detect_deadness_in_disj([], _Deadness, _NonLocals, _LiveInfo,
		Union, Union, []).
detect_deadness_in_disj([Goal0 | Goals0], Deadness, NonLocals, LiveInfo,
		Union0, Union, [Goal | Goals]) :-
	detect_deadness_in_goal(Goal0, Deadness, LiveInfo, Deadness1, Goal1),
	set__union(Union0, Deadness1, Union1),
	detect_deadness_in_disj(Goals0, Deadness, NonLocals, LiveInfo,
		Union1, Union, Goals),
	set__intersect(Union, NonLocals, NonLocalUnion),
	set__difference(NonLocalUnion, Deadness1, Residue),
	add_deadness_before_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_cases(prog_var, list(case), set(prog_var),
	set(prog_var), live_info, set(prog_var), set(prog_var), list(case)).
:- mode detect_deadness_in_cases(in, in, in, in, in, in, out, out) is det.

detect_deadness_in_cases(_Var, [], _Deadness, _NonLocals, _LiveInfo,
		Union, Union, []).
detect_deadness_in_cases(SwitchVar, [case(Cons, Goal0) | Goals0], Deadness0,
		NonLocals, LiveInfo, Union0, Union,
		[case(Cons, Goal) | Goals]) :-
	detect_deadness_in_goal(Goal0, Deadness0, LiveInfo, Deadness1, Goal1),
	set__union(Union0, Deadness1, Union1),
	detect_deadness_in_cases(SwitchVar, Goals0, Deadness0, NonLocals,
		LiveInfo, Union1, Union2, Goals),
		% If the switch variable does not become dead in a case
		% it must be put in the pre-death set of that case.
	set__insert(Union2, SwitchVar, Union),
	set__intersect(Union, NonLocals, NonLocalUnion),
	set__difference(NonLocalUnion, Deadness1, Residue),
	add_deadness_before_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_par_conj(list(hlds_goal), set(prog_var),
		set(prog_var), live_info, set(prog_var), set(prog_var),
		list(hlds_goal)).
:- mode detect_deadness_in_par_conj(in, in, in, in, in, out, out) is det.

detect_deadness_in_par_conj([], _Deadness, _NonLocals, _LiveInfo,
		Union, Union, []).
detect_deadness_in_par_conj([Goal0 | Goals0], Deadness, NonLocals, LiveInfo,
		Union0, Union, [Goal | Goals]) :-
	detect_deadness_in_goal(Goal0, Deadness, LiveInfo, Deadness1, Goal1),
	set__union(Union0, Deadness1, Union1),
	detect_deadness_in_par_conj(Goals0, Deadness, NonLocals, LiveInfo,
		Union1, Union, Goals),
	set__intersect(Union, NonLocals, NonLocalUnion),
	set__difference(NonLocalUnion, Deadness1, Residue),
	add_deadness_before_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_resume_points_in_goal(hlds_goal, set(prog_var), live_info,
		set(prog_var), hlds_goal, set(prog_var)).
:- mode detect_resume_points_in_goal(in, in, in, in, out, out) is det.

detect_resume_points_in_goal(Goal0 - GoalInfo0, Liveness0, LiveInfo,
		ResumeVars0, Goal - GoalInfo0, Liveness) :-
	goal_info_get_pre_deaths(GoalInfo0, PreDeaths0),
	goal_info_get_pre_births(GoalInfo0, PreBirths0),
	goal_info_get_post_deaths(GoalInfo0, PostDeaths0),
	goal_info_get_post_births(GoalInfo0, PostBirths0),

	set__difference(Liveness0, PreDeaths0, Liveness1),
	set__union(Liveness1, PreBirths0, Liveness2),

	detect_resume_points_in_goal_2(Goal0, GoalInfo0, Liveness2, LiveInfo,
		ResumeVars0, Goal, Liveness3),

	set__difference(Liveness3, PostDeaths0, Liveness4),
	set__union(Liveness4, PostBirths0, Liveness).

:- pred detect_resume_points_in_goal_2(hlds_goal_expr, hlds_goal_info,
	set(prog_var), live_info, set(prog_var), hlds_goal_expr, set(prog_var)).
:- mode detect_resume_points_in_goal_2(in, in, in, in, in, out, out) is det.

detect_resume_points_in_goal_2(conj(Goals0), _, Liveness0, LiveInfo,
		ResumeVars0, conj(Goals), Liveness) :-
	detect_resume_points_in_conj(Goals0, Liveness0, LiveInfo, ResumeVars0,
		Goals, Liveness).

detect_resume_points_in_goal_2(par_conj(Goals0, SM), _, Liveness0, LiveInfo,
		ResumeVars0, par_conj(Goals, SM), Liveness) :-
	detect_resume_points_in_par_conj(Goals0, Liveness0, LiveInfo,
		ResumeVars0, Goals, Liveness).

detect_resume_points_in_goal_2(disj(Goals0, SM), GoalInfo, Liveness0, LiveInfo,
		ResumeVars0, disj(Goals, SM), Liveness) :-
	goal_info_get_code_model(GoalInfo, CodeModel),
	( CodeModel = model_non ->
		detect_resume_points_in_non_disj(Goals0, Liveness0, LiveInfo,
			ResumeVars0, Goals, Liveness, _)
	;
		detect_resume_points_in_pruned_disj(Goals0, Liveness0, LiveInfo,
			ResumeVars0, Goals, Liveness, _)
	).

detect_resume_points_in_goal_2(switch(Var, CF, Cases0, SM), _, Liveness0,
		LiveInfo, ResumeVars0, switch(Var, CF, Cases, SM), Liveness) :-
	detect_resume_points_in_cases(Cases0, Liveness0, LiveInfo, ResumeVars0,
		Cases, Liveness).

detect_resume_points_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0, SM),
		GoalInfo0, Liveness0, LiveInfo, ResumeVars0,
		if_then_else(Vars, Cond, Then, Else, SM), LivenessThen) :-

	% compute the set of variables that may be needed at the start
	% of the else part and attach this set to the condition
	Else0 = _ElseExpr0 - ElseInfo0,
	goal_info_get_pre_deaths(ElseInfo0, ElsePreDeath0),
	set__difference(Liveness0, ElsePreDeath0, CondResumeVars0),
	liveness__maybe_complete_with_typeinfos(LiveInfo, CondResumeVars0,
		CondResumeVars1),
		% ResumeVars0 should already have been completed.
	set__union(CondResumeVars1, ResumeVars0, CondResumeVars),

	detect_resume_points_in_goal(Cond0, Liveness0, LiveInfo,
		CondResumeVars, Cond1, LivenessCond),
	detect_resume_points_in_goal(Then0, LivenessCond, LiveInfo,
		ResumeVars0, Then, LivenessThen),
	detect_resume_points_in_goal(Else0, Liveness0, LiveInfo,
		ResumeVars0, Else, LivenessElse),

	% Figure out which entry labels we need at the resumption point.
	% By minimizing the number of labels we use, we also minimize
	% the amount of data movement code we emit between such labels.
	(
		code_util__cannot_stack_flush(Cond1),
		goal_info_get_code_model(GoalInfo0, CodeModel),
		CodeModel \= model_non
	->
		CondResumeLocs = orig_only
	;
		code_util__cannot_fail_before_stack_flush(Cond1)
	->
		CondResumeLocs = stack_only
	;
		CondResumeLocs = stack_and_orig
	),

	% Attach the set of variables needed after the condition
	% as the resume point set of the condition.
	CondResume = resume_point(CondResumeVars, CondResumeLocs),
	goal_set_resume_point(Cond1, CondResume, Cond),

	require_equal(LivenessThen, LivenessElse, "if-then-else", LiveInfo).

detect_resume_points_in_goal_2(some(Vars, CanRemove, Goal0), _, Liveness0,
		LiveInfo, ResumeVars0, some(Vars, CanRemove, Goal),
		Liveness) :-
	detect_resume_points_in_goal(Goal0, Liveness0, LiveInfo, ResumeVars0,
					Goal, Liveness).

detect_resume_points_in_goal_2(not(Goal0), _, Liveness0, LiveInfo, ResumeVars0,
		not(Goal), Liveness) :-
	detect_resume_points_in_goal(Goal0, Liveness0, LiveInfo, ResumeVars0,
		_, Liveness),
	liveness__maybe_complete_with_typeinfos(LiveInfo, Liveness,
		CompletedLiveness),
		% ResumeVars0 should already have been completed.
	set__union(CompletedLiveness, ResumeVars0, ResumeVars1),
	detect_resume_points_in_goal(Goal0, Liveness0, LiveInfo, ResumeVars1,
		Goal1, _Liveness),

	% Figure out which entry labels we need at the resumption point.
	% By minimizing the number of labels we use, we also minimize
	% the amount of data movement code we emit between such labels.
	( code_util__cannot_stack_flush(Goal1) ->
		ResumeLocs = orig_only
	; code_util__cannot_fail_before_stack_flush(Goal1) ->
		ResumeLocs = stack_only
	;
		ResumeLocs = stack_and_orig
	),

	% Attach the set of variables alive after the negation
	% as the resume point set of the negated goal.
	Resume = resume_point(ResumeVars1, ResumeLocs),
	goal_set_resume_point(Goal1, Resume, Goal).

detect_resume_points_in_goal_2(generic_call(A,B,C,D), _, Liveness,
		_, _, generic_call(A,B,C,D), Liveness).

detect_resume_points_in_goal_2(call(A,B,C,D,E,F), _, Liveness, _, _,
		call(A,B,C,D,E,F), Liveness).

detect_resume_points_in_goal_2(unify(A,B,C,D,E), _, Liveness, _, _,
		unify(A,B,C,D,E), Liveness).

detect_resume_points_in_goal_2(pragma_c_code(A,B,C,D,E,F,G), _, Liveness,
		_, _,
		pragma_c_code(A,B,C,D,E,F,G), Liveness).

detect_resume_points_in_goal_2(bi_implication(_, _), _, _, _, _, _, _) :-
	% these should have been expanded out by now
	error("detect_resume_points_in_goal_2: unexpected bi_implication").

:- pred detect_resume_points_in_conj(list(hlds_goal), set(prog_var), live_info,
	set(prog_var), list(hlds_goal), set(prog_var)).
:- mode detect_resume_points_in_conj(in, in, in, in, out, out) is det.

detect_resume_points_in_conj([], Liveness, _, _, [], Liveness).
detect_resume_points_in_conj([Goal0 | Goals0], Liveness0, LiveInfo,
		ResumeVars0, [Goal | Goals], Liveness) :-
	detect_resume_points_in_goal(Goal0, Liveness0, LiveInfo, ResumeVars0,
		Goal, Liveness1),
	detect_resume_points_in_conj(Goals0, Liveness1, LiveInfo, ResumeVars0,
		Goals, Liveness).

	% There are only two differences in the handling of pruned disjs
	% versus nondet disjs. First, for nondet disjunctions we always
	% generate code for all disjuncts, whereas for pruned disjunctions
	% we stop generating code after the first cannot_fail disjunct.
	% Second, an empty pruned disjunction is legal, while an empty
	% nondet disjunction isn't.
	%
	% For both kinds of disjunctions, the resume points to be attached to
	% the non-last disjuncts must be completed with the required typeinfos
	% if --typeinfo-liveness is set. ResumeVars0 should already be so
	% completed, so we need only complete the sets added here. We therefore
	% perform this completion when we return the set of variables needed by
	% the last disjunct, and when we add to this set the set of variables
	% needed by a non-last disjunct.

:- pred detect_resume_points_in_non_disj(list(hlds_goal), set(prog_var),
		live_info, set(prog_var), list(hlds_goal),
		set(prog_var), set(prog_var)).
:- mode detect_resume_points_in_non_disj(in, in, in, in, out, out, out) is det.

detect_resume_points_in_non_disj([], _, _, _, _, _, _) :-
	error("empty nondet disjunction").
detect_resume_points_in_non_disj([Goal0 | Goals0], Liveness0, LiveInfo,
		ResumeVars0, [Goal | Goals], Liveness, Needed) :-
	(
		% If there are any more disjuncts, then this disjunct
		% establishes a resumption point.
		Goals0 = [_ | _]
	->
		detect_resume_points_in_non_disj(Goals0, Liveness0, LiveInfo,
			ResumeVars0, Goals, LivenessRest, NeededRest),
		detect_resume_points_in_non_last_disjunct(Goal0, no,
			Liveness0, LivenessRest, LiveInfo, ResumeVars0, Goal,
			Liveness, NeededRest, Needed)
	;
		detect_resume_points_in_last_disjunct(Goal0, Liveness0,
			LiveInfo, ResumeVars0, Goal, Liveness, Needed),
		Goals = Goals0
	).

:- pred detect_resume_points_in_pruned_disj(list(hlds_goal), set(prog_var),
		live_info, set(prog_var), list(hlds_goal), set(prog_var),
		set(prog_var)).
:- mode detect_resume_points_in_pruned_disj(in, in, in, in, out, out, out)
	is det.

detect_resume_points_in_pruned_disj([], Liveness, _, _, [], Liveness, Needed) :-
	set__init(Needed).
detect_resume_points_in_pruned_disj([Goal0 | Goals0], Liveness0, LiveInfo,
		ResumeVars0, [Goal | Goals], Liveness, Needed) :-
	Goal0 = _ - GoalInfo0,
	goal_info_get_determinism(GoalInfo0, Detism0),
	determinism_components(Detism0, CanFail0, _),
	(
		% This disjunct establishes a resumption point only if
		% there are more disjuncts *and* this one can fail.
		% If there are more disjuncts but this one can't fail,
		% then the code generator will ignore any later disjuncts,
		% so this one will be effectively the last.
		CanFail0 = can_fail,
		Goals0 = [_ | _]
	->
		detect_resume_points_in_pruned_disj(Goals0, Liveness0, LiveInfo,
			ResumeVars0, Goals, LivenessRest, NeededRest),
		detect_resume_points_in_non_last_disjunct(Goal0, yes,
			Liveness0, LivenessRest, LiveInfo, ResumeVars0, Goal,
			Liveness, NeededRest, Needed)
	;
		detect_resume_points_in_last_disjunct(Goal0, Liveness0,
			LiveInfo, ResumeVars0, Goal, Liveness, Needed),
		Goals = Goals0
	).

:- pred detect_resume_points_in_non_last_disjunct(hlds_goal, bool,
	set(prog_var), set(prog_var), live_info, set(prog_var), hlds_goal,
	set(prog_var), set(prog_var), set(prog_var)).
:- mode detect_resume_points_in_non_last_disjunct(in, in, in, in, in, in,
	out, out, in, out) is det.

detect_resume_points_in_non_last_disjunct(Goal0, MayUseOrigOnly,
		Liveness0, LivenessRest, LiveInfo, ResumeVars0, Goal,
		Liveness, NeededRest, Needed) :-
	% we must save a variable across this disjunct if it is
	% needed in a later disjunct or in an enclosing resume point

	set__union(NeededRest, ResumeVars0, ResumeVars1),
	detect_resume_points_in_goal(Goal0, Liveness0, LiveInfo,
		ResumeVars1, Goal1, Liveness),

	% Figure out which entry labels we need at the resumption point.
	% By minimizing the number of labels we use, we also minimize
	% the amount of data movement code we emit between such labels.
	(
		MayUseOrigOnly = yes,
		code_util__cannot_stack_flush(Goal1)
	->
		ResumeLocs = orig_only
	;
		code_util__cannot_fail_before_stack_flush(Goal1)
	->
		ResumeLocs = stack_only
	;
		ResumeLocs = stack_and_orig
	),

	% Attach the set of variables needed in the following disjuncts
	% as the resume point set of this disjunct.
	Resume = resume_point(ResumeVars1, ResumeLocs),
	goal_set_resume_point(Goal1, Resume, Goal),

	Goal = _ - GoalInfo,
	goal_info_get_pre_deaths(GoalInfo, PreDeaths),
	set__difference(Liveness0, PreDeaths, NeededFirst),
	liveness__maybe_complete_with_typeinfos(LiveInfo, NeededFirst,
		CompletedNeededFirst),
		% NeededRest has already been completed.
	set__union(CompletedNeededFirst, NeededRest, Needed),

	require_equal(Liveness, LivenessRest, "disjunction", LiveInfo).

:- pred detect_resume_points_in_last_disjunct(hlds_goal, set(prog_var),
	live_info, set(prog_var), hlds_goal, set(prog_var), set(prog_var)).
:- mode detect_resume_points_in_last_disjunct(in, in, in, in, out, out, out)
	is det.

detect_resume_points_in_last_disjunct(Goal0, Liveness0, LiveInfo,
		ResumeVars0, Goal, Liveness, CompletedNeeded) :-
	detect_resume_points_in_goal(Goal0, Liveness0, LiveInfo,
		ResumeVars0, Goal, Liveness),
	Goal = _ - GoalInfo,
	goal_info_get_pre_deaths(GoalInfo, PreDeaths),
	set__difference(Liveness0, PreDeaths, Needed),
	liveness__maybe_complete_with_typeinfos(LiveInfo, Needed,
		CompletedNeeded).

:- pred detect_resume_points_in_cases(list(case), set(prog_var), live_info,
		set(prog_var), list(case), set(prog_var)).
:- mode detect_resume_points_in_cases(in, in, in, in, out, out) is det.

detect_resume_points_in_cases([], Liveness, _, _, [], Liveness).
detect_resume_points_in_cases([case(ConsId, Goal0) | Cases0], Liveness0,
		LiveInfo, ResumeVars0,
		[case(ConsId, Goal) | Cases], LivenessFirst) :-
	detect_resume_points_in_goal(Goal0, Liveness0, LiveInfo, ResumeVars0,
		Goal, LivenessFirst),
	( Cases0 = [_ | _] ->
		detect_resume_points_in_cases(Cases0, Liveness0, LiveInfo,
			ResumeVars0, Cases, LivenessRest),
		require_equal(LivenessFirst, LivenessRest, "switch",
			LiveInfo)
	;
		Cases = Cases0
	).

:- pred detect_resume_points_in_par_conj(list(hlds_goal), set(prog_var),
		live_info, set(prog_var), list(hlds_goal), set(prog_var)).
:- mode detect_resume_points_in_par_conj(in, in, in, in, out, out) is det.

detect_resume_points_in_par_conj([], Liveness, _, _, [], Liveness).
detect_resume_points_in_par_conj([Goal0 | Goals0], Liveness0, LiveInfo,
		ResumeVars0, [Goal | Goals], LivenessFirst) :-
	detect_resume_points_in_goal(Goal0, Liveness0, LiveInfo, ResumeVars0,
		Goal, LivenessFirst),
	detect_resume_points_in_par_conj(Goals0, Liveness0, LiveInfo,
		ResumeVars0, Goals, _LivenessRest).

:- pred require_equal(set(prog_var), set(prog_var), string, live_info).
:- mode require_equal(in, in, in, in) is det.

require_equal(LivenessFirst, LivenessRest, GoalType, LiveInfo) :-
	(
		set__equal(LivenessFirst, LivenessRest)
	->
		true
	;
		Varset = LiveInfo^varset,
		set__to_sorted_list(LivenessFirst, FirstVarsList),
		set__to_sorted_list(LivenessRest, RestVarsList),
		list__map(varset__lookup_name(Varset),
			FirstVarsList, FirstVarNames),
		list__map(varset__lookup_name(Varset),
			RestVarsList, RestVarNames),
		Pad = lambda([S0::in, S::out] is det,
			string__append(S0, " ", S)),
		list__map(Pad, FirstVarNames, PaddedFirstNames),
		list__map(Pad, RestVarNames, PaddedRestNames),
		string__append_list(PaddedFirstNames, FirstNames),
		string__append_list(PaddedRestNames, RestNames),
		string__append_list(["branches of ", GoalType, 
			" disagree on liveness\nFirst: ",
			FirstNames, "\nRest: ", RestNames], Msg),
		error(Msg)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

initial_liveness(ProcInfo, PredId, ModuleInfo, Liveness) :-
	proc_info_headvars(ProcInfo, Vars),
	proc_info_argmodes(ProcInfo, Modes),
	proc_info_vartypes(ProcInfo, VarTypes),
	map__apply_to_list(Vars, VarTypes, Types),
	set__init(Liveness0),
	(
		initial_liveness_2(Vars, Modes, Types, ModuleInfo,
			Liveness0, Liveness1)
	->
		Liveness2 = Liveness1
	;
		error("initial_liveness: list length mismatch")
	),

		% If a variable is unused in the goal, it shouldn't be
		% in the initial liveness. (If we allowed it to start
		% live, it wouldn't ever become dead, because it would
		% have to be used to be killed).
		% So we intersect the headvars with the non-locals and
		% (if doing typeinfo liveness calculation) their
		% typeinfo vars.
	module_info_globals(ModuleInfo, Globals),
	proc_info_goal(ProcInfo, _Goal - GoalInfo),
	goal_info_get_code_gen_nonlocals(GoalInfo, NonLocals0),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	body_should_use_typeinfo_liveness(Globals, TypeinfoLiveness),
	pred_info_module(PredInfo, PredModule),
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, PredArity),
	( 	
		TypeinfoLiveness = yes,
		\+ polymorphism__no_type_info_builtin(PredModule,
			PredName, PredArity)
	->
		proc_info_typeinfo_varmap(ProcInfo, TVarMap),
		proc_info_get_typeinfo_vars(NonLocals0, VarTypes, TVarMap,
			TypeInfoNonLocals),
		set__union(NonLocals0, TypeInfoNonLocals, NonLocals)
	;
		NonLocals = NonLocals0
	),
	set__intersect(Liveness2, NonLocals, Liveness).

:- pred initial_liveness_2(list(prog_var), list(mode), list(type), module_info,
		set(prog_var), set(prog_var)).
:- mode initial_liveness_2(in, in, in, in, in, out) is semidet.

initial_liveness_2([], [], [], _ModuleInfo, Liveness, Liveness).
initial_liveness_2([V | Vs], [M | Ms], [T | Ts], ModuleInfo,
		Liveness0, Liveness) :-
	(
		mode_to_arg_mode(ModuleInfo, M, T, top_in)
	->
		set__insert(Liveness0, V, Liveness1)
	;
		Liveness1 = Liveness0
	),
	initial_liveness_2(Vs, Ms, Ts, ModuleInfo, Liveness1, Liveness).

%-----------------------------------------------------------------------------%

:- pred initial_deadness(proc_info, live_info, module_info, set(prog_var)).
:- mode initial_deadness(in, in, in, out) is det.

initial_deadness(ProcInfo, LiveInfo, ModuleInfo, Deadness) :-
	proc_info_headvars(ProcInfo, Vars),
	proc_info_argmodes(ProcInfo, Modes),
	proc_info_vartypes(ProcInfo, VarTypes),
	map__apply_to_list(Vars, VarTypes, Types),
	set__init(Deadness0),
		% All output arguments are in the initial deadness.
	(
		initial_deadness_2(Vars, Modes, Types, ModuleInfo,
			Deadness0, Deadness1)
	->
		Deadness2 = Deadness1
	;
		error("initial_deadness: list length mis-match")
	),

		% If doing alternate liveness, the corresponding
		% typeinfos need to be added to these.
	( 
		LiveInfo^typeinfo_liveness = yes
	->
		proc_info_typeinfo_varmap(ProcInfo, TVarMap),
		proc_info_get_typeinfo_vars(Deadness2, VarTypes, TVarMap,
			TypeInfoVars),
		set__union(Deadness2, TypeInfoVars, Deadness)
	;
		Deadness = Deadness2
	).

:- pred initial_deadness_2(list(prog_var), list(mode), list(type),
		module_info, set(prog_var), set(prog_var)).
:- mode initial_deadness_2(in, in, in, in, in, out) is semidet.

initial_deadness_2([], [], [], _ModuleInfo, Deadness, Deadness).
initial_deadness_2([V | Vs], [M | Ms], [T | Ts], ModuleInfo,
		Deadness0, Deadness) :-
	(
		mode_to_arg_mode(ModuleInfo, M, T, top_out)
	->
		set__insert(Deadness0, V, Deadness1)
	;
		Deadness1 = Deadness0
	),
	initial_deadness_2(Vs, Ms, Ts, ModuleInfo, Deadness1, Deadness).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred add_liveness_after_goal(hlds_goal, set(prog_var), hlds_goal).
:- mode add_liveness_after_goal(in, in, out) is det.

add_liveness_after_goal(Goal - GoalInfo0, Residue, Goal - GoalInfo) :-
	goal_info_get_post_births(GoalInfo0, PostBirths0),
	set__union(PostBirths0, Residue, PostBirths),
	goal_info_set_post_births(GoalInfo0, PostBirths, GoalInfo).

:- pred add_deadness_before_goal(hlds_goal, set(prog_var), hlds_goal).
:- mode add_deadness_before_goal(in, in, out) is det.

add_deadness_before_goal(Goal - GoalInfo0, Residue, Goal - GoalInfo) :-
	goal_info_get_pre_deaths(GoalInfo0, PreDeaths0),
	set__union(PreDeaths0, Residue, PreDeaths),
	goal_info_set_pre_deaths(GoalInfo0, PreDeaths, GoalInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Given a list of variables and an instmap delta, determine which
	% of those variables have a value given to them (i.e. they are bound
	% or aliased; in the latter case the "value" is the location they
	% should be stored in), and insert them into the accumulated set
	% of value-given vars.
	%
	% We don't handle the aliasing part yet.

:- pred find_value_giving_occurrences(list(prog_var), live_info,
	instmap_delta, set(prog_var), set(prog_var)).
:- mode find_value_giving_occurrences(in, in, in, in, out) is det.

find_value_giving_occurrences([], _, _, ValueVars, ValueVars).
find_value_giving_occurrences([Var | Vars], LiveInfo, InstMapDelta,
		ValueVars0, ValueVars) :-
	VarTypes = LiveInfo^vartypes,
	map__lookup(VarTypes, Var, Type),
	(
		instmap_delta_search_var(InstMapDelta, Var, Inst),
		ModuleInfo = LiveInfo^module_info,
		mode_to_arg_mode(ModuleInfo, (free -> Inst), Type, top_out)
	->
		set__insert(ValueVars0, Var, ValueVars1)
	;
		ValueVars1 = ValueVars0
	),
	find_value_giving_occurrences(Vars, LiveInfo, InstMapDelta,
		ValueVars1, ValueVars).

%-----------------------------------------------------------------------------%

	% Get the nonlocals, and, if doing alternate liveness, add the
	% typeinfo vars for the nonlocals.

:- pred liveness__get_nonlocals_and_typeinfos(live_info::in, hlds_goal_info::in,
	set(prog_var)::out) is det.

liveness__get_nonlocals_and_typeinfos(LiveInfo, GoalInfo, 
		NonLocals) :-
	goal_info_get_code_gen_nonlocals(GoalInfo, NonLocals0),
	liveness__maybe_complete_with_typeinfos(LiveInfo,
		NonLocals0, NonLocals).

:- pred liveness__maybe_complete_with_typeinfos(live_info::in,
	set(prog_var)::in, set(prog_var)::out) is det.

liveness__maybe_complete_with_typeinfos(LiveInfo, Vars0, Vars) :-
	proc_info_maybe_complete_with_typeinfo_vars(Vars0,
		LiveInfo^typeinfo_liveness, LiveInfo^vartypes,
		LiveInfo^type_info_varmap, Vars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type live_info
	--->	live_info(
			module_info		::	module_info,
			typeinfo_liveness	::	bool,
			vartypes		::	vartypes,
			type_info_varmap	::	type_info_varmap,
			varset			::	prog_varset
		).

:- pred live_info_init(module_info::in, bool::in, vartypes::in,
	type_info_varmap::in, prog_varset::in, live_info::out) is det.

live_info_init(ModuleInfo, ProcInfo, TypeInfoLiveness, VarTypes, Varset,
	live_info(ModuleInfo, ProcInfo, TypeInfoLiveness, VarTypes, Varset)).

%-----------------------------------------------------------------------------%
