%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: liveness.m
%
% Main authors: conway, zs.
%
% This module traverses the goal for each procedure, and adds
% liveness annotations to the goal_info for each sub-goal.
%
% Note - the concept of `liveness' here is different to that used in
% mode analysis. Mode analysis is concerned with the liveness of what
% is *pointed* to by a variable, for the purpose of avoiding and/or keeping
% track of aliasing and for structure re-use optimization, whereas here
% we are concerned with the liveness of the variable itself, for the
% purposes of optimizing stack slot and register usage.
%
% We compute liveness related information in three distinct passes.
%
% The first pass, detect_liveness_in_goal, finds the first occurrence
% of each variable on each computation path. Goals containing the first
% occurrence of a variable include that variable in their pre-birth
% set. In branched structures, branches whose endpoint is not reachable
% include a post-birth set listing the variables that should have been
% born in that branch but haven't.
%
% The second pass, detect_deadness_in_goal, finds the last occurrence
% of each variable on each computation path. Goals containing the last
% occurrence of a variable include that variable in their post-death
% set. In branched structures, branches in which a variable is not
% used at all include a pre-death set listing the variables that
% have died in parallel branches. Note that when using accurate gc,
% a variable holding a typeinfo is live while any variable described
% (in whole or in part) by that typeinfo is live.
%
% The third pass, detect_resume_points_in_goal, finds goals that
% establish resume points and attaches to them a resume_point
% annotation listing the variables that may be referenced by the
% code at that resume point as well as the nature of the required
% entry labels.

%-----------------------------------------------------------------------------%

:- module liveness.

:- interface.

:- import_module hlds_module, hlds_pred.
:- import_module io.

	% Add liveness annotations to the goal of the procedure.
	% This consists of the {pre,post}{birth,death} sets and
	% resume point information.

:- pred detect_liveness_proc(pred_id, proc_id, module_info,
	proc_info, proc_info, io__state, io__state).
:- mode detect_liveness_proc(in, in, in, in, out, di, uo) is det.

	% Return the set of variables live at the start of the procedure.

:- pred initial_liveness(proc_info, module_info, set(var)).
:- mode initial_liveness(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, hlds_data, llds, quantification, instmap.
:- import_module hlds_out, mode_util, code_util.
:- import_module prog_data, globals, passes_aux.
:- import_module bool, list, map, set, std_util, term, assoc_list, require.
:- import_module varset, string.

detect_liveness_proc(PredId, ProcId, ModuleInfo, ProcInfo0, ProcInfo) -->
	write_proc_progress_message("% Computing liveness in ", PredId, ProcId,
		ModuleInfo),
	{ proc_info_goal(ProcInfo0, Goal0) },
	{ proc_info_variables(ProcInfo0, Varset) },
	{ proc_info_vartypes(ProcInfo0, VarTypes) },
	{ live_info_init(ModuleInfo, ProcInfo0, VarTypes, Varset, LiveInfo) },

	{ initial_liveness(ProcInfo0, ModuleInfo, Liveness0) },
	{ detect_liveness_in_goal(Goal0, Liveness0, LiveInfo,
		_, Goal1) },

	{ initial_deadness(ProcInfo0, ModuleInfo, Deadness0) },
	{ detect_deadness_in_goal(Goal1, Deadness0, LiveInfo, _, Goal2) },

	{ set__init(ResumeVars0) },
	{ detect_resume_points_in_goal(Goal2, Liveness0, LiveInfo,
		ResumeVars0, Goal, _) },

	{ proc_info_set_goal(ProcInfo0, Goal, ProcInfo1) },
	{ proc_info_set_liveness_info(ProcInfo1, Liveness0, ProcInfo) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_goal(hlds__goal, set(var), live_info,
	set(var), hlds__goal).
:- mode detect_liveness_in_goal(in, in, in, out, out) is det.

detect_liveness_in_goal(Goal0 - GoalInfo0, Liveness0, LiveInfo,
		Liveness, Goal - GoalInfo) :-

		% work out which variables get born in this goal
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
	set__difference(NonLocals, Liveness0, NewVarsSet),
	set__to_sorted_list(NewVarsSet, NewVarsList),
	goal_info_get_instmap_delta(GoalInfo0, InstMapDelta),
	set__init(Births0),
	find_binding_occurrences(NewVarsList, LiveInfo,
		InstMapDelta, Births0, Births),
	set__union(Liveness0, Births, Liveness),
	(
		goal_is_atomic(Goal0)
	->
		PreBirths = Births,
		set__init(PostBirths),
		Goal = Goal0
	;
		set__init(PreBirths),
		detect_liveness_in_goal_2(Goal0, Liveness0, NonLocals,
			LiveInfo, Liveness1, Goal),
		set__difference(Births, Liveness1, PostBirths)
	),
	goal_info_set_pre_births(GoalInfo0, PreBirths, GoalInfo1),
	goal_info_set_post_births(GoalInfo1, PostBirths, GoalInfo).

%-----------------------------------------------------------------------------%

	% Here we process each of the different sorts of goals.

:- pred detect_liveness_in_goal_2(hlds__goal_expr, set(var), set(var),
	live_info, set(var), hlds__goal_expr).
:- mode detect_liveness_in_goal_2(in, in, in, in, out, out) is det.

detect_liveness_in_goal_2(conj(Goals0), Liveness0, _, LiveInfo,
		Liveness, conj(Goals)) :-
	detect_liveness_in_conj(Goals0, Liveness0, LiveInfo, Liveness, Goals).

detect_liveness_in_goal_2(disj(Goals0, SM), Liveness0, NonLocals, LiveInfo,
		Liveness, disj(Goals, SM)) :-
	set__init(Union0),
	detect_liveness_in_disj(Goals0, Liveness0, NonLocals, LiveInfo,
		Union0, Union, Goals),
	set__union(Liveness0, Union, Liveness).

detect_liveness_in_goal_2(not(Goal0), Liveness0, _, LiveInfo,
		Liveness, not(Goal)) :-
	detect_liveness_in_goal(Goal0, Liveness0, LiveInfo, Liveness, Goal).

detect_liveness_in_goal_2(switch(Var, Det, Cases0, SM), Liveness0, NonLocals,
		LiveInfo, Liveness, switch(Var, Det, Cases, SM)) :-
	set__init(Union0),
	detect_liveness_in_cases(Cases0, Liveness0, NonLocals, LiveInfo,
		Union0, Union, Cases),
	set__union(Liveness0, Union, Liveness).

detect_liveness_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0, SM),
		Liveness0, NonLocals, LiveInfo, Liveness,
		if_then_else(Vars, Cond, Then, Else, SM)) :-
	detect_liveness_in_goal(Cond0, Liveness0, LiveInfo, LivenessCond, Cond),
	detect_liveness_in_goal(Then0, LivenessCond, LiveInfo, LivenessThen,
		Then1),
	detect_liveness_in_goal(Else0, Liveness0, LiveInfo, LivenessElse,
		Else1),

	set__union(LivenessThen, LivenessElse, Liveness),
	set__intersect(Liveness, NonLocals, NonLocalLiveness),

	set__difference(NonLocalLiveness, LivenessThen, ResidueThen),
	set__difference(NonLocalLiveness, LivenessElse, ResidueElse),

	stuff_liveness_residue_after_goal(Then1, ResidueThen, Then),
	stuff_liveness_residue_after_goal(Else1, ResidueElse, Else).

detect_liveness_in_goal_2(some(Vars, Goal0), Liveness0, _, LiveInfo,
		Liveness, some(Vars, Goal)) :-
	detect_liveness_in_goal(Goal0, Liveness0, LiveInfo, Liveness, Goal).

detect_liveness_in_goal_2(higher_order_call(A,B,C,D,E), L, _, _, L,
		higher_order_call(A,B,C,D,E)).

detect_liveness_in_goal_2(call(A,B,C,D,E,F), L, _, _, L, call(A,B,C,D,E,F)).

detect_liveness_in_goal_2(unify(A,B,C,D,E), L, _, _, L, unify(A,B,C,D,E)).

detect_liveness_in_goal_2(pragma_c_code(A,B,C,D,E,F), L, _, _, L,
		pragma_c_code(A,B,C,D,E,F)).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_conj(list(hlds__goal), set(var), live_info,
	set(var), list(hlds__goal)).
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

:- pred detect_liveness_in_disj(list(hlds__goal), set(var), set(var),
	live_info, set(var), set(var), list(hlds__goal)).
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
	stuff_liveness_residue_after_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_cases(list(case), set(var), set(var),
	live_info, set(var), set(var), list(case)).
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
	stuff_liveness_residue_after_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_goal(hlds__goal, set(var), live_info,
	set(var), hlds__goal).
:- mode detect_deadness_in_goal(in, in, in, out, out) is det.

detect_deadness_in_goal(Goal0 - GoalInfo0, Deadness0, LiveInfo, Deadness,
		Goal - GoalInfo) :-
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
	live_info_get_module_info(LiveInfo, ModuleInfo),
	module_info_globals(ModuleInfo, Globals),
	globals__get_gc_method(Globals, GCmethod),
	(
		goal_is_atomic(Goal0)
	->
		(
			GCmethod = accurate
		->
			live_info_get_proc_info(LiveInfo, ProcInfo),
			proc_info_get_used_typeinfos_setwise(ProcInfo,
				NonLocals, TypeInfoVars),
			set__union(NonLocals, TypeInfoVars, NonLocals1)
		;
			NonLocals1 = NonLocals
		),
		set__init(PreDeaths),
		set__difference(NonLocals1, Deadness0, PostDeaths),
		set__union(Deadness0, PostDeaths, Deadness),
		Goal = Goal0
	;
		set__union(Deadness0, NonLocals, DeadnessNonlocals),
		(
			GCmethod = accurate
		->
			live_info_get_proc_info(LiveInfo, ProcInfo),
			proc_info_get_used_typeinfos_setwise(ProcInfo,
				NonLocals, TypeInfoVars),
			set__union(DeadnessNonlocals, TypeInfoVars,
				Deadness)
		;
			Deadness = DeadnessNonlocals
		),
		set__init(PostDeaths),
		detect_deadness_in_goal_2(Goal0, GoalInfo0, Deadness0,
			LiveInfo, Deadness1, Goal),
		set__difference(Deadness, Deadness1, PreDeaths)
	),
	goal_info_set_post_deaths(GoalInfo0, PostDeaths, GoalInfo1),
	goal_info_set_pre_deaths(GoalInfo1, PreDeaths, GoalInfo).

	% Here we process each of the different sorts of goals.

:- pred detect_deadness_in_goal_2(hlds__goal_expr, hlds__goal_info,
	set(var), live_info, set(var), hlds__goal_expr).
:- mode detect_deadness_in_goal_2(in, in, in, in, out, out) is det.

detect_deadness_in_goal_2(conj(Goals0), _, Deadness0, LiveInfo,
		Deadness, conj(Goals)) :-
	detect_deadness_in_conj(Goals0, Deadness0, LiveInfo,
		Goals, Deadness).

detect_deadness_in_goal_2(disj(Goals0, SM), GoalInfo, Deadness0,
		LiveInfo, Deadness, disj(Goals, SM)) :-
	set__init(Union0),
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	detect_deadness_in_disj(Goals0, Deadness0, NonLocals,
		LiveInfo, Union0, Union, Goals),
	set__union(Deadness0, Union, Deadness).

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

	goal_info_get_nonlocals(GoalInfo, NonLocals),
	set__union(DeadnessCond, DeadnessElse, Deadness),
	set__intersect(Deadness, NonLocals, NonLocalDeadness),

	set__difference(NonLocalDeadness, DeadnessCond, ResidueCond),
	set__difference(NonLocalDeadness, DeadnessElse, ResidueElse),

	stuff_deadness_residue_before_goal(Cond1, ResidueCond, Cond),
	stuff_deadness_residue_before_goal(Else1, ResidueElse, Else).

detect_deadness_in_goal_2(switch(Var, Det, Cases0, SM), GoalInfo, Deadness0,
		LiveInfo, Deadness, switch(Var, Det, Cases, SM)) :-
	set__init(Union0),
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	detect_deadness_in_cases(Var, Cases0, Deadness0, NonLocals, LiveInfo,
		Union0, Union, Cases),
	set__union(Deadness0, Union, Deadness).

detect_deadness_in_goal_2(some(Vars, Goal0), _, Deadness0, LiveInfo,
		Deadness, some(Vars, Goal)) :-
	detect_deadness_in_goal(Goal0, Deadness0, LiveInfo, Deadness, Goal).

detect_deadness_in_goal_2(higher_order_call(A,B,C,D,E), _, Dn, _, Dn,
			higher_order_call(A,B,C,D,E)).

detect_deadness_in_goal_2(call(A,B,C,D,E,F), _, Dn, _, Dn,
			call(A,B,C,D,E,F)).

detect_deadness_in_goal_2(unify(A,B,C,D,E), _, Dn, _, Dn, unify(A,B,C,D,E)).

detect_deadness_in_goal_2(pragma_c_code(A,B,C,D,E,F), _, Dn, _, Dn,
		pragma_c_code(A,B,C,D,E,F)).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_conj(list(hlds__goal), set(var), live_info,
	list(hlds__goal), set(var)).
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

:- pred detect_deadness_in_disj(list(hlds__goal), set(var), set(var),
	live_info, set(var), set(var), list(hlds__goal)).
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
	stuff_deadness_residue_before_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_cases(var, list(case), set(var), set(var),
	live_info, set(var), set(var), list(case)).
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
	stuff_deadness_residue_before_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_resume_points_in_goal(hlds__goal, set(var), live_info, set(var),
	hlds__goal, set(var)).
:- mode detect_resume_points_in_goal(in, in, in, in, out, out) is det.

detect_resume_points_in_goal(Goal0 - GoalInfo0, Liveness0, LiveInfo,
		ResumeVars0, Goal - GoalInfo0, Liveness) :-
	goal_info_pre_births(GoalInfo0, PreBirths0),
	goal_info_post_births(GoalInfo0, PostBirths0),
	goal_info_pre_deaths(GoalInfo0, PreDeaths0),
	goal_info_post_deaths(GoalInfo0, PostDeaths0),

	set__difference(Liveness0, PreDeaths0, Liveness1),
	set__union(Liveness1, PreBirths0, Liveness2),

	detect_resume_points_in_goal_2(Goal0, GoalInfo0, Liveness2, LiveInfo,
		ResumeVars0, Goal, Liveness3),

	set__difference(Liveness3, PostDeaths0, Liveness4),
	set__union(Liveness4, PostBirths0, Liveness).

:- pred detect_resume_points_in_goal_2(hlds__goal_expr, hlds__goal_info,
	set(var), live_info, set(var), hlds__goal_expr, set(var)).
:- mode detect_resume_points_in_goal_2(in, in, in, in, in, out, out) is det.

detect_resume_points_in_goal_2(conj(Goals0), _, Liveness0, LiveInfo,
		ResumeVars0, conj(Goals), Liveness) :-
	detect_resume_points_in_conj(Goals0, Liveness0, LiveInfo, ResumeVars0,
		Goals, Liveness).

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
	goal_info_pre_deaths(ElseInfo0, ElsePreDeath0),
	set__difference(Liveness0, ElsePreDeath0, CondResumeVars0),
	set__union(CondResumeVars0, ResumeVars0, CondResumeVars),

	(
		code_util__cannot_stack_flush(Cond0),
		goal_info_get_code_model(GoalInfo0, CodeModel),
		CodeModel \= model_non
	->
		CondResumeLocs = orig_only
	;
		code_util__cannot_fail_before_stack_flush(Cond0)
	->
		CondResumeLocs = stack_only
	;
		CondResumeLocs = stack_and_orig
	),

	CondResume = resume_point(CondResumeVars, CondResumeLocs),
	goal_set_resume_point(Cond0, CondResume, Cond1),

	detect_resume_points_in_goal(Cond1, Liveness0, LiveInfo,
		CondResumeVars, Cond, LivenessCond),
	detect_resume_points_in_goal(Then0, LivenessCond, LiveInfo,
		ResumeVars0, Then, LivenessThen),
	detect_resume_points_in_goal(Else0, Liveness0, LiveInfo,
		ResumeVars0, Else, LivenessElse),

	(
		set__equal(LivenessThen, LivenessElse)
	->
		true
	;
		live_info_get_varset(LiveInfo, Varset),
		set__to_sorted_list(LivenessThen, ThenVarsList),
		set__to_sorted_list(LivenessElse, ElseVarsList),
		list__map(varset__lookup_name(Varset),
			ThenVarsList, ThenVarNames),
		list__map(varset__lookup_name(Varset),
			ElseVarsList, ElseVarNames),
		Pad = lambda([S0::in, S::out] is det,
			string__append(S0, " ", S)),
		list__map(Pad, ThenVarNames, PaddedThenNames),
		list__map(Pad, ElseVarNames, PaddedElseNames),
		string__append_list(PaddedThenNames, ThenNames),
		string__append_list(PaddedElseNames, ElseNames),
		string__append_list([
			"branches of if-then-else disagree on liveness\nThen: ",
			ThenNames, "\nElse: ", ElseNames], Msg),
		error(Msg)
	).

detect_resume_points_in_goal_2(some(Vars, Goal0), _, Liveness0, LiveInfo,
		ResumeVars0, some(Vars, Goal), Liveness) :-
	detect_resume_points_in_goal(Goal0, Liveness0, LiveInfo, ResumeVars0,
					Goal, Liveness).

detect_resume_points_in_goal_2(not(Goal0), _, Liveness0, LiveInfo, ResumeVars0,
		not(Goal), Liveness) :-
	detect_resume_points_in_goal(Goal0, Liveness0, LiveInfo, ResumeVars0,
		_, Liveness),
	set__union(Liveness, ResumeVars0, ResumeVars1),
	detect_resume_points_in_goal(Goal0, Liveness0, LiveInfo, ResumeVars1,
		Goal1, _Liveness),
	% attach the set of variables alive after the negation
	% as the resume point set of the negated goal
	Resume = resume_point(ResumeVars1, stack_and_orig),
	goal_set_resume_point(Goal1, Resume, Goal).

detect_resume_points_in_goal_2(higher_order_call(A,B,C,D,E), _, Liveness, _, _,
		higher_order_call(A,B,C,D,E), Liveness).

detect_resume_points_in_goal_2(call(A,B,C,D,E,F), _, Liveness, _, _,
		call(A,B,C,D,E,F), Liveness).

detect_resume_points_in_goal_2(unify(A,B,C,D,E), _, Liveness, _, _,
		unify(A,B,C,D,E), Liveness).

detect_resume_points_in_goal_2(pragma_c_code(A,B,C,D,E,F), _, Liveness, _, _,
		pragma_c_code(A,B,C,D,E,F), Liveness).

:- pred detect_resume_points_in_conj(list(hlds__goal), set(var), live_info,
	set(var), list(hlds__goal), set(var)).
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

:- pred detect_resume_points_in_non_disj(list(hlds__goal), set(var),
	live_info, set(var), list(hlds__goal), set(var), set(var)).
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

:- pred detect_resume_points_in_pruned_disj(list(hlds__goal), set(var),
	live_info, set(var), list(hlds__goal), set(var), set(var)).
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

:- pred detect_resume_points_in_non_last_disjunct(hlds__goal, bool,
	set(var), set(var), live_info, set(var), hlds__goal,
	set(var), set(var), set(var)).
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

	Resume = resume_point(ResumeVars1, ResumeLocs),
	goal_set_resume_point(Goal1, Resume, Goal),

	Goal = _ - GoalInfo,
	goal_info_pre_deaths(GoalInfo, PreDeaths),
	set__difference(Liveness0, PreDeaths, NeededFirst),
	set__union(NeededFirst, NeededRest, Needed),

	require(set__equal(Liveness, LivenessRest),
		"branches of disjunction disagree on liveness").

:- pred detect_resume_points_in_last_disjunct(hlds__goal, set(var), live_info,
	set(var), hlds__goal, set(var), set(var)).
:- mode detect_resume_points_in_last_disjunct(in, in, in, in, out, out, out)
	is det.

detect_resume_points_in_last_disjunct(Goal0, Liveness0, LiveInfo,
		ResumeVars0, Goal, Liveness, Needed) :-
	detect_resume_points_in_goal(Goal0, Liveness0, LiveInfo,
		ResumeVars0, Goal, Liveness),
	Goal = _ - GoalInfo,
	goal_info_pre_deaths(GoalInfo, PreDeaths),
	set__difference(Liveness0, PreDeaths, Needed).

:- pred detect_resume_points_in_cases(list(case), set(var), live_info,
	set(var), list(case), set(var)).
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
		require(set__equal(LivenessFirst, LivenessRest),
			"branches of switch disagree on liveness")
	;
		Cases = Cases0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

initial_liveness(ProcInfo, ModuleInfo, Liveness) :-
	proc_info_headvars(ProcInfo, Vars),
	proc_info_argmodes(ProcInfo, Modes),
	proc_info_vartypes(ProcInfo, VarTypes),
	map__apply_to_list(Vars, VarTypes, Types),
	set__init(Liveness0),
	(
		initial_liveness_2(Vars, Modes, Types, ModuleInfo,
			Liveness0, Liveness1)
	->
		Liveness = Liveness1
	;
		error("initial_liveness: list length mismatch")
	).

:- pred initial_liveness_2(list(var), list(mode), list(type), module_info,
	set(var), set(var)).
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

:- pred initial_deadness(proc_info, module_info, set(var)).
:- mode initial_deadness(in, in, out) is det.

initial_deadness(ProcInfo, ModuleInfo, Deadness) :-
	proc_info_headvars(ProcInfo, Vars),
	proc_info_argmodes(ProcInfo, Modes),
	proc_info_vartypes(ProcInfo, VarTypes),
	map__apply_to_list(Vars, VarTypes, Types),
	set__init(Deadness0),
	(
		initial_deadness_2(Vars, Modes, Types, ModuleInfo,
			Deadness0, Deadness1)
	->
		Deadness2 = Deadness1
	;
		error("initial_deadness: list length mis-match")
	),
		% If doing accurate garbage collection, the corresponding
		% typeinfos need to be added to these.
	module_info_globals(ModuleInfo, Globals),
	globals__get_gc_method(Globals, GCmethod),
	(
		GCmethod = accurate
	->
		proc_info_get_used_typeinfos_setwise(ProcInfo, Deadness2,
			TypeInfoVars),
		set__union(Deadness2, TypeInfoVars, Deadness)
	;
		Deadness = Deadness2
	).

:- pred initial_deadness_2(list(var), list(mode), list(type),
				module_info, set(var), set(var)).
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

:- pred stuff_liveness_residue_after_goal(hlds__goal, set(var), hlds__goal).
:- mode stuff_liveness_residue_after_goal(in, in, out) is det.

stuff_liveness_residue_after_goal(Goal - GoalInfo0, Residue, Goal - GoalInfo) :-
	goal_info_post_births(GoalInfo0, PostBirths0),
	set__union(PostBirths0, Residue, PostBirths),
	goal_info_set_post_births(GoalInfo0, PostBirths, GoalInfo).

:- pred stuff_deadness_residue_before_goal(hlds__goal, set(var), hlds__goal).
:- mode stuff_deadness_residue_before_goal(in, in, out) is det.

stuff_deadness_residue_before_goal(Goal - GoalInfo0, Residue, Goal - GoalInfo)
		:-
	goal_info_pre_deaths(GoalInfo0, PreDeaths0),
	set__union(PreDeaths0, Residue, PreDeaths),
	goal_info_set_pre_deaths(GoalInfo0, PreDeaths, GoalInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Given a list of variables and an instmap delta, determine
	% which of those variables become bound (according to the instmap
	% delta) and insert them into the accumulated set of bound vars.

:- pred find_binding_occurrences(list(var), live_info,
	instmap_delta, set(var), set(var)).
:- mode find_binding_occurrences(in, in, in, in, out) is det.

find_binding_occurrences([], _, _, BoundVars, BoundVars).
find_binding_occurrences([Var | Vars], LiveInfo, InstMapDelta,
		BoundVars0, BoundVars) :-
	live_info_get_var_types(LiveInfo, VarTypes),
	live_info_get_module_info(LiveInfo, ModuleInfo),
	map__lookup(VarTypes, Var, Type),
	instmap_delta_lookup_var(InstMapDelta, Var, Inst),
	( mode_to_arg_mode(ModuleInfo, (free -> Inst), Type, top_out) ->
		set__insert(BoundVars0, Var, BoundVars1)
	;
		BoundVars1 = BoundVars0
	),
	find_binding_occurrences(Vars, LiveInfo, InstMapDelta,
		BoundVars1, BoundVars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type live_info	--->	live_info(
					module_info,
					proc_info,
					map(var, type),
					varset
				).

:- pred live_info_init(module_info, proc_info, map(var, type),
	varset, live_info).
:- mode live_info_init(in, in, in, in, out) is det.

live_info_init(ModuleInfo, ProcInfo, VarTypes, Varset,
	live_info(ModuleInfo, ProcInfo, VarTypes, Varset)).

:- pred live_info_get_module_info(live_info, module_info).
:- mode live_info_get_module_info(in, out) is det.

live_info_get_module_info(live_info(ModuleInfo, _, _, _), ModuleInfo).

:- pred live_info_get_proc_info(live_info, proc_info).
:- mode live_info_get_proc_info(in, out) is det.

live_info_get_proc_info(live_info(_, ProcInfo, _, _), ProcInfo).

:- pred live_info_get_var_types(live_info, map(var, type)).
:- mode live_info_get_var_types(in, out) is det.

live_info_get_var_types(live_info(_, _, VarTypes, _), VarTypes).

:- pred live_info_get_varset(live_info, varset).
:- mode live_info_get_varset(in, out) is det.

live_info_get_varset(live_info(_, _, _, Varset), Varset).

%-----------------------------------------------------------------------------%
