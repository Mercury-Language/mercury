%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module defines annotations on HLDS goals that are used by the LLDS
% back end.

% Author: zs.

:- module hlds__hlds_llds.

:- interface.

:- import_module hlds__hlds_goal, parse_tree__prog_data, ll_backend__llds.
:- import_module bool, map, set, std_util.

%
% The following types are annotations on the HLDS
% that are used only by the LLDS back-end.
%

:- type stack_slots	==	map(prog_var, lval).
				% Maps variables to their stack slots.
				% The only legal lvals in the range are
				% stackvars and framevars.

:- type follow_vars_map	==	map(prog_var, lval).

:- type follow_vars	--->	follow_vars(follow_vars_map, int).
				% Advisory information about where variables
				% ought to be put next. Variables may or may
				% not appear in the map. If they do, then the
				% associated lval says where the value of that
				% variable ought to be put when it is computed,
				% or, if the lval refers to the nonexistent
				% register r(-1), it says that it should be
				% put into an available register. The integer
				% in the second half of the pair gives the
				% number of the first register that is
				% not reserved for other purposes, and is
				% free to hold such variables.

:- type store_map	==	map(prog_var, lval).
				% Authoritative information about where
				% variables must be put at the ends of
				% branches of branched control structures.
				% However, between the follow_vars and
				% and store_alloc passes, these fields
				% temporarily hold follow_vars information.
				% Apart from this, the legal range is
				% the set of legal lvals.

	% see compiler/notes/allocation.html for what these alternatives mean
:- type resume_point	--->	resume_point(set(prog_var), resume_locs)
			;	no_resume_point.

:- type resume_locs	--->	orig_only
			;	stack_only
			;	orig_and_stack
			;	stack_and_orig.

	% When code in live_vars.m finds out what variables need to be saved
	% across calls (including generic calls and foreign_procs that can call
	% back to Mercury), at resume points, and in parallel conjunctions, it
	% records the results in the code_gen_details field of hlds_goal_infos.
	% It does so in values of types need_across_call, need_in_resume and
	% need_in_par_conj.

	% call_forward_vars contains the set of variables that need to be saved
	% across a call because they are needed when the call succeeds.
	% call_resume vars contains the set of variables that need to be saved
	% across a call because they are needed when the call fails, and
	% execution backtracks to a resume point to the call's left.
	% call_nondet_vars contains the set of variables that need to be on
	% the stack across a call because they are needed by a model_non goal
	% to the left of the call.

:- type need_across_call
	--->	need_across_call(
	 		call_forward_vars	:: set(prog_var),
	 		call_resume_vars	:: set(prog_var),
	 		call_nondet_vars	:: set(prog_var)
		).

	% resume_vars_on_stack is true if the resume point has a stack label.
	% resume_resume vars contains the set of variables that need to be
	% saved at a resume point because they are needed when execution
	% backtracks to this resume point, or to a resume point on its left.
	% resume_nondet_vars contains the set of variables that need to be on
	% the stack at a resume point because they are needed by a model_non
	% goal to the left of the resume point.

:- type need_in_resume
	--->	need_in_resume(
			resume_vars_on_stack	:: bool,
	 		resume_resume_vars	:: set(prog_var),
	 		resume_nondet_vars	:: set(prog_var)
		).

	% par_conj_engine_vars gives the set of variables that the execution
	% mechanism of the parallel conjunction requires to be stored in stack
	% slots.
:- type need_in_par_conj
	--->	need_in_par_conj(
	 		par_conj_engine_vars	:: set(prog_var)
		).

:- type llds_code_gen_details.

%-----------------------------------------------------------------------------%

% Instead of recording the liveness of every variable at every
% part of the goal, we just keep track of the initial liveness
% and the changes in liveness.  Note that when traversing forwards
% through a goal, deaths must be applied before births;
% this is necessary to handle certain circumstances where a
% variable can occur in both the post-death and post-birth sets,
% or in both the pre-death and pre-birth sets.

:- pred goal_info_get_pre_births(hlds_goal_info::in,
	set(prog_var)::out) is det.

:- pred goal_info_get_post_births(hlds_goal_info::in,
	set(prog_var)::out) is det.

:- pred goal_info_get_pre_deaths(hlds_goal_info::in,
	set(prog_var)::out) is det.

:- pred goal_info_get_post_deaths(hlds_goal_info::in,
	set(prog_var)::out) is det.

:- pred goal_info_get_follow_vars(hlds_goal_info::in,
	maybe(follow_vars)::out) is det.

:- pred goal_info_get_store_map(hlds_goal_info::in,
	store_map::out) is det.

:- pred goal_info_get_resume_point(hlds_goal_info::in,
	resume_point::out) is det.

:- pred goal_info_get_maybe_need_across_call(hlds_goal_info::in,
	maybe(need_across_call)::out) is det.

:- pred goal_info_get_maybe_need_in_resume(hlds_goal_info::in,
	maybe(need_in_resume)::out) is det.

:- pred goal_info_get_maybe_need_in_par_conj(hlds_goal_info::in,
	maybe(need_in_par_conj)::out) is det.

%-----------------------------------------------------------------------------%

:- pred goal_info_maybe_get_pre_births(hlds_goal_info::in,
	set(prog_var)::out) is semidet.

:- pred goal_info_maybe_get_post_births(hlds_goal_info::in,
	set(prog_var)::out) is semidet.

:- pred goal_info_maybe_get_pre_deaths(hlds_goal_info::in,
	set(prog_var)::out) is semidet.

:- pred goal_info_maybe_get_post_deaths(hlds_goal_info::in,
	set(prog_var)::out) is semidet.

:- pred goal_info_maybe_get_follow_vars(hlds_goal_info::in,
	maybe(follow_vars)::out) is semidet.

:- pred goal_info_maybe_get_store_map(hlds_goal_info::in,
	store_map::out) is semidet.

:- pred goal_info_maybe_get_resume_point(hlds_goal_info::in,
	resume_point::out) is semidet.

:- pred goal_info_maybe_get_maybe_need_across_call(hlds_goal_info::in,
	maybe(need_across_call)::out) is semidet.

:- pred goal_info_maybe_get_maybe_need_in_resume(hlds_goal_info::in,
	maybe(need_in_resume)::out) is semidet.

:- pred goal_info_maybe_get_maybe_need_in_par_conj(hlds_goal_info::in,
	maybe(need_in_par_conj)::out) is semidet.

%-----------------------------------------------------------------------------%

	% goal_info_initialize_liveness_info(GoalInfo0, PreBirths, PostBirths,
	%	PreDeaths, PostDeaths, ResumePoint, GoalInfo):
	% Updates GoalInfo0 by overwriting the previous values of its
	% pre_births, post_births, pre_deaths, post_deaths and resume_point
	% fields, yielding GoalInfo.

:- pred goal_info_initialize_liveness_info(hlds_goal_info::in,
	set(prog_var)::in, set(prog_var)::in, set(prog_var)::in,
	set(prog_var)::in, resume_point::in, hlds_goal_info::out) is det.

:- pred goal_info_set_pre_births(hlds_goal_info::in, set(prog_var)::in,
	hlds_goal_info::out) is det.

:- pred goal_info_set_post_births(hlds_goal_info::in, set(prog_var)::in,
	hlds_goal_info::out) is det.

:- pred goal_info_set_pre_deaths(hlds_goal_info::in, set(prog_var)::in,
	hlds_goal_info::out) is det.

:- pred goal_info_set_post_deaths(hlds_goal_info::in, set(prog_var)::in,
	hlds_goal_info::out) is det.

:- pred goal_info_set_follow_vars(hlds_goal_info::in, maybe(follow_vars)::in,
	hlds_goal_info::out) is det.

:- pred goal_info_set_store_map(hlds_goal_info::in, store_map::in,
	hlds_goal_info::out) is det.

:- pred goal_info_set_resume_point(hlds_goal_info::in, resume_point::in,
	hlds_goal_info::out) is det.

:- pred goal_info_set_need_across_call(hlds_goal_info::in,
	need_across_call::in, hlds_goal_info::out) is det.

:- pred goal_info_set_need_in_resume(hlds_goal_info::in,
	need_in_resume::in, hlds_goal_info::out) is det.

:- pred goal_info_set_need_in_par_conj(hlds_goal_info::in,
	need_in_par_conj::in, hlds_goal_info::out) is det.

%-----------------------------------------------------------------------------%

:- pred goal_set_follow_vars(hlds_goal::in, maybe(follow_vars)::in,
	hlds_goal::out) is det.

:- pred goal_set_resume_point(hlds_goal::in, resume_point::in,
	hlds_goal::out) is det.

:- pred goal_info_resume_vars_and_loc(resume_point::in,
	set(prog_var)::out, resume_locs::out) is det.

%-----------------------------------------------------------------------------%

	% rename_vars_in_llds_code_gen_info(Details0, Must, Subn, Details):
	% Renames all the variables in Details0 according to the substitution
	% Subn, yielding Details. If Must is yes, then require every variable
	% in Details0 to be in the domain of Subn. If Must is no, then leave
	% variables not in the domain of Subn unchanged.

:- pred rename_vars_in_llds_code_gen_info(llds_code_gen_details::in,
	bool::in, map(prog_var, prog_var)::in, llds_code_gen_details::out)
	is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__goal_util.
:- import_module list, assoc_list, require.

	% For the meaning of this type, see the documentation of the
	% maybe_need field of llds_code_gen_details below.
:- type maybe_need
	--->	no_need
	;	need_call(need_across_call)
	;	need_resume(need_in_resume)
	;	need_par_conj(need_in_par_conj).

:- type llds_code_gen_details --->
	llds_code_gen_details(
		pre_births		:: set(prog_var),
		post_births		:: set(prog_var),
		pre_deaths		:: set(prog_var),
		post_deaths		:: set(prog_var),
			% All four of these fields are computed by liveness.m.
			% For atomic goals, the post-deadness should be applied
			% _before_ the goal.

		follow_vars		:: maybe(follow_vars),
			% Initially set to `no' for all goals, which
			% means the absence of the advisory
			% information. Can be set to `yes' by the
			% follow_vars pass, if it is invoked. Can be
			% set to `yes' for any kind of goal.
			%
			% For the semantics of the value inside a `yes',
			% see the documentation of the follow_vars type.

		store_map		:: store_map,
			% This annotation is meaningful only after the
			% store_alloc pass, and even then only if
			% attached to a goal representing a branched
			% control structure, i.e. an if_then_else,
			% switch or disj goal. For those goals, the map
			% will have an entry for every variable that is
			% forward live after the goal, and will map
			% each of those variables to the location where
			% all the branches will leave the value of the
			% variable. The code after the branched goal
			% can therefore pick it up from there.
			%
			% This field should contain an empty map if its
			% contents are not meaningful.

		resume_point		:: resume_point,
			% If this goal establishes a resumption point,
			% i.e. it is the second or later disjunct of a
			% disjunction or if it is the condition of an
			% if-then-else, this field will state what variables
			% need to be saved for that resumption point, and
			% which entry labels of the resumption point will be
			% needed. (See compiler/notes/allocation.html)
			%
			% This field is filled in during the liveness
			% pass. Before then, and after then if the goal
			% does not establish a resumption point, it
			% should contain no_resume_point.

		maybe_need		:: maybe_need
			% This field is filled in during the stackvars pass.
			% It is not meaningful before then, and should
			% contain `no_need'.
			% 
			% For calls, generic calls, and for foreign_proc
			% goals that may call back to Mercury, the stackvars
			% pass will set this argument to need_call(NC), where
			% NC specifies what variables need to be stored on the
			% stack across the call.
			% 
			% For disjunctions, if-then-elses and negations,
			% the stackvars pass will set this argument to
			% need_resume(NR), where NR specifies what variables
			% need to be stored on the stack at the resumption
			% point established by the goal.
			% 
			% For parallel conjunctions, the stackvars pass will
			% set this argument to need_par_conj(NPC), where NPC
			% specifies what variables are required to be stored
			% on the stack by the parallel conjunction execution
			% mechanism.
	).

%-----------------------------------------------------------------------------%

goal_info_get_pre_births(GoalInfo, PreBirths) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	( PreBirthsPrime = CodeGenInfo ^ llds_code_gen ^ pre_births ->
		PreBirths = PreBirthsPrime
	;
		error("goal_info_get_pre_births: no code_gen_info")
	).

goal_info_get_post_births(GoalInfo, PostBirths) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	( PostBirthsPrime = CodeGenInfo ^ llds_code_gen ^ post_births ->
		PostBirths = PostBirthsPrime
	;
		error("goal_info_get_post_births: no code_gen_info")
	).

goal_info_get_pre_deaths(GoalInfo, PreDeaths) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	( PreDeathsPrime = CodeGenInfo ^ llds_code_gen ^ pre_deaths ->
		PreDeaths = PreDeathsPrime
	;
		error("goal_info_get_pre_deaths: no code_gen_info")
	).

goal_info_get_post_deaths(GoalInfo, PostDeaths) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	( PostDeathsPrime = CodeGenInfo ^ llds_code_gen ^ post_deaths ->
		PostDeaths = PostDeathsPrime
	;
		error("goal_info_get_post_deaths: no code_gen_info")
	).

goal_info_get_follow_vars(GoalInfo, FollowVars) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	( FollowVarsPrime = CodeGenInfo ^ llds_code_gen ^ follow_vars ->
		FollowVars = FollowVarsPrime
	;
		error("goal_info_get_follow_vars: no code_gen_info")
	).

goal_info_get_store_map(GoalInfo, StoreMap) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	( StoreMapPrime = CodeGenInfo ^ llds_code_gen ^ store_map ->
		StoreMap = StoreMapPrime
	;
		error("goal_info_get_store_map: no code_gen_info")
	).

goal_info_get_resume_point(GoalInfo, ResumePoint) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	( ResumePointPrime = CodeGenInfo ^ llds_code_gen ^ resume_point ->
		ResumePoint = ResumePointPrime
	;
		error("goal_info_get_resume_point: no code_gen_info")
	).

goal_info_get_maybe_need_across_call(GoalInfo, MaybeNeedAtCall) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	( MaybeNeed = CodeGenInfo ^ llds_code_gen ^ maybe_need ->
		( MaybeNeed = need_call(NeedAtCall) ->
			MaybeNeedAtCall = yes(NeedAtCall)
		;
			MaybeNeedAtCall = no
		)
	;
		error("goal_info_get_need_at_call: no code_gen_info")
	).

goal_info_get_maybe_need_in_resume(GoalInfo, MaybeNeedInResume) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	( MaybeNeed = CodeGenInfo ^ llds_code_gen ^ maybe_need ->
		( MaybeNeed = need_resume(NeedInResume) ->
			MaybeNeedInResume = yes(NeedInResume)
		;
			MaybeNeedInResume = no
		)
	;
		error("goal_info_get_need_in_resume: no code_gen_info")
	).

goal_info_get_maybe_need_in_par_conj(GoalInfo, MaybeNeedInParConj) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	( MaybeNeed = CodeGenInfo ^ llds_code_gen ^ maybe_need ->
		( MaybeNeed = need_par_conj(NeedInParConj) ->
			MaybeNeedInParConj = yes(NeedInParConj)
		;
			MaybeNeedInParConj = no
		)
	;
		error("goal_info_get_need_in_par_conj: no code_gen_info")
	).

%-----------------------------------------------------------------------------%

goal_info_maybe_get_pre_births(GoalInfo, PreBirths) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	PreBirths = CodeGenInfo ^ llds_code_gen ^ pre_births.

goal_info_maybe_get_post_births(GoalInfo, PostBirths) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	PostBirths = CodeGenInfo ^ llds_code_gen ^ post_births.

goal_info_maybe_get_pre_deaths(GoalInfo, PreDeaths) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	PreDeaths = CodeGenInfo ^ llds_code_gen ^ pre_deaths.

goal_info_maybe_get_post_deaths(GoalInfo, PostDeaths) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	PostDeaths = CodeGenInfo ^ llds_code_gen ^ post_deaths.

goal_info_maybe_get_follow_vars(GoalInfo, FollowVars) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	FollowVars = CodeGenInfo ^ llds_code_gen ^ follow_vars.

goal_info_maybe_get_store_map(GoalInfo, StoreMap) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	StoreMap = CodeGenInfo ^ llds_code_gen ^ store_map.

goal_info_maybe_get_resume_point(GoalInfo, ResumePoint) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	ResumePoint = CodeGenInfo ^ llds_code_gen ^ resume_point.

goal_info_maybe_get_maybe_need_across_call(GoalInfo, MaybeNeedAcrossCall) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	MaybeNeed = CodeGenInfo ^ llds_code_gen ^ maybe_need,
	MaybeNeed = need_call(NeedAcrossCall),
	MaybeNeedAcrossCall = yes(NeedAcrossCall).

goal_info_maybe_get_maybe_need_in_resume(GoalInfo, MaybeNeedInResume) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	MaybeNeed = CodeGenInfo ^ llds_code_gen ^ maybe_need,
	MaybeNeed = need_resume(NeedInResume),
	MaybeNeedInResume = yes(NeedInResume).

goal_info_maybe_get_maybe_need_in_par_conj(GoalInfo, MaybeNeedInParConj) :-
	goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
	MaybeNeed = CodeGenInfo ^ llds_code_gen ^ maybe_need,
	MaybeNeed = need_par_conj(NeedInParConj),
	MaybeNeedInParConj = yes(NeedInParConj).

%-----------------------------------------------------------------------------%

goal_info_initialize_liveness_info(GoalInfo0, PreBirths, PostBirths,
		PreDeaths, PostDeaths, ResumePoint, GoalInfo) :-
	goal_info_get_code_gen_info(GoalInfo0, CodeGenInfo0),
	LLDSInfo0 = get_details(CodeGenInfo0),
	LLDSInfo = (((((LLDSInfo0
		^ pre_births := PreBirths)
		^ post_births := PostBirths)
		^ pre_deaths := PreDeaths)
		^ post_deaths := PostDeaths)
		^ resume_point := ResumePoint),
	CodeGenInfo = llds_code_gen_info(LLDSInfo),
	goal_info_set_code_gen_info(GoalInfo0, CodeGenInfo, GoalInfo).

goal_info_set_pre_births(GoalInfo0, PreBirths, GoalInfo) :-
	goal_info_get_code_gen_info(GoalInfo0, CodeGenInfo0),
	LLDSInfo0 = get_details(CodeGenInfo0),
	LLDSInfo = LLDSInfo0 ^ pre_births := PreBirths,
	CodeGenInfo = llds_code_gen_info(LLDSInfo),
	goal_info_set_code_gen_info(GoalInfo0, CodeGenInfo, GoalInfo).

goal_info_set_post_births(GoalInfo0, PostBirths, GoalInfo) :-
	goal_info_get_code_gen_info(GoalInfo0, CodeGenInfo0),
	LLDSInfo0 = get_details(CodeGenInfo0),
	LLDSInfo = LLDSInfo0 ^ post_births := PostBirths,
	CodeGenInfo = llds_code_gen_info(LLDSInfo),
	goal_info_set_code_gen_info(GoalInfo0, CodeGenInfo, GoalInfo).

goal_info_set_pre_deaths(GoalInfo0, PreDeaths, GoalInfo) :-
	goal_info_get_code_gen_info(GoalInfo0, CodeGenInfo0),
	LLDSInfo0 = get_details(CodeGenInfo0),
	LLDSInfo = LLDSInfo0 ^ pre_deaths := PreDeaths,
	CodeGenInfo = llds_code_gen_info(LLDSInfo),
	goal_info_set_code_gen_info(GoalInfo0, CodeGenInfo, GoalInfo).

goal_info_set_post_deaths(GoalInfo0, PostDeaths, GoalInfo) :-
	goal_info_get_code_gen_info(GoalInfo0, CodeGenInfo0),
	LLDSInfo0 = get_details(CodeGenInfo0),
	LLDSInfo = LLDSInfo0 ^ post_deaths := PostDeaths,
	CodeGenInfo = llds_code_gen_info(LLDSInfo),
	goal_info_set_code_gen_info(GoalInfo0, CodeGenInfo, GoalInfo).

goal_info_set_follow_vars(GoalInfo0, FollowVars, GoalInfo) :-
	goal_info_get_code_gen_info(GoalInfo0, CodeGenInfo0),
	LLDSInfo0 = get_details(CodeGenInfo0),
	LLDSInfo = LLDSInfo0 ^ follow_vars := FollowVars,
	CodeGenInfo = llds_code_gen_info(LLDSInfo),
	goal_info_set_code_gen_info(GoalInfo0, CodeGenInfo, GoalInfo).

goal_info_set_store_map(GoalInfo0, StoreMap, GoalInfo) :-
	goal_info_get_code_gen_info(GoalInfo0, CodeGenInfo0),
	LLDSInfo0 = get_details(CodeGenInfo0),
	LLDSInfo = LLDSInfo0 ^ store_map := StoreMap,
	CodeGenInfo = llds_code_gen_info(LLDSInfo),
	goal_info_set_code_gen_info(GoalInfo0, CodeGenInfo, GoalInfo).

goal_info_set_resume_point(GoalInfo0, ResumePoint, GoalInfo) :-
	goal_info_get_code_gen_info(GoalInfo0, CodeGenInfo0),
	LLDSInfo0 = get_details(CodeGenInfo0),
	LLDSInfo = LLDSInfo0 ^ resume_point := ResumePoint,
	CodeGenInfo = llds_code_gen_info(LLDSInfo),
	goal_info_set_code_gen_info(GoalInfo0, CodeGenInfo, GoalInfo).

goal_info_set_need_across_call(GoalInfo0, NeedAcrossCall, GoalInfo) :-
	goal_info_get_code_gen_info(GoalInfo0, CodeGenInfo0),
	LLDSInfo0 = get_details(CodeGenInfo0),
	LLDSInfo = LLDSInfo0 ^ maybe_need := need_call(NeedAcrossCall),
	CodeGenInfo = llds_code_gen_info(LLDSInfo),
	goal_info_set_code_gen_info(GoalInfo0, CodeGenInfo, GoalInfo).

goal_info_set_need_in_resume(GoalInfo0, NeedInResume, GoalInfo) :-
	goal_info_get_code_gen_info(GoalInfo0, CodeGenInfo0),
	LLDSInfo0 = get_details(CodeGenInfo0),
	LLDSInfo = LLDSInfo0 ^ maybe_need := need_resume(NeedInResume),
	CodeGenInfo = llds_code_gen_info(LLDSInfo),
	goal_info_set_code_gen_info(GoalInfo0, CodeGenInfo, GoalInfo).

goal_info_set_need_in_par_conj(GoalInfo0, NeedInParConj, GoalInfo) :-
	goal_info_get_code_gen_info(GoalInfo0, CodeGenInfo0),
	LLDSInfo0 = get_details(CodeGenInfo0),
	LLDSInfo = LLDSInfo0 ^ maybe_need := need_par_conj(NeedInParConj),
	CodeGenInfo = llds_code_gen_info(LLDSInfo),
	goal_info_set_code_gen_info(GoalInfo0, CodeGenInfo, GoalInfo).

%-----------------------------------------------------------------------------%

goal_set_follow_vars(Goal - GoalInfo0, FollowVars, Goal - GoalInfo) :-
	goal_info_set_follow_vars(GoalInfo0, FollowVars, GoalInfo).

goal_set_resume_point(Goal - GoalInfo0, ResumePoint, Goal - GoalInfo) :-
	goal_info_set_resume_point(GoalInfo0, ResumePoint, GoalInfo).

goal_info_resume_vars_and_loc(Resume, Vars, Locs) :-
	(
		Resume = resume_point(Vars, Locs)
	;
		Resume = no_resume_point,
		error("goal_info__get_resume_vars_and_loc: no resume point")
	).

%-----------------------------------------------------------------------------%

:- func get_details(hlds_goal_code_gen_info) = llds_code_gen_details.

get_details(no_code_gen_info) = init_llds_code_gen_details.
get_details(llds_code_gen_info(LLDSInfo)) = LLDSInfo.

:- func init_llds_code_gen_details = llds_code_gen_details.

init_llds_code_gen_details =
	llds_code_gen_details(set__init, set__init, set__init, set__init,
	no, map__init, no_resume_point, no_need).

%-----------------------------------------------------------------------------%

rename_vars_in_llds_code_gen_info(Details0, Must, Subn, Details) :-
	Details0 = llds_code_gen_details(PreBirths0, PostBirths0,
		PreDeaths0, PostDeaths0, MaybeFollowVars0, StoreMap0,
		ResumePoint0, MaybeNeed0),
	rename_vars_in_var_set(PreBirths0, Must, Subn, PreBirths),
	rename_vars_in_var_set(PostBirths0, Must, Subn, PostBirths),
	rename_vars_in_var_set(PreDeaths0, Must, Subn, PreDeaths),
	rename_vars_in_var_set(PostDeaths0, Must, Subn, PostDeaths),
	(
		MaybeFollowVars0 = no,
		MaybeFollowVars = no
	;
		MaybeFollowVars0 = yes(FollowVars0),
		FollowVars0 = follow_vars(FollowVarsMap0, FirstFreeReg),
		rename_vars_in_var_lval_map(FollowVarsMap0, Must, Subn,
			FollowVarsMap),
		FollowVars = follow_vars(FollowVarsMap, FirstFreeReg),
		MaybeFollowVars = yes(FollowVars)
	),
	rename_vars_in_var_lval_map(StoreMap0, Must, Subn, StoreMap),
	(
		ResumePoint0 = no_resume_point,
		ResumePoint = no_resume_point
	;
		ResumePoint0 = resume_point(ResumePointVars0, ResumeLocs),
		rename_vars_in_var_set(ResumePointVars0, Must, Subn,
			ResumePointVars),
		ResumePoint = resume_point(ResumePointVars, ResumeLocs)
	),
	(
		MaybeNeed0 = no_need,
		MaybeNeed = no_need
	;
		MaybeNeed0 = need_call(NeedAcrossCall0),
		NeedAcrossCall0 = need_across_call(ForwardVars0,
			CallResumeVars0, CallNondetLiveVars0),
		rename_vars_in_var_set(ForwardVars0, Must, Subn,
			ForwardVars),
		rename_vars_in_var_set(CallResumeVars0, Must, Subn,
			CallResumeVars),
		rename_vars_in_var_set(CallNondetLiveVars0, Must, Subn,
			CallNondetLiveVars),
		NeedAcrossCall = need_across_call(ForwardVars,
			CallResumeVars, CallNondetLiveVars),
		MaybeNeed = need_call(NeedAcrossCall)
	;
		MaybeNeed0 = need_resume(NeedInResume0),
		NeedInResume0 = need_in_resume(OnStack, ResumeVars0,
			NondetLiveVars0),
		rename_vars_in_var_set(ResumeVars0, Must, Subn,
			ResumeVars),
		rename_vars_in_var_set(NondetLiveVars0, Must, Subn,
			NondetLiveVars),
		NeedInResume = need_in_resume(OnStack, ResumeVars,
			NondetLiveVars),
		MaybeNeed = need_resume(NeedInResume)
	;
		MaybeNeed0 = need_par_conj(NeedInParConj0),
		NeedInParConj0 = need_in_par_conj(ParConjVars0),
		rename_vars_in_var_set(ParConjVars0, Must, Subn, ParConjVars),
		NeedInParConj = need_in_par_conj(ParConjVars),
		MaybeNeed = need_par_conj(NeedInParConj)
	),
	Details = llds_code_gen_details(PreBirths, PostBirths,
		PreDeaths, PostDeaths, MaybeFollowVars, StoreMap,
		ResumePoint, MaybeNeed).

:- pred rename_vars_in_var_lval_map(map(prog_var, lval)::in,
	bool::in, map(prog_var, prog_var)::in, map(prog_var, lval)::out)
	is det.

rename_vars_in_var_lval_map(VarLvalMap0, Must, Subn, VarLvalMap) :-
	map__to_assoc_list(VarLvalMap0, VarLvalList0),
	rename_vars_in_var_lval_list(VarLvalList0, Must, Subn, VarLvalList),
	map__from_assoc_list(VarLvalList, VarLvalMap).

:- pred rename_vars_in_var_lval_list(assoc_list(prog_var, lval)::in,
	bool::in, map(prog_var, prog_var)::in, assoc_list(prog_var, lval)::out)
	is det.

rename_vars_in_var_lval_list([], _Must, _Subn, []).
rename_vars_in_var_lval_list([Var0 - Lval0 | VarLvals0], Must, Subn,
		[Var - Lval | VarLvals]) :-
	rename_var(Var0, Must, Subn, Var),
	rename_vars_in_lval(Lval0, Must, Subn, Lval),
	rename_vars_in_var_lval_list(VarLvals0, Must, Subn, VarLvals).

:- pred rename_vars_in_lval(lval::in, bool::in, map(prog_var, prog_var)::in,
	lval::out) is det.

rename_vars_in_lval(Lval0, _Must, _Subn, Lval) :-
	(
		( Lval0 = stackvar(_)
		; Lval0 = framevar(_)
		; Lval0 = reg(_, _)
		)
	->
		Lval = Lval0
	;
		error("rename_vars_in_lval: unexpected lval")
	).

%-----------------------------------------------------------------------------%
