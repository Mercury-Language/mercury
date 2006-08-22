%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: hlds_llds.m.
% Author: zs.
% 
% This module defines annotations on HLDS goals that are used by the LLDS
% back end.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_llds.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module set.

% The following types are annotations on the HLDS
% that are used only by the LLDS back-end.

:- type stack_slot
    --->    det_slot(int)
    ;       nondet_slot(int).

    % Maps variables to their stack slots.
    %
:- type stack_slots ==  map(prog_var, stack_slot).

:- type abs_locn
    --->    any_reg
    ;       abs_reg(int)
    ;       abs_stackvar(int)
    ;       abs_framevar(int).

:- type abs_follow_vars_map ==  map(prog_var, abs_locn).

:- type abs_follow_vars --->    abs_follow_vars(abs_follow_vars_map, int).
                                % Advisory information about where variables
                                % ought to be put next. Variables may or may
                                % not appear in the map. If they do, then the
                                % associated locn says where the value of that
                                % variable ought to be put when it is computed,
                                % or, if the locn is any_reg, it says that it
                                % should be put into any available register.
                                % The integer in the second half of the pair
                                % gives the number of the first register that
                                % is not reserved for other purposes, and is
                                % free to hold such variables.

:- type abs_store_map ==        map(prog_var, abs_locn).
                                % Authoritative information about where
                                % variables must be put at the ends of
                                % branches of branched control structures.
                                % However, between the follow_vars and
                                % and store_alloc passes, these fields
                                % temporarily hold follow_vars information.
                                % The final value is not allowed to map any
                                % variable to any_reg.

    % see compiler/notes/allocation.html for what these alternatives mean
:- type resume_point
    --->    resume_point(set(prog_var), resume_locs)
    ;       no_resume_point.

:- type resume_locs
    --->    resume_locs_orig_only
    ;       resume_locs_stack_only
    ;       resume_locs_orig_and_stack
    ;       resume_locs_stack_and_orig.

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
    --->    need_across_call(
                call_forward_vars   :: set(prog_var),
                call_resume_vars    :: set(prog_var),
                call_nondet_vars    :: set(prog_var)
            ).

    % resume_vars_on_stack is true if the resume point has a stack label.
    % resume_resume vars contains the set of variables that need to be
    % saved at a resume point because they are needed when execution
    % backtracks to this resume point, or to a resume point on its left.
    % resume_nondet_vars contains the set of variables that need to be on
    % the stack at a resume point because they are needed by a model_non
    % goal to the left of the resume point.

:- type need_in_resume
    --->    need_in_resume(
                resume_vars_on_stack    :: bool,
                resume_resume_vars  :: set(prog_var),
                resume_nondet_vars  :: set(prog_var)
            ).

    % par_conj_engine_vars gives the set of variables that the execution
    % mechanism of the parallel conjunction requires to be stored in stack
    % slots.
    %
:- type need_in_par_conj
    --->    need_in_par_conj(
                par_conj_engine_vars    :: set(prog_var)
            ).

:- type llds_code_gen_details.

%-----------------------------------------------------------------------------%

% Instead of recording the liveness of every variable at every part of the
% goal, we just keep track of the initial liveness and the changes in
% liveness.  Note that when traversing forwards through a goal, deaths must be
% applied before births; this is necessary to handle certain circumstances
% where a variable can occur in both the post-death and post-birth sets, or in
% both the pre-death and pre-birth sets.

:- pred goal_info_get_pre_births(hlds_goal_info::in,
    set(prog_var)::out) is det.

:- pred goal_info_get_post_births(hlds_goal_info::in,
    set(prog_var)::out) is det.

:- pred goal_info_get_pre_deaths(hlds_goal_info::in,
    set(prog_var)::out) is det.

:- pred goal_info_get_post_deaths(hlds_goal_info::in,
    set(prog_var)::out) is det.

:- pred goal_info_get_follow_vars(hlds_goal_info::in,
    maybe(abs_follow_vars)::out) is det.

:- pred goal_info_get_store_map(hlds_goal_info::in,
    abs_store_map::out) is det.

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
    maybe(abs_follow_vars)::out) is semidet.

:- pred goal_info_maybe_get_store_map(hlds_goal_info::in,
    abs_store_map::out) is semidet.

:- pred goal_info_maybe_get_resume_point(hlds_goal_info::in,
    resume_point::out) is semidet.

:- pred goal_info_maybe_get_maybe_need_across_call(hlds_goal_info::in,
    maybe(need_across_call)::out) is semidet.

:- pred goal_info_maybe_get_maybe_need_in_resume(hlds_goal_info::in,
    maybe(need_in_resume)::out) is semidet.

:- pred goal_info_maybe_get_maybe_need_in_par_conj(hlds_goal_info::in,
    maybe(need_in_par_conj)::out) is semidet.

%-----------------------------------------------------------------------------%

    % goal_info_initialize_liveness_info(PreBirths, PostBirths,
    %   PreDeaths, PostDeaths, ResumePoint, !GoalInfo):
    % Updates !GoalInfo by overwriting the previous values of its
    % pre_births, post_births, pre_deaths, post_deaths and resume_point
    % fields.
    %
:- pred goal_info_initialize_liveness_info(
    set(prog_var)::in, set(prog_var)::in, set(prog_var)::in, set(prog_var)::in,
    resume_point::in, hlds_goal_info::in, hlds_goal_info::out) is det.

:- pred goal_info_set_pre_births(set(prog_var)::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

:- pred goal_info_set_post_births(set(prog_var)::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

:- pred goal_info_set_pre_deaths(set(prog_var)::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

:- pred goal_info_set_post_deaths(set(prog_var)::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

:- pred goal_info_set_follow_vars(maybe(abs_follow_vars)::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

:- pred goal_info_set_store_map(abs_store_map::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

:- pred goal_info_set_resume_point(resume_point::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

:- pred goal_info_set_need_across_call(need_across_call::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

:- pred goal_info_set_need_in_resume(need_in_resume::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

:- pred goal_info_set_need_in_par_conj(need_in_par_conj::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

%-----------------------------------------------------------------------------%

:- pred goal_set_follow_vars(maybe(abs_follow_vars)::in,
    hlds_goal::in, hlds_goal::out) is det.

:- pred goal_set_resume_point(resume_point::in,
    hlds_goal::in, hlds_goal::out) is det.

:- pred goal_info_resume_vars_and_loc(resume_point::in,
    set(prog_var)::out, resume_locs::out) is det.

%-----------------------------------------------------------------------------%

    % rename_vars_in_llds_code_gen_info(Must, Subn, Details0, Details):
    % Renames all the variables in Details0 according to the substitution
    % Subn, yielding Details. If Must is yes, then require every variable
    % in Details0 to be in the domain of Subn. If Must is no, then leave
    % variables not in the domain of Subn unchanged.
    %
:- pred rename_vars_in_llds_code_gen_info(bool::in,
    map(prog_var, prog_var)::in,
    llds_code_gen_details::in, llds_code_gen_details::out) is det.

%-----------------------------------------------------------------------------%

:- func stack_slot_num(stack_slot) = int.

:- func stack_slot_to_abs_locn(stack_slot) = abs_locn.
:- func key_stack_slot_to_abs_locn(_, stack_slot) = abs_locn.

:- func abs_locn_to_string(abs_locn) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module libs.compiler_util.

:- import_module assoc_list.
:- import_module list.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

    % For the meaning of this type, see the documentation of the
    % maybe_need field of llds_code_gen_details below.
    %
:- type maybe_need
    --->    no_need
    ;       need_call(need_across_call)
    ;       need_resume(need_in_resume)
    ;       need_par_conj(need_in_par_conj).

:- type llds_code_gen_details
    --->    llds_code_gen_details(
                % All four of these fields are computed by liveness.m.
                % For atomic goals, the post-deadness should be applied
                % _before_ the goal.
                pre_births          :: set(prog_var),
                post_births         :: set(prog_var),
                pre_deaths          :: set(prog_var),
                post_deaths         :: set(prog_var),

                % Initially set to `no' for all goals, which means the absence
                % of the advisory information. Can be set to `yes' by the
                % follow_vars pass, if it is invoked. Can be set to `yes'
                % for any kind of goal.
                %
                % For the semantics of the value inside a `yes',
                % see the documentation of the follow_vars type.
                follow_vars         :: maybe(abs_follow_vars),

                % This annotation is meaningful only after the store_alloc
                % pass, and even then only if attached to a goal representing
                % a branched control structure, i.e. an if_then_else, switch
                % or disj goal. For those goals, the map will have an entry
                % for every variable that is forward live after the goal,
                % and will map each of those variables to the location where
                % all the branches will leave the value of the variable.
                % The code after the branched goal can therefore pick it
                % up from there.
                %
                % This field should contain an empty map if its contents
                % are not meaningful.
                store_map           :: abs_store_map,

                % If this goal establishes a resumption point, i.e. it is
                % the second or later disjunct of a disjunction or if it is
                % the condition of an if-then-else, this field will state
                % what variables need to be saved for that resumption point,
                % and which entry labels of the resumption point will be
                % needed. (See compiler/notes/allocation.html)
                %
                % This field is filled in during the liveness pass. Before
                % then, and after then if the goal does not establish a
                % resumption point, it should contain no_resume_point.
                resume_point        :: resume_point,

                % This field is filled in during the stackvars pass.
                % It is not meaningful before then, and should contain
                % `no_need'.
                %
                % For calls, generic calls, and for foreign_proc goals
                % that may call back to Mercury, the stackvars pass will set
                % this argument to need_call(NC), where NC specifies what
                % variables need to be stored on the stack across the call.
                %
                % For disjunctions, if-then-elses and negations, the stackvars
                % pass will set this argument to need_resume(NR), where NR
                % specifies what variables need to be stored on the stack
                % at the resumption point established by the goal.
                %
                % For parallel conjunctions, the stackvars pass will set
                % this argument to need_par_conj(NPC), where NPC specifies
                % what variables are required to be stored on the stack
                % by the parallel conjunction execution mechanism.
                maybe_need          :: maybe_need
            ).

%-----------------------------------------------------------------------------%

goal_info_get_pre_births(GoalInfo, PreBirths) :-
    goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
    ( PreBirthsPrime = CodeGenInfo ^ llds_code_gen ^ pre_births ->
        PreBirths = PreBirthsPrime
    ;
        unexpected(this_file, "goal_info_get_pre_births: no code_gen_info")
    ).

goal_info_get_post_births(GoalInfo, PostBirths) :-
    goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
    ( PostBirthsPrime = CodeGenInfo ^ llds_code_gen ^ post_births ->
        PostBirths = PostBirthsPrime
    ;
        unexpected(this_file, "goal_info_get_post_births: no code_gen_info")
    ).

goal_info_get_pre_deaths(GoalInfo, PreDeaths) :-
    goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
    ( PreDeathsPrime = CodeGenInfo ^ llds_code_gen ^ pre_deaths ->
        PreDeaths = PreDeathsPrime
    ;
        unexpected(this_file, "goal_info_get_pre_deaths: no code_gen_info")
    ).

goal_info_get_post_deaths(GoalInfo, PostDeaths) :-
    goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
    ( PostDeathsPrime = CodeGenInfo ^ llds_code_gen ^ post_deaths ->
        PostDeaths = PostDeathsPrime
    ;
        unexpected(this_file, "goal_info_get_post_deaths: no code_gen_info")
    ).

goal_info_get_follow_vars(GoalInfo, FollowVars) :-
    goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
    ( FollowVarsPrime = CodeGenInfo ^ llds_code_gen ^ follow_vars ->
        FollowVars = FollowVarsPrime
    ;
        unexpected(this_file, "goal_info_get_follow_vars: no code_gen_info")
    ).

goal_info_get_store_map(GoalInfo, StoreMap) :-
    goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
    ( StoreMapPrime = CodeGenInfo ^ llds_code_gen ^ store_map ->
        StoreMap = StoreMapPrime
    ;
        unexpected(this_file, "goal_info_get_store_map: no code_gen_info")
    ).

goal_info_get_resume_point(GoalInfo, ResumePoint) :-
    goal_info_get_code_gen_info(GoalInfo, CodeGenInfo),
    ( ResumePointPrime = CodeGenInfo ^ llds_code_gen ^ resume_point ->
        ResumePoint = ResumePointPrime
    ;
        unexpected(this_file, "goal_info_get_resume_point: no code_gen_info")
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
        unexpected(this_file, "goal_info_get_need_at_call: no code_gen_info")
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
        unexpected(this_file, "goal_info_get_need_in_resume: no code_gen_info")
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
        unexpected(this_file,
            "goal_info_get_need_in_par_conj: no code_gen_info")
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

goal_info_initialize_liveness_info(PreBirths, PostBirths,
        PreDeaths, PostDeaths, ResumePoint, !GoalInfo) :-
    goal_info_get_code_gen_info(!.GoalInfo, CodeGenInfo0),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = (((((LLDSInfo0
        ^ pre_births := PreBirths)
        ^ post_births := PostBirths)
        ^ pre_deaths := PreDeaths)
        ^ post_deaths := PostDeaths)
        ^ resume_point := ResumePoint),
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_pre_births(PreBirths, !GoalInfo) :-
    goal_info_get_code_gen_info(!.GoalInfo, CodeGenInfo0),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ pre_births := PreBirths,
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_post_births(PostBirths, !GoalInfo) :-
    goal_info_get_code_gen_info(!.GoalInfo, CodeGenInfo0),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ post_births := PostBirths,
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_pre_deaths(PreDeaths, !GoalInfo) :-
    goal_info_get_code_gen_info(!.GoalInfo, CodeGenInfo0),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ pre_deaths := PreDeaths,
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_post_deaths(PostDeaths, !GoalInfo) :-
    goal_info_get_code_gen_info(!.GoalInfo, CodeGenInfo0),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ post_deaths := PostDeaths,
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_follow_vars(FollowVars, !GoalInfo) :-
    goal_info_get_code_gen_info(!.GoalInfo, CodeGenInfo0),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ follow_vars := FollowVars,
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_store_map(StoreMap, !GoalInfo) :-
    goal_info_get_code_gen_info(!.GoalInfo, CodeGenInfo0),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ store_map := StoreMap,
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_resume_point(ResumePoint, !GoalInfo) :-
    goal_info_get_code_gen_info(!.GoalInfo, CodeGenInfo0),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ resume_point := ResumePoint,
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_need_across_call(NeedAcrossCall, !GoalInfo) :-
    goal_info_get_code_gen_info(!.GoalInfo, CodeGenInfo0),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ maybe_need := need_call(NeedAcrossCall),
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_need_in_resume(NeedInResume, !GoalInfo) :-
    goal_info_get_code_gen_info(!.GoalInfo, CodeGenInfo0),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ maybe_need := need_resume(NeedInResume),
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_need_in_par_conj(NeedInParConj, !GoalInfo) :-
    goal_info_get_code_gen_info(!.GoalInfo, CodeGenInfo0),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ maybe_need := need_par_conj(NeedInParConj),
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

%-----------------------------------------------------------------------------%

goal_set_follow_vars(FollowVars, Goal - GoalInfo0, Goal - GoalInfo) :-
    goal_info_set_follow_vars(FollowVars, GoalInfo0, GoalInfo).

goal_set_resume_point(ResumePoint, Goal - GoalInfo0, Goal - GoalInfo) :-
    goal_info_set_resume_point(ResumePoint, GoalInfo0, GoalInfo).

goal_info_resume_vars_and_loc(Resume, Vars, Locs) :-
    (
        Resume = resume_point(Vars, Locs)
    ;
        Resume = no_resume_point,
        unexpected(this_file,
            "goal_info_resume_vars_and_loc: no resume point")
    ).

%-----------------------------------------------------------------------------%

:- func get_details(hlds_goal_code_gen_info) = llds_code_gen_details.

get_details(no_code_gen_info) = init_llds_code_gen_details.
get_details(llds_code_gen_info(LLDSInfo)) = LLDSInfo.

:- func init_llds_code_gen_details = llds_code_gen_details.

init_llds_code_gen_details =
    llds_code_gen_details(set.init, set.init, set.init, set.init,
    no, map.init, no_resume_point, no_need).

%-----------------------------------------------------------------------------%

rename_vars_in_llds_code_gen_info(Must, Subn, Details0, Details) :-
    Details0 = llds_code_gen_details(PreBirths0, PostBirths0,
        PreDeaths0, PostDeaths0, MaybeFollowVars0, StoreMap0,
        ResumePoint0, MaybeNeed0),
    rename_vars_in_var_set(Must, Subn, PreBirths0, PreBirths),
    rename_vars_in_var_set(Must, Subn, PostBirths0, PostBirths),
    rename_vars_in_var_set(Must, Subn, PreDeaths0, PreDeaths),
    rename_vars_in_var_set(Must, Subn, PostDeaths0, PostDeaths),
    (
        MaybeFollowVars0 = no,
        MaybeFollowVars = no
    ;
        MaybeFollowVars0 = yes(FollowVars0),
        FollowVars0 = abs_follow_vars(FollowVarsMap0, FirstFreeReg),
        rename_vars_in_var_locn_map(Must, Subn,
            FollowVarsMap0, FollowVarsMap),
        FollowVars = abs_follow_vars(FollowVarsMap, FirstFreeReg),
        MaybeFollowVars = yes(FollowVars)
    ),
    rename_vars_in_var_locn_map(Must, Subn, StoreMap0, StoreMap),
    (
        ResumePoint0 = no_resume_point,
        ResumePoint = no_resume_point
    ;
        ResumePoint0 = resume_point(ResumePointVars0, ResumeLocs),
        rename_vars_in_var_set(Must, Subn, ResumePointVars0, ResumePointVars),
        ResumePoint = resume_point(ResumePointVars, ResumeLocs)
    ),
    (
        MaybeNeed0 = no_need,
        MaybeNeed = no_need
    ;
        MaybeNeed0 = need_call(NeedAcrossCall0),
        NeedAcrossCall0 = need_across_call(ForwardVars0,
            CallResumeVars0, CallNondetLiveVars0),
        rename_vars_in_var_set(Must, Subn, ForwardVars0, ForwardVars),
        rename_vars_in_var_set(Must, Subn, CallResumeVars0, CallResumeVars),
        rename_vars_in_var_set(Must, Subn,
            CallNondetLiveVars0, CallNondetLiveVars),
        NeedAcrossCall = need_across_call(ForwardVars,
            CallResumeVars, CallNondetLiveVars),
        MaybeNeed = need_call(NeedAcrossCall)
    ;
        MaybeNeed0 = need_resume(NeedInResume0),
        NeedInResume0 = need_in_resume(OnStack, ResumeVars0, NondetLiveVars0),
        rename_vars_in_var_set(Must, Subn, ResumeVars0, ResumeVars),
        rename_vars_in_var_set(Must, Subn, NondetLiveVars0, NondetLiveVars),
        NeedInResume = need_in_resume(OnStack, ResumeVars, NondetLiveVars),
        MaybeNeed = need_resume(NeedInResume)
    ;
        MaybeNeed0 = need_par_conj(NeedInParConj0),
        NeedInParConj0 = need_in_par_conj(ParConjVars0),
        rename_vars_in_var_set(Must, Subn, ParConjVars0, ParConjVars),
        NeedInParConj = need_in_par_conj(ParConjVars),
        MaybeNeed = need_par_conj(NeedInParConj)
    ),
    Details = llds_code_gen_details(PreBirths, PostBirths,
        PreDeaths, PostDeaths, MaybeFollowVars, StoreMap,
        ResumePoint, MaybeNeed).

:- pred rename_vars_in_var_locn_map(bool::in, map(prog_var, prog_var)::in,
    map(prog_var, abs_locn)::in, map(prog_var, abs_locn)::out) is det.

rename_vars_in_var_locn_map(Must, Subn, VarLocnMap0, VarLocnMap) :-
    map.to_assoc_list(VarLocnMap0, VarLocnList0),
    rename_vars_in_var_locn_list(Must, Subn, VarLocnList0, VarLocnList),
    map.from_assoc_list(VarLocnList, VarLocnMap).

:- pred rename_vars_in_var_locn_list(bool::in, map(prog_var, prog_var)::in,
    assoc_list(prog_var, abs_locn)::in,
    assoc_list(prog_var, abs_locn)::out) is det.

rename_vars_in_var_locn_list(_Must, _Subn, [], []).
rename_vars_in_var_locn_list(Must, Subn,
        [Var0 - Locn | VarLocns0], [Var - Locn | VarLocns]) :-
    rename_var(Must, Subn, Var0, Var),
    rename_vars_in_var_locn_list(Must, Subn, VarLocns0, VarLocns).

%-----------------------------------------------------------------------------%

stack_slot_num(det_slot(N)) = N.
stack_slot_num(nondet_slot(N)) = N.

stack_slot_to_abs_locn(det_slot(N)) = abs_stackvar(N).
stack_slot_to_abs_locn(nondet_slot(N)) = abs_framevar(N).

key_stack_slot_to_abs_locn(_, Slot) =
    stack_slot_to_abs_locn(Slot).

abs_locn_to_string(any_reg) = "any_reg".
abs_locn_to_string(abs_reg(N)) = "r" ++ int_to_string(N).
abs_locn_to_string(abs_stackvar(N)) = "stackvar" ++ int_to_string(N).
abs_locn_to_string(abs_framevar(N)) = "framevar" ++ int_to_string(N).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "hlds_llds.m".

%-----------------------------------------------------------------------------%
