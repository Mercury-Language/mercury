%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2007, 2009-2011 The University of Melbourne.
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
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.

    % reg_r are the general purpose registers.
    %
    % reg_f are float registers.
    % They are only present when float is wider than a word.
    %
:- type reg_type
    --->    reg_r
    ;       reg_f.

:- type stack_slot
    --->    det_slot(int, stack_slot_width)
    ;       parent_det_slot(int, stack_slot_width)
    ;       nondet_slot(int).

:- type stack_slot_width
    --->    single_width
    ;       double_width.   % occupies slots N and N+1

    % Maps variables to their stack slots.
    %
:- type stack_slots == map(prog_var, stack_slot).

:- func explain_stack_slots(stack_slots, prog_varset) = string.

:- type abs_locn
    --->    any_reg
    ;       abs_reg(reg_type, int)
    ;       abs_stackvar(int, stack_slot_width)
    ;       abs_parent_stackvar(int, stack_slot_width)
    ;       abs_framevar(int).

:- type abs_follow_vars_map ==  map(prog_var, abs_locn).

    % Advisory information about where variables ought to be put next.
    % Variables may or may not appear in the map. If they do, then the
    % associated locn says where the value of that variable ought to be put
    % when it is computed, or, if the locn is any_reg, it says that it
    % should be put into any available register. The two integers give the
    % first regular and float registers, respectively, that are not reserved
    % for other purposes, and so are free to hold such variables.
    %
:- type abs_follow_vars
    --->    abs_follow_vars(
                afv_map                 :: abs_follow_vars_map,
                afv_next_non_res_reg_r  :: int,
                afv_next_non_res_reg_f  :: int
            ).

    % Authoritative information about where variables must be put
    % at the ends of branches of branched control structures.
    % However, between the follow_vars and store_alloc passes,
    % these fields temporarily hold follow_vars information.
    % The final value is not allowed to map any variable to any_reg.
    %
:- type abs_store_map == map(prog_var, abs_locn).

    % see compiler/notes/allocation.html for what these alternatives mean
:- type resume_point
    --->    resume_point(set_of_progvar, resume_locs)
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
                call_forward_vars       :: set_of_progvar,
                call_resume_vars        :: set_of_progvar,
                call_nondet_vars        :: set_of_progvar
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
                resume_resume_vars      :: set_of_progvar,
                resume_nondet_vars      :: set_of_progvar
            ).

    % par_conj_engine_vars gives the set of variables that the execution
    % mechanism of the parallel conjunction requires to be stored in stack
    % slots.
    %
:- type need_in_par_conj
    --->    need_in_par_conj(
                par_conj_engine_vars    :: set_of_progvar
            ).

    % loop_control_distinct_stackvars gives sets of variables that must not
    % have overlapping stack slot allocations so that concurrent access to the
    % same stack frame is safe.
    %
:- type need_for_loop_control
    --->    need_for_loop_control(
                loop_control_distinct_stackvars :: list(set_of_progvar)
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
    set_of_progvar::out) is det.

:- pred goal_info_get_post_births(hlds_goal_info::in,
    set_of_progvar::out) is det.

:- pred goal_info_get_pre_deaths(hlds_goal_info::in,
    set_of_progvar::out) is det.

:- pred goal_info_get_post_deaths(hlds_goal_info::in,
    set_of_progvar::out) is det.

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
    set_of_progvar::out) is semidet.

:- pred goal_info_maybe_get_post_births(hlds_goal_info::in,
    set_of_progvar::out) is semidet.

:- pred goal_info_maybe_get_pre_deaths(hlds_goal_info::in,
    set_of_progvar::out) is semidet.

:- pred goal_info_maybe_get_post_deaths(hlds_goal_info::in,
    set_of_progvar::out) is semidet.

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
    % pre_births, post_births, pre_deaths, post_deaths and resume_point fields.
    %
:- pred goal_info_initialize_liveness_info(
    set_of_progvar::in, set_of_progvar::in,
    set_of_progvar::in, set_of_progvar::in,
    resume_point::in, hlds_goal_info::in, hlds_goal_info::out) is det.

:- pred goal_info_set_pre_births(set_of_progvar::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

:- pred goal_info_set_post_births(set_of_progvar::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

:- pred goal_info_set_pre_deaths(set_of_progvar::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

:- pred goal_info_set_post_deaths(set_of_progvar::in,
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
    set_of_progvar::out, resume_locs::out) is det.

%-----------------------------------------------------------------------------%

    % rename_vars_in_llds_code_gen_info(Must, Rename, Details0, Details):
    %
    % Rename all the variables in Details0 according to the substitution
    % Rename, yielding Details. If Must is must_rename, then require every
    % variable in Details0 to be in the domain of Rename. If Must is
    % need_not_rename, then leave variables not in the domain of Rename
    % unchanged.
    %
:- pred rename_vars_in_llds_code_gen_info(must_rename::in,
    map(prog_var, prog_var)::in,
    llds_code_gen_details::in, llds_code_gen_details::out) is det.

%-----------------------------------------------------------------------------%

:- func stack_slot_num(stack_slot) = int.

:- func stack_slot_to_abs_locn(stack_slot) = abs_locn.
:- func key_stack_slot_to_abs_locn(_, stack_slot) = abs_locn.

:- pred abs_locn_to_string(abs_locn::in, string::out, maybe(string)::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

explain_stack_slots(StackSlots, VarSet) = Explanation :-
    map.to_assoc_list(StackSlots, StackSlotsList),
    explain_stack_slots_2(StackSlotsList, VarSet, "", Explanation1),
    Explanation = "\nStack slot assignments (if any):\n" ++ Explanation1.

:- pred explain_stack_slots_2(assoc_list(prog_var, stack_slot)::in,
    prog_varset::in, string::in, string::out) is det.

explain_stack_slots_2([], _, !Explanation).
explain_stack_slots_2([Var - Slot | Rest], VarSet, !Explanation) :-
    explain_stack_slots_2(Rest, VarSet, !Explanation),
    (
        Slot = det_slot(SlotNum, Width),
        StackStr = "sv"
    ;
        Slot = parent_det_slot(SlotNum, Width),
        StackStr = "parent_sv"
    ;
        Slot = nondet_slot(SlotNum),
        Width = single_width,
        StackStr = "fv"
    ),
    int_to_string(SlotNum, SlotStr),
    (
        Width = single_width,
        WidthStr = ""
    ;
        Width = double_width,
        WidthStr = " (double width)"
    ),
    varset.lookup_name(VarSet, Var, VarName),
    string.append_list([VarName, "\t ->\t", StackStr, SlotStr, WidthStr, "\n",
        !.Explanation], !:Explanation).

%----------------------------------------------------------------------------%

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
                pre_births          :: set_of_progvar,
                post_births         :: set_of_progvar,
                pre_deaths          :: set_of_progvar,
                post_deaths         :: set_of_progvar,

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
                % For parallel conjunctions and loop control scopes, the
                % stackvars pass will set this argument to need_par_conj(NPC),
                % where NPC specifies what variables are required to be stored
                % on the stack by the parallel conjunction and loop control
                % execution mechanisms.
                maybe_need          :: maybe_need
            ).

%-----------------------------------------------------------------------------%

goal_info_get_pre_births(GoalInfo, PreBirths) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    ( if PreBirthsPrime = CodeGenInfo ^ llds_code_gen ^ pre_births then
        PreBirths = PreBirthsPrime
    else
        unexpected($pred, "no code_gen_info")
    ).

goal_info_get_post_births(GoalInfo, PostBirths) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    ( if PostBirthsPrime = CodeGenInfo ^ llds_code_gen ^ post_births then
        PostBirths = PostBirthsPrime
    else
        unexpected($pred, "no code_gen_info")
    ).

goal_info_get_pre_deaths(GoalInfo, PreDeaths) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    ( if PreDeathsPrime = CodeGenInfo ^ llds_code_gen ^ pre_deaths then
        PreDeaths = PreDeathsPrime
    else
        unexpected($pred, "no code_gen_info")
    ).

goal_info_get_post_deaths(GoalInfo, PostDeaths) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    ( if PostDeathsPrime = CodeGenInfo ^ llds_code_gen ^ post_deaths then
        PostDeaths = PostDeathsPrime
    else
        unexpected($pred, "no code_gen_info")
    ).

goal_info_get_follow_vars(GoalInfo, FollowVars) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    ( if FollowVarsPrime = CodeGenInfo ^ llds_code_gen ^ follow_vars then
        FollowVars = FollowVarsPrime
    else
        unexpected($pred, "no code_gen_info")
    ).

goal_info_get_store_map(GoalInfo, StoreMap) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    ( if StoreMapPrime = CodeGenInfo ^ llds_code_gen ^ store_map then
        StoreMap = StoreMapPrime
    else
        unexpected($pred, "no code_gen_info")
    ).

goal_info_get_resume_point(GoalInfo, ResumePoint) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    ( if ResumePointPrime = CodeGenInfo ^ llds_code_gen ^ resume_point then
        ResumePoint = ResumePointPrime
    else
        unexpected($pred, "no code_gen_info")
    ).

goal_info_get_maybe_need_across_call(GoalInfo, MaybeNeedAtCall) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    ( if MaybeNeed = CodeGenInfo ^ llds_code_gen ^ maybe_need then
        (
            MaybeNeed = need_call(NeedAtCall),
            MaybeNeedAtCall = yes(NeedAtCall)
        ;
            ( MaybeNeed = need_resume(_)
            ; MaybeNeed = need_par_conj(_)
            ; MaybeNeed = no_need
            ),
            MaybeNeedAtCall = no
        )
    else
        unexpected($pred, "no code_gen_info")
    ).

goal_info_get_maybe_need_in_resume(GoalInfo, MaybeNeedInResume) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    ( if MaybeNeed = CodeGenInfo ^ llds_code_gen ^ maybe_need then
        (
            MaybeNeed = need_resume(NeedInResume),
            MaybeNeedInResume = yes(NeedInResume)
        ;
            ( MaybeNeed = need_call(_)
            ; MaybeNeed = need_par_conj(_)
            ; MaybeNeed = no_need
            ),
            MaybeNeedInResume = no
        )
    else
        unexpected($pred, "no code_gen_info")
    ).

goal_info_get_maybe_need_in_par_conj(GoalInfo, MaybeNeedInParConj) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    ( if MaybeNeed = CodeGenInfo ^ llds_code_gen ^ maybe_need then
        (
            MaybeNeed = need_par_conj(NeedInParConj),
            MaybeNeedInParConj = yes(NeedInParConj)
        ;
            ( MaybeNeed = need_call(_)
            ; MaybeNeed = need_resume(_)
            ; MaybeNeed = no_need
            ),
            MaybeNeedInParConj = no
        )
    else
        unexpected($pred, "no code_gen_info")
    ).

%-----------------------------------------------------------------------------%

goal_info_maybe_get_pre_births(GoalInfo, PreBirths) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    PreBirths = CodeGenInfo ^ llds_code_gen ^ pre_births.

goal_info_maybe_get_post_births(GoalInfo, PostBirths) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    PostBirths = CodeGenInfo ^ llds_code_gen ^ post_births.

goal_info_maybe_get_pre_deaths(GoalInfo, PreDeaths) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    PreDeaths = CodeGenInfo ^ llds_code_gen ^ pre_deaths.

goal_info_maybe_get_post_deaths(GoalInfo, PostDeaths) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    PostDeaths = CodeGenInfo ^ llds_code_gen ^ post_deaths.

goal_info_maybe_get_follow_vars(GoalInfo, FollowVars) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    FollowVars = CodeGenInfo ^ llds_code_gen ^ follow_vars.

goal_info_maybe_get_store_map(GoalInfo, StoreMap) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    StoreMap = CodeGenInfo ^ llds_code_gen ^ store_map.

goal_info_maybe_get_resume_point(GoalInfo, ResumePoint) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    ResumePoint = CodeGenInfo ^ llds_code_gen ^ resume_point.

goal_info_maybe_get_maybe_need_across_call(GoalInfo, MaybeNeedAcrossCall) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    MaybeNeed = CodeGenInfo ^ llds_code_gen ^ maybe_need,
    MaybeNeed = need_call(NeedAcrossCall),
    MaybeNeedAcrossCall = yes(NeedAcrossCall).

goal_info_maybe_get_maybe_need_in_resume(GoalInfo, MaybeNeedInResume) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    MaybeNeed = CodeGenInfo ^ llds_code_gen ^ maybe_need,
    MaybeNeed = need_resume(NeedInResume),
    MaybeNeedInResume = yes(NeedInResume).

goal_info_maybe_get_maybe_need_in_par_conj(GoalInfo, MaybeNeedInParConj) :-
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    MaybeNeed = CodeGenInfo ^ llds_code_gen ^ maybe_need,
    MaybeNeed = need_par_conj(NeedInParConj),
    MaybeNeedInParConj = yes(NeedInParConj).

%-----------------------------------------------------------------------------%

goal_info_initialize_liveness_info(PreBirths, PostBirths,
        PreDeaths, PostDeaths, ResumePoint, !GoalInfo) :-
    CodeGenInfo0 = goal_info_get_code_gen_info(!.GoalInfo),
    some [!LLDSInfo] (
        !:LLDSInfo = get_details(CodeGenInfo0),
        !LLDSInfo ^ pre_births := PreBirths,
        !LLDSInfo ^ post_births := PostBirths,
        !LLDSInfo ^ pre_deaths := PreDeaths,
        !LLDSInfo ^ post_deaths := PostDeaths,
        !LLDSInfo ^ resume_point := ResumePoint,
        CodeGenInfo = llds_code_gen_info(!.LLDSInfo)
    ),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_pre_births(PreBirths, !GoalInfo) :-
    CodeGenInfo0 = goal_info_get_code_gen_info(!.GoalInfo),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ pre_births := PreBirths,
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_post_births(PostBirths, !GoalInfo) :-
    CodeGenInfo0 = goal_info_get_code_gen_info(!.GoalInfo),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ post_births := PostBirths,
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_pre_deaths(PreDeaths, !GoalInfo) :-
    CodeGenInfo0 = goal_info_get_code_gen_info(!.GoalInfo),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ pre_deaths := PreDeaths,
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_post_deaths(PostDeaths, !GoalInfo) :-
    CodeGenInfo0 = goal_info_get_code_gen_info(!.GoalInfo),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ post_deaths := PostDeaths,
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_follow_vars(FollowVars, !GoalInfo) :-
    CodeGenInfo0 = goal_info_get_code_gen_info(!.GoalInfo),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ follow_vars := FollowVars,
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_store_map(StoreMap, !GoalInfo) :-
    CodeGenInfo0 = goal_info_get_code_gen_info(!.GoalInfo),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ store_map := StoreMap,
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_resume_point(ResumePoint, !GoalInfo) :-
    CodeGenInfo0 = goal_info_get_code_gen_info(!.GoalInfo),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ resume_point := ResumePoint,
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_need_across_call(NeedAcrossCall, !GoalInfo) :-
    CodeGenInfo0 = goal_info_get_code_gen_info(!.GoalInfo),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ maybe_need := need_call(NeedAcrossCall),
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_need_in_resume(NeedInResume, !GoalInfo) :-
    CodeGenInfo0 = goal_info_get_code_gen_info(!.GoalInfo),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ maybe_need := need_resume(NeedInResume),
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

goal_info_set_need_in_par_conj(NeedInParConj, !GoalInfo) :-
    CodeGenInfo0 = goal_info_get_code_gen_info(!.GoalInfo),
    LLDSInfo0 = get_details(CodeGenInfo0),
    LLDSInfo = LLDSInfo0 ^ maybe_need := need_par_conj(NeedInParConj),
    CodeGenInfo = llds_code_gen_info(LLDSInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

%-----------------------------------------------------------------------------%

goal_set_follow_vars(FollowVars, hlds_goal(GoalExpr, GoalInfo0),
        hlds_goal(GoalExpr, GoalInfo)) :-
    goal_info_set_follow_vars(FollowVars, GoalInfo0, GoalInfo).

goal_set_resume_point(ResumePoint, hlds_goal(GoalExpr, GoalInfo0),
        hlds_goal(GoalExpr, GoalInfo)) :-
    goal_info_set_resume_point(ResumePoint, GoalInfo0, GoalInfo).

goal_info_resume_vars_and_loc(Resume, Vars, Locs) :-
    (
        Resume = resume_point(Vars, Locs)
    ;
        Resume = no_resume_point,
        unexpected($pred, "no resume point")
    ).

%-----------------------------------------------------------------------------%

:- func get_details(hlds_goal_code_gen_info) = llds_code_gen_details.

get_details(no_code_gen_info) = init_llds_code_gen_details.
get_details(llds_code_gen_info(LLDSInfo)) = LLDSInfo.

:- func init_llds_code_gen_details = llds_code_gen_details.

init_llds_code_gen_details =
    llds_code_gen_details(set_of_var.init, set_of_var.init,
        set_of_var.init, set_of_var.init,
        no, map.init, no_resume_point, no_need).

%-----------------------------------------------------------------------------%

rename_vars_in_llds_code_gen_info(Must, Subn, Details0, Details) :-
    Details0 = llds_code_gen_details(PreBirths0, PostBirths0,
        PreDeaths0, PostDeaths0, MaybeFollowVars0, StoreMap0,
        ResumePoint0, MaybeNeed0),
    rename_vars_in_set_of_var(Must, Subn, PreBirths0, PreBirths),
    rename_vars_in_set_of_var(Must, Subn, PostBirths0, PostBirths),
    rename_vars_in_set_of_var(Must, Subn, PreDeaths0, PreDeaths),
    rename_vars_in_set_of_var(Must, Subn, PostDeaths0, PostDeaths),
    (
        MaybeFollowVars0 = no,
        MaybeFollowVars = no
    ;
        MaybeFollowVars0 = yes(FollowVars0),
        FollowVars0 = abs_follow_vars(FollowVarsMap0, FirstFreeRegR,
            FirstFreeRegF),
        rename_vars_in_var_locn_map(Must, Subn,
            FollowVarsMap0, FollowVarsMap),
        FollowVars = abs_follow_vars(FollowVarsMap, FirstFreeRegR,
            FirstFreeRegF),
        MaybeFollowVars = yes(FollowVars)
    ),
    rename_vars_in_var_locn_map(Must, Subn, StoreMap0, StoreMap),
    (
        ResumePoint0 = no_resume_point,
        ResumePoint = no_resume_point
    ;
        ResumePoint0 = resume_point(ResumePointVars0, ResumeLocs),
        rename_vars_in_set_of_var(Must, Subn,
            ResumePointVars0, ResumePointVars),
        ResumePoint = resume_point(ResumePointVars, ResumeLocs)
    ),
    (
        MaybeNeed0 = no_need,
        MaybeNeed = no_need
    ;
        MaybeNeed0 = need_call(NeedAcrossCall0),
        NeedAcrossCall0 = need_across_call(ForwardVars0,
            CallResumeVars0, CallNondetLiveVars0),
        rename_vars_in_set_of_var(Must, Subn, ForwardVars0, ForwardVars),
        rename_vars_in_set_of_var(Must, Subn, CallResumeVars0, CallResumeVars),
        rename_vars_in_set_of_var(Must, Subn,
            CallNondetLiveVars0, CallNondetLiveVars),
        NeedAcrossCall = need_across_call(ForwardVars,
            CallResumeVars, CallNondetLiveVars),
        MaybeNeed = need_call(NeedAcrossCall)
    ;
        MaybeNeed0 = need_resume(NeedInResume0),
        NeedInResume0 = need_in_resume(OnStack, ResumeVars0, NondetLiveVars0),
        rename_vars_in_set_of_var(Must, Subn, ResumeVars0, ResumeVars),
        rename_vars_in_set_of_var(Must, Subn, NondetLiveVars0, NondetLiveVars),
        NeedInResume = need_in_resume(OnStack, ResumeVars, NondetLiveVars),
        MaybeNeed = need_resume(NeedInResume)
    ;
        MaybeNeed0 = need_par_conj(NeedInParConj0),
        NeedInParConj0 = need_in_par_conj(ParConjVars0),
        rename_vars_in_set_of_var(Must, Subn, ParConjVars0, ParConjVars),
        NeedInParConj = need_in_par_conj(ParConjVars),
        MaybeNeed = need_par_conj(NeedInParConj)
    ),
    Details = llds_code_gen_details(PreBirths, PostBirths,
        PreDeaths, PostDeaths, MaybeFollowVars, StoreMap,
        ResumePoint, MaybeNeed).

:- pred rename_vars_in_var_locn_map(must_rename::in,
    map(prog_var, prog_var)::in,
    map(prog_var, abs_locn)::in, map(prog_var, abs_locn)::out) is det.

rename_vars_in_var_locn_map(Must, Subn, VarLocnMap0, VarLocnMap) :-
    map.to_assoc_list(VarLocnMap0, VarLocnList0),
    rename_vars_in_var_locn_list(Must, Subn, VarLocnList0, VarLocnList),
    map.from_assoc_list(VarLocnList, VarLocnMap).

:- pred rename_vars_in_var_locn_list(must_rename::in,
    map(prog_var, prog_var)::in,
    assoc_list(prog_var, abs_locn)::in, assoc_list(prog_var, abs_locn)::out)
    is det.

rename_vars_in_var_locn_list(_Must, _Subn, [], []).
rename_vars_in_var_locn_list(Must, Subn,
        [Var0 - Locn | VarLocns0], [Var - Locn | VarLocns]) :-
    rename_var(Must, Subn, Var0, Var),
    rename_vars_in_var_locn_list(Must, Subn, VarLocns0, VarLocns).

%-----------------------------------------------------------------------------%

stack_slot_num(StackSlot) = N :-
    (
        StackSlot = det_slot(N, Width)
    ;
        StackSlot = parent_det_slot(N, Width)
    ;
        StackSlot = nondet_slot(N),
        Width = single_width
    ),
    (
        Width = single_width
    ;
        Width = double_width,
        unexpected($pred, "double_width")
    ).

stack_slot_to_abs_locn(StackSlot) = AbsLocn :-
    (
        StackSlot = det_slot(N, Width),
        AbsLocn = abs_stackvar(N, Width)
    ;
        StackSlot = parent_det_slot(N, Width),
        AbsLocn = abs_parent_stackvar(N, Width)
    ;
        StackSlot = nondet_slot(N),
        AbsLocn = abs_framevar(N)
    ).

key_stack_slot_to_abs_locn(_, Slot) =
    stack_slot_to_abs_locn(Slot).

abs_locn_to_string(Locn, Str, MaybeWidth) :-
    (
        Locn = any_reg,
        Str = "any_reg",
        MaybeWidth = no
    ;
        Locn = abs_reg(reg_r, N),
        Str = "r" ++ int_to_string(N),
        MaybeWidth = no
    ;
        Locn = abs_reg(reg_f, N),
        Str = "f" ++ int_to_string(N),
        MaybeWidth = no
    ;
        Locn = abs_stackvar(N, Width),
        Str = "stackvar" ++ int_to_string(N),
        MaybeWidth = stack_slot_width_to_string(Width)
    ;
        Locn = abs_parent_stackvar(N, Width),
        Str = "parent_stackvar" ++ int_to_string(N),
        MaybeWidth = stack_slot_width_to_string(Width)
    ;
        Locn = abs_framevar(N),
        Str = "framevar" ++ int_to_string(N),
        MaybeWidth = no
    ).

:- func stack_slot_width_to_string(stack_slot_width) = maybe(string).

stack_slot_width_to_string(single_width) = no.
stack_slot_width_to_string(double_width) = yes("(double width)").

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_llds.
%-----------------------------------------------------------------------------%
