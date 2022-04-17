%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2008, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: store_alloc.m.
% Original author: conway.
% Extensive modification by zs.
%
% Allocates the storage location for each live variable at the end of
% each branched structure, so that the code generator will generate code
% which puts the variable in the same place in each branch.
%
% This module requires arg_infos and livenesses to have already been computed,
% and stack slots allocated.
%
% If the appropriate option is set, the code calls the follow_vars module
% to help guide its decisions.
%
% See compiler/notes/allocation.html for a description of the framework that
% this pass operates within.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.store_alloc.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

%-----------------------------------------------------------------------------%

:- type store_map_run_type
    --->    final_allocation
    ;       for_stack_opt.

:- pred allocate_store_maps(store_map_run_type::in, module_info::in,
    pred_proc_id::in, proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module ll_backend.code_util.
:- import_module ll_backend.follow_vars.
:- import_module ll_backend.liveness.
:- import_module ll_backend.llds.
:- import_module ll_backend.trace_gen.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.vartypes.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.

%-----------------------------------------------------------------------------%

allocate_store_maps(RunType, ModuleInfo, proc(PredId, _), !ProcInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    (
        RunType = final_allocation,
        proc_info_get_goal(!.ProcInfo, Goal0),

        find_final_follow_vars(!.ProcInfo, FollowVarsMap0, NextNonReservedR0,
            NextNonReservedF0),
        find_follow_vars_in_goal(Goal0, Goal1, VarTypes, ModuleInfo,
            FollowVarsMap0, FollowVarsMap, NextNonReservedR0, NextNonReservedR,
            NextNonReservedF0, NextNonReservedF),
        Goal1 = hlds_goal(GoalExpr1, GoalInfo1),
        FollowVars = abs_follow_vars(FollowVarsMap, NextNonReservedR,
            NextNonReservedF),
        goal_info_set_follow_vars(yes(FollowVars), GoalInfo1, GoalInfo2),
        Goal2 = hlds_goal(GoalExpr1, GoalInfo2)
    ;
        RunType = for_stack_opt,
        proc_info_get_goal(!.ProcInfo, Goal2)
    ),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    initial_liveness(ModuleInfo, PredInfo, !.ProcInfo, Liveness0),
    globals.get_trace_level(Globals, TraceLevel),
    NeedFailVars = eff_trace_level_needs_fail_vars(ModuleInfo, PredInfo,
        !.ProcInfo, TraceLevel),
    (
        NeedFailVars = yes,
        trace_fail_vars(ModuleInfo, !.ProcInfo, ResumeVars)
    ;
        NeedFailVars = no,
        ResumeVars = set_of_var.init
    ),
    build_input_arg_list(!.ProcInfo, InputArgLvals),
    LastLocns0 = initial_last_locns(InputArgLvals),
    proc_info_get_stack_slots(!.ProcInfo, StackSlots),
    proc_info_get_varset_vartypes(!.ProcInfo, _VarSet, VarTypes),
    globals.lookup_bool_option(Globals, use_float_registers, FloatRegs),
    (
        FloatRegs = yes,
        FloatRegType = reg_f
    ;
        FloatRegs = no,
        FloatRegType = reg_r
    ),
    StoreAllocInfo = store_alloc_info(StackSlots, VarTypes, FloatRegType),
    store_alloc_in_goal(Goal2, Goal, Liveness0, _, LastLocns0, _,
        ResumeVars, StoreAllocInfo),
    proc_info_set_goal(Goal, !ProcInfo).

:- func initial_last_locns(assoc_list(prog_var, lval)) = last_locns.

initial_last_locns([]) = map.init.
initial_last_locns([Var - Lval | VarLvals]) =
    map.det_insert(initial_last_locns(VarLvals), Var,
        set.make_singleton_set(Lval)).

%-----------------------------------------------------------------------------%

:- type store_alloc_info
    --->    store_alloc_info(
                % Maps each var to its stack slot (if it has one).
                sai_stack_slots     :: stack_slots,
                sai_vartypes        :: vartypes,
                sai_float_reg       :: reg_type
            ).

:- type where_stored    == set(lval).   % These lvals may contain var() rvals.

:- type last_locns  == map(prog_var, where_stored).

:- pred store_alloc_in_goal(hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out, last_locns::in, last_locns::out,
    set_of_progvar::in, store_alloc_info::in) is det.

store_alloc_in_goal(Goal0, Goal, Liveness0, Liveness, !LastLocns, ResumeVars0,
        StoreAllocInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    % note: we must be careful to apply deaths before births
    goal_info_get_pre_deaths(GoalInfo0, PreDeaths),
    goal_info_get_pre_births(GoalInfo0, PreBirths),
    goal_info_get_post_deaths(GoalInfo0, PostDeaths),
    goal_info_get_post_births(GoalInfo0, PostBirths),

    set_of_var.difference(Liveness0,  PreDeaths, Liveness1),
    set_of_var.union(Liveness1, PreBirths, Liveness2),
    store_alloc_in_goal_2(GoalExpr0, GoalExpr, Liveness2, Liveness3,
        !LastLocns, ResumeVars0, BranchedGoal, StoreAllocInfo),
    set_of_var.difference(Liveness3, PostDeaths, Liveness4),
    % If any variables magically become live in the PostBirths,
    % then they have to mundanely become live in a parallel goal,
    % so we don't need to allocate anything for them here.
    set_of_var.union(Liveness4, PostBirths, Liveness),
    (
        BranchedGoal = is_branched_goal,
        % Any variables that become magically live at the
        % end of the goal should not be included in the store map.
        % That is why we use Liveness4 instead of Liveness here.
        set_of_var.union(Liveness4, ResumeVars0, MappedSet),
        MappedVars = set_of_var.to_sorted_list(MappedSet),
        ( if goal_info_maybe_get_store_map(GoalInfo0, StoreMapPrime) then
            AdvisoryStoreMap = StoreMapPrime
        else
            AdvisoryStoreMap = map.init
        ),
        store_alloc_allocate_storage(MappedVars, StoreAllocInfo,
            AdvisoryStoreMap, StoreMap),
        goal_info_set_store_map(StoreMap, GoalInfo0, GoalInfo)
    ;
        BranchedGoal = is_not_branched_goal,
        GoalInfo = GoalInfo0
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

:- type branched_goal
    --->    is_branched_goal
    ;       is_not_branched_goal.

    % Here we process each of the different sorts of goals.
    %
:- pred store_alloc_in_goal_2(hlds_goal_expr::in, hlds_goal_expr::out,
    set_of_progvar::in, set_of_progvar::out,
    last_locns::in, last_locns::out, set_of_progvar::in, branched_goal::out,
    store_alloc_info::in) is det.

store_alloc_in_goal_2(GoalExpr0, GoalExpr, !Liveness, !LastLocns,
        ResumeVars0, BranchedGoal, StoreAllocInfo) :-
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            store_alloc_in_conj(Goals0, Goals, !Liveness, !LastLocns,
                ResumeVars0, StoreAllocInfo)
        ;
            ConjType = parallel_conj,
            store_alloc_in_par_conj(Goals0, Goals, !Liveness, !LastLocns,
                ResumeVars0, StoreAllocInfo)
        ),
        GoalExpr = conj(ConjType, Goals),
        BranchedGoal = is_not_branched_goal
    ;
        GoalExpr0 = disj(Goals0),
        store_alloc_in_disj(Goals0, Goals, !Liveness,
            !.LastLocns, LastLocnsList, ResumeVars0, StoreAllocInfo),
        merge_last_locations(LastLocnsList, !:LastLocns),
        GoalExpr = disj(Goals),
        BranchedGoal = is_branched_goal
    ;
        GoalExpr0 = switch(Var, Det, Cases0),
        store_alloc_in_cases(Cases0, Cases, !Liveness,
            !.LastLocns, LastLocnsList, ResumeVars0, StoreAllocInfo),
        merge_last_locations(LastLocnsList, !:LastLocns),
        GoalExpr = switch(Var, Det, Cases),
        BranchedGoal = is_branched_goal
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        Liveness0 = !.Liveness,
        LastLocns0 = !.LastLocns,

        Cond0 = hlds_goal(_, CondGoalInfo0),
        goal_info_get_resume_point(CondGoalInfo0, ResumeCond),
        goal_info_resume_vars_and_loc(ResumeCond, ResumeCondVars, _),
        store_alloc_in_goal(Cond0, Cond, Liveness0, Liveness1,
            LastLocns0, LastLocnsCond, ResumeCondVars, StoreAllocInfo),
        store_alloc_in_goal(Then0, Then, Liveness1, Liveness,
            LastLocnsCond, LastLocnsThen, ResumeVars0, StoreAllocInfo),
        store_alloc_in_goal(Else0, Else, Liveness0, _Liveness2,
            LastLocns0, LastLocnsElse, ResumeVars0, StoreAllocInfo),
        merge_last_locations([LastLocnsThen, LastLocnsElse], LastLocns),

        !:Liveness = Liveness,
        !:LastLocns = LastLocns,
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        BranchedGoal = is_branched_goal
    ;
        GoalExpr0 = negation(SubGoal0),
        SubGoal0 = hlds_goal(_, SubGoalInfo0),
        goal_info_get_resume_point(SubGoalInfo0, ResumeNot),
        goal_info_resume_vars_and_loc(ResumeNot, ResumeNotVars, _),
        store_alloc_in_goal(SubGoal0, SubGoal, !Liveness, !.LastLocns, _,
            ResumeNotVars, StoreAllocInfo),
        GoalExpr = negation(SubGoal),
        BranchedGoal = is_not_branched_goal
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(TermVar, from_ground_term_construct)
        then
            GoalExpr = GoalExpr0,
            set_of_var.insert(TermVar, !Liveness)
        else
            store_alloc_in_goal(SubGoal0, SubGoal, !Liveness, !LastLocns,
                ResumeVars0, StoreAllocInfo),
            GoalExpr = scope(Reason, SubGoal)
        ),
        BranchedGoal = is_not_branched_goal
    ;
        ( GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0,
        BranchedGoal = is_not_branched_goal
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out, last_locns::in, last_locns::out,
    set_of_progvar::in, store_alloc_info::in) is det.

store_alloc_in_conj([], [], !Liveness, !LastLocns, _, _).
store_alloc_in_conj([Goal0 | Goals0], [Goal | Goals], !Liveness, !LastLocns,
        ResumeVars0, StoreAllocInfo) :-
    ( if
        % XXX should be threading the instmap.
        Goal0 = hlds_goal(_, GoalInfo),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        instmap_delta_is_unreachable(InstMapDelta)
    then
        store_alloc_in_goal(Goal0, Goal, !Liveness, !LastLocns,
            ResumeVars0, StoreAllocInfo),
        Goals = Goals0
    else
        store_alloc_in_goal(Goal0, Goal, !Liveness, !LastLocns,
            ResumeVars0, StoreAllocInfo),
        store_alloc_in_conj(Goals0, Goals, !Liveness, !LastLocns,
            ResumeVars0, StoreAllocInfo)
    ).

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_par_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out, last_locns::in, last_locns::out,
    set_of_progvar::in, store_alloc_info::in) is det.

store_alloc_in_par_conj([], [], _Liveness0, set_of_var.init, !LastLocns, _, _).
store_alloc_in_par_conj([Goal0 | Goals0], [Goal | Goals], Liveness0, Liveness,
        !LastLocns, ResumeVars0, StoreAllocInfo) :-
    store_alloc_in_goal(Goal0, Goal, Liveness0, Liveness1,
        !LastLocns, ResumeVars0, StoreAllocInfo),
    store_alloc_in_par_conj(Goals0, Goals, Liveness0, Liveness2,
        !LastLocns, ResumeVars0, StoreAllocInfo),
    Liveness = set_of_var.union(Liveness1, Liveness2).

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out,
    last_locns::in, list(last_locns)::out,
    set_of_progvar::in, store_alloc_info::in) is det.

store_alloc_in_disj([], [], !Liveness, _, [], _, _).
store_alloc_in_disj([Goal0 | Goals0], [Goal | Goals], Liveness0, Liveness,
        LastLocns0, [LastLocnsGoal | LastLocnsDisj],
        ResumeVars0, StoreAllocInfo) :-
    Goal0 = hlds_goal(_, GoalInfo0),
    goal_info_get_resume_point(GoalInfo0, ResumeGoal),
    (
        ResumeGoal = no_resume_point,
        ResumeGoalVars = ResumeVars0
    ;
        ResumeGoal = resume_point(ResumeGoalVars, _)
    ),
    store_alloc_in_goal(Goal0, Goal, Liveness0, Liveness,
        LastLocns0, LastLocnsGoal, ResumeGoalVars, StoreAllocInfo),
    store_alloc_in_disj(Goals0, Goals, Liveness0, _Liveness1,
        LastLocns0, LastLocnsDisj, ResumeVars0, StoreAllocInfo).

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_cases(list(case)::in, list(case)::out,
    set_of_progvar::in, set_of_progvar::out,
    last_locns::in, list(last_locns)::out,
    set_of_progvar::in, store_alloc_info::in) is det.

store_alloc_in_cases([], [], !Liveness, _, [], _, _).
store_alloc_in_cases([Case0 | Cases0], [Case | Cases], Liveness0, Liveness,
        LastLocns0, [LastLocnsGoal | LastLocnsCases],
        ResumeVars0, StoreAllocInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    store_alloc_in_goal(Goal0, Goal, Liveness0, Liveness,
        LastLocns0, LastLocnsGoal, ResumeVars0, StoreAllocInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    store_alloc_in_cases(Cases0, Cases, Liveness0, _Liveness1,
        LastLocns0, LastLocnsCases, ResumeVars0, StoreAllocInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred merge_last_locations(list(last_locns)::in, last_locns::out) is det.

merge_last_locations(LastLocnsList, LastLocns) :-
    (
        LastLocnsList = [LastLocns | _]
    ;
        LastLocnsList = [],
        LastLocns = map.init
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Given a follow_map which
    %
    % 1 may contain entries for non-live variables,
    %
    % 2 may contain no entry for a live variable,
    %
    % 3 which may map two live variables to one lval, and/or
    %
    % 4 map an lval to the artificial location reg(r(-1)),
    %
    % generate a store map that maps every live variable to its own
    % real location.
    %
:- pred store_alloc_allocate_storage(list(prog_var)::in, store_alloc_info::in,
    abs_store_map::in, abs_store_map::out) is det.

store_alloc_allocate_storage(LiveVars, StoreAllocInfo, FollowVars,
        !:StoreMap) :-
    % This addresses point 1.
    map.keys(FollowVars, FollowKeys),
    store_alloc_remove_nonlive(FollowKeys, LiveVars, FollowVars, !:StoreMap),

    % This addresses points 3 and 4.
    map.keys(!.StoreMap, StoreVars),
    set.init(SeenLvals0),
    store_alloc_handle_conflicts_and_nonreal(StoreAllocInfo, StoreVars,
        1, N, SeenLvals0, SeenLvals, !StoreMap),

    % This addresses point 2.
    store_alloc_allocate_extras(StoreAllocInfo, LiveVars, N, SeenLvals,
        !StoreMap).

:- pred store_alloc_remove_nonlive(list(prog_var)::in, list(prog_var)::in,
    abs_store_map::in, abs_store_map::out) is det.

store_alloc_remove_nonlive([], _LiveVars, !StoreMap).
store_alloc_remove_nonlive([Var | Vars], LiveVars, !StoreMap) :-
    ( if list.member(Var, LiveVars) then
        true
    else
        map.delete(Var, !StoreMap)
    ),
    store_alloc_remove_nonlive(Vars, LiveVars, !StoreMap).

:- pred store_alloc_handle_conflicts_and_nonreal(store_alloc_info::in,
    list(prog_var)::in, int::in, int::out,
    set(abs_locn)::in, set(abs_locn)::out,
    abs_store_map::in, abs_store_map::out) is det.

store_alloc_handle_conflicts_and_nonreal(_, [],
        !N, !SeenLocns, !StoreMap).
store_alloc_handle_conflicts_and_nonreal(StoreAllocInfo, [Var | Vars],
        !N, !SeenLocns, !StoreMap) :-
    map.lookup(!.StoreMap, Var, Locn),
    ( if
        ( Locn = any_reg
        ; set.member(Locn, !.SeenLocns)
        )
    then
        ( if Locn = abs_reg(RegTypePrime, _) then
            RegType = RegTypePrime
        else
            reg_type_for_var(StoreAllocInfo, Var, RegType)
        ),
        next_free_reg(RegType, !.SeenLocns, !N),
        FinalLocn = abs_reg(RegType, !.N),
        map.det_update(Var, FinalLocn, !StoreMap)
    else
        FinalLocn = Locn
    ),
    set.insert(FinalLocn, !SeenLocns),
    store_alloc_handle_conflicts_and_nonreal(StoreAllocInfo, Vars,
        !N, !SeenLocns, !StoreMap).

:- pred store_alloc_allocate_extras(store_alloc_info::in, list(prog_var)::in,
    int::in, set(abs_locn)::in, abs_store_map::in, abs_store_map::out) is det.

store_alloc_allocate_extras(_, [], _, _, !StoreMap).
store_alloc_allocate_extras(StoreAllocInfo, [Var | Vars], !.N, !.SeenLocns,
        !StoreMap) :-
    ( if map.contains(!.StoreMap, Var) then
        % We have already allocated a slot for this variable.
        true
    else
        % We have not yet allocated a slot for this variable,
        % which means it is not in the follow vars (if any).
        StoreAllocInfo = store_alloc_info(StackSlots, _, _),
        ( if
            map.search(StackSlots, Var, StackSlot),
            StackSlotLocn = stack_slot_to_abs_locn(StackSlot),
            not set.member(StackSlotLocn, !.SeenLocns)
            % Follow_vars was run, so the only reason why a var would not be
            % in the follow_vars set is if it was supposed to be in its stack
            % slot.
        then
            Locn = StackSlotLocn
        else
            reg_type_for_var(StoreAllocInfo, Var, RegType),
            next_free_reg(RegType, !.SeenLocns, !N),
            Locn = abs_reg(RegType, !.N)
        ),
        map.det_insert(Var, Locn, !StoreMap),
        set.insert(Locn, !SeenLocns)
    ),
    store_alloc_allocate_extras(StoreAllocInfo, Vars, !.N, !.SeenLocns,
        !StoreMap).

:- pred reg_type_for_var(store_alloc_info::in, prog_var::in, reg_type::out)
    is det.

reg_type_for_var(StoreAllocInfo, Var, RegType) :-
    StoreAllocInfo = store_alloc_info(_, VarTypes, FloatRegType),
    (
        FloatRegType = reg_r,
        RegType = reg_r
    ;
        FloatRegType = reg_f,
        lookup_var_type(VarTypes, Var, VarType),
        ( if VarType = float_type then
            RegType = reg_f
        else
            RegType = reg_r
        )
    ).

%-----------------------------------------------------------------------------%

:- pred next_free_reg(reg_type::in, set(abs_locn)::in, int::in, int::out)
    is det.

next_free_reg(RegType, Values, N0, N) :-
    ( if set.member(abs_reg(RegType, N0), Values) then
        N1 = N0 + 1,
        next_free_reg(RegType, Values, N1, N)
    else
        N = N0
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.store_alloc.
%-----------------------------------------------------------------------------%
