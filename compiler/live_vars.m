%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2008, 2010-2012 The University of Melbourne.
% Copyright (C) 2014-2018, 2020-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: live_vars.m.
% Main authors: conway, zs.
%
% This module finds out what variables need to be saved across calls,
% across goals that may fail, and in parallel conjunctions. It then does two
% things with that information. First, it attaches that information to the
% relevant goal as a LLDS-backend-specific annotation. Second, it invokes
% the relevant type class method of the allocator-specific data structure
% it is passed. The basic stack slot allocator and the optimizing stack slot
% allocator pass different instances of this type class.
%
%---------------------------------------------------------------------------%

:- module ll_backend.live_vars.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module bool.

%---------------------------------------------------------------------------%

:- type dummy_var_info
    --->    no_var_is_dummy
    ;       some_var_may_be_dummy(var_table).

:- type alloc_data
    --->    alloc_data(
                ad_module_info          ::  module_info,
                ad_proc_info            ::  proc_info,
                ad_pred_proc_id         ::  pred_proc_id,
                ad_dummy_var_info       ::  dummy_var_info,
                ad_typeinfo_liveness    ::  bool,
                ad_opt_no_return_calls  ::  bool
            ).

:- typeclass stack_alloc_info(T) where [
    pred at_call_site(need_across_call::in, alloc_data::in,
        T::in, T::out) is det,
    pred at_resume_site(need_in_resume::in, alloc_data::in,
        T::in, T::out) is det,
    pred at_par_conj(need_in_par_conj::in, alloc_data::in,
        T::in, T::out) is det,
    pred at_recursive_call_for_loop_control(need_for_loop_control::in,
        alloc_data::in, T::in, T::out) is det
].

:- pred build_live_sets_in_goal_no_par_stack(alloc_data::in,
    set_of_progvar::in, hlds_goal::in, hlds_goal::out, T::in, T::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det <= stack_alloc_info(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.arg_info.
:- import_module hlds.code_model.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%

% The stack_slots structure (map(prog_var, lval)) is threaded through the
% traversal of the goal. The liveness information is computed from the liveness
% delta annotations.

build_live_sets_in_goal_no_par_stack(AllocData, ResumeVars0, Goal0, Goal,
        !StackAlloc, !Liveness, !NondetLiveness) :-
    empty_par_stackvars(ParStackVars0),
    build_live_sets_in_goal(AllocData, ResumeVars0, Goal0, Goal,
        !StackAlloc, !Liveness, !NondetLiveness, ParStackVars0, _ParStackVars).

:- pred build_live_sets_in_goal(alloc_data::in, set_of_progvar::in,
    hlds_goal::in, hlds_goal::out, T::in, T::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out,
    parallel_stackvars::in, parallel_stackvars::out)
    is det <= stack_alloc_info(T).

build_live_sets_in_goal(AllocData, ResumeVars0, Goal0, Goal,
        !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    goal_info_get_pre_deaths(GoalInfo0, PreDeaths),
    goal_info_get_pre_births(GoalInfo0, PreBirths),
    goal_info_get_post_deaths(GoalInfo0, PostDeaths),
    goal_info_get_post_births(GoalInfo0, PostBirths),

    % NOTE We must be careful to apply deaths before births.
    set_of_var.difference(!.Liveness, PreDeaths, !:Liveness),
    set_of_var.union(!.Liveness, PreBirths, !:Liveness),

    % If the goal is atomic, we want to apply the postdeaths before processing
    % the goal, but if the goal is a compound goal, then we want to apply them
    % after processing it.
    HasSubGoals = goal_expr_has_subgoals(GoalExpr0),
    (
        HasSubGoals = does_not_have_subgoals,
        set_of_var.difference(!.Liveness, PostDeaths, !:Liveness)
    ;
        HasSubGoals = has_subgoals
    ),

    goal_info_get_resume_point(GoalInfo0, ResumePoint),
    (
        ResumePoint = no_resume_point,
        ResumeVars1 = ResumeVars0,
        GoalInfo1 = GoalInfo0
    ;
        ResumePoint = resume_point(ResumePointVars, Locs),
        resume_locs_include_stack(Locs, InclStack),
        (
            InclStack = yes,
            set_of_var.union(ResumeVars0, ResumePointVars, ResumeVars1),
            ResumeOnStack = yes
        ;
            InclStack = no,
            ResumeVars1 = ResumeVars0,
            ResumeOnStack = no
        ),
        NeedInResume = need_in_resume(ResumeOnStack, ResumeVars1,
            !.NondetLiveness),
        record_resume_site(NeedInResume, AllocData,
            GoalInfo0, GoalInfo1, !StackAlloc)
    ),

    build_live_sets_in_goal_expr(AllocData, ResumeVars1,
        GoalExpr0, GoalExpr, GoalInfo1, GoalInfo,
        !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars),

    (
        HasSubGoals = does_not_have_subgoals
    ;
        HasSubGoals = has_subgoals,
        set_of_var.difference(!.Liveness, PostDeaths, !:Liveness)
    ),

    set_of_var.union(!.Liveness, PostBirths, !:Liveness),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred resume_locs_include_stack(resume_locs::in, bool::out) is det.

resume_locs_include_stack(resume_locs_orig_only, no).
resume_locs_include_stack(resume_locs_stack_only, yes).
resume_locs_include_stack(resume_locs_orig_then_stack, yes).
resume_locs_include_stack(resume_locs_stack_then_orig, yes).

%---------------------------------------------------------------------------%

    % Here we process each of the different sorts of goals. `Liveness' is the
    % set of live variables, i.e. vars which have been referenced and may be
    % referenced again (during forward execution). `ResumeVars' is the set
    % of variables that may or may not be `live' during the current forward
    % execution but will become live again on backtracking. `StackAlloc' is the
    % interference graph, i.e. the set of sets of variables which need to be
    % on the stack at the same time.
    %
:- pred build_live_sets_in_goal_expr(alloc_data::in, set_of_progvar::in,
    hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out, T::in, T::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out,
    parallel_stackvars::in, parallel_stackvars::out)
    is det <= stack_alloc_info(T).

build_live_sets_in_goal_expr(AllocData, ResumeVars0,
        GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
        !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars) :-
    (
        GoalExpr0 = unify(_, _, _, Unification, _),
        GoalExpr = GoalExpr0,
        GoalInfo = GoalInfo0,
        (
            ( Unification = construct(_, _, _, _, _, _, _)
            ; Unification = deconstruct(_, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            )
        ;
            Unification = complicated_unify(_, _, _),
            unexpected($pred, "complicated_unify")
        )
    ;
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        build_live_sets_in_plain_call(AllocData, ResumeVars0,
            GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
            !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars)
    ;
        GoalExpr0 = generic_call(GenericCall, ArgVars, Modes, _, _),
        GoalExpr = GoalExpr0,
        (
            GenericCall = cast(_),
            GoalInfo = GoalInfo0
        ;
            ( GenericCall = higher_order(_, _, _, _, _)
            ; GenericCall = class_method(_, _, _, _)
            ; GenericCall = event_call(_)
            ),
            ModuleInfo = AllocData ^ ad_module_info,
            ProcInfo = AllocData ^ ad_proc_info,
            proc_info_get_var_table(ProcInfo, VarTable),
            lookup_var_types(VarTable, ArgVars, Types),
            arg_info.partition_generic_call_args(ModuleInfo, ArgVars,
                Types, Modes, _InVars, OutVars, _UnusedVars),
            build_live_sets_in_call(AllocData, ResumeVars0,
                set_to_bitset(OutVars), GoalInfo0, GoalInfo,
                !StackAlloc, !.Liveness, !NondetLiveness, !ParStackVars)
        )
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        build_live_sets_in_foreign_proc(AllocData, ResumeVars0,
            GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
            !StackAlloc, !.Liveness, !NondetLiveness, !ParStackVars)
    ;
        GoalExpr0 = conj(_, _),
        build_live_sets_in_conj(AllocData, ResumeVars0,
            GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
            !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars)
    ;
        GoalExpr0 = disj(_),
        build_live_sets_in_disj(AllocData, ResumeVars0,
            GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
            !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        build_live_sets_in_cases(AllocData, ResumeVars0, Cases0, Cases,
            !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars),
        GoalExpr = switch(Var, CanFail, Cases),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        Liveness0 = !.Liveness,
        NondetLiveness0 = !.NondetLiveness,
        build_live_sets_in_goal(AllocData, ResumeVars0, Cond0, Cond,
            !StackAlloc, Liveness0, LivenessCond,
            NondetLiveness0, NondetLivenessCond, !ParStackVars),
        build_live_sets_in_goal(AllocData, ResumeVars0, Then0, Then,
            !StackAlloc, LivenessCond, _LivenessThen,
            NondetLivenessCond, NondetLivenessThen, !ParStackVars),
        build_live_sets_in_goal(AllocData, ResumeVars0, Else0, Else,
            !StackAlloc, Liveness0, Liveness,
            NondetLiveness0, NondetLivenessElse, !ParStackVars),
        set_of_var.union(NondetLivenessThen, NondetLivenessElse,
            NondetLiveness),
        !:Liveness = Liveness,
        !:NondetLiveness = NondetLiveness,
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = negation(SubGoal0),
        build_live_sets_in_goal(AllocData, ResumeVars0, SubGoal0, SubGoal,
            !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars),
        GoalExpr = negation(SubGoal),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = scope(_, _),
        build_live_sets_in_scope(AllocData, ResumeVars0,
            GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
            !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars)
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

%---------------------------------------------------------------------------%

:- pred build_live_sets_in_plain_call(alloc_data::in, set_of_progvar::in,
    hlds_goal_expr::in(goal_expr_plain_call), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out, T::in, T::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out,
    parallel_stackvars::in, parallel_stackvars::out)
    is det <= stack_alloc_info(T).

build_live_sets_in_plain_call(AllocData, ResumeVars0,
        GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
        !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars) :-
    GoalExpr0 = plain_call(PredId, ProcId, ArgVars, Builtin, _, _),
    GoalExpr = GoalExpr0,
    ModuleInfo = AllocData ^ ad_module_info,
    CallerProcInfo = AllocData ^ ad_proc_info,
    proc_info_get_var_table(CallerProcInfo, VarTable),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    arg_info.partition_proc_call_args(ModuleInfo, ProcInfo, VarTable,
        ArgVars, _InVars, OutVars, _UnusedVars),
    (
        Builtin = inline_builtin,
        GoalInfo = GoalInfo0
    ;
        Builtin = not_builtin,
        build_live_sets_in_call(AllocData, ResumeVars0,
            set_to_bitset(OutVars), GoalInfo0, GoalInfo,
            !StackAlloc, !.Liveness, !NondetLiveness, !ParStackVars)
    ),
    CalleePredProcId = AllocData ^ ad_pred_proc_id,
    ( if CalleePredProcId = proc(PredId, ProcId) then
        % If a call is recursive and a loop control scope has been seen,
        % then the recursive call is a barrier for loop control, and
        % we have to ensure that spawned off computations use distinct
        % stack slots from one another, and from the code up to and including
        % this call.
        par_stack_vars_recursive_call(MaybeNeedLC, DelayDeathSet,
            !ParStackVars),
        (
            MaybeNeedLC = yes(NeedLC),
            at_recursive_call_for_loop_control(NeedLC, AllocData,
                !StackAlloc)
        ;
            MaybeNeedLC = no
        ),
        !:Liveness = set_of_var.difference(!.Liveness, DelayDeathSet)
    else
        true
    ).

    % The variables which need to be saved onto the stack, directly or
    % indirectly, before a call or may_call_mercury foreign_proc are all
    % the variables that are live after the goal except for the output
    % arguments produced by the goal, plus all the variables that may be
    % needed at an enclosing resumption point.
    %
:- pred build_live_sets_in_call(alloc_data::in, set_of_progvar::in,
    set_of_progvar::in, hlds_goal_info::in, hlds_goal_info::out, T::in, T::out,
    set_of_progvar::in, set_of_progvar::in, set_of_progvar::out,
    parallel_stackvars::in, parallel_stackvars::out)
    is det <= stack_alloc_info(T).

build_live_sets_in_call(AllocData, ResumeVars0, OutVars, GoalInfo0, GoalInfo,
        !StackAlloc, Liveness, !NondetLiveness, !ParStackVars) :-
    set_of_var.difference(Liveness, OutVars, ForwardVars0),

    % Might need to add more live variables with typeinfo liveness
    % calculation.
    maybe_add_typeinfo_liveness(AllocData, OutVars, ForwardVars0, ForwardVars),

    Detism = goal_info_get_determinism(GoalInfo0),
    ( if
        Detism = detism_erroneous,
        AllocData ^ ad_opt_no_return_calls = yes
    then
        NeedAcrossCall = need_across_call(set_of_var.init, set_of_var.init,
            set_of_var.init)
    else
        NeedAcrossCall = need_across_call(ForwardVars, ResumeVars0,
            !.NondetLiveness)
    ),

    record_call_site(NeedAcrossCall, AllocData,
        GoalInfo0, GoalInfo, !StackAlloc),

    % If this is a nondet call, then all the stack slots we need
    % must be protected against reuse in following code.

    CodeModel = goal_info_get_code_model(GoalInfo),
    (
        CodeModel = model_det
    ;
        CodeModel = model_semi
    ;
        CodeModel = model_non,
        set_of_var.union(!.NondetLiveness, ForwardVars, !:NondetLiveness)
    ),

    % In a parallel conjunction all the stack slots we need must not be reused
    % in other parallel conjuncts. We keep track of which variables have been
    % allocated stack slots in each conjunct.
    par_stack_vars_record_call_stack_vars(ForwardVars, !ParStackVars).

%---------------------------------------------------------------------------%

:- pred build_live_sets_in_foreign_proc(alloc_data::in, set_of_progvar::in,
    hlds_goal_expr::in(goal_expr_foreign_proc), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out, T::in, T::out,
    set_of_progvar::in, set_of_progvar::in, set_of_progvar::out,
    parallel_stackvars::in, parallel_stackvars::out)
    is det <= stack_alloc_info(T).

build_live_sets_in_foreign_proc(AllocData, ResumeVars0,
        GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
        !StackAlloc, Liveness0, !NondetLiveness, !ParStackVars) :-
    GoalExpr0 = call_foreign_proc(Attributes, PredId, ProcId, Args, _, _, _),
    GoalExpr = GoalExpr0,
    ModuleInfo = AllocData ^ ad_module_info,
    CallerProcInfo = AllocData ^ ad_proc_info,
    proc_info_get_var_table(CallerProcInfo, VarTable),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    ArgVars = list.map(foreign_arg_var, Args),
    arg_info.partition_proc_call_args(ModuleInfo, ProcInfo, VarTable,
        ArgVars, _InVars, OutVars, _UnusedVars),
    CodeModel = goal_info_get_code_model(GoalInfo0),
    ( if
        % We don't need to save any variables onto the stack before a
        % foreign_proc if we know that it can't succeed more than once
        % and that it is not going to call back Mercury code, because
        % such a foreign_proc code won't clobber the registers.
        CodeModel \= model_non,
        get_may_call_mercury(Attributes) = proc_will_not_call_mercury
    then
        GoalInfo = GoalInfo0
    else
        % The variables which need to be saved onto the stack before the call
        % are all the variables that are live after the call (except for the
        % output arguments produced by the call), plus all the variables
        % that may be needed at an enclosing resumption point.
        build_live_sets_in_call(AllocData, ResumeVars0,
            set_to_bitset(OutVars), GoalInfo0, GoalInfo,
            !StackAlloc, Liveness0, !NondetLiveness, !ParStackVars)
    ).

%---------------------------------------------------------------------------%

:- pred build_live_sets_in_conj(alloc_data::in, set_of_progvar::in,
    hlds_goal_expr::in(goal_expr_conj), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out, T::in, T::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out,
    parallel_stackvars::in, parallel_stackvars::out)
    is det <= stack_alloc_info(T).

build_live_sets_in_conj(AllocData, ResumeVars0,
        GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
        !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars) :-
    GoalExpr0 = conj(ConjType, Goals0),
    (
        ConjType = plain_conj,
        GoalInfo = GoalInfo0,
        build_live_sets_in_conjuncts(AllocData, ResumeVars0, Goals0, Goals,
            !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars)
    ;
        ConjType = parallel_conj,
        % Since each parallel conjunct may be run in a different Mercury
        % context to the current context, we must save all the variables
        % that are live or nonlocal to the parallel conjunction. Nonlocal
        % variables that are currently free, but are bound inside one of
        % the conjuncts need a stackslot, because they are passed out
        % by reference to that stackslot. Variables needed on backtracking
        % must be available in a stackslot past the parallel conjunction
        % as well.
        NonLocals = goal_info_get_code_gen_nonlocals(GoalInfo0),
        LiveSet = set_of_var.union_list([NonLocals, !.Liveness, ResumeVars0]),

        par_stack_vars_get_nonlocals(!.ParStackVars, OuterNonLocals),

        OuterParStackVars = !.ParStackVars,
        par_stack_vars_start_parallel_conjunction(LiveSet, !ParStackVars),
        build_live_sets_in_par_conj(AllocData, ResumeVars0, Goals0, Goals,
            !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars),
        par_stack_vars_get_stackvars(!.ParStackVars, InnerStackVars),

        % This is safe but suboptimal. It causes all variables which need
        % stack slots in a parallel conjunction to have distinct stack slots.
        % Variables local to a single conjunct could share stack slots,
        % as long as the _sets_ of stack slots allocated to different
        % parallel conjuncts are distinct.
        InnerNonLocals = LiveSet `set_of_var.union` OuterNonLocals,
        NeedInParConj = need_in_par_conj(InnerNonLocals `set_of_var.union`
            InnerStackVars),
        record_par_conj(NeedInParConj, AllocData,
            GoalInfo0, GoalInfo, !StackAlloc),

        par_stack_vars_end_parallel_conjunction(LiveSet, OuterParStackVars,
            !ParStackVars)
    ),
    GoalExpr = conj(ConjType, Goals).

:- pred build_live_sets_in_conjuncts(alloc_data::in, set_of_progvar::in,
    list(hlds_goal)::in, list(hlds_goal)::out, T::in, T::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out,
    parallel_stackvars::in, parallel_stackvars::out)
    is det <= stack_alloc_info(T).

build_live_sets_in_conjuncts(_, _, [], [],
        !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars).
build_live_sets_in_conjuncts(AllocData, ResumeVars0,
        [Goal0 | Goals0], [Goal | Goals],
        !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars) :-
    Goal0 = hlds_goal(_, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    ( if instmap_delta_is_unreachable(InstMapDelta) then
        build_live_sets_in_goal(AllocData, ResumeVars0, Goal0, Goal,
            !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars),
        Goals = []
    else
        build_live_sets_in_goal(AllocData, ResumeVars0, Goal0, Goal,
            !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars),
        build_live_sets_in_conjuncts(AllocData, ResumeVars0, Goals0, Goals,
            !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars)
    ).

%---------------------------------------------------------------------------%

:- pred build_live_sets_in_par_conj(alloc_data::in, set_of_progvar::in,
    list(hlds_goal)::in, list(hlds_goal)::out, T::in, T::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out,
    parallel_stackvars::in, parallel_stackvars::out)
    is det <= stack_alloc_info(T).

build_live_sets_in_par_conj(_, _, [], [],
        !StackAlloc, Liveness, Liveness, !NondetLiveness, !ParStackVars).
build_live_sets_in_par_conj(AllocData, ResumeVars0,
        [Goal0 | Goals0], [Goal | Goals],
        !StackAlloc, Liveness0, Liveness, !NondetLiveness, !ParStackVars) :-
    build_live_sets_in_goal(AllocData, ResumeVars0, Goal0, Goal,
        !StackAlloc, Liveness0, Liveness, !NondetLiveness, !ParStackVars),
    par_stack_vars_next_par_conjunct(!ParStackVars),
    build_live_sets_in_par_conj(AllocData, ResumeVars0, Goals0, Goals,
        !StackAlloc, Liveness0, _Liveness1, !NondetLiveness, !ParStackVars).

%---------------------------------------------------------------------------%

:- pred build_live_sets_in_disj(alloc_data::in, set_of_progvar::in,
    hlds_goal_expr::in(goal_expr_disj), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out, T::in, T::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out,
    parallel_stackvars::in, parallel_stackvars::out)
    is det <= stack_alloc_info(T).

build_live_sets_in_disj(AllocData, ResumeVars0,
        GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
        !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars) :-
    GoalExpr0 = disj(Goals0),
    build_live_sets_in_disjuncts(AllocData, ResumeVars0, GoalInfo0,
        Goals0, Goals, !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars),
    (
        Goals = [FirstGoal | _],
        FirstGoal = hlds_goal(_, FirstGoalInfo),
        goal_info_get_resume_point(FirstGoalInfo, ResumePoint),
        (
            ResumePoint = resume_point(ResumeVars, _Locs),
            % If we can backtrack into the disjunction, we must protect the
            % stack slots needed by any of its resumption points from being
            % reused in the following code. The first resumption point's
            % variables include all the variables needed by all the
            % resumption points. However, the first disjunct can be
            % orig_only while later disjuncts are include the stack.

            % Note that we must check the disjunction's code model, not any
            % disjuncts'; the disjunction as a whole can be model_non
            % without any disjunct being model_non.
            ( if
                goal_info_get_code_model(GoalInfo0) = model_non,
                some [Disjunct] (
                    list.member(Disjunct, Goals),
                    Disjunct = hlds_goal(_, DisjunctGoalInfo),
                    goal_info_get_resume_point(DisjunctGoalInfo,
                        DisjunctResumePoint),
                    DisjunctResumePoint = resume_point(_, Locs),
                    resume_locs_include_stack(Locs, yes)
                )
            then
                set_of_var.union(!.NondetLiveness, ResumeVars,
                    !:NondetLiveness)
            else
                true
            )
        ;
            % We can get here if the disjunction is not really a disjunction,
            % because the first alternative cannot fail and will be committed
            % to (e.g. in a first-solution context). Simplification should
            % eliminate such disjunctions, replacing them with the first
            % disjunct, but until that is done, we must handle them here.
            ResumePoint = no_resume_point
        )
    ;
        Goals = []
    ),
    GoalExpr = disj(Goals),
    GoalInfo = GoalInfo0.

:- pred build_live_sets_in_disjuncts(alloc_data::in, set_of_progvar::in,
    hlds_goal_info::in, list(hlds_goal)::in, list(hlds_goal)::out,
    T::in, T::out, set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out,
    parallel_stackvars::in, parallel_stackvars::out)
    is det <= stack_alloc_info(T).

build_live_sets_in_disjuncts(_, _, _, [], [],
        !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars).
build_live_sets_in_disjuncts(AllocData, ResumeVars0, DisjGoalInfo,
        [Goal0 | Goals0], [Goal | Goals], !StackAlloc, Liveness0, Liveness,
        NondetLiveness0, NondetLiveness, !ParStackVars) :-
    Goal = hlds_goal(_, GoalInfo),
    build_live_sets_in_goal(AllocData, ResumeVars0, Goal0, Goal,
        !StackAlloc, Liveness0, Liveness, NondetLiveness0, NondetLiveness1,
        !ParStackVars),
    build_live_sets_in_disjuncts(AllocData, ResumeVars0, DisjGoalInfo,
        Goals0, Goals, !StackAlloc, Liveness0, _Liveness2,
        NondetLiveness0, NondetLiveness2, !ParStackVars),
    DisjCodeModel = goal_info_get_code_model(DisjGoalInfo),
    (
        DisjCodeModel = model_non,
        % NondetLiveness should be a set of prog_var sets. Instead of taking
        % the union of the NondetLive sets at the ends of disjuncts, we should
        % just keep them in this set of sets.
        set_of_var.union(NondetLiveness1, NondetLiveness2, NondetLiveness3),
        goal_info_get_resume_point(GoalInfo, Resume),
        ( if
            Resume = resume_point(ResumePointVars, Locs),
            resume_locs_include_stack(Locs, yes)
        then
            set_of_var.union(NondetLiveness3, ResumePointVars, NondetLiveness)
        else
            NondetLiveness = NondetLiveness3
        )
    ;
        ( DisjCodeModel = model_det
        ; DisjCodeModel = model_semi
        ),
        NondetLiveness = NondetLiveness0
    ).

%---------------------------------------------------------------------------%

:- pred build_live_sets_in_cases(alloc_data::in, set_of_progvar::in,
    list(case)::in, list(case)::out, T::in, T::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out,
    parallel_stackvars::in, parallel_stackvars::out)
    is det <= stack_alloc_info(T).

build_live_sets_in_cases(_, _, [], [],
        !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars).
build_live_sets_in_cases(AllocData, ResumeVars0,
        [Case0 | Cases0], [Case | Cases], !StackAlloc, Liveness0, Liveness,
        NondetLiveness0, NondetLiveness, !ParStackVars) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    build_live_sets_in_goal(AllocData, ResumeVars0, Goal0, Goal,
        !StackAlloc, Liveness0, Liveness, NondetLiveness0, NondetLiveness1,
        !ParStackVars),
    Case = case(MainConsId, OtherConsIds, Goal),
    build_live_sets_in_cases(AllocData, ResumeVars0, Cases0, Cases,
        !StackAlloc, Liveness0, _Liveness2, NondetLiveness0, NondetLiveness2,
        !ParStackVars),
    set_of_var.union(NondetLiveness1, NondetLiveness2, NondetLiveness).

%---------------------------------------------------------------------------%

:- pred build_live_sets_in_scope(alloc_data::in, set_of_progvar::in,
    hlds_goal_expr::in(goal_expr_scope), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out, T::in, T::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out,
    parallel_stackvars::in, parallel_stackvars::out)
    is det <= stack_alloc_info(T).

build_live_sets_in_scope(AllocData, ResumeVars0,
        GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
        !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars) :-
    GoalExpr0 = scope(Reason, SubGoal0),
    ( if Reason = from_ground_term(TermVar, from_ground_term_construct) then
        % We do not modify construct unifications or conjunctions,
        % so we do not modify these scopes, which contain only a
        % conjunction of construct unifications.
        GoalExpr = GoalExpr0,
        GoalInfo = GoalInfo0,
        % The scope does not contain any calls, resume points or parallel
        % conjunctions, so there are no updates to !StackAlloc,
        % !NondetLiveness, or !ParStackVars.
        set_of_var.insert(TermVar, !Liveness)
        % XXX We could treat from_ground_term_deconstruct scopes specially
        % as well.
    else if Reason = loop_control(LCVar, LCSVar, _) then
        % We must handle loop control scopes specially, see the comment for
        % parallel conjunctions above. Like parallel conjunctions, we need
        % stack slots for the NonLocals, but we also need non-overlapping
        % slots for LC and LCS.

        % XXX: When we use a frame on the child's stack rather than the
        % parent's, we may be able to save fewer variables to the stack at
        % this time. This is an optimization for later, not doing it now
        % will make it easier to generate the code in the spawned off
        % computation (since it can have the same layout as the parent).

        NonLocals = goal_info_get_code_gen_nonlocals(GoalInfo0),
        % Include NonLocals as these need to be on the stack to communicate
        % with the spawned off context,
        % Include ResumeVars0 because this goal may be backtracked over,
        % (Except that loop control is only applied to deterministic
        % predicates and it would mean entering a recursive call more than
        % once. So I think that ResumeVars0 will always be empty.
        % The loop control variables must also be allocated stack slots.
        % Inclusion of !.Liveness is a conservative approximation. Values
        % in !.Liveness don't need to have stack slots but if they already
        % have stack slots then those slots most be distinct from others by
        % the spawn off scope. So what we want here is the intersection of
        % !.Liveness and AlreadyHasStackSlot.
        OuterParStackVars = !.ParStackVars,
        LCStackVars =
            set_of_var.union_list([NonLocals, !.Liveness, ResumeVars0])
            `set_of_var.union` list_to_set([LCVar, LCSVar]),
        par_stack_vars_start_loop_control(LCStackVars, !ParStackVars),
        build_live_sets_in_goal(AllocData, ResumeVars0, SubGoal0, SubGoal,
            !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars),
        par_stack_vars_get_stackvars(!.ParStackVars, InnerStackVars),

        NeedInParConjSet = LCStackVars `set_of_var.union` InnerStackVars,
        NeedInParConj = need_in_par_conj(NeedInParConjSet),
        record_par_conj(NeedInParConj, AllocData,
            GoalInfo0, GoalInfo, !StackAlloc),

        % NeedInParConjSet says live, any calls between now and the
        % recursive call must include this set in the set of stack variables.
        % XXX What does that mean?
        %
        % WouldDieSet are variables that would normally die if this were
        % not a parallel goal, but we only want them to die after the
        % recursive call.
        WouldDieSet = set_of_var.difference(NeedInParConjSet, !.Liveness),
        !:Liveness = set_of_var.union(!.Liveness, WouldDieSet),
        par_stack_vars_end_loop_control(WouldDieSet, OuterParStackVars,
            !ParStackVars),

        GoalExpr = scope(Reason, SubGoal)
    else
        NondetLiveness0 = !.NondetLiveness,
        build_live_sets_in_goal(AllocData, ResumeVars0, SubGoal0, SubGoal,
            !StackAlloc, !Liveness, !NondetLiveness, !ParStackVars),
        % If the "some" goal cannot succeed more than once, then execution
        % cannot backtrack into the inner goal once control has left it.
        % Therefore the code following the scope can reuse any stack slots
        % needed by nondet code in the inner goal.
        CodeModel = goal_info_get_code_model(GoalInfo0),
        (
            CodeModel = model_non
        ;
            ( CodeModel = model_det
            ; CodeModel = model_semi
            ),
            !:NondetLiveness = NondetLiveness0
        ),
        GoalExpr = scope(Reason, SubGoal),
        GoalInfo = GoalInfo0
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % If doing typeinfo liveness calculation, any typeinfos for output
    % variables or live variables are also live. This is because if you want
    % to examine the live data, you need to know what shape the polymorphic
    % args of the variables are, so you need the typeinfos to be present
    % on the stack.
    %
    % The live variables obviously need their typeinfos live, but the output
    % variables also need their typeinfos saved (otherwise we would throw out
    % typeinfos and might need one at a continuation point just after a call).
    %
    % maybe_add_typeinfo_liveness takes a set of vars (output vars) and a set
    % of live vars and if we are doing typeinfo liveness, adds the appropriate
    % typeinfo variables to the set of variables. If not, it returns the live
    % vars unchanged.
    %
    % Make sure you get the output vars first, and the live vars second,
    % since this makes a significant difference to the output set of vars.
    %
:- pred maybe_add_typeinfo_liveness(alloc_data::in, set_of_progvar::in,
    set_of_progvar::in, set_of_progvar::out) is det.

maybe_add_typeinfo_liveness(AllocData, OutVars, !LiveVars) :-
    TypeInfoLiveness = AllocData ^ ad_typeinfo_liveness,
    (
        TypeInfoLiveness = yes,
        ProcInfo = AllocData ^ ad_proc_info,
        proc_info_get_var_table(ProcInfo, VarTable),
        proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
        get_typeinfo_vars(VarTable, RttiVarMaps, !.LiveVars, TypeInfoVarsLive),
        get_typeinfo_vars(VarTable, RttiVarMaps, OutVars, TypeInfoVarsOut),
        set_of_var.union(!.LiveVars, TypeInfoVarsOut, !:LiveVars),
        set_of_var.union(!.LiveVars, TypeInfoVarsLive, !:LiveVars)
    ;
        TypeInfoLiveness = no
    ).

%---------------------------------------------------------------------------%

:- pred record_call_site(need_across_call::in, alloc_data::in,
    hlds_goal_info::in, hlds_goal_info::out, T::in, T::out) is det
    <= stack_alloc_info(T).

record_call_site(NeedAcrossCall, AllocData, !GoalInfo, !StackAlloc) :-
    goal_info_set_need_across_call(NeedAcrossCall, !GoalInfo),
    at_call_site(NeedAcrossCall, AllocData, !StackAlloc).

:- pred record_resume_site(need_in_resume::in, alloc_data::in,
    hlds_goal_info::in, hlds_goal_info::out, T::in, T::out) is det
    <= stack_alloc_info(T).

record_resume_site(NeedInResume, AllocData, !GoalInfo, !StackAlloc) :-
    goal_info_set_need_in_resume(NeedInResume, !GoalInfo),
    at_resume_site(NeedInResume, AllocData, !StackAlloc).

:- pred record_par_conj(need_in_par_conj::in, alloc_data::in,
    hlds_goal_info::in, hlds_goal_info::out, T::in, T::out) is det
    <= stack_alloc_info(T).

record_par_conj(NeedInParConj, AllocData, !GoalInfo, !StackAlloc) :-
    goal_info_set_need_in_par_conj(NeedInParConj, !GoalInfo),
    at_par_conj(NeedInParConj, AllocData, !StackAlloc).

%---------------------------------------------------------------------------%

    % Information about which variables in a parallel conjunction need stack
    % slots.
    %
:- type parallel_stackvars
    --->    not_in_parallel_context
    ;       parallel_conjunction(
                % Variables nonlocal to the parallel conjunction which need
                % their own stack slots.
                set_of_progvar,

                % Variables local to parallel conjuncts prior to the
                % current conjunct which need stack slots.
                list(set_of_progvar),

                % Accumulating set of variables local to the current
                % parallel conjunct which need stack slots.
                set_of_progvar
            )
    ;       in_loop_control_spawn_scope(
                % Variables nonlocal to the scope, these all need stack slots.
                % This field may be unnecessary since it's used to remove items
                % from sets when adding them to the set below. And then a
                % union of it and the set below is calculated to set
                % need_in_par_conj.
                set_of_progvar,

                % Accumulating set of variables local to the scope that need
                % stack slots, these are allocated on the parent's stack frame
                % (for now).
                set_of_progvar
            )
    ;       after_loop_control_spawn(
                % We are in a conjunction *after* a loop_control scope
                % that spawns off a computation to be done by another engine.

                % List of variables local to each of the previous loop control
                % scopes.
                %
                % XXX I (zs) do not understand what *exactly* the above
                % comment is supposed to mean. It is a hint, but it is not
                % a specification.
                %
                % This field is updated only by par_stack_vars_end_loop_control
                % which gets called when we finish processing each loop control
                % scope, i.e. each spawned-off goal. It contains the list of
                % the sets_of_var returned by par_stack_vars_get_stackvars
                % for the ParStackVars value at the end of the those loop
                % control scopes. These are in reverse order, with the first
                % set_of_var being for the spawned-off goal we processed
                % most recently.
                list(set_of_progvar),

                % The set of variables whose death we must delay until after
                % the recursive call, because they may still be using
                % their slots in our stack frame.
                set_of_progvar,

                % Accumulating set of variables that need stack slots between a
                % loop control scope and either another loop control scope or a
                % recursive call.
                %
                % XXX I (zs) do not understand what *exactly* the above
                % comment is supposed to mean. It is a hint, but it is not
                % a specification. The following says what code actually
                % updates this field.
                %
                % par_stack_vars_end_loop_control, which always constructs
                % the initial after_loop_control_spawn structure, initializes
                % this field to the empty set.
                %
                % par_stack_vars_record_call_stack_vars adds the variables
                % that we need to save on the stack across that call to the
                % value of this field.
                %
                % par_stack_vars_end_parallel_conjunction adds the StackVars
                % (returned by par_stack_vars_get_stackvars) of the inner
                % ParStackVars structure to this field of the outer
                % ParStackVars structure.
                set_of_progvar
            ).

:- pred empty_par_stackvars(parallel_stackvars::out) is det.

empty_par_stackvars(not_in_parallel_context).

:- pred par_stack_vars_start_parallel_conjunction(set_of_progvar::in,
    parallel_stackvars::in, parallel_stackvars::out) is det.

par_stack_vars_start_parallel_conjunction(LiveSet,
        OuterParStackVars, InnerParStackVars) :-
    par_stack_vars_get_nonlocals(OuterParStackVars, OuterNonLocals),
    InnerNonLocals = OuterNonLocals `set_of_var.union` LiveSet,
    CurConjunctVars = set_of_var.init,
    InnerParStackVars = parallel_conjunction(InnerNonLocals, [],
        CurConjunctVars).

:- pred par_stack_vars_end_parallel_conjunction(set_of_progvar::in,
    parallel_stackvars::in, parallel_stackvars::in, parallel_stackvars::out)
    is det.

par_stack_vars_end_parallel_conjunction(LiveSet, OuterParStackVars,
        ParStackVars0, ParStackVars) :-
    par_stack_vars_get_stackvars(ParStackVars0, InnerStackVars),
    (
        OuterParStackVars = not_in_parallel_context,
        ParStackVars = not_in_parallel_context
    ;
        OuterParStackVars = parallel_conjunction(OuterNonLocals,
            OuterLocalStackVars, OuterAccStackVars0),
        % All the local variables which needed stack slots in the parallel
        % conjuncts (InnerStackVars) become part of the accumulating set of
        % variables that have stack slots. Variables which are not local to
        % but are needed in the parallel conjunctions also become part of the
        % accumulating set.
        OuterAccStackVars = OuterAccStackVars0
            `set_of_var.union` InnerStackVars
            `set_of_var.union`
                (LiveSet `set_of_var.difference` OuterNonLocals),
        ParStackVars = parallel_conjunction(OuterNonLocals,
            OuterLocalStackVars, OuterAccStackVars)
    ;
        OuterParStackVars = in_loop_control_spawn_scope(OuterNonLocals,
            StackVars0),
        % The loop control scope must ensure that any stackvars needed by a
        % parallel conjunction are distinct from those already needed by loop
        % control. The same is true in the case for
        % after_loop_control_spawn/2 below.
        StackVars = StackVars0 `set_of_var.union` InnerStackVars
            `set_of_var.union`
                (LiveSet `set_of_var.difference` OuterNonLocals),
        ParStackVars = in_loop_control_spawn_scope(OuterNonLocals, StackVars)
    ;
        OuterParStackVars = after_loop_control_spawn(StackVarsList,
            WouldDieSet, StackVars0),
        % In this case we don't have access to an OuterNonLocals set, so this
        % is a conservative approximation.
        StackVars = StackVars0 `set_of_var.union` InnerStackVars
            `set_of_var.union` LiveSet,
        ParStackVars = after_loop_control_spawn(StackVarsList,
            WouldDieSet, StackVars)
    ).

:- pred par_stack_vars_start_loop_control(set_of_progvar::in,
    parallel_stackvars::in, parallel_stackvars::out) is det.

par_stack_vars_start_loop_control(NonLocals, ParStackVars0, ParStackVars) :-
    (
        ( ParStackVars0 = not_in_parallel_context
        ; ParStackVars0 = after_loop_control_spawn(_, _, _)
        ),
        ParStackVars = in_loop_control_spawn_scope(NonLocals, set_of_var.init)
    ;
        ( ParStackVars0 = parallel_conjunction(_, _, _)
        ; ParStackVars0 = in_loop_control_spawn_scope(_, _)
        ),
        unexpected($pred, "Loop control scope found in other parallel context")
    ).

:- pred par_stack_vars_end_loop_control(set_of_progvar::in,
    parallel_stackvars::in,
    parallel_stackvars::in, parallel_stackvars::out) is det.

par_stack_vars_end_loop_control(NewWouldDieSet, OldParStackVars,
        ParStackVars0, ParStackVars) :-
    par_stack_vars_get_stackvars(ParStackVars0, NewStackVars),
    (
        OldParStackVars = not_in_parallel_context,
        ParStackVars = after_loop_control_spawn([NewStackVars], NewWouldDieSet,
            set_of_var.init)
    ;
        OldParStackVars = after_loop_control_spawn(StackVarsList0,
            WouldDieSet0, StackVarsAcc),
        StackVarsList = [NewStackVars | StackVarsList0],
        WouldDieSet = WouldDieSet0 `set_of_var.union` NewWouldDieSet,
        ParStackVars = after_loop_control_spawn(StackVarsList,
            WouldDieSet, StackVarsAcc)
    ;
        ( OldParStackVars = parallel_conjunction(_, _, _)
        ; OldParStackVars = in_loop_control_spawn_scope(_, _)
        ),
        unexpected($pred, "Loop control scope found in other parallel context")
    ).

:- pred par_stack_vars_get_stackvars(parallel_stackvars::in,
    set_of_progvar::out) is det.

par_stack_vars_get_stackvars(ParStackVars, StackVars) :-
    (
        ParStackVars = not_in_parallel_context,
        StackVars = set_of_var.init
    ;
        ParStackVars = parallel_conjunction(_, PrevConjVarss, _),
        StackVars = set_of_var.union_list(PrevConjVarss)
    ;
        ParStackVars = in_loop_control_spawn_scope(_, StackVars)
    ;
        ParStackVars = after_loop_control_spawn(_, _, StackVars)
    ).

:- pred par_stack_vars_record_call_stack_vars(set_of_progvar::in,
    parallel_stackvars::in, parallel_stackvars::out) is det.

par_stack_vars_record_call_stack_vars(ForwardVars, !ParStackVars) :-
    (
        !.ParStackVars = not_in_parallel_context
    ;
        !.ParStackVars = parallel_conjunction(Nonlocals, PrevConjVarss,
            AccStackVars0),
        AccStackVars = AccStackVars0 `set_of_var.union`
            (ForwardVars `set_of_var.difference` Nonlocals),
        !:ParStackVars = parallel_conjunction(Nonlocals, PrevConjVarss,
            AccStackVars)
    ;
        !.ParStackVars = in_loop_control_spawn_scope(NonLocals, AccStackVars0),
        AccStackVars = AccStackVars0 `set_of_var.union` ForwardVars,
        !:ParStackVars = in_loop_control_spawn_scope(NonLocals, AccStackVars)
    ;
        !.ParStackVars = after_loop_control_spawn(LocalStackVars, WouldDieSet,
            AccStackVars0),
        AccStackVars = AccStackVars0 `set_of_var.union` ForwardVars,
        !:ParStackVars = after_loop_control_spawn(LocalStackVars, WouldDieSet,
            AccStackVars)
    ).

:- pred par_stack_vars_get_nonlocals(parallel_stackvars::in,
    set_of_progvar::out) is det.

par_stack_vars_get_nonlocals(ParStackVars, NonLocals) :-
    (
        ( ParStackVars = not_in_parallel_context
        ; ParStackVars = after_loop_control_spawn(_, _, _)
        ),
        NonLocals = set_of_var.init
    ;
        ParStackVars = parallel_conjunction(NonLocals, _, _)
    ;
        ParStackVars = in_loop_control_spawn_scope(NonLocals, _)
    ).

:- pred par_stack_vars_next_par_conjunct(
    parallel_stackvars::in, parallel_stackvars::out) is det.

par_stack_vars_next_par_conjunct(!ParStackVars) :-
    (
        !.ParStackVars = parallel_conjunction(Nonlocals, PrevSets, CurSet),
        !:ParStackVars = parallel_conjunction(Nonlocals, [CurSet | PrevSets],
            set_of_var.init)
    ;
        ( !.ParStackVars = not_in_parallel_context
        ; !.ParStackVars = in_loop_control_spawn_scope(_, _)
        ; !.ParStackVars = after_loop_control_spawn(_, _, _)
        ),
        unexpected($pred, "expected parallel_conjunction/3")
    ).

:- pred par_stack_vars_recursive_call(maybe(need_for_loop_control)::out,
    set_of_progvar::out, parallel_stackvars::in, parallel_stackvars::out)
    is det.

par_stack_vars_recursive_call(MaybeNeedLC, DelayDeathSet, !ParStackVars) :-
    (
        ( !.ParStackVars = not_in_parallel_context
        ; !.ParStackVars = parallel_conjunction(_, _, _)
        ),
        MaybeNeedLC = no,
        DelayDeathSet = set_of_var.init
    ;
        !.ParStackVars = in_loop_control_spawn_scope(_, _),
        unexpected($pred, "recursive call in loop control scope")
    ;
        !.ParStackVars = after_loop_control_spawn(StackVarsList0,
            DelayDeathSet, StackVars),
        cartesian_product_list(StackVars, StackVarsList0, NonoverlapSets),
        MaybeNeedLC = yes(need_for_loop_control(NonoverlapSets)),
        !:ParStackVars = not_in_parallel_context
    ).

%---------------------------------------------------------------------------%

    % XXX This predicate has exactly one caller above, in the code that
    % constructs NonoverlapSets for a parallel conjunction goal.
    % It was added by Paul in a commit in October 2011. That commit
    % was not reviewed by anyone, because it was posted to m-rev as a diff
    % without a request for a review. I (zs) think now (in January 2025)
    % that it only ever works only by accident.
    %
    % To illustrate the reason why I think that, consider the input of
    % this predicate consisting of four sets, SetA, SetB, SetC and SetD.
    % Given the code just before the only call, SetA will represent
    % the variables used by the code after the last spawned-off goal,
    % which SetB, SetC and SetC will represent the variables used by
    % spawned-off goals. cartesian_product_list will call its auxiliary
    % predicate cartesian_product_list_2 with the non-accumulator input
    % arguments being the pairs <SetA, SetB>, <SetA, SetC> and <SetA, SetD>.
    % It will accumulate all the pairs that cartesian_product returns
    % for each of these pairs, which would protect against any stack slot
    % collisions between, the code after the last spawned-off goal
    % on the one hand, and each spawned-off goal on the other hand.
    % However, this but it will NOT protect against stack slot collisions
    % between two different spawned-off goals.
    %
:- pred cartesian_product_list(set_of_progvar::in, list(set_of_progvar)::in,
    list(set_of_progvar)::out) is det.

cartesian_product_list(FirstSet, OtherSets, Product) :-
    list.foldl(cartesian_product_list_2(FirstSet), OtherSets, [], Product).

:- pred cartesian_product_list_2(set_of_progvar::in, set_of_progvar::in,
    list(set_of_progvar)::in, list(set_of_progvar)::out) is det.

cartesian_product_list_2(SetX, SetY, SetsAcc, SetXYPairs ++ SetsAcc) :-
    cartesian_product(SetX, SetY, SetXYPairs).

%---------------------%

    % For two sets SetA and SetB, this predicate returns the list of
    % all possible pairs between one element of SetA and one element of SetB.
    %
    % I (zs) believe that the intent of this code is to require
    % the allocation of disjoint sets of stack slots to the two input
    % sets of variables, while still allowing the reuse of stack slots
    % as long as all the variables allocated to a given stack slot
    % are whole within one of the input sets or the other (not both).
    % (Just taking the union of the two input sets would accomplish
    % the first objective, but not the second.)
    %
    % The price of providing this flexibility is that the output
    % will contain |SetA| * |SetB| pairs. This is really bad worst-case
    % behavior. It is possible that we could avoid that by doing graph
    % colouring for each spawned-off goal separately from both other
    % spawned-off goals and from the surrounding goal, and then merging
    % the resulting sets of colors together into a stack slot assignment.
    % However, this would require considerably more complex code.
    %
:- pred cartesian_product(set_of_var(T)::in, set_of_var(T)::in,
    list(set_of_var(T))::out) is det.

cartesian_product(SetA, SetB, Product) :-
    set_of_var.fold(cartesian_product_loop_over_a(SetA), SetB, [], Product).

:- pred cartesian_product_loop_over_a(set_of_var(T)::in, var(T)::in,
    list(set_of_var(T))::in, list(set_of_var(T))::out) is det.

cartesian_product_loop_over_a(SetA, VarB, !PairsAB) :-
    PairEachAWithB =
        ( pred(VarA::in, Pairs0::in, Pairs1::out) is det :-
            Pair = set_of_var.list_to_set([VarA, VarB]),
            Pairs1 = [Pair | Pairs0]
        ),
    set_of_var.fold(PairEachAWithB, SetA, !PairsAB).

%---------------------------------------------------------------------------%
:- end_module ll_backend.live_vars.
%---------------------------------------------------------------------------%
