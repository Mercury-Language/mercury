%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: live_vars.m.
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

:- module ll_backend.live_vars.
:- interface.

% Parse tree modules
:- import_module parse_tree.prog_data.

% HLDS modules
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

% Standard library modules
:- import_module bool.
:- import_module set.

:- type alloc_data
    --->    alloc_data(
                module_info         ::  module_info,
                proc_info           ::  proc_info,
                typeinfo_liveness   ::  bool,
                opt_no_return_calls ::  bool
            ).

:- typeclass stack_alloc_info(T) where [
    pred at_call_site(need_across_call::in, hlds_goal_info::in,
        T::in, T::out) is det,
    pred at_resume_site(need_in_resume::in, hlds_goal_info::in,
        T::in, T::out) is det,
    pred at_par_conj(need_in_par_conj::in, hlds_goal_info::in,
        T::in, T::out) is det
].

:- pred build_live_sets_in_goal(hlds_goal::in, hlds_goal::out,
    set(prog_var)::in, alloc_data::in, T::in, T::out,
    set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is det <= stack_alloc_info(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.arg_info.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module libs.compiler_util.
:- import_module ll_backend.liveness.
:- import_module ll_backend.llds.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.

%-----------------------------------------------------------------------------%

% The stack_slots structure (map(prog_var, lval)) is threaded through the
% traversal of the goal. The liveness information is computed from the liveness
% delta annotations.

build_live_sets_in_goal(Goal0 - GoalInfo0, Goal - GoalInfo, ResumeVars0,
        AllocData, !StackAlloc, !Liveness, !NondetLiveness) :-
    goal_info_get_pre_deaths(GoalInfo0, PreDeaths),
    goal_info_get_pre_births(GoalInfo0, PreBirths),
    goal_info_get_post_deaths(GoalInfo0, PostDeaths),
    goal_info_get_post_births(GoalInfo0, PostBirths),

    % note: we must be careful to apply deaths before births
    set.difference(!.Liveness, PreDeaths, !:Liveness),
    set.union(!.Liveness, PreBirths, !:Liveness),

    % If the goal is atomic, we want to apply the postdeaths before processing
    % the goal, but if the goal is a compound goal, then we want to apply them
    % after processing it.
    ( goal_is_atomic(Goal0) ->
        set.difference(!.Liveness, PostDeaths, !:Liveness)
    ;
        true
    ),

    goal_info_get_resume_point(GoalInfo0, ResumePoint),
    (
        ResumePoint = no_resume_point,
        ResumeVars1 = ResumeVars0,
        GoalInfo1 = GoalInfo0
    ;
        ResumePoint = resume_point(ResumePointVars, Locs),
        ( resume_locs_include_stack(Locs, yes) ->
            set.union(ResumeVars0, ResumePointVars, ResumeVars1),
            ResumeOnStack = yes
        ;
            ResumeVars1 = ResumeVars0,
            ResumeOnStack = no
        ),
        NeedInResume = need_in_resume(ResumeOnStack, ResumeVars1,
            !.NondetLiveness),
        record_resume_site(NeedInResume, GoalInfo0, GoalInfo1, !StackAlloc)
    ),

    build_live_sets_in_goal_2(Goal0, Goal, GoalInfo1, GoalInfo, ResumeVars1,
        AllocData, !StackAlloc, !Liveness, !NondetLiveness),

    ( goal_is_atomic(Goal0) ->
        true
    ;
        set.difference(!.Liveness, PostDeaths, !:Liveness)
    ),

    set.union(!.Liveness, PostBirths, !:Liveness).

:- pred resume_locs_include_stack(resume_locs::in, bool::out) is det.

resume_locs_include_stack(orig_only, no).
resume_locs_include_stack(stack_only, yes).
resume_locs_include_stack(orig_and_stack, yes).
resume_locs_include_stack(stack_and_orig, yes).

%-----------------------------------------------------------------------------%

    % Here we process each of the different sorts of goals. `Liveness' is the
    % set of live variables, i.e. vars which have been referenced and may be
    % referenced again (during forward execution). `ResumeVars' is the set
    % of variables that may or may not be `live' during the current forward
    % execution but will become live again on backtracking. `StackAlloc' is the
    % interference graph, i.e. the set of sets of variables which need to be
    % on the stack at the same time.
    %
:- pred build_live_sets_in_goal_2(hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    set(prog_var)::in, alloc_data::in, T::in, T::out,
    set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is det <= stack_alloc_info(T).

build_live_sets_in_goal_2(conj(ConjType, Goals0), conj(ConjType, Goals),
        GoalInfo0, GoalInfo, ResumeVars0, AllocData,
        !StackAlloc, !Liveness, !NondetLiveness) :-
    (
        ConjType = plain_conj,
        GoalInfo = GoalInfo0,
        build_live_sets_in_conj(Goals0, Goals, ResumeVars0, AllocData,
            !StackAlloc, !Liveness, !NondetLiveness)
    ;
        ConjType = parallel_conj,
        goal_info_get_code_gen_nonlocals(GoalInfo0, NonLocals),
        set.union(NonLocals, !.Liveness, LiveSet),
        % Since each parallel conjunct may be run on a different Mercury engine
        % to the current engine, we must save all the variables that are live
        % or nonlocal to the parallel conjunction. Nonlocal variables that are
        % currently free, but are bound inside one of the conjuncts need a
        % stackslot because they are passed out by reference to that stackslot.
        NeedInParConj = need_in_par_conj(LiveSet),
        record_par_conj(NeedInParConj, GoalInfo0, GoalInfo, !StackAlloc),
        build_live_sets_in_par_conj(Goals0, Goals, ResumeVars0, AllocData,
            !StackAlloc, !.Liveness, !Liveness, !NondetLiveness)
    ).

build_live_sets_in_goal_2(disj(Goals0), disj(Goals), GoalInfo, GoalInfo,
        ResumeVars0, AllocData, !StackAlloc, !Liveness, !NondetLiveness) :-
    build_live_sets_in_disj(Goals0, Goals, GoalInfo, ResumeVars0, AllocData,
        !StackAlloc, !Liveness, !NondetLiveness),
    (
        Goals = [First | _],
        First = _ - FirstGoalInfo,
        goal_info_get_resume_point(FirstGoalInfo, ResumePoint),
        (
            ResumePoint = resume_point(ResumeVars, _Locs),
            % If we can backtrack into the disjunction, we must protect the
            % stack slots needed by any of its resumption points from being
            % reused in the following code. The first resumption point's vars
            % include all the vars needed by all the resumption points.
            % However, the first disjunct can be orig_only while later
            % disjuncts are include the stack.

            % Note that we must check the disjunction's code model, not any
            % disjuncts'; the disjunction as a whole can be model_non
            % without any disjunct being model_non.
            (
                goal_info_get_code_model(GoalInfo, model_non),
                some [Disjunct] (
                    list.member(Disjunct, Goals),
                    Disjunct = _ - DisjunctGoalInfo,
                    goal_info_get_resume_point(DisjunctGoalInfo,
                        DisjunctResumePoint),
                    DisjunctResumePoint = resume_point(_, Locs),
                    resume_locs_include_stack(Locs, yes)
                )
            ->
                set.union(!.NondetLiveness, ResumeVars, !:NondetLiveness)
            ;
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
    ).

build_live_sets_in_goal_2(switch(Var, CanFail, Cases0),
        switch(Var, CanFail, Cases), GoalInfo, GoalInfo, ResumeVars0,
        AllocData, !StackAlloc, !Liveness, !NondetLiveness) :-
    build_live_sets_in_cases(Cases0, Cases, ResumeVars0, AllocData,
        !StackAlloc, !Liveness, !NondetLiveness).

build_live_sets_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0),
        if_then_else(Vars, Cond, Then, Else), GoalInfo, GoalInfo,
        ResumeVars0, AllocData, !StackAlloc,
        Liveness0, Liveness, NondetLiveness0, NondetLiveness) :-
    build_live_sets_in_goal(Cond0, Cond, ResumeVars0, AllocData, !StackAlloc,
        Liveness0, LivenessCond, NondetLiveness0, NondetLivenessCond),
    build_live_sets_in_goal(Then0, Then, ResumeVars0, AllocData, !StackAlloc,
        LivenessCond, _LivenessThen, NondetLivenessCond, NondetLivenessThen),
    build_live_sets_in_goal(Else0, Else, ResumeVars0, AllocData, !StackAlloc,
        Liveness0, Liveness, NondetLiveness0, NondetLivenessElse),
    set.union(NondetLivenessThen, NondetLivenessElse, NondetLiveness).

build_live_sets_in_goal_2(negation(Goal0), negation(Goal), GoalInfo, GoalInfo,
        ResumeVars0, AllocData, !StackAlloc, !Liveness, !NondetLiveness) :-
    build_live_sets_in_goal(Goal0, Goal, ResumeVars0, AllocData,
        !StackAlloc, !Liveness, !NondetLiveness).

build_live_sets_in_goal_2(scope(Reason, Goal0), scope(Reason, Goal),
        GoalInfo, GoalInfo, ResumeVars0, AllocData,
        !StackAlloc, !Liveness, !NondetLiveness) :-
    NondetLiveness0 = !.NondetLiveness,
    build_live_sets_in_goal(Goal0, Goal, ResumeVars0, AllocData,
        !StackAlloc, !Liveness, !NondetLiveness),

    % If the "some" goal cannot succeed more than once, then execution cannot
    % backtrack into the inner goal once control has left it. Therefore the
    % code following the "some" can reuse any stack slots needed by nondet
    % code in the inner goal.

    goal_info_get_code_model(GoalInfo, CodeModel),
    ( CodeModel = model_non ->
        true
    ;
        !:NondetLiveness = NondetLiveness0
    ).

build_live_sets_in_goal_2(Goal, Goal, GoalInfo0, GoalInfo, ResumeVars0,
        AllocData, !StackAlloc, !Liveness, !NondetLiveness) :-
    Goal = generic_call(GenericCall, ArgVars, Modes, _Det),
    ( GenericCall = cast(_) ->
        GoalInfo = GoalInfo0
    ;
        ProcInfo = AllocData ^ proc_info,
        proc_info_get_vartypes(ProcInfo, VarTypes),
        map.apply_to_list(ArgVars, VarTypes, Types),
        ModuleInfo = AllocData ^ module_info,
        arg_info.partition_generic_call_args(ModuleInfo, ArgVars,
            Types, Modes, _InVars, OutVars, _UnusedVars),
        build_live_sets_in_call(OutVars, GoalInfo0, GoalInfo, ResumeVars0,
            AllocData, !StackAlloc, !.Liveness, !NondetLiveness)
    ).

build_live_sets_in_goal_2(Goal, Goal, GoalInfo0, GoalInfo, ResumeVars0,
        AllocData, !StackAlloc, !Liveness, !NondetLiveness) :-
    Goal = plain_call(PredId, ProcId, ArgVars, Builtin, _, _),
    ModuleInfo = AllocData ^ module_info,
    CallerProcInfo = AllocData ^ proc_info,
    proc_info_get_vartypes(CallerProcInfo, VarTypes),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    arg_info.partition_proc_call_args(ProcInfo, VarTypes, ModuleInfo,
        ArgVars, _InVars, OutVars, _UnusedVars),
    ( Builtin = inline_builtin ->
        GoalInfo = GoalInfo0
    ;
        build_live_sets_in_call(OutVars, GoalInfo0, GoalInfo,
            ResumeVars0, AllocData, !StackAlloc, !.Liveness, !NondetLiveness)
    ).

build_live_sets_in_goal_2(Goal, Goal, GoalInfo, GoalInfo,
        _ResumeVars0, _AllocData, !StackAlloc, !Liveness, !NondetLiveness) :-
    Goal = unify(_, _, _, Unification, _),
    ( Unification = complicated_unify(_, _, _) ->
        unexpected(this_file, "build_live_sets_in_goal_2: complicated_unify")
    ;
        true
    ).

build_live_sets_in_goal_2(Goal, Goal, GoalInfo0, GoalInfo, ResumeVars0,
        AllocData, !StackAlloc, !Liveness, !NondetLiveness) :-
    Goal = call_foreign_proc(Attributes, PredId, ProcId, Args, _, _, _),
    ModuleInfo = AllocData ^ module_info,
    CallerProcInfo = AllocData ^ proc_info,
    proc_info_get_vartypes(CallerProcInfo, VarTypes),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    ArgVars = list.map(foreign_arg_var, Args),
    arg_info.partition_proc_call_args(ProcInfo, VarTypes, ModuleInfo,
        ArgVars, _InVars, OutVars, _UnusedVars),
    goal_info_get_code_model(GoalInfo0, CodeModel),
    (
        % We don't need to save any variables onto the stack before a
        % foreign_proc if we know that it can't succeed more than once
        % and that it is not going to call back Mercury code, because
        % such pragma C code won't clobber the registers.

        CodeModel \= model_non,
        may_call_mercury(Attributes) = will_not_call_mercury
    ->
        GoalInfo = GoalInfo0
    ;
        % The variables which need to be saved onto the stack before the call
        % are all the variables that are live after the call (except for the
        % output arguments produced by the call), plus all the variables
        % that may be needed at an enclosing resumption point.

        build_live_sets_in_call(OutVars, GoalInfo0, GoalInfo,
            ResumeVars0, AllocData, !StackAlloc, !.Liveness, !NondetLiveness)
    ).

build_live_sets_in_goal_2(shorthand(_), _,_,_,_,_,_,_,_,_,_,_) :-
    % these should have been expanded out by now
    unexpected(this_file, "build_live_sets_in_goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

    % The variables which need to be saved onto the stack, directly or
    % indirectly, before a call or may_call_mercury foreign_proc are all
    % the variables that are live after the goal except for the output
    % arguments produced by the goal, plus all the variables that may be
    % needed at an enclosing resumption point.
    %
:- pred build_live_sets_in_call(set(prog_var)::in, hlds_goal_info::in,
    hlds_goal_info::out, set(prog_var)::in, alloc_data::in, T::in, T::out,
    set(prog_var)::in, set(prog_var)::in, set(prog_var)::out) is det
    <= stack_alloc_info(T).

build_live_sets_in_call(OutVars, GoalInfo0, GoalInfo, ResumeVars0, AllocData,
        !StackAlloc, Liveness, !NondetLiveness) :-

    set.difference(Liveness, OutVars, ForwardVars0),

    % Might need to add more live variables with typeinfo liveness
    % calculation.

    maybe_add_typeinfo_liveness(AllocData ^ proc_info,
        AllocData ^ typeinfo_liveness, OutVars, ForwardVars0, ForwardVars),

    goal_info_get_determinism(GoalInfo0, Detism),
    (
        Detism = detism_erroneous,
        AllocData ^ opt_no_return_calls = yes
    ->
        NeedAcrossCall = need_across_call(set.init, set.init, set.init)
    ;
        NeedAcrossCall = need_across_call(ForwardVars, ResumeVars0,
            !.NondetLiveness)
    ),

    record_call_site(NeedAcrossCall, GoalInfo0, GoalInfo, !StackAlloc),

    % If this is a nondet call, then all the stack slots we need
    % must be protected against reuse in following code.

    goal_info_get_code_model(GoalInfo, CodeModel),
    ( CodeModel = model_non ->
        set.union(!.NondetLiveness, ForwardVars, !:NondetLiveness)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred build_live_sets_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    set(prog_var)::in, alloc_data::in, T::in, T::out,
    set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is det <= stack_alloc_info(T).

build_live_sets_in_conj([], [], _, _, !StackAlloc, !Liveness, !NondetLiveness).
build_live_sets_in_conj([Goal0 | Goals0], [Goal | Goals], ResumeVars0,
        AllocData, !StackAlloc, !Liveness, !NondetLiveness) :-
    (
        Goal0 = _ - GoalInfo,
        goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
        instmap_delta_is_unreachable(InstMapDelta)
    ->
        build_live_sets_in_goal(Goal0, Goal, ResumeVars0, AllocData,
            !StackAlloc, !Liveness, !NondetLiveness),
        Goals = [] % XXX was Goals = Goal0
    ;
        build_live_sets_in_goal(Goal0, Goal, ResumeVars0, AllocData,
            !StackAlloc, !Liveness, !NondetLiveness),
        build_live_sets_in_conj(Goals0, Goals, ResumeVars0, AllocData,
            !StackAlloc, !Liveness, !NondetLiveness)
    ).

%-----------------------------------------------------------------------------%

:- pred build_live_sets_in_par_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    set(prog_var)::in, alloc_data::in, T::in, T::out,
    set(prog_var)::in, set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is det <= stack_alloc_info(T).

build_live_sets_in_par_conj([], [], _, _,
        !StackAlloc, _Liveness0, !Liveness, !NondetLiveness).
build_live_sets_in_par_conj([Goal0 | Goals0], [Goal | Goals], ResumeVars0,
        AllocData, !StackAlloc, Liveness0, !Liveness, !NondetLiveness) :-
    build_live_sets_in_goal(Goal0, Goal, ResumeVars0, AllocData,
        !StackAlloc, Liveness0, Liveness1, !NondetLiveness),
    set.union(Liveness1, !Liveness),
    build_live_sets_in_par_conj(Goals0, Goals, ResumeVars0, AllocData,
        !StackAlloc, Liveness0, !Liveness, !NondetLiveness).

%-----------------------------------------------------------------------------%

:- pred build_live_sets_in_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    hlds_goal_info::in, set(prog_var)::in, alloc_data::in,
    T::in, T::out, set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is det <= stack_alloc_info(T).

build_live_sets_in_disj([], [], _, _, _,
        !StackAlloc, !Liveness, !NondetLiveness).
build_live_sets_in_disj([Goal0 | Goals0], [Goal | Goals],
        DisjGoalInfo, ResumeVars0, AllocData, !StackAlloc,
        Liveness0, Liveness, NondetLiveness0, NondetLiveness) :-
    Goal = _ - GoalInfo,
    build_live_sets_in_goal(Goal0, Goal, ResumeVars0, AllocData,
        !StackAlloc, Liveness0, Liveness,
        NondetLiveness0, NondetLiveness1),
    build_live_sets_in_disj(Goals0, Goals, DisjGoalInfo, ResumeVars0,
        AllocData, !StackAlloc, Liveness0, _Liveness2,
        NondetLiveness0, NondetLiveness2),
    goal_info_get_code_model(DisjGoalInfo, DisjCodeModel),
    ( DisjCodeModel = model_non ->
        % NondetLiveness should be a set of prog_var sets. Instead of taking
        % the union of the NondetLive sets at the ends of disjuncts, we should
        % just keep them in this set of sets.
        set.union(NondetLiveness1, NondetLiveness2, NondetLiveness3),
        goal_info_get_resume_point(GoalInfo, Resume),
        (
            Resume = resume_point(ResumePointVars, Locs),
            resume_locs_include_stack(Locs, yes)
        ->
            set.union(NondetLiveness3, ResumePointVars, NondetLiveness)
        ;
            NondetLiveness = NondetLiveness3
        )
    ;
        NondetLiveness = NondetLiveness0
    ).

%-----------------------------------------------------------------------------%

:- pred build_live_sets_in_cases(list(case)::in, list(case)::out,
    set(prog_var)::in, alloc_data::in, T::in, T::out,
    set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is det <= stack_alloc_info(T).

build_live_sets_in_cases([], [], _, _,
        !StackAlloc, !Liveness, !NondetLiveness).
build_live_sets_in_cases([case(Cons, Goal0) | Cases0],
        [case(Cons, Goal) | Cases], ResumeVars0, AllocData,
        !StackAlloc, Liveness0, Liveness, NondetLiveness0, NondetLiveness) :-
    build_live_sets_in_goal(Goal0, Goal, ResumeVars0, AllocData,
        !StackAlloc, Liveness0, Liveness, NondetLiveness0, NondetLiveness1),
    build_live_sets_in_cases(Cases0, Cases, ResumeVars0, AllocData,
        !StackAlloc, Liveness0, _Liveness2, NondetLiveness0, NondetLiveness2),
    set.union(NondetLiveness1, NondetLiveness2, NondetLiveness).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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
:- pred maybe_add_typeinfo_liveness(proc_info::in, bool::in,
    set(prog_var)::in, set(prog_var)::in, set(prog_var)::out) is det.

maybe_add_typeinfo_liveness(ProcInfo, TypeInfoLiveness, OutVars, !LiveVars) :-
    (
        TypeInfoLiveness = yes,
        proc_info_get_vartypes(ProcInfo, VarTypes),
        proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
        get_typeinfo_vars(!.LiveVars, VarTypes, RttiVarMaps, TypeInfoVarsLive),
        get_typeinfo_vars(OutVars, VarTypes, RttiVarMaps, TypeInfoVarsOut),
        set.union(!.LiveVars, TypeInfoVarsOut, !:LiveVars),
        set.union(!.LiveVars, TypeInfoVarsLive, !:LiveVars)
    ;
        TypeInfoLiveness = no
    ).

%-----------------------------------------------------------------------------%

:- pred record_call_site(need_across_call::in, hlds_goal_info::in,
    hlds_goal_info::out, T::in, T::out) is det <= stack_alloc_info(T).

record_call_site(NeedAcrossCall, !GoalInfo, !StackAlloc) :-
    goal_info_set_need_across_call(NeedAcrossCall, !GoalInfo),
    at_call_site(NeedAcrossCall, !.GoalInfo, !StackAlloc).

:- pred record_resume_site(need_in_resume::in, hlds_goal_info::in,
    hlds_goal_info::out, T::in, T::out) is det <= stack_alloc_info(T).

record_resume_site(NeedInResume, !GoalInfo, !StackAlloc) :-
    goal_info_set_need_in_resume(NeedInResume, !GoalInfo),
    at_resume_site(NeedInResume, !.GoalInfo, !StackAlloc).

:- pred record_par_conj(need_in_par_conj::in, hlds_goal_info::in,
    hlds_goal_info::out, T::in, T::out) is det <= stack_alloc_info(T).

record_par_conj(NeedInParConj, !GoalInfo, !StackAlloc) :-
    goal_info_set_need_in_par_conj(NeedInParConj, !GoalInfo),
    at_par_conj(NeedInParConj, !.GoalInfo, !StackAlloc).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "live_vars.m".

%-----------------------------------------------------------------------------%
