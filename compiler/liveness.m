%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: liveness.m.
% Main authors: conway, zs, trd.
%
% This module traverses the goal for each procedure, and adds liveness
% annotations to the goal_info for each sub-goal. These annotations are the
% pre-birth set, the post-birth set, the pre-death set, the post-death set,
% and the resume_point field.
%
% Because it recomputes each of these annotations from scratch, it is safe to
% call this module multiple times, and in fact we do so if stack slot
% optimization is enabled.
%
% NOTE: the concept of `liveness' here is different to that used in mode
% analysis. Mode analysis is concerned with the liveness of what is *pointed*
% to by a variable, for the purpose of avoiding and/or keeping track of
% aliasing and for structure re-use optimization, whereas here we are
% concerned with the liveness of the variable itself, for the purposes of
% optimizing stack slot and register usage. Variables have a lifetime: each
% variable is born, gets used, and then dies. To minimize stack slot and
% register usage, the birth should be as late as possible (but before the
% first possible use), and the death should be as early as possible (but after
% the last possible use).
%
% We compute liveness related information in four distinct passes.
%
% The first pass, detect_liveness_in_goal, finds the first value-giving
% occurrence of each variable on each computation path. Goals containing the
% first such occurrence of a variable include that variable in their pre-birth
% set. In branched structures, branches whose endpoint is not reachable
% include a post-birth set listing the variables that should have been born in
% that branch but haven't. Variables that shouldn't have been born but have
% been (in computation paths that cannot succeed) are included in the
% post-death set of the goal concerned.
%
% The second pass, detect_deadness_in_goal, finds the last occurrence of each
% variable on each computation path. Goals containing the last occurrence of a
% variable include that variable in their post-death set. In branched
% structures, branches in which a variable is not used at all include a
% pre-death set listing the variables that have died in parallel branches.
% Branches whose end-points are unreachable are handled specially; see the
% comment before union_branch_deadness for details.
%
% The third pass is optional: it delays the deaths of named variables until
% the last possible moment. This can be useful if debugging is enabled, as it
% allows the programmer to look at the values of such variables at as many
% trace events as possible. If debugging is not enabled, this pass is a pure
% pessimization (it increases stack slot pressure and thus probably increases
% the size of the procedure's stack frame), and therefore should not be
% enabled.
%
% The second and third passes cannot be combined, because the second pass
% traverses goals backwards while the third pass traverses goals forwards.
% (The second pass does propagate liveness forwards, but it does so only along
% one branch of every branched control structure.)
%
% The fourth pass, detect_resume_points_in_goal, finds goals that establish
% resume points and attaches to them a resume_point annotation listing the
% variables that may be referenced by the code at that resume point as well as
% the nature of the required entry labels.
%
% Typeinfo liveness calculation notes:
%
% When using accurate gc or execution tracing, liveness is computed slightly
% differently. The runtime system needs access to the typeinfo variables of
% any variable that is live at a continuation or event. (This includes
% typeclass info variables that hold typeinfos; in the following "typeinfo
% variables" also includes typeclass info variables.)
%
% Hence, the invariant needed for typeinfo-liveness calculation:
%   a variable holding a typeinfo must be live at any continuation
%   where any variable whose type is described (in whole or in part)
%   by that typeinfo is live.
%
% Typeinfos are introduced as either one of the head variables, or a new
% variable created by a goal in the procedure. If introduced as a head
% variable, initial_liveness will add it to the initial live set of variables
% -- no variable could be introduced earlier than the start of the goal. If
% introduced by a goal in the procedure, that goal must occur before any call
% that requires the typeinfo, so the variable will be born before the
% continuation after the call. So the typeinfo variables will always be born
% before any continuation where they are needed.
%
% A typeinfo variable becomes dead after both the following conditions
% are true:
%
%   (1) The typeinfo variable is not used again (when it is no
%       longer part of the nonlocals)
%   (2) No other nonlocal variable's type is described by that typeinfo
%       variable.
%
% (1) happens without any changes to the liveness computation (it is
%     the normal condition for variables becoming dead). This is more
%     conservative than what is required for the invariant, but is
%     required for code generation, so we should keep it ;-)
% (2) is implemented by adding the typeinfo variables for the types of the
%     nonlocals to the nonlocals for the purposes of computing liveness.
%
% In some circumstances, one of which is tests/debugger/resume_typeinfos.m, it
% is possible for a typeinfo variable to be born in a goal without that
% typeinfo variable appearing anywhere else in the procedure body.
% Nevertheless, with typeinfo liveness, we must consider such variables to be
% born in such goals even though they do not appear in the nonlocals set or in
% the instmap delta. (If they were not born, it would be an error for them to
% die, and die they will, at the last occurrence of a variable whose type they
% (partially) describe.) The special case solution we adopt for such
% situations is that we consider the first appearance of a typeinfo variable
% in the typeinfo-completed nonlocals set of a goal to be a value giving
% occurrence, even if the typeinfo does not appear in the instmap delta. This
% is safe, since with our current scheme for handling polymorphism, the first
% appearance will in fact always ground the typeinfo.
%
% So typeinfo variables will always be born before they are needed, and die
% only when no other variable needing them will be live, so the invariant
% holds.
%
% Quantification notes:
%
% If a variable is not live on entry to a goal, but the goal gives it a value,
% the code of this module assumes that:
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
% If a variable occurs in the nonlocal set of the goal, but is actually local
% to the goal, then any occurrence of that variable in the postbirth sets of
% parallel goals will lead to an inconsistency, because the variable will not
% die on those parallel paths, but will die on the path that actually gives a
% value to the variable.
%
% The nonlocal set of a goal is in general allowed to overapproximate the true
% set of nonlocal variables of the goal. Since this module requires *exact*
% information about nonlocals, it must recompute the nonlocal sets before
% starting.
%
% As written this module expects goals to be simplified, otherwise there may be
% assertion failures.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.liveness.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.set_of_var.

%-----------------------------------------------------------------------------%

    % Add liveness annotations to the goal of the procedure. This consists of
    % the {pre,post}{birth,death} sets and resume point information.
    %
:- pred detect_liveness_proc(module_info::in, pred_proc_id::in,
    proc_info::in, proc_info::out) is det.

    % Add liveness annotations to the goals of nonimported procedures in the
    % module. Attempt to do so in parallel. Debugging liveness is not supported
    % in this version.
    %
:- pred detect_liveness_preds_parallel(module_info::in, module_info::out)
    is det.

    % Return the set of variables live at the start of the procedure.
    %
:- pred initial_liveness(module_info::in, pred_info::in, proc_info::in,
    set_of_progvar::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.arg_info.
:- import_module hlds.code_model.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.quantification.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module ll_backend.trace_gen.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

detect_liveness_proc(ModuleInfo, proc(PredId, _ProcId), !ProcInfo) :-
    detect_liveness_proc_2(ModuleInfo, PredId, !ProcInfo).

:- pred detect_liveness_proc_2(module_info::in, pred_id::in,
    proc_info::in, proc_info::out) is det.

detect_liveness_proc_2(ModuleInfo, PredId, !ProcInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_int_option(Globals, debug_liveness, DebugLiveness),
    pred_id_to_int(PredId, PredIdInt),

    proc_info_get_goal(!.ProcInfo, GoalBeforeQuant),
    proc_info_get_varset(!.ProcInfo, VarSetBeforeQuant),

    trace [io(!IO)] (
        maybe_debug_liveness(ModuleInfo, "\nbefore requantify",
            DebugLiveness, PredIdInt, VarSetBeforeQuant, GoalBeforeQuant, !IO)
    ),
    requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo),

    proc_info_get_goal(!.ProcInfo, GoalAfterQuant),
    proc_info_get_varset(!.ProcInfo, VarSet),
    proc_info_get_vartypes(!.ProcInfo, VarTypes),
    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    body_should_use_typeinfo_liveness(PredInfo, Globals, TypeInfoLiveness),
    live_info_init(ModuleInfo, TypeInfoLiveness, VarSet, VarTypes, RttiVarMaps,
        LiveInfo),

    trace [io(!IO)] (
        maybe_debug_liveness(ModuleInfo, "\nbefore liveness",
            DebugLiveness, PredIdInt, VarSet, GoalAfterQuant, !IO)
    ),

    initial_liveness(ModuleInfo, PredInfo, !.ProcInfo, Liveness0),
    detect_liveness_in_goal(GoalAfterQuant, GoalAfterLiveness,
        Liveness0, _, LiveInfo),

    trace [io(!IO)] (
        maybe_debug_liveness(ModuleInfo, "\nafter liveness",
            DebugLiveness, PredIdInt, VarSet, GoalAfterLiveness, !IO)
    ),

    initial_deadness(ModuleInfo, !.ProcInfo, LiveInfo, Deadness0),
    detect_deadness_in_goal(GoalAfterLiveness, GoalAfterDeadness,
        Deadness0, _, Liveness0, LiveInfo),
    trace [io(!IO)] (
        maybe_debug_liveness(ModuleInfo, "\nafter deadness",
            DebugLiveness, PredIdInt, VarSet, GoalAfterDeadness, !IO)
    ),

    ( if
        globals.get_trace_level(Globals, TraceLevel),
        AllowDelayDeath = trace_level_allows_delay_death(TraceLevel),
        AllowDelayDeath = yes,
        globals.lookup_bool_option(Globals, delay_death, DelayDeath),
        DelayDeath = yes,
        globals.lookup_int_option(Globals, delay_death_max_vars,
            DelayDeathMaxVars),
        % Don't count the variables in the vartypes map if the varset
        % shows that it cannot possibly contain too many variables.
        (
            varset.num_allocated(VarSet) =< DelayDeathMaxVars
        ;
            vartypes_count(VarTypes, NumVars),
            NumVars =< DelayDeathMaxVars
        ),
        pred_info_get_origin(PredInfo, Origin),
        Origin \= origin_special_pred(_, _)
    then
        delay_death_proc_body(GoalAfterDeadness, GoalAfterDelayDeath,
            VarSet, Liveness0),
        trace [io(!IO)] (
            maybe_debug_liveness(ModuleInfo, "\nafter delay death",
                DebugLiveness, PredIdInt, VarSet, GoalAfterDelayDeath, !IO)
        )
    else
        GoalAfterDelayDeath = GoalAfterDeadness
    ),

    globals.get_trace_level(Globals, TraceLevel),
    NeedsFailVars = eff_trace_level_needs_fail_vars(ModuleInfo, PredInfo,
        !.ProcInfo, TraceLevel),
    (
        NeedsFailVars = yes,
        trace_fail_vars(ModuleInfo, !.ProcInfo, ResumeVars0)
    ;
        NeedsFailVars = no,
        ResumeVars0 = set_of_var.init
    ),
    detect_resume_points_in_goal(GoalAfterDelayDeath, Goal, Liveness0, _,
        LiveInfo, ResumeVars0),
    trace [io(!IO)] (
        maybe_debug_liveness(ModuleInfo, "\nafter resume point",
            DebugLiveness, PredIdInt, VarSet, Goal, !IO)
    ),
    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_liveness_info(Liveness0, !ProcInfo).

:- pred maybe_debug_liveness(module_info::in, string::in, int::in, int::in,
    prog_varset::in, hlds_goal::in, io::di, io::uo) is det.

maybe_debug_liveness(ModuleInfo, Message, DebugLiveness, PredIdInt, VarSet,
        Goal, !IO) :-
    ( if DebugLiveness = PredIdInt then
        io.write_string(Message, !IO),
        io.write_string(":\n", !IO),
        module_info_get_globals(ModuleInfo, Globals),
        OutInfo = init_hlds_out_info(Globals, output_debug),
        write_goal(OutInfo, ModuleInfo, VarSet, print_name_and_num, 0,
            "\n", Goal, !IO)
    else
        true
    ).

%-----------------------------------------------------------------------------%

detect_liveness_preds_parallel(!HLDS) :-
    module_info_get_valid_pred_ids(!.HLDS, PredIds),
    detect_liveness_preds_parallel_2(PredIds, !.HLDS, !HLDS).

:- pred detect_liveness_preds_parallel_2(list(pred_id)::in,
    module_info::in, module_info::in, module_info::out) is det.

detect_liveness_preds_parallel_2(PredIds, HLDS0, !HLDS) :-
    ( if list.split_list(1000, PredIds, HeadPredIds, TailPredIds) then
        ( detect_liveness_preds_parallel_3(HeadPredIds, HLDS0, !HLDS)
        % XXX The following should be a parallel conjunction.
        , detect_liveness_preds_parallel_2(TailPredIds, HLDS0, !HLDS)
        )
    else
        detect_liveness_preds_parallel_3(PredIds, HLDS0, !HLDS)
    ).

:- pred detect_liveness_preds_parallel_3(list(pred_id)::in,
    module_info::in, module_info::in, module_info::out) is det.

detect_liveness_preds_parallel_3(PredIds, HLDS0, !HLDS) :-
    list.map(detect_liveness_pred(HLDS0), PredIds, PredInfos),
    list.foldl_corresponding(module_info_set_pred_info, PredIds, PredInfos,
        !HLDS).

:- pred detect_liveness_pred(module_info::in, pred_id::in, pred_info::out)
    is det.

detect_liveness_pred(ModuleInfo, PredId, PredInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo0),
    ProcIds = pred_info_non_imported_procids(PredInfo0),
    list.foldl(detect_liveness_pred_proc(ModuleInfo, PredId), ProcIds,
        PredInfo0, PredInfo).

:- pred detect_liveness_pred_proc(module_info::in, pred_id::in,
    proc_id::in, pred_info::in, pred_info::out) is det.

detect_liveness_pred_proc(ModuleInfo, PredId, ProcId, !PredInfo) :-
    pred_info_proc_info(!.PredInfo, ProcId, ProcInfo0),
    detect_liveness_proc_2(ModuleInfo, PredId, ProcInfo0, ProcInfo),
    pred_info_set_proc_info(ProcId, ProcInfo, !PredInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_goal(hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out, live_info::in) is det.

detect_liveness_in_goal(Goal0, Goal, Liveness0, FinalLiveness, LiveInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    % Work out which variables get born in this goal.
    get_nonlocals_and_typeinfos(LiveInfo, GoalInfo0,
        BaseNonLocals, CompletedNonLocals),
    set_of_var.difference(CompletedNonLocals, Liveness0, NewVarsSet),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo0),
    ( if instmap_delta_is_unreachable(InstMapDelta) then
        Births = set_of_var.init
    else
        NewVarsList = set_of_var.to_sorted_list(NewVarsSet),
        Births0 = set_of_var.init,
        find_value_giving_occurrences(NewVarsList, LiveInfo,
            InstMapDelta, Births0, Births1),
        set_of_var.difference(CompletedNonLocals, BaseNonLocals, TypeInfos),
        set_of_var.difference(TypeInfos, Liveness0, NewTypeInfos),
        set_of_var.union(Births1, NewTypeInfos, Births)
    ),
    set_of_var.union(Liveness0, Births, FinalLiveness),

    (
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_,_,  _, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ),
        PreDeaths = set_of_var.init,
        PreBirths = Births,
        PostDeaths = set_of_var.init,
        PostBirths = set_of_var.init,
        GoalExpr = GoalExpr0
    ;
        (
            GoalExpr0 = conj(ConjType, Goals0),
            (
                ConjType = plain_conj,
                detect_liveness_in_conj(Goals0, Goals, Liveness0, Liveness,
                    LiveInfo)
            ;
                ConjType = parallel_conj,
                Union0 = set_of_var.init,
                detect_liveness_in_par_conj(Goals0, Goals, Liveness0,
                    CompletedNonLocals, LiveInfo, Union0, Union),
                set_of_var.union(Union, Liveness0, Liveness)
            ),
            GoalExpr = conj(ConjType, Goals)
        ;
            GoalExpr0 = disj(Goals0),
            Union0 = set_of_var.init,
            detect_liveness_in_disj(Goals0, Goals, Liveness0,
                CompletedNonLocals, LiveInfo, Union0, Union),
            set_of_var.union(Union, Liveness0, Liveness),
            GoalExpr = disj(Goals)
        ;
            GoalExpr0 = switch(Var, Det, Cases0),
            detect_liveness_in_cases(Cases0, Cases, Liveness0,
                CompletedNonLocals, LiveInfo, Liveness0, Liveness),
            GoalExpr = switch(Var, Det, Cases)
        ;
            GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),

            detect_liveness_in_goal(Cond0, Cond, Liveness0, LivenessCond,
                LiveInfo),

            % If the condition cannot succeed, any variables which become
            % live in the else part should be put in the post-birth set
            % of the then part by add_liveness_after_goal, and the other
            % sets should be empty.
            Cond = hlds_goal(_, CondInfo),
            CondDelta = goal_info_get_instmap_delta(CondInfo),
            ( if instmap_delta_is_unreachable(CondDelta) then
                LivenessThen = LivenessCond,
                % XXX Initialize liveness-related fields, since some other
                % code in this module assumes that they are initialized.
                detect_liveness_in_goal(Then0, Then1,
                    LivenessCond, _LivenessThen, LiveInfo)
            else
                detect_liveness_in_goal(Then0, Then1,
                    LivenessCond, LivenessThen, LiveInfo)
            ),

            detect_liveness_in_goal(Else0, Else1, Liveness0, LivenessElse,
                LiveInfo),

            set_of_var.union(LivenessThen, LivenessElse, Liveness),
            set_of_var.intersect(Liveness, CompletedNonLocals,
                ITENonLocalLiveness),

            set_of_var.difference(ITENonLocalLiveness, LivenessThen,
                ResidueThen),
            set_of_var.difference(ITENonLocalLiveness, LivenessElse,
                ResidueElse),

            add_liveness_after_goal(Then1, ResidueThen, Then),
            add_liveness_after_goal(Else1, ResidueElse, Else),

            GoalExpr = if_then_else(Vars, Cond, Then, Else)
        ;
            GoalExpr0 = negation(SubGoal0),
            detect_liveness_in_goal(SubGoal0, SubGoal, Liveness0, Liveness,
                LiveInfo),
            GoalExpr = negation(SubGoal)
        ;
            GoalExpr0 = scope(Reason, SubGoal0),
            ( if
                Reason = from_ground_term(TermVar, from_ground_term_construct)
            then
                ( if set_of_var.is_empty(CompletedNonLocals) then
                    % Don't let the later passes in the module include TermVar
                    % in the set of seen variables, since if this scope is
                    % one arm of a branched structure, the other arms won't
                    % mention TermVar.
                    GoalExpr = conj(plain_conj, []),
                    Liveness = Liveness0
                else
                    detect_liveness_in_fgt_construct(SubGoal0, SubGoal,
                        Liveness0, Liveness, LiveInfo, TermVar),
                    GoalExpr = scope(Reason, SubGoal)
                )
            else
                % XXX We could treat from_ground_term_deconstruct specially
                % as well.
                detect_liveness_in_goal(SubGoal0, SubGoal, Liveness0, Liveness,
                    LiveInfo),
                GoalExpr = scope(Reason, SubGoal)
            )
        ),
        PreDeaths = set_of_var.init,
        PreBirths = set_of_var.init,
        set_of_var.intersect(CompletedNonLocals, Liveness, NonLocalLiveness),
        set_of_var.union(NonLocalLiveness, Liveness0, GoalFinalLiveness),
        set_of_var.difference(GoalFinalLiveness, FinalLiveness, PostDeaths),
        set_of_var.difference(FinalLiveness, GoalFinalLiveness, PostBirths)
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ),
    % We always initialize all the liveness-related fields in order to
    % obliterate any annotations left by a previous invocation of this module.
    goal_info_initialize_liveness_info(PreBirths, PostBirths,
        PreDeaths, PostDeaths, no_resume_point, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out, live_info::in) is det.

detect_liveness_in_conj([], [], !Liveness, _LiveInfo).
detect_liveness_in_conj([Goal0 | Goals0], [Goal | Goals], !Liveness,
        LiveInfo) :-
    detect_liveness_in_goal(Goal0, Goal, !Liveness, LiveInfo),
    ( if
        Goal0 = hlds_goal(_, GoalInfo),
        InstmapDelta = goal_info_get_instmap_delta(GoalInfo),
        instmap_delta_is_unreachable(InstmapDelta)
    then
        % If we continued processing goals, the final value of Liveness
        % would not reflect reality. If we stopped processing goals but
        % included the original Goals0 in Goals, then the liveness
        % fields in Goals would remain uninitialized. Removing goals
        % following a goal that cannot succeed works.
        Goals = []
    else
        detect_liveness_in_conj(Goals0, Goals, !Liveness, LiveInfo)
    ).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::in, live_info::in,
    set_of_progvar::in, set_of_progvar::out) is det.

detect_liveness_in_disj([], [], _Liveness, _NonLocals, _LiveInfo, !Union).
detect_liveness_in_disj([Goal0 | Goals0], [Goal | Goals], Liveness0, NonLocals,
        LiveInfo, !Union) :-
    detect_liveness_in_goal(Goal0, Goal1, Liveness0, Liveness1, LiveInfo),
    set_of_var.union(Liveness1, !Union),
    detect_liveness_in_disj(Goals0, Goals, Liveness0, NonLocals, LiveInfo,
        !Union),
    set_of_var.intersect(!.Union, NonLocals, NonLocalUnion),
    set_of_var.difference(NonLocalUnion, Liveness1, Residue),
    add_liveness_after_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_cases(list(case)::in, list(case)::out,
    set_of_progvar::in, set_of_progvar::in, live_info::in,
    set_of_progvar::in, set_of_progvar::out) is det.

detect_liveness_in_cases([], [], _Liveness, _NonLocals, _LiveInfo, !Union).
detect_liveness_in_cases([Case0 | Cases0], [Case | Cases], Liveness0,
        NonLocals, LiveInfo, !Union) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    detect_liveness_in_goal(Goal0, Goal1, Liveness0, Liveness1, LiveInfo),
    set_of_var.union(Liveness1, !Union),
    detect_liveness_in_cases(Cases0, Cases, Liveness0, NonLocals, LiveInfo,
        !Union),
    set_of_var.intersect(!.Union, NonLocals, NonLocalUnion),
    set_of_var.difference(NonLocalUnion, Liveness1, Residue),
    add_liveness_after_goal(Goal1, Residue, Goal),
    Case = case(MainConsId, OtherConsIds, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_fgt_construct(hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out, live_info::in, prog_var::in)
    is det.

detect_liveness_in_fgt_construct(Goal0, Goal, !Liveness, LiveInfo, TermVar) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ( if GoalExpr0 = conj(plain_conj, Conjuncts0) then
        LocalLiveVars0 = set_of_var.init,
        detect_liveness_in_fgt_construct_goal_loop(Conjuncts0, Conjuncts,
            LocalLiveVars0, LocalLiveVars),
        ( if set_of_var.is_singleton(LocalLiveVars, TermVar) then
            maybe_complete_with_typeinfos(LiveInfo,
                set_of_var.make_singleton(TermVar), CompletedTermVars),
            set_of_var.union(CompletedTermVars, !Liveness),
            GoalExpr = conj(plain_conj, Conjuncts),
            PreBirths  = set_of_var.init,
            PostBirths = set_of_var.init,
            PreDeaths  = set_of_var.init,
            PostDeaths = set_of_var.init,
            goal_info_initialize_liveness_info(PreBirths, PostBirths,
                PreDeaths, PostDeaths, no_resume_point, GoalInfo0, GoalInfo),
            Goal = hlds_goal(GoalExpr, GoalInfo)
        else
            unexpected($pred, "unexpected liveness")
        )
    else
        unexpected($pred, "not conj")
    ).

:- pred detect_liveness_in_fgt_construct_goal_loop(
    list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out) is det.

detect_liveness_in_fgt_construct_goal_loop([], [], !LocalLiveVars).
detect_liveness_in_fgt_construct_goal_loop([Goal0 | Goals0], [Goal | Goals],
        !LocalLiveVars) :-
    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    ( if
        GoalExpr = unify(_, _, _, Unification, _),
        Unification = construct(LHSVar, _ConsId, RHSVars, _ArgModes,
            construct_statically, cell_is_shared, no_construct_sub_info)
    then
        ( if set_of_var.remove_list(RHSVars, !LocalLiveVars) then
            set_of_var.insert(LHSVar, !LocalLiveVars),
            PreBirths = set_of_var.make_singleton(LHSVar),
            PostBirths = set_of_var.init,
            PreDeaths  = set_of_var.init,
            PostDeaths = set_of_var.list_to_set(RHSVars),
            goal_info_initialize_liveness_info(PreBirths, PostBirths,
                PreDeaths, PostDeaths, no_resume_point, GoalInfo0, GoalInfo),
            Goal = hlds_goal(GoalExpr, GoalInfo)
        else
            unexpected($pred, "rhs var not live")
        )
    else
        unexpected($pred, "unexpected conjunct")
    ),
    detect_liveness_in_fgt_construct_goal_loop(Goals0, Goals, !LocalLiveVars).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_par_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::in, live_info::in,
    set_of_progvar::in, set_of_progvar::out) is det.

detect_liveness_in_par_conj([], [], _Liveness, _NonLocals, _LiveInfo, !Union).
detect_liveness_in_par_conj([Goal0 | Goals0], [Goal | Goals], Liveness0,
        NonLocals, LiveInfo, !Union) :-
    detect_liveness_in_goal(Goal0, Goal1, Liveness0, Liveness1, LiveInfo),
    set_of_var.union(Liveness1, !Union),
    detect_liveness_in_par_conj(Goals0, Goals, Liveness0, NonLocals,
        LiveInfo, !Union),
    set_of_var.intersect(!.Union, NonLocals, NonLocalUnion),
    set_of_var.difference(NonLocalUnion, Liveness1, Residue),
    add_liveness_after_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % At any given program point, Deadness is the set of variables that are
    % live now and whose values will be needed beyond that program point.
    %
:- pred detect_deadness_in_goal(hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out, set_of_progvar::in,
    live_info::in) is det.

detect_deadness_in_goal(Goal0, Goal, !Deadness, !.Liveness, LiveInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    goal_info_get_pre_deaths(GoalInfo0, PreDeaths0),
    goal_info_get_pre_births(GoalInfo0, PreBirths0),
    goal_info_get_post_deaths(GoalInfo0, PostDeaths0),
    goal_info_get_post_births(GoalInfo0, PostBirths0),

    set_of_var.difference(!.Deadness, PostBirths0, !:Deadness),
    set_of_var.union(PostDeaths0, !Deadness),

    set_of_var.difference(!.Liveness, PreDeaths0, !:Liveness),
    set_of_var.union(PreBirths0, !Liveness),

    (
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ),
        get_nonlocals_and_typeinfos(LiveInfo, GoalInfo0,
            _BaseNonLocals, CompletedNonLocals),
        set_of_var.intersect(!.Liveness, CompletedNonLocals, LiveNonLocals),
        set_of_var.difference(LiveNonLocals, !.Deadness, NewPostDeaths),
        set_of_var.union(NewPostDeaths, !Deadness),
        GoalExpr = GoalExpr0,

        set_of_var.union(PostDeaths0, NewPostDeaths, PostDeaths),
        goal_info_set_post_deaths(PostDeaths, GoalInfo0, GoalInfo)
    ;
        GoalExpr0 = conj(ConjType, Conjuncts0),
        (
            Conjuncts0 = [],
            GoalExpr = GoalExpr0
        ;
            Conjuncts0 = [_ | _],
            (
                ConjType = plain_conj,
                detect_deadness_in_conj(Conjuncts0, Conjuncts, !Deadness,
                    !.Liveness, LiveInfo)
            ;
                ConjType = parallel_conj,
                get_nonlocals_and_typeinfos(LiveInfo, GoalInfo0,
                    _, CompletedNonLocals),
                Union0 = set_of_var.init,
                detect_deadness_in_par_conj(Conjuncts0, Conjuncts, !.Deadness,
                    !.Liveness, CompletedNonLocals, LiveInfo, Union0, Union,
                    _CompletedNonLocalUnion),
                !:Deadness = Union
            ),
            GoalExpr = conj(ConjType, Conjuncts)
        ),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = disj(Disjuncts0),
        (
            Disjuncts0 = [],
            GoalExpr = GoalExpr0
        ;
            Disjuncts0 = [_ | _],
            get_nonlocals_and_typeinfos(LiveInfo, GoalInfo0,
                _, CompletedNonLocals),
            Union0 = set_of_var.init,
            detect_deadness_in_disj(Disjuncts0, Disjuncts, !.Deadness,
                !.Liveness, CompletedNonLocals, LiveInfo, Union0, Union, _),
            !:Deadness = Union,
            GoalExpr = disj(Disjuncts)
        ),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = switch(Var, Det, Cases0),
        get_nonlocals_and_typeinfos(LiveInfo, GoalInfo0,
            _, CompletedNonLocals),
        Union0 = set_of_var.init,
        detect_deadness_in_cases(Var, Cases0, Cases, !.Deadness, !.Liveness,
            CompletedNonLocals, LiveInfo, Union0, Union, _),
        !:Deadness = Union,
        GoalExpr = switch(Var, Det, Cases),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        Deadness0 = !.Deadness,

        update_liveness_goal(Cond0, LiveInfo, !.Liveness, LivenessCond),

        detect_deadness_in_goal(Else0, Else1, Deadness0, DeadnessElse,
            !.Liveness, LiveInfo),
        detect_deadness_in_goal(Then0, Then, Deadness0, DeadnessThen,
            LivenessCond, LiveInfo),
        detect_deadness_in_goal(Cond0, Cond1, DeadnessThen, DeadnessCond,
            !.Liveness, LiveInfo),

        get_nonlocals_and_typeinfos(LiveInfo, GoalInfo0,
            _, CompletedNonLocals),
        InstmapDelta = goal_info_get_instmap_delta(GoalInfo0),
        ( if instmap_delta_is_reachable(InstmapDelta) then
            Cond0 = hlds_goal(_, CondGoalInfo),
            CondInstmapDelta = goal_info_get_instmap_delta(CondGoalInfo),
            Then0 = hlds_goal(_, ThenGoalInfo),
            ThenInstmapDelta = goal_info_get_instmap_delta(ThenGoalInfo),
            Else0 = hlds_goal(_, ElseGoalInfo),
            ElseInstmapDelta = goal_info_get_instmap_delta(ElseGoalInfo),
            ( if
                instmap_delta_is_reachable(CondInstmapDelta),
                instmap_delta_is_reachable(ThenInstmapDelta)
            then
                CondThenInstmapReachable = yes
            else
                CondThenInstmapReachable = no
            ),
            ( if
                instmap_delta_is_reachable(ElseInstmapDelta)
            then
                ElseInstmapReachable = yes
            else
                ElseInstmapReachable = no
            ),
            Union0 = set_of_var.init,
            union_branch_deadness(DeadnessCond, Deadness0,
                CondThenInstmapReachable, Union0, Union1),
            union_branch_deadness(DeadnessElse, Deadness0,
                ElseInstmapReachable, Union1, Union),
            Deadness = Union,
            set_of_var.intersect(Deadness, CompletedNonLocals,
                CompletedNonLocalDeadness),
            add_branch_pre_deaths(DeadnessCond, Deadness0,
                CompletedNonLocalDeadness, CondThenInstmapReachable,
                Cond1, Cond),
            add_branch_pre_deaths(DeadnessElse, Deadness0,
                CompletedNonLocalDeadness, ElseInstmapReachable, Else1, Else)
        else
            set_of_var.union(DeadnessCond, DeadnessElse, Deadness),
            set_of_var.intersect(Deadness, CompletedNonLocals,
                CompletedNonLocalDeadness),
            InstmapReachable = no,
            add_branch_pre_deaths(DeadnessCond, Deadness0,
                CompletedNonLocalDeadness, InstmapReachable, Cond1, Cond),
            add_branch_pre_deaths(DeadnessElse, Deadness0,
                CompletedNonLocalDeadness, InstmapReachable, Else1, Else)
        ),
        !:Deadness = Deadness,
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = negation(SubGoal0),
        detect_deadness_in_goal(SubGoal0, SubGoal, !Deadness,
            !.Liveness, LiveInfo),
        GoalExpr = negation(SubGoal),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(TermVar, from_ground_term_construct)
        then
            maybe_complete_with_typeinfos(LiveInfo,
                set_of_var.make_singleton(TermVar), CompletedTermVars),
            set_of_var.difference(!.Deadness, CompletedTermVars, !:Deadness),

            % The job on SubGoal0 was done by detect_liveness_in_goal.
            SubGoal = SubGoal0
        else
            % XXX We could treat from_ground_term_deconstruct specially
            % as well.
            detect_deadness_in_goal(SubGoal0, SubGoal, !Deadness,
                !.Liveness, LiveInfo)
        ),
        GoalExpr = scope(Reason, SubGoal),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ),

    Goal = hlds_goal(GoalExpr, GoalInfo),
    set_of_var.difference(!.Deadness, PreBirths0, !:Deadness),
    set_of_var.union(PreDeaths0, !Deadness).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out, set_of_progvar::in,
    live_info::in) is det.

detect_deadness_in_conj([], [], !Deadness, _, _LiveInfo).
detect_deadness_in_conj([Goal0 | Goals0], [Goal | Goals], !Deadness,
        Liveness0, LiveInfo) :-
    Goal0 = hlds_goal(_, GoalInfo),
    InstmapDelta = goal_info_get_instmap_delta(GoalInfo),
    ( if instmap_delta_is_unreachable(InstmapDelta) then
        Goals = Goals0,
        detect_deadness_in_goal(Goal0, Goal, !Deadness, Liveness0, LiveInfo)
    else
        update_liveness_goal(Goal0, LiveInfo, Liveness0, LivenessGoal),
        detect_deadness_in_conj(Goals0, Goals, !Deadness,
            LivenessGoal, LiveInfo),
        detect_deadness_in_goal(Goal0, Goal, !Deadness, Liveness0, LiveInfo)
    ).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::in, set_of_progvar::in,
    live_info::in, set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::out) is det.

detect_deadness_in_disj([], [], _Deadness0, _Liveness0, CompletedNonLocals,
        _LiveInfo, !Union, CompletedNonLocalUnion) :-
    set_of_var.intersect(!.Union, CompletedNonLocals, CompletedNonLocalUnion).
detect_deadness_in_disj([Goal0 | Goals0], [Goal | Goals], Deadness0, Liveness0,
        CompletedNonLocals, LiveInfo, !Union, CompletedNonLocalUnion) :-
    detect_deadness_in_goal(Goal0, Goal1, Deadness0, DeadnessGoal,
        Liveness0, LiveInfo),
    Goal1 = hlds_goal(_, GoalInfo1),
    InstmapDelta1 = goal_info_get_instmap_delta(GoalInfo1),
    ( if instmap_delta_is_reachable(InstmapDelta1) then
        InstmapReachable = yes
    else
        InstmapReachable = no
    ),
    union_branch_deadness(DeadnessGoal, Deadness0, InstmapReachable, !Union),
    detect_deadness_in_disj(Goals0, Goals, Deadness0, Liveness0,
        CompletedNonLocals, LiveInfo, !Union, CompletedNonLocalUnion),
    add_branch_pre_deaths(DeadnessGoal, Deadness0, CompletedNonLocalUnion,
        InstmapReachable, Goal1, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_cases(prog_var::in, list(case)::in, list(case)::out,
    set_of_progvar::in, set_of_progvar::in, set_of_progvar::in, live_info::in,
    set_of_progvar::in, set_of_progvar::out, set_of_progvar::out) is det.

detect_deadness_in_cases(SwitchVar, [], [], _Deadness0, _Liveness,
        CompletedNonLocals, LiveInfo, !Union, CompletedNonLocalUnion) :-
    % If the switch variable does not become dead in a case, it must be put in
    % the pre-death set of that case.
    maybe_complete_with_typeinfos(LiveInfo,
        set_of_var.make_singleton(SwitchVar), CompletedSwitchVar),
    set_of_var.union(CompletedSwitchVar, !Union),
    set_of_var.intersect(!.Union, CompletedNonLocals, CompletedNonLocalUnion).
detect_deadness_in_cases(SwitchVar, [Case0 | Cases0], [Case | Cases],
        Deadness0, Liveness0, CompletedNonLocals, LiveInfo, !Union,
        CompletedNonLocalUnion) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    detect_deadness_in_goal(Goal0, Goal1, Deadness0, DeadnessGoal,
        Liveness0, LiveInfo),
    Goal1 = hlds_goal(_, GoalInfo1),
    InstmapDelta1 = goal_info_get_instmap_delta(GoalInfo1),
    ( if instmap_delta_is_reachable(InstmapDelta1) then
        InstmapReachable = yes
    else
        InstmapReachable = no
    ),
    union_branch_deadness(DeadnessGoal, Deadness0, InstmapReachable, !Union),
    detect_deadness_in_cases(SwitchVar, Cases0, Cases, Deadness0,
        Liveness0, CompletedNonLocals, LiveInfo, !Union,
        CompletedNonLocalUnion),
    add_branch_pre_deaths(DeadnessGoal, Deadness0, CompletedNonLocalUnion,
        InstmapReachable, Goal1, Goal),
    Case = case(MainConsId, OtherConsIds, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_par_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::in, set_of_progvar::in, live_info::in,
    set_of_progvar::in, set_of_progvar::out, set_of_progvar::out) is det.

detect_deadness_in_par_conj([], [], _Deadness0, _Liveness0, CompletedNonLocals,
        _LiveInfo, !Union, CompletedNonLocalUnion) :-
    set_of_var.intersect(!.Union, CompletedNonLocals, CompletedNonLocalUnion).
detect_deadness_in_par_conj([Goal0 | Goals0], [Goal | Goals], Deadness0,
        Liveness0, CompletedNonLocals, LiveInfo, !Union,
        CompletedNonLocalUnion) :-
    detect_deadness_in_goal(Goal0, Goal1, Deadness0, DeadnessGoal,
        Liveness0, LiveInfo),
    set_of_var.union(DeadnessGoal, !Union),
    detect_deadness_in_par_conj(Goals0, Goals, Deadness0,
        Liveness0, CompletedNonLocals, LiveInfo, !Union,
        CompletedNonLocalUnion),
    Goal1 = hlds_goal(_, GoalInfo1),
    InstmapDelta1 = goal_info_get_instmap_delta(GoalInfo1),
    ( if instmap_delta_is_reachable(InstmapDelta1) then
        InstmapReachable = yes
    else
        unexpected($pred, "unreachable instmap")
    ),
    add_branch_pre_deaths(DeadnessGoal, Deadness0, CompletedNonLocalUnion,
        InstmapReachable, Goal1, Goal).

%-----------------------------------------------------------------------------%

% The situation that requires the use of these predicates is the following:
%
% ... ( branch1 ; branch2 ), <goal2>, <goal3> ...
%
% where a variable is born in goal2 and dies in goal3, but, because e.g.
% branch1 has determinism erroneous, the same variable is born and dies in
% branch1 as well. (The current mode system permits this, although whether it
% should is another matter.) Since the variable dies in branch1, it is put into
% the pre-death set of branch2 as well.
%
% The problem arises when the death of the variable is delayed by the third
% phase of liveness. The variable is still born in branch1, but is not born in
% branch2, which leads to an inconsistency and a compiler abort.
%
% The solution is to take into account only the non-erroneous branches (the
% branches that have reachable instmaps) when computing the set of variables
% that have been seen in the branched control structure for the first time
% in the backward traversal in the procedure body, and which therefore should
% be put into the pre-death set of the branches which do not mention them.
%
% A further complication is branched control structures that have *no* branches
% whose end points are reachable. (They exist, typically to select the
% appropriate argument to invoke error/1 with.) We treat such control
% structures as if they were erroneous non-branched goals, reducing the
% situation to the one discussed in the previous paragraph. Treating them
% as non-branched goals in this case requires treating all branches as if
% their end points *were* reachable. Any excess deadness acquired by the goal
% in this fashion will be discarded when the erroneous goal is paralleled by
% a non-erroneous branch of an enclosing branched control structure.

:- pred union_branch_deadness(set_of_progvar::in, set_of_progvar::in,
    bool::in, set_of_progvar::in, set_of_progvar::out) is det.

union_branch_deadness(DeadnessGoal, Deadness0, InstmapReachable, !Union) :-
    (
        InstmapReachable = yes,
        set_of_var.union(!.Union, DeadnessGoal, !:Union)
    ;
        InstmapReachable = no,
        set_of_var.difference(DeadnessGoal, Deadness0, FilteredDeadnessGoal),
        set_of_var.union(!.Union, FilteredDeadnessGoal, !:Union)
    ).

:- pred add_branch_pre_deaths(set_of_progvar::in, set_of_progvar::in,
    set_of_progvar::in, bool::in, hlds_goal::in, hlds_goal::out) is det.

add_branch_pre_deaths(DeadnessGoal, Deadness0, CompletedNonLocalUnion,
        InstmapReachable, !Goal) :-
    set_of_var.difference(CompletedNonLocalUnion, DeadnessGoal, PreDeaths),
    (
        InstmapReachable = yes,
        add_deadness_before_goal(PreDeaths, !Goal)
    ;
        InstmapReachable = no,
        set_of_var.difference(PreDeaths, Deadness0, FilteredPreDeaths),
        add_deadness_before_goal(FilteredPreDeaths, !Goal)
    ).

%-----------------------------------------------------------------------------%

:- pred update_liveness_goal(hlds_goal::in, live_info::in,
    set_of_progvar::in, set_of_progvar::out) is det.

update_liveness_goal(Goal, LiveInfo, !Liveness) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    goal_info_get_pre_deaths(GoalInfo, PreDeaths),
    goal_info_get_pre_births(GoalInfo, PreBirths),
    goal_info_get_post_deaths(GoalInfo, PostDeaths),
    goal_info_get_post_births(GoalInfo, PostBirths),

    Liveness0 = !.Liveness,
    set_of_var.difference(!.Liveness, PreDeaths, !:Liveness),
    set_of_var.union(PreBirths, !Liveness),
    update_liveness_expr(GoalExpr, LiveInfo, !Liveness),
    set_of_var.difference(!.Liveness, PostDeaths, !:Liveness),
    set_of_var.union(PostBirths, !Liveness),

    set_of_var.divide_by_set(Liveness0, !.Liveness, OldLiveness, NewLiveness0),
    get_nonlocals_and_typeinfos(LiveInfo, GoalInfo, _, CompletedNonLocals),
    set_of_var.intersect(NewLiveness0, CompletedNonLocals, NewLiveness),
    set_of_var.union(OldLiveness, NewLiveness, !:Liveness).

:- pred update_liveness_expr(hlds_goal_expr::in, live_info::in,
    set_of_progvar::in, set_of_progvar::out) is det.

update_liveness_expr(GoalExpr, LiveInfo, !Liveness) :-
    (
        ( GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        )
    ;
        GoalExpr = conj(_ConjType, Goals),
        % XXX Do parallel conjunctions need special treatment?
        update_liveness_conj(Goals, LiveInfo, !Liveness)
    ;
        GoalExpr = disj(Goals),
        ( if find_reachable_goal(Goals, Goal) then
            update_liveness_goal(Goal, LiveInfo, !Liveness)
        else
            true
        )
    ;
        GoalExpr = switch(_, _, Cases),
        ( if find_reachable_case(Cases, Goal) then
            update_liveness_goal(Goal, LiveInfo, !Liveness)
        else
            true
        )
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        Else = hlds_goal(_, ElseGoalInfo),
        ElseInstmapDelta = goal_info_get_instmap_delta(ElseGoalInfo),
        Cond = hlds_goal(_, CondGoalInfo),
        CondInstmapDelta = goal_info_get_instmap_delta(CondGoalInfo),
        Then = hlds_goal(_, ThenGoalInfo),
        ThenInstmapDelta = goal_info_get_instmap_delta(ThenGoalInfo),
        ( if
            instmap_delta_is_reachable(ElseInstmapDelta)
        then
            update_liveness_goal(Else, LiveInfo, !Liveness)
        else if
            instmap_delta_is_reachable(CondInstmapDelta),
            instmap_delta_is_reachable(ThenInstmapDelta)
        then
            update_liveness_goal(Cond, LiveInfo, !Liveness),
            update_liveness_goal(Then, LiveInfo, !Liveness)
        else
            true
        )
    ;
        GoalExpr = negation(SubGoal),
        update_liveness_goal(SubGoal, LiveInfo, !Liveness)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(TermVar, from_ground_term_construct)
        then
            maybe_complete_with_typeinfos(LiveInfo,
                set_of_var.make_singleton(TermVar), CompletedTermVars),
            set_of_var.union(CompletedTermVars, !Liveness)
        else
            % XXX We could treat from_ground_term_deconstruct specially
            % as well.
            update_liveness_goal(SubGoal, LiveInfo, !Liveness)
        )
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred update_liveness_conj(list(hlds_goal)::in, live_info::in,
    set_of_progvar::in, set_of_progvar::out) is det.

update_liveness_conj([], _, !Liveness).
update_liveness_conj([Goal | Goals], LiveInfo, !Liveness) :-
    update_liveness_goal(Goal, LiveInfo, !Liveness),
    update_liveness_conj(Goals, LiveInfo, !Liveness).

:- pred find_reachable_goal(list(hlds_goal)::in, hlds_goal::out) is semidet.

find_reachable_goal([Goal | Goals], ReachableGoal) :-
    Goal = hlds_goal(_, GoalInfo),
    InstmapDelta = goal_info_get_instmap_delta(GoalInfo),
    ( if instmap_delta_is_reachable(InstmapDelta) then
        ReachableGoal = Goal
    else
        find_reachable_goal(Goals, ReachableGoal)
    ).

:- pred find_reachable_case(list(case)::in, hlds_goal::out) is semidet.

find_reachable_case([case(_, _, Goal) | Cases], ReachableGoal) :-
    Goal = hlds_goal(_, GoalInfo),
    InstmapDelta = goal_info_get_instmap_delta(GoalInfo),
    ( if instmap_delta_is_unreachable(InstmapDelta) then
        find_reachable_case(Cases, ReachableGoal)
    else
        ReachableGoal = Goal
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The delay_death pass works by maintaining a set of variables (all named)
% that, according to the deadness pass should have died by now, but which
% are being kept alive so that their values are accessible to the debugger.
% Variables in this DelayedDead set are finally killed when we come to the end
% of the goal in which they were born. This is because if a variable is born in
% one arm of a branched control structure (e.g. a switch), it cannot live
% beyond the control structure, because it is not given a value in other
% branches.
%
% The correctness of this pass with typeinfo_liveness depends on the fact that
% typeinfo and typeclass_info variables are all named. If they weren't, then it
% would be possible for a (named) variable to have its death delayed without
% the type(class)info variable describing part of its type having its death
% delayed as well. In fact, its death will be delayed by at least as much,
% since a variable cannot be live on entry to a branched control structure
% without the type(class)info variables describing its type being live there as
% well.
%
% This is why cse_detection.m, when it duplicates a deconstruction unification
% and finds that an argument variable contains a typeinfo or typeclass info
% (which can happen with existential types), it will preserve the name of
% that argument variable. Specifically, will copy the name of the argument
% variable in the deconstruction in ONE of the branches of the branched control
% structure it processes, but since such variables should be named in ALL
% branches, and we don't care about the actual name itself, this should be ok.
%
% For the details of potential problems with delaying the death of some
% but not all variables, see tests/valid/bug50.m.

:- pred delay_death_proc_body(hlds_goal::in, hlds_goal::out, prog_varset::in,
    set_of_progvar::in) is det.

delay_death_proc_body(Goal0, Goal, VarSet, BornVars0) :-
    delay_death_goal(Goal0, Goal1, BornVars0, _, set_of_var.init, DelayedDead,
        VarSet),
    Goal1 = hlds_goal(GoalExpr, GoalInfo1),
    goal_info_get_post_deaths(GoalInfo1, PostDeaths1),
    set_of_var.union(PostDeaths1, DelayedDead, PostDeaths),
    goal_info_set_post_deaths(PostDeaths, GoalInfo1, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred delay_death_goal(hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out, prog_varset::in) is det.

delay_death_goal(Goal0, Goal, !BornVars, !DelayedDead, VarSet) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    goal_info_get_pre_births(GoalInfo0, PreBirths),
    goal_info_get_pre_deaths(GoalInfo0, PreDeaths0),
    BornVars0 = !.BornVars,

    set_of_var.union(PreBirths, !BornVars),
    set_of_var.divide(var_is_named(VarSet), PreDeaths0,
        PreDelayedDead, UnnamedPreDeaths),
    set_of_var.union(PreDelayedDead, !DelayedDead),
    goal_info_set_pre_deaths(UnnamedPreDeaths, GoalInfo0, GoalInfo1),

    delay_death_goal_expr(GoalExpr0, GoalExpr, GoalInfo1, GoalInfo2,
        !BornVars, !DelayedDead, VarSet),

    goal_info_get_post_births(GoalInfo2, PostBirths),
    goal_info_get_post_deaths(GoalInfo2, PostDeaths2),

    set_of_var.union(PostBirths, !BornVars),
    set_of_var.divide(var_is_named(VarSet), PostDeaths2,
        PostDelayedDead, UnnamedPostDeaths),
    set_of_var.union(PostDelayedDead, !DelayedDead),
    set_of_var.divide_by_set(BornVars0, !.DelayedDead,
        !:DelayedDead, ToBeKilled),
    set_of_var.union(UnnamedPostDeaths, ToBeKilled, PostDeaths),
    goal_info_set_post_deaths(PostDeaths, GoalInfo2, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred var_is_named(prog_varset::in, prog_var::in) is semidet.

var_is_named(VarSet, Var) :-
    varset.search_name(VarSet, Var, _).

:- pred delay_death_goal_expr(hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out, prog_varset::in) is det.

delay_death_goal_expr(!GoalExpr, !GoalInfo, !BornVars, !DelayedDead, VarSet) :-
    (
        !.GoalExpr = plain_call(_, _, _, _, _, _)
    ;
        !.GoalExpr = generic_call(_, _, _, _, _)
    ;
        !.GoalExpr = unify(_, _, _, _, _)
    ;
        !.GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        !.GoalExpr = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            delay_death_conj(Goals0, Goals, !BornVars, !DelayedDead, VarSet)
        ;
            ConjType = parallel_conj,
            delay_death_par_conj(Goals0, Goals, !BornVars, !DelayedDead,
                VarSet)
        ),
        !:GoalExpr = conj(ConjType, Goals)
    ;
        !.GoalExpr = disj(Goals0),
        delay_death_disj(Goals0, GoalDeaths, !.BornVars, !.DelayedDead,
            VarSet, MaybeBornVarsDelayedDead),
        (
            MaybeBornVarsDelayedDead = yes(!:BornVars - !:DelayedDead),
            Goals = list.map(kill_excess_delayed_dead_goal(!.DelayedDead),
                GoalDeaths),
            !:GoalExpr = disj(Goals)
        ;
            MaybeBornVarsDelayedDead = no
            % Empty disjunctions represent the goal `fail',
            % so we process them as if they were primitive goals.
        )
    ;
        !.GoalExpr = switch(Var, CanFail, Cases0),
        delay_death_cases(Cases0, CaseDeaths, !.BornVars, !.DelayedDead,
            VarSet, MaybeBornVarsDelayedDead),
        (
            MaybeBornVarsDelayedDead = yes(!:BornVars - !:DelayedDead),
            Cases = list.map(kill_excess_delayed_dead_case(!.DelayedDead),
                CaseDeaths),
            !:GoalExpr = switch(Var, CanFail, Cases)
        ;
            MaybeBornVarsDelayedDead = no,
            unexpected($pred, "empty switch")
        )
    ;
        !.GoalExpr = negation(Goal0),
        delay_death_goal(Goal0, Goal, !.BornVars, _, !DelayedDead, VarSet),
        !:GoalExpr = negation(Goal)
    ;
        !.GoalExpr = if_then_else(QuantVars, Cond0, Then0, Else0),
        BornVars0 = !.BornVars,
        DelayedDead0 = !.DelayedDead,
        delay_death_goal(Cond0, Cond, BornVars0, BornVarsCond,
            DelayedDead0, DelayedDeadCond, VarSet),
        delay_death_goal(Then0, Then1, BornVarsCond, BornVarsThen,
            DelayedDeadCond, DelayedDeadThen, VarSet),
        delay_death_goal(Else0, Else1, BornVars0, BornVarsElse,
            DelayedDead0, DelayedDeadElse, VarSet),
        set_of_var.intersect(BornVarsThen, BornVarsElse, BornVars),
        set_of_var.intersect(DelayedDeadThen, DelayedDeadElse, DelayedDead),
        Then = kill_excess_delayed_dead_goal(DelayedDead,
            Then1 - DelayedDeadThen),
        !:BornVars = BornVars,
        !:DelayedDead = DelayedDead,
        Else = kill_excess_delayed_dead_goal(DelayedDead,
            Else1 - DelayedDeadElse),
        !:GoalExpr = if_then_else(QuantVars, Cond, Then, Else)
    ;
        !.GoalExpr = scope(Reason, Goal0),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            % All the variables in the scope are anonymous, so there would
            % be no point in delaying their death.
            Goal = Goal0
        else
            % XXX We could treat from_ground_term_deconstruct specially
            % as well.
            delay_death_goal(Goal0, Goal, !.BornVars, _, !DelayedDead, VarSet)
        ),
        !:GoalExpr = scope(Reason, Goal)
    ;
        !.GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred delay_death_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out, prog_varset::in) is det.

delay_death_conj([], [], !BornVars, !DelayedDead, _).
delay_death_conj([Goal0 | Goals0], [Goal | Goals], !BornVars, !DelayedDead,
        VarSet) :-
    delay_death_goal(Goal0, Goal, !BornVars, !DelayedDead, VarSet),
    delay_death_conj(Goals0, Goals, !BornVars, !DelayedDead, VarSet).

:- pred delay_death_par_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out, prog_varset::in) is det.

delay_death_par_conj([], [], !BornVars, !DelayedDead, _).
delay_death_par_conj([Goal0 | Goals0], [Goal | Goals],
        BornVars0, BornVars, DelayedDead0, DelayedDead, VarSet) :-
    delay_death_goal(Goal0, Goal, BornVars0, BornVarsGoal,
        DelayedDead0, DelayedDeadGoal, VarSet),
    delay_death_par_conj(Goals0, Goals, BornVars0, BornVarsGoals,
        DelayedDead0, DelayedDeadGoals, VarSet),
    set_of_var.union(BornVarsGoal, BornVarsGoals, BornVars),
    set_of_var.union(DelayedDeadGoal, DelayedDeadGoals, DelayedDead).

:- pred delay_death_disj(list(hlds_goal)::in,
    assoc_list(hlds_goal, set_of_progvar)::out,
    set_of_progvar::in, set_of_progvar::in, prog_varset::in,
    maybe(pair(set_of_progvar))::out) is det.

delay_death_disj([], [], _, _, _, no).
delay_death_disj([Goal0 | Goals0], [Goal - DelayedDeadGoal | Goals],
        BornVars0, DelayedDead0, VarSet, yes(BornVars - DelayedDead)) :-
    delay_death_goal(Goal0, Goal, BornVars0, BornVarsGoal,
        DelayedDead0, DelayedDeadGoal, VarSet),
    delay_death_disj(Goals0, Goals, BornVars0, DelayedDead0, VarSet,
        MaybeBornVarsDelayedDead),
    (
        MaybeBornVarsDelayedDead = yes(BornVarsGoals - DelayedDeadGoals),
        set_of_var.intersect(BornVarsGoal, BornVarsGoals, BornVars),
        set_of_var.intersect(DelayedDeadGoal, DelayedDeadGoals, DelayedDead)
    ;
        MaybeBornVarsDelayedDead = no,
        BornVars = BornVarsGoal,
        DelayedDead = DelayedDeadGoal
    ).

:- pred delay_death_cases(list(case)::in,
    assoc_list(case, set_of_progvar)::out,
    set_of_progvar::in, set_of_progvar::in, prog_varset::in,
    maybe(pair(set_of_progvar))::out) is det.

delay_death_cases([], [], _, _, _, no).
delay_death_cases([Case0 | Cases0], [Case - DelayedDeadGoal | Cases],
        BornVars0, DelayedDead0, VarSet, yes(BornVars - DelayedDead)) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    delay_death_goal(Goal0, Goal, BornVars0, BornVarsGoal,
        DelayedDead0, DelayedDeadGoal, VarSet),
    Case = case(MainConsId, OtherConsIds, Goal),
    delay_death_cases(Cases0, Cases, BornVars0, DelayedDead0, VarSet,
        MaybeBornVarsDelayedDead),
    (
        MaybeBornVarsDelayedDead = yes(BornVarsCases - DelayedDeadCases),
        set_of_var.intersect(BornVarsGoal, BornVarsCases, BornVars),
        set_of_var.intersect(DelayedDeadGoal, DelayedDeadCases, DelayedDead)
    ;
        MaybeBornVarsDelayedDead = no,
        BornVars = BornVarsGoal,
        DelayedDead = DelayedDeadGoal
    ).

% The kill_excess_delayed_dead_* functions are called on each branch of a
% branched control structure to make sure that all branches kill the same
% set of variables.

:- func kill_excess_delayed_dead_goal(set_of_progvar,
    pair(hlds_goal, set_of_progvar)) = hlds_goal.

kill_excess_delayed_dead_goal(FinalDelayedDead, Goal0 - DelayedDead0) = Goal :-
    set_of_var.difference(DelayedDead0, FinalDelayedDead, ToBeKilled),
    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    goal_info_get_post_deaths(GoalInfo0, PostDeath0),
    set_of_var.union(PostDeath0, ToBeKilled, PostDeath),
    goal_info_set_post_deaths(PostDeath, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- func kill_excess_delayed_dead_case(set_of_progvar,
    pair(case, set_of_progvar)) = case.

kill_excess_delayed_dead_case(FinalDelayedDead, Case0 - DelayedDead0) = Case :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    set_of_var.difference(DelayedDead0, FinalDelayedDead, ToBeKilled),
    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    goal_info_get_post_deaths(GoalInfo0, PostDeath0),
    set_of_var.union(PostDeath0, ToBeKilled, PostDeath),
    goal_info_set_post_deaths(PostDeath, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    Case = case(MainConsId, OtherConsIds, Goal).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_resume_points_in_goal(hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out, live_info::in,
    set_of_progvar::in) is det.

detect_resume_points_in_goal(Goal0, Goal, !Liveness, LiveInfo, ResumeVars0) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    goal_info_get_pre_deaths(GoalInfo0, PreDeaths0),
    goal_info_get_pre_births(GoalInfo0, PreBirths0),
    goal_info_get_post_deaths(GoalInfo0, PostDeaths0),
    goal_info_get_post_births(GoalInfo0, PostBirths0),

    set_of_var.difference(!.Liveness, PreDeaths0, !:Liveness),
    set_of_var.union(PreBirths0, !Liveness),

    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            detect_resume_points_in_conj(Goals0, Goals, !Liveness,
                LiveInfo, ResumeVars0)
        ;
            ConjType = parallel_conj,
            detect_resume_points_in_par_conj(Goals0, Goals, !Liveness,
                LiveInfo, ResumeVars0)
        ),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        CodeModel = goal_info_get_code_model(GoalInfo0),
        (
            CodeModel = model_non,
            detect_resume_points_in_non_disj(Goals0, Goals, !Liveness,
                LiveInfo, ResumeVars0, _)
        ;
            ( CodeModel = model_det
            ; CodeModel = model_semi
            ),
            detect_resume_points_in_pruned_disj(Goals0, Goals, !Liveness,
                LiveInfo, ResumeVars0, _)
        ),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CF, Cases0),
        detect_resume_points_in_cases(Cases0, Cases, !Liveness,
            LiveInfo, ResumeVars0),
        GoalExpr = switch(Var, CF, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        Liveness0 = !.Liveness,

        % Compute the set of variables that may be needed at the start
        % of the else part and attach this set to the condition.
        Else0 = hlds_goal(_ElseExpr0, ElseInfo0),
        goal_info_get_pre_deaths(ElseInfo0, ElsePreDeath0),
        set_of_var.difference(Liveness0, ElsePreDeath0, CondResumeVars0),
        maybe_complete_with_typeinfos(LiveInfo, CondResumeVars0,
            CondResumeVars1),
        % ResumeVars0 should already have been completed.
        set_of_var.union(CondResumeVars1, ResumeVars0, CondResumeVars),

        detect_resume_points_in_goal(Cond0, Cond1, Liveness0, LivenessCond,
            LiveInfo, CondResumeVars),
        detect_resume_points_in_goal(Then0, Then, LivenessCond, LivenessThen,
            LiveInfo, ResumeVars0),
        detect_resume_points_in_goal(Else0, Else, Liveness0, LivenessElse,
            LiveInfo, ResumeVars0),

        % Figure out which entry labels we need at the resumption point.
        % By minimizing the number of labels we use, we also minimize
        % the amount of data movement code we emit between such labels.
        ( if
            cannot_stack_flush(Cond1),
            CodeModel = goal_info_get_code_model(GoalInfo0),
            CodeModel \= model_non
        then
            CondResumeLocs = resume_locs_orig_only
        else if
            set_of_var.is_empty(CondResumeVars)
        then
            % There is no difference between orig_only and stack_only when
            % there are no resume variables, but some parts of code_info
            % insist on a stack label if e.g. the condition contains
            % commits, which is why we choose to use stack_only here.
            CondResumeLocs = resume_locs_stack_only
        else if
            cannot_fail_before_stack_flush(Cond1)
        then
            CondResumeLocs = resume_locs_stack_only
        else
            CondResumeLocs = resume_locs_stack_and_orig
        ),

        % Attach the set of variables needed after the condition
        % as the resume point set of the condition.
        make_and_set_resume_point(LiveInfo, CondResumeVars, CondResumeLocs,
            Cond1, Cond),

        require_equal(LivenessThen, LivenessElse, "if-then-else", LiveInfo),

        !:Liveness = LivenessThen,
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(SubGoal0),
        Liveness0 = !.Liveness,

        detect_resume_points_in_goal(SubGoal0, _, Liveness0, Liveness,
            LiveInfo, ResumeVars0),
        maybe_complete_with_typeinfos(LiveInfo, Liveness, CompletedLiveness),
        % ResumeVars0 should already have been completed.
        set_of_var.union(CompletedLiveness, ResumeVars0, ResumeVars1),
        detect_resume_points_in_goal(SubGoal0, SubGoal1, Liveness0, _Liveness,
            LiveInfo, ResumeVars1),

        % Figure out which entry labels we need at the resumption point.
        % By minimizing the number of labels we use, we also minimize
        % the amount of data movement code we emit between such labels.
        ( if cannot_stack_flush(SubGoal1) then
            ResumeLocs = resume_locs_orig_only
        else if cannot_fail_before_stack_flush(SubGoal1) then
            ResumeLocs = resume_locs_stack_only
        else
            ResumeLocs = resume_locs_stack_and_orig
        ),

        % Attach the set of variables alive after the negation
        % as the resume point set of the negated goal.
        make_and_set_resume_point(LiveInfo, ResumeVars1, ResumeLocs,
            SubGoal1, SubGoal),

        !:Liveness = Liveness,
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(TermVar, from_ground_term_construct)
        then
            maybe_complete_with_typeinfos(LiveInfo,
                set_of_var.make_singleton(TermVar), CompletedTermVars),
            set_of_var.union(CompletedTermVars, !Liveness),

            % There are no resume points in these scopes.
            SubGoal = SubGoal0
        else
            % XXX We could treat from_ground_term_deconstruct specially
            % as well.
            detect_resume_points_in_goal(SubGoal0, SubGoal, !Liveness,
                LiveInfo, ResumeVars0)
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        ( GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ),

    Goal = hlds_goal(GoalExpr, GoalInfo0),
    set_of_var.difference(!.Liveness, PostDeaths0, !:Liveness),
    set_of_var.union(PostBirths0, !Liveness).

:- pred detect_resume_points_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out,
    live_info::in, set_of_progvar::in) is det.

detect_resume_points_in_conj([], [], !Liveness, _, _).
detect_resume_points_in_conj([Goal0 | Goals0], [Goal | Goals],
        !Liveness, LiveInfo, ResumeVars0) :-
    detect_resume_points_in_goal(Goal0, Goal, !Liveness,
        LiveInfo, ResumeVars0),
    detect_resume_points_in_conj(Goals0, Goals, !Liveness,
        LiveInfo, ResumeVars0).

    % There are only two differences in the handling of pruned disjs versus
    % nondet disjs. First, for nondet disjunctions we always generate code
    % for all disjuncts, whereas for pruned disjunctions we stop generating
    % code after the first cannot_fail disjunct. Second, an empty pruned
    % disjunction is legal, while an empty nondet disjunction isn't.
    %
    % For both kinds of disjunctions, the resume points to be attached to
    % the non-last disjuncts must be completed with the required typeinfos
    % if --typeinfo-liveness is set. ResumeVars0 should already be so
    % completed, so we need only complete the sets added here. We therefore
    % perform this completion when we return the set of variables needed by
    % the last disjunct, and when we add to this set the set of variables
    % needed by a non-last disjunct.
    %
:- pred detect_resume_points_in_non_disj(list(hlds_goal)::in,
    list(hlds_goal)::out, set_of_progvar::in, set_of_progvar::out,
    live_info::in, set_of_progvar::in, set_of_progvar::out) is det.

detect_resume_points_in_non_disj([], _, _, _, _, _, _) :-
    unexpected($pred, "empty nondet disjunction").
detect_resume_points_in_non_disj([Goal0 | Goals0], [Goal | Goals],
        Liveness0, Liveness, LiveInfo, ResumeVars0, Needed) :-
    (
        % If there are any more disjuncts, then this disjunct
        % establishes a resumption point.
        Goals0 = [_ | _],
        detect_resume_points_in_non_disj(Goals0, Goals,
            Liveness0, LivenessRest, LiveInfo,
            ResumeVars0, NeededRest),
        detect_resume_points_in_non_last_disjunct(Goal0, Goal, no,
            Liveness0, LivenessRest, LiveInfo, ResumeVars0,
            Liveness, NeededRest, Needed)
    ;
        Goals0 = [],
        detect_resume_points_in_last_disjunct(Goal0, Goal,
            Liveness0, Liveness, LiveInfo, ResumeVars0, Needed),
        Goals = Goals0
    ).

:- pred detect_resume_points_in_pruned_disj(list(hlds_goal)::in,
    list(hlds_goal)::out, set_of_progvar::in, set_of_progvar::out,
    live_info::in, set_of_progvar::in, set_of_progvar::out) is det.

detect_resume_points_in_pruned_disj([], [], !Liveness, _, _, Needed) :-
    Needed = set_of_var.init.
detect_resume_points_in_pruned_disj([Goal0 | Goals0], [Goal | Goals],
        Liveness0, Liveness, LiveInfo, ResumeVars0, Needed) :-
    Goal0 = hlds_goal(_, GoalInfo0),
    Detism0 = goal_info_get_determinism(GoalInfo0),
    determinism_components(Detism0, CanFail0, _),
    ( if
        % This disjunct establishes a resumption point only if
        % there are more disjuncts *and* this one can fail.
        % If there are more disjuncts but this one can't fail,
        % then the code generator will ignore any later disjuncts,
        % so this one will be effectively the last.
        CanFail0 = can_fail,
        Goals0 = [_ | _]
    then
        detect_resume_points_in_pruned_disj(Goals0, Goals,
            Liveness0, LivenessRest, LiveInfo, ResumeVars0, NeededRest),
        detect_resume_points_in_non_last_disjunct(Goal0, Goal, yes,
            Liveness0, LivenessRest, LiveInfo, ResumeVars0,
            Liveness, NeededRest, Needed)
    else
        detect_resume_points_in_last_disjunct(Goal0, Goal,
            Liveness0, Liveness, LiveInfo, ResumeVars0, Needed),
        Goals = Goals0
    ).

:- pred detect_resume_points_in_non_last_disjunct(hlds_goal::in,hlds_goal::out,
    bool::in, set_of_progvar::in, set_of_progvar::in, live_info::in,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det.

detect_resume_points_in_non_last_disjunct(Goal0, Goal, MayUseOrigOnly,
        Liveness0, LivenessRest, LiveInfo, ResumeVars0,
        Liveness, NeededRest, Needed) :-
    % We must save a variable across this disjunct if it is needed
    % in a later disjunct or in an enclosing resume point
    set_of_var.union(NeededRest, ResumeVars0, ResumeVars1),
    detect_resume_points_in_goal(Goal0, Goal1, Liveness0, Liveness,
        LiveInfo, ResumeVars1),

    % Figure out which entry labels we need at the resumption point.
    % By minimizing the number of labels we use, we also minimize
    % the amount of data movement code we emit between such labels.
    ( if
        MayUseOrigOnly = yes,
        cannot_stack_flush(Goal1)
    then
        ResumeLocs = resume_locs_orig_only
    else if
        cannot_fail_before_stack_flush(Goal1)
    then
        ResumeLocs = resume_locs_stack_only
    else
        ResumeLocs = resume_locs_stack_and_orig
    ),

    % Attach the set of variables needed in the following disjuncts
    % as the resume point set of this disjunct.
    make_and_set_resume_point(LiveInfo, ResumeVars1, ResumeLocs, Goal1, Goal),

    Goal = hlds_goal(_, GoalInfo),
    goal_info_get_pre_deaths(GoalInfo, PreDeaths),
    set_of_var.difference(Liveness0, PreDeaths, NeededFirst),
    maybe_complete_with_typeinfos(LiveInfo, NeededFirst, CompletedNeededFirst),
    % NeededRest has already been completed.
    set_of_var.union(CompletedNeededFirst, NeededRest, Needed),

    require_equal(Liveness, LivenessRest, "disjunction", LiveInfo).

:- pred detect_resume_points_in_last_disjunct(hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out, live_info::in,
    set_of_progvar::in, set_of_progvar::out) is det.

detect_resume_points_in_last_disjunct(Goal0, Goal, Liveness0, Liveness,
        LiveInfo, ResumeVars0, CompletedNeeded) :-
    detect_resume_points_in_goal(Goal0, Goal, Liveness0, Liveness,
        LiveInfo, ResumeVars0),
    Goal = hlds_goal(_, GoalInfo),
    goal_info_get_pre_deaths(GoalInfo, PreDeaths),
    set_of_var.difference(Liveness0, PreDeaths, Needed),
    maybe_complete_with_typeinfos(LiveInfo, Needed, CompletedNeeded).

:- pred detect_resume_points_in_cases(list(case)::in, list(case)::out,
    set_of_progvar::in, set_of_progvar::out,
    live_info::in, set_of_progvar::in) is det.

detect_resume_points_in_cases([], [], !Liveness, _, _).
detect_resume_points_in_cases([Case0 | Cases0], [Case | Cases],
        Liveness0, LivenessFirst, LiveInfo, ResumeVars0) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    detect_resume_points_in_goal(Goal0, Goal, Liveness0, LivenessFirst,
        LiveInfo, ResumeVars0),
    Case = case(MainConsId, OtherConsIds, Goal),
    (
        Cases0 = [_ | _],
        detect_resume_points_in_cases(Cases0, Cases,
            Liveness0, LivenessRest, LiveInfo, ResumeVars0),
        require_equal(LivenessFirst, LivenessRest, "switch", LiveInfo)
    ;
        Cases0 = [],
        Cases = Cases0
    ).

:- pred detect_resume_points_in_par_conj(list(hlds_goal)::in,
    list(hlds_goal)::out, set_of_progvar::in, set_of_progvar::out,
    live_info::in, set_of_progvar::in) is det.

detect_resume_points_in_par_conj([], [], !Liveness, _, _).
detect_resume_points_in_par_conj([Goal0 | Goals0], [Goal | Goals],
        Liveness0, Liveness, LiveInfo, ResumeVars0) :-
    detect_resume_points_in_goal(Goal0, Goal, Liveness0, Liveness,
        LiveInfo, ResumeVars0),
    detect_resume_points_in_par_conj(Goals0, Goals,
        Liveness0, _LivenessRest, LiveInfo, ResumeVars0).

:- pred require_equal(set_of_progvar::in, set_of_progvar::in, string::in,
    live_info::in) is det.

require_equal(LivenessFirst, LivenessRest, GoalType, LiveInfo) :-
    ( if set_of_var.equal(LivenessFirst, LivenessRest) then
        true
    else
        VarSet = LiveInfo ^ li_varset,
        FirstVars = set_of_var.to_sorted_list(LivenessFirst),
        RestVars  = set_of_var.to_sorted_list(LivenessRest),
        FirstNames =
            mercury_vars_to_string(VarSet, print_name_and_num, FirstVars),
        RestNames =
            mercury_vars_to_string(VarSet, print_name_and_num, RestVars),
        Msg = "branches of " ++ GoalType ++ " disagree on liveness\n" ++
            "First: " ++ FirstNames ++ "\n" ++ "Rest:  " ++ RestNames ++ "\n",
        unexpected($pred, Msg)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

initial_liveness(ModuleInfo, PredInfo, ProcInfo, !:Liveness) :-
    proc_info_get_headvars(ProcInfo, Vars),
    proc_info_get_argmodes(ProcInfo, Modes),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    lookup_var_types(VarTypes, Vars, Types),
    !:Liveness = set_of_var.init,
    ( if initial_liveness_2(ModuleInfo, Vars, Types, Modes, !Liveness) then
        true
    else
        unexpected($pred, "length mismatch")
    ),

    % If a variable is unused in the goal, it shouldn't be in the initial
    % liveness. (If we allowed it to start live, it wouldn't ever become dead,
    % because it would have to be used to be killed). So we intersect the
    % headvars with the non-locals and (if doing typeinfo liveness calculation)
    % their typeinfo vars.

    module_info_get_globals(ModuleInfo, Globals),
    proc_info_get_goal(ProcInfo, hlds_goal(_Goal, GoalInfo)),
    NonLocals0 = goal_info_get_code_gen_nonlocals(GoalInfo),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    body_should_use_typeinfo_liveness(PredInfo, Globals, TypeinfoLiveness),
    maybe_complete_with_typeinfo_vars(NonLocals0, TypeinfoLiveness, VarTypes,
        RttiVarMaps, NonLocals),
    set_of_var.intersect(!.Liveness, NonLocals, !:Liveness).

:- pred initial_liveness_2(module_info::in,
    list(prog_var)::in, list(mer_type)::in, list(mer_mode)::in,
    set_of_progvar::in, set_of_progvar::out) is semidet.

initial_liveness_2(_ModuleInfo, [], [], [], !Liveness).
initial_liveness_2(ModuleInfo, [Var | Vars], [Type | Types], [Mode | Modes],
        !Liveness) :-
    ( if mode_to_top_functor_mode(ModuleInfo, Mode, Type, top_in) then
        set_of_var.insert(Var, !Liveness)
    else
        true
    ),
    initial_liveness_2(ModuleInfo, Vars, Types, Modes, !Liveness).

%-----------------------------------------------------------------------------%

    % Return the set of variables whose values are needed beyond the end
    % of the procedure (i.e. its output arguments).
    %
:- pred initial_deadness(module_info::in, proc_info::in, live_info::in,
    set_of_progvar::out) is det.

initial_deadness(ModuleInfo, ProcInfo, LiveInfo, Deadness) :-
    % The output arguments are all in the initial deadness.
    arg_info.partition_proc_args(ProcInfo, ModuleInfo, _, Deadness0, _),

    % If doing typeinfo liveness, the corresponding typeinfos need to be added
    % to these.
    proc_info_get_vartypes(ProcInfo, VarTypes),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    maybe_complete_with_typeinfo_vars(set_to_bitset(Deadness0),
        LiveInfo ^ li_typeinfo_liveness, VarTypes, RttiVarMaps, Deadness).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred add_liveness_after_goal(hlds_goal::in, set_of_progvar::in,
    hlds_goal::out) is det.

add_liveness_after_goal(Goal0, Residue, Goal) :-
    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    goal_info_get_post_births(GoalInfo0, PostBirths0),
    set_of_var.union(PostBirths0, Residue, PostBirths),
    goal_info_set_post_births(PostBirths, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred add_deadness_before_goal(set_of_progvar::in,
    hlds_goal::in, hlds_goal::out) is det.

add_deadness_before_goal(Residue, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    goal_info_get_pre_deaths(GoalInfo0, PreDeaths0),
    set_of_var.union(PreDeaths0, Residue, PreDeaths),
    goal_info_set_pre_deaths(PreDeaths, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Given a list of variables and an instmap delta, determine which
    % of those variables have a value given to them (i.e. they are bound
    % or aliased; in the latter case the "value" is the location they
    % should be stored in), and insert them into the accumulated set
    % of value-given vars.
    %
    % We don't handle the aliasing part yet.
    %
:- pred find_value_giving_occurrences(list(prog_var)::in, live_info::in,
    instmap_delta::in, set_of_progvar::in, set_of_progvar::out) is det.

find_value_giving_occurrences([], _, _, !ValueVars).
find_value_giving_occurrences([Var | Vars], LiveInfo, InstMapDelta,
        !ValueVars) :-
    VarTypes = LiveInfo ^ li_vartypes,
    lookup_var_type(VarTypes, Var, Type),
    ( if
        instmap_delta_search_var(InstMapDelta, Var, Inst),
        ModuleInfo = LiveInfo ^ li_module_info,
        mode_to_top_functor_mode(ModuleInfo, from_to_mode(free, Inst), Type,
            top_out)
    then
        set_of_var.insert(Var, !ValueVars)
    else
        true
    ),
    find_value_giving_occurrences(Vars, LiveInfo, InstMapDelta, !ValueVars).

%-----------------------------------------------------------------------------%

    % Get the nonlocals, and, if doing typeinfo liveness, add the
    % typeinfo vars for the nonlocals.
    %
:- pred get_nonlocals_and_typeinfos(live_info::in,
    hlds_goal_info::in, set_of_progvar::out, set_of_progvar::out) is det.

get_nonlocals_and_typeinfos(LiveInfo, GoalInfo,
        NonLocals, CompletedNonLocals) :-
    NonLocals = goal_info_get_code_gen_nonlocals(GoalInfo),
    maybe_complete_with_typeinfos(LiveInfo, NonLocals, CompletedNonLocals).

:- pred maybe_complete_with_typeinfos(live_info::in,
    set_of_progvar::in, set_of_progvar::out) is det.

maybe_complete_with_typeinfos(LiveInfo, Vars0, Vars) :-
    maybe_complete_with_typeinfo_vars(Vars0, LiveInfo ^ li_typeinfo_liveness,
        LiveInfo ^ li_vartypes, LiveInfo ^ li_rtti_varmaps, Vars).

%-----------------------------------------------------------------------------%

:- pred make_and_set_resume_point(live_info::in,
    set_of_progvar::in, resume_locs::in, hlds_goal::in, hlds_goal::out) is det.

make_and_set_resume_point(LiveInfo, ResumeVars0, ResumeLocs, Goal0, Goal) :-
    AllowPackingDummies = LiveInfo ^ li_allow_packing_dummies,
    (
        AllowPackingDummies = no,
        % Each dummy argument of a term is stored in a full word in the term's
        % memory cell, and we can copy this word to and from a register
        % or stack slot when creating and when restoring from resume points.
        ResumeVars = ResumeVars0
    ;
        AllowPackingDummies = yes,
        % Each dummy argument of a term is NOT stored ANYWHERE in the term's
        % memory cell, which means that when the code that establishes
        % the resume point tries to create the resume map (which maps
        % each variable in ResumeVars0 to a register or stack slot),
        % we have no source lval for the copying assignment.
        %
        % We could generalize the copying code to make it accept a source
        % rval (such as the integer constant zero) as the source, but that
        % would be suboptimal. Instead, we simply don't save the value
        % of dummy variables, and create the value in var_locn.m out of
        % thin air when (and if) it is ever needed.
        ModuleInfo = LiveInfo ^ li_module_info,
        VarTypes = LiveInfo ^ li_vartypes,
        set_of_var.filter(var_is_not_dummy_type(ModuleInfo, VarTypes),
            ResumeVars0, ResumeVars)
    ),
    Resume = resume_point(ResumeVars, ResumeLocs),
    goal_set_resume_point(Resume, Goal0, Goal).

:- pred var_is_not_dummy_type(module_info::in, vartypes::in, prog_var::in)
    is semidet.

var_is_not_dummy_type(ModuleInfo, VarTypes, Var) :-
    lookup_var_type(VarTypes, Var, Type),
    IsDummy = is_type_a_dummy(ModuleInfo, Type),
    IsDummy = is_not_dummy_type.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type live_info
    --->    live_info(
                li_module_info              :: module_info,
                li_typeinfo_liveness        :: bool,
                li_allow_packing_dummies    :: bool,
                li_varset                   :: prog_varset,
                li_vartypes                 :: vartypes,
                li_rtti_varmaps             :: rtti_varmaps
            ).

:- pred live_info_init(module_info::in, bool::in,
    prog_varset::in, vartypes::in, rtti_varmaps::in, live_info::out) is det.

live_info_init(ModuleInfo, TypeInfoLiveness, VarSet, VarTypes, RttiVarMaps,
        LiveInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, allow_packing_dummies,
        AllowPackingDummies),
    LiveInfo = live_info(ModuleInfo, TypeInfoLiveness, AllowPackingDummies,
        VarSet, VarTypes, RttiVarMaps).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.liveness.
%-----------------------------------------------------------------------------%
