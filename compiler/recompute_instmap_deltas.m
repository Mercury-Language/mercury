%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: recompute_instmap_deltas.m.
% Main author: fjh.
%
% Most code transformations invalidate the instmap deltas in the goal_infos
% of nonatomic goals, and some transformations invalidate these in atomic goals
% as well. This module recomputes these instmap deltas.
%
%---------------------------------------------------------------------------%

:- module check_hlds.recompute_instmap_deltas.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

%---------------------------------------------------------------------------%

:- type recomp_atomics
    --->    recomp_atomics
    ;       no_recomp_atomics.

    % Use the instmap deltas for all the atomic subgoals to recompute
    % the instmap deltas for all the non-atomic subgoals of a goal.
    % Used to ensure that the instmap deltas remain valid after code has
    % been re-arranged, e.g. by followcode. This also takes the module_info
    % as input and output since it may need to insert new merge_insts
    % into the merge_inst table. The first argument says whether the
    % instmap_deltas for calls and deconstruction unifications
    % should also recomputed. (For example, when common.m is run,
    % we have to recompute the instmap deltas of atomic goals because
    % some outputs of calls and deconstructions may have become non-local.)
    %
:- pred recompute_instmap_delta_proc(recomp_atomics::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

:- pred recompute_instmap_delta(recomp_atomics::in,
    var_table::in, inst_varset::in, instmap::in, hlds_goal::in, hlds_goal::out,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_abstract_unify.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.set_of_var.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type recompute_info
    --->    recompute_info(
                ri_module_info  :: module_info,
                ri_inst_varset  :: inst_varset
            ).

recompute_instmap_delta_proc(RecomputeAtomic, !ProcInfo, !ModuleInfo) :-
    proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InstMap0),
    proc_info_get_var_table(!.ProcInfo, VarTable),
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_inst_varset(!.ProcInfo, InstVarSet),
    recompute_instmap_delta(RecomputeAtomic, VarTable, InstVarSet, InstMap0,
        Goal0, Goal, !ModuleInfo),
    proc_info_set_goal(Goal, !ProcInfo).

recompute_instmap_delta(RecomputeAtomic, VarTable, InstVarSet, InstMap0,
        Goal0, Goal, ModuleInfo0, ModuleInfo) :-
    Params = recompute_params(RecomputeAtomic, vts_var_table(VarTable)),
    RI0 = recompute_info(ModuleInfo0, InstVarSet),
    recompute_instmap_delta_1(Params, InstMap0, _, Goal0, Goal, RI0, RI),
    ModuleInfo = RI ^ ri_module_info.

%---------------------------------------------------------------------------%

:- type recompute_params
    --->    recompute_params(
                recomp_atomics,
                var_type_source
            ).

:- pred recompute_instmap_delta_1(recompute_params::in,
    instmap::in, instmap_delta::out, hlds_goal::in, hlds_goal::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_1(Params, InstMap0, InstMapDelta, Goal0, Goal, !RI) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = switch(Var, Det, Cases0),
        ( if
            goal_info_has_feature(GoalInfo0, feature_mode_check_clauses_goal)
        then
            Cases = Cases0,
            InstMapDelta1 = goal_info_get_instmap_delta(GoalInfo0)
        else
            NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
            recompute_instmap_delta_switch(Params, Var, NonLocals0, InstMap0,
                InstMapDelta1, Cases0, Cases, !RI)
        ),
        GoalExpr = switch(Var, Det, Cases)
    ;
        GoalExpr0 = conj(ConjType, Conjuncts0),
        recompute_instmap_delta_conj(Params, InstMap0, InstMapDelta1,
            Conjuncts0, Conjuncts, !RI),
        GoalExpr = conj(ConjType, Conjuncts)
    ;
        GoalExpr0 = disj(Disjuncts0),
        ( if
            goal_info_has_feature(GoalInfo0, feature_mode_check_clauses_goal)
        then
            Disjuncts = Disjuncts0,
            InstMapDelta1 = goal_info_get_instmap_delta(GoalInfo0)
        else
            NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
            recompute_instmap_delta_disj(Params, NonLocals0, InstMap0,
                InstMapDelta1, Disjuncts0, Disjuncts, !RI)
        ),
        GoalExpr = disj(Disjuncts)
    ;
        GoalExpr0 = negation(SubGoal0),
        InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
        ( if instmap_delta_is_reachable(InstMapDelta0) then
            instmap_delta_init_reachable(InstMapDelta1)
        else
            instmap_delta_init_unreachable(InstMapDelta1)
        ),
        recompute_instmap_delta_1(Params, InstMap0, _, SubGoal0, SubGoal, !RI),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        recompute_instmap_delta_1(Params, InstMap0, InstMapDeltaCond,
            Cond0, Cond, !RI),
        apply_instmap_delta(InstMapDeltaCond, InstMap0, InstMapCond),
        recompute_instmap_delta_1(Params, InstMapCond, InstMapDeltaThen,
            Then0, Then, !RI),
        recompute_instmap_delta_1(Params, InstMap0, InstMapDeltaElse,
            Else0, Else, !RI),
        instmap_delta_apply_instmap_delta(InstMapDeltaCond, InstMapDeltaThen,
            test_size, InstMapDeltaCondThen),
        NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
        ModuleInfo0 = !.RI ^ ri_module_info,
        Params = recompute_params(_, VarTypeSrc),
        merge_instmap_delta(VarTypeSrc, NonLocals0, InstMap0,
            InstMapDeltaElse, InstMapDeltaCondThen, InstMapDelta1,
            ModuleInfo0, ModuleInfo),
        !RI ^ ri_module_info := ModuleInfo,
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if Reason = from_ground_term(_, FGT) then
            (
                ( FGT = from_ground_term_construct
                ; FGT = from_ground_term_deconstruct
                ),
                SubGoal = SubGoal0,
                SubGoal = hlds_goal(_, SubGoalInfo),
                InstMapDelta1 = goal_info_get_instmap_delta(SubGoalInfo)
            ;
                FGT = from_ground_term_initial,
                unexpected($pred, "from_ground_term_initial")
            ;
                FGT = from_ground_term_other,
                recompute_instmap_delta_1(Params, InstMap0, InstMapDelta1,
                    SubGoal0, SubGoal, !RI)
            )
        else
            recompute_instmap_delta_1(Params, InstMap0, InstMapDelta1,
                SubGoal0, SubGoal, !RI)
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = generic_call(_Details, Vars, Modes, _, Detism),
        Params = recompute_params(RecomputeAtomic, _),
        (
            RecomputeAtomic = no_recomp_atomics,
            InstMapDelta1 = goal_info_get_instmap_delta(GoalInfo0)
        ;
            RecomputeAtomic = recomp_atomics,
            ( if determinism_components(Detism, _, at_most_zero) then
                instmap_delta_init_unreachable(InstMapDelta1)
            else
                ModuleInfo = !.RI ^ ri_module_info,
                instmap_delta_from_mode_list(ModuleInfo, Vars, Modes,
                    InstMapDelta1)
            )
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = plain_call(PredId, ProcId, ArgVars, _BI, _UC, _Name),
        Params = recompute_params(RecomputeAtomic, _),
        (
            RecomputeAtomic = no_recomp_atomics,
            InstMapDelta1 = goal_info_get_instmap_delta(GoalInfo0)
        ;
            RecomputeAtomic = recomp_atomics,
            recompute_instmap_delta_call(Params, PredId, ProcId, ArgVars,
                InstMap0, InstMapDelta1, !RI)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = unify(LHS, RHS0, UniMode0, Uni, Context),
        (
            RHS0 = rhs_lambda_goal(Purity, Groundness, PorF, EvalMethod,
                LambdaNonLocals, ArgVarsModes, Det, SubGoal0),
            ModuleInfo0 = !.RI ^ ri_module_info,
            instmap.pre_lambda_update(ModuleInfo0, ArgVarsModes,
                InstMap0, InstMap),
            recompute_instmap_delta_1(Params, InstMap, _,
                SubGoal0, SubGoal, !RI),
            RHS = rhs_lambda_goal(Purity, Groundness, PorF, EvalMethod,
                LambdaNonLocals, ArgVarsModes, Det, SubGoal)
        ;
            ( RHS0 = rhs_var(_)
            ; RHS0 = rhs_functor(_, _, _)
            ),
            RHS = RHS0
        ),
        Params = recompute_params(RecomputeAtomic, _),
        (
            RecomputeAtomic = no_recomp_atomics,
            UniMode = UniMode0,
            InstMapDelta1 = goal_info_get_instmap_delta(GoalInfo0)
        ;
            RecomputeAtomic = recomp_atomics,
            recompute_instmap_delta_unify(Uni, UniMode0, UniMode,
                GoalInfo0, InstMap0, InstMapDelta1, !RI)
        ),
        GoalExpr = unify(LHS, RHS, UniMode, Uni, Context)
    ;
        GoalExpr0 = call_foreign_proc(_Attr, PredId, ProcId, Args, ExtraArgs,
            _MTRC, _Impl),
        Params = recompute_params(RecomputeAtomic, _),
        (
            RecomputeAtomic = no_recomp_atomics,
            InstMapDelta1 = goal_info_get_instmap_delta(GoalInfo0)
        ;
            RecomputeAtomic = recomp_atomics,
            ArgVars = list.map(foreign_arg_var, Args),
            recompute_instmap_delta_call(Params, PredId, ProcId, ArgVars,
                InstMap0, InstMapDelta0, !RI),
            (
                ExtraArgs = [],
                InstMapDelta1 = InstMapDelta0
            ;
                ExtraArgs = [_ | _],
                OldInstMapDelta = goal_info_get_instmap_delta(GoalInfo0),
                ExtraArgVars = list.map(foreign_arg_var, ExtraArgs),
                instmap_delta_restrict(set_of_var.list_to_set(ExtraArgVars),
                    OldInstMapDelta, ExtraArgsInstMapDelta),
                instmap_delta_apply_instmap_delta(InstMapDelta0,
                    ExtraArgsInstMapDelta, large_base, InstMapDelta1)
            )
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            Goals0 = [MainGoal0 | OrElseGoals0],
            NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
            recompute_instmap_delta_disj(Params, NonLocals0, InstMap0,
                InstMapDelta1, Goals0, Goals, !RI),
            (
                Goals = [],
                unexpected($pred, "Goals = []")
            ;
                Goals = [MainGoal | OrElseGoals]
            ),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            recompute_instmap_delta_1(Params, InstMap0, InstMapDelta1,
                SubGoal0, SubGoal, !RI),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ),
    % If the initial instmap is unreachable, so is the final instmap.
    ( if instmap_is_unreachable(InstMap0) then
        instmap_delta_init_unreachable(InstMapDelta)
    else
        NonLocals = goal_info_get_nonlocals(GoalInfo0),
        instmap_delta_restrict(NonLocals, InstMapDelta1, InstMapDelta)
    ),
    goal_info_set_instmap_delta(InstMapDelta, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%---------------------%

:- pred recompute_instmap_delta_conj(recompute_params::in,
    instmap::in, instmap_delta::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_conj(_, _, InstMapDelta, [], [], !RI) :-
    instmap_delta_init_reachable(InstMapDelta).
recompute_instmap_delta_conj(Params, InstMap0, InstMapDelta,
        [Goal0 | Goals0], [Goal | Goals], !RI) :-
    recompute_instmap_delta_1(Params, InstMap0, InstMapDelta0,
        Goal0, Goal, !RI),
    apply_instmap_delta(InstMapDelta0, InstMap0, InstMap1),
    recompute_instmap_delta_conj(Params, InstMap1, InstMapDelta1,
        Goals0, Goals, !RI),
    instmap_delta_apply_instmap_delta(InstMapDelta0, InstMapDelta1,
        large_overlay, InstMapDelta).

%---------------------%

:- pred recompute_instmap_delta_disj(recompute_params::in,
    set_of_progvar::in, instmap::in, instmap_delta::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_disj(Params, NonLocals, InstMap, InstMapDelta,
        Goals0, Goals, !RI) :-
    recompute_instmap_delta_disjuncts(Params, NonLocals, InstMap,
        Goals0, Goals, InstMapDeltas, !RI),
    (
        InstMapDeltas = [],
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        InstMapDeltas = [_ | _],
        Params = recompute_params(_, VarTypeSrc),
        ModuleInfo0 = !.RI ^ ri_module_info,
        merge_instmap_deltas(VarTypeSrc, NonLocals, InstMap, InstMapDeltas,
            InstMapDelta, ModuleInfo0, ModuleInfo),
        !RI ^ ri_module_info := ModuleInfo
    ).

:- pred recompute_instmap_delta_disjuncts(recompute_params::in,
    set_of_progvar::in, instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out, list(instmap_delta)::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_disjuncts(_Params, _NonLocals, _InstMap,
        [], [], [], !RI).
recompute_instmap_delta_disjuncts(Params, NonLocals, InstMap,
        [Goal0 | Goals0], [Goal | Goals],
        [InstMapDelta | InstMapDeltas], !RI) :-
    recompute_instmap_delta_1(Params, InstMap, InstMapDelta, Goal0, Goal, !RI),
    recompute_instmap_delta_disjuncts(Params, NonLocals, InstMap,
        Goals0, Goals, InstMapDeltas, !RI).

%---------------------%

:- pred recompute_instmap_delta_switch(recompute_params::in, prog_var::in,
    set_of_progvar::in, instmap::in, instmap_delta::out,
    list(case)::in, list(case)::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_switch(Params, Var, NonLocals, InstMap0, InstMapDelta,
        Cases0, Cases, !RI) :-
    recompute_instmap_delta_cases(Params, Var, NonLocals, InstMap0,
        Cases0, Cases, InstMapDeltas, !RI),
    (
        InstMapDeltas = [],
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        InstMapDeltas = [_ | _],
        Params = recompute_params(_, VarTypeSrc),
        ModuleInfo0 = !.RI ^ ri_module_info,
        merge_instmap_deltas(VarTypeSrc, NonLocals, InstMap0, InstMapDeltas,
            InstMapDelta, ModuleInfo0, ModuleInfo),
        !RI ^ ri_module_info := ModuleInfo
    ).

:- pred recompute_instmap_delta_cases(recompute_params::in, prog_var::in,
    set_of_progvar::in, instmap::in,
    list(case)::in, list(case)::out, list(instmap_delta)::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_cases(_Params, _Var, _NonLocals, _InstMap,
        [], [], [], !RI).
recompute_instmap_delta_cases(Params, Var, NonLocals, InstMap0,
        [Case0 | Cases0], [Case | Cases],
        [InstMapDelta | InstMapDeltas], !RI) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    Params = recompute_params(_, VarTypeSrc),
    lookup_var_type_in_source(VarTypeSrc, Var, Type),
    ModuleInfo0 = !.RI ^ ri_module_info,
    bind_var_to_functors(Var, Type, MainConsId, OtherConsIds,
        InstMap0, InstMap1, ModuleInfo0, ModuleInfo1),
    !RI ^ ri_module_info := ModuleInfo1,
    recompute_instmap_delta_1(Params, InstMap1, InstMapDelta0,
        Goal0, Goal, !RI),
    ModuleInfo2 = !.RI ^ ri_module_info,
    instmap_delta_bind_var_to_functors(Var, Type, MainConsId, OtherConsIds,
        InstMap0, InstMapDelta0, InstMapDelta, ModuleInfo2, ModuleInfo3),
    !RI ^ ri_module_info := ModuleInfo3,
    Case = case(MainConsId, OtherConsIds, Goal),

    recompute_instmap_delta_cases(Params, Var, NonLocals, InstMap0,
        Cases0, Cases, InstMapDeltas, !RI).

%---------------------%

:- pred recompute_instmap_delta_call(recompute_params::in,
    pred_id::in, proc_id::in, list(prog_var)::in,
    instmap::in, instmap_delta::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_call(Params, PredId, ProcId, ArgVars, InstMap,
        InstMapDelta, !RI) :-
    ModuleInfo0 = !.RI ^ ri_module_info,
    module_info_pred_proc_info(ModuleInfo0, PredId, ProcId, _, ProcInfo),
    proc_info_interface_determinism(ProcInfo, Detism),
    ( if determinism_components(Detism, _, at_most_zero) then
        instmap_delta_init_unreachable(InstMapDelta)
    else
        proc_info_get_argmodes(ProcInfo, ArgModes0),
        proc_info_get_inst_varset(ProcInfo, ProcInstVarSet),
        InstVarSet0 = !.RI ^ ri_inst_varset,
        rename_apart_inst_vars(InstVarSet0, ProcInstVarSet, InstVarSet,
            ArgModes0, ArgModes1),
        !RI ^ ri_inst_varset := InstVarSet,
        mode_list_get_initial_insts(ModuleInfo0, ArgModes1, InitialInsts),

        % Compute the inst_var substitution from the initial insts
        % of the called procedure and the insts of the argument variables.
        ( if instmap_is_reachable(InstMap) then
            map.init(InstVarSub0),
            Params = recompute_params(_, VarTypeSrc),
            compute_inst_var_sub(VarTypeSrc, InstMap, ArgVars, InitialInsts,
                InstVarSub0, InstVarSub, ModuleInfo0, ModuleInfo1),

            % Apply the inst_var substitution to the argument modes.
            mode_list_apply_substitution(InstVarSub, ArgModes1, ArgModes2),

            % Calculate the final insts of the argument variables from their
            % initial insts and the final insts of the called procedure
            % (with inst_var substitutions applied).
            recompute_instmap_delta_call_args(ArgVars, InstMap,
                ArgModes2, ArgModes, ModuleInfo1, ModuleInfo),
            !RI ^ ri_module_info := ModuleInfo
        else
            list.length(ArgVars, NumArgVars),
            list.duplicate(NumArgVars, from_to_mode(not_reached, not_reached),
                ArgModes),
            ModuleInfo = ModuleInfo0
        ),
        instmap_delta_from_mode_list(ModuleInfo, ArgVars, ArgModes,
            InstMapDelta)
    ).

:- pred compute_inst_var_sub(var_type_source::in, instmap::in,
    list(prog_var)::in, list(mer_inst)::in,
    inst_var_sub::in, inst_var_sub::out,
    module_info::in, module_info::out) is det.

compute_inst_var_sub(_, _, [], [], !Sub, !ModuleInfo).
compute_inst_var_sub(_, _, [_ | _], [], !Sub, !ModuleInfo) :-
    unexpected($pred, "length mismatch").
compute_inst_var_sub(_, _, [], [_ | _], !Sub, !ModuleInfo) :-
    unexpected($pred, "length mismatch").
compute_inst_var_sub(VarTypeSrc, InstMap, [ArgVar | ArgVars], [Inst | Insts],
        !Sub, !ModuleInfo) :-
    % This is similar to modecheck_var_has_inst.
    SaveModuleInfo = !.ModuleInfo,
    SaveSub = !.Sub,
    instmap_lookup_var(InstMap, ArgVar, ArgInst),
    lookup_var_type_in_source(VarTypeSrc, ArgVar, Type),
    ( if inst_matches_initial_sub(Type, ArgInst, Inst, !ModuleInfo, !Sub) then
        true
    else
        % error("compute_inst_var_sub: " ++
        %   ++ "inst_matches_initial failed")
        % XXX  We shouldn't ever get here, but unfortunately
        % the mode system currently has several problems (most
        % noticeably lack of alias tracking for unique modes)
        % which mean inst_matches_initial can sometimes fail here.
        !:ModuleInfo = SaveModuleInfo,
        !:Sub = SaveSub
    ),
    compute_inst_var_sub(VarTypeSrc, InstMap, ArgVars, Insts,
        !Sub, !ModuleInfo).

:- pred recompute_instmap_delta_call_args(list(prog_var)::in, instmap::in,
    list(mer_mode)::in, list(mer_mode)::out, module_info::in, module_info::out)
    is det.

recompute_instmap_delta_call_args([], _, [], [], !ModuleInfo).
recompute_instmap_delta_call_args([_ | _], _, [], _, !ModuleInfo) :-
    unexpected($pred, "length mismatch").
recompute_instmap_delta_call_args([], _, [_ | _], _, !ModuleInfo) :-
    unexpected($pred, "length mismatch").
recompute_instmap_delta_call_args([Arg | Args], InstMap, [Mode0 | Modes0],
        [Mode | Modes], !ModuleInfo) :-
    % This is similar to modecheck_set_var_inst.
    instmap_lookup_var(InstMap, Arg, ArgInst0),
    mode_get_insts(!.ModuleInfo, Mode0, _, FinalInst),
    ( if
        % The is_dead allows abstractly_unify_inst to succeed when
        % some parts of ArgInst0 and the corresponding parts of FinalInst
        % are free.
        % XXX There should be a better way to communicate that information.
        abstractly_unify_inst(is_dead, ArgInst0, FinalInst,
            fake_unify, UnifyInst, _, !ModuleInfo)
    then
        Mode = from_to_mode(ArgInst0, UnifyInst)
    else
        unexpected($pred, "unify_inst failed")
    ),
    recompute_instmap_delta_call_args(Args, InstMap, Modes0, Modes,
        !ModuleInfo).

%---------------------%

:- pred recompute_instmap_delta_unify(unification::in, unify_mode::in,
    unify_mode::out, hlds_goal_info::in, instmap::in, instmap_delta::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_unify(Unification, UniMode0, UniMode, GoalInfo,
        InstMap, InstMapDelta, !RI) :-
    % Deconstructions are the only types of unifications that can require
    % updating of the instmap_delta after simplify.m has been run.
    % Type specialization may require constructions of type-infos,
    % typeclass-infos or predicate constants to be added to the
    % instmap_delta.
    ModuleInfo0 = !.RI ^ ri_module_info,
    (
        Unification = deconstruct(LHSVar, _ConsId, RHSVars, ArgModes,
            _, _CanCGC),

        % Get the final inst of the deconstructed var, which will be the same
        % as in the old instmap.

        OldInstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        instmap_lookup_var(InstMap, LHSVar, LHSInitialInst),
        ( if instmap_delta_search_var(OldInstMapDelta, LHSVar, DeltaInst) then
            % Inlining can result in situations where the initial inst
            % (from procedure 1) can decide that a variable must be bound
            % to one set of function symbols, while the instmap delta from
            % a later unification (from procedure 2) can say that it is bound
            % to a different, non-overlapping set of function symbols.
            %
            % The is_dead allows abstractly_unify_inst to succeed when some
            % parts of InitialInst and the corresponding parts of DeltaInst
            % are free.
            % XXX There should be a better way to communicate that information.
            ( if
                abstractly_unify_inst(is_dead, LHSInitialInst, DeltaInst,
                    fake_unify, LHSFinalInstPrime, _Detism,
                    ModuleInfo0, ModuleInfo1)
            then
                LHSFinalInst = LHSFinalInstPrime,
                ModuleInfo = ModuleInfo1,
                !RI ^ ri_module_info := ModuleInfo
            else
                unexpected($pred, "abstractly_unify_inst failed")
            )
        else
            % It wasn't in the instmap_delta, so the inst didn't change.
            LHSFinalInst = LHSInitialInst,
            ModuleInfo = ModuleInfo0
        ),
        LHSTuple = var_init_final_insts(LHSVar, LHSInitialInst, LHSFinalInst),
        pair_arg_vars_with_rhs_insts(RHSVars, ArgModes, RHSTuples),
        instmap_delta_from_var_init_final_insts(ModuleInfo,
            [LHSTuple | RHSTuples], InstMapDelta),
        UniMode = UniMode0
    ;
        Unification = construct(Var, ConsId, Args, _, _, _, _),
        ( if
            NonLocals = goal_info_get_nonlocals(GoalInfo),
            set_of_var.member(NonLocals, Var),
            OldInstMapDelta = goal_info_get_instmap_delta(GoalInfo),
            not instmap_delta_search_var(OldInstMapDelta, Var, _),
            MaybeInst = cons_id_to_shared_inst(ModuleInfo0, ConsId,
                list.length(Args)),
            MaybeInst = yes(Inst)
        then
            UniMode = UniMode0,
            instmap_delta_init_reachable(InstMapDelta0),
            instmap_delta_set_var(Var, Inst, InstMapDelta0, InstMapDelta)
        else
            InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
            UniMode = UniMode0
        )
    ;
        ( Unification = assign(_, _)
        ; Unification = simple_test(_, _)
        ; Unification = complicated_unify(_, _, _)
        ),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        UniMode = UniMode0
    ).

:- pred pair_arg_vars_with_rhs_insts(list(prog_var)::in, list(unify_mode)::in,
    list(var_init_final_insts)::out) is det.

pair_arg_vars_with_rhs_insts([], [], []).
pair_arg_vars_with_rhs_insts([], [_ | _], _) :-
    unexpected($pred, "mismatched list lengths").
pair_arg_vars_with_rhs_insts([_ | _], [], _) :-
    unexpected($pred, "mismatched list lengths").
pair_arg_vars_with_rhs_insts([RHSVar | RHSVars], [UnifyMode | UnifyModes],
        [Tuple | Tuples]) :-
    UnifyMode = unify_modes_li_lf_ri_rf(_, _, InitRHS, FinalRHS),
    Tuple = var_init_final_insts(RHSVar, InitRHS, FinalRHS),
    pair_arg_vars_with_rhs_insts(RHSVars, UnifyModes, Tuples).

    % For a builtin constructor, return the inst of the constructed term.
    % Handling user-defined constructors properly would require running
    % mode analysis again.
    %
:- func cons_id_to_shared_inst(module_info, cons_id, int) = maybe(mer_inst).

cons_id_to_shared_inst(ModuleInfo, ConsId, NumArgs) = MaybeInst :-
    (
        ( ConsId = cons(_, _, _)
        ; ConsId = tuple_cons(_)
        ),
        MaybeInst = no
    ;
        % Note that before the change that introduced the char_const functor,
        % we used to handle character constants as user-defined cons_ids.
        ( ConsId = some_int_const(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ),
        MaybeInst = yes(bound(shared, inst_test_results_fgtc,
            [bound_functor(ConsId, [])]))
    ;
        ConsId = impl_defined_const(_),
        unexpected($pred, "impl_defined_const")
    ;
        ConsId = closure_cons(PredProcId, _),
        module_info_pred_proc_info(ModuleInfo,
            unshroud_pred_proc_id(PredProcId), PredInfo, ProcInfo),
        PorF = pred_info_is_pred_or_func(PredInfo),
        proc_info_interface_determinism(ProcInfo, Det),
        proc_info_get_argmodes(ProcInfo, ProcArgModes),
        list.det_drop(NumArgs, ProcArgModes, Modes),
        Inst = ground(shared, higher_order(pred_inst_info(PorF, Modes,
            arg_reg_types_unset, Det))),
        MaybeInst = yes(Inst)
    ;
        ( ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ; ConsId = ground_term_const(_, _)
        ; ConsId = tabling_info_const(_)
        ; ConsId = table_io_entry_desc(_)
        ; ConsId = deep_profiling_proc_layout(_)
        ),
        MaybeInst = yes(ground(shared, none_or_default_func))
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.recompute_instmap_deltas.
%---------------------------------------------------------------------------%
