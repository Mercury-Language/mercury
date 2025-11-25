%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: switch_detection.m.
% Main authors: fjh, zs.
%
% Switch detection - when a disjunction contains disjuncts that unify the
% same input variable with different function symbols, replace (part of)
% the disjunction with a switch.
%
%---------------------------------------------------------------------------%

:- module check_hlds.switch_detection.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- type switch_detect_info.

:- func init_switch_detect_info(module_info) = switch_detect_info.

:- pred detect_switches_in_module(io.text_output_stream::in,
    module_info::in, module_info::out) is det.

:- pred detect_switches_in_proc(switch_detect_info::in,
    proc_info::in, proc_info::out) is det.

%---------------------------------------------------------------------------%

:- type found_deconstruct
    --->    did_find_deconstruct
    ;       did_not_find_deconstruct.

:- inst goal_expr_deconstruct for hlds_goal_expr/0
    --->    unify(ground, ground, ground, unification_deconstruct, ground).

:- type process_unify(Result, Info) ==
    pred(prog_var, hlds_goal_expr, hlds_goal_info, list(hlds_goal),
        Result, Result, Info, Info).
:- inst process_unify ==
    (pred(in, in(goal_expr_deconstruct), in, out,
        in, out, in, out) is det).

    % find_bind_var(Var, ProcessUnify, Goal0, Goal, !Result, !Info,
    %   FoundDeconstruct):
    %
    % Used by both switch_detection and cse_detection. Searches through
    % Goal0 looking for the first deconstruction unification with Var
    % or an alias of Var. If find_bind_var finds a deconstruction unification
    % of the variable, it calls ProcessUnify to handle it (which may replace
    % the unification with some other goals, which is why we return Goal),
    % and it stops searching. If it doesn't find such a deconstruction,
    % find_bind_var leaves !Result unchanged.
    %
:- pred find_bind_var(prog_var::in,
    process_unify(Result, Info)::in(process_unify),
    hlds_goal::in, hlds_goal::out, Result::in, Result::out,
    Info::in, Info::out, found_deconstruct::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_util.
:- import_module hlds.goal_path.
:- import_module hlds.goal_refs.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_desc.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.inst_test.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module hlds.type_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module term_subst.
:- import_module term_unify.
:- import_module unit.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type allow_multi_arm
    --->    allow_multi_arm
    ;       do_not_allow_multi_arm.

:- type switch_detect_info
    --->    switch_detect_info(
                sdi_module_info     :: module_info,
                sdi_allow_multi_arm :: allow_multi_arm
            ).

:- pred lookup_allow_multi_arm(module_info::in, allow_multi_arm::out) is det.

lookup_allow_multi_arm(ModuleInfo, AllowMulti) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, allow_multi_arm_switches, Allow),
    (
        Allow = yes,
        AllowMulti = allow_multi_arm
    ;
        Allow = no,
        AllowMulti = do_not_allow_multi_arm
    ).

init_switch_detect_info(ModuleInfo) = Info :-
    lookup_allow_multi_arm(ModuleInfo, AllowMulti),
    Info = switch_detect_info(ModuleInfo, AllowMulti).

%---------------------------------------------------------------------------%

detect_switches_in_module(ProgressStream, !ModuleInfo) :-
    % Traverse the module structure, calling detect_switches_in_goal
    % for each procedure body.
    Info = init_switch_detect_info(!.ModuleInfo),
    module_info_get_valid_pred_ids(!.ModuleInfo, ValidPredIds),
    ValidPredIdSet = set_tree234.list_to_set(ValidPredIds),

    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    map.to_assoc_list(PredIdTable0, PredIdsInfos0),

    detect_switches_in_preds(ProgressStream, Info, ValidPredIdSet,
        PredIdsInfos0, PredIdsInfos),
    map.from_sorted_assoc_list(PredIdsInfos, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

:- pred detect_switches_in_preds(io.text_output_stream::in,
    switch_detect_info::in, set_tree234(pred_id)::in,
    assoc_list(pred_id, pred_info)::in, assoc_list(pred_id, pred_info)::out)
    is det.

detect_switches_in_preds(_, _, _, [], []).
detect_switches_in_preds(ProgressStream, Info, ValidPredIdSet,
        [PredIdInfo0 | PredIdsInfos0], [PredIdInfo | PredIdsInfos]) :-
    PredIdInfo0 = PredId - PredInfo0,
    ( if set_tree234.contains(ValidPredIdSet, PredId) then
        detect_switches_in_pred(ProgressStream, Info, PredId,
            PredInfo0, PredInfo),
        PredIdInfo = PredId - PredInfo
    else
        PredIdInfo = PredIdInfo0
    ),
    detect_switches_in_preds(ProgressStream, Info, ValidPredIdSet,
        PredIdsInfos0, PredIdsInfos).

:- pred detect_switches_in_pred(io.text_output_stream::in,
    switch_detect_info::in, pred_id::in, pred_info::in, pred_info::out) is det.

detect_switches_in_pred(ProgressStream, Info, PredId, !PredInfo) :-
    NonImportedProcIds = pred_info_all_non_imported_procids(!.PredInfo),
    (
        NonImportedProcIds = [_ | _],
        trace [io(!IO)] (
            ModuleInfo = Info ^ sdi_module_info,
            maybe_write_pred_progress_message(ProgressStream, ModuleInfo,
                "Detecting switches in", PredId, !IO)
        ),
        pred_info_get_proc_table(!.PredInfo, ProcTable0),
        map.to_assoc_list(ProcTable0, ProcList0),
        detect_switches_in_procs(Info, NonImportedProcIds,
            ProcList0, ProcList),
        map.from_sorted_assoc_list(ProcList, ProcTable),
        pred_info_set_proc_table(ProcTable, !PredInfo)

        % This is where we should print statistics, if we ever need
        % to debug the performance of switch detection.
    ;
        NonImportedProcIds = []
    ).

:- pred detect_switches_in_procs(switch_detect_info::in, list(proc_id)::in,
    assoc_list(proc_id, proc_info)::in, assoc_list(proc_id, proc_info)::out)
    is det.

detect_switches_in_procs(_Info, _NonImportedProcIds, [], []).
detect_switches_in_procs(Info, NonImportedProcIds,
        [ProcIdInfo0 | ProcIdsInfos0], [ProcIdInfo | ProcIdsInfos]) :-
    ProcIdInfo0 = ProcId - ProcInfo0,
    ( if list.member(ProcId, NonImportedProcIds) then
        detect_switches_in_proc(Info, ProcInfo0, ProcInfo),
        ProcIdInfo = ProcId - ProcInfo
    else
        ProcIdInfo = ProcIdInfo0
    ),
    detect_switches_in_procs(Info, NonImportedProcIds,
        ProcIdsInfos0, ProcIdsInfos).

%---------------------------------------------------------------------------%

detect_switches_in_proc(Info, !ProcInfo) :-
    Info = switch_detect_info(ModuleInfo, AllowMulti),

    % XXX TODO Deforest with scout_disjunctions_in_goal.
    fill_goal_id_slots_in_proc(ModuleInfo, _ContainingGoalMap, !ProcInfo),
    proc_info_get_var_table(!.ProcInfo, VarTable),
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_initial_instmap(ModuleInfo, !.ProcInfo, InstMap0),
    map.init(DisjunctionInfoMap0),
    map.init(DisjunctInfoMap0),
    ScoutInfo0 = scout_disj_info(ModuleInfo, VarTable,
        DisjunctionInfoMap0, DisjunctInfoMap0),
    SubstDb0 = init_subst_db,
    scout_disjunctions_in_goal(Goal0, InstMap0, _InstMap,
        not_in_zone, _, SubstDb0, _, ScoutInfo0, ScoutInfo),

    trace [runtime(env("SCOUT_DISJUNCTIONS")), io(!IO)] (
        io.stderr_stream(StrErr, !IO),
        varset.init(TVarSet),
        proc_info_get_inst_varset(!.ProcInfo, InstVarSet),
        io.write_string(StrErr, "\nPROC BODY\n", !IO),
        dump_goal_nl(StrErr, ModuleInfo, vns_var_table(VarTable),
            TVarSet, InstVarSet, Goal0, !IO),
        ScoutInfo = scout_disj_info(_, _, DisjunctionInfoMap, _),
        DisjunctionInfoMapStr =
            disjunction_info_map_to_string(VarTable, DisjunctionInfoMap),
        io.write_string(StrErr, DisjunctionInfoMapStr, !IO)
    ),

    % To process each ProcInfo, we get the goal, initialize the instmap
    % based on the modes of the head vars, and pass these to
    % `detect_switches_in_goal'.
    Requant0 = do_not_need_to_requantify,
    BodyDeletedCallCallees0 = set.init,
    LocalInfo0 = local_switch_detect_info(VarTable, AllowMulti,
        ScoutInfo, ModuleInfo, Requant0, BodyDeletedCallCallees0),

    detect_switches_in_goal(InstMap0, nrsv, Goal0, Goal,
        LocalInfo0, LocalInfo),
    proc_info_set_goal(Goal, !ProcInfo),
    LocalInfo = local_switch_detect_info(_VarTable, _AllowMulti,
        _MaybeScoutInfo, _ModuleInfo, Requant, BodyDeletedCallCallees),
    (
        Requant = need_to_requantify,
        requantify_proc_general(ord_nl_maybe_lambda, !ProcInfo)
    ;
        Requant = do_not_need_to_requantify
    ),
    proc_info_get_deleted_call_callees(!.ProcInfo, DeletedCallCallees0),
    set.union(BodyDeletedCallCallees,
        DeletedCallCallees0, DeletedCallCallees),
    proc_info_set_deleted_call_callees(DeletedCallCallees, !ProcInfo).

%---------------------------------------------------------------------------%

:- type local_switch_detect_info
    --->    local_switch_detect_info(
                % These fields are read-only.
                lsdi_var_table          :: var_table,
                lsdi_allow_multi_arm    :: allow_multi_arm,
                lsdi_scout_disj_info    :: scout_disj_info,

                % These fields are read-write.
                %
                % We update module_info when we enter a switch arm, and update
                % the inst of the switched-on variable to record it being bound
                % to one of the cons_ids that the switch arm is for.
                % XXX However, detect_switches_in_proc then throws away
                % the updated module_info.
                lsdi_module_info        :: module_info,
                lsdi_requant            :: need_to_requantify,
                lsdi_deleted_callees    :: set(pred_proc_id)
            ).

:- type maybe_required_switch_var
    --->    nrsv
            % "no required switch var": This goal is not inside a scope
            % that requires its top level goal to be a switch on a specific
            % variable.
    ;       rsv(prog_var).
            % "required switch var": This goal *is* inside a scope
            % that requires its top level goal to be a switch on *this*
            % variable.

:- pred detect_switches_in_goal(instmap::in, maybe_required_switch_var::in,
    hlds_goal::in, hlds_goal::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_switches_in_goal(InstMap0, MaybeRequiredVar, Goal0, Goal, !LocalInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = disj(Disjuncts0),
        (
            Disjuncts0 = [],
            GoalExpr = GoalExpr0
        ;
            Disjuncts0 = [_ | _],
            detect_switches_in_disj(InstMap0, MaybeRequiredVar,
                Disjuncts0, GoalInfo0, GoalExpr, !LocalInfo)
        )
    ;
        GoalExpr0 = conj(ConjType, Conjuncts0),
        detect_switches_in_conj(InstMap0, Conjuncts0, Conjuncts, !LocalInfo),
        GoalExpr = conj(ConjType, Conjuncts)
    ;
        GoalExpr0 = negation(SubGoal0),
        detect_switches_in_goal(InstMap0, nrsv, SubGoal0, SubGoal, !LocalInfo),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        detect_switches_in_goal(InstMap0, nrsv, Cond0, Cond, !LocalInfo),
        apply_goal_instmap_delta(Cond0, InstMap0, InstMap1),
        detect_switches_in_goal(InstMap1, nrsv, Then0, Then, !LocalInfo),
        detect_switches_in_goal(InstMap0, nrsv, Else0, Else, !LocalInfo),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        detect_switches_in_cases(Var, InstMap0, Cases0, Cases, !LocalInfo),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        (
            Reason = from_ground_term(_, from_ground_term_construct),
            % There are neither disjunctions nor deconstruction unifications
            % inside these scopes.
            %
            % XXX We could treat from_ground_term_deconstruct specially
            % as well, since the only variable whose deconstruction could be of
            % interest here is the one named in Reason.
            SubGoal = SubGoal0
        ;
            ( Reason = from_ground_term(_, from_ground_term_initial)
            ; Reason = from_ground_term(_, from_ground_term_deconstruct)
            ; Reason = from_ground_term(_, from_ground_term_other)
            ; Reason = exist_quant(_, _)
            ; Reason = disable_warnings(_, _)
            ; Reason = promise_solutions(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            ; Reason = loop_control(_, _, _)
            ),
            detect_switches_in_goal(InstMap0, nrsv, SubGoal0, SubGoal,
                !LocalInfo)
        ;
            ( Reason = require_complete_switch(RequiredVar)
            ; Reason = require_switch_arms_detism(RequiredVar, _)
            ),
            detect_switches_in_goal(InstMap0, rsv(RequiredVar),
                SubGoal0, SubGoal, !LocalInfo)
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = unify(_, RHS0, _, _, _),
        (
            RHS0 = rhs_lambda_goal(_, _, _, _, VarsModes, _, LambdaGoal0),
            % We need to insert the initial insts for the lambda variables
            % in the instmap before processing the lambda goal.
            ModuleInfo = !.LocalInfo ^ lsdi_module_info,
            instmap.pre_lambda_update(ModuleInfo, VarsModes,
                InstMap0, InstMap1),
            detect_switches_in_goal(InstMap1, nrsv, LambdaGoal0, LambdaGoal,
                !LocalInfo),
            RHS = RHS0 ^ rhs_lambda_goal := LambdaGoal,
            GoalExpr = GoalExpr0 ^ unify_rhs := RHS
        ;
            ( RHS0 = rhs_var(_)
            ; RHS0 = rhs_functor(_, _, _)
            ),
            GoalExpr = GoalExpr0
        )
    ;
        ( GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            detect_switches_in_goal(InstMap0, nrsv, MainGoal0, MainGoal,
                !LocalInfo),
            detect_switches_in_orelse(InstMap0, OrElseGoals0, OrElseGoals,
                !LocalInfo),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            detect_switches_in_goal(InstMap0, nrsv, SubGoal0, SubGoal,
                !LocalInfo),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo0).

:- pred detect_sub_switches_in_disj(instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_sub_switches_in_disj(_, [], [], !LocalInfo).
detect_sub_switches_in_disj(InstMap, [Goal0 | Goals0], [Goal | Goals],
        !LocalInfo) :-
    detect_switches_in_goal(InstMap, nrsv, Goal0, Goal, !LocalInfo),
    detect_sub_switches_in_disj(InstMap, Goals0, Goals, !LocalInfo).

:- pred detect_switches_in_cases(prog_var::in, instmap::in,
    list(case)::in, list(case)::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_switches_in_cases(_, _, [], [], !LocalInfo).
detect_switches_in_cases(Var, InstMap0,
        [Case0 | Cases0], [Case | Cases], !LocalInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    VarTable = !.LocalInfo ^ lsdi_var_table,
    lookup_var_type(VarTable, Var, VarType),
    ModuleInfo0 = !.LocalInfo ^ lsdi_module_info,
    bind_var_to_functors(Var, VarType, MainConsId, OtherConsIds,
        InstMap0, InstMap1, ModuleInfo0, ModuleInfo),
    !LocalInfo ^ lsdi_module_info := ModuleInfo,
    detect_switches_in_goal(InstMap1, nrsv, Goal0, Goal, !LocalInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    detect_switches_in_cases(Var, InstMap0, Cases0, Cases, !LocalInfo).

:- pred detect_switches_in_conj(instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_switches_in_conj(_, [], [], !LocalInfo).
detect_switches_in_conj(InstMap0,
        [Goal0 | Goals0], [Goal | Goals], !LocalInfo) :-
    detect_switches_in_goal(InstMap0, nrsv, Goal0, Goal, !LocalInfo),
    apply_goal_instmap_delta(Goal0, InstMap0, InstMap1),
    detect_switches_in_conj(InstMap1, Goals0, Goals, !LocalInfo).

:- pred detect_switches_in_orelse(instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_switches_in_orelse(_, [], [], !LocalInfo).
detect_switches_in_orelse(InstMap,
        [Goal0 | Goals0], [Goal | Goals], !LocalInfo) :-
    detect_switches_in_goal(InstMap, nrsv, Goal0, Goal, !LocalInfo),
    detect_switches_in_orelse(InstMap, Goals0, Goals, !LocalInfo).

%---------------------------------------------------------------------------%

:- type case_arm
    --->    single_cons_id_arm(cons_id, hlds_goal)
    ;       multi_cons_id_arm(cons_id, list(cons_id), hlds_goal).

:- type cons_id_state
    --->    cons_id_has_all_singles
    ;       cons_id_has_one_multi
    ;       cons_id_has_conflict.

:- type cons_id_entry
    --->    cons_id_entry(
                cons_id_state       :: cons_id_state,
                cons_id_arms        :: cord(case_arm)
            ).

:- type cases_table
    --->    cases_table(
                cases_map           :: map(cons_id, cons_id_entry),
                conflict_cons_ids   :: set_tree234(cons_id)
            ).

:- func convert_cases_table(hlds_goal_info, cases_table) = list(case).

convert_cases_table(GoalInfo, CasesTable) = SortedCases :-
    CasesTable = cases_table(CasesMap, ConflictIds),
    map.to_assoc_list(CasesMap, CasesAssocList),
    list.foldl2(convert_case(GoalInfo, ConflictIds), CasesAssocList, [], Cases,
        set_tree234.init, _AlreadyHandledConsIds),
    list.sort(Cases, SortedCases).

:- pred convert_case(hlds_goal_info::in, set_tree234(cons_id)::in,
    pair(cons_id, cons_id_entry)::in, list(case)::in, list(case)::out,
    set_tree234(cons_id)::in, set_tree234(cons_id)::out) is det.

convert_case(GoalInfo0, ConflictConsIds, ConsId - Entry, !Cases,
        !AlreadyHandledConsIds) :-
    ( if set_tree234.contains(!.AlreadyHandledConsIds, ConsId) then
        Entry = cons_id_entry(State, _ArmCord),
        expect(unify(State, cons_id_has_one_multi), $pred,
            "already handled but not cons_id_has_one_multi")
    else
        Entry = cons_id_entry(State, ArmsCord),
        Arms = cord.list(ArmsCord),
        (
            State = cons_id_has_conflict,
            set_tree234.is_member(ConflictConsIds, ConsId, IsMember),
            expect(unify(IsMember, yes), $pred,
                "conflict status but not in ConflictConsIds"),
            Disjuncts = list.map(project_arm_goal, Arms),
            use_context_of_first_disjunct(Disjuncts, GoalInfo0, GoalInfo),
            disj_list_to_goal(Disjuncts, GoalInfo, Goal),
            Case = case(ConsId, [], Goal),
            !:Cases = [Case | !.Cases]
        ;
            State = cons_id_has_all_singles,
            set_tree234.is_member(ConflictConsIds, ConsId, IsMember),
            expect(unify(IsMember, no), $pred,
                "singles status but in ConflictConsIds"),
            Disjuncts = list.map(project_single_arm_goal, Arms),
            use_context_of_first_disjunct(Disjuncts, GoalInfo0, GoalInfo),
            disj_list_to_goal(Disjuncts, GoalInfo, Goal),
            Case = case(ConsId, [], Goal),
            !:Cases = [Case | !.Cases]
        ;
            State = cons_id_has_one_multi,
            ( if
                Arms = [Arm],
                Arm = multi_cons_id_arm(MainConsId0, OtherConsIds0, Goal)
            then
                % The code that creates multi_cons_id_arms should ensure
                % that [MainConsId | OtherConsIds0] is sorted, and
                % convert_cases_table should call convert_case for ConsIds
                % in the same sorted order. In the usual case, by the time
                % convert_case is called for any of the cons_ids in
                % OtherConsIds, the call to convert_case for MainConsId will
                % have put the cons_ids in OtherConsIds into
                % !.AlreadyHandledConsIds, so we won't get here. That is when
                % the entry for MainConsId has state cons_id_has_one_multi.
                % However, MainConsId0 may have an entry whose state is
                % cons_id_has_conflict. In that case ConsId will not equal
                % MainConsId0.
                AllConsIds0 = [MainConsId0 | OtherConsIds0],
                % This can filter out MainConsId0.
                list.filter(set_tree234.contains(ConflictConsIds),
                    AllConsIds0, _, AllConsIds),
                (
                    AllConsIds = [MainConsId | OtherConsIds],
                    Case = case(MainConsId, OtherConsIds, Goal),
                    set_tree234.insert_list(OtherConsIds,
                        !AlreadyHandledConsIds),
                    !:Cases = [Case | !.Cases]
                ;
                    AllConsIds = [],
                    % At least, AllConsIds should contain ConsId.
                    unexpected($pred, "cons_id_has_one_multi: AllConsIds = []")
                )
            else
                unexpected($pred, "misleading cons_id_has_one_multi")
            )
        )
    ).

:- func project_arm_goal(case_arm) = hlds_goal.

project_arm_goal(single_cons_id_arm(_, Goal)) = Goal.
project_arm_goal(multi_cons_id_arm(_, _, Goal)) = Goal.

:- func project_single_arm_goal(case_arm) = hlds_goal.

project_single_arm_goal(single_cons_id_arm(_, Goal)) = Goal.
project_single_arm_goal(multi_cons_id_arm(_, _, _)) = _ :-
    unexpected($pred, "multi arm").

:- pred use_context_of_first_disjunct(list(hlds_goal)::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

use_context_of_first_disjunct(Disjuncts, GoalInfo0, GoalInfo) :-
    (
        Disjuncts = [FirstDisjunct | LaterDisjuncts],
        FirstDisjunct = hlds_goal(_FirstGoalExpr, FirstGoalInfo),
        FirstContext = goal_info_get_context(FirstGoalInfo),
        gather_smallest_context(LaterDisjuncts, FirstContext, SmallestContext),
        goal_info_set_context(SmallestContext, GoalInfo0, GoalInfo)
    ;
        Disjuncts = [],
        GoalInfo = GoalInfo0
    ).

    % Compute the smallest context among the given goals and the initial
    % accumulator.
    %
    % In the common case that all the goals' contexts have the same filename,
    % this selects the smallest line number.
    %
    % If the goals' contexts refer to more than one filename, we have no way
    % to choose the least surprising context to use, so may as well use
    % the overall smallest one.
    %
:- pred gather_smallest_context(list(hlds_goal)::in,
    term.context::in, term.context::out) is det.

gather_smallest_context([], !SmallestContext).
gather_smallest_context([Goal | Goals], !SmallestContext) :-
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    ( if compare((<), Context, !.SmallestContext) then
        !:SmallestContext = Context
    else
        true
    ),
    gather_smallest_context(Goals, !SmallestContext).

:- func num_cases_in_table(cases_table) = int.

num_cases_in_table(cases_table(CasesMap, _)) = map.count(CasesMap).

:- pred add_single_entry(cons_id::in, hlds_goal::in,
    cases_table::in, cases_table::out) is det.

add_single_entry(ConsId, Goal, CasesTable0, CasesTable) :-
    CasesTable0 = cases_table(CasesMap0, ConflictConsIds0),
    Arm = single_cons_id_arm(ConsId, Goal),
    ( if map.search(CasesMap0, ConsId, Entry0) then
        Entry0 = cons_id_entry(State0, Arms0),
        (
            State0 = cons_id_has_all_singles,
            State = cons_id_has_all_singles,
            ConflictConsIds = ConflictConsIds0
        ;
            State0 = cons_id_has_one_multi,
            State = cons_id_has_conflict,
            set_tree234.insert(ConsId, ConflictConsIds0, ConflictConsIds)
        ;
            State0 = cons_id_has_conflict,
            State = cons_id_has_conflict,
            ConflictConsIds = ConflictConsIds0
        ),
        Arms = snoc(Arms0, Arm),
        Entry = cons_id_entry(State, Arms),
        map.det_update(ConsId, Entry, CasesMap0, CasesMap)
    else
        State = cons_id_has_all_singles,
        Arms = cord.singleton(Arm),
        Entry = cons_id_entry(State, Arms),
        map.det_insert(ConsId, Entry, CasesMap0, CasesMap),
        ConflictConsIds = ConflictConsIds0
    ),
    CasesTable = cases_table(CasesMap, ConflictConsIds).

:- pred add_multi_entry(cons_id::in, list(cons_id)::in, hlds_goal::in,
    cases_table::in, cases_table::out) is det.

add_multi_entry(MainConsId, OtherConsIds, Goal, CasesTable0, CasesTable) :-
    Arm = multi_cons_id_arm(MainConsId, OtherConsIds, Goal),
    list.foldl(add_multi_entry_for_cons_id(Arm), [MainConsId | OtherConsIds],
        CasesTable0, CasesTable).

:- pred add_multi_entry_for_cons_id(case_arm::in, cons_id::in,
    cases_table::in, cases_table::out) is det.

add_multi_entry_for_cons_id(Arm, ConsId, CasesTable0, CasesTable) :-
    CasesTable0 = cases_table(CasesMap0, ConflictConsIds0),
    ( if map.search(CasesMap0, ConsId, Entry0) then
        Entry0 = cons_id_entry(State0, Arms0),
        (
            ( State0 = cons_id_has_all_singles
            ; State0 = cons_id_has_one_multi
            ),
            set_tree234.insert(ConsId, ConflictConsIds0, ConflictConsIds)
        ;
            State0 = cons_id_has_conflict,
            ConflictConsIds = ConflictConsIds0
        ),
        State = cons_id_has_conflict,
        cord.snoc(Arm, Arms0, Arms),
        Entry = cons_id_entry(State, Arms),
        map.det_update(ConsId, Entry, CasesMap0, CasesMap)
    else
        State = cons_id_has_one_multi,
        Arms = cord.singleton(Arm),
        Entry = cons_id_entry(State, Arms),
        map.det_insert(ConsId, Entry, CasesMap0, CasesMap),
        ConflictConsIds = ConflictConsIds0
    ),
    CasesTable = cases_table(CasesMap, ConflictConsIds).

%---------------------------------------------------------------------------%

    % A disjunction is a candidate for conversion to a switch on cs_var
    % if the conditions that is_candidate_switch tests for are satisfied.
    %
    % The disjuncts of the original disjunction will each end up in one
    % the next three fields: cs_cases, cs_unreachable_case_goals, and
    % cs_left_over_disjuncts.
    %
    % The left over disjuncts are the disjuncts that do not unify cs_var
    % with any function symbol, at least in a way that is visible to
    % switch detection. The disjuncts that *do* unify cs_var with a function
    % symbol will be converted into cases. If two (or more) disjuncts
    % unify cs_var with the same function symbol, those disjuncts will be put
    % into the same case (with the case's goal being a disjunction containing
    % just these disjuncts). Some cases may turn out to be unselectable,
    % because cs_var's initial inst guarantees that it won't be unifiable
    % with the case's function symbol; cs_unreachable_case_goals contains
    % the bodies of such cases.
    %
    % Since we try to convert only non-empty disjunctions to switches,
    % the above guarantees that at least one of cs_cases,
    % cs_unreachable_case_goals and cs_left_over_disjuncts will be non-empty.
    % If both cs_cases and cs_unreachable_case_goals would be empty for a given
    % variable, then we won't consider converting the disjunction into
    % a switch on that variable, so for any candidate_switch we *do* construct,
    % at least one of cs_cases and cs_unreachable_case_goals will be nonempty.
    % However, it is possible for either one of those fields to be empty
    % if the other contains at least one entry.
    %
    % Some disjunctions can be converted into switches on more than one
    % variable. We prefer to pick the variable that will allow determinism
    % analysis to find the tightest possible bounds on the number of solutions.
    % As a heuristic to help us choose well, we associate a rank with each
    % candidate conversion scheme. If there is more than candidate switch
    % we can turn the disjunction into, we choose the candidate with
    % the highest rank; we break any ties by picking the candidate
    % with the smallest variable number.
    %
    % When we convert the disjunction into a switch based on the chosen
    % candidate, we need to fill in the can_fail field of the switch
    % we create. We get the value we need from the cs_can_fail field.
:- type candidate_switch
    --->    candidate_switch(
                cs_var                      :: prog_var,
                cs_cases                    :: list(case),
                cs_unreachable_case_goals   :: list(hlds_goal),
                cs_left_over_disjuncts      :: list(hlds_goal),
                cs_rank                     :: candidate_switch_rank,
                cs_can_fail                 :: can_fail
            ).

    % The initial version of switch detection, which we used for a *long* time,
    % looked at nonlocal variables in variable number order and stopped looking
    % when it found a viable candidate switch.
    %
    % This could lead to an suboptimal outcome for two separate reasons.
    %
    % - First, when the user specifies which variable the switch should be on
    %   (via a require_switch_* scope), the committed-to variable is
    %   not necessarily the specified variable.
    %
    % - Second, switching on a variable later in the order can lead to a
    %   tighter determinism (because it leads to a cannot_fail switch, when
    %   the switch on the earlier, committed-to variable is can_fail).
    %
    % We fixed the first problem by putting RequiredVar at the start of
    % VarsToTry in cases where MaybeRequiredVar is yes(RequiredVar), and
    % we fixed the second by evaluating all candidate switches, without
    % stopping when we found a viable one. The second fix obsoletes the first;
    % if we look at all candidates and select the one with the best rank,
    % then for correctness, it *doesn't matter* in what order we look at
    % the nonlocals.
    %
    % It might matter for performance. When MaybeRequiredVar is
    % yes(RequiredVar), we *could* arrange to look at RequiredVar first,
    % and if it does yield a candidate switch with the best possible rank,
    % stop looking at the other variables. However, this would require
    % detect_switch_candidates_in_disj testing that condition after finding
    % each candidate switch. Since require_switch_* scopes are relatively rare,
    % the cost of the test in the common case where MaybeRequiredVar is "no"
    % would probably cost us more overall than we could save in cases where
    % MaybeRequiredVar is "yes".

    % The order of preference that we use to decide which candidate switch
    % to turn a disjunction into, for disjunctions in which we actually
    % have a choice. The ranks are in order from the least attractive
    % to the most attractive choice.
    %
    % In general, we prefer to have no disjuncts "left over" after we convert
    % disjuncts to case arms. All else being equal, we also prefer switches
    % in which the resulting switch arms cover all the function symbols
    % in the type of the switched-on variable that are allowed by the instmap
    % at entry to the switch.
    %
:- type candidate_switch_rank
    --->    some_leftover_can_fail(
                % Some of the disjuncts will have to remain outside the switch,
                % and the switch will be can_fail. This is the least useful
                % kind of switch that switch detection can create.
                int     % number of case arms
            )

    ;       some_leftover_cannot_fail(
                % Some of the disjuncts will have to remain outside the switch,
                % but at least the switch will be cannot_fail (though the
                % code inside the switch arms may fail).
                int     % number of case arms
            )

    ;       no_leftover_twoplus_cases_finite_can_fail
            % All disjuncts unify the switch variable with a function symbol,
            % but there is at least one function symbol that the switch
            % variable can be bound to at the start of the disjunction
            % that is not covered by any of the original disjuncts.
            % There are at least two cases, and at least one is reachable
            % (the rest may be unreachable).

    ;       no_leftover_one_case
            % With no_leftover_twoplus_cases_finite_can_fail, we *know*
            % that the resulting switch will be can_fail, and therefore
            % it can't be det. However, if all the disjuncts unify this
            % candidate var with the *same* function symbol, which is the
            % situation that no_leftover_one_case describes, then we know that
            % cse_detection.m will pull this deconstruction unification
            % out of all the disjuncts. Then, when cse_detection.m repeats
            % switch detection, there is at least a chance that we will be
            % able to transform this disjunction to a det switch.

    ;       no_leftover_twoplus_cases_infinite_can_fail
            % All disjuncts unify the switch variable with a function symbol,
            % but the domain of the switch variable is infinite, so it is
            % not possible for all the function symbols in the domain
            % to be covered by a case.
            %
            % I (zs) don't know of any strong argument for deciding
            % the relative order of no_leftover_twoplus_cases_infinite_can_fail
            % and no_leftover_one_case either way. The current order replicates
            % the relative order between these two cases that was effectively
            % imposed by old code.

    ;       no_leftover_twoplus_cases_finite_cannot_fail
            % The best switch we can hope for in normal circumstances;
            % all disjuncts unify the switch variable with a function symbol,
            % and every function symbol that the switch variable can be
            % bound to at the start of the disjunction is covered by at least
            % one disjunct. There are at least two cases, and at least one
            % is reachable (the rest may be unreachable).

    ;       all_disjuncts_are_unreachable
            % We can convert the entire disjunction to just `fail'.
            % This is possible only in the very rare case when all disjuncts
            % unify the switch variable with a function symbol that the switch
            % variable's initial inst rules out, but when it *is* possible,
            % the resulting goal will have the tightest possible determinism
            % we can hope for, namely failure.

    ;       no_leftover_twoplus_cases_explicitly_selected.
            % If the disjunction is what the programmer would consider
            % to be a switch on the variable that they explicitly said
            % that they expect the disjunction to switch on, i.e. if
            % all disjunct unify the specified variable with a function
            % symbol and there are at least two cases, then follow the
            % programmer's lead. The programmer may prefer an incomplete
            % switch on the specified variable to a complete switch on
            % another variable. An example from the compiler: when handling
            % special options in options.m, we would prefer the option handler
            % to switch on the option, not on the kind of data (none, bool,
            % int, string, maybe_string) given to it.

:- pred detect_switches_in_disj(instmap::in, maybe_required_switch_var::in,
    list(hlds_goal)::in, hlds_goal_info::in, hlds_goal_expr::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_switches_in_disj(InstMap0, MaybeRequiredVar, Disjuncts0, GoalInfo,
        GoalExpr, !LocalInfo) :-

    NonLocals = goal_info_get_nonlocals(GoalInfo),
    set_of_var.to_sorted_list(NonLocals, VarsToTry),
    detect_switch_candidates_in_disj(GoalInfo, Disjuncts0, InstMap0,
        MaybeRequiredVar, VarsToTry, cord.init, CandidatesCord, !LocalInfo),
    Candidates = cord.to_list(CandidatesCord),
    (
        Candidates = [],
        detect_sub_switches_in_disj(InstMap0, Disjuncts0, Disjuncts,
            !LocalInfo),
        GoalExpr = disj(Disjuncts)
    ;
        Candidates = [FirstCandidate | LaterCandidates],
        (
            LaterCandidates = [],
            BestCandidate = FirstCandidate
        ;
            LaterCandidates = [_ | _],
            BestCandidate0 = FirstCandidate,
            select_best_candidate_switch(LaterCandidates,
                BestCandidate0, BestCandidate)
        ),
        BestRank = BestCandidate ^ cs_rank,
        (
            BestRank = no_leftover_one_case,
            % Leave the disjunction unchanged for now. Let cse_detection
            % lift the unification that occurs in all the disjunct out of
            % those disjuncts, and let the invocation of switch detection
            % *after* cse look for switches in the transformed disjunction.
            %
            % We don't even look for switches *inside* each disjunct,
            % since that could hide the common unifications inside them
            % from the code of cse_detection.m (by wrapping them up
            % inside switches). This is why we don't call
            % detect_sub_switches_in_disj here.
            GoalExpr = disj(Disjuncts0)
        ;
            ( BestRank = some_leftover_can_fail(_)
            ; BestRank = some_leftover_cannot_fail(_)
            ; BestRank = no_leftover_twoplus_cases_finite_can_fail
            ; BestRank = no_leftover_twoplus_cases_infinite_can_fail
            ; BestRank = no_leftover_twoplus_cases_finite_cannot_fail
            ; BestRank = all_disjuncts_are_unreachable
            ; BestRank = no_leftover_twoplus_cases_explicitly_selected
            ),
            cases_to_switch(BestCandidate, InstMap0, SwitchGoalExpr,
                !LocalInfo),
            LeftDisjuncts0 = BestCandidate ^ cs_left_over_disjuncts,
            (
                LeftDisjuncts0 = [],
                GoalExpr = SwitchGoalExpr
            ;
                LeftDisjuncts0 = [_ | _],
                detect_switches_in_disj(InstMap0, MaybeRequiredVar,
                    LeftDisjuncts0, GoalInfo, LeftGoal, !LocalInfo),
                goal_to_disj_list(hlds_goal(LeftGoal, GoalInfo),
                    LeftDisjuncts),
                SwitchGoal = hlds_goal(SwitchGoalExpr, GoalInfo),
                GoalExpr = disj([SwitchGoal | LeftDisjuncts])
            )
        )
    ).

    % This is the interesting bit - we have found a non-empty disjunction,
    % and we have got a list of the non-local variables of that disjunction.
    % Now for each non-local variable, we check whether there is a partition
    % of the disjuncts such that each group of disjunctions can only succeed
    % if the variable is bound to a different functor.
    %
:- pred detect_switch_candidates_in_disj(hlds_goal_info::in,
    list(hlds_goal)::in, instmap::in, maybe_required_switch_var::in,
    list(prog_var)::in,
    cord(candidate_switch)::in, cord(candidate_switch)::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_switch_candidates_in_disj(_GoalInfo, _Disjuncts0, _InstMap0,
        _MaybeRequiredVar, [], !Candidates, !LocalInfo).
detect_switch_candidates_in_disj(GoalInfo, Disjuncts0, InstMap0,
        MaybeRequiredVar, [Var | Vars], !Candidates, !LocalInfo) :-
    % Can we do at least a partial switch on this variable?
    ModuleInfo = !.LocalInfo ^ lsdi_module_info,
    instmap_lookup_var(InstMap0, Var, VarInst0),
    ( if
        inst_is_bound(ModuleInfo, VarInst0),
        partition_disj(Var, Disjuncts0, GoalInfo, Left, Cases, !LocalInfo),

        VarTable = !.LocalInfo ^ lsdi_var_table,
        lookup_var_type(VarTable, Var, VarType),
        is_candidate_switch(Cases, Left)
    then
        categorize_candidate_switch(ModuleInfo, MaybeRequiredVar,
            Var, VarType, VarInst0, Cases, Left, Candidate),
        !:Candidates = cord.snoc(!.Candidates, Candidate)
    else
        true
    ),
    detect_switch_candidates_in_disj(GoalInfo, Disjuncts0, InstMap0,
        MaybeRequiredVar, Vars, !Candidates, !LocalInfo).

%---------------------------------------------------------------------------%

    % partition_disj(Var, Disjuncts, GoalInfo, Left, Cases, !LocalInfo):
    %
    % Attempts to partition the disjunction Disjuncts into a switch on Var.
    % If at least partially successful, returns the resulting Cases, with
    % any disjunction goals not fitting into the switch in Left.
    %
    % Given the list of goals in a disjunction, and an input variable to switch
    % on, we attempt to partition the goals into a switch. For each constructor
    % id, we record the list of disjuncts which unify the variable with that
    % constructor. We partition the goals by abstractly interpreting the
    % unifications at the start of each disjunction, to build up a
    % substitution.
    %
:- pred partition_disj(prog_var::in, list(hlds_goal)::in,
    hlds_goal_info::in, list(hlds_goal)::out, list(case)::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is semidet.

partition_disj(Var, Disjuncts0, GoalInfo, Left, Cases, !LocalInfo) :-
    CasesTable0 = cases_table(map.init, set_tree234.init),
    partition_disj_trial(!.LocalInfo, Var, Disjuncts0,
        [], Left1, CasesTable0, CasesTable1),
    (
        Left1 = [],
        % There must be at least one case in CasesTable1.
        num_cases_in_table(CasesTable1) >= 1,
        Left = Left1,
        Cases = convert_cases_table(GoalInfo, CasesTable1)
    ;
        Left1 = [_ | _],
        % We do not insist on there being at least one case in CasesTable1,
        % to allow for switches in which *all* cases contain subsidiary
        % disjunctions.
        ( if
            expand_sub_disjs(!.LocalInfo, Var, Left1, CasesTable1, CasesTable)
        then
            Left = [],
            num_cases_in_table(CasesTable) >= 1,
            Cases = convert_cases_table(GoalInfo, CasesTable),
            !LocalInfo ^ lsdi_requant := need_to_requantify
        else
            trace [runtime(env("SCOUT_DISJUNCTIONS")), io(!IO)] (
                ModuleInfo = !.LocalInfo ^ lsdi_module_info,
                VarTable = !.LocalInfo ^ lsdi_var_table,
                varset.init(TVarSet),
                varset.init(InstVarSet),
                io.stderr_stream(StrErr, !IO),

                VarStr = mercury_var_to_string(VarTable,
                    print_name_and_num, Var),
                io.format(StrErr, "\nVar = %s\n", [s(VarStr)], !IO),

                io.write_string(StrErr, "\nLEFT GOALS\n", !IO),
                list.foldl(
                    dump_goal_nl(StrErr, ModuleInfo, vns_var_table(VarTable),
                        TVarSet, InstVarSet),
                    Left1, !IO),
                io.write_string(StrErr, "\nEND LEFT GOALS\n", !IO)
            ),

            Left = Left1,
            Cases = convert_cases_table(GoalInfo, CasesTable1)
        )
    ).

%---------------------------------------------------------------------------%

:- pred partition_disj_trial(local_switch_detect_info::in, prog_var::in,
    list(hlds_goal)::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    cases_table::in, cases_table::out) is det.

partition_disj_trial(_, _, [], !Left, !CasesTable).
partition_disj_trial(LocalInfo, Var, [Disjunct0 | Disjuncts0],
        !Left, !CasesTable) :-
    find_bind_var(Var, find_bind_var_for_switch_in_deconstruct,
        Disjunct0, Disjunct, no, MaybeConsId, unit, _, _),
    trace [compile_time(flag("partition_disj")), io(!IO)] (
        ModuleInfo = LocalInfo ^ lsdi_module_info,
        VarTable = LocalInfo ^ lsdi_var_table,
        io.output_stream(Stream, !IO),
        DisjunctDesc0 =
            describe_structured_goal(ModuleInfo, VarTable, 1u, Disjunct0),
        (
            MaybeConsId = no,
            ResultStr = "no"
        ;
            MaybeConsId = yes(ConsId0),
            ResultStr = "yes(" ++ cons_id_and_arity_to_string(ConsId0) ++ ")"
        ),
        io.format(Stream, "\nfind_bind_var for %s on\n",
            [s(describe_var(VarTable, Var))], !IO),
        io.write_string(Stream, DisjunctDesc0, !IO),
        io.format(Stream, "MaybeConsId = %s\n\n", [s(ResultStr)], !IO)
    ),
    (
        MaybeConsId = yes(ConsId),
        add_single_entry(ConsId, Disjunct, !CasesTable)
    ;
        MaybeConsId = no,
        !:Left = [Disjunct0 | !.Left]
    ),
    partition_disj_trial(LocalInfo, Var, Disjuncts0, !Left, !CasesTable).

:- pred find_bind_var_for_switch_in_deconstruct(prog_var::in,
    hlds_goal_expr::in(goal_expr_deconstruct), hlds_goal_info::in,
    list(hlds_goal)::out, maybe(cons_id)::in, maybe(cons_id)::out,
    unit::in, unit::out) is det.

find_bind_var_for_switch_in_deconstruct(SwitchVar, GoalExpr0, GoalInfo0, Goals,
        _Result0, Result, _, unit) :-
    GoalExpr0 = unify(_, _, _, Unification0, _),
    Unification0 = deconstruct(UnifyVar, Functor, ArgVars, _, _, _),
    Result = yes(Functor),
    ( if
        ArgVars = [],
        SwitchVar = UnifyVar
    then
        % The test will get carried out in the switch, there are no
        % argument values to pick up, and the test was on the switch
        % variable (not on one of its aliases), so the unification
        % serve no further purpose. We delete it here, so simplify
        % doesn't have to.
        Goals = []
    else
        % The deconstruction unification now becomes deterministic, since
        % the test will get carried out in the switch.
        Unification = Unification0 ^ deconstruct_can_fail := cannot_fail,
        GoalExpr = GoalExpr0 ^ unify_kind := Unification,
        Goal = hlds_goal(GoalExpr, GoalInfo0),
        Goals = [Goal]
    ).

%---------------------------------------------------------------------------%

:- pred expand_sub_disjs(local_switch_detect_info::in, prog_var::in,
    list(hlds_goal)::in, cases_table::in, cases_table::out) is semidet.

expand_sub_disjs(_, _, [], !CasesTable).
expand_sub_disjs(LocalInfo, Var, [LeftGoal | LeftGoals], !CasesTable) :-
    expand_sub_disj(LocalInfo, Var, LeftGoal, !CasesTable),
    expand_sub_disjs(LocalInfo, Var, LeftGoals, !CasesTable).

:- pred expand_sub_disj(local_switch_detect_info::in, prog_var::in,
    hlds_goal::in, cases_table::in, cases_table::out) is semidet.

expand_sub_disj(LocalInfo, Var, Goal, !CasesTable) :-
    Goal = hlds_goal(GoalExpr, GoalInfo0),
    goal_info_add_feature(feature_duplicated_for_switch, GoalInfo0, GoalInfo),
    ( if GoalExpr = conj(plain_conj, SubGoals) then
        expand_sub_disj_process_conj(LocalInfo, Var, SubGoals, GoalInfo,
            [], !CasesTable)
    else if GoalExpr = disj(_) then
        expand_sub_disj_process_conj(LocalInfo, Var, [Goal], GoalInfo,
            [], !CasesTable)
    else
        fail
    ).

:- pred expand_sub_disj_process_conj(local_switch_detect_info::in,
    prog_var::in, list(hlds_goal)::in, hlds_goal_info::in, list(hlds_goal)::in,
    cases_table::in, cases_table::out) is semidet.

expand_sub_disj_process_conj(LocalInfo, Var, ConjGoals, GoalInfo,
        !.RevUnifies, !CasesTable) :-
    (
        ConjGoals = [],
        fail
    ;
        ConjGoals = [FirstGoal | LaterGoals],
        FirstGoal = hlds_goal(FirstGoalExpr, FirstGoalInfo),
        (
            FirstGoalExpr = unify(_, _, _, _, _),
            !:RevUnifies = [FirstGoal | !.RevUnifies],
            expand_sub_disj_process_conj(LocalInfo, Var, LaterGoals,
                GoalInfo, !.RevUnifies, !CasesTable)
        ;
            FirstGoalExpr = disj(Disjuncts),
            Disjuncts = [_ | _],
            ( if
                LocalInfo ^ lsdi_allow_multi_arm = allow_multi_arm,
                !.RevUnifies = [],

                % If the unifications pick up the values of variables,
                % we would need to include in the switch arm of each cons_id
                % not just LaterGoals, but also the disjunct in FirstGoal
                % that does this picking up. This disjunct would have to be
                % specific to each cons_id, so it could not be shared with
                % other cons_ids.
                NonLocals = goal_info_get_nonlocals(FirstGoalInfo),
                set_of_var.delete(Var, NonLocals, OtherNonLocals),
                set_of_var.is_empty(OtherNonLocals),

                all_disjuncts_are_switch_var_unifies(Var, Disjuncts,
                    DisjConsIds),
                list.sort(DisjConsIds, SortedDisjConsIds),
                SortedDisjConsIds = [MainConsId | OtherConsIds]
            then
                SharedGoal = hlds_goal(conj(plain_conj, LaterGoals), GoalInfo),
                add_multi_entry(MainConsId, OtherConsIds, SharedGoal,
                    !CasesTable)
            else if
                LocalInfo ^ lsdi_allow_multi_arm = allow_multi_arm,
                !.RevUnifies = [],

                CasesTableMap = !.CasesTable ^ cases_map,
                map.keys_as_set(CasesTableMap, CasesConsIds),
                % Without this test, we can create a top-level switch
                % which has one arm that lists all the cons_ids of the
                % switched-on variable, with that arm containing the
                % same switch, and so on, in an infinitely large set
                % of russian nesting dolls. And of course, constructing
                % an infinite set of switches would take infinite time,
                % at least if we had infinite memory.
                set.is_non_empty(CasesConsIds),

                ScoutInfo = LocalInfo ^ lsdi_scout_disj_info,
                DisjunctionMap = ScoutInfo ^ sdi_disjunction_info_map,
                FirstGoalId = goal_info_get_goal_id(FirstGoalInfo),
                map.search(DisjunctionMap, disjunction_id(FirstGoalId),
                    FirstGoalDisjunctionInfo),
                FirstGoalSummary = FirstGoalDisjunctionInfo ^ dni_summary_map,
                map.search(FirstGoalSummary, Var, VarSummary),
                VarSummary = var_all_arms_summary(SummaryConsIds0, SubDisj),
                SummaryConsIds = set.map(switchable_cons_id_to_cons_id,
                    SummaryConsIds0),
                % If two or more disjuncts have deconstructions that unify
                % the variable being switched on with the *same* cons_id,
                % (which we call a "conflict"), then in the switch we create,
                % the case for that cons_id must be a new disjunction of just
                % those disjuncts from the original disjunction. However, this
                % is not easy to do when one or more of those disjuncts
                % are not direct arms of the original disjunction, but
                % are arms in a sub-disjunction, sub-sub-disjunction, and
                % so on. We do not have any code that pull arms out of such
                % sub^N-disjunctions, so we insist on there being no conflict
                % either among the sub^N-disjunctions of this top level
                % disjunct, ....
                SubDisj = sub_disj_is_not_needed,
                set.intersect(SummaryConsIds, CasesConsIds,
                    NewConflictConsIds),
                % ... or between one sub^N-disjunction of this top-level
                % disjunct, and its neighboring top-level disjuncts.
                set.is_empty(NewConflictConsIds),
                set.to_sorted_list(SummaryConsIds, SortedDisjConsIds),
                SortedDisjConsIds = [MainConsId | OtherConsIds]
            then
                SharedGoal = hlds_goal(conj(plain_conj, ConjGoals), GoalInfo),
                add_multi_entry(MainConsId, OtherConsIds, SharedGoal,
                    !CasesTable)
            else
                list.reverse(!.RevUnifies, Unifies),
                list.map(
                    create_expanded_conjunction(Unifies, LaterGoals, GoalInfo),
                    Disjuncts, ExpandedConjunctions),
                partition_disj_trial(LocalInfo, Var, ExpandedConjunctions,
                    [], Left, !CasesTable),
                Left = []
            )
        )
    ).

:- pred all_disjuncts_are_switch_var_unifies(prog_var::in,
    list(hlds_goal)::in, list(cons_id)::out) is semidet.

all_disjuncts_are_switch_var_unifies(_Var, [], []).
all_disjuncts_are_switch_var_unifies(Var, [Goal | Goals],
        [ConsId | ConsIds]) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    GoalExpr = unify(_LHS, _RHS, _, UnifyInfo0, _),
    UnifyInfo0 = deconstruct(Var, ConsId, _, _, _, _),
    all_disjuncts_are_switch_var_unifies(Var, Goals, ConsIds).

:- pred create_expanded_conjunction(list(hlds_goal)::in, list(hlds_goal)::in,
    hlds_goal_info::in, hlds_goal::in, hlds_goal::out) is det.

create_expanded_conjunction(Unifies, LaterGoals, GoalInfo, Disjunct, Goal) :-
    ( if Disjunct = hlds_goal(conj(plain_conj, DisjunctGoals), _) then
        Conjuncts = Unifies ++ DisjunctGoals ++ LaterGoals
    else
        Conjuncts = Unifies ++ [Disjunct] ++ LaterGoals
    ),
    Goal = hlds_goal(conj(plain_conj, Conjuncts), GoalInfo).

%---------------------------------------------------------------------------%

:- pred is_candidate_switch(list(case)::in, list(hlds_goal)::in) is semidet.

is_candidate_switch(Cases0, LeftOver) :-
    (
        % If every disjunct unifies Var with a function symbol, then
        % it is candidate switch on Var, *even if* all disjuncts unify Var
        % with the *same* function symbol. This is because the resulting
        % single-arm switch may turn out to contain sub-switches on the
        % *arguments* of that function symbol.
        LeftOver = []
    ;
        % If some disjunct does not unify Var with any function symbol,
        % then we insist on at least two cases (though one may unreachable,
        % see below). We do this because the presence of the LeftOver
        % disjunct(s) requires us to have an outer disjunction anyway;
        % having one of its arms be a single-arm switch would be
        % indistinguishable from the original disjunction in almost all cases.
        % The only exception I (zs) can think of would happen if the same
        % X = f(...) goal occurred inside all the disjuncts that would end up
        % in an inner disjunction inside the single-arm switch's single arm,
        % but not in the other disjuncts. In that case, acting on the
        % candidate we would create here may allow cse_detection.m to make
        % a change could enable later follow-on changes by switch detection
        % itself. However, I have never seen any real-life code that could
        % benefit from this theoretical possibility, and until we do see
        % such code, so the gain from deleting this test would be minimal
        % at best, while the cost of deleting it would be to greatly increase
        % the number of candidates and thus the time taken by switch detection.
        Cases0 = [_, _ | _]
    ).

:- pred categorize_candidate_switch(module_info::in,
    maybe_required_switch_var::in, prog_var::in, mer_type::in, mer_inst::in,
    list(case)::in, list(hlds_goal)::in, candidate_switch::out) is det.

categorize_candidate_switch(ModuleInfo, MaybeRequiredVar, Var, VarType,
        VarInst0, Cases0, LeftOver, Candidate) :-
    can_candidate_switch_fail(ModuleInfo, VarType, VarInst0, Cases0,
        CanFail, CasesMissing, Cases, UnreachableCaseGoals),
    (
        LeftOver = [],
        (
            Cases = [],
            Rank = all_disjuncts_are_unreachable
        ;
            Cases = [_FirstCase | LaterCases],
            ( if
                LaterCases = [],
                UnreachableCaseGoals = []
            then
                Rank = no_leftover_one_case
            else
                % FirstCase is one case, and whichever of LaterCases and
                % UnreachableCaseGoals is nonempty is the second case.
                ( if
                    MaybeRequiredVar = rsv(RequiredVar),
                    RequiredVar = Var
                then
                    Rank = no_leftover_twoplus_cases_explicitly_selected
                else
                    (
                        CasesMissing = some_cases_missing,
                        Rank = no_leftover_twoplus_cases_finite_can_fail
                    ;
                        CasesMissing = no_cases_missing,
                        Rank = no_leftover_twoplus_cases_finite_cannot_fail
                    ;
                        CasesMissing = unbounded_cases,
                        Rank = no_leftover_twoplus_cases_infinite_can_fail
                    )
                )
            )
        )
    ;
        LeftOver = [_ | _],
        list.length(Cases, NumCases),
        (
            CanFail = cannot_fail,
            Rank = some_leftover_cannot_fail(NumCases)
        ;
            CanFail = can_fail,
            Rank = some_leftover_can_fail(NumCases)
        )
    ),
    Candidate = candidate_switch(Var, Cases, UnreachableCaseGoals,
        LeftOver, Rank, CanFail).

:- pred select_best_candidate_switch(list(candidate_switch)::in,
    candidate_switch::in, candidate_switch::out) is det.

select_best_candidate_switch([], !BestCandidate).
select_best_candidate_switch([Candidate | Candidates], !BestCandidate) :-
    compare(Result, Candidate ^ cs_rank, !.BestCandidate ^ cs_rank),
    (
        ( Result = (<)
        ; Result = (=)
        )
    ;
        Result = (>),
        !:BestCandidate = Candidate
    ),
    select_best_candidate_switch(Candidates, !BestCandidate).

%---------------------------------------------------------------------------%

find_bind_var(Var, ProcessUnify, !Goal, !Result, !Info, FoundDeconstruct) :-
    map.init(Subst),
    find_bind_var_2(Var, ProcessUnify, !Goal, Subst, _,
        !Result, !Info, DeconstructSearch),
    (
        DeconstructSearch = before_deconstruct,
        FoundDeconstruct = did_not_find_deconstruct
    ;
        DeconstructSearch = found_deconstruct,
        FoundDeconstruct = did_find_deconstruct
    ;
        DeconstructSearch = given_up_search,
        FoundDeconstruct = did_not_find_deconstruct
    ).

:- type deconstruct_search
    --->    before_deconstruct
    ;       found_deconstruct
    ;       given_up_search.

:- pred find_bind_var_2(prog_var::in,
    process_unify(Result, Info)::in(process_unify),
    hlds_goal::in, hlds_goal::out,
    prog_substitution::in, prog_substitution::out, Result::in, Result::out,
    Info::in, Info::out, deconstruct_search::out) is det.

find_bind_var_2(Var, ProcessUnify, Goal0, Goal, !Subst,
        !Result, !Info, FoundDeconstruct) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = scope(Reason0, SubGoal0),
        ( if Reason0 = from_ground_term(_, from_ground_term_construct) then
            % There are no deconstruction unifications inside these scopes.
            Goal = Goal0,
            % Whether we want to keep looking at the code that follows them
            % is a more interesting question. Since we keep going after
            % construction unifications (whose behavior this scope resembles),
            % we keep going.
            FoundDeconstruct = before_deconstruct
        else
            find_bind_var_2(Var, ProcessUnify, SubGoal0, SubGoal,
                !Subst, !Result, !Info, FoundDeconstruct),
            ( if
                FoundDeconstruct = found_deconstruct,
                Reason0 = from_ground_term(_, from_ground_term_deconstruct)
            then
                % If we remove a goal from such a scope, what is left
                % may no longer satisfy the invariants we expect it to satisfy.
                Goal = SubGoal
            else
                Goal = hlds_goal(scope(Reason0, SubGoal), GoalInfo)
            )
        )
    ;
        GoalExpr0 = conj(ConjType, SubGoals0),
        (
            ConjType = plain_conj,
            (
                SubGoals0 = [],
                Goal = Goal0,
                FoundDeconstruct = before_deconstruct
            ;
                SubGoals0 = [_ | _],
                conj_find_bind_var(Var, ProcessUnify, SubGoals0, SubGoals,
                    !Subst, !Result, !Info, FoundDeconstruct),
                Goal = hlds_goal(conj(ConjType, SubGoals), GoalInfo)
            )
        ;
            ConjType = parallel_conj,
            Goal = Goal0,
            FoundDeconstruct = given_up_search
        )
    ;
        GoalExpr0 = unify(LHS, RHS, UnifyMode, UnifyInfo0, UnifyContext),
        ( if
            % Check whether the unification is a deconstruction unification
            % on either Var or on a variable aliased to Var.
            UnifyInfo0 = deconstruct(UnifyVar, _ConsId, _, _, _, _),
            term_subst.apply_rec_substitution_in_term(!.Subst,
                term.variable(Var, dummy_context),
                term.variable(SubstVar, dummy_context)),
            term_subst.apply_rec_substitution_in_term(!.Subst,
                term.variable(UnifyVar, dummy_context),
                term.variable(SubstUnifyVar, dummy_context)),
            SubstVar = SubstUnifyVar
        then
            % Rebuild GoalExpr0, but this time with the compiler knowing
            % that UnifyInfo0 is bound to deconstruct. Common structure
            % optimization should prevent any runtime overhead.
            GoalExpr1 = unify(LHS, RHS, UnifyMode, UnifyInfo0, UnifyContext),
            ProcessUnify(Var, GoalExpr1, GoalInfo, Goals, !Result, !Info),
            conj_list_to_goal(Goals, GoalInfo, Goal),
            FoundDeconstruct = found_deconstruct
        else
            Goal = Goal0,
            FoundDeconstruct = before_deconstruct,
            % Otherwise abstractly interpret the unification.
            % XXX interpret_unify is defined in det_util.m,
            % but it is used only here.
            % XXX The code that sets things up for interpretation
            % would be simpler if we had a version of library/term.m
            % that did NOT include a context in each term.
            ( if interpret_unify(LHS, RHS, !.Subst, NewSubst) then
                !:Subst = NewSubst
            else
                % The unification must fail - just ignore it.
                true
            )
        )
    ;
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = disj(_)
        ; GoalExpr0 = switch(_, _, _)
        ; GoalExpr0 = negation(_)
        ; GoalExpr0 = if_then_else(_, _, _, _)
        ),
        Goal = Goal0,
        ( if goal_info_has_feature(GoalInfo, feature_from_head) then
            FoundDeconstruct = before_deconstruct
        else
            FoundDeconstruct = given_up_search
        )
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(_, _, _, _, _, _, _),
            Goal = Goal0,
            FoundDeconstruct = given_up_search
        ;
            ShortHand0 = try_goal(_, _, _),
            Goal = Goal0,
            FoundDeconstruct = given_up_search
        ;
            ShortHand0 = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        )
    ).

:- pred conj_find_bind_var(prog_var::in,
    process_unify(Result, Info)::in(process_unify),
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_substitution::in, prog_substitution::out, Result::in, Result::out,
    Info::in, Info::out, deconstruct_search::out) is det.

conj_find_bind_var(_Var, _, [], [],
        !Subst, !Result, !Info, before_deconstruct).
conj_find_bind_var(Var, ProcessUnify, [HeadGoal0 | TailGoals0], Goals,
        !Subst, !Result, !Info, FoundDeconstruct) :-
    find_bind_var_2(Var, ProcessUnify, HeadGoal0, HeadGoal, !Subst,
        !Result, !Info, FoundDeconstruct1),
    (
        FoundDeconstruct1 = before_deconstruct,
        conj_find_bind_var(Var, ProcessUnify, TailGoals0, TailGoals,
            !Subst, !Result, !Info, FoundDeconstruct),
        Goals = [HeadGoal | TailGoals]
    ;
        FoundDeconstruct1 = found_deconstruct,
        FoundDeconstruct = FoundDeconstruct1,
        HeadGoal = hlds_goal(HeadGoalExpr, _),
        ( if HeadGoalExpr = conj(_, []) then
            % HeadGoal is "true". Delete it now, so simplify doesn't have to.
            Goals = TailGoals0
        else
            Goals = [HeadGoal | TailGoals0]
        )
    ;
        FoundDeconstruct1 = given_up_search,
        FoundDeconstruct = FoundDeconstruct1,
        Goals = [HeadGoal | TailGoals0]
    ).

%---------------------------------------------------------------------------%

:- pred cases_to_switch(candidate_switch::in, instmap::in,
    hlds_goal_expr::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

cases_to_switch(Candidate, InstMap0, GoalExpr, !LocalInfo) :-
    Candidate = candidate_switch(Var, Cases0, UnreachableCaseGoals, _Left,
        _Rank, CanFail),
    (
        UnreachableCaseGoals = []
    ;
        UnreachableCaseGoals = [_ | _],
        UnreachableCalledProcs = goals_proc_refs(UnreachableCaseGoals),

        DeletedCallCallees0 = !.LocalInfo ^ lsdi_deleted_callees,
        set.union(UnreachableCalledProcs,
            DeletedCallCallees0, DeletedCallCallees),
        !LocalInfo ^ lsdi_deleted_callees := DeletedCallCallees
    ),

    detect_switches_in_cases(Var, InstMap0, Cases0, Cases, !LocalInfo),
    % We turn switches with no arms into fail, since this avoids having
    % the code generator flush the control variable of the switch.
    % We can't easily eliminate switches with one arm, since the
    % code of the arm will have the unification between the variable
    % and the function symbol as det. The gain would be minimal to
    % nonexistent anyway.
    (
        Cases = [],
        GoalExpr = disj([])
    ;
        Cases = [_ | _],
        GoalExpr = switch(Var, CanFail, Cases)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Compute whether a switch on a given variable with a given set
% of cases can fail.
%

:- type cases_missing
    --->    no_cases_missing
    ;       some_cases_missing
    ;       unbounded_cases.

:- pred can_candidate_switch_fail(module_info::in, mer_type::in, mer_inst::in,
    list(case)::in, can_fail::out, cases_missing::out, list(case)::out,
    list(hlds_goal)::out) is det.

can_candidate_switch_fail(ModuleInfo, VarType, VarInst0, Cases0,
        CanFail, CasesMissing, Cases, UnreachableCaseGoals) :-
    ( if inst_is_bound_to_functors(ModuleInfo, VarInst0, Functors) then
        type_to_ctor_det(VarType, TypeCtor),
        bound_functors_to_cons_ids(TypeCtor, Functors, ConsIds),
        set_tree234.list_to_set(ConsIds, ConsIdSet),
        delete_unreachable_cases(Cases0, ConsIdSet,
            Cases, UnreachableCaseGoals),
        compute_can_fail(ConsIdSet, Cases, CanFail, CasesMissing)
    else
        % We do not have any inst information that would allow us to decide
        % that any case is unreachable.
        Cases = Cases0,
        UnreachableCaseGoals = [],
        ( if switch_type_num_functors(ModuleInfo, VarType, NumFunctors) then
            % We could check for each cons_id of the type whether a case covers
            % it, but given that type checking ensures that the set of covered
            % cons_ids is a subset of the set of cons_ids of the type, checking
            % whether the cardinalities of the two sets match is *equivalent*
            % to checking whether they are the same set.
            does_switch_cover_n_cases(NumFunctors, Cases,
                CanFail, CasesMissing)
        else
            % switch_type_num_functors fails only for types on which
            % you cannot have a complete switch, e.g. integers and strings.
            CanFail = can_fail,
            CasesMissing = unbounded_cases
        )
    ).

:- pred compute_can_fail(set_tree234(cons_id)::in, list(case)::in,
    can_fail::out, cases_missing::out) is det.

compute_can_fail(Functors, Cases, SwitchCanFail, CasesMissing) :-
    UncoveredFunctors0 = Functors,
    delete_covered_functors(Cases, UncoveredFunctors0, UncoveredFunctors),
    ( if set_tree234.is_empty(UncoveredFunctors) then
        SwitchCanFail = cannot_fail,
        CasesMissing = no_cases_missing
    else
        SwitchCanFail = can_fail,
        CasesMissing = some_cases_missing
    ).

    % Delete from !UncoveredConsIds all cons_ids mentioned in any of the cases.
    %
:- pred delete_covered_functors(list(case)::in,
    set_tree234(cons_id)::in, set_tree234(cons_id)::out) is det.

delete_covered_functors([], !UncoveredConsIds).
delete_covered_functors([Case | Cases], !UncoveredConsIds) :-
    Case = case(MainConsId, OtherConsIds, _Goal),
    set_tree234.delete(MainConsId, !UncoveredConsIds),
    set_tree234.delete_list(OtherConsIds, !UncoveredConsIds),
    delete_covered_functors(Cases, !UncoveredConsIds).

    % Check whether a switch handles the given number of cons_ids.
    %
:- pred does_switch_cover_n_cases(int::in, list(case)::in,
    can_fail::out, cases_missing::out) is det.

does_switch_cover_n_cases(NumFunctors, Cases, SwitchCanFail, CasesMissing) :-
    NumCoveredConsIds = count_covered_cons_ids(Cases),
    ( if NumCoveredConsIds = NumFunctors then
        SwitchCanFail = cannot_fail,
        CasesMissing = no_cases_missing
    else
        SwitchCanFail = can_fail,
        CasesMissing = some_cases_missing
    ).

:- func count_covered_cons_ids(list(case)) = int.

count_covered_cons_ids([]) = 0.
count_covered_cons_ids([Case | Cases]) = CaseCount + CasesCount :-
    Case = case(_MainConsId, OtherConsIds, _Goal),
    CaseCount = 1 + list.length(OtherConsIds),
    CasesCount = count_covered_cons_ids(Cases).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% What follows are the data structures we use in scout_disjunctions_in_goal.
% This is a pre-pass before detect_switches_in_goal, the traversal that
% actually decides which disjunctions can be turned into switches,
% and if they can possibly be turned into more than one kind of switch,
% which one we should choose.
%
% The result of scouting is information about the terrain ahead, which
% in this case means information about deconstruction unifications
% and disjunctions that the main traversal has not yet seen.
%
% At the moment, we use scouting results at only point in the main traversal.
% However, this may change in the future.
%

:- type scout_disj_info
    --->    scout_disj_info(
                % Conceptually, both of these are read-only, though in
                % actuality, we update module_info when we handle cases
                % inside switches.
                sdi_module_info             :: module_info,
                sdi_var_table               :: var_table,

                % These are the data structures we are constructing.
                % The one we really want is the disjunction_info_map;
                % we build the disjunct_info_map as a stepping stone to it.
                sdi_disjunction_info_map    :: disjunction_info_map,
                sdi_disjunct_info_map       :: disjunct_info_map
            ).

%---------------------%

    % Maps the id of a disjunction to information about that disjunction.
:- type disjunction_info_map == map(disjunction_id, disjunction_info).

    % Map the id of a disjunct to information about that disjunct.
:- type disjunct_info_map == map(disjunct_id, disjunct_info).

%---------------------%

    % The type whose values we use to identify a disjunction.
    % The goal specified by the given goal_id will be a disj(...) goal.
:- type disjunction_id
    --->    disjunction_id(goal_id).

    % The type whose values we use to identify a disjunct in a disjunction.
    % The goal specified by the given goal_id will have a disj(...) goal
    % as its immediate parent.
:- type disjunct_id
    --->    disjunct_id(goal_id).

%---------------------%

    % The information that scouting finds about a disjunction.
    % There should be one of these in the disjunction_info_map
    % for every disjunction in the procedure body.
:- type disjunction_info
    --->    disjunction_info(
                % The list of disjuncts in the disjunction.
                % This field is not yet used.
                dni_arms                    :: one_or_more(disjunct_id_info),

                % This field is the main product of the scouting pass.
                % The map will contain an entry for every variable
                % that is deconstructed in the zone of every disjunct.
                % Such a deconstruction can occur directly in the disjunct,
                % or it can occur in smaller disjunctions inside it, nested
                % at an any depth.
                dni_summary_map             :: all_arms_summary_map
            ).

    % This type is used only for the dni_summary_map field.
    % Please see its documentation.
:- type all_arms_summary_map == map(prog_var, var_all_arms_summary).

:- type var_all_arms_summary
    --->    var_all_arms_summary(
                % The set of cons_ids that disjuncts in this disjunction
                % unify the associated variable with in the zone, either
                % in the disjunct directly, or in a subdisjunction
                % (which may be arbitrarily deeply nested).
                %
                % (The associated variable is the key in the
                % all_arms_summary_map for this value.)
                set(switchable_cons_id),

                % If the associated variable is deconstructed to one of
                % the above cons_ids in *more than one* disjunct, then
                % turning the overall disjunction into a switch would
                % require making the switch arm for that cons_id into
                % a subdisjunction. Is there such a cons_id?
                is_sub_disj_needed
            ).

:- type is_sub_disj_needed
    --->    sub_disj_is_not_needed
    ;       sub_disj_is_needed.

    % Values of this type contain summary information about one disjunct
    % of a disjunction. Their sole intended use is as an input for the
    % construction of var_all_arms_summary structures.
:- type one_arm_summary_map == map(prog_var, var_one_arm_summary).

:- type var_one_arm_summary
    --->    voas_deconstruct(deconstruct_info)
            % The disjunct deconstructs the associated variable directly
            % in its zone. The argument gives the specifics of the
            % deconstruction.
    ;       voas_sub_disjunction(var_all_arms_summary).
            % The disjunct does not deconstruct the associated variable
            % directly in its zone, but it does contain a subdisjunction
            % in the zone which does so, either directly or indirectly.

    % The set of cons_id kinds that we consider creating switch arms for.
:- type switchable_cons_id =< cons_id
    --->    du_data_ctor(du_ctor)
    ;       some_int_const(some_int_const)
    ;       float_const(float)
    ;       char_const(char)
    ;       string_const(string).

%---------------------%

:- type maybe_in_zone
    --->    in_zone(disjunct_id)
            % We are in one of the disjuncts of a disjunction; the argument
            % specifies the disjunct. And we are within the initial sequence
            % of unifications within that disjunct. (We treat calls from the
            % clause head as unifications for this purpose.)
            %
            % As soon as we leave this initial part of a disjunct,
            % we switch to not_in_zone. The only deconstruction unifications
            % we consider for switch detection are the ones that occur
            % "in the zone".
            %
            % We originally adopted this rule to reduce the cost (in compile
            % time) of searching for deconstruction unifications that denote
            % switch arms. However, converting such a deconstruction
            % unification into the test for a switch arm can also change
            % the order execution of the disjunct's conjuncts, and restricting
            % the reordering to happen only among unifications eliminates
            % any concerns about changing the operational semantics of the
            % procedure in terms of exceptions being raised or nontermination
            % being introduced. (Function calls from clause heads do not have
            % a clearly specified order with respect to goals in the clause
            % body, which is why we allow reordering with respect to them.)
            %
            % The effect on compile times is no longer meaningful, but the
            % effect on operational semantics is still relevant.
            %
            % There is also an ergonomic argument here: requiring unifications
            % that effectively serve as case constants in C switch statements
            % to be near the start of their switch arms keeps code readable,
            % compared to a hypothetical alternative arrangement in which
            % we allow unifications from the ends of possibly-long disjuncts
            % to provide the cons_id that identifies a switch arm.
    ;       not_in_zone.

:- type disjunct_id_info
    --->    disjunct_id_info(disjunct_id, disjunct_info).

    % The information that scouting finds about a disjunct.
    % There should be one of these in the disjunct_info_map
    % for every disjunct in the procedure body.
:- type disjunct_info
    --->    disjunct_info(
                di_iz_deconstruct_map       :: in_zone_deconstruct_map,
                % We record info about at most one disjunction, since
                % a disjunction ends the zone.
                di_iz_sub_disjunctions      :: maybe(disjunction_id)
            ).

    % Note that we map not just the deconstructed variable
    % to a deconstruct_info, but also all variables equivalent to it.
:- type in_zone_deconstruct_map == map(prog_var, deconstruct_info).

:- type deconstruct_info
    --->    deconstruct_info(
                % This goal ...
                goal_id,

                % ... deconstructs this variable ...
                prog_var,

                % ... which is part of this equivalence class ...
                set(prog_var),

                % ... with this cons_id.
                switchable_cons_id
            ).

%---------------------------------------------------------------------------%

:- pred scout_disjunctions_in_goal(hlds_goal::in, instmap::in, instmap::out,
    maybe_in_zone::in, maybe_in_zone::out, subst_db::in, subst_db::out,
    scout_disj_info::in, scout_disj_info::out) is det.

scout_disjunctions_in_goal(Goal, InstMap0, InstMap,
        !InZone, !SubstDb, !ScoutInfo) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = unify(_, _, _, _, _),
        scout_disjunctions_in_unify_expr(GoalExpr, GoalInfo, InstMap0,
            !.InZone, !SubstDb, !ScoutInfo)
    ;
        ( GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        ( if goal_info_has_feature(GoalInfo, feature_from_head) then
            true
        else
            !:InZone = not_in_zone
        )
    ;
        GoalExpr = conj(ConjType, Conjuncts),
        (
            ConjType = plain_conj,
            scout_disjunctions_in_conjuncts(Conjuncts, InstMap0,
                !InZone, !SubstDb, !ScoutInfo)
        ;
            ConjType = parallel_conj,
            (
                Conjuncts = []
            ;
                Conjuncts = [HeadConjunct | TailConjuncts],
                % The first parallel conjunct can be in the zone;
                % any later conjuncts cannot be in the zone.
                scout_disjunctions_in_goal(HeadConjunct, InstMap0, InstMap1,
                    !.InZone, _, !SubstDb, !ScoutInfo),
                scout_disjunctions_in_conjuncts(TailConjuncts, InstMap1,
                    not_in_zone, _, !SubstDb, !ScoutInfo),
                !:InZone = not_in_zone
            )
        )
    ;
        GoalExpr = disj(Disjuncts),
        (
            Disjuncts = []
        ;
            Disjuncts = [HeadDisjunct | TailDisjuncts],
            scout_disjunctions_in_disjuncts(HeadDisjunct, TailDisjuncts,
                InstMap0, !.SubstDb,
                HeadDisjunctIdInfo, TailDisjunctIdInfos, !ScoutInfo),

            OoMDisjunctIdsInfos =
                one_or_more(HeadDisjunctIdInfo, TailDisjunctIdInfos),
            construct_disjunction_info(!.ScoutInfo, OoMDisjunctIdsInfos,
                DisjunctionInfo),

            DisjunctionGoalId = goal_info_get_goal_id(GoalInfo),
            DisjunctionId = disjunction_id(DisjunctionGoalId),
            DisjunctionInfoMap0 = !.ScoutInfo ^ sdi_disjunction_info_map,
            map.det_insert(DisjunctionId, DisjunctionInfo,
                DisjunctionInfoMap0, DisjunctionInfoMap),
            !ScoutInfo ^ sdi_disjunction_info_map := DisjunctionInfoMap,

            (
                !.InZone = in_zone(DisjunctId),
                DisjunctInfoMap0 = !.ScoutInfo ^ sdi_disjunct_info_map,
                map.lookup(DisjunctInfoMap0, DisjunctId, DisjunctInfo0),
                DisjunctInfo0 =
                    disjunct_info(DeconstructMap0, SubDisjunctions0),
                expect(unify(SubDisjunctions0, no), $pred,
                    "SubDisjunctions0 != no"),
                SubDisjunctions = yes(DisjunctionId),
                DisjunctInfo = disjunct_info(DeconstructMap0, SubDisjunctions),
                map.det_update(DisjunctId, DisjunctInfo,
                    DisjunctInfoMap0, DisjunctInfoMap),
                !ScoutInfo ^ sdi_disjunct_info_map := DisjunctInfoMap
            ;
                !.InZone = not_in_zone
            ),
            !:InZone = not_in_zone
        )
    ;
        GoalExpr = switch(Var, _CanFail, Cases),
        scout_disjunctions_in_cases(Var, Cases, InstMap0,
            !.SubstDb, !ScoutInfo),
        !:InZone = not_in_zone
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        scout_disjunctions_in_goal(Cond, InstMap0, InstMap1,
            not_in_zone, _, !.SubstDb, SubstDbCond, !ScoutInfo),
        scout_disjunctions_in_goal(Then, InstMap1, _,
            not_in_zone, _, SubstDbCond, _, !ScoutInfo),
        scout_disjunctions_in_goal(Else, InstMap0, _,
            not_in_zone, _, !.SubstDb, _, !ScoutInfo),
        !:InZone = not_in_zone
    ;
        GoalExpr = negation(SubGoal),
        scout_disjunctions_in_goal(SubGoal, InstMap0, _,
            not_in_zone, _, !.SubstDb, _, !ScoutInfo),
        !:InZone = not_in_zone
    ;
        GoalExpr = scope(Reason, SubGoal),
        (
            Reason = from_ground_term(_, FgtKind),
            (
                FgtKind = from_ground_term_deconstruct,
                SubGoal = hlds_goal(SubGoalExpr, _),
                ( if
                    SubGoalExpr = conj(plain_conj, [HeadSubGoal | _]),
                    HeadSubGoal = hlds_goal(HeadSubGoalExpr, HeadSubGoalInfo),
                    HeadSubGoalExpr = unify(_, _, _, _, _)
                then
                    scout_disjunctions_in_unify_expr(HeadSubGoalExpr,
                        HeadSubGoalInfo, InstMap0, !.InZone,
                        !SubstDb, !ScoutInfo)
                    % Ignore the goals after HeadSubGoal; nothing in them
                    % could interest us, since none of the variables
                    % they deconstruct are visible from outside this scope.
                else
                    unexpected($pred, "unexpected goal in fgt scope")
                )
            ;
                ( FgtKind = from_ground_term_initial
                ; FgtKind = from_ground_term_construct
                ; FgtKind = from_ground_term_other
                )
                % Ignore the scope; nothing in it could interest us.
            )
        ;
            ( Reason = exist_quant(_, _)
            ; Reason = disable_warnings(_, _)
            ; Reason = promise_solutions(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            ; Reason = loop_control(_, _, _)
            ),
            scout_disjunctions_in_goal(SubGoal, InstMap0, _,
                !.InZone, _, !.SubstDb, _, !ScoutInfo)
        ;
            ( Reason = require_complete_switch(_RequiredVar)
            ; Reason = require_switch_arms_detism(_RequiredVar, _)
            ),
            scout_disjunctions_in_goal(SubGoal, InstMap0, _,
                not_in_zone, !:InZone, !.SubstDb, _, !ScoutInfo),
            expect(unify(!.InZone, not_in_zone), $pred,
                "in_zone after switch-related reason")
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, _Outer, _Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            scout_disjunctions_in_goal(MainGoal, InstMap0, _,
                not_in_zone, _, !.SubstDb, _, !ScoutInfo),
            scout_disjunctions_in_orelse_goals(OrElseGoals, InstMap0,
                !.SubstDb, !ScoutInfo),
            !:InZone = not_in_zone
        ;
            ShortHand = try_goal(_MaybeIO, _ResultVar, SubGoal),
            scout_disjunctions_in_goal(SubGoal, InstMap0, _,
                not_in_zone, _, !.SubstDb, _, !ScoutInfo),
            !:InZone = not_in_zone
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ),
    apply_goal_instmap_delta(Goal, InstMap0, InstMap).

:- pred scout_disjunctions_in_unify_expr(hlds_goal_expr::in(goal_expr_unify),
    hlds_goal_info::in, instmap::in, maybe_in_zone::in,
    subst_db::in, subst_db::out,
    scout_disj_info::in, scout_disj_info::out) is det.

scout_disjunctions_in_unify_expr(GoalExpr, GoalInfo, InstMap0,
        InZone0, !SubstDb, !ScoutInfo) :-
    GoalExpr = unify(XVar, RHS, _, Unification, _),
    % For both rhs_var and rhs_functor, we record the effect of the
    % unification on the substitution database even when we are
    % outside the zone. This extra info won't help us find more
    % aliases for in-zone deconstructions in this disjunct (since there
    % aren't any more past the end of the zone), but the extra information
    % in the substitution database may help us find more aliases inside
    % nested disjunctions.
    (
        RHS = rhs_lambda_goal(_, _, _, _, VarsModes, _, LambdaGoal),
        % We need to insert the initial insts for the lambda variables
        % into the instmap before processing the lambda goal.
        ModuleInfo = !.ScoutInfo ^ sdi_module_info,
        instmap.pre_lambda_update(ModuleInfo, VarsModes, InstMap0, InstMap1),
        % LambdaGoal may be in_zone from the point of view of the code
        % outside this unification, but the proper perspective for
        % this call is the code *inside* the lambda goal. And from that
        % point of view, LambdaGoal is not inside *any* disjunction.
        scout_disjunctions_in_goal(LambdaGoal, InstMap1, _,
            not_in_zone, _, !.SubstDb, _, !ScoutInfo)
    ;
        RHS = rhs_var(YVar),
        record_var_var_unify(XVar, YVar, !SubstDb)
    ;
        RHS = rhs_functor(ConsId, _IsExistConstr, YVars),
        (
            ( ConsId = du_data_ctor(_)
            ; ConsId = some_int_const(_)
            ; ConsId = float_const(_)
            ; ConsId = char_const(_)
            ; ConsId = string_const(_)
            ),
            (
                Unification = assign(_, _),
                unexpected($pred, "assign")
            ;
                Unification = simple_test(_, _),
                unexpected($pred, "simple_test")
            ;
                Unification = construct(_, _, _, _, _, _, _)
            ;
                Unification = deconstruct(_, _, _, _, _, _),
                (
                    InZone0 = in_zone(DisjunctId),
                    GoalId = goal_info_get_goal_id(GoalInfo),
                    record_deconstruct(GoalId, XVar, coerce(ConsId),
                        !.SubstDb, DisjunctId, !ScoutInfo)
                ;
                    InZone0 = not_in_zone
                )
            ;
                Unification = complicated_unify(_, _, _),
                unexpected($pred, "complicated_unify")
            ),
            record_var_functor_unify(XVar, ConsId, YVars, !SubstDb)
        ;
            ( ConsId = tuple_cons(_)
            ; ConsId = closure_cons(_)
            ; ConsId = impl_defined_const(_)
            ; ConsId = type_ctor_info_const(_, _, _)
            ; ConsId = base_typeclass_info_const(_, _, _, _)
            ; ConsId = type_info_cell_constructor(_)
            ; ConsId = typeclass_info_cell_constructor
            ; ConsId = type_info_const(_)
            ; ConsId = typeclass_info_const(_)
            ; ConsId = ground_term_const(_, _)
            ; ConsId = tabling_info_const(_)
            ; ConsId = table_io_entry_desc(_)
            ; ConsId = deep_profiling_proc_layout(_)
            )
        )
    ).

:- pred record_deconstruct(goal_id::in, prog_var::in, switchable_cons_id::in,
    subst_db::in, disjunct_id::in,
    scout_disj_info::in, scout_disj_info::out) is det.

record_deconstruct(GoalId, XVar, ConsId, SubstDb, DisjunctId, !ScoutInfo) :-
    get_equivalent_vars(SubstDb, XVar, XEqvVars),
    DeconstructInfo = deconstruct_info(GoalId, XVar, XEqvVars, ConsId),

    DisjunctInfoMap0 = !.ScoutInfo ^ sdi_disjunct_info_map,
    map.lookup(DisjunctInfoMap0, DisjunctId, DisjunctInfo0),
    DisjunctInfo0 = disjunct_info(DeconstructMap0, SubDisjunctions0),
    set.foldl(maybe_add_deconstruct(DeconstructInfo), XEqvVars,
        DeconstructMap0, DeconstructMap),
    DisjunctInfo = disjunct_info(DeconstructMap, SubDisjunctions0),
    map.det_update(DisjunctId, DisjunctInfo,
        DisjunctInfoMap0, DisjunctInfoMap),
    !ScoutInfo ^ sdi_disjunct_info_map := DisjunctInfoMap.

:- pred maybe_add_deconstruct(deconstruct_info::in, prog_var::in,
    in_zone_deconstruct_map::in, in_zone_deconstruct_map::out) is det.

maybe_add_deconstruct(DeconstructInfo, XEqvVar, !DeconstructMap) :-
    map.search_insert(XEqvVar, DeconstructInfo, _, !DeconstructMap).

%---------------------------------------------------------------------------%

:- pred scout_disjunctions_in_conjuncts(list(hlds_goal)::in, instmap::in,
    maybe_in_zone::in, maybe_in_zone::out, subst_db::in, subst_db::out,
    scout_disj_info::in, scout_disj_info::out) is det.

scout_disjunctions_in_conjuncts([], _InstMap0,
        !InZone, !SubstDb, !ScoutInfo).
scout_disjunctions_in_conjuncts([Conjunct | Conjuncts], InstMap0,
        !InZone, !SubstDb, !ScoutInfo) :-
    scout_disjunctions_in_goal(Conjunct, InstMap0, InstMap1,
        !InZone, !SubstDb, !ScoutInfo),
    scout_disjunctions_in_conjuncts(Conjuncts, InstMap1,
        !InZone, !SubstDb, !ScoutInfo).

%---------------------------------------------------------------------------%

:- pred scout_disjunctions_in_disjuncts(hlds_goal::in, list(hlds_goal)::in,
    instmap::in, subst_db::in,
    disjunct_id_info::out, list(disjunct_id_info)::out,
    scout_disj_info::in, scout_disj_info::out) is det.

scout_disjunctions_in_disjuncts(HeadDisjunct, TailDisjuncts, InstMap0,
        SubstDb0, HeadDisjunctIdInfo, TailDisjunctIdInfos, !ScoutInfo) :-
    % We pass in_zone here because a deconstruction unification
    % near the start of Disjunct could make Disjunct one arm of a switch.
    HeadDisjunct = hlds_goal(_, HeadDisjunctGoalInfo),
    HeadDisjunctGoalId = goal_info_get_goal_id(HeadDisjunctGoalInfo),
    HeadDisjunctId = disjunct_id(HeadDisjunctGoalId),
    DisjunctInfoMap0 = !.ScoutInfo ^ sdi_disjunct_info_map,
    HeadDisjunctInfo0 = disjunct_info(map.init, no),
    map.det_insert(HeadDisjunctId, HeadDisjunctInfo0,
        DisjunctInfoMap0, DisjunctInfoMap1),
    !ScoutInfo ^ sdi_disjunct_info_map := DisjunctInfoMap1,

    scout_disjunctions_in_goal(HeadDisjunct, InstMap0, _,
        in_zone(HeadDisjunctId), _, SubstDb0, _, !ScoutInfo),
    DisjunctInfoMap = !.ScoutInfo ^ sdi_disjunct_info_map,
    map.lookup(DisjunctInfoMap, HeadDisjunctId, HeadDisjunctInfo),
    HeadDisjunctIdInfo = disjunct_id_info(HeadDisjunctId, HeadDisjunctInfo),
    (
        TailDisjuncts = [],
        TailDisjunctIdInfos = []
    ;
        TailDisjuncts = [HeadTailDisjunct | TailTailDisjuncts],
        scout_disjunctions_in_disjuncts(HeadTailDisjunct, TailTailDisjuncts,
            InstMap0, SubstDb0,
            HeadTailDisjunctIdInfo, TailTailDisjunctIdInfos, !ScoutInfo),
        TailDisjunctIdInfos =
            [HeadTailDisjunctIdInfo | TailTailDisjunctIdInfos]
    ).

%---------------------------------------------------------------------------%

:- pred scout_disjunctions_in_orelse_goals(list(hlds_goal)::in,
    instmap::in, subst_db::in,
    scout_disj_info::in, scout_disj_info::out) is det.

scout_disjunctions_in_orelse_goals([], _InstMap0, _Subst0, !ScoutInfo).
scout_disjunctions_in_orelse_goals([OrElseGoal | OrElseGoals], InstMap0,
        Subst0, !ScoutInfo) :-
    % We pass not_in_zone here because a deconstruction unification
    % near the start of OrElseGoal *cannot* OrElseGoal an arm of a switch.
    scout_disjunctions_in_goal(OrElseGoal, InstMap0, _,
        not_in_zone, _, Subst0, _, !ScoutInfo),
    scout_disjunctions_in_orelse_goals(OrElseGoals, InstMap0,
        Subst0, !ScoutInfo).

%---------------------------------------------------------------------------%

:- pred scout_disjunctions_in_cases(prog_var::in, list(case)::in,
    instmap::in, subst_db::in,
    scout_disj_info::in, scout_disj_info::out) is det.

scout_disjunctions_in_cases(_Var, [], _InstMap0, _SubstDb0, !ScoutInfo).
scout_disjunctions_in_cases(Var, [Case | Cases], InstMap0,
        SubstDb0, !ScoutInfo) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    VarTable = !.ScoutInfo ^ sdi_var_table,
    lookup_var_type(VarTable, Var, VarType),
    ModuleInfo0 = !.ScoutInfo ^ sdi_module_info,
    bind_var_to_functors(Var, VarType, MainConsId, OtherConsIds,
        InstMap0, InstMap1, ModuleInfo0, ModuleInfo),
    !ScoutInfo ^ sdi_module_info := ModuleInfo,

    scout_disjunctions_in_goal(Goal, InstMap1, _,
        not_in_zone, _, SubstDb0, _, !ScoutInfo),
    scout_disjunctions_in_cases(Var, Cases, InstMap0, SubstDb0, !ScoutInfo).

%---------------------------------------------------------------------------%

:- pred construct_disjunction_info(scout_disj_info::in,
    one_or_more(disjunct_id_info)::in, disjunction_info::out) is det.

construct_disjunction_info(ScoutInfo, OoMDisjunctIdsInfos, DisjunctionInfo) :-
    OoMDisjunctIdsInfos =
        one_or_more(HeadDisjunctIdInfo, TailDisjunctIdInfos),

    disjunct_id_info_to_one_arm_summary(ScoutInfo,
        HeadDisjunctIdInfo, HeadOneArmMap),
    list.map(disjunct_id_info_to_one_arm_summary(ScoutInfo),
        TailDisjunctIdInfos, TailOneArmMaps),
    summarize_all_one_arms(HeadOneArmMap, TailOneArmMaps, AllArmsMap),

    DisjunctionInfo = disjunction_info(OoMDisjunctIdsInfos, AllArmsMap).

%---------------------%

:- pred disjunct_id_info_to_one_arm_summary(scout_disj_info::in,
    disjunct_id_info::in, map(prog_var, var_one_arm_summary)::out) is det.

disjunct_id_info_to_one_arm_summary(ScoutInfo, DisjunctIdInfo, OneArmMap) :-
    DisjunctIdInfo = disjunct_id_info(_DisjunctId, DisjunctInfo),
    DisjunctInfo = disjunct_info(DeconstructMap, MaybeSubDisjunction),
    map.map_values_only(in_zone_deconstruct_to_one_arm_summary,
        DeconstructMap, OneArmMap0),
    (
        MaybeSubDisjunction = no,
        OneArmMap = OneArmMap0
    ;
        MaybeSubDisjunction = yes(SubDisjunctionId),
        DisjunctionInfoMap = ScoutInfo ^ sdi_disjunction_info_map,
        map.lookup(DisjunctionInfoMap, SubDisjunctionId, SubDisjunctionInfo),
        SubDisjunctionInfo = disjunction_info(_, SubAllArmsMap),
        % If a variable already occurs in !.OneArmMap, then it must have been
        % added from DeconstructMap, meaning it must have been deconstructed
        % in the zone. Since the disjunction identified by SubDisjunctionId
        % would end the zone, we can ignore any reference to deconstructions
        % of such variables in SubAllArmsMap, because
        %
        % - if the referenced deconstructions's cons_id is the same cons_id
        %   assigned to the variable in OneArmMap0, then that reference
        %   is redundant, while
        %
        % - if the referenced deconstructions's cons_id is NOT the same
        %   cons_id as assigned to the variable in OneArmMap0, then that
        %   unification cannot possibly succeed, making the arm in which
        %   it occurs a dead arm. (One reason why we process deconstructions
        %   only in the zone of initial goals in each disjunct is to allow us
        %   to delete such dead arms without changing the operational
        %   semantics of the predicate body.)
        map.foldl(acc_sub_disjunction_summary, SubAllArmsMap,
            OneArmMap0, OneArmMap)
    ).

:- pred in_zone_deconstruct_to_one_arm_summary(deconstruct_info::in,
    var_one_arm_summary::out) is det.

in_zone_deconstruct_to_one_arm_summary(DeconstructInfo, OneArm) :-
    OneArm = voas_deconstruct(DeconstructInfo).

:- pred acc_sub_disjunction_summary(prog_var::in, var_all_arms_summary::in,
    one_arm_summary_map::in, one_arm_summary_map::out) is det.

acc_sub_disjunction_summary(Var, SubDisjAllArms, !OneArmMap) :-
    map.search_insert(Var, voas_sub_disjunction(SubDisjAllArms), _OldOneArm,
        !OneArmMap).

%---------------------%

:- pred summarize_all_one_arms(
    one_arm_summary_map::in, list(one_arm_summary_map)::in,
    all_arms_summary_map::out) is det.

summarize_all_one_arms(HeadOneArmMap, TailOneArmMaps, !:AllArmsMap) :-
    map.init(!:AllArmsMap),
    map.foldl(maybe_acc_all_arm_for_var(TailOneArmMaps),
        HeadOneArmMap, !AllArmsMap).

:- pred maybe_acc_all_arm_for_var(list(one_arm_summary_map)::in,
    prog_var::in, var_one_arm_summary::in,
    all_arms_summary_map::in, all_arms_summary_map::out) is det.

maybe_acc_all_arm_for_var(TailOneArmMaps, Var, HeadArmSummary, !AllArmsMap) :-
    ( if
        find_var_one_arm_summaries(Var, TailOneArmMaps,
            [], RevTailArmSummaries)
    then
        (
            HeadArmSummary = voas_deconstruct(DeconstructInfo),
            DeconstructInfo = deconstruct_info(_, _, _, ConsId),
            ConsIdSet = set.make_singleton_set(ConsId),
            AllArmsSummary0 =
                var_all_arms_summary(ConsIdSet, sub_disj_is_not_needed)
        ;
            HeadArmSummary = voas_sub_disjunction(AllArmsSummary0)
        ),
        % The order in which we add the tail arms summaries does not matter.
        list.foldl(add_arm_to_all_arms_summary, RevTailArmSummaries,
            AllArmsSummary0, AllArmsSummary),
        map.det_insert(Var, AllArmsSummary, !AllArmsMap)
    else
        true
    ).

:- pred find_var_one_arm_summaries(prog_var::in,
    list(one_arm_summary_map)::in,
    list(var_one_arm_summary)::in, list(var_one_arm_summary)::out) is semidet.

find_var_one_arm_summaries(_Var, [], !ArmSummaries).
find_var_one_arm_summaries(Var, [ArmSummaryMap | ArmSummaryMaps],
        !ArmSummaries) :-
    map.search(ArmSummaryMap, Var, ArmSummary),
    !:ArmSummaries = [ArmSummary | !.ArmSummaries],
    find_var_one_arm_summaries(Var, ArmSummaryMaps, !ArmSummaries).

:- pred add_arm_to_all_arms_summary(var_one_arm_summary::in,
    var_all_arms_summary::in, var_all_arms_summary::out) is det.

add_arm_to_all_arms_summary(OneArmSummary, !AllArmsSummary) :-
    !.AllArmsSummary = var_all_arms_summary(ConsIdSet0, SubDisjNeeded0),
    (
        OneArmSummary = voas_deconstruct(DeconstructInfo),
        DeconstructInfo = deconstruct_info(_, _, _, ConsId),
        % Was ConsId already in ConsIdSet0?
        ( if set.insert_new(ConsId, ConsIdSet0, ConsIdSetPrime) then
            % No, it was not.
            ConsIdSet = ConsIdSetPrime,
            SubDisjNeeded = SubDisjNeeded0
        else
            % Yes, it was. Adding it to ConsIdSet0 would leave it unchanged.
            ConsIdSet = ConsIdSet0,
            % If the variable whose summaries we are now processing
            % is selected as the switched-on variable, then its case for
            % ConsId would need to include a disjunction containing at least
            % the arm that first added ConsId to ConsIdSet0, and this arm.
            SubDisjNeeded = sub_disj_is_needed
        )
    ;
        OneArmSummary = voas_sub_disjunction(SubAllArmsSummary),
        SubAllArmsSummary =
            var_all_arms_summary(SubConsIdSet, SubSubDisjNeeded),
        set.union(SubConsIdSet, ConsIdSet0, ConsIdSet),
        ( if
            SubDisjNeeded0 = sub_disj_is_not_needed,
            SubSubDisjNeeded = sub_disj_is_not_needed
        then
            set.intersect(SubConsIdSet, ConsIdSet0, IntersectSet),
            ( if set.is_empty(IntersectSet) then
                SubDisjNeeded = sub_disj_is_not_needed
            else
                SubDisjNeeded = sub_disj_is_needed
            )
        else
            SubDisjNeeded = sub_disj_is_needed
        )
    ),
    !:AllArmsSummary = var_all_arms_summary(ConsIdSet, SubDisjNeeded).

%---------------------%

:- func disjunction_info_map_to_string(var_table, disjunction_info_map)
    = string.

disjunction_info_map_to_string(VarTable, DisjunctionInfoMap) = Str :-
    HeaderStr = "\nDISJUNCTION INFO MAP\n",
    EndHeaderStr = "END DISJUNCTION INFO MAP\n",
    map.to_sorted_assoc_list(DisjunctionInfoMap, DisjunctionIdsInfos),
    DisjunctionIdInfoStrs =
        list.map(disjunction_id_info_to_string(VarTable), DisjunctionIdsInfos),
    string.append_list(
        [HeaderStr | DisjunctionIdInfoStrs] ++ [EndHeaderStr], Str).

:- func disjunction_id_info_to_string(var_table,
    pair(disjunction_id, disjunction_info)) = string.

disjunction_id_info_to_string(VarTable, DisjunctionId - DisjunctionInfo)
        = Str :-
    DisjunctionIdStr = string.string(DisjunctionId),
    string.format("\n%s\n", [s(DisjunctionIdStr)], HeaderStr),
    DisjunctionInfo = disjunction_info(_, AllArmsMap),
    map.to_sorted_assoc_list(AllArmsMap, AllArmsEntries),
    AllArmsEntryStrs =
        list.map(var_all_arms_summary_to_string(VarTable), AllArmsEntries),
    string.append_list([HeaderStr | AllArmsEntryStrs], Str).

:- func var_all_arms_summary_to_string(var_table,
    pair(prog_var, var_all_arms_summary)) = string.

var_all_arms_summary_to_string(VarTable, Var - AllArmsSummary) = Str :-
    VarStr = mercury_var_to_string(VarTable, print_name_and_num, Var),
    AllArmsSummary = var_all_arms_summary(ConsIdSet0, SubDisj),
    ConsIdSet = set.map(switchable_cons_id_to_cons_id, ConsIdSet0),
    ConsIdStrSet = set.map(cons_id_and_arity_to_string, ConsIdSet),
    set.to_sorted_list(ConsIdStrSet, ConsIdStrs),
    ConsIdsStr = string.string(ConsIdStrs),
    SubDisjStr = string.string(SubDisj),
    string.format("%s -> var_all_arms_summary(%s, %s)\n",
        [s(VarStr), s(ConsIdsStr), s(SubDisjStr)], Str).

    % We should be able to coerce sets of switchable_cons_ids to cons_ids,
    % but we cannot. We use this until we can do so.
    %
:- func switchable_cons_id_to_cons_id(switchable_cons_id) = cons_id.

switchable_cons_id_to_cons_id(ConsId) = coerce(ConsId).

%---------------------------------------------------------------------------%
%
% We use the "substitution database" to figure out the set of variables
% that a given variable is an alias for.
%
% The occurrence of the deconstruction unification X = f(...) in an arm
% of a disjunction can be used to support turning that disjunct into
% an arm of a switch on X, but also into an arm of a switch on Y,
% if at the program point of that deconstruction, X and Y are known
% to be aliases. This can happen not just if we saw a unification X = Y,
% but also if we saw e.g. X = Z and Z = Y.
%

:- type subst_db
    --->    subst_db(
                % The set of variables we have seen in unifications
                % at the current point of the traversal. Only variables
                % in this set can possibly be such aliases at the current
                % program point.
                set(prog_var),

                % The substitution representing the relationships (if any)
                % between those variables.
                prog_substitution
            ).

:- func init_subst_db = subst_db.

init_subst_db = subst_db(set.init, map.init).

:- pred record_var_var_unify(prog_var::in, prog_var::in,
    subst_db::in, subst_db::out) is det.

record_var_var_unify(XVar, YVar, !SubstDb) :-
    !.SubstDb = subst_db(SeenVars0, Subst0),
    set.insert(XVar, SeenVars0, SeenVars1),
    set.insert(YVar, SeenVars1, SeenVars),
    XTerm = term.variable(XVar, dummy_context),
    YTerm = term.variable(YVar, dummy_context),
    ( if unify_terms(XTerm, YTerm, Subst0, Subst1) then
        Subst = Subst1
    else
        % The unification must fail - just ignore it.
        Subst = Subst0
    ),
    !:SubstDb = subst_db(SeenVars, Subst).

:- pred record_var_functor_unify(prog_var::in,
    cons_id::in(switchable_cons_id), list(prog_var)::in,
    subst_db::in, subst_db::out) is det.

record_var_functor_unify(XVar, ConsId, YVars, !SubstDb) :-
    !.SubstDb = subst_db(SeenVars0, Subst0),
    set.insert(XVar, SeenVars0, SeenVars1),
    set.insert_list(YVars, SeenVars1, SeenVars),
    XTerm = term.variable(XVar, dummy_context),
    term_subst.var_list_to_term_list(YVars, YVarTerms),
    cons_id_and_args_to_term(ConsId, YVarTerms, YTerm),
    ( if unify_terms(XTerm, YTerm, Subst0, Subst1) then
        Subst = Subst1
    else
        % The unification must fail - just ignore it.
        Subst = Subst0
    ),
    !:SubstDb = subst_db(SeenVars, Subst).

:- pred get_equivalent_vars(subst_db::in, prog_var::in,
    set(prog_var)::out) is det.

get_equivalent_vars(SubstDb, Var, EqvVarsSet) :-
    SubstDb = subst_db(SeenVars, Subst),
    term_subst.apply_rec_substitution_in_term(Subst,
        term.variable(Var, dummy_context), VarSubstTerm),
    list.foldl(acc_var_if_equivalent(Subst, VarSubstTerm),
        [Var | set.to_sorted_list(SeenVars)], [], EqvVars),
    set.list_to_set(EqvVars, EqvVarsSet).

:- pred acc_var_if_equivalent(prog_substitution::in, prog_term::in,
    prog_var::in, list(prog_var)::in, list(prog_var)::out) is det.

acc_var_if_equivalent(Subst, VarSubstTerm, SeenVar, !EqvVars) :-
    term_subst.apply_rec_substitution_in_term(Subst,
        term.variable(SeenVar, dummy_context), SeenVarSubstTerm),
    % Are Var in our caller (the variable being deconstructed) and SeenVar
    % - mapped to the same term by Subst, and
    % - is this term a variable?
    ( if
        VarSubstTerm = term.variable(X, _),
        SeenVarSubstTerm = term.variable(X, _)
    then
        !:EqvVars = [SeenVar | !.EqvVars]
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.switch_detection.
%---------------------------------------------------------------------------%
