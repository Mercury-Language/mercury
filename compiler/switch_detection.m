%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015-2026 The Mercury team.
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
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.

%---------------------------------------------------------------------------%

:- type switch_detect_info.

:- func init_switch_detect_info(module_info) = switch_detect_info.

:- pred detect_switches_in_module(io.text_output_stream::in,
    module_info::in, module_info::out) is det.

:- pred detect_switches_in_proc(switch_detect_info::in,
    proc_info::in, proc_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.find_bind_var.
:- import_module check_hlds.scout_disjunctions.
:- import_module check_hlds.switch_candidates.
:- import_module hlds.goal_refs.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_desc.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.inst_test.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
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

    scout_disjunctions_in_proc(ModuleInfo, !ProcInfo, DisjunctionInfoMap),
    proc_info_get_initial_instmap(ModuleInfo, !.ProcInfo, InstMap0),
    proc_info_get_var_table(!.ProcInfo, VarTable),
    proc_info_get_goal(!.ProcInfo, Goal0),
    Requant0 = do_not_need_to_requantify,
    BodyDeletedCallCallees0 = set.init,
    LocalInfo0 = local_switch_detect_info(VarTable, AllowMulti,
         DisjunctionInfoMap, ModuleInfo, Requant0, BodyDeletedCallCallees0),

    detect_switches_in_goal(InstMap0, nrsv, Goal0, Goal,
        LocalInfo0, LocalInfo),
    proc_info_set_goal(Goal, !ProcInfo),
    LocalInfo = local_switch_detect_info(_VarTable, _AllowMulti,
        _DisjunctionInfoMap, _ModuleInfo, Requant, BodyDeletedCallCallees),
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
                lsdi_var_table              :: var_table,
                lsdi_allow_multi_arm        :: allow_multi_arm,
                lsdi_disjunction_info_map   :: disjunction_info_map,

                % These fields are read-write.
                %
                % We update module_info when we enter a switch arm, and update
                % the inst of the switched-on variable to record it being bound
                % to one of the cons_ids that the switch arm is for.
                % XXX However, detect_switches_in_proc then throws away
                % the updated module_info.
                lsdi_module_info            :: module_info,
                lsdi_requant                :: need_to_requantify,
                lsdi_deleted_callees        :: set(pred_proc_id)
            ).

:- pred detect_switches_in_goal(instmap::in, maybe_required_switch_var::in,
    hlds_goal::in, hlds_goal::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_switches_in_goal(InstMap0, MaybeRequiredVar, Goal0, Goal, !LocalInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
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
        GoalExpr0 = conj(ConjType, Conjuncts0),
        detect_switches_in_conj(InstMap0, Conjuncts0, Conjuncts, !LocalInfo),
        GoalExpr = conj(ConjType, Conjuncts)
    ;
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
        GoalExpr0 = switch(Var, CanFail, Cases0),
        detect_switches_in_cases(Var, InstMap0, Cases0, Cases, !LocalInfo),
        GoalExpr = switch(Var, CanFail, Cases)
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

:- func convert_cases_table(local_switch_detect_info, prog_var,
    hlds_goal_info, cases_table) = list(case).

convert_cases_table(LocalInfo, Var, GoalInfo, CasesTable) = SortedCases :-
    CasesTable = cases_table(CasesMap, ConflictIds),
    map.to_assoc_list(CasesMap, CasesAssocList),
    list.foldl2(convert_case(LocalInfo, Var, GoalInfo, ConflictIds),
        CasesAssocList, [], Cases, set_tree234.init, _AlreadyHandledConsIds),
    list.sort(Cases, SortedCases).

:- pred convert_case(local_switch_detect_info::in, prog_var::in,
    hlds_goal_info::in, set_tree234(cons_id)::in,
    pair(cons_id, cons_id_entry)::in, list(case)::in, list(case)::out,
    set_tree234(cons_id)::in, set_tree234(cons_id)::out) is det.

convert_case(LocalInfo, Var, GoalInfo0, ConflictConsIds, ConsId - Entry,
        !Cases, !AlreadyHandledConsIds) :-
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
            Case = arm_disjuncts_to_case(ConsId, Disjuncts, GoalInfo0),
            !:Cases = [Case | !.Cases]
        ;
            State = cons_id_has_all_singles,
            set_tree234.is_member(ConflictConsIds, ConsId, IsMember),
            expect(unify(IsMember, no), $pred,
                "singles status but in ConflictConsIds"),
            Disjuncts = list.map(project_single_arm_goal, Arms),
            Case = arm_disjuncts_to_case(ConsId, Disjuncts, GoalInfo0),
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
        ),

        trace [
            compile_time(flag("scout-disjunctions")),
            runtime(env("SCOUT_DISJUNCTIONS")),
            io(!IO)
        ] (
            ModuleInfo = LocalInfo ^ lsdi_module_info,
            module_info_get_globals(ModuleInfo, Globals),
            Info = init_hlds_out_info(Globals, output_debug),
            VarTable = LocalInfo ^ lsdi_var_table,
            VarNameSrc = vns_var_table(VarTable),
            VarNamePrint = print_name_and_num,
            varset.init(TVarSet),
            varset.init(InstVarSet),
            TypeQual = no_tvarset_var_table,
            InfoGoal = hlds_out_info_goal(Info, ModuleInfo, VarNameSrc,
                VarNamePrint, TVarSet, InstVarSet, TypeQual),
            io.stderr_stream(StrErr, !IO),

            CaseStr = case_to_string(InfoGoal, 1u, Var, Case),
            io.write_string(StrErr, "\nCONVERTED_CASE\n", !IO),
            io.write_string(StrErr, CaseStr, !IO),
            io.write_string(StrErr, "END CONVERTED_CASE\n", !IO)
        )
    ).

:- func arm_disjuncts_to_case(cons_id, list(hlds_goal), hlds_goal_info) = case.

arm_disjuncts_to_case(ConsId, Disjuncts, GoalInfo0) = Case :-
    use_context_of_first_disjunct(Disjuncts, GoalInfo0, GoalInfo),
    disj_list_to_goal(Disjuncts, GoalInfo, Goal),
    Case = case(ConsId, [], Goal).

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
        select_best_candidate_switch(FirstCandidate, LaterCandidates,
            BestCandidate),
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
        Cases = convert_cases_table(!.LocalInfo, Var, GoalInfo, CasesTable1)
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
            Cases =
                convert_cases_table(!.LocalInfo, Var, GoalInfo, CasesTable),
            !LocalInfo ^ lsdi_requant := need_to_requantify
        else
            trace [
                compile_time(flag("scout-disjunctions")),
                runtime(env("SCOUT_DISJUNCTIONS")),
                io(!IO)
            ] (
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
                io.write_string(StrErr, "END LEFT GOALS\n", !IO)
            ),

            Left = Left1,
            Cases =
                convert_cases_table(!.LocalInfo, Var, GoalInfo, CasesTable1)
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

                DisjunctionMap = LocalInfo ^ lsdi_disjunction_info_map,
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
:- end_module check_hlds.switch_detection.
%---------------------------------------------------------------------------%
