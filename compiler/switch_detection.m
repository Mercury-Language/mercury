%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: switch_detection.m.
% Main authors: fjh, zs.
%
% Switch detection - when a disjunction contains disjuncts that unify the
% same input variable with different function symbols, replace (part of)
% the disjunction with a switch.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.switch_detection.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred detect_switches_in_module(module_info::in, module_info::out) is det.

:- type switch_detect_info.

:- func init_switch_detect_info(module_info) = switch_detect_info.

:- pred detect_switches_in_proc(switch_detect_info::in,
    proc_info::in, proc_info::out) is det.

:- type found_deconstruct
    --->    did_find_deconstruct
    ;       did_not_find_deconstruct.

    % find_bind_var(Var, ProcessUnify, Goal0, Goal, !Result, !Info,
    %   FoundDeconstruct):
    %
    % Used by both switch_detection and cse_detection. Searches through
    % `Goal0' looking for the first deconstruction unification with `Var'
    % or an alias of `Var'. If a deconstruction unification of the
    % variable is found, `ProcessUnify' is called to handle it (which may
    % replace the unification with some other goals, which is why we return
    % Goal), and searching is stopped. If we don't find such a deconstruction,
    % `!Result' is unchanged.
    %
:- pred find_bind_var(prog_var::in,
    process_unify(Result, Info)::in(process_unify),
    hlds_goal::in, hlds_goal::out, Result::in, Result::out,
    Info::in, Info::out, found_deconstruct::out) is det.

:- type process_unify(Result, Info) ==
    pred(prog_var, hlds_goal, list(hlds_goal), Result, Result, Info, Info).
:- inst process_unify == (pred(in, in, out, in, out, in, out) is det).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_util.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module term.
:- import_module unit.

%-----------------------------------------------------------------------------%

:- type allow_multi_arm
    --->    allow_multi_arm
    ;       dont_allow_multi_arm.

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
        AllowMulti = dont_allow_multi_arm
    ).

init_switch_detect_info(ModuleInfo) = Info :-
    lookup_allow_multi_arm(ModuleInfo, AllowMulti),
    Info = switch_detect_info(ModuleInfo, AllowMulti).

%-----------------------------------------------------------------------------%

detect_switches_in_module(!ModuleInfo) :-
    % Traverse the module structure, calling `detect_switches_in_goal'
    % for each procedure body.
    Info = init_switch_detect_info(!.ModuleInfo),
    module_info_get_valid_predids(ValidPredIds, !ModuleInfo),
    ValidPredIdSet = set_tree234.list_to_set(ValidPredIds),

    module_info_get_preds(!.ModuleInfo, PredMap0),
    map.to_assoc_list(PredMap0, PredIdsInfos0),

    detect_switches_in_preds(Info, ValidPredIdSet,
        PredIdsInfos0, PredIdsInfos),
    map.from_sorted_assoc_list(PredIdsInfos, PredMap),
    module_info_set_preds(PredMap, !ModuleInfo).

:- pred detect_switches_in_preds(switch_detect_info::in,
    set_tree234(pred_id)::in,
    assoc_list(pred_id, pred_info)::in, assoc_list(pred_id, pred_info)::out)
    is det.

detect_switches_in_preds(_, _, [], []).
detect_switches_in_preds(Info, ValidPredIdSet,
        [PredIdInfo0 | PredIdsInfos0], [PredIdInfo | PredIdsInfos]) :-
    PredIdInfo0 = PredId - PredInfo0,
    ( set_tree234.contains(ValidPredIdSet, PredId) ->
        detect_switches_in_pred(Info, PredId, PredInfo0, PredInfo),
        PredIdInfo = PredId - PredInfo
    ;
        PredIdInfo = PredIdInfo0
    ),
    detect_switches_in_preds(Info, ValidPredIdSet,
        PredIdsInfos0, PredIdsInfos).

:- pred detect_switches_in_pred(switch_detect_info::in, pred_id::in,
    pred_info::in, pred_info::out) is det.

detect_switches_in_pred(Info, PredId, !PredInfo) :-
    NonImportedProcIds = pred_info_non_imported_procids(!.PredInfo),
    (
        NonImportedProcIds = [_ | _],
        trace [io(!IO)] (
            ModuleInfo = Info ^ sdi_module_info,
            write_pred_progress_message("% Detecting switches in ", PredId,
                ModuleInfo, !IO)
        ),
        pred_info_get_procedures(!.PredInfo, ProcTable0),
        map.to_assoc_list(ProcTable0, ProcList0),
        detect_switches_in_procs(Info, NonImportedProcIds,
            ProcList0, ProcList),
        map.from_sorted_assoc_list(ProcList, ProcTable),
        pred_info_set_procedures(ProcTable, !PredInfo)

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
    ( list.member(ProcId, NonImportedProcIds) ->
        detect_switches_in_proc(Info, ProcInfo0, ProcInfo),
        ProcIdInfo = ProcId - ProcInfo
    ;
        ProcIdInfo = ProcIdInfo0
    ),
    detect_switches_in_procs(Info, NonImportedProcIds,
        ProcIdsInfos0, ProcIdsInfos).

detect_switches_in_proc(Info, !ProcInfo) :-
    % To process each ProcInfo, we get the goal, initialize the instmap
    % based on the modes of the head vars, and pass these to
    % `detect_switches_in_goal'.
    Info = switch_detect_info(ModuleInfo, AllowMulti),
    proc_info_get_vartypes(!.ProcInfo, VarTypes),
    LocalInfo0 =  local_switch_detect_info(ModuleInfo, AllowMulti, VarTypes,
        do_not_need_to_requantify),

    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_initial_instmap(!.ProcInfo, ModuleInfo, InstMap0),
    detect_switches_in_goal(InstMap0, Goal0, Goal,
        LocalInfo0, LocalInfo),
    proc_info_set_goal(Goal, !ProcInfo),
    Requant = LocalInfo ^ lsdi_requant,
    (
        Requant = need_to_requantify,
        requantify_proc_general(ordinary_nonlocals_maybe_lambda, !ProcInfo)
    ;
        Requant = do_not_need_to_requantify
    ).

%-----------------------------------------------------------------------------%

:- type local_switch_detect_info
    --->    local_switch_detect_info(
                lsdi_module_info        :: module_info,
                lsdi_allow_multi_arm    :: allow_multi_arm,
                lsdi_vartypes           :: vartypes,
                lsdi_requant            :: need_to_requantify
            ).

    % Given a goal, and the instmap on entry to that goal,
    % replace disjunctions with switches whereever possible.
    %
:- pred detect_switches_in_goal(instmap::in, hlds_goal::in, hlds_goal::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_switches_in_goal(InstMap0, !Goal, !LocalInfo) :-
    detect_switches_in_goal_update_instmap(InstMap0, _InstMap, !Goal,
        !LocalInfo).

    % This version is the same as the above except that it returns the
    % resulting instmap on exit from the goal, which is computed by applying
    % the instmap delta specified in the goal's goalinfo.
    %
:- pred detect_switches_in_goal_update_instmap(instmap::in, instmap::out,
    hlds_goal::in, hlds_goal::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_switches_in_goal_update_instmap(!InstMap, Goal0, Goal, !LocalInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    detect_switches_in_goal_expr(!.InstMap, GoalInfo, GoalExpr0, GoalExpr,
        !LocalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    update_instmap(Goal0, !InstMap).

    % Here we process each of the different sorts of goals.
    %
:- pred detect_switches_in_goal_expr(instmap::in, hlds_goal_info::in,
    hlds_goal_expr::in, hlds_goal_expr::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_switches_in_goal_expr(InstMap0, GoalInfo, GoalExpr0, GoalExpr,
        !LocalInfo) :-
    (
        GoalExpr0 = disj(Disjuncts0),
        (
            Disjuncts0 = [],
            GoalExpr = disj([])
        ;
            Disjuncts0 = [_ | _],
            NonLocals = goal_info_get_nonlocals(GoalInfo),
            set_of_var.to_sorted_list(NonLocals, NonLocalsList),
            detect_switches_in_disj(GoalInfo, NonLocalsList,
                Disjuncts0, NonLocalsList, InstMap0, [], GoalExpr, !LocalInfo)
        )
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        detect_switches_in_conj(InstMap0, Goals0, Goals, !LocalInfo),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = negation(SubGoal0),
        detect_switches_in_goal(InstMap0, SubGoal0, SubGoal, !LocalInfo),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        detect_switches_in_goal_update_instmap(InstMap0, InstMap1, Cond0, Cond,
            !LocalInfo),
        detect_switches_in_goal(InstMap1, Then0, Then, !LocalInfo),
        detect_switches_in_goal(InstMap0, Else0, Else, !LocalInfo),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        detect_switches_in_cases(Var, InstMap0, Cases0, Cases, !LocalInfo),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( Reason = from_ground_term(_, from_ground_term_construct) ->
            % There are neither disjunctions nor deconstruction unifications
            % inside these scopes.
            SubGoal = SubGoal0
        ;
            % XXX We could treat from_ground_term_deconstruct specially
            % as well, since the only variable whose deconstruction could be of
            % interest here is the one named in Reason.
            detect_switches_in_goal(InstMap0, SubGoal0, SubGoal, !LocalInfo)
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = unify(_, RHS0, _, _, _),
        (
            RHS0 = rhs_lambda_goal(_, _, _, _, _, Vars, Modes, _, LambdaGoal0),
            % We need to insert the initial insts for the lambda variables
            % in the instmap before processing the lambda goal.
            ModuleInfo = !.LocalInfo ^ lsdi_module_info,
            instmap.pre_lambda_update(ModuleInfo, Vars, Modes,
                InstMap0, InstMap1),
            detect_switches_in_goal(InstMap1, LambdaGoal0, LambdaGoal,
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
            detect_switches_in_goal(InstMap0, MainGoal0, MainGoal, !LocalInfo),
            detect_switches_in_orelse(InstMap0, OrElseGoals0, OrElseGoals,
                !LocalInfo),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            detect_switches_in_goal(InstMap0, SubGoal0, SubGoal, !LocalInfo),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($module, $pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ).

:- pred detect_sub_switches_in_disj(instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_sub_switches_in_disj(_, [], [], !LocalInfo).
detect_sub_switches_in_disj(InstMap, [Goal0 | Goals0], [Goal | Goals],
        !LocalInfo) :-
    detect_switches_in_goal(InstMap, Goal0, Goal, !LocalInfo),
    detect_sub_switches_in_disj(InstMap, Goals0, Goals, !LocalInfo).

:- pred detect_switches_in_cases(prog_var::in, instmap::in,
    list(case)::in, list(case)::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_switches_in_cases(_, _, [], [], !LocalInfo).
detect_switches_in_cases(Var, InstMap0, [Case0 | Cases0], [Case | Cases],
        !LocalInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    VarTypes = !.LocalInfo ^ lsdi_vartypes,
    ModuleInfo0 = !.LocalInfo ^ lsdi_module_info,
    lookup_var_type(VarTypes, Var, VarType),
    bind_var_to_functors(Var, VarType, MainConsId, OtherConsIds,
        InstMap0, InstMap1, ModuleInfo0, ModuleInfo),
    !LocalInfo ^ lsdi_module_info := ModuleInfo,
    detect_switches_in_goal(InstMap1, Goal0, Goal, !LocalInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    detect_switches_in_cases(Var, InstMap0, Cases0, Cases, !LocalInfo).

:- pred detect_switches_in_conj(instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_switches_in_conj(_, [], [], !LocalInfo).
detect_switches_in_conj(InstMap0,
        [Goal0 | Goals0], [Goal | Goals], !LocalInfo) :-
    detect_switches_in_goal_update_instmap(InstMap0, InstMap1, Goal0, Goal,
        !LocalInfo),
    detect_switches_in_conj(InstMap1, Goals0, Goals, !LocalInfo).

:- pred detect_switches_in_orelse(instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_switches_in_orelse(_, [], [], !LocalInfo).
detect_switches_in_orelse(InstMap, [Goal0 | Goals0], [Goal | Goals],
        !LocalInfo) :-
    detect_switches_in_goal(InstMap, Goal0, Goal, !LocalInfo),
    detect_switches_in_orelse(InstMap, Goals0, Goals, !LocalInfo).

%-----------------------------------------------------------------------------%

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

convert_case(GoalInfo, ConflictConsIds, ConsId - Entry, !Cases,
        !AlreadyHandledConsIds) :-
    ( set_tree234.contains(!.AlreadyHandledConsIds, ConsId) ->
        Entry = cons_id_entry(State, _ArmCord),
        expect(unify(State, cons_id_has_one_multi), $module, $pred,
            "already handled but not cons_id_has_one_multi")
    ;
        Entry = cons_id_entry(State, ArmsCord),
        Arms = cord.list(ArmsCord),
        (
            State = cons_id_has_conflict,
            set_tree234.is_member(ConflictConsIds, ConsId, IsMember),
            expect(unify(IsMember, yes), $module, $pred,
                "conflict status but not in ConflictConsIds"),
            Disjuncts = list.map(project_arm_goal, Arms),
            disj_list_to_goal(Disjuncts, GoalInfo, Goal),
            Case = case(ConsId, [], Goal),
            !:Cases = [Case | !.Cases]
        ;
            State = cons_id_has_all_singles,
            set_tree234.is_member(ConflictConsIds, ConsId, IsMember),
            expect(unify(IsMember, no), $module, $pred,
                "singles status but in ConflictConsIds"),
            Disjuncts = list.map(project_single_arm_goal, Arms),
            disj_list_to_goal(Disjuncts, GoalInfo, Goal),
            Case = case(ConsId, [], Goal),
            !:Cases = [Case | !.Cases]
        ;
            State = cons_id_has_one_multi,
            ( Arms = [multi_cons_id_arm(MainConsId0, OtherConsIds0, Goal)] ->
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
                    unexpected($module, $pred,
                        "cons_id_has_one_multi: AllConsIds = []")
                )
            ;
                unexpected($module, $pred, "misleading cons_id_has_one_multi")
            )
        )
    ).

:- func project_arm_goal(case_arm) = hlds_goal.

project_arm_goal(single_cons_id_arm(_, Goal)) = Goal.
project_arm_goal(multi_cons_id_arm(_, _, Goal)) = Goal.

:- func project_single_arm_goal(case_arm) = hlds_goal.

project_single_arm_goal(single_cons_id_arm(_, Goal)) = Goal.
project_single_arm_goal(multi_cons_id_arm(_, _, _)) = _ :-
    unexpected($module, $pred, "multi arm").

:- func num_cases_in_table(cases_table) = int.

num_cases_in_table(cases_table(CasesMap, _)) = map.count(CasesMap).

:- pred add_single_entry(cons_id::in, hlds_goal::in,
    cases_table::in, cases_table::out) is det.

add_single_entry(ConsId, Goal, CasesTable0, CasesTable) :-
    CasesTable0 = cases_table(CasesMap0, ConflictConsIds0),
    Arm = single_cons_id_arm(ConsId, Goal),
    ( map.search(CasesMap0, ConsId, Entry0) ->
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
    ;
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
    ( map.search(CasesMap0, ConsId, Entry0) ->
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
        Arms = snoc(Arms0, Arm),
        Entry = cons_id_entry(State, Arms),
        map.det_update(ConsId, Entry, CasesMap0, CasesMap)
    ;
        State = cons_id_has_one_multi,
        Arms = cord.singleton(Arm),
        Entry = cons_id_entry(State, Arms),
        map.det_insert(ConsId, Entry, CasesMap0, CasesMap),
        ConflictConsIds = ConflictConsIds0
    ),
    CasesTable = cases_table(CasesMap, ConflictConsIds).

:- type again
    --->    again(prog_var, list(hlds_goal), list(case)).

    % This is the interesting bit - we've found a non-empty disjunction,
    % and we've got a list of the non-local variables of that disjunction.
    % Now for each non-local variable, we check whether there is a partition
    % of the disjuncts such that each group of disjunctions can only succeed
    % if the variable is bound to a different functor.
    %
:- pred detect_switches_in_disj(hlds_goal_info::in, list(prog_var)::in,
    list(hlds_goal)::in, list(prog_var)::in, instmap::in, list(again)::in,
    hlds_goal_expr::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

detect_switches_in_disj(GoalInfo, AllVars, Disjuncts0,
        [Var | Vars], InstMap, AgainList0, GoalExpr, !LocalInfo) :-
    % Can we do at least a partial switch on this variable?
    (
        instmap_lookup_var(InstMap, Var, VarInst0),
        ModuleInfo = !.LocalInfo ^ lsdi_module_info,
        inst_is_bound(ModuleInfo, VarInst0),
        partition_disj(Disjuncts0, Var, GoalInfo, Left, CasesList, !LocalInfo)
    ->
        % A switch needs to have at least two cases.
        %
        % But, if there is a complete one-case switch for a goal, we must leave
        % it as a disjunction rather than doing an incomplete switch on a
        % different variable, because otherwise we might get determinism
        % analysis wrong.  (The complete one-case switch may be decomposable
        % into other complete sub-switches on the functor's arguments)
        (
            % Are there any disjuncts that are not part of the switch? No.
            Left = [],
            ( CasesList = [_, _ | _] ->
                cases_to_switch(Var, CasesList, InstMap, GoalExpr, !LocalInfo)
            ;
                detect_sub_switches_in_disj(InstMap, Disjuncts0, Disjuncts,
                    !LocalInfo),
                GoalExpr = disj(Disjuncts)
            )
        ;
            % Are there any disjuncts that are not part of the switch? Yes.
            Left = [_ | _],
            % Insert this switch into the list of incomplete switches
            % only if it has at least two cases.
            ( CasesList = [_, _ | _] ->
                AgainList1 = [again(Var, Left, CasesList) | AgainList0]
            ;
                AgainList1 = AgainList0
            ),
            % Try to find a switch.
            detect_switches_in_disj(GoalInfo, AllVars, Disjuncts0,
                Vars, InstMap, AgainList1, GoalExpr, !LocalInfo)
        )
    ;
        detect_switches_in_disj(GoalInfo, AllVars, Disjuncts0,
            Vars, InstMap, AgainList0, GoalExpr, !LocalInfo)
    ).
detect_switches_in_disj(GoalInfo, AllVars, Disjuncts0,
        [], InstMap, AgainList0, disj(Disjuncts), !LocalInfo) :-
    (
        AgainList0 = [],
        detect_sub_switches_in_disj(InstMap, Disjuncts0, Disjuncts, !LocalInfo)
    ;
        AgainList0 = [Again | AgainList1],
        select_best_switch(AgainList1, Again, BestAgain),
        BestAgain = again(Var, Left0, CasesList),
        cases_to_switch(Var, CasesList, InstMap, SwitchGoalExpr, !LocalInfo),
        detect_switches_in_disj(GoalInfo, AllVars, Left0, AllVars, InstMap,
            [], Left, !LocalInfo),
        goal_to_disj_list(hlds_goal(Left, GoalInfo), LeftList),
        Disjuncts = [hlds_goal(SwitchGoalExpr, GoalInfo) | LeftList]
    ).

:- pred select_best_switch(list(again)::in, again::in, again::out) is det.

select_best_switch([], BestAgain, BestAgain).
select_best_switch([Again | AgainList], BestAgain0, BestAgain) :-
    (
        Again = again(_, _, CasesList),
        BestAgain0 = again(_, _, BestCasesList),
        list.length(CasesList, Length),
        list.length(BestCasesList, BestLength),
        Length < BestLength
    ->
        BestAgain1 = BestAgain0
    ;
        BestAgain1 = Again
    ),
    select_best_switch(AgainList, BestAgain1, BestAgain).

%-----------------------------------------------------------------------------%

    % partition_disj(Disjuncts, Var, GoalInfo, Left, Cases, !LocalInfo):
    %
    % Attempts to partition the disjunction `Disjuncts' into a switch on `Var'.
    % If at least partially successful, returns the resulting `Cases', with
    % any disjunction goals not fitting into the switch in Left.
    %
    % Given the list of goals in a disjunction, and an input variable to switch
    % on, we attempt to partition the goals into a switch. For each constructor
    % id, we record the list of disjuncts which unify the variable with that
    % constructor. We partition the goals by abstractly interpreting the
    % unifications at the start of each disjunction, to build up a
    % substitution.
    %
:- pred partition_disj(list(hlds_goal)::in, prog_var::in, hlds_goal_info::in,
    list(hlds_goal)::out, list(case)::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is semidet.

partition_disj(Disjuncts0, Var, GoalInfo, Left, Cases, !LocalInfo) :-
    CasesTable0 = cases_table(map.init, set_tree234.init),
    partition_disj_trial(Disjuncts0, Var, [], Left1, CasesTable0, CasesTable1),
    (
        Left1 = [],
        % There must be at least one case in CasesTable1.
        num_cases_in_table(CasesTable1) >= 1,
        Left = Left1,
        Cases = convert_cases_table(GoalInfo, CasesTable1)
    ;
        Left1 = [_ | _],
        % We don't insist on there being at least one case in CasesTable1,
        % to allow for switches in which *all* cases contain subsidiary
        % disjunctions.
        AllowMulti = !.LocalInfo ^ lsdi_allow_multi_arm,
        ( expand_sub_disjs(AllowMulti, Var, Left1, CasesTable1, CasesTable) ->
            Left = [],
            num_cases_in_table(CasesTable) >= 1,
            Cases = convert_cases_table(GoalInfo, CasesTable),
            !LocalInfo ^ lsdi_requant := need_to_requantify
        ;
            Left = Left1,
            Cases = convert_cases_table(GoalInfo, CasesTable1)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred expand_sub_disjs(allow_multi_arm::in, prog_var::in,
    list(hlds_goal)::in, cases_table::in, cases_table::out) is semidet.

expand_sub_disjs(_AllowMulti, _Var, [], !CasesTable).
expand_sub_disjs(AllowMulti, Var, [LeftGoal | LeftGoals], !CasesTable) :-
    expand_sub_disj(AllowMulti, Var, LeftGoal, !CasesTable),
    expand_sub_disjs(AllowMulti, Var, LeftGoals, !CasesTable).

:- pred expand_sub_disj(allow_multi_arm::in, prog_var::in, hlds_goal::in,
    cases_table::in, cases_table::out) is semidet.

expand_sub_disj(AllowMulti, Var, Goal, !CasesTable) :-
    Goal = hlds_goal(GoalExpr, GoalInfo0),
    goal_info_add_feature(feature_duplicated_for_switch, GoalInfo0, GoalInfo),
    ( GoalExpr = conj(plain_conj, SubGoals) ->
        expand_sub_disj_process_conj(AllowMulti, Var, SubGoals, [], GoalInfo,
            !CasesTable)
    ; GoalExpr = disj(_) ->
        expand_sub_disj_process_conj(AllowMulti, Var, [Goal], [], GoalInfo,
            !CasesTable)
    ;
        fail
    ).

:- pred expand_sub_disj_process_conj(allow_multi_arm::in, prog_var::in,
    list(hlds_goal)::in, list(hlds_goal)::in, hlds_goal_info::in,
    cases_table::in, cases_table::out) is semidet.

expand_sub_disj_process_conj(AllowMulti, Var, ConjGoals, !.RevUnifies,
        GoalInfo, !CasesTable) :-
    (
        ConjGoals = [],
        fail
    ;
        ConjGoals = [FirstGoal | LaterGoals],
        FirstGoal = hlds_goal(FirstGoalExpr, FirstGoalInfo),
        (
            FirstGoalExpr = unify(_, _, _, _, _),
            !:RevUnifies = [FirstGoal | !.RevUnifies],
            expand_sub_disj_process_conj(AllowMulti, Var, LaterGoals,
                !.RevUnifies, GoalInfo, !CasesTable)
        ;
            FirstGoalExpr = disj(Disjuncts),
            Disjuncts = [_ | _],
            (
                AllowMulti = allow_multi_arm,
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
            ->
                SharedGoal = hlds_goal(conj(plain_conj, LaterGoals), GoalInfo),
                add_multi_entry(MainConsId, OtherConsIds, SharedGoal,
                    !CasesTable)
            ;
                list.reverse(!.RevUnifies, Unifies),
                list.map(
                    create_expanded_conjunction(Unifies, LaterGoals, GoalInfo),
                    Disjuncts, ExpandedConjunctions),
                partition_disj_trial(ExpandedConjunctions, Var, [], Left,
                    !CasesTable),
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
    ( Disjunct = hlds_goal(conj(plain_conj, DisjunctGoals), _) ->
        Conjuncts = Unifies ++ DisjunctGoals ++ LaterGoals
    ;
        Conjuncts = Unifies ++ [Disjunct] ++ LaterGoals
    ),
    Goal = hlds_goal(conj(plain_conj, Conjuncts), GoalInfo).

%-----------------------------------------------------------------------------%

:- pred partition_disj_trial(list(hlds_goal)::in, prog_var::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    cases_table::in, cases_table::out) is det.

partition_disj_trial([], _Var, !Left, !CasesTable).
partition_disj_trial([Disjunct0 | Disjuncts0], Var, !Left, !CasesTable) :-
    find_bind_var(Var, find_bind_var_for_switch_in_deconstruct, Disjunct0,
        Disjunct, no, MaybeConsId, unit, _, _),
    (
        MaybeConsId = yes(ConsId),
        add_single_entry(ConsId, Disjunct, !CasesTable)
    ;
        MaybeConsId = no,
        !:Left = [Disjunct0 | !.Left]
    ),
    partition_disj_trial(Disjuncts0, Var, !Left, !CasesTable).

:- pred find_bind_var_for_switch_in_deconstruct(prog_var::in, hlds_goal::in,
    list(hlds_goal)::out, maybe(cons_id)::in, maybe(cons_id)::out,
    unit::in, unit::out) is det.

find_bind_var_for_switch_in_deconstruct(SwitchVar, Goal0, Goals,
        _Result0, Result, _, unit) :-
    (
        Goal0 = hlds_goal(GoalExpr0, GoalInfo),
        UnifyInfo0 = GoalExpr0 ^ unify_kind,
        UnifyInfo0 = deconstruct(UnifyVar, Functor, ArgVars, _, _, _)
    ->
        Result = yes(Functor),
        (
            ArgVars = [],
            SwitchVar = UnifyVar
        ->
            % The test will get carried out in the switch, there are no
            % argument values to pick up, and the test was on the switch
            % variable (not on one of its aliases), so the unification
            % serve no further purpose. We delete it here, so simplify
            % doesn't have to.
            Goals = []
        ;
            % The deconstruction unification now becomes deterministic, since
            % the test will get carried out in the switch.
            UnifyInfo = UnifyInfo0 ^ deconstruct_can_fail := cannot_fail,
            GoalExpr = GoalExpr0 ^ unify_kind := UnifyInfo,
            Goal = hlds_goal(GoalExpr, GoalInfo),
            Goals = [Goal]
        )
    ;
        unexpected($module, $pred, "condition failed")
    ).

%-----------------------------------------------------------------------------%

find_bind_var(Var, ProcessUnify, !Goal, !Result, !Info, FoundDeconstruct) :-
    map.init(Subst),
    find_bind_var_2(Var, ProcessUnify, !Goal, Subst, _, !Result, !Info,
        DeconstructSearch),
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

find_bind_var_2(Var, ProcessUnify, Goal0, Goal, !Subst, !Result, !Info,
        FoundDeconstruct) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = scope(Reason0, SubGoal0),
        ( Reason0 = from_ground_term(_, from_ground_term_construct) ->
            % There are no deconstruction unifications inside these scopes.
            Goal = Goal0,
            % Whether we want to keep looking at the code that follows them
            % is a more interesting question. Since we keep going after
            % construction unifications (whose behavior this scope resembles),
            % we keep going.
            FoundDeconstruct = before_deconstruct
        ;
            find_bind_var_2(Var, ProcessUnify, SubGoal0, SubGoal, !Subst,
                !Result, !Info, FoundDeconstruct),
            (
                FoundDeconstruct = found_deconstruct,
                Reason0 = from_ground_term(_, from_ground_term_deconstruct)
            ->
                % If we remove a goal from such a scope, what is left
                % may no longer satisfy the invariants we expect it to satisfy.
                Goal = SubGoal
            ;
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
        GoalExpr0 = unify(LHS, RHS, _, UnifyInfo0, _),
        (
            % Check whether the unification is a deconstruction unification
            % on either Var or on a variable aliased to Var.
            UnifyInfo0 = deconstruct(UnifyVar, _, _, _, _, _),
            term.apply_rec_substitution(term.variable(Var, context_init),
                !.Subst, term.variable(SubstVar, context_init)),
            term.apply_rec_substitution(term.variable(UnifyVar, context_init),
                !.Subst, term.variable(SubstUnifyVar, context_init)),
            SubstVar = SubstUnifyVar
        ->
            call(ProcessUnify, Var, Goal0, Goals, !Result, !Info),
            conj_list_to_goal(Goals, GoalInfo, Goal),
            FoundDeconstruct = found_deconstruct
        ;
            Goal = Goal0,
            FoundDeconstruct = before_deconstruct,
            % Otherwise abstractly interpret the unification.
            ( interpret_unify(LHS, RHS, !.Subst, NewSubst) ->
                !:Subst = NewSubst
            ;
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
        ( goal_info_has_feature(GoalInfo, feature_from_head) ->
            FoundDeconstruct = before_deconstruct
        ;
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
            unexpected($module, $pred, "bi_implication")
        )
    ).

:- pred conj_find_bind_var(prog_var::in,
    process_unify(Result, Info)::in(process_unify),
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_substitution::in, prog_substitution::out, Result::in, Result::out,
    Info::in, Info::out, deconstruct_search::out) is det.

conj_find_bind_var(_Var, _, [], [], !Subst, !Result, !Info,
        before_deconstruct).
conj_find_bind_var(Var, ProcessUnify, [Goal0 | Goals0], [Goal | Goals],
        !Subst, !Result, !Info, FoundDeconstruct) :-
    find_bind_var_2(Var, ProcessUnify, Goal0, Goal, !Subst,
        !Result, !Info, FoundDeconstruct1),
    (
        FoundDeconstruct1 = before_deconstruct,
        conj_find_bind_var(Var, ProcessUnify, Goals0, Goals,
            !Subst, !Result, !Info, FoundDeconstruct)
    ;
        ( FoundDeconstruct1 = found_deconstruct
        ; FoundDeconstruct1 = given_up_search
        ),
        FoundDeconstruct = FoundDeconstruct1,
        Goals = Goals0
    ).

%-----------------------------------------------------------------------------%

:- pred cases_to_switch(prog_var::in, list(case)::in, instmap::in,
    hlds_goal_expr::out,
    local_switch_detect_info::in, local_switch_detect_info::out) is det.

cases_to_switch(Var, Cases0, InstMap, GoalExpr, !LocalInfo) :-
    ModuleInfo = !.LocalInfo ^ lsdi_module_info,
    VarTypes = !.LocalInfo ^ lsdi_vartypes,
    instmap_lookup_var(InstMap, Var, VarInst),
    lookup_var_type(VarTypes, Var, Type),
    ( inst_is_bound_to_functors(ModuleInfo, VarInst, Functors) ->
        type_to_ctor_det(Type, TypeCtor),
        bound_insts_to_cons_ids(TypeCtor, Functors, ConsIds),
        delete_unreachable_cases(Cases0, ConsIds, Cases1),
        CanFail = compute_can_fail(ConsIds, Cases1)
    ;
        Cases1 = Cases0,
        ( switch_type_num_functors(ModuleInfo, Type, NumFunctors) ->
            % We could check for each cons_id of the type whether a case covers
            % it, but given that type checking ensures that the set of covered
            % cons_ids is a subset of the set of cons_ids of the type, checking
            % whether the cardinalities of the two sets match is *equivalent*
            % to checking whether they are the same set.
            CanFail = switch_covers_n_cases(NumFunctors, Cases1)
        ;
            % switch_type_num_functors fails only for types on which
            % you cannot have a complete switch, e.g. integers and strings.
            CanFail = can_fail
        )
    ),
    detect_switches_in_cases(Var, InstMap, Cases1, Cases, !LocalInfo),

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

:- func compute_can_fail(list(cons_id), list(case)) = can_fail.

compute_can_fail(Functors, Cases) = SwitchCanFail :-
    UncoveredFunctors0 = set_tree234.list_to_set(Functors),
    delete_covered_functors(Cases, UncoveredFunctors0, UncoveredFunctors),
    ( set_tree234.empty(UncoveredFunctors) ->
        SwitchCanFail = cannot_fail
    ;
        SwitchCanFail = can_fail
    ).

    % Delete from !UncoveredConsIds all cons_ids mentioned in any of the cases.
    %
:- pred delete_covered_functors(list(case)::in,
    set_tree234(cons_id)::in, set_tree234(cons_id)::out) is det.

delete_covered_functors([], !UncoveredConsIds).
delete_covered_functors([Case | Cases], !UncoveredConsIds) :-
    Case = case(MainConsId, OtherConsIds, _Goal),
    set_tree234.delete(MainConsId, !UncoveredConsIds),
    list.foldl(set_tree234.delete, OtherConsIds, !UncoveredConsIds),
    delete_covered_functors(Cases, !UncoveredConsIds).

    % Check whether a switch handles the given number of cons_ids.
    %
:- func switch_covers_n_cases(int, list(case)) = can_fail.

switch_covers_n_cases(NumFunctors, Cases) = SwitchCanFail :-
    NumCoveredConsIds = count_covered_cons_ids(Cases),
    ( NumCoveredConsIds = NumFunctors ->
        SwitchCanFail = cannot_fail
    ;
        SwitchCanFail = can_fail
    ).

:- func count_covered_cons_ids(list(case)) = int.

count_covered_cons_ids([]) = 0.
count_covered_cons_ids([Case | Cases]) = CaseCount + CasesCount :-
    Case = case(_MainConsId, OtherConsIds, _Goal),
    CaseCount = 1 + list.length(OtherConsIds),
    CasesCount = count_covered_cons_ids(Cases).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.switch_detection.
%-----------------------------------------------------------------------------%
