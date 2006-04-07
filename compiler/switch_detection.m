%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: switch_detection.
% Main author: fjh.

% Switch detection - when a disjunction contains disjuncts that unify the
% same input variable with different function symbols, replace (part of)
% the disjunction with a switch.

%-----------------------------------------------------------------------------%

:- module check_hlds.switch_detection.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.

:- pred detect_switches(module_info::in, module_info::out,
    io::di, io::uo) is det.

:- pred detect_switches_in_proc(proc_id::in, pred_id::in,
    module_info::in, module_info::out) is det.

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
    Info::in, Info::out, bool::out) is det.

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
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.

:- import_module assoc_list.
:- import_module char.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module term.
:- import_module unit.

%-----------------------------------------------------------------------------%

detect_switches(!ModuleInfo, !IO) :-
    % Traverse the module structure, calling `detect_switches_in_goal'
    % for each procedure body.
    module_info_predids(!.ModuleInfo, PredIds),
    detect_switches_in_preds(PredIds, !ModuleInfo, !IO).

:- pred detect_switches_in_preds(list(pred_id)::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

detect_switches_in_preds([], !ModuleInfo, !IO).
detect_switches_in_preds([PredId | PredIds], !ModuleInfo, !IO) :-
    module_info_preds(!.ModuleInfo, PredTable),
    map.lookup(PredTable, PredId, PredInfo),
    detect_switches_in_pred(PredId, PredInfo, !ModuleInfo, !IO),
    detect_switches_in_preds(PredIds, !ModuleInfo, !IO).

:- pred detect_switches_in_pred(pred_id::in, pred_info::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

detect_switches_in_pred(PredId, PredInfo0, !ModuleInfo, !IO) :-
    ProcIds = pred_info_non_imported_procids(PredInfo0),
    (
        ProcIds = [_ | _],
        write_pred_progress_message("% Detecting switches in ", PredId,
            !.ModuleInfo, !IO),
        detect_switches_in_procs(ProcIds, PredId, !ModuleInfo)
        % This is where we should print statistics, if we ever need
        % to debug the performance of switch detection.
    ;
        ProcIds = []
    ).

:- pred detect_switches_in_procs(list(proc_id)::in, pred_id::in,
    module_info::in, module_info::out) is det.

detect_switches_in_procs([], _PredId, !ModuleInfo).
detect_switches_in_procs([ProcId | ProcIds], PredId, !ModuleInfo) :-
    detect_switches_in_proc(ProcId, PredId, !ModuleInfo),
    detect_switches_in_procs(ProcIds, PredId, !ModuleInfo).

detect_switches_in_proc(ProcId, PredId, !ModuleInfo) :-
    module_info_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_procedures(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),

    % To process each ProcInfo, we get the goal, initialize the instmap
    % based on the modes of the head vars, and pass these to
    % `detect_switches_in_goal'.
    proc_info_get_goal(ProcInfo0, Goal0),
    proc_info_get_vartypes(ProcInfo0, VarTypes),
    proc_info_get_initial_instmap(ProcInfo0, !.ModuleInfo, InstMap0),
    detect_switches_in_goal(!.ModuleInfo, VarTypes, InstMap0, Goal0, Goal,
        no, Requant),

    proc_info_set_goal(Goal, ProcInfo0, ProcInfo1),
    (
        Requant = yes,
        requantify_proc(ProcInfo1, ProcInfo)
    ;
        Requant = no,
        ProcInfo = ProcInfo1
    ),
    map.det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
    pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
    map.det_update(PredTable0, PredId, PredInfo, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo).

%-----------------------------------------------------------------------------%

    % Given a goal, and the instmap on entry to that goal,
    % replace disjunctions with switches whereever possible.
    %
:- pred detect_switches_in_goal(module_info::in, vartypes::in,
    instmap::in, hlds_goal::in, hlds_goal::out, bool::in, bool::out) is det.

detect_switches_in_goal(ModuleInfo, VarTypes, InstMap0, !Goal, !Requant) :-
    detect_switches_in_goal_1(ModuleInfo, VarTypes, InstMap0, _InstMap,
        !Goal, !Requant).

    % This version is the same as the above except that it returns the
    % resulting instmap on exit from the goal, which is computed by applying
    % the instmap delta specified in the goal's goalinfo.
    %
:- pred detect_switches_in_goal_1(module_info::in, vartypes::in,
    instmap::in, instmap::out, hlds_goal::in, hlds_goal::out,
    bool::in, bool::out) is det.

detect_switches_in_goal_1(ModuleInfo, VarTypes, !InstMap,
        Goal0 - GoalInfo, Goal - GoalInfo, !Requant) :-
    detect_switches_in_goal_2(ModuleInfo, VarTypes, !.InstMap, GoalInfo,
        Goal0, Goal, !Requant),
    update_instmap(Goal0 - GoalInfo, !InstMap).

    % Here we process each of the different sorts of goals.
    %
:- pred detect_switches_in_goal_2(module_info::in, vartypes::in, instmap::in,
    hlds_goal_info::in, hlds_goal_expr::in, hlds_goal_expr::out,
    bool::in, bool::out) is det.

detect_switches_in_goal_2(ModuleInfo, VarTypes, InstMap0, GoalInfo,
        Goal0, Goal, !Requant) :-
    (
        Goal0 = disj(Goals0),
        (
            Goals0 = [],
            Goal = disj([])
        ;
            Goals0 = [_ | _],
            goal_info_get_nonlocals(GoalInfo, NonLocals),
            set.to_sorted_list(NonLocals, NonLocalsList),
            detect_switches_in_disj(NonLocalsList, Goals0, GoalInfo, InstMap0,
                VarTypes, NonLocalsList, ModuleInfo, [], Goal, !Requant)
        )
    ;
        Goal0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            detect_switches_in_conj(ModuleInfo, VarTypes, InstMap0,
                Goals0, Goals, !Requant)
        ;
            ConjType = parallel_conj,
            detect_switches_in_par_conj(ModuleInfo, VarTypes, InstMap0,
                Goals0, Goals, !Requant)
        ),
        Goal = conj(ConjType, Goals)
    ;
        Goal0 = not(SubGoal0),
        detect_switches_in_goal(ModuleInfo, VarTypes, InstMap0,
            SubGoal0, SubGoal, !Requant),
        Goal = not(SubGoal)
    ;
        Goal0 = if_then_else(Vars, Cond0, Then0, Else0),
        detect_switches_in_goal_1(ModuleInfo, VarTypes, InstMap0, InstMap1,
            Cond0, Cond, !Requant),
        detect_switches_in_goal(ModuleInfo, VarTypes, InstMap1, Then0, Then,
            !Requant),
        detect_switches_in_goal(ModuleInfo, VarTypes, InstMap0, Else0, Else,
            !Requant),
        Goal = if_then_else(Vars, Cond, Then, Else)
    ;
        Goal0 = switch(Var, CanFail, Cases0),
        detect_switches_in_cases(ModuleInfo, VarTypes, InstMap0,
            Cases0, Cases,  !Requant),
        Goal = switch(Var, CanFail, Cases)
    ;
        Goal0 = scope(Reason, SubGoal0),
        detect_switches_in_goal(ModuleInfo, VarTypes, InstMap0,
            SubGoal0, SubGoal, !Requant),
        Goal = scope(Reason, SubGoal)
    ;
        Goal0 = unify(_, RHS0, _, _, _),
        ( RHS0 = lambda_goal(_, _, _, _, Vars, Modes, _, LambdaGoal0) ->
            % We need to insert the initial insts for the lambda variables
            % in the instmap before processing the lambda goal.
            instmap.pre_lambda_update(ModuleInfo, Vars, Modes,
                InstMap0, InstMap1),
            detect_switches_in_goal(ModuleInfo, VarTypes, InstMap1,
                LambdaGoal0, LambdaGoal, !Requant),
            RHS = RHS0 ^ rhs_lambda_goal := LambdaGoal,
            Goal = Goal0 ^ unify_rhs := RHS
        ;
            Goal = Goal0
        )
    ;
        Goal0 = generic_call(_, _, _, _),
        Goal = Goal0
    ;
        Goal0 = call(_, _, _, _, _, _),
        Goal = Goal0
    ;
        Goal0 = foreign_proc(_, _, _, _, _, _),
        Goal = Goal0
    ;
        Goal0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected(this_file, "detect_switches_in_goal_2: shorthand")
    ).

%-----------------------------------------------------------------------------%

:- type cases == map(cons_id, list(hlds_goal)).

:- type sorted_case_list == list(case).
    % the sorted_case_list should always be sorted on cons_id -
    % `delete_unreachable_cases' relies on this.

:- type again
    --->    again(prog_var, list(hlds_goal), sorted_case_list).

    % This is the interesting bit - we've found a non-empty disjunction,
    % and we've got a list of the non-local variables of that disjunction.
    % Now for each non-local variable, we check whether there is a partition
    % of the disjuncts such that each group of disjunctions can only succeed
    % if the variable is bound to a different functor.
    %
:- pred detect_switches_in_disj(list(prog_var)::in, list(hlds_goal)::in,
    hlds_goal_info::in, instmap::in, vartypes::in,
    list(prog_var)::in, module_info::in, list(again)::in,
    hlds_goal_expr::out, bool::in, bool::out) is det.

detect_switches_in_disj([Var | Vars], Goals0, GoalInfo, InstMap,
        VarTypes, AllVars, ModuleInfo, Again0, Goal, !Requant) :-
    % Can we do at least a partial switch on this variable?
    (
        instmap.lookup_var(InstMap, Var, VarInst0),
        inst_is_bound(ModuleInfo, VarInst0),
        partition_disj(Goals0, Var, GoalInfo, Left, CasesList, !Requant)
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
                cases_to_switch(CasesList, Var, VarTypes, GoalInfo, InstMap,
                    ModuleInfo, Goal, !Requant)
            ;
                detect_sub_switches_in_disj(ModuleInfo, VarTypes, InstMap,
                    Goals0, Goals, !Requant),
                Goal = disj(Goals)
            )
        ;
            % Are there any disjuncts that are not part of the switch? Yes.
            Left = [_ | _],
            % Insert this switch into the list of incomplete switches
            % only if it has at least two cases.
            ( CasesList = [_, _ | _] ->
                Again1 = [again(Var, Left, CasesList) | Again0]
            ;
                Again1 = Again0
            ),
            % Try to find a switch.
            detect_switches_in_disj(Vars, Goals0, GoalInfo, InstMap, VarTypes,
                AllVars, ModuleInfo, Again1, Goal, !Requant)
        )
    ;
        detect_switches_in_disj(Vars, Goals0, GoalInfo, InstMap,
            VarTypes, AllVars, ModuleInfo, Again0, Goal, !Requant)
    ).
detect_switches_in_disj([], Goals0, GoalInfo, InstMap,
        VarTypes, AllVars, ModuleInfo, AgainList0, disj(Goals), !Requant) :-
    (
        AgainList0 = [],
        detect_sub_switches_in_disj(ModuleInfo, VarTypes, InstMap,
            Goals0, Goals, !Requant)
    ;
        AgainList0 = [Again | AgainList1],
        select_best_switch(AgainList1, Again, BestAgain),
        BestAgain = again(Var, Left0, CasesList),
        cases_to_switch(CasesList, Var, VarTypes, GoalInfo, InstMap,
            ModuleInfo, SwitchGoal, !Requant),
        detect_switches_in_disj(AllVars, Left0, GoalInfo, InstMap,
            VarTypes, AllVars, ModuleInfo, [], Left, !Requant),
        goal_to_disj_list(Left - GoalInfo, LeftList),
        Goals = [SwitchGoal - GoalInfo | LeftList]
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

:- pred detect_sub_switches_in_disj(module_info::in, vartypes::in, instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out, bool::in, bool::out) is det.

detect_sub_switches_in_disj(_ModuleInfo, _VarTypes, _InstMap, [], [],
        !Requant).
detect_sub_switches_in_disj(ModuleInfo, VarTypes, InstMap,
        [Goal0 | Goals0], [Goal | Goals], !Requant) :-
    detect_switches_in_goal(ModuleInfo, VarTypes, InstMap, Goal0, Goal,
        !Requant),
    detect_sub_switches_in_disj(ModuleInfo, VarTypes, InstMap,
        Goals0, Goals, !Requant).

:- pred detect_switches_in_cases(module_info::in, vartypes::in, instmap::in,
    list(case)::in, list(case)::out, bool::in, bool::out) is det.

detect_switches_in_cases(_, _, _, [], [], !Requant).
detect_switches_in_cases(ModuleInfo, VarTypes, InstMap,
        [Case0 | Cases0], [Case | Cases], !Requant) :-
    Case0 = case(Functor, Goal0),
    detect_switches_in_goal(ModuleInfo, VarTypes, InstMap, Goal0, Goal,
        !Requant),
    Case = case(Functor, Goal),
    detect_switches_in_cases(ModuleInfo, VarTypes, InstMap, Cases0, Cases,
        !Requant).

:- pred detect_switches_in_par_conj(module_info::in, vartypes::in, instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out, bool::in, bool::out) is det.

detect_switches_in_par_conj(_, _, _, [], [], !Requant).
detect_switches_in_par_conj(ModuleInfo, VarTypes, InstMap,
        [Goal0 | Goals0], [Goal | Goals], !Requant) :-
    detect_switches_in_goal(ModuleInfo, VarTypes, InstMap, Goal0, Goal,
        !Requant),
    detect_switches_in_par_conj(ModuleInfo, VarTypes, InstMap,
        Goals0, Goals, !Requant).

:- pred detect_switches_in_conj(module_info::in, vartypes::in, instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out, bool::in, bool::out) is det.

detect_switches_in_conj(_, _, _, [], [], !Requant).
detect_switches_in_conj(ModuleInfo, VarTypes, InstMap0,
        [Goal0 | Goals0], [Goal | Goals], !Requant) :-
    detect_switches_in_goal_1(ModuleInfo, VarTypes, InstMap0, InstMap1,
        Goal0, Goal, !Requant),
    detect_switches_in_conj(ModuleInfo, VarTypes, InstMap1, Goals0, Goals,
        !Requant).

%-----------------------------------------------------------------------------%

    % partition_disj(Goals, Var, GoalInfo, VarTypes, ModuleInfo, Left, Cases):
    %
    % Attempts to partition the disjunction `Goals' into a switch on `Var'.
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
    list(hlds_goal)::out, sorted_case_list::out, bool::in, bool::out)
    is semidet.

partition_disj(Goals0, Var, GoalInfo, Left, CasesList, !Requant) :-
    map.init(Cases0),
    partition_disj_trial(Goals0, Var, [], Left1, Cases0, Cases1),
    map.to_assoc_list(Cases1, CasesAssocList1),
    (
        Left1 = [],
        CasesAssocList1 = [_ | _], % There must be at least one case.
        Left = Left1,
        fix_case_list(CasesAssocList1, GoalInfo, CasesList)
    ;
        Left1 = [_ | _],
        % We don't insist on CasesAssocList1 not being empty, to allow for
        % switches in which *all* cases contain subsidiary disjunctions.
        ( expand_sub_disjs(Var, Left1, Cases1, Cases) ->
            Left = [],
            map.to_assoc_list(Cases, CasesAssocList),
            CasesAssocList = [_ | _], % There must be at least one case.
            fix_case_list(CasesAssocList, GoalInfo, CasesList),
            !:Requant = yes
        ;
            Left = Left1,
            fix_case_list(CasesAssocList1, GoalInfo, CasesList)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred expand_sub_disjs(prog_var::in, list(hlds_goal)::in,
    cases::in, cases::out) is semidet.

expand_sub_disjs(_Var, [], !Cases).
expand_sub_disjs(Var, [LeftGoal | LeftGoals], !Cases) :-
    expand_sub_disj(Var, LeftGoal, !Cases),
    expand_sub_disjs(Var, LeftGoals, !Cases).

:- pred expand_sub_disj(prog_var::in, hlds_goal::in, cases::in, cases::out)
    is semidet.

expand_sub_disj(Var, Goal, !Cases) :-
    Goal = GoalExpr - GoalInfo,
    ( GoalExpr = conj(plain_conj, SubGoals) ->
        expand_sub_disj_process_conj(Var, SubGoals, [], GoalInfo, !Cases)
    ; GoalExpr = disj(_) ->
        expand_sub_disj_process_conj(Var, [Goal], [], GoalInfo, !Cases)
    ;
        fail
    ).

:- pred expand_sub_disj_process_conj(prog_var::in, list(hlds_goal)::in,
    list(hlds_goal)::in, hlds_goal_info::in, cases::in, cases::out) is semidet.

expand_sub_disj_process_conj(Var, ConjGoals, !.RevUnifies, GoalInfo,
        !Cases) :-
    (
        ConjGoals = [],
        fail
    ;
        ConjGoals = [FirstGoal | RestGoals],
        FirstGoal = FirstGoalExpr - _,
        ( FirstGoalExpr = unify(_, _, _, _, _) ->
            !:RevUnifies = [FirstGoal | !.RevUnifies],
            expand_sub_disj_process_conj(Var, RestGoals, !.RevUnifies,
                GoalInfo, !Cases)
        ; FirstGoalExpr = disj(Disjuncts) ->
            Disjuncts = [_ | _],
            list.reverse(!.RevUnifies, Unifies),
            list.map(
                create_expanded_conjunction(Unifies, RestGoals, GoalInfo),
                Disjuncts, ExpandedConjunctions),
            partition_disj_trial(ExpandedConjunctions, Var, [], Left, !Cases),
            Left = []
        ;
            fail
        )
    ).

:- pred create_expanded_conjunction(list(hlds_goal)::in, list(hlds_goal)::in,
    hlds_goal_info::in, hlds_goal::in, hlds_goal::out) is det.

create_expanded_conjunction(Unifies, RestGoals, GoalInfo, Disjunct, Goal) :-
    ( Disjunct = conj(plain_conj, DisjunctGoals) - _ ->
        Conjuncts = Unifies ++ DisjunctGoals ++ RestGoals
    ;
        Conjuncts = Unifies ++ [Disjunct] ++ RestGoals
    ),
    Goal = conj(plain_conj, Conjuncts) - GoalInfo.

%-----------------------------------------------------------------------------%

:- pred partition_disj_trial(list(hlds_goal)::in, prog_var::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    cases::in, cases::out) is det.

partition_disj_trial([], _Var, !Left, !Cases).
partition_disj_trial([Goal0 | Goals], Var, !Left, !Cases) :-
    find_bind_var(Var, find_bind_var_for_switch_in_deconstruct, Goal0, Goal,
        no, MaybeFunctor, unit, _, _),
    (
        MaybeFunctor = yes(Functor),
        ( map.search(!.Cases, Functor, DisjList0) ->
            DisjList = [Goal | DisjList0],
            map.det_update(!.Cases, Functor, DisjList, !:Cases)
        ;
            DisjList = [Goal],
            map.det_insert(!.Cases, Functor, DisjList, !:Cases)
        )
    ;
        MaybeFunctor = no,
        !:Left = [Goal0 | !.Left]
    ),
    partition_disj_trial(Goals, Var, !Left, !Cases).

:- pred find_bind_var_for_switch_in_deconstruct(prog_var::in, hlds_goal::in,
    list(hlds_goal)::out, maybe(cons_id)::in, maybe(cons_id)::out,
    unit::in, unit::out) is det.

find_bind_var_for_switch_in_deconstruct(_UnifyVar, Goal0, Goals,
        _Result0, Result, _, unit) :-
    (
        Goal0 = GoalExpr0 - GoalInfo,
        UnifyInfo0 = GoalExpr0 ^ unify_kind,
        UnifyInfo0 = deconstruct(_, Functor, _, _, _, _)
    ->
        Result = yes(Functor),
        % The deconstruction unification now becomes deterministic, since
        % the test will get carried out in the switch.
        UnifyInfo = UnifyInfo0 ^ deconstruct_can_fail := cannot_fail,
        GoalExpr = GoalExpr0 ^ unify_kind := UnifyInfo,
        Goal = GoalExpr - GoalInfo,
        Goals = [Goal]
    ;
        unexpected(this_file, "find_bind_var_for_switch_in_deconstruct")
    ).

%-----------------------------------------------------------------------------%

find_bind_var(Var, ProcessUnify, !Goal, !Result, !Info, FoundDeconstruct) :-
    map.init(Subst),
    find_bind_var_2(Var, ProcessUnify, !Goal, Subst, _, !Result, !Info,
        DeconstructSearch),
    (
        DeconstructSearch = before_deconstruct,
        FoundDeconstruct = no
    ;
        DeconstructSearch = found_deconstruct,
        FoundDeconstruct = yes
    ;
        DeconstructSearch = given_up_search,
        FoundDeconstruct = no
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
    Goal0 = GoalExpr0 - GoalInfo,
    (
        GoalExpr0 = scope(Reason, SubGoal0)
    ->
        find_bind_var_2(Var, ProcessUnify, SubGoal0, SubGoal, !Subst,
            !Result, !Info, FoundDeconstruct),
        Goal = scope(Reason, SubGoal) - GoalInfo
    ;
        GoalExpr0 = conj(ConjType, SubGoals0),
        ConjType = plain_conj
    ->
        (
            SubGoals0 = [],
            Goal = Goal0,
            FoundDeconstruct = before_deconstruct
        ;
            SubGoals0 = [_ | _],
            conj_find_bind_var(Var, ProcessUnify, SubGoals0, SubGoals,
                !Subst, !Result, !Info, FoundDeconstruct),
            Goal = conj(ConjType, SubGoals) - GoalInfo
        )
    ;
        GoalExpr0 = unify(LHS, RHS, _, UnifyInfo0, _)
    ->
        (
            % Check whether the unification is a deconstruction unification
            % on either Var or on a variable aliased to Var.
            UnifyInfo0 = deconstruct(UnifyVar, _, _, _, _, _),
            term.apply_rec_substitution(term.variable(Var),
                !.Subst, term.variable(SubstVar)),
            term.apply_rec_substitution(term.variable(UnifyVar),
                !.Subst, term.variable(SubstUnifyVar)),
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
        Goal = Goal0,
        ( goal_info_has_feature(GoalInfo, from_head) ->
            FoundDeconstruct = before_deconstruct
        ;
            FoundDeconstruct = given_up_search
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
    ( FoundDeconstruct1 = before_deconstruct ->
        conj_find_bind_var(Var, ProcessUnify, Goals0, Goals,
            !Subst, !Result, !Info, FoundDeconstruct)
    ;
        FoundDeconstruct = FoundDeconstruct1,
        Goals = Goals0
    ).

%-----------------------------------------------------------------------------%

:- pred cases_to_switch(sorted_case_list::in, prog_var::in, vartypes::in,
    hlds_goal_info::in, instmap::in, module_info::in, hlds_goal_expr::out,
    bool::in, bool::out) is det.

cases_to_switch(CasesList, Var, VarTypes, _GoalInfo, InstMap, ModuleInfo,
        Goal, !Requant) :-
    instmap.lookup_var(InstMap, Var, VarInst),
    ( inst_is_bound_to_functors(ModuleInfo, VarInst, Functors) ->
        functors_to_cons_ids(Functors, ConsIds0),
        list.sort(ConsIds0, ConsIds),
        delete_unreachable_cases(CasesList, ConsIds, CasesList1),
        ( list.same_length(Functors, CasesList1) ->
            CanFail = cannot_fail
        ;
            CanFail = can_fail
        )
    ;
        map.lookup(VarTypes, Var, Type),
        CasesList1 = CasesList,
        ( switch_covers_all_cases(ModuleInfo, Type, CasesList1) ->
            CanFail = cannot_fail
        ;
            CanFail = can_fail
        )
    ),
    detect_switches_in_cases(ModuleInfo, VarTypes, InstMap,
        CasesList1, Cases, !Requant),

    % We turn switches with no arms into fail, since this avoids having
    % the code generator flush the control variable of the switch.
    % We can't easily eliminate switches with one arm, since the
    % code of the arm will have the unification between the variable
    % and the function symbol as det. The gain would be minimal to
    % nonexistent anyway.
    (
        Cases = [],
        Goal = disj([])
    ;
        Cases = [_ | _],
        Goal = switch(Var, CanFail, Cases)
    ).

    % Check whether a switch handles all the possible constants/functors
    % for the type.
    %
:- pred switch_covers_all_cases(module_info::in, mer_type::in,
    sorted_case_list::in) is semidet.

switch_covers_all_cases(ModuleInfo, Type, CasesList) :-
    type_util.switch_type_num_functors(ModuleInfo, Type, NumFunctors),
    list.length(CasesList, NumCases),
    NumCases = NumFunctors.

    % Convert the assoc_list(cons_id, list(hlds_goal)) back into a plain
    % list(case).
    %
:- pred fix_case_list(assoc_list(cons_id, list(hlds_goal))::in,
    hlds_goal_info::in, list(case)::out) is det.

fix_case_list([], _, []).
fix_case_list([Functor - DisjList0 | Cases0], GoalInfo,
        [case(Functor, Goal) | Cases]) :-
    % We need to put the list back the right way around.
    list.reverse(DisjList0, DisjList),
    disj_list_to_goal(DisjList, GoalInfo, Goal),
    fix_case_list(Cases0, GoalInfo, Cases).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "switch_detection.m".

%-----------------------------------------------------------------------------%
