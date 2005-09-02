%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Switch detection - when a disjunction contains disjuncts that unify the
% same input variable with different function symbols, replace (part of)
% the disjunction with a switch.
%
% Main author: fjh.
%
%-----------------------------------------------------------------------------%

:- module check_hlds__switch_detection.

:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.

:- pred detect_switches(module_info::in, module_info::out,
    io::di, io::uo) is det.

:- pred detect_switches_in_proc(proc_id::in, pred_id::in,
    module_info::in, module_info::out) is det.

    % find_bind_var(Var, ProcessUnify, Goal0, Goals, Subst0, Subst,
    %   !Result, FoundDeconstruct):
    % Used by both switch_detection and cse_detection. Searches through
    % `Goal0' looking for the first deconstruction unification with `Var'
    % or an alias of `Var'. If a deconstruction unification of the
    % variable is found, `ProcessUnify' is called to handle it and
    % searching is stopped. If not, `Result' is set to `Result0'.
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

:- import_module check_hlds__det_util.
:- import_module check_hlds__inst_match.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__instmap.
:- import_module hlds__passes_aux.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_mode.

:- import_module assoc_list.
:- import_module char.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module term.

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
    map__lookup(PredTable, PredId, PredInfo),
    detect_switches_in_pred(PredId, PredInfo, !ModuleInfo, !IO),
    detect_switches_in_preds(PredIds, !ModuleInfo, !IO).

:- pred detect_switches_in_pred(pred_id::in, pred_info::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

detect_switches_in_pred(PredId, PredInfo0, !ModuleInfo, !IO) :-
    ProcIds = pred_info_non_imported_procids(PredInfo0),
    (
        ProcIds = [_ | _],
        write_pred_progress_message("% Detecting switches in ", PredId,
            !.ModuleInfo, !IO)
    ;
        ProcIds = []
    ),
    detect_switches_in_procs(ProcIds, PredId, !ModuleInfo).

:- pred detect_switches_in_procs(list(proc_id)::in, pred_id::in,
    module_info::in, module_info::out) is det.

detect_switches_in_procs([], _PredId, !ModuleInfo).
detect_switches_in_procs([ProcId | ProcIds], PredId, !ModuleInfo) :-
    detect_switches_in_proc(ProcId, PredId, !ModuleInfo),
    detect_switches_in_procs(ProcIds, PredId, !ModuleInfo).

detect_switches_in_proc(ProcId, PredId, !ModuleInfo) :-
    module_info_preds(!.ModuleInfo, PredTable0),
    map__lookup(PredTable0, PredId, PredInfo0),
    pred_info_procedures(PredInfo0, ProcTable0),
    map__lookup(ProcTable0, ProcId, ProcInfo0),

    % To process each ProcInfo, we get the goal, initialize the instmap
    % based on the modes of the head vars, and pass these to
    % `detect_switches_in_goal'.
    proc_info_goal(ProcInfo0, Goal0),
    proc_info_vartypes(ProcInfo0, VarTypes),
    proc_info_get_initial_instmap(ProcInfo0, !.ModuleInfo, InstMap0),
    detect_switches_in_goal(!.ModuleInfo, VarTypes, InstMap0, Goal0, Goal),

    proc_info_set_goal(Goal, ProcInfo0, ProcInfo),
    map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
    pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
    map__det_update(PredTable0, PredId, PredInfo, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo).

%-----------------------------------------------------------------------------%

    % Given a goal, and the instmap on entry to that goal,
    % replace disjunctions with switches whereever possible.
    %
:- pred detect_switches_in_goal(module_info::in, vartypes::in,
    instmap::in, hlds_goal::in, hlds_goal::out) is det.

detect_switches_in_goal(ModuleInfo, VarTypes, InstMap0, !Goal) :-
    detect_switches_in_goal_1(ModuleInfo, VarTypes, InstMap0, _InstMap,
        !Goal).

    % This version is the same as the above except that it returns the
    % resulting instmap on exit from the goal, which is computed by applying
    % the instmap delta specified in the goal's goalinfo.
    %
:- pred detect_switches_in_goal_1(module_info::in, vartypes::in,
    instmap::in, instmap::out, hlds_goal::in, hlds_goal::out) is det.

detect_switches_in_goal_1(ModuleInfo, VarTypes, !InstMap,
        Goal0 - GoalInfo, Goal - GoalInfo) :-
    detect_switches_in_goal_2(ModuleInfo, VarTypes, !.InstMap, GoalInfo,
        Goal0, Goal),
    update_instmap(Goal0 - GoalInfo, !InstMap).

    % Here we process each of the different sorts of goals.
    %
:- pred detect_switches_in_goal_2(module_info::in, vartypes::in, instmap::in,
    hlds_goal_info::in, hlds_goal_expr::in, hlds_goal_expr::out) is det.

detect_switches_in_goal_2(ModuleInfo, VarTypes, InstMap0, GoalInfo,
        disj(Goals0), Goal) :-
    (
        Goals0 = [],
        Goal = disj([])
    ;
        Goals0 = [_ | _],
        goal_info_get_nonlocals(GoalInfo, NonLocals),
        set__to_sorted_list(NonLocals, NonLocalsList),
        detect_switches_in_disj(NonLocalsList, Goals0, GoalInfo,
            InstMap0, VarTypes, NonLocalsList, ModuleInfo, [], Goal)
    ).

detect_switches_in_goal_2(ModuleInfo, VarTypes, InstMap0, _GoalInfo,
        conj(Goals0), conj(Goals)) :-
    detect_switches_in_conj(ModuleInfo, VarTypes, InstMap0, Goals0, Goals).

detect_switches_in_goal_2(ModuleInfo, VarTypes, InstMap0,
        _GoalInfo, par_conj(Goals0), par_conj(Goals)) :-
    detect_switches_in_par_conj(ModuleInfo, VarTypes, InstMap0,
        Goals0, Goals).

detect_switches_in_goal_2(ModuleInfo, VarTypes, InstMap0, _GoalInfo,
        not(Goal0), not(Goal)) :-
    detect_switches_in_goal(ModuleInfo, VarTypes, InstMap0, Goal0, Goal).

detect_switches_in_goal_2(ModuleInfo, VarTypes, InstMap0, _GoalInfo,
        if_then_else(Vars, Cond0, Then0, Else0),
        if_then_else(Vars, Cond, Then, Else)) :-
    detect_switches_in_goal_1(ModuleInfo, VarTypes, InstMap0, InstMap1,
        Cond0, Cond),
    detect_switches_in_goal(ModuleInfo, VarTypes, InstMap1, Then0, Then),
    detect_switches_in_goal(ModuleInfo, VarTypes, InstMap0, Else0, Else).

detect_switches_in_goal_2(ModuleInfo, VarTypes, InstMap0, _GoalInfo,
        scope(Reason, Goal0), scope(Reason, Goal)) :-
    detect_switches_in_goal(ModuleInfo, VarTypes, InstMap0, Goal0, Goal).

detect_switches_in_goal_2(_, _, _, _, Goal @ generic_call(_, _, _, _), Goal).

detect_switches_in_goal_2(_, _, _, _, Goal @ call(_, _, _, _, _, _), Goal).

detect_switches_in_goal_2(ModuleInfo, VarTypes, InstMap0, _GoalInfo,
        Goal0, Goal) :-
    Goal0 = unify(_, RHS0, _, _, _),
    ( RHS0 = lambda_goal(_, _, _, _, _, Vars, Modes, _, LambdaGoal0) ->
        % we need to insert the initial insts for the lambda
        % variables in the instmap before processing the lambda goal
        instmap__pre_lambda_update(ModuleInfo, Vars, Modes,
            InstMap0, InstMap1),
        detect_switches_in_goal(ModuleInfo, VarTypes, InstMap1,
            LambdaGoal0, LambdaGoal),
        RHS = RHS0 ^ rhs_lambda_goal := LambdaGoal,
        Goal = Goal0 ^ unify_rhs := RHS
    ;
        Goal = Goal0
    ).

detect_switches_in_goal_2(ModuleInfo, VarTypes, InstMap, _,
        switch(Var, CanFail, Cases0), switch(Var, CanFail, Cases)) :-
    detect_switches_in_cases(ModuleInfo, VarTypes, InstMap, Cases0, Cases).

detect_switches_in_goal_2(_, _, _, _, Goal @ foreign_proc(_, _, _, _, _, _),
        Goal).
detect_switches_in_goal_2(_, _, _, _, shorthand(_), _) :-
    % These should have been expanded out by now.
    unexpected(this_file, "detect_switches_in_goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

:- type cases == map(cons_id, list(hlds_goal)).

:- type sorted_case_list == list(case).
    % the sorted_case_list should always be sorted on cons_id -
    % `delete_unreachable_cases' relies on this.

:- type again ---> again(prog_var, list(hlds_goal), sorted_case_list).

    % This is the interesting bit - we've found a non-empty disjunction,
    % and we've got a list of the non-local variables of that disjunction.
    % Now for each non-local variable, we check whether there is a partition
    % of the disjuncts such that each group of disjunctions can only succeed
    % if the variable is bound to a different functor.
    %
:- pred detect_switches_in_disj(list(prog_var)::in, list(hlds_goal)::in,
    hlds_goal_info::in, instmap::in, vartypes::in,
    list(prog_var)::in, module_info::in, list(again)::in,
    hlds_goal_expr::out) is det.

detect_switches_in_disj([Var | Vars], Goals0, GoalInfo, InstMap,
        VarTypes, AllVars, ModuleInfo, Again0, Goal) :-
    % Can we do at least a partial switch on this variable?
    (
        instmap__lookup_var(InstMap, Var, VarInst0),
        inst_is_bound(ModuleInfo, VarInst0),
        partition_disj(Goals0, Var, GoalInfo, Left, CasesList)
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
                cases_to_switch(CasesList, Var, VarTypes,
                    GoalInfo, InstMap, ModuleInfo, Goal)
            ;
                detect_sub_switches_in_disj(ModuleInfo,
                    VarTypes, InstMap, Goals0, Goals),
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
                AllVars, ModuleInfo, Again1, Goal)
        )
    ;
        detect_switches_in_disj(Vars, Goals0, GoalInfo, InstMap,
            VarTypes, AllVars, ModuleInfo, Again0, Goal)
    ).
detect_switches_in_disj([], Goals0, GoalInfo, InstMap,
        VarTypes, AllVars, ModuleInfo, AgainList0, disj(Goals)) :-
    (
        AgainList0 = [],
        detect_sub_switches_in_disj(ModuleInfo, VarTypes, InstMap,
            Goals0, Goals)
    ;
        AgainList0 = [Again | AgainList1],
        select_best_switch(AgainList1, Again, BestAgain),
        BestAgain = again(Var, Left0, CasesList),
        cases_to_switch(CasesList, Var, VarTypes, GoalInfo, InstMap,
            ModuleInfo, SwitchGoal),
        detect_switches_in_disj(AllVars, Left0, GoalInfo, InstMap,
            VarTypes, AllVars, ModuleInfo, [], Left),
        goal_to_disj_list(Left - GoalInfo, LeftList),
        Goals = [SwitchGoal - GoalInfo | LeftList]
    ).

:- pred select_best_switch(list(again)::in, again::in, again::out) is det.

select_best_switch([], BestAgain, BestAgain).
select_best_switch([Again | AgainList], BestAgain0, BestAgain) :-
    (
        Again = again(_, _, CasesList),
        BestAgain0 = again(_, _, BestCasesList),
        list__length(CasesList, Length),
        list__length(BestCasesList, BestLength),
        Length < BestLength
    ->
        BestAgain1 = BestAgain0
    ;
        BestAgain1 = Again
    ),
    select_best_switch(AgainList, BestAgain1, BestAgain).

:- pred detect_sub_switches_in_disj(module_info::in, vartypes::in, instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

detect_sub_switches_in_disj(_ModuleInfo, _VarTypes, _InstMap, [], []).
detect_sub_switches_in_disj(ModuleInfo, VarTypes, InstMap,
        [Goal0 | Goals0], [Goal | Goals]) :-
    detect_switches_in_goal(ModuleInfo, VarTypes, InstMap, Goal0, Goal),
    detect_sub_switches_in_disj(ModuleInfo, VarTypes, InstMap,
        Goals0, Goals).

:- pred detect_switches_in_cases(module_info::in, vartypes::in, instmap::in,
    list(case)::in, list(case)::out) is det.

detect_switches_in_cases(_ModuleInfo, _VarTypes, _InstMap, [], []).
detect_switches_in_cases(ModuleInfo, VarTypes, InstMap,
        [Case0 | Cases0], [Case | Cases]) :-
    Case0 = case(Functor, Goal0),
    detect_switches_in_goal(ModuleInfo, VarTypes, InstMap, Goal0, Goal),
    Case = case(Functor, Goal),
    detect_switches_in_cases(ModuleInfo, VarTypes, InstMap, Cases0, Cases).

:- pred detect_switches_in_par_conj(module_info::in, vartypes::in, instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

detect_switches_in_par_conj(_ModuleInfo, _VarTypes, _InstMap, [], []).
detect_switches_in_par_conj(ModuleInfo, VarTypes, InstMap,
        [Goal0 | Goals0], [Goal | Goals]) :-
    detect_switches_in_goal(ModuleInfo, VarTypes, InstMap, Goal0, Goal),
    detect_switches_in_par_conj(ModuleInfo, VarTypes, InstMap,
        Goals0, Goals).

:- pred detect_switches_in_conj(module_info::in, vartypes::in, instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

detect_switches_in_conj(_ModuleInfo, _VarTypes, _InstMap, [], []).
detect_switches_in_conj(ModuleInfo, VarTypes, InstMap0,
        [Goal0 | Goals0], [Goal | Goals]) :-
    detect_switches_in_goal_1(ModuleInfo, VarTypes, InstMap0, InstMap1,
        Goal0, Goal),
    detect_switches_in_conj(ModuleInfo, VarTypes, InstMap1, Goals0, Goals).

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
    list(hlds_goal)::out, sorted_case_list::out) is semidet.

partition_disj(Goals0, Var, GoalInfo, Left, CasesList) :-
    map__init(Cases0),
    partition_disj_trial(Goals0, Var, [], Left, Cases0, Cases),
    map__to_assoc_list(Cases, CasesAssocList),
    CasesAssocList \= [], % there must be at least one case
    fix_case_list(CasesAssocList, GoalInfo, CasesList).

:- pred partition_disj_trial(list(hlds_goal)::in, prog_var::in,
    list(hlds_goal)::in, list(hlds_goal)::out, cases::in, cases::out) is det.

partition_disj_trial([], _Var, !Left, !Cases).
partition_disj_trial([Goal0 | Goals], Var, !Left, !Cases) :-
    find_bind_var(Var, find_bind_var_for_switch_in_deconstruct,
        Goal0, Goal, no, MaybeFunctor, unit, _, _),
    (
        MaybeFunctor = yes(Functor),
        ( map__search(!.Cases, Functor, DisjList0) ->
            DisjList = [Goal | DisjList0],
            map__det_update(!.Cases, Functor, DisjList, !:Cases)
        ;
            DisjList = [Goal],
            map__det_insert(!.Cases, Functor, DisjList, !:Cases)
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
    map__init(Subst),
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

find_bind_var_2(Var, ProcessUnify, Goal0 - GoalInfo, Goal, !Subst, !Result,
        !Info, FoundDeconstruct) :-
    ( Goal0 = scope(Reason, SubGoal0) ->
        find_bind_var_2(Var, ProcessUnify, SubGoal0, SubGoal, !Subst,
            !Result, !Info, FoundDeconstruct),
        Goal = scope(Reason, SubGoal) - GoalInfo
    ; Goal0 = conj(SubGoals0) ->
        (
            SubGoals0 = [],
            Goal = Goal0 - GoalInfo,
            FoundDeconstruct = before_deconstruct
        ;
            SubGoals0 = [_ | _],
            conj_find_bind_var(Var, ProcessUnify, SubGoals0, SubGoals,
                !Subst, !Result, !Info, FoundDeconstruct),
            Goal = conj(SubGoals) - GoalInfo
        )
    ; Goal0 = unify(LHS, RHS, _, UnifyInfo0, _) ->
        (
            % Check whether the unification is a deconstruction unification
            % on either Var or on a variable aliased to Var.
            UnifyInfo0 = deconstruct(UnifyVar, _, _, _, _, _),
            term__apply_rec_substitution(term__variable(Var),
                !.Subst, term__variable(Var1)),
            term__apply_rec_substitution(term__variable(UnifyVar),
                !.Subst, term__variable(UnifyVar1)),
            Var1 = UnifyVar1
        ->
            call(ProcessUnify, Var, Goal0 - GoalInfo, Goals, !Result, !Info),
            conj_list_to_goal(Goals, GoalInfo, Goal),
            FoundDeconstruct = found_deconstruct
        ;
            Goal = Goal0 - GoalInfo,
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
        Goal = Goal0 - GoalInfo,
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
    hlds_goal_info::in, instmap::in, module_info::in, hlds_goal_expr::out)
    is det.

cases_to_switch(CasesList, Var, VarTypes, _GoalInfo, InstMap, ModuleInfo,
        Goal) :-
    instmap__lookup_var(InstMap, Var, VarInst),
    ( inst_is_bound_to_functors(ModuleInfo, VarInst, Functors) ->
        functors_to_cons_ids(Functors, ConsIds0),
        list__sort(ConsIds0, ConsIds),
        delete_unreachable_cases(CasesList, ConsIds, CasesList1),
        ( list__same_length(Functors, CasesList1) ->
            CanFail = cannot_fail
        ;
            CanFail = can_fail
        )
    ;
        map__lookup(VarTypes, Var, Type),
        CasesList1 = CasesList,
        ( switch_covers_all_cases(ModuleInfo, Type, CasesList1) ->
            CanFail = cannot_fail
        ;
            CanFail = can_fail
        )
    ),
    detect_switches_in_cases(ModuleInfo, VarTypes, InstMap,
        CasesList1, Cases),

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
:- pred switch_covers_all_cases(module_info::in, (type)::in,
    sorted_case_list::in) is semidet.

switch_covers_all_cases(ModuleInfo, Type, CasesList) :-
    type_util__switch_type_num_functors(ModuleInfo, Type, NumFunctors),
    list__length(CasesList, NumCases),
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
    list__reverse(DisjList0, DisjList),
    disj_list_to_goal(DisjList, GoalInfo, Goal),
    fix_case_list(Cases0, GoalInfo, Cases).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "switch_detection.m".

%-----------------------------------------------------------------------------%
