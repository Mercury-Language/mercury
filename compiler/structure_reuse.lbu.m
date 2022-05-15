%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2008, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: structure_reuse.lbu.m.
% Main authors: nancy.
%
% Implementation of the process of annotating each program point within
% a procedure with local backward use information.
%
% Each program point (goal within a procedure definition) is annotated with a
% set of variables that are in Local Backward Use (LBU). A variable is said to
% be in LBU if it may be accessed upon backtracking.  This information is
% computed based on the backtrack-vars (i.e. the input variables of the
% alternative goals of a disjunction), and forward use information.
%
% The implementation is based on the theory detailed in Nancy Mazur's PhD
% ("Instantiation 2", cf. Section 7.4).
% XXX Note: slight variations as to the treatment of disjunctions,
% switches and if-then-elses.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.lbu.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- pred backward_use_information(module_info::in,
    proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module list.
:- import_module require.

%-----------------------------------------------------------------------------%

backward_use_information(ModuleInfo, !ProcInfo):-
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_var_table(ModuleInfo, !.ProcInfo, VarTable),

    % Before the first goal, the set of variables in LBU is empty.
    LBU0 = set_of_var.init,
    backward_use_in_goal(VarTable, Goal0, Goal, LBU0, _LBU),

    proc_info_set_goal(Goal, !ProcInfo).

:- pred backward_use_in_goal(var_table::in, hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out) is det.

backward_use_in_goal(VarTable, !TopGoal, !LBU) :-
    !.TopGoal = hlds_goal(Expr0, Info0),

    % Add resume_vars to the LBU-set.
    set_of_var.union(get_backtrack_vars(VarTable, Info0), !LBU),

    backward_use_in_goal_2(VarTable, Info0, Expr0, Expr, !LBU),

    goal_info_set_lbu(!.LBU, Info0, Info),
    !:TopGoal = hlds_goal(Expr, Info).

:- pred backward_use_in_goal_2(var_table::in, hlds_goal_info::in,
    hlds_goal_expr::in, hlds_goal_expr::out,
    set_of_progvar::in, set_of_progvar::out) is det.

backward_use_in_goal_2(VarTable, Info0, !Expr, !LBU) :-
    % Handle each goal type separately:
    (
        !.Expr = unify(_, _, _, _, _)
    ;
        !.Expr = plain_call(_,_, _, _, _, _),
        Det = goal_info_get_determinism(Info0),
        ( if detism_allows_multiple_solns(Det) then
            % Implementation of Instantiation 2 from Nancy's PhD.
            % In this instantation, a non-deterministic procedure
            % call only adds its LFU-variables to the current set
            % of lbu-variables. Cf. PhD Nancy Mazur.

            goal_info_get_pre_births(Info0, PreBirths),
            goal_info_get_post_births(Info0, PostBirths),
            !:LBU = set_of_var.union_list([goal_info_get_lfu(Info0),
                remove_typeinfo_vars_from_set_of_var(VarTable, PreBirths),
                remove_typeinfo_vars_from_set_of_var(VarTable, PostBirths),
                !.LBU])
        else
            true
        )
    ;
        !.Expr = generic_call(_, _, _, _, _)
    ;
        % XXX Can they be nondet? If so, LFU variables need to be added
        % to !LBU.
        !.Expr = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        !.Expr = conj(ConjType, Goals0),
        backward_use_in_conj(VarTable, Goals0, Goals, !LBU),
        !:Expr = conj(ConjType, Goals)
    ;
        !.Expr = disj(Goals0),
        backward_use_in_disj(VarTable, Goals0, Goals, !LBU),
        !:Expr = disj(Goals)
    ;
        !.Expr = switch(A, B, Cases0),
        backward_use_in_cases(VarTable, Cases0, Cases, !LBU),
        !:Expr = switch(A, B, Cases)
    ;
        !.Expr = negation(SubGoal0),
        % handled as: if SubGoal0 then fail else true
        LBU0 = !.LBU,
        backward_use_in_goal(VarTable, SubGoal0, SubGoal, !.LBU, _),
        % A negation does not introduce any choice-points! Hence the
        % negation itself is deterministic, and no new variables in LBU
        % are introduced into the resulting LBU-set.
        !:LBU = LBU0,
        !:Expr = negation(SubGoal)
    ;
        !.Expr = scope(Reason, SubGoal0),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            SubGoal = SubGoal0
        else
            % XXX We could treat from_ground_term_deconstruct specially
            % as well.
            backward_use_in_goal(VarTable, SubGoal0, SubGoal, !LBU)
        ),
        !:Expr = scope(Reason, SubGoal)
    ;
        % XXX The implementation for if-then-else is different from the theory
        % in the thesis.  We can obtain more precision when the Condition-goal
        % is deterministic, which means that the Then-goal can be analysed with
        % the initial LBU-set (instead of the set obtained after the analysis
        % of the Condition-goal).
        !.Expr = if_then_else(Vars, Cond0, Then0, Else0),
        LBU0 = !.LBU,

        % Annotate Cond-goal.
        backward_use_in_goal(VarTable, Cond0, Cond, LBU0, _),

        % Annotate Then-goal.
        % When annotating the then-part, the lbu used for it should not
        % contain the resume-vars due to the else part.
        % trick: to calculate inital LBU for the Then-goal, we set the
        % resume-point of the condition to no_resume_point.
        Cond0 = hlds_goal(CondGoal0, CondInfo0),
        goal_info_set_resume_point(no_resume_point, CondInfo0, InfoTmp),
        CondTmp = hlds_goal(CondGoal0, InfoTmp),
        backward_use_in_goal(VarTable, CondTmp, _, LBU0, LBU0T),
        backward_use_in_goal(VarTable, Then0, Then, LBU0T, LBUT),

        % Annotate Else-goal.
        backward_use_in_goal(VarTable, Else0, Else, LBU0, LBUE),
        set_of_var.union(LBUT, LBUE, !:LBU),
        !:Expr = if_then_else(Vars, Cond, Then, Else)
    ;
        !.Expr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

:- func get_backtrack_vars(var_table, hlds_goal_info) = set_of_progvar.

get_backtrack_vars(VarTable, Info) = Vars :-
    goal_info_get_resume_point(Info, ResPoint),
    (
        ResPoint = resume_point(ResVars, _),
        Vars = remove_typeinfo_vars_from_set_of_var(VarTable, ResVars)
    ;
        ResPoint = no_resume_point,
        Vars = set_of_var.init
    ).

:- pred detism_allows_multiple_solns(prog_data__determinism::in) is semidet.

detism_allows_multiple_solns(detism_non).
detism_allows_multiple_solns(detism_multi).
detism_allows_multiple_solns(detism_cc_non).
detism_allows_multiple_solns(detism_cc_multi).

:- pred backward_use_in_conj(var_table::in, list(hlds_goal)::in,
    list(hlds_goal)::out, set_of_progvar::in, set_of_progvar::out) is det.

backward_use_in_conj(VarTable, !Goals, !LBU) :-
    list.map_foldl(backward_use_in_goal(VarTable), !Goals, !LBU).

:- pred backward_use_in_cases(var_table::in, list(case)::in, list(case)::out,
    set_of_progvar::in, set_of_progvar::out) is det.

backward_use_in_cases(VarTable, !Cases, !LBU) :-
    % Every case is analysed with the same initial set of LBU-vars.
    LBU0 = !.LBU,
    list.map_foldl(backward_use_in_case(LBU0, VarTable), !Cases, !LBU).

:- pred backward_use_in_case(set_of_progvar::in, var_table::in,
    case::in, case::out, set_of_progvar::in, set_of_progvar::out) is det.

backward_use_in_case(LBU0, VarTable, !Case, !LBU):-
    !.Case = case(MainConsId, OtherConsIds, Goal0),
    backward_use_in_goal(VarTable, Goal0, Goal, LBU0, NewLBU),
    !:Case = case(MainConsId, OtherConsIds, Goal),
    set_of_var.union(NewLBU, !LBU).

:- pred backward_use_in_disj(var_table::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out) is det.

backward_use_in_disj(VarTable, !Goals, !LBU) :-
    % Every disj-goal is analysed with the same initial set of LBU-vars.
    LBU0 = !.LBU,
    list.map_foldl(backward_use_in_disj_goal(LBU0, VarTable), !Goals, !LBU).

:- pred backward_use_in_disj_goal(set_of_progvar::in, var_table::in,
    hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out) is det.

backward_use_in_disj_goal(LBU0, VarTable, !Goal, !LBU) :-
    backward_use_in_goal(VarTable, !Goal, LBU0, NewLBU),
    set_of_var.union(NewLBU, !LBU).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.lbu.
%-----------------------------------------------------------------------------%
