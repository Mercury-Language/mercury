%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
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
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.lbu.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- pred backward_use_information(module_info::in,
    proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation. 

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module pair.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

backward_use_information(ModuleInfo, !ProcInfo):- 
    proc_info_get_goal(!.ProcInfo, Goal0),

    % Before the first goal, the set of variables in LBU is empty.
    LBU0 = set.init,
    backward_use_in_goal(ModuleInfo, !.ProcInfo, Goal0, Goal, LBU0, _LBU),

    proc_info_set_goal(Goal, !ProcInfo).

:- pred backward_use_in_goal(module_info::in, proc_info::in, 
    hlds_goal::in, hlds_goal::out, set(prog_var)::in, set(prog_var)::out)
    is det.

backward_use_in_goal(ModuleInfo, ProcInfo, !TopGoal, !LBU) :-
    !.TopGoal = Expr0 - Info0,

    % Add resume_vars to the LBU-set.
    set.union(get_backtrack_vars(Info0), !LBU), 

    backward_use_in_goal_2(ModuleInfo, ProcInfo, Info0, Expr0, Expr, !LBU),

    goal_info_set_lbu(!.LBU, Info0, Info), 
    !:TopGoal = Expr - Info.    

:- pred backward_use_in_goal_2(module_info::in, proc_info::in, 
    hlds_goal_info::in, hlds_goal_expr::in, hlds_goal_expr::out, 
    set(prog_var)::in, set(prog_var)::out) is det.

backward_use_in_goal_2(ModuleInfo, ProcInfo, Info0, !Expr, !LBU) :- 
    % Handle each goal type separately:
    (
        !.Expr = unify(_, _, _, _, _)
    ;
        !.Expr = call(_,_, _, _, _, _),
        goal_info_get_determinism(Info0, Det),
        (
            detism_allows_multiple_solns(Det)
        ->
            % Implementation of Instantiation 2 from Nancy's Phd.
            % In this instantation, a non-deterministic procedure
            % call only adds its LFU-variables to the current set
            % of lbu-variables. Cf. Phd Nancy Mazur. 

            goal_info_get_pre_births(Info0, PreBirths),
            goal_info_get_post_births(Info0, PostBirths),
            !:LBU = set.union_list([goal_info_get_lfu(Info0),
                 PreBirths, PostBirths, !.LBU])
        ;
            true
        )
    ;
        !.Expr = generic_call(_, _, _, _)
    ;
        % XXX Can they be nondet? If so, LFU variables need to be added
        % to !LBU.
        !.Expr = foreign_proc(_, _, _, _, _, _)
    ; 
        !.Expr = conj(ConjType, Goals0),
        backward_use_in_conj(ModuleInfo, ProcInfo, 
                Goals0, Goals, !LBU),
        !:Expr = conj(ConjType, Goals)
    ;
        !.Expr = disj(Goals0),
        backward_use_in_disj(ModuleInfo, ProcInfo, Goals0, Goals, !LBU),
        !:Expr = disj(Goals)
    ;
        !.Expr = switch(A, B, Cases0),
        backward_use_in_cases(ModuleInfo, ProcInfo, Cases0, Cases, !LBU),
        !:Expr = switch(A, B, Cases)
    ;
        !.Expr = not(Goal0),
        % handled as: if(Goal0) then fail else true
        LBU0 = !.LBU, 
        backward_use_in_goal(ModuleInfo, ProcInfo, Goal0, Goal, !.LBU, _),
        % A not does not introduce any choice-points! Hence the
        % not itself is deterministic, and no new variables in LBU
        % are introduced into the resulting LBU-set. 
        !:LBU = LBU0,
        !:Expr = not(Goal)
    ;
        !.Expr = scope(Reason, SomeGoal0),
        backward_use_in_goal(ModuleInfo, ProcInfo, SomeGoal0, SomeGoal, !LBU),
        !:Expr = scope(Reason, SomeGoal)
    ;
        % XXX The implementation for if-then-else is different from the theory
        % in the thesis.  We can obtain more precision when the Condition-goal
        % is deterministic, which means that the Then-goal can be analysed with
        % the initial LBU-set (instead of the set obtained after the analysis
        % of the Condition-goal). 
        !.Expr = if_then_else(Vars, Cond0, Then0, Else0),
        LBU0 = !.LBU, 

            % Annotate Cond-goal.
        backward_use_in_goal(ModuleInfo, ProcInfo, Cond0, Cond, LBU0, _),

            % Annotate Then-goal.
            % When annotating the then-part, the lbu used for it should not
            % contain the resume-vars due to the else part.     
            % trick: to calculate inital LBU for the Then-goal, we set the
            % resume-point of the condition to no_resume_point.
        Cond0 = CondGoal0 - CondInfo0,
        goal_info_set_resume_point(no_resume_point, CondInfo0, InfoTmp),
        CondTmp = CondGoal0 - InfoTmp, 
        backward_use_in_goal(ModuleInfo, ProcInfo, CondTmp, _, LBU0, LBU0T),
        backward_use_in_goal(ModuleInfo, ProcInfo, Then0, Then, LBU0T, LBUT), 

            % Annotate Else-goal.
        backward_use_in_goal(ModuleInfo, ProcInfo, Else0, Else, 
                LBU0, LBUE), 
        set.union(LBUT, LBUE, !:LBU),
        !:Expr = if_then_else(Vars, Cond, Then, Else)
    ;
        !.Expr = shorthand(_), 
        unexpected(this_file, "backward_use_in_goal_2: shorthand goal.")
    ).

:- func get_backtrack_vars(hlds_goal_info) = set(prog_var).

get_backtrack_vars(Info) = Vars :-
    goal_info_get_resume_point(Info, ResPoint), 
    (
        ResPoint = resume_point(ResVars, _),
        Vars = ResVars
    ;
        ResPoint = no_resume_point,
        Vars = set.init
    ). 

:- pred detism_allows_multiple_solns(prog_data__determinism::in) is semidet.

detism_allows_multiple_solns(nondet).
detism_allows_multiple_solns(multidet).
detism_allows_multiple_solns(cc_nondet).
detism_allows_multiple_solns(cc_multidet).

:- pred backward_use_in_conj(module_info::in, proc_info::in, 
    hlds_goals::in, hlds_goals::out, set(prog_var)::in, set(prog_var)::out)
    is det.

backward_use_in_conj(ModuleInfo, ProcInfo, !Goals, !LBU) :- 
    list.map_foldl(backward_use_in_goal(ModuleInfo, ProcInfo), !Goals, !LBU). 

:- pred backward_use_in_cases(module_info::in, proc_info::in, 
    list(case)::in, list(case)::out, set(prog_var)::in, set(prog_var)::out) 
    is det.

backward_use_in_cases(ModuleInfo, ProcInfo, !Cases, !LBU) :- 
    % Every case is analysed with the same initial set of LBU-vars.
    LBU0 = !.LBU, 
    list.map_foldl(backward_use_in_case(LBU0, ModuleInfo, ProcInfo),
        !Cases, !LBU).
 
:- pred backward_use_in_case(set(prog_var)::in, module_info::in,
    proc_info::in, case::in, case::out, set(prog_var)::in, set(prog_var)::out)
    is det.

backward_use_in_case(LBU0, ModuleInfo, ProcInfo, !Case, !LBU):- 
    !.Case = case(Cons, Goal0), 
    backward_use_in_goal(ModuleInfo, ProcInfo, Goal0, Goal, LBU0, NewLBU),
    !:Case = case(Cons, Goal), 
    set.union(NewLBU, !LBU).

:- pred backward_use_in_disj(module_info::in, proc_info::in, 
    hlds_goals::in, hlds_goals::out, set(prog_var)::in, set(prog_var)::out)
    is det.

backward_use_in_disj(ModuleInfo, ProcInfo, !Goals, !LBU) :- 
    % Every disj-goal is analysed with the same initial set of LBU-vars.
    LBU0 = !.LBU, 
    list.map_foldl(backward_use_in_disj_goal(LBU0, ModuleInfo, ProcInfo),
        !Goals, !LBU).

:- pred backward_use_in_disj_goal(set(prog_var)::in, module_info::in,
    proc_info::in, hlds_goal::in, hlds_goal::out, set(prog_var)::in,
    set(prog_var)::out) is det.

backward_use_in_disj_goal(LBU0, ModuleInfo, ProcInfo, !Goal, !LBU) :- 
    backward_use_in_goal(ModuleInfo, ProcInfo, !Goal, LBU0, NewLBU), 
    set.union(NewLBU, !LBU).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "structure_reuse.lbu.m".

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.lbu.
%-----------------------------------------------------------------------------%
