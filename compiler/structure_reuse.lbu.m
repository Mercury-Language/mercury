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
:- import_module parse_tree.prog_type.

:- import_module list.
:- import_module pair.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

backward_use_information(_ModuleInfo, !ProcInfo):- 
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes), 

    % Before the first goal, the set of variables in LBU is empty.
    LBU0 = set.init,
    backward_use_in_goal(VarTypes, Goal0, Goal, LBU0, _LBU),

    proc_info_set_goal(Goal, !ProcInfo).

:- pred backward_use_in_goal(vartypes::in, hlds_goal::in, hlds_goal::out, 
    set(prog_var)::in, set(prog_var)::out) is det.

backward_use_in_goal(VarTypes, !TopGoal, !LBU) :-
    !.TopGoal = Expr0 - Info0,

    % Add resume_vars to the LBU-set.
    set.union(get_backtrack_vars(VarTypes, Info0), !LBU), 

    backward_use_in_goal_2(VarTypes, Info0, Expr0, Expr, !LBU),

    goal_info_set_lbu(!.LBU, Info0, Info), 
    !:TopGoal = Expr - Info.    

:- pred backward_use_in_goal_2(vartypes::in, hlds_goal_info::in, 
    hlds_goal_expr::in, hlds_goal_expr::out, set(prog_var)::in, 
    set(prog_var)::out) is det.

backward_use_in_goal_2(VarTypes, Info0, !Expr, !LBU) :- 
    % Handle each goal type separately:
    (
        !.Expr = unify(_, _, _, _, _)
    ;
        !.Expr = plain_call(_,_, _, _, _, _),
        goal_info_get_determinism(Info0, Det),
        (
            detism_allows_multiple_solns(Det)
        ->
            % Implementation of Instantiation 2 from Nancy's PhD.
            % In this instantation, a non-deterministic procedure
            % call only adds its LFU-variables to the current set
            % of lbu-variables. Cf. PhD Nancy Mazur. 

            goal_info_get_pre_births(Info0, PreBirths),
            goal_info_get_post_births(Info0, PostBirths),
            !:LBU = set.union_list([goal_info_get_lfu(Info0),
                remove_typeinfo_vars_from_set(VarTypes, PreBirths), 
                remove_typeinfo_vars_from_set(VarTypes, PostBirths), !.LBU])
        ;
            true
        )
    ;
        !.Expr = generic_call(_, _, _, _)
    ;
        % XXX Can they be nondet? If so, LFU variables need to be added
        % to !LBU.
        !.Expr = call_foreign_proc(_, _, _, _, _, _, _)
    ; 
        !.Expr = conj(ConjType, Goals0),
        backward_use_in_conj(VarTypes, Goals0, Goals, !LBU),
        !:Expr = conj(ConjType, Goals)
    ;
        !.Expr = disj(Goals0),
        backward_use_in_disj(VarTypes, Goals0, Goals, !LBU),
        !:Expr = disj(Goals)
    ;
        !.Expr = switch(A, B, Cases0),
        backward_use_in_cases(VarTypes, Cases0, Cases, !LBU),
        !:Expr = switch(A, B, Cases)
    ;
        !.Expr = negation(Goal0),
        % handled as: if(Goal0) then fail else true
        LBU0 = !.LBU, 
        backward_use_in_goal(VarTypes, Goal0, Goal, !.LBU, _),
        % A not does not introduce any choice-points! Hence the
        % not itself is deterministic, and no new variables in LBU
        % are introduced into the resulting LBU-set. 
        !:LBU = LBU0,
        !:Expr = negation(Goal)
    ;
        !.Expr = scope(Reason, SomeGoal0),
        backward_use_in_goal(VarTypes, SomeGoal0, SomeGoal, !LBU),
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
        backward_use_in_goal(VarTypes, Cond0, Cond, LBU0, _),

            % Annotate Then-goal.
            % When annotating the then-part, the lbu used for it should not
            % contain the resume-vars due to the else part.     
            % trick: to calculate inital LBU for the Then-goal, we set the
            % resume-point of the condition to no_resume_point.
        Cond0 = CondGoal0 - CondInfo0,
        goal_info_set_resume_point(no_resume_point, CondInfo0, InfoTmp),
        CondTmp = CondGoal0 - InfoTmp, 
        backward_use_in_goal(VarTypes, CondTmp, _, LBU0, LBU0T),
        backward_use_in_goal(VarTypes, Then0, Then, LBU0T, LBUT), 

            % Annotate Else-goal.
        backward_use_in_goal(VarTypes, Else0, Else, LBU0, LBUE), 
        set.union(LBUT, LBUE, !:LBU),
        !:Expr = if_then_else(Vars, Cond, Then, Else)
    ;
        !.Expr = shorthand(_), 
        unexpected(this_file, "backward_use_in_goal_2: shorthand goal.")
    ).

:- func get_backtrack_vars(vartypes, hlds_goal_info) = set(prog_var).

get_backtrack_vars(VarTypes, Info) = Vars :-
    goal_info_get_resume_point(Info, ResPoint), 
    (
        ResPoint = resume_point(ResVars, _),
        Vars = remove_typeinfo_vars_from_set(VarTypes, ResVars)
    ;
        ResPoint = no_resume_point,
        Vars = set.init
    ). 

:- pred detism_allows_multiple_solns(prog_data__determinism::in) is semidet.

detism_allows_multiple_solns(detism_non).
detism_allows_multiple_solns(detism_multi).
detism_allows_multiple_solns(detism_cc_non).
detism_allows_multiple_solns(detism_cc_multi).

:- pred backward_use_in_conj(vartypes::in, list(hlds_goal)::in, 
    list(hlds_goal)::out, set(prog_var)::in, set(prog_var)::out) is det.

backward_use_in_conj(VarTypes, !Goals, !LBU) :- 
    list.map_foldl(backward_use_in_goal(VarTypes), !Goals, !LBU). 

:- pred backward_use_in_cases(vartypes::in, list(case)::in, list(case)::out, 
    set(prog_var)::in, set(prog_var)::out) is det.

backward_use_in_cases(VarTypes, !Cases, !LBU) :- 
    % Every case is analysed with the same initial set of LBU-vars.
    LBU0 = !.LBU, 
    list.map_foldl(backward_use_in_case(LBU0, VarTypes), !Cases, !LBU).
 
:- pred backward_use_in_case(set(prog_var)::in, vartypes::in, case::in, 
    case::out, set(prog_var)::in, set(prog_var)::out) is det.

backward_use_in_case(LBU0, VarTypes, !Case, !LBU):- 
    !.Case = case(Cons, Goal0), 
    backward_use_in_goal(VarTypes, Goal0, Goal, LBU0, NewLBU),
    !:Case = case(Cons, Goal), 
    set.union(NewLBU, !LBU).

:- pred backward_use_in_disj(vartypes::in, list(hlds_goal)::in, 
    list(hlds_goal)::out, set(prog_var)::in, set(prog_var)::out) is det.

backward_use_in_disj(VarTypes, !Goals, !LBU) :- 
    % Every disj-goal is analysed with the same initial set of LBU-vars.
    LBU0 = !.LBU, 
    list.map_foldl(backward_use_in_disj_goal(LBU0, VarTypes), !Goals, !LBU).

:- pred backward_use_in_disj_goal(set(prog_var)::in, vartypes::in, 
    hlds_goal::in, hlds_goal::out, set(prog_var)::in,
    set(prog_var)::out) is det.

backward_use_in_disj_goal(LBU0, VarTypes, !Goal, !LBU) :- 
    backward_use_in_goal(VarTypes, !Goal, LBU0, NewLBU), 
    set.union(NewLBU, !LBU).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "structure_reuse.lbu.m".

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.lbu.
%-----------------------------------------------------------------------------%
