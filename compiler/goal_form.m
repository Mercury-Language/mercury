%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: goal_form.m.
% Main authors: conway, zs.
%
% A module that provides functions that check whether goals fulfill particular
% criteria.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module hlds.goal_form.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%

    % Is the input goal a conjunction of unifications that constructs every
    % variable in the given set? A from_ground_term_construct scope counts
    % as a unification.
    %
:- pred goal_is_conj_of_unify(set_of_progvar::in, hlds_goal::in) is semidet.

    % Run goal_is_conj_of_unify on each goal in the list.
    %
:- pred all_disjuncts_are_conj_of_unify(set_of_progvar::in,
    list(hlds_goal)::in) is semidet.

%-----------------------------------------------------------------------------%

    % An indication of whether a goal can loop forever.
    %
:- type goal_loop_status
    --->    can_loop
    ;       cannot_loop.

    % An indication of whether a goal can throw an exception.
    %
:- type goal_throw_status
    --->    can_throw
    ;       cannot_throw.

    % An indication of whether a goal can loop forever or throw an exception.
    %
:- type goal_loop_or_throw_status
    --->    can_loop_or_throw
    ;       cannot_loop_or_throw.

%-----------------------------------------------------------------------------%
%
% These versions use information from the intermodule-analysis framework.
%

% XXX Eventually we will only use these versions and the others can be
% deleted.

    % Return `goal_can_throw' if the given goal may throw an exception; return
    % `goal_cannot_throw' otherwise.
    %
    % This version differs from the ones below in that it can use results from
    % the intermodule-analysis framework (if they are available). The HLDS
    % and I/O state need to be threaded through in case analysis files need to
    % be read and in case IMDGs need to be updated.
    %
:- pred goal_can_throw(hlds_goal::in, goal_throw_status::out,
    module_info::in, module_info::out) is det.

    % Return `can_loop_or_throw' if the goal may loop forever or throw an
    % exception and return `cannot_loop_or_throw' otherwise.
    %
:- pred goal_can_loop_or_throw(hlds_goal::in, goal_loop_or_throw_status::out,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%

% The first three versions may be more accurate because they can use
% results of the termination and exception analyses.
% XXX These don't work with the intermodule-analysis framework, so don't
% use them in new code.

    % Succeeds if the goal cannot loop forever.
    %
:- pred goal_cannot_loop(module_info::in, hlds_goal::in) is semidet.

    % Succeeds if the goal can loop forever.
    %
:- pred goal_can_loop(module_info::in, hlds_goal::in) is semidet.

    % Succeeds if the goal cannot throw an exception.
    %
:- pred goal_cannot_throw(module_info::in, hlds_goal::in) is semidet.

    % Succeeds if the goal can throw an exception.
    %
:- pred goal_can_throw(module_info::in, hlds_goal::in) is semidet.

    % Succeeds if the goal cannot loop forever or throw an exception.
    %
:- pred goal_cannot_loop_or_throw(module_info::in, hlds_goal::in) is semidet.

    % Succeeds if the goal can loop forever or throw an exception.
    %
:- pred goal_can_loop_or_throw(module_info::in, hlds_goal::in) is semidet.

% These versions do not use the results of the termination or exception
% analyses.

    % Succeeds if the goal cannot loop forever or throw an exception.
    %
:- pred goal_cannot_loop_or_throw(hlds_goal::in) is semidet.

    % Succeed if the goal can loop forever or throw an exception.
    %
:- pred goal_can_loop_or_throw(hlds_goal::in) is semidet.

    % goal_is_flat(Goal) return `yes' if Goal does not contain any
    % branched structures (ie if-then-else or disjunctions or switches.)
    %
:- func goal_is_flat(hlds_goal) = bool.

    % Determine whether a goal might allocate some heap space, i.e.
    % whether it contains any construction unifications or predicate calls.
    % BEWARE that this predicate is only an approximation,
    % used to decide whether or not to try to reclaim the heap space;
    % currently it fails even for some goals which do allocate heap space,
    % such as construction of boxed constants.
    %
:- pred goal_may_allocate_heap(hlds_goal::in) is semidet.
:- pred goal_list_may_allocate_heap(hlds_goals::in) is semidet.

    % Succeed if execution of the given goal cannot encounter a context
    % that causes any variable to be flushed to its stack slot. If such a goal
    % needs a resume point, and that resume point cannot be backtracked to
    % once control leaves the goal, then the only entry point we need
    % for the resume point is the one with the resume variables in their
    % original locations.
    %
:- pred cannot_stack_flush(hlds_goal::in) is semidet.

    % Succeed if the given goal cannot fail before encountering a
    % context that forces all variables to be flushed to their stack slots.
    % If such a goal needs a resume point, the only entry point we need
    % is the stack entry point.
    %
:- pred cannot_fail_before_stack_flush(hlds_goal::in) is semidet.

    % count_recursive_calls(Goal, PredId, ProcId, Min, Max). Given that
    % we are in predicate PredId and procedure ProcId, return the minimum
    % and maximum number of recursive calls that an execution of Goal
    % may encounter.
    %
:- pred count_recursive_calls(hlds_goal::in, pred_id::in, proc_id::in,
    int::out, int::out) is det.

%-----------------------------------------------------------------------------%

    % Returns `yes' if the goal does not modify the trail.
    %
:- func goal_cannot_modify_trail(hlds_goal_info) = bool.

    % Returns `yes' if the goal may modify the trail.
    %
:- func goal_may_modify_trail(hlds_goal_info) = bool.

%-----------------------------------------------------------------------------%

    % Returns yes if the goal, or subgoal contained within, contains
    % any foreign code.
    %
:- func goal_has_foreign(hlds_goal) = bool.

:- type has_subgoals
    --->    has_subgoals
    ;       does_not_have_subgoals.

    % A goal is primitive iff it doesn't contain any sub-goals
    % (except possibly goals inside lambda expressions --
    % but lambda expressions will get transformed into separate
    % predicates by lambda.m).
    %
:- func goal_expr_has_subgoals(hlds_goal_expr) = has_subgoals.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.code_model.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module transform_hlds.
:- import_module transform_hlds.exception_analysis.
:- import_module transform_hlds.term_constr_main_types.

:- import_module int.
:- import_module maybe.
:- import_module require.

%-----------------------------------------------------------------------------%

goal_is_conj_of_unify(ToAssignVars0, Goal) :-
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    CodeModel = goal_info_get_code_model(GoalInfo),
    CodeModel = model_det,
    goal_to_conj_list(Goal, Conj),
    only_constant_goals(Conj, ToAssignVars0, ToAssignVars),
    set_of_var.is_empty(ToAssignVars).

all_disjuncts_are_conj_of_unify(_ToAssignVars, []).
all_disjuncts_are_conj_of_unify(ToAssignVars, [Disjunct | Disjuncts]) :-
    goal_is_conj_of_unify(ToAssignVars, Disjunct),
    all_disjuncts_are_conj_of_unify(ToAssignVars, Disjuncts).

:- pred only_constant_goals(list(hlds_goal)::in,
    set_of_progvar::in, set_of_progvar::out) is semidet.

only_constant_goals([], !ToAssignVars).
only_constant_goals([Goal | Goals], !ToAssignVars) :-
    Goal = hlds_goal(GoalExpr, _),
    % We could allow calls as well. Some procedures have an output inst
    % that fixes the value of the output variable, which is thus a constant.
    % However, calls to such procedures should have been inlined by now.
    (
        GoalExpr = unify(_, _, _, Unification, _),
        Unification = construct(Var, _, _, _, _, _, _)
    ;
        GoalExpr = scope(Reason, _),
        Reason = from_ground_term(Var, from_ground_term_construct)
    ),
    set_of_var.delete(Var, !ToAssignVars),
    only_constant_goals(Goals, !ToAssignVars).

%-----------------------------------------------------------------------------%
%
% A version of goal_cannot_loop_or_throw that uses results from the
% intermodule-analysis framework.
%

goal_can_throw(hlds_goal(GoalExpr, GoalInfo), Result, !ModuleInfo) :-
    Determinism = goal_info_get_determinism(GoalInfo),
    ( if Determinism = detism_erroneous then
        Result = can_throw
    else
        goal_can_throw_2(GoalExpr, GoalInfo, Result, !ModuleInfo)
    ).

:- pred goal_can_throw_2(hlds_goal_expr::in, hlds_goal_info::in,
    goal_throw_status::out, module_info::in, module_info::out) is det.

goal_can_throw_2(GoalExpr, _GoalInfo, Result, !ModuleInfo) :-
    (
        (
            GoalExpr = conj(_, Goals)
        ;
            GoalExpr = disj(Goals)
        ;
            GoalExpr = if_then_else(_, CondGoal, ThenGoal, ElseGoal),
            Goals = [CondGoal, ThenGoal, ElseGoal]
        ),
        goals_can_throw(Goals, Result, !ModuleInfo)
    ;
        GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
        lookup_exception_analysis_result(proc(PredId, ProcId), Status,
            !ModuleInfo),
        (
            Status = will_not_throw,
            Result = cannot_throw
        ;
            ( Status = may_throw(_)
            ; Status = throw_conditional
            ),
            Result = can_throw
        )
    ;
        GoalExpr = generic_call(_, _, _, _, _),
        % XXX We should use results form closure analysis here.
        Result = can_throw
    ;
        GoalExpr = switch(_, _, Cases),
        cases_can_throw(Cases, Result, !ModuleInfo)
    ;
        GoalExpr = unify(_, _, _, Uni, _),
        % Complicated unifies are _non_builtin_
        (
            Uni = complicated_unify(_, _, _),
            Result = can_throw
        ;
            ( Uni = construct(_, _, _, _, _, _, _)
            ; Uni = deconstruct(_, _, _, _, _, _)
            ; Uni = assign(_, _)
            ; Uni = simple_test(_, _)
            ),
            Result = cannot_throw
        )
    ;
        GoalExpr = negation(SubGoal),
        goal_can_throw(SubGoal, Result, !ModuleInfo)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These scopes contain only construction/deconstruction
            % unifications.
            Result = cannot_throw
        else
            goal_can_throw(SubGoal, Result, !ModuleInfo)
        )
    ;
        GoalExpr = call_foreign_proc(Attributes, _, _, _, _, _, _),
        ExceptionStatus = get_may_throw_exception(Attributes),
        ( if
            (
                ExceptionStatus = proc_will_not_throw_exception
            ;
                ExceptionStatus = default_exception_behaviour,
                get_may_call_mercury(Attributes) = proc_will_not_call_mercury
            )
        then
            Result = cannot_throw
        else
            Result = can_throw
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = bi_implication(GoalA, GoalB),
            goals_can_throw([GoalA, GoalB], Result, !ModuleInfo)
        ;
            ShortHand = atomic_goal(_, _, _, _, _, _, _),
            % Atomic goals currently throw an exception to signal a rollback so
            % it is pretty safe to say that any goal inside an atomic goal
            % can throw an exception.
            Result = can_throw
        ;
            ShortHand = try_goal(_, _, _),
            Result = can_throw
        )
    ).

:- pred goals_can_throw(hlds_goals::in, goal_throw_status::out,
    module_info::in, module_info::out) is det.

goals_can_throw([], cannot_throw, !ModuleInfo).
goals_can_throw([Goal | Goals], Result, !ModuleInfo) :-
    goal_can_throw(Goal, Result0, !ModuleInfo),
    (
        Result0 = cannot_throw,
        goals_can_throw(Goals, Result, !ModuleInfo)
    ;
        Result0 = can_throw,
        Result  = can_throw
    ).

:- pred cases_can_throw(list(case)::in, goal_throw_status::out,
    module_info::in, module_info::out) is det.

cases_can_throw([], cannot_throw, !ModuleInfo).
cases_can_throw([Case | Cases], Result, !ModuleInfo) :-
    Case = case(_, _, Goal),
    goal_can_throw(Goal, Result0, !ModuleInfo),
    (
        Result0 = cannot_throw,
        cases_can_throw(Cases, Result, !ModuleInfo)
    ;
        Result0 = can_throw,
        Result  = can_throw
    ).

goal_can_loop_or_throw(Goal, Result, !ModuleInfo) :-
    % XXX This will need to change after the termination analyses are converted
    % to use the intermodule-analysis framework.
    ( if goal_cannot_loop(!.ModuleInfo, Goal) then
        goal_can_throw(Goal, ThrowResult, !ModuleInfo),
        (
            ThrowResult = can_throw,
            Result = can_loop_or_throw
        ;
            ThrowResult = cannot_throw,
            Result = cannot_loop_or_throw
        )
    else
        Result = can_loop_or_throw
    ).

%-----------------------------------------------------------------------------%

goal_cannot_loop(ModuleInfo, Goal) :-
    goal_can_loop_func(yes(ModuleInfo), Goal) = no.

goal_can_loop(ModuleInfo, Goal) :-
    goal_can_loop_func(yes(ModuleInfo), Goal) = yes.

goal_cannot_throw(ModuleInfo, Goal) :-
    goal_can_throw_func(yes(ModuleInfo), Goal) = no.

goal_can_throw(ModuleInfo, Goal) :-
    goal_can_throw_func(yes(ModuleInfo), Goal) = yes.

goal_cannot_loop_or_throw(ModuleInfo, Goal) :-
    goal_can_loop_func(yes(ModuleInfo), Goal) = no,
    goal_can_throw_func(yes(ModuleInfo), Goal) = no.

goal_can_loop_or_throw(ModuleInfo, Goal) :-
    not goal_cannot_loop_or_throw(ModuleInfo, Goal).

goal_cannot_loop_or_throw(Goal) :-
    goal_can_loop_func(no, Goal) = no,
    goal_can_throw_func(no, Goal) = no.

goal_can_loop_or_throw(Goal) :-
    not goal_cannot_loop_or_throw(Goal).

:- func goal_can_loop_func(maybe(module_info), hlds_goal) = bool.

goal_can_loop_func(MaybeModuleInfo, Goal) = CanLoop :-
    Goal = hlds_goal(GoalExpr, _),
    (
        GoalExpr = unify(_, _, _, Uni, _),
        (
            ( Uni = assign(_, _)
            ; Uni = simple_test(_, _)
            ; Uni = construct(_, _, _, _, _, _, _)
            ; Uni = deconstruct(_, _, _, _, _, _)
            ),
            CanLoop = no
        ;
            Uni = complicated_unify(_, _, _),
            % It can call, possibly indirectly, a user-specified unification
            % predicate.
            CanLoop = yes
        )
    ;
        GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
        ( if
            MaybeModuleInfo = yes(ModuleInfo),
            module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _,
                ProcInfo),
            (
                proc_info_get_maybe_termination_info(ProcInfo, MaybeTermInfo),
                MaybeTermInfo = yes(cannot_loop(_))
            ;
                proc_info_get_termination2_info(ProcInfo, Term2Info),
                term2_info_get_term_status(Term2Info) = yes(cannot_loop(_))
            )
        then
            CanLoop = no
        else
            CanLoop = yes
        )
    ;
        GoalExpr = generic_call(_, _, _, _, _),
        % We have no idea whether the called goal can throw exceptions,
        % at least without closure analysis.
        CanLoop = yes
    ;
        GoalExpr = call_foreign_proc(Attributes, _, _, _, _, _, _),
        ( if
            Terminates = get_terminates(Attributes),
            require_complete_switch [Terminates]
            (
                Terminates = proc_terminates
            ;
                Terminates = depends_on_mercury_calls,
                get_may_call_mercury(Attributes) = proc_will_not_call_mercury
            ;
                Terminates = proc_does_not_terminate,
                fail
            )
        then
            CanLoop = no
        else
            CanLoop = yes
        )
    ;
        GoalExpr = conj(plain_conj, Goals),
        CanLoop = goal_list_can_loop(MaybeModuleInfo, Goals)
    ;
        GoalExpr = conj(parallel_conj, _Goals),
        % In theory, parallel conjunctions can get into deadlocks, which are
        % effectively a form of nontermination. We can return `no' here only
        % if we are sure this cannot happen for this conjunction.
        CanLoop = yes
    ;
        GoalExpr = disj(Goals),
        CanLoop = goal_list_can_loop(MaybeModuleInfo, Goals)
    ;
        GoalExpr = switch(_Var, _CanFail, Cases),
        CanLoop = case_list_can_loop(MaybeModuleInfo, Cases)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        ( if goal_can_loop_func(MaybeModuleInfo, Cond) = yes then
            CanLoop = yes
        else if goal_can_loop_func(MaybeModuleInfo, Then) = yes then
            CanLoop = yes
        else
            CanLoop = goal_can_loop_func(MaybeModuleInfo, Else)
        )
    ;
        GoalExpr = negation(SubGoal),
        CanLoop = goal_can_loop_func(MaybeModuleInfo, SubGoal)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These scopes contain only construction/deconstruction
            % unifications.
            CanLoop = no
        else
            CanLoop = goal_can_loop_func(MaybeModuleInfo, SubGoal)
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            MainGoalCanLoop = goal_can_loop_func(MaybeModuleInfo, MainGoal),
            OrElseCanLoop = goal_list_can_loop(MaybeModuleInfo, OrElseGoals),
            CanLoop = MainGoalCanLoop `or` OrElseCanLoop
        ;
            ShortHand = try_goal(_, _, SubGoal),
            CanLoop = goal_can_loop_func(MaybeModuleInfo, SubGoal)
        ;
            ShortHand = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        )
    ).

:- func goal_list_can_loop(maybe(module_info), list(hlds_goal)) = bool.

goal_list_can_loop(_, []) = no.
goal_list_can_loop(MaybeModuleInfo, [Goal | Goals]) =
    ( if goal_can_loop_func(MaybeModuleInfo, Goal) = yes then
        yes
    else
        goal_list_can_loop(MaybeModuleInfo, Goals)
    ).

:- func case_list_can_loop(maybe(module_info), list(case)) = bool.

case_list_can_loop(_, []) = no.
case_list_can_loop(MaybeModuleInfo, [case(_, _, Goal) | Cases]) =
    ( if goal_can_loop_func(MaybeModuleInfo, Goal) = yes then
        yes
    else
        case_list_can_loop(MaybeModuleInfo, Cases)
    ).

%-----------------------------------------------------------------------------%

:- func goal_can_throw_func(maybe(module_info), hlds_goal) = bool.

goal_can_throw_func(MaybeModuleInfo, hlds_goal(GoalExpr, GoalInfo))
        = CanThrow :-
    Determinism = goal_info_get_determinism(GoalInfo),
    ( if Determinism = detism_erroneous then
        CanThrow = yes
    else
        CanThrow = goal_expr_can_throw(MaybeModuleInfo, GoalExpr)
    ).

:- func goal_expr_can_throw(maybe(module_info), hlds_goal_expr) = bool.

goal_expr_can_throw(MaybeModuleInfo, GoalExpr) = CanThrow :-
    (
        GoalExpr = unify(_, _, _, Uni, _),
        (
            ( Uni = assign(_, _)
            ; Uni = simple_test(_, _)
            ; Uni = construct(_, _, _, _, _, _, _)
            ; Uni = deconstruct(_, _, _, _, _, _)
            ),
            CanThrow = no
        ;
            Uni = complicated_unify(_, _, _),
            % It can call, possibly indirectly, a user-specified unification
            % predicate.
            CanThrow = yes
        )
    ;
        GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
        ( if
            MaybeModuleInfo = yes(ModuleInfo),
            module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
                _PredInfo, ProcInfo),
            proc_info_get_exception_info(ProcInfo, MaybeExceptionInfo),
            MaybeExceptionInfo = yes(ExceptionInfo),
            ExceptionInfo = proc_exception_info(will_not_throw, _)
        then
            CanThrow = no
        else
            CanThrow = yes
        )
    ;
        GoalExpr = call_foreign_proc(Attributes, _, _, _, _, _, _),
        ExceptionStatus = get_may_throw_exception(Attributes),
        ( if
            (
                ExceptionStatus = proc_will_not_throw_exception
            ;
                ExceptionStatus = default_exception_behaviour,
                get_may_call_mercury(Attributes) = proc_will_not_call_mercury
            )
        then
            CanThrow = no
        else
            CanThrow = yes
        )
    ;
        GoalExpr = generic_call(_, _, _, _, _),
        % We have no idea whether the called goal can throw exceptions,
        % at least without closure analysis.
        CanThrow = yes
    ;
        ( GoalExpr = conj(_ConjType, Goals)
        ; GoalExpr = disj(Goals)
        ),
        CanThrow = goal_list_can_throw(MaybeModuleInfo, Goals)
    ;
        GoalExpr = switch(_Var, _CanFail, Cases),
        CanThrow = case_list_can_throw(MaybeModuleInfo, Cases)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        ( if goal_can_throw_func(MaybeModuleInfo, Cond) = yes then
            CanThrow = yes
        else if goal_can_throw_func(MaybeModuleInfo, Then) = yes then
            CanThrow = yes
        else
            CanThrow = goal_can_throw_func(MaybeModuleInfo, Else)
        )
    ;
        GoalExpr = negation(SubGoal),
        CanThrow = goal_can_throw_func(MaybeModuleInfo, SubGoal)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These scopes contain only construction/deconstruction
            % unifications.
            CanThrow = no
        else
            CanThrow = goal_can_throw_func(MaybeModuleInfo, SubGoal)
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, _, _, _),
            CanThrow = yes
        ;
            ShortHand = try_goal(_, _, _),
            CanThrow = yes
        ;
            ShortHand = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        )
    ).

:- func goal_list_can_throw(maybe(module_info), list(hlds_goal)) = bool.

goal_list_can_throw(_, []) = no.
goal_list_can_throw(MaybeModuleInfo, [Goal | Goals]) =
    ( if goal_can_throw_func(MaybeModuleInfo, Goal) = yes then
        yes
    else
        goal_list_can_throw(MaybeModuleInfo, Goals)
    ).

:- func case_list_can_throw(maybe(module_info), list(case)) = bool.

case_list_can_throw(_, []) = no.
case_list_can_throw(MaybeModuleInfo, [case(_, _, Goal) | Cases]) =
    ( if goal_can_throw_func(MaybeModuleInfo, Goal) = yes then
        yes
    else
        case_list_can_throw(MaybeModuleInfo, Cases)
    ).

%-----------------------------------------------------------------------------%

goal_is_flat(hlds_goal(GoalExpr, _GoalInfo)) = goal_is_flat_expr(GoalExpr).

:- func goal_is_flat_expr(hlds_goal_expr) = bool.

goal_is_flat_expr(GoalExpr) = IsFlat :-
    (
        ( GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        IsFlat = yes
    ;
        GoalExpr = conj(ConjType, Goals),
        (
            ConjType = parallel_conj,
            IsFlat = no
        ;
            ConjType = plain_conj,
            IsFlat = goal_is_flat_list(Goals)
        )
    ;
        ( GoalExpr = disj(_)
        ; GoalExpr = switch(_, _, _)
        ; GoalExpr = if_then_else(_, _, _, _)
        ; GoalExpr = shorthand(_)
        ),
        IsFlat = no
    ;
        GoalExpr = negation(SubGoal),
        IsFlat = goal_is_flat(SubGoal)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            IsFlat = yes
        else
            IsFlat = goal_is_flat(SubGoal)
        )
    ).

:- func goal_is_flat_list(list(hlds_goal)) = bool.

goal_is_flat_list([]) = yes.
goal_is_flat_list([Goal | Goals]) = IsFlat :-
    ( if goal_is_flat(Goal) = yes then
        IsFlat = goal_is_flat_list(Goals)
    else
        IsFlat = no
    ).

%-----------------------------------------------------------------------------%

goal_may_allocate_heap(Goal) :-
    goal_may_allocate_heap(Goal, yes).

goal_list_may_allocate_heap(Goals) :-
    goal_list_may_allocate_heap(Goals, yes).

:- pred goal_may_allocate_heap(hlds_goal::in, bool::out) is det.

goal_may_allocate_heap(hlds_goal(GoalExpr, _GoalInfo), May) :-
    goal_may_allocate_heap_2(GoalExpr, May).

:- pred goal_may_allocate_heap_2(hlds_goal_expr::in, bool::out) is det.

goal_may_allocate_heap_2(GoalExpr, May) :-
    (
        GoalExpr = unify(_, _, _, Unification, _),
        ( if
            Unification = construct(_, _, Args, _, _, _, _),
            Args = [_ | _]
        then
            May = yes
        else
            May = no
        )
    ;
        GoalExpr = plain_call(_, _, _, Builtin, _, _),
        (
            Builtin = inline_builtin,
            May = no
        ;
            Builtin = not_builtin,
            May = yes
        )
    ;
        GoalExpr = generic_call(_, _, _, _, _),
        May = yes
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        % We cannot safely say that a foreign code fragment does not
        % allocate memory without knowing all the #defined macros that
        % expand to incr_hp and variants thereof.
        % XXX You could make it an attribute of the foreign code and
        % trust the programmer.
        May = yes
    ;
        GoalExpr = conj(ConjType, Goals),
        (
            ConjType = parallel_conj,
            May = yes
        ;
            ConjType = plain_conj,
            goal_list_may_allocate_heap(Goals, May)
        )
    ;
        GoalExpr = disj(Goals),
        goal_list_may_allocate_heap(Goals, May)
    ;
        GoalExpr = switch(_Var, _CanFail, Cases),
        cases_may_allocate_heap(Cases, May)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        ( if goal_may_allocate_heap(Cond, yes) then
            May = yes
        else if goal_may_allocate_heap(Then, yes) then
            May = yes
        else
            goal_may_allocate_heap(Else, May)
        )
    ;
        GoalExpr = negation(SubGoal),
        goal_may_allocate_heap(SubGoal, May)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % Construct scopes construct ground terms, but they construct them
            % statically, so if we modify the code above to check the
            % construct_how field of construction unifications, we could
            % return May = no for them.
            % Deconstruct scopes do not construct new ground terms.
            May = yes
        else
            goal_may_allocate_heap(SubGoal, May)
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ( ShortHand = atomic_goal(_, _, _, _, _, _, _)
            ; ShortHand = try_goal(_, _, _)
            ),
            May = yes
        ;
            ShortHand = bi_implication(GoalA, GoalB),
            ( if goal_may_allocate_heap(GoalA, yes) then
                May = yes
            else
                goal_may_allocate_heap(GoalB, May)
            )
        )
    ).

:- pred goal_list_may_allocate_heap(list(hlds_goal)::in, bool::out) is det.

goal_list_may_allocate_heap([], no).
goal_list_may_allocate_heap([Goal | Goals], May) :-
    ( if goal_may_allocate_heap(Goal, yes) then
        May = yes
    else
        goal_list_may_allocate_heap(Goals, May)
    ).

:- pred cases_may_allocate_heap(list(case)::in, bool::out) is det.

cases_may_allocate_heap([], no).
cases_may_allocate_heap([case(_, _, Goal) | Cases], May) :-
    ( if goal_may_allocate_heap(Goal, yes) then
        May = yes
    else
        cases_may_allocate_heap(Cases, May)
    ).

%-----------------------------------------------------------------------------%

cannot_stack_flush(hlds_goal(GoalExpr, _)) :-
    cannot_stack_flush_2(GoalExpr).

:- pred cannot_stack_flush_2(hlds_goal_expr::in) is semidet.

cannot_stack_flush_2(unify(_, _, _, Unify, _)) :-
    Unify \= complicated_unify(_, _, _).
cannot_stack_flush_2(plain_call(_, _, _, BuiltinState, _, _)) :-
    BuiltinState = inline_builtin.
cannot_stack_flush_2(conj(ConjType, Goals)) :-
    ConjType = plain_conj,
    cannot_stack_flush_goals(Goals).
cannot_stack_flush_2(switch(_, _, Cases)) :-
    cannot_stack_flush_cases(Cases).
cannot_stack_flush_2(negation(hlds_goal(unify(_, _, _, Unify, _), _))) :-
    Unify \= complicated_unify(_, _, _).

:- pred cannot_stack_flush_goals(list(hlds_goal)::in) is semidet.

cannot_stack_flush_goals([]).
cannot_stack_flush_goals([Goal | Goals]) :-
    cannot_stack_flush(Goal),
    cannot_stack_flush_goals(Goals).

:- pred cannot_stack_flush_cases(list(case)::in) is semidet.

cannot_stack_flush_cases([]).
cannot_stack_flush_cases([case(_, _, Goal) | Cases]) :-
    cannot_stack_flush(Goal),
    cannot_stack_flush_cases(Cases).

%-----------------------------------------------------------------------------%

cannot_fail_before_stack_flush(hlds_goal(GoalExpr, GoalInfo)) :-
    Detism = goal_info_get_determinism(GoalInfo),
    determinism_components(Detism, CanFail, _),
    (
        CanFail = cannot_fail
    ;
        CanFail = can_fail,
        cannot_fail_before_stack_flush_2(GoalExpr)
    ).

:- pred cannot_fail_before_stack_flush_2(hlds_goal_expr::in) is semidet.

cannot_fail_before_stack_flush_2(conj(ConjType, Goals)) :-
    ConjType = plain_conj,
    cannot_fail_before_stack_flush_conj(Goals).

:- pred cannot_fail_before_stack_flush_conj(list(hlds_goal)::in) is semidet.

cannot_fail_before_stack_flush_conj([]).
cannot_fail_before_stack_flush_conj([Goal | Goals]) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    ( if
        (
            GoalExpr = plain_call(_, _, _, BuiltinState, _, _),
            BuiltinState \= inline_builtin
        ;
            GoalExpr = generic_call(_, _, _, _, _)
        )
    then
        true
    else if
        Detism = goal_info_get_determinism(GoalInfo),
        determinism_components(Detism, cannot_fail, _)
    then
        cannot_fail_before_stack_flush_conj(Goals)
    else
        fail
    ).

%-----------------------------------------------------------------------------%

count_recursive_calls(Goal, PredId, ProcId, Min, Max) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Min = 0,
        Max = 0
    ;
        GoalExpr = plain_call(CallPredId, CallProcId, _, _, _, _),
        ( if
            PredId = CallPredId,
            ProcId = CallProcId
        then
            Count = 1
        else
            Count = 0
        ),
        Min = Count,
        Max = Count
    ;
        GoalExpr = conj(_, Goals),
        count_recursive_calls_conj(Goals, PredId, ProcId, 0, 0, Min, Max)
    ;
        GoalExpr = disj(Goals),
        count_recursive_calls_disj(Goals, PredId, ProcId, Min, Max)
    ;
        GoalExpr = switch(_, _, Cases),
        count_recursive_calls_cases(Cases, PredId, ProcId, Min, Max)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        count_recursive_calls(Cond, PredId, ProcId, CMin, CMax),
        count_recursive_calls(Then, PredId, ProcId, TMin, TMax),
        count_recursive_calls(Else, PredId, ProcId, EMin, EMax),
        CTMin = CMin + TMin,
        CTMax = CMax + TMax,
        int.min(CTMin, EMin, Min),
        int.max(CTMax, EMax, Max)
    ;
        GoalExpr = negation(SubGoal),
        count_recursive_calls(SubGoal, PredId, ProcId, Min, Max)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These scopes contain only construction/deconstruction
            % unifications.
            Min = 0,
            Max = 0
        else
            count_recursive_calls(SubGoal, PredId, ProcId, Min, Max)
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            count_recursive_calls_disj([MainGoal | OrElseGoals],
                PredId, ProcId, Min, Max)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            count_recursive_calls(SubGoal, PredId, ProcId, Min, Max)
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

:- pred count_recursive_calls_conj(list(hlds_goal)::in,
    pred_id::in, proc_id::in, int::in, int::in, int::out, int::out) is det.

count_recursive_calls_conj([], _, _, Min, Max, Min, Max).
count_recursive_calls_conj([Goal | Goals], PredId, ProcId, Min0, Max0,
        Min, Max) :-
    count_recursive_calls(Goal, PredId, ProcId, Min1, Max1),
    Min2 = Min0 + Min1,
    Max2 = Max0 + Max1,
    count_recursive_calls_conj(Goals, PredId, ProcId,
        Min2, Max2, Min, Max).

:- pred count_recursive_calls_disj(list(hlds_goal)::in,
    pred_id::in, proc_id::in, int::out, int::out) is det.

count_recursive_calls_disj([], _, _, 0, 0).
count_recursive_calls_disj([Goal | Goals], PredId, ProcId, Min, Max) :-
    (
        Goals = [],
        count_recursive_calls(Goal, PredId, ProcId, Min, Max)
    ;
        Goals = [_ | _],
        count_recursive_calls(Goal, PredId, ProcId, Min0, Max0),
        count_recursive_calls_disj(Goals, PredId, ProcId, Min1, Max1),
        int.min(Min0, Min1, Min),
        int.max(Max0, Max1, Max)
    ).

:- pred count_recursive_calls_cases(list(case)::in, pred_id::in, proc_id::in,
    int::out, int::out) is det.

count_recursive_calls_cases([], _, _, _, _) :-
    unexpected($pred, "[]").
count_recursive_calls_cases([case(_, _, Goal) | Cases], PredId, ProcId,
        Min, Max) :-
    (
        Cases = [],
        count_recursive_calls(Goal, PredId, ProcId, Min, Max)
    ;
        Cases = [_ | _],
        count_recursive_calls(Goal, PredId, ProcId, Min0, Max0),
        count_recursive_calls_cases(Cases, PredId, ProcId, Min1, Max1),
        int.min(Min0, Min1, Min),
        int.max(Max0, Max1, Max)
    ).

%-----------------------------------------------------------------------------%
%
% Trail usage
%

goal_cannot_modify_trail(GoalInfo) =
    ( if goal_info_has_feature(GoalInfo, feature_will_not_modify_trail) then
        yes
    else
        no
    ).

goal_may_modify_trail(GoalInfo) = bool.not(goal_cannot_modify_trail(GoalInfo)).

%-----------------------------------------------------------------------------%

goal_has_foreign(Goal) = HasForeign :-
    Goal = hlds_goal(GoalExpr, _),
    (
        ( GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ),
        HasForeign = no
    ;
        GoalExpr = conj(_, Goals),
        HasForeign = goal_list_has_foreign(Goals)
    ;
        GoalExpr = disj(Goals),
        HasForeign = goal_list_has_foreign(Goals)
    ;
        GoalExpr = switch(_, _, Cases),
        HasForeign = case_list_has_foreign(Cases)
    ;
        GoalExpr = negation(SubGoal),
        HasForeign = goal_has_foreign(SubGoal)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            HasForeign = no
        else
            HasForeign = goal_has_foreign(SubGoal)
        )
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        ( if
            ( goal_has_foreign(Cond) = yes
            ; goal_has_foreign(Then) = yes
            ; goal_has_foreign(Else) = yes
            )
        then
            HasForeign = yes
        else
            HasForeign = no
        )
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        HasForeign = yes
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, _, _, _),
            HasForeign = yes
        ;
            ShortHand = try_goal(_, _, SubGoal),
            HasForeign = goal_has_foreign(SubGoal)
        ;
            ShortHand = bi_implication(GoalA, GoalB),
            HasForeign = bool.or(goal_has_foreign(GoalA),
                goal_has_foreign(GoalB))
        )
    ).

:- func goal_list_has_foreign(list(hlds_goal)) = bool.

goal_list_has_foreign([]) = no.
goal_list_has_foreign([Goal | Goals]) = HasForeign :-
    ( if goal_has_foreign(Goal) = yes then
        HasForeign = yes
    else
        HasForeign = goal_list_has_foreign(Goals)
    ).

:- func case_list_has_foreign(list(case)) = bool.

case_list_has_foreign([]) = no.
case_list_has_foreign([Case | Cases]) = HasForeign :-
    Case = case(_, _, Goal),
    ( if goal_has_foreign(Goal) = yes then
        HasForeign = yes
    else
        HasForeign = case_list_has_foreign(Cases)
    ).

%-----------------------------------------------------------------------------%

goal_expr_has_subgoals(GoalExpr) = HasSubGoals :-
    (
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _,  _)
        ),
        HasSubGoals = does_not_have_subgoals
    ;
        ( GoalExpr = conj(_, SubGoals)
        ; GoalExpr = disj(SubGoals)
        ),
        (
            SubGoals = [],
            HasSubGoals = does_not_have_subgoals
        ;
            SubGoals = [_ | _],
            HasSubGoals = has_subgoals
        )
    ;
        ( GoalExpr = if_then_else(_, _, _, _)
        ; GoalExpr = negation(_)
        ; GoalExpr = switch(_, _, _)
        ; GoalExpr = scope(_, _)
        ),
        HasSubGoals = has_subgoals
    ;
        GoalExpr = shorthand(ShortHand),
        ( ShortHand = atomic_goal(_, _, _, _, _, _, _)
        ; ShortHand = try_goal(_, _, _)
        ; ShortHand = bi_implication(_, _)
        ),
        HasSubGoals = has_subgoals
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.goal_form.
%-----------------------------------------------------------------------------%
