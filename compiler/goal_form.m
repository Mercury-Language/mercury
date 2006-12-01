%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006 The University of Melbourne.
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

:- import_module bool.
:- import_module io.

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
% These versions use information from the intermodule-analysis framework
%

% XXX Eventually we will only use these versions and the others can be
%     deleted.

    % Return `goal_can_throw' if the given goal may throw an exception; return
    % `goal_cannot_throw' otherwise.
    %
    % This version differs from the ones below in that it can use results from
    % the intermodule-analysis framework (if they are available).  The HLDS
    % and I/O state need to be threaded through in case analysis files need to
    % be read and in case IMDGs need to be updated.
    %
:- pred goal_can_throw(hlds_goal::in, goal_throw_status::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

    % Return `can_loop_or_throw' if the goal may loop forever or throw an
    % exception and return `cannot_loop_or_throw' otherwise.
    %
:- pred goal_can_loop_or_throw(hlds_goal::in, goal_loop_or_throw_status::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

%
% The first three versions may be more accurate because they can use
% results of the termination and exception analyses.
% XXX These don't work with the intermodule-analysis framework, so don't
%     use them in new code.

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

%
% These versions do not use the results of the termination or exception
% analyses.
%

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
    % whether it contains any construction unifications or predicate
    % calls.  BEWARE that this predicate is only an approximation,
    % used to decide whether or not to try to reclaim the heap
    % space; currently it fails even for some goals which do
    % allocate heap space, such as construction of boxed constants.
    %
:- pred goal_may_allocate_heap(hlds_goal::in) is semidet.
:- pred goal_list_may_allocate_heap(hlds_goals::in) is semidet.

    % Succeed if execution of the given goal cannot encounter a context
    % that causes any variable to be flushed to its stack slot.  If such a
    % goal needs a resume point, and that resume point cannot be
    % backtracked to once control leaves the goal, then the only entry
    % point we need for the resume point is the one with the resume
    % variables in their original locations.
    %
:- pred cannot_stack_flush(hlds_goal::in) is semidet.

    % Succeed if the given goal cannot fail before encountering a
    % context that forces all variables to be flushed to their stack
    % slots.  If such a goal needs a resume point, the only entry
    % point we need is the stack entry point.
    %
:- pred cannot_fail_before_stack_flush(hlds_goal::in) is semidet.

    % count_recursive_calls(Goal, PredId, ProcId, Min, Max). Given
    % that we are in predicate PredId and procedure ProcId, return
    % the minimum and maximum number of recursive calls that an
    % execution of Goal may encounter.
    %
:- pred count_recursive_calls(hlds_goal::in, pred_id::in, proc_id::in,
    int::out, int::out) is det.

%-----------------------------------------------------------------------------%
%
% Trail usage
%

    % Returns `yes' if the goal does not modify the trail.
    %
:- func goal_cannot_modify_trail(hlds_goal_info) = bool.

    % Returns `yes' if the goal may modify the trail.
    %
:- func goal_may_modify_trail(hlds_goal_info) = bool.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.exception_analysis.
:- import_module transform_hlds.term_constr_main.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.

%-----------------------------------------------------------------------------%
%
% A version of goal_cannot_loop_or_throw that uses results from the
% intermodule-analysis framework
%

goal_can_throw(GoalExpr - GoalInfo, Result, !ModuleInfo, !IO) :-
    goal_info_get_determinism(GoalInfo, Determinism),
    ( Determinism \= detism_erroneous ->
        goal_can_throw_2(GoalExpr, GoalInfo, Result, !ModuleInfo, !IO)
    ;
        Result = can_throw
    ).

:- pred goal_can_throw_2(hlds_goal_expr::in, hlds_goal_info::in,
    goal_throw_status::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

goal_can_throw_2(Goal, _GoalInfo, Result, !ModuleInfo, !IO) :-
    (
        Goal = conj(_, Goals)
    ;
        Goal = disj(Goals)
    ;
        Goal = if_then_else(_, IfGoal, ThenGoal, ElseGoal),
        Goals = [IfGoal, ThenGoal, ElseGoal]
    ),
    goals_can_throw(Goals, Result, !ModuleInfo, !IO).
goal_can_throw_2(Goal, _GoalInfo, Result, !ModuleInfo, !IO) :-
    Goal = plain_call(PredId, ProcId, _, _, _, _),
    lookup_exception_analysis_result(proc(PredId, ProcId), Status,
        !ModuleInfo, !IO),
    ( Status = will_not_throw ->
        Result = cannot_throw
    ;
        Result = can_throw
    ).
goal_can_throw_2(Goal, _GoalInfo, Result, !ModuleInfo, !IO) :-
    % XXX We should use results form closure analysis here.
    Goal = generic_call(_, _, _, _),
    Result = can_throw.
goal_can_throw_2(Goal, _GoalInfo, Result, !ModuleInfo, !IO) :-
    Goal = switch(_, _, Cases),
    cases_can_throw(Cases, Result, !ModuleInfo, !IO).
goal_can_throw_2(Goal, _GoalInfo, Result, !ModuleInfo, !IO) :-
    Goal = unify(_, _, _, Uni, _),
    % Complicated unifies are _non_builtin_
    ( Uni = complicated_unify(_, _, _) ->
        Result = can_throw
    ;
        Result = cannot_throw
    ).
goal_can_throw_2(OuterGoal, _, Result, !ModuleInfo, !IO) :-
    (
        OuterGoal = negation(InnerGoal)
    ;
        OuterGoal = scope(_, InnerGoal)
    ),
    goal_can_throw(InnerGoal, Result, !ModuleInfo, !IO).
goal_can_throw_2(Goal, _, Result, !ModuleInfo, !IO) :-
    Goal = call_foreign_proc(Attributes, _, _, _, _, _, _),
    ExceptionStatus = get_may_throw_exception(Attributes),
    (
        (
            ExceptionStatus = proc_will_not_throw_exception
        ;
            ExceptionStatus = default_exception_behaviour,
            get_may_call_mercury(Attributes) = proc_will_not_call_mercury
        )
    ->
        Result = cannot_throw
    ;
        Result = can_throw
    ).
goal_can_throw_2(Goal, _, can_throw, !ModuleInfo, !IO) :-
    Goal = shorthand(_).    % XXX maybe call unexpected/2 here.

:- pred goals_can_throw(hlds_goals::in, goal_throw_status::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

goals_can_throw([], cannot_throw, !ModuleInfo, !IO).
goals_can_throw([Goal | Goals], Result, !ModuleInfo, !IO) :-
    goal_can_throw(Goal, Result0, !ModuleInfo, !IO),
    (
        Result0 = cannot_throw,
        goals_can_throw(Goals, Result, !ModuleInfo, !IO)
    ;
        Result0 = can_throw,
        Result  = can_throw
    ).

:- pred cases_can_throw(list(case)::in, goal_throw_status::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

cases_can_throw([], cannot_throw, !ModuleInfo, !IO).
cases_can_throw([Case | Cases], Result, !ModuleInfo, !IO) :-
    Case = case(_, Goal),
    goal_can_throw(Goal, Result0, !ModuleInfo, !IO),
    (
        Result0 = cannot_throw,
        cases_can_throw(Cases, Result, !ModuleInfo, !IO)
    ;
        Result0 = can_throw,
        Result  = can_throw
    ).

goal_can_loop_or_throw(Goal, Result, !ModuleInfo, !IO) :-
    % XXX this will need to change after the termination analyses are
    %     converted to use the intermodule-analysis framework.
    ( goal_cannot_loop(!.ModuleInfo, Goal) ->
        goal_can_throw(Goal, ThrowResult, !ModuleInfo, !IO),
        (
            ThrowResult = can_throw,
            Result = can_loop_or_throw
        ;
            ThrowResult = cannot_throw,
            Result = cannot_loop_or_throw
        )
    ;
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

goal_can_loop_func(MaybeModuleInfo, GoalExpr - _) =
    goal_expr_can_loop(MaybeModuleInfo, GoalExpr).

:- func goal_expr_can_loop(maybe(module_info), hlds_goal_expr) = bool.

goal_expr_can_loop(MaybeModuleInfo, conj(plain_conj, Goals)) =
    goal_list_can_loop(MaybeModuleInfo, Goals).
goal_expr_can_loop(_MaybeModuleInfo, conj(parallel_conj, _Goals)) = yes.
    % In theory, parallel conjunctions can get into deadlocks, which are
    % effectively a form of nontermination. We can return `no' here only
    % if we are sure this cannot happen for this conjunction.
goal_expr_can_loop(MaybeModuleInfo, disj(Goals)) =
    goal_list_can_loop(MaybeModuleInfo, Goals).
goal_expr_can_loop(MaybeModuleInfo, switch(_Var, _CanFail, Cases)) =
    case_list_can_loop(MaybeModuleInfo, Cases).
goal_expr_can_loop(MaybeModuleInfo, negation(Goal)) =
    goal_can_loop_func(MaybeModuleInfo, Goal).
goal_expr_can_loop(MaybeModuleInfo, scope(_, Goal)) =
    goal_can_loop_func(MaybeModuleInfo, Goal).
goal_expr_can_loop(MaybeModuleInfo, Goal) = CanLoop :-
    Goal = if_then_else(_Vars, Cond, Then, Else),
    ( goal_can_loop_func(MaybeModuleInfo, Cond) = yes ->
        CanLoop = yes
    ; goal_can_loop_func(MaybeModuleInfo, Then) = yes ->
        CanLoop = yes
    ; goal_can_loop_func(MaybeModuleInfo, Else) = yes ->
        CanLoop = yes
    ;
        CanLoop = no
    ).
goal_expr_can_loop(_MaybeModuleInfo, Goal) = CanLoop :-
    Goal = call_foreign_proc(Attributes, _, _, _, _, _, _),
    (
        Terminates = get_terminates(Attributes),
        (
            Terminates = proc_terminates
        ;
            Terminates = depends_on_mercury_calls,
            get_may_call_mercury(Attributes) = proc_will_not_call_mercury
        )
    ->
        CanLoop = no
    ;
        CanLoop = yes
    ).
goal_expr_can_loop(MaybeModuleInfo, Goal) = CanLoop :-
    Goal = plain_call(PredId, ProcId, _, _, _, _),
    (
        MaybeModuleInfo = yes(ModuleInfo),
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
        (
            proc_info_get_maybe_termination_info(ProcInfo, MaybeTermInfo),
            MaybeTermInfo = yes(cannot_loop(_))
        ;
            proc_info_get_termination2_info(ProcInfo, Term2Info),
            Term2Info ^ term_status = yes(cannot_loop(_))
        )
    ->
        CanLoop = no
    ;
        CanLoop = yes
    ).
goal_expr_can_loop(_MaybeModuleInfo, Goal) = yes :-
    % We have no idea whether the called goal can throw exceptions,
    % at least without closure analysis.
    Goal = generic_call(_, _, _, _).
goal_expr_can_loop(_, unify(_, _, _, Uni, _)) = CanLoop :-
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
    ).
goal_expr_can_loop(_, shorthand(_)) = _ :-
    unexpected(this_file, "goal_expr_can_loop: shorthand").

:- func goal_list_can_loop(maybe(module_info), list(hlds_goal)) = bool.

goal_list_can_loop(_, []) = no.
goal_list_can_loop(MaybeModuleInfo, [Goal | Goals]) =
    ( goal_can_loop_func(MaybeModuleInfo, Goal) = yes ->
        yes
    ;
        goal_list_can_loop(MaybeModuleInfo, Goals)
    ).

:- func case_list_can_loop(maybe(module_info), list(case)) = bool.

case_list_can_loop(_, []) = no.
case_list_can_loop(MaybeModuleInfo, [case(_, Goal) | Cases]) =
    ( goal_can_loop_func(MaybeModuleInfo, Goal) = yes ->
        yes
    ;
        case_list_can_loop(MaybeModuleInfo, Cases)
    ).

%-----------------------------------------------------------------------------%

goal_cannot_throw(ModuleInfo, Goal) :-
    goal_can_throw_func(yes(ModuleInfo), Goal) = no.

:- func goal_can_throw_func(maybe(module_info), hlds_goal) = bool.

goal_can_throw_func(MaybeModuleInfo, GoalExpr - GoalInfo) = CanThrow :-
    goal_info_get_determinism(GoalInfo, Determinism),
    ( Determinism = detism_erroneous ->
        CanThrow = yes
    ;
        CanThrow = goal_expr_can_throw(MaybeModuleInfo, GoalExpr)
    ).

:- func goal_expr_can_throw(maybe(module_info), hlds_goal_expr) = bool.

goal_expr_can_throw(MaybeModuleInfo, conj(_ConjType, Goals)) =
    goal_list_can_throw(MaybeModuleInfo, Goals).
goal_expr_can_throw(MaybeModuleInfo, disj(Goals)) =
    goal_list_can_throw(MaybeModuleInfo, Goals).
goal_expr_can_throw(MaybeModuleInfo, switch(_Var, _Category, Cases)) =
    case_list_can_throw(MaybeModuleInfo, Cases).
goal_expr_can_throw(MaybeModuleInfo, negation(Goal)) =
    goal_can_throw_func(MaybeModuleInfo, Goal).
goal_expr_can_throw(MaybeModuleInfo, scope(_, Goal)) =
    goal_can_throw_func(MaybeModuleInfo, Goal).
goal_expr_can_throw(MaybeModuleInfo, Goal) = CanThrow :-
    Goal = if_then_else(_, Cond, Then, Else),
    ( goal_can_throw_func(MaybeModuleInfo, Cond) = yes ->
        CanThrow = yes
    ; goal_can_throw_func(MaybeModuleInfo, Then) = yes ->
        CanThrow = yes
    ; goal_can_throw_func(MaybeModuleInfo, Else) = yes ->
        CanThrow = yes
    ;
        CanThrow = no
    ).
goal_expr_can_throw(_MaybeModuleInfo, Goal) = CanThrow :-
    Goal = call_foreign_proc(Attributes, _, _, _, _, _, _),
    ExceptionStatus = get_may_throw_exception(Attributes),
    (
        (
            ExceptionStatus = proc_will_not_throw_exception
        ;
            ExceptionStatus = default_exception_behaviour,
            get_may_call_mercury(Attributes) = proc_will_not_call_mercury
        )
    ->
        CanThrow = no
    ;
        CanThrow = yes
    ).
goal_expr_can_throw(MaybeModuleInfo, Goal) = CanThrow :-
    Goal = plain_call(PredId, ProcId, _, _, _, _),
    (
        MaybeModuleInfo = yes(ModuleInfo),
        module_info_get_exception_info(ModuleInfo, ExceptionInfo),
        map.search(ExceptionInfo, proc(PredId, ProcId), ProcExceptionInfo),
        ProcExceptionInfo = proc_exception_info(will_not_throw, _)
    ->
        CanThrow = no
    ;
        CanThrow = yes
    ).
goal_expr_can_throw(_MaybeModuleInfo, Goal) = yes :-
    % We have no idea whether the called goal can throw exceptions,
    % at least without closure analysis.
    Goal = generic_call(_, _, _, _).
goal_expr_can_throw(_, unify(_, _, _, Uni, _)) = CanThrow :-
    % Complicated unifies are _non_builtin_
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
    ).
goal_expr_can_throw(_, shorthand(_)) = _ :-
    unexpected(this_file, "goal_expr_can_throw: shorthand").

:- func goal_list_can_throw(maybe(module_info), list(hlds_goal)) = bool.

goal_list_can_throw(_, []) = no.
goal_list_can_throw(MaybeModuleInfo, [Goal | Goals]) =
    ( goal_can_throw_func(MaybeModuleInfo, Goal) = yes ->
        yes
    ;
        goal_list_can_throw(MaybeModuleInfo, Goals)
    ).

:- func case_list_can_throw(maybe(module_info), list(case)) = bool.

case_list_can_throw(_, []) = no.
case_list_can_throw(MaybeModuleInfo, [case(_, Goal) | Cases]) =
    ( goal_can_throw_func(MaybeModuleInfo, Goal) = yes ->
        yes
    ;
        case_list_can_throw(MaybeModuleInfo, Cases)
    ).

%-----------------------------------------------------------------------------%

goal_is_flat(Goal - _GoalInfo) = goal_is_flat_expr(Goal).

:- func goal_is_flat_expr(hlds_goal_expr) = bool.

goal_is_flat_expr(generic_call(_, _, _, _)) = yes.
goal_is_flat_expr(plain_call(_, _, _, _, _, _)) = yes.
goal_is_flat_expr(unify(_, _, _, _, _)) = yes.
goal_is_flat_expr(call_foreign_proc(_, _, _, _, _, _, _)) = yes.
goal_is_flat_expr(conj(ConjType, Goals)) = IsFlat :-
    (
        ConjType = parallel_conj,
        IsFlat = no
    ;
        ConjType = plain_conj,
        IsFlat = goal_is_flat_list(Goals)
    ).
goal_is_flat_expr(disj(_)) = no.
goal_is_flat_expr(switch(_, _, _)) = no.
goal_is_flat_expr(if_then_else(_, _, _, _)) = no.
goal_is_flat_expr(negation(Goal)) = goal_is_flat(Goal).
goal_is_flat_expr(scope(_, Goal)) = goal_is_flat(Goal).
goal_is_flat_expr(shorthand(_)) = no.

:- func goal_is_flat_list(list(hlds_goal)) = bool.

goal_is_flat_list([]) = yes.
goal_is_flat_list([Goal | Goals]) = IsFlat :-
    ( goal_is_flat(Goal) = yes ->
        IsFlat = goal_is_flat_list(Goals)
    ;
        IsFlat = no
    ).

%-----------------------------------------------------------------------------%

goal_may_allocate_heap(Goal) :-
    goal_may_allocate_heap(Goal, yes).

goal_list_may_allocate_heap(Goals) :-
    goal_list_may_allocate_heap(Goals, yes).

:- pred goal_may_allocate_heap(hlds_goal::in, bool::out) is det.

goal_may_allocate_heap(Goal - _GoalInfo, May) :-
    goal_may_allocate_heap_2(Goal, May).

:- pred goal_may_allocate_heap_2(hlds_goal_expr::in, bool::out) is det.

goal_may_allocate_heap_2(generic_call(_, _, _, _), yes).
goal_may_allocate_heap_2(plain_call(_, _, _, Builtin, _, _), May) :-
    ( Builtin = inline_builtin ->
        May = no
    ;
        May = yes
    ).
goal_may_allocate_heap_2(unify(_, _, _, Unification, _), May) :-
    (
        Unification = construct(_, _, Args, _, _, _, _),
        Args = [_ | _]
    ->
        May = yes
    ;
        May = no
    ).
    % We cannot safely say that a foreign code fragment does not
    % allocate memory without knowing all the #defined macros that
    % expand to incr_hp and variants thereof.
    % XXX You could make it an attribute of the foreign code and
    % trust the programmer.
goal_may_allocate_heap_2(call_foreign_proc(_, _, _, _, _, _, _), yes).
goal_may_allocate_heap_2(scope(_, Goal), May) :-
    goal_may_allocate_heap(Goal, May).
goal_may_allocate_heap_2(negation(Goal), May) :-
    goal_may_allocate_heap(Goal, May).
goal_may_allocate_heap_2(conj(ConjType, Goals), May) :-
    (
        ConjType = parallel_conj,
        May = yes
    ;
        ConjType = plain_conj,
        goal_list_may_allocate_heap(Goals, May)
    ).
goal_may_allocate_heap_2(disj(Goals), May) :-
    goal_list_may_allocate_heap(Goals, May).
goal_may_allocate_heap_2(switch(_Var, _Det, Cases), May) :-
    cases_may_allocate_heap(Cases, May).
goal_may_allocate_heap_2(if_then_else(_Vars, C, T, E), May) :-
    ( goal_may_allocate_heap(C, yes) ->
        May = yes
    ; goal_may_allocate_heap(T, yes) ->
        May = yes
    ;
        goal_may_allocate_heap(E, May)
    ).
goal_may_allocate_heap_2(shorthand(ShorthandGoal), May) :-
    goal_may_allocate_heap_2_shorthand(ShorthandGoal, May).

:- pred goal_may_allocate_heap_2_shorthand(shorthand_goal_expr::in, bool::out)
    is det.

goal_may_allocate_heap_2_shorthand(bi_implication(G1, G2), May) :-
    ( goal_may_allocate_heap(G1, yes) ->
        May = yes
    ;
        goal_may_allocate_heap(G2, May)
    ).

:- pred goal_list_may_allocate_heap(list(hlds_goal)::in, bool::out) is det.

goal_list_may_allocate_heap([], no).
goal_list_may_allocate_heap([Goal | Goals], May) :-
    ( goal_may_allocate_heap(Goal, yes) ->
        May = yes
    ;
        goal_list_may_allocate_heap(Goals, May)
    ).

:- pred cases_may_allocate_heap(list(case)::in, bool::out) is det.

cases_may_allocate_heap([], no).
cases_may_allocate_heap([case(_, Goal) | Cases], May) :-
    ( goal_may_allocate_heap(Goal, yes) ->
        May = yes
    ;
        cases_may_allocate_heap(Cases, May)
    ).

%-----------------------------------------------------------------------------%

cannot_stack_flush(GoalExpr - _) :-
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
cannot_stack_flush_2(negation(unify(_, _, _, Unify, _) - _)) :-
    Unify \= complicated_unify(_, _, _).

:- pred cannot_stack_flush_goals(list(hlds_goal)::in) is semidet.

cannot_stack_flush_goals([]).
cannot_stack_flush_goals([Goal | Goals]) :-
    cannot_stack_flush(Goal),
    cannot_stack_flush_goals(Goals).

:- pred cannot_stack_flush_cases(list(case)::in) is semidet.

cannot_stack_flush_cases([]).
cannot_stack_flush_cases([case(_, Goal) | Cases]) :-
    cannot_stack_flush(Goal),
    cannot_stack_flush_cases(Cases).

%-----------------------------------------------------------------------------%

cannot_fail_before_stack_flush(GoalExpr - GoalInfo) :-
    goal_info_get_determinism(GoalInfo, Detism),
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
    Goal = GoalExpr - GoalInfo,
    (
        (
            GoalExpr = plain_call(_, _, _, BuiltinState, _, _),
            BuiltinState \= inline_builtin
        ;
            GoalExpr = generic_call(_, _, _, _)
        )
    ->
        true
    ;
        goal_info_get_determinism(GoalInfo, Detism),
        determinism_components(Detism, cannot_fail, _)
    ->
        cannot_fail_before_stack_flush_conj(Goals)
    ;
        fail
    ).

%-----------------------------------------------------------------------------%

count_recursive_calls(Goal - _, PredId, ProcId, Min, Max) :-
    count_recursive_calls_2(Goal, PredId, ProcId, Min, Max).

:- pred count_recursive_calls_2(hlds_goal_expr::in, pred_id::in, proc_id::in,
    int::out, int::out) is det.

count_recursive_calls_2(negation(Goal), PredId, ProcId, Min, Max) :-
    count_recursive_calls(Goal, PredId, ProcId, Min, Max).
count_recursive_calls_2(scope(_, Goal), PredId, ProcId, Min, Max) :-
    count_recursive_calls(Goal, PredId, ProcId, Min, Max).
count_recursive_calls_2(unify(_, _, _, _, _), _, _, 0, 0).
count_recursive_calls_2(generic_call(_, _, _, _), _, _, 0, 0).
count_recursive_calls_2(call_foreign_proc(_, _, _, _, _, _, _), _, _, 0, 0).
count_recursive_calls_2(plain_call(CallPredId, CallProcId, _, _, _, _),
        PredId, ProcId, Count, Count) :-
    (
        PredId = CallPredId,
        ProcId = CallProcId
    ->
        Count = 1
    ;
        Count = 0
    ).
count_recursive_calls_2(conj(_, Goals), PredId, ProcId, Min, Max) :-
    count_recursive_calls_conj(Goals, PredId, ProcId, 0, 0, Min, Max).
count_recursive_calls_2(disj(Goals), PredId, ProcId, Min, Max) :-
    count_recursive_calls_disj(Goals, PredId, ProcId, Min, Max).
count_recursive_calls_2(switch(_, _, Cases), PredId, ProcId, Min, Max) :-
    count_recursive_calls_cases(Cases, PredId, ProcId, Min, Max).
count_recursive_calls_2(if_then_else(_, Cond, Then, Else), PredId, ProcId,
        Min, Max) :-
    count_recursive_calls(Cond, PredId, ProcId, CMin, CMax),
    count_recursive_calls(Then, PredId, ProcId, TMin, TMax),
    count_recursive_calls(Else, PredId, ProcId, EMin, EMax),
    CTMin = CMin + TMin,
    CTMax = CMax + TMax,
    int.min(CTMin, EMin, Min),
    int.max(CTMax, EMax, Max).
count_recursive_calls_2(shorthand(_), _, _, _, _) :-
    % these should have been expanded out by now
    unexpected(this_file, "count_recursive_calls_2: unexpected shorthand").

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
    unexpected(this_file, "empty cases in count_recursive_calls_cases").
count_recursive_calls_cases([case(_, Goal) | Cases], PredId, ProcId,
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
    ( goal_info_has_feature(GoalInfo, feature_will_not_modify_trail) ->
        yes
    ;
        no
    ).

goal_may_modify_trail(GoalInfo) = bool.not(goal_cannot_modify_trail(GoalInfo)).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "goal_form.m".

%-----------------------------------------------------------------------------%
:- end_module goal_form.
%-----------------------------------------------------------------------------%
