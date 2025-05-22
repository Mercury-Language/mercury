%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2015-2019, 2021-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: goal_reorder.m.
%
% This module provides predicates that test whether goals can be reordered
% with respect to one another.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module hlds.goal_reorder.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.var_table.

:- import_module bool.

%---------------------------------------------------------------------------%

    % can_reorder_goals_old(ModuleInfo, VarTable, FullyStrict,
    %   InstmapBeforeGoal1, Goal1, InstmapBeforeGoal2, Goal2):
    %
    % Succeed if the conjunction (Goal1, Goal2) can be reordered to
    % (Goal2, Goal1).
    %
    % NOTE: this version is deprecated. New code should use the following
    % version, because it supports the intermodule-analysis framework.
    %
:- pred can_reorder_goals_old(module_info::in, var_table::in, bool::in,
    instmap::in, hlds_goal::in, instmap::in, hlds_goal::in) is semidet.

:- type can_reorder_goals
    --->    cannot_reorder_goals
    ;       can_reorder_goals.

    % can_reorder_goals(VarTable, FullyStrict, InstmapBeforeGoal1, Goal1,
    %   InstmapBeforeGoal2, Goal2, Result, !ModuleInfo).
    %
    % Return `yes' if the goals can be reordered; otherwise, return 'no'.
    %
    % Goals can be reordered if
    % - the goals are independent;
    % - the goals are not impure; and
    % - the compiler options specifying the programmer's chosen
    %   Mercury semantics permit any changes in termination behaviour
    %   that may result from the reordering.
    %
    % NOTE: new code should use this version as it supports the
    %       intermodule-analysis framework.
    %
:- pred can_reorder_goals(var_table::in, bool::in, instmap::in,
    hlds_goal::in, instmap::in, hlds_goal::in, can_reorder_goals::out,
    module_info::in, module_info::out) is det.

%---------------------%

    % reordering_maintains_termination_old(ModuleInfo, FullyStrict,
    %   Goal1, Goal2).
    %
    % Succeeds if any possible change in termination behaviour from reordering
    % the goals is allowed according to the semantics options.
    % The information computed by termination and exception analyses is used
    % when making this decision.
    %
    % NOTE: this version is deprecated; new code should use the following
    % version because it supports the intermodule-analysis framework.
    %
:- pred reordering_maintains_termination_old(module_info::in, bool::in,
    hlds_goal::in, hlds_goal::in) is semidet.

:- type reorder_maintains_termination
    --->    reorder_maintains_termination
    ;       reorder_does_not_maintain_termination.

    % reordering_maintains_termination(FullyStrict, Goal1, Goal2, Result,
    %   !ModuleInfo).
    %
    % Result is `yes' if any possible change in termination behaviour from
    % reordering the goals is allowed according to the semantics options.
    % The information computed by termination and exception analyses is used
    % when making this decision.
    %
    % NOTE: new code should use this version as it supports the
    % intermodule-analysis framework.
    %
:- pred reordering_maintains_termination(bool::in,
    hlds_goal::in, hlds_goal::in, reorder_maintains_termination::out,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_form.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

%---------------------------------------------------------------------------%

can_reorder_goals_old(ModuleInfo, VarTable, FullyStrict,
        InstmapBeforeEarlierGoal, EarlierGoal,
        InstmapBeforeLaterGoal, LaterGoal) :-
    % The logic here is mostly duplicated in can_reorder_goals below
    % and in pd_can_reorder_goals in pd_util.m.

    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    LaterGoal = hlds_goal(_, LaterGoalInfo),

    % Impure goals and trace goals cannot be reordered.
    goal_info_get_goal_purity(EarlierGoalInfo, EarlierPurity, EarlierTrace),
    goal_info_get_goal_purity(LaterGoalInfo, LaterPurity, LaterTrace),
    EarlierPurity \= purity_impure,
    LaterPurity \= purity_impure,
    EarlierTrace = contains_no_trace_goal,
    LaterTrace = contains_no_trace_goal,

    reordering_maintains_termination_old(ModuleInfo, FullyStrict,
        EarlierGoal, LaterGoal),

    % Don't reorder the goals if the later goal depends on the outputs
    % of the current goal.
    not goal_depends_on_earlier_goal(ModuleInfo, VarTable,
        LaterGoal, EarlierGoal, InstmapBeforeEarlierGoal),

    % Don't reorder the goals if the later goal changes the instantiatedness
    % of any of the non-locals of the earlier goal. This is necessary if the
    % later goal clobbers any of the non-locals of the earlier goal, and
    % avoids rerunning full mode analysis in other cases.
    not goal_depends_on_earlier_goal(ModuleInfo, VarTable,
        EarlierGoal, LaterGoal, InstmapBeforeLaterGoal).

can_reorder_goals(VarTable, FullyStrict, InstmapBeforeEarlierGoal,
        EarlierGoal, InstmapBeforeLaterGoal, LaterGoal, CanReorder,
        !ModuleInfo) :-
    % The logic here is mostly duplicated in can_reorder_goals_old above
    % and in pd_can_reorder_goals in pd_util.m.

    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    LaterGoal = hlds_goal(_, LaterGoalInfo),

    % Impure goals and trace goals cannot be reordered.
    goal_info_get_goal_purity(EarlierGoalInfo, EarlierPurity, EarlierTrace),
    goal_info_get_goal_purity(LaterGoalInfo, LaterPurity, LaterTrace),
    ( if
        ( EarlierPurity = purity_impure
        ; LaterPurity = purity_impure
        ; EarlierTrace = contains_trace_goal
        ; LaterTrace = contains_trace_goal
        )
    then
        CanReorder = cannot_reorder_goals
    else
        reordering_maintains_termination(FullyStrict,
            EarlierGoal, LaterGoal, MaintainsTermination, !ModuleInfo),
        (
            MaintainsTermination = reorder_does_not_maintain_termination,
            CanReorder = cannot_reorder_goals
        ;
            MaintainsTermination = reorder_maintains_termination,
            ( if
                % Don't reorder the goals if the later goal depends on the
                % outputs of the current goal.
                %
                goal_depends_on_earlier_goal(!.ModuleInfo, VarTable,
                    LaterGoal, EarlierGoal, InstmapBeforeEarlierGoal)
            then
                CanReorder = cannot_reorder_goals
            else if
                % Don't reorder the goals if the later goal changes the
                % instantiatedness of any of the non-locals of the earlier
                % goal. This is necessary if the later goal clobbers any of
                % the non-locals of the earlier goal, and avoids rerunning
                % full mode analysis in other cases.
                %
                goal_depends_on_earlier_goal(!.ModuleInfo, VarTable,
                    EarlierGoal, LaterGoal, InstmapBeforeLaterGoal)
            then
                CanReorder = cannot_reorder_goals
            else
                CanReorder = can_reorder_goals
            )
        )
    ).

%---------------------%

reordering_maintains_termination_old(ModuleInfo, FullyStrict,
        EarlierGoal, LaterGoal) :-
    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    LaterGoal = hlds_goal(_, LaterGoalInfo),

    EarlierDetism = goal_info_get_determinism(EarlierGoalInfo),
    determinism_components(EarlierDetism, EarlierCanFail, _),
    LaterDetism = goal_info_get_determinism(LaterGoalInfo),
    determinism_components(LaterDetism, LaterCanFail, _),

    % If --fully-strict was specified, don't convert (can_loop, can_fail)
    % into (can_fail, can_loop).
    ( if
        FullyStrict = yes,
        not goal_cannot_loop_or_throw(EarlierGoal)
    then
        LaterCanFail = cannot_fail
    else
        true
    ),
    % Don't convert (can_fail, can_loop) into (can_loop, can_fail), since
    % this could worsen the termination properties of the program.
    (
        EarlierCanFail = can_fail,
        goal_cannot_loop_or_throw_term_info(ModuleInfo, LaterGoal)
    ;
        EarlierCanFail = cannot_fail
    ).

reordering_maintains_termination(FullyStrict, EarlierGoal, LaterGoal,
        MaintainsTermination, !ModuleInfo) :-
    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    LaterGoal = hlds_goal(_, LaterGoalInfo),

    EarlierDetism = goal_info_get_determinism(EarlierGoalInfo),
    determinism_components(EarlierDetism, EarlierCanFail, _),
    LaterDetism = goal_info_get_determinism(LaterGoalInfo),
    determinism_components(LaterDetism, LaterCanFail, _),

    % If --fully-strict was specified, don't convert (can_loop, can_fail) into
    % (can_fail, can_loop).
    goal_can_loop_or_throw_imaf(EarlierGoal, EarlierCanLoopOrThrow,
        !ModuleInfo),
    ( if
        FullyStrict = yes,
        EarlierCanLoopOrThrow = can_loop_or_throw,
        LaterCanFail = can_fail
    then
        MaintainsTermination = reorder_does_not_maintain_termination
    else
        % Don't convert (can_fail, can_loop) into (can_loop, can_fail), since
        % this could worsen the termination properties of the program.
        goal_can_loop_or_throw_imaf(LaterGoal, LaterCanLoopOrThrow,
            !ModuleInfo),
        ( if
            EarlierCanFail = can_fail,
            LaterCanLoopOrThrow = can_loop_or_throw
        then
            MaintainsTermination = reorder_does_not_maintain_termination
        else
            MaintainsTermination = reorder_maintains_termination
        )
    ).

%---------------------%

    % If the earlier goal changes the instantiatedness of a variable
    % that is used in the later goal, then the later goal depends on
    % the earlier goal.
    %
    % This code does work on the alias branch.
    %
:- pred goal_depends_on_earlier_goal(module_info::in, var_table::in,
    hlds_goal::in, hlds_goal::in, instmap::in) is semidet.

goal_depends_on_earlier_goal(ModuleInfo, VarTable,
        LaterGoal, EarlierGoal, InstMapBeforeEarlierGoal) :-
    LaterGoal = hlds_goal(_, LaterGoalInfo),
    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    EarlierInstMapDelta = goal_info_get_instmap_delta(EarlierGoalInfo),
    apply_instmap_delta(EarlierInstMapDelta,
        InstMapBeforeEarlierGoal, InstMapAfterEarlierGoal),
    instmap_changed_vars(ModuleInfo, VarTable,
        InstMapBeforeEarlierGoal, InstMapAfterEarlierGoal,
        EarlierChangedVars),
    LaterGoalNonLocals = goal_info_get_nonlocals(LaterGoalInfo),
    set_of_var.intersect(EarlierChangedVars, LaterGoalNonLocals, Intersection),
    not set_of_var.is_empty(Intersection).

%---------------------------------------------------------------------------%
:- end_module hlds.goal_reorder.
%---------------------------------------------------------------------------%
