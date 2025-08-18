%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mark_trace_goals.m.
%
% This module ensures that goals have the contains_trace_goals feature
% if and only if they actually contain trace goals.
%
%---------------------------------------------------------------------------%

:- module check_hlds.simplify.mark_trace_goals.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

    % Most of the time, the contexts in a predicate body will all be from
    % a single file, but just in case they are not, we record the maximum
    % line number we have seen from a non-trace atomic goal for *every*
    % file name we have seen.
    %
:- type last_nontrace_map == map(string, int).

:- pred set_goal_contains_trace_features_in_goal(hlds_goal::in, hlds_goal::out,
    contains_trace_goal::out, last_nontrace_map::in, last_nontrace_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module assoc_list.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module term_context.

%---------------------------------------------------------------------------%

set_goal_contains_trace_features_in_goal(Goal0, Goal, ContainsTrace,
        !LastNonTraceMap, !Specs) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0,
        ContainsTrace = contains_no_trace_goal,

        goal_info_get_context(GoalInfo0) = context(FileName, LineNumber),
        ( if map.search(!.LastNonTraceMap, FileName, MaxLineNumber0) then
            ( if LineNumber > MaxLineNumber0 then
                map.det_update(FileName, LineNumber, !LastNonTraceMap)
            else
                true
            )
        else
            map.det_insert(FileName, LineNumber, !LastNonTraceMap)
        )
    ;
        GoalExpr0 = conj(ConjType, SubGoals0),
        ContainsTrace0 = contains_no_trace_goal,
        set_goal_contains_trace_features_in_conj(SubGoals0, SubGoals,
            ContainsTrace0, ContainsTrace, !LastNonTraceMap, !Specs),
        GoalExpr = conj(ConjType, SubGoals)
    ;
        GoalExpr0 = disj(SubGoals0),
        ContainsTrace0 = contains_no_trace_goal,
        set_goal_contains_trace_features_in_disj(SubGoals0, SubGoals,
            ContainsTrace0, ContainsTrace,
            !.LastNonTraceMap, [], EndLastNonTraceMaps, !Specs),
        rejoin_last_nontrace_maps(EndLastNonTraceMaps, !:LastNonTraceMap),
        GoalExpr = disj(SubGoals)
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        ContainsTrace0 = contains_no_trace_goal,
        set_goal_contains_trace_features_in_cases(Cases0, Cases,
            ContainsTrace0, ContainsTrace,
            !.LastNonTraceMap, [], EndLastNonTraceMaps, !Specs),
        rejoin_last_nontrace_maps(EndLastNonTraceMaps, !:LastNonTraceMap),
        GoalExpr = switch(SwitchVar, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        set_goal_contains_trace_features_in_goal(Cond0, Cond,
            CondContainsTrace,
            !.LastNonTraceMap, LastNonTraceMapAfterCond, !Specs),
        set_goal_contains_trace_features_in_goal(Then0, Then,
            ThenContainsTrace,
            LastNonTraceMapAfterCond, LastNonTraceMapAfterThen, !Specs),
        set_goal_contains_trace_features_in_goal(Else0, Else,
            ElseContainsTrace,
            !.LastNonTraceMap, LastNonTraceMapAfterElse, !Specs),
        ContainsTrace =
            worst_contains_trace(CondContainsTrace,
            worst_contains_trace(ThenContainsTrace, ElseContainsTrace)),
        rejoin_last_nontrace_maps(
            [LastNonTraceMapAfterThen, LastNonTraceMapAfterElse],
            !:LastNonTraceMap),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(SubGoal0),
        set_goal_contains_trace_features_in_goal(SubGoal0, SubGoal,
            ContainsTrace, !.LastNonTraceMap, _, !Specs),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        (
            Reason = trace_goal(_, _, _, _, _),
            SubGoal = SubGoal0,
            ContainsTrace = contains_trace_goal,

            Context = goal_info_get_context(GoalInfo0),
            Context = context(FileName, LineNumber),
            ( if
                map.search(!.LastNonTraceMap, FileName, LastLineNumber),
                LastLineNumber > LineNumber
            then
                Phase = phase_simplify(report_in_any_mode),
                TracePieces = [words("Warning: this trace goal was")] ++
                    color_as_hint([words("reordered")]) ++
                    [words("to execute after some goals that follow it"),
                    words("in the text of the program."), nl],
                LastPieces = [words("This is the location"),
                    words("of the last primitive goal that"),
                    words("this trace goal was moved after."),
                    % XXX This could probably be worded better, but
                    % it is intrinsically a somewhat-complicated concept
                    % to get across.
                    words("Note that the trace goal would have been moved"),
                    words("only within its original conjunction,"),
                    words("and this goal may be only part of the conjunct"),
                    words("that the trace goal was moved after."), nl],
                TraceMsg = msg(Context, TracePieces),
                LastMsg = msg(context(FileName, LastLineNumber), LastPieces),
                Severity = severity_warning(warn_moved_trace_goal),
                Spec = error_spec($pred, Severity, Phase, [TraceMsg, LastMsg]),
                !:Specs = [Spec | !.Specs]
            else
                true
            )
        ;
            Reason = from_ground_term(_, FGT),
            (
                ( FGT = from_ground_term_construct
                ; FGT = from_ground_term_deconstruct
                ),
                SubGoal = SubGoal0,
                ContainsTrace = contains_no_trace_goal
            ;
                ( FGT = from_ground_term_initial
                ; FGT = from_ground_term_other
                ),
                set_goal_contains_trace_features_in_goal(SubGoal0, SubGoal,
                    ContainsTrace, !LastNonTraceMap, !Specs)
            )
        ;
            ( Reason = exist_quant(_, _)
            ; Reason = disable_warnings(_, _)
            ; Reason = promise_solutions(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = loop_control(_, _, _)
            ),
            set_goal_contains_trace_features_in_goal(SubGoal0, SubGoal,
                ContainsTrace, !LastNonTraceMap, !Specs)
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            set_goal_contains_trace_features_in_goal(MainGoal0, MainGoal,
                MainContainsTrace,
                !.LastNonTraceMap, LastNonTraceMapAfterMain, !Specs),
            OrElseContainsTrace0 = contains_no_trace_goal,
            set_goal_contains_trace_features_in_disj(
                OrElseGoals0, OrElseGoals,
                OrElseContainsTrace0, OrElseContainsTrace,
                !.LastNonTraceMap,
                [LastNonTraceMapAfterMain], LastNonTraceMaps, !Specs),
            ContainsTrace = worst_contains_trace(MainContainsTrace,
                OrElseContainsTrace),
            rejoin_last_nontrace_maps(LastNonTraceMaps, !:LastNonTraceMap),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners),
            GoalExpr = shorthand(ShortHand)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            set_goal_contains_trace_features_in_goal(SubGoal0, SubGoal,
                ContainsTrace, !LastNonTraceMap, !Specs),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal),
            GoalExpr = shorthand(ShortHand)
        ;
            ShortHand0 = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        )
    ),
    (
        ContainsTrace = contains_trace_goal,
        goal_info_add_feature(feature_contains_trace, GoalInfo0, GoalInfo)
    ;
        ContainsTrace = contains_no_trace_goal,
        goal_info_remove_feature(feature_contains_trace, GoalInfo0, GoalInfo)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%---------------------%

:- pred set_goal_contains_trace_features_in_conj(
    list(hlds_goal)::in, list(hlds_goal)::out,
    contains_trace_goal::in, contains_trace_goal::out,
    last_nontrace_map::in, last_nontrace_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

set_goal_contains_trace_features_in_conj([], [], !ContainsTrace,
        !LastNonTraceMap, !Specs).
set_goal_contains_trace_features_in_conj([Goal0 | Goals0], [Goal | Goals],
        !ContainsTrace, !LastNonTraceMap, !Specs) :-
    set_goal_contains_trace_features_in_goal(Goal0, Goal, GoalContainsTrace,
        !LastNonTraceMap, !Specs),
    !:ContainsTrace = worst_contains_trace(GoalContainsTrace, !.ContainsTrace),
    set_goal_contains_trace_features_in_conj(Goals0, Goals, !ContainsTrace,
        !LastNonTraceMap, !Specs).

%---------------------%

:- pred set_goal_contains_trace_features_in_disj(
    list(hlds_goal)::in, list(hlds_goal)::out,
    contains_trace_goal::in, contains_trace_goal::out,
    last_nontrace_map::in,
    list(last_nontrace_map)::in, list(last_nontrace_map)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

set_goal_contains_trace_features_in_disj([], [], !ContainsTrace,
        _LastNonTraceMap0, !LastNonTraceMaps, !Specs).
set_goal_contains_trace_features_in_disj([Goal0 | Goals0], [Goal | Goals],
        !ContainsTrace, LastNonTraceMap0, !LastNonTraceMaps, !Specs) :-
    set_goal_contains_trace_features_in_goal(Goal0, Goal, GoalContainsTrace,
        LastNonTraceMap0, GoalLastNonTraceMap, !Specs),
    !:ContainsTrace = worst_contains_trace(GoalContainsTrace, !.ContainsTrace),
    !:LastNonTraceMaps = [GoalLastNonTraceMap | !.LastNonTraceMaps],
    set_goal_contains_trace_features_in_disj(Goals0, Goals, !ContainsTrace,
        LastNonTraceMap0, !LastNonTraceMaps, !Specs).

%---------------------%

:- pred set_goal_contains_trace_features_in_cases(
    list(case)::in, list(case)::out,
    contains_trace_goal::in, contains_trace_goal::out,
    last_nontrace_map::in,
    list(last_nontrace_map)::in, list(last_nontrace_map)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

set_goal_contains_trace_features_in_cases([], [], !ContainsTrace,
        _LastNonTraceMap0, !LastNonTraceMaps, !Specs).
set_goal_contains_trace_features_in_cases([Case0 | Cases0], [Case | Cases],
        !ContainsTrace, LastNonTraceMap0, !LastNonTraceMaps, !Specs) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    set_goal_contains_trace_features_in_goal(Goal0, Goal, GoalContainsTrace,
        LastNonTraceMap0, GoalLastNonTraceMap, !Specs),
    Case = case(MainConsId, OtherConsIds, Goal),
    !:ContainsTrace = worst_contains_trace(GoalContainsTrace, !.ContainsTrace),
    !:LastNonTraceMaps = [GoalLastNonTraceMap | !.LastNonTraceMaps],
    set_goal_contains_trace_features_in_cases(Cases0, Cases, !ContainsTrace,
        LastNonTraceMap0, !LastNonTraceMaps, !Specs).

%---------------------------------------------------------------------------%

:- pred rejoin_last_nontrace_maps(list(last_nontrace_map)::in,
    last_nontrace_map::out) is det.

rejoin_last_nontrace_maps(LastNonTraceMaps, LastNonTraceMap) :-
    list.map(map.to_assoc_list, LastNonTraceMaps, LastNonTraceMapALs),
    list.condense(LastNonTraceMapALs, LastNonTraceMapAL),
    % Sort all the filename - linenumber pairs. This brings all the entries
    % with the same filename next to each other, in ascending order on both
    % filename and linenumber.
    list.sort(LastNonTraceMapAL, SortedLastNonTraceMapAL),
    % Put the list in descending order by filename (which is irrelevant)
    % and line number (which we do want).
    list.reverse(SortedLastNonTraceMapAL, RevSortedLastNonTraceMapAL),
    map.init(LastNonTraceMap0),
    (
        RevSortedLastNonTraceMapAL = [],
        LastNonTraceMap = LastNonTraceMap0
    ;
        RevSortedLastNonTraceMapAL =
            [HeadFileName - HeadLineNumber | TailFileNameLineNumbers],
        map.det_insert(HeadFileName, HeadLineNumber,
            LastNonTraceMap0, LastNonTraceMap1),
        build_rejoined_last_nontrace_map(HeadFileName, TailFileNameLineNumbers,
            LastNonTraceMap1, LastNonTraceMap)
    ).

:- pred build_rejoined_last_nontrace_map(
    string::in, assoc_list(string, int)::in,
    last_nontrace_map::in, last_nontrace_map::out) is det.

build_rejoined_last_nontrace_map(_, [], !LastNonTraceMap).
build_rejoined_last_nontrace_map(PrevFileName,
        [FileName - LineNumber | TailFileNameLineNumbers], !LastNonTraceMap) :-
    ( if FileName = PrevFileName then
        % Since we are processing pairs in descending order of line number,
        % !.LastNonTraceMap already contains the highest line number we have
        % seen so far for FileName.
        true
    else
        map.det_insert(FileName, LineNumber, !LastNonTraceMap)
    ),
    build_rejoined_last_nontrace_map(FileName, TailFileNameLineNumbers,
        !LastNonTraceMap).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.mark_trace_goals.
%---------------------------------------------------------------------------%
