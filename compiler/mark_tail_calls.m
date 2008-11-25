%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mark_tail_calls.m.
% Main author: zs.
%
% This module adds feature_tailcall to all self-recursive calls that can be
% implemented as tail calls.
%
% Since an assignment unification that simply renames an output of a recursive
% call may prevent that call from being recognized as a tail call, you probably
% want to run excess assign elimination just before invoking this module.
%
%-----------------------------------------------------------------------------%

:- module hlds.mark_tail_calls.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- pred mark_tail_calls(goal_feature::in, pred_id::in, proc_id::in,
    module_info::in, pred_info::in, proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.
:- import_module require.

:- type found_tail_calls
    --->    found_tail_calls
    ;       not_found_tail_calls.

mark_tail_calls(Feature, PredId, ProcId, ModuleInfo, PredInfo, !ProcInfo) :-
    pred_info_get_arg_types(PredInfo, Types),
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_interface_determinism(!.ProcInfo, Detism),
    determinism_components(Detism, _CanFail, SolnCount),
    (
        % In at_most_many procedures, we cannot in general know at compile time
        % whether we can delete the current stack frame at a tail call.
        % For at_most_zero procedures, there is no point in handling tail calls
        % specially.
        ( SolnCount = at_most_many
        ; SolnCount = at_most_zero
        )
    ;
        ( SolnCount = at_most_one
        ; SolnCount = at_most_many_cc
        ),
        proc_info_get_argmodes(!.ProcInfo, Modes),
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        find_maybe_output_args(ModuleInfo, Types, Modes, HeadVars, Outputs),
        mark_tail_calls_in_goal(Feature, PredId, ProcId, Outputs, _,
            Goal0, Goal, not_found_tail_calls, FoundTailCalls),
        proc_info_set_goal(Goal, !ProcInfo),
        (
            FoundTailCalls = found_tail_calls,
            TailCallEvents = tail_call_events
        ;
            FoundTailCalls = not_found_tail_calls,
            TailCallEvents = no_tail_call_events
        ),
        proc_info_set_has_tail_call_events(TailCallEvents, !ProcInfo)
    ).

:- pred find_maybe_output_args(module_info::in,
     list(mer_type)::in, list(mer_mode)::in, list(prog_var)::in,
     list(maybe(prog_var))::out) is det.

find_maybe_output_args(ModuleInfo, Types, Modes, Vars, Outputs) :-
    ( find_maybe_output_args_2(ModuleInfo, Types, Modes, Vars, OutputsPrime) ->
        Outputs = OutputsPrime
    ;
        unexpected(this_file, "find_maybe_output_args: list length mismatch")
    ).

:- pred find_maybe_output_args_2(module_info::in,
    list(mer_type)::in, list(mer_mode)::in, list(prog_var)::in,
    list(maybe(prog_var))::out) is semidet.

find_maybe_output_args_2(_, [], [], [], []).
find_maybe_output_args_2(ModuleInfo, [Type | Types], [Mode | Modes],
        [Var | Vars], [OutputVar | OutputVars]) :-
    mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode),
    (
        ( ArgMode = top_in
        ; ArgMode = top_unused
        ),
        OutputVar = no
    ;
        ArgMode = top_out,
        OutputVar = yes(Var)
    ),
    find_maybe_output_args_2(ModuleInfo, Types, Modes, Vars, OutputVars).

%-----------------------------------------------------------------------------%

    % mark_tail_calls_in_goal(Feature, PredId, ProcId, Outputs0, MaybeOutputs,
    %   Goal0, Goal, !FoundTailCalls):
    %
    % This predicate transforms Goal0 into Goal by marking all tail calls
    % in it with Feature. Tailcalls are calls to the given PredId and ProcId
    % in which the variables of the argument list match the corresponding
    % variables in the elements of the Outputs list that actually contain
    % a variable.
    %
    % If Goal0 neither is a tailcall nor contains a tailcall, but could
    % actually follow a tailcall (which is possible if it is either an
    % assignment unification that simply renames an output variable,
    % or a conjunction of such unifications), then return MaybeOutputs
    % as copy of Outputs0 updated to account for the renaming. Otherwise,
    % return 'no' for MaybeOutputs.
    %
:- pred mark_tail_calls_in_goal(goal_feature::in, pred_id::in, proc_id::in,
    list(maybe(prog_var))::in, maybe(list(maybe(prog_var)))::out,
    hlds_goal::in, hlds_goal::out, found_tail_calls::in, found_tail_calls::out)
    is det.

mark_tail_calls_in_goal(Feature, PredId, ProcId, Outputs0, MaybeOutputs,
        Goal0, Goal, !FoundTailCalls) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _)
        ; GoalExpr0 = scope(_, _)
        ; GoalExpr0 = negation(_)
        ),
        MaybeOutputs = no,
        Goal = Goal0
    ;
        GoalExpr0 = unify(_, _, _, Unify0, _),
        Goal = Goal0,
        (
            ( Unify0 = construct(_, _, _, _, _, __, _)
            ; Unify0 = deconstruct(_, _, _, _, __, _)
            ; Unify0 = simple_test(_, _)
            ; Unify0 = complicated_unify(_, _, _)
            ),
            MaybeOutputs = no
        ;
            Unify0 = assign(ToVar, FromVar),
            ( is_output_arg_rename(ToVar, FromVar, Outputs0, Outputs) ->
                MaybeOutputs = yes(Outputs)
            ;
                MaybeOutputs = no
            )
        )
    ;
        GoalExpr0 = plain_call(CallPredId, CallProcId, Args, Builtin,
            _UnifyContext, _SymName),
        MaybeOutputs = no,
        (
            CallPredId = PredId,
            CallProcId = ProcId,
            match_output_args(Outputs0, Args),
            Builtin = not_builtin
        ->
            goal_info_add_feature(Feature, GoalInfo0, GoalInfo),
            Goal = hlds_goal(GoalExpr0, GoalInfo),
            !:FoundTailCalls = found_tail_calls
        ;
            Goal = Goal0
        )
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            list.reverse(Goals0, RevGoals0),
            mark_tail_calls_in_conj(Feature, PredId, ProcId,
                Outputs0, MaybeOutputs, RevGoals0, RevGoals, !FoundTailCalls),
            list.reverse(RevGoals, Goals),
            GoalExpr = conj(ConjType, Goals),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            ConjType = parallel_conj,
            MaybeOutputs = no,
            Goal = Goal0
        )
    ;
        GoalExpr0 = disj(Goals0),
        mark_tail_calls_in_goals(Feature, PredId, ProcId, Outputs0,
            Goals0, Goals, !FoundTailCalls),
        MaybeOutputs = no,
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        mark_tail_calls_in_cases(Feature, PredId, ProcId, Outputs0,
            Cases0, Cases, !FoundTailCalls),
        MaybeOutputs = no,
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond, Then0, Else0),
        mark_tail_calls_in_goal(Feature, PredId, ProcId, Outputs0, _,
            Then0, Then, !FoundTailCalls),
        mark_tail_calls_in_goal(Feature, PredId, ProcId, Outputs0, _,
            Else0, Else, !FoundTailCalls),
        MaybeOutputs = no,
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file, "mark_tail_calls_in_goal: shorthand")
    ).

:- pred is_output_arg_rename(prog_var::in, prog_var::in,
    list(maybe(prog_var))::in, list(maybe(prog_var))::out) is semidet.

is_output_arg_rename(ToVar, FromVar,
        [MaybeVar0 | MaybeVars0], [MaybeVar | MaybeVars]) :-
    (
        MaybeVar0 = yes(ToVar),
        MaybeVar = yes(FromVar),
        MaybeVars = MaybeVars0
    ;
        MaybeVar0 = no,
        MaybeVar = MaybeVar0,
        is_output_arg_rename(ToVar, FromVar, MaybeVars0, MaybeVars)
    ).

:- pred mark_tail_calls_in_goals(goal_feature::in, pred_id::in, proc_id::in,
    list(maybe(prog_var))::in, list(hlds_goal)::in, list(hlds_goal)::out,
    found_tail_calls::in, found_tail_calls::out) is det.

mark_tail_calls_in_goals(_Feature, _PredId, _ProcId, _Outputs0,
        [], [], !FoundTailCalls).
mark_tail_calls_in_goals(Feature, PredId, ProcId, Outputs0,
        [Goal0 | Goals0], [Goal | Goals], !FoundTailCalls) :-
    mark_tail_calls_in_goal(Feature, PredId, ProcId, Outputs0, _, Goal0, Goal,
        !FoundTailCalls),
    mark_tail_calls_in_goals(Feature, PredId, ProcId, Outputs0, Goals0, Goals,
        !FoundTailCalls).

:- pred mark_tail_calls_in_cases(goal_feature::in, pred_id::in, proc_id::in,
    list(maybe(prog_var))::in, list(case)::in, list(case)::out,
    found_tail_calls::in, found_tail_calls::out) is det.

mark_tail_calls_in_cases(_Feature, _PredId, _ProcId, _Outputs0,
        [], [], !FoundTailCalls).
mark_tail_calls_in_cases(Feature, PredId, ProcId, Outputs0,
        [Case0 | Cases0], [Case | Cases], !FoundTailCalls) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    mark_tail_calls_in_goal(Feature, PredId, ProcId, Outputs0, _, Goal0, Goal,
        !FoundTailCalls),
    Case = case(MainConsId, OtherConsIds, Goal),
    mark_tail_calls_in_cases(Feature, PredId, ProcId, Outputs0, Cases0, Cases,
        !FoundTailCalls).

:- pred mark_tail_calls_in_conj(goal_feature::in, pred_id::in, proc_id::in,
    list(maybe(prog_var))::in, maybe(list(maybe(prog_var)))::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    found_tail_calls::in, found_tail_calls::out) is det.

mark_tail_calls_in_conj(_Feature, _PredId, _ProcId, Outputs0, yes(Outputs0),
        [], [], !FoundTailCalls).
mark_tail_calls_in_conj(Feature, PredId, ProcId, Outputs0, MaybeOutputs,
        [RevGoal0 | RevGoals0], [RevGoal | RevGoals], !FoundTailCalls) :-
    mark_tail_calls_in_goal(Feature, PredId, ProcId, Outputs0, MaybeOutputs1,
        RevGoal0, RevGoal, !FoundTailCalls),
    (
        MaybeOutputs1 = yes(Outputs1),
        mark_tail_calls_in_conj(Feature, PredId, ProcId,
            Outputs1, MaybeOutputs, RevGoals0, RevGoals, !FoundTailCalls)
    ;
        MaybeOutputs1 = no,
        MaybeOutputs = no,
        RevGoals = RevGoals0
    ).

:- pred match_output_args(list(maybe(prog_var))::in, list(prog_var)::in)
    is semidet.

match_output_args([], []).
match_output_args([], [_ | _]) :-
    unexpected(this_file, "match_output_args: length mismatch").
match_output_args([_ | _], []) :-
    unexpected(this_file, "match_output_args: length mismatch").
match_output_args([MaybeOutputVar | MaybeOutputVars], [ArgVar | ArgVars]) :-
    (
        MaybeOutputVar = no
    ;
        MaybeOutputVar = yes(ArgVar)
    ),
    match_output_args(MaybeOutputVars, ArgVars).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "mark_tail_calls.m".

%-----------------------------------------------------------------------------%
