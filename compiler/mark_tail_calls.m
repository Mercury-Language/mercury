%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2008, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mark_tail_calls.m.
% Main author: zs.
%
% This module adds a feature to all self-recursive calls that can be
% implemented as tail calls.
%
% Since an assignment unification that simply renames an output of a recursive
% call may prevent that call from being recognized as a tail call, you probably
% want to run excess assign elimination just before invoking this module.
%
% This module also contains a pass that detects predicates which are directly
% recursive, but not tail-recursive, and warns about them.
%
%-----------------------------------------------------------------------------%

:- module hlds.mark_tail_calls.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.globals.

:- import_module io.

:- pred mark_tail_calls(goal_feature::in, module_info::in, pred_proc_id::in,
    pred_info::in, proc_info::in, proc_info::out) is det.

:- pred warn_non_tail_calls(module_info::in, io::di, io::uo) is det.

:- pred warn_non_tail_calls_in_proc(globals::in, pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module solutions.

%-----------------------------------------------------------------------------%

:- type mark_tail_calls_info
    --->    mark_tail_calls_info(
                mtc_feature     :: goal_feature,
                mtc_module      :: module_info,
                mtc_pred_id     :: pred_id,
                mtc_proc_id     :: proc_id,
                mtc_vartypes    :: vartypes
            ).

:- type found_tail_calls
    --->    found_tail_calls
    ;       not_found_tail_calls.

mark_tail_calls(Feature, ModuleInfo, proc(PredId, ProcId), PredInfo,
        !ProcInfo) :-
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
        proc_info_get_vartypes(!.ProcInfo, VarTypes),
        find_maybe_output_args(ModuleInfo, Types, Modes, HeadVars, Outputs),
        Info = mark_tail_calls_info(Feature, ModuleInfo, PredId, ProcId,
            VarTypes),
        mark_tail_calls_in_goal(Info, Outputs, _, Goal0, Goal,
            not_found_tail_calls, FoundTailCalls),
        proc_info_set_goal(Goal, !ProcInfo),
        (
            FoundTailCalls = found_tail_calls,
            TailCallEvents = has_tail_call_event
        ;
            FoundTailCalls = not_found_tail_calls,
            TailCallEvents = has_no_tail_call_event
        ),
        proc_info_set_has_tail_call_event(TailCallEvents, !ProcInfo)
    ).

:- pred find_maybe_output_args(module_info::in,
     list(mer_type)::in, list(mer_mode)::in, list(prog_var)::in,
     list(maybe(prog_var))::out) is det.

find_maybe_output_args(ModuleInfo, Types, Modes, Vars, Outputs) :-
    ( find_maybe_output_args_2(ModuleInfo, Types, Modes, Vars, OutputsPrime) ->
        Outputs = OutputsPrime
    ;
        unexpected($module, $pred, "list length mismatch")
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
        IsDummy = check_dummy_type(ModuleInfo, Type),
        (
            IsDummy = is_not_dummy_type,
            OutputVar = yes(Var)
        ;
            IsDummy = is_dummy_type,
            OutputVar = no
        )
    ),
    find_maybe_output_args_2(ModuleInfo, Types, Modes, Vars, OutputVars).

%-----------------------------------------------------------------------------%

    % mark_tail_calls_in_goal(Info, Outputs0, MaybeOutputs, Goal0, Goal,
    %   !FoundTailCalls):
    %
    % This predicate transforms Goal0 into Goal by marking all tail calls
    % in it with the feature in Info. Tailcalls are calls to the pred_id
    % and proc_id in Info, in which the variables of the argument list match
    % the corresponding variables in the elements of the Outputs list that
    % actually contain a variable.
    %
    % If Goal0 neither is a tailcall nor contains a tailcall, but could
    % actually follow a tailcall (which is possible if it is either an
    % assignment unification that simply renames an output variable,
    % or a conjunction of such unifications), then return MaybeOutputs
    % as copy of Outputs0 updated to account for the renaming. Otherwise,
    % return 'no' for MaybeOutputs.
    %
:- pred mark_tail_calls_in_goal(mark_tail_calls_info::in,
    list(maybe(prog_var))::in, maybe(list(maybe(prog_var)))::out,
    hlds_goal::in, hlds_goal::out, found_tail_calls::in, found_tail_calls::out)
    is det.

mark_tail_calls_in_goal(Info, Outputs0, MaybeOutputs, Goal0, Goal,
        !FoundTailCalls) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = scope(_, _)
        ; GoalExpr0 = negation(_)
        ),
        MaybeOutputs = no,
        Goal = Goal0
    ;
        GoalExpr0 = unify(LHS, _, _, Unify0, _),
        Goal = Goal0,
        ModuleInfo = Info ^ mtc_module,
        VarTypes = Info ^ mtc_vartypes,
        ( var_is_of_dummy_type(ModuleInfo, VarTypes, LHS) ->
            % Unifications involving dummy type variables are no-ops,
            % and do not inhibit a preceding tail call.
            MaybeOutputs = yes(Outputs0)
        ;
            (
                ( Unify0 = construct(_, _, _, _, _, _, _)
                ; Unify0 = deconstruct(_, _, _, _, _, _)
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
        )
    ;
        GoalExpr0 = plain_call(CallPredId, CallProcId, Args, Builtin,
            _UnifyContext, _SymName),
        MaybeOutputs = no,
        PredId = Info ^ mtc_pred_id,
        ProcId = Info ^ mtc_proc_id,
        (
            CallPredId = PredId,
            CallProcId = ProcId,
            match_output_args(Outputs0, Args),
            Builtin = not_builtin
        ->
            Feature = Info ^ mtc_feature,
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
            mark_tail_calls_in_conj(Info, Outputs0, MaybeOutputs,
                RevGoals0, RevGoals, !FoundTailCalls),
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
        mark_tail_calls_in_goals(Info, Outputs0, Goals0, Goals,
            !FoundTailCalls),
        MaybeOutputs = no,
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        mark_tail_calls_in_cases(Info, Outputs0, Cases0, Cases,
            !FoundTailCalls),
        MaybeOutputs = no,
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond, Then0, Else0),
        mark_tail_calls_in_goal(Info, Outputs0, _, Then0, Then,
            !FoundTailCalls),
        mark_tail_calls_in_goal(Info, Outputs0, _, Else0, Else,
            !FoundTailCalls),
        MaybeOutputs = no,
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = shorthand(_),
        unexpected($module, $pred, "shorthand")
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
        MaybeVar = no,
        is_output_arg_rename(ToVar, FromVar, MaybeVars0, MaybeVars)
    ).

:- pred mark_tail_calls_in_goals(mark_tail_calls_info::in,
    list(maybe(prog_var))::in, list(hlds_goal)::in, list(hlds_goal)::out,
    found_tail_calls::in, found_tail_calls::out) is det.

mark_tail_calls_in_goals(_Info, _Outputs0, [], [], !FoundTailCalls).
mark_tail_calls_in_goals(Info, Outputs0, [Goal0 | Goals0], [Goal | Goals],
        !FoundTailCalls) :-
    mark_tail_calls_in_goal(Info, Outputs0, _, Goal0, Goal, !FoundTailCalls),
    mark_tail_calls_in_goals(Info, Outputs0, Goals0, Goals, !FoundTailCalls).

:- pred mark_tail_calls_in_cases(mark_tail_calls_info::in,
    list(maybe(prog_var))::in, list(case)::in, list(case)::out,
    found_tail_calls::in, found_tail_calls::out) is det.

mark_tail_calls_in_cases(_Info, _Outputs0, [], [], !FoundTailCalls).
mark_tail_calls_in_cases(Info, Outputs0, [Case0 | Cases0], [Case | Cases],
        !FoundTailCalls) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    mark_tail_calls_in_goal(Info, Outputs0, _, Goal0, Goal, !FoundTailCalls),
    Case = case(MainConsId, OtherConsIds, Goal),
    mark_tail_calls_in_cases(Info, Outputs0, Cases0, Cases, !FoundTailCalls).

:- pred mark_tail_calls_in_conj(mark_tail_calls_info::in,
    list(maybe(prog_var))::in, maybe(list(maybe(prog_var)))::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    found_tail_calls::in, found_tail_calls::out) is det.

mark_tail_calls_in_conj(_Info, Outputs0, yes(Outputs0),
        [], [], !FoundTailCalls).
mark_tail_calls_in_conj(Info, Outputs0, MaybeOutputs,
        [RevGoal0 | RevGoals0], [RevGoal | RevGoals], !FoundTailCalls) :-
    mark_tail_calls_in_goal(Info, Outputs0, MaybeOutputs1, RevGoal0, RevGoal,
        !FoundTailCalls),
    (
        MaybeOutputs1 = yes(Outputs1),
        mark_tail_calls_in_conj(Info, Outputs1, MaybeOutputs,
            RevGoals0, RevGoals, !FoundTailCalls)
    ;
        MaybeOutputs1 = no,
        MaybeOutputs = no,
        RevGoals = RevGoals0
    ).

:- pred match_output_args(list(maybe(prog_var))::in, list(prog_var)::in)
    is semidet.

match_output_args([], []).
match_output_args([], [_ | _]) :-
    unexpected($module, $pred, "length mismatch").
match_output_args([_ | _], []) :-
    unexpected($module, $pred, "length mismatch").
match_output_args([MaybeOutputVar | MaybeOutputVars], [ArgVar | ArgVars]) :-
    (
        MaybeOutputVar = no
    ;
        MaybeOutputVar = yes(ArgVar)
    ),
    match_output_args(MaybeOutputVars, ArgVars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

warn_non_tail_calls(ModuleInfo, !IO) :-
    solutions.solutions(nontailcall_in_hlds(ModuleInfo), Warnings),
    module_info_get_globals(ModuleInfo, Globals),
    list.foldl(report_nontailcall_warning(Globals), Warnings, !IO).

warn_non_tail_calls_in_proc(Globals, PredId, ProcId, PredInfo, ProcInfo,
        !IO) :-
    solutions.solutions(
        nontailcall_in_proc(PredId, ProcId, PredInfo, ProcInfo), Warnings),
    list.foldl(report_nontailcall_warning(Globals), Warnings, !IO).

:- type tailcall_warning
    --->    tailcall_warning(
                pred_or_func,
                sym_name,
                arity,
                proc_id,
                prog_context
            ).

:- pred nontailcall_in_hlds(module_info::in, tailcall_warning::out) is nondet.

nontailcall_in_hlds(!.ModuleInfo, Warning) :-
    module_info_get_valid_predids(PredIds, !ModuleInfo),
    list.member(PredId, PredIds),
    nontailcall_in_pred(!.ModuleInfo, PredId, Warning).

:- pred nontailcall_in_pred(module_info::in, pred_id::in,
    tailcall_warning::out) is nondet.

nontailcall_in_pred(ModuleInfo, PredId, Warning) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_non_imported_procids(PredInfo),
    list.member(ProcId, ProcIds),
    pred_info_proc_info(PredInfo, ProcId, ProcInfo),
    nontailcall_in_proc(PredId, ProcId, PredInfo, ProcInfo, Warning).

:- pred nontailcall_in_proc(pred_id::in, proc_id::in, pred_info::in,
    proc_info::in, tailcall_warning::out) is nondet.

nontailcall_in_proc(PredId, ProcId, PredInfo, ProcInfo, Warning) :-
    proc_info_get_goal(ProcInfo, Goal),
    goal_contains_goal(Goal, SubGoal),
    SubGoal = hlds_goal(SubGoalExpr, SubGoalInfo),
    SubGoalExpr = plain_call(CallPredId, CallProcId, CallArgs, Builtin,
        _UnifyContext, SymName),
    % Check if this call is a directly recursive call.
    CallPredId = PredId,
    CallProcId = ProcId,
    Builtin = not_builtin,
    not goal_has_feature(SubGoal, feature_debug_tail_rec_call),
    % Don't warn about special predicates.
    not is_unify_or_compare_pred(PredInfo),

    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    list.length(CallArgs, Arity),
    Context = goal_info_get_context(SubGoalInfo),
    Warning = tailcall_warning(PredOrFunc, SymName, Arity, CallProcId,
        Context).

:- pred report_nontailcall_warning(globals::in, tailcall_warning::in,
    io::di, io::uo) is det.

report_nontailcall_warning(Globals, Warning, !IO) :-
    Warning = tailcall_warning(PredOrFunc, SymName, Arity, ProcId, Context),
    Name = unqualify_name(SymName),
    SimpleCallId = simple_call_id(PredOrFunc, unqualified(Name), Arity),
    proc_id_to_int(ProcId, ProcNumber0),
    ProcNumber = ProcNumber0 + 1,
    Pieces =
        [words("In mode number"), int_fixed(ProcNumber),
        words("of"), simple_call(SimpleCallId), suffix(":"), nl,
        words("warning: recursive call is not tail recursive."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_warning, phase_code_gen, [Msg]),
    write_error_spec(Spec, Globals, 0, _NumWarnings, 0, _NumErrors, !IO).

%-----------------------------------------------------------------------------%
:- end_module hlds.mark_tail_calls.
%-----------------------------------------------------------------------------%
