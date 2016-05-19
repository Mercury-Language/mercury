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

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module list.

:- pred mark_tail_calls_in_pred(pred_id::in,
    module_info::in, module_info::out, pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % This pass is only required when either: tail recursion warnings are
    % enabled (via the command line option or the require_tail_recursion
    % pragma) or a debug grade is used.  In all other situations the HLDS
    % traversal can be skipped and mark_tail_calls_in_proc will return
    % not_changed to allow a caller to avoid updating the proc table.
    %
:- type maybe_changed
    --->    not_changed
    ;       maybe_changed.

:- pred mark_tail_calls_in_proc(module_info::in, pred_proc_id::in,
    pred_info::in, list(error_spec)::out, maybe_changed::out,
    proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type mark_tail_calls_info
    --->    mark_tail_calls_info(
                mtc_add_feature             :: add_goal_feature,
                mtc_module                  :: module_info,
                mtc_pred_info               :: pred_info,
                mtc_pred_id                 :: pred_id,
                mtc_proc_id                 :: proc_id,
                mtc_vartypes                :: vartypes,
                mtc_warn_tail_calls         :: warn_tail_calls,
                mtc_maybe_require_tailrec   :: maybe(require_tail_recursion)
            ).

:- type add_goal_feature
    --->    add_goal_feature
    ;       do_not_add_goal_feature.

:- type warn_tail_calls
    --->    warn_tail_calls
    ;       do_not_warn_tail_calls.

    % Is the current position within the procedure a tail position, if so
    % what are the output arguments.
    %
:- type at_tail
    --->    at_tail(list(maybe(prog_var)))
    ;       not_at_tail_seen_reccall
    ;       not_at_tail_have_not_seen_reccall.

    % Has any tail call been found so far.  This is used to set the tailcall
    % procedure feature if there is at least one tailcall in the procedure.
    %
:- type found_tail_calls
    --->    found_tail_calls
    ;       not_found_tail_calls.

mark_tail_calls_in_pred(PredId, !ModuleInfo, !PredInfo, !Specs) :-
    ProcIds = pred_info_non_imported_procids(!.PredInfo),
    mark_tail_calls_in_procs(!.ModuleInfo, PredId, ProcIds, !PredInfo,
        !Specs).

:- pred mark_tail_calls_in_procs(module_info::in, pred_id::in,
    list(proc_id)::in, pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_tail_calls_in_procs(_ModuleInfo, _PredId, [], !PredInfo, !Specs).
mark_tail_calls_in_procs(ModuleInfo, PredId, [ProcId | ProcIds], !PredInfo,
        !Specs) :-
    pred_info_proc_info(!.PredInfo, ProcId, ProcInfo0),
    mark_tail_calls_in_proc(ModuleInfo, proc(PredId, ProcId),
        !.PredInfo, Specs, MaybeChanged, ProcInfo0, ProcInfo),
    (
        MaybeChanged = maybe_changed,
        pred_info_set_proc_info(ProcId, ProcInfo, !PredInfo)
    ;
        MaybeChanged = not_changed
    ),
    !:Specs = Specs ++ !.Specs,
    mark_tail_calls_in_procs(ModuleInfo, PredId, ProcIds, !PredInfo, !Specs).

mark_tail_calls_in_proc(ModuleInfo, proc(PredId, ProcId), PredInfo,
        Errors, MaybeChanged, !ProcInfo) :-
    proc_info_interface_determinism(!.ProcInfo, Detism),
    determinism_components(Detism, _CanFail, SolnCount),
    (
        % In at_most_many procedures, we cannot in general know at compile time
        % whether we can delete the current stack frame at a tail call.
        % For at_most_zero procedures, there is no point in handling tail calls
        % specially.
        ( SolnCount = at_most_many
        ; SolnCount = at_most_zero
        ),
        Errors = [],
        MaybeChanged = not_changed
    ;
        ( SolnCount = at_most_one
        ; SolnCount = at_most_many_cc
        ),
        proc_info_get_maybe_require_tailrec_info(!.ProcInfo,
            MaybeRequireTailRecursion),

        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, exec_trace_tail_rec,
            ExecTraceTailRec),
        (
            ExecTraceTailRec = yes,
            AddGoalFeature = add_goal_feature
        ;
            ExecTraceTailRec = no,
            AddGoalFeature = do_not_add_goal_feature
        ),
        globals.lookup_bool_option(Globals, warn_non_tail_recursion,
            WarnNonTailRecursionBool),
        (
            WarnNonTailRecursionBool = yes,
            WarnNonTailRecursion = warn_tail_calls
        ;
            WarnNonTailRecursionBool = no,
            WarnNonTailRecursion = do_not_warn_tail_calls
        ),

        % It is reasonably common that we don't need to check for tail calls
        % at all.
        ( if
            AddGoalFeature = do_not_add_goal_feature,
            (
                WarnNonTailRecursion = do_not_warn_tail_calls,
                (
                    MaybeRequireTailRecursion = no
                ;
                    MaybeRequireTailRecursion =
                        yes(suppress_tailrec_warnings(_))
                )
            ;
                WarnNonTailRecursion = warn_tail_calls,
                MaybeRequireTailRecursion = yes(suppress_tailrec_warnings(_))
            )
        then
            Errors = [],
            MaybeChanged = not_changed
        else
            do_mark_tail_calls_in_proc(AddGoalFeature, WarnNonTailRecursion,
                MaybeRequireTailRecursion, ModuleInfo, PredId, PredInfo,
                ProcId, Errors, !ProcInfo),
            MaybeChanged = maybe_changed
        )
    ).

:- pred do_mark_tail_calls_in_proc(add_goal_feature::in,
    warn_tail_calls::in, maybe(require_tail_recursion)::in,
    module_info::in, pred_id::in, pred_info::in, proc_id::in,
    list(error_spec)::out, proc_info::in, proc_info::out) is det.

do_mark_tail_calls_in_proc(AddGoalFeature, WarnNonTailRecursion,
        MaybeRequireTailRecursion, ModuleInfo, PredId,
        PredInfo, ProcId, Errors, !ProcInfo) :-
    pred_info_get_arg_types(PredInfo, Types),
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_argmodes(!.ProcInfo, Modes),
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_vartypes(!.ProcInfo, VarTypes),
    find_maybe_output_args(ModuleInfo, Types, Modes, HeadVars, Outputs),

    Info = mark_tail_calls_info(AddGoalFeature, ModuleInfo, PredInfo,
        PredId, ProcId, VarTypes, WarnNonTailRecursion,
        MaybeRequireTailRecursion),
    mark_tail_calls_in_goal(Info, FoundTailCalls, Errors0, Goal0, Goal,
        at_tail(Outputs), _),
    proc_info_set_goal(Goal, !ProcInfo),
    (
        FoundTailCalls = found_tail_calls,
        TailCallEvents = has_tail_call_event,
        Errors = Errors0
    ;
        FoundTailCalls = not_found_tail_calls,
        TailCallEvents = has_no_tail_call_event,
        (
            MaybeRequireTailRecursion = yes(RequireTailrecInfo),
            ( RequireTailrecInfo = suppress_tailrec_warnings(Context)
            ; RequireTailrecInfo = enable_tailrec_warnings(_, _, Context)
            ),
            PredOrFunc = pred_info_is_pred_or_func(PredInfo),
            pred_info_get_name(PredInfo, Name),
            pred_info_get_orig_arity(PredInfo, Arity),
            SimpleCallId = simple_call_id(PredOrFunc, unqualified(Name),
                Arity),
            Pieces =
                [words("In:"), pragma_decl("require_tail_recursion"),
                words("for"), simple_call(SimpleCallId), suffix(":"), nl,
                words("warning: code is not recursive."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            NonRecursiveSpec = error_spec(severity_warning, phase_code_gen,
                [Msg]),
            Errors = [NonRecursiveSpec | Errors0]
        ;
            MaybeRequireTailRecursion = no,
            Errors = Errors0
        )
    ),
    proc_info_set_has_tail_call_event(TailCallEvents, !ProcInfo).

:- pred find_maybe_output_args(module_info::in,
     list(mer_type)::in, list(mer_mode)::in, list(prog_var)::in,
     list(maybe(prog_var))::out) is det.

find_maybe_output_args(ModuleInfo, Types, Modes, Vars, Outputs) :-
    ( if
        find_maybe_output_args_2(ModuleInfo, Types, Modes, Vars, OutputsPrime)
    then
        Outputs = OutputsPrime
    else
        unexpected($module, $pred, "list length mismatch")
    ).

:- pred find_maybe_output_args_2(module_info::in,
    list(mer_type)::in, list(mer_mode)::in, list(prog_var)::in,
    list(maybe(prog_var))::out) is semidet.

find_maybe_output_args_2(_, [], [], [], []).
find_maybe_output_args_2(ModuleInfo, [Type | Types], [Mode | Modes],
        [Var | Vars], [OutputVar | OutputVars]) :-
    mode_to_top_functor_mode(ModuleInfo, Mode, Type, TopFunctorMode),
    (
        ( TopFunctorMode = top_in
        ; TopFunctorMode = top_unused
        ),
        OutputVar = no
    ;
        TopFunctorMode = top_out,
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
    found_tail_calls::out, list(error_spec)::out,
    hlds_goal::in, hlds_goal::out, at_tail::in, at_tail::out) is det.

mark_tail_calls_in_goal(Info, FoundTailCalls, Errors, Goal0, Goal,
        AtTail0, AtTail) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = negation(_)
        ),
        Goal = Goal0,
        not_at_tail(AtTail0, AtTail),
        FoundTailCalls = not_found_tail_calls,
        Errors = []
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        (
            ( Reason = exist_quant(_)
            ; Reason = promise_solutions(_, _)
            ; Reason = commit(_)
            ),
            not_at_tail(AtTail0, AtTail1),
            mark_tail_calls_in_goal(Info, FoundTailCalls, Errors,
                SubGoal0, SubGoal, AtTail1, AtTail)
        ;
            ( Reason = promise_purity(_)
            ; Reason = barrier(_)
            ; Reason = from_ground_term(_, _)
            ; Reason = trace_goal(_, _, _, _, _)
            ; Reason = loop_control(_, _, _)
            ),
            mark_tail_calls_in_goal(Info, FoundTailCalls, Errors,
                SubGoal0, SubGoal, AtTail0, AtTail)
        ;
            ( Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ),
            unexpected($file, $pred, "unexpected scope kind")
        ),
        Goal = hlds_goal(scope(Reason, SubGoal), GoalInfo0)
    ;
        GoalExpr0 = unify(LHS, _, _, Unify0, _),
        Goal = Goal0,
        ModuleInfo = Info ^ mtc_module,
        VarTypes = Info ^ mtc_vartypes,
        ( if var_is_of_dummy_type(ModuleInfo, VarTypes, LHS) then
            % Unifications involving dummy type variables are no-ops,
            % and do not inhibit a preceding tail call.
            AtTail = AtTail0
        else
            (
                ( Unify0 = construct(_, _, _, _, _, _, _)
                ; Unify0 = deconstruct(_, _, _, _, _, _)
                ; Unify0 = simple_test(_, _)
                ; Unify0 = complicated_unify(_, _, _)
                ),
                not_at_tail(AtTail0, AtTail)
            ;
                Unify0 = assign(ToVar, FromVar),
                ( if
                    AtTail0 = at_tail(Outputs0),
                    is_output_arg_rename(ToVar, FromVar, Outputs0, Outputs)
                then
                    AtTail = at_tail(Outputs)
                else
                    AtTail = not_at_tail_have_not_seen_reccall
                )
            )
        ),
        FoundTailCalls = not_found_tail_calls,
        Errors = []
    ;
        GoalExpr0 = plain_call(CallPredId, CallProcId, Args, Builtin,
            _UnifyContext, SymName),
        PredId = Info ^ mtc_pred_id,
        ProcId = Info ^ mtc_proc_id,
        ( if
            CallPredId = PredId,
            CallProcId = ProcId,
            Builtin = not_builtin
        then
            ( if
                AtTail0 = at_tail(Outputs0),
                match_output_args(Outputs0, Args)
            then
                AddFeature = Info ^ mtc_add_feature,
                (
                    AddFeature = add_goal_feature,
                    goal_info_add_feature(feature_debug_tail_rec_call,
                        GoalInfo0, GoalInfo),
                    Goal = hlds_goal(GoalExpr0, GoalInfo)
                ;
                    AddFeature = do_not_add_goal_feature,
                    Goal = Goal0
                ),
                Errors = []
            else
                Goal = Goal0,
                Arity = length(Args),
                maybe_report_nontailcall(AtTail0, Info, SymName, Arity,
                    CallProcId, goal_info_get_context(GoalInfo0), Errors)
            ),
            AtTail = not_at_tail_seen_reccall,
            FoundTailCalls = found_tail_calls
        else
            Goal = Goal0,
            not_at_tail(AtTail0, AtTail),
            FoundTailCalls = not_found_tail_calls,
            Errors = []
        )
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            AtTail1 = AtTail0
        ;
            ConjType = parallel_conj,
            % Tail calls in parallel conjunctions are only supported when
            % loop control is enabled.  But loop control would have
            % re-written the conjunction into a loop control scope and
            % therefore all parallel conjunctions at this point do not
            % support tail calls.
            not_at_tail(AtTail0, AtTail1)
        ),
        list.reverse(Goals0, RevGoals0),
        mark_tail_calls_in_conj(Info, RevGoals0, RevGoals,
            AtTail1, AtTail, not_found_tail_calls, FoundTailCalls,
            [], Errors),
        list.reverse(RevGoals, Goals),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Disjs0),
        map4(mark_tail_calls_in_disj(Info, AtTail0), Disjs0, Disjs,
            AtTails, FoundTailCallDisjs, DisjErrors),
        AtTail = at_tail_branches(AtTails),
        FoundTailCalls = found_tail_calls_condense(FoundTailCallDisjs),
        Errors = condense(DisjErrors),
        GoalExpr = disj(Disjs),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        map4(mark_tail_calls_in_case(Info, AtTail0), Cases0, Cases,
            AtTails, FoundTailCallsCases, SwitchErrors),
        AtTail = at_tail_branches(AtTails),
        FoundTailCalls = found_tail_calls_condense(FoundTailCallsCases),
        Errors = condense(SwitchErrors),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        mark_tail_calls_in_goal(Info, FoundTailCallsThen, ErrorsThen,
            Then0, Then, AtTail0, AtTailThen),
        mark_tail_calls_in_goal(Info, FoundTailCallsElse, ErrorsElse,
            Else0, Else, AtTail0, AtTailElse),
        AtTailBranch0 = at_tail_branch(AtTailThen, AtTailElse),
        not_at_tail(AtTailBranch0, AtTailBranch),
        mark_tail_calls_in_goal(Info, _, ErrorsCond, Cond0, Cond,
            AtTailBranch, AtTail),
        ( if
            ( FoundTailCallsThen = found_tail_calls
            ; FoundTailCallsElse = found_tail_calls
            )
        then
            FoundTailCalls = found_tail_calls
        else
            FoundTailCalls = not_found_tail_calls
        ),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0),
        Errors = ErrorsCond ++ ErrorsThen ++ ErrorsElse
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

:- pred mark_tail_calls_in_disj(mark_tail_calls_info::in, at_tail::in,
    hlds_goal::in, hlds_goal::out, at_tail::out, found_tail_calls::out,
    list(error_spec)::out) is det.

mark_tail_calls_in_disj(Info, AtTail0, !Disj, AtTail, FoundAtTail, Errors) :-
    mark_tail_calls_in_goal(Info, FoundAtTail, Errors, !Disj, AtTail0,
        AtTail).

:- pred mark_tail_calls_in_case(mark_tail_calls_info::in, at_tail::in,
    case::in, case::out, at_tail::out, found_tail_calls::out,
    list(error_spec)::out) is det.

mark_tail_calls_in_case(Info, AtTail0, Case0, Case, AtTail, FoundTailCalls,
        Errors) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    mark_tail_calls_in_goal(Info, FoundTailCalls, Errors, Goal0, Goal,
        AtTail0, AtTail),
    Case = case(MainConsId, OtherConsIds, Goal).

:- pred mark_tail_calls_in_conj(mark_tail_calls_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out, at_tail::in, at_tail::out,
    found_tail_calls::in, found_tail_calls::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_tail_calls_in_conj(_Info, [], [], !AtTail, !FoundTailCalls, !Errors).
mark_tail_calls_in_conj(Info, [RevGoal0 | RevGoals0], [RevGoal | RevGoals],
        !AtTail, !FoundTailCalls, !Errors) :-
    mark_tail_calls_in_goal(Info, FoundTailCallsConj, Errors,
        RevGoal0, RevGoal, !AtTail),
    (
        FoundTailCallsConj = found_tail_calls,
        !:FoundTailCalls = found_tail_calls
    ;
        FoundTailCallsConj = not_found_tail_calls
    ),
    !:Errors = Errors ++ !.Errors,
    mark_tail_calls_in_conj(Info, RevGoals0, RevGoals, !AtTail,
        !FoundTailCalls, !Errors).

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

:- func found_tail_calls_condense(list(found_tail_calls)) =
    found_tail_calls.

found_tail_calls_condense(List) =
    ( if member(found_tail_calls, List) then
        found_tail_calls
    else
        not_found_tail_calls
    ).

:- pred not_at_tail(at_tail::in, at_tail::out) is det.

not_at_tail(at_tail(_), not_at_tail_have_not_seen_reccall).
not_at_tail(not_at_tail_seen_reccall, not_at_tail_seen_reccall).
not_at_tail(not_at_tail_have_not_seen_reccall,
    not_at_tail_have_not_seen_reccall).

:- func at_tail_branches(list(at_tail)) = at_tail.

at_tail_branches(List) =
    foldl(at_tail_branch, List, not_at_tail_have_not_seen_reccall).

:- func at_tail_branch(at_tail, at_tail) = at_tail.

at_tail_branch(A, B) = R :-
    (
        A = at_tail(_),
        (
            B = at_tail(_),
            % This shouldn't happen.
            R = not_at_tail_have_not_seen_reccall
        ;
            ( B = not_at_tail_have_not_seen_reccall
            ; B = not_at_tail_seen_reccall
            ),
            R = B
        )
    ;
        A = not_at_tail_have_not_seen_reccall,
        R = B
    ;
        A = not_at_tail_seen_reccall,
        R = not_at_tail_seen_reccall
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_report_nontailcall(at_tail::in, mark_tail_calls_info::in,
    sym_name::in, arity::in, proc_id::in, prog_context::in,
    list(error_spec)::out) is det.

maybe_report_nontailcall(AtTail, Info, SymName, Arity, ProcId,
        Context, Specs) :-
    (
        ( AtTail = at_tail(_)
        ; AtTail = not_at_tail_have_not_seen_reccall
        ),
        PredInfo = Info ^ mtc_pred_info,
        WarnTailCalls = Info ^ mtc_warn_tail_calls,
        MaybeRequireTailRecursion = Info ^ mtc_maybe_require_tailrec,
        (
            MaybeRequireTailRecursion = no,
            (
                WarnTailCalls = warn_tail_calls,
                report_nontailcall(PredInfo, SymName, Arity, ProcId,
                    Context, we_warning, Specs)
            ;
                WarnTailCalls = do_not_warn_tail_calls,
                Specs = []
            )
        ;
            MaybeRequireTailRecursion = yes(RequireTailrecInfo),
            (
                RequireTailrecInfo = enable_tailrec_warnings(WarnOrError,
                    _Type, _),
                % TODO: Check recursion type to implement support for
                % mutual vs self recursion checking.
                report_nontailcall(PredInfo, SymName, Arity, ProcId,
                    Context, WarnOrError, Specs)
            ;
                RequireTailrecInfo = suppress_tailrec_warnings(_),
                Specs = []
            )
        )
    ;
        % Never report calls that are followed by recursive calls.
        % NOTE: We could report these issues, doing so would help
        % programmers ensure that they use constant stack space.  This was
        % not part of the initial design for the pragma but I'd like to add
        % support for it with another option in the near future.
        AtTail = not_at_tail_seen_reccall,
        Specs = []
    ).

:- pred report_nontailcall(pred_info::in, sym_name::in, arity::in,
    proc_id::in, prog_context::in, warning_or_error::in,
    list(error_spec)::out) is det.

report_nontailcall(PredInfo, SymName, Arity, ProcId, Context, WarnOrError,
        Specs) :-
    (
        WarnOrError = we_warning,
        Severity = severity_warning,
        WarnOrErrorWord = words("warning:")
    ;
        WarnOrError = we_error,
        Severity = severity_error,
        WarnOrErrorWord = words("error:")
    ),

    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Name = unqualify_name(SymName),
    SimpleCallId = simple_call_id(PredOrFunc, unqualified(Name), Arity),
    proc_id_to_int(ProcId, ProcNumber0),
    ProcNumber = ProcNumber0 + 1,
    Pieces =
        [words("In mode number"), int_fixed(ProcNumber),
        words("of"), simple_call(SimpleCallId), suffix(":"), nl,
        WarnOrErrorWord, words("recursive call is not tail recursive."),
        nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(Severity, phase_code_gen, [Msg]),
    Specs = [Spec].

%-----------------------------------------------------------------------------%
:- end_module hlds.mark_tail_calls.
%-----------------------------------------------------------------------------%
