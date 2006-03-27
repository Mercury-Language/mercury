%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: simplify.m.
% Main authors: zs, stayl.

% The two jobs of the simplification module are
%
%   to find and exploit opportunities for simplifying the internal form
%   of the program, both to optimize the code and to massage the code
%   into a form the code generator will accept, and
%
%   to warn the programmer about any constructs that are so simple that
%   they should not have been included in the program in the first place.
%
% Simplification is done in two passes. The first pass performs common
% structure and duplicate call elimination. The second pass performs excess
% assignment elimination and cleans up the code after the first pass.
%
% Two passes are required because the goal must be requantified after the
% optimizations in common.m are run so that excess assignment elimination
% works properly.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.simplify.
:- interface.

:- import_module check_hlds.common.
:- import_module check_hlds.det_report.
:- import_module check_hlds.det_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module libs.globals.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred simplify_pred(list(simplification)::in, pred_id::in,
    module_info::in, module_info::out, pred_info::in, pred_info::out,
    int::out, int::out, io::di, io::uo) is det.

:- pred simplify_proc(list(simplification)::in, pred_id::in, proc_id::in,
    module_info::in, module_info::out, proc_info::in, proc_info::out,
    io::di, io::uo) is det.

:- pred simplify_proc_return_msgs(list(simplification)::in, pred_id::in,
    proc_id::in, module_info::in, module_info::out,
    proc_info::in, proc_info::out, set(context_det_msg)::out,
    io::di, io::uo) is det.

:- pred process_goal(hlds_goal::in, hlds_goal::out,
    simplify_info::in, simplify_info::out, io::di, io::uo) is det.

    % Find out which simplifications should be run from the options table
    % stored in the globals. The first argument states whether warnings
    % should be issued during this pass of simplification.
    %
:- pred find_simplifications(bool::in, globals::in, list(simplification)::out)
    is det.

:- type simplification
    --->    warn_simple_code        % --warn-simple-code
    ;       warn_duplicate_calls    % --warn-duplicate-calls
    ;       warn_known_bad_format   % --warn-known-bad-format-calls
    ;       warn_unknown_format     % --warn-unknown-format-calls
    ;       warn_obsolete           % --warn-obsolete
    ;       do_once                 % run things that should be done once
    ;       excess_assigns          % remove excess assignment unifications
    ;       duplicate_calls         % optimize duplicate calls
    ;       constant_prop           % partially evaluate calls
    ;       common_struct           % common structure elimination
    ;       extra_common_struct     % do common structure elimination
                                    % even when it might increase stack
                                    % usage (used by deforestation).
    .

:- type simplify_info.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_analysis.
:- import_module check_hlds.format_call.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.modes.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.polymorphism.
:- import_module check_hlds.purity.
:- import_module check_hlds.type_util.
:- import_module check_hlds.unify_proc.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.special_pred.
:- import_module libs.compiler_util.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module transform_hlds.const_prop.
:- import_module transform_hlds.pd_cost.

:- import_module int.
:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

simplify_pred(Simplifications0, PredId, !ModuleInfo, !PredInfo,
        WarnCnt, ErrCnt, !IO) :-
    write_pred_progress_message("% Simplifying ", PredId, !.ModuleInfo, !IO),
    ProcIds = pred_info_non_imported_procids(!.PredInfo),
    (
        % Don't warn for compiler-generated procedures.
        list.member(warn_simple_code, Simplifications0),
        is_unify_or_compare_pred(!.PredInfo)
    ->
        list.delete_all(Simplifications0, warn_simple_code, Simplifications)
    ;
        Simplifications = Simplifications0
    ),
    MaybeMsgs0 = no,
    simplify_procs(Simplifications, PredId, ProcIds, !ModuleInfo, !PredInfo,
        MaybeMsgs0, MaybeMsgs, !IO),
    (
        MaybeMsgs = yes(Msgs0 - Msgs1),
        set.union(Msgs0, Msgs1, Msgs2),
        set.to_sorted_list(Msgs2, Msgs),
        det_report_msgs(Msgs, !.ModuleInfo, WarnCnt, ErrCnt, !IO)
    ;
        MaybeMsgs = no,
        WarnCnt = 0,
        ErrCnt = 0
    ),
    globals.io_lookup_bool_option(detailed_statistics, Statistics, !IO),
    maybe_report_stats(Statistics, !IO).

:- pred simplify_procs(list(simplification)::in, pred_id::in,
    list(proc_id)::in, module_info::in, module_info::out,
    pred_info::in, pred_info::out,
    maybe(pair(set(context_det_msg)))::in,
    maybe(pair(set(context_det_msg)))::out,
    io::di, io::uo) is det.

simplify_procs(_, _, [], !ModuleInfo, !PredInfo, !Msgs, !IO).
simplify_procs(Simplifications, PredId, [ProcId | ProcIds], !ModuleInfo,
        !PredInfo, !MaybeMsgs, !IO) :-
    pred_info_procedures(!.PredInfo, Procs0),
    map.lookup(Procs0, ProcId, Proc0),
    simplify_proc_return_msgs(Simplifications, PredId, ProcId,
        !ModuleInfo, Proc0, Proc, ProcMsgSet, !IO),
    map.det_update(Procs0, ProcId, Proc, Procs),
    pred_info_set_procedures(Procs, !PredInfo),
    set.to_sorted_list(ProcMsgSet, ProcMsgs),
    list.filter((pred(context_det_msg(_, Msg)::in) is semidet :-
            det_msg_is_any_mode_msg(Msg, any_mode)
        ), ProcMsgs, ProcAnyModeMsgs, ProcAllModeMsgs),
    set.sorted_list_to_set(ProcAnyModeMsgs, ProcAnyModeMsgSet),
    set.sorted_list_to_set(ProcAllModeMsgs, ProcAllModeMsgSet),
    (
        !.MaybeMsgs = yes(AnyModeMsgSet0 - AllModeMsgSet0),
        set.union(AnyModeMsgSet0, ProcAnyModeMsgSet, AnyModeMsgSet),
        set.intersect(AllModeMsgSet0, ProcAllModeMsgSet, AllModeMsgSet),
        !:MaybeMsgs = yes(AllModeMsgSet - AnyModeMsgSet)
    ;
        !.MaybeMsgs = no,
        !:MaybeMsgs = yes(ProcAnyModeMsgSet - ProcAllModeMsgSet)
    ),
    simplify_procs(Simplifications, PredId, ProcIds, !ModuleInfo, !PredInfo,
        !MaybeMsgs, !IO).

simplify_proc(Simplifications, PredId, ProcId, !ModuleInfo, !Proc, !IO)  :-
    write_pred_progress_message("% Simplifying ", PredId, !.ModuleInfo, !IO),
    simplify_proc_return_msgs(Simplifications, PredId, ProcId, !ModuleInfo,
        !Proc, _, !IO).

simplify_proc_return_msgs(Simplifications, PredId, ProcId, !ModuleInfo,
        !ProcInfo, DetMsgs, !IO) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    proc_info_vartypes(!.ProcInfo, VarTypes0),
    det_info_init(!.ModuleInfo, VarTypes0, PredId, ProcId, Globals,
        DetInfo0),
    proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InstMap0),
    simplify_info_init(DetInfo0, Simplifications, InstMap0, !.ProcInfo, Info0),
    proc_info_goal(!.ProcInfo, Goal0),

    simplify_info_get_pred_info(Info0, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    (
        check_marker(Markers, mode_check_clauses),
        Goal0 = GoalExpr0 - GoalInfo0,
        ( GoalExpr0 = disj(_)
        ; GoalExpr0 = switch(_, _, _)
        )
    ->
        goal_info_add_feature(mode_check_clauses_goal, GoalInfo0, GoalInfo1),
        Goal1 = GoalExpr0 - GoalInfo1
    ;
        Goal1 = Goal0
    ),

    process_goal(Goal1, Goal, Info0, Info, !IO),

    simplify_info_get_varset(Info, VarSet),
    simplify_info_get_var_types(Info, VarTypes),
    simplify_info_get_rtti_varmaps(Info, RttiVarMaps),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),
    simplify_info_get_module_info(Info, !:ModuleInfo),
    simplify_info_get_det_msgs(Info, DetMsgs0),
    (
        Info ^ format_calls = yes,
        (
            list.member(warn_known_bad_format, Simplifications)
        ;
            list.member(warn_unknown_format, Simplifications)
        )
    ->
        % We must use the original goal, Goal0, here. This is because excess
        % assignment optimization may delete some of the unifications that
        % build the format strings or values, which means that the new version
        % in Goal may not contain the information find_format_call_errors needs
        % to avoid spurious messages about unknown format strings or values.
        find_format_call_errors(!.ModuleInfo, Goal0, DetMsgs0, DetMsgs1)
    ;
        % Either there are no calls to check or we would ignore the added
        % messages anyway.
        DetMsgs1 = DetMsgs0
    ),
    pred_info_import_status(PredInfo, Status),
    status_defined_in_this_module(Status, IsDefinedHere),
    (
        IsDefinedHere = no,
        % Don't generate any warnings or even errors if the predicate isn't
        % defined here; any such messages will be generated when we compile
        % the module the predicate comes from.
        set.init(DetMsgs)
    ;
        IsDefinedHere = yes,
        DetMsgs = DetMsgs1
    ).

process_goal(Goal0, Goal, !Info, !IO) :-
    simplify_info_get_simplifications(!.Info, Simplifications0),
    simplify_info_get_instmap(!.Info, InstMap0),

    (
        ( simplify_do_common(!.Info)
        ; simplify_do_calls(!.Info)
        )
    ->
        % On the first pass do common structure and call elimination.
        NotOnFirstPass = [do_once, excess_assigns],

        set.delete_list(Simplifications0, NotOnFirstPass, Simplifications1),
        simplify_info_set_simplifications(Simplifications1, !Info),

        do_process_goal(Goal0, Goal1, !Info, !IO),

        NotOnSecondPass = [warn_simple_code, warn_duplicate_calls,
            common_struct, duplicate_calls],
        set.delete_list(Simplifications0, NotOnSecondPass, Simplifications2),
        simplify_info_reinit(Simplifications2, InstMap0, !Info)
    ;
        Goal1 = Goal0
    ),
    % On the second pass do excess assignment elimination and some cleaning up
    % after the common structure pass.
    do_process_goal(Goal1, Goal, !Info, !IO).

:- pred do_process_goal(hlds_goal::in, hlds_goal::out,
    simplify_info::in, simplify_info::out, io::di, io::uo) is det.

do_process_goal(Goal0, Goal, !Info, !IO) :-
    simplify_info_get_instmap(!.Info, InstMap0),
    simplify_goal(Goal0, Goal1, !Info, !IO),
    simplify_info_get_varset(!.Info, VarSet0),
    simplify_info_get_var_types(!.Info, VarTypes0),
    ( simplify_info_requantify(!.Info) ->
        Goal1 = _ - GoalInfo1,
        goal_info_get_nonlocals(GoalInfo1, NonLocals),
        implicitly_quantify_goal(NonLocals, _, Goal1, Goal2,
            VarSet0, VarSet1, VarTypes0, VarTypes1),

        simplify_info_set_varset(VarSet1, !Info),
        simplify_info_set_var_types(VarTypes1, !Info),

        % Always recompute instmap_deltas for atomic goals - this
        % is safer in the case where unused variables should no
        % longer be included in the instmap_delta for a goal.
        % In the alias branch this is necessary anyway.
        RecomputeAtomic = yes,

        simplify_info_get_module_info(!.Info, ModuleInfo0),
        recompute_instmap_delta(RecomputeAtomic, Goal2, Goal3, VarTypes1,
            !.Info ^ inst_varset, InstMap0, ModuleInfo0, ModuleInfo1),
        simplify_info_set_module_info(ModuleInfo1, !Info)
    ;
        Goal3 = Goal1
    ),
    ( simplify_info_rerun_det(!.Info) ->
        Goal0 = _ - GoalInfo0,
        goal_info_get_determinism(GoalInfo0, Det),
        det_get_soln_context(Det, SolnContext),

        % det_infer_goal looks up the proc_info in the module_info
        % for the vartypes, so we'd better stick them back in the
        % module_info.
        simplify_info_get_module_info(!.Info, ModuleInfo2),
        simplify_info_get_varset(!.Info, VarSet2),
        simplify_info_get_var_types(!.Info, VarTypes2),
        simplify_info_get_det_info(!.Info, DetInfo2),
        det_info_get_pred_id(DetInfo2, PredId),
        det_info_get_proc_id(DetInfo2, ProcId),
        module_info_pred_proc_info(ModuleInfo2, PredId, ProcId,
            PredInfo, ProcInfo0),
        proc_info_set_vartypes(VarTypes2, ProcInfo0, ProcInfo1),
        proc_info_set_varset(VarSet2, ProcInfo1, ProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId,
            PredInfo, ProcInfo, ModuleInfo2, ModuleInfo3),
        simplify_info_set_module_info(ModuleInfo3, !Info),

        simplify_info_get_det_info(!.Info, DetInfo),
        det_infer_goal(Goal3, Goal, InstMap0, SolnContext, [], no, DetInfo,
            _, _, _)
    ;
        Goal = Goal3
    ).

%-----------------------------------------------------------------------------%

find_simplifications(WarnThisPass, Globals, Simps) :-
    find_simplifications_2(WarnThisPass, Globals, [], Simps).

:- pred find_simplifications_2(bool::in, globals::in,
    list(simplification)::in, list(simplification)::out) is det.

find_simplifications_2(WarnThisPass, Globals, !Simps) :-
    (
        WarnThisPass = yes,
        set_by_option(Globals, warn_duplicate_calls,
            warn_duplicate_calls, !Simps),
        set_by_option(Globals, warn_simple_code,
            warn_simple_code, !Simps),
        set_by_option(Globals, warn_known_bad_format_calls,
            warn_known_bad_format, !Simps),
        set_by_option(Globals, warn_unknown_format_calls,
            warn_unknown_format, !Simps),
        set_by_option(Globals, warn_obsolete, warn_obsolete,
            !Simps)
    ;
        WarnThisPass = no
    ),
    set_by_option(Globals, excess_assign, excess_assigns, !Simps),
    set_by_option(Globals, common_struct, common_struct, !Simps),
    set_by_option(Globals, optimize_duplicate_calls, duplicate_calls, !Simps),
    set_by_option(Globals, constant_propagation, constant_prop, !Simps).

:- pred set_by_option(globals::in, option::in, simplification::in,
    list(simplification)::in, list(simplification)::out) is det.

set_by_option(Globals, Option, Simplification, !Simplifications) :-
    globals.lookup_bool_option(Globals, Option, Result),
    (
        Result = yes,
        !:Simplifications = [Simplification | !.Simplifications]
    ;
        Result = no
    ).

%-----------------------------------------------------------------------------%

:- pred simplify_goal(hlds_goal::in, hlds_goal::out,
    simplify_info::in, simplify_info::out, io::di, io::uo) is det.

simplify_goal(Goal0, Goal - GoalInfo, !Info, !IO) :-
    Goal0 = _ - GoalInfo0,
    goal_info_get_determinism(GoalInfo0, Detism),
    simplify_info_get_det_info(!.Info, DetInfo),
    simplify_info_get_module_info(!.Info, ModuleInfo0),
    goal_can_loop_or_throw(Goal0, Goal0CanLoopOrThrow, ModuleInfo0,
        ModuleInfo, !IO),
    simplify_info_set_module_info(ModuleInfo, !Info),
    (
        %
        % if --no-fully-strict,
        % replace goals with determinism failure with `fail'.
        %
        Detism = failure,
        % ensure goal is pure or semipure
        \+ goal_info_is_impure(GoalInfo0),
        ( det_info_get_fully_strict(DetInfo, no)
        ; Goal0CanLoopOrThrow = cannot_loop_or_throw
        )
    ->
        % Warn about this, unless the goal was an explicit `fail', call to
        % `builtin.false/0' or  some goal containing `fail' or a call to
        % `builtin.false/0'.
        %
        goal_info_get_context(GoalInfo0, Context),
        (
            simplify_do_warn(!.Info),
            \+ (
                    goal_contains_goal(Goal0, SubGoal),
                    ( SubGoal = disj([]) - _
                    ; goal_is_call_to_builtin_false(SubGoal))
            )
        ->
            Msg = goal_cannot_succeed,
            ContextMsg = context_det_msg(Context, Msg),
            simplify_info_add_det_msg(ContextMsg, !Info)
        ;
            true
        ),

        % If the goal had any non-locals we should requantify.
        goal_info_get_nonlocals(GoalInfo0, NonLocals0),
        ( set.empty(NonLocals0) ->
            true
        ;
            simplify_info_set_requantify(!Info)
        ),
        goal_cost(Goal0, CostDelta),
        simplify_info_incr_cost_delta(CostDelta, !Info),
        Goal1 = fail_goal_with_context(Context)
    ;
        %
        % If --no-fully-strict, replace goals which cannot fail and have
        % no output variables with `true'. However, we don't do this for
        % erroneous goals, since these may occur in conjunctions where there
        % are no producers for some variables, and the code generator would
        % fail for these.
        %
        determinism_components(Detism, cannot_fail, MaxSoln),
        MaxSoln \= at_most_zero,
        goal_info_get_instmap_delta(GoalInfo0, InstMapDelta),
        goal_info_get_nonlocals(GoalInfo0, NonLocalVars),
        simplify_info_get_instmap(!.Info, InstMap0),
        det_no_output_vars(NonLocalVars, InstMap0, InstMapDelta, DetInfo),
        % ensure goal is pure or semipure
        \+ goal_info_is_impure(GoalInfo0),
        ( det_info_get_fully_strict(DetInfo, no)
        ; Goal0CanLoopOrThrow = cannot_loop_or_throw
        )
    ->
% The following warning is disabled, because it often results in spurious
% warnings.  Sometimes predicate calls are used just to constrain the types,
% to avoid type ambiguities or unbound type variables, and in such cases,
% it is perfectly legitimate for a call to be det and to have no outputs.
% There's no simple way of telling those cases from cases for which we
% really ought to warn.
% XXX This hasn't really been true since we added `with_type`.
%
%       % warn about this, if the goal wasn't `true', wasn't `!',
%       % and wasn't a deconstruction unification.
%       % We don't warn about deconstruction unifications
%       % with no outputs that always succeed, because that
%       % would result in bogus warnings, since switch detection
%       % converts deconstruction unifications that can fail
%       % into ones that always succeed by moving the test into
%       % the switch.
%       % We also don't warn about conjunctions or existential
%       % quantifications, because it seems that warnings in those
%       % cases are usually spurious.
%       (
%           simplify_do_warn(!.Info),
%           % Goal0 \= conj(plain_conj, []) - _,
%           \+ (Goal0 = call(_, _, _, _, _, SymName) - _,
%               unqualify_name(SymName, "!")),
%           Goal0 \= conj(plain_conj, _) - _,
%           Goal0 \= some(_, _) - _,
%           \+ (Goal0 = unify(_, _, _, Unification, _) - _,
%               Unification = deconstruct(_, _, _, _, _))
%       ->
%           Msg = det_goal_has_no_outputs,
%           ContextMsg = context_det_msg(Context, Msg),
%           simplify_info_add_det_msg(ContextMsg, !Info)
%       ;
%           true
%       ),

        % If the goal had any non-locals we should requantify.
        goal_info_get_nonlocals(GoalInfo0, NonLocals0),
        ( set.empty(NonLocals0) ->
            true
        ;
            simplify_info_set_requantify(!Info)
        ),
        goal_cost(Goal0, CostDelta),
        simplify_info_incr_cost_delta(CostDelta, !Info),
        goal_info_get_context(GoalInfo0, Context),
        Goal1 = true_goal_with_context(Context)
    ;
        Goal1 = Goal0
    ),

    %
    % Remove unnecessary explicit quantifications before working
    % out whether the goal can cause a stack flush.
    %
    ( Goal1 = scope(Reason, SomeGoal1) - GoalInfo1 ->
        nested_scopes(Reason, SomeGoal1, GoalInfo1, Goal2)
    ;
        Goal2 = Goal1
    ),
    simplify_info_maybe_clear_structs(before, Goal2, !Info),
    Goal2 = GoalExpr2 - GoalInfo2,
    simplify_goal_2(GoalExpr2, Goal, GoalInfo2, GoalInfo3, !Info, !IO),
    simplify_info_maybe_clear_structs(after, Goal - GoalInfo3, !Info),
    enforce_invariant(GoalInfo3, GoalInfo, !Info).

    % Ensure that the mode information and the determinism
    % information say consistent things about unreachability.
    %
:- pred enforce_invariant(hlds_goal_info::in, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is det.

enforce_invariant(GoalInfo0, GoalInfo, !Info) :-
    goal_info_get_determinism(GoalInfo0, Determinism0),
    goal_info_get_instmap_delta(GoalInfo0, DeltaInstmap0),
    determinism_components(Determinism0, CanFail0, NumSolns0),
    (
        NumSolns0 = at_most_zero,
        instmap_delta_is_reachable(DeltaInstmap0)
    ->
        instmap_delta_init_unreachable(UnreachableInstMapDelta),
        goal_info_set_instmap_delta(UnreachableInstMapDelta,
            GoalInfo0, GoalInfo),
        simplify_info_set_rerun_det(!Info)
    ;
        instmap_delta_is_unreachable(DeltaInstmap0),
        NumSolns0 \= at_most_zero
    ->
        determinism_components(Determinism, CanFail0, at_most_zero),
        goal_info_set_determinism(Determinism, GoalInfo0, GoalInfo),
        simplify_info_set_rerun_det(!Info)
    ;
        GoalInfo = GoalInfo0
    ).

:- pred goal_is_call_to_builtin_false(hlds_goal::in) is semidet.

goal_is_call_to_builtin_false(Goal - _) :-
    Goal = call(_, _, _, _, _, SymName),
    SymName = qualified(mercury_public_builtin_module, "false").

%-----------------------------------------------------------------------------%

:- pred simplify_goal_2(hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_info::in, simplify_info::out, io::di, io::uo) is det.

simplify_goal_2(conj(ConjType, Goals0), Goal, GoalInfo0, GoalInfo,
        !Info, !IO) :-
    (
        ConjType = plain_conj,
        simplify_info_get_instmap(!.Info, InstMap0),
        excess_assigns_in_conj(GoalInfo0, Goals0, Goals1, !Info),
        simplify_conj(Goals1, [], Goals, GoalInfo0, !Info, !IO),
        simplify_info_set_instmap(InstMap0, !Info),
        (
            Goals = [],
            goal_info_get_context(GoalInfo0, Context),
            Goal - GoalInfo = true_goal_with_context(Context)
        ;
            Goals = [SingleGoal - SingleGoalInfo],
            % A singleton conjunction is equivalent to the goal itself.
            maybe_wrap_goal(GoalInfo0, SingleGoalInfo, SingleGoal,
                Goal, GoalInfo, !Info)
        ;
            Goals = [_, _ | _],
            %
            % Conjunctions that cannot produce solutions may nevertheless
            % contain nondet and multi goals. If this happens, the conjunction
            % is put inside a `scope' to appease the code generator.
            %
            goal_info_get_determinism(GoalInfo0, Detism),
            (
                simplify_do_once(!.Info),
                determinism_components(Detism, CanFail, at_most_zero),
                contains_multisoln_goal(Goals)
            ->
                determinism_components(InnerDetism, CanFail, at_most_many),
                goal_info_set_determinism(InnerDetism, GoalInfo0, InnerInfo),
                InnerGoal = conj(plain_conj, Goals) - InnerInfo,
                Goal = scope(commit(dont_force_pruning), InnerGoal)
            ;
                Goal = conj(plain_conj, Goals)
            ),
            GoalInfo = GoalInfo0
        )
    ;
        ConjType = parallel_conj,
        (
            Goals0 = [],
            goal_info_get_context(GoalInfo0, Context),
            Goal - GoalInfo = true_goal_with_context(Context)
        ;
            Goals0 = [SingleGoal0],
            simplify_goal(SingleGoal0, SingleGoal - SingleGoalInfo, !Info, !IO),
            maybe_wrap_goal(GoalInfo0, SingleGoalInfo, SingleGoal,
                Goal, GoalInfo, !Info)
        ;
            Goals0 = [_, _ | _],
            GoalInfo = GoalInfo0,
            simplify_par_conj(Goals0, Goals, !.Info, !Info, !IO),
            Goal = conj(parallel_conj, Goals)
        )
    ).

simplify_goal_2(disj(Disjuncts0), Goal, GoalInfo0, GoalInfo, !Info, !IO) :-
    simplify_info_get_instmap(!.Info, InstMap0),
    simplify_disj(Disjuncts0, [], Disjuncts, [], InstMaps, !.Info, !Info,
        !IO),
    (
        Disjuncts = [],
        goal_info_get_context(GoalInfo0, Context),
        Goal - GoalInfo = fail_goal_with_context(Context)
    ;
        Disjuncts = [SingleGoal],
        % A singleton disjunction is equivalent to the goal itself.
        SingleGoal = Goal1 - GoalInfo1,
        maybe_wrap_goal(GoalInfo0, GoalInfo1, Goal1, Goal, GoalInfo, !Info)
    ;
        Disjuncts = [_, _ | _],
        Goal = disj(Disjuncts),
        ( goal_info_has_feature(GoalInfo0, mode_check_clauses_goal) ->
            % Recomputing the instmap delta would take very long and is
            % very unlikely to get any better precision.
            GoalInfo = GoalInfo0
        ;
            simplify_info_get_module_info(!.Info, ModuleInfo1),
            goal_info_get_nonlocals(GoalInfo0, NonLocals),
            simplify_info_get_var_types(!.Info, VarTypes),
            merge_instmap_deltas(InstMap0, NonLocals, VarTypes, InstMaps,
                NewDelta, ModuleInfo1, ModuleInfo2),
            simplify_info_set_module_info(ModuleInfo2, !Info),
            goal_info_set_instmap_delta(NewDelta, GoalInfo0, GoalInfo)
        )
    ),
    list.length(Disjuncts, DisjunctsLength),
    list.length(Disjuncts0, Disjuncts0Length),
    ( DisjunctsLength \= Disjuncts0Length ->
        %
        % If we pruned some disjuncts, variables used by those disjuncts
        % may no longer be non-local to the disjunction. Also, the determinism
        % may have changed (especially if we pruned all the disjuncts).
        % If the disjunction now can't succeed, it is necessary to recompute
        % instmap_deltas and rerun determinism analysis to avoid aborts
        % in the code generator because the disjunction now cannot produce
        % variables it did before.
        %
        simplify_info_set_requantify(!Info),
        simplify_info_set_rerun_det(!Info)
    ;
        true
    ).

simplify_goal_2(switch(Var, SwitchCanFail0, Cases0), Goal,
        GoalInfo0, GoalInfo, !Info, !IO) :-
    simplify_info_get_instmap(!.Info, InstMap0),
    simplify_info_get_module_info(!.Info, ModuleInfo0),
    instmap.lookup_var(InstMap0, Var, VarInst),
    ( inst_is_bound_to_functors(ModuleInfo0, VarInst, Functors) ->
        functors_to_cons_ids(Functors, ConsIds0),
        list.sort(ConsIds0, ConsIds),
        delete_unreachable_cases(Cases0, ConsIds, Cases1),
        MaybeConsIds = yes(ConsIds)
    ;
        Cases1 = Cases0,
        MaybeConsIds = no
    ),
    simplify_switch(Var, Cases1, [], Cases, [], InstMaps,
        SwitchCanFail0, SwitchCanFail, !.Info, !Info, !IO),
    (
        Cases = [],
        % An empty switch always fails.
        simplify_info_incr_cost_delta(cost_of_eliminate_switch, !Info),
        goal_info_get_context(GoalInfo0, Context),
        Goal - GoalInfo = fail_goal_with_context(Context)
    ;
        Cases = [case(ConsId, SingleGoal)],
        % A singleton switch is equivalent to the goal itself with a
        % possibly can_fail unification with the functor on the front.
        Arity = cons_id_arity(ConsId),
        (
            SwitchCanFail = can_fail,
            MaybeConsIds \= yes([ConsId])
        ->
            % Don't optimize in the case of an existentially typed constructor
            % because currently create_test_unification does not handle the
            % existential type variables in the types of the constructor
            % arguments or their typeinfos.

            simplify_info_get_var_types(!.Info, VarTypes1),
            map.lookup(VarTypes1, Var, Type),
            simplify_info_get_module_info(!.Info, ModuleInfo1),
            ( type_util.is_existq_cons(ModuleInfo1, Type, ConsId) ->
                Goal = switch(Var, SwitchCanFail, Cases),
                goal_info_get_nonlocals(GoalInfo0, NonLocals),
                simplify_info_get_var_types(!.Info, VarTypes),
                merge_instmap_deltas(InstMap0, NonLocals, VarTypes,
                    InstMaps, NewDelta, ModuleInfo1, ModuleInfo2),
                simplify_info_set_module_info(ModuleInfo2, !Info),
                goal_info_set_instmap_delta(NewDelta, GoalInfo0, GoalInfo)
            ;
                create_test_unification(Var, ConsId, Arity, UnifyGoal, !Info),

                % Conjoin the test and the rest of the case.
                goal_to_conj_list(SingleGoal, SingleGoalConj),
                GoalList = [UnifyGoal | SingleGoalConj],

                % Work out the nonlocals, instmap_delta
                % and determinism of the entire conjunction.
                goal_info_get_nonlocals(GoalInfo0, NonLocals0),
                set.insert(NonLocals0, Var, NonLocals),
                goal_info_get_instmap_delta(GoalInfo0, InstMapDelta0),
                simplify_info_get_instmap(!.Info, InstMap),
                instmap_delta_bind_var_to_functor(Var, Type, ConsId,
                    InstMap, InstMapDelta0, InstMapDelta,
                    ModuleInfo1, ModuleInfo),
                simplify_info_set_module_info(ModuleInfo, !Info),
                goal_info_get_determinism(GoalInfo0, CaseDetism),
                det_conjunction_detism(semidet, CaseDetism, Detism),
                goal_list_purity(GoalList, Purity),
                goal_info_init(NonLocals, InstMapDelta, Detism, Purity,
                    CombinedGoalInfo),

                simplify_info_set_requantify(!Info),
                Goal = conj(plain_conj, GoalList),
                GoalInfo = CombinedGoalInfo
            )
        ;
            % The var can only be bound to this cons_id, so a test
            % is unnecessary.
            SingleGoal = Goal - GoalInfo
        ),
        simplify_info_incr_cost_delta(cost_of_eliminate_switch, !Info)
    ;
        Cases = [_, _ | _],
        Goal = switch(Var, SwitchCanFail, Cases),
        ( goal_info_has_feature(GoalInfo0, mode_check_clauses_goal) ->
            % Recomputing the instmap delta would take very long and is
            % very unlikely to get any better precision.
            GoalInfo = GoalInfo0
        ;
            simplify_info_get_module_info(!.Info, ModuleInfo1),
            goal_info_get_nonlocals(GoalInfo0, NonLocals),
            simplify_info_get_var_types(!.Info, VarTypes),
            merge_instmap_deltas(InstMap0, NonLocals, VarTypes, InstMaps,
                NewDelta, ModuleInfo1, ModuleInfo2),
            simplify_info_set_module_info(ModuleInfo2, !Info),
            goal_info_set_instmap_delta(NewDelta, GoalInfo0, GoalInfo)
        )
    ),
    list.length(Cases0, Cases0Length),
    list.length(Cases, CasesLength),
    ( CasesLength \= Cases0Length ->
        % If we pruned some cases, variables used by those cases may no longer
        % be non-local to the switch. Also, the determinism may have changed
        % (especially if we pruned all the cases). If the switch now can't
        % succeed, it is necessary to recompute instmap_deltas and rerun
        % determinism analysis to avoid aborts in the code generator because
        % the switch now cannot produce variables it did before.

        simplify_info_set_requantify(!Info),
        simplify_info_set_rerun_det(!Info)
    ;
        true
    ).

simplify_goal_2(Goal0, Goal, GoalInfo, GoalInfo, !Info, !IO) :-
    Goal0 = generic_call(GenericCall, Args, Modes, Det),
    (
        simplify_do_calls(!.Info),
        % XXX We should do duplicate call elimination for
        % class method calls here.
        GenericCall = higher_order(Closure, Purity, _, _),
        % XXX Should we handle semipure higher-order calls too?
        Purity = purity_pure
    ->
        common.optimise_higher_order_call(Closure, Args, Modes, Det,
            GoalInfo, Goal0, Goal, !Info)
    ;
        simplify_do_warn_calls(!.Info),
        GenericCall = higher_order(Closure, Purity, _, _),
        % XXX Should we handle impure/semipure higher-order calls too?
        Purity = purity_pure
    ->
        % We need to do the pass, for the warnings, but we ignore
        % the optimized goal and instead use the original one.
        common.optimise_higher_order_call(Closure, Args, Modes, Det,
            GoalInfo, Goal0, _Goal1, !Info),
        Goal = Goal0
    ;
        Goal = Goal0
    ).

simplify_goal_2(Goal0, Goal, GoalInfo0, GoalInfo, !Info, !IO) :-
    Goal0 = call(PredId, ProcId, Args, IsBuiltin, _, _),
    simplify_info_get_module_info(!.Info, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = hlds_pred.pred_info_module(PredInfo),
    Name = hlds_pred.pred_info_name(PredInfo),
    ( is_format_call(ModuleName, Name, Args, _, _) ->
        simplify_info_set_format_calls(yes, !Info)
    ;
        true
    ),
    %
    % Convert calls to builtin @=<, @<, @>=, @> into the corresponding
    % calls to builtin.compare/3.
    %
    (
        Args = [TI, X, Y],
        ModuleName = mercury_public_builtin_module,
        ( Name =  "@<", Inequality = "<", Invert = no
        ; Name = "@=<", Inequality = ">", Invert = yes
        ; Name = "@>=", Inequality = "<", Invert = yes
        ; Name = "@>",  Inequality = ">", Invert = no
        )
    ->
        inequality_goal(TI, X, Y, Inequality, Invert, GoalInfo0,
            Goal, GoalInfo, !Info)
    ;
        call_goal(PredId, ProcId, Args, IsBuiltin, Goal0, Goal,
            GoalInfo0, GoalInfo, !Info)
    ).

simplify_goal_2(Goal0, Goal, GoalInfo0, GoalInfo, !Info, !IO) :-
    Goal0 = unify(LT0, RT0, M, U0, C),
    (
        % A unification of the form X = X can be safely
        % optimised away.

        RT0 = var(LT0)
    ->
        goal_info_get_context(GoalInfo0, Context),
        Goal - GoalInfo = true_goal_with_context(Context)
    ;
        RT0 = lambda_goal(Purity, PredOrFunc, EvalMethod, NonLocals,
            Vars, Modes, LambdaDeclaredDet, LambdaGoal0)
    ->
        simplify_info_enter_lambda(!Info),
        simplify_info_get_common_info(!.Info, Common1),
        simplify_info_get_module_info(!.Info, ModuleInfo),
        simplify_info_get_instmap(!.Info, InstMap1),
        instmap.pre_lambda_update(ModuleInfo, Vars, Modes, InstMap1, InstMap2),
        simplify_info_set_instmap(InstMap2, !Info),

        % Don't attempt to pass structs into lambda_goals,
        % since that could change the curried non-locals of the
        % lambda_goal, and that would be difficult to fix up.
        simplify_info_set_common_info(common_info_init, !Info),

        % Don't attempt to pass structs out of lambda_goals.
        simplify_goal(LambdaGoal0, LambdaGoal, !Info, !IO),
        simplify_info_set_common_info(Common1, !Info),
        simplify_info_set_instmap(InstMap1, !Info),
        RT = lambda_goal(Purity, PredOrFunc, EvalMethod, NonLocals,
            Vars, Modes, LambdaDeclaredDet, LambdaGoal),
        simplify_info_leave_lambda(!Info),
        Goal = unify(LT0, RT, M, U0, C),
        GoalInfo = GoalInfo0
    ;
        U0 = complicated_unify(UniMode, CanFail, TypeInfoVars)
    ->
        ( RT0 = var(V) ->
            process_compl_unify(LT0, V, UniMode, CanFail, TypeInfoVars, C,
                GoalInfo0, Goal1, !Info, !IO),
            Goal1 = Goal - GoalInfo
        ;
            unexpected(this_file, "invalid RHS for complicated unify")
        )
    ;
        simplify_do_common(!.Info)
    ->
        common.optimise_unification(U0, LT0, RT0, M, C,
            Goal0, Goal, GoalInfo0, GoalInfo, !Info)
    ;
        ( simplify_do_calls(!.Info)
        ; simplify_do_warn_calls(!.Info)
        )
    ->
        % We need to do the pass, to record the variable
        % equivalences used for optimizing or warning about
        % duplicate calls.  But we don't want to perform
        % the optimization, so we disregard the optimized goal
        % and instead use the original one.
        common.optimise_unification(U0, LT0, RT0, M, C,
            Goal0, _Goal1, GoalInfo0, _GoalInfo1, !Info),
        Goal = Goal0,
        GoalInfo = GoalInfo0
    ;
        Goal = Goal0,
        GoalInfo = GoalInfo0
    ).

simplify_goal_2(if_then_else(Vars, Cond0, Then0, Else0), Goal,
        GoalInfo0, GoalInfo, !Info, !IO) :-
    % (A -> B ; C) is logically equivalent to (A, B ; ~A, C).
    % If the determinism of A means that one of these disjuncts
    % cannot succeed, then we replace the if-then-else with the
    % other disjunct. (We could also eliminate A, but we leave
    % that to the recursive invocations.)
    %
    % Note however that rerunning determinism analysis, which
    % we do at the end of simplification, may introduce more
    % occurrences of these; since we don't iterate simplification
    % and determinism anaysis until a fixpoint is reached,
    % we don't guarantee to eliminate all such if-then-elses.
    % Hence the code generator must be prepared to handle the
    % case when the condition of an if-then-else has determinism
    % `det' or `failure'.
    %
    % The conjunction operator in the remaining disjunct ought to be
    % a sequential conjunction, because Mercury's if-then-else always
    % guarantees sequentiality, whereas conjunction only guarantees
    % sequentiality if the --no-reorder-conj option is enabled.
    %
    % However, currently reordering is only done in mode analysis,
    % not in the code generator, so we don't yet need a sequential
    % conjunction construct. This will change when constraint pushing
    % is finished, or when we start doing coroutining.

    Cond0 = _ - CondInfo0,
    goal_info_get_determinism(CondInfo0, CondDetism0),
    determinism_components(CondDetism0, CondCanFail0, CondSolns0),
    ( CondCanFail0 = cannot_fail ->
        goal_to_conj_list(Cond0, CondList),
        goal_to_conj_list(Then0, ThenList),
        list.append(CondList, ThenList, List),
        simplify_goal(conj(plain_conj, List) - GoalInfo0, Goal - GoalInfo,
            !Info, !IO),
        goal_info_get_context(GoalInfo0, Context),
        Msg = ite_cond_cannot_fail,
        ContextMsg = context_det_msg(Context, Msg),
        simplify_info_add_det_msg(ContextMsg, !Info),
        simplify_info_set_requantify(!Info),
        simplify_info_set_rerun_det(!Info)
    ; CondSolns0 = at_most_zero ->
        % Optimize away the condition and the `then' part.
        det_negation_det(CondDetism0, MaybeNegDetism),
        (
            Cond0 = not(NegCond) - _,
            % XXX BUG! This optimization is only safe if it
            % preserves mode correctness, which means in particular
            % that the the negated goal must not clobber any
            % variables.
            % For now I've just disabled the optimization.
            semidet_fail
        ->
            Cond = NegCond
        ;
            (
                MaybeNegDetism = yes(NegDetism1),
                (
                    NegDetism1 = erroneous,
                    instmap_delta_init_unreachable(NegInstMapDelta1)
                ;
                    NegDetism1 = det,
                    instmap_delta_init_reachable(NegInstMapDelta1)
                )
            ->
                NegDetism = NegDetism1,
                NegInstMapDelta = NegInstMapDelta1
            ;
                unexpected(this_file, "goal_2: cannot get negated determinism")
            ),
            goal_info_set_determinism(NegDetism, CondInfo0, NegCondInfo0),
            goal_info_set_instmap_delta(NegInstMapDelta,
                NegCondInfo0, NegCondInfo),
            Cond = not(Cond0) - NegCondInfo
        ),
        goal_to_conj_list(Else0, ElseList),
        List = [Cond | ElseList],
        simplify_goal(conj(plain_conj, List) - GoalInfo0, Goal - GoalInfo,
            !Info, !IO),
        goal_info_get_context(GoalInfo0, Context),
        Msg = ite_cond_cannot_succeed,
        ContextMsg = context_det_msg(Context, Msg),
        simplify_info_add_det_msg(ContextMsg, !Info),
        simplify_info_set_requantify(!Info),
        simplify_info_set_rerun_det(!Info)
    ; Else0 = disj([]) - _ ->
        % (A -> C ; fail) is equivalent to (A, C)
        goal_to_conj_list(Cond0, CondList),
        goal_to_conj_list(Then0, ThenList),
        list.append(CondList, ThenList, List),
        simplify_goal(conj(plain_conj, List) - GoalInfo0, Goal - GoalInfo,
            !Info, !IO),
        simplify_info_set_requantify(!Info),
        simplify_info_set_rerun_det(!Info)
    ;
        %
        % recursively simplify the sub-goals,
        % and rebuild the resulting if-then-else
        %
        Info0 = !.Info,
        simplify_info_get_instmap(!.Info, InstMap0),
        simplify_goal(Cond0, Cond, !Info, !IO),
        simplify_info_update_instmap(Cond, !Info),
        simplify_goal(Then0, Then, !Info, !IO),
        simplify_info_post_branch_update(Info0, !Info),
        simplify_goal(Else0, Else, !Info, !IO),
        simplify_info_post_branch_update(Info0, !Info),
        Cond = _ - CondInfo,
        goal_info_get_instmap_delta(CondInfo, CondDelta),
        Then = _ - ThenInfo,
        goal_info_get_instmap_delta(ThenInfo, ThenDelta),
        instmap_delta_apply_instmap_delta(CondDelta, ThenDelta,
            test_size, CondThenDelta),
        Else = _ - ElseInfo,
        goal_info_get_instmap_delta(ElseInfo, ElseDelta),
                goal_info_get_nonlocals(GoalInfo0, NonLocals),
        simplify_info_get_module_info(!.Info, ModuleInfo0),
        simplify_info_get_var_types(!.Info, VarTypes),
        merge_instmap_deltas(InstMap0, NonLocals, VarTypes,
            [CondThenDelta, ElseDelta], NewDelta,
            ModuleInfo0, ModuleInfo1),
        simplify_info_set_module_info(ModuleInfo1, !Info),
        goal_info_set_instmap_delta(NewDelta, GoalInfo0, GoalInfo1),
        IfThenElse = if_then_else(Vars, Cond, Then, Else),

        goal_info_get_determinism(GoalInfo0, IfThenElseDetism0),
        determinism_components(IfThenElseDetism0, IfThenElseCanFail,
            IfThenElseNumSolns),

        goal_info_get_determinism(CondInfo, CondDetism),
        determinism_components(CondDetism, CondCanFail, CondSolns),
        (
            % Check again if we can apply one of the above simplifications
            % after having simplified the sub-goals (we need to do this
            % to ensure that the goal is fully simplified, to maintain the
            % invariants that the MLDS back-end depends on)
            ( CondCanFail = cannot_fail
            ; CondSolns = at_most_zero
            ; Else = disj([]) - _
            )
        ->
            simplify_info_undo_goal_updates(Info0, !Info),
            simplify_goal_2(IfThenElse, Goal, GoalInfo1, GoalInfo, !Info,
                !IO)
        ;
            (
                % If-then-elses that are det or semidet may nevertheless
                % contain nondet or multidet conditions. If this happens,
                % the if-then-else must be put inside a `scope' to appease the
                % code generator.  (Both the MLDS and LLDS back-ends rely
                % on this.)
                %
                simplify_do_once(!.Info),
                CondSolns = at_most_many,
                IfThenElseNumSolns \= at_most_many
            ->
                determinism_components(InnerDetism,
                    IfThenElseCanFail, at_most_many),
                goal_info_set_determinism(InnerDetism, GoalInfo1, InnerInfo),
                Goal = scope(commit(dont_force_pruning),
                    IfThenElse - InnerInfo)
            ;
                Goal = IfThenElse
            ),
            GoalInfo = GoalInfo1
        )
    ).

simplify_goal_2(not(Goal0), Goal, GoalInfo0, GoalInfo, !Info, !IO) :-
    % Can't use calls or unifications seen within a negation,
    % since non-local variables may not be bound within the negation.
    simplify_info_get_common_info(!.Info, Common),
    simplify_goal(Goal0, Goal1, !Info, !IO),
    simplify_info_set_common_info(Common, !Info),
    Goal1 = _ - GoalInfo1,
    goal_info_get_determinism(GoalInfo1, Detism),
    determinism_components(Detism, CanFail, MaxSoln),
    goal_info_get_context(GoalInfo0, Context),
    ( CanFail = cannot_fail ->
        Msg = negated_goal_cannot_fail,
        ContextMsg = context_det_msg(Context, Msg),
        simplify_info_add_det_msg(ContextMsg, !Info)
    ; MaxSoln = at_most_zero ->
        Msg = negated_goal_cannot_succeed,
        ContextMsg = context_det_msg(Context, Msg),
        simplify_info_add_det_msg(ContextMsg, !Info)
    ;
        true
    ),
    (
        % replace `not true' with `fail'
        Goal1 = conj(plain_conj, []) - _GoalInfo
    ->
        Goal - GoalInfo = fail_goal_with_context(Context)
    ;
        % replace `not fail' with `true'
        Goal1 = disj([]) - _GoalInfo2
    ->
        Goal - GoalInfo = true_goal_with_context(Context)
    ;
        % remove double negation
        Goal1 = not(SubGoal - SubGoalInfo) - _,
        % XXX BUG! This optimization is only safe if it preserves
        % mode correctness, which means in particular that the
        % the negated goal must not clobber any variables.
        % For now I've just disabled the optimization.
        semidet_fail
    ->
        maybe_wrap_goal(GoalInfo0, SubGoalInfo, SubGoal, Goal, GoalInfo, !Info)
    ;
        Goal = not(Goal1),
        GoalInfo = GoalInfo0
    ).

simplify_goal_2(scope(Reason0, Goal1), GoalExpr, SomeInfo, GoalInfo,
        !Info, !IO) :-
    simplify_info_get_common_info(!.Info, Common),
    simplify_goal(Goal1, Goal2, !Info, !IO),
    nested_scopes(Reason0, Goal2, SomeInfo, Goal),
    Goal = GoalExpr - GoalInfo,
    ( Goal = scope(FinalReason, _) - _ ->
        (
            FinalReason = promise_purity(_, _),
            KeepCommon = yes
        ;
            FinalReason = commit(_),
            KeepCommon = no
        ;
            FinalReason = from_ground_term(_),
            KeepCommon = yes
        ;
            FinalReason = barrier(removable),
            KeepCommon = yes
        ;
            FinalReason = barrier(not_removable),
            KeepCommon = no
        ;
            FinalReason = exist_quant(_),
            KeepCommon = no
        ;
            FinalReason = promise_solutions(_, _),
            KeepCommon = no
        ),
        (
            KeepCommon = yes
        ;
            KeepCommon = no,
            % Replacing calls, constructions or deconstructions
            % outside a commit with references to variables created
            % inside the commit would increase the set of output
            % variables of the goal inside the commit. This is not
            % allowed because it could change the determinism.
            %
            % Thus we need to reset the common_info to what it
            % was before processing the goal inside the commit,
            % to ensure that we don't make any such replacements
            % when processing the rest of the goal.
            simplify_info_set_common_info(Common, !Info)
        )
    ;
        true
    ).

simplify_goal_2(Goal0, Goal, GoalInfo, GoalInfo, !Info, !IO) :-
    Goal0 = foreign_proc(Attributes, PredId, ProcId, Args0, ExtraArgs0, Impl),
    BoxPolicy = box_policy(Attributes),
    (
        BoxPolicy = native_if_possible,
        Args = Args0,
        ExtraArgs = ExtraArgs0,
        Goal1 = Goal0
    ;
        BoxPolicy = always_boxed,
        Args = list.map(make_arg_always_boxed, Args0),
        ExtraArgs = list.map(make_arg_always_boxed, ExtraArgs0),
        Goal1 = foreign_proc(Attributes, PredId, ProcId, Args, ExtraArgs, Impl)
    ),
    (
        simplify_do_calls(!.Info),
        goal_info_is_pure(GoalInfo),
        ExtraArgs = []
        ->
        ArgVars = list.map(foreign_arg_var, Args),
        common.optimise_call(PredId, ProcId, ArgVars, GoalInfo, Goal1, Goal,
            !Info)
    ;
        Goal = Goal1
    ).

simplify_goal_2(shorthand(_), _, _, _, _, _, _, _) :-
    % These should have been expanded out by now.
    unexpected(this_file, "goal_2: unexpected shorthand").

:- func make_arg_always_boxed(foreign_arg) = foreign_arg.

make_arg_always_boxed(Arg) = Arg ^ arg_box_policy := always_boxed.

%-----------------------------------------------------------------------------%

:- pred inequality_goal(prog_var::in, prog_var::in, prog_var::in, string::in,
    bool::in, hlds_goal_info::in, hlds_goal_expr::out, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is det.

inequality_goal(TI, X, Y, Inequality, Invert, GoalInfo, GoalExpr, GoalInfo,
        !Info) :-

    % Construct the variable to hold the comparison result.
    VarSet0 = !.Info ^ varset,
    varset.new_var(VarSet0, R, VarSet),
    !:Info = !.Info ^ varset := VarSet,

    % We have to add the type of R to the var_types.
    simplify_info_get_var_types(!.Info, VarTypes0),
    VarTypes = VarTypes0 ^ elem(R) := comparison_result_type,
    simplify_info_set_var_types(VarTypes, !Info),

    % Construct the call to compare/3.
    mercury_public_builtin_module(BuiltinModule),
    hlds_goal.goal_info_get_context(GoalInfo, Context),
    Args     = [TI, R, X, Y],

    simplify_info_get_instmap(!.Info, InstMap),
    instmap.lookup_var(InstMap, X, XInst),
    instmap.lookup_var(InstMap, Y, YInst),
    simplify_info_get_module_info(!.Info, ModuleInfo),
    ModeNo =
        ( if inst_is_unique(ModuleInfo, XInst) then
            ( if inst_is_unique(ModuleInfo, YInst) then 1 else 2 )
        else
            ( if inst_is_unique(ModuleInfo, YInst) then 3 else 0 )
        ),

    Unique   = ground(unique, none),
    ArgInsts = [R - Unique],
    goal_util.generate_simple_call(BuiltinModule, "compare", predicate,
        mode_no(ModeNo), det, Args, [], ArgInsts, ModuleInfo, Context,
        CmpGoal0),
    CmpGoal0 = CmpExpr - CmpInfo0,
    goal_info_get_nonlocals(CmpInfo0, CmpNonLocals0),
    goal_info_set_nonlocals(CmpNonLocals0 `insert` R, CmpInfo0, CmpInfo),
    CmpGoal  = CmpExpr - CmpInfo,

    % Construct the unification R = Inequality.
    ConsId   = cons(qualified(BuiltinModule, Inequality), 0),
    Bound    = bound(shared,  [functor(ConsId, [])]),
    UMode    = ((Unique -> Bound) - (Bound -> Bound)),
    RHS      = functor(ConsId, no, []),
    UKind    = deconstruct(R, ConsId, [], [], can_fail, cannot_cgc),
    UContext = unify_context(implicit(
                "replacement of inequality with call to compare/3"), []),
    UfyExpr  = unify(R, RHS, UMode, UKind, UContext),
    goal_info_get_nonlocals(GoalInfo, UfyNonLocals0),
    goal_info_set_nonlocals(UfyNonLocals0 `insert` R, GoalInfo, UfyInfo),
    UfyGoal  = UfyExpr - UfyInfo,

    (
        Invert   = no,
        GoalExpr = conj(plain_conj, [CmpGoal, UfyGoal])
    ;
        Invert   = yes,
        GoalExpr = conj(plain_conj, [CmpGoal, not(UfyGoal) - UfyInfo])
    ).

%-----------------------------------------------------------------------------%

:- pred call_goal(pred_id::in, proc_id::in, list(prog_var)::in,
    builtin_state::in, hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is det.

call_goal(PredId, ProcId, Args, IsBuiltin, Goal0, Goal, GoalInfo0, GoalInfo,
        !Info) :-
    simplify_info_get_module_info(!.Info, ModuleInfo),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    % Check for calls to predicates with `pragma obsolete' declarations.
    (
        simplify_do_warn_obsolete(!.Info),
        pred_info_get_markers(PredInfo, Markers),
        check_marker(Markers, obsolete),

        simplify_info_get_det_info(!.Info, DetInfo0),
        det_info_get_pred_id(DetInfo0, ThisPredId),
        
        % Don't warn about directly recursive calls. (That would cause
        % spurious warnings, particularly with builtin predicates,
        % or preds defined using foreign_procs.)
        PredId \= ThisPredId,

        % Don't warn about calls from predicates that also have a 
        % `pramga obsolete' declaration.  Doing so just results in 
        % spurious warnings.
        module_info_pred_info(ModuleInfo, ThisPredId, ThisPredInfo),
        pred_info_get_markers(ThisPredInfo, ThisPredMarkers),
        not check_marker(ThisPredMarkers, obsolete)
    ->
        goal_info_get_context(GoalInfo0, Context1),
        ObsoleteMsg = warn_obsolete(PredId),
        ObsoleteContextMsg = context_det_msg(Context1, ObsoleteMsg),
        simplify_info_add_det_msg(ObsoleteContextMsg, !Info)
    ;
        true
    ),

    % Check for recursive calls with the same input arguments,
    % and warn about them (since they will lead to infinite loops).
    (
        simplify_do_warn(!.Info),

        % Is this a (directly) recursive call, i.e. is the procedure being
        % called the same as the procedure we're analyzing?
        simplify_info_get_det_info(!.Info, DetInfo),
        det_info_get_pred_id(DetInfo, PredId),
        det_info_get_proc_id(DetInfo, ProcId),

        % Don't count inline builtins. (The compiler generates code for
        % builtins that looks recursive, so that you can take their address,
        % but since the recursive call actually expands into inline code,
        % it is not infinite recursion.)
        IsBuiltin \= inline_builtin,

        % Don't warn if we're inside a lambda goal, because the recursive call
        % may not be executed.
        \+ simplify_info_inside_lambda(!.Info),

        % Are the input arguments the same (or equivalent)?
        simplify_info_get_module_info(!.Info, ModuleInfo1),
        module_info_pred_proc_info(ModuleInfo1, PredId, ProcId,
            PredInfo1, ProcInfo1),
        proc_info_headvars(ProcInfo1, HeadVars),
        proc_info_argmodes(ProcInfo1, ArgModes),
        simplify_info_get_common_info(!.Info, CommonInfo1),
        input_args_are_equiv(Args, HeadVars, ArgModes,
            CommonInfo1, ModuleInfo1),

        % Don't warn if the input arguments' modes initial insts
        % contain `any' insts, since the arguments might have become
        % more constrained before the recursive call, in which case
        % the recursion might eventually terminate.
        %
        % XXX The following check will only warn if the inputs are
        % all fully ground; i.e. we won't warn in the case of
        % partially instantiated insts such as list_skel(free).
        % Still, it is better to miss warnings in that rare and
        % unsupported case rather than to issue spurious warnings
        % in cases involving `any' insts.  We should only warn about
        % definite nontermination here, not possible nontermination;
        % warnings about possible nontermination should only be given
        % if the termination analysis pass is enabled.
        all [ArgMode] (
            (
                list.member(ArgMode, ArgModes),
                mode_is_input(ModuleInfo1, ArgMode)
            )
        =>
            mode_is_fully_input(ModuleInfo1, ArgMode)
        ),

        % Don't count procs using minimal evaluation as they should always
        % terminate if they have a finite number of answers.
        \+ proc_info_eval_method(ProcInfo, eval_minimal(_)),

        % Don't warn about impure procedures, since they may modify the state
        % in ways not visible to us (unlike pure and semipure procedures).
        pred_info_get_purity(PredInfo1, Purity),
        \+ Purity = purity_impure
    ->
        goal_info_get_context(GoalInfo0, Context2),
        InfiniteRecMsg = warn_infinite_recursion,
        InfiniteRecContextMsg = context_det_msg(Context2, InfiniteRecMsg),
        simplify_info_add_det_msg(InfiniteRecContextMsg, !Info)
    ;
        true
    ),

    % Check for duplicate calls to the same procedure.
    (
        simplify_do_calls(!.Info),
        goal_info_is_pure(GoalInfo0)
    ->
        common.optimise_call(PredId, ProcId, Args, GoalInfo0, Goal0, Goal1,
            !Info)
    ;
        simplify_do_warn_calls(!.Info),
        goal_info_is_pure(GoalInfo0)
    ->
        % we need to do the pass, for the warnings, but we ignore
        % the optimized goal and instead use the original one
        common.optimise_call(PredId, ProcId, Args, GoalInfo0, Goal0, _Goal1,
            !Info),
        Goal1 = Goal0
    ;
        Goal1 = Goal0
    ),

    % Try to evaluate the call at compile-time.
    ( simplify_do_const_prop(!.Info) ->
        simplify_info_get_instmap(!.Info, Instmap0),
        simplify_info_get_module_info(!.Info, ModuleInfo2),
        simplify_info_get_var_types(!.Info, VarTypes),
        (
            Goal1 = call(_, _, _, _, _, _),
            const_prop.evaluate_call(PredId, ProcId, Args, VarTypes,
                Instmap0, ModuleInfo2, Goal2, GoalInfo0, GoalInfo2)
        ->
            Goal = Goal2,
            GoalInfo = GoalInfo2,
            simplify_info_set_module_info(ModuleInfo2, !Info),
            simplify_info_set_requantify(!Info)
        ;
            Goal = Goal1,
            GoalInfo = GoalInfo0
        )
    ;
        Goal = Goal1,
        GoalInfo = GoalInfo0
    ).

%-----------------------------------------------------------------------------%

:- pred process_compl_unify(prog_var::in, prog_var::in, uni_mode::in,
    can_fail::in, list(prog_var)::in, unify_context::in, hlds_goal_info::in,
    hlds_goal::out, simplify_info::in, simplify_info::out,
    io::di, io::uo) is det.

process_compl_unify(XVar, YVar, UniMode, CanFail, _OldTypeInfoVars, Context,
        GoalInfo0, Goal, !Info, !IO) :-
    simplify_info_get_module_info(!.Info, ModuleInfo),
    simplify_info_get_var_types(!.Info, VarTypes),
    map.lookup(VarTypes, XVar, Type),
    ( Type = variable(TypeVar, Kind) ->
        %
        % Convert polymorphic unifications into calls to `unify/2',
        % the general unification predicate, passing the appropriate type_info:
        %   unify(TypeInfoVar, X, Y)
        % where TypeInfoVar is the type_info variable associated with
        % the type of the variables that are being unified.
        %
        type_info_locn(TypeVar, Kind, TypeInfoVar, ExtraGoals, !Info),
        call_generic_unify(TypeInfoVar, XVar, YVar, ModuleInfo, !.Info,
            Context, GoalInfo0, Call)

    ; type_is_higher_order(Type, _, _, _, _) ->
        % Convert higher-order unifications into calls to
        % builtin_unify_pred (which calls error/1).
        goal_info_get_context(GoalInfo0, GContext),
        generate_simple_call(mercury_private_builtin_module,
            "builtin_unify_pred", predicate, mode_no(0), semidet,
            [XVar, YVar], [], [], ModuleInfo, GContext, Call0 - _),
        simplify_goal_2(Call0, Call1, GoalInfo0, GoalInfo, !Info, !IO),
        Call = Call1 - GoalInfo,
        ExtraGoals = []
    ;
        ( type_to_ctor_and_args(Type, TypeCtorPrime, TypeArgsPrime) ->
            TypeCtor = TypeCtorPrime,
            TypeArgs = TypeArgsPrime
        ;
            unexpected(this_file, "type_to_ctor_and_args failed")
        ),
        determinism_components(Det, CanFail, at_most_one),
        unify_proc.lookup_mode_num(ModuleInfo, TypeCtor, UniMode, Det,
            ProcId),
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, special_preds, SpecialPreds),
        (
            hlds_pred.in_in_unification_proc_id(ProcId),
            (
                SpecialPreds = no
            ;
                SpecialPreds = yes,

                % For most imported types we only generate unification
                % predicate declarations if they are needed for complicated
                % unifications other than proc_id 0. higher_order.m will
                % specialize these cases if possible.
                %
                special_pred_is_generated_lazily(ModuleInfo, TypeCtor)
            )
        ->
            make_type_info_vars([Type], TypeInfoVars, ExtraGoals, !Info),
            ( TypeInfoVars = [TypeInfoVarPrime] ->
                TypeInfoVar = TypeInfoVarPrime
            ;
                unexpected(this_file, "process_compl_unify: " ++
                    "more than one typeinfo for one type var")
            ),
            call_generic_unify(TypeInfoVar, XVar, YVar, ModuleInfo, !.Info,
                Context, GoalInfo0, Call)
        ;
            % Convert other complicated unifications into calls to
            % specific unification predicates, inserting extra typeinfo
            % arguments if necessary.
            make_type_info_vars(TypeArgs, TypeInfoVars, ExtraGoals, !Info),
            call_specific_unify(TypeCtor, TypeInfoVars, XVar, YVar, ProcId,
                ModuleInfo, Context, GoalInfo0, Call0, CallGoalInfo0),
            simplify_goal_2(Call0, Call1, CallGoalInfo0, CallGoalInfo1, !Info,
                !IO),
            Call = Call1 - CallGoalInfo1
        )
    ),
    list.append(ExtraGoals, [Call], ConjList),
    conj_list_to_goal(ConjList, GoalInfo0, Goal).

:- pred call_generic_unify(prog_var::in, prog_var::in,  prog_var::in,
    module_info::in, simplify_info::in, unify_context::in,
    hlds_goal_info::in, hlds_goal::out) is det.

call_generic_unify(TypeInfoVar, XVar, YVar, ModuleInfo, _, _, GoalInfo,
        Call) :-
    ArgVars = [TypeInfoVar, XVar, YVar],
    goal_info_get_context(GoalInfo, Context),
    goal_util.generate_simple_call(mercury_public_builtin_module,
        "unify", predicate, mode_no(0), semidet, ArgVars, [], [],
        ModuleInfo, Context, Call).

:- pred call_specific_unify(type_ctor::in, list(prog_var)::in,
    prog_var::in, prog_var::in, proc_id::in,
    module_info::in, unify_context::in, hlds_goal_info::in,
    hlds_goal_expr::out, hlds_goal_info::out) is det.

call_specific_unify(TypeCtor, TypeInfoVars, XVar, YVar, ProcId, ModuleInfo,
        Context, GoalInfo0, CallExpr, CallGoalInfo) :-
    % Create the new call goal.
    list.append(TypeInfoVars, [XVar, YVar], ArgVars),
    module_info_get_special_pred_map(ModuleInfo, SpecialPredMap),
    map.lookup(SpecialPredMap, spec_pred_unify - TypeCtor, PredId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    SymName = qualified(ModuleName, PredName),
    CallContext = call_unify_context(XVar, var(YVar), Context),
    CallExpr = call(PredId, ProcId, ArgVars, not_builtin,
        yes(CallContext), SymName),

    % Add the extra type_info vars to the nonlocals for the call.
    goal_info_get_nonlocals(GoalInfo0, NonLocals0),
    set.insert_list(NonLocals0, TypeInfoVars, NonLocals),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, CallGoalInfo).

:- pred make_type_info_vars(list(mer_type)::in, list(prog_var)::out,
    list(hlds_goal)::out, simplify_info::in, simplify_info::out) is det.

make_type_info_vars(Types, TypeInfoVars, TypeInfoGoals, !Info) :-
    % Extract the information from simplify_info.
    simplify_info_get_det_info(!.Info, DetInfo0),
    simplify_info_get_varset(!.Info, VarSet0),
    simplify_info_get_var_types(!.Info, VarTypes0),
    simplify_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    det_info_get_module_info(DetInfo0, ModuleInfo0),
    det_info_get_pred_id(DetInfo0, PredId),
    det_info_get_proc_id(DetInfo0, ProcId),

    some [!PredInfo, !ProcInfo, !PolyInfo] (
        % The varset, vartypes and rtti_varmaps get updated by the call to
        % polymorphism.m, below.  That module will work on the poly_info,
        % however, which is derived from the information in the proc_info.
        % Therefore we:
        %   - copy the info from the simplify_info to the proc_info,
        %   - create a poly_info from the proc_info,
        %   - do the polymorphism transformation,
        %   - extract info from the poly_info and put it in the proc_info,
        %   - copy the information from the proc_info back into the
        %     simplify_info.

        module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
            !:PredInfo, !:ProcInfo),
        proc_info_set_vartypes(VarTypes0, !ProcInfo),
        proc_info_set_varset(VarSet0, !ProcInfo),
        proc_info_set_rtti_varmaps(RttiVarMaps0, !ProcInfo),

        % Call polymorphism.m to create the type_infos.
        create_poly_info(ModuleInfo0, !.PredInfo, !.ProcInfo, !:PolyInfo),
        term.context_init(Context),
        polymorphism.make_type_info_vars(Types, Context,
            TypeInfoVars, TypeInfoGoals, !PolyInfo),
        poly_info_extract(!.PolyInfo, !PredInfo, !ProcInfo, ModuleInfo1),

        proc_info_vartypes(!.ProcInfo, VarTypes),
        proc_info_varset(!.ProcInfo, VarSet),
        proc_info_rtti_varmaps(!.ProcInfo, RttiVarMaps),
        simplify_info_set_var_types(VarTypes, !Info),
        simplify_info_set_varset(VarSet, !Info),
        simplify_info_set_rtti_varmaps(RttiVarMaps, !Info),

        % Put the new proc_info and pred_info back in the module_info
        % and put the new module_info back in the simplify_info.
        module_info_set_pred_proc_info(PredId, ProcId, !.PredInfo, !.ProcInfo,
            ModuleInfo1, ModuleInfo)
    ),
    simplify_info_set_module_info(ModuleInfo, !Info).

:- pred type_info_locn(tvar::in, kind::in, prog_var::out,
    list(hlds_goal)::out, simplify_info::in, simplify_info::out) is det.

type_info_locn(TypeVar, Kind, TypeInfoVar, Goals, !Info) :-
    simplify_info_get_rtti_varmaps(!.Info, RttiVarMaps),
    rtti_lookup_type_info_locn(RttiVarMaps, TypeVar, TypeInfoLocn),
    (
        % If the typeinfo is available in a variable, just use it.
        TypeInfoLocn = type_info(TypeInfoVar),
        Goals = []
    ;
        % If the typeinfo is in a typeclass_info then we need to extract it.
        TypeInfoLocn = typeclass_info(TypeClassInfoVar, Index),
        extract_type_info(TypeVar, Kind, TypeClassInfoVar, Index, Goals,
            TypeInfoVar, !Info)
    ).

:- pred extract_type_info(tvar::in, kind::in, prog_var::in, int::in,
    list(hlds_goal)::out, prog_var::out,
    simplify_info::in, simplify_info::out) is det.

extract_type_info(TypeVar, Kind, TypeClassInfoVar, Index, Goals, TypeInfoVar,
        !Info) :-
    simplify_info_get_module_info(!.Info, ModuleInfo),
    simplify_info_get_varset(!.Info, VarSet0),
    simplify_info_get_var_types(!.Info, VarTypes0),
    simplify_info_get_rtti_varmaps(!.Info, RttiVarMaps0),

    polymorphism.gen_extract_type_info(TypeVar, Kind, TypeClassInfoVar, Index,
        ModuleInfo, Goals, TypeInfoVar, VarSet0, VarSet, VarTypes0, VarTypes,
        RttiVarMaps0, RttiVarMaps),

    simplify_info_set_var_types(VarTypes, !Info),
    simplify_info_set_varset(VarSet, !Info),
    simplify_info_set_rtti_varmaps(RttiVarMaps, !Info).

%-----------------------------------------------------------------------------%

    % input_args_are_equiv(Args, HeadVars, Modes, CommonInfo, ModuleInfo1):
    %
    % Succeeds if all the input arguments (determined by looking at `Modes')
    % in `Args' are equivalent (according to the equivalence class specified
    % by `CommonInfo') to the corresponding variables in HeadVars.
    % HeadVars, Modes, and Args should all be lists of the same length.
    %
:- pred input_args_are_equiv(list(prog_var)::in, list(prog_var)::in,
    list(mer_mode)::in, common_info::in, module_info::in) is semidet.

input_args_are_equiv([], [], _, _, _).
input_args_are_equiv([Arg | Args], [HeadVar | HeadVars], [Mode | Modes],
        CommonInfo, ModuleInfo) :-
    ( mode_is_input(ModuleInfo, Mode) ->
        common.vars_are_equivalent(Arg, HeadVar, CommonInfo)
    ;
        true
    ),
    input_args_are_equiv(Args, HeadVars, Modes, CommonInfo, ModuleInfo).

%-----------------------------------------------------------------------------%

    % replace nested `scope's with a single `scope',
    %
:- pred nested_scopes(scope_reason::in, hlds_goal::in,
    hlds_goal_info::in, hlds_goal::out) is det.

nested_scopes(Reason0, InnerGoal0, OuterGoalInfo, Goal) :-
    nested_scopes_2(Reason0, Reason, InnerGoal0, InnerGoal),
    InnerGoal = _ - GoalInfo,
    (
        Reason = exist_quant(_),
        goal_info_get_determinism(GoalInfo, Detism),
        goal_info_get_determinism(OuterGoalInfo, Detism)
    ->
        % If the inner and outer detisms match the `some' scope is unnecessary.
        Goal = InnerGoal
    ;
        Goal = scope(Reason, InnerGoal) - OuterGoalInfo
    ).

:- pred nested_scopes_2(scope_reason::in, scope_reason::out,
    hlds_goal::in, hlds_goal::out) is det.

nested_scopes_2(Reason0, Reason, Goal0, Goal) :-
    (
        Goal0 = scope(Reason1, Goal1) - _GoalInfo0,
        (
            Reason0 = exist_quant(Vars0),
            Reason1 = exist_quant(Vars1)
        ->
            list.append(Vars0, Vars1, Vars2),
            Reason2 = exist_quant(Vars2)
        ;
            Reason0 = from_ground_term(_)
        ->
            Reason2 = Reason1
        ;
            Reason1 = from_ground_term(_)
        ->
            Reason2 = Reason0
        ;
            Reason0 = barrier(Removable0),
            Reason1 = barrier(Removable1)
        ->
            (
                Removable0 = removable,
                Removable1 = removable
            ->
                Removable2 = removable
            ;
                Removable2 = not_removable
            ),
            Reason2 = barrier(Removable2)
        ;
            Reason0 = commit(ForcePruning0),
            Reason1 = commit(ForcePruning1)
        ->
            (
                ForcePruning0 = dont_force_pruning,
                ForcePruning1 = dont_force_pruning
            ->
                ForcePruning2 = dont_force_pruning
            ;
                ForcePruning2 = force_pruning
            ),
            Reason2 = commit(ForcePruning2)
        ;
            fail
        )
    ->
        nested_scopes_2(Reason2, Reason, Goal1, Goal)
    ;
        Reason = Reason0,
        Goal = Goal0
    ).

%-----------------------------------------------------------------------------%

    % When removing a level of wrapping around a goal, if the determinisms
    % are not the same, we really need to rerun determinism analysis on the
    % procedure. I think this is a similar situation to inlining of erroneous
    % goals. The safe thing to do is to wrap a `scope' around the inner goal
    % if the inner and outer determinisms are not the same. It probably
    % won't happen that often.
    %
:- pred maybe_wrap_goal(hlds_goal_info::in, hlds_goal_info::in,
    hlds_goal_expr::in, hlds_goal_expr::out, hlds_goal_info::out,
    simplify_info::in, simplify_info::out)  is det.

maybe_wrap_goal(OuterGoalInfo, InnerGoalInfo, Goal1, Goal, GoalInfo, !Info) :-
    (
        goal_info_get_determinism(InnerGoalInfo, Det),
        goal_info_get_determinism(OuterGoalInfo, Det)
    ->
        Goal = Goal1,
        GoalInfo = InnerGoalInfo
    ;
        Goal = scope(commit(dont_force_pruning), Goal1 - InnerGoalInfo),
        GoalInfo = OuterGoalInfo,
        simplify_info_set_rerun_det(!Info)
    ).

%-----------------------------------------------------------------------------%

:- pred simplify_conj(list(hlds_goal)::in, list(hlds_goal)::in,
    list(hlds_goal)::out, hlds_goal_info::in,
    simplify_info::in, simplify_info::out, io::di, io::uo) is det.

simplify_conj([], RevGoals, Goals, _, !Info, !IO) :-
    list.reverse(RevGoals, Goals).
simplify_conj([Goal0 | Goals0], !.RevGoals, Goals, ConjInfo, !Info, !IO) :-
    Info0 = !.Info,
    % Flatten conjunctions.
    ( Goal0 = conj(plain_conj, SubGoals) - _ ->
        list.append(SubGoals, Goals0, Goals1),
        simplify_conj(Goals1, !.RevGoals, Goals, ConjInfo, !Info, !IO)
    ;
        simplify_goal(Goal0, Goal1, !Info, !IO),
        (
            % Flatten conjunctions.
            Goal1 = conj(plain_conj, SubGoals1) - _
        ->
            simplify_info_undo_goal_updates(Info0, !Info),
            list.append(SubGoals1, Goals0, Goals1),
            simplify_conj(Goals1, !.RevGoals, Goals, ConjInfo, !Info, !IO)
        ;
            % Delete unreachable goals.
            (
                simplify_info_get_instmap(!.Info, InstMap1),
                instmap.is_unreachable(InstMap1)
            ;
                Goal1 = _ - GoalInfo1,
                goal_info_get_determinism(GoalInfo1, Detism1),
                determinism_components(Detism1, _, at_most_zero)
            )
        ->
            conjoin_goal_and_rev_goal_list(Goal1, !RevGoals),
            (
                ( Goal1 = disj([]) - _
                ; Goals0 = []
                )
            ->
                true
            ;
                % We insert an explicit failure at the end of the
                % non-succeeding conjunction. This is necessary, since
                % the unreachability of the instmap could have been derived
                % using inferred determinism information. Without the
                % explicit fail goal, mode errors could result if mode
                % analysis is rerun, since according to the language
                % specification, mode analysis does not use inferred
                % determinism information when deciding what can never succeed.
                Goal0 = _ - GoalInfo0,
                goal_info_get_context(GoalInfo0, Context),
                FailGoal = fail_goal_with_context(Context),
                conjoin_goal_and_rev_goal_list(FailGoal, !RevGoals)
            ),
            list.reverse(!.RevGoals, Goals)
        ;
            conjoin_goal_and_rev_goal_list(Goal1, !RevGoals),
            simplify_info_update_instmap(Goal1, !Info),
            simplify_conj(Goals0, !.RevGoals, Goals, ConjInfo, !Info, !IO)
        )
    ).

:- pred conjoin_goal_and_rev_goal_list(hlds_goal::in,
    hlds_goals::in, hlds_goals::out) is det.

conjoin_goal_and_rev_goal_list(Goal, RevGoals0, RevGoals) :-
    ( Goal = conj(plain_conj, Goals) - _ ->
        list.reverse(Goals, Goals1),
        list.append(Goals1, RevGoals0, RevGoals)
    ;
        RevGoals = [Goal | RevGoals0]
    ).

%-----------------------------------------------------------------------------%

:- pred simplify_par_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    simplify_info::in, simplify_info::in, simplify_info::out,
    io::di, io::uo) is det.

simplify_par_conj([], [], _, !Info, !IO).
simplify_par_conj([Goal0 |Goals0], [Goal | Goals], Info0, !Info, !IO) :-
    simplify_goal(Goal0, Goal, !Info, !IO),
    simplify_info_post_branch_update(Info0, !Info),
    simplify_par_conj(Goals0, Goals, Info0, !Info, !IO).

%-----------------------------------------------------------------------------%

:- pred excess_assigns_in_conj(hlds_goal_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    simplify_info::in, simplify_info::out) is det.

excess_assigns_in_conj(ConjInfo, Goals0, Goals, !Info) :-
    ( simplify_do_excess_assigns(!.Info) ->
        goal_info_get_nonlocals(ConjInfo, ConjNonLocals),
        map.init(Subn0),
        simplify_info_get_module_info(!.Info, ModuleInfo),
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_trace_level(Globals, TraceLevel),
        globals.lookup_bool_option(Globals, trace_optimized, TraceOptimized),
        simplify_info_get_varset(!.Info, VarSet0),
        find_excess_assigns_in_conj(TraceLevel, TraceOptimized,
            VarSet0, ConjNonLocals, Goals0, [], RevGoals, Subn0, Subn1),
        ( map.is_empty(Subn1) ->
            Goals = Goals0
        ;
            renaming_transitive_closure(Subn1, Subn),
            list.reverse(RevGoals, Goals1),
            MustSub = no,
            goal_util.rename_vars_in_goals(MustSub, Subn, Goals1, Goals),
            map.keys(Subn0, RemovedVars),
            varset.delete_vars(VarSet0, RemovedVars, VarSet),
            simplify_info_set_varset(VarSet, !Info),
            simplify_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
            apply_substitutions_to_rtti_varmaps(map.init, map.init, Subn,
                RttiVarMaps0, RttiVarMaps),
            simplify_info_set_rtti_varmaps(RttiVarMaps, !Info)
        )
    ;
        Goals = Goals0
    ).

:- type var_renaming == map(prog_var, prog_var).

:- pred find_excess_assigns_in_conj(trace_level::in, bool::in,
    prog_varset::in, set(prog_var)::in, list(hlds_goal)::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    var_renaming::in, var_renaming::out) is det.

find_excess_assigns_in_conj(_, _, _, _, [], !RevGoals, !Subn).
find_excess_assigns_in_conj(Trace, TraceOptimized, VarSet, ConjNonLocals,
        [Goal | Goals], !RevGoals, !Subn) :-
    (
        goal_is_excess_assign(Trace, TraceOptimized, VarSet, ConjNonLocals,
            Goal, !Subn)
    ->
        true
    ;
        !:RevGoals = [Goal | !.RevGoals]
    ),
    find_excess_assigns_in_conj(Trace, TraceOptimized, VarSet, ConjNonLocals,
        Goals, !RevGoals, !Subn).

:- pred goal_is_excess_assign(trace_level::in, bool::in, prog_varset::in,
    set(prog_var)::in, hlds_goal::in, var_renaming::in,
    var_renaming::out) is semidet.

goal_is_excess_assign(Trace, TraceOptimized, VarSet, ConjNonLocals, Goal0,
        !Subn) :-
    Goal0 = unify(_, _, _, Unif, _) - _,
    Unif = assign(LeftVar0, RightVar0),

    % Check if we've already substituted one or both of the variables.
    find_renamed_var(!.Subn, LeftVar0, LeftVar),
    find_renamed_var(!.Subn, RightVar0, RightVar),

    CanElimLeft = ( set.member(LeftVar, ConjNonLocals) -> no ; yes ),
    CanElimRight = ( set.member(RightVar, ConjNonLocals) -> no ; yes ),

    % If we have a choice, eliminate an unnamed variable.
    ( CanElimLeft = yes, CanElimRight = yes ->
        ( var_is_named(VarSet, LeftVar) ->
            ElimVar = RightVar,
            ReplacementVar = LeftVar
        ;
            ElimVar = LeftVar,
            ReplacementVar = RightVar
        )
    ; CanElimLeft = yes ->
        ElimVar = LeftVar,
        ReplacementVar = RightVar
    ; CanElimRight = yes ->
        ElimVar = RightVar,
        ReplacementVar = LeftVar
    ;
        fail
    ),
    map.det_insert(!.Subn, ElimVar, ReplacementVar, !:Subn),

    % If the module is being compiled with `--trace deep' and
    % `--no-trace-optimized' don't replace a meaningful variable name
    % with `HeadVar__n' or an anonymous variable.
    \+ (
        trace_level_needs_meaningful_var_names(Trace) = yes,
        TraceOptimized = no,
        var_is_named(VarSet, ElimVar),
        \+ var_is_named(VarSet, ReplacementVar)
    ).

:- pred var_is_named(prog_varset::in, prog_var::in) is semidet.

var_is_named(VarSet, Var) :-
    varset.search_name(VarSet, Var, Name),
    \+ (
        string.append("HeadVar__", Suffix, Name),
        string.to_int(Suffix, _)
    ).

:- pred find_renamed_var(var_renaming::in, prog_var::in, prog_var::out) is det.

find_renamed_var(Subn, Var0, Var) :-
    ( map.search(Subn, Var0, Var1) ->
        find_renamed_var(Subn, Var1, Var)
    ;
        Var = Var0
    ).

    % Collapse chains of renamings.
    %
:- pred renaming_transitive_closure(var_renaming::in, var_renaming::out)
    is det.

renaming_transitive_closure(VarRenaming0, VarRenaming) :-
    map.map_values(
        (pred(_::in, Value0::in, Value::out) is det :-
            find_renamed_var(VarRenaming0, Value0, Value)
        ), VarRenaming0, VarRenaming).

%-----------------------------------------------------------------------------%

:- pred simplify_switch(prog_var::in, list(case)::in, list(case)::in,
    list(case)::out, list(instmap_delta)::in, list(instmap_delta)::out,
    can_fail::in, can_fail::out, simplify_info::in,
    simplify_info::in, simplify_info::out, io::di, io::uo) is det.

simplify_switch(_, [], RevCases, Cases, !InstMaps, !CanFail, _, !Info, !IO) :-
    list.reverse(RevCases, Cases).
simplify_switch(Var, [Case0 | Cases0], RevCases0, Cases, !InstMaps,
        !CanFail, Info0, !Info, !IO) :-
    simplify_info_get_instmap(Info0, InstMap0),
    Case0 = case(ConsId, Goal0),
    simplify_info_get_module_info(!.Info, ModuleInfo0),
    simplify_info_get_var_types(!.Info, VarTypes),
    map.lookup(VarTypes, Var, Type),
    instmap.bind_var_to_functor(Var, Type, ConsId, InstMap0, InstMap1,
        ModuleInfo0, ModuleInfo1),
    simplify_info_set_module_info(ModuleInfo1, !Info),
    simplify_info_set_instmap(InstMap1, !Info),
    simplify_goal(Goal0, Goal, !Info, !IO),

        % Remove failing branches.
    ( Goal = disj([]) - _ ->
        RevCases = RevCases0,
        !:CanFail = can_fail
    ;
        Case = case(ConsId, Goal),
        Goal = _ - GoalInfo,

        %
        % Make sure the switched on variable appears in the instmap delta.
        % This avoids an abort in merge_instmap_delta if another branch
        % further instantiates the switched-on variable. If the switched on
        % variable does not appear in this branch's instmap_delta, the inst
        % before the goal would be used, resulting in a mode error.
        %
        goal_info_get_instmap_delta(GoalInfo, InstMapDelta0),
        simplify_info_get_module_info(!.Info, ModuleInfo2),
        instmap_delta_bind_var_to_functor(Var, Type, ConsId,
            InstMap0, InstMapDelta0, InstMapDelta, ModuleInfo2, ModuleInfo),
        simplify_info_set_module_info(ModuleInfo, !Info),

        !:InstMaps = [InstMapDelta | !.InstMaps],
        RevCases = [Case | RevCases0]
    ),

    simplify_info_post_branch_update(Info0, !Info),
    simplify_switch(Var, Cases0, RevCases, Cases, !InstMaps, !CanFail, Info0,
        !Info, !IO).

    % Create a semidet unification at the start of a singleton case
    % in a can_fail switch.
    % This will abort if the cons_id is existentially typed.
    %
:- pred create_test_unification(prog_var::in, cons_id::in, int::in,
    hlds_goal::out, simplify_info::in, simplify_info::out) is det.

create_test_unification(Var, ConsId, ConsArity, ExtraGoal - ExtraGoalInfo,
        !Info) :-
    simplify_info_get_varset(!.Info, VarSet0),
    simplify_info_get_var_types(!.Info, VarTypes0),
    varset.new_vars(VarSet0, ConsArity, ArgVars, VarSet),
    map.lookup(VarTypes0, Var, VarType),
    simplify_info_get_module_info(!.Info, ModuleInfo),
    type_util.get_cons_id_arg_types(ModuleInfo, VarType, ConsId, ArgTypes),
    map.det_insert_from_corresponding_lists(VarTypes0, ArgVars,
        ArgTypes, VarTypes),
    simplify_info_set_varset(VarSet, !Info),
    simplify_info_set_var_types(VarTypes, !Info),
    simplify_info_get_instmap(!.Info, InstMap),
    instmap.lookup_var(InstMap, Var, Inst0),
    (
        inst_expand(ModuleInfo, Inst0, Inst1),
        get_arg_insts(Inst1, ConsId, ConsArity, ArgInsts1)
    ->
        ArgInsts = ArgInsts1
    ;
        unexpected(this_file, "create_test_unification - get_arg_insts failed")
    ),
    InstToUniMode =
        (pred(ArgInst::in, ArgUniMode::out) is det :-
            ArgUniMode = ((ArgInst - free) -> (ArgInst - ArgInst))
        ),
    list.map(InstToUniMode, ArgInsts, UniModes),
    UniMode = (Inst0 -> Inst0) - (Inst0 -> Inst0),
    UnifyContext = unify_context(explicit, []),
    Unification = deconstruct(Var, ConsId, ArgVars, UniModes, can_fail,
        cannot_cgc),
    ExtraGoal = unify(Var, functor(ConsId, no, ArgVars),
        UniMode, Unification, UnifyContext),
    set.singleton_set(NonLocals, Var),

    % The test can't bind any variables, so the InstMapDelta should be empty.
    instmap_delta_init_reachable(InstMapDelta),
    goal_info_init(NonLocals, InstMapDelta, semidet, purity_pure,
        ExtraGoalInfo).

%-----------------------------------------------------------------------------%

:- pred simplify_disj(list(hlds_goal)::in, list(hlds_goal)::in,
    list(hlds_goal)::out,
    list(instmap_delta)::in, list(instmap_delta)::out,
    simplify_info::in, simplify_info::in, simplify_info::out,
    io::di, io::uo) is det.

simplify_disj([], RevGoals, Goals, !PostBranchInstMaps, _, !Info, !IO) :-
    list.reverse(RevGoals, Goals).
simplify_disj([Goal0 | Goals0], RevGoals0, Goals, !PostBranchInstMaps,
        Info0, !Info, !IO) :-
    simplify_goal(Goal0, Goal, !Info, !IO),
    Goal = _ - GoalInfo,

    (
        % Don't prune or warn about impure disjuncts that can't succeed.
        \+ goal_info_is_impure(GoalInfo),
        goal_info_get_determinism(GoalInfo, Detism),
        determinism_components(Detism, _CanFail, MaxSolns),
        MaxSolns = at_most_zero
    ->
        (
            simplify_do_warn(!.Info),
            % Don't warn where the initial goal was fail, since that can result
            % from mode analysis pruning away cases in a switch which cannot
            % succeed due to sub-typing in the modes.
            Goal0 \= disj([]) - _
        ->
            goal_info_get_context(GoalInfo, Context),
            Msg = zero_soln_disjunct,
            ContextMsg = context_det_msg(Context, Msg),
            simplify_info_add_det_msg(ContextMsg, !Info)
        ;
            true
        ),

        % Prune away non-succeeding disjuncts where possible.
        (
            (
                Goal0 = disj([]) - _
            ;
                % Only remove disjuncts that might loop
                % or call error/1 if --no-fully-strict.
                simplify_info_get_det_info(!.Info, DetInfo),
                det_info_get_fully_strict(DetInfo, no)
            )
        ->
            RevGoals1 = RevGoals0
        ;
            RevGoals1 = [Goal | RevGoals0],
            goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
            !:PostBranchInstMaps = [InstMapDelta | !.PostBranchInstMaps]
        )
    ;
        RevGoals1 = [Goal | RevGoals0],
        goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
        !:PostBranchInstMaps = [InstMapDelta | !.PostBranchInstMaps]
    ),

    simplify_info_post_branch_update(Info0, !Info),
    simplify_disj(Goals0, RevGoals1, Goals, !PostBranchInstMaps, Info0, !Info,
        !IO).

    % Disjunctions that cannot succeed more than once when viewed from the
    % outside generally need some fixing up, and/or some warnings to be issued.
    %
    % We previously converted them all to if-then-elses using the code below,
    % however converting disjs that have output variables but that nevertheless
    % cannot succeed more than one (e.g. cc_nondet or cc_multi disjs) into
    % if-then-elses may cause problems with other parts of the compiler that
    % assume that an if-then-else is mode-correct, i.e. that the condition
    % doesn't bind variables.
    %
    %       goal_info_get_determinism(GoalInfo, Detism),
    %       determinism_components(Detism, _CanFail, MaxSoln),
    %       MaxSoln \= at_most_many
    %   ->
    %       goal_info_get_instmap_delta(GoalInfo, DeltaInstMap),
    %       goal_info_get_nonlocals(GoalInfo, NonLocalVars),
    %       (
    %           det_no_output_vars(NonLocalVars, InstMap0,
    %               DeltaInstMap, DetInfo)
    %       ->
    %           OutputVars = no
    %       ;
    %           OutputVars = yes
    %       ),
    %       fixup_disj(Disjuncts, Detism, OutputVars, GoalInfo, InstMap0,
    %           DetInfo, Goal, MsgsA, Msgs)
    %   ;
    %
:- pred fixup_disj(list(hlds_goal)::in, determinism::in, bool::in,
    hlds_goal_info::in, hlds_goal_expr::out,
    simplify_info::in, simplify_info::out, io::di, io::uo) is det.

fixup_disj(Disjuncts, _, _OutputVars, GoalInfo, Goal, !Info, !IO) :-
    det_disj_to_ite(Disjuncts, GoalInfo, IfThenElse),
    simplify_goal(IfThenElse, Simplified, !Info, !IO),
    Simplified = Goal - _.

    % det_disj_to_ite is used to transform disjunctions that occur
    % in prunable contexts into if-then-elses.
    % For example, it would transform
    %
    %   ( Disjunct1
    %   ; Disjunct2
    %   ; Disjunct3
    %   )
    % into
    %   ( Disjunct1 ->
    %       true
    %   ; Disjunct2 ->
    %       true
    %   ;
    %       Disjunct3
    %   ).
    %
:- pred det_disj_to_ite(list(hlds_goal)::in, hlds_goal_info::in,
    hlds_goal::out) is det.

det_disj_to_ite([], _GoalInfo, _) :-
    unexpected(this_file, "reached base case of det_disj_to_ite").
det_disj_to_ite([Disjunct | Disjuncts], GoalInfo, Goal) :-
    (
        Disjuncts = [],
        Goal = Disjunct
    ;
        Disjuncts = [_ | _],
        Cond = Disjunct,
        Cond = _CondGoal - CondGoalInfo,

        Then = true_goal,

        det_disj_to_ite(Disjuncts, GoalInfo, Rest),
        Rest = _RestGoal - RestGoalInfo,

        goal_info_get_nonlocals(CondGoalInfo, CondNonLocals),
        goal_info_get_nonlocals(RestGoalInfo, RestNonLocals),
        set.union(CondNonLocals, RestNonLocals, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo, NewGoalInfo0),

        goal_info_get_instmap_delta(GoalInfo, InstMapDelta0),
        instmap_delta_restrict(NonLocals, InstMapDelta0, InstMapDelta),
        goal_info_set_instmap_delta(InstMapDelta, NewGoalInfo0, NewGoalInfo1),

        goal_info_get_determinism(CondGoalInfo, CondDetism),
        goal_info_get_determinism(RestGoalInfo, RestDetism),
        determinism_components(CondDetism, CondCanFail, CondMaxSoln),
        determinism_components(RestDetism, RestCanFail, RestMaxSoln),
        det_disjunction_canfail(CondCanFail, RestCanFail, CanFail),
        det_disjunction_maxsoln(CondMaxSoln, RestMaxSoln, MaxSoln0),
        ( MaxSoln0 = at_most_many ->
            MaxSoln = at_most_one
        ;
            MaxSoln = MaxSoln0
        ),
        determinism_components(Detism, CanFail, MaxSoln),
        goal_info_set_determinism(Detism, NewGoalInfo1, NewGoalInfo),

        Goal = if_then_else([], Cond, Then, Rest) - NewGoalInfo
    ).

%-----------------------------------------------------------------------------%

:- pred contains_multisoln_goal(list(hlds_goal)::in) is semidet.

contains_multisoln_goal(Goals) :-
    list.member(_Goal - GoalInfo, Goals),
    goal_info_get_determinism(GoalInfo, Detism),
    determinism_components(Detism, _, at_most_many).

%-----------------------------------------------------------------------------%

:- type simplify_info
    --->    simplify_info(
                det_info                :: det_info,
                msgs                    :: set(context_det_msg),
                simplifications         :: set(simplification),
                common_info             :: common_info,
                                        % Info about common subexpressions.
                instmap                 :: instmap,
                varset                  :: prog_varset,
                inst_varset             :: inst_varset,
                requantify              :: bool,
                                        % Does the goal need requantification.
                recompute_atomic        :: bool,
                                        % Do we need to recompute
                                        % instmap_deltas for atomic goals
                rerun_det               :: bool,
                                        % Does determinism analysis need to
                                        % be rerun.
                cost_delta              :: int,
                                        % Measure of the improvement in
                                        % the goal from simplification.
                lambdas                 :: int,
                                        % Count of the number of lambdas
                                        % which enclose the current goal.
                rtti_varmaps            :: rtti_varmaps,
                                        % Information about type_infos and
                                        % typeclass_infos.
                format_calls            :: bool
                                        % Do we have any calls to
                                        % string.format and io.format?
            ).

simplify_info_init(DetInfo, Simplifications, InstMap, ProcInfo, Info) :-
    proc_info_varset(ProcInfo, VarSet),
    proc_info_inst_varset(ProcInfo, InstVarSet),
    proc_info_rtti_varmaps(ProcInfo, RttiVarMaps),
    set.init(Msgs),
    set.list_to_set(Simplifications, SimplificationsSet),
    Info = simplify_info(DetInfo, Msgs, SimplificationsSet,
        common_info_init, InstMap, VarSet, InstVarSet,
        no, no, no, 0, 0, RttiVarMaps, no).

    % Reinitialise the simplify_info before reprocessing a goal.
    %
:- pred simplify_info_reinit(set(simplification)::in, instmap::in,
    simplify_info::in, simplify_info::out) is det.

simplify_info_reinit(Simplifications, InstMap0, !Info) :-
    !:Info = !.Info ^ simplifications := Simplifications,
    !:Info = !.Info ^ common_info := common_info_init,
    !:Info = !.Info ^ instmap := InstMap0,
    !:Info = !.Info ^ requantify := no,
    !:Info = !.Info ^ recompute_atomic := no,
    !:Info = !.Info ^ rerun_det := no,
    !:Info = !.Info ^ lambdas := 0.

    % exported for common.m
:- interface.

:- import_module parse_tree.prog_data.
:- import_module set.

:- pred simplify_info_init(det_info::in, list(simplification)::in,
    instmap::in, proc_info::in, simplify_info::out) is det.

:- pred simplify_info_get_det_info(simplify_info::in, det_info::out) is det.
:- pred simplify_info_get_det_msgs(simplify_info::in,
    set(context_det_msg)::out) is det.
:- pred simplify_info_get_simplifications(simplify_info::in,
    set(simplification)::out) is det.
:- pred simplify_info_get_common_info(simplify_info::in, common_info::out)
    is det.
:- pred simplify_info_get_instmap(simplify_info::in, instmap::out) is det.
:- pred simplify_info_get_varset(simplify_info::in, prog_varset::out) is det.
:- pred simplify_info_get_var_types(simplify_info::in, vartypes::out) is det.
:- pred simplify_info_requantify(simplify_info::in) is semidet.
:- pred simplify_info_recompute_atomic(simplify_info::in) is semidet.
:- pred simplify_info_rerun_det(simplify_info::in) is semidet.
:- pred simplify_info_get_cost_delta(simplify_info::in, int::out) is det.
:- pred simplify_info_get_rtti_varmaps(simplify_info::in, rtti_varmaps::out)
    is det.

:- pred simplify_info_get_module_info(simplify_info::in, module_info::out)
    is det.
:- pred simplify_info_get_pred_info(simplify_info::in, pred_info::out) is det.

:- pred simplify_info_set_common_info(common_info::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_requantify(
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_rerun_det(
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_rtti_varmaps(rtti_varmaps::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_do_add_det_msg(context_det_msg::in,
    simplify_info::in, simplify_info::out) is det.

:- pred simplify_info_incr_cost_delta(int::in,
    simplify_info::in, simplify_info::out) is det.

:- pred simplify_info_apply_type_substitution(tsubst::in,
    simplify_info::in, simplify_info::out) is det.

:- implementation.

:- pred simplify_info_get_format_calls(simplify_info::in, bool::out) is det.

simplify_info_get_det_info(Info, Info ^ det_info).
simplify_info_get_det_msgs(Info, Info ^ msgs).
simplify_info_get_simplifications(Info, Info ^ simplifications).
simplify_info_get_common_info(Info, Info ^ common_info).
simplify_info_get_instmap(Info, Info ^ instmap).
simplify_info_get_varset(Info, Info ^ varset).
simplify_info_get_var_types(Info, VarTypes) :-
    det_info_get_vartypes(Info ^ det_info, VarTypes).
simplify_info_requantify(Info) :-
    Info ^ requantify = yes.
simplify_info_recompute_atomic(Info) :-
    Info ^ recompute_atomic = yes.
simplify_info_rerun_det(Info) :-
    Info ^ rerun_det = yes.
simplify_info_get_cost_delta(Info, Info ^ cost_delta).
simplify_info_get_rtti_varmaps(Info, Info ^ rtti_varmaps).
simplify_info_get_format_calls(Info, Info ^ format_calls).

simplify_info_get_module_info(Info, ModuleInfo) :-
    simplify_info_get_det_info(Info, DetInfo),
    det_info_get_module_info(DetInfo, ModuleInfo).

simplify_info_get_pred_info(Info, PredInfo) :-
    simplify_info_get_det_info(Info, DetInfo),
    det_info_get_module_info(DetInfo, ModuleInfo),
    det_info_get_pred_id(DetInfo, PredId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo).

:- pred simplify_info_set_det_info(det_info::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_det_msgs(set(context_det_msg)::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_simplifications(set(simplification)::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_instmap(instmap::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_varset(prog_varset::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_var_types(vartypes::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_recompute_atomic(
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_format_calls(bool::in,
    simplify_info::in, simplify_info::out) is det.

:- pred simplify_info_add_det_msg(context_det_msg::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_cost_delta(int::in,
    simplify_info::in, simplify_info::out) is det.

:- pred simplify_info_enter_lambda(simplify_info::in, simplify_info::out)
    is det.
:- pred simplify_info_leave_lambda(simplify_info::in, simplify_info::out)
    is det.
:- pred simplify_info_inside_lambda(simplify_info::in) is semidet.

:- pred simplify_info_set_module_info(module_info::in,
    simplify_info::in, simplify_info::out) is det.

simplify_info_set_det_info(Det, Info, Info ^ det_info := Det).
simplify_info_set_det_msgs(Msgs, Info, Info ^ msgs := Msgs).
simplify_info_set_simplifications(Simp, Info, Info ^ simplifications := Simp).
simplify_info_set_instmap(InstMap, Info, Info ^ instmap := InstMap).
simplify_info_set_common_info(Common, Info, Info ^ common_info := Common).
simplify_info_set_varset(VarSet, Info, Info ^ varset := VarSet).
simplify_info_set_var_types(VarTypes, Info, Info ^ det_info := DetInfo) :-
    det_info_set_vartypes(Info  ^  det_info, VarTypes, DetInfo).
simplify_info_set_requantify(Info, Info ^ requantify := yes).
simplify_info_set_recompute_atomic(Info, Info ^ recompute_atomic := yes).
simplify_info_set_rerun_det(Info, Info ^ rerun_det := yes).
simplify_info_set_cost_delta(Delta, Info, Info ^ cost_delta := Delta).
simplify_info_set_rtti_varmaps(Rtti, Info, Info ^ rtti_varmaps := Rtti).
simplify_info_set_format_calls(FC, Info, Info ^ format_calls := FC).

simplify_info_incr_cost_delta(Incr, Info,
    Info ^ cost_delta := Info ^ cost_delta + Incr).

simplify_info_add_det_msg(Msg, !Info) :-
    ( simplify_do_warn(!.Info) ->
        simplify_info_do_add_det_msg(Msg, !Info)
    ;
        true
    ).

simplify_info_do_add_det_msg(Msg, !Info) :-
    simplify_info_get_det_msgs(!.Info, Msgs0),
    set.insert(Msgs0, Msg, Msgs),
    simplify_info_set_det_msgs(Msgs, !Info).

simplify_info_enter_lambda(Info, Info ^ lambdas := Info ^ lambdas + 1).
simplify_info_leave_lambda(Info, Info ^ lambdas := LambdaCount) :-
    LambdaCount1 = Info ^ lambdas - 1,
    ( LambdaCount1 >= 0 ->
        LambdaCount = LambdaCount1
    ;
        unexpected(this_file,
            "simplify_info_leave_lambda: Left too many lambdas")
    ).
simplify_info_inside_lambda(Info) :-
    Info ^ lambdas > 0.

simplify_info_set_module_info(ModuleInfo, !Info) :-
    simplify_info_get_det_info(!.Info, DetInfo0),
    det_info_set_module_info(DetInfo0, ModuleInfo, DetInfo),
    simplify_info_set_det_info(DetInfo, !Info).

simplify_info_apply_type_substitution(TSubst, !Info) :-
    simplify_info_get_var_types(!.Info, VarTypes0),
    simplify_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    ApplyTSubst = (pred(_::in, T0::in, T::out) is det :-
            apply_rec_subst_to_type(TSubst, T0, T)
        ),
    map.map_values(ApplyTSubst, VarTypes0, VarTypes),
    apply_substitutions_to_rtti_varmaps(map.init, TSubst, map.init,
        RttiVarMaps0, RttiVarMaps),
    simplify_info_set_var_types(VarTypes, !Info),
    simplify_info_set_rtti_varmaps(RttiVarMaps, !Info).

:- interface.

:- pred simplify_do_warn(simplify_info::in) is semidet.
:- pred simplify_do_warn_calls(simplify_info::in) is semidet.
:- pred simplify_do_warn_obsolete(simplify_info::in) is semidet.
:- pred simplify_do_once(simplify_info::in) is semidet.
:- pred simplify_do_common(simplify_info::in) is semidet.
:- pred simplify_do_excess_assigns(simplify_info::in) is semidet.
:- pred simplify_do_calls(simplify_info::in) is semidet.
:- pred simplify_do_const_prop(simplify_info::in) is semidet.
:- pred simplify_do_more_common(simplify_info::in) is semidet.

:- implementation.

simplify_do_warn(Info) :-
    simplify_info_get_simplifications(Info, Simplifications),
    set.member(warn_simple_code, Simplifications).
simplify_do_warn_calls(Info) :-
    simplify_info_get_simplifications(Info, Simplifications),
    set.member(warn_duplicate_calls, Simplifications).
simplify_do_warn_obsolete(Info) :-
    simplify_info_get_simplifications(Info, Simplifications),
    set.member(warn_obsolete, Simplifications).
simplify_do_once(Info) :-
    simplify_info_get_simplifications(Info, Simplifications),
    set.member(do_once, Simplifications).
simplify_do_common(Info) :-
    simplify_info_get_simplifications(Info, Simplifications),
    set.member(common_struct, Simplifications).
simplify_do_excess_assigns(Info) :-
    simplify_info_get_simplifications(Info, Simplifications),
    set.member(excess_assigns, Simplifications).
simplify_do_calls(Info) :-
    simplify_info_get_simplifications(Info, Simplifications),
    set.member(duplicate_calls, Simplifications).
simplify_do_const_prop(Info) :-
    simplify_info_get_simplifications(Info, Simplifications),
    set.member(constant_prop, Simplifications).
simplify_do_more_common(Info) :-
    simplify_info_get_simplifications(Info, Simplifications),
    set.member(extra_common_struct, Simplifications).

:- pred simplify_info_update_instmap(hlds_goal::in,
    simplify_info::in, simplify_info::out) is det.

simplify_info_update_instmap(Goal, Info, Info ^ instmap := InstMap) :-
    update_instmap(Goal, Info ^ instmap, InstMap).

:- type before_after
    --->    before
    ;       after.

    % Clear the common_info structs accumulated since the last goal that
    % could cause a stack flush. This is done to avoid replacing a
    % deconstruction with assignments to the arguments where this
    % would cause more variables to be live across the stack flush.
    % Calls and construction unifications are not treated in this
    % way since it is nearly always better to optimize them away.
    % When doing deforestation, it may be better to remove
    % as many common structures as possible.
    %
:- pred simplify_info_maybe_clear_structs(before_after::in, hlds_goal::in,
    simplify_info::in, simplify_info::out) is det.

simplify_info_maybe_clear_structs(BeforeAfter, Goal, !Info) :-
    (
        simplify_do_common(!.Info),
        \+ simplify_do_more_common(!.Info),
        Goal = GoalExpr - _,
        will_flush(GoalExpr, BeforeAfter) = yes
    ->
        simplify_info_get_common_info(!.Info, CommonInfo0),
        common_info_clear_structs(CommonInfo0, CommonInfo),
        simplify_info_set_common_info(CommonInfo, !Info)
    ;
        true
    ).

    % Succeed if execution of the given goal cannot encounter a context
    % that causes any variable to be flushed to its stack slot or to a
    % register at the specified time.
    %
:- func will_flush(hlds_goal_expr, before_after) = bool.

will_flush(unify(_, _, _, Unify, _), _) = WillFlush :-
    ( Unify = complicated_unify(_, _, _) ->
        WillFlush = yes
    ;
        WillFlush = no
    ).
will_flush(call(_, _, _, BuiltinState, _, _), BeforeAfter) = WillFlush :-
    ( BuiltinState = inline_builtin ->
        WillFlush = no
    ;
        (
            BeforeAfter = before,
            WillFlush = no
        ;
            BeforeAfter = after,
            WillFlush = yes
        )
    ).
will_flush(generic_call(GenericCall, _, _, _), BeforeAfter) = WillFlush :-
    (
        GenericCall = higher_order(_, _, _, _),
        WillFlush0 = yes
    ;
        GenericCall = class_method(_, _, _, _),
        WillFlush0 = yes
    ;
        GenericCall = cast(_),
        WillFlush0 = no
    ),
    (
        BeforeAfter = before,
        WillFlush = no
    ;
        BeforeAfter = after,
        WillFlush = WillFlush0
    ).
will_flush(foreign_proc(_, _, _, _, _, _), BeforeAfter) = WillFlush :-
    (
        BeforeAfter = before,
        WillFlush = no
    ;
        BeforeAfter = after,
        WillFlush = yes
    ).
will_flush(conj(ConjType, _), _) = WillFlush :-
    (
        ConjType = plain_conj,
        WillFlush = no
    ;
        ConjType = parallel_conj,
        WillFlush = yes
    ).
will_flush(switch(_, _, _), _) = no.
will_flush(disj(_), BeforeAfter) = WillFlush :-
    (
        BeforeAfter = before,
        WillFlush = yes
    ;
        BeforeAfter = after,
        WillFlush = no
    ).
will_flush(if_then_else(_, _, _, _), BeforeAfter) = WillFlush :-
    (
        BeforeAfter = before,
        WillFlush = yes
    ;
        BeforeAfter = after,
        WillFlush = no
    ).
will_flush(not(_), _) = yes.
will_flush(scope(_, _), _) = no.
will_flush(shorthand(_), _) = _ :-
    % These should have been expanded out by now.
    unexpected(this_file, "will_flush: unexpected shorthand").

    % Reset the instmap and seen calls for the next branch.
    %
:- pred simplify_info_post_branch_update(simplify_info::in, simplify_info::in,
    simplify_info::out) is det.

simplify_info_post_branch_update(PreBranchInfo, PostBranchInfo0, Info) :-
    simplify_info_get_instmap(PreBranchInfo, InstMap),
    simplify_info_set_instmap(InstMap, PostBranchInfo0, PostBranchInfo1),
    simplify_info_get_common_info(PreBranchInfo, Common),
    simplify_info_set_common_info(Common, PostBranchInfo1, Info).

    % Undo updates to the simplify_info before redoing simplification
    % on a goal.
    %
:- pred simplify_info_undo_goal_updates(simplify_info::in, simplify_info::in,
    simplify_info::out) is det.

simplify_info_undo_goal_updates(Info0, !Info) :-
    simplify_info_get_common_info(Info0, CommonInfo0),
    simplify_info_set_common_info(CommonInfo0, !Info),
    simplify_info_get_instmap(Info0, InstMap),
    simplify_info_set_instmap(InstMap, !Info).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "simplify.m".

%-----------------------------------------------------------------------------%
:- end_module simplify.
%-----------------------------------------------------------------------------%
