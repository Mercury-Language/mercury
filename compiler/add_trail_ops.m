%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: add_trail_ops.m.
% Authors: fjh, juliensf.

% This module is an HLDS-to-HLDS transformation that inserts code to
% handle trailing.  The module implements two ways of doing this:
%
% (1) by adding calls to impure predicates defined in
%     library/private_builtin.m, which in turn call macros defined in
%     runtime/mercury_trail.h.
%
% (2) by inserting foreign_procs that call the macros defined in
%     runtime/mercury_trail.h.
%
% There is a space/time tradeoff between these two methods, the second
% is generally faster but results in larger executables.  The
% `--generate-trail-ops-inline' option can be used to control which
% of the methods is used.

% This pass is currently only used for the MLDS back-end.
% For some reason (perhaps efficiency?? or more likely just historical?),
% the LLDS back-end inserts the trail operations as it is generating
% LLDS code, rather than via an HLDS to HLDS transformation.
%
% See compiler/notes/trailing.html for more information about trailing
% in the Mercury implementation.

% This module also implements trail usage optimization for those backends
% that use it to implement trailing (see trailing_analysis.m for details).

% NOTE: it is important that passes following this one do not attempt
%       to reorder disjunctions.  If trail usage optimization is being
%       performed and a disjunction is reordered then the trail might
%       be corrupted.

% TODO:
%       - explore the space/time tradeoff between the inlining and
%         non-inlining methods of implementing trailing.

%-----------------------------------------------------------------------------%

% XXX check goal_infos for correctness

%-----------------------------------------------------------------------------%

:- module ml_backend.add_trail_ops.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module bool.

%-----------------------------------------------------------------------------%

:- pred add_trail_ops(bool::in, bool::in, module_info::in,
    proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.code_model.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % As we traverse the goal, we add new variables to hold the trail tickets
    % (i.e. saved values of the trail pointer) and the saved values of the
    % trail ticket counter. So we need to thread a varset and a vartypes
    % mapping through, to record the names and types of the new variables.
    %
    % We also keep the module_info around, so that we can use the predicate
    % table that it contains to lookup the pred_ids for the builtin procedures
    % that we insert calls to. We do not update the module_info as we're
    % traversing the goal.
    %
:- type trail_ops_info
    --->    trail_ops_info(
                varset          :: prog_varset,
                var_types       :: vartypes,
                module_info     :: module_info,
                opt_trail_usage :: bool,
                inline_ops      :: bool
            ).

add_trail_ops(OptTrailUsage, GenerateInline, ModuleInfo0, !Proc) :-
    proc_info_get_goal(!.Proc, Goal0),
    proc_info_get_varset(!.Proc, VarSet0),
    proc_info_get_vartypes(!.Proc, VarTypes0),
    TrailOpsInfo0 = trail_ops_info(VarSet0, VarTypes0, ModuleInfo0,
        OptTrailUsage, GenerateInline),
    goal_add_trail_ops(Goal0, Goal, TrailOpsInfo0, TrailOpsInfo),
    TrailOpsInfo = trail_ops_info(VarSet, VarTypes, _, _, _),
    proc_info_set_goal(Goal, !Proc),
    proc_info_set_varset(VarSet, !Proc),
    proc_info_set_vartypes(VarTypes, !Proc),
    % The code below does not maintain the non-local variables,
    % so we need to requantify.
    % XXX it would be more efficient to maintain them rather than
    % recomputing them every time.
    requantify_proc(!Proc).

:- pred goal_add_trail_ops(hlds_goal::in, hlds_goal::out,
    trail_ops_info::in, trail_ops_info::out) is det.

goal_add_trail_ops(!Goal, !Info) :-
    OptTrailUsage = !.Info ^ opt_trail_usage,
    !.Goal = GoalExpr0 - GoalInfo,
    (
        OptTrailUsage = yes,
        goal_cannot_modify_trail(GoalInfo) = yes
    ->
        % Don't add trail ops if the goal cannot modify the trail
        % and we are optimizing trail usage.
        true
    ;
        goal_expr_add_trail_ops(GoalExpr0, GoalInfo, !:Goal, !Info)
    ).

:- pred goal_expr_add_trail_ops(hlds_goal_expr::in, hlds_goal_info::in,
    hlds_goal::out, trail_ops_info::in, trail_ops_info::out) is det.

goal_expr_add_trail_ops(conj(ConjType, Goals0), GI, conj(ConjType, Goals) - GI,
        !Info) :-
    conj_add_trail_ops(Goals0, Goals, !Info).

goal_expr_add_trail_ops(disj([]), GI, disj([]) - GI, !Info).
goal_expr_add_trail_ops(disj(Goals0), GoalInfo, Goal - GoalInfo, !Info) :-
    Goals0 = [_ | _],

    goal_info_get_context(GoalInfo, Context),
    goal_info_get_code_model(GoalInfo, CodeModel),

    % Allocate a new trail ticket so that we can restore things on
    % back-tracking.
    new_ticket_var(TicketVar, !Info),
    gen_store_ticket(TicketVar, Context, StoreTicketGoal, !.Info),
    disj_add_trail_ops(Goals0, yes, no, CodeModel, TicketVar, Goals, !Info),
    Goal = conj(plain_conj, [StoreTicketGoal, disj(Goals) - GoalInfo]).

goal_expr_add_trail_ops(switch(A, B, Cases0), GI, switch(A, B, Cases) - GI,
        !Info) :-
    cases_add_trail_ops(Cases0, Cases, !Info).

goal_expr_add_trail_ops(negation(InnerGoal), OuterGoalInfo, Goal, !Info) :-
    %
    % We handle negations by converting them into if-then-elses:
    %   not(G)  ===>  (if G then fail else true)
    %
    goal_info_get_context(OuterGoalInfo, Context),
    InnerGoal = _ - InnerGoalInfo,
    goal_info_get_determinism(InnerGoalInfo, Determinism),
    determinism_components(Determinism, _CanFail, NumSolns),
    True = true_goal_with_context(Context),
    Fail = fail_goal_with_context(Context),
    ModuleInfo = !.Info ^ module_info,
    ( NumSolns = at_most_zero ->
        % The "then" part of the if-then-else will be unreachable, but to
        % preserve the invariants that the MLDS back-end relies on, we need to
        % make sure that it can't fail. So we use a call to
        % `private_builtin.unused' (which will call error/1) rather than
        % `fail' for the "then" part.
        mercury_private_builtin_module(PrivateBuiltin),
        generate_simple_call(PrivateBuiltin, "unused", predicate, only_mode,
            detism_det, purity_pure, [], [], [], ModuleInfo, Context, ThenGoal)
    ;
        ThenGoal = Fail
    ),
    NewOuterGoal = if_then_else([], InnerGoal, ThenGoal, True),
    goal_expr_add_trail_ops(NewOuterGoal, OuterGoalInfo, Goal, !Info).

goal_expr_add_trail_ops(scope(Reason, Goal0), OuterGoalInfo,
        Goal - OuterGoalInfo, !Info) :-
    Goal0 = _ - InnerGoalInfo,
    goal_info_get_code_model(InnerGoalInfo, InnerCodeModel),
    goal_info_get_code_model(OuterGoalInfo, OuterCodeModel),
    (
        InnerCodeModel = model_non,
        OuterCodeModel \= model_non
    ->
        % Handle commits.

        % Before executing the goal, we save the ticket counter,
        % and allocate a new trail ticket.
        goal_info_get_context(OuterGoalInfo, Context),
        new_ticket_counter_var(SavedTicketCounterVar, !Info),
        new_ticket_var(TicketVar, !Info),
        gen_mark_ticket_stack(SavedTicketCounterVar, Context,
            MarkTicketStackGoal, !.Info),
        gen_store_ticket(TicketVar, Context, StoreTicketGoal, !.Info),

        % Next we execute the goal that we're committing across.
        goal_add_trail_ops(Goal0, Goal1, !Info),

        % If the goal succeeds, then we have committed to that goal, so we need
        % to commit the trail entries and prune any trail tickets that have
        % been allocated since we saved the ticket counter.
        gen_reset_ticket_commit(TicketVar, Context,
            ResetTicketCommitGoal, !.Info),
        gen_prune_tickets_to(SavedTicketCounterVar, Context,
            PruneTicketsToGoal, !.Info),

        % If the goal fails, then we should undo the trail entries and
        % discard this trail ticket before backtracking over it.
        gen_reset_ticket_undo(TicketVar, Context, ResetTicketUndoGoal, !.Info),
        gen_discard_ticket(Context, DiscardTicketGoal, !.Info),
        FailGoal = fail_goal_with_context(Context),

        % Put it all together.
        Goal2 = scope(Reason, Goal1) - OuterGoalInfo,
        SuccCode = conj(plain_conj,
            [Goal2, ResetTicketCommitGoal, PruneTicketsToGoal])
            - OuterGoalInfo,
        ( OuterCodeModel = model_semi ->
            FailGoal = _ - FailGoalInfo,
            FailCode = conj(plain_conj,
                [ResetTicketUndoGoal, DiscardTicketGoal, FailGoal])
                - FailGoalInfo,
            Goal3 = disj([SuccCode, FailCode]) - OuterGoalInfo
        ;
            Goal3 = SuccCode
        ),
        Goal = conj(plain_conj, [MarkTicketStackGoal, StoreTicketGoal, Goal3])
    ;
        goal_add_trail_ops(Goal0, Goal1, !Info),
        Goal = scope(Reason, Goal1)
    ).

goal_expr_add_trail_ops(if_then_else(A, Cond0, Then0, Else0), GoalInfo,
        Goal - GoalInfo, !Info) :-
    goal_add_trail_ops(Cond0, Cond, !Info),
    goal_add_trail_ops(Then0, Then1, !Info),
    goal_add_trail_ops(Else0, Else1, !Info),

    % Allocate a new trail ticket so that we can restore things
    % if the condition fails.
    new_ticket_var(TicketVar, !Info),
    goal_info_get_context(GoalInfo, Context),
    gen_store_ticket(TicketVar, Context, StoreTicketGoal, !.Info),

    % Commit the trail ticket entries if the condition succeeds.
    Then1 = _ - Then1GoalInfo,
    Cond = _ - CondGoalInfo,
    goal_info_get_code_model(CondGoalInfo, CondCodeModel),
    ( CondCodeModel = model_non ->
        gen_reset_ticket_solve(TicketVar, Context, ResetTicketSolveGoal,
            !.Info),
        Then = conj(plain_conj, [ResetTicketSolveGoal, Then1]) - Then1GoalInfo
    ;
        gen_reset_ticket_commit(TicketVar, Context, ResetTicketCommitGoal,
            !.Info),
        gen_prune_ticket(Context, PruneTicketGoal, !.Info),
        Then = conj(plain_conj,
            [ResetTicketCommitGoal, PruneTicketGoal, Then1])
            - Then1GoalInfo
    ),
    gen_reset_ticket_undo(TicketVar, Context, ResetTicketUndoGoal, !.Info),
    gen_discard_ticket(Context, DiscardTicketGoal, !.Info),
    Else1 = _ - Else1GoalInfo,
    Else = conj(plain_conj, [ResetTicketUndoGoal, DiscardTicketGoal, Else1])
        - Else1GoalInfo,
    IfThenElse = if_then_else(A, Cond, Then, Else) - GoalInfo,
    Goal = conj(plain_conj, [StoreTicketGoal, IfThenElse]).

goal_expr_add_trail_ops(Goal @ plain_call(_, _, _, _, _, _), GI, Goal - GI,
        !Info).

goal_expr_add_trail_ops(Goal @ generic_call(_, _, _, _), GI, Goal - GI, !Info).

goal_expr_add_trail_ops(Goal @ unify(_, _, _, _, _), GI, Goal - GI, !Info).

goal_expr_add_trail_ops(PragmaForeign, GoalInfo, Goal, !Info) :-
    PragmaForeign = call_foreign_proc(_, _, _, _, _, _, Impl),
    ( Impl = fc_impl_model_non(_, _, _, _, _, _, _, _, _) ->
        % XXX Implementing trailing for nondet pragma foreign_code via
        % transformation is difficult, because there's nowhere in the HLDS
        % pragma_foreign_code goal where we can insert trailing operations.
        % For now, we don't support this. Instead, we just generate a call
        % to a procedure which will at runtime call error/1 with an appropriate
        % "Sorry, not implemented" error message.
        ModuleInfo = !.Info^ module_info,
        goal_info_get_context(GoalInfo, Context),
        trail_generate_call("trailed_nondet_pragma_foreign_code",
            detism_erroneous, purity_pure, [], [], ModuleInfo, Context,
            SorryNotImplementedCode),
        Goal = SorryNotImplementedCode
    ;
        Goal = PragmaForeign - GoalInfo
    ).

goal_expr_add_trail_ops(shorthand(_), _, _, !Info) :-
    % These should have been expanded out by now.
    unexpected(this_file, "goal_expr_add_trail_ops: unexpected shorthand").

:- pred conj_add_trail_ops(hlds_goals::in, hlds_goals::out,
    trail_ops_info::in, trail_ops_info::out) is det.

conj_add_trail_ops(Goals0, Goals, !Info) :-
    list.map_foldl(goal_add_trail_ops, Goals0, Goals, !Info).

:- pred disj_add_trail_ops(hlds_goals::in, bool::in, bool::in,
    code_model::in, prog_var::in, hlds_goals::out,
    trail_ops_info::in, trail_ops_info::out) is det.

disj_add_trail_ops([], _, _, _, _, [], !Info).
disj_add_trail_ops([Goal0 | Goals0], IsFirstBranch, PrevDisjunctModifiesTrail,
        CodeModel, TicketVar, [Goal | Goals], !Info) :-
    Goal0 = _ - GoalInfo0,
    goal_info_get_context(GoalInfo0, Context),

    % First undo the effects of any earlier branches.
    (
        IsFirstBranch = yes,
        UndoList = [],
        expect(unify(PrevDisjunctModifiesTrail, no), this_file,
            "PrevDisjunctModifiesTrail = yes for initial disjunct.")
    ;
        IsFirstBranch = no,
        %
        % We only need to undo the changes from the last disjunction if it
        % actually modified the trail.  We only do this if
        % `--optimize-trail-usage' is set.
        %
        (
            PrevDisjunctModifiesTrail = no,
            !.Info ^ opt_trail_usage = yes
        ->
            UndoList0 = []
        ;
            gen_reset_ticket_undo(TicketVar, Context, ResetTicketUndoGoal,
                !.Info),
            UndoList0 = [ResetTicketUndoGoal]
        ),
        (
            Goals0 = [],
            % Once we've reached the last disjunct, we can discard
            % the trail ticket.
            gen_discard_ticket(Context, DiscardTicketGoal, !.Info),
            UndoList = UndoList0 ++ [DiscardTicketGoal]
        ;
            Goals0 = [_ | _],
            UndoList = UndoList0
        )
    ),
    %
    % Add trailing code to the disjunct itself.  We can omit the trailing code
    % if the disjunct doesn't modify the trail and `--optimize-trail-usage' is
    % set.
    %
    ThisDisjunctModifiesTrail = goal_may_modify_trail(GoalInfo0),
    CanOmitTrailOps =
        not(ThisDisjunctModifiesTrail) `and` !.Info ^ opt_trail_usage,
    (
        CanOmitTrailOps = yes,
        Goal1 = Goal0
    ;
        CanOmitTrailOps = no,
        goal_add_trail_ops(Goal0, Goal1, !Info)
    ),
    %
    % For model_semi and model_det disjunctions, once we reach the end of
    % the disjunct goal, we're committing to this disjunct, so we need to
    % prune the trail ticket.
    %
    ( CodeModel = model_non ->
        PruneList = []
    ;
        gen_reset_ticket_commit(TicketVar, Context, ResetTicketCommitGoal,
            !.Info),
        gen_prune_ticket(Context, PruneTicketGoal, !.Info),
        PruneList = [ResetTicketCommitGoal, PruneTicketGoal]
    ),
    %
    % Package up the stuff we built earlier.
    %
    Goal1 = _ - GoalInfo1,
    conj_list_to_goal(UndoList ++ [Goal1] ++ PruneList, GoalInfo1, Goal),
    %
    % Recursively handle the remaining disjuncts.
    %
    disj_add_trail_ops(Goals0, no, ThisDisjunctModifiesTrail, CodeModel,
        TicketVar, Goals, !Info).

:- pred cases_add_trail_ops(list(case)::in, list(case)::out,
    trail_ops_info::in, trail_ops_info::out) is det.

cases_add_trail_ops([], [], !Info).
cases_add_trail_ops([Case0 | Cases0], [Case | Cases], !Info) :-
    Case0 = case(ConsId, Goal0),
    Case = case(ConsId, Goal),
    goal_add_trail_ops(Goal0, Goal, !Info),
    cases_add_trail_ops(Cases0, Cases, !Info).

%-----------------------------------------------------------------------------%

:- pred gen_store_ticket(prog_var::in, prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_store_ticket(TicketVar, Context, SaveTicketGoal, Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = no,
        trail_generate_call("store_ticket", detism_det, purity_impure,
            [TicketVar], [TicketVar - trail_ground_inst],
            Info ^ module_info, Context, SaveTicketGoal)
    ;
        GenerateInline =  yes,
        Args = [foreign_arg(TicketVar, yes("Ticket" - out_mode),
            ticket_type, native_if_possible)],
        ForeignCode = "MR_store_ticket(Ticket);",
        trail_generate_foreign_proc("store_ticket", purity_impure,
            [TicketVar - trail_ground_inst], Info ^ module_info, Context,
            Args, ForeignCode, SaveTicketGoal)
    ).

:- pred gen_reset_ticket_undo(prog_var::in, prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_reset_ticket_undo(TicketVar, Context, ResetTicketGoal, Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = no,
        trail_generate_call("reset_ticket_undo", detism_det, purity_impure,
            [TicketVar], [], Info ^ module_info, Context, ResetTicketGoal)
    ;
        GenerateInline = yes,
        Args = [foreign_arg(TicketVar, yes("Ticket" - in_mode),
            ticket_type, native_if_possible)],
        ForeignCode = "MR_reset_ticket(Ticket, MR_undo);",
        trail_generate_foreign_proc("reset_ticket_undo", purity_impure,
            [], Info ^ module_info, Context, Args, ForeignCode,
            ResetTicketGoal)
    ).

:- pred gen_reset_ticket_solve(prog_var::in, prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_reset_ticket_solve(TicketVar, Context, ResetTicketGoal, Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = no,
        trail_generate_call("reset_ticket_solve", detism_det, purity_impure,
            [TicketVar], [], Info ^ module_info, Context, ResetTicketGoal)
    ;
        GenerateInline = yes,
        Args = [foreign_arg(TicketVar, yes("Ticket" - in_mode),
            ticket_type, native_if_possible)],
        ForeignCode = "MR_reset_ticket(Ticket, MR_solve);",
        trail_generate_foreign_proc("reset_ticket_solve", purity_impure,
            [], Info ^ module_info, Context, Args, ForeignCode,
            ResetTicketGoal)
    ).

:- pred gen_reset_ticket_commit(prog_var::in, prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_reset_ticket_commit(TicketVar, Context, ResetTicketGoal, Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = no,
        trail_generate_call("reset_ticket_commit", detism_det, purity_impure,
            [TicketVar], [], Info ^ module_info, Context, ResetTicketGoal)
    ;
        GenerateInline = yes,
        Args = [foreign_arg(TicketVar, yes("Ticket" - in_mode),
            ticket_type, native_if_possible)],
        ForeignCode = "MR_reset_ticket(Ticket, MR_commit);",
        trail_generate_foreign_proc("reset_ticket_commit", purity_impure,
            [], Info ^ module_info, Context, Args, ForeignCode,
            ResetTicketGoal)
    ).

:- pred gen_prune_ticket(prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_prune_ticket(Context, PruneTicketGoal, Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = no,
        trail_generate_call("prune_ticket", detism_det, purity_impure,
            [], [], Info ^ module_info, Context, PruneTicketGoal)
    ;
        GenerateInline = yes,
        Args = [],
        ForeignCode = "MR_prune_ticket();",
        trail_generate_foreign_proc("prune_ticket", purity_impure,
            [], Info ^ module_info, Context, Args, ForeignCode,
            PruneTicketGoal)
    ).

:- pred gen_discard_ticket(prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_discard_ticket(Context, DiscardTicketGoal, Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = no,
        trail_generate_call("discard_ticket", detism_det, purity_impure,
            [], [], Info ^ module_info, Context, DiscardTicketGoal)
    ;
        GenerateInline = yes,
        Args = [],
        ForeignCode = "MR_discard_ticket();",
        trail_generate_foreign_proc("discard_ticket", purity_impure,
            [], Info ^ module_info, Context, Args, ForeignCode,
            DiscardTicketGoal)
    ).

:- pred gen_mark_ticket_stack(prog_var::in, prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_mark_ticket_stack(SavedTicketCounterVar, Context, MarkTicketStackGoal,
        Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = no,
        trail_generate_call("mark_ticket_stack", detism_det, purity_impure,
            [SavedTicketCounterVar], [], Info ^ module_info, Context,
            MarkTicketStackGoal)
    ;
        GenerateInline = yes,
        Args = [foreign_arg(SavedTicketCounterVar,
            yes("TicketCounter" - out_mode), ticket_counter_type,
            native_if_possible)],
        ForeignCode = "MR_mark_ticket_stack(TicketCounter);",
        trail_generate_foreign_proc("mark_ticket_stack", purity_impure,
            [], Info ^ module_info, Context, Args, ForeignCode,
            MarkTicketStackGoal)
    ).

:- pred gen_prune_tickets_to(prog_var::in, prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_prune_tickets_to(SavedTicketCounterVar, Context, PruneTicketsToGoal,
        Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = no,
        trail_generate_call("prune_tickets_to", detism_det, purity_impure,
            [SavedTicketCounterVar], [], Info ^ module_info, Context,
            PruneTicketsToGoal)
    ;
        GenerateInline = yes,
        Args = [foreign_arg(SavedTicketCounterVar,
            yes("TicketCounter" - in_mode), ticket_counter_type,
            native_if_possible)],
        ForeignCode = "MR_prune_tickets_to(TicketCounter);",
        trail_generate_foreign_proc("prune_tickets_to", purity_impure,
            [], Info ^ module_info, Context, Args, ForeignCode,
            PruneTicketsToGoal)
    ).

:- func trail_ground_inst = mer_inst.

trail_ground_inst = ground(unique, none).

%-----------------------------------------------------------------------------%

:- pred new_ticket_var(prog_var::out,
    trail_ops_info::in, trail_ops_info::out) is det.

new_ticket_var(Var, !Info) :-
    new_var("TrailTicket", ticket_type, Var, !Info).

:- pred new_ticket_counter_var(prog_var::out,
    trail_ops_info::in, trail_ops_info::out) is det.

new_ticket_counter_var(Var, !Info) :-
    new_var("SavedTicketCounter", ticket_counter_type, Var, !Info).

:- pred new_var(string::in, mer_type::in, prog_var::out,
    trail_ops_info::in, trail_ops_info::out) is det.

new_var(Name, Type, Var, !Info) :-
    VarSet0 = !.Info ^ varset,
    VarTypes0 = !.Info ^ var_types,
    varset.new_named_var(VarSet0, Name, Var, VarSet),
    map.det_insert(VarTypes0, Var, Type, VarTypes),
    !:Info = !.Info ^ varset := VarSet,
    !:Info = !.Info ^ var_types := VarTypes.

%-----------------------------------------------------------------------------%

:- func ticket_type = mer_type.

ticket_type = c_pointer_type.

:- func ticket_counter_type = mer_type.

ticket_counter_type = c_pointer_type.

%-----------------------------------------------------------------------------%

:- pred trail_generate_call(string::in, determinism::in, purity::in,
    list(prog_var)::in, assoc_list(prog_var, mer_inst)::in,
    module_info::in, term.context::in, hlds_goal::out) is det.

trail_generate_call(PredName, Detism, Purity, Args, InstMap, ModuleInfo,
        Context, CallGoal) :-
    mercury_private_builtin_module(BuiltinModule),
    goal_util.generate_simple_call(BuiltinModule, PredName, predicate,
        only_mode, Detism, Purity, Args, [], InstMap, ModuleInfo, Context,
        CallGoal).

%-----------------------------------------------------------------------------%

:- pred trail_generate_foreign_proc(string::in, purity::in,
    assoc_list(prog_var, mer_inst)::in, module_info::in, term.context::in,
    list(foreign_arg)::in, string::in, hlds_goal::out) is det.

trail_generate_foreign_proc(PredName, Purity, InstMap,
        ModuleInfo, Context, Args, ForeignCode, ForeignProcGoal) :-
    mercury_private_builtin_module(PrivateBuiltinModule),
    Detism = detism_det,
    some [!ForeignProcAttrs] (
        % XXX handle other target languages here.
        !:ForeignProcAttrs = default_attributes(lang_c),
        set_may_call_mercury(will_not_call_mercury, !ForeignProcAttrs),
        set_thread_safe(thread_safe, !ForeignProcAttrs),
        FinalForeignProcAttrs = !.ForeignProcAttrs
    ),
    ExtraArgs  = [],
    MaybeTraceRuntimeCond = no,
    goal_util.generate_foreign_proc(PrivateBuiltinModule, PredName,
        predicate, only_mode, Detism, Purity, FinalForeignProcAttrs, Args,
        ExtraArgs, MaybeTraceRuntimeCond, ForeignCode, [], InstMap,
        ModuleInfo, Context, ForeignProcGoal).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "add_trail_ops.m".

%-----------------------------------------------------------------------------%
