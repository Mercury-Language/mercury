%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: add_trail_ops.m.
% Authors: fjh, juliensf.
%
% This module is an HLDS-to-HLDS transformation that inserts code to
% handle trailing. The module implements two ways of doing this:
%
% (1) by adding calls to impure predicates defined in
%     library/private_builtin.m, which in turn call macros defined in
%     runtime/mercury_trail.h.
%
% (2) by inserting foreign_procs that call the macros defined in
%     runtime/mercury_trail.h.
%
% There is a space/time tradeoff between these two methods, the second
% is generally faster but results in larger executables.
% The `--generate-trail-ops-inline' option can be used to control which
% of the methods is used.
%
% This pass is currently only used for the MLDS back-end.
% For some reason (perhaps efficiency?? or more likely just historical?),
% the LLDS back-end inserts the trail operations as it is generating
% LLDS code, rather than via an HLDS to HLDS transformation.
%
% See compiler/notes/trailing.html for more information about trailing
% in the Mercury implementation.
%
% This module also implements trail usage optimization for those backends
% that use it to implement trailing (see trailing_analysis.m for details).
%
% NOTE: it is important that passes following this one do not attempt
%       to reorder disjunctions. If trail usage optimization is being
%       performed and a disjunction is reordered then the trail might
%       be corrupted.
%
% TODO:
%       - explore the space/time tradeoff between the inlining and
%         non-inlining methods of implementing trailing.
%
%-----------------------------------------------------------------------------%
%
% XXX check goal_infos for correctness
%
%-----------------------------------------------------------------------------%

:- module ml_backend.add_trail_ops.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.optimization_options.

:- import_module bool.

%-----------------------------------------------------------------------------%

:- pred add_trail_ops(bool::in, maybe_gen_trail_ops_inline::in,
    module_info::in, proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.code_model.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.vartypes.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_mode.

:- import_module list.
:- import_module maybe.
:- import_module require.
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
                trail_varset        :: prog_varset,
                trail_var_types     :: vartypes,
                trail_module_info   :: module_info,
                opt_trail_usage     :: bool,
                inline_ops          :: maybe_gen_trail_ops_inline
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
    requantify_proc_general(ordinary_nonlocals_no_lambda, !Proc).

:- pred goal_add_trail_ops(hlds_goal::in, hlds_goal::out,
    trail_ops_info::in, trail_ops_info::out) is det.

goal_add_trail_ops(Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    goal_expr_add_trail_ops(GoalExpr0, GoalInfo, Goal, !Info).

:- pred goal_expr_add_trail_ops(hlds_goal_expr::in, hlds_goal_info::in,
    hlds_goal::out, trail_ops_info::in, trail_ops_info::out) is det.

goal_expr_add_trail_ops(GoalExpr0, GoalInfo0, Goal, !Info) :-
    (
        GoalExpr0 = conj(ConjType, Goals0),
        conj_add_trail_ops(Goals0, Goals, !Info),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Disjuncts0),
        (
            Disjuncts0 = [],
            GoalExpr = GoalExpr0
        ;
            Disjuncts0 = [_ | _],
            Context = goal_info_get_context(GoalInfo0),
            CodeModel = goal_info_get_code_model(GoalInfo0),

            % Allocate a new trail ticket so that we can restore things on
            % back-tracking.
            new_ticket_var(TicketVar, !Info),
            gen_store_ticket(TicketVar, Context, StoreTicketGoal, !.Info),
            disj_add_trail_ops(Disjuncts0, Disjuncts, is_first_disjunct,
                CodeModel, TicketVar, !Info),
            GoalExpr = conj(plain_conj,
                [StoreTicketGoal, hlds_goal(disj(Disjuncts), GoalInfo0)])
        ),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        cases_add_trail_ops(Cases0, Cases, !Info),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(InnerGoal),
        OuterGoalInfo = GoalInfo0,
        % We handle negations by converting them into if-then-elses:
        %   not(G)  ===>  (if G then fail else true)

        Context = goal_info_get_context(OuterGoalInfo),
        InnerGoal = hlds_goal(_, InnerGoalInfo),
        Determinism = goal_info_get_determinism(InnerGoalInfo),
        determinism_components(Determinism, _CanFail, NumSolns),
        True = true_goal_with_context(Context),
        Fail = fail_goal_with_context(Context),
        (
            NumSolns = at_most_zero,
            % The "then" part of the if-then-else will be unreachable, but to
            % preserve the invariants that the MLDS back-end relies on, we
            % need to make sure that it can't fail. So we use a call to
            % `private_builtin.unused' (which will call error/1) rather than
            % `fail' for the "then" part.
            trail_generate_call(!.Info, "unused", [],
                instmap_delta_bind_no_var, detism_det, purity_pure, Context,
                ThenGoal)
        ;
            ( NumSolns = at_most_one
            ; NumSolns = at_most_many
            ; NumSolns = at_most_many_cc
            ),
            ThenGoal = Fail
        ),
        NewOuterGoal = if_then_else([], InnerGoal, ThenGoal, True),
        goal_expr_add_trail_ops(NewOuterGoal, OuterGoalInfo, Goal, !Info)
    ;
        GoalExpr0 = scope(Reason, InnerGoal0),
        OuterGoalInfo = GoalInfo0,
        InnerGoal0 = hlds_goal(_, InnerGoalInfo),
        InnerCodeModel = goal_info_get_code_model(InnerGoalInfo),
        OuterCodeModel = goal_info_get_code_model(OuterGoalInfo),
        ( if
            InnerCodeModel = model_non,
            OuterCodeModel \= model_non
        then
            % Handle commits.

            % Before executing the goal, we save the ticket counter,
            % and allocate a new trail ticket.
            Context = goal_info_get_context(OuterGoalInfo),
            new_ticket_counter_var(SavedTicketCounterVar, !Info),
            new_ticket_var(TicketVar, !Info),
            gen_mark_ticket_stack(SavedTicketCounterVar, Context,
                MarkTicketStackGoal, !.Info),
            gen_store_ticket(TicketVar, Context, StoreTicketGoal, !.Info),

            % Next we execute the goal that we're committing across.
            goal_add_trail_ops(InnerGoal0, InnerGoal, !Info),

            % If the goal succeeds, then we have committed to that goal,
            % so we need to commit the trail entries and prune any trail
            % tickets that have been allocated since we saved the ticket
            % counter.
            gen_reset_ticket_commit(TicketVar, Context,
                ResetTicketCommitGoal, !.Info),
            gen_prune_tickets_to(SavedTicketCounterVar, Context,
                PruneTicketsToGoal, !.Info),

            % If the goal fails, then we should undo the trail entries and
            % discard this trail ticket before backtracking over it.
            gen_reset_ticket_undo(TicketVar, Context, ResetTicketUndoGoal,
                !.Info),
            gen_discard_ticket(Context, DiscardTicketGoal, !.Info),
            FailGoal = fail_goal_with_context(Context),

            % Put it all together.
            Goal2 = hlds_goal(scope(Reason, InnerGoal), OuterGoalInfo),
            SuccCode = hlds_goal(
                conj(plain_conj,
                    [Goal2, ResetTicketCommitGoal, PruneTicketsToGoal]),
                OuterGoalInfo),
            (
                OuterCodeModel = model_semi,
                FailGoal = hlds_goal(_, FailGoalInfo),
                FailCode = hlds_goal(
                    conj(plain_conj,
                        [ResetTicketUndoGoal, DiscardTicketGoal, FailGoal]),
                    FailGoalInfo),
                Goal3 = hlds_goal(disj([SuccCode, FailCode]), OuterGoalInfo)
            ;
                ( OuterCodeModel = model_det
                ; OuterCodeModel = model_non
                ),
                Goal3 = SuccCode
            ),
            GoalExpr =
                conj(plain_conj, [MarkTicketStackGoal, StoreTicketGoal, Goal3])
        else if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % The scope has no goals that either create choice points
            % or allocate dynamic terms.
            GoalExpr = scope(Reason, InnerGoal0)
        else
            goal_add_trail_ops(InnerGoal0, InnerGoal, !Info),
            GoalExpr = scope(Reason, InnerGoal)
        ),
        Goal = hlds_goal(GoalExpr, OuterGoalInfo)
    ;
        GoalExpr0 = if_then_else(ExistQVars, Cond0, Then0, Else0),
        goal_add_trail_ops(Cond0, Cond, !Info),
        goal_add_trail_ops(Then0, Then1, !Info),
        goal_add_trail_ops(Else0, Else1, !Info),

        % If the condition does not modify the trail and does not create
        % any choicepoints, then we can omit the trailing code around it.
        OptTrailUsage = !.Info ^ opt_trail_usage,
        Cond = hlds_goal(_, CondGoalInfo),
        CondCodeModel = goal_info_get_code_model(CondGoalInfo),
        ( if
            OptTrailUsage = yes,
            CondCodeModel \= model_non,
            goal_cannot_modify_trail(CondGoalInfo) = yes
        then
            GoalExpr = if_then_else(ExistQVars, Cond, Then1, Else1)
        else
            % Allocate a new trail ticket so that we can restore things if the
            % condition fails.

            new_ticket_var(TicketVar, !Info),
            Context = goal_info_get_context(GoalInfo0),
            gen_store_ticket(TicketVar, Context, StoreTicketGoal, !.Info),

            % Commit the trail ticket entries if the condition succeeds.
            Then1 = hlds_goal(_, Then1GoalInfo),
            (
                CondCodeModel = model_non,
                gen_reset_ticket_solve(TicketVar, Context,
                    ResetTicketSolveGoal, !.Info),
                Then = hlds_goal(
                    conj(plain_conj, [ResetTicketSolveGoal, Then1]),
                    Then1GoalInfo)
            ;
                ( CondCodeModel = model_det
                ; CondCodeModel = model_semi
                ),
                gen_reset_ticket_commit(TicketVar, Context,
                    ResetTicketCommitGoal, !.Info),
                gen_prune_ticket(Context, PruneTicketGoal, !.Info),
                Then = hlds_goal(
                    conj(plain_conj,
                        [ResetTicketCommitGoal, PruneTicketGoal, Then1]),
                    Then1GoalInfo)
            ),
            gen_reset_ticket_undo(TicketVar, Context, ResetTicketUndoGoal,
                !.Info),
            gen_discard_ticket(Context, DiscardTicketGoal, !.Info),
            Else1 = hlds_goal(_, Else1GoalInfo),
            Else = hlds_goal(
                conj(plain_conj,
                    [ResetTicketUndoGoal, DiscardTicketGoal, Else1]),
                Else1GoalInfo),
            IfThenElse = hlds_goal(
                if_then_else(ExistQVars, Cond, Then, Else),
                GoalInfo0),
            GoalExpr = conj(plain_conj, [StoreTicketGoal, IfThenElse])
        ),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ),
        Goal = hlds_goal(GoalExpr0, GoalInfo0)
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        Goal = hlds_goal(GoalExpr0, GoalInfo0)
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

:- pred conj_add_trail_ops(hlds_goals::in, hlds_goals::out,
    trail_ops_info::in, trail_ops_info::out) is det.

conj_add_trail_ops(Goals0, Goals, !Info) :-
    list.map_foldl(goal_add_trail_ops, Goals0, Goals, !Info).

:- pred disj_add_trail_ops(list(hlds_goal)::in, list(hlds_goal)::out,
    is_first_disjunct::in, code_model::in, prog_var::in,
    trail_ops_info::in, trail_ops_info::out) is det.

disj_add_trail_ops([], [], _, _, _, !Info).
disj_add_trail_ops([Goal0 | Goals0], [Goal | Goals], IsFirstBranch, CodeModel,
        TicketVar, !Info) :-
    Goal0 = hlds_goal(_, GoalInfo0),
    Context = goal_info_get_context(GoalInfo0),

    % First undo the effects of any earlier branches.
    (
        IsFirstBranch = is_first_disjunct,
        UndoList = []
    ;
        IsFirstBranch = is_not_first_disjunct,
        gen_reset_ticket_undo(TicketVar, Context, ResetTicketUndoGoal, !.Info),
        UndoList0 = [ResetTicketUndoGoal],
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
    goal_add_trail_ops(Goal0, Goal1, !Info),

    % For model_semi and model_det disjunctions, once we reach the end of
    % the disjunct goal, we're committing to this disjunct, so we need to
    % prune the trail ticket.
    (
        CodeModel = model_non,
        PruneList = []
    ;
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        gen_reset_ticket_commit(TicketVar, Context, ResetTicketCommitGoal,
            !.Info),
        gen_prune_ticket(Context, PruneTicketGoal, !.Info),
        PruneList = [ResetTicketCommitGoal, PruneTicketGoal]
    ),

    % Package up the stuff we built earlier.
    Goal1 = hlds_goal(_, GoalInfo1),
    conj_list_to_goal(UndoList ++ [Goal1] ++ PruneList, GoalInfo1, Goal),

    % Recursively handle the remaining disjuncts.
    disj_add_trail_ops(Goals0, Goals, is_not_first_disjunct, CodeModel,
        TicketVar, !Info).

:- pred cases_add_trail_ops(list(case)::in, list(case)::out,
    trail_ops_info::in, trail_ops_info::out) is det.

cases_add_trail_ops([], [], !Info).
cases_add_trail_ops([Case0 | Cases0], [Case | Cases], !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    goal_add_trail_ops(Goal0, Goal, !Info),
    Case = case(MainConsId, OtherConsIds, Goal),
    cases_add_trail_ops(Cases0, Cases, !Info).

%-----------------------------------------------------------------------------%

:- pred gen_store_ticket(prog_var::in, prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_store_ticket(TicketVar, Context, SaveTicketGoal, Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = do_not_gen_trail_ops_inline,
        trail_generate_call(Info, "store_ticket",
            [TicketVar], instmap_delta_bind_var(TicketVar),
            detism_det, purity_impure, Context, SaveTicketGoal)
    ;
        GenerateInline = gen_trail_ops_inline,
        Arg1 = foreign_arg(TicketVar,
            yes(foreign_arg_name_mode("Ticket", out_mode)),
            ticket_type, bp_native_if_possible),
        ForeignCode = "MR_store_ticket(Ticket);",
        trail_generate_foreign_proc(Info, "store_ticket", [Arg1],
            instmap_delta_bind_var(TicketVar), purity_impure, Context,
            ForeignCode, SaveTicketGoal)
    ).

:- pred gen_reset_ticket_undo(prog_var::in, prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_reset_ticket_undo(TicketVar, Context, ResetTicketGoal, Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = do_not_gen_trail_ops_inline,
        trail_generate_call(Info, "reset_ticket_undo",
            [TicketVar], instmap_delta_bind_no_var,
            detism_det, purity_impure, Context, ResetTicketGoal)
    ;
        GenerateInline = gen_trail_ops_inline,
        Arg1 = foreign_arg(TicketVar,
            yes(foreign_arg_name_mode("Ticket", in_mode)),
            ticket_type, bp_native_if_possible),
        ForeignCode = "MR_reset_ticket(Ticket, MR_undo);",
        trail_generate_foreign_proc(Info, "reset_ticket_undo", [Arg1],
            instmap_delta_bind_no_var, purity_impure, Context,
            ForeignCode, ResetTicketGoal)
    ).

:- pred gen_reset_ticket_solve(prog_var::in, prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_reset_ticket_solve(TicketVar, Context, ResetTicketGoal, Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = do_not_gen_trail_ops_inline,
        trail_generate_call(Info, "reset_ticket_solve",
            [TicketVar], instmap_delta_bind_no_var,
            detism_det, purity_impure, Context, ResetTicketGoal)
    ;
        GenerateInline = gen_trail_ops_inline,
        Arg1 = foreign_arg(TicketVar,
            yes(foreign_arg_name_mode("Ticket", in_mode)),
            ticket_type, bp_native_if_possible),
        ForeignCode = "MR_reset_ticket(Ticket, MR_solve);",
        trail_generate_foreign_proc(Info, "reset_ticket_solve", [Arg1],
            instmap_delta_bind_no_var, purity_impure, Context,
            ForeignCode, ResetTicketGoal)
    ).

:- pred gen_reset_ticket_commit(prog_var::in, prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_reset_ticket_commit(TicketVar, Context, ResetTicketGoal, Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = do_not_gen_trail_ops_inline,
        trail_generate_call(Info, "reset_ticket_commit",
            [TicketVar], instmap_delta_bind_no_var,
            detism_det, purity_impure, Context, ResetTicketGoal)
    ;
        GenerateInline = gen_trail_ops_inline,
        Arg1 = foreign_arg(TicketVar,
            yes(foreign_arg_name_mode("Ticket", in_mode)),
            ticket_type, bp_native_if_possible),
        ForeignCode = "MR_reset_ticket(Ticket, MR_commit);",
        trail_generate_foreign_proc(Info, "reset_ticket_commit", [Arg1],
            instmap_delta_bind_no_var, purity_impure, Context,
            ForeignCode, ResetTicketGoal)
    ).

:- pred gen_prune_ticket(prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_prune_ticket(Context, PruneTicketGoal, Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = do_not_gen_trail_ops_inline,
        trail_generate_call(Info, "prune_ticket",
            [], instmap_delta_bind_no_var,
            detism_det, purity_impure, Context, PruneTicketGoal)
    ;
        GenerateInline = gen_trail_ops_inline,
        ForeignCode = "MR_prune_ticket();",
        trail_generate_foreign_proc(Info, "prune_ticket", [],
            instmap_delta_bind_no_var, purity_impure, Context,
            ForeignCode, PruneTicketGoal)
    ).

:- pred gen_discard_ticket(prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_discard_ticket(Context, DiscardTicketGoal, Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = do_not_gen_trail_ops_inline,
        trail_generate_call(Info, "discard_ticket",
            [], instmap_delta_bind_no_var,
            detism_det, purity_impure, Context, DiscardTicketGoal)
    ;
        GenerateInline = gen_trail_ops_inline,
        ForeignCode = "MR_discard_ticket();",
        trail_generate_foreign_proc(Info, "discard_ticket", [],
            instmap_delta_bind_no_var, purity_impure, Context,
            ForeignCode, DiscardTicketGoal)
    ).

:- pred gen_mark_ticket_stack(prog_var::in, prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_mark_ticket_stack(SavedTicketCounterVar, Context, MarkTicketStackGoal,
        Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = do_not_gen_trail_ops_inline,
        trail_generate_call(Info, "mark_ticket_stack",
            [SavedTicketCounterVar], instmap_delta_bind_no_var,
            detism_det, purity_impure, Context, MarkTicketStackGoal)
    ;
        GenerateInline = gen_trail_ops_inline,
        Arg1 = foreign_arg(SavedTicketCounterVar,
            yes(foreign_arg_name_mode("TicketCounter", out_mode)),
            ticket_counter_type, bp_native_if_possible),
        ForeignCode = "MR_mark_ticket_stack(TicketCounter);",
        trail_generate_foreign_proc(Info, "mark_ticket_stack", [Arg1],
            instmap_delta_bind_no_var, purity_impure, Context,
            ForeignCode, MarkTicketStackGoal)
    ).

:- pred gen_prune_tickets_to(prog_var::in, prog_context::in, hlds_goal::out,
    trail_ops_info::in) is det.

gen_prune_tickets_to(SavedTicketCounterVar, Context, PruneTicketsToGoal,
        Info) :-
    GenerateInline = Info ^ inline_ops,
    (
        GenerateInline = do_not_gen_trail_ops_inline,
        trail_generate_call(Info, "prune_tickets_to",
            [SavedTicketCounterVar], instmap_delta_bind_no_var,
            detism_det, purity_impure, Context, PruneTicketsToGoal)
    ;
        GenerateInline = gen_trail_ops_inline,
        Arg1 = foreign_arg(SavedTicketCounterVar,
            yes(foreign_arg_name_mode("TicketCounter", in_mode)),
            ticket_counter_type, bp_native_if_possible),
        ForeignCode = "MR_prune_tickets_to(TicketCounter);",
        trail_generate_foreign_proc(Info, "prune_tickets_to", [Arg1],
            instmap_delta_bind_no_var, purity_impure, Context,
            ForeignCode, PruneTicketsToGoal)
    ).

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
    VarSet0 = !.Info ^ trail_varset,
    VarTypes0 = !.Info ^ trail_var_types,
    varset.new_named_var(Name, Var, VarSet0, VarSet),
    add_var_type(Var, Type, VarTypes0, VarTypes),
    !Info ^ trail_varset := VarSet,
    !Info ^ trail_var_types := VarTypes.

%-----------------------------------------------------------------------------%

:- func ticket_type = mer_type.

ticket_type = c_pointer_type.

:- func ticket_counter_type = mer_type.

ticket_counter_type = c_pointer_type.

%-----------------------------------------------------------------------------%

:- pred trail_generate_call(trail_ops_info::in, string::in, list(prog_var)::in,
    instmap_delta::in, determinism::in, purity::in, term.context::in,
    hlds_goal::out) is det.

trail_generate_call(Info, PredName, ArgVars, InstMapDelta, Detism,
        Purity, Context, CallGoal) :-
    ModuleInfo = Info ^ trail_module_info,
    generate_simple_call(ModuleInfo, mercury_private_builtin_module,
        PredName, pf_predicate, only_mode, Detism, Purity, [], ArgVars, [],
        InstMapDelta, Context, CallGoal).

%-----------------------------------------------------------------------------%

:- pred trail_generate_foreign_proc(trail_ops_info::in, string::in,
    list(foreign_arg)::in, instmap_delta::in, purity::in, term.context::in,
    string::in, hlds_goal::out) is det.

trail_generate_foreign_proc(Info, PredName, Args, InstMapDelta,
        Purity, Context, ForeignCode, ForeignProcGoal) :-
    ModuleInfo = Info ^ trail_module_info,
    PrivateBuiltinModule = mercury_private_builtin_module,
    Detism = detism_det,
    some [!ForeignProcAttrs] (
        % XXX handle other target languages here.
        !:ForeignProcAttrs = default_attributes(lang_c),
        set_may_call_mercury(proc_will_not_call_mercury, !ForeignProcAttrs),
        set_thread_safe(proc_thread_safe, !ForeignProcAttrs),
        FinalForeignProcAttrs = !.ForeignProcAttrs
    ),
    ExtraArgs  = [],
    MaybeTraceRuntimeCond = no,
    goal_util.generate_foreign_proc(ModuleInfo, PrivateBuiltinModule, PredName,
        pf_predicate, only_mode, Detism, Purity, FinalForeignProcAttrs,
        [], Args, ExtraArgs, MaybeTraceRuntimeCond, ForeignCode, [],
        InstMapDelta, Context, ForeignProcGoal).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.add_trail_ops.
%-----------------------------------------------------------------------------%
