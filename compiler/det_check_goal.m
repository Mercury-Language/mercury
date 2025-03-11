%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2013-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: det_check_goal.m.
% Author: zs.
%
% This module handles reporting of errors and warnings about goals
% not having the right determinism, as well as errors and warnings
% from some other related compiler passes such as simplify.
%
%---------------------------------------------------------------------------%

:- module check_hlds.det_check_goal.
:- interface.

:- import_module check_hlds.det_util.
:- import_module check_hlds.det_check_switch.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.error_sort.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % The given goal should have determinism Desired, but doesn't.
    % Find out what is wrong, and return a list of messages giving the causes.
    %
:- pred det_diagnose_goal_get_msgs(instmap::in, determinism::in, hlds_goal::in,
    list(error_msg)::out, det_info::in, det_info::out) is det.

    % det_diagnose_conj(InstMap0, FailingContexts, Desired, Goals,
    %   Msgs, !DetInfo):
    %
    % The conjunction Goals with initial instmap InstMap0 should have
    % determinism Desired, but doesn't. Find out what is wrong, and return
    % a list of messages giving the causes.
    %
    % det_diagnose_conj is used for both plain (i.e. sequential) and parallel
    % conjunctions.
    %
    % This predicate is exported to det_infer_goal, which uses it to diagnose
    % which subgoals cause a parallel conjunction to not deviate from being
    % model_det.
    %
:- pred det_diagnose_conj(instmap::in, list(switch_context)::in,
    determinism::in, list(hlds_goal)::in, list(error_msg_group)::out,
    det_info::in, det_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_name.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module bag.
:- import_module maybe.
:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

det_diagnose_goal_get_msgs(InstMap0, Desired, Goal, Msgs, !DetInfo) :-
    SwitchContexts = [],
    det_diagnose_goal(InstMap0, SwitchContexts, Desired, Goal,
        MsgGroups, !DetInfo),
    sort_error_msg_groups(MsgGroups, SortedMsgGroups),
    Msgs = flatten_error_msg_groups(SortedMsgGroups).

    % The given goal should have determinism Desired, but doesn't.
    % Find out what is wrong, and return a list of messages giving the causes.
    %
:- pred det_diagnose_goal(instmap::in, list(switch_context)::in,
    determinism::in, hlds_goal::in, list(error_msg_group)::out,
    det_info::in, det_info::out) is det.

det_diagnose_goal(InstMap0, SwitchContexts, Desired, Goal, MsgGroups,
        !DetInfo) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    Actual = goal_info_get_determinism(GoalInfo),
    compare_determinisms(Desired, Actual, CompareResult),
    (
        ( CompareResult = first_detism_tighter_than
        ; CompareResult = first_detism_incomparable
        ),
        det_diagnose_goal_expr(GoalExpr, GoalInfo, InstMap0, Desired, Actual,
            SwitchContexts, !DetInfo, MsgGroups)
    ;
        ( CompareResult = first_detism_same_as
        ; CompareResult = first_detism_looser_than
        ),
        MsgGroups = []
    ).

%---------------------------------------------------------------------------%

:- pred det_diagnose_goal_expr(hlds_goal_expr::in, hlds_goal_info::in,
    instmap::in, determinism::in, determinism::in, list(switch_context)::in,
    det_info::in, det_info::out, list(error_msg_group)::out) is det.

det_diagnose_goal_expr(GoalExpr, GoalInfo, InstMap0, Desired, Actual,
        SwitchContexts, !DetInfo, MsgGroups) :-
    (
        GoalExpr = unify(LHS, RHS, _, _, UnifyContext),
        Context = goal_info_get_context(GoalInfo),
        det_report_unify_context(is_first, is_last, UnifyContext, !.DetInfo,
            LHS, RHS, SurroundingContextPieces, GoalPieces),
        det_diagnose_primitive_goal(Desired, Actual, ProblemPieces),
        Pieces = SurroundingContextPieces ++
            [lower_case_next_if_not_first] ++ GoalPieces ++ ProblemPieces,
        MsgGroups = [error_msg_group(msg(Context, Pieces), [])]
    ;
        GoalExpr = plain_call(PredId, ProcId, _, _, CallContext, _),
        Context = goal_info_get_context(GoalInfo),
        det_report_call_context(CallContext, !.DetInfo, PredId, ProcId,
            AnyUnifyPieces, SurroundingContextPieces, GoalPieces),
        det_diagnose_primitive_goal(Desired, Actual, ProblemPieces),
        Pieces = AnyUnifyPieces ++ SurroundingContextPieces ++
            [lower_case_next_if_not_first] ++ GoalPieces ++ ProblemPieces,
        MsgGroups = [error_msg_group(msg(Context, Pieces), [])]
    ;
        GoalExpr = generic_call(GenericCall, _, _, _, _),
        Context = goal_info_get_context(GoalInfo),
        det_info_get_var_table(!.DetInfo, VarTable),
        VarNameSrc = vns_var_table(VarTable),
        GenericCallPieces = generic_call_to_pieces(print_ho_var_name,
            VarNameSrc, GenericCall),
        GoalPieces = color_as_subject(GenericCallPieces),
        det_diagnose_primitive_goal(Desired, Actual, ProblemPieces),
        Pieces = GoalPieces ++ ProblemPieces,
        MsgGroups = [error_msg_group(msg(Context, Pieces), [])]
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        Context = goal_info_get_context(GoalInfo),
        DesiredStr = determinism_to_string(Desired),
        Pieces = [words("Determinism declaration not satisfied."),
            words("Desired determinism is"), words(DesiredStr),
            suffix("."), nl],
        MsgGroups = [error_msg_group(msg(Context, Pieces), [])]
    ;
        GoalExpr = conj(_, Goals),
        det_diagnose_conj(InstMap0, SwitchContexts, Desired,
            Goals, MsgGroups, !DetInfo)
    ;
        GoalExpr = disj(Goals),
        % We use bags instead of sets because it is possible (though it is
        % *incredibly* rare) for more than one disjunct to have the same
        % context, and we don't want to mistake two possibly-successful
        % disjuncts that have the same context, which call for a message here,
        % with just one possibly-successful disjunct, which does not.
        det_diagnose_disj(InstMap0, SwitchContexts, Desired, Actual,
            Goals, SubMsgGroups, bag.init, DisjunctsWithSolnSet, !DetInfo),
        determinism_components(Desired, _, DesSolns),
        bag.to_list(DisjunctsWithSolnSet, DisjunctsWithSoln),
        ( if
            DesSolns \= at_most_many,
            DesSolns \= at_most_many_cc,
            DisjunctsWithSoln = [FirstContext | LaterContexts],
            LaterContexts = [_ | _]
        then
            det_diagnose_switch_context(!.DetInfo, SwitchContexts,
                NestingPieces),
            FirstDisjPieces = [lower_case_next_if_not_first,
                words("Disjunction has more than one disjunct"),
                words("with solutions."), nl],
            FirstMsg =
                msg(FirstContext, NestingPieces ++ FirstDisjPieces),
            LaterDisjPieces = [words("This later disjunct")] ++
                color_as_incorrect([words("may have a solution.")]) ++
                [nl],
            MakeLaterMsgs =
                ( func(LaterContext) = LaterMsg :-
                    LaterMsg = msg(LaterContext, LaterDisjPieces)
                ),
            list.sort(LaterContexts, SortedLaterContexts),
            LaterMsgs = list.map(MakeLaterMsgs, SortedLaterContexts),
            DisjMsgGroup = error_msg_group(FirstMsg, LaterMsgs),
            MsgGroups = [DisjMsgGroup | SubMsgGroups]
        else
            MsgGroups = SubMsgGroups
        )
    ;
        GoalExpr = switch(Var, SwitchCanFail, Cases),
        % The determinism of a switch is the worst of the determinism of each
        % of the cases. Also, if only a subset of the constructors are handled,
        % then it is semideterministic or worse - this is determined
        % in switch_detection.m and handled via the CanFail field.
        ( if
            SwitchCanFail = can_fail,
            determinism_components(Desired, cannot_fail, _)
        then
            Context = goal_info_get_context(GoalInfo),
            find_missing_cons_ids(!.DetInfo, yes(10), InstMap0, SwitchContexts,
                Var, Cases, NestingPieces, VarStr, MaybeMissingInfo),
            (
                MaybeMissingInfo = yes(MissingInfo),
                MissingInfo = missing_cons_id_info(_, _,
                    MainPieces, VerbosePieces),
                SwitchOnPieces = [lower_case_next_if_not_first,
                    words("The switch on")] ++
                    color_as_subject([quote(VarStr)]),
                NoCoverPieces = [words("does not cover")],
                append_prefix_and_maybe_verbose(yes(color_incorrect),
                    NestingPieces ++ SwitchOnPieces, NoCoverPieces,
                    MainPieces, VerbosePieces, Component)
            ;
                MaybeMissingInfo = no,
                NoCoverPieces = [lower_case_next_if_not_first,
                    words("The switch on"), fixed(VarStr)] ++
                    color_as_incorrect([words("can fail.")]) ++ [nl],
                Component = always(NestingPieces ++ NoCoverPieces)
            ),
            SwitchMsg = simple_msg(Context, [Component]),
            SwitchMsgGroups = [error_msg_group(SwitchMsg, [])]
        else
            SwitchMsgGroups = []
        ),
        det_info_get_var_table(!.DetInfo, VarTable),
        lookup_var_type(VarTable, Var, VarType),
        det_diagnose_switch_arms(InstMap0, SwitchContexts, Desired,
            Var, VarType, Cases, SubMsgGroups, !DetInfo),
        MsgGroups = SwitchMsgGroups ++ SubMsgGroups
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        determinism_components(Desired, _DesiredCanFail, DesiredSolns),
        Cond = hlds_goal(_CondGoal, CondInfo),
        CondDetism = goal_info_get_determinism(CondInfo),
        determinism_components(CondDetism, _CondCanFail, CondSolns),
        ( if
            CondSolns = at_most_many,
            DesiredSolns \= at_most_many
        then
            determinism_components(DesiredCond, can_fail, DesiredSolns),
            det_diagnose_goal(InstMap0, SwitchContexts, DesiredCond,
                Cond, MsgGroupsCond, !DetInfo)
        else
            MsgGroupsCond = []
        ),
        apply_goal_instmap_delta(Cond, InstMap0, InstMap1),
        det_diagnose_goal(InstMap1, SwitchContexts, Desired,
            Then, MsgGroupsThen, !DetInfo),
        det_diagnose_goal(InstMap0, SwitchContexts, Desired,
            Else, MsgGroupsElse, !DetInfo),
        MsgGroups = MsgGroupsCond ++ MsgGroupsThen ++ MsgGroupsElse
    ;
        GoalExpr = negation(_),
        determinism_components(Desired, DesiredCanFail, DesiredSolns),
        determinism_components(Actual, ActualCanFail, ActualSolns),
        ( if
            DesiredCanFail = cannot_fail,
            ActualCanFail = can_fail
        then
            Context = goal_info_get_context(GoalInfo),
            Pieces = [words("Negated goal can succeed."), nl],
            MsgGroups = [error_msg_group(msg(Context, Pieces), [])]
        else if
            DesiredSolns = at_most_zero,
            ActualSolns \= at_most_zero
        then
            Context = goal_info_get_context(GoalInfo),
            Pieces = [words("Negated goal can fail."), nl],
            MsgGroups = [error_msg_group(msg(Context, Pieces), [])]
        else
            MsgGroups = []
        )
    ;
        GoalExpr = scope(_, SubGoal),
        SubGoal = hlds_goal(_, SubGoalInfo),
        Internal = goal_info_get_determinism(SubGoalInfo),
        ( if Actual = Internal then
            InternalDesired = Desired
        else
            determinism_components(Desired, CanFail, _),
            determinism_components(InternalDesired, CanFail, at_most_many)
        ),
        det_diagnose_goal(InstMap0, SwitchContexts, InternalDesired,
            SubGoal, MsgGroups, !DetInfo)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            det_diagnose_goal(InstMap0, SwitchContexts, Desired,
                MainGoal, MainMsgGroups, !DetInfo),
            det_diagnose_orelse_goals(InstMap0, SwitchContexts, Desired,
                OrElseGoals, OrElseMsgGroups, !DetInfo),
            MsgGroups = MainMsgGroups ++ OrElseMsgGroups
        ;
            ShortHand = try_goal(_, _, SubGoal),
            det_diagnose_goal(InstMap0, SwitchContexts, Desired,
                SubGoal, MsgGroups, !DetInfo)
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

%---------------------------------------------------------------------------%

:- pred det_diagnose_primitive_goal(determinism::in, determinism::in,
    list(format_piece)::out) is det.

det_diagnose_primitive_goal(Desired, Actual, Pieces) :-
    determinism_components(Desired, DesiredCanFail, DesiredSolns),
    determinism_components(Actual, ActualCanFail, ActualSolns),
    compare_canfails(DesiredCanFail, ActualCanFail, CmpCanFail),
    (
        CmpCanFail = first_tighter_than,
        CanFailPieces = [words("can fail")]
    ;
        ( CmpCanFail = first_same_as
        ; CmpCanFail = first_looser_than
        ),
        CanFailPieces = []
    ),
    compare_solncounts(DesiredSolns, ActualSolns, CmpSolns),
    (
        CmpSolns = first_tighter_than,
        (
            DesiredSolns = at_most_one,
            SolnsPieces = [words("can succeed more than once")]
        ;
            ( DesiredSolns = at_most_zero
            ; DesiredSolns = at_most_many
            ; DesiredSolns = at_most_many_cc
            ),
            SolnsPieces = [words("can succeed")]
        )
    ;
        ( CmpSolns = first_same_as
        ; CmpSolns = first_looser_than
        ),
        SolnsPieces = []
    ),
    (
        CanFailPieces = [],
        SolnsPieces = [],
        ProblemPieces = []
    ;
        CanFailPieces = [],
        SolnsPieces = [_ | _],
        ProblemPieces = color_as_incorrect(SolnsPieces ++ [suffix(".")])
    ;
        CanFailPieces = [_ | _],
        SolnsPieces = [],
        ProblemPieces = color_as_incorrect(CanFailPieces ++ [suffix(".")])
    ;
        CanFailPieces = [_ | _],
        SolnsPieces = [_ | _],
        ProblemPieces = color_as_incorrect(CanFailPieces) ++
            [words("and")] ++
            color_as_incorrect(SolnsPieces ++ [suffix(".")])
    ),
    (
        ProblemPieces = [_ | _],
        Pieces = ProblemPieces
    ;
        ProblemPieces = [],
        DesiredPieces0 = [fixed(determinism_to_string(Desired)), suffix(",")],
        ActualPieces0 = [fixed(determinism_to_string(Actual)), suffix(".")],
        DesiredPieces = color_as_correct(DesiredPieces0),
        ActualPieces = color_as_incorrect(ActualPieces0),
        Pieces =
            [words("has unknown determinism problem;"), nl,
            words("desired determinism is")] ++ DesiredPieces ++ [nl] ++
            [words("while actual determinism is")] ++ ActualPieces ++ [nl]
    ).

det_diagnose_conj(_InstMap0, _SwitchContexts, _Desired, [], [], !DetInfo).
det_diagnose_conj(InstMap0, SwitchContexts, Desired,
        [Goal | Goals], MsgGroups, !DetInfo) :-
    det_diagnose_goal(InstMap0, SwitchContexts, Desired,
        Goal, HeadMsgGroups, !DetInfo),
    apply_goal_instmap_delta(Goal, InstMap0, InstMap1),
    det_diagnose_conj(InstMap1, SwitchContexts, Desired,
        Goals, TailMsgGroups, !DetInfo),
    MsgGroups = HeadMsgGroups ++ TailMsgGroups.

:- pred det_diagnose_disj(instmap::in, list(switch_context)::in,
    determinism::in, determinism::in,
    list(hlds_goal)::in, list(error_msg_group)::out,
    bag(prog_context)::in, bag(prog_context)::out,
    det_info::in, det_info::out) is det.

det_diagnose_disj(_InstMap0, _SwitchContexts, _Desired, _Actual,
        [], [], !DisjunctsWithSoln, !DetInfo).
det_diagnose_disj(InstMap0, SwitchContexts, Desired, Actual,
        [Goal | Goals], MsgGroups, !DisjunctsWithSoln, !DetInfo) :-
    determinism_components(Actual, ActualCanFail, _),
    determinism_components(Desired, DesiredCanFail, DesiredSolns),
    ( if
        DesiredCanFail = cannot_fail,
        ActualCanFail = can_fail
    then
        % If the disjunction was declared to never fail, but we inferred that
        % it might fail, then we want to print an error message for every
        % disjunct that might fail.
        ClauseCanFail = cannot_fail
    else
        % Otherwise, either the disjunction is allowed to fail, or there is
        % at least one disjunct that we inferred won't fail, so we don't want
        % any error messages for the disjuncts that might fail.
        ClauseCanFail = can_fail
    ),
    determinism_components(ClauseDesired, ClauseCanFail, DesiredSolns),
    det_diagnose_goal(InstMap0, SwitchContexts, ClauseDesired,
        Goal, HeadMsgGroups, !DetInfo),
    Goal = hlds_goal(_, GoalInfo),
    ( if
        GoalDetism = goal_info_get_determinism(GoalInfo),
        determinism_components(GoalDetism, _, at_most_zero)
    then
        true
    else
        GoalContext = goal_info_get_context(GoalInfo),
        bag.insert(GoalContext, !DisjunctsWithSoln)
    ),
    det_diagnose_disj(InstMap0, SwitchContexts, Desired, Actual,
        Goals, TailMsgGroups, !DisjunctsWithSoln, !DetInfo),
    MsgGroups = HeadMsgGroups ++ TailMsgGroups.

:- pred det_diagnose_switch_arms(instmap::in, list(switch_context)::in,
    determinism::in, prog_var::in, mer_type::in,
    list(case)::in, list(error_msg_group)::out,
    det_info::in, det_info::out) is det.

det_diagnose_switch_arms(_InstMap0, _Desired, _SwitchContexts, _Var, _VarType,
        [], [], !DetInfo).
det_diagnose_switch_arms(InstMap0, SwitchContexts0, Desired, Var, VarType,
        [Case | Cases], MsgGroups, !DetInfo) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    goal_to_conj_list(Goal, GoalSeq),
    find_switch_var_matches(GoalSeq, [Var], MainConsId, OtherConsIds,
        MainMatch, OtherMatches),
    NewSwitchContext = switch_context(Var, MainMatch, OtherMatches),
    SwitchContexts1 = [NewSwitchContext | SwitchContexts0],
    det_info_get_module_info(!.DetInfo, ModuleInfo0),
    bind_var_to_functors(Var, VarType, MainConsId, OtherConsIds,
        InstMap0, InstMap1, ModuleInfo0, ModuleInfo),
    det_info_set_module_info(ModuleInfo, !DetInfo),
    det_diagnose_goal(InstMap1, SwitchContexts1, Desired,
        Goal, HeadMsgGroups, !DetInfo),
    det_diagnose_switch_arms(InstMap0, SwitchContexts0, Desired, Var, VarType,
        Cases, TailMsgGroups, !DetInfo),
    MsgGroups = HeadMsgGroups ++ TailMsgGroups.

:- pred det_diagnose_orelse_goals(instmap::in, list(switch_context)::in,
    determinism::in, list(hlds_goal)::in, list(error_msg_group)::out,
    det_info::in, det_info::out) is det.

det_diagnose_orelse_goals(_InstMap, _SwitchContexts, _Desired,
        [], [], !DetInfo).
det_diagnose_orelse_goals(InstMap0, SwitchContexts0, Desired,
        [Goal | Goals], MsgGroups, !DetInfo) :-
    % XXX Once we start using STM in earnest, we should add something
    % representing "In orelse arm #n:" to the switch context.
    det_diagnose_goal(InstMap0, SwitchContexts0, Desired,
        Goal, HeadMsgGroups, !DetInfo),
    det_diagnose_orelse_goals(InstMap0, SwitchContexts0, Desired,
        Goals, TailMsgGroups, !DetInfo),
    MsgGroups = HeadMsgGroups ++ TailMsgGroups.

%---------------------------------------------------------------------------%

:- pred det_report_call_context(maybe(call_unify_context)::in, det_info::in,
    pred_id::in, proc_id::in, list(format_piece)::out,
    list(format_piece)::out, list(format_piece)::out) is det.

det_report_call_context(CallUnifyContext, DetInfo, PredId, ProcId,
        UnifyPieces, SurroundingUnifyContextPieces, GoalPieces) :-
    det_info_get_module_info(DetInfo, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_origin(PredInfo, Origin),

    % If the error was in a call to a type-specific unification predicate
    % (i.e. in the unification itself), then don't print out the predicate
    % name, just print out the context. If it wasn't, then print them
    % both out. (The latter can happen if there is a determinism error
    % in a function call inside some unification.)

    ( if Origin = origin_compiler(made_for_uci(spec_pred_unify, _)) then
        UnifyPieces = [],
        (
            CallUnifyContext = yes(call_unify_context(LHS, RHS, UC)),
            det_report_unify_context(is_first, is_last, UC, DetInfo,
                LHS, RHS, SurroundingUnifyContextPieces, GoalPieces)
        ;
            % This shouldn't happen; every call to a compiler generated
            % type-specific unification predicate should have a unify_context.
            CallUnifyContext = no,
            SurroundingUnifyContextPieces = [],
            GoalPieces = [words("Some weird unification"),
                words("(or explicit call to a"),
                words("type-specific unify predicate?)")]
        )
    else
        (
            CallUnifyContext = yes(call_unify_context(LHS, RHS, UC)),
            det_report_unify_context(is_first, is_not_last, UC,
                DetInfo, LHS, RHS, CallContextPieces, MainUnifyPieces),
            UnifyPieces = CallContextPieces ++
                MainUnifyPieces ++ [suffix(":"), nl]
        ;
            CallUnifyContext = no,
            UnifyPieces = []
        ),
        PredProcId = proc(PredId, ProcId),
        PredPieces = describe_one_proc_name_maybe_argmodes(ModuleInfo,
            output_mercury, yes(color_subject), should_module_qualify,
            [], PredProcId),
        SurroundingUnifyContextPieces = [],
        GoalPieces = [words("This call to")] ++ PredPieces
    ).

%---------------------------------------------------------------------------%

    % det_report_unify_context returns information about the context of an
    % error, i.e. where the error occurred.
    %
    % The first two arguments are boolean flags that specify whether this is
    % the first part of a sentence (in which case we start the error message
    % with a capital letter) and whether it is the last part (in which case we
    % omit the word "in" on the final "... in unification ...").
    %
    % We return SurroundingContextPieces and MainPieces separately. MainPieces
    % describes the unification whose unify_context is the third argument,
    % while SurroundingContextPieces describes the location of that
    % unification. Returning these two separately enables our caller,
    % if it so chooses, to color only MainPieces as incorrect.
    %
:- pred det_report_unify_context(is_first::in, is_last::in, unify_context::in,
    det_info::in, prog_var::in, unify_rhs::in,
    list(format_piece)::out, list(format_piece)::out) is det.

det_report_unify_context(!.First, Last, UnifyContext, DetInfo,
        LHSVar, RHS, SurroundingContextPieces, MainPieces) :-
    unify_context_first_to_pieces(!First, UnifyContext, _LastContextWord,
        [], SurroundingContextPieces),
    det_info_get_module_info(DetInfo, ModuleInfo),
    det_info_get_var_table(DetInfo, VarTable),
    (
        !.First = is_first,
        (
            Last = is_last,
            StartWords = "Unification"
        ;
            Last = is_not_last,
            StartWords = "In unification"
        )
    ;
        !.First = is_not_first,
        (
            Last = is_last,
            StartWords = "unification"
        ;
            Last = is_not_last,
            StartWords = "in unification"
        )
    ),
    lookup_var_entry(VarTable, LHSVar, LHSVarEntry),
    LHSVarRawName = LHSVarEntry ^ vte_name,
    ( if LHSVarRawName = "" then
        RHSStr = unify_rhs_to_string(ModuleInfo, VarTable,
            print_name_only, RHS),
        MainPieces = [words(StartWords), words("with")] ++
            color_as_subject([words(add_quotes(RHSStr))])
    else
        % LHSVarName may differ from LHSVarRawName; see
        % mercury_convert_var_name for details.
        LHSVarName = mercury_var_to_string(VarTable, print_name_only, LHSVar),
        ( if
            RHS = rhs_var(RHSVar),
            lookup_var_entry(VarTable, RHSVar, RHSVarEntry),
            RHSVarEntry ^ vte_name = ""
        then
            MainPieces = [words(StartWords), words("with")] ++
                color_as_subject([words(add_quotes(LHSVarName))])
        else
            RHSStr = unify_rhs_to_string(ModuleInfo, VarTable,
                print_name_only, RHS),
            MainPieces = [words(StartWords), words("of")] ++
                color_as_subject([words(add_quotes(LHSVarName))]) ++
                [words("and")] ++
                color_as_subject([words(add_quotes(RHSStr))])
        )
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.det_check_goal.
%---------------------------------------------------------------------------%
