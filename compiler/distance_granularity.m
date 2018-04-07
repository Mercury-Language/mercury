%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: distance_granularity.m.
% Author: tannier.
%
% This module contains a program transformation that adds a mechanism that
% controls the granularity of parallel execution using the distance metric.
% For more information, see the paper by K. Shen, V. Santos Costa, and A. King:
% Distance: a New Metric for Controlling Granularity for Parallel Execution.
% In Proceedings of the Joint International Conference and Symposium on Logic
% Programming, MIT Press, 1998.
%
% NOTE: The module introduce_parallelism.m implements another transformation
% with the same objective.
%
% To see how the distance granularity transformation works, consider
% this parallel version of the double recursive fibonacci predicate:
%
% :- pred fibonacci(int::in, int::out) is det.
%
% fibonacci(X, Y) :-
%     ( if X = 0 then
%         Y = 0
%     else
%         ( if X = 1 then
%             Y = 1
%         else
%             ( if X > 1 then
%                 J = X - 1,
%                 K = X - 2,
%                 (
%                     fibonacci(J, Jout)
%                 &
%                     fibonacci(K, Kout)
%                 ),
%                 Y = Jout + Kout
%             else
%                 error("fibonacci: wrong value")
%             )
%         )
%     ).
%
% Assuming that the distance metric specified during compilation is 10,
% this module creates this specialized version of the above predicate:
%
% :- pred DistanceGranularityFor__pred__fibonacci__10(int::in, int::out,
%     int::in) is det.
%
% DistanceGranularityFor__pred__fibonacci__10(X, Y, Distance) :-
%     ( if X = 0 then
%         Y = 0
%     else
%         ( if X = 1 then
%             Y = 1
%         else
%             ( if X > 1 then
%                 J = X - 1,
%                 K = X - 2,
%                 ( if Distance = 0 then
%                     (
%                         DistanceGranularityFor__pred__fibonacci__10(J, Jout,
%                             10)
%                     &
%                         DistanceGranularityFor__pred__fibonacci__10(K, Kout,
%                             10)
%                     )
%                 else
%                     DistanceGranularityFor__pred__fibonacci__10(J, Jout,
%                         Distance - 1),
%                     DistanceGranularityFor__pred__fibonacci__10(K, Kout,
%                         Distance - 1)
%                 ),
%                 Y = Jout + Kout
%             else
%                 error("fibonacci: wrong value")
%             )
%         )
%     ).
%
% After which, the original version becomes:
%
% :- pred fibonacci(int::in, int::out) is det.
%
% fibonacci(X, Y) :-
%     ( if X = 0 then
%         Y = 0
%     else
%         ( if X = 1 then
%             Y = 1
%         else
%             ( if X > 1 then
%                 J = X - 1,
%                 K = X - 2,
%                 (
%                     DistanceGranularityFor__pred__fibonacci__10(J, Jout, 10)
%                 &
%                     DistanceGranularityFor__pred__fibonacci__10(K, Kout, 10)
%                 ),
%                 Y = Jout + Kout
%             else
%                 error("fibonacci: wrong value")
%             )
%         )
%     ).
%
% The second part of the transformation makes the granularity control
% transparent to the original procedure's callers by replacing the recursive
% calls in the body of the original procedure with calls to the specialized
% version. The original procedure's callers should never need to call the
% specialized version directly.
%
% XXX For the time being, we assume that the int module was imported in the
% source code of the program for which we apply the distance granularity
% transformation.
%
% XXX To me (zs), the above example code looks wrong.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.distance_granularity.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

%-----------------------------------------------------------------------------%

    % control_distance_granularity(!ModuleInfo, Distance)
    %
    % Control the granularity of parallelism of a module using the distance
    % metric.
    %
:- pred control_distance_granularity(module_info::in, module_info::out,
    int::in) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%
%
% This section contains predicates which apply the first part of the
% transformation, i.e. creating the specialized version of the
% original predicate.
%

control_distance_granularity(!ModuleInfo, Distance) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    apply_dg_to_preds(PredIds, Distance, !ModuleInfo).

    % Apply the distance granularity transformation to each predicate in the
    % list.
    %
:- pred apply_dg_to_preds(list(pred_id)::in, int::in,
    module_info::in, module_info::out) is det.

apply_dg_to_preds([], _Distance, !ModuleInfo).
apply_dg_to_preds([PredId | PredIdList], Distance, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    % We need to know what the pred_id will be for the specified predicate
    % before we actually clone it (this avoids doing one more pass to update
    % the pred_id in the recursive plain calls).
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable),
    get_next_pred_id(PredicateTable, NewPredId),

    % Create the new sym_name for the recursive plain calls.
    ModuleName = pred_info_module(PredInfo),
    Prefix = granularity_prefix,
    MaybePredOrFunc = yes(pf_predicate),
    NewPredIdGranularity = newpred_distance_granularity(Distance),
    PredName0 = pred_info_name(PredInfo),
    make_pred_name(ModuleName, Prefix, MaybePredOrFunc, PredName0,
        NewPredIdGranularity, NewCallSymName),

    ProcIds = pred_info_non_imported_procids(PredInfo),
    apply_dg_to_procs(PredId, ProcIds, Distance, NewPredId, NewCallSymName,
        PredInfo, PredInfoClone0, no, Specialized, !ModuleInfo),
    (
        Specialized = yes,
        % The predicate has been specialized as it contains recursive calls.

        % Create the name of the specialized predicate out of the name of the
        % original one.
        create_specialized_pred_name(Prefix, Distance, PredName0, PredName),
        pred_info_set_name(PredName, PredInfoClone0, PredInfoClone1),

        % If the original predicate was a function then the specialized version
        % is a predicate.
        pred_info_set_is_pred_or_func(pf_predicate, PredInfoClone1,
            PredInfoClone2),

        % The arity and the argument types of the specialized predicate must be
        % modified.
        Arity = pred_info_orig_arity(PredInfoClone2),
        pred_info_set_orig_arity(Arity + 1, PredInfoClone2, PredInfoClone3),
        pred_info_get_arg_types(PredInfoClone3, ArgTypes0),
        list.append(ArgTypes0, [int_type], ArgTypes),
        pred_info_get_typevarset(PredInfoClone3, Tvarset),
        pred_info_get_exist_quant_tvars(PredInfoClone3, ExistqTvars),
        pred_info_set_arg_types(Tvarset, ExistqTvars, ArgTypes,
            PredInfoClone3, PredInfoClone),

        % Add the specialized predicate to the predicate table.
        module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
        predicate_table_insert(PredInfoClone, _, PredicateTable0,
            PredicateTable1),
        module_info_set_predicate_table(PredicateTable1, !ModuleInfo),

        update_original_predicate_procs(PredId, ProcIds, Distance, NewPredId,
            NewCallSymName, PredInfo, PredInfoUpdated, !ModuleInfo),
        module_info_set_pred_info(PredId, PredInfoUpdated, !ModuleInfo)
    ;
        Specialized = no
        % The predicate has not been specialized.
    ),
    apply_dg_to_preds(PredIdList, Distance, !ModuleInfo).

    % Apply the distance granularity transformation to each procedure in the
    % list.
    % PredIdSpecialized is the pred_id of the predicate to be specialized.
    % SymNameSpecialized is the sym_name of the predicate to be specialized.
    %
:- pred apply_dg_to_procs(pred_id::in, list(proc_id)::in, int::in,
    pred_id::in, sym_name::in, pred_info::in, pred_info::out,
    bool::in, bool::out, module_info::in, module_info::out) is det.

apply_dg_to_procs(_PredId, [], _Distance, _PredIdSpecialized,
        _SymNameSpecialized, !PredInfo, !Specialized, !ModuleInfo).
apply_dg_to_procs(PredId, [ProcId | ProcIds], Distance, PredIdSpecialized,
        SymNameSpecialized, !PredInfo, !Specialized, !ModuleInfo) :-
    module_info_proc_info(!.ModuleInfo, proc(PredId, ProcId), ProcInfo0),
    proc_info_get_has_parallel_conj(ProcInfo0, HasParallelConj),
    (
        HasParallelConj = has_parallel_conj,
        % The procedure contains parallel conjunction(s).

        proc_info_get_goal(ProcInfo0, Body),
        apply_dg_to_goal(Body, BodyClone, PredId, ProcId, PredIdSpecialized,
            SymNameSpecialized, ProcInfo0, ProcInfo1, !ModuleInfo,
            Distance, no, no, MaybeGranularityVar, _),
        (
            MaybeGranularityVar = yes(_),
            % The granularity variable has been created while the procedure was
            % processed. That means that the predicate must be specialized.
            !:Specialized = yes,
            proc_info_set_goal(BodyClone, ProcInfo1, ProcInfo2),
            requantify_proc_general(ordinary_nonlocals_no_lambda,
                ProcInfo2, ProcInfo3),
            recompute_instmap_delta_proc(
                do_not_recompute_atomic_instmap_deltas, ProcInfo3, ProcInfo,
                !ModuleInfo),
            pred_info_set_proc_info(ProcId, ProcInfo, !PredInfo)
        ;
            MaybeGranularityVar = no
        )
    ;
        HasParallelConj = has_no_parallel_conj
        % No need to apply the distance granularity transformation to this
        % procedure as it does not contain any parallel conjunctions.
    ),
    apply_dg_to_procs(PredId, ProcIds, Distance, PredIdSpecialized,
        SymNameSpecialized, !PredInfo, !Specialized, !ModuleInfo).

    % Apply the distance granularity transformation to a goal.
    % CallerPredId and CallerProcId are those of the original predicate.
    %
:- pred apply_dg_to_goal(hlds_goal::in, hlds_goal::out, pred_id::in,
    proc_id::in, pred_id::in, sym_name::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out, int::in, bool::in,
    maybe(prog_var)::in, maybe(prog_var)::out, bool::out) is det.

apply_dg_to_goal(!Goal, CallerPredId, CallerProcId, PredIdSpecialized,
        SymNameSpecialized, !ProcInfo, !ModuleInfo, Distance, IsInParallelConj,
        !MaybeGranularityVar, IsRecursiveCallInParallelConj) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = unify(_, _, _, _, _),
        IsRecursiveCallInParallelConj = no
    ;
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        apply_dg_to_plain_call(GoalExpr0, GoalExpr, CallerPredId,
            PredIdSpecialized, SymNameSpecialized, CallerProcId, !ProcInfo,
            !ModuleInfo, IsInParallelConj, !MaybeGranularityVar,
            IsRecursiveCallInParallelConj),
        !:Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        IsRecursiveCallInParallelConj = no
    ;
        GoalExpr0 = generic_call(_, _, _, _, _),
        IsRecursiveCallInParallelConj = no
    ;
        GoalExpr0 = conj(Type, Goals0),
        apply_dg_to_conj(Goals0, [], Goals, CallerPredId, CallerProcId,
            PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
            Distance, yes, !MaybeGranularityVar, no, ContainRecursiveCalls),
        (
            Type = plain_conj,
            GoalExpr = conj(plain_conj, Goals),
            !:Goal = hlds_goal(GoalExpr, GoalInfo)
        ;
            Type = parallel_conj,
            (
                ContainRecursiveCalls = yes,
                create_if_then_else_goal(Goals, GoalInfo,
                    !.MaybeGranularityVar, PredIdSpecialized, CallerProcId,
                    Distance, !:Goal, !ProcInfo, !.ModuleInfo)
            ;
                ContainRecursiveCalls = no
            )
        ),
        IsRecursiveCallInParallelConj = no
    ;
        GoalExpr0 = disj(Goals0),
        apply_dg_to_disj(Goals0, [], Goals, CallerPredId, CallerProcId,
            PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
            Distance, !MaybeGranularityVar),
        GoalExpr = disj(Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo),
        IsRecursiveCallInParallelConj = no
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        apply_dg_to_switch(Cases0, [], Cases, CallerPredId, CallerProcId,
            PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
            Distance, !MaybeGranularityVar),
        GoalExpr = switch(Var, CanFail, Cases),
        !:Goal = hlds_goal(GoalExpr, GoalInfo),
        IsRecursiveCallInParallelConj = no
    ;
        GoalExpr0 = negation(SubGoal0),
        apply_dg_to_goal(SubGoal0, SubGoal, CallerPredId, CallerProcId,
            PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
            Distance, IsInParallelConj, !MaybeGranularityVar,
            IsRecursiveCallInParallelConj),
        GoalExpr = negation(SubGoal),
        !:Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            % Return !.Goal as !:Goal.
            IsRecursiveCallInParallelConj = no
        else
            apply_dg_to_goal(SubGoal0, SubGoal, CallerPredId, CallerProcId,
                PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
                Distance, IsInParallelConj, !MaybeGranularityVar,
                IsRecursiveCallInParallelConj),
            GoalExpr = scope(Reason, SubGoal),
            !:Goal = hlds_goal(GoalExpr, GoalInfo)
        )
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        apply_dg_to_goal(Cond0, Cond, CallerPredId, CallerProcId,
            PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
            Distance, no, !MaybeGranularityVar, _),
        apply_dg_to_goal(Then0, Then, CallerPredId, CallerProcId,
            PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
            Distance, no, !MaybeGranularityVar, _),
        apply_dg_to_goal(Else0, Else, CallerPredId, CallerProcId,
            PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
            Distance, no, !MaybeGranularityVar, _),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        !:Goal = hlds_goal(GoalExpr, GoalInfo),
        IsRecursiveCallInParallelConj = no
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

    % Apply the distance granularity transformation to a plain call.
    %
:- pred apply_dg_to_plain_call(
    hlds_goal_expr::in(goal_expr_plain_call), hlds_goal_expr::out,
    pred_id::in, pred_id::in, sym_name::in, proc_id::in, proc_info::in,
    proc_info::out, module_info::in, module_info::out, bool::in,
    maybe(prog_var)::in, maybe(prog_var)::out, bool::out) is det.

apply_dg_to_plain_call(!GoalExpr, CallerPredId, PredIdSpecialized,
        SymNameSpecialized, CallerProcId, !ProcInfo, !ModuleInfo,
        IsInParallelConj, !MaybeGranularityVar,
        IsRecursiveCallInParallelConj) :-
    !.GoalExpr = plain_call(CalleePredId, CalleeProcId, CallArgs, CallBuiltin,
        CallUnifyContext, _),
    ( if
        IsInParallelConj = yes,
        CalleePredId = CallerPredId,
        CalleeProcId = CallerProcId
    then
        % That is a recursive plain call in a parallel conjunction.
        (
            !.MaybeGranularityVar = yes(_GranularityVar)
            % The variable Granularity has already been added to ProcInfo.
        ;
            !.MaybeGranularityVar = no,
            % Add the variable Granularity to ProcInfo.
            proc_info_create_var_from_type(int_type, no, GranularityVar,
                !ProcInfo),
            !:MaybeGranularityVar = yes(GranularityVar),

            % XXX Check if the int module is imported (that is why
            % ModuleInfo can be modified).

            % Add the granularity variable to the head variables of the
            % procedure and update the argmodes.
            proc_info_get_argmodes(!.ProcInfo, ArgsModes0),
            proc_info_get_headvars(!.ProcInfo, HeadVars0),
            list.append(ArgsModes0, [in_mode], ArgsModes),
            list.append(HeadVars0, [GranularityVar], HeadVars),
            proc_info_set_argmodes(ArgsModes, !ProcInfo),
            proc_info_set_headvars(HeadVars, !ProcInfo)
        ),

        % Change the pred_id and the sym_name. We will deal with the
        % arguments later as they are not identical for the then and the
        % else part of the if_then_else goal introduced by the
        % transformation.
        !:GoalExpr = plain_call(PredIdSpecialized, CallerProcId,
            CallArgs, CallBuiltin, CallUnifyContext, SymNameSpecialized),
        IsRecursiveCallInParallelConj = yes
    else
        IsRecursiveCallInParallelConj = no
    ).

    % Apply the distance granularity transformation to a conjunction.
    %
:- pred apply_dg_to_conj(hlds_goals::in, hlds_goals::in, hlds_goals::out,
    pred_id::in, proc_id::in, pred_id::in, sym_name::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out, int::in,
    bool::in, maybe(prog_var)::in, maybe(prog_var)::out,
    bool::in, bool::out) is det.

apply_dg_to_conj([], !GoalsAcc, _CallerPredId, _CallerProcId,
        _PredIdSpecialized, _SymNameSpecialized, !ProcInfo, !ModuleInfo,
        _Distance, _IsInParallelConj,
        !MaybeGranularityVar, !HasRecursiveCallsInParallelConj).
apply_dg_to_conj([Goal0 | Goals], !GoalsAcc, CallerPredId, CallerProcId,
        PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
        Distance, IsInParallelConj, !MaybeGranularityVar,
        !HasRecursiveCallsInParallelConj) :-
    apply_dg_to_goal(Goal0, Goal, CallerPredId, CallerProcId,
        PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
        Distance, IsInParallelConj, !MaybeGranularityVar, IsRecursiveCall),
    list.append(!.GoalsAcc, [Goal], !:GoalsAcc),
    (
        IsRecursiveCall = yes,
        % The goal we just processed is a recursive call in a parallel
        % conjunction. Therefore, the conjunction contains recursive calls.
        !:HasRecursiveCallsInParallelConj = yes
    ;
        IsRecursiveCall = no,
        !:HasRecursiveCallsInParallelConj = !.HasRecursiveCallsInParallelConj
    ),
    apply_dg_to_conj(Goals, !GoalsAcc, CallerPredId, CallerProcId,
        PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
        Distance, IsInParallelConj, !MaybeGranularityVar,
        !HasRecursiveCallsInParallelConj).

    % Create the if_then_else goal surrounding the recursive plain call as
    % shown in the example.
    %
:- pred create_if_then_else_goal(hlds_goals::in, hlds_goal_info::in,
    maybe(prog_var)::in, pred_id::in, proc_id::in, int::in, hlds_goal::out,
    proc_info::in, proc_info::out, module_info::in) is det.

create_if_then_else_goal(GoalsInConj, ConjInfo, MaybeGranularityVar,
        PredIdSpecialized, CallerProcId, Distance, IfThenElseGoal, !ProcInfo,
        ModuleInfo) :-
    proc_info_create_var_from_type(int_type, no, Var, !ProcInfo),
    make_int_const_construction(term.context_init, Var, 0, UnifyGoal),
    (
        MaybeGranularityVar = yes(GranularityVar),
        % Create the condition.
        make_simple_test(GranularityVar, Var,
            umc_implicit("distance_granularity"), [], Test),
        create_conj(UnifyGoal, Test, plain_conj, Cond),

        % Create the then.
        Then0 = hlds_goal(conj(parallel_conj, GoalsInConj), ConjInfo),
        apply_dg_to_then(Then0, Then, GranularityVar, PredIdSpecialized,
            CallerProcId, Distance, !ProcInfo),

        % Create the else.
        Else0 = hlds_goal(conj(plain_conj, GoalsInConj), ConjInfo),
        apply_dg_to_else(Else0, Else, GranularityVar, PredIdSpecialized,
            CallerProcId, ModuleInfo, !ProcInfo),

        % The non-locals of the hlds_goal_info of the if_then_else goal must
        % contain the variable controlling the granularity.
        NonLocals0 = goal_info_get_nonlocals(ConjInfo),
        set_of_var.insert(GranularityVar, NonLocals0, NonLocals),
        goal_info_set_nonlocals(NonLocals, ConjInfo, IfThenElseInfo),
        IfThenElseGoal = hlds_goal(if_then_else([], Cond, Then, Else),
            IfThenElseInfo)
    ;
        MaybeGranularityVar = no,
        % The conjunction contains recursive calls so the
        % granularity variable must have been created.
        unexpected($pred, "MaybeGranularityVar = no")
    ).

    % Update the then part of the new if_then_else goal introduced by the
    % transformation as shown in the example. It creates a variable Granularity
    % containing the value Distance and uses it as the last argument of the
    % calls of the recursive procedure.
    %
:- pred apply_dg_to_then(hlds_goal::in, hlds_goal::out, prog_var::in,
    pred_id::in, proc_id::in, int::in, proc_info::in, proc_info::out) is det.

apply_dg_to_then(!Goal, GranularityVar, CallerPredId, CallerProcId, Distance,
        !ProcInfo) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo),
    apply_dg_to_then2(GoalExpr0, GoalExpr, 1, _, GranularityVar, CallerPredId,
        CallerProcId, Distance, !ProcInfo),
    Goal0 = hlds_goal(GoalExpr, GoalInfo),
    recompute_conj_info(Goal0, !:Goal).

:- pred apply_dg_to_then2(hlds_goal_expr::in, hlds_goal_expr::out,
    int::in, int::out, prog_var::in, pred_id::in, proc_id::in, int::in,
    proc_info::in, proc_info::out) is det.

apply_dg_to_then2(!GoalExpr, !IndexInConj, GranularityVar, CallerPredId,
        CallerProcId, Distance, !ProcInfo) :-
    ( if !.GoalExpr = conj(parallel_conj, Goals0) then
        list.length(Goals0, Length),
        ( if !.IndexInConj > Length then
            true
        else
            list.det_index1(Goals0, !.IndexInConj, Goal0),
            Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
            ( if
                GoalExpr0 = plain_call(CalleePredId, CalleeProcId, CallArgs0,
                    CallBuiltin, _, CallSymName)
            then
                ( if
                    CalleePredId = CallerPredId,
                    CalleeProcId = CallerProcId
                then
                    % That is a recursive plain call.

                    % Create granularity variable containing value Distance.
                    proc_info_create_var_from_type(int_type, no,
                        Var, !ProcInfo),
                    make_int_const_construction(term.context_init,
                        Var, Distance, UnifyGoal),

                    % Use that variable as the last argument of the call.
                    list.append(CallArgs0, [Var], CallArgs),

                    % If the original predicate is a function then the
                    % specialized version is a predicate. Therefore,
                    % there is no need for the unify context anymore.
                    CallUnifyContext = no,

                    GoalExpr = plain_call(CalleePredId, CalleeProcId, CallArgs,
                        CallBuiltin, CallUnifyContext, CallSymName),

                    % Var has instmap bound(Distance).
                    InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
                    MerInst = bound(shared, inst_test_results_fgtc,
                        [bound_functor(int_const(Distance), [])]),
                    instmap_delta_insert_var(Var, MerInst,
                        InstMapDelta0, InstMapDelta),
                    goal_info_set_instmap_delta(InstMapDelta, GoalInfo0,
                        GoalInfo),

                    Goal = hlds_goal(GoalExpr, GoalInfo),

                    create_conj(UnifyGoal, Goal, plain_conj, PlainConj),

                    % Replace the call by the newly created conjunction.
                    list.det_replace_nth(Goals0, !.IndexInConj, PlainConj,
                        Goals),
                    !:GoalExpr = conj(parallel_conj, Goals)
                else
                    % Not a recursive call.
                    true
                ),
                !:IndexInConj = !.IndexInConj + 1
            else
                !:IndexInConj = !.IndexInConj + 1
            ),
            apply_dg_to_then2(!GoalExpr, !IndexInConj, GranularityVar,
                CallerPredId, CallerProcId, Distance, !ProcInfo)
        )
    else
        % Not a parallel conjunction.
        unexpected($pred, "unexpected goal type")
    ).

    % Recompute the hlds_goal_info of a conjunction.
    %
:- pred recompute_conj_info(hlds_goal::in, hlds_goal::out) is det.

recompute_conj_info(!Conj) :-
    ( if !.Conj = hlds_goal(conj(Type, Goals), ConjInfo0) then
        goal_list_nonlocals(Goals, NonLocals),
        goal_list_instmap_delta(Goals, InstMapDelta),
        goal_list_determinism(Goals, Detism),
        goal_list_purity(Goals, Purity),
        goal_info_set_nonlocals(NonLocals, ConjInfo0, ConjInfo1),
        goal_info_set_instmap_delta(InstMapDelta, ConjInfo1, ConjInfo2),
        goal_info_set_determinism(Detism, ConjInfo2, ConjInfo3),
        goal_info_set_purity(Purity, ConjInfo3, ConjInfo),
        !:Conj = hlds_goal(conj(Type, Goals), ConjInfo)
    else
        % Not a conjunction.
        unexpected($pred, "unexpected goal type")
    ).

    % Update the else part of the new if_then_else goal introduced by the
    % transformation as shown in the example. It decrements the value of
    % GranularityVar and uses it as the last argument of the calls of the
    % recursive procedure.
    %
:- pred apply_dg_to_else(hlds_goal::in, hlds_goal::out, prog_var::in,
    pred_id::in, proc_id::in, module_info::in,
    proc_info::in, proc_info::out) is det.

apply_dg_to_else(!Goal, GranularityVar, CallerPredId, CallerProcId,
        ModuleInfo, !ProcInfo) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo),
    apply_dg_to_else2(GoalExpr0, GoalExpr, 1, _, GranularityVar, CallerPredId,
        CallerProcId, ModuleInfo, !ProcInfo),
    Goal0 = hlds_goal(GoalExpr, GoalInfo),
    recompute_conj_info(Goal0, !:Goal).

:- pred apply_dg_to_else2(hlds_goal_expr::in, hlds_goal_expr::out,
    int::in, int::out, prog_var::in, pred_id::in, proc_id::in,
    module_info::in, proc_info::in, proc_info::out) is det.

apply_dg_to_else2(!GoalExpr, !IndexInConj, GranularityVar, CallerPredId,
        CallerProcId, ModuleInfo, !ProcInfo) :-
    ( if !.GoalExpr = conj(plain_conj, Goals0) then
        list.length(Goals0, Length),
        ( if !.IndexInConj > Length then
            true
        else
            list.det_index1(Goals0, !.IndexInConj, Goal0),
            Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
            ( if
                GoalExpr0 = plain_call(CalleePredId, CalleeProcId, CallArgs0,
                    CallBuiltin, _, CallSymName)
            then
                ( if
                    CalleePredId = CallerPredId,
                    CalleeProcId = CallerProcId
                then
                    % That is a recursive plain call.

                    % Create an int variable containing the value 1.
                    proc_info_create_var_from_type(int_type, no,
                        Var, !ProcInfo),
                    make_int_const_construction(term.context_init,
                        Var, 1, UnifyGoal),

                    % Create a variable which will contain the decremented
                    % granularity distance.
                    proc_info_create_var_from_type(int_type, no,
                        VarResult, !ProcInfo),

                    % Decrement GranularityVar before the call.
                    lookup_builtin_pred_proc_id(ModuleInfo,
                        unqualified("int"), "minus", pf_function, 2, only_mode,
                        MinusPredId, MinusProcId),
                    MinusCallArgs = [GranularityVar, Var, VarResult],
                    MinusCallBuiltin = inline_builtin,
                    MinusCallSymName = qualified(unqualified("int"), "-"),
                    ConsId =
                        cons(MinusCallSymName, 2, cons_id_dummy_type_ctor),
                    Rhs = rhs_functor(ConsId, is_not_exist_constr,
                        [GranularityVar, Var]),
                    MinusCallUnifyContext = yes(call_unify_context(VarResult,
                        Rhs, unify_context(
                        umc_implicit("distance_granularity"), []))),
                    DecrementGoalExpr = plain_call(MinusPredId, MinusProcId,
                        MinusCallArgs, MinusCallBuiltin, MinusCallUnifyContext,
                        MinusCallSymName),
                    set_of_var.list_to_set([GranularityVar, Var, VarResult],
                        NonLocals),
                    VarResultDelta =
                        VarResult - ground(unique, none_or_default_func),
                    VarDelta = Var - bound(shared, inst_test_results_fgtc,
                        [bound_functor(int_const(1), [])]),
                    InstMapDeltaDecrement = instmap_delta_from_assoc_list(
                        [VarDelta, VarResultDelta]),
                    Detism = detism_det,
                    Purity = purity_pure,
                    % Take the context of the first goal of the conjunction.
                    list.det_index1(Goals0, 1, FirstGoal),
                    FirstGoal = hlds_goal(_, FirstGoalInfo),
                    Context = goal_info_get_context(FirstGoalInfo),
                    goal_info_init(NonLocals, InstMapDeltaDecrement, Detism,
                        Purity, Context, DecrementGoalInfo),
                    DecrementGoal = hlds_goal(DecrementGoalExpr,
                        DecrementGoalInfo),

                    % Use the decremented value of GranularityVar as the
                    % last argument of the call.
                    list.append(CallArgs0, [VarResult], CallArgs),

                    % If the original predicate is a function then the
                    % specialized version is a predicate. Therefore, there is
                    % no need for the unify context anymore.
                    CallUnifyContext = no,

                    GoalExpr = plain_call(CalleePredId, CalleeProcId, CallArgs,
                        CallBuiltin, CallUnifyContext, CallSymName),
                    InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
                    MerInst = ground(shared, none_or_default_func),
                    instmap_delta_insert_var(Var, MerInst,
                        InstMapDelta0, InstMapDelta),
                    goal_info_set_instmap_delta(InstMapDelta, GoalInfo0,
                        GoalInfo),
                    Goal = hlds_goal(GoalExpr, GoalInfo),
                    list.det_replace_nth(Goals0, !.IndexInConj, Goal, Goals1),

                    % Append the goals in the right order.
                    list.det_split_list(!.IndexInConj - 1, Goals1, StartGoals,
                        EndGoals),
                    list.append(StartGoals, [UnifyGoal], GoalsAppend0),
                    list.append(GoalsAppend0, [DecrementGoal],
                        GoalsAppend1),
                    list.append(GoalsAppend1, EndGoals, Goals),
                    !:GoalExpr = conj(plain_conj, Goals)
                else
                    % Not a recursive call.
                    true
                ),
                !:IndexInConj = !.IndexInConj + 3
            else
                !:IndexInConj = !.IndexInConj + 1
            ),
            apply_dg_to_else2(!GoalExpr, !IndexInConj, GranularityVar,
                CallerPredId, CallerProcId, ModuleInfo, !ProcInfo)
        )
    else
        unexpected($pred, "unexpected goal type")
    ).

    % Apply the distance granularity transformation to a disjunction.
    %
:- pred apply_dg_to_disj(list(hlds_goal)::in,
    list(hlds_goal)::in, list(hlds_goal)::out, pred_id::in, proc_id::in,
    pred_id::in, sym_name::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out, int::in,
    maybe(prog_var)::in, maybe(prog_var)::out) is det.

apply_dg_to_disj([], !GoalsAcc, _CallerPredId, _CallerProcId,
        _PredIdSpecialized, _SymNameSpecialized, !ProcInfo, !ModuleInfo,
        _Distance, !MaybeGranularityVar).
apply_dg_to_disj([Goal0 | Goals], !GoalsAcc, CallerPredId, CallerProcId,
        PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
        Distance, !MaybeGranularityVar) :-
    apply_dg_to_goal(Goal0, Goal, CallerPredId, CallerProcId,
        PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
        Distance, no, !MaybeGranularityVar, _),
    list.append( !.GoalsAcc, [Goal], !:GoalsAcc),
    apply_dg_to_disj(Goals, !GoalsAcc, CallerPredId, CallerProcId,
        PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
        Distance, !MaybeGranularityVar).

    % Apply the distance granularity transformation to a switch.
    %
:- pred apply_dg_to_switch(
    list(case)::in, list(case)::in, list(case)::out, pred_id::in,
    proc_id::in, pred_id::in, sym_name::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out, int::in,
    maybe(prog_var)::in, maybe(prog_var)::out) is det.

apply_dg_to_switch([], !CasesAcc, _CallerPredId, _CallerProcId,
        _PredIdSpecialized, _SymNameSpecialized, !ProcInfo, !ModuleInfo,
        _Distance, !MaybeGranularityVar).
apply_dg_to_switch([Case | Cases], !CasesAcc, CallerPredId, CallerProcId,
        PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
        Distance, !MaybeGranularityVar) :-
    Case = case(MainConsId, OtherConsIds, Goal0),
    apply_dg_to_goal(Goal0, Goal, CallerPredId, CallerProcId,
        PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
        Distance, no, !MaybeGranularityVar, _),
    !:CasesAcc = [case(MainConsId, OtherConsIds, Goal) | !.CasesAcc],
    apply_dg_to_switch(Cases, !CasesAcc, CallerPredId, CallerProcId,
        PredIdSpecialized, SymNameSpecialized, !ProcInfo, !ModuleInfo,
        Distance, !MaybeGranularityVar).

    % Create the string name of the specialized predicate (same format as
    % make_pred_name in prog_util) out of the name of the original one.
    %
:- pred create_specialized_pred_name(string::in, int::in,
    string::in, string::out) is det.

create_specialized_pred_name(Prefix, Distance, !PredName) :-
    PFS = "pred",
    int_to_string(Distance, PredIdStr),
    string.format("%s__%s__%s__%s",
        [s(Prefix), s(PFS), s(!.PredName), s(PredIdStr)], !:PredName).

:-func granularity_prefix = string.

granularity_prefix = "DistanceGranularityFor".

%-----------------------------------------------------------------------------%
%
% This section contains predicates that make the granularity control
% transparent to the original procedure's callers by replacing the recursive
% calls in the body of the original procedure with calls to the specialized
% version.
%

    % Update the recursive calls in each procedure in the list so that the
    % pred_id called is the one of the specialized procedure.
    %
:- pred update_original_predicate_procs(pred_id::in, list(proc_id)::in,
    int::in, pred_id::in, sym_name::in, pred_info::in, pred_info::out,
    module_info::in, module_info::out) is det.

update_original_predicate_procs(_PredId, [], _Distance, _PredIdSpecialized,
        _SymNameSpecialized, !PredInfo, !ModuleInfo).
update_original_predicate_procs(PredId, [ProcId | ProcIds], Distance,
        PredIdSpecialized, SymNameSpecialized, !PredInfo,
        !ModuleInfo) :-
    module_info_proc_info(!.ModuleInfo, proc(PredId, ProcId), ProcInfo0),
    proc_info_get_goal(ProcInfo0, Body0),
    update_original_predicate_goal(Body0, Body, PredId, ProcId,
        PredIdSpecialized, SymNameSpecialized, ProcInfo0, ProcInfo1, Distance),
    proc_info_set_goal(Body, ProcInfo1, ProcInfo2),
    requantify_proc_general(ordinary_nonlocals_no_lambda,
        ProcInfo2, ProcInfo3),
    recompute_instmap_delta_proc(do_not_recompute_atomic_instmap_deltas,
        ProcInfo3, ProcInfo, !ModuleInfo),
    pred_info_set_proc_info(ProcId, ProcInfo, !PredInfo),
    update_original_predicate_procs(PredId, ProcIds, Distance,
        PredIdSpecialized, SymNameSpecialized, !PredInfo, !ModuleInfo).

    % Update the recursive calls of a goal so that the pred_id called
    % is the one of the specialized procedure.
    %
:- pred update_original_predicate_goal(hlds_goal::in, hlds_goal::out,
    pred_id::in, proc_id::in, pred_id::in, sym_name::in,
    proc_info::in, proc_info::out, int::in) is det.

update_original_predicate_goal(!Goal, CallerPredId, CallerProcId,
        PredIdSpecialized, SymNameSpecialized, !ProcInfo, Distance) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = unify(_, _, _, _, _)
    ;
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        % XXX Due to the absence of alias tracking, passing !.Goal instead of
        % !.Goal would result in a mode error.
        !:Goal = hlds_goal(GoalExpr0, GoalInfo),
        update_original_predicate_plain_call(!Goal, CallerPredId, CallerProcId,
            PredIdSpecialized, SymNameSpecialized, !ProcInfo, Distance)
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        GoalExpr0 = generic_call(_, _, _, _, _)
    ;
        GoalExpr0 = conj(Type, Goals0),
        update_original_predicate_goals(Goals0, [], Goals1, CallerPredId,
            CallerProcId, PredIdSpecialized, SymNameSpecialized, !ProcInfo,
            Distance),
        (
            Type = plain_conj,
            flatten_conj(Goals1, Goals)
        ;
            Type = parallel_conj,
            % No need to flatten parallel conjunctions as the transformation
            % may only create plain conjunctions
            % (see update_original_predicate_plain_call).
            Goals = Goals1
        ),
        GoalExpr = conj(Type, Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = disj(Goals0),
        update_original_predicate_goals(Goals0, [], Goals, CallerPredId,
            CallerProcId, PredIdSpecialized, SymNameSpecialized, !ProcInfo,
            Distance),
        GoalExpr = disj(Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        update_original_predicate_switch(Cases0, [], Cases, CallerPredId,
            CallerProcId, PredIdSpecialized, SymNameSpecialized, !ProcInfo,
            Distance),
        GoalExpr = switch(Var, CanFail, Cases),
        !:Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = negation(SubGoal0),
        update_original_predicate_goal(SubGoal0, SubGoal,
            CallerPredId, CallerProcId, PredIdSpecialized, SymNameSpecialized,
            !ProcInfo, Distance),
        GoalExpr = negation(SubGoal),
        !:Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            % Leave !Goal as it is.
            true
        else
            update_original_predicate_goal(SubGoal0, SubGoal,
                CallerPredId, CallerProcId,
                PredIdSpecialized, SymNameSpecialized, !ProcInfo, Distance),
            GoalExpr = scope(Reason, SubGoal),
            !:Goal = hlds_goal(GoalExpr, GoalInfo)
        )
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        update_original_predicate_goal(Cond0, Cond, CallerPredId, CallerProcId,
            PredIdSpecialized, SymNameSpecialized, !ProcInfo, Distance),
        update_original_predicate_goal(Then0, Then, CallerPredId, CallerProcId,
            PredIdSpecialized, SymNameSpecialized, !ProcInfo, Distance),
        update_original_predicate_goal(Else0, Else, CallerPredId, CallerProcId,
            PredIdSpecialized, SymNameSpecialized, !ProcInfo, Distance),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        !:Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = shorthand(_),
        % Shorthand are not supposed to occur here.
        unexpected($pred, "shorthand")
    ).

    % Update the plain call so that the pred_id called is the one of the
    % specialized procedure.
    %
:- pred update_original_predicate_plain_call(
    hlds_goal::in(goal_plain_call), hlds_goal::out,
    pred_id::in, proc_id::in, pred_id::in, sym_name::in,
    proc_info::in, proc_info::out, int::in) is det.

update_original_predicate_plain_call(!Goal, CallerPredId, CallerProcId,
        PredIdSpecialized, SymNameSpecialized, !ProcInfo, Distance) :-
    !.Goal = hlds_goal(CallExpr0, CallInfo0),
    CallExpr0 = plain_call(CalleePredId, CalleeProcId, CallArgs0,
        CallBuiltin, _, _),
    ( if
        CalleePredId = CallerPredId,
        CalleeProcId = CallerProcId
    then
        % That is a recursive plain call.

        % Create the int variable which will be used as the last argument of
        % the call.
        proc_info_create_var_from_type(int_type, no, Var, !ProcInfo),
        make_int_const_construction(term.context_init,
            Var, Distance, UnifyGoal),
        list.append(CallArgs0, [Var], CallArgs),

        % If the original predicate is a function then the specialized
        % version is a predicate. Therefore, there is no need for the unify
        % context anymore.
        CallUnifyContext = no,

        % Update the pred_id to the pred_id of the specialized pred.
        CallExpr = plain_call(PredIdSpecialized, CalleeProcId, CallArgs,
            CallBuiltin, CallUnifyContext, SymNameSpecialized),

        % Update the nonlocals and the instmap_delta of the hlds_goal_info
        % of the recursive plain call for Var.
        NonLocals0 = goal_info_get_nonlocals(CallInfo0),
        set_of_var.insert(Var, NonLocals0, NonLocals),
        goal_info_set_nonlocals(NonLocals, CallInfo0, CallInfo1),
        InstMapDelta0 = goal_info_get_instmap_delta(CallInfo1),
        MerInst = ground(shared, none_or_default_func),
        instmap_delta_insert_var(Var, MerInst, InstMapDelta0, InstMapDelta),
        goal_info_set_instmap_delta(InstMapDelta, CallInfo1, CallInfo),
        Call = hlds_goal(CallExpr, CallInfo),

        % The resuling conjunction may not be flat. We deal with that after
        % the conjunction has been processed
        % (see update_original_predicate_goal).
        create_conj(UnifyGoal, Call, plain_conj, !:Goal)
    else
        true
    ).

    % Update the recursive calls of each goal in the list so that the pred_id
    % called is the one of the specialized procedure.
    %
:- pred update_original_predicate_goals(list(hlds_goal)::in,
    list(hlds_goal)::in, list(hlds_goal)::out, pred_id::in, proc_id::in,
    pred_id::in, sym_name::in, proc_info::in, proc_info::out, int::in) is det.

update_original_predicate_goals([], !GoalsAcc, _CallerPredId,
        _CallerProcId, _PredIdSpecialized, _SymNameSpecialized, !ProcInfo,
        _Distance).
update_original_predicate_goals([Goal0 | Goals], !GoalsAcc, CallerPredId,
        CallerProcId, PredIdSpecialized, SymNameSpecialized, !ProcInfo,
        Distance) :-
    update_original_predicate_goal(Goal0, Goal, CallerPredId, CallerProcId,
        PredIdSpecialized, SymNameSpecialized, !ProcInfo, Distance),
    list.append(!.GoalsAcc, [Goal], !:GoalsAcc),
    update_original_predicate_goals(Goals, !GoalsAcc, CallerPredId,
        CallerProcId, PredIdSpecialized, SymNameSpecialized, !ProcInfo,
        Distance).

    % Update the recursive calls of a switch so that the pred_id called is the
    % one of the specialized procedure.
    %
:- pred update_original_predicate_switch(
    list(case)::in, list(case)::in, list(case)::out, pred_id::in,
    proc_id::in, pred_id::in, sym_name::in, proc_info::in, proc_info::out,
    int::in) is det.

update_original_predicate_switch([], !CasesAcc, _CallerPredId, _CallerProcId,
        _PredIdSpecialized, _SymNameSpecialized, !ProcInfo, _Distance).
update_original_predicate_switch([Case | Cases], !CasesAcc, CallerPredId,
        CallerProcId, PredIdSpecialized, SymNameSpecialized, !ProcInfo,
        Distance) :-
    Case = case(MainConsId, OtherConsIds, Goal0),
    update_original_predicate_goal(Goal0, Goal, CallerPredId, CallerProcId,
        PredIdSpecialized, SymNameSpecialized, !ProcInfo, Distance),
    !:CasesAcc = [case(MainConsId, OtherConsIds, Goal) | !.CasesAcc],
    update_original_predicate_switch(Cases, !CasesAcc, CallerPredId,
        CallerProcId, PredIdSpecialized, SymNameSpecialized, !ProcInfo,
        Distance).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.distance_granularity.
%-----------------------------------------------------------------------------%
