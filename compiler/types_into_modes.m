%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module propagates type information into the argument modes
% of procedures.
%
%---------------------------------------------------------------------------%

:- module check_hlds.types_into_modes.
:- interface.

:- import_module check_hlds.inst_mode_type_prop.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.

%---------------------------------------------------------------------------%

    % Propagate type information into the argument modes of all
    % the procedures of the given predicate.
    %
    % Return a list of the ids of the procedures in which this process failed,
    % and a list of errors describing situations in which a named inst
    % was applied to a type whose type constructor is *not* the type
    % constructor that this inst was declared to be for.
    %
    % This predicate is exported for use by add_special_pred.m, which does
    % most of the task of this module itself, but delegates this one to us.
    %
:- pred propagate_checked_types_into_pred_modes(module_info::in,
    list(proc_id)::out, list(error_spec)::out,
    tprop_cache::in, tprop_cache::out, pred_info::in, pred_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module varset.

%---------------------------------------------------------------------------%

propagate_checked_types_into_pred_modes(ModuleInfo, ErrorProcIds,
        !:Specs, !Cache, !PredInfo) :-
    pred_info_get_proc_table(!.PredInfo, Procs0),
    ProcIds = pred_info_all_procids(!.PredInfo),
    !:Specs = [],
    propagate_checked_types_into_procs_modes(ModuleInfo, !.PredInfo, ProcIds,
        [], RevErrorProcIds, !Cache, !Specs, Procs0, Procs),
    list.reverse(RevErrorProcIds, ErrorProcIds),
    pred_info_set_proc_table(Procs, !PredInfo),
    pred_info_get_markers(!.PredInfo, Markers),
    ( if check_marker(Markers, marker_has_rhs_lambda) then
        % We have not copied goals in clauses to become the bodies
        % of procedures, so the lambda expressions in whose arguments
        % we should propagate types into insts exist only in the clauses_info.
        pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
        clauses_info_get_had_syntax_errors(ClausesInfo0, FoundSyntaxError),
        (
            FoundSyntaxError = some_clause_syntax_errors
            % Any errors we could generate could be spurious. Any that aren't
            % will be found and reported by the first compiler invocation
            % after the user fixes the syntax errors.
        ;
            FoundSyntaxError = no_clause_syntax_errors,
            clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, ItemNums),
            get_clause_list_for_replacement(ClausesRep0, Clauses0),
            VarTable = ClausesInfo0 ^ cli_var_table,
            propagate_checked_types_into_lambda_modes_in_clauses(ModuleInfo,
                VarTable, Clauses0, Clauses, !Cache, !Specs),
            set_clause_list(Clauses, ClausesRep),
            clauses_info_set_clauses_rep(ClausesRep, ItemNums,
                ClausesInfo0, ClausesInfo),
            pred_info_set_clauses_info(ClausesInfo, !PredInfo)
        )
    else
        true
    ).

%---------------------%

:- pred propagate_checked_types_into_procs_modes(module_info::in,
    pred_info::in, list(proc_id)::in, list(proc_id)::in, list(proc_id)::out,
    tprop_cache::in, tprop_cache::out,
    list(error_spec)::in, list(error_spec)::out,
    proc_table::in, proc_table::out) is det.

propagate_checked_types_into_procs_modes(_, _, [],
        !RevErrorProcIds, !Cache, !Specs, !Procs).
propagate_checked_types_into_procs_modes(ModuleInfo, PredInfo,
        [ProcId | ProcIds], !RevErrorProcIds, !Cache, !Specs, !Procs) :-
    propagate_checked_types_into_proc_modes(ModuleInfo, PredInfo, ProcId,
        !RevErrorProcIds, !Cache, !Specs, !Procs),
    propagate_checked_types_into_procs_modes(ModuleInfo, PredInfo, ProcIds,
        !RevErrorProcIds, !Cache, !Specs, !Procs).

:- pred propagate_checked_types_into_proc_modes(module_info::in,
    pred_info::in, proc_id::in, list(proc_id)::in, list(proc_id)::out,
    tprop_cache::in, tprop_cache::out,
    list(error_spec)::in, list(error_spec)::out,
    proc_table::in, proc_table::out) is det.

propagate_checked_types_into_proc_modes(ModuleInfo, PredInfo, ProcId,
        !RevErrorProcIds, !Cache, !Specs, !Procs) :-
    pred_info_get_arg_types(PredInfo, ArgTypes),
    map.lookup(!.Procs, ProcId, ProcInfo0),
    proc_info_get_argmodes(ProcInfo0, ArgModes0),
    propagate_checked_types_into_modes(ModuleInfo, ta_pred(PredInfo),
        ArgTypes, ArgModes0, ArgModes, !Cache, !Specs),

    % Check for unbound inst vars.
    %
    % This needs to be done after calling propagate_checked_types_into_modes,
    % because we need the insts to be module qualified.
    %
    % It also needs to be done before mode analysis, to avoid internal errors
    % in mode analysis.
    ( if
        mode_list_contains_inst_var(ArgModes, _InstVar)
        % XXX This should be
        %   some [InstVar] (
        %       mode_list_contains_inst_var(ArgModes, InstVar)
        %   )
        % but that gets a singleton variable warning, because quantification.m
        % replaces the list of quantified variables with the empty list
        % BEFORE the singleton variable warning is generated.
    then
        !:RevErrorProcIds = [ProcId | !.RevErrorProcIds]
    else
        proc_info_set_argmodes(ArgModes, ProcInfo0, ProcInfo),
        map.det_update(ProcId, ProcInfo, !Procs)
    ).

%---------------------%

:- pred propagate_checked_types_into_lambda_modes_in_clauses(module_info::in,
    var_table::in, list(clause)::in, list(clause)::out,
    tprop_cache::in, tprop_cache::out,
    list(error_spec)::in, list(error_spec)::out) is det.

propagate_checked_types_into_lambda_modes_in_clauses(_, _, [], [],
        !Cache, !Specs).
propagate_checked_types_into_lambda_modes_in_clauses(ModuleInfo, VarTable,
        [Clause0 | Clauses0], [Clause | Clauses], !Cache, !Specs) :-
    propagate_checked_types_into_lambda_modes_in_clause(ModuleInfo, VarTable,
        Clause0, Clause, !Cache, !Specs),
    propagate_checked_types_into_lambda_modes_in_clauses(ModuleInfo, VarTable,
        Clauses0, Clauses, !Cache, !Specs).

:- pred propagate_checked_types_into_lambda_modes_in_clause(module_info::in,
    var_table::in, clause::in, clause::out,
    tprop_cache::in, tprop_cache::out,
    list(error_spec)::in, list(error_spec)::out) is det.

propagate_checked_types_into_lambda_modes_in_clause(ModuleInfo, VarTable,
        Clause0, Clause, !Cache, !Specs) :-
    Lang = Clause0 ^ clause_lang,
    (
        Lang = impl_lang_mercury,
        Goal0 = Clause0 ^ clause_body,
        propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo, VarTable,
            Goal0, Goal, !Cache, !Specs),
        Clause = Clause0 ^ clause_body := Goal
    ;
        Lang = impl_lang_foreign(_),
        Clause = Clause0
    ).

:- pred propagate_checked_types_into_lambda_modes_in_goal(module_info::in,
    var_table::in, hlds_goal::in, hlds_goal::out,
    tprop_cache::in, tprop_cache::out,
    list(error_spec)::in, list(error_spec)::out) is det.

propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo, VarTable,
        Goal0, Goal, !Cache, !Specs) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = unify(LHS0, RHS0, UnifyMode0, Unification0, UniContext0),
        (
            ( RHS0 = rhs_var(_)
            ; RHS0 = rhs_functor(_, _, _)
            ),
            Goal = Goal0
        ;
            RHS0 = rhs_lambda_goal(Purity0, HOGroundness0, PorF0, EvalMethod0,
                ClosureVars0, ArgVarsModes0, Detism0, LambdaGoal0),
            list.length(ArgVarsModes0, NumArgs),
            Context = goal_info_get_context(GoalInfo0),
            Args = ta_lambda(PorF0, NumArgs, Context),
            propagate_checked_types_into_var_modes(ModuleInfo,
                VarTable, Args, 1, ArgVarsModes0, ArgVarsModes,
                !Cache, !Specs),
            propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
                VarTable, LambdaGoal0, LambdaGoal, !Cache, !Specs),
            RHS = rhs_lambda_goal(Purity0, HOGroundness0, PorF0, EvalMethod0,
                ClosureVars0, ArgVarsModes, Detism0, LambdaGoal),
            GoalExpr = unify(LHS0, RHS, UnifyMode0, Unification0, UniContext0),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = conj(ConjType, Conjuncts0),
        propagate_checked_types_into_lambda_modes_in_goals(ModuleInfo,
            VarTable, Conjuncts0, Conjuncts, !Cache, !Specs),
        GoalExpr = conj(ConjType, Conjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Disjuncts0),
        propagate_checked_types_into_lambda_modes_in_goals(ModuleInfo,
            VarTable, Disjuncts0, Disjuncts, !Cache, !Specs),
        GoalExpr = disj(Disjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var0, CanFail0, Cases0),
        propagate_checked_types_into_lambda_modes_in_cases(ModuleInfo,
            VarTable, Cases0, Cases, !Cache, !Specs),
        GoalExpr = switch(Var0, CanFail0, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars0, Cond0, Then0, Else0),
        propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
            VarTable, Cond0, Cond, !Cache, !Specs),
        propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
            VarTable, Then0, Then, !Cache, !Specs),
        propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
            VarTable, Else0, Else, !Cache, !Specs),
        GoalExpr = if_then_else(Vars0, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
            VarTable, SubGoal0, SubGoal, !Cache, !Specs),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason0, SubGoal0),
        propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
            VarTable, SubGoal0, SubGoal, !Cache, !Specs),
        GoalExpr = scope(Reason0, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = bi_implication(GoalA0, GoalB0),
            propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
                VarTable, GoalA0, GoalA, !Cache, !Specs),
            propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
                VarTable, GoalB0, GoalB, !Cache, !Specs),
            ShortHand = bi_implication(GoalA, GoalB)
        ;
            ShortHand0 = atomic_goal(AtomicGoalType0, OuterVars0, InnerVars0,
                OutputVars0, MainGoal0, OrElseGoals0, OrElseInners0),
            propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
                VarTable, MainGoal0, MainGoal, !Cache, !Specs),
            propagate_checked_types_into_lambda_modes_in_goals(ModuleInfo,
                VarTable, OrElseGoals0, OrElseGoals, !Cache, !Specs),
            ShortHand = atomic_goal(AtomicGoalType0, OuterVars0, InnerVars0,
                OutputVars0, MainGoal, OrElseGoals, OrElseInners0)
        ;
            ShortHand0 = try_goal(MaybeIOVars0, ResultVars0, SubGoal0),
            propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
                VarTable, SubGoal0, SubGoal, !Cache, !Specs),
            ShortHand = try_goal(MaybeIOVars0, ResultVars0, SubGoal)
        ),
        GoalExpr = shorthand(ShortHand),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ).

:- pred propagate_checked_types_into_lambda_modes_in_goals(module_info::in,
    var_table::in, list(hlds_goal)::in, list(hlds_goal)::out,
    tprop_cache::in, tprop_cache::out,
    list(error_spec)::in, list(error_spec)::out) is det.

propagate_checked_types_into_lambda_modes_in_goals(_, _, [], [],
        !Cache, !Specs).
propagate_checked_types_into_lambda_modes_in_goals(ModuleInfo, VarTable,
        [Goal0 | Goals0], [Goal | Goals], !Cache, !Specs) :-
    propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo, VarTable,
        Goal0, Goal, !Cache, !Specs),
    propagate_checked_types_into_lambda_modes_in_goals(ModuleInfo, VarTable,
        Goals0, Goals, !Cache, !Specs).

:- pred propagate_checked_types_into_lambda_modes_in_cases(module_info::in,
    var_table::in, list(case)::in, list(case)::out,
    tprop_cache::in, tprop_cache::out,
    list(error_spec)::in, list(error_spec)::out) is det.

propagate_checked_types_into_lambda_modes_in_cases(_, _, [], [],
        !Cache, !Specs).
propagate_checked_types_into_lambda_modes_in_cases(ModuleInfo, VarTable,
        [Case0 | Cases0], [Case | Cases], !Cache, !Specs) :-
    Case0 = case(MainConsId0, OtherConsIds0, Goal0),
    propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo, VarTable,
        Goal0, Goal, !Cache, !Specs),
    Case = case(MainConsId0, OtherConsIds0, Goal),
    propagate_checked_types_into_lambda_modes_in_cases(ModuleInfo, VarTable,
        Cases0, Cases, !Cache, !Specs).

%---------------------%

:- pred propagate_checked_types_into_var_modes(module_info::in, var_table::in,
    tprop_args::in, int::in,
    assoc_list(prog_var, mer_mode)::in, assoc_list(prog_var, mer_mode)::out,
    tprop_cache::in, tprop_cache::out,
    list(error_spec)::in, list(error_spec)::out) is det.

propagate_checked_types_into_var_modes(_, _, _, _, [], [], !Cache, !Specs).
propagate_checked_types_into_var_modes(ModuleInfo, VarTable, Args, ArgNum,
        [Var - Mode0 | VarsModes0], [Var - Mode | VarsModes],
        !Cache, !Specs) :-
    lookup_var_type(VarTable, Var, Type),
    Context = tprop_arg_list_slot(Args, ArgNum),
    propagate_checked_type_into_mode(ModuleInfo, Context,
        Type, Mode0, Mode, !Cache, !Specs),
    propagate_checked_types_into_var_modes(ModuleInfo, VarTable,
        Args, ArgNum + 1, VarsModes0, VarsModes, !Cache, !Specs).

%---------------------------------------------------------------------------%
:- end_module check_hlds.types_into_modes.
%---------------------------------------------------------------------------%
