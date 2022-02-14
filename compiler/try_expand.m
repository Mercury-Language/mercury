%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public Licence - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: try_expand.m
% Author: wangp.
%
% Try goals are implemented by replacing them with calls to predicates in the
% `exception' module.  For example, goal such as:
%
%      (try [] p(X, Y)
%       then q(X, Y)
%       else r
%       catch ...
%      )
%
% is expanded to:
%
%      exception.try(
%          (pred(OutputTuple::out) is semidet :-
%              p(X, Y),
%              OutputTuple = {X, Y}
%          ), TryResult),
%      (
%          TryResult = succeeded({X, Y}),
%          q(X, Y)
%      ;
%          TryResult = failed,
%          r
%      ;
%          TryResult = exception(Excp),
%          ...exception handling...
%      )
%
% The transformation requires us to know which variables are bound in the
% higher order term that is passed to `try', as well as the determinism, so we
% can't transform them immediately when converting from the parse tree to HLDS
% representations.  But try goals have very complex control flows and we don't
% really want to write and maintain mode, determinism, and other analyses to
% work on them directly either.
%
% Instead, we cheat a little and "pre-transform" try goals very early on (when
% converting from a parse tree to an HLDS) into something that resembles
% somewhat the final forms, e.g.
%
%      magic_exception_result(TryResult),  % out(cannot_fail)
%      (
%          TryResult = succeeded({}),
%          ( if p(X, Y) then
%              q(X, Y)
%          else
%              r
%          )
%      ;
%          TryResult = exception(Excp),
%          ...exception handling...
%      )
%
% We let the semantic checks work on these pre-transformed goals.  Afterwards
% we pick out the various pieces and construct the proper, final goals.
%
%-----------------------------------------------------------------------------%
%
% PRE-TRANSFORMATION (implemented in add_clause.m)
%
% 1. try goal without I/O but with an else part
%
%      magic_exception_result(TryResult),  % out(cannot_fail)
%      (
%          TryResult = succeeded({}),
%          ( if <Goal> then
%              <Then>
%          else
%              <Else>
%          )
%      ;
%          TryResult = exception(Excp),
%          <ExcpHandling>
%      )
%
%   As intended, variables bound in <Goal> are only in scope within <Then>,
%   not <Else> nor <ExcpHandling>.
%
% 2. try goal without I/O and without an else part, or
% 3. try goal with I/O
%
%       magic_exception_result(TryResult),  % out(cannot_fail)
%       (
%           TryResult = succeeded({}),
%           some [] <Goal>,
%           some [] <Then>
%       ;
%           TryResult = exception(Excp),
%           <ExcpHandling>
%       )
%
%   The `some' scopes there so that we can distinguish between <Goal> and
%   <Then> later. (They act as barrier scopes, except we can't introduce
%   barrier scopes then.  We depend on the early analyses not to move things
%   in and out of `some' scopes.)
%
%-----------------------------------------------------------------------------%
%
% POST-TRANSFORMATION (implemented in this module)
%
% 1. try goal without I/O, and can fail
%
%      try((pred(OutputTuple::out) is semidet :-
%              <Goal>,
%              OutputTuple = { <BoundVars> }
%          ), TryResult),
%      (
%          TryResult = succeeded(TmpTupleVar),
%          inst_cast(TmpTupleVar, TupleVar),
%          TupleVar = { <BoundVars> },
%          <Then>
%      ;
%          TryResult = failed,
%          <Else>
%      ;
%          TryResult = exception(Excp),
%          <ExcpHandling>
%      )
%
% 2. try goal without I/O, and cannot fail
%
%      try((pred(OutputTuple::out) is det :-
%              <Goal>,
%              OutputTuple = { <BoundVars> }
%          ), TryResult),
%      (
%          TryResult = succeeded(TmpTupleVar),
%          inst_cast(TmpTupleVar, TupleVar),
%          TupleVar = { <BoundVars> },
%          <Then>
%      ;
%          TryResult = exception(Excp),
%          <ExcpHandling>
%      )
%
% 3. try goal with I/O
%
%      try_io((pred(OutputTuple::out, !.IO::di, !:IO::uo) is det :-
%              <Goal>,
%              OutputTuple = { <BoundVars> }
%          ), TryResult, !IO),
%      (
%          TryResult = succeeded(TmpTupleVar),
%          inst_cast(TmpTupleVar, TupleVar),
%          TupleVar = { <BoundVars> },
%          <Then>
%      ;
%          TryResult = exception(Excp),
%          <ExcpHandling>
%      )
%
%   We have to rename an io.state variable in ExcpHandling so that the sequence
%   begins with the output I/O state of the `try_io' call.
%
% The inst casts preserve the known insts of the BoundVars, which were lost due
% to the calls to try*.
%
% The <ExcpHandling> parts can be passed through from the pre-transformation.
% If a `catch_any' is present the exception handling looks like this:
%
%      ( if exc_univ_to_type(Excp, <CatchPattern1>) then
%          <CatchGoal1>
%      else if exc_univ_to_type(Excp, <CatchPattern2>) then
%          <CatchGoal2>
%      else
%          CatchAnyVar = exc_univ_value(Excp),
%          <CatchAnyGoal>
%      )
%
% Otherwise, if `catch_any' is not present:
%
%      ( if exc_univ_to_type(Excp, <CatchPattern1>) then
%          <CatchGoal1>
%      else if exc_univ_to_type(Excp, <CatchPattern2>) then
%          <CatchGoal2>
%      else
%          rethrow(TryResult)
%      )
%
%-----------------------------------------------------------------------------%

:- module check_hlds.try_expand.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred expand_try_goals_in_module(module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_analysis.
:- import_module check_hlds.det_report.
:- import_module check_hlds.modes.
:- import_module check_hlds.polymorphism_type_info.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

expand_try_goals_in_module(!ModuleInfo, !Specs) :-
    % The exception module is implicitly imported if any try goals were seen,
    % so if the exception module is not imported, then we know there are
    % no try goals to be expanded.
    module_info_get_avail_module_map(!.ModuleInfo, AvailModuleMap),
    ( if map.search(AvailModuleMap, mercury_exception_module, _) then
        some [!Globals] (
            module_info_get_globals(!.ModuleInfo, !:Globals),
            disable_det_warnings(OptionsToRestore, !Globals),
            module_info_set_globals(!.Globals, !ModuleInfo),

            module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
            list.foldl2(expand_try_goals_in_pred, PredIds,
                !ModuleInfo, !Specs),

            module_info_get_globals(!.ModuleInfo, !:Globals),
            restore_det_warnings(OptionsToRestore, !Globals),
            module_info_set_globals(!.Globals, !ModuleInfo)
        )
    else
        true
    ).

:- pred expand_try_goals_in_pred(pred_id::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

expand_try_goals_in_pred(PredId, !ModuleInfo, !Specs) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_non_imported_procids(PredInfo),
    list.foldl2(expand_try_goals_in_proc(PredId), ProcIds,
        !ModuleInfo, !Specs).

:- pred expand_try_goals_in_proc(pred_id::in, proc_id::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

expand_try_goals_in_proc(PredId, ProcId, !ModuleInfo, !Specs) :-
    some [!PredInfo, !ProcInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            !:PredInfo, !:ProcInfo),
        proc_info_get_goal(!.ProcInfo, Goal0),
        proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InitInstMap),

        Info0 = trys_info(!.ModuleInfo, !.PredInfo, !.ProcInfo, no),
        expand_try_goals_in_goal(InitInstMap, Goal0, Goal, Info0, Info),
        Info = trys_info(!:ModuleInfo, !:PredInfo, !:ProcInfo, Changed),

        (
            Changed = yes,
            update_changed_proc(Goal, PredId, ProcId, !.PredInfo, !.ProcInfo,
                !ModuleInfo, !Specs),
            module_info_clobber_dependency_info(!ModuleInfo)
        ;
            Changed = no
        )
    ).

:- pred update_changed_proc(hlds_goal::in, pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

update_changed_proc(Goal, PredId, ProcId, PredInfo, !.ProcInfo, !ModuleInfo,
        !Specs) :-
    proc_info_set_goal(Goal, !ProcInfo),
    requantify_proc_general(ordinary_nonlocals_maybe_lambda, !ProcInfo),
    module_info_set_pred_proc_info(PredId, ProcId, PredInfo, !.ProcInfo,
        !ModuleInfo),

    modecheck_proc(PredId, ProcId, !ModuleInfo, _Changed, ModeSpecs),
    module_info_get_globals(!.ModuleInfo, Globals),
    HasModeErrors = contains_errors(Globals, ModeSpecs),
    (
        HasModeErrors = yes,
        % In some cases we may detect mode errors after expanding try goals
        % which were missed before, so we don't abort the compiler, but we do
        % stop compiling not long after this pass.
        !:Specs = ModeSpecs ++ !.Specs
    ;
        HasModeErrors = no,
        % Determinism errors should have been detected before expansion, but
        % compilation would continue anyway.
        % XXX It would be nice to replace any pre-expansion determinism error
        % messages (which mention the hidden predicate magic_exception_result)
        % with these error messages.
        determinism_check_proc(ProcId, PredId, !ModuleInfo, _DetismSpecs)
    ).

%-----------------------------------------------------------------------------%

:- type trys_info
    --->    trys_info(
                ti_module_info  :: module_info,
                ti_pred_info    :: pred_info,
                ti_proc_info    :: proc_info,
                ti_changed      :: bool
            ).

:- pred expand_try_goals_in_goal(instmap::in, hlds_goal::in, hlds_goal::out,
    trys_info::in, trys_info::out) is det.

expand_try_goals_in_goal(InstMap, Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = unify(_, _, _, _, _),
        Goal = Goal0
    ;
        GoalExpr0 = conj(ConjType, Conjuncts0),
        expand_try_goals_in_conj(InstMap, Conjuncts0, Conjuncts, !Info),
        GoalExpr = conj(ConjType, Conjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Disjuncts0),
        expand_try_goals_in_disj(InstMap, Disjuncts0, Disjuncts, !Info),
        GoalExpr = disj(Disjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        expand_try_goals_in_goal(InstMap, SubGoal0, SubGoal, !Info),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        expand_try_goals_in_cases(InstMap, Cases0, Cases, !Info),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, InnerGoal0),
        (
            Reason = from_ground_term(_, from_ground_term_construct),
            % There can be no try goals inside this scope.
            Goal = Goal0
        ;
            ( Reason = disable_warnings(_, _)
            ; Reason = exist_quant(_)
            ; Reason = promise_solutions(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = from_ground_term(_, from_ground_term_deconstruct)
            ; Reason = from_ground_term(_, from_ground_term_other)
            ; Reason = trace_goal(_, _, _, _, _)
            ; Reason = loop_control(_, _, _)
            ),
            expand_try_goals_in_goal(InstMap, InnerGoal0, InnerGoal, !Info),
            GoalExpr = scope(Reason, InnerGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            Reason = from_ground_term(_, from_ground_term_initial),
            unexpected($pred, "from_ground_term_initial")
        )
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        expand_try_goals_in_if_then_else(InstMap, Cond0, Cond, Then0, Then,
            Else0, Else, !Info),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        ( GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        % This should be expanded out at this stage
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = try_goal(_, _, _),
            expand_try_goal(InstMap, ShortHand0, Goal, !Info)
        ;
            ShortHand0 = atomic_goal(AtomicGoalType, Outer, Inner,
                MaybeOutputVars, MainGoal0, OrElseGoals0, OrElseInners),
            expand_try_goals_in_goal(InstMap, MainGoal0, MainGoal, !Info),
            expand_try_goals_in_disj(InstMap, OrElseGoals0, OrElseGoals,
                !Info),
            GoalExpr = atomic_goal(AtomicGoalType, Outer, Inner,
                MaybeOutputVars, MainGoal, OrElseGoals, OrElseInners),
            Goal = hlds_goal(shorthand(GoalExpr), GoalInfo0)
        ;
            ShortHand0 = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        )
    ).

:- pred expand_try_goals_in_conj(instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    trys_info::in, trys_info::out) is det.

expand_try_goals_in_conj(_InstMap0, [], [], !Info).
expand_try_goals_in_conj(InstMap0, [Goal0 | Goals0], [Goal | Goals], !Info) :-
    expand_try_goals_in_goal(InstMap0, Goal0, Goal, !Info),
    Goal0 = hlds_goal(_, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstMapDelta, InstMap0, InstMap),
    expand_try_goals_in_conj(InstMap, Goals0, Goals, !Info).

:- pred expand_try_goals_in_disj(instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    trys_info::in, trys_info::out) is det.

expand_try_goals_in_disj(InstMap0, Goals0, Goals, !Info) :-
    list.map_foldl(expand_try_goals_in_goal(InstMap0), Goals0, Goals, !Info).

:- pred expand_try_goals_in_cases(instmap::in, list(case)::in, list(case)::out,
    trys_info::in, trys_info::out) is det.

expand_try_goals_in_cases(_InstMap0, [], [], !Info).
expand_try_goals_in_cases(InstMap0, [Case0 | Cases0], [Case | Cases], !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    expand_try_goals_in_goal(InstMap0, Goal0, Goal, !Info),
    expand_try_goals_in_cases(InstMap0, Cases0, Cases, !Info),
    Case = case(MainConsId, OtherConsIds, Goal).

:- pred expand_try_goals_in_if_then_else(instmap::in,
    hlds_goal::in, hlds_goal::out, hlds_goal::in,
    hlds_goal::out, hlds_goal::in, hlds_goal::out,
    trys_info::in, trys_info::out) is det.

expand_try_goals_in_if_then_else(InstMap0, Cond0, Cond, Then0, Then,
        Else0, Else, !Info) :-
    expand_try_goals_in_goal(InstMap0, Cond0, Cond, !Info),

    Cond0 = hlds_goal(_, CondInfo),
    CondInstMapDelta = goal_info_get_instmap_delta(CondInfo),
    apply_instmap_delta(CondInstMapDelta, InstMap0, InstMapAfterCond),
    expand_try_goals_in_goal(InstMapAfterCond, Then0, Then, !Info),

    expand_try_goals_in_goal(InstMap0, Else0, Else, !Info).

%-----------------------------------------------------------------------------%

:- inst try_goal for shorthand_goal_expr/0
    --->    try_goal(ground, ground, ground).

:- pred expand_try_goal(instmap::in, shorthand_goal_expr::in(try_goal),
    hlds_goal::out, trys_info::in, trys_info::out) is det.

expand_try_goal(InstMap, TryGoal, FinalGoal, !Info) :-
    TryGoal = try_goal(MaybeIO, ResultVar, IntermediateGoal),
    extract_intermediate_goal_parts(!.Info ^ ti_module_info, ResultVar,
        IntermediateGoal, Goal0, Then0, MaybeElse0, ExcpHandling0),

    % Handle nested try goals.
    expand_try_goals_in_goal(InstMap, Goal0, Goal1, !Info),
    update_instmap(Goal0, InstMap, InstMapAfterGoal),
    expand_try_goals_in_goal(InstMapAfterGoal, Then0, Then1, !Info),
    (
        MaybeElse0 = yes(Else0),
        expand_try_goals_in_goal(InstMap, Else0, Else1, !Info),
        MaybeElse1 = yes(Else1)
    ;
        MaybeElse0 = no,
        MaybeElse1 = no
    ),
    expand_try_goals_in_goal(InstMap, ExcpHandling0, ExcpHandling1, !Info),

    % Find the output variables.  Note we use Goal0, not Goal1, as any nested
    % tries would have been transformed will mess up the calculation.
    bound_nonlocals_in_goal(!.Info ^ ti_module_info, InstMap, Goal0,
        GoalOutputVarsSet0),
    (
        MaybeIO = yes(try_io_state_vars(_IOStateVarInitial, IOStateVarFinal)),
        set_of_var.delete(IOStateVarFinal,
            GoalOutputVarsSet0, GoalOutputVarsSet)
    ;
        MaybeIO = no,
        GoalOutputVarsSet = GoalOutputVarsSet0
    ),

    some [!ModuleInfo, !PredInfo, !ProcInfo, !VarTypes] (
        !.Info = trys_info(!:ModuleInfo, !:PredInfo, !:ProcInfo, _),
        expand_try_goal_2(MaybeIO, ResultVar, Goal1, Then1, MaybeElse1,
            ExcpHandling1, InstMapAfterGoal, GoalOutputVarsSet, FinalGoal,
            !PredInfo, !ProcInfo, !ModuleInfo),
        !:Info = trys_info(!.ModuleInfo, !.PredInfo, !.ProcInfo, yes)
    ).

:- pred expand_try_goal_2(maybe(try_io_state_vars)::in, prog_var::in,
    hlds_goal::in, hlds_goal::in, maybe(hlds_goal)::in, hlds_goal::in,
    instmap::in, set_of_progvar::in, hlds_goal::out,
    pred_info::in, pred_info::out, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

expand_try_goal_2(MaybeIO, ResultVar, Goal1, Then1, MaybeElse1, ExcpHandling1,
        InstMap, GoalOutputVarsSet, FinalGoal,
        !PredInfo, !ProcInfo, !ModuleInfo) :-
    some [!VarTypes] (
        % Get the type of the output tuple.
        proc_info_get_vartypes(!.ProcInfo, !:VarTypes),
        GoalOutputVars = set_of_var.to_sorted_list(GoalOutputVarsSet),
        lookup_var_types(!.VarTypes, GoalOutputVars, GoalOutputVarTypes),
        OutputTupleType = tuple_type(GoalOutputVarTypes, kind_star),

        % Fix the type of the result of the try call, now that we know what it
        % should be.
        RealResultVarType = defined_type(
            qualified(mercury_exception_module, "exception_result"),
            [OutputTupleType], kind_star),
        update_var_type(ResultVar, RealResultVarType, !VarTypes),
        proc_info_set_vartypes(!.VarTypes, !ProcInfo)
    ),

    make_try_lambda(Goal1, GoalOutputVarsSet, OutputTupleType, MaybeIO,
        LambdaVar, AssignLambdaVar, !ProcInfo),

    Goal1 = hlds_goal(_, GoalInfo1),
    GoalPurity = goal_info_get_purity(GoalInfo1),
    GoalContext = goal_info_get_context(GoalInfo1),

    (
        MaybeIO = yes(try_io_state_vars(GoalInitialIOVar, GoalFinalIOVar)),

        % We need to rearrange I/O state variables a bit.
        %
        % Let Goal take the I/O state from GoalInitialIOVar to GoalFinalIOVar.
        % The input to try_io will be GoalInitialIOVar.
        % Let the output of try_io to be TryIOOutputVar.
        %
        % Due to the pre-transformation, ExcpHandling also takes the I/O state
        % from GoalInitialIOVar to GoalFinalIOVar.  We need to rename
        % GoalInitialIOVar to TryIOOutputVar as the exception handling code
        % follows the try_io call.
        %
        % We cannot let TryIOOutputVar be GoalFinalIOVar, as the latter may
        % already appear somewhere in ExcpHandling.  TryIOOutputVar must be a
        % new variable.
        %
        % The Then part starts the I/O state sequence from GoalFinalIOVar, so
        % we need to unify "GoalFinalIOVar = TryIOOutputVar".  We don't use
        % renaming in this case because GoalFinalIOVar might not even occur in
        % the Then part; then renaming would lead to a mode error.

        proc_info_create_var_from_type(io_state_type, yes("TryIOOutput"),
            TryIOOutputVar, !ProcInfo),
        make_try_call("try_io", LambdaVar, ResultVar,
            [GoalInitialIOVar, TryIOOutputVar], OutputTupleType, GoalPurity,
            GoalContext, CallTryGoal, !PredInfo, !ProcInfo, !ModuleInfo),

        create_pure_atomic_complicated_unification(GoalFinalIOVar,
            rhs_var(TryIOOutputVar), term.context_init,
            umc_implicit("try_expand"), [], UnifyThenInitialIOVar),
        conjoin_goals(UnifyThenInitialIOVar, Then1, Then),

        RenamingExcp =
            map.from_assoc_list([GoalInitialIOVar - TryIOOutputVar]),
        rename_some_vars_in_goal(RenamingExcp, ExcpHandling1, ExcpHandling)
    ;
        MaybeIO = no,
        make_try_call("try", LambdaVar, ResultVar, [], OutputTupleType,
            GoalPurity, GoalContext, CallTryGoal, !PredInfo, !ProcInfo,
            !ModuleInfo),
        Then = Then1,
        ExcpHandling = ExcpHandling1
    ),

    goal_info_init(GoalInfo),

    % The `succeeded' case.
    proc_info_create_var_from_type(OutputTupleType, yes("TmpOutputTuple"),
        TmpTupleVar, !ProcInfo),
    proc_info_create_var_from_type(OutputTupleType, yes("OutputTuple"),
        TupleVar, !ProcInfo),
    deconstruct_functor(ResultVar, exception_succeeded_functor, [TmpTupleVar],
        DeconstructSucceeded),
    instmap_lookup_vars(InstMap, GoalOutputVars, TupleArgInsts),
    make_output_tuple_inst_cast(TmpTupleVar, TupleVar, TupleArgInsts,
        CastOutputTuple),
    deconstruct_tuple(TupleVar, GoalOutputVars, DeconstructOutputs),
    conj_list_to_goal([DeconstructSucceeded, CastOutputTuple,
        DeconstructOutputs, Then], GoalInfo, DeconstructsThen),
    SucceededCase = case(exception_succeeded_functor, [], DeconstructsThen),

    % The `exception' case.
    ExceptionCase = case(exception_exception_functor, [], ExcpHandling),

    % The `failed' case.
    (
        MaybeElse1 = yes(Else1),
        FailedCase = case(exception_failed_functor, [], Else1),
        MaybeFailedCase = [FailedCase]
    ;
        MaybeElse1 = no,
        MaybeFailedCase = []
    ),

    Cases = [SucceededCase, ExceptionCase | MaybeFailedCase],
    ResultSwitch = hlds_goal(switch(ResultVar, cannot_fail, Cases), GoalInfo),
    conj_list_to_goal([AssignLambdaVar, CallTryGoal, ResultSwitch], GoalInfo,
        FinalGoal).

    % Pick out the parts of the original try goal from a pre-transformed goal.
    %
:- pred extract_intermediate_goal_parts(module_info::in, prog_var::in,
    hlds_goal::in, hlds_goal::out, hlds_goal::out, maybe(hlds_goal)::out,
    hlds_goal::out) is det.

extract_intermediate_goal_parts(ModuleInfo, ResultVar, IntermediateGoal,
        Goal, Then, MaybeElse, ExcpHandling) :-
    ( if
        extract_intermediate_goal_parts_2(ModuleInfo, ResultVar,
            IntermediateGoal, GoalPrime, ThenPrime, MaybeElsePrime,
            ExcpHandlingPrime)
    then
        Goal = GoalPrime,
        Then = ThenPrime,
        MaybeElse = MaybeElsePrime,
        ExcpHandling = ExcpHandlingPrime
    else if
        % This form should only be encountered if there was an error in the
        % program, when the inner goal may fail, in a context where it must not
        % fail.  Compilation doesn't stop immediately after determinism
        % analysis detects the error so we need to handle this form as well.
        IntermediateGoal = hlds_goal(scope(_, ScopedGoal), _),
        extract_intermediate_goal_parts_2(ModuleInfo, ResultVar,
            ScopedGoal, GoalPrime, ThenPrime, MaybeElsePrime,
            ExcpHandlingPrime)
    then
        Goal = GoalPrime,
        Then = ThenPrime,
        MaybeElse = MaybeElsePrime,
        ExcpHandling = ExcpHandlingPrime
    else
        unexpected($pred, "unexpected goal form")
    ).

:- pred extract_intermediate_goal_parts_2(module_info::in, prog_var::in,
    hlds_goal::in, hlds_goal::out, hlds_goal::out, maybe(hlds_goal)::out,
    hlds_goal::out) is semidet.

extract_intermediate_goal_parts_2(ModuleInfo, ResultVar, IntermediateGoal,
        Goal, Then, MaybeElse, ExcpHandling) :-
    IntermediateGoal = hlds_goal(conj(plain_conj, Conjuncts), _),
    Conjuncts = [
        hlds_goal(MagicCall, _),
        hlds_goal(Switch, _)
    ],
    MagicCall = plain_call(_, _, [ResultVar], _, _, _),
    Switch = switch(ResultVar, cannot_fail, Cases),

    lookup_case_goal(Cases, exception_succeeded_functor, SucceededGoal),
    extract_from_succeeded_goal(ModuleInfo, SucceededGoal, Goal, Then,
        MaybeElse),

    lookup_case_goal(Cases, exception_exception_functor, ExcpHandling).

    % There are two forms we could extract when TryResult has the
    % functor exception.succeeded/1.
    %
    %      TryResult = exception.succeeded(V),
    %      V = {},
    %      ( if Goal then
    %          Then
    %      else
    %          Else
    %      ),
    %      Rest
    %
    % or:
    %
    %      TryResult = exception.succeeded(V),
    %      V = {},
    %      some [] Goal,
    %      some [] Then,
    %      Rest
    %
:- pred extract_from_succeeded_goal(module_info::in, hlds_goal::in,
    hlds_goal::out, hlds_goal::out, maybe(hlds_goal)::out) is semidet.

extract_from_succeeded_goal(ModuleInfo, SucceededGoal, Goal, Then,
        MaybeElse) :-
    SucceededGoal = hlds_goal(conj(plain_conj, Conjuncts0), _),
    Conjuncts0 = [DeconstructResult, TestNullTuple | Conjuncts1],
    DeconstructResult = hlds_goal(unify(_ResultVar, _, _, _, _), _),
    TestNullTuple = hlds_goal(unify(_, TestRHS, _, _, _), _),
    TestRHS = rhs_functor(tuple_cons(0), is_not_exist_constr, []),

    ( if
        Conjuncts1 = [hlds_goal(IfThenElse, _) | Rest],
        IfThenElse = if_then_else(_, GoalPrime, Then0, Else0)
    then
        Goal = GoalPrime,

        % If Goal is erroneous the Then part may have been optimised away to
        % `true'.  However, we will be separating the Goal and Then parts in
        % the final goal, so the knowledge that Goal won't succeed will be lost
        % to the mode checker.  In that case we replace the Then goal by a call
        % to an `erroneous' procedure.
        Goal = hlds_goal(_, GoalInfo),
        GoalDetism = goal_info_get_determinism(GoalInfo),
        determinism_components(GoalDetism, _, GoalMaxSoln),
        (
            GoalMaxSoln = at_most_zero,
            make_unreachable_call(ModuleInfo, Then)
        ;
            ( GoalMaxSoln = at_most_one
            ; GoalMaxSoln = at_most_many_cc
            ; GoalMaxSoln = at_most_many
            ),
            conjoin_goal_and_goal_list(Then0, Rest, Then)
        ),

        conjoin_goal_and_goal_list(Else0, Rest, Else),
        MaybeElse = yes(Else)
    else
        Conjuncts1 = [SomeGoal | AfterSomeGoal],
        SomeGoal = hlds_goal(scope(exist_quant([]), Goal), _),
        ( if
            AfterSomeGoal = [SomeThen | Rest],
            SomeThen = hlds_goal(scope(exist_quant([]), Then0), _)
        then
            conjoin_goal_and_goal_list(Then0, Rest, Then),
            MaybeElse = no
        else
            % If "some [] Then" is missing then "some [] Goal" must be
            % `erroneous'.  Make the Then part into a call to an erroneous
            % procedure.
            Goal = hlds_goal(_, GoalInfo),
            GoalDetism = goal_info_get_determinism(GoalInfo),
            determinism_components(GoalDetism, _, GoalMaxSoln),
            (
                GoalMaxSoln = at_most_zero,
                make_unreachable_call(ModuleInfo, Then),
                MaybeElse = no
            ;
                ( GoalMaxSoln = at_most_one
                ; GoalMaxSoln = at_most_many_cc
                ; GoalMaxSoln = at_most_many
                ),
                unexpected($pred, "goal not erroneous")
            )
        )
    ).

:- pred lookup_case_goal(list(case)::in, cons_id::in, hlds_goal::out) is det.

lookup_case_goal([], ConsId, _) :-
    unexpected($pred, "couldn't find " ++ string(ConsId)).
lookup_case_goal([Case | Cases], ConsId, Goal) :-
    ( if Case = case(ConsId, [], GoalPrime) then
        Goal = GoalPrime
    else
        lookup_case_goal(Cases, ConsId, Goal)
    ).

:- pred bound_nonlocals_in_goal(module_info::in, instmap::in, hlds_goal::in,
    set_of_progvar::out) is det.

bound_nonlocals_in_goal(ModuleInfo, InstMap, Goal, BoundNonLocals) :-
    Goal = hlds_goal(_, GoalInfo),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    BoundNonLocals = set_of_var.filter(
        var_is_bound_in_instmap_delta(ModuleInfo, InstMap, InstMapDelta),
        NonLocals).

:- pred make_try_lambda(hlds_goal::in, set_of_progvar::in, mer_type::in,
    maybe(try_io_state_vars)::in, prog_var::out, hlds_goal::out,
    proc_info::in, proc_info::out) is det.

make_try_lambda(Body0, OutputVarsSet, OutputTupleType, MaybeIO,
        LambdaVar, AssignLambdaVarGoal, !ProcInfo) :-
    Body0 = hlds_goal(_, BodyInfo0),
    NonLocals0 = goal_info_get_nonlocals(BodyInfo0),
    set_of_var.difference(NonLocals0, OutputVarsSet, NonLocals1),

    proc_info_create_var_from_type(OutputTupleType, yes("OutputTuple"),
        OutputTupleVar, !ProcInfo),
    (
        MaybeIO = yes(try_io_state_vars(IOVarInitial, IOVarFinal)),
        LambdaParamsModes = [OutputTupleVar - out_mode,
            IOVarInitial - di_mode, IOVarFinal - uo_mode],
        LambdaParamTypes = [OutputTupleType, io_state_type, io_state_type],
        set_of_var.delete(IOVarFinal, NonLocals1, NonLocals)
    ;
        MaybeIO = no,
        LambdaParamsModes = [OutputTupleVar - out_mode],
        LambdaParamTypes = [OutputTupleType],
        NonLocals = NonLocals0
    ),
    LambdaType = higher_order_type(pf_predicate, LambdaParamTypes,
        none_or_default_func, purity_pure, lambda_normal),
    proc_info_create_var_from_type(LambdaType, yes("TryLambda"), LambdaVar,
        !ProcInfo),

    % Add the construction of OutputTuple to the body.
    construct_tuple(OutputTupleVar, set_of_var.to_sorted_list(OutputVarsSet),
        MakeOutputTuple),
    conjoin_goals(Body0, MakeOutputTuple, LambdaBody0),

    % Rename away output variables in the lambda body.
    proc_info_get_varset(!.ProcInfo, VarSet0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    clone_variables(set_of_var.to_sorted_list(OutputVarsSet),
        VarSet0, VarTypes0, VarSet0, VarSet, VarTypes0, VarTypes,
        map.init, Renaming),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    rename_some_vars_in_goal(Renaming, LambdaBody0, LambdaBody),

    % Get the determinism of the lambda.
    LambdaBody = hlds_goal(_, BodyGoalInfo),
    BodyDetism = goal_info_get_determinism(BodyGoalInfo),
    detism_to_try_lambda_detism(BodyDetism, LambdaDetism),

    % Make the lambda assignment.
    RHS = rhs_lambda_goal(purity_pure, ho_ground, pf_predicate, lambda_normal,
        set_of_var.to_sorted_list(NonLocals), LambdaParamsModes,
        LambdaDetism, LambdaBody),
    create_pure_atomic_complicated_unification(LambdaVar, RHS,
        term.context_init, umc_implicit("try_expand"), [],
        AssignLambdaVarGoal0),
    goal_add_feature(feature_lambda_from_try,
        AssignLambdaVarGoal0, AssignLambdaVarGoal).

    % try* don't cover all possible determinisms so we have generate lambdas
    % with less restrictive determinisms.
    %
:- pred detism_to_try_lambda_detism(determinism::in, determinism::out) is det.

detism_to_try_lambda_detism(detism_det, detism_det).
detism_to_try_lambda_detism(detism_semi, detism_semi).
detism_to_try_lambda_detism(detism_multi, detism_cc_multi).
detism_to_try_lambda_detism(detism_non, detism_cc_non).
detism_to_try_lambda_detism(detism_cc_multi, detism_cc_multi).
detism_to_try_lambda_detism(detism_cc_non, detism_cc_non).
detism_to_try_lambda_detism(detism_erroneous, detism_det).
detism_to_try_lambda_detism(detism_failure, detism_semi).

:- pred make_try_call(string::in, prog_var::in, prog_var::in,
    list(prog_var)::in, mer_type::in, purity::in, prog_context::in,
    hlds_goal::out,
    pred_info::in, pred_info::out, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

make_try_call(PredName, LambdaVar, ResultVar, ExtraArgs, OutputTupleType,
        GoalPurity, Context, OverallGoal, !PredInfo, !ProcInfo, !ModuleInfo) :-
    polymorphism_make_type_info_var_raw(OutputTupleType, Context,
        TypeInfoVar, MakeTypeInfoGoals, !ModuleInfo, !PredInfo, !ProcInfo),

    % The mode will be fixed up by a later analysis.
    Mode = mode_no(0),
    Args = [TypeInfoVar, LambdaVar, ResultVar] ++ ExtraArgs,
    Features = [],
    generate_simple_call(!.ModuleInfo, mercury_exception_module, PredName,
        pf_predicate, Mode, detism_cc_multi, purity_pure, Args, Features,
        instmap_delta_bind_no_var, Context, CallGoal0),

    goal_info_init(Context, GoalInfo),

    % The try* predicates are only implemented for pure lambdas.  If the lambda
    % is actually non-pure, retain that in the call to try* with a purity
    % scope.
    (
        GoalPurity = purity_pure,
        CallGoal = CallGoal0
    ;
        ( GoalPurity = purity_semipure
        ; GoalPurity = purity_impure
        ),
        ScopeReason = promise_purity(GoalPurity),
        CallGoal = hlds_goal(scope(ScopeReason, CallGoal0), GoalInfo)
    ),

    conj_list_to_goal(MakeTypeInfoGoals ++ [CallGoal], GoalInfo, OverallGoal).

:- pred make_unreachable_call(module_info::in, hlds_goal::out) is det.

make_unreachable_call(ModuleInfo, Goal) :-
    generate_simple_call(ModuleInfo, mercury_exception_module, "unreachable",
        pf_predicate, only_mode, detism_erroneous, purity_pure,
        [], [], instmap_delta_bind_no_var, term.context_init, Goal).

:- pred make_output_tuple_inst_cast(prog_var::in, prog_var::in,
    list(mer_inst)::in, hlds_goal::out) is det.

make_output_tuple_inst_cast(TmpTupleVar, TupleVar, TupleArgInsts,
        CastOrUnify) :-
    % If all the arguments have inst `ground' then a unification is enough.
    ( if
        list.member(ArgInst, TupleArgInsts),
        ArgInst \= ground(_, none_or_default_func)
    then
        TupleArity = list.length(TupleArgInsts),
        TupleInst = bound(shared, inst_test_no_results, [
            bound_functor(tuple_cons(TupleArity), TupleArgInsts)
        ]),
        generate_cast_with_insts(unsafe_type_inst_cast, TmpTupleVar, TupleVar,
            ground_inst, TupleInst, term.context_init, CastOrUnify)
    else
        create_pure_atomic_complicated_unification(TupleVar,
            rhs_var(TmpTupleVar), term.context_init,
            umc_implicit("try_expand"), [], CastOrUnify)
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.try_expand.
%-----------------------------------------------------------------------------%
