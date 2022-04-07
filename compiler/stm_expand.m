%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public Licence - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: stm.m
% Author: lm
%
% This module contains the source to source transformations for expanding out
% atomic goals.
%
% The atomic goals are converted into a series of predicate calls and
% predicate definitions using standard calls from the library modules
% "stm_builtin", "exception" and "univ".
%
% An example transformation might be the following:
%
%   :- pred foo(int::in, int::out, io::di, io::uo) is det.
%
%   foo(X, Y, IO0, IO) :-
%       atomic [outer(IO0, IO), inner(STM0, STM)] (
%           stm_operations(X, Y, STM0, STM)
%           ...
%       )
%
% into
%
%   foo(X, Y, IO0, IO) :-
%       'StmExpanded_toplevel_0_0_0'(X, Y, IO0, IO).
%
%   :- pred 'StmExpanded_toplevel_0_0_0'(int::in, int::out, io::di, io::uo)
%           is det.
%   'StmExpanded_toplevel_0_0_0'(X, Y, IO0, IO) :-
%       'StmExpanded_rollback_0_0_0'(X, Y),
%       IO0 = IO.
%
%   :- pred 'StmExpanded_rollback_0_0_0'(int::in, int::out) is cc_multi.
%   'StmExpanded_rollback_0_0_0'(X, Y) :-
%       promise_pure (
%           impure stm_create_trasaction_log(STM0),
%           Closure = 'StmExpanded_wrapper_0_0_0'(X),
%           unsafe_try_stm(Closure(X), Result0, STM0, STM),
%           (
%               Result0 = succeeded(Y)
%           ;
%               Result0 = exception(Excp),
%               ( if Excp = univ(rollback_invalid_transaction) then
%                   impure stm_discard_transaction_log(STM),
%                   'StmExpanded_rollback_0_0_0'(X, Y)
%               else if Excp = univ(rollback_retry) then
%                   impure stm_lock,
%                   impure stm_validate(STM, IsValid),
%                   (
%                       IsValid = stm_transaction_valid,
%                       impure stm_block(STM)
%                   ;
%                       IsValid = stm_transaction_invalid,
%                       impure stm_unlock
%                   ),
%                   impure stm_discard_trasaction_log(STM),
%                   'StmExpanded_rollback_0_0_0'(X, Y)
%               else
%                   impure stm_lock,
%                   impure stm_validate(STM, IsValid),
%                   impure stm_unlock,
%                   (
%                       IsValid = stm_transaction_valid,
%                       rethrow(Result0)
%                   ;
%                       IsValid = stm_transaction_invalid,
%                       impure stm_discard_transaction_log(STM),
%                       'StmExpanded_rollback_0_0_0'(X, Y)
%                   )
%               )
%           )
%       ).
%
%   :- pred 'StmExpanded_wrapper_0_0_0'(int::in, int::out, stm::di, stm::uo)
%           is det.
%   'StmExpanded_wrapper_0_0_0'(X, Result, STM0, STM) :-
%       stm_operations(X, Y, STM0, STM)
%       ...
%       Result = Y,
%       promise_pure (
%           impure stm_lock,
%           impure stm_validate(STM, IsValid),
%           (
%               IsValid = stm_transaction_valid,
%               impure stm_commit(STM),
%               impure stm_unlock
%           ;
%               IsValid = stm_transaction_invalid,
%               impure stm_unlock,
%               throw(rollback_invalid_transaction)
%           ).
%
% Currently, the atomic goal supports a single STM transaction with any number
% of input and output arguments. As the atomic goal may need to unroll the
% call stack (when performing a retry or a rollback), the exception module
% is used. The use of the exception module impacts the passing of output
% variables and is explained below.
%
% Nonlocals instantiated before the atomic goal are passed through the
% expanded predicates as input arguments (with mode "in"). Nonlocals which
% are instantiated inside the atomic goal and are used outside the atomic goal
% (which, for the sake of simplicitly, will be called "output" variables in
% this discussion) are passed as output arguments in the "entrypoint" and
% "rollback" expanded predicates (with mode "out). In the "actual" expanded
% predicate, these variables must be passed as part of an exception result and
% are handled in the following way:
%
%   - If there are no output variables, a dummy variable is created and
%     passed up to the rollback predicate. This variable simply exists to
%     satify the requirement of the closure returning an argument and
%     will be ignored in the rollback predicate.
%   - If there is one output variable, that variable will be passed up to
%     the rollback predicate as it is.
%   - If there is more than one output variable, a tuple of these variables
%     is created and the tuple itself is passed up to the rollback predicate.
%     There, it will be deconstructed and the associated output variables
%     will be returned as output arguments.
%
% Currently a subset of the complete STM system is implemented. The following
% features will be included in subsequent review postings. A number of
% these relate to this module, whilst others relate to other modules.
%
%   - Nested atomic blocks: Whilst this will eventually be incluced, this
%     is neither supported in the front end or in this module (although some
%     passes, such as the type checker, has code for handling this).
%     However, the current method of mode checking atomic goals pervents
%     nested atomic goals (the uniqueness of the outer and inner variables
%     are handled by inserting dummy predicates at the beginning and end
%     of the atomic goal. The current implementation of these predicates
%     only allow the outer variables to be of type io).
%
%   - The "vars" parameter: The "vars" atomic goal parameter is used by the
%     programmer to list the outer variables. Whilst it is optional, the
%     variables it lists needs to be checked to ensure that they are properly
%     instantiated.
%
%   - State Variables: The "outer" and "inner" atomic goal parameters are
%     designed to take state variables along with variable pairs. Although
%     they are handled in the parser, they are not yet handled in the
%     parse tree -> HLDS transformation.
%
%   - Automatic importing of necessary modules: Currently, all necessary
%     modules must be explicitly imported by the programmer if they wish
%     to use the STM constructs.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.stm_expand.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

%-----------------------------------------------------------------------------%

:- pred stm_process_module(module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.polymorphism_type_info.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.pred_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % Information about the predicate which contains the atomic goal along
    % with other information relative to all STM expansions.
    %
:- type stm_info
    --->    stm_info(
                stm_info_module_info     :: module_info,
                stm_info_pred_id         :: pred_id,
                stm_info_proc_id         :: proc_id,
                stm_info_proc_info       :: proc_info,
                stm_info_pred_info       :: pred_info,
                stm_info_requalify       :: bool,
                stm_info_expand_id       :: int     % Number of goals expanded
            ).

    % Information about a newly created predicate. Mainly used to save
    % explicitly passing pred_info and proc_info for creation of goals.
    %
:- type stm_new_pred_info
    --->    stm_new_pred_info(
                new_pred_module_info     :: module_info,
                new_pred_pred_id         :: pred_id,
                new_pred_proc_id         :: proc_id,
                new_pred_pred_info       :: pred_info,
                new_pred_proc_info       :: proc_info,
                new_pred_context         :: term.context,
                new_pred_var_cnt         :: int
            ).

    % Information about the local and non-local variables of an atomic goal.
    %
:- type stm_goal_vars
    --->    stm_goal_vars(
                vars_input               :: set_of_progvar,
                vars_local               :: set_of_progvar,
                vars_output              :: set_of_progvar,
                vars_innerDI             :: prog_var,       % inner STM di var
                vars_innerUO             :: prog_var        % inner STM uo var
            ).

%-----------------------------------------------------------------------------%

stm_process_module(!ModuleInfo) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    list.foldl(stm_process_pred, PredIds, !ModuleInfo),
    module_info_clobber_dependency_info(!ModuleInfo).

:- pred stm_process_pred(pred_id::in, module_info::in, module_info::out)
    is det.

stm_process_pred(PredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_procids(PredInfo),
    list.foldl(stm_process_proc(PredId), ProcIds, !ModuleInfo).

:- pred stm_process_proc(pred_id::in, proc_id::in,
    module_info::in, module_info::out) is det.

stm_process_proc(PredId, ProcId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),

    stm_process_proc_2(ProcInfo0, ProcInfo, PredId, ProcId, PredInfo0,
        PredInfo1, !ModuleInfo),

    pred_info_get_proc_table(PredInfo1, ProcTable1),
    map.det_update(ProcId, ProcInfo, ProcTable1, ProcTable),
    pred_info_set_proc_table(ProcTable, PredInfo1, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- pred stm_process_proc_2(proc_info::in, proc_info::out, pred_id::in,
    proc_id::in, pred_info::in, pred_info::out,
    module_info::in, module_info::out) is det.

stm_process_proc_2(!ProcInfo, PredId, ProcId, !PredInfo, !ModuleInfo) :-
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InitInstmap),
    StmInfo0 = stm_info(!.ModuleInfo, PredId, ProcId, !.ProcInfo,
        !.PredInfo, no, 0),
    stm_process_goal(InitInstmap, Goal0, Goal, StmInfo0, StmInfo),
    StmInfo = stm_info(!:ModuleInfo, _, _, !:ProcInfo, !:PredInfo,
        RecalcInfo, _),
    proc_info_set_goal(Goal, !ProcInfo),

    (
        RecalcInfo = yes,
        requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo),
        recompute_instmap_delta_proc(recompute_atomic_instmap_deltas,
            !ProcInfo, !ModuleInfo)
    ;
        RecalcInfo = no
    ).

:- pred stm_process_goal(instmap::in, hlds_goal::in, hlds_goal::out,
    stm_info::in, stm_info::out) is det.

stm_process_goal(Instmap, Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = unify(_, _, _, _, _),
        Goal = Goal0
    ;
        GoalExpr0 = conj(ConjType, Conjuncts0),
        stm_process_conj(Instmap, Conjuncts0, Conjuncts, !Info),
        GoalExpr = conj(ConjType, Conjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Disjuncts0),
        stm_process_disj(Instmap, Disjuncts0, Disjuncts, !Info),
        GoalExpr = disj(Disjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        stm_process_goal(Instmap, SubGoal0, SubGoal, !Info),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        stm_process_switch_cases(Instmap, Cases0, Cases, !Info),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, InnerGoal0),
        (
            Reason = from_ground_term(_, FGT),
            (
                ( FGT = from_ground_term_construct
                ; FGT = from_ground_term_deconstruct
                ),
                % There can be no atomic goals inside this scope.
                Goal = Goal0
            ;
                FGT = from_ground_term_other,
                stm_process_goal(Instmap, InnerGoal0, InnerGoal, !Info),
                GoalExpr = scope(Reason, InnerGoal),
                Goal = hlds_goal(GoalExpr, GoalInfo0)
            ;
                FGT = from_ground_term_initial,
                % These scopes should have been deleted by now.
                unexpected($pred, "unexpected scope")
            )
        ;
            ( Reason = disable_warnings(_, _)
            ; Reason = exist_quant(_)
            ; Reason = promise_solutions(_, _)
            ; Reason = promise_purity(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            ; Reason = loop_control(_, _, _)
            ),
            stm_process_goal(Instmap, InnerGoal0, InnerGoal, !Info),
            GoalExpr = scope(Reason, InnerGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            ( Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ),
            % These scopes should have been deleted by now.
            unexpected($pred, "unexpected scope")
        )
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        stm_process_if_then_else(Instmap, Cond0, Then0, Else0,
            Cond, Then, Else, !Info),
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
            % XXX STM: Why do we ignore _MaybeOutputVars?
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, _MaybeOutputVars,
                MainGoal0, OrElseGoals0, _OrElseInners),

            GoalDisj0 = [MainGoal0 | OrElseGoals0],
            stm_process_disj(Instmap, GoalDisj0, GoalDisj, !Info),
            MainGoal = list.det_head(GoalDisj),
            OrElseGoals = list.det_tail(GoalDisj),

            InstmapDelta = goal_info_get_instmap_delta(GoalInfo0),
            apply_instmap_delta(InstmapDelta, Instmap, FinalInstmap),

            % Traverse the goal and if an inside goal is encountered:
            %   1. If goal is single, connect the outers and inners
            %   2. Process or_else as if it would be called directly in goal

            Context = goal_info_get_context(GoalInfo0),
            stm_create_actual_goal(Context, GoalType, Instmap, FinalInstmap,
                Outer, Inner, MainGoal, OrElseGoals, Goal, !Info)
        ;
            ShortHand0 = try_goal(_, _, _),
            unexpected($pred, "try_goal")
        ;
            ShortHand0 = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        )
    ).

:- pred stm_process_conj(instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    stm_info::in, stm_info::out) is det.

stm_process_conj(Instmap0, GoalList0, GoalList, !Info) :-
    (
        GoalList0 = [],
        GoalList = []
    ;
        GoalList0 = [Goal0 | Goals0],
        InstmapDelta = goal_info_get_instmap_delta(GoalInfo),

        stm_process_goal(Instmap0, Goal0, Goal, !Info),

        Goal0 = hlds_goal(_, GoalInfo),
        apply_instmap_delta(InstmapDelta, Instmap0, Instmap),
        stm_process_conj(Instmap, Goals0, Goals, !Info),
        GoalList = [Goal | Goals]
    ).

:- pred stm_process_disj(instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    stm_info::in, stm_info::out) is det.

stm_process_disj(Instmap, GoalList0, GoalList, !Info) :-
    (
        GoalList0 = [],
        GoalList = []
    ;
        GoalList0 = [Goal0 | Goals0],
        stm_process_goal(Instmap, Goal0, Goal, !Info),
        stm_process_disj(Instmap, Goals0, Goals, !Info),
        GoalList = [Goal | Goals]
    ).

:- pred stm_process_if_then_else(instmap::in, hlds_goal::in, hlds_goal::in,
    hlds_goal::in, hlds_goal::out, hlds_goal::out, hlds_goal::out,
    stm_info::in, stm_info::out) is det.

stm_process_if_then_else(Instmap0, Cond0, Then0, Else0, Cond, Then, Else,
        !Info) :-
    stm_process_goal(Instmap0, Cond0, Cond, !Info),

    % XXX: It is currently assumed that the initial instmap of the Then part
    % is the same as the final instmap of the condition part whilst the
    % initial instmap of the else part is the same as the initial instmap
    % of the entire if_then_else goal. I'm not sure if this is correct
    % or not.

    Cond0 = hlds_goal(_, CondInfo),
    CondInstmapDelta = goal_info_get_instmap_delta(CondInfo),
    apply_instmap_delta(CondInstmapDelta, Instmap0, InstmapAfterCond),
    stm_process_goal(InstmapAfterCond, Then0, Then, !Info),
    stm_process_goal(Instmap0, Else0, Else, !Info).

:- pred stm_process_switch_cases(instmap::in, list(case)::in, list(case)::out,
    stm_info::in, stm_info::out) is det.

stm_process_switch_cases(_Instmap0, [], [], !Info).
stm_process_switch_cases(Instmap0, [Case0 | Cases0], [Case | Cases], !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    stm_process_goal(Instmap0, Goal0, Goal, !Info),
    stm_process_switch_cases(Instmap0, Cases0, Cases, !Info),
    Case = case(MainConsId, OtherConsIds, Goal).

%-----------------------------------------------------------------------------%
%
% Predicate related to the creation of the top level goal.

    % Creates all the required predicates and returns the call to the
    % newly created top_level goal. The InitInstmap and FinalInstmap is the
    % instmap before and after the atomic goal respectivly.
    %
:- pred stm_create_actual_goal(prog_context::in,
    atomic_goal_type::in, instmap::in, instmap::in,
    atomic_interface_vars::in, atomic_interface_vars::in, hlds_goal::in,
    list(hlds_goal)::in, hlds_goal::out, stm_info::in, stm_info::out) is det.

stm_create_actual_goal(Context, GoalType, InitInstmap, FinalInstmap,
        Outer, Inner, MainGoal, OrElseGoals, FinalGoal, !StmInfo) :-
    Outer = atomic_interface_vars(OuterDI, OuterUO),
    Inner = atomic_interface_vars(InnerDI, InnerUO),
    (
        GoalType = top_level_atomic_goal,
        create_top_level_goal(InitInstmap, FinalInstmap,
            OuterDI, OuterUO, InnerDI, InnerUO, MainGoal, OrElseGoals,
            FinalGoal, !StmInfo)
    ;
        GoalType = nested_atomic_goal,
        trace [compiletime(flag("debug_stm")), io(!IO)] (
            io.write_string("Creating nested atomic goal\n",!IO)
        ),
        create_nested_goal(Context, InitInstmap, FinalInstmap,
            OuterDI, OuterUO, InnerDI, InnerUO, MainGoal, OrElseGoals,
            FinalGoal, !StmInfo)
    ;
        GoalType = unknown_atomic_goal_type,
        unexpected($pred, "unknown atomic goal type")
    ),
    !StmInfo ^ stm_info_requalify := yes.

%-----------------------------------------------------------------------------%
%
% Predicates to determine if variables are inputs, outputs or local to a goal.
% This decision is currenly governed by the following rules:
%
%   1. If it is free in the initial instmap and not free in the final instmap,
%      the variable is an output.
%   2. If it is not free in the initial instmap and not free in the final
%      instmap, the

    % Arranges variables into groups of local variables, input variables and
    % output variables. This uses the instmap before and after the atomic
    % goal.
    %
:- pred order_vars_into_groups(module_info::in, list(prog_var)::in,
    instmap::in, instmap::in, list(prog_var)::out, list(prog_var)::out,
    list(prog_var)::out) is det.

order_vars_into_groups(ModuleInfo, Vars, InitInstmap, FinalInstmap, Local,
        Input, Output) :-
    order_vars_into_groups_2(ModuleInfo, Vars, InitInstmap, FinalInstmap,
        [], Local, [], Input, [], Output).

:- pred order_vars_into_groups_2(module_info::in, list(prog_var)::in,
    instmap::in, instmap::in, list(prog_var)::in, list(prog_var)::out,
    list(prog_var)::in, list(prog_var)::out, list(prog_var)::in,
    list(prog_var)::out) is det.

order_vars_into_groups_2(_, [], _, _, !Local, !Input, !Output).
order_vars_into_groups_2(ModuleInfo, [Var|Vars], InitInstmap, FinalInstmap,
        !LocalVars, !InputVars, !OutputVars) :-
    instmap_lookup_var(InitInstmap, Var, InitVarInst),
    instmap_lookup_var(FinalInstmap, Var, FinalVarInst),
    ( if
        inst_is_free(ModuleInfo, InitVarInst),
        inst_is_free(ModuleInfo, FinalVarInst)
    then
        !:LocalVars = [Var | !.LocalVars]
    else if
        inst_is_free(ModuleInfo, InitVarInst),
        inst_is_bound(ModuleInfo, FinalVarInst)
    then
        !:OutputVars = [Var | !.OutputVars]
    else if
        inst_is_bound(ModuleInfo, InitVarInst),
        inst_is_bound(ModuleInfo, FinalVarInst)
    then
        !:InputVars = [Var | !.InputVars]
    else
        unexpected($pred, "unhandled inst case")
    ),
    order_vars_into_groups_2(ModuleInfo, Vars, InitInstmap, FinalInstmap,
        !LocalVars, !InputVars, !OutputVars).

    % Return the var sets for the first atomic goal in the list, taking the
    % union of the input var sets of all the goals. If the first atomic goal
    % does not succeed, we will try the later goals, so inputs to the later
    % goals must also be inputs of the first goal.
    %
    % XXX This probably could done directly in calc_pred_variables_list.
    %
:- pred common_goal_vars_from_list(list(stm_goal_vars)::in, stm_goal_vars::out)
    is det.

common_goal_vars_from_list(GoalList, GoalVar) :-
    ExtractInputSet =
        ( pred(AGV::in, Input::out) is det :-
            Input = AGV ^ vars_input
        ),
    list.map(ExtractInputSet, GoalList, InputVarList),
    InputVars = set_of_var.union_list(InputVarList),
    GoalVar0 = list.det_head(GoalList),
    GoalVar = GoalVar0 ^ vars_input := InputVars.

:- pred copy_input_vars_in_goallist(stm_goal_vars::in,
    list(stm_goal_vars)::in, list(stm_goal_vars)::out) is det.
:- pragma consider_used(pred(copy_input_vars_in_goallist/3)).

copy_input_vars_in_goallist(GoalVar, !GoalList) :-
    CopyInputVarLambda =
        ( pred(AGV0::in, AGV::out) is det :-
            AGV = AGV0 ^ vars_input := (GoalVar ^ vars_input)
        ),
    list.map(CopyInputVarLambda, !GoalList).

:- pred calc_pred_variables_list(instmap::in, instmap::in,
    list(hlds_goal)::in, list(prog_var)::in,
    list(prog_var)::in, list(prog_var)::in, list(stm_goal_vars)::out,
    stm_info::in, stm_info::out) is det.

calc_pred_variables_list(InitInstmap, FinalInstmap, Goals, InnerDIs, InnerUOs,
        IgnoreVarList0, StmGoalVarList, !StmInfo) :-
    ( if
        Goals = [],
        InnerDIs = [],
        InnerUOs = []
    then
        StmGoalVarList = []
    else if
        Goals = [HeadGoal | TailGoals],
        InnerDIs = [HeadInnerDI | TailInnerDIs],
        InnerUOs = [HeadInnerUO | TailInnerUOs]
    then
        IgnoreVarList = [HeadInnerDI, HeadInnerUO | IgnoreVarList0],

        calc_pred_variables(InitInstmap, FinalInstmap, HeadGoal, HeadInnerDI,
            HeadInnerUO, IgnoreVarList, StmGoalVar, !StmInfo),
        calc_pred_variables_list(InitInstmap, FinalInstmap, TailGoals,
            TailInnerDIs, TailInnerUOs, IgnoreVarList, StmGoalVarList0,
            !StmInfo),
        StmGoalVarList = [StmGoalVar | StmGoalVarList0]
    else
        unexpected($pred, "lengths mismatch")
    ).

    % Arranges all variables from the goal and non-locals into local variables,
    % input variables and output variables. All variables that appear in the
    % list of IgnoreVarList are not included.
    %
:- pred calc_pred_variables(instmap::in, instmap::in,
    hlds_goal::in, prog_var::in, prog_var::in, list(prog_var)::in,
    stm_goal_vars::out, stm_info::in, stm_info::out) is det.

calc_pred_variables(InitInstmap, FinalInstmap, Goal,
        InnerDI, InnerUO, IgnoreVarList, StmGoalVars, !StmInfo) :-
    ModuleInfo = !.StmInfo ^ stm_info_module_info,

    goal_vars(Goal, GoalVars0),
    set_of_var.delete_list(IgnoreVarList, GoalVars0, GoalVars),
    GoalVarList = set_of_var.to_sorted_list(GoalVars),

    Goal = hlds_goal(_, GoalInfo),
    GoalNonLocalSet0 = goal_info_get_nonlocals(GoalInfo),
    set_of_var.delete_list(IgnoreVarList, GoalNonLocalSet0, GoalNonLocalSet),
    GoalNonLocals = set_of_var.to_sorted_list(GoalNonLocalSet),

    order_vars_into_groups(ModuleInfo, GoalVarList, InitInstmap, FinalInstmap,
        LocalVarsList, InputVarsList, _),
    order_vars_into_groups(ModuleInfo, GoalNonLocals, InitInstmap,
        FinalInstmap, _, _InputVarsList, OutputVarsList),

    LocalVars = set_of_var.list_to_set(LocalVarsList),
    InputVars = set_of_var.list_to_set(InputVarsList),
    OutputVars = set_of_var.list_to_set(OutputVarsList),

    StmGoalVars = stm_goal_vars(InputVars, LocalVars, OutputVars,
        InnerDI, InnerUO).

%-----------------------------------------------------------------------------%
%
% Predicates involved in the removal of the dummy predicates
% "stm_from_inner_to_outer" and "stm_from_outer_to_inner".
%

    % Removes all calls to the dummy predicates in a list of goals.
    %
:- pred remove_tail(list(hlds_goal)::in, list(hlds_goal)::out,
    pair(maybe(prog_var), maybe(prog_var))::out,
	pair(maybe(prog_var), maybe(prog_var))::out) is det.

remove_tail([], [], no - no, no - no).
remove_tail([HeadGoal0 | TailGoals0], Goals,
        MaybeOutDI - MaybeOutUO, MaybeInDI - MaybeInUO) :-
    remove_tail(TailGoals0, TailGoals, MaybeOutDI0 - MaybeOutUO0,
        MaybeInDI0 - MaybeInUO0),
    HeadGoal0 = hlds_goal(HeadGoalExpr0, _),
    ( if
        HeadGoalExpr0 = plain_call(_, _, [_, X, V], _, _, stm_outer_inner)
    then
        MaybeInDI = yes(V),
        MaybeInUO = MaybeInUO0,
		MaybeOutDI = yes(X),
		MaybeOutUO = MaybeOutUO0,
        Goals = TailGoals
    else if
        HeadGoalExpr0 = plain_call(_, _, [_, V, X], _, _, stm_inner_outer)
    then
        MaybeInDI = MaybeInDI0,
        MaybeInUO = yes(V),
		MaybeOutDI = MaybeOutDI0,
		MaybeOutUO = yes(X),
        Goals = TailGoals
    else
        Goals = [HeadGoal0 | TailGoals],
        MaybeInDI = MaybeInDI0,
        MaybeInUO = MaybeInUO0,
        MaybeOutDI = MaybeOutDI0,
        MaybeOutUO = MaybeOutUO0
    ).

    % Strip the dummy predicates. At the very minimum, these predicates
    % should be in the atomic goal so the atomic goal must be a conjunction.
    %
:- pred strip_goal_calls(hlds_goal::in, hlds_goal::out,
    prog_var::out, prog_var::out, prog_var::out, prog_var::out) is det.

strip_goal_calls(Goal0, Goal, StmOutDI, StmOutUO, StmInDI, StmInUO) :-
    ( if Goal0 = hlds_goal(conj(plain_conj, GoalList0), GoalInfo) then
        (
            GoalList0 = [],
            unexpected($pred, "empty conjunction")
        ;
            GoalList0 = [_ | _],
            remove_tail(GoalList0, GoalList, MaybeOutVarPair, MaybeInVarPair),
            MaybeInDI = fst(MaybeInVarPair),
            MaybeInUO = snd(MaybeInVarPair),
            MaybeOutDI = fst(MaybeOutVarPair),
            MaybeOutUO = snd(MaybeOutVarPair),
            ( if
                MaybeInDI = yes(StmInDI0),
                MaybeInUO = yes(StmInUO0),
                MaybeOutDI = yes(StmOutDI0),
                MaybeOutUO = yes(StmOutUO0)
            then
                StmInDI = StmInDI0,
                StmInUO = StmInUO0,
                StmOutDI = StmOutDI0,
                StmOutUO = StmOutUO0,
                Goal = hlds_goal(conj(plain_conj, GoalList), GoalInfo)
            else
                unexpected($pred, "Vars not extracted")
            )
        )
    else
        unexpected($pred, "atomic_goal not a conj")
    ).

%-----------------------------------------------------------------------------%
%
% Predicates related to the creation of the top level predicate.
% The created predicate calls the rollback predicate and threads the IO state.
% Creating the top-level predicate implicitly creates the rollback predicate
% and wrapper predicates.
%

    % Creates a nested atomic goal.
    %
:- pred create_nested_goal(prog_context::in, instmap::in, instmap::in,
    prog_var::in, prog_var::in, prog_var::in, prog_var::in,
    hlds_goal::in, list(hlds_goal)::in, hlds_goal::out,
    stm_info::in, stm_info::out) is det.

create_nested_goal(Context, InitInstmap, FinalInstmap, OuterDI, OuterUO,
        _InnerDI, _InnerUO, AtomicGoal0, OrElseGoals0, Goal, !StmInfo) :-
    strip_goal_calls(AtomicGoal0, AtomicGoal, MainOuterDI, MainOuterUO,
        MainInnerDI, MainInnerUO),
    list.map5(strip_goal_calls, OrElseGoals0, OrElseGoals, _, _,
        OrElseInnerDIs, OrElseInnerUOs),
    UnifyModeUoDi = unify_modes_li_lf_ri_rf(free, unique_inst,
        unique_inst, clobbered_inst),
    (
        OrElseGoals = [],

        % If no or_else goals, simply connect up the outer and inner variables.
        create_var_unify_stm(MainInnerDI, MainOuterDI, UnifyModeUoDi,
            CopyDIVars, !StmInfo),
        create_var_unify_stm(MainOuterUO, MainInnerUO, UnifyModeUoDi,
            CopyUOVars, !StmInfo),
        create_plain_conj([CopyDIVars, AtomicGoal, CopyUOVars], Goal)
    ;
        OrElseGoals = [_ | _],

        % Creates a call to an or_else branch predicate.
        calc_pred_variables_list(InitInstmap, FinalInstmap,
            [AtomicGoal0 | OrElseGoals0], [MainInnerDI | OrElseInnerDIs],
            [MainInnerUO | OrElseInnerUOs], [OuterDI, OuterUO],
            AtomicGoalVarList, !StmInfo),
        GoalList = [AtomicGoal | OrElseGoals],

        common_goal_vars_from_list(AtomicGoalVarList, AtomicGoalVars),
%       copy_input_vars_in_goallist(AtomicGoalVars, AtomicGoalVarList,
%           AtomicGoalVarList1),
        AtomicGoalVarList1 = AtomicGoalVarList,

        trace [compiletime(flag("debug_stm")), io(!IO)] (
            io.write_string("Local: " ++
                string(AtomicGoalVars ^ vars_local) ++ "\n", !IO),
            io.write_string("Inner: " ++
                string(AtomicGoalVars ^ vars_input) ++ "\n", !IO),
            io.write_string("Outer: " ++
                string(AtomicGoalVars ^ vars_output) ++ "\n", !IO)
        ),

        get_input_output_types(AtomicGoalVars, !.StmInfo, _, OutputTypes),
        make_return_type(OutputTypes, ResultType),
        create_aux_variable_stm(ResultType, yes("res"), ResultVar, !StmInfo),
        CreateWrapperForEachGoal =
            ( pred(ThisGoal::in, GoalVars::in, PPID::out, SInfo0::in,
                    SInfo::out) is det :-
                % These predicates should be plain predicates without code to
                % validate logs.
                create_simple_wrapper_pred(Context, GoalVars,
                    ResultType, ResultVar, ThisGoal, PPID, _, SInfo0, SInfo)
            ),
        map2_in_foldl(CreateWrapperForEachGoal, GoalList, AtomicGoalVarList1,
            PPIDList, !StmInfo),

        create_or_else_pred(Context, AtomicGoalVars, AtomicGoalVarList1,
            PPIDList, MainInnerDI, MainInnerUO, OrElseCall, !StmInfo),
        create_var_unify_stm(MainInnerDI, MainOuterDI, UnifyModeUoDi,
            CopyDIVars, !StmInfo),
        create_var_unify_stm(MainOuterUO, MainInnerUO, UnifyModeUoDi,
            CopyUOVars, !StmInfo),
        create_plain_conj([CopyDIVars, OrElseCall, CopyUOVars], Goal)
    ).

    % Creates the top level predicate and returns a call to that predicate.
    %
:- pred create_top_level_goal(instmap::in, instmap::in,
    prog_var::in, prog_var::in, prog_var::in, prog_var::in,
    hlds_goal::in, list(hlds_goal)::in, hlds_goal::out,
    stm_info::in, stm_info::out) is det.

create_top_level_goal(InitInstmap, FinalInstmap, OuterDI, OuterUO,
        _InnerDI, _InnerUO, AtomicGoal0, OrElseGoals0, Goal, !StmInfo) :-
    strip_goal_calls(AtomicGoal0, AtomicGoal, _, _, MainInnerDI, MainInnerUO),
    list.map5(strip_goal_calls, OrElseGoals0, OrElseGoals, _, _,
        OrElseInnerDIs, OrElseInnerUOs),

    % The input and output variables of the atomic goal and or_else goals
    % should be the same as or_elses are treated as disjunctions.

    calc_pred_variables_list(InitInstmap, FinalInstmap,
        [AtomicGoal0 | OrElseGoals0], [MainInnerDI | OrElseInnerDIs],
        [MainInnerUO | OrElseInnerUOs], [OuterDI, OuterUO],
        AtomicGoalVarList, !StmInfo),

    AtomicGoal = hlds_goal(_, AtomicGoalInfo),
    Context = goal_info_get_context(AtomicGoalInfo),
    create_top_level_pred(Context, AtomicGoalVarList, OuterDI, OuterUO,
        AtomicGoal, OrElseGoals, Goal, !StmInfo).

    % Creates the top level predicate. Calling this implicitly creates the
    % rollback and wrapper predicate.
    %
:- pred create_top_level_pred(prog_context::in, list(stm_goal_vars)::in,
    prog_var::in, prog_var::in, hlds_goal::in, list(hlds_goal)::in,
    hlds_goal::out, stm_info::in, stm_info::out) is det.

create_top_level_pred(Context, AtomicGoalVarList, OuterDI, OuterUO, AtomicGoal,
        OrElseGoals, Goal, !StmInfo) :-
    create_rollback_pred(Context, AtomicGoalVarList, WrapperCall, AtomicGoal,
        OrElseGoals, !StmInfo),

    common_goal_vars_from_list(AtomicGoalVarList, AtomicGoalVars),
    get_input_output_varlist(AtomicGoalVars, InputVars, OutputVars),
    get_input_output_types(AtomicGoalVars, !.StmInfo, InputTypes, OutputTypes),
    get_input_output_modes(AtomicGoalVars, InputModes, OutputModes),

    create_cloned_pred(InputVars ++ OutputVars ++ [OuterDI, OuterUO],
        InputTypes ++ OutputTypes ++ [io_io_type, io_io_type],
        InputModes ++ OutputModes ++ [di_mode, uo_mode], stmck_top_level,
        AtomicGoal, no, NewPredInfo0, Goal, !StmInfo),

    UnifyModeUoDi = unify_modes_li_lf_ri_rf(free, unique_inst,
        unique_inst, clobbered_inst),
    create_var_unify(OuterUO, OuterDI, UnifyModeUoDi,
        CopyIOAssign, NewPredInfo0, NewPredInfo1),
    create_plain_conj([WrapperCall, CopyIOAssign], TopLevelGoal),

    new_pred_set_goal(TopLevelGoal, NewPredInfo1, NewPredInfo2),
    run_quantification_over_pred(NewPredInfo2, NewPredInfo),
    commit_new_pred(NewPredInfo, !StmInfo).

%-----------------------------------------------------------------------------%
%
% Template predicates. These predicates are used to create frequently
% occurring patterns in the predicate clause.
%

    % Predicate that creates the following goal:
    %
    %       ( if
    %           X <- univ.univ(<<ExceptRes>>),
    %           X == << stm_rollback_exception_functor >>
    %       then
    %           << true_goal >>
    %       else
    %           << false_goal >>
    %       )
    %
    % The RttiVar variable must contain ...
    %
:- pred template_if_exceptres_is_cons(prog_context::in,
    prog_var::in, prog_var::in, cons_id::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::out,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

template_if_exceptres_is_cons(Context, TypeInfoVar, ExceptVar,
        RollbackExceptCons, TrueGoal, FalseGoal, Goal, !NewPredInfo) :-
    create_aux_variable(stm_rollback_exception_type, yes("UnivPayload"),
        UnivPayloadVar, !NewPredInfo),
    create_aux_variable_assignment(Context, RollbackExceptCons,
        stm_rollback_exception_type, yes("RollbackExcpt"), AssignGoal,
        RollbackExceptVar, !NewPredInfo),
    create_simple_call(mercury_univ_module, "type_to_univ", pf_predicate,
        [TypeInfoVar], [UnivPayloadVar, ExceptVar],
        instmap_delta_from_assoc_list(
            [TypeInfoVar - ground(shared, none_or_default_func),
            ExceptVar - ground(shared, none_or_default_func),
            UnivPayloadVar - free]),
        mode_no(2), detism_semi, purity_pure, [], UnivCall, !NewPredInfo),
    create_simple_call(mercury_public_builtin_module, "unify", pf_predicate,
        [TypeInfoVar], [RollbackExceptVar, UnivPayloadVar],
        instmap_delta_bind_no_var, only_mode, detism_semi, purity_pure,
        [], _UnifyCall, !NewPredInfo),
    create_var_test(UnivPayloadVar, RollbackExceptVar,
        unify_modes_li_lf_ri_rf(ground_inst, ground_inst,
            ground_inst, ground_inst),
        TestGoal, !NewPredInfo),
%   XXX STM
%   create_plain_conj([AssignGoal, UnivCall, TestGoal, UnifyCall], CondGoal),
    create_plain_conj([AssignGoal, UnivCall, TestGoal], CondGoal),

    ITEDetermism = detism_det,
    ITEPurity = purity_impure,

    create_if_then_else([], CondGoal, TrueGoal, FalseGoal, ITEDetermism,
        ITEPurity, Goal, !NewPredInfo).

    % Predicate that creates the following goals.
    %
    %       impure stm_builtin.lock,
    %       impure stm_builtin.validate(<<STM>>, IsValid),
    %       { impure stm_builtin.unlock } when unlock_after == yes
    %       (
    %           IsValid = stm_transaction_valid,
    %           << TrueGoal >>
    %       ;
    %           IsValid = stm_transaction_invalid,
    %           << FalseGoal >>
    %       )
    %
    % The call to "stm_builtin.unlock" is only included if the value of
    % UnlockAfterwards is yes.
    %
:- pred template_lock_and_validate(prog_var::in, bool::in, hlds_goal::in,
    hlds_goal::in, list(hlds_goal)::out,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

template_lock_and_validate(StmVar, UnlockAfterwards, ValidGoal, InvalidGoal,
        Goals, !NewPredInfo) :-
    create_aux_variable(stm_valid_result_type, yes("ValidResult"),
        IsValidVar, !NewPredInfo),
    create_simple_call(mercury_stm_builtin_module, "stm_lock", pf_predicate,
        [], [], instmap_delta_bind_no_var, only_mode,
        detism_det, purity_impure, [], LockCall, !NewPredInfo),
    create_simple_call(mercury_stm_builtin_module, "stm_validate",
        pf_predicate, [], [StmVar, IsValidVar],
        instmap_delta_from_assoc_list(
            [StmVar - ground(unique, none_or_default_func),
            IsValidVar - free]),
        only_mode, detism_det, purity_impure, [], ValidCall, !NewPredInfo),
    create_switch_disjunction(IsValidVar,
        [case(stm_validres_valid_functor, [], ValidGoal),
         case(stm_validres_invalid_functor, [], InvalidGoal)], detism_det,
         purity_impure,  DisjGoal, !NewPredInfo),
    (
        UnlockAfterwards = yes,
        create_simple_call(mercury_stm_builtin_module, "stm_unlock",
            pf_predicate, [], [], instmap_delta_bind_no_var, only_mode,
            detism_det, purity_impure, [], UnlockCall, !NewPredInfo),
        Goals = [LockCall, ValidCall, UnlockCall, DisjGoal]
    ;
        UnlockAfterwards = no,
        Goals = [LockCall, ValidCall, DisjGoal]
    ).

    % Lock and validate a number of transactions. The success branch will
    % be passed if all transactions are valid.
    %
:- pred template_lock_and_validate_many(prog_context::in,
    list(prog_var)::in, bool::in,
    hlds_goal::in, hlds_goal::in, list(hlds_goal)::out,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

template_lock_and_validate_many(Context, StmVars, UnlockAfterwards, ValidGoal,
        InvalidGoal, Goals, !NewPredInfo) :-
    create_aux_variable_assignment(Context, stm_validres_valid_functor,
        stm_valid_result_type, yes("IsValidConst"), AssignValidConst,
        IsValidConstVar, !NewPredInfo),

    create_simple_call(mercury_stm_builtin_module, "stm_lock", pf_predicate,
        [], [], instmap_delta_bind_no_var, only_mode,
        detism_det, purity_impure, [], LockCall, !NewPredInfo),

    % Create N value result variables. Variables are returned as a list

    CreateValidate =
        ( pred(StmVarL::in, ValidGoalL::out, ValidResL::out,
                NPI0::in, NPI::out) is det :-
            create_aux_variable(stm_valid_result_type, yes("ValidResult"),
                ValidResL, NPI0, NPI1),
            create_simple_call(mercury_stm_builtin_module, "stm_validate",
                pf_predicate, [], [StmVarL, ValidResL],
                instmap_delta_from_assoc_list(
                    [StmVarL - ground(unique, none_or_default_func),
                    ValidResL - free]),
                only_mode, detism_det, purity_impure, [], ValidGoalL,
                NPI1, NPI)
        ),

    list.map2_foldl(CreateValidate, StmVars, ValidCalls, IsValidVars,
        !NewPredInfo),

    CreateValidTests =
        ( pred(ValidRes::in, ValidTest::out, NPI0::in, NPI::out) is det :-
            create_var_test(ValidRes, IsValidConstVar,
                unify_modes_li_lf_ri_rf(ground_inst, ground_inst,
                    ground_inst, ground_inst),
                ValidTest, NPI0, NPI)
        ),

    list.map_foldl(CreateValidTests, IsValidVars, TestValidGoals,
        !NewPredInfo),
    create_plain_conj(TestValidGoals, TestValidCond),

    create_if_then_else([], TestValidCond, ValidGoal, InvalidGoal,
        detism_cc_multi, purity_impure, ITEGoal, !NewPredInfo),

    (
        UnlockAfterwards = yes,
        create_simple_call(mercury_stm_builtin_module, "stm_unlock",
            pf_predicate, [], [], instmap_delta_bind_no_var, only_mode,
            detism_det, purity_impure, [], UnlockCall, !NewPredInfo),
        Goals = [AssignValidConst, LockCall | ValidCalls] ++
            [UnlockCall, ITEGoal]
    ;
        UnlockAfterwards = no,
        Goals = [AssignValidConst, LockCall | ValidCalls] ++ [ITEGoal]
    ).

%-----------------------------------------------------------------------------%
%
% Predicates involved in the creation of the rollback predicate. The rollback
% predicate is responsible for calling the wrapper predicate and handling
% the excepion result. If the exception result indicates a rollback because
% of an invalid transaction or a retry, this predicate is responsible for
% handling these. For an example of the goals created by this predicate,
% please see the comment in the top of this file.
%

    % Creates the necessary goals for handling exceptions that do not indicate
    % a rollback. The role of the these goals is to validate the transaction
    % log and act upon the result. The goals created are listed below:
    %
    %   impure stm_builtin.stm_lock,
    %   impure stm_builtin.validate(STM, IsValid),
    %   impure stm_builtin.stm_unlock,
    %   (
    %       IsValid = stm_transaction_valid,
    %       rethrow(Exception)
    %   ;
    %       IsValid = stm_transaction_invalid,
    %       impure stm_discard_transaction_log(STM),
    %       'StmExpanded_rollback_0_0_0'(X, Y)
    %   )
    %
:- pred create_validate_exception_goal(prog_var::in, prog_var::in,
    mer_type::in, hlds_goal::in, hlds_goal::out,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_validate_exception_goal(StmVar, ExceptionVar, ReturnType, RecursiveCall,
        Goal, !NewPredInfo) :-
    make_type_info(ReturnType, ReturnTypeInfoVar, ReturnTypeInfoVarAssign,
        !NewPredInfo),
    create_simple_call(mercury_exception_module, "rethrow", pf_predicate,
        [ReturnTypeInfoVar], [ExceptionVar],
        instmap_delta_bind_vars([ReturnTypeInfoVar, ExceptionVar]),
        only_mode, detism_erroneous, purity_pure, [],
        Goal_ExceptionThrow_Call, !NewPredInfo),
    create_plain_conj(ReturnTypeInfoVarAssign ++ [Goal_ExceptionThrow_Call],
        Goal_ValidBranch),
    create_simple_call(mercury_stm_builtin_module,
        "stm_discard_transaction_log", pf_predicate,
        [], [StmVar],
        instmap_delta_from_assoc_list(
            [StmVar - ground(clobbered, none_or_default_func)]),
        only_mode, detism_det, purity_impure, [], DropStateCall, !NewPredInfo),
    create_plain_conj([DropStateCall, RecursiveCall], Goal_InvalidBranch),
    template_lock_and_validate(StmVar, yes, Goal_ValidBranch,
        Goal_InvalidBranch, Goals, !NewPredInfo),
    create_plain_conj(Goals, Goal).

    % Creates the necessary goals for handling explicit retries. The role
    % of these goals is to validate the log and block the thread if the
    % log is valid (provided that transaction variables to wait on exist
    % in the log).
    %
:- pred create_retry_handler_branch(prog_var::in, hlds_goal::in,
    hlds_goal::out, stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_retry_handler_branch(StmVar, RecCall, Goal, !NewPredInfo) :-
    create_simple_call(mercury_stm_builtin_module, "stm_block", pf_predicate,
        [], [StmVar], instmap_delta_bind_var(StmVar), only_mode,
        detism_det, purity_impure, [], BlockGoal, !NewPredInfo),
    create_simple_call(mercury_stm_builtin_module, "stm_unlock", pf_predicate,
        [], [], instmap_delta_bind_no_var, only_mode,
        detism_det, purity_impure, [], UnlockGoal, !NewPredInfo),
    template_lock_and_validate(StmVar, no, BlockGoal, UnlockGoal,
        LockAndValidateGoals, !NewPredInfo),
    create_simple_call(mercury_stm_builtin_module,
        "stm_discard_transaction_log", pf_predicate, [], [StmVar],
        instmap_delta_from_assoc_list(
            [StmVar - ground(clobbered, none_or_default_func)]),
        only_mode, detism_det, purity_impure, [], DropStateCall, !NewPredInfo),
    create_plain_conj(LockAndValidateGoals ++ [DropStateCall, RecCall],
        Goal).

    % Creates the necessary goals for switching on an exception. The role of
    % the created goals is to extract the exception from the exception result
    % (using predicates from the "univ" module) and create the if-then-else
    % statements which branch on the result.
    %
:- pred create_test_on_exception(prog_context::in, prog_var::in, prog_var::in,
    mer_type::in, hlds_goal::in, hlds_goal::out,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_test_on_exception(Context, ExceptVar, StmVar, ReturnType, RecCall, Goal,
        !NewPredInfo) :-
    create_aux_variable(univ_type, yes("ExceptUnivVar"), ExceptUnivVar,
        !NewPredInfo),
    deconstruct_functor(ExceptVar, exception_exception_functor,
        [ExceptUnivVar], DeconstructException),
    make_type_info(stm_rollback_exception_type, TypeInfoRollbackVar,
        TypeInfoRollbackAssign, !NewPredInfo),
    create_simple_call(mercury_stm_builtin_module,
        "stm_discard_transaction_log", pf_predicate, [], [StmVar],
        instmap_delta_from_assoc_list(
            [StmVar - ground(clobbered, none_or_default_func)]),
        only_mode, detism_det, purity_impure, [], DropStateGoal, !NewPredInfo),

    create_plain_conj([DropStateGoal, RecCall], TrueGoal),
    create_validate_exception_goal(StmVar, ExceptVar, ReturnType, RecCall,
        RethrowBranch, !NewPredInfo),

    create_retry_handler_branch(StmVar, RecCall, RetryBranch, !NewPredInfo),

    template_if_exceptres_is_cons(Context, TypeInfoRollbackVar, ExceptUnivVar,
        stm_rollback_retry_functor, RetryBranch, RethrowBranch, FalseGoal,
        !NewPredInfo),
    template_if_exceptres_is_cons(Context, TypeInfoRollbackVar, ExceptUnivVar,
        stm_rollback_exception_functor, TrueGoal, FalseGoal, IfThenElseGoal,
        !NewPredInfo),
    create_plain_conj([DeconstructException] ++ TypeInfoRollbackAssign ++
        [IfThenElseGoal], Goal).

    % Creates the main goal for the rollback predicate. The goals created
    % by this predicate create the closure for the wrapper predicate and
    % deconstructs the value returned if no exception is present. It relies
    % on the above predicates to generate code for handling exceptions.
    %
:- pred create_rollback_handler_goal(prog_context::in, stm_goal_vars::in,
    mer_type::in, prog_var::in, prog_var::in, pred_proc_id::in, hlds_goal::in,
    hlds_goal::out, stm_info::in,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_rollback_handler_goal(Context, AtomicGoalVars, ReturnType,
        StmVarDI, StmVarUO, WrapperID, RecCall, Goal, StmInfo, !NewPredInfo) :-
    get_input_output_varlist(AtomicGoalVars, InputVars, _),
    get_input_output_types(AtomicGoalVars, StmInfo, InputTypes, _),
    get_input_output_modes(AtomicGoalVars, InputModes, _),

    create_closure(WrapperID, InputVars,
        InputTypes ++ [ReturnType, stm_state_type, stm_state_type],
        InputModes ++ [out_mode, di_mode, uo_mode],
        AtomicClosureVar, ClosureAssign, !NewPredInfo),

    make_type_info(ReturnType, ReturnTypeInfoVar,
        ReturnTypeInfoVarAssign, !NewPredInfo),

    % Creates the necessary exception types, based on the output type of
    % the stm predicate.

    Exception_Result_Type = exception_result_type(ReturnType),
    ExceptRes_Success_Functor = exception_succeeded_functor,
    ExceptRes_Failure_Functor = exception_exception_functor,

    create_aux_variable(Exception_Result_Type, yes("ExceptionResult"),
        ReturnExceptVar, !NewPredInfo),

    create_simple_call(mercury_stm_builtin_module,
        "stm_create_transaction_log", pf_predicate, [], [StmVarDI],
        instmap_delta_from_assoc_list(
            [StmVarDI - ground(unique, none_or_default_func)]),
        only_mode, detism_det, purity_impure, [],
        Goal_StmCreate, !NewPredInfo),

    % TODO: Select mode based on determism of actual goal. 0 if determistic,
    % 1 if cc_multi.

    create_simple_call(mercury_exception_module, "unsafe_try_stm",
        pf_predicate, [ReturnTypeInfoVar],
        [AtomicClosureVar, ReturnExceptVar, StmVarDI, StmVarUO],
        instmap_delta_from_assoc_list([
            ReturnTypeInfoVar - ground(shared, none_or_default_func),
            AtomicClosureVar - ground(shared, none_or_default_func),
            ReturnExceptVar - ground(shared, none_or_default_func),
            StmVarDI - ground(clobbered, none_or_default_func),
            StmVarUO - ground(unique, none_or_default_func)]),
        mode_no(0), detism_cc_multi, purity_pure, [],
        Goal_TryStm, !NewPredInfo),

    % For successfull execution, deconstruct and return true
    deconstruct_output(AtomicGoalVars, ReturnType, ReturnExceptVar,
        Branch_AtomicSuccess, StmInfo, !NewPredInfo),
    create_test_on_exception(Context, ReturnExceptVar, StmVarUO,
        ReturnType, RecCall, Branch_AtomicException, !NewPredInfo),

    create_switch_disjunction(ReturnExceptVar,
        [case(ExceptRes_Failure_Functor, [], Branch_AtomicException),
        case(ExceptRes_Success_Functor, [], Branch_AtomicSuccess)],
        detism_det, purity_impure, DisjGoal, !NewPredInfo),

    create_plain_conj([Goal_StmCreate | ReturnTypeInfoVarAssign] ++
        [ClosureAssign, Goal_TryStm, DisjGoal], Goal0),
    create_promise_purity_scope(Goal0, purity_pure, Goal).

    % Creates the rollback predicate. This predicate is responsible for
    % making the closure to the wrapper predicate and executing it whilst
    % catching any possible exceptions that might be thrown It is also
    % responsible for handing retries and rollbacks.
    %
:- pred create_rollback_pred(prog_context::in, list(stm_goal_vars)::in,
    hlds_goal::out, hlds_goal::in, list(hlds_goal)::in,
    stm_info::in, stm_info::out) is det.

create_rollback_pred(Context, AtomicGoalVarList, CallGoal, AtomicGoal,
        OrElseGoals, !StmInfo) :-
    common_goal_vars_from_list(AtomicGoalVarList, AtomicGoalVars),
    get_input_output_varlist(AtomicGoalVars, InputVars, OutputVars),
    get_input_output_types(AtomicGoalVars, !.StmInfo, InputTypes, OutputTypes),
    get_input_output_modes(AtomicGoalVars, InputModes, OutputModes),

    create_cloned_pred(InputVars ++ OutputVars, InputTypes ++ OutputTypes,
        InputModes ++ OutputModes, stmck_rollback, AtomicGoal,
        no, NewPredInfo0, CallGoal, !StmInfo),

    create_rollback_pred_2(Context, AtomicGoalVarList, CallGoal,
        AtomicGoal, OrElseGoals, NewPredInfo0, NewPredInfo, !StmInfo),
    commit_new_pred(NewPredInfo, !StmInfo).

:- pred create_rollback_pred_2(prog_context::in, list(stm_goal_vars)::in,
    hlds_goal::in, hlds_goal::in, list(hlds_goal)::in,
    stm_new_pred_info::in, stm_new_pred_info::out,
    stm_info::in, stm_info::out) is det.

create_rollback_pred_2(Context, AtomicGoalVarList, RecCallGoal,
        AtomicGoal, OrElseGoals, !NewPredInfo, !StmInfo) :-
    common_goal_vars_from_list(AtomicGoalVarList, AtomicGoalVars),

    get_input_output_types(AtomicGoalVars, !.StmInfo, _, OutputTypes),
    make_return_type(OutputTypes, ResultType),
    create_aux_variable(ResultType, yes("ResultVar"), ResultVar, !NewPredInfo),
    create_aux_variable(stm_state_type, yes("STM0"), InnerDI, !NewPredInfo),
    create_aux_variable(stm_state_type, yes("STM"), InnerUO, !NewPredInfo),

    % Temporally commits the predicate to the StmInfo so that the wrapper
    % predicate can have the most up to date copy of the module info.
    commit_new_pred(!.NewPredInfo, !StmInfo),

    ProcessGoalList = [AtomicGoal | OrElseGoals],
    create_wrapper_for_goal_list(Context, AtomicGoalVarList,
        ResultType, ResultVar, ProcessGoalList, WrapperID, _, !StmInfo),

    % Stores the up to date module info back into the new predicate info.
    update_new_pred_info(!.StmInfo, !NewPredInfo),

    create_rollback_handler_goal(Context, AtomicGoalVars, ResultType,
        InnerDI, InnerUO, WrapperID, RecCallGoal, RollbackGoal,
        !.StmInfo, !NewPredInfo),
    new_pred_set_goal(RollbackGoal, !NewPredInfo),
    run_quantification_over_pred(!NewPredInfo),
    commit_new_pred(!.NewPredInfo, !StmInfo).

%-----------------------------------------------------------------------------%
%
% Predicates involved in moving local variables from the original predicate
% to the newly created wrapper predicate.

    % Moves a single variable, along with its type, from the original
    % predicate to the newly created wrapper predicate.
    %
:- pred apply_varset_to_preds(prog_var::in, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

apply_varset_to_preds(ProgVar, !NewPredVarSet, !NewPredVarTypes,
        !OldPredVarSet, !OldPredVarTypes, !VarMapping) :-
    lookup_var_type(!.OldPredVarTypes, ProgVar, ProgType),
%   delete_var(!.OldPredVarSet, ProgVar, !:OldPredVarSet),
%   map.delete(!.OldPredVarTypes, ProgVar, !:OldPredVarTypes),
    varset.new_var(NewProgVar, !NewPredVarSet),
    add_var_type(NewProgVar, ProgType, !NewPredVarTypes),
    map.det_insert(ProgVar, NewProgVar, !VarMapping).

    % Moves all local variables from the original predicate to the newly
    % created wrapper predicate. This also includes the original STM
    % di and uo variables.
    %
:- pred move_variables_to_new_pred(hlds_goal::in, hlds_goal::out,
    stm_goal_vars::in, prog_var::in, prog_var::in,
    stm_new_pred_info::in, stm_new_pred_info::out,
    stm_info::in, stm_info::out) is det.

move_variables_to_new_pred(AtomicGoal0, AtomicGoal, AtomicGoalVars,
        InnerDI, InnerUO, !NewPredInfo, !StmInfo) :-
    NewProcInfo0 = !.NewPredInfo ^ new_pred_proc_info,
    OldProcInfo0 = !.StmInfo ^ stm_info_proc_info,
    proc_info_get_varset_vartypes(NewProcInfo0,
        NewPredVarSet0, NewPredVarTypes0),
    proc_info_get_varset_vartypes(OldProcInfo0,
        OldPredVarSet0, OldPredVarTypes0),
    AtomicGoalVars = stm_goal_vars(_, LocalVars, _, OrigInnerDI, OrigInnerUO),
    LocalVarList = set_of_var.to_sorted_list(LocalVars),

    VarMapping0 = map.init,
    list.foldl5(apply_varset_to_preds, LocalVarList,
        NewPredVarSet0, NewPredVarSet, NewPredVarTypes0, NewPredVarTypes,
        OldPredVarSet0, OldPredVarSet, OldPredVarTypes0, OldPredVarTypes,
        VarMapping0, VarMapping1),

    ( if OrigInnerDI = OrigInnerUO then
        map.det_insert(OrigInnerDI, InnerDI, VarMapping1, VarMapping)
    else
        map.det_insert(OrigInnerDI, InnerDI, VarMapping1, VarMapping2),
        map.det_insert(OrigInnerUO, InnerUO, VarMapping2, VarMapping)
    ),

    rename_some_vars_in_goal(VarMapping, AtomicGoal0, AtomicGoal),
    proc_info_set_varset_vartypes(NewPredVarSet, NewPredVarTypes,
        NewProcInfo0, NewProcInfo),
    proc_info_set_varset_vartypes(OldPredVarSet, OldPredVarTypes,
        OldProcInfo0, OldProcInfo),
    !NewPredInfo ^ new_pred_proc_info := NewProcInfo,
    !StmInfo ^ stm_info_proc_info := OldProcInfo.

%-----------------------------------------------------------------------------%
%
% Predicates involved in the creation of the wrapper predicate.
%

:- pred create_wrapper_for_goal_list(prog_context::in, list(stm_goal_vars)::in,
    mer_type::in, prog_var::in, list(hlds_goal)::in,
    pred_proc_id::out, hlds_goal::out, stm_info::in, stm_info::out) is det.

create_wrapper_for_goal_list(Context, AtomicGoalVarList, ResultType, ResultVar,
        GoalList, PredProcId, CallGoal, !StmInfo) :-
    (
        GoalList = [],
        unexpected($pred, "empty list")
    ;
        GoalList = [SingleGoal],
        AtomicGoalVars = list.det_head(AtomicGoalVarList),
        create_wrapper_pred(AtomicGoalVars, ResultType, ResultVar, SingleGoal,
            PredProcId, CallGoal, !StmInfo)
    ;
        GoalList = [_, _ | _],

        CreateWrapperForEachGoal =
            ( pred(Goal::in, GoalVars::in, PPID::out,
                    SInfo0::in, SInfo::out) is det :-
                % These predicates should be plain predicates without code to
                % validate logs.
                create_simple_wrapper_pred(Context, GoalVars,
                    ResultType, ResultVar, Goal, PPID, _, SInfo0, SInfo)
            ),
        map2_in_foldl(CreateWrapperForEachGoal, GoalList, AtomicGoalVarList,
            PPIDList, !StmInfo),

        common_goal_vars_from_list(AtomicGoalVarList, AtomicGoalVars),
%       XXX STM
%       copy_input_vars_in_goallist(AtomicGoalVars, AtomicGoalVarList,
%           AtomicGoalVarList1),
        AtomicGoalVarList1 = AtomicGoalVarList,
        StmDI = AtomicGoalVars ^ vars_innerDI,
        StmUO = AtomicGoalVars ^ vars_innerUO,

        create_or_else_pred(Context, AtomicGoalVars, AtomicGoalVarList1,
            PPIDList, StmDI, StmUO, NewAtomicGoal, !StmInfo),
        create_wrapper_pred(AtomicGoalVars, ResultType, ResultVar,
            NewAtomicGoal, PredProcId, CallGoal, !StmInfo)
    ).

    % Creates the wrapper predicate. Return the pred_proc_id of the newly
    % created wrapper predicate as well as a goal to call it.
    %
:- pred create_wrapper_pred(stm_goal_vars::in, mer_type::in, prog_var::in,
    hlds_goal::in, pred_proc_id::out, hlds_goal::out,
    stm_info::in, stm_info::out) is det.

create_wrapper_pred(AtomicGoalVars, ResultType, ResultVar0, AtomicGoal,
        PredProcId, CallGoal, !StmInfo) :-
    create_wrapper_pred_2(AtomicGoalVars, ResultType, ResultVar0, AtomicGoal,
        PredProcId, _, CallGoal, !StmInfo).

:- pred create_wrapper_pred_2(stm_goal_vars::in, mer_type::in, prog_var::in,
    hlds_goal::in, pred_proc_id::out, stm_new_pred_info::out, hlds_goal::out,
    stm_info::in, stm_info::out) is det.

create_wrapper_pred_2(AtomicGoalVars, ResultType, ResultVar0,
        !.AtomicGoal, PredProcId, !:NewPredInfo, CallGoal, !StmInfo) :-
    InnerDI = AtomicGoalVars ^ vars_innerDI,
    InnerUO0 = AtomicGoalVars ^ vars_innerUO,

    get_input_output_varlist(AtomicGoalVars, InputVars, _),
    get_input_output_types(AtomicGoalVars, !.StmInfo, InputTypes, _),
    get_input_output_modes(AtomicGoalVars, InputModes, _),

    create_cloned_pred(InputVars ++ [ResultVar0, InnerDI, InnerUO0],
        InputTypes ++ [ResultType, stm_state_type, stm_state_type],
        InputModes ++ [out_mode, di_mode, uo_mode],
        stmck_wrapper, !.AtomicGoal, no, !:NewPredInfo, CallGoal, !StmInfo),

    rename_var_in_wrapper_pred("stm_ResultVar", ResultVar0, ResultType,
        ResultVar, !NewPredInfo, !AtomicGoal),
    move_variables_to_new_pred(!AtomicGoal, AtomicGoalVars, InnerDI, InnerUO0,
        !NewPredInfo, !StmInfo),

    % Handles the case when the Inner di and Inner uo variables are the same.
    % Explicitly creates a unification to keep these variables different
    % (because of the uniqueness requirements of a number of calls added to
    % the end of the original goal)

    ( if InnerUO0 = InnerDI then
        CopyStm = yes,
        create_aux_variable(stm_state_type, yes("NewUO"), InnerUO,
            !NewPredInfo)
    else
        CopyStm = no,
        InnerUO = InnerUO0
    ),

    create_post_wrapper_goal(AtomicGoalVars, !.AtomicGoal, ResultType,
        ResultVar, InnerDI, InnerUO, CopyStm, WrapperGoal, !.StmInfo,
        !NewPredInfo),

    set_head_vars(InputVars ++ [ResultVar0, InnerDI, InnerUO], !NewPredInfo),
    new_pred_set_goal(WrapperGoal, !NewPredInfo),
    run_quantification_over_pred(!NewPredInfo),
    get_pred_proc_id(!.NewPredInfo, PredProcId),
    commit_new_pred(!.NewPredInfo, !StmInfo).

    % Creates the goals for validating and committing (or raising a rollback
    % exception) a transaction log. These goals appear after the original goal.
    % If the value of CopySTM is "yes", a goal unifying the variable in StmDI
    % and the variable in StmUO will be created before the log is validated.
    %
:- pred create_post_wrapper_goal(stm_goal_vars::in, hlds_goal::in,
    mer_type::in, prog_var::in, prog_var::in, prog_var::in, bool::in,
    hlds_goal::out, stm_info::in,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_post_wrapper_goal(AtomicGoalVars, AtomicGoal, ResultType, ResultVar,
        StmDI, StmUO, CopySTM, Goal, StmInfo, !NewPredInfo) :-
    StmModuleName = mercury_stm_builtin_module,
    ExceptionModuleName = mercury_exception_module,

    AtomicGoal = hlds_goal(_, AtomicGoalInfo),
    Context = goal_info_get_context(AtomicGoalInfo),
    construct_output(Context, AtomicGoalVars, ResultType, ResultVar, StmInfo,
        AssignResult, !NewPredInfo),
    create_aux_variable(stm_valid_result_type, yes("Stm_Expand_IsValid"),
        IsValidVar, !NewPredInfo),

    ValidTrueFunctor = stm_validres_valid_functor,
    ValidFalseFunctor = stm_validres_invalid_functor,
    RollbackCons = stm_rollback_exception_functor,

    % Creates the necessary predicate calls.

    create_aux_variable_assignment(Context,
        RollbackCons, stm_rollback_exception_type,
        yes("Stm_Expand_Rollback"), ConstRollbackGoal, RollbackVar,
        !NewPredInfo),
    create_simple_call(StmModuleName, "stm_lock", pf_predicate,
        [], [], instmap_delta_bind_no_var, only_mode,
        detism_det, purity_impure, [], Goal_StmLock_Call, !NewPredInfo),
    create_simple_call(StmModuleName, "stm_unlock", pf_predicate,
        [], [], instmap_delta_bind_no_var, only_mode,
        detism_det, purity_impure, [], Goal_StmUnLock_Call, !NewPredInfo),
    create_simple_call(StmModuleName, "stm_validate", pf_predicate,
        [], [StmUO, IsValidVar],
        instmap_delta_from_assoc_list(
            [StmUO - ground(unique, none_or_default_func),
            IsValidVar - ground(shared, none_or_default_func)]),
        only_mode, detism_det, purity_impure,
        [], Goal_StmValidate_Call, !NewPredInfo),
    create_simple_call(StmModuleName, "stm_commit", pf_predicate,
        [], [StmUO],
        instmap_delta_from_assoc_list(
            [StmUO - ground(unique, none_or_default_func)]),
        only_mode, detism_det, purity_impure,
        [], Goal_StmCommit_Call, !NewPredInfo),

    make_type_info(stm_rollback_exception_type, TypeInfoVar,
        CreateTypeInfoGoals, !NewPredInfo),

    create_simple_call(ExceptionModuleName, "throw", pf_predicate,
        [TypeInfoVar], [RollbackVar],
        instmap_delta_bind_vars([TypeInfoVar, RollbackVar]),
        only_mode, detism_erroneous, purity_pure,
        [], Goal_ExceptionThrow_Call, !NewPredInfo),

    % Creates the branch on the validation result of the log.
    create_plain_conj([Goal_StmCommit_Call, Goal_StmUnLock_Call],
        Goal_ValidBranch),
    create_plain_conj([Goal_StmUnLock_Call, ConstRollbackGoal] ++
        CreateTypeInfoGoals ++ [Goal_ExceptionThrow_Call],
        Goal_InvalidBranch),

    create_switch_disjunction(IsValidVar,
        [case(ValidTrueFunctor, [], Goal_ValidBranch),
         case(ValidFalseFunctor, [], Goal_InvalidBranch)],
        detism_det, purity_impure, DisjGoal, !NewPredInfo),

    % Creates the main validation and commission goal.
    PostAtomicTopLevelList = [Goal_StmLock_Call,
        Goal_StmValidate_Call, DisjGoal],

    create_plain_conj(PostAtomicTopLevelList, PostAtomicGoal0),
    create_promise_purity_scope(PostAtomicGoal0, purity_pure, PostAtomicGoal),

    % Creates the unification between StmUO and StmDI is needed.
    (
        CopySTM = yes,
        create_var_unify(StmUO, StmDI,
            unify_modes_li_lf_ri_rf(free, unique_inst,
                unique_inst, clobbered_inst),
            CopySTMAssign, !NewPredInfo),
        TopLevelGoalList0 = [AtomicGoal] ++ AssignResult ++ [CopySTMAssign,
            PostAtomicGoal]
    ;
        CopySTM = no,
        TopLevelGoalList0 = [AtomicGoal] ++ AssignResult ++
            [PostAtomicGoal]
    ),

    flatten_conj(TopLevelGoalList0, TopLevelGoalList),
    create_plain_conj(TopLevelGoalList, Goal).

    % Creates a simpler wrapper predicate for or_else branches.
    %
:- pred create_simple_wrapper_pred(prog_context::in,
    stm_goal_vars::in, mer_type::in, prog_var::in, hlds_goal::in,
    pred_proc_id::out, hlds_goal::out,
    stm_info::in, stm_info::out) is det.

create_simple_wrapper_pred(Context, AtomicGoalVars, ResultType, ResultVar0,
        AtomicGoal, PredProcId, CallGoal, !StmInfo) :-
    create_simple_wrapper_pred_2(Context, AtomicGoalVars,
        ResultType, ResultVar0, AtomicGoal, PredProcId, _, CallGoal, !StmInfo).

:- pred create_simple_wrapper_pred_2(prog_context::in,
    stm_goal_vars::in, mer_type::in, prog_var::in, hlds_goal::in,
    pred_proc_id::out, stm_new_pred_info::out,
    hlds_goal::out, stm_info::in, stm_info::out) is det.

create_simple_wrapper_pred_2(Context, AtomicGoalVars, ResultType, ResultVar0,
        !.AtomicGoal, PredProcId, !:NewPredInfo, CallGoal, !StmInfo) :-
    InnerDI = AtomicGoalVars ^ vars_innerDI,
    InnerUO0 = AtomicGoalVars ^ vars_innerUO,

    get_input_output_varlist(AtomicGoalVars, InputVars, _),
    get_input_output_types(AtomicGoalVars, !.StmInfo, InputTypes, _),
    get_input_output_modes(AtomicGoalVars, InputModes, _),

    create_cloned_pred(InputVars ++ [ResultVar0, InnerDI, InnerUO0],
        InputTypes ++ [ResultType, stm_state_type, stm_state_type],
        InputModes ++ [out_mode, di_mode, uo_mode], stmck_simple_wrapper,
        !.AtomicGoal, no, !:NewPredInfo, CallGoal, !StmInfo),

    rename_var_in_wrapper_pred("stm_ResultVar", ResultVar0, ResultType,
        ResultVar, !NewPredInfo, !AtomicGoal),
    move_variables_to_new_pred(!AtomicGoal, AtomicGoalVars, InnerDI, InnerUO0,
        !NewPredInfo, !StmInfo),

    % Handles the case when the Inner di and Inner uo variables are the same.
    % Explicitly creates a unification to keep these variables different
    % (because of the uniqueness requirements of a number of calls added to
    % the end of the original goal)

    ( if InnerUO0 = InnerDI then
        CopyStm = yes,
        create_aux_variable(stm_state_type, yes("NewUO"), InnerUO,
            !NewPredInfo)
    else
        CopyStm = no,
        InnerUO = InnerUO0
    ),

    create_simple_post_wrapper_goal(Context, AtomicGoalVars, !.AtomicGoal,
        ResultType, ResultVar, InnerDI, InnerUO, CopyStm, WrapperGoal,
        !.StmInfo, !NewPredInfo),

    set_head_vars(InputVars ++ [ResultVar, InnerDI, InnerUO], !NewPredInfo),
    new_pred_set_goal(WrapperGoal, !NewPredInfo),
    run_quantification_over_pred(!NewPredInfo),
    get_pred_proc_id(!.NewPredInfo, PredProcId),
    commit_new_pred(!.NewPredInfo, !StmInfo).

% To Remove eventually
:- pred create_probe_call(string::in, prog_var::in, list(hlds_goal)::out,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_probe_call(_Name, _Var, Goals, !NewPredInfo) :-
    Goals = [].

    % Creates the goals for validating and committing (or raising a rollback
    % exception) a transaction log. These goals appear after the original goal.
    % If the value of CopySTM is "yes", a goal unifying the variable in StmDI
    % and the variable in StmUO will be created before the log is validated.
    %
:- pred create_simple_post_wrapper_goal(prog_context::in,
    stm_goal_vars::in, hlds_goal::in, mer_type::in, prog_var::in,
    prog_var::in, prog_var::in, bool::in, hlds_goal::out, stm_info::in,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_simple_post_wrapper_goal(Context, AtomicGoalVars, AtomicGoal,
        ResultType, ResultVar, StmDI, StmUO, CopySTM, Goal, StmInfo,
        !NewPredInfo) :-
    construct_output(Context, AtomicGoalVars, ResultType, ResultVar, StmInfo,
        AssignResult, !NewPredInfo),

    create_probe_call("start_of_wrapper", StmDI, Call1, !NewPredInfo),
    create_probe_call("start_of_wrapper", StmUO, Call2, !NewPredInfo),

    % Creates the unification between StmUO and StmDI is needed.
    (
        CopySTM = yes,
        create_var_unify(StmUO, StmDI,
            unify_modes_li_lf_ri_rf(free, unique_inst,
                unique_inst, clobbered_inst),
            CopySTMAssign, !NewPredInfo),
        TopLevelGoalList0 = Call1 ++ [CopySTMAssign, AtomicGoal] ++ Call2 ++
            AssignResult
    ;
        CopySTM = no,
        TopLevelGoalList0 = Call1 ++ [AtomicGoal] ++ Call2 ++ AssignResult
    ),

    flatten_conj(TopLevelGoalList0, TopLevelGoalList),
    create_plain_conj(TopLevelGoalList, Goal).

%-----------------------------------------------------------------------------%
%
% Predicates used in the creation of "or_else" goals.
%

    % or_else(<<inners>>, <<outers>>, <<STM_di>>, <<STM_uo>>) is det.
    %
:- pred create_or_else_pred(prog_context::in,
    stm_goal_vars::in, list(stm_goal_vars)::in, list(pred_proc_id)::in,
    prog_var::in, prog_var::in, hlds_goal::out,
    stm_info::in, stm_info::out) is det.

create_or_else_pred(Context, AtomicGoalVars, BranchGoalVars, Closures,
        StmDI, StmUO, CallGoal, !StmInfo) :-
    get_input_output_varlist(AtomicGoalVars, InputVars, OutputVars),
    get_input_output_types(AtomicGoalVars, !.StmInfo, InputTypes, OutputTypes),
    get_input_output_modes(AtomicGoalVars, InputModes, OutputModes),

%   MaybeDetism = yes(detism_cc_multi),
    MaybeDetism = no,

    make_return_type(OutputTypes, ReturnType),
    create_cloned_pred(InputVars ++ OutputVars ++ [StmDI, StmUO],
        InputTypes ++ OutputTypes ++ [stm_state_type, stm_state_type],
        InputModes ++ OutputModes ++ [di_mode, uo_mode], stmck_or_else,
        true_goal, MaybeDetism, NewPredInfo0, CallGoal, !StmInfo),

    create_aux_variable(stm_state_type, yes("STMDI"), NewStmDI,
        NewPredInfo0, NewPredInfo1),
    create_aux_variable(stm_state_type, yes("STMUO"), NewStmUO,
        NewPredInfo1, NewPredInfo2),
    set_head_vars(InputVars ++ OutputVars ++ [NewStmDI, NewStmUO],
        NewPredInfo2, NewPredInfo3),

    create_or_else_pred_2(Context, BranchGoalVars, Closures,
        NewStmDI, NewStmUO, ReturnType, !.StmInfo, NewPredInfo3, NewPredInfo),

    commit_new_pred(NewPredInfo, !StmInfo).

:- pred create_or_else_pred_2(prog_context::in,
    list(stm_goal_vars)::in, list(pred_proc_id)::in,
    prog_var::in, prog_var::in, mer_type::in, stm_info::in,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_or_else_pred_2(Context, AtomicGoalVars, Closures, StmDI, StmUO,
        ReturnType, StmInfo, !NewPredInfo) :-
    list.length(Closures, ClosureCount),
    create_or_else_inner_stm_vars(ClosureCount, InnerSTMVars, !NewPredInfo),

    make_type_info(ReturnType, ReturnTypeInfoVar, CreateRetTypeInfo,
        !NewPredInfo),
    make_type_info(stm_rollback_exception_type, ExceptRttiVar,
        CreateExceptTypeInfo, !NewPredInfo),

    create_or_else_end_branch(Context, InnerSTMVars, StmDI, StmUO,
        ExceptRttiVar, EndBranchGoal, !NewPredInfo),

    create_or_else_branches(Context, AtomicGoalVars, ReturnType, StmDI, StmUO,
        InnerSTMVars, ReturnTypeInfoVar, ExceptRttiVar, Closures,
        EndBranchGoal, MainGoal0, StmInfo, !NewPredInfo),

    TopLevelGoalList0 = CreateRetTypeInfo ++ CreateExceptTypeInfo ++
        [MainGoal0],
    flatten_conj(TopLevelGoalList0, TopLevelGoalList),

    create_plain_conj(TopLevelGoalList, MainGoal1),
    create_promise_purity_scope(MainGoal1, purity_pure, MainGoal),

    new_pred_set_goal(MainGoal, !NewPredInfo),
    run_quantification_over_pred(!NewPredInfo).

:- pred create_or_else_branches(prog_context::in,
    list(stm_goal_vars)::in, mer_type::in,
    prog_var::in, prog_var::in, list(prog_var)::in, prog_var::in, prog_var::in,
    list(pred_proc_id)::in, hlds_goal::in, hlds_goal::out, stm_info::in,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_or_else_branches(Context, AtomicGoalVars, ReturnType, OuterStmDIVar,
        OuterStmUOVar, InnerSTMVars, RttiVar, RollbackExceptionRttiVar,
        WrapperIDs, EndBranch, Goal, StmInfo, !NewPredInfo) :-
    ( if
        InnerSTMVars = [],
        WrapperIDs = [],
        AtomicGoalVars = []
    then
        Goal = EndBranch
    else if
        AtomicGoalVars = [AGV | AGVs],
        InnerSTMVars = [InnerVar | InnerSTMVars0],
        WrapperIDs = [WrapID | WrapperIDs0]
    then
        create_or_else_branches(Context, AGVs, ReturnType, OuterStmDIVar,
            OuterStmUOVar, InnerSTMVars0, RttiVar, RollbackExceptionRttiVar,
            WrapperIDs0, EndBranch, Goal0, StmInfo, !NewPredInfo),
        create_or_else_branch(Context, AGV, ReturnType, OuterStmDIVar,
            OuterStmUOVar, InnerVar, RttiVar, RollbackExceptionRttiVar,
            WrapID, Goal0, Goal, StmInfo, !NewPredInfo)
    else
        unexpected($pred, "mismatched lists")
    ).

:- pred create_or_else_inner_stm_vars(int::in, list(prog_var)::out,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_or_else_inner_stm_vars(Count, Vars, !NewPredInfo) :-
    ( if Count = 0 then
        Vars = []
    else if Count > 0 then
        create_aux_variable(stm_state_type, yes("InnSTM"), Var, !NewPredInfo),
        Count1 = Count - 1,
        create_or_else_inner_stm_vars(Count1, Vars0, !NewPredInfo),
        Vars = [Var | Vars0]
    else
        unexpected($pred, "negative count")
    ).

    % Creates an or_else branch.
    %
    %       impure stm_create_nested_log(OuterSTM0, InnerSTM0),
    %       unsafe_try_stm(TransA, ResultA, InnerSTM0, InnerSTM),
    %       (
    %           ResultA = succeeded(Result),
    %           impure stm_merge_nested_logs(InnerSTM, OuterSTM0, OuterSTM)
    %       ;
    %           ResultA = exception(Excp)
    %           ( if Excp = univ(rollback_retry) then
    %               << nested or_else branch >>
    %           else
    %               impure stm_discard_transaction_log(InnerSTM),
    %               rethrow(Result)
    %           )
    %       )
    %
:- pred map2_in_foldl(
    pred(K, L, N, A, A)::in(pred(in, in, out, in, out) is det),
    list(K)::in, list(L)::in, list(N)::out, A::in, A::out) is det.

map2_in_foldl(Pred, Src1, Src2, Dest, !Accum) :-
    ( if
        Src1 = [],
        Src2 = []
    then
        Dest = []
    else if
        Src1 = [S | Ss],
        Src2 = [T | Ts]
    then
        Pred(S, T, R, !Accum),
        map2_in_foldl(Pred, Ss, Ts,  Rs, !Accum),
        Dest = [R | Rs]
    else
        unexpected($pred, "source list lengths mismatch")
    ).

:- pred map3_in_foldl(
    pred(K, L, M, N, A, A)::in(pred(in, in, in, out, in, out) is det),
    list(K)::in, list(L)::in, list(M)::in, list(N)::out, A::in, A::out) is det.

map3_in_foldl(Pred, Src1, Src2, Src3, Dest, !Accum) :-
    ( if
        Src1 = [],
        Src2 = [],
        Src3 = []
    then
        Dest = []
    else if
        Src1 = [S | Ss],
        Src2 = [T | Ts],
        Src3 = [U | Us]
    then
        Pred(S, T, U, R, !Accum),
        map3_in_foldl(Pred, Ss, Ts, Us, Rs, !Accum),
        Dest = [R | Rs]
    else
        unexpected($pred, "source list lengths mismatch")
    ).

:- pred create_or_else_end_branch(prog_context::in,
    list(prog_var)::in, prog_var::in, prog_var::in, prog_var::in,
    hlds_goal::out, stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_or_else_end_branch(Context, StmVars, OuterSTMDI, OuterSTMUO,
        ExceptionRttiVar, Goal, !NewPredInfo) :-
    MakeIntermediateStmVars =
        ( pred(_::in, Var::out, NPI0::in, NPI::out) is det:-
            create_aux_variable(stm_state_type, yes("InterSTM"), Var,
                NPI0, NPI)
        ),

    % We don't actually need the list as it is simply used as a counter.
    StmVarsMinusHead = list.det_tail(StmVars),
    list.map_foldl(MakeIntermediateStmVars, StmVarsMinusHead,
        IntermediateStmVars, !NewPredInfo),

    MergeStmVarsIn = [OuterSTMDI | IntermediateStmVars],
    MergeStmVarsOut = IntermediateStmVars ++ [OuterSTMUO],

    MakeMergeGoals =
        ( pred(StmVar::in, ThreadSTMDI::in, ThreadSTMUO::in,
                ThisGoal::out, NPI0::in, NPI::out) is det :-
            create_simple_call(mercury_stm_builtin_module,
                "stm_merge_nested_logs", pf_predicate,
                [], [StmVar, ThreadSTMDI, ThreadSTMUO],
                instmap_delta_from_assoc_list(
                    [StmVar - ground(unique, none_or_default_func),
                    ThreadSTMDI - free,
                    ThreadSTMUO - ground(unique, none_or_default_func)]),
                only_mode, detism_det, purity_impure,
                [], ThisGoal, NPI0, NPI)
        ),

    map3_in_foldl(MakeMergeGoals, StmVars, MergeStmVarsIn, MergeStmVarsOut,
        MergeGoals, !NewPredInfo),

    create_simple_call(mercury_stm_builtin_module, "stm_unlock", pf_predicate,
        [], [], instmap_delta_bind_no_var, only_mode,
        detism_det, purity_impure, [], UnlockCall, !NewPredInfo),

    create_aux_variable_assignment(Context, stm_rollback_retry_functor,
        stm_rollback_exception_type, yes("RetryCons"), AssignRetryCons,
        RetryConsVar, !NewPredInfo),
    create_simple_call(mercury_exception_module, "throw", pf_predicate,
        [ExceptionRttiVar], [RetryConsVar],
        instmap_delta_bind_vars([ExceptionRttiVar, RetryConsVar]),
        only_mode, detism_erroneous, purity_pure, [], RetryCall, !NewPredInfo),

%   XXX STM
%   create_simple_call(mercury_stm_builtin_module, "retry",
%       pf_predicate, only_mode,
%       detism_det, purity_pure, [OuterSTMUO], [],
%       instmap_delta_bind_var(OuterSTMUO), RetryCall, !NewPredInfo),
    create_plain_conj(MergeGoals ++ [UnlockCall, AssignRetryCons, RetryCall],
        ValidGoal),

    % Failure break

    create_aux_variable_assignment(Context, stm_rollback_exception_functor,
        stm_rollback_exception_type, yes("RollbackCons"), AssignRollbackCons,
        RollbackConsVar, !NewPredInfo),
    create_simple_call(mercury_exception_module, "throw", pf_predicate,
        [ExceptionRttiVar], [RollbackConsVar],
        instmap_delta_bind_vars([ExceptionRttiVar, RollbackConsVar]),
        only_mode, detism_erroneous, purity_pure, [], ThrowCall, !NewPredInfo),
    create_plain_conj([UnlockCall, AssignRollbackCons, ThrowCall],
        InvalidGoal),

    template_lock_and_validate_many(Context, StmVars, no,
        ValidGoal, InvalidGoal, Goals, !NewPredInfo),
    create_plain_conj(Goals, Goal).

    % Variables are:
    %
    % StmGoalVars
    % ReturnType -- Return type of the or_else pred
    % ReturnValue -- Return variable of the or_else pred (not decompressed)
    % OuterStmDIVar -- Outer STM DI Variable (in pred head)
    % OuterStmUOVar -- Outer STM UO Variable (in pred head)
    % RttiVar -- Variable holding type_info for ReturnType
    % RollbackExceptionRttiVar -- Variable holding type_info for
    %   "stm_builtin.rollback_exception_type"
    % WrapperID -- The predicate ID of the call to try
    % RetryBranch -- The goal to execute when a retry is called
    % InnerSTMVar -- The DI variable of the retry branch. It must be created
    %   outside this predicate as it needs to be known to the validate & merge
    %   branch.
    %
:- pred create_or_else_branch(prog_context::in,
    stm_goal_vars::in, mer_type::in, prog_var::in,
    prog_var::in, prog_var::in, prog_var::in, prog_var::in,
    pred_proc_id::in, hlds_goal::in, hlds_goal::out, stm_info::in,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_or_else_branch(Context, AtomicGoalVars, ReturnType, OuterStmDIVar,
        OuterStmUOVar, InnerSTMVar, RttiVar, RollbackExceptionRttiVar,
        WrapperID, RetryBranch, Goal, StmInfo, !NewPredInfo) :-
    get_input_output_varlist(AtomicGoalVars, InputVars, _),
    get_input_output_types(AtomicGoalVars, StmInfo, InputTypes, _),
    get_input_output_modes(AtomicGoalVars, InputModes, _),

    create_aux_variable(stm_state_type, yes("InnerSTM0"), InnerSTM0Var,
        !NewPredInfo),
    create_aux_variable(exception_result_type(ReturnType), yes("ExcptRes"),
        ReturnExceptVar, !NewPredInfo),

    create_closure(WrapperID, InputVars,
        InputTypes ++ [ReturnType, stm_state_type, stm_state_type],
        InputModes ++ [out_mode, di_mode, uo_mode],
        AtomicClosureVar, ClosureAssign, !NewPredInfo),

    create_simple_call(mercury_stm_builtin_module,
        "stm_create_nested_transaction_log", pf_predicate,
        [OuterStmDIVar, InnerSTM0Var], [],
        instmap_delta_from_assoc_list(
            [OuterStmDIVar - ground(unique, none_or_default_func),
            InnerSTM0Var - free]),
        only_mode, detism_det, purity_impure,
        [], CreateNestedLogCall, !NewPredInfo),

    create_simple_call(mercury_exception_module, "unsafe_try_stm",
        pf_predicate, [RttiVar],
        [AtomicClosureVar, ReturnExceptVar, InnerSTM0Var, InnerSTMVar],
        instmap_delta_from_assoc_list([
            RttiVar - ground(shared, none_or_default_func),
            AtomicClosureVar - ground(shared, none_or_default_func),
            ReturnExceptVar - free,
            InnerSTM0Var - ground(unique, none_or_default_func),
            InnerSTMVar - free]),
        mode_no(0), detism_cc_multi, purity_pure,
        [], TryStmCall, !NewPredInfo),

    % Successfull execution, deconstruct and return
    deconstruct_output(AtomicGoalVars, ReturnType, ReturnExceptVar,
        DeconstructGoal, StmInfo, !NewPredInfo),
    create_simple_call(mercury_stm_builtin_module, "stm_merge_nested_logs",
        pf_predicate, [], [InnerSTMVar, OuterStmDIVar, OuterStmUOVar],
        instmap_delta_from_assoc_list(
            [InnerSTMVar - ground(unique, none_or_default_func),
            OuterStmDIVar - ground(unique, none_or_default_func),
            OuterStmUOVar - free]),
        only_mode, detism_det, purity_impure,
        [], MergeNestedLogsCall, !NewPredInfo),

    create_plain_conj([DeconstructGoal, MergeNestedLogsCall], SuccessBranch),

    % General exception: discard and throw upwards
    create_simple_call(mercury_stm_builtin_module,
        "stm_discard_transaction_log", pf_predicate, [], [InnerSTMVar],
        instmap_delta_from_assoc_list(
            [InnerSTMVar - ground(unique, none_or_default_func)]),
        only_mode, detism_det, purity_impure, [], DiscardCall, !NewPredInfo),
    create_simple_call(mercury_exception_module, "rethrow",
        pf_predicate, [], [RttiVar, ReturnExceptVar],
        instmap_delta_bind_vars([RttiVar, ReturnExceptVar]),
        only_mode, detism_erroneous, purity_pure,
        [], RethrowCall, !NewPredInfo),

    % Code to extract the exception result.
    create_aux_variable(univ_type, yes("ExceptUnivVar"), ExceptUnivVar,
        !NewPredInfo),
    deconstruct_functor(ReturnExceptVar, exception_exception_functor,
        [ExceptUnivVar], DeconstructException),

    create_plain_conj([DiscardCall, RethrowCall], NotRetryBranch),

    % Code to generate top level goals.
    template_if_exceptres_is_cons(Context,
        RollbackExceptionRttiVar, ExceptUnivVar, stm_rollback_retry_functor,
        RetryBranch, NotRetryBranch, IfRetryGoal, !NewPredInfo),

    create_plain_conj([DeconstructException, IfRetryGoal], ExceptionBranch),
    create_switch_disjunction(ReturnExceptVar,
        [case(exception_exception_functor, [], ExceptionBranch),
         case(exception_succeeded_functor, [], SuccessBranch)],
        detism_det, purity_impure, DisjGoal, !NewPredInfo),

    create_plain_conj([CreateNestedLogCall, ClosureAssign, TryStmCall,
        DisjGoal], Goal).

%-----------------------------------------------------------------------------%
%
% Utility predicates used in the creation of the rollback predicate and the
% wrapper predicate.
%

    % Returns the type of the value that is to be returned by the wrapper
    % predicate given the types of the output variables.
    %
:- pred make_return_type(list(mer_type)::in, mer_type::out) is det.

make_return_type(Types, ReturnType) :-
    (
        Types = [],
        ReturnType = stm_dummy_output_type
    ;
        Types = [ReturnType]
    ;
        Types = [_, _ | _],
        ReturnType = tuple_type(Types, kind_star)
    ).

    % Creates the goals necessary for extracting the output variables from
    % the return value of the wrapper.
    %
:- pred deconstruct_output(stm_goal_vars::in, mer_type::in, prog_var::in,
    hlds_goal::out, stm_info::in,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

deconstruct_output(AtomicGoalVars, ReturnType, ReturnExceptVar,
        Goal, StmInfo, !NewPredInfo) :-
    get_input_output_varlist(AtomicGoalVars, _, OutputVars),
    get_input_output_types(AtomicGoalVars, StmInfo, _, OutputTypes),

    (
        OutputTypes = [],
        % Extract the return type but do nothing with it. For reasons that
        % I do not know, this is the bare minimum that is required without
        % causing an exception in a later stage.
        create_aux_variable(ReturnType, yes("BoringResult"), SuccessResultVar,
            !NewPredInfo),
        deconstruct_functor(ReturnExceptVar, exception_succeeded_functor,
            [SuccessResultVar], Goal)
    ;
        OutputTypes = [_],
        % Wrapper returns a single value -- Simply get the value from the
        % exception result and return.
        OutVar = list.det_head(OutputVars),
        deconstruct_functor(ReturnExceptVar, exception_succeeded_functor,
            [OutVar], Goal)
    ;
        OutputTypes = [_, _ | _],
        % Wrapper returns a tuple. Get the tuple result and return it.
        make_type_info(ReturnType, _, MakeType, !NewPredInfo),
        create_aux_variable(ReturnType, yes("SuccessResult"), SuccessResultVar,
            !NewPredInfo),
        deconstruct_functor(ReturnExceptVar, exception_succeeded_functor,
            [SuccessResultVar], DeconstructGoal),
        deconstruct_tuple(SuccessResultVar, OutputVars, UnifyOutputGoal),

        create_plain_conj([DeconstructGoal, UnifyOutputGoal | MakeType],
            Goal)
    ).

    % Creates the goals necessary for constructing the output variables
    % in the wrapper predicate. It is necessary to compress all the output
    % values into a single variable to be passed along with the exception
    % result.
    %
:- pred construct_output(prog_context::in, stm_goal_vars::in,
    mer_type::in, prog_var::in, stm_info::in, list(hlds_goal)::out,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

construct_output(Context, AtomicGoalVars, ResultType, ResultVar, StmInfo,
        Goals, !NewPredInfo) :-
    get_input_output_varlist(AtomicGoalVars, _, OutputVars),
    get_input_output_types(AtomicGoalVars, StmInfo, _, OutputTypes),
    (
        OutputTypes = [],
        % Since a value must be returned, simply return a value which will be
        % discarded.
        make_const_construction(Context,
            ResultVar, stm_dummy_output_functor, Goal),
        Goals = [Goal]
    ;
        OutputTypes = [_],
        % Wrapper returns a single value -- Simply get the value from the
        % exception result and return.
        OutVar = list.det_head(OutputVars),
        create_var_unify(ResultVar, OutVar,
            unify_modes_li_lf_ri_rf(free, ground_inst,
                ground_inst, ground_inst),
            Goal, !NewPredInfo),
        Goals = [Goal]
    ;
        OutputTypes = [_, _ | _],
        % Wrapper returns a tuple. Creates a tuple from the output values.
        make_type_info(ResultType, _, MakeType, !NewPredInfo),
        construct_tuple(ResultVar, OutputVars, Goal),
        Goals = [Goal | MakeType]
    ).

    % Renames the value of a variable in a predicate.
    %
:- pred rename_var_in_wrapper_pred(string::in, prog_var::in, mer_type::in,
    prog_var::out, stm_new_pred_info::in, stm_new_pred_info::out,
    hlds_goal::in, hlds_goal::out) is det.

rename_var_in_wrapper_pred(Name, ResultVar0, ResultType, ResultVar,
        !NewPredInfo, !Goal) :-
    NewProcInfo0 = !.NewPredInfo ^ new_pred_proc_info,
    proc_info_get_varset_vartypes(NewProcInfo0,
        NewPredVarSet0, NewPredVarTypes0),
    proc_info_get_headvars(NewProcInfo0, NewHeadVars0),
    varset.delete_var(ResultVar0, NewPredVarSet0, NewPredVarSet1),
    delete_var_type(ResultVar0, NewPredVarTypes0, NewPredVarTypes1),

    varset.new_named_var(Name, ResultVar, NewPredVarSet1, NewPredVarSet),
    add_var_type(ResultVar, ResultType, NewPredVarTypes1, NewPredVarTypes),
    VarMapping = map.singleton(ResultVar0, ResultVar),

    MapLambda =
        ( pred(X::in, Y::out) is det :-
            ( if X = ResultVar0 then
                Y = ResultVar
            else
                Y = X
            )
        ),
    list.map(MapLambda, NewHeadVars0, NewHeadVars),

    rename_some_vars_in_goal(VarMapping, !Goal),
    proc_info_set_varset_vartypes(NewPredVarSet, NewPredVarTypes,
        NewProcInfo0, NewProcInfo1),
    proc_info_set_headvars(NewHeadVars, NewProcInfo1, NewProcInfo),
    !NewPredInfo ^ new_pred_proc_info := NewProcInfo.

%-----------------------------------------------------------------------------%
%
% Predicates to assist in the creation of hlds_goals. To simplify the creation
% of goals in a predicate, many of these functions thread the type
% "stm_new_pred_info" which contains, amonst other things, the predicate info,
% procedure info and module info of the newly created predicate.
%
% Many of the created goals create default instmap_deltas and non-local
% variable sets. This is because it is assumed that quantification and
% recalculation of the instmap_deltas will be done over the newly created
% predicate (the call to "run_quantification_over_pred" will do this).
%

    % Creates an auxiliary variable with a specific type
    %
:- pred create_aux_variable_stm(mer_type::in, maybe(string)::in, prog_var::out,
    stm_info::in, stm_info::out) is det.

create_aux_variable_stm(Type, MaybeName0, Var, !StmInfo) :-
    ProcInfo0 = !.StmInfo ^ stm_info_proc_info,
    (
        MaybeName0 = no,
        MaybeName0 = MaybeName
    ;
        MaybeName0 = yes(Name),
        MaybeName = yes(Name ++ "_Aux_STM")
    ),
    proc_info_create_var_from_type(Type, MaybeName, Var, ProcInfo0, ProcInfo),
    !StmInfo ^ stm_info_proc_info := ProcInfo.

    % Creates an auxiliary variable with a specific type
    %
:- pred create_aux_variable(mer_type::in, maybe(string)::in, prog_var::out,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_aux_variable(Type, MaybeName0, Var, !NewPredInfo) :-
    ProcInfo0 = !.NewPredInfo ^ new_pred_proc_info,
    Cnt0 = !.NewPredInfo ^ new_pred_var_cnt,
    (
        MaybeName0 = no,
        MaybeName0 = MaybeName
    ;
        MaybeName0 = yes(Name),
        MaybeName = yes(Name ++ "_Aux_" ++ string(Cnt0))
    ),
    proc_info_create_var_from_type(Type, MaybeName, Var, ProcInfo0, ProcInfo),
    Cnt = Cnt0 + 1,
    !NewPredInfo ^ new_pred_proc_info := ProcInfo,
    !NewPredInfo ^ new_pred_var_cnt := Cnt.

    % Creates a new auxiliary variable and a goal which assigns it to a
    % cons_id.
    %
:- pred create_aux_variable_assignment(prog_context::in,
    cons_id::in, mer_type::in, maybe(string)::in,
    hlds_goal::out, prog_var::out,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_aux_variable_assignment(Context, ConsId, Type, MaybeName, Goal, Var,
        !NewPredInfo) :-
    create_aux_variable(Type, MaybeName, Var, !NewPredInfo),
    make_const_construction(Context, Var, ConsId, Goal).

    % Creates a simple test between two variables (using the unify goal).
    %
:- pred create_var_test(prog_var::in, prog_var::in, unify_mode::in,
    hlds_goal::out, stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_var_test(VarLHS, VarRHS, UnifyMode, Goal, !NewPredInfo) :-
    Context = !.NewPredInfo ^ new_pred_context,
    ModuleInfo = !.NewPredInfo ^ new_pred_module_info,

    UnifyType = simple_test(VarLHS, VarRHS),
    UnifyRHS = rhs_var(VarRHS),
    UnifyContext = unify_context(umc_explicit, []),
    UnifyMode = unify_modes_li_lf_ri_rf(LHSInitInst, LHSFinalInst,
        RHSInitInst, RHSFinalInst),
    LHSTuple = var_init_final_insts(VarLHS, LHSInitInst, LHSFinalInst),
    RHSTuple = var_init_final_insts(VarRHS, RHSInitInst, RHSFinalInst),
    instmap_delta_from_var_init_final_insts(ModuleInfo, [LHSTuple, RHSTuple],
        InstmapDelta),
    GoalExpr = unify(VarLHS, UnifyRHS, UnifyMode, UnifyType, UnifyContext),

    set_of_var.list_to_set([VarLHS, VarRHS], NonLocals),
    Determism = detism_semi,
    Purity = purity_pure,
    goal_info_init(NonLocals, InstmapDelta, Determism, Purity, Context,
        GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

    % Creates a unification between two variables (using the unify goal)
    % Takes the "stm_info" state
    %
:- pred create_var_unify_stm(prog_var::in, prog_var::in, unify_mode::in,
    hlds_goal::out, stm_info::in, stm_info::out) is det.

create_var_unify_stm(VarLHS, VarRHS, UnifyMode, Goal, !StmInfo) :-
    Context = term.context("--temp-context--", 999),
    ModuleInfo = !.StmInfo ^ stm_info_module_info,

    UnifyType = assign(VarLHS, VarRHS),
    UnifyRHS = rhs_var(VarRHS),
    UnifyContext = unify_context(umc_explicit, []),
    UnifyMode = unify_modes_li_lf_ri_rf(LHSInitInst, LHSFinalInst,
        RHSInitInst, RHSFinalInst),
    LHSTuple = var_init_final_insts(VarLHS, LHSInitInst, LHSFinalInst),
    RHSTuple = var_init_final_insts(VarRHS, RHSInitInst, RHSFinalInst),
    instmap_delta_from_var_init_final_insts(ModuleInfo, [LHSTuple, RHSTuple],
        InstmapDelta),
    GoalExpr = unify(VarLHS, UnifyRHS, UnifyMode, UnifyType, UnifyContext),

    set_of_var.list_to_set([VarLHS, VarRHS], NonLocals),
    Determism = detism_det,
    Purity = purity_pure,
    goal_info_init(NonLocals, InstmapDelta, Determism, Purity, Context,
        GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

    % Creates a unification between two variables (using the unify goal)
    %
:- pred create_var_unify(prog_var::in, prog_var::in, unify_mode::in,
    hlds_goal::out, stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_var_unify(VarLHS, VarRHS, UnifyMode, Goal, !NewPredInfo) :-
    Context = !.NewPredInfo ^ new_pred_context,
    ModuleInfo = !.NewPredInfo ^ new_pred_module_info,

    UnifyType = assign(VarLHS, VarRHS),
    UnifyRHS = rhs_var(VarRHS),
    UnifyContext = unify_context(umc_explicit, []),
    UnifyMode = unify_modes_li_lf_ri_rf(LHSInitInst, LHSFinalInst,
        RHSInitInst, RHSFinalInst),
    LHSTuple = var_init_final_insts(VarLHS, LHSInitInst, LHSFinalInst),
    RHSTuple = var_init_final_insts(VarRHS, RHSInitInst, RHSFinalInst),
    instmap_delta_from_var_init_final_insts(ModuleInfo, [LHSTuple, RHSTuple],
        InstmapDelta),
    GoalExpr = unify(VarLHS, UnifyRHS, UnifyMode, UnifyType, UnifyContext),

    set_of_var.list_to_set([VarLHS, VarRHS], NonLocals),
    Determism = detism_det,
    Purity = purity_pure,
    goal_info_init(NonLocals, InstmapDelta, Determism, Purity, Context,
        GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

    % Creates a simple call. If the call is polymorphic, remember to add
    % the runtime type information as well ("type_info" variable).
    %
:- pred create_simple_call(module_name::in, string::in, pred_or_func::in,
    list(prog_var)::in, list(prog_var)::in, instmap_delta::in,
    mode_no::in, determinism::in, purity::in, list(goal_feature)::in,
    hlds_goal::out, stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_simple_call(ModuleName, ProcName, PredOrFunc, TIArgVars, NonTIArgVars,
        InstmapDelta, Mode, Detism, Purity, GoalFeatures, Goal,
        !NewPredInfo) :-
    Context = !.NewPredInfo ^ new_pred_context,
    ModuleInfo = !.NewPredInfo ^ new_pred_module_info,
    generate_plain_call(ModuleInfo, PredOrFunc, ModuleName, ProcName,
        TIArgVars, NonTIArgVars, InstmapDelta, Mode,
        Detism, Purity, GoalFeatures, Context, Goal).

    % Creates a closure for a predicate.
    %
:- pred create_closure(pred_proc_id::in, list(prog_var)::in,
    list(mer_type)::in, list(mer_mode)::in, prog_var::out, hlds_goal::out,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_closure(PredProcID, Args, ArgTypes, ArgModes, ClosureVar,
        ClosureAssignGoal, !NewPredInfo) :-
    ShroudPredProcID = shroud_pred_proc_id(PredProcID),
    construct_higher_order_pred_type(purity_pure, lambda_normal, ArgTypes,
        ClosureType),
    ClosureCons = closure_cons(ShroudPredProcID, lambda_normal),
    create_aux_variable(ClosureType, yes("Closure"), ClosureVar, !NewPredInfo),
    construct_functor(ClosureVar, ClosureCons, Args, ClosureAssignGoal0),

    ClosureAssignInstmapDeltaList = assoc_list.from_corresponding_lists(
        [ClosureVar], [ground(shared, higher_order(pred_inst_info(
        pf_predicate, ArgModes, arg_reg_types_unset, detism_det)))]),
    ClosureAssignInstmapDelta =
        instmap_delta_from_assoc_list(ClosureAssignInstmapDeltaList),

    ClosureAssignGoal0 = hlds_goal(ClosureAssignExpr, ClosureAssignInfo0),
    goal_info_set_instmap_delta(ClosureAssignInstmapDelta, ClosureAssignInfo0,
        ClosureAssignInfo),
    ClosureAssignGoal = hlds_goal(ClosureAssignExpr, ClosureAssignInfo).

    % Creates an if-then-else goal.
    %
:- pred create_if_then_else(list(prog_var)::in, hlds_goal::in, hlds_goal::in,
    hlds_goal::in, determinism::in, purity::in, hlds_goal::out,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_if_then_else(ExistVars, Cond, Then, Else, Detism, Purity, OutGoal,
        !NewPredInfo) :-
    Context = !.NewPredInfo ^ new_pred_context,
    OutGoalExpr = if_then_else(ExistVars, Cond, Then, Else),
    NonLocals = set_of_var.init,
    instmap_delta_init_reachable(InstMapDelta),
    goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context,
        GoalInfo),
    OutGoal = hlds_goal(OutGoalExpr, GoalInfo).

    % Creates a switch goal.
    %
:- pred create_switch_disjunction(prog_var::in, list(case)::in,
    determinism::in, purity::in, hlds_goal::out,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

create_switch_disjunction(ProgVar, Cases, Detism, Purity, OutGoal,
        !NewPredInfo) :-
    Context = !.NewPredInfo ^ new_pred_context,
    NonLocals = set_of_var.init,
    instmap_delta_init_reachable(InstMapDelta),
    OutGoalExpr = switch(ProgVar, cannot_fail, Cases),
    goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context,
        GoalInfo),
    OutGoal = hlds_goal(OutGoalExpr, GoalInfo).

    % Creates a promise_purity around a goal for a given purity.
    %
:- pred create_promise_purity_scope(hlds_goal::in, purity::in,
    hlds_goal::out) is det.

create_promise_purity_scope(GoalIn, ScopePurity, GoalOut) :-
    GoalIn = hlds_goal(_, GoalInInfo),
    NonLocals = goal_info_get_nonlocals(GoalInInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInInfo),
    Detism = goal_info_get_determinism(GoalInInfo),
    GoalPurity = ScopePurity,
    Context = goal_info_get_context(GoalInInfo),
    goal_info_init(NonLocals, InstMapDelta, Detism, GoalPurity, Context,
        GoalInfo),
    Reason = promise_purity(ScopePurity),
    GoalOutExpr = scope(Reason, GoalIn),
    GoalOut = hlds_goal(GoalOutExpr, GoalInfo).

    % Creates a list of regular conjoined goals.
    %
:- pred create_plain_conj(list(hlds_goal)::in, hlds_goal::out) is det.

create_plain_conj(GoalsInConj, ConjGoal) :-
    Type = plain_conj,
    ConjGoalExpr = conj(Type, GoalsInConj),
    goal_list_nonlocals(GoalsInConj, NonLocals),
    goal_list_instmap_delta(GoalsInConj, InstMapDelta),
    goal_list_determinism(GoalsInConj, Detism),
    goal_list_purity(GoalsInConj, Purity),
    GoalAInfo = list.det_head(GoalsInConj) ^ hg_info,
    Context = goal_info_get_context(GoalAInfo),
    goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context,
        ConjGoalInfo),
    ConjGoal = hlds_goal(ConjGoalExpr, ConjGoalInfo).

    % Create typeinfo for use in polymorphic predicates
    %
:- pred make_type_info(mer_type::in, prog_var::out, list(hlds_goal)::out,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

make_type_info(Type, TypeInfoVar, Goals, NewPredInfo0, NewPredInfo) :-
    NewPredInfo0 = stm_new_pred_info(ModuleInfo0, PredId, ProcId,
        PredInfo0, ProcInfo0, Context, VarCnt),
    polymorphism_make_type_info_var_raw(Type, Context, TypeInfoVar, Goals,
        ModuleInfo0, ModuleInfo, PredInfo0, PredInfo, ProcInfo0, ProcInfo),
    NewPredInfo = stm_new_pred_info(ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo, Context, VarCnt).

%-----------------------------------------------------------------------------%
%
% Predicates to assist in the creation of new predicates.
%

    % Creates a new predicate. The head variables, head variable types, head
    % variable modes, name and goal of the new predicate are set from the
    % arguments. All other properties are copied from the predicate in which
    % the original atomic goal appears in. The predicate returns a
    % "stm_new_pred_info" value (so that the body of the predicate can be
    % built) as well as a call to the new predicate.
    %
:- pred create_cloned_pred(list(prog_var)::in, list(mer_type)::in,
    list(mer_mode)::in, stm_clone_kind::in, hlds_goal::in,
    maybe(determinism)::in, stm_new_pred_info::out, hlds_goal::out,
    stm_info::in, stm_info::out) is det.

create_cloned_pred(ProcHeadVars, PredArgTypes, ProcHeadModes, CloneKind,
        OrigGoal, MaybeDetism, NewStmPredInfo, CallGoal, !StmInfo) :-
    ModuleInfo0 = !.StmInfo ^ stm_info_module_info,
    PredInfo = !.StmInfo ^ stm_info_pred_info,
    PredId = !.StmInfo ^ stm_info_pred_id,
    ProcId = !.StmInfo ^ stm_info_proc_id,
    ExpansionCnt0 = !.StmInfo ^ stm_info_expand_id,

    list.length(ProcHeadVars, Arity),
    OrigGoal = hlds_goal(_, GoalInfo0),

    pred_info_proc_info(PredInfo, ProcId, ProcInfo),
    proc_info_get_context(ProcInfo, ProcContext),
    proc_info_get_varset_vartypes(ProcInfo, ProcVarSet, ProcVarTypes),
    proc_info_get_inst_varset(ProcInfo, ProcInstVarSet),
    (
        MaybeDetism = yes(ProcDetism)
    ;
        MaybeDetism = no,
        proc_info_get_inferred_determinism(ProcInfo, ProcDetism)
    ),
    proc_info_get_goal(ProcInfo, ProcGoal),
    proc_info_get_rtti_varmaps(ProcInfo, ProcRttiVarMaps),
    proc_info_get_has_parallel_conj(ProcInfo, HasParallelConj),
    proc_info_get_var_name_remap(ProcInfo, VarNameRemap),
    SeqNum = item_no_seq_num,
    proc_info_create(ProcContext, SeqNum,
        ProcVarSet, ProcVarTypes, ProcHeadVars,
        ProcInstVarSet, ProcHeadModes, detism_decl_none, ProcDetism,
        ProcGoal, ProcRttiVarMaps, address_is_not_taken, HasParallelConj,
        VarNameRemap, NewProcInfo),
    ModuleName = pred_info_module(PredInfo),
    OrigPredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_context(PredInfo, PredContext),

    Transform = tn_stm_expanded(PredOrFunc, CloneKind, Arity, 
        pred_id_to_int(PredId), ExpansionCnt0),
    make_transformed_pred_name(OrigPredName, Transform, NewPredName),

    pred_info_get_origin(PredInfo, OrigPredOrigin),
    NewPredOrigin = origin_transformed(transform_stm_expansion,
        OrigPredOrigin, PredId),

    pred_info_get_typevarset(PredInfo, PredTypeVarSet),
    pred_info_get_exist_quant_tvars(PredInfo, PredExistQVars),
    pred_info_get_class_context(PredInfo, PredClassContext),
    pred_info_get_assertions(PredInfo, PredAssertions),
    pred_info_get_markers(PredInfo, Markers),
    GoalType = goal_not_for_promise(np_goal_type_none),
    pred_info_create(PredOrFunc, ModuleName, NewPredName, PredContext,
        NewPredOrigin, pred_status(status_local), Markers, PredArgTypes,
        PredTypeVarSet, PredExistQVars, PredClassContext, PredAssertions,
        VarNameRemap, GoalType, NewProcInfo, NewProcId, NewPredInfo),

    module_info_get_predicate_table(ModuleInfo0, PredicateTable0),
    predicate_table_insert(NewPredInfo, NewPredId,
        PredicateTable0, PredicateTable),
    module_info_set_predicate_table(PredicateTable, ModuleInfo0, ModuleInfo),
    CallExpr = plain_call(NewPredId, NewProcId, ProcHeadVars, not_builtin, no,
        qualified(ModuleName, NewPredName)),

    set_of_var.list_to_set(ProcHeadVars, CallNonLocals),
    instmap_delta_from_mode_list(ModuleInfo0, ProcHeadVars, ProcHeadModes,
        CallInstmapDelta),

    CallDeterminism = ProcDetism,
    CallPurity = goal_info_get_purity(GoalInfo0),
    CallContext = goal_info_get_context(GoalInfo0),

    goal_info_init(CallNonLocals, CallInstmapDelta, CallDeterminism,
        CallPurity, CallContext, GoalInfo),
    CallGoal = hlds_goal(CallExpr, GoalInfo),

    ExpansionCnt = ExpansionCnt0 + 1,
    !StmInfo ^ stm_info_expand_id := ExpansionCnt,
    !StmInfo ^ stm_info_module_info := ModuleInfo,
    NewStmPredInfo = stm_new_pred_info(ModuleInfo, NewPredId, NewProcId,
       NewPredInfo, NewProcInfo, CallContext, 0).

    % Sets the head variables of the new predicate.
    %
:- pred set_head_vars(list(prog_var)::in,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

set_head_vars(NewHeadVars, !NewPredInfo) :-
    ProcInfo0 = !.NewPredInfo ^ new_pred_proc_info,
    proc_info_set_headvars(NewHeadVars, ProcInfo0, ProcInfo),
    !NewPredInfo ^ new_pred_proc_info := ProcInfo.

    % Writes the changes made to the new predicate to the predicate table
    % and returns an updates the stm_info state.
    %
:- pred commit_new_pred(stm_new_pred_info::in,
    stm_info::in, stm_info::out) is det.

commit_new_pred(NewPred, StmInfo0, StmInfo) :-
    StmInfo0 = stm_info(_StmModuleInfo0, OrigPredId, OrigProcId, OrigProcInfo,
        OrigPredInfo, StmExpanded, ExpandNum),

    NewPred = stm_new_pred_info(PredModuleInfo0, NewPredId, NewProcId,
        NewPredInfo, NewProcInfo, _, _),
    module_info_set_pred_proc_info(NewPredId, NewProcId, NewPredInfo,
        NewProcInfo, PredModuleInfo0, PredModuleInfo),
    StmInfo = stm_info(PredModuleInfo, OrigPredId, OrigProcId, OrigProcInfo,
        OrigPredInfo, StmExpanded, ExpandNum).

    % If changes have been made to the stm_info type (specifically the
    % module_info), update these changes in stm_new_pred_info.
    %
:- pred update_new_pred_info(stm_info::in,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

update_new_pred_info(StmInfo, !NewPredInfo) :-
    ModuleInfo = StmInfo ^ stm_info_module_info,
    !NewPredInfo ^ new_pred_module_info := ModuleInfo.

    % Runs quantification and recalculates the instmap-delta over the
    % new predicate.
    %
:- pred run_quantification_over_pred(
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

run_quantification_over_pred(!NewPredInfo) :-
    ProcInfo0 = !.NewPredInfo ^ new_pred_proc_info,
    ModuleInfo0 = !.NewPredInfo ^ new_pred_module_info,
    requantify_proc_general(ordinary_nonlocals_no_lambda,
        ProcInfo0, ProcInfo1),
    recompute_instmap_delta_proc(recompute_atomic_instmap_deltas,
        ProcInfo1, ProcInfo, ModuleInfo0, ModuleInfo),
    !NewPredInfo ^ new_pred_module_info := ModuleInfo,
    !NewPredInfo ^ new_pred_proc_info := ProcInfo.

    % Sets the goal of the new predicate.
    %
:- pred new_pred_set_goal(hlds_goal::in,
    stm_new_pred_info::in, stm_new_pred_info::out) is det.

new_pred_set_goal(Goal, !NewPredInfo) :-
    ProcInfo0 = !.NewPredInfo ^ new_pred_proc_info,
    goal_vars(Goal, GoalVars),
    GoalVarsSet = set_of_var.bitset_to_set(GoalVars),
    proc_info_get_varset_vartypes(ProcInfo0, ProcVarSet0, ProcVarTypes0),

    varset.select(GoalVarsSet, ProcVarSet0, ProgVarSet),
    vartypes_select(GoalVarsSet, ProcVarTypes0, ProcVarTypes),

    proc_info_set_varset_vartypes(ProgVarSet, ProcVarTypes,
        ProcInfo0, ProcInfo1),
    proc_info_set_goal(Goal, ProcInfo1, ProcInfo),
    !NewPredInfo ^ new_pred_proc_info := ProcInfo.

    % Returns the pred_proc_id of the new predicate.
    %
:- pred get_pred_proc_id(stm_new_pred_info::in, pred_proc_id::out) is det.

get_pred_proc_id(NewPredInfo0, PredProcId) :-
    PredId = NewPredInfo0 ^ new_pred_pred_id,
    ProcId = NewPredInfo0 ^ new_pred_proc_id,
    PredProcId = proc(PredId, ProcId).

%-----------------------------------------------------------------------------%
%
% Predicates related to the goal variables.
%

    % Get the list of input and output variables of the original atomic goal.
    %
:- pred get_input_output_varlist(stm_goal_vars::in,
    list(prog_var)::out, list(prog_var)::out) is det.

get_input_output_varlist(StmGoalVars, Input, Output) :-
    InputSet = StmGoalVars ^ vars_input,
    OutputSet = StmGoalVars ^ vars_output,

    Input = set_of_var.to_sorted_list(InputSet),
    Output = set_of_var.to_sorted_list(OutputSet).

    % Get the list of types corresponding to the input and output
    % variables of the original atomic goal.
    %
:- pred get_input_output_types(stm_goal_vars::in, stm_info::in,
    list(mer_type)::out, list(mer_type)::out) is det.

get_input_output_types(StmGoalVars, StmInfo, InputTypes, OutputTypes) :-
    ProcInfo0 = StmInfo ^ stm_info_proc_info,
    proc_info_get_varset_vartypes(ProcInfo0, _VarSet, VarTypes),
    get_input_output_varlist(StmGoalVars, InputVars, OutputVars),

    lookup_var_types(VarTypes, InputVars, InputTypes),
    lookup_var_types(VarTypes, OutputVars, OutputTypes).

    % Used by "get_input_output_modes".
    %
:- pred set_list_val(X::in, Y::in, X::out) is det.

set_list_val(X, _, X).

    % Get the list of modes corresponding to the input and output
    % variables of the original atomic goal. Input variables will have
    % the mode "in" while output variables will have the mode "out".
    %
:- pred get_input_output_modes(stm_goal_vars::in,
    list(mer_mode)::out, list(mer_mode)::out) is det.

get_input_output_modes(StmGoalVars, InputModes, OutputModes) :-
    get_input_output_varlist(StmGoalVars, InputVars, OutputVars),
    list.map(set_list_val(in_mode), InputVars, InputModes),
    list.map(set_list_val(out_mode), OutputVars, OutputModes).

%-----------------------------------------------------------------------------%

    % Special (dummy) predicate names.
    %
:- func stm_inner_outer = sym_name.
:- func stm_outer_inner = sym_name.

stm_inner_outer =
    qualified(mercury_stm_builtin_module, "stm_from_inner_to_outer").
stm_outer_inner =
    qualified(mercury_stm_builtin_module, "stm_from_outer_to_inner").

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.stm_expand.
%-----------------------------------------------------------------------------%
