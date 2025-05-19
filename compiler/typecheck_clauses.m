%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2021, 2023-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_clauses.m.
% Main author: fjh.
%
% This file contains the part of the Mercury type-checker
% that checks the definition of a single predicate or function.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_clauses.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_info.
:- import_module hlds.
:- import_module hlds.hlds_clauses.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % Typecheck over the list of clauses for a predicate.
    %
:- pred typecheck_clauses(list(prog_var)::in, list(mer_type)::in,
    list(clause)::in, list(clause)::out,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

%---------------------------------------------------------------------------%

:- type stuff_to_check
    --->    clause_only
    ;       whole_pred.

    % If there are multiple type assignments, then issue an error message.
    %
    % If stuff-to-check = whole_pred, report an error for any ambiguity,
    % and also check for unbound type variables.
    % But if stuff-to-check = clause_only, then only report errors
    % for type ambiguities that don't involve the head vars, because
    % we may be able to resolve a type ambiguity for a head var in one clause
    % by looking at later clauses. (Ambiguities in the head variables
    % can only arise if we are inferring the type for this pred.)
    %
:- pred typecheck_check_for_ambiguity(prog_context::in, stuff_to_check::in,
    list(prog_var)::in, type_assign_set::in,
    typecheck_info::in, typecheck_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module check_hlds.typecheck_coerce.
:- import_module check_hlds.typecheck_debug.
:- import_module check_hlds.typecheck_error_arg_vector.
:- import_module check_hlds.typecheck_error_overload.
:- import_module check_hlds.typecheck_error_undef.
:- import_module check_hlds.typecheck_error_unify.
:- import_module check_hlds.typecheck_error_util.
:- import_module check_hlds.typecheck_error_wrong_type.
:- import_module check_hlds.typecheck_unify_var_functor.
:- import_module check_hlds.typecheck_util.
:- import_module check_hlds.typeclasses.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data_event.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_event.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_construct.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_unify.
:- import_module parse_tree.vartypes.

:- import_module assoc_list.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

typecheck_clauses(HeadVars, ArgTypes, Clauses0, Clauses,
        !TypeAssignSet, !Info) :-
    typecheck_clauses_loop(HeadVars, ArgTypes, Clauses0, [], RevClauses,
        !TypeAssignSet, !Info),
    list.reverse(RevClauses, Clauses).

    % Typecheck over the list of clauses for a predicate.
    %
:- pred typecheck_clauses_loop(list(prog_var)::in, list(mer_type)::in,
    list(clause)::in, list(clause)::in, list(clause)::out,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_clauses_loop(_, _, [], !RevClauses, !TypeAssignSet, !Info).
typecheck_clauses_loop(HeadVars, ArgTypes, [Clause0 | Clauses0], !RevClauses,
        !TypeAssignSet, !Info) :-
    typecheck_clause(HeadVars, ArgTypes, Clause0, Clause,
        !TypeAssignSet, !Info),
    !:RevClauses = [Clause | !.RevClauses],
    typecheck_clauses_loop(HeadVars, ArgTypes, Clauses0, !RevClauses,
        !TypeAssignSet, !Info).

%---------------------------------------------------------------------------%

    % Type-check a single clause.
    %
    % As we go through a clause, we determine the set of possible type
    % assignments for the clause. A type assignment is an assignment of a type
    % to each variable in the clause.
    %
    % Note that this may have exponential complexity for both time and space.
    % If there are n variables Vi (for i in 1..n) that may each have either
    % type Ti1 or Ti2, then we generate 2^n type assignments to represent all
    % the possible combinations of their types. This can easily be a serious
    % problem for even medium-sized predicates that extensively use function
    % symbols that belong to more than one type (such as `no', which belongs
    % to both `bool' and `maybe').
    %
    % The pragmatic short-term solution we apply here is to generate a warning
    % when the number of type assignments exceeds one bound (given by the value
    % of the typecheck_ambiguity_warn_limit option), and an error when it
    % exceeds another, higher bound (given by typecheck_ambiguity_error_limit).
    %
    % The better but more long-term solution is to switch to using
    % a constraint based type checker, which does not need to materialize
    % the cross product of all the possible type assignments of different
    % variables in a clause. The module type_constraints.m contains
    % an incomplete prototype of such a type checker.
    %
:- pred typecheck_clause(list(prog_var)::in, list(mer_type)::in,
    clause::in, clause::out, type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_clause(HeadVars, ArgTypes, !Clause, !TypeAssignSet, !Info) :-
    !.Clause = clause(_, Body0, _, Context, _, _, _),

    % Typecheck the clause - first the head unification, and then the body.
    ArgVectorKind = arg_vector_clause_head,
    typecheck_vars_have_types(ArgVectorKind, Context, HeadVars, ArgTypes,
        !TypeAssignSet, !Info),
    typecheck_goal(Body0, Body, Context, !TypeAssignSet, !Info),
    trace [compiletime(flag("type_checkpoint")), io(!IO)] (
        typecheck_info_get_error_clause_context(!.Info, ClauseContext),
        VarSet = ClauseContext ^ tecc_varset,
        type_checkpoint("end of clause", !.Info, VarSet, !.TypeAssignSet, !IO)
    ),
    typecheck_prune_coerce_constraints(!.Info, !TypeAssignSet),
    !Clause ^ clause_body := Body,
    typecheck_check_for_ambiguity(Context, clause_only, HeadVars,
        !.TypeAssignSet, !Info).
    % We should perhaps do manual garbage collection here.

%---------------------------------------------------------------------------%

    % Typecheck a goal.
    %
:- pred typecheck_goal(hlds_goal::in, hlds_goal::out, prog_context::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_goal(Goal0, Goal, EnclosingContext, !TypeAssignSet, !Info) :-
    % If the context of the goal is empty, we set the context of the goal
    % to the surrounding context. (That should probably be done in make_hlds,
    % but it was easier to do here.)
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    Context0 = goal_info_get_context(GoalInfo0),
    ( if is_dummy_context(Context0) then
        Context = EnclosingContext,
        goal_info_set_context(Context, GoalInfo0, GoalInfo)
    else
        Context = Context0,
        GoalInfo = GoalInfo0
    ),

    % Our algorithm handles overloading quite inefficiently: for each
    % unification of a variable with a function symbol that matches N type
    % declarations, we make N copies of the existing set of type assignments.
    % The consequence is that the worst case complexity of our algorithm,
    % is exponential in the number of ambiguous symbols. Unfortunately,
    % this is true for space complexity as well as time complexity,
    %
    % We issue a warning whenever the number of type assignments exceeds
    % the warn limit, and stop typechecking (after generating an error)
    % whenever it exceeds the error limit.

    list.length(!.TypeAssignSet, NumTypeAssignSets),
    typecheck_info_get_ambiguity_warn_limit(!.Info, WarnLimit),
    ( if NumTypeAssignSets > WarnLimit then
        typecheck_info_get_ambiguity_error_limit(!.Info, ErrorLimit),
        typecheck_info_get_error_clause_context(!.Info, ClauseContext),
        typecheck_info_get_overloaded_symbol_map(!.Info, OverloadedSymbolMap),
        ( if NumTypeAssignSets > ErrorLimit then
            % Override any existing overload warning.
            ErrorSpec = report_error_too_much_overloading(ClauseContext,
                Context, OverloadedSymbolMap),
            typecheck_info_set_overload_error(yes(ErrorSpec), !Info),

            % Don't call typecheck_goal_expr to do the actual typechecking,
            % since it will almost certainly take too much time and memory.
            GoalExpr = GoalExpr0
        else
            typecheck_info_get_overload_error(!.Info, MaybePrevSpec),
            (
                MaybePrevSpec = no,
                WarnSpec = report_warning_too_much_overloading(ClauseContext,
                    Context, OverloadedSymbolMap),
                typecheck_info_set_overload_error(yes(WarnSpec), !Info)
            ;
                MaybePrevSpec = yes(_)
            ),
            typecheck_goal_expr(GoalExpr0, GoalExpr, GoalInfo,
                !TypeAssignSet, !Info)
        )
    else
        typecheck_goal_expr(GoalExpr0, GoalExpr, GoalInfo,
            !TypeAssignSet, !Info)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred typecheck_goal_expr(hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_goal_expr(GoalExpr0, GoalExpr, GoalInfo, !TypeAssignSet, !Info) :-
    typecheck_info_get_error_clause_context(!.Info, ClauseContext),
    VarSet = ClauseContext ^ tecc_varset,
    Context = goal_info_get_context(GoalInfo),
    (
        GoalExpr0 = conj(ConjType, SubGoals0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("conj", !.Info, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_goal_list(SubGoals0, SubGoals, Context,
            !TypeAssignSet, !Info),
        GoalExpr = conj(ConjType, SubGoals)
    ;
        GoalExpr0 = disj(SubGoals0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("disj", !.Info, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_goal_list(SubGoals0, SubGoals, Context,
            !TypeAssignSet, !Info),
        GoalExpr = disj(SubGoals)
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        % We have not run switch detection yet, so there can be no switches
        % in user-written goals yet. However, the compiler can create clauses
        % containing switches, and unify_proc.m now does just that for
        % type-constructor-specific comparison predicates.
        %
        % In these switches, all of the main and other cons_ids in the cases
        % have the form cons/3, and all have the type_ctor field of cons/3
        % filled in with the same valid type_ctor, which is the type
        % of SwitchVar. We *could* add code here to get this type_ctor
        % out of the cons_ids in Cases0, and record that the top level
        % type constructor of SwitchVar's type is this type_ctor,
        % but SwitchVar will be one the predicate's arguments, and this
        % argument will have a declared type, so the typechecker will
        % *already* know this.
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("switch", !.Info, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_case_list(Cases0, Cases, Context, !TypeAssignSet, !Info),
        GoalExpr = switch(SwitchVar, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("if", !.Info, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_goal(Cond0, Cond, Context, !TypeAssignSet, !Info),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("then", !.Info, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_goal(Then0, Then, Context, !TypeAssignSet, !Info),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("else", !.Info, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_goal(Else0, Else, Context, !TypeAssignSet, !Info),
        ensure_vars_have_a_type(var_vector_cond_quant, Context, Vars,
            !TypeAssignSet, !Info),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(SubGoal0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("not", !.Info, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_goal(SubGoal0, SubGoal, Context, !TypeAssignSet, !Info),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("scope", !.Info, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_goal(SubGoal0, SubGoal, Context, !TypeAssignSet, !Info),
        (
            (
                (
                    Reason = exist_quant(Vars, _),
                    VarVectorKind = var_vector_exist_quant
                ;
                    Reason = promise_solutions(Vars, _),
                    VarVectorKind = var_vector_promise_solutions
                )
            ;
                Reason = require_complete_switch(Var),
                Vars = [Var],
                VarVectorKind = var_vector_switch_complete
            ;
                Reason = require_switch_arms_detism(Var, _),
                Vars = [Var],
                VarVectorKind = var_vector_switch_arm_detism
            ;
                % These variables are introduced by the compiler and may
                % only have a single, specific type.
                Reason = loop_control(LCVar, LCSVar, _),
                Vars = [LCVar, LCSVar],
                VarVectorKind = var_vector_loop_control
            ),
            ensure_vars_have_a_type(VarVectorKind, Context, Vars,
                !TypeAssignSet, !Info)
        ;
            ( Reason = disable_warnings(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = from_ground_term(_, _)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            )
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = plain_call(_, ProcId, ArgVars, BI, UC, SymName),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("call", !.Info, VarSet, !.TypeAssignSet, !IO)
        ),
        GoalId = goal_info_get_goal_id(GoalInfo),
        typecheck_plain_call(SymName, Context, GoalId, ArgVars,
            PredId, !TypeAssignSet, !Info),
        GoalExpr = plain_call(PredId, ProcId, ArgVars, BI, UC, SymName)
    ;
        GoalExpr0 = call_foreign_proc(_, PredId, _, Args, _, _, _),
        % Foreign_procs are automatically generated, so they will always be
        % type-correct, but we need to do the type analysis in order to
        % correctly compute the HeadTypeParams that result from existentially
        % typed foreign_procs. (We could probably do that more efficiently
        % than the way it is done below, though.)
        ArgVectorKind = arg_vector_foreign_proc_call(PredId),
        ArgVars = list.map(foreign_arg_var, Args),
        GoalId = goal_info_get_goal_id(GoalInfo),
        typecheck_plain_or_foreign_call_pred_id(ArgVectorKind, Context, GoalId,
            PredId, ArgVars, !TypeAssignSet, !Info),
        perform_context_reduction(Context, !TypeAssignSet, !Info),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = generic_call(GenericCall, ArgVars, _Modes, _MaybeArgRegs,
            _Detism),
        (
            GenericCall = higher_order(PredVar, Purity, _, _, _),
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("higher-order call", !.Info, VarSet,
                    !.TypeAssignSet, !IO)
            ),
            typecheck_higher_order_call(GenericCall, Context,
                PredVar, Purity, ArgVars, !TypeAssignSet, !Info)
        ;
            GenericCall = class_method(_, _, _, _),
            unexpected($pred, "unexpected class method call")
        ;
            GenericCall = event_call(EventName),
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("event call", !.Info, VarSet,
                    !.TypeAssignSet, !IO)
            ),
            typecheck_event_call(Context, EventName, ArgVars,
                !TypeAssignSet, !Info)
        ;
            GenericCall = cast(CastType),
            (
                ( CastType = unsafe_type_cast
                ; CastType = unsafe_type_inst_cast
                ; CastType = equiv_type_cast
                ; CastType = exists_cast
                )
                % A cast imposes no restrictions on its argument types,
                % so nothing needs to be done here.
            ;
                CastType = subtype_coerce,
                trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                    type_checkpoint("coerce", !.Info, VarSet,
                        !.TypeAssignSet, !IO)
                ),
                typecheck_coerce(!.Info, Context, ArgVars, !TypeAssignSet)
            )
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = unify(LHS, RHS0, UnifyMode, Unification, UnifyContext),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("unify", !.Info, VarSet, !.TypeAssignSet, !IO)
        ),
        GoalId = goal_info_get_goal_id(GoalInfo),
        typecheck_unification(UnifyContext, Context, GoalId,
            LHS, RHS0, RHS, !TypeAssignSet, !Info),
        GoalExpr = unify(LHS, RHS, UnifyMode, Unification, UnifyContext)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = bi_implication(LHS0, RHS0),
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("<=>", !.Info, VarSet, !.TypeAssignSet, !IO)
            ),
            typecheck_goal(LHS0, LHS, Context, !TypeAssignSet, !Info),
            typecheck_goal(RHS0, RHS, Context, !TypeAssignSet, !Info),
            ShortHand = bi_implication(LHS, RHS)
        ;
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("atomic_goal", !.Info, VarSet,
                    !.TypeAssignSet, !IO)
            ),
            (
                MaybeOutputVars = yes(OutputVars),
                VarVectorKindOutput = var_vector_atomic_output,
                ensure_vars_have_a_type(VarVectorKindOutput, Context,
                    OutputVars, !TypeAssignSet, !Info)
            ;
                MaybeOutputVars = no
            ),

            typecheck_goal(MainGoal0, MainGoal, Context,
                !TypeAssignSet, !Info),
            typecheck_goal_list(OrElseGoals0, OrElseGoals, Context,
                !TypeAssignSet, !Info),

            VarVectorKindOuter = var_vector_atomic_outer,
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            ensure_vars_have_a_single_type(VarVectorKindOuter, Context,
                [OuterDI, OuterUO], !TypeAssignSet, !Info),

            % The outer variables must either be both I/O states or STM states.
            % Checking that here could double the number of type assign sets.
            % We therefore delay the check until after we have typechecked
            % the predicate body, in post_typecheck. The code in the
            % post_typecheck pass (actually in purity.m) will do this
            % if the GoalType is unknown_atomic_goal_type.
            InnerVars =
                atomic_interface_list_to_var_list([Inner | OrElseInners]),
            list.foldl2(typecheck_var_has_stm_atomic_type(Context),
                InnerVars, !TypeAssignSet, !Info),
            expect(unify(GoalType, unknown_atomic_goal_type), $pred,
                "GoalType != unknown_atomic_goal_type"),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("try_goal", !.Info, VarSet,
                    !.TypeAssignSet, !IO)
            ),
            typecheck_goal(SubGoal0, SubGoal, Context, !TypeAssignSet, !Info),
            (
                MaybeIO = yes(try_io_state_vars(InitialIO, FinalIO)),
                VarVectorKind = var_vector_try_io,
                ensure_vars_have_a_type(VarVectorKind, Context,
                    [InitialIO, FinalIO], !TypeAssignSet, !Info),
                InitialGoalContext =
                    type_error_in_var_vector(VarVectorKind, 1),
                FinalGoalContext =
                    type_error_in_var_vector(VarVectorKind, 2),
                typecheck_var_has_type(InitialGoalContext, Context,
                    InitialIO, io_state_type, !TypeAssignSet, !Info),
                typecheck_var_has_type(FinalGoalContext, Context,
                    FinalIO, io_state_type, !TypeAssignSet, !Info)
            ;
                MaybeIO = no
            ),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ),
        GoalExpr = shorthand(ShortHand)
    ).

:- func atomic_interface_list_to_var_list(list(atomic_interface_vars)) =
    list(prog_var).

atomic_interface_list_to_var_list([]) = [].
atomic_interface_list_to_var_list([atomic_interface_vars(I, O) | Interfaces]) =
    [I, O | atomic_interface_list_to_var_list(Interfaces)].

%---------------------------------------------------------------------------%

:- pred typecheck_goal_list(list(hlds_goal)::in, list(hlds_goal)::out,
    prog_context::in, type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_goal_list([], [], _, !TypeAssignSet, !Info).
typecheck_goal_list([Goal0 | Goals0], [Goal | Goals], Context,
        !TypeAssignSet, !Info) :-
    typecheck_goal(Goal0, Goal, Context, !TypeAssignSet, !Info),
    typecheck_goal_list(Goals0, Goals, Context, !TypeAssignSet, !Info).

:- pred typecheck_case_list(list(case)::in, list(case)::out,
    prog_context::in, type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_case_list([], [], _, !TypeAssignSet, !Info).
typecheck_case_list([Case0 | Cases0], [Case | Cases], Context,
        !TypeAssignSet, !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    typecheck_goal(Goal0, Goal, Context, !TypeAssignSet, !Info),
    Case = case(MainConsId, OtherConsIds, Goal),
    typecheck_case_list(Cases0, Cases, Context, !TypeAssignSet, !Info).

%---------------------------------------------------------------------------%

:- pred typecheck_plain_call(sym_name::in, prog_context::in,
    goal_id::in, list(prog_var)::in, pred_id::out,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_plain_call(SymName, Context, GoalId, ArgVars, PredId,
        !TypeAssignSet, !Info) :-
    % Look up the called predicate's arg types.
    typecheck_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    PredFormArity = arg_list_arity(ArgVars),
    SymNamePredFormArity = sym_name_pred_form_arity(SymName, PredFormArity),
    typecheck_info_get_calls_are_fully_qualified(!.Info, IsFullyQualified),
    predicate_table_lookup_pf_sym_arity(PredicateTable, IsFullyQualified,
        pf_predicate, SymName, PredFormArity, PredIds),
    (
        PredIds = [],
        PredId = invalid_pred_id,
        typecheck_info_get_error_clause_context(!.Info, ClauseContext),
        Spec = report_error_call_to_undef_pred(ClauseContext, Context,
            SymNamePredFormArity),
        typecheck_info_add_error(Spec, !Info)
    ;
        PredIds = [HeadPredId | TailPredIds],
        (
            TailPredIds = [],
            % Handle the case of non-overloaded predicate calls separately
            % from overloaded ones, because
            %
            % - this is the usual case, and
            % - it can be handled more simply and quickly
            %   than overloaded calls.
            PredId = HeadPredId,
            ArgVectorKind = arg_vector_plain_call_pred_id(PredId),
            typecheck_plain_or_foreign_call_pred_id(ArgVectorKind, Context,
                GoalId, PredId, ArgVars, !TypeAssignSet, !Info)
        ;
            TailPredIds = [_ | _],
            typecheck_plain_call_overloaded(SymName, Context, GoalId,
                PredIds, ArgVars, !TypeAssignSet, !Info),
            % In general, figuring out which predicate is being called
            % requires resolving any overloading, which may not be possible
            % until we have typechecked the entire clause, which, in the
            % presence of type inference, means it cannot be done until
            % after the typechecking pass is done. Hence, here we just
            % record an invalid pred_id in the HLDS, and let the invocation of
            % finally_resolve_pred_overloading by purity.m replace that
            % with the actual pred_id.
            PredId = invalid_pred_id
        ),

        % Arguably, we could do context reduction at a different point.
        % See the paper: "Type classes: an exploration of the design space",
        % S. Peyton-Jones, M. Jones 1997, for a discussion of some of the
        % issues.
        perform_context_reduction(Context, !TypeAssignSet, !Info)
    ).

    % Typecheck a call to a specific predicate.
    %
:- pred typecheck_plain_or_foreign_call_pred_id(arg_vector_kind::in,
    prog_context::in, goal_id::in, pred_id::in, list(prog_var)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_plain_or_foreign_call_pred_id(ArgVectorKind, Context, GoalId,
        PredId, ArgVars, !TypeAssignSet, !Info) :-
    typecheck_info_get_module_info(!.Info, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
        PredArgTypes),
    pred_info_get_class_context(PredInfo, PredClassContext),

    % Rename apart the type variables in the called predicate's arg types
    % and then unify the types of the call arguments with the called
    % predicates' arg types. Optimize the common case of a non-polymorphic,
    % non-constrained predicate.
    ( if
        varset.is_empty(PredTypeVarSet),
        PredClassContext = univ_exist_constraints([], [])
    then
        typecheck_vars_have_types(ArgVectorKind, Context, ArgVars,
            PredArgTypes, !TypeAssignSet, !Info)
    else
        module_info_get_class_table(ModuleInfo, ClassTable),
        make_body_hlds_constraints(ClassTable, PredTypeVarSet,
            GoalId, PredClassContext, PredConstraints),
        typecheck_vars_have_polymorphic_type_list(atas_pred(PredId),
            var_vector_args(ArgVectorKind), Context, ArgVars,
            PredTypeVarSet, PredExistQVars, PredArgTypes, PredConstraints,
            !TypeAssignSet, !Info)
    ).

:- pred typecheck_plain_call_overloaded(sym_name::in, prog_context::in,
    goal_id::in, list(pred_id)::in, list(prog_var)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_plain_call_overloaded(SymName, Context, GoalId, PredIds,
        ArgVars, TypeAssignSet0, TypeAssignSet, !Info) :-
    PredFormArity = arg_list_arity(ArgVars),
    SymNamePredFormArity = sym_name_pred_form_arity(SymName, PredFormArity),
    Symbol = overloaded_pred(SymNamePredFormArity, PredIds),
    typecheck_info_add_overloaded_symbol(Symbol, Context, !Info),

    % Let the new arg_type_assign_set be the cross-product of the current
    % type_assign_set and the set of possible lists of argument types
    % for the overloaded predicate, suitable renamed apart.
    typecheck_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_class_table(ModuleInfo, ClassTable),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    predicate_table_get_pred_id_table(PredicateTable, PredIdTable),
    get_overloaded_pred_arg_types(PredIdTable, ClassTable, GoalId, PredIds,
        TypeAssignSet0, [], ArgsTypeAssignSet0),

    % Then unify the types of the call arguments with the
    % called predicates' arg types.
    VarVectorKind =
        var_vector_args(arg_vector_plain_pred_call(SymNamePredFormArity)),
    typecheck_vars_have_arg_types(VarVectorKind, Context, 1, ArgVars,
        ArgsTypeAssignSet0, ArgsTypeAssignSet, !Info),
    TypeAssignSet = convert_args_type_assign_set(ArgsTypeAssignSet).

:- pred get_overloaded_pred_arg_types(pred_id_table::in, class_table::in,
    goal_id::in, list(pred_id)::in, type_assign_set::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

get_overloaded_pred_arg_types(_PredTable, _ClassTable, _GoalId,
        [], _TypeAssignSet0, !ArgsTypeAssignSet).
get_overloaded_pred_arg_types(PredTable, ClassTable, GoalId,
        [PredId | PredIds], TypeAssignSet0, !ArgsTypeAssignSet) :-
    map.lookup(PredTable, PredId, PredInfo),
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
        PredArgTypes),
    pred_info_get_class_context(PredInfo, PredClassContext),
    pred_info_get_typevarset(PredInfo, TVarSet),
    make_body_hlds_constraints(ClassTable, TVarSet, GoalId,
        PredClassContext, PredConstraints),
    add_renamed_apart_arg_type_assigns(atas_pred(PredId), PredTypeVarSet,
        PredExistQVars, PredArgTypes, PredConstraints,
        TypeAssignSet0, !ArgsTypeAssignSet),
    get_overloaded_pred_arg_types(PredTable, ClassTable, GoalId,
        PredIds, TypeAssignSet0, !ArgsTypeAssignSet).

%---------------------------------------------------------------------------%

:- pred typecheck_higher_order_call(generic_call::in, prog_context::in,
    prog_var::in, purity::in, list(prog_var)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_higher_order_call(GenericCall, Context, PredVar, Purity, ArgVars,
        !TypeAssignSet, !Info) :-
    list.length(ArgVars, Arity),
    higher_order_pred_type(Purity, Arity, TypeVarSet, PredVarType, ArgTypes),
    ArgVectorKind = arg_vector_generic_call(GenericCall),
    VarVectorKind = var_vector_args(ArgVectorKind),
    % The class context is empty because higher-order predicates
    % are always monomorphic. Similarly for ExistQVars.
    ExistQVars = [],
    typecheck_vars_have_polymorphic_type_list(atas_higher_order_call(PredVar),
        VarVectorKind, Context, [PredVar | ArgVars], TypeVarSet, ExistQVars,
        [PredVarType | ArgTypes], empty_hlds_constraints,
        !TypeAssignSet, !Info).

%---------------------------------------------------------------------------%

:- pred typecheck_event_call(prog_context::in, string::in, list(prog_var)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_event_call(Context, EventName, ArgVars, !TypeAssignSet, !Info) :-
    typecheck_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_event_set(ModuleInfo, EventSet),
    EventSpecMap = EventSet ^ event_set_spec_map,
    ( if event_arg_types(EventSpecMap, EventName, EventArgTypes) then
        list.length(ArgVars, NumArgVars),
        list.length(EventArgTypes, NumEventArgTypes),
        ( if NumArgVars = NumEventArgTypes then
            ArgVectorKind = arg_vector_event(EventName),
            typecheck_vars_have_types(ArgVectorKind, Context,
                ArgVars, EventArgTypes, !TypeAssignSet, !Info)
        else
            Spec = report_error_undef_event_arity(Context,
                EventName, EventArgTypes, ArgVars),
            typecheck_info_add_error(Spec, !Info)
        )
    else
        Spec = report_error_undef_event(Context, EventName),
        typecheck_info_add_error(Spec, !Info)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Typecheck a unification.
    %
:- pred typecheck_unification(unify_context::in, prog_context::in, goal_id::in,
    prog_var::in, unify_rhs::in, unify_rhs::out,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_unification(UnifyContext, Context, GoalId, LHSVar, RHS0, RHS,
        !TypeAssignSet, !Info) :-
    (
        RHS0 = rhs_var(RHSVar),
        typecheck_unify_var_var(UnifyContext, Context, LHSVar, RHSVar,
            !TypeAssignSet, !Info),
        RHS = RHS0
    ;
        RHS0 = rhs_functor(ConsId, _ExistConstraints, ArgVars),
        ( if
            cons_id_must_be_builtin_type(ConsId, BuiltinType, BuiltinTypeName)
        then
            typecheck_unify_var_functor_builtin(UnifyContext, Context, LHSVar,
                ConsId, BuiltinType, BuiltinTypeName, !TypeAssignSet, !Info)
        else
            typecheck_unify_var_functor_std(UnifyContext, Context, LHSVar,
                ConsId, ArgVars, GoalId, !TypeAssignSet, !Info)
        ),
        perform_context_reduction(Context, !TypeAssignSet, !Info),
        RHS = RHS0
    ;
        RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc,
            NonLocals, VarsModes, Det, Goal0),
        typecheck_info_set_rhs_lambda(has_rhs_lambda, !Info),
        assoc_list.keys(VarsModes, Vars),
        typecheck_lambda_var_has_type(UnifyContext, Context, Purity,
            PredOrFunc, LHSVar, Vars, !TypeAssignSet, !Info),
        typecheck_goal(Goal0, Goal, Context, !TypeAssignSet, !Info),
        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc,
            NonLocals, VarsModes, Det, Goal)
    ).

%---------------------------------------------------------------------------%

:- pred typecheck_unify_var_var(unify_context::in, prog_context::in,
    prog_var::in, prog_var::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_unify_var_var(UnifyContext, Context, X, Y,
        TypeAssignSet0, TypeAssignSet, !Info) :-
    type_assigns_unify_var_var(TypeAssignSet0, X, Y, [], TypeAssignSet1),
    ( if
        TypeAssignSet1 = [],
        TypeAssignSet0 = [_ | _]
    then
        TypeAssignSet = TypeAssignSet0,
        Spec = report_error_unify_var_var(!.Info, UnifyContext, Context,
            X, Y, TypeAssignSet0),
        typecheck_info_add_error(Spec, !Info)
    else
        TypeAssignSet = TypeAssignSet1
    ).

    % Iterate type_assign_unify_var_var over all the given type assignments.
    %
:- pred type_assigns_unify_var_var(type_assign_set::in,
    prog_var::in, prog_var::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assigns_unify_var_var([], _, _, !TypeAssignSet).
type_assigns_unify_var_var([TypeAssign | TypeAssigns], X, Y, !TypeAssignSet) :-
    type_assign_unify_var_var(TypeAssign, X, Y, !TypeAssignSet),
    type_assigns_unify_var_var(TypeAssigns, X, Y, !TypeAssignSet).

    % Typecheck the unification of two variables,
    % and update the type assignment.
    % TypeAssign0 is the type assignment we are updating,
    % TypeAssignSet0 is an accumulator for the list of possible
    % type assignments so far, and TypeAssignSet is TypeAssignSet plus
    % any type assignment(s) resulting from TypeAssign0 and this unification.
    %
:- pred type_assign_unify_var_var(type_assign::in, prog_var::in, prog_var::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_unify_var_var(TypeAssign0, X, Y, !TypeAssignSet) :-
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    ( if search_var_type(VarTypes0, X, TypeX) then
        search_insert_var_type(Y, TypeX, MaybeTypeY, VarTypes0, VarTypes),
        (
            MaybeTypeY = yes(TypeY),
            % Both X and Y already have types - just unify their types.
            ( if
                type_assign_unify_type(TypeX, TypeY, TypeAssign0, TypeAssign3)
            then
                !:TypeAssignSet = [TypeAssign3 | !.TypeAssignSet]
            else
                !:TypeAssignSet = !.TypeAssignSet
            )
        ;
            MaybeTypeY = no,
            type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        )
    else
        ( if search_var_type(VarTypes0, Y, TypeY) then
            % X is a fresh variable which hasn't been assigned a type yet.
            add_var_type(X, TypeY, VarTypes0, VarTypes),
            type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        else
            % Both X and Y are fresh variables - introduce a fresh type
            % variable with kind `star' to represent their type.
            type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
            varset.new_var(TypeVar, TypeVarSet0, TypeVarSet),
            type_assign_set_typevarset(TypeVarSet, TypeAssign0, TypeAssign1),
            Type = type_variable(TypeVar, kind_star),
            add_var_type(X, Type, VarTypes0, VarTypes1),
            ( if X = Y then
                VarTypes = VarTypes1
            else
                add_var_type(Y, Type, VarTypes1, VarTypes)
            ),
            type_assign_set_var_types(VarTypes, TypeAssign1, TypeAssign),
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        )
    ).

%---------------------------------------------------------------------------%

:- pred cons_id_must_be_builtin_type(cons_id::in, builtin_type::out,
    string::out) is semidet.

cons_id_must_be_builtin_type(ConsId, BuiltinType, BuiltinTypeName) :-
    (
        ConsId = some_int_const(IntConst),
        BuiltinType = builtin_type_int(type_of_int_const(IntConst)),
        BuiltinTypeName = type_name_of_int_const(IntConst)
    ;
        ConsId = float_const(_),
        BuiltinTypeName = "float",
        BuiltinType = builtin_type_float
    ;
        ConsId = string_const(_),
        BuiltinTypeName = "string",
        BuiltinType = builtin_type_string
    ).

%---------------------------------------------------------------------------%

    % typecheck_lambda_var_has_type(..., Var, ArgVars, !Info)
    %
    % Check that Var has type pred(T1, T2, ...) where T1, T2, ...
    % are the types of the ArgVars.
    %
:- pred typecheck_lambda_var_has_type(unify_context::in, prog_context::in,
    purity::in, pred_or_func::in, prog_var::in, list(prog_var)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_lambda_var_has_type(UnifyContext, Context, Purity, PredOrFunc,
        Var, ArgVars, TypeAssignSet0, TypeAssignSet, !Info) :-
    typecheck_lambda_var_has_type_2(TypeAssignSet0, Purity, PredOrFunc,
        Var, ArgVars, [], TypeAssignSet1),
    ( if
        TypeAssignSet1 = [],
        TypeAssignSet0 = [_ | _]
    then
        TypeAssignSet = TypeAssignSet0,
        Spec = report_error_unify_var_lambda(!.Info, UnifyContext, Context,
            PredOrFunc, Var, ArgVars, TypeAssignSet0),
        typecheck_info_add_error(Spec, !Info)
    else
        TypeAssignSet = TypeAssignSet1
    ).

:- pred typecheck_lambda_var_has_type_2(type_assign_set::in, purity::in,
    pred_or_func::in, prog_var::in,
    list(prog_var)::in, type_assign_set::in, type_assign_set::out) is det.

typecheck_lambda_var_has_type_2([], _, _, _, _, !TypeAssignSet).
typecheck_lambda_var_has_type_2([TypeAssign0 | TypeAssignSet0], Purity,
        PredOrFunc, Var, ArgVars, !TypeAssignSet) :-
    type_assign_get_types_of_vars(ArgVars, ArgVarTypes,
        TypeAssign0, TypeAssign1),
    construct_higher_order_type(Purity, PredOrFunc, ArgVarTypes, LambdaType),
    type_assign_var_has_type(TypeAssign1, Var, LambdaType, !TypeAssignSet),
    typecheck_lambda_var_has_type_2(TypeAssignSet0,
        Purity, PredOrFunc, Var, ArgVars, !TypeAssignSet).

:- pred type_assign_get_types_of_vars(list(prog_var)::in, list(mer_type)::out,
    type_assign::in, type_assign::out) is det.

type_assign_get_types_of_vars([], [], !TypeAssign).
type_assign_get_types_of_vars([Var | Vars], [Type | Types], !TypeAssign) :-
    % Check whether the variable already has a type.
    type_assign_get_var_types(!.TypeAssign, VarTypes0),
    ( if search_var_type(VarTypes0, Var, VarType) then
        % If so, use that type.
        Type = VarType
    else
        % Otherwise, introduce a fresh type variable with kind `star' to use
        % as the type of that variable.
        type_assign_fresh_type_var(Var, Type, !TypeAssign)
    ),
    % Recursively process the rest of the variables.
    type_assign_get_types_of_vars(Vars, Types, !TypeAssign).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Rename apart the type variables in called predicate's arg types
    % separately for each type assignment, resulting in an "arg type
    % assignment set", and then for each arg type assignment in the
    % arg type assignment set, check that the argument variables have
    % the expected types.
    % A set of class constraints are also passed in, which must have the
    % types contained within renamed apart.
    %
:- pred typecheck_vars_have_polymorphic_type_list(args_type_assign_source::in,
    var_vector_kind::in, prog_context::in, list(prog_var)::in, tvarset::in,
    existq_tvars::in, list(mer_type)::in, hlds_constraints::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_vars_have_polymorphic_type_list(Source, VarVectorKind, Context,
        ArgVars, PredTypeVarSet, PredExistQVars, PredArgTypes, PredConstraints,
        TypeAssignSet0, TypeAssignSet, !Info) :-
    add_renamed_apart_arg_type_assigns(Source, PredTypeVarSet, PredExistQVars,
        PredArgTypes, PredConstraints, TypeAssignSet0, [], ArgsTypeAssignSet0),
    typecheck_vars_have_arg_types(VarVectorKind, Context, 1, ArgVars,
        ArgsTypeAssignSet0, ArgsTypeAssignSet, !Info),
    TypeAssignSet = convert_args_type_assign_set(ArgsTypeAssignSet).

:- pred add_renamed_apart_arg_type_assigns(args_type_assign_source::in,
    tvarset::in, existq_tvars::in, list(mer_type)::in, hlds_constraints::in,
    type_assign_set::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

add_renamed_apart_arg_type_assigns(_, _, _, _, _, [], !ArgsTypeAssigns).
add_renamed_apart_arg_type_assigns(Source, PredTypeVarSet, PredExistQVars,
        PredArgTypes, PredConstraints, [TypeAssign0 | TypeAssigns0],
        !ArgsTypeAssigns) :-
    % Rename everything apart.
    type_assign_rename_apart(TypeAssign0, PredTypeVarSet, PredArgTypes,
        TypeAssign1, ParentArgTypes, Renaming),
    apply_variable_renaming_to_tvar_list(Renaming, PredExistQVars,
        ParentExistQVars),
    apply_variable_renaming_to_constraints(Renaming, PredConstraints,
        ParentConstraints),

    % Insert the existentially quantified type variables for the called
    % predicate into HeadTypeParams (which holds the set of type
    % variables which the caller is not allowed to bind).
    type_assign_get_existq_tvars(TypeAssign1, ExistQTVars0),
    ExistQTVars = ParentExistQVars ++ ExistQTVars0,
    type_assign_set_existq_tvars(ExistQTVars, TypeAssign1, TypeAssign),

    % Save the results and recurse.
    NewArgsTypeAssign = args_type_assign(TypeAssign, ParentArgTypes,
        ParentConstraints, Source),
    !:ArgsTypeAssigns = [NewArgsTypeAssign | !.ArgsTypeAssigns],
    add_renamed_apart_arg_type_assigns(Source, PredTypeVarSet,
        PredExistQVars, PredArgTypes, PredConstraints, TypeAssigns0,
        !ArgsTypeAssigns).

%---------------------------------------------------------------------------%

:- pred typecheck_vars_have_arg_types(var_vector_kind::in, prog_context::in,
    int::in, list(prog_var)::in,
    args_type_assign_set::in, args_type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_vars_have_arg_types(_, _, _, [], !ArgsTypeAssignSet, !Info).
typecheck_vars_have_arg_types(VarVectorKind, Context, CurArgNum, [Var | Vars],
        !ArgsTypeAssignSet, !Info) :-
    GoalContext = type_error_in_var_vector(VarVectorKind, CurArgNum),
    typecheck_var_has_arg_type(GoalContext, Context, CurArgNum, Var,
        !ArgsTypeAssignSet, !Info),
    typecheck_vars_have_arg_types(VarVectorKind, Context, CurArgNum + 1, Vars,
        !ArgsTypeAssignSet, !Info).

:- pred typecheck_var_has_arg_type(type_error_goal_context::in,
    prog_context::in, int::in, prog_var::in,
    args_type_assign_set::in, args_type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_var_has_arg_type(GoalContext, Context, ArgNum, Var,
        ArgsTypeAssignSet0, ArgsTypeAssignSet, !Info) :-
    typecheck_var_has_arg_type_in_args_type_assigns(ArgNum, Var,
        ArgsTypeAssignSet0, [], ArgsTypeAssignSet1),
    ( if
        ArgsTypeAssignSet1 = [],
        ArgsTypeAssignSet0 = [_ | _]
    then
        Spec = report_error_var_has_wrong_type_arg(!.Info,
            GoalContext, Context, ArgNum, Var, ArgsTypeAssignSet0),
        ArgsTypeAssignSet = ArgsTypeAssignSet0,
        typecheck_info_add_error(Spec, !Info)
    else
        ArgsTypeAssignSet = ArgsTypeAssignSet1
    ).

:- pred typecheck_var_has_arg_type_in_args_type_assigns(int::in, prog_var::in,
    args_type_assign_set::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

typecheck_var_has_arg_type_in_args_type_assigns(_, _, [], !ArgsTypeAssignSet).
typecheck_var_has_arg_type_in_args_type_assigns(ArgNum, Var,
        [ArgsTypeAssign | ArgsTypeAssigns], !ArgsTypeAssignSet) :-
    typecheck_var_has_arg_type_in_args_type_assign(ArgNum, Var,
        ArgsTypeAssign, !ArgsTypeAssignSet),
    typecheck_var_has_arg_type_in_args_type_assigns(ArgNum, Var,
        ArgsTypeAssigns, !ArgsTypeAssignSet).

:- pred typecheck_var_has_arg_type_in_args_type_assign(int::in, prog_var::in,
    args_type_assign::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

typecheck_var_has_arg_type_in_args_type_assign(ArgNum, Var, ArgsTypeAssign0,
        !ArgsTypeAssignSet) :-
    ArgsTypeAssign0 = args_type_assign(TypeAssign0, ArgTypes,
        ClassContext, Source),
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    list.det_index1(ArgTypes, ArgNum, ArgType),
    search_insert_var_type(Var, ArgType, MaybeOldVarType, VarTypes0, VarTypes),
    (
        MaybeOldVarType = yes(OldVarType),
        ( if
            type_assign_unify_type(OldVarType, ArgType,
                TypeAssign0, TypeAssign)
        then
            ArgsTypeAssign = args_type_assign(TypeAssign, ArgTypes,
                ClassContext, Source),
            !:ArgsTypeAssignSet = [ArgsTypeAssign | !.ArgsTypeAssignSet]
        else
            true
        )
    ;
        MaybeOldVarType = no,
        type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
        ArgsTypeAssign = args_type_assign(TypeAssign, ArgTypes,
            ClassContext, Source),
        !:ArgsTypeAssignSet = [ArgsTypeAssign | !.ArgsTypeAssignSet]
    ).

%---------------------------------------------------------------------------%

    % Given a list of variables and a list of types, ensure that
    % each variable has the corresponding type.
    %
:- pred typecheck_vars_have_types(arg_vector_kind::in,
    prog_context::in, list(prog_var)::in, list(mer_type)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_vars_have_types(ArgVectorKind, Context, Vars, Types,
        !TypeAssignSet, !Info) :-
    typecheck_vars_have_types_in_arg_vector(!.Info, Context, ArgVectorKind, 1,
        Vars, Types, !TypeAssignSet,
        [], Specs, yes([]), MaybeArgVectorTypeErrors),
    ( if
        MaybeArgVectorTypeErrors = yes(ArgVectorTypeErrors),
        ArgVectorTypeErrors = [_, _ | _]
    then
        AllArgsSpec = report_error_wrong_types_in_arg_vector(!.Info, Context,
            ArgVectorKind, !.TypeAssignSet, ArgVectorTypeErrors),
        typecheck_info_add_error(AllArgsSpec, !Info)
    else
        list.foldl(typecheck_info_add_error, Specs, !Info)
    ).

:- pred typecheck_vars_have_types_in_arg_vector(typecheck_info::in,
    prog_context::in, arg_vector_kind::in, int::in,
    list(prog_var)::in, list(mer_type)::in,
    type_assign_set::in, type_assign_set::out,
    list(error_spec)::in, list(error_spec)::out,
    maybe(list(arg_vector_type_error))::in,
    maybe(list(arg_vector_type_error))::out) is det.

typecheck_vars_have_types_in_arg_vector(_, _, _, _, [], [],
        !TypeAssignSet, !Specs, !MaybeArgVectorTypeErrors).
typecheck_vars_have_types_in_arg_vector(_, _, _, _, [], [_ | _],
        !TypeAssignSet, !Specs, !MaybeArgVectorTypeErrors) :-
    unexpected($pred, "length mismatch").
typecheck_vars_have_types_in_arg_vector(_, _, _, _, [_ | _], [],
        !TypeAssignSet, !Specs, !MaybeArgVectorTypeErrors) :-
    unexpected($pred, "length mismatch").
typecheck_vars_have_types_in_arg_vector(Info, Context, ArgVectorKind, ArgNum,
        [Var | Vars], [Type | Types], !TypeAssignSet, !Specs,
        !MaybeArgVectorTypeErrors) :-
    typecheck_var_has_type_in_arg_vector(Info, Context, ArgVectorKind, ArgNum,
        Var, Type, !TypeAssignSet, !Specs, !MaybeArgVectorTypeErrors),
    typecheck_vars_have_types_in_arg_vector(Info, Context,
        ArgVectorKind, ArgNum + 1, Vars, Types, !TypeAssignSet, !Specs,
        !MaybeArgVectorTypeErrors).

:- pred typecheck_var_has_type_in_arg_vector(typecheck_info::in,
    prog_context::in, arg_vector_kind::in, int::in,
    prog_var::in, mer_type::in, type_assign_set::in, type_assign_set::out,
    list(error_spec)::in, list(error_spec)::out,
    maybe(list(arg_vector_type_error))::in,
    maybe(list(arg_vector_type_error))::out) is det.

typecheck_var_has_type_in_arg_vector(Info, Context, ArgVectorKind, ArgNum,
        Var, Type, TypeAssignSet0, TypeAssignSet, !Specs,
        !MaybeArgVectorTypeErrors) :-
    typecheck_var_has_type_2(TypeAssignSet0, Var, Type, [], TypeAssignSet1),
    ( if
        TypeAssignSet1 = [],
        TypeAssignSet0 = [_ | _]
    then
        TypeAssignSet = TypeAssignSet0,
        GoalContext =
            type_error_in_var_vector(var_vector_args(ArgVectorKind), ArgNum),
        SpecAndMaybeActualExpected = report_error_var_has_wrong_type(Info,
            GoalContext, Context, Var, Type, TypeAssignSet0),
        SpecAndMaybeActualExpected =
            spec_and_maybe_actual_expected(Spec, MaybeActualExpected),
        !:Specs = [Spec | !.Specs],
        (
            !.MaybeArgVectorTypeErrors = no
        ;
            !.MaybeArgVectorTypeErrors = yes(ArgVectorTypeErrors0),
            (
                MaybeActualExpected = no,
                !:MaybeArgVectorTypeErrors = no
            ;
                MaybeActualExpected = yes(ActualExpected),
                ArgVectorTypeError = arg_vector_type_error(ArgNum, Var,
                    ActualExpected),
                ArgVectorTypeErrors =
                    [ArgVectorTypeError | ArgVectorTypeErrors0],
                !:MaybeArgVectorTypeErrors = yes(ArgVectorTypeErrors)
            )
        )
    else
        TypeAssignSet = TypeAssignSet1
    ).

:- pred typecheck_var_has_stm_atomic_type(prog_context::in, prog_var::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_var_has_stm_atomic_type(Context, Var, !TypeAssignSet, !Info) :-
    typecheck_var_has_type(type_error_in_atomic_inner, Context,
        Var, stm_atomic_type, !TypeAssignSet, !Info).

:- pred typecheck_var_has_type(type_error_goal_context::in, prog_context::in,
    prog_var::in, mer_type::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_var_has_type(GoalContext, Context, Var, Type,
        TypeAssignSet0, TypeAssignSet, !Info) :-
    typecheck_var_has_type_2(TypeAssignSet0, Var, Type, [], TypeAssignSet1),
    ( if
        TypeAssignSet1 = [],
        TypeAssignSet0 = [_ | _]
    then
        TypeAssignSet = TypeAssignSet0,
        SpecAndMaybeActualExpected = report_error_var_has_wrong_type(!.Info,
            GoalContext, Context, Var, Type, TypeAssignSet0),
        SpecAndMaybeActualExpected = spec_and_maybe_actual_expected(Spec, _),
        typecheck_info_add_error(Spec, !Info)
    else
        TypeAssignSet = TypeAssignSet1
    ).

:- pred typecheck_var_has_type_2(type_assign_set::in, prog_var::in,
    mer_type::in, type_assign_set::in, type_assign_set::out) is det.

typecheck_var_has_type_2([], _, _, !TypeAssignSet).
typecheck_var_has_type_2([TypeAssign0 | TypeAssigns0], Var, Type,
        !TypeAssignSet) :-
    type_assign_var_has_type(TypeAssign0, Var, Type, !TypeAssignSet),
    typecheck_var_has_type_2(TypeAssigns0, Var, Type, !TypeAssignSet).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

typecheck_check_for_ambiguity(Context, StuffToCheck, HeadVars,
        TypeAssignSet, !Info) :-
    (
        % There should always be a type assignment, because if there is
        % an error somewhere, instead of setting the current type assignment
        % set to the empty set, the type-checker should continue with the
        % previous type assignment set (so that it can detect other errors
        % in the same clause).
        TypeAssignSet = [],
        unexpected($pred, "no type-assignment")
    ;
        TypeAssignSet = [_SingleTypeAssign]
    ;
        TypeAssignSet = [TypeAssign1, TypeAssign2 | TypeAssigns3plus],
        % We only report an ambiguity error if
        % (a) we haven't encountered any other errors and if
        %     StuffToCheck = clause_only(_), and also
        % (b) the ambiguity occurs only in the body, rather than in the
        %     head variables (and hence can't be resolved by looking at
        %     later clauses).
        typecheck_info_get_all_errors(!.Info, ErrorsSoFar),
        ( if
            ErrorsSoFar = [],
            (
                StuffToCheck = whole_pred
            ;
                StuffToCheck = clause_only,
                compute_headvar_types_in_type_assign(HeadVars,
                    TypeAssign1, HeadTypesInAssign1),
                compute_headvar_types_in_type_assign(HeadVars,
                    TypeAssign2, HeadTypesInAssign2),
                list.map(compute_headvar_types_in_type_assign(HeadVars),
                    TypeAssigns3plus, HeadTypesInAssigns3plus),

                % Only report an error if the headvar types are identical
                % (which means that the ambiguity must have occurred
                % in the body).
                all_identical_up_to_renaming(HeadTypesInAssign1,
                    [HeadTypesInAssign2 | HeadTypesInAssigns3plus])
            )
        then
            typecheck_info_get_error_clause_context(!.Info, ClauseContext),
            typecheck_info_get_overloaded_symbol_map(!.Info,
                OverloadedSymbolMap),
            Spec = report_ambiguity_error(ClauseContext, Context,
                OverloadedSymbolMap, TypeAssign1, TypeAssign2,
                TypeAssigns3plus),
            typecheck_info_add_error(Spec, !Info)
        else
            true
        )
    ).

:- pred compute_headvar_types_in_type_assign(list(prog_var)::in,
    type_assign::in, list(mer_type)::out) is det.

compute_headvar_types_in_type_assign(HeadVars, TypeAssign, HeadTypes) :-
    type_assign_get_var_types(TypeAssign, VarTypes),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    lookup_var_types(VarTypes, HeadVars, HeadTypes0),
    apply_rec_subst_to_type_list(TypeBindings, HeadTypes0, HeadTypes).

:- pred all_identical_up_to_renaming(list(mer_type)::in,
    list(list(mer_type))::in) is semidet.

all_identical_up_to_renaming(_, []).
all_identical_up_to_renaming(HeadTypes1, [HeadTypes2 | HeadTypes3plus]) :-
    identical_up_to_renaming(HeadTypes1, HeadTypes2),
    all_identical_up_to_renaming(HeadTypes1, HeadTypes3plus).

%---------------------------------------------------------------------------%

    % Ensure that each variable in Vars has been assigned a type.
    %
:- pred ensure_vars_have_a_type(var_vector_kind::in, prog_context::in,
    list(prog_var)::in, type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

ensure_vars_have_a_type(VarVectorKind, Context, Vars, !TypeAssignSet, !Info) :-
    (
        Vars = []
    ;
        Vars = [_ | _],
        % Invent some new type variables to use as the types of these
        % variables. Since each type is the type of a program variable,
        % each must have kind `star'.
        list.length(Vars, NumVars),
        varset.init(TypeVarSet0),
        varset.new_vars(NumVars, TypeVars, TypeVarSet0, TypeVarSet),
        prog_type.var_list_to_type_list(map.init, TypeVars, Types),
        typecheck_vars_have_polymorphic_type_list(atas_ensure_have_a_type,
            VarVectorKind, Context, Vars, TypeVarSet, [], Types,
            empty_hlds_constraints, !TypeAssignSet, !Info)
    ).

    % Ensure that each variable in Vars has been assigned a single type.
    %
:- pred ensure_vars_have_a_single_type(var_vector_kind::in, prog_context::in,
    list(prog_var)::in, type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

ensure_vars_have_a_single_type(VarVectorKind, Context, Vars,
        !TypeAssignSet, !Info) :-
    (
        Vars = []
    ;
        Vars = [_ | _],
        % Invent a new type variable to use as the type of these
        % variables. Since the type is the type of a program variable,
        % each must have kind `star'.
        varset.init(TypeVarSet0),
        varset.new_var(TypeVar, TypeVarSet0, TypeVarSet),
        Type = type_variable(TypeVar, kind_star),
        list.length(Vars, NumVars),
        list.duplicate(NumVars, Type, Types),
        typecheck_vars_have_polymorphic_type_list(atas_ensure_have_a_type,
            VarVectorKind, Context, Vars, TypeVarSet, [], Types,
            empty_hlds_constraints, !TypeAssignSet, !Info)
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_clauses.
%---------------------------------------------------------------------------%
