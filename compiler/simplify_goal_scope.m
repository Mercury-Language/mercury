%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: simplify_goal_scope.m.
%
% This module handles simplification of scope goals.
%
%----------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_goal_scope.
:- interface.

:- import_module check_hlds.simplify.common.
:- import_module check_hlds.simplify.simplify_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.

    % Handle simplification of scope goals.
    %
:- pred simplify_goal_scope(
    hlds_goal_expr::in(goal_expr_scope), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

    % If the goal nested inside this scope goal is another scope goal,
    % then merge the two scopes, if this is possible. Repeat for as many
    % nested scopes as possible.
    %
:- pred try_to_merge_nested_scopes(scope_reason::in, hlds_goal::in,
    hlds_goal_info::in, hlds_goal::out) is det.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.simplify.simplify_goal.
:- import_module check_hlds.simplify.simplify_tasks.
:- import_module hlds.const_struct.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_module.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.

simplify_goal_scope(GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
        NestedContext0, InstMap0, Common0, Common, !Info) :-
    GoalExpr0 = scope(Reason0, SubGoal0),
    ( if Reason0 = from_ground_term(TermVar, from_ground_term_construct) then
        simplify_info_get_module_info(!.Info, ModuleInfo0),
        module_info_get_const_struct_db(ModuleInfo0, ConstStructDb0),
        const_struct_db_get_ground_term_enabled(ConstStructDb0,
            ConstStructEnabled),
        (
            ConstStructEnabled = do_not_enable_const_struct_user,
            module_info_get_globals(ModuleInfo0, Globals),
            globals.get_opt_tuple(Globals, OptTuple),
            CommonStruct = OptTuple ^ ot_opt_common_structs,
            (
                CommonStruct = opt_common_structs,
                % Traversing the construction unifications inside the scope
                % would allow common.m to
                %
                % - replace some of those constructions with references to
                %   other variables that were constructed the same way, and
                % - remember those constructions, so that other constructions
                %   outside the scope could be replaced with references to
                %   variables built inside the scope.
                %
                % Since unifying a variable with a statically constructed
                % ground term yields code that is at least as fast as unifying
                % that variable with another variable that is already bound to
                % that term, and probably faster because it does not require
                % saving the other variable across calls, neither of these
                % actions would be an advantage. On the other hand, both would
                % complicate the required treatment of
                % from_ground_term_construct scopes in liveness.m, slowing down
                % the liveness pass, as well as this pass. Since the code
                % inside the scope is already as simple as it can be, we
                % leave it alone.
                GoalExpr = GoalExpr0,
                GoalInfo = GoalInfo0,
                Common = Common0
            ;
                CommonStruct = do_not_opt_common_structs,
                % Looking inside the scope may allow us to reduce the number of
                % memory cells we may need to allocate dynamically. This
                % improvement in the generated code trumps the cost in compile
                % time. However, we need to update the reason, since leaving it
                % as from_ground_term_construct would tell liveness.m that the
                % code inside the scope hasn't had either of the actions
                % mentioned in the comment above applied to it, and in this
                % case, we cannot guarantee that.
                simplify_goal(SubGoal0, SubGoal, NestedContext0, InstMap0,
                    Common0, Common, !Info),
                NewReason = from_ground_term(TermVar, from_ground_term_other),
                GoalExpr = scope(NewReason, SubGoal),
                GoalInfo = GoalInfo0
            )
        ;
            ConstStructEnabled = enable_const_struct_user,
            ( if
                SubGoal0 = hlds_goal(SubGoalExpr, _),
                SubGoalExpr = conj(plain_conj, Conjuncts),
                Conjuncts = [HeadConjunctPrime | TailConjunctsPrime]
            then
                HeadConjunct = HeadConjunctPrime,
                TailConjuncts = TailConjunctsPrime
            else
                unexpected($pred,
                    "from_ground_term_construct scope is not conjunction")
            ),
            simplify_info_get_var_table(!.Info, VarTable),
            simplify_info_get_defined_where(!.Info, DefinedWhere),
            simplify_construct_ground_terms(DefinedWhere, VarTable, TermVar,
                HeadConjunct, TailConjuncts, [], ElimVars,
                map.init, VarArgMap, ConstStructDb0, ConstStructDb),
            module_info_set_const_struct_db(ConstStructDb,
                ModuleInfo0, ModuleInfo),
            simplify_info_add_elim_vars(ElimVars, !Info),
            simplify_info_set_module_info(ModuleInfo, !Info),

            map.to_assoc_list(VarArgMap, VarArgs),
            ( if
                VarArgs = [TermVar - TermArg],
                TermArg = csa_const_struct(TermConstNumPrime)
            then
                TermConstNum = TermConstNumPrime
            else
                unexpected($pred, "unexpected VarArgMap")
            ),

            lookup_const_struct_num(ConstStructDb, TermConstNum,
                TermConstStruct),
            TermConsId = TermConstStruct ^ cs_cons_id,
            ConsId = ground_term_const(TermConstNum, TermConsId),
            RHS = rhs_functor(ConsId, is_not_exist_constr, []),
            Unification = construct(TermVar, ConsId, [], [],
                construct_statically(born_static), cell_is_shared,
                no_construct_sub_info),
            InstMapDelta = goal_info_get_instmap_delta(GoalInfo0),
            instmap_delta_lookup_var(InstMapDelta, TermVar, TermInst),
            UnifyMode = unify_modes_li_lf_ri_rf(free, TermInst,
                TermInst, TermInst),
            UnifyContext = unify_context(umc_explicit, []),
            GoalExpr = unify(TermVar, RHS, UnifyMode, Unification,
                UnifyContext),
            GoalInfo = GoalInfo0,
            Common = Common0
        )
    else
        ( if Reason0 = disable_warnings(HeadWarning, TailWarnings) then
            simplify_info_get_simplify_tasks(!.Info, Tasks0),
            list.foldl(disable_simplify_warning,
                [HeadWarning | TailWarnings], Tasks0, ScopeTasks),
            simplify_info_set_simplify_tasks(ScopeTasks, !Info),
            simplify_goal(SubGoal0, SubGoal, NestedContext0, InstMap0,
                Common0, Common1, !Info),
            simplify_info_set_simplify_tasks(Tasks0, !Info)
        else
            simplify_goal(SubGoal0, SubGoal, NestedContext0, InstMap0,
                Common0, Common1, !Info)
        ),
        try_to_merge_nested_scopes(Reason0, SubGoal, GoalInfo0, Goal1),
        Goal1 = hlds_goal(GoalExpr1, _GoalInfo1),
        ( if GoalExpr1 = scope(FinalReason, FinalSubGoal) then
            (
                ( FinalReason = disable_warnings(_, _)
                ; FinalReason = promise_purity(_)
                ; FinalReason = from_ground_term(_, _)
                ; FinalReason = barrier(removable)
                ),
                Goal = Goal1,
                Common = Common1
            ;
                ( FinalReason = require_detism(_)
                ; FinalReason = require_complete_switch(_)
                ; FinalReason = require_switch_arms_detism(_, _)
                ),
                % The scope has served its purpose, and it is not needed
                % anymore.
                Goal = FinalSubGoal,
                Common = Common1
            ;
                ( FinalReason = commit(_)
                ; FinalReason = exist_quant(_)
                ; FinalReason = promise_solutions(_, _)
                ; FinalReason = barrier(not_removable)
                ; FinalReason = loop_control(_, _, _)
                ),
                Goal = Goal1,
                % Replacing calls, constructions or deconstructions outside
                % a commit with references to variables created inside the
                % commit would increase the set of output variables of the goal
                % inside the commit. This is not allowed because it could
                % change the determinism.
                %
                % Thus we need to reset the common_info to what it was before
                % processing the goal inside the commit, to ensure that we
                % don't make any such replacements when processing the rest
                % of the goal.
                %
                % We do the same for several other kinds of scopes from which
                % we do not want to "export" common unifications.
                Common = Common0
            ;
                FinalReason = trace_goal(MaybeCompiletimeExpr,
                    MaybeRuntimeExpr, _, _, _),
                ( if simplify_do_after_front_end(!.Info) then
                    simplify_goal_trace_goal(MaybeCompiletimeExpr,
                        MaybeRuntimeExpr, FinalSubGoal, Goal1, Goal, !Info)
                else
                    Goal = Goal1
                ),
                % We throw away the updated Common1 for the same reason
                % as in the case above: we don't want to add any outputs
                % to the trace_goal scope, since such scopes should not
                % have ANY outputs.
                Common = Common0
            )
        else
            Goal = Goal1,
            Common = Common1
        ),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

:- pred disable_simplify_warning(goal_warning::in,
    simplify_tasks::in, simplify_tasks::out) is det.

disable_simplify_warning(Warning, !Tasks) :-
    (
        Warning = goal_warning_singleton_vars
        % Warning about singleton vars is done when clauses are added
        % to the HLDS, not during simplification.
    ;
        Warning = goal_warning_occurs_check
        % Warning about occurs check violations is done during the creation
        % of the HLDS, not during simplification.
    ;
        Warning = goal_warning_non_tail_recursive_calls
        % Warning about non-tail-recursive calls is done during
        % code generation, not during simplification. (What calls
        % are tail recursive depends on the backend.)
    ;
        Warning = goal_warning_suspicious_recursion,
        !Tasks ^ do_warn_suspicious_recursion := do_not_warn_suspicious_rec
    ;
        Warning = goal_warning_no_solution_disjunct,
        !Tasks ^ do_warn_no_solution_disjunct := do_not_warn_no_soln_disjunct
    ;
        Warning = goal_warning_unknown_format_calls
        % Unknown format warnings are generated by format_calls.m, which
        % does its own traversal of the procedure body. Therefore that warning
        % needs to be disabled during *that* traversal, not during the
        % traversal that our caller is doing.
    ).

%-----------------------------------------------------------------------------%

:- type var_to_arg_map == map(prog_var, const_struct_arg).

:- pred simplify_construct_ground_terms(defined_where::in, var_table::in,
    prog_var::in, hlds_goal::in, list(hlds_goal)::in,
    list(prog_var)::in, list(prog_var)::out,
    var_to_arg_map::in, var_to_arg_map::out,
    const_struct_db::in, const_struct_db::out) is det.

simplify_construct_ground_terms(DefinedWhere, VarTable, TermVar,
        Conjunct, Conjuncts, !ElimVars, !VarArgMap, !ConstStructDb) :-
    Conjunct = hlds_goal(GoalExpr, GoalInfo),
    ( if
        GoalExpr = unify(_, _, _, Unify, _),
        Unify = construct(LHSVarPrime, ConsIdPrime, RHSVarsPrime, _, _, _, _)
    then
        LHSVar = LHSVarPrime,
        ConsId = ConsIdPrime,
        RHSVars = RHSVarsPrime
    else
        unexpected($pred, "not construction unification")
    ),
    lookup_var_type(VarTable, LHSVar, TermType),
    (
        RHSVars = [],
        Arg = csa_constant(ConsId, TermType)
    ;
        RHSVars = [_ | _],
        list.map_foldl(map.det_remove, RHSVars, RHSArgs, !VarArgMap),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        instmap_delta_lookup_var(InstMapDelta, LHSVar, TermInst),
        ConstStruct = const_struct(ConsId, RHSArgs, TermType, TermInst,
            DefinedWhere),
        lookup_insert_const_struct(ConstStruct, ConstNum, !ConstStructDb),
        Arg = csa_const_struct(ConstNum)
    ),
    map.det_insert(LHSVar, Arg, !VarArgMap),
    (
        Conjuncts = [],
        expect(unify(TermVar, LHSVar), $pred, "last var is not TermVar")
    ;
        Conjuncts = [HeadConjunct | TailConjuncts],
        !:ElimVars = [LHSVar | !.ElimVars],
        simplify_construct_ground_terms(DefinedWhere, VarTable, TermVar,
            HeadConjunct, TailConjuncts,
            !ElimVars, !VarArgMap, !ConstStructDb)
    ).

%---------------------------------------------------------------------------%

:- pred simplify_goal_trace_goal(maybe(trace_expr(trace_compiletime))::in,
    maybe(trace_expr(trace_runtime))::in, hlds_goal::in, hlds_goal::in,
    hlds_goal::out, simplify_info::in, simplify_info::out) is det.

simplify_goal_trace_goal(MaybeCompiletimeExpr, MaybeRuntimeExpr, SubGoal,
        Goal0, Goal, !Info) :-
    (
        MaybeCompiletimeExpr = yes(CompiletimeExpr),
        KeepGoal = evaluate_compile_time_condition(CompiletimeExpr, !.Info)
    ;
        MaybeCompiletimeExpr = no,
        % A missing compile time condition means that the
        % trace goal is always compiled in.
        KeepGoal = yes
    ),
    (
        KeepGoal = no,
        Goal0 = hlds_goal(_GoalExpr0, GoalInfo0),
        Context = goal_info_get_context(GoalInfo0),
        Goal = true_goal_with_context(Context),

        simplify_info_get_deleted_call_callees(!.Info, DeletedCallCallees0),
        SubGoalCalledProcs = goal_proc_refs(SubGoal),
        set.union(SubGoalCalledProcs,
            DeletedCallCallees0, DeletedCallCallees),
        simplify_info_set_deleted_call_callees(DeletedCallCallees, !Info)
    ;
        KeepGoal = yes,
        (
            MaybeRuntimeExpr = no,
            % We keep the scope as a marker of the existence of the
            % trace scope.
            Goal = Goal0
        ;
            KeepGoal = yes,
            MaybeRuntimeExpr = yes(RuntimeExpr),
            % We want to execute SubGoal if and only if RuntimeExpr turns out
            % to be true. We could have the code generators treat this kind of
            % scope as if it were an if-then-else, but that would require
            % duplicating most of the code required to handle code generation
            % for if-then-elses. Instead, we transform the scope into an
            % if-then-else, thus reducing the problem to one that has already
            % been solved.
            %
            % The evaluation of the runtime condition is done as a special kind
            % of foreign_proc, i.e. one that has yes(RuntimeExpr) as its
            % foreign_trace_cond field. This kind of foreign_proc also acts
            % as the marker for the fact that the then-part originated as
            % the goal of a trace scope.
            simplify_info_get_module_info(!.Info, ModuleInfo),
            module_info_get_globals(ModuleInfo, Globals),
            globals.get_target(Globals, Target),
            PrivateBuiltin = mercury_private_builtin_module,
            EvalPredName = "trace_evaluate_runtime_condition",
            some [!EvalAttributes] (
                (
                    Target = target_c,
                    !:EvalAttributes = default_attributes(lang_c)
                ;
                    Target = target_java,
                    !:EvalAttributes = default_attributes(lang_java)
                ;
                    Target = target_csharp,
                    !:EvalAttributes = default_attributes(lang_csharp)
                ),
                set_may_call_mercury(proc_will_not_call_mercury,
                    !EvalAttributes),
                set_thread_safe(proc_thread_safe, !EvalAttributes),
                set_purity(purity_semipure, !EvalAttributes),
                set_terminates(proc_terminates, !EvalAttributes),
                set_may_throw_exception(proc_will_not_throw_exception,
                    !EvalAttributes),
                set_may_modify_trail(proc_will_not_modify_trail,
                    !EvalAttributes),
                set_may_call_mm_tabled(proc_will_not_call_mm_tabled,
                    !EvalAttributes),
                EvalAttributes = !.EvalAttributes
            ),
            EvalFeatures = [],
            % The code field of the call_foreign_proc goal is ignored when
            % its foreign_trace_cond field is set to `yes', as we do here.
            EvalCode = "",
            Goal0 = hlds_goal(_GoalExpr0, GoalInfo0),
            Context = goal_info_get_context(GoalInfo0),
            generate_call_foreign_proc(ModuleInfo, pf_predicate,
                PrivateBuiltin, EvalPredName,
                [], [], [], instmap_delta_bind_no_var, only_mode,
                detism_semi, purity_semipure, EvalFeatures, EvalAttributes,
                yes(RuntimeExpr), EvalCode, Context, CondGoal),
            GoalExpr = if_then_else([], CondGoal, SubGoal, true_goal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ).

:- func evaluate_compile_time_condition(trace_expr(trace_compiletime),
    simplify_info) = bool.

evaluate_compile_time_condition(TraceExpr, Info) = Result :-
    (
        TraceExpr = trace_base(BaseExpr),
        Result = evaluate_compile_time_condition_comptime(BaseExpr, Info)
    ;
        TraceExpr = trace_not(ExprA),
        ResultA = evaluate_compile_time_condition(ExprA, Info),
        Result = bool.not(ResultA)
    ;
        TraceExpr = trace_op(Op, ExprA, ExprB),
        ResultA = evaluate_compile_time_condition(ExprA, Info),
        ResultB = evaluate_compile_time_condition(ExprB, Info),
        (
            Op = trace_or,
            Result = bool.or(ResultA, ResultB)
        ;
            Op = trace_and,
            Result = bool.and(ResultA, ResultB)
        )
    ).

:- func evaluate_compile_time_condition_comptime(trace_compiletime,
    simplify_info) = bool.

evaluate_compile_time_condition_comptime(CompTime, Info) = Result :-
    simplify_info_get_module_info(Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    (
        CompTime = trace_flag(FlagName),
        globals.lookup_accumulating_option(Globals, trace_goal_flags, Flags),
        ( if list.member(FlagName, Flags) then
            Result = yes
        else
            Result = no
        )
    ;
        CompTime = trace_grade(Grade),
        (
            Grade = trace_grade_debug,
            globals.lookup_bool_option(Globals, exec_trace, Result)
        ;
            Grade = trace_grade_ssdebug,
            globals.lookup_bool_option(Globals, source_to_source_debug, Result)
            % XXX Should we take into account force_disable_ssdebug as well?
        ;
            Grade = trace_grade_prof,
            globals.lookup_bool_option(Globals, profile_calls, ProfCalls),
            globals.lookup_bool_option(Globals, profile_time, ProfTime),
            globals.lookup_bool_option(Globals, profile_memory, ProfMem),
            bool.or_list([ProfCalls, ProfTime, ProfMem], Result)
        ;
            Grade = trace_grade_profdeep,
            globals.lookup_bool_option(Globals, profile_deep, Result)
        ;
            Grade = trace_grade_par,
            globals.lookup_bool_option(Globals, parallel, Result)
        ;
            Grade = trace_grade_trail,
            globals.lookup_bool_option(Globals, use_trail, Result)
        ;
            Grade = trace_grade_rbmm,
            globals.lookup_bool_option(Globals, use_regions, Result)
        ;
            Grade = trace_grade_llds,
            globals.lookup_bool_option(Globals, highlevel_code, NotResult),
            bool.not(NotResult, Result)
        ;
            Grade = trace_grade_mlds,
            globals.lookup_bool_option(Globals, highlevel_code, Result)
        ;
            Grade = trace_grade_c,
            globals.get_target(Globals, Target),
            ( if Target = target_c then
                Result = yes
            else
                Result = no
            )
        ;
            Grade = trace_grade_csharp,
            globals.get_target(Globals, Target),
            ( if Target = target_csharp then
                Result = yes
            else
                Result = no
            )
        ;
            Grade = trace_grade_java,
            globals.get_target(Globals, Target),
            ( if Target = target_java then
                Result = yes
            else
                Result = no
            )
        )
    ;
        CompTime = trace_trace_level(RequiredLevel),
        simplify_info_get_eff_trace_level_optimized(Info, EffTraceLevel, _),
        (
            RequiredLevel = trace_level_shallow,
            Result = at_least_at_shallow(EffTraceLevel)
        ;
            RequiredLevel = trace_level_deep,
            Result = at_least_at_deep(EffTraceLevel)
        )
    ).

%---------------------------------------------------------------------------%

try_to_merge_nested_scopes(Reason0, InnerGoal0, OuterGoalInfo, Goal) :-
    loop_over_any_nested_scopes(Reason0, Reason, InnerGoal0, InnerGoal),
    InnerGoal = hlds_goal(_, GoalInfo),
    ( if
        Reason = exist_quant(_),
        Detism = goal_info_get_determinism(GoalInfo),
        OuterDetism = goal_info_get_determinism(OuterGoalInfo),
        Detism = OuterDetism
    then
        % If the inner and outer detisms match, then we do not need
        % the `some' scope.
        Goal = InnerGoal
    else
        Goal = hlds_goal(scope(Reason, InnerGoal), OuterGoalInfo)
    ).

:- pred loop_over_any_nested_scopes(scope_reason::in, scope_reason::out,
    hlds_goal::in, hlds_goal::out) is det.

loop_over_any_nested_scopes(Reason0, Reason, Goal0, Goal) :-
    ( if
        Goal0 = hlds_goal(scope(Reason1, Goal1), _),
        ( if
            Reason0 = exist_quant(Vars0),
            Reason1 = exist_quant(Vars1)
        then
            Reason2 = exist_quant(Vars0 ++ Vars1)
        else if
            Reason0 = barrier(Removable0),
            Reason1 = barrier(Removable1)
        then
            ( if
                Removable0 = removable,
                Removable1 = removable
            then
                Removable2 = removable
            else
                Removable2 = not_removable
            ),
            Reason2 = barrier(Removable2)
        else if
            Reason0 = commit(ForcePruning0),
            Reason1 = commit(ForcePruning1)
        then
            ( if
                ForcePruning0 = dont_force_pruning,
                ForcePruning1 = dont_force_pruning
            then
                ForcePruning2 = dont_force_pruning
            else
                ForcePruning2 = force_pruning
            ),
            Reason2 = commit(ForcePruning2)
        else
            fail
        )
    then
        loop_over_any_nested_scopes(Reason2, Reason, Goal1, Goal)
    else
        Reason = Reason0,
        Goal = Goal0
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_goal_scope.
%---------------------------------------------------------------------------%
