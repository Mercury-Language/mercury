%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: code_gen.m.
% Main authors: conway, zs.
%
% The task of this module is to provide a generic predicate that can be called
% from anywhere in the code generator to generate code for a goal. We forward
% most of the actual construction of code for particular types of goals
% to other modules. The generation of code for unifications is done
% by unify_gen, for calls, higher-order calls and method calls by call_gen,
% for commits by commit_gen, for if-then-elses and negations by ite_gen,
% for switches by switch_gen and its subsidiary modules, for disjunctions
% by disj_gen, for parallel conjunctions by par_conj_gen, and for foreign_procs
% by pragma_c_gen. The only goals handled directly by code_gen are sequential
% conjunctions.
%
%---------------------------------------------------------------------------%

:- module ll_backend.code_gen.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.

%---------------------------------------------------------------------------%

    % Translate a HLDS goal to LLDS.
    %
:- pred generate_goal(code_model::in, hlds_goal::in, llds_code::out,
    code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_form.
:- import_module hlds.hlds_desc.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module libs.globals.
:- import_module ll_backend.call_gen.
:- import_module ll_backend.commit_gen.
:- import_module ll_backend.disj_gen.
:- import_module ll_backend.ite_gen.
:- import_module ll_backend.opt_debug.
:- import_module ll_backend.par_conj_gen.
:- import_module ll_backend.pragma_c_gen.
:- import_module ll_backend.switch_gen.
:- import_module ll_backend.unify_gen.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

generate_goal(ContextModel, Goal, Code, !CI) :-
    % Generate a goal. This predicate arranges for the necessary updates of
    % the generic data structures before and after the actual code generation,
    % which is delegated to goal-specific predicates.

    trace [compiletime(flag("codegen_goal")), io(!IO)] (
        some [ModuleInfo, VarSet, GoalDesc] (
            code_info.get_module_info(!.CI, ModuleInfo),
            code_info.get_varset(!.CI, VarSet),
            GoalDesc = describe_goal(ModuleInfo, VarSet, Goal),

            ( should_trace_code_gen(!.CI) ->
                io.format("\nGOAL START: %s\n", [s(GoalDesc)], !IO)
            ;
                true
            )
        )
    ),

    % Make any changes to liveness before Goal.
    get_forward_live_vars(!.CI, ForwardLiveVarsBeforeGoal),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    HasSubGoals = goal_expr_has_subgoals(GoalExpr),
    pre_goal_update(GoalInfo, HasSubGoals, !CI),
    get_instmap(!.CI, InstMap),
    ( instmap_is_reachable(InstMap) ->
        CodeModel = goal_info_get_code_model(GoalInfo),
        % Sanity check: code of some code models should occur
        % only in limited contexts.
        (
            CodeModel = model_det
        ;
            CodeModel = model_semi,
            (
                ContextModel = model_det,
                unexpected($module, $pred, "semidet model in det context")
            ;
                ( ContextModel = model_semi
                ; ContextModel = model_non
                )
            )
        ;
            CodeModel = model_non,
            (
                ( ContextModel = model_det
                ; ContextModel = model_semi
                ),
                unexpected($module, $pred,
                    "nondet model in det/semidet context")
            ;
                ContextModel = model_non
            )
        ),

        generate_goal_expr(GoalExpr, GoalInfo, CodeModel,
            ForwardLiveVarsBeforeGoal, GoalCode, !CI),
        Features = goal_info_get_features(GoalInfo),
        get_proc_info(!.CI, ProcInfo),

        % If the predicate's evaluation method is memo, loopcheck or minimal
        % model, the goal generated the variable that represents the call table
        % tip, *and* tracing is enabled, then we save this variable to its
        % stack slot. This is necessary to enable retries across this procedure
        % to reset the call table entry to uninitialized, effectively removing
        % the call table entry.
        %
        % If tracing is not enabled, then CallTableVar isn't guaranteed
        % to have a stack slot.
        (
            set.member(feature_call_table_gen, Features),
            get_proc_info(!.CI, ProcInfo),
            proc_info_get_call_table_tip(ProcInfo, MaybeCallTableVar),
            MaybeCallTableVar = yes(CallTableVar),
            get_maybe_trace_info(!.CI, yes(_))
        ->
            save_variables_on_stack([CallTableVar], TipSaveCode, !CI),
            CodeUptoTip = GoalCode ++ TipSaveCode
        ;
            CodeUptoTip = GoalCode
        ),
        % After the goal that generates the variables needed at the exception
        % port, on which deep_profiling.m puts the save_deep_excp_vars feature,
        % save those variables in their stack slots. The procedure layout
        % structure gives the identity of their slots, and exception.m
        % expects to find the variables in their stack slots.
        %
        % These variables are computed by the call port code and are needed
        % by the exit and fail port codes, so their lifetime is the entire
        % procedure invocation. If the procedure makes any calls other than
        % the ones inserted by deep profiling, then all the variables will have
        % stack slots, and we save them all on the stack. If the procedure
        % doesn't make any such calls, then the variables won't have stack
        % slots, but they won't *need* stack slots either, since there is no
        % way for such a leaf procedure to throw an exception. (Throwing
        % requires calling exception.throw, directly or indirectly.)
        ( set.member(feature_save_deep_excp_vars, Features) ->
            DeepSaveVars = compute_deep_save_excp_vars(ProcInfo),
            save_variables_on_stack(DeepSaveVars, DeepSaveCode, !CI),
            Code = CodeUptoTip ++ DeepSaveCode
        ;
            Code = CodeUptoTip
        ),

        % Make live any variables which subsequent goals will expect to be
        % live, but were not generated.
        set_instmap(InstMap, !CI),
        post_goal_update(GoalInfo, !CI)
    ;
        Code = empty
    ),
    trace [compiletime(flag("codegen_goal")), io(!IO)] (
        some [ModuleInfo, VarSet, GoalDesc] (
            code_info.get_module_info(!.CI, ModuleInfo),
            code_info.get_varset(!.CI, VarSet),
            GoalDesc = describe_goal(ModuleInfo, VarSet, Goal),

            ( should_trace_code_gen(!.CI) ->
                io.format("\nGOAL FINISH: %s\n", [s(GoalDesc)], !IO),
                Instrs = cord.list(Code),
                write_instrs(Instrs, no, yes, !IO)
            ;
                true
            )
        )
    ).

:- func compute_deep_save_excp_vars(proc_info) = list(prog_var).

compute_deep_save_excp_vars(ProcInfo) = DeepSaveVars :-
    proc_info_get_maybe_deep_profile_info(ProcInfo, MaybeDeepProfInfo),
    (
        MaybeDeepProfInfo = yes(DeepProfInfo),
        MaybeDeepLayout = DeepProfInfo ^ deep_layout,
        MaybeDeepLayout = yes(DeepLayout)
    ->
        ExcpVars = DeepLayout ^ deep_layout_excp,
        ExcpVars = hlds_deep_excp_vars(TopCSDVar, MiddleCSDVar,
            MaybeOldOutermostVar),
        proc_info_get_stack_slots(ProcInfo, StackSlots),
        ( map.search(StackSlots, TopCSDVar, _) ->
            % If one of these variables has a stack slot, the others must
            % have one too.
            (
                MaybeOldOutermostVar = yes(OldOutermostVar),
                DeepSaveVars = [TopCSDVar, MiddleCSDVar, OldOutermostVar]
            ;
                MaybeOldOutermostVar = no,
                DeepSaveVars = [TopCSDVar, MiddleCSDVar]
            )
        ;
            DeepSaveVars = []
        )
    ;
        unexpected($module, $pred, "inconsistent proc_info")
    ).

%---------------------------------------------------------------------------%

:- pred generate_goal_expr(hlds_goal_expr::in, hlds_goal_info::in,
    code_model::in, set_of_progvar::in, llds_code::out,
    code_info::in, code_info::out) is det.

generate_goal_expr(GoalExpr, GoalInfo, CodeModel, ForwardLiveVarsBeforeGoal,
        Code, !CI) :-
    (
        GoalExpr = unify(_, _, _, Uni, _),
        unify_gen.generate_unification(CodeModel, Uni, GoalInfo, Code, !CI)
    ;
        GoalExpr = conj(ConjType, Goals),
        (
            ConjType = plain_conj,
            generate_conj(Goals, CodeModel, cord.init, Code, !CI)
        ;
            ConjType = parallel_conj,
            par_conj_gen.generate_par_conj(Goals, GoalInfo, CodeModel, Code,
                !CI)
        )
    ;
        GoalExpr = disj(Goals),
        disj_gen.generate_disj(CodeModel, Goals, GoalInfo, Code, !CI)
    ;
        GoalExpr = negation(Goal),
        ite_gen.generate_negation(CodeModel, Goal, GoalInfo, Code, !CI)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        ite_gen.generate_ite(CodeModel, Cond, Then, Else, GoalInfo, Code, !CI)
    ;
        GoalExpr = switch(Var, CanFail, CaseList),
        switch_gen.generate_switch(CodeModel, Var, CanFail, CaseList, GoalInfo,
            Code, !CI)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( Reason = from_ground_term(TermVar, from_ground_term_construct) ->
            unify_gen.generate_ground_term(TermVar, SubGoal, !CI),
            Code = empty
        ; Reason = loop_control(LCVar, LCSVar, UseParentStack) ->
            par_conj_gen.generate_lc_spawn_off(SubGoal, LCVar, LCSVar,
                UseParentStack, Code, !CI)
        ;
            commit_gen.generate_scope(Reason, CodeModel, GoalInfo,
                ForwardLiveVarsBeforeGoal, SubGoal, Code, !CI)
        )
    ;
        GoalExpr = generic_call(GenericCall, Args, Modes, MaybeRegTypes, Det),
        call_gen.generate_generic_call(CodeModel, GenericCall, Args,
            Modes, MaybeRegTypes, Det, GoalInfo, Code, !CI)
    ;
        GoalExpr = plain_call(PredId, ProcId, Args, BuiltinState, _, _),
        (
            BuiltinState = not_builtin,
            call_gen.generate_call(CodeModel, PredId, ProcId, Args, GoalInfo,
                Code, !CI)
        ;
            ( BuiltinState = inline_builtin
            ; BuiltinState = out_of_line_builtin
            ),
            call_gen.generate_builtin(CodeModel, PredId, ProcId, Args,
                Code, !CI)
        )
    ;
        GoalExpr = call_foreign_proc(Attributes, PredId, ProcId,
            Args, ExtraArgs, MaybeTraceRuntimeCond, PragmaCode),
        Lang = get_foreign_language(Attributes),
        (
            Lang = lang_c,
            generate_foreign_proc_code(CodeModel, Attributes,
                PredId, ProcId, Args, ExtraArgs, MaybeTraceRuntimeCond,
                PragmaCode, GoalInfo, Code, !CI)
        ;
            ( Lang = lang_java
            ; Lang = lang_csharp
            ; Lang = lang_il
            ; Lang = lang_erlang
            ),
            unexpected($module, $pred, "foreign code other than C")
        )
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($module, $pred, "shorthand")
    ).

%---------------------------------------------------------------------------%

    % Generate a conjoined series of goals. State information flows directly
    % from one conjunct to the next.
    %
    % We call generate_goals to generate code for up to 1000 goals.
    % If there are any more goals left after that, we let generate_goals
    % give up all its stack frames before calling it again. This allows us
    % to generate code for *very* long sequences of goals even if the compiler
    % is compiled in a grade that does not allow tail recursion.
    %
:- pred generate_conj(list(hlds_goal)::in, code_model::in,
    llds_code::in, llds_code::out, code_info::in, code_info::out) is det.

generate_conj([], _, !Code, !CI).
generate_conj(Goals @ [_ | _], CodeModel, !Code, !CI) :-
    generate_goals(Goals, 1000, CodeModel, LeftOverGoals, !Code, !CI),
    generate_conj(LeftOverGoals, CodeModel, !Code, !CI).

:- pred generate_goals(list(hlds_goal)::in, int::in, code_model::in,
    list(hlds_goal)::out, llds_code::in, llds_code::out,
    code_info::in, code_info::out) is det.

generate_goals([], _, _, [], !Code, !CI).
generate_goals([Goal | Goals], N, CodeModel, LeftOverGoals, !Code, !CI) :-
    ( N > 0 ->
        generate_goal(CodeModel, Goal, GoalCode, !CI),
        !:Code = !.Code ++ GoalCode,
        get_instmap(!.CI, Instmap),
        ( instmap_is_unreachable(Instmap) ->
            LeftOverGoals = []
        ;
            generate_goals(Goals, N - 1, CodeModel, LeftOverGoals, !Code, !CI)
        )
    ;
        LeftOverGoals = [Goal | Goals]
    ).

%---------------------------------------------------------------------------%
:- end_module ll_backend.code_gen.
%---------------------------------------------------------------------------%
