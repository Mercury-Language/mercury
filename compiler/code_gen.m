%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
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
:- pred generate_goal(code_model::in, hlds_goal::in, code_tree::out,
    code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.tree.
:- import_module ll_backend.call_gen.
:- import_module ll_backend.code_util.
:- import_module ll_backend.commit_gen.
:- import_module ll_backend.disj_gen.
:- import_module ll_backend.ite_gen.
:- import_module ll_backend.middle_rec.
:- import_module ll_backend.par_conj_gen.
:- import_module ll_backend.pragma_c_gen.
:- import_module ll_backend.switch_gen.
:- import_module ll_backend.unify_gen.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.

%---------------------------------------------------------------------------%

generate_goal(ContextModel, Goal - GoalInfo, Code, !CI) :-
    % Generate a goal. This predicate arranges for the necessary updates of
    % the generic data structures before and after the actual code generation,
    % which is delegated to goal-specific predicates.

    % Make any changes to liveness before Goal
    ( goal_is_atomic(Goal) ->
        IsAtomic = yes
    ;
        IsAtomic = no
    ),
    code_info.pre_goal_update(GoalInfo, IsAtomic, !CI),
    code_info.get_instmap(!.CI, InstMap),
    ( instmap.is_reachable(InstMap) ->
        goal_info_get_code_model(GoalInfo, CodeModel),
        % Sanity check: code of some code models should occur
        % only in limited contexts.
        (
            CodeModel = model_det
        ;
            CodeModel = model_semi,
            ( ContextModel \= model_det ->
                true
            ;
                unexpected(this_file, "semidet model in det context")
            )
        ;
            CodeModel = model_non,
            ( ContextModel = model_non ->
                true
            ;
                unexpected(this_file, "nondet model in det/semidet context")
            )
        ),

        generate_goal_2(Goal, GoalInfo, CodeModel, GoalCode, !CI),
        goal_info_get_features(GoalInfo, Features),
        code_info.get_proc_info(!.CI, ProcInfo),

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
            code_info.get_proc_info(!.CI, ProcInfo),
            proc_info_get_call_table_tip(ProcInfo, MaybeCallTableVar),
            MaybeCallTableVar = yes(CallTableVar),
            code_info.get_maybe_trace_info(!.CI, yes(_))
        ->
            code_info.save_variables_on_stack([CallTableVar], TipSaveCode,
                !CI),
            CodeUptoTip = tree(GoalCode, TipSaveCode)
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
            Code = tree(CodeUptoTip, DeepSaveCode)
        ;
            Code = CodeUptoTip
        ),

        % Make live any variables which subsequent goals will expect to be
        % live, but were not generated.
        code_info.set_instmap(InstMap, !CI),
        code_info.post_goal_update(GoalInfo, !CI)
    ;
        Code = empty
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
        unexpected(this_file,
            "compute_deep_save_excp_vars: inconsistent proc_info")
    ).

%---------------------------------------------------------------------------%

:- pred generate_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
    code_model::in, code_tree::out, code_info::in, code_info::out) is det.

generate_goal_2(GoalExpr, GoalInfo, CodeModel, Code, !CI) :-
    (
        GoalExpr = unify(_, _, _, Uni, _),
        unify_gen.generate_unification(CodeModel, Uni, GoalInfo, Code, !CI)
    ;
        GoalExpr = conj(ConjType, Goals),
        (
            ConjType = plain_conj,
            generate_goals(Goals, CodeModel, Code, !CI)
        ;
            ConjType = parallel_conj,
            par_conj_gen.generate_par_conj(Goals, GoalInfo, CodeModel, Code,
                !CI)
        )
    ;
        GoalExpr = disj(Goals),
        AddTrailOps = should_add_trail_ops(!.CI, GoalInfo),
        disj_gen.generate_disj(AddTrailOps, CodeModel, Goals, GoalInfo, Code,
            !CI)
    ;
        GoalExpr = negation(Goal),
        AddTrailOps = should_add_trail_ops(!.CI, GoalInfo),
        ite_gen.generate_negation(AddTrailOps, CodeModel, Goal, GoalInfo,
            Code, !CI)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        AddTrailOps = should_add_trail_ops(!.CI, GoalInfo),
        ite_gen.generate_ite(AddTrailOps, CodeModel, Cond, Then, Else,
            GoalInfo, Code, !CI)
    ;
        GoalExpr = switch(Var, CanFail, CaseList),
        switch_gen.generate_switch(CodeModel, Var, CanFail, CaseList, GoalInfo,
            Code, !CI)
    ;
        GoalExpr = scope(Reason, Goal),
        AddTrailOps = should_add_trail_ops(!.CI, GoalInfo),
        commit_gen.generate_scope(Reason, AddTrailOps, CodeModel, Goal, Code,
            !CI)
    ;
        GoalExpr = generic_call(GenericCall, Args, Modes, Det),
        call_gen.generate_generic_call(CodeModel, GenericCall, Args,
            Modes, Det, GoalInfo, Code, !CI)
    ;
        GoalExpr = plain_call(PredId, ProcId, Args, BuiltinState, _, _),
        ( BuiltinState = not_builtin ->
            call_gen.generate_call(CodeModel, PredId, ProcId, Args,
                GoalInfo, Code, !CI)
        ;
            call_gen.generate_builtin(CodeModel, PredId, ProcId, Args,
                Code, !CI)
        )
    ;
        GoalExpr = call_foreign_proc(Attributes, PredId, ProcId,
            Args, ExtraArgs, MaybeTraceRuntimeCond, PragmaCode),
        Lang = get_foreign_language(Attributes),
        (   Lang = lang_c,
            generate_pragma_c_code(CodeModel, Attributes,
                PredId, ProcId, Args, ExtraArgs, MaybeTraceRuntimeCond,
                PragmaCode, GoalInfo, Code, !CI)
        ;
            ( Lang = lang_java
            ; Lang = lang_csharp
            ; Lang = lang_managed_cplusplus
            ; Lang = lang_il
            ),
            unexpected(this_file,
                "generate_goal_2: foreign code other than C unexpected")
        )
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected(this_file, "generate_goal_2: unexpected shorthand")
    ).

%---------------------------------------------------------------------------%

    % Generate a conjoined series of goals. Note of course, that with a
    % conjunction, state information flows directly from one conjunct
    % to the next.
    %
:- pred generate_goals(hlds_goals::in, code_model::in,
    code_tree::out, code_info::in, code_info::out) is det.

generate_goals([], _, empty, !CI).
generate_goals([Goal | Goals], CodeModel, Code, !CI) :-
    generate_goal(CodeModel, Goal, Code1, !CI),
    code_info.get_instmap(!.CI, Instmap),
    ( instmap.is_unreachable(Instmap) ->
        Code = Code1
    ;
        generate_goals(Goals, CodeModel, Code2, !CI),
        Code = tree(Code1, Code2)
    ).

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "code_gen.m".

%---------------------------------------------------------------------------%
