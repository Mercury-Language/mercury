%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_code_gen.m.
% Main author: fjh.
%
% MLDS code generation -- convert from HLDS to MLDS.
%
% This module is an alternative to the original code generator.
% The original code generator compiles from HLDS to LLDS, generating
% very low-level code. This code generator instead compiles to MLDS,
% generating much higher-level code than the original code generator.
%
% One of the aims of the MLDS is to be able to generated human-readable
% code in languages like C or Java. This means that unlike the LLDS back-end,
% we do not want to rely on macros or conditional compilation. If the
% final code is going to depend on the setting of some compilation option,
% our philosophy is to reflect that change in the generated MLDS and C code
% where possible, rather than generating C code which calls macros that do
% different things in different grades. This is important both for
% readability of the generated code, and to make sure that we can easily
% adapt the MLDS code generator to target languages like Java that don't
% support macros or conditional compilation.
%
% A big challenge in generating MLDS code is handling nondeterminism.
% For nondeterministic procedures, we generate code using an explicit
% continuation passing style. Each nondeterministic procedures gets
% translated into a function which takes an extra parameter which is a
% function pointer that points to the success continuation. On success,
% the function calls its success continuation, and on failure it returns.
%
% To keep things easy, this pass generates code which may contain nested
% functions; if the target language doesn't support nested functions (or
% doesn't support them _efficiently_) then a later MLDS->MLDS simplification
% pass will convert it to a form that does not use nested functions.
%
% Note that when we take the address of a nested function, we only ever
% do two things with it: pass it as a continuation argument, or call it.
% The continuations are never returned and never stored inside heap objects
% or global variables. These conditions are sufficient to ensure that
% we never keep the address of a nested function after the containing
% functions has returned, so we won't get any dangling continuations.
%
%---------------------------------------------------------------------------%
% CODE GENERATION SUMMARY
%---------------------------------------------------------------------------%
%
% In each procedure, we declare a local variable `MR_bool succeeded'.
% This is used to hold the success status of semidet sub-goals.
% Note that the comments below show local declarations for the
% `succeeded' variable in all the places where they would be
% needed if we were generating them locally, but currently
% we actually just generate a single `succeeded' variable for
% each procedure.
%
% The calling convention for sub-goals is as follows.
%
%   model_det goal:
%       On success, fall through.
%       (May clobber `succeeded'.)
%   model_semi goal:
%       On success, set `succeeded' to MR_TRUE and fall through.
%       On failure, set `succeeded' to MR_FALSE and fall through.
%   multi/nondet goal:
%       On success, call the current success continuation.
%       On failure, fall through.
%       (May clobber `succeeded' in either case.)
%
% In comments, we use the following notation to distinguish between
% these three.
%
%   model_det goal:
%       <do Goal>
%           This means execute Goal (which must be model_det).
%   model_semi goal:
%       <succeeded = Goal>
%           This means execute Goal, and set `succeeded' to
%           MR_TRUE if the goal succeeds and MR_FALSE if it fails.
%   model_non goal:
%       <Goal && CONT()>
%           This means execute Goal, calling the success
%           continuation function CONT() if it succeeds,
%           and falling through if it fails.
%
% The notation
%
%   [situation]:
%       <[construct]>
%   ===>
%       [code]
%
% means that in the situation described by [situation],
% for the specified [construct] we will generate the specified [code].
%
% There is one other important thing which can be considered part of the
% calling convention for the code that we generate for each goal.
% If static ground term optimization is enabled, then for the terms
% marked as static by mark_static_terms.m, we will generate static consts.
% These static consts can refer to other static consts defined earlier.
% We need to be careful about the scopes of variables to ensure that
% for any term that mark_static_terms.m marks as static, the C constants
% representing the arguments of that term are in scope at the point
% where that term is constructed. Basically this means that
% all the static consts generated inside a goal must be hoist out to
% the top level scope for that goal, except for goal types where
% goal_expr_mark_static_terms (in mark_static_terms.m) returns the
% same static_info unchanged, i.e. branched goals and negations.
%
% Handling static constants also requires that the calls to ml_gen_goal
% for each subgoal must be done in the right order, so that the
% const_num_map in the ml_gen_info holds the right sequence numbers
% for the constants in scope.
%
%---------------------------------------------------------------------------%
%
% Code for wrapping goals
%
% If a model_foo goal occurs in a model_bar context, where foo != bar,
% then we need to modify the code that we emit for the goal so that
% it conforms to the calling convention expected for model_bar.
%
%   det goal in semidet context:
%       <succeeded = Goal>
%   ===>
%       <do Goal>
%       succeeded = MR_TRUE;
%
%   det goal in nondet context:
%       <Goal && SUCCEED()>
%   ===>
%       <do Goal>
%       SUCCEED();
%
%   semi goal in nondet context:
%       <Goal && SUCCEED()>
%   ===>
%       MR_bool succeeded;
%
%       <succeeded = Goal>
%       if (succeeded) SUCCEED();
%
%---------------------------------------------------------------------------%
%
% Code for empty conjunctions (`true')
%
%
%   model_det goal:
%       <do true>
%   ===>
%       /* fall through */
%
%   model_semi goal:
%       <succeeded = true>
%   ===>
%       succeeded = MR_TRUE;
%
%   model_non goal
%       <true && CONT()>
%   ===>
%       CONT();
%
%---------------------------------------------------------------------------%
%
% Code for non-empty conjunctions
%
%
% We need to handle the case where the first goal cannot succeed
% specially:
%
%   at_most_zero Goal:
%       <Goal, Goals>
%   ===>
%       <Goal>
%
% The remaining cases for conjunction all assume that the first
% goal's determinism is not `erroneous' or `failure'.
%
% If the first goal is model_det, it is straight-forward:
%
%   model_det Goal:
%       <Goal, Goals>
%   ===>
%       <do Goal>
%       <Goals>
%
% If the first goal is model_semidet, then there are two cases:
% if the conj as a whole is semidet, things are simple, and
% if the conj as a whole is model_non, then we do the same as
% for the semidet case, except that we also (ought to) declare
% a local `succeeded' variable.
%
%   model_semi Goal in model_semi conj:
%       <succeeded = (Goal, Goals)>
%   ===>
%       <succeeded = Goal>;
%       if (succeeded) {
%           <Goals>;
%       }
%
%   model_semi Goal in model_non conj:
%       <Goal && Goals>
%   ===>
%       MR_bool succeeded;
%
%       <succeeded = Goal>;
%       if (succeeded) {
%           <Goals>;
%       }
%
% The actual code generation scheme we use is slightly different to that:
% we hoist any declarations generated for <Goals> to the outer scope,
% rather than keeping them inside the `if', so that they remain in scope
% for any later goals which follow this. This is needed for declarations
% of static consts.
%
% For model_non goals, there are a couple of different ways that we could
% generate code, depending on whether we are aiming to maximize readability,
% or whether we prefer to generate code that may be more efficient but is
% a little less readable. The more readable method puts the generated goals
% in the same order that they occur in the source code:
%
%   model_non Goal (optimized for readability)
%       <Goal, Goals>
%   ===>
%       entry_func() {
%           <Goal && succ_func()>;
%       }
%       succ_func() {
%           <Goals && SUCCEED()>;
%       }
%
%       entry_func();
%
% The more efficient method generates the goals in reverse order, so it is less
% readable, but it has fewer function calls and can make it easier for the C
% compiler to inline things:
%
%   model_non Goal (optimized for efficiency):
%       <Goal, Goals>
%   ===>
%       succ_func() {
%           <Goals && SUCCEED()>;
%       }
%
%       <Goal && succ_func()>;
%
% The more efficient method is the one we actually use.
%
% Here is how those two methods look on longer conjunctions of nondet goals:
%
%   model_non goals (optimized for readability):
%       <Goal1, Goal2, Goal3, Goals>
%   ===>
%       label0_func() {
%           <Goal1 && label1_func()>;
%       }
%       label1_func() {
%           <Goal2 && label2_func()>;
%       }
%       label2_func() {
%           <Goal3 && label3_func()>;
%       }
%       label3_func() {
%           <Goals && SUCCEED()>;
%       }
%
%       label0_func();
%
%   model_non goals (optimized for efficiency):
%       <Goal1, Goal2, Goal3, Goals>
%   ===>
%       label1_func() {
%           label2_func() {
%               label3_func() {
%                   <Goals && SUCCEED()>;
%               }
%               <Goal3 && label3_func()>;
%           }
%           <Goal2 && label2_func()>;
%       }
%       <Goal1 && label1_func()>;
%
% Note that it might actually make more sense to generate conjunctions
% of nondet goals like this:
%
%   model_non goals (optimized for efficiency, alternative version):
%       <Goal1, Goal2, Goal3, Goals>
%   ===>
%       label3_func() {
%           <Goals && SUCCEED()>;
%       }
%       label2_func() {
%           <Goal3 && label3_func()>;
%       }
%       label1_func() {
%           <Goal2 && label2_func()>;
%       }
%
%       <Goal1 && label1_func()>;
%
% This would avoid the undesirable deep nesting that we sometimes get with
% our current scheme. However, if we are eliminating nested functions, as is
% normally the case, then after the ml_elim_nested transformation all the
% functions and variables have been hoisted to the top level, so there is
% no difference between these two.
%
% As with semidet conjunctions, we hoist declarations out so that they remain
% in scope for any following goals. This is needed for declarations of static
% consts. However, we want to keep the declarations of non-static variables
% local, since accessing local variables is more efficient that accessing
% variables in the environment from a nested function. So we only hoist
% declarations of static constants.
%
%---------------------------------------------------------------------------%
%
% Code for if-then-else
%
%
%   model_det Cond:
%       <(Cond -> Then ; Else)>
%   ===>
%       <Cond>
%       <Then>
%
%   model_semi Cond:
%       <(Cond -> Then ; Else)>
%   ===>
%       MR_bool succeeded;
%
%       <succeeded = Cond>
%       if (succeeded) {
%           <Then>
%       } else {
%           <Else>
%       }
%
% XXX The following transformation does not do as good a job of GC as it could.
% Ideally we ought to ensure that stuff used only in the `Else' part will be
% reclaimed if a GC occurs during the `Then' part. But that is a bit tricky
% to achieve.
%
%   model_non Cond:
%       <(Cond -> Then ; Else)>
%   ===>
%       MR_bool cond_<N>;
%
%       void then_func() {
%           cond_<N> = MR_TRUE;
%           <Then>
%       }
%
%       cond_<N> = MR_FALSE;
%       <Cond && then_func()>
%       if (!cond_<N>) {
%           <Else>
%       }
%
% except that we hoist any declarations generated for <Cond> to the top
% of the scope, so that they are in scope for the <Then> goal
% (this is needed for declarations of static consts).
%
%---------------------------------------------------------------------------%
%
% Code for negation
%
%
% model_det negation
%       <not(Goal)>
%   ===>
%       MR_bool succeeded;
%       <succeeded = Goal>
%       /* now ignore the value of succeeded,
%        which we know will be MR_FALSE */
%
% model_semi negation, model_det Goal:
%       <succeeded = not(Goal)>
%   ===>
%       <do Goal>
%       succeeded = MR_FALSE;
%
% model_semi negation, model_semi Goal:
%       <succeeded = not(Goal)>
%   ===>
%       <succeeded = Goal>
%       succeeded = !succeeded;
%
%---------------------------------------------------------------------------%
%
% This back-end is still not yet 100% complete.
%
% TODO:
%   - XXX define compare & unify preds for RTTI types
%   - XXX need to generate correct layout information for closures
%     so that tests/hard_coded/copy_pred works.
%   - XXX fix ANSI/ISO C conformance of the generated code
%
% UNIMPLEMENTED FEATURES:
%   - support genuine parallel conjunction
%
% POTENTIAL EFFICIENCY IMPROVEMENTS:
%   - generate local declarations for the `succeeded' variable;
%     this would help in nondet code, because it would avoid
%     the need to access the outermost function's `succeeded'
%     variable via the environment pointer
%     (be careful about the interaction with setjmp(), though)
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_code_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module hlds.vartypes.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Generate MLDS code for the specified goal in the specified code model.
    % Return the result as a single statement (which may be a block statement
    % containing nested declarations).
    %
:- pred ml_gen_goal_as_block(code_model::in, hlds_goal::in, mlds_stmt::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Generate code for a goal that is one branch of a branched control
    % structure. At the end of the branch, we need to forget what we learned
    % during the branch about which variables are bound to constants,
    % since those variables may not be bound to constants (at least not the
    % same constants) in parallel branches, or in code after the branched
    % control if control did not go through this branch.
    %
:- pred ml_gen_goal_as_branch(code_model::in, hlds_goal::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.
:- pred ml_gen_goal_as_branch_block(code_model::in, hlds_goal::in,
    mlds_stmt::out, ml_gen_info::in, ml_gen_info::out) is det.

    % Generate MLDS code for the specified goal in the specified code model.
    % Return the result as two lists, one containing the necessary declarations
    % and the other containing the generated statements.
    %
:- pred ml_gen_goal(code_model::in, hlds_goal::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

    % ml_gen_maybe_convert_goal_code_model(OuterCodeModel, InnerCodeModel,
    %   Context, Stmts0, Stmts, !Info):
    %
    % OuterCodeModel is the code model expected by the context in which a goal
    % is called. InnerCodeModel is the code model which the goal actually has.
    % This predicate converts the code generated for the goal using
    % InnerCodeModel (Stmts0) into code that uses the calling convention
    % appropriate for OuterCodeModel (Stmts).
    %
:- pred ml_gen_maybe_convert_goal_code_model(code_model::in, code_model::in,
    prog_context::in, list(mlds_stmt)::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Generate declarations for a list of local variables.
    %
:- pred ml_gen_local_var_decls(prog_varset::in, vartypes::in,
    prog_context::in, list(prog_var)::in, list(mlds_local_var_defn)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module ml_backend.ml_call_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_commit_gen.
:- import_module ml_backend.ml_disj_gen.
:- import_module ml_backend.ml_foreign_proc_gen.
:- import_module ml_backend.ml_switch_gen.
:- import_module ml_backend.ml_unify_gen.
:- import_module ml_backend.ml_unify_gen_construct.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.set_of_var.

:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Generate code for goals.
%

ml_gen_goal_as_block(CodeModel, Goal, Stmt, !Info) :-
    ml_gen_goal(CodeModel, Goal, LocalVarDefns, FuncDefns, Stmts, !Info),
    Goal = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    Stmt = ml_gen_block(LocalVarDefns, FuncDefns, Stmts, Context).

ml_gen_goal_as_branch(CodeModel, Goal, LocalVarDefns, FuncDefns, Stmts,
        !Info) :-
    ml_gen_info_get_const_var_map(!.Info, InitConstVarMap),
    ml_gen_goal(CodeModel, Goal, LocalVarDefns, FuncDefns, Stmts, !Info),
    ml_gen_info_set_const_var_map(InitConstVarMap, !Info).

ml_gen_goal_as_branch_block(CodeModel, Goal, Stmt, !Info) :-
    ml_gen_info_get_const_var_map(!.Info, InitConstVarMap),
    ml_gen_goal_as_block(CodeModel, Goal, Stmt, !Info),
    ml_gen_info_set_const_var_map(InitConstVarMap, !Info).

%---------------------------------------------------------------------------%

ml_gen_goal(CodeModel, Goal, LocalVarDefns, FuncDefns, Stmts, !Info) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),

    % Generate the local variables for this goal.
    ml_gen_info_get_var_types(!.Info, VarTypes),
    find_vars_to_declare(VarTypes, GoalExpr, GoalInfo, VarsToDeclare),

    ml_gen_info_get_varset(!.Info, VarSet),
    Context = goal_info_get_context(GoalInfo),
    ml_gen_local_var_decls(VarSet, VarTypes, Context, VarsToDeclare, VarDefns,
        !Info),

    % Generate code for the goal in its own code model.
    GoalCodeModel = goal_info_get_code_model(GoalInfo),
    ml_gen_goal_expr(GoalExpr, GoalCodeModel, Context, GoalInfo,
        GoalLocalVarDefns, GoalFuncDefns, GoalStmts0, !Info),

    % Add whatever wrapper is needed to convert the goal's code model
    % to the desired code model.
    ml_gen_maybe_convert_goal_code_model(CodeModel, GoalCodeModel, Context,
        GoalStmts0, GoalStmts, !Info),

    LocalVarDefns = VarDefns ++ GoalLocalVarDefns,
    FuncDefns = GoalFuncDefns,
    Stmts = GoalStmts.

%---------------------------------------------------------------------------%

    % Generate MLDS code for the different kinds of HLDS goals.
    %
:- pred ml_gen_goal_expr(hlds_goal_expr::in, code_model::in, prog_context::in,
    hlds_goal_info::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_goal_expr(GoalExpr, CodeModel, Context, GoalInfo,
        LocalVarDefns, FuncDefns, Stmts, !Info) :-
    (
        GoalExpr = unify(_LHS, _RHS, _Mode, Unification, _UnifyContext),
        ml_generate_unification(CodeModel, Unification, Context,
            LocalVarDefns, Stmts, !Info),
        FuncDefns = []
    ;
        GoalExpr = plain_call(PredId, ProcId, ArgVars, BuiltinState, _, _),
        (
            BuiltinState = not_builtin,
            ml_gen_plain_call(PredId, ProcId, CodeModel, GoalInfo, ArgVars,
                LocalVarDefns, FuncDefns, Stmts, !Info)
        ;
            BuiltinState = inline_builtin,
            ml_gen_builtin(PredId, ProcId, ArgVars, CodeModel, Context,
                LocalVarDefns, FuncDefns, Stmts, !Info)
        )
    ;
        GoalExpr = generic_call(GenericCall, Vars, Modes, _, Detism),
        determinism_to_code_model(Detism, CallCodeModel),
        expect(unify(CodeModel, CallCodeModel), $module, $pred,
            "code model mismatch"),
        ml_gen_generic_call(GenericCall, Vars, Modes, Detism, Context,
            LocalVarDefns, FuncDefns, Stmts, !Info)
    ;
        GoalExpr = call_foreign_proc(Attributes, PredId, ProcId,
            Args, ExtraArgs, MaybeTraceRuntimeCond, PragmaImpl),
        PragmaImpl = fp_impl_ordinary(ForeignCode, MaybeContext),
        (
            MaybeContext = yes(ContextToUse)
        ;
            MaybeContext = no,
            ContextToUse = Context
        ),
        (
            MaybeTraceRuntimeCond = no,
            ml_gen_foreign_proc(CodeModel, Attributes,
                PredId, ProcId, Args, ExtraArgs, ForeignCode,
                ContextToUse, LocalVarDefns, Stmts, !Info),
            FuncDefns = []
        ;
            MaybeTraceRuntimeCond = yes(TraceRuntimeCond),
            ml_gen_trace_runtime_cond(TraceRuntimeCond, ContextToUse,
                Stmts, !Info),
            LocalVarDefns = [],
            FuncDefns = []
        )
    ;
        GoalExpr = conj(_ConjType, Goals),
        % XXX Currently we treat parallel conjunction the same as
        % sequential conjunction -- parallelism is not yet implemented.
        ml_gen_conj(Goals, CodeModel, Context, LocalVarDefns, FuncDefns, Stmts,
            !Info)
    ;
        GoalExpr = disj(Goals),
        ml_gen_disj(Goals, GoalInfo, CodeModel, Context, Stmts, !Info),
        LocalVarDefns = [],
        FuncDefns = []
    ;
        GoalExpr = switch(Var, CanFail, CasesList),
        ml_gen_switch(Var, CanFail, CasesList, CodeModel, Context, GoalInfo,
            LocalVarDefns, Stmts, !Info),
        FuncDefns = []
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        ml_gen_ite(CodeModel, Cond, Then, Else, Context,
            LocalVarDefns, FuncDefns, Stmts, !Info)
    ;
        GoalExpr = negation(SubGoal),
        ml_gen_negation(SubGoal, CodeModel, Context,
            LocalVarDefns, FuncDefns, Stmts, !Info)
    ;
        GoalExpr = scope(Reason, SubGoal),
        (
            Reason = from_ground_term(TermVar, from_ground_term_construct),
            ml_generate_ground_term(TermVar, SubGoal, Stmts, !Info),
            LocalVarDefns = [],
            FuncDefns = []
        ;
            ( Reason = from_ground_term(_, from_ground_term_deconstruct)
            ; Reason = from_ground_term(_, from_ground_term_other)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ; Reason = trace_goal(_, _, _, _, _)
            ),
            ml_gen_goal(CodeModel, SubGoal, LocalVarDefns, FuncDefns, Stmts,
                !Info)
        ;
            Reason = disable_warnings(HeadWarning, TailWarnings),
            ml_gen_info_get_disabled_warnings(!.Info, Warnings0),
            set.insert_list([HeadWarning | TailWarnings], Warnings0, Warnings),
            ml_gen_info_set_disabled_warnings(Warnings, !Info),
            ml_gen_goal(CodeModel, SubGoal, LocalVarDefns, FuncDefns, Stmts,
                !Info),
            ml_gen_info_set_disabled_warnings(Warnings0, !Info)
        ;
            ( Reason = exist_quant(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = promise_solutions(_, _)
            ),
            ml_gen_commit(SubGoal, CodeModel, Context,
                LocalVarDefns, FuncDefns, Stmts, !Info)
        ;
            Reason = loop_control(_, _, _),
            % This hasn't been implemented for the MLDS backend yet.
            unexpected($module, "loop_control NYI")
        ;
            Reason = from_ground_term(_, from_ground_term_initial),
            % These should have been replaced by one of the other
            % from_ground_term_* scopes.
            unexpected($module, "unexpected from_ground_term_initial")
        )
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($module, "unexpected shorthand")
    ).

%---------------------------------------------------------------------------%

    % For any given goal, we need to declare any variables which are local
    % to this goal (including its subgoals), but which are not local to
    % a subgoal. If they are local to a subgoal, they will be declared when
    % we generate code for that subgoal.
    %
    % We need to make sure that we declare any type_info or type_classinfo
    % variables *before* any other variables, since the GC tracing code
    % for the other variables may refer to the type_info variables, so they
    % need to be in scope.
    %
    % However, in the common case that the number of variables to declare is
    % zero or one, such reordering is guaranteed to be a no-op, so avoid the
    % expense.
    %
:- pred find_vars_to_declare(vartypes::in,
    hlds_goal_expr::in, hlds_goal_info::in, list(prog_var)::out) is det.

find_vars_to_declare(VarTypes, GoalExpr, GoalInfo, VarsToDeclare) :-
    goal_expr_find_subgoal_nonlocals(GoalExpr, SubGoalNonLocals),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    set_of_var.difference(SubGoalNonLocals, NonLocals, VarsToDeclareSet),
    set_of_var.to_sorted_list(VarsToDeclareSet, VarsToDeclare0),
    (
        ( VarsToDeclare0 = []
        ; VarsToDeclare0 = [_]
        ),
        VarsToDeclare = VarsToDeclare0
    ;
        VarsToDeclare0 = [_, _ | _],
        VarsToDeclare = put_typeinfo_vars_first(VarsToDeclare0, VarTypes)
    ).

    % The task of this predicate is to help compute the set of MLDS variables
    % that should be declared at the scope of GoalExpr. This should be the
    % set of variables that
    %
    % - do not occur outside GoalExpr, since if they did, they would have to be
    %   declared in a larger scope containing GoalExpr; and
    %
    % - need to be declared at the scope of GoalExpr, since they cannot be
    %   declared in a scope inside GoalExpr.
    %
    % Our caller will take care of the first point by deleting the nonlocals
    % set of GoalExpr from the SubGoalNonLocals we return, which means that
    % we can include any variable from GoalExpr's nonlocals set in
    % SubGoalNonLocals without affecting the final outcome.
    %
    % If GoalExpr is a compound goal, a variable that occurs in GoalExpr
    % can be declared in a smaller scope that GoalExpr if it occurs inside
    % a single one of GoalExpr's subgoals. In this case, we therefore return
    % the union of the nonlocals sets of GoalExpr's direct subgoals.
    %
    % If GoalExpr is an atomic goal, there is no smaller scope, but we do have
    % to declare at GoalExpr's scope any MLDS variables that GoalExpr refers to
    % but are not visible GoalExpr. This can happen e.g. with ignored output
    % arguments from calls and unifications.
    %
:- pred goal_expr_find_subgoal_nonlocals(hlds_goal_expr::in,
    set_of_progvar::out) is det.

goal_expr_find_subgoal_nonlocals(GoalExpr, SubGoalNonLocals) :-
    (
        GoalExpr = unify(_LHS, _RHS, _Mode, Unification, _UnifyContext),
        (
            Unification = construct(LHSVar, _ConsId, ArgVars, _ArgModes,
                _HowToConstruct, _Unique, _SubInfo),
            % _HowToConstruct can contain a var specifying to a cell to reuse
            % or a region to construct the term in, but both of those require
            % that variable to be nonlocal to GoalExpr, which means that they
            % would be subtracted from SubGoalNonLocals by our caller anyway.
            SubGoalNonLocals = set_of_var.list_to_set([LHSVar | ArgVars])
        ;
            Unification = deconstruct(LHSVar, _ConsId, ArgVars, _ArgModes,
                _CanFail, _CanCGC),
            SubGoalNonLocals = set_of_var.list_to_set([LHSVar | ArgVars])
        ;
            Unification = assign(LHSVar, RHSVar),
            SubGoalNonLocals = set_of_var.list_to_set([LHSVar, RHSVar])
        ;
            Unification = simple_test(LHSVar, RHSVar),
            SubGoalNonLocals = set_of_var.list_to_set([LHSVar, RHSVar])
        ;
            Unification = complicated_unify(_, _, _),
            unexpected($module, $pred, "complicated_unify")
        )
    ;
        GoalExpr = plain_call(_PredId, _ProcId, ArgVars, _Builtin,
            _Unify_context, _SymName),
        SubGoalNonLocals = set_of_var.list_to_set(ArgVars)
    ;
        GoalExpr = generic_call(GenericCall, ArgVars, _Modes, _MaybeArgRegs,
            _Detism),
        (
            GenericCall = higher_order(HOVar, _Purity, _Kind, _Arity),
            SubGoalNonLocals = set_of_var.list_to_set([HOVar | ArgVars])
        ;
            GenericCall = class_method(MethodVar, _MethodNum, _MethodClassId,
                _Name),
            SubGoalNonLocals = set_of_var.list_to_set([MethodVar | ArgVars])
        ;
            GenericCall = event_call(_Eventname),
            SubGoalNonLocals = set_of_var.list_to_set(ArgVars)
        ;
            GenericCall = cast(_CastKind),
            SubGoalNonLocals = set_of_var.list_to_set(ArgVars)
        )
    ;
        GoalExpr = call_foreign_proc(_Attr, _PredId, _ProcId, Args, ExtraArgs,
            _TraceCond, _Impl),
        ArgVars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        SubGoalNonLocals = set_of_var.list_to_set(ExtraVars ++ ArgVars)
    ;
        ( GoalExpr = negation(SubGoal)
        ; GoalExpr = scope(_Reason, SubGoal)
        ),
        % If _Reason = from_ground_term, the TermVar in it is guaranteed by
        % construction to be nonlocal, so there is no need to add it
        % separately.
        % If _Reason = exist_quant(Vars), the variables in Vars are ignored by
        % the code generator.
        SubGoalNonLocals = goal_get_nonlocals(SubGoal)
    ;
        ( GoalExpr = conj(_, SubGoals)
        ; GoalExpr = disj(SubGoals)
        ),
        goals_find_subgoal_nonlocals(SubGoals,
            set_of_var.init, SubGoalNonLocals)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        % The value of _Vars is not guaranteed to contain the set of variables
        % shared between only Cond and Then.
        goals_find_subgoal_nonlocals([Cond, Then, Else],
            set_of_var.init, SubGoalNonLocals)
    ;
        GoalExpr = switch(_Var, _CanFail, Cases),
        % _Var must be nonlocal; if it weren't, there would have been a mode
        % error (no producer for _Var before a consumer, namely this switch).
        cases_find_subgoal_nonlocals(Cases, set_of_var.init, SubGoalNonLocals)
    ;
        GoalExpr = shorthand(_),
        unexpected($module, $pred, "shorthand")
    ).

:- pred goals_find_subgoal_nonlocals(list(hlds_goal)::in,
    set_of_progvar::in, set_of_progvar::out) is det.

goals_find_subgoal_nonlocals([], !SubGoalNonLocals).
goals_find_subgoal_nonlocals([SubGoal | SubGoals], !SubGoalNonLocals) :-
    NonLocals = goal_get_nonlocals(SubGoal),
    set_of_var.union(NonLocals, !SubGoalNonLocals),
    goals_find_subgoal_nonlocals(SubGoals, !SubGoalNonLocals).

:- pred cases_find_subgoal_nonlocals(list(case)::in,
    set_of_progvar::in, set_of_progvar::out) is det.

cases_find_subgoal_nonlocals([], !SubGoalNonLocals).
cases_find_subgoal_nonlocals([Case | Cases], !SubGoalNonLocals) :-
    Case = case(_, _, SubGoal),
    NonLocals = goal_get_nonlocals(SubGoal),
    set_of_var.union(NonLocals, !SubGoalNonLocals),
    cases_find_subgoal_nonlocals(Cases, !SubGoalNonLocals).

%---------------------------------------------------------------------------%
%
% Code for if-then-else.
%

:- pred ml_gen_ite(code_model::in, hlds_goal::in, hlds_goal::in, hlds_goal::in,
    prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_ite(CodeModel, Cond, Then, Else, Context,
        LocalVarDefns, FuncDefns, Stmts, !Info) :-
    Cond = hlds_goal(_, CondGoalInfo),
    CondCodeModel = goal_info_get_code_model(CondGoalInfo),
    ml_gen_info_get_packed_word_map(!.Info, EntryPackedWordMap),
    (
        %   model_det Cond:
        %       <(if Cond then Then else Else)>
        %   ===>
        %       <Cond>
        %       <Then>

        CondCodeModel = model_det,
        ml_gen_goal_as_block(model_det, Cond, CondStmt, !Info),
        ml_gen_goal_as_block(CodeModel, Then, ThenStmt, !Info),
        LocalVarDefns = [],
        FuncDefns = [],
        Stmts = [CondStmt, ThenStmt]
    ;
        %   model_semi cond:
        %       <(if Cond then Then else Else)>
        %   ===>
        %       MR_bool succeeded;
        %
        %       <succeeded = Cond>
        %       if (succeeded) {
        %           <Then>
        %       } else {
        %           <Else>
        %       }

        CondCodeModel = model_semi,
        ml_gen_info_get_const_var_map(!.Info, InitConstVarMap),
        ml_gen_goal(model_semi, Cond,
            CondLocalVarDefns, CondFuncDefns, CondStmts, !Info),
        ml_gen_test_success(Succeeded, !Info),
        ml_gen_goal_as_block(CodeModel, Then, ThenStmt, !Info),
        ml_gen_info_set_const_var_map(InitConstVarMap, !Info),
        % Start the else branch with EntryPackedWordMap to prevent it
        % from trying to use map entries added by the then branch.
        ml_gen_info_set_packed_word_map(EntryPackedWordMap, !Info),
        ml_gen_goal_as_block(CodeModel, Else, ElseStmt, !Info),
        % Start the code after the if-then-else with EntryPackedWordMap
        % to prevent it from trying to use map entries added by a branch
        % that was not taken.
        ml_gen_info_set_packed_word_map(EntryPackedWordMap, !Info),
        ml_gen_info_set_const_var_map(InitConstVarMap, !Info),
        IfStmt = ml_stmt_if_then_else(Succeeded, ThenStmt, yes(ElseStmt),
            Context),
        LocalVarDefns = CondLocalVarDefns,
        FuncDefns = CondFuncDefns,
        Stmts = CondStmts ++ [IfStmt]
    ;
        % XXX The following transformation does not do as good a job of GC
        % as it could. Ideally we ought to ensure that stuff used only
        % in the `Else' part will be reclaimed if a GC occurs during the `Then'
        % part. But that is a bit tricky to achieve.
        %
        %   model_non cond:
        %       <(if Cond then Then else Else)>
        %   ===>
        %       MR_bool cond_<N>;
        %
        %       void then_func() {
        %           cond_<N> = MR_TRUE;
        %           <Then>
        %       }
        %
        %       cond_<N> = MR_FALSE;
        %       <Cond && then_func()>
        %       if (!cond_<N>) {
        %           <Else>
        %       }
        %
        % except that we hoist any declarations generated for <Cond> to the top
        % of the scope, so that they are in scope for the <Then> goal (this
        % is needed for declarations of static consts).

        CondCodeModel = model_non,
        ml_gen_info_get_const_var_map(!.Info, EntryConstVarMap),

        % Generate the `cond_<N>' var and the code to initialize it to false.
        ml_gen_info_new_cond_var(CondVar, !Info),
        CondVarDecl = ml_gen_cond_var_decl(CondVar, Context),
        ml_gen_set_cond_var(CondVar, ml_const(mlconst_false), Context,
            SetCondFalse),

        % Allocate a name for the `then_func'.
        ml_gen_new_func_label(no, ThenFuncLabel, ThenFuncLabelRval, !Info),

        % Generate <Cond && then_func()>.
        ml_get_env_ptr(EnvPtrRval),
        SuccessCont = success_cont(ThenFuncLabelRval, EnvPtrRval, []),
        ml_gen_info_push_success_cont(SuccessCont, !Info),
        ml_gen_goal(model_non, Cond, CondLocalVarDefns, CondFuncDefns,
            CondStmts, !Info),
        ml_gen_info_pop_success_cont(!Info),

        % Generate the `then_func'.
        % push nesting level
        Then = hlds_goal(_, ThenGoalInfo),
        ThenContext = goal_info_get_context(ThenGoalInfo),
        ml_gen_set_cond_var(CondVar, ml_const(mlconst_true),
            ThenContext, SetCondTrue),
        % Do not take any information about packed args into the new function
        % depth, since that may cause dangling cross-function references
        % when the new function depth is flattened out.
        ml_gen_info_set_packed_word_map(map.init, !Info),
        ml_gen_info_increment_func_nest_depth(!Info),
        ml_gen_goal_as_block(CodeModel, Then, ThenStmt, !Info),
        ml_gen_info_decrement_func_nest_depth(!Info),
        % Do not take any information about packed args out of the new function
        % depth, for the same reason.
        ml_gen_info_set_packed_word_map(map.init, !Info),
        ThenFuncBody =
            ml_stmt_block([], [], [SetCondTrue, ThenStmt], ThenContext),
        % pop nesting level
        ml_gen_nondet_label_func(!.Info, ThenFuncLabel, ThenContext,
            ThenFuncBody, ThenFuncDefn),

        % Generate `if (!cond_<N>) { <Else> }'.
        ml_gen_test_cond_var(CondVar, CondSucceeded),
        ml_gen_info_set_const_var_map(EntryConstVarMap, !Info),
        % Start the else branch with EntryPackedWordMap to prevent it
        % from trying to use map entries added by the then branch.
        ml_gen_info_set_packed_word_map(EntryPackedWordMap, !Info),
        ml_gen_goal_as_block(CodeModel, Else, ElseStmt, !Info),
        % Start the code after the if-then-else with EntryPackedWordMap
        % to prevent it from trying to use map entries added by a branch
        % that was not taken.
        ml_gen_info_set_packed_word_map(EntryPackedWordMap, !Info),
        ml_gen_info_set_const_var_map(EntryConstVarMap, !Info),
        IfStmt = ml_stmt_if_then_else(ml_unop(logical_not, CondSucceeded),
            ElseStmt, no, Context),

        % Package it all up in the right order.
        LocalVarDefns = [CondVarDecl | CondLocalVarDefns],
        FuncDefns = CondFuncDefns ++ [ThenFuncDefn],
        Stmts = [SetCondFalse | CondStmts] ++ [IfStmt]
    ).

%---------------------------------------------------------------------------%
%
% Code for negation.
%

:- pred ml_gen_negation(hlds_goal::in, code_model::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_negation(Cond, CodeModel, Context, LocalVarDefns, FuncDefns, Stmts,
        !Info) :-
    Cond = hlds_goal(_, CondGoalInfo),
    CondCodeModel = goal_info_get_code_model(CondGoalInfo),
    ml_gen_info_get_packed_word_map(!.Info, EntryPackedWordMap),
    (
        % model_det negation:
        %       <not(Goal)>
        %   ===>
        %   {
        %       MR_bool succeeded;
        %       <succeeded = Goal>
        %       /* now ignore the value of succeeded,
        %        which we know will be MR_FALSE */
        %   }

        CodeModel = model_det,
        ml_gen_goal_as_branch(model_semi, Cond,
            LocalVarDefns, FuncDefns, Stmts, !Info)
    ;
        % model_semi negation, model_det goal:
        %       <succeeded = not(Goal)>
        %   ===>
        %       <do Goal>
        %       succeeded = MR_FALSE;

        CodeModel = model_semi, CondCodeModel = model_det,
        ml_gen_goal_as_branch(model_det, Cond,
            CondLocalVarDefns, CondFuncDefns, CondStmts, !Info),
        % Start the code after the negation with EntryPackedWordMap
        % to prevent it from trying to use map entries added by a branch
        % that was not taken.
        ml_gen_info_set_packed_word_map(EntryPackedWordMap, !Info),
        ml_gen_set_success(ml_const(mlconst_false), Context, SetSuccessFalse,
            !Info),
        LocalVarDefns = CondLocalVarDefns,
        FuncDefns = CondFuncDefns,
        Stmts = CondStmts ++ [SetSuccessFalse]
    ;
        % model_semi negation, model_semi goal:
        %       <succeeded = not(Goal)>
        %   ===>
        %       <succeeded = Goal>
        %       succeeded = !succeeded;

        CodeModel = model_semi, CondCodeModel = model_semi,
        ml_gen_goal_as_branch(model_semi, Cond,
            CondLocalVarDefns, CondFuncDefns, CondStmts, !Info),
        % Start the code after the negation with EntryPackedWordMap
        % to prevent it from trying to use map entries added by a branch
        % that was not taken.
        ml_gen_info_set_packed_word_map(EntryPackedWordMap, !Info),
        ml_gen_test_success(Succeeded, !Info),
        ml_gen_set_success(ml_unop(logical_not, Succeeded),
            Context, InvertSuccess, !Info),
        LocalVarDefns = CondLocalVarDefns,
        FuncDefns = CondFuncDefns,
        Stmts = CondStmts ++ [InvertSuccess]
    ;
        CodeModel = model_semi, CondCodeModel = model_non,
        unexpected($module, $pred, "nondet cond")
    ;
        CodeModel = model_non,
        unexpected($module, $pred, "nondet negation")
    ).

%---------------------------------------------------------------------------%
%
% Code for conjunctions.
%

:- pred ml_gen_conj(hlds_goals::in, code_model::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_conj(Conjuncts, CodeModel, Context, LocalVarDefns, FuncDefns, Stmts,
        !Info) :-
    (
        Conjuncts = [],
        ml_gen_success(CodeModel, Context, Stmts, !Info),
        LocalVarDefns = [],
        FuncDefns = []
    ;
        Conjuncts = [SingleGoal],
        ml_gen_goal(CodeModel, SingleGoal, LocalVarDefns, FuncDefns, Stmts,
            !Info)
    ;
        Conjuncts = [First | Rest],
        Rest = [_ | _],
        First = hlds_goal(_, FirstGoalInfo),
        FirstDeterminism = goal_info_get_determinism(FirstGoalInfo),
        ( if determinism_components(FirstDeterminism, _, at_most_zero) then
            % the `Rest' code is unreachable
            ml_gen_goal(CodeModel, First, LocalVarDefns, FuncDefns, Stmts,
                !Info)
        else
            determinism_to_code_model(FirstDeterminism, FirstCodeModel),
            DoGenFirst = ml_gen_goal(FirstCodeModel, First),
            DoGenRest = ml_gen_conj(Rest, CodeModel, Context),
            ml_combine_conj(FirstCodeModel, Context, DoGenFirst, DoGenRest,
                LocalVarDefns, FuncDefns, Stmts, !Info)
        )
    ).

%---------------------------------------------------------------------------%

ml_gen_maybe_convert_goal_code_model(OuterCodeModel, InnerCodeModel, Context,
        !Stmts, !Info) :-
    (
        % If the inner and outer code models are equal, we don't need to do
        % anything.
        (
            OuterCodeModel = model_det,
            InnerCodeModel = model_det
        ;
            OuterCodeModel = model_semi,
            InnerCodeModel = model_semi
        ;
            OuterCodeModel = model_non,
            InnerCodeModel = model_non
        )
    ;
        % If the inner code model is less precise than the outer code model,
        % then that is either a determinism error, or a situation in which
        % simplify.m is supposed to wrap the goal inside a `some' to indicate
        % that a commit is needed.
        (
            OuterCodeModel = model_det,
            InnerCodeModel = model_semi,
            unexpected($module, $pred, "semi in det")
        ;
            OuterCodeModel = model_det,
            InnerCodeModel = model_non,
            unexpected($module, $pred, "nondet in det")
        ;
            OuterCodeModel = model_semi,
            InnerCodeModel = model_non,
            unexpected($module, $pred, "nondet in semi")
        )
    ;
        % If the inner code model is more precise than the outer code model,
        % then we need to append some statements to convert the calling
        % convention for the inner code model to that of the outer code model.
        (
            OuterCodeModel = model_semi,
            InnerCodeModel = model_det,

            % det goal in semidet context:
            %   <succeeded = Goal>
            % ===>
            %   <do Goal>
            %   succeeded = MR_TRUE

            ml_gen_set_success(ml_const(mlconst_true), Context, SetSuccessTrue,
                !Info),
            !:Stmts = !.Stmts ++ [SetSuccessTrue]
        ;
            OuterCodeModel = model_non,
            InnerCodeModel = model_det,

            % det goal in nondet context:
            %   <Goal && SUCCEED()>
            % ===>
            %   <do Goal>
            %   SUCCEED()

            ml_gen_call_current_success_cont(Context, CallCont, !Info),
            !:Stmts = !.Stmts ++ [CallCont]
        ;
            OuterCodeModel = model_non,
            InnerCodeModel = model_semi,

            % semi goal in nondet context:
            %   <Goal && SUCCEED()>
            % ===>
            %   MR_bool succeeded;
            %
            %   <succeeded = Goal>
            %   if (succeeded) SUCCEED()

            ml_gen_test_success(Succeeded, !Info),
            ml_gen_call_current_success_cont(Context, CallCont, !Info),
            IfStmt = ml_stmt_if_then_else(Succeeded, CallCont, no, Context),
            !:Stmts = !.Stmts ++ [IfStmt]
        )
    ).

%---------------------------------------------------------------------------%

ml_gen_local_var_decls(_VarSet, _VarTypes, _Context, [], [], !Info).
ml_gen_local_var_decls(VarSet, VarTypes, Context, [Var | Vars], Defns,
        !Info) :-
    lookup_var_type(VarTypes, Var, Type),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    IsDummy = is_type_a_dummy(ModuleInfo, Type),
    (
        IsDummy = is_dummy_type,
        % No declaration needed for this variable.
        ml_gen_local_var_decls(VarSet, VarTypes, Context, Vars, Defns, !Info)
    ;
        IsDummy = is_not_dummy_type,
        VarName = ml_gen_local_var_name(VarSet, Var),
        ml_gen_local_var_decl(VarName, Type, Context, Defn, !Info),
        ml_gen_local_var_decls(VarSet, VarTypes, Context, Vars, Defns0, !Info),
        Defns = [Defn | Defns0]
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_code_gen.
%---------------------------------------------------------------------------%
