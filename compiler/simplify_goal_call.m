%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: simplify_goal_call.m.
%
% This module handles simplification of plain calls, generic calls and
% calls to foreign code.
%
%----------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_goal_call.
:- interface.

:- import_module check_hlds.simplify.common.
:- import_module check_hlds.simplify.simplify_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.

    % Handle simplifications of plain calls.
    %
:- pred simplify_goal_plain_call(
    hlds_goal_expr::in(goal_expr_plain_call), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

    % simplify_library_call(ModuleName, ProcName, ModeNum, CrossCompiling,
    %   Args, GoalExpr, !GoalInfo, !Info):
    %
    % This attempts to simplify a call to
    %   ModuleName.ProcName(ArgList)
    % whose mode is specified by ModeNum.
    %
    % The list of predicates and/or functions that we may wish to introduce
    % calls to should be listed in simplify_may_introduce_calls, to prevent
    % dead_proc_elim from deleting them from the predicate table before we
    % get here.
    %
:- pred simplify_library_call(string::in, string::in, int::in, bool::in,
    bool::in, list(prog_var)::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is semidet.

    % Handle simplifications of generic calls.
    %
:- pred simplify_goal_generic_call(
    hlds_goal_expr::in(goal_expr_generic_call), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

    % Handle simplifications of calls to foreign code.
    %
:- pred simplify_goal_foreign_proc(
    hlds_goal_expr::in(goal_expr_foreign_proc), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.
:- import_module transform_hlds.const_prop.

:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module varset.

simplify_goal_plain_call(GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
        NestedContext0, InstMap0, Common0, Common, !Info) :-
    GoalExpr0 = plain_call(PredId, ProcId, Args, IsBuiltin, _, _),
    simplify_info_get_module_info(!.Info, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = hlds_pred.pred_info_module(PredInfo),
    Name = hlds_pred.pred_info_name(PredInfo),

    % Convert calls to builtin @=<, @<, @>=, @> into the corresponding
    % calls to builtin.compare/3.

    (
        Args = [TI, X, Y],
        ModuleName = mercury_public_builtin_module,
        ( Name = "@<", Inequality = "<", Invert = no
        ; Name = "@=<", Inequality = ">", Invert = yes
        ; Name = "@>=", Inequality = "<", Invert = yes
        ; Name = "@>",  Inequality = ">", Invert = no
        )
    ->
        inequality_goal(TI, X, Y, Inequality, Invert, GoalInfo0,
            GoalExpr, GoalInfo, InstMap0, !Info),
        Common = Common0
    ;
        simplify_call_goal(PredId, ProcId, Args, IsBuiltin,
            GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
            NestedContext0, InstMap0, Common0, Common, !Info)
    ).

:- pred inequality_goal(prog_var::in, prog_var::in, prog_var::in, string::in,
    bool::in, hlds_goal_info::in, hlds_goal_expr::out, hlds_goal_info::out,
    instmap::in, simplify_info::in, simplify_info::out) is det.

inequality_goal(TI, X, Y, Inequality, Invert, GoalInfo, GoalExpr, GoalInfo,
        InstMap0, !Info) :-
    % Construct the variable to hold the comparison result.
    simplify_info_get_varset(!.Info, VarSet0),
    varset.new_var(R, VarSet0, VarSet),
    simplify_info_set_varset(VarSet, !Info),

    % We have to add the type of R to the var_types.
    simplify_info_get_var_types(!.Info, VarTypes0),
    add_var_type(R, comparison_result_type, VarTypes0, VarTypes),
    simplify_info_set_var_types(VarTypes, !Info),

    % Construct the call to compare/3.
    Context = hlds_goal.goal_info_get_context(GoalInfo),
    Args    = [TI, R, X, Y],

    instmap_lookup_var(InstMap0, X, XInst),
    instmap_lookup_var(InstMap0, Y, YInst),
    simplify_info_get_module_info(!.Info, ModuleInfo),
    ModeNo =
        ( if inst_is_unique(ModuleInfo, XInst) then
            ( if inst_is_unique(ModuleInfo, YInst) then 1 else 2 )
        else
            ( if inst_is_unique(ModuleInfo, YInst) then 3 else 0 )
        ),

    Unique   = ground(unique, none),
    ArgInsts = [R - Unique],
    BuiltinModule = mercury_public_builtin_module,
    goal_util.generate_simple_call(BuiltinModule, "compare", pf_predicate,
        mode_no(ModeNo), detism_det, purity_pure, Args, [],
        instmap_delta_from_assoc_list(ArgInsts), ModuleInfo, Context,
        CmpGoal0),
    CmpGoal0 = hlds_goal(CmpExpr, CmpInfo0),
    CmpNonLocals0 = goal_info_get_nonlocals(CmpInfo0),
    set_of_var.insert(R, CmpNonLocals0, CmpNonLocals),
    goal_info_set_nonlocals(CmpNonLocals, CmpInfo0, CmpInfo),
    CmpGoal  = hlds_goal(CmpExpr, CmpInfo),

    % Construct the unification R = Inequality.
    TypeCtor = type_ctor(
        qualified(mercury_public_builtin_module, "comparison_result"), 0),
    ConsId   = cons(qualified(BuiltinModule, Inequality), 0, TypeCtor),
    Bound    = bound(shared, inst_test_results_fgtc,
                    [bound_functor(ConsId, [])]),
    UMode    = ((Unique -> Bound) - (Bound -> Bound)),
    RHS      = rhs_functor(ConsId, is_not_exist_constr, []),
    UKind    = deconstruct(R, ConsId, [], [], can_fail, cannot_cgc),
    UContext = unify_context(umc_implicit(
        "replacement of inequality with call to compare/3"), []),
    UfyExpr  = unify(R, RHS, UMode, UKind, UContext),
    UfyNonLocals0 = goal_info_get_nonlocals(GoalInfo),
    set_of_var.insert(R, UfyNonLocals0, UfyNonLocals),
    goal_info_set_nonlocals(UfyNonLocals, GoalInfo, UfyInfo),
    UfyGoal  = hlds_goal(UfyExpr, UfyInfo),

    (
        Invert   = no,
        GoalExpr = conj(plain_conj, [CmpGoal, UfyGoal])
    ;
        Invert   = yes,
        GoalExpr = conj(plain_conj,
            [CmpGoal, hlds_goal(negation(UfyGoal), UfyInfo)])
    ).

:- pred simplify_call_goal(pred_id::in, proc_id::in, list(prog_var)::in,
    builtin_state::in, hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

simplify_call_goal(PredId, ProcId, Args, IsBuiltin, !GoalExpr, !GoalInfo,
        NestedContext0, InstMap0, Common0, Common, !Info) :-
    simplify_info_get_module_info(!.Info, ModuleInfo0),
    module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
        PredInfo, ProcInfo),
    GoalContext = goal_info_get_context(!.GoalInfo),
    % Check for calls to predicates with `pragma obsolete' declarations.
    (
        simplify_do_warn_obsolete(!.Info),
        pred_info_get_markers(PredInfo, Markers),
        check_marker(Markers, marker_obsolete),

        % Don't warn about directly recursive calls to obsolete predicates.
        % That would cause spurious warnings, particularly with builtin
        % predicates, or preds defined using foreign_procs.
        simplify_info_get_pred_proc_id(!.Info, ThisPredId, _),
        PredId \= ThisPredId,

        % Don't warn about calls to obsolete predicates from other predicates
        % that also have a `pragma obsolete' declaration. Doing so
        % would also just result in spurious warnings.
        module_info_pred_info(ModuleInfo0, ThisPredId, ThisPredInfo),
        pred_info_get_markers(ThisPredInfo, ThisPredMarkers),
        not check_marker(ThisPredMarkers, marker_obsolete)
    ->
        % XXX warn_obsolete isn't really a simple code warning.
        % We should add a separate warning type for this.
        ObsoletePredPieces = describe_one_pred_name(ModuleInfo0,
            should_module_qualify, PredId),
        ObsoletePieces = [words("Warning: call to obsolete")] ++
            ObsoletePredPieces ++ [suffix("."), nl],
        ObsoleteMsg = simple_msg(GoalContext,
            [option_is_set(warn_simple_code, yes, [always(ObsoletePieces)])]),
        ObsoleteSeverity = severity_conditional(warn_simple_code, yes,
            severity_warning, no),
        ObsoleteSpec = error_spec(ObsoleteSeverity,
            phase_simplify(report_in_any_mode), [ObsoleteMsg]),
        simplify_info_add_simple_code_spec(ObsoleteSpec, !Info)
    ;
        true
    ),

    % Check for recursive calls with the same input arguments,
    % and warn about them (since they will lead to infinite loops).
    (
        simplify_do_warn_simple_code(!.Info),

        % Is this a (directly) recursive call, i.e. is the procedure being
        % called the same as the procedure we're analyzing?
        simplify_info_get_pred_proc_id(!.Info, PredId, ProcId),

        % Don't count inline builtins. (The compiler generates code for
        % builtins that looks recursive, so that you can take their address,
        % but since the recursive call actually expands into inline code,
        % it is not infinite recursion.)
        IsBuiltin \= inline_builtin,

        % Don't warn if we're inside a lambda goal, because the recursive call
        % may not be executed.
        NestedContext0 ^ snc_num_enclosing_lambdas = 0,

        % Are the input arguments the same (or equivalent)?
        simplify_info_get_module_info(!.Info, ModuleInfo1),
        module_info_pred_proc_info(ModuleInfo1, PredId, ProcId,
            PredInfo1, ProcInfo1),
        proc_info_get_headvars(ProcInfo1, HeadVars),
        proc_info_get_argmodes(ProcInfo1, ArgModes),
        input_args_are_equiv(Args, HeadVars, ArgModes, Common0, ModuleInfo1),

        % Don't warn if the input arguments' modes initial insts contain
        % `any' insts, since the arguments might have become more constrained
        % before the recursive call, in which case the recursion might
        % eventually terminate.
        %
        % XXX The following check will only warn if the inputs are all fully
        % ground; i.e. we won't warn in the case of partially instantiated
        % insts such as list_skel(free). Still, it is better to miss warnings
        % in that rare and unsupported case rather than to issue spurious
        % warnings in cases involving `any' insts.  We should only warn about
        % definite nontermination here, not possible nontermination; warnings
        % about possible nontermination should only be given if the
        % termination analysis pass is enabled.
        all [ArgMode] (
            (
                list.member(ArgMode, ArgModes),
                mode_is_input(ModuleInfo1, ArgMode)
            )
        =>
            mode_is_fully_input(ModuleInfo1, ArgMode)
        ),

        % Don't count procs using minimal evaluation as they should always
        % terminate if they have a finite number of answers.
        \+ proc_info_get_eval_method(ProcInfo, eval_minimal(_)),

        % Don't warn about impure procedures, since they may modify the state
        % in ways not visible to us (unlike pure and semipure procedures).
        pred_info_get_purity(PredInfo1, Purity),
        \+ Purity = purity_impure
    ->
        % It would be better if we supplied more information than just
        % the line number, e.g. we should print the name of the containing
        % predicate.

        InfiniteRecMainPieces = [words("Warning: recursive call will lead to"),
            words("infinite recursion.")],
        InfiniteRecVerbosePieces =
            [words("If this recursive call is executed,"),
            words("the procedure will call itself"),
            words("with exactly the same input arguments,"),
            words("leading to infinite recursion.")],
        InfiniteRecMsg = simple_msg(GoalContext,
            [option_is_set(warn_simple_code, yes,
                [always(InfiniteRecMainPieces),
                verbose_only(InfiniteRecVerbosePieces)])]),
        InfiniteRecSeverity = severity_conditional(warn_simple_code, yes,
            severity_warning, no),
        InfiniteRecSpec = error_spec(InfiniteRecSeverity,
            phase_simplify(report_in_any_mode), [InfiniteRecMsg]),
        simplify_info_add_simple_code_spec(InfiniteRecSpec, !Info)
    ;
        true
    ),

    % Check for duplicate calls to the same procedure.
    (
        simplify_do_opt_duplicate_calls(!.Info),
        goal_info_get_purity(!.GoalInfo) = purity_pure
    ->
        common_optimise_call(PredId, ProcId, Args, !.GoalInfo, !GoalExpr,
            Common0, Common, !Info)
    ;
        simplify_do_warn_duplicate_calls(!.Info),
        goal_info_get_purity(!.GoalInfo) = purity_pure
    ->
        % We need to do the pass, for the warnings, but we ignore
        % the optimized goal and instead use the original one.
        common_optimise_call(PredId, ProcId, Args, !.GoalInfo,
            !.GoalExpr, _NewGoalExpr, Common0, Common, !Info)
    ;
        Common = Common0
    ),

    % Try to evaluate the call at compile-time.
    (
        simplify_info_get_module_info(!.Info, ModuleInfo2),
        !.GoalExpr = plain_call(CallPredId, CallProcId, CallArgs, _, _, _),
        module_info_pred_info(ModuleInfo2, CallPredId, CallPredInfo),
        CallModuleSymName = pred_info_module(CallPredInfo),
        is_std_lib_module_name(CallModuleSymName, CallModuleName)
    ->
        CallPredName = pred_info_name(CallPredInfo),
        proc_id_to_int(CallProcId, CallModeNum),
        simplify_info_get_var_types(!.Info, VarTypes),
        (
            simplify_do_const_prop(!.Info),
            const_prop.evaluate_call(CallModuleName, CallPredName, CallModeNum,
                CallArgs, VarTypes, InstMap0, ModuleInfo2, GoalExprPrime,
                !GoalInfo)
        ->
            !:GoalExpr = GoalExprPrime,
            simplify_info_set_should_requantify(!Info)
        ;
            module_info_get_globals(ModuleInfo2, Globals),
            globals.lookup_bool_option(Globals, cross_compiling,
                CrossCompiling),
            globals.lookup_bool_option(Globals, can_compare_compound_values,
                CanCompareCompoundValues),
            simplify_library_call(CallModuleName, CallPredName, CallModeNum,
                CrossCompiling, CanCompareCompoundValues, CallArgs,
                GoalExprPrime, !GoalInfo, !Info)
        ->
            !:GoalExpr = GoalExprPrime,
            simplify_info_set_should_requantify(!Info)
        ;
            true
        )
    ;
        true
    ).

    % input_args_are_equiv(Args, HeadVars, Modes, CommonInfo, ModuleInfo):
    %
    % Succeeds if all the input arguments (determined by looking at `Modes')
    % in `Args' are equivalent (according to the equivalence class specified
    % by `CommonInfo') to the corresponding variables in HeadVars.
    % HeadVars, Modes, and Args should all be lists of the same length.
    %
:- pred input_args_are_equiv(list(prog_var)::in, list(prog_var)::in,
    list(mer_mode)::in, common_info::in, module_info::in) is semidet.

input_args_are_equiv([], [], _, _, _).
input_args_are_equiv([Arg | Args], [HeadVar | HeadVars], [Mode | Modes],
        CommonInfo, ModuleInfo) :-
    ( mode_is_input(ModuleInfo, Mode) ->
        common_vars_are_equivalent(Arg, HeadVar, CommonInfo)
    ;
        true
    ),
    input_args_are_equiv(Args, HeadVars, Modes, CommonInfo, ModuleInfo).

%---------------------------------------------------------------------------%

simplify_library_call(ModuleName, PredName, _ModeNum, CrossCompiling,
        CanCompareCompoundValues, Args, GoalExpr, !GoalInfo, !Info) :-
    (
        ModuleName = "builtin",
        PredName = "compare",

        % On the Erlang backend, it is faster for us to use builtin comparison
        % operators on high level data structures than to deconstruct the data
        % structure and compare the atomic constituents.  We can only do this
        % on values of types which we know not to have user-defined equality
        % predicates.

        CanCompareCompoundValues = yes,
        list.reverse(Args, [Y, X, Res | _]),
        simplify_info_get_module_info(!.Info, ModuleInfo),
        simplify_info_get_var_types(!.Info, VarTypes),
        lookup_var_type(VarTypes, Y, Type),
        type_definitely_has_no_user_defined_equality_pred(ModuleInfo, Type),

        require_det (
            Context = goal_info_get_context(!.GoalInfo),
            goal_util.generate_simple_call(mercury_private_builtin_module,
                "builtin_compound_eq", pf_predicate, only_mode, detism_semi,
                purity_pure, [X, Y], [], instmap_delta_bind_no_var, ModuleInfo,
                Context, CondEq),
            goal_util.generate_simple_call(mercury_private_builtin_module,
                "builtin_compound_lt", pf_predicate, only_mode, detism_semi,
                purity_pure, [X, Y], [], instmap_delta_bind_no_var, ModuleInfo,
                Context, CondLt),

            Builtin = mercury_public_builtin_module,
            TypeCtor = type_ctor(
                qualified(mercury_public_builtin_module, "comparison_result"),
                0),
            make_const_construction(Res,
                cons(qualified(Builtin, "="), 0, TypeCtor), ReturnEq),
            make_const_construction(Res,
                cons(qualified(Builtin, "<"), 0, TypeCtor), ReturnLt),
            make_const_construction(Res,
                cons(qualified(Builtin, ">"), 0, TypeCtor), ReturnGt),

            NonLocals = set_of_var.list_to_set([Res, X, Y]),
            goal_info_set_nonlocals(NonLocals, !GoalInfo),

            RestExpr = if_then_else([], CondLt, ReturnLt, ReturnGt),
            Rest = hlds_goal(RestExpr, !.GoalInfo),
            GoalExpr = if_then_else([], CondEq, ReturnEq, Rest)
        )
    ;
        ModuleName = "int",
        simplify_do_const_prop(!.Info),
        CrossCompiling = no,
        (
            PredName = "quot_bits_per_int",
            Args = [X, Y],
            % There is no point in checking whether bits_per_int is 0;
            % it isn't.
            Op = "unchecked_quotient",
            simplify_library_call_int_arity2(Op, X, Y, GoalExpr,
                !GoalInfo, !Info)
        ;
            PredName = "times_bits_per_int",
            Args = [X, Y],
            Op = "*",
            simplify_library_call_int_arity2(Op, X, Y, GoalExpr,
                !GoalInfo, !Info)
        ;
            PredName = "rem_bits_per_int",
            Args = [X, Y],
            % There is no point in checking whether bits_per_int is 0;
            % it isn't.
            Op = "unchecked_rem",
            simplify_library_call_int_arity2(Op, X, Y, GoalExpr,
                !GoalInfo, !Info)
        ;
            PredName = "bits_per_int",
            Args = [X],
            require_det (
                ConstConsId = int_const(int.bits_per_int),
                RHS = rhs_functor(ConstConsId, is_not_exist_constr, []),
                ModeOfX = out_mode,
                ModeOfConstConsId = in_mode,
                UnifyMode = ModeOfX - ModeOfConstConsId,
                How = construct_dynamically,
                IsUnique = cell_is_shared,
                Sub = no_construct_sub_info,
                Unification = construct(X, ConstConsId, [], [], How,
                    IsUnique, Sub),
                UnifyMainContext = umc_implicit("simplify_library_call"),
                UnifyContext = unify_context(UnifyMainContext, []),
                GoalExpr = unify(X, RHS, UnifyMode, Unification, UnifyContext)
            )
        )
    ).

:- pred simplify_library_call_int_arity2(string::in,
    prog_var::in, prog_var::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is semidet.

simplify_library_call_int_arity2(Op, X, Y, GoalExpr, !GoalInfo, !Info) :-
    simplify_info_get_varset(!.Info, VarSet0),
    simplify_info_get_var_types(!.Info, VarTypes0),
    varset.new_var(ConstVar, VarSet0, VarSet),
    add_var_type(ConstVar, int_type, VarTypes0, VarTypes),
    simplify_info_set_varset(VarSet, !Info),
    simplify_info_set_var_types(VarTypes, !Info),

    ConstConsId = int_const(int.bits_per_int),
    ConstUnification = construct(ConstVar, ConstConsId, [], [],
        construct_dynamically, cell_is_shared, no_construct_sub_info),
    ConstRHS = rhs_functor(ConstConsId, is_not_exist_constr, []),
    % The context shouldn't matter.
    ConstUnifyContext = unify_context(umc_explicit, []),
    Ground = ground_inst,
    ConstMode = (free -> Ground) - (Ground -> Ground),
    ConstGoalExpr = unify(ConstVar, ConstRHS, ConstMode, ConstUnification,
        ConstUnifyContext),
    ConstNonLocals = set_of_var.make_singleton(ConstVar),
    InstMapDelta = instmap_delta_bind_var(ConstVar),
    goal_info_init(ConstNonLocals, InstMapDelta,
        detism_det, purity_pure, ConstGoalInfo),
    ConstGoal = hlds_goal(ConstGoalExpr, ConstGoalInfo),

    IntModuleSymName = mercury_std_lib_module_name(unqualified("int")),
    OpSymName = qualified(IntModuleSymName, Op),
    simplify_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, PredTable),
    predicate_table_lookup_func_sym_arity(PredTable, is_fully_qualified,
        OpSymName, 2, OpPredIds),
    OpPredIds = [OpPredId],
    OpProcIdInt = 0,
    proc_id_to_int(OpProcId, OpProcIdInt),
    OpArgs = [X, ConstVar, Y],
    MaybeUnifyContext = no,
    IsBuiltin = inline_builtin,
    OpGoalExpr = plain_call(OpPredId, OpProcId, OpArgs, IsBuiltin,
        MaybeUnifyContext, OpSymName),

    OpGoalInfo0 = !.GoalInfo,
    OpNonLocals0 = goal_info_get_nonlocals(OpGoalInfo0),
    set_of_var.insert(ConstVar, OpNonLocals0, OpNonLocals),
    goal_info_set_nonlocals(OpNonLocals, OpGoalInfo0, OpGoalInfo),
    OpGoal = hlds_goal(OpGoalExpr, OpGoalInfo),

    GoalExpr = conj(plain_conj, [ConstGoal, OpGoal]).

%---------------------------------------------------------------------------%

simplify_goal_generic_call(GoalExpr0, GoalExpr, GoalInfo, GoalInfo,
        _NestedContext0, _InstMap0, Common0, Common, !Info) :-
    GoalExpr0 = generic_call(GenericCall, Args, Modes, _, Det),
    (
        GenericCall = higher_order(Closure, Purity, _, _),
        (
            simplify_do_opt_duplicate_calls(!.Info),
            % XXX We should do duplicate call elimination for
            % class method calls here.
            % XXX Should we handle semipure higher-order calls too?
            Purity = purity_pure
        ->
            common_optimise_higher_order_call(Closure, Args, Modes, Det,
                GoalInfo, GoalExpr0, GoalExpr, Common0, Common, !Info)
        ;
            simplify_do_warn_duplicate_calls(!.Info),
            % XXX Should we handle impure/semipure higher-order calls too?
            Purity = purity_pure
        ->
            % We need to do the pass, for the warnings, but we ignore
            % the optimized goal and instead use the original one.
            common_optimise_higher_order_call(Closure, Args, Modes, Det,
                GoalInfo, GoalExpr0, _GoalExpr1, Common0, Common, !Info),
            GoalExpr = GoalExpr0
        ;
            GoalExpr = GoalExpr0,
            Common = Common0
        )
    ;
        GenericCall = event_call(_),
        simplify_info_set_has_user_event(has_user_event, !Info),
        GoalExpr = GoalExpr0,
        Common = Common0
    ;
        ( GenericCall = class_method(_, _, _, _)
        ; GenericCall = cast(_)
        ),
        GoalExpr = GoalExpr0,
        Common = Common0
    ).

%---------------------------------------------------------------------------%

simplify_goal_foreign_proc(GoalExpr0, GoalExpr, !GoalInfo,
        _NestedContext0, _InstMap0, Common0, Common, !Info) :-
    GoalExpr0 = call_foreign_proc(Attributes, PredId, ProcId,
        Args0, ExtraArgs0, MaybeTraceRuntimeCond, Impl),
    (
        simplify_do_const_prop(!.Info),
        simplify_info_get_module_info(!.Info, ModuleInfo),
        module_info_pred_info(ModuleInfo, PredId, CallPredInfo),
        CallModuleSymName = pred_info_module(CallPredInfo),
        is_std_lib_module_name(CallModuleSymName, CallModuleName),
        ExtraArgs0 = [],

        CallPredName = pred_info_name(CallPredInfo),
        proc_id_to_int(ProcId, CallModeNum),
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, cross_compiling, CrossCompiling),
        globals.lookup_bool_option(Globals, can_compare_compound_values,
            CanCompareCompoundValues),
        ArgVars = list.map(foreign_arg_var, Args0),
        simplify_library_call(CallModuleName, CallPredName, CallModeNum,
            CrossCompiling, CanCompareCompoundValues, ArgVars, GoalExprPrime,
            !GoalInfo, !Info)
    ->
        GoalExpr = GoalExprPrime,
        Common = Common0,
        simplify_info_set_should_requantify(!Info)
    ;
        BoxPolicy = get_box_policy(Attributes),
        (
            BoxPolicy = native_if_possible,
            Args = Args0,
            ExtraArgs = ExtraArgs0,
            GoalExpr1 = GoalExpr0
        ;
            BoxPolicy = always_boxed,
            Args = list.map(make_arg_always_boxed, Args0),
            ExtraArgs = list.map(make_arg_always_boxed, ExtraArgs0),
            GoalExpr1 = call_foreign_proc(Attributes, PredId, ProcId,
                Args, ExtraArgs, MaybeTraceRuntimeCond, Impl)
        ),
        (
            simplify_do_opt_duplicate_calls(!.Info),
            goal_info_get_purity(!.GoalInfo) = purity_pure,
            ExtraArgs = []
        ->
            ArgVars = list.map(foreign_arg_var, Args),
            common_optimise_call(PredId, ProcId, ArgVars, !.GoalInfo,
                GoalExpr1, GoalExpr, Common0, Common, !Info)
        ;
            GoalExpr = GoalExpr1,
            Common = Common0
        )
    ).

:- func make_arg_always_boxed(foreign_arg) = foreign_arg.

make_arg_always_boxed(Arg) = Arg ^ arg_box_policy := always_boxed.

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_goal_call.
%---------------------------------------------------------------------------%
