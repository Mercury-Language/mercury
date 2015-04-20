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

    % Handle simplifications of plain calls.
    %
:- pred simplify_goal_plain_call(
    hlds_goal_expr::in(goal_expr_plain_call), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

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
:- import_module libs.int_emu.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.
:- import_module transform_hlds.const_prop.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module varset.

%----------------------------------------------------------------------------%

simplify_goal_plain_call(GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
        NestedContext, InstMap0, Common0, Common, !Info) :-
    GoalExpr0 = plain_call(PredId, ProcId, Args, IsBuiltin, _, _),
    simplify_info_get_module_info(!.Info, ModuleInfo),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),

    maybe_generate_warning_for_call_to_obsolete_predicate(PredId, PredInfo,
        GoalInfo0, !Info),
    maybe_generate_warning_for_infinite_loop_call(PredId, ProcId,
        Args, IsBuiltin, PredInfo, ProcInfo, GoalInfo0, NestedContext,
        Common0, !Info),

    % Try to evaluate the call at compile-time.
    ModuleSymName = pred_info_module(PredInfo),
    module_info_get_globals(ModuleInfo, Globals),
    ( is_std_lib_module_name(ModuleSymName, ModuleName) ->
        % For calls to library predicates, we can do three things to improve
        % them.
        %
        % 1. For some predicates, we can evaluate calls to them
        %    at compile time.
        %
        %    This yields the best code: for each output variable,
        %    an assignment from a constant.
        %
        % 2. For second and later occurrences of the same call,
        %    we can replace the call with code that assigns the values
        %    of the output variables in the first call to the corresponding
        %    output variables in the later calls.
        %
        %    This yields the next best code: for each output variable,
        %    an assignment from a variable bound earlier, which may or may not
        %    need to be saved on the stack across calls.
        %
        % 3. Improve a library call by replacing it with less expensive code.
        %
        %    This yields the smallest code improvement, since the code
        %    it generates will typically still do a call.

        PredName = pred_info_name(PredInfo),
        proc_id_to_int(ProcId, ModeNum),
        simplify_info_get_var_types(!.Info, VarTypes),
        (
            % Step 1.
            simplify_do_const_prop(!.Info),
            const_prop.evaluate_call(Globals, VarTypes, InstMap0,
                ModuleName, PredName, ModeNum, Args, 
                EvaluatedGoalExpr, GoalInfo0, EvaluatedGoalInfo)
        ->
            GoalExpr = EvaluatedGoalExpr,
            GoalInfo = EvaluatedGoalInfo,
            Common = Common0,
            simplify_info_set_should_requantify(!Info)
        ;
            % Step 2.
            simplify_look_for_duplicate_call(PredId, ProcId, Args, GoalExpr0,
                GoalInfo0, MaybeAssignsGoalExpr, Common0, Common, !Info),
            (
                MaybeAssignsGoalExpr = yes(GoalExpr),
                GoalInfo = GoalInfo0
                % simplify_look_for_duplicate_call (or rather its
                % subconstractors in common.m) will set the requantify flag 
                % if needed.
            ;
                MaybeAssignsGoalExpr = no,
                (
                    % Step 3.
                    simplify_improve_library_call(InstMap0,
                        ModuleName, PredName, ModeNum, Args, ImprovedGoalExpr,
                        GoalInfo0, ImprovedGoalInfo, InstMap0, !Info)
                ->
                    % simplify_improve_library_call will have set
                    % the requantify flag.
                    GoalExpr = ImprovedGoalExpr,
                    GoalInfo = ImprovedGoalInfo
                ;
                    GoalExpr = GoalExpr0,
                    GoalInfo = GoalInfo0
                )
            )
        )
    ;
        % For calls to non-library predicates, steps 1 and 3 above
        % don't apply, so we can do only step 2.
        simplify_look_for_duplicate_call(PredId, ProcId, Args, GoalExpr0,
            GoalInfo0, MaybeAssignsGoalExpr, Common0, Common, !Info),
        (
            MaybeAssignsGoalExpr = yes(GoalExpr),
            GoalInfo = GoalInfo0
            % simplify_look_for_duplicate_call (or rather its
            % subconstractors in common.m) will set the requantify flag 
            % if needed.
        ;
            MaybeAssignsGoalExpr = no,
            GoalExpr = GoalExpr0,
            GoalInfo = GoalInfo0
        )
    ).

simplify_goal_generic_call(GoalExpr0, GoalExpr, GoalInfo, GoalInfo,
        _NestedContext0, _InstMap0, Common0, Common, !Info) :-
    % XXX We should do duplicate call elimination for class method calls
    % as well as higher order calls. However, that would require teaching
    % common.m about calls that aren't completely identified by the values
    % of the input arguments.
    GoalExpr0 = generic_call(GenericCall, Args, Modes, _, Det),
    (
        GenericCall = higher_order(Closure, Purity, _, _),
        ( simplify_do_warn_or_opt_duplicate_calls(!.Info, OptDuplicateCalls) ->
            common_optimise_higher_order_call(Closure, Args, Modes, Det,
                Purity, GoalInfo, GoalExpr0, MaybeAssignsGoalExpr,
                Common0, Common, !Info),
            (
                MaybeAssignsGoalExpr = yes(AssignsGoalExpr),
                OptDuplicateCalls = yes
            ->
                GoalExpr = AssignsGoalExpr
            ;
                GoalExpr = GoalExpr0
            )
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

simplify_goal_foreign_proc(GoalExpr0, GoalExpr, !GoalInfo,
        _NestedContext0, InstMap0, Common0, Common, !Info) :-
    GoalExpr0 = call_foreign_proc(Attributes, PredId, ProcId,
        Args0, ExtraArgs0, MaybeTraceRuntimeCond, Impl),
    % XXX The logic of this predicate should be based on
    % simplify_goal_plain_call, and prefer the same transformations
    % as simplify_goal_plain_call, to the maximum extent possible.
    (
        % XXX Why do we insist on const_prop here? What
        % simplify_improve_library_call does is NOT constant propagation.
        simplify_do_const_prop(!.Info),
        simplify_info_get_module_info(!.Info, ModuleInfo),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        ModuleSymName = pred_info_module(PredInfo),
        is_std_lib_module_name(ModuleSymName, ModuleName),
        ExtraArgs0 = [],
        PredName = pred_info_name(PredInfo),
        proc_id_to_int(ProcId, ModeNum),
        ArgVars = list.map(foreign_arg_var, Args0),
        simplify_improve_library_call(InstMap0, ModuleName, PredName,
            ModeNum, ArgVars, ImprovedGoalExpr, !GoalInfo, InstMap0, !Info)
    ->
        GoalExpr = ImprovedGoalExpr,
        Common = Common0
        % simplify_improve_library_call will have set
        % the requantify flag.
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
            simplify_do_warn_or_opt_duplicate_calls(!.Info, OptDuplicateCalls),
            ExtraArgs = []
        ->
            ArgVars = list.map(foreign_arg_var, Args),
            Purity = goal_info_get_purity(!.GoalInfo),
            common_optimise_call(PredId, ProcId, ArgVars, Purity, !.GoalInfo,
                GoalExpr1, MaybeAssignsGoalExpr, Common0, Common, !Info),
            (
                MaybeAssignsGoalExpr = yes(AssignsGoalExpr),
                OptDuplicateCalls = yes
            ->
                GoalExpr = AssignsGoalExpr
            ;
                GoalExpr = GoalExpr1
            )
        ;
            GoalExpr = GoalExpr1,
            Common = Common0
        )
    ).

:- func make_arg_always_boxed(foreign_arg) = foreign_arg.

make_arg_always_boxed(Arg) = Arg ^ arg_box_policy := always_boxed.

%----------------------------------------------------------------------------%
%
% Predicates that generate warnings, if appropriate.
%

    % Generate warnings for calls to predicates that have been marked with
    % `pragma obsolete' declarations.
    %
:- pred maybe_generate_warning_for_call_to_obsolete_predicate(pred_id::in,
    pred_info::in, hlds_goal_info::in,
    simplify_info::in, simplify_info::out) is det.

maybe_generate_warning_for_call_to_obsolete_predicate(PredId, PredInfo,
        GoalInfo, !Info) :-
    simplify_info_get_module_info(!.Info, ModuleInfo),
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
        module_info_pred_info(ModuleInfo, ThisPredId, ThisPredInfo),
        pred_info_get_markers(ThisPredInfo, ThisPredMarkers),
        not check_marker(ThisPredMarkers, marker_obsolete)
    ->
        % XXX warn_obsolete isn't really a simple code warning.
        % We should add a separate warning type for this.
        GoalContext = goal_info_get_context(GoalInfo),
        PredPieces = describe_one_pred_name(ModuleInfo,
            should_module_qualify, PredId),
        Pieces = [words("Warning: call to obsolete")] ++
            PredPieces ++ [suffix("."), nl],
        Msg = simple_msg(GoalContext,
            [option_is_set(warn_simple_code, yes, [always(Pieces)])]),
        Severity = severity_conditional(warn_simple_code, yes,
            severity_warning, no),
        Spec = error_spec(Severity,
            phase_simplify(report_in_any_mode), [Msg]),
        simplify_info_add_simple_code_spec(Spec, !Info)
    ;
        true
    ).

    % Generate warnings for recursive calls that look like they represent
    % infinite loops, because every input argument in the call is either
    % the same variable as the variable in the corresponding position
    % in the head, or is an equivalent variable.
    %
:- pred maybe_generate_warning_for_infinite_loop_call(
    pred_id::in, proc_id::in, list(prog_var)::in, builtin_state::in,
    pred_info::in, proc_info::in, hlds_goal_info::in,
    simplify_nested_context::in,  common_info::in,
    simplify_info::in, simplify_info::out) is det.

maybe_generate_warning_for_infinite_loop_call(PredId, ProcId, Args, IsBuiltin,
        PredInfo, ProcInfo, GoalInfo, NestedContext, Common, !Info) :-
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
        NestedContext ^ snc_num_enclosing_lambdas = 0,

        % Are the input arguments the same (or equivalent)?
        simplify_info_get_module_info(!.Info, ModuleInfo),
        proc_info_get_headvars(ProcInfo, HeadVars),
        proc_info_get_argmodes(ProcInfo, ArgModes),
        input_args_are_equiv(Args, HeadVars, ArgModes, Common, ModuleInfo),

        % Don't warn if the input arguments' modes initial insts contain
        % `any' insts, since the arguments might have become more constrained
        % before the recursive call, in which case the recursion might
        % eventually terminate.
        %
        % XXX The following check will only warn if the inputs are all fully
        % ground; i.e. we won't warn in the case of partially instantiated
        % insts such as list_skel(free). Still, it is better to miss warnings
        % in that rare and unsupported case rather than to issue spurious
        % warnings in cases involving `any' insts. We should only warn about
        % definite nontermination here, not possible nontermination; warnings
        % about possible nontermination should only be given if the
        % termination analysis pass is enabled.
        all [ArgMode] (
            (
                list.member(ArgMode, ArgModes),
                mode_is_input(ModuleInfo, ArgMode)
            )
        =>
            mode_is_fully_input(ModuleInfo, ArgMode)
        ),

        % Don't count procs using minimal evaluation as they should always
        % terminate if they have a finite number of answers.
        \+ proc_info_get_eval_method(ProcInfo, eval_minimal(_)),

        % Don't warn about impure procedures, since (unlike pure and semipure
        % procedures) they may modify the state in ways not visible to us.
        pred_info_get_purity(PredInfo, Purity),
        \+ Purity = purity_impure
    ->
        GoalContext = goal_info_get_context(GoalInfo),

        % It would be better if we supplied more information than just
        % the line number, e.g. we should print the name of the containing
        % predicate.

        MainPieces = [words("Warning: recursive call will lead to"),
            words("infinite recursion."), nl],
        VerbosePieces =
            [words("If this recursive call is executed,"),
            words("the procedure will call itself"),
            words("with exactly the same input arguments,"),
            words("leading to infinite recursion."), nl],
        Msg = simple_msg(GoalContext,
            [option_is_set(warn_simple_code, yes,
                [always(MainPieces),
                verbose_only(verbose_once, VerbosePieces)])]),
        Severity = severity_conditional(warn_simple_code, yes,
            severity_warning, no),
        Spec = error_spec(Severity,
            phase_simplify(report_in_any_mode), [Msg]),
        simplify_info_add_simple_code_spec(Spec, !Info)
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

%----------------------------------------------------------------------------%
%
% Predicates that improve the code, if they can.
%

:- pred simplify_look_for_duplicate_call(pred_id::in, proc_id::in,
    list(prog_var)::in, hlds_goal_expr::in, hlds_goal_info::in,
    maybe(hlds_goal_expr)::out, common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

simplify_look_for_duplicate_call(PredId, ProcId, Args, GoalExpr0, GoalInfo0,
        MaybeAssignsGoalExpr, Common0, Common, !Info) :-
    ( simplify_do_warn_or_opt_duplicate_calls(!.Info, OptDuplicateCalls) ->
        Purity = goal_info_get_purity(GoalInfo0),
        % NOTE We want to call common_optimise_call outside the condition,
        % so we can return an updated Common (which will record the first,
        % NON-duplicate appearance of every call) to our caller.
        common_optimise_call(PredId, ProcId, Args, Purity, GoalInfo0,
            GoalExpr0, MaybeAssignsGoalExpr0, Common0, Common, !Info),
        (
            MaybeAssignsGoalExpr0 = yes(_AssignsGoalExpr0),
            OptDuplicateCalls = yes
        ->
            MaybeAssignsGoalExpr = MaybeAssignsGoalExpr0
        ;
            MaybeAssignsGoalExpr = no
        )
    ;
        Common = Common0,
        MaybeAssignsGoalExpr = no
    ).

    % simplify_improve_library_call(InstMap0, ModuleName, PredName,
    %   ModeNum, Args, ImprovedGoalExpr, GoalInfo0, ImprovedGoalInfo, !Info):
    %
    % This attempts to improve a call to ModuleName.PredName(Args)
    % in mode ModeNum by replacing it with less expensive code.
    %
    % The list of predicates and/or functions that this less expensive code
    % may contain calls to should be listed in simplify_may_introduce_calls,
    % to prevent dead_proc_elim from deleting them from the predicate table
    % before we get here.
    %
:- pred simplify_improve_library_call(instmap::in,
    string::in, string::in, int::in, list(prog_var)::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    instmap::in, simplify_info::in, simplify_info::out) is semidet.

simplify_improve_library_call(InstMap0, ModuleName, PredName, ModeNum, Args,
        ImprovedGoalExpr, GoalInfo0, ImprovedGoalInfo, InstMap0, !Info) :-
    (
        ModuleName = "builtin",
        (
            ( PredName = "@<",  Inequality = "<", Invert = no
            ; PredName = "@>",  Inequality = ">", Invert = no
            ; PredName = "@=<", Inequality = ">", Invert = yes
            ; PredName = "@>=", Inequality = "<", Invert = yes
            ),
            Args = [TI, X, Y],
            % The definitions of @<, @=<, @> and @>= all call builtin.compare
            % and test the result. We improve the call by inlining those
            % definitions.
            % XXX We should test to see whether the target supports
            % builtin compare operations for structured terms, and use those
            % if available.
            simplify_inline_builtin_inequality(TI, X, Y, Inequality, Invert,
                GoalInfo0, ImprovedGoalExpr, InstMap0, !Info),
            ImprovedGoalInfo = GoalInfo0
        ;
            PredName = "compare",
            % When generating code for target languages that have builtin
            % operations for comparing structured terms, we replace calls
            % to Mercury's compare with the target's builtin compare.
            simplify_improve_builtin_compare(ModeNum, Args,
                ImprovedGoalExpr, GoalInfo0, ImprovedGoalInfo, !Info)
        )
    ;
        ModuleName = "int",
        simplify_improve_int_call(InstMap0, PredName, ModeNum, Args,
            ImprovedGoalExpr, GoalInfo0, ImprovedGoalInfo, !Info)
    ),
    simplify_info_set_should_requantify(!Info).

:- pred simplify_inline_builtin_inequality(prog_var::in,
    prog_var::in, prog_var::in, string::in, bool::in, hlds_goal_info::in,
    hlds_goal_expr::out, instmap::in,
    simplify_info::in, simplify_info::out) is det.

simplify_inline_builtin_inequality(TI, X, Y, Inequality, Invert, GoalInfo,
        ImprovedGoalExpr, InstMap0, !Info) :-
    % Construct the variable to hold the comparison result.
    simplify_info_get_varset(!.Info, VarSet0),
    varset.new_var(CmpRes, VarSet0, VarSet),
    simplify_info_set_varset(VarSet, !Info),

    % We have to add the type of CmpRes to the var_types.
    simplify_info_get_var_types(!.Info, VarTypes0),
    add_var_type(CmpRes, comparison_result_type, VarTypes0, VarTypes),
    simplify_info_set_var_types(VarTypes, !Info),

    % Construct the call to compare/3.
    Context = hlds_goal.goal_info_get_context(GoalInfo),
    Args    = [TI, CmpRes, X, Y],

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
    ArgInsts = [CmpRes - Unique],
    BuiltinModule = mercury_public_builtin_module,
    goal_util.generate_simple_call(BuiltinModule, "compare", pf_predicate,
        mode_no(ModeNo), detism_det, purity_pure, Args, [],
        instmap_delta_from_assoc_list(ArgInsts), ModuleInfo, Context,
        CmpGoal0),
    CmpGoal0 = hlds_goal(CmpExpr, CmpInfo0),
    CmpNonLocals0 = goal_info_get_nonlocals(CmpInfo0),
    set_of_var.insert(CmpRes, CmpNonLocals0, CmpNonLocals),
    goal_info_set_nonlocals(CmpNonLocals, CmpInfo0, CmpInfo),
    CmpGoal  = hlds_goal(CmpExpr, CmpInfo),

    % Construct the unification CmpRes = Inequality.
    TypeCtor = type_ctor(
        qualified(mercury_public_builtin_module, "comparison_result"), 0),
    ConsId   = cons(qualified(BuiltinModule, Inequality), 0, TypeCtor),
    Bound    = bound(shared, inst_test_results_fgtc,
                    [bound_functor(ConsId, [])]),
    UMode    = ((Unique -> Bound) - (Bound -> Bound)),
    RHS      = rhs_functor(ConsId, is_not_exist_constr, []),
    UKind    = deconstruct(CmpRes, ConsId, [], [], can_fail, cannot_cgc),
    UContext = unify_context(umc_implicit(
        "simplify_inline_builtin_inequality"), []),
    UnifyExpr  = unify(CmpRes, RHS, UMode, UKind, UContext),
    UnifyNonLocals0 = goal_info_get_nonlocals(GoalInfo),
    set_of_var.insert(CmpRes, UnifyNonLocals0, UnifyNonLocals),
    goal_info_set_nonlocals(UnifyNonLocals, GoalInfo, UnifyInfo),
    UnifyGoal  = hlds_goal(UnifyExpr, UnifyInfo),

    (
        Invert = no,
        ImprovedGoalExpr = conj(plain_conj, [CmpGoal, UnifyGoal])
    ;
        Invert = yes,
        ImprovedGoalExpr = conj(plain_conj,
            [CmpGoal, hlds_goal(negation(UnifyGoal), UnifyInfo)])
    ).

:- pred simplify_improve_builtin_compare(int::in, list(prog_var)::in,
    hlds_goal_expr::out, hlds_goal_info::in, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is semidet.

simplify_improve_builtin_compare(_ModeNum, Args, ImprovedGoalExpr, !GoalInfo,
        !Info) :-
    % On the Erlang backend, it is faster for us to use builtin comparison
    % operators on high level data structures than to deconstruct the data
    % structure and compare the atomic constituents. We can only do this
    % on values of types which we know not to have user-defined equality
    % predicates.

    simplify_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, can_compare_compound_values, yes),
    list.reverse(Args, [Y, X, Res | _]),
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
        ImprovedGoalExpr = if_then_else([], CondEq, ReturnEq, Rest)
    ).

:- pred simplify_improve_int_call(instmap::in, string::in, int::in,
    list(prog_var)::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is semidet.

simplify_improve_int_call(InstMap0, PredName, _ModeNum, Args, ImprovedGoalExpr,
        !GoalInfo, !Info) :-
    simplify_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    target_bits_per_int(Globals, bits_per_int(TargetBitsPerInt)),
    (
        PredName = "quot_bits_per_int",
        Args = [X, Y],
        % There is no point in checking whether bits_per_int is 0;
        % it isn't.
        Op = "unchecked_quotient",
        simplify_make_int_ico_op(Op, X, TargetBitsPerInt, Y, ImprovedGoalExpr,
            !.GoalInfo, !Info)
    ;
        PredName = "times_bits_per_int",
        Args = [X, Y],
        Op = "*",
        simplify_make_int_ico_op(Op, X, TargetBitsPerInt, Y, ImprovedGoalExpr,
            !.GoalInfo, !Info)
    ;
        PredName = "rem_bits_per_int",
        Args = [X, Y],
        % There is no point in checking whether bits_per_int is 0;
        % it isn't.
        Op = "unchecked_rem",
        simplify_make_int_ico_op(Op, X, TargetBitsPerInt, Y, ImprovedGoalExpr,
            !.GoalInfo, !Info)
    ;
        ( PredName = "/"
        ; PredName = "//"
        ),
        Args = [X, Y, Z],
        instmap_lookup_var(InstMap0, Y, InstY),
        InstY = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        Op = "unchecked_quotient",
        simplify_make_int_binary_op_goal_expr(!.Info, Op, inline_builtin,
            X, Y, Z, ImprovedGoalExpr)
    ;
        PredName = "rem",
        Args = [X, Y, Z],
        instmap_lookup_var(InstMap0, Y, InstY),
        InstY = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        Op = "unchecked_rem",
        simplify_make_int_binary_op_goal_expr(!.Info, Op, inline_builtin,
            X, Y, Z, ImprovedGoalExpr)
    ;
        PredName = "<<",
        Args = [X, Y, Z],
        instmap_lookup_var(InstMap0, Y, InstY),
        InstY = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal >= 0,
        YVal < TargetBitsPerInt,
        Op = "unchecked_left_shift",
        simplify_make_int_binary_op_goal_expr(!.Info, Op, inline_builtin,
            X, Y, Z, ImprovedGoalExpr)
    ;
        PredName = ">>",
        Args = [X, Y, Z],
        instmap_lookup_var(InstMap0, Y, InstY),
        InstY = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal >= 0,
        YVal < TargetBitsPerInt,
        Op = "unchecked_right_shift",
        simplify_make_int_binary_op_goal_expr(!.Info, Op, inline_builtin,
            X, Y, Z, ImprovedGoalExpr)
    ).

    % simplify_make_int_ico_op(Op, X, IntConst, Y, GoalExpr, OrigGoalInfo,
    %   !Info):
    %
    % Return a GoalExpr that computes Y := X Op IntConst.
    % (The ico stands for the three arguments being Input, Constant input,
    % and Output.)
    %
:- pred simplify_make_int_ico_op(string::in,
    prog_var::in, int::in, prog_var::in, hlds_goal_expr::out,
    hlds_goal_info::in,
    simplify_info::in, simplify_info::out) is det.

simplify_make_int_ico_op(Op, X, IntConst, Y, GoalExpr, OrigGoalInfo, !Info) :-
    simplify_make_int_const(IntConst, ConstVar, ConstGoal, !Info),
    simplify_make_int_binary_op_goal_expr(!.Info, Op, inline_builtin,
        X, ConstVar, Y, OpGoalExpr),
    % set_of_var.list_to_set([X, Y, ConstVar], NonLocals),
    % goal_info_set_nonlocals(NonLocals, OrigGoalInfo, OpGoalInfo),
    OpGoal = hlds_goal(OpGoalExpr, OrigGoalInfo),
    GoalExpr = conj(plain_conj, [ConstGoal, OpGoal]).

:- pred simplify_make_int_const(int::in, prog_var::out, hlds_goal::out,
    simplify_info::in, simplify_info::out) is det.

simplify_make_int_const(IntConst, ConstVar, Goal, !Info) :-
    simplify_info_get_varset(!.Info, VarSet0),
    simplify_info_get_var_types(!.Info, VarTypes0),
    varset.new_var(ConstVar, VarSet0, VarSet),
    add_var_type(ConstVar, int_type, VarTypes0, VarTypes),
    simplify_info_set_varset(VarSet, !Info),
    simplify_info_set_var_types(VarTypes, !Info),

    ConstConsId = int_const(IntConst),
    Unification = construct(ConstVar, ConstConsId, [], [],
        construct_dynamically, cell_is_shared, no_construct_sub_info),
    RHS = rhs_functor(ConstConsId, is_not_exist_constr, []),
    % The context shouldn't matter.
    UnifyContext = unify_context(umc_explicit, []),
    Ground = ground_inst,
    Mode = (free -> Ground) - (Ground -> Ground),
    GoalExpr = unify(ConstVar, RHS, Mode, Unification, UnifyContext),
    NonLocals = set_of_var.make_singleton(ConstVar),
    InstMapDelta = instmap_delta_bind_var(ConstVar),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred simplify_make_int_binary_op_goal_expr(simplify_info::in,
    string::in, builtin_state::in,
    prog_var::in, prog_var::in, prog_var::in, hlds_goal_expr::out) is det.

simplify_make_int_binary_op_goal_expr(Info, Op, IsBuiltin, X, Y, Z,
        GoalExpr) :-
    IntModuleSymName = mercury_std_lib_module_name(unqualified("int")),
    OpSymName = qualified(IntModuleSymName, Op),
    simplify_info_get_module_info(Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, PredTable),
    predicate_table_lookup_func_sym_arity(PredTable, is_fully_qualified,
        OpSymName, 2, OpPredIds),
    ( OpPredIds = [OpPredIdPrime] ->
        OpPredId = OpPredIdPrime
    ;
        unexpected($module, $pred, "cannot find " ++ Op)
    ),
    OpProcIdInt = 0,
    proc_id_to_int(OpProcId, OpProcIdInt),
    OpArgs = [X, Y, Z],
    MaybeUnifyContext = no,
    GoalExpr = plain_call(OpPredId, OpProcId, OpArgs, IsBuiltin,
        MaybeUnifyContext, OpSymName).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_goal_call.
%---------------------------------------------------------------------------%
