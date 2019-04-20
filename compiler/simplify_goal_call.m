%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: simplify_goal_call.m.
%
% This module handles simplification of plain calls, generic calls and
% calls to foreign code.
%
%---------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_goal_call.
:- interface.

:- import_module check_hlds.simplify.common.
:- import_module check_hlds.simplify.simplify_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module maybe.

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

%---------------------------------------------------------------------------%

    % Generate a warning for calls to predicates that could have explicitly
    % specified a stream, but didn't.
    %
    % This is exported for format_call.m, which checks calls to io.format
    % that this module does not see (because format_call.m transforms them
    % into something else).
    %
:- pred maybe_generate_warning_for_implicit_stream_predicate(module_info::in,
    pred_id::in, pred_info::in, hlds_goal_info::in,
    maybe(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_error_util.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.int_emu.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.
:- import_module transform_hlds.const_prop.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%

simplify_goal_plain_call(GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
        NestedContext, InstMap0, Common0, Common, !Info) :-
    GoalExpr0 = plain_call(PredId, ProcId, Args, IsBuiltin, _, _),
    simplify_info_get_module_info(!.Info, ModuleInfo),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),

    ( if simplify_do_warn_implicit_stream_calls(!.Info) then
        maybe_generate_warning_for_implicit_stream_predicate(ModuleInfo,
            PredId, PredInfo, GoalInfo0, MaybeImplicitStreamSpec),
        (
            MaybeImplicitStreamSpec = no
        ;
            MaybeImplicitStreamSpec = yes(ImplicitStreamSpec),
            simplify_info_add_message(ImplicitStreamSpec, !Info)
        )
    else
        true
    ),
    maybe_generate_warning_for_call_to_obsolete_predicate(PredId, PredInfo,
        GoalInfo0, !Info),
    maybe_generate_warning_for_infinite_loop_call(PredId, ProcId,
        Args, IsBuiltin, PredInfo, ProcInfo, GoalInfo0, NestedContext,
        Common0, !Info),
    maybe_generate_warning_for_useless_comparison(PredInfo,
        InstMap0, Args, GoalInfo0, !Info),

    % Try to evaluate the call at compile-time.
    ModuleSymName = pred_info_module(PredInfo),
    module_info_get_globals(ModuleInfo, Globals),
    ( if is_std_lib_module_name(ModuleSymName, ModuleName) then
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
        ( if
            % Step 1.
            simplify_do_const_prop(!.Info),
            const_prop.evaluate_call(Globals, VarTypes, InstMap0,
                ModuleName, PredName, ModeNum, Args,
                EvaluatedGoalExpr, GoalInfo0, EvaluatedGoalInfo)
        then
            GoalExpr = EvaluatedGoalExpr,
            GoalInfo = EvaluatedGoalInfo,
            Common = Common0,
            simplify_info_set_should_requantify(!Info)
        else
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
                ( if
                    % Step 3.
                    simplify_improve_library_call(InstMap0,
                        ModuleName, PredName, ModeNum, Args, ImprovedGoalExpr,
                        GoalInfo0, ImprovedGoalInfo, !Info)
                then
                    % simplify_improve_library_call will have set
                    % the requantify flag.
                    GoalExpr = ImprovedGoalExpr,
                    GoalInfo = ImprovedGoalInfo
                else
                    GoalExpr = GoalExpr0,
                    GoalInfo = GoalInfo0
                )
            )
        )
    else
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
        ( if
            simplify_do_warn_or_opt_duplicate_calls(!.Info, OptDuplicateCalls)
        then
            common_optimise_higher_order_call(Closure, Args, Modes, Det,
                Purity, GoalInfo, GoalExpr0, MaybeAssignsGoalExpr,
                Common0, Common, !Info),
            ( if
                MaybeAssignsGoalExpr = yes(AssignsGoalExpr),
                OptDuplicateCalls = yes
            then
                GoalExpr = AssignsGoalExpr
            else
                GoalExpr = GoalExpr0
            )
        else
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
    ( if
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
            ModeNum, ArgVars, ImprovedGoalExpr, !GoalInfo, !Info)
    then
        GoalExpr = ImprovedGoalExpr,
        Common = Common0
        % simplify_improve_library_call will have set the requantify flag.
    else
        BoxPolicy = get_box_policy(Attributes),
        (
            BoxPolicy = bp_native_if_possible,
            Args = Args0,
            ExtraArgs = ExtraArgs0,
            GoalExpr1 = GoalExpr0
        ;
            BoxPolicy = bp_always_boxed,
            list.map(make_arg_always_boxed, Args0, Args),
            list.map(make_arg_always_boxed, ExtraArgs0, ExtraArgs),
            GoalExpr1 = call_foreign_proc(Attributes, PredId, ProcId,
                Args, ExtraArgs, MaybeTraceRuntimeCond, Impl)
        ),
        ( if
            simplify_do_warn_or_opt_duplicate_calls(!.Info, OptDuplicateCalls),
            ExtraArgs = []
        then
            ArgVars = list.map(foreign_arg_var, Args),
            Purity = goal_info_get_purity(!.GoalInfo),
            common_optimise_call(PredId, ProcId, ArgVars, Purity, !.GoalInfo,
                GoalExpr1, MaybeAssignsGoalExpr, Common0, Common, !Info),
            ( if
                MaybeAssignsGoalExpr = yes(AssignsGoalExpr),
                OptDuplicateCalls = yes
            then
                GoalExpr = AssignsGoalExpr
            else
                GoalExpr = GoalExpr1
            )
        else
            GoalExpr = GoalExpr1,
            Common = Common0
        )
    ).

:- pred make_arg_always_boxed(foreign_arg::in, foreign_arg::out) is det.

make_arg_always_boxed(!Arg) :-
    !Arg ^ arg_box_policy := bp_always_boxed.

%---------------------------------------------------------------------------%
%
% Predicates that generate warnings, if appropriate.
%

maybe_generate_warning_for_implicit_stream_predicate(ModuleInfo,
        PredId, PredInfo, GoalInfo, MaybeSpec) :-
    pred_info_get_module_name(PredInfo, ModuleName),
    pred_info_get_name(PredInfo, PredName),
    pred_info_is_pred_or_func(PredInfo) = PredOrFunc,
    ( if
        goal_info_has_feature(GoalInfo, feature_do_not_warn_implicit_stream)
    then
        MaybeSpec = no
    else if
        % We want to warn about calls to predicates, ...
        PredOrFunc = pf_predicate,

        % ... which do I/O and thus have two I/O state arguments, ...
        pred_info_get_arg_types(PredInfo, ArgTypes),
        IOStateTypeSymName = qualified(mercury_io_module, "state"),
        IOStateType = defined_type(IOStateTypeSymName, [], kind_star),
        list.filter(unify(IOStateType), ArgTypes, IOStateArgTypes),
        IOStateArgTypes = [_, _],

        % ... where the callee predicate has a twin that has one extra argument
        % that is a stream, and that extra argument is after any typeinfo
        % and/or typeclass_info args added by polymorphism, but before any
        % of the user-visible arguments.

        pred_info_get_orig_arity(PredInfo, OrigArity),
        list.length(ArgTypes, FullArity),
        NumExtraArgs = FullArity - OrigArity,
        list.det_split_list(NumExtraArgs, ArgTypes,
            ExtraArgTypes, UserArgTypes),

        module_info_get_predicate_table(ModuleInfo, PredTable),
        PredSymName = qualified(ModuleName, PredName),
        predicate_table_lookup_pf_sym_arity(PredTable, is_fully_qualified,
            PredOrFunc, PredSymName, OrigArity + 1, PredIds),
        list.filter(
            one_extra_stream_arg(ModuleInfo, NumExtraArgs,
                ExtraArgTypes, UserArgTypes),
            PredIds, OneExtraStreamArgPredIds),
        OneExtraStreamArgPredIds = [_ | _]
    then
        GoalContext = goal_info_get_context(GoalInfo),
        PredPieces = describe_one_pred_name(ModuleInfo,
            should_module_qualify, PredId),
        Pieces = [words("The call to")] ++ PredPieces ++
            [words("could have an additional argument"),
            words("explicitly specifying a stream."), nl],
        Msg = simple_msg(GoalContext,
            [option_is_set(warn_implicit_stream_calls, yes,
                [always(Pieces)])]),
        Severity = severity_conditional(warn_implicit_stream_calls, yes,
            severity_informational, no),
        Spec = error_spec(Severity,
            phase_simplify(report_in_any_mode), [Msg]),
        MaybeSpec = yes(Spec)
    else if
        % We want to warn about calls to predicates that update
        % the current input or output stream.
        ModuleName = mercury_io_module,
        PredOrFunc = pf_predicate,
        (
            ( PredName = "see"
            ; PredName = "seen"
            ; PredName = "set_input_stream"
            ),
            Dir = "input"
        ;
            ( PredName = "tell"
            ; PredName = "told"
            ; PredName = "set_output_stream"
            ),
            Dir = "output"
        )
    then
        GoalContext = goal_info_get_context(GoalInfo),
        PredPieces = describe_one_pred_name(ModuleInfo,
            should_module_qualify, PredId),
        Pieces = [words("The call to")] ++ PredPieces ++
            [words("could be made redundant by explicitly passing"),
            words("the"), words(Dir), words("stream it specifies"),
            words("to later I/O operations."), nl],
        Msg = simple_msg(GoalContext,
            [option_is_set(warn_implicit_stream_calls, yes,
                [always(Pieces)])]),
        Severity = severity_conditional(warn_implicit_stream_calls, yes,
            severity_informational, no),
        Spec = error_spec(Severity,
            phase_simplify(report_in_any_mode), [Msg]),
        MaybeSpec = yes(Spec)
    else
        MaybeSpec = no
    ).

:- pred one_extra_stream_arg(module_info::in, int::in,
    list(mer_type)::in, list(mer_type)::in, pred_id::in) is semidet.

one_extra_stream_arg(ModuleInfo, NumExtraArgs,
        BaseExtraArgTypes, BaseUserArgTypes, PredId) :-
    % We are testing whether the relationship between the argument list
    % of the base predicate (BaseArgTypes) and the argument list of this
    % predicate (ArgTypes) looks like this:
    %
    % BaseArgTypes = BaseExtraArgTypes ++ BaseUserArgTypes
    % ArgTypes = BaseExtraArgTypes ++ [StreamArgType] ++ BaseUserArgTypes
    %
    % where NumExtraArgs is the length of BaseExtraArgTypes.

    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    list.split_list(NumExtraArgs, ArgTypes, ExtraArgTypes, UserArgTypes),

    ExtraArgTypes = BaseExtraArgTypes,
    UserArgTypes = [HeadUserArgType | TailUserArgTypes],
    TailUserArgTypes = BaseUserArgTypes,

    % Is the added argument a stream type? (We could list all the stream
    % types defined in library/io.m here, but it is probably more robust
    % to assume that all types in io.m whose names end in "stream" qualify.
    % We don't think we will ever give any non-stream type such a name.)
    HeadUserArgType = defined_type(HeadUserArgTypeSymName, [], kind_star),
    HeadUserArgTypeSymName = qualified(mercury_io_module, HeadUserArgTypeName),
    string.suffix(HeadUserArgTypeName, "stream").

%---------------------%

    % Generate warnings for calls to predicates that have been marked with
    % `pragma obsolete' declarations.
    %
:- pred maybe_generate_warning_for_call_to_obsolete_predicate(pred_id::in,
    pred_info::in, hlds_goal_info::in,
    simplify_info::in, simplify_info::out) is det.

maybe_generate_warning_for_call_to_obsolete_predicate(PredId, PredInfo,
        GoalInfo, !Info) :-
    ( if
        simplify_do_warn_obsolete(!.Info),
        pred_info_get_markers(PredInfo, Markers),
        check_marker(Markers, marker_obsolete),

        % Don't warn about directly recursive calls to obsolete predicates.
        % That would cause spurious warnings, particularly with builtin
        % predicates, or predicates defined using foreign_procs.
        simplify_info_get_pred_proc_id(!.Info, ThisPredProcId),
        ThisPredProcId = proc(ThisPredId, _ThisProcId),
        PredId \= ThisPredId,

        % Don't warn about calls to obsolete predicates from other predicates
        % that also have a `pragma obsolete' declaration. Doing so
        % would also just result in spurious warnings.
        simplify_info_get_module_info(!.Info, ModuleInfo),
        module_info_pred_info(ModuleInfo, ThisPredId, ThisPredInfo),
        pred_info_get_markers(ThisPredInfo, ThisPredMarkers),
        not check_marker(ThisPredMarkers, marker_obsolete)
    then
        GoalContext = goal_info_get_context(GoalInfo),
        PredPieces = describe_one_pred_name(ModuleInfo,
            should_module_qualify, PredId),
        Pieces = [words("Warning: call to obsolete")] ++
            PredPieces ++ [suffix("."), nl],
        Msg = simple_msg(GoalContext,
            [option_is_set(warn_obsolete, yes, [always(Pieces)])]),
        Severity = severity_conditional(warn_obsolete, yes,
            severity_warning, no),
        Spec = error_spec(Severity,
            phase_simplify(report_in_any_mode), [Msg]),
        simplify_info_add_message(Spec, !Info)
    else
        true
    ).

%---------------------%

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
    ( if
        simplify_do_warn_simple_code(!.Info),

        simplify_info_get_pred_proc_id(!.Info, CurPredProcId),
        % Is this a (directly) recursive call, i.e. is the procedure being
        % called the same as the procedure we are analyzing?
        CurPredProcId = proc(PredId, ProcId),

        % Don't count inline builtins. (The compiler generates code for
        % builtins that looks recursive, so that you can take their address,
        % but since the recursive call actually expands into inline code,
        % it is not infinite recursion.)
        IsBuiltin \= inline_builtin,

        % Don't warn if we are inside a lambda goal that was not created for
        % a try goal, because the recursive call may not be executed, and
        % even if it is, it may not be from inside the call tree of the
        % current predicate.
        NestedContext ^ snc_num_enclosing_barriers = 0,

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
        not proc_info_get_eval_method(ProcInfo, eval_minimal(_)),

        % Don't warn about impure procedures, since (unlike pure and semipure
        % procedures) they may modify the state in ways not visible to us.
        pred_info_get_purity(PredInfo, Purity),
        Purity \= purity_impure
    then
        PredNamePieces = describe_one_pred_info_name(should_not_module_qualify,
            PredInfo),
        MainPieces = [words("Warning: recursive call to") | PredNamePieces] ++
            [words("will lead to infinite recursion."), nl],
        VerbosePieces =
            [words("If this recursive call is executed,"),
            words("the procedure will call itself"),
            words("with exactly the same input arguments,"),
            words("leading to infinite recursion."), nl],
        Msg = simple_msg(goal_info_get_context(GoalInfo),
            [option_is_set(warn_simple_code, yes,
                [always(MainPieces),
                verbose_only(verbose_once, VerbosePieces)])]),
        Severity = severity_conditional(warn_simple_code, yes,
            severity_warning, no),
        Spec = error_spec(Severity, phase_simplify(report_in_any_mode), [Msg]),
        simplify_info_add_message(Spec, !Info)
    else
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
    ( if mode_is_input(ModuleInfo, Mode) then
        common_vars_are_equivalent(Arg, HeadVar, CommonInfo)
    else
        true
    ),
    input_args_are_equiv(Args, HeadVars, Modes, CommonInfo, ModuleInfo).

%---------------------%

    % Generate warnings for comparisons that are tautologies or contradictions.
    % For example:
    %
    %      X : uint32 < 0u32        cannot succeed
    %      X : uint >= 0u           always succeeds
    %
:- pred maybe_generate_warning_for_useless_comparison(pred_info::in,
    instmap::in, list(prog_var)::in, hlds_goal_info::in,
    simplify_info::in, simplify_info::out) is det.

maybe_generate_warning_for_useless_comparison(PredInfo, InstMap, Args,
        GoalInfo, !Info) :-
    pred_info_is_pred_or_func(PredInfo) = PredOrFunc,
    ModuleSymName = pred_info_module(PredInfo),
    ( if
        is_std_lib_module_name(ModuleSymName, ModuleName),
        PredOrFunc = pf_predicate,
        Args = [ArgA, ArgB],
        simplify_do_warn_simple_code(!.Info)
    then
        pred_info_get_name(PredInfo, PredName),
        instmap_lookup_var(InstMap, ArgA, InstA),
        instmap_lookup_var(InstMap, ArgB, InstB),
        ( if
            is_useless_unsigned_comparison(ModuleName, PredName, InstA,
                InstB, WarnPieces)
        then
            GoalContext = goal_info_get_context(GoalInfo),
            PredPieces = describe_one_pred_info_name(should_module_qualify,
                PredInfo),
            Pieces = [words("Warning: call to")] ++ PredPieces ++ WarnPieces,
            Msg = simple_msg(GoalContext,
                [option_is_set(warn_simple_code, yes, [always(Pieces)])]),
            Spec = error_spec(severity_warning,
                phase_simplify(report_in_any_mode), [Msg]),
            simplify_info_add_message(Spec, !Info)
        else
            true
        )
    else
        true
    ).

:- pred is_useless_unsigned_comparison(string::in, string::in, mer_inst::in,
    mer_inst::in, format_components::out) is semidet.

is_useless_unsigned_comparison(ModuleName, PredName, ArgA, ArgB, Pieces) :-
    (
        PredName = ">=",
        arg_is_unsigned_zero(ModuleName, ArgB, ZeroStr),
        Pieces = [words("cannot fail."), nl,
            words("All"), words(ModuleName), words("values are"),
            words(">="), words(ZeroStr), suffix(".")]
    ;
        PredName = "=<",
        arg_is_unsigned_zero(ModuleName, ArgA, ZeroStr),
        Pieces = [words("cannot fail."), nl,
            words(ZeroStr), words("=<"), words("all"),
            words(ModuleName), words("values.")]
    ;
        PredName = "<",
        arg_is_unsigned_zero(ModuleName, ArgB, ZeroStr),
        Pieces = [words("cannot succeed."), nl,
            words("There are no"), words(ModuleName),
            words("values <"), words(ZeroStr), suffix(".")]
    ;
        PredName = ">",
        arg_is_unsigned_zero(ModuleName, ArgA, ZeroStr),
        Pieces = [words("cannot succeed."), nl,
            words(ZeroStr), words("> no"), words(ModuleName),
            words("values.")]
    ).

:- pred arg_is_unsigned_zero(string::in, mer_inst::in, string::out) is semidet.

arg_is_unsigned_zero(ModuleName, Arg, ZeroStr) :-
    (
        ModuleName = "uint",
        Arg = bound(_, _, [bound_functor(uint_const(0u), [])]),
        ZeroStr = "0u"
    ;
        ModuleName = "uint8",
        Arg = bound(_, _, [bound_functor(uint8_const(0u8), [])]),
        ZeroStr = "0u8"
    ;
        ModuleName = "uint16",
        Arg = bound(_, _, [bound_functor(uint16_const(0u16), [])]),
        ZeroStr = "0u16"
    ;
        ModuleName = "uint32",
        Arg = bound(_, _, [bound_functor(uint32_const(0u32), [])]),
        ZeroStr = "0u32"
    ;
        ModuleName = "uint64",
        Arg = bound(_, _, [bound_functor(uint64_const(0u64), [])]),
        ZeroStr = "0u64"
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Predicates that improve the code, if they can.
%

:- pred simplify_look_for_duplicate_call(pred_id::in, proc_id::in,
    list(prog_var)::in, hlds_goal_expr::in, hlds_goal_info::in,
    maybe(hlds_goal_expr)::out, common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

simplify_look_for_duplicate_call(PredId, ProcId, Args, GoalExpr0, GoalInfo0,
        MaybeAssignsGoalExpr, Common0, Common, !Info) :-
    ( if simplify_do_warn_or_opt_duplicate_calls(!.Info, OptDupCalls) then
        Purity = goal_info_get_purity(GoalInfo0),
        % NOTE We want to call common_optimise_call outside the condition,
        % so we can return an updated Common (which will record the first,
        % NON-duplicate appearance of every call) to our caller.
        common_optimise_call(PredId, ProcId, Args, Purity, GoalInfo0,
            GoalExpr0, MaybeAssignsGoalExpr0, Common0, Common, !Info),
        ( if
            MaybeAssignsGoalExpr0 = yes(_AssignsGoalExpr0),
            OptDupCalls = yes
        then
            MaybeAssignsGoalExpr = MaybeAssignsGoalExpr0
        else
            MaybeAssignsGoalExpr = no
        )
    else
        Common = Common0,
        MaybeAssignsGoalExpr = no
    ).

%---------------------------------------------------------------------------%

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
    simplify_info::in, simplify_info::out) is semidet.

simplify_improve_library_call(InstMap0, ModuleName, PredName, ModeNum, Args,
        ImprovedGoalExpr, GoalInfo0, ImprovedGoalInfo, !Info) :-
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
            Context = goal_info_get_context(GoalInfo0),
            simplify_improve_builtin_compare(ModeNum, Args, Context,
                ImprovedGoalExpr, ImprovedGoalInfo, !Info)
        )
    ;
        ModuleName = "private_builtin",
        ( PredName = "builtin_compare_int",     TypeName = "int"
        ; PredName = "builtin_compare_int8",    TypeName = "int8"
        ; PredName = "builtin_compare_int16",   TypeName = "int16"
        ; PredName = "builtin_compare_int32",   TypeName = "int32"
        ; PredName = "builtin_compare_int64",   TypeName = "int64"
        ; PredName = "builtin_compare_uint",    TypeName = "uint"
        ; PredName = "builtin_compare_uint8",   TypeName = "uint8"
        ; PredName = "builtin_compare_uint16",  TypeName = "uint16"
        ; PredName = "builtin_compare_uint32",  TypeName = "uint32"
        ; PredName = "builtin_compare_uint64",  TypeName = "uint64"
        ),
        Args = [R, X, Y],
        ModeNum = 0,
        Context = goal_info_get_context(GoalInfo0),
        simplify_improve_builtin_compare_int_uint(!.Info, TypeName,
            R, X, Y, Context, ImprovedGoalExpr, ImprovedGoalInfo)
    ;
        ( ModuleName = "int",       IntType = int_type_int
        ; ModuleName = "int8",      IntType = int_type_int8
        ; ModuleName = "int16",     IntType = int_type_int16
        ; ModuleName = "int32",     IntType = int_type_int32
        ; ModuleName = "int64",     IntType = int_type_int64
        ; ModuleName = "uint",      IntType = int_type_uint
        ; ModuleName = "uint8",     IntType = int_type_uint8
        ; ModuleName = "uint16",    IntType = int_type_uint16
        ; ModuleName = "uint32",    IntType = int_type_uint32
        ; ModuleName = "uint64",    IntType = int_type_uint64
        ),
        simplify_info_get_module_info(!.Info, ModuleInfo),
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, pregenerated_dist, Pregen),
        (
            Pregen = yes,
            % Tautological comparisons generate warnings (which we treat
            % as errors) from some compilers, so we avoid emitting them
            % even in pregen grades.
            replace_tautological_comparisons(PredName, Args, ImprovedGoalExpr),
            ImprovedGoalInfo = GoalInfo0
        ;
            Pregen = no,
            % This also optimizes away tautological comparisons, but does
            % other optimizations as well.
            simplify_improve_arith_shift_cmp_ops(IntType, InstMap0,
                ModuleName, PredName, ModeNum, Args, ImprovedGoalExpr,
                GoalInfo0, ImprovedGoalInfo, !Info)
        )
    ),
    simplify_info_set_should_requantify(!Info).

%---------------------%

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
    Args = [TI, CmpRes, X, Y],

    instmap_lookup_var(InstMap0, X, XInst),
    instmap_lookup_var(InstMap0, Y, YInst),
    simplify_info_get_module_info(!.Info, ModuleInfo),
    ModeNo =
        ( if inst_is_unique(ModuleInfo, XInst) then
            ( if inst_is_unique(ModuleInfo, YInst) then 1 else 2 )
        else
            ( if inst_is_unique(ModuleInfo, YInst) then 3 else 0 )
        ),

    Unique = ground(unique, none_or_default_func),
    ArgInsts = [CmpRes - Unique],
    BuiltinModule = mercury_public_builtin_module,
    goal_util.generate_simple_call(ModuleInfo, BuiltinModule, "compare",
        pf_predicate, mode_no(ModeNo), detism_det, purity_pure, Args, [],
        instmap_delta_from_assoc_list(ArgInsts), Context, CmpGoal0),
    CmpGoal0 = hlds_goal(CmpExpr, CmpInfo0),
    CmpNonLocals0 = goal_info_get_nonlocals(CmpInfo0),
    set_of_var.insert(CmpRes, CmpNonLocals0, CmpNonLocals),
    goal_info_set_nonlocals(CmpNonLocals, CmpInfo0, CmpInfo),
    CmpGoal = hlds_goal(CmpExpr, CmpInfo),

    % Construct the unification CmpRes = Inequality.
    TypeCtor = type_ctor(
        qualified(mercury_public_builtin_module, "comparison_result"), 0),
    ConsId = cons(qualified(BuiltinModule, Inequality), 0, TypeCtor),
    Bound = bound(shared, inst_test_results_fgtc, [bound_functor(ConsId, [])]),
    UnifyMode = unify_modes_lhs_rhs(
        from_to_insts(Unique, Bound),
        from_to_insts(Bound, Bound)),
    RHS = rhs_functor(ConsId, is_not_exist_constr, []),
    UKind = deconstruct(CmpRes, ConsId, [], [], can_fail, cannot_cgc),
    UContext = unify_context(umc_implicit(
        "simplify_inline_builtin_inequality"), []),
    UnifyExpr = unify(CmpRes, RHS, UnifyMode, UKind, UContext),
    UnifyNonLocals0 = goal_info_get_nonlocals(GoalInfo),
    set_of_var.insert(CmpRes, UnifyNonLocals0, UnifyNonLocals),
    goal_info_set_nonlocals(UnifyNonLocals, GoalInfo, UnifyInfo),
    UnifyGoal = hlds_goal(UnifyExpr, UnifyInfo),

    (
        Invert = no,
        ImprovedGoalExpr = conj(plain_conj, [CmpGoal, UnifyGoal])
    ;
        Invert = yes,
        ImprovedGoalExpr = conj(plain_conj,
            [CmpGoal, hlds_goal(negation(UnifyGoal), UnifyInfo)])
    ).

%---------------------%

:- pred simplify_improve_builtin_compare(int::in, list(prog_var)::in,
    prog_context::in, hlds_goal_expr::out, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is semidet.

simplify_improve_builtin_compare(_ModeNum, Args, Context,
        GoalExpr, GoalInfo, !Info) :-
    % On the Erlang backend, it is faster for us to use builtin comparison
    % operators on high level data structures than to deconstruct the data
    % structure and compare the atomic constituents. We can only do this
    % on values of types which we know not to have user-defined equality
    % predicates.

    simplify_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, can_compare_compound_values, yes),
    list.reverse(Args, [Y, X, R | _]),
    simplify_info_get_var_types(!.Info, VarTypes),
    lookup_var_type(VarTypes, Y, Type),
    type_definitely_has_no_user_defined_equality_pred(ModuleInfo, Type),

    require_det (
        % We cannot use simplify_build_compare_ite because there is
        % no builtin_compound_gt predicate (yet).
        % Using simplify_build_compare_ite would yield faster code,
        % because the code we generate here start with an equality test,
        % which is 99+% likely to fail. Starting with a less than or
        % greater than test would be better. Since such a test can be expected
        % to determine the final outcome in almost 50% of cases, it would
        % let us avoid the cost of the second test *much* more frequently.

        goal_util.generate_simple_call(ModuleInfo,
            mercury_private_builtin_module, "builtin_compound_eq",
            pf_predicate, only_mode, detism_semi, purity_pure, [X, Y], [],
            instmap_delta_bind_no_var, Context, CmpEqGoal),
        goal_util.generate_simple_call(ModuleInfo,
            mercury_private_builtin_module, "builtin_compound_lt",
            pf_predicate, only_mode, detism_semi, purity_pure, [X, Y], [],
            instmap_delta_bind_no_var, Context, CmpLtGoal),

        Builtin = mercury_public_builtin_module,
        CmpRes = qualified(mercury_public_builtin_module, "comparison_result"),
        CmpResTypeCtor = type_ctor(CmpRes, 0),
        FunctorResultLt = cons(qualified(Builtin, "<"), 0, CmpResTypeCtor),
        FunctorResultEq = cons(qualified(Builtin, "="), 0, CmpResTypeCtor),
        FunctorResultGt = cons(qualified(Builtin, ">"), 0, CmpResTypeCtor),
        make_const_construction(Context, R, FunctorResultLt, ReturnLtGoal),
        make_const_construction(Context, R, FunctorResultEq, ReturnEqGoal),
        make_const_construction(Context, R, FunctorResultGt, ReturnGtGoal),

        NonLocals = set_of_var.list_to_set([R, X, Y]),
        goal_info_init(NonLocals, instmap_delta_bind_var(R), detism_det,
            purity_pure, Context, GoalInfo),

        ReturnLtGtGoalExpr = if_then_else([], CmpLtGoal,
            ReturnLtGoal, ReturnGtGoal),
        ReturnLtGtGoal = hlds_goal(ReturnLtGtGoalExpr, GoalInfo),
        GoalExpr = if_then_else([], CmpEqGoal, ReturnEqGoal, ReturnLtGtGoal)
    ).

%---------------------%

:- pred simplify_improve_builtin_compare_int_uint(simplify_info::in,
    string::in, prog_var::in, prog_var::in, prog_var::in,
    prog_context::in, hlds_goal_expr::out, hlds_goal_info::out) is det.

simplify_improve_builtin_compare_int_uint(Info, TypeName,
        R, X, Y, Context, GoalExpr, GoalInfo) :-
    % Replace a call to builtin_compare_<inttype>(R, X, Y) with its body,
    % effectively inlining it, and thus avoiding the cost of a cross-module
    % call.
    ModuleSymName = mercury_private_builtin_module,
    PredNameLt = "builtin_" ++ TypeName ++ "_lt",
    PredNameGt = "builtin_" ++ TypeName ++ "_gt",
    simplify_make_cmp_goal_expr(Info, ModuleSymName, PredNameLt,
        inline_builtin, X, Y, Context, CmpLtGoal),
    simplify_make_cmp_goal_expr(Info, ModuleSymName, PredNameGt,
        inline_builtin, X, Y, Context, CmpGtGoal),
    simplify_build_compare_ite(CmpLtGoal, CmpGtGoal, R, X, Y, Context,
        GoalExpr, GoalInfo).

:- pred simplify_build_compare_ite(hlds_goal::in, hlds_goal::in,
    prog_var::in, prog_var::in, prog_var::in, prog_context::in,
    hlds_goal_expr::out, hlds_goal_info::out) is det.

simplify_build_compare_ite(CmpLtGoal, CmpGtGoal, R, X, Y, Context,
        GoalExpr, GoalInfo) :-
    Builtin = mercury_public_builtin_module,
    CmpRes = qualified(mercury_public_builtin_module, "comparison_result"),
    CmpResTypeCtor = type_ctor(CmpRes, 0),
    FunctorResultLt = cons(qualified(Builtin, "<"), 0, CmpResTypeCtor),
    FunctorResultEq = cons(qualified(Builtin, "="), 0, CmpResTypeCtor),
    FunctorResultGt = cons(qualified(Builtin, ">"), 0, CmpResTypeCtor),
    make_const_construction(Context, R, FunctorResultLt, ReturnLtGoal),
    make_const_construction(Context, R, FunctorResultEq, ReturnEqGoal),
    make_const_construction(Context, R, FunctorResultGt, ReturnGtGoal),

    ( if X = Y then
        ReturnEqGoal = hlds_goal(GoalExpr, GoalInfo)
    else
        % This assumes that CmpLtGoal and CmpGtGoal take only X and Y
        % as inputs. This assumption will be *wrong* if the shared type
        % of X and Y is polymorphic, because in that case, the typeinfos
        % describing the actual types bound to the type variables in the
        % polymorphic type will *also* be nonlocals.
        %
        % If we ever want to use this predicate in such cases, we will
        % have to get our caller to pass us the extra nonlocals.
        NonLocals = set_of_var.list_to_set([R, X, Y]),
        goal_info_init(NonLocals, instmap_delta_bind_var(R), detism_det,
            purity_pure, Context, GoalInfo),

        ReturnGtEqGoalExpr =
            if_then_else([], CmpGtGoal, ReturnGtGoal, ReturnEqGoal),
        ReturnGtEqGoal = hlds_goal(ReturnGtEqGoalExpr, GoalInfo),
        GoalExpr =
            if_then_else([], CmpLtGoal, ReturnLtGoal, ReturnGtEqGoal)
    ).

%---------------------%

    % simplify_make_int_ico_op(ModuleName, Op, X, IntConst, Y, GoalExpr,
    %   OrigGoalInfo, !Info):
    %
    % Return a GoalExpr that computes Y := X Op IntConst.
    % (The ico stands for the three arguments being Input, Constant input,
    % and Output.)
    %
:- pred simplify_make_int_ico_op(string::in, string::in,
    prog_var::in, int::in, prog_var::in, hlds_goal_expr::out,
    hlds_goal_info::in,
    simplify_info::in, simplify_info::out) is det.

simplify_make_int_ico_op(ModuleName, Op, X, IntConst, Y, GoalExpr,
        OrigGoalInfo, !Info) :-
    simplify_make_int_const(IntConst, ConstVar, ConstGoal, !Info),
    simplify_make_binary_op_goal_expr(!.Info, ModuleName, Op, inline_builtin,
        X, ConstVar, Y, OpGoalExpr),
    % set_of_var.list_to_set([X, Y, ConstVar], NonLocals),
    % goal_info_set_nonlocals(NonLocals, OrigGoalInfo, OpGoalInfo),
    OpGoal = hlds_goal(OpGoalExpr, OrigGoalInfo),
    GoalExpr = conj(plain_conj, [ConstGoal, OpGoal]).

:- pred simplify_make_binary_op_goal_expr(simplify_info::in,
    string::in, string::in, builtin_state::in,
    prog_var::in, prog_var::in, prog_var::in, hlds_goal_expr::out) is det.

simplify_make_binary_op_goal_expr(Info, ModuleName, Op, IsBuiltin, X, Y, Z,
        GoalExpr) :-
    ModuleSymName = mercury_std_lib_module_name(unqualified(ModuleName)),
    OpSymName = qualified(ModuleSymName, Op),
    simplify_info_get_module_info(Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, PredTable),
    predicate_table_lookup_func_sym_arity(PredTable, is_fully_qualified,
        OpSymName, 2, OpPredIds),
    ( if OpPredIds = [OpPredIdPrime] then
        OpPredId = OpPredIdPrime
    else
        unexpected($pred, "cannot find " ++ Op)
    ),
    OpProcIdInt = 0,
    proc_id_to_int(OpProcId, OpProcIdInt),
    OpArgs = [X, Y, Z],
    MaybeUnifyContext = no,
    GoalExpr = plain_call(OpPredId, OpProcId, OpArgs, IsBuiltin,
        MaybeUnifyContext, OpSymName).

:- pred simplify_make_cmp_goal_expr(simplify_info::in,
    module_name::in, string::in, builtin_state::in,
    prog_var::in, prog_var::in, prog_context::in, hlds_goal::out) is det.

simplify_make_cmp_goal_expr(Info, ModuleSymName, Op, IsBuiltin, X, Y,
        Context, Goal) :-
    OpSymName = qualified(ModuleSymName, Op),
    simplify_info_get_module_info(Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, PredTable),
    predicate_table_lookup_pred_sym_arity(PredTable, is_fully_qualified,
        OpSymName, 2, OpPredIds),
    ( if OpPredIds = [OpPredIdPrime] then
        OpPredId = OpPredIdPrime
    else
        unexpected($pred, "cannot find " ++ Op)
    ),
    OpProcIdInt = 0,
    proc_id_to_int(OpProcId, OpProcIdInt),
    OpArgs = [X, Y],
    MaybeUnifyContext = no,
    GoalExpr = plain_call(OpPredId, OpProcId, OpArgs, IsBuiltin,
        MaybeUnifyContext, OpSymName),
    goal_info_init(set_of_var.list_to_set([X, Y]), instmap_delta_bind_no_var,
        detism_semi, purity_pure, Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

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
    UnifyMode = unify_modes_lhs_rhs(
        from_to_insts(free, Ground),
        from_to_insts(Ground, Ground)),
    GoalExpr = unify(ConstVar, RHS, UnifyMode, Unification, UnifyContext),
    NonLocals = set_of_var.make_singleton(ConstVar),
    InstMapDelta = instmap_delta_bind_var(ConstVar),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%---------------------%

:- pred simplify_improve_arith_shift_cmp_ops(int_type::in, instmap::in,
    string::in, string::in, int::in, list(prog_var)::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is semidet.

simplify_improve_arith_shift_cmp_ops(IntType, InstMap0, ModuleName, PredName,
        _ModeNum, Args, ImprovedGoalExpr, !GoalInfo, !Info) :-
    simplify_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    (
        ( PredName = "quot_bits_per_int", Op = "unchecked_quotient"
        ; PredName = "rem_bits_per_int", Op = "unchecked_rem"
        ),
        % There is no point in checking whether bits_per_int is 0;
        % it isn't.
        IntType = int_type_int,
        Args = [X, Y],
        target_bits_per_int(Globals, bits_per_int(TargetBitsPerInt)),
        simplify_make_int_ico_op(ModuleName, Op, X, TargetBitsPerInt, Y,
            ImprovedGoalExpr, !.GoalInfo, !Info)
    ;
        PredName = "times_bits_per_int",
        IntType = int_type_int,
        Args = [X, Y],
        Op = "*",
        target_bits_per_int(Globals, bits_per_int(TargetBitsPerInt)),
        simplify_make_int_ico_op(ModuleName, Op, X, TargetBitsPerInt, Y,
            ImprovedGoalExpr, !.GoalInfo, !Info)
    ;
        ( PredName = "/",   Op = "unchecked_quotient"
        ; PredName = "//",  Op = "unchecked_quotient"
        ; PredName = "rem", Op = "unchecked_rem"
        ),
        Args = [X, Y, Z],
        instmap_lookup_var(InstMap0, Y, InstY),
        InstY = bound(_, _, [bound_functor(ConsY, [])]),
        is_non_zero_const(IntType, ConsY),
        simplify_make_binary_op_goal_expr(!.Info, ModuleName, Op,
            inline_builtin, X, Y, Z, ImprovedGoalExpr)
    ;
        ( PredName = "<<", Op = "unchecked_left_shift"
        ; PredName = ">>", Op = "unchecked_right_shift"
        ),
        Args = [X, Y, Z],
        instmap_lookup_var(InstMap0, Y, InstY),
        InstY = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal >= 0,
        YVal < int_type_target_bits(Globals, IntType),
        simplify_make_binary_op_goal_expr(!.Info, ModuleName, Op,
            inline_builtin, X, Y, Z, ImprovedGoalExpr)
    ;
        ( PredName = "<"
        ; PredName = ">"
        ; PredName = "=<"
        ; PredName = ">="
        ),
        replace_tautological_comparisons(PredName, Args, ImprovedGoalExpr)
    ).

:- pred replace_tautological_comparisons(string::in, list(prog_var)::in,
    hlds_goal_expr::out) is semidet.

replace_tautological_comparisons(PredName, Args, ImprovedGoalExpr) :-
    % Our callers should invoke us whenever the code here may succeed.
    (
        ( PredName = "<"
        ; PredName = ">"
        ),
        Args = [X, X],
        ImprovedGoalExpr = fail_goal_expr
    ;
        ( PredName = "=<"
        ; PredName = ">="
        ),
        Args = [X, X],
        ImprovedGoalExpr = true_goal_expr
    ).

%---------------------------------------------------------------------------%
%
% Utility predicates that may be of interest to more than one
% of the predicates above.
%

:- pred is_non_zero_const(int_type::in, cons_id::in) is semidet.

is_non_zero_const(IntType, ConsId) :-
    require_complete_switch[ IntType]
    (
        IntType = int_type_int,
        ConsId = int_const(Val),
        Val \= 0
    ;
        IntType = int_type_uint,
        ConsId = uint_const(Val),
        Val \= 0u
    ;
        IntType = int_type_int8,
        ConsId = int8_const(Val),
        Val \= 0i8
    ;
        IntType = int_type_uint8,
        ConsId = uint8_const(Val),
        Val \= 0u8
    ;
        IntType = int_type_int16,
        ConsId = int16_const(Val),
        Val \= 0i16
    ;
        IntType = int_type_uint16,
        ConsId = uint16_const(Val),
        Val \= 0u16
    ;
        IntType = int_type_int32,
        ConsId = int32_const(Val),
        Val \= 0i32
    ;
        IntType = int_type_uint32,
        ConsId = uint32_const(Val),
        Val \= 0u32
    ;
        IntType = int_type_int64,
        ConsId = int64_const(Val),
        Val \= 0i64
    ;
        IntType = int_type_uint64,
        ConsId = uint64_const(Val),
        Val \= 0u64
    ).

:- func int_type_target_bits(globals, int_type) = int.

int_type_target_bits(Globals, IntType) = IntTypeBits :-
    (
        ( IntType = int_type_int
        ; IntType = int_type_uint
        ),
        target_bits_per_int(Globals, bits_per_int(TargetBitsPerInt)),
        IntTypeBits = TargetBitsPerInt
    ;
        ( IntType = int_type_int8
        ; IntType = int_type_uint8
        ),
        IntTypeBits = 8
    ;
        ( IntType = int_type_int16
        ; IntType = int_type_uint16
        ),
        IntTypeBits = 16
    ;
        ( IntType = int_type_int32
        ; IntType = int_type_uint32
        ),
        IntTypeBits = 32
    ;
        ( IntType = int_type_int64
        ; IntType = int_type_uint64
        ),
        IntTypeBits = 64
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_goal_call.
%---------------------------------------------------------------------------%
