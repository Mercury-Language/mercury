%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: call_gen.m
%
% Authors: conway, zs.
%
% This module provides predicates for generating procedure calls,
% including calls to higher-order pred variables.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module ll_backend__call_gen.

:- interface.

:- import_module hlds__code_model.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_pred.
:- import_module libs__globals.
:- import_module ll_backend__code_info.
:- import_module ll_backend__llds.
:- import_module parse_tree__prog_data.

:- import_module assoc_list.
:- import_module list.

:- pred call_gen__generate_call(code_model::in, pred_id::in, proc_id::in,
    list(prog_var)::in, hlds_goal_info::in, code_tree::out,
    code_info::in, code_info::out) is det.

:- pred call_gen__generate_generic_call(code_model::in, generic_call::in,
    list(prog_var)::in, list(mode)::in, determinism::in,
    hlds_goal_info::in, code_tree::out, code_info::in, code_info::out)
    is det.

:- pred call_gen__generate_builtin(code_model::in, pred_id::in, proc_id::in,
    list(prog_var)::in, code_tree::out, code_info::in, code_info::out)
    is det.

:- type known_call_variant
    --->    known_num
    ;       unknown.

    % call_gen__generic_call_info(Globals, GenericCall, NumImmediateInputArgs,
    %   CodeAddr, SpecifierArgInfos, FirstImmediateInputReg, HoCallVariant).
    %
:- pred call_gen__generic_call_info(globals::in, generic_call::in, int::in,
    code_addr::out, assoc_list(prog_var, arg_info)::out, int::out,
    known_call_variant::out) is det.

:- pred call_gen__input_arg_locs(assoc_list(prog_var, arg_info)::in,
    assoc_list(prog_var, arg_loc)::out) is det.

:- pred call_gen__output_arg_locs(assoc_list(prog_var, arg_info)::in,
    assoc_list(prog_var, arg_loc)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module aditi_backend__rl.
:- import_module backend_libs__builtin_ops.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__polymorphism.
:- import_module check_hlds__type_util.
:- import_module check_hlds__unify_proc.
:- import_module hlds__arg_info.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_llds.
:- import_module hlds__hlds_module.
:- import_module hlds__instmap.
:- import_module libs__options.
:- import_module libs__tree.
:- import_module ll_backend__code_util.
:- import_module ll_backend__trace.
:- import_module parse_tree__error_util.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%

call_gen__generate_call(CodeModel, PredId, ProcId, ArgVars, GoalInfo, Code,
        !CI) :-

        % Find out which arguments are input and which are output.
    ArgInfo = code_info__get_pred_proc_arginfo(!.CI, PredId, ProcId),
    assoc_list__from_corresponding_lists(ArgVars, ArgInfo, ArgsInfos),

        % Save the necessary vars on the stack and move the input args
        % to their registers.
    code_info__setup_call(GoalInfo, ArgsInfos, LiveVals, SetupCode, !CI),
    call_gen__kill_dead_input_vars(ArgsInfos, GoalInfo, NonLiveOutputs,
        !CI),

        % Figure out what the call model is.
    call_gen__prepare_for_call(CodeModel, CallModel, TraceCode, !CI),

        % Make the call.
    code_info__get_module_info(!.CI, ModuleInfo),
    Address = code_info__make_entry_label(!.CI, ModuleInfo,
        PredId, ProcId, yes),
    code_info__get_next_label(ReturnLabel, !CI),
    call_gen__call_comment(CodeModel, CallComment),
    goal_info_get_context(GoalInfo, Context),
    goal_info_get_goal_path(GoalInfo, GoalPath),
    CallCode = node([
        livevals(LiveVals) - "",
        call(Address, label(ReturnLabel), ReturnLiveLvalues, Context,
            GoalPath, CallModel) - CallComment,
        label(ReturnLabel) - "continuation label"
    ]),

        % Figure out what variables will be live at the return point,
        % and where, for use in the accurate garbage collector, and
        % in the debugger.
    code_info__get_instmap(!.CI, InstMap),
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    instmap__apply_instmap_delta(InstMap, InstMapDelta, ReturnInstMap),

        % Update the code generator state to reflect the situation
        % after the call.
    call_gen__handle_return(ArgsInfos, GoalInfo, NonLiveOutputs,
        ReturnInstMap, ReturnLiveLvalues, !CI),

        % If the call can fail, generate code to check for and
        % handle the failure.
    call_gen__handle_failure(CodeModel, GoalInfo, FailHandlingCode, !CI),

    Code = tree_list([SetupCode, TraceCode, CallCode, FailHandlingCode]).

%---------------------------------------------------------------------------%

call_gen__generate_generic_call(OuterCodeModel, GenericCall, Args0,
        Modes0, Det, GoalInfo, Code, !CI) :-
    % For a generic_call, we split the arguments into inputs and outputs,
    % put the inputs in the locations expected by mercury__do_call_closure in
    % runtime/mercury_ho_call.c, generate the call to that code, and pick up
    % the outputs from the locations that we know the runtime system leaves
    % them in.

    % `cast' differs from the other generic call types in that there is no
    % address. Also, live_vars.m assumes that casts do not require live
    % variables to be saved to the stack.
    ( GenericCall = cast(_) ->
        ( Args0 = [InputArg, OutputArg] ->
            call_gen__generate_assign_builtin(OutputArg,
                leaf(InputArg), Code, !CI)
        ;
            unexpected(this_file,
                "generate_generic_call: invalid type/inst cast call")
        )
    ;
        call_gen__generate_generic_call_2(OuterCodeModel,
            GenericCall, Args0, Modes0, Det, GoalInfo, Code, !CI)
    ).

:- pred call_gen__generate_generic_call_2(code_model::in, generic_call::in,
    list(prog_var)::in, list(mode)::in, determinism::in,
    hlds_goal_info::in, code_tree::out, code_info::in, code_info::out)
    is det.

call_gen__generate_generic_call_2(_OuterCodeModel, GenericCall, Args,
        Modes, Det, GoalInfo, Code, !CI) :-
    Types = list__map(code_info__variable_type(!.CI), Args),

    code_info__get_module_info(!.CI, ModuleInfo),
    arg_info__compute_in_and_out_vars(ModuleInfo, Args, Modes, Types,
        InVars, OutVars),
    module_info_globals(ModuleInfo, Globals),
    call_gen__generic_call_info(Globals, GenericCall, length(InVars),
        CodeAddr, SpecifierArgInfos, FirstImmInput, HoCallVariant),
    determinism_to_code_model(Det, CodeModel),
    ( CodeModel = model_semi ->
        FirstOutput = 2
    ;
        FirstOutput = 1
    ),

    give_vars_consecutive_arg_infos(InVars, FirstImmInput, top_in,
        InVarArgInfos),
    give_vars_consecutive_arg_infos(OutVars, FirstOutput, top_out,
        OutArgsInfos),
    list__append(SpecifierArgInfos, InVarArgInfos, InArgInfos),
    list__append(InArgInfos, OutArgsInfos, ArgInfos),

        % Save the necessary vars on the stack and move the input args
        % defined by variables to their registers.
    code_info__setup_call(GoalInfo, ArgInfos, LiveVals0, SetupCode, !CI),
    call_gen__kill_dead_input_vars(ArgInfos, GoalInfo, NonLiveOutputs,
        !CI),

        % Move the input args not defined by variables to their
        % registers. Setting up these arguments last results in
        % slightly more efficient code, since we can use their
        % registers when placing the variables.
    call_gen__generic_call_nonvar_setup(GenericCall, HoCallVariant,
        InVars, OutVars, NonVarCode, !CI),

    call_gen__extra_livevals(FirstImmInput, ExtraLiveVals),
    set__insert_list(LiveVals0, ExtraLiveVals, LiveVals),

    call_gen__prepare_for_call(CodeModel, CallModel, TraceCode, !CI),

        % Make the call.
    code_info__get_next_label(ReturnLabel, !CI),
    goal_info_get_context(GoalInfo, Context),
    goal_info_get_goal_path(GoalInfo, GoalPath),

        % Figure out what variables will be live at the return point,
        % and where, for use in the accurate garbage collector, and
        % in the debugger.
    code_info__get_instmap(!.CI, InstMap),
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    instmap__apply_instmap_delta(InstMap, InstMapDelta, ReturnInstMap),

        % Update the code generator state to reflect the situation
        % after the call.
    call_gen__handle_return(OutArgsInfos, GoalInfo, NonLiveOutputs,
        ReturnInstMap, ReturnLiveLvalues, !CI),

    CallCode = node([
        livevals(LiveVals) - "",
        call(CodeAddr, label(ReturnLabel), ReturnLiveLvalues,
            Context, GoalPath, CallModel) - "Setup and call",
        label(ReturnLabel) - "Continuation label"
    ]),

        % If the call can fail, generate code to check for and
        % handle the failure.
    call_gen__handle_failure(CodeModel, GoalInfo, FailHandlingCode, !CI),

    Code = tree_list([SetupCode, NonVarCode, TraceCode, CallCode,
        FailHandlingCode]).

%---------------------------------------------------------------------------%

    % The registers before the first input argument are all live.
    %
:- pred call_gen__extra_livevals(int::in, list(lval)::out) is det.

call_gen__extra_livevals(FirstInput, ExtraLiveVals) :-
    call_gen__extra_livevals(1, FirstInput, ExtraLiveVals).

:- pred call_gen__extra_livevals(int::in, int::in, list(lval)::out) is det.

call_gen__extra_livevals(Reg, FirstInput, ExtraLiveVals) :-
    ( Reg < FirstInput ->
        ExtraLiveVals = [reg(r, Reg) | ExtraLiveVals1],
        NextReg = Reg + 1,
        call_gen__extra_livevals(NextReg, FirstInput, ExtraLiveVals1)
    ;
        ExtraLiveVals = []
    ).

call_gen__generic_call_info(Globals, GenericCall, NumInputArgs, CodeAddr,
        SpecifierArgInfos, FirstImmediateInputReg, HoCallVariant) :-
    (
        GenericCall = higher_order(PredVar, _, _, _),
        SpecifierArgInfos = [PredVar - arg_info(1, top_in)],
        globals__lookup_int_option(Globals,
            max_specialized_do_call_closure, MaxSpec),
        (
            MaxSpec >= 0,
            NumInputArgs =< MaxSpec
        ->
            CodeAddr = do_call_closure(specialized_known(NumInputArgs)),
            HoCallVariant = known_num,
            FirstImmediateInputReg = 2
        ;
            CodeAddr = do_call_closure(generic),
            HoCallVariant = unknown,
            FirstImmediateInputReg = 3
        )
    ;
        GenericCall = class_method(TCVar, _, _, _),
        SpecifierArgInfos = [TCVar - arg_info(1, top_in)],
        globals__lookup_int_option(Globals,
            max_specialized_do_call_class_method, MaxSpec),
        (
            MaxSpec >= 0,
            NumInputArgs =< MaxSpec
        ->
            CodeAddr = do_call_class_method(specialized_known(NumInputArgs)),
            HoCallVariant = known_num,
            FirstImmediateInputReg = 3
        ;
            CodeAddr = do_call_class_method(generic),
            HoCallVariant = unknown,
            FirstImmediateInputReg = 4
        )
    ;
        % Casts are generated inline.
        GenericCall = cast(_),
        CodeAddr = do_not_reached,
        SpecifierArgInfos = [],
        FirstImmediateInputReg = 1,
        HoCallVariant = unknown     % dummy; not used
    ;
        GenericCall = aditi_builtin(_, _),
        % These should have been transformed into normal calls.
        unexpected(this_file, "generic_call_info: aditi_builtin")
    ).

    % Some of the values that generic call passes to the dispatch routine
    % to specify what code is being indirectly called come from HLDS
    % variables, while the others come from constants. The ones that come
    % from variables (the closure for a higher order call, the
    % typeclass_info for a method call) are set up together with the
    % arguments being passed the indirectly called code, since with eager
    % code generation this ensures that each target register is reserved
    % for the variable destined for it. This is set up by
    % call_gen__generic_call_info. call_gen__generic_call_nonvar_setup
    % generates code to pass to the dispatch routine the parts of the
    % indirectly called code's identifier that come from constants.
    %
:- pred call_gen__generic_call_nonvar_setup(generic_call::in,
    known_call_variant::in, list(prog_var)::in, list(prog_var)::in,
    code_tree::out, code_info::in, code_info::out) is det.

call_gen__generic_call_nonvar_setup(higher_order(_, _, _, _),
        HoCallVariant, InVars, _OutVars, Code, !CI) :-
    (
        HoCallVariant = known_num,
        Code = empty
    ;
        HoCallVariant = unknown,
        code_info__clobber_regs([reg(r, 2)], !CI),
        list__length(InVars, NInVars),
        Code = node([
            assign(reg(r, 2), const(int_const(NInVars))) -
                "Assign number of immediate input arguments"
        ])
    ).
call_gen__generic_call_nonvar_setup(class_method(_, Method, _, _),
        HoCallVariant, InVars, _OutVars, Code, !CI) :-
    (
        HoCallVariant = known_num,
        code_info__clobber_regs([reg(r, 2)], !CI),
        Code = node([
            assign(reg(r, 2), const(int_const(Method))) -
                "Index of class method in typeclass info"
        ])
    ;
        HoCallVariant = unknown,
        code_info__clobber_regs([reg(r, 2), reg(r, 3)], !CI),
        list__length(InVars, NInVars),
        Code = node([
            assign(reg(r, 2), const(int_const(Method))) -
                "Index of class method in typeclass info",
            assign(reg(r, 3), const(int_const(NInVars))) -
                "Assign number of immediate input arguments"
        ])
    ).
call_gen__generic_call_nonvar_setup(cast(_), _, _, _, _, !CI) :-
    unexpected(this_file, "generic_call_nonvar_setup: cast").
call_gen__generic_call_nonvar_setup(aditi_builtin(_, _), _, _, _, _, !CI) :-
    % These should have been transformed into normal calls.
    unexpected(this_file, "generic_call_nonvar_setup: aditi_builtin").

%---------------------------------------------------------------------------%

:- pred call_gen__prepare_for_call(code_model::in, call_model::out,
    code_tree::out, code_info::in, code_info::out) is det.

call_gen__prepare_for_call(CodeModel, CallModel, TraceCode, !CI) :-
    code_info__succip_is_used(!CI),
    (
        CodeModel = model_det,
        CallModel = det
    ;
        CodeModel = model_semi,
        CallModel = semidet
    ;
        CodeModel = model_non,
        code_info__may_use_nondet_tailcall(!.CI, TailCallStatus),
        CallModel = nondet(TailCallStatus),
        code_info__set_resume_point_and_frame_to_unknown(!CI)
    ),
    trace__prepare_for_call(!.CI, TraceCode).

:- pred call_gen__handle_failure(code_model::in, hlds_goal_info::in,
    code_tree::out, code_info::in, code_info::out) is det.

call_gen__handle_failure(CodeModel, GoalInfo, FailHandlingCode, !CI) :-
    ( CodeModel = model_semi ->
        goal_info_get_determinism(GoalInfo, Detism),
        ( Detism = failure ->
            code_info__generate_failure(FailHandlingCode, !CI)
        ;
            code_info__get_next_label(ContLab, !CI),
            FailTestCode = node([
                if_val(lval(reg(r, 1)), label(ContLab)) - "test for success"
            ]),
            code_info__generate_failure(FailCode, !CI),
            ContLabelCode = node([
                label(ContLab) - ""
            ]),
            FailHandlingCode = tree_list([FailTestCode,
                FailCode, ContLabelCode])
        )
    ;
        FailHandlingCode = empty
    ).

:- pred call_gen__call_comment(code_model::in, string::out) is det.

call_gen__call_comment(model_det,  "branch to det procedure").
call_gen__call_comment(model_semi, "branch to semidet procedure").
call_gen__call_comment(model_non,  "branch to nondet procedure").

%---------------------------------------------------------------------------%

    % After we have placed all the input variables in their registers,
    % we will want to clear all the registers so we can start updating
    % the code generator state to reflect their contents after the call.
    % (In the case of higher order calls, we may place some constant
    % input arguments in registers before clearing them.) The register
    % clearing code complains if it is asked to dispose of the last copy
    % of a still live variable, so before we clear the registers, we must
    % make forward-dead all the variables that are in this goal's
    % post-death set. However, a variable may be in this set even if it
    % is not live before the call, if it is bound by the call. (This can
    % happen when the caller ignores some of the output arguments of the
    % called procedure.) We handle such variables not by making them
    % forward-dead but by simply never making them forward-live in the
    % first place.
    %
    % ArgsInfos should list all the output arguments of the call.
    % It may contain the input arguments as well; kill_dead_input_vars
    % and handle_return ignore them.
    %
:- pred call_gen__kill_dead_input_vars(assoc_list(prog_var, arg_info)::in,
    hlds_goal_info::in, set(prog_var)::out,
    code_info::in, code_info::out) is det.

call_gen__kill_dead_input_vars(ArgsInfos, GoalInfo, NonLiveOutputs, !CI) :-
    code_info__get_forward_live_vars(!.CI, Liveness),
    call_gen__find_nonlive_outputs(ArgsInfos, Liveness,
        set__init, NonLiveOutputs),
    goal_info_get_post_deaths(GoalInfo, PostDeaths),
    set__difference(PostDeaths, NonLiveOutputs, ImmediatePostDeaths),
    code_info__make_vars_forward_dead(ImmediatePostDeaths, !CI).

:- pred call_gen__handle_return(assoc_list(prog_var, arg_info)::in,
    hlds_goal_info::in, set(prog_var)::in, instmap::in,
    list(liveinfo)::out, code_info::in, code_info::out) is det.

call_gen__handle_return(ArgsInfos, GoalInfo, _NonLiveOutputs, ReturnInstMap,
        ReturnLiveLvalues, !CI) :-
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    ( instmap_delta_is_reachable(InstMapDelta) ->
        OkToDeleteAny = no
    ;
        OkToDeleteAny = yes
    ),
    code_info__clear_all_registers(OkToDeleteAny, !CI),
    code_info__get_forward_live_vars(!.CI, Liveness),
    call_gen__rebuild_registers(ArgsInfos, Liveness, OutputArgLocs, !CI),
    code_info__generate_return_live_lvalues(!.CI, OutputArgLocs,
        ReturnInstMap, OkToDeleteAny, ReturnLiveLvalues).

:- pred call_gen__find_nonlive_outputs(assoc_list(prog_var, arg_info)::in,
    set(prog_var)::in, set(prog_var)::in, set(prog_var)::out) is det.

call_gen__find_nonlive_outputs([], _, NonLiveOutputs, NonLiveOutputs).
call_gen__find_nonlive_outputs([Var - arg_info(_ArgLoc, Mode) | Args],
        Liveness, NonLiveOutputs0, NonLiveOutputs) :-
    ( Mode = top_out ->
        ( set__member(Var, Liveness) ->
            NonLiveOutputs1 = NonLiveOutputs0
        ;
            set__insert(NonLiveOutputs0, Var, NonLiveOutputs1)
        )
    ;
        NonLiveOutputs1 = NonLiveOutputs0
    ),
    call_gen__find_nonlive_outputs(Args, Liveness,
        NonLiveOutputs1, NonLiveOutputs).

:- pred call_gen__rebuild_registers(assoc_list(prog_var, arg_info)::in,
    set(prog_var)::in, assoc_list(prog_var, arg_loc)::out,
    code_info::in, code_info::out) is det.

call_gen__rebuild_registers([], _, [], !CI).
call_gen__rebuild_registers([Var - arg_info(ArgLoc, Mode) | Args], Liveness,
        OutputArgLocs, !CI) :-
    call_gen__rebuild_registers(Args, Liveness, OutputArgLocs1, !CI),
    (
        Mode = top_out,
        set__member(Var, Liveness)
    ->
        code_util__arg_loc_to_register(ArgLoc, Register),
        code_info__set_var_location(Var, Register, !CI),
        OutputArgLocs = [Var - ArgLoc | OutputArgLocs1]
    ;
        OutputArgLocs = OutputArgLocs1
    ).

%---------------------------------------------------------------------------%

call_gen__generate_builtin(CodeModel, PredId, ProcId, Args, Code, !CI) :-
    code_info__get_module_info(!.CI, ModuleInfo),
    predicate_module(ModuleInfo, PredId, ModuleName),
    predicate_name(ModuleInfo, PredId, PredName),
    (
        builtin_ops__translate_builtin(ModuleName, PredName,
            ProcId, Args, SimpleCode0)
    ->
        SimpleCode = SimpleCode0
    ;
        length(Args, Arity),
        format("Unknown builtin predicate: %s/%d",
            [s(PredName), i(Arity)], Msg),
        error(Msg)
    ),
    (
        CodeModel = model_det,
        ( SimpleCode = assign(Var, AssignExpr) ->
            call_gen__generate_assign_builtin(Var,
                AssignExpr, Code, !CI)
        ;
            error("Malformed det builtin predicate")
        )
    ;
        CodeModel = model_semi,
        ( SimpleCode = test(TestExpr) ->
            call_gen__generate_simple_test(TestExpr, Rval,
                ArgCode, !CI),
            code_info__fail_if_rval_is_false(Rval, TestCode, !CI),
            Code = tree(ArgCode, TestCode)
        ;
            error("Malformed semi builtin predicate")
        )
    ;
        CodeModel = model_non,
        error("Nondet builtin predicate")
    ).

:- pred call_gen__generate_assign_builtin(prog_var::in,
    simple_expr(prog_var)::in, code_tree::out,
    code_info::in, code_info::out) is det.

call_gen__generate_assign_builtin(Var, AssignExpr, Code, !CI) :-
    ( code_info__variable_is_forward_live(!.CI, Var) ->
        Rval = convert_simple_expr(AssignExpr),
        code_info__assign_expr_to_var(Var, Rval, Code, !CI)
    ;
        Code = empty
    ).

:- func convert_simple_expr(simple_expr(prog_var)) = rval.

convert_simple_expr(leaf(Var)) = var(Var).
convert_simple_expr(int_const(Int)) = const(int_const(Int)).
convert_simple_expr(float_const(Float)) = const(float_const(Float)).
convert_simple_expr(unary(UnOp, Expr)) =
    unop(UnOp, convert_simple_expr(Expr)).
convert_simple_expr(binary(BinOp, Expr1, Expr2)) =
    binop(BinOp, convert_simple_expr(Expr1), convert_simple_expr(Expr2)).

:- pred call_gen__generate_simple_test(
    simple_expr(prog_var)::in(simple_test_expr), rval::out,
    code_tree::out, code_info::in, code_info::out) is det.

call_gen__generate_simple_test(TestExpr, Rval, ArgCode, !CI) :-
    (
        TestExpr = binary(BinOp, X0, Y0),
        X1 = convert_simple_expr(X0),
        Y1 = convert_simple_expr(Y0),
        call_gen__generate_builtin_arg(X1, X, CodeX, !CI),
        call_gen__generate_builtin_arg(Y1, Y, CodeY, !CI),
        Rval = binop(BinOp, X, Y),
        ArgCode = tree(CodeX, CodeY)
    ;
        TestExpr = unary(UnOp, X0),
        X1 = convert_simple_expr(X0),
        call_gen__generate_builtin_arg(X1, X, ArgCode, !CI),
        Rval = unop(UnOp, X)
    ).

:- pred call_gen__generate_builtin_arg(rval::in, rval::out, code_tree::out,
    code_info::in, code_info::out) is det.

call_gen__generate_builtin_arg(Rval0, Rval, Code, !CI) :-
    ( Rval0 = var(Var) ->
        code_info__produce_variable(Var, Code, Rval, !CI)
    ;
        Rval = Rval0,
        Code = empty
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

call_gen__input_arg_locs([], []).
call_gen__input_arg_locs([Var - arg_info(Loc, Mode) | Args], Vs) :-
    call_gen__input_arg_locs(Args, Vs0),
    ( Mode = top_in ->
        Vs = [Var - Loc | Vs0]
    ;
        Vs = Vs0
    ).

call_gen__output_arg_locs([], []).
call_gen__output_arg_locs([Var - arg_info(Loc, Mode) | Args], Vs) :-
    call_gen__output_arg_locs(Args, Vs0),
    ( Mode = top_out ->
        Vs = [Var - Loc | Vs0]
    ;
        Vs = Vs0
    ).

%---------------------------------------------------------------------------%

:- pred call_gen__generate_call_vn_livevals(list(arg_loc)::in,
    set(prog_var)::in, code_tree::out,
    code_info::in, code_info::out) is det.

call_gen__generate_call_vn_livevals(InputArgLocs, OutputArgs, Code, !CI) :-
    code_info__generate_call_vn_livevals(!.CI, InputArgLocs, OutputArgs,
        LiveVals),
    Code = node([
        livevals(LiveVals) - ""
    ]).

%---------------------------------------------------------------------------%

:- pred give_vars_consecutive_arg_infos(list(prog_var)::in, int::in,
    arg_mode::in, assoc_list(prog_var, arg_info)::out) is det.

give_vars_consecutive_arg_infos([], _N, _M, []).
give_vars_consecutive_arg_infos([Var | Vars], N0, ArgMode,
        [Var - ArgInfo | ArgInfos]) :-
    ArgInfo = arg_info(N0, ArgMode),
    N1 = N0 + 1,
    give_vars_consecutive_arg_infos(Vars, N1, ArgMode, ArgInfos).

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "call_gen.m".

%---------------------------------------------------------------------------%
