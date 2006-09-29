%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: call_gen.m.
% Authors: conway, zs.
%
% This module provides predicates for generating procedure calls,
% including calls to higher-order pred variables.
%
%---------------------------------------------------------------------------%

:- module ll_backend.call_gen.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module libs.globals.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred generate_call(code_model::in, pred_id::in, proc_id::in,
    list(prog_var)::in, hlds_goal_info::in, code_tree::out,
    code_info::in, code_info::out) is det.

:- pred generate_generic_call(code_model::in, generic_call::in,
    list(prog_var)::in, list(mer_mode)::in, determinism::in,
    hlds_goal_info::in, code_tree::out, code_info::in, code_info::out) is det.

:- pred generate_builtin(code_model::in, pred_id::in, proc_id::in,
    list(prog_var)::in, code_tree::out, code_info::in, code_info::out) is det.

:- type known_call_variant
    --->    ho_call_known_num
    ;       ho_call_unknown.

    % generic_call_info(Globals, GenericCall, NumImmediateInputArgs, CodeAddr,
    %   SpecifierArgInfos, FirstImmediateInputReg, HoCallVariant).
    %
:- pred generic_call_info(globals::in, generic_call::in, int::in,
    code_addr::out, assoc_list(prog_var, arg_info)::out, int::out,
    known_call_variant::out) is det.

:- pred input_arg_locs(assoc_list(prog_var, arg_info)::in,
    assoc_list(prog_var, arg_loc)::out) is det.

:- pred output_arg_locs(assoc_list(prog_var, arg_info)::in,
    assoc_list(prog_var, arg_loc)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.polymorphism.
:- import_module check_hlds.type_util.
:- import_module check_hlds.unify_proc.
:- import_module hlds.arg_info.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module libs.compiler_util.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module ll_backend.code_util.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.trace_gen.
:- import_module parse_tree.prog_event.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_call(CodeModel, PredId, ProcId, ArgVars, GoalInfo, Code, !CI) :-
    % Find out which arguments are input and which are output.
    ArgInfo = code_info.get_pred_proc_arginfo(!.CI, PredId, ProcId),
    assoc_list.from_corresponding_lists(ArgVars, ArgInfo, ArgsInfos),

    % Save the necessary vars on the stack and move the input args
    % to their registers.
    code_info.setup_call(GoalInfo, ArgsInfos, LiveVals, SetupCode, !CI),
    kill_dead_input_vars(ArgsInfos, GoalInfo, NonLiveOutputs, !CI),

    % Figure out what the call model is.
    call_gen.prepare_for_call(CodeModel, CallModel, TraceCode, !CI),

    % Make the call.
    code_info.get_module_info(!.CI, ModuleInfo),
    Address = code_info.make_entry_label(!.CI, ModuleInfo,
        PredId, ProcId, yes),
    code_info.get_next_label(ReturnLabel, !CI),
    call_gen.call_comment(CodeModel, CallComment),
    goal_info_get_context(GoalInfo, Context),
    goal_info_get_goal_path(GoalInfo, GoalPath),
    CallCode = node([
        livevals(LiveVals) - "",
        llcall(Address, label(ReturnLabel), ReturnLiveLvalues, Context,
            GoalPath, CallModel) - CallComment,
        label(ReturnLabel) - "continuation label"
    ]),

    % Figure out what variables will be live at the return point, and where,
    % for use in the accurate garbage collector, and in the debugger.
    code_info.get_instmap(!.CI, InstMap),
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    instmap.apply_instmap_delta(InstMap, InstMapDelta, ReturnInstMap),

    % Update the code generator state to reflect the situation after the call.
    handle_return(ArgsInfos, GoalInfo, NonLiveOutputs,
        ReturnInstMap, ReturnLiveLvalues, !CI),

    % If the call can fail, generate code to check for and handle the failure.
    handle_failure(CodeModel, GoalInfo, FailHandlingCode, !CI),

    Code = tree_list([SetupCode, TraceCode, CallCode, FailHandlingCode]).

%---------------------------------------------------------------------------%

generate_generic_call(OuterCodeModel, GenericCall, Args, Modes, Det,
        GoalInfo, Code, !CI) :-
    % For a generic_call, we split the arguments into inputs and outputs,
    % put the inputs in the locations expected by mercury.do_call_closure in
    % runtime/mercury_ho_call.c, generate the call to that code, and pick up
    % the outputs from the locations that we know the runtime system leaves
    % them in.

    % `cast' differs from the other generic call types in that there is no
    % address. Also, live_vars.m assumes that casts do not require live
    % variables to be saved to the stack.
    (
        ( GenericCall = higher_order(_, _, _, _)
        ; GenericCall = class_method(_, _, _, _)
        ),
        generate_main_generic_call(OuterCodeModel, GenericCall, Args, Modes,
            Det, GoalInfo, Code, !CI)
    ;
        GenericCall = event_call(EventName),
        generate_event_call(EventName, Args, GoalInfo, Code, !CI)
    ;
        GenericCall = cast(_),
        ( Args = [InputArg, OutputArg] ->
            generate_assign_builtin(OutputArg, leaf(InputArg), Code, !CI)
        ;
            unexpected(this_file,
                "generate_generic_call: invalid type/inst cast call")
        )
    ).

:- pred generate_main_generic_call(code_model::in, generic_call::in,
    list(prog_var)::in, list(mer_mode)::in, determinism::in,
    hlds_goal_info::in, code_tree::out, code_info::in, code_info::out)
    is det.

generate_main_generic_call(_OuterCodeModel, GenericCall, Args, Modes, Det,
        GoalInfo, Code, !CI) :-
    Types = list.map(code_info.variable_type(!.CI), Args),

    code_info.get_module_info(!.CI, ModuleInfo),
    arg_info.compute_in_and_out_vars(ModuleInfo, Args, Modes, Types,
        InVars, OutVars),
    module_info_get_globals(ModuleInfo, Globals),
    generic_call_info(Globals, GenericCall, length(InVars), CodeAddr,
        SpecifierArgInfos, FirstImmInput, HoCallVariant),
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
    ArgInfos = SpecifierArgInfos ++ InVarArgInfos ++ OutArgsInfos,

    % Save the necessary vars on the stack and move the input args defined
    % by variables to their registers.
    code_info.setup_call(GoalInfo, ArgInfos, LiveVals0, SetupCode, !CI),
    kill_dead_input_vars(ArgInfos, GoalInfo, NonLiveOutputs, !CI),

    % Move the input args not defined by variables to their registers.
    % Setting up these arguments last results in slightly more efficient code,
    % since we can use their registers when placing the variables.
    generic_call_nonvar_setup(GenericCall, HoCallVariant, InVars, OutVars,
        NonVarCode, !CI),

    extra_livevals(FirstImmInput, ExtraLiveVals),
    set.insert_list(LiveVals0, ExtraLiveVals, LiveVals),

    call_gen.prepare_for_call(CodeModel, CallModel, TraceCode, !CI),

    % Make the call.
    code_info.get_next_label(ReturnLabel, !CI),
    goal_info_get_context(GoalInfo, Context),
    goal_info_get_goal_path(GoalInfo, GoalPath),

    % Figure out what variables will be live at the return point, and where,
    % for use in the accurate garbage collector, and in the debugger.
    code_info.get_instmap(!.CI, InstMap),
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    instmap.apply_instmap_delta(InstMap, InstMapDelta, ReturnInstMap),

    % Update the code generator state to reflect the situation after the call.
    handle_return(OutArgsInfos, GoalInfo, NonLiveOutputs,
        ReturnInstMap, ReturnLiveLvalues, !CI),

    CallCode = node([
        livevals(LiveVals) - "",
        llcall(CodeAddr, label(ReturnLabel), ReturnLiveLvalues,
            Context, GoalPath, CallModel) - "Setup and call",
        label(ReturnLabel) - "Continuation label"
    ]),

    % If the call can fail, generate code to check for and handle the failure.
    handle_failure(CodeModel, GoalInfo, FailHandlingCode, !CI),

    Code = tree_list([SetupCode, NonVarCode, TraceCode, CallCode,
        FailHandlingCode]).

%---------------------------------------------------------------------------%

:- pred generate_event_call(string::in, list(prog_var)::in, hlds_goal_info::in,
    code_tree::out, code_info::in, code_info::out) is det.

generate_event_call(EventName, Args, GoalInfo, Code, !CI) :-
    ( event_arg_names(EventName, AttributeNames) ->
        generate_event_attributes(AttributeNames, Args, Attributes, AttrCodes,
            !CI),
        SolverEventInfo = solver_event_info(EventName, Attributes),
        generate_solver_event_code(SolverEventInfo, GoalInfo, EventCode, !CI),
        Code = tree(tree_list(AttrCodes), EventCode)
    ;
        unexpected(this_file, "generate_event_call: bad event name")
    ).

:- pred generate_event_attributes(list(string)::in, list(prog_var)::in,
    list(solver_attribute)::out, list(code_tree)::out,
    code_info::in, code_info::out) is det.

generate_event_attributes([], [], [], [], !CI).
generate_event_attributes([], [_ | _], _, _, !CI) :-
    unexpected(this_file, "generate_event_attributes: list length mismatch").
generate_event_attributes([_ | _], [], _, _, !CI) :-
    unexpected(this_file, "generate_event_attributes: list length mismatch").
generate_event_attributes([Name | Names], [Var | Vars], [Attr | Attrs],
        [Code | Codes], !CI) :-
    produce_variable(Var, Code, Rval, !CI),
    Type = variable_type(!.CI, Var),
    Attr = solver_attribute(Rval, Type, Name),
    generate_event_attributes(Names, Vars, Attrs, Codes, !CI).

%---------------------------------------------------------------------------%

    % The registers before the first input argument are all live.
    %
:- pred extra_livevals(int::in, list(lval)::out) is det.

extra_livevals(FirstInput, ExtraLiveVals) :-
    extra_livevals_from(1, FirstInput, ExtraLiveVals).

:- pred extra_livevals_from(int::in, int::in, list(lval)::out) is det.

extra_livevals_from(Reg, FirstInput, ExtraLiveVals) :-
    ( Reg < FirstInput ->
        ExtraLiveVals = [reg(reg_r, Reg) | ExtraLiveVals1],
        NextReg = Reg + 1,
        extra_livevals_from(NextReg, FirstInput, ExtraLiveVals1)
    ;
        ExtraLiveVals = []
    ).

generic_call_info(Globals, GenericCall, NumInputArgs, CodeAddr,
        SpecifierArgInfos, FirstImmediateInputReg, HoCallVariant) :-
    (
        GenericCall = higher_order(PredVar, _, _, _),
        SpecifierArgInfos = [PredVar - arg_info(1, top_in)],
        globals.lookup_int_option(Globals,
            max_specialized_do_call_closure, MaxSpec),
        (
            MaxSpec >= 0,
            NumInputArgs =< MaxSpec
        ->
            CodeAddr = do_call_closure(specialized_known(NumInputArgs)),
            HoCallVariant = ho_call_known_num,
            FirstImmediateInputReg = 2
        ;
            CodeAddr = do_call_closure(generic),
            HoCallVariant = ho_call_unknown,
            FirstImmediateInputReg = 3
        )
    ;
        GenericCall = class_method(TCVar, _, _, _),
        SpecifierArgInfos = [TCVar - arg_info(1, top_in)],
        globals.lookup_int_option(Globals,
            max_specialized_do_call_class_method, MaxSpec),
        (
            MaxSpec >= 0,
            NumInputArgs =< MaxSpec
        ->
            CodeAddr = do_call_class_method(specialized_known(NumInputArgs)),
            HoCallVariant = ho_call_known_num,
            FirstImmediateInputReg = 3
        ;
            CodeAddr = do_call_class_method(generic),
            HoCallVariant = ho_call_unknown,
            FirstImmediateInputReg = 4
        )
    ;
        % Events and casts are generated inline.
        ( GenericCall = event_call(_)
        ; GenericCall = cast(_)
        ),
        CodeAddr = do_not_reached,
        SpecifierArgInfos = [],
        FirstImmediateInputReg = 1,
        HoCallVariant = ho_call_unknown     % dummy; not used
    ).

    % Some of the values that generic call passes to the dispatch routine
    % to specify what code is being indirectly called come from HLDS
    % variables, while the others come from constants. The ones that come
    % from variables (the closure for a higher order call, the
    % typeclass_info for a method call) are set up together with the
    % arguments being passed the indirectly called code, since with eager
    % code generation this ensures that each target register is reserved
    % for the variable destined for it. This is set up by generic_call_info.
    % generic_call_nonvar_setup generates code to pass to the dispatch routine
    % the parts of the indirectly called code's identifier that come from
    % constants.
    %
:- pred generic_call_nonvar_setup(generic_call::in, known_call_variant::in,
    list(prog_var)::in, list(prog_var)::in, code_tree::out,
    code_info::in, code_info::out) is det.

generic_call_nonvar_setup(higher_order(_, _, _, _), HoCallVariant,
        InVars, _OutVars, Code, !CI) :-
    (
        HoCallVariant = ho_call_known_num,
        Code = empty
    ;
        HoCallVariant = ho_call_unknown,
        code_info.clobber_regs([reg(reg_r, 2)], !CI),
        list.length(InVars, NInVars),
        Code = node([
            assign(reg(reg_r, 2), const(llconst_int(NInVars))) -
                "Assign number of immediate input arguments"
        ])
    ).
generic_call_nonvar_setup(class_method(_, Method, _, _), HoCallVariant,
        InVars, _OutVars, Code, !CI) :-
    (
        HoCallVariant = ho_call_known_num,
        code_info.clobber_regs([reg(reg_r, 2)], !CI),
        Code = node([
            assign(reg(reg_r, 2), const(llconst_int(Method))) -
                "Index of class method in typeclass info"
        ])
    ;
        HoCallVariant = ho_call_unknown,
        code_info.clobber_regs([reg(reg_r, 2), reg(reg_r, 3)], !CI),
        list.length(InVars, NInVars),
        Code = node([
            assign(reg(reg_r, 2), const(llconst_int(Method))) -
                "Index of class method in typeclass info",
            assign(reg(reg_r, 3), const(llconst_int(NInVars))) -
                "Assign number of immediate input arguments"
        ])
    ).
generic_call_nonvar_setup(event_call(_), _, _, _, _, !CI) :-
    unexpected(this_file, "generic_call_nonvar_setup: event_call").
generic_call_nonvar_setup(cast(_), _, _, _, _, !CI) :-
    unexpected(this_file, "generic_call_nonvar_setup: cast").

%---------------------------------------------------------------------------%

:- pred prepare_for_call(code_model::in, call_model::out,
    code_tree::out, code_info::in, code_info::out) is det.

prepare_for_call(CodeModel, CallModel, TraceCode, !CI) :-
    code_info.succip_is_used(!CI),
    (
        CodeModel = model_det,
        CallModel = call_model_det
    ;
        CodeModel = model_semi,
        CallModel = call_model_semidet
    ;
        CodeModel = model_non,
        code_info.may_use_nondet_tailcall(!.CI, TailCallStatus),
        CallModel = call_model_nondet(TailCallStatus),
        code_info.set_resume_point_and_frame_to_unknown(!CI)
    ),
    trace_prepare_for_call(!.CI, TraceCode).

:- pred handle_failure(code_model::in, hlds_goal_info::in,
    code_tree::out, code_info::in, code_info::out) is det.

handle_failure(CodeModel, GoalInfo, FailHandlingCode, !CI) :-
    ( CodeModel = model_semi ->
        goal_info_get_determinism(GoalInfo, Detism),
        ( Detism = detism_failure ->
            code_info.generate_failure(FailHandlingCode, !CI)
        ;
            code_info.get_next_label(ContLab, !CI),
            FailTestCode = node([
                if_val(lval(reg(reg_r, 1)), label(ContLab))
                    - "test for success"
            ]),
            code_info.generate_failure(FailCode, !CI),
            ContLabelCode = node([
                label(ContLab) - ""
            ]),
            FailHandlingCode = tree_list([FailTestCode,
                FailCode, ContLabelCode])
        )
    ;
        FailHandlingCode = empty
    ).

:- pred call_comment(code_model::in, string::out) is det.

call_comment(model_det,  "branch to det procedure").
call_comment(model_semi, "branch to semidet procedure").
call_comment(model_non,  "branch to nondet procedure").

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
:- pred kill_dead_input_vars(assoc_list(prog_var, arg_info)::in,
    hlds_goal_info::in, set(prog_var)::out,
    code_info::in, code_info::out) is det.

kill_dead_input_vars(ArgsInfos, GoalInfo, NonLiveOutputs, !CI) :-
    code_info.get_forward_live_vars(!.CI, Liveness),
    find_nonlive_outputs(ArgsInfos, Liveness, set.init, NonLiveOutputs),
    goal_info_get_post_deaths(GoalInfo, PostDeaths),
    set.difference(PostDeaths, NonLiveOutputs, ImmediatePostDeaths),
    code_info.make_vars_forward_dead(ImmediatePostDeaths, !CI).

:- pred handle_return(assoc_list(prog_var, arg_info)::in,
    hlds_goal_info::in, set(prog_var)::in, instmap::in,
    list(liveinfo)::out, code_info::in, code_info::out) is det.

handle_return(ArgsInfos, GoalInfo, _NonLiveOutputs, ReturnInstMap,
        ReturnLiveLvalues, !CI) :-
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    ( instmap_delta_is_reachable(InstMapDelta) ->
        OkToDeleteAny = no
    ;
        OkToDeleteAny = yes
    ),
    code_info.clear_all_registers(OkToDeleteAny, !CI),
    code_info.get_forward_live_vars(!.CI, Liveness),
    rebuild_registers(ArgsInfos, Liveness, OutputArgLocs, !CI),
    code_info.generate_return_live_lvalues(!.CI, OutputArgLocs,
        ReturnInstMap, OkToDeleteAny, ReturnLiveLvalues).

:- pred find_nonlive_outputs(assoc_list(prog_var, arg_info)::in,
    set(prog_var)::in, set(prog_var)::in, set(prog_var)::out) is det.

find_nonlive_outputs([], _, NonLiveOutputs, NonLiveOutputs).
find_nonlive_outputs([Var - arg_info(_ArgLoc, Mode) | Args],
        Liveness, NonLiveOutputs0, NonLiveOutputs) :-
    ( Mode = top_out ->
        ( set.member(Var, Liveness) ->
            NonLiveOutputs1 = NonLiveOutputs0
        ;
            set.insert(NonLiveOutputs0, Var, NonLiveOutputs1)
        )
    ;
        NonLiveOutputs1 = NonLiveOutputs0
    ),
    find_nonlive_outputs(Args, Liveness, NonLiveOutputs1, NonLiveOutputs).

:- pred rebuild_registers(assoc_list(prog_var, arg_info)::in,
    set(prog_var)::in, assoc_list(prog_var, arg_loc)::out,
    code_info::in, code_info::out) is det.

rebuild_registers([], _, [], !CI).
rebuild_registers([Var - arg_info(ArgLoc, Mode) | Args], Liveness,
        OutputArgLocs, !CI) :-
    rebuild_registers(Args, Liveness, OutputArgLocs1, !CI),
    (
        Mode = top_out,
        set.member(Var, Liveness)
    ->
        code_util.arg_loc_to_register(ArgLoc, Register),
        code_info.set_var_location(Var, Register, !CI),
        OutputArgLocs = [Var - ArgLoc | OutputArgLocs1]
    ;
        OutputArgLocs = OutputArgLocs1
    ).

%---------------------------------------------------------------------------%

generate_builtin(CodeModel, PredId, ProcId, Args, Code, !CI) :-
    code_info.get_module_info(!.CI, ModuleInfo),
    ModuleName = predicate_module(ModuleInfo, PredId),
    PredName = predicate_name(ModuleInfo, PredId),
    (
        builtin_ops.translate_builtin(ModuleName, PredName,
            ProcId, Args, SimpleCode0)
    ->
        SimpleCode = SimpleCode0
    ;
        length(Args, Arity),
        format("unknown builtin predicate: %s/%d",
            [s(PredName), i(Arity)], Msg),
        unexpected(this_file, Msg)
    ),
    (
        CodeModel = model_det,
        (
            SimpleCode = assign(Var, AssignExpr),
            generate_assign_builtin(Var, AssignExpr, Code, !CI)
        ;
            SimpleCode = ref_assign(AddrVar, ValueVar),
            produce_variable(AddrVar, AddrVarCode, AddrRval, !CI),
            produce_variable(ValueVar, ValueVarCode, ValueRval, !CI),
            StoreCode = node([assign(mem_ref(AddrRval), ValueRval) - ""]),
            Code = tree_list([AddrVarCode, ValueVarCode, StoreCode])
        ;
            SimpleCode = test(_),
            unexpected(this_file, "malformed model_det builtin predicate")
        ;
            SimpleCode = noop(DefinedVars),
            list.foldl(magically_put_var_in_unused_reg, DefinedVars, !CI),
            Code = node([])
        )
    ;
        CodeModel = model_semi,
        (
            SimpleCode = test(TestExpr),
            generate_simple_test(TestExpr, Rval, ArgCode, !CI),
            code_info.fail_if_rval_is_false(Rval, TestCode, !CI),
            Code = tree(ArgCode, TestCode)
        ;
            SimpleCode = assign(_, _),
            unexpected(this_file, "malformed model_semi builtin predicate")
        ;
            SimpleCode = ref_assign(_, _),
            unexpected(this_file, "malformed model_semi builtin predicate")
        ;
            SimpleCode = noop(_),
            unexpected(this_file, "malformed model_semi builtin predicate")
        )
    ;
        CodeModel = model_non,
        unexpected(this_file, "model_non builtin predicate")
    ).

:- pred generate_assign_builtin(prog_var::in, simple_expr(prog_var)::in,
    code_tree::out, code_info::in, code_info::out) is det.

generate_assign_builtin(Var, AssignExpr, Code, !CI) :-
    ( code_info.variable_is_forward_live(!.CI, Var) ->
        Rval = convert_simple_expr(AssignExpr),
        code_info.assign_expr_to_var(Var, Rval, Code, !CI)
    ;
        Code = empty
    ).

:- func convert_simple_expr(simple_expr(prog_var)) = rval.

convert_simple_expr(leaf(Var)) = var(Var).
convert_simple_expr(int_const(Int)) = const(llconst_int(Int)).
convert_simple_expr(float_const(Float)) = const(llconst_float(Float)).
convert_simple_expr(unary(UnOp, Expr)) =
    unop(UnOp, convert_simple_expr(Expr)).
convert_simple_expr(binary(BinOp, Expr1, Expr2)) =
    binop(BinOp, convert_simple_expr(Expr1), convert_simple_expr(Expr2)).

:- pred generate_simple_test(simple_expr(prog_var)::in(simple_test_expr),
    rval::out, code_tree::out, code_info::in, code_info::out) is det.

generate_simple_test(TestExpr, Rval, ArgCode, !CI) :-
    (
        TestExpr = binary(BinOp, X0, Y0),
        X1 = convert_simple_expr(X0),
        Y1 = convert_simple_expr(Y0),
        generate_builtin_arg(X1, X, CodeX, !CI),
        generate_builtin_arg(Y1, Y, CodeY, !CI),
        Rval = binop(BinOp, X, Y),
        ArgCode = tree(CodeX, CodeY)
    ;
        TestExpr = unary(UnOp, X0),
        X1 = convert_simple_expr(X0),
        generate_builtin_arg(X1, X, ArgCode, !CI),
        Rval = unop(UnOp, X)
    ).

:- pred generate_builtin_arg(rval::in, rval::out, code_tree::out,
    code_info::in, code_info::out) is det.

generate_builtin_arg(Rval0, Rval, Code, !CI) :-
    ( Rval0 = var(Var) ->
        code_info.produce_variable(Var, Code, Rval, !CI)
    ;
        Rval = Rval0,
        Code = empty
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

input_arg_locs([], []).
input_arg_locs([Var - arg_info(Loc, Mode) | Args], Vs) :-
    input_arg_locs(Args, Vs0),
    ( Mode = top_in ->
        Vs = [Var - Loc | Vs0]
    ;
        Vs = Vs0
    ).

output_arg_locs([], []).
output_arg_locs([Var - arg_info(Loc, Mode) | Args], Vs) :-
    output_arg_locs(Args, Vs0),
    ( Mode = top_out ->
        Vs = [Var - Loc | Vs0]
    ;
        Vs = Vs0
    ).

%---------------------------------------------------------------------------%

:- pred generate_call_vn_livevals(list(arg_loc)::in, set(prog_var)::in,
    code_tree::out, code_info::in, code_info::out) is det.

generate_call_vn_livevals(InputArgLocs, OutputArgs, Code, !CI) :-
    code_info.generate_call_vn_livevals(!.CI, InputArgLocs, OutputArgs,
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
:- end_module call_gen.
%---------------------------------------------------------------------------%
