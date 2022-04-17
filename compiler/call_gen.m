%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
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

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred generate_call(code_model::in, pred_id::in, proc_id::in,
    list(prog_var)::in, hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred generate_generic_call(code_model::in, generic_call::in,
    list(prog_var)::in, list(mer_mode)::in, arg_reg_type_info::in,
    determinism::in, hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- type known_call_variant
    --->    ho_call_known_num
    ;       ho_call_unknown.

    % generic_call_info(Globals, GenericCall, NumImmediateInputArgsR,
    %   NumImmediateInputArgsF, CodeAddr, SpecifierArgInfos,
    %   FirstImmediateInputReg, HoCallVariant).
    %
:- pred generic_call_info(globals::in, generic_call::in, int::in, int::in,
    code_addr::out, assoc_list(prog_var, arg_info)::out, int::out,
    known_call_variant::out) is det.

:- pred generate_builtin(code_model::in, pred_id::in, proc_id::in,
    list(prog_var)::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred input_arg_locs(assoc_list(prog_var, arg_info)::in,
    assoc_list(prog_var, arg_loc)::out) is det.

:- pred output_arg_locs(assoc_list(prog_var, arg_info)::in,
    assoc_list(prog_var, arg_loc)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.arg_info.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module libs.options.
:- import_module ll_backend.code_util.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.trace_gen.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.prog_data_event.
:- import_module parse_tree.prog_event.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_call(CodeModel, PredId, ProcId, ArgVars, GoalInfo, Code, !CI, !CLD) :-
    % Find out which arguments are input and which are output.
    ArgInfo = get_pred_proc_arginfo(!.CI, PredId, ProcId),
    assoc_list.from_corresponding_lists(ArgVars, ArgInfo, ArgsInfos),

    % Save the necessary vars on the stack and move the input args
    % to their registers.
    setup_call(GoalInfo, ArgsInfos, LiveVals, SetupCode, !.CI, !CLD),
    kill_dead_input_vars(ArgsInfos, GoalInfo, NonLiveOutputs, !CLD),

    % Figure out what the call model is.
    prepare_for_call(CodeModel, GoalInfo, CallModel, TraceResetCode,
        !CI, !CLD),

    % Make the call. Note that the construction of CallCode will be moved
    % *after* the code that computes ReturnLiveLvalues.
    get_module_info(!.CI, ModuleInfo),
    Address = make_proc_entry_label(!.CI, ModuleInfo, PredId, ProcId,
        for_immediate_call),
    get_next_label(ReturnLabel, !CI),
    call_gen.call_comment(!.CI, PredId, CodeModel, CallComment),
    Context = goal_info_get_context(GoalInfo),
    GoalId = goal_info_get_goal_id(GoalInfo),
    get_maybe_containing_goal_map(!.CI, MaybeContainingGoalMap),
    (
        MaybeContainingGoalMap = yes(ContainingGoalMap),
        GoalPath = goal_id_to_forward_path(ContainingGoalMap, GoalId),
        MaybeGoalPath = yes(GoalPath)
    ;
        MaybeContainingGoalMap = no,
        MaybeGoalPath = no
    ),
    CallCode = from_list([
        llds_instr(livevals(LiveVals), ""),
        llds_instr(llcall(Address, code_label(ReturnLabel), ReturnLiveLvalues,
            Context, MaybeGoalPath, CallModel), CallComment),
        llds_instr(label(ReturnLabel), "continuation label")
    ]),

    % Figure out what variables will be live at the return point, and where,
    % for use in the accurate garbage collector, and in the debugger.
    get_instmap(!.CLD, InstMap),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstMapDelta, InstMap, ReturnInstMap),

    % Update the code generator state to reflect the situation after the call.
    handle_return(ArgsInfos, GoalInfo, NonLiveOutputs,
        ReturnInstMap, ReturnLiveLvalues, !.CI, !CLD),

    % If the call can fail, generate code to check for and handle the failure.
    remember_position(!.CLD, AfterReturn),
    handle_call_failure(CodeModel, GoalInfo, FailHandlingCode, !CI, !.CLD),
    reset_to_position(AfterReturn, !.CI, !:CLD),

    ( if
        goal_info_has_feature(GoalInfo, feature_debug_self_tail_rec_call),
        get_maybe_trace_info(!.CI, MaybeTraceInfo),
        MaybeTraceInfo = yes(TraceInfo)
    then
        generate_tailrec_event_code(TraceInfo, ArgsInfos, GoalId, Context,
            TraceTailRecResetAndEventCode, TailRecLabel, !CI, !CLD),
        JumpCode = from_list([
            llds_instr(livevals(LiveVals), ""),
            llds_instr(goto(code_label(TailRecLabel)),
                "tail recursive jump")
        ]),
        Code = SetupCode ++ TraceTailRecResetAndEventCode ++ JumpCode
    else
        Code = SetupCode ++ TraceResetCode ++ CallCode ++ FailHandlingCode
    ).

%---------------------------------------------------------------------------%

generate_generic_call(OuterCodeModel, GenericCall, ArgVars, Modes,
        MaybeRegTypes, Det, GoalInfo, Code, !CI, !CLD) :-
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
        generate_main_generic_call(OuterCodeModel, GenericCall, ArgVars, Modes,
            MaybeRegTypes, Det, GoalInfo, Code, !CI, !CLD)
    ;
        GenericCall = event_call(EventName),
        generate_event_call(EventName, ArgVars, GoalInfo, Code, !CI, !CLD)
    ;
        GenericCall = cast(_),
        ( if ArgVars = [InputArgVar, OutputArgVar] then
            get_var_table(!.CI, VarTable),
            lookup_var_entry(VarTable, InputArgVar, InputArgEntry),
            InputArgVarIsDummy = InputArgEntry ^ vte_is_dummy,
            (
                InputArgVarIsDummy = is_dummy_type,
                % Dummy types don't actually have values, which is
                % normally harmless. However, using the constant zero means
                % that we don't need to allocate space for an existentially
                % typed version of a dummy type. Using the constant zero
                % also avoids keeping pointers to memory that could be freed.
                InputArgRval = int_const(0)
            ;
                InputArgVarIsDummy = is_not_dummy_type,
                InputArgRval = leaf(InputArgVar)
            ),
            generate_assign_builtin(OutputArgVar, InputArgRval, Code, !CLD)
        else
            unexpected($pred, "invalid type/inst cast call")
        )
    ).

:- pred generate_main_generic_call(code_model::in, generic_call::in,
    list(prog_var)::in, list(mer_mode)::in, arg_reg_type_info::in,
    determinism::in, hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_main_generic_call(_OuterCodeModel, GenericCall, ArgVars, Modes,
        MaybeRegTypes, Det, GoalInfo, Code, !CI, !CLD) :-
    get_module_info(!.CI, ModuleInfo),
    arg_info.generic_call_arg_reg_types(ModuleInfo, GenericCall,
        ArgVars, MaybeRegTypes, ArgRegTypes),
    get_var_table(!.CI, VarTable),
    arg_info.compute_in_and_out_vars_sep_regs_table(ModuleInfo, VarTable,
        ArgVars, Modes, ArgRegTypes, InVarsR, InVarsF, OutVarsR, OutVarsF),
    module_info_get_globals(ModuleInfo, Globals),
    generic_call_info(Globals, GenericCall, length(InVarsR), length(InVarsF),
        CodeAddr, SpecifierArgInfos, FirstImmInputR, HoCallVariant),
    FirstImmInputF = 1,
    determinism_to_code_model(Det, CodeModel),
    (
        CodeModel = model_semi,
        FirstOutputR = 2
    ;
        ( CodeModel = model_det
        ; CodeModel = model_non
        ),
        FirstOutputR = 1
    ),
    FirstOutputF = 1,

    give_vars_consecutive_arg_infos(InVarsR, reg_r, FirstImmInputR, top_in,
        InVarArgInfosR),
    give_vars_consecutive_arg_infos(InVarsF, reg_f, FirstImmInputF, top_in,
        InVarArgInfosF),
    give_vars_consecutive_arg_infos(OutVarsR, reg_r, FirstOutputR, top_out,
        OutArgsInfosR),
    give_vars_consecutive_arg_infos(OutVarsF, reg_f, FirstOutputF, top_out,
        OutArgsInfosF),
    ArgInfos = list.condense([SpecifierArgInfos,
        InVarArgInfosR, InVarArgInfosF, OutArgsInfosR, OutArgsInfosF]),

    % Save the necessary vars on the stack and move the input args defined
    % by variables to their registers.
    setup_call(GoalInfo, ArgInfos, LiveVals0, SetupCode, !.CI, !CLD),
    kill_dead_input_vars(ArgInfos, GoalInfo, NonLiveOutputs, !CLD),

    % Move the input args not defined by variables to their registers.
    % Setting up these arguments last results in slightly more efficient code,
    % since we can use their registers when placing the variables.
    generic_call_nonvar_setup(GenericCall, HoCallVariant, InVarsR, InVarsF,
        NonVarCode, !CLD),

    extra_livevals(FirstImmInputR, ExtraLiveVals),
    set.insert_list(ExtraLiveVals, LiveVals0, LiveVals),

    prepare_for_call(CodeModel, GoalInfo, CallModel, TraceCode, !CI, !CLD),

    % Make the call.
    get_next_label(ReturnLabel, !CI),
    Context = goal_info_get_context(GoalInfo),
    GoalId = goal_info_get_goal_id(GoalInfo),

    % Figure out what variables will be live at the return point, and where,
    % for use in the accurate garbage collector, and in the debugger.
    get_instmap(!.CLD, InstMap),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstMapDelta, InstMap, ReturnInstMap),

    % Update the code generator state to reflect the situation after the call.
    OutArgsInfos = OutArgsInfosR ++ OutArgsInfosF,
    handle_return(OutArgsInfos, GoalInfo, NonLiveOutputs,
        ReturnInstMap, ReturnLiveLvalues, !.CI, !CLD),

    get_maybe_containing_goal_map(!.CI, MaybeContainingGoalMap),
    (
        MaybeContainingGoalMap = yes(ContainingGoalMap),
        GoalPath = goal_id_to_forward_path(ContainingGoalMap, GoalId),
        MaybeGoalPath = yes(GoalPath)
    ;
        MaybeContainingGoalMap = no,
        MaybeGoalPath = no
    ),
    CallCode = from_list([
        llds_instr(livevals(LiveVals), ""),
        llds_instr(llcall(CodeAddr, code_label(ReturnLabel), ReturnLiveLvalues,
            Context, MaybeGoalPath, CallModel), "Setup and call"),
        llds_instr(label(ReturnLabel), "Continuation label")
    ]),

    % If the call can fail, generate code to check for and handle the failure.
    remember_position(!.CLD, AfterReturn),
    handle_call_failure(CodeModel, GoalInfo, FailHandlingCode, !CI, !.CLD),
    reset_to_position(AfterReturn, !.CI, !:CLD),

    Code = SetupCode ++ NonVarCode ++ TraceCode ++ CallCode ++
        FailHandlingCode.

%---------------------------------------------------------------------------%

:- pred generate_event_call(string::in, list(prog_var)::in, hlds_goal_info::in,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_event_call(EventName, Args, GoalInfo, Code, !CI, !CLD) :-
    get_module_info(!.CI, ModuleInfo),
    module_info_get_event_set(ModuleInfo, EventSet),
    EventSpecMap = EventSet ^ event_set_spec_map,
    ( if
        event_attributes(EventSpecMap, EventName, Attributes),
        event_number(EventSpecMap, EventName, EventNumber)
    then
        generate_event_attributes(Attributes, Args, MaybeUserAttributes,
            AttrCodes, !.CI, !CLD),
        UserEventInfo = user_event_info(EventNumber, MaybeUserAttributes),
        generate_user_event_code(UserEventInfo, GoalInfo, EventCode,
            !CI, !CLD),
        Code = cord_list_to_cord(AttrCodes) ++ EventCode
    else
        unexpected($pred, "bad event name")
    ).

:- pred generate_event_attributes(list(event_attribute)::in,
    list(prog_var)::in, list(maybe(user_attribute))::out, list(llds_code)::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_event_attributes([], !.Vars, [], [], _CI, !CLD) :-
    (
        !.Vars = [_ | _],
        unexpected($pred, "var")
    ;
        !.Vars = []
    ).
generate_event_attributes([Attribute | Attributes], !.Vars,
        [MaybeUserAttr | MaybeUserAttrs], [Code | Codes], CI, !CLD) :-
    SynthCall = Attribute ^ attr_maybe_synth_call,
    (
        SynthCall = no,
        (
            !.Vars = [Var | !:Vars],
            produce_variable(Var, Code, Rval, !CLD),
            UserAttr = user_attribute(Rval, Var),
            MaybeUserAttr = yes(UserAttr)
        ;
            !.Vars = [],
            unexpected($pred, "no var")
        )
    ;
        SynthCall = yes(_),
        MaybeUserAttr = no,
        Code = empty
    ),
    generate_event_attributes(Attributes, !.Vars, MaybeUserAttrs, Codes,
        CI, !CLD).

%---------------------------------------------------------------------------%

    % The registers before the first reg_r input argument are all live.
    %
:- pred extra_livevals(int::in, list(lval)::out) is det.

extra_livevals(FirstInput, ExtraLiveVals) :-
    extra_livevals_from(1, FirstInput, ExtraLiveVals).

:- pred extra_livevals_from(int::in, int::in, list(lval)::out) is det.

extra_livevals_from(Reg, FirstInput, ExtraLiveVals) :-
    ( if Reg < FirstInput then
        ExtraLiveVals = [reg(reg_r, Reg) | ExtraLiveVals1],
        NextReg = Reg + 1,
        extra_livevals_from(NextReg, FirstInput, ExtraLiveVals1)
    else
        ExtraLiveVals = []
    ).

generic_call_info(Globals, GenericCall, NumInputArgsR, NumInputArgsF,
        CodeAddr, SpecifierArgInfos, FirstImmediateInputReg, HoCallVariant) :-
    (
        GenericCall = higher_order(PredVar, _, _, _),
        Reg = reg(reg_r, 1),
        SpecifierArgInfos = [PredVar - arg_info(Reg, top_in)],
        globals.lookup_int_option(Globals,
            max_specialized_do_call_closure, MaxSpec),
        ( if
            MaxSpec >= 0,
            NumInputArgsR =< MaxSpec,
            NumInputArgsF = 0
        then
            CodeAddr = do_call_closure(specialized_known(NumInputArgsR)),
            HoCallVariant = ho_call_known_num,
            FirstImmediateInputReg = 2
        else
            CodeAddr = do_call_closure(generic),
            HoCallVariant = ho_call_unknown,
            FirstImmediateInputReg = 3
        )
    ;
        GenericCall = class_method(TCVar, _, _, _),
        Reg = reg(reg_r, 1),
        SpecifierArgInfos = [TCVar - arg_info(Reg, top_in)],
        % XXX we do not use float registers for method calls yet
        ( if NumInputArgsF = 0 then
            globals.lookup_int_option(Globals,
                max_specialized_do_call_class_method, MaxSpec),
            ( if
                MaxSpec >= 0,
                NumInputArgsR =< MaxSpec
            then
                CodeAddr = do_call_class_method(
                    specialized_known(NumInputArgsR)),
                HoCallVariant = ho_call_known_num,
                FirstImmediateInputReg = 3
            else
                CodeAddr = do_call_class_method(generic),
                HoCallVariant = ho_call_unknown,
                FirstImmediateInputReg = 4
            )
        else
            sorry($pred, "float reg inputs")
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
    list(prog_var)::in, list(prog_var)::in,
    llds_code::out, code_loc_dep::in, code_loc_dep::out) is det.

generic_call_nonvar_setup(higher_order(_, _, _, _), HoCallVariant,
        InVarsR, InVarsF, Code, !CLD) :-
    (
        HoCallVariant = ho_call_known_num,
        Code = empty
    ;
        HoCallVariant = ho_call_unknown,
        clobber_reg(reg(reg_r, 2), !CLD),
        list.length(InVarsR, NumInVarsR),
        list.length(InVarsF, NumInVarsF),
        NumInVars = encode_num_generic_call_vars(NumInVarsR, NumInVarsF),
        Code = singleton(
            llds_instr(assign(reg(reg_r, 2), const(llconst_int(NumInVars))),
                "Assign number of immediate input arguments")
        )
    ).
generic_call_nonvar_setup(class_method(_, Method, _, _), HoCallVariant,
        InVarsR, InVarsF, Code, !CLD) :-
    (
        InVarsF = []
    ;
        InVarsF = [_ | _],
        sorry($pred, "float input reg")
    ),
    (
        HoCallVariant = ho_call_known_num,
        clobber_reg(reg(reg_r, 2), !CLD),
        Code = singleton(
            llds_instr(assign(reg(reg_r, 2), const(llconst_int(Method))),
                "Index of class method in typeclass info")
        )
    ;
        HoCallVariant = ho_call_unknown,
        clobber_regs([reg(reg_r, 2), reg(reg_r, 3)], !CLD),
        list.length(InVarsR, NumInVarsR),
        % Currently we do not use float registers for method calls.
        NumInVarsF = 0,
        NumInVars = encode_num_generic_call_vars(NumInVarsR, NumInVarsF),
        Code = from_list([
            llds_instr(assign(reg(reg_r, 2), const(llconst_int(Method))),
                "Index of class method in typeclass info"),
            llds_instr(assign(reg(reg_r, 3), const(llconst_int(NumInVars))),
                "Assign number of immediate regular input arguments")
        ])
    ).
generic_call_nonvar_setup(event_call(_), _, _, _, _, !CLD) :-
    unexpected($pred, "event_call").
generic_call_nonvar_setup(cast(_), _, _, _, _, !CLD) :-
    unexpected($pred, "cast").

%---------------------------------------------------------------------------%

:- pred prepare_for_call(code_model::in, hlds_goal_info::in, call_model::out,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

prepare_for_call(CodeModel, GoalInfo, CallModel, TraceCode, !CI, !CLD) :-
    succip_is_used(!CI),
    ( if goal_info_has_feature(GoalInfo, feature_do_not_tailcall) then
        AllowLCO = do_not_allow_lco
    else
        AllowLCO = allow_lco
    ),
    (
        CodeModel = model_det,
        CallModel = call_model_det(AllowLCO)
    ;
        CodeModel = model_semi,
        CallModel = call_model_semidet(AllowLCO)
    ;
        CodeModel = model_non,
        may_use_nondet_tailcall(!.CLD, TailCallStatus),
        CallModel = call_model_nondet(TailCallStatus),
        set_resume_point_and_frame_to_unknown(!CLD)
    ),
    trace_prepare_for_call(!.CI, TraceCode).

:- pred handle_call_failure(code_model::in, hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

handle_call_failure(CodeModel, GoalInfo, FailHandlingCode, !CI, !.CLD) :-
    (
        CodeModel = model_semi,
        Detism = goal_info_get_determinism(GoalInfo),
        ( if Detism = detism_failure then
            generate_failure(FailHandlingCode, !CI, !.CLD)
        else
            get_next_label(ContLab, !CI),
            FailTestCode = singleton(
                llds_instr(if_val(lval(reg(reg_r, 1)), code_label(ContLab)),
                    "test for success")
            ),
            generate_failure(FailCode, !CI, !.CLD),
            ContLabelCode = singleton(
                llds_instr(label(ContLab), "")
            ),
            FailHandlingCode = FailTestCode ++ FailCode ++ ContLabelCode
        )
    ;
        ( CodeModel = model_det
        ; CodeModel = model_non
        ),
        FailHandlingCode = empty
    ).

:- pred call_comment(code_info::in, pred_id::in, code_model::in, string::out)
    is det.

call_comment(CI, PredId, CodeModel, Msg) :-
    (
        CodeModel = model_det,
        BaseMsg = "branch to det procedure"
    ;
        CodeModel = model_semi,
        BaseMsg = "branch to semidet procedure"
    ;
        CodeModel = model_non,
        BaseMsg = "branch to nondet procedure"
    ),
    get_auto_comments(CI, AutoComments),
    (
        AutoComments = yes,
        get_module_info(CI, ModuleInfo),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        PredName = pred_info_name(PredInfo),
        Msg = BaseMsg ++ " " ++ PredName
    ;
        AutoComments = no,
        Msg = BaseMsg
    ).

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
    hlds_goal_info::in, set_of_progvar::out,
    code_loc_dep::in, code_loc_dep::out) is det.

kill_dead_input_vars(ArgsInfos, GoalInfo, NonLiveOutputs, !CLD) :-
    get_forward_live_vars(!.CLD, Liveness),
    find_nonlive_outputs(ArgsInfos, Liveness, set_of_var.init, NonLiveOutputs),
    goal_info_get_post_deaths(GoalInfo, PostDeaths),
    set_of_var.difference(PostDeaths, NonLiveOutputs, ImmediatePostDeaths),
    make_vars_forward_dead(ImmediatePostDeaths, !CLD).

:- pred handle_return(assoc_list(prog_var, arg_info)::in,
    hlds_goal_info::in, set_of_progvar::in, instmap::in, list(liveinfo)::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

handle_return(ArgsInfos, GoalInfo, _NonLiveOutputs, ReturnInstMap,
        ReturnLiveLvalues, CI, !CLD) :-
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    ( if instmap_delta_is_reachable(InstMapDelta) then
        OkToDeleteAny = no
    else
        OkToDeleteAny = yes
    ),
    clear_all_registers(OkToDeleteAny, !CLD),
    get_forward_live_vars(!.CLD, Liveness),
    rebuild_registers(ArgsInfos, Liveness, OutputArgLocs, !CLD),
    generate_return_live_lvalues(CI, !.CLD, OutputArgLocs,
        ReturnInstMap, OkToDeleteAny, ReturnLiveLvalues).

:- pred find_nonlive_outputs(assoc_list(prog_var, arg_info)::in,
    set_of_progvar::in, set_of_progvar::in, set_of_progvar::out) is det.

find_nonlive_outputs([], _, !NonLiveOutputs).
find_nonlive_outputs([Var - arg_info(_ArgLoc, Mode) | Args],
        Liveness, !NonLiveOutputs) :-
    (
        Mode = top_out,
        ( if set_of_var.member(Liveness, Var) then
            true
        else
            set_of_var.insert(Var, !NonLiveOutputs)
        )
    ;
        ( Mode = top_in
        ; Mode = top_unused
        )
    ),
    find_nonlive_outputs(Args, Liveness, !NonLiveOutputs).

:- pred rebuild_registers(assoc_list(prog_var, arg_info)::in,
    set_of_progvar::in, assoc_list(prog_var, arg_loc)::out,
    code_loc_dep::in, code_loc_dep::out) is det.

rebuild_registers([], _, [], !CLD).
rebuild_registers([Var - arg_info(ArgLoc, Mode) | Args], Liveness,
        OutputArgLocs, !CLD) :-
    rebuild_registers(Args, Liveness, OutputArgLocs1, !CLD),
    ( if
        Mode = top_out,
        set_of_var.member(Liveness, Var)
    then
        code_util.arg_loc_to_register(ArgLoc, Register),
        set_var_location(Var, Register, !CLD),
        OutputArgLocs = [Var - ArgLoc | OutputArgLocs1]
    else
        OutputArgLocs = OutputArgLocs1
    ).

%---------------------------------------------------------------------------%

generate_builtin(CodeModel, PredId, ProcId, Args, Code, !CI, !CLD) :-
    get_module_info(!.CI, ModuleInfo),
    ModuleName = predicate_module(ModuleInfo, PredId),
    PredName = predicate_name(ModuleInfo, PredId),
    builtin_ops.translate_builtin(ModuleName, PredName, ProcId, Args,
        SimpleCode),
    (
        CodeModel = model_det,
        (
            SimpleCode = assign(Var, AssignExpr),
            generate_assign_builtin(Var, AssignExpr, Code, !CLD)
        ;
            SimpleCode = ref_assign(AddrVar, ValueVar),
            produce_variable(AddrVar, AddrVarCode, AddrRval, !CLD),
            produce_variable(ValueVar, ValueVarCode, ValueRval, !CLD),
            StoreInstr = llds_instr(assign(mem_ref(AddrRval), ValueRval), ""),
            StoreCode = singleton(StoreInstr),
            Code = AddrVarCode ++ ValueVarCode ++ StoreCode
        ;
            SimpleCode = test(_),
            unexpected($pred, "malformed model_det builtin predicate")
        ;
            SimpleCode = noop(DefinedVars),
            list.foldl(magically_put_var_in_unused_reg, DefinedVars, !CLD),
            Code = empty
        )
    ;
        CodeModel = model_semi,
        (
            SimpleCode = test(TestExpr),
            generate_simple_test(TestExpr, Rval, ArgCode, !CLD),
            fail_if_rval_is_false(Rval, TestCode, !CI, !CLD),
            Code = ArgCode ++ TestCode
        ;
            SimpleCode = assign(_, _),
            unexpected($pred, "malformed model_semi builtin predicate")
        ;
            SimpleCode = ref_assign(_, _),
            unexpected($pred, "malformed model_semi builtin predicate")
        ;
            SimpleCode = noop(_),
            unexpected($pred, "malformed model_semi builtin predicate")
        )
    ;
        CodeModel = model_non,
        unexpected($pred, "model_non builtin predicate")
    ).

:- pred generate_assign_builtin(prog_var::in, simple_expr(prog_var)::in,
    llds_code::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_assign_builtin(Var, AssignExpr, Code, !CLD) :-
    ( if variable_is_forward_live(!.CLD, Var) then
        Rval = convert_simple_expr(AssignExpr),
        assign_expr_to_var(Var, Rval, Code, !CLD)
    else
        Code = empty
    ).

:- func convert_simple_expr(simple_expr(prog_var)) = rval.

convert_simple_expr(leaf(Var)) = var(Var).
convert_simple_expr(int_const(Int)) = const(llconst_int(Int)).
convert_simple_expr(uint_const(UInt)) = const(llconst_uint(UInt)).
convert_simple_expr(int8_const(Int8)) = const(llconst_int8(Int8)).
convert_simple_expr(uint8_const(UInt8)) = const(llconst_uint8(UInt8)).
convert_simple_expr(int16_const(Int16)) = const(llconst_int16(Int16)).
convert_simple_expr(uint16_const(UInt16)) = const(llconst_uint16(UInt16)).
convert_simple_expr(int32_const(Int32)) = const(llconst_int32(Int32)).
convert_simple_expr(uint32_const(UInt32)) = const(llconst_uint32(UInt32)).
convert_simple_expr(int64_const(Int64)) = const(llconst_int64(Int64)).
convert_simple_expr(uint64_const(UInt64)) = const(llconst_uint64(UInt64)).
convert_simple_expr(float_const(Float)) = const(llconst_float(Float)).
convert_simple_expr(unary(UnOp, Expr)) =
    unop(UnOp, convert_simple_expr(Expr)).
convert_simple_expr(binary(BinOp, Expr1, Expr2)) =
    binop(BinOp, convert_simple_expr(Expr1), convert_simple_expr(Expr2)).

:- pred generate_simple_test(simple_expr(prog_var)::in(simple_test_expr),
    rval::out, llds_code::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_simple_test(TestExpr, Rval, ArgCode, !CLD) :-
    (
        TestExpr = binary(BinOp, X0, Y0),
        X1 = convert_simple_expr(X0),
        Y1 = convert_simple_expr(Y0),
        generate_builtin_arg(X1, X, CodeX, !CLD),
        generate_builtin_arg(Y1, Y, CodeY, !CLD),
        Rval = binop(BinOp, X, Y),
        ArgCode = CodeX ++ CodeY
    ;
        TestExpr = unary(UnOp, X0),
        X1 = convert_simple_expr(X0),
        generate_builtin_arg(X1, X, ArgCode, !CLD),
        Rval = unop(UnOp, X)
    ).

:- pred generate_builtin_arg(rval::in, rval::out, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

generate_builtin_arg(Rval0, Rval, Code, !CLD) :-
    (
        Rval0 = var(Var),
        produce_variable(Var, Code, Rval, !CLD)
    ;
        ( Rval0 = const(_)
        ; Rval0 = cast(_, _)
        ; Rval0 = unop(_, _)
        ; Rval0 = binop(_, _, _)
        ; Rval0 = mkword(_, _)
        ; Rval0 = mkword_hole(_)
        ; Rval0 = mem_addr(_)
        ; Rval0 = lval(_)
        ),
        Rval = Rval0,
        Code = empty
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

input_arg_locs([], []).
input_arg_locs([Var - arg_info(Loc, Mode) | Args], Vs) :-
    input_arg_locs(Args, Vs0),
    (
        Mode = top_in,
        Vs = [Var - Loc | Vs0]
    ;
        ( Mode = top_out
        ; Mode = top_unused
        ),
        Vs = Vs0
    ).

output_arg_locs([], []).
output_arg_locs([Var - arg_info(Loc, Mode) | Args], Vs) :-
    output_arg_locs(Args, Vs0),
    (
        Mode = top_out,
        Vs = [Var - Loc | Vs0]
    ;
        ( Mode = top_in
        ; Mode = top_unused
        ),
        Vs = Vs0
    ).

%---------------------------------------------------------------------------%

:- pred give_vars_consecutive_arg_infos(list(prog_var)::in,
    reg_type::in, int::in, top_functor_mode::in,
    assoc_list(prog_var, arg_info)::out) is det.

give_vars_consecutive_arg_infos([], _RegType, _N, _TopFunctorMode, []).
give_vars_consecutive_arg_infos([Var | Vars], RegType, N, TopFunctorMode,
        [Var - ArgInfo | ArgInfos]) :-
    ArgInfo = arg_info(reg(RegType, N), TopFunctorMode),
    give_vars_consecutive_arg_infos(Vars, RegType, N + 1, TopFunctorMode,
        ArgInfos).

%---------------------------------------------------------------------------%
:- end_module ll_backend.call_gen.
%---------------------------------------------------------------------------%
