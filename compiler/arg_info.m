%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2000,2002-2007, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: arg_info.m.
% Main author: fjh.
%
% This module is one of the pre-passes of the code generator.
% It initializes the arg_info field of the proc_info structure in the HLDS,
% which records for each argument of each procedure, whether the
% argument is input/output/unused, and which register it is supposed to
% go into.
%
% The code in this module assumes that none of the modes are undefined.
% The predicates for partitioning arguments into input, unused and output
% should not be used before undefined modes have been detected. For an example
% of how this kind of partitioning can be done in the presence of undefined
% modes, see superhomogeneous.partition_args_and_lambda_vars/8.
%
%-----------------------------------------------------------------------------%

:- module hlds.arg_info.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

    % Annotate every procedure in the module with information
    % about its argument passing interface.
    %
:- pred generate_arg_info(module_info::in, module_info::out) is det.

    % Annotate a single procedure with information
    % about its argument passing interface.
    %
:- pred generate_proc_arg_info(pred_markers::in, list(mer_type)::in,
    module_info::in, proc_info::in, proc_info::out) is det.

    % Given the list of types and modes of the arguments of a procedure
    % and its code model, return the standard argument passing interface
    % for procedure. This will pass float arguments via float registers
    % if present, while passing all other arguments via regular registers.
    %
:- pred make_standard_arg_infos(list(mer_type)::in, list(mer_mode)::in,
    code_model::in, module_info::in, list(arg_info)::out) is det.

    % As above, but pass the register type for each argument explicitly.
    % This is necessary for procedures with float arguments that must be
    % passed via the regular registers instead of float registers.
    %
:- pred make_arg_infos(list(mer_type)::in, list(mer_mode)::in,
    list(reg_type)::in, code_model::in, module_info::in,
    list(arg_info)::out) is det.

    % Return the register type to use for each argument of a generic call.
    %
:- pred generic_call_arg_reg_types(module_info::in, vartypes::in,
    generic_call::in, list(prog_var)::in, arg_reg_type_info::in,
    list(reg_type)::out) is det.

    % Divide the given list of arguments into those treated as inputs
    % by the calling convention and those treated as outputs.
    %
:- pred compute_in_and_out_vars(module_info::in, list(prog_var)::in,
    list(mer_mode)::in, list(mer_type)::in,
    list(prog_var)::out, list(prog_var)::out) is det.

    % Divide the given list of arguments into those treated as inputs
    % by the calling convention and those treated as outputs.
    % Arguments are further divided those passed via regular registers
    % or via float registers.
    %
:- pred compute_in_and_out_vars_sep_regs(module_info::in, list(prog_var)::in,
    list(mer_mode)::in, list(mer_type)::in, list(reg_type)::in,
    list(prog_var)::out, list(prog_var)::out, list(prog_var)::out,
    list(prog_var)::out) is det.

    % Divide the given list of arguments and the arg_infos into two
    % lists: those which are treated as inputs by the calling convention
    % and those which are treated as outputs by the calling convention.
    %
:- pred partition_args(assoc_list(prog_var, arg_info)::in,
    assoc_list(prog_var, arg_info)::out,
    assoc_list(prog_var, arg_info)::out) is det.

    % Divide the given list of arguments and the arg_infos into three lists:
    % the inputs, the outputs, and the unused arguments, in that order.
    %
:- pred partition_args(assoc_list(prog_var, arg_info)::in,
    assoc_list(prog_var, arg_info)::out,
    assoc_list(prog_var, arg_info)::out,
    assoc_list(prog_var, arg_info)::out) is det.

    % Partition the head variables of the given procedure into three sets:
    % the inputs, the outputs, and the unused arguments. This is done based
    % on the arg_info annotations of the arguments, which means that this
    % predicate should only be called after the arg_info pass has been run.
    %
:- pred partition_proc_args(proc_info::in, module_info::in,
    set(prog_var)::out, set(prog_var)::out, set(prog_var)::out) is det.

    % Like partition_proc_args, but partitions the actual
    % arguments of a call (given in the fourth argument) instead of the
    % head variables. Since the first (proc_info) argument is now the
    % proc_info of the callee, we need to pass the types of the arguments
    % (in the caller) separately.
    %
:- pred partition_proc_call_args(proc_info::in, vartypes::in,
    module_info::in, list(prog_var)::in, set(prog_var)::out,
    set(prog_var)::out, set(prog_var)::out) is det.

    % Like partition_proc_call_args, but partitions the actual
    % arguments of a generic call, so instead of looking up the types and modes
    % of the arguments in the module_info, we get them from the arguments.
    %
:- pred partition_generic_call_args(module_info::in,
    list(prog_var)::in, list(mer_type)::in, list(mer_mode)::in,
    set(prog_var)::out, set(prog_var)::out, set(prog_var)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module int.

%-----------------------------------------------------------------------------%
%
% This whole section just traverses the module structure
%

generate_arg_info(ModuleInfo0, ModuleInfo) :-
    module_info_get_preds(ModuleInfo0, Preds),
    map.keys(Preds, PredIds),
    generate_pred_arg_info(PredIds, ModuleInfo0, ModuleInfo).

:- pred generate_pred_arg_info(list(pred_id)::in,
    module_info::in, module_info::out) is det.

generate_pred_arg_info([], !ModuleInfo).
generate_pred_arg_info([PredId | PredIds], !ModuleInfo) :-
    module_info_get_preds(!.ModuleInfo, PredTable),
    map.lookup(PredTable, PredId, PredInfo),
    generate_proc_list_arg_info(PredId, pred_info_procids(PredInfo),
        !ModuleInfo),
    generate_pred_arg_info(PredIds, !ModuleInfo).

:- pred generate_proc_list_arg_info(pred_id::in, list(proc_id)::in,
    module_info::in, module_info::out) is det.

generate_proc_list_arg_info(_PredId, [], !ModuleInfo).
generate_proc_list_arg_info(PredId, [ProcId | ProcIds], !ModuleInfo) :-
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    pred_info_get_markers(PredInfo0, Markers),
    pred_info_get_arg_types(PredInfo0, ArgTypes),
    map.lookup(ProcTable0, ProcId, ProcInfo0),

    generate_proc_arg_info(Markers, ArgTypes, !.ModuleInfo,
        ProcInfo0, ProcInfo),

    map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
    pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo),
    generate_proc_list_arg_info(PredId, ProcIds, !ModuleInfo).

generate_proc_arg_info(Markers, ArgTypes, ModuleInfo, !ProcInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, use_float_registers, UseFloatRegs),
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    ( if
        UseFloatRegs = yes,
        % XXX we don't yet use float registers for class method calls
        not check_marker(Markers, marker_class_instance_method)
    then
        proc_info_get_reg_r_headvars(!.ProcInfo, RegR_HeadVars),
        list.map_corresponding(reg_type_for_headvar(RegR_HeadVars),
            HeadVars, ArgTypes, ArgRegTypes)
    else
        list.duplicate(list.length(HeadVars), reg_r, ArgRegTypes)
    ),
    proc_info_get_argmodes(!.ProcInfo, ArgModes),
    CodeModel = proc_info_interface_code_model(!.ProcInfo),
    make_arg_infos(ArgTypes, ArgModes, ArgRegTypes, CodeModel, ModuleInfo,
        ArgInfo),
    proc_info_set_arg_info(ArgInfo, !ProcInfo).

:- pred reg_type_for_headvar(set_of_progvar::in, prog_var::in, mer_type::in,
    reg_type::out) is det.

reg_type_for_headvar(RegR_HeadVars, HeadVar, Type, RegType) :-
    ( if set_of_var.contains(RegR_HeadVars, HeadVar) then
        RegType = reg_r
    else
        ( if Type = float_type then
            RegType = reg_f
        else
            RegType = reg_r
        )
    ).

%---------------------------------------------------------------------------%

make_standard_arg_infos(ArgTypes, ArgModes, CodeModel, ModuleInfo, ArgInfo) :-
    % This is the useful part of the code ;-).
    %
    % This code is one of the places where we make assumptions about the
    % calling convention. This is the only place in the compiler that makes
    % such assumptions, but there are other places scattered around the
    % runtime and the library that also rely on it.
    %
    % We assume all input arguments always go in sequentially numbered
    % registers starting at registers r1 and f1. We also assume that all output
    % arguments go in sequentially numbered registers starting at r1 and f1,
    % except for model_semi procedures, where r1 is reserved for the result and
    % hence the output arguments start at registers r2 and f1.
    %
    % We allocate unused args as if they were regular (non-floating point)
    % outputs. The calling convention requires that we allocate them a
    % register, and the choice should not matter since unused args should be
    % rare. However, we do have to make sure that all the predicates in this
    % module implement this decision consistently. (No code outside this module
    % should know about the outcome of this decision.)
    %
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, use_float_registers, FloatRegs),
    (
        FloatRegs = yes,
        FloatRegType = reg_f
    ;
        FloatRegs = no,
        FloatRegType = reg_r
    ),
    list.map(standard_reg_type_for_type(FloatRegType), ArgTypes, RegTypes),
    make_arg_infos(ArgTypes, ArgModes, RegTypes, CodeModel, ModuleInfo,
        ArgInfo).

:- pred standard_reg_type_for_type(reg_type::in, mer_type::in, reg_type::out)
    is det.

standard_reg_type_for_type(FloatRegType, Type, RegType) :-
    ( if Type = float_type then
        RegType = FloatRegType
    else
        RegType = reg_r
    ).

make_arg_infos(ArgTypes, ArgModes, ArgRegTypes, CodeModel, ModuleInfo,
        ArgInfo) :-
    (
        CodeModel = model_semi,
        FirstOutRegR = 2
    ;
        ( CodeModel = model_det
        ; CodeModel = model_non
        ),
        FirstOutRegR = 1
    ),
    FirstInRegR = 1,
    FirstInRegF = 1,
    FirstOutRegF = 1,
    (
        make_arg_infos(ArgModes, ArgTypes, ArgRegTypes,
            FirstInRegR, FirstInRegF, FirstOutRegR, FirstOutRegF,
            ModuleInfo, ArgInfoPrime)
    ->
        ArgInfo = ArgInfoPrime
    ;
        unexpected($module, $pred, "length mismatch")
    ).

:- pred make_arg_infos(list(mer_mode)::in, list(mer_type)::in,
    list(reg_type)::in, int::in, int::in, int::in, int::in, module_info::in,
    list(arg_info)::out) is semidet.

make_arg_infos([], [], [], _, _, _, _, _, []).
make_arg_infos([Mode | Modes], [Type | Types], [RegType | RegTypes],
        !.InRegR, !.InRegF, !.OutRegR, !.OutRegF, ModuleInfo,
        [ArgInfo | ArgInfos]) :-
    require_det (
        mode_to_top_functor_mode(ModuleInfo, Mode, Type, TopFunctorMode),
        (
            TopFunctorMode = top_in,
            get_arg_loc(RegType, ArgLoc, !InRegR, !InRegF)
        ;
            ( TopFunctorMode = top_out
            ; TopFunctorMode = top_unused
            ),
            get_arg_loc(RegType, ArgLoc, !OutRegR, !OutRegF)
        ),
        ArgInfo = arg_info(ArgLoc, TopFunctorMode)
    ),
    make_arg_infos(Modes, Types, RegTypes, !.InRegR, !.InRegF,
        !.OutRegR, !.OutRegF, ModuleInfo, ArgInfos).

:- pred get_arg_loc(reg_type::in, arg_loc::out, int::in, int::out,
    int::in, int::out) is det.

get_arg_loc(RegType, ArgLoc, !RegR, !RegF) :-
    (
        RegType = reg_r,
        ArgLoc = reg(reg_r, !.RegR),
        !:RegR = !.RegR + 1
    ;
        RegType = reg_f,
        ArgLoc = reg(reg_f, !.RegF),
        !:RegF = !.RegF + 1
    ).

%-----------------------------------------------------------------------------%

generic_call_arg_reg_types(ModuleInfo, _VarTypes, GenericCall, ArgVars,
        MaybeArgRegs, ArgRegTypes) :-
    (
        GenericCall = higher_order(_, _, _, _),
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, use_float_registers, UseFloatRegs),
        (
            UseFloatRegs = no,
            list.duplicate(length(ArgVars), reg_r, ArgRegTypes)
        ;
            UseFloatRegs = yes,
            MaybeArgRegs = arg_reg_types(ArgRegs),
            ArgRegTypes = list.map(arg_reg_to_reg_type, ArgRegs)
        ;
            UseFloatRegs = yes,
            MaybeArgRegs = arg_reg_types_unset,
            % This should have been set by the float register wrapper pass.
            unexpected($module, $pred, "missing ho_arg_regs")
        )
    ;
        % We don't yet use float registers for class method calls.
        ( GenericCall = class_method(_, _, _, _)
        ; GenericCall = event_call(_)
        ; GenericCall = cast(_)
        ),
        list.duplicate(length(ArgVars), reg_r, ArgRegTypes)
    ).

:- func arg_reg_to_reg_type(ho_arg_reg) = reg_type.

arg_reg_to_reg_type(ho_arg_reg_r) = reg_r.
arg_reg_to_reg_type(ho_arg_reg_f) = reg_f.

%-----------------------------------------------------------------------------%

compute_in_and_out_vars(ModuleInfo, Vars, Modes, Types, !:InVars, !:OutVars) :-
    ( if
        compute_in_and_out_vars_2(ModuleInfo, Vars, Modes, Types,
            [], !:InVars, [], !:OutVars)
    then
        true
    else
        unexpected($module, $pred, "length mismatch")
    ).

:- pred compute_in_and_out_vars_2(module_info::in,
    list(prog_var)::in, list(mer_mode)::in, list(mer_type)::in,
    list(prog_var)::in, list(prog_var)::out,
    list(prog_var)::in, list(prog_var)::out) is semidet.

compute_in_and_out_vars_2(_ModuleInfo, [], [], [], !InVars, !OutVars).
compute_in_and_out_vars_2(ModuleInfo, [Var | Vars], [Mode | Modes],
        [Type | Types], !InVars, !OutVars) :-
    compute_in_and_out_vars_2(ModuleInfo, Vars, Modes, Types,
        !InVars, !OutVars),
    require_det (
        mode_to_top_functor_mode(ModuleInfo, Mode, Type, TopFunctorMode),
        (
            TopFunctorMode = top_in,
            !:InVars = [Var | !.InVars]
        ;
            ( TopFunctorMode = top_out
            ; TopFunctorMode = top_unused
            ),
            !:OutVars = [Var | !.OutVars]
        )
    ).

%-----------------------------------------------------------------------------%

compute_in_and_out_vars_sep_regs(ModuleInfo, Vars, Modes, Types, ArgRegTypes,
        !:InVarsR, !:InVarsF, !:OutVarsR, !:OutVarsF) :-
    ( if
        compute_in_and_out_vars_sep_regs_2(ModuleInfo, Vars, Modes, Types,
            ArgRegTypes, !:InVarsR, !:InVarsF, !:OutVarsR, !:OutVarsF)
    then
        true
    else
        unexpected($module, $pred, "length mismatch")
    ).

:- pred compute_in_and_out_vars_sep_regs_2(module_info::in,
    list(prog_var)::in, list(mer_mode)::in, list(mer_type)::in,
    list(reg_type)::in, list(prog_var)::out, list(prog_var)::out,
    list(prog_var)::out, list(prog_var)::out) is semidet.

compute_in_and_out_vars_sep_regs_2(_ModuleInfo,
        [], [], [], [], [], [], [], []).
compute_in_and_out_vars_sep_regs_2(ModuleInfo,
        [Var | Vars], [Mode | Modes], [Type | Types], [RegType | RegTypes],
        !:InVarsR, !:InVarsF, !:OutVarsR, !:OutVarsF) :-
    compute_in_and_out_vars_sep_regs_2(ModuleInfo, Vars, Modes, Types,
        RegTypes, !:InVarsR, !:InVarsF, !:OutVarsR, !:OutVarsF),
    require_det (
        mode_to_top_functor_mode(ModuleInfo, Mode, Type, TopFunctorMode),
        (
            TopFunctorMode = top_in,
            (
                RegType = reg_r,
                !:InVarsR = [Var | !.InVarsR]
            ;
                RegType = reg_f,
                !:InVarsF = [Var | !.InVarsF]
            )
        ;
            ( TopFunctorMode = top_out
            ; TopFunctorMode = top_unused
            ),
            (
                RegType = reg_r,
                !:OutVarsR = [Var | !.OutVarsR]
            ;
                RegType = reg_f,
                !:OutVarsF = [Var | !.OutVarsF]
            )
        )
    ).

%---------------------------------------------------------------------------%

partition_args(Args, Ins, Outs) :-
    partition_args(Args, Ins, Outs0, Unuseds),
    list.append(Outs0, Unuseds, Outs).

partition_args([], [], [], []).
partition_args([Var - ArgInfo | Rest], !:Ins, !:Outs, !:Unuseds) :-
    partition_args(Rest, !:Ins, !:Outs, !:Unuseds),
    ArgInfo = arg_info(_, ArgMode),
    (
        ArgMode = top_in,
        !:Ins = [Var - ArgInfo | !.Ins]
    ;
        ArgMode = top_out,
        !:Outs = [Var - ArgInfo | !.Outs]
    ;
        ArgMode = top_unused,
        !:Unuseds = [Var - ArgInfo | !.Unuseds]
    ).

%---------------------------------------------------------------------------%

partition_proc_args(ProcInfo, ModuleInfo, Inputs, Outputs, Unuseds) :-
    proc_info_get_headvars(ProcInfo, Vars),
    proc_info_get_argmodes(ProcInfo, Modes),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    lookup_var_types(VarTypes, Vars, Types),
    do_partition_proc_args(ModuleInfo, Vars, Types, Modes,
        Inputs, Outputs, Unuseds).

partition_proc_call_args(ProcInfo, VarTypes, ModuleInfo, Vars,
        Inputs, Outputs, Unuseds) :-
    proc_info_get_argmodes(ProcInfo, Modes),
    lookup_var_types(VarTypes, Vars, Types),
    do_partition_proc_args(ModuleInfo, Vars, Types, Modes,
        Inputs, Outputs, Unuseds).

partition_generic_call_args(ModuleInfo, Vars, Types, Modes,
        Inputs, Outputs, Unuseds) :-
    do_partition_proc_args(ModuleInfo, Vars, Types, Modes,
        Inputs, Outputs, Unuseds).

:- pred do_partition_proc_args(module_info::in, list(prog_var)::in,
    list(mer_type)::in, list(mer_mode)::in, set(prog_var)::out,
    set(prog_var)::out, set(prog_var)::out) is det.

do_partition_proc_args(ModuleInfo, Vars, Types, Modes,
        !:Inputs, !:Outputs, !:Unuseds) :-
    ( if
        partition_proc_args_2(Vars, Types, Modes, ModuleInfo,
            set.init, !:Inputs, set.init, !:Outputs,
            set.init, !:Unuseds)
    then
        true
    else
        unexpected($module, $pred, "list length mismatch")
    ).

:- pred partition_proc_args_2(list(prog_var)::in, list(mer_type)::in,
    list(mer_mode)::in, module_info::in,
    set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is semidet.

partition_proc_args_2([], [], [], _ModuleInfo,
        !Inputs, !Outputs, !Unuseds).
partition_proc_args_2([Var | Vars], [Type | Types], [Mode | Modes],
        ModuleInfo, !Inputs, !Outputs, !Unuseds) :-
    require_det (
        mode_to_top_functor_mode(ModuleInfo, Mode, Type, TopFunctorMode),
        (
            TopFunctorMode = top_in,
            set.insert(Var, !Inputs)
        ;
            TopFunctorMode = top_out,
            set.insert(Var, !Outputs)
        ;
            TopFunctorMode = top_unused,
            set.insert(Var, !Unuseds)
        )
    ),
    partition_proc_args_2(Vars, Types, Modes, ModuleInfo,
        !Inputs, !Outputs, !Unuseds).

%----------------------------------------------------------------------------%
:- end_module hlds.arg_info.
%----------------------------------------------------------------------------%
