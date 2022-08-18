%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2000,2002-2007, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
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
%---------------------------------------------------------------------------%

:- module hlds.arg_info.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

    % Annotate every procedure in the module with information
    % about its argument passing interface.
    %
:- pred generate_arg_info(module_info::in, module_info::out) is det.

    % Annotate a single procedure with information
    % about its argument passing interface.
    %
:- pred generate_proc_arg_info(module_info::in, pred_markers::in,
    list(mer_type)::in, proc_info::in, proc_info::out) is det.

    % Given the list of types and modes of the arguments of a procedure
    % and its code model, return the standard argument passing interface
    % for procedure. This will pass float arguments via float registers
    % if present, while passing all other arguments via regular registers.
    %
:- pred make_standard_arg_infos(module_info::in, code_model::in,
    list(mer_type)::in, list(mer_mode)::in, list(arg_info)::out) is det.

    % As above, but pass the register type for each argument explicitly.
    % This is necessary for procedures with float arguments that must be
    % passed via the regular registers instead of float registers.
    %
:- pred make_arg_infos(module_info::in, code_model::in,
    list(mer_type)::in, list(mer_mode)::in, list(reg_type)::in,
    list(arg_info)::out) is det.

    % Return the register type to use for each argument of a generic call.
    %
:- pred generic_call_arg_reg_types(module_info::in, generic_call::in,
    list(prog_var)::in, arg_reg_type_info::in, list(reg_type)::out) is det.

    % Divide the given list of arguments into those treated as inputs
    % by the calling convention and those treated as outputs.
    %
:- pred compute_in_and_out_vars(module_info::in, var_table::in,
    list(prog_var)::in, list(mer_mode)::in,
    list(prog_var)::out, list(prog_var)::out) is det.

    % Divide the given list of arguments into those treated as inputs
    % by the calling convention and those treated as outputs.
    % Arguments are further divided those passed via regular registers
    % or via float registers.
    %
:- pred compute_in_and_out_vars_sep_regs(module_info::in, var_table::in,
    list(prog_var)::in, list(mer_mode)::in, list(reg_type)::in,
    list(prog_var)::out, list(prog_var)::out,
    list(prog_var)::out, list(prog_var)::out) is det.

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
:- pred partition_proc_args(module_info::in, proc_info::in,
    set(prog_var)::out, set(prog_var)::out, set(prog_var)::out) is det.

    % Like partition_proc_args, but partitions the actual
    % arguments of a call (given in the fourth argument) instead of the
    % head variables. Since the first (proc_info) argument is now the
    % proc_info of the callee, we need to pass the types of the arguments
    % (in the caller) separately.
    %
:- pred partition_proc_call_args(module_info::in, proc_info::in,
    var_table::in, list(prog_var)::in,
    set(prog_var)::out, set(prog_var)::out, set(prog_var)::out) is det.

    % Like partition_proc_call_args, but partitions the actual
    % arguments of a generic call, so instead of looking up the types and modes
    % of the arguments in the module_info, we get them from the arguments.
    %
:- pred partition_generic_call_args(module_info::in,
    list(prog_var)::in, list(mer_type)::in, list(mer_mode)::in,
    set(prog_var)::out, set(prog_var)::out, set(prog_var)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_top_functor.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.

%---------------------------------------------------------------------------%
%
% This whole section just traverses the module structure
%

generate_arg_info(!ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, use_float_registers, UseFloatRegs),
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    map.to_sorted_assoc_list(PredIdTable0, PredIdsInfos0),
    generate_arg_infos_for_preds(!.ModuleInfo, UseFloatRegs, PredIdsInfos0,
        [], RevPredIdsInfos),
    map.from_rev_sorted_assoc_list(RevPredIdsInfos, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

    % We use an accumulator because the list of predicates can be quite long.
    %
:- pred generate_arg_infos_for_preds(module_info::in, bool::in,
    assoc_list(pred_id, pred_info)::in,
    assoc_list(pred_id, pred_info)::in,
    assoc_list(pred_id, pred_info)::out) is det.

generate_arg_infos_for_preds(_, _, [], !RevPredIdsInfos).
generate_arg_infos_for_preds(ModuleInfo, UseFloatRegs,
        [PredIdInfo0 | PredIdsInfos0], !RevPredIdsInfos) :-
    PredIdInfo0 = PredId - PredInfo0,
    pred_info_get_markers(PredInfo0, Markers),
    pred_info_get_arg_types(PredInfo0, ArgTypes),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.to_sorted_assoc_list(ProcTable0, ProcIdsInfos0),
    generate_arg_infos_for_procs(ModuleInfo, UseFloatRegs, Markers, ArgTypes,
        ProcIdsInfos0, ProcIdsInfos),
    map.from_sorted_assoc_list(ProcIdsInfos, ProcTable),
    pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
    PredIdInfo = PredId - PredInfo,
    !:RevPredIdsInfos = [PredIdInfo | !.RevPredIdsInfos],
    generate_arg_infos_for_preds(ModuleInfo, UseFloatRegs,
        PredIdsInfos0, !RevPredIdsInfos).

    % We *don't* use an accumulator because in practice,
    % the list of procedures is never too long except in pathological code.
    %
:- pred generate_arg_infos_for_procs(module_info::in, bool::in,
    pred_markers::in, list(mer_type)::in,
    assoc_list(proc_id, proc_info)::in,
    assoc_list(proc_id, proc_info)::out) is det.

generate_arg_infos_for_procs(_, _, _, _, [], []).
generate_arg_infos_for_procs(ModuleInfo, UseFloatRegs, Markers, ArgTypes,
        [ProcIdInfo0 | ProcIdInfos0], [ProcIdInfo | ProcIdInfos]) :-
    ProcIdInfo0 = ProcId - ProcInfo0,
    generate_arg_infos_for_proc(ModuleInfo, UseFloatRegs, Markers, ArgTypes,
        ProcInfo0, ProcInfo),
    ProcIdInfo = ProcId - ProcInfo,
    generate_arg_infos_for_procs(ModuleInfo, UseFloatRegs, Markers, ArgTypes,
        ProcIdInfos0, ProcIdInfos).

generate_proc_arg_info(ModuleInfo, Markers, ArgTypes, !ProcInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, use_float_registers, UseFloatRegs),
    generate_arg_infos_for_proc(ModuleInfo, UseFloatRegs, Markers, ArgTypes,
        !ProcInfo).

:- pred generate_arg_infos_for_proc(module_info::in, bool::in,
    pred_markers::in, list(mer_type)::in,
    proc_info::in, proc_info::out) is det.

generate_arg_infos_for_proc(ModuleInfo, UseFloatRegs, Markers, ArgTypes,
        !ProcInfo) :-
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_argmodes(!.ProcInfo, ArgModes),
    CodeModel = proc_info_interface_code_model(!.ProcInfo),
    ( if
        UseFloatRegs = yes,
        % XXX we don't yet use float registers for class method calls
        not check_marker(Markers, marker_class_instance_method)
    then
        proc_info_get_reg_r_headvars(!.ProcInfo, RegR_HeadVars),
        list.map_corresponding(reg_type_for_headvar(RegR_HeadVars),
            HeadVars, ArgTypes, ArgRegTypes),
        make_arg_infos(ModuleInfo, CodeModel, ArgTypes, ArgModes, ArgRegTypes,
            ArgInfos)
    else
        make_reg_r_arg_infos(ModuleInfo, CodeModel, ArgTypes, ArgModes,
            ArgInfos)
    ),
    proc_info_set_arg_info(ArgInfos, !ProcInfo).

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

make_standard_arg_infos(ModuleInfo, CodeModel, ArgTypes, ArgModes, ArgInfos) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, use_float_registers, FloatRegs),
    (
        FloatRegs = yes,
        make_std_arg_infos(ModuleInfo, CodeModel, ArgTypes, ArgModes,
            ArgInfos)
    ;
        FloatRegs = no,
        make_reg_r_arg_infos(ModuleInfo, CodeModel, ArgTypes, ArgModes,
            ArgInfos)
    ).

%---------------------------------------------------------------------------%
%
% This is the useful part of the code ;-).
%
% This code the only place in the compiler where we make assumptions about
% the calling convention, though there are other places scattered around
% the runtime and the library that also rely on it.
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
% The next three sections of code define
%
% - make_std_arg_infos,
% - make_reg_r_arg_infos, and
% - make_arg_infos.
%
% These all do variations of the same job, so a change in one may require
% corresponding changes in the other two as well.
%
% make_arg_infos is the most general version, in that it allows the caller
% to specify the reg_type for every argument individually. However, this
% also makes it the slowest. The other two are specialized versions that
% do not take a separate reg_type for every argument. make_reg_r_arg_infos
% allocates every argument a reg_r register, while make_std_arg_infos
% allocates every argument whose type is not "float" a reg_r register.
%
%---------------------------------------------------------------------------%

:- pred make_std_arg_infos(module_info::in, code_model::in,
    list(mer_type)::in, list(mer_mode)::in, list(arg_info)::out) is det.

make_std_arg_infos(ModuleInfo, CodeModel, ArgTypes, ArgModes, ArgInfos) :-
    initial_r_regs(CodeModel, FirstInRegR, FirstOutRegR),
    FirstInRegF = 1,
    FirstOutRegF = 1,
    make_std_arg_infos_loop(ModuleInfo, ArgTypes, ArgModes,
        FirstInRegR, FirstInRegF, FirstOutRegR, FirstOutRegF, ArgInfos).

:- pred make_std_arg_infos_loop(module_info::in,
    list(mer_type)::in, list(mer_mode)::in,
    int::in, int::in, int::in, int::in, list(arg_info)::out) is det.

make_std_arg_infos_loop(_, [], [], _, _, _, _, []).
make_std_arg_infos_loop(_, [], [_ | _], _, _, _, _, _) :-
    unexpected($pred, "length mismatch").
make_std_arg_infos_loop(_, [_ | _], [], _, _, _, _, _) :-
    unexpected($pred, "length mismatch").
make_std_arg_infos_loop(ModuleInfo, [Type | Types], [Mode | Modes],
        !.InRegR, !.InRegF, !.OutRegR, !.OutRegF, [ArgInfo | ArgInfos]) :-
    require_det (
        mode_to_top_functor_mode(ModuleInfo, Mode, Type, TopFunctorMode),
        (
            TopFunctorMode = top_in,
            get_std_arg_loc(Type, ArgLoc, !InRegR, !InRegF)
        ;
            ( TopFunctorMode = top_out
            ; TopFunctorMode = top_unused
            ),
            get_std_arg_loc(Type, ArgLoc, !OutRegR, !OutRegF)
        ),
        ArgInfo = arg_info(ArgLoc, TopFunctorMode)
    ),
    make_std_arg_infos_loop(ModuleInfo, Types, Modes,
        !.InRegR, !.InRegF, !.OutRegR, !.OutRegF, ArgInfos).

:- pred get_std_arg_loc(mer_type::in, arg_loc::out, int::in, int::out,
    int::in, int::out) is det.

get_std_arg_loc(Type, ArgLoc, !RegR, !RegF) :-
    ( if Type = float_type then
        ArgLoc = reg(reg_f, !.RegF),
        !:RegF = !.RegF + 1
    else
        ArgLoc = reg(reg_r, !.RegR),
        !:RegR = !.RegR + 1
    ).

%---------------------%

:- pred make_reg_r_arg_infos(module_info::in, code_model::in,
    list(mer_type)::in, list(mer_mode)::in, list(arg_info)::out) is det.

make_reg_r_arg_infos(ModuleInfo, CodeModel, ArgTypes, ArgModes, ArgInfos) :-
    initial_r_regs(CodeModel, FirstInRegR, FirstOutRegR),
    make_reg_r_arg_infos_loop(ModuleInfo, ArgTypes, ArgModes,
        FirstInRegR, FirstOutRegR, ArgInfos).

:- pred make_reg_r_arg_infos_loop(module_info::in,
    list(mer_type)::in, list(mer_mode)::in,
    int::in, int::in, list(arg_info)::out) is det.

make_reg_r_arg_infos_loop(_, [], [], _, _, []).
make_reg_r_arg_infos_loop(_, [], [_ | _], _, _, _) :-
    unexpected($pred, "length mismatch").
make_reg_r_arg_infos_loop(_, [_ | _], [], _, _, _) :-
    unexpected($pred, "length mismatch").
make_reg_r_arg_infos_loop(ModuleInfo, [Type | Types], [Mode | Modes],
        !.InRegR, !.OutRegR, [ArgInfo | ArgInfos]) :-
    mode_to_top_functor_mode(ModuleInfo, Mode, Type, TopFunctorMode),
    (
        TopFunctorMode = top_in,
        get_reg_r_arg_loc(ArgLoc, !InRegR)
    ;
        ( TopFunctorMode = top_out
        ; TopFunctorMode = top_unused
        ),
        get_reg_r_arg_loc(ArgLoc, !OutRegR)
    ),
    ArgInfo = arg_info(ArgLoc, TopFunctorMode),
    make_reg_r_arg_infos_loop(ModuleInfo, Types, Modes,
        !.InRegR, !.OutRegR, ArgInfos).

:- pred get_reg_r_arg_loc(arg_loc::out, int::in, int::out) is det.

get_reg_r_arg_loc(ArgLoc, !RegR) :-
    ArgLoc = reg(reg_r, !.RegR),
    !:RegR = !.RegR + 1.

%---------------------%

make_arg_infos(ModuleInfo, CodeModel, ArgTypes, ArgModes, ArgRegTypes,
        ArgInfos) :-
    initial_r_regs(CodeModel, FirstInRegR, FirstOutRegR),
    FirstInRegF = 1,
    FirstOutRegF = 1,
    ( if
        make_arg_infos_loop(ModuleInfo, ArgTypes, ArgModes, ArgRegTypes,
            FirstInRegR, FirstInRegF, FirstOutRegR, FirstOutRegF,
            ArgInfosPrime)
    then
        ArgInfos = ArgInfosPrime
    else
        unexpected($pred, "length mismatch")
    ).

:- pred make_arg_infos_loop(module_info::in,
    list(mer_type)::in, list(mer_mode)::in, list(reg_type)::in,
    int::in, int::in, int::in, int::in, list(arg_info)::out) is semidet.

make_arg_infos_loop(_, [], [], [], _, _, _, _, []).
make_arg_infos_loop(ModuleInfo,
        [Type | Types], [Mode | Modes], [RegType | RegTypes],
        !.InRegR, !.InRegF, !.OutRegR, !.OutRegF, [ArgInfo | ArgInfos]) :-
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
    make_arg_infos_loop(ModuleInfo, Types, Modes, RegTypes,
        !.InRegR, !.InRegF, !.OutRegR, !.OutRegF, ArgInfos).

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

%---------------------%

:- pred initial_r_regs(code_model::in, int::out, int::out) is det.
:- pragma inline(pred(initial_r_regs/3)).

initial_r_regs(CodeModel, FirstInRegR, FirstOutRegR) :-
    FirstInRegR = 1,
    (
        CodeModel = model_semi,
        FirstOutRegR = 2
    ;
        ( CodeModel = model_det
        ; CodeModel = model_non
        ),
        FirstOutRegR = 1
    ).

%---------------------------------------------------------------------------%

generic_call_arg_reg_types(ModuleInfo, GenericCall, ArgVars,
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
            unexpected($pred, "missing ho_arg_regs")
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

%---------------------------------------------------------------------------%

compute_in_and_out_vars(_, _, [], [], [], []).
compute_in_and_out_vars(_, _, [], [_ | _], _, _) :-
    unexpected($pred, "length mismatch").
compute_in_and_out_vars(_, _, [_ | _], [], _, _) :-
    unexpected($pred, "length mismatch").
compute_in_and_out_vars(ModuleInfo, VarTable,
        [Var | Vars], [Mode | Modes],
        !:InVars, !:OutVars) :-
    compute_in_and_out_vars(ModuleInfo, VarTable, Vars, Modes,
        !:InVars, !:OutVars),
    lookup_var_entry(VarTable, Var, Entry),
    Type = Entry ^ vte_type,
    mode_to_top_functor_mode(ModuleInfo, Mode, Type, TopFunctorMode),
    (
        TopFunctorMode = top_in,
        !:InVars = [Var | !.InVars]
    ;
        ( TopFunctorMode = top_out
        ; TopFunctorMode = top_unused
        ),
        !:OutVars = [Var | !.OutVars]
    ).

%---------------------------------------------------------------------------%

compute_in_and_out_vars_sep_regs(ModuleInfo, VarTable,
        Vars, Modes, ArgRegTypes,
        !:InVarsR, !:InVarsF, !:OutVarsR, !:OutVarsF) :-
    ( if
        compute_in_and_out_vars_sep_regs_2(ModuleInfo, VarTable,
            Vars, Modes, ArgRegTypes,
            !:InVarsR, !:InVarsF, !:OutVarsR, !:OutVarsF)
    then
        true
    else
        unexpected($pred, "length mismatch")
    ).

:- pred compute_in_and_out_vars_sep_regs_2(module_info::in,
    var_table::in, list(prog_var)::in, list(mer_mode)::in, list(reg_type)::in,
    list(prog_var)::out, list(prog_var)::out,
    list(prog_var)::out, list(prog_var)::out) is semidet.

compute_in_and_out_vars_sep_regs_2(_, _, [], [], [], [], [], [], []).
compute_in_and_out_vars_sep_regs_2(ModuleInfo, VarTable,
        [Var | Vars], [Mode | Modes], [RegType | RegTypes],
        !:InVarsR, !:InVarsF, !:OutVarsR, !:OutVarsF) :-
    compute_in_and_out_vars_sep_regs_2(ModuleInfo, VarTable,
        Vars, Modes, RegTypes, !:InVarsR, !:InVarsF, !:OutVarsR, !:OutVarsF),
    require_det (
        lookup_var_entry(VarTable, Var, Entry),
        Type = Entry ^ vte_type,
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
partition_args([Var - ArgInfo | VarsArgInfos], !:Ins, !:Outs, !:Unuseds) :-
    partition_args(VarsArgInfos, !:Ins, !:Outs, !:Unuseds),
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

partition_proc_args(ModuleInfo, ProcInfo, Inputs, Outputs, Unuseds) :-
    proc_info_get_headvars(ProcInfo, Vars),
    proc_info_get_argmodes(ProcInfo, Modes),
    proc_info_get_var_table(ProcInfo, VarTable),
    lookup_var_types(VarTable, Vars, Types),
    do_partition_proc_args(ModuleInfo, Vars, Types, Modes,
        Inputs, Outputs, Unuseds).

partition_proc_call_args(ModuleInfo, ProcInfo, VarTable, Vars,
        Inputs, Outputs, Unuseds) :-
    proc_info_get_argmodes(ProcInfo, Modes),
    lookup_var_types(VarTable, Vars, Types),
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
        partition_proc_args_2(ModuleInfo, Vars, Types, Modes,
            set.init, !:Inputs, set.init, !:Outputs,
            set.init, !:Unuseds)
    then
        true
    else
        unexpected($pred, "list length mismatch")
    ).

:- pred partition_proc_args_2(module_info::in, list(prog_var)::in,
    list(mer_type)::in, list(mer_mode)::in,
    set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is semidet.

partition_proc_args_2(_ModuleInfo, [], [], [],
        !Inputs, !Outputs, !Unuseds).
partition_proc_args_2(ModuleInfo, [Var | Vars], [Type | Types], [Mode | Modes],
        !Inputs, !Outputs, !Unuseds) :-
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
    partition_proc_args_2(ModuleInfo, Vars, Types, Modes,
        !Inputs, !Outputs, !Unuseds).

%---------------------------------------------------------------------------%
:- end_module hlds.arg_info.
%---------------------------------------------------------------------------%
