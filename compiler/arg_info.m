%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2000,2002-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: arg_info.m
% Main author: fjh.

% This module is one of the pre-passes of the code generator.
% It initializes the arg_info field of the proc_info structure in the HLDS,
% which records for each argument of each procedure, whether the
% argument is input/output/unused, and which register it is supposed to
% go into.

% The code in this module assumes that none of the modes are undefined.  The
% predicates for partitioning arguments into input, unused and output should
% not be used before undefined modes have been detected.  For an example of
% how this kind of partitioning can be done in the presence of undefined modes,
% see superhomogeneous.partition_args_and_lambda_vars/8.

%-----------------------------------------------------------------------------%

:- module hlds__arg_info.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

    % Annotate every non-aditi procedure in the module with information
    % about its argument passing interface.
    %
:- pred generate_arg_info(module_info::in, module_info::out) is det.

    % Annotate a single procedure with information
    % about its argument passing interface.
    %
:- pred generate_proc_arg_info(list(mer_type)::in, module_info::in,
    proc_info::in, proc_info::out) is det.

    % Given the list of types and modes of the arguments of a procedure
    % and its code model, return its argument passing interface.
    %
:- pred make_arg_infos(list(mer_type)::in, list(mer_mode)::in, code_model::in,
    module_info::in, list(arg_info)::out) is det.

    % Divide the given list of arguments into those treated as inputs
    % by the calling convention and those treated as outputs.
    %
:- pred compute_in_and_out_vars(module_info::in,
    list(prog_var)::in, list(mer_mode)::in, list(mer_type)::in,
    list(prog_var)::out, list(prog_var)::out) is det.

    % Return the arg_infos for the two input arguments of a unification
    % of the specified code model.
    %
:- pred unify_arg_info(code_model::in, list(arg_info)::out) is det.

    % Divide the given list of arguments and the arg_infos into three lists:
    % the inputs, the outputs, and the unused arguments, in that order.
    %
:- pred partition_args(assoc_list(prog_var, arg_info)::in,
    assoc_list(prog_var, arg_info)::out,
    assoc_list(prog_var, arg_info)::out,
    assoc_list(prog_var, arg_info)::out) is det.

    % Divide the given list of arguments and the arg_infos into two
    % lists: those which are treated as inputs by the calling convention
    % and those which are treated as outputs by the calling convention.
    %
:- pred partition_args(assoc_list(prog_var, arg_info)::in,
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

:- import_module check_hlds.mode_util.
:- import_module libs.compiler_util.

:- import_module std_util.
:- import_module map.
:- import_module int.
:- import_module require.
:- import_module svset.

%-----------------------------------------------------------------------------%
%
% This whole section just traverses the module structure.

generate_arg_info(ModuleInfo0, ModuleInfo) :-
    module_info_preds(ModuleInfo0, Preds),
    map__keys(Preds, PredIds),
    generate_pred_arg_info(PredIds, ModuleInfo0, ModuleInfo).

:- pred generate_pred_arg_info(list(pred_id)::in,
    module_info::in, module_info::out) is det.

generate_pred_arg_info([], !ModuleInfo).
generate_pred_arg_info([PredId | PredIds], !ModuleInfo) :-
    module_info_preds(!.ModuleInfo, PredTable),
    map__lookup(PredTable, PredId, PredInfo),
    generate_proc_list_arg_info(PredId, pred_info_procids(PredInfo),
        !ModuleInfo),
    generate_pred_arg_info(PredIds, !ModuleInfo).

:- pred generate_proc_list_arg_info(pred_id::in, list(proc_id)::in,
    module_info::in, module_info::out) is det.

generate_proc_list_arg_info(_PredId, [], !ModuleInfo).
generate_proc_list_arg_info(PredId, [ProcId | ProcIds], !ModuleInfo) :-
    module_info_preds(!.ModuleInfo, PredTable0),
    map__lookup(PredTable0, PredId, PredInfo0),
    ( hlds_pred__pred_info_is_aditi_relation(PredInfo0) ->
        true
    ;
        pred_info_procedures(PredInfo0, ProcTable0),
        pred_info_arg_types(PredInfo0, ArgTypes),
        map__lookup(ProcTable0, ProcId, ProcInfo0),

        generate_proc_arg_info(ArgTypes, !.ModuleInfo, ProcInfo0, ProcInfo),

        map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
        pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
        map__det_update(PredTable0, PredId, PredInfo, PredTable),
        module_info_set_preds(PredTable, !ModuleInfo)
    ),
    generate_proc_list_arg_info(PredId, ProcIds, !ModuleInfo).

generate_proc_arg_info(ArgTypes, ModuleInfo, !ProcInfo) :-
    proc_info_argmodes(!.ProcInfo, ArgModes),
    proc_info_interface_code_model(!.ProcInfo, CodeModel),
    make_arg_infos(ArgTypes, ArgModes, CodeModel, ModuleInfo, ArgInfo),
    proc_info_set_arg_info(ArgInfo, !ProcInfo).

%---------------------------------------------------------------------------%

    % This is the useful part of the code ;-).
    %
    % This code is one of the places where we make assumptions about the
    % calling convention.  This is the only place in the compiler that makes
    % such assumptions, but there are other places scattered around the
    % runtime and the library that also rely on it.
    %
    % We assume all input arguments always go in sequentially numbered
    % registers starting at register number 1. We also assume that all output
    % arguments go in sequentially numbered registers starting at register
    % number 1, except for model_semi procedures, where the first register is
    % reserved for the result and hence the output arguments start at register
    % number 2.
    %
    % We allocate unused args as if they were outputs. The calling convention
    % requires that we allocate them a register, and the choice should not
    % matter since unused args should be rare. However, we do have to make
    % sure that all the predicates in this module implement this decision
    % consistently. (No code outside this module should know about the outcome
    % of this decision.)
    %
make_arg_infos(ArgTypes, ArgModes, CodeModel, ModuleInfo, ArgInfo) :-
    ( CodeModel = model_semi ->
        StartReg = 2
    ;
        StartReg = 1
    ),
    make_arg_infos_list(ArgModes, ArgTypes, 1, StartReg, ModuleInfo, ArgInfo).

:- pred make_arg_infos_list(list(mer_mode)::in, list(mer_type)::in,
    int::in, int::in, module_info::in, list(arg_info)::out) is det.

make_arg_infos_list([], [], _, _, _, []).
make_arg_infos_list([Mode | Modes], [Type | Types], !.InReg, !.OutReg,
        ModuleInfo, [ArgInfo | ArgInfos]) :-
    mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode),
    ( ArgMode = top_in ->
        ArgReg = !.InReg,
        !:InReg = !.InReg + 1
    ;
        ArgReg = !.OutReg,
        !:OutReg = !.OutReg + 1
    ),
    ArgInfo = arg_info(ArgReg, ArgMode),
    make_arg_infos_list(Modes, Types, !.InReg, !.OutReg, ModuleInfo, ArgInfos).
make_arg_infos_list([], [_|_], _, _, _, _) :-
    unexpected(this_file, "make_arg_infos_list: length mismatch").
make_arg_infos_list([_|_], [], _, _, _, _) :-
    unexpected(this_file, "make_arg_infos_list: length mismatch").

%---------------------------------------------------------------------------%

compute_in_and_out_vars(ModuleInfo, Vars, Modes, Types,
        !:InVars, !:OutVars) :-
    (
        compute_in_and_out_vars_2(ModuleInfo, Vars, Modes, Types,
            !:InVars, !:OutVars)
    ->
        true
    ;
        unexpected(this_file, "compute_in_and_out_vars: length mismatch")
    ).

:- pred compute_in_and_out_vars_2(module_info::in,
    list(prog_var)::in, list(mer_mode)::in, list(mer_type)::in,
    list(prog_var)::out, list(prog_var)::out) is semidet.

compute_in_and_out_vars_2(_ModuleInfo, [], [], [], [], []).
compute_in_and_out_vars_2(ModuleInfo, [Var | Vars],
        [Mode | Modes], [Type | Types], !:InVars, !:OutVars) :-
    compute_in_and_out_vars_2(ModuleInfo, Vars,
        Modes, Types, !:InVars, !:OutVars),
    mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode),
    ( ArgMode = top_in ->
        !:InVars = [Var | !.InVars]
    ;
        !:OutVars = [Var | !.OutVars]
    ).

%---------------------------------------------------------------------------%

unify_arg_info(model_det,
    [arg_info(1, top_in), arg_info(2, top_in)]).
unify_arg_info(model_semi,
    [arg_info(1, top_in), arg_info(2, top_in)]).
unify_arg_info(model_non, _) :-
    unexpected(this_file, "unify_arg_info: nondet unify!").

%---------------------------------------------------------------------------%

partition_args(Args, Ins, Outs) :-
    partition_args(Args, Ins, Outs0, Unuseds),
    list__append(Outs0, Unuseds, Outs).

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

:- pred input_args(list(arg_info)::in, list(arg_loc)::out) is det.

input_args([], []).
input_args([arg_info(Loc, Mode) | Args], !:Locs) :-
    input_args(Args, !:Locs),
    ( Mode = top_in ->
        !:Locs = [Loc | !.Locs]
    ;
        true
    ).

%---------------------------------------------------------------------------%

partition_proc_args(ProcInfo, ModuleInfo, Inputs, Outputs, Unuseds) :-
    proc_info_headvars(ProcInfo, Vars),
    proc_info_argmodes(ProcInfo, Modes),
    proc_info_vartypes(ProcInfo, VarTypes),
    map__apply_to_list(Vars, VarTypes, Types),
    do_partition_proc_args(ModuleInfo, Vars, Types, Modes,
        Inputs, Outputs, Unuseds).

partition_proc_call_args(ProcInfo, VarTypes, ModuleInfo, Vars,
        Inputs, Outputs, Unuseds) :-
    proc_info_argmodes(ProcInfo, Modes),
    map__apply_to_list(Vars, VarTypes, Types),
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
    (
        partition_proc_args_2(Vars, Types, Modes, ModuleInfo,
            set__init, !:Inputs, set__init, !:Outputs,
            set__init, !:Unuseds)
    ->
        true
    ;
        unexpected(this_file, "do_partition_proc_args: list length mismatch")
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
    mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode),
    (
        ArgMode = top_in,
        svset__insert(Var, !Inputs)
    ;
        ArgMode = top_out,
        svset__insert(Var, !Outputs)
    ;
        ArgMode = top_unused,
        svset__insert(Var, !Unuseds)
    ),
    partition_proc_args_2(Vars, Types, Modes, ModuleInfo,
        !Inputs, !Outputs, !Unuseds).

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "arg_info.m".

%----------------------------------------------------------------------------%
:- end_module arg_info.
%----------------------------------------------------------------------------%
