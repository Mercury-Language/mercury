%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: arg_info.m
% main author: fjh

% This module is one of the pre-passes of the code generator.
% It initializes the arg_info field of the proc_info structure in the HLDS,
% which records for each argument of each procedure, whether the
% argument is input/output/unused, and which register it is supposed to
% go into.

% Possible optimization: at the moment, each argument is assigned a
% different register.  We could try putting both an input and an output
% argument into a single register, which should improve performance
% a bit.

%-----------------------------------------------------------------------------%

:- module arg_info.
:- interface. 
:- import_module hlds, llds, globals.

:- pred generate_arg_info(module_info, args_method, module_info).
:- mode generate_arg_info(in, in, out) is det.

:- pred arg_info__unify_arg_info(code_model, list(arg_info)).
:- mode arg_info__unify_arg_info(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, int, mode_util, list, require.

%-----------------------------------------------------------------------------%

	% This whole section just traverses the module structure.

generate_arg_info(ModuleInfo0, Method, ModuleInfo) :-
	module_info_preds(ModuleInfo0, Preds),
	map__keys(Preds, PredIds),
	generate_pred_arg_info(PredIds, Method, ModuleInfo0, ModuleInfo).

:- pred generate_pred_arg_info(list(pred_id), args_method,
	module_info, module_info).
:- mode generate_pred_arg_info(in, in, in, out) is det.

generate_pred_arg_info([], _Method, ModuleInfo, ModuleInfo).
generate_pred_arg_info([PredId | PredIds], Method, ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procids(PredInfo, ProcIds),
	generate_proc_arg_info(PredId, ProcIds, Method,
		ModuleInfo0, ModuleInfo1),
	generate_pred_arg_info(PredIds, Method, ModuleInfo1, ModuleInfo).

:- pred generate_proc_arg_info(pred_id, list(proc_id), args_method,
	module_info, module_info).
:- mode generate_proc_arg_info(in, in, in, in, out) is det.

generate_proc_arg_info(_PredId, [], _Method, ModuleInfo, ModuleInfo).
generate_proc_arg_info(PredId, [ProcId | ProcIds], Method,
		ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	make_arg_infos(ProcInfo0, Method, ModuleInfo0, ProcInfo),

	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),

	generate_proc_arg_info(PredId, ProcIds, Method,
		ModuleInfo1, ModuleInfo).

%---------------------------------------------------------------------------%

	% This is the useful part of the code ;-).

	% This code is one of the places where we make assumptions
	% about the calling convention.  Here we assume all arguments
	% go in sequentially numbered registers starting at register
	% number 1, except for semi-deterministic procs, where the
	% first register is reserved for the result and hence the arguments
	% start at register number 2.

:- pred make_arg_infos(proc_info, args_method, module_info, proc_info).
:- mode make_arg_infos(in, in, in, out) is det.

make_arg_infos(ProcInfo0, Method, ModuleInfo, ProcInfo) :-
	proc_info_argmodes(ProcInfo0, ArgModes),
	proc_info_interface_code_model(ProcInfo0, CodeModel),
	( CodeModel = model_semi ->
		StartReg = 2
	;
		StartReg = 1
	),
	(
		Method = simple,
		make_arg_infos_list(ArgModes, StartReg, ModuleInfo, ArgInfo)
	;
		Method = compact,
		make_arg_infos_compact_list(ArgModes, StartReg, StartReg,
			ModuleInfo, ArgInfo)
	),
	proc_info_set_arg_info(ProcInfo0, ArgInfo, ProcInfo).

:- pred make_arg_infos_list(list(mode), int, module_info, list(arg_info)).
:- mode make_arg_infos_list(in, in, in, out) is det.

make_arg_infos_list([], _, _, []).
make_arg_infos_list([Mode | Modes], Reg0, ModuleInfo, [ArgInfo | ArgInfos]) :-
	( mode_is_input(ModuleInfo, Mode) ->
		ArgMode = top_in
	; mode_is_output(ModuleInfo, Mode) ->
		ArgMode = top_out
	;
		ArgMode = top_unused
	),
	ArgInfo = arg_info(Reg0, ArgMode),
	Reg1 is Reg0 + 1,
	make_arg_infos_list(Modes, Reg1, ModuleInfo, ArgInfos).

:- pred make_arg_infos_compact_list(list(mode), int, int,
	module_info, list(arg_info)).
:- mode make_arg_infos_compact_list(in, in, in, in, out) is det.

make_arg_infos_compact_list([], _, _, _, []).
make_arg_infos_compact_list([Mode | Modes], InReg0, OutReg0, ModuleInfo,
		[ArgInfo | ArgInfos]) :-
	( mode_is_input(ModuleInfo, Mode) ->
		ArgMode = top_in,
		ArgReg = InReg0,
		InReg1 is InReg0 + 1,
		OutReg1 = OutReg0
	; mode_is_output(ModuleInfo, Mode) ->
		ArgMode = top_out,
		ArgReg = OutReg0,
		InReg1 = InReg0,
		OutReg1 is OutReg0 + 1
	;
		% Allocate unused regs as if they were outputs.
		% We must allocate them a register, and the choice
		% should not matter since unused args should be rare.
		ArgMode = top_unused,
		ArgReg = OutReg0,
		InReg1 = InReg0,
		OutReg1 is OutReg0 + 1
	),
	ArgInfo = arg_info(ArgReg, ArgMode),
	make_arg_infos_compact_list(Modes, InReg1, OutReg1,
		ModuleInfo, ArgInfos).

%---------------------------------------------------------------------------%

arg_info__unify_arg_info(model_det, [arg_info(1, top_in), arg_info(2, top_in)]).
arg_info__unify_arg_info(model_semi, [arg_info(2, top_in), arg_info(3, top_in)]).
arg_info__unify_arg_info(model_non, _X) :-
	error("arg_info: nondet unify!").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
