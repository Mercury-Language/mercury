%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2000 The University of Melbourne.
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

%-----------------------------------------------------------------------------%

:- module arg_info.
:- interface. 
:- import_module hlds_module, hlds_pred, llds, prog_data.
:- import_module list, assoc_list.

:- pred generate_arg_info(module_info, module_info).
:- mode generate_arg_info(in, out) is det.

:- pred arg_info__unify_arg_info(code_model, list(arg_info)).
:- mode arg_info__unify_arg_info(in, out) is det.

:- pred make_arg_infos(list(type), list(mode), code_model, module_info,
	list(arg_info)).
:- mode make_arg_infos(in, in, in, in, out) is det.

	% Given a list of the head variables and their argument information,
	% return a list giving the input variables and their initial locations.
:- pred arg_info__build_input_arg_list(assoc_list(prog_var, arg_info),
	assoc_list(prog_var, lval)).
:- mode arg_info__build_input_arg_list(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module code_util, mode_util.
:- import_module std_util, map, int, require.

%-----------------------------------------------------------------------------%

	% This whole section just traverses the module structure.

generate_arg_info(ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, Preds),
	map__keys(Preds, PredIds),
	generate_pred_arg_info(PredIds, ModuleInfo0, ModuleInfo).

:- pred generate_pred_arg_info(list(pred_id), module_info, module_info).
:- mode generate_pred_arg_info(in, in, out) is det.

generate_pred_arg_info([], ModuleInfo, ModuleInfo).
generate_pred_arg_info([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procids(PredInfo, ProcIds),
	generate_proc_list_arg_info(PredId, ProcIds, ModuleInfo0, ModuleInfo1),
	generate_pred_arg_info(PredIds, ModuleInfo1, ModuleInfo).

:- pred generate_proc_list_arg_info(pred_id, list(proc_id),
	module_info, module_info).
:- mode generate_proc_list_arg_info(in, in, in, out) is det.

generate_proc_list_arg_info(_PredId, [], ModuleInfo, ModuleInfo).
generate_proc_list_arg_info(PredId, [ProcId | ProcIds], 
		ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	( hlds_pred__pred_info_is_aditi_relation(PredInfo0) ->
		ModuleInfo1 = ModuleInfo0
	;
		pred_info_procedures(PredInfo0, ProcTable0),
		pred_info_arg_types(PredInfo0, ArgTypes),
		map__lookup(ProcTable0, ProcId, ProcInfo0),

		generate_proc_arg_info(ProcInfo0, ArgTypes, 
			ModuleInfo0, ProcInfo),

		map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
		pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
		map__det_update(PredTable0, PredId, PredInfo, PredTable),
		module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1)
	),
	generate_proc_list_arg_info(PredId, ProcIds, ModuleInfo1, ModuleInfo).

:- pred generate_proc_arg_info(proc_info, list(type), module_info, proc_info).
:- mode generate_proc_arg_info(in, in, in, out) is det.

generate_proc_arg_info(ProcInfo0, ArgTypes, ModuleInfo, ProcInfo) :-
	proc_info_argmodes(ProcInfo0, ArgModes),
	proc_info_interface_code_model(ProcInfo0, CodeModel),
	make_arg_infos(ArgTypes, ArgModes, CodeModel, ModuleInfo, ArgInfo),
	proc_info_set_arg_info(ProcInfo0, ArgInfo, ProcInfo).

%---------------------------------------------------------------------------%

	% This is the useful part of the code ;-).

	% This code is one of the places where we make assumptions
	% about the calling convention.  This is the only place in
	% the compiler that makes such assumptions, but there are
	% other places scattered around the runtime and the library
	% which also rely on it.

	% We assume all input arguments always go in sequentially numbered
	% registers starting at register number 1. We also assume that
	% all output arguments go in sequentially numbered registers
	% starting at register number 1, except for model_semi procedures,
	% where the first register is reserved for the result and hence
	% the output arguments start at register number 2.

make_arg_infos(ArgTypes, ArgModes, CodeModel, ModuleInfo, ArgInfo) :-
	( CodeModel = model_semi ->
		StartReg = 2
	;
		StartReg = 1
	),
	make_arg_infos_list(ArgModes, ArgTypes, 1, StartReg,
		ModuleInfo, ArgInfo).

:- pred make_arg_infos_list(list(mode), list(type), int, int,
	module_info, list(arg_info)).
:- mode make_arg_infos_list(in, in, in, in, in, out) is det.

make_arg_infos_list([], [], _, _, _, []).
make_arg_infos_list([Mode | Modes], [Type | Types], InReg0, OutReg0,
		ModuleInfo, [ArgInfo | ArgInfos]) :-
	mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode),
	(
		ArgMode = top_in,
		ArgReg = InReg0,
		InReg1 is InReg0 + 1,
		OutReg1 = OutReg0
	;
		ArgMode = top_out,
		ArgReg = OutReg0,
		InReg1 = InReg0,
		OutReg1 is OutReg0 + 1
	;
		% Allocate unused args as if they were outputs.
		% We must allocate them a register, and the choice
		% should not matter since unused args should be rare.
		ArgMode = top_unused,
		ArgReg = OutReg0,
		InReg1 = InReg0,
		OutReg1 is OutReg0 + 1
	),
	ArgInfo = arg_info(ArgReg, ArgMode),
	make_arg_infos_list(Modes, Types, InReg1, OutReg1,
		ModuleInfo, ArgInfos).
make_arg_infos_list([], [_|_], _, _, _, _) :-
	error("make_arg_infos_list: length mis-match").
make_arg_infos_list([_|_], [], _, _, _, _) :-
	error("make_arg_infos_list: length mis-match").

%---------------------------------------------------------------------------%

arg_info__unify_arg_info(model_det,
	[arg_info(1, top_in), arg_info(2, top_in)]).
arg_info__unify_arg_info(model_semi,
	[arg_info(1, top_in), arg_info(2, top_in)]).
arg_info__unify_arg_info(model_non, _) :-
	error("arg_info: nondet unify!").

%---------------------------------------------------------------------------%

arg_info__build_input_arg_list([], []).
arg_info__build_input_arg_list([V - Arg | Rest0], VarArgs) :-
	Arg = arg_info(Loc, Mode),
	( Mode = top_in ->
		code_util__arg_loc_to_register(Loc, Reg),
		VarArgs = [V - Reg | VarArgs0]
	;
		VarArgs = VarArgs0
	),
	arg_info__build_input_arg_list(Rest0, VarArgs0).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
