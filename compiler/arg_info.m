%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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

:- import_module hlds.

:- pred generate_arg_info(module_info, module_info).
:- mode generate_arg_info(input, output) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, int.

%-----------------------------------------------------------------------------%

	% This whole section just traverses the module structure.

generate_arg_info(ModuleInfo0, ModuleInfo) :-
	moduleinfo_predids(ModuleInfo0, PredIds),
	generate_pred_arg_info(PredIds, ModuleInfo0, ModuleInfo).

:- pred generate_pred_arg_info(list(pred_id), module_info, module_info).
:- mode generate_pred_arg_info(in, in, out) is det.

generate_pred_arg_info([], ModuleInfo, ModuleInfo).
generate_pred_arg_info([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	moduleinfo_preds(ModuleInfo0, PredTable),
	map__search(PredTable, PredId, PredInfo),
	predinfo_procids(PredInfo, ProcIds),
	generate_proc_arg_info(PredId, ProcIds, ModuleInfo0, ModuleInfo1),
	generate_pred_arg_info(PredIds, ModuleInfo1, ModuleInfo).

:- pred generate_proc_arg_info(pred_id, list(proc_id),
				module_info, module_info).
:- mode generate_proc_arg_info(in, in, in, out) is det.

generate_proc_arg_info(_PredId, [], ModuleInfo, ModuleInfo).
generate_proc_arg_info(PredId, [ProcId | ProcIds], ModuleInfo0, ModuleInfo) :-
	moduleinfo_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	predinfo_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),
	make_arg_infos(ProcInfo0, ModuleInfo0, ProcInfo),
	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	predinfo_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	moduleinfo_set_preds(ModuleInfo0, PredTable, ModuleInfo1),
	generate_proc_arg_info(PredId, ProcIds, ModuleInfo1, ModuleInfo).

%---------------------------------------------------------------------------%

	% This is the useful part of the code ;-).

	% This code is one of the places where we make assumptions
	% about the calling convention.  Here we assume all arguments
	% go in sequentially numbered registers starting at register
	% number 1, except for semi-deterministic procs, where the
	% first register is reserved for the result and hence the arguments
	% start at register number 2.

:- pred make_arg_infos(proc_info, module_info, proc_info).
:- mode make_arg_infos(in, in, out) is det.

make_arg_infos(ProcInfo0, ModuleInfo, ProcInfo) :-
	procinfo_argmodes(ProcInfo0, ArgModes),
	procinfo_inferred_determinism(ProcInfo0, Determinism),
	( Determinism = semideterministic ->
		StartingRegister = 2
	;
		StartingRegister = 1
	),
	make_arg_infos_list(ArgModes, StartingRegister, ModuleInfo, ArgInfo),
	procinfo_set_arginfo(ProcInfo0, ArgInfo, ProcInfo).

:- pred make_arg_infos_list(list(mode), int, module_info, list(arg_info)).
:- mode make_arg_infos_list(in, in, in, out) is det.

make_arg_infos_list([], _, _, []).
make_arg_infos_list([Mode | Modes], RegNum, ModuleInfo,
			[ArgInfo | ArgInfos]) :-
	( mode_is_input(ModuleInfo, Mode) ->
		ArgMode = top_in
	; mode_is_output(ModuleInfo, Mode) ->
		ArgMode = top_out
	;
		ArgMode = top_unused
	),
	ArgInfo = arginfo(RegNum, ArgMode),
	RegNum1 is RegNum + 1,
	make_arg_infos_list(Modes, RegNum1, ModuleInfo, ArgInfos).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
