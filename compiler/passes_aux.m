%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This file contains auxiliary routines for the passes
% of the front and back ends of the compiler.

% Author: zs

:- module passes_aux.

:- interface.

:- import_module hlds_module, hlds_pred, prog_data.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type task	--->	update_proc(pred(
				proc_info, module_info, proc_info))
		;	update_proc_io(pred(
				pred_id, proc_id, module_info,
				proc_info, proc_info, io__state, io__state))
		;	update_proc_error(pred(
				pred_id, proc_id, module_info, module_info,
				proc_info, proc_info, int, int,
				io__state, io__state))
		;	update_module(pred(
				proc_info, proc_info,
				module_info, module_info))
		;	update_module_io(pred(
				pred_id, proc_id, proc_info, proc_info,
				module_info, module_info,
				io__state, io__state))
		% It would be better to use an existentiallly-quantified type
		% rather than `univ' here, but Mercury 0.6 doesn't support
		% existentially-quantified types.
		;	update_module_cookie(pred(
				pred_id, proc_id, proc_info, proc_info,
				univ, univ, module_info, module_info),
				univ)
		.

/****************

Note that update_module_cookie causes some difficulties.
Ideally, it should be implemented using existential types:

	:- type task --->
			...
		;	some [T] update_module_cookie(pred(
				pred_id, proc_id, proc_info, proc_info,
				T, T, module_info, module_info),
				T)

That would avoid the need for messing about with type_to_univ and
univ_to_type.

Originally, it was implemented by changing `task' to `task(T)':

	:- type task(T) --->
			...
		;	update_module_cookie(pred(
				pred_id, proc_id, proc_info, proc_info,
				T, T, module_info, module_info),
				T)

but that is not a good solution, because it causes a lot of warnings
about unbound type variables.

****************/

:- inst task =	bound(( update_proc(pred(in, in, out) is det)
		;	update_proc_io(pred(in, in, in, in, out, di, uo) is det)
		;	update_proc_error(pred(in, in, in, out, in, out,
				out, out, di, uo) is det)
		;	update_module(pred(in, out, in, out) is det)
		;	update_module_io(pred(in, in, in, out,
				in, out, di, uo) is det)
		;	update_module_cookie(pred(in, in, in, out, in, out,
				in, out) is det, ground)
		)).

:- mode task ::	task -> task.

:- pred process_all_nonimported_procs(task, module_info, module_info,
	io__state, io__state).
:- mode process_all_nonimported_procs(task, in, out, di, uo) is det.

:- pred process_all_nonimported_procs(task, task,
	module_info, module_info, io__state, io__state).
:- mode process_all_nonimported_procs(task, out(task), in, out, di, uo) is det.

:- pred write_pred_progress_message(string::in, pred_id::in, module_info::in,
	io__state::di, io__state::uo) is det.

:- pred write_proc_progress_message(string::in, pred_id::in, proc_id::in,
	module_info::in, io__state::di, io__state::uo) is det.

:- pred maybe_report_stats(bool::in, io__state::di, io__state::uo) is det.
:- pred maybe_write_string(bool::in, string::in,
	io__state::di, io__state::uo) is det.
:- pred maybe_flush_output(bool::in, io__state::di, io__state::uo) is det.

:- pred report_error(string::in, io__state::di, io__state::uo) is det.

:- pred invoke_system_command(string::in, bool::out,
	io__state::di, io__state::uo) is det.

:- pred maybe_report_sizes(module_info::in, io__state::di, io__state::uo)
	is det.


:- pred report_pred_proc_id(module_info, pred_id, proc_id, 
		maybe(term__context), term__context, io__state, io__state).
:- mode report_pred_proc_id(in, in, in, in, out, di, uo) is det.

:- pred report_pred_name_mode(pred_or_func, string, list((mode)),
				io__state, io__state).
:- mode report_pred_name_mode(in, in, in, di, uo) is det.
	

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module options, globals, hlds_out, prog_out, mode_util.
:- import_module mercury_to_mercury.
:- import_module bool, int, map, tree234, std_util, require, list.
:- import_module varset.

process_all_nonimported_procs(Task, ModuleInfo0, ModuleInfo) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	process_nonimported_procs_in_preds(PredIds, Task, _,
		ModuleInfo0, ModuleInfo).

process_all_nonimported_procs(Task0, Task, ModuleInfo0, ModuleInfo) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	process_nonimported_procs_in_preds(PredIds, Task0, Task,
		ModuleInfo0, ModuleInfo).

:- pred process_nonimported_procs_in_preds(list(pred_id), task, task,
	module_info, module_info, io__state, io__state).
:- mode process_nonimported_procs_in_preds(in, task, out(task), in, out,
	di, uo) is det.

process_nonimported_procs_in_preds([], Task, Task, ModuleInfo, ModuleInfo)
		--> [].
process_nonimported_procs_in_preds([PredId | PredIds], Task0, Task,
		ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	process_nonimported_procs(ProcIds, PredId, Task0, Task1,
		ModuleInfo0, ModuleInfo1),
	process_nonimported_procs_in_preds(PredIds, Task1, Task,
		ModuleInfo1, ModuleInfo).

:- pred process_nonimported_procs(list(proc_id), pred_id, task, task,
	module_info, module_info, io__state, io__state).
:- mode process_nonimported_procs(in, in, task, out(task), in, out, di, uo)
	is det.

process_nonimported_procs([], _PredId, Task, Task,
		ModuleInfo, ModuleInfo, State, State).
process_nonimported_procs([ProcId | ProcIds], PredId, Task0, Task,
		ModuleInfo0, ModuleInfo, State0, State) :-

	module_info_preds(ModuleInfo0, Preds0),
	map__lookup(Preds0, PredId, Pred0),
	pred_info_procedures(Pred0, Procs0),
	map__lookup(Procs0, ProcId, Proc0),

	(
		Task0 = update_module(Closure),
		call(Closure, Proc0, Proc, ModuleInfo0, ModuleInfo8),
		Task1 = Task0,
		State9 = State0
	;
		Task0 = update_module_io(Closure),
		call(Closure, PredId, ProcId, Proc0, Proc,
			ModuleInfo0, ModuleInfo8, State0, State9),
		Task1 = Task0
	;
		Task0 = update_proc(Closure),
		call(Closure, Proc0, ModuleInfo0, Proc),
		ModuleInfo8 = ModuleInfo0,
		Task1 = Task0,
		State9 = State0
	;
		Task0 = update_proc_io(Closure),
		call(Closure, PredId, ProcId, ModuleInfo0,
			Proc0, Proc, State0, State9),
		ModuleInfo8 = ModuleInfo0,
		Task1 = Task0
	;
		Task0 = update_proc_error(Closure),
		call(Closure, PredId, ProcId, ModuleInfo0, ModuleInfo1,
			Proc0, Proc, WarnCnt, ErrCnt, State0, State1),
		globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn,
			State1, State2),
		Task1 = Task0,
		(
			(
				ErrCnt > 0
			;
				WarnCnt > 0,
				HaltAtWarn = yes
			)
		->
			io__set_exit_status(1, State2, State9),
			module_info_incr_errors(ModuleInfo1, ModuleInfo8)
		;
			ModuleInfo8 = ModuleInfo1,
			State9 = State2
		)
	;
		Task0 = update_module_cookie(Closure, Cookie0),
		call(Closure, PredId, ProcId, Proc0, Proc,
			Cookie0, Cookie1, ModuleInfo0, ModuleInfo8),
		Task1 = update_module_cookie(Closure, Cookie1),
		State9 = State0
	),

	% If the pass changed the module_info, it may have changed
	% the pred table or the proc table for this pred_id.  Don't
	% take any chances.

	module_info_preds(ModuleInfo8, Preds8),
	map__lookup(Preds8, PredId, Pred8),
	pred_info_procedures(Pred8, Procs8),

	map__det_update(Procs8, ProcId, Proc, Procs),
	pred_info_set_procedures(Pred8, Procs, Pred),
	map__det_update(Preds8, PredId, Pred, Preds),
	module_info_set_preds(ModuleInfo8, Preds, ModuleInfo9),

	process_nonimported_procs(ProcIds, PredId, Task1, Task,
		ModuleInfo9, ModuleInfo, State9, State).

write_pred_progress_message(Message, PredId, ModuleInfo) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		io__write_string(Message),
		hlds_out__write_pred_id(ModuleInfo, PredId),
		io__write_string("\n")
	;
		[]
	).

write_proc_progress_message(Message, PredId, ProcId, ModuleInfo) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		io__write_string(Message),
		hlds_out__write_pred_proc_id(ModuleInfo, PredId, ProcId),
		io__write_string("\n")
	;
		[]
	).

maybe_report_stats(yes) --> io__report_stats.
maybe_report_stats(no) --> [].

maybe_write_string(yes, String) --> io__write_string(String).
maybe_write_string(no, _) --> [].

maybe_flush_output(yes) --> io__flush_output.
maybe_flush_output(no) --> [].

report_error(ErrorMessage) -->
	io__write_string("Error: "),
	io__write_string(ErrorMessage),
	io__write_string("\n"),
	io__set_exit_status(1).

invoke_system_command(Command, Succeeded) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Invoking system command `"),
	maybe_write_string(Verbose, Command),
	maybe_write_string(Verbose, "'...\n"),
	io__call_system(Command, Result),
	( { Result = ok(0) } ->
		maybe_write_string(Verbose, "% done.\n"),
		{ Succeeded = yes }
	; { Result = ok(_) } ->
		report_error("system command returned non-zero exit status."),
		{ Succeeded = no }
	;	
		report_error("unable to invoke system command."),
		{ Succeeded = no }
	).

maybe_report_sizes(HLDS) -->
	globals__io_lookup_bool_option(statistics, Statistics),
	( { Statistics = yes } ->
		report_sizes(HLDS)
	;
		[]
	).

:- pred report_sizes(module_info, io__state, io__state).
:- mode report_sizes(in, di, uo) is det.

report_sizes(ModuleInfo) -->
	{ module_info_preds(ModuleInfo, Preds) },
	tree_stats("Pred table", Preds),
	{ module_info_types(ModuleInfo, Types) },
	tree_stats("Type table", Types),
	{ module_info_ctors(ModuleInfo, Ctors) },
	tree_stats("Constructor table", Ctors).

:- pred tree_stats(string, map(_K, _V), io__state, io__state).
:- mode tree_stats(in, in, di, uo) is det.

tree_stats(Description, Tree) -->
	{ tree234__count(Tree, Count) },
	% { tree234__depth(Tree, Depth) },
	io__write_string(Description),
	io__write_string(": count = "),
	io__write_int(Count),
	% io__write_string(", depth = "),
	% io__write_int(Depth),
	io__write_string("\n").

%-----------------------------------------------------------------------------%


report_pred_proc_id(ModuleInfo, PredId, ProcId, MaybeContext, Context) -->
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo) },
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_arity(PredInfo, Arity) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ proc_info_context(ProcInfo, Context) },
	{ proc_info_argmodes(ProcInfo, ArgModes0) },

	% We need to strip off the extra type_info arguments inserted at the
	% front by polymorphism.m - we only want the last `PredArity' of them.
	%
	{ list__length(ArgModes0, NumArgModes) },
	{ NumToDrop is NumArgModes - Arity },
	( { list__drop(NumToDrop, ArgModes0, ArgModes1) } ->
		{ ArgModes = ArgModes1 }
	;	
		{ error("report_pred_proc_id: list__drop failed") }
	),
	(
		{ MaybeContext = yes(OutContext) }
	;
		{ MaybeContext = no },
		{ OutContext = Context }
	),
	prog_out__write_context(OutContext),
	io__write_string("In `"),
	report_pred_name_mode(PredOrFunc, PredName, ArgModes),
	io__write_string("':\n").


report_pred_name_mode(predicate, PredName, ArgModes) -->
	io__write_string(PredName),
	( { ArgModes \= [] } ->
		{ varset__init(InstVarSet) },	% XXX inst var names
		io__write_string("("),
		{ strip_builtin_qualifiers_from_mode_list(ArgModes,
								ArgModes1) },
		mercury_output_mode_list(ArgModes1, InstVarSet),
		io__write_string(")")
	;
		[]
	).

report_pred_name_mode(function, FuncName, ArgModes) -->
	{ varset__init(InstVarSet) },	% XXX inst var names
	{ strip_builtin_qualifiers_from_mode_list(ArgModes, ArgModes1) },
	{ pred_args_to_func_args(ArgModes1, FuncArgModes, FuncRetMode) },
	io__write_string(FuncName),
	( { FuncArgModes \= [] } ->
		io__write_string("("),
		mercury_output_mode_list(FuncArgModes, InstVarSet),
		io__write_string(")")
	;
		[]
	),
	io__write_string(" = "),
	mercury_output_mode(FuncRetMode, InstVarSet).
%-----------------------------------------------------------------------------%
