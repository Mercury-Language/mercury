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

:- import_module hlds_module, hlds_pred.
:- import_module string, io.

%-----------------------------------------------------------------------------%

:- type task --->	update_module(pred(
				pred_id, proc_id,
				module_info, io__state,
				module_info, io__state))
		;	update_proc(pred(
				pred_id, proc_id, module_info,
				proc_info, io__state,
				proc_info, io__state)).

:- inst task =	bound((update_module(pred(in, in, in, di, out, uo) is det)
		;	update_proc(pred(in, in, in, in, di, out, uo) is det)
		)).

:- mode task ::	task -> task.

:- pred process_all_nonimported_procs(task, module_info, module_info,
	io__state, io__state).
:- mode process_all_nonimported_procs(task, in, out, di, uo) is det.

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

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module options, globals, hlds_out.
:- import_module bool, int, map, tree234, std_util.

process_all_nonimported_procs(Task, ModuleInfo0, ModuleInfo) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	process__nonimported_procs_in_preds(PredIds, Task,
		ModuleInfo0, ModuleInfo).

:- pred process__nonimported_procs_in_preds(list(pred_id), task,
	module_info, module_info, io__state, io__state).
:- mode process__nonimported_procs_in_preds(in, task, in, out, di, uo) is det.

process__nonimported_procs_in_preds([], _, ModuleInfo, ModuleInfo) --> [].
process__nonimported_procs_in_preds([PredId | PredIds], Task,
		ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	process__nonimported_procs(ProcIds, PredId, Task,
		ModuleInfo0, ModuleInfo1),
	process__nonimported_procs_in_preds(PredIds, Task,
		ModuleInfo1, ModuleInfo).

:- pred process__nonimported_procs(list(proc_id), pred_id, task,
	module_info, module_info, io__state, io__state).
:- mode process__nonimported_procs(in, in, task, in, out, di, uo)
	is det.

process__nonimported_procs([], _PredId, _Task,
		ModuleInfo, ModuleInfo, State, State).
process__nonimported_procs([ProcId | ProcIds], PredId, Task,
		ModuleInfo0, ModuleInfo, State0, State) :-
	(
		Task = update_module(Closure),
		call(Closure, ProcId, PredId,
			ModuleInfo0, State0, ModuleInfo1, State1)
	;
		Task = update_proc(Closure),

		module_info_preds(ModuleInfo0, Preds0),
		map__lookup(Preds0, PredId, Pred0),
		pred_info_procedures(Pred0, Procs0),
		map__lookup(Procs0, ProcId, Proc0),

		call(Closure, PredId, ProcId, ModuleInfo0,
			Proc0, State0, Proc, State1),

		map__set(Procs0, ProcId, Proc, Procs),
		pred_info_set_procedures(Pred0, Procs, Pred),
		map__set(Preds0, PredId, Pred, Preds),
		module_info_set_preds(ModuleInfo0, Preds, ModuleInfo1)
	),
	process__nonimported_procs(ProcIds, PredId, Task,
		ModuleInfo1, ModuleInfo, State1, State).

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
