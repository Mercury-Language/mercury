%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This file contains auxiliary routines for the passes
% of the front and back ends of the compiler.

% Author: zs

:- module hlds__passes_aux.

:- interface.

:- import_module hlds__hlds_module, hlds__hlds_pred, parse_tree__prog_data.
:- import_module io, std_util, list, bool.

%-----------------------------------------------------------------------------%

:- type task	--->	update_proc(pred(
				proc_info, module_info, proc_info))
		;	update_proc_predid(pred(
				proc_info, pred_id, module_info, proc_info))
		;	update_proc_predprocid(pred(
				proc_info, pred_id, proc_id,
				module_info, proc_info))
		;	update_proc_io(pred(
				pred_id, proc_id, module_info,
				proc_info, proc_info, io__state, io__state))
		;	update_proc_error(pred(
				pred_id, proc_id, module_info, module_info,
				proc_info, proc_info, int, int,
				io__state, io__state))
		;	update_pred_error(pred_error_task)
		;	update_module(pred(
				pred_info, proc_info, proc_info,
				module_info, module_info))
		;	update_module_io(pred(
				pred_id, proc_id, proc_info, proc_info,
				module_info, module_info,
				io__state, io__state))
		% It would be better to use an existentially-quantified type
		% rather than `univ' here, but the current version of Mercury 
		% doesn't support existentially-quantified types.
		;	update_module_cookie(pred(
				pred_id, proc_id, proc_info, proc_info,
				univ, univ, module_info, module_info),
				univ)
		.


:- type pred_error_task ==
		pred(pred_id, module_info, module_info, pred_info, pred_info,
			int, int, io__state, io__state).

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
		;	update_proc_predid(pred(in, in, in, out) is det)
		;	update_proc_predprocid(pred(in, in, in, in, out)
				is det)
		;	update_proc_io(pred(in, in, in, in, out, di, uo)
				is det)
		;	update_proc_error(pred(in, in, in, out, in, out,
				out, out, di, uo) is det)
		;	update_pred_error(pred(in, in, out, in, out,
				out, out, di, uo) is det)
		;	update_module(pred(in, in, out, in, out) is det)
		;	update_module_io(pred(in, in, in, out,
				in, out, di, uo) is det)
		;	update_module_cookie(pred(in, in, in, out, in, out,
				in, out) is det, ground)
		)).

:- inst pred_error_task =
		(pred(in, in, out, in, out, out, out, di, uo) is det).

:- mode task ::	task -> task.

:- pred process_all_nonimported_procs(task, module_info, module_info,
	io__state, io__state).
:- mode process_all_nonimported_procs(task, in, out, di, uo) is det.

	% Process procedures for which a given test succeeds.
:- pred process_matching_nonimported_procs(task, pred(pred_info),
	module_info, module_info, io__state, io__state).
:- mode process_matching_nonimported_procs(task, pred(in) is semidet,
	in, out, di, uo) is det.

:- pred process_matching_nonimported_procs(task, task, pred(pred_info),
	module_info, module_info, io__state, io__state).
:- mode process_matching_nonimported_procs(task, out(task),
	pred(in) is semidet, in, out, di, uo) is det.

:- pred process_all_nonimported_nonaditi_procs(task, module_info, module_info,
	io__state, io__state).
:- mode process_all_nonimported_nonaditi_procs(task, in, out, di, uo) is det.

:- pred process_all_nonimported_nonaditi_procs(task, task,
	module_info, module_info, io__state, io__state).
:- mode process_all_nonimported_nonaditi_procs(task, out(task),
	in, out, di, uo) is det.

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
:- pred report_error(io__output_stream::in, string::in,
		io__state::di, io__state::uo) is det.

:- pred maybe_report_sizes(module_info::in, io__state::di, io__state::uo)
	is det.

:- pred report_pred_proc_id(module_info, pred_id, proc_id, 
		maybe(prog_context), prog_context, io__state, io__state).
:- mode report_pred_proc_id(in, in, in, in, out, di, uo) is det.

:- pred report_pred_name_mode(pred_or_func, string, list((mode)),
				io__state, io__state).
:- mode report_pred_name_mode(in, in, in, di, uo) is det.

	% Write to a given filename, giving appropriate status
	% messages and error messages if the file cannot be opened.
:- pred output_to_file(string, pred(io__state, io__state),
		io__state, io__state).
:- mode output_to_file(in, pred(di, uo) is det, di, uo) is det.

	% Same as output_to_file/4 above, but allow the writing predicate
	% to generate some output.
:- pred output_to_file(string, pred(T, io__state, io__state),
				maybe(T), io__state, io__state).
:- mode output_to_file(in, pred(out, di, uo) is det, out, di, uo) is det.


%-----------------------------------------------------------------------------%

:- type quote_char
	--->	forward		% '
	;	double.		% "

:- type command_verbosity
	--->	verbose			% Output the command line
					% only with `--verbose'.

	;	verbose_commands	% Output the command line
					% with `--verbose-commands'.
					% This should be used for
					% commands that may be of
					% interest to the user.
	.

	% Invoke a shell script.
	% Both standard and error output will go to the
	% specified output stream.
:- pred invoke_shell_command(io__output_stream::in,
	command_verbosity::in, string::in, bool::out,
	io__state::di, io__state::uo) is det.

	% Invoke an executable.
	% Both standard and error output will go to the
	% specified output stream.
:- pred invoke_system_command(io__output_stream::in,
	command_verbosity::in, string::in, bool::out,
	io__state::di, io__state::uo) is det.

	% Make a command string, which needs to be invoked in a shell
	% environment.
:- pred make_command_string(string::in, quote_char::in, string::out) is det.

	% If the bool is `no' set the exit status to 1.
:- pred maybe_set_exit_status(bool::in, io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs__options, libs__globals, hlds__hlds_out.
:- import_module parse_tree__prog_out, check_hlds__mode_util.
:- import_module parse_tree__mercury_to_mercury, libs__process_util.

:- import_module int, string, map, require, varset.

process_all_nonimported_procs(Task, ModuleInfo0, ModuleInfo) -->
	{ True = lambda([_PredInfo::in] is semidet, true) },
	process_matching_nonimported_procs(Task, True, 
		ModuleInfo0, ModuleInfo).

process_all_nonimported_nonaditi_procs(Task, ModuleInfo0, ModuleInfo) -->
	{ NotAditi = lambda([PredInfo::in] is semidet, (
			\+ hlds_pred__pred_info_is_aditi_relation(PredInfo)
		)) }, 
	process_matching_nonimported_procs(Task, NotAditi, 
		ModuleInfo0, ModuleInfo).

process_all_nonimported_nonaditi_procs(Task0, Task,
		ModuleInfo0, ModuleInfo) -->
	{ NotAditi = lambda([PredInfo::in] is semidet, (
			\+ hlds_pred__pred_info_is_aditi_relation(PredInfo)
		)) }, 
	process_matching_nonimported_procs(Task0, Task, NotAditi, 
		ModuleInfo0, ModuleInfo).

process_all_nonimported_procs(Task0, Task, ModuleInfo0, ModuleInfo) -->
	{ True = lambda([_PredInfo::in] is semidet, true) },
	process_matching_nonimported_procs(Task0, Task, True, 
		ModuleInfo0, ModuleInfo).

process_matching_nonimported_procs(Task, Filter, ModuleInfo0, ModuleInfo) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	( { Task = update_pred_error(Pred) } ->
		list__foldl2(process_nonimported_pred(Pred, Filter), PredIds,
			ModuleInfo0, ModuleInfo)
	;
		process_nonimported_procs_in_preds(PredIds, Task, _, Filter,
			ModuleInfo0, ModuleInfo)
	).

process_matching_nonimported_procs(Task0, Task, Filter, 
		ModuleInfo0, ModuleInfo) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	process_nonimported_procs_in_preds(PredIds, Task0, Task, Filter,
		ModuleInfo0, ModuleInfo).

:- pred process_nonimported_pred(pred_error_task, pred(pred_info), pred_id, 
	module_info, module_info, io__state, io__state).
:- mode process_nonimported_pred(in(pred_error_task), pred(in) is semidet, in,
	in, out, di, uo) is det.

process_nonimported_pred(Task, Filter, PredId, ModuleInfo0, ModuleInfo,
		IO0, IO) :-
	module_info_pred_info(ModuleInfo0, PredId, PredInfo0),
	(
		( pred_info_is_imported(PredInfo0)
		; \+ call(Filter, PredInfo0)
		)
	->
		ModuleInfo = ModuleInfo0,
		IO = IO0
	;
		call(Task, PredId, ModuleInfo0, ModuleInfo1,
			PredInfo0, PredInfo, WarnCnt, ErrCnt, IO0, IO1),
		module_info_set_pred_info(ModuleInfo1,
			PredId, PredInfo, ModuleInfo2),
		passes_aux__handle_errors(WarnCnt, ErrCnt,
			ModuleInfo2, ModuleInfo, IO1, IO)
	).

:- pred process_nonimported_procs_in_preds(list(pred_id), task, task,
	pred(pred_info), module_info, module_info, io__state, io__state).
:- mode process_nonimported_procs_in_preds(in, task, out(task), 
	pred(in) is semidet, in, out, di, uo) is det.

process_nonimported_procs_in_preds([], Task, Task, _, ModuleInfo, ModuleInfo)
		--> [].
process_nonimported_procs_in_preds([PredId | PredIds], Task0, Task, Filter,
		ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	( { call(Filter, PredInfo) } ->
		{ pred_info_non_imported_procids(PredInfo, ProcIds) },
		process_nonimported_procs(ProcIds, PredId, Task0, Task1,
			ModuleInfo0, ModuleInfo1)
	;
		{ ModuleInfo1 = ModuleInfo0 },
		{ Task1 = Task0 }
	),
	process_nonimported_procs_in_preds(PredIds, Task1, Task, Filter,
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
		call(Closure, Pred0, Proc0, Proc, ModuleInfo0, ModuleInfo8),
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
		Task0 = update_proc_predid(Closure),
		call(Closure, Proc0, PredId, ModuleInfo0, Proc),
		ModuleInfo8 = ModuleInfo0,
		Task1 = Task0,
		State9 = State0
	;
		Task0 = update_proc_predprocid(Closure),
		call(Closure, Proc0, PredId, ProcId, ModuleInfo0, Proc),
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
		Task1 = Task0,
		passes_aux__handle_errors(WarnCnt, ErrCnt,
			ModuleInfo1, ModuleInfo8, State1, State9)
	;
		Task0 = update_pred_error(_),
		error("passes_aux:process_non_imported_procs")
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

report_error(Stream, ErrorMessage) -->
	io__set_output_stream(Stream, OldStream),
	report_error(ErrorMessage),
	io__set_output_stream(OldStream, _).

:- pred passes_aux__handle_errors(int, int, module_info, module_info,
		io__state, io__state).
:- mode passes_aux__handle_errors(in, in, in, out, di, uo) is det.

passes_aux__handle_errors(WarnCnt, ErrCnt, ModuleInfo1, ModuleInfo8,
		State1, State9) :-
	globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn,
		State1, State2),
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
	).

maybe_set_exit_status(yes) --> [].
maybe_set_exit_status(no) --> io__set_exit_status(1).

invoke_shell_command(ErrorStream, Verbosity, Command0, Succeeded) -->
	{ make_command_string(Command0, forward, Command) },
	invoke_system_command(ErrorStream, Verbosity, Command, Succeeded).

invoke_system_command(ErrorStream, Verbosity, Command, Succeeded) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	(
		{ Verbosity = verbose },
		{ PrintCommand = Verbose }
	;	
		{ Verbosity = verbose_commands },
		globals__io_lookup_bool_option(verbose_commands, PrintCommand)
	),	
	( { PrintCommand = yes } ->
		io__write_string("% Invoking system command `"),
		io__write_string(Command),
		io__write_string("'...\n"),
		io__flush_output
	;
		[]
	),

	%
	% The output from the command is written to a temporary file,
	% which is then written to the output stream. Without this,
	% the output from the command would go to the current C output
	% and error streams.
	%
	io__make_temp(TmpFile),
	io__call_system_return_signal(
		string__append_list([Command, " > ", TmpFile, " 2>&1"]),
		Result),
	(
		{ Result = ok(exited(Status)) },
		maybe_write_string(PrintCommand, "% done.\n"),
		( { Status = 0 } ->
			{ Succeeded = yes }
		;
			% The command should have produced output
			% describing the error.
			{ Succeeded = no }
		)
	;
		{ Result = ok(signalled(Signal)) },
		% Make sure the current process gets the signal. Some
		% systems (e.g. Linux) ignore SIGINT during a call to
		% system().
		raise_signal(Signal),
		report_error(ErrorStream, "system command received signal "
					++ int_to_string(Signal) ++ "."),
		{ Succeeded = no }
	;
		{ Result = error(Error) },
		report_error(ErrorStream, io__error_message(Error)),
		{ Succeeded = no }
	),

	%
	% Write the output to the error stream.
	%
	io__open_input(TmpFile, TmpFileRes),
	(
		{ TmpFileRes = ok(TmpFileStream) },
		io__input_stream_foldl_io(TmpFileStream,
			io__write_char(ErrorStream), Res),
		(
			{ Res = ok }
		;
			{ Res = error(TmpFileReadError) },
			report_error(ErrorStream,
				"error reading command output: "
					++ io__error_message(TmpFileReadError))
		),
		io__close_input(TmpFileStream)
	;
		{ TmpFileRes = error(TmpFileError) },
		report_error(ErrorStream, "error opening command output: "
				++ io__error_message(TmpFileError))
	),
	io__remove_file(TmpFile, _).

make_command_string(String0, QuoteType, String) :-
	( use_win32 ->
		(
			QuoteType = forward,
			Quote = " '"
		;
			QuoteType = double,
			Quote = " """
		),
		string__append_list(["sh -c ", Quote, String0, Quote], String)
	;
		String = String0
	).

	% Are we compiling in a win32 environment?
:- pred use_win32 is semidet.
:- pragma c_code(use_win32,
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_WIN32
	SUCCESS_INDICATOR = 1;
#else
	SUCCESS_INDICATOR = 0;
#endif
").

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
	{ map__count(Tree, Count) },
	io__write_string(Description),
	io__write_string(": count = "),
	io__write_int(Count),
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

output_to_file(FileName, Action) -->
	{ NewAction = (pred(0::out, di, uo) is det --> Action ) },
	output_to_file(FileName, NewAction, _Result).

output_to_file(FileName, Action, Result) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),
	maybe_write_string(Verbose, "% Writing to file `"),
	maybe_write_string(Verbose, FileName),
	maybe_write_string(Verbose, "'...\n"),
	maybe_flush_output(Verbose),
	io__open_output(FileName, Res),
	( { Res = ok(FileStream) } ->
		io__set_output_stream(FileStream, OutputStream),
		Action(ActionResult),
		io__set_output_stream(OutputStream, _),
		io__close_output(FileStream),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats),
		{ Result = yes(ActionResult) }
	;
		maybe_write_string(Verbose, "\n"),
		{ string__append_list(["can't open file `",
			FileName, "' for output."], ErrorMessage) },
		report_error(ErrorMessage),
		{ Result = no }
	).

%-----------------------------------------------------------------------------%
