%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Author: zs

% This file contains auxiliary routines for the passes of the front and back
% ends of the compiler.

:- module passes_aux.

:- interface.

:- import_module string, io.
:- import_module hlds.

%-----------------------------------------------------------------------------%

:- pred write_progress_message(string::in, pred_id::in, module_info::in,
	io__state::di, io__state::uo) is det.

:- pred maybe_report_stats(bool::in, io__state::di, io__state::uo) is det.
:- pred maybe_write_string(bool::input, string::input,
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
:- import_module bool, tree234, std_util.

write_progress_message(Message, PredId, ModuleInfo) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		io__write_string(Message),
		hlds_out__write_pred_id(ModuleInfo, PredId),
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
