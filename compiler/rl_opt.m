%-----------------------------------------------------------------------------%
% Copyright (C) 1998-1999 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_opt.m
% Main author: stayl
%
% Call the RL optimization passes.
%-----------------------------------------------------------------------------%
:- module aditi_backend__rl_opt.

:- interface.

:- import_module hlds__hlds_module, aditi_backend__rl.
:- import_module io, list.

:- pred rl_opt__procs(module_info, list(rl_proc), list(rl_proc),
		io__state, io__state).
:- mode rl_opt__procs(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module libs__globals, libs__options, hlds__passes_aux.
:- import_module parse_tree__prog_out.
:- import_module aditi_backend__rl_block, aditi_backend__rl_liveness.
:- import_module aditi_backend__rl_block_opt, aditi_backend__rl_loop.
:- import_module aditi_backend__rl_sort, aditi_backend__rl_stream.
:- import_module bool, list.

rl_opt__procs(ModuleInfo, Procs0, Procs) -->
	list__map_foldl(rl_opt__proc(ModuleInfo), Procs0, Procs).

:- pred rl_opt__proc(module_info::in, rl_proc::in, rl_proc::out, 
		io__state::di, io__state::uo) is det.

rl_opt__proc(ModuleInfo, Proc0, Proc) -->
	{ Proc0 = rl_proc(Name, _, _, _, _, _, MercuryProcs) },
	{ rl__proc_name_to_string(Name, NameStr) },

	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(debug_rl_opt, Debug),

	maybe_write_string(Verbose, "% Optimizing RL procedure "),
	maybe_write_string(Verbose, NameStr),
	maybe_write_string(Verbose, "\n"),
	maybe_flush_output(Verbose),
	rl_block__create_flow_graph(Debug, ModuleInfo,
		Proc0, Info0),

	globals__io_lookup_bool_option(optimize_rl_invariants, Loops),
	( { Loops = yes } ->
		maybe_write_string(VeryVerbose, 
			"% Detecting loop invariants in "),
		maybe_write_string(VeryVerbose, NameStr),
		maybe_write_string(VeryVerbose, "..."),
		maybe_flush_output(VeryVerbose),
		{ rl_loop__shift_invariants(Info0, Info10) },
		maybe_write_string(VeryVerbose, "done.\n"),
		rl_opt__maybe_dump_rl(Debug, NameStr,
			"10", "invariants", Info10)
	;
		{ Info10 = Info0 }
	),

	globals__io_lookup_bool_option(optimize_rl, Opt),
	globals__io_lookup_bool_option(optimize_rl_index, OptIndex),
	( { Opt = yes } ->
		% rl_block_opt.m requires liveness to have been run.
		maybe_write_string(VeryVerbose, 
			"% Detecting liveness in "),
		maybe_write_string(VeryVerbose, NameStr),
		maybe_write_string(VeryVerbose, "..."),
		maybe_flush_output(VeryVerbose),
		rl_liveness(Info10, Info15),
		maybe_write_string(VeryVerbose, "done.\n"),
		rl_opt__maybe_dump_rl(Debug, NameStr,
			"15", "liveness1", Info15),

		maybe_write_string(VeryVerbose,
			"% Optimizing basic blocks in "),
		maybe_write_string(VeryVerbose, NameStr),
		maybe_write_string(VeryVerbose, "..."),
		maybe_flush_output(VeryVerbose),

		% The `merge_output_projections' flag enables
		% the merging of multiple projections of a single
		% relation into a single instruction, reducing the
		% number of passes over the input relation.
		% It is disabled because for small relations
		% it significantly worsens performance.
		% The problem is that it is much faster to make
		% multiple passes over the input relation than
		% to materialise all the outputs of the projections.
		% If this pass were run again after stream detection,
		% the merging could be done if all outputs are materialised
		% anyway.
		%{ Flags0 = [merge_output_projections] },
		{ Flags0 = [] },
		{ OptIndex = yes ->
			Flags = [add_uniondiff | Flags0]
		;
			Flags = Flags0
		},
		rl_block_opt(Flags, Info15, Info20),
		maybe_write_string(VeryVerbose, "done.\n"),
		rl_opt__maybe_dump_rl(Debug, NameStr,
			"20", "block_opt", Info20)
	;
		{ Info20 = Info10 }
	),

	maybe_write_string(VeryVerbose, 
		"% Detecting final liveness in "),
	maybe_write_string(VeryVerbose, NameStr),
	maybe_write_string(VeryVerbose, "..."),
	maybe_flush_output(VeryVerbose),
	rl_liveness(Info20, Info30),
	rl_opt__maybe_dump_rl(Debug, NameStr, "30", "liveness", Info30),
	maybe_write_string(VeryVerbose, "done.\n"),

	( { OptIndex = yes } ->
		maybe_write_string(VeryVerbose,
			"% Optimizing sorting and indexing in "),
		maybe_write_string(VeryVerbose, NameStr),
		maybe_write_string(VeryVerbose, "..."),
		rl_sort__proc(Info30, Info40),
		maybe_write_string(VeryVerbose, "done.\n"),
		rl_opt__maybe_dump_rl(Debug, NameStr,
			"40", "rl_sort", Info40)
	;
		{ Info40 = Info30 }
	),

	globals__io_lookup_bool_option(detect_rl_streams, OptStreams),
	( { OptStreams = yes } ->
		maybe_write_string(VeryVerbose,
			"% Detecting streams in "),
		maybe_write_string(VeryVerbose, NameStr),
		maybe_write_string(VeryVerbose, "..."),
		{ rl_stream__detect_streams(Info40, Info50) },
		maybe_write_string(VeryVerbose, "done.\n"),
		rl_opt__maybe_dump_rl(Debug, NameStr,
			"50", "streams", Info50)
	;
		{ Info50 = Info40 }
	),

	{ rl_block__get_proc(Info50, Name, MercuryProcs, Proc) }.

:- pred rl_opt__maybe_dump_rl(bool::in, string::in, string::in, string::in,
		 rl_opt_info::in, io__state::di, io__state::uo) is det.

rl_opt__maybe_dump_rl(no, _ProcName, _StageNum, _StageName, _Info) --> [].
rl_opt__maybe_dump_rl(yes, ProcName, StageNum, StageName, Info) -->
	io__write_string("\n\n%********************************************\n"),
	io__write_strings(["% ", ProcName, "\t", StageNum,
		"\t", StageName, "\n\n"]),
	rl_block__dump_blocks(yes, Info, _),
	io__write_string("%********************************************\n").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
