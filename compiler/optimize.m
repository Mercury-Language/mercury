%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% optimize.m - LLDS to LLDS optimizations.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module ll_backend__optimize.

:- interface.

:- import_module ll_backend__llds.
:- import_module io, list.

:- pred optimize_main(list(c_procedure)::in, global_data::in,
	list(c_procedure)::out, io__state::di, io__state::uo) is det.

:- pred optimize__proc(c_procedure::in, global_data::in,
	c_procedure::out, io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_pred, hlds__passes_aux.
:- import_module ll_backend__jumpopt, ll_backend__labelopt.
:- import_module ll_backend__dupelim, ll_backend__peephole.
:- import_module ll_backend__frameopt, ll_backend__delay_slot.
:- import_module ll_backend__use_local_vars, ll_backend__reassign.
:- import_module ll_backend__opt_util, ll_backend__opt_debug.
:- import_module ll_backend__wrap_blocks.
:- import_module ll_backend__llds_out, ll_backend__continuation_info.
:- import_module libs__options, libs__globals.

:- import_module bool, int, string.
:- import_module map, bimap, set, std_util, require, counter.

optimize_main([], _, []) --> [].
optimize_main([Proc0 | Procs0], GlobalData, [Proc | Procs]) -->
	optimize__proc(Proc0, GlobalData, Proc),
	optimize_main(Procs0, GlobalData, Procs).

optimize__proc(CProc0, GlobalData, CProc) -->
	{ CProc0 = c_procedure(Name, Arity, PredProcId, Instrs0,
		ProcLabel, C0, MayAlterRtti) },
	optimize__init_opt_debug_info(Name, Arity, PredProcId, Instrs0, C0,
		OptDebugInfo0),
	globals__io_lookup_int_option(optimize_repeat, Repeat),
	{
		global_data_maybe_get_proc_layout(GlobalData, PredProcId,
			ProcLayout)
	->
		LabelMap = ProcLayout^internal_map,
		map__sorted_keys(LabelMap, LayoutLabels),
		set__sorted_list_to_set(LayoutLabels, LayoutLabelSet)
	;
		set__init(LayoutLabelSet)
	},
	optimize__repeat(Repeat, LayoutLabelSet, Instrs0, ProcLabel,
		MayAlterRtti, C0, C1, OptDebugInfo0, OptDebugInfo1, Instrs1),
	optimize__middle(Instrs1, yes, LayoutLabelSet, ProcLabel,
		MayAlterRtti, C1, C, OptDebugInfo1, OptDebugInfo, Instrs3),
	optimize__last(Instrs3, LayoutLabelSet, C, OptDebugInfo, Instrs),
	{ CProc = c_procedure(Name, Arity, PredProcId, Instrs,
		ProcLabel, C, MayAlterRtti) }.

%-----------------------------------------------------------------------------%

:- type opt_debug_info
	--->	opt_debug_info(
			string,			% base file name
			int			% last file number written
		)
	;	no_opt_debug_info.

:- pred optimize__init_opt_debug_info(string::in, int::in, pred_proc_id::in,
	list(instruction)::in, counter::in, opt_debug_info::out,
	io__state::di, io__state::uo) is det.

optimize__init_opt_debug_info(Name, Arity, PredProcId, Instrs0, Counter,
		OptDebugInfo) -->
	globals__io_lookup_bool_option(debug_opt, DebugOpt),
	(
		{ DebugOpt = yes },
		{ llds_out__name_mangle(Name, MangledName) },
		{ PredProcId = proc(PredId, ProcId) },
		{ pred_id_to_int(PredId, PredIdInt) },
		{ proc_id_to_int(ProcId, ProcIdInt) },
		{ string__int_to_string(Arity, ArityStr) },
		{ string__int_to_string(PredIdInt, PredIdStr) },
		{ string__int_to_string(ProcIdInt, ProcIdStr) },
		{ string__append_list([MangledName, "_", ArityStr,
			".pred", PredIdStr, ".proc", ProcIdStr], BaseName) },
		{ OptDebugInfo = opt_debug_info(BaseName, 0) },

		{ string__append_list([BaseName, ".opt0"], FileName) },
		io__open_output(FileName, Res),
		( { Res = ok(FileStream) } ->
			io__set_output_stream(FileStream, OutputStream),
			{ counter__allocate(NextLabel, Counter, _) },
			opt_debug__msg(yes, NextLabel, "before optimization"),
			opt_debug__dump_instrs(yes, Instrs0),
			io__set_output_stream(OutputStream, _),
			io__close_output(FileStream)
		;
			{ string__append("cannot open ", FileName, ErrorMsg) },
			{ error(ErrorMsg) }
		)
	;
		{ DebugOpt = no },
		{ OptDebugInfo = no_opt_debug_info }
	).

:- pred optimize__maybe_opt_debug(list(instruction)::in, counter::in,
	string::in, opt_debug_info::in, opt_debug_info::out,
	io__state::di, io__state::uo) is det.

optimize__maybe_opt_debug(Instrs, Counter, Msg,
		OptDebugInfo0, OptDebugInfo) -->
	(
		{ OptDebugInfo0 = opt_debug_info(BaseName, OptNum0) },
		{ OptNum = OptNum0 + 1 },
		{ string__int_to_string(OptNum0, OptNum0Str) },
		{ string__int_to_string(OptNum, OptNumStr) },
		{ string__append_list([BaseName, ".opt", OptNum0Str],
			OptFileName0) },
		{ string__append_list([BaseName, ".opt", OptNumStr],
			OptFileName) },
		{ string__append_list([BaseName, ".diff", OptNumStr],
			DiffFileName) },
		io__open_output(OptFileName, Res),
		( { Res = ok(FileStream) } ->
			io__set_output_stream(FileStream, OutputStream),
			{ counter__allocate(NextLabel, Counter, _) },
			opt_debug__msg(yes, NextLabel, Msg),
			opt_debug__dump_instrs(yes, Instrs),
			io__set_output_stream(OutputStream, _),
			io__close_output(FileStream)
		;
			{ string__append("cannot open ", OptFileName,
				ErrorMsg) },
			{ error(ErrorMsg) }
		),
		% Although the -u is not fully portable, it is available
		% on all the systems we intend to use it on, and the main user
		% of --debug-opt (zs) strongly prefers -u to -c.
		{ string__append_list(["diff -u ",
			OptFileName0, " ", OptFileName,
			" > ", DiffFileName], DiffCommand) },
		io__call_system(DiffCommand, _),
		{ OptDebugInfo = opt_debug_info(BaseName, OptNum) }
	;
		{ OptDebugInfo0 = no_opt_debug_info },
		{ OptDebugInfo = no_opt_debug_info }
	).

%-----------------------------------------------------------------------------%

:- pred optimize__repeat(int::in, set(label)::in, list(instruction)::in,
	proc_label::in, may_alter_rtti::in, counter::in, counter::out,
	opt_debug_info::in, opt_debug_info::out, list(instruction)::out,
	io__state::di, io__state::uo) is det.

optimize__repeat(Iter0, LayoutLabelSet, Instrs0, ProcLabel, MayAlterRtti,
		C0, C, OptDebugInfo0, OptDebugInfo, Instrs) -->
	( { Iter0 > 0 } ->
		{ Iter1 = Iter0 - 1 },
		( { Iter1 = 0 } ->
			{ Final = yes }
		;
			{ Final = no }
		),
		optimize__repeated(Instrs0, Final, LayoutLabelSet, ProcLabel,
			MayAlterRtti, C0, C1, OptDebugInfo0, OptDebugInfo1,
			Instrs1, Mod),
		( { Mod = yes } ->
			optimize__repeat(Iter1, LayoutLabelSet, Instrs1,
				ProcLabel, MayAlterRtti, C1, C,
				OptDebugInfo1, OptDebugInfo, Instrs)
		;
			{ Instrs = Instrs1 },
			{ C = C1 },
			{ OptDebugInfo = OptDebugInfo0 }
		)
	;
		{ Instrs = Instrs0 },
		{ C = C0 },
		{ OptDebugInfo = OptDebugInfo0 }
	).

	% We short-circuit jump sequences before normal peepholing
	% to create more opportunities for use of the tailcall macro.

:- pred optimize__repeated(list(instruction)::in, bool::in, set(label)::in,
	proc_label::in, may_alter_rtti::in, counter::in, counter::out,
	opt_debug_info::in, opt_debug_info::out, list(instruction)::out,
	bool::out, io__state::di, io__state::uo) is det.

optimize__repeated(Instrs0, Final, LayoutLabelSet, ProcLabel, MayAlterRtti,
		C0, C, OptDebugInfo0, OptDebugInfo, Instrs, Mod) -->
	{ opt_util__find_first_label(Instrs0, Label) },
	{ opt_util__format_label(Label, LabelStr) },
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	globals__io_lookup_bool_option(optimize_jumps, Jumpopt),
	globals__io_lookup_bool_option(optimize_fulljumps, FullJumpopt),
	globals__io_lookup_bool_option(pessimize_tailcalls,
		PessimizeTailCalls),
	globals__io_lookup_bool_option(checked_nondet_tailcalls,
		CheckedNondetTailCalls),
	( { Jumpopt = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing jumps for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		{ jumpopt_main(Instrs0, LayoutLabelSet, MayAlterRtti,
			ProcLabel, C0, C1, FullJumpopt, Final,
			PessimizeTailCalls, CheckedNondetTailCalls,
			Instrs1, Mod1) },
		optimize__maybe_opt_debug(Instrs1, C1, "after jump opt",
			OptDebugInfo0, OptDebugInfo1)
	;
		{ Instrs1 = Instrs0 },
		{ C1 = C0 },
		{ OptDebugInfo1 = OptDebugInfo0 },
		{ Mod1 = no }
	),
	globals__io_lookup_bool_option(optimize_peep, Peephole),
	( { Peephole = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing locally for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		globals__io_get_gc_method(GC_Method),
		{ peephole__optimize(GC_Method, Instrs1, Instrs2, Mod2) },
		optimize__maybe_opt_debug(Instrs2, C1, "after peephole",
			OptDebugInfo1, OptDebugInfo2)
	;
		{ Instrs2 = Instrs1 },
		{ OptDebugInfo2 = OptDebugInfo1 },
		{ Mod2 = no }
	),
	globals__io_lookup_bool_option(optimize_labels, LabelElim),
	( { LabelElim = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing labels for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		{ labelopt_main(Instrs2, Final, LayoutLabelSet,
			Instrs3, Mod3) },
		optimize__maybe_opt_debug(Instrs3, C1, "after label opt",
			OptDebugInfo2, OptDebugInfo3)
		;
		{ Instrs3 = Instrs2 },
		{ OptDebugInfo3 = OptDebugInfo2 },
		{ Mod3 = no }
	),
	globals__io_lookup_bool_option(optimize_dups, DupElim),
	( { DupElim = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing duplicates for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		{ dupelim_main(Instrs3, ProcLabel, C1, C, Instrs) },
		optimize__maybe_opt_debug(Instrs, C, "after duplicates",
			OptDebugInfo3, OptDebugInfo)
	;
		{ Instrs = Instrs3 },
		{ OptDebugInfo = OptDebugInfo3 },
		{ C = C1 }
	),
	{ Mod1 = no, Mod2 = no, Mod3 = no, Instrs = Instrs0 ->
		Mod = no
	;
		Mod = yes
	},
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics).

:- pred optimize__middle(list(instruction)::in, bool::in, set(label)::in,
	proc_label::in, may_alter_rtti::in, counter::in, counter::out,
	opt_debug_info::in, opt_debug_info::out, list(instruction)::out,
	io__state::di, io__state::uo) is det.

optimize__middle(Instrs0, Final, LayoutLabelSet, ProcLabel, MayAlterRtti,
		C0, C, OptDebugInfo0, OptDebugInfo, Instrs) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	{ opt_util__find_first_label(Instrs0, Label) },
	{ opt_util__format_label(Label, LabelStr) },

	globals__io_lookup_bool_option(optimize_frames, FrameOpt),
	( { FrameOpt = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing frames for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		{ frameopt_main(Instrs0, ProcLabel, C0, C1, Instrs1,
			Mod1, Jumps) },
		optimize__maybe_opt_debug(Instrs1, C1, "after frame opt",
			OptDebugInfo0, OptDebugInfo1),
		globals__io_lookup_bool_option(optimize_fulljumps, FullJumpopt),
		globals__io_lookup_bool_option(pessimize_tailcalls,
			PessimizeTailCalls),
		globals__io_lookup_bool_option(checked_nondet_tailcalls,
			CheckedNondetTailCalls),
		( { Jumps = yes, FullJumpopt = yes } ->
			( { VeryVerbose = yes } ->
				io__write_string("% Optimizing jumps for "),
				io__write_string(LabelStr),
				io__write_string("\n")
			;
				[]
			),
			{ jumpopt_main(Instrs1, LayoutLabelSet, MayAlterRtti,
				ProcLabel, C1, C2, FullJumpopt, Final,
				PessimizeTailCalls, CheckedNondetTailCalls,
				Instrs2, _Mod2) },
			optimize__maybe_opt_debug(Instrs2, C2, "after jumps",
				OptDebugInfo1, OptDebugInfo2)
		;
			{ Instrs2 = Instrs1 },
			{ OptDebugInfo2 = OptDebugInfo1 },
			{ C2 = C1 }
		),
		( { Mod1 = yes } ->
			( { VeryVerbose = yes } ->
				io__write_string("% Optimizing labels for "),
				io__write_string(LabelStr),
				io__write_string("\n")
			;
				[]
			),
			{ labelopt_main(Instrs2, Final, LayoutLabelSet,
				Instrs3, _Mod3) },
			optimize__maybe_opt_debug(Instrs3, C2, "after labels",
				OptDebugInfo2, OptDebugInfo3)
			;
			{ OptDebugInfo3 = OptDebugInfo2 },
			{ Instrs3 = Instrs2 }
		)
	;
		{ Instrs3 = Instrs0 },
		{ OptDebugInfo3 = OptDebugInfo0 },
		{ C2 = C0 }
	),
	globals__io_lookup_bool_option(use_local_vars, UseLocalVars),
	(
		{ UseLocalVars = yes },
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing local vars for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		globals__io_lookup_int_option(num_real_r_regs, NumRealRRegs),
		globals__io_lookup_int_option(local_var_access_threshold,
			AccessThreshold),
		{ use_local_vars__main(Instrs3, Instrs,
			ProcLabel, NumRealRRegs, AccessThreshold, C2, C) },
		optimize__maybe_opt_debug(Instrs, C, "after use_local_vars",
			OptDebugInfo3, OptDebugInfo)
	;
		{ UseLocalVars = no },
		{ Instrs = Instrs3 },
		{ OptDebugInfo = OptDebugInfo3 },
		{ C = C2 }
	).

:- pred optimize__last(list(instruction)::in, set(label)::in,
	counter::in, opt_debug_info::in, list(instruction)::out,
	io__state::di, io__state::uo) is det.

optimize__last(Instrs0, LayoutLabelSet, C, OptDebugInfo0, Instrs) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	{ opt_util__find_first_label(Instrs0, Label) },
	{ opt_util__format_label(Label, LabelStr) },

	globals__io_lookup_bool_option(optimize_reassign, Reassign),
	globals__io_lookup_bool_option(optimize_delay_slot, DelaySlot),
	globals__io_lookup_bool_option(use_local_vars, UseLocalVars),
	( { Reassign = yes ; DelaySlot = yes ; UseLocalVars = yes } ->
		% We must get rid of any extra labels added by other passes,
		% since they can confuse reassign, wrap_blocks and delay_slot.
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing labels for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		{ labelopt_main(Instrs0, no, LayoutLabelSet, Instrs1, _Mod1) },
		optimize__maybe_opt_debug(Instrs1, C, "after label opt",
			OptDebugInfo0, OptDebugInfo1)
	;
		{ OptDebugInfo1 = OptDebugInfo0 },
		{ Instrs1 = Instrs0 }
	),
	( { Reassign = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing reassign for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		{ remove_reassign(Instrs1, Instrs2) },
		optimize__maybe_opt_debug(Instrs2, C, "after reassign",
			OptDebugInfo1, OptDebugInfo2)
	;
		{ OptDebugInfo2 = OptDebugInfo1 },
		{ Instrs2 = Instrs1 }
	),
	( { DelaySlot = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing delay slot for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		{ fill_branch_delay_slot(Instrs2, Instrs3) },
		optimize__maybe_opt_debug(Instrs3, C, "after delay slots",
			OptDebugInfo2, OptDebugInfo3)
	;
		{ OptDebugInfo3 = OptDebugInfo2 },
		{ Instrs3 = Instrs2 }
	),
	( { UseLocalVars = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Wrapping blocks for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		{ wrap_blocks(Instrs3, Instrs) },
		optimize__maybe_opt_debug(Instrs, C, "after wrap blocks",
			OptDebugInfo3, _OptDebugInfo)
	;
		{ Instrs = Instrs3 }
	).
