%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% optimize.m - LLDS to LLDS optimizations.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module optimize.

:- interface.

:- import_module llds, hlds_module.
:- import_module io, list.

:- pred optimize_main(list(c_procedure)::in, global_data::in,
	list(c_procedure)::out, io__state::di, io__state::uo) is det.

:- pred optimize__proc(c_procedure::in, global_data::in,
	c_procedure::out, io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module jumpopt, labelopt, dupelim, peephole.
:- import_module frameopt, delay_slot, value_number, options.
:- import_module globals, passes_aux, opt_util, opt_debug, vn_debug.
:- import_module continuation_info.

:- import_module bool, int, map, bimap, set, std_util.

optimize_main([], _, []) --> [].
optimize_main([Proc0 | Procs0], GlobalData, [Proc | Procs]) -->
	optimize__proc(Proc0, GlobalData, Proc), !,
	optimize_main(Procs0, GlobalData, Procs).

optimize__proc(CProc0, GlobalData, CProc) -->
	{ CProc0 = c_procedure(Name, Arity, PredProcId, Instrs0) },
	globals__io_lookup_bool_option(debug_opt, DebugOpt),
	opt_debug__msg(DebugOpt, "before optimization"),
	opt_debug__dump_instrs(DebugOpt, Instrs0),
	globals__io_lookup_int_option(optimize_repeat, AllRepeat),
	globals__io_lookup_int_option(optimize_vnrepeat, VnRepeat),
	globals__io_lookup_bool_option(optimize_value_number, ValueNumber),
	{
		global_data_maybe_get_proc_layout(GlobalData, PredProcId,
			ProcLayout)
	->
		ProcLayout = proc_layout_info(_, _, _, _, _, _, _, LabelMap),
		map__sorted_keys(LabelMap, LayoutLabels),
		set__sorted_list_to_set(LayoutLabels, LayoutLabelSet)
	;
		set__init(LayoutLabelSet)
	},
	( { ValueNumber = yes } ->
		{ NovnRepeat is AllRepeat - VnRepeat },
		optimize__repeat(NovnRepeat, no,  LayoutLabelSet,
			Instrs0, Instrs1),
		optimize__middle(Instrs1, no, LayoutLabelSet, Instrs2),
		optimize__repeat(VnRepeat, yes, LayoutLabelSet,
			Instrs2, Instrs3)
	;
		optimize__repeat(AllRepeat, no,  LayoutLabelSet,
			Instrs0, Instrs1),
		optimize__middle(Instrs1, yes, LayoutLabelSet, Instrs3)
	),
	optimize__last(Instrs3, LayoutLabelSet, Instrs),
	{ CProc = c_procedure(Name, Arity, PredProcId, Instrs) }.

%-----------------------------------------------------------------------------%

:- pred optimize__repeat(int::in, bool::in, set(label)::in,
	list(instruction)::in, list(instruction)::out,
	io__state::di, io__state::uo) is det.

optimize__repeat(Iter0, DoVn, LayoutLabelSet, Instrs0, Instrs) -->
	(
		{ Iter0 > 0 }
	->
		{ Iter1 is Iter0 - 1 },
		( { Iter1 = 0 } ->
			{ Final = yes }
		;
			{ Final = no }
		),
		optimize__repeated(Instrs0, DoVn, Final, LayoutLabelSet,
			Instrs1, Mod),
		( { Mod = yes } ->
			optimize__repeat(Iter1, DoVn, LayoutLabelSet,
				Instrs1, Instrs)
		;
			{ Instrs = Instrs1 }
		)
	;
		{ Instrs = Instrs0 }
	).

	% We short-circuit jump sequences before normal peepholing
	% to create more opportunities for use of the tailcall macro.

:- pred optimize__repeated(list(instruction)::in, bool::in, bool::in,
	set(label)::in, list(instruction)::out, bool::out,
	io__state::di, io__state::uo) is det.

optimize__repeated(Instrs0, DoVn, Final, LayoutLabelSet, Instrs, Mod) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	globals__io_lookup_bool_option(debug_opt, DebugOpt),
	{ opt_util__find_first_label(Instrs0, Label) },
	{ opt_util__format_label(Label, LabelStr) },

	globals__io_lookup_bool_option(optimize_value_number, ValueNumber),
	( { ValueNumber = yes, DoVn = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing value number for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		value_number_main(Instrs0, LayoutLabelSet, Instrs1),
		( { Instrs1 = Instrs0 } ->
			[]
		;
			opt_debug__msg(DebugOpt, "after value numbering"),
			opt_debug__dump_instrs(DebugOpt, Instrs1)
		)
	;
		{ Instrs1 = Instrs0 }
	),
	globals__io_lookup_bool_option(optimize_jumps, Jumpopt),
	globals__io_lookup_bool_option(optimize_fulljumps, FullJumpopt),
	( { Jumpopt = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing jumps for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		{ jumpopt_main(Instrs1, FullJumpopt, Final, Instrs2, Mod1) },
		( { Mod1 = yes } ->
			opt_debug__msg(DebugOpt, "after jump optimization"),
			opt_debug__dump_instrs(DebugOpt, Instrs2)
		;
			[]
		)
	;
		{ Instrs2 = Instrs1 },
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
		{ peephole__optimize(GC_Method, Instrs2, Instrs3, Mod2) },
		( { Mod2 = yes } ->
			opt_debug__msg(DebugOpt, "after peepholing"),
			opt_debug__dump_instrs(DebugOpt, Instrs3)
		;
			[]
		)
	;
		{ Instrs3 = Instrs2 },
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
		{ labelopt_main(Instrs3, Final, LayoutLabelSet,
			Instrs4, Mod3) },
		( { Mod3 = yes } ->
			opt_debug__msg(DebugOpt, "after label optimization"),
			opt_debug__dump_instrs(DebugOpt, Instrs4)
		;
			[]
		)
	;
		{ Instrs4 = Instrs3 },
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
		{ dupelim_main(Instrs4, Instrs) },
		( { Instrs = Instrs4 } ->
			[]
		;
			opt_debug__msg(DebugOpt, "after duplicate elimination"),
			opt_debug__dump_instrs(DebugOpt, Instrs)
		)
	;
		{ Instrs = Instrs4 }
	),
	{ Mod1 = no, Mod2 = no, Mod3 = no, Instrs = Instrs0 ->
		Mod = no
	;
		Mod = yes
	},
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics).

:- pred optimize__middle(list(instruction)::in, bool::in, set(label)::in,
	list(instruction)::out, io__state::di, io__state::uo) is det.

optimize__middle(Instrs0, Final, LayoutLabelSet, Instrs) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	globals__io_lookup_bool_option(debug_opt, DebugOpt),
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
		{ frameopt_main(Instrs0, Instrs1, Mod1, Jumps) },
		( { Mod1 = yes } ->
			opt_debug__msg(DebugOpt, "after frame optimization"),
			opt_debug__dump_instrs(DebugOpt, Instrs1)
		;
			[]
		),
		globals__io_lookup_bool_option(optimize_fulljumps, FullJumpopt),
		( { Jumps = yes, FullJumpopt = yes } ->
			( { VeryVerbose = yes } ->
				io__write_string("% Optimizing jumps for "),
				io__write_string(LabelStr),
				io__write_string("\n")
			;
				[]
			),
			{ jumpopt_main(Instrs1, FullJumpopt, Final,
				Instrs2, Mod2) },
			( { Mod2 = yes } ->
				opt_debug__msg(DebugOpt, "after jump optimization"),
				opt_debug__dump_instrs(DebugOpt, Instrs2)
			;
				[]
			)
		;
			{ Instrs2 = Instrs1 }
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
				Instrs, Mod3) },
			( { Mod3 = yes } ->
				opt_debug__msg(DebugOpt, "after label optimization"),
				opt_debug__dump_instrs(DebugOpt, Instrs)
			;
				[]
			)
		;
			{ Instrs = Instrs2 }
		)
	;
		{ Instrs = Instrs0 }
	).

:- pred optimize__last(list(instruction)::in, set(label)::in,
	list(instruction)::out, io__state::di, io__state::uo) is det.

optimize__last(Instrs0, LayoutLabelSet, Instrs) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	globals__io_lookup_bool_option(debug_opt, DebugOpt),
	{ opt_util__find_first_label(Instrs0, Label) },
	{ opt_util__format_label(Label, LabelStr) },

	globals__io_lookup_bool_option(optimize_delay_slot, DelaySlot),
	globals__io_lookup_bool_option(optimize_value_number, ValueNumber),
	( { DelaySlot = yes ; ValueNumber = yes } ->
		% We must get rid of any extra labels added by other passes,
		% since they can confuse both post_value_number and delay_slot.
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing labels for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		{ labelopt_main(Instrs0, no, LayoutLabelSet, Instrs1, Mod1) },
		( { Mod1 = yes } ->
			opt_debug__msg(DebugOpt, "after label optimization"),
			opt_debug__dump_instrs(DebugOpt, Instrs1)
		;
			[]
		)
	;
		{ Instrs1 = Instrs0 }
	),
	( { DelaySlot = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing delay slot for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		{ fill_branch_delay_slot(Instrs1, Instrs2) },
		( { Instrs1 = Instrs0 } ->
			opt_debug__msg(DebugOpt, "after delay slot filling"),
			opt_debug__dump_instrs(DebugOpt, Instrs2)
		;
			[]
		)
	;
		{ Instrs2 = Instrs1 }
	),
	( { ValueNumber = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing post value number for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		{ value_number__post_main(Instrs2, Instrs) },
		( { Instrs = Instrs2 } ->
			[]
		;
			opt_debug__msg(DebugOpt, "after post value number"),
			opt_debug__dump_instrs(DebugOpt, Instrs)
		)
	;
		{ Instrs = Instrs1 }
	).
