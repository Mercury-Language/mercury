%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% optimize.m - LLDS to LLDS optimizations.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module optimize.

:- interface.

:- import_module llds, io.

:- pred optimize__main(list(c_procedure), list(c_procedure),
	io__state, io__state).
:- mode optimize__main(in, out, di, uo) is det.

:- pred optimize__proc(c_procedure, c_procedure, io__state, io__state).
:- mode optimize__proc(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool, list, map, bimap, int, std_util.

:- import_module jumpopt, labelopt, dupelim, peephole.
:- import_module frameopt, delay_slot, value_number, options.
:- import_module globals, passes_aux, opt_util, opt_debug, vn_debug.

optimize__main([], []) --> [].
optimize__main([Proc0|Procs0], [Proc|Procs]) -->
	optimize__proc(Proc0, Proc), !,
	optimize__main(Procs0, Procs).

optimize__proc(c_procedure(Name, Arity, Mode, Instrs0),
		   c_procedure(Name, Arity, Mode, Instrs)) -->
	globals__io_lookup_bool_option(debug_opt, DebugOpt),
	opt_debug__msg(DebugOpt, "before optimization"),
	opt_debug__dump_instrs(DebugOpt, Instrs0),
	globals__io_lookup_int_option(optimize_repeat, AllRepeat),
	globals__io_lookup_int_option(optimize_vnrepeat, VnRepeat),
	globals__io_lookup_bool_option(optimize_value_number, ValueNumber),
	( { ValueNumber = yes } ->
		{ NovnRepeat is AllRepeat - VnRepeat },
		optimize__repeat(NovnRepeat, no,  Instrs0, Instrs1),
		optimize__middle(Instrs1, no, Instrs2),
		optimize__repeat(VnRepeat, yes, Instrs2, Instrs3)
	;
		optimize__repeat(AllRepeat, no,  Instrs0, Instrs1),
		optimize__middle(Instrs1, yes, Instrs3)
	),
	optimize__last(Instrs3, Instrs).

%-----------------------------------------------------------------------------%

:- pred optimize__repeat(int, bool, list(instruction), list(instruction),
	io__state, io__state).
:- mode optimize__repeat(in, in, in, out, di, uo) is det.

optimize__repeat(Iter0, DoVn, Instrs0, Instrs) -->
	(
		{ Iter0 > 0 }
	->
		{ Iter1 is Iter0 - 1 },
		( { Iter1 = 0 } ->
			{ Final = yes }
		;
			{ Final = no }
		),
		optimize__repeated(Instrs0, DoVn, Final, Instrs1, Mod),
		( { Mod = yes } ->
			optimize__repeat(Iter1, DoVn, Instrs1, Instrs)
		;
			{ Instrs = Instrs1 }
		)
	;
		{ Instrs = Instrs0 }
	).

	% We short-circuit jump sequences before normal peepholing
	% to create more opportunities for use of the tailcall macro.

:- pred optimize__repeated(list(instruction), bool, bool,
	list(instruction), bool, io__state, io__state).
:- mode optimize__repeated(in, in, in, out, out, di, uo) is det.

optimize__repeated(Instrs0, DoVn, Final, Instrs, Mod) -->
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
		value_number__main(Instrs0, Instrs1),
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
		{ jumpopt__main(Instrs1, FullJumpopt, Final, Instrs2, Mod1) },
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
		{ peephole__optimize(Instrs2, Instrs3, Mod2) },
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
		{ labelopt__main(Instrs3, Final, Instrs4, Mod3) },
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

:- pred optimize__middle(list(instruction), bool, list(instruction),
	io__state, io__state).
:- mode optimize__middle(in, in, out, di, uo) is det.

optimize__middle(Instrs0, Final, Instrs) -->
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
		{ frameopt__main(Instrs0, Instrs1, Mod1, Jumps) },
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
			{ jumpopt__main(Instrs1, FullJumpopt, Final, Instrs2, Mod2) },
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
			{ labelopt__main(Instrs2, Final, Instrs, Mod3) },
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

:- pred optimize__last(list(instruction), list(instruction),
	io__state, io__state).
:- mode optimize__last(in, out, di, uo) is det.

optimize__last(Instrs0, Instrs) -->
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
		{ labelopt__main(Instrs0, no, Instrs1, Mod1) },
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
