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

:- import_module llds, io, options.

:- pred optimize__main(list(c_procedure), list(c_procedure),
	io__state, io__state).
:- mode optimize__main(in, out, di, uo) is det.

:- pred optimize__proc(c_procedure, c_procedure, io__state, io__state).
:- mode optimize__proc(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module jumpopt, labelopt, dupelim, frameopt, peephole, value_number.
:- import_module globals, opt_util, opt_debug.
:- import_module list, map, bimap, int, std_util.

optimize__main([], []) --> [].
optimize__main([Proc0|Procs0], [Proc|Procs]) -->
	optimize__proc(Proc0, Proc),
	optimize__main(Procs0, Procs).

optimize__proc(c_procedure(Name, Arity, Mode, Instrs0),
		   c_procedure(Name, Arity, Mode, Instrs)) -->
	globals__io_lookup_int_option(optimize_repeat, AllRepeat),
	globals__io_lookup_int_option(optimize_vnrepeat, VnRepeat),
	{ NovnRepeat is AllRepeat - VnRepeat },
	optimize__repeat(NovnRepeat, no,  Instrs0, Instrs1),
	optimize__repeat(VnRepeat, yes, Instrs1, Instrs2),
	optimize__nonrepeat(Instrs2, Instrs),
#if NU_PROLOG
	{ putprop(opt, opt, Instrs) },
	{ fail }.
optimize__proc(c_procedure(Name, Arity, Mode, Instrs0),
		   c_procedure(Name, Arity, Mode, Instrs)) -->
	{ getprop(opt, opt, Instrs, Ref) },
	{ erase(Ref) },
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),
#endif
	{ true }.

%-----------------------------------------------------------------------------%

:- pred optimize__repeat(int, bool, list(instruction), list(instruction),
	io__state, io__state).
:- mode optimize__repeat(in, in, in, out, di, uo) is det.

optimize__repeat(Iter0, DoVn, Instrs0, Instrs) -->
	(
		{ Iter0 > 0 }
	->
		{ bimap__init(TeardownMap) },
		optimize__repeated(Instrs0, DoVn, no, TeardownMap,
			Instrs1, Mod),
		( { Mod = yes } ->
			{ Iter1 is Iter0 - 1 },
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
	bimap(label, label), list(instruction), bool, io__state, io__state).
:- mode optimize__repeated(in, in, in, in, out, out, di, uo) is det.

optimize__repeated(Instrs0, DoVn, Final, TeardownMap, Instrs, Mod) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
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
		value_number__main(Instrs0, Instrs1)
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
		{ jumpopt__main(Instrs1, FullJumpopt, Final, Instrs2, Mod1) }
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
		{ peephole__main(Instrs2, Instrs3, TeardownMap, Mod2) }
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
		{ labelopt__main(Instrs3, Final, Instrs4, Mod3) }
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
		{ dupelim__main(Instrs4, Instrs) }
	;
		{ Instrs = Instrs4 }
	),
	{ Mod1 = no, Mod2 = no, Mod3 = no, Instrs = Instrs0 ->
		Mod = no
	;
		Mod = yes
	},
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),
#if NU_PROLOG
	{ putprop(opt, opt, Instrs - Mod) },
	{ fail }.
optimize__repeated(Instrs0, _, _, Instrs, Mod) -->
	{ getprop(opt, opt, Instrs - Mod, Ref) },
	{ erase(Ref) },
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),
#endif
	{ true }.

:- pred optimize__nonrepeat(list(instruction), list(instruction),
	io__state, io__state).
:- mode optimize__nonrepeat(in, out, di, uo) is det.

optimize__nonrepeat(Instrs0, Instrs) -->
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
		{ frameopt__main(Instrs0, Instrs1, TeardownMap, Mod1) }
	;
		{ Instrs1 = Instrs0 },
		{ bimap__init(TeardownMap) },
		{ Mod1 = no }
	),
	globals__io_lookup_bool_option(optimize_peep, Peephole),
	( { FrameOpt = yes, Peephole = yes } ->
		% get rid of useless incr_sp/decr_sp pairs
		{ bimap__init(Empty1) },
		{ peephole__main(Instrs1, Instrs2, Empty1, _) }
	;
		{ Instrs2 = Instrs1 }
	),
	globals__io_lookup_bool_option(optimize_value_number, ValueNumber),
	( { ValueNumber = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing post value number for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		{ value_number__post_main(Instrs2, Instrs3) }
	;
		{ Instrs3 = Instrs2 }
	),
	( { FrameOpt = yes ; ValueNumber = yes } ->
		optimize__repeated(Instrs3, no, yes, TeardownMap,
			Instrs4, RepMod),
		( { RepMod = yes, FrameOpt = yes, Peephole = yes } ->
			{ bimap__init(Empty2) },
			{ peephole__main(Instrs4, Instrs5, Empty2, _) }
		;
			{ Instrs5 = Instrs4 }
		),
		( { frameopt__is_succip_restored(Instrs4) } ->
			{ Instrs = Instrs5 }
		;
			{ frameopt__dont_save_succip(Instrs5, Instrs) }
		)
	;
		{ Instrs = Instrs3 }
	).
