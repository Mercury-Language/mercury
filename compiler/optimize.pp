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

:- import_module list, globals, opt_util.

:- import_module jumpopt, labelopt, dupelim, frameopt, peephole, value_number.
:- import_module int, std_util.

optimize__main([], []) --> [].
optimize__main([Proc0|Procs0], [Proc|Procs]) -->
	optimize__proc(Proc0, Proc),
	optimize__main(Procs0, Procs).

optimize__proc(c_procedure(Name, Arity, Mode, Instructions0),
		   c_procedure(Name, Arity, Mode, Instructions)) -->
	globals__io_lookup_int_option(optimize_repeat, AllRepeat),
	globals__io_lookup_int_option(optimize_vnrepeat, VnRepeat),
	{ NovnRepeat is AllRepeat - VnRepeat },
	optimize__repeat(NovnRepeat, no,  Instructions0, Instructions1),
	optimize__repeat(VnRepeat, yes, Instructions1, Instructions2),
	optimize__nonrepeat(Instructions2, Instructions),
#if NU_PROLOG
	{ putprop(opt, opt, Instructions) },
	{ fail }.
optimize__proc(c_procedure(Name, Arity, Mode, Instructions0),
		   c_procedure(Name, Arity, Mode, Instructions)) -->
	{ getprop(opt, opt, Instructions, Ref) },
	{ erase(Ref) },
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),
#endif
	{ true }.

%-----------------------------------------------------------------------------%

:- pred optimize__repeat(int, bool, list(instruction), list(instruction),
	io__state, io__state).
:- mode optimize__repeat(in, in, in, out, di, uo) is det.

optimize__repeat(Iter0, DoVn, Instructions0, Instructions) -->
	(
		{ Iter0 > 0 }
	->
		optimize__repeated(Instructions0, DoVn, Instructions1, Mod),
		( { Mod = yes } ->
			{ Iter1 is Iter0 - 1 },
			optimize__repeat(Iter1, DoVn, Instructions1,
				Instructions)
		;
			{ Instructions = Instructions1 }
		)
	;
		{ Instructions = Instructions0 }
	).

	% We short-circuit jump sequences before normal peepholing
	% to create more opportunities for use of the tailcall macro.

:- pred optimize__repeated(list(instruction), bool, list(instruction), bool,
	io__state, io__state).
:- mode optimize__repeated(in, in, out, out, di, uo) is det.

optimize__repeated(Instructions0, DoVn, Instructions, Mod) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	{ opt_util__find_first_label(Instructions0, Label) },
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
		value_number__main(Instructions0, Instructions1)
	;
		{ Instructions1 = Instructions0 }
	),
	globals__io_lookup_bool_option(optimize_jumps, Jumpopt),
	( { Jumpopt = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing jumps for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		;
			[]
		),
		{ jumpopt__main(Instructions1, Instructions2, Mod1) }
	;
		{ Instructions2 = Instructions1 },
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
		{ peephole__main(Instructions2, Instructions3, Mod2) }
	;
		{ Instructions3 = Instructions2 },
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
		{ labelopt__main(Instructions3, Instructions4, Mod3) }
	;
		{ Instructions4 = Instructions3 },
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
		{ dupelim__main(Instructions4, Instructions) }
	;
		{ Instructions = Instructions4 }
	),
	{ Mod1 = no, Mod2 = no, Mod3 = no, Instructions = Instructions0 ->
		Mod = no
	;
		Mod = yes
	},
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),
#if NU_PROLOG
	{ putprop(opt, opt, Instructions - Mod) },
	{ fail }.
optimize__repeated(Instructions0, _, Instructions, Mod) -->
	{ getprop(opt, opt, Instructions - Mod, Ref) },
	{ erase(Ref) },
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),
#endif
	{ true }.

:- pred optimize__nonrepeat(list(instruction), list(instruction),
	io__state, io__state).
:- mode optimize__nonrepeat(in, out, di, uo) is det.

optimize__nonrepeat(Instructions0, Instructions) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	{ opt_util__find_first_label(Instructions0, Label) },
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
		{ frameopt__main(Instructions0, Instructions1, Mod1) }
	;
		{ Instructions1 = Instructions0 },
		{ Mod1 = no }
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
		{ value_number__post_main(Instructions1, Instructions2) }
	;
		{ Instructions2 = Instructions1 }
	),
	( { FrameOpt = yes ; ValueNumber = yes } ->
		optimize__repeated(Instructions2, no, Instructions3, RedMod),
		globals__io_lookup_bool_option(optimize_peep, Peephole),
		( { RepMod = yes, FrameOpt = yes, Peephole = yes } ->
			{ peephole__main(Instructions3, Instructions, _) }
		;
			{ Instructions = Instructions3 }
		)
	;
		{ Instructions = Instructions2 }
	).
