%-----------------------------------------------------------------------------%

% optimize.nl - LLDS to LLDS optimizations.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module optimize.

:- interface.

:- import_module llds, io, options.

:- pred optimize__main(c_file, c_file, io__state, io__state).
:- mode optimize__main(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list, globals, opt_util.

:- import_module jumpopt, labelopt, frameopt, peephole, value_number.
:- import_module int, std_util.

	% Boring LLDS traversal code.

optimize__main(c_file(Name, Modules0), c_file(Name, Modules)) -->
	optimize__module_list(Modules0, Modules).

:- pred optimize__module_list(list(c_module), list(c_module),
	io__state, io__state).
:- mode optimize__module_list(in, out, di, uo) is det.

optimize__module_list([], []) --> [].
optimize__module_list([M0|Ms0], [M|Ms]) -->
	optimize__module(M0, M),
	optimize__module_list(Ms0, Ms).

:- pred optimize__module(c_module, c_module, io__state, io__state).
:- mode optimize__module(in, out, di, uo) is det.

optimize__module(c_module(Name, Procs0), c_module(Name, Procs)) -->
	optimize__proc_list(Procs0, Procs).

:- pred optimize__proc_list(list(c_procedure), list(c_procedure),
	io__state, io__state).
:- mode optimize__proc_list(in, out, di, uo) is det.

optimize__proc_list([], []) --> [].
optimize__proc_list([P0|Ps0], [P|Ps]) -->
	optimize__proc(P0, P),
	optimize__proc_list(Ps0, Ps).

	% We short-circuit jump sequences before normal peepholing
	% to create more opportunities for use of the tailcall macro.

:- pred optimize__proc(c_procedure, c_procedure, io__state, io__state).
:- mode optimize__proc(in, out, di, uo) is det.

optimize__proc(c_procedure(Name, Arity, Mode, Instructions0),
		   c_procedure(Name, Arity, Mode, Instructions)) -->
	optimize__repeat(0, Instructions0, Instructions1),
	optimize__nonrepeat(Instructions1, Instructions),
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

:- pred optimize__repeat(int, list(instruction), list(instruction),
	io__state, io__state).
:- mode optimize__repeat(in, in, out, di, uo) is det.

optimize__repeat(Iter, Instructions0, Instructions) -->
	optimize__repeated(Instructions0, Instructions1, Mod),
	globals__io_lookup_int_option(optimize_repeat, Repeat),
	(
		{ Iter < Repeat },
		{ Mod = yes }
	->
		{ Iter1 is Iter + 1 },
		optimize__repeat(Iter1, Instructions1, Instructions)
	;
		{ Instructions = Instructions1 }
	).

:- pred optimize__repeated(list(instruction), list(instruction), bool,
	io__state, io__state).
:- mode optimize__repeated(in, out, out, di, uo) is det.

optimize__repeated(Instructions0, Instructions, Mod) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	{ opt_util__find_first_label(Instructions0, Label) },
	{ opt_util__format_label(Label, LabelStr) },

	globals__io_lookup_bool_option(optimize_jumps, Jumpopt),
	( { Jumpopt = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing jumps for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		),
		{ jumpopt__main(Instructions0, Instructions1, Mod1) }
	;
		{ Instructions1 = Instructions0 },
		{ Mod1 = no }
	),
	globals__io_lookup_bool_option(optimize_peep, Local),
	( { Local = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing locally for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		),
		{ peephole__main(Instructions1, Instructions2, Mod2) }
	;
		{ Instructions2 = Instructions1 },
		{ Mod2 = no }
	),
	globals__io_lookup_bool_option(optimize_labels, LabelElim),
	( { LabelElim = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing labels for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		),
		{ labelopt__main(Instructions2, Instructions, Mod3) }
	;
		{ Instructions = Instructions2 },
		{ Mod3 = no }
	),
	{ Mod1 = no, Mod2 = no, Mod3 = no ->
		Mod = no
	;
		Mod = yes
	},
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),
#if NU_PROLOG
	{ putprop(opt, opt, Instructions - Mod) },
	{ fail }.
optimize__repeated(Instructions0, Instructions, Mod) -->
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

	globals__io_lookup_bool_option(optimize_value_number, ValueNumber),
	( { ValueNumber = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing value number for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		),
		{ value_number__main(Instructions0, Instructions1) }
	;
		{ Instructions1 = Instructions0 }
	),
	globals__io_lookup_bool_option(optimize_frames, FrameOpt),
	( { FrameOpt = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing frames for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		),
		{ frameopt__main(Instructions1, Instructions2, Mod1) }
	;
		{ Instructions2 = Instructions1 },
		{ Mod1 = no }
	),
	( { ValueNumber = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing post value number for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		),
		{ value_number__post_main(Instructions2, Instructions3) }
	;
		{ Instructions3 = Instructions2 }
	),
	( { Mod1 = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing locally for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		),
		{ peephole__main(Instructions3, Instructions4, _) }
	;
		{ Instructions4 = Instructions3 }
	),
	( { Mod1 = yes } ->
		( { VeryVerbose = yes } ->
			io__write_string("% Optimizing labels for "),
			io__write_string(LabelStr),
			io__write_string("\n")
		),
		{ labelopt__main(Instructions4, Instructions, _) }
	;
		{ Instructions = Instructions4 }
	).
