%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: options.m.
% Main author: fjh.

% This defines the stuff necessary so that getopt.m
% can parse the command-line options.
% When we implement higher-order preds, this and 
% getopt.m should be rewritten to use them.
% Currently the interface dependencies are very hairy.

% IMPORTANT NOTE: any changes to the options should be
% reflected in both the help message produced below,
% and in the Mercury User's Guide (../doc/user_guide.texi).

%-----------------------------------------------------------------------------%

:- module options.
:- interface.
:- import_module int, string, std_util, list, io, map, require.

:- type option_data	--->	bool(bool)
			;	int(int)
			;	string(string)
			;	accumulating(list(string)).

:- type option_table	==	map(option, option_data).
		
:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_defaults(list(pair(option, option_data))::out) is det.

:- pred options__lookup_bool_option(option_table, option, bool).
:- mode options__lookup_bool_option(in, in, out) is det.

:- pred options__lookup_int_option(option_table, option, int).
:- mode options__lookup_int_option(in, in, out) is det.

:- pred options__lookup_string_option(option_table, option, string).
:- mode options__lookup_string_option(in, in, out) is det.

:- pred options_help(io__state::di, io__state::uo) is det.

% A couple of misc utilities

:- pred maybe_report_stats(bool::in, io__state::di, io__state::uo) is det.
:- pred maybe_write_string(bool::input, string::input,
			io__state::di, io__state::uo) is det.
:- pred maybe_flush_output(bool::in, io__state::di, io__state::uo) is det.

:- type option	
	% Warning options
		--->	warn_singleton_vars
		;	warn_missing_det_decls
		;	warn_det_decls_too_lax
	% Verbosity options
		;	verbose
		;	very_verbose
		;	verbose_errors
		;	statistics
		;	debug_types
		;	debug_modes
		;	vndebug
	% Output options
		;	make_interface
		;	make_call_graph
		;	convert_to_mercury
		;	convert_to_goedel
		;	modecheck
		;	dump_hlds
		;	verbose_dump_hlds
		;	generate_code
		;	compile
		;	link
		;	line_numbers
		;	mod_comments
	% Code generation options
		;	trad_passes
		;	lazy_code
		;	polymorphism
		;	reclaim_heap_on_semidet_failure
		;	reclaim_heap_on_nondet_failure
		;	generate_dependencies
		;	tags
		;	num_tag_bits
		;	follow_code
		;	follow_vars
		;	gc
		;	cc
		;	cflags
		;	c_include_directory
		;	gcc_non_local_gotos
		;	gcc_global_registers
		;	asm_labels
	% Optimisation Options
		;	optimize
		;	optimize_peep
		;	optimize_jumps
		;	optimize_fulljumps
		;	optimize_labels
		;	optimize_dups
		;	optimize_value_number
		;	optimize_frames
		;	optimize_repeat
		;	optimize_vnrepeat
		;	pred_value_number
		;	static_ground_terms
		;	smart_indexing
		;	req_density
		;	dense_switch_size
		;	string_switch_size
		;	tag_switch_size
		;	middle_rec
		;	inlining
		;	common_struct
		;	common_goal
		;	c_optimize
		;	debug
		;	grade
		;	procs_per_c_function
        % Debugger Options
                ;       debugger
	% Miscellaneous Options
		;	builtin_module
		;	heap_space
		;	search_directories
		;	help.

:- implementation.

	% I've split up option_defaults into several different clauses
	% purely in order to reduce compilation time/memory usage.

:- type option_category
	--->	warning_option
	;	verbosity_option
	;	output_option
	;	code_gen_option
	;	optimization_option
        ;       debugger_option
	;	miscellaneous_option.

option_defaults(OptionDefaults) :-
	option_defaults_2(warning_option, WarningOptions),
	option_defaults_2(verbosity_option, VerbosityOptions),
	option_defaults_2(output_option, OutputOptions),
	option_defaults_2(code_gen_option, CodeGenOptions),
	option_defaults_2(optimization_option, OptimizationOptions),
        option_defaults_2(debugger_option, DebuggerOptions),
	option_defaults_2(miscellaneous_option, MiscellaneousOptions),
	list__condense([WarningOptions, VerbosityOptions, OutputOptions,
		CodeGenOptions, OptimizationOptions, DebuggerOptions,
                MiscellaneousOptions], OptionDefaults).

:- pred option_defaults_2(option_category::in,
	list(pair(option, option_data))::out) is det.

option_defaults_2(warning_option, [
		% Warning Options
	warn_singleton_vars	-	bool(yes),
	warn_missing_det_decls	-	bool(yes),
	warn_det_decls_too_lax	-	bool(yes)
]).
option_defaults_2(verbosity_option, [
		% Verbosity Options
	verbose			-	bool(no),
	very_verbose		-	bool(no),
	verbose_errors		-	bool(no),
	statistics		-	bool(no),
	debug_types		- 	bool(no),
	debug_modes		- 	bool(no),
	vndebug			- 	int(1)
]).
option_defaults_2(output_option, [
		% Output Options
	generate_dependencies	-	bool(no),
	make_interface		-	bool(no),
	make_call_graph		-	bool(no),
	convert_to_mercury 	-	bool(no),
	convert_to_goedel 	-	bool(no),
	modecheck		-	bool(yes),
	dump_hlds		-	accumulating([]),
	verbose_dump_hlds	-	bool(no),
	generate_code		-	bool(no),
	line_numbers		-	bool(no),
	mod_comments		-	bool(no)
]).
option_defaults_2(code_gen_option, [
		% Code Generation Options
	tags			-	string("low"),
	polymorphism		-	bool(yes),
	follow_code		-	bool(yes),
	follow_vars		-	bool(yes),
	lazy_code		-	bool(yes),
	reclaim_heap_on_semidet_failure	-	bool(yes),
	reclaim_heap_on_nondet_failure	-	bool(yes),
	num_tag_bits		-	int(2),
	gc			-	string("conservative"),
	compile			-	bool(no),
	cc			-	string("gcc"),
	cflags			-	string(""),
	c_include_directory	-	string(""),
	link			-	bool(no),
	gcc_non_local_gotos	-	bool(yes),
	gcc_global_registers	-	bool(yes),
	asm_labels		-	bool(yes)
]).
option_defaults_2(optimization_option, [
		% Optimization options
	debug			-	bool(no),
	c_optimize		-	bool(yes),
	grade			-	string("asm_fast.gc"),
	optimize		-	bool(yes),
	optimize_peep		-	bool(yes),
	optimize_jumps		-	bool(yes),
	optimize_fulljumps	-	bool(yes),
	optimize_labels		-	bool(yes),
	optimize_dups		-	bool(no),
	optimize_value_number	-	bool(no),
	optimize_frames		-	bool(yes),
	optimize_repeat		-	int(4),
	optimize_vnrepeat	-	int(1),
	pred_value_number	-	bool(no),
	static_ground_terms	-	bool(yes),
	smart_indexing		-	bool(yes),
	req_density		-	int(25),
	dense_switch_size	-	int(4),
	string_switch_size	-	int(8),
	tag_switch_size		-	int(8),
	middle_rec		-	bool(yes),
	inlining		-	bool(yes),
	common_struct		-	bool(no),
	common_goal		-	bool(yes),
	procs_per_c_function	-	int(5)
]).
option_defaults_2(debugger_option, [
		% Debugger Options
	debugger		-	bool(no)
]).
option_defaults_2(miscellaneous_option, [
		% Miscellaneous Options
	trad_passes		-	bool(yes),
	builtin_module		-	string("mercury_builtin"),
	heap_space		-	int(0),
	search_directories 	-	accumulating(["."]),
	help 			-	bool(no)
]).

	% please keep this in alphabetic order
short_option('b', 			builtin_module).
short_option('c', 			compile).
short_option('C', 			make_call_graph).
short_option('d', 			dump_hlds).
short_option('D', 			verbose_dump_hlds).
short_option('e', 			verbose_errors).
short_option('G', 			convert_to_goedel).
short_option('g', 			generate_code).
short_option('h', 			help).
short_option('H', 			heap_space).
short_option('i', 			make_interface).
short_option('I', 			search_directories).
short_option('l', 			line_numbers).
short_option('m', 			modecheck).
short_option('M', 			generate_dependencies).
short_option('N', 			debug_modes).
short_option('O', 			c_optimize).
short_option('p', 			polymorphism).
short_option('P', 			convert_to_mercury).
short_option('S', 			statistics).
short_option('s', 			grade).
short_option('T', 			debug_types).
short_option('v', 			verbose).
short_option('x', 			smart_indexing).
short_option('V', 			very_verbose).
short_option('w', 			warn_singleton_vars).
short_option('z', 			inlining).

long_option("polymorphism",		polymorphism).
long_option("grade",			grade).
long_option("c-optimize",		c_optimize).
long_option("debug",			debug).
long_option("verbose",			verbose).
long_option("very-verbose",		very_verbose).
long_option("verbose-error-messages",	verbose_errors).
long_option("statistics",		statistics).
long_option("dump-hlds",		dump_hlds).
long_option("verbose-dump-hlds",	verbose_dump_hlds).
long_option("generate-code",		generate_code).
long_option("trad-passes",		trad_passes).
long_option("builtin-module",		builtin_module).
long_option("make-call-graph",		make_call_graph).
long_option("make-interface",		make_interface).
long_option("heap-space",		heap_space).
long_option("search-directory",		search_directories).
long_option("convert-to-mercury", 	convert_to_mercury).
long_option("convert-to-Mercury", 	convert_to_mercury).
long_option("convert-to-goedel", 	convert_to_goedel).
long_option("convert-to-Goedel", 	convert_to_goedel).
long_option("help",			help).
long_option("line-numbers",		line_numbers).
long_option("warn-singleton-variables",	warn_singleton_vars).
long_option("warn-missing-det-decls",	warn_missing_det_decls).
long_option("warn-det-decls-too-lax",	warn_det_decls_too_lax).
long_option("modecheck",		modecheck).
long_option("debug-types",		debug_types).
long_option("debug-modes",		debug_modes).
long_option("vndebug",			vndebug).
long_option("generate-dependencies",	generate_dependencies).
long_option("tags",			tags).
long_option("follow-code",		follow_code).
long_option("follow-vars",		follow_vars).
long_option("lazy-code",		lazy_code).
long_option("reclaim-heap-on-semidet-failure",
					reclaim_heap_on_semidet_failure).
long_option("reclaim-heap-on-nondet-failure",
					reclaim_heap_on_nondet_failure).
long_option("num-tag-bits",		num_tag_bits).
long_option("gc",			gc).
long_option("garbage-collection",	gc).
long_option("compile",			compile).
long_option("cc",			cc).
long_option("cflags",			cflags).
long_option("c-include-directory",	c_include_directory).
long_option("link",			link).
long_option("gcc-non-local-gotos",	gcc_non_local_gotos).
long_option("gcc-global-registers",	gcc_global_registers).
long_option("asm-labels",		asm_labels).
long_option("mod-comments",		mod_comments).
long_option("optimize",			optimize).
long_option("optimise",			optimize).
long_option("optimize-peep",		optimize_peep).
long_option("optimise-peep",		optimize_peep).
long_option("optimize-jumps",		optimize_jumps).
long_option("optimise-jumps",		optimize_jumps).
long_option("optimize-fulljumps",	optimize_fulljumps).
long_option("optimise-fulljumps",	optimize_fulljumps).
long_option("optimize-labels",		optimize_labels).
long_option("optimise-labels",		optimize_labels).
long_option("optimize-dups",		optimize_dups).
long_option("optimise-dups",		optimize_dups).
long_option("optimize-value-number",	optimize_value_number).
long_option("optimise-value-number",	optimize_value_number).
long_option("optimize-frames",		optimize_frames).
long_option("optimise-frames",		optimize_frames).
long_option("optimize-repeat",		optimize_repeat).
long_option("optimise-repeat",		optimize_repeat).
long_option("optimize-vnrepeat",	optimize_vnrepeat).
long_option("optimise-vnrepeat",	optimize_vnrepeat).
long_option("pred-value-number",	pred_value_number).
long_option("static-ground-terms",	static_ground_terms).
long_option("smart-indexing",		smart_indexing).
long_option("req-density",		req_density).
long_option("dense-switch_size",	dense_switch_size).
long_option("string-switch_size",	string_switch_size).
long_option("tag-switch-size",		tag_switch_size).
long_option("middle-rec",		middle_rec).
long_option("inlining",			inlining).
long_option("common-struct",		common_struct).
long_option("common-goal",		common_goal).
long_option("procs-per-c-function",	procs_per_c_function).
long_option("procs-per-C-function",	procs_per_c_function).
long_option("debugger",			debugger).
options_help -->
	io__write_string("\t-h, --help\n"),
	io__write_string("\t\tPrint this usage message.\n"),

	io__write_string("\nWarning Options:\n"),
	io__write_string("\t-w-, --no-warn-singleton-variables\n"),
	io__write_string("\t\tDon't warn about variables which only occur once.\n"),
	io__write_string("\t--no-warn-missing-det-decls\n"),
	io__write_string("\t\tDon't warn about predicate declarations which don't\n"),
	io__write_string("\t\thave a determinism annotation.\n"),
	io__write_string("\t--no-warn-det-decls-too-lax\n"),
	io__write_string("\t\tDon't warn about determinism declarations\n"),
	io__write_string("\t\twhich could have been stricter.\n"),

	io__write_string("\nVerbosity Options:\n"),
	io__write_string("\t-v, --verbose\n"),
	io__write_string("\t\tOutput progress messages at each stage in the compilation.\n"),
	io__write_string("\t-V, --very_verbose\n"),
	io__write_string("\t\tOutput very verbose progress messages.\n"),
	io__write_string("\t-e, --verbose-error-messages\n"),
	io__write_string("\t\tExplain error messages.  Asks the compiler to give you a more\n"),
	io__write_string("\t\tdetailed explanation of any errors in your program.\n"),
	io__write_string("\t-S, --statistics\n"),
	io__write_string("\t\tOutput messages about the compiler's time/space usage\n"),
	io__write_string("\t-T, --debug-types\n"),
	io__write_string("\t\tOutput detailed debugging traces of the type checking.\n"),
	io__write_string("\t-N, --debug-modes\n"),
	io__write_string("\t\tOutput detailed debugging traces of the mode checking.\n"),
	io__write_string("\t--vndebug <n>\n"),
	io__write_string("\t\tOutput detailed debugging traces of value numbering.\n"),

	io__write_string("\nOutput Options:\n"),
	io__write_string("\t-M, --generate-dependencies\n"),
	io__write_string("\t\tOutput `Make'-style dependencies for the module\n"),
	io__write_string("\t\tand all of its dependencies to `<module>.dep'.\n"),
	io__write_string("\t\tOnly syntax analysis will be performed - this option\n"),
	io__write_string("\t\tdisables all the later phases of compilation.\n"),
	io__write_string("\t-i, --make-interface\n"),
	io__write_string("\t\tWrite the module interface to `<module>.int'.\n"),
	io__write_string("\t\tAlso write the short interface to `<module>.int2'\n"),
	io__write_string("\t\tAs with -M, this disables type-checking, etc.\n"),
	io__write_string("\t-C --make-call-graph\n"),
	io__write_string("\t\tWrite out the call graph to <module>.call_graph.\n"),
	io__write_string("\t-G, --convert-to-goedel\n"),
	io__write_string("\t\tConvert to Goedel. Output to file `<module>.loc'\n"),
	io__write_string("\t\tAs with -M, this disables type-checking, etc.\n"),
	io__write_string("\t-P, --convert-to-mercury\n"),
	io__write_string("\t\tConvert to Mercury. Output to file `<module>.ugly'\n"),
	io__write_string("\t\tThis option acts as a Mercury ugly-printer.\n"),
	io__write_string("\t\tAs with -M, it disables type-checking, etc.\n"),
	io__write_string("\t-m-, --no-modecheck\n"),
	io__write_string("\t\tDon't invoke the mode analysis pass of the compiler.\n"),
	io__write_string("\t\tJust check that the code is syntactically correct and type-correct.\n"),
	io__write_string("\t-d <n>, --dump-hlds <stage number or name>\n"),
	io__write_string("\t\tDump the HLDS (intermediate representation) after\n"),
	io__write_string("\t\tthe specified stage to `<module>.hlds_dump.<num>-<name>'.\n"),
	io__write_string("\t\tStage numbers range from 0-17.\n"),
	io__write_string("\t\tMultiple dump options accumulate.)\n"),
	io__write_string("\t-D, --verbose-dump-hlds\n"),
	io__write_string("\t\tWith --dump-hlds, dumps some additional info.\n"),
	io__write_string("\t-g, --generate-code\n"),
	io__write_string("\t\tGenerate code in `<module>.c'.\n"),
	io__write_string("\t-c, --compile\n"),
	io__write_string("\t\tInvoke the C compiler on the generated .c file.\n"),
	io__write_string("\t\tThis option implies --generate-code.\n"),
	io__write_string("\t--link\n"),
	io__write_string("\t\tLink the named modules to produce an executable.\n"),
	io__write_string("\t\tThis option implies --compile.\n"),
	io__write_string("\t--mod-comments\n"),
	io__write_string("\t\tOutput comments in the .mod file\n"),
	io__write_string("\t-l, --line-numbers\n"),
	io__write_string("\t\tOutput line numbers in the generated code.\n"),
	io__write_string("\t\tCurrently only works with the -G and -M options.\n"),

	io__write_string("\nCode generation options\n"),
	io__write_string("\t--no-trad-passes\n"),
	io__write_string("\t\tGenerate code by phases, not by predicates.\n"),
	io__write_string("\t-p-, --no-polymorphism\n"),
	io__write_string("\t\tDon't handle polymorphic types.\n"),
	io__write_string("\t\t(Generates slightly more efficient code, but stops\n"),
	io__write_string("\t\t(polymorphism from working except in special cases.)\n"),
	io__write_string("\t-s <grade>, --grade <grade>\n"),
	io__write_string("\t\tSelect the compilation model.  This is a convenient way of\n"),
	io__write_string("\t\tselecting a setting for the --c-optimize, --gc,\n"),
	io__write_string("\t\t--gcc-global-registers, --gcc-non-local-gotos,\n"),
	io__write_string("\t\t--asm-labels, and --debug options simultaneously.\n"),
	io__write_string("\t\t<grade> should be one of\n"),
	io__write_string("\t\t\tdebug, none, reg, jump, asm_jump, fast, asm_fast\n"),
	io__write_string("\t\tor one of those with `.gc' appended.\n"),
	io__write_string("\t\t(See the mgnuc shell script source code for details).\n"),
	io__write_string("\t--gc {none, conservative, accurate}\n"),
	io__write_string("\t--garbage-collection {none, conservative, accurate}\n"),
	io__write_string("\t\tSpecify which method of garbage collection to use.\n"),
	io__write_string("\t\t`accurate' GC is not yet implemented.\n"),
	io__write_string("\t\t`conservative' GC implies `--tags none'.\n"),
	io__write_string("\t--no-follow-code\n"),
	io__write_string("\t\tDon't migrate builtin goals into branched goals.\n"),
	io__write_string("\t--no-follow-vars\n"),
	io__write_string("\t\tDon't optimise the assignment of registers in branched goals.\n"),
	io__write_string("\t--no-reclaim-heap-on-nondet-failure\n"),
	io__write_string("\t\tDon't reclaim heap on backtracking in nondet code.\n"),
	io__write_string("\t--no-reclaim-heap-on-semidet-failure\n"),
	io__write_string("\t\tDon't reclaim heap on backtracking in semidet code.\n"),
	io__write_string("\t--no-gcc-global-registers\n"),
	io__write_string("\t\tDon't use GNU C's global register variables extension.\n"),
	io__write_string("\t--no-gcc-non-local-gotos\n"),
	io__write_string("\t\tDon't use GNU C's ""labels as values"" extension.\n"),
	io__write_string("\t--no-asm-labels\n"),
	io__write_string("\t\tDon't use inline assembler labels.\n"),
	io__write_string("\t--tags {none, low, high}\n"),
	io__write_string("\t\tSpecify whether to use the low bits or the high bits of \n"),
	io__write_string("\t\teach word as tag bits (default: low).\n"),
	io__write_string("\t\t`--tags none' implies `--num-tag-bits 0'.\n"),
	io__write_string("\t--num-tag-bits <n>\n"),
	io__write_string("\t\tUse <n> tag bits (used with `--tags high').\n"),
	io__write_string("\t--cc <compiler-name>\n"),
	io__write_string("\t\tSpecify which C compiler to use.\n"),
	io__write_string("\t--c-include-directory <dir>\n"),
	io__write_string("\t\tSpecify the directory containing the Mercury C header files.\n"),
	io__write_string("\t--cflags <options>\n"),
	io__write_string("\t\tSpecify options to be passed to the C compiler\n"),
	io__write_string("\t--debug\n"),
	io__write_string("\t\tEnable debugging.\n"),

	io__write_string("\nOptimization Options\n"),
	io__write_string("\t--no-optimize\n"),
	io__write_string("\t\tDisable the optimisation passes.\n"),
	io__write_string("\t--no-optimize-peep\n"),
	io__write_string("\t\tDisable local peephole optimisations.\n"),
	io__write_string("\t--no-optimize-jumps\n"),
	io__write_string("\t\tDisable elimination of jumps to jumps.\n"),
	io__write_string("\t--no-optimize-fulljumps\n"),
	io__write_string("\t\tDisable elimination of jumps to ordinary code.\n"),
	io__write_string("\t--no-optimize-labels\n"),
	io__write_string("\t\tDisable elimination of dead labels and code\n"),
	io__write_string("\t--no-optimize-dups\n"),
	io__write_string("\t\tDisable elimination of duplicate code\n"),
	io__write_string("\t--optimize-value-number\n"),
	io__write_string("\t\tPerform value numbering\n"),
	io__write_string("\t--no-optimize-frames\n"),
	io__write_string("\t\tDisable stack frame optimizations\n"),
	io__write_string("\t--optimize-repeat <n>\n"),
	io__write_string("\t\tIterate optimizations at most <n> times\n"),
	io__write_string("\t--optimize-vnrepeat <n>\n"),
	io__write_string("\t\tIterate value numbering at most <n> times\n"),
	io__write_string("\t--pred-value-number\n"),
	io__write_string("\t\tExtend value numbering to entire predicates\n"),
	io__write_string("\t--no-static-ground-terms\n"),
	io__write_string("\t\tConstruct all terms at runtime; disable the optimization\n"),
	io__write_string("\t\tof constructing constant ground terms at compile time\n"),
	io__write_string("\t\tand storing them as static constants.\n"),
	io__write_string("\t--no-smart-indexing\n"),
	io__write_string("\t\tGenerate deterministic switches as a simple if-then-else chain;\n"),
	io__write_string("\t\tdisable string hashing and integer table-lookup indexing.\n"),
	io__write_string("\t--req-density <percentage>\n"),
	io__write_string("\t\tThe jump table generated for an atomic switch\n"),
	io__write_string("\t\tmust have at least this percentage of full slots.\n"),
	io__write_string("\t--dense-switch-size <n>\n"),
	io__write_string("\t\tThe jump table generated for an atomic switch\n"),
	io__write_string("\t\tmust have at least <n> entries.\n"),
	io__write_string("\t--string-switch-size <n>\n"),
	io__write_string("\t\tThe hash table generated for a string switch\n"),
	io__write_string("\t\tmust have at least <n> entries.\n"),
	io__write_string("\t--tag-switch-size <n>\n"),
	io__write_string("\t\tThe number of alternatives in a tag switch\n"),
	io__write_string("\t\tmust be at least <n>.\n"),
	io__write_string("\t--no-middle-rec\n"),
	io__write_string("\t\tDisable the middle recursion optimization.\n"),
	io__write_string("\t--no-inlining\n"),
	io__write_string("\t\tDisable the inlining of simple procedures.\n"),
	io__write_string("\t--common-struct\n"),
	io__write_string("\t\tEnable optimisation of common term structures.\n"),
	io__write_string("\t--no-common-goal\n"),
	io__write_string("\t\tDisable optimisation of common goals.\n"),
	io__write_string("\t--procs-per-c-function <n>\n"),
	io__write_string("\t\tDon't put the code for more than <n> Mercury\n"),
	io__write_string("\t\tprocedures in a single C function.  The default\n"),
	io__write_string("\t\t value of <n> is one.  Increasing <n> can produce\n"),
	io__write_string("\t\tslightly more efficient code, but makes compilation slower.\n"),
	io__write_string("\t\tSetting <n> to the special value zero has the effect of\n"),
	io__write_string("\t\tputting all the procedures in a single function,\n"),
	io__write_string("\t\twhich produces the most efficient code but tends to\n"),
	io__write_string("\t\tseverely stress the C compiler on large modules.\n"),
	io__write_string("\t--no-c-optimize\n"),
	io__write_string("\t\tDon't enable the C compiler's optimizations.\n"),

	io__write_string("\nDebugger Options:\n"),
	io__write_string("\t--debugger\n"),
	io__write_string("\t\tDeclarative Debugger(not installed)\n"),

	io__write_string("\nMiscellaneous Options:\n"),
	io__write_string("\t-H <n>, --heap-space <n>\n"),
	io__write_string("\t\tPre-allocate <n> kilobytes of heap space.\n"),
	io__write_string("\t\tThis option is now obsolete.  In the past it\n"),
	io__write_string("\t\twas used to avoid NU-Prolog's\n"),
	io__write_string("\t\t\t""Panic: growing stacks has required shifting the heap""\n"),
	io__write_string("\t\tmessage.\n"),

	io__write_string("\t-b <builtin>, --builtin-module <builtin>\n"),
	io__write_string("\t\tUse `<builtin>' instead of `mercury_builtin' as the \n\t\tmodule which always gets automatically imported.\n"),
	io__write_string("\t-I <dir>, --search-directory <dir>\n"),
	io__write_string("\t\tAppend <dir> to the list of directories to be searched for \n\t\timported modules.\n").

maybe_report_stats(yes) --> io__report_stats.
maybe_report_stats(no) --> [].

maybe_write_string(yes, String) --> io__write_string(String).
maybe_write_string(no, _) --> [].

maybe_flush_output(yes) --> io__flush_output.
maybe_flush_output(no) --> [].

options__lookup_bool_option(OptionTable, Opt, Val) :-
	(
		map__lookup(OptionTable, Opt, bool(Val0))
	->
		Val = Val0
	;
		error("Expected bool option and didn't get one.")
	).

options__lookup_int_option(OptionTable, Opt, Val) :-
	(
		map__lookup(OptionTable, Opt, int(Val0))
	->
		Val = Val0
	;
		error("Expected int option and didn't get one.")
	).

options__lookup_string_option(OptionTable, Opt, Val) :-
	(
		map__lookup(OptionTable, Opt, string(Val0))
	->
		Val = Val0
	;
		error("Expected string option and didn't get one.")
	).

:- end_module options.

%-----------------------------------------------------------------------------%
