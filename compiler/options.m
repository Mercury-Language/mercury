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
		;	warn_nothing_exported
		;	inhibit_warnings
		;	halt_at_warn
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
		;	generate_dependencies
		;	convert_to_mercury
		;	convert_to_goedel
		;	typecheck_only
		;	errorcheck_only
		;	compile_to_c
		;	compile_only
	% Auxiliary output options
		;	line_numbers
		;	auto_comments
		;	show_dependency_graph
		;	dump_hlds
		;	verbose_dump_hlds
		;	profiling
		;	output_file_name
	% Code generation options
		;	trad_passes
		;	lazy_code
		;	polymorphism
		;	reclaim_heap_on_semidet_failure
		;	reclaim_heap_on_nondet_failure
		;	use_macro_for_redo_fail
		;	simple_neg
		;	tags
		;	num_tag_bits
		;	prev_code
		;	follow_code
		;	follow_vars
		;	gc
		;	cc
		;	cflags
		;	c_include_directory
		;	link_flags
		;	gcc_non_local_gotos
		;	gcc_global_registers
		;	asm_labels
		;	emit_c_loops
	% Optimisation Options
		;	optimize
		;	optimize_peep
		;	optimize_jumps
		;	optimize_fulljumps
		;	optimize_labels
		;	optimize_dups
		;	optimize_copyprop
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
		;	constraint_propagation
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
	;	miscellaneous_option.

option_defaults(OptionDefaults) :-
	option_defaults_2(warning_option, WarningOptions),
	option_defaults_2(verbosity_option, VerbosityOptions),
	option_defaults_2(output_option, OutputOptions),
	option_defaults_2(code_gen_option, CodeGenOptions),
	option_defaults_2(optimization_option, OptimizationOptions),
	option_defaults_2(miscellaneous_option, MiscellaneousOptions),
	list__condense([WarningOptions, VerbosityOptions, OutputOptions,
		CodeGenOptions, MiscellaneousOptions, OptimizationOptions],
		OptionDefaults).

:- pred option_defaults_2(option_category::in,
	list(pair(option, option_data))::out) is det.

option_defaults_2(warning_option, [
		% Warning Options
	inhibit_warnings	-	bool(no),
	halt_at_warn		-	bool(no),
	warn_singleton_vars	-	bool(yes),
	warn_missing_det_decls	-	bool(yes),
	warn_det_decls_too_lax	-	bool(yes),
	warn_nothing_exported	-	bool(yes)
]).
option_defaults_2(verbosity_option, [
		% Verbosity Options
	verbose			-	bool(no),
	very_verbose		-	bool(no),
	verbose_errors		-	bool(no),
	statistics		-	bool(no),
	debug_types		- 	bool(no),
	debug_modes		- 	bool(no),
	vndebug			- 	int(0)
]).
option_defaults_2(output_option, [
		% Output Options (mutually exclusive)
	generate_dependencies	-	bool(no),
	make_interface		-	bool(no),
	convert_to_mercury 	-	bool(no),
	convert_to_goedel 	-	bool(no),
	typecheck_only		-	bool(no),
	errorcheck_only		-	bool(no),
	compile_to_c		-	bool(no),
	compile_only		-	bool(no),
		% Auxiliary Output Options
	show_dependency_graph	-	bool(no),
	dump_hlds		-	accumulating([]),
	verbose_dump_hlds	-	bool(no),
	line_numbers		-	bool(no),
	auto_comments		-	bool(no),
	profiling		-	bool(no),
	output_file_name	-	string("")
					% if the output_file_name is an empty
					% string, we use the name of the first
					% module on the command line
]).
option_defaults_2(code_gen_option, [
		% Code Generation Options
	tags			-	string("low"),
	polymorphism		-	bool(yes),
	prev_code		-	bool(no),
	follow_code		-	bool(yes),
	follow_vars		-	bool(yes),
	lazy_code		-	bool(yes),
	reclaim_heap_on_semidet_failure	-	bool(yes),
	reclaim_heap_on_nondet_failure	-	bool(yes),
	use_macro_for_redo_fail	-	bool(no),
	simple_neg		-	bool(yes),
	num_tag_bits		-	int(-1),
					% -1 is a special value which means
					% use the autoconf-determined value
					% instead
	gc			-	string("conservative"),
	cc			-	string("gcc"),
	cflags			-	string(""),
	c_include_directory	-	string(""),
	link_flags		-	string(""),
	gcc_non_local_gotos	-	bool(yes),
	gcc_global_registers	-	bool(yes),
	asm_labels		-	bool(yes),
	emit_c_loops		-	bool(yes)
]).
option_defaults_2(optimization_option, [
		% Optimization options
	debug			-	bool(no),
	c_optimize		-	bool(yes),
	grade			-	string("asm_fast.gc"),
					% this default grade always gets
					% overridden by the `mc' script,
					% which uses the value determined
					% by autoconf
	optimize		-	bool(yes),
	optimize_peep		-	bool(yes),
	optimize_jumps		-	bool(yes),
	optimize_fulljumps	-	bool(yes),
	optimize_labels		-	bool(yes),
	optimize_dups		-	bool(no),
	optimize_copyprop	-	bool(yes),
	optimize_value_number	-	bool(no),
	optimize_frames		-	bool(yes),
	optimize_repeat		-	int(3),
	optimize_vnrepeat	-	int(1),
	pred_value_number	-	bool(no),
	static_ground_terms	-	bool(yes),
	smart_indexing		-	bool(yes),
	req_density		-	int(25),
	dense_switch_size	-	int(4),
	string_switch_size	-	int(8),
	tag_switch_size		-	int(4),
	middle_rec		-	bool(yes),
	inlining		-	bool(yes),
	common_struct		-	bool(yes),
	common_goal		-	bool(yes),
	procs_per_c_function	-	int(1),
	constraint_propagation	-	bool(no)
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
% short_option('b', 			builtin_module).
short_option('c', 			compile_only).
short_option('C', 			compile_to_c).
short_option('d', 			dump_hlds).
short_option('D', 			verbose_dump_hlds).
short_option('e', 			errorcheck_only).
short_option('E', 			verbose_errors).
short_option('G', 			convert_to_goedel).
short_option('h', 			help).
short_option('H', 			heap_space).
short_option('i', 			make_interface).
short_option('I', 			search_directories).
short_option('l', 			line_numbers).
short_option('M', 			generate_dependencies).
short_option('N', 			debug_modes).
short_option('O', 			c_optimize).
short_option('o', 			output_file_name).
% short_option('p', 			polymorphism).
short_option('p', 			profiling).
short_option('P', 			convert_to_mercury).
short_option('s', 			grade).
short_option('S', 			statistics).
short_option('T', 			debug_types).
short_option('t', 			typecheck_only).
short_option('v', 			verbose).
short_option('V', 			very_verbose).
% short_option('x', 			smart_indexing).
short_option('w', 			inhibit_warnings).
% short_option('z', 			inlining).
short_option('?', 			help).

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
long_option("trad-passes",		trad_passes).
long_option("builtin-module",		builtin_module).
long_option("show-dependency-graph",	show_dependency_graph).
long_option("make-interface",		make_interface).
long_option("heap-space",		heap_space).
long_option("search-directory",		search_directories).
long_option("convert-to-mercury", 	convert_to_mercury).
long_option("convert-to-Mercury", 	convert_to_mercury).
long_option("pretty-print", 		convert_to_mercury).
long_option("convert-to-goedel", 	convert_to_goedel).
long_option("convert-to-Goedel", 	convert_to_goedel).
long_option("help",			help).
long_option("line-numbers",		line_numbers).
long_option("warn-singleton-variables",	warn_singleton_vars).
long_option("warn-missing-det-decls",	warn_missing_det_decls).
long_option("warn-det-decls-too-lax",	warn_det_decls_too_lax).
long_option("warn-nothing-exported",	warn_nothing_exported).
long_option("inhibit-warnings",		inhibit_warnings).
long_option("halt-at-warn",		halt_at_warn).
long_option("typecheck-only",		typecheck_only).
long_option("errorcheck-only",		errorcheck_only).
long_option("debug-types",		debug_types).
long_option("debug-modes",		debug_modes).
long_option("vndebug",			vndebug).
long_option("generate-dependencies",	generate_dependencies).
long_option("tags",			tags).
long_option("prev-code",		prev_code).
long_option("follow-code",		follow_code).
long_option("follow-vars",		follow_vars).
long_option("lazy-code",		lazy_code).
long_option("reclaim-heap-on-semidet-failure",
					reclaim_heap_on_semidet_failure).
long_option("reclaim-heap-on-nondet-failure",
					reclaim_heap_on_nondet_failure).
long_option("use-macro-for-redo-fail",	use_macro_for_redo_fail).
long_option("simple_neg",		simple_neg).
long_option("num-tag-bits",		num_tag_bits).
long_option("gc",			gc).
long_option("garbage-collection",	gc).
long_option("compile-to-c",		compile_to_c).
long_option("compile-to-C",		compile_to_c).
long_option("compile-only",		compile_only).
long_option("cc",			cc).
long_option("cflags",			cflags).
long_option("link-flags",		link_flags).
long_option("output-file",		output_file_name).
long_option("c-include-directory",	c_include_directory).
long_option("gcc-non-local-gotos",	gcc_non_local_gotos).
long_option("gcc-global-registers",	gcc_global_registers).
long_option("asm-labels",		asm_labels).
long_option("emit-c-loops",		emit_c_loops).
long_option("auto-comments",		auto_comments).
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
long_option("optimize-copyprop",	optimize_copyprop).
long_option("optimise-copyprop",	optimize_copyprop).
long_option("optimize-value-number",	optimize_value_number).
long_option("optimise-value-number",	optimize_value_number).
long_option("optimize-frames",		optimize_frames).
long_option("optimise-frames",		optimize_frames).
long_option("optimize-repeat",		optimize_repeat).
long_option("optimise-repeat",		optimize_repeat).
long_option("optimize-vnrepeat",	optimize_vnrepeat).
long_option("optimise-vnrepeat",	optimize_vnrepeat).
long_option("profiling",		profiling).
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
long_option("constraint-propagation",	constraint_propagation).

options_help -->
	io__write_string("\t-?, -h, --help\n"),
	io__write_string("\t\tPrint this usage message.\n"),

	io__write_string("\nWarning Options:\n"),
	io__write_string("\t-w, --inhibit-warnings\n"),
	io__write_string("\t\tDisable all warning messages.\n"),
	io__write_string("\t--halt_at_warn\n"),
	io__write_string("\t\tThis option causes the compiler to treat all \n"),
	io__write_string("\t\twarnings as if they were errors.  This means that\n"),
	io__write_string("\t\tif any warning is issued, the compiler will not\n"),
	io__write_string("\t\tgenerate code --- instead, it will return a\n"),
	io__write_string("\t\tnon-zero exit status.\n"),
	io__write_string("\t--no-warn-singleton-variables\n"),
	io__write_string("\t\tDon't warn about variables which only occur once.\n"),
	io__write_string("\t--no-warn-missing-det-decls\n"),
	io__write_string("\t\tDon't warn about predicate declarations which don't\n"),
	io__write_string("\t\thave a determinism annotation.\n"),
	io__write_string("\t--no-warn-det-decls-too-lax\n"),
	io__write_string("\t\tDon't warn about determinism declarations\n"),
	io__write_string("\t\twhich could have been stricter.\n"),
	io__write_string("\t--no-warn-nothing-exported\n"),
	io__write_string("\t\tDon't warn about modules which export nothing.\n"),

	io__write_string("\nVerbosity Options:\n"),
	io__write_string("\t-v, --verbose\n"),
	io__write_string("\t\tOutput progress messages at each stage in the compilation.\n"),
	io__write_string("\t-V, --very_verbose\n"),
	io__write_string("\t\tOutput very verbose progress messages.\n"),
	io__write_string("\t-E, --verbose-error-messages\n"),
	io__write_string("\t\tExplain error messages.  Asks the compiler to give you a more\n"),
	io__write_string("\t\tdetailed explanation of any errors it finds in your program.\n"),
	io__write_string("\t-S, --statistics\n"),
	io__write_string("\t\tOutput messages about the compiler's time/space usage.\n"),
	io__write_string("\t\tAt the moment this option implies --no-trad-passes, so you get\n"),
	io__write_string("\t\tinformation at the boundaries between phases of the compiler.\n"),
	io__write_string("\t-T, --debug-types\n"),
	io__write_string("\t\tOutput detailed debugging traces of the type checking.\n"),
	io__write_string("\t-N, --debug-modes\n"),
	io__write_string("\t\tOutput detailed debugging traces of the mode checking.\n"),
	io__write_string("\t--vndebug <n>\n"),
	io__write_string("\t\tOutput detailed debugging traces of the value numbering\n"),
	io__write_string("\t\toptimization pass. The different bits in the number\n"),
	io__write_string("\t\targument of this option control the printing of\n"),
	io__write_string("\t\tdifferent types of tracing messages.\n"),

	io__write_string("\nOutput Options:\n"),
	io__write_string("\tThese options are mutually exclusive.\n"),
	io__write_string("\tOnly the first one specified will apply.\n"),
	io__write_string("\tIf none of these options are specified, the default action\n"),
	io__write_string("\tis to link the named modules to produce an executable.\n\n"),
	io__write_string("\t-M, --generate-dependencies\n"),
	io__write_string("\t\tOutput `Make'-style dependencies for the module\n"),
	io__write_string("\t\tand all of its dependencies to `<module>.dep'.\n"),
	io__write_string("\t-i, --make-interface\n"),
	io__write_string("\t\tWrite the module interface to `<module>.int'.\n"),
	io__write_string("\t\tAlso write the short interface to `<module>.int2'.\n"),
	io__write_string("\t-G, --convert-to-goedel\n"),
	io__write_string("\t\tConvert to Goedel. Output to file `<module>.loc'.\n"),
	io__write_string("\t\tNote that some Mercury language constructs cannot\n"),
	io__write_string("\t\t(easily) be translated into Goedel.\n"),
	io__write_string("\t-P, --convert-to-mercury\n"),
	io__write_string("\t\tConvert to Mercury. Output to file `<module>.ugly'\n"),
	io__write_string("\t\tThis option acts as a Mercury ugly-printer.\n"),
	io__write_string("\t-t, --typecheck-only\n"),
	io__write_string("\t\tJust check that the code is syntactically correct and\n"),
	io__write_string("\t\ttype-correct. Don't check modes or determism,\n"),
	io__write_string("\t\tand don't generate any code.\n"),
	io__write_string("\t-e, --errorcheck-only\n"),
	io__write_string("\t\tCheck the module for errors, but do not generate any code.\n"),
	io__write_string("\t-C, --compile-to-c\n"),
	io__write_string("\t\tGenerate C code in `<module>.c', but not object code.\n"),
	io__write_string("\t-c, --compile-only\n"),
	io__write_string("\t\tGenerate C code in `<module>.c' and object code in `<module>.o'\n"),
	io__write_string("\t\tbut do not attempt to link the named modules.\n"),

	io__write_string("\n Auxiliary Output Options:\n"),
	io__write_string("\t--auto-comments\n"),
	io__write_string("\t\tOutput comments in the `<module>.c' file.\n"),
	io__write_string("\t-l, --line-numbers\n"),
	io__write_string("\t\tOutput line numbers in the generated code.\n"),
	io__write_string("\t\tOnly works with the -G and -P options.\n"),
	io__write_string("\t--show-dependency-graph\n"),
	io__write_string("\t\tWrite out the dependency graph to <module>.dependency_graph.\n"),
	io__write_string("\t-d <n>, --dump-hlds <stage number or name>\n"),
	io__write_string("\t\tDump the HLDS (intermediate representation) after\n"),
	io__write_string("\t\tthe specified stage to `<module>.hlds_dump.<num>-<name>'.\n"),
	io__write_string("\t\tStage numbers range from 0-17.\n"),
	io__write_string("\t\tMultiple dump options accumulate.\n"),
	io__write_string("\t-D, --verbose-dump-hlds\n"),
	io__write_string("\t\tWith --dump-hlds, dumps some additional info.\n"),
	io__write_string("\t-o <filename>, --output-file <filename>\n"),
	io__write_string("\t\tSpecify the name of the final executable.\n"),
	io__write_string("\t\t(The default executable name is the same as the name\n"),
	io__write_string("\t\tof the first module on the command line.)\n"),

	io__write_string("\nCompilation model options:\n"),
	io__write_string("\tThe following compilation options affect the generated\n"),
	io__write_string("\tcode in such a way that the entire program must be\n"),
	io__write_string("\tcompiled with the same setting of these options,\n"),
	io__write_string("\tand it must be linked to a version of the Mercury\n"),
	io__write_string("\tlibrary which has been compiled with the same setting.\n"),
	io__write_string("\tRather than setting them individually, you must\n"),
	io__write_string("\tspecify them all at once by selecting a particular\n"),
	io__write_string("\tcompilation model (""grade"").\n\n"),
	io__write_string("\t-s <grade>, --grade <grade>\n"),
	io__write_string("\t\tSelect the compilation model. The <grade> should be one of\n"),
	io__write_string("\t\t`debug', `none', `reg', `jump', `asm_jump', `fast', `asm_fast',\n"),
	io__write_string("\t\tor one of those with `.gc', `.prof' or `.gc.prof' appended.\n"),
	io__write_string("\t\tDepending on your particular installation, only a subset\n"),
	io__write_string("\t\tof these possible grades will have been installed.\n"),
	io__write_string("\t\tAttempting to use a grade which has not been installed\n"),
	io__write_string("\t\twill result in an error at link time.\n"),
	io__write_string("\t--gcc-global-registers\t"),
	io__write_string("\t(grades: reg, fast, asm_fast)\n"),
	io__write_string("\t--no-gcc-global-registers"),
	io__write_string("\t(grades: debug, none, jump, asm_jump)\n"),
	io__write_string("\t\tSpecify whether or not to use GNU C's\n"),
	io__write_string("\t\tglobal register variables extension.\n"),
	io__write_string("\t--gcc-non-local-gotos\t"),
	io__write_string("\t(grades: jump, fast, asm_jump, asm_fast)\n"),
	io__write_string("\t--no-gcc-non-local-gotos"),
	io__write_string("\t(grades: debug, none, reg)\n"),
	io__write_string("\t\tSpecify whether or not to use GNU C's\n"),
	io__write_string("\t\t""labels as values"" extension.\n"),
	io__write_string("\t--asm-labels\t\t"),
	io__write_string("\t(grades: asm_jump, asm_fast)\n"),
	io__write_string("\t--no-asm-labels\t\t"),
	io__write_string("\t(grades: debug, none, reg, jump, fast)\n"),
	io__write_string("\t\tSpecify whether or not to use GNU C's\n"),
	io__write_string("\t\tasm extensions for inline assembler labels.\n"),
	io__write_string("\t--gc {none, conservative, accurate}\n"),
	io__write_string("\t--garbage-collection {none, conservative, accurate}\n"),
	io__write_string("\t\t\t\t\t(`.gc' grades use `--gc conservative',\n"),
	io__write_string("\t\t\t\t\tother grades use `--gc none'.)\n"),
	io__write_string("\t\tSpecify which method of garbage collection to use\n"),
	io__write_string("\t\t(default: conservative).  `accurate' GC is not yet implemented.\n"),
	io__write_string("\t--tags {none, low, high}"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tSpecify whether to use the low bits or the high bits of \n"),
	io__write_string("\t\teach word as tag bits (default: low).\n"),
	% io__write_string("\t\t`--tags none' implies `--num-tag-bits 0'.\n"),
	io__write_string("\t--num-tag-bits <n>\t"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tUse <n> tag bits.\n"),
	io__write_string("\t--profiling\t\t"),
	io__write_string("\t(grades: any grade ending in `.prof')\n"),
	io__write_string("\t\tEnable profiling.  Insert profiling hooks in the\n"),
	io__write_string("\t\tgenerated code, and also output some profiling\n"),
	io__write_string("\t\tinformation (the static call graph) to the file\n"),
	io__write_string("\t\t`<module>.prof'.\n"),
	io__write_string("\t--debug\t\t\t"),
	io__write_string("\t(grades: debug)\n"),
	io__write_string("\t\tEnable debugging.\n"),
	io__write_string("\t\tDebugging support is currently extremely primitive.\n"),
	io__write_string("\t\tWe recommend that you use instead use `mnp' or `msp'.\n"),
	io__write_string("\t\tSee the Mercury User's Guide for details.\n"),

	io__write_string("\nCode generation options\n"),
	io__write_string("\t--no-trad-passes\n"),
	io__write_string("\t\tThe default --trad-passes completely processes each predicate\n"),
	io__write_string("\t\tbefore going on to the next predicate.\n"),
	io__write_string("\t\tThis option tells the compiler\n"),
	io__write_string("\t\tto complete each phase of code generation on all predicates\n"),
	io__write_string("\t\tbefore going on the next phase on all predicates.\n"),
	% io__write_string("\t--no-polymorphism\n"),
	% io__write_string("\t\tDon't handle polymorphic types.\n"),
	% io__write_string("\t\t(Generates slightly more efficient code, but stops\n"),
	% io__write_string("\t\tpolymorphism from working except in special cases.)\n"),
	io__write_string("\t--prev-code\n"),
	io__write_string("\t\tMigrate into the start of branched goals.\n"),
	io__write_string("\t--no-follow-code\n"),
	io__write_string("\t\tDon't migrate into the end of branched goals.\n"),
	io__write_string("\t--no-follow-vars\n"),
	io__write_string("\t\tDon't optimise the assignment of registers in branched goals.\n"),
	io__write_string("\t--no-reclaim-heap-on-nondet-failure\n"),
	io__write_string("\t\tDon't reclaim heap on backtracking in nondet code.\n"),
	io__write_string("\t--no-reclaim-heap-on-semidet-failure\n"),
	io__write_string("\t\tDon't reclaim heap on backtracking in semidet code.\n"),
	io__write_string("\t--use-macro-for-redo-fail\n"),
	io__write_string("\t\tEmit the fail or redo macro instead of a branch\n"),
	io__write_string("\t\tto the fail or redo code in the runtime system.\n"),
	io__write_string("\t--no-simple-neg\n"),
	io__write_string("\t\tDon't generate simplified code for simple negations.\n"),
	io__write_string("\t--no-emit-c-loops\n"),
	io__write_string("\t\tUse only gotos, don't emit C loop constructs.\n"),
	io__write_string("\t--cc <compiler-name>\n"),
	io__write_string("\t\tSpecify which C compiler to use.\n"),
	io__write_string("\t--c-include-directory <dir>\n"),
	io__write_string("\t\tSpecify the directory containing the Mercury C header files.\n"),
	io__write_string("\t--cflags <options>\n"),
	io__write_string("\t\tSpecify options to be passed to the C compiler.\n"),
	io__write_string("\t--link-flags <options>\n"),
	io__write_string("\t\tSpecify options to be passed to the linker.\n"),

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
	io__write_string("\t\tDisable elimination of dead labels and code.\n"),
	io__write_string("\t--optimize-dups\n"),
	io__write_string("\t\tEnable elimination of duplicate code.\n"),
	% io__write_string("\t--optimize-copyprop\n"),
	% io__write_string("\t\tEnable the copy propagation optimization.\n"),
	io__write_string("\t--optimize-value-number\n"),
	io__write_string("\t\tPerform value numbering on extended basic blocks.\n"),
	io__write_string("\t--no-optimize-frames\n"),
	io__write_string("\t\tDisable stack frame optimizations.\n"),
	io__write_string("\t--optimize-repeat <n>\n"),
	io__write_string("\t\tIterate most optimizations at most <n> times (default: 3).\n"),
	io__write_string("\t--optimize-vnrepeat <n>\n"),
	io__write_string("\t\tIterate value numbering at most <n> times (default: 1).\n"),
	% io__write_string("\t--pred-value-number\n"),
	% io__write_string("\t\tExtend value numbering to entire predicates\n"),
	io__write_string("\t--no-static-ground-terms\n"),
	io__write_string("\t\tDisable the optimization of constructing constant ground terms\n"),
	io__write_string("\t\tat compile time and storing them as static constants.\n"),
	io__write_string("\t--no-smart-indexing\n"),
	io__write_string("\t\tGenerate switches as a simple if-then-else chains;\n"),
	io__write_string("\t\tdisable string hashing and integer table-lookup indexing.\n"),
	io__write_string("\t--req-density <percentage>\n"),
	io__write_string("\t\tThe jump table generated for an atomic switch\n"),
	io__write_string("\t\tmust have at least this percentage of full slots (default: 25).\n"),
	io__write_string("\t--dense-switch-size <n>\n"),
	io__write_string("\t\tThe jump table generated for an atomic switch\n"),
	io__write_string("\t\tmust have at least this many entries (default: 4).\n"),
	io__write_string("\t--string-switch-size <n>\n"),
	io__write_string("\t\tThe hash table generated for a string switch\n"),
	io__write_string("\t\tmust have at least this many entries (default: 8).\n"),
	io__write_string("\t--tag-switch-size <n>\n"),
	io__write_string("\t\tThe number of alternatives in a tag switch\n"),
	io__write_string("\t\tmust be at least this number (default: 8).\n"),
	io__write_string("\t--no-middle-rec\n"),
	io__write_string("\t\tDisable the middle recursion optimization.\n"),
	io__write_string("\t--no-inlining\n"),
	io__write_string("\t\tDisable the inlining of simple procedures.\n"),
	% io__write_string("\t--common-struct\n"),
	% io__write_string("\t\tEnable optimisation of common term structures.\n"),
	io__write_string("\t--no-common-goal\n"),
	io__write_string("\t\tDisable optimisation of common goals.\n"),
	io__write_string("\t\tAt the moment this optimisation\n"),
	io__write_string("\t\tdetects only common deconstruction unifications.\n"),
	io__write_string("\t\tDisabling this optimisation reduces the class of predicates\n"),
	io__write_string("\t\tthat the compiler considers to be deterministic.\n"),
	io__write_string("\t--procs-per-c-function <n>\n"),
	io__write_string("\t\tDon't put the code for more than <n> Mercury\n"),
	io__write_string("\t\tprocedures in a single C function.  The default\n"),
	io__write_string("\t\tvalue of <n> is one.  Increasing <n> can produce\n"),
	io__write_string("\t\tslightly more efficient code, but makes compilation slower.\n"),
	io__write_string("\t\tSetting <n> to the special value zero has the effect of\n"),
	io__write_string("\t\tputting all the procedures in a single function,\n"),
	io__write_string("\t\twhich produces the most efficient code but tends to\n"),
	io__write_string("\t\tseverely stress the C compiler on large modules.\n"),
	% io__write_string("\t--constraint-propagation\n"),
	% io__write_string("\t\tEnable the C-tranformation.  (Doesn't work.)\n"),
	io__write_string("\t-O-, --no-c-optimize\n"),
	io__write_string("\t\tDon't enable the C compiler's optimizations.\n"),

	io__write_string("\nMiscellaneous Options:\n"),
	% io__write_string("\t-H <n>, --heap-space <n>\n"),
	% io__write_string("\t\tPre-allocate <n> kilobytes of heap space.\n"),
	% io__write_string("\t\tThis option is now obsolete.  In the past it\n"),
	% io__write_string("\t\twas used to avoid NU-Prolog's\n"),
	% io__write_string("\t\t\t""Panic: growing stacks has required shifting the heap""\n"),
	% io__write_string("\t\tmessage.\n"),

	io__write_string("\t-b <builtin>, --builtin-module <builtin>\n"),
	io__write_string("\t\tUse `<builtin>' instead of `mercury_builtin' as the \n\t\tmodule which always gets automatically imported.\n"),
	io__write_string("\t-I <dir>, --search-directory <dir>\n"),
	io__write_string("\t\tAdd <dir> to the list of directories to be searched for \n\t\timported modules.\n").

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
