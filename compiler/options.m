%-----------------------------------------------------------------------------%

	% Define the stuff necessary so that getopt.nl
	% can parse the command-line options.
	% When we implement higher-order preds, this and 
	% getopt.nl should be rewritten to use them.
	% Currently the interface dependencies are very hairy.

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
	% Output options
		;	make_interface
		;	convert_to_mercury
		;	convert_to_goedel
		;	modecheck
		;	dump_hlds
		;	verbose_dump_hlds
		;	generate_code
		;	compile_to_c
		;	compile
		;	link
		;	line_numbers
		;	mod_comments
	% Code generation options
		;	lazy_code
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
	% Optimisation Options
		;	peephole
		;	peephole_local
		;	peephole_jump_opt
		;	peephole_label_elim
		;	peephole_opt_redoip
		;	peephole_value_number
		;	peephole_frame_opt
		;	peephole_repeat
		;	static_ground_terms
		;	smart_indexing
		;	optimize
		;	debug
		;	grade
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
	debug_modes		- 	bool(no)
]).
option_defaults_2(output_option, [
		% Output Options
	generate_dependencies	-	bool(no),
	make_interface		-	bool(no),
	convert_to_mercury 	-	bool(no),
	convert_to_goedel 	-	bool(no),
	modecheck		-	bool(yes),
	dump_hlds		-	bool(no),
	verbose_dump_hlds	-	bool(no),
	generate_code		-	bool(no),
	line_numbers		-	bool(no),
	mod_comments		-	bool(yes)
]).
option_defaults_2(code_gen_option, [
		% Code Generation Options
	tags			-	string("low"),
	follow_code		-	bool(yes),
	follow_vars		-	bool(yes),
	lazy_code		-	bool(yes),
	reclaim_heap_on_semidet_failure	-	bool(yes),
	reclaim_heap_on_nondet_failure	-	bool(yes),
	num_tag_bits		-	int(2),
	gc			-	string("none"),
	compile_to_c		-	bool(no),
	compile			-	bool(no),
	cc			-	string("gcc"),
	cflags			-	string(""),
	c_include_directory	-	string(""),
	link			-	bool(no),
	gcc_non_local_gotos	-	bool(no),
	gcc_global_registers	-	bool(no)
]).
option_defaults_2(optimization_option, [
		% Optimization options
	debug			-	bool(no),
	optimize		-	bool(no),
	grade			-	string(""),
	peephole		-	bool(yes),
	peephole_local		-	bool(yes),
	peephole_jump_opt	-	bool(yes),
	peephole_label_elim	-	bool(yes),
	peephole_opt_redoip	-	bool(yes),
	peephole_value_number	-	bool(no),
	peephole_frame_opt	-	bool(yes),
	peephole_repeat		-	int(4),
	static_ground_terms	-	bool(yes),
	smart_indexing		-	bool(yes)
]).
option_defaults_2(miscellaneous_option, [
		% Miscellaneous Options
	builtin_module		-	string("mercury_builtin"),
	heap_space		-	int(0),
	search_directories 	-	accumulating(["."]),
	help 			-	bool(no)
]).

	% please keep this in alphabetic order
short_option('b', 			builtin_module).
short_option('c', 			compile).
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
short_option('O', 			optimize).
short_option('P', 			convert_to_mercury).
short_option('S', 			statistics).
short_option('s', 			grade).
short_option('T', 			debug_types).
short_option('v', 			verbose).
short_option('x', 			smart_indexing).
short_option('V', 			very_verbose).
short_option('w', 			warn_singleton_vars).

long_option("grade",			grade).
long_option("optimize",			optimize).
long_option("debug",			debug).
long_option("verbose",			verbose).
long_option("very-verbose",		very_verbose).
long_option("verbose-error-messages",	verbose_errors).
long_option("statistics",		statistics).
long_option("dump-hlds",		dump_hlds).
long_option("verbose-dump-hlds",	verbose_dump_hlds).
long_option("generate-code",		generate_code).
long_option("builtin-module",		builtin_module).
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
long_option("compile-to-C",		compile_to_c).
long_option("compile-to-c",		compile_to_c).
long_option("compile",			compile).
long_option("cc",			cc).
long_option("cflags",			cflags).
long_option("c-include-directory",	c_include_directory).
long_option("link",			link).
long_option("gcc-non-local-gotos",	gcc_non_local_gotos).
long_option("gcc-global-registers",	gcc_global_registers).
long_option("mod-comments",		mod_comments).
long_option("peephole",			peephole).
long_option("peephole-local",		peephole_local).
long_option("peephole-jump-opt",	peephole_jump_opt).
long_option("peephole-label-elim",	peephole_label_elim).
long_option("peephole-opt-redoip",	peephole_opt_redoip).
long_option("peephole-value-number",	peephole_value_number).
long_option("peephole-frame-opt",	peephole_frame_opt).
long_option("peephole-repeat",		peephole_repeat).
long_option("static-ground-terms",	static_ground_terms).
long_option("smart-indexing",		smart_indexing).

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
	io__write_string("\t-s, --statistics\n"),
	io__write_string("\t\tOutput messages about the compiler's time/space usage\n"),
	io__write_string("\t-T, --debug-types\n"),
	io__write_string("\t\tOutput detailed debugging traces of the type checking.\n"),
	io__write_string("\t-N, --debug-modes\n"),
	io__write_string("\t\tOutput detailed debugging traces of the mode checking.\n"),

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
	io__write_string("\t-G, --convert-to-goedel\n"),
	io__write_string("\t\tConvert to Goedel. Output to file `<module>.loc'\n"),
	io__write_string("\t\tAs with -M, this disables type-checking, etc.\n"),
	io__write_string("\t-P, --convert-to-mercury\n"),
	io__write_string("\t\tConvert to Mercury. Output to file `<module>.ugly'\n"),
	io__write_string("\t\tThis option acts as a Mercury ugly-printer.\n"),
	io__write_string("\t\tAs with -M, it disables type-checking, etc.\n"),
	io__write_string("\t-m-, --no-modecheck\n"),
	io__write_string("\t\tDon't invoke the mode analysis pass of the compiler.\n"),
	io__write_string("\t-d, --dump-hlds\n"),
	io__write_string("\t\tDump the HLDS (intermediate representation) to\n"),
	io__write_string("\t\t`<module>.hlds_dump'.\n"),
	io__write_string("\t-D, --verbose-dump-hlds\n"),
	io__write_string("\t\tWith --dump-hlds, dumps some additional info.\n"),
	io__write_string("\t-g, --generate-code\n"),
	io__write_string("\t\tGenerate .mod code in `<module>.mod'.\n"),
	io__write_string("\t--compile-to-C\n"),
	io__write_string("\t\tConvert the generated .mod file to a .c file.\n"),
	io__write_string("\t--compile\n"),
	io__write_string("\t\tInvoke the C compiler on the generated .c file.\n"),
	io__write_string("\t--link (*** NOT YET IMPLEMENTED ***)\n"),
	io__write_string("\t\tLink the named modules to produce an executable.\n"),
	io__write_string("\t--no-mod-comments\n"),
	io__write_string("\t\tDon't output comments in the .mod file\n"),
	io__write_string("\t-l, --line-numbers\n"),
	io__write_string("\t\tOutput line numbers in the generated code.\n"),
	io__write_string("\t\tCurrently only works with the -G and -M options.\n"),

	io__write_string("\nCode generation options\n"),
	io__write_string("\t-s {debug, none, reg, jump, fast}\n"),
	io__write_string("\t--grade {debug, none, reg, jump, fast}\n"),
	io__write_string("\t\tSelect the compilation model.  This is a convenient way of\n"),
	io__write_string("\t\tselecting a setting for the --optimize, --gcc-global-registers,\n"),
	io__write_string("\t\t--gcc-non-local-gotos, and --debug options simultaneously.\n"),

	io__write_string("\t--gc {none, conservative, accurate}\n"),
	io__write_string("\t--garbage-collection {none, conservative, accurate}\n"),
	io__write_string("\t\tSpecify which method of garbage collection to use.\n"),
	io__write_string("\t\t[Currently only `none' is implemented.]\n"),
	io__write_string("\t--no-follow-code\n"),
	io__write_string("\t\tDon't migrate builtin goals into branched goals\n"),
	io__write_string("\t--no-follow-vars\n"),
	io__write_string("\t\tDon't optimise the assignment of registers in branched goals\n"),
	io__write_string("\t--no-reclaim-heap-on-nondet-failure\n"),
	io__write_string("\t\tDon't reclaim heap on backtracking in nondet code.\n"),
	io__write_string("\t--no-reclaim-heap-on-semidet-failure\n"),
	io__write_string("\t\tDon't reclaim heap on backtracking in semidet code.\n"),
	io__write_string("\t--gcc-global-registers\n"),
	io__write_string("\t\tUse GNU C's global register variables extension.\n"),
	io__write_string("\t--gcc-non-local-gotos\n"),
	io__write_string("\t\tUse GNU C's \"labels as values\" extension.\n"),
	io__write_string("\t--tags {none, low, high}\n"),
	io__write_string("\t\tSpecify whether to use the low bits or the high bits of \n"),
	io__write_string("\t\teach word as tag bits (default: low).\n"),
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
	io__write_string("\t--no-peephole\n"),
	io__write_string("\t\tDisable the peephole optimisation pass.\n"),
	io__write_string("\t--no-peephole-local\n"),
	io__write_string("\t\tDisable pattern matching optimisations.\n"),
	io__write_string("\t--no-peephole-jump-opt\n"),
	io__write_string("\t\tDisable elimination of jumps to jumps.\n"),
	io__write_string("\t--no-peephole-label-elim\n"),
	io__write_string("\t\tDisable elimination of useless labels\n"),
	io__write_string("\t--no-peephole-opt-redoip\n"),
	io__write_string("\t\tDisable optimizations of redoips\n"),
	io__write_string("\t--peephole-value-number\n"),
	io__write_string("\t\tPerform value numbering\n"),
	io__write_string("\t--no-peephole-frame-opt\n"),
	io__write_string("\t\tDisable stack frame optimizations\n"),
	io__write_string("\t--peephole-repeat <n>\n"),
	io__write_string("\t\tIterate peephole optimizations at most <n> times\n"),
	io__write_string("\t--no-static-ground-terms\n"),
	io__write_string("\t\tConstruct all terms at runtime; disable the optimization\n"),
	io__write_string("\t\tof constructing constant ground terms at compile time\n"),
	io__write_string("\t\tand storing them as static constants.\n"),
	io__write_string("\t--no-smart-indexing\n"),
	io__write_string("\t\tGenerate deterministic switches as a simple if-then-else chain;\n"),
	io__write_string("\t\tdisable string hashing and integer table-lookup indexing.\n"),
	io__write_string("\t--optimize\n"),
	io__write_string("\t\tEnable the C compiler's optimizations.\n"),

	io__write_string("\nMiscellaneous Options:\n"),
	io__write_string("\t-H <n>, --heap-space <n>\n"),
	io__write_string("\t\tPre-allocate <n> kilobytes of heap space.\n"),
	io__write_string("\t\tUse this option to avoid NU-Prolog's\n"),
	io__write_string("\t\t\t\"Panic: growing stacks has required shifting the heap\"\n"),
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
