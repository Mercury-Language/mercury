%-----------------------------------------------------------------------------%

	% Define the stuff necessary so that getopt.nl
	% can parse the command-line options.
	% When we implement higher-order preds, this and 
	% getopt.nl should be rewritten to use them.
	% Currently the interface dependencies are very hairy.

:- module options.
:- interface.
:- import_module int, string, std_util, list, io.

:- type option_data	--->	bool(bool)
			;	int(int)
			;	string(string)
			;	accumulating(list(string)).
		
:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_defaults(list(pair(option, option_data))::output) is det.

:- pred options_help(io__state::di, io__state::uo) is det.

% A couple of misc utilities

:- pred maybe_report_stats(bool::in, io__state::di, io__state::uo) is det.
:- pred maybe_write_string(bool::input, string::input,
			io__state::di, io__state::uo) is det.
:- pred maybe_flush_output(bool::in, io__state::di, io__state::uo) is det.

:- type option		--->	verbose
			;	very_verbose
			;	verbose_errors
			;	statistics
			;	dump_hlds
			;	verbose_dump_hlds
			;	generate_code
			;	generate_dependencies
			;	builtin_module
			;	make_interface
			;	heap_space
			;	search_directories
			;	convert_to_mercury
			;	convert_to_goedel
			;	help
			;	line_numbers
			;	warn_singleton_vars
			;	warn_missing_det_decls
			;	warn_det_decls_too_lax
			;	modecheck
			;	debug_types
			;	debug_modes
			;	tags
			;	num_tag_bits
			;	gc
			;	compile_to_c
			;	compile
			;	cc
			;	cflags
			;	link
			;	gcc_non_local_gotos
			;	gcc_global_registers.

:- implementation.

option_defaults([
	verbose			-	bool(no),
	very_verbose		-	bool(no),
	verbose_errors		-	bool(no),
	statistics		-	bool(no),
	dump_hlds		-	bool(no),
	verbose_dump_hlds	-	bool(no),
	generate_code		-	bool(no),
	generate_dependencies	-	bool(no),
	builtin_module		-	string("mercury_builtin"),
	make_interface		-	bool(no),
	heap_space		-	int(0),
	search_directories 	-	accumulating(["."]),
	convert_to_mercury 	-	bool(no),
	convert_to_goedel 	-	bool(no),
	help 			-	bool(no),
	line_numbers		-	bool(no),
	warn_singleton_vars	-	bool(yes),
	warn_missing_det_decls	-	bool(yes),
	warn_det_decls_too_lax	-	bool(yes),
	modecheck		-	bool(yes),
	debug_types		- 	bool(no),
	debug_modes		- 	bool(no),
	tags			-	string("low"),
	num_tag_bits		-	int(2),
	gc			-	string("none"),
	compile_to_c		-	bool(no),
	compile			-	bool(no),
	cc			-	string("gcc"),
	cflags			-	string(""),
	link			-	bool(no),
	gcc_non_local_gotos	-	bool(no),
	gcc_global_registers	-	bool(no)
]).

short_option('v', 			verbose).
short_option('V', 			very_verbose).
short_option('w', 			warn_singleton_vars).
short_option('e', 			verbose_errors).
short_option('s', 			statistics).
short_option('d', 			dump_hlds).
short_option('D', 			verbose_dump_hlds).
short_option('g', 			generate_code).
short_option('b', 			builtin_module).
short_option('i', 			make_interface).
short_option('H', 			heap_space).
short_option('h', 			help).
short_option('I', 			search_directories).
short_option('P', 			convert_to_mercury).
short_option('G', 			convert_to_goedel).
short_option('l', 			line_numbers).
short_option('m', 			modecheck).
short_option('T', 			debug_types).
short_option('N', 			debug_modes).
short_option('M', 			generate_dependencies).
short_option('c', 			compile).

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
long_option("num-tag-bits",		num_tag_bits).
long_option("gc",			gc).
long_option("garbage-collection",	gc).
long_option("compile-to-C",		compile_to_c).
long_option("compile-to-c",		compile_to_c).
long_option("compile",			compile).
long_option("cc",			cc).
long_option("cflags",			cflags).
long_option("link",			link).
long_option("gcc-non-local-gotos",	gcc_non_local_gotos).
long_option("gcc-global-registers",	gcc_global_registers).

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
	io__write_string("\t-M, --debug-modes\n"),
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
	io__write_string("\t-l, --line-numbers\n"),
	io__write_string("\t\tOutput line numbers in the generated code.\n"),
	io__write_string("\t\tCurrently only works with the -G and -M options.\n"),
	io__write_string("\nCode generation options (**MOSTLY NOT YET IMPLEMENTED**)\n"),
	io__write_string("\t--gcc-global-registers\n"),
	io__write_string("\t\tUse GNU C's global register variables extension.\n"),
	io__write_string("\t--gcc-non-local-gotos\n"),
	io__write_string("\t\tUse GNU C's \"labels as values\" extension.\n"),
	io__write_string("\t--gc {none, conservative, accurate}\n"),
	io__write_string("\t--garbage-collection {none, conservative, accurate}\n"),
	io__write_string("\t\tSpecify which method of garbage collection to use.\n"),
	io__write_string("\t--tags {none, low, high}\n"),
	io__write_string("\t\tSpecify whether to use the low bits or the high bits of \n"),
	io__write_string("\t\teach word as tag bits (default: low).\n"),
	io__write_string("\t--num-tag-bits <n>\n"),
	io__write_string("\t\tUse <n> tag bits (used with `--tags high').\n"),
	io__write_string("\t--compile-to-C\n"),
	io__write_string("\t\tConvert the generated .mod file to a .c file.\n"),
	io__write_string("\t--compile\n"),
	io__write_string("\t\tInvoke the C compiler on the generated .c file.\n"),
	io__write_string("\t--cc <compiler-name>\n"),
	io__write_string("\t\tSpecify which C compiler to use.\n"),
	io__write_string("\t--cflags <options>\n"),
	io__write_string("\t\tSpecify options to be passed to the C compiler\n"),
	io__write_string("\t--link\n"),
	io__write_string("\t\tLink the named modules to produce an executable.\n"),

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

:- end_module options.

%-----------------------------------------------------------------------------%
