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
			;	int(int)	% not yet implemented
			;	string(string)	% not yet implemented
			;	accumulating(list(string)). % not yet imp.
		
:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_defaults(list(pair(option, option_data))::output) is det.

:- pred options_help(io__state::di, io__state::uo).

% A couple of misc utilities

:- pred maybe_report_stats(bool::input, io__state::di, io__state::uo).
:- pred maybe_write_string(bool::input, string::input,
			io__state::di, io__state::uo).

:- type option		--->	verbose
			;	very_verbose
			;	verbose_errors
			;	statistics
			;	dump_hlds
			;	generate_code
			;	builtin_module
			;	make_interface
			;	heap_space.

:- implementation.

option_defaults([
	verbose		-	bool(no),
	very_verbose	-	bool(no),
	verbose_errors	-	bool(no),
	statistics	-	bool(no),
	dump_hlds	-	bool(no),
	generate_code	-	bool(no),
	builtin_module	-	string("mercury_builtin"),
	make_interface	-	bool(no),
	heap_space	-	int(0)
]).

short_option('v', 		verbose).
short_option('w', 		very_verbose).
short_option('e', 		verbose_errors).
short_option('s', 		statistics).
short_option('d', 		dump_hlds).
short_option('g', 		generate_code).
short_option('b', 		builtin_module).
short_option('i', 		make_interface).
short_option('h', 		heap_space).

long_option("verbose",		verbose).
long_option("very-verbose",	very_verbose).
long_option("verbose-error-messages",	verbose_errors).
long_option("statistics",	statistics).
long_option("dump-hlds",	dump_hlds).
long_option("generate-code",	generate_code).
long_option("builtin-module",	builtin_module).
long_option("make-interface",	make_interface).
long_option("heap-space",	heap_space).

options_help -->
	io__write_string(StdErr, "\t-v, --verbose\n"),
	io__write_string(StdErr, "\t\tOutput progress messages at each stage in the compilation.\n"),
	io__write_string(StdErr, "\t-w, --very_verbose\n"),
	io__write_string(StdErr, "\t\tOutput very verbose progress messages.\n"),
	io__write_string(StdErr, "\t-e, --verbose-error-messages\n"),
	io__write_string(StdErr, "\t\tMake error messages more verbose than usual.\n"),
	io__write_string(StdErr, "\t-s, --statistics\n"),
	io__write_string(StdErr, "\t\tOutput messages about the compiler's time/space usage\n"),
	io__write_string(StdErr, "\t-d, --dump-hlds\n"),
	io__write_string(StdErr, "\t\tDump the HLDS to `<module>.hlds'.\n"),
	io__write_string(StdErr, "\t-g, --generate-code\n"),
	io__write_string(StdErr, "\t\tGenerate .mod code in `xxx.xmod'.\n"),
	io__write_string(StdErr, "\t-b <builtin>, --builtin-module <builtin>\n"),
	io__write_string(StdErr, "\t\tUse `<builtin>' instead of `mercury_builtin' as the \n\t\tmodule which always gets automatically imported.\n"),
	io__write_string(StdErr, "\t-i, --make-interface\n"),
	io__write_string(StdErr, "\t\tWrite the module interface to `<module>.int'.\n"),
	io__write_string(StdErr, "\t\tOnly syntax analysis will be performed - this option\n"),
	io__write_string(StdErr, "\t\tdisables all the later phases of compilation.\n"),
	io__write_string(StdErr, "\t-h <n>, --heap-space <n>\n"),
	io__write_string(StdErr, "\t\tPre-allocate <n> kilobytes of heap space.\n"),
	io__write_string(StdErr, "\t\tUse this option to avoid NU-Prolog's\n"),
	io__write_string(StdErr, "\t\t\t\"Panic: growing stacks has required shifting the heap\"\n"),
	io__write_string(StdErr, "\t\tmessage.\n").

maybe_report_stats(yes) --> io__report_stats.
maybe_report_stats(no) --> [].

maybe_write_string(yes, String) --> io__write_string(String).
maybe_write_string(no, _) --> [].

:- end_module options.

%-----------------------------------------------------------------------------%
