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
		
:- type option		--->	verbose
			;	very_verbose
			;	dump_hlds
			;	generate_code
			;	builtin_module.

:- pred short_option(character::i, option::output) is semidet.
:- pred long_option(string::i, option::output) is semidet.
:- pred option_defaults(list(pair(option, option_data))::output) is det.

% A couple of misc utilities

:- pred maybe_report_stats(bool::input, io__state::di, io__state::uo).
:- pred maybe_write_string(bool::input, string::input,
			io__state::di, io__state::uo).

:- implementation.

option_defaults([
	verbose		-	bool(no),
	very_verbose	-	bool(no),
	dump_hlds	-	bool(no),
	generate_code	-	bool(no),
	builtin_module	-	string("mercury_builtin")
]).

short_option('v', 		verbose).
short_option('w', 		very_verbose).
short_option('d', 		dump_hlds).
short_option('g', 		generate_code).
short_option('b', 		builtin_module).

long_option("verbose",		verbose).
long_option("very-verbose",	very_verbose).
long_option("dump-hlds",	dump_hlds).
long_option("generate-code",	generate_code).
long_option("builtin-module",	builtin_module).

maybe_report_stats(yes) --> io__report_stats.
maybe_report_stats(no) --> [].

maybe_write_string(yes, String) --> io__write_string(String).
maybe_write_string(no, _) --> [].

:- end_module options.

%-----------------------------------------------------------------------------%
