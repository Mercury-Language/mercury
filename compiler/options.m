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
			;	very_verbose.

:- pred short_option(character::i, option::output) is semidet.
:- pred long_option(string::i, option::output) is semidet.
:- pred option_defaults(list(pair(option, option_data))::output) is det.

:- implementation.

option_defaults([
	verbose		 -	bool(no),
	very_verbose	 -	bool(no)
]).

short_option('v', 		verbose).
short_option('w', 		very_verbose).

long_option("verbose",		verbose).
long_option("very-verbose",	very_verbose).

:- end_module options.

%-----------------------------------------------------------------------------%
