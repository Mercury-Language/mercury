%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: handle_options.m.
% Main authors: fjh, zs.

% This module does post-procesing on the command-line options, after
% getopt has done its stuff.

%-----------------------------------------------------------------------------%

:- module handle_options.

:- interface.
:- import_module list, bool, std_util, io.

:- pred handle_options(maybe(string), list(string), bool, io__state, io__state).
:- mode handle_options(out, out, out, di, uo) is det.

	% Display error message and then usage message
:- pred usage_error(string::in, io__state::di, io__state::uo) is det.

	% Display usage message
:- pred usage(io__state::di, io__state::uo) is det.

	% Display long usage message for help
:- pred long_usage(io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module options, globals, prog_io_util.
:- import_module int, string, map, getopt, library.

handle_options(MaybeError, Args, Link) -->
	io__command_line_arguments(Args0),
	% io__write_string("original arguments\n"),
	% dump_arguments(Args0),
	{ OptionOps = option_ops(short_option, long_option,
		option_defaults, special_handler) },
	{ getopt__process_options(OptionOps, Args0, Args, Result) },
	% io__write_string("final arguments\n"),
	% dump_arguments(Args),
	postprocess_options(Result, MaybeError),
	( { MaybeError = yes(_) } ->
		{ Link = no }
	;
		globals__io_lookup_bool_option(generate_dependencies,
			GenerateDependencies),
		globals__io_lookup_bool_option(make_interface, MakeInterface),
		globals__io_lookup_bool_option(make_short_interface,
						MakeShortInterface),
		globals__io_lookup_bool_option(make_optimization_interface,
						MakeOptimizationInt),
		globals__io_lookup_bool_option(convert_to_mercury,
			ConvertToMercury),
		globals__io_lookup_bool_option(convert_to_goedel,
			ConvertToGoedel),
		globals__io_lookup_bool_option(typecheck_only, TypecheckOnly),
		globals__io_lookup_bool_option(errorcheck_only, ErrorcheckOnly),
		globals__io_lookup_bool_option(compile_to_c, CompileToC),
		globals__io_lookup_bool_option(compile_only, CompileOnly),
		{ bool__or_list([GenerateDependencies,
			MakeInterface, MakeShortInterface, MakeOptimizationInt,
			ConvertToMercury, ConvertToGoedel, TypecheckOnly,
			ErrorcheckOnly, CompileToC, CompileOnly], NotLink) },
		{ bool__not(NotLink, Link) }
	).

:- pred dump_arguments(list(string), io__state, io__state).
:- mode dump_arguments(in, di, uo) is det.

dump_arguments([]) --> [].
dump_arguments([Arg | Args]) -->
	io__write_string("<"),
	io__write_string(Arg),
	io__write_string(">\n"),
	dump_arguments(Args).

%-----------------------------------------------------------------------------%

% Convert string-valued options into the appropriate enumeration types,
% and process implications among the options (i.e. situations where setting
% one option implies setting/unsetting another one).

:- pred postprocess_options(maybe_option_table(option), maybe(string),
	io__state, io__state).
:- mode postprocess_options(in, out, di, uo) is det.

postprocess_options(error(ErrorMessage), yes(ErrorMessage)) --> [].
postprocess_options(ok(OptionTable0), Error) -->
        { map__lookup(OptionTable0, grade, GradeOpt) },
        (
            { GradeOpt = string(GradeStr) },
            { convert_grade_option(GradeStr, OptionTable0, OptionTable) }
        ->
            { map__lookup(OptionTable, gc, GC_Method0) },
            (
                { GC_Method0 = string(GC_MethodStr) },
                { convert_gc_method(GC_MethodStr, GC_Method) }
            ->
                { map__lookup(OptionTable, tags, TagsMethod0) },
                (
                    { TagsMethod0 = string(TagsMethodStr) },
                    { convert_tags_method(TagsMethodStr, TagsMethod) }
                ->
                    { map__lookup(OptionTable, args, ArgsMethod0) },
                    (
                        { ArgsMethod0 = string(ArgsMethodStr) },
                        { convert_args_method(ArgsMethodStr, ArgsMethod) }
                    ->
                        { map__lookup(OptionTable, type_info,
                            TypeInfoMethod0) },
                        (
                            { TypeInfoMethod0 = string(TypeInfoMethodStr) },
                            { convert_type_info_method(TypeInfoMethodStr,
                                TypeInfoMethod) }
                        ->
                            { map__lookup(OptionTable, prolog_dialect,
                                PrologDialect0) },
                            (
                                { PrologDialect0 = string(PrologDialectStr) },
                                { convert_prolog_dialect(PrologDialectStr,
                                    PrologDialect) }
                            ->
				{ map__lookup(OptionTable,
					fact_table_hash_percent_full,
					PercentFull) },
				( 
				    { PercentFull = int(Percent) },
				    { Percent >= 1 },
				    { Percent =< 100 }
				->
				    postprocess_options_2(OptionTable,
				    	GC_Method, TagsMethod, ArgsMethod,
				    	TypeInfoMethod, PrologDialect),
				    { Error = no }
				;
				    { Error = yes("Invalid argument to option `--fact-table-hash-percent-full'\n                 (must be an integer between 1 and 100)") }
				)
                            ;
                                { Error = yes("Invalid prolog-dialect option (must be `sicstus', `nu', or `default')") }
                            )
                        ;
                            { Error = yes("Invalid type-info option (must be `shared-one-or-two-cell' or `default')") }
                        )
                    ;
                        { Error = yes("Invalid args option (must be `simple' or `compact')") }
                    )
                ;
                    { Error = yes("Invalid tags option (must be `none', `low' or `high')") }
                )
            ;
                { Error = yes("Invalid GC option (must be `none', `conservative' or `accurate')") }
            )
        ;
            { Error = yes("Invalid grade option") }
        ).

:- pred postprocess_options_2(option_table, gc_method, tags_method, args_method,
	type_info_method, prolog_dialect, io__state, io__state).
:- mode postprocess_options_2(in, in, in, in, in, in, di, uo) is det.

postprocess_options_2(OptionTable, GC_Method, TagsMethod, ArgsMethod,
		TypeInfoMethod, PrologDialect) -->
	% work around for NU-Prolog problems
	( { map__search(OptionTable, heap_space, int(HeapSpace)) }
	->
		io__preallocate_heap_space(HeapSpace)
	;
		[]
	),

	{ copy(OptionTable, OptionTable1) }, % XXX
	globals__io_init(OptionTable1, GC_Method, TagsMethod, ArgsMethod,
		TypeInfoMethod, PrologDialect),

	% --gc conservative implies --no-reclaim-heap-*
	( { GC_Method = conservative } ->
		globals__io_set_option(
			reclaim_heap_on_semidet_failure, bool(no)
		),
		globals__io_set_option(
			reclaim_heap_on_nondet_failure, bool(no)
		)
	;
		[]
	),

	% --tags none implies --num-tag-bits 0.
	( { TagsMethod = none } ->
		{ NumTagBits0 = 0 }
	;
		globals__io_lookup_int_option(num_tag_bits, NumTagBits0)
	),

	% if --tags low but --num-tag-bits not specified,
	% use the autoconf-determined value for --num-tag-bits
	% (the autoconf-determined value is passed from the `mc' script
	% using the undocumented --conf-low-tag-bits option)
	(
		{ TagsMethod = low },
		{ NumTagBits0 = -1 }
	->
		globals__io_lookup_int_option(conf_low_tag_bits, NumTagBits1)
	;	
		{ NumTagBits1 = NumTagBits0 }
	),

	% if --num-tag-bits negative or unspecified, issue a warning
	% and assume --num-tag-bits 0
	( { NumTagBits1 < 0 } ->
		io__progname_base("mercury_compile", ProgName),
		io__stderr_stream(StdErr),
		report_warning(ProgName),
		report_warning(": warning: --num-tag-bits invalid or unspecified\n"),
		io__write_string(StdErr, ProgName),
		report_warning(": using --num-tag-bits 0 (tags disabled)\n"),
		{ NumTagBits = 0 }
	;
		{ NumTagBits = NumTagBits1 }
	),

	globals__io_set_option(num_tag_bits, int(NumTagBits)),

	% --split-c-files implies --procs-per-c-function 1
	globals__io_lookup_bool_option(split_c_files, Split_C_Files),
	( { Split_C_Files = yes } ->
		globals__io_set_option(procs_per_c_function, int(1))
	;	
		[]
	),

	% --very-verbose implies --verbose
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		globals__io_set_option(verbose, bool(yes))
	;	
		[]
	),

	% -D all is really -D "abcdefghijklmnopqrstuvwxyz"
	globals__io_lookup_string_option(verbose_dump_hlds, VerboseDump),
	( { VerboseDump = "all" } ->
		globals__io_set_option(verbose_dump_hlds, string("abcdefghijklmnopqrstuvwxyz"))
	;	
		[]
	),

	% --dump-hlds and --statistics require compilation by phases
	globals__io_lookup_accumulating_option(dump_hlds, DumpStages),
	globals__io_lookup_bool_option(statistics, Statistics),
	( { DumpStages \= [] ; Statistics = yes } ->
		globals__io_set_option(trad_passes, bool(no))
	;
		[]
	),
	% --intermod-unused-args implies --intermodule-optimization and
	% --optimize-unused-args.
	globals__io_lookup_bool_option(intermod_unused_args, Intermod),
	( { Intermod = yes } ->
		globals__io_set_option(intermodule_optimization, bool(yes)),
		globals__io_set_option(optimize_unused_args, bool(yes))
	;
		[]
	),
	% Don't do the unused_args optimization when making the
	% optimization interface.
	globals__io_lookup_bool_option(make_optimization_interface, MakeOpt),
	( { MakeOpt = yes } ->
		globals__io_set_option(optimize_unused_args, bool(no))
	;
		[]
	),
	% If --use-search-directories-for-intermod is true, append the
	% search directories to the list of directories to search for
	% .opt files.
	globals__io_lookup_bool_option(use_search_directories_for_intermod,
		UseSearchDirs),
	( { UseSearchDirs = yes } ->
		globals__io_lookup_accumulating_option(intermod_directories,
			IntermodDirs0),
		globals__io_lookup_accumulating_option(search_directories,
			SearchDirs),
		{ list__append(IntermodDirs0, SearchDirs, IntermodDirs) },
		globals__io_set_option(intermod_directories,
			accumulating(IntermodDirs))
	;
		[]
	).

:- pred convert_grade_option(string::in, option_table::in, option_table::out)
	is semidet.

convert_grade_option(Grade0) -->
	( { string__remove_suffix(Grade0, ".cnstr", Grade1) } ->
		{ Grade2 = Grade1 },
		set_bool_opt(constraints, yes)
	;
		{ Grade2 = Grade0 },
		set_bool_opt(constraints, no)
	),
	( { string__remove_suffix(Grade2, ".prof", Grade3) } ->
		{ Grade4 = Grade3 },
		set_bool_opt(profiling, yes)
	;
		{ Grade4 = Grade2 },
		set_bool_opt(profiling, no)
	),
	( { string__remove_suffix(Grade4, ".gc", Grade5) } ->
		{ Grade = Grade5 },
		{ GC = conservative }
	; { string__remove_suffix(Grade4, ".agc", Grade5) } ->
		{ Grade = Grade5 },
		{ GC = accurate }
	;
		{ Grade = Grade4 },
		{ GC = none }
	),
	% Set the type of gc that the grade option implies.
	% 'accurate' is now set in the grade, so we can override it here.
	( 
		{ GC = accurate }, 
		set_string_opt(gc, "accurate") 
	; 
		{ GC = conservative },
		set_string_opt(gc, "conservative")
	;
		{ GC = none },
		set_string_opt(gc, "none")
	),
	convert_grade_option_2(Grade).

:- pred convert_grade_option_2(string::in, option_table::in, option_table::out)
	is semidet.

convert_grade_option_2("asm_fast") -->
	set_bool_opt(debug, no),
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, yes),
	set_bool_opt(gcc_global_registers, yes),
	set_bool_opt(asm_labels, yes).
convert_grade_option_2("fast") -->
	set_bool_opt(debug, no),
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, yes),
	set_bool_opt(gcc_global_registers, yes),
	set_bool_opt(asm_labels, no).
convert_grade_option_2("asm_jump") -->
	set_bool_opt(debug, no),
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, yes),
	set_bool_opt(gcc_global_registers, no),
	set_bool_opt(asm_labels, yes).
convert_grade_option_2("jump") -->
	set_bool_opt(debug, no),
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, yes),
	set_bool_opt(gcc_global_registers, no),
	set_bool_opt(asm_labels, no).
convert_grade_option_2("reg") -->
	set_bool_opt(debug, no),
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, no),
	set_bool_opt(gcc_global_registers, yes),
	set_bool_opt(asm_labels, no).
convert_grade_option_2("none") -->
	set_bool_opt(debug, no),
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, no),
	set_bool_opt(gcc_global_registers, no),
	set_bool_opt(asm_labels, no).
convert_grade_option_2("debug") -->
	set_bool_opt(debug, yes),
	set_bool_opt(c_optimize, no),
	set_bool_opt(gcc_non_local_gotos, no),
	set_bool_opt(gcc_global_registers, no),
	set_bool_opt(asm_labels, no).

:- pred set_bool_opt(option, bool, option_table, option_table).
:- mode set_bool_opt(in, in, in, out) is det.

set_bool_opt(Option, Value, OptionTable0, OptionTable) :-
	map__set(OptionTable0, Option, bool(Value), OptionTable).

:- pred set_string_opt(option, string, option_table, option_table).
:- mode set_string_opt(in, in, in, out) is det.

set_string_opt(Option, Value, OptionTable0, OptionTable) :-
	map__set(OptionTable0, Option, string(Value), OptionTable).

:- pred get_string_opt(option, string, option_table, option_table).
:- mode get_string_opt(in, in, in, out) is semidet.

get_string_opt(Option, Value, OptionTable, OptionTable) :-
	map__lookup(OptionTable, Option, string(Value)).

usage_error(ErrorMessage) -->
	io__progname_base("mercury_compile", ProgName),
	io__stderr_stream(StdErr),
	io__write_string(StdErr, ProgName),
	io__write_string(StdErr, ": "),
	io__write_string(StdErr, ErrorMessage),
	io__write_string(StdErr, "\n"),
	io__set_exit_status(1),
	usage.

usage -->
	io__progname_base("mercury_compile", ProgName),
	io__stderr_stream(StdErr),
	{ library__version(Version) },
 	io__write_strings(StdErr,
			["Mercury Compiler, version ", Version, "\n"]),
 	io__write_string(StdErr,
			"Copyright (C) 1995 University of Melbourne\n"),
	io__write_string(StdErr, "Usage: "),
	io__write_string(StdErr, ProgName),
	io__write_string(StdErr, " [<options>] <module(s)>\n"),
	io__write_string(StdErr, "Use `"),
	io__write_string(StdErr, ProgName),
	io__write_string(StdErr, " --help' for more information.\n").

long_usage -->
	io__progname_base("mercury_compile", ProgName),
	{ library__version(Version) },
 	io__write_strings(["Mercury Compiler, version ", Version, "\n"]),
 	io__write_string("Copyright (C) 1995 University of Melbourne\n"),
	io__write_string("Usage: "),
	io__write_string(ProgName),
	io__write_string(" [<options>] <module(s)>\n"),
	io__write_string("Options:\n"),
	options_help.
