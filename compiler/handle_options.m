%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: handle_options.m.
% Main authors: fjh, zs.

% This module does post-procesing on the command-line options, after
% getopt has done its stuff.

% It also contains code for handling the --grade option.

%-----------------------------------------------------------------------------%

:- module handle_options.

:- interface.
:- import_module list, bool, std_util, io.
:- import_module globals, options.

:- pred handle_options(maybe(string), list(string), bool, io__state, io__state).
:- mode handle_options(out, out, out, di, uo) is det.

	% Display error message and then usage message
:- pred usage_error(string::in, io__state::di, io__state::uo) is det.

	% Display usage message
:- pred usage(io__state::di, io__state::uo) is det.

	% Display long usage message for help
:- pred long_usage(io__state::di, io__state::uo) is det.


	% Given the current set of options, figure out
	% which grade to use.
:- pred compute_grade(globals::in, string::out) is det.

	% The inverse of compute_grade: given a grade,
	% set the appropriate options.
:- pred convert_grade_option(string::in, option_table::in, option_table::out)
	is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module options, globals, prog_io_util.
:- import_module char, int, string, map, set, getopt, library.

handle_options(MaybeError, Args, Link) -->
	io__command_line_arguments(Args0),
	% io__write_string("original arguments\n"),
	% dump_arguments(Args0),
	{ OptionOps = option_ops(short_option, long_option,
		option_defaults, special_handler) },
	% default to optimization level `-O2'
	{ Args1 = ["-O2" | Args0] },
	{ getopt__process_options(OptionOps, Args1, Args, Result) },
	% io__write_string("final arguments\n"),
	% dump_arguments(Args),
	postprocess_options(Result, MaybeError),
	( { MaybeError = yes(_) } ->
		{ Link = no }
	;
		globals__io_lookup_bool_option(generate_dependencies,
			GenerateDependencies),
		globals__io_lookup_bool_option(make_interface, MakeInterface),
		globals__io_lookup_bool_option(make_private_interface,
						MakePrivateInterface),
		globals__io_lookup_bool_option(make_short_interface,
						MakeShortInterface),
		globals__io_lookup_bool_option(make_optimization_interface,
						MakeOptimizationInt),
		globals__io_lookup_bool_option(make_transitive_opt_interface,
						MakeTransOptInt),
		globals__io_lookup_bool_option(convert_to_mercury,
			ConvertToMercury),
		globals__io_lookup_bool_option(convert_to_goedel,
			ConvertToGoedel),
		globals__io_lookup_bool_option(typecheck_only, TypecheckOnly),
		globals__io_lookup_bool_option(errorcheck_only, ErrorcheckOnly),
		globals__io_lookup_bool_option(compile_to_c, CompileToC),
		globals__io_lookup_bool_option(compile_only, CompileOnly),
		{ bool__or_list([GenerateDependencies, MakeInterface,
			MakePrivateInterface, MakeShortInterface,
			MakeOptimizationInt, MakeTransOptInt,
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
postprocess_options(ok(OptionTable), Error) -->
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
                { map__lookup(OptionTable, prolog_dialect, PrologDialect0) },
                (
                    { PrologDialect0 = string(PrologDialectStr) },
                    { convert_prolog_dialect(PrologDialectStr, PrologDialect) }
                ->
                    { map__lookup(OptionTable,
                        fact_table_hash_percent_full, PercentFull) },
                    (
                        { PercentFull = int(Percent) },
                        { Percent >= 1 },
                        { Percent =< 100 }
                    ->
                        { map__lookup(OptionTable, termination_norm,
                            TermNorm0) },
                        (
                            { TermNorm0 = string(TermNormStr) },
                            { convert_termination_norm(TermNormStr, TermNorm) }
                        ->
                            { map__lookup(OptionTable, trace, Trace) },
                            { map__lookup(OptionTable, require_tracing,
                                RequireTracingOpt) },
                            (
                                { Trace = string(TraceStr) },
                                { RequireTracingOpt = bool(RequireTracing) },
                                { convert_trace_level(TraceStr, RequireTracing,
                                    TraceLevel) }
                            ->
                                { map__lookup(OptionTable, dump_hlds_alias,
                                    DumpAliasOption) },
                                (
                                    { DumpAliasOption = string(DumpAlias) },
                                    { DumpAlias = "" }
                                ->
                                    postprocess_options_2(OptionTable,
                                        GC_Method, TagsMethod, ArgsMethod,
                                        PrologDialect, TermNorm, TraceLevel),
                                    { Error = no }
                                ;
                                    { DumpAliasOption = string(DumpAlias) },
                                    { convert_dump_alias(DumpAlias,
                                        DumpOptions) }
                                ->
                                    { map__set(OptionTable, dump_hlds_options,
                                        string(DumpOptions), NewOptionTable) },
                                    postprocess_options_2(NewOptionTable,
                                        GC_Method, TagsMethod, ArgsMethod,
                                        PrologDialect, TermNorm, TraceLevel),
                                    { Error = no }
                                ;
                                    { Error = yes("Invalid argument to option `--hlds-dump-alias'.") }
                                )
                            ;
                                { Error = yes("Invalid argument to option `--trace'\n\t(must be `minimum', `interfaces', `all', or `default').") }
                            )
                        ;
                            { Error = yes("Invalid argument to option `--termination-norm'\n\t(must be `simple', `total' or  `num-data-elems').") }
                        )
                    ;
                        { Error = yes("Invalid argument to option `--fact-table-hash-percent-full'\n\t(must be an integer between 1 and 100)") }
                    )
                ;
                    { Error = yes("Invalid prolog-dialect option (must be `sicstus', `nu', or `default')") }
                )
            ;
                { Error = yes("Invalid args option (must be `simple' or `compact')") }
            )
        ;
            { Error = yes("Invalid tags option (must be `none', `low' or `high')") }
        )
    ;
        { Error = yes("Invalid GC option (must be `none', `conservative' or `accurate')") }
    ).

:- pred postprocess_options_2(option_table, gc_method, tags_method,
	args_method, prolog_dialect, termination_norm, trace_level,
	io__state, io__state).
:- mode postprocess_options_2(in, in, in, in, in, in, in, di, uo) is det.

postprocess_options_2(OptionTable, GC_Method, TagsMethod, ArgsMethod,
		PrologDialect, TermNorm, TraceLevel) -->
	% work around for NU-Prolog problems
	( { map__search(OptionTable, heap_space, int(HeapSpace)) } ->
		io__preallocate_heap_space(HeapSpace)
	;
		[]
	),

	{ unsafe_promise_unique(OptionTable, OptionTable1) }, % XXX
	globals__io_init(OptionTable1, GC_Method, TagsMethod, ArgsMethod,
		PrologDialect, TermNorm, TraceLevel),

	% --gc conservative implies --no-reclaim-heap-*
	( { GC_Method = conservative } ->
		globals__io_set_option(
			reclaim_heap_on_semidet_failure, bool(no)),
		globals__io_set_option(
			reclaim_heap_on_nondet_failure, bool(no))
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
		report_warning(
			": warning: --num-tag-bits invalid or unspecified\n"),
		io__write_string(StdErr, ProgName),
		report_warning(": using --num-tag-bits 0 (tags disabled)\n"),
		{ NumTagBits = 0 }
	;
		{ NumTagBits = NumTagBits1 }
	),

	globals__io_set_option(num_tag_bits, int(NumTagBits)),

	option_implies(verbose_check_termination, check_termination,bool(yes)),
	option_implies(check_termination, termination, bool(yes)),
	option_implies(make_transitive_opt_interface, transitive_optimization,
		bool(yes)),
	option_implies(transitive_optimization, intermodule_optimization,
		bool(yes)),
	option_implies(use_trans_opt_files, use_opt_files, bool(yes)),

	% If we are doing full inter-module or transitive optimization,
	% we need to build all `.opt' or `.trans_opt' files.
	option_implies(intermodule_optimization, use_opt_files, bool(no)),
	option_implies(transitive_optimization, use_trans_opt_files, bool(no)),

	option_implies(very_verbose, verbose, bool(yes)),

	% --split-c-files implies --procs-per-c-function 1
	option_implies(split_c_files, procs_per_c_function, int(1)),

	% The `.debug' grade (i.e. --stack-trace plus --require-tracing)
	% implies --use-trail.
	%
	% The reason for this is to avoid unnecessary proliferation in
	% the number of different grades.  If you're using --debug,
	% you've already taken a major performance hit, so you should
	% be able to afford the minor performance hit caused by
	% --use-trail.

	globals__io_lookup_bool_option(stack_trace, StackTrace),
	globals__io_lookup_bool_option(require_tracing, RequireTracing),
	( { StackTrace = yes, RequireTracing = yes } ->
		globals__io_set_option(use_trail, bool(yes))
	;
		[]
	),

	% Execution tracing requires
	% 	- disabling optimizations that would change
	% 	  the trace being generated
	%	- enabling some low level optimizations to ensure consistent
	%	  paths across optimization levels
	% 	- enabling stack layouts
	% 	- enabling typeinfo liveness
	( { trace_level_trace_interface(TraceLevel, yes) } ->
			% The following options modify the structure
			% of the program, which makes it difficult to
			% relate the trace to the source code (although
			% it can be easily related to the transformed HLDS).
		globals__io_set_option(inline_simple, bool(no)),
		globals__io_set_option(inline_single_use, bool(no)),
		globals__io_set_option(inline_compound_threshold, int(0)),
		globals__io_set_option(optimize_unused_args, bool(no)),
		globals__io_set_option(optimize_higher_order, bool(no)),
		globals__io_set_option(type_specialization, bool(no)),
		globals__io_set_option(deforestation, bool(no)),
		globals__io_set_option(optimize_duplicate_calls, bool(no)),
		globals__io_set_option(optimize_constructor_last_call,
			bool(no)),

			% The following option prevents useless variables
			% from cluttering the trace. Its explicit setting
			% removes a source of variability in the goal paths
			% reported by tracing.
		globals__io_set_option(excess_assign, bool(yes)),
			% The explicit setting of the following option
			% removes a source of variability in the goal paths
			% reported by tracing.
		globals__io_set_option(follow_code, bool(yes)),
			% The following option selects a special-case
			% code generator that cannot (yet) implement tracing.
		globals__io_set_option(middle_rec, bool(no)),
			% Tracing inserts C code into the generated LLDS.
			% Value numbering cannot optimize such LLDS code.
			% (If it tried, it would get it wrong due to the
			% absence of liveness annotations on the introduced
			% labels.) We turn value numbering off now so that
			% we don't have to discover this fact anew
			% for each procedure.
		globals__io_set_option(optimize_value_number, bool(no)),
			% The following options cause the info required
			% by tracing to be generated.
		globals__io_set_option(trace_stack_layout, bool(yes)),
		globals__io_set_option(typeinfo_liveness, bool(yes))
	;
		[]
	),

	% --no-reorder-conj implies --no-deforestation.
	option_neg_implies(reorder_conj, deforestation, bool(no)),

	% --stack-trace requires `procid' stack layouts
	option_implies(stack_trace, procid_stack_layout, bool(yes)),

	% `trace' stack layouts need `procid' stack layouts
	option_implies(trace_stack_layout, procid_stack_layout, bool(yes)),

	% --gc accurate requires `agc' stack layouts, typeinfo liveness,
	% and needs hijacks and frameopt to be switched off.
	( { GC_Method = accurate } ->
		globals__io_set_option(agc_stack_layout, bool(yes)),
		globals__io_set_option(typeinfo_liveness, bool(yes)),
		globals__io_set_option(allow_hijacks, bool(no)),
		globals__io_set_option(optimize_frames, bool(no))
	;
		[]
	),

	% `procid' and `agc' stack layouts need `basic' stack layouts
	option_implies(procid_stack_layout, basic_stack_layout, bool(yes)),
	option_implies(agc_stack_layout, basic_stack_layout, bool(yes)),

	% XXX deforestation does not perform folding on polymorphic
	% predicates correctly with --typeinfo-liveness.
	option_implies(typeinfo_liveness, deforestation, bool(no)),

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
	option_implies(intermod_unused_args, intermodule_optimization,
		bool(yes)),
	option_implies(intermod_unused_args, optimize_unused_args, bool(yes)),

	% Don't do the unused_args optimization when making the
	% optimization interface.
	option_implies(make_optimization_interface, optimize_unused_args,
		bool(no)),

	% The information needed for generating the module ordering
	% is only available while generating the dependencies.
	option_implies(generate_module_order, generate_dependencies,
		bool(yes)),

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
	),

	% --use-opt-files implies --no-warn-missing-opt-files since
	% we are expecting some to be missing.
	option_implies(use_opt_files, warn_missing_opt_files, bool(no)),

	% --optimize-frames requires --optimize-labels and --optimize-jumps
	option_implies(optimize_frames, optimize_labels, bool(yes)),
	option_implies(optimize_frames, optimize_jumps, bool(yes)).

% option_implies(SourceBoolOption, ImpliedOption, ImpliedOptionValue, IO0, IO).
% If the SourceBoolOption is set to yes, then the ImpliedOption is set
% to ImpliedOptionValue.
:- pred option_implies(option::in, option::in, option_data::in,
	io__state::di, io__state::uo) is det.

option_implies(SourceOption, ImpliedOption, ImpliedOptionValue) -->
	globals__io_lookup_bool_option(SourceOption, SourceOptionValue),
	( { SourceOptionValue = yes } ->
		globals__io_set_option(ImpliedOption, ImpliedOptionValue)
	;
		[]
	).

% option_neg_implies(SourceBoolOption, ImpliedOption,
%	ImpliedOptionValue, IO0, IO).
% If the SourceBoolOption is set to no, then the ImpliedOption is set
% to ImpliedOptionValue.
:- pred option_neg_implies(option::in, option::in, option_data::in,
	io__state::di, io__state::uo) is det.

option_neg_implies(SourceOption, ImpliedOption, ImpliedOptionValue) -->
	globals__io_lookup_bool_option(SourceOption, SourceOptionValue),
	( { SourceOptionValue = no } ->
		globals__io_set_option(ImpliedOption, ImpliedOptionValue)
	;
		[]
	).

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
	io__stderr_stream(StdErr),
	{ library__version(Version) },
 	io__write_strings(StdErr, [
		"Mercury Compiler, version ", Version, "\n",
		"Copyright (C) 1993-1998 The University of Melbourne\n",
		"Usage: mmc [<options>] <arguments>\n",
		"Use `mmc --help' for more information.\n"
	]).

long_usage -->
	{ library__version(Version) },
 	io__write_strings(["Mercury Compiler, version ", Version, "\n"]),
 	io__write_string("Copyright (C) 1993-1998 The University of Melbourne\n"),
	io__write_string("Usage: mmc [<options>] <arguments>\n"),
	io__write_string("Arguments:\n"),
	io__write_string("\t\tArguments ending in `.m' are assumed to be source file names.\n"),
	io__write_string("\t\tArguments that do not end in `.m' are assumed to be module names.\n"),
	io__write_string("Options:\n"),
	options_help.

%-----------------------------------------------------------------------------%

	% IMPORTANT: any changes here may require similar changes to
	%	runtime/mercury_grade.h
	%	scripts/parse_grade_options.sh-subr
	%
	% The grade_component type should have one constructor for each
	% dimension of the grade. It is used when converting the components
	% of the grade string to make sure the grade string doesn't contain
	% more than one value for each dimension (eg *.gc.agc).
	% Adding a value here will require adding clauses to the
	% grade_component_table.
	%
	% A --grade option causes all the grade dependent options to be
	% reset, and only those described by the grade string to be set.
	% The value to which a grade option should be reset should be given
	% in the grade_start_values table below.
	%
	% The ordering of the components here is the same as the order
	% used in scripts/ml.in, and any change here will require a
	% corresponding change there. The only place where the ordering
	% actually matters is for constructing the pathname for the
	% grade of the library, etc for linking (and installation).
:- type grade_component
	--->	gcc_ext		% gcc extensions -- see grade_component_table
	;	gc		% the kind of GC to use
	;	prof		% what profiling options to use
	;	trail		% whether or not to use trailing
	;	args		% argument passing convention
	;	trace		% tracing/debugging options
	;	par		% parallelism / multithreading
	;	pic		% Do we need to reserve a register for
				% PIC (position independent code)?
	.

convert_grade_option(GradeString, Options0, Options) :-
	reset_grade_options(Options0, Options1),
	split_grade_string(GradeString, Components),
	set__init(NoComps),
	list__foldl2(lambda([CompStr::in, Opts0::in, Opts::out,
			CompSet0::in, CompSet::out] is semidet, (
		grade_component_table(CompStr, Comp, CompOpts),
			% Check that the component isn't mentioned
			% more than once.
		\+ set__member(Comp, CompSet0),
		set__insert(CompSet0, Comp, CompSet),
		add_option_list(CompOpts, Opts0, Opts)
	)), Components, Options1, Options, NoComps, _FinalComps).

:- pred add_option_list(list(pair(option, option_data)), option_table,
		option_table).
:- mode add_option_list(in, in, out) is det.

add_option_list(CompOpts, Opts0, Opts) :-
	list__foldl(lambda([Opt::in, Opts1::in, Opts2::out] is det, (
		Opt = Option - Data,
		map__set(Opts1, Option, Data, Opts2)
	)), CompOpts, Opts0, Opts).

compute_grade(Globals, Grade) :-
	globals__get_options(Globals, Options),
	compute_grade_components(Options, Components),
	(
		Components = [],
		Grade = "none"
	;
		Components = [_|_],
		construct_string(Components, Grade)
	).

:- pred construct_string(list(pair(grade_component, string)), string).
:- mode construct_string(in, out) is det.

construct_string([], "").
construct_string([_ - Bit|Bits], Grade) :-
	(
		Bits = [_|_],
		construct_string(Bits, Grade0),
		string__append_list([Bit, ".", Grade0], Grade)
	;
		Bits = [],
		Grade = Bit
	).

:- pred compute_grade_components(option_table,
		list(pair(grade_component, string))).
:- mode compute_grade_components(in, out) is det.

compute_grade_components(Options, GradeComponents) :-
	solutions(lambda([CompData::out] is nondet, (
		grade_component_table(Name, Comp, CompOpts),
			% For possible component of the grade string
			% include it in the actual grade string if all
			% the option setting that it implies are true.
			% ie
			%	all [Opt, Value] (
			%	    member(Opt - Value, CompOpts) => 
			%		map__search(Options, Opt, Value)
			%	)
		\+ (
			list__member(Opt - Value, CompOpts),
			\+ map__search(Options, Opt, Value)
		),
		CompData = Comp - Name
	)), GradeComponents).

:- pred grade_component_table(string, grade_component,
		list(pair(option, option_data))).
:- mode grade_component_table(in, out, out) is semidet.
:- mode grade_component_table(out, out, out) is multi.

	% Args method components
grade_component_table("sa", args, [args - string("simple")]).

	% GCC-hack components
grade_component_table("none", gcc_ext, [asm_labels - bool(no),
	gcc_non_local_gotos - bool(no), gcc_global_registers - bool(no)]).
grade_component_table("reg", gcc_ext, [asm_labels - bool(no),
	gcc_non_local_gotos - bool(no), gcc_global_registers - bool(yes)]).
grade_component_table("jump", gcc_ext, [asm_labels - bool(no),
	gcc_non_local_gotos - bool(yes), gcc_global_registers - bool(no)]).
grade_component_table("asm_jump", gcc_ext, [asm_labels - bool(yes),
	gcc_non_local_gotos - bool(yes), gcc_global_registers - bool(no)]).
grade_component_table("fast", gcc_ext, [asm_labels - bool(no),
	gcc_non_local_gotos - bool(yes), gcc_global_registers - bool(yes)]).
grade_component_table("asm_fast", gcc_ext, [asm_labels - bool(yes),
	gcc_non_local_gotos - bool(yes), gcc_global_registers - bool(yes)]).

	% GC components
grade_component_table("gc", gc, [gc - string("conservative")]).
grade_component_table("agc", gc, [gc - string("accurate")]).

	% Parallelism/MT components.
grade_component_table("par", par, [parallel - bool(yes)]).

	% Pic reg components
grade_component_table("picreg", pic, [pic_reg - bool(yes)]).

	% Profiling components
grade_component_table("prof", prof, [profile_time - bool(yes),
	profile_calls - bool(yes), profile_memory - bool(no)]).
grade_component_table("proftime", prof, [profile_time - bool(yes),
	profile_calls - bool(no), profile_memory - bool(no)]).
grade_component_table("profcalls", prof, [profile_time - bool(no),
	profile_calls - bool(yes), profile_memory - bool(no)]).
grade_component_table("memprof", prof, [profile_time - bool(no),
	profile_calls - bool(no), profile_memory - bool(yes)]).
grade_component_table("profall", prof, [profile_time - bool(yes),
	profile_calls - bool(yes), profile_memory - bool(yes)]).

	% Debugging/Tracing components
grade_component_table("debug", trace,
	[stack_trace - bool(yes), require_tracing - bool(yes)]).
grade_component_table("trace", trace,
	[stack_trace - bool(no), require_tracing - bool(yes)]).
grade_component_table("strace", trace,
	[stack_trace - bool(yes), require_tracing - bool(no)]).

	% Trailing components
grade_component_table("tr", trail, [use_trail - bool(yes)]).

:- pred reset_grade_options(option_table, option_table).
:- mode reset_grade_options(in, out) is det.

reset_grade_options(Options0, Options) :-
	aggregate(grade_start_values, lambda([Pair::in, Opts0::in, Opts::out]
			is det, (
		Pair = Option - Value,
		map__set(Opts0, Option, Value, Opts)
	)), Options0, Options).

:- pred grade_start_values(pair(option, option_data)).
:- mode grade_start_values(out) is multi.

grade_start_values(args - string("compact")).
grade_start_values(asm_labels - bool(no)).
grade_start_values(gcc_non_local_gotos - bool(no)).
grade_start_values(gcc_global_registers - bool(no)).
grade_start_values(gc - string("none")).
grade_start_values(parallel - bool(no)).
grade_start_values(pic_reg - bool(no)).
grade_start_values(profile_time - bool(no)).
grade_start_values(profile_calls - bool(no)).
grade_start_values(profile_memory - bool(no)).
grade_start_values(stack_trace - bool(no)).
grade_start_values(require_tracing - bool(no)).
grade_start_values(use_trail - bool(no)).

:- pred split_grade_string(string, list(string)).
:- mode split_grade_string(in, out) is semidet.

split_grade_string(GradeStr, Components) :-
	string__to_char_list(GradeStr, Chars),
	split_grade_string_2(Chars, Components).

:- pred split_grade_string_2(list(char), list(string)).
:- mode split_grade_string_2(in, out) is semidet.

split_grade_string_2([], []).
split_grade_string_2(Chars, Components) :-
	Chars = [_|_],
	list__takewhile(char_is_not('.'), Chars, ThisChars, RestChars0),
	string__from_char_list(ThisChars, ThisComponent),
	Components = [ThisComponent|RestComponents],
	(
		RestChars0 = [_|RestChars], % discard the `.'
		split_grade_string_2(RestChars, RestComponents)
	;
		RestChars0 = [],
		RestComponents = []
	).

:- pred char_is_not(char, char).
:- mode char_is_not(in, in) is semidet.

char_is_not(A, B) :-
	A \= B.

%-----------------------------------------------------------------------------%

% This predicate converts a symbolic name for a set of verbosity options
% (a "dump alias") into the string consisting of those options' characters.
%
% The meanings of the option characters are documented by doc/user_guide.texi
% and by compiler/hlds_out.m. The latter is more authoritative :-)
%
% You are welcome to add more aliases.

:- pred convert_dump_alias(string, string).
:- mode convert_dump_alias(in, out) is semidet.

convert_dump_alias("ALL", "abcdfgilmnprstuvCIMPTU").
convert_dump_alias("all", "abcdfgilmnprstuvCMPT").
convert_dump_alias("codegen", "dfnprsu").
convert_dump_alias("vanessa", "ltuCIU").
