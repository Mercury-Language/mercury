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
:- import_module int, string, map, getopt, library.

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
                                postprocess_options_2(OptionTable,
                                    GC_Method, TagsMethod, ArgsMethod,
                                    PrologDialect, TermNorm, TraceLevel),
                                { Error = no }
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
	option_implies(very_verbose, verbose, bool(yes)),

	% --split-c-files implies --procs-per-c-function 1
	option_implies(split_c_files, procs_per_c_function, int(1)),

	% -D all is really -D "abcdefghijklmnopqrstuvwxyz"
	globals__io_lookup_string_option(verbose_dump_hlds, VerboseDump),
	( { VerboseDump = "all" } ->
		globals__io_set_option(verbose_dump_hlds,
			string("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
	;	
		[]
	),

	% Tracing requires 
	% 	- disabling optimizations that would change 
	% 	  the trace being generated
	%	- enabling some low level optimizations to ensure consistent
	%	  paths across optimization levels
	% 	- enabling stack layouts
	% 	- enabling typeinfo liveness
	( { TraceLevel = interface ; TraceLevel = full } ->
			% The following options modify the structure
			% of the program, which makes it difficult to
			% relate the trace to the source code (although
			% it can be easily related to the transformed HLDS).
		globals__io_set_option(inline_simple, bool(no)),
		globals__io_set_option(inline_single_use, bool(no)),
		globals__io_set_option(inline_compound_threshold, int(0)),
		globals__io_set_option(optimize_unused_args, bool(no)),
		globals__io_set_option(optimize_higher_order, bool(no)),
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

	% --gc accurate requires `agc' stack layouts and typeinfo liveness.
	( { GC_Method = accurate } ->
		globals__io_set_option(agc_stack_layout, bool(yes)),
		globals__io_set_option(typeinfo_liveness, bool(yes)) 
	;
		[]
	),

	% `procid' and `agc' stack layouts need `basic' stack layouts
	option_implies(procid_stack_layout, basic_stack_layout, bool(yes)),
	option_implies(agc_stack_layout, basic_stack_layout, bool(yes)),

	% XXX higher_order.m does not update the typeinfo_varmap
	% for specialised versions.
	% This causes the compiler to abort in unused_args.m when compiling
	% tests/valid/agc_ho_pred.m with `-O3 --intermodule-optimization'.
	option_implies(typeinfo_liveness, optimize_higher_order, bool(no)),

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

	% IMPORTANT: any changes here may require similar changes to
	%	runtime/mercury_grade.h
	%	scripts/ml.in

compute_grade(Globals, Grade) :-
	globals__lookup_bool_option(Globals, asm_labels, AsmLabels),
	globals__lookup_bool_option(Globals, gcc_non_local_gotos,
						NonLocalGotos),
	globals__lookup_bool_option(Globals, gcc_global_registers, GlobalRegs),
	globals__get_gc_method(Globals, GC_Method),
	globals__lookup_bool_option(Globals, profile_time, ProfileTime),
	globals__lookup_bool_option(Globals, profile_calls, ProfileCalls),
	globals__lookup_bool_option(Globals, profile_memory, ProfileMemory),
	globals__lookup_bool_option(Globals, use_trail, UseTrail),
/*
% These vary from machine to machine, and (for backwards compatibility,
% if nothing else) we want examples such as "GRADE = asm_fast.gc.prof"
% to continue to work, so we can't include these in the grade.
	globals__get_tags_method(Globals, TagsMethod),
	globals__lookup_int_option(Globals, tag_bits, TagBits),
	globals__lookup_bool_option(Globals, unboxed_float, UnboxedFloat),
*/
	globals__get_args_method(Globals, ArgsMethod),
	globals__lookup_bool_option(Globals, stack_trace, StackTrace),
	globals__lookup_bool_option(Globals, require_tracing, RequireTracing),
/*
	globals__lookup_bool_option(Globals, pic_reg, PIC_Reg),
*/

	( AsmLabels = yes ->
		Part1 = "asm_"
	;
		Part1 = ""
	),
	( NonLocalGotos = yes ->
		( GlobalRegs = yes ->
			Part2 = "fast"
		;
			Part2 = "jump"
		)
	;
		( GlobalRegs = yes ->
			Part2 = "reg"
		;
			Part2 = "none"
		)
	),
	( GC_Method = conservative, Part3 = ".gc"
	; GC_Method = accurate, Part3 = ".agc"
	; GC_Method = none, Part3 = ""
	),
	( ProfileTime = yes ->
		( ProfileCalls = yes ->
			( ProfileMemory = yes ->
				Part4 = ".profall"
			; 
				Part4 = ".prof"
			)
		;
			( ProfileMemory = yes ->
				Part4 = ".profmemtime" /* not allowed */
					/* `ml' will catch the error */
			; 
				Part4 = ".proftime" /* currently useless */
			)
		)
	;
		( ProfileCalls = yes ->
			( ProfileMemory = yes ->
				Part4 = ".memprof"
			; 
				Part4 = ".profcalls"
			)
		; 
			( ProfileMemory = yes ->
				Part4 = ".profmem" /* not allowed */
					/* `ml' will catch the error */
			; 
				Part4 = ""
			)
		)
	),
	( UseTrail = yes ->
		Part5 = ".tr"
	;
		Part5 = ""
	),

/*
% These vary from machine to machine, and (for backwards compatibility,
% if nothing else) we want examples such as "GRADE = asm_fast.gc.prof"
% to continue to work, so we can't include these in the grade.
	( HighTags = yes ->
		string__format(".hightags%d", [i(TagBits)], Part6)
	;
		string__format(".tags%d", [i(TagBits)], Part6)
	;
		
	),
	( UnboxedFloat = yes ->
		Part7 = ".ubf"
	;
		Part7 = ""
	),
*/
	Part6 = "",
	Part7 = "",

	( ArgsMethod = compact, Part8 = ""
	; ArgsMethod = simple, Part8 = ".sa"
	),

	( StackTrace = yes ->
		( RequireTracing = yes ->
			Part9 = ".debug"
		;
			Part9 = ".strce"
		)
	;
		( RequireTracing = yes ->
			Part9 = ".trace"
		;
			Part9 = ""
		)
	),

/*******
	% This can't be part of the grade, due to the way
	% we handle things on Linux.  See README.Linux.
	( PIC_Reg = yes ->
		Part10 = ".picreg"
	;
		Part10 = ""
	),
*******/
	Part10 = "",

	string__append_list( [Part1, Part2, Part3, Part4, Part5,
				Part6, Part7, Part8, Part9, Part10], Grade).

	% IMPORTANT: any changes here may require similar changes to
	%	runtime/mercury_grade.h
	%	scripts/parse_grade_options.sh-subr

convert_grade_option(Grade0) -->
	% part10
	( { string__remove_suffix(Grade0, ".picreg", Grade1) } ->
		{ Grade2 = Grade1 },
		set_bool_opt(pic_reg, yes)
	;
		{ Grade2 = Grade0 },
		set_bool_opt(pic_reg, no)
	),
	% part9
	( { string__remove_suffix(Grade2, ".debug", Grade3) } ->
		{ Grade4 = Grade3 },
		set_bool_opt(stack_trace, yes),
		set_bool_opt(require_tracing, yes)
	; { string__remove_suffix(Grade2, ".trace", Grade3) } ->
		{ Grade4 = Grade3 },
		set_bool_opt(stack_trace, no),
		set_bool_opt(require_tracing, yes)
	; { string__remove_suffix(Grade2, ".strce", Grade3) } ->
		{ Grade4 = Grade3 },
		set_bool_opt(stack_trace, yes),
		set_bool_opt(require_tracing, no)
	;
		{ Grade4 = Grade2 },
		set_bool_opt(stack_trace, no),
		set_bool_opt(require_tracing, no)
	),
	% part8
	( { string__remove_suffix(Grade4, ".sa", Grade5) } ->
		{ Grade6 = Grade5 },
		set_string_opt(args, "simple")
	;
		{ Grade6 = Grade4 },
		set_string_opt(args, "compact")
	),
	% part6 & 7
	{ Grade10 = Grade6 },
	% part5
	( { string__remove_suffix(Grade10, ".tr", Grade11) } ->
		{ Grade12 = Grade11 },
		set_bool_opt(use_trail, yes)
	;
		{ Grade12 = Grade10 },
		set_bool_opt(use_trail, no)
	),
	% part 4
	( { string__remove_suffix(Grade12, ".prof", Grade13) } ->
		{ Grade14 = Grade13 },
		set_bool_opt(profile_time, yes),
		set_bool_opt(profile_calls, yes),
		set_bool_opt(profile_memory, no)
	; { string__remove_suffix(Grade12, ".proftime", Grade13) } ->
		{ Grade14 = Grade13 },
		set_bool_opt(profile_time, yes),
		set_bool_opt(profile_calls, no),
		set_bool_opt(profile_memory, no)
	; { string__remove_suffix(Grade12, ".profcalls", Grade13) } ->
		{ Grade14 = Grade13 },
		set_bool_opt(profile_time, no),
		set_bool_opt(profile_calls, yes),
		set_bool_opt(profile_memory, no)
	; { string__remove_suffix(Grade12, ".profall", Grade13) } ->
		{ Grade14 = Grade13 },
		set_bool_opt(profile_time, yes),
		set_bool_opt(profile_calls, yes),
		set_bool_opt(profile_memory, yes)
	; { string__remove_suffix(Grade12, ".memprof", Grade13) } ->
		{ Grade14 = Grade13 },
		set_bool_opt(profile_time, no),
		set_bool_opt(profile_calls, yes),
		set_bool_opt(profile_memory, yes)
	;
		{ Grade14 = Grade12 },
		set_bool_opt(profile_time, no),
		set_bool_opt(profile_calls, no),
		set_bool_opt(profile_memory, no)
	),
	% part 3
	( { string__remove_suffix(Grade14, ".gc", Grade15) } ->
		{ Grade = Grade15 },
		{ GC = conservative }
	; { string__remove_suffix(Grade14, ".agc", Grade15) } ->
		{ Grade = Grade15 },
		{ GC = accurate }
	;
		{ Grade = Grade14 },
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
	% parts 2 & 1
	convert_grade_option_2(Grade).

:- pred convert_grade_option_2(string::in, option_table::in, option_table::out)
	is semidet.

convert_grade_option_2("asm_fast") -->
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, yes),
	set_bool_opt(gcc_global_registers, yes),
	set_bool_opt(asm_labels, yes).
convert_grade_option_2("fast") -->
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, yes),
	set_bool_opt(gcc_global_registers, yes),
	set_bool_opt(asm_labels, no).
convert_grade_option_2("asm_jump") -->
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, yes),
	set_bool_opt(gcc_global_registers, no),
	set_bool_opt(asm_labels, yes).
convert_grade_option_2("jump") -->
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, yes),
	set_bool_opt(gcc_global_registers, no),
	set_bool_opt(asm_labels, no).
convert_grade_option_2("reg") -->
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, no),
	set_bool_opt(gcc_global_registers, yes),
	set_bool_opt(asm_labels, no).
convert_grade_option_2("none") -->
	set_bool_opt(c_optimize, yes),
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
