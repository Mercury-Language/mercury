%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001 The University of Melbourne.
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

:- import_module options, globals, prog_io_util, trace_params, unify_proc.
:- import_module prog_data, foreign.
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
		globals__io_lookup_bool_option(typecheck_only, TypecheckOnly),
		globals__io_lookup_bool_option(errorcheck_only, ErrorcheckOnly),
		globals__io_lookup_bool_option(target_code_only,
			TargetCodeOnly),
		globals__io_get_target(Target),
		{ GenerateIL = (if Target = il then yes else no) },
		{ GenerateJava = (if Target = java then yes else no) },
		globals__io_lookup_bool_option(compile_only, CompileOnly),
		globals__io_lookup_bool_option(aditi_only, AditiOnly),
		{ bool__or_list([GenerateDependencies, MakeInterface,
			MakePrivateInterface, MakeShortInterface,
			MakeOptimizationInt, MakeTransOptInt,
			ConvertToMercury, TypecheckOnly,
			ErrorcheckOnly, TargetCodeOnly,
			GenerateIL, GenerateJava,
			CompileOnly, AditiOnly],
			NotLink) },
		{ bool__not(NotLink, Link) },
		globals__io_lookup_bool_option(smart_recompilation, Smart),
		( { Smart = yes, Link = yes } ->
			% XXX Currently smart recompilation doesn't check
			% that all the files needed to link are present
			% and up-to-date, so disable it.
			disable_smart_recompilation("linking")
		;
			[]	
		)
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
    { map__lookup(OptionTable, target, Target0) },
    (
        { Target0 = string(TargetStr) },
        { convert_target(TargetStr, Target) }
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
                                { map__lookup(OptionTable, suppress_trace,
                                    Suppress) },
                                (
                                    { Suppress = string(SuppressStr) },
                                    { convert_trace_suppress(SuppressStr,
                                        TraceSuppress) }
                                ->
                                    { map__lookup(OptionTable, dump_hlds_alias,
                                        DumpAliasOption) },
                                    (
                                        { DumpAliasOption = string(DumpAlias) },
                                        { DumpAlias = "" }
                                    ->
                                        postprocess_options_2(OptionTable,
                                            Target, GC_Method, TagsMethod,
                                            TermNorm, TraceLevel,
                                            TraceSuppress, Error)
                                    ;
                                        { DumpAliasOption = string(DumpAlias) },
                                        { convert_dump_alias(DumpAlias,
                                            DumpOptions) }
                                    ->
                                        { map__set(OptionTable,
                                            dump_hlds_options,
                                            string(DumpOptions),
                                            NewOptionTable) },
                                        postprocess_options_2(NewOptionTable,
                                            Target, GC_Method, TagsMethod,
                                            TermNorm, TraceLevel,
					    TraceSuppress, Error)
                                    ;
                                        { Error = yes("Invalid argument to option `--hlds-dump-alias'.") }
                                    )
                                ;
                                    { Error = yes("Invalid argument to option `--suppress-trace'.") }
				)
                            ;
                                { Error = yes("Invalid argument to option `--trace'\n\t(must be `minimum', `shallow', `deep', or `default').") }
                            )
                        ;
                            { Error = yes("Invalid argument to option `--termination-norm'\n\t(must be `simple', `total' or  `num-data-elems').") }
                        )
                    ;
                        { Error = yes("Invalid argument to option `--fact-table-hash-percent-full'\n\t(must be an integer between 1 and 100)") }
                    )
            ;
                    { Error = yes("Invalid tags option (must be `none', `low' or `high')") }
            )
        ;
            { Error = yes("Invalid GC option (must be `none', `conservative' or `accurate')") }
	)
    ;
        { Error = yes("Invalid target option (must be `c', `asm', `il', or `java')") }
    ).
    

:- pred postprocess_options_2(option_table::in, compilation_target::in,
    gc_method::in, tags_method::in, termination_norm::in,
    trace_level::in, trace_suppress_items::in, maybe(string)::out,
    io__state::di, io__state::uo) is det.

postprocess_options_2(OptionTable, Target, GC_Method, TagsMethod,
		TermNorm, TraceLevel, TraceSuppress, Error) -->
	{ unsafe_promise_unique(OptionTable, OptionTable1) }, % XXX
	globals__io_init(OptionTable1, Target, GC_Method, TagsMethod,
		TermNorm, TraceLevel, TraceSuppress),

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

	% Generating IL implies high-level code, turning off nested functions,
	% using copy-out for nondet output arguments,
	% using zero tags, boxing enums, disabling no_tag_types and no
	% static ground terms.
	( { Target = il } ->
		globals__io_set_option(highlevel_code, bool(yes)),
		globals__io_set_option(gcc_nested_functions, bool(no)),
		globals__io_set_option(nondet_copy_out, bool(yes)),
		globals__io_set_option(num_tag_bits, int(0)),
		globals__io_set_option(unboxed_enums, bool(no)),
		globals__io_set_option(unboxed_no_tag_types, bool(no)),
		globals__io_set_option(static_ground_terms, bool(no))
	;
		[]
	),

	% Set --put-nondet-env-on-heap if --verifiable-code is specified,
	% unless both --il-funcptr-types and --il-refany-fields
	% are specified.
	globals__io_lookup_bool_option(il_funcptr_types, ILFuncPtrTypes),
	globals__io_lookup_bool_option(il_refany_fields, ILRefAnyFields),
	( { ILFuncPtrTypes = yes, ILRefAnyFields = yes } ->
		[]
	;
		option_implies(verifiable_code, put_nondet_env_on_heap, bool(yes))
	),

	% Generating Java implies high-level code, turning off nested functions,
	% using copy-out for both det and nondet output arguments,
	% using no tags, not optimizing tailcalls and no static ground terms.
	% XXX no static ground terms should be eliminated in a later
	%     version.
	% XXX The Java backend should eventually support optimizing tailcalls.
	( { Target = java } ->
		globals__io_set_option(highlevel_code, bool(yes)),
		globals__io_set_option(gcc_nested_functions, bool(no)),
		globals__io_set_option(nondet_copy_out, bool(yes)),
		globals__io_set_option(det_copy_out, bool(yes)),
		globals__io_set_option(num_tag_bits, int(0)),
		globals__io_set_option(optimize_tailcalls, bool(no)),
		globals__io_set_option(static_ground_terms, bool(no))
	;
		[]
	),
	% Generating assembler via the gcc back-end requires
	% using high-level code.
	( { Target = asm } ->
		globals__io_set_option(highlevel_code, bool(yes))
	;
		[]
	),

	% Generating high-level C or asm code requires putting each commit
	% in its own function, to avoid problems with setjmp() and
	% non-volatile local variables.
	( { Target = c ; Target = asm } ->
		option_implies(highlevel_code, put_commit_in_own_func,
			bool(yes))
	;
		[]
	),
	
	% --high-level-code disables the use of low-level gcc extensions
	option_implies(highlevel_code, gcc_non_local_gotos, bool(no)),
	option_implies(highlevel_code, gcc_global_registers, bool(no)),
	option_implies(highlevel_code, asm_labels, bool(no)),

	% --no-gcc-nested-functions implies --no-gcc-local-labels
	option_neg_implies(gcc_nested_functions, gcc_local_labels, bool(no)),

	% --no-mlds-optimize implies --no-optimize-tailcalls
	option_neg_implies(optimize, optimize_tailcalls, bool(no)),

	option_implies(verbose_check_termination, check_termination,bool(yes)),
	option_implies(check_termination, termination, bool(yes)),
	option_implies(check_termination, warn_missing_trans_opt_files,
		bool(yes)),
	option_implies(make_transitive_opt_interface, transitive_optimization,
		bool(yes)),
	option_implies(transitive_optimization, intermodule_optimization,
		bool(yes)),
	option_implies(use_trans_opt_files, use_opt_files, bool(yes)),

	% If we are doing full inter-module or transitive optimization,
	% we need to build all `.opt' or `.trans_opt' files.
	option_implies(intermodule_optimization, use_opt_files, bool(no)),
	option_implies(transitive_optimization, use_trans_opt_files, bool(no)),

	option_implies(smart_recompilation, generate_item_version_numbers,
			bool(yes)),
	
	%
	% Disable `--smart-recompilation' for compilation options
	% which either do not produce a compiled output file or
	% for which smart recompilation will not work.
	%
	option_implies(generate_dependencies, smart_recompilation, bool(no)),
	option_implies(convert_to_mercury, smart_recompilation, bool(no)),
	option_implies(make_private_interface, smart_recompilation, bool(no)),
	option_implies(make_interface, smart_recompilation, bool(no)),
	option_implies(make_short_interface, smart_recompilation, bool(no)),
	option_implies(output_grade_string, smart_recompilation, bool(no)),
	option_implies(make_optimization_interface,
		smart_recompilation, bool(no)),
	option_implies(make_transitive_opt_interface,
		smart_recompilation, bool(no)),
	option_implies(errorcheck_only, smart_recompilation, bool(no)),
	option_implies(typecheck_only, smart_recompilation, bool(no)),

	% disable --line-numbers when building the `.int', `.opt', etc. files,
	% since including line numbers in those would cause unnecessary
	% recompilation
	option_implies(make_private_interface,		line_numbers, bool(no)),
	option_implies(make_interface,			line_numbers, bool(no)),
	option_implies(make_short_interface,		line_numbers, bool(no)),
	option_implies(make_optimization_interface,	line_numbers, bool(no)),
	option_implies(make_transitive_opt_interface,	line_numbers, bool(no)),

	% `--aditi-only' is only used by the Aditi query shell,
	% for queries which should only be compiled once.
	% recompilation_check.m currently doesn't check whether
	% the `.rlo' file is up to date (with `--no-aditi-only' the
	% Aditi-RL bytecode is embedded in the `.c' file.
	option_implies(aditi_only, smart_recompilation, bool(no)),

	% We never use version number information in `.int3',
	% `.opt' or `.trans_opt'  files.
	option_implies(make_short_interface, generate_item_version_numbers,
		bool(no)),

	% XXX Smart recompilation does not yet work with inter-module
	% optimization, but we still want to generate version numbers
	% in interface files for users of a library compiled with
	% inter-module optimization but not using inter-module
	% optimization themselves.
	globals__io_lookup_bool_option(smart_recompilation, Smart),
	maybe_disable_smart_recompilation(Smart, intermodule_optimization, yes,
		"`--intermodule-optimization'"),
	maybe_disable_smart_recompilation(Smart, use_opt_files, yes,
		"`--use-opt-files'"),

	% XXX Smart recompilation does not yet work with
	% `--no-target-code-only'. With `--no-target-code-only'
	% it becomes difficult to work out what all the target
	% files are and check whether they are up-to-date.
	% By default, mmake always enables `--target-code-only' and
	% processes the target code file itself, so this isn't a problem.
	maybe_disable_smart_recompilation(Smart, target_code_only, no,
		"`--no-target-code-only'"),

	option_implies(very_verbose, verbose, bool(yes)),

	globals__io_lookup_int_option(debug_liveness, DebugLiveness),
	(
		{ DebugLiveness >= 0 },
		{ convert_dump_alias("all", AllDumpOptions) }
	->
			% Programmers only enable --debug-liveness if they are
			% interested in the goal annotations put on goals by
			% the various phases of the liveness pass. The default
			% dump options do not print these annotations.
		globals__io_lookup_string_option(dump_hlds_options,
			DumpOptions0),
		{ string__append(DumpOptions0, AllDumpOptions, DumpOptions) },
		globals__io_set_option(dump_hlds_options, string(DumpOptions))
	;
		[]
	),

	% --split-c-files implies --procs-per-c-function 1
	option_implies(split_c_files, procs_per_c_function, int(1)),

	% make_hlds contains an optimization which requires the value of the
	% compare_specialization option to accurately specify the max number
	% of constructors in a type whose comparison procedure is specialized
	% and which therefore don't need index functions.
	globals__io_lookup_int_option(compare_specialization, CompareSpec0),
	{ int__min(unify_proc__max_exploited_compare_spec_value,
		CompareSpec0, CompareSpec) },
	globals__io_set_option(compare_specialization, int(CompareSpec)),

	% Minimal model tabling is not compatible with trailing;
	% see the comment in runtime/mercury_tabling.c.

	globals__io_lookup_bool_option(use_trail, UseTrail),
	globals__io_lookup_bool_option(use_minimal_model, UseMinimalModel),
	{ UseTrail = yes, UseMinimalModel = yes ->
		Error = yes("trailing and minimal model tabling are not compatible")
	;
		Error = no
	},

	% The `.debug' grade (i.e. --stack-trace plus --require-tracing)
	% implies --use-trail, except with --use-minimal-model, which is
	% not compatible with --use-trail.
	%
	% The reason for this is to avoid unnecessary proliferation in
	% the number of different grades.  If you're using --debug,
	% you've already taken a major performance hit, so you should
	% be able to afford the minor performance hit caused by
	% --use-trail.

	globals__io_lookup_bool_option(stack_trace, StackTrace),
	globals__io_lookup_bool_option(require_tracing, RequireTracing),
	( { StackTrace = yes, RequireTracing = yes, UseMinimalModel = no } ->
		globals__io_set_option(use_trail, bool(yes))
	;
		[]
	),

	% Execution tracing requires
	% 	- disabling optimizations that would change
	% 	  the trace being generated (except with --trace-optimized)
	%	- enabling some low level optimizations to ensure consistent
	%	  paths across optimization levels
	% 	- enabling stack layouts
	% 	- enabling typeinfo liveness
	globals__io_lookup_bool_option(trace_optimized, TraceOptimized),
	( { trace_level_is_none(TraceLevel) = no } ->
		( { TraceOptimized = no } ->
			% The following options modify the structure
			% of the program, which makes it difficult to
			% relate the trace to the source code (although
			% it can be easily related to the transformed HLDS).
			globals__io_set_option(inline_simple, bool(no)),
			globals__io_set_option(inline_single_use, bool(no)),
			globals__io_set_option(inline_compound_threshold,
				int(0)),
			globals__io_set_option(optimize_unused_args, bool(no)),
			globals__io_set_option(optimize_higher_order, bool(no)),
			globals__io_set_option(type_specialization, bool(no)),
			globals__io_set_option(user_guided_type_specialization,
				bool(no)),
			globals__io_set_option(deforestation, bool(no)),
			globals__io_set_option(constraint_propagation,
				bool(no)),
			globals__io_set_option(local_constraint_propagation,
				bool(no)),
			globals__io_set_option(optimize_duplicate_calls,
				bool(no)),
			globals__io_set_option(optimize_constructor_last_call,
				bool(no))
		;
			[]
		),

			% Disable hijacks if debugging is enabled. The
			% code we now use to restore the stacks for
			% direct retries works only if the retry does not
			% "backtrack" over a hijacked nondet stack frame
			% whose hijack has not been undone. Note that
			% code compiled without debugging may still hijack
			% nondet stack frames. Execution may reemerge from
			% the nondebugged region in one of two ways. If
			% the nondebugged code returns, then it will have
			% undone hijack, and the retry code will work. If
			% the nondebugged code calls debugged code, there
			% will be a region on the stacks containing no
			% debugging information, and the retry command will
			% refuse to perform retries that go into or beyond
			% this region. Both cases preserve correctness.
			%
			% An alternative solution would be to store everything
			% on the nondet stack that may be hijacked in ordinary
			% stack slots on entry to every procedure, but that
			% would be not only more complex than simply disabling
			% hijacks, it would be slower as well, except in
			% procedures that would have many nested hijacks,
			% and such code is extremely rare.
		globals__io_set_option(allow_hijacks, bool(no)),
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
			% The following options cause the info required
			% by tracing to be generated.
		globals__io_set_option(trace_stack_layout, bool(yes)),
		globals__io_set_option(body_typeinfo_liveness, bool(yes))
	;
		[]
	),

	% Deep profiling will eventually use `procid' stack layouts,
	% but for now, we use a separate copy of each MR_Proc_Id structure.
	% option_implies(profile_deep, procid_stack_layout, bool(yes)),
	globals__io_lookup_bool_option(profile_deep, ProfileDeep),
	globals__io_lookup_bool_option(highlevel_code, HighLevel),
	( { ProfileDeep = yes } ->
		(
			{ HighLevel = no },
			{ Target = c }
		->
			[]
		;
			usage_error("deep profiling is incompatible with high level code")
		),
		globals__io_lookup_bool_option(
			use_lots_of_ho_specialization, LotsOfHOSpec),
		( { LotsOfHOSpec = yes } ->
			{ True = bool(yes) },
			globals__io_set_option(optimize_higher_order, True),
			globals__io_set_option(higher_order_size_limit,
				int(999999))
		;
			[]
		)
	;
		[]
	),

	% --no-reorder-conj implies --no-deforestation,
	% --no-constraint-propagation and --no-local-constraint-propagation.
	option_neg_implies(reorder_conj, deforestation, bool(no)),
	option_neg_implies(reorder_conj, constraint_propagation, bool(no)),
	option_neg_implies(reorder_conj, local_constraint_propagation,
		bool(no)),

	% --stack-trace requires `procid' stack layouts
	option_implies(stack_trace, procid_stack_layout, bool(yes)),

	% `trace' stack layouts need `procid' stack layouts
	option_implies(trace_stack_layout, procid_stack_layout, bool(yes)),

	% --gc accurate requires `agc' stack layouts, typeinfo liveness,
	% and needs hijacks and frameopt to be switched off.
	( { GC_Method = accurate } ->
		globals__io_set_option(agc_stack_layout, bool(yes)),
		globals__io_set_option(body_typeinfo_liveness, bool(yes)),
		globals__io_set_option(allow_hijacks, bool(no)),
		globals__io_set_option(optimize_frames, bool(no))
	;
		[]
	),

	% `procid' and `agc' stack layouts need `basic' stack layouts
	option_implies(procid_stack_layout, basic_stack_layout, bool(yes)),
	option_implies(agc_stack_layout, basic_stack_layout, bool(yes)),

	% dupelim.m doesn't preserve label layout structures
	% (e.g. it can change the return address in a call
	% to a different label whose code is the same but
	% which has a different label layout structure),
	% so we need to disable it when tracing.
	option_implies(procid_stack_layout, optimize_dups, bool(no)),

	% XXX deforestation does not perform folding on polymorphic
	% predicates correctly with --body-typeinfo-liveness.
	option_implies(body_typeinfo_liveness, deforestation, bool(no)),

	% XXX if trailing is enabled, middle recursion optimization
	% can generate code which does not allocate a stack frame 
	% even though stack slots are used to save and restore the
	% trail, if the code being optimized contains a construct which
	% might save/restore the trail state, i.e. an if-then-else,
	% negation, disjunction, or commit.
	option_implies(use_trail, middle_rec, bool(no)),

	% Minimal model tabling needs to be able to rewrite all the redoips
	% in a given nondet stack segments. If we allow hijacks, some of these
	% redoips may have been saved in ordinary framevars, which means that
	% tabling can't find them without label layout info. Since we want
	% to allow tabling in grades that do not have label layout info,
	% we disable hijacks instead.
	option_implies(use_minimal_model, allow_hijacks, bool(no)),

	% --dump-hlds and --statistics require compilation by phases
	globals__io_lookup_accumulating_option(dump_hlds, DumpStages),
	globals__io_lookup_bool_option(statistics, Statistics),
	( { DumpStages \= [] ; Statistics = yes } ->
		globals__io_set_option(trad_passes, bool(no))
	;
		[]
	),

	% If we are doing type-specialization, we may as well take
	% advantage of the declarations supplied by the programmer.
	option_implies(type_specialization, user_guided_type_specialization,
		bool(yes)),

	% The local constraint propagation transformation (constraint.m)
	% is a required part of the constraint propagation transformation
	% performed by deforest.m.
	option_implies(constraint_propagation, local_constraint_propagation,
		bool(yes)),

	% --intermod-unused-args implies --intermodule-optimization and
	% --optimize-unused-args.
	option_implies(intermod_unused_args, intermodule_optimization,
		bool(yes)),
	option_implies(intermod_unused_args, optimize_unused_args, bool(yes)),

	% --introduce-accumulators implies --excess-assign and
	% --common-struct.
	option_implies(introduce_accumulators, excess_assign, bool(yes)),
	option_implies(introduce_accumulators, common_struct, bool(yes)),

	% Don't do the unused_args optimization when making the
	% optimization interface.
	option_implies(make_optimization_interface, optimize_unused_args,
		bool(no)),

	% The information needed for generating the module ordering
	% is only available while generating the dependencies.
	option_implies(generate_module_order, generate_dependencies,
		bool(yes)),

	% --aditi-only implies --aditi.
	option_implies(aditi_only, aditi, bool(yes)),

	% Set --aditi-user to the value of $USER if it is not set already.
	% If $USER is not set, use the string "guest".
	globals__io_lookup_string_option(aditi_user, User0),
	( { User0 = "" } ->
		io__get_environment_var("USER", MaybeUser),
		( { MaybeUser = yes(User1) } ->
			{ User = User1 }
		;
			{ User = "guest" }
		),
		globals__io_set_option(aditi_user, string(User))
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
	),

	% --use-opt-files implies --no-warn-missing-opt-files since
	% we are expecting some to be missing.
	option_implies(use_opt_files, warn_missing_opt_files, bool(no)),


	% The preferred backend foreign language depends on the target.
	( 	
		{ Target = c },
		{ BackendForeignLanguages = ["c"] }
	;
		{ Target = il },
		{ BackendForeignLanguages = ["csharp", "mc++"] }
	;
		{ Target = asm },
		% XXX This is wrong!  It should be asm.
		{ BackendForeignLanguages = ["c"] }
	;
		% XXX We don't generate java or handle it as a foreign
		% language just yet, but if we did, we should fix this
		{ Target = java },
		{ BackendForeignLanguages = [] }
	),

		% only set the backend foreign languages if they are unset
	globals__io_lookup_accumulating_option(backend_foreign_languages,
		CurrentBackendForeignLanguage),
	( 
		{ CurrentBackendForeignLanguage = [] }
	->
		globals__io_set_option(backend_foreign_languages,
			accumulating(BackendForeignLanguages))
	;
		[]
	),

	( { HighLevel = no } ->
		postprocess_options_lowlevel
	;
		[]
	).

	% These option implications only affect the low-level (LLDS) code
	% generator.  They may in fact be harmful if set for the high-level
	% code generator, because sometimes the same option has different
	% meanings and implications in the two backends.
	%
:- pred postprocess_options_lowlevel(io__state::di, io__state::uo) is det.

postprocess_options_lowlevel -->
		% --no-lazy-code assumes that const(_) rvals are really
		% constant, and that create(_) rvals with constant arguments
		% can be materialized in an assignable rval without further
		% code. For float_consts, the former is true only if either
		% static_ground_terms or unboxed_floats is true, and the latter
		% cannot be true without static_ground_terms.
	option_neg_implies(lazy_code, static_ground_terms, bool(yes)),

		% --no-lazy-code requires --follow-vars for acceptable
		% performance.
	option_neg_implies(lazy_code, follow_vars, bool(yes)),

		% --optimize-frames requires --optimize-labels and
		% --optimize-jumps
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

	% Smart recompilation does not yet work with all
	% options (in particular `--intermodule-optimization'
	% and `--no-target-code-only'). Disable smart recompilation
	% if such an option is set, maybe issuing a warning.
:- pred maybe_disable_smart_recompilation(bool::in, option::in, bool::in,
		string::in, io__state::di, io__state::uo) is det.

maybe_disable_smart_recompilation(Smart, ConflictingOption,
		ValueToDisableSmart, OptionDescr) -->
	globals__io_lookup_bool_option(ConflictingOption, Value),
	(
		{ Smart = yes },
		{ Value = ValueToDisableSmart }
	->
		disable_smart_recompilation(OptionDescr)
	;
		[]
	).

:- pred disable_smart_recompilation(string::in,
		io__state::di, io__state::uo) is det.

disable_smart_recompilation(OptionDescr) -->
	globals__io_set_option(smart_recompilation, bool(no)),
	globals__io_lookup_bool_option(warn_smart_recompilation,
		WarnSmart),
	( { WarnSmart = yes } ->
		io__write_string(
	"Warning: smart recompilation does not yet work with "),
		io__write_string(OptionDescr),
		io__write_string(".\n"),
		globals__io_lookup_bool_option(halt_at_warn, Halt),
		( { Halt = yes } ->
			io__set_exit_status(1)
		;
			[]
		)
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
		"Copyright (C) 1993-2000 The University of Melbourne\n",
		"Usage: mmc [<options>] <arguments>\n",
		"Use `mmc --help' for more information.\n"
	]).

long_usage -->
	{ library__version(Version) },
 	io__write_strings(["Mercury Compiler, version ", Version, "\n"]),
 	io__write_string("Copyright (C) 1993-2000 The University of Melbourne\n"),
	io__write_string("Usage: mmc [<options>] <arguments>\n"),
	io__write_string("Arguments:\n"),
	io__write_string("\tArguments ending in `.m' are assumed to be source file names.\n"),
	io__write_string("\tArguments that do not end in `.m' are assumed to be module names.\n"),
	io__write_string("Options:\n"),
	options_help.

%-----------------------------------------------------------------------------%

	% IMPORTANT: any changes here may require similar changes to
	%	runtime/mercury_grade.h
	%	scripts/parse_grade_options.sh-subr
	%	scripts/canonical_grade.sh-subr
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
	--->	gcc_ext		% gcc extensions etc. -- see
				% grade_component_table
	;	par		% parallelism / multithreading
	;	gc		% the kind of GC to use
	;	prof		% what profiling options to use
	;	trail		% whether or not to use trailing
	;       tag             % whether or not to reserve a tag
	;	minimal_model	% whether we set up for minimal model tabling
	;	pic		% Do we need to reserve a register for
				% PIC (position independent code)?
	;	trace           % tracing/debugging options
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
:- mode grade_component_table(out, in, out) is multi.
:- mode grade_component_table(out, out, out) is multi.

	% Base components
	% These specify the basic compilation model we use,
	% including the choice of back-end and the use of gcc extensions.
grade_component_table("none", gcc_ext, [
		asm_labels		- bool(no),
		gcc_non_local_gotos	- bool(no),
		gcc_global_registers	- bool(no),
		highlevel_code		- bool(no),
		gcc_nested_functions	- bool(no),
		highlevel_data		- bool(no),
		target			- string("c")]).
grade_component_table("reg", gcc_ext, [
		asm_labels		- bool(no),
		gcc_non_local_gotos	- bool(no),
		gcc_global_registers	- bool(yes),
		highlevel_code		- bool(no),
		gcc_nested_functions	- bool(no),
		highlevel_data		- bool(no),
		target			- string("c")]).
grade_component_table("jump", gcc_ext, [
		asm_labels		- bool(no),
		gcc_non_local_gotos	- bool(yes),
		gcc_global_registers	- bool(no),
		highlevel_code		- bool(no),
		gcc_nested_functions	- bool(no),
		highlevel_data		- bool(no),
		target			- string("c")]).
grade_component_table("asm_jump", gcc_ext, [
		asm_labels		- bool(yes),
		gcc_non_local_gotos	- bool(yes),
		gcc_global_registers	- bool(no),
		highlevel_code		- bool(no),
		gcc_nested_functions	- bool(no),
		highlevel_data		- bool(no),
		target			- string("c")]).
grade_component_table("fast", gcc_ext, [
		asm_labels		- bool(no),
		gcc_non_local_gotos	- bool(yes),
		gcc_global_registers	- bool(yes),
		highlevel_code		- bool(no),
		gcc_nested_functions	- bool(no),
		highlevel_data		- bool(no),
		target			- string("c")]).
grade_component_table("asm_fast", gcc_ext, [
		asm_labels		- bool(yes),
		gcc_non_local_gotos	- bool(yes),
		gcc_global_registers	- bool(yes),
		highlevel_code		- bool(no),
		gcc_nested_functions	- bool(no),
		highlevel_data		- bool(no),
		target			- string("c")]).
grade_component_table("hl", gcc_ext, [
		asm_labels		- bool(no),
		gcc_non_local_gotos	- bool(no),
		gcc_global_registers	- bool(no),
		highlevel_code		- bool(yes),
		gcc_nested_functions	- bool(no),
		highlevel_data		- bool(yes)
		% target can be either c or asm
		]).
grade_component_table("hlc", gcc_ext, [
		asm_labels		- bool(no),
		gcc_non_local_gotos	- bool(no),
		gcc_global_registers	- bool(no),
		highlevel_code		- bool(yes),
		gcc_nested_functions	- bool(no),
		highlevel_data		- bool(no)
		% target can be either c or asm
		]).
grade_component_table("hl_nest", gcc_ext, [
		asm_labels		- bool(no),
		gcc_non_local_gotos	- bool(no),
		gcc_global_registers	- bool(no),
		highlevel_code		- bool(yes),
		gcc_nested_functions	- bool(yes),
		highlevel_data		- bool(yes)
		% target can be either c or asm
		]).
grade_component_table("hlc_nest", gcc_ext, [
		asm_labels		- bool(no),
		gcc_non_local_gotos	- bool(no),
		gcc_global_registers	- bool(no),
		highlevel_code		- bool(yes),
		gcc_nested_functions	- bool(yes),
		highlevel_data		- bool(no)
		% target can be either c or asm
		]).
grade_component_table("il", gcc_ext, [
		asm_labels		- bool(no),
		gcc_non_local_gotos	- bool(no),
		gcc_global_registers	- bool(no),
		highlevel_code		- bool(yes),
		gcc_nested_functions	- bool(no),
		highlevel_data		- bool(yes),
		target			- string("il")]).
grade_component_table("ilc", gcc_ext, [
		asm_labels		- bool(no),
		gcc_non_local_gotos	- bool(no),
		gcc_global_registers	- bool(no),
		highlevel_code		- bool(yes),
		gcc_nested_functions	- bool(no),
		highlevel_data		- bool(no),
		target			- string("il")]).
grade_component_table("java", gcc_ext, [
		asm_labels		- bool(no),
		gcc_non_local_gotos	- bool(no),
		gcc_global_registers	- bool(no),
		gcc_nested_functions	- bool(no),
		highlevel_code		- bool(yes),
		highlevel_data		- bool(yes),
		target			- string("java")]).

	% Parallelism/multithreading components.
grade_component_table("par", par, [parallel - bool(yes)]).

	% GC components
grade_component_table("gc", gc, [gc - string("conservative")]).
grade_component_table("agc", gc, [gc - string("accurate")]).

	% Profiling components
grade_component_table("prof", prof,
	[profile_time - bool(yes), profile_calls - bool(yes),
	profile_memory - bool(no), profile_deep - bool(no)]).
grade_component_table("proftime", prof,
	[profile_time - bool(yes), profile_calls - bool(no),
	profile_memory - bool(no), profile_deep - bool(no)]).
grade_component_table("profcalls", prof,
	[profile_time - bool(no), profile_calls - bool(yes),
	profile_memory - bool(no), profile_deep - bool(no)]).
grade_component_table("memprof", prof,
	[profile_time - bool(no), profile_calls - bool(yes),
	profile_memory - bool(yes), profile_deep - bool(no)]).
grade_component_table("profall", prof,
	[profile_time - bool(yes), profile_calls - bool(yes),
	profile_memory - bool(yes), profile_deep - bool(no)]).
grade_component_table("profdeep", prof,
	[profile_time - bool(no), profile_calls - bool(no),
	profile_memory - bool(no), profile_deep - bool(yes)]).

	% Trailing components
grade_component_table("tr", trail, [use_trail - bool(yes)]).

	% Tag reservation components
grade_component_table("rt", tag, [reserve_tag - bool(yes)]).

	% Mimimal model tabling components
grade_component_table("mm", minimal_model, [use_minimal_model - bool(yes)]).

	% Pic reg components
grade_component_table("picreg", pic, [pic_reg - bool(yes)]).

	% Debugging/Tracing components
grade_component_table("debug", trace,
	[stack_trace - bool(yes), require_tracing - bool(yes)]).
grade_component_table("trace", trace,
	[stack_trace - bool(no), require_tracing - bool(yes)]).
grade_component_table("strce", trace,
	[stack_trace - bool(yes), require_tracing - bool(no)]).

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

grade_start_values(asm_labels - bool(no)).
grade_start_values(gcc_non_local_gotos - bool(no)).
grade_start_values(gcc_global_registers - bool(no)).
grade_start_values(highlevel_code - bool(no)).
grade_start_values(highlevel_data - bool(no)).
grade_start_values(gcc_nested_functions - bool(no)).
grade_start_values(parallel - bool(no)).
grade_start_values(gc - string("none")).
grade_start_values(profile_deep - bool(no)).
grade_start_values(profile_time - bool(no)).
grade_start_values(profile_calls - bool(no)).
grade_start_values(profile_memory - bool(no)).
grade_start_values(use_trail - bool(no)).
grade_start_values(reserve_tag - bool(no)).
grade_start_values(use_minimal_model - bool(no)).
grade_start_values(pic_reg - bool(no)).
grade_start_values(stack_trace - bool(no)).
grade_start_values(require_tracing - bool(no)).

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
convert_dump_alias("paths", "cP").
convert_dump_alias("petdr", "din").
