%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: options.m.
% Main author: fjh.

% This defines the stuff necessary so that getopt.m
% can parse the command-line options.

% IMPORTANT NOTE: any changes to the options should be
% reflected in both the help message produced below,
% and in the Mercury User's Guide (../doc/user_guide.texi).

%-----------------------------------------------------------------------------%

:- module options.
:- interface.
:- import_module char, io, getopt.

:- pred short_option(char::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_defaults(option::out, option_data::out) is nondet.

:- pred special_handler(option::in, special_data::in, option_table::in,
	maybe_option_table::out) is semidet.
%	special_handler(Option, ValueForThatOption, OptionTableIn,
%			MaybeOptionTableOut).
%	This predicate is invoked whenever getopt finds an option
%	(long or short) designated as special, with special_data holding
%	the argument of the option (if any). The predicate can change the
%	option table in arbitrary ways in the course of handling the option,
%	or it can return an error message.
%	The canonical examples of special options are -O options in compilers,
%	which set many other options at once.
%	The MaybeOptionTableOut may either be ok(OptionTableOut), or it may
%	be error(ErrorString).

:- pred options_help(io__state::di, io__state::uo) is det.

:- type option_table == option_table(option).
:- type maybe_option_table == maybe_option_table(option).

:- type option	
	% Warning options
		--->	inhibit_warnings
		;	halt_at_warn
		;	halt_at_syntax_errors
		;	warn_singleton_vars
		;	warn_overlapping_scopes
		;	warn_det_decls_too_lax
		;	warn_nothing_exported
		;	warn_unused_args
		;	warn_interface_imports
		;	warn_missing_opt_files
		;	warn_missing_trans_opt_deps
		;	warn_non_stratification
		;	warn_simple_code
		;	warn_duplicate_calls
		;	warn_missing_module_name
		;	warn_wrong_module_name
	% Verbosity options
		;	verbose
		;	very_verbose
		;	verbose_errors
		;	statistics
		;	debug_types
		;	debug_modes
		;	debug_inst_keys
		;	debug_det
		;	debug_opt
		;	debug_vn	% vn = value numbering
		;	debug_pd	% pd = partial deduction/deforestation
	% Output options
		;	make_short_interface
		;	make_interface
		;	make_private_interface
		;	make_optimization_interface
		;	make_transitive_opt_interface
		;	generate_dependencies
		;	generate_module_order
		;	convert_to_mercury
		;	convert_to_goedel
		;	typecheck_only
		;	errorcheck_only
		;	compile_to_c
		;	compile_only
	% Auxiliary output options
		;	assume_gmake
		;	trace
		;	generate_bytecode
		;	generate_prolog
		;	prolog_dialect
		;	line_numbers
		;	auto_comments
		;	show_dependency_graph
		;	dump_hlds
		;	verbose_dump_hlds
	% Language semantics options
		;	reorder_conj
		;	reorder_disj
		;	fully_strict
		;	strict_sequential
		;	infer_types
		;	infer_modes
		;	infer_det
		;	infer_all
		;	type_inference_iteration_limit
		;	mode_inference_iteration_limit
	% Compilation Model options
		;	grade
		;	gcc_non_local_gotos
		;	gcc_global_registers
		;	asm_labels
		;	gc
		;	parallel
		;	profiling		% profile_time + profile_calls
		;	time_profiling		% profile_time + profile_calls
		;	memory_profiling	% profime_mem + profile_calls
		;	profile_calls
		;	profile_time
		;	profile_memory
		;	debug
		;	stack_trace
		;	require_tracing
		;	use_trail
		;	pic_reg
		;	tags
		;	num_tag_bits
		;	bits_per_word
		;	bytes_per_word
		;	conf_low_tag_bits
				% The undocumented conf_low_tag_bits option
				% is used by the `mmc' script to pass the
				% default value for num_tag_bits
				% assuming --tags low.
				% The reason that `mmc' doesn't just
				% pass a default value for --num-tag-bits
				% is that we want to be able to give an
				% error message if the user specifies
				% `--tags high' and doesn't specify
				% `--num-tag-bits'.
		;	args
		;	highlevel_c
		;	unboxed_float
		;	sync_term_size % in words
		;	type_layout
	% Options for internal use only
	% (the values of these options are implied by the
	% settings of other options)
				% Stack layout information required to do
				% a stack trace.
		;	basic_stack_layout
				% Stack layout information required to do
				% accurate GC.
		;	agc_stack_layout
				% Stack layout information required to do
				% procedure identification.
		;	procid_stack_layout
				% Stack layout information required to do
				% tracing.
		;	trace_stack_layout
				% Use an alternate calculation of liveness
				% where typeinfos are live for any live data
				% the includes that type variable.
		;	typeinfo_liveness
	% Code generation options
		;	low_level_debug
		;	trad_passes
		;	polymorphism
		;	reclaim_heap_on_failure
		;	reclaim_heap_on_semidet_failure
		;	reclaim_heap_on_nondet_failure
		;	lazy_code
		;	have_delay_slot
		;	num_real_r_regs
		;	num_real_f_regs
		;	num_real_r_temps
		;	num_real_f_temps
		;	cc
		;	cflags
		;	cflags_for_regs
		;	cflags_for_gotos
		;	c_debug
		;	c_include_directory
		;	aditi
		;	fact_table_max_array_size
				% maximum number of elements in a single 
				% fact table data array

		;	fact_table_hash_percent_full
				% how full the fact table hash tables should
				% be allowed to get, given as an integer
				% percentage.

	% Optimization Options
		;	opt_level
		;	opt_space	% default is to optimize time
		;	intermodule_optimization
		;	transitive_optimization
		;	split_c_files
	%	- HLDS
		;	inlining
		;	inline_simple
		;	inline_single_use
		;	inline_compound_threshold
		;	inline_simple_threshold
		;	inline_vars_threshold
		;	intermod_inline_simple_threshold
		;	common_struct
		;	common_goal
		;	constraint_propagation
		;	optimize_unused_args
		;	intermod_unused_args
		;	optimize_higher_order
		;	optimize_constructor_last_call
		;	optimize_duplicate_calls
		;	constant_propagation
		;	excess_assign
		;	optimize_saved_vars
		;	follow_code
		;	prev_code
		;	optimize_dead_procs
		;	deforestation
		;	deforestation_depth_limit
		;	deforestation_cost_factor
		;	deforestation_vars_threshold
		;	termination
		;	check_termination
		;	verbose_check_termination
		;	termination_single_args
		;	termination_norm
		;	termination_error_limit
		;	termination_path_limit
	%	- HLDS->LLDS
		;	smart_indexing
		;	  dense_switch_req_density
		;	  lookup_switch_req_density
		;	  dense_switch_size
		;	  lookup_switch_size
		;	  string_switch_size
		;	  tag_switch_size
		;	  try_switch_size
		;	  binary_switch_size
		;	static_ground_terms
		;	middle_rec
		;	simple_neg
		;	follow_vars
	%	- LLDS
		;	optimize
		;	optimize_peep
		;	optimize_jumps
		;	optimize_fulljumps
		;	optimize_labels
		;	optimize_dups
%%% unused:	;	optimize_copyprop
		;	optimize_value_number
		;	optimize_frames
		;	optimize_delay_slot
		;	optimize_repeat
		;	optimize_vnrepeat
		;	pred_value_number
		;	vn_fudge
	%	- C
		;	use_macro_for_redo_fail
		;	emit_c_loops
		;	procs_per_c_function
		;	everything_in_one_c_function
		;	c_optimize
		;	inline_alloc
	% Link options
		;	output_file_name
		;	link_flags
		;	link_library_directories
		;	link_libraries
		;	link_objects
	% Miscellaneous Options
		;	heap_space
		;	search_directories
		;	intermod_directories
		;	use_search_directories_for_intermod
		;	use_subdirs
		;	help.

:- implementation.

:- import_module string, bool, int, map, std_util, assoc_list, require, list.
:- import_module handle_options.

:- type option_category
	--->	warning_option
	;	verbosity_option
	;	output_option
	;	aux_output_option
	;	language_semantics_option
	;	compilation_model_option
	;	code_gen_option
	;	special_optimization_option
	;	optimization_option
	;	link_option
	;	miscellaneous_option.

option_defaults(Option, Default) :-
	option_defaults_2(_Category, OptionsList),
	list__member(Option - Default, OptionsList).

:- pred option_defaults_2(option_category, list(pair(option, option_data))).
:- mode option_defaults_2(in, out) is det.
:- mode option_defaults_2(out, out) is multidet.

option_defaults_2(warning_option, [
		% Warning Options
	inhibit_warnings	-	bool_special,
	halt_at_warn		-	bool(no),
	halt_at_syntax_errors	-	bool(no),
	%
	% IMPORTANT NOTE:
	% if you add any new warning options, or if you change
	% the default for an existing warning option to `yes',
	% then you will need to modify the handling of inhibit_warnings
	%
	warn_singleton_vars	-	bool(yes),
	warn_overlapping_scopes	-	bool(yes),
	warn_det_decls_too_lax	-	bool(yes),
	warn_nothing_exported	-	bool(yes),
	warn_unused_args	-	bool(no),
	warn_interface_imports	-	bool(yes),
	warn_non_stratification -	bool(no),
	warn_missing_opt_files  -	bool(yes),
	warn_missing_trans_opt_deps  -	bool(yes),
	warn_simple_code	-	bool(yes),
	warn_duplicate_calls	-	bool(no),
	warn_missing_module_name -	bool(yes),
	warn_wrong_module_name -	bool(yes)
]).
option_defaults_2(verbosity_option, [
		% Verbosity Options
	verbose			-	bool(no),
	very_verbose		-	bool(no),
	verbose_errors		-	bool(no),
	statistics		-	bool(no),
	debug_types		- 	bool(no),
	debug_modes		- 	bool(no),
	debug_inst_keys		- 	bool(no),
	debug_det		- 	bool(no),
	debug_opt		- 	bool(no),
	debug_vn		- 	int(0),
	debug_pd		-	bool(no)
]).
option_defaults_2(output_option, [
		% Output Options (mutually exclusive)
	generate_dependencies	-	bool(no),
	generate_module_order 	-	bool(no),
	make_short_interface	-	bool(no),
	make_interface		-	bool(no),
	make_private_interface	-	bool(no),
	make_optimization_interface -	bool(no),
	make_transitive_opt_interface -	bool(no),
	convert_to_mercury 	-	bool(no),
	convert_to_goedel 	-	bool(no),
	typecheck_only		-	bool(no),
	errorcheck_only		-	bool(no),
	compile_to_c		-	bool(no),
	compile_only		-	bool(no)
]).
option_defaults_2(aux_output_option, [
		% Auxiliary Output Options
	assume_gmake		-	bool(yes),
	trace			-	string("default"),
	generate_bytecode	-	bool(no),
	generate_prolog		-	bool(no),
	prolog_dialect		-	string("default"),
	line_numbers		-	bool(no),
	auto_comments		-	bool(no),
	show_dependency_graph	-	bool(no),
	dump_hlds		-	accumulating([]),
	verbose_dump_hlds	-	string("")
]).
option_defaults_2(language_semantics_option, [
	strict_sequential	-	special,
	reorder_conj		-	bool(yes),
	reorder_disj		-	bool(yes),
	fully_strict		-	bool(yes),
	infer_types		-	bool(no),
	infer_modes		-	bool(no),
	infer_det		-	bool(yes),
	infer_all		-	bool_special,
	type_inference_iteration_limit	-	int(60),
	mode_inference_iteration_limit	-	int(30)
]).
option_defaults_2(compilation_model_option, [
		% Compilation model options (ones that affect binary
		% compatibility).
	grade			-	string_special,
					% the `mmc' script will pass the
					% default grade determined
					% at configuration time
	gcc_non_local_gotos	-	bool(yes),
	gcc_global_registers	-	bool(yes),
	asm_labels		-	bool(yes),
	gc			-	string("conservative"),
	parallel		-	bool(no),
	profiling		-	bool_special,
	time_profiling		-	special,
	memory_profiling	-	special,
	profile_calls		-	bool(no),
	profile_time		-	bool(no),
	profile_memory		-	bool(no),
	debug			-	bool_special,
	require_tracing		-	bool(no),
	stack_trace		-	bool(no),
	use_trail		-	bool(no),
	pic_reg			-	bool(no),
	tags			-	string("low"),
	num_tag_bits		-	int(-1),
					% -1 is a special value which means
					% use the value of conf_low_tag_bits
					% instead
	bits_per_word		-	int(32),
					% A good default for the current
					% generation of architectures.
	bytes_per_word		-	int(4),
					% A good default for the current
					% generation of architectures.
	conf_low_tag_bits	-	int(2),
					% the `mmc' script will override the
					% above default with a value determined
					% at configuration time
	args			-	string("compact"),
	sync_term_size		-	int(8),
					% 8 is the size on linux (at the time
					% of writing) - will usually be over-
					% ridden by a value from configure.
	type_layout		-	bool(yes),
	basic_stack_layout	-	bool(no),
	agc_stack_layout	-	bool(no),
	procid_stack_layout	-	bool(no),
	trace_stack_layout	-	bool(no),
	typeinfo_liveness	-	bool(no),
	highlevel_c		-	bool(no),
	unboxed_float		-	bool(no)
]).
option_defaults_2(code_gen_option, [
		% Code Generation Options
	low_level_debug		-	bool(no),
	trad_passes		-	bool(yes),
	polymorphism		-	bool(yes),
	lazy_code		-	bool(yes),
	reclaim_heap_on_failure	-	bool_special,
	reclaim_heap_on_semidet_failure	-	bool(yes),
	reclaim_heap_on_nondet_failure	-	bool(yes),
	have_delay_slot		-	bool(no),
					% the `mmc' script may override the
					% above default if configure says
					% the machine has branch delay slots
	num_real_r_regs		-	int(5),
	num_real_f_regs		-	int(0),
	num_real_r_temps	-	int(5),
	num_real_f_temps	-	int(0),
					% the `mmc' script will override the
					% above defaults with values determined
					% at configuration time
	cc			-	string("gcc"),
					% the `mmc' script will override the
					% above default with a value determined
					% at configuration time
	cflags			-	accumulating([]),
	cflags_for_regs		-	string(""),
	cflags_for_gotos	-	string(""),
					% the `mmc' script will override the
					% above two defaults with values
					% determined at configuration time
	c_debug			-	bool(no),
	c_include_directory	-	string(""),
					% the `mmc' script will override the
					% above default with a value determined
					% at configuration time
	aditi			-	bool(no),
	fact_table_max_array_size -	int(1024),
	fact_table_hash_percent_full - 	int(90)
]).
option_defaults_2(special_optimization_option, [
		% Special optimization options.
		% These ones are not affected by `-O<n>'.
	opt_level		-	int_special,
	opt_space		-	special,
	intermodule_optimization -	bool(no),
	transitive_optimization -	bool(no),
	check_termination	-	bool(no),
	verbose_check_termination -	bool(no),
	termination		-	bool(no),
	termination_single_args	-	int(0),
	termination_norm	-	string("total"),
	termination_error_limit	-	int(3),
	termination_path_limit	-	int(256),
	split_c_files		-	bool(no)
]).
option_defaults_2(optimization_option, [
		% Optimization options
		%
		% IMPORTANT: the default here should be all optimizations OFF.
		% Optimizations should be enabled by the appropriate
		% optimization level in the opt_level table.
		%
% HLDS
	inlining		-	bool_special,
	inline_simple		-	bool(no),
	inline_single_use	-	bool(no),
	inline_compound_threshold -	int(0),
	inline_simple_threshold	-	int(5),	% has no effect until
						% --inline-simple is enabled
	inline_vars_threshold	-	int(100),
	intermod_inline_simple_threshold -	% has no effect until
					int(5),	% --intermodule-optimization
	common_struct		-	bool(no),
	common_goal		-	bool(yes),
		% common_goal is not really an optimization, since
		% it affects the semantics
	constraint_propagation	-	bool(no),
	optimize_duplicate_calls -	bool(no),
	constant_propagation	-	bool(no),
	excess_assign		-	bool(no),
	optimize_saved_vars	-	bool(no),
	prev_code		-	bool(no),
	follow_code		-	bool(no),
	optimize_unused_args	-	bool(no),
	intermod_unused_args	-	bool(no),
	optimize_higher_order	-	bool(no),
	optimize_constructor_last_call -	bool(no),
	optimize_dead_procs	-	bool(no),
	deforestation		-	bool(no),
	deforestation_depth_limit	-	int(4),
	deforestation_cost_factor	-	int(1000),
	deforestation_vars_threshold 	-	int(200),

% HLDS -> LLDS
	smart_indexing		-	bool(no),
	dense_switch_req_density -	int(25),
		% Minimum density before using a dense switch
	lookup_switch_req_density -	int(25),
		% Minimum density before using a lookup switch
	dense_switch_size	-	int(4),
	lookup_switch_size	-	int(4),
	string_switch_size	-	int(8),
	tag_switch_size		-	int(3),
	try_switch_size		-	int(3),
	binary_switch_size	-	int(4),
	static_ground_terms	-	bool(no),
	middle_rec		-	bool(no),
	simple_neg		-	bool(no),
	follow_vars		-	bool(no),

% LLDS
	optimize		-	bool(no),
	optimize_peep		-	bool(no),
	optimize_jumps		-	bool(no),
	optimize_fulljumps	-	bool(no),
	optimize_labels		-	bool(no),
	optimize_dups		-	bool(no),
%%%	optimize_copyprop	-	bool(no),
	optimize_value_number	-	bool(no),
	optimize_frames		-	bool(no),
	optimize_delay_slot	-	bool(no),
	optimize_repeat		-	int(0),
	optimize_vnrepeat	-	int(1),		% ineffective unless
							% value_number is set
	pred_value_number	-	bool(no),
	vn_fudge		-	int(1000),

% LLDS -> C
	use_macro_for_redo_fail	-	bool(no),
	emit_c_loops		-	bool(no),
	procs_per_c_function	-	int(1),
	everything_in_one_c_function -	special,
	c_optimize		-	bool(no),
	inline_alloc		-	bool(no)
]).
option_defaults_2(link_option, [
		% Link Options
	output_file_name	-	string(""),
					% if the output_file_name is an empty
					% string, we use the name of the first
					% module on the command line
	link_flags		-	accumulating([]),
	link_library_directories -	accumulating([]),
	link_libraries		-	accumulating([]),
	link_objects		-	accumulating([])
]).
option_defaults_2(miscellaneous_option, [
		% Miscellaneous Options
	heap_space		-	int(0),
	search_directories 	-	accumulating(["."]),
	intermod_directories	-	accumulating([]),
	use_search_directories_for_intermod
				-	bool(yes),
	use_subdirs		-	bool(no),
	help 			-	bool(no)
]).

	% please keep this in alphabetic order
short_option('c', 			compile_only).
short_option('C', 			compile_to_c).
short_option('d', 			dump_hlds).
short_option('D', 			verbose_dump_hlds).
short_option('e', 			errorcheck_only).
short_option('E', 			verbose_errors).
short_option('G', 			convert_to_goedel).
short_option('h', 			help).
short_option('i', 			make_interface).
short_option('I', 			search_directories).
short_option('l', 			link_libraries).
short_option('L', 			link_library_directories).
short_option('M', 			generate_dependencies).
short_option('N', 			debug_modes).
short_option('n', 			line_numbers).
short_option('o', 			output_file_name).
short_option('O', 			opt_level).
short_option('p', 			profiling).
short_option('P', 			convert_to_mercury).
short_option('s', 			grade).
short_option('S', 			statistics).
short_option('T', 			debug_types).
short_option('t', 			typecheck_only).
short_option('v', 			verbose).
short_option('V', 			very_verbose).
short_option('w', 			inhibit_warnings).
short_option('?', 			help).

% warning options
long_option("inhibit-warnings",		inhibit_warnings).
long_option("halt-at-warn",		halt_at_warn).
long_option("halt-at-syntax-errors",	halt_at_syntax_errors).
long_option("warn-singleton-variables",	warn_singleton_vars).
long_option("warn-overlapping-scopes",	warn_overlapping_scopes).
long_option("warn-det-decls-too-lax",	warn_det_decls_too_lax).
long_option("warn-nothing-exported",	warn_nothing_exported).
long_option("warn-unused-args",		warn_unused_args).
long_option("warn-interface-imports",	warn_interface_imports).
long_option("warn-non-stratification",	warn_non_stratification).
long_option("warn-missing-opt-files",	warn_missing_opt_files).
long_option("warn-missing-trans-opt-deps",	warn_missing_trans_opt_deps).
long_option("warn-simple-code",		warn_simple_code).
long_option("warn-duplicate-calls",	warn_duplicate_calls).
long_option("warn-missing-module-name",	warn_missing_module_name).
long_option("warn-wrong-module-name",	warn_wrong_module_name).

% verbosity options
long_option("verbose",			verbose).
long_option("very-verbose",		very_verbose).
long_option("verbose-error-messages",	verbose_errors).
long_option("statistics",		statistics).
long_option("debug-types",		debug_types).
long_option("debug-modes",		debug_modes).
long_option("debug-inst-keys",		debug_inst_keys).
long_option("debug-determinism",	debug_det).
long_option("debug-det",		debug_det).
long_option("debug-opt",		debug_opt).
long_option("debug-vn",			debug_vn).
long_option("debug-pd",			debug_pd).

% output options (mutually exclusive)
long_option("generate-dependencies",	generate_dependencies).
long_option("generate-module-order",	generate_module_order).
long_option("make-short-interface",	make_short_interface).
long_option("make-short-int",		make_short_interface).
long_option("make-interface",		make_interface).
long_option("make-int",			make_interface).
long_option("make-private-interface",	make_private_interface).
long_option("make-priv-int",		make_private_interface).
long_option("make-optimization-interface",
					make_optimization_interface).
long_option("make-optimisation-interface",
					make_optimization_interface).
long_option("make-opt-int",		make_optimization_interface).
long_option("make-transitive-optimization-interface",
					make_transitive_opt_interface).
long_option("make-transitive-optimisation-interface",
					make_transitive_opt_interface).
long_option("make-trans-opt", 		make_transitive_opt_interface).
long_option("convert-to-mercury", 	convert_to_mercury).
long_option("convert-to-Mercury", 	convert_to_mercury). 
long_option("pretty-print", 		convert_to_mercury).
long_option("convert-to-goedel", 	convert_to_goedel).
long_option("convert-to-Goedel", 	convert_to_goedel).
long_option("typecheck-only",		typecheck_only).
long_option("errorcheck-only",		errorcheck_only).
long_option("compile-to-c",		compile_to_c).
long_option("compile-to-C",		compile_to_c).
long_option("compile-only",		compile_only).

% aux output options
long_option("assume-gmake",		assume_gmake).
long_option("trace",			trace).
long_option("generate-bytecode",	generate_bytecode).
long_option("generate-prolog",		generate_prolog).
long_option("generate-Prolog",		generate_prolog).
long_option("prolog-dialect",		prolog_dialect).
long_option("line-numbers",		line_numbers).
long_option("auto-comments",		auto_comments).
long_option("show-dependency-graph",	show_dependency_graph).
long_option("dump-hlds",		dump_hlds).
long_option("verbose-dump-hlds",	verbose_dump_hlds).

% language semantics options
long_option("reorder-conj",		reorder_conj).
long_option("reorder-disj",		reorder_disj).
long_option("fully-strict",		fully_strict).
long_option("strict-sequential",	strict_sequential).
long_option("infer-all",		infer_all).
long_option("infer-types",		infer_types).
long_option("infer-modes",		infer_modes).
long_option("infer-determinism",	infer_det).
long_option("infer-det",		infer_det).
long_option("type-inference-iteration-limit",
					type_inference_iteration_limit).
long_option("mode-inference-iteration-limit",
					mode_inference_iteration_limit).

% compilation model options
long_option("grade",			grade).
long_option("gcc-non-local-gotos",	gcc_non_local_gotos).
long_option("gcc-global-registers",	gcc_global_registers).
long_option("asm-labels",		asm_labels).
long_option("gc",			gc).
long_option("garbage-collection",	gc).
long_option("parallel",			parallel).
long_option("profiling",		profiling).
long_option("time-profiling",		time_profiling).
long_option("memory-profiling",		memory_profiling).
long_option("profile-calls",		profile_calls).
long_option("profile-time",		profile_time).
long_option("profile-memory",		profile_memory).
long_option("debug",			debug).
% The following options are not allowed, because they're
% not very useful and would probably only confuse people.
% long_option("stack-trace",		stack_trace).
% long_option("require-tracing",		require_tracking).
long_option("use-trail",		use_trail).
long_option("pic-reg",			pic_reg).
long_option("tags",			tags).
long_option("num-tag-bits",		num_tag_bits).
long_option("bits-per-word",		bits_per_word).
long_option("bytes-per-word",		bytes_per_word).
long_option("conf-low-tag-bits",	conf_low_tag_bits).
long_option("args",			args).
long_option("arg-convention",		args).
long_option("type-layout",		type_layout).
long_option("agc-stack-layout",		agc_stack_layout).
long_option("basic-stack-layout",	basic_stack_layout).
long_option("procid-stack-layout",	procid_stack_layout).
long_option("trace-stack-layout",	trace_stack_layout).
long_option("typeinfo-liveness",	typeinfo_liveness).
long_option("highlevel-C",		highlevel_c).
long_option("highlevel-c",		highlevel_c).
long_option("high-level-C",		highlevel_c).
long_option("high-level-c",		highlevel_c).
long_option("unboxed-float",		unboxed_float).

% code generation options
long_option("low-level-debug",		low_level_debug).
long_option("polymorphism",		polymorphism).
long_option("trad-passes",		trad_passes).
long_option("lazy-code",		lazy_code).
long_option("reclaim-heap-on-failure",	reclaim_heap_on_failure).
long_option("reclaim-heap-on-semidet-failure",
					reclaim_heap_on_semidet_failure).
long_option("reclaim-heap-on-nondet-failure",
					reclaim_heap_on_nondet_failure).
long_option("branch-delay-slot",	have_delay_slot).
long_option("have-delay-slot",		have_delay_slot).
long_option("num-real-r-regs",		num_real_r_regs).
long_option("num-real-f-regs",		num_real_f_regs).
long_option("num-real-r-temps",		num_real_r_temps).
long_option("num-real-f-temps",		num_real_f_temps).
long_option("num-real-temps",		num_real_r_temps).	% obsolete
long_option("cc",			cc).
long_option("cflags",			cflags).
long_option("cflags-for-regs",		cflags_for_regs).
long_option("cflags-for-gotos",		cflags_for_gotos).
long_option("c-debug",			c_debug).
long_option("c-include-directory",	c_include_directory).
long_option("fact-table-max-array-size",fact_table_max_array_size).
long_option("fact-table-hash-percent-full",
					fact_table_hash_percent_full).

% optimization options

long_option("opt-level",		opt_level).
long_option("optimization-level",	opt_level).
long_option("optimisation-level",	opt_level).
long_option("opt-space",		opt_space).
long_option("optimize-space",		opt_space).
long_option("optimise-space",		opt_space).
long_option("intermodule-optimization", intermodule_optimization).
long_option("intermodule-optimisation", intermodule_optimization).
long_option("transitive-intermodule-optimization", 
					transitive_optimization).
long_option("transitive-intermodule-optimisation", 
					transitive_optimization).
long_option("trans-intermod-opt", 	transitive_optimization).

% HLDS->HLDS optimizations
long_option("inlining", 		inlining).
long_option("inline-simple",		inline_simple).
long_option("inline-single-use",	inline_single_use).
long_option("inline-compound-threshold",	inline_compound_threshold).
long_option("inline-simple-threshold",		inline_simple_threshold).
long_option("intermod-inline-simple-threshold",
					intermod_inline_simple_threshold).
long_option("inline-vars-threshold",		inline_vars_threshold).
long_option("common-struct",		common_struct).
long_option("common-goal",		common_goal).
long_option("excess-assign",		excess_assign).
long_option("optimize-duplicate-calls", optimize_duplicate_calls).
long_option("optimise-duplicate-calls", optimize_duplicate_calls).
long_option("optimise-constant-propagation", constant_propagation).
long_option("optimize-constant-propagation", constant_propagation).
long_option("optimize-saved-vars",	optimize_saved_vars).
long_option("optimise-saved-vars",	optimize_saved_vars).
long_option("prev-code",		prev_code).
long_option("follow-code",		follow_code).
long_option("constraint-propagation",	constraint_propagation).
long_option("optimize-unused-args",	optimize_unused_args).
long_option("optimise-unused-args",	optimize_unused_args).
long_option("intermod-unused-args",	intermod_unused_args).
long_option("optimize-higher-order",	optimize_higher_order).
long_option("optimise-higher-order",	optimize_higher_order).
long_option("optimise-constructor-last-call",	optimize_constructor_last_call).
long_option("optimize-constructor-last-call",	optimize_constructor_last_call).
long_option("optimize-dead-procs",	optimize_dead_procs).
long_option("optimise-dead-procs",	optimize_dead_procs).
long_option("deforestation",		deforestation).
long_option("deforestation-depth-limit",	deforestation_depth_limit).
long_option("deforestation-cost-factor",	deforestation_cost_factor).
long_option("deforestation-vars-threshold",	deforestation_vars_threshold).
long_option("enable-termination",	termination).
long_option("enable-term",		termination).
long_option("check-termination",	check_termination).
long_option("check-term",		check_termination).
long_option("chk-term",			check_termination).
long_option("verbose-check-termination",verbose_check_termination).
long_option("verb-check-term",		verbose_check_termination).
long_option("verb-chk-term",		verbose_check_termination).
long_option("termination-single-argument-analysis",
					termination_single_args).
long_option("term-single-arg", 		termination_single_args).
long_option("termination-norm",		termination_norm).
long_option("term-norm",		termination_norm).
long_option("termination-error-limit",	termination_error_limit).
long_option("term-err-limit",		termination_error_limit).
long_option("termination-path-limit",	termination_path_limit).
long_option("term-path-limit",		termination_path_limit).

% HLDS->LLDS optimizations
long_option("smart-indexing",		smart_indexing).
long_option("dense-switch-req-density",	dense_switch_req_density).
long_option("lookup-switch-req-density",lookup_switch_req_density).
long_option("dense-switch-size",	dense_switch_size).
long_option("lookup-switch-size",	lookup_switch_size).
long_option("string-switch-size",	string_switch_size).
long_option("tag-switch-size",		tag_switch_size).
long_option("try-switch-size",		try_switch_size).
long_option("binary-switch-size",	binary_switch_size).
long_option("static-ground-terms",	static_ground_terms).
long_option("middle-rec",		middle_rec).
long_option("simple_neg",		simple_neg).
long_option("follow-vars",		follow_vars).

% LLDS optimizations
long_option("llds-optimize",		optimize).
long_option("llds-optimise",		optimize).
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
%%% long_option("optimize-copyprop",	optimize_copyprop).
%%% long_option("optimise-copyprop",	optimize_copyprop).
long_option("optimize-value-number",	optimize_value_number).
long_option("optimise-value-number",	optimize_value_number).
long_option("optimize-frames",		optimize_frames).
long_option("optimise-frames",		optimize_frames).
long_option("optimize-delay-slot",	optimize_delay_slot).
long_option("optimise-delay-slot",	optimize_delay_slot).
long_option("optimize-repeat",		optimize_repeat).
long_option("optimise-repeat",		optimize_repeat).
long_option("optimize-vnrepeat",	optimize_vnrepeat).
long_option("optimise-vnrepeat",	optimize_vnrepeat).
long_option("pred-value-number",	pred_value_number).
long_option("vn-fudge",			vn_fudge).

% LLDS->C optimizations
long_option("use-macro-for-redo-fail",	use_macro_for_redo_fail).
long_option("emit-c-loops",		emit_c_loops).
long_option("procs-per-c-function",	procs_per_c_function).
long_option("procs-per-C-function",	procs_per_c_function).
long_option("everything-in-one-c-function",	everything_in_one_c_function).
long_option("everything-in-one-C-function",	everything_in_one_c_function).
long_option("split-c-files",		split_c_files).
long_option("split-C-files",		split_c_files).
long_option("c-optimise",		c_optimize).
long_option("c-optimize",		c_optimize).
long_option("inline-alloc",		inline_alloc).

% link options
long_option("output-file",		output_file_name).
long_option("link-flags",		link_flags).
long_option("library-directory",	link_library_directories).
long_option("library",			link_libraries).
long_option("link-object",		link_objects).

% misc options
long_option("help",			help).
long_option("heap-space",		heap_space).
long_option("search-directory",		search_directories).
long_option("intermod-directory",	intermod_directories).
long_option("use-search-directories-for-intermod",
					use_search_directories_for_intermod).	
long_option("use-subdirs",		use_subdirs).	

%-----------------------------------------------------------------------------%

special_handler(grade, string(Grade), OptionTable0, Result) :-
	( convert_grade_option(Grade, OptionTable0, OptionTable) ->
		Result = ok(OptionTable)
	;
		string__append_list(["invalid grade `", Grade, "'"], Msg),
		Result = error(Msg)
	).
special_handler(profiling, bool(Value), OptionTable0, ok(OptionTable)) :-
	map__set(OptionTable0, profile_time, bool(Value), OptionTable1),
	map__set(OptionTable1, profile_calls, bool(Value), OptionTable2),
        map__set(OptionTable2, profile_memory, bool(no), OptionTable).
special_handler(time_profiling, none, OptionTable0, ok(OptionTable)) :-
	map__set(OptionTable0, profile_time, bool(yes), OptionTable1),
	map__set(OptionTable1, profile_calls, bool(yes), OptionTable2),
        map__set(OptionTable2, profile_memory, bool(no), OptionTable).
special_handler(memory_profiling, none, OptionTable0, ok(OptionTable)) :-
	map__set(OptionTable0, profile_time, bool(no), OptionTable1),
	map__set(OptionTable1, profile_calls, bool(yes), OptionTable2),
        map__set(OptionTable2, profile_memory, bool(yes), OptionTable).
special_handler(inlining, bool(Value), OptionTable0, ok(OptionTable)) :-
	map__set(OptionTable0, inline_simple, bool(Value), OptionTable1),
	map__set(OptionTable1, inline_single_use, bool(Value), OptionTable2),
	(
		Value = yes,
		map__set(OptionTable2, inline_compound_threshold,
			int(10), OptionTable)
	;
		Value = no,
		map__set(OptionTable2, inline_compound_threshold,
			int(0), OptionTable)
	).
special_handler(everything_in_one_c_function, none, OptionTable0,
		ok(OptionTable)) :-
	map__set(OptionTable0, procs_per_c_function, int(0),
		OptionTable).
special_handler(reclaim_heap_on_failure, bool(Value), OptionTable0,
			ok(OptionTable)) :-
	map__set(OptionTable0, reclaim_heap_on_semidet_failure, bool(Value),
		OptionTable1),
	map__set(OptionTable1, reclaim_heap_on_nondet_failure, bool(Value),
		OptionTable).
special_handler(strict_sequential, none, OptionTable0, ok(OptionTable)) :-
	override_options([
			reorder_conj 	-	bool(no),
			reorder_disj 	-	bool(no),
			fully_strict 	-	bool(yes)
		], OptionTable0, OptionTable).
special_handler(inhibit_warnings, bool(Inhibit), OptionTable0, ok(OptionTable))
		:-
	bool__not(Inhibit, Enable),
	override_options([
			warn_singleton_vars	-	bool(Enable),
			warn_overlapping_scopes	-	bool(Enable),
			warn_det_decls_too_lax	-	bool(Enable),
			warn_nothing_exported	-	bool(Enable),
			warn_interface_imports	-	bool(Enable),
			warn_missing_opt_files	-	bool(Enable),
			warn_missing_trans_opt_deps -	bool(Enable),
			warn_simple_code	-	bool(Enable),
			warn_missing_module_name -	bool(Enable),
			warn_wrong_module_name	-	bool(Enable)
		], OptionTable0, OptionTable).
special_handler(infer_all, bool(Infer), OptionTable0, ok(OptionTable)) :-
	override_options([
			infer_types 		-	bool(Infer),
			infer_modes 		-	bool(Infer),
			infer_det	 	-	bool(Infer)
		], OptionTable0, OptionTable).
special_handler(opt_space, none, OptionTable0, ok(OptionTable)) :-
	opt_space(OptionSettingsList),
	override_options(OptionSettingsList, OptionTable0, OptionTable).
special_handler(opt_level, int(N0), OptionTable0, ok(OptionTable)) :-
	( N0 > 6 ->
		N = 6
	; N0 < -1 ->
		N = -1
	;
		N = N0
	),
	set_opt_level(N, OptionTable0, OptionTable).

:- pred set_opt_level(int, option_table, option_table).
:- mode set_opt_level(in, in, out) is det.

set_opt_level(N, OptionTable0, OptionTable) :-
	%
	% first reset all optimizations to their default
	% (the default should be all optimizations off)
	%
	option_defaults_2(optimization_option, OptimizationDefaults),
	override_options(OptimizationDefaults, OptionTable0, OptionTable1),
	%
	% next enable the optimization levels from 0 up to N.
	%
	enable_opt_levels(0, N, OptionTable1, OptionTable).

:- pred enable_opt_levels(int, int, option_table, option_table).
:- mode enable_opt_levels(in, in, in, out) is det.

enable_opt_levels(N0, N, OptionTable0, OptionTable) :-
	( N0 > N ->
		OptionTable = OptionTable0
	; opt_level(N0, OptionTable0, OptionSettingsList) ->
		override_options(OptionSettingsList, OptionTable0,
			OptionTable1),
		N1 is N0 + 1,
		enable_opt_levels(N1, N, OptionTable1, OptionTable)
	;
		error("Unknown optimization level")
	).

:- pred override_options(list(pair(option, option_data)),
	option_table, option_table).
:- mode override_options(in, in, out) is det.

override_options([], OptionTable, OptionTable).
override_options([Option - Value | Settings], OptionTable0, OptionTable) :-
	map__set(OptionTable0, Option, Value, OptionTable1),
	override_options(Settings, OptionTable1, OptionTable).

%-----------------------------------------------------------------------------%

:- pred opt_space(list(pair(option, option_data))::out) is det.

opt_space([
	optimize_dead_procs	-	bool(yes),
	optimize_labels		-	bool(yes),
	optimize_dups		-	bool(yes),
	optimize_fulljumps	-	bool(no),
	inline_alloc		-	bool(no),
	use_macro_for_redo_fail	-	bool(no)
]).

%-----------------------------------------------------------------------------%

:- pred opt_level(int::in, option_table::in,
	list(pair(option, option_data))::out) is semidet.

% Optimization level -1:
% Generate totally unoptimized code; turns off ALL optimizations that
% can be turned off, including HLDS->HLDS, HLDS->LLDS, LLDS->LLDS, LLDS->C,
% and C->object optimizations.  
% (However, there are some optimizations that can't be disabled.)

% Optimization level 0: aim to minimize overall compilation time.
% XXX I just guessed.  We should run lots of experiments.

opt_level(0, _, [
	optimize		-	bool(yes),
	optimize_repeat		-	int(1),
	optimize_peep		-	bool(yes),
	static_ground_terms	-	bool(yes),
	smart_indexing		-	bool(yes),
	optimize_jumps		-	bool(yes),
	optimize_labels		-	bool(yes),
	excess_assign		-	bool(yes)	% ???
]).

% Optimization level 1: apply optimizations which are cheap and
% have a good payoff while still keeping compilation time small

opt_level(1, OptionTable, [
	c_optimize		-	bool(yes),	% XXX we want `gcc -O1'
	optimize_frames		-	bool(yes),
	optimize_delay_slot	-	bool(DelaySlot),
	follow_vars		-	bool(yes),
	middle_rec		-	bool(yes),
	emit_c_loops		-	bool(yes)
	% dups?
]) :-
	getopt__lookup_bool_option(OptionTable, have_delay_slot, DelaySlot).

% Optimization level 2: apply optimizations which have a good
% payoff relative to their cost; but include optimizations
% which are more costly than with -O1

opt_level(2, _, [
	optimize_fulljumps	-	bool(yes),
	optimize_repeat		-	int(3),
	optimize_dups		-	bool(yes),
	optimize_dead_procs	-	bool(yes),
	follow_code		-	bool(yes),
	inline_simple		-	bool(yes),
	inline_single_use	-	bool(yes),
	inline_compound_threshold -	int(10),
	common_struct		-	bool(no),	% YYY
/****
% XXX optimize_duplicate_calls is broken --
% it doesn't take type information into account.
% See tests/hard_coded/dupcall_types_bug.m.
% We should re-enable the optimization only
% when we have fixed that bug.
	optimize_duplicate_calls -	bool(yes),
****/
	simple_neg		-	bool(yes)
]).

% Optimization level 3: apply optimizations which usually have a good
% payoff even if they increase compilation time quite a bit

opt_level(3, _, [
%%%	optimize_copyprop	-	bool(yes),
	optimize_saved_vars	-	bool(yes),
	optimize_unused_args	-	bool(yes),	
	optimize_higher_order	-	bool(yes),
	deforestation		-	bool(yes),
	constant_propagation	-	bool(yes),
	optimize_repeat		-	int(4)
]).

% Optimization level 4: apply optimizations which may have some
% payoff even if they increase compilation time quite a bit

% Currently this enables value_number
% and increases the inlining thresholds

opt_level(4, _, [
	optimize_value_number	-	bool(yes),
	inline_simple_threshold	-	int(8),
	inline_compound_threshold -	int(20)
]).

% Optimization level 5: apply optimizations which may have some
% payoff even if they increase compilation time a lot

% Currently this enables pred_value_number
% and runs a second pass of value numbering

opt_level(5, _, [
	pred_value_number	-	bool(yes),
	optimize_repeat		-	int(5),
	optimize_vnrepeat	-	int(2),
	inline_compound_threshold -	int(100)
]).

% Optimization level 6: apply optimizations which may have any
% payoff even if they increase compilation time to completely
% unreasonable levels

% Currently this sets `everything_in_one_c_function', which causes
% the compiler to put everything in the one C function and treat
% calls to predicates in the same module as local.
% We also enable inlining of GC_malloc(), redo(), and fail().

opt_level(6, _, [
	procs_per_c_function	-	int(0),	% everything in one C function
	inline_alloc		-	bool(yes),
	use_macro_for_redo_fail	-	bool(yes)
]).

%-----------------------------------------------------------------------------%

options_help -->
	io__write_string("\t-?, -h, --help\n"),
	io__write_string("\t\tPrint this usage message.\n"),
	options_help_warning,
	options_help_verbosity,
	options_help_output,
	options_help_aux_output,
	options_help_semantics,
	options_help_termination,
	options_help_compilation_model,
	options_help_code_generation,
	options_help_optimization,
	options_help_hlds_hlds_optimization,
	options_help_hlds_llds_optimization,
	options_help_llds_llds_optimization,
	options_help_output_optimization,
	options_help_object_optimization,
	options_help_link,
	options_help_misc.

:- pred options_help_warning(io__state::di, io__state::uo) is det.

options_help_warning -->
	io__write_string("\nWarning Options:\n"),
	io__write_string("\t-w, --inhibit-warnings\n"),
	io__write_string("\t\tDisable all warning messages.\n"),
	io__write_string("\t--halt-at-warn\n"),
	io__write_string("\t\tThis option causes the compiler to treat all \n"),
	io__write_string("\t\twarnings as if they were errors.  This means that\n"),
	io__write_string("\t\tif any warning is issued, the compiler will not\n"),
	io__write_string("\t\tgenerate code --- instead, it will return a\n"),
	io__write_string("\t\tnon-zero exit status.\n"),
	io__write_string("\t--halt-at-syntax-errors\n"),
	io__write_string("\t\tThis option causes the compiler to halt immediately\n"),
	io__write_string("\t\tafter syntax checking and not do any semantic checking\n"),
	io__write_string("\t\tif it finds any syntax errors in the program.\n"),
	io__write_string("\t--no-warn-singleton-variables\n"),
	io__write_string("\t\tDon't warn about variables which only occur once.\n"),
	io__write_string("\t--no-warn-overlapping-scopes\n"),
	io__write_string("\t\tDon't warn about variables which occur in overlapping scopes.\n"),
	io__write_string("\t--no-warn-det-decls-too-lax\n"),
	io__write_string("\t\tDon't warn about determinism declarations\n"),
	io__write_string("\t\twhich could have been stricter.\n"),
	io__write_string("\t--no-warn-nothing-exported\n"),
	io__write_string("\t\tDon't warn about modules which export nothing.\n"),
	io__write_string("\t--warn-unused-args\n"),
	io__write_string("\t\tWarn about predicate arguments which are not used.\n"),
	io__write_string("\t--warn-interface-imports\n"),
	io__write_string("\t\tWarn about modules imported in the interface, but\n"),
	io__write_string("\t\twhich are not used in the interface.\n"),
	io__write_string("\t--no-warn-missing-opt-files\n"),
	io__write_string("\t\tDisable warnings about `.opt' files which cannot be opened.\n"),
	io__write_string("\t--no-warn-missing-trans-opt-deps\n"),
	io__write_string("\t\tDisable warnings produced when the information required\n"),
	io__write_string("\t\tto allow `.trans_opt' files to be read when creating other\n"),
	io__write_string("\t\t`.trans_opt' files has been lost.  The information can be\n"),
	io__write_string("\t\trecreated by running `mmake <mainmodule>.depend'\n"),

	io__write_string("\t--warn-non-stratification\n"),
	io__write_string("\t\tWarn about possible non-stratification in the module.\n"),
	io__write_string("\t\tNon-stratification occurs when a predicate/function can call\n"),
	io__write_string("\t\titself negatively through some path along its call graph.\n"),
	io__write_string("\t--no-warn-simple-code\n"),
	io__write_string("\t\tDisable warnings about constructs which are so\n"),
	io__write_string("\t\tsimple that they are likely to be programming errors.\n"),
	io__write_string("\t--warn-duplicate-calls\n"),
	io__write_string("\t\tWarn about multiple calls to a predicate with the\n"),
	io__write_string("\t\tsame input arguments.\n"),
	io__write_string("\t--no-warn-missing-module-name\n"),
	io__write_string("\t\tDisable warnings for modules that do no start with\n"),
	io__write_string("\t\ta `:- module' declaration.\n"),
	io__write_string("\t--no-warn-wrong-module-name\n"),
	io__write_string("\t\tDisable warnings for modules whose `:- module'\n"),
	io__write_string("\t\tdeclaration does not match the module's file name.\n").

:- pred options_help_verbosity(io__state::di, io__state::uo) is det.

options_help_verbosity -->
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
	io__write_string("\t\tAt the moment this option implies `--no-trad-passes', so you get\n"),
	io__write_string("\t\tinformation at the boundaries between phases of the compiler.\n"),
	io__write_string("\t-T, --debug-types\n"),
	io__write_string("\t\tOutput detailed debugging traces of the type checking.\n"),
	io__write_string("\t-N, --debug-modes\n"),
	io__write_string("\t\tOutput detailed debugging traces of the mode checking.\n"),
	io__write_string("\t--debug-inst-keys\n"),
	io__write_string("\t\tOutput debugging traces of the inst key tables.\n"),
	io__write_string("\t\tOnly works when --debug-modes is enabled.\n"),
	io__write_string("\t--debug-det, --debug-determinism\n"),
	io__write_string("\t\tOutput detailed debugging traces of determinism analysis.\n"),
	io__write_string("\t--debug-opt\n"),
	io__write_string("\t\tOutput detailed debugging traces of the optimization process.\n"),
	io__write_string("\t--debug-vn <n>\n"),
	io__write_string("\t\tOutput detailed debugging traces of the value numbering\n"),
	io__write_string("\t\toptimization pass. The different bits in the number\n"),
	io__write_string("\t\targument of this option control the printing of\n"),
	io__write_string("\t\tdifferent types of tracing messages.\n"),
	io__write_string("\t--debug-pd\n"),
	io__write_string("\t\tOutput detailed debugging traces of the partial\n"),
	io__write_string("\t\tdeduction and deforestation process.\n").

:- pred options_help_output(io__state::di, io__state::uo) is det.

options_help_output -->
	io__write_string("\nOutput Options:\n"),
	io__write_string("\tThese options are mutually exclusive.\n"),
	io__write_string("\tOnly the first one specified will apply.\n"),
	io__write_string("\tIf none of these options are specified, the default action\n"),
	io__write_string("\tis to link the named modules to produce an executable.\n\n"),
	io__write_string("\t-M, --generate-dependencies\n"),
	io__write_string("\t\tOutput `Make'-style dependencies for the module\n"),
	io__write_string("\t\tand all of its dependencies to `<module>.dep'.\n"),
	io__write_string("\t--generate-module-order\n"),
	io__write_string("\t\tOutput the strongly connected components of the module\n"),
	io__write_string("\t\tdependency graph in top-down order to `<module>.order'.\n"),
	io__write_string("\t\tImplies --generate-dependencies.\n"),
	io__write_string("\t-i, --make-int, --make-interface\n"),
	io__write_string("\t\tWrite the module interface to `<module>.int',\n"),
	io__write_string("\t\tand write the short interface to `<module>.int2'\n"),
	io__write_string("\t\tThis option should only be used by mmake.\n"),
	io__write_string("\t--make-priv-int, --make-private-interface\n"),
	io__write_string("\t\tWrite the private interface to `<module>.int0'.\n"),
	io__write_string("\t\tThis option should only be used by mmake.\n"),
	io__write_string("\t--make-short-int, --make-short-interface\n"),
	io__write_string("\t\tWrite the unqualified short interface to `<module>.int3'.\n"),
	io__write_string("\t\tThis option should only be used by mmake.\n"),
	io__write_string("\t--make-opt-int, --make-optimization-interface\n"),
	io__write_string("\t\tWrite inter-module optimization information to\n"),
	io__write_string("\t\t`<module>.opt'.\n"),
	io__write_string("\t\tThis option should only be used by mmake.\n"),
	io__write_string("\t--make-trans-opt\n"),
	io__write_string("\t--make-transitive-optimization-interface\n"),
	io__write_string("\t\tOutput transitive optimization information\n"),
	io__write_string("\t\tinto the `<module>.trans_opt' file.\n"),
	io__write_string("\t\tThis option should only be used by mmake.\n"),
	io__write_string("\t-G, --convert-to-goedel\n"),
	io__write_string("\t\tConvert to Goedel. Output to file `<module>.loc'.\n"),
	io__write_string("\t\tNote that some Mercury language constructs cannot\n"),
	io__write_string("\t\t(easily) be translated into Goedel.\n"),
	io__write_string("\t-P, --convert-to-mercury\n"),
	io__write_string("\t\tConvert to Mercury. Output to file `<module>.ugly'\n"),
	io__write_string("\t\tThis option acts as a Mercury ugly-printer.\n"),
	io__write_string("\t-t, --typecheck-only\n"),
	io__write_string("\t\tJust check that the code is syntactically correct and\n"),
	io__write_string("\t\ttype-correct. Don't check modes or determinism,\n"),
	io__write_string("\t\tand don't generate any code.\n"),
	io__write_string("\t-e, --errorcheck-only\n"),
	io__write_string("\t\tCheck the module for errors, but do not generate any code.\n"),
	io__write_string("\t-C, --compile-to-c\n"),
	io__write_string("\t\tGenerate C code in `<module>.c', but not object code.\n"),
	io__write_string("\t-c, --compile-only\n"),
	io__write_string("\t\tGenerate C code in `<module>.c' and object code in `<module>.o'\n"),
	io__write_string("\t\tbut do not attempt to link the named modules.\n").

:- pred options_help_aux_output(io__state::di, io__state::uo) is det.

options_help_aux_output -->
	io__write_string("\n Auxiliary Output Options:\n"),
	io__write_string("\t--no-assume-gmake\n"),
	io__write_string("\t\tWhen generating `.dep' files, generate Makefile\n"),
	io__write_string("\t\tfragments that use only the features of standard make;\n"),
	io__write_string("\t\tdo not assume the availability of GNU Make extensions.\n"),
	io__write_string("\t--trace {minimum, interfaces, all, default}\n"),
	io__write_string("\t\tGenerate code that includes the specified level\n"), 
	io__write_string("\t\tof execution tracing.\n"),
	io__write_string("\t\tSee the [XXX not yet written!] chapter of the\n"),
	io__write_string("\t\tMercury User's Guide for details.\n"),
	io__write_string("\t--generate-bytecode\n"),
	io__write_string("\t\tOutput a bytecode form of the module for use\n"),
	io__write_string("\t\tby an experimental debugger.\n"),
	io__write_string("\t--generate-prolog\n"),
	io__write_string("\t\tConvert the program to Prolog. Output to file `<module>.pl'\n"),
	io__write_string("\t\tor `<module>.nl' (depending on the dialect).\n"),
	io__write_string("\t--prolog-dialect {sicstus,nu}\n"),
	io__write_string("\t\tTarget the named dialect if generating Prolog code.\n"),
	io__write_string("\t-l, --line-numbers\n"),
	io__write_string("\t\tOutput line numbers in the generated code.\n"),
	io__write_string("\t\tOnly works with the `-G' and `-P' options.\n"),
	io__write_string("\t--auto-comments\n"),
	io__write_string("\t\tOutput comments in the `<module>.c' file.\n"),
	io__write_string("\t\t(The code may be easier to understand if you also\n"),
	io__write_string("\t\tuse the `--no-llds-optimize' option.)\n"),
	io__write_string("\t--show-dependency-graph\n"),
	io__write_string("\t\tWrite out the dependency graph to `<module>.dependency_graph'.\n"),
	io__write_string("\t-d <n>, --dump-hlds <stage number or name>\n"),
	io__write_string("\t\tDump the HLDS (intermediate representation) after\n"),
	io__write_string("\t\tthe specified stage to `<module>.hlds_dump.<num>-<name>'.\n"),
	io__write_string("\t\tStage numbers range from 1-99.\n"),
	io__write_string("\t\tMultiple dump options accumulate.\n"),
	io__write_string("\t-D, --verbose-dump-hlds <fields>\n"),
	io__write_string("\t\tWith `--dump-hlds', include extra detail in the dump.\n"),
	io__write_string("\t\tEach type of detail is included in the dump if its\n"),
	io__write_string("\t\tcorresponding letter occurs in the option argument\n"),
	io__write_string("\t\t(see the Mercury User's Guide for details).\n").

:- pred options_help_semantics(io__state::di, io__state::uo) is det.

options_help_semantics -->
	io__write_string("\nLanguage semantics options:\n"),
	io__write_string("(See the Mercury language reference manual for detailed explanations.)\n"),
	io__write_string("\t--no-reorder-conj\n"),
	io__write_string("\t\tExecute conjunctions left-to-right except where the modes imply\n"),
	io__write_string("\t\tthat reordering is unavoidable.\n"),
	io__write_string("\t--no-reorder-disj\n"),
	io__write_string("\t\tExecute disjunctions strictly left-to-right.\n"),
	io__write_string("\t--fully-strict\n"),
	io__write_string("\t\tDon't optimize away loops or calls to error/1.\n"),
	io__write_string("\t--infer-types\n"),
	io__write_string("\t\tIf there is no type declaration for a predicate or function,\n"),
	io__write_string("\t\ttry to infer the type, rather than just reporting an error.\n"),
	io__write_string("\t--infer-modes\n"),
	io__write_string("\t\tIf there is no mode declaration for a predicate,\n"),
	io__write_string("\t\ttry to infer the modes, rather than just reporting an error.\n"),

	io__write_string("\t--no-infer-det, --no-infer-determinism\n"),
	io__write_string("\t\tIf there is no determinism declaration for a procedure,\n"),
	io__write_string("\t\tdon't try to infer the determinism, just report an error.\n"),
	io__write_string("\t--type-inference-iteration-limit <n>\n"),
	io__write_string("\t\tPerform at most <n> passes of type inference (default: 60).\n"),
	io__write_string("\t--mode-inference-iteration-limit <n>\n"),
	io__write_string("\t\tPerform at most <n> passes of mode inference (default: 30).\n").


:- pred options_help_termination(io__state::di, io__state::uo) is det.

options_help_termination -->
	io__write_string("\nTermination Analysis Options:\n"),
	io__write_string("\t--enable-term, --enable-termination\n"),
	io__write_string("\t\tAnalyse each predicate to discover if it terminates.\n"),
	io__write_string("\t--chk-term, --check-term, --check-termination\n"),
	io__write_string("\t\tEnable termination analysis, and emit warnings for some\n"),
	io__write_string("\t\tpredicates or functions that cannot be proved to terminate.  In\n"),
	io__write_string("\t\tmany cases where the compiler is unable to prove termination\n"),
	io__write_string("\t\tthe problem is either a lack of information about the\n"),
	io__write_string("\t\ttermination properties of other predicates, or because language\n"),
	io__write_string("\t\tconstructs (such as higher order calls) were used which could\n"),
	io__write_string("\t\tnot be analysed.  In these cases the compiler does not emit a\n"),
	io__write_string("\t\twarning of non-termination, as it is likely to be spurious.\n"),
	io__write_string("\t--verb-chk-term, --verb-check-term, --verbose-check-termination\n"),
	io__write_string("\t\tEnable termination analysis, and emit warnings for all\n"),
	io__write_string("\t\tpredicates or functions that cannot be proved to terminate.\n"),
	io__write_string("\t--term-single-arg <n>, --termination-single-argument-analysis <n>\n"),
	io__write_string("\t\tWhen performing termination analysis, try analyzing\n"),
	io__write_string("\t\trecursion on single arguments in strongly connected\n"),
	io__write_string("\t\tcomponents of the call graph that have up to <n> procedures.\n"),
	io__write_string("\t\tSetting this limit to zero disables single argument analysis.\n"),
	io__write_string("\t--termination-norm {simple, total, num-data-elems}\n"),
	io__write_string("\t\tThe norm defines how termination analysis measures the size\n"),
	io__write_string("\t\tof a memory cell. The `simple' norm says that size is always\n"),
	io__write_string("\t\tone.  The `total' norm says that it is the number of words\n"),
	io__write_string("\t\tin the cell.  The `num-data-elems' norm says that it is the\n"),
	io__write_string("\t\tnumber of words in the cell that contain something other\n"),
	io__write_string("\t\tthan pointers to cells of the same type.\n"),
	io__write_string("\t--term-err-limit <n>, --termination-error-limit <n>\n"),
	io__write_string("\t\tPrint at most <n> reasons for any single termination error\n"),
	io__write_string("\t\t(default: 3).\n"),
	io__write_string("\t--term-path-limit <n>, --termination-path-limit <n>\n"),
	io__write_string("\t\tPerform termination analysis only on predicates\n"),
	io__write_string("\t\twith at most <n> paths (default: 256).\n").


:- pred options_help_compilation_model(io__state::di, io__state::uo) is det.

options_help_compilation_model -->
	io__write_string("\nCompilation model options:\n"),
	io__write_string("\tThe following compilation options affect the generated\n"),
	io__write_string("\tcode in such a way that the entire program must be\n"),
	io__write_string("\tcompiled with the same setting of these options,\n"),
	io__write_string("\tand it must be linked to a version of the Mercury\n"),
	io__write_string("\tlibrary which has been compiled with the same setting.\n"),
	io__write_string("\t-s <grade>, --grade <grade>\n"),
	io__write_string("\t\tSelect the compilation model. The <grade> should be one of\n"),
	io__write_string("\t\t`none', `reg', `jump', `asm_jump', `fast', `asm_fast',\n"),
	io__write_string("\t\tor one of those with `.gc', `.prof', `.proftime',\n"),
	io__write_string("\t\t`.profcalls', `.tr', `.sa', `.debug', and/or `.pic_reg'\n"),
	io__write_string("\t\tappended (in that order).\n"),
	io__write_string("\t\tDepending on your particular installation, only a subset\n"),
	io__write_string("\t\tof these possible grades will have been installed.\n"),
	io__write_string("\t\tAttempting to use a grade which has not been installed\n"),
	io__write_string("\t\twill result in an error at link time.\n"),
	io__write_string("\t--gcc-global-registers\t"),
	io__write_string("\t(grades: reg, fast, asm_fast)\n"),
	io__write_string("\t--no-gcc-global-registers"),
	io__write_string("\t(grades: none, jump, asm_jump)\n"),
	io__write_string("\t\tSpecify whether or not to use GNU C's\n"),
	io__write_string("\t\tglobal register variables extension.\n"),
	io__write_string("\t--gcc-non-local-gotos\t"),
	io__write_string("\t(grades: jump, fast, asm_jump, asm_fast)\n"),
	io__write_string("\t--no-gcc-non-local-gotos"),
	io__write_string("\t(grades: none, reg)\n"),
	io__write_string("\t\tSpecify whether or not to use GNU C's\n"),
	io__write_string("\t\t""labels as values"" extension.\n"),
	io__write_string("\t--asm-labels\t\t"),
	io__write_string("\t(grades: asm_jump, asm_fast)\n"),
	io__write_string("\t--no-asm-labels\t\t"),
	io__write_string("\t(grades: none, reg, jump, fast)\n"),
	io__write_string("\t\tSpecify whether or not to use GNU C's\n"),
	io__write_string("\t\tasm extensions for inline assembler labels.\n"),
	io__write_string("\t--gc {none, conservative, accurate}\n"),
	io__write_string("\t--garbage-collection {none, conservative, accurate}\n"),
	io__write_string("\t\t\t\t\t(`.gc' grades use `--gc conservative',\n"),
	io__write_string("\t\t\t\t\tother grades use `--gc none'.)\n"),
	io__write_string("\t\tSpecify which method of garbage collection to use\n"),
	io__write_string("\t\t(default: conservative).  `accurate' GC is not yet implemented.\n"),
	io__write_string("\t--use-trail\t\t"),
	io__write_string("\t(grade modifier: `.tr')\n"),
	io__write_string("\t\tEnable use of a trail.\n"),
	io__write_string("\t\tThis is necessary for interfacing with constraint solvers,\n"),
	io__write_string("\t\tor for backtrackable destructive update.\n"),
	io__write_string("\t-p, --profiling, --time-profiling\n"),
	io__write_string("\t\t\t\t\t(grade modifier: `.prof')\n"),
	io__write_string("\t\tEnable time and call profiling.  Insert profiling hooks in the\n"),
	io__write_string("\t\tgenerated code, and also output some profiling\n"),
	io__write_string("\t\tinformation (the static call graph) to the file\n"),
	io__write_string("\t\t`<module>.prof'.\n"),
	io__write_string("\t--memory-profiling\t"),
	io__write_string("\t(grade modifier: `.memprof')\n"),
	io__write_string("\t\tEnable memory and call profiling.\n"),
/*****************
XXX The following options are not documented,
because they are currently not useful.
The idea was for you to be able to use --profile-calls
and --profile-time seperately, but that doesn't work
because compiling with --profile-time instead of
--profile-calls results in different code addresses, 
so you can't combine the data from versions of
your program compiled with different options.

	io__write_string("\t--profile-calls\t\t"),
	io__write_string("\t(grade modifier: `.profcalls')\n"),
	io__write_string("\t\tSimilar to `--profiling', except that only gathers\n"),
	io__write_string("\t\tcall counts, not timing information.\n"),
	io__write_string("\t\tUseful on systems where time profiling is not supported,\n"),
	io__write_string("\t\tbut not as useful as `--memory-profiling'.\n"),
	io__write_string("\t--profile-time\t\t"),
	io__write_string("\t(grade modifier: `.proftime')\n"),
	io__write_string("\t\tSimilar to `--profiling', except that it only gathers\n"),
	io__write_string("\t\ttiming information, not call counts.\n"),
	io__write_string("\t--profile-memory\t\t"),
	io__write_string("\t(grade modifier: `.profmem')\n"),
	io__write_string("\t\tSimilar to `--memory-profiling', except that it only gathers\n"),
	io__write_string("\t\tmemory usage information, not call counts.\n"),
********************/
	io__write_string("\t--debug\t\t\t"),
	io__write_string("\t(grade modifier: `.debug')\n"),
	io__write_string("\t\tEnable Mercury-level debugging.\n"),
	io__write_string("\t\tSee the [XXX not yet written!] chapter of the\n"),
	io__write_string("\t\tMercury User's Guide for details.\n"),
	io__write_string("\t--pic-reg\t\t"),
	io__write_string("\t(grade modifier: `.pic_reg')\n"),
	io__write_string("\t[For Unix with intel x86 architecture only]\n"),
	io__write_string("\t\tSelect a register usage convention that is compatible,\n"),
	io__write_string("\t\twith position-independent code (gcc's `-fpic' option).\n"),
	io__write_string("\t\tThis is necessary when using shared libraries on Intel x86\n"),
	io__write_string("\t\tsystems running Unix.  On other systems it has no effect.\n"),
	io__write_string("\t--tags {none, low, high}"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tSpecify whether to use the low bits or the high bits of \n"),
	io__write_string("\t\teach word as tag bits (default: low).\n"),
	% io__write_string("\t\t`--tags none' implies `--num-tag-bits 0'.\n"),
	io__write_string("\t--num-tag-bits <n>\t"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tUse <n> tag bits.\n"),

		% The --conf-low-tag-bits option is reserved for use
		% by the `mmc' script; it is deliberately not documented.

		% The --bits-per-word option is intended for use
		% by the `mmc' script; it is deliberately not documented.

		% The --bytes-per-word option is intended for use
		% by the `mmc' script; it is deliberately not documented.

	io__write_string("\t--branch-delay-slot\t"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tAssume that branch instructions have a delay slot.\n"),
	io__write_string("\t--num-real-r-regs <n>\t"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tAssume registers r1 up to r<n> are real general purpose\n"),
	io__write_string("\t\tregisters.\n"),
	io__write_string("\t--num-real-f-regs <n>\t"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tAssume registers f1 up to f<n> are real floating point\n"),
	io__write_string("\t\tregisters.\n"),
	io__write_string("\t--num-real-r-temps <n>\t"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tAssume that <n> non-float temporaries will fit into\n"),
	io__write_string("\t\treal machine registers.\n"),
	io__write_string("\t--num-real-f-temps <n>\t"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tAssume that <n> float temporaries will fit into\n"),
	io__write_string("\t\treal machine registers.\n"),
	io__write_string("\t--args {simple, compact}\n"),
	io__write_string("\t--arg-convention {simple, compact}\n"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tUse the specified argument passing convention\n"),
	io__write_string("\t\tin the generated low-level C code. With the `simple'\n"),
	io__write_string("\t\tconvention, the <n>th argument is passed in or out\n"),
	io__write_string("\t\tusing register r<n>. With the `compact' convention,\n"),
	io__write_string("\t\tthe <n>th input argument is passed using register r<n>,\n"),
	io__write_string("\t\tand the <n>th output argument is returned using\n"),
	io__write_string("\t\tregister r<n>. The compact convention generally leads to\n"),
	io__write_string("\t\tmore efficient code. Use of the simple convention requires the\n"),
	io__write_string("\t\tC code to be compiled with -UCOMPACT_ARGS.\n"),

	io__write_string("\t--no-type-layout\n"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tDon't output base_type_layout structures or references\n"),
	io__write_string("\t\tto them. (The C code also needs to be compiled with\n"),
	io__write_string("\t\t`-DNO_TYPE_LAYOUT').\n"),
	
		% This is a developer only option.
%	io__write_string("\t--basic-stack-layout\n"),
%	io__write_string("\t(This option is not for general use.)\n"),
%	io__write_string("\t\tGenerate the simple stack_layout structures required\n"),
%	io__write_string("\t\tfor stack traces.\n"),

		% This is a developer only option.
%	io__write_string("\t--agc-stack-layout\n"),
%	io__write_string("\t(This option is not for general use.)\n"),
%	io__write_string("\t\tGenerate the stack_layout structures required for\n"),
%	io__write_string("\t\taccurate garbage collection.\n"),
%
		% This is a developer only option.
%	io__write_string("\t--procid-stack-layout\n"),
%	io__write_string("\t(This option is not for general use.)\n"),
%	io__write_string("\t\tGenerate the stack_layout structures required for\n"),
%	io__write_string("\t\tlooking up procedure identification information.\n"),

		% This is a developer only option.
%	io__write_string("\t--trace-stack-layout\n"),
%	io__write_string("\t(This option is not for general use.)\n"),
%	io__write_string("\t\tGenerate the stack_layout structures required for\n"),
%	io__write_string("\t\texecution tracing.\n"),

		% This is a developer only option.
%	io__write_string("\t--typeinfo-liveness\n"),
%	io__write_string("\t(This option is not for general use.)\n"),
%	io__write_string("\t\tUse an alternate technique for calculating liveness.\n"),
%	io__write_string("\t\tKeeps typeinfo variables around for as long as any data\n"),
%	io__write_string("\t\tthat has a type that contains that type variable is live\n"),
%
	io__write_string("\t--unboxed-float\n"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tDon't box floating point numbers.\n"),
	io__write_string("\t\tThis assumes that a Mercury float will fit in a word.\n"),
	io__write_string("\t\tThe C code needs to be compiled with `-UBOXED_FLOAT'.\n"),
	io__write_string("\t\tIt may also need to be compiled with\n"),
	io__write_string("\t\t`-DUSE_SINGLE_PREC_FLOAT', if double precision\n"),
	io__write_string("\t\tfloats don't fit into a word.\n").

:- pred options_help_code_generation(io__state::di, io__state::uo) is det.

options_help_code_generation -->
	io__write_string("\nCode generation options:\n"),
	io__write_string("\t--low-level-debug\n"),
	io__write_string("\t\tEnables various low-level debugging stuff, that was in\n"),
	io__write_string("\t\tthe distant past used to debug the low-level code generation.\n"),
	io__write_string("\t\tYou don't want to use this option unless you are hacking\n"),
	io__write_string("\t\tthe Mercury compiler itself (and probably not even then).\n"),
	io__write_string("\t\tCauses the generated code to become VERY big and VERY\n"),
	io__write_string("\t\tinefficient.  Slows down compilation a LOT.\n"),

	io__write_string("\t--no-trad-passes\n"),
	io__write_string("\t\tThe default `--trad-passes' completely processes each predicate\n"),
	io__write_string("\t\tbefore going on to the next predicate.\n"),
	io__write_string("\t\tThis option tells the compiler\n"),
	io__write_string("\t\tto complete each phase of code generation on all predicates\n"),
	io__write_string("\t\tbefore going on the next phase on all predicates.\n"),
	% io__write_string("\t--no-polymorphism\n"),
	% io__write_string("\t\tDon't handle polymorphic types.\n"),
	% io__write_string("\t\t(Generates slightly more efficient code, but stops\n"),
	% io__write_string("\t\tpolymorphism from working except in special cases.)\n"),
	io__write_string("\t--no-reclaim-heap-on-nondet-failure\n"),
	io__write_string("\t\tDon't reclaim heap on backtracking in nondet code.\n"),
	io__write_string("\t--no-reclaim-heap-on-semidet-failure\n"),
	io__write_string("\t\tDon't reclaim heap on backtracking in semidet code.\n"),
	io__write_string("\t--no-reclaim-heap-on-failure\n"),
	io__write_string("\t\tCombines the effect of the two options above.\n"),
	io__write_string("\t--cc <compiler-name>\n"),
	io__write_string("\t\tSpecify which C compiler to use.\n"),
	io__write_string("\t--c-include-directory <dir>\n"),
	io__write_string("\t\tSpecify the directory containing the Mercury C header files.\n"),
	io__write_string("\t--cflags <options>\n"),
	io__write_string("\t\tSpecify options to be passed to the C compiler.\n"),
		% The --cflags-for-regs and --cflags-for-gotos options
		% are reserved for use by the `mmc' script;
		% they are deliberately not documented.

	io__write_string("\t--c-debug\n"),
	io__write_string("\t\tEnable debugging of the generated C code.\n"),
	io__write_string("\t\t(This has the same effect as `--cflags -g'.)\n"),

	io__write_string("\t--fact-table-max-array-size <n>\n"),
	io__write_string("\t\tSpecify the maximum number of elements in a single\n"),
	io__write_string("\t\t`pragma fact_table' data array (default: 1024).\n"),
	io__write_string("\t--fact-table-hash-percent-full <percentage>\n"),
	io__write_string("\t\tSpecify how full the `pragma fact_table' hash tables should be\n"),
	io__write_string("\t\tallowed to get.  Given as an integer percentage\n"),
	io__write_string("\t\t(valid range: 1 to 100, default: 90).\n").

:- pred options_help_optimization(io__state::di, io__state::uo) is det.

options_help_optimization -->
	io__write_string("\nOptimization Options:\n"),
	io__write_string("\t-O <n>, --opt-level <n>, --optimization-level <n>\n"),
	io__write_string("\t\tSet optimization level to <n>.\n"),
	io__write_string("\t\tOptimization level -1 means no optimization\n"),
	io__write_string("\t\twhile optimization level 6 means full optimization.\n"),
	% io__write_string("\t\tFor a full description of each optimization level,\n"),
	% io__write_string("\t\tsee the Mercury User's Guide.\n"),
	io__write_string("\t--opt-space, --optimize-space\n"),
	io__write_string("\t\tTurn on optimizations that reduce code size\n"),
	io__write_string("\t\tand turn off optimizations that significantly\n"),
	io__write_string("\t\tincrease code size.\n"),
	io__write_string("\t--intermodule-optimization\n"),
	io__write_string("\t\tPerform inlining and higher-order specialization of\n"),
	io__write_string("\t\tthe code for predicates imported from other modules.\n"),
	io__write_string("\t\tThis option must be set throughout the compilation process.\n"),
	io__write_string("\t--trans-intermod-opt\n"),
	io__write_string("\t--transitive-intermodule-optimization\n"),
	io__write_string("\t\tImport the transitive intermodule optimization data.\n"),
	io__write_string("\t\tThis data is imported from `<module>.trans_opt' files.\n"),
	io__write_string("\t--split-c-files\n"),
	io__write_string("\t\tGenerate each C function in its own C file,\n"),
	io__write_string("\t\tso that the linker will optimize away unused code.\n"),
	io__write_string("\t\tThis option significantly increases compilation time,\n"),
	io__write_string("\t\tlink time, and intermediate disk space requirements,\n"),
	io__write_string("\t\tbut in return reduces the size of the final\n"),
	io__write_string("\t\texecutable, typically by about 10-20%.\n"),
	io__write_string("\t\tThis option is only useful with `--procs-per-c-function 1'.\n").

:- pred options_help_hlds_hlds_optimization(io__state::di, io__state::uo)
	is det.

options_help_hlds_hlds_optimization -->
	io__write_string("\n    High-level (HLDS -> HLDS) optimizations:\n"),
	io__write_string("\t--no-inlining\n"),
	io__write_string("\t\tDisable all forms of inlining.\n"),
	io__write_string("\t--no-inline-simple\n"),
	io__write_string("\t\tDisable the inlining of simple procedures.\n"),
	io__write_string("\t--no-inline-single-use\n"),
	io__write_string("\t\tDisable the inlining of procedures called only once.\n"),
	io__write_string("\t--inline-compound-threshold <threshold>\n"),
	io__write_string("\t\tInline a procedure if its size (measured roughly\n"),
	io__write_string("\t\tin terms of the number of connectives in its internal form),\n"),
	io__write_string("\t\tmultiplied by the number of times it is called,\n"),
	io__write_string("\t\tis below the given threshold.\n"),
	io__write_string("\t--inline-simple-threshold <threshold>\n"),
	io__write_string("\t\tInline a procedure if its size is less than the\n"),
	io__write_string("\t\tgiven threshold.\n"),
	io__write_string("\t--intermod-inline-simple-threshold\n"),
	io__write_string("\t\tSimilar to `--inline-simple-threshold', except used to\n"),
	io__write_string("\t\tdetermine which predicates should be included in\n"),
	io__write_string("\t\t`.opt' files. Note that changing this between writing\n"),
	io__write_string("\t\tthe `.opt' file and compiling to C may cause link errors,\n"),
	io__write_string("\t\tand too high a value may result in reduced performance.\n"),
	io__write_string("\t--no-common-struct\n"),
	io__write_string("\t\tDisable optimization of common term structures.\n"),
	io__write_string("\t--no-common-goal\n"),
	io__write_string("\t\tDisable optimization of common goals.\n"),
	io__write_string("\t\tAt the moment this optimization\n"),
	io__write_string("\t\tdetects only common deconstruction unifications.\n"),
	io__write_string("\t\tDisabling this optimization reduces the class of predicates\n"),
	io__write_string("\t\tthat the compiler considers to be deterministic.\n"),
	% io__write_string("\t--constraint-propagation\n"),
	% io__write_string("\t\tEnable the C-tranformation.  (Doesn't work.)\n"),
	io__write_string("\t--prev-code\n"),
	io__write_string("\t\tMigrate into the start of branched goals.\n"),
	io__write_string("\t--no-follow-code\n"),
	io__write_string("\t\tDon't migrate into the end of branched goals.\n"),
	io__write_string("\t--excess-assign\n"),
	io__write_string("\t\tRemove excess assignment unifications.\n"),
	io__write_string("\t--optimize-duplicate-calls\n"),
	io__write_string("\t\tOptimize away multiple calls to a predicate\n"),
	io__write_string("\t\twith the same input arguments.\n"),
	io__write_string("\t--optimize-saved-vars\n"),
	io__write_string("\t\tReorder goals to minimize the number of variables\n"),
	io__write_string("\t\tthat have to be saved across calls.\n"),
	io__write_string("\t--optimize-unused-args\n"),
	io__write_string("\t\tRemove unused predicate arguments.\n"),
	io__write_string("\t\tThis will cause the compiler to generate more\n"),
	io__write_string("\t\tefficient code for many polymorphic predicates.\n"),
	io__write_string("\t--intermod-unused-args\n"),
	io__write_string("\t\tPerform unused argument removal across module boundaries.\n"),
	io__write_string("\t\tThis option implies `--optimize-unused-args' and\n"),
	io__write_string("\t\t`--intermodule-optimization'.\n"),

	io__write_string("\t--optimize-higher-order\n"),
	io__write_string("\t\tEnable specialization higher-order predicates.\n"),
	io__write_string("\t--optimize-constructor-last-call\n"),
	io__write_string("\t\tEnable the optimization of ""last"" calls that are followed by\n"),
	io__write_string("\t\tconstructor application.\n"),
	io__write_string("\t--deforestation\n"),
	io__write_string("\t\tEnable deforestation. Deforestation is a program\n"),
	io__write_string("\t\ttransformation whose aim is to avoid the construction of\n"),
	io__write_string("\t\tintermediate data structures and to avoid repeated traversals\n"),
	io__write_string("\t\tover data structures within a conjunction.\n"),
	io__write_string("\t--deforestation-depth-limit\n"),
	io__write_string("\t\tSpecify a depth limit for the deforestation algorithm\n"),
	io__write_string("\t\tin addition to the usual termination checks.\n"),
	io__write_string("\t\tA value of -1 specifies no depth limit. The default is 4.\n"),
	io__write_string("\t--deforestation-vars-threshold\n"),
	io__write_string("\t\tSpecify a rough limit on the number of variables\n"),
	io__write_string("\t\tin a procedure created by deforestation.\n"),
	io__write_string("\t\tA value of -1 specifies no limit. The default is 200.\n").
	 
:- pred options_help_hlds_llds_optimization(io__state::di, io__state::uo) is det.

options_help_hlds_llds_optimization -->
	io__write_string("\n    Medium-level (HLDS -> LLDS) optimizations:\n"),
	io__write_string("\t--no-smart-indexing\n"),
	io__write_string("\t\tGenerate switches as a simple if-then-else chains;\n"),
	io__write_string("\t\tdisable string hashing and integer table-lookup indexing.\n"),
	io__write_string("\t--dense-switch-req-density <percentage>\n"),
	io__write_string("\t\tThe jump table generated for an atomic switch\n"),
	io__write_string("\t\tmust have at least this percentage of full slots (default: 25).\n"),
	io__write_string("\t--lookup-switch-req-density <percentage>\n"),
	io__write_string("\t\tThe jump table generated for an atomic switch\n"),
	io__write_string("\t\tin which all the outputs are constant terms\n"),
	io__write_string("\t\tmust have at least this percentage of full slots (default: 25).\n"),
	io__write_string("\t--dense-switch-size <n>\n"),
	io__write_string("\t\tThe jump table generated for an atomic switch\n"),
	io__write_string("\t\tmust have at least this many entries (default: 4).\n"),
	io__write_string("\t--lookup-switch-size <n>\n"),
	io__write_string("\t\tThe lookup table generated for an atomic switch\n"),
	io__write_string("\t\tmust have at least this many entries (default: 4).\n"),
	io__write_string("\t--string-switch-size <n>\n"),
	io__write_string("\t\tThe hash table generated for a string switch\n"),
	io__write_string("\t\tmust have at least this many entries (default: 8).\n"),
	io__write_string("\t--tag-switch-size <n>\n"),
	io__write_string("\t\tThe number of alternatives in a tag switch\n"),
	io__write_string("\t\tmust be at least this number (default: 3).\n"),
	io__write_string("\t--try-switch-size <n>\n"),
	io__write_string("\t\tThe number of alternatives in a try/retry chain switch\n"),
	io__write_string("\t\tmust be at least this number (default: 3).\n"),
	io__write_string("\t--binary-switch-size <n>\n"),
	io__write_string("\t\tThe number of alternatives in a binary search switch\n"),
	io__write_string("\t\tmust be at least this number (default: 4).\n"),
	io__write_string("\t--no-static-ground-terms\n"),
	io__write_string("\t\tDisable the optimization of constructing constant ground terms\n"),
	io__write_string("\t\tat compile time and storing them as static constants.\n"),
	io__write_string("\t--no-middle-rec\n"),
	io__write_string("\t\tDisable the middle recursion optimization.\n"),
	io__write_string("\t--no-simple-neg\n"),
	io__write_string("\t\tDon't generate simplified code for simple negations.\n"),
	io__write_string("\t--no-follow-vars\n"),
	io__write_string("\t\tDon't optimize the assignment of registers in branched goals.\n").

:- pred options_help_llds_llds_optimization(io__state::di, io__state::uo) is det.

options_help_llds_llds_optimization -->
	io__write_string("\n    Low-level (LLDS -> LLDS) optimizations:\n"),
	io__write_string("\t--no-llds-optimize\n"),
	io__write_string("\t\tDisable the low-level optimization passes.\n"),
	io__write_string("\t--optimize-dead-procs\n"),
	io__write_string("\t\tEnable dead predicate elimination.\n"),
	io__write_string("\t--no-optimize-peep\n"),
	io__write_string("\t\tDisable local peephole optimizations.\n"),
	io__write_string("\t--no-optimize-jumps\n"),
	io__write_string("\t\tDisable elimination of jumps to jumps.\n"),
	io__write_string("\t--no-optimize-fulljumps\n"),
	io__write_string("\t\tDisable elimination of jumps to ordinary code.\n"),
	io__write_string("\t--no-optimize-labels\n"),
	io__write_string("\t\tDisable elimination of dead labels and code.\n"),
	io__write_string("\t--optimize-dups\n"),
	io__write_string("\t\tEnable elimination of duplicate code.\n"),
%%%	io__write_string("\t--optimize-copyprop\n"),
%%%	io__write_string("\t\tEnable the copy propagation optimization.\n"),
	io__write_string("\t--optimize-value-number\n"),
	io__write_string("\t\tPerform value numbering on extended basic blocks.\n"),
	io__write_string("\t--no-optimize-frames\n"),
	io__write_string("\t\tDisable stack frame optimizations.\n"),
	io__write_string("\t--no-optimize-delay-slot\n"),
	io__write_string("\t\tDisable branch delay slot optimizations.\n"),
	io__write_string("\t--optimize-repeat <n>\n"),
	io__write_string("\t\tIterate most optimizations at most <n> times (default: 3).\n"),
	io__write_string("\t--optimize-vnrepeat <n>\n"),
	io__write_string("\t\tIterate value numbering at most <n> times (default: 1).\n"),
	io__write_string("\t--pred-value-number\n"),
	io__write_string("\t\tExtend value numbering to entire predicates.\n").

:- pred options_help_output_optimization(io__state::di, io__state::uo) is det.

options_help_output_optimization -->
	io__write_string("\n    Output-level (LLDS -> C) optimizations:\n"),
	io__write_string("\t--use-macro-for-redo-fail\n"),
	io__write_string("\t\tEmit the fail or redo macro instead of a branch\n"),
	io__write_string("\t\tto the fail or redo code in the runtime system.\n"),
	io__write_string("\t\tThis produces slightly bigger but slightly faster code.\n"),
	io__write_string("\t--no-emit-c-loops\n"),
	io__write_string("\t\tUse only gotos, don't emit C loop constructs.\n"),
	io__write_string("\t--procs-per-c-function <n>\n"),
	io__write_string("\t\tPut the code for up to <n> Mercury\n"),
	io__write_string("\t\tprocedures in a single C function.  The default\n"),
	io__write_string("\t\tvalue of <n> is one.  Increasing <n> can produce\n"),
	io__write_string("\t\tslightly more efficient code, but makes compilation slower.\n"),
	io__write_string("\t--everything-in-one-c-function\n"),
	io__write_string("\t\tThis option has the effect of putting the code for all\n"),
	io__write_string("\t\tthe Mercury procedures in a single C function,\n"),
	io__write_string("\t\twhich produces the most efficient code but tends to\n"),
	io__write_string("\t\tseverely stress the C compiler on large modules.\n").

:- pred options_help_object_optimization(io__state::di, io__state::uo) is det.

options_help_object_optimization -->
	io__write_string("\n    Object-level (C -> object code) optimizations:\n"),
	io__write_string("\t\tNote that if you are using Mmake, you need to pass these\n"),
	io__write_string("\t\toptions to `mgnuc' rather than to `mmc'.\n"),
	io__write_string("\t--no-c-optimize\n"),
	io__write_string("\t\tDon't enable the C compiler's optimizations.\n"),
	io__write_string("\t--inline-alloc\n"),
	io__write_string("\t\tInline calls to GC_malloc().\n"),
	io__write_string("\t\tThis can improve performance a fair bit,\n"),
	io__write_string("\t\tbut may significantly increase code size.\n"),
	io__write_string("\t\tThis option has no effect if `--gc conservative'\n"),
	io__write_string("\t\tis not set or if the C compiler is not GNU C.\n").

:- pred options_help_link(io__state::di, io__state::uo) is det.

options_help_link -->
	io__write_string("\nLink Options:\n"),
	io__write_string("\t-o <filename>, --output-file <filename>\n"),
	io__write_string("\t\tSpecify the name of the final executable.\n"),
	io__write_string("\t\t(The default executable name is the same as the name\n"),
	io__write_string("\t\tof the first module on the command line.)\n"),
	io__write_string("\t--link-flags <options>\n"),
	io__write_string("\t\tSpecify options to be passed to the linker.\n"),
	io__write_string("\t-L <directory>, --library-directory <directory>\n"),
	io__write_string("\t\tAppend <directory> to the list of directories in which\n"),
	io__write_string("\t\tto search for libraries.\n"),
	io__write_string("\t-l <library>, --library <library>\n"),
	io__write_string("\t\tLink with the specified library.\n"),
	io__write_string("\t--link-object <object-file>\n"),
	io__write_string("\t\tLink with the specified object file.\n").

:- pred options_help_misc(io__state::di, io__state::uo) is det.

options_help_misc -->
	io__write_string("\nMiscellaneous Options:\n"),
	% io__write_string("\t-H <n>, --heap-space <n>\n"),
	% io__write_string("\t\tPre-allocate <n> kilobytes of heap space.\n"),
	% io__write_string("\t\tThis option is now obsolete.  In the past it\n"),
	% io__write_string("\t\twas used to avoid NU-Prolog's\n"),
	% io__write_string("\t\t\t""Panic: growing stacks has required shifting the heap""\n"),
	% io__write_string("\t\tmessage.\n"),

	io__write_string("\t-I <dir>, --search-directory <dir>\n"),
	io__write_string("\t\tAdd <dir> to the list of directories to be searched for \n\t\timported modules.\n"),
	io__write_string("\t--intermod-directory <dir>\n"),
	io__write_string("\t\tAdd <dir> to the list of directories to be\n"),
	io__write_string("\t\tsearched for `.opt' files.\n"),
	io__write_string("\t--no-use-search-directories-for-intermod\n"),
	io__write_string("\t\tDon't add arguments to `--search-directory' to the list\n"),
	io__write_string("\t\tof directories to search for `.opt' files - use only the\n"),
	io__write_string("\t\tdirectories given by `--intermod-directory'.\n"),
	io__write_string("\t--use-subdirs\n"),
	io__write_string("\t\tGenerate intermediate files in a `Mercury' subdirectory,\n"),
	io__write_string("\t\trather than generating them in the current directory.\n").

:- end_module options.

%-----------------------------------------------------------------------------%
