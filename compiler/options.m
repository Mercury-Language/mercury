%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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
:- import_module bool, int, string, std_util, list, io, map, require, getopt.

:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_defaults(option::out, option_data::out) is nondet.
:- pred special_handler(option::in, special_data::in, option_table::in,
	maybe_option_table::out) is semidet.

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
		;	warn_missing_det_decls
		;	warn_det_decls_too_lax
		;	warn_nothing_exported
		;	warn_unused_args
	% Verbosity options
		;	verbose
		;	very_verbose
		;	verbose_errors
		;	statistics
		;	debug_types
		;	debug_modes
		;	debug_detism
		;	debug_opt
		;	debug_vn
	% Output options
		;	make_interface
		;	generate_dependencies
		;	convert_to_mercury
		;	convert_to_goedel
		;	typecheck_only
		;	errorcheck_only
		;	compile_to_c
		;	compile_only
	% Auxiliary output options
		;	line_numbers
		;	auto_comments
		;	show_dependency_graph
		;	generate_bytecode
		;	dump_hlds
		;	verbose_dump_hlds
	% Language semantics options
		;	reorder_conj
		;	reorder_disj
		;	fully_strict
		;	strict_sequential
		;	infer_types
		;	infer_modes
		;	infer_determinism
		;	infer_all
	% Compilation Model options
		;	grade
		;	gcc_non_local_gotos
		;	gcc_global_registers
		;	asm_labels
		;	gc
		;	profiling
		;	constraints
		;	debug
		;	debug_data
		;	tags
		;	num_tag_bits
		;	bits_per_word
		;	bytes_per_word
		;	conf_low_tag_bits
				% The undocumented conf_low_tag_bits option
				% is used by the `mc' script to pass the
				% default value for num_tag_bits
				% assuming --tags low.
				% The reason that `mc' doesn't just
				% pass a default value for --num-tag-bits
				% is that we want to be able to give an
				% error message if the user specifies
				% `--tags high' and doesn't specify
				% `--num-tag-bits'.
		;	args
		;	highlevel_c
		;	unboxed_float
	% Code generation options
		;	trad_passes
		;	polymorphism
		;	reclaim_heap_on_failure
		;	reclaim_heap_on_semidet_failure
		;	reclaim_heap_on_nondet_failure
		;	lazy_code
		;	use_macro_for_redo_fail
		;	have_delay_slot
		;	num_real_r_regs
		;	num_real_temps
		;	cc
		;	cflags
		;	cflags_for_regs
		;	cflags_for_gotos
		;	c_include_directory
		;	aditi
	% Optimization Options
		;	opt_level
		;	opt_space	% default is to optimize time
	%	- HLDS
		;	inlining
		;	inline_simple
		;	inline_single_use
		;	inline_threshold
		;	common_struct
		;	common_goal
		;	constraint_propagation
		;	optimize_unused_args
		;	optimize_higher_order
		;	excess_assign
		;	follow_code
		;	prev_code
		;	optimize_dead_procs
	%	- HLDS->LLDS
		;	smart_indexing
		;	  dense_switch_req_density
		;	  lookup_switch_req_density
		;	  dense_switch_size
		;	  lookup_switch_size
		;	  string_switch_size
		;	  tag_switch_size
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
	%	- C
		;	emit_c_loops
		;	procs_per_c_function
		;	everything_in_one_c_function
		;	split_c_files
		;	c_optimize
	% Link options
		;	output_file_name
		;	link_flags
		;	link_library_directories
		;	link_libraries
		;	link_objects
	% Miscellaneous Options
		;	builtin_module
		;	heap_space
		;	search_directories
		;	help.

:- implementation.

:- import_module assoc_list.

:- type option_category
	--->	warning_option
	;	verbosity_option
	;	output_option
	;	aux_output_option
	;	language_semantics_option
	;	compilation_model_option
	;	code_gen_option
	;	optimization_option
	;	link_option
	;	miscellaneous_option.

option_defaults(Option, Default) :-
	option_defaults_2(_Category, OptionsList),
	list__member(Option - Default, OptionsList).

:- pred option_defaults_2(option_category, list(pair(option, option_data))).
:- mode option_defaults_2(in, out) is det.
:- mode option_defaults_2(out, out) is multidet.
:- mode option_defaults_2(in(bound(optimization_option)), out) is det.

option_defaults_2(warning_option, [
		% Warning Options
	inhibit_warnings	-	bool(no),
	halt_at_warn		-	bool(no),
	halt_at_syntax_errors	-	bool(no),
	warn_singleton_vars	-	bool(yes),
	warn_overlapping_scopes	-	bool(yes),
	warn_missing_det_decls	-	bool(yes),
	warn_det_decls_too_lax	-	bool(yes),
	warn_nothing_exported	-	bool(yes),
	warn_unused_args	-	bool(no)
]).
option_defaults_2(verbosity_option, [
		% Verbosity Options
	verbose			-	bool(no),
	very_verbose		-	bool(no),
	verbose_errors		-	bool(no),
	statistics		-	bool(no),
	debug_types		- 	bool(no),
	debug_modes		- 	bool(no),
	debug_detism		- 	bool(no),
	debug_opt		- 	bool(no),
	debug_vn		- 	int(0)
]).
option_defaults_2(output_option, [
		% Output Options (mutually exclusive)
	generate_dependencies	-	bool(no),
	make_interface		-	bool(no),
	convert_to_mercury 	-	bool(no),
	convert_to_goedel 	-	bool(no),
	typecheck_only		-	bool(no),
	errorcheck_only		-	bool(no),
	compile_to_c		-	bool(no),
	compile_only		-	bool(no)
]).
option_defaults_2(aux_output_option, [
		% Auxiliary Output Options
	show_dependency_graph	-	bool(no),
	generate_bytecode	-	bool(no),
	line_numbers		-	bool(no),
	auto_comments		-	bool(no),
	dump_hlds		-	accumulating([]),
	verbose_dump_hlds	-	bool(no)
]).
option_defaults_2(language_semantics_option, [
	strict_sequential	-	special,
	reorder_conj		-	bool(yes),
	reorder_disj		-	bool(yes),
	fully_strict		-	bool(yes),
	infer_types		-	bool(yes),
	infer_modes		-	bool(yes),
	infer_determinism	-	bool(yes),
	infer_all		-	bool_special
]).
option_defaults_2(compilation_model_option, [
		% Compilation model options (ones that affect binary
		% compatibility).
	grade			-	string("asm_fast.gc"),
					% the `mc' script will override the
					% above default with a value determined
					% at configuration time
	gcc_non_local_gotos	-	bool(yes),
	gcc_global_registers	-	bool(yes),
	asm_labels		-	bool(yes),
	gc			-	string("conservative"),
	profiling		-	bool(no),
	constraints		-	bool(no),
	debug			-	bool(no),
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
					% the `mc' script will override the
					% above default with a value determined
					% at configuration time
	args			-	string("simple"),
	highlevel_c		-	bool(no),
	unboxed_float		-	bool(no)
]).
option_defaults_2(code_gen_option, [
		% Code Generation Options
	trad_passes		-	bool(yes),
	polymorphism		-	bool(yes),
	lazy_code		-	bool(yes),
	reclaim_heap_on_failure	-	bool_special,
	reclaim_heap_on_semidet_failure	-	bool(yes),
	reclaim_heap_on_nondet_failure	-	bool(yes),
	use_macro_for_redo_fail	-	bool(no),
	have_delay_slot		-	bool(no),
					% the `mc' script may override the
					% above default if configure says
					% the machine has branch delay slots
	num_real_r_regs		-	int(5),
	num_real_temps		-	int(5),
					% the `mc' script will override the
					% above defaults with values determined
					% at configuration time
	cc			-	string("gcc"),
					% the `mc' script will override the
					% above default with a value determined
					% at configuration time
	cflags			-	accumulating([]),
	cflags_for_regs		-	string(""),
	cflags_for_gotos	-	string(""),
					% the `mc' script will override the
					% above two defaults with values
					% determined at configuration time
	c_include_directory	-	string(""),
					% the `mc' script will override the
					% above default with a value determined
					% at configuration time
	aditi			-	bool(no),

		% split_c_files is really an optimization option,
		% but we list it here as a code_gen_option,
		% because we don't want `-O<n>' to turn it off.
	split_c_files		-	bool(no)
]).
option_defaults_2(optimization_option, [
		% Optimization options
		%
		% IMPORTANT: the default here should be all optimizations OFF.
		% Optimizations should be enabled by the appropriate
		% optimization level in the opt_level table.
		%
	opt_level		-	int_special,
	opt_space		-	special,
% HLDS
	inlining		-	bool_special,
	inline_simple		-	bool(no),
	inline_single_use	-	bool(no),
	inline_threshold	-	int(0),
	common_struct		-	bool(no),
	common_goal		-	bool(yes),
		% commmon_goal is not really an optimization, since
		% it affects the semantics
	constraint_propagation	-	bool(no),
	excess_assign		-	bool(no),
	prev_code		-	bool(no),
	follow_code		-	bool(no),
	optimize_unused_args	-	bool(no),
	optimize_higher_order	-	bool(no),
	optimize_dead_procs	-	bool(no),

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
	optimize_vnrepeat	-	int(0),
	pred_value_number	-	bool(no),

% LLDS -> C
	emit_c_loops		-	bool(no),
	procs_per_c_function	-	int(1),
	everything_in_one_c_function -	special,
	c_optimize		-	bool(no)
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
	builtin_module		-	string("mercury_builtin"),
	heap_space		-	int(0),
	search_directories 	-	accumulating(["."]),
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
long_option("warn-missing-det-decls",	warn_missing_det_decls).
long_option("warn-det-decls-too-lax",	warn_det_decls_too_lax).
long_option("warn-nothing-exported",	warn_nothing_exported).
long_option("warn-unused-args",		warn_unused_args).

% verbosity options
long_option("verbose",			verbose).
long_option("very-verbose",		very_verbose).
long_option("verbose-error-messages",	verbose_errors).
long_option("statistics",		statistics).
long_option("debug-types",		debug_types).
long_option("debug-modes",		debug_modes).
long_option("debug-detism",		debug_detism).
long_option("debug-opt",		debug_opt).
long_option("debug-vn",			debug_vn).

% output options (mutually exclusive)
long_option("generate-dependencies",	generate_dependencies).
long_option("make-interface",		make_interface).
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
long_option("auto-comments",		auto_comments).
long_option("line-numbers",		line_numbers).
long_option("show-dependency-graph",	show_dependency_graph).
long_option("generate-bytecode",	generate_bytecode).
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
long_option("infer-determinism",	infer_determinism).
long_option("infer-det",		infer_determinism).

% compilation model options
long_option("grade",			grade).
long_option("gcc-non-local-gotos",	gcc_non_local_gotos).
long_option("gcc-global-registers",	gcc_global_registers).
long_option("asm-labels",		asm_labels).
long_option("gc",			gc).
long_option("garbage-collection",	gc).
long_option("profiling",		profiling).
long_option("debug",			debug).
long_option("tags",			tags).
long_option("num-tag-bits",		num_tag_bits).
long_option("bits-per-word",		bits_per_word).
long_option("bytes-per-word",		bytes_per_word).
long_option("conf-low-tag-bits",	conf_low_tag_bits).
long_option("args",			args).
long_option("arg-convention",		args).
long_option("highlevel-C",		highlevel_c).
long_option("highlevel-c",		highlevel_c).
long_option("high-level-C",		highlevel_c).
long_option("high-level-c",		highlevel_c).
long_option("single-prec-float",	unboxed_float).
long_option("unboxed-float",		unboxed_float).

% code generation options
long_option("polymorphism",		polymorphism).
long_option("trad-passes",		trad_passes).
long_option("lazy-code",		lazy_code).
long_option("reclaim-heap-on-failure",	reclaim_heap_on_failure).
long_option("reclaim-heap-on-semidet-failure",
					reclaim_heap_on_semidet_failure).
long_option("reclaim-heap-on-nondet-failure",
					reclaim_heap_on_nondet_failure).
long_option("use-macro-for-redo-fail",	use_macro_for_redo_fail).
long_option("branch-delay-slot",	have_delay_slot).
long_option("have-delay-slot",		have_delay_slot).
long_option("num-real-r-regs",		num_real_r_regs).
long_option("num-real-temps",		num_real_temps).
long_option("cc",			cc).
long_option("cflags",			cflags).
long_option("cflags-for-regs",		cflags_for_regs).
long_option("cflags-for-gotos",		cflags_for_gotos).
long_option("c-include-directory",	c_include_directory).

% optimization options

long_option("opt-level",		opt_level).
long_option("optimization-level",	opt_level).
long_option("optimisation-level",	opt_level).
long_option("opt-space",		opt_space).
long_option("optimize-space",		opt_space).
long_option("optimise-space",		opt_space).

% HLDS->HLDS optimizations
long_option("inlining", 		inlining).
long_option("inline-simple",		inline_simple).
long_option("inline-single-use",	inline_single_use).
long_option("inline-threshold",		inline_threshold).
long_option("common-struct",		common_struct).
long_option("common-goal",		common_goal).
long_option("excess-assign",		excess_assign).
long_option("prev-code",		prev_code).
long_option("follow-code",		follow_code).
long_option("constraint-propagation",	constraint_propagation).
long_option("optimize-unused-args",	optimize_unused_args).
long_option("optimise-unused-args",	optimize_unused_args).
long_option("optimize-higher-order",	optimize_higher_order).
long_option("optimise-higher-order",	optimize_higher_order).
long_option("optimize-dead-procs",	optimize_dead_procs).
long_option("optimise-dead-procs",	optimize_dead_procs).

% HLDS->LLDS optimizations
long_option("smart-indexing",		smart_indexing).
long_option("dense-switch-req-density",	dense_switch_req_density).
long_option("lookup-switch-req-density",lookup_switch_req_density).
long_option("dense-switch-size",	dense_switch_size).
long_option("lookup-switch-size",	lookup_switch_size).
long_option("string-switch-size",	string_switch_size).
long_option("tag-switch-size",		tag_switch_size).
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

% LLDS->C optimizations
long_option("emit-c-loops",		emit_c_loops).
long_option("procs-per-c-function",	procs_per_c_function).
long_option("procs-per-C-function",	procs_per_c_function).
long_option("everything-in-one-c-function",	everything_in_one_c_function).
long_option("everything-in-one-C-function",	everything_in_one_c_function).
long_option("split-c-files",		split_c_files).
long_option("split-C-files",		split_c_files).
long_option("c-optimise",		c_optimize).
long_option("c-optimize",		c_optimize).

% link options
long_option("output-file",		output_file_name).
long_option("link-flags",		link_flags).
long_option("library-directory",	link_library_directories).
long_option("library",			link_libraries).
long_option("link-object",		link_objects).

% misc options
long_option("help",			help).
long_option("heap-space",		heap_space).
long_option("builtin-module",		builtin_module).
long_option("search-directory",		search_directories).

%-----------------------------------------------------------------------------%

special_handler(inlining, bool(Value), OptionTable0, ok(OptionTable)) :-
	map__set(OptionTable0, inline_simple, bool(Value), OptionTable1),
	map__set(OptionTable1, inline_single_use, bool(Value), OptionTable2),
	(
		Value = yes,
		map__set(OptionTable2, inline_threshold, int(10), OptionTable)
	;
		Value = no,
		map__set(OptionTable2, inline_threshold, int(0), OptionTable)
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
special_handler(infer_all, bool(Infer), OptionTable0, ok(OptionTable)) :-
	override_options([
			infer_types 		-	bool(Infer),
			infer_modes 		-	bool(Infer),
			infer_determinism 	-	bool(Infer)
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
	optimize_fulljumps	-	bool(no),
	optimize_labels		-	bool(yes),
	optimize_dups		-	bool(yes)
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
	excess_assign		-	bool(yes)	% ???
	% jumps?
	% labels?
]).

% Optimization level 1: apply optimizations which are cheap and
% have a good payoff while still keeping compilation time small

opt_level(1, OptionTable, [
	c_optimize		-	bool(yes),	% XXX we want `gcc -O1'
	optimize_jumps		-	bool(yes),
	optimize_labels		-	bool(yes),
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
	inline_threshold	-	int(0),		% 10 when various
							% bugs are fixed
	common_struct		-	bool(yes),
	simple_neg		-	bool(yes)
]).

% Optimization level 3: apply optimizations which usually have a good
% payoff even if they increase compilation time quite a bit

opt_level(3, _, [
%%%	optimize_copyprop	-	bool(yes),
	optimize_unused_args	-	bool(yes),	
	optimize_higher_order	-	bool(yes),
	optimize_repeat		-	int(4),
	optimize_vnrepeat	-	int(1)
]).

% Optimization level 4: apply optimizations which may have some
% payoff even if they increase compilation time quite a bit

% Currently this just enables value_number

opt_level(4, _, [
	optimize_value_number	-	bool(yes)
]).

% Optimization level 5: apply optimizations which may have some
% payoff even if they increase compilation time a lot

% Currently this enables pred_value_number
% and runs a second pass of value numbering

opt_level(5, _, [
	pred_value_number	-	bool(yes),
	optimize_repeat		-	int(5),
	optimize_vnrepeat	-	int(2)
]).

% Optimization level 6: apply optimizations which may have any
% payoff even if they increase compilation time to completely
% unreasonable levels

% Currently this just sets `everything_in_one_c_function', which causes
% the compiler to put everything in the one C function and treat
% calls to predicates in the same module as local.

opt_level(6, _, [
	procs_per_c_function	-	int(0)	% everything in one C function
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
	options_help_compilation_model,
	options_help_code_generation,
	options_help_optimization,
	options_help_hlds_hlds_optimization,
	options_help_hlds_llds_optimization,
	options_help_llds_llds_optimization,
	options_help_output_optimization,
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
	io__write_string("\t--no-warn-missing-det-decls\n"),
	io__write_string("\t\tDon't warn about predicate declarations which don't\n"),
	io__write_string("\t\thave a determinism annotation.\n"),
	io__write_string("\t--no-warn-det-decls-too-lax\n"),
	io__write_string("\t\tDon't warn about determinism declarations\n"),
	io__write_string("\t\twhich could have been stricter.\n"),
	io__write_string("\t--no-warn-nothing-exported\n"),
	io__write_string("\t\tDon't warn about modules which export nothing.\n"),
	io__write_string("\t--warn-unused-args\n"),
	io__write_string("\t\tWarn about predicate arguments which are not used.\n").

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
	io__write_string("\t\tAt the moment this option implies --no-trad-passes, so you get\n"),
	io__write_string("\t\tinformation at the boundaries between phases of the compiler.\n"),
	io__write_string("\t-T, --debug-types\n"),
	io__write_string("\t\tOutput detailed debugging traces of the type checking.\n"),
	io__write_string("\t-N, --debug-modes\n"),
	io__write_string("\t\tOutput detailed debugging traces of the mode checking.\n"),
	io__write_string("\t--debug-detism\n"),
	io__write_string("\t\tOutput detailed debugging traces of determinism analysis.\n"),
	io__write_string("\t--debug-opt\n"),
	io__write_string("\t\tOutput detailed debugging traces of the optimization process.\n"),
	io__write_string("\t--debug-vn <n>\n"),
	io__write_string("\t\tOutput detailed debugging traces of the value numbering\n"),
	io__write_string("\t\toptimization pass. The different bits in the number\n"),
	io__write_string("\t\targument of this option control the printing of\n"),
	io__write_string("\t\tdifferent types of tracing messages.\n").

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
	io__write_string("\t-i, --make-interface\n"),
	io__write_string("\t\tWrite the module interface to `<module>.int'.\n"),
	io__write_string("\t\tAlso write the short interface to `<module>.int2'.\n"),
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
	io__write_string("\t--generate-bytecode"),
	io__write_string("\t\tOutput a bytecode form of the module for debugging.\n"),
	io__write_string("\t--auto-comments\n"),
	io__write_string("\t\tOutput comments in the `<module>.c' file.\n"),
	io__write_string("\t\t(The code may be easier to understand if you also\n"),
	io__write_string("\t\tuse the `--no-llds-optimize' option.)\n"),
	io__write_string("\t-l, --line-numbers\n"),
	io__write_string("\t\tOutput line numbers in the generated code.\n"),
	io__write_string("\t\tOnly works with the -G and -P options.\n"),
	io__write_string("\t--show-dependency-graph\n"),
	io__write_string("\t\tWrite out the dependency graph to `<module>.dependency_graph'.\n"),
	io__write_string("\t-d <n>, --dump-hlds <stage number or name>\n"),
	io__write_string("\t\tDump the HLDS (intermediate representation) after\n"),
	io__write_string("\t\tthe specified stage to `<module>.hlds_dump.<num>-<name>'.\n"),
	io__write_string("\t\tStage numbers range from 1-19.\n"),
	io__write_string("\t\tMultiple dump options accumulate.\n"),
	io__write_string("\t-D, --verbose-dump-hlds\n"),
	io__write_string("\t\tWith --dump-hlds, dumps some additional info.\n").

:- pred options_help_semantics(io__state::di, io__state::uo) is det.

options_help_semantics -->
	io__write_string("\nLanguage semantics options:\n"),
	io__write_string("(See the Mercury language reference manual for detailed explanations.)\n"),
	io__write_string("\t--no-reorder-conj\n"),
	io__write_string("\t\tExecute conjunctions left-to-right except where the modes imply\n"),
	io__write_string("\t\tthat reordering is unavoidable.\n"),
	io__write_string("\t\tno-reorder-disj\n"),
	io__write_string("\t\tExecute disjunctions strictly left-to-right.\n"),
	io__write_string("\t\tfully-strict\n"),
	io__write_string("\t\tDon't optimize away loops or calls to error/1.\n"),
	io__write_string("\t--no-infer-types\n"),
	io__write_string("\t\tIf there is no type declaration for a predicate or function,\n"),
	io__write_string("\t\tdon't try to infer the type, just report an errror.\n"),

	io__write_string("\t\n").
/****
% The --no-infer-det option has not yet been implemented.
	io__write_string("\t--no-infer-det, --no-infer-determinism\n"),
	io__write_string("\t\tIf there is no determinism declaration for a procedure,\n"),
	io__write_string("\t\tdon't try to infer the determinism, just report an errror.\n").
****/

:- pred options_help_compilation_model(io__state::di, io__state::uo) is det.

options_help_compilation_model -->
	io__write_string("\nCompilation model options:\n"),
	io__write_string("\tThe following compilation options affect the generated\n"),
	io__write_string("\tcode in such a way that the entire program must be\n"),
	io__write_string("\tcompiled with the same setting of these options,\n"),
	io__write_string("\tand it must be linked to a version of the Mercury\n"),
	io__write_string("\tlibrary which has been compiled with the same setting.\n"),
	io__write_string("\tRather than setting them individually, you must\n"),
	io__write_string("\tspecify them all at once by selecting a particular\n"),
	io__write_string("\tcompilation model (""grade"").\n\n"),
	io__write_string("\t-s <grade>, --grade <grade>\n"),
	io__write_string("\t\tSelect the compilation model. The <grade> should be one of\n"),
	io__write_string("\t\t`debug', `none', `reg', `jump', `asm_jump', `fast', `asm_fast',\n"),
	io__write_string("\t\tor one of those with `.gc', `.prof' or `.gc.prof' appended.\n"),
	io__write_string("\t\tDepending on your particular installation, only a subset\n"),
	io__write_string("\t\tof these possible grades will have been installed.\n"),
	io__write_string("\t\tAttempting to use a grade which has not been installed\n"),
	io__write_string("\t\twill result in an error at link time.\n"),
	io__write_string("\t--gcc-global-registers\t"),
	io__write_string("\t(grades: reg, fast, asm_fast)\n"),
	io__write_string("\t--no-gcc-global-registers"),
	io__write_string("\t(grades: debug, none, jump, asm_jump)\n"),
	io__write_string("\t\tSpecify whether or not to use GNU C's\n"),
	io__write_string("\t\tglobal register variables extension.\n"),
	io__write_string("\t--gcc-non-local-gotos\t"),
	io__write_string("\t(grades: jump, fast, asm_jump, asm_fast)\n"),
	io__write_string("\t--no-gcc-non-local-gotos"),
	io__write_string("\t(grades: debug, none, reg)\n"),
	io__write_string("\t\tSpecify whether or not to use GNU C's\n"),
	io__write_string("\t\t""labels as values"" extension.\n"),
	io__write_string("\t--asm-labels\t\t"),
	io__write_string("\t(grades: asm_jump, asm_fast)\n"),
	io__write_string("\t--no-asm-labels\t\t"),
	io__write_string("\t(grades: debug, none, reg, jump, fast)\n"),
	io__write_string("\t\tSpecify whether or not to use GNU C's\n"),
	io__write_string("\t\tasm extensions for inline assembler labels.\n"),
	io__write_string("\t--gc {none, conservative, accurate}\n"),
	io__write_string("\t--garbage-collection {none, conservative, accurate}\n"),
	io__write_string("\t\t\t\t\t(`.gc' grades use `--gc conservative',\n"),
	io__write_string("\t\t\t\t\tother grades use `--gc none'.)\n"),
	io__write_string("\t\tSpecify which method of garbage collection to use\n"),
	io__write_string("\t\t(default: conservative).  `accurate' GC is not yet implemented.\n"),
	io__write_string("\t--profiling\t\t"),
	io__write_string("\t(grades: any grade ending in `.prof')\n"),
	io__write_string("\t\tEnable profiling.  Insert profiling hooks in the\n"),
	io__write_string("\t\tgenerated code, and also output some profiling\n"),
	io__write_string("\t\tinformation (the static call graph) to the file\n"),
	io__write_string("\t\t`<module>.prof'.\n"),
	%io__write_string("\t--constraints\n"),
	%io__write_string("\t\tInterface with the CLP(R) constraint solver.\n"),
	io__write_string("\t--debug\t\t\t"),
	io__write_string("\t(grades: debug)\n"),
	io__write_string("\t\tEnable debugging.\n"),
	io__write_string("\t\tDebugging support is currently extremely primitive.\n"),
	io__write_string("\t\tWe recommend that you use instead use `mnp' or `msp'.\n"),
	io__write_string("\t\tSee the Mercury User's Guide for details.\n"),
	io__write_string("\t--tags {none, low, high}"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tSpecify whether to use the low bits or the high bits of \n"),
	io__write_string("\t\teach word as tag bits (default: low).\n"),
	% io__write_string("\t\t`--tags none' implies `--num-tag-bits 0'.\n"),
	io__write_string("\t--num-tag-bits <n>\t"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tUse <n> tag bits.\n"),

		% The --conf-low-tag-bits option is reserved for use
		% by the `mc' script; it is deliberately not documented.

		% The --bits-per-word option is intended for use
		% by the `mc' script; it is deliberately not documented.

		% The --bytes-per-word option is intended for use
		% by the `mc' script; it is deliberately not documented.

	io__write_string("\t--branch-delay-slot\t"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tAssume that branch instructions have a delay slot.\n"),
	io__write_string("\t--num-real-r-regs <n>\t"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tAssume registers r1 up to r<n> are real machine registers.\n"),
	io__write_string("\t--num-real-temps <n>\t"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tAssume that <n> temporaries will fit into\n"),
	io__write_string("\t\treal machine registers.\n"),
	io__write_string("\t--args {simple, compact}\n"),
	io__write_string("\t--arg-convention {simple, compact}\n"),
	io__write_string("\t\tUse the specified argument passing convention\n"),
	io__write_string("\t\tin the generated low-level C code. With the `simple'\n"),
	io__write_string("\t\tconvention, the <n>th argument is passed in or out\n"),
	io__write_string("\t\tusing register r<n>. With the `compact' convention,\n"),
	io__write_string("\t\tthe <n>th input argument is passed using register r<n>,\n"),
	io__write_string("\t\tand the <n>th output argument is returned using\n"),
	io__write_string("\t\tregister r<n>. The compact convention generally leads to\n"),
	io__write_string("\t\tmore efficient code. However, currently only the simple\n"),
	io__write_string("\t\tconvention is supported.\n"),
	io__write_string("\t--single-prec-float\n"),
	io__write_string("\t--unboxed-float\n"),
	io__write_string("\t(This option is not for general use.)\n"),
	io__write_string("\t\tUse unboxed single-precision floating point numbers,\n"),
	io__write_string("\t\trather than boxed double-precision floats.\n"),
	io__write_string("\t\t(The C code also needs to be compiled with\n"),
	io__write_string("\t\t`-DUSE_SINGLE_PREC_FLOAT'.)\n").

:- pred options_help_code_generation(io__state::di, io__state::uo) is det.

options_help_code_generation -->
	io__write_string("\nCode generation options:\n"),
	io__write_string("\t--no-trad-passes\n"),
	io__write_string("\t\tThe default --trad-passes completely processes each predicate\n"),
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
	io__write_string("\t--use-macro-for-redo-fail\n"),
	io__write_string("\t\tEmit the fail or redo macro instead of a branch\n"),
	io__write_string("\t\tto the fail or redo code in the runtime system.\n"),
	io__write_string("\t--cc <compiler-name>\n"),
	io__write_string("\t\tSpecify which C compiler to use.\n"),
	io__write_string("\t--c-include-directory <dir>\n"),
	io__write_string("\t\tSpecify the directory containing the Mercury C header files.\n"),
	io__write_string("\t--cflags <options>\n"),
	io__write_string("\t\tSpecify options to be passed to the C compiler.\n").

:- pred options_help_optimization(io__state::di, io__state::uo) is det.

options_help_optimization -->
	io__write_string("\nOptimization Options:\n"),
	io__write_string("\t-O <n>, --opt-level <n>, --optimization-level <n>\n"),
	io__write_string("\t\tSet optimization level to <n>.\n"),
	io__write_string("\t\tOptimization level 0 means no optimization\n"),
	io__write_string("\t\twhile optimization level 6 means full optimization.\n"),
	% io__write_string("\t\tFor a full description of each optimization level,\n"),
	% io__write_string("\t\tsee the Mercury User's Guide.\n"),
	io__write_string("\t--opt-space, --optimize-space\n"),
	io__write_string("\t\tTurn on optimizations that reduce code size\n"),
	io__write_string("\t\tand turn off optimizations that significantly\n"),
	io__write_string("\t\tincrease code size.\n").

:- pred options_help_hlds_hlds_optimization(io__state::di, io__state::uo)
	is det.

options_help_hlds_hlds_optimization -->
	io__write_string("\n    High-level (HLDS->HLDS) optimizations:\n"),
	io__write_string("\t--no-inlining\n"),
	io__write_string("\t\tDisable all forms of inlining.\n"),
	io__write_string("\t--no-inline-simple\n"),
	io__write_string("\t\tDisable the inlining of simple procedures.\n"),
	io__write_string("\t--no-inline-single-use\n"),
	io__write_string("\t\tDisable the inlining of procedures called only once.\n"),
	io__write_string("\t--inline-threshold <threshold>\n"),
	io__write_string("\t\tInline a procedure if its size (measured roughly\n"),
	io__write_string("\t\tin terms of the number of connectives in its internal form),\n"),
	io__write_string("\t\tmultiplied by the number of times it is called,\n"),
	io__write_string("\t\tis below the given threshold.\n"),
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
	io__write_string("\t--no-optimize-unused-args\n"),
	io__write_string("\t\tDisable removal of unused predicate arguments.\n"),
	io__write_string("\t\tThis will cause the compiler to generate less\n"),
	io__write_string("\t\tefficient code for many polymorphic predicates.\n"),
	io__write_string("\t--no-optimize-higher-order\n"),
	io__write_string("\t\tDisable specialization of higher-order predicates.\n").

:- pred options_help_hlds_llds_optimization(io__state::di, io__state::uo) is det.

options_help_hlds_llds_optimization -->
	io__write_string("\n    Medium-level (HLDS->LLDS) optimizations:\n"),
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
	io__write_string("\t\tmust be at least this number (default: 8).\n"),
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
	io__write_string("\n    Low-level (LLDS->LLDS) optimizations:\n"),
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
	io__write_string("\t\tIterate value numbering at most <n> times (default: 1).\n").
	% io__write_string("\t--pred-value-number\n"),
	% io__write_string("\t\tExtend value numbering to entire predicates\n").

:- pred options_help_output_optimization(io__state::di, io__state::uo) is det.

options_help_output_optimization -->
	io__write_string("\n    Output-level (LLDS->C) optimizations:\n"),
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
	io__write_string("\t\tseverely stress the C compiler on large modules.\n"),
	io__write_string("\t--split-c-files\n"),
	io__write_string("\t\tGenerate each C function in its own C file,\n"),
	io__write_string("\t\tso that the linker will optimize away unused code.\n"),
	io__write_string("\t\tThis option significantly increases compilation time,\n"),
	io__write_string("\t\tlink time, and intermediate disk space requirements,\n"),
	io__write_string("\t\tbut in return reduces the size of the final\n"),
	io__write_string("\t\texecutable, typically by about 10-20%.\n"),
	io__write_string("\t\tThis option is only useful with `--procs-per-c-function 1'.\n"),
	io__write_string("\t--no-c-optimize\n"),
	io__write_string("\t\tDon't enable the C compiler's optimizations.\n").

:- pred options_help_link(io__state::di, io__state::uo) is det.

options_help_link -->
	io__write_string("\nLink Options:\n"),
	io__write_string("\t-o <filename>, --output-file <filename>\n"),
	io__write_string("\t\tSpecify the name of the final executable.\n"),
	io__write_string("\t\t(The default executable name is the same as the name\n"),
	io__write_string("\t--link-flags <options>\n"),
	io__write_string("\t\tSpecify options to be passed to the linker.\n"),
	io__write_string("\t-L <directory>, --library-directory <directory>\n"),
	io__write_string("\t\tAppend <directory> to the list of directories in which\n"),
	io__write_string("\t\tto search for libraries.\n"),
	io__write_string("\t-l <library>, --library <library>\n"),
	io__write_string("\t\tLink with the specified library.\n"),
	io__write_string("\t--link-object <object-file>\n"),
	io__write_string("\t\tLink with the specified object file.\n"),
	io__write_string("\t\tof the first module on the command line.)\n").

:- pred options_help_misc(io__state::di, io__state::uo) is det.

options_help_misc -->
	io__write_string("\nMiscellaneous Options:\n"),
	% io__write_string("\t-H <n>, --heap-space <n>\n"),
	% io__write_string("\t\tPre-allocate <n> kilobytes of heap space.\n"),
	% io__write_string("\t\tThis option is now obsolete.  In the past it\n"),
	% io__write_string("\t\twas used to avoid NU-Prolog's\n"),
	% io__write_string("\t\t\t""Panic: growing stacks has required shifting the heap""\n"),
	% io__write_string("\t\tmessage.\n"),

	io__write_string("\t-b <builtin>, --builtin-module <builtin>\n"),
	io__write_string("\t\tUse `<builtin>' instead of `mercury_builtin' as the \n\t\tmodule which always gets automatically imported.\n"),
	io__write_string("\t-I <dir>, --search-directory <dir>\n"),
	io__write_string("\t\tAdd <dir> to the list of directories to be searched for \n\t\timported modules.\n").

:- end_module options.

%-----------------------------------------------------------------------------%
