%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: options.m.
% Main author: fjh.
%
% This modules defines the set of options accepted by the Mercury compiler.
% The definition takes the form of the types and predicates that getopt.m
% needs to parse command-line options.
%
% IMPORTANT NOTE: any changes to the options should be reflected in
% at least four places, and maybe more, in this module, with the order
% of options being the same in each of these places:
%
% - Every option must of course must appear in the definition of
%   the type "option" itself.
%
% - Every option should have its default value defined in a clause of
%   the optdef predicate.
%
%   For optimization options that should be set automatically at a specific
%   optimization level, there should also be an entry in opt_level.
%
%   For optimization options that should be set automatically if --opt-space
%   is given, there should also be an entry in opt_space.
%
% - Every option should have a clause in the long_table predicate
%   that converts the user-visible name of the option into its internal
%   representation as a value in the options type. For options whose names
%   include words that whose spelling differs in American vs British English,
%   there should normally be one entry for each spelling. In the rare case
%   that the option is used very frequently, there may also be an entry
%   for the option in the short_option predicate.
%
% - Every option should have a description in one of the options_help_x
%   predicates, with the right predicate again depending on what category
%   of options the option belongs to.
%
% Each option should also be documented in the Mercury User's Guide,
% which is in ../doc/user_guide.texi.
%
% Normally, the documentation in the users' guide is a copy of the help
% message, but it may also have additional detail.
%
% For options that should not be visible to users, there should still be
% a help message and an entry in the users' guide, for use by developers,
% but these should be commented out.
%
% XXX We should move towards a new arrangement where we have just two areas
% that need to be kept in sync:
%
% - the definition of the "option" type, and
%
% - the definition of a new predicate (named maybe opt_db)
%   whose arguments are
%
%   - an option,
%   - that option's option_category (this will be the first arg, for grouping),
%   - that option's initial value, and
%   - that option's help structure, with the names of any args separated out.
%
% The help structure itself will contain all the short and/or long names
% of the option. (Having the option's initial value in this structure
% will allow us, for accumulating and maybe options, to include the option name
% only in the documentation of the positive version, and not in the
% documentation of the negated version.)
%
% NOTE Unlike the current design, this would *require every option to be
% documented*, even if the documentation is kept private.
%
% NOTE It would also enforce that the help text for an option include
% the exact same set of option names as we give to getopt for that option.
%
% Invoking solutions on opt_db, we can process the result to
% automatically generate
%
% - a map from short option names to options (ShortOptionMap),
% - a map from long option names to options (LongOptionMap),and
% - a list which maps each option to its initial value (OptionInitValuesList).
%
% We can then pass closures for map.search(ShortOptionMap),
% map.search(LongOptionMap) and list.member(OptionInitValuesList)
% as the inputs to getopt.
% XXX getopt uses the last argument to create a map from option
% to initial value, which we can easily construct directly.
% We can, and should, modify getopt.m/getopt_io.m to allow this map to be
% supplied directly, through new function symbols in the option_ops type.
%
% We can also process the result of invoking solutions on opt_db by
%
% - classifying each opt_db entry to one or other of the sections
%   of the help text (a single section could contain e.g. both oc_warn_style
%   options *and* the oc_warn_style_c options, if that is what we call
%   the category of those oc_warn_ctrl options that control oc_warn_style
%   options),
%
% - sorting the opt_db entries in each section, and
%
% - printing the help text of each section, with appropriate headings
%   between the sections.
%
% Note that this would give a guiding principle for where new options
% should be added to the option type. This would be at the point where
% their help text should go, which should always be somewhere near,
% and hopefully in a logical relation to, the options related to them.
%
% XXX We should have a policy that says "when an option is supported
% only on a subset of all backends, its help text should list the
% *supported* backends, NOT the *unsupported* backends".
%
%---------------------------------------------------------------------------%

:- module libs.options.
:- interface.

:- import_module libs.optdb_help.
:- import_module libs.optimization_options.

:- import_module char.
:- import_module cord.
:- import_module getopt.
:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

:- pred optdb(option_category, option, option_data, libs.optdb_help.help).
:- mode optdb(out, in, out, out) is det.
:- mode optdb(in, out, out, out) is multi.
:- mode optdb(out, out, out, out) is multi.

%---------------------------------------------------------------------------%

:- pred get_short_option(short_option(option)::out(short_option)) is det.

:- pred get_long_option(long_option(option)::out(long_option)) is det.

:- pred get_option_default_table(option_default_value_map(option)::out) is det.

%---------------------------------------------------------------------------%

    % Return the list of all long option strings excluding the "--"
    % or "--no-" prefixes.
    %
:- pred all_long_option_strings(list(string)::out) is det.

    % Return the list of all long options strings that can be preceded
    % with the "--no-" prefix. The strings do not include the prefix.
    %
:- pred all_negatable_long_option_strings(list(string)::out) is det.

    % special_handler(Option, ValueForThatOption, OptionTableIn,
    %   MaybeOptionTableOut):
    %
    % This predicate is invoked whenever getopt finds an option
    % (long or short) designated as special, with special_data holding
    % the argument of the option (if any). The predicate can change the
    % option table in arbitrary ways in the course of handling the option,
    % or it can return an error message.
    % The canonical examples of special options are -O options in
    % compilers, which set many other options at once.
    % The MaybeOptionTableOut may either be ok(OptionTableOut), or it may
    % be error(ErrorString).
    %
:- pred special_handler(option::in, special_data::in,
    option_table::in, maybe_option_table::out,
    cord(optimization_option)::in, cord(optimization_option)::out) is semidet.

    % Return the options that warn about code that may not be
    % what the programmer intended.
    %
:- func dodgy_code_warning_options = list(option).

    % Return the options that warn about code that may be
    % unnecessarily slower than the programmer intended.
    %
:- func slow_code_warning_options = list(option).

    % Return the options that warn about issues that are only stylistic,
    % i.e. do not affect the implementation of the programmer's intention.
    %
:- func style_warning_options = list(option).

    % Return the options that request information from the compiler.
    %
:- func info_request_options = list(option).

    % Return the set of options which are inconsequential as far as the
    % `--track-flags' option is concerned. That is, adding or removing such
    % an option to a module should not force the module to be recompiled.
    %
:- func options_not_to_track = list(option).

%---------------------------------------------------------------------------%

    % Add a directory to search for Mercury libraries. This
    % adds `--search-directory', `--c-include-directory',
    % `--library-directory' and `--init-file-directory' options.
    %
:- pred option_table_add_mercury_library_directory(string::in,
    option_table::in, option_table::out) is det.

    % Add a directory using all of the
    % `--search-directory', `--intermod-directory',
    % `--library-directory', `--init-file-directory' and
    % `--c-include-directory', options.
    %
:- pred option_table_add_search_library_files_directory(string::in,
    option_table::in, option_table::out) is det.

    % Set all of the given options to the given value.
    %
:- pred set_all_options_to(list(option)::in, option_data::in,
    option_table::in, option_table::out) is det.

%---------------------------------------------------------------------------%

    % NOTE: ALL OPTIONS SHOULD BE DOCUMENTED!
    %
    % Officially supported options should be documented both in the
    % help message output by options_help/2, and also in the
    % "invocation" chapter of doc/user_guide.texi.
    %
    % Options which are not officially supported (e.g. those used
    % internally by the Mercury implementation, those which are not
    % sufficiently useful to be worth mentioning in the User Guide,
    % or options for experimental features that are not yet stable
    % enough to be officially supported should still be documented).
    % The documentation can go either next to the option definition
    % here, or as commented-out code in the appropriate subroutine
    % of options_help/2.
    %
    % NOTE: We should ensure that the order of the options
    % is consistent in all the following places:
    %
    % - in this definition of the `option' type
    % - in the clauses of the `optdef' predicate
    % - in the clauses of `long_table' predicate
    %   (not in the clauses of the `short_option' predicate, since
    %   that is sorted alphabetically on the option letter)
    % - in the special_handler predicate, if they appear there
    % - in the help predicates.
    %
    % At the moment, there are considerable deviations from such consistency.
:- type option
    % Help options.
    --->    help
    ;       help_alt
    ;       version

    % Warning options
    ;       inhibit_warnings
    ;       inhibit_style_warnings
    ;       warn_accumulator_swaps
    ;       halt_at_warn
    ;       halt_at_warn_make_int
    ;       halt_at_warn_make_opt
            % Almost all code in the compiler should only look at the value
            % of halt_at_warn. handle_options.m will overwrite its value
            % with the value of halt_at_warn_make_int when making interface
            % files and with the value of halt_at_warn_make_opt when making
            % optimization files.
    ;       halt_at_syntax_errors
    ;       halt_at_auto_parallel_failure
    ;       halt_at_invalid_interface
    ;       warn_singleton_vars
    ;       warn_repeated_singleton_vars
    ;       warn_overlapping_scopes
    ;       warn_det_decls_too_lax
    ;       warn_inferred_erroneous
    ;       warn_nothing_exported
    ;       warn_unused_args
    % XXX We should also warn about unused statevars in "some (...)" scopes.
    ;       warn_unneeded_initial_statevars
    ;       warn_unneeded_initial_statevars_lambda
    ;       warn_unneeded_final_statevars
    ;       warn_unneeded_final_statevars_lambda
    ;       warn_interface_imports
    ;       warn_interface_imports_in_parents
    ;       warn_missing_opt_files
    ;       warn_missing_trans_opt_files
    ;       warn_missing_trans_opt_deps
    ;       warn_inconsistent_pred_order_clauses
    ;       warn_inconsistent_pred_order_foreign_procs
    ;       warn_non_contiguous_decls
    ;       warn_non_contiguous_clauses
    ;       warn_non_contiguous_foreign_procs
    ;       allow_non_contiguity_for
    ;       warn_non_stratification
    ;       warn_unification_cannot_succeed
    ;       warn_simple_code
    ;       warn_duplicate_calls
    ;       warn_implicit_stream_calls
    ;       warn_smart_recompilation
    ;       warn_undefined_options_variables
    ;       warn_suspicious_recursion
    ;       warn_non_tail_recursion_self
    ;       warn_non_tail_recursion_mutual
    ;       warn_non_tail_recursion
    ;       warn_obvious_non_tail_recursion
    ;       warn_target_code
    ;       warn_up_to_date
    ;       warn_stubs
    ;       warn_dead_procs
    ;       warn_dead_preds
    ;       warn_table_with_inline
    ;       warn_non_term_special_preds
    ;       warn_known_bad_format_calls
    ;       warn_only_one_format_string_error
    ;       warn_unknown_format_calls
    ;       warn_obsolete
    ;       warn_insts_without_matching_type
    ;       warn_insts_with_functors_without_type
    ;       warn_unused_imports
    ;       warn_unused_interface_imports
    ;       inform_ite_instead_of_switch
    ;       inform_incomplete_switch
    ;       inform_incomplete_switch_threshold
    ;       warn_unresolved_polymorphism
    ;       warn_suspicious_foreign_procs
    ;       warn_suspicious_foreign_code
    ;       warn_state_var_shadowing
    ;       warn_unneeded_mode_specific_clause
    ;       warn_suspected_occurs_check_failure
    ;       warn_potentially_ambiguous_pragma
    ;       warn_ambiguous_pragma
    ;       warn_stdlib_shadowing
    ;       inform_incomplete_color_scheme
    ;       inform_inferred
    ;       inform_inferred_types
    ;       inform_inferred_modes
    ;       inform_suboptimal_packing
    ;       print_error_spec_id
    ;       inform_ignored_pragma_errors
    ;       inform_generated_type_spec_pragmas
    ;       warn_redundant_coerce
    ;       warn_can_fail_function
    ;       warn_unsorted_import_blocks

    % Verbosity options
    ;       verbose
    ;       very_verbose
    ;       verbose_errors
    ;       verbose_recompilation
    ;       find_all_recompilation_reasons
    ;       verbose_make
    ;       verbose_commands
    ;       output_compile_error_lines
    ;       report_cmd_line_args
    ;       report_cmd_line_args_in_doterr
    ;       statistics
    ;       detailed_statistics
    ;       proc_size_statistics
    ;       inst_statistics
    ;       limit_error_contexts
    ;       config_default_color_diagnostics
    ;       color_diagnostics
    ;       color_diagnostics_is_set
    ;       color_diagnostics_is_set_to
    ;       use_color_diagnostics
            % The color_scheme used by the compiler is specified by
            % the color_scheme_set_to option. The color_scheme_set_by
            % option specifies what agent set the value of color_scheme_set_to
            % with the valid values being "default", "option" and "envvar".
            % Both color_scheme_set_by and color_scheme_set_to are for
            % internal use only.
            %
            % The color_scheme option is visible to users.
            % It is a special option whose action sets color_scheme_set_to
            % to its own argument, and sets color_scheme_set_by to "option".
            %
            % The color_scheme_envvar option is not visible to users.
            % It is a special option whose action sets color_scheme_set_to
            % to its own argument, and sets color_scheme_set_by to "envvar".
    ;       color_scheme
    ;       color_scheme_envvar
    ;       color_scheme_set_by
    ;       color_scheme_set_to
            % The ignore_color_scheme_envvar option is intended for
            % only one purpose: enable the testing of color options
            % in tests/invalid *despite* the setting of MERCURY_COLOR_SCHEME
            % by tools/bootcheck.
    ;       ignore_color_scheme_envvar
            % The set_color_X options are for internal use only.
            % They record the results of the color scheme selection.
    ;       set_color_subject
    ;       set_color_correct
    ;       set_color_incorrect
    ;       set_color_inconsistent
    ;       set_color_hint
    ;       debug_types
    ;       debug_types_pred_name
    ;       debug_modes
    ;       debug_modes_statistics
    ;       debug_modes_minimal
    ;       debug_modes_verbose
    ;       debug_modes_delay_vars
    ;       debug_modes_goal_ids
    ;       debug_modes_pred_id
    ;       debug_dep_par_conj
    ;       debug_det
    ;       debug_code_gen_pred_id
    ;       debug_dead_proc_elim
    ;       debug_higher_order_specialization
    ;       debug_opt
    ;       debug_term          % term = constraint termination analysis
    ;       debug_opt_pred_id
    ;       debug_opt_pred_name
    ;       debug_pd            % pd = partial deduction/deforestation
    ;       debug_liveness
    ;       debug_stack_opt
    ;       debug_make
    ;       debug_closure
    ;       debug_trail_usage
    ;       debug_mode_constraints
    ;       debug_intermodule_analysis
    ;       debug_mm_tabling_analysis
    ;       debug_indirect_reuse
    ;       debug_type_rep

    % Opmode options
    ;       only_opmode_make_short_interface
    ;       only_opmode_make_interface
    ;       only_opmode_make_private_interface
    ;       only_opmode_make_optimization_interface
    ;       only_opmode_make_transitive_opt_interface
    ;       only_opmode_make_analysis_registry
    ;       only_opmode_make_xml_documentation
    ;       only_opmode_generate_source_file_mapping
    ;       only_opmode_generate_dependency_file
    ;       only_opmode_generate_dependencies
    ;       only_opmode_generate_dependencies_ints
    ;       only_opmode_generate_standalone_interface
    ;       only_opmode_convert_to_mercury
    ;       only_opmode_typecheck_only
    ;       only_opmode_errorcheck_only
    ;       only_opmode_target_code_only
    ;       only_opmode_compile_only
    ;       only_opmode_output_grade_string
    ;       only_opmode_output_link_command
    ;       only_opmode_output_shared_lib_link_command
    ;       only_opmode_output_stdlib_grades
    ;       only_opmode_output_library_install_grades
    ;       only_opmode_output_cc
    ;       only_opmode_output_c_compiler_type
    ;       only_opmode_output_csharp_compiler
    ;       only_opmode_output_csharp_compiler_type
    ;       only_opmode_output_cflags
    ;       only_opmode_output_library_link_flags
    ;       only_opmode_output_grade_defines
    ;       only_opmode_output_c_include_directory_flags
    ;       only_opmode_output_target_arch
    ;       only_opmode_output_java_class_dir
    ;       only_opmode_output_stdlib_modules
    ;       only_opmode_output_optimization_options
    ;       only_opmode_output_optimization_options_upto

    % Auxiliary output options
    ;       error_output_suffix
    ;       progress_output_suffix
    ;       inference_output_suffix
    ;       debug_output_suffix
    ;       recompile_output_suffix

    ;       generate_module_order
    ;       shared_lib_not_executable

    ;       smart_recompilation
            % Even if this option is set to `yes', smart recompilation may
            % have been disabled with io_set_disable_smart_recompilation.
            % Before using the value of this option, call
            % io_get_disable_smart_recompilation to see whether this
            % has been done.

    ;       generate_item_version_numbers
            % This option is used to control output of version numbers
            % in interface files. It is implied by --smart-recompilation,
            % and cannot be set explicitly by the user.

            % Even if this option is set to `yes', version numbers may have
            % been disabled with io_set_disable_generate_item_version_numbers.
            % Before using the value of this option, call
            % io_get_disable_generate_item_version_numbers to see whether this
            % has been done.

    ;       generate_mmc_make_module_dependencies
    ;       trace_level
    ;       trace_optimized
    ;       trace_prof
    ;       trace_table_io
    ;       trace_table_io_only_retry
    ;       trace_table_io_states
    ;       trace_table_io_require
    ;       trace_table_io_all
    ;       trace_goal_flags
    ;       prof_optimized
    ;       exec_trace_tail_rec
    ;       suppress_trace
    ;       force_disable_tracing
            % Force no tracing, even in .debug grades. This is used to turn off
            % tracing in the browser directory while still allowing the browser
            % library to be linked in with an executable compiled in a .debug
            % grade.
    ;       delay_death
    ;       delay_death_max_vars

    ;       stack_trace_higher_order
    ;       force_disable_ssdebug
    ;       line_numbers
    ;       line_numbers_around_foreign_code
    ;       line_numbers_for_c_headers
    ;       type_repns_for_humans
    ;       auto_comments
    ;       frameopt_comments
    ;       max_error_line_width
    ;       reverse_error_order
    ;       show_definitions
    ;       show_definition_line_counts
    ;       show_definition_extents
    ;       show_local_call_tree
    ;       show_local_type_repns
    ;       show_all_type_repns
    ;       show_developer_type_repns
    ;       show_dependency_graph
    ;       show_imports_graph
    ;       show_pred_movability
    ;       trans_opt_deps_spec
    ;       dump_trace_counts
    ;       dump_hlds
    ;       dump_hlds_pred_id
    ;       dump_hlds_pred_name
    ;       dump_hlds_pred_name_order
    ;       dump_hlds_spec_preds
    ;       dump_hlds_spec_preds_for
    ;       dump_hlds_alias
    ;       dump_hlds_options
    ;       dump_hlds_inst_limit
    ;       dump_hlds_inst_size_limit
    ;       dump_hlds_file_suffix
    ;       dump_same_hlds
    ;       dump_mlds
    ;       dump_mlds_pred_name
    ;       verbose_dump_mlds
    ;       dump_options_file
    ;       mode_constraints
    ;       simple_mode_constraints
    ;       prop_mode_constraints
    ;       compute_goal_modes
    ;       benchmark_modes
    ;       benchmark_modes_repeat
    ;       debug_unneeded_code
    ;       debug_unneeded_code_pred_name
    ;       debug_common_struct_preds

    % Language semantics options
    ;       reorder_conj
    ;       reorder_disj
    ;       fully_strict
    ;       strict_sequential
    ;       allow_stubs
    ;       infer_types
    ;       infer_modes
    ;       infer_det
    ;       infer_all
    ;       type_inference_iteration_limit
    ;       mode_inference_iteration_limit
    ;       event_set_file_name

    % Compilation Model options
    ;       grade

    % Target selection options
    ;       target
    ;       compile_to_c        % target c + target_code_only
    ;       java                % target java
    ;       java_only           % target java + target_code_only
    ;       csharp              % target csharp
    ;       csharp_only         % target csharp + target_code_only

    % Compilation model options for optional features:

    % (a) Debugging
    % For documentation of the exec_trace and decl_debug options, see the
    % documentation for MR_EXEC_TRACE and MR_DECL_DEBUG in
    % runtime/mercury_conf_param.h.
    ;       exec_trace
    ;       decl_debug

    % (b) Profiling
    ;       profiling           % profile_time + profile_calls
    ;       time_profiling      % profile_time + profile_calls
    ;       memory_profiling    % profile_mem + profile_calls
    ;       deep_profiling      % profile_deep
    ;       profile_calls
    ;       profile_time
    ;       profile_memory
    ;       profile_deep
    ;       use_activation_counts
            % Use_activation_counts is used to determine which mechanism for
            % cycle detection should be used for deep profiling. Actually,
            % we only want to use the `yes' value, but we keep support for
            % the `no' value for benchmarks for the paper.

    ;       pre_prof_transforms_simplify
            % Run the simplification pass at before profiling (stage 215) this
            % is implied by some of the profiling settings. Specifying this
            % option causes this simplification pass to run even when profiling
            % is not enabled.

    ;       pre_implicit_parallelism_simplify
            % Run the simplification pass before the implicit parallelism pass
            % to ensure that the HLDS more closely matches the feedback data.

            % Perform coverage profiling, this affects only deep profiling
            % grades.
    ;       coverage_profiling
    ;       coverage_profiling_via_calls
    ;       coverage_profiling_static

            % What types of coverage points to instrument the code with.
    ;       profile_deep_coverage_after_goal
    ;       profile_deep_coverage_branch_ite
    ;       profile_deep_coverage_branch_switch
    ;       profile_deep_coverage_branch_disj

            % Tunables for the coverage profiling pass.
            % XXX: Currently both these options are unsupported.
    ;       profile_deep_coverage_use_portcounts
    ;       profile_deep_coverage_use_trivial

            % Turn on flags relevant for profiler directed feedback analysis.
            % Currently the only feedback analysis is automatic parallelism.
    ;       profile_for_feedback

    ;       use_zeroing_for_ho_cycles
    ;       use_lots_of_ho_specialization

            % We do not currently enable (or publicly document) this option
            % because its use results in significant overheads. Also, it is
            % not compatible with coverage profiling, which is enabled by
            % default. By default, all deep profiling grades are also built
            % with --stack-segments in order to avoid problems caused by the
            % lack of tail recursion.
    ;       deep_profile_tail_recursion
    ;       record_term_sizes_as_words
    ;       record_term_sizes_as_cells
    ;       experimental_complexity

    % (c) Miscellaneous
    ;       gc
    ;       parallel
    ;       threadscope
    ;       use_trail
    ;       use_minimal_model_stack_copy
    ;       use_minimal_model_own_stacks
    ;       minimal_model_debug
    ;       pregenerated_dist
    ;       single_prec_float
    ;       type_layout
    ;       maybe_thread_safe_opt
    ;       extend_stacks_when_needed
    ;       stack_segments
    ;       use_regions
    ;       use_alloc_regions
    ;       use_regions_debug
    ;       use_regions_profiling
    ;       source_to_source_debug
    ;       ssdb_trace_level
    ;       link_ssdb_libs

    % Data representation compilation model options
    ;       num_ptag_bits
    ;       bits_per_word
    ;       bytes_per_word
            % The undocumented conf_low_ptag_bits option is used by the `mmc'
            % script to pass the default value for num_ptag_bits assuming
            % target_c. The reason that `mmc' doesn't just pass a default
            % value for --num-ptag-bits is that users need to be able to
            % override this default when cross compiling.
    ;       conf_low_ptag_bits
    ;       unboxed_float
    ;       unboxed_int64s
    ;       unboxed_no_tag_types
    ;       arg_pack_bits
    ;       pack_everything
    ;       allow_direct_args
    ;       allow_double_word_fields
    ;       allow_double_word_ints          % XXX bootstrapping option
    ;       allow_packing_dummies           % XXX bootstrapping option
    ;       allow_packing_ints              % XXX bootstrapping option
    ;       allow_packing_chars             % XXX bootstrapping option
    ;       allow_packing_local_sectags     % XXX bootstrapping option
    ;       allow_packing_remote_sectags    % XXX bootstrapping option
    ;       allow_packing_mini_types        % XXX bootstrapping option
    ;       allow_packed_unify_compare      % XXX bootstrapping option
    ;       sync_term_size_in_words

    % LLDS back-end compilation model options
    ;       gcc_non_local_gotos
    ;       gcc_global_registers
    ;       asm_labels
    ;       use_float_registers

    % MLDS back-end compilation model options
    ;       highlevel_code
    ;       c_debug_grade
    ;       det_copy_out
    ;       nondet_copy_out
    ;       put_commit_in_own_func

    % Options for internal use only (the values of these options are implied
    % by the settings of other options)

    ;       backend_foreign_languages
            % The foreign programming languages that this backend can
            % interface to.

    ;       stack_trace
            % Stack layout information required to do a stack trace.

    ;       basic_stack_layout
            % Stack layout information required to do accurate GC.

    ;       agc_stack_layout
            % Stack layout information required to do procedure identification.

    ;       procid_stack_layout
            % Stack layout information required to do execution tracing.

    ;       trace_stack_layout

    ;       body_typeinfo_liveness
            % Use an alternate calculation of liveness where the typeinfo
            % for a type variable must live at any point in the body of the
            % procedure at which a live variable's type includes that type
            % variable.
            %
            % Although this option governs whether the body of a procedure
            % uses this liveness calculation, it is not the only consideration
            % we have to take into account when deciding on the interface
            % of any procedure whose address may be taken. We must include
            % typeinfos describing the types of all arguments in the interface
            % of a procedure if either this option is set *or* the procedure's
            % address may be taken, otherwise, the layout structure we include
            % in closures using that procedure may not have all the information
            % required to reconstruct the types of all the values inside the
            % closure.
            %
            % The only place in the compiler that should look at this option
            % is the predicate body_should_use_typeinfo_liveness in
            % hlds_pred.m; everything else, including the predicates deciding
            % interface typeinfo liveness, should go through there.

    ;       can_compare_constants_as_ints
            % Should be set to yes if the target back end guarantees that
            % comparing two values for equality, at least one of which is a
            % constant, can be done by casting them both to integers and
            % comparing the integers for equality.

    ;       pretest_equality_cast_pointers
            % Should be set to yes if the test of whether two input arguments
            % are object identical should be done by casting the arguments to a
            % generic pointer type. Otherwise they will be cast to integers.

    ;       delay_partial_instantiations

    % Options for internal use only (setting these options to non-default
    % values can result in programs that do not link, or programs that dump
    % core)
    ;       allow_defn_of_builtins
            % Do not generate errors for definitions of builtin predicates.
            % When a new builtin is introduced, the installed compiler won't
            % know about it, and thus when it sees its declaration, it wants a
            % definition, but when the modified compiler is bootstrapped,
            % it would normally generate an error when it sees that very same
            % definition in the library (usually in builtin.m or
            % private_builtin.m). When this option is set, it allows such
            % definitions. Once the modified compiler is installed on all
            % relevant machines, the option can be turned off again.

%   ;       special_preds
            % Generate unify and compare preds. For measurement only.
            % Code generated with this set to `no' is unlikely to actually
            % work.
            % Disabled to allow the code paths for generating special preds
            % to be simplified. If this option is ever needed again, which
            % I (zs) do not think is likely, it should be implemented
            % differently: by generating the special predicates, and then
            % not writing them out. The logic for *that* should be a lot
            % simpler.

    ;       type_ctor_info
            % Generate type_ctor_info structures. For measurement only --
            % if you turn this off, then you're unlikely to be able to link.

    ;       type_ctor_layout
            % Generate type_ctor_layout structures. For measurement only --
            % if you turn this off, then you're unlikely to be able to link.

    ;       type_ctor_functors
            % Generate type_ctor_functors structures. For measurement only --
            % if you turn this off, then you're unlikely to be able to link.

    ;       new_type_class_rtti
            % XXX temporary option: enables the generation of new style static
            % data structures for runtime information about type classes.
            % These are not yet used. When we add code to generate the matching
            % dynamic data structures and switch over to use them, we won't
            % need this option anymore.

    ;       rtti_line_numbers
            % Generate line number information in the RTTI when debugging is
            % enabled. For measurement only -- if you turn this off, then the
            % debugger may dereference garbage pointers.

    ;       disable_mmsc_pneg
    ;       disable_mmsc_cut
    ;       use_mmsc_pneg
    ;       use_mmsc_cut
            % These four are used to analyze the performance effects
            % of minimal model tabling.

    ;       disable_trail_ops
            % This is used to analyze the performance effects of trailing.

    ;       size_region_ite_fixed
    ;       size_region_disj_fixed
%   ;       size_region_semi_disj_fixed     % XXX unused, which may be a bug
    ;       size_region_commit_fixed

    ;       size_region_ite_protect
    ;       size_region_ite_snapshot
    ;       size_region_semi_disj_protect
    ;       size_region_disj_snapshot
    ;       size_region_commit_entry

    ;       allow_multi_arm_switches

    ;       type_check_using_constraints

    % Code generation options
    ;       table_debug
    ;       trad_passes
    ;       parallel_liveness
    ;       parallel_code_gen
    ;       reclaim_heap_on_failure
    ;       reclaim_heap_on_semidet_failure
    ;       reclaim_heap_on_nondet_failure
    ;       have_delay_slot
    ;       num_real_r_regs
    ;       num_real_f_regs
    ;       num_real_r_temps
    ;       num_real_f_temps
    ;       max_jump_table_size
    ;       max_specialized_do_call_closure
    ;       max_specialized_do_call_class_method
    ;       compare_specialization
    ;       should_pretest_equality
    ;       fact_table_max_array_size
            % Maximum number of elements in a single fact table data array.

    ;       fact_table_hash_percent_full
            % How full the fact table hash tables should be allowed to get,
            % given as an integer percentage.

    ;       prefer_switch
    ;       prefer_while_loop_over_jump_self
    ;       prefer_while_loop_over_jump_mutual
    ;       opt_no_return_calls                     % XXX should be oc_opt
    ;       debug_class_init

    % Optimization Options

    %   - HLDS
    ;       optopt_allow_inlining
    ;       inlining
    ;       optopt_inline_simple
    ;       optopt_inline_builtins
    ;       optopt_inline_single_use
    ;       optopt_inline_call_cost
    ;       optopt_inline_compound_threshold
    ;       optopt_inline_simple_threshold
    ;       optopt_inline_vars_threshold
    ;       optopt_intermod_inline_simple_threshold
    ;       optopt_inline_tr_sccs
    ;       optopt_inline_tr_sccs_max_extra
    ;       optopt_from_ground_term_threshold
    ;       optopt_enable_const_struct_poly
    ;       optopt_enable_const_struct_user
    ;       optopt_common_struct
    ;       optopt_constraint_propagation
    ;       optopt_local_constraint_propagation
    ;       optopt_unused_args
    ;       optopt_intermod_unused_args
    ;       optopt_higher_order
    ;       optopt_higher_order_size_limit
    ;       optopt_higher_order_arg_limit
    ;       optopt_unneeded_code
    ;       optopt_unneeded_code_copy_limit
    ;       optopt_type_specialization
    ;       optopt_user_guided_type_specialization
    ;       optopt_introduce_accumulators
    ;       optopt_lcmc_accumulator
    ;       optopt_lcmc_null
    ;       optopt_lcmc
    ;       optopt_duplicate_calls
    ;       optopt_constant_propagation
    ;       optopt_excess_assign
    ;       optopt_merge_code_after_switch
    ;       optopt_format_calls
    ;       optopt_split_switch_arms
    ;       optopt_saved_vars_const
    ;       optopt_svcell
    ;       optopt_svcell_loop
    ;       optopt_svcell_full_path
    ;       optopt_svcell_on_stack
    ;       optopt_svcell_candidate_headvars
    ;       optopt_svcell_cv_store_cost
    ;       optopt_svcell_cv_load_cost
    ;       optopt_svcell_fv_store_cost
    ;       optopt_svcell_fv_load_cost
    ;       optopt_svcell_op_ratio
    ;       optopt_svcell_node_ratio
    ;       optopt_svcell_all_path_node_ratio
    ;       optopt_svcell_all_candidates
    ;       optimize_saved_vars
    ;       optopt_loop_invariants
    ;       optopt_delay_construct
    ;       optopt_follow_code
    ;       optopt_dead_procs
    ;       optopt_deforestation
    ;       optopt_deforestation_depth_limit
    ;       optopt_deforestation_cost_factor
    ;       optopt_deforestation_vars_threshold
    ;       optopt_deforestation_size_threshold
    ;       optopt_untuple
    ;       optopt_tuple
    ;       optopt_tuple_trace_counts_file
    ;       optopt_tuple_costs_ratio
    ;       optopt_tuple_min_args
    ;       optopt_inline_par_builtins
    ;       optopt_always_spec_dep_par_conjs
    ;       optopt_allow_some_paths_only_waits
    ;       optopt_region_analysis

    %   - HLDS->LLDS
    ;       optopt_smart_indexing
    ;         optopt_dense_switch_req_density
    ;         optopt_lookup_switch_req_density
    ;         optopt_dense_switch_size
    ;         optopt_lookup_switch_size
    ;         optopt_string_trie_switch_size
    ;         optopt_string_hash_switch_size
    ;         optopt_string_binary_switch_size
    ;         optopt_tag_switch_size
    ;         optopt_try_switch_size
    ;         optopt_binary_switch_size
    ;         optopt_switch_single_rec_base_first
    ;         optopt_switch_multi_rec_base_first

    ;         optopt_smart_atomic_indexing
    ;         optopt_smart_string_indexing
    ;         optopt_smart_tag_indexing
    ;         optopt_smart_float_indexing

    ;       optopt_static_ground_cells
    ;       optopt_static_ground_floats
    ;       optopt_static_ground_int64s
    ;       optopt_static_code_addresses

    ;       optopt_use_atomic_cells
    ;       optopt_middle_rec
    ;       optopt_simple_neg
    ;       optopt_allow_hijacks

    %   - MLDS
    ;       optopt_optimize_mlds
    ;       optopt_peep_mlds
    ;       optopt_mlds_tailcalls
    ;       optopt_initializations
    ;       optopt_eliminate_unused_mlds_assigns
    ;       optopt_eliminate_local_vars
    ;       optopt_generate_trail_ops_inline

    %   - LLDS
    ;       optopt_common_data
    ;       optopt_common_layout_data
    ;       optopt_optimize_llds
    ;       optopt_peep_llds
    ;       optopt_peep_llds_mkword
    ;       optopt_jumps
    ;       optopt_fulljumps
    ;       optopt_pessimize_tailcalls
    ;       optopt_checked_nondet_tailcalls
    ;       optopt_use_local_vars
    ;       optopt_local_var_access_threshold
    ;       optopt_standardize_labels
    ;       optopt_labels
    ;       optopt_dups
    ;       optopt_proc_dups
    ;       optopt_frames
    ;       optopt_delay_slot
    ;       optopt_reassign
    ;       optopt_repeat_opts
    ;       optopt_layout_compression_limit

    %   - C
    ;       optopt_use_macro_for_redo_fail
    ;       optopt_emit_c_loops
    ;       optopt_procs_per_c_function
    ;       optopt_use_just_one_c_func
    ;       optopt_local_thread_engine_base
    ;       optopt_inline_alloc
    ;       optopt_c_optimize

    % Special optimization options.

    ;       default_opt_level
    ;       opt_level
    ;       opt_space                   % Default is to optimize time.
    ;       intermodule_optimization
    ;       read_opt_files_transitively
    ;       use_opt_files
    ;       use_trans_opt_files
    ;       transitive_optimization
    ;       intermodule_analysis
    ;       analysis_repeat
    ;       analysis_file_cache

    ;       analyse_trail_usage
    ;       optimize_trail_usage
    ;       optimize_region_ops
    ;       analyse_mm_tabling

    % Stuff for the CTGC system (structure sharing / structure reuse).
    ;       structure_sharing_analysis
    ;           structure_sharing_widening
    ;       structure_reuse_analysis
    ;           structure_reuse_constraint
    ;           structure_reuse_constraint_arg
    ;           structure_reuse_max_conditions
    ;           structure_reuse_repeat
    ;           structure_reuse_free_cells

    % Stuff for the old termination analyser.
    ;       termination_enable
    ;       termination_check
    ;       termination_check_verbose
    ;       termination_single_args
    ;       termination_norm
    ;       termination_error_limit
    ;       termination_path_limit

    % Stuff for the new termination analyser.
    ;       termination2_enable
    ;       termination2_check
    ;       termination2_check_verbose
    ;       termination2_norm
    ;       termination2_widening_limit
    ;       termination2_arg_size_only
    ;       termination2_prop_fail_constrs
    ;       termination2_maximum_matrix_size

    ;       analyse_exceptions
    ;       analyse_closures

    % Target code compilation options
    ;       target_debug

    % C
    ;       cc
    ;       cflags
    ;       quoted_cflag
    ;       c_include_directories
    ;       ansi_c

    % Flags for specific C compilers.
    ;       gcc_flags
    ;       quoted_gcc_flag
    ;       clang_flags
    ;       quoted_clang_flag
    ;       msvc_flags
    ;       quoted_msvc_flag

    % Auto-configured C compilation options.
    ;       cflags_for_warnings
    ;       cflags_for_optimization
    ;       cflags_for_ansi
    ;       cflags_for_regs
    ;       cflags_for_gotos
    ;       cflags_for_threads
    ;       cflags_for_debug
    ;       cflags_for_sanitizers
    ;       cflags_for_pic
    ;       cflags_for_lto
    ;       c_flag_to_name_object_file
    ;       object_file_extension
    ;       pic_object_file_extension
    ;       c_compiler_type
    ;       csharp_compiler_type

    % Java
    ;       java_compiler
    ;       java_interpreter
    ;       java_compiler_flags
    ;       quoted_java_compiler_flag
    ;       java_classpath
    ;       java_runtime_flags
    ;       quoted_java_runtime_flag

    % C#
    ;       csharp_compiler
    ;       csharp_flags
    ;       quoted_csharp_flag
    ;       cli_interpreter

    % Link options
    ;       output_file_name
    ;       ld_flags
    ;       quoted_ld_flag
    ;       ld_libflags
    ;       quoted_ld_libflag
    ;       link_library_directories
    ;       runtime_link_library_directories
    ;       use_default_runtime_library_directory
    ;       link_libraries
    ;       link_objects
    ;       mercury_library_directories
    ;       mercury_library_directory_special
    ;       search_library_files_directories
    ;       search_library_files_directory_special
    ;       mercury_libraries
    ;       mercury_library_special
    ;       mercury_standard_library_directory
    ;       mercury_standard_library_directory_special
    ;       init_file_directories
    ;       init_files
    ;       trace_init_files
    ;       only_globals_linkage
    ;       linkage_special
    ;       only_globals_mercury_linkage
    ;       mercury_linkage_special
    ;       strip
    ;       demangle
    ;       main
    ;       allow_undefined
    ;       use_readline
    ;       runtime_flags
    ;       extra_init_functions
    ;       frameworks
    ;       framework_directories
    ;       sign_assembly
    ;       cstack_reserve_size

    % Auto-configured options.
    ;       shared_library_extension
    ;       library_extension
    ;       executable_file_extension
    ;       link_executable_command
    ;       link_shared_lib_command
    ;       create_archive_command
    ;       create_archive_command_output_flag
    ;       create_archive_command_flags
    ;       ranlib_command
    ;       ranlib_flags
    ;       mkinit_command
    ;       demangle_command
    ;       filtercc_command
    ;       filterjavac_command
    ;       trace_libs
    ;       thread_libs
    ;       hwloc_libs
    ;       hwloc_static_libs
    ;       shared_libs
    ;       math_lib
    ;       readline_libs
    ;       linker_opt_separator
    ;       linker_thread_flags
    ;       shlib_linker_thread_flags
    ;       linker_lto_flags
    ;       linker_static_flags
    ;       linker_strip_flag
    ;       linker_link_lib_flag
    ;       linker_link_lib_suffix
    ;       shlib_linker_link_lib_flag
    ;       shlib_linker_link_lib_suffix
    ;       linker_debug_flags
    ;       shlib_linker_debug_flags
    ;       linker_sanitizer_flags
    ;       linker_trace_flags
    ;       shlib_linker_trace_flags
    ;       linker_path_flag
    ;       linker_rpath_flag
    ;       linker_rpath_separator
    ;       shlib_linker_rpath_flag
    ;       shlib_linker_rpath_separator
    ;       linker_allow_undefined_flag
    ;       linker_error_undefined_flag
    ;       shlib_linker_use_install_name
    ;       shlib_linker_install_name_flag
    ;       shlib_linker_install_name_path
    ;       strip_executable_command
    ;       strip_executable_shared_flags
    ;       strip_executable_static_flags
    ;       java_archive_command

    % Build system options
    ;       only_opmode_make
    ;       only_opmode_invoked_by_mmc_make
    ;       part_opmode_rebuild
    ;       keep_going
    ;       make_max_jobs
    ;       make_track_flags
    ;       extra_init_command
    ;       make_pre_link_command
    ;       install_prefix
    ;       use_symlinks
    ;       mercury_configuration_directory
    ;       mercury_configuration_directory_special
    ;       install_method
    ;       install_command
    ;       install_command_dir_option
    ;       detect_stdlib_grades
    ;       library_install_grades
    ;       library_install_grades_incl_components
    ;       library_install_grades_excl_components
    ;       only_globals_library_install_linkages
    ;       flags_file
    ;       options_files
    ;       config_file
    ;       options_search_directories
    ;       setting_only_use_subdirs
    ;       setting_only_use_grade_subdirs
    ;       error_files_in_subdir
    ;       std_int_file_not_written_msgs
    ;       search_directories
    ;       intermod_directories
    ;       use_search_directories_for_intermod
    ;       interface_dirs_same_subdir_setting
    ;       interface_dirs_indep_subdir_setting
    ;       interface_dirs_installed_library
    ;       intermod_dirs_same_subdir_setting
    ;       intermod_dirs_indep_subdir_setting
    ;       intermod_dirs_installed_library
    ;       c_incl_dirs_same_subdir_setting
    ;       c_incl_dirs_indep_subdir_setting
    ;       c_incl_dirs_installed_library
    ;       c_incl_dirs_external
    ;       mer_lib_dirs_same_subdir_setting
    ;       mer_lib_dirs_indep_subdir_setting
    ;       mer_lib_dirs_installed_library
    ;       chosen_stdlib_dir
    ;       libgrade_install_check
    ;       order_make_by_timestamp
    ;       show_make_times
    ;       extra_library_header
    ;       restricted_command_line
    ;       env_type
    ;       host_env_type
    ;       system_env_type
    ;       target_env_type

    % Miscellaneous Options
    ;       filenames_from_stdin
    ;       typecheck_ambiguity_warn_limit
    ;       typecheck_ambiguity_error_limit
    ;       target_arch
    ;       local_module_id
    ;       analysis_file_cache_dir
    ;       default_globals
            % If set to "yes", default_globals tells the main body of
            % handle_options.m that it is constructing the *default* globals,
            % after the initial construction of the *intended* globals failed.
    ;       compiler_sufficiently_recent
            % This option is used to test that the compiler is sufficiently
            % recent when no other test can easily be constructed in
            % configure.in.

    ;       experiment
    ;       experiment1
    ;       experiment2
    ;       experiment3
    ;       experiment4
    ;       experiment5
            % These options are provided for use by implementors who want to
            % compare a new way of doing something with the old way.
            % The idea is that the code that switches between the two ways
            % should consult one (or more) of these options and make its
            % decision accordingly.
            %
            % Experiment[1-5] are booleans; experiment itself is a string.
            %
            % The intention is that most use of these options is
            % within developer workspaces, with rare examples of code
            % using some of these options being committed, but only
            % for short lengths of time (a week or two at most;
            % enough for all members of a team to try out the experiment).
            %
            % Of course, a developer could always create a purpose-specific
            % option to control their code, but adding an option requires
            % recompiling most of the modules in the compiler. Having these
            % options permanently here should reduce the need for that.

    ;       allow_ho_insts_as_modes
    ;       ignore_par_conjunctions
    ;       control_granularity
    ;       distance_granularity
    ;       implicit_parallelism
    ;       feedback_file
    ;       par_loop_control
    ;       par_loop_control_keep_tail_rec.

:- type option_table == option_table(option).
:- type maybe_option_table == maybe_option_table(option).

%---------------------------------------------------------------------------%

:- type option_category
    --->    oc_help
            % Options that call for the output of help text.

    ;       oc_cmdline
            % Options that manipulate the command line itself.

    ;       oc_warn_dodgy
            % Warnings about code that is possibly incorrect.
    ;       oc_warn_perf
    ;       oc_warn_perf_c
            % Warnings about code that probably could be faster.
    ;       oc_warn_style
    ;       oc_warn_style_c
            % Warnings about programming style.
    ;       oc_warn_ctrl
            % Options that *control* warnings.
            % XXX Split into subparts, one for each of oc_warn_*
            % that some now-oc_warn_ctrl option controls.
            % This should enable us to put the documentation of e.g.
            % inform_incomplete_switch_threshold, immediately after
            % the documentation of inform_incomplete_switch.
    ;       oc_warn_halt
            % Options that specify when warnings should treated as errors.
    ;       oc_inform
            % Requests for information.
    ;       oc_verbosity
            % Options that users can use to control how many progress updates
            % they want the compiler to give them.
    ;       oc_verb_dev
            % Developer-only kinds of oc_verbosity options.
    ;       oc_verb_dbg
            % Oc_verbosity options intended only for use by developers
            % to debug the compiler.
    ;       oc_opmode
            % Options that are used only to select an invocation's op_mode.
    ;       oc_diag_gen
            % Options for controlling diagnostics generally.
            % One class tells the compiler either under what circumstances
            % it should generate diagnostic output; another class specifies
            % how diagnostic output should be presented.
    ;       oc_diag_color
            % Options for controlling color in diagnostics.
    ;       oc_diag_int
            % Internal-use-only options for controlling diagnostics.
    ;       oc_mdb
            % Options that control how the compiler prepares for mdb debugging.
    ;       oc_mdb_dev
            % Developer-only options about mdb debugging.
            % XXX Some of these may be internal-use-only, not intended
            % even for developers.
    ;       oc_tracegoal
            % Options that control trace goals.
    ;       oc_mdprof
            % Options that control deep profiling.
    ;       oc_output_mod
            % Options that ask the compiler to modify some aspect
            % of the generated target code.
    ;       oc_output_dev
            % Developer-only options that ask the compiler to modify some aspect
            % of the generated target code.
    ;       oc_file_req
            % Options that request the compiler to generate files
            % containing information derived from the module being compiled.
    ;       oc_make
            % Options controlling mmc --make.
    ;       oc_semantics
            % Options that specify which semantics variant to use.
    ;       oc_infer
            % Options that tell the compiler what to infer.
    ;       oc_grade
            % Options that affect binary compatibility.
            % This category should subdivided into separate subcategories
            % for mdb, ssd, mprof, mdprof etc, but only after moving
            % the non-grade options out of the category altogether.
    ;       oc_internal
            % Options for compiler use only, that should not be used
            % even by developers.
    ;       oc_opt_hh
    ;       oc_opt_hh_exp
            % HLDS->HLDS optimizations. The _exp suffix indicates that
            % the optimization is experimental.
    ;       oc_opt_hlm
    ;       oc_opt_hm
    ;       oc_opt_hl
            % HLDS->{LLDS,MLDS}, HLDS->LLDS and HLDS->MLDS optimizations.
    ;       oc_opt_ll
    ;       oc_opt_mm
            % MLDS->MLDS and LLDS->LLDS optimizations.
    ;       oc_opt_lc
            % LLDS-> C optimizations. (There are no MLDS->C optimizations.)
    ;       oc_opt_ctrl
            % Options that control optimization levels.
    ;       oc_trans_opt
            % Options that control the operation of transitive intermodule
            % optimization.
    ;       oc_target_comp
    ;       oc_target_c
    ;       oc_target_java
    ;       oc_target_csharp
            % Options that control how the target language files we generate
            % are further compiled.
            % Subdivided for C, Java and C#.
    ;       oc_link_c
    ;       oc_link_java
    ;       oc_link_csharp
    ;       oc_link_c_cs
    ;       oc_link_c_cs_j
            % Options that control how executables, or their equivalents
            % for some target languages, are generated.
            % Subdivided for C, Java and C#, and for the combinations
            % that actually apply to some option.
    ;       oc_buildsys
            % XXX Document me.
            % XXX We should separate search path options (the majority)
            % from everything else.
    ;       oc_search
            % XXX Document me.
    ;       oc_env
            % Options that tell the compiler something about the environment,
            % or platform, on which it is operating.
    ;       oc_dev_ctrl
            % Options developers can use to control what the compiler does.
    ;       oc_dev_debug
            % Options developers can use to debug compiler components.
    ;       oc_dev_dump
            % Options to control dumps of internal code representations.
    ;       oc_config
            % The results of autoconfiguration, or of executing
            % tools/configure_cross.
    ;       oc_mconfig
            % Options which are reserved for use by the Mercury.config file.
    ;       oc_analysis
            % Options for user control of program analyses.
    ;       oc_unused.
            % options that are now unused, and which are kept around
            % only for backward compatibility.
            % XXX Is this a good idea? If users want the effect of an
            % old option, then deleting the option and breaking their code
            % is less likely to mislead than keeping the option as a noop.

    % The job of this predicate is to provide an authoritative list
    % of all the option_categories for print_help.m. This is done by
    % calling solutions on the second mode.
    %
    % The first mode is unused; it is there only to ensure completeness.
    %
    % The int output is a never-used argument.
    %
:- pred option_categories(option_category, int).
:- mode option_categories(in, out) is det.
:- mode option_categories(out, out) is multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compute_grade.
:- import_module libs.shell_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module dir.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%

    % Help options.

optdb(oc_help,     help,                               bool(no),
    gen_help(['?', 'h'], "help", [], help_public,
        ["Print this usage message."])).
optdb(oc_help,     help_alt,                           bool(no),
    priv_help("help-alt", ["Print the usage message."])).
optdb(oc_help,     version,                            bool(no),
    help("version", ["Display the compiler version."])).

%---------------------------------------------------------------------------%

optdb(oc_cmdline,  filenames_from_stdin,               bool(no),
    help("filenames-from-stdin", [
        "Read then compile a newline terminated module name or",
        "file name from the standard input. Repeat this until EOF",
        "is reached. (This allows a program or user to interactively",
        "compile several modules without the overhead of process",
        "creation for each one.)"])).
optdb(oc_cmdline,  flags_file,                         file_special,
    alt_arg_help("flags-file", ["flags"], "file", [
        "Take options from the specified file, and handle them",
        "as if they were specified on the command line."])).

%---------------------------------------------------------------------------%

    % Warning and information-request options.
    %
    % IMPORTANT NOTE:
    % if you add any new warning options, or if you change the default
    % for an existing warning option to `yes', then you will need to modify
    % the handling of inhibit_warnings in special_handler, and if the change
    % affects a style warning, you will need to modify the handling of
    % inhibit_style_warnings as well.

    % XXX warn_non_contiguous_clauses should default to yes.

    % XXX warn_unused_imports is disabled by default until someone
    % removes all the unused imports from the compiler itself,
    % which is compiled with --halt-at-warn by default.
    % XXX The above comment is obsolete; warn_unused_imports is now
    % turned on by default in COMP_FLAGS.

    % Since warn_unused_interface_imports does *part* of the job
    % of warn_unused_imports, it is automatically turned off if
    % warn_unused_imports is turned on. It is also turned off when
    % generating interface files, because the presence of unused
    % imports in the interface of module A should not prevent the
    % testing of a module B that imports A.

%---------------------%

% Warnings about module-level issues.

optdb(oc_warn_dodgy, warn_nothing_exported,               bool(yes),
    help("warn-nothing-exported", [
        "Do not warn about modules which export nothing."])).
optdb(oc_warn_dodgy, warn_unused_imports,                 bool(no),
    help("warn-unused-imports", [
        "Warn about modules that are imported but not used."])).
optdb(oc_warn_dodgy, warn_unused_interface_imports,       bool(yes),
    % Not documented because its relationship with --warn-unused-imports
    % is too complicated for users (and maybe even developers ...).
    priv_help("warn-unused-interface-imports", [
        "Warn about modules that are imported in the interface",
        "but not used there."])).
optdb(oc_warn_dodgy, warn_interface_imports,              bool(yes),
    help("warn-interface-imports", [
        "Do not warn about modules imported in the interface, but",
        "which are not used in the interface."])).
optdb(oc_warn_dodgy, warn_interface_imports_in_parents,   bool(no),
    help("warn-interface-imports-in-parents", [
        "Warn about modules that are imported in the interface of",
        "a parent module, but not used in the interface of that module."])).
optdb(oc_warn_dodgy, warn_stdlib_shadowing,               bool(yes),
    help("warn-stdlib-shadowing", [
        "Do not generate warnings for module names that either duplicate",
        "the name of a module in the Mercury standard library, or contain",
        "a subsequence of name components that do so."])).

% Warnings about goal-level issues.

optdb(oc_warn_dodgy, warn_singleton_vars,                 bool(yes),
    alt_help("warn-singleton-variables",
            ["warn-singleton-vars"], [
        "Do not warn about variables which only occur once in a clause,",
        "but whose names do not start with an underscore."])).
optdb(oc_warn_dodgy, warn_repeated_singleton_vars,        bool(yes),
    alt_help("warn-repeated-singleton-variables",
            ["warn-repeated-singleton-vars"], [
        "Do not warn about variables which occur more than once in a clause,",
        "but whose names do start with an underscore."])).
optdb(oc_warn_dodgy, warn_unification_cannot_succeed,     bool(yes),
    help("warn-unification-cannot-succeed", [
        "Do not warn about unifications which cannot succeed."])).
optdb(oc_warn_dodgy, warn_known_bad_format_calls,         bool(yes),
    help("warn-known-bad-format-calls", [
        "Do not warn about calls to string.format or io.format that contain",
        "mismatches between the format string and the supplied values."])).
optdb(oc_warn_dodgy, warn_obsolete,                       bool(yes),
    help("warn-obsolete", [
        "Do not warn about calls to predicates and functions",
        "that have been marked as obsolete."])).
optdb(oc_warn_dodgy, warn_overlapping_scopes,             bool(yes),
    help("warn-overlapping-scopes", [
        "Do not warn about variables which occur in overlapping scopes."])).
optdb(oc_warn_dodgy, warn_suspected_occurs_check_failure, bool(yes),
    alt_help("warn-suspected-occurs-check-failure",
            ["warn-suspected-occurs-failure"], [
        "Do not warn about code that looks like it unifies a variable",
        "with a term that contains that same variable. Such code cannot",
        "succeed because it fails what is called the",
        "QUOTE", "`occurs check'."])).
optdb(oc_warn_dodgy, warn_suspicious_recursion,           bool(no),
    help("warn-suspicious-recursion", [
        "Warn about recursive calls which are likely to have problems,",
        "such as leading to infinite recursion."])).
optdb(oc_warn_dodgy, warn_unresolved_polymorphism,        bool(yes),
    help("warn-unresolved-polymorphism", [
        "Do not warn about unresolved polymorphism, which occurs when",
        "the type of a variable contains a type variable",
        "that is not bound to an actual type, even though it should be."])).
optdb(oc_warn_dodgy, warn_unused_args,                    bool(no),
    help("warn-unused-args", [
        "Warn about predicate arguments which are not used."])).

% Warnings about predicate determinism issues.

optdb(oc_warn_dodgy, warn_det_decls_too_lax,              bool(yes),
    help("warn-det-decls-too-lax", [
        "Do not warn about determinism declarations",
        "which could have been stricter."])).
optdb(oc_warn_dodgy, warn_inferred_erroneous,             bool(yes),
    help("warn-inferred-erroneous", [
        "Do not warn about procedures whose determinism is inferred erroneous",
        "but whose determinism declarations are laxer."])).

% Warnings about predicate pragma issues.

optdb(oc_warn_dodgy, warn_ambiguous_pragma,               bool(yes),
    alt_help("warn-ambiguous-pragmas",
            ["warn-ambiguous-pragma"], [
        "Do not warn about pragmas that do not specify whether",
        "they are for a predicate or a function, even when there is both",
        "a predicate and a function with the given name and arity."])).
optdb(oc_warn_dodgy, warn_potentially_ambiguous_pragma,   bool(no),
    alt_help("warn-potentially-ambiguous-pragmas",
            ["warn-potentially-ambiguous-pragma"], [
        "Warn about pragmas that do not specify whether they are",
        "for a predicate or a function."])).
optdb(oc_warn_dodgy, warn_table_with_inline,              bool(yes),
    help("warn-table-with-inline", [
        "Do not warn about tabled procedures that also have",
        "a `pragma inline' declaration. (This combination does not work,",
        "because inlined copies of procedure bodies cannot be tabled."])).

% Warnings about other predicate-level issues.

optdb(oc_warn_dodgy, warn_stubs,                          bool(yes),
    help("warn-stubs", [
        "Do not warn about procedures for which there are no clauses.",
        "Note that this option is meaningful only if",
        "the `--allow-stubs' option is enabled."])).
optdb(oc_warn_dodgy, warn_non_term_special_preds,         bool(yes),
    help("warn-non-term-special-preds", [
        "Do not warn about types that have user-defined equality or",
        "comparison predicates that cannot be proved to terminate.",
        "This option is meaningful only if",
        "termination analysis is enabled."])).
optdb(oc_warn_dodgy, warn_non_stratification,             bool(no),
    help("warn-non-stratification", [
        "Warn about possible non-stratification",
        "of the predicates and/or functions in the module.",
        "Non-stratification occurs when a predicate or function can call",
        "itself through negation through some path in its call graph."])).

% Warnings about issues with insts.

optdb(oc_warn_dodgy, warn_insts_without_matching_type,    bool(yes),
    help("warn-insts-without-matching-type", [
        "Do not warn about insts that are not consistent",
        "with any of the types in scope."])).
optdb(oc_warn_dodgy, warn_insts_with_functors_without_type, bool(no),
    help("warn-insts-with-functors-without-type", [
        "Warn about insts that do specify functors, but do not specify",
        "what type they are for."])).

% Warnings about issues with files.

optdb(oc_warn_dodgy, warn_undefined_options_variables,    bool(yes),
    alt_help("warn-undefined-options-variables",
            ["warn-undefined-options-vars"], [
        "Do not warn about references to undefined variables",
        "in options files with `--make'."])).
optdb(oc_warn_dodgy, warn_missing_opt_files,              bool(yes),
    help("warn-missing-opt-files", [
        "Do not warn about `.opt' files which cannot be opened."])).
optdb(oc_warn_dodgy, warn_missing_trans_opt_files,        bool(no),
    help("warn-missing-trans-opt-files", [
        "Warn about `.trans_opt' files which cannot be opened."])).
optdb(oc_warn_dodgy, warn_missing_trans_opt_deps,         bool(yes),
    help("warn-missing-trans-opt-deps", [
        "Do not generate a warning when the information required",
        "to allow `.trans_opt' files to be read when creating other",
        "`.trans_opt' files has been lost. The information can be",
        "recreated by running `mmake <mainmodule>.depend'"])).

%---------------------%

optdb(oc_warn_perf,  warn_accumulator_swaps,              bool(yes),
    help("warn-accumulator-swaps", [
        "Do not warn about argument order rearrangement caused",
        "by `--introduce-accumulators'."])).
optdb(oc_warn_perf,  warn_unneeded_final_statevars,       bool(yes),
    help("warn-unneeded-final-statevars", [
        "Do not warn about !:S state variables in clause heads",
        "whose value will always be the same as !.S."])).
optdb(oc_warn_perf,  warn_unneeded_final_statevars_lambda, bool(yes),
    help("warn-unneeded-final-statevars-lambda", [
        "Do not warn about !:S state variables in lambda expressions",
        "whose value will always be the same as !.S."])).
optdb(oc_warn_perf,  warn_obvious_non_tail_recursion,     bool(no),
    help("warn-obvious-non-tail-recursion", [
        "Warn about recursive calls that are not tail calls",
        "even if they obviously cannot be tail calls,",
        "because they are followed by other recursive calls."])).
% These are the internal options that implement
% --warn-non-tail-recursion.
optdb(oc_warn_perf,  warn_non_tail_recursion_self,        bool(no),
    priv_help("warn-non-tail-recursion-self", [
        "Warn about any self recursive calls that are not tail
        recursive."])).
optdb(oc_warn_perf,  warn_non_tail_recursion_mutual,      bool(no),
    priv_help("warn-non-tail-recursion-mutual", [
        "Warn about any mutually recursive calls that are not",
        "tail recursive."])).
optdb(oc_warn_perf_c, warn_non_tail_recursion,          maybe_string_special,
    arg_help("warn-non-tail-recursion", "type", [
        "Warn about recursive calls that are not tail calls,",
        "<type> may be ""self"", ""self-and-mutual"" or ""none""."])).

%---------------------%

% Warnings about dead code.

optdb(oc_warn_style, warn_dead_preds,                     bool(no),
    alt_help("warn-dead-predicates",
            ["warn-dead-preds"], [
        "Warn about predicates that have no procedures which are",
        "ever called."])).
optdb(oc_warn_style, warn_dead_procs,                     bool(no),
    alt_help("warn-dead-procedures",
            ["warn-dead-procs"], [
        "Warn about procedures which are never called."])).

% Warnings about simple style mistakes.

optdb(oc_warn_style, warn_simple_code,                    bool(yes),
    help("warn-simple-code", [
        "Do not warn about constructs which are so simple",
        "that they are likely to be programming errors."])).
optdb(oc_warn_style, inform_ite_instead_of_switch,        bool(no),
    help("inform-ite-instead-of-switch", [
        "Generate informational messages for if-then-elses that could be",
        "replaced by switches."])).
optdb(oc_warn_style, inform_incomplete_switch,            bool(no),
    help("inform-incomplete-switch", [
        "Generate informational messages for switches that do not cover",
        "all the function symbols that the switched-on variable could be",
        "bound to."])).
optdb(oc_warn_style_c, inform_incomplete_switch_threshold, int(0),
    arg_help("inform-incomplete-switch-threshold", "N", [
        "Have the --inform-incomplete-switch option generate its messages",
        "only for switches that *do* cover at least N% of the function",
        "symbols that the switched-on variable could be bound to."])).
optdb(oc_warn_style, warn_duplicate_calls,                bool(no),
    help("warn-duplicate-calls", [
        "Warn about multiple calls to a predicate with the",
        "same input arguments."])).
optdb(oc_warn_style, warn_redundant_coerce,               bool(yes),
    help("warn-redundant-coerce", [
        "Do not warn about redundant type coercions."])).

% Warnings about state vars.

optdb(oc_warn_style, warn_state_var_shadowing,            bool(yes),
    help("warn-state-var-shadowing", [
        "Do not warn about one state variable shadowing another."])).
optdb(oc_warn_style, warn_unneeded_initial_statevars,     bool(yes),
    help("warn-unneeded-initial-statevars", [
        "Do not warn about state variables in clause heads",
        "that could be ordinary variables."])).
optdb(oc_warn_style, warn_unneeded_initial_statevars_lambda, bool(yes),
    help("warn-unneeded-initial-statevars-lambda", [
        "Do not warn about state variables in the heads of lambda expressions",
        "that could be ordinary variables."])).

% Warnings about I/O predicates.

optdb(oc_warn_style, warn_implicit_stream_calls,          bool(no),
    help("warn-implicit-stream-calls", [
        "Warn about calls to I/O predicates that could take explicit",
        "stream arguments, but do not do so."])).
optdb(oc_warn_style, warn_unknown_format_calls,           bool(no),
    help("warn-unknown-format-calls", [
        "Warn about calls to string.format, io.format or",
        "stream.string_writer.format for which the compiler cannot tell",
        "whether there are any mismatches between the format string and",
        "the supplied values."])).

% Warnings about predicate level issues.

optdb(oc_warn_style, warn_can_fail_function,              bool(no),
    help("warn-can-fail-function", [
        "Warn about functions that can fail."])).
optdb(oc_warn_style, warn_unneeded_mode_specific_clause,  bool(yes),
    help("warn-unneeded-mode-specific-clause", [
        "Do not warn about clauses that specify the modes of their arguments",
        "unnecessarily."])).

% Warnings about missing order.

optdb(oc_warn_style, warn_unsorted_import_blocks,         bool(no),
    alt_help("warn-unsorted-import-blocks",
            ["warn-unsorted-import-block"], [
        "Warn about two import and/or use declarations on the same line,",
        "or if a sequence of such declarations on consecutive lines",
        "are not sorted on module name."])).
optdb(oc_warn_style, warn_inconsistent_pred_order_clauses, bool(no),
    alt_help("warn-inconsistent-pred-order-clauses",
            ["warn-inconsistent-pred-order"], [
        "Generate a warning if the order of the definitions does not match",
        "the order of the declarations for either the exported predicates",
        "and functions of the module, or for the nonexported predicates",
        "and functions of the module. Applies for definitions by",
        "Mercury clauses."])).
optdb(oc_warn_style, warn_inconsistent_pred_order_foreign_procs, bool(no),
    help("warn-inconsistent-pred-order-foreign-procs", [
        "Generate a warning if the order of the definitions does not match",
        "the order of the declarations for either the exported predicates",
        "and functions of the module, or for the nonexported predicates",
        "and functions of the module. Applies for definitions by either",
        "Mercury clauses or foreign_proc pragmas."])).

% Warnings about missing contiguity.

optdb(oc_warn_style, warn_non_contiguous_decls,           bool(yes),
    help("warn-non-contiguous-decls", [
        "Do not generate a warning if the mode declarations of a",
        "predicate or function don't all immediately follow its",
        "predicate or function declaration."])).
optdb(oc_warn_style, warn_non_contiguous_clauses,         bool(no),
    help("warn-non-contiguous-clauses", [
        "Generate a warning if the clauses of a predicate or",
        "function are not contiguous."])).
optdb(oc_warn_style, warn_non_contiguous_foreign_procs,   bool(no),
    help("warn-non-contiguous-foreign-procs", [
        "Generate a warning if the clauses and foreign_procs of a",
        "predicate or function are not contiguous."])).
optdb(oc_warn_style_c, allow_non_contiguity_for, accumulating([]),
    arg_help("allow-non-contiguity-for", "name1,name2,...", [
        "Allow the clauses (or, with --warn-non-contiguous-foreign-procs,",
        "the clauses and/or foreign_proc pragmas) of the named predicates",
        "and/or functions to be intermingled with each other, but not",
        "with those or any other predicates or functions. This option",
        "may be specified more than once, with each option value",
        "specifying a distinct set of predicates and/or function names",
        "that may be intermingled. Each name must uniquely specify",
        "a predicate or a function."])).

% Warnings about foreign code.

optdb(oc_warn_style, warn_suspicious_foreign_code,        bool(no),
    help("warn-suspicious-foreign-code", [
        "Warn about possible errors in the bodies of foreign code",
        "pragmas."])).
optdb(oc_warn_style, warn_suspicious_foreign_procs,       bool(no),
    help("warn-suspicious-foreign-procs", [
        "Warn about possible errors in the bodies of foreign",
        "procedures."])).

%---------------------%

optdb(oc_warn_ctrl,  inhibit_warnings,                    bool_special,
    short_help('w', "inhibit-warnings", [],
        ["Disable all warning messages."])).
optdb(oc_warn_ctrl,  inhibit_style_warnings,              bool_special,
    help("inhibit-style-warnings",
        ["Disable all warning messages about programming style."])).
optdb(oc_warn_ctrl,  warn_only_one_format_string_error,   bool(yes),
    % XXX This options should be replaced by a new one named
    % warn_all_format_string_errors, which the negation of this one.
    help("warn-only-one-format-string-error", [
        "If a format string has more one than mismatch with the supplied",
        "values, generate a warning for all mismatches, not just the first.",
        "(The later mismatches may be avalanche errors caused by earlier",
        "mismatches.)"])).
optdb(oc_warn_ctrl,  warn_smart_recompilation,            bool(yes),
    % XXX Private until smart recompilation works and is announced.
    priv_help("warn-smart-recompilation", [
        "Disable warnings from the smart recompilation system."])).
optdb(oc_warn_ctrl,  warn_up_to_date,                     bool(yes),
    help("warn-up-to-date", [
        "Do not warn if targets specified on the command line",
        "with", "QUOTE", "`mmc --make'", "are already up-to-date."])).

optdb(oc_inform,     inform_inferred,                     bool_special,
    help("inform-inferred", [
        "Do not print inferred types or modes."])).
optdb(oc_inform,     inform_inferred_types,               bool(yes),
    help("inform-inferred-types", [
        "Do not print inferred types."])).
optdb(oc_inform,     inform_inferred_modes,               bool(yes),
    help("inform-inferred-modes", [
        "Do not print inferred modes."])).
optdb(oc_inform,     inform_incomplete_color_scheme,      bool(no),
    priv_help("inform-incomplete-color-scheme", [
        "Report if the argument if either the value of the",
        "--color-scheme option, or the value of MERCURY_COLOR_SCHEME",
        "environment variable, does not specify a color for some role."])).
optdb(oc_inform,     inform_suboptimal_packing,           bool(no),
    help("inform-suboptimal-packing", [
        "Generate messages if the arguments of a data constructor",
        "could be packed more tightly if they were reordered."])).

optdb(oc_warn_halt,  halt_at_warn,                        bool(no),
    help("halt-at-warn", [
        "This option causes the compiler to treat all warnings",
        "as if they were errors when generating target code.",
        "This means that if the compiler issues any warning,",
        "it will not generate target code; instead, it will",
        "return a non-zero exit status."])).
optdb(oc_warn_halt,  halt_at_warn_make_int,               bool(no),
    alt_help("halt-at-warn-make-interface",
            ["halt-at-warn-make-int"], [
        "This option causes the compiler to treat all warnings",
        "as if they were errors when generating an interface file",
        "(a .int, .int0, .int2 or .int3 file). This means that",
        "if the compiler issues any warnings at that time,",
        "it will not generate the interface file; instead,",
        "it will return a non-zero exit status."])).
optdb(oc_warn_halt,  halt_at_warn_make_opt,               bool(no),
    help("halt-at-warn-make-opt", [
        "This option causes the compiler to treat all warnings",
        "as if they were errors when generating an optimization file",
        "(.opt or .trans_opt file.) This means that if the compiler",
        "issues any warnings at that time, it will not generate the",
        "optimization file; instead, it will return a non-zero",
        "exit status."])).
optdb(oc_warn_halt,  halt_at_syntax_errors,               bool(no),
    help("halt-at-syntax-errors", [
        "This option causes the compiler to halt immediately",
        "after syntax checking and not do any semantic checking",
        "if it finds any syntax errors in the program."])).
optdb(oc_warn_halt,  halt_at_invalid_interface,           bool(yes),
    % --halt-at-invalid-interface is a temporary developer-only option.
    help("halt-at-invalid-interface", [
        "This option operates when the compiler is invoked with the",
        "--make--interface option to generate .int and .int2 files",
        "for one or more modules. In its default setting,",
        "--halt-at-invalid-interface, it causes the compiler to check",
        "the consistency of those parts of each of those modules",
        "that are intended to end up in the .int and .int2 files.",
        "If these checks find any problems, the compiler will stop",
        "without generating those files after printing an error message",
        "for each problem. Users can prevent this behavior,",
        "and thus allow the generation of invalid interface files,",
        "by specifying --no-halt-at-invalid-interface."])).
optdb(oc_warn_halt,  halt_at_auto_parallel_failure,       bool(no),
    priv_help("halt-at-auto-parallel-failure", [
        "This option causes the compiler to halt if it cannot perform",
        "an auto-parallelization requested by a feedback file."])).

%---------------------------------------------------------------------------%

    % Options that control diagnostics.

% XXX Internal/external mismatch: verbose_errors vs verbose-error-messages
optdb(oc_diag_gen, verbose_errors,                    bool(no),
    short_help('E', "verbose-error-messages", [], [
        "Explain error messages. Asks the compiler to give you a more",
        "detailed explanation of any errors it finds in your program."])).
optdb(oc_diag_gen, reverse_error_order,               bool(no),
    help("reverse-error-order", [
        "Print error messages in descending order of their line numbers,",
        "instead of the usual ascending order. This is useful if you want",
        "to work on the last errors in a file first."])).
optdb(oc_diag_gen, max_error_line_width,              maybe_int(yes(79)),
    arg_help("max-error-line-width", "n", [
        "Set the maximum width of an error message line to <n> characters",
        "(unless a long single word forces the line over this limit).",
        "Specifying --no-max-error-line-width removes the limit."])).
optdb(oc_diag_gen, limit_error_contexts,              accumulating([]),
    arg_help("limit-error-contexts",
            "filename:minline1-maxline1,minline2-maxline2", [
        "Print errors and warnings for the named file only when their",
        "line number is in one of the specified ranges.",
        "The minimum or maximum line number in each range may be omitted,",
        "in which case the range has no lower or upper bound respectively.",
        "Multiple --limit-error-context options accumulate.",
        "If more than one --limit-error-context option is given for",
        "the same file, only the last one will have an effect.",
        "If the file name and colon are missing, the limit will apply",
        "to all files."])).
optdb(oc_diag_gen, error_files_in_subdir,             bool(no),
    help("error-files-in-subdir", [
        "This option causes", "QUOTE", "`mmc --make'",
        "to put .err files into the `Mercury' subdirectory",
        "instead of the current directory.",
        "(This option has no effect on Mmake.)"])).
optdb(oc_diag_gen, std_int_file_not_written_msgs,     bool(no),
    % We use this option to eliminate the need for a .int_err_exp file for the
    % -use-subdir case for every test in the tests/invalid_make_int
    % directory.
    priv_help("std-int-file-not-written-msgs", [
        "Standardize messages about interface files not being written",
        "by omitting any directory name components from file names."])).
optdb(oc_diag_gen, typecheck_ambiguity_warn_limit,    int(50),
    arg_help("typecheck-ambiguity-warn-limit", "n", [
        "Set the number of type assignments required to generate a",
        "warning about highly ambiguous overloading to <n>."])).
optdb(oc_diag_gen, typecheck_ambiguity_error_limit,   int(3000),
    arg_help("typecheck-ambiguity-error-limit", "n", [
        "Set the number of type assignments required to generate an error",
        "about excessively ambiguous overloading to <n>. If this limit is",
        "reached, the typechecker will not process the predicate or",
        "function any further."])).

optdb(oc_diag_color, color_diagnostics,                bool_special,
    alt_help("color-diagnostics",
            ["colour-diagnostics"], [
        "Disable the use of colors in diagnostic messages. Please see",
        "the section named \"Enabling the use of color\" section in the",
        "Mercury Users's Guide for details."])).
optdb(oc_diag_color, config_default_color_diagnostics, bool(yes),
    % This option should be used only by the configure script.
    priv_alt_help("config-default-color-diagnostics",
            ["config-default-colour-diagnostics"], [
        "The default value of the --color-diagnostics option,",
        "set by the configure script."])).
optdb(oc_diag_int,   color_diagnostics_is_set,         bool(no), no_help).
optdb(oc_diag_int,   color_diagnostics_is_set_to,      bool(no), no_help).
optdb(oc_diag_int,   use_color_diagnostics,            bool(no), no_help).
optdb(oc_diag_color, color_scheme,                     string_special,
    alt_arg_help("color-scheme",
            ["colour-scheme"], "ColorScheme", [
        "Specify the color scheme to use for diagnostics, if the use of",
        "color in diagnostics is enabled. For information about how the",
        "compiler uses colors in diagnostic messages, and about the",
        "syntax of color scheme specifications, please see the",
        "section named \"Color schemes\" in the Mercury user's Guide",
        "for the details."])).
optdb(oc_diag_int,  color_scheme_envvar,          string_special,
    priv_arg_help("color-scheme-envvar", "ColorScheme", [])).
optdb(oc_diag_int,  color_scheme_set_by,          string("default"), no_help).
optdb(oc_diag_int,  color_scheme_set_to,          string("light16"), no_help).
optdb(oc_diag_int,  ignore_color_scheme_envvar,        bool(no),
    % This option should be used only by our test suite.
    priv_help("ignore-color-scheme-envvar", [
        "Ignore the --color-scheme-envvar option."])).
optdb(oc_diag_int,  set_color_subject,                 string(""), no_help).
optdb(oc_diag_int,  set_color_correct,                 string(""), no_help).
optdb(oc_diag_int,  set_color_incorrect,               string(""), no_help).
optdb(oc_diag_int,  set_color_inconsistent,            string(""), no_help).
optdb(oc_diag_int,  set_color_hint,                    string(""), no_help).

%---------------------------------------------------------------------------%

    % Verbosity options.

optdb(oc_verbosity, verbose,                           bool(no),
    short_help('v', "verbose", [], [
        "Output progress messages at each stage in the compilation."])).
optdb(oc_verbosity, very_verbose,                      bool(no),
    short_help('V', "very-verbose", [], [
        "Output very verbose progress messages."])).
optdb(oc_verbosity, statistics,                        bool(no),
    short_help('S', "statistics", [], [
        "Output messages about the compiler's time/space usage.",
        "At the moment this option implies `--no-trad-passes', so you get",
        "information at the boundaries between phases of the compiler."])).
optdb(oc_verb_dev,  detailed_statistics,               bool(no),
    % The only sensible way to use --detailed-statistics, based on
    % --very-verbose, is implemented automatically in handle_options,
    % so users shouldn't need to be aware of it.
    priv_help("detailed-statistics", [
        "Output more detailed messages about the compiler's",
        "time/space usage."])).
optdb(oc_verb_dev,  benchmark_modes,                   bool(no),
    priv_help("benchmark-modes", [
        "Benchmark mode analysis, including its experimental version,",
        "if it is enabled."])).
optdb(oc_verb_dev,  benchmark_modes_repeat,            int(1),
    priv_arg_help("benchmark-modes-repeat", "num_repeats", [
        "The number of times to execute mode analysis, if",
        "--benchmark-modes is enabled."])).
optdb(oc_verbosity, verbose_make,                      bool(yes),
    help("verbose-make", [
        "Disable messages about the progress of builds when using",
        "QUOTE", "`mmc --make'."])).
optdb(oc_verbosity, output_compile_error_lines,        maybe_int(yes(100)),
    arg_help("output-compile-error-lines", "n", [
        "With `--make', output the first <n> lines of the `.err'",
        "file after compiling a module (default: 100).",
        "Specifying --no-output-compile-error-lines removes the limit."])).
optdb(oc_verbosity, verbose_recompilation,             bool(no),
    help("verbose-recompilation", [
        "When using `--smart-recompilation', output messages",
        "explaining why a module needs to be recompiled."])).
optdb(oc_verbosity, find_all_recompilation_reasons,    bool(no),
    help("find-all-recompilation-reasons", [
        "Find all the reasons why a module needs to be recompiled,",
        "not just the first. Implies `--verbose-recompilation'."])).
optdb(oc_verbosity, verbose_commands,                  bool(no),
    help("verbose-commands", [
        "Output each external command before it is run.",
        "Note that some commands will only be printed with `--verbose'."])).
optdb(oc_verbosity, show_pred_movability,              accumulating([]),
    alt_arg_help("show-pred-moveability",
            ["show-pred-movability"], "pred_or_func_name", [
        "Write out a short report on the effect of moving the code of",
        "the named predicate or function (or the named several predicates",
        "and/or functions, if the option is given several times)",
        "to a new module. This includes listing the other predicates",
        "and/or functions that would have to be moved with them, and",
        "whether the move would cause unwanted coupling between",
        "the new module and the old."])).

optdb(oc_verb_dbg,  inform_ignored_pragma_errors,       bool(no),
    priv_help("inform-ignored-pragma-errors", [
        "Print an informational message for each otherwise-ignored error",
        "that reports an inability to find the procedure that a pragma",
        "refers to."])).
optdb(oc_verb_dbg,  inform_generated_type_spec_pragmas, bool(no),
    priv_help("inform-generated-type-spec-pragmas", [
        "Print an informational message for each type_spec pragma that",
        "the compiler generates to implement a type_spec_constrained_pred",
        "pragma."])).
optdb(oc_verb_dbg,  report_cmd_line_args,               bool(no),
    help("report-cmd-line-args", [
        "Report the command line arguments."])).
optdb(oc_verb_dbg,  report_cmd_line_args_in_doterr,     bool(no),
    help("report-cmd-line-args-in-doterr", [
        "Report the command line arguments for compilations whose output",
        "mmake normally redirects to a `.err' file."])).
optdb(oc_verb_dbg,  proc_size_statistics,               string(""),
    arg_help("proc-size-statistics", "filename", [
        "Append information about the size of each procedure in the",
        "module in terms of goals and variables to the end of the",
        "named file."])).
optdb(oc_verb_dbg,  inst_statistics,                    string(""),
    priv_arg_help("inst-statistics", "filename", [
        "Append a count of each kind of insts in the procedures in the",
        "module to the end of the named file."])).
optdb(oc_verb_dbg,  print_error_spec_id,                bool(no),
    priv_help("print-error-spec-id", [
        "After each error message is printed, print its id, which",
        "by convention is the $pred of the code that constructs it."])).
optdb(oc_verb_dbg,  debug_types,                        bool(no),
    priv_short_help('T', "debug-types", [], [
        "Output detailed debugging traces of type checking.",
        "Effective only with --trace-flag type_checkpoint."])).
optdb(oc_verb_dbg,  debug_types_pred_name,              accumulating([]),
    priv_arg_help("debug-types-pred-name", "pred_or_func_name", [
        "Output detailed debugging traces of type checking",
        "only for predicates and functions named by one of these options."])).
optdb(oc_verb_dbg,  debug_type_rep,                     bool(no),
    help("debug-type-rep", [
        "Output debugging traces of type representation choices."])).
optdb(oc_verb_dbg,  debug_modes,                        bool(no),
    short_help('N', "debug-modes", [], [
        "Output debugging traces of the mode checking."])).
optdb(oc_verb_dbg,  debug_modes_verbose,                bool(no),
    help("debug-modes-verbose", [
        "Output detailed debugging traces of the mode checking."])).
optdb(oc_verb_dbg,  debug_modes_minimal,                bool(no),
    help("debug-modes-minimal", [
        "Output only minimal debugging traces of the mode checking."])).
optdb(oc_verb_dbg,  debug_modes_statistics,             bool(no),
    help("debug-modes-statistics", [
        "Output statistics after each step of mode checking."])).
optdb(oc_verb_dbg,  debug_modes_delay_vars,             bool(yes),
    priv_help("debug-modes-delay-vars", [
        "Output info about the variables involved in delayed goals."])).
optdb(oc_verb_dbg,  debug_modes_goal_ids,               bool(yes),
    priv_help("debug-modes-goal-ids", [
        "Output the id of the goal at all mode debug checkpoints."])).
optdb(oc_verb_dbg,  debug_modes_pred_id,                int(-1),
    arg_help("debug-modes-pred-id", "n", [
        "With `--debug-modes', restrict the debugging traces to the",
        "mode checking of the predicate or function with the specified",
        "pred id."])).
optdb(oc_verb_dbg,  debug_mode_constraints,             bool(no),
    priv_help("debug-mode-constraints", [
        "Output detailed debugging traces of the `--prop-mode-constraints'",
        "option."])).
optdb(oc_verb_dbg,  debug_det,                          bool(no),
    alt_help("debug-determinism", ["debug-det"], [
        "Output detailed debugging traces of determinism analysis."])).
optdb(oc_verb_dbg,  debug_common_struct_preds,          string(""),
    priv_arg_help("debug-common-struct-preds", "predids", [
        "Limit common struct optimization to the preds with",
        "the given ids."])).
optdb(oc_verb_dbg,  debug_closure,                      bool(no),
    % This can be make public together with the '--analyse-closures' option.
    priv_help("debug-closure", [
        "Output detailed debugging traces of the closure analysis."])).
optdb(oc_verb_dbg,  debug_term,                         bool(no),
    % The new termination analyser is currently a work-in-progress.
    priv_alt_help("debug-termination", ["debug-term"], [
        "Output detailed debugging traces of the termination2 analysis."])).
optdb(oc_verb_dbg,  debug_dead_proc_elim,               bool(no),
    priv_help("debug-dead-proc-elim", [
        "Output the needed-entity-map generated by dead procedure",
        "elimination."])).
optdb(oc_verb_dbg,  debug_higher_order_specialization,  bool(no),
    priv_help("debug-higher-order-specialization", [
        "Output messages about the procedure specializations done",
        "by higher_order.m."])).
optdb(oc_verb_dbg,  debug_pd,                           bool(no),
    help("debug-pd", [
        "Output detailed debugging traces of the partial",
        "deduction and deforestation process."])).
optdb(oc_verb_dbg,  debug_indirect_reuse,               bool(no),
    help("debug-indirect-reuse", [
        "Output detailed debugging traces of the indirect reuse pass of",
        "the `--structure-reuse' option."])).
optdb(oc_verb_dbg,  debug_trail_usage,                  bool(no),
    help("debug-trail-usage", [
        "Output detailed debugging traces of the `--analyse-trail-usage'",
        "option."])).
optdb(oc_verb_dbg,  debug_unneeded_code,                bool(no),
    priv_help("debug-unneeded-code", [
        "Print progress messages during the unneeded code elimination",
        "passes."])).
optdb(oc_verb_dbg,  debug_unneeded_code_pred_name,      accumulating([]),
    priv_arg_help("debug-unneeded-code-pred-name", "predname", [
        "Print the definition of <predname> at the start of each pass",
        "of the unneeded code elimination algorithm."])).
optdb(oc_verb_dbg,  debug_mm_tabling_analysis,          bool(no),
    priv_help("debug-mm-tabling-analysis", [])).
optdb(oc_verb_dbg,  debug_dep_par_conj,                 accumulating([]),
    priv_arg_help("debug-dep-par-conj", "n", [
        "Output detailed debugging traces during the dependent",
        "AND-parallelism transformation of the predicate with the given",
        "predicate id. Effective only with the right --trace-flags."])).
optdb(oc_verb_dbg,  debug_liveness,                     int(-1),
    arg_help("debug-liveness", "pred_id", [
        "Output detailed debugging traces of the liveness analysis",
        "of the predicate with the given predicate id."])).
optdb(oc_verb_dbg,  debug_stack_opt,                    int(-1),
    priv_arg_help("debug-stack-opt", "pred-id", [
        "Generate debug messages when performing stack slot optimization",
        "on the predicate with the given id."])).
optdb(oc_verb_dbg,  debug_code_gen_pred_id,             int(-1),
    priv_arg_help("debug-code-gen-pred-id", "n", [
        "Output detailed debugging traces of code generation for the",
        "predicate or function with the given pred id.",
        "Effectively only with the right trace flags."])).
optdb(oc_verb_dbg,  debug_opt,                          bool(no),
    help("debug-opt", [
        "Output detailed debugging traces of the optimization process."])).
optdb(oc_verb_dbg,  debug_opt_pred_id,                  accumulating([]),
    arg_help("debug-opt-pred-id", "n", [
        "Output detailed debugging traces of the optimization process",
        "only for the predicate/function with the specified pred id."])).
optdb(oc_verb_dbg,  debug_opt_pred_name,                accumulating([]),
    arg_help("debug-opt-pred-name", "name", [
        "Output detailed debugging traces of the optimization process",
        "only for the predicate/function with the specified name."])).
optdb(oc_verb_dbg,  debug_make,                         bool(no),
    help("debug-make", [
        "Output detailed debugging traces of the `--make' option."])).
optdb(oc_verb_dbg,  debug_intermodule_analysis,         bool(no),
    help("debug-intermodule-analysis", [
        "Output detailed debugging traces of the `--intermodule-analysis'",
        "option."])).

%---------------------------------------------------------------------------%

    % Opmode options. These are all mutually exclusive.

optdb(oc_opmode, only_opmode_generate_source_file_mapping, bool(no),
    short_help('f', "generate-source-file-mapping", [], [
        "Output the module name to file name mapping for the list",
        "of source files given as non-option arguments to mmc",
        "to `Mercury.modules'. This must be done before",
        "`mmc --generate-dependencies' if there are any modules",
        "for which the file name does not match the module name.",
        "If there are no such modules the mapping need not be",
        "generated."])).
optdb(oc_opmode, only_opmode_make,                     bool(no),
    short_help('m', "make", [], [
        "Treat the non-option arguments to `mmc' as files to make,",
        "rather than source files. Build or rebuild the specified files",
        "if they do not exist or are not up-to-date.",
        "Note that this option also enables `--use-subdirs'."])).
% NOTE part_opmode_rebuild should be only_opmode_rebuild, but make.*.m
% look up its value as an ordinary option, NOT as a part of the op_mode.
% In one place, we clear this option, but we do it effectively because
% we want a different op_mode for a recursive invocation of the compiler.
optdb(oc_opmode, part_opmode_rebuild,                  bool(no),
    short_help('r', "rebuild", [], [
        "Same as `--make', but always rebuild the target files",
        "even if they are up-to-date."])).
optdb(oc_opmode, only_opmode_invoked_by_mmc_make,      bool(no),
    priv_help("invoked-by-mmc-make", [
        "This option is only for internal use by the compiler.",
        "QUOTE", "`mmc --make'", "passes it as the first argument when",
        "compiling a module."])).
optdb(oc_opmode, only_opmode_generate_dependencies,    bool(no),
    % XXX This leaves out important details, such as .dv and .d files.
    short_help('M', "generate-dependencies", [], [
        "Output `Make'-style dependencies for the module",
        "and all of its dependencies to `<module>.dep'."])).
optdb(oc_opmode, only_opmode_generate_dependencies_ints, bool(no),
    help("generate-dependencies-ints", [
        "Does the same job as --generate-dependencies, but also",
        "outputs .int3, .int0, .int and .int2 files for all the modules",
        "in the program."])).
optdb(oc_opmode, only_opmode_generate_dependency_file, bool(no),
    help("generate-dependency-file", [
        "Output `Make'-style dependencies for the module",
        "to `<module>.d'."])).
optdb(oc_opmode, only_opmode_generate_standalone_interface, maybe_string(no),
    arg_help("generate-standalone-interface", "basename", [
        "Output a stand-alone interface.",
        "<basename> is used as the basename of any files generated for",
        "the stand-alone interface. (See the Stand-alone Interface",
        "chapter of the Mercury User's Guide for further details.)"])).
optdb(oc_opmode, only_opmode_make_short_interface,     bool(no),
    alt_help("make-short-interface", ["make-short-int"], [
        "Write the unqualified short interface to `<module>.int3'.",
        "This option should only be used by mmake."])).
optdb(oc_opmode, only_opmode_make_private_interface,   bool(no),
    alt_help("make-private-interface", ["make-priv-int"], [
        "Write the private interface to `<module>.int0'.",
        "This option should only be used by mmake."])).
optdb(oc_opmode, only_opmode_make_interface,           bool(no),
    short_help('i', "make-interface", ["make-int"], [
        "Write the module interface to `<module>.int',",
        "and write the short interface to `<module>.int2'",
        "This option should only be used by mmake."])).
optdb(oc_opmode, only_opmode_make_optimization_interface, bool(no),
    alt_help("make-optimization-interface",
            ["make-optimisation-interface", "make-opt-int"], [
        "Write inter-module optimization information to `<module>.opt'.",
        "This option should only be used by mmake."])).
optdb(oc_opmode, only_opmode_make_transitive_opt_interface, bool(no),
    alt_help("make-transitive-optimization-interface",
            ["make-transitive-optimisation-interface", "make-trans-opt"], [
        "Output transitive optimization information",
        "into the `<module>.trans_opt' file.",
        "This option should only be used by mmake."])).
optdb(oc_opmode, only_opmode_make_analysis_registry,   bool(no),
    priv_help("make-analysis-registry", [])).
optdb(oc_opmode, only_opmode_make_xml_documentation,   bool(no),
    short_help('x', "make-xml-documentation", ["make-xml-doc"], [
        "Output XML documentation of the module",
        "into the `<module>.xml' file.",
        "This option should only be used by mmake."])).
optdb(oc_opmode, only_opmode_convert_to_mercury,       bool(no),
    short_help('P', "convert-to-mercury",
            ["convert-to-Mercury", "pretty-print"], [
        "Convert to Mercury. Output to file `<module>.ugly'",
        "This option acts as a Mercury ugly-printer."])).
optdb(oc_opmode, only_opmode_typecheck_only,           bool(no),
    short_help('t', "typecheck-only", [], [
        "Just check that the code is syntactically correct and",
        "type-correct. Don't check modes or determinism,",
        "and don't generate any code."])).
optdb(oc_opmode, only_opmode_errorcheck_only,          bool(no),
    short_help('e', "errorcheck-only", [], [
        "Check the module for errors, but do not generate any code."])).
optdb(oc_opmode, only_opmode_target_code_only,         bool(no),
    short_help('C', "target-code-only", [], [
        "Generate target code (i.e. C code in `<module>.c',",
        "C# code in `<module>.cs', or Java code in",
        "`<module>.java'), but not object code."])).
optdb(oc_opmode, only_opmode_compile_only,             bool(no),
    short_help('c', "compile-only", [], [
        "Generate C code in `<module>.c' and object code in `<module>.o'",
        "but do not attempt to link the named modules."])).
optdb(oc_opmode, only_opmode_output_grade_string,      bool(no),
    help("output-grade-string", [
        "Compute the canonical string representing the currently",
        "selected grade, and print it on the standard output."])).
optdb(oc_opmode, only_opmode_output_stdlib_modules,    bool(no),
    help("output-stdlib-modules", [
        "Print to standard output the names of the modules in the",
        "Mercury standard library."])).
optdb(oc_opmode, only_opmode_output_stdlib_grades,     bool(no),
    help("output-stdlib-grades", [
        "Print to standard output the list of compilation grades in which",
        "the Mercury standard library is available with this compiler."])).
optdb(oc_opmode, only_opmode_output_library_install_grades, bool(no),
    alt_help("output-libgrades",
            ["output-library-install-grades"], [
        "Print to standard output the list of compilation grades in which",
        "a library to be installed should be built."])).
optdb(oc_opmode, only_opmode_output_cc,                bool(no),
    help("output-cc", [
        "Print to standard output the command used to invoke the",
        "C compiler."])).
optdb(oc_opmode, only_opmode_output_c_compiler_type,   bool(no),
    alt_help("output-c-compiler-type", ["output-cc-type"], [
        "Print the C compiler type to the standard output."])).
optdb(oc_opmode, only_opmode_output_cflags,            bool(no),
    help("output-cflags", [
        "Print to standard output the flags with which the C compiler",
        "will be invoked."])).
optdb(oc_opmode, only_opmode_output_c_include_directory_flags, bool(no),
    alt_help("output-c-include-directory-flags",
            ["output-c-include-dir-flags"], [
        "Print to standard output the flags that are passed to the",
        "C compiler to specify which directories to search for",
        "C header files. This includes the C header files from the",
        "standard library."])).
optdb(oc_opmode, only_opmode_output_grade_defines,     bool(no),
    help("output-grade-defines", [
        "Print to standard output the flags that are passed to the",
        "C compiler to define the macros whose values specify the",
        "compilation grade."])).
optdb(oc_opmode, only_opmode_output_csharp_compiler,   bool(no),
    help("output-csharp-compiler", [
        "Print to standard output the command used to invoke the C#",
        "compiler."])).
optdb(oc_opmode, only_opmode_output_csharp_compiler_type, bool(no),
    help("output-csharp-compiler-type", [
        "Print the C# compiler type to the standard output."])).
optdb(oc_opmode, only_opmode_output_target_arch,       bool(no),
    help("output-target-arch", [
        "Print the target architecture to the standard output."])).
optdb(oc_opmode, only_opmode_output_java_class_dir,    bool(no),
    alt_help("output-java-class-directory",
            ["output-class-directory", "output-java-class-dir",
            "output-class-dir"], [
        "Print to standard output the name of the directory in which",
        "generated Java class files will be placed."])).
optdb(oc_opmode, only_opmode_output_link_command,      bool(no),
    help("output-link-command", [
        "Print to standard output the command used to link executables."])).
optdb(oc_opmode, only_opmode_output_shared_lib_link_command, bool(no),
    help("output-shared-lib-link-command", [
        "Print to standard output the command used to link",
        "shared libraries"])).
optdb(oc_opmode, only_opmode_output_library_link_flags, bool(no),
    help("output-library-link-flags", [
        "Print to standard output the flags that are passed to linker",
        "in order to link against the current set of libraries.",
        "This includes the standard library, as well as any other",
        "libraries specified via the --ml option."])).
optdb(oc_opmode, only_opmode_output_optimization_options, bool(no),
    alt_help("output-optimization-options",
            ["output-opt-opts"], [
        "Print a list of the optimization at each optimization level."])).
optdb(oc_opmode, only_opmode_output_optimization_options_upto, int(-1),
    alt_arg_help("output-optimization-options-upto",
            ["output-opt-opts-upto"], "max_level", [
        "Print a list of the optimization at each optimization level",
        "up to the given level."])).

%---------------------------------------------------------------------------%

    % Options that control how the compiler prepares for mdb debugging.

optdb(oc_mdb,     trace_level,                      string("default"),
    % "--trace decl" is not documented, because it is for backwards
    % compatibility only. It is now equivalent to `--trace rep'.
    arg_help("trace", "{minimum, shallow, deep, rep, default}", [
        "Generate code that includes the specified level",
        "of execution tracing.",
        "See the Debugging chapter of the Mercury User's Guide",
        "for details."])).
optdb(oc_mdb,     exec_trace_tail_rec,              bool(no),
    help("exec-trace-tail-rec", [
        "Generate TAIL events for self-tail-recursive calls instead of",
        "EXIT events. This allows these recursive calls to reuse",
        "their parent call's stack frame, but it also means that",
        "the debugger won't have access to the contents of the reused",
        "stack frames"])).
optdb(oc_mdb,     trace_optimized,                  bool(no),
    alt_help("trace-optimized",
            ["trace-optimised"], [
        "Do not disable optimizations that can change the trace."])).
optdb(oc_mdb_dev, trace_prof,                       bool(no),
    % "--trace-prof" is not documented because it is intended
    % only for developers of the deep profiler.
    priv_help("trace-prof", [
        "Enable tracing of deep profiling service predicates."])).
optdb(oc_mdb,     event_set_file_name,              string(""),
    arg_help("event-set-file-name", "filename", [
        "Get the specification of user-defined events from <filename>."])).
optdb(oc_mdb,        delay_death,                      bool(yes),
    help("delay-death", [
        "When the trace level is `deep', the compiler normally",
        "preserves the values of variables as long as possible, even",
        "beyond the point of their last use, in order to make them",
        "accessible from as many debugger events as possible.",
        "However, it will not do so if the user specifies",
        "`--no-delay-death'. This may be necessary if without it,",
        "the stack frames of some procedures grow too big."])).
optdb(oc_mdb,        delay_death_max_vars,             int(1000),
    arg_help("delay-death-max-vars", "N", [
        "Delay the deaths of variables only when the number of variables",
        "in the procedure is no more than N. The default value is 1000."])).
optdb(oc_mdb,        stack_trace_higher_order,         bool(no),
    help("stack-trace-higher-order", [
        "Enable stack traces through predicates and functions with",
        "higher-order arguments, even if stack tracing is not",
        "supported in general."])).
    % I/O tabling is deliberately not documented. It is meant to be
    % switched on, with consistent parameters, in debugging grades,
    % and to be consistently switched off in non-debugging grades.
    % Inconsistent use of the options governing I/O tabling
    % can yield core dumps from the debugger, so these options
    % are for implementors only.
optdb(oc_mdb_dev, trace_table_io,                   bool(no),
    priv_help("trace-table-io", [
        "Enable the tabling of I/O actions, to allow the debugger",
        "to execute retry commands across I/O actions."])).
optdb(oc_mdb_dev, trace_table_io_only_retry,        bool(no),
    priv_help("trace-table-io-only-retry", [
        "Set up I/O tabling to support only retries across I/O",
        "actions, not the printing of actions or declarative",
        "debugging. This reduces the size of the I/O action table."])).
optdb(oc_mdb_dev, trace_table_io_states,            bool(no),
    priv_help("trace-table-io-states", [
        "When tabling I/O actions, table the io.state arguments",
        "together with the others. This should be required iff",
        "values of type io.state actually contain information."])).
optdb(oc_mdb_dev, trace_table_io_require,           bool(no),
    priv_help("trace-table-io-require", [
        "Require the tabling of I/O actions, i.e. generate an error",
        "if an I/O primitive does not have the tabled_for_io",
        "annotation."])).
optdb(oc_mdb_dev, trace_table_io_all,               bool(no),
    priv_help("trace-table-io-all", [
        "Table all I/O actions even in the absence of annotations.",
        "If a primitive has no annotation specifying the type of",
        "tabling required, deduce it from the values of the other",
        "annotations."])).
optdb(oc_mdb_dev,    suppress_trace,                   string(""),
    priv_arg_help("suppress-trace", "suppress-items,", [
        "Suppress the named aspects of the execution tracing system."])).
optdb(oc_mdb_dev,    force_disable_tracing,            bool(no),
    priv_help("force-disable-tracing", [
        "Force tracing to be set to trace level none.",
        "This overrides all other tracing/grade options.",
        "Its main use is to turn off tracing in the browser",
        "directory, even for .debug and .decldebug grades."])).
optdb(oc_mdb_dev,    force_disable_ssdebug,            bool(no),
    % This is a developer-only option:
    priv_help("force-disable-ssdebug", [
        "Disable ssdebug transformation even in ssdebug grades."])).

%---------------------------------------------------------------------------%

    % Options that control trace goals.

optdb(oc_tracegoal, trace_goal_flags,                 accumulating([]),
    arg_help("trace-flag", "keyword", [
        "Enable the trace goals that depend on the <keyword> trace flag."])).

%---------------------------------------------------------------------------%

    % Options that control profiling.

optdb(oc_mdprof, prof_optimized,                   bool(no),
    alt_help("profile-optimized",
            ["profile-optimised"], [
        "Do not disable optimizations that can distort deep profiles."])).

%---------------------------------------------------------------------------%

    % Options to control transitive intermodule optimization.

optdb(oc_trans_opt, generate_module_order,            bool(no),
    help("generate-module-order", [
        "Output the strongly connected components of the module",
        "dependency graph in top-down order to `<module>.order'.",
        "Effective only if --generate-dependencies is also specified."])).
optdb(oc_trans_opt, trans_opt_deps_spec,              maybe_string(no),
    % This option is for developers only for now.
    priv_arg_help("trans-opt-deps-spec", "filename", [
        "Specify a file to remove edges from the trans-opt dependency",
        "graph."])).

%---------------------------------------------------------------------------%

    % Options that ask for small modifications to generated files.

optdb(oc_output_mod, line_numbers,                     bool(no),
    short_help('n', "line-numbers", [], [
        "Put source line numbers into the generated code.",
        "The generated code may be in C (the usual case),",
        "or in Mercury (with the option --convert-to-mercury)."])).
optdb(oc_output_mod, line_numbers_around_foreign_code, bool(yes),
    help("line-numbers-around-foreign-code", [
        "Do not put source line numbers into the generated",
        "target language file",
        "around inclusions of foreign language code."])).
optdb(oc_output_mod, line_numbers_for_c_headers,       bool(no),
    help("line-numbers-for-c-headers", [
        "Put source line numbers in the generated C header files.",
        "This can make it easier to track down any problems with",
        "C code in foreign_decl pragmas, but may cause unnecessary",
        "recompilations of other modules if any of these line numbers",
        "changes (e.g. because the location of a predicate declaration",
        "changes in the Mercury source file)."])).
optdb(oc_output_dev, type_repns_for_humans,            bool(no),
    % This option is for developers only.
    priv_help("type-repns-for-humans", [
        "Format type_repn items in automatically generated interface files",
        "to be more easily read by humans."])).
optdb(oc_output_dev, auto_comments,                    bool(no),
    help("auto-comments", [
        "Output comments in the generated target language file.",
        "(The code may be easier to understand if you also",
        "use the `--no-llds-optimize' option.)"])).
optdb(oc_output_dev, frameopt_comments,                bool(no),
    % This option is for developers only. Since it can include one C comment
    % inside another, the resulting code is not guaranteed to be valid C.
    priv_help("frameopt-comments", [
        "Get frameopt.m to generate comments describing its operation.",
        "(The code may be easier to understand if you also",
        "use the `--no-llds-optimize' option.)"])).

optdb(oc_file_req, show_definitions,                 bool(no),
    alt_help("show-definitions",
            ["show-defns"], [
        "Write out a list of the types, insts, modes, predicates, functions",
        "typeclasses and instances defined in the module to",
        "`<module>.defns'."])).
optdb(oc_file_req, show_definition_line_counts,      bool(no),
    alt_help("show-definition-line-counts",
            ["show-defn-line-counts"], [
        "Write out a list of the predicates and functions defined in",
        "the module, together with the names of the files containing them",
        "and their approximate line counts, to `<module>.defn_line_counts'.",
        "The list will be ordered on the names and arities of the",
        "predicates and functions."])).
optdb(oc_file_req, show_definition_extents,          bool(no),
    alt_help("show-definition-extents",
            ["show-defn-extents"], [
        "Write out a list of the predicates and functions defined in",
        "the module, together with the approximate line numbers of their",
        "first and last lines, to `<module>.defn_extents'.",
        "The list will be ordered on the starting line numbers",
        "of the predicates and functions."])).
optdb(oc_file_req, show_local_call_tree,             bool(no),
    help("show-local-call-tree", [
        "Construct the local call tree of the predicates and functions",
        "defined in the module. Each node of this tree is a local",
        "predicate or function, and each node has edges linking it to the",
        "nodes of the other local predicates and functions it directly",
        "refers to. Write out to `<module>.local_call_tree' a list of",
        "these nodes. Put these nodes into the order in which they are",
        "encountered by a depth-first left-to-right traversal of the bodies",
        "(as reordered by mode analysis), of the first procedure of",
        "each predicate or function, starting the traversal at the",
        "exported predicates and/or functions of the module.",
        "List the callees of each node in the same order.",
        "Write a flattened form of this call tree, containing just",
        "the predicates and functions in the same traversal order,",
        "to `<module>.local_call_tree_order'.",
        "Construct another call tree of the predicates and functions",
        "defined in the module in which each entry lists",
        "not just the local predicates/functions directly referred to,",
        "but all directly or indirectly referenced predicates/functions,",
        "whether or not they are defined in the current module.",
        "The one restriction is that we consider only references",
        "that occur in the body of the current module.",
        "Write out this tree to `<module>.local_call_full'."])).
optdb(oc_file_req, show_local_type_repns,            bool(no),
    alt_help("show-local-type-representations",
            ["show-local-type-repns"], [
        "Write out information about the representations of all types",
        "defined in the module being compiled to `<module>.type_repns'."])).
optdb(oc_file_req, show_all_type_repns,              bool(no),
    alt_help("show-all-type-representations",
            ["show-all-type-repns"], [
        "Write out information about the representations of all types",
        "visible in the module being compiled to `<module>.type_repns'."])).
optdb(oc_file_req, show_developer_type_repns,        bool(no),
    priv_alt_help("show-developer-type-representations",
            ["show-developer-type-repns"], [
        "When writing out information about the representations of types,",
        "include information that is of interest to mmc developers only."])).
optdb(oc_file_req, show_dependency_graph,            bool(no),
    help("show-dependency-graph", [
        "Write out the dependency graph to `<module>.dependency_graph'."])).
optdb(oc_file_req, show_imports_graph,               bool(no),
    alt_help("show-imports-graph", ["imports-graph"], [
        "If --generate-dependencies is specified, then write out",
        "the imports graph to `<module>.imports_graph' in a format",
        "that can be processed by the graphviz tools.",
        "The graph will contain an edge from the node of module A",
        "to the node of module B if module A imports module B."])).

%---------------------------------------------------------------------------%

    % Language semantics options.

optdb(oc_semantics, strict_sequential,                 special,
    help("strict-sequential", [
        "--strict-sequential sets --no-reorder-conj, --no-reorder-disj",
        "and --fully-strict."])).
optdb(oc_semantics, reorder_conj,                      bool(yes),
    help("reorder-conj", [
        "Execute conjunctions left-to-right. Do not reorder conjuncts,",
        "except where the modes require it."])).
optdb(oc_semantics, reorder_disj,                      bool(yes),
    help("reorder-disj", [
        "Execute disjunctions strictly left-to-right;",
        "do not reorder disjuncts."])).
optdb(oc_semantics, fully_strict,                      bool(yes),
    help("fully-strict", [
        "Allow infinite loops, and goals whose determinism is erroneous,",
        "to be optimised away."])).

%---------------------------------------------------------------------------%

    % Inference control options.

optdb(oc_infer, infer_all,                         bool_special,
    help("infer-all", [
        "Abbreviation for `--infer-types --infer-modes --infer-det'."])).
optdb(oc_infer, infer_types,                       bool(no),
    help("infer-types", [
        "If there is no type declaration for a predicate or function,",
        "try to infer the type, rather than just reporting an error."])).
optdb(oc_infer, infer_modes,                       bool(no),
    help("infer-modes", [
        "If there is no mode declaration for a predicate,",
        "try to infer the modes, rather than just reporting an error."])).
optdb(oc_infer, infer_det,                         bool(yes),
    alt_help("infer-determinism", ["infer-det"], [
        "If there is no determinism declaration for a procedure,",
        "just report an error; do not try to infer the determinism."])).
optdb(oc_infer, type_inference_iteration_limit,    int(60),
    arg_help("type-inference-iteration-limit", "n", [
        "Perform at most <n> passes of type inference (default: 60)."])).
optdb(oc_infer, mode_inference_iteration_limit,    int(30),
    arg_help("mode-inference-iteration-limit", "n", [
        "Perform at most <n> passes of mode inference (default: 30)."])).
optdb(oc_infer, allow_stubs,                       bool(no),
    % A stub is sort-of like inferring a clause body, right ?-)
    help("allow-stubs", [
        "Allow procedures to have no clauses. Any calls to",
        "such procedures will raise an exception at run-time.",
        "This option is sometimes useful during program development.",
        "(See also the documentation for the `--warn-stubs' option",
        "in the ""Warning Options"" section.)"])).

%---------------------------------------------------------------------------%

    % Compilation model options (ones that affect binary compatibility).
    %
    % XXX Many of the options in this section are NOT grade options.
    % Some merely control *how* a grade's functionality operates;
    % others are internal-use-only.

optdb(oc_grade, grade,                                 string_special,
    % The `mmc' script will pass the default grade determined
    % at configuration time.
    short_arg_help('s', "grade", [], "grade", [
        "Select the compilation model. The <grade> should consist of",
        "one of the base grades `none', `reg', `asm_fast', `hlc', `java'",
        "or `csharp',",
        "followed by zero or more of the grade modifiers",
        "`.gc', `.prof', `.memprof', `.profdeep', `.tr',",
        "`.spf', `.stseg', `.debug', and `.par'.",
        "Depending on your particular installation, only a subset",
        "of these possible grades will have been installed.",
        "Attempting to use a grade which has not been installed",
        "will result in an error at link time."])).

%---------------------%

    % Target selection compilation model options.

optdb(oc_grade, target,                                string("c"),
    alt_arg_align_help("target", [
            arg_align("c",      "(grades: none, reg, asm_fast, hlc)"),
            arg_align("csharp", "(grades: csharp)"),
            arg_align("java",   "(grades: java)")], [
        "Specify the target language: C, C# or Java.",
        "The default is C.",
        "Targets other than C imply `--high-level-code' (see below)."])).
optdb(oc_grade, compile_to_c,                          special,
    alt_help("compile-to-c", ["compile-to-C"], [
        "An abbreviation for `--target c --target-code-only'.",
        "Generate C code in `<module>.c', but do not generate object",
        "code."])).
optdb(oc_grade, csharp,                                special,
    alt_help("csharp", ["C#"], [
        "An abbreviation for `--target csharp'."])).
optdb(oc_grade, csharp_only,                           special,
    % XXX Using "object code" for C# is iffy.
    alt_help("csharp-only", ["C#-only"], [
        "An abbreviation for `--target csharp --target-code-only'.",
        "Generate C# code in `<module>.cs', but do not generate",
        "object code."])).
optdb(oc_grade, java,                                  special,
    alt_help("java", ["Java"], [
        "An abbreviation for `--target java'."])).
optdb(oc_grade, java_only,                             special,
    % XXX Using "object code" for Java is iffy.
    alt_help("java-only", ["Java-only"], [
        "An abbreviation for `--target java --target-code-only'.",
        "Generate Java code in `<module>.java', but do not generate",
        "object code."])).

%---------------------%

    % Optional feature compilation model options:
    % (a1) Mdb debugging

optdb(oc_grade, exec_trace,                            bool(no),
    % Yes, the internal and external names of this option are THAT different.
    alt_align_help("debug", [],
            "(grade modifier: `.debug')", [
        "Enable Mercury-level debugging.",
        "See the Debugging chapter of the Mercury User's Guide",
        "for details.",
        "This option is not yet supported for the `--high-level-code'",
        "back-ends."])).
optdb(oc_grade, decl_debug,                            bool(no),
    alt_align_help("decl-debug", [],
            "(grade modifier: `.decldebug')", [
        "Enable full support for declarative debugging.",
        "This allows subterm dependency tracking in the declarative",
        "debugger.",
        "See the Debugging chapter of the Mercury User's Guide",
        "for details.",
        "This option is not yet supported for the `--high-level-code'",
        "back-ends."])).

%---------------------%

    % Optional feature compilation model options:
    % (a2) Ssdb debugging

optdb(oc_grade, source_to_source_debug,                bool(no),
    % XXX Source-to-source debugging is not ready for the public.
    priv_alt_align_help("source-to-source-debug",
            ["ss-debug", "ssdb"], "(grade modifier: `.ssdebug')", [
        "Enable the source-to-source debugging transform."])).

%---------------------%

    % Optional feature compilation model options:
    % (b1) Mprof profiling

% XXX The profiling and time_profiling options are ALMOST identical;
% the only difference is that profiling can be negated, while
% time_profiling cannot. I (zs) do not understand what need there is
% for time_profiling, especially since it is not documented.
% XXX ORDER
optdb(oc_grade, profiling,                             bool_special,
    short_alt_align_help('p', "profiling", [],
            "(grade modifier: `.prof')", [
        "Enable time and call profiling. Insert profiling hooks in the",
        "generated code, and also output some profiling",
        "information (the static call graph) to the file",
        "`<module>.prof'.",
        "This option is not supported for the C# or Java back-ends."])).
optdb(oc_grade, time_profiling,                        special,
    % XXX This option should not be visible even to developers.
    priv_help("time-profiling", [])).
optdb(oc_grade, memory_profiling,                      special,
    alt_align_help("memory-profiling", [],
            "(grade modifier: `.memprof')", [
        "Enable memory and call profiling.",
        "This option is not supported for the C# or Java back-ends."])).
    % XXX The next three options are private, because they are not useful.
    % The idea was for you to be able to use --profile-calls and
    % --profile-time separately, but that doesn't work because compiling
    % with --profile-time instead of --profile-calls results in
    % different code addresses, so you can't combine the data
    % from versions of your program compiled with different options.
optdb(oc_grade, profile_calls,                         bool(no),
    priv_alt_align_help("profile-calls", [],
            "(grade modifier: `.profcalls')", [
        "Similar to `--profiling', except that only gathers",
        "call counts, not timing information.",
        "Useful on systems where time profiling is not supported,",
        "but not as useful as `--memory-profiling'."])).
optdb(oc_grade, profile_time,                          bool(no),
    priv_alt_align_help("profile-time", [],
            "(grade modifier: `.proftime')", [
        "Similar to `--profiling', except that it only gathers",
        "timing information, not call counts."])).
optdb(oc_grade, profile_memory,                        bool(no),
    priv_alt_align_help("profile-memory", [],
            "(grade modifier: `.profmem')", [
        "Similar to `--memory-profiling', except that it only",
        "gathers memory usage information, not call counts."])).

%---------------------%

    % Optional feature compilation model options:
    % (b2) Mdprof profiling

optdb(oc_grade, deep_profiling,                        special,
    alt_align_help("deep-profiling", [],
            "(grade modifier: `.profdeep')", [
        "Enable deep profiling.",
        "This option is not supported for the high-level C, C#",
        "or Java back-ends."])).
optdb(oc_grade, profile_deep,                          bool(no),
    % This the *actual* grade option that switches on deep profiling.
    % The deep_profiling option sets profile_deep to "yes", *and* sets
    % the grade options for mprof profiling to "no".
    priv_help("profile-deep", [])).
optdb(oc_grade, use_activation_counts,                 bool(no),
    priv_help("use-activation-counts", [])).
    % use_activation_counts is an experimental feature.
    % It *is* a grade option.
optdb(oc_grade, coverage_profiling,                    bool(yes),
    help("coverage-profiling", [
        "Do not gather profiling information that is useful",
        "only for coverage profiling."])).
% The next two options are intended for experiments.
optdb(oc_grade, coverage_profiling_via_calls,          bool(no),
    priv_help("coverage-profiling-via-calls", [
        "Use calls to implement coverage points, not inline foreign code."])).
optdb(oc_grade, coverage_profiling_static,             bool(no),
    % This help text could be clearer.
    priv_help("coverage-profiling-static", [
        "Disable coverage profiling of ProcDynamics; cover only ProcStatics.",
        "This uses less memory, and may be faster."])).
% The next four options control coverage profiling (part of deep profiling):
% they enable different types of coverage points.
optdb(oc_grade, profile_deep_coverage_after_goal,      bool(yes),
    priv_help("profile-deep-coverage-after-goal", [
        "Disable coverage points after goals."])).
optdb(oc_grade, profile_deep_coverage_branch_ite,      bool(yes),
    priv_help("profile-deep-coverage-branch-ite", [
        "Disable coverage points at the beginning of then and else",
        "branches."])).
optdb(oc_grade, profile_deep_coverage_branch_switch,   bool(yes),
    priv_help("profile-deep-coverage-branch-switch", [
        "Disable coverage points at the beginning of switch branches."])).
optdb(oc_grade, profile_deep_coverage_branch_disj,     bool(yes),
    priv_help("profile-deep-coverage-branch-disj", [
        "Disable coverage points at the beginning of disjunction branches."])).
% The next two options tune the coverage profiling pass, useful for debugging.
% I believe these options are broken - pbone.
optdb(oc_grade, profile_deep_coverage_use_portcounts,  bool(no),
    priv_help("profile-deep-coverage-use-portcounts", [
        "Use port counts to provide coverage information."])).
optdb(oc_grade, profile_deep_coverage_use_trivial,     bool(no),
    priv_help("profile-deep-coverage-use-trivial", [
        "Use simple goal properties for coverage information."])).
optdb(oc_grade, profile_for_feedback,                  bool(no),
    alt_help("profile-for-feedback",
            ["profile-for-implicit-parallelism"], [
        "Select deep profiling options suitable for profiler directed",
        "implicit parallelism.",
        "--profile-for-implicit-parallelism is a deprecated synonym for",
        "this option."])).
% The next three options are developer-only non-grade options.
optdb(oc_grade, use_zeroing_for_ho_cycles,             bool(yes),
    priv_help("use-zeroing-for-ho-cycles", [])).
optdb(oc_grade, use_lots_of_ho_specialization,         bool(no),
    priv_help("use-lots-of-ho-specialization", [])).
optdb(oc_grade, deep_profile_tail_recursion,           bool(no),
    priv_help("deep-profile-tail-recursion", [])).

%---------------------%

    % Optional feature compilation model options:
    % (b3) Complexity profiling

optdb(oc_grade, record_term_sizes_as_words,            bool(no),
    priv_alt_align_help("record-term-sizes-as-words", [],
            "(grade modifier: `.tsw')", [
        "Augment each heap cell with its size in words."])).
optdb(oc_grade, record_term_sizes_as_cells,            bool(no),
    priv_alt_align_help("record-term-sizes-as-cells", [],
            "(grade modifier: `.tsc')", [
        "Augment each heap cell with its size in cells."])).
optdb(oc_grade, experimental_complexity,               string(""),
    % XXX This is NOT a grade option; it only takes advantage of a grade.
    priv_arg_help("experimental-complexity", "filename", [
        "Enable experimental complexity analysis for the predicates",
        "listed in the given file.",
        "This option is supported only for the C back-end with",
        "`--no-highlevel-code'."])).

%---------------------%

    % Optional feature compilation model options:
    % (b4) Threadscope profiling

optdb(oc_grade, threadscope,                           bool(no),
    % XXX Threadscope profiling has not been maintained since Paul stopped
    % working on Mercury. It has almost certainly suffered bit rot by now.
    alt_align_help("threadscope", [],
            "(grade modifier: `.threadscope')", [
        "Enable support for profiling parallel execution.",
        "This option is supported by the low-level C back-end parallel",
        "grades on some processors. See README.ThreadScope for details."])).

%---------------------%

    % Optional feature compilation model options:
    % (c) Miscellaneous optional features

optdb(oc_grade, gc,                                    string("boehm"),
    % We do not document the "accurate" and "hgc" GC methods,
    % as those methods are still experimental.
    alt_arg_help("garbage-collection", ["gc"],
            "{none, boehm, automatic}", [
        % "`hgc' is our own conservative collector;",
        % "`accurate' is our own type-accurate copying GC;",
        % "it requires `--high-level-code'.",
        "Specify which method of garbage collection to use.",
        "When targeting Java or C#, the only possible choice",
        "is `automatic', which means the garbage collector",
        "built into the target language.",
        "When targeting C, the default is `boehm',",
        "which is Hans Boehm et al's conservative collector.",
        "The use of the `boehm' collector is indicated by",
        "the `.gc' grade component.",
        "The other alternative when targeting C is `none'",
        "meaning there is no garbage collector.",
        "This works only for programs with very short runtimes."])).
optdb(oc_grade, stack_segments,                        bool(no),
    alt_align_help("stack-segments", [],
            "(grade modifier: `.stseg')", [
        "Specify whether to use dynamically sized stacks that are",
        "composed of small segments. This can help to avoid stack",
        "exhaustion at the cost of increased execution time.",
        "This option is not supported by the `--high-level-code'",
        "back-ends."])).
optdb(oc_grade, extend_stacks_when_needed,             bool(no),
    % This is private as this feature is still experimental.
    priv_help("extend-stacks-when-needed", [
        "Specify that code that increments a stack pointer must",
        "extend the stack when this is needed."])).
optdb(oc_grade, use_trail,                             bool(no),
    alt_align_help("use-trail", [],
            "(grade modifier: `.tr')", [
        "Enable use of a trail.",
        "This is necessary for interfacing with constraint solvers,",
        "or for backtrackable destructive update.",
        "This option is not yet supported for the C# or Java backends."])).
optdb(oc_grade, pregenerated_dist,                     bool(no),
    % XXX The pregen grade component *should* be documented.
    priv_help("pregenerated-dist", [])).
optdb(oc_grade, single_prec_float,                     bool(no),
    alt_align_help("single-precision-float",
            ["single-prec-float"], "(grade modifier: `.spf')", [
        "Use single precision floats so that, on 32-bit machines,",
        "floating point values don't need to be boxed. Double",
        "precision floats are used by default.",
        "This option is not supported for the C# or Java back-ends."])).
optdb(oc_grade, parallel,                              bool(no),
    alt_align_help("parallel", [],
            "(grade modifier: `.par')", [
        "Enable parallel execution support for the low-level C grades.",
        "Enable concurrency (via pthreads) for the high-level C grades."])).
optdb(oc_grade, maybe_thread_safe_opt,                 string("no"),
    % XXX How is a yes/no argument better than a no- prefix?
    arg_help("maybe-thread-safe", "{yes, no}", [
        "Specify how to treat the `maybe_thread_safe' foreign code",
        "attribute. `yes' means that a foreign procedure with the",
        "`maybe_thread_safe' option is treated as though it has a",
        "`thread_safe' attribute. `no' means that the foreign",
        "procedure is treated as though it has a `not_thread_safe'",
        "attribute. The default is `no'."])).
optdb(oc_grade, use_minimal_model_stack_copy,          bool(no),
    % This controls the .mmsc grade component.
    priv_help("use-minimal-model-stack-copy", [
        "Enable the use of the standard form of minimal model tabling."])).
optdb(oc_grade, use_minimal_model_own_stacks,          bool(no),
    % This controls the .mmos grade component.
    priv_help("use-minimal-model-own-stacks", [
        "Enable the use of an experimental form of minimal model tabling."])).
optdb(oc_grade, minimal_model_debug,                   bool(no),
    % This turns .mmos into .dmmos and .mmsc into .dmmsc.
    priv_help("minimal-model-debug", [
        "Enables extra data structures that assist in debugging",
        "minimal model tabling."])).
% RBMM is undocumented since it is still experimental.
optdb(oc_grade, use_regions,                           bool(no),
    priv_alt_align_help("use-regions", [],
            "(grade modifier: `.rbmm')", [
        "Enable support for region-based memory management."])).
optdb(oc_grade, use_alloc_regions,                     bool(yes),
    priv_help("use-alloc-regions", [
        "Compute and use the exact set of regions",
        "that may be allocated into by a call."])).
% use_regions_debug and use_regions_profiling *are* (private) grade options.
% XXX They should be documented.
optdb(oc_grade, use_regions_debug,                     bool(no),
    priv_help("use-regions-debug", [])).
optdb(oc_grade, use_regions_profiling,                 bool(no),
    priv_help("use-regions-profiling", [])).
% XXX Source-to-source debugging is not ready for the public.
% XXX Neither of the following two options is a grade option.
optdb(oc_grade, ssdb_trace_level,                      string("default"),
    priv_arg_help("ssdb-trace", "{none, shallow, deep}", [
        "The trace level to use for source to source debugging of",
        "the given module."])).
optdb(oc_grade, link_ssdb_libs,                        bool(no),
    priv_alt_help("link-ssdebug-libs", ["link-ssdb-libs"], [
        "Link the source to source debugging libraries into the",
        "executable."])).

%---------------------%

    % Optional feature compilation model options:
    % (d) representation compilation model options

optdb(oc_grade, num_ptag_bits,                         int(-1),
    % -1 means: use the value of conf_low_ptag_bits instead.
    %
    % Normally, the --num-tag-bits option is used only by the compiler.
    % By default, its value is set to the value of the --conf-low-tag-bits
    % option when targeting C, and to zero when targeting other languages.
    % Its only legitimate use by non-developers is for cross-compilation.
    % XXX That fact should be included in the help text.
    alt_arg_help("num-ptag-bits",
            ["num-tag-bits"], "n", [
        "Use <n> primary tag bits.",
        "Note that the value of this option is normally autoconfigured;",
        "its use should never be needed except for cross-compilation."])).
optdb(oc_grade, conf_low_ptag_bits,                    int(2),
    % The `mmc' script will override the above default with
    % a value determined at configuration time.
    priv_alt_arg_help("conf-low-ptag-bits",
            ["conf-low-tag-bits"], "n", [
        "Reserved for use by the `mmc' script."])).
optdb(oc_grade, bits_per_word,                         int(32),
    % A good default for the current generation of architectures.
    priv_arg_help("bits-per-word", "n", [
        "Reserved for use by the `mmc' script."])).
optdb(oc_grade, bytes_per_word,                        int(4),
    % A good default for the current generation of architectures.
    priv_arg_help("bytes-per-word", "n", [
        "Reserved for use by the `mmc' script."])).
% XXX All the unboxed_X options should be replaced by boxed_X options.
optdb(oc_grade, unboxed_float,                         bool(no),
    priv_help("unboxed-float", [
        "Do not box floating point numbers.",
        "This assumes that a Mercury float will fit in a word.",
        "The C code needs to be compiled with `-UMR_BOXED_FLOAT'.",
        "It may also need to be compiled with",
        "`-DMR_USE_SINGLE_PREC_FLOAT', if double precision",
        "floats don't fit into a word.",
        "Note that the value of this option is normally autoconfigured;",
        "its use should never be needed except for cross-compilation."])).
optdb(oc_grade, unboxed_int64s,                        bool(no),
    priv_help("unboxed-int64s", [
        "Do not box 64-bit integer numbers",
        "This assumes that word size of the target machine is at least",
        "64-bits in size.",
        "The C code needs to be compiled with `-UMR_BOXED_INT64S'.",
        "Note that the value of this option is normally autoconfigured;",
        "its use should never be needed except for cross-compilation."])).
optdb(oc_grade, unboxed_no_tag_types,                  bool(yes),
    priv_help("unboxed-no-tag-types", [
        "Box no-tag types. (By default, no-tag types are unboxed.)",
        "Note that the value of this option is normally autoconfigured;",
        "its use should never be needed except for cross-compilation."])).
optdb(oc_grade, arg_pack_bits,                         int(-1),
    % -1 is a special value which means use all word bits
    % for argument packing.
    priv_arg_help("arg-pack-bits", "n", [
        "The number of bits in a word in which to pack constructor arguments.",
        "Note that the value of this option is normally autoconfigured;",
        "its use should never be needed except for cross-compilation."])).
optdb(oc_grade, pack_everything,                       bool(no),
    priv_help("pack-everything", [
        "Tell decide_type_repn.m to pack everything that can be packed."])).
optdb(oc_grade, allow_direct_args,                     bool(yes),
    priv_help("allow-direct-args", [
        "Allow the direct arg optimization."])).
optdb(oc_grade, allow_double_word_fields,              bool(yes),
    priv_help("allow-double-word-fields", [
        "Disallow storing a single constructor argument in two words.",
        "(This mainly applies to arguments that are",
        "double-precision floats or whose type is int64 or uint64."])).
optdb(oc_grade, allow_double_word_ints,                bool(no),
    priv_help("allow-double-word-ints", [])).
optdb(oc_grade, allow_packing_dummies,                 bool(no),
    priv_help("allow-packing-dummies", [])).
optdb(oc_grade, allow_packing_ints,                    bool(no),
    priv_help("allow-packing-ints", [])).
optdb(oc_grade, allow_packing_chars,                   bool(no),
    priv_help("allow-packing-chars", [])).
optdb(oc_grade, allow_packing_local_sectags,           bool(no),
    priv_help("allow-packing-local-sectags", [])).
optdb(oc_grade, allow_packing_remote_sectags,          bool(no),
    priv_help("allow-packing-remote-sectags", [])).
optdb(oc_grade, allow_packing_mini_types,              bool(no),
    priv_help("allow-packing-mini-types", [])).
optdb(oc_grade, allow_packed_unify_compare,            bool(no),
    priv_help("allow-packed-unify-compare", [])).
optdb(oc_grade, sync_term_size_in_words,               int(8),
    % 8 is the size on linux (at the time of writing) - will usually be
    % overridden by a value from configure.
    priv_alt_arg_help("sync-term-size-in-words", ["sync-term-size"],
        "num_words", [])).

%---------------------%

    % Optional feature compilation model options:
    % LLDS back-end compilation model options

optdb(oc_grade,    gcc_global_registers,                  bool(yes),
    % We do not document "fast".
    no_align_help("gcc-global-registers",
            "(grades: reg, asm_fast)",
            "(grades: none)", [
        "Specify whether or not to use GNU C's",
        "global register variables extension.",
        "This option is ignored if the `--high-level-code' option is",
        "enabled."])).
optdb(oc_grade,    gcc_non_local_gotos,                   bool(yes),
    % We do not document "jump", "fast", "asm_jump".
    no_align_help("gcc-non-local-gotos",
            "(grades: asm_fast)",
            "(grades: none, reg)", [
        "Specify whether or not to use GNU C's",
        """labels as values"" extension.",
        "This option is ignored if the `--high-level-code' option is",
        "enabled."])).
optdb(oc_grade,    asm_labels,                            bool(yes),
    % We do not document "asm_jump".
    no_align_help("asm-labels",
            "(grades: asm_fast)",
            "(grades: none, reg)", [
        "Specify whether or not to use GNU C's asm extensions for",
        "inline assembler labels.",
        "This option is ignored if the `--high-level-code' option is",
        "enabled."])).
optdb(oc_grade,    use_float_registers,                   bool(yes),
    priv_help("use-float-registers", [
        "Use float registers for argument passing."])).

%---------------------%

    % Optional feature compilation model options:
    % MLDS back-end compilation model options

optdb(oc_grade,    highlevel_code,                        bool(no),
    short_alt_align_help('H', "high-level-code",
            ["high-level-c", "high-level-C",
            "highlevel-code", "highlevel-c", "highlevel-C"],
            "(grades: hlc, csharp, java)", [
        "Use an alternative back-end that generates high-level code",
        "rather than the very low-level code that is generated by our",
        "original back-end."])).
optdb(oc_grade,    c_debug_grade,                         bool(no),
    alt_align_help("c-debug-grade", [],
            "(grades: hlc)", [
        "Require that all modules in the program be compiled to object code",
        "in a way that allows the program executable to be debuggable",
        "with debuggers for C, such as gdb. This option is intended mainly",
        "for the developers of Mercury, though it can also help to debug",
        "C code included in Mercury programs."])).

%---------------------------------------------------------------------------%

    % Code generation options.

optdb(oc_config,   have_delay_slot,                     bool(no),
    % The `mmc' script may override the default if configure says
    % the machine has branch delay slots.
    alt_help("branch-delay-slot", ["have-delay-slot"], [
        "Assume that branch instructions have a delay slot.",
        "Note that the value of this option is normally autoconfigured;",
        "its use should never be needed except for cross-compilation."])).
% The `mmc' script will override the next four options' defaults
% with values determined at configuration time.
optdb(oc_config,   num_real_r_regs,                     int(5),
    arg_help("num-real-r-regs", "n", [
        "Assume registers r1 up to r<n> are real general purpose registers.",
        "Note that the value of this option is normally autoconfigured;",
        "its use should never be needed except for cross-compilation."])).
optdb(oc_config,   num_real_f_regs,                     int(0),
    arg_help("num-real-f-regs", "n", [
        "Assume registers f1 up to f<n> are real floating point registers.",
        "Note that the value of this option is normally autoconfigured;",
        "its use should never be needed except for cross-compilation."])).
optdb(oc_config,   num_real_r_temps,                    int(5),
    alt_arg_help("num-real-r-temps",
            ["num-real-temps"], "n", [
        "Assume that <n> non-float temporaries will fit into",
        "real machine registers.",
        "Note that the value of this option is normally autoconfigured;",
        "its use should never be needed except for cross-compilation."])).
optdb(oc_config,   num_real_f_temps,                    int(0),
    arg_help("num-real-f-temps", "n", [
        "Assume that <n> float temporaries will fit into",
        "real machine registers.",
        "Note that the value of this option is normally autoconfigured;",
        "its use should never be needed except for cross-compilation."])).
optdb(oc_config,   max_jump_table_size,                 int(0),
    % 0 indicates jump tables can be any size.
    % XXX This option works around limitations in 1998 C compilers.
    % Its value should be set automatically by handle_options.m
    % based on the value of the c_compiler_type option.
    arg_help("max-jump-table-size", "n", [
        "The maximum number of entries a jump table can have.",
        "The special value 0 indicates the table size is unlimited.",
        "This option can be useful to avoid exceeding fixed limits",
        "imposed by some C compilers."])).

%---------------------------------------------------------------------------%

    % Special optimization options.
    % These ones are not affected by `-O<n>'.

optdb(oc_opt_ctrl, default_opt_level,                  string("-O2"),
    % This is for use by Mercury.config only.
    priv_arg_help("default-opt-level", "-O<n>", [
        "Set the default optimization level to <n>."])).
optdb(oc_opt_ctrl, opt_level,                          int_special,
    short_arg_help('O', "optimization-level",
            ["optimisation-level", "opt-level"], "n", [
        "Set optimization level to <n>.",
        "Optimization level -1 means no optimization",
        "while optimization level 6 means full optimization."])).
        % "For a full description of each optimization level,",
        % "see the Mercury User's Guide.",
optdb(oc_opt_ctrl, opt_space,                          special,
    alt_help("optimize-space",
            ["optimise-space", "opt-space"], [
        "Turn on optimizations that reduce code size",
        "and turn off optimizations that significantly",
        "increase code size."])).
optdb(oc_opt_ctrl, intermodule_optimization,           bool(no),
    alt_help("intermodule-optimization",
            ["intermodule-optimisation", "intermod-opt"], [
        "Perform inlining and higher-order specialization of",
        "the code for predicates imported from other modules.",
        "This option must be set throughout the compilation process."])).
optdb(oc_opt_ctrl, transitive_optimization,            bool(no),
    alt_help("transitive-intermodule-optimization",
            ["transitive-intermodule-optimisation", "trans-intermod-opt"], [
        "Import the transitive intermodule optimization data.",
        "This data is imported from `<module>.trans_opt' files.",
        "Note that `--transitive-intermodule-optimization' does not",
        "work with", "QUOTE", "`mmc --make'."])).
optdb(oc_opt_ctrl, read_opt_files_transitively,        bool(yes),
    help("read-opt-files-transitively", [
        "Only read the inter-module optimization information",
        "for directly imported modules, not the transitive",
        "closure of the imports."])).
optdb(oc_opt_ctrl, use_opt_files,                      bool(no),
    help("use-opt-files", [
        "Perform inter-module optimization using any",
        "`.opt' files which are already built,",
        "e.g. those for the standard library, but do",
        "not build any others."])).
optdb(oc_opt_ctrl, use_trans_opt_files,                bool(no),
    help("use-trans-opt-files", [
        "Perform inter-module optimization using any",
        "`.trans_opt' files which are already built,",
        "e.g. those for the standard library, but do",
        "not build any others."])).
optdb(oc_opt_ctrl, intermodule_analysis,               bool(no),
    help("intermodule-analysis", [
        "Perform analyses such as termination analysis and",
        "unused argument elimination across module boundaries.",
        "This option is not yet fully implemented."])).
optdb(oc_opt_ctrl, analysis_repeat,                    int(0),
    arg_help("analysis-repeat", "n", [
        "The maximum number of times to repeat analyses of",
        "suboptimal modules with `--intermodule-analysis'",
        "(default: 0)."])).
optdb(oc_opt_ctrl, analysis_file_cache,                bool(no),
    % This feature is still experimental.
    priv_help("analysis-file-cache", [
        "Enable caching of parsed analysis files. This may",
        "improve compile times with `--intermodule-analysis'."])).
optdb(oc_opt_ctrl, analysis_file_cache_dir,            string(""),
    % The `--analysis-file-cache-dir' option is used by `mmc --make'.
    priv_arg_help("analysis-file-cache-dir", "dir", [])).

%---------------------------------------------------------------------------%

    % Optimization options
    % IMPORTANT: the default here should be all optimizations OFF.
    % Optimizations should be enabled by the appropriate optimization level
    % in the opt_level table.

    % HLDS -> HLDS

optdb(oc_opt_hh, optopt_allow_inlining,                   bool_special,
    priv_help("allow-inlining", [
        "Disallow all forms of inlining."])).
optdb(oc_opt_hh, inlining,                                bool_special,
    help("inlining", [
        "Ask the compiler to inline procedures using its usual heurisrics."])).
optdb(oc_opt_hh, optopt_inline_simple,                    bool_special,
    help("inline-simple", [
        "Ask the compiler to inline simple procedures."])).
optdb(oc_opt_hh, optopt_inline_builtins,                  bool_special,
    help("inline-builtins", [
        "Normally, the compiler implements builtin operations",
        "(such as arithmeetic) using inline code.",
        "With --no-inline-builtins, the compiler will implement them",
        "as calls to out-of-line procedures.",
        "This latter is done by default when debugging,",
        "since this allows the execution of builtins to be traced."])).
optdb(oc_opt_hh, optopt_inline_single_use,                bool_special,
    help("inline-single-use", [
        "Inline procedures which are called only from one call site."])).
optdb(oc_opt_hh, optopt_inline_call_cost,                 int_special,
    arg_help("inline-call-cost", "cost", [
        "Assume that the cost of a call is the given parameter.",
        "Used only in conjunction with `--inline-compound-threshold'."])).
optdb(oc_opt_hh, optopt_inline_compound_threshold,        int_special,
    arg_help("inline-compound-threshold", "threshold", [
        "Inline a procedure if its size (measured roughly",
        "in terms of the number of connectives in its internal form)",
        "less the assumed call cost, multiplied by the number of times",
        "it is called is below the given threshold."])).
optdb(oc_opt_hh, optopt_inline_simple_threshold,          int_special,
    arg_help("inline-simple-threshold", "threshold", [
        "With --inline-simple, inline a procedure",
        "if its size is less than the given threshold."])).
optdb(oc_opt_hh, optopt_intermod_inline_simple_threshold, int_special,
    arg_help("intermod-inline-simple-threshold", "threshold", [
        "Similar to `--inline-simple-threshold', except used to",
        "determine which predicates should be included in",
        "`.opt' files. Note that changing this between writing",
        "the `.opt' file and compiling to C may cause link errors,",
        "and too high a value may result in reduced performance."])).
optdb(oc_opt_hh, optopt_inline_vars_threshold,            int_special,
    % Has no effect until --intermodule-optimization.
    % XXX Is this true? If so, Why?
    arg_help("inline-vars-threshold", "threshold", [
        "Don't inline a call if it would result in a procedure",
        "containing more than <threshold> variables. Procedures",
        "containing large numbers of variables can cause",
        "slow compilation."])).
optdb(oc_opt_hh, optopt_inline_tr_sccs,                   bool_special,
    help("inline-linear-tail-rec-sccs", [
        "Given a set of mutually recursive procedures (an SCC, or strongly",
        "connected component, of the call graph) in which each procedure",
        "contains exactly tail call to a procedure in the SCC, so that",
        "the tail recursive calls form a linear chain through the SCC,",
        "inline the callee at every one of those mutually tail recursive",
        "call sites. This converts mutual tail recursion into self tail",
        "recursion, which the MLDS backend can turn into code that runs",
        "in constant stack space."])).
optdb(oc_opt_hh, optopt_inline_tr_sccs_max_extra,         int_special,
    priv_arg_help("inline-linear-tail-rec-sccs-max-extra", "E", [
        "When considering whether to apply --inline-linear-tail-rec-sccs",
        "to an SCC containing N procedures, allow the SCC to contain",
        "up to N+E mutually recursive tail calls."])).
optdb(oc_opt_hh, optopt_inline_par_builtins,              bool_special,
    % This is for measurements by implementors only.
    priv_help("inline-par-builtins", [
        "With `--inline-par-builtins', the compiler",
        "implements the operations of par_builtin.m using inline code.",
        "With `--no-inline-par-builtins', it implements them as calls." ])).
optdb(oc_opt_hh, optopt_from_ground_term_threshold,       int_special,
    priv_arg_help("from-ground-term-threshold", "n", [
        "Wrap a from_ground_term scope around the expanded,",
        "superhomogeneous form of a ground term that involves at least.",
        "the given number of function symbols."])).
optdb(oc_opt_hh, optopt_enable_const_struct_user,         bool_special,
    help("const-struct", [
        "Gather constant ground structures in a separate table.",
        "A structure will be stored in this table just once",
        "even if it occurs in many different procedures."])).
optdb(oc_opt_hh, optopt_common_struct,                    bool_special,
    help("common-struct", [
        "Replace two or more occurrences of the same term",
        "in a conjunction with just one copy."])).
optdb(oc_opt_hh, optopt_constraint_propagation,           bool_special,
    help("constraint-propagation", [
        "Enable the constraint propagation transformation,",
        "which attempts to ensure that",
        "goals which can fail are executed as early as possible."])).
optdb(oc_opt_hh, optopt_local_constraint_propagation,     bool_special,
    help("local-constraint-propagation", [
        "Enable the constraint propagation transformation,",
        "but only rearrange goals within each procedure.",
        "Specialized versions of procedures will not be created."])).
optdb(oc_opt_hh, optopt_duplicate_calls,                  bool_special,
    alt_help("optimize-duplicate-calls",
            ["optimise-duplicate-calls"], [
        "Optimize away multiple calls to a predicate",
        "with the same input arguments."])).
optdb(oc_opt_hh, optopt_constant_propagation,             bool_special,
    alt_help("optimize-constant-propagation",
            ["optimise-constant-propagation"], [
        "Given calls to some frequently used library functions and",
        "predicates, mainly those that do arithmetic, evaluate them",
        "at compile time, if all their input arguments are constants."])).
optdb(oc_opt_hh, optopt_excess_assign,                    bool_special,
    help("excess-assign", [
        "Remove excess assignment unifications."])).
optdb(oc_opt_hh, optopt_merge_code_after_switch,          bool_special,
    priv_help("merge-code-after-switch", [
        "Merge the goal after a switch into the switch, if we can.",
        "Two cases in which we can are when that goal just tests",
        "the value of a variable set in the switch, and when that goal",
        "is a switch on the same variable."])).
optdb(oc_opt_hh, optopt_format_calls,                     bool_special,
    help("optimize-format-calls", [
        "Interpret the format string in calls to",
        "string.format and related predicates at compile time,",
        "replacing those calls with the sequence of more primitive operations",
        "required to implement them."])).
optdb(oc_opt_hh, optopt_split_switch_arms,                bool_special,
    help("split-switch-arms", [
        "When a switch on a variable has an inner switch on that",
        "same variable inside one of its arms, split up that arm of the",
        "outer switch along the same lines, effectively inlining",
        "the inner switch."])).
optdb(oc_opt_hh, optopt_loop_invariants,                  bool_special,
    help("loop-invariants", [
        "Hoist loop invariants out of loops."])).
optdb(oc_opt_hh, optimize_saved_vars,                     bool_special,
    alt_help("optimize-saved-vars",
            ["optimise-saved-vars"], [
        "Minimize the number of variables saved across calls."])).
optdb(oc_opt_hh, optopt_saved_vars_const,                 bool_special,
    priv_alt_help("optimize-saved-vars-const",
            ["optimise-saved-vars-const"], [
        "Minimize the number of variables saved across calls by",
        "introducing duplicate copies of variables bound to",
        "constants in each interval between flushes where they",
        "are needed."])).
optdb(oc_opt_hh, optopt_svcell,                           bool_special,
    priv_alt_help("optimize-saved-vars-cell",
            ["optimise-saved-vars-cell"], [
        "Minimize the number of variables saved across calls by",
        "trying to use saved variables pointing to cells to reach",
        "the variables stored in those cells."])).
optdb(oc_opt_hh, optopt_svcell_loop,                      bool_special,
    priv_help("osv-loop", [])).
optdb(oc_opt_hh, optopt_svcell_full_path,                 bool_special,
    priv_help("osv-full-path", [])).
optdb(oc_opt_hh, optopt_svcell_on_stack,                  bool_special,
    priv_help("osv-on-stack", [])).
optdb(oc_opt_hh, optopt_svcell_candidate_headvars,        bool_special,
    priv_help("osv-cand-head", [])).
% The next four options are used by tupling.m as well; changes to them
% may require changes there as well.
optdb(oc_opt_hh, optopt_svcell_cv_store_cost,             int_special,
    priv_arg_help("osv-cvstore-cost", "cost", [])).
optdb(oc_opt_hh, optopt_svcell_cv_load_cost,              int_special,
    priv_arg_help("osv-cvload-cost", "cost", [])).
optdb(oc_opt_hh, optopt_svcell_fv_store_cost,             int_special,
    priv_arg_help("osv-fvstore-cost", "cost", [])).
optdb(oc_opt_hh, optopt_svcell_fv_load_cost,              int_special,
    priv_arg_help("osv-fvload-cost", "cost", [])).
optdb(oc_opt_hh, optopt_svcell_op_ratio,                  int_special,
    priv_arg_help("osv-op-ratio", "percentage", [])).
optdb(oc_opt_hh, optopt_svcell_node_ratio,                int_special,
    priv_arg_help("osv-node-ratio", "percentage", [])).
optdb(oc_opt_hh, optopt_svcell_all_path_node_ratio,       int_special,
    priv_arg_help("osv-allpath-node-ratio", "percentage", [])).
optdb(oc_opt_hh, optopt_svcell_all_candidates,            bool_special,
    priv_help("osv-all-cand", [])).
optdb(oc_opt_hh, optopt_delay_construct,                  bool_special,
    alt_help("delay-constructs",
            ["delay-construct"], [
        "Reorder goals to move construction unifications after",
        "primitive goals that can fail."])).
optdb(oc_opt_hh, optopt_follow_code,                      bool_special,
    priv_help("follow-code", [
        "Move the code following a branched goal (if-then-else, disjunction,",
        "or switch) until the next call into each branch of that goal.",
        "Having a call as the goal just after the branched goal",
        "gives the LLDS code generator a consistent set of places",
        "into which each branch should store live variables."])).
optdb(oc_opt_hh, optopt_unused_args,                      bool_special,
    alt_help("optimize-unused-args",
            ["optimise-unused-args"], [
        "Remove unused predicate arguments.",
        "This will cause the compiler to generate more",
        "efficient code for many polymorphic predicates."])).
optdb(oc_opt_hh, optopt_intermod_unused_args,             bool_special,
    help("intermod-unused-args", [
        "Perform unused argument removal across module boundaries.",
        "This option implies `--optimize-unused-args' and",
        "`--intermodule-optimization'."])).
optdb(oc_opt_hh, optopt_higher_order,                     bool_special,
    alt_help("optimize-higher-order",
            ["optimise-higher-order"], [
        "Enable specialization of higher-order predicates."])).
optdb(oc_opt_hh, optopt_higher_order_size_limit,          int_special,
    arg_help("higher-order-size-limit", "max_size", [
        "Set the maximum goal size of specialized versions created by",
        "`--optimize-higher-order' and `--type-specialization'.",
        "Goal size is measured as the number of calls, unifications",
        "and branched goals."])).
optdb(oc_opt_hh, optopt_higher_order_arg_limit,           int_special,
    arg_help("higher-order-arg-limit", "max_size", [
        "Set the maximum size of higher-order arguments to",
        "be specialized by `--optimize-higher-order' and",
        "`--type-specialization'."])).
optdb(oc_opt_hh, optopt_unneeded_code,                    bool_special,
    help("unneeded-code", [
        "Remove goals from computation paths where their outputs are",
        "not needed, provided the semantics options allow the deletion",
        "or movement of the goal."])).
optdb(oc_opt_hh, optopt_unneeded_code_copy_limit,         int_special,
    arg_help("unneeded-code-copy-limit", "copy_limit", [
        "Gives the maximum number of places to which a goal may be copied",
        "when removing it from computation paths on which its outputs are",
        "not needed. A value of zero forbids goal movement and allows",
        "only goal deletion; a value of one prevents any increase in the",
        "size of the code."])).
optdb(oc_opt_hh, optopt_type_specialization,              bool_special,
    alt_help("type-specialization",
            ["type-specialisation"], [
        "Enable specialization of polymorphic predicates where the",
        "polymorphic types are known."])).
optdb(oc_opt_hh, optopt_user_guided_type_specialization,  bool_special,
    alt_help("user-guided-type-specialization",
            ["user-guided-type-specialisation"], [
        "Enable specialization of polymorphic predicates for which",
        "there are `:- pragma type_spec' declarations."])).
optdb(oc_opt_hh, optopt_introduce_accumulators,           bool_special,
    help("introduce-accumulators", [
        "Attempt to introduce accumulating variables into",
        "procedures, so as to make them tail recursive."])).
optdb(oc_opt_hh, optopt_lcmc,                             bool_special,
    alt_help("optimize-constructor-last-call",
            ["optimise-constructor-last-call"], [
        "Enable the optimization of ""last"" calls that are followed by",
        "constructor application."])).
optdb(oc_opt_hh, optopt_lcmc_accumulator,                 bool_special,
    priv_alt_help("optimize-constructor-last-call-accumulator",
            ["optimise-constructor-last-call-accumulator"], [
        "Enable the optimization via accumulators of ""last"" calls",
        "that are followed by constructor application."])).
optdb(oc_opt_hh, optopt_lcmc_null,                        bool_special,
    priv_alt_help("optimize-constructor-last-call-null",
            ["optimise-constructor-last-call-null"], [
        "When --optimize-constructor-last-call is enabled, put NULL in",
        "uninitialized fields (to prevent the garbage collector from",
        "looking at and following a random bit pattern)."])).
optdb(oc_opt_hh, optopt_dead_procs,                       bool_special,
    alt_help("optimize-dead-procs",
            ["optimise-dead-procs"], [
        "Enable dead predicate elimination."])).
optdb(oc_opt_hh, optopt_deforestation,                    bool_special,
    help("deforestation", [
        "Enable deforestation. Deforestation is a program",
        "transformation whose aim is to avoid the construction of",
        "intermediate data structures and to avoid repeated traversals",
        "over data structures within a conjunction."])).
optdb(oc_opt_hh, optopt_deforestation_depth_limit,        int_special,
    arg_help("deforestation-depth-limit", "depth_limit", [
        "Specify a depth limit to prevent infinite loops in the",
        "deforestation algorithm.",
        "A value of -1 specifies no depth limit. The default is 4."])).
optdb(oc_opt_hh, optopt_deforestation_cost_factor,       int_special,
    priv_arg_help("deforestation-cost-factor", "fudge-factor", [])).
optdb(oc_opt_hh, optopt_deforestation_vars_threshold,     int_special,
    arg_help("deforestation-vars-threshold", "threshold", [
        "Specify a rough limit on the number of variables",
        "in a procedure created by deforestation.",
        "A value of -1 specifies no limit. The default is 200."])).
optdb(oc_opt_hh, optopt_deforestation_size_threshold,     int_special,
    arg_help("deforestation-size-threshold", "threshold", [
        "Specify a rough limit on the size of a goal",
        "to be optimized by deforestation.",
        "A value of -1 specifies no limit. The default is 15."])).
optdb(oc_opt_hh, optopt_untuple,                          bool_special,
    priv_help("untuple", [
        "Expand out procedure arguments when the argument type",
        "is a tuple or a type with exactly one functor.",
        "Note: this is almost always a pessimization."])).
optdb(oc_opt_hh, optopt_tuple,                            bool_special,
    priv_help("tuple", [
        "Try to find opportunities for procedures to pass some",
        "arguments to each other as a tuple rather than as",
        "individual arguments.",
        "Note: so far this has mostly a detrimental effect."])).
optdb(oc_opt_hh, optopt_tuple_trace_counts_file,          string_special,
    priv_arg_help("tuple-trace-counts-file", "filename", [
        "Supply a trace counts summary file for the tupling",
        "transformation. The summary should be made from a sample",
        "run of the program you are compiling, compiled without",
        "optimizations."])).
optdb(oc_opt_hh, optopt_tuple_costs_ratio,                int_special,
    priv_arg_help("tuple-costs-ratio", "ratio", [
        "A value of 110 for this parameter means the tupling",
        "transformation will transform a procedure if it thinks",
        "that procedure would be 10% worse, on average, than",
        "whatever transformed version of the procedure it has in",
        "mind. The default is 100."])).
optdb(oc_opt_hh, optopt_tuple_min_args,                   int_special,
    priv_arg_help("tuple-min-args", "min-num-args", [
        "The minimum number of input arguments that the tupling",
        "transformation will consider passing together as a",
        "tuple. This is mostly to speed up the compilation",
        "process by not pursuing (presumably) unfruitful searches."])).
optdb(oc_opt_hh, optopt_always_spec_dep_par_conjs,        bool_special,
    % This is for measurements by implementors only.
    priv_help("always-specialize-in-dep-par-conjs", [
        "When the transformation for handling dependent parallel",
        "conjunctions adds waits and/or signals around a call,",
        "create a specialized version of the called procedure, even if",
        "this is not profitable."])).
optdb(oc_opt_hh, optopt_allow_some_paths_only_waits,      bool_special,
    priv_help("allow-some-paths-only-waits", [])).
optdb(oc_opt_hh, optopt_region_analysis,                  bool_special,
    % This option is not documented because it is still experimental.
    priv_help("region-analysis", [
        "Enable the analysis for region-based memory management."])).

% XXX All the help text for these experimental options should be private.
optdb(oc_opt_hh_exp, structure_sharing_analysis,         bool(no),
    help("structure-sharing", [
        "Perform structure sharing analysis."])).
optdb(oc_opt_hh_exp, structure_sharing_widening,         int(0),
    arg_help("structure-sharing-widening", "n", [
        "Perform widening when the set of structure sharing pairs becomes",
        "larger than <n>. When n=0, widening is not enabled.",
        "(default: 0)."])).
optdb(oc_opt_hh_exp, structure_reuse_analysis,           bool(no),
    alt_help("structure-reuse", ["ctgc"], [
        "Perform structure reuse analysis (Compile Time Garbage",
        "Collection)."])).
optdb(oc_opt_hh_exp, structure_reuse_constraint,
                                        string("within_n_cells_difference"),
    alt_arg_help("structure-reuse-constraint",
            ["ctgc-constraint"],
            "{same_cons_id, within_n_cells_difference}", [
        "Constraint on the way we allow structure reuse. `same_cons_id'",
        "specifies that reuse is only allowed between terms of the same",
        "type and constructor. `within_n_cells_difference' states that",
        "reuse is allowed as long as the arities between the reused term",
        "and new term does not exceed a certain threshold. The threshold",
        "needs to be set using `--structure-reuse-constraint-arg'.",
        "(default: within_n_cells_difference, with threshold 0)"])).
optdb(oc_opt_hh_exp, structure_reuse_constraint_arg,     int(0),
    alt_arg_help("structure-reuse-constraint-arg",
            ["ctgc-constraint-arg"], "max_difference", [
        "Specify the maximum difference in arities between the terms that",
        "can be reused, and the terms that reuse these terms.",
        "(default: 0)"])).
optdb(oc_opt_hh_exp, structure_reuse_max_conditions,     int(10),
    priv_arg_help("structure-reuse-max-conditions", "max_num_conditions", [
        "Soft limit on the number of reuse conditions to accumulate",
        "for a procedure. (default: 10)"])).
optdb(oc_opt_hh_exp, structure_reuse_repeat,             int(0),
    priv_arg_help("structure-reuse-repeat", "num_repeats", [])).
optdb(oc_opt_hh_exp, structure_reuse_free_cells,         bool(no),
    % This option is likely to break many optimisations
    % which haven't been updated.
    priv_help("structure-reuse-free-cells", [
        "Immediately free cells which are known to be dead but which",
        "cannot be reused."])).

    % HLDS -> {LLDS,MLDS}

optdb(oc_opt_hlm, optopt_smart_indexing,                  bool_special,
    help("smart-indexing", [
        "Implement switches using the fastest applicable",
        "implementation method, which may be e.g. binary search",
        "or a hash table. With --no-smart-indexing, the default is",
        "to implement switches as simple if-then-else chains."])).
% The following options are for developers only --they provide
% finer grained control over smart indexing.
optdb(oc_opt_hlm, optopt_smart_atomic_indexing,           bool_special,
    priv_help("smart-atomic-indexing", [
        "Generate smart switches on atomic types when appropriate."])).
optdb(oc_opt_hlm, optopt_smart_string_indexing,           bool_special,
    priv_help("smart-string-indexing", [
        "Generate smart switches on strings when appropriate."])).
optdb(oc_opt_hlm, optopt_smart_tag_indexing,              bool_special,
    priv_help("smart-tag-indexing", [
        "Generate smart switches on discriminated union types",
        "when appropriate."])).
optdb(oc_opt_hlm, optopt_smart_float_indexing,            bool_special,
    priv_help("smart-float-indexing", [
        "Generate smart switches on floats when appropriate."])).
optdb(oc_opt_hlm, optopt_dense_switch_req_density,        int_special,
    arg_help("dense-switch-req-density", "percentage", [
        "The jump table generated for an atomic switch",
        "must have at least this percentage of full slots (default: 25)."])).
optdb(oc_opt_hlm, optopt_lookup_switch_req_density,       int_special,
    arg_help("lookup-switch-req-density", "percentage", [
        "The jump table generated for an atomic switch",
        "in which all the outputs are constant terms",
        "must have at least this percentage of full slots (default: 25)."])).
optdb(oc_opt_hlm, optopt_dense_switch_size,               int_special,
    arg_help("dense-switch-size", "n", [
        "The jump table generated for an atomic switch",
        "must have at least this many entries (default: 4)."])).
optdb(oc_opt_hlm, optopt_lookup_switch_size,              int_special,
    arg_help("lookup-switch-size", "n", [
        "The lookup table generated for an atomic switch",
        "must have at least this many entries (default: 4)."])).
optdb(oc_opt_hlm, optopt_string_trie_switch_size,         int_special,
    alt_arg_help("string-trie-switch-size",
            ["string-trie-size"], "n", [
        "The trie generated for a string switch",
        "must have at least this many entries (default: 16)."])).
optdb(oc_opt_hlm, optopt_string_hash_switch_size,         int_special,
    alt_arg_help("string-hash-switch-size",
            ["string-switch-size"], "n", [
        "The hash table generated for a string switch",
        "must have at least this many entries (default: 8)."])).
optdb(oc_opt_hlm, optopt_string_binary_switch_size,       int_special,
    arg_help("string-binary-switch-size", "n", [
        "The binary search table generated for a string switch",
        "must have at least this many entries (default: 4)."])).
optdb(oc_opt_hlm, optopt_tag_switch_size,                 int_special,
    arg_help("tag-switch-size", "n", [
        "The number of alternatives in a tag switch",
        "must be at least this number (default: 3)."])).
% The next two options are only for performance tests.
optdb(oc_opt_hlm, optopt_switch_single_rec_base_first,    bool_special,
    priv_help("switch-single-rec-base-first", [
        "In a switch with two arms, one a base case and one with a single",
        "recursive call, put the base case first."])).
optdb(oc_opt_hlm, optopt_switch_multi_rec_base_first,     bool_special,
    priv_help("switch-multi-rec-base-first", [
        "In a switch with two arms, one a base case and one with multiple",
        "recursive calls, put the base case first."])).
optdb(oc_opt_hlm, optopt_static_ground_cells,             bool_special,
    % XXX cf const-struct
    help("static-ground-terms", [
        "Enable the optimization of constructing constant ground terms",
        "at compile time and storing them as static constants.",
        "Note that auxiliary data structures created by the compiler",
        "for purposes such as debugging will always be created as",
        "static constants."])).
optdb(oc_opt_hlm, optopt_use_atomic_cells,                bool_special,
    help("use-atomic-cells", [
        "Use the atomic variants of the Boehm gc allocator calls",
        "when the cell to be allocated cannot contain pointers."])).

    % MLDS -> MLDS

optdb(oc_opt_mm, optopt_optimize_mlds,                    bool_special,
    alt_help("mlds-optimize",
            ["mlds-optimise"], [
        "Enable the MLDS->MLDS optimization passes."])).
optdb(oc_opt_mm, optopt_peep_mlds,                        bool_special,
    help("mlds-peephole", [
        "Perform peephole optimization of the MLDS."])).
optdb(oc_opt_hm, optopt_mlds_tailcalls,                   bool_special,
    alt_help("optimize-tailcalls",
            ["optimise-tailcalls"], [
        "Turn self-tailcalls into loops."])).
optdb(oc_opt_mm, optopt_initializations,                  bool_special,
    alt_help("optimize-initializations",
            ["optimise-initializations"], [
        "Whenever possible,",
        "convert the first assignment to each local variable",
        "in the target code into an initializer on its declaration."])).
optdb(oc_opt_mm, optopt_eliminate_unused_mlds_assigns,    bool_special,
    % This is useful for developers only.
    priv_help("eliminate-unused-mlds-assigns", [
        "Eliminate assignments to dead variables in the MLDS."])).
optdb(oc_opt_mm, optopt_eliminate_local_vars,             bool_special,
    help("eliminate-local-vars", [
        "Eliminate local variables with known values, where possible,",
        "by replacing occurrences of such variables with their values."])).
% From the user point of view, the next options control an MLDS optimization.
optdb(oc_opt_hh, optopt_generate_trail_ops_inline,        bool_special,
    help("generate-trail-ops-inline", [
        "Generate inline code for trailing operations.",
        "With `--no-generate-trail-ops-inline', the compiler",
        "will implement them using calls to those operations",
        "in the standard library."])).
optdb(oc_opt_hm, optimize_trail_usage,                    bool(no),
    % This option is developer-only.
    % It is intended for the benchmarking of trail usage optimization.
    % Otherwise, it should not be turned off, as doing so interferes with
    % the results of the trail usage analysis.
    priv_help("optimize-trail-usage", [
        "Try to restrict trailing operations to those parts of the program",
        "that actually use the trail."])).

    % HLDS -> LLDS

% XXX reword some of the following help texts.
optdb(oc_opt_hl, optopt_try_switch_size,                  int_special,
    arg_help("try-switch-size", "n", [
        "The number of alternatives in a try/retry chain switch",
        "must be at least this number (default: 3)."])).
optdb(oc_opt_hl, optopt_binary_switch_size,               int_special,
    arg_help("binary-switch-size", "n", [
        "The number of alternatives in a binary search switch",
        "must be at least this number (default: 4)."])).
optdb(oc_opt_hl, optopt_middle_rec,                       bool_special,
    help("middle-rec", [
        "Enable the middle recursion optimization."])).
optdb(oc_opt_hl, optopt_simple_neg,                       bool_special,
    help("simple-neg", [
        "Generate simplified code for simple negations."])).
optdb(oc_opt_hl, optopt_allow_hijacks,                    bool_special,
    priv_help("allow-hijacks", [
        "When appropriate, generate code to hijack nondet stack frames",
        "that possibly belongs to another procedure invocation."])).
optdb(oc_opt_hl, optimize_region_ops,                     bool(no),
    priv_help("optimize-region-ops", [
        "Try and restrict region operations to those parts of the program",
        "that actually use regions."])).

    % LLDS -> LLDS

optdb(oc_opt_ll, optopt_common_data,                      bool_special,
    help("common-data", [
        "Enable optimization of common data structures."])).
optdb(oc_opt_ll, optopt_common_layout_data,               bool_special,
    help("common-layout-data", [
        "Enable optimization of common subsequences in layout structures."])).
optdb(oc_opt_ll, optopt_optimize_llds,                    bool_special,
    alt_help("llds-optimize",
            ["llds-optimise"], [
        "Enable the low-level optimization passes."])).
optdb(oc_opt_ll, optopt_peep_llds,                        bool_special,
    alt_help("optimize-peep",
            ["optimise-peep"], [
        "Enable local peephole optimizations."])).
optdb(oc_opt_ll, optopt_peep_llds_mkword,                 bool_special,
    % This is useful for developers only, to test whether a gcc bug
    % has been fixed.
    priv_alt_help("optimize-peep-mkword",
            ["optimise-peep-mkword"], [
        "Enable peephole optimizations of words created by mkword."])).
optdb(oc_opt_ll, optopt_jumps,                            bool_special,
    alt_help("optimize-jumps",
            ["optimise-jumps"], [
        "Enable the short-circuiting of jumps to jumps."])).
optdb(oc_opt_ll, optopt_fulljumps,                        bool_special,
    alt_help("optimize-fulljumps",
            ["optimise-fulljumps"], [
        "Enable the elimination of jumps to ordinary code."])).
optdb(oc_opt_ll, optopt_pessimize_tailcalls,              bool_special,
    help("pessimize-tailcalls", [
        "Disable the optimization of tailcalls."])).
optdb(oc_opt_ll, optopt_checked_nondet_tailcalls,         bool_special,
    help("checked-nondet-tailcalls", [
        "Convert nondet calls into tail calls whenever possible, even",
        "when this requires a runtime check. This option tries to",
        "minimize stack consumption, possibly at the expense of speed."])).
optdb(oc_opt_ll, optopt_use_local_vars,                   bool_special,
    help("use-local-vars", [
        "Use local variables in C code blocks wherever possible."])).
optdb(oc_opt_ll, optopt_local_var_access_threshold,       int_special,
    priv_arg_help("local-var-access-threshold", "XXX document me", [])).
optdb(oc_opt_ll, optopt_standardize_labels,               bool_special,
    % This is useful for developers only.
    priv_alt_help("standardize-labels",
            ["standardise-labels"], [
        "Standardize internal labels in the generated code."])).
optdb(oc_opt_ll, optopt_labels,                           bool_special,
    alt_help("optimize-labels",
            ["optimise-labels"], [
        "Delete dead labels and code."])).
optdb(oc_opt_ll, optopt_dups,                             bool_special,
    alt_help("optimize-dups",
            ["optimise-dups"], [
        "Enable elimination of duplicate code within procedures."])).
optdb(oc_opt_ll, optopt_proc_dups,                        bool_special,
    alt_help("optimize-proc-dups",
            ["optimise-proc-dups"], [
        "Enable elimination of duplicate procedures."])).
optdb(oc_opt_ll, optopt_frames,                           bool_special,
    alt_help("optimize-frames",
            ["optimise-frames"], [
        "Optimize the operations that maintain stack frames."])).
optdb(oc_opt_ll, optopt_delay_slot,                       bool_special,
    alt_help("optimize-delay-slot",
            ["optimise-delay-slot"], [
        "Disable branch delay slot optimizations,",
        "This option is meaningful only if",
        "the target architecture has delay slots."])).
optdb(oc_opt_ll, optopt_reassign,                         bool_special,
    alt_help("optimize-reassign",
            ["optimise-reassign"], [
        "Optimize away assignments to locations that already hold",
        "the assigned value."])).
optdb(oc_opt_ll, optopt_repeat_opts,                      int_special,
    alt_arg_help("optimize-repeat",
            ["optimise-repeat"], "n", [
        "Iterate most optimizations at most <n> times (default: 3)."])).
optdb(oc_opt_ll, optopt_layout_compression_limit,         int_special,
    arg_help("layout-compression-limit", "n", [
        "Attempt to compress the layout structures used by the debugger",
        "only as long as the arrays involved have at most <n> elements",
        "(default: 4000)."])).

    % LLDS -> C

optdb(oc_opt_lc, optopt_use_macro_for_redo_fail,          bool_special,
    help("use-macro-for-redo-fail", [
        "Emit the fail or redo macro instead of a branch",
        "to the fail or redo code in the runtime system.",
        "This produces slightly bigger but slightly faster code."])).
optdb(oc_opt_lc, optopt_emit_c_loops,                     bool_special,
    help("emit-c-loops", [
        "Use C loop contstructs to implement loops.",
        "With `--no-emit-c-loops', use only gotos."])).
optdb(oc_opt_lc, optopt_procs_per_c_function,             int_special,
    alt_arg_help("procs-per-c-function",
            ["procs-per-C-function"], "n", [
        "Put the code for up to <n> Mercury",
        "procedures in a single C function. The default",
        "value of <n> is one. Increasing <n> can produce",
        "slightly more efficient code, but makes compilation slower."])).
optdb(oc_opt_lc, optopt_local_thread_engine_base,         bool_special,
    help("local-thread-engine-base", [
        "Do not copy the thread-local Mercury engine base address",
        "into a local variable, even when this would be appropriate.",
        "This option is effective only in low-level parallel grades",
        "that do not use the GNU C global register variables extension."])).
% optopt_inline_alloc works by giving a flag to the C compiler, but
% from the user's point of view, it is about giving different code
% to the C compiler.
optdb(oc_opt_lc, optopt_inline_alloc,                     bool_special,
    help("inline-alloc", [
        "Inline calls to GC_malloc().",
        "This can improve performance a fair bit,",
        "but may significantly increase code size.",
        "This option has no effect if `--gc boehm'",
        "is not set or if the C compiler is not GNU C."])).

optdb(oc_analysis, termination_enable,                 bool(no),
    alt_help("enable-termination", ["enable-term"], [
        "Analyse each predicate to discover if it terminates."])).
optdb(oc_analysis, termination_check,                  bool(no),
    alt_help("check-termination",
            ["check-term", "chk-term"], [
        "Enable termination analysis, and emit warnings for some",
        "predicates or functions that cannot be proved to terminate.",
        "In many cases where the compiler is unable to prove termination",
        "the problem is either a lack of information about the",
        "termination properties of other predicates, or because language",
        "constructs (such as higher order calls) were used which could",
        "not be analysed. In these cases the compiler does not emit a",
        "warning of non-termination, as it is likely to be spurious."])).
optdb(oc_analysis, termination_check_verbose,          bool(no),
    alt_help("verbose-check-termination",
            ["verb-check-term", "verb-chk-term"], [
        "Enable termination analysis, and emit warnings for all",
        "predicates or functions that cannot be proved to terminate."])).
optdb(oc_analysis, termination_single_args,            int(0),
    alt_arg_help("termination-single-argument-analysis",
            ["term-single-arg"], "n", [
        "When performing termination analysis, try analyzing",
        "recursion on single arguments in strongly connected",
        "components of the call graph that have up to <n> procedures.",
        "Setting this limit to zero disables single argument analysis."])).
optdb(oc_analysis, termination_norm,                   string("total"),
    alt_arg_help("termination-norm",
            ["term-norm"], "{simple, total, num-data-elems}", [
        "The norm defines how termination analysis measures the size",
        "of a memory cell. The `simple' norm says that size is always",
        "one. The `total' norm says that it is the number of words",
        "in the cell. The `num-data-elems' norm says that it is the",
        "number of words in the cell that contain something other",
        "than pointers to cells of the same type."])).
optdb(oc_analysis, termination_error_limit,            int(3),
    alt_arg_help("termination-error-limit",
            ["term-err-limit"], "n", [
        "Print at most <n> reasons for any single termination error",
        "(default: 3)."])).
optdb(oc_analysis, termination_path_limit,             int(256),
    alt_arg_help("termination-path-limit",
            ["term-path-limit"], "n", [
        "Perform termination analysis only on predicates",
        "with at most <n> paths (default: 256)."])).
    % The termination2_* options are used to control the new termination
    % analyser. They are currently undocumented because that is still
    % a work-in-progress. XXX Or is it?
optdb(oc_analysis, termination2_enable,                bool(no),
    priv_alt_help("enable-termination2", ["enable-term2"], [
        "Analyse each predicate to discover if it terminates.",
        "This uses an alternative termination analysis based",
        "on convex constraints."])).
optdb(oc_analysis, termination2_check,                 bool(no),
    priv_alt_help("check-termination2",
            ["check-term2", "chk-term2"], [
        "Enable the alternative termination analysis, and emit warnings for",
        "some predicates or functions that cannot be proved to terminate.",
        "In many cases where the compiler is unable to prove termination",
        "the problem is either a lack of information about the",
        "termination properties of other predicates, or because language",
        "constructs (such as higher order calls) were used which could",
        "not be analysed. In these cases the compiler does not emit a",
        "warning of non-termination, as it is likely to be spurious."])).
optdb(oc_analysis, termination2_check_verbose,         bool(no),
    priv_alt_help("verbose-check-termination2",
            ["verb-check-term2", "verb-chk-term2"], [
        % XXX These options used to have no documentation at all.
        % The following is my guess (zs).
        "Report more verbose errors from the alternative termination",
        "analysis algorithm"])).
optdb(oc_analysis, termination2_norm,                  string("total"),
    priv_alt_arg_help("termination2-norm",
            ["term2-norm"], "{simple, total, num-data-elems}", [
        "Tell the alternative termination analyser which norm to use.",
        "See the description of the `--termination-norm' option for a",
        "description of the different types of norm available."])).
optdb(oc_analysis, termination2_widening_limit,        int(4),
    priv_alt_arg_help("termination2-widening-limit",
            ["term2-widening-limit"], "n", [
        "Set the threshold for the number of iterations after which the",
        "argument size analyser invokes widening."])).
optdb(oc_analysis, termination2_prop_fail_constrs,     bool(yes),
    priv_alt_help("termination2-propagate-failure-constraints",
            ["term2-propagate-failure-constraints",
            "term2-propagate-failure-constrs"], [
        "Make the argument analyser infer information about the sizes of",
        "any inputs to a goal in contexts where that goal fails."])).
optdb(oc_analysis, termination2_maximum_matrix_size,   int(70),
    % XXX This matrix size is just a guess.
    priv_alt_arg_help("termination2-maximum-matrix-size",
            ["term2-max-matrix-size"], "n", [
        "Limit the sizes of constraints systems in the analyser to <n>",
        "constraints. Use approximations of some constraint operations,",
        "such as projection, if this threshold is exceeded. This will",
        "speed up the analysis at the cost of reduced precision."])).
optdb(oc_analysis, termination2_arg_size_only,         bool(no),
    % This option is for developers only.
    % It is useful for benchmarking the argument size analysis.
    priv_alt_help("term2-argument-size-analysis-only",
            ["term2-arg-size-analysis-only", "arg-size-analysis-only"], [
        "Perform argument size analysis on each SCC but do not",
        "attempt to infer termination,"])).

optdb(oc_analysis, analyse_exceptions,                 bool(no),
    help("analyse-exceptions", [
        "Enable exception analysis. Identify those",
        "procedures that will not throw an exception.",
        "Some optimizations can make use of this information."])).
optdb(oc_analysis, analyse_closures,                   bool(no),
    % XXX The options controlling closure analysis are currently
    % commented out because it isn't useful. It can be uncommented when
    % we actually have something that uses it.
    priv_alt_help("analyse-local-closures",
            ["analyse-closures"], [
        "Enable closure analysis. Try to identify the possible",
        "values that higher-order valued variables can take.",
        "Some optimizations can make use of this information."])).
optdb(oc_analysis, analyse_trail_usage,                bool(no),
    help("analyse-trail-usage", [
        "Enable trail usage analysis. Identify those",
        "procedures that will not modify the trail.",
        "This information is used to reduce the overhead",
        "of trailing."])).
optdb(oc_analysis, analyse_mm_tabling,                 bool(no),
    help("analyse-mm-tabling", [
        "Identify those goals that do not call procedures",
        "that are evaluated using minimal model tabling.",
        "This information is used to reduce the overhead",
        "of minimal model tabling."])).

%---------------------------------------------------------------------------%

    % Target code compilation options.

optdb(oc_target_comp, target_debug,                    bool(no),
    % XXX I, zs, think that we should have separate debug options
    % for each target language that actually supports
    % useful target language debugging. I have no idea whether Java
    % and C# do.
    % XXX The "java-debug" name exists, but the description does not mention
    % Java.
    % XXX The description *does* mention C#, but the "csharp-debug" name
    % does NOT exist.
    alt_help("target-debug",
            ["c-debug", "java-debug"], [
        "Enable debugging of the generated target code.",
        "If the target language is C, this has the same effect as",
        "`--c-debug' (see below).",
        "If the target language is C#, this causes the compiler to",
        "pass `/debug' to the C# compiler."])).
optdb(oc_target_comp, warn_target_code,                bool(yes),
    % XXX Does this apply to Java and C# too?
    help("warn-target-code", [
        "Disable warnings from the compiler (e.g. gcc)",
        "that processes the target code generated by mmc."])).

%---------------------%

    % C

optdb(oc_target_c, cc,                              string("gcc"),
    % The `mmc' script will override the default with a value
    % determined at configuration time.
    arg_help("cc", "compiler-name", [
        "Specify which C compiler to use."])).
optdb(oc_target_c, c_compiler_type,                 string("gcc"),
    priv_arg_help("c-compiler-type",
        "{gcc,clang,msvc_x86,msvc_x64,unknown}", [])).
    % The `mmc' script will override the default with a value
    % determined at configuration time for the above two options.
    % XXX That argues for this option being oc_config.
optdb(oc_target_c, optopt_c_optimize,               bool_special,
    alt_help("c-optimize",
            ["c-optimise"], [
        "Do not enable optimizations by the C compiler."])).
optdb(oc_target_c, c_include_directories,           accumulating([]),
    % The `mmc' script will override the default with a value
    % determined at configuration time.
    alt_arg_help("c-include-directory",
            ["c-include-dir"], "dir", [
        "Append <dir> to the list of directories to be searched for",
        "C header files. Note that if you want to override",
        "this list, rather than append to it, then you can set the",
        "`MERCURY_MC_ALL_C_INCL_DIRS' environment variable to a",
        "sequence of `--c-include-directory' options."])).
optdb(oc_target_c, cflags,                          accumulating([]),
    arg_help("cflags", "<options", [
        "Specify options to be passed to the C compiler.",
        "These options will not be quoted when passed to the shell."])).
optdb(oc_target_c, quoted_cflag,                    string_special,
    arg_help("cflag", "option", [
        "Specify a single word option to be passed to the C compiler.",
        "The word will be quoted when passed to the shell."])).
optdb(oc_target_c, gcc_flags,                       accumulating([]),
    % XXX part of mmc --make; but needs more detail.
    priv_arg_help("gcc-flags", "flags", [])).
optdb(oc_target_c, quoted_gcc_flag,                 string_special,
    % XXX document me.
    priv_arg_help("gcc-flag", "flag", [])).
optdb(oc_target_c, clang_flags,                     accumulating([]),
    % XXX part of mmc --make; but needs more detail.
    priv_arg_help("clang-flags", "flags", [])).
optdb(oc_target_c, quoted_clang_flag,               string_special,
    % XXX document me.
    priv_arg_help("clang-flag", "flag", [])).
optdb(oc_target_c, msvc_flags,                      accumulating([]),
    % XXX part of mmc --make; but needs more detail.
    priv_arg_help("msvc-flags", "flags", [])).
optdb(oc_target_c, quoted_msvc_flag,                string_special,
    % XXX document me.
    priv_arg_help("msvc-flag", "flag", [])).
% XXX All of the following options are reserved for the mmc script,
% but they nevertheless should have private help text.
optdb(oc_target_c, cflags_for_warnings,             string(""),
    % The `mmc' script will override the default with values
    % determined at configuration time.
    priv_arg_help("cflags-for-warnings", "flags", [])).
optdb(oc_target_c, cflags_for_sanitizers,           string(""),
    priv_arg_help("cflags-for-sanitizers", "flags", [])).
optdb(oc_target_c, cflags_for_optimization,         string("-O"),
    priv_arg_help("cflags-for-optimization", "flags", [])).
optdb(oc_target_c, cflags_for_regs,                 string(""),
    priv_arg_help("cflags-for-regs", "flags", [])).
optdb(oc_target_c, cflags_for_gotos,                string(""),
    priv_arg_help("cflags-for-gotos", "flags", [])).
optdb(oc_target_c, cflags_for_threads,              string(""),
    priv_arg_help("cflags-for-threads", "flags", [])).
optdb(oc_target_c, cflags_for_debug,                string("-g"),
    priv_arg_help("cflags-for-debug", "flags", [])).
optdb(oc_target_c, cflags_for_pic,                  string(""),
    priv_arg_help("cflags-for-pic", "flags", [])).
optdb(oc_target_c, cflags_for_lto,                  string(""),
    priv_arg_help("cflags-for-lto", "flags", [])).
optdb(oc_target_c, c_flag_to_name_object_file,      string("-o "),
    priv_arg_help("c-flag-to-name-object-file", "flags", [])).
optdb(oc_target_c, object_file_extension,           string(".o"),
    priv_arg_help("object-file-extension", "extension", [])).
optdb(oc_target_c, pic_object_file_extension,       string(".o"),
    priv_arg_help("pic-object-file-extension", "extension", [])).

%---------------------%

    % Java

optdb(oc_target_java, java_compiler,                   string("javac"),
    alt_arg_help("java-compiler", ["javac"], "javac", [
        "Specify which Java compiler to use. The default is `javac'."])).
optdb(oc_target_java, java_interpreter,                string("java"),
    arg_help("java-interpreter", "java", [
        "Specify which Java interpreter to use.",
        "The default is `java'."])).
optdb(oc_target_java, java_compiler_flags,             accumulating([]),
    alt_arg_help("javac-flags", ["java-flags"], "options", [
        "Specify options to be passed to the Java compiler.",
        "These options will not be quoted when passed to the shell."])).
optdb(oc_target_java, quoted_java_compiler_flag,       string_special,
    alt_arg_help("javac-flag",
            ["java-flag"], "option", [
        "Specify a single word option to be passed to the Java compiler.",
        "The word will be quoted when passed to the shell."])).
optdb(oc_target_java, java_classpath,                  accumulating([]),
    arg_help("java-classpath", "path", [
        "Set the classpath for the Java compiler and interpreter."])).
optdb(oc_target_java, java_runtime_flags,              accumulating([]),
    arg_help("java-runtime-flags", "options", [
        "Specify options to be passed to the Java interpreter.",
        "These options will not be quoted when passed to the shell."])).
optdb(oc_target_java, quoted_java_runtime_flag,        string_special,
    arg_help("java-runtime-flag", "option", [
        "Specify a single word option to be passed to the Java interpreter.",
        "The word will be quoted when passed to the shell."])).

%---------------------%

    % C#

optdb(oc_target_csharp, csharp_compiler,                 string("csc"),
    arg_help("csharp-compiler", "csc", [
        "Specify the name of the C# Compiler. The default is `csc'."])).
optdb(oc_target_csharp, cli_interpreter,                 string(""),
    arg_help("cli-interpreter", "prog", [
        "Specify the program that implements the Common Language",
        "Infrastructure (CLI) execution environment, e.g. `mono'."])).
optdb(oc_target_csharp, csharp_compiler_type,         string("mono"),
    % The `mmc' script will override the default with a value
    % determined at configuration time for the above two options.
    priv_arg_help("csharp-compiler-type", "{microsoft,mono,unknown}", [])).
optdb(oc_target_csharp, csharp_flags,                    accumulating([]),
    arg_help("csharp-flags", "options", [
        "Specify options to be passed to the C# compiler.",
        "These options will not be quoted when passed to the shell."])).
optdb(oc_target_csharp, quoted_csharp_flag,              string_special,
    arg_help("csharp-flag", "option", [
        "Specify a single word option to be passed to the C# compiler.",
        "The word will be quoted when passed to the shell."])).

%---------------------------------------------------------------------------%

    % Link Options.
    %
    % XXX Several of the options in this section do not affect linking, but
    % instead affect compilation (of things other than Mercury modules).
    % These include init_files, trace_init_files, runtime_flags,
    % extra_init_functions, mkinit_command, and maybe more.

optdb(oc_link_c_cs_j, mercury_library_directory_special, string_special,
    % NOTE mercury_library_directory_special sets search_directories,
    % c_include_directories and mercury_library_directories.
    alt_arg_help("mercury-library-directory",
            ["mld"], "directory", [
        "Append <directory> to the list of directories to",
        "be searched for Mercury libraries. This will add",
        "`--search-directory', `--library-directory',",
        "`--init-file-directory' and `--c-include-directory'",
        "options as needed."])).
optdb(oc_link_c_cs_j, mercury_library_directories,       accumulating([]),
    no_help).
optdb(oc_link_c_cs_j, search_library_files_directory_special, string_special,
    % NOTE search_library_files_directory_special sets search_directories,
    % c_include_directories and search_library_files_directories.
    % XXX This list just above DIFFERS from the list below.
    alt_arg_help("search-library-files-directory",
            ["search-lib-files-dir"], "directory", [
        "Search <directory> for Mercury library files that have not yet",
        "been installed. Similar to adding <directory> using all of the",
        "`--search-directory', `--intermod-directory',",
        "`--library-directory', `--init-file-directory' and",
        "`--c-include-directory' options."])).
optdb(oc_link_c_cs_j, search_library_files_directories,  accumulating([]),
    no_help).
optdb(oc_link_c_cs_j, mercury_library_special,           string_special,
    % NOTE mercury_library_special adds to the values of link_libraries,
    % mercury_libraries and init_files.
    alt_arg_help("mercury-library", ["ml"], "library", [
        "Link with the specified Mercury library."])).
optdb(oc_link_c_cs_j, mercury_libraries,                 accumulating([]),
    no_help).
optdb(oc_link_c_cs_j, mercury_standard_library_directory_special,
                                                     maybe_string_special,
    % NOTE mercury_standard_library_directory_special sets
    % mercury_standard_library_directory and mercury_configuration_directory.
    alt_arg_help("mercury-standard-library-directory",
            ["mercury-stdlib-dir"], "directory", [
        "Search <directory> for the Mercury standard library.",
        "Implies `--mercury-library-directory <directory>'",
        "and `--mercury-configuration-directory <directory>'.",
        "The negative version, --no-mercury-standard-library-directory",
        "tells the compiler not to use the Mercury standard library,",
        "and also implies `--no-mercury-configuration-directory'."])).
optdb(oc_link_c_cs_j, mercury_standard_library_directory, maybe_string(no),
    % The Mercury.config file will set the default
    % standard library directory.
    no_help).

% XXX The internal and external names of the next two options
% seem to be different for no good reason.
optdb(oc_link_c_cs,   link_library_directories,          accumulating([]),
    short_arg_help('L', "library-directory", [], "directory", [
        "Append <directory> to the list of directories in which",
        "to search for libraries."])).
optdb(oc_link_c_cs,   link_libraries,                    accumulating([]),
    short_arg_help('l', "library", [], "library", [
        "Link with the specified library."])).

optdb(oc_link_c,      output_file_name,                       string(""),
    % If the output_file_name is an empty string, we use the name
    % of the first module on the command line.
    short_arg_help('o', "output-file", [], "filename", [
        "Specify the name of the final executable.",
        "(The default executable name is the same as the name",
        "of the first module on the command line.)",
        "This option is ignored by", "QUOTE", "`mmc --make'."])).
optdb(oc_link_c,      link_objects,                      accumulating([]),
    arg_help("link-object", "file", [
        "Link with the specified object or archive file."])).
optdb(oc_link_c,      ld_flags,                          accumulating([]),
    arg_help("ld-flags", "options", [
        "Specify options to be passed to the linker command",
        "invoked by ml to link an executable.",
        "These options will not be quoted when passed to the shell.",
        "Use `ml --print-link-command' to find out which",
        "command is used."])).
optdb(oc_link_c,      quoted_ld_flag,                    string_special,
    arg_help("ld-flag", "option", [
        "Specify a single word option to be passed to the linker command",
        "invoked by ml to link an executable.",
        "The word will be quoted when passed to the shell.",
        "Use `ml --print-link-command' to find out which",
        "command is used."])).
optdb(oc_link_c,      ld_libflags,                       accumulating([]),
    arg_help("ld-libflags", "options", [
        "Specify options to be passed to the linker command",
        "invoked by ml to link a shared library.",
        "These options will not be quoted when passed to the shell.",
        "Use `ml --print-shared-lib-link-command' to find out",
        "which command is used."])).
optdb(oc_link_c,      quoted_ld_libflag,                 string_special,
    arg_help("ld-libflag", "option", [
        "Specify a single word option to be passed to the linker command",
        "invoked by ml to link a shared library.",
        "The word will be quoted when passed to the shell.",
        "Use `ml --print-shared-lib-link-command' to find out",
        "which command is used."])).
optdb(oc_link_c,      runtime_link_library_directories,  accumulating([]),
    short_arg_help('R', "runtime-library-directory", [], "directory", [
        "Append <directory> to the list of directories in which",
        "to search for shared libraries at runtime."])).
optdb(oc_link_c,      use_default_runtime_library_directory, bool(yes),
    % The use_default_runtime_library_directory option is only ever used
    % to decide whether to modify the value of the
    % runtime_link_library_directories option.
    help("default-runtime-library-directory", [
        "Do not add any directories to the runtime search path",
        "automatically."])).
optdb(oc_link_c,      init_file_directories,             accumulating([]),
    arg_help("init-file-directory", "directory", [
        "Append <directory> to the list of directories to",
        "be searched for `.init' files by c2init."])).
optdb(oc_link_c,      init_files,                        accumulating([]),
    arg_help("init-file", "init-file", [
        "Append <init-file> to the list of `.init' files to",
        "be passed to c2init."])).
optdb(oc_link_c,      trace_init_files,                  accumulating([]),
    arg_help("trace-init-file", "init-file", [
        "Append <init-file> to the list of `.init' files to",
        "be passed to c2init when tracing is enabled."])).
% XXX The options
%   linkage_special
%   only_globals_linkage
%   mercury_linkage_special
%   only_globals_mercury_linkage
%   only_globals_library_install_linkages
% are all stringly typed. As the name says, we use the only_globals_* options
% only to construct the globals; after that point, we use three strongly
% typed fields in the globals.
% NOTE linkage_special first checks and then sets only_globals_linkage and
% only_globals_mercury_linkage.
optdb(oc_link_c,      linkage_special,                 string_special,
    arg_help("linkage", "{shared, static}", [
        "Specify whether to use shared or static linking for",
        "executables. Shared libraries are always linked",
        "with `--linkage shared'."])).
optdb(oc_link_c,      only_globals_linkage,            string("shared"),
    no_help).
% NOTE mercury_linkage_special first checks and then sets
% only_globals_mercury_linkage.
optdb(oc_link_c,      mercury_linkage_special,         string_special,
    arg_help("mercury-linkage", "{shared, static}", [
        "Specify whether to use shared or static linking when",
        "linking an executable with Mercury libraries.",
        "Shared libraries are always linked with",
        "`--mercury-linkage shared'."])).
optdb(oc_link_c,      only_globals_mercury_linkage,    string("shared"),
    no_help).
optdb(oc_link_c,      demangle,                        bool(yes),
    help("demangle", [
        "Do not pipe the output of the linker through the Mercury demangler.",
        "The demangler usually makes it easier to understand the diagnostics",
        "for any link errors that involve code generated by mmc."])).
optdb(oc_link_c,      strip,                           bool(yes),
    help("strip", [
        "Do not invoke the strip command on executables.",
        "Stripping minimizes executables' sizes,",
        "but also makes debugging them much harder."])).
optdb(oc_link_c,      main,                            bool(yes),
    help("main", [
        "Do not generate a C main() function. With `--no-main',",
        "the user's own code must provide a main() function."])).
optdb(oc_link_c,      allow_undefined,                 bool(yes),
    help("allow-undefined", [
        "Do not allow undefined symbols in shared libraries."])).
optdb(oc_link_c,      use_readline,                    bool(yes),
    help("use-readline", [
        "Disable use of the readline library in the debugger."])).
optdb(oc_link_c,      runtime_flags,                   accumulating([]),
    arg_help("runtime-flags", "flags", [
        "Specify flags to pass to the Mercury runtime."])).
optdb(oc_link_c,      extra_init_functions,            bool(no),
    alt_help("extra-initialization-functions",
            ["extra-inits"], [
        "Search `.c' files for extra initialization functions.",
        "(This may be necessary if the C files contain",
        "hand-coded C code with `INIT' comments, rather than",
        "containing only C code that was automatically generated",
        "by the Mercury compiler.)"])).
optdb(oc_link_c,      frameworks,                      accumulating([]),
    arg_help("framework", "framework", [
        "Build and link against the specified framework.",
        "(Mac OS X only.)"])).
optdb(oc_link_c,      framework_directories,           accumulating([]),
    short_arg_help('F', "framework-directory", [], "directory", [
        "Append the specified directory to the framework search path.",
        "(Mac OS X only.)"])).
optdb(oc_link_c,      cstack_reserve_size,             int(-1),
    arg_help("cstack-reserve-size", "size", [
        "Set the total size of the C stack in virtual memory for",
        "executables. The stack size is given in bytes.",
        "(Microsoft Windows only.)"])).
optdb(oc_link_c,      link_executable_command,         string("gcc"),
    arg_help("link-executable-command", "command", [
        "Specify the command used to invoke the linker when linking",
        "an executable."])).
optdb(oc_link_c,      link_shared_lib_command,         string("gcc -shared"),
    arg_help("link-shared-lib-command", "command", [
        "Specify the command used to invoke the linker when linking",
        "a shared library."])).
optdb(oc_link_c,  shlib_linker_install_name_path,  string(""),
    arg_help("shlib-linker-install-name-path", "directory", [
        "Specify the path where a shared library will be installed.",
        "This option is useful on systems where the runtime search",
        "path is obtained from the shared library and not via the",
        "-R option above (such as Mac OS X)."])).
optdb(oc_link_c,  strip_executable_command,        string(""),
    arg_help("strip-executable-command", "command", [
        "Specify the command used to strip executables if no linker",
        "flag to do so is available. This option has no effect on ml."])).
optdb(oc_link_c,  strip_executable_shared_flags,   string(""),
    arg_help("strip-executable-shared-flags", "options", [
        "Specify options to pass to the strip executable command when",
        "linking against Mercury shared libraries."])).
optdb(oc_link_c,  strip_executable_static_flags,   string(""),
    arg_help("strip-executable-static-flags", "options", [
        "Specify options to pass to the strip executable command when",
        "linking against Mercury static libraries."])).
optdb(oc_link_c, shared_lib_not_executable,                bool(no),
    % XXX Improve this documentation.
    % XXX The name of the option is misleading about its purpose.
    priv_help("compile-to-shared-lib", [
        "This option is intended only for use by the debugger's",
        "interactive query facility."])).

optdb(oc_link_csharp, sign_assembly,                   string(""),
    arg_help("sign-assembly", "keyfile", [
        "Sign the current assembly with the strong name contained",
        "in the specified key file.",
        "(This option is only meaningful when generating library",
        "assemblies with the C# back-end.)"])).

optdb(oc_link_java,   java_archive_command,            string("jar"),
    arg_help("java-archive-command", "command", [
        "Specify the command used to produce Java archive (JAR) files."])).

%---------------------------------------------------------------------------%

optdb(oc_mconfig,  create_archive_command,          string("ar"),
    priv_arg_help("create-archive-command", "command", [])).
optdb(oc_mconfig,  create_archive_command_flags,    accumulating([]), % "cr"
    priv_arg_help("create-archive-command-flags", "flags", [])).
optdb(oc_mconfig,  create_archive_command_output_flag, string(""),
    priv_arg_help("create-archive-command-output-flag", "flag", [])).
optdb(oc_mconfig,  demangle_command,                string("mdemangle"),
    priv_arg_help("demangle-command", "command", [])).
optdb(oc_mconfig,  executable_file_extension,       string(""),
    priv_arg_help("executable-file-extension", "extension", [])).
optdb(oc_mconfig,  filtercc_command,                string("mfiltercc"),
    priv_arg_help("filtercc-command", "command", [])).
optdb(oc_config,   filterjavac_command,             string("mfilterjavac"),
    priv_arg_help("filterjavac-command", "command", [])).
optdb(oc_mconfig,  hwloc_libs,                      string(""),
    priv_arg_help("hwloc-libs", "XXX document me", [])).
optdb(oc_mconfig,  hwloc_static_libs,               string(""),
    priv_arg_help("hwloc-static-libs", "XXX document me", [])).
optdb(oc_mconfig,  library_extension,               string(".a"),
    priv_arg_help("library-extension", "extension", [])).
optdb(oc_mconfig,  linker_allow_undefined_flag,     string(""),
    priv_arg_help("linker-allow-undefined-flag", "flag", [])).
optdb(oc_mconfig,  linker_debug_flags,              string("-g"),
    priv_arg_help("linker-debug-flags", "flags", [])).
optdb(oc_mconfig,  linker_error_undefined_flag,    string("-Wl,-no-undefined"),
    priv_arg_help("linker-error-undefined-flag", "flag", [])).
optdb(oc_mconfig,  linker_link_lib_flag,            string("-l"),
    priv_arg_help("linker-link-lib-flag", "flag", [])).
optdb(oc_mconfig,  linker_link_lib_suffix,          string(""),
    priv_arg_help("linker-link-lib-suffix", "extension", [])).
optdb(oc_mconfig,  linker_lto_flags,                string(""),
    priv_arg_help("linker-lto-flags", "flags", [])).
optdb(oc_mconfig,  linker_opt_separator,            string(""),
    priv_arg_help("linker-opt-separator", "separator", [])).
optdb(oc_mconfig,  linker_path_flag,                string("-L"),
    priv_arg_help("linker-path-flag", "flag", [])).
optdb(oc_mconfig,  linker_rpath_flag,               string("-Wl,-rpath"),
    priv_arg_help("linker-rpath-flag", "flag", [])).
optdb(oc_mconfig,  linker_rpath_separator,          string(" -Wl,-rpath"),
    priv_arg_help("linker-rpath-separator", "separator", [])).
optdb(oc_mconfig,  linker_sanitizer_flags,          string(""),
    priv_arg_help("linker-sanitizer-flags", "flag", [])).
optdb(oc_mconfig,  linker_static_flags,             string("-static"),
    priv_arg_help("linker-static-flags", "flags", [])).
optdb(oc_mconfig,  linker_strip_flag,               string("-s"),
    priv_arg_help("linker-strip-flag", "flag", [])).
optdb(oc_mconfig,  linker_thread_flags,             string(""),
    priv_arg_help("linker-thread-flags", "flags", [])).
optdb(oc_mconfig,  linker_trace_flags,              string(""),
    priv_arg_help("linker-trace-flags", "flags", [])).
optdb(oc_mconfig,  math_lib,                        string(""),
    priv_arg_help("math-lib", "library", [])).
optdb(oc_mconfig,  mkinit_command,                  string("mkinit"),
    priv_arg_help("mkinit-command", "command", [])).
optdb(oc_mconfig,  ranlib_command,                  string(""),
    priv_arg_help("ranlib-command", "command", [])).
optdb(oc_mconfig,  ranlib_flags,                    string(""),
    priv_arg_help("ranlib-flags", "flags", [])).
optdb(oc_mconfig,  readline_libs,                   string(""),
    priv_arg_help("readline-libs", "XXX document me", [])).
optdb(oc_mconfig,  shared_library_extension,        string(".so"),
    % The `mmc' script will override the default with a value
    % determined at configuration time.
    % XXX *Which* "configuration time" does this mean?
    priv_arg_help("shared-library-extension", "extension", [])).
optdb(oc_mconfig,  shared_libs,                     string(""),
    priv_arg_help("shared-libs", "XXX document me", [])).
optdb(oc_mconfig,  shlib_linker_debug_flags,        string("-g"),
    priv_arg_help("shlib-linker-debug-flags", "flags", [])).
optdb(oc_mconfig,  shlib_linker_install_name_flag,  string("-install_name "),
    priv_arg_help("shlib-linker-install-name-flag", "flag", [])).
optdb(oc_mconfig,  shlib_linker_link_lib_flag,      string("-l"),
    priv_arg_help("shlib-linker-link-lib-flag", "flag", [])).
optdb(oc_mconfig,  shlib_linker_link_lib_suffix,    string(""),
    priv_arg_help("shlib-linker-link-lib-suffix", "extension", [])).
optdb(oc_mconfig,  shlib_linker_rpath_flag,         string("-Wl,-rpath"),
    priv_arg_help("shlib-linker-rpath-flag", "flags", [])).
optdb(oc_mconfig,  shlib_linker_rpath_separator,    string(" -Wl,-rpath"),
    priv_arg_help("shlib-linker-rpath-separator", "separator", [])).
optdb(oc_mconfig,  shlib_linker_thread_flags,       string(""),
    priv_arg_help("shlib-linker-thread-flags", "flags", [])).
optdb(oc_mconfig,  shlib_linker_trace_flags,        string(""),
    priv_arg_help("shlib-linker-trace-flags", "flags", [])).
optdb(oc_mconfig,  shlib_linker_use_install_name,   bool(no),
    priv_help("shlib-linker-use-install-name", [])).
optdb(oc_mconfig,  thread_libs,                     string(""),
    priv_arg_help("thread-libs", "library", [])).
optdb(oc_mconfig,  trace_libs,                      string(""),
    priv_arg_help("trace-libs", "library", [])).
optdb(oc_mconfig,  install_method,                  string("external"),
    priv_arg_help("install-method", "XXX document me", [])).
optdb(oc_mconfig,  use_symlinks,                    bool(yes),
    priv_help("use-symlinks", [])).
optdb(oc_mconfig, target_arch,                      string(""),
    priv_arg_help("target-arch", "architecture", [])).

%---------------------------------------------------------------------------%

    % "mmc --make" options.

optdb(oc_make, keep_going,                         bool(no),
    short_help('k', "keep-going", [], [
        "With `--make', keep going as far as possible",
        "even if an error is detected."])).
optdb(oc_make, order_make_by_timestamp,            bool(no),
    help("order-make-by-timestamp", [
        "Make", "QUOTE", "`mmc --make'", "compile",
        "more recently modified source files first."])).
optdb(oc_make, show_make_times,                    bool(no),
    help("show-make-times", [
        "Report run times for commands executed by",
        "QUOTE", "`mmc --make'."])).
optdb(oc_make, make_max_jobs,                      int(1),
    short_arg_help('j', "jobs", [], "n", [
        "With `--make', attempt to perform up to <n> jobs concurrently."])).
optdb(oc_make, make_track_flags,                   bool(no),
    alt_help("track-flags", ["track-options"], [
        "With `--make', keep track of the options used when compiling",
        "each module. If an option for a module is added or removed,",
        "QUOTE", "`mmc --make'", "will then know to recompile",
        "the module even if the timestamp on the file itself",
        "has not changed. Warning, verbosity and build system options",
        "are not tracked."])).
optdb(oc_make, make_pre_link_command,              maybe_string(no),
    arg_help("pre-link-command", "command", [
        "Specify a command to run before linking with",
        "QUOTE", "`mmc --make'.",
        "This can be used to compile C source files which rely on",
        "header files generated by the Mercury compiler.",
        "The command will be passed the names of all of the source",
        "files in the program or library, with the source file",
        "containing the main module given first."])).

%---------------------------------------------------------------------------%

    % Search options

optdb(oc_search, options_search_directories,          accumulating(["."]),
    arg_help("options-search-directory", "dir", [
        "Add <dir> to the list of directories to be searched for",
        "options files."])).
optdb(oc_search, setting_only_use_subdirs,            bool(no),
    help("use-subdirs", [
        "Generate intermediate files in a `Mercury' subdirectory,",
        "rather than generating them in the current directory."])).
optdb(oc_search, setting_only_use_grade_subdirs,      bool(no),
    help("use-grade-subdirs", [
        "Generate intermediate files in a `Mercury' subdirectory,",
        "laid out so that multiple grades can be built simultaneously.",
        "Executables and libraries will be symlinked or copied into",
        "the current directory.",
        "`--use-grade-subdirs' does not work with Mmake (it does",
        "work with", "QUOTE", "`mmc --make')."])).
optdb(oc_search, search_directories,                  accumulating(["."]),
    short_arg_help('I', "search-directory", [], "dir", [
        "Append <dir> to the list of directories to be searched",
        "for `.int*' and `.module_dep' files."])).
optdb(oc_search, intermod_directories,                accumulating([]),
    arg_help("intermod-directory", "dir", [
        "Add <dir> to the list of directories to be searched",
        "for `.opt' and `.trans_opt' files."])).
optdb(oc_search, use_search_directories_for_intermod, bool(yes),
    help("use-search-directories-for-intermod", [
        "With `--use-search-directories-for-intermod', the compiler",
        "will add the arguments of `--search-directory' options",
        "to the list of directories to search for `.opt' files.",
        "With `--no-use-search-directories-for-intermod', the compiler",
        "will use use only the arguments of",
        "`--intermod-directory' options."])).
optdb(oc_search, interface_dirs_same_subdir_setting,  accumulating([]),
    priv_alt_arg_help("interface-dir-same-workspace",
        ["interface-dir-same-ws"], "dir", [])).
optdb(oc_search, interface_dirs_indep_subdir_setting, accumulating([]),
    priv_alt_arg_help("interface-dir-independent-workspace",
        ["interface-dir-indep-ws"], "dir", [])).
optdb(oc_search, interface_dirs_installed_library,    accumulating([]),
    priv_alt_arg_help("interface-dir-installed-library",
        ["interface-dir-installed-lib"], "dir", [])).
optdb(oc_search, intermod_dirs_same_subdir_setting,   accumulating([]),
    priv_alt_arg_help("intermod-dir-same-workspace",
        ["intermod-dir-same-ws"], "dir", [])).
optdb(oc_search, intermod_dirs_indep_subdir_setting,  accumulating([]),
    priv_alt_arg_help("intermod-dir-independent-workspace",
        ["intermod-dir-indep-ws"], "dir", [])).
optdb(oc_search, intermod_dirs_installed_library,     accumulating([]),
    priv_alt_arg_help("intermod-dir-installed-library",
        ["intermod-dir-installed-lib"], "dir", [])).
optdb(oc_search, c_incl_dirs_same_subdir_setting,     accumulating([]),
    priv_alt_arg_help("c-include-dir-same-workspace",
        ["c-incl-dir-same-ws"], "dir", [])).
optdb(oc_search, c_incl_dirs_indep_subdir_setting,    accumulating([]),
    priv_alt_arg_help("c-include-dir-independent-workspace",
        ["c-incl-dir-indep-ws"], "dir", [])).
optdb(oc_search, c_incl_dirs_installed_library,       accumulating([]),
    priv_alt_arg_help("c-include-dir-installed-library",
        ["c-incl-dir-installed-lib"], "dir", [])).
optdb(oc_search, c_incl_dirs_external,                accumulating([]),
    priv_alt_arg_help("c-include-dir-external",
        ["c-incl-dir-external"], "dir", [])).
optdb(oc_search, mer_lib_dirs_same_subdir_setting,    accumulating([]),
    priv_alt_arg_help("mercury-library-dir-same-workspace",
        ["mer-lib-dir-same-ws"], "dir", [])).
optdb(oc_search, mer_lib_dirs_indep_subdir_setting,   accumulating([]),
    priv_alt_arg_help("mercury-library-dir-independent-workspace",
        ["mer-lib-dir-indep-ws"], "dir", [])).
optdb(oc_search, mer_lib_dirs_installed_library,      accumulating([]),
    priv_alt_arg_help("mercury-library-dir-installed-library",
        ["mer-lib-dir-installed-lib"], "dir", [])).

%---------------------------------------------------------------------------%

    % Build system options.

optdb(oc_buildsys, extra_init_command,             maybe_string(no),
    arg_help("extra-init-command", "command", [
        "Specify a command to produce extra entries in the `.init'",
        "file for a library.",
        "The command will be passed the names of all of the source",
        "files in the program or library, with the source file",
        "containing the main module given first."])).
optdb(oc_buildsys, install_prefix,                 string("/usr/local/"),
    arg_help("install-prefix", "dir", [
        "The directory under which to install Mercury libraries."])).
optdb(oc_buildsys, detect_stdlib_grades,           bool(yes),
    alt_help("detect-stdlib-grades",
            ["detect-libgrades"], [
        "Do not scan the installation directory to determine",
        "which standard library grades are available."])).
% XXX Add more descriptive names for the libgrade* options.
optdb(oc_buildsys, library_install_grades,         accumulating(["stdlib"]),
    % XXX We should add --library-install-grade as the *preferred* name
    % of this option.
    alt_arg_help("library-grade",
            ["libgrade"], "grade", [
        "The positive form adds <grade> to the list of compilation grades",
        "in which a library to be installed should be built.",
        "(The list is initialized to the set of grades in which",
        "the standard library is installed.)",
        "The negative form clears the list of compilation grades in which",
        "a library to be installed should be built."])).
optdb(oc_buildsys, library_install_grades_incl_components, accumulating([]),
    alt_arg_help("libgrades-include-component",
            ["libgrades-include"], "grade_component", [
        "Remove grades that do not contain the specified component from",
        "the set of library grades to be installed.",
        "(This option works only with", "QUOTE", "`mmc --make';",
        "it does not work with Mmake.)"])).
optdb(oc_buildsys, library_install_grades_excl_components, accumulating([]),
    alt_arg_help("libgrades-exclude-component",
            ["libgrades-exclude"], "grade_component", [
        "Remove grades that contain the specified component from the",
        "set of library grades to be installed.",
        "(This option works only with", "QUOTE", "`mmc --make';",
        "it does not work with Mmake.)"])).
optdb(oc_buildsys, only_globals_library_install_linkages, accumulating([]),
    alt_arg_help("library-install-linkage",
            ["library-linkage", "lib-linkage"], "{shared, static}", [
        "Specify whether libraries should be installed for shared",
        "or static linking. This option can be specified multiple",
        "times. By default, libraries will be installed for",
        "both shared and static linking."])).
optdb(oc_buildsys, libgrade_install_check,         bool(yes),
    help("libgrade-install-check", [
        "Do not check that libraries have been installed before",
        "attempting to use them. (This option is meaningful only with",
        "QUOTE", "`mmc --make'.)"])).
optdb(oc_buildsys, extra_library_header,           accumulating([]),
    alt_arg_help("extra-library-header",
            ["extra-lib-header"], "file", [
        "Install the specified C header file with along with",
        "a Mercury library.",
        "(This option is supported only by", "QUOTE", "`mmc --make'.)"])).

%---------------------------------------------------------------------------%

    % Options specifying properties of the environment.

    % If `--mercury-stdlib-dir' is set, `--mercury-config-dir'
    % must also be set. This invariant is maintained by the `special' variants
    % of the options.
% XXX We should standardize on either "dir" or "directory" as arg name.
% XXX We should standardize on either "file" or "filename" as arg name.
% XXX We should standardize on either "n" or "N" as arg name.
optdb(oc_env, mercury_configuration_directory_special, string_special,
    alt_arg_help("mercury-configuration-directory",
            ["mercury-config-dir"], "directory", [
        "Search <directory> for Mercury system's configuration files."])).
optdb(oc_env, mercury_configuration_directory,         maybe_string(no),
    % "--mercury-config-dir argdir" sets this option to yes(argdir).
    no_help).
optdb(oc_env, install_command,                         string("cp"),
    arg_help("install-command", "command", [
        "Specify the command to use to install the files in",
        "Mercury libraries. The given command will be invoked as",
        "`<command> <source> <target>' to install each file",
        "in a Mercury library. The default command is `cp'."])).
optdb(oc_env, options_files,      accumulating(["Mercury.options"]),
    arg_help("options-file", "file", [
        "Add <file> to the list of options files to be processed.",
        "If <file> is `-', an options file will be read from the",
        "standard input. By default the file `Mercury.options'",
        "in the current directory will be read."])).
optdb(oc_env, config_file,        maybe_string(yes("")), % yes("") means unset.
    arg_help("config-file", "file", [
        "Read the Mercury compiler's configuration information",
        "from <file>. If the `--config-file' option is not set,",
        "a default configuration will be used, unless",
        "`--no-mercury-stdlib-dir' is passed to mmc.",
        "The configuration file is just an options file."])).
optdb(oc_env, env_type,                                string_special,
    arg_help("env-type", "type", [
        "Specify the environment type in which the compiler and generated",
        "programs will be invoked.",
        "The <type> should be one of `posix', `cygwin', `msys', or",
        "`windows'.",
        "This option is equivalent to setting all of `--host-env-type',",
        "`--system-env-type' and `--target-env-type' to <type>."])).
optdb(oc_env, host_env_type,                           string("posix"),
    arg_help("host-env-type", "type", [
        "Specify the environment type in which the compiler will be",
        "invoked."])).
optdb(oc_env, system_env_type,                         string(""),
    arg_help("system-env-type", "type", [
        "Specify the environment type in which external programs invoked",
        "by the compiler will run.",
        "If not specified, this defaults to the value given by",
        "`--host-env-type'."])).
optdb(oc_env, target_env_type,                         string("posix"),
    arg_help("target-env-type", "type", [
        "Specify the environment type in which generated programs will be",
        "invoked."])).
optdb(oc_env, restricted_command_line,                 bool(no),
    help("restricted-command-line", [
        "Enable this option if your shell doesn't support long",
        "command lines. This option uses temporary files to pass arguments",
        "to sub-commands.",
        "(This option is supported only by", "QUOTE", "`mmc --make'.)"])).

%---------------------------------------------------------------------------%

    % Miscellaneous internal-use-only options.
    %
    % XXX NO, MOST ARE NOT.
    % We need a separate category (maybe named oc_int_dev?) for options
    % - whose main intended use is internal-use-only,
    % - but which can be command-line enabled by developers for experiments.
    % OR we could just delete the possibility of those experiments,
    % whose time has long passed.

optdb(oc_internal, pre_implicit_parallelism_simplify,    bool(no),
    priv_help("pre-implicit-parallelism-simplify", [])).
optdb(oc_internal, type_layout,                          bool(yes),
    priv_help("type-layout", [
        "Don't output type_ctor_layout structures or references to them.",
        "(The C code must then be compiled with `-DNO_TYPE_LAYOUT')."])).
% The next two options are not documented,
% because they are not yet tested much, and are probably not very useful,
% except for Java, where they are the default.
optdb(oc_internal, det_copy_out,                         bool(no),
    priv_help("det-copy-out", [
        "Specify whether to handle output arguments for det/semidet",
        "procedures using return-by-value rather than pass-by-reference.",
        "This option is ignored if the `--high-level-code' option",
        "is not enabled."])).
optdb(oc_internal, nondet_copy_out,                     bool(no),
    priv_help("nondet-copy-out", [
        "Specify whether to handle output arguments for nondet",
        "procedures using pass-by-value rather than pass-by-reference.",
        "This option is ignored if the `--high-level-code' option",
        "is not enabled."])).
% The --put-commit-in-own-func option is not documented because
% it is enabled automatically (by handle_options) in the situations
% where it is needed; the user should never need to set it.
optdb(oc_internal, put_commit_in_own_func,              bool(no),
    priv_help("put-commit-in-own-func", [
        "Put each commit in its own C function.",
        "This option only affects the MLDS back-ends.",
        "It is needed for the high-level C back-end,",
        "where commits are implemented via setjmp()/longjmp(),",
        "since longjmp() may clobber any non-volatile local vars",
        "in the function that called setjmp()."])).
optdb(oc_internal, backend_foreign_languages,       accumulating([]),
    % The backend_foreign_languages option depends on the target,
    % and is set in handle_options, BUT can be set on the command line.
    % It makes no sense to do so, but ...
    priv_arg_help("backend-foreign-languages", "{c/c#/csharp/java}", [])).
optdb(oc_internal, stack_trace,                     bool(no), no_help).
optdb(oc_internal, basic_stack_layout,              bool(no),
    priv_help("basic-stack-layout", [
        "Generate the simple stack_layout structures required",
        "for stack traces."])).
optdb(oc_internal, agc_stack_layout,                bool(no),
    priv_help("agc-stack-layout", [
        "Generate the stack_layout structures required for",
        "accurate garbage collection."])).
optdb(oc_internal, procid_stack_layout,             bool(no),
    priv_help("procid-stack-layout", [
        "Generate the stack_layout structures required for",
        "looking up procedure identification information."])).
optdb(oc_internal, trace_stack_layout,              bool(no),
    priv_help("trace-stack-layout", [
        "Generate the stack_layout structures required for",
        "execution tracing."])).
optdb(oc_internal, body_typeinfo_liveness,          bool(no),
    priv_help("body-typeinfo-liveness", [
        "Ensure that whenever a variable whose type contains",
        "a type variable is live, the type_info for that type variable",
        "is available."])).
optdb(oc_internal, can_compare_constants_as_ints,   bool(no),
    priv_help("can-compare-constants-as-ints", [])).
optdb(oc_internal, pretest_equality_cast_pointers,  bool(no),
    priv_help("pretest-equality-cast-pointers", [])).
optdb(oc_internal, delay_partial_instantiations,    bool(no),
    priv_help("delay-partial-instantiations", [])).
optdb(oc_internal, allow_defn_of_builtins,          bool(no),
    priv_help("allow-defn-of-builtins", [])).
optdb(oc_internal, type_ctor_info,                  bool(yes),
    priv_help("type-ctor-info", [])).
optdb(oc_internal, type_ctor_layout,                bool(yes),
    priv_help("type-ctor-layout", [])).
optdb(oc_internal, type_ctor_functors,              bool(yes),
    priv_help("type-ctor-functors", [])).
optdb(oc_internal, rtti_line_numbers,               bool(yes),
    priv_help("rtti-line-numbers", [])).
optdb(oc_internal, new_type_class_rtti,             bool(no),
    priv_help("new-type-class-rtti", [])).
optdb(oc_internal, use_mmsc_pneg,                   bool(no), no_help).
optdb(oc_internal, use_mmsc_cut,                    bool(no), no_help).
    % The size_* values below *must* be consistent with the corresponding
    % values or data structures in runtime/mercury_region.h.
optdb(oc_internal, size_region_ite_fixed,           int(4),
    priv_arg_help("size-region-ite-fixed", "num_words", [])).
optdb(oc_internal, size_region_disj_fixed,          int(4),
    priv_arg_help("size-region-disj-fixed", "num_words", [])).
optdb(oc_internal, size_region_commit_fixed,        int(5),
    priv_arg_help("size-region-commit-fixed", "num_words", [])).
optdb(oc_internal, size_region_ite_protect,         int(1),
    priv_arg_help("size-region-ite-protect", "num_words", [])).
optdb(oc_internal, size_region_ite_snapshot,        int(3),
    priv_arg_help("size-region-ite-snapshot", "num_words", [])).
optdb(oc_internal, size_region_semi_disj_protect,   int(1),
    priv_arg_help("size-region-semi-disj-protect", "num_words", [])).
optdb(oc_internal, size_region_disj_snapshot,       int(3),
    priv_arg_help("size-region-disj-snapshot", "num_words", [])).
optdb(oc_internal, size_region_commit_entry,        int(1),
    priv_arg_help("size-region-commit-entry", "num_words", [])).
optdb(oc_internal, allow_multi_arm_switches,        bool(yes),
    priv_help("allow-multi-arm-switches", [
        "Do not allow the compiler to generate switches",
        "in which one arm handles more than one cons_id."])).
optdb(oc_internal, reclaim_heap_on_semidet_failure,     bool(yes),
    priv_help("reclaim-heap-on-semidet-failure", [
        "Do not reclaim heap on backtracking in semidet code."])).
optdb(oc_internal, reclaim_heap_on_nondet_failure,      bool(yes),
    priv_help("reclaim-heap-on-nondet-failure", [
        "Do not reclaim heap on backtracking in nondet code."])).
optdb(oc_internal, reclaim_heap_on_failure,             bool_special,
    priv_help("reclaim-heap-on-failure", [
        "Combines the effect of the two options above."])).
optdb(oc_internal, max_specialized_do_call_closure,     int(5), no_help).
    % mercury.do_call_closure_N exists for N <= option_value;
    % set to -1 to disable. Should be less than or equal to
    % max_spec_explicit_arg in tools/make_spec_ho_call.
optdb(oc_internal, max_specialized_do_call_class_method, int(6), no_help).
    % mercury.do_call_class_method_N exists for N <= option_value;
    % set to -1 to disable. Should be less than or equal to
    % max_spec_explicit_arg in tools/make_spec_method_call.
optdb(oc_internal, compare_specialization,              int(-1),
    % -1 asks handle_options.m to give the value, which may be grade dependent.
    priv_arg_help("compare-specialization", "n", [
        "Generate quadratic instead of linear compare predicates for",
        "types with up to n function symbols. Higher values of n lead to",
        "faster but also bigger compare predicates."])).
optdb(oc_internal, chosen_stdlib_dir,               maybe_string(no), no_help).
optdb(oc_internal, default_globals,                    bool(no), no_help).
optdb(oc_internal, local_module_id,                 accumulating([]),
    priv_arg_help("local-module-id", "XXX document me", [])).
optdb(oc_internal, generate_item_version_numbers,         bool(no),
    unnamed_help([
        "This option is used to control output of version numbers",
        "in interface files. It is implied by --smart-recompilation,",
        "and cannot be set explicitly by the user.",
        "Even if this option is set to `yes', version numbers may have",
        "been disabled with io_set_disable_generate_item_version_numbers.",
        "Before using the value of this option, call",
        "io_get_disable_generate_item_version_numbers to see whether this",
        "has been done."])).
optdb(oc_internal, generate_mmc_make_module_dependencies, bool(no),
    priv_alt_help("generate-mmc-make-module-dependencies",
            ["generate-mmc-deps"], [])).
optdb(oc_internal, optopt_static_ground_floats,         bool_special, no_help).
optdb(oc_internal, optopt_static_ground_int64s,         bool_special, no_help).
optdb(oc_internal, optopt_static_code_addresses,        bool_special, no_help).

%---------------------------------------------------------------------------%

    % Options that developers can use to ask for optional compiler actions.

optdb(oc_dev_ctrl, progress_output_suffix,              string(""),
    priv_arg_help("progress-output-suffix", ".xyz", [
        "When compiling module M, output messages about the progress",
        "of the compilation to a file named `M.xyz'. This includes any",
        "statistics about the performance of compiler passes, if enabled.",
        "The default is for such output to go to standard error."])).
optdb(oc_dev_ctrl, error_output_suffix,                 string(""),
    priv_arg_help("error-output-suffix", ".xyz", [
        "When compiling module M, output any error, warning and/or",
        "informational messages about the module to a file named `M.xyz'.",
        "The default is for such output to go to standard error."])).
optdb(oc_dev_ctrl, inference_output_suffix,             string(""),
    priv_arg_help("inference-output-suffix", ".xyz", [
        "When compiling module M, output the results of any type and/or",
        "mode inference to a file named `M.xyz'.",
        "The default is for such output to go to standard error."])).
optdb(oc_dev_ctrl, debug_output_suffix,                 string(""),
    priv_arg_help("debug-output-suffix", ".xyz", [
        "When compiling module M, direct output that is intended to",
        "help debug the compiler to a file named `M.xyz'.",
        "The default is for such output to go to standard error."])).
optdb(oc_dev_ctrl, recompile_output_suffix,             string(""),
    priv_arg_help("recompile-output-suffix", ".xyz", [
        "This is intended to direct the output from the test cases in
        tests/recompilation to a file."])).

optdb(oc_dev_ctrl,   mode_constraints,                 bool(no),
    priv_help("mode-constraints", [
        "Run constraint based mode analysis. The default is to use",
        "the robdd solution using the full (subtyping) constraints,",
        "and to dump its results."])).
optdb(oc_dev_ctrl,   simple_mode_constraints,          bool(no),
    priv_help("simple-mode-constraints", [
        "Use only the simplified constraint system when running",
        "the robdd solver constraints based mode analysis."])).
optdb(oc_dev_ctrl,   prop_mode_constraints,            bool(no),
    priv_alt_help("propagate-mode-constraints",
            ["prop-mode-constraints"], [
        "Use the new propagation solver for constraints based",
        "mode analysis."])).
optdb(oc_dev_ctrl,   compute_goal_modes,               bool(no),
    priv_help("compute-goal-modes", [
        "Compute goal modes."])).
optdb(oc_dev_ctrl,   smart_recompilation,              bool(no),
    help("smart-recompilation", [
        "When compiling, write program dependency information",
        "to be used to avoid unnecessary recompilations if an",
        "imported module's interface changes in a way which does",
        "not invalidate the compiled code. `--smart-recompilation'",
        "does not yet work with `--intermodule-optimization'."])).
optdb(oc_dev_ctrl, pre_prof_transforms_simplify,       bool(no),
    priv_help("pre-prof-transforms-simplify", [
        "Force the pre-profiling simplification pass that is usually",
        "enabled when building a profiling version of a program. This",
        "allows a developer to enable this pass when using a",
        "non-profiling build. It can be used to test that generated code",
        "introduced in earlier passes is well-formed before it is",
        "potentially removed by the later dead procedure elimination pass."])).
optdb(oc_dev_ctrl, disable_mmsc_pneg,               bool(no),
    priv_help("disable-mm-pneg", [])).
optdb(oc_dev_ctrl, disable_mmsc_cut,                bool(no),
    priv_help("disable-mm-cut", [])).
optdb(oc_dev_ctrl, disable_trail_ops,               bool(no),
    priv_help("disable-trail-ops", [])).
optdb(oc_dev_ctrl, type_check_using_constraints,    bool(no),
    priv_help("type-check-constraints", [
        "Use the constraint based type checker instead of the old one."])).
optdb(oc_dev_ctrl, trad_passes,                         bool(yes),
    priv_help("trad-passes", [
        "The default `--trad-passes' completes code generaion each predicate",
        "before going on to the next predicate.",
        "The --no-trad-passes  option tells the compiler",
        "to complete each phase of code generation on all predicates",
        "before going on the next phase on all predicates."])).
optdb(oc_dev_ctrl, parallel_liveness,                   bool(no),
    priv_help("parallel-liveness", [
        "Use multiple threads when computing liveness.",
        "At the moment this option implies `--no-trad-passes',",
        "and requires the compiler to be built in a",
        "low-level parallel grade and running with multiple engines."])).
optdb(oc_dev_ctrl, parallel_code_gen,                   bool(no),
    priv_help("parallel-code-gen", [
        "Use multiple threads when generating code.",
        "At the moment this option implies `--no-trad-passes',",
        "and requires the compiler to be built in a",
        "low-level parallel grade and running with multiple engines."])).
optdb(oc_dev_ctrl, should_pretest_equality,             bool(yes),
    priv_help("should-pretest-equality", [
        "Normally, the compiler adds to the starts of",
        "potentially expensive unify and compare predicates",
        "a test for the two values being equal as words.",
        "Specifying `--no-should-pretest-equality' prevents this."])).
optdb(oc_dev_ctrl, fact_table_max_array_size,           int(1024),
    arg_help("fact-table-max-array-size", "n", [
        "Specify the maximum number of elements in a single",
        "`:- pragma fact_table' data array (default: 1024)."])).
optdb(oc_dev_ctrl, fact_table_hash_percent_full,        int(90),
    arg_help("fact-table-hash-percent-full", "percentage", [
        "Specify how full the `:- pragma fact_table' hash tables",
        "should be allowed to get. Given as an integer percentage",
        "(valid range: 1 to 100, default: 90)."])).
optdb(oc_dev_ctrl, prefer_switch,                       bool(yes),
    % This option is private because it is not yet useful; currently
    % we don't take advantage of GNU C's computed gotos extension.
    priv_help("prefer-switch", [
        "Generate code using computed gotos rather than switches.",
        "This makes the generated code less readable,",
        "but potentially slightly more efficient.",
        "This option is effective only with `--high-level-code'."])).
optdb(oc_dev_ctrl, prefer_while_loop_over_jump_self,    bool(yes),
    % This option is intended for testing and benchmarking.
    priv_help("prefer-while-loop-over-jump-self", [
        "Generate code for tail-recursive single procedures using an",
        "infinite while loop, with tail calls being done by a continue.",
        "The alternative is a label at the start of the procedure,",
        "with tail calls being done by a jump to the label.",
        "This option is effective only with `--high-level-code'."])).
optdb(oc_dev_ctrl, prefer_while_loop_over_jump_mutual,  bool(no),
    priv_help("prefer-while-loop-over-jump-mutual", [
        "Generate code for tail-recursive-SCCs using an infinite while loop",
        "wrapped around a switch, with one switch arm for each procedure",
        "in the TSCC, with tail calls being done by setting the value of",
        "the switched-on variable and a continue. The alternative is",
        "a simple label before the code of each procedure, with tail calls",
        "being done by a jump to the label.",
        "This option has no effect unless the `--high-level-code' option",
        "is enabled."])).
optdb(oc_dev_ctrl, opt_no_return_calls,                 bool(yes),
    % This option provides the fairest test of --optimize-saved-vars-cell.
    priv_help("opt-no-return-calls", [
        "Do not optimize the stack usage of calls that cannot return."])).

optdb(oc_dev_ctrl, compiler_sufficiently_recent,       bool(no),
    alt_help("bug-intermod-2002-06-13", [
        "bug-intermod-2006-09-28",
        "bug-foreign_import-2002-08-06",
        "install-opt-files-2002-08-30",
        "read-config-file-2003-03-01",
        "no-noncompact-ho-call-2004-01-15",
        "trace-io-builtins-2006-08-14",
        "compound-compare-builtins-2007-07-09",
        "no-det-warning-compound-compare-2007-07-17",
        "foreign-enum-switch-fix",
        "failing-disjunct-in-switch-dup-fix",
        "store-at-ref-impure-2008-09-11",
        "java-export-ref-out",
        "java-generics-2010-04-13",
        "strip-executable-2014-05-05",
        "trace-goal-only-locals-2017-07-05",
        "no-reserved-addrs",
        "builtin-lt-gt-2018-10-08",
        "fixed-contiguity-2018-10-19",
        "simplest-msg-2019-09-22",
        "unqual-foreign-enums-in-int-files-2019-10-04",
        "obsolete-proc-2019-10-23",
        "type-repn-int3-2020-03-22",
        "github-85--2020-03-24",
        "foreign-proc-typeinfo-2020-04-08",
        "ushift-2020-04-30",
        "unsigned_lt-2020-05-02",
        "format-uint-2020-05-23",
        "mmake-all-2020-05-25",
        "unsigned-lt-2020-05-25",
        "may-ignore-without-warning-2020-08-18",
        "prolog-is-2020-08-21",
        "partial-inst-copy-2021-01-04",
        "mantis-bug-529-2021-02-25",
        "subtype-opt-2022-02-19",
        "typespec-pragma-2022-07-20",
        "ushift-2022-12-06",
        "ushift-2022-12-07",
        "strtrie-2022-12-08",
        "term-pass2-2022-12-28",
        "format-2023-01-27",
        "singleton-2023-06-10",
        "warn-obsolete-transform-2023-07-03",
        "gen-dep-ints-2023-10-15",
        "tscp-2024-02-07",
        "format-2024-02-07",
        "dym-2024-02-08",
        "wne-2024-02-21",
        "escape-2024-04-28",
        "can-fail-function-obsolete-2024-08-10",
        "unused-statevar-warn-2025-05-16",
        "allow-non-contig-for-2025-06-01"], [
        "Is the compiler sufficiently recent to contain the new feature",
        "or bugfix referred to by each name?"])).
optdb(oc_dev_ctrl, experiment,                         string(""),
    priv_arg_help("experiment", "experiment_name", [])).
optdb(oc_dev_ctrl, experiment1,                        bool(no),
    priv_help("experiment1", [])).
optdb(oc_dev_ctrl, experiment2,                        bool(no),
    priv_help("experiment2", [])).
optdb(oc_dev_ctrl, experiment3,                        bool(no),
    priv_help("experiment3", [])).
optdb(oc_dev_ctrl, experiment4,                        bool(no),
    priv_help("experiment4", [])).
optdb(oc_dev_ctrl, experiment5,                        bool(no),
    priv_help("experiment5", [])).
optdb(oc_dev_ctrl, allow_ho_insts_as_modes,            bool(yes),
    priv_help("allow-ho-insts-as-modes", [
        "Do not allow higher order insts to be used as modes."])).
optdb(oc_dev_ctrl, ignore_par_conjunctions,            bool(no),
    priv_help("ignore-par-conjunctions", [
        "Replace parallel conjunctions with plain ones, this is useful",
        "for benchmarking. Note that it does not affect implicit",
        "parallelism"])).
optdb(oc_dev_ctrl, control_granularity,                bool(no),
    help("control-granularity", [
        "Don't try to generate more parallelism than the machine can",
        "handle, which may be specified at runtime or detected",
        "automatically."])).
optdb(oc_dev_ctrl, distance_granularity,               int(0),
    arg_help("distance-granularity", "distance", [
        "Control the granularity of parallel execution using the",
        "specified distance value."])).
optdb(oc_dev_ctrl, implicit_parallelism,               bool(no),
    help("implicit-parallelism", [
        "Introduce parallel conjunctions where it could be worthwhile",
        "(implicit parallelism) using information generated by",
        "mdprof_create_feedback.",
        "The profiling feedback file can be specified using the",
        "`--feedback-file' option."])).
optdb(oc_dev_ctrl, feedback_file,                      string(""),
    arg_help("feedback-file", "file", [
        "Use the specified profiling feedback file which may currently",
        "only be processed for implicit parallelism."])).
optdb(oc_dev_ctrl, par_loop_control,                   bool(no),
    priv_help("par-loop-control", [])).
optdb(oc_dev_ctrl, par_loop_control_keep_tail_rec,     bool(no),
    priv_help("par-loop-control-preserve-tail-recursion", [])).
optdb(oc_dev_ctrl, optopt_enable_const_struct_poly,    bool_special,
    unnamed_help([
        "Disable the gathering of constant structures holding",
        "typeinfos and typeclass_infos in global_data structures."])).

%---------------------------------------------------------------------------%

    % Options for helping to debuug the compiler's operations.
    % Most such options are in option class oc_verb_dbg; the options here
    % are the ones that do NOT work by adding to the compiler output stream.

optdb(oc_dev_debug, table_debug,                        bool(no),
    priv_help("table-debug", [
        "Enables the generation of code that helps to debug tabling",
        "primitives."])).
optdb(oc_dev_debug, debug_class_init,                   bool(no),
    priv_help("debug-class-init", [
        "In Java grades, generate code that causes a trace of class",
        "initialization to be printed to the standard output when the",
        "environment variable MERCURY_DEBUG_CLASS_INIT is defined."])).

%---------------------------------------------------------------------------%

    % Options that the compiler does not use anymore.

optdb(oc_unused, ansi_c,                          bool(yes),
    help("ansi-c", [
        "This option is deprecated and does not have any effect."])).
optdb(oc_unused, cflags_for_ansi,                 string(""),
    priv_arg_help("cflags-for-ansi", "flags", [])).
optdb(oc_unused, install_command_dir_option,      string("-R"),
    priv_arg_help("install-command-dir-option", "flag", [])).
optdb(oc_unused, optopt_use_just_one_c_func,              bool_special,
    alt_help("everything-in-one-c-function",
            ["everything-in-one-C-function"], [
        "This option is deprecated and does not have any effect."])).

%---------------------------------------------------------------------------%

    % Options for creating dumps of the compiler's internal data structures.

optdb(oc_dev_dump,   dump_hlds,                        accumulating([]),
    short_arg_help('d', "dump-hlds", ["hlds-dump"], "stage number or name", [
        "Dump the HLDS (high level intermediate representation) after",
        "the specified stage to `<module>.hlds_dump.<num>-<name>'.",
        "Stage numbers range from 1-599.",
        "Multiple dump options accumulate."])).
optdb(oc_dev_dump,   dump_hlds_pred_id,                accumulating([]),
    arg_help("dump-hlds-pred-id", "n", [
        "Dump the HLDS only of the predicate/function with the given",
        "pred id."])).
optdb(oc_dev_dump,   dump_hlds_pred_name,              accumulating([]),
    arg_help("dump-hlds-pred-name", "name", [
        "Dump the HLDS only of the predicate/function with the given",
        "name."])).
optdb(oc_dev_dump,   dump_hlds_pred_name_order,        bool(no),
    priv_help("dump-hlds-pred-name-order", [
        "Dump the predicates in the HLDS ordered by name",
        "not ordered by pred id."])).
optdb(oc_dev_dump,   dump_hlds_spec_preds,             bool(no),
    priv_help("dump-hlds-spec-preds", [
        "With `--dump-hlds', dump the special (unify, compare, and index)",
        "predicates not in pred-id order, but in alphabetical order",
        "by type constructor."])).
optdb(oc_dev_dump,   dump_hlds_spec_preds_for,         accumulating([]),
    priv_arg_help("dump-hlds-spec-preds-for", "typename", [
        "Dump only the special (unify, compare, and index) predicates",
        "for the types named by the (possibly multiple) occurrences",
        "of this option."])).
optdb(oc_dev_dump,   dump_hlds_alias,                  string(""),
    priv_short_arg_help('D', "dump-hlds-alias", [], "dump-alias", [
        "With `--dump-hlds', include extra detail in the dump.",
        "Each dump alias is shorthand for a set of option letters.",
        "The list of aliases is in handle_options.m"])).
optdb(oc_dev_dump,   dump_hlds_options,                string(""),
    arg_help("dump-hlds-options", "options", [
        "With `--dump-hlds', include extra detail in the dump.",
        "Each type of detail is included in the dump if its",
        "corresponding letter occurs in the option argument",
        "(see the Mercury User's Guide for details)."])).
optdb(oc_dev_dump,   dump_hlds_inst_limit,             int(100),
    arg_help("dump-hlds-inst-limit", "N", [
        "Dump at most N insts in each inst table."])).
optdb(oc_dev_dump,   dump_hlds_inst_size_limit,        int(40),
    arg_help("dump-hlds-inst-size-limit", "N", [
        "Dump insts in an inst table only if their size does not exceed N."])).
optdb(oc_dev_dump,   dump_hlds_file_suffix,            string(""),
    arg_help("dump-hlds-file-suffix", "suffix", [
        "Append the given suffix to the names of the files created by",
        "the `--dump-hlds' option."])).
optdb(oc_dev_dump,   dump_same_hlds,                   bool(no),
    help("dump-same-hlds", [
        "Create a file for a HLDS stage even if the file notes only that",
        "this stage is identical to the previously dumped HLDS stage."])).
optdb(oc_dev_dump,   dump_mlds,                        accumulating([]),
    alt_arg_help("dump-mlds", ["mlds-dump"], "stage number or name", [
        "Dump the MLDS (medium level intermediate representation)",
        "after the specified stage, as C code,",
        "to`<module>.c_dump.<num>-<name>',",
        "and `<module>.mih_dump.<num>-<name>'.",
        "Stage numbers range from 1-99.",
        "Multiple dump options accumulate.",
        "This option works only in MLDS grades that target C."])).
optdb(oc_dev_dump,   dump_mlds_pred_name,              accumulating([]),
    arg_help("dump-mlds-pred-name", "pred or func name", [
        "Dump the MLDS (medium level intermediate representation)",
        "of the predicate or function with the specified name",
        "at the stages specified by the --dump-mlds option.",
        "The dump file will consist of the predicates and functions",
        "named by all the occurrences of this option (there may be",
        "more than one), and nothing else."])).
optdb(oc_dev_dump,   verbose_dump_mlds,                accumulating([]),
    alt_arg_help("verbose-dump-mlds", ["verbose-mlds-dump"],
            "stage number or name", [
        "Dump the internal compiler representation of the MLDS, after",
        "the specified stage, to `<module>.mlds_dump.<num>-<name>'.",
        "This option works in all MLDS grades."])).
optdb(oc_dev_dump,   dump_trace_counts,                accumulating([]),
    % This option is for developers only.
    priv_arg_help("dump-trace-counts", "stage number or name", [
        "If the compiler was compiled with debugging enabled and is being",
        "run with trace counting enabled, write out the trace counts file",
        "after the specified stage to `<module>.trace_counts.<num>-<name>'.",
        "Stage numbers range from 1-599.",
        "Multiple dump options accumulate."])).
optdb(oc_dev_dump,   dump_options_file,                string(""),
    priv_arg_help("dump-options-file", "output_file", [
        "Dump the internal compiler representation of files named in",
        "options-file options to output_file."])).

%---------------------------------------------------------------------------%

:- type getopt_maps
    --->    getopt_maps(
                % The map from short option names to options.
                gom_short_name_map      :: map(char, option),

                % The map from long option names to options.
                gom_long_name_map       :: map(string, option),

                % The map from options to their default values.
                gom_default_value_map   :: map(option, option_data)
            ).

:- mutable(getopt_maps_mutable, getopt_maps, get_getopt_maps, ground,
    [untrailed, constant]).

:- func get_getopt_maps = getopt_maps.

get_getopt_maps = GetoptMaps :-
    OptdbPred =
        ( pred(OptdbTuple::out) is multi :-
            optdb(_Cat, Opt, OptData, Help),
            OptdbTuple = optdb_tuple(Opt, OptData, Help)
        ),
    solutions_set(OptdbPred, OptdbTuples),
    set.foldl3(acc_optdb_data, OptdbTuples,
        [], ShortNamePairs, [], LongNamePairs, [], OptionDefaultPairs),
    list.sort(ShortNamePairs, SortedShortNamePairs),
    list.sort(LongNamePairs, SortedLongNamePairs),
    list.sort(OptionDefaultPairs, SortedOptionDefaultPairs),
    map.from_sorted_assoc_list(SortedShortNamePairs, ShortNameMap),
    map.from_sorted_assoc_list(SortedLongNamePairs, LongNameMap),
    map.from_sorted_assoc_list(SortedOptionDefaultPairs, DefaultValueMap),
    GetoptMaps = getopt_maps(ShortNameMap, LongNameMap, DefaultValueMap).

get_short_option(ShortOption) :-
    get_getopt_maps_mutable(GetoptMaps),
    GetoptMaps = getopt_maps(ShortNameMap, _LongNameMap, _DefaultValueMap),
    ShortOption = map.search(ShortNameMap).

get_long_option(LongOption) :-
    get_getopt_maps_mutable(GetoptMaps),
    GetoptMaps = getopt_maps(_ShortNameMap, LongNameMap, _DefaultValueMap),
    LongOption = map.search(LongNameMap).

get_option_default_table(DefaultOptionTable) :-
    get_getopt_maps_mutable(GetoptMaps),
    GetoptMaps = getopt_maps(_ShortNameMap, _LongNameMap, DefaultOptionTable).

%---------------------------------------------------------------------------%

all_long_option_strings(LongNames) :-
    get_getopt_maps_mutable(GetoptMaps),
    GetoptMaps = getopt_maps(_ShortNameMap, LongNameMap, _DefaultValueMap),
    map.keys(LongNameMap, LongNames).

all_negatable_long_option_strings(NegateableLongNames) :-
    get_getopt_maps_mutable(GetoptMaps),
    GetoptMaps = getopt_maps(_ShortNameMap, LongNameMap, _DefaultValueMap),
    AccNegateableLongNames =
        ( pred(LongName::in, Option::in,
                NegateableLongNames0::in,
                NegateableLongNames1::out) is det :-
            ( if
                optdb(_, Option, OptionData, _),
                ( OptionData = bool(_)
                ; OptionData = maybe_int(_)
                ; OptionData = maybe_string(_)
                ; OptionData = accumulating(_)
                ; OptionData = bool_special
                ; OptionData = maybe_string_special
                )
            then
                NegateableLongNames1 = [LongName | NegateableLongNames0]
            else
                NegateableLongNames1 = NegateableLongNames0
            )
        ),
    map.foldl(AccNegateableLongNames, LongNameMap,
        [], RevNegateableLongNames),
    list.reverse(RevNegateableLongNames, NegateableLongNames).

%---------------------------------------------------------------------------%

special_handler(Option, SpecialData, !.OptionTable, Result, !OptOptions) :-
    require_switch_arms_semidet [Option]
    (
        (
            Option = grade,
            SpecialData = string(Grade),
            ( if convert_grade_option(Grade, !OptionTable) then
                Result = ok(!.OptionTable)
            else
                Result = error("invalid grade `" ++ Grade ++ "'")
            )
        ;
            Option = linkage_special,
            SpecialData = string(Flag),
            ( if ( Flag = "shared" ; Flag = "static" ) then
                map.det_update(only_globals_mercury_linkage, string(Flag),
                    !OptionTable),
                map.det_update(only_globals_linkage, string(Flag),
                    !OptionTable),
                Result = ok(!.OptionTable)
            else
                Result = error("argument of `--linkage' should be " ++
                    "either ""shared"" or ""static"", not """ ++ Flag ++ """.")
            )
        ;
            Option = mercury_linkage_special,
            SpecialData = string(Flag),
            ( if ( Flag = "shared" ; Flag = "static" ) then
                map.det_update(only_globals_mercury_linkage, string(Flag),
                    !OptionTable),
                Result = ok(!.OptionTable)
            else
                Result = error("argument of `--mercury-linkage' should be " ++
                    "either ""shared"" or ""static"", not """ ++ Flag ++ """.")
            )
        )
    ;
        (
            Option = color_diagnostics,
            SpecialData = bool(Enable),
            map.set(color_diagnostics_is_set, bool(yes), !OptionTable),
            map.set(color_diagnostics_is_set_to, bool(Enable), !OptionTable)
        ;
            Option = color_scheme,
            SpecialData = string(ColorScheme),
            map.set(color_scheme_set_by, string("option"), !OptionTable),
            map.set(color_scheme_set_to, string(ColorScheme), !OptionTable)
        ;
            Option = color_scheme_envvar,
            SpecialData = string(ColorScheme),
            map.lookup(!.OptionTable, ignore_color_scheme_envvar, IgnoreValue),
            ( if IgnoreValue = bool(yes) then
                true
            else
                map.set(color_scheme_set_by, string("envvar"), !OptionTable),
                map.set(color_scheme_set_to, string(ColorScheme), !OptionTable)
            )
        ;
            Option = compile_to_c,
            SpecialData = none,
            map.set(target, string("c"), !OptionTable),
            map.set(only_opmode_target_code_only, bool(yes), !OptionTable)
        ;
            Option = java,
            SpecialData = none,
            map.set(target, string("java"), !OptionTable)
        ;
            Option = java_only,
            SpecialData = none,
            map.set(target, string("java"), !OptionTable),
            map.set(only_opmode_target_code_only, bool(yes), !OptionTable)
        ;
            Option = csharp,
            SpecialData = none,
            map.set(target, string("csharp"), !OptionTable)
        ;
            Option = csharp_only,
            SpecialData = none,
            map.set(target, string("csharp"), !OptionTable),
            map.set(only_opmode_target_code_only, bool(yes), !OptionTable)
        ;
            Option = profiling,
            SpecialData = bool(Profile),
            map.set(profile_time, bool(Profile), !OptionTable),
            map.set(profile_calls, bool(Profile), !OptionTable),
            map.set(profile_memory, bool(no), !OptionTable),
            map.set(profile_deep, bool(no), !OptionTable)
        ;
            Option = time_profiling,
            SpecialData = none,
            map.set(profile_time, bool(yes), !OptionTable),
            map.set(profile_calls, bool(yes), !OptionTable),
            map.set(profile_memory, bool(no), !OptionTable),
            map.set(profile_deep, bool(no), !OptionTable)
        ;
            Option = memory_profiling,
            SpecialData = none,
            map.set(profile_time, bool(no), !OptionTable),
            map.set(profile_calls, bool(yes), !OptionTable),
            map.set(profile_memory, bool(yes), !OptionTable),
            map.set(profile_deep, bool(no), !OptionTable)
        ;
            Option = deep_profiling,
            SpecialData = none,
            map.set(profile_time, bool(no), !OptionTable),
            map.set(profile_calls, bool(no), !OptionTable),
            map.set(profile_memory, bool(no), !OptionTable),
            map.set(profile_deep, bool(yes), !OptionTable)
        ;
            Option = inlining,
            SpecialData = bool(Inline),
            (
                Inline = yes,
                CompoundThreshold = 10
            ;
                Inline = no,
                CompoundThreshold = 0
            ),
            InlineOpts = cord.from_list([
                oo_inline_simple(Inline),
                oo_inline_builtins(Inline),
                oo_inline_single_use(Inline),
                oo_inline_compound_threshold(CompoundThreshold)
            ]),
            !:OptOptions = !.OptOptions ++ InlineOpts
        ;
            Option = reclaim_heap_on_failure,
            SpecialData = bool(Reclaim),
            map.set(reclaim_heap_on_semidet_failure, bool(Reclaim),
                !OptionTable),
            map.set(reclaim_heap_on_nondet_failure, bool(Reclaim),
                !OptionTable)
        ;
            Option = strict_sequential,
            SpecialData = none,
            override_options([
                    reorder_conj - bool(no),
                    reorder_disj - bool(no),
                    fully_strict - bool(yes)
                ], !OptionTable)
        ;
            Option = inhibit_warnings,
            SpecialData = bool(Inhibit),
            bool.not(Inhibit, Enable),
            DodgyOptions = dodgy_code_warning_options,
            SlowOptions = slow_code_warning_options,
            StyleOptions = style_warning_options,
            InformOptions = info_request_options,
            set_all_options_to(DodgyOptions,  bool(Enable), !OptionTable),
            set_all_options_to(SlowOptions,   bool(Enable), !OptionTable),
            set_all_options_to(StyleOptions,  bool(Enable), !OptionTable),
            set_all_options_to(InformOptions, bool(Enable), !OptionTable)
        ;
            Option = inhibit_style_warnings,
            SpecialData = bool(Inhibit),
            bool.not(Inhibit, Enable),
            StyleOptions = style_warning_options,
            set_all_options_to(StyleOptions, bool(Enable), !OptionTable)
        ;
            Option = warn_non_tail_recursion,
            SpecialData = maybe_string(MaybeRecCalls0),
            (
                MaybeRecCalls0 = yes(RecCalls0),
                RecCalls = to_lower(RecCalls0),
                (
                    RecCalls = "none",
                    WarnSelfRec = no,
                    WarnMutualRec = no
                ;
                    RecCalls = "self",
                    WarnSelfRec = yes,
                    WarnMutualRec = no
                ;
                    ( RecCalls = "self-and-mutual"
                    ; RecCalls = "both"
                    ; RecCalls = "all"
                    ),
                    WarnSelfRec = yes,
                    WarnMutualRec = yes
                )
            ;
                MaybeRecCalls0 = no,
                WarnSelfRec = no,
                WarnMutualRec = no
            ),
            map.set(warn_non_tail_recursion_self, bool(WarnSelfRec),
                !OptionTable),
            map.set(warn_non_tail_recursion_mutual, bool(WarnMutualRec),
                !OptionTable)
        ;
            Option = infer_all,
            SpecialData = bool(Infer),
            override_options([
                    infer_types                     -   bool(Infer),
                    infer_modes                     -   bool(Infer),
                    infer_det                       -   bool(Infer)
                ], !OptionTable)
        ;
            Option = optimize_saved_vars,
            SpecialData = bool(Optimize),
            SavedVarsOpts = cord.from_list([
                oo_opt_saved_vars_const(Optimize),
                oo_opt_svcell(Optimize)
            ]),
            !:OptOptions = !.OptOptions ++ SavedVarsOpts
        ;
            Option = mercury_library_directory_special,
            SpecialData = string(Dir),
            option_table_add_mercury_library_directory(Dir, !OptionTable)
        ;
            Option = search_library_files_directory_special,
            SpecialData = string(Dir),
            option_table_add_search_library_files_directory(Dir, !OptionTable)
        ;
            Option = mercury_library_special,
            SpecialData = string(Lib),
            list.foldl(append_to_accumulating_option, [
                    link_libraries                  - Lib,
                    mercury_libraries               - Lib,
                    init_files                      - (Lib ++ ".init")
                ], !OptionTable)
        ;
            Option = mercury_standard_library_directory_special,
            SpecialData = maybe_string(StdLibDir),
            MaybeStdLibDir = maybe_string(StdLibDir),
            map.set(mercury_standard_library_directory, MaybeStdLibDir,
                !OptionTable),
            map.set(mercury_configuration_directory, MaybeStdLibDir,
                !OptionTable)
        ;
            Option = mercury_configuration_directory_special,
            SpecialData = string(ConfDir),
            map.set(mercury_configuration_directory,
                maybe_string(yes(ConfDir)), !OptionTable)
        ;
            Option = quoted_cflag,
            SpecialData = string(Flag),
            handle_quoted_flag(cflags, Flag, !OptionTable)
        ;
            Option = quoted_gcc_flag,
            SpecialData = string(Flag),
            handle_quoted_flag(gcc_flags, Flag, !OptionTable)
        ;
            Option = quoted_clang_flag,
            SpecialData = string(Flag),
            handle_quoted_flag(clang_flags, Flag, !OptionTable)
        ;
            Option = quoted_msvc_flag,
            SpecialData = string(Flag),
            handle_quoted_flag(msvc_flags, Flag, !OptionTable)
        ;
            Option = quoted_java_compiler_flag,
            SpecialData = string(Flag),
            handle_quoted_flag(java_compiler_flags, Flag, !OptionTable)
        ;
            Option = quoted_java_runtime_flag,
            SpecialData = string(Flag),
            handle_quoted_flag(java_runtime_flags, Flag, !OptionTable)
        ;
            Option = quoted_csharp_flag,
            SpecialData = string(Flag),
            handle_quoted_flag(csharp_flags, Flag, !OptionTable)
        ;
            Option = quoted_ld_flag,
            SpecialData = string(Flag),
            handle_quoted_flag(ld_flags, Flag, !OptionTable)
        ;
            Option = quoted_ld_libflag,
            SpecialData = string(Flag),
            handle_quoted_flag(ld_libflags, Flag, !OptionTable)
        ;
            Option = env_type,
            SpecialData = string(EnvTypeStr),
            override_options([
                    host_env_type   - string(EnvTypeStr),
                    system_env_type - string(EnvTypeStr),
                    target_env_type - string(EnvTypeStr)
                ], !OptionTable)
        ;
            Option = inform_inferred,
            SpecialData = bool(Inform),
            override_options([
                    inform_inferred_types -   bool(Inform),
                    inform_inferred_modes -   bool(Inform)
                ], !OptionTable)
        ),
        Result = ok(!.OptionTable)
    ;
        % The part of this file between this start marker and the
        % corresponding end marker below is automatically generated by
        % tools/make_optimization_options. Do not edit.
        % INCLUDE_HANDLER_FILE_START
        (
            Option = optopt_allow_inlining,
            SpecialData = bool(Bool),
            OptOption = oo_allow_inlining(Bool)
        ;
            Option = optopt_inline_simple,
            SpecialData = bool(Bool),
            OptOption = oo_inline_simple(Bool)
        ;
            Option = optopt_inline_builtins,
            SpecialData = bool(Bool),
            OptOption = oo_inline_builtins(Bool)
        ;
            Option = optopt_inline_single_use,
            SpecialData = bool(Bool),
            OptOption = oo_inline_single_use(Bool)
        ;
            Option = optopt_inline_tr_sccs,
            SpecialData = bool(Bool),
            OptOption = oo_inline_tr_sccs(Bool)
        ;
            Option = optopt_enable_const_struct_poly,
            SpecialData = bool(Bool),
            OptOption = oo_enable_const_struct_poly(Bool)
        ;
            Option = optopt_enable_const_struct_user,
            SpecialData = bool(Bool),
            OptOption = oo_enable_const_struct_user(Bool)
        ;
            Option = optopt_common_struct,
            SpecialData = bool(Bool),
            OptOption = oo_opt_common_structs(Bool)
        ;
            Option = optopt_constraint_propagation,
            SpecialData = bool(Bool),
            OptOption = oo_prop_constraints(Bool)
        ;
            Option = optopt_local_constraint_propagation,
            SpecialData = bool(Bool),
            OptOption = oo_prop_local_constraints(Bool)
        ;
            Option = optopt_duplicate_calls,
            SpecialData = bool(Bool),
            OptOption = oo_opt_dup_calls(Bool)
        ;
            Option = optopt_constant_propagation,
            SpecialData = bool(Bool),
            OptOption = oo_prop_constants(Bool)
        ;
            Option = optopt_excess_assign,
            SpecialData = bool(Bool),
            OptOption = oo_elim_excess_assigns(Bool)
        ;
            Option = optopt_merge_code_after_switch,
            SpecialData = bool(Bool),
            OptOption = oo_merge_code_after_switch(Bool)
        ;
            Option = optopt_format_calls,
            SpecialData = bool(Bool),
            OptOption = oo_opt_format_calls(Bool)
        ;
            Option = optopt_split_switch_arms,
            SpecialData = bool(Bool),
            OptOption = oo_split_switch_arms(Bool)
        ;
            Option = optopt_loop_invariants,
            SpecialData = bool(Bool),
            OptOption = oo_opt_loop_invariants(Bool)
        ;
            Option = optopt_saved_vars_const,
            SpecialData = bool(Bool),
            OptOption = oo_opt_saved_vars_const(Bool)
        ;
            Option = optopt_svcell,
            SpecialData = bool(Bool),
            OptOption = oo_opt_svcell(Bool)
        ;
            Option = optopt_svcell_loop,
            SpecialData = bool(Bool),
            OptOption = oo_opt_svcell_loop(Bool)
        ;
            Option = optopt_svcell_full_path,
            SpecialData = bool(Bool),
            OptOption = oo_opt_svcell_full_path(Bool)
        ;
            Option = optopt_svcell_on_stack,
            SpecialData = bool(Bool),
            OptOption = oo_opt_svcell_on_stack(Bool)
        ;
            Option = optopt_svcell_candidate_headvars,
            SpecialData = bool(Bool),
            OptOption = oo_opt_svcell_candidate_headvars(Bool)
        ;
            Option = optopt_svcell_all_candidates,
            SpecialData = bool(Bool),
            OptOption = oo_opt_svcell_all_candidates(Bool)
        ;
            Option = optopt_delay_construct,
            SpecialData = bool(Bool),
            OptOption = oo_delay_constructs(Bool)
        ;
            Option = optopt_follow_code,
            SpecialData = bool(Bool),
            OptOption = oo_opt_follow_code(Bool)
        ;
            Option = optopt_unused_args,
            SpecialData = bool(Bool),
            OptOption = oo_opt_unused_args(Bool)
        ;
            Option = optopt_intermod_unused_args,
            SpecialData = bool(Bool),
            OptOption = oo_opt_unused_args_intermod(Bool)
        ;
            Option = optopt_higher_order,
            SpecialData = bool(Bool),
            OptOption = oo_opt_higher_order(Bool)
        ;
            Option = optopt_unneeded_code,
            SpecialData = bool(Bool),
            OptOption = oo_opt_unneeded_code(Bool)
        ;
            Option = optopt_type_specialization,
            SpecialData = bool(Bool),
            OptOption = oo_spec_types(Bool)
        ;
            Option = optopt_user_guided_type_specialization,
            SpecialData = bool(Bool),
            OptOption = oo_spec_types_user_guided(Bool)
        ;
            Option = optopt_introduce_accumulators,
            SpecialData = bool(Bool),
            OptOption = oo_introduce_accumulators(Bool)
        ;
            Option = optopt_lcmc_accumulator,
            SpecialData = bool(Bool),
            OptOption = oo_opt_lcmc_accumulator(Bool)
        ;
            Option = optopt_lcmc_null,
            SpecialData = bool(Bool),
            OptOption = oo_opt_lcmc_null(Bool)
        ;
            Option = optopt_lcmc,
            SpecialData = bool(Bool),
            OptOption = oo_opt_lcmc(Bool)
        ;
            Option = optopt_dead_procs,
            SpecialData = bool(Bool),
            OptOption = oo_opt_dead_procs(Bool)
        ;
            Option = optopt_deforestation,
            SpecialData = bool(Bool),
            OptOption = oo_deforest(Bool)
        ;
            Option = optopt_untuple,
            SpecialData = bool(Bool),
            OptOption = oo_untuple(Bool)
        ;
            Option = optopt_tuple,
            SpecialData = bool(Bool),
            OptOption = oo_tuple(Bool)
        ;
            Option = optopt_inline_par_builtins,
            SpecialData = bool(Bool),
            OptOption = oo_inline_par_builtins(Bool)
        ;
            Option = optopt_always_spec_dep_par_conjs,
            SpecialData = bool(Bool),
            OptOption = oo_spec_in_all_dep_par_conjs(Bool)
        ;
            Option = optopt_allow_some_paths_only_waits,
            SpecialData = bool(Bool),
            OptOption = oo_allow_some_paths_only_waits(Bool)
        ;
            Option = optopt_region_analysis,
            SpecialData = bool(Bool),
            OptOption = oo_analyse_regions(Bool)
        ;
            Option = optopt_smart_indexing,
            SpecialData = bool(Bool),
            OptOption = oo_use_smart_indexing(Bool)
        ;
            Option = optopt_smart_atomic_indexing,
            SpecialData = bool(Bool),
            OptOption = oo_use_smart_indexing_atomic(Bool)
        ;
            Option = optopt_smart_string_indexing,
            SpecialData = bool(Bool),
            OptOption = oo_use_smart_indexing_string(Bool)
        ;
            Option = optopt_smart_tag_indexing,
            SpecialData = bool(Bool),
            OptOption = oo_use_smart_indexing_tag(Bool)
        ;
            Option = optopt_smart_float_indexing,
            SpecialData = bool(Bool),
            OptOption = oo_use_smart_indexing_float(Bool)
        ;
            Option = optopt_switch_single_rec_base_first,
            SpecialData = bool(Bool),
            OptOption = oo_put_base_first_single_rec(Bool)
        ;
            Option = optopt_switch_multi_rec_base_first,
            SpecialData = bool(Bool),
            OptOption = oo_put_base_first_multi_rec(Bool)
        ;
            Option = optopt_static_ground_cells,
            SpecialData = bool(Bool),
            OptOption = oo_use_static_ground_cells(Bool)
        ;
            Option = optopt_static_ground_floats,
            SpecialData = bool(Bool),
            OptOption = oo_use_static_ground_floats(Bool)
        ;
            Option = optopt_static_ground_int64s,
            SpecialData = bool(Bool),
            OptOption = oo_use_static_ground_int64s(Bool)
        ;
            Option = optopt_static_code_addresses,
            SpecialData = bool(Bool),
            OptOption = oo_use_static_code_addresses(Bool)
        ;
            Option = optopt_use_atomic_cells,
            SpecialData = bool(Bool),
            OptOption = oo_use_atomic_cells(Bool)
        ;
            Option = optopt_middle_rec,
            SpecialData = bool(Bool),
            OptOption = oo_opt_middle_rec(Bool)
        ;
            Option = optopt_simple_neg,
            SpecialData = bool(Bool),
            OptOption = oo_opt_simple_neg(Bool)
        ;
            Option = optopt_allow_hijacks,
            SpecialData = bool(Bool),
            OptOption = oo_allow_hijacks(Bool)
        ;
            Option = optopt_mlds_tailcalls,
            SpecialData = bool(Bool),
            OptOption = oo_opt_mlds_tailcalls(Bool)
        ;
            Option = optopt_initializations,
            SpecialData = bool(Bool),
            OptOption = oo_opt_initializations(Bool)
        ;
            Option = optopt_eliminate_unused_mlds_assigns,
            SpecialData = bool(Bool),
            OptOption = oo_elim_unused_mlds_assigns(Bool)
        ;
            Option = optopt_eliminate_local_vars,
            SpecialData = bool(Bool),
            OptOption = oo_elim_local_vars(Bool)
        ;
            Option = optopt_generate_trail_ops_inline,
            SpecialData = bool(Bool),
            OptOption = oo_gen_trail_ops_inline(Bool)
        ;
            Option = optopt_common_data,
            SpecialData = bool(Bool),
            OptOption = oo_use_common_data(Bool)
        ;
            Option = optopt_common_layout_data,
            SpecialData = bool(Bool),
            OptOption = oo_use_common_layout_data(Bool)
        ;
            Option = optopt_optimize_llds,
            SpecialData = bool(Bool),
            OptOption = oo_optimize_llds(Bool)
        ;
            Option = optopt_optimize_mlds,
            SpecialData = bool(Bool),
            OptOption = oo_optimize_mlds(Bool)
        ;
            Option = optopt_peep_llds,
            SpecialData = bool(Bool),
            OptOption = oo_peep_llds(Bool)
        ;
            Option = optopt_peep_llds_mkword,
            SpecialData = bool(Bool),
            OptOption = oo_peep_llds_mkword(Bool)
        ;
            Option = optopt_peep_mlds,
            SpecialData = bool(Bool),
            OptOption = oo_peep_mlds(Bool)
        ;
            Option = optopt_jumps,
            SpecialData = bool(Bool),
            OptOption = oo_opt_jumps(Bool)
        ;
            Option = optopt_fulljumps,
            SpecialData = bool(Bool),
            OptOption = oo_opt_fulljumps(Bool)
        ;
            Option = optopt_pessimize_tailcalls,
            SpecialData = bool(Bool),
            OptOption = oo_pessimize_tailcalls(Bool)
        ;
            Option = optopt_checked_nondet_tailcalls,
            SpecialData = bool(Bool),
            OptOption = oo_opt_checked_nondet_tailcalls(Bool)
        ;
            Option = optopt_use_local_vars,
            SpecialData = bool(Bool),
            OptOption = oo_use_local_vars(Bool)
        ;
            Option = optopt_standardize_labels,
            SpecialData = bool(Bool),
            OptOption = oo_standardize_labels(Bool)
        ;
            Option = optopt_labels,
            SpecialData = bool(Bool),
            OptOption = oo_opt_labels(Bool)
        ;
            Option = optopt_dups,
            SpecialData = bool(Bool),
            OptOption = oo_opt_dups(Bool)
        ;
            Option = optopt_proc_dups,
            SpecialData = bool(Bool),
            OptOption = oo_opt_proc_dups(Bool)
        ;
            Option = optopt_frames,
            SpecialData = bool(Bool),
            OptOption = oo_opt_frames(Bool)
        ;
            Option = optopt_delay_slot,
            SpecialData = bool(Bool),
            OptOption = oo_opt_delay_slot(Bool)
        ;
            Option = optopt_reassign,
            SpecialData = bool(Bool),
            OptOption = oo_opt_reassign(Bool)
        ;
            Option = optopt_use_macro_for_redo_fail,
            SpecialData = bool(Bool),
            OptOption = oo_use_macro_for_redo_fail(Bool)
        ;
            Option = optopt_emit_c_loops,
            SpecialData = bool(Bool),
            OptOption = oo_emit_c_loops(Bool)
        ;
            Option = optopt_use_just_one_c_func,
            SpecialData = bool(Bool),
            OptOption = oo_use_just_one_c_func(Bool)
        ;
            Option = optopt_local_thread_engine_base,
            SpecialData = bool(Bool),
            OptOption = oo_use_local_thread_engine_base(Bool)
        ;
            Option = optopt_inline_alloc,
            SpecialData = bool(Bool),
            OptOption = oo_inline_alloc(Bool)
        ;
            Option = optopt_c_optimize,
            SpecialData = bool(Bool),
            OptOption = oo_opt_c(Bool)
        ;
            Option = optopt_inline_call_cost,
            SpecialData = int(N),
            OptOption = oo_inline_call_cost(N)
        ;
            Option = optopt_inline_compound_threshold,
            SpecialData = int(N),
            OptOption = oo_inline_compound_threshold(N)
        ;
            Option = optopt_inline_simple_threshold,
            SpecialData = int(N),
            OptOption = oo_inline_simple_threshold(N)
        ;
            Option = optopt_inline_vars_threshold,
            SpecialData = int(N),
            OptOption = oo_inline_vars_threshold(N)
        ;
            Option = optopt_intermod_inline_simple_threshold,
            SpecialData = int(N),
            OptOption = oo_intermod_inline_simple_threshold(N)
        ;
            Option = optopt_inline_tr_sccs_max_extra,
            SpecialData = int(N),
            OptOption = oo_inline_tr_sccs_max_extra(N)
        ;
            Option = optopt_from_ground_term_threshold,
            SpecialData = int(N),
            OptOption = oo_from_ground_term_threshold(N)
        ;
            Option = optopt_svcell_cv_store_cost,
            SpecialData = int(N),
            OptOption = oo_opt_svcell_cv_store_cost(N)
        ;
            Option = optopt_svcell_cv_load_cost,
            SpecialData = int(N),
            OptOption = oo_opt_svcell_cv_load_cost(N)
        ;
            Option = optopt_svcell_fv_store_cost,
            SpecialData = int(N),
            OptOption = oo_opt_svcell_fv_store_cost(N)
        ;
            Option = optopt_svcell_fv_load_cost,
            SpecialData = int(N),
            OptOption = oo_opt_svcell_fv_load_cost(N)
        ;
            Option = optopt_svcell_op_ratio,
            SpecialData = int(N),
            OptOption = oo_opt_svcell_op_ratio(N)
        ;
            Option = optopt_svcell_node_ratio,
            SpecialData = int(N),
            OptOption = oo_opt_svcell_node_ratio(N)
        ;
            Option = optopt_svcell_all_path_node_ratio,
            SpecialData = int(N),
            OptOption = oo_opt_svcell_all_path_node_ratio(N)
        ;
            Option = optopt_higher_order_size_limit,
            SpecialData = int(N),
            OptOption = oo_higher_order_size_limit(N)
        ;
            Option = optopt_higher_order_arg_limit,
            SpecialData = int(N),
            OptOption = oo_higher_order_arg_limit(N)
        ;
            Option = optopt_unneeded_code_copy_limit,
            SpecialData = int(N),
            OptOption = oo_opt_unneeded_code_copy_limit(N)
        ;
            Option = optopt_deforestation_depth_limit,
            SpecialData = int(N),
            OptOption = oo_deforestation_depth_limit(N)
        ;
            Option = optopt_deforestation_cost_factor,
            SpecialData = int(N),
            OptOption = oo_deforestation_cost_factor(N)
        ;
            Option = optopt_deforestation_vars_threshold,
            SpecialData = int(N),
            OptOption = oo_deforestation_vars_threshold(N)
        ;
            Option = optopt_deforestation_size_threshold,
            SpecialData = int(N),
            OptOption = oo_deforestation_size_threshold(N)
        ;
            Option = optopt_tuple_costs_ratio,
            SpecialData = int(N),
            OptOption = oo_tuple_costs_ratio(N)
        ;
            Option = optopt_tuple_min_args,
            SpecialData = int(N),
            OptOption = oo_tuple_min_args(N)
        ;
            Option = optopt_dense_switch_req_density,
            SpecialData = int(N),
            OptOption = oo_dense_switch_req_density(N)
        ;
            Option = optopt_lookup_switch_req_density,
            SpecialData = int(N),
            OptOption = oo_lookup_switch_req_density(N)
        ;
            Option = optopt_dense_switch_size,
            SpecialData = int(N),
            OptOption = oo_dense_switch_size(N)
        ;
            Option = optopt_lookup_switch_size,
            SpecialData = int(N),
            OptOption = oo_lookup_switch_size(N)
        ;
            Option = optopt_string_trie_switch_size,
            SpecialData = int(N),
            OptOption = oo_string_trie_switch_size(N)
        ;
            Option = optopt_string_hash_switch_size,
            SpecialData = int(N),
            OptOption = oo_string_hash_switch_size(N)
        ;
            Option = optopt_string_binary_switch_size,
            SpecialData = int(N),
            OptOption = oo_string_binary_switch_size(N)
        ;
            Option = optopt_tag_switch_size,
            SpecialData = int(N),
            OptOption = oo_tag_switch_size(N)
        ;
            Option = optopt_try_switch_size,
            SpecialData = int(N),
            OptOption = oo_try_switch_size(N)
        ;
            Option = optopt_binary_switch_size,
            SpecialData = int(N),
            OptOption = oo_binary_switch_size(N)
        ;
            Option = optopt_local_var_access_threshold,
            SpecialData = int(N),
            OptOption = oo_local_var_access_threshold(N)
        ;
            Option = optopt_repeat_opts,
            SpecialData = int(N),
            OptOption = oo_opt_repeat(N)
        ;
            Option = optopt_layout_compression_limit,
            SpecialData = int(N),
            OptOption = oo_layout_compression_limit(N)
        ;
            Option = optopt_procs_per_c_function,
            SpecialData = int(N),
            OptOption = oo_procs_per_c_function(N)
        ;
            Option = optopt_tuple_trace_counts_file,
            SpecialData = string(Str),
            OptOption = oo_tuple_trace_counts_file(Str)
        ;
            Option = opt_level,
            SpecialData = int(N),
            OptOption = oo_opt_level(N)
        ;
            Option = opt_space,
            SpecialData = none,
            OptOption = oo_opt_for_space
        ),
        % INCLUDE_HANDLER_FILE_END
        cord.snoc(OptOption, !OptOptions),
        Result = ok(!.OptionTable)
    ).

dodgy_code_warning_options = DodgyWarnOptions :-
    FindOptionsPred =
        ( pred(Opt::out) is nondet :-
            optdb(oc_warn_dodgy, Opt, _Data, _Help)
        ),
    solutions(FindOptionsPred, DodgyWarnOptions).

slow_code_warning_options = SlowWarnOptions :-
    FindOptionsPred =
        ( pred(Opt::out) is nondet :-
            optdb(oc_warn_perf, Opt, _Data, _Help)
        ),
    solutions(FindOptionsPred, SlowWarnOptions).

style_warning_options = StyleWarnOptions :-
    FindOptionsPred =
        ( pred(Opt::out) is nondet :-
            optdb(oc_warn_style, Opt, _Data, _Help)
        ),
    solutions(FindOptionsPred, StyleWarnOptions).

info_request_options = InfoRequestOptions :-
    FindOptionsPred =
        ( pred(Opt::out) is nondet :-
            optdb(oc_inform, Opt, _Data, _Help)
        ),
    solutions(FindOptionsPred, InfoRequestOptions).

options_not_to_track = InconsequentialOptions :-
    % XXX This needs to be updated when the oc_X changes are all done.
    InconsequentialCategories = set.list_to_set([oc_warn_ctrl, oc_warn_dodgy,
        oc_warn_style, oc_inform, oc_verbosity, oc_internal, oc_buildsys]),
    FindOptionsPred =
        ( pred(Opt::out) is nondet :-
            optdb(Cat, Opt, _Data, _Help),
            set.member(Cat, InconsequentialCategories)
        ),
    solutions(FindOptionsPred, InconsequentialOptions).

%---------------------------------------------------------------------------%

option_table_add_mercury_library_directory(Dir, !OptionTable) :-
    % The init_file_directories and link_library_directories for Mercury
    % libraries are grade dependent, so they need to be handled in
    % handle_options.m after we know the grade.
    % XXX LEGACY
    list.foldl(append_to_accumulating_option, [
        search_directories          - dir.make_path_name(Dir, "ints"),
        c_include_directories       - dir.make_path_name(Dir, "inc"),
        mercury_library_directories - Dir
    ], !OptionTable).

option_table_add_search_library_files_directory(Dir, !OptionTable) :-
    % Grade dependent directories need to be handled in handle_options.m
    % after we know the grade.
    % XXX LEGACY
    list.foldl(append_to_accumulating_option, [
        search_directories          - Dir,
        c_include_directories       - Dir,
        search_library_files_directories - Dir
    ], !OptionTable).

:- pred append_to_accumulating_option(pair(option, string)::in,
    option_table::in, option_table::out) is det.

append_to_accumulating_option(Option - Value, !OptionTable) :-
    getopt.lookup_accumulating_option(!.OptionTable, Option, Values0),
    Values = Values0 ++ [Value],
    map.det_update(Option, accumulating(Values), !OptionTable).

:- pred override_options(list(pair(option, option_data))::in,
    option_table::in, option_table::out) is det.

override_options([], !OptionTable).
override_options([Option - Value | OptionsValues], !OptionTable) :-
    map.det_update(Option, Value, !OptionTable),
    override_options(OptionsValues, !OptionTable).

set_all_options_to([], _Value, !OptionTable).
set_all_options_to([Option | Options], Value, !OptionTable) :-
    map.set(Option, Value, !OptionTable),
    set_all_options_to(Options, Value, !OptionTable).

%---------------------------------------------------------------------------%

:- pred handle_quoted_flag(option::in, string::in,
    option_table::in, option_table::out) is det.

handle_quoted_flag(Option, Flag, !OptionTable) :-
    append_to_accumulating_option(Option - quote_shell_cmd_arg(Flag),
        !OptionTable).

%---------------------------------------------------------------------------%

:- type optdb_tuple
    --->    optdb_tuple(option, option_data, optdb_help.help).

:- pred acc_optdb_data(optdb_tuple::in,
    assoc_list(char, option)::in, assoc_list(char, option)::out,
    assoc_list(string, option)::in, assoc_list(string, option)::out,
    assoc_list(option, option_data)::in,
    assoc_list(option, option_data)::out) is det.

acc_optdb_data(OptdbTuple, !ShortPairs, !LongPairs, !DataPairs) :-
    OptdbTuple = optdb_tuple(Opt, Data, Help),
    !:DataPairs = [Opt - Data | !.DataPairs],
    (
        ( Help = no_help
        ; Help = unnamed_help(_)
        )
    ;
        Help = gen_help(Shorts, Long1, Longs, _, _),
        list.foldl(insert_short(Opt), Shorts, !ShortPairs),
        insert_long(Opt, Long1, !LongPairs),
        list.foldl(insert_long(Opt), Longs, !LongPairs)
    ;
        ( Help = short_alt_align_help(Short, Long1, Longs, _, _)
        ; Help = short_help(Short, Long1, Longs, _)
        ; Help = priv_short_help(Short, Long1, Longs, _)
        ; Help = short_arg_help(Short, Long1, Longs, _, _)
        ; Help = priv_short_arg_help(Short, Long1, Longs, _, _)
        ),
        insert_short(Opt, Short, !ShortPairs),
        insert_long(Opt, Long1, !LongPairs),
        list.foldl(insert_long(Opt), Longs, !LongPairs)
    ;
        ( Help = help(Long, _)
        ; Help = arg_help(Long, _, _)
        ; Help = priv_help(Long, _)
        ; Help = priv_arg_help(Long, _, _)
        ; Help = alt_arg_align_help(Long, _, _)
        ; Help = no_align_help(Long, _, _, _)
        ),
        insert_long(Opt, Long, !LongPairs)
    ;
        ( Help = alt_help(Long1, Longs, _)
        ; Help = priv_alt_help(Long1, Longs, _)
        ; Help = alt_align_help(Long1, Longs, _, _)
        ; Help = priv_alt_align_help(Long1, Longs, _, _)
        ; Help = alt_arg_help(Long1, Longs, _, _)
        ; Help = priv_alt_arg_help(Long1, Longs, _, _)
        ),
        insert_long(Opt, Long1, !LongPairs),
        list.foldl(insert_long(Opt), Longs, !LongPairs)
    ).

%---------------------%

:- pred insert_short(option::in, char::in,
    assoc_list(char, option)::in, assoc_list(char, option)::out) is det.

insert_short(Option, Name, !ShortPairs) :-
    !:ShortPairs = [Name - Option | !.ShortPairs].

:- pred insert_long(option::in, string::in,
    assoc_list(string, option)::in, assoc_list(string, option)::out) is det.

insert_long(Option, Name, !LongPairs) :-
    !:LongPairs = [Name - Option | !.LongPairs].

%---------------------------------------------------------------------------%

option_categories(oc_help, 0).
option_categories(oc_cmdline, 0).
option_categories(oc_warn_dodgy, 0).
option_categories(oc_warn_perf, 0).
option_categories(oc_warn_perf_c, 0).
option_categories(oc_warn_style, 0).
option_categories(oc_warn_style_c, 0).
option_categories(oc_warn_ctrl, 0).
option_categories(oc_warn_halt, 0).
option_categories(oc_inform, 0).
option_categories(oc_verbosity, 0).
option_categories(oc_verb_dev, 0).
option_categories(oc_verb_dbg, 0).
option_categories(oc_opmode, 0).
option_categories(oc_diag_gen, 0).
option_categories(oc_diag_color, 0).
option_categories(oc_diag_int, 0).
option_categories(oc_mdb, 0).
option_categories(oc_mdb_dev, 0).
option_categories(oc_tracegoal, 0).
option_categories(oc_mdprof, 0).
option_categories(oc_output_mod, 0).
option_categories(oc_output_dev, 0).
option_categories(oc_file_req, 0).
option_categories(oc_make, 0).
option_categories(oc_semantics, 0).
option_categories(oc_infer, 0).
option_categories(oc_grade, 0).
option_categories(oc_internal, 0).
option_categories(oc_opt_hh, 0).
option_categories(oc_opt_hh_exp, 0).
option_categories(oc_opt_hlm, 0).
option_categories(oc_opt_hm, 0).
option_categories(oc_opt_hl, 0).
option_categories(oc_opt_ll, 0).
option_categories(oc_opt_mm, 0).
option_categories(oc_opt_lc, 0).
option_categories(oc_opt_ctrl, 0).
option_categories(oc_trans_opt, 0).
option_categories(oc_target_comp, 0).
option_categories(oc_target_c, 0).
option_categories(oc_target_java, 0).
option_categories(oc_target_csharp, 0).
option_categories(oc_link_c, 0).
option_categories(oc_link_java, 0).
option_categories(oc_link_csharp, 0).
option_categories(oc_link_c_cs, 0).
option_categories(oc_link_c_cs_j, 0).
option_categories(oc_buildsys, 0).
option_categories(oc_search, 0).
option_categories(oc_env, 0).
option_categories(oc_dev_ctrl, 0).
option_categories(oc_dev_debug, 0).
option_categories(oc_dev_dump, 0).
option_categories(oc_config, 0).
option_categories(oc_mconfig, 0).
option_categories(oc_analysis, 0).
option_categories(oc_unused, 0).

%---------------------------------------------------------------------------%
:- end_module libs.options.
%---------------------------------------------------------------------------%
