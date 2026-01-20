%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: options.m.
% Main author: zs.
%
% This modules defines the set of options accepted by the Mercury compiler.
%
% NOTE: any changes to the options should be reflected in two places,
%
% = the option type, and
% - the optdb predicate.
%
% The order of the options must be the same in both places, and this is
% enforced by the require_switch_arms_in_type_order pragma on optdb.
%
% The clauses of the optdb predicate are formatted in blocks of clauses
% without blank lines between them. (The blocks themselves are separated
% by comments and blank lines.)
%
% Each block should contain all the options in one section of the help text,
% as shown by print_help.m. The order of the blocks should reflect exactly
% the order of those sections in the help text.
%
% The order of the options in each block should be chosen to make
% the help text as easy to read as possible. This means that
%
% - related options should be grouped together, and
% - if the meaning of option A is required to understand the meaning
%   of option B, then option A should be listed first.
%
% The list of long names (of the form "--option-name") as well as any
% short names (of the form "-x") of each option are listed in its optdb clause.
%
% For options whose long names include words that whose spelling differs
% in American vs British English, there should normally be one long name
% for each spelling.
%
% If the full option name is long enough that typing it is burdensome,
% then we often have one (or sometimes more) abbreviated versions of the name
% as well.
%
% If an option has more than one long name, then
%
% - the non-abbreviated names should come before the abbreviated ones, and
% - the versions using American spelling should come before the British ones.

% We have standardized on "directory" over "dir" as argument name.
% We have standardized on "filename" over "file" as argument name.
% We have standardized on "N" over "n" as argument name.
%
% The set of options enabled at each optimization level is controlled by
% the predicate named opts_enabled_at_level. This is defined in
% compiler/optimization_options.m, but it cannot be edited there,
% because that module's source code is generated automatically. Instead,
% you need to edit tools/make_optimization_options_end, and then execute
% tools/make_optimization_options to update compiler/optimization_options.m
% (and the part of this file between the INCLUDE_HANDLER_FILE_* markers).
%
% Likewise, the set of optimization options that should be set automatically
% to implement --opt-space is controlled from make_optimization_options_end.
%
%---------------------------------------------------------------------------%

:- module libs.options.
:- interface.

:- import_module libs.optdb_help.
:- import_module libs.optimization_options.
:- import_module libs.option_categories.

:- import_module cord.
:- import_module getopt.
:- import_module list.

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

%---------------------------------------------------------------------------%

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

    % Return the options that warn about code that may not be
    % what the programmer intended.
    %
:- func dodgy_code_warning_bool_options = list(option).

    % Return the options that warn about code that may be
    % unnecessarily slower than the programmer intended.
    %
:- func slow_code_warning_bool_options = list(option).

    % Return the options that warn about issues that are only stylistic,
    % i.e. do not affect the implementation of the programmer's intention.
    %
:- func style_warning_bool_options = list(option).

    % Return the options that request information from the compiler.
    %
:- func info_request_bool_options = list(option).

    % Return the set of options which are inconsequential as far as the
    % `--track-flags' option is concerned. That is, adding or removing such
    % an option to a module should not force the module to be recompiled.
    %
:- func options_not_to_track = list(option).

%---------------------------------------------------------------------------%

    % NOTE: ALL OPTIONS SHOULD BE DOCUMENTED!
    %
    % Officially supported options should always be documented.
    %
    % Options which are not officially supported, such as
    %
    % - those used internally by the Mercury implementation,
    % - those which are not sufficiently useful to be worth mentioning
    %   in the User Guide, or
    % - options for experimental features that are not yet stable
    %   enough to be officially supported
    %
    % should still be documented, but the documentation should be private.
    %
:- type option

% Help options.
    --->    help
    ;       help_priv
    ;       help_texinfo
    ;       version

% Options for modifying the command line.
    ;       flags_file
    ;       filenames_from_stdin

% Opmode options.
    ;       only_opmode_generate_source_file_mapping
    ;       only_opmode_generate_dependencies
    ;       only_opmode_generate_dependencies_ints
    ;       only_opmode_generate_dependency_file
    ;       only_opmode_make_short_interface
    ;       only_opmode_make_private_interface
    ;       only_opmode_make_interface
    ;       only_opmode_make_optimization_interface
    ;       only_opmode_make_transitive_opt_interface
    ;       only_opmode_typecheck_only
    ;       only_opmode_errorcheck_only
    ;       only_opmode_target_code_only
    ;       only_opmode_compile_only

    ;       only_opmode_generate_standalone_interface
    ;       only_opmode_convert_to_mercury
    ;       only_opmode_make_xml_documentation
    ;       only_opmode_make_analysis_registry

    ;       only_opmode_make
    ;       part_opmode_rebuild
    ;       only_opmode_invoked_by_mmc_make

    ;       only_opmode_output_grade_string
    ;       only_opmode_output_grade_defines
    ;       only_opmode_output_stdlib_grades
    ;       only_opmode_output_stdlib_modules
    ;       only_opmode_output_library_install_grades

    ;       only_opmode_output_target_arch
    ;       only_opmode_output_cc
    ;       only_opmode_output_c_compiler_type
    ;       only_opmode_output_cflags
    ;       only_opmode_output_c_include_directory_flags
    ;       only_opmode_output_link_command
    ;       only_opmode_output_shared_lib_link_command
    ;       only_opmode_output_library_link_flags
    ;       only_opmode_output_csharp_compiler
    ;       only_opmode_output_csharp_compiler_type
    ;       only_opmode_output_java_class_dir

    ;       only_opmode_output_optimization_options
    ;       only_opmode_output_optimization_options_upto

% Grade options
    % Grade options, part a: specifying a complete grade
    ;       grade

    % Grade options, part b: target selection
    ;       target
    ;       compile_to_c        % target c + target_code_only
    ;       java                % target java
    ;       java_only           % target java + target_code_only
    ;       csharp              % target csharp
    ;       csharp_only         % target csharp + target_code_only

    % Grade options, part c: LLDS compilation
    ;       gcc_global_registers
    ;       gcc_non_local_gotos
    ;       asm_labels
    ;       use_float_registers

    % Grade options, part d: MLDS compilation
    ;       highlevel_code
    ;       target_debug_grade

    % Grade options, part e: debugging

    % Grade options, part e1: mdb debugging
    % For documentation of the exec_trace and decl_debug options, see the
    % documentation for MR_EXEC_TRACE and MR_DECL_DEBUG in
    % runtime/mercury_conf_param.h.
    ;       exec_trace
    ;       decl_debug

    % Grade options, part e2: ssdb debugging
    ;       source_to_source_debug

    % Grade options, part f: profiling

    % Grade options, part f1: mprof profiling
    ;       profiling           % profile_time + profile_calls
    ;       time_profiling      % profile_time + profile_calls
    ;       memory_profiling    % profile_mem + profile_calls
    ;       profile_calls
    ;       profile_time
    ;       profile_memory

    % Grade options, part f2: mdprof profiling
    ;       deep_profiling
    ;       profile_deep
    ;       use_activation_counts
    ;       use_zeroing_for_ho_cycles
    ;       use_lots_of_ho_specialization
    ;       deep_profile_tail_recursion

    ;       coverage_profiling
    ;       coverage_profiling_via_calls
    ;       coverage_profiling_static
    ;       profile_deep_coverage_after_goal
    ;       profile_deep_coverage_branch_ite
    ;       profile_deep_coverage_branch_switch
    ;       profile_deep_coverage_branch_disj
    ;       profile_deep_coverage_use_portcounts
    ;       profile_deep_coverage_use_trivial
    ;       profile_for_feedback

    % Grade options, part f3: complexity profiling
    ;       experimental_complexity
    ;       record_term_sizes_as_words
    ;       record_term_sizes_as_cells

    % Grade options, part f4: threadscope profiling
    ;       threadscope

    % Grade options, part g: miscellaneous
    ;       pregenerated_dist
    ;       gc
    ;       stack_segments
    ;       extend_stacks_when_needed
    ;       use_trail
    ;       single_prec_float
    ;       parallel
    ;       maybe_thread_safe_opt
    ;       use_minimal_model_stack_copy
    ;       use_minimal_model_own_stacks
    ;       minimal_model_debug
    ;       use_regions
    ;       use_alloc_regions
    ;       use_regions_debug
    ;       use_regions_profiling

    % Grade options, part h: data representation/developer
    ;       num_ptag_bits
    ;       bits_per_word
    ;       bytes_per_word
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

% Inference control options
    ;       infer_all
    ;       infer_types
    ;       infer_modes
    ;       infer_det
    ;       type_inference_iteration_limit
    ;       mode_inference_iteration_limit
    ;       allow_stubs

% Language semantics options
    ;       strict_sequential
    ;       reorder_conj
    ;       reorder_disj
    ;       fully_strict

% Verbosity options
    ;       verbose
    ;       very_verbose
    ;       statistics
    ;       verbose_make
    ;       output_compile_error_lines
    ;       verbose_recompilation
    ;       find_all_recompilation_reasons
    ;       verbose_commands

% Diagnostics.
    % Diagnostics control.
    ;       verbose_errors
    ;       reverse_error_order
    ;       max_error_line_width
    ;       limit_error_contexts
    ;       error_files_in_subdir
    ;       std_int_file_not_written_msgs
    ;       typecheck_ambiguity_warn_limit
    ;       typecheck_ambiguity_error_limit

    % Diagnostics color.
    ;       color_diagnostics
    ;       config_default_color_diagnostics
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

% Warning options.
    % Warnings about dodgy code, at the module level.
    ;       warn_nothing_exported
    ;       warn_unused_imports
    ;       warn_unused_interface_imports
    ;       warn_interface_imports_in_parents
    ;       warn_stdlib_shadowing
    ;       warn_duplicate_abstract_instances
    ;       warn_too_private_instances
    ;       warn_subtype_ctor_order
    ;       warn_trans_opt_deps_spec

    % Warnings about dodgy code, involving determinism.
    ;       warn_det_decls_too_lax
    ;       warn_inferred_erroneous

    % Warnings about dodgy code, specifically predicates.
    ;       warn_unresolved_polymorphism
    ;       warn_stubs
    ;       warn_cannot_table
    ;       warn_non_term_special_preds
    ;       warn_non_stratification
    ;       warn_unneeded_purity_pred_decl
    ;       warn_typecheck_ambiguity_limit

    % Warnings about dodgy code, specifically pragmas.
    ;       warn_ambiguous_pragma
    ;       warn_potentially_ambiguous_pragma
    ;       warn_table_with_inline
    ;       warn_unneeded_purity_pragma
    ;       warn_nonexported_pragma

    % Warnings about dodgy code, at the goal level.
    ;       warn_dodgy_simple_code
    ;       warn_singleton_vars
    ;       warn_repeated_singleton_vars
    ;       warn_unification_cannot_succeed
    ;       warn_known_bad_format_calls
    ;       warn_obsolete
    ;       warn_overlapping_scopes
    ;       warn_suspected_occurs_check_failure
    ;       warn_suspicious_recursion
    ;       warn_unused_args
    ;       warn_unneeded_purity_indicator
    ;       warn_missing_state_var_init
    ;       warn_moved_trace_goal
    ;       warn_disj_fills_partial_inst
    ;       warn_unknown_warning_name

    % Warnings about dodgy code, involving insts.
    ;       warn_insts_without_matching_type
    ;       warn_insts_with_functors_without_type
    ;       warn_exported_inst_for_private_type

    % Warnings about dodgy code, involving files.
    ;       warn_undefined_options_variables
    ;       warn_missing_descendant_modules
    ;       warn_missing_opt_files
    ;       warn_missing_trans_opt_files
    ;       warn_missing_trans_opt_deps

    % Warnings about potential performance issues.
    ;       warn_accumulator_swaps
    ;       warn_unneeded_final_statevars
    ;       warn_unneeded_final_statevars_lambda
    ;       warn_no_auto_parallel
    ;       warn_obvious_non_tail_recursion
    ;       warn_non_tail_recursion_self
    ;       warn_non_tail_recursion_mutual
    ;       warn_non_tail_recursion
    ;       warn_no_recursion

    % Warnings about programming style, at the module level.
    ;       warn_include_and_non_include

    % Warnings about programming style, at the predicate level.
    ;       warn_dead_preds
    ;       warn_dead_procs
    ;       warn_can_fail_function
    ;       warn_unneeded_mode_specific_clause

    % Warnings about programming style, simple mistakes.
    ;       warn_redundant_code
    ;       warn_ite_instead_of_switch
    ;       warn_incomplete_switch
    ;       warn_incomplete_switch_threshold
    ;       warn_duplicate_calls
    ;       warn_redundant_coerce
    ;       warn_requested_by_code
    ;       warn_requested_by_option

    % Warnings about programming style, involving state vars.
    ;       warn_state_var_shadowing
    ;       warn_unneeded_initial_statevars
    ;       warn_unneeded_initial_statevars_lambda
    % XXX We should also warn about unused statevars in "some (...)" scopes.

    % Warnings about programming style, involving I/O.
    ;       warn_implicit_stream_calls
    ;       warn_unknown_format_calls

    % Warnings about programming style, foreign code.
    ;       warn_suspicious_foreign_code
    ;       warn_suspicious_foreign_procs

    % Warnings about programming style, missing order.
    ;       warn_unsorted_import_blocks
    ;       warn_inconsistent_pred_order_clauses
    ;       warn_inconsistent_pred_order_foreign_procs

    % Warnings about programming style, missing contiguity.
    ;       warn_non_contiguous_decls
    ;       warn_non_contiguous_clauses
    ;       warn_non_contiguous_foreign_procs
    ;       allow_non_contiguity_for

    % Options that control warnings.
    ;       inhibit_warnings
    ;       inhibit_style_warnings
    ;       warn_all_format_string_errors
    ;       warn_smart_recompilation
    ;       warn_up_to_date

    % Warnings treated as errors.
    ;       halt_at_warn
    ;       halt_at_warn_make_int
    ;       halt_at_warn_make_opt
            % Almost all code in the compiler should only look at the value
            % of halt_at_warn. handle_options.m will overwrite its value
            % with the value of halt_at_warn_make_int when making interface
            % files and with the value of halt_at_warn_make_opt when making
            % optimization files.
    ;       halt_at_syntax_errors
    ;       halt_at_invalid_interface
    ;       halt_at_auto_parallel_failure

% Options that request information.
    ;       inform_inferred
    ;       inform_inferred_types
    ;       inform_inferred_modes
    ;       inform_incomplete_color_scheme
    ;       inform_suboptimal_packing
    ;       show_pred_movability

% Options that request information in files.
    ;       show_definitions
    ;       show_definition_line_counts
    ;       show_definition_extents
    ;       show_local_call_tree
    ;       show_local_type_repns
    ;       show_all_type_repns
    ;       show_developer_type_repns
    ;       show_dependency_graph
    ;       show_imports_graph

% Options that control whether some compiler reports are printed.
    ;       report_noop
    ;       report_not_written
    ;       report_recompile_reason
    ;       report_used_file_error

% Options that control trace goals.
    ;       trace_goal_flags

% Options that control mdb debugging.
    ;       trace_level
    ;       exec_trace_tail_rec
    ;       trace_optimized
    ;       trace_prof
    ;       event_set_file_name
    ;       trace_table_io
    ;       trace_table_io_only_retry
    ;       trace_table_io_states
    ;       trace_table_io_require
    ;       trace_table_io_all
    ;       suppress_trace
    ;       delay_death
    ;       delay_death_max_vars
    ;       stack_trace_higher_order
    ;       force_disable_tracing

% Options that control ssdb debugging.
    ;       ssdb_trace_level
    ;       force_disable_ssdebug

% Options that control profiling.
    ;       prof_optimized

% Options that control optimization.

    % Options that control the optimization process.
    ;       default_opt_level
    ;       opt_level
    ;       opt_space                   % Default is to optimize time.

    % HLDS->HLDS optimizations.
    ;       optopt_opt_dead_procs
    ;       optopt_opt_unneeded_code
    ;       optopt_opt_unneeded_code_copy_limit

    ;       optopt_opt_unused_args
    ;       optopt_opt_unused_args_intermod

    ;       optopt_opt_format_calls
    ;       optopt_prop_constants
    ;       optopt_opt_dup_calls
    ;       optopt_elim_excess_assigns

    ;       optopt_allow_inlining
    ;       inlining
    ;       optopt_inline_builtins
    ;       optopt_inline_par_builtins
    ;       optopt_inline_single_use
    ;       optopt_inline_simple
    ;       optopt_inline_simple_threshold
    ;       optopt_intermod_inline_simple_threshold
    ;       optopt_inline_compound_threshold
    ;       optopt_inline_call_cost
    ;       optopt_inline_vars_threshold

    ;       optopt_opt_higher_order
    ;       optopt_spec_types
    ;       optopt_spec_types_user_guided
    ;       optopt_higher_order_size_limit
    ;       optopt_higher_order_arg_limit

    ;       optopt_opt_loop_invariants
    ;       optopt_introduce_accumulators
    ;       optopt_opt_lcmc
    ;       optopt_opt_lcmc_accumulator
    ;       optopt_opt_lcmc_null

    ;       optopt_split_switch_arms
    ;       optopt_merge_code_after_switch

    ;       optopt_from_ground_term_threshold
    ;       optopt_enable_const_struct_user
    ;       optopt_opt_common_structs

    ;       optimize_saved_vars
    ;       optopt_opt_saved_vars_const
    ;       optopt_opt_svcell
    ;       optopt_opt_svcell_loop
    ;       optopt_opt_svcell_full_path
    ;       optopt_opt_svcell_on_stack
    ;       optopt_opt_svcell_candidate_headvars
    ;       optopt_opt_svcell_cv_store_cost
    ;       optopt_opt_svcell_cv_load_cost
    ;       optopt_opt_svcell_fv_store_cost
    ;       optopt_opt_svcell_fv_load_cost
    ;       optopt_opt_svcell_op_ratio
    ;       optopt_opt_svcell_node_ratio
    ;       optopt_opt_svcell_all_path_node_ratio
    ;       optopt_opt_svcell_all_candidates

    ;       optopt_prop_constraints
    ;       optopt_prop_local_constraints

    ;       optopt_deforest
    ;       optopt_deforestation_depth_limit
    ;       optopt_deforestation_cost_factor
    ;       optopt_deforestation_vars_threshold
    ;       optopt_deforestation_size_threshold

    ;       optopt_untuple
    ;       optopt_tuple
    ;       optopt_tuple_trace_counts_file
    ;       optopt_tuple_costs_ratio
    ;       optopt_tuple_min_args

    ;       optopt_delay_constructs

    ;       optopt_opt_follow_code
    ;       optopt_spec_in_all_dep_par_conjs
    ;       optopt_allow_some_paths_only_waits
    ;       optopt_analyse_regions

    % Stuff for the CTGC system (structure sharing / structure reuse).
    ;       structure_sharing_analysis
    ;           structure_sharing_widening
    ;       structure_reuse_analysis
    ;           structure_reuse_constraint
    ;           structure_reuse_constraint_arg
    ;           structure_reuse_max_conditions
    ;           structure_reuse_repeat
    ;           structure_reuse_free_cells

    % HLDS->{LLDS,MLDS} optimizations.
    ;       optopt_use_smart_indexing
    ;           optopt_use_smart_indexing_atomic
    ;           optopt_use_smart_indexing_string
    ;           optopt_use_smart_indexing_tag
    ;           optopt_use_smart_indexing_float

    ;           optopt_dense_switch_req_density
    ;           optopt_lookup_switch_req_density
    ;           optopt_dense_switch_size
    ;           optopt_lookup_switch_size
    ;           optopt_string_trie_switch_size
    ;           optopt_string_hash_switch_size
    ;           optopt_string_binary_switch_size
    ;           optopt_tag_switch_size
    ;           optopt_put_base_first_single_rec
    ;           optopt_put_base_first_multi_rec

    ;       optopt_use_static_ground_cells
    ;       optopt_use_atomic_cells
    ;       optimize_trail_usage

    % HLDS->MLDS optimizations.
    ;       optopt_optimize_mlds
    ;       optopt_peep_mlds
    ;       optopt_opt_mlds_tailcalls
    ;       optopt_opt_mlds_inits
    ;       optopt_elim_unused_mlds_assigns
    ;       optopt_elim_local_vars_mlds
    ;       optopt_gen_trail_ops_inline_mlds

    % LLDS->LLDS optimizations.
    ;       optopt_try_switch_size
    ;       optopt_binary_switch_size
    ;       optopt_opt_middle_rec_llds
    ;       optopt_opt_simple_neg_llds
    ;       optopt_allow_hijacks

    ;       optopt_optimize_llds
    ;       optopt_repeat_opts
    ;       optopt_peep_llds
    ;       optopt_peep_llds_mkword
    ;       optopt_opt_labels
    ;       optopt_standardize_labels
    ;       optopt_opt_jumps
    ;       optopt_opt_fulljumps
    ;       optopt_opt_checked_nondet_tailcalls
    ;       optopt_pessimize_llds_tailcalls
    ;       optopt_opt_delay_slot
    ;       optopt_opt_frames
    ;       optopt_opt_llds_reassign
    ;       optopt_use_local_vars_llds
    ;       optopt_llds_local_var_access_threshold
    ;       optopt_opt_dup_instrs_llds
    ;       optopt_opt_dup_procs_llds
    ;       optopt_use_llds_common_data
    ;       optopt_use_llds_common_layout_data
    ;       optopt_llds_layout_compression_limit
    ;       optimize_region_ops

    % LLDS->C optimizations.
    ;       optopt_emit_c_loops
    ;       optopt_procs_per_c_function
    ;       optopt_use_local_thread_engine_base
    ;       optopt_inline_alloc
    ;       optopt_use_macro_for_redo_fail

% Intermodule optimization options.
    ;       intermodule_optimization
    ;       use_opt_files
    ;       read_opt_files_transitively

    ;       transitive_optimization
    ;       use_trans_opt_files
    ;       also_output_module_order
    ;       trans_opt_deps_spec

    ;       intermodule_analysis
    ;       analysis_repeat
    ;       analysis_file_cache
    ;       analysis_file_cache_dir

% Analysis options.
    % Options for the old termination analyser.
    ;       termination_enable
    ;       termination_check
    ;       termination_check_verbose
    ;       termination_single_args
    ;       termination_norm
    ;       termination_error_limit
    ;       termination_path_limit

    % Options for the new termination analyser.
    ;       termination2_enable
    ;       termination2_check
    ;       termination2_check_verbose
    ;       termination2_norm
    ;       termination2_widening_limit
    ;       termination2_arg_size_only
    ;       termination2_prop_fail_constrs
    ;       termination2_maximum_matrix_size

    % Other analyses.
    ;       analyse_exceptions
    ;       analyse_closures
    ;       analyse_trail_usage
    ;       analyse_mm_tabling

% Auxiliary output options.
    ;       line_numbers
    ;       line_numbers_around_foreign_code
    ;       line_numbers_for_c_headers
    ;       type_repns_for_humans
    ;       auto_comments
    ;       frameopt_comments

% Options that control mmc --make.
    ;       keep_going
    ;       order_make_by_timestamp
    ;       show_make_times
    ;       make_max_jobs
    ;       make_track_flags
    ;       make_pre_link_command

% Target code compilation options.
    % Target code compilation options
    ;       target_debug
    ;       warn_target_code

    % C
    ;       cc
    ;       c_compiler_type
    ;       optopt_c_optimize
    ;       c_include_directories
    ;       cflags
    ;       quoted_cflag

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
    ;       cflags_for_debug
    ;       cflags_for_regs
    ;       cflags_for_gotos
    ;       cflags_for_threads
    ;       cflags_for_pic
    ;       cflags_for_sanitizers
    ;       cflags_for_lto
    ;       c_flag_to_name_object_file
    ;       object_file_extension
    ;       pic_object_file_extension

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
    ;       cli_interpreter
    ;       csharp_compiler_type
    ;       csharp_flags
    ;       quoted_csharp_flag
    ;       mono_path_directories

% Link options.
    % General link options.
    ;       mercury_library_directory_special
    ;       mercury_library_directories
    ;       search_library_files_directory_special
    ;       search_library_files_directories
    ;       mercury_library_special
    ;       mercury_libraries
    ;       mercury_standard_library_directory_special
    ;       mercury_standard_library_directory

    % Link options for C and C#.
    ;       link_library_directories
    ;       link_libraries
    ;       link_ssdb_libs

    % Link options for C.
    ;       output_file_name
    ;       link_objects
    ;       ld_flags
    ;       quoted_ld_flag
    ;       ld_libflags
    ;       quoted_ld_libflag
    ;       runtime_link_library_directories
    ;       use_default_runtime_library_directory
    ;       init_file_directories
    ;       init_files
    ;       trace_init_files
    ;       linkage_special
    ;       only_globals_linkage
    ;       mercury_linkage_special
    ;       only_globals_mercury_linkage

    ;       demangle
    ;       strip
    ;       main

    ;       allow_undefined
    ;       use_readline
    ;       runtime_flags
    ;       extra_init_functions
    ;       frameworks
    ;       framework_directories
    ;       cstack_reserve_size
    ;       link_executable_command
    ;       link_shared_lib_command
    ;       shlib_linker_install_name_path
    ;       strip_executable_command
    ;       strip_executable_shared_flags
    ;       strip_executable_static_flags
    ;       shared_lib_not_executable

    % Link options for Java.
    ;       java_archive_command

    % Link options for C#.
    ;       sign_assembly

% File search options.
    ;       options_search_directories
    ;       setting_only_use_subdirs
    ;       setting_only_use_grade_subdirs
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

% Build system options.
    ;       install_prefix
    ;       library_install_grades
    ;       library_install_grades_incl_components
    ;       library_install_grades_excl_components
    ;       only_globals_library_install_linkages
    ;       detect_stdlib_grades
    ;       libgrade_install_check
    ;       extra_init_command
    ;       extra_library_header

% Options that specify the environment.
    ;       mercury_configuration_directory_special
    ;       mercury_configuration_directory
    ;       install_command
    ;       options_files
    ;       config_file
    ;       env_type
    ;       host_env_type
    ;       system_env_type
    ;       target_env_type
    ;       restricted_command_line

% Configuration options.
    ;       conf_low_ptag_bits
    ;       have_delay_slot
    ;       num_real_r_regs
    ;       num_real_f_regs
    ;       num_real_r_temps
    ;       num_real_f_temps
    ;       max_jump_table_size

% Mercury.config options.
    ;       mkinit_command
    ;       target_arch

    ;       executable_file_extension
    ;       library_extension
    ;       shared_library_extension

    ;       create_archive_command
    ;       create_archive_command_flags
    ;       create_archive_command_output_flag
    ;       ranlib_command
    ;       ranlib_flags

    ;       demangle_command
    ;       filtercc_command
    ;       filterjavac_command

    ;       linker_allow_undefined_flag
    ;       linker_debug_flags
    ;       linker_error_undefined_flag
    ;       linker_link_lib_flag
    ;       linker_link_lib_suffix
    ;       linker_lto_flags
    ;       linker_opt_separator
    ;       linker_path_flag
    ;       linker_rpath_flag
    ;       linker_rpath_separator
    ;       linker_sanitizer_flags
    ;       linker_static_flags
    ;       linker_strip_flag
    ;       linker_thread_flags
    ;       linker_trace_flags
    ;       shlib_linker_debug_flags
    ;       shlib_linker_install_name_flag
    ;       shlib_linker_link_lib_flag
    ;       shlib_linker_link_lib_suffix
    ;       shlib_linker_rpath_flag
    ;       shlib_linker_rpath_separator
    ;       shlib_linker_thread_flags
    ;       shlib_linker_trace_flags
    ;       shlib_linker_use_install_name

    ;       hwloc_libs
    ;       hwloc_static_libs
    ;       math_lib
    ;       readline_libs
    ;       shared_libs
    ;       thread_libs
    ;       trace_libs

    ;       install_method
    ;       use_symlinks

% Developer options.
    % Developer control options.
    ;       progress_output_suffix
    ;       error_output_suffix
    ;       inference_output_suffix
    ;       debug_output_suffix
    ;       recompile_output_suffix

    ;       mode_constraints
    ;       simple_mode_constraints
    ;       prop_mode_constraints
    ;       compute_goal_modes
    ;       smart_recompilation
    ;       pre_prof_transforms_simplify
    ;       disable_mmsc_pneg
    ;       disable_mmsc_cut
    ;       disable_trail_ops
    ;       type_check_using_constraints
    ;       trad_passes
    ;       parallel_liveness
    ;       parallel_code_gen
    ;       should_pretest_equality
    ;       fact_table_max_array_size
    ;       fact_table_hash_percent_full
    ;       prefer_switch
    ;       prefer_while_loop_over_jump_self
    ;       prefer_while_loop_over_jump_mutual
    ;       opt_no_return_calls                     % XXX should be oc_opt
    ;       compiler_sufficiently_recent
    ;       experiment
    ;       experiment1
    ;       experiment2
    ;       experiment3
    ;       experiment4
    ;       experiment5
    ;       allow_ho_insts_as_modes
    ;       ignore_par_conjunctions
    ;       control_granularity
    ;       distance_granularity
    ;       implicit_parallelism
    ;       feedback_file
    ;       par_loop_control
    ;       par_loop_control_keep_tail_rec
    ;       optopt_enable_const_struct_poly
    ;       canonicalize_error_path_names

    % Developer verbosity options.

    ;       detailed_statistics
    ;       benchmark_modes
    ;       benchmark_modes_repeat
    ;       report_cmd_line_args
    ;       report_cmd_line_args_in_doterr
    ;       inform_ignored_pragma_errors
    ;       inform_generated_type_spec_pragmas
    ;       proc_size_statistics
    ;       inst_statistics
    ;       print_error_spec_id

    ;       debug_types
    ;       debug_types_pred_name
    ;       debug_type_rep
    ;       debug_modes
    ;       debug_modes_verbose
    ;       debug_modes_minimal
    ;       debug_modes_statistics
    ;       debug_modes_delay_vars
    ;       debug_modes_goal_ids
    ;       debug_modes_pred_id
    ;       debug_mode_constraints
    ;       debug_det
    ;       debug_common_struct_preds
    ;       debug_closure
    ;       debug_term          % term = constraint termination analysis
    ;       debug_dead_proc_elim
    ;       debug_higher_order_specialization
    ;       debug_pd            % pd = partial deduction/deforestation
    ;       debug_indirect_reuse
    ;       debug_trail_usage
    ;       debug_unneeded_code
    ;       debug_unneeded_code_pred_name
    ;       debug_mm_tabling_analysis
    ;       debug_dep_par_conj
    ;       debug_liveness
    ;       debug_stack_opt
    ;       debug_code_gen_pred_id
    ;       debug_opt
    ;       debug_opt_pred_id
    ;       debug_opt_pred_name
    ;       debug_make
    ;       debug_intermodule_analysis

    % Developer debug options.

    ;       table_debug
    ;       debug_class_init

    % Developer dump options.

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

    ;       dump_trace_counts
    ;       dump_options_file

    % Internal-use-only options.
    ;       pre_implicit_parallelism_simplify
    ;       type_layout
    ;       det_copy_out
    ;       nondet_copy_out
    ;       put_commit_in_own_func
    ;       backend_foreign_languages
    ;       stack_trace
    ;       basic_stack_layout
    ;       agc_stack_layout
    ;       procid_stack_layout
    ;       trace_stack_layout
    ;       body_typeinfo_liveness
    ;       can_compare_constants_as_ints
    ;       pretest_equality_cast_pointers
    ;       delay_partial_instantiations
    ;       allow_defn_of_builtins
    ;       type_ctor_info
    ;       type_ctor_layout
    ;       type_ctor_functors
    ;       rtti_line_numbers
    ;       new_type_class_rtti
    ;       use_mmsc_pneg
    ;       use_mmsc_cut
    ;       size_region_ite_fixed
    ;       size_region_disj_fixed
%   ;       size_region_semi_disj_fixed     % XXX unused, which may be a bug
    ;       size_region_commit_fixed
    ;       size_region_ite_protect
    ;       size_region_ite_snapshot
    ;       size_region_semi_disj_protect
    ;       size_region_disj_snapshot
    ;       size_region_commit_entry
    ;       reclaim_heap_on_failure
    ;       reclaim_heap_on_semidet_failure
    ;       reclaim_heap_on_nondet_failure
    ;       max_specialized_do_call_closure
    ;       max_specialized_do_call_class_method
    ;       compare_specialization
    ;       chosen_stdlib_dir
    ;       default_globals
    ;       local_module_id
    ;       generate_item_version_numbers
    ;       generate_mmc_make_module_dependencies
    ;       optopt_use_static_ground_floats
    ;       optopt_use_static_ground_int64s
    ;       optopt_use_static_code_addresses

% Unused/deprecated options.
    ;       ansi_c
    ;       cflags_for_ansi
    ;       install_command_dir_option
    ;       optopt_use_just_one_c_func.

:- type option_table == option_table(option).
:- type maybe_option_table == maybe_option_table(option).

%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compute_grade.
:- import_module libs.shell_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

:- pragma require_switch_arms_in_type_order(pred(optdb/4)).

    % Help options.

optdb(oc_help,      help,                              bool(no),
    gen_help(['?', 'h'], "help", [], help_public, [
        cindex("Help option"),
        w("Print a usage message.")])).
optdb(oc_help,      help_priv,                         bool(no),
    priv_help("help-priv", [
        w("Print a usage message, including private options.")])).
optdb(oc_help,      help_texinfo,                      bool(no),
    priv_help("help-texinfo", [
        w("Print the documentation of all options for the user guide.")])).
optdb(oc_help,      version,                           bool(no),
    help("version", [w("Print the compiler version.")])).

%---------------------------------------------------------------------------%

    % Options for modifying the command line.

optdb(oc_cmdline,   flags_file,                        file_special,
    alt_arg_help("flags-file", ["flags"], "filename", [
        w("Take options from the specified file, and handle them"),
        w("as if they were specified on the command line.")])).
optdb(oc_cmdline,   filenames_from_stdin,              bool(no),
    help("filenames-from-stdin", [
        w("Read in from standard input a newline-terminated"),
        w("module name or file name, compile that module or file,"),
        w("and repeat until reaching end-of-file."),
        w("(This allows a program or user to interactively compile"),
        w("several modules without the overhead of creating a process"),
        w("for each one.)")])).

%---------------------------------------------------------------------------%

    % Opmode options. These are all mutually exclusive.

optdb(oc_opmode,    only_opmode_generate_source_file_mapping, bool(no),
    short_help('f', "generate-source-file-mapping", [], [
        w("Output to"), file("Mercury.modules"),
        w("the module-name-to-file-name mapping for the"),
        w("list of source files given as non-option arguments to mmc."),
        w("This mapping is needed if either"),
        w("for some modules, the file name does not match the module name,"),
        w("or if some of the modules are outside the current directory."),
        w("In such cases, the mapping must be generated"),
        w("before invoking any one of"),
        code("mmc --generate-dependencies", ","),
        code("mmc --make", ","), w("or"), code("mmake depend", ".")])).
optdb(oc_opmode,    only_opmode_generate_dependencies, bool(no),
    short_help('M', "generate-dependencies", [], [
        cindex("Dependencies"),
        w("Output"), quote("Make", "-style"), w("dependencies"),
        w("for the given main module,"),
        w("and all the other modules in the program"),
        w("(i.e. all the other modules in this directory"),
        w("that the main module imports either directly or indirectly)"),
        w("to"), file_var("module", "dep", ","),
        w("to"), file_var("module", "dv", ","),
        w("and to the"), file(".d"), w("files"),
        w("of all the modules in the program.")])).
optdb(oc_opmode,    only_opmode_generate_dependencies_ints, bool(no),
    help("generate-dependencies-ints", [
        w("Do the same job as --generate-dependencies, but also output"),
        file(".int3", ","), file(".int0", ","), file(".int"),
        w("and"), file(".int2"),
        w("files for all the modules in the program.")])).
optdb(oc_opmode,    only_opmode_generate_dependency_file, bool(no),
    help("generate-dependency-file", [
        w("Output"), quote("Make", "-style"), w("dependencies"),
        w("for the given module to"), file_var("module", "d", ".")])).
% XXX Describe the purposes of the different .intN files.
optdb(oc_opmode,    only_opmode_make_short_interface,  bool(no),
    alt_help("make-short-interface", ["make-short-int"], [
        w("Write to"), file_var("module", "int3"),
        w("a list of the types, insts, modes, typeclasses and instances"),
        w("defined in the interface section of the named module."),
        w("The compiler uses these files to create"),
        file(".int0", ","), file(".int"), w("and"), file(".int2"),
        w("files.")])).
optdb(oc_opmode,    only_opmode_make_private_interface, bool(no),
    alt_help("make-private-interface", ["make-priv-int"], [
        w("Write to"), file_var("module", "int0"),
        w("the list of the entities"),
        w("(including types, insts, modes, predicates and functions)"),
        w("defined in the given module that its submodules have access to."),
        w("(This will include even entities that are"),
        emph("not"), w("exported from the module.)"),
        w("Besides the code of the module itself,"),
        w("the inputs to this task are the"), file(".int0"),
        w("files of the given module's own ancestor modules,"),
        w("and the"), file(".int3"), w("files of the modules"),
        w("it imports directly or indirectly."),
        w("Note that this command is unnecessary"),
        w("for modules that have no submodules.")])).
optdb(oc_opmode,    only_opmode_make_interface,        bool(no),
    short_help('i', "make-interface", ["make-int"], [
        w("Write to"), file_var("module", "int"),
        w("and to"), file_var("module", "int2"),
        w("a list of entities"),
        w("(including types, insts, modes, predicates and functions)"),
        w("that the given module exports for use by other modules."),
        w("When generating code, the compiler reads the"),
        file(".int"), w("file of every directly imported module,"),
        w("and the"), file(".int2"), w("file"),
        w("of every indirectly imported module."),
        w("(Each"), file(".int2"), w("file"),
        w("is a slightly shorter version"),
        w("of the corresponding"), file(".int"), w("file,"),
        w("because it is specialized for its intended use.)")])).
optdb(oc_opmode,    only_opmode_make_optimization_interface, bool(no),
    alt_help("make-optimization-interface",
            ["make-optimisation-interface", "make-opt-int"], [
        w("Write to"), file_var("module", "opt"),
        w("information about the semantically-private parts of"),
        w("the named module that can be useful when optimizing"),
        w("another module that imports this one."),
        w("Note that"), file(".opt"), w("files are used by"),
        opt("--intermodule-optimization", ".")])).
optdb(oc_opmode,    only_opmode_make_transitive_opt_interface, bool(no),
    alt_help("make-transitive-optimization-interface",
            ["make-transitive-optimisation-interface", "make-trans-opt"], [
        w("Write to"), file_var("module", "trans_opt"),
        w("information about the named module"),
        w("that can be useful when optimizing"),
        w("another module that imports this one."),
        w("The distinction from"), file(".opt"), w("files is that a"),
        file(".trans_opt"), w("file can include information"),
        w("not just from the source code of its module,"),
        w("but also from the"), file(".opt"), w("and"), file(".trans_opt"),
        w("files of other modules."),
        w("Note that"), file(".trans_opt"), w("files are used by"),
        opt("--transitive-intermodule-optimization", ".")])).
optdb(oc_opmode,    only_opmode_typecheck_only,        bool(no),
    short_help('t', "typecheck-only", [], [
        w("Check the module's code only for syntax- and type errors."),
        w("Do not execute any other semantic checks,"),
        w("and do not generate any code."),
        w("(When converting Prolog code to Mercury,"),
        w(" it can sometimes be useful to get the types right first"),
        w("and worry about modes second;"),
        w("this option supports that approach.)")])).
optdb(oc_opmode,    only_opmode_errorcheck_only,       bool(no),
    short_help('e', "errorcheck-only", [], [
        w("Check the module's code for syntax- and semantic errors,"),
        w("but do not generate any code.")])).
optdb(oc_opmode,    only_opmode_target_code_only,      bool(no),
    short_help('C', "target-code-only", [], [
        w("Generate target code"),
        w("(meaning C code in"), file_var("module", "c", ","),
        w("C# code in"), file_var("module", "cs", ","),
        w("or Java code in"), file_var("module", "java", "),"),
        w("but do not generate object code.")])).
optdb(oc_opmode,    only_opmode_compile_only,          bool(no),
    short_help('c', "compile-only", [], [
        w("Generate C code in"), file_var("module", "c"),
        w("and object code in"), file_var("module", "o", ","),
        w("but do not attempt to link the object files.")])).
optdb(oc_opmode,    only_opmode_generate_standalone_interface,
                                                       maybe_string(no),
    arg_help("generate-standalone-interface", "basename", [
        w("Output a stand-alone interface."),
        % XXX We should describe what this means exactly.
        w("Use"), arg("basename"), w("as the basename"),
        w("of any files generated for the stand-alone-interface."),
        w("(See"), ref("the", "Stand-alone interfaces",
            "chapter of the Mercury User's Guide"),
        w("for further details.)")])).
optdb(oc_opmode,    only_opmode_convert_to_mercury,    bool(no),
    short_help('P', "convert-to-mercury",
            ["convert-to-Mercury", "pretty-print"], [
        w("Output the code of the module to"), file_var("module", "ugly"),
        w("in a standard format."),
        w("This option acts as a Mercury ugly-printer."),
        w("(It would be a pretty-printer, except that comments are stripped,"),
        w("and nested if-then-elses are indented too much,"),
        w("so the result is rather ugly.)")])).
optdb(oc_opmode,    only_opmode_make_xml_documentation, bool(no),
    short_help('x', "make-xml-documentation", ["make-xml-doc"], [
        w("Output an XML representation of all the declarations"),
        w("in the module to"), file_var("module", "xml", "."),
        w("This XML file can then be transformed via a XSL transform"),
        w("into another documentation format.")])).
optdb(oc_opmode,    only_opmode_make_analysis_registry, bool(no),
    priv_help("make-analysis-registry", [])).
optdb(oc_opmode,    only_opmode_make,                  bool(no),
    short_help('m', "make", [], [
        w("Treat the non-option arguments to"), code("mmc"),
        w("as files to make, rather than source files."),
        w("Build or rebuild the specified files"),
        w("if they do not exist or are not up-to-date."),
        w("Note that this option also enables"),
        opt("--use-subdirs", ".")])).
% NOTE part_opmode_rebuild should be only_opmode_rebuild, but make.*.m
% look up its value as an ordinary option, NOT as a part of the op_mode.
% In one place, we clear this option, but we do it effectively because
% we want a different op_mode for a recursive invocation of the compiler.
optdb(oc_opmode,    part_opmode_rebuild,               bool(no),
    short_help('r', "rebuild", [], [
        w("A variant of the"), opt("--make"), w("option, with the"),
        w("difference being that it always rebuilds the target files,"),
        w("even if they are up-to-date.")])).
optdb(oc_opmode,    only_opmode_invoked_by_mmc_make,   bool(no),
    priv_help("invoked-by-mmc-make", [
        w("This option is only for internal use by the compiler."),
        code("mmc --make"), w("passes it as the first argument"),
        w("when compiling a module.")])).

optdb(oc_opmode,    only_opmode_output_grade_string,   bool(no),
    help("output-grade-string", [
        w("Print to standard output the canonical string"),
        w("representing the currently selected grade.")])).
optdb(oc_opmode,    only_opmode_output_grade_defines,  bool(no),
    help("output-grade-defines", [
        w("Print to standard output the C compiler flags"),
        w("that define the macros"),
        w("which specify the selected compilation grade.")])).
optdb(oc_opmode,    only_opmode_output_stdlib_grades,  bool(no),
    help("output-stdlib-grades", [
        w("Print to standard output the list of compilation grades in which"),
        w("the Mercury standard library is available with this compiler.")])).
optdb(oc_opmode,    only_opmode_output_stdlib_modules, bool(no),
    help("output-stdlib-modules", [
        w("Print to standard output the names of the modules in the"),
        w("Mercury standard library.")])).
optdb(oc_opmode,    only_opmode_output_library_install_grades, bool(no),
    alt_help("output-libgrades",
            ["output-library-install-grades"], [
        w("Print to standard output the list of compilation grades in which"),
        w("a library to be installed should be built.")])).
optdb(oc_opmode,    only_opmode_output_target_arch,    bool(no),
    help("output-target-arch", [
        w("Print to standard output the target architecture.")])).
optdb(oc_opmode,    only_opmode_output_cc,             bool(no),
    help("output-cc", [
        w("Print to standard output the command for invoking"),
        w("the C compiler.")])).
optdb(oc_opmode,    only_opmode_output_c_compiler_type, bool(no),
    alt_help("output-c-compiler-type", ["output-cc-type"], [
        w("Print to standard output the C compiler's type.")])).
optdb(oc_opmode,    only_opmode_output_cflags,         bool(no),
    help("output-cflags", [
        w("Print to standard output the flags with which the C compiler"),
        w("will be invoked.")])).
optdb(oc_opmode,    only_opmode_output_c_include_directory_flags, bool(no),
    alt_help("output-c-include-directory-flags",
            ["output-c-include-dir-flags"], [
        w("Print to standard output the C compiler flags"),
        w("that specify which directories to search for C header files."),
        w("This includes the C header files from the standard library.")])).
optdb(oc_opmode,    only_opmode_output_link_command,   bool(no),
    help("output-link-command", [
        w("Print to standard output the link command"),
        w("used to create executables.")])).
optdb(oc_opmode,    only_opmode_output_shared_lib_link_command, bool(no),
    help("output-shared-lib-link-command", [
        w("Print to standard output the link command"),
        w("used to create shared libraries.")])).
optdb(oc_opmode,    only_opmode_output_library_link_flags, bool(no),
    help("output-library-link-flags", [
        w("Print to standard output the flags"),
        w("that must be passed to the linker in order to"),
        w("link against the current set of libraries."),
        w("This includes the Mercury standard library, as well as any other"),
        w("libraries specified via either the"), opt("--ml"),
        w("or"), opt("-l"), w("option.")])).
optdb(oc_opmode,    only_opmode_output_csharp_compiler, bool(no),
    help("output-csharp-compiler", [
        w("Print to standard output the command for invoking"),
        w("the C# compiler.")])).
optdb(oc_opmode,    only_opmode_output_csharp_compiler_type, bool(no),
    help("output-csharp-compiler-type", [
        w("Print to standard output the C# compiler's type.")])).
optdb(oc_opmode,    only_opmode_output_java_class_dir, bool(no),
    alt_help("output-java-class-directory",
            ["output-class-directory", "output-java-class-dir",
            "output-class-dir"], [
        w("Print to standard output the name of the directory"),
        w("in which the compiler will place"),
        w("any generated Java class files.")])).
optdb(oc_opmode,    only_opmode_output_optimization_options, bool(no),
    alt_help("output-optimization-options",
            ["output-opt-opts"], [
        w("Print to standard output a list of the optimizations"),
        w("enabled at each optimization level.")])).
optdb(oc_opmode,    only_opmode_output_optimization_options_upto, int(-1),
    alt_arg_help("output-optimization-options-upto",
            ["output-opt-opts-upto"], "max_level", [
        w("Print to standard output a list of the optimizations"),
        w("enabled at each optimization level up to the given maximum.")])).

%---------------------------------------------------------------------------%

    % Compilation model options (ones that affect binary compatibility).
    %
    % XXX Many of the options in this section are NOT grade options.
    % Some merely control *how* a grade's functionality operates;
    % others are internal-use-only.

% Grade options, part a: specifying a complete grade

optdb(oc_grade_gen, grade,                             string_special,
    % The `mmc' script will pass the default grade determined
    % at configuration time.
    short_arg_help('s', "grade", [], "grade", [
        w("Select the compilation model."),
        w("This model, which Mercury calls a"), emph("grade", ","),
        w("specifies what properties the resulting executable or library"),
        w("should have. Properties such as"),
        help_text_texinfo(
            [w("`generates profiling data for"), code("mprof"),
                w("when executed'.")],
            [w("@samp{generates profiling data for @code{mprof}"),
                w("when executed}")]),
        w("and"),
        help_text_texinfo(
            [w("`can be debugged with the Mercury debugger'.")],
            [w("@samp{can be debugged with the Mercury debugger}.")]),
        w("As such, it controls decisions that must be made the same way"),
        w("in all the modules of a program."),
        w("For example, it does not make sense"),
        w("to compile some modules of a program to C and some to Java;"),
        w("nor does it make sense"),
        w("to compile some modules to support profiling"),
        w("and some to not support profiling."),
        blank_line,
        w("The"), arg("grade"), w("should consist of one of the base grades"),
        samp("none", ","), samp("reg", ","), samp("asm_fast", ","),
        samp("hlc", ","), samp("java", ","), w("or"), samp("csharp", ","),
        w("followed by zero or more of the grade modifiers"),
        w("in the following options."),
        % "following options" because this old list is seriously incomplete:
        % w("`.gc', `.prof', `.memprof', `.profdeep', `.tr',"),
        % w("`.spf', `.stseg', `.debug', and `.par'."),
        w("The names of all grade modifiers start with a period,"),
        w("so a complete grade name consists of a list of name components"),
        w("(the base grade and some grade modifiers) separated by periods."),
        blank_line,
        w("Note that not all combinations of components are allowed,"),
        w("and that the Mercury standard library"),
        w("will have been installed on your system"),
        w("in only a subset of the set of all possible grades."),
        blank_line,
        w("Attempting to build a program"),
        w("in a grade which has not been installed"),
        w("or to link together modules"),
        w("that have been compiled in different grades,"),
        w("will result in an error.")])).

%---------------------%

% Grade options, part b: target selection

optdb(oc_grade_target, target,                         string("c"),
    alt_arg_align_help("target", [
            arg_align("c",
                "(grades: none, reg, asm_fast, hlc)",
            "(grades: @samp{none}, @samp{reg}, @samp{asm_fast}, @samp{hlc})"),
            arg_align("csharp",
                "(grades: csharp)",
                "(grades: @samp{csharp})"),
            arg_align("java",
                "(grades: java)",
                "(grades: @samp{java})")], [
        cindex("asm_fast (compilation grade"),
        cindex("reg (compilation grade"),
        cindex("none (compilation grade"),
        cindex("hlc (compilation grade"),
        cindex("java (compilation grade"),
        cindex("csharp (compilation grade"),
        w("Specify the target language: C, C# or Java."),
        w("The default is C."),
        w("Targets other than C imply"), opt("--high-level-code"),
        w("(see below).")])).
optdb(oc_grade_target, compile_to_c,                   special,
    alt_help("compile-to-c", ["compile-to-C"], [
        w("An abbreviation for"),
        opt("--target c"), opt("-target-code-only", "."),
        w("Generate C code in"), file_var("module", "c", ","),
        w("but do not generate object code.")])).
optdb(oc_grade_target, java,                           special,
    alt_help("java", ["Java"], [
        w("An abbreviation for"), opt("--target java", ".")])).
optdb(oc_grade_target, java_only,                      special,
    alt_help("java-only", ["Java-only"], [
        w("An abbreviation for"),
        opt("--target java"), opt("--target-code-only", "."),
        w("Generate Java code in"), file_var("module", "java", ","),
        w("but do not generate Java bytecode.")])).
optdb(oc_grade_target, csharp,                         special,
    alt_help("csharp", ["C#"], [
        w("An abbreviation for"), opt("--target csharp", ".")])).
optdb(oc_grade_target, csharp_only,                    special,
    alt_help("csharp-only", ["C#-only"], [
        w("An abbreviation for"),
        opt("--target csharp"), opt("--target-code-only", "."),
        w("Generate C# code in"), file_var("module", "cs", ","),
        w("but do not generate CIL bytecode.")])).

%---------------------%

% Grade options, part c: LLDS compilation

optdb(oc_grade_llds, gcc_global_registers,             bool(yes),
    % We do not document "fast".
    no_align_help("gcc-global-registers",
            "(grades: reg, asm_fast)",
            "(grades: none)",
            "(grades: @samp{reg}, @samp{asm_fast})",
            "(grades: @samp{none})", [
        w("Specify whether to use GNU C's"),
        w("global register variables extension."),
        w("This option is used only when targeting C with"),
        opt("--no-high-level-code", ".")])).
optdb(oc_grade_llds, gcc_non_local_gotos,              bool(yes),
    % We do not document "jump", "fast", "asm_jump".
    no_align_help("gcc-non-local-gotos",
            "(grades: asm_fast)",
            "(grades: none, reg)",
            "(grades: @samp{asm_fast})",
            "(grades: @samp{none}, @samp{reg})", [
        w("Specify whether to use GNU C's"),
        quote("labels as values"), w("extension."),
        w("This option is used only when targeting C with"),
        opt("--no-high-level-code", ".")])).
optdb(oc_grade_llds, asm_labels,                       bool(yes),
    % We do not document "asm_jump".
    no_align_help("asm-labels",
            "(grades: asm_fast)",
            "(grades: none, reg)",
            "(grades: @samp{asm_fast})",
            "(grades: @samp{none}, @samp{reg})", [
        w("Specify whether to use GNU C's asm extensions for"),
        w("inline assembler labels."),
        w("This option is used only when targeting C with"),
        opt("--no-high-level-code", ".")])).
optdb(oc_grade_llds, use_float_registers,              bool(yes),
    priv_help("use-float-registers", [
        w("Use float registers for argument passing."),
        w("This option is used only when targeting C with"),
        opt("--no-high-level-code", ".")])).

%---------------------%

% Grade options, part d: MLDS compilation

optdb(oc_grade_mlds, highlevel_code,                   bool(no),
    short_alt_align_help('H', "high-level-code",
            ["high-level-c", "high-level-C",
            "highlevel-code", "highlevel-c", "highlevel-C"],
            "(grades: hlc, java, csharp)",
            "(grades: @samp{hlc}, @samp{java}, @samp{csharp})",
        [w("Use the MLDS backend,"),
        w("which generates idiomatic high-level-language code,"),
        w("rather than the LLDS backend,"),
        w("which generates assembly language code in C syntax.") ])).
optdb(oc_grade_mlds, target_debug_grade,               bool(no),
    alt_align_help("target-debug-grade", [],
            "(grades: hlc, java, csharp)",
            "(grades: @samp{hlc}, @samp{java}, @samp{csharp})",
        % XXX Why do we not document the grade *modifier*?
        [w("Require that all modules in the program be compiled to"),
        w("object code"), fixed("(for C),"),
        file(".class"), w("files"), fixed("(for Java),"),
        w("or"), file(".dll"), w("files"), fixed("(for C#)"),
        w("in a way that allows the program executable to be debuggable"),
        w("with debuggers for the target language,"),
        w("such as"), code("gdb"), w("for C."),
        w("This option is intended mainly for the developers of Mercury,"),
        w("though it can also help to debug"),
        w("foreign language code included in Mercury programs.")])).

%---------------------%

% Grade options, part e: debugging

% Grade options, part e1: mdb debugging

optdb(oc_grade_mdb, exec_trace,                        bool(no),
    % Yes, the internal and external names of this option are THAT different.
    alt_align_help("debug", [],
            "(grade modifier: `.debug')",
            "(grade modifier: @samp{.debug})", [
        cindex("Debugging"),
        cindex(".debug (grade modifier"),
        w("Enable Mercury-level debugging."),
        w("See"), ref("the", "Debugging",
            "chapter of the Mercury User's Guide", ""),
        w("for details."),
        w("This option is supported only when targeting C with"),
        opt("--no-high-level-code", ".")])).
optdb(oc_grade_mdb, decl_debug,                        bool(no),
    alt_align_help("decl-debug", [],
            "(grade modifier: `.decldebug')",
            "(grade modifier: @samp{.decldebug})", [
        cindex("Debugging"),
        cindex(".decldebug (grade modifier"),
        w("Enable full support for declarative debugging."),
        w("This allows subterm dependency tracking"),
        w("in the declarative debugger."),
        w("See"), ref("the", "Debugging",
            "chapter of the Mercury User's Guide", ""),
        w("for details."),
        w("This option is supported only when targeting C with"),
        opt("--no-high-level-code", ".")])).

%---------------------%

% Grade options, part e2: ssdb debugging

optdb(oc_grade_ssdb, source_to_source_debug,           bool(no),
    % Source-to-source debugging is not ready for the public.
    priv_alt_align_help("source-to-source-debug",
            ["ss-debug", "ssdb"],
            "(grade modifier: `.ssdebug')",
            "(grade modifier: @samp{.ssdebug})", [
        cindex(".ssdebug (grade modifier"),
        w("Enable the source-to-source debugging transform.")])).

%---------------------%

% Grade options, part f: profiling

% Grade options, part f1: mprof profiling

% XXX The profiling and time_profiling options are ALMOST identical;
% the only difference is that profiling can be negated, while
% time_profiling cannot. I (zs) do not understand what need there is
% for time_profiling, especially since it is not documented.
optdb(oc_grade_mprof, profiling,                       bool_special,
    short_alt_align_help('p', "profiling", [],
            "(grade modifier: `.prof')",
            "(grade modifier: @samp{.prof})", [
        cindex("Profiling"),
        cindex("Time profiling"),
        cindex(".prof (grade modifier"),
        w("Prepare the generated code for time profiling"),
        w("by Mercury's version of the standard Unix profiler"),
        code("gprof", ","),
        w("which is a tool called"), code("mprof", "."),
        w("In"), samp(".prof"), w("grades, the compiler will insert"),
        w("profiling hooks into the generated code (e.g. to count calls),"),
        w("and will also output the static call graph of the module to"),
        file_var("module", "prof"), w("for use by"), code("mprof", "."),
        w("Please see the"),
        ref("", "Building profiled applications", "section"),
        w("in the Mercury User's Guide for details."),
        blank_line,
        w("This option is supported only when targeting C.")])).
optdb(oc_grade_mprof, time_profiling,                  special,
    % XXX This option should not be visible even to developers.
    priv_help("time-profiling", [])).
optdb(oc_grade_mprof, memory_profiling,                special,
    alt_align_help("memory-profiling", [],
            "(grade modifier: `.memprof')",
            "(grade modifier: @samp{.memprof})", [
        cindex("Profiling"),
        cindex("Memory profiling"),
        cindex("Heap profiling"),
        cindex("Allocation profiling"),
        cindex(".memprof (grade modifier"),
        w("Prepare the generated code for"),
        w("profiling of memory usage and retention by mprof."),
        w("Please see"),
        ref("the", "Using mprof for profiling memory allocation", ""),
        w("and"),
        ref("", "Using mprof for profiling memory retention", "sections"),
        w("in the Mercury User's Guide for details."),
        blank_line,
        w("This option is supported only when targeting C.")])).
    % XXX The next three options are private, because they are not useful.
    % The idea was for you to be able to use --profile-calls and
    % --profile-time separately, but that doesn't work because compiling
    % with --profile-time instead of --profile-calls results in
    % different code addresses, so you can't combine the data
    % from versions of your program compiled with different options.
optdb(oc_grade_mprof, profile_calls,                   bool(no),
    priv_alt_align_help("profile-calls", [],
            "(grade modifier: `.profcalls')",
            "(grade modifier: @samp{.profcalls})", [
        cindex(".profcalls (grade modifier"),
        w("Similar to"), opt("--profiling", ","),
        w("except that it only gathers call counts, not timing information."),
        w("Useful on systems where time profiling is not supported,"),
        w("but not as useful as"), opt("--memory-profiling", ".")])).
optdb(oc_grade_mprof, profile_time,                    bool(no),
    priv_alt_align_help("profile-time", [],
            "(grade modifier: `.proftime')",
            "(grade modifier: @samp{.proftime})", [
        cindex(".proftime (grade modifier"),
        w("Similar to"), opt("--profiling", ","),
        w("except that it only gathers timing information,"),
        w("not call counts.")])).
optdb(oc_grade_mprof, profile_memory,                  bool(no),
    priv_alt_align_help("profile-memory", [],
            "(grade modifier: `.profmem')",
            "(grade modifier: @samp{.profmem})", [
        cindex(".profmem (grade modifier"),
        w("Similar to"), opt("--memory-profiling", ","),
        w("except that it only gathers memory usage information,"),
        w("not call counts.")])).

%---------------------%

% Grade options, part f2: mdprof profiling

optdb(oc_grade_mdprof, deep_profiling,                 special,
    alt_align_help("deep-profiling", [],
            "(grade modifier: `.profdeep')",
            "(grade modifier: @samp{.profdeep})", [
        cindex("Deep profiling"),
        cindex(".profdeep (grade modifier"),
        w("Prepare the generated code for deep profiling."),
        w("The Mercury deep profiling tool"), code("mdprof"),
        w("(note the"), quote("d"), w("in the name)"),
        w("associates much more context with each measurement than"),
        code("mprof", ","), w("making it much more suitable"),
        w("for handling polymorphic code and higher order code,"),
        w("both of which are much more common"),
        w("in typical Mercury code than in typical C code."),
        w("This option is supported only when targeting C with"),
        opt("--no-high-level-code", ".")])).
optdb(oc_grade_mdprof, profile_deep,                   bool(no),
    % This the *actual* grade option that switches on deep profiling.
    % The deep_profiling option sets profile_deep to "yes", *and* sets
    % the grade options for mprof profiling to "no".
    priv_help("profile-deep", [])).
optdb(oc_grade_mdprof, use_activation_counts,          bool(no),
    % use_activation_counts is an experimental feature.
    % It *is* a grade option.
    % Use_activation_counts is used to determine which mechanism for
    % cycle detection should be used for deep profiling. Actually,
    % we only want to use the `yes' value, but we keep support for
    % the `no' value for benchmarks for the paper.
    priv_help("use-activation-counts", [])).
% The next three options are developer-only non-grade options.
optdb(oc_grade_mdprof, use_zeroing_for_ho_cycles,      bool(yes),
    priv_help("use-zeroing-for-ho-cycles", [])).
optdb(oc_grade_mdprof, use_lots_of_ho_specialization,  bool(no),
    priv_help("use-lots-of-ho-specialization", [])).
% XXX This is not a grade option.
optdb(oc_grade_mdprof, deep_profile_tail_recursion,    bool(no),
    % We do not currently enable (or publicly document) this option
    % because its use results in significant overheads. Also, it is
    % not compatible with coverage profiling, which is enabled by default.
    % By default, all deep profiling grades are also built with
    % --stack-segments in order to avoid *some* of the problems
    % caused by the lack of tail recursion.
    priv_help("deep-profile-tail-recursion", [])).
optdb(oc_grade_mdprof, coverage_profiling,             bool(yes),
    help("coverage-profiling", [
        cindex("Coverage profiling"),
        w("Do not gather deep profiling information"),
        w("that is useful only for coverage profiling.")])).
% The next two options are intended for experiments.
% XXX They are not grade options.
optdb(oc_grade_mdprof, coverage_profiling_via_calls,   bool(no),
    priv_help("coverage-profiling-via-calls", [
        w("Use calls to implement coverage points,"),
        w("not inline foreign code.")])).
optdb(oc_grade_mdprof, coverage_profiling_static,      bool(no),
    % This help text could be clearer.
    priv_help("coverage-profiling-static", [
        w("Disable coverage profiling of ProcDynamics;"),
        w("cover only ProcStatics."),
        w("This uses less memory, and may be faster.")])).
% The next four options control coverage profiling (part of deep profiling):
% they enable different types of coverage points.
% XXX They are not grade options.
optdb(oc_grade_mdprof, profile_deep_coverage_after_goal, bool(yes),
    priv_help("profile-deep-coverage-after-goal", [
        w("Disable coverage points after goals.")])).
optdb(oc_grade_mdprof, profile_deep_coverage_branch_ite, bool(yes),
    priv_help("profile-deep-coverage-branch-ite", [
        w("Disable coverage points at the beginning of then and else"),
        w("branches.")])).
optdb(oc_grade_mdprof, profile_deep_coverage_branch_switch, bool(yes),
    priv_help("profile-deep-coverage-branch-switch", [
        w("Disable coverage points at the beginning of switch branches.")])).
optdb(oc_grade_mdprof, profile_deep_coverage_branch_disj, bool(yes),
    priv_help("profile-deep-coverage-branch-disj", [
        w("Disable coverage points at the beginning of"),
        w("disjunction branches.")])).
% The next two options tune the coverage profiling pass, useful for debugging.
% I believe these options are broken - pbone.
% XXX They are not grade options.
optdb(oc_grade_mdprof, profile_deep_coverage_use_portcounts, bool(no),
    priv_help("profile-deep-coverage-use-portcounts", [
        w("Use port counts to provide coverage information.")])).
optdb(oc_grade_mdprof, profile_deep_coverage_use_trivial, bool(no),
    priv_help("profile-deep-coverage-use-trivial", [
        w("Use simple goal properties for coverage information.")])).
% XXX I (zs) don't think this is a grade option.
optdb(oc_grade_mdprof, profile_for_feedback,           bool(no),
    % Turns on flags relevant for profiler directed feedback analysis.
    % Currently the only feedback analysis is automatic parallelism.
    alt_help("profile-for-feedback",
            ["profile-for-implicit-parallelism"], [
        cindex("Coverage profiling"),
        cindex("Profiler feedback"),
        cindex("Automatic parallelism"),
        w("Select deep profiling options that are suitable"),
        w("for profiler directed implicit parallelism."),
        opt("--profile-for-implicit-parallelism"),
        w("is a deprecated synonym for this option.")])).

%---------------------%

% Grade options, part f3: complexity profiling

optdb(oc_grade_clprof, experimental_complexity,        string(""),
    % XXX This is NOT a grade option; it only takes advantage of a grade.
    priv_arg_help("experimental-complexity", "filename", [
        w("Enable experimental complexity analysis for the predicates"),
        w("listed in the given file."),
        w("This option is supported only when targeting C with"),
        opt("--no-high-level-code", ".")])).
optdb(oc_grade_clprof, record_term_sizes_as_words,     bool(no),
    priv_alt_align_help("record-term-sizes-as-words", [],
            "(grade modifier: `.tsw')",
            "(grade modifier: @samp{.tsw})", [
        cindex(".tsw (grade modifier"),
        w("Augment each heap cell with its size in words.")])).
optdb(oc_grade_clprof, record_term_sizes_as_cells,     bool(no),
    priv_alt_align_help("record-term-sizes-as-cells", [],
            "(grade modifier: `.tsc')",
            "(grade modifier: @samp{.tsc})", [
        cindex(".tsc (grade modifier"),
        w("Augment each heap cell with its size in cells.")])).

%---------------------%

% Grade options, part f4: threadscope profiling

optdb(oc_grade_tsprof, threadscope,                    bool(no),
    % XXX Threadscope profiling has not been maintained since Paul stopped
    % working on Mercury. It has almost certainly suffered bit rot by now.
    priv_alt_align_help("threadscope", [],
            "(grade modifier: `.threadscope')",
            "(grade modifier: @samp{.threadscope})", [
        cindex(".threadscope (grade modifier"),
        w("Enable support for profiling parallel execution."),
        w("using Threadscope."),
        w("This option is supported only when targeting C"),
        w("in parallel grades with"), opt("--no-high-level-code", ","),
        w("and only on some processors."),
        w("See Documentation/README.ThreadScope and"),
        ref("the", "Using threadscope", "section", ""), w("for details.")])).

%---------------------%

% Grade options, part g: miscellaneous

optdb(oc_grade_etc, pregenerated_dist,                 bool(no),
    % XXX The pregen grade component *should* be documented.
    priv_help("pregenerated-dist", [])).
optdb(oc_grade_etc, gc,                                string("boehm"),
    % We do not document the "accurate" and "hgc" GC methods,
    % as those methods are still experimental.
    alt_arg_help("garbage-collection", ["gc"],
            "{none, boehm, automatic}", [
        cindex(".gc (grade modifier"),
        % "`hgc' is our own conservative collector;",
        % "`accurate' is our own type-accurate copying GC;",
        % "it requires `--high-level-code'.",
        w("Specify which method of garbage collection to use."),
        w("When targeting Java or C#, the only possible choice is"),
        samp("automatic", ","), w("which means the garbage collector"),
        w("built into the target language."),
        w("When targeting C, the usual choice is"), samp("boehm", ","),
        w("which is Hans Boehm et al's conservative collector."),
        w("The use of the Boehm collector is indicated by the"),
        samp(".gc"), w("grade component."),
        w("The other alternative when targeting C is"), samp("none", ","),
        w("meaning there is no garbage collector."),
        w("This works only for programs with very short runtimes.")])).
optdb(oc_grade_etc, stack_segments,                    bool(no),
    alt_align_help("stack-segments", [],
            "(grade modifier: `.stseg')",
            "(grade modifier: @samp{.stseg})", [
        cindex(".stseg (grade modifier"),
        w("Specify the use of dynamically sized stacks that are"),
        w("composed of small segments. This can help to avoid stack"),
        w("exhaustion at the cost of increased execution time."),
        w("This option is supported only when targeting C with"),
        opt("--no-high-level-code", ".")])).
optdb(oc_grade_etc, extend_stacks_when_needed,         bool(no),
    % This is private as this feature is still experimental.
    priv_help("extend-stacks-when-needed", [
        cindex(".exts (grade modifier"),
        w("Specify that code that increments the stack pointer"),
        w("must extend the stack on demand."),
        w("This option is supported only when targeting C with"),
        opt("--no-high-level-code", ".")])).
optdb(oc_grade_etc, use_trail,                         bool(no),
    alt_align_help("use-trail", [],
            "(grade modifier: `.tr')",
            "(grade modifier: @samp{.tr})", [
        cindex(".tr (grade modifier"),
        w("Enable use of a trail."),
        w("This is necessary for interfacing with constraint solvers,"),
        w("or for backtrackable destructive update."),
        w("This option is supported only when targeting C.")])).
optdb(oc_grade_etc, single_prec_float,                 bool(no),
    alt_align_help("single-precision-float",
            ["single-prec-float"],
            "(grade modifier: `.spf')",
            "(grade modifier: @samp{.spf})", [
        w("Use single precision floats so that, on 32-bit machines,"),
        w("floating point values don't need to be boxed."),
        w("The default is to use double precision floats."),
        w("This option is supported only when targeting C.")])).
optdb(oc_grade_etc, parallel,                          bool(no),
    alt_align_help("parallel", [],
            "(grade modifier: `.par')",
            "(grade modifier: @samp{.par})", [
        cindex(".par (grade modifier"),
        % XXX These two should be *different* grade modifiers.
        w("Enable concurrency and parallel conjunction support for the"),
        w("low-level C grades."),
        w("Enable concurrency (via pthreads) for the high-level C grades.")])).
optdb(oc_grade_etc, maybe_thread_safe_opt,             string("no"),
    % XXX How is a yes/no argument better than a no- prefix?
    arg_help("maybe-thread-safe", "{yes, no}", [
        w("Specify how the compiler should treat the"),
        code("maybe_thread_safe"), w("foreign code attribute."),
        samp("yes"), w("means that a foreign procedure with the"),
        code("maybe_thread_safe"), w("attribute is treated"),
        w("as if it has a"), code("thread_safe"), w("attribute."),
        samp("no"), w("means that the foreign procedure is treated"),
        w("as if it has a"), code("not_thread_safe"), w("attribute."),
        w("The default is"), samp("no", ".")])).
optdb(oc_grade_etc, use_minimal_model_stack_copy,      bool(no),
    % This controls the .mmsc grade component.
    priv_help("use-minimal-model-stack-copy", [
        cindex(".mmsc (grade modifier"),
        w("Enable the use of the standard form of minimal model tabling.")])).
optdb(oc_grade_etc, use_minimal_model_own_stacks,      bool(no),
    % This controls the .mmos grade component.
    priv_help("use-minimal-model-own-stacks", [
        cindex(".mmos (grade modifier"),
        w("Enable the use of an experimental form of"),
        w("minimal model tabling.")])).
optdb(oc_grade_etc, minimal_model_debug,               bool(no),
    % This turns .mmos into .dmmos and .mmsc into .dmmsc.
    priv_help("minimal-model-debug", [
        cindex(".dmmsc (grade modifier"),
        cindex(".dmmos (grade modifier"),
        w("Enables extra data structures that assist in debugging"),
        w("minimal model tabling.")])).
% RBMM is undocumented since it is still experimental.
optdb(oc_grade_etc, use_regions,                       bool(no),
    priv_alt_align_help("use-regions", [],
            "(grade modifier: `.rbmm')",
            "(grade modifier: @samp{.rbmm})", [
        cindex(".rbmm (grade modifier"),
        w("Enable support for region-based memory management.")])).
optdb(oc_grade_etc, use_alloc_regions,                 bool(yes),
    priv_help("use-alloc-regions", [
        w("Compute and use the exact set of regions"),
        w("that may be allocated into by a call.")])).
% use_regions_debug and use_regions_profiling *are* (private) grade options.
% XXX They should be documented.
optdb(oc_grade_etc, use_regions_debug,                 bool(no),
    % This option affects the grade by appending to a grade modifier.
    priv_help("use-regions-debug", [])).
optdb(oc_grade_etc, use_regions_profiling,             bool(no),
    % This option affects the grade by appending to a grade modifier.
    priv_help("use-regions-profiling", [])).

%---------------------%

% Grade options, part h: data representation/developer

optdb(oc_grade_dev, num_ptag_bits,                     int(-1),
    % -1 means: use the value of conf_low_ptag_bits instead.
    %
    % Normally, the --num-tag-bits option is used only by the compiler.
    % By default, its value is set to the value of the --conf-low-tag-bits
    % option when targeting C, and to zero when targeting other languages.
    % Its only legitimate use by non-developers is for cross-compilation.
    % XXX That fact should be included in the help text.
    alt_arg_help("num-ptag-bits",
            ["num-tag-bits"], "N", [
        cindex("Tags"), cindex("Data representation"),
        w("Use"), bare_arg("N"), w("primary tag bits."),
        w("Note that the value of this option is normally autoconfigured;"),
        w("its use should never be needed except for cross-compilation"),
        w("to an architecture where autoconfiguration would yield"),
        w("a different value.")])).
optdb(oc_grade_dev, bits_per_word,                     int(32),
    % A good default for the current generation of architectures.
    priv_arg_help("bits-per-word", "N", [
        w("Reserved for use by the `mmc' script.")])).
optdb(oc_grade_dev, bytes_per_word,                    int(4),
    % A good default for the current generation of architectures.
    priv_arg_help("bytes-per-word", "N", [
        w("Reserved for use by the `mmc' script.")])).
% XXX All the unboxed_X options should be replaced by boxed_X options.
optdb(oc_grade_dev, unboxed_float,                     bool(no),
    priv_help("unboxed-float", [
        w("Do not box floating point numbers."),
        w("This assumes that a Mercury float will fit in a word."),
        w("The C code must be compiled with"), code("-UMR_BOXED_FLOAT", "."),
        w("It may also need to be compiled with"),
        code("-DMR_USE_SINGLE_PREC_FLOAT", ","),
        w("if double precision floats do not fit into a word."),
        w("Note that the value of this option is normally autoconfigured;"),
        w("its use should never be needed except for cross-compilation.")])).
optdb(oc_grade_dev, unboxed_int64s,                    bool(no),
    priv_help("unboxed-int64s", [
        w("Do not box 64-bit integer numbers."),
        w("This assumes that word size of the target machine is at least"),
        w("64-bits in size."),
        w("The C code must be compiled with"), code("-UMR_BOXED_INT64S", "."),
        w("Note that the value of this option is normally autoconfigured;"),
        w("its use should never be needed except for cross-compilation.")])).
optdb(oc_grade_dev, unboxed_no_tag_types,              bool(yes),
    priv_help("unboxed-no-tag-types", [
        w("Box no-tag types. (By default, no-tag types are unboxed.)"),
        w("Note that the value of this option is normally autoconfigured;"),
        w("its use should never be needed except for cross-compilation.")])).
optdb(oc_grade_dev, arg_pack_bits,                     int(-1),
    % -1 is a special value which means use all word bits
    % for argument packing.
    priv_arg_help("arg-pack-bits", "N", [
        w("The number of bits in a word in which to pack"),
        w("constructor arguments."),
        w("Note that the value of this option is normally autoconfigured;"),
        w("its use should never be needed except for cross-compilation.")])).
optdb(oc_grade_dev, pack_everything,                   bool(no),
    priv_help("pack-everything", [
        w("Tell decide_type_repn.m to pack everything that can be packed.")])).
optdb(oc_grade_dev, allow_direct_args,                 bool(yes),
    priv_help("allow-direct-args", [
        w("Allow the direct arg optimization.")])).
optdb(oc_grade_dev, allow_double_word_fields,          bool(yes),
    priv_help("allow-double-word-fields", [
        w("Disallow storing a single constructor argument in two words."),
        w("(This mainly applies to arguments that are"),
        w("double-precision floats or whose type is int64 or uint64.")])).
optdb(oc_grade_dev, allow_double_word_ints,            bool(no),
    priv_help("allow-double-word-ints", [])).
optdb(oc_grade_dev, allow_packing_dummies,             bool(no),
    priv_help("allow-packing-dummies", [])).
optdb(oc_grade_dev, allow_packing_ints,                bool(no),
    priv_help("allow-packing-ints", [])).
optdb(oc_grade_dev, allow_packing_chars,               bool(no),
    priv_help("allow-packing-chars", [])).
optdb(oc_grade_dev, allow_packing_local_sectags,       bool(no),
    priv_help("allow-packing-local-sectags", [])).
optdb(oc_grade_dev, allow_packing_remote_sectags,      bool(no),
    priv_help("allow-packing-remote-sectags", [])).
optdb(oc_grade_dev, allow_packing_mini_types,          bool(no),
    priv_help("allow-packing-mini-types", [])).
optdb(oc_grade_dev, allow_packed_unify_compare,        bool(no),
    priv_help("allow-packed-unify-compare", [])).
optdb(oc_grade_dev, sync_term_size_in_words,           int(8),
    % 8 is the size on linux (at the time of writing) - will usually be
    % overridden by a value from configure.
    priv_alt_arg_help("sync-term-size-in-words", ["sync-term-size"],
        "num_words", [])).

%---------------------------------------------------------------------------%

    % Inference control options.

optdb(oc_infer,     infer_all,                         bool_special,
    help("infer-all", [
        cindex("Inference"),
        w("This option is an abbreviation for the combination of"),
        opt("--infer-types", ","), opt("--infer-modes", ","),
        w("and"), opt("--infer-det", ".")])).
optdb(oc_infer,     infer_types,                       bool(no),
    help("infer-types", [
        cindex("Inference of types"),
        cindex("Type inference"),
        w("If there is no type declaration for a predicate or function,"),
        w("try to infer its type, instead of just reporting an error.")])).
optdb(oc_infer,     infer_modes,                       bool(no),
    help("infer-modes", [
        cindex("Inference of modes"),
        cindex("Mode inference"),
        w("If there is no mode declaration for a predicate,"),
        w("try to infer its mode (or modes),"),
        w("instead of just reporting an error.")])).
optdb(oc_infer,     infer_det,                         bool(yes),
    alt_help("infer-determinism", ["infer-det"], [
        cindex("Inference of determinisms"),
        cindex("Determinism inference"),
        w("If there is no determinism declaration for a procedure"),
        w("(a mode of a predicate or of a function),"),
        w("just report an error; do not try to infer its determinism.")])).
optdb(oc_infer,     type_inference_iteration_limit,    int(60),
    arg_help("type-inference-iteration-limit", "N", [
        cindex("Inference of types"),
        cindex("Type inference"),
        w("Perform at most"), arg("N"),
        w("passes of type inference (default: 60).")])).
optdb(oc_infer,     mode_inference_iteration_limit,    int(30),
    arg_help("mode-inference-iteration-limit", "N", [
        cindex("Inference of modes"),
        cindex("Mode inference"),
        w("Perform at most"), arg("N"),
        w("passes of mode inference (default: 30).")])).
optdb(oc_infer,     allow_stubs,                       bool(no),
    % A stub is sort-of like inferring a clause body, right ?-)
    help("allow-stubs", [
        cindex("Stubs"),
        cindex("Procedures with no clauses"),
        cindex("No clauses, procedures with"),
        cindex("Clauses, procedures without"),
        w("Allow procedures to have no clauses. Any calls to"),
        w("such procedures will raise an exception at run-time."),
        w("This option is sometimes useful during program development."),
        w("(See also the documentation for the"), opt("--warn-stubs"),
        w("option in"), ref("the", "Warning options", "section", ".)")])).

%---------------------------------------------------------------------------%

    % Language semantics options.

% XXX Add a mechanism to print_help.m that allows us to include
% these lines from the old invocation chapter:
%
% @cindex Semantics options
% @cindex Order of execution
% @cindex Reordering
% @cindex Optimization
%
% See the Mercury language reference manual for detailed explanations
% of these options.

optdb(oc_semantics, strict_sequential,                 special,
    help("strict-sequential", [
        w("This option is an abbreviation for the combination of"),
        opt("--no-reorder-conj", ","), opt("--no-reorder-disj", ","),
        w("and"), opt("--fully-strict", ".")])).
optdb(oc_semantics, reorder_conj,                      bool(yes),
    help("reorder-conj", [
        w("Execute conjunctions left-to-right. Do not reorder conjuncts,"),
        w("except where the modes require it (to put the producer"),
        w("of each variable before all its consumers).")])).
optdb(oc_semantics, reorder_disj,                      bool(yes),
    help("reorder-disj", [
        w("Execute disjunctions strictly left-to-right;"),
        w("do not reorder disjuncts.")])).
optdb(oc_semantics, fully_strict,                      bool(yes),
    help("fully-strict", [
        w("Allow infinite loops, and goals whose determinism is erroneous,"),
        w("to be optimised away.")])).

%---------------------------------------------------------------------------%

    % Verbosity options.

optdb(oc_verbosity, verbose,                           bool(no),
    short_help('v', "verbose", [], [
        w("Output progress messages at each stage in the compilation.")])).
optdb(oc_verbosity, very_verbose,                      bool(no),
    short_help('V', "very-verbose", [], [
        w("Output very verbose progress messages.")])).
optdb(oc_verbosity, statistics,                        bool(no),
    short_help('S', "statistics", [], [
        w("Output messages about the compiler's time/space usage"),
        w("at the boundaries between phases of the compiler.")])).
% Disable this part the help text while --trad-passes is private.
%       w("At the moment this option implies"),
%       opt("--no-trad-passes", ","), w("so you get information"),
%       w("at the boundaries between phases of the compiler.")])).
optdb(oc_verbosity, verbose_make,                      bool(yes),
    help("verbose-make", [
        w("Disable messages about the progress of builds when using"),
        code("mmc --make", ".")])).
optdb(oc_verbosity, output_compile_error_lines,        maybe_int(yes(100)),
    arg_help("output-compile-error-lines", "N", [
        w("With"), opt("--make", ","),
        w("output the first"), arg("N"), w("lines of the"),
        file(".err"), w("file after compiling a module (default: 100)."),
        w("Specifying"), opt("--no-output-compile-error-lines"),
        w("removes the limit.")])).
optdb(oc_verbosity, verbose_recompilation,             bool(no),
    priv_help("verbose-recompilation", [
        w("When using"), opt("--smart-recompilation", ","),
        w("output messages explaining"),
        w("why a module needs to be recompiled.")])).
optdb(oc_verbosity, find_all_recompilation_reasons,    bool(no),
    priv_help("find-all-recompilation-reasons", [
        w("Find all the reasons why a module needs to be recompiled,"),
        w("not just the first."),
        w("Implies"), opt("--verbose-recompilation", ".")])).
optdb(oc_verbosity, verbose_commands,                  bool(no),
    help("verbose-commands", [
        w("Output each external command before it is run."),
        w("Note that some commands will only be printed with"),
        opt("--verbose", ".")])).

%---------------------------------------------------------------------------%

    % Options that control diagnostics.

optdb(oc_diag_gen,  verbose_errors,                    bool(no),
    short_help('E', "verbose-error-messages", ["verbose-errors"], [
        w("Some error messages have two versions:"),
        w("a standard version intended for experienced users, and"),
        w("a verbose version intended for new users."),
        w("The default is to print the first version."),
        w("This option tells the compiler to print the second version,"),
        w("which will offer a more detailed explanation"),
        w("of any errors it finds in your code.")])).
optdb(oc_diag_gen,  reverse_error_order,               bool(no),
    help("reverse-error-order", [
        w("Print error messages in descending order of their line numbers,"),
        w("instead of the usual ascending order. This is useful if you want"),
        w("to work on the last errors in a file first.")])).
optdb(oc_diag_gen,  max_error_line_width,              maybe_int(yes(79)),
    arg_help("max-error-line-width", "N", [
        w("Set the maximum width of an error message line to"),
        arg("N"), w("characters"),
        w("(unless a long single word forces the line over this limit)."),
        w("Specifying"), opt("--no-max-error-line-width"),
        w("removes the limit.")])).
optdb(oc_diag_gen,  limit_error_contexts,              accumulating([]),
    arg_help("limit-error-contexts",
            "filename:minline1-maxline1,minline2-maxline2", [
        w("Print errors and warnings for the named file only when their"),
        w("line number is in one of the specified ranges."),
        w("The minimum or maximum line number in each range may be omitted,"),
        w("in which case the range has no lower or upper bound respectively."),
        w("Multiple"), opt("--limit-error-context"), w("options accumulate."),
        w("If more than one"), opt("--limit-error-context"),
        w("option is given for the same file,"),
        w("only the last one will have an effect."),
        w("If the file name and colon are missing, the limit will apply"),
        w("to all files.")])).
optdb(oc_diag_gen,  error_files_in_subdir,             bool(no),
    help("error-files-in-subdir", [
        cindex("File names"),
        cindex("Directories"),
        cindex("Subdirectories"),
        cindex("@file{Mercury} subdirectory"),
        w("This option causes"), code("mmc --make"),
        w("to put"), file(".err"), w("files into the"),
        file("Mercury"), w("subdirectory instead of the current directory."),
        w("(This option has no effect on"), code("mmake", ".)")])).
optdb(oc_diag_gen,  std_int_file_not_written_msgs,     bool(no),
    % We use this option to eliminate the need for a .int_err_exp file for the
    % -use-subdir case for every test in the tests/invalid_make_int
    % directory.
    priv_help("std-int-file-not-written-msgs", [
        w("Standardize messages about interface files not being written"),
        w("by omitting any directory name components from file names.")])).
optdb(oc_diag_gen,  typecheck_ambiguity_warn_limit,    int(50),
    arg_help("typecheck-ambiguity-warn-limit", "N", [
        w("Set the number of type assignments required to generate a"),
        w("warning about highly ambiguous overloading to"), arg("N", "."),
        w("(Default: 50.)")])).
optdb(oc_diag_gen,  typecheck_ambiguity_error_limit,   int(3000),
    arg_help("typecheck-ambiguity-error-limit", "N", [
        w("Set the number of type assignments required to generate an error"),
        w("about excessively ambiguous overloading to"), arg("N", "."),
        w("If this limit is reached, the typechecker"),
        w("will not process the predicate or function any further."),
        w("(Default: 3000.)")])).

optdb(oc_diag_color, color_diagnostics,                 bool_special,
    alt_help("color-diagnostics",
            ["colour-diagnostics"], [
        w("Disable the use of colors in diagnostic messages. Please see"),
        ref("the", "Enabling the use of color",
            "section in the Mercury User's Guide"),
        w("for the details.")])).
optdb(oc_diag_color, config_default_color_diagnostics,  bool(yes),
    % This option should be used only by the configure script.
    priv_alt_help("config-default-color-diagnostics",
            ["config-default-colour-diagnostics"], [
        w("The default value of the"), opt("--color-diagnostics"),
        w("option, set by the configure script.")])).
optdb(oc_diag_int,  color_diagnostics_is_set,          bool(no), no_help).
optdb(oc_diag_int,  color_diagnostics_is_set_to,       bool(no), no_help).
optdb(oc_diag_int,  use_color_diagnostics,             bool(no), no_help).
optdb(oc_diag_color, color_scheme,                      string_special,
    alt_arg_help("color-scheme",
            ["colour-scheme"], "ColorScheme", [
        w("Specify the color scheme to use for diagnostics, if the use of"),
        w("color in diagnostics is enabled. For information about how the"),
        w("compiler uses colors in diagnostic messages, and about the"),
        w("syntax of color scheme specifications, please see"),
        ref("the", "Color schemes",
            "section in the Mercury User's Guide", ".")])).
optdb(oc_diag_int,  color_scheme_envvar,               string_special,
    priv_arg_help("color-scheme-envvar", "ColorScheme", [])).
optdb(oc_diag_int,  color_scheme_set_by,               string("default"),
    no_help).
optdb(oc_diag_int,  color_scheme_set_to,               string("light16"),
    no_help).
optdb(oc_diag_int,  ignore_color_scheme_envvar,        bool(no),
    % This option should be used only by our test suite.
    priv_help("ignore-color-scheme-envvar", [
        w("Ignore the"), opt("--color-scheme-envvar"), w("option.")])).
optdb(oc_diag_int,  set_color_subject,                 string(""), no_help).
optdb(oc_diag_int,  set_color_correct,                 string(""), no_help).
optdb(oc_diag_int,  set_color_incorrect,               string(""), no_help).
optdb(oc_diag_int,  set_color_inconsistent,            string(""), no_help).
optdb(oc_diag_int,  set_color_hint,                    string(""), no_help).

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

% XXX Support a third level, subsections, in texinfo help,
% to make these blocks of options visible in the user guide.

% Warnings about module-level issues.

optdb(oc_warn_dodgy_mod, warn_nothing_exported,        bool(yes),
    help("warn-nothing-exported", [
        w("Do not warn about modules which export nothing.")])).
optdb(oc_warn_dodgy_mod, warn_unused_imports,          bool(no),
    help("warn-unused-imports", [
        w("Warn about modules that are imported but not used.")])).
optdb(oc_warn_dodgy_mod, warn_unused_interface_imports, bool(yes),
    alt_help("warn-unused-interface-imports", ["warn-interface-imports"], [
        w("Do not warn about modules imported in the interface,"),
        w("but which are not used in the interface.")])).
optdb(oc_warn_dodgy_mod, warn_interface_imports_in_parents, bool(no),
    help("warn-interface-imports-in-parents", [
        w("Warn about modules that are imported in the interface of"),
        w("a parent module, but not used in the interface of that module.")])).
optdb(oc_warn_dodgy_mod, warn_stdlib_shadowing,        bool(yes),
    help("warn-stdlib-shadowing", [
        w("Do not generate warnings for module names that either duplicate"),
        w("the name of a module in the Mercury standard library, or contain"),
        w("a subsequence of name components that do so.")])).
optdb(oc_warn_dodgy_mod, warn_duplicate_abstract_instances,     bool(yes),
    help("warn-duplicate-abstract-instances", [
        w("Do not warn about duplicate abstract typeclass instances."),
        w("(Duplicate concrete typeclass instances are errors,"),
        w("and are always reported.)")])).
optdb(oc_warn_dodgy_mod, warn_too_private_instances,   bool(no),
    help("warn-too-private-instances", [
        w("An instance declaration has to be private"),
        w("if it is for a private type class (meaning a type class"),
        w("that is not visible outside the current module),"),
        w("if some of its member types refer to private type constructors,"),
        w("or if its class constraints refer to"),
        w("private type classes or private type constructors."),
        w("Such instances can only be relevant in the current module."),
        w("Generate a warning if an instance declaration"),
        w("that can be relevant outside the current module"),
        w("is not exported.")])).
optdb(oc_warn_dodgy_mod, warn_subtype_ctor_order,      bool(yes),
    help("warn-subtype-ctor-order", [
        w("Do not warn about a subtype definition that lists its"),
        w("data constructors in a different order than its supertype.")])).
optdb(oc_warn_dodgy_mod, warn_trans_opt_deps_spec,     bool(yes),
    priv_help("warn-trans-opt-deps-spec", [
        w("Do not warn about missing or unknown module names"),
        w("in files named by --trans-opt-deps-spec options.")])).

% Warnings about predicate determinism issues.

optdb(oc_warn_dodgy_pred, warn_det_decls_too_lax,      bool(yes),
    help("warn-det-decls-too-lax", [
        w("Do not warn about determinism declarations"),
        w("which could be stricter.")])).
optdb(oc_warn_dodgy_pred, warn_inferred_erroneous,     bool(yes),
    help("warn-inferred-erroneous", [
        w("Do not warn about procedures whose determinism"),
        w("is inferred to be"), samp("erroneous", ","),
        w("but whose determinism declarations are looser.")])).

% Warnings about other predicate-level issues.

optdb(oc_warn_dodgy_pred, warn_unresolved_polymorphism, bool(yes),
    help("warn-unresolved-polymorphism", [
        w("Do not warn about unresolved polymorphism, which occurs when"),
        w("the type of a variable contains a type variable"),
        w("that is not bound to an actual type, even though it should be.")])).
optdb(oc_warn_dodgy_pred, warn_stubs,                  bool(yes),
    help("warn-stubs", [
        w("Do not warn about procedures for which there are no clauses."),
        w("Note that this option is meaningful only if the"),
        opt("--allow-stubs"), w("option is enabled.")])).
optdb(oc_warn_dodgy_pred, warn_cannot_table,           bool(yes),
    help("warn-cannot-table", [
        w("Do not warn about tabling pragmas which for some reason"),
        w("cannot be applied to the specified procedure.")])).
optdb(oc_warn_dodgy_pred, warn_non_term_special_preds, bool(yes),
    help("warn-non-term-special-preds", [
        w("Do not warn about types that have user-defined equality or"),
        w("comparison predicates that cannot be proved to terminate."),
        w("This option is meaningful only if"),
        w("termination analysis is enabled.")])).
optdb(oc_warn_dodgy_pred, warn_non_stratification,     bool(no),
    help("warn-non-stratification", [
        w("Warn about possible non-stratification"),
        w("of the predicates and/or functions in the module."),
        w("Non-stratification occurs when a predicate or function can call"),
        w("itself through negation through some path in its call graph.")])).
optdb(oc_warn_dodgy_pred, warn_unneeded_purity_pred_decl, bool(yes),
    help("warn-unneeded-purity-pred-decl", [
        w("Do not warn about predicate and function declarations that"),
        w("specify a purity level that is less pure than the predicate"),
        w("or function definition.")])).
optdb(oc_warn_dodgy_pred, warn_typecheck_ambiguity_limit, bool(yes),
    help("warn-typecheck-ambiguity-limit", [
        w("Do not generate a warning when the number of type assignments"),
        w("needed to process the definition of a predicate or function"),
        w("reaches or exceeds the typechecker's ambiguity warn limit.")])).

% Warnings about predicate pragma issues.

optdb(oc_warn_dodgy_prg, warn_ambiguous_pragma,        bool(yes),
    alt_help("warn-ambiguous-pragmas",
            ["warn-ambiguous-pragma"], [
        w("Do not warn about pragmas that do not specify whether"),
        w("they are for a predicate or a function, even when there is both"),
        w("a predicate and a function with the given name and arity.")])).
optdb(oc_warn_dodgy_prg, warn_potentially_ambiguous_pragma, bool(no),
    alt_help("warn-potentially-ambiguous-pragmas",
            ["warn-potentially-ambiguous-pragma"], [
        w("Warn about pragmas that do not specify whether they are"),
        w("for a predicate or a function.")])).
optdb(oc_warn_dodgy_prg, warn_table_with_inline,       bool(yes),
    help("warn-table-with-inline", [
        w("Do not warn about tabled procedures that also have a"),
        code("pragma inline"), w("declaration."),
        w("(This combination does not work, because"),
        w("inlined copies of procedure bodies cannot be tabled.)")])).
optdb(oc_warn_dodgy_prg, warn_unneeded_purity_pragma,  bool(yes),
    help("warn-unneeded-purity-pragma", [
        w("Do not warn about purity promise pragmas that specify"),
        w("a purity level that is less pure than the definition of"),
        w("the predicate or function that they apply to.")])).
optdb(oc_warn_dodgy_prg, warn_nonexported_pragma,      bool(yes),
    help("warn-nonexported-pragma", [
        w("Do not warn about non-exported pragmas that declare something"),
        w("about an exported predicate or function (such as an assertion"),
        w("that it terminates).")])).

% Warnings about goal-level issues.

optdb(oc_warn_dodgy_goal, warn_dodgy_simple_code,      bool(yes),
    % NOTE The external name is *much* older than the internal name.
    help("warn-simple-code", [
        w("Do not warn about constructs which are so simple"),
        w("that they are likely to be programming errors."),
        w("(One example is if-then-elses"),
        w("whose condition always succeeds.)")])).
optdb(oc_warn_dodgy_goal, warn_singleton_vars,         bool(yes),
    alt_help("warn-singleton-variables",
            ["warn-singleton-vars"], [
        w("Do not warn about variables which only occur once in a clause,"),
        w("but whose names do not start with an underscore.")])).
optdb(oc_warn_dodgy_goal, warn_repeated_singleton_vars, bool(yes),
    alt_help("warn-repeated-singleton-variables",
            ["warn-repeated-singleton-vars"], [
        w("Do not warn about variables which occur"),
        w("more than once in a clause,"),
        w("but whose names do start with an underscore.")])).
optdb(oc_warn_dodgy_goal, warn_unification_cannot_succeed, bool(yes),
    help("warn-unification-cannot-succeed", [
        w("Do not warn about unifications which cannot succeed.")])).
optdb(oc_warn_dodgy_goal, warn_known_bad_format_calls, bool(yes),
    help("warn-known-bad-format-calls", [
        w("Do not warn about calls to"), code("string.format", ","),
        code("io.format", ","), w("or"), code("stream.string_writer.format"),
        w("that contain mismatches between"),
        w("the format string and the supplied values.")])).
optdb(oc_warn_dodgy_goal, warn_obsolete,               bool(yes),
    help("warn-obsolete", [
        w("Do not warn about calls to predicates and functions"),
        w("that have been marked as obsolete.")])).
optdb(oc_warn_dodgy_goal, warn_overlapping_scopes,     bool(yes),
    help("warn-overlapping-scopes", [
        w("Do not warn about variables which occur in overlapping scopes.")])).
optdb(oc_warn_dodgy_goal, warn_suspected_occurs_check_failure, bool(yes),
    alt_help("warn-suspected-occurs-check-failure",
            ["warn-suspected-occurs-failure"], [
        w("Do not warn about code that looks like it unifies a variable"),
        w("with a term that contains that same variable. Such code cannot"),
        w("succeed because it fails the test called the"),
        emph("occurs check", ".")])).
optdb(oc_warn_dodgy_goal, warn_suspicious_recursion,   bool(no),
    help("warn-suspicious-recursion", [
        w("Warn about recursive calls which are likely to have problems,"),
        w("such as leading to infinite recursion.")])).
optdb(oc_warn_dodgy_goal, warn_unused_args,            bool(no),
    help("warn-unused-args", [
        w("Warn about predicate or function arguments which are not used.")])).
optdb(oc_warn_dodgy_goal, warn_unneeded_purity_indicator, bool(yes),
    help("warn-unneeded-purity-indicator", [
        w("Do not warn about purity indicators on goals that specify"),
        w("a purity level that is less pure than the declaration of"),
        w("the called predicate or function.")])).
optdb(oc_warn_dodgy_goal, warn_missing_state_var_init, bool(yes),
    help("warn-missing-state-var-init", [
        w("Do not print warnings about state variables that are initialized"),
        w("on some but not all paths through a disjunction"),
        w("or if-then-else.")])).
optdb(oc_warn_dodgy_goal, warn_moved_trace_goal,       bool(yes),
    help("warn-moved-trace-goal", [
        w("Do not print warning about trace goals that were moved"),
        w("after goals that follow them in the text of the program."),
        w("Such reordering may mean that the trace goal will not be"),
        w("executed if the goal moved before it fails, and even if"),
        w("it does get executed, it may be executed in a different context"),
        w("than the one expected by the programmer.")])).
optdb(oc_warn_dodgy_goal, warn_disj_fills_partial_inst, bool(yes),
    help("warn-disj-fills-partial-inst", [
        w("Do not print warnings about disjunctions that"),
        w("further instantiate some variables that enter the disjunction"),
        w("in a partially-instantiated state. While such disjunctions"),
        w("work fine in most contexts, if they constitute the body"),
        w("of a predicate or a function, that predicate or function"),
        w("will not work when passed to an all-solutions predicate."),
        w("This is because while the solutions that the different disjuncts"),
        w("in the disjunction can generate distinct values for"),
        w("the affected variables, those values will be represented"),
        w("as terms that have the exact same address, namely"),
        w("the address of the initial partially-instantiated term."),
        w("This fact will lead the all-solutions predicate to consider them"),
        w("to be the *same* solution.")])).
optdb(oc_warn_dodgy_goal, warn_unknown_warning_name, bool(yes),
    help("warn-unknown-warning-name", [
        w("Do not report unknown warning names in the list of warnings"),
        w("to disable in disable_warning(s) scopes.")])).

% Warnings about issues with insts.

optdb(oc_warn_dodgy_inst, warn_insts_without_matching_type, bool(yes),
    help("warn-insts-without-matching-type", [
        w("Do not warn about insts that are not consistent"),
        w("with any of the types in scope.")])).
optdb(oc_warn_dodgy_inst, warn_insts_with_functors_without_type, bool(no),
    help("warn-insts-with-functors-without-type", [
        w("Warn about insts that do specify functors, but do not specify"),
        w("what type they are for.")])).
optdb(oc_warn_dodgy_inst, warn_exported_inst_for_private_type, bool(yes),
    help("warn-exported-insts-for-private-type", [
        w("Do not warn about exported insts that match"),
        w("only private types.")])).

% Warnings about issues with files.

optdb(oc_warn_file, warn_undefined_options_variables,  bool(yes),
    alt_help("warn-undefined-options-variables",
            ["warn-undefined-options-vars"], [
        w("Do not warn about references to undefined variables"),
        w("in options files with"), opt("--make", ".")])).
optdb(oc_warn_file, warn_missing_descendant_modules,   bool(yes),
    help("warn-missing-descendant-modules", [
        w("Do not warn about modules which cannot be found,"),
        w("even though some of their ancestor modules"),
        w("exist in the current directory.")])).
optdb(oc_warn_file, warn_missing_opt_files,            bool(yes),
    help("warn-missing-opt-files", [
        w("Do not warn about"), file(".opt"), w("files"),
        w("which cannot be opened.")])).
optdb(oc_warn_file, warn_missing_trans_opt_files,      bool(no),
    help("warn-missing-trans-opt-files", [
        w("Warn about"), file(".trans_opt"), w("files"),
        w("which cannot be opened.")])).
optdb(oc_warn_file, warn_missing_trans_opt_deps,       bool(yes),
    help("warn-missing-trans-opt-deps", [
        w("Do not generate a warning when the information required"),
        w("to allow"), file(".trans_opt"), w("files to be read"),
        w("when creating other"), file(".trans_opt"), w("files"),
        w("has been lost. The information can be recreated by running"),
        help_text_texinfo(
            [quote("mmake <mainmodule>.depend", ".")],
            [code("mmake"), file_var("mainmodule", "depend", ".")])])).

%---------------------%

optdb(oc_warn_perf, warn_accumulator_swaps,            bool(yes),
    help("warn-accumulator-swaps", [
        w("Do not warn about argument order rearrangements done by"),
        opt("--introduce-accumulators", ".")])).
optdb(oc_warn_perf, warn_unneeded_final_statevars,     bool(yes),
    help("warn-unneeded-final-statevars", [
        w("Do not warn about"), code("!:S"),
        w("state variables in clause heads"),
        w("whose value will always be the same as"), code("!.S", ".")])).
optdb(oc_warn_perf, warn_unneeded_final_statevars_lambda, bool(yes),
    help("warn-unneeded-final-statevars-lambda", [
        w("Do not warn about"), code("!:S"),
        w("state variables in lambda expressions"),
        w("whose value will always be the same as"), code("!.S", ".")])).
optdb(oc_warn_perf, warn_no_auto_parallel,            bool(yes),
    alt_help("warn-no-auto-parallelisation",
            ["warn-no-auto-parallelization"], [
        w("Do not warn about procedures and goals that could not be"),
        w("automatically parallelised.")])).
optdb(oc_warn_perf, warn_obvious_non_tail_recursion,   bool(no),
    help("warn-obvious-non-tail-recursion", [
        w("Warn about recursive calls that are not tail calls"),
        w("even if they obviously cannot be tail calls,"),
        w("because they are followed by other recursive calls.")])).
% These are the internal options that implement
% --warn-non-tail-recursion.
optdb(oc_warn_perf, warn_non_tail_recursion_self,      bool(no),
    priv_help("warn-non-tail-recursion-self", [
        w("Warn about any self recursive calls that are not"),
        w("tail recursive.")])).
optdb(oc_warn_perf, warn_non_tail_recursion_mutual,    bool(no),
    priv_help("warn-non-tail-recursion-mutual", [
        w("Warn about any mutually recursive calls that are not"),
        w("tail recursive.")])).
optdb(oc_warn_perf_c, warn_non_tail_recursion,         maybe_string_special,
    arg_help("warn-non-tail-recursion", "{none,self,self-and-mutual}", [
        w("Specify when the compiler should warn"),
        w("about recursive calls that are not tail calls.")])).
        % Uncomment this if/when the two named options become public.
        % w("This option works by specifying the implied values for"),
        % opt("--warn-non-tail-recursion-self"), w("and"),
        % opt("--warn-non-tail-recursion-mutual", ".")])).
optdb(oc_warn_perf_c, warn_no_recursion,               bool(yes),
    help("warn-no-recursion", [
        w("Do not generate a warning when a predicate or function in which"),
        w("the programmer requests warnings about non-tail recursion"),
        w("has no recursive calls at all.")])).

%---------------------%

% Warnings about module level issues.

optdb(oc_warn_style_mod, warn_include_and_non_include,  bool(no),
    help("warn-include-and-non-include", [
        w("Warn about modules that contain both"), quote("include_module"),
        w("declarations and other kinds of entities, such as"),
        w("types or predicates."),
        blank_line,
        w("When a module contains both"), quote("include_module"),
        w("declarations"), w("and other code, changes to that code"),
        w("will often cause the recompilation"),
        w("of all the included submodules."),
        w("This is because those submodules have access to"),
        w("the entities declared in their parent module,"),
        w("so if the set of those entities changes in any way,"),
        w("the submodules must be checked to see whether they relied"),
        w("on some entity in the parent module that is not there anymore."),
        blank_line,
        w("Modules that contain"), quote("include_module"), w("declarations"),
        w("and nothing else, which are often called"), quote("packages", ","),
        w("do not have this problem.")])).

% Warnings about dead code.

optdb(oc_warn_style_pred, warn_dead_preds,             bool(no),
    alt_help("warn-dead-predicates",
            ["warn-dead-preds"], [
        w("Warn about predicates and functions that have"),
        w("no procedures which are ever called.")])).
optdb(oc_warn_style_pred, warn_dead_procs,             bool(no),
    alt_help("warn-dead-procedures",
            ["warn-dead-procs"], [
        w("Warn about procedures which are never called.")])).

% Warnings about predicate level issues.

optdb(oc_warn_style_pred, warn_can_fail_function,      bool(no),
    help("warn-can-fail-function", [
        w("Warn about functions that can fail."),
        w("(Such functions should be replaced by semidet predicates.)")])).
optdb(oc_warn_style_pred, warn_unneeded_mode_specific_clause, bool(yes),
    help("warn-unneeded-mode-specific-clause", [
        w("Do not warn about clauses that unnecessarily specify"),
        w("the modes of their arguments.")])).

% Warnings about simple style mistakes.

optdb(oc_warn_style_goal, warn_redundant_code,         bool(yes),
    help("warn-redundant-code", [
        w("Do not warn about redundant constructs in Mercury code."),
        w("(One example is importing a module more than once.)")])).
optdb(oc_warn_style_goal, warn_ite_instead_of_switch,  bool(no),
    alt_help("warn-ite-instead-of-switch", ["inform-ite-instead-of-switch"], [
        w("Generate warnings for if-then-elses that could be replaced"),
        w("by switches.")])).
optdb(oc_warn_style_goal, warn_incomplete_switch,      bool(no),
    alt_help("warn-incomplete-switch", ["inform-incomplete-switch"], [
        w("Generate warnings for switches that do not cover all the"),
        w("function symbols that the switched-on variable could be"),
        w("bound to.")])).
optdb(oc_warn_style_goal_c, warn_incomplete_switch_threshold, int(0),
    alt_arg_help("warn-incomplete-switch-threshold",
            ["inform-incomplete-switch-threshold"], "N", [
        w("Have the"), opt("--warn-incomplete-switch"), w("option"),
        w("generate its messages only for switches that"), emph("do"),
        w("cover at least"), bare_arg("N", "%"), w("of the function symbols"),
        w("that the switched-on variable could be bound to.")])).
optdb(oc_warn_style_goal, warn_duplicate_calls,        bool(no),
    help("warn-duplicate-calls", [
        w("Warn about multiple calls to a predicate or function"),
        w("with the same input arguments.")])).
optdb(oc_warn_style_goal, warn_redundant_coerce,       bool(yes),
    help("warn-redundant-coerce", [
        w("Do not warn about redundant type coercions,"),
        w("which occur when the type of the result of the"), code("coerce"),
        w("expression is the same as the type of its argument.")])).
optdb(oc_warn_style_goal, warn_requested_by_code,      bool(yes),
    help("warn-requested-by-code", [
        w("Do not generate warnings that are specifically requested"),
        w("by the code being compiled, such as"),
        code("require_switch_arms_in_type_order"), w("pragmas.")])).
optdb(oc_warn_style_goal, warn_requested_by_option,    bool(yes),
    help("warn-requested-by-option", [
        w("Do not generate warnings that are specifically requested"),
        w("by compiler options, such as"),
        quote("--enable-termination", ".")])).

% Warnings about state vars.

optdb(oc_warn_style_goal, warn_state_var_shadowing,    bool(yes),
    help("warn-state-var-shadowing", [
        w("Do not warn about one state variable shadowing another.")])).
optdb(oc_warn_style_goal, warn_unneeded_initial_statevars, bool(yes),
    help("warn-unneeded-initial-statevars", [
        w("Do not warn about state variables in clause heads"),
        w("that could be ordinary variables.")])).
optdb(oc_warn_style_goal, warn_unneeded_initial_statevars_lambda, bool(yes),
    help("warn-unneeded-initial-statevars-lambda", [
        w("Do not warn about state variables"),
        w("in the heads of lambda expressions"),
        w("that could be ordinary variables.")])).

% Warnings about I/O predicates.

optdb(oc_warn_style_goal, warn_implicit_stream_calls,  bool(no),
    help("warn-implicit-stream-calls", [
        w("Warn about calls to I/O predicates that could take"),
        w("explicit stream arguments, but do not do so.")])).
optdb(oc_warn_style_goal, warn_unknown_format_calls,   bool(no),
    help("warn-unknown-format-calls", [
        w("Warn about calls to"), code("string.format", ","),
        code("io.format"), w("or"), code("stream.string_writer.format"),
        w("for which the compiler cannot tell whether"),
        w("there are any mismatches between the format string"),
        w("and the supplied values.")])).

% Warnings about foreign code.

optdb(oc_warn_style_goal, warn_suspicious_foreign_code, bool(no),
    help("warn-suspicious-foreign-code", [
        w("Warn about possible errors in the bodies of foreign code"),
        w("pragmas."),
        blank_line,
        w("Note that since the compiler's ability to parse"),
        w("foreign language code is limited, some warnings"),
        w("reported by this option may be spurious, and"),
        w("some actual errors may not be detected at all.")])).
optdb(oc_warn_style_goal, warn_suspicious_foreign_procs, bool(no),
    help("warn-suspicious-foreign-procs", [
        w("Warn about possible errors in the bodies of foreign_proc"),
        w("pragmas."),
        w("When enabled, the compiler attempts to determine"),
        w("whether the success indicator for a foreign procedure"),
        w("is correctly set, and whether the foreign procedure body"),
        w("contains operations that it should not contain, such as"),
        code("return"), w("statements in a C foreign procedure."),
        blank_line,
        w("Note that since the compiler's ability to parse"),
        w("foreign language code is limited, some warnings"),
        w("reported by this option may be spurious, and"),
        w("some actual errors may not be detected at all.")])).

% Warnings about missing order.

optdb(oc_warn_style_order, warn_unsorted_import_blocks, bool(no),
    alt_help("warn-unsorted-import-blocks",
            ["warn-unsorted-import-block"], [
        w("Generate a warning if two"), code("import_module"), w("and/or"),
        code("use_module"), w("declarations occur on the same line,"),
        w("or if a sequence of such declarations on consecutive lines"),
        w("are not sorted on module name.")])).
optdb(oc_warn_style_order, warn_inconsistent_pred_order_clauses, bool(no),
    alt_help("warn-inconsistent-pred-order-clauses",
            ["warn-inconsistent-pred-order"], [
        w("Generate a warning if the order of the definitions does not match"),
        w("the order of the declarations for either the exported predicates"),
        w("and functions of the module, or for the nonexported predicates"),
        w("and functions of the module. Applies for definitions by"),
        w("Mercury clauses.")])).
optdb(oc_warn_style_order, warn_inconsistent_pred_order_foreign_procs,
                                                       bool(no),
    help("warn-inconsistent-pred-order-foreign-procs", [
        w("Generate a warning if the order of the definitions does not match"),
        w("the order of the declarations for either the exported predicates"),
        w("and functions of the module, or for the nonexported predicates"),
        w("and functions of the module. Applies for definitions by either"),
        w("Mercury clauses or foreign_proc pragmas.")])).

% Warnings about missing contiguity.

optdb(oc_warn_style_ctg, warn_non_contiguous_decls,     bool(yes),
    help("warn-non-contiguous-decls", [
        w("Do not generate a warning if the mode declarations of a"),
        w("predicate or function do not all immediately follow its"),
        code("pred"), w("or"), code("func"), w("declaration.")])).
optdb(oc_warn_style_ctg, warn_non_contiguous_clauses,   bool(yes),
    help("warn-non-contiguous-clauses", [
        w("Do not generate a warning if the clauses of a predicate"),
        w("or function are not contiguous.")])).
optdb(oc_warn_style_ctg, warn_non_contiguous_foreign_procs, bool(no),
    help("warn-non-contiguous-foreign-procs", [
        w("Generate a warning if the clauses and foreign_procs of a"),
        w("predicate or function are not contiguous.")])).
optdb(oc_warn_style_ctg_c, allow_non_contiguity_for,   accumulating([]),
    arg_help("allow-non-contiguity-for", "name1,name2,...", [
        w("Allow the clauses (or, with"),
        opt("--warn-non-contiguous-foreign-procs", ","),
        w("the clauses and/or foreign_proc pragmas) of the named predicates"),
        w("and/or functions to be intermingled with each other, but not"),
        w("with those or any other predicates or functions. This option"),
        w("may be specified more than once, with each option value"),
        w("specifying a distinct set of predicates and/or function names"),
        w("that may be intermingled. Each name must uniquely specify"),
        w("a predicate or a function.")])).

%---------------------%

optdb(oc_warn_ctrl, inhibit_warnings,                  bool_special,
    short_help('w', "inhibit-warnings", [], [
        w("Disable all warning messages.")])).
optdb(oc_warn_ctrl, inhibit_style_warnings,            bool_special,
    help("inhibit-style-warnings", [
        w("Disable all warning messages about programming style.")])).
optdb(oc_warn_ctrl, warn_all_format_string_errors,     bool(no),
    help("warn-all-format-string-errors", [
        w("If a format string has more than one mismatch"),
        w("with the supplied values,"),
        w("generate a warning for all mismatches, not just the first."),
        w("(The default is to report only the first, because"),
        w("later mismatches may be avalanche errors"),
        w("caused by earlier mismatches.)")])).
optdb(oc_warn_ctrl, warn_smart_recompilation,          bool(yes),
    % XXX Private until smart recompilation works and is announced.
    priv_help("warn-smart-recompilation", [
        w("Disable warnings from the smart recompilation system.")])).
optdb(oc_warn_ctrl, warn_up_to_date,                   bool(yes),
    help("warn-up-to-date", [
        w("Do not warn if targets specified on the command line with"),
        code("mmc --make"), w("are already up-to-date.")])).

%---------------------%

optdb(oc_warn_halt, halt_at_warn,                      bool(no),
    help("halt-at-warn", [
        w("This option causes the compiler to treat all warnings"),
        w("as if they were errors when intending to generate target code."),
        w("This means that if the compiler issues any warnings,"),
        w("it will not generate target code; instead, it will"),
        w("return a non-zero exit status.")])).
optdb(oc_warn_halt, halt_at_warn_make_int,             bool(no),
    alt_help("halt-at-warn-make-interface",
            ["halt-at-warn-make-int"], [
        w("This option causes the compiler to treat all warnings"),
        w("as if they were errors when intending to generate"),
        w("an interface file"),
        w("(a"), file(".int", ","), file(".int0", ","),
        file(".int2"), w("or"), file(".int3"), w("file)."),
        w("This means that if the compiler issues any warnings at that time,"),
        w("it will not generate the interface file; instead,"),
        w("it will return a non-zero exit status.")])).
optdb(oc_warn_halt, halt_at_warn_make_opt,             bool(no),
    help("halt-at-warn-make-opt", [
        w("This option causes the compiler to treat all warnings"),
        w("as if they were errors when intending to generate"),
        w("an optimization file"),
        w("(a"), file(".opt"), w("or"), file(".trans_opt"), w("file.)"),
        w("This means that if the compiler issues any warnings at that time,"),
        w("it will not generate the optimization file;"),
        w("instead, it will return a non-zero exit status.")])).
optdb(oc_warn_halt, halt_at_syntax_errors,             bool(no),
    help("halt-at-syntax-errors", [
        w("This option causes the compiler to halt immediately,"),
        w("without doing any semantic checking,"),
        w("if it finds any syntax errors in the program.")])).
optdb(oc_warn_halt, halt_at_invalid_interface,         bool(yes),
    % --halt-at-invalid-interface is a temporary developer-only option.
    help("halt-at-invalid-interface", [
        w("This option operates when the compiler is invoked with the"),
        opt("--make--interface"), w("option to generate"),
        file(".int"), w("and"), file(".int2"), w("files"),
        w("for one or more modules. In its default setting,"),
        opt("--halt-at-invalid-interface", ","),
        w("it causes the compiler to check the consistency of those parts"),
        w("of each of those modules that are intended to end up in the"),
        file(".int"), w("and"), file(".int2"), w("files."),
        w("If these checks find any problems, the compiler will print"),
        w("an error message for each problem, but will then stop."),
        blank_line,
        w("Users can prevent this stop,"),
        w("and thus allow the generation of invalid interface files,"),
        w("by specifying"), opt("--no-halt-at-invalid-interface", "."),
        w("(In this case, the problems in the invalid information files"),
        w("will be reported when compiling the modules that import them.)")])).
optdb(oc_warn_halt, halt_at_auto_parallel_failure,     bool(no),
    priv_help("halt-at-auto-parallel-failure", [
        w("This option causes the compiler to halt if it cannot perform"),
        w("an auto-parallelization requested by a feedback file.")])).

%---------------------------------------------------------------------------%

    % Options that request information.

optdb(oc_inform,    inform_inferred,                   bool_special,
    help("inform-inferred", [
        w("Do not print inferred types or modes.")])).
optdb(oc_inform,    inform_inferred_types,             bool(yes),
    help("inform-inferred-types", [
        w("Do not print inferred types.")])).
optdb(oc_inform,    inform_inferred_modes,             bool(yes),
    help("inform-inferred-modes", [
        w("Do not print inferred modes.")])).
optdb(oc_inform,    inform_incomplete_color_scheme,    bool(no),
    priv_help("inform-incomplete-color-scheme", [
        w("Report if either the value of the"), opt("--color-scheme"),
        w("option, or the value of the"), env("MERCURY_COLOR_SCHEME"),
        w("environment variable, is incomplete,"),
        w("in that it does not specify a color for some role.")])).
optdb(oc_inform,    inform_suboptimal_packing,         bool(no),
    help("inform-suboptimal-packing", [
        w("Generate messages if the arguments of a data constructor"),
        w("could be packed more tightly if they were reordered.")])).
optdb(oc_inform, show_pred_movability,              accumulating([]),
    alt_arg_help("show-pred-moveability",
            ["show-pred-movability"], "pred_or_func_name", [
        w("Write out a short report on the effect of moving the code of"),
        w("the named predicate or function (or the named several predicates"),
        w("and/or functions, if the option is given several times)"),
        w("to a new module. This includes listing the other predicates"),
        w("and/or functions that would have to be moved with them, and"),
        w("whether the move would cause unwanted coupling between"),
        w("the new module and the old.")])).

%---------------------------------------------------------------------------%

    % Options that request information in files.

optdb(oc_file_req,  show_definitions,                  bool(no),
    alt_help("show-definitions",
            ["show-defns"], [
        w("Write out a list of the types, insts, modes,"),
        w("predicates, functions, typeclasses and instances"),
        w("defined in the module to"), file_var("module", "defns", ".")])).
optdb(oc_file_req,  show_definition_line_counts,       bool(no),
    alt_help("show-definition-line-counts",
            ["show-defn-line-counts"], [
        w("Write out a list of the predicates and functions defined in"),
        w("the module, together with the names of the files containing them"),
        w("and their approximate line counts, to"),
        file_var("module", "defn_line_counts", "."),
        w("The list will be ordered on the names and arities of the"),
        w("predicates and functions."),
        blank_line,
        w("The line counts are only approximate because the compiler"),
        w("does not need, and therefore does not keep around,"),
        w("information such as the context of a line"),
        w("that contains only a close parenthesis ending a clause.")])).
optdb(oc_file_req,  show_definition_extents,           bool(no),
    alt_help("show-definition-extents",
            ["show-defn-extents"], [
        w("Write out a list of the predicates and functions defined in"),
        w("the module, together with the approximate line numbers of their"),
        w("first and last lines, to"), file_var("module", "defn_extents", "."),
        w("The list will be ordered on the starting line numbers"),
        w("of the predicates and functions."),
        blank_line,
        w("The line numbers are only approximate because the compiler"),
        w("does not need, and therefore does not keep around,"),
        w("information such as the context of a line"),
        w("that contains only a close parenthesis ending a clause.")])).
optdb(oc_file_req,  show_local_call_tree,              bool(no),
    help("show-local-call-tree", [
        w("Construct the local call tree of the predicates and functions"),
        w("defined in the module. Each node of this tree is a local"),
        w("predicate or function, and each node has edges linking it to the"),
        w("nodes of the other local predicates and functions it directly"),
        w("refers to. Write out to"), file_var("module", "local_call_tree"),
        w("a list of these nodes."),
        w("Put these nodes into the order in which they are"),
        w("encountered by a depth-first left-to-right traversal of the"),
        w("bodies (as reordered by mode analysis), of the first procedure of"),
        w("each predicate or function, starting the traversal at the"),
        w("exported predicates and/or functions of the module."),
        w("List the callees of each node in the same order."),
        blank_line,
        w("Write a flattened form of this call tree, containing just"),
        w("the predicates and functions in the same traversal order, to"),
        file_var("module", "local_call_tree_order", "."),
        blank_line,
        w("Construct another call tree of the predicates and functions"),
        w("defined in the module in which each entry lists"),
        w("not just the local predicates/functions directly referred to,"),
        w("but all directly or indirectly referenced predicates/functions,"),
        w("whether or not they are defined in the current module."),
        w("The one restriction is that we consider only references"),
        w("that occur in the body of the current module."),
        w("Write out this tree to"),
        file_var("module", "local_call_full", ".")])).
optdb(oc_file_req,  show_local_type_repns,             bool(no),
    alt_help("show-local-type-representations",
            ["show-local-type-repns"], [
        w("Write out information about the representations of all types"),
        w("defined in the module being compiled to"),
        file_var("module", "type_repns", ".")])).
optdb(oc_file_req,  show_all_type_repns,               bool(no),
    alt_help("show-all-type-representations",
            ["show-all-type-repns"], [
        w("Write out information about the representations of all types"),
        w("visible in the module being compiled to"),
        file_var("module", "type_repns", ".")])).
optdb(oc_file_req,  show_developer_type_repns,         bool(no),
    priv_alt_help("show-developer-type-representations",
            ["show-developer-type-repns"], [
        w("When writing out information about the representations of types,"),
        w("include information that is of interest"),
        w("to Mercury developers only.")])).
optdb(oc_file_req,  show_dependency_graph,             bool(no),
    help("show-dependency-graph", [
        % XXX This help text is next to useless.
        w("Write out the dependency graph to"),
        file_var("module", "dependency_graph", ".")])).
optdb(oc_file_req,  show_imports_graph,                bool(no),
    alt_help("show-imports-graph", ["imports-graph"], [
        w("If"), opt("--generate-dependencies"), w("is specified, then"),
        w("write out the imports graph to"),
        file_var("module", "imports_graph"), w("in a format"),
        w("that can be processed by the"), code("graphviz"), w("tools."),
        w("The graph will contain an edge from the node of module A"),
        w("to the node of module B if module A imports module B.")])).

%---------------------------------------------------------------------------%

    % Options that control whether some compiler reports are printed.

optdb(oc_report,    report_noop,                       bool(yes), no_help).
    % We use severity_informational(report_noop) error_specs when we
    % - do NOT want to print anything or affect the process exit status,
    % - but we do want the presence of an error_spec to signal that
    %   something is wrong.
    % This is not an ideal solution, but sometimes it is the best we can do.
optdb(oc_report,    report_not_written,                bool(yes),
    % This option exists only to satisfy the requirement that every
    % severity_informational message specify an option. But actually
    % turning off this option is *very* unlikely to be a good idea.
    priv_help("report-not-written", [
        w("Do not print a message when some files cannot be written out"),
        w("due to previous errors.")])).
optdb(oc_report,    report_recompile_reason,           bool(yes),
    % This is private because smart recompilation is not yet ready
    % for prime time. It nevertheless default to "yes" for the sake
    % of tests/recompilation.
    priv_help("report-recompile-reason", [
        w("Do not print the reason why smart recompilation"),
        w("recompiles a module.")])).
optdb(oc_report,    report_used_file_error,            bool(yes),
    % This is private because smart recompilation is not yet ready
    % for prime time. It nevertheless default to "yes" for the sake
    % of tests/recompilation.
    % XXX The diagnostics that use this option are severity_informational.
    % To me (zs) they look like they should be severity_error, but switching
    % to that causes lots of failures in tests/recompilation.
    priv_help("report-used-file-error", [
        w("Do not print messages about problems with .used files.")])).

%---------------------------------------------------------------------------%

    % Options that control trace goals.

optdb(oc_tracegoal, trace_goal_flags,                  accumulating([]),
    arg_help("trace-flag", "keyword", [
        w("Enable the trace goals that depend on the"),
        arg("keyword"), w("trace flag.")])).

%---------------------------------------------------------------------------%

    % Options that control how the compiler prepares for mdb debugging.

optdb(oc_mdb,       trace_level,                       string("default"),
    % "--trace decl" is not documented, because it is for backwards
    % compatibility only. It is now equivalent to `--trace rep'.
    arg_help("trace", "{minimum, shallow, deep, rep, default}", [
        w("Generate code that includes the specified level"),
        w("of execution tracing."),
        help_text_only([
            w("See the Debugging chapter of the Mercury User's Guide"),
            w("for details.")]),
        texinfo_only([xref("Debugging", ".")])])).
optdb(oc_mdb,       exec_trace_tail_rec,               bool(no),
    help("exec-trace-tail-rec", [
        w("Generate TAIL events for self-tail-recursive calls instead of"),
        w("EXIT events. This allows these recursive calls to reuse"),
        w("their parent call's stack frame, but it also means that"),
        w("the debugger won't have access to the contents of the reused"),
        w("stack frames.")])).
optdb(oc_mdb,       trace_optimized,                   bool(no),
    alt_help("trace-optimized",
            ["trace-optimised"], [
        w("Do not disable optimizations that can change the trace.")])).
optdb(oc_mdb_dev,   trace_prof,                        bool(no),
    % "--trace-prof" is not documented because it is intended
    % only for developers of the deep profiler.
    priv_help("trace-prof", [
        w("Enable tracing of deep profiling service predicates.")])).
optdb(oc_mdb,       event_set_file_name,               string(""),
    arg_help("event-set-file-name", "filename", [
        w("Get the specification of user-defined events from"),
        arg("filename", ".")])).
optdb(oc_mdb_dev,   trace_table_io,                    bool(no),
    % I/O tabling is deliberately not documented. It is meant to be
    % switched on, with consistent parameters, in debugging grades,
    % and to be consistently switched off in non-debugging grades.
    % Inconsistent use of the options governing I/O tabling
    % can yield core dumps from the debugger, so these options
    % are for implementors only.
    priv_help("trace-table-io", [
        w("Enable the tabling of I/O actions, to allow the debugger"),
        w("to execute retry commands across I/O actions.")])).
optdb(oc_mdb_dev,   trace_table_io_only_retry,         bool(no),
    priv_help("trace-table-io-only-retry", [
        w("Set up I/O tabling to support only retries across I/O actions,"),
        w("not the printing of actions or declarative debugging."),
        w("This reduces the size of the I/O action table.")])).
optdb(oc_mdb_dev,   trace_table_io_states,             bool(no),
    priv_help("trace-table-io-states", [
        w("When tabling I/O actions, table the"),
        code("io.state"), w("arguments together with the others."),
        w("This should be required iff values of type"), code("io.state"),
        w("actually contain information.")])).
optdb(oc_mdb_dev,   trace_table_io_require,            bool(no),
    priv_help("trace-table-io-require", [
        w("Require the tabling of I/O actions, i.e. generate an error"),
        w("if an I/O primitive does not have the"), code("tabled_for_io"),
        w("annotation.")])).
optdb(oc_mdb_dev,   trace_table_io_all,                bool(no),
    priv_help("trace-table-io-all", [
        w("Table all I/O actions even in the absence of annotations."),
        w("If a primitive has no annotation specifying the type of"),
        w("tabling required, deduce it from the values of the other"),
        w("annotations.")])).
optdb(oc_mdb_dev,   suppress_trace,                    string(""),
    % Force no tracing, even in .debug grades. This is used to turn off
    % tracing in the browser directory while still allowing the browser
    % library to be linked in with an executable compiled in a .debug grade.
    priv_arg_help("suppress-trace", "suppress-items,", [
        w("Suppress the named aspects of the execution tracing system.")])).
optdb(oc_mdb,       delay_death,                       bool(yes),
    help("delay-death", [
        w("When the trace level is"), samp("deep", ","),
        w("the compiler normally preserves the values of variables"),
        w("as long as possible, even beyond the point of their last use,"),
        w("in order to make them accessible from as many debugger events"),
        w("as possible. However, it will not do so if the user specifies"),
        opt("--no-delay-death", "."),
        w("This may be necessary if without it,"),
        w("the stack frames of some procedures grow too big.")])).
optdb(oc_mdb,       delay_death_max_vars,              int(1000),
    arg_help("delay-death-max-vars", "N", [
        w("Delay the deaths of variables only when the number of variables"),
        w("in the procedure is no more than"), bare_arg("N", "."),
        w("The default value is 1000.")])).
optdb(oc_mdb,       stack_trace_higher_order,          bool(no),
    help("stack-trace-higher-order", [
        w("Enable stack traces through predicates and functions with"),
        w("higher-order arguments, even if stack tracing is not"),
        w("supported in general.")])).
optdb(oc_mdb_dev,   force_disable_tracing,             bool(no),
    priv_help("force-disable-tracing", [
        w("Force tracing to be set to trace level none."),
        w("This overrides all other tracing/grade options."),
        w("Its main use is to turn off tracing in the browser directory,"),
        w("even for"), samp(".debug"), w("and"), samp(".decldebug"),
        w("grades.")])).

optdb(oc_ssdb,      ssdb_trace_level,                  string("default"),
    priv_arg_help("ssdb-trace", "{none, shallow, deep}", [
        w("The trace level to use for source to source debugging of"),
        w("the given module.")])).
optdb(oc_ssdb_dev,   force_disable_ssdebug,             bool(no),
    % This is a developer-only option:
    priv_help("force-disable-ssdebug", [
        w("Disable the ssdebug transformation even in"),
        samp(".ssdebug"), w("grades.")])).

%---------------------------------------------------------------------------%

    % Options that control profiling.

optdb(oc_mdprof,    prof_optimized,                    bool(no),
    alt_help("profile-optimized",
            ["profile-optimised"], [
        w("Do not disable optimizations that can distort deep profiles.")])).

%---------------------------------------------------------------------------%

    % Optimization options
    % IMPORTANT: the default here should be all optimizations OFF.
    % Optimizations should be enabled by the appropriate optimization level
    % in the opt_level table.

%---------------------%

    % Options that control the optimization process.

optdb(oc_opt_ctrl,  default_opt_level,                 string("-O2"),
    % This is for use by Mercury.config only.
    priv_arg_help("default-opt-level", "-O<n>", [
        w("Set the default optimization level to"), arg("N", ".")])).
optdb(oc_opt_ctrl,  opt_level,                         int_special,
    short_arg_help('O', "optimization-level",
            ["optimisation-level", "opt-level"], "N", [
        cindex("Optimization levels"),
        cindex("Compilation speed"),
        cindex("Intermodule optimization"),
        cindex("Cross-mrmodule optimization"),
        w("Set optimization level to"), arg("N", "."),
        w("Optimization level -1 means no optimization"),
        w("while optimization level 6 means full optimization."),
        w("The option"), opt("--output-optimization-options"),
        w("lists the optimizations enabled at each level."),
        blank_line,
        w("Note that some options are not enabled automatically at"),
        emph("any"), w("optimization level."),
        w("These include options that are"),
        w("too new and experimental for large scale use,"),
        w("and options that generate speedups in some use cases,"),
        w("but slowdowns in others, require situation-specific"),
        w("consideration of their use."),
        blank_line,
        w("Likewise, if you want the compiler to perform"),
        w("intermodule optimizations, where the compiler"),
        w("exploits information about the non-public parts"),
        w("of the modules it imports"),
        w("(which it gets from their"), file(".opt"), w("files)"),
        w("for optimization purposes"),
        w("then you must enable that separately,"),
        w("partially because they affect the compilation process"),
        w("in ways that require special treatment by"), code("mmake", "."),
        w("This goes double for"), emph("transitive"),
        w("intermodule optimizations, where the compiler"),
        w("exploits information about the non-public parts"),
        w("of not just the modules it imports, but also from"),
        w("the modules that"), emph("they"), w("import,"),
        w("directly or indirectly."),
        w("(It gets this info from the"), file(".trans_opt"), w("files"),
        w("of the directly or indirectly imported modules.)")])).
%       blank_line,
%       w("In general, there is a tradeoff between compilation speed"),
%       w("and the speed of the generated code."),
optdb(oc_opt_ctrl,  opt_space,                         special,
    alt_help("optimize-space",
            ["optimise-space", "opt-space"], [
        w("Turn on optimizations that reduce code size,"),
        w("and turn off optimizations that significantly"),
        w("increase code size.")])).

%---------------------%

    % HLDS -> HLDS

optdb(oc_opt_hh,    optopt_opt_dead_procs,             bool_special,
    alt_help("optimize-dead-procs",
            ["optimise-dead-procs"], [
        w("Delete all procedures that are never called.")])).
optdb(oc_opt_hh,    optopt_opt_unneeded_code,          bool_special,
    help("unneeded-code", [
        w("Remove goals from computation paths where their outputs are"),
        w("not needed, provided the semantics options allow the deletion"),
        w("or movement of the goal.")])).
optdb(oc_opt_hh,    optopt_opt_unneeded_code_copy_limit, int_special,
    arg_help("unneeded-code-copy-limit", "copy_limit", [
        w("Specify the maximum number of places"),
        w("to which a goal may be copied"),
        w("when removing it from computation paths on which its outputs are"),
        w("not needed. A value of zero forbids goal movement and allows"),
        w("only goal deletion; a value of one prevents any increase in the"),
        w("size of the code.")])).

optdb(oc_opt_hh,    optopt_opt_unused_args,            bool_special,
    alt_help("optimize-unused-args",
            ["optimise-unused-args"], [
        w("Delete unused arguments from predicates and functions."),
        w("This will cause the compiler to generate more efficient code"),
        w("for many polymorphic predicates.")])).
optdb(oc_opt_hh,    optopt_opt_unused_args_intermod,   bool_special,
    help("intermod-unused-args", [
        w("Delete unused arguments from predicates and functions"),
        w("even when the analysis required crosses module boundaries."),
        w("This option implies"), opt("--optimize-unused-args"), w("and"),
        opt("--intermodule-optimization", ".")])).

optdb(oc_opt_hh,    optopt_opt_format_calls,           bool_special,
    help("optimize-format-calls", [
        w("Do not optimize calls to"),
        code("string.format", ","), code("io.format", ","), w("and"),
        code("stream.string_writer.format"), w("at compile time."),
        w("The default is to interpret the format string in such calls"),
        w("at compile time, replacing those calls with"),
        w("the sequence of more primitive operations"),
        w("required to implement them.")])).
optdb(oc_opt_hh,    optopt_prop_constants,             bool_special,
    alt_help("optimize-constant-propagation",
            ["optimise-constant-propagation"], [
        w("Given calls to some frequently used library functions and"),
        w("predicates, mainly those that do arithmetic, evaluate them"),
        w("at compile time, if all their input arguments are constants.")])).
optdb(oc_opt_hh,    optopt_opt_dup_calls,              bool_special,
    alt_help("optimize-duplicate-calls",
            ["optimise-duplicate-calls"], [
        w("Given multiple calls to a predicate or function"),
        w("with the same input arguments, optimize away all but one.")])).
optdb(oc_opt_hh,    optopt_elim_excess_assigns,        bool_special,
    priv_help("excess-assign", [
        w("Remove assignment unifications whose only effect"),
        w("is to give another name to the same value.")])).

optdb(oc_opt_hh,    optopt_allow_inlining,             bool_special,
    priv_help("allow-inlining", [
        w("Disallow all forms of inlining.")])).
optdb(oc_opt_hh,    inlining,                          bool_special,
    help("inlining", [
        w("Ask the compiler to inline procedures"),
        w("using its usual heuristics.")])).
optdb(oc_opt_hh,    optopt_inline_builtins,            bool_special,
    priv_help("inline-builtins", [
        w("Normally, the compiler implements builtin operations"),
        w("(such as arithmetic) using inline code. With"),
        opt("--no-inline-builtins", ","), w("the compiler will"),
        w("implement them as calls to out-of-line procedures."),
        w("This latter is done by default when debugging,"),
        w("since this allows the execution of builtins to be traced.")])).
optdb(oc_opt_hh,    optopt_inline_par_builtins,        bool_special,
    % This is for measurements by implementors only.
    priv_help("inline-par-builtins", [
        w("With"), opt("--inline-par-builtins", ","), w("the compiler"),
        w("implements the parallel builtin operations using inline code."),
        w("With"), opt("--no-inline-par-builtins", ","),
        w("it implements them as calls.")])).
optdb(oc_opt_hh,    optopt_inline_single_use,          bool_special,
    help("inline-single-use", [
        w("Inline procedures which are called only from one call site.")])).
optdb(oc_opt_hh,    optopt_inline_simple,              bool_special,
    help("inline-simple", [
        w("Inline all simple procedures.")])).
optdb(oc_opt_hh,    optopt_inline_simple_threshold,    int_special,
    % XXX Define "size".
    arg_help("inline-simple-threshold", "threshold", [
        w("With"), opt("--inline-simple", ","), w("inline a procedure"),
        w("if its size is less than the given threshold.")])).
optdb(oc_opt_hh,    optopt_intermod_inline_simple_threshold, int_special,
    arg_help("intermod-inline-simple-threshold", "threshold", [
        w("Similar to"), opt("--inline-simple-threshold", ","),
        w("except used to determine which predicates should be included in"),
        file(".opt"), w("files. Note that changing this between writing the"),
        file(".opt"), w("file and compiling to C may cause link errors,"),
        w("and too high a value may result in reduced performance.")])).
optdb(oc_opt_hh,    optopt_inline_compound_threshold,  int_special,
    arg_help("inline-compound-threshold", "threshold", [
        w("Inline a procedure if its size (measured roughly"),
        w("in terms of the number of connectives in its internal form)"),
        w("less the assumed call cost, multiplied by the number of times"),
        w("it is called, is below the given threshold.")])).
optdb(oc_opt_hh,    optopt_inline_call_cost,           int_special,
    arg_help("inline-call-cost", "cost", [
        w("Assume that the cost of a call is the given parameter."),
        w("Used only in conjunction with"),
        opt("--inline-compound-threshold", ".")])).
optdb(oc_opt_hh,    optopt_inline_vars_threshold,      int_special,
    % Has no effect until --intermodule-optimization.
    % XXX Is this true? If so, Why?
    arg_help("inline-vars-threshold", "threshold", [
        w("Don't inline a call if it would result in a procedure"),
        w("containing more than"), arg("threshold"), w("variables."),
        w("Procedures containing large numbers of variables"),
        w("can cause slow compilation.")])).

optdb(oc_opt_hh,    optopt_opt_higher_order,           bool_special,
    alt_help("optimize-higher-order",
            ["optimise-higher-order"], [
        cindex("Higher-order specialization"),
        cindex("Specialization of higher-order calls"),
        w("Create specialized variants of"),
        w("higher-order predicates and functions"),
        w("for call sites where the values of the higher-order arguments"),
        w("are known.")])).
optdb(oc_opt_hh,    optopt_spec_types,                 bool_special,
    alt_help("type-specialization",
            ["type-specialisation"], [
        w("Enable specialization of polymorphic predicates where the"),
        w("polymorphic types are known.")])).
optdb(oc_opt_hh,    optopt_spec_types_user_guided,     bool_special,
    alt_help("user-guided-type-specialization",
            ["user-guided-type-specialisation"], [
        w("Enable specialization of polymorphic predicates for which"),
        w("there are"), code(":- pragma type_spec"), w("declarations."),
        texinfo_only(
            [w("See the"), quote("Type specialization"), w("section"),
            w("of the"), quote("Pragmas"), w("chapter of the"),
            w("Mercury language Reference Manual for more details.")])])).
optdb(oc_opt_hh,    optopt_higher_order_size_limit,    int_special,
    arg_help("higher-order-size-limit", "max_size", [
        w("Set the maximum goal size of specialized versions created by"),
        opt("--optimize-higher-order"), w("and"),
        opt("--type-specialization", "."),
        w("Goal size is measured as the number of calls, unifications"),
        w("and branched goals.")])).
optdb(oc_opt_hh,    optopt_higher_order_arg_limit,     int_special,
    arg_help("higher-order-arg-limit", "max_size", [
        w("Set the maximum size of higher-order arguments to"),
        w("be specialized by"), opt("--optimize-higher-order"), w("and"),
        opt("--type-specialization", ".")])).

optdb(oc_opt_hh,    optopt_opt_loop_invariants,        bool_special,
    help("loop-invariants", [
        w("Hoist loop invariant computations out of loops.")])).
optdb(oc_opt_hh,    optopt_introduce_accumulators,     bool_special,
    help("introduce-accumulators", [
        w("Attempt to make procedures tail recursive"),
        w("by introducing accumulator variables into them.")])).
optdb(oc_opt_hh,    optopt_opt_lcmc,                   bool_special,
    alt_help("optimize-constructor-last-call",
            ["optimise-constructor-last-call"], [
        w("Enable the optimization of almost-last calls that are followed"),
        w("only by constructor application.")])).
optdb(oc_opt_hh,    optopt_opt_lcmc_accumulator,       bool_special,
    priv_alt_help("optimize-constructor-last-call-accumulator",
            ["optimise-constructor-last-call-accumulator"], [
        w("Enable the optimization via accumulators of almost-last calls"),
        w("that are followed only by constructor application.")])).
optdb(oc_opt_hh,    optopt_opt_lcmc_null,              bool_special,
    priv_alt_help("optimize-constructor-last-call-null",
            ["optimise-constructor-last-call-null"], [
        w("When"), opt("--optimize-constructor-last-call"), w("is enabled,"),
        w("put NULL in uninitialized fields"),
        w("to prevent the garbage collector from"),
        w("looking at and following a random bit pattern.")])).

optdb(oc_opt_hh,    optopt_split_switch_arms,          bool_special,
    help("split-switch-arms", [
        w("When a switch on a variable has an inner switch on that"),
        w("same variable inside one of its arms, the default is"),
        w("to split up that arm of the outer switch along the same lines,"),
        w("effectively inlining the inner switch."),
        opt("--no-split-switch-arms"), w("prevents this split.")])).
optdb(oc_opt_hh,    optopt_merge_code_after_switch,    bool_special,
    priv_help("merge-code-after-switch", [
        w("Merge the goal after a switch into the switch, if we can."),
        w("Two cases in which we can are when that goal just tests"),
        w("the value of a variable set in the switch, and when that goal"),
        w("is a switch on the same variable.")])).

optdb(oc_opt_hh,    optopt_from_ground_term_threshold, int_special,
    priv_arg_help("from-ground-term-threshold", "N", [
        w("Wrap a from_ground_term scope around the expanded,"),
        w("superhomogeneous form of a ground term that involves at least."),
        w("the given number of function symbols.")])).
optdb(oc_opt_hh,    optopt_enable_const_struct_user,   bool_special,
    help("const-struct", [
        w("By default, the compiler will gather constant ground structures"),
        w("in a separate table, with each such structure being stored"),
        w("in this table just once, even if it occurs"),
        w("in many different procedures."),
        opt("--no-const-struct"), w("prevents this behavior.")])).
optdb(oc_opt_hh,    optopt_opt_common_structs,         bool_special,
    help("common-struct", [
        w("Replace two or more occurrences of the same term"),
        w("in a conjunction with just one copy.")])).

optdb(oc_opt_hh,    optimize_saved_vars,               bool_special,
    alt_help("optimize-saved-vars",
            ["optimise-saved-vars"], [
        w("Minimize the number of variables saved across calls.")])).
optdb(oc_opt_hh,    optopt_opt_saved_vars_const,       bool_special,
    priv_alt_help("optimize-saved-vars-const",
            ["optimise-saved-vars-const"], [
        w("Minimize the number of variables saved across calls by"),
        w("introducing duplicate copies of variables bound to constants"),
        w("in each interval between flushes where they are needed.")])).
optdb(oc_opt_hh,    optopt_opt_svcell,                 bool_special,
    priv_alt_help("optimize-saved-vars-cell",
            ["optimise-saved-vars-cell"], [
        w("Minimize the number of variables saved across calls by"),
        w("trying to use saved variables pointing to cells to reach"),
        w("the variables stored in those cells.")])).
optdb(oc_opt_hh,    optopt_opt_svcell_loop,            bool_special,
    priv_help("osv-loop", [])).
optdb(oc_opt_hh,    optopt_opt_svcell_full_path,       bool_special,
    priv_help("osv-full-path", [])).
optdb(oc_opt_hh,    optopt_opt_svcell_on_stack,        bool_special,
    priv_help("osv-on-stack", [])).
optdb(oc_opt_hh,    optopt_opt_svcell_candidate_headvars, bool_special,
    priv_help("osv-cand-head", [])).
% The next four options are used by tupling.m as well; changes to them
% may require changes there as well.
optdb(oc_opt_hh,    optopt_opt_svcell_cv_store_cost,   int_special,
    priv_arg_help("osv-cvstore-cost", "cost", [])).
optdb(oc_opt_hh,    optopt_opt_svcell_cv_load_cost,    int_special,
    priv_arg_help("osv-cvload-cost", "cost", [])).
optdb(oc_opt_hh,    optopt_opt_svcell_fv_store_cost,   int_special,
    priv_arg_help("osv-fvstore-cost", "cost", [])).
optdb(oc_opt_hh,    optopt_opt_svcell_fv_load_cost,    int_special,
    priv_arg_help("osv-fvload-cost", "cost", [])).
optdb(oc_opt_hh,    optopt_opt_svcell_op_ratio,        int_special,
    priv_arg_help("osv-op-ratio", "percentage", [])).
optdb(oc_opt_hh,    optopt_opt_svcell_node_ratio,      int_special,
    priv_arg_help("osv-node-ratio", "percentage", [])).
optdb(oc_opt_hh,    optopt_opt_svcell_all_path_node_ratio, int_special,
    priv_arg_help("osv-allpath-node-ratio", "percentage", [])).
optdb(oc_opt_hh,    optopt_opt_svcell_all_candidates,  bool_special,
    priv_help("osv-all-cand", [])).

optdb(oc_opt_hh,    optopt_prop_constraints,           bool_special,
    help("constraint-propagation", [
        w("Perform the constraint propagation transformation,"),
        w("which attempts to ensure that"),
        w("goals which can fail are executed as early as possible.")])).
optdb(oc_opt_hh,    optopt_prop_local_constraints,     bool_special,
    help("local-constraint-propagation", [
        w("Perform the constraint propagation transformation,"),
        w("but only rearrange goals within each procedure."),
        w("Specialized versions of procedures will not be created.")])).

optdb(oc_opt_hh,    optopt_deforest,                   bool_special,
    help("deforestation", [
        w("Perform deforestation, which is a program transformation"),
        w("whose aims are to avoid the construction of"),
        w("intermediate data structures, and to avoid repeated traversals"),
        w("over data structures within a conjunction.")])).
optdb(oc_opt_hh,    optopt_deforestation_depth_limit,  int_special,
    % XXX Document what can result in infinite loops.
    arg_help("deforestation-depth-limit", "depth_limit", [
        w("Specify a depth limit to prevent infinite loops in the"),
        w("deforestation algorithm."),
        w("A value of -1 specifies no depth limit. The default is 4.")])).
optdb(oc_opt_hh,    optopt_deforestation_cost_factor,  int_special,
    priv_arg_help("deforestation-cost-factor", "fudge-factor", [])).
optdb(oc_opt_hh,    optopt_deforestation_vars_threshold, int_special,
    arg_help("deforestation-vars-threshold", "threshold", [
        w("Specify a rough limit on the number of variables"),
        w("in a procedure created by deforestation."),
        w("A value of -1 specifies no limit. The default is 200.")])).
optdb(oc_opt_hh,    optopt_deforestation_size_threshold, int_special,
    arg_help("deforestation-size-threshold", "threshold", [
        w("Specify a rough limit on the size of a goal"),
        w("to be optimized by deforestation."),
        w("A value of -1 specifies no limit. The default is 15.")])).

optdb(oc_opt_hh,    optopt_untuple,                    bool_special,
    priv_help("untuple", [
        w("Expand out procedure arguments when the argument type"),
        w("is either a tuple, or a type with exactly one functor."),
        w("This increases the cost of parameter passing,"),
        w("but reduces the cost of access to the individual fields"),
        w("of the tuple or functor."),
        w("Which effect is greater will depend greatly"),
        w("on the relative frequency with which"),
        w("execution takes different program paths,"),
        w("which means that this transformation can cause"),
        w("slowdowns as well as speedups."),
        w("This means that detailed profiling data"),
        w("is crucial for the proper use of this option.")])).
        % w("Note: this transformation"),
        % w("is almost always a pessimization on its own, but"),
        % w("it can help other optimizations reach an overall speedup".)])).
optdb(oc_opt_hh,    optopt_tuple,                      bool_special,
    priv_help("tuple", [
        w("Try to find opportunities for procedures to pass some arguments"),
        w("to each other as a tuple rather than separately."),
        w("This reduces the cost of parameter passing,"),
        w("but increases the cost of access to each of the old arguments."),
        % w("Note: so far this has mostly a detrimental effect.")])).
        w("Which effect is greater will depend greatly"),
        w("on the relative frequency with which"),
        w("execution takes different program paths,"),
        w("which means that this transformation can cause"),
        w("slowdowns as well as speedups."),
        w("This means that detailed profiling data"),
        w("is crucial for the proper use of this option.")])).
optdb(oc_opt_hh,    optopt_tuple_trace_counts_file,    string_special,
    priv_arg_help("tuple-trace-counts-file", "filename", [
        w("This option tells the compiler that the profiling data"),
        w("needed for the untupling and tupling transformations"),
        w("is available in the named trace counts summary file."),
        w("The summary should be made from a sample run of the program"),
        w("you are compiling, compiled without optimizations.")])).
optdb(oc_opt_hh,    optopt_tuple_costs_ratio,          int_special,
    priv_arg_help("tuple-costs-ratio", "ratio", [
        w("A value of 110 for this parameter means the tupling"),
        w("transformation will transform a procedure if it thinks"),
        w("that procedure would be 10% worse, on average, than"),
        w("whatever transformed version of the procedure it has in mind."),
        w("The default is 100.")])).
optdb(oc_opt_hh,    optopt_tuple_min_args,             int_special,
    priv_arg_help("tuple-min-args", "min-num-args", [
        w("The minimum number of input arguments that the tupling"),
        w("transformation will consider passing together as a tuple."),
        w("This is mostly to speed up the compilation process"),
        w("by not pursuing (presumably) unfruitful searches.")])).

optdb(oc_opt_hh,    optopt_delay_constructs,           bool_special,
    alt_help("delay-constructs",
            ["delay-construct"], [
        w("Reorder goals to move construction unifications"),
        w("after primitive goals that can fail.")])).

optdb(oc_opt_hh,    optopt_opt_follow_code,            bool_special,
    priv_help("follow-code", [
        w("Move the code following a branched goal"),
        w("(if-then-else, disjunction, or switch)"),
        w("until the next call into each branch of that goal."),
        w("Having a call as the goal just after the branched goal"),
        w("gives the LLDS code generator a consistent set of places"),
        w("into which each branch should store live variables.")])).
optdb(oc_opt_hh,    optopt_spec_in_all_dep_par_conjs,  bool_special,
    % This is for measurements by implementors only.
    priv_help("always-specialize-in-dep-par-conjs", [
        w("When the transformation for handling dependent parallel"),
        w("conjunctions adds waits and/or signals around a call,"),
        w("create a specialized version of the called procedure,"),
        w("even if this is not profitable.")])).
optdb(oc_opt_hh,    optopt_allow_some_paths_only_waits, bool_special,
    priv_help("allow-some-paths-only-waits", [])).
optdb(oc_opt_hh,    optopt_analyse_regions,             bool_special,
    % This option is not documented because it is still experimental.
    priv_help("region-analysis", [
        w("Enable the analysis for region-based memory management.")])).

% XXX All the help text for these experimental options should be private.
optdb(oc_opt_hh_exp, structure_sharing_analysis,        bool(no),
    priv_help("structure-sharing", [
        w("Perform structure sharing analysis.")])).
optdb(oc_opt_hh_exp, structure_sharing_widening,        int(0),
    priv_arg_help("structure-sharing-widening", "N", [
        w("Perform widening when the set of structure sharing pairs"),
        w("becomes larger than"), arg("N", "."),
        w("When n=0, widening is not enabled."),
        w("(default: 0).")])).
optdb(oc_opt_hh_exp, structure_reuse_analysis,          bool(no),
    priv_alt_help("structure-reuse", ["ctgc"], [
        w("Perform structure reuse analysis"),
        w("(Compile Time Garbage Collection).")])).
optdb(oc_opt_hh_exp, structure_reuse_constraint,
                                        string("within_n_cells_difference"),
    priv_alt_arg_help("structure-reuse-constraint",
            ["ctgc-constraint"],
            "{same_cons_id, within_n_cells_difference}", [
        w("Constraint on the way we allow structure reuse."),
        samp("same_cons_id"), w("specifies that reuse is only allowed"),
        w("between terms of the same type and constructor."),
        samp("within_n_cells_difference"), w("states that reuse"),
        w("is allowed as long as the arities between the reused term"),
        w("and new term does not exceed a certain threshold."),
        w("The threshold needs to be set using"),
        opt("--structure-reuse-constraint-arg", "."),
        w("(The default is"), samp("within_n_cells_difference"),
        w("with the threshold being 0.)")])).
optdb(oc_opt_hh_exp, structure_reuse_constraint_arg,    int(0),
    priv_alt_arg_help("structure-reuse-constraint-arg",
            ["ctgc-constraint-arg"], "max_difference", [
        w("Specify the maximum difference in arities between"),
        w("the terms whose memory cells can be reused on the one hand, and"),
        w("the terms that need memory cells on the other hand."),
        w("(default: 0)")])).
optdb(oc_opt_hh_exp, structure_reuse_max_conditions,    int(10),
    priv_arg_help("structure-reuse-max-conditions", "max_num_conditions", [
        w("Soft limit on the number of reuse conditions to accumulate"),
        w("for a procedure. (default: 10)")])).
optdb(oc_opt_hh_exp, structure_reuse_repeat,            int(0),
    priv_arg_help("structure-reuse-repeat", "num_repeats", [])).
optdb(oc_opt_hh_exp, structure_reuse_free_cells,        bool(no),
    % This option is likely to break many optimisations
    % which haven't been updated.
    priv_help("structure-reuse-free-cells", [
        w("Immediately free cells which are known to be dead"),
        w("but which cannot be reused.")])).

%---------------------%

    % HLDS -> {LLDS,MLDS}

optdb(oc_opt_hlm,   optopt_use_smart_indexing,         bool_special,
    help("smart-indexing", [
        w("Implement switches using the fastest applicable"),
        w("implementation method, which may be e.g. binary search"),
        w("or a hash table. With"), opt("--no-smart-indexing", ","),
        w("the default is to implement switches"),
        w("as simple if-then-else chains.")])).
% The following options are for developers only --they provide
% finer grained control over smart indexing.
optdb(oc_opt_hlm,   optopt_use_smart_indexing_atomic,  bool_special,
    priv_help("smart-atomic-indexing", [
        w("Generate smart switches on atomic types when appropriate.")])).
optdb(oc_opt_hlm,   optopt_use_smart_indexing_string,  bool_special,
    priv_help("smart-string-indexing", [
        w("Generate smart switches on strings when appropriate.")])).
optdb(oc_opt_hlm,   optopt_use_smart_indexing_tag,     bool_special,
    priv_help("smart-tag-indexing", [
        w("Generate smart switches on discriminated union types"),
        w("when appropriate.")])).
optdb(oc_opt_hlm,   optopt_use_smart_indexing_float,   bool_special,
    priv_help("smart-float-indexing", [
        w("Generate smart switches on floats when appropriate.")])).
optdb(oc_opt_hlm,   optopt_dense_switch_req_density,   int_special,
    arg_help("dense-switch-req-density", "percentage", [
        w("The jump table generated for an atomic switch"),
        w("must have at least this percentage of full slots"),
        w("(default: 25).")])).
optdb(oc_opt_hlm,   optopt_lookup_switch_req_density,  int_special,
    arg_help("lookup-switch-req-density", "percentage", [
        w("The jump table generated for an atomic switch"),
        w("in which all the outputs are constant terms"),
        w("must have at least this percentage of full slots"),
        w("(default: 25).")])).
optdb(oc_opt_hlm,   optopt_dense_switch_size,          int_special,
    arg_help("dense-switch-size", "N", [
        w("The jump table generated for an atomic switch"),
        w("must have at least this many entries (default: 4).")])).
optdb(oc_opt_hlm,   optopt_lookup_switch_size,         int_special,
    arg_help("lookup-switch-size", "N", [
        w("The lookup table generated for an atomic switch"),
        w("must have at least this many entries (default: 4).")])).
optdb(oc_opt_hlm,   optopt_string_trie_switch_size,    int_special,
    alt_arg_help("string-trie-switch-size",
            ["string-trie-size"], "N", [
        w("The trie generated for a string switch"),
        w("must have at least this many entries (default: 16).")])).
optdb(oc_opt_hlm,   optopt_string_hash_switch_size,    int_special,
    alt_arg_help("string-hash-switch-size",
            ["string-switch-size"], "N", [
        w("The hash table generated for a string switch"),
        w("must have at least this many entries (default: 8).")])).
optdb(oc_opt_hlm,   optopt_string_binary_switch_size,  int_special,
    arg_help("string-binary-switch-size", "N", [
        w("The binary search table generated for a string switch"),
        w("must have at least this many entries (default: 4).")])).
optdb(oc_opt_hlm,   optopt_tag_switch_size,            int_special,
    arg_help("tag-switch-size", "N", [
        w("The number of alternatives in a tag switch"),
        w("must be at least this number (default: 3).")])).
% The next two options are only for performance tests.
optdb(oc_opt_hlm,   optopt_put_base_first_single_rec,   bool_special,
    priv_help("switch-single-rec-base-first", [
        w("In a switch with two arms, one a base case and one with a single"),
        w("recursive call, put the base case first.")])).
optdb(oc_opt_hlm,   optopt_put_base_first_multi_rec,   bool_special,
    priv_help("switch-multi-rec-base-first", [
        w("In a switch with two arms, one a base case and one with multiple"),
        w("recursive calls, do not put the base case first.")])).

optdb(oc_opt_hlm,   optopt_use_static_ground_cells,    bool_special,
    % XXX cf const-struct
    help("static-ground-terms", [
        w("Enable the optimization of constructing constant ground terms"),
        w("at compile time and storing them as static constants."),
        w("Note that auxiliary data structures created by the compiler"),
        w("for purposes such as debugging will always be created as"),
        w("static constants.")])).
optdb(oc_opt_hlm,   optopt_use_atomic_cells,           bool_special,
    help("use-atomic-cells", [
        w("Use the atomic variants of the Boehm gc allocator calls"),
        w("when the memory cell to be allocated cannot contain pointers.")])).
optdb(oc_opt_hlm,   optimize_trail_usage,              bool(no),
    % This option is developer-only.
    % It is intended for the benchmarking of trail usage optimization.
    % Otherwise, it should not be turned off, as doing so interferes with
    % the results of the trail usage analysis.
    % XXX Actually, this option is used in two completely different ways
    % in the LLDS and MLDS backends. The lines above apply to the MLDS version.
    priv_help("optimize-trail-usage", [
        w("Try to restrict trailing operations to those parts of the program"),
        w("that actually use the trail.")])).

%---------------------%

    % MLDS -> MLDS

% The internal names of the options below include an "mlds" qualifier.
% XXX Their preferred external names should do the same.

optdb(oc_opt_mm,    optopt_optimize_mlds,              bool_special,
    alt_help("mlds-optimize",
            ["mlds-optimise"], [
        w("Enable the MLDS->MLDS optimization passes.")])).
optdb(oc_opt_mm,    optopt_peep_mlds,                  bool_special,
    help("mlds-peephole", [
        w("Perform peephole optimization of the MLDS.")])).
optdb(oc_opt_hm,    optopt_opt_mlds_tailcalls,         bool_special,
    % The "self" qualifier is obsolete, and the user-visible option name
    % should include "mlds".
    alt_help("optimize-tailcalls",
            ["optimise-tailcalls"], [
        w("Turn self-tailcalls into loops.")])).
optdb(oc_opt_mm,    optopt_opt_mlds_inits,             bool_special,
    alt_help("optimize-initializations",
            ["optimise-initializations"], [
        w("Whenever possible,"),
        w("convert the first assignment to each local variable"),
        w("in the target code into an initializer on its declaration."),
        w("Some target language compilers"),
        w("can generate faster code that way.")])).
optdb(oc_opt_mm,    optopt_elim_unused_mlds_assigns,   bool_special,
    % This is useful for developers only.
    priv_help("eliminate-unused-mlds-assigns", [
        w("Eliminate assignments to dead variables in the MLDS.")])).
optdb(oc_opt_mm,    optopt_elim_local_vars_mlds,       bool_special,
    alt_help("eliminate-local-variables", ["eliminate-local-vars"], [
        w("Eliminate local variables with known values, where possible,"),
        w("by replacing occurrences of such variables with their values.")])).
% From the user point of view, the next options control an MLDS optimization.
optdb(oc_opt_hh,    optopt_gen_trail_ops_inline_mlds,  bool_special,
    help("generate-trail-ops-inline", [
        w("Normally, the compiler generates inline code"),
        w("for trailing operations."),
        w("With"), opt("--no-generate-trail-ops-inline", ","),
        w("the compiler will implement them using calls to those operations"),
        w("in the standard library.")])).

%---------------------%

    % HLDS -> LLDS

% The internal names of the options below include an "llds" qualifier,
% with the exceptions being the option names that do not make sense for MLDS,
% *and* the option names that *do* make sense for the MLDS, but there is
% some chance of them being *implemented* there.

% XXX reword some of the following help texts.
optdb(oc_opt_hl,    optopt_try_switch_size,            int_special,
    arg_help("try-switch-size", "N", [
        w("The number of alternatives in a try/retry chain switch"),
        w("must be at least this number (default: 3).")])).
optdb(oc_opt_hl,    optopt_binary_switch_size,         int_special,
    arg_help("binary-switch-size", "N", [
        w("The number of alternatives in a binary search switch"),
        w("must be at least this number (default: 4).")])).
optdb(oc_opt_hl,    optopt_opt_middle_rec_llds,        bool_special,
    help("middle-rec", [
        % XXX This could benefit from more detail.
        w("Enable the middle recursion optimization.")])).
optdb(oc_opt_hl,    optopt_opt_simple_neg_llds,        bool_special,
    help("simple-neg", [
        % XXX This could benefit from more detail.
        w("Generate simplified code for simple negations.")])).
optdb(oc_opt_hl,    optopt_allow_hijacks,              bool_special,
    priv_help("allow-hijacks", [
        w("When appropriate, the compiler normally generates code"),
        w("that temporarily hijacks an existing nondet stack frame"),
        w("that probably belongs to another procedure invocation."),
        opt("--no-allow-hijacks"), w("tells the compiler"),
        w("to create a new nondet stack frame instead.")])).

%---------------------%

    % LLDS -> LLDS

% The internal names of the options below include an "llds" qualifier,
% with the exceptions being the option names that do not make sense for MLDS.

optdb(oc_opt_ll,    optopt_optimize_llds,              bool_special,
    alt_help("llds-optimize",
            ["llds-optimise"], [
        w("Enable the LLDs->LLDS optimization passes.")])).
optdb(oc_opt_ll,    optopt_repeat_opts,                int_special,
    alt_arg_help("optimize-repeat",
            ["optimise-repeat"], "N", [
        w("Iterate most LLDS->LLDS optimizations at most"), arg("N"),
        w("times (default: 3).")])).
optdb(oc_opt_ll,    optopt_peep_llds,                  bool_special,
    alt_help("optimize-peep",
            ["optimise-peep"], [
        w("Enable local peephole optimizations.")])).
optdb(oc_opt_ll,    optopt_peep_llds_mkword,           bool_special,
    % This is useful for developers only, to test whether a gcc bug
    % has been fixed.
    priv_alt_help("optimize-peep-mkword",
            ["optimise-peep-mkword"], [
        w("Enable peephole optimizations of words created by mkword.")])).
optdb(oc_opt_ll,    optopt_opt_labels,                 bool_special,
    alt_help("optimize-labels",
            ["optimise-labels"], [
        w("Delete dead labels, and the unreachable code following them.")])).
optdb(oc_opt_ll,    optopt_standardize_labels,         bool_special,
    % This is useful for developers only.
    priv_alt_help("standardize-labels",
            ["standardise-labels"], [
        w("Standardize internal labels in the generated code.")])).
optdb(oc_opt_ll,    optopt_opt_jumps,                  bool_special,
    alt_help("optimize-jumps",
            ["optimise-jumps"], [
        w("Enable the short-circuiting of jumps to jumps.")])).
optdb(oc_opt_ll,    optopt_opt_fulljumps,              bool_special,
    alt_help("optimize-fulljumps",
            ["optimise-fulljumps"], [
        w("Enable the elimination of jumps to ordinary code.")])).
optdb(oc_opt_ll,    optopt_opt_checked_nondet_tailcalls, bool_special,
    help("checked-nondet-tailcalls", [
        w("Convert nondet calls into tail calls whenever possible, even"),
        w("when this requires a runtime check. This option tries to"),
        w("minimize stack consumption, possibly at the expense of speed.")])).
optdb(oc_opt_ll,    optopt_pessimize_llds_tailcalls,   bool_special,
    help("pessimize-tailcalls", [
        w("Disable the optimization of tailcalls."),
        w("This option tries to minimize code size"),
        w("at the expense of speed.")])).
optdb(oc_opt_ll,    optopt_opt_delay_slot,             bool_special,
    alt_help("optimize-delay-slot",
            ["optimise-delay-slot"], [
        w("Disable branch delay slot optimizations."),
        w("This option is meaningful only if"),
        w("the target architecture has delay slots.")])).
optdb(oc_opt_ll,    optopt_opt_frames,                 bool_special,
    alt_help("optimize-frames",
            ["optimise-frames"], [
        w("Optimize the operations that maintain stack frames.")])).
optdb(oc_opt_ll,    optopt_opt_llds_reassign,          bool_special,
    alt_help("optimize-reassign",
            ["optimise-reassign"], [
        w("Optimize away assignments to memory locations"),
        w("that already hold the to-be-assigned value.")])).
optdb(oc_opt_ll,    optopt_use_local_vars_llds,        bool_special,
    help("use-local-vars", [
        w("Use local variables in C code blocks wherever possible.")])).
optdb(oc_opt_ll,    optopt_llds_local_var_access_threshold, int_special,
    priv_arg_help("local-var-access-threshold", "XXX document me", [])).
optdb(oc_opt_ll,    optopt_opt_dup_instrs_llds,        bool_special,
    alt_help("optimize-dups",
            ["optimise-dups"], [
        w("Enable elimination of duplicate code within procedures.")])).
optdb(oc_opt_ll,    optopt_opt_dup_procs_llds,         bool_special,
    alt_help("optimize-proc-dups",
            ["optimise-proc-dups"], [
        w("Enable elimination of duplicate procedures.")])).
optdb(oc_opt_ll,    optopt_use_llds_common_data,       bool_special,
    help("common-data", [
        w("Enable optimization of common data structures.")])).
optdb(oc_opt_ll,    optopt_use_llds_common_layout_data, bool_special,
    help("common-layout-data", [
        w("Disable optimization of common subsequences"),
        w("in layout structures.")])).
optdb(oc_opt_ll,    optopt_llds_layout_compression_limit, int_special,
    arg_help("layout-compression-limit", "N", [
        w("Attempt to compress the layout structures used by the debugger"),
        w("only as long as the arrays involved have at most"),
        arg("N"), w("elements (default: 4000).")])).
optdb(oc_opt_hl,    optimize_region_ops,               bool(no),
    priv_help("optimize-region-ops", [
        w("Try and restrict region operations to those parts of the program"),
        w("that actually use regions.")])).

%---------------------%

    % LLDS -> C

optdb(oc_opt_lc,    optopt_emit_c_loops,               bool_special,
    help("emit-c-loops", [
        w("Use C loop constructs to implement loops."),
        w("With"), opt("--no-emit-c-loops", ","), w("use only gotos.")])).
optdb(oc_opt_lc,    optopt_procs_per_c_function,       int_special,
    alt_arg_help("procs-per-c-function",
            ["procs-per-C-function"], "N", [
        w("Put the code for up to"), arg("N"),
        w("Mercury procedures in a single C function."),
        w("The default value of"), arg("N"), w("is one."),
        w("Increasing"), arg("N"), w("can produce"),
        w("slightly more efficient code, but makes compilation slower.")])).
optdb(oc_opt_lc,    optopt_use_local_thread_engine_base, bool_special,
    help("local-thread-engine-base", [
        w("Do not copy the thread-local Mercury engine base address"),
        w("into a local variable, even when this would be appropriate."),
        w("This option is effective only in low-level parallel grades"),
        w("that do not use the GNU C global register variables extension.")])).
% optopt_inline_alloc works by giving a flag to the C compiler, but
% from the user's point of view, it is about giving different code
% to the C compiler.
optdb(oc_opt_lc,    optopt_inline_alloc,               bool_special,
    % XXX The speedup from this option is dubious.
    help("inline-alloc", [
        w("Inline calls to"), code("GC_malloc()", "."),
        w("This can improve performance a fair bit,"),
        w("but may significantly increase code size."),
        w("This option is meaningful only if the selected garbage collector"),
        w("is boehm, and if the C compiler is gcc.")])).
optdb(oc_opt_lc,    optopt_use_macro_for_redo_fail,    bool_special,
    % XXX The speedup from this option is dubious.
    help("use-macro-for-redo-fail", [
        w("Emit the fail or redo macro instead of a branch"),
        w("to the fail or redo code in the runtime system."),
        w("This produces slightly bigger but slightly faster code.")])).

%---------------------------------------------------------------------------%

    % Options to control intermodule optimization.

optdb(oc_plain_opt,  intermodule_optimization,         bool(no),
    alt_help("intermodule-optimization",
            ["intermodule-optimisation", "intermod-opt"], [
        w("Perform inlining, higher-order specialization,"),
        w("and other optimizations using knowledge"),
        w("of the otherwise non-public data of directly imported modules."),
        w("The compiler gets this data from the"),
        file_var("module", "opt"), w("files"),
        w("of the directly imported modules."),
        blank_line,
        w("This option must be set consistently"),
        w("throughout the compilation process, starting with"),
        code("mmc --generate-dependencies", ".")])).
optdb(oc_plain_opt, use_opt_files,                     bool(no),
    help("use-opt-files", [
        w("Perform inter-module optimization using any"),
        file(".opt"), w("files which are already built,"),
        % XXX In texinfo, we use e.g.@:
        w("e.g. those for the standard library,"),
        w("but do not build any others.")])).
optdb(oc_plain_opt, read_opt_files_transitively,       bool(yes),
    help("read-opt-files-transitively", [
        w("Only read the inter-module optimization information"),
        w("for directly imported modules, not the transitive"),
        w("closure of the imports.")])).

optdb(oc_trans_opt, transitive_optimization,           bool(no),
    alt_help("transitive-intermodule-optimization",
            ["transitive-intermodule-optimisation", "trans-intermod-opt"], [
        w("Perform inlining, higher-order specialization,"),
        w("and other optimizations using knowledge"),
        w("of the otherwise non-public data of"),
        w("both directly and indirectly imported modules."),
        w("The compiler gets this data from the"),
        file_var("module", "trans_opt"), w("files"),
        w("of the directly and indirectly imported modules."),
        blank_line,
        w("This option must be set consistently"),
        w("throughout the compilation process, starting with"),
        code("mmc --generate-dependencies", "."),
        blank_line,
        w("Note that"), opt("--transitive-intermodule-optimization"),
        w("works only with"), code("mmake", ";"),
        w("it does not work with"), code("mmc --make", ".")])).
optdb(oc_trans_opt, use_trans_opt_files,               bool(no),
    help("use-trans-opt-files", [
        w("Perform inter-module optimization using any"),
        file(".trans_opt"), w("files which are already built,"),
        w("e.g. those for the standard library,"),
        w("but do not build any others.")])).
optdb(oc_trans_opt, also_output_module_order,          bool(no),
    % XXX The connection between this option and transitive intermodule
    % optimization is not made clear by this help text.
    % "--generate-module-order" is the old name of this option.
    alt_help("also-output-module-order", ["generate-module-order"], [
        w("If"), opt("--generate-dependencies"), w("is also specified,"),
        w("then output the strongly connected components of the module"),
        w("dependency graph in top-down order to"),
        file_var("module", "module_order", "."),
        % XXX We also output it, in a possibly-further-modified form,
        % to module.module_order_for_trans_opt, but we want to document that
        % only *after* we have documented the trans_opt_deps_spec option,
        % which specifies *how* that modification should be done.
        w("If"), opt("--generate-dependencies"), w("is not specified,"),
        w("then this option does nothing.")])).
optdb(oc_trans_opt, trans_opt_deps_spec,               maybe_string(no),
    % This option is for developers only for now.
    priv_arg_help("trans-opt-deps-spec", "filename", [
        % XXX This sentence is missing some words.
        w("When constructing e.g."), file_var("module_a", "trans_opt", ","),
        w("the compiler is allowed to read the"), file(".trans_opt"),
        w("files of only the modules that follow"), var("module_a"),
        w("in a designated module order. The purpose of this restriction"),
        w("is the prevention of circular dependencies between"),
        file(".trans_opt"), w("files."),
        w("The compiler normally decides this module order by"),
        w("constructing the dependency graph between modules,"),
        w("computing its strongly connected components (SCCs),"),
        w("and flattening those components in a top-down-order,"),
        w("breaking the circular dependencies inside each SCC"),
        w("effectively randomly."),
        w("If this option is specified, and the named file contains"),
        w("a properly formatted specification of a set of directed edges"),
        w("between pairs of modules, then the compiler will remove"),
        w("those edges from the dependency graph before computing the SCCs."),
        % XXX Add a pointer to the location of that file format specification,
        % once it exists.
        blank_line,
        w("If the information flowing between"), file(".trans_opt"),
        w("files along those edges is less useful for optimization"),
        w("than information flowing through other edges, then"),
        w("removing those edges can help save more valuable edges"),
        w("from being broken. It can also break a large SCC into"),
        w("several smaller SCCs, which may allow more"),
        file(".trans_opt"), w("files to be built in parallel."),
        w("(The"), file(".trans_opt"), w("files of the modules in an SCC"),
        w("have to be built in sequence,"),
        w("but it is often possible to build the"), file(".trans_opt"),
        w("file sequences of different SCCs in parallel.)")])).

optdb(oc_latex_opt, intermodule_analysis,              bool(no),
    priv_help("intermodule-analysis", [
        w("Perform analyses such as termination analysis and"),
        w("unused argument elimination across module boundaries."),
        w("This option is not yet fully implemented.")])).
optdb(oc_latex_opt, analysis_repeat,                   int(0),
    priv_arg_help("analysis-repeat", "N", [
        w("Specify the maximum number of times to repeat analyses"),
        w("of suboptimal modules with"), opt("--intermodule-analysis"),
        w("(default: 0)."),
        w("(This option works only with"), code("mmc --make", ".)")])).
optdb(oc_latex_opt, analysis_file_cache,               bool(no),
    % This feature is still experimental.
    priv_help("analysis-file-cache", [
        w("Enable caching of parsed analysis files. This may"),
        w("improve compile times with"), opt("--intermodule-analysis", ".")])).
optdb(oc_latex_opt, analysis_file_cache_dir,           string(""),
    % The `--analysis-file-cache-dir' option is used by `mmc --make'.
    priv_arg_help("analysis-file-cache-dir", "directory", [])).

%---------------------------------------------------------------------------%

    % Analysis options.

optdb(oc_pm_term1,  termination_enable,                bool(no),
    alt_help("enable-termination", ["enable-term"], [
        w("Enable termination analysis, which analyses each mode"),
        w("of each predicate or function to see whether it terminates."),
        w("The"), code("terminates", ","), code("does_not_terminate", ","),
        w("and"), code("check_termination"), w("pragmas have an effect"),
        w("only when termination analysis is enabled."),
        blank_line,
        w("Note that both"), opt("--intermodule-optimization"),
        w("and"), opt("--transitive-intermodule-optimization"),
        w("greatly improve the accuracy of the analysis.")])).
optdb(oc_pm_term1,  termination_check,                 bool(no),
    alt_help("check-termination",
            ["check-term", "chk-term"], [
        w("Enable termination analysis, and emit warnings for some"),
        w("predicates or functions that cannot be proved to terminate."),
        blank_line,
        w("In many cases where the compiler is unable to prove termination,"),
        w("the problem is either a lack of information about the"),
        w("termination properties of other predicates, or the use of"),
        w("language constructs (such as higher order calls)"),
        w("which are beyond the capabilities of the analyser."),
        w("In these cases, the compiler does not emit a"),
        w("warning of non-termination, as it is likely to be spurious.")])).
optdb(oc_pm_term1,  termination_check_verbose,         bool(no),
    alt_help("verbose-check-termination",
            ["verb-check-term", "verb-chk-term"], [
        w("Enable termination analysis, and emit warnings for all"),
        w("predicates or functions that cannot be proved to terminate.")])).
optdb(oc_pm_term1,  termination_single_args,           int(0),
    alt_arg_help("termination-single-argument-analysis",
            ["term-single-arg"], "N", [
        w("When performing termination analysis, try analyzing"),
        w("recursion on single arguments in strongly connected components"),
        w("of the call graph that have up to the given number of procedures."),
        w("Setting this limit to zero disables single argument analysis.")])).
optdb(oc_pm_term1,  termination_norm,                  string("total"),
    alt_arg_help("termination-norm",
            ["term-norm"], "{simple, total, num-data-elems}", [
        w("The norm defines how termination analysis measures the size"),
        w("of a memory cell. The"), samp("simple"), w("norm says that"),
        w("size is always one. The"), samp("total"), w("norm says that"),
        w("it is the number of words in the cell. The"),
        samp("num-data-elems"), w("norm says that it is the number of words"),
        w("in the cell that contain something other than pointers"),
        w("to cells of the same type.")])).
optdb(oc_pm_term1,  termination_error_limit,           int(3),
    alt_arg_help("termination-error-limit",
            ["term-err-limit"], "N", [
        w("Print at most this number of reasons for"),
        w(" any single termination error (default: 3).")])).
optdb(oc_pm_term1,  termination_path_limit,            int(256),
    alt_arg_help("termination-path-limit",
            ["term-path-limit"], "N", [
        w("Perform termination analysis only on predicates"),
        w("with at most this many paths (default: 256).")])).

    % The termination2_* options are used to control the new termination
    % analyser. They are currently undocumented because that is still
    % a work-in-progress. XXX Or is it?
optdb(oc_pm_term2,  termination2_enable,               bool(no),
    priv_alt_help("enable-termination2", ["enable-term2"], [
        w("Analyse each predicate to discover if it terminates."),
        w("This uses an alternative termination analysis based"),
        w("on convex constraints.")])).
optdb(oc_pm_term2,  termination2_check,                bool(no),
    priv_alt_help("check-termination2",
            ["check-term2", "chk-term2"], [
        w("Enable the alternative termination analysis,"),
        w("and emit warnings for some predicates or functions"),
        w("that cannot be proven to terminate."),
        w("In many cases where the compiler is unable to prove termination,"),
        w("the problem is either a lack of information about the"),
        w("termination properties of other predicates, or because language"),
        w("constructs (such as higher order calls) were used which could"),
        w("not be analysed. In these cases, the compiler does not emit a"),
        w("warning of non-termination, as it is likely to be spurious.")])).
optdb(oc_pm_term2,  termination2_check_verbose,        bool(no),
    priv_alt_help("verbose-check-termination2",
            ["verb-check-term2", "verb-chk-term2"], [
        % XXX These options used to have no documentation at all.
        % The following is my guess (zs).
        w("Report more verbose errors from the alternative termination"),
        w("analysis algorithm")])).
optdb(oc_pm_term2,  termination2_norm,                 string("total"),
    priv_alt_arg_help("termination2-norm",
            ["term2-norm"], "{simple, total, num-data-elems}", [
        w("Tell the alternative termination analyser which norm to use."),
        w("See the description of the"), opt("--termination-norm"),
        w("option for a description of the different norms available.")])).
optdb(oc_pm_term2,  termination2_widening_limit,       int(4),
    priv_alt_arg_help("termination2-widening-limit",
            ["term2-widening-limit"], "N", [
        w("Set the threshold for the number of iterations after which"),
        w("the argument size analyser invokes widening.")])).
optdb(oc_pm_term2,  termination2_arg_size_only,        bool(no),
    % This option is for developers only.
    % It is useful for benchmarking the argument size analysis.
    priv_alt_help("term2-argument-size-analysis-only",
            ["term2-arg-size-analysis-only", "arg-size-analysis-only"], [
        w("Perform argument size analysis on each SCC, but do not"),
        w("attempt to infer termination,")])).
optdb(oc_pm_term2,  termination2_prop_fail_constrs,    bool(yes),
    priv_alt_help("termination2-propagate-failure-constraints",
            ["term2-propagate-failure-constraints",
            "term2-propagate-failure-constrs"], [
        w("Make the argument analyser infer information about the sizes of"),
        w("any inputs to a goal in contexts where that goal fails.")])).
optdb(oc_pm_term2,  termination2_maximum_matrix_size,  int(70),
    % XXX This matrix size is just a guess.
    priv_alt_arg_help("termination2-maximum-matrix-size",
            ["term2-max-matrix-size"], "N", [
        w("Limit the sizes of constraints systems in the analyser to the"),
        w("given number of constraints. Use approximations of some"),
        w("constraint operations, such as projection, if this threshold"),
        w("is exceeded. This will speed up the analysis"),
        w("at the cost of reduced precision.")])).

optdb(oc_pm_misc,  analyse_exceptions,                bool(no),
    help("analyse-exceptions", [
        w("Enable exception analysis. which tries to identify"),
        w("procedures that will not throw an exception."),
        w("Some optimizations can make use of this information.")])).
optdb(oc_pm_misc,  analyse_closures,                  bool(no),
    % XXX The option controlling closure analysis are currently
    % private because it isn't useful. It can be made public when
    % we actually have something that uses it.
    priv_alt_help("analyse-local-closures",
            ["analyse-closures"], [
        w("Enable closure analysis, which tries identify the possible"),
        w("values that higher-order valued variables can take."),
        w("Some optimizations can make use of this information.")])).
optdb(oc_pm_misc,  analyse_trail_usage,               bool(no),
    help("analyse-trail-usage", [
        w("Enable trail usage analysis, which tries to identify"),
        w("procedures that will not modify the trail."),
        w("The compiler can use this information to reduce the overhead"),
        w("of trailing.")])).
optdb(oc_pm_misc,  analyse_mm_tabling,                bool(no),
    help("analyse-mm-tabling", [
        w("Identify those goals that do not call procedures"),
        w("that are evaluated using minimal model tabling."),
        w("The compiler can use this information to reduce the overhead"),
        w("of minimal model tabling.")])).

%---------------------------------------------------------------------------%

    % Options that ask for small modifications to generated files.

optdb(oc_output_mod, line_numbers,                      bool(no),
    short_help('n', "line-numbers", [], [
        w("Put source line numbers into the generated code."),
        w("The generated code may be in C, Java or C# (the usual cases),"),
        w("or in Mercury (with"), opt("--convert-to-mercury", ").")])).
optdb(oc_output_mod, line_numbers_around_foreign_code,  bool(yes),
    help("line-numbers-around-foreign-code", [
        w("Do not put source line numbers into the generated"),
        w("target language file"),
        w("around inclusions of foreign language code.")])).
optdb(oc_output_mod, line_numbers_for_c_headers,        bool(no),
    help("line-numbers-for-c-headers", [
        w("Put source line numbers in the generated C header files."),
        w("This can make it easier to track down any problems with"),
        w("C code in"), code("foreign_decl"), w("pragmas, but may cause"),
        w("unnecessary recompilations of other modules if"),
        w("any of these line numbers changes"),
        w("(e.g. because the location of a predicate declaration"),
        w("changes in the Mercury source file).")])).
optdb(oc_output_dev, type_repns_for_humans,             bool(no),
    % This option is for developers only.
    priv_help("type-repns-for-humans", [
        w("Format type_repn items in automatically generated interface files"),
        w("to be more easily read by humans.")])).
optdb(oc_output_dev, auto_comments,                     bool(no),
    help("auto-comments", [
        w("Output comments in the generated target language file."),
        w("This is primarily useful for trying to understand"),
        w("how the generated target language code relates"),
        w("to the source code, e.g.@: in order to debug the compiler."),
        w("(The code may be easier to understand if you also"),
        w("use the"), opt("--no-llds-optimize"), w("option.)")])).
optdb(oc_output_dev, frameopt_comments,                 bool(no),
    % This option is for developers only. Since it can include one C comment
    % inside another, the resulting code is not guaranteed to be valid C.
    priv_help("frameopt-comments", [
        w("Get frameopt.m to generate comments describing its operation."),
        w("(The code may be easier to understand if you also"),
        w("use the"), opt("--no-llds-optimize"), w("option.)")])).

%---------------------------------------------------------------------------%

    % "mmc --make" options.

optdb(oc_make,      keep_going,                        bool(no),
    short_help('k', "keep-going", [], [
        w("With"), opt("--make", ","), w("keep going as far as possible"),
        w("even if an error is detected.")])).
optdb(oc_make,      order_make_by_timestamp,           bool(no),
    help("order-make-by-timestamp", [
        w("Tell"), code("mmc --make"), w("to compile"),
        w("more recently modified source files first.")])).
optdb(oc_make,      show_make_times,                   bool(no),
    help("show-make-times", [
        w("Report run times for commands executed by"),
        code("mmc --make", ".")])).
optdb(oc_make,      make_max_jobs,                     int(1),
    short_arg_help('j', "jobs", [], "N", [
        w("With"), opt("--make", ","), w("attempt to perform up to"),
        bare_arg("N"), w("jobs concurrently.")])).
optdb(oc_make,      make_track_flags,                  bool(no),
    alt_help("track-flags", ["track-options"], [
        w("With"), opt("--make", ","), w("keep track of the options used"),
        w("when compiling each module. If an option for a module"),
        w("is added or removed,"), code("mmc --make"), w("will then know"),
        w("to recompile the module even if"),
        w("the timestamp on the file itself has not changed."),
        w("Warning options, verbosity options, and build system options"),
        w("are not tracked.")])).
optdb(oc_make,      make_pre_link_command,             maybe_string(no),
    arg_help("pre-link-command", "command", [
        w("Specify a command to run before linking with"),
        code("mmc --make", "."),
        w("This can be used to compile C source files which rely on"),
        w("header files generated by the Mercury compiler."),
        w("The command will be passed the names of all of the source"),
        w("files in the program or library, with the source file"),
        w("containing the main module given first.")])).

%---------------------------------------------------------------------------%

    % Target code compilation options.

optdb(oc_target_comp, target_debug,                      bool(no),
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
        w("Enable debugging of the generated target code."),
        w("If the target language is C, this has the same effect as"),
        opt("--c-debug"), w("(see below)."),
        w("If the target language is C#, this causes the compiler to pass"),
        code("/debug"), w("to the C# compiler.")])).
optdb(oc_target_comp, warn_target_code,                  bool(yes),
    % XXX Does this apply to Java and C# too?
    help("warn-target-code", [
        w("Disable warnings from the compiler (which may be e.g. gcc)"),
        w("that processes the target code generated by mmc.")])).

%---------------------%

    % C

optdb(oc_target_c,  cc,                                string("gcc"),
    % The `mmc' script will override the default with a value
    % determined at configuration time.
    arg_help("cc", "compiler-name", [
        cindex("C compiler"),
        w("Specify which C compiler to use.")])).
optdb(oc_target_c,  c_compiler_type,                   string("gcc"),
    priv_arg_help("c-compiler-type",
        "{gcc,clang,msvc_x86,msvc_x64,unknown}", [])).
    % The `mmc' script will override the default with a value
    % determined at configuration time for the above two options.
    % XXX That argues for this option being oc_config.
optdb(oc_target_c,  optopt_c_optimize,                 bool_special,
    alt_help("c-optimize",
            ["c-optimise"], [
        w("Enable optimizations by the C compiler.")])).
optdb(oc_target_c,  c_include_directories,             accumulating([]),
    % The `mmc' script will override the default with a value
    % determined at configuration time.
    alt_arg_help("c-include-directory",
            ["c-include-dir"], "directory", [
        w("Append"), arg("directory"), w("to the list of directories"),
        w("to be searched for C header files."),
        w("Note that if you want to override this list, instead of"),
        w("appending to it, then you can set the"),
        env("MERCURY_MC_ALL_C_INCL_DIRS"), w("environment variable to a"),
        w("sequence of"), opt("--c-include-directory"), w("options.")])).
optdb(oc_target_c,  cflags,                            accumulating([]),
    arg_help("cflags", "<options", [
        cindex("C compiler options"),
        w("Specify options to be passed to the C compiler."),
        w("These options will not be quoted when passed to the shell.")])).
optdb(oc_target_c,  quoted_cflag,                      string_special,
    alt_arg_help("quoted-cflag", ["cflag"], "option", [
        w("Specify a single word option to be passed to the C compiler."),
        w("The word will be quoted when passed to the shell.")])).

optdb(oc_target_c,  gcc_flags,                         accumulating([]),
    % XXX part of mmc --make; but needs more detail.
    priv_arg_help("gcc-flags", "flags", [])).
optdb(oc_target_c,  quoted_gcc_flag,                   string_special,
    % XXX document me.
    priv_alt_arg_help("quoted-gcc-flag", ["gcc-flag"], "flag", [])).
optdb(oc_target_c,  clang_flags,                       accumulating([]),
    % XXX part of mmc --make; but needs more detail.
    priv_arg_help("clang-flags", "flags", [])).
optdb(oc_target_c,  quoted_clang_flag,                 string_special,
    % XXX document me.
    priv_alt_arg_help("quoted-clang-flag", ["clang-flag"], "flag", [])).
optdb(oc_target_c,  msvc_flags,                        accumulating([]),
    % XXX part of mmc --make; but needs more detail.
    priv_arg_help("msvc-flags", "flags", [])).
optdb(oc_target_c,  quoted_msvc_flag,                  string_special,
    % XXX document me.
    priv_alt_arg_help("quoted-msvc-flag", ["msvc-flag"], "flag", [])).

% XXX All of the following options are reserved for the mmc script,
% but they nevertheless should have private help text.
optdb(oc_target_c,  cflags_for_warnings,               string(""),
    % The `mmc' script will override the default with values
    % determined at configuration time.
    priv_arg_help("cflags-for-warnings", "flags", [])).
optdb(oc_target_c,  cflags_for_optimization,           string("-O"),
    priv_arg_help("cflags-for-optimization", "flags", [])).
optdb(oc_target_c,  cflags_for_debug,                  string("-g"),
    priv_arg_help("cflags-for-debug", "flags", [])).
optdb(oc_target_c,  cflags_for_regs,                   string(""),
    priv_arg_help("cflags-for-regs", "flags", [])).
optdb(oc_target_c,  cflags_for_gotos,                  string(""),
    priv_arg_help("cflags-for-gotos", "flags", [])).
optdb(oc_target_c,  cflags_for_threads,                string(""),
    priv_arg_help("cflags-for-threads", "flags", [])).
optdb(oc_target_c,  cflags_for_pic,                    string(""),
    priv_arg_help("cflags-for-pic", "flags", [])).
optdb(oc_target_c,  cflags_for_sanitizers,             string(""),
    priv_arg_help("cflags-for-sanitizers", "flags", [])).
optdb(oc_target_c,  cflags_for_lto,                    string(""),
    priv_arg_help("cflags-for-lto", "flags", [])).
optdb(oc_target_c,  c_flag_to_name_object_file,        string("-o "),
    priv_arg_help("c-flag-to-name-object-file", "flags", [])).
optdb(oc_target_c,  object_file_extension,             string(".o"),
    priv_arg_help("object-file-extension", "extension", [])).
optdb(oc_target_c,  pic_object_file_extension,         string(".o"),
    priv_arg_help("pic-object-file-extension", "extension", [])).

%---------------------%

    % Java

optdb(oc_target_java, java_compiler,                     string("javac"),
    alt_arg_help("java-compiler", ["javac"], "javac", [
        cindex("Java compiler"),
        w("Specify which Java compiler to use. The default is"),
        samp("javac", ".")])).
optdb(oc_target_java, java_interpreter,                  string("java"),
    arg_help("java-interpreter", "java", [
        cindex("Java interpreter"),
        w("Specify which Java interpreter to use."),
        w("The default is"), samp("java", ".")])).
optdb(oc_target_java, java_compiler_flags,               accumulating([]),
    alt_arg_help("javac-flags", ["java-flags"], "options", [
        cindex("Java compiler flags"),
        w("Specify options to be passed to the Java compiler."),
        w("These options will not be quoted when passed to the shell.")])).
optdb(oc_target_java, quoted_java_compiler_flag,         string_special,
    alt_arg_help("quoted-javac-flag",
            ["quoted-java-flag", "javac-flag", "java-flag"], "option", [
        w("Specify a single word option to be passed to the Java compiler."),
        w("The word will be quoted when passed to the shell.")])).
optdb(oc_target_java, java_classpath,                    accumulating([]),
    arg_help("java-classpath", "path", [
        cindex("classpath"),
        cindex("Directories"),
        w("Set the classpath for the Java compiler and interpreter.")])).
optdb(oc_target_java, java_runtime_flags,                accumulating([]),
    arg_help("java-runtime-flags", "options", [
        cindex("Java runtime options"),
        w("Specify options to be passed to the Java interpreter."),
        w("These options will not be quoted when passed to the shell.")])).
optdb(oc_target_java, quoted_java_runtime_flag,          string_special,
    alt_arg_help("quoted-java-runtime-flag", ["java-runtime-flag"], "option", [
        w("Specify a single word option to be passed"),
        w("to the Java interpreter."),
        w("The word will be quoted when passed to the shell.")])).

%---------------------%

    % C#

optdb(oc_target_csharp, csharp_compiler,                   string("csc"),
    arg_help("csharp-compiler", "csc", [
        cindex("C# compiler"),
        w("Specify the name of the C# Compiler. The default is"),
        samp("csc", ".")])).
optdb(oc_target_csharp, cli_interpreter,                   string(""),
    arg_help("cli-interpreter", "prog", [
        cindex("CIL interpreter"),
        w("Specify the program that implements the Common Language"),
        w("Infrastructure (CLI) execution environment, e.g."),
        samp("mono", ".")])).
optdb(oc_target_csharp, csharp_compiler_type,              string("mono"),
    % The `mmc' script will override the default with a value
    % determined at configuration time for the above two options.
    priv_arg_help("csharp-compiler-type", "{microsoft,mono,unknown}", [])).
optdb(oc_target_csharp, csharp_flags,                      accumulating([]),
    arg_help("csharp-flags", "options", [
        cindex("C# compiler options"),
        w("Specify options to be passed to the C# compiler."),
        w("These options will not be quoted when passed to the shell.")])).
optdb(oc_target_csharp, quoted_csharp_flag,                string_special,
    alt_arg_help("quoted-csharp-flag", ["csharp-flag"], "option", [
        w("Specify a single word option to be passed to the C# compiler."),
        w("The word will be quoted when passed to the shell.")])).
optdb(oc_target_csharp, mono_path_directories,           accumulating([]),
    alt_arg_help("mono-path-directory",
        ["mono-path-dir"], "directory", [
        w("Specify a directory to add to the runtime library assembly"),
        w("search path passed to the Mono CLR using the"),
        env("MONO_PATH"), w("environment variable.")])).

%---------------------------------------------------------------------------%

    % Link Options.
    %
    % XXX Several of the options in this section do not affect linking, but
    % instead affect compilation (of things other than Mercury modules).
    % These include init_files, trace_init_files, runtime_flags,
    % extra_init_functions, mkinit_command, and maybe more.

%---------------------%

    % Options that apply to C, C# and Java.

optdb(oc_link_c_cs_j, mercury_library_directory_special, string_special,
    % NOTE mercury_library_directory_special sets search_directories,
    % c_include_directories and mercury_library_directories.
    alt_arg_help("mercury-library-directory",
            ["mld"], "directory", [
        cindex("Directories for libraries"),
        cindex("Search path for libraries"),
        w("Append"), arg("directory"), w("to the list of directories"),
        w("to be searched for Mercury libraries. This will add"),
        opt("--search-directory", ","), opt("--library-directory", ","),
        opt("--init-file-directory"), w("and"), opt("--c-include-directory"),
        w("options as needed."),
        texinfo_only(
            [xref("Using installed libraries with mmc --make", ".")])])).
optdb(oc_link_c_cs_j, mercury_library_directories,       accumulating([]),
    no_help).
optdb(oc_link_c_cs_j, search_library_files_directory_special, string_special,
    % NOTE search_library_files_directory_special sets search_directories,
    % c_include_directories and search_library_files_directories.
    % XXX This list just above DIFFERS from the list below.
    alt_arg_help("search-library-files-directory",
            ["search-lib-files-dir"], "directory", [
        cindex("Directories for libraries"),
        cindex("Search path for libraries"),
        % XXX Similar to, but in what respects exactly?
        % And in what respects NOT similar?
        w("Search"), arg("directory"), w("for Mercury library files"),
        w("that have not yet been installed. Similar to adding"),
        arg("directory"), w("using all of the"),
        opt("--search-directory", ","), opt("--intermod-directory", ","),
        opt("--library-directory", ","), opt("--init-file-directory"),
        w("and"), opt("--c-include-directory"), w("options.")])).
optdb(oc_link_c_cs_j, search_library_files_directories,  accumulating([]),
    no_help).
optdb(oc_link_c_cs_j, mercury_library_special,           string_special,
    % NOTE mercury_library_special adds to the values of link_libraries,
    % mercury_libraries and init_files.
    alt_arg_help("mercury-library", ["ml"], "library", [
        w("Link with the specified Mercury library."),
        texinfo_only(
            [xref("Using installed libraries with mmc --make", ".")])])).
optdb(oc_link_c_cs_j, mercury_libraries,                 accumulating([]),
    no_help).
optdb(oc_link_c_cs_j, mercury_standard_library_directory_special,
                                                     maybe_string_special,
    % NOTE mercury_standard_library_directory_special sets
    % mercury_standard_library_directory and mercury_configuration_directory.
    alt_arg_help("mercury-standard-library-directory",
            ["mercury-stdlib-dir"], "directory", [
        w("Search"), arg("directory"), w("for the Mercury standard library."),
        w("Implies"), opt_arg("--mercury-library-directory", "directory"),
        w("and"),
        opt_arg("--mercury-configuration-directory", "directory", "."),
        w("The negative version,"),
        opt("--no-mercury-standard-library-directory", ","),
        w("tells the compiler not to use the Mercury standard library,"),
        w("and also implies"),
        opt("--no-mercury-configuration-directory", ".")])).
optdb(oc_link_c_cs_j, mercury_standard_library_directory, maybe_string(no),
    % The Mercury.config file will set the default
    % standard library directory.
    no_help).

%---------------------%

    % Options that apply to C and C#.

% XXX The internal and external names of the next two options
% seem to be different for no good reason.
optdb(oc_link_c_cs, link_library_directories,          accumulating([]),
    short_arg_help('L', "library-directory", [], "directory", [
        cindex("Directories for libraries"),
        cindex("Search path for libraries"),
        w("Append"), arg("directory"), w("to the list of directories"),
        w("in which to search for libraries.")])).
optdb(oc_link_c_cs, link_libraries,                    accumulating([]),
    short_arg_help('l', "library", [], "library", [
        cindex("Libraries, linking with"),
        w("Link with the specified library.")])).
% XXX Source-to-source debugging is not ready for the public.
% XXX And for some reason, it does not (yet) apply to Java.
optdb(oc_link_c_cs, link_ssdb_libs,                    bool(no),
    priv_alt_help("link-ssdebug-libs", ["link-ssdb-libs"], [
        w("Link the source to source debugging libraries into the"),
        w("executable.")])).

%---------------------%

    % Options that apply only to C.

optdb(oc_link_c,    output_file_name,                  string(""),
    % If the output_file_name is an empty string, we use the name
    % of the first module on the command line.
    short_arg_help('o', "output-file", [], "filename", [
        w("Specify the name of the final executable."),
        w("(The default executable name is the same as the name"),
        w("of the first module on the command line.)"),
        % XXX So when *is* it effective?
        w("This option is ignored by"), code("mmc --make", ".")])).
optdb(oc_link_c,    link_objects,                      accumulating([]),
    arg_help("link-object", "filename", [
        cindex("Object files, linking with"),
        w("Link with the specified object file,"),
        w("or archive of object files.")])).
optdb(oc_link_c,    ld_flags,                          accumulating([]),
    arg_help("ld-flags", "options", [
        w("Specify options to be passed to the linker command"),
        w("that will create an executable."),
        w("These options will not be quoted when passed to the shell."),
        w("Use"), code("mmc --output-link-command"),
        w("to find out what the linker command is.")])).
optdb(oc_link_c,    quoted_ld_flag,                    string_special,
    alt_arg_help("quoted-ld-flag", ["ld-flag"], "option", [
        w("Specify a single word option to be passed to the linker command"),
        w("that will create an executable."),
        w("The word will be quoted when passed to the shell."),
        w("Use"), code("mmc --output-link-command"),
        w("to find out what the linker command is.")])).
optdb(oc_link_c,    ld_libflags,                       accumulating([]),
    arg_help("ld-libflags", "options", [
        w("Specify options to be passed to the linker command"),
        w("that will create a shared library."),
        w("These options will not be quoted when passed to the shell."),
        w("Use"), code("mmc --output-shared-lib-link-command"),
        w("to find out what the linker command is.")])).
optdb(oc_link_c,    quoted_ld_libflag,                 string_special,
    alt_arg_help("quoted-ld-libflag", ["ld-libflag"], "option", [
        w("Specify a single word option to be passed to the linker command"),
        w("that will create a shared library."),
        w("The word will be quoted when passed to the shell."),
        w("Use"), code("mmc --output-shared-lib-link-command"),
        w("to find out what the linker command is.")])).
optdb(oc_link_c,    runtime_link_library_directories,  accumulating([]),
    short_arg_help('R', "runtime-library-directory", [], "directory", [
        w("Append"), arg("directory"), w("to the list of directories"),
        w("in which to search for shared libraries at runtime.")])).
optdb(oc_link_c,    use_default_runtime_library_directory, bool(yes),
    % The use_default_runtime_library_directory option is only ever used
    % to decide whether to modify the value of the
    % runtime_link_library_directories option.
    help("default-runtime-library-directory", [
        w("Do not add any directories to the runtime search path"),
        w("automatically.")])).
optdb(oc_link_c,    init_file_directories,             accumulating([]),
    arg_help("init-file-directory", "directory", [
        w("Append"), arg("directory"), w("to the list of directories"),
        w("to be searched for"), file(".init"), w("files by"),
        code("c2init", ".")])).
optdb(oc_link_c,    init_files,                        accumulating([]),
    arg_help("init-file", "init-file", [
        w("Append"), arg("init-file"), w("to the list of"), file(".init"),
        w("files to be passed to"), code("c2init", ".")])).
optdb(oc_link_c,    trace_init_files,                  accumulating([]),
    arg_help("trace-init-file", "init-file", [
        w("Append"), arg("init-file"), w("to the list of"), file(".init"),
        w("files to be passed to"), code("c2init"),
        w("when tracing is enabled.")])).
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
optdb(oc_link_c,    linkage_special,                   string_special,
    % Both the external and internal names of this option
    % should be more descriptive.
    arg_help("linkage", "{shared, static}", [
        w("Specify whether to use shared or static linking for executables."),
        w("Shared libraries are always linked with"),
        opt("--linkage shared", ".")])).
optdb(oc_link_c,    only_globals_linkage,              string("shared"),
    no_help).
% NOTE mercury_linkage_special first checks and then sets
% only_globals_mercury_linkage.
optdb(oc_link_c,    mercury_linkage_special,           string_special,
    % Both the external and internal names of this option
    % should be more descriptive.
    arg_help("mercury-linkage", "{shared, static}", [
        w("Specify whether to use shared or static linking when"),
        w("linking an executable with Mercury libraries."),
        w("Shared libraries are always linked with"),
        opt("--mercury-linkage shared", ".")])).
optdb(oc_link_c,    only_globals_mercury_linkage,      string("shared"),
    no_help).

optdb(oc_link_c,    demangle,                          bool(yes),
    help("demangle", [
        w("Do not pipe the output of the linker through"),
        w("the Mercury demangler."),
        w("The demangler usually makes it easier to understand"),
        w("the diagnostics for any link errors"),
        w("that involve code generated by the Mercury compiler.")])).
optdb(oc_link_c,    strip,                             bool(yes),
    help("strip", [
        w("Do not invoke the"), code("strip"), w("command on executables."),
        w("Stripping minimizes executables' sizes,"),
        w("but also makes debugging them much harder.")])).
optdb(oc_link_c,    main,                              bool(yes),
    help("main", [
        w("Do not generate a C"), code("main()"), w("function. With"),
        opt("--no-main", ","),
        w("the user's own code must provide a main() function.")])).

optdb(oc_link_c,    allow_undefined,                   bool(yes),
    help("allow-undefined", [
        w("Do not allow undefined symbols in shared libraries.")])).
optdb(oc_link_c,    use_readline,                      bool(yes),
    help("use-readline", [
        w("Disable use of the readline library in the debugger.")])).
optdb(oc_link_c,    runtime_flags,                     accumulating([]),
    % Both the external and internal names of this option
    % should be more descriptive.
    arg_help("runtime-flags", "flags", [
        w("Specify flags to pass to the Mercury runtime.")])).
optdb(oc_link_c,    extra_init_functions,              bool(no),
    alt_help("extra-initialization-functions",
            ["extra-inits"], [
        w("Search"), file(".c"),
        w("files for extra initialization functions."),
        w("(This may be necessary if the C files contain hand-written C code"),
        w("with"), code("INIT"), w("comments, rather than containing"),
        w("only C code that was automatically generated"),
        w("by the Mercury compiler.)")])).
optdb(oc_link_c,    frameworks,                        accumulating([]),
    arg_help("framework", "framework", [
        % XXX Should we s/Mac OS X/Mac OS/g?
        cindex("Mac OS X, Using frameworks"),
        w("Build and link against the specified framework."),
        w("(Mac OS X only.)")])).
optdb(oc_link_c,    framework_directories,             accumulating([]),
    short_arg_help('F', "framework-directory", [], "directory", [
        cindex("Directories for libraries"),
        cindex("Search path for libraries"),
        cindex("Mac OS X, Using frameworks"),
        w("Append the specified directory to the framework search path."),
        w("(Mac OS X only.)")])).
optdb(oc_link_c,    cstack_reserve_size,               int(-1),
    arg_help("cstack-reserve-size", "size", [
        w("Set the total size of the C stack in virtual memory for"),
        w("executables. The stack size is given in bytes."),
        w("This option is only supported (and indeed only necessary)"),
        w("on systems running Microsoft Windows.")])).
optdb(oc_link_c,    link_executable_command,           string("gcc"),
    arg_help("link-executable-command", "command", [
        % XXX s/linking/creating/
        w("Specify the command used to invoke the linker when linking"),
        w("an executable.")])).
optdb(oc_link_c,    link_shared_lib_command,           string("gcc -shared"),
    arg_help("link-shared-lib-command", "command", [
        % XXX s/linking/creating/
        w("Specify the command used to invoke the linker when linking"),
        w("a shared library.")])).
optdb(oc_link_c,    shlib_linker_install_name_path,    string(""),
    arg_help("shlib-linker-install-name-path", "directory", [
        w("Specify the path where a shared library will be installed."),
        w("This option is useful on systems where the runtime search path"),
        w("is obtained from the shared library and not via the"),
        opt("-R"), w("option above (such as Mac OS X).")])).
optdb(oc_link_c,    strip_executable_command,          string(""),
    arg_help("strip-executable-command", "command", [
        w("Specify the command used to strip executables if no linker"),
        w("flag to do so is available."),
        % XXX Why *would* it affect ml?
        w("This option has no effect on"), code("ml", ".")])).
optdb(oc_link_c,    strip_executable_shared_flags,     string(""),
    arg_help("strip-executable-shared-flags", "options", [
        w("Specify options to pass to the strip executable command when"),
        w("linking against Mercury shared libraries.")])).
optdb(oc_link_c,    strip_executable_static_flags,     string(""),
    arg_help("strip-executable-static-flags", "options", [
        w("Specify options to pass to the strip executable command when"),
        w("linking against Mercury static libraries.")])).
optdb(oc_link_c,    shared_lib_not_executable,         bool(no),
    % XXX Improve this documentation.
    % XXX The name of the option is misleading about its purpose.
    priv_help("compile-to-shared-lib", [
        w("This option is intended only for use by the debugger's"),
        w("interactive query facility.")])).

%---------------------%

    % Options that apply only to Java.

optdb(oc_link_java, java_archive_command,              string("jar"),
    arg_help("java-archive-command", "command", [
        w("Specify the command used to produce Java archive (JAR) files.")])).

%---------------------%

    % Options that apply only to C#.

optdb(oc_link_csharp, sign_assembly,                     string(""),
    arg_help("sign-assembly", "keyfile", [
        w("Sign the current assembly with the strong name contained"),
        w("in the specified key file."),
        w("(This option is only meaningful when generating library"),
        w("assemblies with the C# back-end.)")])).

%---------------------------------------------------------------------------%

    % File search options.

optdb(oc_search,    options_search_directories,        accumulating(["."]),
    arg_help("options-search-directory", "directory", [
        w("Add"), arg("directory"), w("to the list of directories"),
        w("to be searched for options files.")])).
optdb(oc_search,    setting_only_use_subdirs,          bool(no),
    help("use-subdirs", [
        cindex("File names"),
        cindex("Directories"),
        cindex("Subdirectories"),
        cindex("@file{Mercury} subdirectory"),
        w("Generate intermediate files in a"), file("Mercury"),
        w("subdirectory,"),
        w("rather than generating them in the current directory.")])).
optdb(oc_search,    setting_only_use_grade_subdirs,    bool(no),
    help("use-grade-subdirs", [
        cindex("File names"),
        cindex("Directories"),
        cindex("Subdirectories"),
        cindex("@file{Mercury} subdirectory"),
        cindex("Grades"),
        w("Generate intermediate files in a"), file("Mercury"),
        w("subdirectory,"),
        w("laid out so that multiple grades can be built simultaneously."),
        w("Executables and libraries will be symlinked or copied into"),
        w("the current directory."),
        opt("--use-grade-subdirs"), w("is supported only by"),
        code("mmc --make", ";"), w("it does not work with"),
        code("mmake", ".")])).
optdb(oc_search,    search_directories,                accumulating(["."]),
    short_arg_help('I', "search-directory", [], "directory", [
        cindex("Directories"),
        cindex("Search path"),
        w("Append"), arg("directory"), w("to the list of directories to be"),
        w("searched for"), file(".int*"), w("and"), file(".module_dep"),
        w("files.")])).
optdb(oc_search,    intermod_directories,              accumulating([]),
    arg_help("intermod-directory", "directory", [
        cindex("Directories"),
        cindex("Search path"),
        w("Add"), arg("directory"), w("to the list of directories to be"),
        w("searched for"), file(".opt"), w("and"), file(".trans_opt"),
        w("files.")])).
optdb(oc_search,    use_search_directories_for_intermod, bool(yes),
    help("use-search-directories-for-intermod", [
        cindex("Directories"),
        cindex("Search path"),
        w("With"), opt("--use-search-directories-for-intermod", ","),
        w("the compiler will add the arguments of"),
        opt("--search-directory"), w("options to the list of directories"),
        w("to search for `.opt' files. With"),
        opt("--no-use-search-directories-for-intermod", ","),
        w("the compiler will use only the arguments of"),
        opt("--intermod-directory"), w("options.")])).
optdb(oc_search,    interface_dirs_same_subdir_setting, accumulating([]),
    priv_alt_arg_help("interface-dir-same-workspace",
        ["interface-dir-same-ws"], "directory", [])).
optdb(oc_search,    interface_dirs_indep_subdir_setting, accumulating([]),
    priv_alt_arg_help("interface-dir-independent-workspace",
        ["interface-dir-indep-ws"], "directory", [])).
optdb(oc_search,    interface_dirs_installed_library,  accumulating([]),
    priv_alt_arg_help("interface-dir-installed-library",
        ["interface-dir-installed-lib"], "directory", [])).
optdb(oc_search,    intermod_dirs_same_subdir_setting, accumulating([]),
    priv_alt_arg_help("intermod-dir-same-workspace",
        ["intermod-dir-same-ws"], "directory", [])).
optdb(oc_search,    intermod_dirs_indep_subdir_setting, accumulating([]),
    priv_alt_arg_help("intermod-dir-independent-workspace",
        ["intermod-dir-indep-ws"], "directory", [])).
optdb(oc_search,    intermod_dirs_installed_library,   accumulating([]),
    priv_alt_arg_help("intermod-dir-installed-library",
        ["intermod-dir-installed-lib"], "directory", [])).
optdb(oc_search,    c_incl_dirs_same_subdir_setting,   accumulating([]),
    priv_alt_arg_help("c-include-dir-same-workspace",
        ["c-incl-dir-same-ws"], "directory", [])).
optdb(oc_search,    c_incl_dirs_indep_subdir_setting,  accumulating([]),
    priv_alt_arg_help("c-include-dir-independent-workspace",
        ["c-incl-dir-indep-ws"], "directory", [])).
optdb(oc_search,    c_incl_dirs_installed_library,     accumulating([]),
    priv_alt_arg_help("c-include-dir-installed-library",
        ["c-incl-dir-installed-lib"], "directory", [])).
optdb(oc_search,    c_incl_dirs_external,              accumulating([]),
    priv_alt_arg_help("c-include-dir-external",
        ["c-incl-dir-external"], "directory", [])).
optdb(oc_search,    mer_lib_dirs_same_subdir_setting,  accumulating([]),
    priv_alt_arg_help("mercury-library-dir-same-workspace",
        ["mer-lib-dir-same-ws"], "directory", [])).
optdb(oc_search,    mer_lib_dirs_indep_subdir_setting, accumulating([]),
    priv_alt_arg_help("mercury-library-dir-independent-workspace",
        ["mer-lib-dir-indep-ws"], "directory", [])).
optdb(oc_search,    mer_lib_dirs_installed_library,    accumulating([]),
    priv_alt_arg_help("mercury-library-dir-installed-library",
        ["mer-lib-dir-installed-lib"], "directory", [])).

%---------------------------------------------------------------------------%

    % Build system options.

optdb(oc_buildsys,  install_prefix,                    string("/usr/local/"),
    arg_help("install-prefix", "directory", [
        w("The directory under which to install Mercury libraries.")])).
% XXX Add more descriptive names for the libgrade* options.
optdb(oc_buildsys,  library_install_grades,           accumulating(["stdlib"]),
    % XXX We should add --library-install-grade as the *preferred* name
    % of this option.
    alt_arg_help("library-grade",
            ["libgrade"], "grade", [
        w("The positive form adds"), arg("grade"), w("to the list of"),
        w("compilation grades in which a library to be installed"),
        w("should be built."),
        w("(The list is initialized to the set of grades in which"),
        w("the standard library is installed.)"),
        w("The negative form clears the list of compilation grades in which"),
        w("a library to be installed should be built.")])).
optdb(oc_buildsys,  library_install_grades_incl_components, accumulating([]),
    alt_arg_help("libgrades-include-component",
            ["libgrades-include"], "grade_component", [
        w("Remove grades that do not contain the specified component from"),
        w("the set of library grades to be installed."),
        w("(This option works only with"), code("mmc --make", ";"),
        w("it does not work with"), code("mmake", ".)")])).
optdb(oc_buildsys,  library_install_grades_excl_components, accumulating([]),
    alt_arg_help("libgrades-exclude-component",
            ["libgrades-exclude"], "grade_component", [
        w("Remove grades that contain the specified component from the"),
        w("set of library grades to be installed."),
        w("(This option works only with"), code("mmc --make", ";"),
        w("it does not work with"), code("mmake", ".)")])).
optdb(oc_buildsys,  only_globals_library_install_linkages, accumulating([]),
    alt_arg_help("library-install-linkage",
            ["library-linkage", "lib-linkage"], "{shared, static}", [
        w("Specify whether libraries should be installed for shared"),
        w("or static linking. This option can be specified multiple times."),
        w("By default, libraries will be installed for"),
        w("both shared and static linking.")])).
optdb(oc_buildsys,  detect_stdlib_grades,              bool(yes),
    alt_help("detect-stdlib-grades",
            ["detect-libgrades"], [
        w("Do not scan the installation directory to determine"),
        w("which standard library grades are available.")])).
optdb(oc_buildsys,  libgrade_install_check,            bool(yes),
    help("libgrade-install-check", [
        w("Do not check that libraries have been installed before"),
        w("attempting to use them. (This option is meaningful only with"),
        code("mmc --make", ".)")])).
optdb(oc_buildsys,  extra_init_command,                maybe_string(no),
    arg_help("extra-init-command", "command", [
        w("Specify a command to produce extra entries in the"),
        file(".init"), w("file for a library."),
        w("The command will be passed the names of all of the source files"),
        w("in the program or library, with the source file"),
        w("containing the main module given first.")])).
optdb(oc_buildsys,  extra_library_header,              accumulating([]),
    alt_arg_help("extra-library-header",
            ["extra-lib-header"], "filename", [
        w("Install the specified C header file along with"),
        w("a Mercury library."),
        w("(This option is supported only by"), code("mmc --make", ".)")])).

%---------------------------------------------------------------------------%

    % Options specifying properties of the environment.

    % If `--mercury-stdlib-dir' is set, `--mercury-config-dir'
    % must also be set. This invariant is maintained by the `special' variants
    % of the options.
optdb(oc_env,       mercury_configuration_directory_special, string_special,
    alt_arg_help("mercury-configuration-directory",
            ["mercury-config-dir"], "directory", [
        w("Search"), arg("directory"),
        w("for Mercury system's configuration files.")])).
optdb(oc_env,       mercury_configuration_directory,   maybe_string(no),
    % "--mercury-config-dir argdir" sets this option to yes(argdir).
    no_help).
optdb(oc_env,       install_command,                   string("cp"),
    arg_help("install-command", "command", [
        w("Specify the command to use to install the files in"),
        w("Mercury libraries. The given command will be invoked as"),
        help_text_texinfo(
            [quote("<command> <source> <target>")],
            [code("@var{command} @var{source} @var{target}")]),
        w("to install each file in a Mercury library."),
        w("The default command is"), code("cp", ".")])).
optdb(oc_env,       options_files,           accumulating(["Mercury.options"]),
    arg_help("options-file", "filename", [
        w("Add"), arg("filename"),
        w("to the list of options files to be processed."),
        w("If"), arg("filename"), w("is"), samp("-", ","),
        w("an options file will be read from the standard input."),
        w("By default, the compiler will read the file named"),
        file("Mercury.options"), w("in the current directory."),
        w("Note that this option is intended to be used"),
        w("only on command lines. When specified in options files"),
        w("(i.e. in files named by other --options-file options),"),
        w("it has no effect.")])).
optdb(oc_env,       config_file,                       maybe_string(yes("")),
    % yes("") means unset.
    arg_help("config-file", "filename", [
        w("Read the Mercury compiler's configuration information from"),
        arg("filename", "."), w("If the"), opt("--config-file"),
        w("option is not set, a default configuration will be used, unless"),
        opt("--no-mercury-stdlib-dir"), w("is passed to"), code("mmc", "."),
        w("The configuration file is just an options file."),
        texinfo_only([xref("Using Mmake", ".")])])).
optdb(oc_env,       env_type,                          string_special,
    arg_help("env-type", "{posix,cygwin,msys,windows}", [
        w("Specify the environment type in which the compiler and"),
        w("generated programs will be invoked."),
        w("This option is equivalent to setting all of"),
        opt("--host-env-type", ","), opt("--system-env-type"), w("and"),
        opt("--target-env-type"), w("to the given environment type.")])).
optdb(oc_env,       host_env_type,                     string("posix"),
    arg_help("host-env-type", "{posix,cygwin,msys,windows}", [
        w("Specify the environment type in which the compiler will be"),
        w("invoked.")])).
optdb(oc_env,       system_env_type,                   string(""),
    arg_help("system-env-type", "{posix,cygwin,msys,windows}", [
        w("Specify the environment type in which external programs invoked"),
        w("by the compiler will run."),
        w("If not specified, this defaults to the value given by"),
        w("`--host-env-type'.")])).
optdb(oc_env,       target_env_type,                   string("posix"),
    arg_help("target-env-type", "{posix,cygwin,msys,windows}", [
        w("Specify the environment type in which generated programs will be"),
        w("invoked.")])).
optdb(oc_env,       restricted_command_line,           bool(no),
    help("restricted-command-line", [
        w("Enable this option if your shell does not support long"),
        w("command lines. This option uses temporary files to pass arguments"),
        w("to sub-commands."),
        w("(This option is supported only by"), code("mmc --make", ".)")])).

%---------------------------------------------------------------------------%

    % Configuration options.

optdb(oc_config,     conf_low_ptag_bits,                int(2),
    % The undocumented conf_low_ptag_bits option is used by the `mmc' script
    % to pass the default value for num_ptag_bits assuming target_c.
    % The reason that `mmc' doesn't just pass a default value for
    % --num-ptag-bits is that users need to be able to override this default
    % when cross compiling.
    priv_alt_arg_help("conf-low-ptag-bits",
            ["conf-low-tag-bits"], "N", [
        w("Reserved for use by the"), code("mmc"), w("script.")])).
optdb(oc_config,    have_delay_slot,                   bool(no),
    % The `mmc' script may override the default if configure says
    % the machine has branch delay slots.
    priv_alt_help("branch-delay-slot", ["have-delay-slot"], [
        w("Assume that branch instructions have a delay slot."),
        w("Note that the value of this option is normally autoconfigured;"),
        w("its use should never be needed except for cross-compilation.")])).
% The `mmc' script will override the next four options' defaults
% with values determined at configuration time.
optdb(oc_config,    num_real_r_regs,                   int(5),
    arg_help("num-real-r-regs", "N", [
        w("Assume registers"), bare_arg("r1"), w("up to"), bare_arg("rN"),
        w("are real (i.e. not virtual)  general purpose registers."),
        w("Note that the value of this option is normally autoconfigured;"),
        w("its use should never be needed except for cross-compilation.")])).
optdb(oc_config,    num_real_f_regs,                   int(0),
    arg_help("num-real-f-regs", "N", [
        w("Assume registers"), bare_arg("f1"), w("up to"), bare_arg("fN"),
        w("are real (i.e. not virtual) floating point registers."),
        w("Note that the value of this option is normally autoconfigured;"),
        w("its use should never be needed except for cross-compilation.")])).
optdb(oc_config,    num_real_r_temps,                  int(5),
    alt_arg_help("num-real-r-temps",
            ["num-real-temps"], "N", [
        w("Assume that"), bare_arg("N"), w("non-float temporaries"),
        w("will fit into real machine registers."),
        w("Note that the value of this option is normally autoconfigured;"),
        w("its use should never be needed except for cross-compilation.")])).
optdb(oc_config,    num_real_f_temps,                  int(0),
    arg_help("num-real-f-temps", "N", [
        w("Assume that"), bare_arg("N"), w("float temporaries"),
        w("will fit into real machine registers."),
        w("Note that the value of this option is normally autoconfigured;"),
        w("its use should never be needed except for cross-compilation.")])).
optdb(oc_config,    max_jump_table_size,               int(0),
    % 0 indicates jump tables can be any size.
    % XXX This option works around limitations in 1998 C compilers.
    % Its value should be set automatically by handle_options.m
    % based on the value of the c_compiler_type option.
    arg_help("max-jump-table-size", "N", [
        w("Specify the maximum number of entries a jump table may have."),
        w("The special value 0 indicates the table size is unlimited."),
        w("This option can be useful to avoid exceeding fixed limits"),
        w("imposed by some C compilers.")])).

%---------------------------------------------------------------------------%

    % Options reserved form Mercury.config files.

optdb(oc_mconfig,   mkinit_command,                    string("mkinit"),
    priv_arg_help("mkinit-command", "command", [])).

optdb(oc_mconfig,   target_arch,                       string(""),
    priv_arg_help("target-arch", "architecture", [])).

optdb(oc_mconfig,   executable_file_extension,         string(""),
    priv_arg_help("executable-file-extension", "extension", [])).
optdb(oc_mconfig,   library_extension,                 string(".a"),
    priv_arg_help("library-extension", "extension", [])).
optdb(oc_mconfig,   shared_library_extension,          string(".so"),
    % The `mmc' script will override the default with a value
    % determined at configuration time.
    % XXX *Which* "configuration time" does this mean?
    priv_arg_help("shared-library-extension", "extension", [])).

optdb(oc_mconfig,   create_archive_command,            string("ar"),
    priv_arg_help("create-archive-command", "command", [])).
optdb(oc_mconfig,   create_archive_command_flags,      accumulating([]), % "cr"
    priv_arg_help("create-archive-command-flags", "flags", [])).
optdb(oc_mconfig,   create_archive_command_output_flag, string(""),
    priv_arg_help("create-archive-command-output-flag", "flag", [])).
optdb(oc_mconfig,   ranlib_command,                    string(""),
    priv_arg_help("ranlib-command", "command", [])).
optdb(oc_mconfig,   ranlib_flags,                      string(""),
    priv_arg_help("ranlib-flags", "flags", [])).

optdb(oc_mconfig,   demangle_command,                  string("mdemangle"),
    priv_arg_help("demangle-command", "command", [])).
optdb(oc_mconfig,   filtercc_command,                  string("mfiltercc"),
    priv_arg_help("filtercc-command", "command", [])).
optdb(oc_config,    filterjavac_command,               string("mfilterjavac"),
    priv_arg_help("filterjavac-command", "command", [])).

optdb(oc_mconfig,   linker_allow_undefined_flag,       string(""),
    priv_arg_help("linker-allow-undefined-flag", "flag", [])).
optdb(oc_mconfig,   linker_debug_flags,                string("-g"),
    priv_arg_help("linker-debug-flags", "flags", [])).
optdb(oc_mconfig,   linker_error_undefined_flag,   string("-Wl,-no-undefined"),
    priv_arg_help("linker-error-undefined-flag", "flag", [])).
optdb(oc_mconfig,   linker_link_lib_flag,              string("-l"),
    priv_arg_help("linker-link-lib-flag", "flag", [])).
optdb(oc_mconfig,   linker_link_lib_suffix,            string(""),
    priv_arg_help("linker-link-lib-suffix", "extension", [])).
optdb(oc_mconfig,   linker_lto_flags,                  string(""),
    priv_arg_help("linker-lto-flags", "flags", [])).
optdb(oc_mconfig,   linker_opt_separator,              string(""),
    priv_arg_help("linker-opt-separator", "separator", [])).
optdb(oc_mconfig,   linker_path_flag,                  string("-L"),
    priv_arg_help("linker-path-flag", "flag", [])).
optdb(oc_mconfig,   linker_rpath_flag,                 string("-Wl,-rpath"),
    priv_arg_help("linker-rpath-flag", "flag", [])).
optdb(oc_mconfig,   linker_rpath_separator,            string(" -Wl,-rpath"),
    priv_arg_help("linker-rpath-separator", "separator", [])).
optdb(oc_mconfig,   linker_sanitizer_flags,            string(""),
    priv_arg_help("linker-sanitizer-flags", "flag", [])).
optdb(oc_mconfig,   linker_static_flags,               string("-static"),
    priv_arg_help("linker-static-flags", "flags", [])).
optdb(oc_mconfig,   linker_strip_flag,                 string("-s"),
    priv_arg_help("linker-strip-flag", "flag", [])).
optdb(oc_mconfig,   linker_thread_flags,               string(""),
    priv_arg_help("linker-thread-flags", "flags", [])).
optdb(oc_mconfig,   linker_trace_flags,                string(""),
    priv_arg_help("linker-trace-flags", "flags", [])).

optdb(oc_mconfig,   shlib_linker_debug_flags,          string("-g"),
    priv_arg_help("shlib-linker-debug-flags", "flags", [])).
optdb(oc_mconfig,   shlib_linker_install_name_flag,   string("-install_name "),
    priv_arg_help("shlib-linker-install-name-flag", "flag", [])).
optdb(oc_mconfig,   shlib_linker_link_lib_flag,        string("-l"),
    priv_arg_help("shlib-linker-link-lib-flag", "flag", [])).
optdb(oc_mconfig,   shlib_linker_link_lib_suffix,      string(""),
    priv_arg_help("shlib-linker-link-lib-suffix", "extension", [])).
optdb(oc_mconfig,   shlib_linker_rpath_flag,           string("-Wl,-rpath"),
    priv_arg_help("shlib-linker-rpath-flag", "flags", [])).
optdb(oc_mconfig,   shlib_linker_rpath_separator,      string(" -Wl,-rpath"),
    priv_arg_help("shlib-linker-rpath-separator", "separator", [])).
optdb(oc_mconfig,   shlib_linker_thread_flags,         string(""),
    priv_arg_help("shlib-linker-thread-flags", "flags", [])).
optdb(oc_mconfig,   shlib_linker_trace_flags,          string(""),
    priv_arg_help("shlib-linker-trace-flags", "flags", [])).
optdb(oc_mconfig,   shlib_linker_use_install_name,     bool(no),
    priv_help("shlib-linker-use-install-name", [])).

optdb(oc_mconfig,   hwloc_libs,                        string(""),
    priv_arg_help("hwloc-libs", "XXX document me", [])).
optdb(oc_mconfig,   hwloc_static_libs,                 string(""),
    priv_arg_help("hwloc-static-libs", "XXX document me", [])).
optdb(oc_mconfig,   math_lib,                          string(""),
    priv_arg_help("math-lib", "library", [])).
optdb(oc_mconfig,   readline_libs,                     string(""),
    priv_arg_help("readline-libs", "XXX document me", [])).
optdb(oc_mconfig,   shared_libs,                       string(""),
    priv_arg_help("shared-libs", "XXX document me", [])).
optdb(oc_mconfig,   thread_libs,                       string(""),
    priv_arg_help("thread-libs", "library", [])).
optdb(oc_mconfig,   trace_libs,                        string(""),
    priv_arg_help("trace-libs", "library", [])).

optdb(oc_mconfig,   install_method,                    string("external"),
    priv_arg_help("install-method", "XXX document me", [])).
optdb(oc_mconfig,   use_symlinks,                      bool(yes),
    priv_help("use-symlinks", [])).

%---------------------------------------------------------------------------%

    % Options for developers only.

%---------------------%

    % Options that developers can use to ask for optional compiler actions.

% XXX Should we move the fact table options to LLDS codegen options?
% XXX Make all other public options in this section private.
optdb(oc_dev_ctrl,  progress_output_suffix,            string(""),
    priv_arg_help("progress-output-suffix", ".xyz", [
        w("When compiling a module, output messages about the"),
        w("progress of the compilation to a file named"),
        file_var("module", "xyz", "."),
        w("This includes any statistics about the performance of"),
        w("compiler passes, if enabled."),
        w("The default is for such output to go to standard error.")])).
optdb(oc_dev_ctrl,  error_output_suffix,               string(""),
    priv_arg_help("error-output-suffix", ".xyz", [
        w("When compiling a module, output any error, warning and/or"),
        w("informational messages about the module to a file named"),
        file_var("module", "xyz", "."),
        w("The default is for such output to go to standard error.")])).
optdb(oc_dev_ctrl,  inference_output_suffix,           string(""),
    priv_arg_help("inference-output-suffix", ".xyz", [
        w("When compiling a module, output the results of any type and/or"),
        w("mode inference to a file named"), file_var("module", "xyz", "."),
        w("The default is for such output to go to standard error.")])).
optdb(oc_dev_ctrl,  debug_output_suffix,               string(""),
    priv_arg_help("debug-output-suffix", ".xyz", [
        w("When compiling a module, direct output that is intended to"),
        w("help debug the compiler to a file named"),
        file_var("module", "xyz", "."),
        w("The default is for such output to go to standard error.")])).
optdb(oc_dev_ctrl,  recompile_output_suffix,           string(""),
    priv_arg_help("recompile-output-suffix", ".xyz", [
        w("This is intended to direct the output from the test cases in"),
        w("tests/recompilation to a file.")])).
optdb(oc_dev_ctrl,  mode_constraints,                  bool(no),
    priv_help("mode-constraints", [
        w("Run constraint based mode analysis. The default is to use"),
        w("the robdd solution using the full (subtyping) constraints,"),
        w("and to dump its results.")])).
optdb(oc_dev_ctrl,  simple_mode_constraints,           bool(no),
    priv_help("simple-mode-constraints", [
        w("Use only the simplified constraint system when running"),
        w("the robdd solver constraints based mode analysis.")])).
optdb(oc_dev_ctrl,  prop_mode_constraints,             bool(no),
    priv_alt_help("propagate-mode-constraints",
            ["prop-mode-constraints"], [
        w("Use the new propagation solver for constraints based"),
        w("mode analysis.")])).
optdb(oc_dev_ctrl,  compute_goal_modes,                bool(no),
    priv_help("compute-goal-modes", [
        w("Compute goal modes.")])).
optdb(oc_dev_ctrl,  smart_recompilation,               bool(no),
    % Even if this option is set to `yes', smart recompilation may have been
    % disabled with io_set_disable_smart_recompilation. Before using the value
    % of this option, call io_get_disable_smart_recompilation to see
    % whether this has been done.
    priv_help("smart-recompilation", [
        w("When compiling, write program dependency information"),
        w("to be used to avoid unnecessary recompilations if"),
        w("the interface of an imported module changes in a way which does"),
        w("not invalidate the compiled code."), opt("--smart-recompilation"),
        w("does not yet work with"), opt("--intermodule-optimization", ".")])).
optdb(oc_dev_ctrl,  pre_prof_transforms_simplify,      bool(no),
    priv_help("pre-prof-transforms-simplify", [
        w("Force the pre-profiling simplification pass that is usually"),
        w("enabled when building a profiling version of a program. This"),
        w("allows a developer to enable this pass when using a"),
        w("non-profiling build. It can be used to test that generated code"),
        w("introduced in earlier passes is well-formed before it is"),
        w("potentially removed by the later"),
        w("dead procedure elimination pass.")])).
optdb(oc_dev_ctrl,  disable_mmsc_pneg,                 bool(no),
    priv_help("disable-mm-pneg", [])).
optdb(oc_dev_ctrl,  disable_mmsc_cut,                  bool(no),
    priv_help("disable-mm-cut", [])).
optdb(oc_dev_ctrl,  disable_trail_ops,                 bool(no),
    priv_help("disable-trail-ops", [
        w("This option can be used to analyze"),
        w("the performance effects of trailing.")])).
optdb(oc_dev_ctrl,  type_check_using_constraints,      bool(no),
    priv_help("type-check-constraints", [
        w("Use the constraint based type checker instead of the old one.")])).
optdb(oc_dev_ctrl,  trad_passes,                       bool(yes),
    priv_help("trad-passes", [
        w("The default"), opt("--trad-passes"), w("completes code generation"),
        w("of each predicate before going on to the next predicate."),
        w("The"), opt("--no-trad-passes"), w("option tells the compiler"),
        w("to complete each phase of code generation on all predicates"),
        w("before going on to the next phase on all predicates.")])).
optdb(oc_dev_ctrl,  parallel_liveness,                 bool(no),
    priv_help("parallel-liveness", [
        w("Use multiple threads when computing liveness."),
        w("At the moment this option implies"), opt("--no-trad-passes", ","),
        w("and requires the compiler to be built in a"),
        w("low-level parallel grade and running with multiple engines.")])).
optdb(oc_dev_ctrl,  parallel_code_gen,                 bool(no),
    priv_help("parallel-code-gen", [
        w("Use multiple threads when generating code."),
        w("At the moment this option implies"), opt("--no-trad-passes", ","),
        w("and requires the compiler to be built in a"),
        w("low-level parallel grade and running with multiple engines.")])).
optdb(oc_dev_ctrl,  should_pretest_equality,           bool(yes),
    priv_help("should-pretest-equality", [
        w("Normally, the compiler adds to the starts of"),
        w("potentially expensive unify and compare predicates"),
        w("a test for the two values being equal as words. Specifying"),
        opt("--no-should-pretest-equality"), w("prevents this.")])).
optdb(oc_dev_ctrl,  fact_table_max_array_size,         int(1024),
    priv_arg_help("fact-table-max-array-size", "N", [
        w("Specify the maximum number of elements in a single"),
        code(":- pragma fact_table"), w("data array (default: 1024).")])).
optdb(oc_dev_ctrl,  fact_table_hash_percent_full,      int(90),
    priv_arg_help("fact-table-hash-percent-full", "percentage", [
        w("Specify how full"), code(":- pragma fact_table"), w("hash tables"),
        w("should be allowed to get. Given as an integer percentage"),
        w("(valid range: 1 to 100, default: 90).")])).
optdb(oc_dev_ctrl,  prefer_switch,                     bool(yes),
    % This option is private because it is not yet useful; currently
    % we don't take advantage of GNU C's computed gotos extension.
    priv_help("prefer-switch", [
        w("Generate code using computed gotos rather than switches."),
        w("This makes the generated code less readable,"),
        w("but potentially slightly more efficient."),
        w("This option is effective only with"),
        opt("--high-level-code", ".")])).
optdb(oc_dev_ctrl,  prefer_while_loop_over_jump_self,  bool(yes),
    % This option is intended for testing and benchmarking.
    priv_help("prefer-while-loop-over-jump-self", [
        w("Generate code for tail-recursive single procedures using an"),
        w("infinite while loop, with tail calls being done by a continue."),
        w("The alternative is a label at the start of the procedure,"),
        w("with tail calls being done by a jump to the label."),
        w("This option is effective only with"),
        opt("--high-level-code", ".")])).
optdb(oc_dev_ctrl,  prefer_while_loop_over_jump_mutual, bool(no),
    priv_help("prefer-while-loop-over-jump-mutual", [
        w("Generate code for tail-recursive-SCCs"),
        w("using an infinite while loop wrapped around a switch,"),
        w("with one switch arm for each procedure in the TSCC,"),
        w("with tail calls being done by setting the value of"),
        w("the switched-on variable and a continue. The alternative is"),
        w("a simple label before the code of each procedure, with tail calls"),
        w("being done by a jump to the label."),
        w("This option is effective only with"),
        opt("--high-level-code", ".")])).
optdb(oc_dev_ctrl,  opt_no_return_calls,               bool(yes),
    % This option provides the fairest test of --optimize-saved-vars-cell.
    priv_help("opt-no-return-calls", [
        w("Do not optimize the stack usage of calls that cannot return.")])).
optdb(oc_dev_ctrl,  compiler_sufficiently_recent,      bool(no),
    priv_alt_help("bug-intermod-2002-06-13", [
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
        "allow-non-contig-for-2025-06-01",
        "subtype-int2-2025-07-07",
        "inrange-2025-10-01",
        "scout-disj-2025-11-15"], [
        w("Is the compiler sufficiently recent to contain the new feature"),
        w("or bugfix referred to by each name?")])).
% These options are provided for use by implementors who want to compare
% a new way of doing something with the old way. The idea is that
% the code that switches between the two ways should consult one (or more)
% of these options and make its decision accordingly.
%
% Experiment[1-5] are booleans; experiment itself is a string.
%
% The intention is that most use of these options is within developer
% workspaces, with rare examples of code using some of these options
% being committed, but only for short lengths of time (a week or two at most;
% enough for all members of a team to try out the experiment).
%
% Of course, a developer could always create a purpose-specific option
% to control their code, but adding an option requires recompiling
% most of the modules in the compiler. Having these options permanently here
% should reduce the need for that.
optdb(oc_dev_ctrl,  experiment,                        string(""),
    priv_arg_help("experiment", "experiment_name", [])).
optdb(oc_dev_ctrl,  experiment1,                       bool(no),
    priv_help("experiment1", [])).
optdb(oc_dev_ctrl,  experiment2,                       bool(no),
    priv_help("experiment2", [])).
optdb(oc_dev_ctrl,  experiment3,                       bool(no),
    priv_help("experiment3", [])).
optdb(oc_dev_ctrl,  experiment4,                       bool(no),
    priv_help("experiment4", [])).
optdb(oc_dev_ctrl,  experiment5,                       bool(no),
    priv_help("experiment5", [])).
optdb(oc_dev_ctrl,  allow_ho_insts_as_modes,           bool(yes),
    priv_help("allow-ho-insts-as-modes", [
        w("Do not allow higher order insts to be used as modes.")])).
optdb(oc_dev_ctrl,  ignore_par_conjunctions,           bool(no),
    priv_help("ignore-par-conjunctions", [
        w("Replace parallel conjunctions with plain ones, this is useful"),
        w("for benchmarking. Note that it does not affect implicit"),
        w("parallelism.")])).
optdb(oc_dev_ctrl,  control_granularity,               bool(no),
    help("control-granularity", [
        w("Don't try to generate more parallelism than the machine can"),
        w("handle, which may be specified at runtime or detected"),
        w("automatically.")])).
optdb(oc_dev_ctrl,  distance_granularity,              int(0),
    priv_arg_help("distance-granularity", "distance", [
        w("Control the granularity of parallel execution using the"),
        w("specified distance value.")])).
optdb(oc_dev_ctrl,  implicit_parallelism,              bool(no),
    priv_help("implicit-parallelism", [
        w("Introduce parallel conjunctions where it could be worthwhile"),
        w("(implicit parallelism) using information generated by"),
        w("mdprof_create_feedback."),
        w("The profiling feedback file can be specified using the"),
        opt("--feedback-file"), w("option.")])).
optdb(oc_dev_ctrl,  feedback_file,                     string(""),
    priv_arg_help("feedback-file", "filename", [
        w("Use the specified profiling feedback file to help make decisions"),
        w("about where to introduce implicit parallelism.")])).
optdb(oc_dev_ctrl,  par_loop_control,                  bool(no),
    priv_help("par-loop-control", [])).
optdb(oc_dev_ctrl,  par_loop_control_keep_tail_rec,    bool(no),
    priv_help("par-loop-control-preserve-tail-recursion", [])).
optdb(oc_dev_ctrl,  optopt_enable_const_struct_poly,   bool_special,
    unnamed_help([
        w("Disable the gathering of constant structures holding"),
        w("typeinfos and typeclass_infos in global_data structures.")])).
optdb(oc_dev_ctrl,  canonicalize_error_path_names,     bool(no),
    priv_help("canonicalize-error-path-names", [
        w("Canonicalize path names in error messages that report"),
        w("being unable to open files, in two ways."),
        w("The first is deleting any initial './' or '.\' prefix;"),
        w("the second is deleting the directory component of any"),
        w("absolute path name, leaving only the within-directory component."),
        w("This option considers any string between single quotes"),
        w("to be a path name.")])).

%---------------------%

% ALL the options in the oc_dev_verb category are private. Keep it that way.

optdb(oc_dev_verb,  detailed_statistics,               bool(no),
    % The only sensible way to use --detailed-statistics, based on
    % --very-verbose, is implemented automatically in handle_options,
    % so users shouldn't need to be aware of it.
    priv_help("detailed-statistics", [
        w("Output more detailed messages about the compiler's"),
        w("time/space usage.")])).
optdb(oc_dev_verb,  benchmark_modes,                   bool(no),
    priv_help("benchmark-modes", [
        w("Benchmark mode analysis, including its experimental version,"),
        w("if it is enabled.")])).
optdb(oc_dev_verb,  benchmark_modes_repeat,            int(1),
    priv_arg_help("benchmark-modes-repeat", "num_repeats", [
        w("The number of times to execute mode analysis, if"),
        opt("--benchmark-modes"), w("is enabled.")])).
optdb(oc_dev_verb,  report_cmd_line_args,              bool(no),
    priv_help("report-cmd-line-args", [
        w("Report the command line arguments.")])).
optdb(oc_dev_verb,  report_cmd_line_args_in_doterr,    bool(no),
    priv_help("report-cmd-line-args-in-doterr", [
        w("Report the command line arguments for compilations whose output"),
        w("mmake normally redirects to a"), file(".err"), w("file.")])).
optdb(oc_dev_verb,  inform_ignored_pragma_errors,      bool(no),
    priv_help("inform-ignored-pragma-errors", [
        w("Print an informational message for each otherwise-ignored error"),
        w("that reports an inability to find the procedure that a pragma"),
        w("refers to.")])).
optdb(oc_dev_verb,  inform_generated_type_spec_pragmas, bool(no),
    priv_help("inform-generated-type-spec-pragmas", [
        w("Print an informational message for each type_spec pragma that"),
        w("the compiler generates to implement"),
        w("a type_spec_constrained_pred pragma.")])).
optdb(oc_dev_verb,  proc_size_statistics,              string(""),
    priv_arg_help("proc-size-statistics", "filename", [
        w("Append information about the size of each procedure"),
        w("in the module in terms of goals and variables"),
        w("to the end of the named file.")])).
optdb(oc_dev_verb,  inst_statistics,                   string(""),
    priv_arg_help("inst-statistics", "filename", [
        w("Append a count of each kind of insts in the procedures in the"),
        w("module to the end of the named file.")])).
optdb(oc_dev_verb,  print_error_spec_id,               bool(no),
    priv_help("print-error-spec-id", [
        w("After each error message is printed, print its id, which"),
        w("by convention is the $pred of the code that constructs it.")])).
optdb(oc_dev_verb,  debug_types,                       bool(no),
    priv_short_help('T', "debug-types", [], [
        w("Output detailed debugging traces of type checking."),
        w("Effective only with the right trace flags.")])).
optdb(oc_dev_verb,  debug_types_pred_name,             accumulating([]),
    priv_arg_help("debug-types-pred-name", "pred_or_func_name", [
        w("Output detailed debugging traces of type checking only"),
        w("for predicates and functions named by one of these options.")])).
optdb(oc_dev_verb,  debug_type_rep,                    bool(no),
    priv_help("debug-type-rep", [
        w("Output debugging traces of type representation choices.")])).
optdb(oc_dev_verb,  debug_modes,                       bool(no),
    priv_short_help('N', "debug-modes", [], [
        w("Output debugging traces of the mode checking.")])).
optdb(oc_dev_verb,  debug_modes_verbose,               bool(no),
    priv_help("debug-modes-verbose", [
        w("Output detailed debugging traces of the mode checking.")])).
optdb(oc_dev_verb,  debug_modes_minimal,               bool(no),
    priv_help("debug-modes-minimal", [
        w("Output only minimal debugging traces of the mode checking.")])).
optdb(oc_dev_verb,  debug_modes_statistics,            bool(no),
    priv_help("debug-modes-statistics", [
        w("Output statistics after each step of mode checking.")])).
optdb(oc_dev_verb,  debug_modes_delay_vars,            bool(yes),
    priv_help("debug-modes-delay-vars", [
        w("Output info about the variables involved in delayed goals.")])).
optdb(oc_dev_verb,  debug_modes_goal_ids,              bool(yes),
    priv_help("debug-modes-goal-ids", [
        w("Output the id of the goal at all mode debug checkpoints.")])).
optdb(oc_dev_verb,  debug_modes_pred_id,               int(-1),
    priv_arg_help("debug-modes-pred-id", "pred_id", [
        w("With"), opt("--debug-modes", ","),
        w("restrict the debugging traces to the mode checking"),
        w("of the predicate or function with the specified pred id.")])).
optdb(oc_dev_verb,  debug_mode_constraints,            bool(no),
    priv_help("debug-mode-constraints", [
        w("Output detailed debugging traces of the `--prop-mode-constraints'"),
        w("option.")])).
optdb(oc_dev_verb,  debug_det,                         bool(no),
    priv_alt_help("debug-determinism", ["debug-det"], [
        w("Output detailed debugging traces of determinism analysis.")])).
optdb(oc_dev_verb,  debug_common_struct_preds,         string(""),
    priv_arg_help("debug-common-struct-preds", "predids", [
        w("Limit common struct optimization to the preds with"),
        w("the given ids.")])).
optdb(oc_dev_verb,  debug_closure,                     bool(no),
    % This can be make public together with the '--analyse-closures' option.
    priv_help("debug-closure", [
        w("Output detailed debugging traces of the closure analysis.")])).
optdb(oc_dev_verb,  debug_term,                        bool(no),
    % The new termination analyser is currently a work-in-progress.
    priv_alt_help("debug-termination", ["debug-term"], [
        w("Output detailed debugging traces of the termination2 analysis.")])).
optdb(oc_dev_verb,  debug_dead_proc_elim,              bool(no),
    priv_help("debug-dead-proc-elim", [
        w("Output the needed-entity-map generated by dead procedure"),
        w("elimination.")])).
optdb(oc_dev_verb,  debug_higher_order_specialization, bool(no),
    priv_help("debug-higher-order-specialization", [
        w("Output messages about the procedure specializations done"),
        w("by higher_order.m.")])).
optdb(oc_dev_verb,  debug_pd,                          bool(no),
    priv_help("debug-pd", [
        w("Output detailed debugging traces of the partial deduction"),
        w("and deforestation process.")])).
optdb(oc_dev_verb,  debug_indirect_reuse,              bool(no),
    priv_help("debug-indirect-reuse", [
        w("Output detailed debugging traces of the indirect reuse pass"),
        w("of the"), opt("--structure-reuse"), w("option.")])).
optdb(oc_dev_verb,  debug_trail_usage,                 bool(no),
    priv_help("debug-trail-usage", [
        w("Output detailed debugging traces of the"),
        opt("--analyse-trail-usage"), w("option.")])).
optdb(oc_dev_verb,  debug_unneeded_code,               bool(no),
    priv_help("debug-unneeded-code", [
        w("Print progress messages during the unneeded code elimination"),
        w("passes.")])).
optdb(oc_dev_verb,  debug_unneeded_code_pred_name,     accumulating([]),
    priv_arg_help("debug-unneeded-code-pred-name", "predname", [
        w("Print the definition of <predname> at the start of each pass"),
        w("of the unneeded code elimination algorithm.")])).
optdb(oc_dev_verb,  debug_mm_tabling_analysis,         bool(no),
    priv_help("debug-mm-tabling-analysis", [])).
optdb(oc_dev_verb,  debug_dep_par_conj,                accumulating([]),
    priv_arg_help("debug-dep-par-conj", "pred_id", [
        w("Output detailed debugging traces during the dependent"),
        w("AND-parallelism transformation of the predicate with the given"),
        w("predicate id. Effective only with the right trace flags.")])).
optdb(oc_dev_verb,  debug_liveness,                    int(-1),
    priv_arg_help("debug-liveness", "pred_id", [
        w("Output detailed debugging traces of the liveness analysis"),
        w("of the predicate with the given predicate id.")])).
optdb(oc_dev_verb,  debug_stack_opt,                   int(-1),
    priv_arg_help("debug-stack-opt", "pred-id", [
        w("Generate debug messages when performing stack slot optimization"),
        w("on the predicate with the given id.")])).
optdb(oc_dev_verb,  debug_code_gen_pred_id,            int(-1),
    priv_arg_help("debug-code-gen-pred-id", "pred_id", [
        w("Output detailed debugging traces of code generation for the"),
        w("predicate or function with the given pred id."),
        w("Effectively only with the right trace flags.")])).
optdb(oc_dev_verb,  debug_opt,                         bool(no),
    priv_help("debug-opt", [
        w("Output detailed debugging traces of the optimization process.")])).
optdb(oc_dev_verb,  debug_opt_pred_id,                 accumulating([]),
    priv_arg_help("debug-opt-pred-id", "pred_id", [
        w("Output detailed debugging traces of the optimization process"),
        w("only for the predicate/function with the specified pred id.")])).
optdb(oc_dev_verb,  debug_opt_pred_name,               accumulating([]),
    priv_arg_help("debug-opt-pred-name", "name", [
        w("Output detailed debugging traces of the optimization process"),
        w("only for the predicate/function with the specified name.")])).
optdb(oc_dev_verb,  debug_make,                        bool(no),
    priv_help("debug-make", [
        w("Output detailed debugging traces of the operation of the"),
        opt("--make"), w("option.")])).
optdb(oc_dev_verb,  debug_intermodule_analysis,        bool(no),
    priv_help("debug-intermodule-analysis", [
        w("Output detailed debugging traces of the operation of the"),
        opt("--intermodule-analysis"), w("option.")])).

%---------------------%

    % Options for helping to debug the compiler's operations.
    % Most such options are in option class oc_dev_verb; the options here
    % are the ones that do NOT work by adding to the compiler output stream.

optdb(oc_dev_debug, table_debug,                       bool(no),
    priv_help("table-debug", [
        w("Enables the generation of code that helps to debug tabling"),
        w("primitives.")])).
optdb(oc_dev_debug, debug_class_init,                  bool(no),
    priv_help("debug-class-init", [
        w("In Java grades, generate code that causes a trace of class"),
        w("initialization to be printed to the standard output when the"),
        w("environment variable"), env("MERCURY_DEBUG_CLASS_INIT"),
        w("is defined.")])).

%---------------------%

    % Options for creating dumps of the compiler's internal data structures.

% XXX Make most options in this section private.
optdb(oc_dev_dump,  dump_hlds,                         accumulating([]),
    short_arg_help('d', "dump-hlds", ["hlds-dump"], "stage number or name", [
        w("Dump the HLDS (high level intermediate representation) after"),
        w("the specified stage to"),
        help_text_texinfo(
            [quote("<module>.hlds_dump.<num>-<name>", ".")],
            [fixed("@samp{module}.hlds_dump.@samp{num}-@samp{name}.")]),
        w("Stage numbers range from 1-599."),
        w("Multiple dump options accumulate.")])).
optdb(oc_dev_dump,  dump_hlds_pred_id,                 accumulating([]),
    priv_arg_help("dump-hlds-pred-id", "pred_id", [
        w("Dump the HLDS only of the predicate/function with the given"),
        w("pred id.")])).
optdb(oc_dev_dump,  dump_hlds_pred_name,               accumulating([]),
    arg_help("dump-hlds-pred-name", "name", [
        w("Dump the HLDS only of the predicate/function with the given"),
        w("name.")])).
optdb(oc_dev_dump,  dump_hlds_pred_name_order,         bool(no),
    priv_help("dump-hlds-pred-name-order", [
        w("Dump the predicates in the HLDS ordered by name,"),
        w("not ordered by pred id.")])).
optdb(oc_dev_dump,  dump_hlds_spec_preds,              bool(no),
    priv_help("dump-hlds-spec-preds", [
        w("With"), opt("--dump-hlds", ","), w("dump the special"),
        w("(unify, compare, and index) predicates not in pred-id order,"),
        w("but in alphabetical order by type constructor.")])).
optdb(oc_dev_dump,  dump_hlds_spec_preds_for,          accumulating([]),
    priv_arg_help("dump-hlds-spec-preds-for", "typename", [
        w("Dump the special (unify, compare, and index) predicates"),
        w("only for the types named by the (possibly multiple) occurrences"),
        w("of this option.")])).
optdb(oc_dev_dump,  dump_hlds_alias,                   string(""),
    priv_short_arg_help('D', "dump-hlds-alias", [], "dump-alias", [
        w("With"), opt("--dump-hlds", ","), w("include extra detail"),
        w("in the dump. Each dump alias is shorthand for a set of"),
        w("option letters. The list of aliases is in handle_options.m.")])).
optdb(oc_dev_dump,  dump_hlds_options,                 string(""),
    arg_help("dump-hlds-options", "options", [
        w("With"), opt("--dump-hlds", ","), w("include extra detail"),
        w("in the dump. Each type of detail is included in the dump if its"),
        w("corresponding letter occurs in the option argument"),
        w("(see the Mercury User's Guide for details).")])).
optdb(oc_dev_dump,  dump_hlds_inst_limit,              int(100),
    priv_arg_help("dump-hlds-inst-limit", "N", [
        w("Dump at most N insts in each inst table.")])).
optdb(oc_dev_dump,  dump_hlds_inst_size_limit,         int(40),
    priv_arg_help("dump-hlds-inst-size-limit", "N", [
        w("Dump insts in an inst table"),
        w("only if their size does not exceed N.")])).
optdb(oc_dev_dump,  dump_hlds_file_suffix,             string(""),
    priv_arg_help("dump-hlds-file-suffix", "suffix", [
        w("Append the given suffix to the names of the files created by the"),
        opt("--dump-hlds"), w("option.")])).
optdb(oc_dev_dump,  dump_same_hlds,                    bool(no),
    help("dump-same-hlds", [
        w("Create a file for a HLDS stage even if the file notes only that"),
        w("this stage is identical to the previously dumped HLDS stage.")])).
optdb(oc_dev_dump,  dump_mlds,                         accumulating([]),
    priv_alt_arg_help("dump-mlds", ["mlds-dump"], "stage number or name", [
        w("Dump the MLDS (medium level intermediate representation)"),
        w("after the specified stage, as C code, to"),
        help_text_texinfo(
            [quote("<module>.c_dump.<num>-<name>"), w("and"),
            quote("<module>.mih_dump.<num>-<name>", ".")],
            [fixed("@samp{module}.c_dump.@samp{num}-@samp{name}"), w("and"),
            fixed("@samp{module}.mih_dump.@samp{num}-@samp{name}.")]),
        w("Stage numbers range from 1-99."),
        w("Multiple dump options accumulate."),
        w("This option works only in MLDS grades that target C.")])).
optdb(oc_dev_dump,  dump_mlds_pred_name,               accumulating([]),
    priv_arg_help("dump-mlds-pred-name", "pred or func name", [
        w("Dump the MLDS (medium level intermediate representation)"),
        w("of the predicate or function with the specified name"),
        w("at the stages specified by the"), opt("--dump-mlds"), w("option."),
        w("The dump file will consist of the predicates and functions"),
        w("named by all the occurrences of this option (there may be"),
        w("more than one), and nothing else.")])).
optdb(oc_dev_dump,  verbose_dump_mlds,                 accumulating([]),
    priv_alt_arg_help("verbose-dump-mlds", ["verbose-mlds-dump"],
            "stage number or name", [
        w("Dump the internal compiler representation of the MLDS, after"),
        w("the specified stage, to"),
        help_text_texinfo(
            [quote("<module>.mlds_dump.<num>-<name>", ".")],
            [fixed("@samp{module}.mlds_dump.@samp{num}-@samp{name}.")]),
        w("This option works in all MLDS grades.")])).
optdb(oc_dev_dump,  dump_trace_counts,                 accumulating([]),
    % This option is for developers only.
    priv_arg_help("dump-trace-counts", "stage number or name", [
        w("If the compiler was compiled with debugging enabled and is being"),
        w("run with trace counting enabled, write out the trace counts file"),
        w("after the specified stage to"),
        help_text_texinfo(
            [quote("<module>.trace_counts.<num>-<name>", ".")],
            [fixed("@samp{module}.trace_counts.@samp{num}-@samp{name}.")]),
        w("Stage numbers range from 1-599."),
        w("Multiple dump options accumulate.")])).
optdb(oc_dev_dump,  dump_options_file,                 string(""),
    priv_arg_help("dump-options-file", "output_file", [
        w("Dump the internal compiler representation of files named in"),
        w("options-file options to output_file.")])).

%---------------------%

    % Miscellaneous internal-use-only options.
    %
    % XXX NO, MOST ARE NOT.
    % We need a separate category (maybe named oc_int_dev?) for options
    % - whose main intended use is internal-use-only,
    % - but which can be command-line enabled by developers for experiments.
    % OR we could just delete the possibility of those experiments,
    % whose time has long passed.
    %
    % For some of these options, setting them to non-default values
    % can result in programs that do not link, or programs that dump core.

optdb(oc_internal,  pre_implicit_parallelism_simplify, bool(no),
    priv_help("pre-implicit-parallelism-simplify", [
        w("Run the simplification pass before the implicit parallelism pass"),
        w("to ensure that the HLDS"),
        w("more closely matches the feedback data.")])).
optdb(oc_internal,  type_layout,                       bool(yes),
    priv_help("type-layout", [
        w("Don't output type_ctor_layout structures or references to them."),
        w("(The C code must then be compiled with"),
        code("-DNO_TYPE_LAYOUT", ".)")])).
% The next two options are not documented,
% because they are not yet tested much, and are probably not very useful,
% except for Java, where they are the default.
optdb(oc_internal,  det_copy_out,                      bool(no),
    priv_help("det-copy-out", [
        w("Specify whether to handle output arguments for det/semidet"),
        w("procedures using return-by-value rather than pass-by-reference."),
        w("This option is effective only with"),
        opt("--high-level-code", ".")])).
optdb(oc_internal,  nondet_copy_out,                   bool(no),
    priv_help("nondet-copy-out", [
        w("Specify whether to handle output arguments for nondet"),
        w("procedures using pass-by-value rather than pass-by-reference."),
        w("This option is effective only with"),
        opt("--high-level-code", ".")])).
% The --put-commit-in-own-func option is not documented because
% it is enabled automatically (by handle_options) in the situations
% where it is needed; the user should never need to set it.
optdb(oc_internal,  put_commit_in_own_func,            bool(no),
    priv_help("put-commit-in-own-func", [
        w("Put each commit in its own C function."),
        w("This option only affects the MLDS back-ends."),
        w("It is needed for the high-level C back-end, where"),
        w("commits are implemented via"), code("setjmp()/longjmp()", ","),
        w("since"), code("longjmp()"), w("may clobber any"),
        w("non-volatile local vars in the function that called"),
        code("setjmp()", ".")])).
optdb(oc_internal,  backend_foreign_languages,         accumulating([]),
    % The foreign programming languages that this backend can interface to.
    % The backend_foreign_languages option depends on the target,
    % and is set in handle_options, BUT can be set on the command line.
    % It makes no sense to do so, but ...
    priv_arg_help("backend-foreign-languages", "{c/c#/csharp/java}", [])).
optdb(oc_internal,  stack_trace,                       bool(no),
    unnamed_help([
        w("Generate the stack layout information required to do"),
        w("a stack trace.")])).
optdb(oc_internal,  basic_stack_layout,                bool(no),
    priv_help("basic-stack-layout", [
        w("Generate the simple stack_layout structures required"),
        w("for stack traces.")])).
optdb(oc_internal,  agc_stack_layout,                  bool(no),
    priv_help("agc-stack-layout", [
        w("Generate the stack_layout structures required for"),
        w("accurate garbage collection.")])).
optdb(oc_internal,  procid_stack_layout,               bool(no),
    priv_help("procid-stack-layout", [
        w("Generate the stack_layout structures required for"),
        w("looking up procedure identification information.")])).
optdb(oc_internal,  trace_stack_layout,                bool(no),
    priv_help("trace-stack-layout", [
        w("Generate the stack_layout structures required for"),
        w("execution tracing.")])).
% Use an alternate calculation of liveness where the typeinfo for
% a type variable must live at any point in the body of the procedure
% at which a live variable's type includes that type variable.
%
% Although this option governs whether the body of a procedure uses
% this liveness calculation, it is not the only consideration we have to
% take into account when deciding on the interface of any procedure
% whose address may be taken. We must include typeinfos describing
% the types of all arguments in the interface of a procedure if either
% this option is set *or* the procedure's address may be taken, otherwise,
% the layout structure we include in closures using that procedure
% may not have all the information required to reconstruct the types
% of all the values inside the closure.
%
% The only place in the compiler that should look at this option is
% the predicate body_should_use_typeinfo_liveness in hlds_pred.m;
% everything else, including the predicates deciding interface
% typeinfo liveness, should go through there.
optdb(oc_internal,  body_typeinfo_liveness,            bool(no),
    priv_help("body-typeinfo-liveness", [
        w("Ensure that whenever a variable whose type contains"),
        w("a type variable is live, the type_info for that type variable"),
        w("is available.")])).
% Should be set to yes if the target back end guarantees that
% comparing two values for equality, at least one of which is a constant,
% can be done by casting them both to integers and comparing the integers
% for equality.
optdb(oc_internal,  can_compare_constants_as_ints,     bool(no),
    priv_help("can-compare-constants-as-ints", [])).
% Should be set to yes if the test of whether two input arguments
% are object identical should be done by casting the arguments to a
% generic pointer type. Otherwise they will be cast to integers.
optdb(oc_internal,  pretest_equality_cast_pointers,    bool(no),
    priv_help("pretest-equality-cast-pointers", [])).
optdb(oc_internal,  delay_partial_instantiations,      bool(no),
    priv_help("delay-partial-instantiations", [])).

optdb(oc_internal,  allow_defn_of_builtins,            bool(no),
    priv_help("allow-defn-of-builtins", [
        w("Do not generate errors for definitions of builtin predicates."),
        w("When a new builtin is introduced, the installed compiler won't"),
        w("know about it, and thus when it sees its declaration, it wants a"),
        w("definition, but when the modified compiler is bootstrapped,"),
        w("it would normally generate an error when it sees that very same"),
        w("definition in the library (usually in builtin.m or"),
        w("private_builtin.m). When this option is set, it allows such"),
        w("definitions. Once the modified compiler is installed on all"),
        w("relevant machines, the option can be turned off again.")])).
% optdb(oc_internal, special_preds,                   bool(yes),
%   priv_help("allow-defn-of-builtins", [
%       "Do not generate unify and compare preds. For measurement only.",
%       "Code generated with this set to `no' is unlikely to actually work.",
%       "Disabled to allow the code paths for generating special preds",
%       "to be simplified. If this option is ever needed again, which",
%       "I (zs) do not think is likely, it should be implemented",
%       "differently: by generating the special predicates, and then",
%       "not writing them out. The logic for *that* should be a lot",
%       "simpler."])).
optdb(oc_internal,  type_ctor_info,                    bool(yes),
    priv_help("type-ctor-info", [
        w("Do not generate type_ctor_info structures. For measurement only;"),
        w("if you turn this off, then you will not be able to link.")])).
optdb(oc_internal,  type_ctor_layout,                  bool(yes),
    priv_help("type-ctor-layout", [
        w("Do not generate type_ctor_layout structures."),
        w("For measurement only;"),
        w("if you turn this off, then you will not to be able to link.")])).
optdb(oc_internal,  type_ctor_functors,                bool(yes),
    priv_help("type-ctor-functors", [
        w("Do not generate type_ctor_functors structures."),
        w("For measurement only;"),
        w("if you turn this off, then you will not to be able to link.")])).
optdb(oc_internal,  rtti_line_numbers,                 bool(yes),
    priv_help("rtti-line-numbers", [
        w("Generate line number information in the RTTI when debugging is"),
        w("enabled. For measurement only; if you turn this off, then the"),
        w("debugger may dereference garbage pointers.")])).
optdb(oc_internal,  new_type_class_rtti,               bool(no),
    priv_help("new-type-class-rtti", [
        w("Generate of new style static data structures"),
        w("for runtime information about type classes."),
        w("These are not yet used. When we add code to generate the matching"),
        w("dynamic data structures and switch over to use them, we will not"),
        w("need this option anymore.")])).
% These two options are used to analyze the performance effects
% of minimal model tabling.
optdb(oc_internal,  use_mmsc_pneg,                     bool(no), no_help).
optdb(oc_internal,  use_mmsc_cut,                      bool(no), no_help).
    % The size_* values below *must* be consistent with the corresponding
    % values or data structures in runtime/mercury_region.h.
optdb(oc_internal,  size_region_ite_fixed,             int(4),
    priv_arg_help("size-region-ite-fixed", "num_words", [])).
optdb(oc_internal,  size_region_disj_fixed,            int(4),
    priv_arg_help("size-region-disj-fixed", "num_words", [])).
optdb(oc_internal,  size_region_commit_fixed,          int(5),
    priv_arg_help("size-region-commit-fixed", "num_words", [])).
optdb(oc_internal,  size_region_ite_protect,           int(1),
    priv_arg_help("size-region-ite-protect", "num_words", [])).
optdb(oc_internal,  size_region_ite_snapshot,          int(3),
    priv_arg_help("size-region-ite-snapshot", "num_words", [])).
optdb(oc_internal,  size_region_semi_disj_protect,     int(1),
    priv_arg_help("size-region-semi-disj-protect", "num_words", [])).
optdb(oc_internal,  size_region_disj_snapshot,         int(3),
    priv_arg_help("size-region-disj-snapshot", "num_words", [])).
optdb(oc_internal,  size_region_commit_entry,          int(1),
    priv_arg_help("size-region-commit-entry", "num_words", [])).
optdb(oc_internal,  reclaim_heap_on_failure,           bool_special,
    priv_help("reclaim-heap-on-failure", [
        w("Combines the effect of the two options below.")])).
optdb(oc_internal,  reclaim_heap_on_semidet_failure,   bool(yes),
    priv_help("reclaim-heap-on-semidet-failure", [
        w("Do not reclaim heap on backtracking in semidet code.")])).
optdb(oc_internal,  reclaim_heap_on_nondet_failure,    bool(yes),
    priv_help("reclaim-heap-on-nondet-failure", [
        w("Do not reclaim heap on backtracking in nondet code.")])).
optdb(oc_internal,  max_specialized_do_call_closure,   int(5), no_help).
    % mercury.do_call_closure_N exists for N <= option_value;
    % set to -1 to disable. Should be less than or equal to
    % max_spec_explicit_arg in tools/make_spec_ho_call.
optdb(oc_internal,  max_specialized_do_call_class_method, int(6), no_help).
    % mercury.do_call_class_method_N exists for N <= option_value;
    % set to -1 to disable. Should be less than or equal to
    % max_spec_explicit_arg in tools/make_spec_method_call.
optdb(oc_internal,  compare_specialization,            int(-1),
    % -1 asks handle_options.m to give the value, which may be grade dependent.
    priv_arg_help("compare-specialization", "N", [
        w("Generate quadratic instead of linear compare predicates for"),
        w("types with up to N function symbols. Higher values of N lead to"),
        w("faster but also bigger compare predicates.")])).
optdb(oc_internal,  chosen_stdlib_dir,                 maybe_string(no),
    no_help).
optdb(oc_internal,  default_globals,                   bool(no),
    unnamed_help([
        w("If set to 'yes', default_globals tells the main body of"),
        w("handle_options.m that it is constructing the"), emph("default"),
        w("globals, after the initial construction of the"),
        emph("intended"), w("globals failed.")])).
optdb(oc_internal,  local_module_id,                   accumulating([]),
    priv_arg_help("local-module-id", "XXX document me", [])).
optdb(oc_internal,  generate_item_version_numbers,     bool(no),
    unnamed_help([
        w("This option is used to control output of version numbers"),
        w("in interface files. It is implied by"),
        opt("--smart-recompilation", ","),
        w("and cannot be set explicitly by the user."),
        w("Even if this option is set to"), quote("yes", ","),
        w("version numbers may have been disabled"),
        w("with io_set_disable_generate_item_version_numbers."),
        w("Before using the value of this option, call"),
        w("io_get_disable_generate_item_version_numbers to see whether this"),
        w("has been done.")])).
% XXX reclassify this option, but to oc_opmode, or oc_output_mod?
optdb(oc_internal,  generate_mmc_make_module_dependencies, bool(no),
    alt_help("generate-mmc-make-module-dependencies",
            ["generate-mmc-deps"], [
        w("Generate dependencies for use by"), code("mmc --make"),
        w("even when using"), code("mmake", "."),
        w("This is recommended when building a library"),
        w("for installation.")])).
optdb(oc_internal,  optopt_use_static_ground_floats,   bool_special, no_help).
optdb(oc_internal,  optopt_use_static_ground_int64s,   bool_special, no_help).
optdb(oc_internal,  optopt_use_static_code_addresses,  bool_special, no_help).

%---------------------------------------------------------------------------%

    % Options that the compiler does not use anymore.

optdb(oc_unused,    ansi_c,                            bool(yes),
    help("ansi-c", [
        w("This option is deprecated and does not have any effect.")])).
optdb(oc_unused,    cflags_for_ansi,                   string(""),
    priv_arg_help("cflags-for-ansi", "flags", [])).
optdb(oc_unused,    install_command_dir_option,        string("-R"),
    priv_arg_help("install-command-dir-option", "flag", [])).
optdb(oc_unused,    optopt_use_just_one_c_func,        bool_special,
    alt_help("everything-in-one-c-function",
            ["everything-in-one-C-function"], [
        w("This option is deprecated and does not have any effect.")])).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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
        ( Help = short_alt_align_help(Short, Long1, Longs, _, _, _)
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
        ; Help = no_align_help(Long, _, _, _, _, _)
        ),
        insert_long(Opt, Long, !LongPairs)
    ;
        ( Help = alt_help(Long1, Longs, _)
        ; Help = priv_alt_help(Long1, Longs, _)
        ; Help = alt_align_help(Long1, Longs, _, _, _)
        ; Help = priv_alt_align_help(Long1, Longs, _, _, _)
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
            DodgyOptions = dodgy_code_warning_bool_options,
            SlowOptions = slow_code_warning_bool_options,
            StyleOptions = style_warning_bool_options,
            InformOptions = info_request_bool_options,
            set_all_options_to(DodgyOptions,  bool(Enable), !OptionTable),
            set_all_options_to(SlowOptions,   bool(Enable), !OptionTable),
            set_all_options_to(StyleOptions,  bool(Enable), !OptionTable),
            set_all_options_to(InformOptions, bool(Enable), !OptionTable)
        ;
            Option = inhibit_style_warnings,
            SpecialData = bool(Inhibit),
            bool.not(Inhibit, Enable),
            StyleOptions = style_warning_bool_options,
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
            Option = optopt_enable_const_struct_poly,
            SpecialData = bool(Bool),
            OptOption = oo_enable_const_struct_poly(Bool)
        ;
            Option = optopt_enable_const_struct_user,
            SpecialData = bool(Bool),
            OptOption = oo_enable_const_struct_user(Bool)
        ;
            Option = optopt_opt_common_structs,
            SpecialData = bool(Bool),
            OptOption = oo_opt_common_structs(Bool)
        ;
            Option = optopt_prop_constraints,
            SpecialData = bool(Bool),
            OptOption = oo_prop_constraints(Bool)
        ;
            Option = optopt_prop_local_constraints,
            SpecialData = bool(Bool),
            OptOption = oo_prop_local_constraints(Bool)
        ;
            Option = optopt_opt_dup_calls,
            SpecialData = bool(Bool),
            OptOption = oo_opt_dup_calls(Bool)
        ;
            Option = optopt_prop_constants,
            SpecialData = bool(Bool),
            OptOption = oo_prop_constants(Bool)
        ;
            Option = optopt_elim_excess_assigns,
            SpecialData = bool(Bool),
            OptOption = oo_elim_excess_assigns(Bool)
        ;
            Option = optopt_merge_code_after_switch,
            SpecialData = bool(Bool),
            OptOption = oo_merge_code_after_switch(Bool)
        ;
            Option = optopt_opt_format_calls,
            SpecialData = bool(Bool),
            OptOption = oo_opt_format_calls(Bool)
        ;
            Option = optopt_split_switch_arms,
            SpecialData = bool(Bool),
            OptOption = oo_split_switch_arms(Bool)
        ;
            Option = optopt_opt_loop_invariants,
            SpecialData = bool(Bool),
            OptOption = oo_opt_loop_invariants(Bool)
        ;
            Option = optopt_opt_saved_vars_const,
            SpecialData = bool(Bool),
            OptOption = oo_opt_saved_vars_const(Bool)
        ;
            Option = optopt_opt_svcell,
            SpecialData = bool(Bool),
            OptOption = oo_opt_svcell(Bool)
        ;
            Option = optopt_opt_svcell_loop,
            SpecialData = bool(Bool),
            OptOption = oo_opt_svcell_loop(Bool)
        ;
            Option = optopt_opt_svcell_full_path,
            SpecialData = bool(Bool),
            OptOption = oo_opt_svcell_full_path(Bool)
        ;
            Option = optopt_opt_svcell_on_stack,
            SpecialData = bool(Bool),
            OptOption = oo_opt_svcell_on_stack(Bool)
        ;
            Option = optopt_opt_svcell_candidate_headvars,
            SpecialData = bool(Bool),
            OptOption = oo_opt_svcell_candidate_headvars(Bool)
        ;
            Option = optopt_opt_svcell_all_candidates,
            SpecialData = bool(Bool),
            OptOption = oo_opt_svcell_all_candidates(Bool)
        ;
            Option = optopt_delay_constructs,
            SpecialData = bool(Bool),
            OptOption = oo_delay_constructs(Bool)
        ;
            Option = optopt_opt_follow_code,
            SpecialData = bool(Bool),
            OptOption = oo_opt_follow_code(Bool)
        ;
            Option = optopt_opt_unused_args,
            SpecialData = bool(Bool),
            OptOption = oo_opt_unused_args(Bool)
        ;
            Option = optopt_opt_unused_args_intermod,
            SpecialData = bool(Bool),
            OptOption = oo_opt_unused_args_intermod(Bool)
        ;
            Option = optopt_opt_higher_order,
            SpecialData = bool(Bool),
            OptOption = oo_opt_higher_order(Bool)
        ;
            Option = optopt_opt_unneeded_code,
            SpecialData = bool(Bool),
            OptOption = oo_opt_unneeded_code(Bool)
        ;
            Option = optopt_spec_types,
            SpecialData = bool(Bool),
            OptOption = oo_spec_types(Bool)
        ;
            Option = optopt_spec_types_user_guided,
            SpecialData = bool(Bool),
            OptOption = oo_spec_types_user_guided(Bool)
        ;
            Option = optopt_introduce_accumulators,
            SpecialData = bool(Bool),
            OptOption = oo_introduce_accumulators(Bool)
        ;
            Option = optopt_opt_lcmc_accumulator,
            SpecialData = bool(Bool),
            OptOption = oo_opt_lcmc_accumulator(Bool)
        ;
            Option = optopt_opt_lcmc_null,
            SpecialData = bool(Bool),
            OptOption = oo_opt_lcmc_null(Bool)
        ;
            Option = optopt_opt_lcmc,
            SpecialData = bool(Bool),
            OptOption = oo_opt_lcmc(Bool)
        ;
            Option = optopt_opt_dead_procs,
            SpecialData = bool(Bool),
            OptOption = oo_opt_dead_procs(Bool)
        ;
            Option = optopt_deforest,
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
            Option = optopt_spec_in_all_dep_par_conjs,
            SpecialData = bool(Bool),
            OptOption = oo_spec_in_all_dep_par_conjs(Bool)
        ;
            Option = optopt_allow_some_paths_only_waits,
            SpecialData = bool(Bool),
            OptOption = oo_allow_some_paths_only_waits(Bool)
        ;
            Option = optopt_analyse_regions,
            SpecialData = bool(Bool),
            OptOption = oo_analyse_regions(Bool)
        ;
            Option = optopt_use_smart_indexing,
            SpecialData = bool(Bool),
            OptOption = oo_use_smart_indexing(Bool)
        ;
            Option = optopt_use_smart_indexing_atomic,
            SpecialData = bool(Bool),
            OptOption = oo_use_smart_indexing_atomic(Bool)
        ;
            Option = optopt_use_smart_indexing_string,
            SpecialData = bool(Bool),
            OptOption = oo_use_smart_indexing_string(Bool)
        ;
            Option = optopt_use_smart_indexing_tag,
            SpecialData = bool(Bool),
            OptOption = oo_use_smart_indexing_tag(Bool)
        ;
            Option = optopt_use_smart_indexing_float,
            SpecialData = bool(Bool),
            OptOption = oo_use_smart_indexing_float(Bool)
        ;
            Option = optopt_put_base_first_single_rec,
            SpecialData = bool(Bool),
            OptOption = oo_put_base_first_single_rec(Bool)
        ;
            Option = optopt_put_base_first_multi_rec,
            SpecialData = bool(Bool),
            OptOption = oo_put_base_first_multi_rec(Bool)
        ;
            Option = optopt_use_static_ground_cells,
            SpecialData = bool(Bool),
            OptOption = oo_use_static_ground_cells(Bool)
        ;
            Option = optopt_use_static_ground_floats,
            SpecialData = bool(Bool),
            OptOption = oo_use_static_ground_floats(Bool)
        ;
            Option = optopt_use_static_ground_int64s,
            SpecialData = bool(Bool),
            OptOption = oo_use_static_ground_int64s(Bool)
        ;
            Option = optopt_use_static_code_addresses,
            SpecialData = bool(Bool),
            OptOption = oo_use_static_code_addresses(Bool)
        ;
            Option = optopt_use_atomic_cells,
            SpecialData = bool(Bool),
            OptOption = oo_use_atomic_cells(Bool)
        ;
            Option = optopt_opt_middle_rec_llds,
            SpecialData = bool(Bool),
            OptOption = oo_opt_middle_rec_llds(Bool)
        ;
            Option = optopt_opt_simple_neg_llds,
            SpecialData = bool(Bool),
            OptOption = oo_opt_simple_neg_llds(Bool)
        ;
            Option = optopt_allow_hijacks,
            SpecialData = bool(Bool),
            OptOption = oo_allow_hijacks(Bool)
        ;
            Option = optopt_opt_mlds_tailcalls,
            SpecialData = bool(Bool),
            OptOption = oo_opt_mlds_tailcalls(Bool)
        ;
            Option = optopt_opt_mlds_inits,
            SpecialData = bool(Bool),
            OptOption = oo_opt_mlds_inits(Bool)
        ;
            Option = optopt_elim_unused_mlds_assigns,
            SpecialData = bool(Bool),
            OptOption = oo_elim_unused_mlds_assigns(Bool)
        ;
            Option = optopt_elim_local_vars_mlds,
            SpecialData = bool(Bool),
            OptOption = oo_elim_local_vars_mlds(Bool)
        ;
            Option = optopt_gen_trail_ops_inline_mlds,
            SpecialData = bool(Bool),
            OptOption = oo_gen_trail_ops_inline_mlds(Bool)
        ;
            Option = optopt_use_llds_common_data,
            SpecialData = bool(Bool),
            OptOption = oo_use_llds_common_data(Bool)
        ;
            Option = optopt_use_llds_common_layout_data,
            SpecialData = bool(Bool),
            OptOption = oo_use_llds_common_layout_data(Bool)
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
            Option = optopt_opt_jumps,
            SpecialData = bool(Bool),
            OptOption = oo_opt_jumps(Bool)
        ;
            Option = optopt_opt_fulljumps,
            SpecialData = bool(Bool),
            OptOption = oo_opt_fulljumps(Bool)
        ;
            Option = optopt_pessimize_llds_tailcalls,
            SpecialData = bool(Bool),
            OptOption = oo_pessimize_llds_tailcalls(Bool)
        ;
            Option = optopt_opt_checked_nondet_tailcalls,
            SpecialData = bool(Bool),
            OptOption = oo_opt_checked_nondet_tailcalls(Bool)
        ;
            Option = optopt_use_local_vars_llds,
            SpecialData = bool(Bool),
            OptOption = oo_use_local_vars_llds(Bool)
        ;
            Option = optopt_standardize_labels,
            SpecialData = bool(Bool),
            OptOption = oo_standardize_labels(Bool)
        ;
            Option = optopt_opt_labels,
            SpecialData = bool(Bool),
            OptOption = oo_opt_labels(Bool)
        ;
            Option = optopt_opt_dup_instrs_llds,
            SpecialData = bool(Bool),
            OptOption = oo_opt_dup_instrs_llds(Bool)
        ;
            Option = optopt_opt_dup_procs_llds,
            SpecialData = bool(Bool),
            OptOption = oo_opt_dup_procs_llds(Bool)
        ;
            Option = optopt_opt_frames,
            SpecialData = bool(Bool),
            OptOption = oo_opt_frames(Bool)
        ;
            Option = optopt_opt_delay_slot,
            SpecialData = bool(Bool),
            OptOption = oo_opt_delay_slot(Bool)
        ;
            Option = optopt_opt_llds_reassign,
            SpecialData = bool(Bool),
            OptOption = oo_opt_llds_reassign(Bool)
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
            Option = optopt_use_local_thread_engine_base,
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
            Option = optopt_from_ground_term_threshold,
            SpecialData = int(N),
            OptOption = oo_from_ground_term_threshold(N)
        ;
            Option = optopt_opt_svcell_cv_store_cost,
            SpecialData = int(N),
            OptOption = oo_opt_svcell_cv_store_cost(N)
        ;
            Option = optopt_opt_svcell_cv_load_cost,
            SpecialData = int(N),
            OptOption = oo_opt_svcell_cv_load_cost(N)
        ;
            Option = optopt_opt_svcell_fv_store_cost,
            SpecialData = int(N),
            OptOption = oo_opt_svcell_fv_store_cost(N)
        ;
            Option = optopt_opt_svcell_fv_load_cost,
            SpecialData = int(N),
            OptOption = oo_opt_svcell_fv_load_cost(N)
        ;
            Option = optopt_opt_svcell_op_ratio,
            SpecialData = int(N),
            OptOption = oo_opt_svcell_op_ratio(N)
        ;
            Option = optopt_opt_svcell_node_ratio,
            SpecialData = int(N),
            OptOption = oo_opt_svcell_node_ratio(N)
        ;
            Option = optopt_opt_svcell_all_path_node_ratio,
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
            Option = optopt_opt_unneeded_code_copy_limit,
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
            Option = optopt_llds_local_var_access_threshold,
            SpecialData = int(N),
            OptOption = oo_llds_local_var_access_threshold(N)
        ;
            Option = optopt_repeat_opts,
            SpecialData = int(N),
            OptOption = oo_opt_repeat(N)
        ;
            Option = optopt_llds_layout_compression_limit,
            SpecialData = int(N),
            OptOption = oo_llds_layout_compression_limit(N)
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

dodgy_code_warning_bool_options = DodgyWarnOptions :-
    FindOptionsPred =
        ( pred(Opt::out) is nondet :-
            ( optdb(oc_warn_dodgy_mod,  Opt, bool(_), _Help)
            ; optdb(oc_warn_dodgy_pred, Opt, bool(_), _Help)
            ; optdb(oc_warn_dodgy_prg,  Opt, bool(_), _Help)
            ; optdb(oc_warn_dodgy_goal, Opt, bool(_), _Help)
            ; optdb(oc_warn_dodgy_inst, Opt, bool(_), _Help)
            ; optdb(oc_warn_file,       Opt, bool(_), _Help)
            )
        ),
    solutions(FindOptionsPred, DodgyWarnOptions).

slow_code_warning_bool_options = SlowWarnOptions :-
    FindOptionsPred =
        ( pred(Opt::out) is nondet :-
            optdb(oc_warn_perf, Opt, bool(_), _Help)
        ),
    solutions(FindOptionsPred, SlowWarnOptions).

style_warning_bool_options = StyleWarnOptions :-
    FindOptionsPred =
        ( pred(Opt::out) is nondet :-
            ( optdb(oc_warn_style_mod,   Opt, bool(_), _Help)
            ; optdb(oc_warn_style_pred,  Opt, bool(_), _Help)
            ; optdb(oc_warn_style_goal,  Opt, bool(_), _Help)
            ; optdb(oc_warn_style_order, Opt, bool(_), _Help)
            ; optdb(oc_warn_style_ctg,   Opt, bool(_), _Help)
            )
        ),
    solutions(FindOptionsPred, StyleWarnOptions).

info_request_bool_options = InfoRequestOptions :-
    FindOptionsPred =
        ( pred(Opt::out) is nondet :-
            optdb(oc_inform, Opt, bool(_), _Help)
        ),
    solutions(FindOptionsPred, InfoRequestOptions).

options_not_to_track = InconsequentialOptions :-
    % XXX This needs to be updated when the oc_X changes are all done.
    InconsequentialCategories = set.list_to_set([oc_warn_ctrl,
        oc_warn_dodgy_mod, oc_warn_dodgy_pred, oc_warn_dodgy_prg,
        oc_warn_dodgy_goal, oc_warn_dodgy_inst, oc_warn_file,
        oc_warn_style_mod, oc_warn_style_pred, oc_warn_style_goal,
        oc_warn_style_goal_c, oc_warn_style_order,
        oc_warn_style_ctg, oc_warn_style_ctg_c,
        oc_inform, oc_verbosity, oc_internal, oc_buildsys]),
    FindOptionsPred =
        ( pred(Opt::out) is nondet :-
            optdb(Cat, Opt, _Data, _Help),
            set.member(Cat, InconsequentialCategories)
        ),
    solutions(FindOptionsPred, InconsequentialOptions).

%---------------------------------------------------------------------------%
:- end_module libs.options.
%---------------------------------------------------------------------------%
