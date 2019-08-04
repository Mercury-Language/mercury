%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: options.m.
% Main author: fjh.
%
% This modules defines the set of options accepted by the Mercury compiler.
% The definition takes the form of the types and predicates that getopt_io.m
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
%   the option_defaults_2 predicate. Which clause depends on what category
%   the option falls into (warning, optimization, etc).
%
%   For optimization options that should be set automatically at a specific
%   optimization level, there should also be an entry in opt_level.
%
%   For optimization options that should be set automatically if --opt-space
%   is given, there should also be an entry in opt_space.
%
%   For warning options, there should be an entry in the section of the
%   special_handler predicate that handles inhibit_warnings, and if the
%   option is a style warning option, in the second that handles
%   inhibit_style_warnings.
%
% - Every option should have a clause in the long_options predicate
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
%---------------------------------------------------------------------------%

:- module libs.options.
:- interface.

:- import_module char.
:- import_module getopt_io.
:- import_module io.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- pred short_option(char::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_defaults(option::out, option_data::out) is nondet.

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
:- pred special_handler(option::in, special_data::in, option_table::in,
    maybe_option_table::out) is semidet.

    % Return the style and non-style warning options.
    %
:- func style_warning_options = list(option).
:- func non_style_warning_options = list(option).

    % Return the set of options which are inconsequential as far as the
    % `--track-flags' option is concerned. That is, adding or removing such
    % an option to a module should not force the module to be recompiled.
    %
:- pred inconsequential_options(set(option)::out) is det.

:- pred options_help(io::di, io::uo) is det.

:- type option_table == option_table(option).
:- type maybe_option_table == maybe_option_table(option).

    % Add a directory to search for Mercury libraries. This
    % adds `--search-directory', `--c-include-directory',
    % `--erlang-include-directory',
    % `--library-directory' and `--init-file-directory' options.
    %
:- pred option_table_add_mercury_library_directory(string::in,
    option_table::in, option_table::out) is det.

    % Add a directory using all of the
    % `--search-directory', `--intermod-directory',
    % `--library-directory', `--init-file-directory' and
    % `--c-include-directory', `--erlang-include-directory'
    % options.
    %
:- pred option_table_add_search_library_files_directory(string::in,
    option_table::in, option_table::out) is det.

    % Set all of the given options to the given value.
    %
:- pred set_all_options_to(list(option)::in, option_data::in,
    option_table::in, option_table::out) is det.

    % Quote an argument to a shell command.
    %
:- func quote_arg(string) = string.

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
    % enough to be officially supported should still be documented.
    % The documentation can go either next to the option definition
    % here, or as commented-out code in the appropriate subroutine
    % of options_help/2.
:- type option

    % Warning options
    --->    inhibit_warnings
    ;       inhibit_style_warnings
    ;       warn_accumulator_swaps
    ;       halt_at_warn
    ;       halt_at_syntax_errors
    ;       halt_at_auto_parallel_failure
    ;       warn_singleton_vars
    ;       warn_overlapping_scopes
    ;       warn_det_decls_too_lax
    ;       warn_inferred_erroneous
    ;       warn_nothing_exported
    ;       warn_unused_args
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
    ;       warn_non_stratification
    ;       warn_unification_cannot_succeed
    ;       warn_simple_code
    ;       warn_duplicate_calls
    ;       warn_implicit_stream_calls
    ;       warn_missing_module_name
    ;       warn_wrong_module_name
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
    ;       inform_ite_instead_of_switch
    ;       inform_incomplete_switch
    ;       inform_incomplete_switch_threshold
    ;       warn_unresolved_polymorphism
    ;       warn_suspicious_foreign_procs
    ;       warn_suspicious_foreign_code
    ;       warn_state_var_shadowing
    ;       inform_inferred
    ;       inform_inferred_types
    ;       inform_inferred_modes
    ;       inform_suboptimal_packing

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
    ;       limit_error_contexts
    ;       debug_types
    ;       debug_modes
    ;       debug_modes_statistics
    ;       debug_modes_minimal
    ;       debug_modes_verbose
    ;       debug_modes_pred_id
    ;       debug_dep_par_conj
    ;       debug_det
    ;       debug_code_gen_pred_id
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

    % Output options
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
    ;       generate_module_order
    ;       generate_standalone_interface
    ;       only_opmode_convert_to_mercury
    ;       only_opmode_typecheck_only
    ;       only_opmode_errorcheck_only
    ;       only_opmode_target_code_only
    ;       only_opmode_compile_only
    ;       compile_to_shared_lib
    ;       only_opmode_output_grade_string
    ;       only_opmode_output_link_command
    ;       only_opmode_output_shared_lib_link_command
    ;       only_opmode_output_libgrades
    ;       only_opmode_output_cc
    ;       only_opmode_output_c_compiler_type
    ;       only_opmode_output_csharp_compiler
    ;       only_opmode_output_csharp_compiler_type
    ;       only_opmode_output_cflags
    ;       only_opmode_output_library_link_flags
    ;       only_opmode_output_grade_defines
    ;       only_opmode_output_c_include_directory_flags
    ;       only_opmode_output_target_arch
    ;       only_opmode_output_class_dir

    % Auxiliary output options
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
    ;       assume_gmake
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
    ;       generate_bytecode
    ;       line_numbers
    ;       line_numbers_around_foreign_code
    ;       line_numbers_for_c_headers
    ;       auto_comments
    ;       frameopt_comments
    ;       max_error_line_width
    ;       show_definitions
    ;       show_definition_line_counts
    ;       show_local_type_repns
    ;       show_all_type_repns
    ;       show_developer_type_repns
    ;       show_dependency_graph
    ;       imports_graph
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
    ;       dump_hlds_file_suffix
    ;       dump_same_hlds
    ;       dump_mlds
    ;       dump_mlds_pred_name
    ;       verbose_dump_mlds
    ;       mode_constraints
    ;       simple_mode_constraints
    ;       prop_mode_constraints
    ;       compute_goal_modes
    ;       benchmark_modes
    ;       benchmark_modes_repeat

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
    % XXX The following options need to be documented.
    ;       erlang              % target erlang
    ;       erlang_only         % target erlang + target_code_only

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
    ;       trail_segments
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
    ;       tags
    ;       num_ptag_bits
    ;       bits_per_word
    ;       bytes_per_word
            % The undocumented conf_low_ptag_bits option is used by the `mmc'
            % script to pass the default value for num_ptag_bits assuming
            % --tags low. The reason that `mmc' doesn't just pass a default
            % value for --num-tag-bits is that we want to be able to give an
            % error message if the user specifies `--tags high' and doesn't
            % specify `--num-tag-bits'.

    ;       conf_low_ptag_bits
    ;       unboxed_float
    ;       unboxed_int64s
    ;       unboxed_no_tag_types
    ;       arg_pack_bits
    ;       allow_double_word_fields
    ;       allow_double_word_ints          % XXX bootstrapping option
    ;       allow_packing_dummies           % XXX bootstrapping option
    ;       allow_packing_ints              % XXX bootstrapping option
    ;       allow_packing_chars             % XXX bootstrapping option
    ;       allow_packing_local_sectags     % XXX bootstrapping option
    ;       allow_packing_remote_sectags    % XXX bootstrapping option
    ;       allow_packing_mini_types        % XXX bootstrapping option
    ;       allow_packed_unify_compare      % XXX bootstrapping option
    ;       sync_term_size % in words

    % LLDS back-end compilation model options
    ;       gcc_non_local_gotos
    ;       gcc_global_registers
    ;       asm_labels
    ;       use_float_registers

    % MLDS back-end compilation model options
    ;       highlevel_code
    ;       highlevel_data
    ;       det_copy_out
    ;       nondet_copy_out
    ;       put_commit_in_own_func
    ;       put_nondet_env_on_heap

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

    ;       can_compare_compound_values
            % Should be set to yes if the target back end supports comparison
            % of non-atomic values with builtin operators.

    ;       order_constructors_for_erlang
            % Should be set to yes if we need to order functors the way Erlang
            % expects them to be ordered, i.e. first by arity, then by
            % lexicographic order on function symbol name.

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

    ;       disable_minimal_model_stack_copy_pneg
    ;       disable_minimal_model_stack_copy_cut
    ;       use_minimal_model_stack_copy_pneg
    ;       use_minimal_model_stack_copy_cut
            % These four are used to analyze the performance effects
            % of minimal model tabling.

    ;       disable_trail_ops
            % This is used to analyze the performance effects of trailing.

    ;       size_region_ite_fixed
    ;       size_region_disj_fixed
    ;       size_region_semi_disj_fixed
    ;       size_region_commit_fixed

    ;       size_region_ite_protect
    ;       size_region_ite_snapshot
    ;       size_region_semi_disj_protect
    ;       size_region_disj_snapshot
    ;       size_region_commit_entry

    ;       allow_multi_arm_switches

    ;       type_check_constraints

    % Code generation options
    ;       low_level_debug
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
    ;       opt_no_return_calls
    ;       debug_class_init

    % Optimization Options
    ;       opt_level
    ;       opt_level_number
    ;       opt_space                   % Default is to optimize time.
    ;       intermodule_optimization
    ;       read_opt_files_transitively
    ;       use_opt_files
    ;       use_trans_opt_files
    ;       transitive_optimization
    ;       intermodule_analysis
    ;       analysis_repeat
    ;       analysis_file_cache

    %   - HLDS
    ;       allow_inlining
    ;       inlining
    ;       inline_simple
    ;       inline_builtins
    ;       inline_single_use
    ;       inline_call_cost
    ;       inline_compound_threshold
    ;       inline_simple_threshold
    ;       inline_vars_threshold
    ;       intermod_inline_simple_threshold
    ;       inline_linear_tail_rec_sccs
    ;       inline_linear_tail_rec_sccs_max_extra
    ;       from_ground_term_threshold
    ;       enable_const_struct
    ;       common_struct
    ;       common_struct_preds
    ;       constraint_propagation
    ;       local_constraint_propagation
    ;       optimize_unused_args
    ;       intermod_unused_args
    ;       optimize_higher_order
    ;       higher_order_size_limit
    ;       higher_order_arg_limit
    ;       unneeded_code
    ;       unneeded_code_copy_limit
    ;       unneeded_code_debug
    ;       unneeded_code_debug_pred_name
    ;       type_specialization
    ;       user_guided_type_specialization
    ;       introduce_accumulators
    ;       optimize_constructor_last_call_accumulator
    ;       optimize_constructor_last_call_null
    ;       optimize_constructor_last_call
    ;       optimize_duplicate_calls
    ;       constant_propagation
    ;       excess_assign
    ;       test_after_switch
    ;       optimize_format_calls
    ;       optimize_saved_vars_const
    ;       optimize_saved_vars_cell
    ;       optimize_saved_vars_cell_loop
    ;       optimize_saved_vars_cell_full_path
    ;       optimize_saved_vars_cell_on_stack
    ;       optimize_saved_vars_cell_candidate_headvars
    ;       optimize_saved_vars_cell_cv_store_cost
    ;       optimize_saved_vars_cell_cv_load_cost
    ;       optimize_saved_vars_cell_fv_store_cost
    ;       optimize_saved_vars_cell_fv_load_cost
    ;       optimize_saved_vars_cell_op_ratio
    ;       optimize_saved_vars_cell_node_ratio
    ;       optimize_saved_vars_cell_all_path_node_ratio
    ;       optimize_saved_vars_cell_include_all_candidates
    ;       optimize_saved_vars
    ;       loop_invariants
    ;       delay_construct
    ;       follow_code
    ;       optimize_dead_procs
    ;       deforestation
    ;       deforestation_depth_limit
    ;       deforestation_cost_factor
    ;       deforestation_vars_threshold
    ;       deforestation_size_threshold
    ;       analyse_trail_usage
    ;       optimize_trail_usage
    ;       optimize_region_ops
    ;       analyse_mm_tabling
    ;       untuple
    ;       tuple
    ;       tuple_trace_counts_file
    ;       tuple_costs_ratio
    ;       tuple_min_args
    ;       inline_par_builtins
    ;       always_specialize_in_dep_par_conjs
    ;       allow_some_paths_only_waits
    ;       region_analysis

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
    ;       termination
    ;       termination_check
    ;       verbose_check_termination
    ;       termination_single_args
    ;       termination_norm
    ;       termination_error_limit
    ;       termination_path_limit

    % Stuff for the new termination analyser.
    ;       termination2
    ;          check_termination2
    ;          verbose_check_termination2
    ;          termination2_norm
    ;          widening_limit
    ;          arg_size_analysis_only
    ;          propagate_failure_constrs
    ;          term2_maximum_matrix_size
    ;       analyse_exceptions
    ;       analyse_closures

    %   - HLDS->LLDS
    ;       smart_indexing
    ;         dense_switch_req_density
    ;         lookup_switch_req_density
    ;         dense_switch_size
    ;         lookup_switch_size
    ;         string_trie_switch_size
    ;         string_hash_switch_size
    ;         string_binary_switch_size
    ;         tag_switch_size
    ;         try_switch_size
    ;         binary_switch_size
    ;         switch_single_rec_base_first
    ;         switch_multi_rec_base_first

    ;         smart_atomic_indexing
    ;         smart_string_indexing
    ;         smart_tag_indexing
    ;         smart_float_indexing

    ;       static_ground_cells
    ;       static_ground_floats
    ;       static_ground_int64s
    ;       static_code_addresses

    ;       use_atomic_cells
    ;       middle_rec
    ;       simple_neg
    ;       allow_hijacks

    %   - MLDS
    ;       optimize_tailcalls
    ;       optimize_initializations
    ;       eliminate_unused_mlds_assigns
    ;       eliminate_local_vars
    ;       generate_trail_ops_inline

    %   - LLDS
    ;       common_data
    ;       common_layout_data
    ;       optimize            % Also used for MLDS->MLDS optimizations.
    ;       optimize_peep
    ;       optimize_peep_mkword
    ;       optimize_jumps
    ;       optimize_fulljumps
    ;       pessimize_tailcalls
    ;       checked_nondet_tailcalls
    ;       use_local_vars
    ;       local_var_access_threshold
    ;       standardize_labels
    ;       optimize_labels
    ;       optimize_dups
    ;       optimize_proc_dups
    ;       optimize_frames
    ;       optimize_delay_slot
    ;       optimize_reassign
    ;       optimize_repeat
    ;       layout_compression_limit

    %   - C
    ;       use_macro_for_redo_fail
    ;       emit_c_loops
    ;       procs_per_c_function
    ;       everything_in_one_c_function
    ;       local_thread_engine_base

    %   - Erlang
    ;       erlang_switch_on_strings_as_atoms

    % Target code compilation options
    ;       target_debug

    % C
    ;       cc
    ;       cflags
    ;       quoted_cflag
    ;       c_include_directory
    ;       c_optimize
    ;       ansi_c
    ;       inline_alloc

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
    ;       c_flag_to_name_object_file
    ;       object_file_extension
    ;       pic_object_file_extension
    ;       c_compiler_type
    ;       csharp_compiler_type

    % Java
    ;       java_compiler
    ;       java_interpreter
    ;       java_flags
    ;       quoted_java_flag
    ;       java_classpath
    ;       java_object_file_extension

    % C#
    ;       csharp_compiler
    ;       csharp_flags
    ;       quoted_csharp_flag
    ;       cli_interpreter

    % Erlang
    ;       erlang_compiler
    ;       erlang_interpreter
    ;       erlang_flags
    ;       quoted_erlang_flag
    ;       erlang_include_directory
    ;       erlang_object_file_extension
    ;       erlang_native_code
    ;       erlang_inhibit_trivial_warnings

    % Link options
    ;       output_file_name
    ;       ld_flags
    ;       quoted_ld_flag
    ;       ld_libflags
    ;       quoted_ld_libflag
    ;       link_library_directories
    ;       runtime_link_library_directories
    ;       default_runtime_library_directory
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
    ;       linkage
    ;       linkage_special
    ;       mercury_linkage
    ;       mercury_linkage_special
    ;       strip
    ;       demangle
    ;       main
    ;       allow_undefined
    ;       use_readline
    ;       runtime_flags
    ;       extra_initialization_functions
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
    ;       mkinit_erl_command
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
    ;       rebuild
    ;       keep_going
    ;       jobs
    ;       track_flags
    ;       invoked_by_mmc_make
    ;       extra_init_command
    ;       pre_link_command
    ;       install_prefix
    ;       use_symlinks
    ;       mercury_configuration_directory
    ;       mercury_configuration_directory_special
    ;       install_command
    ;       install_command_dir_option
    ;       detect_libgrades
    ;       libgrades
    ;       libgrades_include_components
    ;       libgrades_exclude_components
    ;       lib_linkages
    ;       flags_file
    ;       options_files
    ;       config_file
    ;       options_search_directories
    ;       use_subdirs
    ;       use_grade_subdirs
    ;       search_directories
    ;       intermod_directories
    ;       use_search_directories_for_intermod
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
    ;       help
    ;       version
    ;       target_arch
    ;       cross_compiling
    ;       local_module_id
    ;       analysis_file_cache_dir
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

    ;       ignore_par_conjunctions
    ;       control_granularity
    ;       distance_granularity
    ;       implicit_parallelism
    ;       feedback_file
    ;       par_loop_control
    ;       par_loop_control_preserve_tail_recursion.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compute_grade.

:- import_module assoc_list.
:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

:- type option_category
    --->    warning_option
    ;       verbosity_option
    ;       output_option
    ;       aux_output_option
    ;       language_semantics_option
    ;       compilation_model_option
    ;       internal_use_option
    ;       code_gen_option
    ;       special_optimization_option
    ;       optimization_option
    ;       target_code_compilation_option
    ;       link_option
    ;       build_system_option
    ;       miscellaneous_option.

option_defaults(Option, Default) :-
    option_defaults_2(_Category, OptionsList),
    list.member(Option - Default, OptionsList).

:- pred option_defaults_2(option_category, list(pair(option, option_data))).
:- mode option_defaults_2(in, out) is det.
:- mode option_defaults_2(out, out) is multi.

option_defaults_2(warning_option, [
    % Warning Options
    inhibit_warnings                    -   bool_special,
    inhibit_style_warnings              -   bool_special,
    warn_accumulator_swaps              -   bool(yes),
    halt_at_warn                        -   bool(no),
    halt_at_syntax_errors               -   bool(no),
    halt_at_auto_parallel_failure       -   bool(no),

    % IMPORTANT NOTE:
    % if you add any new warning options, or if you change the default
    % for an existing warning option to `yes', then you will need to modify
    % the handling of inhibit_warnings in special_handler, and if the change
    % affects a style warning, you will need to modify the handling of
    % inhibit_style_warnings as well.

    warn_singleton_vars                 -   bool(yes),
    warn_overlapping_scopes             -   bool(yes),
    warn_det_decls_too_lax              -   bool(yes),
    warn_inferred_erroneous             -   bool(yes),
    warn_nothing_exported               -   bool(yes),
    warn_unused_args                    -   bool(no),
    warn_interface_imports              -   bool(yes),
    warn_interface_imports_in_parents   -   bool(no),
    warn_inconsistent_pred_order_clauses        -   bool(no),
    warn_inconsistent_pred_order_foreign_procs  -   bool(no),
    warn_non_contiguous_decls           -   bool(yes),
    warn_non_contiguous_clauses         -   bool(no),   % XXX should be yes
    warn_non_contiguous_foreign_procs   -   bool(no),
    warn_non_stratification             -   bool(no),
    warn_missing_opt_files              -   bool(yes),
    warn_missing_trans_opt_files        -   bool(no),
    warn_missing_trans_opt_deps         -   bool(yes),
    warn_unification_cannot_succeed     -   bool(yes),
    warn_simple_code                    -   bool(yes),
    warn_duplicate_calls                -   bool(no),
    warn_implicit_stream_calls          -   bool(no),
    warn_missing_module_name            -   bool(yes),
    warn_wrong_module_name              -   bool(yes),
    warn_smart_recompilation            -   bool(yes),
    warn_undefined_options_variables    -   bool(yes),
    warn_suspicious_recursion           -   bool(no),
    warn_non_tail_recursion_self        -   bool(no),
    warn_non_tail_recursion_mutual      -   bool(no),
    warn_non_tail_recursion             -   maybe_string_special,
    warn_obvious_non_tail_recursion     -   bool(no),
    warn_target_code                    -   bool(yes),
    warn_up_to_date                     -   bool(yes),
    warn_stubs                          -   bool(yes),
    warn_dead_procs                     -   bool(no),
    warn_dead_preds                     -   bool(no),
    warn_table_with_inline              -   bool(yes),
    warn_non_term_special_preds         -   bool(yes),
    warn_known_bad_format_calls         -   bool(yes),
    warn_only_one_format_string_error   -   bool(yes),
    warn_unknown_format_calls           -   bool(no),
    warn_obsolete                       -   bool(yes),
    warn_insts_without_matching_type    -   bool(yes),
    warn_insts_with_functors_without_type - bool(no),
        % XXX disabled by default until someone
        % removes all the unused imports from
        % the compiler itself which is compiled
        % with --halt-at-warn by default.
    warn_unused_imports                 -   bool(no),
    inform_ite_instead_of_switch        -   bool(no),
    inform_incomplete_switch            -   bool(no),
    inform_incomplete_switch_threshold  -   int(0),
    warn_unresolved_polymorphism        -   bool(yes),
    warn_suspicious_foreign_procs       -   bool(no),
    warn_suspicious_foreign_code        -   bool(no),
    warn_state_var_shadowing            -   bool(yes),
    inform_inferred                     -   bool_special,
    inform_inferred_types               -   bool(yes),
    inform_inferred_modes               -   bool(yes),
    inform_suboptimal_packing           -   bool(no)
]).
option_defaults_2(verbosity_option, [
    % Verbosity Options
    verbose                             -   bool(no),
    very_verbose                        -   bool(no),
    verbose_errors                      -   bool(no),
    verbose_recompilation               -   bool(no),
    find_all_recompilation_reasons      -   bool(no),
    verbose_make                        -   bool(yes),
    verbose_commands                    -   bool(no),
    output_compile_error_lines          -   int(15),
    report_cmd_line_args                -   bool(no),
    report_cmd_line_args_in_doterr      -   bool(no),
    statistics                          -   bool(no),
    detailed_statistics                 -   bool(no),
    proc_size_statistics                -   string(""),
    limit_error_contexts                -   accumulating([]),
    debug_types                         -   bool(no),
    debug_modes                         -   bool(no),
    debug_modes_statistics              -   bool(no),
    debug_modes_minimal                 -   bool(no),
    debug_modes_verbose                 -   bool(no),
    debug_modes_pred_id                 -   int(-1),
    debug_dep_par_conj                  -   accumulating([]),
    debug_det                           -   bool(no),
    debug_code_gen_pred_id              -   int(-1),
    debug_term                          -   bool(no),
    debug_opt                           -   bool(no),
    debug_opt_pred_id                   -   accumulating([]),
    debug_opt_pred_name                 -   accumulating([]),
    debug_pd                            -   bool(no),
    debug_liveness                      -   int(-1),
    debug_stack_opt                     -   int(-1),
    debug_make                          -   bool(no),
    debug_closure                       -   bool(no),
    debug_trail_usage                   -   bool(no),
    debug_mode_constraints              -   bool(no),
    debug_intermodule_analysis          -   bool(no),
    debug_mm_tabling_analysis           -   bool(no),
    debug_indirect_reuse                -   bool(no),
    debug_type_rep                      -   bool(no)
]).
option_defaults_2(output_option, [
    % Output Options (mutually exclusive)
    only_opmode_generate_source_file_mapping -      bool(no),
    only_opmode_generate_dependency_file -  bool(no),
    only_opmode_generate_dependencies   -   bool(no),
    generate_module_order               -   bool(no),
    generate_standalone_interface       -   maybe_string(no),
    only_opmode_make_short_interface    -   bool(no),
    only_opmode_make_interface          -   bool(no),
    only_opmode_make_private_interface  -   bool(no),
    only_opmode_make_optimization_interface -       bool(no),
    only_opmode_make_transitive_opt_interface -     bool(no),
    only_opmode_make_analysis_registry  -   bool(no),
    only_opmode_make_xml_documentation  -   bool(no),
    only_opmode_convert_to_mercury      -   bool(no),
    only_opmode_typecheck_only          -   bool(no),
    only_opmode_errorcheck_only         -   bool(no),
    only_opmode_target_code_only        -   bool(no),
    only_opmode_compile_only            -   bool(no),
    compile_to_shared_lib               -   bool(no),
    only_opmode_output_grade_string     -   bool(no),
    only_opmode_output_link_command     -   bool(no),
    only_opmode_output_shared_lib_link_command -    bool(no),
    only_opmode_output_libgrades        -   bool(no),
    only_opmode_output_cc               -   bool(no),
    only_opmode_output_c_compiler_type  -   bool(no),
    only_opmode_output_csharp_compiler  -   bool(no),
    only_opmode_output_csharp_compiler_type -       bool(no),
    only_opmode_output_cflags           -   bool(no),
    only_opmode_output_library_link_flags -   bool(no),
    only_opmode_output_grade_defines    -   bool(no),
    only_opmode_output_c_include_directory_flags -  bool(no),
    only_opmode_output_target_arch      -   bool(no),
    only_opmode_output_class_dir        -   bool(no)
]).
option_defaults_2(aux_output_option, [
    % Auxiliary Output Options
    smart_recompilation                 -   bool(no),
    generate_item_version_numbers       -   bool(no),
    generate_mmc_make_module_dependencies - bool(no),
    assume_gmake                        -   bool(yes),
    trace_level                         -   string("default"),
    trace_optimized                     -   bool(no),
    trace_prof                          -   bool(no),
    trace_table_io                      -   bool(no),
    trace_table_io_only_retry           -   bool(no),
    trace_table_io_states               -   bool(no),
    trace_table_io_require              -   bool(no),
    trace_table_io_all                  -   bool(no),
    trace_goal_flags                    -   accumulating([]),
    prof_optimized                      -   bool(no),
    exec_trace_tail_rec                 -   bool(no),
    suppress_trace                      -   string(""),
    force_disable_tracing               -   bool(no),
    delay_death                         -   bool(yes),
    delay_death_max_vars                -   int(1000),
    stack_trace_higher_order            -   bool(no),
    force_disable_ssdebug               -   bool(no),
    generate_bytecode                   -   bool(no),
    line_numbers                        -   bool(no),
    line_numbers_around_foreign_code    -   bool(yes),
    line_numbers_for_c_headers          -   bool(no),
    auto_comments                       -   bool(no),
    frameopt_comments                   -   bool(no),
    max_error_line_width                -   maybe_int(yes(79)),
    show_definitions                    -   bool(no),
    show_definition_line_counts         -   bool(no),
    show_local_type_repns               -   bool(no),
    show_all_type_repns                 -   bool(no),
    show_developer_type_repns           -   bool(no),
    show_dependency_graph               -   bool(no),
    imports_graph                       -   bool(no),
    dump_trace_counts                   -   accumulating([]),
    dump_hlds                           -   accumulating([]),
    dump_hlds_pred_id                   -   accumulating([]),
    dump_hlds_pred_name                 -   accumulating([]),
    dump_hlds_pred_name_order           -   bool(no),
    dump_hlds_spec_preds                -   bool(no),
    dump_hlds_spec_preds_for            -   accumulating([]),
    dump_hlds_alias                     -   string(""),
    dump_hlds_options                   -   string(""),
    dump_hlds_inst_limit                -   int(100),
    dump_hlds_file_suffix               -   string(""),
    dump_same_hlds                      -   bool(no),
    dump_mlds                           -   accumulating([]),
    dump_mlds_pred_name                 -   accumulating([]),
    verbose_dump_mlds                   -   accumulating([]),
    mode_constraints                    -   bool(no),
    simple_mode_constraints             -   bool(no),
    prop_mode_constraints               -   bool(no),
    compute_goal_modes                  -   bool(no),
    benchmark_modes                     -   bool(no),
    benchmark_modes_repeat              -   int(1)
]).
option_defaults_2(language_semantics_option, [
    strict_sequential                   -   special,
    reorder_conj                        -   bool(yes),
    reorder_disj                        -   bool(yes),
    fully_strict                        -   bool(yes),
    allow_stubs                         -   bool(no),
    infer_types                         -   bool(no),
    infer_modes                         -   bool(no),
    infer_det                           -   bool(yes),
    infer_all                           -   bool_special,
    type_inference_iteration_limit      -   int(60),
    mode_inference_iteration_limit      -   int(30),
    event_set_file_name                 -   string("")
]).
option_defaults_2(compilation_model_option, [
    % Compilation model options (ones that affect binary compatibility).
    grade                               -   string_special,
    % The `mmc' script will pass the default grade determined
    % at configuration time.

    % Target selection compilation model options
    target                              -   string("c"),
    compile_to_c                        -   special,
    csharp                              -   special,
    csharp_only                         -   special,
    java                                -   special,
    java_only                           -   special,
    erlang                              -   special,
    erlang_only                         -   special,

    % Optional feature compilation model options:
    % (a) Debuggging
    exec_trace                          -   bool(no),
    decl_debug                          -   bool(no),
    % (b) Profiling
    profiling                           -   bool_special,
    time_profiling                      -   special,
    memory_profiling                    -   special,
    deep_profiling                      -   special,
    profile_calls                       -   bool(no),
    profile_time                        -   bool(no),
    profile_memory                      -   bool(no),
    profile_deep                        -   bool(no),
    use_activation_counts               -   bool(no),
    pre_prof_transforms_simplify        -   bool(no),
    pre_implicit_parallelism_simplify   -   bool(no),
    coverage_profiling                  -   bool(yes),
    coverage_profiling_via_calls        -   bool(no),
    coverage_profiling_static           -   bool(no),
    profile_deep_coverage_after_goal    -   bool(yes),
    profile_deep_coverage_branch_ite    -   bool(yes),
    profile_deep_coverage_branch_switch -   bool(yes),
    profile_deep_coverage_branch_disj   -   bool(yes),
    profile_deep_coverage_use_portcounts -  bool(no),
    profile_deep_coverage_use_trivial   -   bool(no),
    profile_for_feedback                -   bool(no),
    use_zeroing_for_ho_cycles           -   bool(yes),
    use_lots_of_ho_specialization       -   bool(no),
    deep_profile_tail_recursion         -   bool(no),
    record_term_sizes_as_words          -   bool(no),
    record_term_sizes_as_cells          -   bool(no),
    experimental_complexity             -   string(""),
    % (c) Miscellaneous optional features
    gc                                  -   string("boehm"),
    parallel                            -   bool(no),
    threadscope                         -   bool(no),
    use_trail                           -   bool(no),
    trail_segments                      -   bool(no),
    maybe_thread_safe_opt               -   string("no"),
    extend_stacks_when_needed           -   bool(no),
    stack_segments                      -   bool(no),
    use_regions                         -   bool(no),
    use_alloc_regions                   -   bool(yes),
    use_regions_debug                   -   bool(no),
    use_regions_profiling               -   bool(no),
    use_minimal_model_stack_copy        -   bool(no),
    use_minimal_model_own_stacks        -   bool(no),
    minimal_model_debug                 -   bool(no),
    pregenerated_dist                   -   bool(no),
    single_prec_float                   -   bool(no),
    type_layout                         -   bool(yes),
    source_to_source_debug              -   bool(no),
    ssdb_trace_level                    -   string("default"),
    link_ssdb_libs                      -   bool(no),

    % Data representation compilation model options
    tags                                -   string("low"),
    num_ptag_bits                        -   int(-1),
                                        % -1 is a special value which means
                                        % use the value of conf_low_ptag_bits
                                        % instead
    bits_per_word                       -   int(32),
                                        % A good default for the current
                                        % generation of architectures.
    bytes_per_word                      -   int(4),
                                        % A good default for the current
                                        % generation of architectures.
    conf_low_ptag_bits                  -   int(2),
                                        % The `mmc' script will override the
                                        % above default with a value determined
                                        % at configuration time.
    unboxed_float                       -   bool(no),
    unboxed_int64s                      -   bool(no),
    unboxed_no_tag_types                -   bool(yes),
    arg_pack_bits                       -   int(-1),
                                        % -1 is a special value which means use
                                        % all word bits for argument packing.
    allow_double_word_fields            -   bool(yes),
    allow_double_word_ints              -   bool(no),
    allow_packing_dummies               -   bool(no),
    allow_packing_ints                  -   bool(no),
    allow_packing_chars                 -   bool(no),
    allow_packing_local_sectags         -   bool(no),
    allow_packing_remote_sectags        -   bool(no),
    allow_packing_mini_types            -   bool(no),
    allow_packed_unify_compare          -   bool(no),
    sync_term_size                      -   int(8),
                                        % 8 is the size on linux (at the time
                                        % of writing) - will usually be
                                        % overridden by a value from configure.

    % LLDS back-end compilation model options
    gcc_non_local_gotos                 -   bool(yes),
    gcc_global_registers                -   bool(yes),
    asm_labels                          -   bool(yes),
    use_float_registers                 -   bool(yes),

    % MLDS back-end compilation model options
    highlevel_code                      -   bool(no),
    highlevel_data                      -   bool(no),
    det_copy_out                        -   bool(no),
    nondet_copy_out                     -   bool(no),
    put_commit_in_own_func              -   bool(no),
    put_nondet_env_on_heap              -   bool(no)
]).
option_defaults_2(internal_use_option, [
    % Options for internal use only
    backend_foreign_languages           -  accumulating([]),
                                        % The backend_foreign_languages option
                                        % depends on the target and is set in
                                        % handle_options.
    stack_trace                         -   bool(no),
    basic_stack_layout                  -   bool(no),
    agc_stack_layout                    -   bool(no),
    procid_stack_layout                 -   bool(no),
    trace_stack_layout                  -   bool(no),
    body_typeinfo_liveness              -   bool(no),
    can_compare_constants_as_ints       -   bool(no),
    pretest_equality_cast_pointers      -   bool(no),
    can_compare_compound_values         -   bool(no),
    order_constructors_for_erlang       -   bool(no),
    delay_partial_instantiations        -   bool(no),
    allow_defn_of_builtins              -   bool(no),
    type_ctor_info                      -   bool(yes),
    type_ctor_layout                    -   bool(yes),
    type_ctor_functors                  -   bool(yes),
    rtti_line_numbers                   -   bool(yes),
    new_type_class_rtti                 -   bool(no),
    disable_minimal_model_stack_copy_pneg - bool(no),
    disable_minimal_model_stack_copy_cut -  bool(no),
    use_minimal_model_stack_copy_pneg   -   bool(no),
    use_minimal_model_stack_copy_cut    -   bool(no),
    disable_trail_ops                   -   bool(no),
    % The size_* values below *must* be consistent with the corresponding
    % values or data structures in mercury_region.h.
    size_region_ite_fixed               -   int(4),
    size_region_disj_fixed              -   int(4),
    size_region_commit_fixed            -   int(5),
    size_region_ite_protect             -   int(1),
    size_region_ite_snapshot            -   int(3),
    size_region_semi_disj_protect       -   int(1),
    size_region_disj_snapshot           -   int(3),
    size_region_commit_entry            -   int(1),
    allow_multi_arm_switches            -   bool(yes),
    type_check_constraints              -   bool(no)
]).
option_defaults_2(code_gen_option, [
    % Code Generation Options
    low_level_debug                     -   bool(no),
    table_debug                         -   bool(no),
    trad_passes                         -   bool(yes),
    parallel_liveness                   -   bool(no),
    parallel_code_gen                   -   bool(no),
    reclaim_heap_on_failure             -   bool_special,
    reclaim_heap_on_semidet_failure     -   bool(yes),
    reclaim_heap_on_nondet_failure      -   bool(yes),
    have_delay_slot                     -   bool(no),
                                        % The `mmc' script may override the
                                        % above default if configure says
                                        % the machine has branch delay slots.
    num_real_r_regs                     -   int(5),
    num_real_f_regs                     -   int(0),
    num_real_r_temps                    -   int(5),
    num_real_f_temps                    -   int(0),
                                        % The `mmc' script will override the
                                        % above defaults with values determined
                                        % at configuration time.
    max_jump_table_size                 -   int(0),
                                        % 0 indicates any size.
    max_specialized_do_call_closure     -   int(5),
                                        % mercury.do_call_closure_N
                                        % exists for N <= option_value;
                                        % set to -1 to disable.
                                        % Should be less than or equal to
                                        % max_spec_explicit_arg
                                        % in tools/make_spec_ho_call.
    max_specialized_do_call_class_method -  int(6),
                                        % mercury.do_call_class_method_N
                                        % exists for N <= option_value;
                                        % set to -1 to disable.
                                        % Should be less than or equal to
                                        % max_spec_explicit_arg
                                        % in tools/make_spec_method_call.
    compare_specialization              -   int(-1),
                                        % -1 asks handle_options.m to give
                                        % the value, which may be grade
                                        % dependent.
    should_pretest_equality             -   bool(yes),
    fact_table_max_array_size           -   int(1024),
    fact_table_hash_percent_full        -   int(90),
    prefer_switch                       -   bool(yes),
    prefer_while_loop_over_jump_self    -   bool(yes),
    prefer_while_loop_over_jump_mutual  -   bool(no),
    opt_no_return_calls                 -   bool(yes),
    debug_class_init                    -   bool(no)
]).
option_defaults_2(special_optimization_option, [
    % Special optimization options.
    % These ones are not affected by `-O<n>'.
    opt_level                           -   int_special,
    opt_level_number                    -   int(-2),
    opt_space                           -   special,
    intermodule_optimization            -   bool(no),
    read_opt_files_transitively         -   bool(yes),
    use_opt_files                       -   bool(no),
    use_trans_opt_files                 -   bool(no),
    transitive_optimization             -   bool(no),
    intermodule_analysis                -   bool(no),
    analysis_repeat                     -   int(0),
    analysis_file_cache                 -   bool(no),
    termination_check                   -   bool(no),
    verbose_check_termination           -   bool(no),
    structure_sharing_analysis          -   bool(no),
    structure_sharing_widening          -   int(0),
    structure_reuse_analysis            -   bool(no),
    structure_reuse_constraint        -   string("within_n_cells_difference"),
    structure_reuse_constraint_arg      -   int(0),
    structure_reuse_max_conditions      -   int(10),
    structure_reuse_repeat              -   int(0),
    structure_reuse_free_cells          -   bool(no),
    termination                         -   bool(no),
    termination_single_args             -   int(0),
    termination_norm                    -   string("total"),
    termination_error_limit             -   int(3),
    termination_path_limit              -   int(256),
    termination2                        -   bool(no),
    termination2_norm                   -   string("total"),
    check_termination2                  -   bool(no),
    verbose_check_termination2          -   bool(no),
    widening_limit                      -   int(4),
    arg_size_analysis_only              -   bool(no),
    propagate_failure_constrs           -   bool(yes),
    % XXX This is just a guess - I'm not sure what sensible
    % value for this is.
    term2_maximum_matrix_size           -   int(70),
    analyse_exceptions                  -   bool(no),
    analyse_closures                    -   bool(no),
    analyse_trail_usage                 -   bool(no),
    optimize_trail_usage                -   bool(no),
    optimize_region_ops                 -   bool(no),
    analyse_mm_tabling                  -   bool(no)
]).
option_defaults_2(optimization_option, [
    % Optimization options
    %
    % IMPORTANT: the default here should be all optimizations OFF.
    % Optimizations should be enabled by the appropriate
    % optimization level in the opt_level table.

    % HLDS
    allow_inlining                      -   bool(yes),
    inlining                            -   bool_special,
    inline_simple                       -   bool(no),
    inline_builtins                     -   bool(yes),
    inline_single_use                   -   bool(no),
    inline_call_cost                    -   int(0),
    inline_compound_threshold           -   int(0),
    inline_simple_threshold             -   int(5),
                                        % Has no effect until
                                        % --inline-simple is enabled.
    inline_vars_threshold               -   int(100),
    intermod_inline_simple_threshold    -   int(5),
                                        % Has no effect until
                                        % --intermodule-optimization.
    inline_linear_tail_rec_sccs         -   bool(no),
    inline_linear_tail_rec_sccs_max_extra -   int(0),
    from_ground_term_threshold          -   int(5),
    enable_const_struct                 -   bool(yes),
    common_struct                       -   bool(no),
    common_struct_preds                 -   string(""),
    constraint_propagation              -   bool(no),
    local_constraint_propagation        -   bool(no),
    optimize_duplicate_calls            -   bool(no),
    constant_propagation                -   bool(no),
    excess_assign                       -   bool(no),
    test_after_switch                   -   bool(no),
    optimize_format_calls               -   bool(yes),
    loop_invariants                     -   bool(no),
    optimize_saved_vars_const           -   bool(no),
    optimize_saved_vars_cell            -   bool(no),
    optimize_saved_vars_cell_loop       -   bool(yes),
    optimize_saved_vars_cell_full_path  -   bool(yes),
    optimize_saved_vars_cell_on_stack   -   bool(yes),
    optimize_saved_vars_cell_candidate_headvars -   bool(yes),
    optimize_saved_vars_cell_cv_store_cost - int(3),
    optimize_saved_vars_cell_cv_load_cost  - int(1),
    optimize_saved_vars_cell_fv_store_cost - int(1),
    optimize_saved_vars_cell_fv_load_cost  - int(1),
    optimize_saved_vars_cell_op_ratio   -   int(100),
    optimize_saved_vars_cell_node_ratio -   int(100),
    optimize_saved_vars_cell_all_path_node_ratio    - int(100),
    optimize_saved_vars_cell_include_all_candidates - bool(yes),
    optimize_saved_vars                 -   bool_special,
    delay_construct                     -   bool(no),
    follow_code                         -   bool(no),
    optimize_unused_args                -   bool(no),
    intermod_unused_args                -   bool(no),
    optimize_higher_order               -   bool(no),
    higher_order_size_limit             -   int(20),
    higher_order_arg_limit              -   int(10),
    unneeded_code                       -   bool(no),
    unneeded_code_copy_limit            -   int(10),
    unneeded_code_debug                 -   bool(no),
    unneeded_code_debug_pred_name       -   accumulating([]),
    type_specialization                 -   bool(no),
    user_guided_type_specialization     -   bool(no),
    introduce_accumulators              -   bool(no),
    optimize_constructor_last_call_accumulator -    bool(no),
    optimize_constructor_last_call_null -   bool(no),
    optimize_constructor_last_call      -   bool(no),
    optimize_dead_procs                 -   bool(no),
    deforestation                       -   bool(no),
    deforestation_depth_limit           -   int(4),
    deforestation_cost_factor           -   int(1000),
    deforestation_vars_threshold        -   int(200),
    deforestation_size_threshold        -   int(15),
    untuple                             -   bool(no),
    tuple                               -   bool(no),
    tuple_trace_counts_file             -   string(""),
    tuple_costs_ratio                   -   int(100),
    tuple_min_args                      -   int(4),
    inline_par_builtins                 -   bool(no),
    always_specialize_in_dep_par_conjs  -   bool(no),
    allow_some_paths_only_waits         -   bool(yes),
    region_analysis                     -   bool(no),

    % HLDS -> LLDS
    smart_indexing                      -   bool(no),
    smart_atomic_indexing               -   bool(yes),
    smart_string_indexing               -   bool(yes),
    smart_tag_indexing                  -   bool(yes),
    smart_float_indexing                -   bool(yes),

    dense_switch_req_density            -   int(25),
                                        % Minimum density before using
                                        % a dense switch.
    lookup_switch_req_density           -   int(25),
                                        % Minimum density before using
                                        % a lookup switch.
    dense_switch_size                   -   int(4),
    lookup_switch_size                  -   int(4),
    string_trie_switch_size             -   int(16),
    string_hash_switch_size             -   int(8),
    string_binary_switch_size           -   int(4),
    tag_switch_size                     -   int(3),
    try_switch_size                     -   int(3),
    binary_switch_size                  -   int(4),
    switch_single_rec_base_first        -   bool(no),
    switch_multi_rec_base_first         -   bool(yes),
    static_ground_cells                 -   bool(no),
    static_ground_floats                -   bool(no),
    static_ground_int64s                -   bool(no),
    static_code_addresses               -   bool(no),
    use_atomic_cells                    -   bool(no),
    middle_rec                          -   bool(no),
    simple_neg                          -   bool(no),
    allow_hijacks                       -   bool(yes),

    % MLDS
    optimize_tailcalls                  -   bool(no),
    optimize_initializations            -   bool(no),
    eliminate_unused_mlds_assigns       -   bool(yes),
    eliminate_local_vars                -   bool(no),
    generate_trail_ops_inline           -   bool(yes),

    % LLDS
    common_data                         -   bool(no),
    common_layout_data                  -   bool(yes),
    optimize                            -   bool(no),
    optimize_peep                       -   bool(no),
    optimize_peep_mkword                -   bool(no),
    optimize_jumps                      -   bool(no),
    optimize_fulljumps                  -   bool(no),
    pessimize_tailcalls                 -   bool(no),
    checked_nondet_tailcalls            -   bool(no),
    use_local_vars                      -   bool(no),
    local_var_access_threshold          -   int(2),
    standardize_labels                  -   bool(no),
    optimize_labels                     -   bool(no),
    optimize_dups                       -   bool(no),
    optimize_proc_dups                  -   bool(no),
    optimize_frames                     -   bool(no),
    optimize_delay_slot                 -   bool(no),
    optimize_reassign                   -   bool(no),
    optimize_repeat                     -   int(0),
    layout_compression_limit            -   int(4000),

    % LLDS -> C
    use_macro_for_redo_fail             -   bool(no),
    emit_c_loops                        -   bool(no),
    procs_per_c_function                -   int(1),
    everything_in_one_c_function        -   special,
    local_thread_engine_base            -   bool(yes),

    % Erlang
    erlang_switch_on_strings_as_atoms   -   bool(no)
]).
option_defaults_2(target_code_compilation_option, [
    % Target code compilation options
    target_debug                        -   bool(no),

    % C
    cc                                  -   string("gcc"),
                                        % The `mmc' script will override the
                                        % default with a value determined at
                                        % configuration time.
    c_include_directory                 -   accumulating([]),
                                        % The `mmc' script will override the
                                        % default with a value determined at
                                        % configuration time.
    c_optimize                          -   bool(no),
    ansi_c                              -   bool(yes),
    inline_alloc                        -   bool(no),
    cflags                              -   accumulating([]),
    quoted_cflag                        -   string_special,

    gcc_flags                           -   accumulating([]),
    quoted_gcc_flag                     -   string_special,
    clang_flags                         -   accumulating([]),
    quoted_clang_flag                   -   string_special,
    msvc_flags                          -   accumulating([]),
    quoted_msvc_flag                    -   string_special,

    cflags_for_warnings                 -   string(""),
                                        % The `mmc' script will override the
                                        % default with values determined at
                                        % configuration time.
    cflags_for_sanitizers               -   string(""),
    cflags_for_optimization             -   string("-O"),
    cflags_for_ansi                     -   string(""),
    cflags_for_regs                     -   string(""),
    cflags_for_gotos                    -   string(""),
    cflags_for_threads                  -   string(""),
    cflags_for_debug                    -   string("-g"),
    cflags_for_pic                      -   string(""),
    c_flag_to_name_object_file          -   string("-o "),
    object_file_extension               -   string(".o"),
    pic_object_file_extension           -   string(".o"),
    c_compiler_type                     -   string("gcc"),
    csharp_compiler_type                -   string("mono"),
                                        % The `mmc' script will override the
                                        % default with a value determined at
                                        % configuration time for the above
                                        % two options
    % Java
    java_compiler                       -   string("javac"),
    java_interpreter                    -   string("java"),
    java_flags                          -   accumulating([]),
    quoted_java_flag                    -   string_special,
    java_classpath                      -   accumulating([]),
    java_object_file_extension          -   string(".class"),

    % C#
    csharp_compiler                     -   string("csc"),
    csharp_flags                        -   accumulating([]),
    quoted_csharp_flag                  -   string_special,
    cli_interpreter                     -   string(""),

    % Erlang
    erlang_compiler                     -   string("erlc"),
    erlang_interpreter                  -   string("erl"),
    erlang_flags                        -   accumulating([]),
    quoted_erlang_flag                  -   string_special,
    erlang_include_directory            -   accumulating([]),
    erlang_object_file_extension        -   string(".beam"),
    erlang_native_code                  -   bool(no),
    erlang_inhibit_trivial_warnings     -   bool(yes)
]).
option_defaults_2(link_option, [
    % Link Options
    output_file_name                    -   string(""),
                                        % If the output_file_name is an empty
                                        % string, we use the name of the first
                                        % module on the command line.
    ld_flags                            -   accumulating([]),
    quoted_ld_flag                      -   string_special,
    ld_libflags                         -   accumulating([]),
    quoted_ld_libflag                   -   string_special,
    link_library_directories            -   accumulating([]),
    runtime_link_library_directories    -   accumulating([]),
    default_runtime_library_directory   -   bool(yes),
    link_libraries                      -   accumulating([]),
    link_objects                        -   accumulating([]),
    mercury_library_directory_special   -   string_special,
    mercury_library_directories         -   accumulating([]),
    search_library_files_directory_special - string_special,
    search_library_files_directories    -   accumulating([]),
    mercury_library_special             -   string_special,
    mercury_libraries                   -   accumulating([]),
    mercury_standard_library_directory  -   maybe_string(no),
                                        % The Mercury.config file will set the
                                        % default standard library directory.
    mercury_standard_library_directory_special - maybe_string_special,
    init_file_directories               -   accumulating([]),
    init_files                          -   accumulating([]),
    trace_init_files                    -   accumulating([]),
    linkage                             -   string("shared"),
    linkage_special                     -   string_special,
    mercury_linkage                     -   string("shared"),
    mercury_linkage_special             -   string_special,
    demangle                            -   bool(yes),
    strip                               -   bool(yes),
    main                                -   bool(yes),
    allow_undefined                     -   bool(yes),
    use_readline                        -   bool(yes),
    runtime_flags                       -   accumulating([]),
    extra_initialization_functions      -   bool(no),
    frameworks                          -   accumulating([]),
    framework_directories               -   accumulating([]),
    sign_assembly                       -   string(""),
    cstack_reserve_size                 -   int(-1),

    shared_library_extension            -   string(".so"),
                                        % The `mmc' script will override the
                                        % default with a value determined at
                                        % configuration time.
    library_extension                   -   string(".a"),
    executable_file_extension           -   string(""),
    link_executable_command             -   string("gcc"),
    link_shared_lib_command             -   string("gcc -shared"),
    create_archive_command              -   string("ar"),
    create_archive_command_output_flag  -   string(""),
    create_archive_command_flags        -   accumulating([]), % "cr"
    ranlib_command                      -   string(""),
    ranlib_flags                        -   string(""),
    mkinit_command                      -   string("mkinit"),
    mkinit_erl_command                  -   string("mkinit_erl"),
    demangle_command                    -   string("mdemangle"),
    filtercc_command                    -   string("mfiltercc"),
    filterjavac_command                 -   string("mfilterjavac"),
    trace_libs                          -   string(""),
    thread_libs                         -   string(""),
    hwloc_libs                          -   string(""),
    hwloc_static_libs                   -   string(""),
    shared_libs                         -   string(""),
    math_lib                            -   string(""),
    readline_libs                       -   string(""),
    linker_opt_separator                -   string(""),
    linker_debug_flags                  -   string("-g"),
    shlib_linker_debug_flags            -   string("-g"),
    linker_sanitizer_flags              -   string(""),
    linker_trace_flags                  -   string(""),
    shlib_linker_trace_flags            -   string(""),
    linker_thread_flags                 -   string(""),
    shlib_linker_thread_flags           -   string(""),
    linker_static_flags                 -   string("-static"),
    linker_strip_flag                   -   string("-s"),
    linker_link_lib_flag                -   string("-l"),
    linker_link_lib_suffix              -   string(""),
    shlib_linker_link_lib_flag          -   string("-l"),
    shlib_linker_link_lib_suffix        -   string(""),
    linker_path_flag                    -   string("-L"),
    linker_rpath_flag                   -   string("-Wl,-rpath"),
    linker_rpath_separator              -   string(" -Wl,-rpath"),
    shlib_linker_rpath_flag             -   string("-Wl,-rpath"),
    shlib_linker_rpath_separator        -   string(" -Wl,-rpath"),
    linker_allow_undefined_flag         -   string(""),
    linker_error_undefined_flag         -   string("-Wl,-no-undefined"),
    shlib_linker_use_install_name       -   bool(no),
    shlib_linker_install_name_flag      -   string("-install_name "),
    shlib_linker_install_name_path      -   string(""),
    strip_executable_command            -   string(""),
    strip_executable_shared_flags       -   string(""),
    strip_executable_static_flags       -   string(""),
    java_archive_command                -   string("jar")
]).
option_defaults_2(build_system_option, [
    % Build System Options
    only_opmode_make                    -   bool(no),
    rebuild                             -   bool(no),
    keep_going                          -   bool(no),
    jobs                                -   int(1),
    track_flags                         -   bool(no),
    invoked_by_mmc_make                 -   bool(no),
    pre_link_command                    -   maybe_string(no),
    extra_init_command                  -   maybe_string(no),
    install_prefix                      -   string("/usr/local/"),
    use_symlinks                        -   bool(yes),

    % If `--mercury-stdlib-dir' is set, `--mercury-config-dir'
    % must also be set. This invariant is maintained by the `special' variants
    % of the options.
    mercury_configuration_directory_special - string_special,
    mercury_configuration_directory     -   maybe_string(no),
    install_command                     -   string("cp"),
    install_command_dir_option          -   string("-R"),
    detect_libgrades                    -   bool(yes),
    libgrades                           -   accumulating([]),
    libgrades_include_components        -   accumulating([]),
    libgrades_exclude_components        -   accumulating([]),
    lib_linkages                        -   accumulating([]),
    flags_file                          -   file_special,
    options_files                       -   accumulating(["Mercury.options"]),

    config_file                         -   maybe_string(yes("")),
                                        % yes("") means unset.
    options_search_directories          -   accumulating(["."]),
    use_subdirs                         -   bool(no),
    use_grade_subdirs                   -   bool(no),
    search_directories                  -   accumulating(["."]),
    intermod_directories                -   accumulating([]),
    use_search_directories_for_intermod -   bool(yes),
    libgrade_install_check              -   bool(yes),
    order_make_by_timestamp             -   bool(no),
    show_make_times                     -   bool(no),
    extra_library_header                -   accumulating([]),
    restricted_command_line             -   bool(no),
    env_type                            -   string_special,
    host_env_type                       -   string("posix"),
    system_env_type                     -   string(""),
    target_env_type                     -   string("posix")
]).
option_defaults_2(miscellaneous_option, [
    % Miscellaneous Options
    filenames_from_stdin                -   bool(no),
    typecheck_ambiguity_warn_limit      -   int(50),
    typecheck_ambiguity_error_limit     -   int(3000),
    help                                -   bool(no),
    version                             -   bool(no),
    target_arch                         -   string(""),
    cross_compiling                     -   bool(no),
    local_module_id                     -   accumulating([]),
    analysis_file_cache_dir             -   string(""),
    compiler_sufficiently_recent        -   bool(no),
    experiment                          -   string(""),
    experiment1                         -   bool(no),
    experiment2                         -   bool(no),
    experiment3                         -   bool(yes),
    experiment4                         -   bool(no),
    experiment5                         -   bool(no),
    ignore_par_conjunctions             -   bool(no),
    control_granularity                 -   bool(no),
    distance_granularity                -   int(0),
    implicit_parallelism                -   bool(no),
    feedback_file                       -   string(""),
    par_loop_control                    -   bool(no),
    par_loop_control_preserve_tail_recursion - bool(no)
]).

    % please keep this in alphabetic order
short_option('c', only_opmode_compile_only).
short_option('C', only_opmode_target_code_only).
short_option('d', dump_hlds).
short_option('D', dump_hlds_alias).
short_option('e', only_opmode_errorcheck_only).
short_option('E', verbose_errors).
short_option('f', only_opmode_generate_source_file_mapping).
short_option('F', framework_directories).
short_option('h', help).
short_option('H', highlevel_code).
short_option('i', only_opmode_make_interface).
short_option('j', jobs).
short_option('I', search_directories).
short_option('k', keep_going).
short_option('l', link_libraries).
short_option('L', link_library_directories).
short_option('m', only_opmode_make).
short_option('M', only_opmode_generate_dependencies).
short_option('n', line_numbers).
short_option('N', debug_modes).
short_option('o', output_file_name).
short_option('O', opt_level).
short_option('p', profiling).
short_option('P', only_opmode_convert_to_mercury).
short_option('r', rebuild).
short_option('R', runtime_link_library_directories).
short_option('s', grade).
short_option('S', statistics).
short_option('T', debug_types).
short_option('t', only_opmode_typecheck_only).
short_option('v', verbose).
short_option('V', very_verbose).
short_option('w', inhibit_warnings).
short_option('x', only_opmode_make_xml_documentation).
short_option('?', help).

% warning options
long_option("inhibit-warnings",         inhibit_warnings).
long_option("inhibit-style-warnings",   inhibit_style_warnings).
long_option("warn-accumulator-swaps",   warn_accumulator_swaps).
long_option("halt-at-warn",             halt_at_warn).
long_option("halt-at-syntax-errors",    halt_at_syntax_errors).
long_option("halt-at-auto-parallel-failure", halt_at_auto_parallel_failure).
long_option("warn-singleton-variables", warn_singleton_vars).
long_option("warn-singleton-vars",      warn_singleton_vars).
long_option("warn-overlapping-scopes",  warn_overlapping_scopes).
long_option("warn-det-decls-too-lax",   warn_det_decls_too_lax).
long_option("warn-inferred-erroneous",  warn_inferred_erroneous).
long_option("warn-nothing-exported",    warn_nothing_exported).
long_option("warn-unused-args",         warn_unused_args).
long_option("warn-interface-imports",   warn_interface_imports).
long_option("warn-interface-imports-in-parents",
                                        warn_interface_imports_in_parents).
long_option("warn-inconsistent-pred-order",
                    warn_inconsistent_pred_order_clauses).
long_option("warn-inconsistent-pred-order-clauses",
                    warn_inconsistent_pred_order_clauses).
long_option("warn-inconsistent-pred-order-foreign-procs",
                    warn_inconsistent_pred_order_foreign_procs).
long_option("warn-non-contiguous-decls",    warn_non_contiguous_decls).
long_option("warn-non-contiguous-clauses",  warn_non_contiguous_clauses).
long_option("warn-non-contiguous-foreign-procs",
                                        warn_non_contiguous_foreign_procs).
long_option("warn-non-stratification",  warn_non_stratification).
long_option("warn-missing-opt-files",   warn_missing_opt_files).
long_option("warn-missing-trans-opt-files", warn_missing_trans_opt_files).
long_option("warn-missing-trans-opt-deps",  warn_missing_trans_opt_deps).
long_option("warn-unification-cannot-succeed",
                                        warn_unification_cannot_succeed).
long_option("warn-simple-code",         warn_simple_code).
long_option("warn-duplicate-calls",     warn_duplicate_calls).
long_option("warn-implicit-stream-calls",   warn_implicit_stream_calls).
long_option("warn-missing-module-name", warn_missing_module_name).
long_option("warn-wrong-module-name",   warn_wrong_module_name).
long_option("warn-smart-recompilation", warn_smart_recompilation).
long_option("warn-undefined-options-variables",
                                        warn_undefined_options_variables).
long_option("warn-undefined-options-vars",
                                        warn_undefined_options_variables).
long_option("warn-suspicious-recursion", warn_suspicious_recursion).
long_option("warn-non-tail-recursion-self",
                                        warn_non_tail_recursion_self).
long_option("warn-non-tail-recursion-mutual",
                                        warn_non_tail_recursion_mutual).
long_option("warn-non-tail-recursion",  warn_non_tail_recursion).
long_option("warn-obvious-non-tail-recursion",
                                        warn_obvious_non_tail_recursion).
long_option("warn-target-code",         warn_target_code).
long_option("warn-up-to-date",          warn_up_to_date).
long_option("warn-stubs",               warn_stubs).
long_option("warn-dead-procs",          warn_dead_procs).
long_option("warn-dead-procedures",     warn_dead_procs).
long_option("warn-dead-preds",          warn_dead_preds).
long_option("warn-dead-predicates",     warn_dead_preds).
long_option("warn-table-with-inline",   warn_table_with_inline).
long_option("warn-non-term-special-preds", warn_non_term_special_preds).
long_option("warn-known-bad-format-calls", warn_known_bad_format_calls).
long_option("warn-only-one-format-string-error",
                    warn_only_one_format_string_error).
long_option("warn-unknown-format-calls", warn_unknown_format_calls).
long_option("warn-obsolete",            warn_obsolete).
long_option("warn-insts-without-matching-type",
                                        warn_insts_without_matching_type).
long_option("warn-insts-with-functors-without-type",
                    warn_insts_with_functors_without_type).
long_option("warn-unused-imports",      warn_unused_imports).
long_option("inform-ite-instead-of-switch", inform_ite_instead_of_switch).
long_option("inform-incomplete-switch", inform_incomplete_switch).
long_option("inform-incomplete-switch-threshold",
                    inform_incomplete_switch_threshold).
long_option("warn-unresolved-polymorphism", warn_unresolved_polymorphism).
long_option("warn-suspicious-foreign-procs", warn_suspicious_foreign_procs).
long_option("warn-suspicious-foreign-code", warn_suspicious_foreign_code).
long_option("warn-state-var-shadowing", warn_state_var_shadowing).
long_option("inform-inferred",          inform_inferred).
long_option("inform-inferred-types",    inform_inferred_types).
long_option("inform-inferred-modes",    inform_inferred_modes).
long_option("inform-suboptimal-packing",    inform_suboptimal_packing).

% verbosity options
long_option("verbose",                  verbose).
long_option("very-verbose",             very_verbose).
long_option("verbose-error-messages",   verbose_errors).
long_option("verbose-recompilation",    verbose_recompilation).
long_option("find-all-recompilation-reasons",
                                        find_all_recompilation_reasons).
long_option("verbose-make",             verbose_make).
long_option("verbose-commands",         verbose_commands).
long_option("output-compile-error-lines",   output_compile_error_lines).
long_option("report-cmd-line-args",     report_cmd_line_args).
long_option("report-cmd-line-args-in-doterr",
                                        report_cmd_line_args_in_doterr).
long_option("statistics",               statistics).
long_option("detailed-statistics",      detailed_statistics).
long_option("proc-size-statistics",     proc_size_statistics).
long_option("limit-error-contexts",     limit_error_contexts).
long_option("debug-types",              debug_types).
long_option("debug-modes",              debug_modes).
long_option("debug-modes-statistics",   debug_modes_statistics).
long_option("debug-modes-minimal",      debug_modes_minimal).
long_option("debug-modes-verbose",      debug_modes_verbose).
long_option("debug-modes-pred-id",      debug_modes_pred_id).
long_option("debug-dep-par-conj",       debug_dep_par_conj).
long_option("debug-determinism",        debug_det).
long_option("debug-det",                debug_det).
long_option("debug-code-gen-pred-id",   debug_code_gen_pred_id).
long_option("debug-termination",        debug_term).
long_option("debug-term",               debug_term).
long_option("debug-opt",                debug_opt).
long_option("debug-opt-pred-id",        debug_opt_pred_id).
long_option("debug-opt-pred-name",      debug_opt_pred_name).
long_option("debug-pd",                 debug_pd).
long_option("debug-liveness",           debug_liveness).
long_option("debug-stack-opt",          debug_stack_opt).
long_option("debug-make",               debug_make).
long_option("debug-closure",            debug_closure).
long_option("debug-trail-usage",        debug_trail_usage).
long_option("debug-mode-constraints",   debug_mode_constraints).
long_option("debug-intermodule-analysis",   debug_intermodule_analysis).
long_option("debug-mm-tabling-analysis",    debug_mm_tabling_analysis).
long_option("debug-indirect-reuse",         debug_indirect_reuse).
long_option("debug-type-rep",               debug_type_rep).

% output options (mutually exclusive)
long_option("generate-source-file-mapping",
    only_opmode_generate_source_file_mapping).
long_option("generate-dependency-file", only_opmode_generate_dependency_file).
long_option("generate-dependencies",    only_opmode_generate_dependencies).
long_option("generate-module-order",    generate_module_order).
long_option("generate-standalone-interface", generate_standalone_interface).
long_option("make-short-interface",     only_opmode_make_short_interface).
long_option("make-short-int",           only_opmode_make_short_interface).
long_option("make-interface",           only_opmode_make_interface).
long_option("make-int",                 only_opmode_make_interface).
long_option("make-private-interface",   only_opmode_make_private_interface).
long_option("make-priv-int",            only_opmode_make_private_interface).
long_option("make-optimization-interface",
    only_opmode_make_optimization_interface).
long_option("make-optimisation-interface",
    only_opmode_make_optimization_interface).
long_option("make-opt-int",
    only_opmode_make_optimization_interface).
long_option("make-transitive-optimization-interface",
    only_opmode_make_transitive_opt_interface).
long_option("make-transitive-optimisation-interface",
    only_opmode_make_transitive_opt_interface).
long_option("make-trans-opt",
    only_opmode_make_transitive_opt_interface).
long_option("make-analysis-registry",   only_opmode_make_analysis_registry).
long_option("make-xml-doc",             only_opmode_make_xml_documentation).
long_option("make-xml-documentation",   only_opmode_make_xml_documentation).
long_option("convert-to-mercury",       only_opmode_convert_to_mercury).
long_option("convert-to-Mercury",       only_opmode_convert_to_mercury).
long_option("pretty-print",             only_opmode_convert_to_mercury).
long_option("typecheck-only",           only_opmode_typecheck_only).
long_option("errorcheck-only",          only_opmode_errorcheck_only).
long_option("target-code-only",         only_opmode_target_code_only).
long_option("compile-only",             only_opmode_compile_only).
long_option("compile-to-shared-lib",    compile_to_shared_lib).
long_option("output-grade-string",      only_opmode_output_grade_string).
long_option("output-link-command",      only_opmode_output_link_command).
long_option("output-shared-lib-link-command",
    only_opmode_output_shared_lib_link_command).
long_option("output-libgrades",         only_opmode_output_libgrades).
long_option("output-cc",                only_opmode_output_cc).
long_option("output-cc-type",           only_opmode_output_c_compiler_type).
long_option("output-c-compiler-type",   only_opmode_output_c_compiler_type).
long_option("output-csharp-compiler",   only_opmode_output_csharp_compiler).
long_option("output-csharp-compiler-type",
    only_opmode_output_csharp_compiler_type).
long_option("output-cflags",            only_opmode_output_cflags).
long_option("output-library-link-flags",
    only_opmode_output_library_link_flags).
long_option("output-grade-defines",     only_opmode_output_grade_defines).
long_option("output-c-include-directory-flags",
    only_opmode_output_c_include_directory_flags).
long_option("output-c-include-dir-flags",
    only_opmode_output_c_include_directory_flags).
long_option("output-target-arch",       only_opmode_output_target_arch).
long_option("output-class-directory",   only_opmode_output_class_dir).
long_option("output-class-dir",         only_opmode_output_class_dir).

% aux output options
long_option("smart-recompilation",      smart_recompilation).
long_option("assume-gmake",             assume_gmake).
long_option("generate-mmc-make-module-dependencies",
                                        generate_mmc_make_module_dependencies).
long_option("generate-mmc-deps",        generate_mmc_make_module_dependencies).
long_option("ssdb-trace",               ssdb_trace_level).
long_option("link-ssdb-libs",           link_ssdb_libs).
long_option("link-ssdebug-libs",        link_ssdb_libs).
long_option("trace",                    trace_level).
long_option("trace-optimised",          trace_optimized).
long_option("trace-optimized",          trace_optimized).
long_option("trace-prof",               trace_prof).
long_option("trace-table-io",           trace_table_io).
long_option("trace-table-io-only-retry",    trace_table_io_only_retry).
long_option("trace-table-io-states",    trace_table_io_states).
long_option("trace-table-io-require",   trace_table_io_require).
long_option("trace-table-io-all",       trace_table_io_all).
long_option("trace-flag",               trace_goal_flags).
long_option("profile-optimised",        prof_optimized).
long_option("profile-optimized",        prof_optimized).
long_option("exec-trace-tail-rec",      exec_trace_tail_rec).
long_option("suppress-trace",           suppress_trace).
long_option("force-disable-tracing",    force_disable_tracing).
long_option("delay-death",              delay_death).
long_option("delay-death-max-vars",     delay_death_max_vars).
long_option("stack-trace-higher-order", stack_trace_higher_order).
long_option("force-disable-ssdebug",    force_disable_ssdebug).
long_option("generate-bytecode",        generate_bytecode).
long_option("line-numbers",             line_numbers).
long_option("line-numbers-around-foreign-code",
                                        line_numbers_around_foreign_code).
long_option("line-numbers-for-c-headers", line_numbers_for_c_headers).
long_option("auto-comments",            auto_comments).
long_option("frameopt-comments",        frameopt_comments).
long_option("max-error-line-width",     max_error_line_width).
long_option("show-definitions",         show_definitions).
long_option("show-definition-line-counts",  show_definition_line_counts).
long_option("show-all-type-repns",              show_all_type_repns).
long_option("show-all-type-representations",    show_all_type_repns).
long_option("show-local-type-repns",            show_local_type_repns).
long_option("show-local-type-representations",  show_local_type_repns).
long_option("show-developer-type-repns",            show_developer_type_repns).
long_option("show-developer-type-representations",  show_developer_type_repns).
long_option("show-dependency-graph",    show_dependency_graph).
long_option("imports-graph",            imports_graph).
long_option("dump-trace-counts",        dump_trace_counts).
long_option("dump-hlds",                dump_hlds).
long_option("hlds-dump",                dump_hlds).
long_option("dump-hlds-pred-id",        dump_hlds_pred_id).
long_option("dump-hlds-pred-name",      dump_hlds_pred_name).
long_option("dump-hlds-pred-name-order", dump_hlds_pred_name_order).
long_option("dump-hlds-alias",          dump_hlds_alias).
long_option("dump-hlds-spec-preds",     dump_hlds_spec_preds).
long_option("dump-hlds-spec-preds-for", dump_hlds_spec_preds_for).
long_option("dump-hlds-options",        dump_hlds_options).
long_option("dump-hlds-inst-limit",     dump_hlds_inst_limit).
long_option("dump-hlds-file-suffix",    dump_hlds_file_suffix).
long_option("dump-same-hlds",           dump_same_hlds).
long_option("dump-mlds",                dump_mlds).
long_option("dump-mlds-pred-name",      dump_mlds_pred_name).
long_option("mlds-dump",                dump_mlds).
long_option("verbose-dump-mlds",        verbose_dump_mlds).
long_option("verbose-mlds-dump",        verbose_dump_mlds).
long_option("mode-constraints",         mode_constraints).
long_option("simple-mode-constraints",  simple_mode_constraints).
long_option("prop-mode-constraints",    prop_mode_constraints).
long_option("compute-goal-modes",       compute_goal_modes).
long_option("propagate-mode-constraints",   prop_mode_constraints).
long_option("benchmark-modes",          benchmark_modes).
long_option("benchmark-modes-repeat",   benchmark_modes_repeat).

% language semantics options
long_option("reorder-conj",         reorder_conj).
long_option("reorder-disj",         reorder_disj).
long_option("fully-strict",         fully_strict).
long_option("strict-sequential",    strict_sequential).
long_option("allow-stubs",          allow_stubs).
long_option("infer-all",            infer_all).
long_option("infer-types",          infer_types).
long_option("infer-modes",          infer_modes).
long_option("infer-determinism",    infer_det).
long_option("infer-det",            infer_det).
long_option("type-inference-iteration-limit", type_inference_iteration_limit).
long_option("mode-inference-iteration-limit", mode_inference_iteration_limit).
long_option("event-set-file-name",  event_set_file_name).

% compilation model options
long_option("grade",                grade).
% target selection options
long_option("target",               target).
long_option("compile-to-c",         compile_to_c).
long_option("compile-to-C",         compile_to_c).
long_option("java",                 java).
long_option("Java",                 java).
long_option("java-only",            java_only).
long_option("Java-only",            java_only).
long_option("csharp",               csharp).
long_option("C#",                   csharp).
long_option("csharp-only",          csharp_only).
long_option("C#-only",              csharp_only).
long_option("erlang",               erlang).
long_option("Erlang",               erlang).
long_option("erlang-only",          erlang_only).
long_option("Erlang-only",          erlang_only).
% Optional features compilation model options:
% (a) debugging
long_option("debug",                exec_trace).
long_option("decl-debug",           decl_debug).
long_option("ssdb",                 source_to_source_debug).
long_option("ss-debug",             source_to_source_debug).
long_option("source-to-source-debug", source_to_source_debug).
% (b) profiling
long_option("profiling",            profiling).
long_option("time-profiling",       time_profiling).
long_option("memory-profiling",     memory_profiling).
long_option("deep-profiling",       deep_profiling).
long_option("profile-calls",        profile_calls).
long_option("profile-time",         profile_time).
long_option("profile-memory",       profile_memory).
long_option("profile-deep",         profile_deep).
long_option("use-activation-counts",    use_activation_counts).
long_option("pre-prof-transforms-simplify", pre_prof_transforms_simplify).
long_option("pre-implicit-parallelism-simplify",
    pre_implicit_parallelism_simplify).
long_option("coverage-profiling",   coverage_profiling).
long_option("coverage-profiling-via-calls",
    coverage_profiling_via_calls).
long_option("coverage-profiling-static",
    coverage_profiling_static).
long_option("profile-deep-coverage-after-goal",
    profile_deep_coverage_after_goal).
long_option("profile-deep-coverage-branch-ite",
    profile_deep_coverage_branch_ite).
long_option("profile-deep-coverage-branch-switch",
    profile_deep_coverage_branch_switch).
long_option("profile-deep-coverage-branch-disj",
    profile_deep_coverage_branch_disj).
long_option("profile-deep-coverage-use-portcounts",
    profile_deep_coverage_use_portcounts).
long_option("profile-deep-coverage-use-trivial",
    profile_deep_coverage_use_trivial).
long_option("profile-for-implicit-parallelism",
    profile_for_feedback).
long_option("profile-for-feedback",
    profile_for_feedback).
long_option("use-zeroing-for-ho-cycles",
    use_zeroing_for_ho_cycles).
long_option("use-lots-of-ho-specialization",
    use_lots_of_ho_specialization).
long_option("deep-profile-tail-recursion",
    deep_profile_tail_recursion).
long_option("record-term-sizes-as-words", record_term_sizes_as_words).
long_option("record-term-sizes-as-cells", record_term_sizes_as_cells).
long_option("experimental-complexity",  experimental_complexity).
long_option("region-analysis",      region_analysis).
% (c) miscellaneous optional features
long_option("gc",                   gc).
long_option("garbage-collection",   gc).
long_option("parallel",             parallel).
long_option("use-trail",            use_trail).
long_option("trail-segments",       trail_segments).
long_option("type-layout",          type_layout).
long_option("maybe-thread-safe",    maybe_thread_safe_opt).
long_option("extend-stacks-when-needed",    extend_stacks_when_needed).
long_option("stack-segments",       stack_segments).
long_option("use-regions",          use_regions).
long_option("use-alloc-regions",    use_alloc_regions).
long_option("use-regions-debug",    use_regions_debug).
long_option("use-regions-profiling",use_regions_profiling).
% Data representation options
long_option("use-minimal-model-stack-copy", use_minimal_model_stack_copy).
long_option("use-minimal-model-own-stacks", use_minimal_model_own_stacks).
long_option("minimal-model-debug",  minimal_model_debug).
long_option("pregenerated-dist",    pregenerated_dist).
long_option("single-prec-float",    single_prec_float).
long_option("single-precision-float",   single_prec_float).
long_option("tags",                 tags).
long_option("num-tag-bits",         num_ptag_bits). % for historical reasons
long_option("num-ptag-bits",        num_ptag_bits).
long_option("bits-per-word",        bits_per_word).
long_option("bytes-per-word",       bytes_per_word).
long_option("conf-low-tag-bits",    conf_low_ptag_bits). % for historical ...
long_option("conf-low-ptag-bits",   conf_low_ptag_bits).
long_option("unboxed-float",        unboxed_float).
long_option("unboxed-int64s",       unboxed_int64s).
long_option("unboxed-no-tag-types", unboxed_no_tag_types).
long_option("arg-pack-bits",        arg_pack_bits).
long_option("allow-double-word-fields", allow_double_word_fields).
long_option("allow-double-word-ints", allow_double_word_ints).
long_option("allow-packing-dummies", allow_packing_dummies).
long_option("allow-packing-ints",   allow_packing_ints).
long_option("allow-packing-chars",  allow_packing_chars).
long_option("allow-packing-local-sectags",  allow_packing_local_sectags).
long_option("allow-packing-remote-sectags", allow_packing_remote_sectags).
long_option("allow-packing-mini-types",     allow_packing_mini_types).
long_option("allow-packed-unify-compare",   allow_packed_unify_compare).
long_option("sync-term-size",       sync_term_size).
long_option("highlevel-data",       highlevel_data).
long_option("high-level-data",      highlevel_data).
% LLDS back-end compilation model options
long_option("gcc-non-local-gotos",  gcc_non_local_gotos).
long_option("gcc-global-registers", gcc_global_registers).
long_option("asm-labels",           asm_labels).
long_option("use-float-registers",  use_float_registers).
% MLDS back-end compilation model options
long_option("highlevel-code",       highlevel_code).
long_option("high-level-code",      highlevel_code).
long_option("highlevel-C",          highlevel_code).
long_option("highlevel-c",          highlevel_code).
long_option("high-level-C",         highlevel_code).
long_option("high-level-c",         highlevel_code).
long_option("det-copy-out",         det_copy_out).
long_option("nondet-copy-out",      nondet_copy_out).
long_option("put-commit-in-own-func",   put_commit_in_own_func).
long_option("put-nondet-env-on-heap",   put_nondet_env_on_heap).

% internal use options
long_option("backend-foreign-languages", backend_foreign_languages).
long_option("agc-stack-layout",     agc_stack_layout).
long_option("basic-stack-layout",   basic_stack_layout).
long_option("procid-stack-layout",  procid_stack_layout).
long_option("trace-stack-layout",   trace_stack_layout).
long_option("body-typeinfo-liveness",   body_typeinfo_liveness).
long_option("can-compare-constants-as-ints",    can_compare_constants_as_ints).
long_option("pretest-equality-cast-pointers",   pretest_equality_cast_pointers).
long_option("can-compare-compound-values",      can_compare_compound_values).
long_option("order-constructors-for-erlang",
                                    order_constructors_for_erlang).
long_option("delay-partial-instantiations", delay_partial_instantiations).
long_option("allow-defn-of-builtins",           allow_defn_of_builtins).
long_option("type-ctor-info",       type_ctor_info).
long_option("type-ctor-layout",     type_ctor_layout).
long_option("type-ctor-functors",   type_ctor_functors).
long_option("new-type-class-rtti",  new_type_class_rtti).
long_option("rtti-line-numbers",    rtti_line_numbers).
long_option("disable-mm-pneg",      disable_minimal_model_stack_copy_pneg).
long_option("disable-mm-cut",       disable_minimal_model_stack_copy_cut).
long_option("disable-trail-ops",    disable_trail_ops).
long_option("size-region-ite-fixed",            size_region_ite_fixed).
long_option("size-region-disj-fixed",           size_region_disj_fixed).
long_option("size-region-commit-fixed",         size_region_commit_fixed).
long_option("size-region-ite-protect",          size_region_ite_protect).
long_option("size-region-ite-snapshot",         size_region_ite_snapshot).
long_option("size-region-semi-disj-protect",    size_region_semi_disj_protect).
long_option("size-region-disj-snapshot",        size_region_disj_snapshot).
long_option("size-region-commit-entry",         size_region_commit_entry).
long_option("allow-multi-arm-switches", allow_multi_arm_switches).
long_option("type-check-constraints",   type_check_constraints).

% code generation options
long_option("low-level-debug",      low_level_debug).
long_option("table-debug",          table_debug).
long_option("trad-passes",          trad_passes).
long_option("parallel-liveness",    parallel_liveness).
long_option("parallel-code-gen",    parallel_code_gen).
long_option("reclaim-heap-on-failure",  reclaim_heap_on_failure).
long_option("reclaim-heap-on-semidet-failure",
                                    reclaim_heap_on_semidet_failure).
long_option("reclaim-heap-on-nondet-failure",
                                    reclaim_heap_on_nondet_failure).
long_option("branch-delay-slot",    have_delay_slot).
long_option("have-delay-slot",      have_delay_slot).
long_option("num-real-r-regs",      num_real_r_regs).
long_option("num-real-f-regs",      num_real_f_regs).
long_option("num-real-r-temps",     num_real_r_temps).
long_option("num-real-f-temps",     num_real_f_temps).
long_option("num-real-temps",       num_real_r_temps).  % obsolete
long_option("max-jump-table-size",  max_jump_table_size).
% long_option("max-spec-do-call-closure",
%                   max_specialized_do_call_closure).
% long_option("max-spec-do-call-class-method",
%                   max_specialized_do_call_class_method).
long_option("compare-specialization",   compare_specialization).
long_option("should-pretest-equality",  should_pretest_equality).
long_option("fact-table-max-array-size",fact_table_max_array_size).
long_option("fact-table-hash-percent-full",
                    fact_table_hash_percent_full).
long_option("prefer-switch",        prefer_switch).
long_option("prefer-while-loop-over-jump-self",
                                    prefer_while_loop_over_jump_self).
long_option("prefer-while-loop-over-jump-mutual",
                                    prefer_while_loop_over_jump_mutual).
long_option("opt-no-return-calls",  opt_no_return_calls).
long_option("debug-class-init",     debug_class_init).

% optimization options

long_option("opt-level",            opt_level).
long_option("optimization-level",   opt_level).
long_option("optimisation-level",   opt_level).
long_option("opt-space",            opt_space).
long_option("optimize-space",       opt_space).
long_option("optimise-space",       opt_space).
long_option("intermod-opt",        intermodule_optimization).
long_option("intermodule-optimization", intermodule_optimization).
long_option("intermodule-optimisation", intermodule_optimization).
long_option("read-opt-files-transitively", read_opt_files_transitively).
long_option("use-opt-files",        use_opt_files).
long_option("use-trans-opt-files",  use_trans_opt_files).
long_option("transitive-intermodule-optimization",
                    transitive_optimization).
long_option("transitive-intermodule-optimisation",
                    transitive_optimization).
long_option("trans-intermod-opt",   transitive_optimization).
long_option("intermodule-analysis", intermodule_analysis).
long_option("analysis-repeat",      analysis_repeat).
long_option("analysis-file-cache",  analysis_file_cache).

% HLDS->HLDS optimizations
long_option("inlining",             inlining).
long_option("inline-simple",        inline_simple).
long_option("inline-builtins",      inline_builtins).
long_option("inline-single-use",    inline_single_use).
long_option("inline-call-cost",     inline_call_cost).
long_option("inline-compound-threshold",    inline_compound_threshold).
long_option("inline-simple-threshold",      inline_simple_threshold).
long_option("intermod-inline-simple-threshold",
                                    intermod_inline_simple_threshold).
long_option("inline-linear-tail-rec-sccs",  inline_linear_tail_rec_sccs).
long_option("inline-linear-tail-rec-sccs-max-extra",
                                    inline_linear_tail_rec_sccs_max_extra).
long_option("from-ground-term-threshold",
                                    from_ground_term_threshold).
long_option("inline-vars-threshold",        inline_vars_threshold).
long_option("const-struct",         enable_const_struct).
long_option("common-struct",        common_struct).
long_option("common-struct-preds",  common_struct_preds).
long_option("excess-assign",        excess_assign).
long_option("test-after-switch",    test_after_switch).
long_option("optimize-format-calls",         optimize_format_calls).
long_option("optimize-duplicate-calls", optimize_duplicate_calls).
long_option("optimise-duplicate-calls", optimize_duplicate_calls).
long_option("optimise-constant-propagation", constant_propagation).
long_option("optimize-constant-propagation", constant_propagation).
long_option("optimize-saved-vars",  optimize_saved_vars).
long_option("optimise-saved-vars",  optimize_saved_vars).
long_option("loop-invariants",      loop_invariants).
long_option("optimize-saved-vars-const",    optimize_saved_vars_const).
long_option("optimise-saved-vars-const",    optimize_saved_vars_const).
long_option("optimize-saved-vars-cell", optimize_saved_vars_cell).
long_option("optimise-saved-vars-cell", optimize_saved_vars_cell).
long_option("osv-loop",             optimize_saved_vars_cell_loop).
long_option("osv-full-path",        optimize_saved_vars_cell_full_path).
long_option("osv-on-stack",         optimize_saved_vars_cell_on_stack).
long_option("osv-cand-head",
                            optimize_saved_vars_cell_candidate_headvars).
% The next four options are used by tupling.m as well; changes to them
% may require changes there as well.
long_option("osv-cvstore-cost",     optimize_saved_vars_cell_cv_store_cost).
long_option("osv-cvload-cost",      optimize_saved_vars_cell_cv_load_cost).
long_option("osv-fvstore-cost",     optimize_saved_vars_cell_fv_store_cost).
long_option("osv-fvload-cost",      optimize_saved_vars_cell_fv_load_cost).
long_option("osv-op-ratio",         optimize_saved_vars_cell_op_ratio).
long_option("osv-node-ratio",       optimize_saved_vars_cell_node_ratio).
long_option("osv-allpath-node-ratio",
                            optimize_saved_vars_cell_all_path_node_ratio).
long_option("osv-all-cand",
                            optimize_saved_vars_cell_include_all_candidates).
long_option("delay-construct",      delay_construct).
long_option("delay-constructs",     delay_construct).
long_option("follow-code",          follow_code).
long_option("constraint-propagation",   constraint_propagation).
long_option("local-constraint-propagation", local_constraint_propagation).
long_option("optimize-unused-args", optimize_unused_args).
long_option("optimise-unused-args", optimize_unused_args).
long_option("intermod-unused-args", intermod_unused_args).
long_option("optimize-higher-order",    optimize_higher_order).
long_option("optimise-higher-order",    optimize_higher_order).
long_option("higher-order-size-limit",  higher_order_size_limit).
long_option("higher-order-arg-limit",   higher_order_arg_limit).
long_option("unneeded-code",        unneeded_code).
long_option("unneeded-code-copy-limit", unneeded_code_copy_limit).
long_option("unneeded-code-debug",  unneeded_code_debug).
long_option("unneeded-code-debug-pred-name",  unneeded_code_debug_pred_name).
long_option("type-specialization",  type_specialization).
long_option("type-specialisation",  type_specialization).
long_option("user-guided-type-specialization",
                    user_guided_type_specialization).
long_option("user-guided-type-specialisation",
                    user_guided_type_specialization).
% This option is for use in configure.in to test for some bug-fixes for
% type-specialization which are needed to compile the library. It's not
% documented, and should eventually be removed.
long_option("fixed-user-guided-type-specialization",
                    user_guided_type_specialization).
long_option("introduce-accumulators",   introduce_accumulators).
long_option("optimise-constructor-last-call-accumulator",
                    optimize_constructor_last_call_accumulator).
long_option("optimize-constructor-last-call-accumulator",
                    optimize_constructor_last_call_accumulator).
long_option("optimise-constructor-last-call-null",
                    optimize_constructor_last_call_null).
long_option("optimize-constructor-last-call-null",
                    optimize_constructor_last_call_null).
long_option("optimise-constructor-last-call",
                    optimize_constructor_last_call).
long_option("optimize-constructor-last-call",
                    optimize_constructor_last_call).
long_option("optimize-dead-procs",  optimize_dead_procs).
long_option("optimise-dead-procs",  optimize_dead_procs).
long_option("deforestation",        deforestation).
long_option("deforestation-depth-limit",    deforestation_depth_limit).
long_option("deforestation-cost-factor",    deforestation_cost_factor).
long_option("deforestation-vars-threshold", deforestation_vars_threshold).
long_option("deforestation-size-threshold", deforestation_size_threshold).
long_option("enable-termination",   termination).
long_option("enable-term",          termination).
long_option("check-termination",    termination_check).
long_option("check-term",           termination_check).
long_option("chk-term",             termination_check).
long_option("verbose-check-termination",verbose_check_termination).
long_option("verb-check-term",      verbose_check_termination).
long_option("verb-chk-term",        verbose_check_termination).
long_option("termination-single-argument-analysis",
                    termination_single_args).
long_option("term-single-arg",      termination_single_args).
long_option("termination-norm",     termination_norm).
long_option("term-norm",            termination_norm).
long_option("termination-error-limit",  termination_error_limit).
long_option("term-err-limit",       termination_error_limit).
long_option("termination-path-limit",   termination_path_limit).
long_option("term-path-limit",      termination_path_limit).
long_option("enable-termination2",  termination2).
long_option("enable-term2",         termination2).
long_option("check-termination2",   check_termination2).
long_option("check-term2",          check_termination2).
long_option("chk-term2",            check_termination2).
long_option("verbose-check-termination2",verbose_check_termination2).
long_option("verb-check-term2",     verbose_check_termination2).
long_option("verb-chk-term2",       verbose_check_termination2).
long_option("termination2-widening-limit", widening_limit).
long_option("term2-widening-limit",     widening_limit).
long_option("arg-size-analysis-only",   arg_size_analysis_only).
long_option("termination2-propagate-failure-constraints",
                    propagate_failure_constrs).
long_option("term2-propagate-failure-constraints",
                    propagate_failure_constrs).
long_option("term2-propagate-failure-constrs", propagate_failure_constrs).
long_option("termination2-norm", termination2_norm).
long_option("term2-norm", termination2_norm).
long_option("termination2-maximum-matrix-size", term2_maximum_matrix_size).
long_option("term2-max-matrix-size", term2_maximum_matrix_size).
long_option("analyse-exceptions",   analyse_exceptions).
long_option("analyse-closures",     analyse_closures).
long_option("analyse-local-closures",   analyse_closures).
long_option("analyse-trail-usage",  analyse_trail_usage).
long_option("optimize-trail-usage", optimize_trail_usage).
long_option("optimize-region-ops",  optimize_region_ops).
long_option("analyse-mm-tabling",   analyse_mm_tabling).
long_option("untuple",              untuple).
long_option("tuple",                tuple).
long_option("tuple-trace-counts-file",  tuple_trace_counts_file).
long_option("tuple-costs-ratio",    tuple_costs_ratio).
long_option("tuple-min-args",       tuple_min_args).
long_option("inline-par-builtins",  inline_par_builtins).
long_option("always-specialize-in-dep-par-conjs",
                                    always_specialize_in_dep_par_conjs).
long_option("allow-some-paths-only-waits",
                                    allow_some_paths_only_waits).

% CTGC related options.
long_option("structure-sharing",    structure_sharing_analysis).
long_option("structure-sharing-widening", structure_sharing_widening).
long_option("structure-reuse",      structure_reuse_analysis).
long_option("ctgc",                 structure_reuse_analysis).
long_option("structure-reuse-constraint", structure_reuse_constraint).
long_option("ctgc-constraint",      structure_reuse_constraint).
long_option("structure-reuse-constraint-arg", structure_reuse_constraint_arg).
long_option("ctgc-constraint-arg",  structure_reuse_constraint_arg).
long_option("structure-reuse-max-conditions", structure_reuse_max_conditions).
long_option("structure-reuse-repeat", structure_reuse_repeat).
long_option("structure-reuse-free-cells", structure_reuse_free_cells).

% HLDS->LLDS optimizations
long_option("smart-indexing",       smart_indexing).
long_option("smart-atomic-indexing", smart_atomic_indexing).
long_option("smart-string-indexing", smart_string_indexing).
long_option("smart-tag-indexing",    smart_tag_indexing).
long_option("smart-float-indexing",  smart_float_indexing).
long_option("dense-switch-req-density", dense_switch_req_density).
long_option("lookup-switch-req-density",lookup_switch_req_density).
long_option("dense-switch-size",    dense_switch_size).
long_option("lookup-switch-size",   lookup_switch_size).
long_option("string-switch-size",   string_hash_switch_size).
long_option("string-trie-size",     string_trie_switch_size).
long_option("string-trie-switch-size",      string_trie_switch_size).
long_option("string-hash-switch-size",      string_hash_switch_size).
long_option("string-binary-switch-size",    string_binary_switch_size).
long_option("tag-switch-size",      tag_switch_size).
long_option("try-switch-size",      try_switch_size).
long_option("binary-switch-size",   binary_switch_size).
long_option("switch-single-rec-base-first", switch_single_rec_base_first).
long_option("switch-multi-rec-base-first",  switch_multi_rec_base_first).
long_option("static-ground-terms",  static_ground_cells).
% static_ground_floats should be set only in handle_options.m.
% long_option("static-ground-floats", static_ground_floats).
% static_ground_int64s should be set only in handle_options.m.
% long_option("static-ground-int64s", static_ground_int64s).
% static_code_addresses should be set only in handle_options.m.
% long_option("static-code-addresses", static_code_addresses).
long_option("use-atomic-cells",     use_atomic_cells).
long_option("middle-rec",           middle_rec).
long_option("simple-neg",           simple_neg).
long_option("allow-hijacks",        allow_hijacks).

% MLDS optimizations
% Option `optimize' is used for both MLDS and LLDS optimizations, but since
% you can't use both at the same time it doesn't really matter.
long_option("mlds-optimize",        optimize).
long_option("mlds-optimise",        optimize).
long_option("mlds-peephole",        optimize_peep).
long_option("optimize-tailcalls",   optimize_tailcalls).
long_option("optimise-tailcalls",   optimize_tailcalls).
long_option("optimize-initializations", optimize_initializations).
long_option("optimise-initializations", optimize_initializations).
long_option("eliminate-unused-mlds-assigns", eliminate_unused_mlds_assigns).
long_option("eliminate-local-vars", eliminate_local_vars).
long_option("generate-trail-ops-inline", generate_trail_ops_inline).

% LLDS optimizations
long_option("common-data",          common_data).
long_option("common-layout-data",   common_layout_data).
long_option("llds-optimize",        optimize).
long_option("llds-optimise",        optimize).
long_option("optimize-peep",        optimize_peep).
long_option("optimise-peep",        optimize_peep).
long_option("optimize-peep-mkword", optimize_peep_mkword).
long_option("optimise-peep-mkword", optimize_peep_mkword).
long_option("optimize-jumps",       optimize_jumps).
long_option("optimise-jumps",       optimize_jumps).
long_option("optimize-fulljumps",   optimize_fulljumps).
long_option("optimise-fulljumps",   optimize_fulljumps).
long_option("pessimize-tailcalls",  pessimize_tailcalls).
long_option("checked-nondet-tailcalls", checked_nondet_tailcalls).
long_option("use-local-vars",       use_local_vars).
long_option("local-var-access-threshold", local_var_access_threshold).
long_option("standardise-labels",   standardize_labels).
long_option("standardize-labels",   standardize_labels).
long_option("optimize-labels",      optimize_labels).
long_option("optimise-labels",      optimize_labels).
long_option("optimize-dups",        optimize_dups).
long_option("optimise-dups",        optimize_dups).
long_option("optimize-proc-dups",   optimize_proc_dups).
long_option("optimise-proc-dups",   optimize_proc_dups).
%%% long_option("optimize-copyprop",    optimize_copyprop).
%%% long_option("optimise-copyprop",    optimize_copyprop).
long_option("optimize-frames",      optimize_frames).
long_option("optimise-frames",      optimize_frames).
long_option("optimize-delay-slot",  optimize_delay_slot).
long_option("optimise-delay-slot",  optimize_delay_slot).
long_option("optimize-reassign",    optimize_reassign).
long_option("optimise-reassign",    optimize_reassign).
long_option("optimize-repeat",      optimize_repeat).
long_option("optimise-repeat",      optimize_repeat).
long_option("layout-compression-limit",      layout_compression_limit).

% LLDS->C optimizations
long_option("use-macro-for-redo-fail",  use_macro_for_redo_fail).
long_option("emit-c-loops",         emit_c_loops).
long_option("procs-per-c-function", procs_per_c_function).
long_option("procs-per-C-function", procs_per_c_function).
long_option("everything-in-one-c-function", everything_in_one_c_function).
long_option("everything-in-one-C-function", everything_in_one_c_function).
long_option("inline-alloc",         inline_alloc).
long_option("local-thread-engine-base", local_thread_engine_base).

% Erlang
long_option("erlang-switch-on-strings-as-atoms",
                erlang_switch_on_strings_as_atoms).

% Target code compilation options
long_option("target-debug",         target_debug).

long_option("cc",                   cc).
long_option("c-optimise",           c_optimize).
long_option("c-optimize",           c_optimize).
% XXX we should consider the relationship between c_debug and target_debug
% more carefully. Perhaps target_debug could imply C debug if the target is C.
% However for the moment they are just synonyms.
long_option("c-debug",              target_debug).
long_option("c-include-directory",  c_include_directory).
long_option("c-include-dir",        c_include_directory).
long_option("ansi-c",               ansi_c).
long_option("cflags",               cflags).
long_option("cflag",                quoted_cflag).

long_option("gcc-flags",            gcc_flags).
long_option("gcc-flag",             quoted_gcc_flag).
long_option("clang-flags",          clang_flags).
long_option("clang-flag",           quoted_clang_flag).
long_option("msvc-flags",           msvc_flags).
long_option("msvc-flag",            quoted_msvc_flag).

long_option("cflags-for-warnings",  cflags_for_warnings).
long_option("cflags-for-optimization",  cflags_for_optimization).
long_option("cflags-for-ansi",      cflags_for_ansi).
long_option("cflags-for-regs",      cflags_for_regs).
long_option("cflags-for-gotos",     cflags_for_gotos).
long_option("cflags-for-threads",   cflags_for_threads).
long_option("cflags-for-debug",     cflags_for_debug).
long_option("cflags-for-sanitizers", cflags_for_sanitizers).
long_option("cflags-for-pic",       cflags_for_pic).
long_option("c-flag-to-name-object-file", c_flag_to_name_object_file).
long_option("object-file-extension",    object_file_extension).
long_option("pic-object-file-extension", pic_object_file_extension).
long_option("c-compiler-type",      c_compiler_type).
long_option("csharp-compiler-type", csharp_compiler_type).

long_option("java-compiler",        java_compiler).
long_option("javac",                java_compiler).
long_option("java-interpreter",     java_interpreter).
long_option("java-flags",           java_flags).
long_option("java-flag",            quoted_java_flag).
% XXX we should consider the relationship between java_debug and target_debug
% more carefully. Perhaps target_debug could imply Java debug if the target
% is Java. However for the moment they are just synonyms.
long_option("java-debug",           target_debug).
long_option("java-classpath",       java_classpath).
long_option("java-object-file-extension", java_object_file_extension).

long_option("csharp-compiler",      csharp_compiler).
long_option("csharp-flags",         csharp_flags).
long_option("csharp-flag",          quoted_csharp_flag).
long_option("cli-interpreter",      cli_interpreter).

long_option("erlang-compiler",      erlang_compiler).
long_option("erlang-interpreter",   erlang_interpreter).
long_option("erlang-flags",         erlang_flags).
long_option("erlang-flag",          quoted_erlang_flag).
long_option("erlang-include-directory", erlang_include_directory).
long_option("erlang-include-dir",       erlang_include_directory).
long_option("erlang-object-file-extension", erlang_object_file_extension).
long_option("erlang-native-code",   erlang_native_code).
long_option("erlang-inhibit-trivial-warnings",
                                    erlang_inhibit_trivial_warnings).

% link options
long_option("output-file",          output_file_name).
long_option("ld-flags",             ld_flags).
long_option("ld-flag",              quoted_ld_flag).
long_option("ld-libflags",          ld_libflags).
long_option("ld-libflag",           quoted_ld_libflag).
long_option("library-directory",    link_library_directories).
long_option("runtime-library-directory", runtime_link_library_directories).
long_option("default-runtime-library-directory",
                                    default_runtime_library_directory).
long_option("library",              link_libraries).
long_option("link-object",          link_objects).
long_option("mercury-library",      mercury_library_special).
long_option("ml",                   mercury_library_special).
long_option("mercury-library-directory", mercury_library_directory_special).
long_option("mld",                  mercury_library_directory_special).
long_option("search-library-files-directory",
                search_library_files_directory_special).
long_option("search-lib-files-dir",
                search_library_files_directory_special).
long_option("mercury-standard-library-directory",
                mercury_standard_library_directory_special).
long_option("mercury-stdlib-dir",
                mercury_standard_library_directory_special).
long_option("init-file-directory",  init_file_directories).
long_option("init-file",            init_files).
long_option("trace-init-file",      trace_init_files).
long_option("linkage",              linkage_special).
long_option("mercury-linkage",      mercury_linkage_special).
long_option("demangle",             demangle).
long_option("strip",                strip).
long_option("main",                 main).
long_option("allow-undefined",      allow_undefined).
long_option("use-readline",         use_readline).
long_option("runtime-flags",        runtime_flags).
long_option("extra-initialization-functions",
                    extra_initialization_functions).
long_option("extra-inits",      extra_initialization_functions).
long_option("framework",        frameworks).
long_option("framework-directory", framework_directories).
long_option("sign-assembly", sign_assembly).
long_option("cstack-reserve-size", cstack_reserve_size).

long_option("shared-library-extension", shared_library_extension).
long_option("library-extension",    library_extension).
long_option("executable-file-extension", executable_file_extension).
long_option("create-archive-command",   create_archive_command).
long_option("create-archive-command-output-flag",
                    create_archive_command_output_flag).
long_option("create-archive-command-flags", create_archive_command_flags).
long_option("link-executable-command",  link_executable_command).
long_option("link-shared-lib-command",  link_shared_lib_command).
long_option("ranlib-command",       ranlib_command).
long_option("ranlib-flags",         ranlib_flags).
long_option("mkinit-command",       mkinit_command).
long_option("mkinit-erl-command",   mkinit_erl_command).
long_option("demangle-command",     demangle_command).
long_option("filtercc-command",     filtercc_command).
long_option("filterjavac-command",  filterjavac_command).
long_option("trace-libs",           trace_libs).
long_option("thread-libs",          thread_libs).
long_option("hwloc-libs",           hwloc_libs).
long_option("hwloc-static-libs",    hwloc_static_libs).
long_option("shared-libs",          shared_libs).
long_option("math-lib",             math_lib).
long_option("readline-libs",        readline_libs).
long_option("linker-opt-separator", linker_opt_separator).
long_option("linker-debug-flags",   linker_debug_flags).
long_option("shlib-linker-debug-flags", shlib_linker_debug_flags).
long_option("linker-sanitizer-flags", linker_sanitizer_flags).
long_option("linker-trace-flags",   linker_trace_flags).
long_option("shlib-linker-trace-flags", shlib_linker_trace_flags).
long_option("linker-thread-flags",  linker_thread_flags).
long_option("shlib-linker-thread-flags", shlib_linker_thread_flags).
long_option("linker-static-flags",  linker_static_flags).
long_option("linker-strip-flag",    linker_strip_flag).
long_option("linker-link-lib-flag", linker_link_lib_flag).
long_option("linker-link-lib-suffix",   linker_link_lib_suffix).
long_option("shlib-linker-link-lib-flag", shlib_linker_link_lib_flag).
long_option("shlib-linker-link-lib-suffix", shlib_linker_link_lib_suffix).
long_option("linker-path-flag",     linker_path_flag).
long_option("linker-rpath-flag",    linker_rpath_flag).
long_option("linker-rpath-separator",   linker_rpath_separator).
long_option("shlib-linker-rpath-flag",  shlib_linker_rpath_flag).
long_option("shlib-linker-rpath-separator", shlib_linker_rpath_separator).
long_option("linker-allow-undefined-flag", linker_allow_undefined_flag).
long_option("linker-error-undefined-flag", linker_error_undefined_flag).
long_option("shlib-linker-use-install-name", shlib_linker_use_install_name).
long_option("shlib-linker-install-name-flag", shlib_linker_install_name_flag).
long_option("shlib-linker-install-name-path", shlib_linker_install_name_path).
long_option("strip-executable-command", strip_executable_command).
long_option("strip-executable-shared-flags", strip_executable_shared_flags).
long_option("strip-executable-static-flags", strip_executable_static_flags).
long_option("java-archive-command", java_archive_command).

% build system options
long_option("make",                 only_opmode_make).
long_option("keep-going",           keep_going).
long_option("rebuild",              rebuild).
long_option("jobs",                 jobs).
long_option("track-flags",          track_flags).
long_option("track-options",        track_flags).
long_option("invoked-by-mmc-make",  invoked_by_mmc_make).
long_option("pre-link-command",     pre_link_command).
long_option("extra-init-command",   extra_init_command).
long_option("mercury-configuration-directory",
                mercury_configuration_directory_special).
long_option("mercury-config-dir",
                mercury_configuration_directory_special).
long_option("install-prefix",       install_prefix).
long_option("install-command",      install_command).
long_option("install-command-dir-option", install_command_dir_option).
long_option("use-symlinks",         use_symlinks).
long_option("library-grade",        libgrades).
long_option("detect-libgrades",     detect_libgrades).
long_option("libgrade",             libgrades).
long_option("libgrades-include-component", libgrades_include_components).
long_option("libgrades-include",           libgrades_include_components).
long_option("libgrades-exclude-component", libgrades_exclude_components).
long_option("libgrades-exclude",           libgrades_exclude_components).
long_option("library-linkage",      lib_linkages).
long_option("lib-linkage",          lib_linkages).
long_option("flags",                flags_file).
long_option("flags-file",           flags_file).
long_option("options-file",         options_files).
long_option("config-file",          config_file).
long_option("options-search-directory", options_search_directories).
long_option("use-subdirs",          use_subdirs).
long_option("use-grade-subdirs",    use_grade_subdirs).
long_option("search-directory",     search_directories).
long_option("intermod-directory",   intermod_directories).
long_option("use-search-directories-for-intermod",
                    use_search_directories_for_intermod).
long_option("libgrade-install-check", libgrade_install_check).
long_option("order-make-by-timestamp", order_make_by_timestamp).
long_option("show-make-times",      show_make_times).
long_option("extra-lib-header",     extra_library_header).
long_option("extra-library-header", extra_library_header).
long_option("restricted-command-line", restricted_command_line).
long_option("env-type",                env_type).
long_option("host-env-type",           host_env_type).
long_option("system-env-type",         system_env_type).
long_option("target-env-type",         target_env_type).

% misc options
long_option("typecheck-ambiguity-warn-limit",
                                    typecheck_ambiguity_warn_limit).
long_option("typecheck-ambiguity-error-limit",
                                    typecheck_ambiguity_error_limit).
long_option("help",                 help).
long_option("version",              version).
long_option("filenames-from-stdin", filenames_from_stdin).
long_option("fullarch",             target_arch).
long_option("target-arch",          target_arch).
long_option("cross-compiling",      cross_compiling).
long_option("local-module-id",      local_module_id).
long_option("analysis-file-cache-dir",  analysis_file_cache_dir).
long_option("bug-intermod-2002-06-13",  compiler_sufficiently_recent).
long_option("bug-intermod-2006-09-28",  compiler_sufficiently_recent).
long_option("bug-foreign_import-2002-08-06", compiler_sufficiently_recent).
long_option("install-opt-files-2002-08-30", compiler_sufficiently_recent).
long_option("read-config-file-2003-03-01", compiler_sufficiently_recent).
% XXX this option won't be recognised because of the "no-" prefix,
% but "no-no-" will be recognised.
long_option("no-noncompact-ho-call-2004-01-15", compiler_sufficiently_recent).
long_option("trace-io-builtins-2006-08-14", compiler_sufficiently_recent).
long_option("compound-compare-builtins-2007-07-09",
                                    compiler_sufficiently_recent).
% XXX this option won't be recognised because of the "no-" prefix,
% but "no-no-" will be recognised.
long_option("no-det-warning-compound-compare-2007-07-17",
                                    compiler_sufficiently_recent).
long_option("foreign-enum-switch-fix",
                                    compiler_sufficiently_recent).
long_option("failing-disjunct-in-switch-dup-fix",
                                    compiler_sufficiently_recent).
long_option("store-at-ref-impure-2008-09-11",
                                    compiler_sufficiently_recent).
long_option("java-export-ref-out",  compiler_sufficiently_recent).
long_option("java-generics-2010-04-13",
                                    compiler_sufficiently_recent).
long_option("strip-executable-2014-05-05",
                                    compiler_sufficiently_recent).
long_option("trace-goal-only-locals-2017-07-05",
                                    compiler_sufficiently_recent).
long_option("no-reserved-addrs",
                                    compiler_sufficiently_recent).
long_option("builtin-lt-gt-2018-10-08",
                                    compiler_sufficiently_recent).
long_option("fixed-contiguity-2018-10-19",
                                    compiler_sufficiently_recent).
long_option("experiment",           experiment).
long_option("experiment1",          experiment1).
long_option("experiment2",          experiment2).
long_option("experiment3",          experiment3).
long_option("experiment4",          experiment4).
long_option("experiment5",          experiment5).
long_option("ignore-par-conjunctions",
                                    ignore_par_conjunctions).
long_option("control-granularity",  control_granularity).
long_option("distance-granularity", distance_granularity).
long_option("implicit-parallelism", implicit_parallelism).
long_option("feedback-file",        feedback_file).
long_option("par-loop-control",     par_loop_control).
long_option("par-loop-control-preserve-tail-recursion",
                                    par_loop_control_preserve_tail_recursion).

%---------------------------------------------------------------------------%

special_handler(Option, SpecialData, !.OptionTable, Result) :-
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
                map.det_update(mercury_linkage, string(Flag), !OptionTable),
                map.det_update(linkage, string(Flag), !OptionTable),
                Result = ok(!.OptionTable)
            else
                Result = error("argument of `--linkage' should be " ++
                    "either ""shared"" or ""static"".")
            )
        ;
            Option = mercury_linkage_special,
            SpecialData = string(Flag),
            ( if ( Flag = "shared" ; Flag = "static" ) then
                map.det_update(mercury_linkage, string(Flag), !OptionTable),
                Result = ok(!.OptionTable)
            else
                Result = error("argument of `--mercury-linkage' should be " ++
                    "either ""shared"" or ""static"".")
            )
        )
    ;
        (
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
            Option = erlang,
            SpecialData = none,
            map.set(target, string("erlang"), !OptionTable)
        ;
            Option = erlang_only,
            SpecialData = none,
            map.set(target, string("erlang"), !OptionTable),
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
            map.set(inline_simple, bool(Inline), !OptionTable),
            map.set(inline_builtins, bool(Inline), !OptionTable),
            map.set(inline_single_use, bool(Inline), !OptionTable),
            (
                Inline = yes,
                map.set(inline_compound_threshold, int(10), !OptionTable)
            ;
                Inline = no,
                map.set(inline_compound_threshold, int(0), !OptionTable)
            )
        ;
            Option = everything_in_one_c_function,
            SpecialData = none,
            map.set(procs_per_c_function, int(0), !OptionTable)
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
            set_all_options_to(style_warning_options, bool(Enable),
                !OptionTable),
            set_all_options_to(non_style_warning_options, bool(Enable),
                !OptionTable)
        ;
            Option = inhibit_style_warnings,
            SpecialData = bool(Inhibit),
            bool.not(Inhibit, Enable),
            set_all_options_to(style_warning_options, bool(Enable),
                !OptionTable)
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
            Option = opt_space,
            SpecialData = none,
            opt_space(OptionSettingsList),
            override_options(OptionSettingsList, !OptionTable)
        ;
            Option = opt_level,
            SpecialData = int(SpecifiedLevel),
            ( if SpecifiedLevel > 6 then
                EffectiveLevel = 6
            else if SpecifiedLevel < -1 then
                EffectiveLevel = -1
            else
                EffectiveLevel = SpecifiedLevel
            ),
            map.set(opt_level_number, int(EffectiveLevel), !OptionTable),
            set_opt_level(EffectiveLevel, !OptionTable)
        ;
            Option = optimize_saved_vars,
            SpecialData = bool(Optimize),
            map.set(optimize_saved_vars_const, bool(Optimize), !OptionTable),
            map.set(optimize_saved_vars_cell, bool(Optimize), !OptionTable)
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
            Option = quoted_java_flag,
            SpecialData = string(Flag),
            handle_quoted_flag(java_flags, Flag, !OptionTable)
        ;
            Option = quoted_csharp_flag,
            SpecialData = string(Flag),
            handle_quoted_flag(csharp_flags, Flag, !OptionTable)
        ;
            Option = quoted_erlang_flag,
            SpecialData = string(Flag),
            handle_quoted_flag(erlang_flags, Flag, !OptionTable)
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
    ).

style_warning_options = [
    warn_inconsistent_pred_order_clauses,
    warn_inconsistent_pred_order_foreign_procs,
    warn_non_contiguous_decls,
    warn_non_contiguous_clauses,
    warn_non_contiguous_foreign_procs,
    warn_simple_code,
    warn_duplicate_calls,
    warn_implicit_stream_calls,
    warn_non_tail_recursion_self,
    warn_non_tail_recursion_mutual,
    warn_obvious_non_tail_recursion,
    warn_dead_procs,
    warn_dead_preds,
    warn_known_bad_format_calls,
    warn_unknown_format_calls,
    warn_insts_without_matching_type,
    warn_insts_with_functors_without_type,
    inform_ite_instead_of_switch,
    inform_incomplete_switch,
    warn_suspicious_foreign_procs,
    warn_state_var_shadowing,
    inform_suboptimal_packing
].

non_style_warning_options = [
    warn_accumulator_swaps,
    warn_singleton_vars,
    warn_overlapping_scopes,
    warn_det_decls_too_lax,
    warn_inferred_erroneous,
    warn_nothing_exported,
    warn_unused_args,
    warn_interface_imports,
    warn_interface_imports_in_parents,
    warn_missing_opt_files,
    warn_missing_trans_opt_files,
    warn_missing_trans_opt_deps,
    warn_non_stratification,
    warn_unification_cannot_succeed,
    warn_missing_module_name,
    warn_wrong_module_name,
    warn_smart_recompilation,
    warn_undefined_options_variables,
    warn_target_code,
    warn_up_to_date,
    warn_stubs,
    warn_table_with_inline,
    warn_non_term_special_preds,
    inform_inferred_types
].

%---------------------------------------------------------------------------%

option_table_add_mercury_library_directory(Dir, !OptionTable) :-
    % The init_file_directories and link_library_directories for Mercury
    % libraries are grade dependent, so they need to be handled in
    % handle_options.m after we know the grade.
    list.foldl(append_to_accumulating_option, [
        search_directories          - dir.make_path_name(Dir, "ints"),
        c_include_directory         - dir.make_path_name(Dir, "inc"),
        erlang_include_directory    - dir.make_path_name(Dir, "inc"),
        mercury_library_directories - Dir
    ], !OptionTable).

option_table_add_search_library_files_directory(Dir, !OptionTable) :-
    % Grade dependent directories need to be handled in handle_options.m
    % after we know the grade.
    list.foldl(append_to_accumulating_option, [
        search_directories          - Dir,
        c_include_directory         - Dir,
        erlang_include_directory    - Dir,
        search_library_files_directories - Dir
    ], !OptionTable).

:- pred append_to_accumulating_option(pair(option, string)::in,
    option_table::in, option_table::out) is det.

append_to_accumulating_option(Option - Value, !OptionTable) :-
    getopt_io.lookup_accumulating_option(!.OptionTable, Option, Values0),
    Values = Values0 ++ [Value],
    map.set(Option, accumulating(Values), !OptionTable).

:- pred set_opt_level(int::in, option_table::in, option_table::out) is det.

set_opt_level(N, !OptionTable) :-
    % First reset all optimizations to their default
    % (the default should be all optimizations off).
    option_defaults_2(optimization_option, OptimizationDefaults),
    override_options(OptimizationDefaults, !OptionTable),

    % Next enable the optimization levels from 0 up to N.
    enable_opt_levels(0, N, !OptionTable).

:- pred enable_opt_levels(int::in, int::in,
    option_table::in, option_table::out) is det.

enable_opt_levels(Cur, Max, !OptionTable) :-
    ( if Cur > Max then
        true
    else if opt_level(Cur, !.OptionTable, OptionSettingsList) then
        override_options(OptionSettingsList, !OptionTable),
        enable_opt_levels(Cur + 1, Max, !OptionTable)
    else
        unexpected($pred, "unknown optimization level")
    ).

:- pred override_options(list(pair(option, option_data))::in,
    option_table::in, option_table::out) is det.

override_options([], !OptionTable).
override_options([Option - Value | OptionsValues], !OptionTable) :-
    map.set(Option, Value, !OptionTable),
    override_options(OptionsValues, !OptionTable).

set_all_options_to([], _Value, !OptionTable).
set_all_options_to([Option | Options], Value, !OptionTable) :-
    map.set(Option, Value, !OptionTable),
    set_all_options_to(Options, Value, !OptionTable).

%---------------------------------------------------------------------------%

:- pred opt_space(list(pair(option, option_data))::out) is det.

opt_space([
    unneeded_code_copy_limit    -   int(1),
    optimize_dead_procs         -   bool(yes),
    optimize_labels             -   bool(yes),
    optimize_dups               -   bool(yes),
    optimize_proc_dups          -   bool(yes),
    optimize_fulljumps          -   bool(no),
    optimize_reassign           -   bool(yes),
    inline_alloc                -   bool(no),
    use_macro_for_redo_fail     -   bool(no),
    loop_invariants             -   bool(no)
]).

%---------------------------------------------------------------------------%

:- pred opt_level(int::in, option_table::in,
    list(pair(option, option_data))::out) is semidet.

opt_level(OptLevel, OptionTable, Settings) :-
    % If the optimization level is not set, we use the default values of
    % all the optimization options (HLDS->HLDS, HLDS->LLDS, LLDS->LLDS,
    % LLDS->C, and C->object, and their equivalents for the other backends
    % and targets.
    %
    % However, some optimizations are not controlled by options
    % and thus can't be disabled.

    (
        OptLevel = 0,
        % Optimization level 0: aim to minimize overall compilation time.
        % XXX I just guessed. We should run lots of experiments.
        Settings = [
            common_data                 -   bool(yes),
            optimize                    -   bool(yes),
            optimize_repeat             -   int(1),
            optimize_peep               -   bool(yes),
            optimize_peep_mkword        -   bool(yes),
            static_ground_cells         -   bool(yes),
            smart_indexing              -   bool(yes),
            optimize_jumps              -   bool(yes),
            optimize_labels             -   bool(yes),
            optimize_dead_procs         -   bool(yes),
            excess_assign               -   bool(yes)   % ???
        ]
    ;
        OptLevel = 1,
        % Optimization level 1: apply optimizations which are cheap and
        % have a good payoff while still keeping compilation time small.
        getopt_io.lookup_bool_option(OptionTable, have_delay_slot, DelaySlot),
        Settings = [
            use_local_vars              -   bool(yes),
            c_optimize                  -   bool(yes),  % XXX we want `gcc -O1'
            optimize_frames             -   bool(yes),
            optimize_delay_slot         -   bool(DelaySlot),
            middle_rec                  -   bool(yes),
            emit_c_loops                -   bool(yes),
            optimize_tailcalls          -   bool(yes)
            % dups?
        ]
    ;
        OptLevel = 2,
        % Optimization level 2: apply optimizations which have a good payoff
        % relative to their cost; but include optimizations which are
        % more costly than with -O1.
        Settings = [
            optimize_fulljumps          -   bool(yes),
            optimize_repeat             -   int(3),
            optimize_dups               -   bool(yes),
            follow_code                 -   bool(yes),
            inline_simple               -   bool(yes),
            inline_single_use           -   bool(yes),
            inline_compound_threshold   -   int(10),
            common_struct               -   bool(yes),
            user_guided_type_specialization -   bool(yes),
            % XXX While inst `array' is defined as `ground', we can't optimize
            % duplicate calls (we might combine calls to `array.init').
            % optimize_duplicate_calls  -   bool(yes),
            simple_neg                  -   bool(yes),
            test_after_switch           -   bool(yes),

            optimize_initializations    -  bool(yes)
        ]
    ;
        OptLevel = 3,
        % Optimization level 3: apply optimizations which usually have a good
        % payoff even if they increase compilation time quite a bit.
        Settings = [
            optimize_saved_vars_const   - bool(yes),
            optimize_unused_args        -   bool(yes),
            optimize_higher_order       -   bool(yes),
            deforestation               -   bool(yes),
            local_constraint_propagation -  bool(yes),
            constant_propagation        -   bool(yes),
            optimize_reassign           -   bool(yes),
            % Disabled until a bug in extras/trailed_update/var.m is resolved.
            % introduce_accumulators    -   bool(yes),
            optimize_repeat             -   int(4)
        ]
    ;
        OptLevel = 4,
        % Optimization level 4: apply optimizations which may have some payoff
        % even if they increase compilation time quite a bit.
        %
        % Currently this enables the use of local variables
        % and increases the inlining thresholds.
        Settings = [
            inline_simple_threshold     -   int(8),
            inline_compound_threshold   -   int(20),
            higher_order_size_limit     -   int(30)
        ]
    ;
        OptLevel = 5,
        % Optimization level 5: apply optimizations which may have some
        % payoff even if they increase compilation time a lot.
        %
        % Currently this enables the search for construction unifications that
        % can be delayed past failing computations, allows more passes of the
        % low-level optimizations, and increases the inlining thresholds
        % still further. We also enable eliminate_local_vars only at
        % this level, because that pass is implemented pretty inefficiently.
        Settings = [
            optimize_repeat             -   int(5),
            delay_construct             -   bool(yes),
            inline_compound_threshold   -   int(100),
            higher_order_size_limit     -   int(40),
            eliminate_local_vars        -   bool(yes),
            loop_invariants             -   bool(yes)
        ]
    ;
        OptLevel = 6,
        % Optimization level 6: apply optimizations which may have any payoff
        % even if they increase compilation time to completely unreasonable
        % levels.
        %
        % Currently this sets `everything_in_one_c_function', which causes
        % the compiler to put everything in the one C function and treat calls
        % to predicates in the same module as local. We also enable inlining
        % of GC_malloc(), redo(), and fail().
        Settings = [
            procs_per_c_function        -   int(0),
            inline_alloc                -   bool(yes),
            use_macro_for_redo_fail     -   bool(yes)
        ]

        % The following optimization options are not enabled at any level:
        %
        %   checked_nondet_tailcalls:
        %       This is deliberate, because the transformation
        %       might make code run slower.
        %
        %   constraint_propagation:
        %       I think this is deliberate, because the transformation
        %       might make code run slower?
        %
        %   unneeded_code:
        %       Because it can cause slowdowns at high optimization levels;
        %       cause unknown
        %   type_specialization:
        %       XXX why not?
        %
        %   introduce_accumulators:
        %       XXX Disabled until a bug in extras/trailed_update/var.m
        %       is resolved.
        %
        %   optimize_constructor_last_call:
        %       Not a speedup in general.
    ).

%---------------------------------------------------------------------------%

:- pred handle_quoted_flag(option::in, string::in,
    option_table::in, option_table::out) is det.

handle_quoted_flag(Option, Flag, !OptionTable) :-
    append_to_accumulating_option(Option - quote_arg(Flag), !OptionTable).

quote_arg(Arg0) = Arg :-
    % XXX Instead of using dir.use_windows_paths, this should really
    % test whether we are using a Unix or Windows shell.
    ( if dir.use_windows_paths then
        ( if
            ( string_contains_whitespace(Arg0)
            ; Arg0 = ""
            )
        then
            Arg = """" ++ Arg0 ++ """"
        else
            Arg = Arg0
        )
    else
        ArgList = quote_arg_unix(string.to_char_list(Arg0)),
        (
            ArgList = [],
            Arg = """"""
        ;
            ArgList = [_ | _],
            ( if
                list.member(Char, ArgList),
                not
                    ( char.is_alnum_or_underscore(Char)
                    ; Char = ('-')
                    ; Char = ('/')
                    ; Char = ('.')
                    ; Char = (',')
                    ; Char = (':')
                    )
            then
                Arg = """" ++ string.from_char_list(ArgList) ++ """"
            else
                Arg = string.from_char_list(ArgList)
            )
        )
    ).

:- pred string_contains_whitespace(string::in) is semidet.

string_contains_whitespace(Str) :-
    Chars = string.to_char_list(Str),
    some [Char] (
        list.member(Char, Chars),
        char.is_whitespace(Char)
    ).

:- func quote_arg_unix(list(char)) = list(char).

quote_arg_unix([]) = [].
quote_arg_unix([Char | Chars0]) = Chars :-
    Chars1 = quote_arg_unix(Chars0),
    ( if quote_char_unix(Char) then
        Chars = [('\\'), Char | Chars1]
    else
        Chars = [Char | Chars1]
    ).

:- pred quote_char_unix(char::in) is semidet.

quote_char_unix('\\').
quote_char_unix('"').
quote_char_unix('`').
quote_char_unix('$').

%---------------------------------------------------------------------------%

inconsequential_options(InconsequentialOptions) :-
    option_defaults_2(warning_option, WarningOptions),
    option_defaults_2(verbosity_option, VerbosityOptions),
    option_defaults_2(internal_use_option, InternalUseOptions),
    option_defaults_2(build_system_option, BuildSystemOptions),
    assoc_list.keys(WarningOptions, WarningKeys),
    assoc_list.keys(VerbosityOptions, VerbosityKeys),
    assoc_list.keys(InternalUseOptions, InternalUseKeys),
    assoc_list.keys(BuildSystemOptions, BuildSystemKeys),
    Keys = WarningKeys ++ VerbosityKeys ++ InternalUseKeys ++ BuildSystemKeys,
    InconsequentialOptions = set.from_list(Keys).

%---------------------------------------------------------------------------%

options_help -->
    io.write_string("\t-?, -h, --help\n"),
    io.write_string("\t\tPrint this usage message.\n"),
    options_help_warning,
    options_help_verbosity,
    options_help_output,
    options_help_aux_output,
    options_help_semantics,
    options_help_termination,
    options_help_ctgc,
    options_help_compilation_model,
    options_help_code_generation,
    options_help_optimization,
    options_help_hlds_hlds_optimization,
    options_help_hlds_llds_optimization,
    options_help_llds_llds_optimization,
    options_help_mlds_mlds_optimization,
    options_help_hlds_elds_optimization,
    options_help_output_optimization,
    options_help_target_code_compilation,
    options_help_link,
    options_help_build_system,
    options_help_misc.

:- pred options_help_warning(io::di, io::uo) is det.

options_help_warning -->
    io.write_string("\nWarning Options:\n"),
    write_tabbed_lines([
        "-w, --inhibit-warnings",
        "\tDisable all warning messages.",
        "--inhibit-style-warnings",
        "\tDisable all warning messages about programming style.",
        "--halt-at-warn",
        "\tThis option causes the compiler to treat all",
        "\twarnings as if they were errors. This means that",
        "\tif any warning is issued, the compiler will not",
        "\tgenerate code --- instead, it will return a",
        "\tnon-zero exit status.",
        "--halt-at-syntax-errors",
        "\tThis option causes the compiler to halt immediately",
        "\tafter syntax checking and not do any semantic checking",
        "\tif it finds any syntax errors in the program.",
%       "--halt-at-auto-parallel-failure",
%       "\tThis option causes the compiler to halt if it cannot perform",
%       "\tan auto-parallelization requested by a feedback file.",
        "--no-warn-accumulator-swaps",
        "\tDo not warn about argument order rearrangement caused",
        "\tby `--introduce-accumulators'.",
        "--no-warn-singleton-vars, --no-warn-singleton-variables",
        "\tDo not warn about variables which only occur once.",
        "--no-warn-overlapping-scopes",
        "\tDo not warn about variables which occur in overlapping scopes.",
        "--no-warn-det-decls-too-lax",
        "\tDo not warn about determinism declarations",
        "\twhich could have been stricter.",
        "--no-warn-inferred-erroneous",
        "\tDo not warn about procedures whose determinism is inferred",
        "\terroneous but whose determinism declarations are laxer.",
        "--no-warn-insts-without-matching-type",
        "\tDo not warn about insts that are not consistent with any",
        "\tof the types in scope.",
        "--warn-insts-with-functors-without-type",
        "\tWarn about insts that do specify functors but do not specify",
        "\twhat type they are for.",
        % XXX disabled until compiler unused_imports,
        % don't forget to update the user_guide.texi
        % "--no-warn-unused-imports",
        % "\tDo not warn about modules that are imported but not used.",
        "--warn-unused-imports",
        "\tWarn about modules that are imported but not used.",
        "--no-warn-nothing-exported",
        "\tDo not warn about modules which export nothing.",
        "--warn-unused-args",
        "\tWarn about predicate arguments which are not used.",
        "--no-warn-interface-imports",
        "\tDo not warn about modules imported in the interface, but",
        "\twhich are not used in the interface.",
        "--warn-interface-imports-in-parents",
        "\tWarn about modules that are imported in the interface of",
        "\ta parent module, but not used in the interface of that module.",
        "--no-warn-missing-opt-files",
        "\tDisable warnings about `.opt' files which cannot be opened.",
        "--warn-missing-trans-opt-files",
        "\tEnable warnings about `.trans_opt' files which cannot",
        "\tbe opened.",
        "--no-warn-missing-trans-opt-deps",
        "\tDisable warnings produced when the information required",
        "\tto allow `.trans_opt' files to be read when creating other",
        "\t`.trans_opt' files has been lost. The information can be",
        "\trecreated by running `mmake <mainmodule>.depend'",
        "--warn-inconsistent-pred-order-clauses",
        "\tGenerate a warning if the order of the definitions does not match",
        "\tthe order of the declarations for either the exported predicates",
        "\tand functions of the module, or for the nonexported predicates",
        "\tand functions of the module. Applies for definitions by",
        "\tMercury clauses.",
        "--warn-inconsistent-pred-order-foreign-procs",
        "\tGenerate a warning if the order of the definitions does not match",
        "\tthe order of the declarations for either the exported predicates",
        "\tand functions of the module, or for the nonexported predicates",
        "\tand functions of the module. Applies for definitions by either",
        "\tMercury clauses or foreign_proc pragmas.",
        "--no-warn-non-contiguous-decl",
        "\tDo not generate a warning if the mode declarations of a",
        "\tpredicate or function don't all immediately follow its",
        "\tpredicate or function declaration.",
        "--no-warn-non-contiguous-clauses",
        "\tDo not generate a warning if the clauses of a predicate or",
        "\tfunction are not contiguous.",
        "--warn-non-contiguous-foreign-procs",
        "\tGenerate a warning if the clauses and foreign_procs of a",
        "\tpredicate or function are not contiguous.",
        "--warn-non-stratification",
        "\tWarn about possible non-stratification of the predicates and/or",
        "\tfunctions in the module.",
        "\tNon-stratification occurs when a predicate or function can call",
        "\titself negatively through some path along its call graph.",
        "--no-warn-unification-cannot-succeed",
        "\tDisable warnings about unifications which cannot succeed.",
        "--no-warn-simple-code",
        "\tDisable warnings about constructs which are so",
        "\tsimple that they are likely to be programming errors.",
        "--warn-duplicate-calls",
        "\tWarn about multiple calls to a predicate with the",
        "\tsame input arguments.",
        "--warn-implicit-stream-calls",
        "\tWarn about calls to I/O predicates that could take explicit",
        "\tstream arguments, but do not do so.",
        "--no-warn-missing-module-name",
        "\tDisable warnings for modules that do not start with",
        "\ta `:- module' declaration.",
        "--no-warn-wrong-module-name",
        "\tDisable warnings for modules whose `:- module'",
        "\tdeclaration does not match the module's file name.",
        "--no-warn-smart-recompilation",
        "\tDisable warnings from the smart recompilation system.",
        "--no-warn-undefined-options-vars",
        "--no-warn-undefined-options-variables",
        "\tDo not warn about references to undefined variables in",
        "\toptions files with `--make'.",
%       "--warn-suspicious-recursion",
%       "\tWarn about recursive calls which are likely to have problems,",
%       "\tsuch as leading to infinite recursion.",
% These are the internal options that implement --warn-non-tail-recursion.
%       "--warn-non-tail-recursion-self",
%       "\tWarn about any self recursive calls that are not tail recursive.",
%       "--warn-non-tail-recursion-mutual",
%       "\tWarn about any mutually recursive calls that are not",
%       "\ttail recursive.",
        "--warn-non-tail-recursion <type>",
        "\tWarn about recursive calls that are not tail calls,",
        "\t<type> may be ""self"", ""self-and-mutual"" or ""none"".",
        "--warn-obvious-non-tail-recursion",
        "\tWarn about recursive calls that are not tail calls",
        "\teven if they obviously cannot be tail calls,",
        "\tbecause they are followed by other recursive calls.",
        "--no-warn-up-to-date",
        "\tDo not warn if targets specified on the command line",
        "\twith `--make' are already up-to-date.",
        "--no-warn-stubs",
        "\tDisable warnings about procedures for which there are no",
        "\tclauses. Note that this option only has any effect if",
        "\tthe `--allow-stubs' option (described in the ""Language",
        "\tSemantics Options"" section below) is enabled.",
        "--warn-dead-procs",
        "\tWarn about procedures which are never called.",
        "--warn-dead-preds",
        "\tWarn about predicates that have no procedures which are",
        "\tever called.",
        "--no-warn-target-code",
        "\tDisable warnings from the compiler used to process the",
        "\ttarget code (e.g. gcc).",
        "--no-warn-table-with-inline",
        "\tDisable warnings about tabled procedures that also have",
        "\ta `pragma inline' declaration.",
        "--no-warn-non-term-special-preds",
        "\tDo not warn about types that have user-defined equality or",
        "\tcomparison predicates that cannot be proved to terminate.",
        "\tThis option is only enabled when termination analysis is enabled.",
        "\t(See the ""Termination Analysis Options"" section below).",
        "--no-warn-known-bad-format-calls",
        "\tDo not warn about calls to string.format or io.format that",
        "\tthe compiler knows for sure contain mismatches between the",
        "\tformat string and the supplied values.",
        "--no-warn-only-one-format-string-error",
        "\tIf a format string has more one than mismatch with the supplied,",
        "\tvalues, generate a warning for all mismatches, not just the first.",
        "\tThe later mismatches may be avalanche errors caused by earlier",
        "\tmismatches.",
        "--warn-unknown-format-calls",
        "\tWarn about calls to string.format or io.format for which",
        "\tthe compiler cannot tell whether there are any mismatches",
        "\tbetween the format string and the supplied values.",
        "--no-warn-obsolete",
        "\tDo not warn about calls to predicates or functions that have",
        "\tbeen marked as obsolete.",
        "--inform-ite-instead-of-switch",
        "\tGenerate informational messages for if-then-elses that could be",
        "\treplaced by switches.",
        "--inform-incomplete-switch",
        "\tGenerate informational messages for switches that do not cover",
        "\tall the function symbols that the switched-on variable could be",
        "\tbound to.",
        "--inform-incomplete-switch-threshold <N>",
        "\tHave the --inform-incomplete-switch option generate its messages",
        "\tonly for switches that *do* cover at least N% of the function",
        "\tsymbols that the switched-on variable could be bound to.",
        "--no-warn-unresolved-polymorphism",
        "\tDo not warn about unresolved polymorphism.",
        "--warn-suspicious-foreign-procs",
        "\tWarn about possible errors in the bodies of foreign",
        "\tprocedures.",
        "--warn-suspicious-foreign-code",
        "\tWarn about possible errors in the bodies of foreign code",
        "\tpragmas.",
        "--no-warn-state-var-shadowing",
        "\tDo not warn about one state variable shadowing another.",
        "--no-inform-inferred",
        "\tDo not generate messages about inferred types or modes.",
        "--no-inform-inferred-types",
        "\tDo not generate messages about inferred types.",
        "--no-inform-inferred-modes",
        "\tDo not generate messages about inferred modes.",
        "--inform-suboptimal-packing",
        "\tGenerate messages if the arguments of a data constructor",
        "\tcould be packed more tightly if they were reordered."
    ]).

:- pred options_help_verbosity(io::di, io::uo) is det.

options_help_verbosity -->
    io.write_string("\nVerbosity Options:\n"),
    write_tabbed_lines([
        "-v, --verbose",
        "\tOutput progress messages at each stage in the compilation.",
        "-V, --very-verbose",
        "\tOutput very verbose progress messages.",
        "-E, --verbose-error-messages",
        "\tExplain error messages. Asks the compiler to give you a more",
        "\tdetailed explanation of any errors it finds in your program.",
        "--no-verbose-make",
        "\tDisable messages about the progress of builds using",
        "\tthe `--make' option.",
        "--verbose-commands",
        "\tOutput each external command before it is run.",
        "\tNote that some commands will only be printed with `--verbose'.",
        "--verbose-recompilation",
        "\tWhen using `--smart-recompilation', output messages",
        "\texplaining why a module needs to be recompiled.",
        "--find-all-recompilation-reasons",
        "\tFind all the reasons why a module needs to be recompiled,",
        "\tnot just the first. Implies `--verbose-recompilation'.",
        "--output-compile-error-lines <n>",
        "\tWith `--make', output the first <n> lines of the `.err'",
        "\tfile after compiling a module (default: 15).",
        "--report-cmd-line-args-doterr",
        "\tReport the command line arguments.",
        "--report-cmd-line-args-in-doterr",
        "\tReport the command line arguments for compilations whose output",
        "\tmmake normally redirects to a `.err' file.",
        "-S, --statistics",
        "\tOutput messages about the compiler's time/space usage.",
        "\tAt the moment this option implies `--no-trad-passes', so you get",
        "\tinformation at the boundaries between phases of the compiler.",
% The only sensible way to use --detailed-statistics, based on --very-verbose,
% is implemented automatically in handle_options, so users shouldn't need to be
% aware of it.
%       "--detailed-statistics",
%       "\tOutput more detailed messages about the compiler's",
%       "\ttime/space usage.",
        "--proc-size-statistics <filename>",
        "\tAppend information about the size of each procedure in the",
        "\tmodule in terms of goals and variables to the end of the",
        "\tnamed file.",
        "--limit-error-contexts filename:minline1-maxline1,minline2-maxline2",
        "\tPrint errors and warnings for the named file only when their",
        "\tline number is in one of the specified ranges.",
        "\tThe minimum or maximum line number in each range may be omitted,",
        "\tin which case the range has no lower or upper bound respectively.",
        "\tMultiple --limit-error-context options accumulate.",
        "\tIf more than one --limit-error-context option is given for",
        "\tthe same file, only the last one will have an effect.",
        "\tIf the file name and colon are missing, the limit will apply",
        "\tto all files.",
% --debug-types works only if the compiler was compiled with
% "--trace-flag type_checkpoint".
%       "-T, --debug-types",
%       "\tOutput detailed debugging traces of the type checking.",
        "-N, --debug-modes",
        "\tOutput debugging traces of the mode checking.",
        "--debug-modes-statistics",
        "\tOutput statistics after each step of mode checking.",
        "--debug-modes-minimal",
        "\tOutput only minimal debugging traces of the mode checking.",
        "--debug-modes-verbose",
        "\tOutput detailed debugging traces of the mode checking.",
        "--debug-modes-pred-id <n>",
        "\tWith `--debug-modes', restrict the debugging traces to the",
        "\tmode checking of the predicate or function with the specified",
        "\tpred id.",
% --debug-dep-par-conj <n> is a developer only option,
% and it is effective only if the compiler was compiled with the right
% trace flags.
%       "--debug-dep-par-conj <n>",
%       "\tOutput detailed debugging traces during the dependent",
%       "\tAND-parallelism transformation of the predicate with the",
%       "\tpredicate id.",
        "--debug-det, --debug-determinism",
        "\tOutput detailed debugging traces of determinism analysis.",
% --debug-code-gen-pred-id <n> is a developer only option,
% and it is effective only if the compiler was compiled with the right
% trace flags.
%       "--debug-code-gen-pred-id <n>",
%       "\tOutput detailed debugging traces of code generation for the",
%       "\tpredicate or function with the given pred id.",
% The new termination analyser is currently a work-in-progress.
%
        %"--debug-term, --debug-termination",
        %"\tOutput detailed debugging traces of the termination2 analysis.",
        "--debug-opt",
        "\tOutput detailed debugging traces of the optimization process.",
        "--debug-opt-pred-id <n>",
        "\tOutput detailed debugging traces of the optimization process",
        "\tonly for the predicate/function with the specified pred id.",
        "--debug-opt-pred-name <name>",
        "\tOutput detailed debugging traces of the optimization process",
        "\tonly for the predicate/function with the specified name.",
        "--debug-pd",
        "\tOutput detailed debugging traces of the partial",
        "\tdeduction and deforestation process.",
        "--debug-liveness <pred_id>",
        "\tOutput detailed debugging traces of the liveness analysis",
        "\tof the predicate with the given predicate id.",
        "--debug-make",
        "\tOutput detailed debugging traces of the `--make' option.",
% This can be uncommented when the '--analyse-closures' option is uncommented.
% (See below.)
%       "--debug-closure",
%       "\tOutput detailed debugging traces of the closure analysis."
        "--debug-trail-usage",
        "\tOutput detailed debugging traces of the `--analyse-trail-usage'",
        "\toption.",
        "--debug-intermodule-analysis",
        "\tOutput detailed debugging traces of the `--intermodule-analysis'",
        "\toption.",
        "--debug-indirect-reuse",
        "\tOutput detailed debugging traces of the indirect reuse pass of",
        "\tthe `--structure-reuse' option.",
        "--debug-type-rep",
        "\tOutput debugging traces of type representation choices."
% The mode constraints code is still experimental so this option is
% currently commented out.
%         "--debug-mode-constraints",
%         "\tOutput detailed debugging traces of the `--prop-mode-constraints'",
%         "\toption."
    ]).

:- pred options_help_output(io::di, io::uo) is det.

options_help_output -->
    io.write_string("\nOutput Options:\n"),
    write_tabbed_lines([
        "These options are mutually exclusive.",
        "Only the first one specified will apply.",
        "If none of these options are specified, the default action",
        "is to link the named modules to produce an executable.\n",
        "-f, --generate-source-file-mapping",
        "\tOutput the module name to file name mapping for the list",
        "\tof source files given as non-option arguments to mmc",
        "\tto `Mercury.modules'. This must be done before",
        "\t`mmc --generate-dependencies' if there are any modules",
        "\tfor which the file name does not match the module name.",
        "\tIf there are no such modules the mapping need not be",
        "\tgenerated.",
        "-M, --generate-dependencies",
        "\tOutput `Make'-style dependencies for the module",
        "\tand all of its dependencies to `<module>.dep'.",
        "--generate-dependency-file",
        "\tOutput `Make'-style dependencies for the module",
        "\tto `<module>.d'.",
        "--generate-module-order",
        "\tOutput the strongly connected components of the module",
        "\tdependency graph in top-down order to `<module>.order'.",
        "\tEffective only if --generate-dependencies is also specified.",
        "--generate-standalone-interface <basename>",
        "\tOutput a stand-alone interface.",
        "\t<basename> is used as the basename of any files generated for",
        "\tthe stand-alone interface. (See the Stand-alone Interface",
        "\tchapter of the Mercury User's Guide for further details.)",
        "-i, --make-int, --make-interface",
        "\tWrite the module interface to `<module>.int',",
        "\tand write the short interface to `<module>.int2'",
        "\tThis option should only be used by mmake.",
        "--make-priv-int, --make-private-interface",
        "\tWrite the private interface to `<module>.int0'.",
        "\tThis option should only be used by mmake.",
        "--make-short-int, --make-short-interface",
        "\tWrite the unqualified short interface to `<module>.int3'.",
        "\tThis option should only be used by mmake.",
        "--make-opt-int, --make-optimization-interface",
        "\tWrite inter-module optimization information to",
        "\t`<module>.opt'.",
        "\tThis option should only be used by mmake.",
        "--make-trans-opt",
        "--make-transitive-optimization-interface",
        "\tOutput transitive optimization information",
        "\tinto the `<module>.trans_opt' file.",
        "\tThis option should only be used by mmake.",
        "-x,--make-xml-doc,--make-xml-documentation",
        "\tOutput XML documentation of the module",
        "\tinto the `<module>.xml' file.",
        "\tThis option should only be used by mmake.",
        "-P, --convert-to-mercury",
        "\tConvert to Mercury. Output to file `<module>.ugly'",
        "\tThis option acts as a Mercury ugly-printer.",
        "-t, --typecheck-only",
        "\tJust check that the code is syntactically correct and",
        "\ttype-correct. Don't check modes or determinism,",
        "\tand don't generate any code.",
        "-e, --errorcheck-only",
        "\tCheck the module for errors, but do not generate any code.",
        "-C, --target-code-only",
        "\tGenerate target code (i.e. C code in `<module>.c',",
        "\tC# code in `<module>.cs', or Java code in",
        "\t`<module>.java'), but not object code.",
        "-c, --compile-only",
        "\tGenerate C code in `<module>.c' and object code in `<module>.o'",
        "\tbut do not attempt to link the named modules.",
        % --compile-to-shared-lib is intended only for use
        % by the debugger's interactive query facility,
        % so it isn't documented.
        "--output-grade-string",
        "\tCompute the grade of the library to link with based on",
        "\tthe command line options, and print it to the standard",
        "\toutput.",
        "--output-link-command",
        "\tPrint the command used to link executables to the",
        "\tstandard output.",
        "--output-shared-lib-link-command",
        "\tPrint the command used to link shared libraries to the",
        "\tstandard output.",
        "--output-libgrades",
        "\tPrint the list of compilation grades in which a library",
        "\tto be installed should be built to the standard output.",
        "--output-cc",
        "\tPrint the command used to invoke the C compiler to the",
        "\tstandard output.",
        "--output-cc-type, --output-c-compiler-type",
        "\tPrint the C compiler type to the standard output.",
        "--output-cflags",
        "\tPrint the flags with which the C compiler will be invoked",
        "\tto the standard output.",
        "--output-csharp-compiler",
        "\tPrint the command used to invoke the C# compiler to the",
        "\tstandard output.",
        "--output-csharp-compiler-type",
        "\tPrint the C# compiler type to the standard output.",
        "--output-library-link-flags",
        "\tPrint the flags that are passed to linker in order to link",
        "\tagainst the current set of libraries. This includes the",
        "\tstandard library as well as any other libraries specified",
        "\tvia the --ml option. The flags are printed to the standard",
        "\toutput.",
        "--output-grade-defines",
        "\tPrint the flags that are passed to the C compiler to define the",
        "\tmacros used to specify the compilation grade.",
        "\tThe flags are printed to the standard output.",
        "--output-c-include-dir-flags, --output-c-include-directory-flags",
        "\tPrint the flags that are passed to the C compiler to specify",
        "\twhich directories to search for C header files.",
        "\tThis includes the C header files from the standard library.",
        "\tThe flags are printed to the standard output.",
        "--output-target-arch",
        "\tPrint the target architecture to the standard output.",
        "--output-class-dir, --output-class-directory",
        "\tPrint to standard output the name of the directory in which",
        "\tgenerated Java class files will be placed."
    ]).

:- pred options_help_aux_output(io::di, io::uo) is det.

options_help_aux_output -->
    io.write_string("\nAuxiliary Output Options:\n"),
    write_tabbed_lines([
        "--smart-recompilation",
        "\tWhen compiling, write program dependency information",
        "\tto be used to avoid unnecessary recompilations if an",
        "\timported module's interface changes in a way which does",
        "\tnot invalidate the compiled code. `--smart-recompilation'",
        "\tdoes not yet work with `--intermodule-optimization'.",
        "--no-assume-gmake",
        "\tWhen generating `.dep' files, generate Makefile",
        "\tfragments that use only the features of standard make;",
        "\tdo not assume the availability of GNU Make extensions.",
        "\tWhen generating `.dep' files, generate dependencies",
        "\tfor use by `mmc --make' in addition to the dependencies",
        "\tused by mmake.",
        "--generate-mmc-deps",
        "--generate-mmc-make-module-dependencies",
        "\tGenerate dependencies for use by `mmc --make' even",
        "\twhen using Mmake. This is recommended when building a",
        "\tlibrary for installation.",

% XXX The source-to-source debugging transform is not ready for public
% consumption.
        %"--link-ssdebug-libs",
        %"--link-ssdb-libs",
        %"\tLink the source to source debugging libraries into the",
        %"\tthe executable.",
        %"--ss-trace {none, shallow, deep}",
        %"\tThe trace level to use for source to source debugging of",
        %"\tthe given module.",

% "--trace decl" is not documented, because it is for backwards
% compatibility only. It is now equivalent to `--trace rep'.
%       "--trace {minimum, shallow, deep, decl, rep, default}",
        "--trace {minimum, shallow, deep, rep, default}",
        "\tGenerate code that includes the specified level",
        "\tof execution tracing.",
        "\tSee the Debugging chapter of the Mercury User's Guide",
        "\tfor details.",
        "--exec-trace-tail-rec",
        "\tGenerate TAIL events for self-tail-recursive calls instead of",
        "\tEXIT events. This allows these recursive calls to reuse",
        "\ttheir parent call's stack frame, but it also means that",
        "\tthe debugger won't have access to the contents of the reused",
        "\tstack frames",
%       "--suppress-trace <suppress-items>,",
%       "\tSuppress the named aspects of the execution tracing system.",
%       This is a developer-only option:
%       "--force-disable-trace",
%       "\tForce tracing to be set to trace level none.",
%       "\tThis overrides all other tracing/grade options.",
%       "\tIts main use is to turn off tracing in the browser",
%       "\tdirectory, even for .debug and .decldebug grades.",
        "--trace-optimized",
        "\tDo not disable optimizations that can change the trace.",
% "--trace-prof" is not documented because if is only intended for developers
% of the deep profiler.
%       "--trace-prof"",
%       "\tEnable tracing of deep profiling service predicates.",
% I/O tabling is deliberately not documented. It is mean to be switched on,
% with consistent parameters, in debugging grades, and to be consistently
% switched off in non-debugging grades. Inconsistent use of the options
% governing I/O tabling can yield core dumps from the debugger, so these
% options are for implementors only.
%       "--trace-table-io",
%       "\tEnable the tabling of I/O actions, to allow the debugger",
%       "\tto execute retry commands across I/O actions.",
%       "--trace-table-io-only-retry",
%       "\tSet up I/O tabling to support only retries across I/O",
%       "\tactions, not the printing of actions or declarative",
%       "\tdebugging. This reduces the size of the I/O action table.",
%       "--trace-table-io-states",
%       "\tWhen tabling I/O actions, table the io.state arguments",
%       "\ttogether with the others. This should be required iff",
%       "\tvalues of type io.state actually contain information.",
%       "--trace-table-io-require",
%       "\tRequire the tabling of I/O actions, i.e. generate an error",
%       "\tif an I/O primitive does not have the tabled_for_io",
%       "\tannotation.",
%       "--trace-table-io-all",
%       "\tTable all I/O actions even in the absence of annotations.",
%       "\tIf a primitive has no annotation specifying the type of",
%       "\ttabling required, deduce it from the values of the other",
%       "\tannotations.",
        "--trace-flag <keyword>",
        "\tEnable the trace goals that depend on the <keyword> trace flag.",
        "--profile-optimized",
        "\tDo not disable optimizations that can distort deep profiles.",
        "--no-delay-death",
        "\tWhen the trace level is `deep', the compiler normally",
        "\tpreserves the values of variables as long as possible, even",
        "\tbeyond the point of their last use, in order to make them",
        "\taccessible from as many debugger events as possible.",
        "\tHowever, it will not do this if this option is given.",
        "--delay-death-max-vars <N>",
        "\tDelay the deaths of variables only when the number of variables",
        "\tin the procedure is no more than N. The default value is 1000.",
        "--stack-trace-higher-order",
        "\tEnable stack traces through predicates and functions with",
        "\thigher-order arguments, even if stack tracing is not",
        "\tsupported in general.",
%       This is a developer-only option:
%       "--force-disable-ssdebug",
%       "\tDisable ssdebug transformation even in ssdebug grades.",
%       "--tabling-via-extra-args",
%       "\tGenerate output via extra_args in foreign_procs.",
%       "--allow-table-reset",
%       "\tGenerate C code for resetting tabling data structures.",
        "--generate-bytecode",
        "\tOutput a bytecode form of the module for use",
        "\tby an experimental debugger.",
        "-n, --line-numbers",
        "\tPut source line numbers into the generated code.",
        "\tThe generated code may be in C (the usual case),",
        "\tor in Mercury (with the option --convert-to-mercury).",
        "--no-line-numbers-around-foreign-code",
        "\tDo not put source line numbers into the generated code",
        "\taround inclusions of foreign language code.",
        "--line-numbers-for-c-headers",
        "\tPut source line numbers in the generated C header files.",
        "\tThis can make it easier to track down any problems with",
        "\tC code in foreign_decl pragmas, but may cause unnecessary",
        "\trecompilations of other modules if any of these line numbers",
        "\tchanges (e.g. because the location of a predicate declaration",
        "\tchanges in the Mercury source file).",
        "--auto-comments",
        "\tOutput comments in the generated target language file.",
% This option is for developers only. Since it can include one C comment inside
% another, the resulting code is not guaranteed to be valid C.
%       "--frameopt-comments",
%       "\tGet frameopt.m to generate comments describing its operation.",
        "\t(The code may be easier to understand if you also",
        "\tuse the `--no-llds-optimize' option.)",
        "--max-error-line-width <n>",
        "\tSet the maximum width of an error message line to <n> characters",
        "\t(unless a long single word forces the line over this limit).",
        "\tSpecifying --no-max-error-line-width removes the limit.",
        "--show-definition-line-counts",
        "\tWrite out a list of the predicates and functions defined in",
        "\tthe module, together with the names of the files containing them",
        "\tand their approximate line counts, to `<module>.defn_line_counts'.",
        "--show-local-type-representations",
        "\tWrite out information about the representations of all types",
        "\tdefined in the module being compiled to `<module>.type_repns'.",
        "--show-all-type-representations",
        "\tWrite out information about the representations of all types",
        "\tvisible in the module being compiled to `<module>.type_repns'.",
%       "--show-developer-type-representations",
%       "\tWhen writing out information about the representations of types,",
%       "\tinclude information that is of interest to mmc developers only.",
        "--show-dependency-graph",
        "\tWrite out the dependency graph to `<module>.dependency_graph'.",
        "--imports-graph",
        "\tWrite out the imports graph to `<module>.imports_graph'.",
        "\tThe imports graph contains the directed graph module A",
        "\timports module B.",
        "\tThe resulting file can be processed by the graphviz tools.",
        "\tEffective only if --generate-dependencies is also specified.",
% This option is for developers only.
%       "--dump-trace-counts <stage number or name>",
%       "\tIf the compiler was compiled with debugging enabled and is being",
%       "\trun with trace counting enabled, write out the trace counts file",
%       "\tafter the specified stage to `<module>.trace_counts.<num>-<name>'.",
%       "\tStage numbers range from 1-599.",
%       "\tMultiple dump options accumulate.",
        "-d <n>, --dump-hlds <stage number or name>",
        "\tDump the HLDS (high level intermediate representation) after",
        "\tthe specified stage to `<module>.hlds_dump.<num>-<name>'.",
        "\tStage numbers range from 1-599.",
        "\tMultiple dump options accumulate.",
        "--dump-hlds-pred-id <n>",
        "\tDump the HLDS only of the predicate/function with the given",
        "\tpred id.",
        "--dump-hlds-pred-name <name>",
        "\tDump the HLDS only of the predicate/function with the given",
        "\tname.",
% This option is for developers only.
%       "--dump-hlds-pred-name-order",
%       "\tDump the predicates in the HLDS ordered by name",
%       "\tnot ordered by pred id.",
% This option is for developers only.
%       "--dump-hlds-spec-preds",
%       "\tWith `--dump-hlds', dump the special (unify, compare, and index)",
%       "\tpredicates not in pred-id order, but in alphabetical order",
%       "\tby type constructor.",
% This option is for developers only.
%       "--dump-hlds-spec-preds-for <typename>",
%       "\tDump only the special (unify, compare, and index) predicates",
%       "\tfor the types named by the (possibly multiple) occurrences",
%       "\tof this option.",
% This option is for developers only.
%       "-D, --dump-hlds-alias <dump-alias>",
%       "\tWith `--dump-hlds', include extra detail in the dump.",
%       "\tEach dump alias is shorthand for a set of option letters.",
%       "\tThe list of aliases is in handle_options.m",
        "--dump-hlds-options <options>",
        "\tWith `--dump-hlds', include extra detail in the dump.",
        "\tEach type of detail is included in the dump if its",
        "\tcorresponding letter occurs in the option argument",
        "\t(see the Mercury User's Guide for details).",
        "--dump-hlds-inst-limit <N>",
        "\tDump at most N insts in each inst table.",
        "--dump-hlds-file-suffix <suffix>",
        "\tAppend the given suffix to the names of the files created by",
        "\tthe `--dump-hlds' option.",
        "--dump-same-hlds",
        "\tCreate a file for a HLDS stage even if the file notes only that",
        "\tthis stage is identical to the previously dumped HLDS stage.",
        "--dump-mlds <stage number or name>",
        "\tDump the MLDS (medium level intermediate representation)",
        "\tafter the specified stage, as C code,",
        "\tto`<module>.c_dump.<num>-<name>',",
        "\tand `<module>.h_dump.<num>-<name>'.",
        "\tStage numbers range from 1-99.",
        "\tMultiple dump options accumulate.",
        "--dump-mlds-pred-name <pred or func name>",
        "\tDump the MLDS (medium level intermediate representation)",
        "\tof the predicate or function with the specified name",
        "\tat the stages specified by the --dump-mlds option.",
        "\tThe dump file will consist of the predicates and functions",
        "\tnamed by all the occurrences of this option (there may be",
        "\tmore than one), and nothing else.",
        "--verbose-dump-mlds <stage number or name>",
        "\tDump the internal compiler representation of the MLDS, after",
        "\tthe specified stage, to `<module>.mlds_dump.<num>-<name>'."
% The mode constraints code is still experimental so these options are
% currently commented out.
%       "--mode-constraints"
%       "\tRun constraint based mode analysis. The default is to",
%       "\tuse the robdd solution using the full (subtyping)",
%       "\tconstraints and dump results.",
%       "--simple-mode-constraints",
%       "\tUse only the simplified constraint system when running",
%       "\tthe robdd solver constraints based mode analysis.",
%       "--prop-mode-constraints",
%       "\tUse the new propagation solver for constraints based",
%       "\tmode analysis.",
%       "--compute-goal-modes",
%       "\tCompute goal modes.",
    ]).

:- pred options_help_semantics(io::di, io::uo) is det.

options_help_semantics -->
    io.write_string("\nLanguage semantics options:\n"),
    io.write_string("(See the Mercury language reference manual for detailed explanations.)\n"),
    write_tabbed_lines([
        "--no-reorder-conj",
        "\tExecute conjunctions left-to-right except where the modes imply",
        "\tthat reordering is unavoidable.",
        "--no-reorder-disj",
        "\tExecute disjunctions strictly left-to-right.",
        "--no-fully-strict",
        "\tAllow infinite loops or goals with determinism erroneous to be",
        "\toptimised away.",
        "--allow-stubs",
        "\tAllow procedures to have no clauses. Any calls to",
        "\tsuch procedures will raise an exception at run-time.",
        "\tThis option is sometimes useful during program development.",
        "\t(See also the documentation for the `--warn-stubs' option",
        "\tin the ""Warning Options"" section.)",
        "--infer-all",
        "\tAbbreviation for `--infer-types --infer-modes --infer-det'.",
        "--infer-types",
        "\tIf there is no type declaration for a predicate or function,",
        "\ttry to infer the type, rather than just reporting an error.",
        "--infer-modes",
        "\tIf there is no mode declaration for a predicate,",
        "\ttry to infer the modes, rather than just reporting an error.",

        "--no-infer-det, --no-infer-determinism",
        "\tIf there is no determinism declaration for a procedure,",
        "\tdon't try to infer the determinism, just report an error.",
        "--type-inference-iteration-limit <n>",
        "\tPerform at most <n> passes of type inference (default: 60).",
        "--mode-inference-iteration-limit <n>",
        "\tPerform at most <n> passes of mode inference (default: 30).",
        "--event-set-file-name <filename>",
        "\tGet the specification of user-defined events from <filename>."
    ]).

:- pred options_help_ctgc(io::di, io::uo) is det.

options_help_ctgc -->
    io.write_string("\nCompile Time Garbage Collection Options:\n"),
    write_tabbed_lines([
        "--structure-sharing",
        "\tPerform structure sharing analysis.",
        "--structure-sharing-widening <n>",
        "\tPerform widening when the set of structure sharing pairs becomes",
        "\tlarger than <n>. When n=0, widening is not enabled.",
        "\t(default: 0).",
        "--structure-reuse, --ctgc",
        "\tPerform structure reuse analysis (Compile Time Garbage",
        "\tCollection).",
        "--structure-reuse-constraint {same_cons_id, within_n_cells_difference}",
        "--ctgc-constraint {same_cons_id, within_n_cells_difference}",
        "\tConstraint on the way we allow structure reuse. `same_cons_id'",
        "\tspecifies that reuse is only allowed between terms of the same",
        "\ttype and constructor. `within_n_cells_difference' states that",
        "\treuse is allowed as long as the arities between the reused term",
        "\tand new term does not exceed a certain threshold. The threshold",
        "\tneeds to be set using `--structure-reuse-constraint-arg'.",
        "\t(default: within_n_cells_difference, with threshold 0)",
        "--structure-reuse-constraint-arg, --ctgc-constraint-arg",
        "\tSpecify the maximum difference in arities between the terms that",
        "\tcan be reused, and the terms that reuse these terms.",
        "\t(default: 0)"

% This option is for developers only.
%       "--structure-reuse-max-conditions",
%       "\tSoft limit on the number of reuse conditions to accumulate",
%       "\tfor a procedure. (default: 10)"

% This option is likely to break many optimisations which haven't been updated.
%       "--structure-reuse-free-cells",
%       "\tImmediately free cells which are known to be dead but which",
%       "\tcannot be reused."
    ]).

:- pred options_help_termination(io::di, io::uo) is det.

options_help_termination -->
    io.write_string("\nTermination Analysis Options:\n"),
    write_tabbed_lines([
        "--enable-term, --enable-termination",
        "\tAnalyse each predicate to discover if it terminates.",
        "--chk-term, --check-term, --check-termination",
        "\tEnable termination analysis, and emit warnings for some",
        "\tpredicates or functions that cannot be proved to terminate.",
        "\tIn many cases where the compiler is unable to prove termination",
        "\tthe problem is either a lack of information about the",
        "\ttermination properties of other predicates, or because language",
        "\tconstructs (such as higher order calls) were used which could",
        "\tnot be analysed. In these cases the compiler does not emit a",
        "\twarning of non-termination, as it is likely to be spurious.",
        "--verb-chk-term, --verb-check-term, --verbose-check-termination",
        "\tEnable termination analysis, and emit warnings for all",
        "\tpredicates or functions that cannot be proved to terminate.",
        "--term-single-arg <n>, --termination-single-argument-analysis <n>",
        "\tWhen performing termination analysis, try analyzing",
        "\trecursion on single arguments in strongly connected",
        "\tcomponents of the call graph that have up to <n> procedures.",
        "\tSetting this limit to zero disables single argument analysis.",
        "--termination-norm {simple, total, num-data-elems}",
        "\tThe norm defines how termination analysis measures the size",
        "\tof a memory cell. The `simple' norm says that size is always",
        "\tone. The `total' norm says that it is the number of words",
        "\tin the cell. The `num-data-elems' norm says that it is the",
        "\tnumber of words in the cell that contain something other",
        "\tthan pointers to cells of the same type.",
        "--term-err-limit <n>, --termination-error-limit <n>",
        "\tPrint at most <n> reasons for any single termination error",
        "\t(default: 3).",
        "--term-path-limit <n>, --termination-path-limit <n>",
        "\tPerform termination analysis only on predicates",
        "\twith at most <n> paths (default: 256)."

% The following options are used to control the new termination analyser.
% They are currently disabled because that is still a work-in-progress.
%
%       "--enable-term2, --enable-termination2",
%       "\tAnalyse each predicate to discover if it terminates.",
%       "\tThis uses an alternative termination analysis based",
%       "\ton convex constraints.",
%       "--chk-term2, --check-termination2",
%       "\tEnable the alternative termination analysis, and emit warnings for",
%       "\tsome predicates or functions that cannot be proved to terminate. In",
%       "\tmany cases where the compiler is unable to prove termination",
%       "\tthe problem is either a lack of information about the",
%       "\ttermination properties of other predicates, or because language",
%       "\tconstructs (such as higher order calls) were used which could",
%       "\tnot be analysed. In these cases the compiler does not emit a",
%       "\twarning of non-termination, as it is likely to be spurious.",
%       "--verb-chk-term2, --verb-check-term2, --verbose-check-termination2",
%       "--termination2-norm {simple, total, num-data-elems}",
%       "\tTell the alternative termination analyser which norm to use.",
%       "\tSee the description of the `--termination-norm' option for a",
%       "\tdescription of the different types of norm available."
%       "--term2-widening-limit <n>, --termination2-widening-limit <n>",
%       "\tSet the threshold for the number of iterations after which the",
%       "\targument size analyser invokes widening.",
%       "--term2-propagate-failure-constrs, --termination2-propagate-failure-constraints",
%       "\tMake the argument analyser infer information about the sizes of any"
%       "\tinputs to a goal in contexts where that goal fails."
%       "--term2-max-matrix-size <n>, --termination2-maximum-matrix-size <n>",
%       "\tLimit the sizes of constraints systems in the analyser to <n>",
%       "\tconstraints. Use approximations of some constraint operations,",
%       "\tsuch as projection, if this threshold is exceeded. This will",
%       "\tspeed up the analysis at the cost of reduced precision.",

% This option is for developers only.
% It is useful for benchmarking the argument size analysis.
%       "--term2-argument-size-analysis-only, --term2-arg-size-analysis-only",
%       "\tPerform argument size analysis on each SCC but do not",
%       "\tattempt to infer termination,"
    ]).

:- pred options_help_compilation_model(io::di, io::uo) is det.

options_help_compilation_model -->
    io.write_string("\nCompilation model options:\n"),
    write_tabbed_lines([
        "The following compilation options affect the generated",
        "code in such a way that the entire program must be",
        "compiled with the same setting of these options,",
        "and it must be linked to a version of the Mercury",
        "library which has been compiled with the same setting."
    ]),
    io.write_string("\n"),
    write_tabbed_lines([
        "-s <grade>, --grade <grade>",
        "\tSelect the compilation model. The <grade> should be one of",
        "\tthe base grades `none', `reg', `asm_fast', `hlc', `java',",
        "\t`csharp' or `erlang'",

% The following base grade components are not publicly documented:
%
%  asm_jump
%  fast
%  jump        These three are not tested as much as the other
%              three LLDS base grades and have proved to be a bit
%              delicate in any case.
%
%  hlc_nest
%  hl_nest     These two rely on GCC nested functions extension.
%
%  hl          Not useful.
%
        "\tor one of those with one or more of the grade modifiers",
        "\t`.gc', `.prof', `.memprof', `.profdeep', `.tr',",
        "\t`.spf', `.stseg', `.debug', and/or `.par' appended.",
        "\tDepending on your particular installation, only a subset",
        "\tof these possible grades will have been installed.",
        "\tAttempting to use a grade which has not been installed",
        "\twill result in an error at link time."
    ]),

    io.write_string("\n    Target selection compilation model options:\n"),
    write_tabbed_lines([
        %"--target c\t\t\t(grades: none, reg, jump, fast,",
        %"\t\t\t\t\tasm_jump, asm_fast, hl, hlc)",
        "--target c\t\t\t(grades: none, reg, asm_fast, hlc)",
        "--target csharp\t\t\t(grades: csharp)",
        "--target java\t\t\t(grades: java)",
        "--target erlang\t\t\t(grades: erlang)",
        "\tSpecify the target language: C, C#, Java or Erlang.",
        "\tThe default is C.",
        "\tTargets other than C imply `--high-level-code' (see below).",

        "--csharp",
        "\tAn abbreviation for `--target csharp'.",
        "--csharp-only",
        "\tAn abbreviation for `--target csharp --target-code-only'.",
        "\tGenerate C# code in `<module>.cs', but do not generate",
        "\tobject code.",

        "--java",
        "\tAn abbreviation for `--target java'.",
        "--java-only",
        "\tAn abbreviation for `--target java --target-code-only'.",
        "\tGenerate Java code in `<module>.java', but do not generate",
        "\tobject code.",

        "--erlang",
        "\tAn abbreviation for `--target erlang'.",
        "--erlang-only",
        "\tAn abbreviation for `--target erlang --target-code-only'.",
        "\tGenerate Erlang code in `<module>.erl', but do not generate",
        "\tobject code.",

        "--compile-to-c",
        "\tAn abbreviation for `--target c --target-code-only'.",
        "\tGenerate C code in `<module>.c', but do not generate object",
        "\tcode."
    ]),

    io.write_string("\n    Optional feature compilation model options:\n"),
    io.write_string("\n      Debugging\n"),
    write_tabbed_lines([
        "--debug\t\t\t\t(grade modifier: `.debug')",
        "\tEnable Mercury-level debugging.",
        "\tSee the Debugging chapter of the Mercury User's Guide",
        "\tfor details.",
        "\tThis option is not yet supported for the `--high-level-code'",
        "\tback-ends.",
        "--decl-debug\t\t\t\t(grade modifier: `.decldebug')",
        "\tEnable full support for declarative debugging.",
        "\tThis allows subterm dependency tracking in the declarative",
        "\tdebugger.",
        "\tSee the Debugging chapter of the Mercury User's Guide",
        "\tfor details.",
        "\tThis option is not yet supported for the `--high-level-code'",
        "\tback-ends."
% XXX The source-to-source debugging transform is not ready for public
% consumption.
%       "--ss-debug\t\t\t\t(grade modifier: `.ssdebug')",
%       "\tEnable the source-to-source debugging transform."
    ]),
    io.write_string("\n      Profiling\n"),
    write_tabbed_lines([
        "-p, --profiling, --time-profiling",
        "\t\t\t\t(grade modifier: `.prof')",
        "\tEnable time and call profiling. Insert profiling hooks in the",
        "\tgenerated code, and also output some profiling",
        "\tinformation (the static call graph) to the file",
        "\t`<module>.prof'.",
        "\tThis option is not supported for the C#, Erlang or Java back-ends.",
        "--memory-profiling\t\t(grade modifier: `.memprof')",
        "\tEnable memory and call profiling.",
        "\tThis option is not supported for the C#, Erlang or Java back-ends.",
        "--deep-profiling\t\t(grade modifier: `.profdeep')",
        "\tEnable deep profiling.",
        "\tThis option is not supported for the high-level C, C#, Erlang",
        "\tor Java back-ends.",

% This option is not documented, it is intended for use by developers only.
%
%       "--pre-prof-transforms-simplify",
%       "\tForce the pre-profiling simplification pass that is usually",
%       "\tenabled when building a profiling version of a program. This",
%       "\tallows a developer to enable this pass when using a",
%       "\tnon-profiling build. It can be used to test that generated code",
%       "\tintroduced in earlier passes is well-formed before it is",
%       "\tpotentially removed by the dead procedure elimination pass later",
%       "\ton.",
%

% XXX The following options are not documented,
% because they are currently not useful.
% The idea was for you to be able to use --profile-calls
% and --profile-time separately, but that doesn't work
% because compiling with --profile-time instead of
% --profile-calls results in different code addresses,
% so you can't combine the data from versions of
% your program compiled with different options.
%
%       "--profile-calls\t\t(grade modifier: `.profcalls')",
%       "\tSimilar to `--profiling', except that only gathers",
%       "\tcall counts, not timing information.",
%       "\tUseful on systems where time profiling is not supported,",
%       "\tbut not as useful as `--memory-profiling'.",
%       "--profile-time\t\t(grade modifier: `.proftime')",
%       "\tSimilar to `--profiling', except that it only gathers",
%       "\ttiming information, not call counts.",
%       "--profile-memory\t\t(grade modifier: `.profmem')",
%       "\tSimilar to `--memory-profiling', except that it only",
%       "\tgathers memory usage information, not call counts.",

        "--no-coverage-profiling",
        "\tDisable coverage profiling.",
% The following options are for implementors only (intended for experiments).
%       "--coverage-profiling-via-calls",
%       "\tUse calls to implement coverage points, not inline foreign code.",

%       "--coverage-profiling-static",
%       "\tDisable dynamic coverage profiling, this uses less memory and may",
%       "\tbe faster.",

%       "Switches to effect coverage profiling (part of deep profiling).",
%       "they enable different types of coverage points.",

%       "--no-profile-deep-coverage-after-goal",
%       "\tDisable coverage points after goals.",
%       "--no-profile-deep-coverage-branch-ite",
%       "\tDisable coverage points at the beginning of then and else",
%       "\tbranches.",
%       "--no-profile-deep-coverage-branch-switch",
%       "\tDisable coverage points at the beginning of switch branches.",
%       "--no-profile-deep-coverage-branch-disj",
%       "\tDisable coverage points at the beginning of disjunction branches.",

%       I believe these options are broken - pbone.
%       "Switches to tune the coverage profiling pass, useful for",
%       "debugging.",
%
%       "--no-profile-deep-coverage-use-portcounts",
%       "\tTurn off usage of port counts in the deep profiler to provide",
%       "\tsome coverage information.",
%       "--no-profile-deep-coverage-use-trivial",
%       "\tTurn off usage of trivial goal information",

        "--profile-for-feedback",
        "\tSelect deep profiling options suitable for profiler directed",
        "\timplicit parallelism.",
        "\t--profile-for-implicit-parallelism is a deprecated synonym for",
        "\tthis option",

        % These are commented out as this feature is still experimental.
        %"--record-term-sizes-as-words\t\t(grade modifier: `.tsw')",
        %"\tAugment each heap cell with its size in words.",
        %"--record-term-sizes-as-cells\t\t(grade modifier: `.tsc')",
        %"\tAugment each heap cell with its size in cells.",

        "--experimental-complexity <filename>",
        "\tEnable experimental complexity analysis for the predicates",
        "\tlisted in the given file.",
        "\tThis option is supported for the C back-end, with",
        "\t`--no-highlevel-code'.",

        "--threadscope\t\t(grade modifier: `.threadscope')",
        "\tEnable support for profiling parallel execution.",
        "\tThis option is supported by the low-level C back-end parallel",
        "\tgrades on some processors, See README.ThreadScope for details."
    ]),

    io.write_string("\n      Miscellaneous optional features\n"),
    write_tabbed_lines([
        "--gc {none, boehm, hgc, accurate, automatic}",
        "--garbage-collection {none, boehm, hgc, accurate, automatic}",
        "\t\t\t\t(`java', `csharp', and `erlang'",
        "\t\t\t\t\tgrades use `--gc automatic',",
        "\t\t\t\t`.gc' grades use `--gc boehm',",
        "\t\t\t\t`.hgc' grades use `--gc hgc',",
        "\t\t\t\tother grades use `--gc none'.)",
        "\tSpecify which method of garbage collection to use",
        "\t(default: boehm).",
        "\t`boehm' is Hans Boehm et al's conservative collector.",
        "\t`hgc' is our own conservative collector;",
        "\t`accurate' is our own type-accurate copying GC;",
        "\tit requires `--high-level-code'.",
        "\t`automatic' means the target language provides it.",
        "\tThis is the case for the C#, Java and Erlang back-ends,",
        "\twhich always use the garbage collector of the underlying",
        "\timplementation.",
        "--use-trail\t\t\t(grade modifier: `.tr')",
        "\tEnable use of a trail.",
        "\tThis is necessary for interfacing with constraint solvers,",
        "\tor for backtrackable destructive update.",
        "\tThis option is not yet supported for the C#, Java",
        "\tor Erlang back-ends.",
        "--trail-segments\t\t\t(grade modifier: `.trseg')",
        "\tAs above, but use a dynamically sized trail that is composed",
        "\tof small segments. This can help to avoid trail exhaustion",
        "\tat the cost of increased execution time.",
        "--parallel\t\t(grade modifier: `.par')",
        "\tEnable parallel execution support for the low-level C grades.",
        "\tEnable concurrency (via pthreads) for the high-level C grades.",
        "--maybe-thread-safe {yes, no}",
        "\tSpecify how to treat the `maybe_thread_safe' foreign code",
        "\tattribute. `yes' means that a foreign procedure with the",
        "\t`maybe_thread_safe' option is treated as though it has a",
        "\t`thread_safe' attribute. `no' means that the foreign",
        "\tprocedure is treated as though it has a `not_thread_safe'",
        "\tattribute. The default is `no'.",
        "--single-prec-float\t\t(grade modifier: `.spf')",
        "\tUse single precision floats so that, on 32-bit machines,",
        "\tfloating point values don't need to be boxed. Double",
        "\tprecision floats are used by default.",
        "\tThis option is not supported for the C#, Java or Erlang back-ends."
        % This is commented out as this feature is still experimental.
        %"--extend-stacks-when-needed",
        %"\tSpecify that code that increments a stack pointer must",
        %"\textend the stack when this is needed.",
        % RBMM is undocumented since it is still experimental.
        % should also document rbmmd rbmmp rbmmdp
        %"--use-regions\t\t(grade modifier: `.rbmm')",
        %"\tEnable support for region-based memory management."
        %"--use-alloc-regions",
        %"\tCompute and use the exact set of regions",
        %"\t that may be allocated into by a call."
    ]),

    io.write_string("\n    LLDS back-end compilation model options:\n"),
    write_tabbed_lines([

        %"--gcc-global-registers\t\t(grades: reg, fast, asm_fast)",
        %"--no-gcc-global-registers\t(grades: none, jump, asm_jump)",
        "--gcc-global-registers\t\t(grades: reg, asm_fast)",
        "--no-gcc-global-registers\t(grades: none)",
        "\tSpecify whether or not to use GNU C's",
        "\tglobal register variables extension.",
        "\tThis option is ignored if the `--high-level-code' option is",
        "\tenabled.",
        %"--gcc-non-local-gotos\t\t(grades: jump, fast, asm_jump, asm_fast)",
        %"--no-gcc-non-local-gotos\t(grades: none, reg)",
        "--gcc-non-local-gotos\t\t(grades: asm_fast)",
        "--no-gcc-non-local-gotos\t(grades: none, reg)",
        "\tSpecify whether or not to use GNU C's",
        "\t""labels as values"" extension.",
        "\tThis option is ignored if the `--high-level-code' option is",
        "\tenabled.",
        %"--asm-labels\t\t\t(grades: asm_jump, asm_fast)",
        %"--no-asm-labels\t\t\t(grades: none, reg, jump, fast)",
        "--asm-labels\t\t\t(grades: asm_fast)",
        "--no-asm-labels\t\t\t(grades: none, reg)",
        "\tSpecify whether or not to use GNU C's",
        "\tasm extensions for inline assembler labels.",
        "\tThis option is ignored if the `--high-level-code' option is",
        "\tenabled.",
        "--stack-segments\t\t(grade modifier: `.stseg')",
        "\tSpecify whether to use dynamically sized stacks that are",
        "\tcomposed of small segments. This can help to avoid stack",
        "\texhaustion at the cost of increased execution time.",
        "\tThis option is not supported by the `--high-level-code'",
        "\tback-ends."
        % This is a developer only option.
%       "--use-float-registers",
%       "(This option is not for general use.)",
%       "\tUse float registers for argument passing."
    ]),

    io.write_string("\n    MLDS back-end compilation model options:\n"),
    write_tabbed_lines([
        "-H, --high-level-code\t\t\t(grades: hlc, csharp, java)",
        "\tUse an alternative back-end that generates high-level code",
        "\trather than the very low-level code that is generated by our",
        "\toriginal back-end.",
        "--high-level-data\t\t\t(grades: csharp, java)",
        "\tUse an alternative higher-level data representation.",
%       "--high-level\t\t\t(grades: hl, hl_nest, il, csharp, java)",
        "--high-level\t\t\t(grades: csharp, java)",
        "\tAn abbreviation for `--high-level-code --high-level-data'."
% The --det-copy-out option is not yet documented,
% because it is not yet tested much and probably not very useful,
% except for Java, where it is the default.
%       "--det-copy-out",
%       "\tSpecify whether to handle output arguments for det/semidet",
%       "\tprocedures using return-by-value rather than pass-by-reference.",
%       "\tThis option is ignored if the `--high-level-code' option is not enabled.",
% The --nondet-copy-out option is not yet documented,
% because it is probably not very useful except for Java,
% where it is the default.
%       "--nondet-copy-out",
%       "\tSpecify whether to handle output arguments for nondet",
%       "\tprocedures using pass-by-value rather than pass-by-reference.",
%       "\tThis option is ignored if the `--high-level-code' option is not enabled.",
% The --put-commit-in-own-func option is not documented because
% it is enabled automatically (by handle_options) in the situations
% where it is needed; the user should never need to set it.
%       "--put-commit-in-own-func",
%       "\tPut each commit in its own C function.",
%       "\tThis option only affects the MLDS back-ends.",
%       "\tIt is needed for the high-level C back-end,",
%       "\twhere commits are implemented via setjmp()/longjmp(),",
%       "\tsince longjmp() may clobber any non-volatile local vars",
%       "\tin the function that called setjmp().",
% The --put-nondet-env-on-heap option is not documented because
% it is enabled automatically (by handle_options) in the situations
% where it is needed; the user should never need to set it.
%       "--put-nondet-env-on-heap",
%       "\tAllocate the environment structures used for",
%       "\tnondeterministic Mercury procedures on the heap,",
%       "\trather than on the stack."
%   ])
    ]),

    io.write_string("\n    Developer compilation model options:\n"),
    io.write_string("\n      Data representation\n"),
    write_tabbed_lines([
        "--tags {none, low, high}      (This option is not for general use.)",
        "\tSpecify whether to use the low bits or the high bits of",
        "\teach word as tag bits (default: low).",
    %   "\t\t`--tags none' implies `--num-tag-bits 0'.",
        "--num-tag-bits <n>            (This option is not for general use.)",
        "\tUse <n> tag bits."

        % The --conf-low-tag-bits option is reserved for use
        % by the `mmc' script; it is deliberately not documented.

        % The --bits-per-word option is intended for use
        % by the `mmc' script; it is deliberately not documented.

        % The --bytes-per-word option is intended for use
        % by the `mmc' script; it is deliberately not documented.

        % This is a developer only option.
%       "--unboxed-float",
%       "(This option is not for general use.)",
%       "\tDo not box floating point numbers.",
%       "\tThis assumes that a Mercury float will fit in a word.",
%       "\tThe C code needs to be compiled with `-UMR_BOXED_FLOAT'.",
%       "\tIt may also need to be compiled with",
%       "\t`-DMR_USE_SINGLE_PREC_FLOAT', if double precision",
%       "\tfloats don't fit into a word."

%        % This is a developer only option.
%        "--unboxed-int64s",
%        "(This option is not for general use.)",
%        "\tDo not box 64-bit integer numbers",
%        "\tThis assumes that word size of the target machine is at least",
%        "\t64-bits in size.",
%        "\tThe C code needs to be compiled with `-UMR_BOXED_INT64S'.",

        % This is a developer only option.
%       "--no-unboxed-no-tag-types",
%       "(This option is not for general use.)",
%       "\tBox no-tag types. This option is disabled by default."

        % This is a developer only option.
%       "--arg-pack-bits <n>",
%       "(This option is not for general use.)",
%       "\tThe number of bits in a word in which to pack constructor"
%       "arguments.",

        % This is a developer only option.
%       "--no-allow-double-word-fields",
%       "(This option is not for general use.)",
%       "\tDisallow storing a single constructor argument in two words"
%       "(namely, double-precision floats).",
    ]),
    io.write_string("\n      Developer optional features\n"),
    write_tabbed_lines([
        "--use-minimal-model-stack-copy",
        "(This option is not for general use.)",
        "\tEnable the use of the standard form of minimal model tabling.",

        "--use-minimal-model-own-stacks",
        "(This option is not for general use.)",
        "\tEnable the use of an experimental form of minimal model tabling.",

        "--minimal-model-debug",
        "(This option is not for general use.)",
        "\tEnables extra data structures that assist in debugging",
        "\tminimal model tabling.",

        "--no-type-layout",
        "(This option is not for general use.)",
        "\tDon't output type_ctor_layout structures or references",
        "\tto them. (The C code also needs to be compiled with",
        "\t`-DNO_TYPE_LAYOUT')."

        % This is a developer only option.
%       "--basic-stack-layout",
%       "(This option is not for general use.)",
%       "\tGenerate the simple stack_layout structures required",
%       "\tfor stack traces.",

        % This is a developer only option.
%       "--agc-stack-layout",
%       "(This option is not for general use.)",
%       "\tGenerate the stack_layout structures required for",
%       "\taccurate garbage collection.",

        % This is a developer only option.
%       "--procid-stack-layout",
%       "(This option is not for general use.)",
%       "\tGenerate the stack_layout structures required for",
%       "\tlooking up procedure identification information.",

        % This is a developer only option.
%       "--trace-stack-layout",
%       "(This option is not for general use.)",
%       "\tGenerate the stack_layout structures required for",
%       "\texecution tracing.",

        % This is a developer only option.
%       "--body-typeinfo-liveness",
%       "(This option is not for general use.)",
%       For documentation, see the comment in the type declaration.

        % This is a developer only option.
%       "--can-compare-constants-as-ints",
%       "(This option is not for general use.)",
%       For documentation, see the comment in the type declaration.

        % This is a developer only option.
%       "--pretest-equality-cast-pointers",
%       "(This option is not for general use.)",
%       For documentation, see the comment in the type declaration.

        % This is a developer only option.
%       "--can-compare-compound-values"
%       "(This option is not for general use.)",
%       For documentation, see the comment in the type declaration.

        % This is a developer only option.
%       "--lexically-order-constructors"
%       "(This option is not for general use.)",
%       For documentation, see the comment in the type declaration.

        % This is a developer only option.
%       "--mutable-always-boxed",
%       "(This option is not for general use.)",
%       For documentation, see the comment in the type declaration.

        % This is a developer only option.
%       "--delay-partial-instantiations",
%       "(This option is not for general use.)",
%       For documentation, see delay_partial_inst.m

        % This is a developer only option.
%       "--allow-defn-of-builtins",
%       "(This option is not for general use.)",
%       For documentation, see the comment in the type declaration.

        % This is a developer only option.
%       "--special-preds",
%       "(This option is not for general use.)",
%       For documentation, see the comment in the type declaration.

        % All these are developer only options.
%       "(These options are not for general use.)",
%       For documentation, see runtime/mercury_region.h.
%       "--size-region-ite-fixed"
%       "--size-region-disj-fixed"
%       "--size-region-commit-fixed"
%       "--size-region-ite-protect"
%       "--size-region-ite-snapshot"
%       "--size-region-disj-protect"
%       "--size-region-disj-snapshot"
%       "--size-region-commit-entry"

        % This is a developer only option.
%       "--allow-multi-arm-switches",
%       "(This option is not for general use.)",
%       Allow the compiler to generate switches in which one arm handles
%       more than one cons_id.

        % This is a developer only option.
%       "--type-check-constraints",
%       "(This option is not for general use.)",
%       Use the constraint based type checker instead of the old one.
    ]).

:- pred options_help_code_generation(io::di, io::uo) is det.

options_help_code_generation -->
    io.write_string("\nCode generation options:\n"),
    write_tabbed_lines([
%       "--low-level-debug",
%       "\tEnables various low-level debugging stuff, that was in",
%       "\tthe distant past used to debug the low-level code generation.",
%       "\tYou don't want to use this option unless you are hacking",
%       "\tthe Mercury compiler itself (and probably not even then).",
%       "\tCauses the generated code to become VERY big and VERY",
%       "\tinefficient. Slows down compilation a LOT.",

%       "--table-debug",
%       "\tEnables the generation of code that helps to debug tabling",
%       "\tprimitives.",

        "--no-trad-passes",
        "\tThe default `--trad-passes' completely processes each predicate",
        "\tbefore going on to the next predicate.",
        "\tThis option tells the compiler",
        "\tto complete each phase of code generation on all predicates",
        "\tbefore going on the next phase on all predicates.",
    %   "--parallel-liveness",
    %   "Use multiple threads when computing liveness.",
    %   "At the moment this option implies `--no-trad-passes',",
    %   "and requires the compiler to be built in a",
    %   "low-level parallel grade and running with multiple engines.",
    %   "--parallel-code-gen",
    %   "Use multiple threads when generating code.",
    %   "At the moment this option implies `--no-trad-passes',",
    %   "and requires the compiler to be built in a",
    %   "low-level parallel grade and running with multiple engines.",
        "--no-reclaim-heap-on-nondet-failure",
        "\tDon't reclaim heap on backtracking in nondet code.",
        "--no-reclaim-heap-on-semidet-failure",
        "\tDon't reclaim heap on backtracking in semidet code.",
        "--no-reclaim-heap-on-failure",
        "\tCombines the effect of the two options above.",

        "--max-jump-table-size=<n>",
        "\tThe maximum number of entries a jump table can have.",
        "\tThe special value 0 indicates the table size is unlimited.",
        "\tThis option can be useful to avoid exceeding fixed limits",
        "\timposed by some C compilers.",

        % This is a developer only option.
%       "--compare-specialization=<n>",
%       "\tGenerate quadratic instead of linear compare predicates for",
%       "\ttypes with up to n function symbols. Higher values of n lead to",
%       "\tfaster but also bigger compare predicates.",

        % This is a developer only option.
%       "--no-should-pretest-equality",
%       "\tIf specified, do not add a test for the two values being equal",
%       "\tas words to the starts of potentially expensive unify and compare",
%       "\tpredicates."

        "--fact-table-max-array-size <n>",
        "\tSpecify the maximum number of elements in a single",
        "\t`:- pragma fact_table' data array (default: 1024).",
        "--fact-table-hash-percent-full <percentage>",
        "\tSpecify how full the `:- pragma fact_table' hash tables",
        "\tshould be allowed to get. Given as an integer percentage",
        "\t(valid range: 1 to 100, default: 90)."

% This option is not yet documented because it is not yet useful -- currently
% we don't take advantage of GNU C's computed gotos extension.
%       "--no-prefer-switch",
%       "\tGenerate code using computed gotos rather than switches.",
%       "\tThis makes the generated code less readable, but potentially",
%       "\tslightly more efficient.",
%       "\tThis option has no effect unless the `--high-level-code' option",
%       "\tis enabled.",
% These options are for implementors only, for use in testing and benchmarking.
%       "--prefer-while-loop-over-jump-self",
%       "\tGenerate code for tail-recursive single procedures using an",
%       "\tinfinite while loop, with tail calls being done by a continue.",
%       "\tThe alternative is a label at the start of the procedure,",
%       "\twith tail calls being done by a jump to the label.",
%       "\tThis option has no effect unless the `--high-level-code' option",
%       "\tis enabled.",
%       "--prefer-while-loop-over-jump-mutual",
%       "\tGenerate code for tail-recursive-SCCs using an infinite while loop",
%       "\twrapped around a switch, with one switch arm for each procedure",
%       "\tin the TSCC, with tail calls being done by setting the value of",
%       "\tthe switched-on variable and a continue. The alternative is",
%       "\ta simple label before the code of each procedure, with tail calls",
%       "\tbeing done by a jump to the label.",
%       "\tThis option has no effect unless the `--high-level-code' option",
%       "\tis enabled.",
% This optimization is for implementors only. Turning this option on provides
% the fairest possible test of --optimize-saved-vars-cell.
%       "--no-opt-no-return-calls",
%       "\tDo not optimize the stack usage of calls that cannot return.",

        % This is a developer only option.
%        "--debug-class-init",
%        "\tIn Java grades, generate code that causes a trace of class",
%        "\tinitialization to be printed to the standard output when the",
%        "\tenvironment variable MERCURY_DEBUG_CLASS_INIT is defined."
    ]),

    io.write_string("\n    Code generation target options:\n"),
    write_tabbed_lines([
        "--branch-delay-slot    \t(This option is not for general use.)",
        "\tAssume that branch instructions have a delay slot.",
        "--num-real-r-regs <n>  \t(This option is not for general use.)",
        "\tAssume registers r1 up to r<n> are real general purpose",
        "\tregisters.",
        "--num-real-f-regs <n>  \t(This option is not for general use.)",
        "\tAssume registers f1 up to f<n> are real floating point",
        "\tregisters.",
        "--num-real-r-temps <n> \t(This option is not for general use.)",
        "\tAssume that <n> non-float temporaries will fit into",
        "\treal machine registers.",
        "--num-real-f-temps <n> \t(This option is not for general use.)",
        "\tAssume that <n> float temporaries will fit into",
        "\treal machine registers."
    ]).

:- pred options_help_optimization(io::di, io::uo) is det.

options_help_optimization -->
    io.write_string("\nOptimization Options:\n"),
    write_tabbed_lines([
        "-O <n>, --opt-level <n>, --optimization-level <n>",
        "\tSet optimization level to <n>.",
        "\tOptimization level -1 means no optimization",
        "\twhile optimization level 6 means full optimization.",
    %   "\t\tFor a full description of each optimization level,",
    %   "\t\tsee the Mercury User's Guide.",
        "--opt-space, --optimize-space",
        "\tTurn on optimizations that reduce code size",
        "\tand turn off optimizations that significantly",
        "\tincrease code size.",
        "--intermod-opt",
        "--intermodule-optimization",
        "\tPerform inlining and higher-order specialization of",
        "\tthe code for predicates imported from other modules.",
        "\tThis option must be set throughout the compilation process.",
        "--trans-intermod-opt",
        "--transitive-intermodule-optimization",
        "\tImport the transitive intermodule optimization data.",
        "\tThis data is imported from `<module>.trans_opt' files.",
        "\tNote that `--transitive-intermodule-optimization' does not",
        "\twork with `mmc --make'.",
        "--no-read-opt-files-transitively",
        "\tOnly read the inter-module optimization information",
        "\tfor directly imported modules, not the transitive",
        "\tclosure of the imports.",
        "--use-opt-files",
        "\tPerform inter-module optimization using any",
        "\t`.opt' files which are already built,",
        "\te.g. those for the standard library, but do",
        "\tnot build any others.",
        "--use-trans-opt-files",
        "\tPerform inter-module optimization using any",
        "\t`.trans_opt' files which are already built,",
        "\te.g. those for the standard library, but do",
        "\tnot build any others.",
        "--intermodule-analysis",
        "\tPerform analyses such as termination analysis and",
        "\tunused argument elimination across module boundaries.",
        "\tThis option is not yet fully implemented.",
        "--analysis-repeat <n>",
        "\tThe maximum number of times to repeat analyses of",
        "\tsuboptimal modules with `--intermodule-analysis'",
        "\t(default: 0)."
        % This is commented out as this feature is still experimental.
%       "--analysis-file-cache",
%       "\tEnable caching of parsed analysis files. This may",
%       "\timprove compile times with `--intermodule-analysis'."
    ]).

:- pred options_help_hlds_hlds_optimization(io::di, io::uo) is det.

options_help_hlds_hlds_optimization -->
    io.write_string("\n    High-level (HLDS -> HLDS) optimizations:\n"),
    write_tabbed_lines([
        "--no-inlining",
        "\tDisable all forms of inlining.",
        "--no-inline-simple",
        "\tDisable the inlining of simple procedures.",
        "--no-inline-builtins",
        "\tGenerate builtins (e.g. arithmetic operators) as calls to",
        "\tout-of-line procedures. This is done by default when,",
        "\tdebugging, as without this option the execution of",
        "\tbuiltins is not traced.",
        "--no-inline-single-use",
        "\tDisable the inlining of procedures called only once.",
        "--inline-call-cost <cost>",
        "\tAssume that the cost of a call is the given parameter.",
        "\tUsed only in conjunction with `--inline-compound-threshold'.",
        "\tmultiplied by the number of times it is called,",
        "--inline-compound-threshold <threshold>",
        "\tInline a procedure if its size (measured roughly",
        "\tin terms of the number of connectives in its internal form)",
        "\tless the assumed call cost, multiplied by the number of times",
        "\tit is called is below the given threshold.",
        "--inline-simple-threshold <threshold>",
        "\tInline a procedure if its size is less than the",
        "\tgiven threshold.",
        "--intermod-inline-simple-threshold",
        "\tSimilar to `--inline-simple-threshold', except used to",
        "\tdetermine which predicates should be included in",
        "\t`.opt' files. Note that changing this between writing",
        "\tthe `.opt' file and compiling to C may cause link errors,",
        "\tand too high a value may result in reduced performance.",
        "--inline-vars-threshold <threshold>",
        "\tDon't inline a call if it would result in a procedure",
        "\tcontaining more than <threshold> variables. Procedures",
        "\tcontaining large numbers of variables can cause",
        "\tslow compilation.",
        "--inline-linear-tail-rec-sccs",
        "\tGiven a set of mutually recursive procedures (an SCC, or strongly",
        "\tconnected component, of the call graph) in which each procedure",
        "\tcontains exactly tail call to a procedure in the SCC, so that",
        "\tthe tail recursive calls form a linear chain through the SCC,",
        "\tinline the callee at every one of those mutually tail recursive",
        "\tcall sites. This converts mutual tail recursion into self tail",
        "\trecursion, which the MLDS backend can turn into code that runs",
        "\tin constant stack space.",
%       "--inline-linear-tail-rec-sccs-max-extra <E>",
%       "\tWhen considering whether to apply --inline-linear-tail-rec-sccs",
%       "\tto an SCC containing N procedures, allow the SCC to contain",
%       "\tup to N+E mutually recursive tail calls."
%       "--from-ground-term-threshold <n>",
%       "\tWrap a from_ground_term scope around the expanded,",
%       "\tsuperhomogeneous form of a ground term that involves at least.",
%       "\tthe given number of function symbols.",
        "--no-const-struct",
        "\tDisable the gathering of constant structures in a separate",
        "\ttable.",
        "--no-common-struct",
        "\tDisable optimization of common term structures.",
%       "--common-struct-preds <predids>",
%       "\tLimit common struct optimization to the preds with the given ids.",

%       Common goal optimization should not be turned off, since it can
%       break programs that would otherwise compile properly (e.g.,
%       benchmarks/icfp2000). This is kept as a developer-only option.
%
%       "--no-common-goal",
%       "\tDisable optimization of common goals.",
%       "\tAt the moment this optimization",
%       "\tdetects only common deconstruction unifications.",
%       "\tDisabling this optimization reduces the class of predicates",
%       "\tthat the compiler considers to be deterministic.",

        "--constraint-propagation",
        "\tEnable the constraint propagation transformation,",
        "\twhich attempts to transform the code so that goals",
        "\twhich can fail are executed as early as possible.",
        "--local-constraint-propagation",
        "\tEnable the constraint propagation transformation,",
        "\tbut only rearrange goals within each procedure.",
        "\tSpecialized versions of procedures will not be created.",
        "--no-follow-code",
        "\tDon't migrate into the end of branched goals.",
        "--excess-assign",
        "\tRemove excess assignment unifications.",
        % "--test-after-switch",
        % "\tOptimize away test unifications after switches whose arms",
        % "\tdo nothing except set the to-be-tested variable.",
        "--no-optimize-format-calls",
        "\tDo not attempt to interpret the format string in calls to",
        "\tstring.format and related predicates at compile time;",
        "\talways leave this to be done at runtime.",
        "--optimize-duplicate-calls",
        "\tOptimize away multiple calls to a predicate",
        "\twith the same input arguments.",
        "--loop-invariants",
        "\tHoist loop invariants out of loops.",
        "--delay-constructs",
        "\tReorder goals to move construction unifications after",
        "\tprimitive goals that can fail.",
        % "--optimize-saved-vars-const",
        % "\tMinimize the number of variables saved across calls by",
        % "\tintroducing duplicate copies of variables bound to",
        % "\tconstants in each interval between flushes where they",
        % "\tare needed.",
        % "--optimize-saved-vars-cell",
        % "\tMinimize the number of variables saved across calls by",
        % "\ttrying to use saved variables pointing to cells to reach",
        % "\tthe variables stored in those cells.",
        "--optimize-saved-vars",
        "\tMinimize the number of variables saved across calls.",
        "--optimize-unused-args",
        "\tRemove unused predicate arguments.",
        "\tThis will cause the compiler to generate more",
        "\tefficient code for many polymorphic predicates.",
        "--intermod-unused-args",
        "\tPerform unused argument removal across module boundaries.",
        "\tThis option implies `--optimize-unused-args' and",
        "\t`--intermodule-optimization'.",

        "--optimize-higher-order",
        "\tEnable specialization of higher-order predicates.",
        "--type-specialization",
        "\tEnable specialization of polymorphic predicates where the",
        "\tpolymorphic types are known.",
        "--user-guided-type-specialization",
        "\tEnable specialization of polymorphic predicates for which",
        "\tthere are `:- pragma type_spec' declarations.",
        "--higher-order-size-limit",
        "\tSet the maximum goal size of specialized versions created by",
        "\t`--optimize-higher-order' and `--type-specialization'.",
        "\tGoal size is measured as the number of calls, unifications",
        "\tand branched goals.",
        "--higher-order-arg-limit <limit>",
        "\tSet the maximum size of higher-order arguments to",
        "\tbe specialized by `--optimize-higher-order' and",
        "\t`--type-specialization'.",
        "--unneeded-code",
        "\tRemove goals from computation paths where their outputs are",
        "\tnot needed, provided the semantics options allow the deletion",
        "\tor movement of the goal.",
        "--unneeded-code-copy-limit",
        "\tGives the maximum number of places to which a goal may be copied",
        "\twhen removing it from computation paths on which its outputs are",
        "\tnot needed. A value of zero forbids goal movement and allows",
        "\tonly goal deletion; a value of one prevents any increase in the",
        "\tsize of the code.",
%       "--unneeded-code-debug",
%       "\tPrint progress messages during the unneeded code elimination",
%       "\tpasses.",
%       "--unneeded-code-debug-pred-name <predname>",
%       "\tPrint the definition of <predname> at the start of each pass",
%       "\tof the unneeded code elimination algorithm.",
        "--introduce-accumulators",
        "\tAttempt to introduce accumulating variables into",
        "\tprocedures, so as to make them tail recursive.",
%       "--optimize-constructor-last-call-accumulator",
%       "\tEnable the optimization via accumulators of ""last"" calls",
%       "\tthat are followed by constructor application.",
%       "--optimize-constructor-last-call-null",
%       "\tWhen --optimize-constructor-last-call is enabled, put NULL in"
%       "\tuninitialized fields (to prevent the garbage collector from",
%       "\tlooking at and following a random bit pattern).",
        "--optimize-constructor-last-call",
        "\tEnable the optimization of ""last"" calls that are followed by",
        "\tconstructor application.",
        "--deforestation",
        "\tEnable deforestation. Deforestation is a program",
        "\ttransformation whose aim is to avoid the construction of",
        "\tintermediate data structures and to avoid repeated traversals",
        "\tover data structures within a conjunction.",
        "--deforestation-depth-limit <limit>",
        "\tSpecify a depth limit to prevent infinite loops in the",
        "\tdeforestation algorithm.",
        "\tA value of -1 specifies no depth limit. The default is 4.",
        "--deforestation-vars-threshold <threshold>",
        "\tSpecify a rough limit on the number of variables",
        "\tin a procedure created by deforestation.",
        "\tA value of -1 specifies no limit. The default is 200.",
        "--deforestation-size-threshold <threshold>",
        "\tSpecify a rough limit on the size of a goal",
        "\tto be optimized by deforestation.",
        "\tA value of -1 specifies no limit. The default is 15.",
        "--analyse-exceptions",
        "\tEnable exception analysis. Identify those",
        "\tprocedures that will not throw an exception.",
        "\tSome optimizations can make use of this information.",
% XXX The options controlling closure analysis are currently
% commented out because it isn't useful. It can be uncommented when
% we actually have something that uses it.
%       "--analyse-closures",
%       "\tEnable closure analysis. Try to identify the possible",
%       "\tvalues that higher-order valued variables can take.",
%       "\tSome optimizations can make use of this information.",
        "--analyse-trail-usage",
        "\tEnable trail usage analysis. Identify those",
        "\tprocedures that will not modify the trail.",
        "\tThis information is used to reduce the overhead",
        "\tof trailing.",
% `--no-optimize-trail-usage' is a developer-only option.
% It is intended for the benchmarking of trail usage optimization.
% Otherwise, it should not be turned off as doing so interferes with
% the results of the trail usage analysis.
        % "--no-optimize-trail-usage",
        % "\tDo not try and restrict trailing to those parts",
        % "\tof the program that actually use it.",
% `--no-optimize-region-ops' is a developer-only option.
% It is intended for the benchmarking of region ops optimization.
        % "--no-optimize-region-ops",
        % "\tDo not try and restrict region operations to those parts",
        % "\tof the program that actually use it.",
        "--analyse-mm-tabling",
        "\tIdentify those goals that do not call procedures",
        "\tthat are evaluated using minimal model tabling.",
        "\tThis information is used to reduce the overhead",
        "\tof minimal model tabling."
        % "--untuple",
        % "\tExpand out procedure arguments when the argument type",
        % "\tis a tuple or a type with exactly one functor.",
        % "\tNote: this is almost always a pessimization.",
        % "--tuple",
        % "\tTry to find opportunities for procedures to pass some",
        % "\targuments to each other as a tuple rather than as",
        % "\tindividual arguments.",
        % "\tNote: so far this has mostly a detrimental effect.",
        % "--tuple-trace-counts-file",
        % "\tSupply a trace counts summary file for the tupling",
        % "\ttransformation. The summary should be made from a sample",
        % "\trun of the program you are compiling, compiled without",
        % "\toptimizations.",
        % "--tuple-costs-ratio",
        % "\tA value of 110 for this parameter means the tupling",
        % "\ttransformation will transform a procedure if it thinks",
        % "\tthat procedure would be 10% worse, on average, than",
        % "\twhatever transformed version of the procedure it has in",
        % "\tmind. The default is 100.",
        % "--tuple-min-args",
        % "\tThe minimum number of input arguments that the tupling",
        % "\ttransformation will consider passing together as a",
        % "\ttuple. This is mostly to speed up the compilation",
        % "\tprocess by not pursuing (presumably) unfruitful searches.",
% This is for measurements by implementors only.
%       "--no-inline-par-builtins",
%       "\tGenerate calls to the predicates of par_builtin.m, instead of",
%       "\tbodily including their definitions as C code.",
%       Actually, this is the default for now.
% This is for measurements by implementors only.
%       "--always-specialize-in-dep-par-conjs",
%       "\tWhen the transformation for handling dependent parallel conjunctions",
%       "\tadds waits and/or signals around a call, create a specialized",
%       "\tversion of the called procedure, even if this is not profitable.",
% '--region-analysis' is not documented because it is still experimental.
%        "--region-analysis",
%        "\tEnable the analysis for region-based memory management."
    ]).

    % XXX This is out-of-date. --smart-indexing also affects the
    % MLDS backend.
    %
:- pred options_help_hlds_llds_optimization(io::di, io::uo) is det.

options_help_hlds_llds_optimization -->
    io.write_string("\n    Medium-level (HLDS -> LLDS) optimizations:\n"),
    write_tabbed_lines([
        "--no-smart-indexing",
        "\tGenerate switches as simple if-then-else chains;",
        "\tdisable string hashing and integer table-lookup indexing.",
% The following options are for developers only --they provide finer grained
% control over smart indexing.
%       "--no-smart-atomic-indexing",
%       "\tDo not generate smart switches on atomic types.",
%       "--no-smart-string-indexing",
%       "\tDo not generate smart switches on strings."
%       "--no-smart-tag-indexing",
%       "\tDo not generate smart switches on discriminated union types.",
%       "---no-smart-float-indexing",
%       "\tDo not generate smart switches on floats."
        "--dense-switch-req-density <percentage>",
        "\tThe jump table generated for an atomic switch",
        "\tmust have at least this percentage of full slots (default: 25).",
        "--lookup-switch-req-density <percentage>",
        "\tThe jump table generated for an atomic switch",
        "\tin which all the outputs are constant terms",
        "\tmust have at least this percentage of full slots (default: 25).",
        "--dense-switch-size <n>",
        "\tThe jump table generated for an atomic switch",
        "\tmust have at least this many entries (default: 4).",
        "--lookup-switch-size <n>",
        "\tThe lookup table generated for an atomic switch",
        "\tmust have at least this many entries (default: 4).",
        "--string-trie-switch-size <n>",
        "\tThe trie generated for a string switch",
        "\tmust have at least this many entries (default: 16).",
        "--string-hash-switch-size <n>",
        "\tThe hash table generated for a string switch",
        "\tmust have at least this many entries (default: 8).",
        "--string-binary-switch-size <n>",
        "\tThe binary search table generated for a string switch",
        "\tmust have at least this many entries (default: 4).",
        "--tag-switch-size <n>",
        "\tThe number of alternatives in a tag switch",
        "\tmust be at least this number (default: 3).",
        "--try-switch-size <n>",
        "\tThe number of alternatives in a try/retry chain switch",
        "\tmust be at least this number (default: 3).",
        "--binary-switch-size <n>",
        "\tThe number of alternatives in a binary search switch",
        "\tmust be at least this number (default: 4).",
% These options are only for performance tests.
%       "--switch-single-rec-base-first",
%       "\tIn a switch with two arms, one a base case and one with a single",
%       "\trecursive call, put the base case first.
%       "--switch-multi-rec-base-first",
%       "\tIn a switch with two arms, one a base case and one with multiple",
%       "\trecursive calls, put the base case first.
        "--no-static-ground-terms",
        "\tDisable the optimization of constructing constant ground terms",
        "\tat compile time and storing them as static constants.",
        "\tNote that auxiliary data structures created by the compiler",
        "\tfor purposes such as debugging will still be created as",
        "\tstatic constants.",
        "--no-use-atomic-cells",
        "\tDon't use the atomic variants of the Boehm gc allocator calls,",
        "\teven when this would otherwise be possible.",
        "--no-middle-rec",
        "\tDisable the middle recursion optimization.",
        "--no-simple-neg",
        "\tDon't generate simplified code for simple negations."
%       "--no-allow-hijacks",
%       "\tDo not generate code in which a procedure hijacks",
%       "\ta nondet stack frame that possibly belongs to",
%       "\tanother procedure invocation".
    ]).

:- pred options_help_llds_llds_optimization(io::di, io::uo) is det.

options_help_llds_llds_optimization -->
    io.write_string("\n    Low-level (LLDS -> LLDS) optimizations:\n"),
    write_tabbed_lines([
        "--no-common-data",
        "\tDisable optimization of common data structures.",
        "--no-common-layout-data",
        "\tDisable optimization of common subsequences in layout",
        "\tstructures.",
        "--no-llds-optimize",
        "\tDisable the low-level optimization passes.",
        "--optimize-dead-procs",
        "\tEnable dead predicate elimination.",
        "--no-optimize-peep",
        "\tDisable local peephole optimizations.",
% This is useful for developers only, to test whether a gcc bug has been fixed.
%       "--no-optimize-peep-mkword",
%       "\tDisable peephole optimizations of words created by mkword.",
        "--no-optimize-jumps",
        "\tDisable elimination of jumps to jumps.",
        "--no-optimize-fulljumps",
        "\tDisable elimination of jumps to ordinary code.",
        "--pessimize-tailcalls",
        "\tDisable the optimization of tailcalls.",
        "--checked-nondet-tailcalls",
        "\tConvert nondet calls into tail calls whenever possible, even",
        "\twhen this requires a runtime check. This option tries to",
        "\tminimize stack consumption, possibly at the expense of speed.",
        "--no-use-local-vars",
        "\tDisable the transformation to use local variables in C code",
        "\tblocks wherever possible.",
% This is useful for developers only.
%       "--standardize-labels",
%       "\tStandardize internal labels in the generated code.",
        "--no-optimize-labels",
        "\tDisable elimination of dead labels and code.",
        "--optimize-dups",
        "\tEnable elimination of duplicate code within procedures.",
        "--optimize-proc-dups",
        "\tEnable elimination of duplicate procedures.",
%%%     "--optimize-copyprop",
%%%     "\tEnable the copy propagation optimization.",
        "--no-optimize-frames",
        "\tDisable stack frame optimizations.",
        "--no-optimize-delay-slot",
        "\tDisable branch delay slot optimizations.",
        "--optimize-reassign",
        "\tOptimize away assignments to locations that already hold",
        "\tthe assigned value.",
        "--optimize-repeat <n>",
        "\tIterate most optimizations at most <n> times (default: 3).",
        "--layout-compression-limit <n>",
        "\tAttempt to compress the layout structures used by the debugger",
        "\tonly as long as the arrays involved have at most <n> elements",
        "\t(default: 4000)."
    ]).

:- pred options_help_mlds_mlds_optimization(io::di, io::uo) is det.

options_help_mlds_mlds_optimization -->
    io.write_string("\n    MLDS -> MLDS optimizations:\n"),
    write_tabbed_lines([
        "--no-mlds-optimize",
        "\tDisable the MLDS->MLDS optimization passes.",
        "--no-mlds-peephole",
        "\tDo not perform peephole optimization of the MLDS.",
        "--no-optimize-tailcalls",
        "\tTreat tailcalls as ordinary calls, rather than optimizing",
        "\tby turning self-tailcalls into loops.",
        "--no-optimize-initializations",
        "\tLeave initializations of local variables as",
        "\tassignment statements, rather than converting such",
        "\tassignment statements into initializers.",
% This is useful for developers only.
%       "--eliminate-unused-mlds-assigns",
%       "\tEliminate assignments to dead variables in the MLDS.",
        "--eliminate-local-vars",
        "\tEliminate local variables with known values, where possible,",
        "\tby replacing occurrences of such variables with their values.",
        "--no-generate-trail-ops-inline",
        "\tDo not generate trailing operations inline,",
        "\tbut instead insert calls to the versions of these operations",
        "\tin the standard library."
]).

:- pred options_help_hlds_elds_optimization(io::di, io::uo) is det.

options_help_hlds_elds_optimization -->
    io.write_string("\n    HLDS -> ELDS optimizations:\n"),
    write_tabbed_lines([
        "--erlang-switch-on-strings-as-atoms",
        "\tEnable a workaround for slow HiPE compilation of large string",
        "\tswitches by converting the string to an atom at runtime and",
        "\tswitching on atoms. Do not enable if the string switched on",
        "\tcould be longer than 255 characters at runtime."
    ]).

:- pred options_help_output_optimization(io::di, io::uo) is det.

options_help_output_optimization -->
    io.write_string("\n    Output-level (LLDS -> C) optimizations:\n"),
    write_tabbed_lines([
        "--use-macro-for-redo-fail",
        "\tEmit the fail or redo macro instead of a branch",
        "\tto the fail or redo code in the runtime system.",
        "\tThis produces slightly bigger but slightly faster code.",
        "--no-emit-c-loops",
        "\tUse only gotos, don't emit C loop constructs.",
        "--procs-per-c-function <n>",
        "\tPut the code for up to <n> Mercury",
        "\tprocedures in a single C function. The default",
        "\tvalue of <n> is one. Increasing <n> can produce",
        "\tslightly more efficient code, but makes compilation slower.",
        "--everything-in-one-c-function",
        "\tThis option has the effect of putting the code for all",
        "\tthe Mercury procedures in a single C function,",
        "\twhich produces the most efficient code but tends to",
        "\tseverely stress the C compiler on large modules.",
        "--no-local-thread-engine-base",
        "\tDo not copy the thread-local Mercury engine base address",
        "\tinto local variables. This option only affects low-level",
        "\tparallel grades not using the GNU C global register variables",
        "\textension."
    ]).

:- pred options_help_target_code_compilation(io::di, io::uo) is det.

options_help_target_code_compilation -->
    io.write_string("\n    Target code compilation:\n"),
    write_tabbed_lines([
        "\tNote that if you are using Mmake, you need to pass these",
        "\toptions to the target code compiler (e.g. `mgnuc')",
        "\trather than to `mmc'.",

        "--target-debug",
        "\tEnable debugging of the generated target code.",
        "\tIf the target language is C, this has the same effect as",
        "\t`--c-debug' (see below).",
        "\tIf the target language is C#, this causes the compiler to",
        "\tpass `/debug' to the C# compiler.)",

        "--cc <compiler-name>",
        "\tSpecify which C compiler to use.",

        "--no-c-optimize",
        "\tDon't enable the C compiler's optimizations.",

        "--no-ansi-c",
        "\tDon't specify to the C compiler that the ANSI dialect",
        "\tof C should be used. Use the full contents of system",
        "\theaders, rather than the ANSI subset.",

        "--c-debug",
        "\tEnable debugging of the generated C code.",
        "\t(This has the same effect as `--cflags ""-g""'",
        "\tand disables stripping of the executable.)",

        "--c-include-directory <dir>, --c-include-dir <dir>",
        "\tAppend <dir> to the list of directories to be searched for",
        "\tC header files. Note that if you want to override",
        "\tthis list, rather than append to it, then you can set the",
        "\t`MERCURY_MC_ALL_C_INCL_DIRS' environment variable to a",
        "\tsequence of `--c-include-directory' options.",

        "--inline-alloc",
        "\tInline calls to GC_malloc().",
        "\tThis can improve performance a fair bit,",
        "\tbut may significantly increase code size.",
        "\tThis option has no effect if `--gc boehm'",
        "\tis not set or if the C compiler is not GNU C.",

        "--cflags <options>, --cflag <option>",
        "\tSpecify options to be passed to the C compiler.",
        "\t`--cflag' should be used for single words which need",
        "\tto be quoted when passed to the shell.",

        % The --gcc-flags, --gcc-flag, --clang-flags, --clang-flag,
        % --msvc-flags and --msvc-flag options are an internal
        % part of the implementation of mmc --make; they are deliberately
        % not documented.

        % The --cflags-for-regs, --cflags-for-gotos,
        % --cflags-for-threads, --cflags-for-pic,
        % --cflags-for-warnings, --cflags-for-ansi,
        % --cflags-for-optimization, --cflags-for-sanitizers,
        % --c-flag-to-name-object-file,
        % --object-file-extension and --pic-object-file-extension
        % options are reserved for use by the `mmc' script;
        % they are deliberately not documented.

        "--javac <javac>",
        "--java-compiler <javac>",
        "\tSpecify which Java compiler to use. The default is `javac'.",

        "--java-interpreter <java>",
        "\tSpecify which Java interpreter to use.",
        "\tThe default is `java'",

        "--java-flags <options>, --java-flag <option>",
        "\tSpecify options to be passed to the Java compiler.",
        "\t`--java-flag' should be used for single words which need",
        "\tto be quoted when passed to the shell.",

        "--java-classpath <path>",
        "\tSet the classpath for the Java compiler.",

        "--java-object-file-extension <ext>",
        "\tSpecify an extension for Java object (bytecode) files",
        "\tBy default this is `.class'.",

        "--csharp-compiler <csc>",
        "\tSpecify the name of the C# Compiler. The default is `csc'.",
        "--csharp-flags <options>, --csharp-flag <option>",
        "\tSpecify options to be passed to the C# compiler.",
        "\t`--csharp-flag' should be used for single words which need",
        "\tto be quoted when passed to the shell.",
        "--cli-interpreter <prog>",
        "\tSpecify the program that implements the Common Language",
        "\tInfrastructure (CLI) execution environment, e.g. `mono'.",

        "--erlang-compiler <erlc>",
        "\tSpecify the name of the Erlang compiler.",
        "\tThe default is `erlc'.",
        "--erlang-interpreter <erl>",
        "\tSpecify the name of the Erlang interpreter.",
        "\tThe default is `erl'.",
        "--erlang-flags <options>, --erlang-flag <option>",
        "\tSpecify options to be passed to the Erlang compiler.",
        "\t`--erlang-flag' should be used for single words which need",
        "\tto be quoted when passed to the shell.",
        "--erlang-include-directory <dir>, --erlang-include-dir <dir>",
        "\tAppend <dir> to the list of directories to be searched for",
        "\tErlang header files (.hrl).",
        "--erlang-native-code",
        "\tAdd `+native' to the list of flags passed to the",
        "\tErlang compiler. Cancelled out by `--no-erlang-native code'",
        "\tso it is useful when you wish to enable native code",
        "\tgeneration for all modules except for a select few.",
        "--no-erlang-inhibit-trivial-warnings",
        "\tDo not add `+nowarn_unused_vars +nowarn_unused_function' to the",
        "\tlist of flags passed to the Erlang compiler."

        % --erlang-object-file-extension is deliberately not documented.
        % It is not fully implemented and not very useful.
    ]).

:- pred options_help_link(io::di, io::uo) is det.

options_help_link -->
    io.write_string("\nLink Options:\n"),
    write_tabbed_lines([
        "-o <filename>, --output-file <filename>",
        "\tSpecify the name of the final executable.",
        "\t(The default executable name is the same as the name",
        "\tof the first module on the command line.)",
        "\tThis option is ignored by `mmc --make'.",
        "--ld-flags <options>, --ld-flag <option>",
        "\tSpecify options to be passed to the linker command",
        "\tinvoked by ml to link an executable.",
        "\tUse `ml --print-link-command' to find out which",
        "\tcommand is used.",
        "\t`--ld-flag' should be used for single words which need",
        "\tto be quoted when passed to the shell.",
        "--ld-libflags <options>, --ld-libflag <option>",
        "\tSpecify options to be passed to the linker command",
        "\tinvoked by ml to link a shared library.",
        "\tUse `ml --print-shared-lib-link-command' to find out",
        "\twhich command is used.",
        "\t`--ld-libflag' should be used for single words which need",
        "\tto be quoted when passed to the shell.",
        "-L <directory>, --library-directory <directory>",
        "\tAppend <directory> to the list of directories in which",
        "\tto search for libraries.",
        "-R <directory>, --runtime-library-directory <directory>",
        "\tAppend <directory> to the list of directories in which",
        "\tto search for shared libraries at runtime.",
        "--no-default-runtime-library-directory",
        "\tDo not add any directories to the runtime search path",
        "\tautomatically.",
        "--shlib-linker-install-name-path <directory>",
        "\tSpecify the path where a shared library will be installed.",
        "\tThis option is useful on systems where the runtime search",
        "\tpath is obtained from the shared library and not via the",
        "\t-R option above (such as Mac OS X).",
        "-l <library>, --library <library>",
        "\tLink with the specified library.",
        "--link-object <file>",
        "\tLink with the specified object or archive file.",
        "--search-lib-files-dir <directory>",
        "--search-library-files-directory <directory>",
        "\tSearch <directory> for Mercury library files that have not yet",
        "\tbeen installed. Similar to adding <directory> using all of the",
        "\t`--search-directory', `--intermod-directory',",
        "\t`--library-directory', `--init-file-directory' and",
        "\t`--c-include-directory' options.",
        "--mld <directory>, --mercury-library-directory <directory>",
        "\tAppend <directory> to the list of directories to",
        "\tbe searched for Mercury libraries. This will add",
        "\t`--search-directory', `--library-directory',",
        "\t`--init-file-directory' and `--c-include-directory'",
        "\toptions as needed.",
        "--mercury-standard-library-directory <directory>",
        "--mercury-stdlib-dir <directory>",
        "\tSearch <directory> for the Mercury standard library.",
        "\tImplies `--mercury-library-directory <directory>'",
        "\tand `--mercury-configuration-directory <directory>'.",
        "--no-mercury-standard-library-directory",
        "--no-mercury-stdlib-dir",
        "\tDon't use the Mercury standard library.",
        "\tImplies `--no-mercury-configuration-directory'.",
        "--ml <library>, --mercury-library <library>",
        "\tLink with the specified Mercury library.",

        "--linkage {shared, static}",
        "\tSpecify whether to use shared or static linking for",
        "\texecutables. Shared libraries are always linked",
        "\twith `--linkage shared'.",
        "--mercury-linkage {shared, static}",
        "\tSpecify whether to use shared or static linking when",
        "\tlinking an executable with Mercury libraries.",
        "\tShared libraries are always linked with",
        "\t`--mercury-linkage shared'.",

        "--init-file-directory <directory>",
        "\tAppend <directory> to the list of directories to",
        "\tbe searched for `.init' files by c2init.",
        "--init-file <init-file>",
        "\tAppend <init-file> to the list of `.init' files to",
        "\tbe passed to c2init.",
        "--trace-init-file <init-file>",
        "\tAppend <init-file> to the list of `.init' files to",
        "\tbe passed to c2init when tracing is enabled.",

        "--no-demangle",
        "\tDon't pipe link errors through the Mercury demangler.",
        "--no-main",
        "\tDon't generate a C main() function. The user's code must",
        "\tprovide a main() function.",
        "--no-allow-undefined",
        "\tDo not allow undefined symbols in shared libraries.",
        "--no-use-readline",
        "\tDisable use of the readline library in the debugger.",
        "--runtime-flags <flags>",
        "\tSpecify flags to pass to the Mercury runtime.",
        "--extra-initialization-functions, --extra-inits",
        "\tSearch `.c' files for extra initialization functions.",
        "\t(This may be necessary if the C files contain",
        "\thand-coded C code with `INIT' comments, rather than",
        "\tcontaining only C code that was automatically generated",
        "\tby the Mercury compiler.)",

        "--link-executable-command <command>",
        "\tSpecify the command used to invoke the linker when linking",
        "\tan executable.",
        "--link-shared-lib-command <command>",
        "\tSpecify the command used to invoke the linker when linking",
        "\ta shared library.",

        "--no-strip",
        "\tDo not strip executables.",
        "--strip-executable-command <command>",
        "\tSpecify the command used to strip executables if no linker",
        "\tflag to do so is available. This option has no effect on ml.",
        "--strip-executable-shared-flags <options>",
        "\tSpecify options to pass to the strip executable command when",
        "\tlinking against Mercury shared libraries.",
        "--strip-executable-static-flags <options>",
        "\tSpecify options to pass to the strip executable command when",
        "\tlinking against Mercury static libraries.",

        "--java-archive-command <command>",
        "\tSpecify the command used to produce Java archive (JAR) files.",

        "--framework <framework>",
        "\tBuild and link against the specified framework.",
        "\t(Mac OS X only.)",
        "-F <directory>, --framework-directory <directory>",
        "\tAppend the specified directory to the framework search path.",
        "\t(Mac OS X only.)",

        "--sign-assembly <keyfile>",
        "\tSign the current assembly with the strong name contained",
        "\tin the specified key file.",
        "\t(This option is only meaningful when generating library",
        "\tassemblies with the C# back-end.)",

        "--cstack-reserve-size <size>",
        "\tSet the total size of the C stack in virtual memory for",
        "\texecutables. The stack size is given in bytes.",
        "\t(Microsoft Windows only.)"

        % The --shared-library-extension,
        % --library-extension, --executable-file-extension
        % --create-archive-command, --create-archive-command-flags
        % --create-archive-command-output-flag, --ranlib-command,
        % --ranlib-flags,
        % --mkinit-command, --demangle-command, --filtercc-command,
        % --filterjavac-command --trace-libs,
        % --thread-libs, --shared-libs, --math-lib, --readline-libs,
        % --hwloc-libs, --hwloc-static-libs,
        % --linker-opt-separator,
        % --linker-debug-flags, --shlib-linker-debug-flags,
        % --linker-sanitizer-flags,
        % --linker-trace-flags, --shlib-linker-trace-flags,
        % --linker-thread-flags, --shlib-linker-thread-flags,
        % --linker-static-flags, --linker-strip-flag,
        % --linker-link-lib-flag, --linker-link-lib-suffix,
        % --shlib-linker-link-lib-flag, --shlib-linker-link-lib-suffix,
        % --linker-path-flag, --linker-link-with-lib-flag,
        % --linker-rpath-flag, --linker-rpath-separator,
        % --shlib-linker-link-with-lib-flag,
        % --shlib-linker-rpath-flag, --shlib-linker-rpath-separator,
        % --linker-allow-undefined-flag and
        % --linker-error-undefined-flag,
        % --shlib-linker-install-name-flag,
        % --shlib-linker-use-install-name,
        % options are reserved for use by the `Mercury.config' file;
        % they are deliberately not documented.
    ]).

:- pred options_help_build_system(io::di, io::uo) is det.

options_help_build_system -->
    io.write_string("\nBuild System Options:\n"),
    write_tabbed_lines([
        % `--invoked-by-mmc-make' is for internal use by the
        % compiler. `mmc --make' passes it as the first argument
        % when compiling a module.
        "-m, --make",
        "\tTreat the non-option arguments to `mmc' as files to make,",
        "\trather than source files. Create the specified files,",
        "\tif they are not already up-to-date.",
        "\tNote that this option also enables `--use-subdirs'.",
        "-r, --rebuild",
        "\tSame as `--make', but always rebuild the target files",
        "\teven if they are up-to-date.",
        "-k, --keep-going",
        "\tWith `--make', keep going as far as possible",
        "\teven if an error is detected.",
        "-j <n>, --jobs <n>",
        "\tWith `--make', attempt to perform up to <n> jobs",
        "\tconcurrently for some tasks.",

        "--track-flags",
        "\tWith `--make', keep track of the options used when compiling",
        "\teach module. If an option for a module is added or removed,",
        "\t`mmc --make' will then know to recompile the module even if the",
        "\ttimestamp on the file itself has not changed. Warning,",
        "\tverbosity and build system options are not tracked.",

        "--pre-link-command <command>",
        "\tSpecify a command to run before linking with `mmc --make'.",
        "\tThis can be used to compile C source files which rely on",
        "\theader files generated by the Mercury compiler.",
        "\tThe command will be passed the names of all of the source",
        "\tfiles in the program or library, with the source file",
        "\tcontaining the main module given first.",

        "--extra-init-command <command>",
        "\tSpecify a command to produce extra entries in the `.init'",
        "\tfile for a library.",
        "\tThe command will be passed the names of all of the source",
        "\tfiles in the program or library, with the source file",
        "\tcontaining the main module given first.",

        "--install-prefix <dir>",
        "\tThe directory under which to install Mercury libraries.",

        % --use-symlinks is only used by Mercury.config.
        % It controls whether the build system should attempt
        % to use symlinks.

        "--install-command <command>",
        "\tSpecify the command to use to install the files in",
        "\tMercury libraries. The given command will be invoked as",
        "\t`<command> <source> <target>' to install each file",
        "\tin a Mercury library. The default command is `cp'.",
        "--install-command-dir-option <option>",
        "\tSpecify the flag to pass to the install command to install",
        "\ta directory. The given command will be invoked as",
        "\t`<command> <option> <source> <target>'",
        "\tto install each directory. The default option is `-R'.",
        "--no-detect-libgrades",
        "\tDo not scan the installation directory to determine which",
        "\tstandard library grades are available.",
        "--libgrade <grade>",
        "\tAdd <grade> to the list of compilation grades in",
        "\twhich a library to be installed should be built.",
        "--no-libgrade",
        "\tClear the list of compilation grades in which a library",
        "\tto be installed should be built.",
        "--libgrades-include-component <component>",
        "--libgrades-include <component>",
        "\tRemove grades that do not contain the specified component from",
        "\tthe set of library grades to be installed.",
        "\t(This option does not work with Mmake, only `mmc --make'.)",
        "--libgrades-exclude-component <component>",
        "--libgrades-exclude <component>",
        "\tRemove grades that contain the specified component from the",
        "\tset of library grades to be installed.",
        "\t(This option does not work with Mmake, only `mmc --make'.)",
        "--lib-linkage {shared, static}",
        "\tSpecify whether libraries should be installed for shared",
        "\tor static linking. This option can be specified multiple",
        "\ttimes. By default libraries will be installed for",
        "\tboth shared and static linking.",
        "--flags <file>",
        "--flags-file <file>",
        "\tTake options from the specified file, and handle them",
        "\tas if they were specified on the command line.",
        "--options-file <file>",
        "\tAdd <file> to the list of options files to be processed.",
        "\tIf <file> is `-', an options file will be read from the",
        "\tstandard input. By default the file `Mercury.options'",
        "\tin the current directory will be read.",
        "--options-search-directory <dir>",
        "\tAdd <dir> to the list of directories to be searched for",
        "\toptions files.",
        "--mercury-configuration-directory <directory>",
        "--mercury-config-dir <directory>",
        "\tSearch <directory> for Mercury system's configuration files.",
        "--config-file <file>",
        "\tRead the Mercury compiler's configuration information",
        "\tfrom <file>. If the `--config-file' option is not set,",
        "\ta default configuration will be used, unless",
        "\t`--no-mercury-stdlib-dir' is passed to mmc.",
        "\tThe configuration file is just an options file.",
        "-I <dir>, --search-directory <dir>",
        "\tAppend <dir> to the list of directories to be searched for",
        "\timported modules.",
        "--intermod-directory <dir>",
        "\tAdd <dir> to the list of directories to be",
        "\tsearched for `.opt' files.",
        "--no-use-search-directories-for-intermod",
        "\tDon't add arguments to `--search-directory' to the list",
        "\tof directories to search for `.opt' files - use only the",
        "\tdirectories given by `--intermod-directory'.",
        "--use-subdirs",
        "\tGenerate intermediate files in a `Mercury' subdirectory,",
        "\trather than generating them in the current directory.",
        "--use-grade-subdirs",
        "\tGenerate intermediate files in a `Mercury' subdirectory,",
        "\tlaid out so that multiple grades can be built simultaneously.",
        "\tExecutables and libraries will be symlinked or copied into",
        "\tthe current directory.",
        "\t`--use-grade-subdirs' does not work with Mmake (it does",
        "\twork with `mmc --make').",
        "--no-libgrade-install-check",
        "\tDo not check that libraries have been installed before",
        "\tattempting to use them. (This option is only meaningful with",
        "\t`mmc --make'.)",
        "--order-make-by-timestamp",
        "\tMake `mmc --make' compile more recently modified source files",
        "\tfirst.",
        "--show-make-times",
        "\tReport run times for commands executed by `mmc --make'.",
        "--extra-library-header <file>, --extra-lib-header <file>",
        "\tInstall the specified C header file with along with",
        "\ta Mercury library.",
        "\t(This option is only supported by `mmc --make'.)",
        "--restricted-command-line",
        "\tEnable this option if your shell doesn't support long command lines.",
        "\tThis option uses temporary files to pass arguments to sub-commands.",
        "\t(This option is only supported by `mmc --make'.)",
        "--env-type <type>",
        "\tSpecify the environment type in which the compiler and generated",
        "\tprograms will be invoked.",
        "\tThe <type> should be one of `posix', `cygwin', `msys', or",
        "\t`windows'.",
        "\tThis option is equivalent to setting all of `--host-env-type',",
        "\t`--system-env-type' and `--target-env-type' to <type>.",
        "--host-env-type <type>",
        "\tSpecify the environment type in which the compiler will be",
        "\tinvoked.",
        "--system-env-type <type>",
        "\tSpecify the environment type in which external programs invoked",
        "\tby the compiler will run.",
        "\tIf not specified, this defaults to the value given by",
        "\t`--host-env-type'.",
        "--target-env-type <type>",
        "\tSpecify the environment type in which generated programs will be",
        "\tinvoked."
    ]).

:- pred options_help_misc(io::di, io::uo) is det.

options_help_misc -->
    io.write_string("\nMiscellaneous Options:\n"),
    write_tabbed_lines([
        "--filenames-from-stdin",
        "\tRead then compile a newline terminated module name or",
        "\tfile name from the standard input. Repeat this until EOF",
        "\tis reached. (This allows a program or user to interactively",
        "\tcompile several modules without the overhead of process",
        "\tcreation for each one.)",
        "--typecheck-ambiguity-warn-limit <n>",
        "\tSet the number of type assignments required to generate a",
        "\twarning about highly ambiguous overloading to <n>.",
        "--typecheck-ambiguity-error-limit <n>",
        "\tSet the number of type assignments required to generate an error",
        "\tabout excessively ambiguous overloading to <n>. If this limit is",
        "\treached, the typechecker will not process the predicate or",
        "\tfunction any further.",
        "--version",
        "\tDisplay the compiler version.",

        % The `--target-arch' options is reserved for use by the
        % `Mercury.config' file. The `--fullarch' option is a deprecated
        % synonym for this.

% This option has no effect now.
%       "--cross-compiling",
%       "\tDo not assume that the code being generated is for the",
%       "\tplatform the compiler is running on.",

        % The `--local-module-id' option is used by `mmc --make'.
        % The `--analysis-file-cache-dir' option is used by `mmc --make'.

%       "--ignore-parallel-conjunctions",
%       "\tReplace parallel conjunctions with plain ones, this is useful",
%       "\tfor benchmarking. Note that it does not affect implicit",
%       "\tparallelism",

        "--control-granularity",
        "\tDon't try to generate more parallelism than the machine can",
        "\thandle, which may be specified at runtime or detected",
        "\tautomatically.",
        "--distance-granularity <distance>",
        "\tControl the granularity of parallel execution using the",
        "\tspecified distance value.",
        "--implicit-parallelism",
        "\tIntroduce parallel conjunctions where it could be worthwhile",
        "\t(implicit parallelism) using information generated by",
        "\tmdprof_create_feedback.",
        "\tThe profiling feedback file can be specified using the",
        "\t`--feedback-file' option.",

        "--feedback-file <file>",
        "\tUse the specified profiling feedback file which may currently",
        "\tonly be processed for implicit parallelism."
    ]).

:- pred write_tabbed_lines(list(string)::in, io::di, io::uo) is det.

write_tabbed_lines([], !IO).
write_tabbed_lines([Str | Strs], !IO) :-
    io.write_char('\t', !IO),
    io.write_string(Str, !IO),
    io.write_char('\n', !IO),
    write_tabbed_lines(Strs, !IO).

%---------------------------------------------------------------------------%
:- end_module libs.options.
%---------------------------------------------------------------------------%
