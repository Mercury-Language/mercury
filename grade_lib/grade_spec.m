%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_spec.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

:- type solver_var_id
    --->    svar_ac_gcc_regs_avail
    ;       svar_ac_gcc_gotos_avail
    ;       svar_ac_gcc_labels_avail
    ;       svar_ac_low_tag_bits_avail
    ;       svar_ac_size_of_double
    ;       svar_ac_merc_file

    ;       svar_backend
    ;       svar_datarep
    ;       svar_target
    ;       svar_nested_funcs
    ;       svar_gcc_regs_use
    ;       svar_gcc_gotos_use
    ;       svar_gcc_labels_use
    ;       svar_low_tag_bits_use
    ;       svar_stack_len
    ;       svar_trail
    ;       svar_trail_segments
    ;       svar_minmodel
    ;       svar_thread_safe
    ;       svar_gc
    ;       svar_deep_prof
    ;       svar_mprof_call
    ;       svar_mprof_time
    ;       svar_mprof_memory
    ;       svar_tscope_prof
            % Paul wants to call the threadscope style profiler a different
            % name, since the data it generates isn't compatible with
            % threadscope anymore. But the name we use here in the solver
            % isn't visible outside, and "tscope" gives readers the right
            % intuition.
    ;       svar_term_size_prof
    ;       svar_debug
    ;       svar_ssdebug
    ;       svar_lldebug
    ;       svar_rbmm
    ;       svar_rbmm_debug
    ;       svar_rbmm_prof
    ;       svar_pregen
    ;       svar_request_single_prec_float
    ;       svar_merc_float.

:- type solver_var_value_id
    --->    svalue_ac_gcc_regs_avail_no
    ;       svalue_ac_gcc_regs_avail_yes

    ;       svalue_ac_gcc_gotos_avail_no
    ;       svalue_ac_gcc_gotos_avail_yes

    ;       svalue_ac_gcc_labels_avail_no
    ;       svalue_ac_gcc_labels_avail_yes

    ;       svalue_ac_low_tag_bits_avail_0
    ;       svalue_ac_low_tag_bits_avail_2
    ;       svalue_ac_low_tag_bits_avail_3
            % Autoconf can detect that 4 low tag bits are available.
            % However, since we never use 4 low tag bits, this is functionally
            % indistinguishable from 3 low tag bits being available,
            % so we require the code setting up the grade settings to be solved
            % to map 4 avail tag bits to 3.

    ;       svalue_ac_size_of_double_eq_ptr
    ;       svalue_ac_size_of_double_ne_ptr

    ;       svalue_ac_merc_file_no
    ;       svalue_ac_merc_file_yes

    ;       svalue_backend_mlds
    ;       svalue_backend_llds
    ;       svalue_backend_elds

    ;       svalue_datarep_heap_cells
    ;       svalue_datarep_classes
    ;       svalue_datarep_erlang

    ;       svalue_target_c
    ;       svalue_target_csharp
    ;       svalue_target_java
    ;       svalue_target_erlang

    ;       svalue_nested_funcs_no
    ;       svalue_nested_funcs_yes

    ;       svalue_gcc_regs_use_no
    ;       svalue_gcc_regs_use_yes

    ;       svalue_gcc_gotos_use_no
    ;       svalue_gcc_gotos_use_yes

    ;       svalue_gcc_labels_use_no
    ;       svalue_gcc_labels_use_yes

    ;       svalue_low_tag_bits_use_0
            % If we are using 0 low primary tag bits, then the data
            % representation may use no primary tag at all (which is what
            % svalue_low_tag_bits_use_0 is intended to mean here), or
            % it may use (an almost arbitrary number of) high primary tag bits.
            % Since we haven't used high ptag bits in ages, I (zs) see no need
            % to handle them here. If that changes, we would need a new solver
            % variable named (say) svar_high_tag_bits_use, whose value
            % being any nonzero value would imply svalue_low_tag_bits_use_0.
    ;       svalue_low_tag_bits_use_2
    ;       svalue_low_tag_bits_use_3

    ;       svalue_stack_len_std
    ;       svalue_stack_len_segments
    ;       svalue_stack_len_extend

    ;       svalue_trail_no
    ;       svalue_trail_yes

    ;       svalue_trail_segments_no
    ;       svalue_trail_segments_yes

    ;       svalue_minmodel_no
    ;       svalue_minmodel_stack_copy
    ;       svalue_minmodel_stack_copy_debug
    ;       svalue_minmodel_own_stack
    ;       svalue_minmodel_own_stack_debug

    ;       svalue_thread_safe_no
    ;       svalue_thread_safe_yes

    ;       svalue_gc_none
    ;       svalue_gc_bdw
    ;       svalue_gc_bdw_debug
    ;       svalue_gc_target_native
    ;       svalue_gc_accurate
    ;       svalue_gc_history

    ;       svalue_deep_prof_no
    ;       svalue_deep_prof_yes

    ;       svalue_mprof_call_no
    ;       svalue_mprof_call_yes

    ;       svalue_mprof_time_no
    ;       svalue_mprof_time_yes

    ;       svalue_mprof_memory_no
    ;       svalue_mprof_memory_yes

    ;       svalue_tscope_prof_no
    ;       svalue_tscope_prof_yes

    ;       svalue_term_size_prof_no
    ;       svalue_term_size_prof_cells
    ;       svalue_term_size_prof_words

    ;       svalue_debug_none
    ;       svalue_debug_debug
    ;       svalue_debug_decldebug

    ;       svalue_ssdebug_no
    ;       svalue_ssdebug_yes

    ;       svalue_lldebug_no
    ;       svalue_lldebug_yes

    ;       svalue_rbmm_no
    ;       svalue_rbmm_yes

    ;       svalue_rbmm_debug_no
    ;       svalue_rbmm_debug_yes

    ;       svalue_rbmm_prof_no
    ;       svalue_rbmm_prof_yes

    ;       svalue_pregen_no
    ;       svalue_pregen_yes

    ;       svalue_request_single_prec_float_no
    ;       svalue_request_single_prec_float_yes

    ;       svalue_merc_float_is_boxed_c_double
    ;       svalue_merc_float_is_unboxed_c_double
    ;       svalue_merc_float_is_unboxed_c_float.

%---------------------------------------------------------------------------%

:- type specs_version
    --->    specs_version_0
    ;       specs_version_1.

:- type solver_var_spec
    --->    solver_var_spec(
                svs_var                     :: solver_var_id,
                svs_values                  :: list(solver_var_value_id)
            ).

:- func init_solver_var_specs(specs_version) = list(solver_var_spec).

%---------------------------------------------------------------------------%

:- type if_spec
    --->    being(solver_var_id, solver_var_value_id).

:- type then_spec
    --->    is_one_of(solver_var_id, list(solver_var_value_id)).

:- type implication_spec
    --->    implies_that(if_spec, then_spec).

:- type requirement_spec
    --->    requirement_spec(
                rs_explanation              :: string,
                rs_implication              :: implication_spec
            ).

:- func init_requirement_specs = list(requirement_spec).

%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

init_solver_var_specs(SpecsVersion) = Specs :-
    (
        SpecsVersion = specs_version_0,
        StackLenPrefOrder =
            [svalue_stack_len_std, svalue_stack_len_segments,
            svalue_stack_len_extend],
        GcPrefOrder = 
            [svalue_gc_none, svalue_gc_bdw, svalue_gc_target_native,
            svalue_gc_accurate, svalue_gc_bdw_debug, svalue_gc_history]
    ;
        SpecsVersion = specs_version_1,
        StackLenPrefOrder =
            [svalue_stack_len_segments, svalue_stack_len_std,
            svalue_stack_len_extend],
        GcPrefOrder = 
            [svalue_gc_bdw, svalue_gc_target_native, svalue_gc_accurate,
            svalue_gc_bdw_debug, svalue_gc_none, svalue_gc_history]
    ),

    % The order of solver variables here is the order in which labeling
    % will try to choose a variable to bind.
    %
    % The order of values within each solver variables is the order of
    % preference: labeling will always choose to set the chosen solver variable
    % to the first of its values that has not previously been ruled out.
    %
    % The first group of variables are set to autoconfigured values
    % before constraint solving starts. By putting them at the start,
    % the very first labeling step will skip over them; later labelling steps
    % won't have to look at them.
    Specs = [
        solver_var_spec(svar_ac_gcc_regs_avail,
            [svalue_ac_gcc_regs_avail_no,
            svalue_ac_gcc_regs_avail_yes]),
        solver_var_spec(svar_ac_gcc_gotos_avail,
            [svalue_ac_gcc_gotos_avail_no,
            svalue_ac_gcc_gotos_avail_yes]),
        solver_var_spec(svar_ac_gcc_labels_avail,
            [svalue_ac_gcc_labels_avail_no,
            svalue_ac_gcc_labels_avail_yes]),
        solver_var_spec(svar_ac_low_tag_bits_avail,
            [svalue_ac_low_tag_bits_avail_0,
            svalue_ac_low_tag_bits_avail_2,
            svalue_ac_low_tag_bits_avail_3]),
        solver_var_spec(svar_ac_size_of_double,
            [svalue_ac_size_of_double_eq_ptr,
            svalue_ac_size_of_double_ne_ptr]),
        solver_var_spec(svar_ac_merc_file,
            [svalue_ac_merc_file_no, svalue_ac_merc_file_yes]),

        solver_var_spec(svar_backend,
            [svalue_backend_mlds, svalue_backend_llds, svalue_backend_elds]),
        solver_var_spec(svar_datarep,
            [svalue_datarep_heap_cells, svalue_datarep_classes,
            svalue_datarep_erlang]),
        solver_var_spec(svar_target,
            [svalue_target_c, svalue_target_csharp,
            svalue_target_java, svalue_target_erlang]),
        solver_var_spec(svar_nested_funcs,
            [svalue_nested_funcs_no, svalue_nested_funcs_yes]),

        solver_var_spec(svar_gcc_regs_use,
            [svalue_gcc_regs_use_yes, svalue_gcc_regs_use_no]),
        solver_var_spec(svar_gcc_gotos_use,
            [svalue_gcc_gotos_use_yes, svalue_gcc_gotos_use_no]),
        solver_var_spec(svar_gcc_labels_use,
            [svalue_gcc_labels_use_yes, svalue_gcc_labels_use_no]),

        solver_var_spec(svar_pregen,
            [svalue_pregen_no, svalue_pregen_yes]),
        solver_var_spec(svar_low_tag_bits_use,
            [svalue_low_tag_bits_use_3, svalue_low_tag_bits_use_2,
            svalue_low_tag_bits_use_0]),

        solver_var_spec(svar_stack_len,
            StackLenPrefOrder),
        solver_var_spec(svar_trail,
            [svalue_trail_no, svalue_trail_yes]),
        solver_var_spec(svar_trail_segments,
            [svalue_trail_segments_yes, svalue_trail_segments_no]),

        solver_var_spec(svar_minmodel,
            [svalue_minmodel_no,
            svalue_minmodel_stack_copy, svalue_minmodel_stack_copy_debug,
            svalue_minmodel_own_stack, svalue_minmodel_own_stack_debug]),

        solver_var_spec(svar_thread_safe,
            [svalue_thread_safe_no, svalue_thread_safe_yes]),

        solver_var_spec(svar_gc,
            GcPrefOrder),

        solver_var_spec(svar_deep_prof,
            [svalue_deep_prof_no, svalue_deep_prof_yes]),
        solver_var_spec(svar_mprof_call,
            [svalue_mprof_call_no, svalue_mprof_call_yes]),
        solver_var_spec(svar_mprof_time,
            [svalue_mprof_time_no, svalue_mprof_time_yes]),
        solver_var_spec(svar_mprof_memory,
            [svalue_mprof_memory_no, svalue_mprof_memory_yes]),
        solver_var_spec(svar_tscope_prof,
            [svalue_tscope_prof_no, svalue_tscope_prof_yes]),
        solver_var_spec(svar_term_size_prof,
            [svalue_term_size_prof_no,
            svalue_term_size_prof_cells, svalue_term_size_prof_words]),

        solver_var_spec(svar_debug,
            [svalue_debug_none, svalue_debug_debug, svalue_debug_decldebug]),
        solver_var_spec(svar_ssdebug,
            [svalue_ssdebug_no, svalue_ssdebug_yes]),
        solver_var_spec(svar_lldebug,
            [svalue_lldebug_no, svalue_lldebug_yes]),

        solver_var_spec(svar_rbmm,
            [svalue_rbmm_no, svalue_rbmm_yes]),
        solver_var_spec(svar_rbmm_debug,
            [svalue_rbmm_debug_no, svalue_rbmm_debug_yes]),
        solver_var_spec(svar_rbmm_prof,
            [svalue_rbmm_prof_no, svalue_rbmm_prof_yes]),

        solver_var_spec(svar_request_single_prec_float,
            [svalue_request_single_prec_float_no,
            svalue_request_single_prec_float_yes]),

        solver_var_spec(svar_merc_float,
            [svalue_merc_float_is_unboxed_c_double,
            svalue_merc_float_is_boxed_c_double,
            svalue_merc_float_is_unboxed_c_float])
    ].

%---------------------------------------------------------------------------%

init_requirement_specs = [
% Requirements of values of svar_ac_gcc_regs_avail.
    % None. The value is set by configure.

% Requirements of values of svar_ac_gcc_gotos_avail.
    % None. The value is set by configure.

% Requirements of values of svar_ac_gcc_labels_avail.
    % None. The value is set by configure.

% Requirements of values of svar_ac_low_tag_bits_avail.
    % None. The value is set by configure.

% Requirements of values of svar_ac_size_of_double.
    % None. The value is set by configure.

% Requirements of values of svar_ac_merc_file.
    % None. The value is set by configure.

% Requirements of values of svar_backend.
    requirement_spec(
        "MLDS backend requires targeting C, C# or Java",
        (svar_backend `being` svalue_backend_mlds) `implies_that`
        (svar_target `is_one_of` 
            [svalue_target_c, svalue_target_csharp, svalue_target_java])
    ),
    requirement_spec(
        "LLDS backend requires targeting C",
        (svar_backend `being` svalue_backend_llds) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "LLDS backend requires storing data in heap cells",
        (svar_backend `being` svalue_backend_llds) `implies_that`
        (svar_datarep `is_one_of` [svalue_datarep_heap_cells])
    ),
    requirement_spec(
        "ELDS backend requires targeting Erlang",
        (svar_backend `being` svalue_backend_elds) `implies_that`
        (svar_target `is_one_of` [svalue_target_erlang])
    ),

% Requirements of values of svar_datarep.
    requirement_spec(
        "representing data using classes data requires the MLDS backend",
        (svar_datarep `being` svalue_datarep_classes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_mlds])
    ),

% Requirements of values of svar_target.
    requirement_spec(
        "targeting C# requires the MLDS backend",
        (svar_target `being` svalue_target_csharp) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_mlds])
    ),
    requirement_spec(
        "targeting Java requires the MLDS backend",
        (svar_target `being` svalue_target_java) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_mlds])
    ),
    requirement_spec(
        "targeting Erlang requires the ELDS backend",
        (svar_target `being` svalue_target_erlang) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_elds])
    ),

    requirement_spec(
        "targeting C# requires representing data using classes",
        (svar_target `being` svalue_target_csharp) `implies_that`
        (svar_datarep `is_one_of` [svalue_datarep_classes])
    ),
    requirement_spec(
        "targeting Java requires representing data using classes",
        (svar_target `being` svalue_target_java) `implies_that`
        (svar_datarep `is_one_of` [svalue_datarep_classes])
    ),
    requirement_spec(
        "targeting Erlang requires using Erlang terms",
        (svar_target `being` svalue_target_erlang) `implies_that`
        (svar_datarep `is_one_of` [svalue_datarep_erlang])
    ),

    requirement_spec(
        "C does not have a native garbage collector",
        (svar_target `being` svalue_target_c) `implies_that`
        (svar_gc `is_one_of` [svalue_gc_bdw, svalue_gc_bdw_debug,
            svalue_gc_accurate, svalue_gc_history, svalue_gc_none])
    ),
    requirement_spec(
        "targeting C# requires target native gc",
        (svar_target `being` svalue_target_csharp) `implies_that`
        (svar_gc `is_one_of` [svalue_gc_target_native])
    ),
    requirement_spec(
        "targeting Java requires target native gc",
        (svar_target `being` svalue_target_java) `implies_that`
        (svar_gc `is_one_of` [svalue_gc_target_native])
    ),
    requirement_spec(
        "targeting Erlang requires target native gc",
        (svar_target `being` svalue_target_erlang) `implies_that`
        (svar_gc `is_one_of` [svalue_gc_target_native])
    ),

    requirement_spec(
        "generated C# is always thread safe",
        (svar_target `being` svalue_target_csharp) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_yes])
    ),
    requirement_spec(
        "generated Java is always thread safe",
        (svar_target `being` svalue_target_java) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_yes])
    ),
    % Generated Erlang is also always thread safe, but library/thread.m
    % does not (yet) have Erlang implementations of its foreign_procs,
    % so the program cannot create new threads.
    requirement_spec(
        "targeting Erlang does not allow new threads to be created",
        (svar_target `being` svalue_target_erlang) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_no])
    ),

% These are covered by a single requirement from spf back to target.
%   requirement_spec(
%       "targeting C# is incompatible with single-precision floats",
%       (svar_target `being` svalue_target_csharp) `implies_that`
%       (svar_single_prec_float `is_one_of` [svalue_single_prec_float_no])
%   ),
%   requirement_spec(
%       "targeting Java is incompatible with single-precision floats",
%       (svar_target `being` svalue_target_java) `implies_that`
%       (svar_single_prec_float `is_one_of` [svalue_single_prec_float_no])
%   ),
%   requirement_spec(
%       "targeting Erlang is incompatible with single-precision floats",
%       (svar_target `being` svalue_target_erlang) `implies_that`
%       (svar_single_prec_float `is_one_of` [svalue_single_prec_float_no])
%   ),

% Requirements of values of svar_nested_funcs.
    requirement_spec(
        "using gcc nested functions requires the MLDS backend",
        (svar_nested_funcs `being` svalue_nested_funcs_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_mlds])
    ),
    requirement_spec(
        "using gcc nested functions requires targeting C",
        (svar_nested_funcs `being` svalue_nested_funcs_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),

% Requirements of values of svar_gcc_regs_use.
    requirement_spec(
        "using gcc regs requires them to be available",
        (svar_gcc_regs_use `being` svalue_gcc_regs_use_yes) `implies_that`
        (svar_ac_gcc_regs_avail `is_one_of` [svalue_ac_gcc_regs_avail_yes])
    ),
    requirement_spec(
        "using gcc regs requires targeting C",
        (svar_gcc_regs_use `being` svalue_gcc_regs_use_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "using gcc regs requires the LLDS backend",
        (svar_gcc_regs_use `being` svalue_gcc_regs_use_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),

% Requirements of values of svar_gcc_gotos_use.
    requirement_spec(
        "using gcc nonlocal gotos requires them to be available",
        (svar_gcc_gotos_use `being` svalue_gcc_gotos_use_yes) `implies_that`
        (svar_ac_gcc_gotos_avail `is_one_of` [svalue_ac_gcc_gotos_avail_yes])
    ),
    requirement_spec(
        "using gcc nonlocal gotos requires targeting C",
        (svar_gcc_gotos_use `being` svalue_gcc_gotos_use_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "using gcc nonlocal gotos requires the LLDS backend",
        (svar_gcc_gotos_use `being` svalue_gcc_gotos_use_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),

% Requirements of values of svar_gcc_labels_use.
    requirement_spec(
        "using gcc asm labels requires them to be available",
        (svar_gcc_labels_use `being` svalue_gcc_labels_use_yes) `implies_that`
        (svar_ac_gcc_labels_avail `is_one_of` [svalue_ac_gcc_labels_avail_yes])
    ),
    requirement_spec(
        "using gcc asm labels requires using gcc nonlocal gotos",
        (svar_gcc_labels_use `being` svalue_gcc_labels_use_yes) `implies_that`
        (svar_gcc_gotos_use `is_one_of` [svalue_gcc_gotos_use_yes])
    ),
    requirement_spec(
        "using gcc asm labels requires targeting C",
        (svar_gcc_labels_use `being` svalue_gcc_labels_use_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "using gcc asm labels requires the LLDS backend",
        (svar_gcc_labels_use `being` svalue_gcc_labels_use_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),

% Requirements of values of svar_pregen.
    requirement_spec(
        "pregenerated code always targets C",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "pregenerated code uses 2 low tag bits",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_low_tag_bits_use `is_one_of` [svalue_low_tag_bits_use_2])
    ),
    requirement_spec(
        "pregenerated code is incompatible with single precision floats",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_request_single_prec_float `is_one_of`
            [svalue_request_single_prec_float_no])
    ),

% Requirements of values of svar_low_tag_bits_use.
    requirement_spec(
        "using 2 low tag bits needs at least 2 low tag bits to be available",
        (svar_low_tag_bits_use `being` svalue_low_tag_bits_use_2)
            `implies_that`
        (svar_ac_low_tag_bits_avail `is_one_of`
            [svalue_ac_low_tag_bits_avail_2, svalue_ac_low_tag_bits_avail_3])
    ),
    requirement_spec(
        "using 3 low tag bits needs at least 3 low tag bits to be available",
        (svar_low_tag_bits_use `being` svalue_low_tag_bits_use_3)
            `implies_that`
        (svar_ac_low_tag_bits_avail `is_one_of`
            [svalue_ac_low_tag_bits_avail_3])
    ),

% Requirements of values of svar_stack_segments.
    requirement_spec(
        "stack segments require the LLDS backend",
        (svar_stack_len `being` svalue_stack_len_segments) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "stack extension requires the LLDS backend",
        (svar_stack_len `being` svalue_stack_len_extend) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),

% Requirements of values of svar_trail.
    requirement_spec(
        "trailing requires targeting C",
        (svar_trail `being` svalue_trail_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "trailing interferes with minimal model tabling",
        (svar_trail `being` svalue_trail_yes) `implies_that`
        (svar_minmodel `is_one_of` [svalue_minmodel_no])
    ),

% Requirements of values of svar_trail_segments.
    requirement_spec(
        "trail segments require trailing",
        (svar_trail_segments `being` svalue_trail_segments_yes) `implies_that`
        (svar_trail `is_one_of` [svalue_trail_yes])
    ),

% Requirements of values of svar_minmodel.
    requirement_spec(
        "minimal model tabling requires the LLDS backend",
        (svar_minmodel `being` svalue_minmodel_stack_copy)
            `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "minimal model tabling requires the LLDS backend",
        (svar_minmodel `being` svalue_minmodel_stack_copy_debug)
            `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "minimal model tabling requires the LLDS backend",
        (svar_minmodel `being` svalue_minmodel_own_stack)
            `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "minimal model tabling requires the LLDS backend",
        (svar_minmodel `being` svalue_minmodel_own_stack_debug)
            `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "minimal model tabling requires boehm-demers-weiser gc",
        (svar_minmodel `being` svalue_minmodel_stack_copy)
            `implies_that`
        (svar_gc `is_one_of`
            [svalue_gc_none, svalue_gc_bdw, svalue_gc_bdw_debug])
    ),
    requirement_spec(
        "minimal model tabling requires boehm-demers-weiser gc",
        (svar_minmodel `being` svalue_minmodel_stack_copy_debug)
            `implies_that`
        (svar_gc `is_one_of`
            [svalue_gc_none, svalue_gc_bdw, svalue_gc_bdw_debug])
    ),
    requirement_spec(
        "minimal model tabling requires boehm-demers-weiser gc",
        (svar_minmodel `being` svalue_minmodel_own_stack)
            `implies_that`
        (svar_gc `is_one_of`
            [svalue_gc_none, svalue_gc_bdw, svalue_gc_bdw_debug])
    ),
    requirement_spec(
        "minimal model tabling requires boehm-demers-weiser gc",
        (svar_minmodel `being` svalue_minmodel_own_stack_debug)
            `implies_that`
        (svar_gc `is_one_of`
            [svalue_gc_none, svalue_gc_bdw, svalue_gc_bdw_debug])
    ),
    requirement_spec(
        "minimal model tabling does not respect thread safety",
        (svar_minmodel `being` svalue_minmodel_stack_copy)
            `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_no])
    ),
    requirement_spec(
        "minimal model tabling does not respect thread safety",
        (svar_minmodel `being` svalue_minmodel_stack_copy_debug)
            `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_no])
    ),
    % XXX Do svalue_minmodel_own_stack{,_debug} imply
    % svalue_thread_safe_no?

% Requirements of values of svar_thread_safe.
    % None.
    % XXX This is probably wrong; there ought to be some.

% Requirements of values of svar_gc.
    requirement_spec(
        "boehm-demers-weiser gc requires targeting C",
        (svar_gc `being` svalue_gc_bdw) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "boehm-demers-weiser debug gc requires targeting C",
        (svar_gc `being` svalue_gc_bdw_debug) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "accurate gc requires targeting C",
        (svar_gc `being` svalue_gc_accurate) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "accurate gc requires the MLDS backend",
        (svar_gc `being` svalue_gc_accurate) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_mlds])
    ),
    requirement_spec(
        "history gc requires targeting C",
        (svar_gc `being` svalue_gc_history) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),

% Requirements of values of svar_deep_prof.
    requirement_spec(
        "deep profiling requires the LLDS backend",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "deep profiling interferes with minimal model tabling",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_minmodel `is_one_of` [svalue_minmodel_no])
    ),
    requirement_spec(
        "deep profiling is incompatible with mprof call profiling",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_mprof_call `is_one_of` [svalue_mprof_call_no])
    ),
    requirement_spec(
        "deep profiling is incompatible with mprof time profiling",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_mprof_time `is_one_of` [svalue_mprof_time_no])
    ),
    requirement_spec(
        "deep profiling is incompatible with mprof memory profiling",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_mprof_memory `is_one_of` [svalue_mprof_memory_no])
    ),

% Requirements of values of svar_mprof_call.
    requirement_spec(
        "mprof call profiling requires targeting C",
        (svar_mprof_call `being` svalue_mprof_call_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "mprof call profiling interferes with minimal model tabling",
        (svar_mprof_call `being` svalue_mprof_call_yes) `implies_that`
        (svar_minmodel `is_one_of` [svalue_minmodel_no])
    ),

% Requirements of values of svar_mprof_time.
    requirement_spec(
        "mprof time profiling requires targeting C",
        (svar_mprof_time `being` svalue_mprof_time_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "mprof time profiling requires mprof call profiling",
        % XXX runtime/mercury_grade.h allows MR_MPROF_PROFILE_TIME without
        % MR_MPROF_PROFILE_CALLS, but calls the combination "useless".
        (svar_mprof_time `being` svalue_mprof_time_yes) `implies_that`
        (svar_mprof_call `is_one_of` [svalue_mprof_call_yes])
    ),

% Requirements of values of svar_mprof_memory.
    requirement_spec(
        "mprof memory profiling requires targeting C",
        (svar_mprof_memory `being` svalue_mprof_memory_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "mprof memory profiling requires mprof call profiling",
        (svar_mprof_memory `being` svalue_mprof_memory_yes) `implies_that`
        (svar_mprof_call `is_one_of` [svalue_mprof_call_yes])
    ),

% Requirements of values of svar_tscope_prof.
    requirement_spec(
        "threadscope style profiling requires the LLDS backend",
        (svar_tscope_prof `being` svalue_tscope_prof_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "threadscope style profiling requires thread safe code",
        (svar_tscope_prof `being` svalue_tscope_prof_yes) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_yes])
    ),

% Requirements of values of svar_term_size_prof.
    requirement_spec(
        "term size profiling requires the LLDS backend",
        (svar_term_size_prof `being` svalue_term_size_prof_cells) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "term size profiling requires the LLDS backend",
        (svar_term_size_prof `being` svalue_term_size_prof_words) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),

% Requirements of values of svar_debug.
    requirement_spec(
        "debugging requires the LLDS backend",
        (svar_debug `being` svalue_debug_debug) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "declarative debugging requires the LLDS backend",
        (svar_debug `being` svalue_debug_decldebug) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),

% Requirements of values of svar_lldebug.
    requirement_spec(
        "source-to-source debugging does not make sense for the LLDS backend",
        (svar_ssdebug `being` svalue_ssdebug_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_mlds, svalue_backend_elds])
    ),

% Requirements of values of svar_lldebug.
    requirement_spec(
        "low level debugging applies only to the LLDS backend",
        (svar_lldebug `being` svalue_lldebug_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),

% Requirements of values of svar_rbmm.
    requirement_spec(
        "region based memory management requires the LLDS backend",
        (svar_rbmm `being` svalue_rbmm_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),

% Requirements of values of svar_request_single_prec_float.
    requirement_spec(
        "single precision floats are available only when targeting C",
        (svar_request_single_prec_float `being`
            svalue_request_single_prec_float_yes)
            `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "single precision floats are available when requested",
        % Since nothing forbids svalue_merc_float_is_unboxed_c_float,
        % this implication should always succeed.
        (svar_request_single_prec_float `being`
            svalue_request_single_prec_float_yes)
            `implies_that`
        (svar_merc_float `is_one_of` [svalue_merc_float_is_unboxed_c_float])
    ),

% Requirements of values of svar_merc_float.
    requirement_spec(
        "unboxed double precision floats require pointer-sized doubles",
        (svar_merc_float `being` svalue_merc_float_is_unboxed_c_double)
            `implies_that`
        (svar_ac_size_of_double `is_one_of` [svalue_ac_size_of_double_eq_ptr])
    )
].

%---------------------------------------------------------------------------%
:- end_module grade_spec.
%---------------------------------------------------------------------------%
