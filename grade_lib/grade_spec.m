%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This module defines the "terrain" on which the grade solver operates,
% by defining the list of solver variables, the values they may have,
% and the constraints that assignments of values to solver variables
% must satisfy to be a valid solution.
%
% To keep this module easy to read and thus to check, it contains only
% specification-level information; it intentionally leaves out the data
% structures needed during constraint solving that are irrelevant to
% the specification of the problem. Those will be added by the code of
% setup_solver_info in grade_setup.m.
%

:- module grade_lib.grade_spec.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%
%
% The solver_var_id and solver_var_value_id types embody one of the four
% representations of grades described in compiler/notes/grade_library.html.
% This is the representation used by the solver, because it allows
% the solver to use generic code to implement propagation on *all*
% requirements, regardless of what solver variables and values they involve.
%

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
    ;       svar_gcc_conf
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
    ;       svar_target_debug
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

    ;       svalue_gcc_conf_none       % used for non-LLDS backends
    ;       svalue_gcc_conf_reg
    ;       svalue_gcc_conf_jump
    ;       svalue_gcc_conf_fast
    ;       svalue_gcc_conf_asm_jump
    ;       svalue_gcc_conf_asm_fast

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

    ;       svalue_thread_safe_c_no
    ;       svalue_thread_safe_c_yes
    ;       svalue_thread_safe_target_native

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

    ;       svalue_target_debug_no
    ;       svalue_target_debug_yes

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

    % See the documentation of init_solver_var_specs below.
:- type specs_version
    --->    specs_version_0
    ;       specs_version_1.

    % solvar_var_spec(VarId, [ValueId1, ValueId2, ... ValueIdN]) means that
    % the value of VarId must be one of {ValueId1, ValueId2, ValueIdN}.
    %
:- type solver_var_spec
    --->    solver_var_spec(
                svs_var                     :: solver_var_id,
                svs_values                  :: list(solver_var_value_id)
            ).

    % Return the list of solver variables in a grade specification problem,
    % and the list of possible values of each of those solver variables.
    %
    % The order of solver variables in the returned list is the order
    % in which labeling will try to choose a variable to bind.
    %
    % The order of values within each solver variable is the order of
    % preference: labeling will always choose to set the chosen solver variable
    % to the first of its values that has not previously been ruled out.
    %
    % For a few solver variables, the order of "preference" we currently use
    % is not the one we *want* to use. If a grade string does not explicitly
    % specify the use of a specific gc algorithm, we currently assume that this
    % implicitly specifies NOT using any garbage collector, and we likewise
    % assume that the absence of a grade component (.stseg or .exts) that
    % asks for a dynamically sized stack means that the user wants a fixed
    % size stack. This is "version 0" of the preferences, and hence of
    % the solver var specs. However, we want to change the defaults
    % to the use of the Boehm collector and stack segments respectively,
    % which is what version 1 of the preferences calls for.
    %
:- func init_solver_var_specs(specs_version) = list(solver_var_spec).

%---------------------------------------------------------------------------%
%
% A requirement is a named implication of the form
%
%   (IfVar `being` IfValue)
%       `implies_that`
%   (ThenVar `is_one_of` [ThenValue1, ..., ThenValueN])
%
% The solver can use the implication to do propagation in both directions.
%
% Forward direction: if the solver knows that IfVar = IfValue, it will
% mark all values of ThenVar that are not among ThenValue1 .. ThenValueN
% as not possible.
%
% Reverse direction: if the solver knows that ThenVar cannot be
% any of the values in ThenValue1 .. ThenValueN, then it will record that
% IfVar = IfValue is not possible either.
%

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

    % Return the list of requirements that represent the dependencies
    % and incompatibilities between grade components, as represented
    % by their solver vars.
    %
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
            svalue_gc_bdw_debug, svalue_gc_accurate, svalue_gc_history]
    ;
        SpecsVersion = specs_version_1,
        StackLenPrefOrder =
            [svalue_stack_len_segments, svalue_stack_len_std,
            svalue_stack_len_extend],
        GcPrefOrder =
            [svalue_gc_bdw, svalue_gc_target_native, svalue_gc_bdw_debug,
            svalue_gc_none, svalue_gc_accurate, svalue_gc_history]
    ),

    Specs = [
        % This first group of variables are set (by setup_solver_info)
        % to autoconfigured values before constraint solving starts.
        % By putting them at the start, the very first labeling step will
        % skip over them; later labelling steps won't have to look at them.
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

        solver_var_spec(svar_gcc_conf,
            % XXX The order of preference here is partially speed,
            % partially how well the grade is tested.
            [svalue_gcc_conf_asm_fast,
            svalue_gcc_conf_reg,
            svalue_gcc_conf_none,
            svalue_gcc_conf_fast,
            svalue_gcc_conf_jump,
            svalue_gcc_conf_asm_jump]),

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
            [svalue_thread_safe_c_no, svalue_thread_safe_c_yes,
            svalue_thread_safe_target_native]),

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
        solver_var_spec(svar_target_debug,
            [svalue_target_debug_no, svalue_target_debug_yes]),

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
        "Using the MLDS backend requires targeting C, C# or Java.",
        (svar_backend `being` svalue_backend_mlds) `implies_that`
        (svar_target `is_one_of`
            [svalue_target_c, svalue_target_csharp, svalue_target_java])
    ),
    requirement_spec(
        "Using the LLDS backend requires targeting C.",
        (svar_backend `being` svalue_backend_llds) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "Using the LLDS backend requires storing data in heap cells.",
        (svar_backend `being` svalue_backend_llds) `implies_that`
        (svar_datarep `is_one_of` [svalue_datarep_heap_cells])
    ),
    requirement_spec(
        "Using the ELDS backend requires targeting Erlang.",
        (svar_backend `being` svalue_backend_elds) `implies_that`
        (svar_target `is_one_of` [svalue_target_erlang])
    ),

    requirement_spec(
        "The use of gcc extensions makes sense only for the LLDS backend.",
        (svar_backend `being` svalue_backend_mlds) `implies_that`
        (svar_gcc_conf `is_one_of` [svalue_gcc_conf_none])
    ),
    requirement_spec(
        "The use of gcc extensions makes sense only for the LLDS backend.",
        (svar_backend `being` svalue_backend_elds) `implies_that`
        (svar_gcc_conf `is_one_of` [svalue_gcc_conf_none])
    ),

% Requirements of values of svar_datarep.
    requirement_spec(
        "Representing data using classes requires the MLDS backend.",
        (svar_datarep `being` svalue_datarep_classes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_mlds])
    ),

% Requirements of values of svar_target.
    requirement_spec(
        "Targeting C# requires the MLDS backend.",
        (svar_target `being` svalue_target_csharp) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_mlds])
    ),
    requirement_spec(
        "Targeting Java requires the MLDS backend.",
        (svar_target `being` svalue_target_java) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_mlds])
    ),
    requirement_spec(
        "Targeting Erlang requires the ELDS backend.",
        (svar_target `being` svalue_target_erlang) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_elds])
    ),

    requirement_spec(
        "Targeting C# requires representing data using classes.",
        (svar_target `being` svalue_target_csharp) `implies_that`
        (svar_datarep `is_one_of` [svalue_datarep_classes])
    ),
    requirement_spec(
        "Targeting Java requires representing data using classes.",
        (svar_target `being` svalue_target_java) `implies_that`
        (svar_datarep `is_one_of` [svalue_datarep_classes])
    ),
    requirement_spec(
        "Targeting Erlang requires using Erlang terms.",
        (svar_target `being` svalue_target_erlang) `implies_that`
        (svar_datarep `is_one_of` [svalue_datarep_erlang])
    ),

    requirement_spec(
        "C does not have a native garbage collector.",
        (svar_target `being` svalue_target_c) `implies_that`
        (svar_gc `is_one_of` [svalue_gc_bdw, svalue_gc_bdw_debug,
            svalue_gc_accurate, svalue_gc_history, svalue_gc_none])
    ),
    requirement_spec(
        "Targeting C# requires target native gc.",
        (svar_target `being` svalue_target_csharp) `implies_that`
        (svar_gc `is_one_of` [svalue_gc_target_native])
    ),
    requirement_spec(
        "Targeting Java requires target native gc.",
        (svar_target `being` svalue_target_java) `implies_that`
        (svar_gc `is_one_of` [svalue_gc_target_native])
    ),
    requirement_spec(
        "Targeting Erlang requires target native gc.",
        (svar_target `being` svalue_target_erlang) `implies_that`
        (svar_gc `is_one_of` [svalue_gc_target_native])
    ),

    requirement_spec(
        "C does not have its own native threading model.",
        (svar_target `being` svalue_target_c) `implies_that`
        (svar_thread_safe `is_one_of`
            [svalue_thread_safe_c_no, svalue_thread_safe_c_yes])
    ),
    requirement_spec(
        "Generating C# implies using the C# threading model.",
        (svar_target `being` svalue_target_csharp) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_target_native])
    ),
    requirement_spec(
        "Generating Java implies using the Java threading model.",
        (svar_target `being` svalue_target_java) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_target_native])
    ),
    % XXX Generated Erlang is also always thread safe, but library/thread.m
    % does not (yet) have Erlang implementations of its foreign_procs,
    % so the program cannot create new threads.
    requirement_spec(
        "Generating Erlang implies using the Erlang threading model.",
        (svar_target `being` svalue_target_erlang) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_target_native])
    ),

% These are covered by a single requirement from spf back to target.
%   requirement_spec(
%       "Targeting C# is incompatible with single-precision floats.",
%       (svar_target `being` svalue_target_csharp) `implies_that`
%       (svar_single_prec_float `is_one_of` [svalue_single_prec_float_no])
%   ),
%   requirement_spec(
%       "Targeting Java is incompatible with single-precision floats.",
%       (svar_target `being` svalue_target_java) `implies_that`
%       (svar_single_prec_float `is_one_of` [svalue_single_prec_float_no])
%   ),
%   requirement_spec(
%       "Targeting Erlang is incompatible with single-precision floats.",
%       (svar_target `being` svalue_target_erlang) `implies_that`
%       (svar_single_prec_float `is_one_of` [svalue_single_prec_float_no])
%   ),

% Requirements of values of svar_gcc_conf.
    % These requirements are expressed in reverse.
    requirement_spec(
        "Using gcc register extensions requires them to be available.",
        (svar_ac_gcc_regs_avail `being` svalue_ac_gcc_regs_avail_no)
            `implies_that`
        (svar_gcc_conf `is_one_of`
            [svalue_gcc_conf_none,
            svalue_gcc_conf_jump,
            svalue_gcc_conf_asm_jump])
    ),
    requirement_spec(
        "Using gcc nonlocal gotos requires them to be available.",
        (svar_ac_gcc_gotos_avail `being` svalue_ac_gcc_gotos_avail_no)
            `implies_that`
        (svar_gcc_conf `is_one_of`
            [svalue_gcc_conf_none,
            svalue_gcc_conf_reg])
    ),
    requirement_spec(
        "Using gcc asm labels requires them to be available.",
        (svar_ac_gcc_labels_avail `being` svalue_ac_gcc_labels_avail_no)
            `implies_that`
        (svar_gcc_conf `is_one_of`
            [svalue_gcc_conf_none,
            svalue_gcc_conf_reg,
            svalue_gcc_conf_jump,
            svalue_gcc_conf_fast])
    ),

% Requirements of values of svar_pregen.
    requirement_spec(
        "Pregenerated code always targets C.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "Pregenerated code always uses none, reg, asm_fast or hlc.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_gcc_conf `is_one_of`
            [svalue_gcc_conf_none, svalue_gcc_conf_reg,
            svalue_gcc_conf_asm_fast])
    ),
    requirement_spec(
        "Pregenerated code uses 2 low tag bits.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_low_tag_bits_use `is_one_of` [svalue_low_tag_bits_use_2])
    ),
    % XXX We should require stack segments on llds.
    % With version 1 of the requirements, we prefer stack segments when
    % possible, and with the llds backend, they are possible,
    % but this is not as good as a requirement, since it can be overridden.
    % Unfortunately, we don't (yet) support implications with *two*
    % conditions, such as "pregen=yes and backend=llds implies stack segments".
    % XXX Should we relax the following restriction?
    requirement_spec(
        "Pregenerated code never uses a trail.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_trail `is_one_of` [svalue_trail_no])
    ),
    requirement_spec(
        "Pregenerated code never uses minimal model.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_minmodel `is_one_of` [svalue_minmodel_no])
    ),
    requirement_spec(
        "Pregenerated code never supports thread safety.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_c_no])
    ),
    requirement_spec(
        "Pregenerated code always uses the Boehm garbage collector.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_gc `is_one_of` [svalue_gc_bdw])
    ),
    requirement_spec(
        "Pregenerated code never supports deep profiling.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_deep_prof `is_one_of` [svalue_deep_prof_no])
    ),
    requirement_spec(
        "Pregenerated code never supports mprof-style profiling.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_mprof_call `is_one_of` [svalue_mprof_call_no])
    ),
    requirement_spec(
        "Pregenerated code never supports term size profiling.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_term_size_prof `is_one_of` [svalue_term_size_prof_no])
    ),
    requirement_spec(
        "Pregenerated code never supports debugging.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_debug `is_one_of` [svalue_debug_none])
    ),
    requirement_spec(
        "Pregenerated code never supports source-to-source debugging.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_ssdebug `is_one_of` [svalue_ssdebug_no])
    ),
    requirement_spec(
        "Pregenerated code never supports target language debugging.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_target_debug `is_one_of` [svalue_target_debug_no])
    ),
    requirement_spec(
        "Pregenerated code never supports region based memory management.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_rbmm `is_one_of` [svalue_rbmm_no])
    ),
    requirement_spec(
        "Pregenerated code always uses boxed double-precision floats.",
        (svar_pregen `being` svalue_pregen_yes) `implies_that`
        (svar_merc_float `is_one_of` [svalue_merc_float_is_boxed_c_double])
    ),

% Requirements of values of svar_low_tag_bits_use.
    requirement_spec(
        "Using 2 low tag bits needs at least 2 low tag bits to be available.",
        (svar_low_tag_bits_use `being` svalue_low_tag_bits_use_2)
            `implies_that`
        (svar_ac_low_tag_bits_avail `is_one_of`
            [svalue_ac_low_tag_bits_avail_2, svalue_ac_low_tag_bits_avail_3])
    ),
    requirement_spec(
        "Using 3 low tag bits needs at least 3 low tag bits to be available.",
        (svar_low_tag_bits_use `being` svalue_low_tag_bits_use_3)
            `implies_that`
        (svar_ac_low_tag_bits_avail `is_one_of`
            [svalue_ac_low_tag_bits_avail_3])
    ),

% Requirements of values of svar_stack_segments.
    requirement_spec(
        "Stack segments require the LLDS backend.",
        (svar_stack_len `being` svalue_stack_len_segments) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "Stack extension requires the LLDS backend.",
        (svar_stack_len `being` svalue_stack_len_extend) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),

% Requirements of values of svar_trail.
    requirement_spec(
        "Trailing requires targeting C.",
        (svar_trail `being` svalue_trail_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "Trailing interferes with minimal model tabling.",
        (svar_trail `being` svalue_trail_yes) `implies_that`
        (svar_minmodel `is_one_of` [svalue_minmodel_no])
    ),

% Requirements of values of svar_trail_segments.
    requirement_spec(
        "Trail segments require trailing.",
        (svar_trail_segments `being` svalue_trail_segments_yes) `implies_that`
        (svar_trail `is_one_of` [svalue_trail_yes])
    ),

% Requirements of values of svar_minmodel.
    requirement_spec(
        "Minimal model tabling requires the LLDS backend.",
        (svar_minmodel `being` svalue_minmodel_stack_copy)
            `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "Minimal model tabling requires the LLDS backend.",
        (svar_minmodel `being` svalue_minmodel_stack_copy_debug)
            `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "Minimal model tabling requires the LLDS backend.",
        (svar_minmodel `being` svalue_minmodel_own_stack)
            `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "Minimal model tabling requires the LLDS backend.",
        (svar_minmodel `being` svalue_minmodel_own_stack_debug)
            `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "Minimal model tabling requires Boehm-Demers-Weiser gc.",
        (svar_minmodel `being` svalue_minmodel_stack_copy)
            `implies_that`
        (svar_gc `is_one_of`
            [svalue_gc_none, svalue_gc_bdw, svalue_gc_bdw_debug])
    ),
    requirement_spec(
        "Minimal model tabling requires Boehm-Demers-Weiser gc.",
        (svar_minmodel `being` svalue_minmodel_stack_copy_debug)
            `implies_that`
        (svar_gc `is_one_of`
            [svalue_gc_none, svalue_gc_bdw, svalue_gc_bdw_debug])
    ),
    requirement_spec(
        "Minimal model tabling requires Boehm-Demers-Weiser gc.",
        (svar_minmodel `being` svalue_minmodel_own_stack)
            `implies_that`
        (svar_gc `is_one_of`
            [svalue_gc_none, svalue_gc_bdw, svalue_gc_bdw_debug])
    ),
    requirement_spec(
        "Minimal model tabling requires Boehm-Demers-Weiser gc.",
        (svar_minmodel `being` svalue_minmodel_own_stack_debug)
            `implies_that`
        (svar_gc `is_one_of`
            [svalue_gc_none, svalue_gc_bdw, svalue_gc_bdw_debug])
    ),
    requirement_spec(
        "Minimal model tabling does not respect thread safety.",
        (svar_minmodel `being` svalue_minmodel_stack_copy)
            `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_c_no])
    ),
    requirement_spec(
        "Minimal model tabling does not respect thread safety.",
        (svar_minmodel `being` svalue_minmodel_stack_copy_debug)
            `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_c_no])
    ),
    % XXX Do svalue_minmodel_own_stack{,_debug} imply svalue_thread_safe_c_no?
    % For now, since the implementation of own stack minimal model
    % is not yet complete, we need not include its requirements in the list,
    % and not including them should make constraint solving a tiny bit faster.

% Requirements of values of svar_thread_safe.
    % None. There are some settings of other solver variables
    % that are incompatible with thread safety, but those incompatibilities
    % are expressed by requirements listed under the *other* solver variable.

% Requirements of values of svar_gc.
    requirement_spec(
        "Boehm-Demers-Weiser gc requires targeting C.",
        (svar_gc `being` svalue_gc_bdw) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "Boehm-Demers-Weiser debug gc requires targeting C.",
        (svar_gc `being` svalue_gc_bdw_debug) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "Accurate gc requires targeting C.",
        (svar_gc `being` svalue_gc_accurate) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "Accurate gc conflicts with threading.",
        (svar_gc `being` svalue_gc_accurate) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_c_no])
    ),
    requirement_spec(
        "History gc requires targeting C.",
        (svar_gc `being` svalue_gc_history) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "History gc conflicts with threading.",
        (svar_gc `being` svalue_gc_history) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_c_no])
    ),

% Requirements of values of svar_deep_prof.
    requirement_spec(
        "Deep profiling requires the LLDS backend.",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "Deep profiling interferes with minimal model tabling.",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_minmodel `is_one_of` [svalue_minmodel_no])
    ),
    requirement_spec(
        "Deep profiling does not work for multithreaded programs.",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_c_no])
    ),
    requirement_spec(
        "Deep profiling is incompatible with mprof call profiling.",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_mprof_call `is_one_of` [svalue_mprof_call_no])
    ),
    requirement_spec(
        "Deep profiling is incompatible with mprof time profiling.",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_mprof_time `is_one_of` [svalue_mprof_time_no])
    ),
    requirement_spec(
        "Deep profiling is incompatible with mprof memory profiling.",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_mprof_memory `is_one_of` [svalue_mprof_memory_no])
    ),

% Requirements of values of svar_mprof_call.
    requirement_spec(
        "Mprof call profiling requires targeting C.",
        (svar_mprof_call `being` svalue_mprof_call_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "Mprof call profiling interferes with minimal model tabling.",
        (svar_mprof_call `being` svalue_mprof_call_yes) `implies_that`
        (svar_minmodel `is_one_of` [svalue_minmodel_no])
    ),
    requirement_spec(
        "Mprof call profiling does not work for multithreaded programs.",
        (svar_mprof_call `being` svalue_mprof_call_yes) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_c_no])
    ),

% Requirements of values of svar_mprof_time.
    requirement_spec(
        "Mprof time profiling requires targeting C.",
        (svar_mprof_time `being` svalue_mprof_time_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "Mprof time profiling requires mprof call profiling.",
        % XXX runtime/mercury_grade.h allows MR_MPROF_PROFILE_TIME without
        % MR_MPROF_PROFILE_CALLS, but calls the combination "useless".
        (svar_mprof_time `being` svalue_mprof_time_yes) `implies_that`
        (svar_mprof_call `is_one_of` [svalue_mprof_call_yes])
    ),

% Requirements of values of svar_mprof_memory.
    requirement_spec(
        "Mprof memory profiling requires targeting C.",
        (svar_mprof_memory `being` svalue_mprof_memory_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "Mprof memory profiling requires mprof call profiling.",
        (svar_mprof_memory `being` svalue_mprof_memory_yes) `implies_that`
        (svar_mprof_call `is_one_of` [svalue_mprof_call_yes])
    ),

% Requirements of values of svar_tscope_prof.
    requirement_spec(
        "Threadscope style profiling requires the LLDS backend.",
        (svar_tscope_prof `being` svalue_tscope_prof_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "Threadscope style profiling requires thread safe code.",
        (svar_tscope_prof `being` svalue_tscope_prof_yes) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_c_yes])
    ),

% Requirements of values of svar_term_size_prof.
    requirement_spec(
        "Term size profiling requires the LLDS backend.",
        (svar_term_size_prof `being` svalue_term_size_prof_cells) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "Term size profiling requires the LLDS backend.",
        (svar_term_size_prof `being` svalue_term_size_prof_words) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "Term size profiling is incompatible with thread safety.",
        (svar_term_size_prof `being` svalue_term_size_prof_cells) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_c_no])
    ),
    requirement_spec(
        "Term size profiling is incompatible with thread safety.",
        (svar_term_size_prof `being` svalue_term_size_prof_words) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_c_no])
    ),

% Requirements of values of svar_debug.
    requirement_spec(
        "Debugging requires the LLDS backend.",
        (svar_debug `being` svalue_debug_debug) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "Declarative debugging requires the LLDS backend.",
        (svar_debug `being` svalue_debug_decldebug) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "Debugging does not work for multithreaded programs.",
        (svar_debug `being` svalue_debug_debug) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_c_no])
    ),
    requirement_spec(
        "Declarative debugging does not work for multithreaded programs.",
        (svar_debug `being` svalue_debug_decldebug) `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_c_no])
    ),

% Requirements of values of svar_ssdebug.
    requirement_spec(
        "Source-to-source debugging does not make sense for the LLDS backend.",
        (svar_ssdebug `being` svalue_ssdebug_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_mlds, svalue_backend_elds])
    ),
    requirement_spec(
        "Source-to-source debugging does not work for multithreaded programs.",
        (svar_ssdebug `being` svalue_ssdebug_yes) `implies_that`
        (svar_thread_safe `is_one_of`
            [svalue_thread_safe_c_no, svalue_thread_safe_target_native])
    ),

% Requirements of values of svar_target_debug.
    % None. It should be applicable in all cases.

% Requirements of values of svar_rbmm.
    requirement_spec(
        "Region based memory management requires the LLDS backend.",
        (svar_rbmm `being` svalue_rbmm_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),

% Requirements of values of svar_request_single_prec_float.
    requirement_spec(
        "Single precision floats are available only when targeting C.",
        (svar_request_single_prec_float `being`
            svalue_request_single_prec_float_yes)
            `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),
    requirement_spec(
        "Single precision floats are available when requested.",
        % Since nothing forbids svalue_merc_float_is_unboxed_c_float,
        % this implication should always succeed.
        (svar_request_single_prec_float `being`
            svalue_request_single_prec_float_yes)
            `implies_that`
        (svar_merc_float `is_one_of` [svalue_merc_float_is_unboxed_c_float])
    ),

% Requirements of values of svar_merc_float.
    requirement_spec(
        "Unboxed double precision floats require pointer-sized doubles.",
        (svar_merc_float `being` svalue_merc_float_is_unboxed_c_double)
            `implies_that`
        (svar_ac_size_of_double `is_one_of` [svalue_ac_size_of_double_eq_ptr])
    )
].

%---------------------------------------------------------------------------%
:- end_module grade_lib.grade_spec.
%---------------------------------------------------------------------------%
