%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_spec.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

:- type solver_var_id
    --->    svar_backend
    ;       svar_data_level
    ;       svar_target
    ;       svar_nested_funcs
    ;       svar_gcc_regs_avail
    ;       svar_gcc_regs_use
    ;       svar_gcc_gotos_avail
    ;       svar_gcc_gotos_use
    ;       svar_gcc_labels_avail
    ;       svar_gcc_labels_use
    ;       svar_stack_len
    ;       svar_trail
    ;       svar_trail_segments
    ;       svar_minimal_model
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
    ;       svar_single_prec_float.

:- type solver_var_value_id
    --->    svalue_backend_mlds
    ;       svalue_backend_llds
    ;       svalue_backend_elds

    ;       svalue_data_level_hld
    ;       svalue_data_level_lld

    ;       svalue_target_c
    ;       svalue_target_csharp
    ;       svalue_target_java
    ;       svalue_target_erlang

    ;       svalue_nested_funcs_no
    ;       svalue_nested_funcs_yes

    ;       svalue_gcc_regs_avail_no
    ;       svalue_gcc_regs_avail_yes

    ;       svalue_gcc_regs_use_no
    ;       svalue_gcc_regs_use_yes

    ;       svalue_gcc_gotos_avail_no
    ;       svalue_gcc_gotos_avail_yes

    ;       svalue_gcc_gotos_use_no
    ;       svalue_gcc_gotos_use_yes

    ;       svalue_gcc_labels_avail_no
    ;       svalue_gcc_labels_avail_yes

    ;       svalue_gcc_labels_use_no
    ;       svalue_gcc_labels_use_yes

    ;       svalue_stack_len_std
    ;       svalue_stack_len_segments
    ;       svalue_stack_len_extend

    ;       svalue_trail_no
    ;       svalue_trail_yes

    ;       svalue_trail_segments_no
    ;       svalue_trail_segments_yes

    ;       svalue_minimal_model_no
    ;       svalue_minimal_model_yes_stack_copy
    % ;     svalue_minimal_model_yes_stack_copy_debug   % not for general use
    % ;     svalue_minimal_model_yes_own_stack          % not finished yet

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

    ;       svalue_single_prec_float_no
    ;       svalue_single_prec_float_yes.

:- pred solver_var_name(string, solver_var_id).
:- mode solver_var_name(in, out) is semidet.
:- mode solver_var_name(out, in) is det.

:- pred solver_var_value_name(string, solver_var_value_id).
:- mode solver_var_value_name(in, out) is semidet.
:- mode solver_var_value_name(out, in) is det.

%---------------------------------------------------------------------------%

:- type solver_var_spec
    --->    solver_var_spec(
                svs_var                     :: solver_var_id,
                svs_values                  :: list(solver_var_value_id)
            ).

:- func init_solver_var_specs = list(solver_var_spec).

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

solver_var_name("backend",                          svar_backend).
solver_var_name("data_level",                       svar_data_level).
solver_var_name("target",                           svar_target).
solver_var_name("nested_funcs",                     svar_nested_funcs).
solver_var_name("gcc_regs_avail",                   svar_gcc_regs_avail).
solver_var_name("gcc_regs_use",                     svar_gcc_regs_use).
solver_var_name("gcc_gotos_avail",                  svar_gcc_gotos_avail).
solver_var_name("gcc_gotos_use",                    svar_gcc_gotos_use).
solver_var_name("gcc_labels_avail",                 svar_gcc_labels_avail).
solver_var_name("gcc_labels_use",                   svar_gcc_labels_use).
solver_var_name("stack_len",                        svar_stack_len).
solver_var_name("trail",                            svar_trail).
solver_var_name("trail_segments",                   svar_trail_segments).
solver_var_name("minimal_model",                    svar_minimal_model).
solver_var_name("thread_safe",                      svar_thread_safe).
solver_var_name("gc",                               svar_gc).
solver_var_name("deep_prof",                        svar_deep_prof).
solver_var_name("mprof_call",                       svar_mprof_call).
solver_var_name("mprof_time",                       svar_mprof_time).
solver_var_name("mprof_memory",                     svar_mprof_memory).
solver_var_name("tscope_prof",                      svar_tscope_prof).
solver_var_name("term_size_prof",                   svar_term_size_prof).
solver_var_name("debug",                            svar_debug).
solver_var_name("ssdebug",                          svar_ssdebug).
solver_var_name("lldebug",                          svar_lldebug).
solver_var_name("rbmm",                             svar_rbmm).
solver_var_name("rbmm_debug",                       svar_rbmm_debug).
solver_var_name("rbmm_prof",                        svar_rbmm_prof).
solver_var_name("single_prec_float",                svar_single_prec_float).

solver_var_value_name("mlds",                       svalue_backend_mlds).
solver_var_value_name("llds",                       svalue_backend_llds).
solver_var_value_name("elds",                       svalue_backend_elds).

solver_var_value_name("hld",                        svalue_data_level_hld).
solver_var_value_name("lld",                        svalue_data_level_lld).

solver_var_value_name("c",                          svalue_target_c).
solver_var_value_name("csharp",                     svalue_target_csharp).
solver_var_value_name("java",                       svalue_target_java).
solver_var_value_name("erlang",                     svalue_target_erlang).

solver_var_value_name("no_nest",                    svalue_nested_funcs_no).
solver_var_value_name("nest",                       svalue_nested_funcs_yes).

solver_var_value_name("gcc_regs_not_avail",         svalue_gcc_regs_avail_no).
solver_var_value_name("gcc_regs_avail",             svalue_gcc_regs_avail_yes).

solver_var_value_name("dont_use_gcc_regs",          svalue_gcc_regs_use_no).
solver_var_value_name("use_gcc_regs",               svalue_gcc_regs_use_yes).

solver_var_value_name("gcc_gotos_not_avail",        svalue_gcc_gotos_avail_no).
solver_var_value_name("gcc_gotos_avail",            svalue_gcc_gotos_avail_yes).

solver_var_value_name("dont_use_gcc_gotos",         svalue_gcc_gotos_use_no).
solver_var_value_name("use_gcc_gotos",              svalue_gcc_gotos_use_yes).

solver_var_value_name("gcc_labels_not_avail",       svalue_gcc_labels_avail_no).
solver_var_value_name("gcc_labels_avail",       svalue_gcc_labels_avail_yes).

solver_var_value_name("dont_use_gcc_labels",        svalue_gcc_labels_use_no).
solver_var_value_name("use_gcc_labels",             svalue_gcc_labels_use_yes).

solver_var_value_name("stfix",                      svalue_stack_len_std).
solver_var_value_name("stseg",                      svalue_stack_len_segments).
solver_var_value_name("exts",                       svalue_stack_len_extend).

solver_var_value_name("no_trail",                   svalue_trail_no).
solver_var_value_name("trail",                      svalue_trail_yes).

solver_var_value_name("trfix",                      svalue_trail_segments_no).
solver_var_value_name("trseg",                      svalue_trail_segments_yes).

solver_var_value_name("no_mm",                      svalue_minimal_model_no).
solver_var_value_name("mm_stack_copy",  svalue_minimal_model_yes_stack_copy).

solver_var_value_name("not_thread_safe",            svalue_thread_safe_no).
solver_var_value_name("thread_safe",                svalue_thread_safe_yes).

solver_var_value_name("no_gc",                      svalue_gc_none).
solver_var_value_name("bdw",                        svalue_gc_bdw).
solver_var_value_name("bdw_debug",                  svalue_gc_bdw_debug).
solver_var_value_name("target_native",              svalue_gc_target_native).
solver_var_value_name("accurate",                   svalue_gc_accurate).
solver_var_value_name("history",                    svalue_gc_history).

solver_var_value_name("no_deep_prof",               svalue_deep_prof_no).
solver_var_value_name("deep_prof",                  svalue_deep_prof_yes).

solver_var_value_name("no_mprof_call",              svalue_mprof_call_no).
solver_var_value_name("mprof_call",                 svalue_mprof_call_yes).

solver_var_value_name("no_mprof_time",              svalue_mprof_time_no).
solver_var_value_name("mprof_time",                 svalue_mprof_time_yes).

solver_var_value_name("no_mprof_memory",            svalue_mprof_memory_no).
solver_var_value_name("mprof_memory",               svalue_mprof_memory_yes).

solver_var_value_name("no_tscope_prof",             svalue_tscope_prof_no).
solver_var_value_name("tscope_prof",                svalue_tscope_prof_yes).

solver_var_value_name("no_term_size_prof",          svalue_term_size_prof_no).
solver_var_value_name("term_size_prof_cells",   svalue_term_size_prof_cells).
solver_var_value_name("term_size_prof_words",   svalue_term_size_prof_words).

solver_var_value_name("nodebug",                    svalue_debug_none).
solver_var_value_name("debug",                      svalue_debug_debug).
solver_var_value_name("decldebug",                  svalue_debug_decldebug).

solver_var_value_name("no_ssdebug",                 svalue_ssdebug_no).
solver_var_value_name("ssdebug",                    svalue_ssdebug_yes).

solver_var_value_name("no_lldebug",                 svalue_lldebug_no).
solver_var_value_name("lldebug",                    svalue_lldebug_yes).

solver_var_value_name("no_rbmm",                    svalue_rbmm_no).
solver_var_value_name("rbmm",                       svalue_rbmm_yes).

solver_var_value_name("no_rbmm_debug",              svalue_rbmm_debug_no).
solver_var_value_name("rbmm_debug",                 svalue_rbmm_debug_yes).

solver_var_value_name("no_rbmm_prof",               svalue_rbmm_prof_no).
solver_var_value_name("rbmm_prof",                  svalue_rbmm_prof_yes).

solver_var_value_name("no_spf",                 svalue_single_prec_float_no).
solver_var_value_name("spf",                    svalue_single_prec_float_yes).

%---------------------------------------------------------------------------%

init_solver_var_specs = [
    % The order of solver variables here is the order in which labeling
    % will try to choose a variable to bind.
    %
    % The order of values within each solver variables is the order of
    % preference: labeling will always choose to set the chosen solver variable
    % to the first of its values that has not previously been rules out.
    %
    % Some of the value names are grade components. Many are not, and will
    % need tranlation (e.g. via a lookup table) into a grade component.
    % This is inevitable if we don't want a lot of value names to be "".
    %
    % The first two fields of the solver_var structures, representing the
    % total number of values and the number of still possible values,
    % are just placeholders here; the real values will be filled in
    % by setup_solver_vars, which will count the entries in the lists of
    % solver_var_values.

    solver_var_spec(svar_backend,
        [svalue_backend_mlds, svalue_backend_llds, svalue_backend_elds]),
    solver_var_spec(svar_data_level,
        [svalue_data_level_lld, svalue_data_level_hld]),
    solver_var_spec(svar_target,
        [svalue_target_c, svalue_target_csharp,
        svalue_target_java, svalue_target_erlang]),
    solver_var_spec(svar_nested_funcs,
        [svalue_nested_funcs_no, svalue_nested_funcs_yes]),

    solver_var_spec(svar_gcc_regs_avail,
        [svalue_gcc_regs_avail_no, svalue_gcc_regs_avail_yes]),
    solver_var_spec(svar_gcc_regs_use,
        [svalue_gcc_regs_use_yes, svalue_gcc_regs_use_no]),
    solver_var_spec(svar_gcc_gotos_avail,
        [svalue_gcc_gotos_avail_no, svalue_gcc_gotos_avail_yes]),
    solver_var_spec(svar_gcc_gotos_use,
        [svalue_gcc_gotos_use_yes, svalue_gcc_gotos_use_no]),
    solver_var_spec(svar_gcc_labels_avail,
        [svalue_gcc_labels_avail_no, svalue_gcc_labels_avail_yes]),
    solver_var_spec(svar_gcc_labels_use,
        [svalue_gcc_labels_use_yes, svalue_gcc_labels_use_no]),

    solver_var_spec(svar_stack_len,
        [svalue_stack_len_segments, svalue_stack_len_std,
        svalue_stack_len_extend]),
    solver_var_spec(svar_trail,
        [svalue_trail_no, svalue_trail_yes]),
    solver_var_spec(svar_trail_segments,
        [svalue_trail_segments_yes, svalue_trail_segments_no]),
    solver_var_spec(svar_minimal_model,
        [svalue_minimal_model_no, svalue_minimal_model_yes_stack_copy]),
    solver_var_spec(svar_thread_safe,
        [svalue_thread_safe_no, svalue_thread_safe_yes]),
    solver_var_spec(svar_gc,
        [svalue_gc_bdw, svalue_gc_target_native, svalue_gc_accurate,
        svalue_gc_bdw_debug, svalue_gc_none, svalue_gc_history]),
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
    solver_var_spec(svar_single_prec_float,
        [svalue_single_prec_float_no, svalue_single_prec_float_yes])
].

%---------------------------------------------------------------------------%

init_requirement_specs = [
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
        "ELDS backend requires targeting Erlang",
        (svar_backend `being` svalue_backend_elds) `implies_that`
        (svar_target `is_one_of` [svalue_target_erlang])
    ),

% Requirements of values of svar_data_level.
    requirement_spec(
        "high level data requires high level code",
        (svar_data_level `being` svalue_data_level_hld) `implies_that`
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
        "targeting C# requires high level data",
        (svar_target `being` svalue_target_csharp) `implies_that`
        (svar_data_level `is_one_of` [svalue_data_level_hld])
    ),
    requirement_spec(
        "targeting Java requires high level data",
        (svar_target `being` svalue_target_java) `implies_that`
        (svar_data_level `is_one_of` [svalue_data_level_hld])
    ),
    requirement_spec(
        "targeting Erlang requires low level data",
        (svar_target `being` svalue_target_erlang) `implies_that`
        (svar_data_level `is_one_of` [svalue_data_level_lld])
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
        "targeting C# is incompatible with trailing",
        (svar_target `being` svalue_target_csharp) `implies_that`
        (svar_trail `is_one_of` [svalue_trail_no])
    ),
    requirement_spec(
        "targeting Java is incompatible with trailing",
        (svar_target `being` svalue_target_java) `implies_that`
        (svar_trail `is_one_of` [svalue_trail_no])
    ),
    requirement_spec(
        "targeting Erlang is incompatible with trailing",
        (svar_target `being` svalue_target_erlang) `implies_that`
        (svar_trail `is_one_of` [svalue_trail_no])
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

% Requirements of values of svar_gcc_regs_avail.
    % None. The value is set by configure.

% Requirements of values of svar_gcc_regs_use.
    requirement_spec(
        "using gcc regs requires them to be available",
        (svar_gcc_regs_use `being` svalue_gcc_regs_use_yes) `implies_that`
        (svar_gcc_regs_avail `is_one_of` [svalue_gcc_regs_avail_yes])
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

% Requirements of values of svar_gcc_gotos_avail.
    % None. The value is set by configure.

% Requirements of values of svar_gcc_gotos_use.
    requirement_spec(
        "using gcc nonlocal gotos requires them to be available",
        (svar_gcc_gotos_use `being` svalue_gcc_gotos_use_yes) `implies_that`
        (svar_gcc_gotos_avail `is_one_of` [svalue_gcc_gotos_avail_yes])
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

% Requirements of values of svar_gcc_labels_avail.
    % None. The value is set by configure.

% Requirements of values of svar_gcc_labels_use.
    requirement_spec(
        "using gcc asm labels requires them to be available",
        (svar_gcc_labels_use `being` svalue_gcc_labels_use_yes) `implies_that`
        (svar_gcc_labels_avail `is_one_of` [svalue_gcc_labels_avail_yes])
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
        "trailing interferes with minimal model tabling",
        (svar_trail `being` svalue_trail_yes) `implies_that`
        (svar_minimal_model `is_one_of` [svalue_minimal_model_no])
    ),

% Requirements of values of svar_trail_segments.
    requirement_spec(
        "trail segments require trailing",
        (svar_trail_segments `being` svalue_trail_segments_yes) `implies_that`
        (svar_trail `is_one_of` [svalue_trail_yes])
    ),
    requirement_spec(
        "trail segments require the LLDS backend",
        (svar_trail_segments `being` svalue_trail_segments_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),

% Requirements of values of svar_minimal_model.
    requirement_spec(
        "minimal model tabling requires the LLDS backend",
        (svar_minimal_model `being` svalue_minimal_model_yes_stack_copy)
            `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "minimal model tabling requires boehm-demers-weiser gc",
        (svar_minimal_model `being` svalue_minimal_model_yes_stack_copy)
            `implies_that`
        (svar_gc `is_one_of` [svalue_gc_bdw, svalue_gc_bdw_debug])
    ),
    requirement_spec(
        "minimal model tabling does not respect thread safety",
        (svar_minimal_model `being` svalue_minimal_model_yes_stack_copy)
            `implies_that`
        (svar_thread_safe `is_one_of` [svalue_thread_safe_no])
    ),

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
    requirement_spec(
        "history gc requires the MLDS backend",
        (svar_gc `being` svalue_gc_history) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_mlds])
    ),

% Requirements of values of svar_deep_prof.
    requirement_spec(
        "deep_profiling requires the LLDS backend",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "deep_profiling is incompatible with mprof call profiling",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_mprof_call `is_one_of` [svalue_mprof_call_no])
    ),
    requirement_spec(
        "deep_profiling is incompatible with mprof time profiling",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_mprof_time `is_one_of` [svalue_mprof_time_no])
    ),
    requirement_spec(
        "deep_profiling is incompatible with mprof memory profiling",
        (svar_deep_prof `being` svalue_deep_prof_yes) `implies_that`
        (svar_mprof_memory `is_one_of` [svalue_mprof_memory_no])
    ),

% Requirements of values of svar_mprof_call.
    requirement_spec(
        "mprof call profiling requires the LLDS backend",
        (svar_mprof_call `being` svalue_mprof_call_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "mprof call profiling requires the LLDS backend",
        % XXX is this correct?
        (svar_mprof_call `being` svalue_mprof_call_yes) `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    ),

% Requirements of values of svar_mprof_time.
    requirement_spec(
        "mprof time profiling requires the LLDS backend",
        (svar_mprof_time `being` svalue_mprof_time_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
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
        "mprof memory profiling requires the LLDS backend",
        (svar_mprof_memory `being` svalue_mprof_memory_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),
    requirement_spec(
        "mprof memory profiling requires targeting C",
        % XXX is this correct?
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
        "low level debugging requires the LLDS backend",
        (svar_lldebug `being` svalue_lldebug_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),

% Requirements of values of svar_rbmm.
    requirement_spec(
        "region based memory management requires the LLDS backend",
        (svar_rbmm `being` svalue_rbmm_yes) `implies_that`
        (svar_backend `is_one_of` [svalue_backend_llds])
    ),

% Requirements of values of svar_single_prec_float.
    requirement_spec(
        "single precision floats requires targeting C",
        (svar_single_prec_float `being` svalue_single_prec_float_yes)
            `implies_that`
        (svar_target `is_one_of` [svalue_target_c])
    )
].

%---------------------------------------------------------------------------%
:- end_module grade_spec.
%---------------------------------------------------------------------------%
