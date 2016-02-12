%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_spec.
:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

:- type solver_var_map == map(solver_var_name, solver_var).

:- type solver_var_name
    --->    sv_name(string).

:- type solver_var_value_name
    --->    svv_name(string).

:- type solver_var
    --->    solver_var(
                sv_cnt_all              :: int,
                sv_cnt_possible         :: int,
                sv_values               :: list(solver_var_value)
            ).

:- type solver_var_value
    --->    solver_var_value(
                svv_name                :: solver_var_value_name,
                svv_is_possible         :: solver_var_value_possible
            ).

:- type solver_var_value_possible
    --->    is_possible
    ;       not_possible(
                np_why                  :: not_possible_why
            ).

:- type not_possible_why
    --->    npw_config
    ;       npw_user
    ;       npw_requirement(
                npwr_req_id             :: requirement_id
            )
    ;       npw_labeling.

:- func init_solver_vars = assoc_list(solver_var_name, solver_var).

%---------------------------------------------------------------------------%

:- type requirement_id
    --->    requirement_id(int).

:- type requirement
    --->    requirement(
                req_id                  :: requirement_id,
                req_desc                :: string,
                req_if_solver_var       :: solver_var_name,
                req_if_value            :: solver_var_value_name,
                req_then_solver_var     :: solver_var_name,
                req_then_values         :: list(solver_var_value_name)
            ).

:- func init_requirements = list(requirement).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module pair.

%---------------------------------------------------------------------------%

init_solver_vars = [
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

    sv_name("backend") - solver_var(0, 0, [
        solver_var_value(svv_name("mlds"), is_possible),
        solver_var_value(svv_name("llds"), is_possible),
        solver_var_value(svv_name("elds"), is_possible)
    ]),
    sv_name("data_level") - solver_var(0, 0, [
        solver_var_value(svv_name("hld"), is_possible),
        solver_var_value(svv_name("lld"), is_possible)
    ]),
    sv_name("target") - solver_var(0, 0, [
        solver_var_value(svv_name("c"), is_possible),
        solver_var_value(svv_name("csharp"), is_possible),
        solver_var_value(svv_name("java"), is_possible),
        solver_var_value(svv_name("erlang"), is_possible)
    ]),
    sv_name("nested_funcs") - solver_var(0, 0, [
        solver_var_value(svv_name("no_nest"), is_possible),
        solver_var_value(svv_name("nest"), is_possible)
    ]),
    sv_name("avail_gcc_regs") - solver_var(0, 0, [
        solver_var_value(svv_name("gcc_regs_not_avail"), is_possible),
        solver_var_value(svv_name("gcc_regs_avail"), is_possible)
    ]),
    sv_name("use_gcc_regs") - solver_var(0, 0, [
        solver_var_value(svv_name("use_gcc_regs"), is_possible),
        solver_var_value(svv_name("dont_use_gcc_regs"), is_possible)
    ]),
    sv_name("avail_gcc_nonlocal_gotos") - solver_var(0, 0, [
        solver_var_value(svv_name("gcc_nonlocal_gotos_not_avail"), is_possible),
        solver_var_value(svv_name("gcc_nonlocal_gotos_avail"), is_possible)
    ]),
    sv_name("use_gcc_nonlocal_gotos") - solver_var(0, 0, [
        solver_var_value(svv_name("use_gcc_nonlocal_gotos"), is_possible),
        solver_var_value(svv_name("dont_use_gcc_nonlocal_gotos"), is_possible)
    ]),
    sv_name("avail_gcc_asm_labels") - solver_var(0, 0, [
        solver_var_value(svv_name("gcc_asm_labels_not_avail"), is_possible),
        solver_var_value(svv_name("gcc_asm_labels_avail"), is_possible)
    ]),
    sv_name("use_gcc_asm_labels") - solver_var(0, 0, [
        solver_var_value(svv_name("use_gcc_asm_labels"), is_possible),
        solver_var_value(svv_name("dont_use_gcc_asm_labels"), is_possible)
    ]),
    sv_name("stack_segments") - solver_var(0, 0, [
        solver_var_value(svv_name("stseg"), is_possible),
        solver_var_value(svv_name("stfix"), is_possible)
    ]),
    sv_name("trail") - solver_var(0, 0, [
        solver_var_value(svv_name("no_trail"), is_possible),
        solver_var_value(svv_name("trail"), is_possible)
    ]),
    sv_name("trail_segments") - solver_var(0, 0, [
        solver_var_value(svv_name("trfix"), is_possible),
        solver_var_value(svv_name("trseg"), is_possible)
    ]),
    sv_name("minimal_model") - solver_var(0, 0, [
        solver_var_value(svv_name("no_mm"), is_possible),
        solver_var_value(svv_name("mm_stack_copy"), is_possible)
        % The debug version of mm_stack_copy is not for general use.
        % The mm_own_stack implementation of minimal model tabling
        % is not finished.
    ]),
    sv_name("thread_safe") - solver_var(0, 0, [
        solver_var_value(svv_name("not_thread_safe"), is_possible),
        solver_var_value(svv_name("thread_safe"), is_possible)
    ]),
    sv_name("gc") - solver_var(0, 0, [
        solver_var_value(svv_name("bdw"), is_possible),
        solver_var_value(svv_name("target_native"), is_possible),
        solver_var_value(svv_name("accurate"), is_possible),
        solver_var_value(svv_name("bdw_debug"), is_possible),
        solver_var_value(svv_name("no_gc"), is_possible),
        solver_var_value(svv_name("history"), is_possible)
    ]),
    sv_name("deep_prof") - solver_var(0, 0, [
        solver_var_value(svv_name("no_deep_prof"), is_possible),
        solver_var_value(svv_name("deep_prof"), is_possible)
    ]),
    sv_name("mprof_call") - solver_var(0, 0, [
        solver_var_value(svv_name("no_mprof_call"), is_possible),
        solver_var_value(svv_name("mprof_call"), is_possible)
    ]),
    sv_name("mprof_time") - solver_var(0, 0, [
        solver_var_value(svv_name("no_mprof_time"), is_possible),
        solver_var_value(svv_name("mprof_time"), is_possible)
    ]),
    sv_name("mprof_memory") - solver_var(0, 0, [
        solver_var_value(svv_name("no_mprof_memory"), is_possible),
        solver_var_value(svv_name("mprof_memory"), is_possible)
    ]),
    sv_name("tscope_prof") - solver_var(0, 0, [
        % Paul wants to call the threadscope style profiler a different name,
        % since the data it generates isn't compatible with threadscope
        % anymore. But the name we use here in the solver isn't visible
        % outside, and "tscope" gives readers the right intuition.
        solver_var_value(svv_name("no_tscope_prof"), is_possible),
        solver_var_value(svv_name("tscope_prof"), is_possible)
    ]),
    sv_name("term_size_prof") - solver_var(0, 0, [
        solver_var_value(svv_name("no_term_size_prof"), is_possible),
        solver_var_value(svv_name("term_size_prof_cells"), is_possible),
        solver_var_value(svv_name("term_size_prof_words"), is_possible)
    ]),
    sv_name("debug") - solver_var(0, 0, [
        solver_var_value(svv_name("nodebug"), is_possible),
        solver_var_value(svv_name("debug"), is_possible),
        solver_var_value(svv_name("decldebug"), is_possible)
        % The source-to-source debugger is not yet mature enough
        % to be generally useful. When it IS mature enough,
        % it could be added here as an alternative to debug and decldebug,
        % or it could be implemented as a solver variable of its own
        % that requires debug=nodebug. (Giving the user TWO debuggers to
        % interact with at the same time is not a good idea.)
    ]),
    sv_name("single_prec_float") - solver_var(0, 0, [
        solver_var_value(svv_name("no_spf"), is_possible),
        solver_var_value(svv_name("spf"), is_possible)
    ])
].

%---------------------------------------------------------------------------%

init_requirements = [
    % The requirement ids are just placeholders; the real values will be
    % filled in by setup_requirements. This avoid requiring renumbering
    % the requirements, like statement numbers in Basic.

% Requirements of settings of the backend solver var.
    requirement(requirement_id(0),
        "MLDS backend requires targeting C, C# or Java",
        sv_name("backend"), svv_name("mlds"),
        sv_name("target"),
            [svv_name("c"), svv_name("csharp"), svv_name("java")]
    ),
    requirement(requirement_id(0),
        "LLDS backend requires targeting C",
        sv_name("backend"), svv_name("llds"),
        sv_name("target"), [svv_name("c")]
    ),
    requirement(requirement_id(0),
        "ELDS backend requires targeting Erlang",
        sv_name("backend"), svv_name("elds"),
        sv_name("target"), [svv_name("erlang")]
    ),

% Requirements of settings of the data_level solver var.
    requirement(requirement_id(0),
        "high level data requires high level code",
        sv_name("data_level"), svv_name("hld"),
        sv_name("backend"), [svv_name("mlds")]
    ),

% Requirements of settings of the target solver var.
    requirement(requirement_id(0),
        "targeting C# requires the MLDS backend",
        sv_name("target"), svv_name("csharp"),
        sv_name("backend"), [svv_name("mlds")]
    ),
    requirement(requirement_id(0),
        "targeting Java requires the MLDS backend",
        sv_name("target"), svv_name("java"),
        sv_name("backend"), [svv_name("mlds")]
    ),
    requirement(requirement_id(0),
        "targeting Erlang requires the ELDS backend",
        sv_name("target"), svv_name("erlang"),
        sv_name("backend"), [svv_name("elds")]
    ),
    requirement(requirement_id(0),
        "targeting C# requires high level data",
        sv_name("target"), svv_name("csharp"),
        sv_name("data_level"), [svv_name("hld")]
    ),
    requirement(requirement_id(0),
        "targeting Java requires high level data",
        sv_name("target"), svv_name("java"),
        sv_name("data_level"), [svv_name("hld")]
    ),
    requirement(requirement_id(0),
        "targeting Erlang requires low level data",
        sv_name("target"), svv_name("erlang"),
        sv_name("data_level"), [svv_name("lld")]
    ),
    requirement(requirement_id(0),
        "targeting C# requires target native gc",
        sv_name("target"), svv_name("csharp"),
        sv_name("gc"), [svv_name("target_native")]
    ),
    requirement(requirement_id(0),
        "targeting Java requires target native gc",
        sv_name("target"), svv_name("java"),
        sv_name("gc"), [svv_name("target_native")]
    ),
    requirement(requirement_id(0),
        "targeting Erlang requires target native gc",
        sv_name("target"), svv_name("erlang"),
        sv_name("gc"), [svv_name("target_native")]
    ),

% Requirements of settings of the nested_funcs solver var.
    requirement(requirement_id(0),
        "using gcc nested functions requires the MLDS backend",
        sv_name("nested_funcs"), svv_name("nest"),
        sv_name("backend"), [svv_name("mlds")]
    ),
    requirement(requirement_id(0),
        "using gcc nested functions requires targeting C",
        sv_name("nested_funcs"), svv_name("nest"),
        sv_name("target"), [svv_name("c")]
    ),

% Requirements of settings of the avail_gcc_regs solver var.
    % None. The value of this solver is intended to be given by configure.

% Requirements of settings of the use_gcc_regs solver var.
    requirement(requirement_id(0),
        "using gcc regs requires them to be available",
        sv_name("use_gcc_regs"), svv_name("use_gcc_regs"),
        sv_name("avail_gcc_regs"), [svv_name("gcc_regs_avail")]
    ),
    requirement(requirement_id(0),
        "using gcc regs requires them to be available",
        sv_name("use_gcc_regs"), svv_name("use_gcc_regs"),
        sv_name("backend"), [svv_name("llds")]
    ),

% Requirements of settings of the avail_gcc_nonlocal_gotos solver var.
    % None. The value of this solver is intended to be given by configure.

% Requirements of settings of the use_gcc_nonlocal_gotos solver var.
    requirement(requirement_id(0),
        "using gcc nonlocal gotos requires them to be available",
        sv_name("use_gcc_nonlocal_gotos"), svv_name("use_gcc_nonlocal_gotos"),
        sv_name("avail_gcc_nonlocal_gotos"),
            [svv_name("gcc_nonlocal_gotos_avail")]
    ),
    requirement(requirement_id(0),
        "using gcc nonlocal gotos requires them to be available",
        sv_name("use_gcc_nonlocal_gotos"), svv_name("use_gcc_nonlocal_gotos"),
        sv_name("backend"), [svv_name("llds")]
    ),

% Requirements of settings of the avail_gcc_asm_labels solver var.
    % None. The value of this solver is intended to be given by configure.

% Requirements of settings of the use_gcc_asm_labels solver var.
    requirement(requirement_id(0),
        "using gcc asm labels requires them to be available",
        sv_name("use_gcc_asm_labels"), svv_name("use_gcc_asm_labels"),
        sv_name("avail_gcc_asm_labels"),
            [svv_name("gcc_asm_labels_avail")]
    ),
    requirement(requirement_id(0),
        "using gcc asm labels requires them to be available",
        sv_name("use_gcc_asm_labels"), svv_name("use_gcc_asm_labels"),
        sv_name("backend"), [svv_name("llds")]
    ),
    requirement(requirement_id(0),
        "using gcc asm labels requires gcc nonlocal gotos",
        sv_name("use_gcc_asm_labels"), svv_name("use_gcc_asm_labels"),
        sv_name("use_gcc_nonlocal_gotos"), [svv_name("use_gcc_nonlocal_gotos")]
    ),

% Requirements of settings of the stack_segments solver var.
    requirement(requirement_id(0),
        "stack segments require the LLDS backend",
        sv_name("stack_segments"), svv_name("stseg"),
        sv_name("backend"), [svv_name("llds")]
    ),

% Requirements of settings of the trail solver var.
    requirement(requirement_id(0),
        "trailing interferes with minimal model tabling",
        sv_name("trail"), svv_name("trail"),
        sv_name("minimal_model"), [svv_name("no_mm")]
    ),

% Requirements of settings of the trail_segments solver var.
    requirement(requirement_id(0),
        "trail segments require trailing",
        sv_name("trail_segments"), svv_name("trseg"),
        sv_name("trail"), [svv_name("trail")]
    ),
    requirement(requirement_id(0),
        "trail segments require the LLDS backend",
        sv_name("trail_segments"), svv_name("trseg"),
        sv_name("backend"), [svv_name("llds")]
    ),

% Requirements of settings of the minimal_model solver var.
    requirement(requirement_id(0),
        "minimal model tabling requires the LLDS backend",
        sv_name("minimal_model"), svv_name("mm_stack_copy"),
        sv_name("backend"), [svv_name("llds")]
    ),
    requirement(requirement_id(0),
        "minimal model tabling requires boehm-demers-weiser gc",
        sv_name("minimal_model"), svv_name("mm_stack_copy"),
        sv_name("gc"), [svv_name("bdw")]
    ),
    requirement(requirement_id(0),
        "minimal model tabling does not work with thread-safe code",
        sv_name("minimal_model"), svv_name("mm_stack_copy"),
        sv_name("thread_safe"), [svv_name("not_thread_safe")]
    ),

% Requirements of settings of the thread_safe solver var.
    % None.
    % XXX This is probably wrong; there ought to be some.

% Requirements of settings of the gc solver var.
    requirement(requirement_id(0),
        "boehm-demers-weiser gc requires targeting C",
        sv_name("gc"), svv_name("bdw"),
        sv_name("target"), [svv_name("c")]
    ),
    requirement(requirement_id(0),
        "boehm-demers-weiser gc requires targeting C",
        sv_name("gc"), svv_name("bdw_debug"),
        sv_name("target"), [svv_name("c")]
    ),
    requirement(requirement_id(0),
        "accurate gc requires targeting C",
        sv_name("gc"), svv_name("accurate"),
        sv_name("target"), [svv_name("c")]
    ),
    requirement(requirement_id(0),
        "accurate gc requires the MLDS backend",
        sv_name("gc"), svv_name("accurate"),
        sv_name("backend"), [svv_name("mlds")]
    ),
    requirement(requirement_id(0),
        "history gc requires targeting C",
        sv_name("gc"), svv_name("history"),
        sv_name("target"), [svv_name("c")]
    ),
    requirement(requirement_id(0),
        "history gc requires the MLDS backend",
        sv_name("gc"), svv_name("history"),
        sv_name("backend"), [svv_name("mlds")]
    ),

% Requirements of settings of the deep_prof solver var.
    requirement(requirement_id(0),
        "deep_profiling requires the LLDS backend",
        sv_name("deep_prof"), svv_name("deep_prof"),
        sv_name("backend"), [svv_name("llds")]
    ),
    requirement(requirement_id(0),
        "deep_profiling is incompatible with mprof call profiling",
        sv_name("deep_prof"), svv_name("deep_prof"),
        sv_name("mprof_call"), [svv_name("no_mprof_call")]
    ),
    requirement(requirement_id(0),
        "deep_profiling is incompatible with mprof time profiling",
        sv_name("deep_prof"), svv_name("deep_prof"),
        sv_name("mprof_time"), [svv_name("no_mprof_time")]
    ),
    requirement(requirement_id(0),
        "deep_profiling is incompatible with mprof memory profiling",
        sv_name("deep_prof"), svv_name("deep_prof"),
        sv_name("mprof_memory"), [svv_name("no_mprof_memory")]
    ),

% Requirements of settings of the mprof_call solver var.
    requirement(requirement_id(0),
        "mprof call profiling requires the LLDS backend",
        sv_name("mprof_call"), svv_name("mprof_call"),
        sv_name("backend"), [svv_name("llds")]
    ),

% Requirements of settings of the mprof_call solver var.
    requirement(requirement_id(0),
        "mprof call profiling requires targeting C",
        sv_name("mprof_call"), svv_name("mprof_call"),
        sv_name("target"), [svv_name("c")]
    ),

% Requirements of settings of the mprof_memory solver var.
    requirement(requirement_id(0),
        "mprof memory profiling requires targeting C",
        sv_name("mprof_memory"), svv_name("mprof_memory"),
        sv_name("target"), [svv_name("c")]
    ),
    requirement(requirement_id(0),
        "mprof memory profiling requires mprof call profiling",
        sv_name("mprof_memory"), svv_name("mprof_memory"),
        sv_name("mprof_call"), [svv_name("mprof_call")]
    ),

% Requirements of settings of the mprof_time solver var.
    requirement(requirement_id(0),
        % Unlike mprof_call and mprof_memory, mprof_time requires
        % not just targeting C but the LLDS backend.
        % XXX Is this correct?
        "mprof time profiling requires the LLDS backend",
        sv_name("mprof_time"), svv_name("mprof_time"),
        sv_name("backend"), [svv_name("llds")]
    ),
    % XXX runtime/mercury_grade.h allows MR_MPROF_PROFILE_TIME without
    % MR_MPROF_PROFILE_CALLS, but calls the combination "useless".
    requirement(requirement_id(0),
        "mprof time profiling requires mprof call profiling",
        sv_name("mprof_time"), svv_name("mprof_time"),
        sv_name("mprof_call"), [svv_name("mprof_call")]
    ),

% Requirements of settings of the tscope_prof solver var.
    requirement(requirement_id(0),
        "threadscope style profiling requires the LLDS backend",
        sv_name("tscope_prof"), svv_name("tscope_prof"),
        sv_name("backend"), [svv_name("llds")]
    ),
    requirement(requirement_id(0),
        "threadscope style profiling requires thread safe code",
        sv_name("tscope_prof"), svv_name("tscope_prof"),
        sv_name("thread_safe"), [svv_name("thread_safe")]
    ),

% Requirements of settings of the term_size_prof solver var.
    requirement(requirement_id(0),
        "term size profiling requires the LLDS backend",
        sv_name("term_size_prof"), svv_name("term_size_prof_cells"),
        sv_name("backend"), [svv_name("llds")]
    ),
    requirement(requirement_id(0),
        "term size profiling requires the LLDS backend",
        sv_name("term_size_prof"), svv_name("term_size_prof_words"),
        sv_name("backend"), [svv_name("llds")]
    ),

% Requirements of settings of the debug solver var.
    requirement(requirement_id(0),
        "debugging requires the LLDS backend",
        sv_name("debug"), svv_name("debug"),
        sv_name("backend"), [svv_name("llds")]
    ),
    requirement(requirement_id(0),
        "declarative debugging the LLDS backend",
        sv_name("debug"), svv_name("decldebug"),
        sv_name("backend"), [svv_name("llds")]
    ),

% Requirements of settings of the single_prec_float solver var.
    requirement(requirement_id(0),
        "single precision floats requires targeting C",
        sv_name("single_prec_float"), svv_name("spf"),
        sv_name("target"), [svv_name("c")]
    )
].

%---------------------------------------------------------------------------%
:- end_module grade_spec.
%---------------------------------------------------------------------------%
