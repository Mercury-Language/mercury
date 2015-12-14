%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module choose.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.

main(!IO) :-
    setup_solver_vars(SolverVarMap0, PrioSolverVarNames),
    setup_requirements(SolverVarMap0, Requirements),
    io.command_line_arguments(Args, !IO),
    process_arguments(Args, BadArgLines, SolverVarMap0, SolverVarMap1),
    (
        BadArgLines = [_ | _],
        io.write_string("unrecognized arguments:\n", !IO),
        list.foldl(io.write_string, BadArgLines, !IO)
    ;
        BadArgLines = [],
        trace [compile_time(flag("debug_choose_grade")), io(!TIO)] (
            io.write_string("AFTER SETUP\n", !TIO),
            io.write_string(solver_var_map_to_str(SolverVarMap1), !TIO)
        ),
        solve(Requirements, PrioSolverVarNames, SolverVarMap1, 1, Soln),
        io.write_string(soln_to_str(Soln), !IO)
    ).

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

%---------------------------------------------------------------------------%

:- func init_solver_vars = assoc_list(solver_var_name, solver_var).

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
        solver_var_value(svv_name("none"), is_possible),
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
        solver_var_value(svv_name("none"), is_possible),
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
        solver_var_value(svv_name("none"), is_possible),
        solver_var_value(svv_name("debug"), is_possible),
        solver_var_value(svv_name("decldebug"), is_possible)
        % The source-to-source debugger is not yet mature enough
        % to be generally useful. When it IS mature enough,
        % it could be added here as an alternative to debug and decldebug,
        % or it could be implemented as a solver variable of its own
        % that requires debug=none. (Giving the user TWO debuggers to
        % interact with at the same time is not a good idea.)
    ]),
    sv_name("single_prec_float") - solver_var(0, 0, [
        solver_var_value(svv_name("no_spf"), is_possible),
        solver_var_value(svv_name("spf"), is_possible)
    ])
].

:- func init_requirements = list(requirement).

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
        sv_name("minimal_model"), [svv_name("none")]
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

:- pred setup_solver_vars(solver_var_map::out, list(solver_var_name)::out)
    is det.

setup_solver_vars(SolverVarMap, PrioSolverVarNames) :-
    SolverVars0 = init_solver_vars,
    assoc_list.keys(SolverVars0, PrioSolverVarNames),
    init_solver_vars(SolverVars0, map.init, SolverVarMap).

:- pred init_solver_vars(assoc_list(solver_var_name, solver_var)::in,
    solver_var_map::in, solver_var_map::out) is det.

init_solver_vars([], !SolverVarMap).
init_solver_vars([Pair0 | Pairs0], !SolverVarMap) :-
    Pair0 = SolverVarName - SolverVar0,
    SolverVar0 = solver_var(_, _, Values0),
    init_solver_var_values(0, NumValues, Values0, Values),
    SolverVar = solver_var(NumValues, NumValues, Values),
    expect(isnt(unify(0), NumValues), $pred, "no values for solver var"),
    map.det_insert(SolverVarName, SolverVar, !SolverVarMap),
    init_solver_vars(Pairs0, !SolverVarMap).

:- pred init_solver_var_values(int::in, int::out,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

init_solver_var_values(CurNumValues, CurNumValues, [], []).
init_solver_var_values(CurNumValues, NumValues,
        [Value0 | Values0], [Value | Values]) :-
    Value0 = solver_var_value(Name, _),
    Value = solver_var_value(Name, is_possible),
    init_solver_var_values(CurNumValues + 1, NumValues, Values0, Values).

:- pred setup_requirements(solver_var_map::in, list(requirement)::out) is det.

setup_requirements(SolverVarMap, Requirements) :-
    Requirements0 = init_requirements,
    fill_in_and_check_requirements(0, SolverVarMap,
        Requirements0, Requirements, BadRequirements),
    (
        BadRequirements = []
    ;
        BadRequirements = [_ | _],
        trace [io(!IO)] (
            io.write(BadRequirements, !IO)
        ),
        unexpected($pred, "some bad requirements")
    ).

:- pred fill_in_and_check_requirements(int::in, solver_var_map::in,
    list(requirement)::in, list(requirement)::out, list(requirement)::out)
    is det.

fill_in_and_check_requirements(_CurId, _SolverVarMap, [], [], []).
fill_in_and_check_requirements(CurId, SolverVarMap,
        [Requirement0 | Requirements0], OkRequirements, BadRequirements) :-
    fill_in_and_check_requirements(CurId + 1, SolverVarMap, Requirements0,
        OkRequirementsTail, BadRequirementsTail),
    Requirement0 = requirement(_, ReqDesc,
        IfVar, IfValue, ThenVar, ThenValues),
    Requirement = requirement(requirement_id(CurId), ReqDesc,
        IfVar, IfValue, ThenVar, ThenValues),
    ( if
        % Both variable names in the requirement must be in SolverMap,
        % and all the value names must be in the SolverMap entries
        % of their respective variables. If they are not, the requirement
        % does not make sense.
        known_var_values(SolverVarMap, IfVar, [IfValue]),
        known_var_values(SolverVarMap, ThenVar, ThenValues),

        % Requirement represents the implication:
        %
        %   (IfVar = IfValue) -> (ThenVar in ThenValues)
        %
        % If ThenValues = [], then the requirement is unsatisfiable.
        ThenValues = [_ | _]
    then
        OkRequirements = [Requirement | OkRequirementsTail],
        BadRequirements = BadRequirementsTail
    else
        OkRequirements = OkRequirementsTail,
        BadRequirements = [Requirement | BadRequirementsTail]
    ).

:- pred known_var_values(solver_var_map::in,
    solver_var_name::in, list(solver_var_value_name)::in) is semidet.

known_var_values(SolverVarMap, SearchVarName, SearchValueNames) :-
    map.search(SolverVarMap, SearchVarName, SolverVar),
    SolverVar = solver_var(_, _, VarValues),
    set.list_to_set(SearchValueNames, SearchSet),
    ValueNames = list.map(solver_var_value_project_name, VarValues),
    set.list_to_set(ValueNames, Set),
    set.subset(SearchSet, Set).

:- func solver_var_value_project_name(solver_var_value)
    = solver_var_value_name.

solver_var_value_project_name(SolverVarValue) = SolverVarValue ^ svv_name.

%---------------------------------------------------------------------------%

:- pred process_arguments(list(string)::in, list(string)::out,
    solver_var_map::in, solver_var_map::out) is det.

process_arguments([], [], !SolverVarMap).
process_arguments([Arg | Args], BadArgLines, !SolverVarMap) :-
    ( if string.remove_prefix("CONFIG", Arg, ArgSuffixPrime) then
        WhyNot = npw_config,
        ArgSuffix = ArgSuffixPrime
    else
        WhyNot = npw_user,
        ArgSuffix = Arg
    ),
    ( if
        NeqComponents = string.split_at_string("!=", ArgSuffix),
        NeqComponents = [VarStr, ValueStr]
    then
        VarName = sv_name(VarStr),
        ValueName = svv_name(ValueStr),
        ( if
            map.search(!.SolverVarMap, VarName, SolverVar0),
            SolverVar0 = solver_var(CntAll, CntPoss0, Values0),
            set_value_to_false(WhyNot, ValueName, Values0, Values)
        then
            IsBad = no,
            CntPoss = CntPoss0 - 1,
            SolverVar = solver_var(CntAll, CntPoss, Values),
            map.det_update(VarName, SolverVar, !SolverVarMap)
        else
            IsBad = yes
        )
    else if
        EqComponents = string.split_at_string("=", ArgSuffix),
        EqComponents = [VarStr, ValueStr]
    then
        VarName = sv_name(VarStr),
        ValueName = svv_name(ValueStr),
        ( if
            map.search(!.SolverVarMap, VarName, SolverVar0),
            SolverVar0 = solver_var(CntAll, _CntPoss0, Values0),
            set_value_to_true(WhyNot, ValueName, 0, Matches, Values0, Values),
            Matches = 1
        then
            IsBad = no,
            CntPoss = 1,
            SolverVar = solver_var(CntAll, CntPoss, Values),
            map.det_update(VarName, SolverVar, !SolverVarMap)
        else
            IsBad = yes
        )
    else
        IsBad = yes
    ),
    process_arguments(Args, BadArgLinesTail, !SolverVarMap),
    (
        IsBad = no,
        BadArgLines = BadArgLinesTail
    ;
        IsBad = yes,
        BadArgLines = [Arg ++ "\n" | BadArgLinesTail]
    ).

:- pred set_value_to_false(not_possible_why::in, solver_var_value_name::in,
    list(solver_var_value)::in, list(solver_var_value)::out) is semidet.

set_value_to_false(WhyNot, SetName, [HeadValue0 | TailValues0], Values) :-
    HeadValue0 = solver_var_value(Name, Possible0),
    ( if SetName = Name then
        % Fail if this value was set to not_possible even before this.
        Possible0 = is_possible,
        Possible = not_possible(WhyNot),
        HeadValue = solver_var_value(Name, Possible),
        Values = [HeadValue | TailValues0]
    else
        set_value_to_false(WhyNot, SetName, TailValues0, TailValues),
        Values = [HeadValue0 | TailValues]
    ).

:- pred set_value_to_true(not_possible_why::in, solver_var_value_name::in,
    int::in, int::out,
    list(solver_var_value)::in, list(solver_var_value)::out) is semidet.

set_value_to_true(_WhyNot, _SetName, !Matches, [], []).
set_value_to_true(WhyNot, SetName, !Matches,
        [HeadValue0 | TailValues0], [HeadValue | TailValues]) :-
    HeadValue0 = solver_var_value(Name, Possible0),
    ( if SetName = Name then
        !:Matches = !.Matches + 1,
        % Fail if this value was set to not_possible previously.
        Possible0 = is_possible,
        HeadValue = HeadValue0
    else
        Possible = not_possible(WhyNot),
        HeadValue = solver_var_value(Name, Possible)
    ),
    set_value_to_true(WhyNot, SetName, !Matches, TailValues0, TailValues).

%---------------------------------------------------------------------------%

:- type maybe_changed
    --->    not_changed
    ;       changed.

:- pred solve(list(requirement)::in, list(solver_var_name)::in,
    solver_var_map::in, int::in, solution::out) is det.

solve(Requirements, PrioSolverVarNames, !.SolverVarMap, !.Pass, Soln) :-
    propagate_to_fixpoint(Requirements, !SolverVarMap, !Pass),
    at_solution(PrioSolverVarNames, !.SolverVarMap, MaybeSoln),
    (
        MaybeSoln = yes(Soln)
    ;
        MaybeSoln = no,
        label_step(PrioSolverVarNames, !SolverVarMap),
        solve(Requirements, PrioSolverVarNames, !.SolverVarMap, !.Pass, Soln)
    ).

%---------------------------------------------------------------------------%

:- pred propagate_to_fixpoint(list(requirement)::in,
    solver_var_map::in, solver_var_map::out,
    int::in, int::out) is det.

propagate_to_fixpoint(Requirements, !SolverVarMap, !Pass) :-
    propagate_pass(Requirements, !SolverVarMap, not_changed, Changed),
    trace [compile_time(flag("debug_choose_grade")), io(!IO)] (
        io.format("AFTER PROPAGATE PASS %d\n", [i(!.Pass)], !IO),
        io.write_string(solver_var_map_to_str(!.SolverVarMap), !IO)
    ),
    !:Pass = !.Pass + 1,
    (
        Changed = not_changed
    ;
        Changed = changed,
        propagate_to_fixpoint(Requirements, !SolverVarMap, !Pass)
    ).

:- pred propagate_pass(list(requirement)::in,
    solver_var_map::in, solver_var_map::out,
    maybe_changed::in, maybe_changed::out) is det.

propagate_pass([], !SolverVarMap, !Changed).
propagate_pass([Requirement | Requirements], !SolverVarMap, !Changed) :-
    Requirement = requirement(ReqId, _Desc,
        IfVarName, IfValueName, ThenVarName, ReqThenValueNames),
    % The requirement represents the implication:
    %
    %   (IfVarName = IfValueName) -> (ThenVarName in ReqThenValueNames)
    %
    map.lookup(!.SolverVarMap, IfVarName, IfVar0),
    IfVar0 = solver_var(_IfCntAll, IfCntPoss0, IfValues0),
    ( if is_value_possible(IfValues0, IfValueName, is_possible) then
        map.lookup(!.SolverVarMap, ThenVarName, ThenVar0),
        ThenVar0 = solver_var(_ThenCntAll, ThenCntPoss0, ThenValues0),
        ( if some_value_is_possible(ThenValues0, ReqThenValueNames) then
            ( if IfCntPoss0 = 1 then
                % We know that (IfVarName = IfValueName). We can therefore
                % impose (ThenVarName in ReqThenValueNames).
                %
                restrict_possibilities_to(ReqThenValueNames, ReqId,
                    0, ThenCntPoss, ThenValues0, ThenValues),
                ( if ThenCntPoss < ThenCntPoss0 then
                    ThenVar1 = ThenVar0 ^ sv_cnt_possible := ThenCntPoss,
                    ThenVar = ThenVar1 ^ sv_values := ThenValues,
                    map.det_update(ThenVarName, ThenVar, !SolverVarMap),
                    !:Changed = changed
                else
                    % The condition (ThenVarName in ReqThenValueNames)
                    % already held, so nothing has changed.
                    true
                )
            else
                true
            )
        else
            % Since (ThenVarName in ReqThenValueNames) is false,
            % we can impose (IfVarName = IfValueName) being false as well.
            %
            IfCntPoss = IfCntPoss0 - 1,
            set_value_to_not_possible(IfValueName, ReqId, IfValues0, IfValues),
            IfVar1 = IfVar0 ^ sv_cnt_possible := IfCntPoss,
            IfVar = IfVar1 ^ sv_values := IfValues,
            map.det_update(IfVarName, IfVar, !SolverVarMap),
            !:Changed = changed
        )
    else
        % We already know that (IfVarName = IfValueName) is false.
        % Since false implies anything, the requirement is already met.
        true
    ),
    propagate_pass(Requirements, !SolverVarMap, !Changed).

:- pred some_value_is_possible(list(solver_var_value)::in,
    list(solver_var_value_name)::in) is semidet.

some_value_is_possible(Values, [SearchName | SearchNames]) :-
    ( if is_value_possible(Values, SearchName, is_possible) then
        true
    else
        some_value_is_possible(Values, SearchNames)
    ).

:- pred is_value_possible(list(solver_var_value)::in,
    solver_var_value_name::in, solver_var_value_possible::out) is det.

is_value_possible([], _, _) :-
    unexpected($pred, "did not find value").
is_value_possible([Value | Values], SearchName, Possible) :-
    Value = solver_var_value(ValueName, ValuePossible),
    ( if ValueName = SearchName then
        Possible = ValuePossible
    else
        is_value_possible(Values, SearchName, Possible)
    ).

:- pred set_value_to_not_possible(solver_var_value_name::in,
    requirement_id::in,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

set_value_to_not_possible(_SetName, _ReqId, [], _) :-
    unexpected($pred, "value not found").
set_value_to_not_possible(SetName, ReqId, [HeadValue0 | TailValues0],
        Values) :-
    HeadValue0 = solver_var_value(Name, _Possible0),
    ( if SetName = Name then
        Possible = not_possible(npw_requirement(ReqId)),
        HeadValue = solver_var_value(Name, Possible),
        Values = [HeadValue | TailValues0]
    else
        set_value_to_not_possible(SetName, ReqId, TailValues0, TailValues),
        Values = [HeadValue0 | TailValues]
    ).

:- pred restrict_possibilities_to(list(solver_var_value_name)::in,
    requirement_id::in, int::in, int::out,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

restrict_possibilities_to(!.SetNames, _ReqId, !CntPoss, [], []) :-
    expect(unify(!.SetNames, []), $pred, "value not found").
restrict_possibilities_to(!.SetNames, ReqId, !CntPoss,
        [HeadValue0 | TailValues0], [HeadValue | TailValues]) :-
    HeadValue0 = solver_var_value(Name, Possible0),
    ( if list.delete_first(!.SetNames, Name, !:SetNames) then
        (
            Possible0 = is_possible,
            !:CntPoss = !.CntPoss + 1
        ;
            Possible0 = not_possible(_)
        ),
        HeadValue = HeadValue0
    else
        Possible = not_possible(npw_requirement(ReqId)),
        HeadValue = solver_var_value(Name, Possible)
    ),
    restrict_possibilities_to(!.SetNames, ReqId, !CntPoss,
        TailValues0, TailValues).

%---------------------------------------------------------------------------%

:- pred label_step(list(solver_var_name)::in,
    solver_var_map::in, solver_var_map::out) is det.

label_step(PrioSolverVarNames, SolverMap0, SolverMap) :-
    find_first_undetermined_var(PrioSolverVarNames, SolverMap0,
        FirstVarName, FirstVar0),
    FirstVar0 = solver_var(_CntAll, _CntPoss, FirstValues0),
    find_first_possible_value_and_commit(FirstVarName,
        FirstValues0, FirstValues),
    FirstVar1 = FirstVar0 ^ sv_cnt_possible := 1,
    FirstVar = FirstVar1 ^ sv_values := FirstValues,
    map.det_update(FirstVarName, FirstVar, SolverMap0, SolverMap).

:- pred find_first_undetermined_var(list(solver_var_name)::in,
    solver_var_map::in, solver_var_name::out, solver_var::out) is det.

find_first_undetermined_var([], _, _, _) :-
    unexpected($pred, "no undetermined var").
find_first_undetermined_var([SolverVarName | SolverVarNames], SolverMap,
        FirstVarName, FirstVar) :-
    map.lookup(SolverMap, SolverVarName, SolverVar),
    SolverVar = solver_var(_CntAll, CntPoss, _Values),
    ( if CntPoss > 1 then
        FirstVarName = SolverVarName,
        FirstVar = SolverVar
    else
        find_first_undetermined_var(SolverVarNames, SolverMap,
            FirstVarName, FirstVar)
    ).

:- pred find_first_possible_value_and_commit(solver_var_name::in,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

find_first_possible_value_and_commit(_VarName, [], []) :-
    unexpected($pred, "no possible value").
find_first_possible_value_and_commit(VarName, [Value0 | Values0],
        [Value | Values]) :-
    Value0 = solver_var_value(ValueName, Possible0),
    (
        Possible0 = is_possible,
        VarName = sv_name(VarNameStr),
        ValueName = svv_name(ValueNameStr),
        trace [compile_time(flag("debug_choose_grade")), io(!TIO)] (
            io.format("LABELING sets %s to %s\n",
                [s(VarNameStr), s(ValueNameStr)], !TIO)
        ),
        Value = Value0,
        set_values_not_possible_labeling(Values0, Values)
    ;
        Possible0 = not_possible(_),
        Value = Value0,
        find_first_possible_value_and_commit(VarName, Values0, Values)
    ).

:- pred set_values_not_possible_labeling(
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

set_values_not_possible_labeling([], []).
set_values_not_possible_labeling([Value0 | Values0], [Value | Values]) :-
    Value0 = solver_var_value(Name, Possible0),
    (
        Possible0 = is_possible,
        Possible = not_possible(npw_labeling),
        Value = solver_var_value(Name, Possible)
    ;
        Possible0 = not_possible(_),
        Value = Value0
    ),
    set_values_not_possible_labeling(Values0, Values).

%---------------------------------------------------------------------------%

:- type solution
    --->    soln_failure
    ;       soln_success(
                assoc_list(solver_var_name, solver_var_value_name)
            ).

:- pred at_solution(list(solver_var_name)::in, solver_var_map::in,
    maybe(solution)::out) is det.

at_solution(PrioSolverVarNames, SolverVarMap, MaybeSoln) :-
    map.foldl2_values(cnt_poss_classes, SolverVarMap, 0, NumZero, 0, NumMore),
    ( if NumZero > 0 then
        MaybeSoln = yes(soln_failure)
    else if NumMore = 0 then
        list.map(project_to_one_poss_value(SolverVarMap),
            PrioSolverVarNames, SuccessSoln),
        MaybeSoln = yes(soln_success(SuccessSoln))
    else
        MaybeSoln = no
    ).

:- pred cnt_poss_classes(solver_var::in,
    int::in, int::out, int::in, int::out) is det.

cnt_poss_classes(SolverVar, !NumZero, !NumMore) :-
    SolverVar = solver_var(_CntAll, CntPoss, _Values),
    ( if CntPoss = 0 then
        !:NumZero = !.NumZero + 1
    else if CntPoss = 1 then
        true
    else
        !:NumMore = !.NumMore + 1
    ).

:- pred project_to_one_poss_value(solver_var_map::in, solver_var_name::in,
    pair(solver_var_name, solver_var_value_name)::out) is det.

project_to_one_poss_value(SolverVarMap, VarName, VarName - ValueName) :-
    map.lookup(SolverVarMap, VarName, SolverVar),
    SolverVar = solver_var(_CntAll, _CntPoss, Values),
    get_poss_values(Values, PossValueNames),
    ( if PossValueNames = [ValueNamePrime] then
        ValueName = ValueNamePrime
    else
        unexpected($pred, "number of possible values is not one")
    ).

:- pred get_poss_values(list(solver_var_value)::in,
    list(solver_var_value_name)::out) is det.

get_poss_values([], []).
get_poss_values([SolverVarValue | SolverVarValues], PossValueNames) :-
    get_poss_values(SolverVarValues, PossValueNamesTail),
    SolverVarValue = solver_var_value(Name, Possible),
    (
        Possible = is_possible,
        PossValueNames = [Name | PossValueNamesTail]
    ;
        Possible = not_possible(_),
        PossValueNames = PossValueNamesTail
    ).

%---------------------------------------------------------------------------%

:- func solver_var_map_to_str(solver_var_map) = string.

solver_var_map_to_str(SolverVarMap) = Str :-
    map.to_sorted_assoc_list(SolverVarMap, SolverAssocList),
    SolverVarStrs = list.map(solver_var_pair_to_str, SolverAssocList),
    Str = string.append_list(SolverVarStrs).

:- func solver_var_pair_to_str(pair(solver_var_name, solver_var)) = string.

solver_var_pair_to_str(SolverVarName - SolverVar) = Str :-
    SolverVar = solver_var(_CntAll, _CntPoss, _Values),
    Str = solver_var_to_str(SolverVarName, SolverVar).

:- func solver_var_to_str(solver_var_name, solver_var) = string.

solver_var_to_str(SolverVarName, SolverVar) = Str :-
    SolverVar = solver_var(_CntAll, CntPoss, Values),
    SolverVarName = sv_name(NameStr),
    ValuesStrs = list.map(solver_var_value_to_str(CntPoss), Values),
    ValuesStr = string.join_list(", ", ValuesStrs),
    expect(unify(CntPoss, count_possible_values(Values)), $pred,
        "CntPoss mismatch"),
    string.format("solver var %s: %d possible\n\t%s\n",
        [s(NameStr), i(CntPoss), s(ValuesStr)], Str).

:- func solver_var_value_to_str(int, solver_var_value) = string.

solver_var_value_to_str(CntPoss, SolverVarValue) = Str :-
    SolverVarValue = solver_var_value(Name, Possible),
    Name = svv_name(NameStr),
    (
        Possible = is_possible,
        ( if CntPoss = 1 then
            string.format("%s yes", [s(NameStr)], Str)
        else
            string.format("%s maybe", [s(NameStr)], Str)
        )
    ;
        Possible = not_possible(WhyNot),
        (
            WhyNot = npw_config,
            string.format("%s no (config)", [s(NameStr)], Str)
        ;
            WhyNot = npw_user,
            string.format("%s no (user)", [s(NameStr)], Str)
        ;
            WhyNot = npw_requirement(requirement_id(ReqIdNum)),
            string.format("%s no (req %d)", [s(NameStr), i(ReqIdNum)], Str)
        ;
            WhyNot = npw_labeling,
            string.format("%s no (labeling)", [s(NameStr)], Str)
        )
    ).

:- func count_possible_values(list(solver_var_value)) = int.

count_possible_values([]) = 0.
count_possible_values([SolverVarValue | SolverVarValues]) = N :-
    NTail = count_possible_values(SolverVarValues),
    SolverVarValue = solver_var_value(_Name, Possible),
    (
        Possible = is_possible,
        N = NTail + 1
    ;
        Possible = not_possible(_),
        N = NTail
    ).

%---------------------------------------------------------------------------%

:- func soln_to_str(solution) = string.

soln_to_str(soln_failure) = "FAILURE\n".
soln_to_str(soln_success(SuccessValues)) = Str :-
    SuccessStr = "SUCCESS\n",
    SuccessValueStrs = list.map(success_value_to_str, SuccessValues),
    Str = SuccessStr ++ string.append_list(SuccessValueStrs).

:- func success_value_to_str(pair(solver_var_name, solver_var_value_name))
    = string.

success_value_to_str(VarName - ValueName) = Str :-
    VarName = sv_name(VarNameStr),
    ValueName = svv_name(ValueNameStr),
    string.format("%s = %s\n", [s(VarNameStr), s(ValueNameStr)], Str).

%---------------------------------------------------------------------------%
:- end_module choose.
%---------------------------------------------------------------------------%
