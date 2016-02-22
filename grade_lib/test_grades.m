%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_grades.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module grade_setup.
:- import_module grade_solver.
:- import_module grade_state.
:- import_module grade_string.

:- import_module bool.
:- import_module cord.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- type solve_count_stats
    --->    solve_count_stats(
                scs_total_num_label_steps   :: int,
                scs_total_num_passes        :: int,
                scs_total_count             :: int
            ).

main(!IO) :-
    setup_solver_info(SolverInfo0),
    TestSetSpecs = [broad_test_set_spec, llds_test_set_spec],
    SolveCountStats0 = solve_count_stats(0, 0, 0),
    run_test_sets(SolverInfo0, TestSetSpecs,
        SolveCountStats0, SolveCountStats, !IO),
    SolveCountStats = solve_count_stats(TotalNumLabelSteps, TotalNumPasses,
        Count),
    io.nl(!IO),
    io.format("Average number of label steps: %5.2f\n",
        [f(float(TotalNumLabelSteps) / float(Count))], !IO),
    io.format("Average number of passes:      %5.2f\n",
        [f(float(TotalNumPasses) / float(Count))], !IO).

%---------------------------------------------------------------------------%

:- type test_set_component
    --->    test_set_component(
                tsc_solver_var_name         :: string,
                tsc_solver_var_value_names  :: list(string)
            ).

:- type test_component
    --->    test_component(
                tc_solver_var_name          :: string,
                tc_solver_var_value_name    :: string
            ).

:- type test_set_spec == list(test_set_component).
:- type test_spec == cord(test_component).

%---------------------------------------------------------------------------%

:- func broad_test_set_spec = test_set_spec.

broad_test_set_spec = [
    test_set_component("target", ["c", "csharp", "java", "erlang"]),
    test_set_component("trail", ["no_trail", "trail"]),
    test_set_component("thread_safe", ["not_thread_safe", "thread_safe"]),
    test_set_component("ssdebug", ["no_ssdebug", "ssdebug"]),
    test_set_component("single_prec_float", ["no_spf", "spf"])
].

:- func llds_test_set_spec = test_set_spec.

llds_test_set_spec = [
    test_set_component("gcc_regs_avail",
        ["gcc_regs_not_avail", "gcc_regs_avail"]),
    test_set_component("gcc_gotos_avail",
        ["gcc_gotos_not_avail", "gcc_gotos_avail"]),
    test_set_component("gcc_labels_avail",
        ["gcc_labels_not_avail", "gcc_labels_avail"]),
    test_set_component("stack_len", ["stseg", "stfix"]),
    test_set_component("thread_safe", ["not_thread_safe", "thread_safe"]),
    test_set_component("minimal_model", ["no_mm", "mm_stack_copy"]),
    test_set_component("deep_prof", ["no_deep_prof", "deep_prof"]),
    test_set_component("debug", ["nodebug", "debug", "decldebug"]),
    test_set_component("single_prec_float", ["no_spf", "spf"])
].

%---------------------------------------------------------------------------%

:- pred run_test_sets(solver_info::in, list(test_set_spec)::in,
    solve_count_stats::in, solve_count_stats::out, io::di, io::uo) is det.

run_test_sets(_SolverInfo0, [], !SolveCountStats, !IO).
run_test_sets(SolverInfo0, [TestSpec | TestSpecs], !SolveCountStats, !IO) :-
    run_test_set(SolverInfo0, TestSpec, cord.init, !SolveCountStats, !IO),
    run_test_sets(SolverInfo0, TestSpecs, !SolveCountStats, !IO).

%---------------------------------------------------------------------------%

:- pred run_test_set(solver_info::in, test_set_spec::in, test_spec::in,
    solve_count_stats::in, solve_count_stats::out, io::di, io::uo) is det.

run_test_set(SolverInfo0, TestSetSpec, TestSpecSoFar0, !SolveCountStats, !IO) :-
    (
        TestSetSpec = [],
        !.SolveCountStats = solve_count_stats(TotalNumLabelSteps0,
            TotalNumPasses0, Seq0),
        solve(SolverInfo0, SolveCounts, Soln),
        SolveCounts = solve_counts(NumLabelSteps, NumPasses),
        TotalNumLabelSteps = TotalNumLabelSteps0 + NumLabelSteps,
        TotalNumPasses = TotalNumPasses0 + NumPasses,
        Seq = Seq0 + 1,
        !:SolveCountStats = solve_count_stats(TotalNumLabelSteps,
            TotalNumPasses, Seq),

        SpecStr = test_spec_to_string("", Seq, TestSpecSoFar0),
        string.format("PERF: %2d label steps, %2d passes\n",
            [i(NumLabelSteps), i(NumPasses)], CountStr),
        SolnStr = soln_to_str("    ", Soln),
        (
            Soln = soln_failure(_),
            SolnGradeStr = ""
        ;
            Soln = soln_success(SuccMap),
            Grade = success_soln_to_grade(SuccMap),
            GradeStr = grade_to_grade_string(Grade),
            SolnGradeStr = "    GRADE " ++ GradeStr ++ "\n"
        ),
        io.nl(!IO),
        io.write_string(SpecStr, !IO),
        io.write_string(CountStr, !IO),
        io.write_string(SolnStr, !IO),
        io.write_string(SolnGradeStr, !IO)
    ;
        TestSetSpec = [TestSetSpecHead | TestSetSpecTail],
        TestSetSpecHead = test_set_component(SolverVar, SolverValues),
        run_alternatives_for_var(SolverInfo0, SolverVar, SolverValues,
            TestSetSpecTail, TestSpecSoFar0, !SolveCountStats, !IO)
    ).

:- pred run_alternatives_for_var(solver_info::in, string::in, list(string)::in,
    test_set_spec::in, test_spec::in,
    solve_count_stats::in, solve_count_stats::out, io::di, io::uo) is det.

run_alternatives_for_var(_SolverInfo0, _SolverVar, [],
        _TestSetSpecTail, _TestSpecSoFar0, !SolveCountStats, !IO).
run_alternatives_for_var(SolverInfo0, SolverVar, [SolverValue | SolverValues],
        TestSetSpecTail, TestSpecSoFar0, !SolveCountStats, !IO) :-
    set_solver_var(SolverVar, SolverValue, set_to_true, npw_user, MaybeError,
        SolverInfo0, SolverInfoForThisAlternative),
    (
        MaybeError = ok
    ;
        MaybeError = error(ErrorMsg),
        unexpected($pred, ErrorMsg)
    ),
    ThisTestComponent = test_component(SolverVar, SolverValue),
    TestSpecSoFarForThisAlternative =
        cord.snoc(TestSpecSoFar0, ThisTestComponent),
    run_test_set(SolverInfoForThisAlternative, TestSetSpecTail,
        TestSpecSoFarForThisAlternative, !SolveCountStats, !IO),
    run_alternatives_for_var(SolverInfo0, SolverVar, SolverValues,
        TestSetSpecTail, TestSpecSoFar0, !SolveCountStats, !IO).

%---------------------------------------------------------------------------%

:- func test_spec_to_string(string, int, test_spec) = string.

test_spec_to_string(Prefix, Seq, TestSpec) = Str :-
    string.format("%sTest %d:\n", [s(Prefix), i(Seq)], TestSeqStr),
    TestSpecComponents = cord.to_list(TestSpec),
    TestComponentStrs =
        list.map(test_component_to_string(Prefix), TestSpecComponents),
    Str = string.append_list([TestSeqStr | TestComponentStrs]).

:- func test_component_to_string(string, test_component) = string.

test_component_to_string(Prefix, TestComponent) = Str :-
    TestComponent = test_component(SolverVar, SolverValue),
    string.format("%s%s = %s\n",
        [s(Prefix), s(SolverVar), s(SolverValue)], Str).

%---------------------------------------------------------------------------%
:- end_module test_grades.
%---------------------------------------------------------------------------%
