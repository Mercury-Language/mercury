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
:- import_module grade_spec.
:- import_module grade_state.
:- import_module grade_string.
:- import_module grade_structure.
:- import_module grade_vars.
:- import_module var_value_names.

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
    AutoconfResults = autoconf_results(autoconf_gcc_regs_avail_yes,
        autoconf_gcc_gotos_avail_yes, autoconf_gcc_labels_avail_yes,
        autoconf_low_tag_bits_avail_3, autoconf_merc_file_no),
    setup_solver_info(AutoconfResults, SolverInfo0),
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
                tsc_solver_var_id           :: solver_var_id,
                tsc_solver_var_value_ids    :: list(solver_var_value_id)
            ).

:- type test_component
    --->    test_component(
                tc_solver_var_id            :: solver_var_id,
                tc_solver_var_value_id      :: solver_var_value_id
            ).

:- type test_set_spec == list(test_set_component).
:- type test_spec == cord(test_component).

%---------------------------------------------------------------------------%

:- func broad_test_set_spec = test_set_spec.

broad_test_set_spec = [
    test_set_component(svar_target,
        [svalue_target_c, svalue_target_csharp,
        svalue_target_java, svalue_target_erlang]),
    test_set_component(svar_trail,
        [svalue_trail_no, svalue_trail_yes]),
    test_set_component(svar_thread_safe,
        [svalue_thread_safe_no, svalue_thread_safe_yes]),
    test_set_component(svar_ssdebug,
        [svalue_ssdebug_no, svalue_ssdebug_yes]),
    test_set_component(svar_single_prec_float,
        [svalue_single_prec_float_no, svalue_single_prec_float_yes])
].

:- func llds_test_set_spec = test_set_spec.

llds_test_set_spec = [
    test_set_component(svar_stack_len,
        [svalue_stack_len_segments, svalue_stack_len_std]),
    test_set_component(svar_thread_safe,
        [svalue_thread_safe_no, svalue_thread_safe_yes]),
    test_set_component(svar_minmodel,
        [svalue_minmodel_no, svalue_minmodel_stack_copy]),
    test_set_component(svar_deep_prof,
        [svalue_deep_prof_no, svalue_deep_prof_yes]),
    test_set_component(svar_debug,
        [svalue_debug_none, svalue_debug_debug, svalue_debug_decldebug]),
    test_set_component(svar_single_prec_float,
        [svalue_single_prec_float_no, svalue_single_prec_float_yes])
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
            GradeVars = success_map_to_grade_vars(SuccMap),
            GradeStructure = grade_vars_to_grade_structure(GradeVars),
            GradeStr = grade_structure_to_grade_string(
                grade_string_link_check, GradeStructure),
            SolnGradeStr = "    GRADE " ++ GradeStr ++ "\n"
        ),
        io.nl(!IO),
        io.write_string(SpecStr, !IO),
        io.write_string(CountStr, !IO),
        io.write_string(SolnStr, !IO),
        io.write_string(SolnGradeStr, !IO)
    ;
        TestSetSpec = [TestSetSpecHead | TestSetSpecTail],
        TestSetSpecHead = test_set_component(VarId, ValueIds),
        run_alternatives_for_var(SolverInfo0, VarId, ValueIds,
            TestSetSpecTail, TestSpecSoFar0, !SolveCountStats, !IO)
    ).

:- pred run_alternatives_for_var(solver_info::in,
    solver_var_id::in, list(solver_var_value_id)::in,
    test_set_spec::in, test_spec::in,
    solve_count_stats::in, solve_count_stats::out, io::di, io::uo) is det.

run_alternatives_for_var(_SolverInfo0, _VarId, [],
        _TestSetSpecTail, _TestSpecSoFar0, !SolveCountStats, !IO).
run_alternatives_for_var(SolverInfo0, VarId, [ValueId | ValueIds],
        TestSetSpecTail, TestSpecSoFar0, !SolveCountStats, !IO) :-
    solver_var_name(VarName, VarId),
    solver_var_value_name(ValueName, ValueId),
    set_solver_var(VarName, ValueName, VarId, ValueId, set_to_true, npw_user,
        MaybeError, SolverInfo0, SolverInfoForThisAlternative),
    (
        MaybeError = ok
    ;
        MaybeError = error(ErrorMsg),
        unexpected($pred, ErrorMsg)
    ),
    ThisTestComponent = test_component(VarId, ValueId),
    TestSpecSoFarForThisAlternative =
        cord.snoc(TestSpecSoFar0, ThisTestComponent),
    run_test_set(SolverInfoForThisAlternative, TestSetSpecTail,
        TestSpecSoFarForThisAlternative, !SolveCountStats, !IO),
    run_alternatives_for_var(SolverInfo0, VarId, ValueIds,
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
    TestComponent = test_component(VarId, ValueId),
    solver_var_name(VarName, VarId),
    solver_var_value_name(ValueName, ValueId),
    string.format("%s%s = %s\n", [s(Prefix), s(VarName), s(ValueName)], Str).

%---------------------------------------------------------------------------%
:- end_module test_grades.
%---------------------------------------------------------------------------%
