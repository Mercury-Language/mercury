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
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

main(!IO) :-
    setup_solver_info(SolverInfo0),
    TestSetSpecs = [broad_test_set_spec, llds_test_set_spec],
    run_test_sets(SolverInfo0, TestSetSpecs, 0, !IO).

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
    test_set_component("stack_segments", ["stseg", "stfix"]),
    test_set_component("thread_safe", ["not_thread_safe", "thread_safe"]),
    test_set_component("minimal_model", ["no_mm", "mm_stack_copy"]),
    test_set_component("deep_prof", ["no_deep_prof", "deep_prof"]),
    test_set_component("debug", ["nodebug", "debug", "decldebug"]),
    test_set_component("single_prec_float", ["no_spf", "spf"])
].

%---------------------------------------------------------------------------%

:- pred run_test_sets(solver_info::in, list(test_set_spec)::in,
    int::in, io::di, io::uo) is det.

run_test_sets(_SolverInfo0, [], _Seq, !IO).
run_test_sets(SolverInfo0, [TestSpec | TestSpecs], !.Seq, !IO) :-
    run_test_set(SolverInfo0, TestSpec, cord.init, !Seq, !IO),
    run_test_sets(SolverInfo0, TestSpecs, !.Seq, !IO).

%---------------------------------------------------------------------------%

:- pred run_test_set(solver_info::in, test_set_spec::in, test_spec::in,
    int::in, int::out, io::di, io::uo) is det.

run_test_set(SolverInfo0, TestSetSpec, TestSpecSoFar0, !Seq, !IO) :-
    (
        TestSetSpec = [],
        !:Seq = !.Seq + 1,
        solve(SolverInfo0, Soln),
        SpecStr = test_spec_to_string("", !.Seq, TestSpecSoFar0),
        SolnStr = soln_to_str("    ", Soln),
        (
            Soln = soln_failure,
            GradeStr = ""
        ;
            Soln = soln_success(SuccMap),
            GradeStr =
                "GRADE " ++ success_soln_to_grade_string(SuccMap) ++ "\n"
        ),
        io.nl(!IO),
        io.write_string(SpecStr, !IO),
        io.write_string(SolnStr, !IO),
        io.write_string(GradeStr, !IO)
    ;
        TestSetSpec = [TestSetSpecHead | TestSetSpecTail],
        TestSetSpecHead = test_set_component(SolverVar, SolverValues),
        run_alternatives_for_var(SolverInfo0, SolverVar, SolverValues,
            TestSetSpecTail, TestSpecSoFar0, !Seq, !IO)
    ).

:- pred run_alternatives_for_var(solver_info::in, string::in, list(string)::in,
    test_set_spec::in, test_spec::in, int::in, int::out, io::di, io::uo) is det.

run_alternatives_for_var(_SolverInfo0, _SolverVar, [],
        _TestSetSpecTail, _TestSpecSoFar0, !Seq, !IO).
run_alternatives_for_var(SolverInfo0, SolverVar, [SolverValue | SolverValues],
        TestSetSpecTail, TestSpecSoFar0, !Seq, !IO) :-
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
        TestSpecSoFarForThisAlternative, !Seq, !IO),
    run_alternatives_for_var(SolverInfo0, SolverVar, SolverValues,
        TestSetSpecTail, TestSpecSoFar0, !Seq, !IO).

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
