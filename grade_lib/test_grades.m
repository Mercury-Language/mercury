%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Test the solver by giving it a set of grade problems to solve,
% both in the absolute space of all possible grades and relative to
% several possible sets of installed grades, and print the results,
% together with operation counts.
%
% The intention is that
%
% - the absence of ERROR messages in the printed results can be checked
%   simply by grepping for ERROR,
% - the printout can be checked visually for correctness, both in terms of
%   whether successful solutions are correct and whether failures are diagnosed
%   correctly and with useful error messages, and
% - the operation counts can be used to see the effects of any attempted
%   speedups.
%

:- module test_grades.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module grade_lib.
:- import_module grade_lib.grade_setup.
:- import_module grade_lib.grade_solver.
:- import_module grade_lib.grade_spec.
:- import_module grade_lib.grade_state.
:- import_module grade_lib.grade_string.
:- import_module grade_lib.grade_structure.
:- import_module grade_lib.grade_vars.
:- import_module var_value_names.

:- import_module cord.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- type installed_grade_set
    --->    installed_grade_set(
                igs_name                    :: string,
                igs_grades                  :: list(installed_grade)
            ).

:- type solve_count_stats
    --->    solve_count_stats(
                scs_total_num_label_steps   :: int,
                scs_total_num_passes        :: int,
                scs_total_num_req_tests     :: int,
                scs_num_tests               :: int
            ).

main(!IO) :-
    SpecsVersion = specs_version_1,
    AutoconfResults = autoconf_results(autoconf_gcc_regs_avail_yes,
        autoconf_gcc_gotos_avail_yes, autoconf_gcc_labels_avail_yes,
        autoconf_low_tag_bits_avail_3, autoconf_size_of_double_eq_ptr,
        autoconf_merc_file_no),
    setup_solver_info(SpecsVersion, AutoconfResults, SolverInfo0),

    parse_installed_grade(SolverInfo0,
        "asm_fast.gc",                  AsmFastGc),
    parse_installed_grade(SolverInfo0,
        "asm_fast.gc.stseg",            AsmFastGcStseg),
    parse_installed_grade(SolverInfo0,
        "asm_fast.gc.stseg.tr",         AsmFastGcStsegTr),
    parse_installed_grade(SolverInfo0,
        "asm_fast.gc.debug.stseg",      AsmFastGcDebugStseg),
    parse_installed_grade(SolverInfo0,
        "asm_fast.gc.decldebug.stseg",  AsmFastGcDecldebugStseg),
    parse_installed_grade(SolverInfo0,
        "asm_fast.gc.profdeep.stseg",   AsmFastGcProfDeepStseg),
    parse_installed_grade(SolverInfo0,
        "asm_fast.gc.profall.stseg",    AsmFastGcProfAllStseg),
    parse_installed_grade(SolverInfo0,
        "none.gc.profall.stseg",        NoneGcProfAllStseg),
    parse_installed_grade(SolverInfo0,
        "hlc.gc",                       HlcGc),
    parse_installed_grade(SolverInfo0,
        "hlc.gc.tr",                    HlcGcTr),
    parse_installed_grade(SolverInfo0,
        "hlc.gc.par",                   HlcGcPar),
    parse_installed_grade(SolverInfo0,
        "csharp",                       Csharp),
    parse_installed_grade(SolverInfo0,
        "java",                         Java),

    GradesAll = [AsmFastGc, AsmFastGcStseg, AsmFastGcStsegTr,
        AsmFastGcDebugStseg, AsmFastGcDecldebugStseg,
        AsmFastGcProfDeepStseg, AsmFastGcProfAllStseg, NoneGcProfAllStseg,
        HlcGc, HlcGcTr, HlcGcPar, Csharp, Java],
    SetAll = installed_grade_set("grades_all", GradesAll),

    GradesLLDSAll = [AsmFastGc, AsmFastGcStseg, AsmFastGcStsegTr,
        AsmFastGcDebugStseg, AsmFastGcDecldebugStseg, AsmFastGcProfDeepStseg,
        AsmFastGcProfAllStseg, NoneGcProfAllStseg],
    SetLLDSAll = installed_grade_set("grades_llds_all", GradesLLDSAll),

    GradesLLDSDebug = [AsmFastGc, AsmFastGcStseg, AsmFastGcStsegTr,
        AsmFastGcDebugStseg, AsmFastGcDecldebugStseg, AsmFastGcProfDeepStseg],
    SetLLDSDebug = installed_grade_set("grades_llds_debug", GradesLLDSDebug),

    GradesLLDSProf = [AsmFastGc, AsmFastGcStseg, AsmFastGcStsegTr,
        AsmFastGcProfAllStseg, NoneGcProfAllStseg],
    SetLLDSProf = installed_grade_set("grades_llds_prof", GradesLLDSProf),

    GradesEMLDS = [HlcGc, HlcGcTr, HlcGcPar, Csharp, Java],
    SetEMLDS = installed_grade_set("grades_emlds", GradesEMLDS),

    GradesTrOnlyLLDS = [AsmFastGc, AsmFastGcStseg, HlcGc, AsmFastGcStsegTr],
    SetTrOnlyLLDS = installed_grade_set("grades_tr_llds", GradesTrOnlyLLDS),

    GradesTrOnlyMLDS = [AsmFastGc, AsmFastGcStseg, HlcGc, HlcGcTr],
    SetTrOnlyMLDS = installed_grade_set("grades_tr_mlds", GradesTrOnlyMLDS),

    InstalledSets = [SetAll, SetLLDSAll, SetLLDSDebug, SetLLDSProf,
        SetEMLDS, SetTrOnlyLLDS, SetTrOnlyMLDS],

    TestSetSpecs = [broad_test_set_spec, llds_test_set_spec],
    AbsSolveCountStats0 = solve_count_stats(0, 0, 0, 0),
    RelSolveCountStats0 = solve_count_stats(0, 0, 0, 0),
    run_test_sets(SolverInfo0, InstalledSets, TestSetSpecs,
        AbsSolveCountStats0, AbsSolveCountStats,
        RelSolveCountStats0, RelSolveCountStats, !IO),
    print_solve_count_stats("\nAbsolute solve counts:\n",
        AbsSolveCountStats, !IO),
    print_solve_count_stats("\nRelative solve counts:\n",
        RelSolveCountStats, !IO).

:- pred parse_installed_grade(solver_info::in, string::in,
    installed_grade::out) is det.

parse_installed_grade(SolverInfo0, GradeStr, InstalledGrade) :-
    MaybeSpecSuccMap = grade_string_to_succ_soln(GradeStr),
    (
        MaybeSpecSuccMap = ok(SpecSuccMap)
    ;
        MaybeSpecSuccMap = error(HeadErrorMsg, TailErrorMsgs),
        string.append_list([HeadErrorMsg | TailErrorMsgs], CombinedErrorMsg),
        unexpected($pred, CombinedErrorMsg)
    ),
    SolverVarMap0 = SolverInfo0 ^ si_solver_var_map,
    map.foldl(assign_var_in_map(npw_config), SpecSuccMap,
        SolverVarMap0, SolverVarMap),
    SolverInfo = SolverInfo0 ^ si_solver_var_map := SolverVarMap,
    solve_absolute(SolverInfo, _SolveCounts, Soln),
    (
        Soln = soln_failure(_),
        unexpected($pred, "cannot solve installed grade string " ++ GradeStr)
    ;
        Soln = soln_success(StdSuccMap)
    ),
    StdGradeVars = success_map_to_grade_vars(StdSuccMap),
    StdGradeStructure = grade_vars_to_grade_structure(StdGradeVars),
    StdGradeStr = grade_structure_to_grade_string(grade_string_user,
        StdGradeStructure),
    InstalledGrade = installed_grade(StdGradeStr, StdSuccMap).

:- pred print_solve_count_stats(string::in, solve_count_stats::in,
    io::di, io::uo) is det.

print_solve_count_stats(Msg, SolveCountStats, !IO) :-
    SolveCountStats = solve_count_stats(TotalNumLabelSteps, TotalNumPasses,
        TotalNumReqTests, NumTests),
    io.format("%s\n", [s(Msg)], !IO),
    io.format("Number of tests:                     %4d\n",
        [i(NumTests)], !IO),
    ( if NumTests > 0 then
        io.format("Average number of label steps:       %7.2f\n",
            [f(float(TotalNumLabelSteps) / float(NumTests))], !IO),
        io.format("Average number of passes:            %7.2f\n",
            [f(float(TotalNumPasses) / float(NumTests))], !IO),
        io.format("Average number of requirement tests: %7.2f\n",
            [f(float(TotalNumReqTests) / float(NumTests))], !IO)
    else
        true
    ).

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
        [svalue_target_c, svalue_target_csharp, svalue_target_java]),
    test_set_component(svar_trail,
        [svalue_trail_no, svalue_trail_yes]),
    test_set_component(svar_thread_safe,
        [svalue_thread_safe_c_no, svalue_thread_safe_c_yes,
        svalue_thread_safe_target_native]),
    test_set_component(svar_ssdebug,
        [svalue_ssdebug_no, svalue_ssdebug_yes]),
    test_set_component(svar_request_single_prec_float,
        [svalue_request_single_prec_float_no,
        svalue_request_single_prec_float_yes])
].

:- func llds_test_set_spec = test_set_spec.

llds_test_set_spec = [
    test_set_component(svar_stack_len,
        [svalue_stack_len_segments, svalue_stack_len_std]),
    test_set_component(svar_thread_safe,
        [svalue_thread_safe_c_no, svalue_thread_safe_c_yes]),
    test_set_component(svar_minmodel,
        [svalue_minmodel_no, svalue_minmodel_stack_copy]),
    test_set_component(svar_deep_prof,
        [svalue_deep_prof_no, svalue_deep_prof_yes]),
    test_set_component(svar_debug,
        [svalue_debug_none, svalue_debug_debug, svalue_debug_decldebug]),
    test_set_component(svar_request_single_prec_float,
        [svalue_request_single_prec_float_no,
        svalue_request_single_prec_float_yes])
].

%---------------------------------------------------------------------------%

:- pred run_test_sets(solver_info::in, list(installed_grade_set)::in,
    list(test_set_spec)::in,
    solve_count_stats::in, solve_count_stats::out,
    solve_count_stats::in, solve_count_stats::out, io::di, io::uo) is det.

run_test_sets(_SolverInfo0, _InstalledSets, [],
        !AbsSolveCountStats, !RelSolveCountStats, !IO).
run_test_sets(SolverInfo0, InstalledSets, [TestSpec | TestSpecs],
        !AbsSolveCountStats, !RelSolveCountStats, !IO) :-
    run_test_set(SolverInfo0, InstalledSets, TestSpec, cord.init,
        !AbsSolveCountStats, !RelSolveCountStats, !IO),
    run_test_sets(SolverInfo0, InstalledSets, TestSpecs,
        !AbsSolveCountStats, !RelSolveCountStats, !IO).

%---------------------------------------------------------------------------%

:- pred run_test_set(solver_info::in, list(installed_grade_set)::in,
    test_set_spec::in, test_spec::in,
    solve_count_stats::in, solve_count_stats::out,
    solve_count_stats::in, solve_count_stats::out, io::di, io::uo) is det.

run_test_set(SolverInfo0, InstalledSets, TestSetSpec, TestSpecSoFar0,
        !AbsSolveCountStats, !RelSolveCountStats, !IO) :-
    (
        TestSetSpec = [],
        run_test(SolverInfo0, InstalledSets, TestSpecSoFar0,
            !AbsSolveCountStats, !RelSolveCountStats, !IO)
    ;
        TestSetSpec = [TestSetSpecHead | TestSetSpecTail],
        TestSetSpecHead = test_set_component(VarId, ValueIds),
        run_alternatives_for_var(SolverInfo0, InstalledSets, VarId, ValueIds,
            TestSetSpecTail, TestSpecSoFar0,
            !AbsSolveCountStats, !RelSolveCountStats, !IO)
    ).

:- pred run_alternatives_for_var(solver_info::in,
    list(installed_grade_set)::in,
    solver_var_id::in, list(solver_var_value_id)::in,
    test_set_spec::in, test_spec::in,
    solve_count_stats::in, solve_count_stats::out,
    solve_count_stats::in, solve_count_stats::out, io::di, io::uo) is det.

run_alternatives_for_var(_SolverInfo0, _InstalledSets, _VarId, [],
        _TestSetSpecTail, _TestSpecSoFar0,
        !AbsSolveCountStats, !RelSolveCountStats, !IO).
run_alternatives_for_var(SolverInfo0, InstalledSets,
        VarId, [ValueId | ValueIds],
        TestSetSpecTail, TestSpecSoFar0,
        !AbsSolveCountStats, !RelSolveCountStats, !IO) :-
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
    run_test_set(SolverInfoForThisAlternative, InstalledSets, TestSetSpecTail,
        TestSpecSoFarForThisAlternative,
        !AbsSolveCountStats, !RelSolveCountStats, !IO),
    run_alternatives_for_var(SolverInfo0, InstalledSets, VarId, ValueIds,
        TestSetSpecTail, TestSpecSoFar0,
        !AbsSolveCountStats, !RelSolveCountStats, !IO).

:- pred run_test(solver_info::in, list(installed_grade_set)::in,
    test_spec::in,
    solve_count_stats::in, solve_count_stats::out,
    solve_count_stats::in, solve_count_stats::out, io::di, io::uo) is det.

run_test(SolverInfo0, InstalledSets, TestSpec,
        !AbsSolveCountStats, !RelSolveCountStats, !IO) :-
    solve_absolute(SolverInfo0, SolveCounts, Soln),
    accumulate_solve_count_stats(SolveCounts, !AbsSolveCountStats),

    !.AbsSolveCountStats = solve_count_stats(_, _, _, NumTests),
    SolveCounts = solve_counts(NumLabelSteps, NumPasses, NumReqTests),

    io.nl(!IO),
    io.write_string(test_spec_to_string("", NumTests, TestSpec), !IO),
    io.format(
        "ABS PERF: %2d label steps, %2d passes, %3d requirement tests\n",
        [i(NumLabelSteps), i(NumPasses), i(NumReqTests)], !IO),
    io.write_string(soln_to_str("    ", Soln), !IO),
    (
        Soln = soln_failure(_)
        % soln_to_str has already printed FAILURE
    ;
        Soln = soln_success(SuccMap),
        GradeVars = success_map_to_grade_vars(SuccMap),
        GradeStructure = grade_vars_to_grade_structure(GradeVars),
        UserGradeStr = grade_structure_to_grade_string(grade_string_user,
            GradeStructure),
        LinkGradeStr = grade_structure_to_grade_string(grade_string_link_check,
            GradeStructure),
        io.format("    ABS GRADE USER %s\n", [s(UserGradeStr)], !IO),
        io.format("    ABS GRADE LINK CHECK %s\n", [s(LinkGradeStr)], !IO),

        list.foldl2(run_installed_grade_set_test(SolverInfo0), InstalledSets,
            !RelSolveCountStats, !IO)
    ).

:- pred run_installed_grade_set_test(solver_info::in, installed_grade_set::in,
    solve_count_stats::in, solve_count_stats::out, io::di, io::uo) is det.

run_installed_grade_set_test(SolverInfo, InstalledSet,
        !RelSolveCountStats, !IO) :-
    InstalledSet = installed_grade_set(SetName, InstalledGrades),
    solve_best_installed_grade(SolverInfo, should_commit, InstalledGrades,
        CommitSolveCounts, CommitInstalledGradeSoln),
    solve_best_installed_grade(SolverInfo, should_not_commit, InstalledGrades,
        _NonCommitSolveCounts, NonCommitInstalledGradeSoln),
    accumulate_solve_count_stats(CommitSolveCounts, !RelSolveCountStats),
    io.format("    installed grade set %-20s ", [s(SetName)], !IO),
    (
        CommitInstalledGradeSoln = no_such_installed_grade,
        io.write_string("-\n", !IO)
    ;
        CommitInstalledGradeSoln = installed_grade_spec_is_inconsistent(_),
        io.write_string("INCONSISTENT\n", !IO)
    ;
        CommitInstalledGradeSoln =
            installed_grade_success(ChosenInstalledGrade),
        ChosenInstalledGrade = installed_grade(ChosenInstalledGradeName, _),
        io.format("%s\n", [s(ChosenInstalledGradeName)], !IO)
    ),
    ( if CommitInstalledGradeSoln = NonCommitInstalledGradeSoln then
        true
    else
        io.write_string("NONCOMMIT IS DIFFERENT%s\n", !IO)
    ).

:- pred accumulate_solve_count_stats(solve_counts::in,
    solve_count_stats::in, solve_count_stats::out) is det.

accumulate_solve_count_stats(SolveCounts, !SolveCountStats) :-
    !.SolveCountStats = solve_count_stats(TotalNumLabelSteps0,
        TotalNumPasses0, TotalNumReqTests0, NumTests0),
    SolveCounts = solve_counts(NumLabelSteps, NumPasses, NumReqTests),
    TotalNumLabelSteps = TotalNumLabelSteps0 + NumLabelSteps,
    TotalNumPasses = TotalNumPasses0 + NumPasses,
    TotalNumReqTests = TotalNumReqTests0 + NumReqTests,
    NumTests = NumTests0 + 1,
    !:SolveCountStats = solve_count_stats(TotalNumLabelSteps,
        TotalNumPasses, TotalNumReqTests, NumTests).

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
