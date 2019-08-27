%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% A test program that checks whether all the possible values of the grade
% structure type are valid. It does this by generating all those values,
% and then checking, for each value, whether
%
% - converting it to a grade string,
% - converting the grade string to a partial grade specification,
% - solving that specification, and
% - converting the solution back to a grade structure
%
% (a) succeeds, and (b) yields the exact same grade structure that
% we started with.
%
% Besides testing whether the grade structure type is "tight" in the sense
% of being unable to express any invalid grades, this round-trip test also
% stress-tests all the parts of the grade library except those that explain
% the inconsistencies in invalid grade specifications.
%

:- module try_all_grade_structs.
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

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module solutions.
:- import_module string.

main(!IO) :-
    SpecsVersion = specs_version_0,
    AutoconfResults = autoconf_results(autoconf_gcc_regs_avail_yes,
        autoconf_gcc_gotos_avail_yes, autoconf_gcc_labels_avail_yes,
        autoconf_low_tag_bits_avail_3, autoconf_size_of_double_eq_ptr,
        autoconf_merc_file_no),
    setup_solver_info(SpecsVersion, AutoconfResults, SolverInfo0),

    solutions(generate_all_tests, GradeStructures),
    list.length(GradeStructures, NumGradeStructures),
    io.format("There are %d grade structures to test.\n",
        [i(NumGradeStructures)], !IO),
    list.foldl2(check_grade_struct(SolverInfo0), GradeStructures, 0, _, !IO).

%---------------------------------------------------------------------------%

:- pred generate_all_tests(grade_structure::out) is multi.

generate_all_tests(GradeStructure) :-
    ( generate_pregen_tests(GradeStructure)
    ; generate_llds_tests(GradeStructure)
    ; generate_mlds_tests(GradeStructure)
    ; generate_elds_tests(GradeStructure)
    ).

%---------------------%

:- pred generate_pregen_tests(grade_structure::out) is multi.

generate_pregen_tests(GradeStructure) :-
    ( PregenKind = pregen_mlds_hlc
    ; PregenKind = pregen_llds_none
    ; PregenKind = pregen_llds_reg
    ; PregenKind = pregen_llds_asm_fast
    ),
    GradeStructure = grade_pregen(PregenKind).

%---------------------%

:- pred generate_llds_tests(grade_structure::out) is multi.

generate_llds_tests(GradeStructure) :-
    ( GccConf = grade_var_gcc_conf_none
    ; GccConf = grade_var_gcc_conf_reg
    ; GccConf = grade_var_gcc_conf_jump
    ; GccConf = grade_var_gcc_conf_fast
    ; GccConf = grade_var_gcc_conf_asm_jump
    ; GccConf = grade_var_gcc_conf_asm_fast
    ),
    ( StackLen = grade_var_stack_len_std
    ; StackLen = grade_var_stack_len_segments
    ; StackLen = grade_var_stack_len_extend
    ),
    (
        generate_c_gc(CGc),
        ( CTrail = c_trail_no
        ; CTrail = c_trail_yes(grade_var_trail_segments_no)
        ; CTrail = c_trail_yes(grade_var_trail_segments_yes)
        ),
        (
            LLDSPerfProf = llds_perf_prof_none
        ;
            LLDSPerfProf = llds_perf_prof_deep
        ;
            ( MProfTime = grade_var_mprof_time_no
            ; MProfTime = grade_var_mprof_time_yes
            ),
            ( MProfMemory = grade_var_mprof_memory_no
            ; MProfMemory = grade_var_mprof_memory_yes
            ),
            LLDSPerfProf = llds_perf_prof_mprof(MProfTime, MProfMemory)
        ),
        ( TermSizeProf = grade_var_term_size_prof_no
        ; TermSizeProf = grade_var_term_size_prof_cells
        ; TermSizeProf = grade_var_term_size_prof_words
        ),
        ( Debug = grade_var_debug_none
        ; Debug = grade_var_debug_debug
        ; Debug = grade_var_debug_decldebug
        ),
        (
            RBMM = llds_rbmm_no
        ;
            ( RBMMDebug = grade_var_rbmm_debug_no
            ; RBMMDebug = grade_var_rbmm_debug_yes
            ),
            ( RBMMProf = grade_var_rbmm_prof_no
            ; RBMMProf = grade_var_rbmm_prof_yes
            ),
            RBMM = llds_rbmm_yes(RBMMDebug, RBMMProf)
        ),
        LLDSTSMinModel = llds_thread_safe_no_minmodel_no(CGc, CTrail,
            LLDSPerfProf, TermSizeProf, Debug, RBMM)
    ;
        ( MinModelKind = lmk_stack_copy
        ; MinModelKind = lmk_stack_copy_debug
        ; MinModelKind = lmk_own_stack
        ; MinModelKind = lmk_own_stack_debug
        ),
        ( LLDSMMGc = llds_mm_gc_bdw
        ; LLDSMMGc = llds_mm_gc_bdw_debug
        ),
        ( Debug = grade_var_debug_none
        ; Debug = grade_var_debug_debug
        ; Debug = grade_var_debug_decldebug
        ),
        LLDSTSMinModel = llds_thread_safe_no_minmodel_yes(MinModelKind,
            LLDSMMGc, Debug)
    ;
        generate_thread_safe_c_gc(ThreadSafeCGc),
        ( CTrail = c_trail_no
        ; CTrail = c_trail_yes(grade_var_trail_segments_no)
        ; CTrail = c_trail_yes(grade_var_trail_segments_yes)
        ),
        ( TScopeProf = grade_var_tscope_prof_no
        ; TScopeProf = grade_var_tscope_prof_yes
        ),
        LLDSTSMinModel = llds_thread_safe_yes_minmodel_no(ThreadSafeCGc,
            CTrail, TScopeProf)
    ),
    MercFile = grade_var_merc_file_no,
    generate_grade_var_low_tag_bits_use(LowTagBitsUse),
    generate_grade_var_merc_float(MercFloat),
    generate_grade_var_target_debug(TargetDebug),
    GradeStructure = grade_llds(GccConf, StackLen, LLDSTSMinModel,
        MercFile, LowTagBitsUse, MercFloat, TargetDebug).

%---------------------%

:- pred generate_mlds_tests(grade_structure::out) is multi.

generate_mlds_tests(GradeStructure) :-
    (
        generate_mlds_c_target(MLDSTarget)
    ;
        generate_grade_var_ssdebug(SSDebug),
        MLDSTarget = mlds_target_csharp(SSDebug)
    ;
        generate_grade_var_ssdebug(SSDebug),
        MLDSTarget = mlds_target_java(SSDebug)
    ),
    generate_grade_var_target_debug(TargetDebug),
    GradeStructure = grade_mlds(MLDSTarget, TargetDebug).

:- pred generate_mlds_c_target(mlds_target::out) is multi.

generate_mlds_c_target(MLDSCTarget) :-
    ( MLDSCDataRep = mlds_c_datarep_heap_cells
    ; MLDSCDataRep = mlds_c_datarep_classes
    ),
    (
        generate_c_gc(CGc),
        (
            MLDSCPerfProf = mlds_c_perf_prof_none
        ;
            ( MProfTime = grade_var_mprof_time_no
            ; MProfTime = grade_var_mprof_time_yes
            ),
            ( MProfMemory = grade_var_mprof_memory_no
            ; MProfMemory = grade_var_mprof_memory_yes
            ),
            MLDSCPerfProf = mlds_c_perf_prof_mprof(MProfTime, MProfMemory)
        ),
        generate_grade_var_ssdebug(SSDebug),
        MLDSCThreadSafe = mlds_c_thread_safe_no(CGc, MLDSCPerfProf, SSDebug)
    ;
        generate_thread_safe_c_gc(ThreadSafeCGc),
        MLDSCThreadSafe = mlds_c_thread_safe_yes(ThreadSafeCGc)
    ),
    ( CTrail = c_trail_no
    ; CTrail = c_trail_yes(grade_var_trail_segments_no)
    ; CTrail = c_trail_yes(grade_var_trail_segments_yes)
    ),
    MercFile = grade_var_merc_file_no,
    generate_grade_var_low_tag_bits_use(LowTagBitsUse),
    generate_grade_var_merc_float(MercFloat),
    MLDSCTarget = mlds_target_c(MLDSCDataRep, MLDSCThreadSafe,
        CTrail, MercFile, LowTagBitsUse, MercFloat).

%---------------------%

:- pred generate_elds_tests(grade_structure::out) is multi.

generate_elds_tests(GradeStructure) :-
    generate_grade_var_ssdebug(SSDebug),
    generate_grade_var_target_debug(TargetDebug),
    GradeStructure = grade_elds(SSDebug, TargetDebug).

%---------------------%

:- pred generate_c_gc(c_gc::out) is multi.

generate_c_gc(c_gc_none).
generate_c_gc(c_gc_bdw).
generate_c_gc(c_gc_bdw_debug).
generate_c_gc(c_gc_accurate).
generate_c_gc(c_gc_history).

:- pred generate_thread_safe_c_gc(thread_safe_c_gc::out) is multi.

generate_thread_safe_c_gc(thread_safe_c_gc_none).
generate_thread_safe_c_gc(thread_safe_c_gc_bdw).
generate_thread_safe_c_gc(thread_safe_c_gc_bdw_debug).

:- pred generate_grade_var_ssdebug(grade_var_ssdebug::out) is multi.

generate_grade_var_ssdebug(grade_var_ssdebug_no).
generate_grade_var_ssdebug(grade_var_ssdebug_yes).

:- pred generate_grade_var_target_debug(grade_var_target_debug::out) is multi.

generate_grade_var_target_debug(grade_var_target_debug_no).
generate_grade_var_target_debug(grade_var_target_debug_yes).

:- pred generate_grade_var_low_tag_bits_use(grade_var_low_tag_bits_use::out)
    is multi.
:- pragma no_determinism_warning(generate_grade_var_low_tag_bits_use/1).

generate_grade_var_low_tag_bits_use(LowTagBitsUse) :-
    % ( LowTagBitsUse = grade_var_low_tag_bits_use_0
    % ; LowTagBitsUse = grade_var_low_tag_bits_use_2
    % ; LowTagBitsUse = grade_var_low_tag_bits_use_3
    % ),
    LowTagBitsUse = grade_var_low_tag_bits_use_3.

:- pred generate_grade_var_merc_float(grade_var_merc_float::out) is multi.

generate_grade_var_merc_float(grade_var_merc_float_is_unboxed_c_double).
generate_grade_var_merc_float(grade_var_merc_float_is_unboxed_c_float).
% As long as we generate only grade_var_low_tag_bits_use_3,
% returning grade_var_merc_float_is_boxed_c_double would NOT make sense.

%---------------------------------------------------------------------------%

:- pred check_grade_struct(solver_info::in, grade_structure::in,
    int::in, int::out, io::di, io::uo) is det.

check_grade_struct(SolverInfo0, GradeStructure, !N, !IO) :-
    GradeStr = grade_structure_to_grade_string(grade_string_user,
        GradeStructure),
    !:N = !.N + 1,
    io.format( "TEST %d: grade struct %s\n", [i(!.N), s(GradeStr)], !IO),

    MaybeSpecSuccMap = grade_string_to_succ_soln(GradeStr),
    (
        MaybeSpecSuccMap = error(HeadErrorMsg, TailErrorMsgs),
        CombinedErrorMsg =
            string.join_list(", ", [HeadErrorMsg | TailErrorMsgs]),
        io.format("ERROR: cannot convert to succ soln: %s\n",
            [s(CombinedErrorMsg)], !IO)
    ;
        MaybeSpecSuccMap = ok(SpecSuccMap),
        SolverVarMap0 = SolverInfo0 ^ si_solver_var_map,
        map.foldl(assign_var_in_map(npw_config), SpecSuccMap, 
            SolverVarMap0, SolverVarMap),
        SolverInfo = SolverInfo0 ^ si_solver_var_map := SolverVarMap,
        solve_absolute(SolverInfo, _SolveCounts, Soln),
        (
            Soln = soln_failure(_),
            FailureStr0 = soln_to_str(" ", Soln),
            string.replace_all(FailureStr0, "\n", " ", FailureStr),
            io.format("ERROR: cannot solve succ soln\n%s\n",
                [s(FailureStr)], !IO)
        ;
            Soln = soln_success(StdSuccMap),
            StdGradeVars = success_map_to_grade_vars(StdSuccMap),
            StdGradeStructure = grade_vars_to_grade_structure(StdGradeVars),
            ( if StdGradeStructure = GradeStructure then
                true
            else
                StdGradeStr =
                    grade_structure_to_grade_string(grade_string_user,
                        StdGradeStructure),
                io.format("ERROR: round trip does not match: %s\n",
                    [s(StdGradeStr)], !IO),
                io.write(GradeStructure, !IO),
                io.nl(!IO),
                io.write(StdGradeStructure, !IO),
                io.nl(!IO)
            )
        )
    ).

%---------------------------------------------------------------------------%
:- end_module try_all_grade_structs.
%---------------------------------------------------------------------------%
