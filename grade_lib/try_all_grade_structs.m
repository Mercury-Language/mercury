%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2016 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

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

:- import_module bool.
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
    ( generate_llds_tests(GradeStructure)
    ; generate_mlds_tests(GradeStructure)
    ; generate_elds_tests(GradeStructure)
    ).

%---------------------%

:- pred generate_llds_tests(grade_structure::out) is multi.

generate_llds_tests(GradeStructure) :-
    ( GccConf = llds_gcc_conf_none
    ; GccConf = llds_gcc_conf_reg
    ; GccConf = llds_gcc_conf_jump
    ; GccConf = llds_gcc_conf_fast
    ; GccConf = llds_gcc_conf_asm_jump
    ; GccConf = llds_gcc_conf_asm_fast
    ),
    ( StackLen = grade_var_stack_len_std
    ; StackLen = grade_var_stack_len_segments
    ; StackLen = grade_var_stack_len_extend
    ),
    (
        ( MinModelKind = lmk_stack_copy
        ; MinModelKind = lmk_stack_copy_debug
        ; MinModelKind = lmk_own_stack
        ; MinModelKind = lmk_own_stack_debug
        ),
        ( LLDSMMGc = llds_mm_gc_none
        ; LLDSMMGc = llds_mm_gc_bdw
        ; LLDSMMGc = llds_mm_gc_bdw_debug
        ),
        MinModel = llds_minmodel_yes(MinModelKind, LLDSMMGc)
    ;
        ( LLDSGc = llds_gc_none
        ; LLDSGc = llds_gc_bdw
        ; LLDSGc = llds_gc_bdw_debug
        ; LLDSGc = llds_gc_history
        ),
        ( CTrail = c_trail_no
        ; CTrail = c_trail_yes(grade_var_trail_segments_no)
        ; CTrail = c_trail_yes(grade_var_trail_segments_yes)
        ),
        ( ThreadSafe = llds_thread_safe_no
        ; ThreadSafe = llds_thread_safe_yes(grade_var_tscope_prof_no)
        ; ThreadSafe = llds_thread_safe_yes(grade_var_tscope_prof_yes)
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
        MinModel = llds_minmodel_no(LLDSGc, CTrail, ThreadSafe, LLDSPerfProf)
    ),
    ( TermSizeProf = grade_var_term_size_prof_no
    ; TermSizeProf = grade_var_term_size_prof_cells
    ; TermSizeProf = grade_var_term_size_prof_words
    ),
    ( Debug = grade_var_debug_none
    ; Debug = grade_var_debug_debug
    ; Debug = grade_var_debug_decldebug
    ),
    ( LLDebug = grade_var_lldebug_no
    ; LLDebug = grade_var_lldebug_yes
    ),
    (
        Rbmm = llds_rbmm_no
    ;
        ( RbmmDebug = grade_var_rbmm_debug_no
        ; RbmmDebug = grade_var_rbmm_debug_yes
        ),
        ( RbmmProf = grade_var_rbmm_prof_no
        ; RbmmProf = grade_var_rbmm_prof_yes
        ),
        Rbmm = llds_rbmm_yes(RbmmDebug, RbmmProf)
    ),
    MercFile = grade_var_merc_file_no,
    generate_low_tags_floats(LowTagsFloats),
    GradeStructure = grade_llds(GccConf, StackLen, MinModel,
        TermSizeProf, Debug, LLDebug, Rbmm, MercFile, LowTagsFloats).

%---------------------%

:- pred generate_mlds_tests(grade_structure::out) is multi.

generate_mlds_tests(GradeStructure) :-
    ( generate_mlds_c_target(MLDSTarget)
    ; MLDSTarget = mlds_target_csharp
    ; MLDSTarget = mlds_target_java
    ),
    generate_grade_var_ssdebug(SSDebug),
    GradeStructure = grade_mlds(MLDSTarget, SSDebug).

:- pred generate_mlds_c_target(mlds_target::out) is multi.

generate_mlds_c_target(MLDSCTarget) :-
    ( MLDSCDataRep = mlds_c_datarep_heap_cells
    ; MLDSCDataRep = mlds_c_datarep_classes
    ),
    ( NestedFuncs = grade_var_nested_funcs_no
    ; NestedFuncs = grade_var_nested_funcs_yes
    ),
    generate_grade_var_thread_safe(ThreadSafe),
    ( MLDSCGc = mlds_c_gc_none
    ; MLDSCGc = mlds_c_gc_bdw
    ; MLDSCGc = mlds_c_gc_bdw_debug
    ; MLDSCGc = mlds_c_gc_accurate
    ; MLDSCGc = mlds_c_gc_history
    ),
    ( CTrail = c_trail_no
    ; CTrail = c_trail_yes(grade_var_trail_segments_no)
    ; CTrail = c_trail_yes(grade_var_trail_segments_yes)
    ),
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
    MercFile = grade_var_merc_file_no,
    generate_low_tags_floats(LowTagsFloats),
    MLDSCTarget = mlds_target_c(MLDSCDataRep, NestedFuncs, ThreadSafe,
        MLDSCGc, CTrail, MLDSCPerfProf, MercFile, LowTagsFloats).

%---------------------%

:- pred generate_elds_tests(grade_structure::out) is multi.

generate_elds_tests(GradeStructure) :-
    generate_grade_var_ssdebug(SSDebug),
    GradeStructure = grade_elds(SSDebug).

%---------------------%

:- pred generate_grade_var_thread_safe(grade_var_thread_safe::out) is multi.

generate_grade_var_thread_safe(grade_var_thread_safe_no).
generate_grade_var_thread_safe(grade_var_thread_safe_yes).

:- pred generate_grade_var_ssdebug(grade_var_ssdebug::out) is multi.

generate_grade_var_ssdebug(grade_var_ssdebug_no).
generate_grade_var_ssdebug(grade_var_ssdebug_yes).

:- pred generate_low_tags_floats(low_tags_floats::out) is multi.

generate_low_tags_floats(LowTagsFloats) :-
    (
        % ( LowTagBitsUse = grade_var_low_tag_bits_use_0
        % ; LowTagBitsUse = grade_var_low_tag_bits_use_2
        % ; LowTagBitsUse = grade_var_low_tag_bits_use_3
        % ),
        % ( MercFloat = grade_var_merc_float_is_unboxed_c_double
        % ; MercFloat = grade_var_merc_float_is_boxed_c_double
        % ; MercFloat = grade_var_merc_float_is_unboxed_c_float
        % ),
        LowTagBitsUse = grade_var_low_tag_bits_use_3,
        ( MercFloat = grade_var_merc_float_is_unboxed_c_double
        ; MercFloat = grade_var_merc_float_is_unboxed_c_float
        ),
        LowTagsFloats = low_tags_floats_pregen_no(LowTagBitsUse, MercFloat)
    ;
        LowTagsFloats = low_tags_floats_pregen_yes
    ).

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
