%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_string.
:- interface.

:- import_module grade_components.
:- import_module grade_solver.

:- import_module maybe.

%---------------------------------------------------------------------------%

:- type grade
    --->    grade_llds(
                llds_gcc_conf,
                soln_low_tag_bits_use,
                soln_stack_len,
                llds_gc,
                c_trail,
                llds_thread_safe,
                llds_perf_prof,
                soln_term_size_prof,
                soln_minmodel,
                soln_debug,
                soln_lldebug,
                llds_rbmm,
                soln_merc_file,
                soln_pregen,
                soln_single_prec_float
            )
    ;       grade_mlds(
                mlds_target,
                soln_ssdebug
            )
    ;       grade_elds(
                soln_ssdebug
            ).

:- type llds_gcc_conf                 % labels, gotos,  regs
    --->    llds_gcc_conf_none        % no      no      no
    ;       llds_gcc_conf_reg         % no      no      yes
    ;       llds_gcc_conf_jump        % no      yes     no
    ;       llds_gcc_conf_fast        % no      yes     yes
    ;       llds_gcc_conf_asm_jump    % yes     yes     no
    ;       llds_gcc_conf_asm_fast.   % yes     yes     yes

:- type c_trail
    --->    c_trail_no
    ;       c_trail_yes(
                soln_trail_segments
            ).

:- type llds_gc
    --->    llds_gc_none
    ;       llds_gc_bdw
    ;       llds_gc_bdw_debug
    ;       llds_gc_history.

:- type llds_thread_safe
    --->    llds_thread_safe_no
    ;       llds_thread_safe_yes(
                soln_tscope_prof
            ).

:- type llds_perf_prof
    --->    llds_perf_prof_none
    ;       llds_perf_prof_deep
    ;       llds_perf_prof_mprof(
                soln_mprof_time,
                soln_mprof_memory
            ).

:- type llds_rbmm
    --->    llds_rbmm_no
    ;       llds_rbmm_yes(
                soln_rbmm_debug,
                soln_rbmm_prof
            ).

:- type mlds_target
    --->    mlds_target_c(
                soln_data_level,
                soln_nested_funcs,
                soln_low_tag_bits_use,
                soln_thread_safe,
                mlds_c_gc,
                c_trail,
                mlds_c_perf_prof,
                soln_merc_file,
                soln_pregen,
                soln_single_prec_float
            )
    ;       mlds_target_csharp
    ;       mlds_target_java.

:- type mlds_c_gc
    --->    mlds_c_gc_none
    ;       mlds_c_gc_bdw
    ;       mlds_c_gc_bdw_debug
    ;       mlds_c_gc_accurate
    ;       mlds_c_gc_history.

:- type mlds_c_perf_prof
    --->    mlds_c_perf_prof_none
    ;       mlds_c_perf_prof_mprof(
                soln_mprof_time,
                soln_mprof_memory
            ).

:- func success_soln_to_grade(success_soln_map) = grade.

%---------------------------------------------------------------------------%

:- type which_grade_string
    --->    grade_string_user
    ;       grade_string_link_check.

:- func grade_to_grade_string(which_grade_string, grade) = string.

%---------------------------------------------------------------------------%

:- func grade_string_to_succ_soln(string) =
    maybe_errors(success_soln_map, string).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module grade_spec.

:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

success_soln_to_grade(SuccMap) = Grade :-
    GradeComponents = collect_grade_components(SuccMap),

    % XXX We want to verify that every solution component is used on every
    % path, for one of these three things: (a) to make a decision, (c) to check
    % that it has the expected value, or (c) to record its value in the grade.
    %
    % We pick up the values of the other arguments in separate deconstructions
    % in each arm of the switch on Backend, to give us a singleton variable
    % warning if don't handle a solution component in an arm.
    %
    % Unfortunately, I (zs) don't see how to repeat the trick for the switch
    % on Target in the mlds case: having two deconstructs that each pick up
    % *some* arguments is vulnerable to not picking up some arguments
    % in *either*.
    GradeComponents = grade_components(Backend, _, _, _, _, _, _, _,
        _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _),

    % XXX The order of the grade components we generate may as yet differ
    % from the order in runtime/mercury_grade.h, or the canonical order
    % when targeting non-C languages.
    % XXX We should consider rationalizing the standard order.
    (
        Backend = soln_backend_llds,

        GradeComponents = grade_components(_Backend, DataLevel,
            Target, NestedFuncs, GccRegsUse, GccGotosUse, GccLabelsUse,
            LowTagBitsUse, StackLen, Trail, TrailSegments,
            MinimalModel, ThreadSafe, Gc,
            DeepProf, MprofCall, MprofTime, MprofMemory, TScopeProf,
            TermSizeProf, Debug, SSDebug, LLDebug, RBMM, RBMMDebug, RBMMProf,
            MercFile, Pregen, SinglePrecFloat),

        expect(unify(DataLevel, soln_data_level_lld), $pred,
            "DataLevel != soln_data_level_lld"),
        expect(unify(Target, soln_target_c), $pred,
            "Target != soln_target_c"),
        expect(unify(NestedFuncs, soln_nested_funcs_no), $pred,
            "NestedFuncs != soln_nested_funcs_no"),
        expect(unify(SSDebug, soln_ssdebug_no), $pred,
            "SSDebug != soln_ssdebug_no"),
        (
            GccLabelsUse = soln_gcc_labels_use_no,
            (
                GccGotosUse = soln_gcc_gotos_use_no,
                (
                    GccRegsUse = soln_gcc_regs_use_no,
                    LLDSGccConf = llds_gcc_conf_none
                ;
                    GccRegsUse = soln_gcc_regs_use_yes,
                    LLDSGccConf = llds_gcc_conf_reg
                )
            ;
                GccGotosUse = soln_gcc_gotos_use_yes,
                (
                    GccRegsUse = soln_gcc_regs_use_no,
                    LLDSGccConf = llds_gcc_conf_jump
                ;
                    GccRegsUse = soln_gcc_regs_use_yes,
                    LLDSGccConf = llds_gcc_conf_fast
                )
            )
        ;
            GccLabelsUse = soln_gcc_labels_use_yes,
            (
                GccGotosUse = soln_gcc_gotos_use_no,
                unexpected($pred, "GccUseLabels = yes, GccUseGotos = no")
            ;
                GccGotosUse = soln_gcc_gotos_use_yes,
                (
                    GccRegsUse = soln_gcc_regs_use_no,
                    LLDSGccConf = llds_gcc_conf_asm_jump
                ;
                    GccRegsUse = soln_gcc_regs_use_yes,
                    LLDSGccConf = llds_gcc_conf_asm_fast
                )
            )
        ),
        (
            Trail = soln_trail_no,
            CTrail = c_trail_no
        ;
            Trail = soln_trail_yes,
            CTrail = c_trail_yes(TrailSegments)
        ),
        (
            Gc = soln_gc_none,
            LLDSGc = llds_gc_none
        ;
            Gc = soln_gc_target_native,
            unexpected($pred, "Target = c, Gc = target_native")
        ;
            Gc = soln_gc_bdw,
            LLDSGc = llds_gc_bdw
        ;
            Gc = soln_gc_bdw_debug,
            LLDSGc = llds_gc_bdw_debug
        ;
            Gc = soln_gc_accurate,
            unexpected($pred, "Backend = llds, Gc = accurate")
        ;
            Gc = soln_gc_history,
            % XXX Is this supported?
            LLDSGc = llds_gc_history
        ),
        (
            ThreadSafe = soln_thread_safe_no,
            expect(unify(TScopeProf, soln_tscope_prof_no), $pred,
                "TScopeProf != soln_tscope_prof_no"),
            LLDSThreadSafe = llds_thread_safe_no
        ;
            ThreadSafe = soln_thread_safe_yes,
            LLDSThreadSafe = llds_thread_safe_yes(TScopeProf)
        ),
        (
            DeepProf = soln_deep_prof_no,
            (
                MprofCall = soln_mprof_call_no,
                expect(unify(MprofTime, soln_mprof_time_no), $pred,
                    "MprofTime != soln_mprof_time_no"),
                expect(unify(MprofMemory, soln_mprof_memory_no), $pred,
                    "MprofMemory != soln_mprof_memory_no"),
                LLDSPerfProf = llds_perf_prof_none
            ;
                MprofCall = soln_mprof_call_yes,
                LLDSPerfProf = llds_perf_prof_mprof(MprofTime, MprofMemory)
            )
        ;
            DeepProf = soln_deep_prof_yes,
            expect(unify(MprofCall, soln_mprof_call_no), $pred,
                "MprofCall != soln_mprof_call_no"),
            expect(unify(MprofTime, soln_mprof_time_no), $pred,
                "MprofTime != soln_mprof_time_no"),
            expect(unify(MprofMemory, soln_mprof_memory_no), $pred,
                "MprofMemory != soln_mprof_memory_no"),
            LLDSPerfProf = llds_perf_prof_deep
        ),
        (
            RBMM = soln_rbmm_no,
            expect(unify(RBMMDebug, soln_rbmm_debug_no), $pred,
                "RBMMDebug != soln_rbmm_debug_no"),
            expect(unify(RBMMProf, soln_rbmm_prof_no), $pred,
                "RBMMProf != soln_rbmm_prof_no"),
            LLDSRBMM = llds_rbmm_no
        ;
            RBMM = soln_rbmm_yes,
            LLDSRBMM = llds_rbmm_yes(RBMMDebug, RBMMProf)
        ),
        Grade = grade_llds(LLDSGccConf, LowTagBitsUse, StackLen, LLDSGc,
            CTrail, LLDSThreadSafe, LLDSPerfProf, TermSizeProf,
            MinimalModel, Debug, LLDebug, LLDSRBMM,
            MercFile, Pregen, SinglePrecFloat)
    ;
        Backend = soln_backend_mlds,

        GradeComponents = grade_components(_Backend, DataLevel,
            Target, NestedFuncs, GccRegsUse, GccGotosUse, GccLabelsUse,
            LowTagBitsUse, StackLen, Trail, TrailSegments,
            MinimalModel, ThreadSafe, Gc,
            DeepProf, MprofCall, MprofTime, MprofMemory, TScopeProf,
            TermSizeProf, Debug, SSDebug, LLDebug, RBMM, RBMMDebug, RBMMProf,
            MercFile, Pregen, SinglePrecFloat),

        expect(unify(GccRegsUse, soln_gcc_regs_use_no), $pred,
            "GccRegsUse != soln_gcc_regs_use_no"),
        expect(unify(GccGotosUse, soln_gcc_gotos_use_no), $pred,
            "GccGotosUse != soln_gcc_gotos_use_no"),
        expect(unify(GccLabelsUse, soln_gcc_labels_use_no), $pred,
            "GccLabelsUse != soln_gcc_labels_use_no"),
        expect(unify(StackLen, soln_stack_len_std), $pred,
            "StackLen != soln_stack_len_std"),
        expect(unify(MinimalModel, soln_minmodel_no), $pred,
            "MinimalModel != soln_minmodel_no"),
        expect(unify(DeepProf, soln_deep_prof_no), $pred,
            "DeepProf != soln_deep_prof_no"),
        expect(unify(TScopeProf, soln_tscope_prof_no), $pred,
            "TScopeProf != soln_tscope_prof_no"),
        expect(unify(TermSizeProf, soln_term_size_prof_no), $pred,
            "TermSizeProf != soln_term_size_prof_no"),
        expect(unify(Debug, soln_debug_none), $pred,
            "Debug != soln_debug_none"),
        expect(unify(LLDebug, soln_lldebug_no), $pred,
            "LLDebug != soln_lldebug_no"),
        expect(unify(RBMM, soln_rbmm_no), $pred,
            "RBMM != soln_rbmm_no"),
        expect(unify(RBMMDebug, soln_rbmm_debug_no), $pred,
            "RBMMDebug != soln_rbmm_debug_no"),
        expect(unify(RBMMProf, soln_rbmm_prof_no), $pred,
            "RBMMProf != soln_rbmm_prof_no"),
        (
            Target = soln_target_c,
            % XXX We should switch on Pregen, and if it is soln_pregen_yes,
            % then we should return grade_pregen, not grade_mlds(...).
            % If we cannot do that, because soln_pregen_yes does not COMPLETELY
            % determine the value of ALL the other solver variables, then
            % we should include Pregen as a field of mlds_target_c.
            (
                Gc = soln_gc_none,
                MLDSGc = mlds_c_gc_none
            ;
                Gc = soln_gc_target_native,
                unexpected($pred, "Target = c, Gc = target_native")
            ;
                Gc = soln_gc_bdw,
                MLDSGc = mlds_c_gc_bdw
            ;
                Gc = soln_gc_bdw_debug,
                MLDSGc = mlds_c_gc_bdw_debug
            ;
                Gc = soln_gc_accurate,
                MLDSGc = mlds_c_gc_accurate
            ;
                Gc = soln_gc_history,
                % XXX Is this supported?
                MLDSGc = mlds_c_gc_history
            ),
            (
                Trail = soln_trail_no,
                CTrail = c_trail_no
            ;
                Trail = soln_trail_yes,
                CTrail = c_trail_yes(TrailSegments)
            ),
            (
                MprofCall = soln_mprof_call_no,
                expect(unify(MprofTime, soln_mprof_time_no), $pred,
                    "MprofTime != soln_mprof_time_no"),
                expect(unify(MprofMemory, soln_mprof_memory_no), $pred,
                    "MprofMemory != soln_mprof_memory_no"),
                MLDSPerfProf = mlds_c_perf_prof_none
            ;
                MprofCall = soln_mprof_call_yes,
                MLDSPerfProf = mlds_c_perf_prof_mprof(MprofTime, MprofMemory)
            ),
            TargetC = mlds_target_c(DataLevel, NestedFuncs, LowTagBitsUse,
                ThreadSafe, MLDSGc, CTrail, MLDSPerfProf,
                MercFile, Pregen, SinglePrecFloat),
            Grade = grade_mlds(TargetC, SSDebug)
        ;
            ( Target = soln_target_csharp
            ; Target = soln_target_java
            ),
            expect(unify(Pregen, soln_pregen_no), $pred,
                "Pregen != soln_pregen_no"),
            expect(unify(DataLevel, soln_data_level_hld), $pred,
                "DataLevel != soln_data_level_hld"),
            expect(unify(NestedFuncs, soln_nested_funcs_no), $pred,
                "NestedFuncs != soln_nested_funcs_no"),
            expect(unify(ThreadSafe, soln_thread_safe_yes), $pred,
                "ThreadSafe != soln_thread_safe_yes"),
            expect(unify(Gc, soln_gc_target_native), $pred,
                "Gc != soln_gc_target_native"),
            expect(unify(Trail, soln_trail_no), $pred,
                "Trail != soln_trail_no"),
            expect(unify(TrailSegments, soln_trail_segments_no), $pred,
                "TrailSegments != soln_trail_segments_no"),
            expect(unify(MprofCall, soln_mprof_call_no), $pred,
                "MprofCall != soln_mprof_call_no"),
            expect(unify(MprofTime, soln_mprof_time_no), $pred,
                "MprofTime != soln_mprof_time_no"),
            expect(unify(MprofMemory, soln_mprof_memory_no), $pred,
                "MprofMemory != soln_mprof_memory_no"),
            % The definition of MR_NEW_MERCURYFILE_STRUCT applies only
            % to grades that target C. When targeting other languages,
            % we don't insist on MercFile = soln_merc_file_no. 
            expect(unify(Pregen, soln_pregen_no), $pred,
                "Pregen != soln_pregen_no"),
            expect(unify(SinglePrecFloat, soln_single_prec_float_no), $pred,
                "SinglePrecFloat != soln_single_prec_float_no"),
            % We repeat this here in case we later add some function symbols
            % to one or both of mlds_target_csharp/mlds_target_java.
            (
                Target = soln_target_csharp,
                MLDSTarget = mlds_target_csharp
            ;
                Target = soln_target_java,
                MLDSTarget = mlds_target_java
            ),
            Grade = grade_mlds(MLDSTarget, SSDebug)
        ;
            Target = soln_target_erlang,
            unexpected($pred, "Backend = mlds but Target = erlang")
        )
    ;
        Backend = soln_backend_elds,

        GradeComponents = grade_components(_Backend, DataLevel,
            Target, NestedFuncs, GccRegsUse, GccGotosUse, GccLabelsUse,
            _LowTagBitsUse, StackLen, Trail, TrailSegments,
            MinimalModel, ThreadSafe, Gc,
            DeepProf, MprofCall, MprofTime, MprofMemory, TScopeProf,
            TermSizeProf, Debug, SSDebug, LLDebug, RBMM, RBMMDebug, RBMMProf,
            _MercFile, Pregen, SinglePrecFloat),

        % XXX The ELDS backend's data representation is NOT the same
        % as the LLDS backends'. If it were, we couldn't ignore the value of
        % _LowTagBitsUse.
        expect(unify(DataLevel, soln_data_level_lld), $pred,
            "DataLevel != soln_data_level_lld"),
        expect(unify(Target, soln_target_erlang), $pred,
            "Target != soln_target_erlang"),
        expect(unify(NestedFuncs, soln_nested_funcs_no), $pred,
            "NestedFuncs != soln_nested_funcs_no"),
        expect(unify(GccRegsUse, soln_gcc_regs_use_no), $pred,
            "GccRegsUse != soln_gcc_regs_use_no"),
        expect(unify(GccGotosUse, soln_gcc_gotos_use_no), $pred,
            "GccGotosUse != soln_gcc_gotos_use_no"),
        expect(unify(GccLabelsUse, soln_gcc_labels_use_no), $pred,
            "GccLabelsUse != soln_gcc_labels_use_no"),
        expect(unify(StackLen, soln_stack_len_std), $pred,
            "StackLen != soln_stack_len_std"),
        expect(unify(Trail, soln_trail_no), $pred,
            "Trail != soln_trail_no"),
        expect(unify(TrailSegments, soln_trail_segments_no), $pred,
            "TrailSegments != soln_trail_segments_no"),
        expect(unify(MinimalModel, soln_minmodel_no), $pred,
            "MinimalModel != soln_minmodel_no"),
        expect(unify(ThreadSafe, soln_thread_safe_no), $pred,
            "ThreadSafe != soln_thread_safe_no"),
        expect(unify(Gc, soln_gc_target_native), $pred,
            "Gc != soln_gc_target_native"),
        expect(unify(DeepProf, soln_deep_prof_no), $pred,
            "DeepProf != soln_deep_prof_no"),
        expect(unify(MprofCall, soln_mprof_call_no), $pred,
            "MprofCall != soln_mprof_call_no"),
        expect(unify(MprofTime, soln_mprof_time_no), $pred,
            "MprofTime != soln_mprof_time_no"),
        expect(unify(MprofMemory, soln_mprof_memory_no), $pred,
            "MprofMemory != soln_mprof_memory_no"),
        expect(unify(TScopeProf, soln_tscope_prof_no), $pred,
            "TScopeProf != soln_tscope_prof_no"),
        expect(unify(TermSizeProf, soln_term_size_prof_no), $pred,
            "TermSizeProf != soln_term_size_prof_no"),
        expect(unify(Debug, soln_debug_none), $pred,
            "Debug != soln_debug_none"),
        expect(unify(LLDebug, soln_lldebug_no), $pred,
            "LLDebug != soln_lldebug_no"),
        expect(unify(RBMM, soln_rbmm_no), $pred,
            "RBMM != soln_rbmm_no"),
        expect(unify(RBMMDebug, soln_rbmm_debug_no), $pred,
            "RBMMDebug != soln_rbmm_debug_no"),
        expect(unify(RBMMProf, soln_rbmm_prof_no), $pred,
            "RBMMProf != soln_rbmm_prof_no"),
        % The definition of MR_NEW_MERCURYFILE_STRUCT applies only
        % to grades that target C. When targeting other languages,
        % we don't insist on MercFile = soln_merc_file_no. 
        expect(unify(Pregen, soln_pregen_no), $pred,
            "Pregen != soln_pregen_no"),
        expect(unify(SinglePrecFloat, soln_single_prec_float_no), $pred,
            "SinglePrecFloat != soln_single_prec_float_no"),
        % XXX probably incomplete
        Grade = grade_elds(SSDebug)
    ).

%---------------------------------------------------------------------------%

grade_to_grade_string(WhichGradeString, Grade) = GradeStr :-
    % XXX unboxed floats
    (
        Grade = grade_llds(GccConf, LowTagBitsUse, StackLen, LLDSGc,
            CTrail, LLDSThreadSafe, LLDSPerfProf, TermSizeProf, MinimalModel,
            Debug, LLDebug, LLDSRBMM, MercFile, Pregen, SinglePrecFloat),

        ( GccConf = llds_gcc_conf_none,             GccConfStr = "none"
        ; GccConf = llds_gcc_conf_reg,              GccConfStr = "reg"
        ; GccConf = llds_gcc_conf_jump,             GccConfStr = "jump"
        ; GccConf = llds_gcc_conf_fast,             GccConfStr = "fast"
        ; GccConf = llds_gcc_conf_asm_jump,         GccConfStr = "asm_jump"
        ; GccConf = llds_gcc_conf_asm_fast,         GccConfStr = "asm_fast"
        ),
        LowTagBitsUseStr =
            low_tag_bits_use_to_str(WhichGradeString, LowTagBitsUse),
        (
            LLDSThreadSafe = llds_thread_safe_no,
            ThreadSafeStr = thread_safe_to_str(soln_thread_safe_no),
            TScopeProfStr = ""
        ;
            LLDSThreadSafe = llds_thread_safe_yes(TScopeProf),
            ThreadSafeStr = thread_safe_to_str(soln_thread_safe_yes),
            (
                TScopeProf = soln_tscope_prof_no,
                TScopeProfStr = ""
            ;
                TScopeProf = soln_tscope_prof_yes,
                TScopeProfStr = ".threadscope"
            )
        ),
        ( LLDSGc = llds_gc_none,             Gc = soln_gc_none
        ; LLDSGc = llds_gc_bdw,              Gc = soln_gc_bdw
        ; LLDSGc = llds_gc_bdw_debug,        Gc = soln_gc_bdw_debug
        ; LLDSGc = llds_gc_history,          Gc = soln_gc_history
        ),
        GcStr = gc_to_str(Gc),
        (
            LLDSPerfProf = llds_perf_prof_none,
            LLDSPerfProfStr = ""
        ;
            LLDSPerfProf = llds_perf_prof_deep,
            LLDSPerfProfStr = ".profdeep"
        ;
            LLDSPerfProf = llds_perf_prof_mprof(MprofTime, MprofMemory),
            LLDSPerfProfStr = mprof_to_string(MprofTime, MprofMemory)
        ),
        ( TermSizeProf = soln_term_size_prof_no,    TermSizeProfStr = ""
        ; TermSizeProf = soln_term_size_prof_cells, TermSizeProfStr = ".tsc"
        ; TermSizeProf = soln_term_size_prof_words, TermSizeProfStr = ".tsw"
        ),
        TrailStr = c_trail_to_str(CTrail),
        (
            MinimalModel = soln_minmodel_no,
            MinimalModelStr = ""
        ;
            MinimalModel = soln_minmodel_stack_copy,
            MinimalModelStr = ".mmsc"
        ;
            MinimalModel = soln_minmodel_stack_copy_debug,
            MinimalModelStr = ".dmmsc"
        ;
            MinimalModel = soln_minmodel_own_stack,
            MinimalModelStr = ".mmos"
        ;
            MinimalModel = soln_minmodel_own_stack_debug,
            MinimalModelStr = ".dmmos"
        ),
        PregenStr = pregen_to_str(Pregen),
        SinglePrecFloatStr = single_prec_float_to_str(SinglePrecFloat),
        ( Debug = soln_debug_none,                  DebugStr = ""
        ; Debug = soln_debug_debug,                 DebugStr = ".debug"
        ; Debug = soln_debug_decldebug,             DebugStr = ".decldebug"
        ),
        ( LLDebug = soln_lldebug_no,                LLDebugStr = ""
        ; LLDebug = soln_lldebug_yes,               LLDebugStr = ".ll_debug"
        ),
        ( StackLen = soln_stack_len_std,            StackLenStr = ""
        ; StackLen = soln_stack_len_segments,       StackLenStr = ".stseg"
        ; StackLen = soln_stack_len_extend,         StackLenStr = ".exts"
        ),
        (
            LLDSRBMM = llds_rbmm_no,
            RBMMStr = ""
        ;
            LLDSRBMM = llds_rbmm_yes(RBMMDebug, RBMMProf),
            (
                RBMMDebug = soln_rbmm_debug_no,
                RBMMProf = soln_rbmm_prof_no,
                RBMMStr = ".rbmm"
            ;
                RBMMDebug = soln_rbmm_debug_no,
                RBMMProf = soln_rbmm_prof_yes,
                RBMMStr = ".rbmmp"
            ;
                RBMMDebug = soln_rbmm_debug_yes,
                RBMMProf = soln_rbmm_prof_no,
                RBMMStr = ".rbmmd"
            ;
                RBMMDebug = soln_rbmm_debug_yes,
                RBMMProf = soln_rbmm_prof_yes,
                RBMMStr = ".rbmmdp"
            )
        ),
        MercFileStr = merc_file_to_str(WhichGradeString, MercFile),
        GradeStr = string.append_list([GccConfStr, LowTagBitsUseStr,
            ThreadSafeStr, GcStr, LLDSPerfProfStr, TermSizeProfStr,
            TrailStr, MinimalModelStr, MercFileStr,
            PregenStr, SinglePrecFloatStr, DebugStr, LLDebugStr,
            StackLenStr, RBMMStr, TScopeProfStr])
    ;
        Grade = grade_mlds(MLDSTarget, SSDebug),
        SSDebugStr = ssdebug_to_str(SSDebug),
        (
            MLDSTarget = mlds_target_c(DataLevel, NestedFuncs, LowTagBitsUse,
                ThreadSafe, MLDSCGc, CTrail, MLDSPerfProf,
                MercFile, Pregen, SinglePrecFloat),
            ( DataLevel = soln_data_level_hld,      DataLevelStr = "hl"
            ; DataLevel = soln_data_level_lld,      DataLevelStr = "hlc"
            ),
            ( NestedFuncs = soln_nested_funcs_no,   NestedFuncsStr = ""
            ; NestedFuncs = soln_nested_funcs_yes,  NestedFuncsStr = "_nest"
            ),
            LowTagBitsUseStr =
                low_tag_bits_use_to_str(WhichGradeString, LowTagBitsUse),
            ( MLDSCGc = mlds_c_gc_none,             Gc = soln_gc_none
            ; MLDSCGc = mlds_c_gc_bdw,              Gc = soln_gc_bdw
            ; MLDSCGc = mlds_c_gc_bdw_debug,        Gc = soln_gc_bdw_debug
            ; MLDSCGc = mlds_c_gc_accurate,         Gc = soln_gc_accurate
            ; MLDSCGc = mlds_c_gc_history,          Gc = soln_gc_history
            ),
            ThreadSafeStr = thread_safe_to_str(ThreadSafe),
            GcStr = gc_to_str(Gc),
            TrailStr = c_trail_to_str(CTrail),
            (
                MLDSPerfProf = mlds_c_perf_prof_none,
                MLDSPerfProfStr = ""
            ;
                MLDSPerfProf = mlds_c_perf_prof_mprof(MprofTime, MprofMemory),
                MLDSPerfProfStr = mprof_to_string(MprofTime, MprofMemory)
            ),
            MercFileStr = merc_file_to_str(WhichGradeString, MercFile),
            PregenStr = pregen_to_str(Pregen),
            SinglePrecFloatStr = single_prec_float_to_str(SinglePrecFloat),
            GradeStr = string.append_list([DataLevelStr, NestedFuncsStr,
                LowTagBitsUseStr, ThreadSafeStr, SSDebugStr, GcStr, TrailStr,
                MLDSPerfProfStr, MercFileStr, PregenStr, SinglePrecFloatStr])
        ;
            MLDSTarget = mlds_target_csharp,
            GradeStr = string.append_list(["csharp", SSDebugStr])
        ;
            MLDSTarget = mlds_target_java,
            GradeStr = string.append_list(["java", SSDebugStr])
        )
    ;
        Grade = grade_elds(SSDebug),
        SSDebugStr = ssdebug_to_str(SSDebug),
        GradeStr = string.append_list(["erlang", SSDebugStr])
    ).

:- func gc_to_str(soln_gc) = string.

gc_to_str(soln_gc_none) = "".
gc_to_str(soln_gc_target_native) = _ :-
    unexpected($pred, "soln_gc_target_native").
gc_to_str(soln_gc_bdw) = ".gc".
gc_to_str(soln_gc_bdw_debug) = ".gcd".
gc_to_str(soln_gc_accurate) = ".agc".
gc_to_str(soln_gc_history) = ".hgc".

:- func c_trail_to_str(c_trail) = string.

c_trail_to_str(c_trail_no) = "".
c_trail_to_str(c_trail_yes(soln_trail_segments_no)) = ".tr".
c_trail_to_str(c_trail_yes(soln_trail_segments_yes)) = ".trseg".

:- func thread_safe_to_str(soln_thread_safe) = string.

thread_safe_to_str(soln_thread_safe_no) = "".
thread_safe_to_str(soln_thread_safe_yes) = ".par".

:- func ssdebug_to_str(soln_ssdebug) = string.

ssdebug_to_str(soln_ssdebug_no) = "".
ssdebug_to_str(soln_ssdebug_yes) = ".ssdebug".

:- func mprof_to_string(soln_mprof_time, soln_mprof_memory) = string.

mprof_to_string(soln_mprof_time_no, soln_mprof_memory_no) = ".profcalls".
mprof_to_string(soln_mprof_time_no, soln_mprof_memory_yes) = ".memprof".
mprof_to_string(soln_mprof_time_yes, soln_mprof_memory_no) =".prof".
mprof_to_string(soln_mprof_time_yes, soln_mprof_memory_yes) = ".profall".

:- func pregen_to_str(soln_pregen) = string.

pregen_to_str(soln_pregen_no) = "".
pregen_to_str(soln_pregen_yes) = ".pregen".

:- func merc_file_to_str(which_grade_string, soln_merc_file) = string.

merc_file_to_str(grade_string_user, _) = "".
merc_file_to_str(grade_string_link_check, soln_merc_file_no) = "".
merc_file_to_str(grade_string_link_check, soln_merc_file_yes) = ".file".

:- func low_tag_bits_use_to_str(which_grade_string, soln_low_tag_bits_use)
    = string.

low_tag_bits_use_to_str(grade_string_user, _) = "".
low_tag_bits_use_to_str(grade_string_link_check, soln_low_tag_bits_use_0)
    = ".tags0".
low_tag_bits_use_to_str(grade_string_link_check, soln_low_tag_bits_use_2)
    = ".tags2".
low_tag_bits_use_to_str(grade_string_link_check, soln_low_tag_bits_use_3)
    = ".tags3".

:- func single_prec_float_to_str(soln_single_prec_float) = string.

single_prec_float_to_str(soln_single_prec_float_no) = "".
single_prec_float_to_str(soln_single_prec_float_yes) = ".spf".

%---------------------------------------------------------------------------%

:- type grade_components_map == map(solver_var_id, grade_components_entry).

:- type grade_components_entry
    --->    grade_components_entry(
                % The specified value of the solver var.
                solver_var_value_id,

                % The grade string component that specified that value.
                string
            ).

grade_string_to_succ_soln(GradeStr) = MaybSuccMap :-
    GradeComponentStrs = string.split_at_char('.', GradeStr),
    accumulate_grade_component_map_loop(GradeComponentStrs,
        map.init, ComponentsMap, [], RevErrorMsgs),
    list.reverse(RevErrorMsgs, ErrorMsgs),
    (
        ErrorMsgs = [],
        map.map_values_only(project_value_only, ComponentsMap, SuccMap),
        MaybSuccMap = ok(SuccMap)
    ;
        ErrorMsgs = [HeadErrorMsg | TailErrorMsgs],
        MaybSuccMap = error(HeadErrorMsg, TailErrorMsgs)
    ).

:- pred project_value_only(grade_components_entry::in,
    solver_var_value_id::out) is det.

project_value_only(grade_components_entry(ValueId, _), ValueId).

:- pred accumulate_grade_component_map_loop(list(string)::in,
    grade_components_map::in, grade_components_map::out,
    list(string)::in, list(string)::out) is det.

accumulate_grade_component_map_loop([], !ComponentMap, !RevErrorMsgs).
accumulate_grade_component_map_loop([ComponentStr | ComponentStrs],
        !ComponentMap, !RevErrorMsgs) :-
    ( if
        translate_grade_component(ComponentStr, HeadSetting, TailSettings)
    then
        apply_setting(ComponentStr, HeadSetting, 
            !ComponentMap, !RevErrorMsgs),
        list.foldl2(apply_setting(ComponentStr), TailSettings, 
            !ComponentMap, !RevErrorMsgs)
    else
        string.format("unknown grade component %s",
            [s(ComponentStr)], ErrorMsg),
        !:RevErrorMsgs = [ErrorMsg | !.RevErrorMsgs]
    ),
    accumulate_grade_component_map_loop(ComponentStrs,
        !ComponentMap, !RevErrorMsgs).

:- pred apply_setting(string::in, pair(solver_var_id, solver_var_value_id)::in,
    grade_components_map::in, grade_components_map::out,
    list(string)::in, list(string)::out) is det.

apply_setting(ComponentStr, VarId - ValueId, !ComponentMap, !RevErrorMsgs) :-
    ( if map.search(!.ComponentMap, VarId, OldEntry) then
        OldEntry = grade_components_entry(OldValueId, OldComponentStr),
        ( if OldValueId = ValueId then
            true
        else
            string.format("grade components %s and %s are incompatible",
                [s(OldComponentStr), s(ComponentStr)], ErrorMsg),
            !:RevErrorMsgs = [ErrorMsg | !.RevErrorMsgs]
        )
    else
        Entry = grade_components_entry(ValueId, ComponentStr),
        map.det_insert(VarId, Entry, !ComponentMap)
    ).

:- pred translate_grade_component(string::in,
    pair(solver_var_id, solver_var_value_id)::out,
    list(pair(solver_var_id, solver_var_value_id))::out) is semidet.

translate_grade_component(ComponentStr, Setting, Settings) :-
    (
        ComponentStr = "none",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_no,
            svar_gcc_regs_use - svalue_gcc_regs_use_no]
    ;
        ComponentStr = "reg",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_no,
            svar_gcc_regs_use - svalue_gcc_regs_use_yes]
    ;
        ComponentStr = "jump",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_yes,
            svar_gcc_regs_use - svalue_gcc_regs_use_no]
    ;
        ComponentStr = "fast",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_yes,
            svar_gcc_regs_use - svalue_gcc_regs_use_yes]
    ;
        ComponentStr = "asm_jump",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_gcc_labels_use - svalue_gcc_labels_use_yes,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_yes,
            svar_gcc_regs_use - svalue_gcc_regs_use_no]
    ;
        ComponentStr = "asm_fast",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_gcc_labels_use - svalue_gcc_labels_use_yes,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_yes,
            svar_gcc_regs_use - svalue_gcc_regs_use_yes]
    ;
        ComponentStr = "hl",
        Setting = svar_backend - svalue_backend_mlds,
        Settings =
            [svar_target - svalue_target_c,
            svar_data_level - svalue_data_level_hld,
            svar_nested_funcs - svalue_nested_funcs_no]
    ;
        ComponentStr = "hlc",
        Setting = svar_backend - svalue_backend_mlds,
        Settings =
            [svar_target - svalue_target_c,
            svar_data_level - svalue_data_level_lld,
            svar_nested_funcs - svalue_nested_funcs_no]
    ;
        ComponentStr = "hl_nest",
        Setting = svar_backend - svalue_backend_mlds,
        Settings =
            [svar_target - svalue_target_c,
            svar_data_level - svalue_data_level_hld,
            svar_nested_funcs - svalue_nested_funcs_yes]
    ;
        ComponentStr = "hlc_nest",
        Setting = svar_backend - svalue_backend_mlds,
        Settings =
            [svar_target - svalue_target_c,
            svar_data_level - svalue_data_level_lld,
            svar_nested_funcs - svalue_nested_funcs_yes]
    ;
        ComponentStr = "csharp",
        Setting = svar_target - svalue_target_csharp,
        Settings = []
    ;
        ComponentStr = "java",
        Setting = svar_target - svalue_target_java,
        Settings = []
    ;
        ComponentStr = "erlang",
        Setting = svar_target - svalue_target_erlang,
        Settings = []
    ;
        ComponentStr = "gc",
        Setting = svar_gc - svalue_gc_bdw,
        Settings = []
    ;
        ComponentStr = "gcd",
        Setting = svar_gc - svalue_gc_bdw_debug,
        Settings = []
    ;
        ComponentStr = "agc",
        Setting = svar_gc - svalue_gc_accurate,
        Settings = []
    ;
        ComponentStr = "hgc",
        Setting = svar_gc - svalue_gc_history,
        Settings = []
    ;
        ComponentStr = "par",
        Setting = svar_thread_safe - svalue_thread_safe_yes,
        Settings = []
    ;
        ComponentStr = "threadscope",
        Setting = svar_tscope_prof - svalue_tscope_prof_yes,
        Settings = []
    ;
        ComponentStr = "profdeep",
        Setting = svar_deep_prof - svalue_deep_prof_yes,
        Settings = []
    ;
        ComponentStr = "profcalls",
        Setting = svar_mprof_call - svalue_mprof_call_yes,
        Settings =
            [svar_mprof_time - svalue_mprof_time_no,
            svar_mprof_memory - svalue_mprof_memory_no]
    ;
        ComponentStr = "memprof",
        Setting = svar_mprof_call - svalue_mprof_call_yes,
        Settings =
            [svar_mprof_time - svalue_mprof_time_no,
            svar_mprof_memory - svalue_mprof_memory_yes]
    ;
        ComponentStr = "prof",
        Setting = svar_mprof_call - svalue_mprof_call_yes,
        Settings =
            [svar_mprof_time - svalue_mprof_time_yes,
            svar_mprof_memory - svalue_mprof_memory_no]
    ;
        ComponentStr = "profall",
        Setting = svar_mprof_call - svalue_mprof_call_yes,
        Settings =
            [svar_mprof_time - svalue_mprof_time_yes,
            svar_mprof_memory - svalue_mprof_memory_yes]
    ;
        ComponentStr = "tsc",
        Setting = svar_term_size_prof - svalue_term_size_prof_cells,
        Settings = []
    ;
        ComponentStr = "tsw",
        Setting = svar_term_size_prof - svalue_term_size_prof_words,
        Settings = []
    ;
        ComponentStr = "tr",
        Setting = svar_trail - svalue_trail_yes,
        Settings = [svar_trail_segments - svalue_trail_segments_no]
    ;
        ComponentStr = "trseg",
        Setting = svar_trail - svalue_trail_yes,
        Settings = [svar_trail_segments - svalue_trail_segments_yes]
    ;
        ( ComponentStr = "mm"
        ; ComponentStr = "mmsc"
        ),
        Setting = svar_minmodel - svalue_minmodel_stack_copy,
        Settings = []
    ;
        ( ComponentStr = "dmm"
        ; ComponentStr = "dmmsc"
        ),
        Setting = svar_minmodel - svalue_minmodel_stack_copy_debug,
        Settings = []
    ;
        ComponentStr = "mmos",
        Setting = svar_minmodel - svalue_minmodel_own_stack,
        Settings = []
    ;
        ComponentStr = "dmmos",
        Setting = svar_minmodel - svalue_minmodel_own_stack_debug,
        Settings = []
    ;
        ComponentStr = "spf",
        Setting = svar_single_prec_float - svalue_single_prec_float_yes,
        Settings = []
    ;
        ComponentStr = "debug",
        Setting = svar_debug - svalue_debug_debug,
        Settings = []
    ;
        ComponentStr = "decldebug",
        Setting = svar_debug - svalue_debug_decldebug,
        Settings = []
    ;
        ComponentStr = "rbmm",
        Setting = svar_rbmm - svalue_rbmm_yes,
        Settings =
            [svar_rbmm_debug - svalue_rbmm_debug_no,
            svar_rbmm_prof - svalue_rbmm_prof_no]
    ;
        ComponentStr = "rbmmp",
        Setting = svar_rbmm - svalue_rbmm_yes,
        Settings =
            [svar_rbmm_debug - svalue_rbmm_debug_no,
            svar_rbmm_prof - svalue_rbmm_prof_yes]
    ;
        ComponentStr = "rbmmd",
        Setting = svar_rbmm - svalue_rbmm_yes,
        Settings =
            [svar_rbmm_debug - svalue_rbmm_debug_yes,
            svar_rbmm_prof - svalue_rbmm_prof_no]
    ;
        ComponentStr = "rbmmdp",
        Setting = svar_rbmm - svalue_rbmm_yes,
        Settings =
            [svar_rbmm_debug - svalue_rbmm_debug_yes,
            svar_rbmm_prof - svalue_rbmm_prof_yes]
    ;
        ComponentStr = "ssdebug",
        Setting = svar_ssdebug - svalue_ssdebug_yes,
        Settings = []
    ).

%---------------------------------------------------------------------------%
:- end_module grade_string.
%---------------------------------------------------------------------------%
