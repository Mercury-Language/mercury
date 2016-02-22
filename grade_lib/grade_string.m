%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_string.
:- interface.

:- import_module grade_components.
:- import_module grade_solver.

%---------------------------------------------------------------------------%

:- type grade
    --->    grade_llds(
                llds_gcc_conf,
                soln_stack_len,
                llds_trail,
                llds_gc,
                llds_thread_safe,
                llds_perf_prof,
                soln_term_size_prof,
                soln_minimal_model,
                soln_debug,
                soln_lldebug,
                llds_rbmm,
                soln_single_prec_float
            )
    ;       grade_mlds(
                mlds_target,
                soln_ssdebug
            )
    ;       grade_elds(
                soln_thread_safe,
                soln_ssdebug
            ).

:- type llds_gcc_conf                 % labels, gotos,  regs
    --->    llds_gcc_conf_none        % no      no      no
    ;       llds_gcc_conf_reg         % no      no      yes
    ;       llds_gcc_conf_jump        % no      yes     no
    ;       llds_gcc_conf_fast        % no      yes     yes
    ;       llds_gcc_conf_asm_jump    % yes     yes     no
    ;       llds_gcc_conf_asm_fast.   % yes     yes     yes

:- type llds_trail
    --->    llds_trail_no
    ;       llds_trail_yes(
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
    % XXX Does thread safety mean the same thing for all three targets?
    % If yes, then thread safety should be pulled from all three function
    % symbols here and put into grade_mlds(...) instead.
    --->    mlds_target_c(
                soln_data_level,
                soln_nested_funcs,
                mlds_c_gc,
                soln_trail,                 % XXX trail segments?
                soln_thread_safe,
                soln_single_prec_float
            )
    ;       mlds_target_csharp(
                soln_thread_safe
            )
    ;       mlds_target_java(
                soln_thread_safe
            ).

:- type mlds_c_gc
    --->    mlds_c_gc_none
    ;       mlds_c_gc_bdw
    ;       mlds_c_gc_bdw_debug
    ;       mlds_c_gc_accurate
    ;       mlds_c_gc_history.

:- func success_soln_to_grade(success_soln_map) = grade.

%---------------------------------------------------------------------------%

:- func success_soln_to_grade_string(success_soln_map) = string.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module grade_spec.

:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

success_soln_to_grade(SuccMap) = Grade :-
    SolutionComponents = collect_solution_components(SuccMap),

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
    SolutionComponents = solution_components(Backend, _, _, _, _, _, _, _,
        _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _),

    (
        Backend = soln_backend_llds,

        SolutionComponents = solution_components(_Backend, DataLevel,
            Target, NestedFuncs, GccRegsUse, GccGotosUse, GccLabelsUse,
            StackLen, Trail, TrailSegments, MinimalModel, ThreadSafe, Gc,
            DeepProf, MprofCall, MprofTime, MprofMemory, TScopeProf,
            TermSizeProf, Debug, SSDebug, LLDebug, RBMM, RBMMDebug, RBMMProf,
            SinglePrecFloat),

        expect(unify(Target, soln_target_c), $pred,
            "Target != soln_target_c"),
        expect(unify(DataLevel, soln_data_level_lld), $pred,
            "DataLevel != soln_data_level_lld"),
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
            LLDSTrail = llds_trail_no
        ;
            Trail = soln_trail_yes,
            LLDSTrail = llds_trail_yes(TrailSegments)
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
        Grade = grade_llds(LLDSGccConf, StackLen, LLDSTrail,
            LLDSGc, LLDSThreadSafe, LLDSPerfProf, TermSizeProf,
            MinimalModel, Debug, LLDebug, LLDSRBMM, SinglePrecFloat)
    ;
        Backend = soln_backend_mlds,

        SolutionComponents = solution_components(_Backend, DataLevel,
            Target, NestedFuncs, GccRegsUse, GccGotosUse, GccLabelsUse,
            StackLen, Trail, TrailSegments, MinimalModel, ThreadSafe, Gc,
            DeepProf, MprofCall, MprofTime, MprofMemory, TScopeProf,
            TermSizeProf, Debug, SSDebug, LLDebug, RBMM, RBMMDebug, RBMMProf,
            SinglePrecFloat),

        expect(unify(GccRegsUse, soln_gcc_regs_use_no), $pred,
            "GccRegsUse != soln_gcc_regs_use_no"),
        expect(unify(GccGotosUse, soln_gcc_gotos_use_no), $pred,
            "GccGotosUse != soln_gcc_gotos_use_no"),
        expect(unify(GccLabelsUse, soln_gcc_labels_use_no), $pred,
            "GccLabelsUse != soln_gcc_labels_use_no"),
        expect(unify(StackLen, soln_stack_len_std), $pred,
            "StackLen != soln_stack_len_std"),
        expect(unify(TrailSegments, soln_trail_segments_no), $pred,
            "TrailSegments != soln_trail_segments_no"),
        expect(unify(MinimalModel, soln_minimal_model_no), $pred,
            "MinimalModel != soln_minimal_model_no"),
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
        (
            Target = soln_target_c,
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
            Grade = grade_mlds(mlds_target_c(DataLevel, NestedFuncs,
                MLDSGc, Trail, ThreadSafe, SinglePrecFloat), SSDebug)
        ;
            ( Target = soln_target_csharp
            ; Target = soln_target_java
            ),
            expect(unify(DataLevel, soln_data_level_hld), $pred,
                "DataLevel != soln_data_level_hld"),
            expect(unify(NestedFuncs, soln_nested_funcs_no), $pred,
                "NestedFuncs != soln_nested_funcs_no"),
            expect(unify(Gc, soln_gc_target_native), $pred,
                "Gc != soln_gc_target_native"),
            expect(unify(Trail, soln_trail_no), $pred,
                "Trail != soln_trail_no"),
            expect(unify(SinglePrecFloat, soln_single_prec_float_no), $pred,
                "SinglePrecFloat != soln_single_prec_float_no"),
            (
                Target = soln_target_csharp,
                MLDSTarget = mlds_target_csharp(ThreadSafe)
            ;
                Target = soln_target_java,
                MLDSTarget = mlds_target_java(ThreadSafe)
            ),
            Grade = grade_mlds(MLDSTarget, SSDebug)
        ;
            Target = soln_target_erlang,
            unexpected($pred, "Backend = mlds but Target = erlang")
        )
    ;
        Backend = soln_backend_elds,

        SolutionComponents = solution_components(_Backend, DataLevel,
            Target, NestedFuncs, GccRegsUse, GccGotosUse, GccLabelsUse,
            StackLen, Trail, TrailSegments, MinimalModel, ThreadSafe, Gc,
            DeepProf, MprofCall, MprofTime, MprofMemory, TScopeProf,
            TermSizeProf, Debug, SSDebug, LLDebug, RBMM, RBMMDebug, RBMMProf,
            SinglePrecFloat),

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
        expect(unify(MinimalModel, soln_minimal_model_no), $pred,
            "MinimalModel != soln_minimal_model_no"),
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
        expect(unify(SinglePrecFloat, soln_single_prec_float_no), $pred,
            "SinglePrecFloat != soln_single_prec_float_no"),
        % XXX incomplete
        Grade = grade_elds(ThreadSafe, SSDebug)
    ).

%---------------------------------------------------------------------------%

success_soln_to_grade_string(SuccMap) = GradeStr :-
    Grade = success_soln_to_grade(SuccMap),

    % XXX mercuryfile struct
    % XXX tag bits: high/low/none, 2/3
    % XXX unboxed float
    % XXX regparm
    % XXX picreg
    % XXX pregen

    (
        Grade = grade_llds(GccConf, StackLen, LLDSTrail,
            LLDSGc, LLDSThreadSafe, LLDSPerfProf, TermSizeProf, MinimalModel,
            Debug, LLDebug, LLDSRBMM, SinglePrecFloat),

        ( GccConf = llds_gcc_conf_none,             GccConfStr = "none"
        ; GccConf = llds_gcc_conf_reg,              GccConfStr = "reg"
        ; GccConf = llds_gcc_conf_jump,             GccConfStr = "jump"
        ; GccConf = llds_gcc_conf_fast,             GccConfStr = "fast"
        ; GccConf = llds_gcc_conf_asm_jump,         GccConfStr = "asm_jump"
        ; GccConf = llds_gcc_conf_asm_fast,         GccConfStr = "asm_fast"
        ),
        (
            LLDSThreadSafe = llds_thread_safe_no,
            ThreadSafeStr = "",
            TScopeProfStr = ""
        ;
            LLDSThreadSafe = llds_thread_safe_yes(TScopeProf),
            ThreadSafeStr = ".par",
            (
                TScopeProf = soln_tscope_prof_no,
                TScopeProfStr = ""
            ;
                TScopeProf = soln_tscope_prof_yes,
                TScopeProfStr = ".threadscope"
            )
        ),
        ( LLDSGc = llds_gc_none,                    GcStr = ""
        ; LLDSGc = llds_gc_bdw,                     GcStr = ".gc"
        ; LLDSGc = llds_gc_bdw_debug,               GcStr = ".gcd"
        ; LLDSGc = llds_gc_history,                 GcStr = ".hgc"
        ),
        (
            LLDSPerfProf = llds_perf_prof_none,
            LLDSPerfProfStr = ""
        ;
            LLDSPerfProf = llds_perf_prof_deep,
            LLDSPerfProfStr = ".profdeep"
        ;
            LLDSPerfProf = llds_perf_prof_mprof(MprofTime, MprofMemory),
            (
                MprofTime = soln_mprof_time_no,
                MprofMemory = soln_mprof_memory_no,
                LLDSPerfProfStr = ".profcalls"
            ;
                MprofTime = soln_mprof_time_no,
                MprofMemory = soln_mprof_memory_yes,
                LLDSPerfProfStr = ".memprof"
            ;
                MprofTime = soln_mprof_time_yes,
                MprofMemory = soln_mprof_memory_no,
                LLDSPerfProfStr = ".prof"
            ;
                MprofTime = soln_mprof_time_yes,
                MprofMemory = soln_mprof_memory_yes,
                LLDSPerfProfStr = ".profall"
            )
        ),
        ( TermSizeProf = soln_term_size_prof_no,    TermSizeProfStr = ""
        ; TermSizeProf = soln_term_size_prof_cells, TermSizeProfStr = ".tsc"
        ; TermSizeProf = soln_term_size_prof_words, TermSizeProfStr = ".tsw"
        ),
        (
            LLDSTrail = llds_trail_no ,
            TrailStr = ""
        ;
            LLDSTrail = llds_trail_yes(TrailSegments),
            ( TrailSegments = soln_trail_segments_no,   TrailStr = ".tr"
            ; TrailSegments = soln_trail_segments_yes,  TrailStr = ".trseg"
            )
        ),
        (
            MinimalModel = soln_minimal_model_no,
            MinimalModelStr = ""
        ;
            MinimalModel = soln_minimal_model_yes_stack_copy,
            MinimalModelStr = ".mmsc"
        ),
        (
            SinglePrecFloat = soln_single_prec_float_no,
            SinglePrecFloatStr = ""
        ;
            SinglePrecFloat = soln_single_prec_float_yes,
            SinglePrecFloatStr = ".spf"
        ),
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
        GradeStr = string.append_list([GccConfStr, ThreadSafeStr, GcStr,
            LLDSPerfProfStr, TermSizeProfStr, TrailStr, MinimalModelStr,
            SinglePrecFloatStr, DebugStr, LLDebugStr,
            StackLenStr, RBMMStr, TScopeProfStr])
    ;
        Grade = grade_mlds(MLDSTarget, SSDebug),
        ( SSDebug = soln_ssdebug_no,            SSDebugStr = ""
        ; SSDebug = soln_ssdebug_yes,           SSDebugStr = ".ssdebug"
        ),
        (
            MLDSTarget = mlds_target_c(DataLevel, NestedFuncs, MLDSCGc,
                Trail, ThreadSafe, SinglePrecFloat),
            ( DataLevel = soln_data_level_hld,      DataLevelStr = "hl"
            ; DataLevel = soln_data_level_lld,      DataLevelStr = "hlc"
            ),
            ( NestedFuncs = soln_nested_funcs_no,   NestedFuncsStr = ""
            ; NestedFuncs = soln_nested_funcs_yes,  NestedFuncsStr = "_nest"
            ),
            ( ThreadSafe = soln_thread_safe_no,     ThreadSafeStr = ""
            ; ThreadSafe = soln_thread_safe_yes,    ThreadSafeStr = ".par"
            ),
            ( MLDSCGc = mlds_c_gc_none,             GcStr = ""
            ; MLDSCGc = mlds_c_gc_bdw,              GcStr = ".gc"
            ; MLDSCGc = mlds_c_gc_bdw_debug,        GcStr = ".gcd"
            ; MLDSCGc = mlds_c_gc_accurate,         GcStr = ".agc"
            ; MLDSCGc = mlds_c_gc_history,          GcStr = ".hgc"
            ),
            ( Trail = soln_trail_no,                TrailStr = ""
            ; Trail = soln_trail_yes,               TrailStr = ".tr"
            ),
            (
                SinglePrecFloat = soln_single_prec_float_no,
                SinglePrecFloatStr = ""
            ;
                SinglePrecFloat = soln_single_prec_float_yes,
                SinglePrecFloatStr = ".spf"
            ),
            GradeStr = string.append_list([DataLevelStr, NestedFuncsStr,
                ThreadSafeStr, SSDebugStr, GcStr, TrailStr,
                SinglePrecFloatStr])
        ;
            MLDSTarget = mlds_target_csharp(ThreadSafe),
            ( ThreadSafe = soln_thread_safe_no,     ThreadSafeStr = ""
            ; ThreadSafe = soln_thread_safe_yes,    ThreadSafeStr = ".par"
            ),
            GradeStr = string.append_list(["csharp", ThreadSafeStr, SSDebugStr])
        ;
            MLDSTarget = mlds_target_java(ThreadSafe),
            ( ThreadSafe = soln_thread_safe_no,     ThreadSafeStr = ""
            ; ThreadSafe = soln_thread_safe_yes,    ThreadSafeStr = ".par"
            ),
            GradeStr = string.append_list(["java", ThreadSafeStr, SSDebugStr])
        )
    ;
        Grade = grade_elds(ThreadSafe, SSDebug),
        ( ThreadSafe = soln_thread_safe_no,     ThreadSafeStr = ""
        ; ThreadSafe = soln_thread_safe_yes,    ThreadSafeStr = ".par"
        ),
        ( SSDebug = soln_ssdebug_no,            SSDebugStr = ""
        ; SSDebug = soln_ssdebug_yes,           SSDebugStr = ".ssdebug"
        ),
        GradeStr = string.append_list(["erlang", ThreadSafeStr, SSDebugStr])
    ).

%---------------------------------------------------------------------------%
:- end_module grade_string.
%---------------------------------------------------------------------------%
