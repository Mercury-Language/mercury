%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_string.
:- interface.

:- import_module grade_solver.

%---------------------------------------------------------------------------%

:- type solution_components
    --->    solution_components(
                soln_backend,
                soln_data_level,
                soln_target,
                soln_nested_funcs,
                soln_gcc_regs,
                soln_gcc_gotos,
                soln_gcc_labels,
                soln_stack_len,
                soln_trail,
                soln_trail_segments,
                soln_minimal_model,
                soln_thread_safe,
                soln_gc,
                soln_deep_prof,
                soln_mprof_call,
                soln_mprof_time,
                soln_mprof_memory,
                soln_tscope_prof,
                soln_term_size_prof,
                soln_debug,
                soln_ssdebug,
                soln_lldebug,
                soln_rbmm,
                soln_rbmm_debug,
                soln_rbmm_prof,
                soln_single_prec_float
            ).

:- type soln_backend
    --->    soln_backend_mlds
    ;       soln_backend_llds
    ;       soln_backend_elds.

:- type soln_data_level
    --->    soln_data_level_hld
    ;       soln_data_level_lld.

:- type soln_target
    --->    soln_target_c
    ;       soln_target_csharp
    ;       soln_target_java
    ;       soln_target_erlang.

:- type soln_nested_funcs
    --->    soln_nested_funcs_no
    ;       soln_nested_funcs_yes.

:- type soln_gcc_regs
    --->    soln_gcc_regs_use_no
    ;       soln_gcc_regs_use_yes.

:- type soln_gcc_gotos
    --->    soln_gcc_gotos_use_no
    ;       soln_gcc_gotos_use_yes.

:- type soln_gcc_labels
    --->    soln_gcc_labels_use_no
    ;       soln_gcc_labels_use_yes.

:- type soln_stack_len
    --->    soln_stack_len_std
    ;       soln_stack_len_segments
    ;       soln_stack_len_extend.

:- type soln_trail
    --->    soln_trail_no
    ;       soln_trail_yes.

:- type soln_trail_segments
    --->    soln_trail_segments_no
    ;       soln_trail_segments_yes.

:- type soln_minimal_model
    --->    soln_minimal_model_no
    ;       soln_minimal_model_yes_stack_copy.

:- type soln_thread_safe
    --->    soln_thread_safe_no
    ;       soln_thread_safe_yes.

:- type soln_gc
    --->    soln_gc_none
    ;       soln_gc_bdw
    ;       soln_gc_bdw_debug
    ;       soln_gc_target_native
    ;       soln_gc_accurate
    ;       soln_gc_history.

:- type soln_deep_prof
    --->    soln_deep_prof_no
    ;       soln_deep_prof_yes.

:- type soln_mprof_call
    --->    soln_mprof_call_no
    ;       soln_mprof_call_yes.

:- type soln_mprof_time
    --->    soln_mprof_time_no
    ;       soln_mprof_time_yes.

:- type soln_mprof_memory
    --->    soln_mprof_memory_no
    ;       soln_mprof_memory_yes.

:- type soln_tscope_prof
    --->    soln_tscope_prof_no
    ;       soln_tscope_prof_yes.

:- type soln_term_size_prof
    --->    soln_term_size_prof_no
    ;       soln_term_size_prof_cells
    ;       soln_term_size_prof_words.

:- type soln_debug
    --->    soln_debug_none
    ;       soln_debug_debug
    ;       soln_debug_decldebug.

:- type soln_ssdebug
    --->    soln_ssdebug_no
    ;       soln_ssdebug_yes.

:- type soln_lldebug
    --->    soln_lldebug_no
    ;       soln_lldebug_yes.

:- type soln_rbmm
    --->    soln_rbmm_no
    ;       soln_rbmm_yes.

:- type soln_rbmm_debug
    --->    soln_rbmm_debug_no
    ;       soln_rbmm_debug_yes.

:- type soln_rbmm_prof
    --->    soln_rbmm_prof_no
    ;       soln_rbmm_prof_yes.

:- type soln_single_prec_float
    --->    soln_single_prec_float_no
    ;       soln_single_prec_float_yes.

%---------------------------------------------------------------------------%

:- func collect_solution_components(success_soln_map) = solution_components.

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

collect_solution_components(!.SolnMap) = SolutionComponents :-
    map.det_remove(svar_backend, Backend, !SolnMap),
    map.det_remove(svar_data_level, DataLevel, !SolnMap),
    map.det_remove(svar_target, Target, !SolnMap),
    map.det_remove(svar_nested_funcs, NestedFuncs, !SolnMap),
    map.det_remove(svar_gcc_regs_avail, _GccRegsAvail, !SolnMap),
    map.det_remove(svar_gcc_regs_use, GccRegsUse, !SolnMap),
    map.det_remove(svar_gcc_gotos_avail, _GccGotosAvail, !SolnMap),
    map.det_remove(svar_gcc_gotos_use, GccGotosUse, !SolnMap),
    map.det_remove(svar_gcc_labels_avail, _GccLabelsAvail, !SolnMap),
    map.det_remove(svar_gcc_labels_use, GccLabelsUse, !SolnMap),
    map.det_remove(svar_stack_len, StackLen, !SolnMap),
    map.det_remove(svar_trail, Trail, !SolnMap),
    map.det_remove(svar_trail_segments, TrailSegments, !SolnMap),
    map.det_remove(svar_minimal_model, MinimalModel, !SolnMap),
    map.det_remove(svar_thread_safe, ThreadSafe, !SolnMap),
    map.det_remove(svar_gc, Gc, !SolnMap),
    map.det_remove(svar_deep_prof, DeepProf, !SolnMap),
    map.det_remove(svar_mprof_call, MprofCall, !SolnMap),
    map.det_remove(svar_mprof_time, MprofTime, !SolnMap),
    map.det_remove(svar_mprof_memory, MprofMemory, !SolnMap),
    map.det_remove(svar_tscope_prof, TScopeProf, !SolnMap),
    map.det_remove(svar_term_size_prof, TermSizeProf, !SolnMap),
    map.det_remove(svar_debug, Debug, !SolnMap),
    map.det_remove(svar_ssdebug, SSDebug, !SolnMap),
    map.det_remove(svar_lldebug, LLDebug, !SolnMap),
    map.det_remove(svar_rbmm, RBMM, !SolnMap),
    map.det_remove(svar_rbmm_debug, RBMMDebug, !SolnMap),
    map.det_remove(svar_rbmm_prof, RBMMProf, !SolnMap),
    map.det_remove(svar_single_prec_float, SinglePrecFloat, !SolnMap),
    expect(map.is_empty(!.SolnMap), $pred, "unexpected entries in SolnMap"),

    ( if Backend = svalue_backend_mlds then
        ComponentBackend = soln_backend_mlds
    else if Backend = svalue_backend_llds then
        ComponentBackend = soln_backend_llds
    else if Backend = svalue_backend_elds then
        ComponentBackend = soln_backend_elds
    else
        unexpected($pred, "unexpected value of Backend")
    ),

    ( if DataLevel = svalue_data_level_hld then
        ComponentDataLevel = soln_data_level_hld
    else if DataLevel = svalue_data_level_lld then
        ComponentDataLevel = soln_data_level_lld
    else
        unexpected($pred, "unexpected value of DataLevel")
    ),

    ( if Target = svalue_target_c then
        ComponentTarget = soln_target_c
    else if Target = svalue_target_csharp then
        ComponentTarget = soln_target_csharp
    else if Target = svalue_target_java then
        ComponentTarget = soln_target_java
    else if Target = svalue_target_erlang then
        ComponentTarget = soln_target_erlang
    else
        unexpected($pred, "unexpected value of Target")
    ),

    ( if NestedFuncs = svalue_nested_funcs_no then
        ComponentNestedFuncs = soln_nested_funcs_no
    else if NestedFuncs = svalue_nested_funcs_yes then
        ComponentNestedFuncs = soln_nested_funcs_yes
    else
        unexpected($pred, "unexpected value of NestedFuncs")
    ),

    ( if GccRegsUse = svalue_gcc_regs_use_no then
        ComponentGccRegsUse = soln_gcc_regs_use_no
    else if GccRegsUse = svalue_gcc_regs_use_yes then
        ComponentGccRegsUse = soln_gcc_regs_use_yes
    else
        unexpected($pred, "unexpected value of GccRegsUse")
    ),

    ( if GccGotosUse = svalue_gcc_gotos_use_no then
        ComponentGccGotosUse = soln_gcc_gotos_use_no
    else if GccGotosUse = svalue_gcc_gotos_use_yes then
        ComponentGccGotosUse = soln_gcc_gotos_use_yes
    else
        unexpected($pred, "unexpected value of GccGotosUse")
    ),

    ( if GccLabelsUse = svalue_gcc_labels_use_no then
        ComponentGccLabelsUse = soln_gcc_labels_use_no
    else if GccLabelsUse = svalue_gcc_labels_use_yes then
        ComponentGccLabelsUse = soln_gcc_labels_use_yes
    else
        unexpected($pred, "unexpected value of GccLabelsUse")
    ),

    ( if StackLen = svalue_stack_len_std then
        ComponentStackLen = soln_stack_len_std
    else if StackLen = svalue_stack_len_segments then
        ComponentStackLen = soln_stack_len_segments
    else if StackLen = svalue_stack_len_extend then
        ComponentStackLen = soln_stack_len_extend
    else
        unexpected($pred, "unexpected value of StackLen")
    ),

    ( if Trail = svalue_trail_no then
        ComponentTrail = soln_trail_no
    else if Trail = svalue_trail_yes then
        ComponentTrail = soln_trail_yes
    else
        unexpected($pred, "unexpected value of Trail")
    ),

    ( if TrailSegments = svalue_trail_segments_no then
        ComponentTrailSegments = soln_trail_segments_no
    else if TrailSegments = svalue_trail_segments_yes then
        ComponentTrailSegments = soln_trail_segments_yes
    else
        unexpected($pred, "unexpected value of TrailSegments")
    ),

    ( if MinimalModel = svalue_minimal_model_no then
        ComponentMinimalModel = soln_minimal_model_no
    else if MinimalModel = svalue_minimal_model_yes_stack_copy then
        ComponentMinimalModel = soln_minimal_model_yes_stack_copy
    else
        unexpected($pred, "unexpected value of MinimalModel")
    ),

    ( if ThreadSafe = svalue_thread_safe_no then
        ComponentThreadSafe = soln_thread_safe_no
    else if ThreadSafe = svalue_thread_safe_yes then
        ComponentThreadSafe = soln_thread_safe_yes
    else
        unexpected($pred, "unexpected value of ThreadSafe")
    ),

    ( if Gc = svalue_gc_none then
        ComponentGc = soln_gc_none
    else if Gc = svalue_gc_bdw then
        ComponentGc = soln_gc_bdw
    else if Gc = svalue_gc_bdw_debug then
        ComponentGc = soln_gc_bdw_debug
    else if Gc = svalue_gc_target_native then
        ComponentGc = soln_gc_target_native
    else if Gc = svalue_gc_accurate then
        ComponentGc = soln_gc_accurate
    else if Gc = svalue_gc_history then
        ComponentGc = soln_gc_history
    else
        unexpected($pred, "unexpected value of Gc")
    ),

    ( if DeepProf = svalue_deep_prof_no then
        ComponentDeepProf = soln_deep_prof_no
    else if DeepProf = svalue_deep_prof_yes then
        ComponentDeepProf = soln_deep_prof_yes
    else
        unexpected($pred, "unexpected value of DeepProf")
    ),

    ( if MprofCall = svalue_mprof_call_no then
        ComponentMprofCall = soln_mprof_call_no
    else if MprofCall = svalue_mprof_call_yes then
        ComponentMprofCall = soln_mprof_call_yes
    else
        unexpected($pred, "unexpected value of MprofCall")
    ),

    ( if MprofTime = svalue_mprof_time_no then
        ComponentMprofTime = soln_mprof_time_no
    else if MprofTime = svalue_mprof_time_yes then
        ComponentMprofTime = soln_mprof_time_yes
    else
        unexpected($pred, "unexpected value of MprofTime")
    ),

    ( if MprofMemory = svalue_mprof_memory_no then
        ComponentMprofMemory = soln_mprof_memory_no
    else if MprofMemory = svalue_mprof_memory_yes then
        ComponentMprofMemory = soln_mprof_memory_yes
    else
        unexpected($pred, "unexpected value of MprofMemory")
    ),

    ( if TScopeProf = svalue_tscope_prof_no then
        ComponentTScopeProf = soln_tscope_prof_no
    else if TScopeProf = svalue_tscope_prof_yes then
        ComponentTScopeProf = soln_tscope_prof_yes
    else
        unexpected($pred, "unexpected value of TScopeProf")
    ),

    ( if TermSizeProf = svalue_term_size_prof_no then
        ComponentTermSizeProf = soln_term_size_prof_no
    else if TermSizeProf = svalue_term_size_prof_cells then
        ComponentTermSizeProf = soln_term_size_prof_cells
    else if TermSizeProf = svalue_term_size_prof_words then
        ComponentTermSizeProf = soln_term_size_prof_words
    else
        unexpected($pred, "unexpected value of TermSizeProf")
    ),

    ( if Debug = svalue_debug_none then
        ComponentDebug = soln_debug_none
    else if Debug = svalue_debug_debug then
        ComponentDebug = soln_debug_debug
    else if Debug = svalue_debug_decldebug then
        ComponentDebug = soln_debug_decldebug
    else
        unexpected($pred, "unexpected value of Debug")
    ),

    ( if SSDebug = svalue_ssdebug_no then
        ComponentSSDebug = soln_ssdebug_no
    else if SSDebug = svalue_ssdebug_yes then
        ComponentSSDebug = soln_ssdebug_yes
    else
        unexpected($pred, "unexpected value of SSDebug")
    ),

    ( if LLDebug = svalue_lldebug_no then
        ComponentLLDebug = soln_lldebug_no
    else if LLDebug = svalue_lldebug_yes then
        ComponentLLDebug = soln_lldebug_yes
    else
        unexpected($pred, "unexpected value of LLDebug")
    ),

    ( if RBMM = svalue_rbmm_no then
        ComponentRBMM = soln_rbmm_no
    else if RBMM = svalue_rbmm_yes then
        ComponentRBMM = soln_rbmm_yes
    else
        unexpected($pred, "unexpected value of RBMM")
    ),

    ( if RBMMDebug = svalue_rbmm_debug_no then
        ComponentRBMMDebug = soln_rbmm_debug_no
    else if RBMMDebug = svalue_rbmm_debug_yes then
        ComponentRBMMDebug = soln_rbmm_debug_yes
    else
        unexpected($pred, "unexpected value of RBMMDebug")
    ),

    ( if RBMMProf = svalue_rbmm_prof_no then
        ComponentRBMMProf = soln_rbmm_prof_no
    else if RBMMProf = svalue_rbmm_prof_yes then
        ComponentRBMMProf = soln_rbmm_prof_yes
    else
        unexpected($pred, "unexpected value of RBMMProf")
    ),

    ( if SinglePrecFloat = svalue_single_prec_float_no then
        ComponentSinglePrecFloat = soln_single_prec_float_no
    else if SinglePrecFloat = svalue_single_prec_float_yes then
        ComponentSinglePrecFloat = soln_single_prec_float_yes
    else
        unexpected($pred, "unexpected value of SinglePrecFloat")
    ),

    SolutionComponents = solution_components(
        ComponentBackend, ComponentDataLevel,
        ComponentTarget, ComponentNestedFuncs,
        ComponentGccRegsUse, ComponentGccGotosUse, ComponentGccLabelsUse,
        ComponentStackLen, ComponentTrail, ComponentTrailSegments,
        ComponentMinimalModel, ComponentThreadSafe, ComponentGc,
        ComponentDeepProf,
        ComponentMprofCall, ComponentMprofTime, ComponentMprofMemory,
        ComponentTScopeProf, ComponentTermSizeProf,
        ComponentDebug, ComponentSSDebug, ComponentLLDebug,
        ComponentRBMM, ComponentRBMMDebug, ComponentRBMMProf,
        ComponentSinglePrecFloat
    ).

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
