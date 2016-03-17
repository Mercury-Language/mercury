%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_structure.
:- interface.

:- import_module grade_vars.

%---------------------------------------------------------------------------%

:- type grade_structure
    --->    grade_llds(
                llds_gcc_conf,
                grade_var_low_tag_bits_use,
                grade_var_stack_len,
                llds_gc,
                llds_trail_minmodel,
                llds_thread_safe,
                llds_perf_prof,
                grade_var_term_size_prof,
                grade_var_debug,
                grade_var_lldebug,
                llds_rbmm,
                grade_var_merc_file,
                grade_var_pregen,
                grade_var_merc_float
            )
    ;       grade_mlds(
                mlds_target,
                grade_var_ssdebug
            )
    ;       grade_elds(
                grade_var_ssdebug
            ).

    % We could record whether we use gcc registers and gcc gotos independently.
    % (We use gcc labels only if we use gcc gots, so our choices on those
    % two grade variables are not independent.) However, we choose to use
    % this flat representation, because we may not wish to support all
    % of the possible combinations. Some gcc bugs may show up only in
    % the presence of certain combinations, and since some combinations
    % (jump, fast and asm_jump in particular) are rarely used and thus
    % not well tested, we wouldn't necessarily find out about them.
    % This flat representation allows us to pick and choose which
    % combinations we support.
:- type llds_gcc_conf                 % labels, gotos,  regs
    --->    llds_gcc_conf_none        % no      no      no
    ;       llds_gcc_conf_reg         % no      no      yes
    ;       llds_gcc_conf_jump        % no      yes     no
    ;       llds_gcc_conf_fast        % no      yes     yes
    ;       llds_gcc_conf_asm_jump    % yes     yes     no
    ;       llds_gcc_conf_asm_fast.   % yes     yes     yes

:- type llds_trail_minmodel
    --->    ltm_none
    ;       ltm_trail(
                grade_var_trail_segments
            )
    ;       ltm_minmodel(
                llds_minmodel    
            ).

:- type llds_minmodel
    --->    lm_stack_copy
    ;       lm_stack_copy_debug
    ;       lm_own_stack
    ;       lm_own_stack_debug.

:- type llds_gc
    --->    llds_gc_none
    ;       llds_gc_bdw
    ;       llds_gc_bdw_debug
    ;       llds_gc_history.

:- type llds_thread_safe
    --->    llds_thread_safe_no
    ;       llds_thread_safe_yes(
                grade_var_tscope_prof
            ).

:- type llds_perf_prof
    --->    llds_perf_prof_none
    ;       llds_perf_prof_deep
    ;       llds_perf_prof_mprof(
                grade_var_mprof_time,
                grade_var_mprof_memory
            ).

:- type llds_rbmm
    --->    llds_rbmm_no
    ;       llds_rbmm_yes(
                grade_var_rbmm_debug,
                grade_var_rbmm_prof
            ).

:- type mlds_target
    --->    mlds_target_c(
                mlds_c_dararep,
                grade_var_nested_funcs,
                grade_var_low_tag_bits_use,
                grade_var_thread_safe,
                mlds_c_gc,
                mlds_c_trail,
                mlds_c_perf_prof,
                grade_var_merc_file,
                grade_var_pregen,
                grade_var_merc_float
            )
    ;       mlds_target_csharp
    ;       mlds_target_java.

:- type mlds_c_dararep
    --->    mlds_c_datarep_heap_cells
    ;       mlds_c_datarep_classes.

:- type mlds_c_gc
    --->    mlds_c_gc_none
    ;       mlds_c_gc_bdw
    ;       mlds_c_gc_bdw_debug
    ;       mlds_c_gc_accurate
    ;       mlds_c_gc_history.

:- type mlds_c_trail
    --->    mlds_c_trail_no
    ;       mlds_c_trail_yes(
                grade_var_trail_segments
            ).

:- type mlds_c_perf_prof
    --->    mlds_c_perf_prof_none
    ;       mlds_c_perf_prof_mprof(
                grade_var_mprof_time,
                grade_var_mprof_memory
            ).

:- func grade_vars_to_grade_structure(grade_vars) = grade_structure.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

grade_vars_to_grade_structure(GradeVars) = GradeStructure :-
    % XXX We want to verify that grade variable is used on every path,
    % for one of these three things: (a) to make a decision, (c) to check
    % that it has the expected value, or (c) to record its value in the
    % structured grade.
    %
    % We pick up the values of the arguments other than Backend in separate
    % deconstructions in each arm of the switch on Backend, to give us
    % a singleton variable warning if don't handle a grade var in an arm.
    %
    % Unfortunately, I (zs) don't see how to repeat the trick for the switch
    % on Target in the mlds case: having two deconstructs that each pick up
    % *some* arguments is vulnerable to not picking up some arguments
    % in *either*.
    GradeVars = grade_vars(Backend, _, _, _, _, _, _, _,
        _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _),

    % XXX The order of the arguments of grade_llds and grade_mls we generate
    % may differ from the order in runtime/mercury_grade.h, or the canonical
    % order when targeting non-C languages.
    % XXX We should consider imposing a standard order.
    (
        Backend = grade_var_backend_llds,

        GradeVars = grade_vars(_Backend, DataRep,
            Target, NestedFuncs, GccRegsUse, GccGotosUse, GccLabelsUse,
            LowTagBitsUse, StackLen, Trail, TrailSegments,
            MinimalModel, ThreadSafe, Gc,
            DeepProf, MprofCall, MprofTime, MprofMemory, TScopeProf,
            TermSizeProf, Debug, SSDebug, LLDebug, RBMM, RBMMDebug, RBMMProf,
            MercFile, Pregen, MercFloat),

        expect(unify(DataRep, grade_var_datarep_heap_cells), $pred,
            "DataRep != grade_var_datarep_heap_cells"),
        expect(unify(Target, grade_var_target_c), $pred,
            "Target != grade_var_target_c"),
        expect(unify(NestedFuncs, grade_var_nested_funcs_no), $pred,
            "NestedFuncs != grade_var_nested_funcs_no"),
        expect(unify(SSDebug, grade_var_ssdebug_no), $pred,
            "SSDebug != grade_var_ssdebug_no"),
        (
            GccLabelsUse = grade_var_gcc_labels_use_no,
            (
                GccGotosUse = grade_var_gcc_gotos_use_no,
                (
                    GccRegsUse = grade_var_gcc_regs_use_no,
                    LLDSGccConf = llds_gcc_conf_none
                ;
                    GccRegsUse = grade_var_gcc_regs_use_yes,
                    LLDSGccConf = llds_gcc_conf_reg
                )
            ;
                GccGotosUse = grade_var_gcc_gotos_use_yes,
                (
                    GccRegsUse = grade_var_gcc_regs_use_no,
                    LLDSGccConf = llds_gcc_conf_jump
                ;
                    GccRegsUse = grade_var_gcc_regs_use_yes,
                    LLDSGccConf = llds_gcc_conf_fast
                )
            )
        ;
            GccLabelsUse = grade_var_gcc_labels_use_yes,
            (
                GccGotosUse = grade_var_gcc_gotos_use_no,
                unexpected($pred, "GccUseLabels = yes, GccUseGotos = no")
            ;
                GccGotosUse = grade_var_gcc_gotos_use_yes,
                (
                    GccRegsUse = grade_var_gcc_regs_use_no,
                    LLDSGccConf = llds_gcc_conf_asm_jump
                ;
                    GccRegsUse = grade_var_gcc_regs_use_yes,
                    LLDSGccConf = llds_gcc_conf_asm_fast
                )
            )
        ),
        (
            Trail = grade_var_trail_no,
            (
                MinimalModel = grade_var_minmodel_no,
                LLDSTrailMinModel = ltm_none
            ;
                MinimalModel = grade_var_minmodel_stack_copy,
                LLDSTrailMinModel = ltm_minmodel(lm_stack_copy)
            ;
                MinimalModel = grade_var_minmodel_stack_copy_debug,
                LLDSTrailMinModel = ltm_minmodel(lm_stack_copy_debug)
            ;
                MinimalModel = grade_var_minmodel_own_stack,
                LLDSTrailMinModel = ltm_minmodel(lm_own_stack)
            ;
                MinimalModel = grade_var_minmodel_own_stack_debug,
                LLDSTrailMinModel = ltm_minmodel(lm_own_stack_debug)
            )
        ;
            Trail = grade_var_trail_yes,
            expect(unify(MinimalModel, grade_var_minmodel_no), $pred,
                "MinimalModel != grade_var_minmodel_no"),
            LLDSTrailMinModel = ltm_trail(TrailSegments)
        ),
        (
            Gc = grade_var_gc_none,
            LLDSGc = llds_gc_none
        ;
            Gc = grade_var_gc_target_native,
            unexpected($pred, "Target = c, Gc = target_native")
        ;
            Gc = grade_var_gc_bdw,
            LLDSGc = llds_gc_bdw
        ;
            Gc = grade_var_gc_bdw_debug,
            LLDSGc = llds_gc_bdw_debug
        ;
            Gc = grade_var_gc_accurate,
            unexpected($pred, "Backend = llds, Gc = accurate")
        ;
            Gc = grade_var_gc_history,
            LLDSGc = llds_gc_history
        ),
        (
            ThreadSafe = grade_var_thread_safe_no,
            expect(unify(TScopeProf, grade_var_tscope_prof_no), $pred,
                "TScopeProf != grade_var_tscope_prof_no"),
            LLDSThreadSafe = llds_thread_safe_no
        ;
            ThreadSafe = grade_var_thread_safe_yes,
            LLDSThreadSafe = llds_thread_safe_yes(TScopeProf)
        ),
        (
            DeepProf = grade_var_deep_prof_no,
            (
                MprofCall = grade_var_mprof_call_no,
                expect(unify(MprofTime, grade_var_mprof_time_no), $pred,
                    "MprofTime != grade_var_mprof_time_no"),
                expect(unify(MprofMemory, grade_var_mprof_memory_no), $pred,
                    "MprofMemory != grade_var_mprof_memory_no"),
                LLDSPerfProf = llds_perf_prof_none
            ;
                MprofCall = grade_var_mprof_call_yes,
                LLDSPerfProf = llds_perf_prof_mprof(MprofTime, MprofMemory)
            )
        ;
            DeepProf = grade_var_deep_prof_yes,
            expect(unify(MprofCall, grade_var_mprof_call_no), $pred,
                "MprofCall != grade_var_mprof_call_no"),
            expect(unify(MprofTime, grade_var_mprof_time_no), $pred,
                "MprofTime != grade_var_mprof_time_no"),
            expect(unify(MprofMemory, grade_var_mprof_memory_no), $pred,
                "MprofMemory != grade_var_mprof_memory_no"),
            LLDSPerfProf = llds_perf_prof_deep
        ),
        (
            RBMM = grade_var_rbmm_no,
            expect(unify(RBMMDebug, grade_var_rbmm_debug_no), $pred,
                "RBMMDebug != grade_var_rbmm_debug_no"),
            expect(unify(RBMMProf, grade_var_rbmm_prof_no), $pred,
                "RBMMProf != grade_var_rbmm_prof_no"),
            LLDSRBMM = llds_rbmm_no
        ;
            RBMM = grade_var_rbmm_yes,
            LLDSRBMM = llds_rbmm_yes(RBMMDebug, RBMMProf)
        ),
        GradeStructure = grade_llds(LLDSGccConf, LowTagBitsUse, StackLen,
            LLDSGc, LLDSTrailMinModel, LLDSThreadSafe,
            LLDSPerfProf, TermSizeProf, Debug, LLDebug, LLDSRBMM,
            MercFile, Pregen, MercFloat)
    ;
        Backend = grade_var_backend_mlds,

        GradeVars = grade_vars(_Backend, DataRep,
            Target, NestedFuncs, GccRegsUse, GccGotosUse, GccLabelsUse,
            LowTagBitsUse, StackLen, Trail, TrailSegments,
            MinimalModel, ThreadSafe, Gc,
            DeepProf, MprofCall, MprofTime, MprofMemory, TScopeProf,
            TermSizeProf, Debug, SSDebug, LLDebug, RBMM, RBMMDebug, RBMMProf,
            MercFile, Pregen, MercFloat),

        expect(unify(GccRegsUse, grade_var_gcc_regs_use_no), $pred,
            "GccRegsUse != grade_var_gcc_regs_use_no"),
        expect(unify(GccGotosUse, grade_var_gcc_gotos_use_no), $pred,
            "GccGotosUse != grade_var_gcc_gotos_use_no"),
        expect(unify(GccLabelsUse, grade_var_gcc_labels_use_no), $pred,
            "GccLabelsUse != grade_var_gcc_labels_use_no"),
        expect(unify(StackLen, grade_var_stack_len_std), $pred,
            "StackLen != grade_var_stack_len_std"),
        expect(unify(MinimalModel, grade_var_minmodel_no), $pred,
            "MinimalModel != grade_var_minmodel_no"),
        expect(unify(DeepProf, grade_var_deep_prof_no), $pred,
            "DeepProf != grade_var_deep_prof_no"),
        expect(unify(TScopeProf, grade_var_tscope_prof_no), $pred,
            "TScopeProf != grade_var_tscope_prof_no"),
        expect(unify(TermSizeProf, grade_var_term_size_prof_no), $pred,
            "TermSizeProf != grade_var_term_size_prof_no"),
        expect(unify(Debug, grade_var_debug_none), $pred,
            "Debug != grade_var_debug_none"),
        expect(unify(LLDebug, grade_var_lldebug_no), $pred,
            "LLDebug != grade_var_lldebug_no"),
        expect(unify(RBMM, grade_var_rbmm_no), $pred,
            "RBMM != grade_var_rbmm_no"),
        expect(unify(RBMMDebug, grade_var_rbmm_debug_no), $pred,
            "RBMMDebug != grade_var_rbmm_debug_no"),
        expect(unify(RBMMProf, grade_var_rbmm_prof_no), $pred,
            "RBMMProf != grade_var_rbmm_prof_no"),
        (
            Target = grade_var_target_c,
            (
                DataRep = grade_var_datarep_heap_cells,
                MLDSCDataRep = mlds_c_datarep_heap_cells
            ;
                DataRep = grade_var_datarep_classes,
                MLDSCDataRep = mlds_c_datarep_classes
            ;
                DataRep = grade_var_datarep_erlang,
                unexpected($pred, "Target = c, DataRep = erlang")
            ),
            (
                Gc = grade_var_gc_none,
                MLDSGc = mlds_c_gc_none
            ;
                Gc = grade_var_gc_target_native,
                unexpected($pred, "Target = c, Gc = target_native")
            ;
                Gc = grade_var_gc_bdw,
                MLDSGc = mlds_c_gc_bdw
            ;
                Gc = grade_var_gc_bdw_debug,
                MLDSGc = mlds_c_gc_bdw_debug
            ;
                Gc = grade_var_gc_accurate,
                MLDSGc = mlds_c_gc_accurate
            ;
                Gc = grade_var_gc_history,
                % XXX Is this supported?
                MLDSGc = mlds_c_gc_history
            ),
            (
                Trail = grade_var_trail_no,
                MLDSCTrail = mlds_c_trail_no
            ;
                Trail = grade_var_trail_yes,
                MLDSCTrail = mlds_c_trail_yes(TrailSegments)
            ),
            (
                MprofCall = grade_var_mprof_call_no,
                expect(unify(MprofTime, grade_var_mprof_time_no), $pred,
                    "MprofTime != grade_var_mprof_time_no"),
                expect(unify(MprofMemory, grade_var_mprof_memory_no), $pred,
                    "MprofMemory != grade_var_mprof_memory_no"),
                MLDSPerfProf = mlds_c_perf_prof_none
            ;
                MprofCall = grade_var_mprof_call_yes,
                MLDSPerfProf = mlds_c_perf_prof_mprof(MprofTime, MprofMemory)
            ),
            TargetC = mlds_target_c(MLDSCDataRep, NestedFuncs, LowTagBitsUse,
                ThreadSafe, MLDSGc, MLDSCTrail, MLDSPerfProf,
                MercFile, Pregen, MercFloat),
            GradeStructure = grade_mlds(TargetC, SSDebug)
        ;
            ( Target = grade_var_target_csharp
            ; Target = grade_var_target_java
            ),
            expect(unify(Pregen, grade_var_pregen_no), $pred,
                "Pregen != grade_var_pregen_no"),
            expect(unify(DataRep, grade_var_datarep_classes), $pred,
                "DataRep != grade_var_datarep_classes"),
            expect(unify(NestedFuncs, grade_var_nested_funcs_no), $pred,
                "NestedFuncs != grade_var_nested_funcs_no"),
            expect(unify(ThreadSafe, grade_var_thread_safe_yes), $pred,
                "ThreadSafe != grade_var_thread_safe_yes"),
            expect(unify(Gc, grade_var_gc_target_native), $pred,
                "Gc != grade_var_gc_target_native"),
            expect(unify(Trail, grade_var_trail_no), $pred,
                "Trail != grade_var_trail_no"),
            expect(unify(TrailSegments, grade_var_trail_segments_no), $pred,
                "TrailSegments != grade_var_trail_segments_no"),
            expect(unify(MprofCall, grade_var_mprof_call_no), $pred,
                "MprofCall != grade_var_mprof_call_no"),
            expect(unify(MprofTime, grade_var_mprof_time_no), $pred,
                "MprofTime != grade_var_mprof_time_no"),
            expect(unify(MprofMemory, grade_var_mprof_memory_no), $pred,
                "MprofMemory != grade_var_mprof_memory_no"),
            % The definition of MR_NEW_MERCURYFILE_STRUCT applies only
            % to grades that target C. When targeting other languages,
            % we don't insist on MercFile = grade_var_merc_file_no. 
            expect(unify(Pregen, grade_var_pregen_no), $pred,
                "Pregen != grade_var_pregen_no"),
            (
                ( MercFloat = grade_var_merc_float_is_boxed_c_double
                ; MercFloat = grade_var_merc_float_is_unboxed_c_double
                )
                % We don't care which one. XXX Should we, on either backend?
            ;
                MercFloat = grade_var_merc_float_is_unboxed_c_float,
                unexpected($pred,
                    "MercFloat = grade_var_merc_float_is_unboxed_c_float")
            ),
            % We repeat this here in case we later add some function symbols
            % to one or both of mlds_target_csharp/mlds_target_java.
            (
                Target = grade_var_target_csharp,
                MLDSTarget = mlds_target_csharp
            ;
                Target = grade_var_target_java,
                MLDSTarget = mlds_target_java
            ),
            GradeStructure = grade_mlds(MLDSTarget, SSDebug)
        ;
            Target = grade_var_target_erlang,
            unexpected($pred, "Backend = mlds but Target = erlang")
        )
    ;
        Backend = grade_var_backend_elds,

        GradeVars = grade_vars(_Backend, DataRep,
            Target, NestedFuncs, GccRegsUse, GccGotosUse, GccLabelsUse,
            _LowTagBitsUse, StackLen, Trail, TrailSegments,
            MinimalModel, ThreadSafe, Gc,
            DeepProf, MprofCall, MprofTime, MprofMemory, TScopeProf,
            TermSizeProf, Debug, SSDebug, LLDebug, RBMM, RBMMDebug, RBMMProf,
            _MercFile, Pregen, MercFloat),

        % XXX The ELDS backend's data representation is NOT the same
        % as the LLDS backends'. If it were, we couldn't ignore the value of
        % _LowTagBitsUse.
        expect(unify(DataRep, grade_var_datarep_erlang), $pred,
            "DataRep != grade_var_datarep_erlang"),
        expect(unify(Target, grade_var_target_erlang), $pred,
            "Target != grade_var_target_erlang"),
        expect(unify(NestedFuncs, grade_var_nested_funcs_no), $pred,
            "NestedFuncs != grade_var_nested_funcs_no"),
        expect(unify(GccRegsUse, grade_var_gcc_regs_use_no), $pred,
            "GccRegsUse != grade_var_gcc_regs_use_no"),
        expect(unify(GccGotosUse, grade_var_gcc_gotos_use_no), $pred,
            "GccGotosUse != grade_var_gcc_gotos_use_no"),
        expect(unify(GccLabelsUse, grade_var_gcc_labels_use_no), $pred,
            "GccLabelsUse != grade_var_gcc_labels_use_no"),
        expect(unify(StackLen, grade_var_stack_len_std), $pred,
            "StackLen != grade_var_stack_len_std"),
        expect(unify(Trail, grade_var_trail_no), $pred,
            "Trail != grade_var_trail_no"),
        expect(unify(TrailSegments, grade_var_trail_segments_no), $pred,
            "TrailSegments != grade_var_trail_segments_no"),
        expect(unify(MinimalModel, grade_var_minmodel_no), $pred,
            "MinimalModel != grade_var_minmodel_no"),
        expect(unify(ThreadSafe, grade_var_thread_safe_no), $pred,
            "ThreadSafe != grade_var_thread_safe_no"),
        expect(unify(Gc, grade_var_gc_target_native), $pred,
            "Gc != grade_var_gc_target_native"),
        expect(unify(DeepProf, grade_var_deep_prof_no), $pred,
            "DeepProf != grade_var_deep_prof_no"),
        expect(unify(MprofCall, grade_var_mprof_call_no), $pred,
            "MprofCall != grade_var_mprof_call_no"),
        expect(unify(MprofTime, grade_var_mprof_time_no), $pred,
            "MprofTime != grade_var_mprof_time_no"),
        expect(unify(MprofMemory, grade_var_mprof_memory_no), $pred,
            "MprofMemory != grade_var_mprof_memory_no"),
        expect(unify(TScopeProf, grade_var_tscope_prof_no), $pred,
            "TScopeProf != grade_var_tscope_prof_no"),
        expect(unify(TermSizeProf, grade_var_term_size_prof_no), $pred,
            "TermSizeProf != grade_var_term_size_prof_no"),
        expect(unify(Debug, grade_var_debug_none), $pred,
            "Debug != grade_var_debug_none"),
        expect(unify(LLDebug, grade_var_lldebug_no), $pred,
            "LLDebug != grade_var_lldebug_no"),
        expect(unify(RBMM, grade_var_rbmm_no), $pred,
            "RBMM != grade_var_rbmm_no"),
        expect(unify(RBMMDebug, grade_var_rbmm_debug_no), $pred,
            "RBMMDebug != grade_var_rbmm_debug_no"),
        expect(unify(RBMMProf, grade_var_rbmm_prof_no), $pred,
            "RBMMProf != grade_var_rbmm_prof_no"),
        % The definition of MR_NEW_MERCURYFILE_STRUCT applies only
        % to grades that target C. When targeting other languages,
        % we don't insist on MercFile = grade_var_merc_file_no. 
        expect(unify(Pregen, grade_var_pregen_no), $pred,
            "Pregen != grade_var_pregen_no"),
        (
            ( MercFloat = grade_var_merc_float_is_boxed_c_double
            ; MercFloat = grade_var_merc_float_is_unboxed_c_double
            )
            % We don't care which one. XXX Should we, on either backend?
        ;
            MercFloat = grade_var_merc_float_is_unboxed_c_float,
            unexpected($pred,
                "MercFloat = grade_var_merc_float_is_unboxed_c_float")
        ),
        % XXX probably incomplete
        GradeStructure = grade_elds(SSDebug)
    ).

%---------------------------------------------------------------------------%
:- end_module grade_structure.
%---------------------------------------------------------------------------%
