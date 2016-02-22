%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_components.
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

    % This function translates the representation of a grade from a form
    % which has one type (solver_var_id) for all solver variables and
    % just one type (solver_var_valud_id) for all solver variable values,
    % and translates it into a representation which has a separate type
    % (soln_<var>) for each solver variable, whose function symbols include
    % only the values appropriate for that variable.
    %
    % The solver needs the first, uniform representation, since without it,
    % one cannot write generic code that can handle all the requirements.
    % However, this representation cannot express the fact that each solver
    % variable has only a restricted set of valid values. The code that deals
    % with grade strings is much easier to write if one can assume that
    % the invariant expressing this fact holds. The return value of this
    % function guarantees that invariant. (If some bug in the solver, or
    % in the data it is given, causes a violation of the invariant, this
    % function will throw an exception.)
    %
:- func collect_solution_components(success_soln_map) = solution_components.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module grade_spec.

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
:- end_module grade_components.
%---------------------------------------------------------------------------%
