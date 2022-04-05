%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This module defines grade_vars, a representation for grades that allows
% the compiler to answer simple questions such as "should the generated high
% level C code use nested functions?" in a type safe manner. It provides
% a mechanism to convert solutions from the solver, which uses a different
% representation, to this representation.
%
% See compiler/notes/grade_library.html for documentation of the
% relationship between these representations, and their uses.
%

:- module grade_lib.grade_vars.
:- interface.

:- import_module grade_lib.grade_solver.

%---------------------------------------------------------------------------%

    % This function translates the representation of a grade from a form
    % which has just one type (solver_var_id) for all solver variables and
    % just one type (solver_var_value_id) for all solver variable values,
    % and translates it into a representation which has a separate type
    % (grade_var_<varname>) for each solver variable, whose function symbols
    % include only the values appropriate for that variable.
    %
    % The solver needs the first, uniform representation, since without it,
    % one cannot write generic code that can handle all the requirements.
    % However, this representation cannot express the fact that each solver
    % variable has only a restricted set of valid values. The code that deals
    % with grade strings is much easier to write if one can assume that
    % the invariant expressing this fact holds. The return value of this
    % function guarantees that invariant.
    %
    % If some bug in the solver, or in the data it is given, causes
    % a violation of the invariant, this function will throw an exception.
    %
:- func success_map_to_grade_vars(success_soln_map) = grade_vars.

%---------------------------------------------------------------------------%

:- type grade_vars
    --->    grade_vars(
                gv_pregen               :: grade_var_pregen,
                gv_backend              :: grade_var_backend,
                gv_target               :: grade_var_target,
                gv_gcc_conf             :: grade_var_gcc_conf,
                gv_low_tag_bits_use     :: grade_var_low_tag_bits_use,
                gv_stack_len            :: grade_var_stack_len,
                gv_trail                :: grade_var_trail,
                gv_minmodel             :: grade_var_minmodel,
                gv_thread_safe          :: grade_var_thread_safe,
                gv_gc                   :: grade_var_gc,
                gv_deep_prof            :: grade_var_deep_prof,
                gv_mprof_call           :: grade_var_mprof_call,
                gv_mprof_time           :: grade_var_mprof_time,
                gv_mprof_memory         :: grade_var_mprof_memory,
                gv_tscope_prof          :: grade_var_tscope_prof,
                gv_term_size_prof       :: grade_var_term_size_prof,
                gv_debug                :: grade_var_debug,
                gv_ssdebug              :: grade_var_ssdebug,
                gv_target_debug         :: grade_var_target_debug,
                gv_rbmm                 :: grade_var_rbmm,
                gv_rbmm_debug           :: grade_var_rbmm_debug,
                gv_rbmm_prof            :: grade_var_rbmm_prof,
                gv_merc_file            :: grade_var_merc_file,
                gv_merc_float           :: grade_var_merc_float
            ).

:- type grade_var_pregen
    --->    grade_var_pregen_no
    ;       grade_var_pregen_yes.

:- type grade_var_backend
    --->    grade_var_backend_mlds
    ;       grade_var_backend_llds.

:- type grade_var_target
    --->    grade_var_target_c
    ;       grade_var_target_csharp
    ;       grade_var_target_java.

:- type grade_var_nested_funcs
    --->    grade_var_nested_funcs_no
    ;       grade_var_nested_funcs_yes.

:- type grade_var_gcc_conf
    --->    grade_var_gcc_conf_none
    ;       grade_var_gcc_conf_reg
    ;       grade_var_gcc_conf_jump
    ;       grade_var_gcc_conf_fast
    ;       grade_var_gcc_conf_asm_jump
    ;       grade_var_gcc_conf_asm_fast.

:- type grade_var_low_tag_bits_use
    --->    grade_var_low_tag_bits_use_0
    ;       grade_var_low_tag_bits_use_2
    ;       grade_var_low_tag_bits_use_3.

:- type grade_var_stack_len
    --->    grade_var_stack_len_std
    ;       grade_var_stack_len_segments
    ;       grade_var_stack_len_extend.

:- type grade_var_trail
    --->    grade_var_trail_no
    ;       grade_var_trail_yes.

:- type grade_var_minmodel
    --->    grade_var_minmodel_no
    ;       grade_var_minmodel_stack_copy
    ;       grade_var_minmodel_stack_copy_debug
    ;       grade_var_minmodel_own_stack
    ;       grade_var_minmodel_own_stack_debug.

:- type grade_var_thread_safe
    --->    grade_var_thread_safe_c_no
    ;       grade_var_thread_safe_c_yes
    ;       grade_var_thread_safe_target_native.

:- type grade_var_gc
    --->    grade_var_gc_none
    ;       grade_var_gc_bdw
    ;       grade_var_gc_bdw_debug
    ;       grade_var_gc_target_native
    ;       grade_var_gc_accurate
    ;       grade_var_gc_history.

:- type grade_var_deep_prof
    --->    grade_var_deep_prof_no
    ;       grade_var_deep_prof_yes.

:- type grade_var_mprof_call
    --->    grade_var_mprof_call_no
    ;       grade_var_mprof_call_yes.

:- type grade_var_mprof_time
    --->    grade_var_mprof_time_no
    ;       grade_var_mprof_time_yes.

:- type grade_var_mprof_memory
    --->    grade_var_mprof_memory_no
    ;       grade_var_mprof_memory_yes.

:- type grade_var_tscope_prof
    --->    grade_var_tscope_prof_no
    ;       grade_var_tscope_prof_yes.

:- type grade_var_term_size_prof
    --->    grade_var_term_size_prof_no
    ;       grade_var_term_size_prof_cells
    ;       grade_var_term_size_prof_words.

:- type grade_var_debug
    --->    grade_var_debug_none
    ;       grade_var_debug_debug
    ;       grade_var_debug_decldebug.

:- type grade_var_ssdebug
    --->    grade_var_ssdebug_no
    ;       grade_var_ssdebug_yes.

:- type grade_var_target_debug
    --->    grade_var_target_debug_no
    ;       grade_var_target_debug_yes.

:- type grade_var_rbmm
    --->    grade_var_rbmm_no
    ;       grade_var_rbmm_yes.

:- type grade_var_rbmm_debug
    --->    grade_var_rbmm_debug_no
    ;       grade_var_rbmm_debug_yes.

:- type grade_var_rbmm_prof
    --->    grade_var_rbmm_prof_no
    ;       grade_var_rbmm_prof_yes.

:- type grade_var_merc_file
    --->    grade_var_merc_file_no
    ;       grade_var_merc_file_yes.

:- type grade_var_merc_float
    --->    grade_var_merc_float_is_boxed_c_double
    ;       grade_var_merc_float_is_unboxed_c_double
    ;       grade_var_merc_float_is_unboxed_c_float.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module grade_lib.grade_spec.

:- import_module map.
:- import_module require.

%---------------------------------------------------------------------------%

success_map_to_grade_vars(!.SuccMap) = GradeVars :-
    map.det_remove(svar_ac_gcc_regs_avail, _GccRegsAvail, !SuccMap),
    map.det_remove(svar_ac_gcc_gotos_avail, _GccGotosAvail, !SuccMap),
    map.det_remove(svar_ac_gcc_labels_avail, _GccLabelsAvail, !SuccMap),
    map.det_remove(svar_ac_low_tag_bits_avail, _LowTagBitsAvail, !SuccMap),
    map.det_remove(svar_ac_size_of_double, _SizeOfDouble, !SuccMap),
    map.det_remove(svar_ac_merc_file, MercFile, !SuccMap),

    map.det_remove(svar_pregen, Pregen, !SuccMap),
    map.det_remove(svar_backend, Backend, !SuccMap),
    map.det_remove(svar_target, Target, !SuccMap),
    map.det_remove(svar_gcc_conf, GccConf, !SuccMap),
    map.det_remove(svar_low_tag_bits_use, LowTagBitsUse, !SuccMap),
    map.det_remove(svar_stack_len, StackLen, !SuccMap),
    map.det_remove(svar_trail, Trail, !SuccMap),
    map.det_remove(svar_minmodel, MinimalModel, !SuccMap),
    map.det_remove(svar_thread_safe, ThreadSafe, !SuccMap),
    map.det_remove(svar_gc, Gc, !SuccMap),
    map.det_remove(svar_deep_prof, DeepProf, !SuccMap),
    map.det_remove(svar_mprof_call, MprofCall, !SuccMap),
    map.det_remove(svar_mprof_time, MprofTime, !SuccMap),
    map.det_remove(svar_mprof_memory, MprofMemory, !SuccMap),
    map.det_remove(svar_tscope_prof, TScopeProf, !SuccMap),
    map.det_remove(svar_term_size_prof, TermSizeProf, !SuccMap),
    map.det_remove(svar_debug, Debug, !SuccMap),
    map.det_remove(svar_ssdebug, SSDebug, !SuccMap),
    map.det_remove(svar_target_debug, TargetDebug, !SuccMap),
    map.det_remove(svar_rbmm, RBMM, !SuccMap),
    map.det_remove(svar_rbmm_debug, RBMMDebug, !SuccMap),
    map.det_remove(svar_rbmm_prof, RBMMProf, !SuccMap),
    map.det_remove(svar_request_single_prec_float, _ReqSinglePrecFloat,
        !SuccMap),
    map.det_remove(svar_merc_float, MercFloat, !SuccMap),
    expect(map.is_empty(!.SuccMap), $pred, "unexpected entries in SuccMap"),

    ( if Pregen = svalue_pregen_no then
        GradeVarPregen = grade_var_pregen_no
    else if Pregen = svalue_pregen_yes then
        GradeVarPregen = grade_var_pregen_yes
    else
        unexpected($pred, "unexpected value of Pregen")
    ),

    ( if Backend = svalue_backend_mlds then
        GradeVarBackend = grade_var_backend_mlds
    else if Backend = svalue_backend_llds then
        GradeVarBackend = grade_var_backend_llds
    else
        unexpected($pred, "unexpected value of Backend")
    ),

    ( if Target = svalue_target_c then
        GradeVarTarget = grade_var_target_c
    else if Target = svalue_target_csharp then
        GradeVarTarget = grade_var_target_csharp
    else if Target = svalue_target_java then
        GradeVarTarget = grade_var_target_java
    else
        unexpected($pred, "unexpected value of Target")
    ),

    ( if GccConf = svalue_gcc_conf_none then
        GradeVarGccConf = grade_var_gcc_conf_none
    else if GccConf = svalue_gcc_conf_reg then
        GradeVarGccConf = grade_var_gcc_conf_reg
    else if GccConf = svalue_gcc_conf_jump then
        GradeVarGccConf = grade_var_gcc_conf_jump
    else if GccConf = svalue_gcc_conf_fast then
        GradeVarGccConf = grade_var_gcc_conf_fast
    else if GccConf = svalue_gcc_conf_asm_jump then
        GradeVarGccConf = grade_var_gcc_conf_asm_jump
    else if GccConf = svalue_gcc_conf_asm_fast then
        GradeVarGccConf = grade_var_gcc_conf_asm_fast
    else
        unexpected($pred, "unexpected value of GccConf")
    ),

    ( if LowTagBitsUse = svalue_low_tag_bits_use_0 then
        GradeVarLowTagBitsUse = grade_var_low_tag_bits_use_0
    else if LowTagBitsUse = svalue_low_tag_bits_use_2 then
        GradeVarLowTagBitsUse = grade_var_low_tag_bits_use_2
    else if LowTagBitsUse = svalue_low_tag_bits_use_3 then
        GradeVarLowTagBitsUse = grade_var_low_tag_bits_use_3
    else
        unexpected($pred, "unexpected value of LowTagBitsUse")
    ),

    ( if StackLen = svalue_stack_len_std then
        GradeVarStackLen = grade_var_stack_len_std
    else if StackLen = svalue_stack_len_segments then
        GradeVarStackLen = grade_var_stack_len_segments
    else if StackLen = svalue_stack_len_extend then
        GradeVarStackLen = grade_var_stack_len_extend
    else
        unexpected($pred, "unexpected value of StackLen")
    ),

    ( if Trail = svalue_trail_no then
        GradeVarTrail = grade_var_trail_no
    else if Trail = svalue_trail_yes then
        GradeVarTrail = grade_var_trail_yes
    else
        unexpected($pred, "unexpected value of Trail")
    ),

    ( if MinimalModel = svalue_minmodel_no then
        GradeVarMinimalModel = grade_var_minmodel_no
    else if MinimalModel = svalue_minmodel_stack_copy then
        GradeVarMinimalModel = grade_var_minmodel_stack_copy
    else if MinimalModel = svalue_minmodel_stack_copy_debug then
        GradeVarMinimalModel = grade_var_minmodel_stack_copy_debug
    else if MinimalModel = svalue_minmodel_own_stack then
        GradeVarMinimalModel = grade_var_minmodel_own_stack
    else if MinimalModel = svalue_minmodel_own_stack_debug then
        GradeVarMinimalModel = grade_var_minmodel_own_stack_debug
    else
        unexpected($pred, "unexpected value of MinimalModel")
    ),

    ( if ThreadSafe = svalue_thread_safe_c_no then
        GradeVarThreadSafe = grade_var_thread_safe_c_no
    else if ThreadSafe = svalue_thread_safe_c_yes then
        GradeVarThreadSafe = grade_var_thread_safe_c_yes
    else if ThreadSafe = svalue_thread_safe_target_native then
        GradeVarThreadSafe = grade_var_thread_safe_target_native
    else
        unexpected($pred, "unexpected value of ThreadSafe")
    ),

    ( if Gc = svalue_gc_none then
        GradeVarGc = grade_var_gc_none
    else if Gc = svalue_gc_bdw then
        GradeVarGc = grade_var_gc_bdw
    else if Gc = svalue_gc_bdw_debug then
        GradeVarGc = grade_var_gc_bdw_debug
    else if Gc = svalue_gc_target_native then
        GradeVarGc = grade_var_gc_target_native
    else if Gc = svalue_gc_accurate then
        GradeVarGc = grade_var_gc_accurate
    else if Gc = svalue_gc_history then
        GradeVarGc = grade_var_gc_history
    else
        unexpected($pred, "unexpected value of Gc")
    ),

    ( if DeepProf = svalue_deep_prof_no then
        GradeVarDeepProf = grade_var_deep_prof_no
    else if DeepProf = svalue_deep_prof_yes then
        GradeVarDeepProf = grade_var_deep_prof_yes
    else
        unexpected($pred, "unexpected value of DeepProf")
    ),

    ( if MprofCall = svalue_mprof_call_no then
        GradeVarMprofCall = grade_var_mprof_call_no
    else if MprofCall = svalue_mprof_call_yes then
        GradeVarMprofCall = grade_var_mprof_call_yes
    else
        unexpected($pred, "unexpected value of MprofCall")
    ),

    ( if MprofTime = svalue_mprof_time_no then
        GradeVarMprofTime = grade_var_mprof_time_no
    else if MprofTime = svalue_mprof_time_yes then
        GradeVarMprofTime = grade_var_mprof_time_yes
    else
        unexpected($pred, "unexpected value of MprofTime")
    ),

    ( if MprofMemory = svalue_mprof_memory_no then
        GradeVarMprofMemory = grade_var_mprof_memory_no
    else if MprofMemory = svalue_mprof_memory_yes then
        GradeVarMprofMemory = grade_var_mprof_memory_yes
    else
        unexpected($pred, "unexpected value of MprofMemory")
    ),

    ( if TScopeProf = svalue_tscope_prof_no then
        GradeVarTScopeProf = grade_var_tscope_prof_no
    else if TScopeProf = svalue_tscope_prof_yes then
        GradeVarTScopeProf = grade_var_tscope_prof_yes
    else
        unexpected($pred, "unexpected value of TScopeProf")
    ),

    ( if TermSizeProf = svalue_term_size_prof_no then
        GradeVarTermSizeProf = grade_var_term_size_prof_no
    else if TermSizeProf = svalue_term_size_prof_cells then
        GradeVarTermSizeProf = grade_var_term_size_prof_cells
    else if TermSizeProf = svalue_term_size_prof_words then
        GradeVarTermSizeProf = grade_var_term_size_prof_words
    else
        unexpected($pred, "unexpected value of TermSizeProf")
    ),

    ( if Debug = svalue_debug_none then
        GradeVarDebug = grade_var_debug_none
    else if Debug = svalue_debug_debug then
        GradeVarDebug = grade_var_debug_debug
    else if Debug = svalue_debug_decldebug then
        GradeVarDebug = grade_var_debug_decldebug
    else
        unexpected($pred, "unexpected value of Debug")
    ),

    ( if SSDebug = svalue_ssdebug_no then
        GradeVarSSDebug = grade_var_ssdebug_no
    else if SSDebug = svalue_ssdebug_yes then
        GradeVarSSDebug = grade_var_ssdebug_yes
    else
        unexpected($pred, "unexpected value of SSDebug")
    ),

    ( if TargetDebug = svalue_target_debug_no then
        GradeVarTargetDebug = grade_var_target_debug_no
    else if TargetDebug = svalue_target_debug_yes then
        GradeVarTargetDebug = grade_var_target_debug_yes
    else
        unexpected($pred, "unexpected value of TargetDebug")
    ),

    ( if RBMM = svalue_rbmm_no then
        GradeVarRBMM = grade_var_rbmm_no
    else if RBMM = svalue_rbmm_yes then
        GradeVarRBMM = grade_var_rbmm_yes
    else
        unexpected($pred, "unexpected value of RBMM")
    ),

    ( if RBMMDebug = svalue_rbmm_debug_no then
        GradeVarRBMMDebug = grade_var_rbmm_debug_no
    else if RBMMDebug = svalue_rbmm_debug_yes then
        GradeVarRBMMDebug = grade_var_rbmm_debug_yes
    else
        unexpected($pred, "unexpected value of RBMMDebug")
    ),

    ( if RBMMProf = svalue_rbmm_prof_no then
        GradeVarRBMMProf = grade_var_rbmm_prof_no
    else if RBMMProf = svalue_rbmm_prof_yes then
        GradeVarRBMMProf = grade_var_rbmm_prof_yes
    else
        unexpected($pred, "unexpected value of RBMMProf")
    ),

    ( if MercFile = svalue_ac_merc_file_no then
        GradeVarMercFile = grade_var_merc_file_no
    else if MercFile = svalue_ac_merc_file_yes then
        GradeVarMercFile = grade_var_merc_file_yes
    else
        unexpected($pred, "unexpected value of MercFile")
    ),

    ( if MercFloat = svalue_merc_float_is_boxed_c_double then
        GradeVarMercFloat = grade_var_merc_float_is_boxed_c_double
    else if MercFloat = svalue_merc_float_is_unboxed_c_double then
        GradeVarMercFloat = grade_var_merc_float_is_unboxed_c_double
    else if MercFloat = svalue_merc_float_is_unboxed_c_float then
        GradeVarMercFloat = grade_var_merc_float_is_unboxed_c_float
    else
        unexpected($pred, "unexpected value of MercFloat")
    ),

    GradeVars = grade_vars(GradeVarPregen, GradeVarBackend, GradeVarTarget,
        GradeVarGccConf, GradeVarLowTagBitsUse, GradeVarStackLen,
        GradeVarTrail,
        GradeVarMinimalModel, GradeVarThreadSafe, GradeVarGc,
        GradeVarDeepProf,
        GradeVarMprofCall, GradeVarMprofTime, GradeVarMprofMemory,
        GradeVarTScopeProf, GradeVarTermSizeProf,
        GradeVarDebug, GradeVarSSDebug, GradeVarTargetDebug,
        GradeVarRBMM, GradeVarRBMMDebug, GradeVarRBMMProf,
        GradeVarMercFile, GradeVarMercFloat
    ).

%---------------------------------------------------------------------------%
:- end_module grade_lib.grade_vars.
%---------------------------------------------------------------------------%
