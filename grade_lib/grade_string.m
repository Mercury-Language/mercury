%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_string.
:- interface.

:- import_module grade_solver.
:- import_module grade_structure.

:- import_module maybe.

%---------------------------------------------------------------------------%

:- type which_grade_string
    --->    grade_string_user
    ;       grade_string_link_check.

:- func grade_structure_to_grade_string(which_grade_string, grade_structure)
    = string.

%---------------------------------------------------------------------------%

:- func grade_string_to_succ_soln(string) =
    maybe_errors(success_soln_map, string).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module grade_spec.
:- import_module grade_vars.

:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

grade_structure_to_grade_string(WhichGradeString, GradeStructure) = GradeStr :-
    % XXX unboxed floats
    (
        GradeStructure = grade_llds(GccConf, LowTagBitsUse, StackLen, LLDSGc,
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
            ThreadSafeStr = thread_safe_to_str(grade_var_thread_safe_no),
            TScopeProfStr = ""
        ;
            LLDSThreadSafe = llds_thread_safe_yes(TScopeProf),
            ThreadSafeStr = thread_safe_to_str(grade_var_thread_safe_yes),
            (
                TScopeProf = grade_var_tscope_prof_no,
                TScopeProfStr = ""
            ;
                TScopeProf = grade_var_tscope_prof_yes,
                TScopeProfStr = ".threadscope"
            )
        ),
        ( LLDSGc = llds_gc_none,             Gc = grade_var_gc_none
        ; LLDSGc = llds_gc_bdw,              Gc = grade_var_gc_bdw
        ; LLDSGc = llds_gc_bdw_debug,        Gc = grade_var_gc_bdw_debug
        ; LLDSGc = llds_gc_history,          Gc = grade_var_gc_history
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
        (
            TermSizeProf = grade_var_term_size_prof_no,
            TermSizeProfStr = ""
        ;
            TermSizeProf = grade_var_term_size_prof_cells,
            TermSizeProfStr = ".tsc"
        ;
            TermSizeProf = grade_var_term_size_prof_words,
            TermSizeProfStr = ".tsw"
        ),
        TrailStr = c_trail_to_str(CTrail),
        (
            MinimalModel = grade_var_minmodel_no,
            MinimalModelStr = ""
        ;
            MinimalModel = grade_var_minmodel_stack_copy,
            MinimalModelStr = ".mmsc"
        ;
            MinimalModel = grade_var_minmodel_stack_copy_debug,
            MinimalModelStr = ".dmmsc"
        ;
            MinimalModel = grade_var_minmodel_own_stack,
            MinimalModelStr = ".mmos"
        ;
            MinimalModel = grade_var_minmodel_own_stack_debug,
            MinimalModelStr = ".dmmos"
        ),
        PregenStr = pregen_to_str(Pregen),
        SinglePrecFloatStr = single_prec_float_to_str(SinglePrecFloat),
        ( Debug = grade_var_debug_none,              DebugStr = ""
        ; Debug = grade_var_debug_debug,             DebugStr = ".debug"
        ; Debug = grade_var_debug_decldebug,         DebugStr = ".decldebug"
        ),
        ( LLDebug = grade_var_lldebug_no,            LLDebugStr = ""
        ; LLDebug = grade_var_lldebug_yes,           LLDebugStr = ".ll_debug"
        ),
        ( StackLen = grade_var_stack_len_std,        StackLenStr = ""
        ; StackLen = grade_var_stack_len_segments,   StackLenStr = ".stseg"
        ; StackLen = grade_var_stack_len_extend,     StackLenStr = ".exts"
        ),
        (
            LLDSRBMM = llds_rbmm_no,
            RBMMStr = ""
        ;
            LLDSRBMM = llds_rbmm_yes(RBMMDebug, RBMMProf),
            (
                RBMMDebug = grade_var_rbmm_debug_no,
                RBMMProf = grade_var_rbmm_prof_no,
                RBMMStr = ".rbmm"
            ;
                RBMMDebug = grade_var_rbmm_debug_no,
                RBMMProf = grade_var_rbmm_prof_yes,
                RBMMStr = ".rbmmp"
            ;
                RBMMDebug = grade_var_rbmm_debug_yes,
                RBMMProf = grade_var_rbmm_prof_no,
                RBMMStr = ".rbmmd"
            ;
                RBMMDebug = grade_var_rbmm_debug_yes,
                RBMMProf = grade_var_rbmm_prof_yes,
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
        GradeStructure = grade_mlds(MLDSTarget, SSDebug),
        SSDebugStr = ssdebug_to_str(SSDebug),
        (
            MLDSTarget = mlds_target_c(DataRep, NestedFuncs, LowTagBitsUse,
                ThreadSafe, MLDSCGc, CTrail, MLDSPerfProf,
                MercFile, Pregen, SinglePrecFloat),
            (
                DataRep = mlds_c_datarep_heap_cells,
                DataRepStr = "hlc"
            ;
                DataRep = mlds_c_datarep_classes,
                DataRepStr = "hl"
            ),
            (
                NestedFuncs = grade_var_nested_funcs_no,
                NestedFuncsStr = ""
            ;
                NestedFuncs = grade_var_nested_funcs_yes,
                NestedFuncsStr = "_nest"
            ),
            LowTagBitsUseStr =
                low_tag_bits_use_to_str(WhichGradeString, LowTagBitsUse),
            ( MLDSCGc = mlds_c_gc_none,             Gc = grade_var_gc_none
            ; MLDSCGc = mlds_c_gc_bdw,              Gc = grade_var_gc_bdw
            ; MLDSCGc = mlds_c_gc_bdw_debug,        Gc = grade_var_gc_bdw_debug
            ; MLDSCGc = mlds_c_gc_accurate,         Gc = grade_var_gc_accurate
            ; MLDSCGc = mlds_c_gc_history,          Gc = grade_var_gc_history
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
            GradeStr = string.append_list([DataRepStr, NestedFuncsStr,
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
        GradeStructure = grade_elds(SSDebug),
        SSDebugStr = ssdebug_to_str(SSDebug),
        GradeStr = string.append_list(["erlang", SSDebugStr])
    ).

:- func gc_to_str(grade_var_gc) = string.

gc_to_str(grade_var_gc_none) = "".
gc_to_str(grade_var_gc_target_native) = _ :-
    unexpected($pred, "grade_var_gc_target_native").
gc_to_str(grade_var_gc_bdw) = ".gc".
gc_to_str(grade_var_gc_bdw_debug) = ".gcd".
gc_to_str(grade_var_gc_accurate) = ".agc".
gc_to_str(grade_var_gc_history) = ".hgc".

:- func c_trail_to_str(c_trail) = string.

c_trail_to_str(c_trail_no) = "".
c_trail_to_str(c_trail_yes(grade_var_trail_segments_no)) = ".tr".
c_trail_to_str(c_trail_yes(grade_var_trail_segments_yes)) = ".trseg".

:- func thread_safe_to_str(grade_var_thread_safe) = string.

thread_safe_to_str(grade_var_thread_safe_no) = "".
thread_safe_to_str(grade_var_thread_safe_yes) = ".par".

:- func ssdebug_to_str(grade_var_ssdebug) = string.

ssdebug_to_str(grade_var_ssdebug_no) = "".
ssdebug_to_str(grade_var_ssdebug_yes) = ".ssdebug".

:- func mprof_to_string(grade_var_mprof_time, grade_var_mprof_memory) = string.

mprof_to_string(grade_var_mprof_time_no, grade_var_mprof_memory_no) =
    ".profcalls".
mprof_to_string(grade_var_mprof_time_no, grade_var_mprof_memory_yes) =
    ".memprof".
mprof_to_string(grade_var_mprof_time_yes, grade_var_mprof_memory_no) =".prof".
mprof_to_string(grade_var_mprof_time_yes, grade_var_mprof_memory_yes) =
    ".profall".

:- func pregen_to_str(grade_var_pregen) = string.

pregen_to_str(grade_var_pregen_no) = "".
pregen_to_str(grade_var_pregen_yes) = ".pregen".

:- func merc_file_to_str(which_grade_string, grade_var_merc_file) = string.

merc_file_to_str(grade_string_user, _) = "".
merc_file_to_str(grade_string_link_check, grade_var_merc_file_no) = "".
merc_file_to_str(grade_string_link_check, grade_var_merc_file_yes) = ".file".

:- func low_tag_bits_use_to_str(which_grade_string, grade_var_low_tag_bits_use)
    = string.

low_tag_bits_use_to_str(grade_string_user, _) = "".
low_tag_bits_use_to_str(grade_string_link_check, grade_var_low_tag_bits_use_0)
    = ".tags0".
low_tag_bits_use_to_str(grade_string_link_check, grade_var_low_tag_bits_use_2)
    = ".tags2".
low_tag_bits_use_to_str(grade_string_link_check, grade_var_low_tag_bits_use_3)
    = ".tags3".

:- func single_prec_float_to_str(grade_var_single_prec_float) = string.

single_prec_float_to_str(grade_var_single_prec_float_no) = "".
single_prec_float_to_str(grade_var_single_prec_float_yes) = ".spf".

%---------------------------------------------------------------------------%

:- type grade_component_map == map(solver_var_id, grade_component_entry).

:- type grade_component_entry
    --->    grade_component_entry(
                % The specified value of the solver var.
                solver_var_value_id,

                % The grade string component that specified that value.
                string
            ).

grade_string_to_succ_soln(GradeStr) = MaybeSuccMap :-
    ComponentStrs = string.split_at_char('.', GradeStr),
    accumulate_grade_component_map_loop(ComponentStrs,
        map.init, ComponentsMap, [], RevErrorMsgs),
    list.reverse(RevErrorMsgs, ErrorMsgs),
    (
        ErrorMsgs = [],
        map.map_values_only(project_value_only, ComponentsMap, SuccMap),
        MaybeSuccMap = ok(SuccMap)
    ;
        ErrorMsgs = [HeadErrorMsg | TailErrorMsgs],
        MaybeSuccMap = error(HeadErrorMsg, TailErrorMsgs)
    ).

:- pred project_value_only(grade_component_entry::in,
    solver_var_value_id::out) is det.

project_value_only(grade_component_entry(ValueId, _), ValueId).

:- pred accumulate_grade_component_map_loop(list(string)::in,
    grade_component_map::in, grade_component_map::out,
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
    grade_component_map::in, grade_component_map::out,
    list(string)::in, list(string)::out) is det.

apply_setting(ComponentStr, VarId - ValueId, !ComponentMap, !RevErrorMsgs) :-
    ( if map.search(!.ComponentMap, VarId, OldEntry) then
        OldEntry = grade_component_entry(OldValueId, OldComponentStr),
        ( if OldValueId = ValueId then
            true
        else
            string.format("grade components %s and %s are incompatible",
                [s(OldComponentStr), s(ComponentStr)], ErrorMsg),
            !:RevErrorMsgs = [ErrorMsg | !.RevErrorMsgs]
        )
    else
        Entry = grade_component_entry(ValueId, ComponentStr),
        map.det_insert(VarId, Entry, !ComponentMap)
    ).

:- pred translate_grade_component(string::in,
    pair(solver_var_id, solver_var_value_id)::out,
    list(pair(solver_var_id, solver_var_value_id))::out) is semidet.

translate_grade_component(ComponentStr, Setting, Settings) :-
    % Some of the settings we return are an expression of the inherent
    % meaning of the grade component, while others are their required
    % implications. The solver would *handle* the implications itself,
    % but including them here will give it less work to do and thus
    % make it faster.
    %
    % Including the implications is therefore nice, but not required.
    (
        ComponentStr = "none",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_target - svalue_target_c,
            svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_no,
            svar_gcc_regs_use - svalue_gcc_regs_use_no,
            svar_datarep - svalue_datarep_heap_cells,
            svar_nested_funcs - svalue_nested_funcs_no]
    ;
        ComponentStr = "reg",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_target - svalue_target_c,
            svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_no,
            svar_gcc_regs_use - svalue_gcc_regs_use_yes,
            svar_datarep - svalue_datarep_heap_cells,
            svar_nested_funcs - svalue_nested_funcs_no]
    ;
        ComponentStr = "jump",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_target - svalue_target_c,
            svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_yes,
            svar_gcc_regs_use - svalue_gcc_regs_use_no,
            svar_datarep - svalue_datarep_heap_cells,
            svar_nested_funcs - svalue_nested_funcs_no]
    ;
        ComponentStr = "fast",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_target - svalue_target_c,
            svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_yes,
            svar_gcc_regs_use - svalue_gcc_regs_use_yes,
            svar_datarep - svalue_datarep_heap_cells,
            svar_nested_funcs - svalue_nested_funcs_no]
    ;
        ComponentStr = "asm_jump",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_target - svalue_target_c,
            svar_gcc_labels_use - svalue_gcc_labels_use_yes,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_yes,
            svar_gcc_regs_use - svalue_gcc_regs_use_no,
            svar_datarep - svalue_datarep_heap_cells,
            svar_nested_funcs - svalue_nested_funcs_no]
    ;
        ComponentStr = "asm_fast",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_target - svalue_target_c,
            svar_gcc_labels_use - svalue_gcc_labels_use_yes,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_yes,
            svar_gcc_regs_use - svalue_gcc_regs_use_yes,
            svar_datarep - svalue_datarep_heap_cells,
            svar_nested_funcs - svalue_nested_funcs_no]
    ;
        ComponentStr = "hl",
        Setting = svar_target - svalue_target_c,
        Settings =
            [svar_backend - svalue_backend_mlds,
            svar_datarep - svalue_datarep_classes,
            svar_nested_funcs - svalue_nested_funcs_no,
            svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_no,
            svar_gcc_regs_use - svalue_gcc_regs_use_no]
    ;
        ComponentStr = "hlc",
        Setting = svar_target - svalue_target_c,
        Settings =
            [svar_backend - svalue_backend_mlds,
            svar_datarep - svalue_datarep_heap_cells,
            svar_nested_funcs - svalue_nested_funcs_no,
            svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_no,
            svar_gcc_regs_use - svalue_gcc_regs_use_no]
    ;
        ComponentStr = "hl_nest",
        Setting = svar_target - svalue_target_c,
        Settings =
            [svar_backend - svalue_backend_mlds,
            svar_datarep - svalue_datarep_classes,
            svar_nested_funcs - svalue_nested_funcs_yes,
            svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_no,
            svar_gcc_regs_use - svalue_gcc_regs_use_no]
    ;
        ComponentStr = "hlc_nest",
        Setting = svar_target - svalue_target_c,
        Settings =
            [svar_backend - svalue_backend_mlds,
            svar_datarep - svalue_datarep_heap_cells,
            svar_nested_funcs - svalue_nested_funcs_yes,
            svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_no,
            svar_gcc_regs_use - svalue_gcc_regs_use_no]
    ;
        ComponentStr = "csharp",
        Setting = svar_target - svalue_target_csharp,
        Settings =
            [svar_backend - svalue_backend_mlds,
            svar_datarep - svalue_datarep_classes,
            svar_nested_funcs - svalue_nested_funcs_no,
            svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_no,
            svar_gcc_regs_use - svalue_gcc_regs_use_no]
    ;
        ComponentStr = "java",
        Setting = svar_target - svalue_target_java,
        Settings =
            [svar_backend - svalue_backend_mlds,
            svar_datarep - svalue_datarep_classes,
            svar_nested_funcs - svalue_nested_funcs_no,
            svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_no,
            svar_gcc_regs_use - svalue_gcc_regs_use_no]
    ;
        ComponentStr = "erlang",
        Setting = svar_target - svalue_target_erlang,
        Settings =
            [svar_backend - svalue_backend_elds,
            svar_datarep - svalue_datarep_erlang,
            svar_nested_funcs - svalue_nested_funcs_no,
            svar_gcc_labels_use - svalue_gcc_labels_use_no,
            svar_gcc_gotos_use - svalue_gcc_gotos_use_no,
            svar_gcc_regs_use - svalue_gcc_regs_use_no]
    ;
        ComponentStr = "gc",
        Setting = svar_gc - svalue_gc_bdw,
        Settings = [svar_target - svalue_target_c]
    ;
        ComponentStr = "gcd",
        Setting = svar_gc - svalue_gc_bdw_debug,
        Settings = [svar_target - svalue_target_c]
    ;
        ComponentStr = "agc",
        Setting = svar_gc - svalue_gc_accurate,
        Settings = [svar_target - svalue_target_c]
    ;
        ComponentStr = "hgc",
        Setting = svar_gc - svalue_gc_history,
        Settings = [svar_target - svalue_target_c]
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
        Settings =
            [svar_target - svalue_target_c,
            svar_backend - svalue_backend_llds,
            svar_mprof_call - svalue_mprof_call_no,
            svar_mprof_time - svalue_mprof_time_no,
            svar_mprof_memory - svalue_mprof_memory_no]
    ;
        ComponentStr = "profcalls",
        Setting = svar_mprof_call - svalue_mprof_call_yes,
        Settings =
            [svar_mprof_time - svalue_mprof_time_no,
            svar_mprof_memory - svalue_mprof_memory_no,
            svar_deep_prof - svalue_deep_prof_no,
            svar_target - svalue_target_c]
    ;
        ComponentStr = "memprof",
        Setting = svar_mprof_call - svalue_mprof_call_yes,
        Settings =
            [svar_mprof_time - svalue_mprof_time_no,
            svar_mprof_memory - svalue_mprof_memory_yes,
            svar_deep_prof - svalue_deep_prof_no,
            svar_target - svalue_target_c]
    ;
        ComponentStr = "prof",
        Setting = svar_mprof_call - svalue_mprof_call_yes,
        Settings =
            [svar_mprof_time - svalue_mprof_time_yes,
            svar_mprof_memory - svalue_mprof_memory_no,
            svar_deep_prof - svalue_deep_prof_no,
            svar_target - svalue_target_c]
    ;
        ComponentStr = "profall",
        Setting = svar_mprof_call - svalue_mprof_call_yes,
        Settings =
            [svar_mprof_time - svalue_mprof_time_yes,
            svar_mprof_memory - svalue_mprof_memory_yes,
            svar_deep_prof - svalue_deep_prof_no,
            svar_target - svalue_target_c]
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
        Settings =
            [svar_target - svalue_target_c,
            svar_backend - svalue_backend_llds]
    ;
        ( ComponentStr = "dmm"
        ; ComponentStr = "dmmsc"
        ),
        Setting = svar_minmodel - svalue_minmodel_stack_copy_debug,
        Settings =
            [svar_target - svalue_target_c,
            svar_backend - svalue_backend_llds]
    ;
        ComponentStr = "mmos",
        Setting = svar_minmodel - svalue_minmodel_own_stack,
        Settings =
            [svar_target - svalue_target_c,
            svar_backend - svalue_backend_llds]
    ;
        ComponentStr = "dmmos",
        Setting = svar_minmodel - svalue_minmodel_own_stack_debug,
        Settings =
            [svar_target - svalue_target_c,
            svar_backend - svalue_backend_llds]
    ;
        ComponentStr = "spf",
        Setting = svar_single_prec_float - svalue_single_prec_float_yes,
        Settings =
            [svar_target - svalue_target_c]
    ;
        ComponentStr = "debug",
        Setting = svar_debug - svalue_debug_debug,
        Settings =
            [svar_target - svalue_target_c,
            svar_backend - svalue_backend_llds]
    ;
        ComponentStr = "decldebug",
        Setting = svar_debug - svalue_debug_decldebug,
        Settings =
            [svar_target - svalue_target_c,
            svar_backend - svalue_backend_llds]
    ;
        ComponentStr = "rbmm",
        Setting = svar_rbmm - svalue_rbmm_yes,
        Settings =
            [svar_rbmm_debug - svalue_rbmm_debug_no,
            svar_rbmm_prof - svalue_rbmm_prof_no,
            svar_target - svalue_target_c,
            svar_backend - svalue_backend_llds]
    ;
        ComponentStr = "rbmmp",
        Setting = svar_rbmm - svalue_rbmm_yes,
        Settings =
            [svar_rbmm_debug - svalue_rbmm_debug_no,
            svar_rbmm_prof - svalue_rbmm_prof_yes,
            svar_target - svalue_target_c,
            svar_backend - svalue_backend_llds]
    ;
        ComponentStr = "rbmmd",
        Setting = svar_rbmm - svalue_rbmm_yes,
        Settings =
            [svar_rbmm_debug - svalue_rbmm_debug_yes,
            svar_rbmm_prof - svalue_rbmm_prof_no,
            svar_target - svalue_target_c,
            svar_backend - svalue_backend_llds]
    ;
        ComponentStr = "rbmmdp",
        Setting = svar_rbmm - svalue_rbmm_yes,
        Settings =
            [svar_rbmm_debug - svalue_rbmm_debug_yes,
            svar_rbmm_prof - svalue_rbmm_prof_yes,
            svar_target - svalue_target_c,
            svar_backend - svalue_backend_llds]
    ;
        ComponentStr = "ssdebug",
        Setting = svar_ssdebug - svalue_ssdebug_yes,
        Settings = []
    ).

%---------------------------------------------------------------------------%
:- end_module grade_string.
%---------------------------------------------------------------------------%
