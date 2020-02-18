%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This module provides the means to convert grade structures to strings,
% and to parse grade strings into the settings of the (explicitly specified)
% grade variables.
%

:- module grade_lib.grade_string.
:- interface.

:- import_module grade_lib.grade_solver.
:- import_module grade_lib.grade_structure.

:- import_module maybe.

%---------------------------------------------------------------------------%

:- type which_grade_string
    --->    grade_string_user
    ;       grade_string_link_check.

    % Given a grade structure, convert it to a grade string. If the first
    % argument is grade_string_user, make this the kind of grade string
    % that users see: a '.'-separated list of user-visible grade components.
    % If it is grade_string_link_check, make it the string we use on the C
    % backend to check, at link time, that all modules are compiled in the
    % same grade. These link check grade strings separate the grade components
    % with '_'s, they include autoconfigured grade components such as tags2,
    % and (where relevant) they include version numbers.
    %
:- func grade_structure_to_grade_string(which_grade_string, grade_structure)
    = string.

%---------------------------------------------------------------------------%

    % Given a string representation of user-visible grade ('.'-separated,
    % no version numbers, no autoconf-generated grade components such as
    % .tags2), return a map that maps the grade variables whose values are
    % specified by the given grade components to the specified values.
    % The contents of this success_soln_map can be used to set up a
    % solver_var_map, and then the solver can be used to propagate through
    % the implications of the selected grade components.
    %
    % If the string is not a '.'-separated sequence of valid grade components,
    % and/or if the grade components specify inconsistent values for
    % one or more grade variables, return a list of error messages
    % describing the problem(s).
    %
:- func grade_string_to_succ_soln(string) =
    maybe_errors(success_soln_map, string).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module grade_lib.grade_spec.
:- import_module grade_lib.grade_vars.

:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%
%
% You should increment the general binary version compatibility number
% any time you make a change that breaks binary backwards compatibility.
% Note that the binary compatibility version number has no direct relationship
% with the source release number (which is in ../VERSION).
%
% It is a good idea to inspect all code for RTTI version number checks
% and remove them when increasing the binary compatibility version number.
% Searching for MR_RTTI_VERSION__ should find all code related to the
% RTTI version number.
%
% The exec_trace, deep_prof and llc_par version numbers should be incremented
% when a change breaks binary backwards compatibility only in debugging,
% deep profiling, and low-level C parallel grades respectively.
%

:- func link_grade_str_general_binary_compat_version = string.
:- func link_grade_str_exec_trace_version = string.
:- func link_grade_str_deep_prof_version = string.
:- func link_grade_str_llc_par_version = string.

link_grade_str_general_binary_compat_version = "v18".
link_grade_str_exec_trace_version = "12".
link_grade_str_deep_prof_version = "4".
link_grade_str_llc_par_version = "1".

%---------------------------------------------------------------------------%

grade_structure_to_grade_string(WhichGradeString, GradeStructure) = GradeStr :-
    (
        GradeStructure = grade_pregen(PregenKind),
        ( PregenKind = pregen_mlds_hlc,             BaseStr = "hlc"
        ; PregenKind = pregen_llds_none,            BaseStr = "none"
        ; PregenKind = pregen_llds_reg,             BaseStr = "reg"
        ; PregenKind = pregen_llds_asm_fast,        BaseStr = "asm_fast"
        ),
        (
            WhichGradeString = grade_string_user,
            GradeComponents = [BaseStr, "pregen"]
        ;
            WhichGradeString = grade_string_link_check,
            GradeComponents = [BaseStr, "tags2", "pregen"]
        )
    ;
        GradeStructure = grade_llds(GccConf, StackLen, LLDSTSMinModel,
            MercFile, LowTagBits, MercFloat, TargetDebug),

        BinaryCompatStrs = binary_compat_version_to_strs(WhichGradeString),
        ( GccConf = grade_var_gcc_conf_none,        BaseStr = "none"
        ; GccConf = grade_var_gcc_conf_reg,         BaseStr = "reg"
        ; GccConf = grade_var_gcc_conf_jump,        BaseStr = "jump"
        ; GccConf = grade_var_gcc_conf_fast,        BaseStr = "fast"
        ; GccConf = grade_var_gcc_conf_asm_jump,    BaseStr = "asm_jump"
        ; GccConf = grade_var_gcc_conf_asm_fast,    BaseStr = "asm_fast"
        ),
        (
            LLDSTSMinModel = llds_thread_safe_no_minmodel_no(CGc, CTrail,
                LLDSPerfProf, TermSizeProf, Debug, LLDSRBMM),
            ThreadSafeStrs = [],
            MinimalModelStrs = [],
            Gc = c_gc_to_gc(CGc),
            GcStrs = gc_to_strs(Gc),
            TrailStrs = c_trail_to_strs(CTrail),
            (
                LLDSPerfProf = llds_perf_prof_none,
                LLDSPerfProfStrs = []
            ;
                LLDSPerfProf = llds_perf_prof_deep,
                LLDSPerfProfStrs =
                    ["profdeep" ++
                    deep_prof_version_to_string(WhichGradeString)]
            ;
                LLDSPerfProf = llds_perf_prof_mprof(MprofTime, MprofMemory),
                LLDSPerfProfStrs = [mprof_to_str(MprofTime, MprofMemory)]
            ),
            (
                TermSizeProf = grade_var_term_size_prof_no,
                TermSizeProfStrs = []
            ;
                TermSizeProf = grade_var_term_size_prof_cells,
                TermSizeProfStrs = ["tsc"]
            ;
                TermSizeProf = grade_var_term_size_prof_words,
                TermSizeProfStrs = ["tsw"]
            ),
            DebugStrs = llds_debug_to_strs(WhichGradeString, Debug),
            (
                LLDSRBMM = llds_rbmm_no,
                RBMMStrs = []
            ;
                LLDSRBMM = llds_rbmm_yes(RBMMDebug, RBMMProf),
                (
                    RBMMDebug = grade_var_rbmm_debug_no,
                    RBMMProf = grade_var_rbmm_prof_no,
                    RBMMStrs = ["rbmm"]
                ;
                    RBMMDebug = grade_var_rbmm_debug_no,
                    RBMMProf = grade_var_rbmm_prof_yes,
                    RBMMStrs = ["rbmmp"]
                ;
                    RBMMDebug = grade_var_rbmm_debug_yes,
                    RBMMProf = grade_var_rbmm_prof_no,
                    RBMMStrs = ["rbmmd"]
                ;
                    RBMMDebug = grade_var_rbmm_debug_yes,
                    RBMMProf = grade_var_rbmm_prof_yes,
                    RBMMStrs = ["rbmmdp"]
                )
            ),
            TScopeProfStrs = []
        ;
            LLDSTSMinModel = llds_thread_safe_no_minmodel_yes(MinModelKind,
                LLDSMMGc, Debug),
            ThreadSafeStrs = [],
            (
                MinModelKind = lmk_stack_copy,
                MinimalModelStrs = ["mmsc"]
            ;
                MinModelKind = lmk_stack_copy_debug,
                MinimalModelStrs = ["dmmsc"]
            ;
                MinModelKind = lmk_own_stack,
                MinimalModelStrs = ["mmos"]
            ;
                MinModelKind = lmk_own_stack_debug,
                MinimalModelStrs = ["dmmos"]
            ),
            ( LLDSMMGc = llds_mm_gc_bdw,        Gc = grade_var_gc_bdw
            ; LLDSMMGc = llds_mm_gc_bdw_debug,  Gc = grade_var_gc_bdw_debug
            ),
            GcStrs = gc_to_strs(Gc),
            TrailStrs = [],
            LLDSPerfProfStrs = [],
            TermSizeProfStrs = [],
            DebugStrs = llds_debug_to_strs(WhichGradeString, Debug),
            TScopeProfStrs = [],
            RBMMStrs = []
        ;
            LLDSTSMinModel = llds_thread_safe_yes_minmodel_no(ThreadSafeGc,
                CTrail, TScopeProf),
            ThreadSafeStrs =
                thread_safe_to_strs(tsb_llds(WhichGradeString),
                    grade_var_thread_safe_c_yes),
            MinimalModelStrs = [],
            Gc = thread_safe_c_gc_to_gc(ThreadSafeGc),
            GcStrs = gc_to_strs(Gc),
            TrailStrs = c_trail_to_strs(CTrail),
            LLDSPerfProfStrs = [],
            TermSizeProfStrs = [],
            DebugStrs = [],
            RBMMStrs = [],
            (
                TScopeProf = grade_var_tscope_prof_no,
                TScopeProfStrs = []
            ;
                TScopeProf = grade_var_tscope_prof_yes,
                TScopeProfStrs = ["threadscope"]
            )
        ),
        ( StackLen = grade_var_stack_len_std,        StackLenStrs = []
        ; StackLen = grade_var_stack_len_segments,   StackLenStrs = ["stseg"]
        ; StackLen = grade_var_stack_len_extend,     StackLenStrs = ["exts"]
        ),
        TargetDebugStrs = target_debug_to_strs(TargetDebug),
        LowTagBitStrs = low_tag_bits_use_to_strs(WhichGradeString, LowTagBits),
        MercFloatStrs = merc_float_to_strs(WhichGradeString, MercFloat),
        MercFileStrs = merc_file_to_strs(WhichGradeString, MercFile),
        % XXX The order does NOT match the one in runtime/mercury_grade.h.
        % XXX The order here should be revisited.
        GradeComponents = BinaryCompatStrs ++ [BaseStr]
            ++ LowTagBitStrs ++ MercFloatStrs ++ ThreadSafeStrs ++ GcStrs
            ++ LLDSPerfProfStrs ++ TermSizeProfStrs
            ++ TrailStrs ++ MinimalModelStrs ++ MercFileStrs
            ++ DebugStrs ++ TargetDebugStrs
            ++ StackLenStrs ++ RBMMStrs ++ TScopeProfStrs
    ;
        GradeStructure = grade_mlds(MLDSTarget, TargetDebug),
        TargetDebugStrs = target_debug_to_strs(TargetDebug),
        (
            MLDSTarget = mlds_target_c(DataRep, MLDSCThreadSafe, CTrail,
                MercFile, LowTagBits, MercFloat),
            BinaryCompatStrs =
                binary_compat_version_to_strs(WhichGradeString),
            (
                DataRep = mlds_c_datarep_heap_cells,
                DataRepStr = "hlc"
            ;
                DataRep = mlds_c_datarep_classes,
                DataRepStr = "hl"
            ),
            BaseStr = DataRepStr,
            TrailStrs = c_trail_to_strs(CTrail),
            LowTagBitStrs =
                low_tag_bits_use_to_strs(WhichGradeString, LowTagBits),
            MercFloatStrs = merc_float_to_strs(WhichGradeString, MercFloat),
            (
                MLDSCThreadSafe = mlds_c_thread_safe_no(CGc, MLDSPerfProf,
                    SSDebug),
                ThreadSafeStrs = thread_safe_to_strs(tsb_mlds,
                    grade_var_thread_safe_c_no),
                Gc = c_gc_to_gc(CGc),
                GcStrs = gc_to_strs(Gc),
                (
                    MLDSPerfProf = mlds_c_perf_prof_none,
                    MLDSPerfProfStrs = []
                ;
                    MLDSPerfProf =
                        mlds_c_perf_prof_mprof(MprofTime, MprofMemory),
                    MLDSPerfProfStrs = [mprof_to_str(MprofTime, MprofMemory)]
                ),
                SSDebugStrs = ssdebug_to_strs(WhichGradeString, SSDebug)
            ;
                MLDSCThreadSafe = mlds_c_thread_safe_yes(ThreadSafeGc),
                ThreadSafeStrs = thread_safe_to_strs(tsb_mlds,
                    grade_var_thread_safe_c_yes),
                Gc = thread_safe_c_gc_to_gc(ThreadSafeGc),
                GcStrs = gc_to_strs(Gc),
                MLDSPerfProfStrs = [],
                SSDebugStrs = []
            ),
            MercFileStrs = merc_file_to_strs(WhichGradeString, MercFile),
            % XXX The order does NOT match the one in runtime/mercury_grade.h.
            % XXX The order here should be revisited.
            GradeComponents = BinaryCompatStrs ++ [BaseStr]
                ++ LowTagBitStrs ++ MercFloatStrs
                ++ ThreadSafeStrs ++ SSDebugStrs
                ++ TargetDebugStrs ++ GcStrs ++ TrailStrs
                ++ MLDSPerfProfStrs ++ MercFileStrs
        ;
            MLDSTarget = mlds_target_csharp(SSDebug),
            SSDebugStrs = ssdebug_to_strs(WhichGradeString, SSDebug),
            GradeComponents = ["csharp" | SSDebugStrs] ++ TargetDebugStrs
        ;
            MLDSTarget = mlds_target_java(SSDebug),
            SSDebugStrs = ssdebug_to_strs(WhichGradeString, SSDebug),
            GradeComponents = ["java" | SSDebugStrs] ++ TargetDebugStrs
        )
    ;
        GradeStructure = grade_elds(SSDebug, TargetDebug),
        SSDebugStrs = ssdebug_to_strs(WhichGradeString, SSDebug),
        TargetDebugStrs = target_debug_to_strs(TargetDebug),
        GradeComponents = ["erlang" | SSDebugStrs] ++ TargetDebugStrs
    ),
    (
        WhichGradeString = grade_string_user,
        GradeStr = string.join_list(".", GradeComponents)
    ;
        WhichGradeString = grade_string_link_check,
        GradeStr = string.join_list("_", GradeComponents)
    ).

:- func c_gc_to_gc(c_gc) = grade_var_gc.

c_gc_to_gc(CGc) = Gc :-
    ( CGc = c_gc_none,         Gc = grade_var_gc_none
    ; CGc = c_gc_bdw,          Gc = grade_var_gc_bdw
    ; CGc = c_gc_bdw_debug,    Gc = grade_var_gc_bdw_debug
    ; CGc = c_gc_accurate,     Gc = grade_var_gc_accurate
    ; CGc = c_gc_history,      Gc = grade_var_gc_history
    ).

:- func thread_safe_c_gc_to_gc(thread_safe_c_gc) = grade_var_gc.

thread_safe_c_gc_to_gc(ThreadSafeGc) = Gc :-
    ( ThreadSafeGc = thread_safe_c_gc_none,         Gc = grade_var_gc_none
    ; ThreadSafeGc = thread_safe_c_gc_bdw,          Gc = grade_var_gc_bdw
    ; ThreadSafeGc = thread_safe_c_gc_bdw_debug,    Gc = grade_var_gc_bdw_debug
    ).

:- func gc_to_strs(grade_var_gc) = list(string).

gc_to_strs(grade_var_gc_none) = [].
gc_to_strs(grade_var_gc_target_native) = _ :-
    unexpected($pred, "grade_var_gc_target_native").
gc_to_strs(grade_var_gc_bdw) = ["gc"].
gc_to_strs(grade_var_gc_bdw_debug) = ["gcd"].
gc_to_strs(grade_var_gc_accurate) = ["agc"].
gc_to_strs(grade_var_gc_history) = ["hgc"].

:- func c_trail_to_strs(c_trail) = list(string).

c_trail_to_strs(c_trail_no) = [].
c_trail_to_strs(c_trail_yes) = ["tr"].

:- type thread_safe_backend
    --->    tsb_llds(which_grade_string)
    ;       tsb_mlds.

:- func thread_safe_to_strs(thread_safe_backend, grade_var_thread_safe)
    = list(string).

thread_safe_to_strs(_, grade_var_thread_safe_target_native) = _ :-
    unexpected($pred, "grade_var_thread_safe_target_native").
thread_safe_to_strs(_, grade_var_thread_safe_c_no) = [].
thread_safe_to_strs(Backend, grade_var_thread_safe_c_yes) = [Str] :-
    % XXX We include a version number in the link check grade string
    % for the LLDS backend. What makes it unnecessary to do likewise
    % for the MLDS backend?
    (
        ( Backend = tsb_mlds
        ; Backend = tsb_llds(grade_string_user)
        ),
        Str = "par"
    ;
        Backend = tsb_llds(grade_string_link_check),
        Str = "par" ++ link_grade_str_llc_par_version
    ).

:- func llds_debug_to_strs(which_grade_string, grade_var_debug) = list(string).

llds_debug_to_strs(_WhichGradeString, grade_var_debug_none) = [].
llds_debug_to_strs(WhichGradeString, grade_var_debug_debug) =
    ["debug" ++ exec_trace_version_to_string(WhichGradeString)].
llds_debug_to_strs(WhichGradeString, grade_var_debug_decldebug) =
    ["decldebug" ++ exec_trace_version_to_string(WhichGradeString)].

:- func ssdebug_to_strs(which_grade_string, grade_var_ssdebug) = list(string).

ssdebug_to_strs(_, grade_var_ssdebug_no) = [].
ssdebug_to_strs(grade_string_user, grade_var_ssdebug_yes) =
    ["ssdebug"].
ssdebug_to_strs(grade_string_link_check, grade_var_ssdebug_yes) =
    % XXX Why use a version string intented for the LLDS debugger?
    ["ssdebug" ++ link_grade_str_exec_trace_version].

:- func target_debug_to_strs(grade_var_target_debug) = list(string).

target_debug_to_strs(grade_var_target_debug_no) = [].
target_debug_to_strs(grade_var_target_debug_yes) = ["ll_debug"].

:- func mprof_to_str(grade_var_mprof_time, grade_var_mprof_memory) = string.

mprof_to_str(grade_var_mprof_time_no, grade_var_mprof_memory_no) = "profcalls".
mprof_to_str(grade_var_mprof_time_no, grade_var_mprof_memory_yes) = "memprof".
mprof_to_str(grade_var_mprof_time_yes, grade_var_mprof_memory_no) = "prof".
mprof_to_str(grade_var_mprof_time_yes, grade_var_mprof_memory_yes) = "profall".

:- func merc_file_to_strs(which_grade_string, grade_var_merc_file)
    = list(string).

merc_file_to_strs(grade_string_user, _) = [].
merc_file_to_strs(grade_string_link_check, grade_var_merc_file_no) = [].
merc_file_to_strs(grade_string_link_check, grade_var_merc_file_yes) = ["file"].

:- func low_tag_bits_use_to_strs(which_grade_string,
    grade_var_low_tag_bits_use) = list(string).

low_tag_bits_use_to_strs(WhichGradeString, LowTagBits) = Strs :-
    (
        WhichGradeString = grade_string_user,
        Strs = []
    ;
        WhichGradeString = grade_string_link_check,
        (
            LowTagBits = grade_var_low_tag_bits_use_0,
            Strs = ["notags"]
        ;
            LowTagBits = grade_var_low_tag_bits_use_2,
            Strs = ["tags2"]
        ;
            LowTagBits = grade_var_low_tag_bits_use_3,
            Strs = ["tags3"]
        )
    ).

:- func merc_float_to_strs(which_grade_string, grade_var_merc_float)
    = list(string).

merc_float_to_strs(WhichGradeString, MercFloat) = Strs :-
    (
        MercFloat = grade_var_merc_float_is_boxed_c_double,
        Strs = []
    ;
        MercFloat = grade_var_merc_float_is_unboxed_c_double,
        (
            WhichGradeString = grade_string_user,
            Strs = []
        ;
            WhichGradeString = grade_string_link_check,
            Strs = ["ubf"]
        )
    ;
        MercFloat = grade_var_merc_float_is_unboxed_c_float,
        Strs = ["spf"]
    ).

%---------------------------------------------------------------------------%

:- func binary_compat_version_to_strs(which_grade_string) = list(string).

binary_compat_version_to_strs(grade_string_user) = [].
binary_compat_version_to_strs(grade_string_link_check) =
    [link_grade_str_general_binary_compat_version].

:- func exec_trace_version_to_string(which_grade_string) = string.

exec_trace_version_to_string(grade_string_user) = "".
exec_trace_version_to_string(grade_string_link_check) =
    link_grade_str_exec_trace_version.

:- func deep_prof_version_to_string(which_grade_string) = string.

deep_prof_version_to_string(grade_string_user) = "".
deep_prof_version_to_string(grade_string_link_check) =
    link_grade_str_deep_prof_version.

%---------------------------------------------------------------------------%

:- type grade_component_map == map(solver_var_id, grade_component_entry).

:- type grade_component_entry
    --->    grade_component_entry(
                % The specified value of the solver var.
                solver_var_value_id,

                % The grade string component that specified that value.
                % We record this so that we can generate meaningful error
                % messages for conflicts between grade components.
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
    % but including them here will give it less work to do, and will
    % therefore make the solver do its work *faster*.
    %
    % Including the implications is therefore nice, but not required.
    (
        ComponentStr = "none",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_target - svalue_target_c,
            svar_gcc_conf - svalue_gcc_conf_none,
            svar_datarep - svalue_datarep_heap_cells]
    ;
        ComponentStr = "reg",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_target - svalue_target_c,
            svar_gcc_conf - svalue_gcc_conf_reg,
            svar_datarep - svalue_datarep_heap_cells]
    ;
        ComponentStr = "jump",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_target - svalue_target_c,
            svar_gcc_conf - svalue_gcc_conf_jump,
            svar_datarep - svalue_datarep_heap_cells]
    ;
        ComponentStr = "fast",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_target - svalue_target_c,
            svar_gcc_conf - svalue_gcc_conf_fast,
            svar_datarep - svalue_datarep_heap_cells]
    ;
        ComponentStr = "asm_jump",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_target - svalue_target_c,
            svar_gcc_conf - svalue_gcc_conf_asm_jump,
            svar_datarep - svalue_datarep_heap_cells]
    ;
        ComponentStr = "asm_fast",
        Setting = svar_backend - svalue_backend_llds,
        Settings =
            [svar_target - svalue_target_c,
            svar_gcc_conf - svalue_gcc_conf_asm_fast,
            svar_datarep - svalue_datarep_heap_cells]
    ;
        ComponentStr = "hl",
        Setting = svar_target - svalue_target_c,
        Settings =
            [svar_backend - svalue_backend_mlds,
            svar_datarep - svalue_datarep_classes,
            svar_gcc_conf - svalue_gcc_conf_none]
    ;
        ComponentStr = "hlc",
        Setting = svar_target - svalue_target_c,
        Settings =
            [svar_backend - svalue_backend_mlds,
            svar_datarep - svalue_datarep_heap_cells,
            svar_gcc_conf - svalue_gcc_conf_none]
    ;
        ComponentStr = "csharp",
        Setting = svar_target - svalue_target_csharp,
        Settings =
            [svar_backend - svalue_backend_mlds,
            svar_datarep - svalue_datarep_classes,
            svar_gcc_conf - svalue_gcc_conf_none]
    ;
        ComponentStr = "java",
        Setting = svar_target - svalue_target_java,
        Settings =
            [svar_backend - svalue_backend_mlds,
            svar_datarep - svalue_datarep_classes,
            svar_gcc_conf - svalue_gcc_conf_none]
    ;
        ComponentStr = "erlang",
        Setting = svar_target - svalue_target_erlang,
        Settings =
            [svar_backend - svalue_backend_elds,
            svar_datarep - svalue_datarep_erlang,
            svar_gcc_conf - svalue_gcc_conf_none]
    ;
        ComponentStr = "par",
        Setting = svar_thread_safe - svalue_thread_safe_c_yes,
        Settings = []
    ;
        ComponentStr = "threadscope",
        Setting = svar_tscope_prof - svalue_tscope_prof_yes,
        Settings = [svar_thread_safe - svalue_thread_safe_c_yes]
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
        ( ComponentStr = "tr"
        ; ComponentStr = "trseg"
        ),
        Setting = svar_trail - svalue_trail_yes,
        Settings = []
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
        ComponentStr = "ll_debug",
        Setting = svar_target_debug - svalue_target_debug_yes,
        Settings = []
    ;
        ComponentStr = "stseg",
        Setting = svar_stack_len - svalue_stack_len_segments,
        Settings = []
    ;
        ComponentStr = "exts",
        Setting = svar_stack_len - svalue_stack_len_extend,
        Settings = []
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
        ComponentStr = "pregen",
        Setting = svar_pregen - svalue_pregen_yes,
        Settings =
            [svar_low_tag_bits_use - svalue_low_tag_bits_use_2,
            svar_merc_float - svalue_merc_float_is_boxed_c_double]
    ;
        ComponentStr = "spf",
        Setting = svar_request_single_prec_float -
            svalue_request_single_prec_float_yes,
        Settings = [svar_target - svalue_target_c]
    ;
        ComponentStr = "ssdebug",
        Setting = svar_ssdebug - svalue_ssdebug_yes,
        Settings = []
    ).

%---------------------------------------------------------------------------%
:- end_module grade_lib.grade_string.
%---------------------------------------------------------------------------%
