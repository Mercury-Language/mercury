%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Maps the solver_var_ids and solver_var_value_ids to strings and vice versa.
% These names are used by choose_grade.m to specify what grade problem
% the user wishes to set up, and by error messages during the solver setup
% process.
%
% XXX The compiler will use a mechanism for specifying grade problems
% to be solved using a mechanism ENTIRELY SEPARATE from this one.
%

:- module var_value_names.
:- interface.

:- import_module grade_lib.
:- import_module grade_lib.grade_spec.

%---------------------------------------------------------------------------%

:- pred solver_var_name(string, solver_var_id).
:- mode solver_var_name(in, out) is semidet.
:- mode solver_var_name(out, in) is det.

:- pred solver_var_value_name(string, solver_var_value_id).
:- mode solver_var_value_name(in, out) is semidet.
:- mode solver_var_value_name(out, in) is det.

%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

solver_var_name("gcc_regs_avail",               svar_ac_gcc_regs_avail).
solver_var_name("gcc_gotos_avail",              svar_ac_gcc_gotos_avail).
solver_var_name("gcc_labels_avail",             svar_ac_gcc_labels_avail).
solver_var_name("low_tag_bits_avail",           svar_ac_low_tag_bits_avail).
solver_var_name("size_of_double",               svar_ac_size_of_double).
solver_var_name("mercuryfile",                  svar_ac_merc_file).

solver_var_name("backend",                      svar_backend).
solver_var_name("target",                       svar_target).
solver_var_name("gcc_conf",                     svar_gcc_conf).
solver_var_name("low_tag_bits_use",             svar_low_tag_bits_use).
solver_var_name("stack_len",                    svar_stack_len).
solver_var_name("trail",                        svar_trail).
solver_var_name("minmodel",                     svar_minmodel).
solver_var_name("thread_safe",                  svar_thread_safe).
solver_var_name("gc",                           svar_gc).
solver_var_name("deep_prof",                    svar_deep_prof).
solver_var_name("mprof_call",                   svar_mprof_call).
solver_var_name("mprof_time",                   svar_mprof_time).
solver_var_name("mprof_memory",                 svar_mprof_memory).
solver_var_name("tscope_prof",                  svar_tscope_prof).
solver_var_name("term_size_prof",               svar_term_size_prof).
solver_var_name("debug",                        svar_debug).
solver_var_name("ssdebug",                      svar_ssdebug).
solver_var_name("target_debug",                 svar_target_debug).
solver_var_name("rbmm",                         svar_rbmm).
solver_var_name("rbmm_debug",                   svar_rbmm_debug).
solver_var_name("rbmm_prof",                    svar_rbmm_prof).
solver_var_name("pregen",                       svar_pregen).
solver_var_name("single_prec_float",            svar_request_single_prec_float).
solver_var_name("merc_float",                   svar_merc_float).

solver_var_value_name("gcc_regs_not_avail",     svalue_ac_gcc_regs_avail_no).
solver_var_value_name("gcc_regs_avail",         svalue_ac_gcc_regs_avail_yes).

solver_var_value_name("gcc_gotos_not_avail",    svalue_ac_gcc_gotos_avail_no).
solver_var_value_name("gcc_gotos_avail",        svalue_ac_gcc_gotos_avail_yes).

solver_var_value_name("gcc_labels_not_avail",   svalue_ac_gcc_labels_avail_no).
solver_var_value_name("gcc_labels_avail",       svalue_ac_gcc_labels_avail_yes).

solver_var_value_name("low_tag_bits_avail_0",   svalue_ac_low_tag_bits_avail_0).
solver_var_value_name("low_tag_bits_avail_2",   svalue_ac_low_tag_bits_avail_2).
solver_var_value_name("low_tag_bits_avail_3",   svalue_ac_low_tag_bits_avail_3).

solver_var_value_name("size_of_double_eq_ptr", svalue_ac_size_of_double_eq_ptr).
solver_var_value_name("size_of_double_ne_ptr", svalue_ac_size_of_double_ne_ptr).

solver_var_value_name("no_mercuryfile",         svalue_ac_merc_file_no).
solver_var_value_name("mercuryfile",            svalue_ac_merc_file_yes).

solver_var_value_name("mlds",                   svalue_backend_mlds).
solver_var_value_name("llds",                   svalue_backend_llds).

solver_var_value_name("c",                      svalue_target_c).
solver_var_value_name("csharp",                 svalue_target_csharp).
solver_var_value_name("java",                   svalue_target_java).

solver_var_value_name("none",                   svalue_gcc_conf_none).
solver_var_value_name("reg",                    svalue_gcc_conf_reg).
solver_var_value_name("jump",                   svalue_gcc_conf_jump).
solver_var_value_name("fast",                   svalue_gcc_conf_fast).
solver_var_value_name("asm_jump",               svalue_gcc_conf_asm_jump).
solver_var_value_name("asm_fast",               svalue_gcc_conf_asm_fast).

solver_var_value_name("low_tag_bits_use_0",     svalue_low_tag_bits_use_0).
solver_var_value_name("low_tag_bits_use_2",     svalue_low_tag_bits_use_2).
solver_var_value_name("low_tag_bits_use_3",     svalue_low_tag_bits_use_3).

solver_var_value_name("stfix",                  svalue_stack_len_std).
solver_var_value_name("stseg",                  svalue_stack_len_segments).
solver_var_value_name("exts",                   svalue_stack_len_extend).

solver_var_value_name("no_trail",               svalue_trail_no).
solver_var_value_name("trail",                  svalue_trail_yes).

solver_var_value_name("no_mm",                  svalue_minmodel_no).
solver_var_value_name("mm_stack_copy",
                                    svalue_minmodel_stack_copy).
solver_var_value_name("mm_stack_copy_debug",
                                    svalue_minmodel_stack_copy_debug).
solver_var_value_name("mm_own_stack",
                                    svalue_minmodel_own_stack).
solver_var_value_name("mm_own_stack_debug",
                                    svalue_minmodel_own_stack_debug).

solver_var_value_name("not_thread_safe",        svalue_thread_safe_c_no).
solver_var_value_name("thread_safe",            svalue_thread_safe_c_yes).
solver_var_value_name("native_thread_safe",
                                    svalue_thread_safe_target_native).

solver_var_value_name("no_gc",                  svalue_gc_none).
solver_var_value_name("bdw",                    svalue_gc_bdw).
solver_var_value_name("bdw_debug",              svalue_gc_bdw_debug).
solver_var_value_name("target_native",          svalue_gc_target_native).
solver_var_value_name("accurate",               svalue_gc_accurate).
solver_var_value_name("history",                svalue_gc_history).

solver_var_value_name("no_deep_prof",           svalue_deep_prof_no).
solver_var_value_name("deep_prof",              svalue_deep_prof_yes).

solver_var_value_name("no_mprof_call",          svalue_mprof_call_no).
solver_var_value_name("mprof_call",             svalue_mprof_call_yes).

solver_var_value_name("no_mprof_time",          svalue_mprof_time_no).
solver_var_value_name("mprof_time",             svalue_mprof_time_yes).

solver_var_value_name("no_mprof_memory",        svalue_mprof_memory_no).
solver_var_value_name("mprof_memory",           svalue_mprof_memory_yes).

solver_var_value_name("no_tscope_prof",         svalue_tscope_prof_no).
solver_var_value_name("tscope_prof",            svalue_tscope_prof_yes).

solver_var_value_name("no_term_size_prof",      svalue_term_size_prof_no).
solver_var_value_name("term_size_prof_cells",   svalue_term_size_prof_cells).
solver_var_value_name("term_size_prof_words",   svalue_term_size_prof_words).

solver_var_value_name("nodebug",                svalue_debug_none).
solver_var_value_name("debug",                  svalue_debug_debug).
solver_var_value_name("decldebug",              svalue_debug_decldebug).

solver_var_value_name("no_ssdebug",             svalue_ssdebug_no).
solver_var_value_name("ssdebug",                svalue_ssdebug_yes).

solver_var_value_name("no_target_debug",        svalue_target_debug_no).
solver_var_value_name("target_debug",           svalue_target_debug_yes).

solver_var_value_name("no_rbmm",                svalue_rbmm_no).
solver_var_value_name("rbmm",                   svalue_rbmm_yes).

solver_var_value_name("no_rbmm_debug",          svalue_rbmm_debug_no).
solver_var_value_name("rbmm_debug",             svalue_rbmm_debug_yes).

solver_var_value_name("no_rbmm_prof",           svalue_rbmm_prof_no).
solver_var_value_name("rbmm_prof",              svalue_rbmm_prof_yes).

solver_var_value_name("no_pregen",              svalue_pregen_no).
solver_var_value_name("pregen",                 svalue_pregen_yes).

solver_var_value_name("no_spf",         svalue_request_single_prec_float_no).
solver_var_value_name("spf",            svalue_request_single_prec_float_yes).

solver_var_value_name("boxed_double",   svalue_merc_float_is_boxed_c_double).
solver_var_value_name("unboxed_double", svalue_merc_float_is_unboxed_c_double).
solver_var_value_name("unboxed_float",  svalue_merc_float_is_unboxed_c_float).

%---------------------------------------------------------------------------%
:- end_module var_value_names.
%---------------------------------------------------------------------------%
