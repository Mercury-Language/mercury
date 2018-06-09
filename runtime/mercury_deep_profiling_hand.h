// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2001, 2004, 2006-2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_deep_profiling_hand.h -- definitions for deep profiling
// for use in hand-written procedures.

#ifndef MERCURY_DEEP_PROFILING_HAND_H
#define MERCURY_DEEP_PROFILING_HAND_H

#include "mercury_std.h"
#include "mercury_deep_profiling.h"

////////////////////////////////////////////////////////////////////////////

#define MR_proc_static_user_name(module, name, arity, mode)             \
    MR_PASTE8(mercury_data__proc_static__,                              \
        module, __, name, _, arity, _, mode)
#define MR_call_sites_user_name(module, name, arity, mode)              \
    MR_PASTE8(mercury_data__proc_static_call_sites__mercury__,          \
        module, __, name, _, arity, _, mode)

#define MR_proc_static_uci_name(module, name, type, arity, mode)        \
    MR_PASTE10(mercury_data__proc_static__,                             \
        name, _, module, __, type, _, arity, _, mode)
#define MR_call_sites_uci_name(module, name, type, arity, mode)         \
    MR_PASTE10(mercury_data__proc_static_call_sites__mercury__,         \
        name, _, module, __, type, _, arity, _, mode)

////////////////////////////////////////////////////////////////////////////

#ifdef MR_USE_ACTIVATION_COUNTS
  #define   MR_maybe_activation_count_field     0,
#else
  #define   MR_maybe_activation_count_field
#endif

#define MR_proc_static_no_site(global_var_name, file, line, interface)  \
    MR_ProcStatic global_var_name = {                                   \
        file,                                                           \
        line,                                                           \
        interface,                                                      \
        0,                                                              \
        NULL,                                                           \
        MR_maybe_activation_count_field                                 \
        NULL,                                                           \
        -1,                                                             \
        -1,                                                             \
        -1                                                              \
    }

#define MR_proc_static_one_site(global_var_name, call_sites_var_name,   \
        file, line, interface)                                          \
    MR_ProcStatic global_var_name = {                                   \
        file,                                                           \
        line,                                                           \
        interface,                                                      \
        1,                                                              \
        call_sites_var_name,                                            \
        MR_maybe_activation_count_field                                 \
        NULL,                                                           \
        -1,                                                             \
        -1,                                                             \
        -1                                                              \
    }

#define MR_proc_static_user_no_site(module, name, arity, mode,          \
        file, line, interface)                                          \
    MR_proc_static_no_site(                                             \
        MR_proc_static_user_name(module, name, arity, mode),            \
        file, line, interface)

#define MR_proc_static_user_one_site(module, name, arity, mode,         \
        file, line, interface)                                          \
    MR_proc_static_one_site(                                            \
        MR_proc_static_user_name(module, name, arity, mode),            \
        MR_call_sites_user_name(module, name, arity, mode),             \
        file, line, interface)

#define MR_proc_static_uci_no_site(module, name, type, arity, mode,     \
        file, line, interface)                                          \
    MR_proc_static_no_site(                                             \
        MR_proc_static_uci_name(module, name, type, arity, mode),       \
        file, line, interface)

#define MR_proc_static_uci_one_site(module, name, type, arity, mode,    \
        file, line, interface)                                          \
    MR_proc_static_one_site(                                            \
        MR_proc_static_uci_name(module, name, type, arity, mode),       \
        MR_call_sites_uci_name(module, name, type, arity, mode),        \
        file, line, interface)

////////////////////////////////////////////////////////////////////////////

#define MR_call_sites_uci_one_normal(module, name, type, arity, mode,   \
        cmodule, cname, carity, cmode, line)                            \
    static const MR_CallSiteStatic                                      \
    MR_call_sites_uci_name(module, name, type, arity, mode)[] = {       \
        { MR_callsite_normal_call, (MR_ProcLayout *)                    \
        &MR_proc_layout_user_name(cmodule, cname, carity, cmode),       \
        NULL, "", line, "" }                                            \
    }

#define MR_call_sites_user_one_ho(module, name, arity, mode, line)      \
    static const MR_CallSiteStatic                                      \
    MR_call_sites_user_name(module, name, arity, mode)[] = {            \
        { MR_callsite_higher_order_call, NULL,                          \
        NULL, "", line, "" }                                            \
    }

////////////////////////////////////////////////////////////////////////////

#define MR_deep_det_call_ac(proclabel, procstatic, first_slot, label)   \
    MR_r1 = (MR_Word) (MR_Word *) &procstatic;                          \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__det_call_port_code_ac_3_0),         \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel));                   \
    MR_stackvar(first_slot) = MR_r1;    /* TopCSD */                    \
    MR_stackvar(first_slot+1) = MR_r2;  /* MiddleCSD */                 \
    (void) 0

#define MR_deep_det_exit_ac(proclabel, first_slot, label)               \
    MR_r1 = MR_stackvar(first_slot);    /* TopCSD */                    \
    MR_r2 = MR_stackvar(first_slot+1);  /* MiddleCSD */                 \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__det_exit_port_code_ac_2_0),         \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel))

////////////////////////////////////////////////////////////////////////////

#define MR_deep_det_call_sr(proclabel, procstatic, first_slot, label)   \
    MR_r1 = (MR_Word) (MR_Word *) &procstatic;                          \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__det_call_port_code_sr_4_0),         \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel));                   \
    MR_stackvar(first_slot) = MR_r1;    /* TopCSD */                    \
    MR_stackvar(first_slot+1) = MR_r2;  /* MiddleCSD */                 \
    MR_stackvar(first_slot+2) = MR_r3;  /* OldActivationPtr */          \
    (void) 0

#define MR_deep_det_exit_sr(proclabel, first_slot, label)               \
    MR_r1 = MR_stackvar(first_slot);    /* TopCSD */                    \
    MR_r2 = MR_stackvar(first_slot+1);  /* MiddleCSD */                 \
    MR_r3 = MR_stackvar(first_slot+2);  /* OldActivationPtr */          \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__det_exit_port_code_sr_3_0),         \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel))

////////////////////////////////////////////////////////////////////////////

#define MR_deep_semi_call_ac(proclabel, procstatic, first_slot, label)  \
    MR_r1 = (MR_Word) (MR_Word *) &procstatic;                          \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__semi_call_port_code_ac_3_0),        \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel));                   \
    MR_stackvar(first_slot) = MR_r1;    /* TopCSD */                    \
    MR_stackvar(first_slot+1) = MR_r2;  /* MiddleCSD */                 \
    (void) 0

#define MR_deep_semi_exit_ac(proclabel, first_slot, label)              \
    MR_r1 = MR_stackvar(first_slot);    /* TopCSD */                    \
    MR_r2 = MR_stackvar(first_slot+1);  /* MiddleCSD */                 \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__semi_exit_port_code_ac_2_0),        \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel))

#define MR_deep_semi_fail_ac(proclabel, first_slot, label)              \
    MR_r1 = MR_stackvar(first_slot);    /* TopCSD */                    \
    MR_r2 = MR_stackvar(first_slot+1);  /* MiddleCSD */                 \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__semi_fail_port_code_ac_2_0),        \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel))

////////////////////////////////////////////////////////////////////////////

#define MR_deep_semi_call_sr(proclabel, procstatic, first_slot, label)  \
    MR_r1 = (MR_Word) (MR_Word *) &procstatic;                          \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__semi_call_port_code_sr_4_0),        \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel));                   \
    MR_stackvar(first_slot) = MR_r1;    /* TopCSD */                    \
    MR_stackvar(first_slot+1) = MR_r2;  /* MiddleCSD */                 \
    MR_stackvar(first_slot+2) = MR_r3;  /* OldActivationPtr */          \
    (void) 0

#define MR_deep_semi_exit_sr(proclabel, first_slot, label)              \
    MR_r1 = MR_stackvar(first_slot);    /* TopCSD */                    \
    MR_r2 = MR_stackvar(first_slot+1);  /* MiddleCSD */                 \
    MR_r3 = MR_stackvar(first_slot+2);  /* OldActivationPtr */          \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__semi_exit_port_code_sr_3_0),        \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel))

#define MR_deep_semi_fail_sr(proclabel, first_slot, label)              \
    MR_r1 = MR_stackvar(first_slot);    /* TopCSD */                    \
    MR_r2 = MR_stackvar(first_slot+1);  /* MiddleCSD */                 \
    MR_r3 = MR_stackvar(first_slot+2);  /* OldActivationPtr */          \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__semi_fail_port_code_sr_3_0),        \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel))

////////////////////////////////////////////////////////////////////////////

#define MR_deep_non_call_ac(proclabel, procstatic, first_slot, label)   \
    MR_r1 = (MR_Word) (MR_Word *) &procstatic;                          \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__non_call_port_code_ac_4_0),         \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel));                   \
    MR_framevar(first_slot) = MR_r1;    /* TopCSD */                    \
    MR_framevar(first_slot+1) = MR_r2;  /* MiddleCSD */                 \
    MR_framevar(first_slot+3) = MR_r3;  /* NewActivationPtr */          \
    (void) 0

#define MR_deep_non_exit_ac(proclabel, first_slot, label)               \
    MR_r1 = MR_framevar(first_slot);    /* TopCSD */                    \
    MR_r2 = MR_framevar(first_slot+1);  /* MiddleCSD */                 \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__non_exit_port_code_ac_2_0),         \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel))

#define MR_deep_non_redo_ac(proclabel, first_slot, label)               \
    MR_r1 = MR_framevar(first_slot+1);  /* MiddleCSD */                 \
    MR_r2 = MR_framevar(first_slot+2);  /* NewActivationPtr */          \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__non_redo_port_code_ac_2_0),         \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel))

#define MR_deep_non_fail_ac(proclabel, first_slot, label)               \
    MR_r1 = MR_framevar(first_slot);    /* TopCSD */                    \
    MR_r2 = MR_framevar(first_slot+1);  /* MiddleCSD */                 \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__non_fail_port_code_ac_2_0),         \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel))

////////////////////////////////////////////////////////////////////////////

#define MR_deep_non_call_sr(proclabel, procstatic, first_slot, label)   \
    MR_r1 = (MR_Word) (MR_Word *) &procstatic;                          \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__non_call_port_code_sr_5_0),         \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel));                   \
    MR_framevar(first_slot) = MR_r1;    /* TopCSD */                    \
    MR_framevar(first_slot+1) = MR_r2;  /* MiddleCSD */                 \
    MR_framevar(first_slot+2) = MR_r3;  /* OldActivationPtr */          \
    MR_framevar(first_slot+3) = MR_r4;  /* NewActivationPtr */          \
    (void) 0

#define MR_deep_non_exit_sr(proclabel, first_slot, label)               \
    MR_r1 = MR_framevar(first_slot);    /* TopCSD */                    \
    MR_r2 = MR_framevar(first_slot+1);  /* MiddleCSD */                 \
    MR_r3 = MR_framevar(first_slot+2);  /* OldActivationPtr */          \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__non_exit_port_code_sr_3_0),         \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel))

#define MR_deep_non_redo_sr(proclabel, first_slot, label)               \
    MR_r1 = MR_framevar(first_slot+1);  /* MiddleCSD */                 \
    MR_r2 = MR_framevar(first_slot+3);  /* NewActivationPtr */          \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__non_redo_port_code_sr_2_0),         \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel))

#define MR_deep_non_fail_sr(proclabel, first_slot, label)               \
    MR_r1 = MR_framevar(first_slot);    /* TopCSD */                    \
    MR_r2 = MR_framevar(first_slot+1);  /* MiddleCSD */                 \
    MR_r3 = MR_framevar(first_slot+2);  /* OldActivationPtr */          \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__non_fail_port_code_sr_3_0),         \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel))

////////////////////////////////////////////////////////////////////////////

#ifdef  MR_USE_ACTIVATION_COUNTS
  #define   MR_deep_det_call(proclabel, proclayout, first_slot, label)  \
        MR_deep_det_call_ac(proclabel, proclayout, first_slot, label)
  #define   MR_deep_det_exit(proclabel, first_slot, label)              \
        MR_deep_det_exit_ac(proclabel, first_slot, label)

  #define   MR_deep_semi_call(proclabel, proclayout, first_slot, label) \
        MR_deep_semi_call_ac(proclabel, proclayout, first_slot, label)
  #define   MR_deep_semi_exit(proclabel, first_slot, label)             \
        MR_deep_semi_exit_ac(proclabel, first_slot, label)
  #define   MR_deep_semi_fail(proclabel, first_slot, label)             \
        MR_deep_semi_fail_ac(proclabel, first_slot, label)

  #define   MR_deep_non_call(proclabel, proclayout, first_slot, label)  \
        MR_deep_non_call_ac(proclabel, proclayout, first_slot, label)
  #define   MR_deep_non_exit(proclabel, first_slot, label)              \
        MR_deep_non_exit_ac(proclabel, first_slot, label)
  #define   MR_deep_non_redo(proclabel, first_slot, label)              \
        MR_deep_non_redo_ac(proclabel, first_slot, label)
  #define   MR_deep_non_fail(proclabel, first_slot, label)              \
        MR_deep_non_fail_ac(proclabel, first_slot, label)
#else
  #define   MR_deep_det_call(proclabel, proclayout, first_slot, label)  \
        MR_deep_det_call_sr(proclabel, proclayout, first_slot, label)
  #define   MR_deep_det_exit(proclabel, first_slot, label)              \
        MR_deep_det_exit_sr(proclabel, first_slot, label)

  #define   MR_deep_semi_call(proclabel, proclayout, first_slot, label) \
        MR_deep_semi_call_sr(proclabel, proclayout, first_slot, label)
  #define   MR_deep_semi_exit(proclabel, first_slot, label)             \
        MR_deep_semi_exit_sr(proclabel, first_slot, label)
  #define   MR_deep_semi_fail(proclabel, first_slot, label)             \
        MR_deep_semi_fail_sr(proclabel, first_slot, label)

  #define   MR_deep_non_call(proclabel, proclayout, first_slot, label)  \
        MR_deep_non_call_sr(proclabel, proclayout, first_slot, label)
  #define   MR_deep_non_exit(proclabel, first_slot, label)              \
        MR_deep_non_exit_sr(proclabel, first_slot, label)
  #define   MR_deep_non_redo(proclabel, first_slot, label)              \
        MR_deep_non_redo_sr(proclabel, first_slot, label)
  #define   MR_deep_non_fail(proclabel, first_slot, label)              \
        MR_deep_non_fail_sr(proclabel, first_slot, label)
#endif

////////////////////////////////////////////////////////////////////////////

// MR_deep_prepare_normal_call and MR_deep_prepare_ho_call are for use
// only from procedures that live on the det stack. For procedures that live
// on the nondet stack, you will need variants that get MiddleCSD from the
// appropriate framevar.

#define MR_deep_prepare_normal_call(proclabel, first_slot, label, site) \
    MR_r1 = site;                                                       \
    MR_call_localret(MR_ENTRY(                                          \
        mercury__profiling_builtin__prepare_for_normal_call_1_0),       \
        label, MR_ENTRY(proclabel));                                    \
    MR_define_label(label);                                             \
    MR_update_prof_current_proc(MR_LABEL(proclabel))

#define MR_deep_prepare_ho_call(proclabel, first_slot, label, site, closure) \
    MR_r1 = site;                                                            \
    MR_r2 = closure;                                                         \
    MR_call_localret(MR_ENTRY(                                               \
        mercury__profiling_builtin__prepare_for_ho_call_2_0),                \
        label, MR_ENTRY(proclabel));                                         \
    MR_define_label(label);                                                  \
    MR_update_prof_current_proc(MR_LABEL(proclabel))

#endif  // MERCURY_DEEP_PROFILING_HAND_H
