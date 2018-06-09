// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1995-2000, 2004-2005 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_calls.h - definitions for calls and returns.

#ifndef MERCURY_CALLS_H
#define MERCURY_CALLS_H

#include "mercury_regs.h"   // for MR_succip
#include "mercury_goto.h"   // calls are implemented using gotos
#include "mercury_debug.h"  // we need to debug them
#include "mercury_prof.h"   // we need to profile them

// The noprof (no profiling) versions of call, call_localret, localcall,
// tailcall, and localtailcall.

// On some systems [basically those using PIC (Position Independent Code)],
// if we are using gcc non-local gotos to jump between functions, then
// we need to do ASM_FIXUP_REGS after each return from a procedure call.
// However, if we are using asm labels, then this is done in the
// MR_define_label(), MR_define_static(), and MR_define_Entry() macros,
// so there is no need to do it here.
// Also if we are using native gc, then the fixup_gp label below
// would stuff up the succip values, so we can't do it.
//
// For the non-asm `jump' and `fast' grades, we do in theory need
// to do something like this. However, doing ASM_FIXUP_REGS only
// on returns is *not* sufficient to ensure correctness in general.
// So the non-asm `jump' and `fast' grades are in theory broken,
// with or without this code. In practice they happen to work
// (for our current set of test cases!) either way.
// So it is simpler (and more efficient) to just comment it out.

#if 0
  #define   MR_noprof_call(proc, succ_cont)                             \
        ({                                                              \
            __label__ fixup_gp;                                         \
            MR_debugcall((proc), (succ_cont));                          \
            MR_succip_word = (MR_Word) (&&fixup_gp);                    \
            MR_GOTO(proc);                                              \
        fixup_gp:                                                       \
            ASM_FIXUP_REGS                                              \
            MR_GOTO(succ_cont);                                         \
        })
        // same as above, but with MR_GOTO_LABEL rather than MR_GOTO
  #define   MR_noprof_call_localret(proc, succ_cont)                    \
        ({                                                              \
            __label__ fixup_gp;                                         \
            MR_debugcall((proc), (succ_cont));                          \
            MR_succip_word = (MR_Word) (&&fixup_gp);                    \
            MR_GOTO(proc);                                              \
        fixup_gp:                                                       \
            ASM_FIXUP_REGS                                              \
            MR_GOTO_LABEL(succ_cont);                                   \
        })
#else
  #define   MR_noprof_call(proc, succ_cont)                             \
        do {                                                            \
            MR_debugcall((proc), (succ_cont));                          \
            MR_succip_word = (MR_Word) (succ_cont);                     \
            MR_GOTO(proc);                                              \
        } while (0)
  #define   MR_noprof_call_localret(proc, succ_cont)                    \
        MR_noprof_call((proc), MR_LABEL(succ_cont))
#endif

#define MR_noprof_localcall(label, succ_cont)                           \
        do {                                                            \
            MR_debugcall(MR_LABEL(label), (succ_cont));                 \
            MR_succip_word = (MR_Word) (succ_cont);                     \
            MR_GOTO_LABEL(label);                                       \
        } while (0)

#define MR_noprof_tailcall(proc)                                        \
        do {                                                            \
            MR_debugtailcall(proc);                                     \
            MR_GOTO(proc);                                              \
        } while (0)

#define MR_noprof_localtailcall(label)                                  \
        do {                                                            \
            MR_debugtailcall(MR_LABEL(label));                          \
            MR_GOTO_LABEL(label);                                       \
        } while (0)

// The shorthand versions of the non-profiling versions of
// call_localret, localcall, tailcall, and localtailcall.
// To a first approximation, just plain call doesn't occur at all,
// so it isn't worth a shorthand form.

#define MR_np_call_localret(proc, succ_cont)                            \
    MR_noprof_call_localret(MR_add_prefix(proc), MR_add_prefix(succ_cont))

#define MR_np_call_localret_ent(proc, succ_cont)                        \
    MR_noprof_call_localret(MR_ENTRY_AP(proc), MR_add_prefix(succ_cont))

#define MR_np_localcall(label, succ_cont)                               \
    MR_noprof_localcall(MR_add_prefix(label), MR_add_prefix(succ_cont))

#define MR_np_localcall_ent(label, succ_cont)                           \
    MR_noprof_localcall(MR_add_prefix(label), MR_ENTRY_AP(succ_cont))

#define MR_np_localcall_lab(label, succ_cont)                           \
    MR_noprof_localcall(MR_add_prefix(label), MR_LABEL_AP(succ_cont))

#define MR_np_tailcall(proc)                                            \
    MR_noprof_tailcall(MR_add_prefix(proc))

#define MR_np_tailcall_ent(proc)                                        \
    MR_noprof_tailcall(MR_ENTRY_AP(proc))

#define MR_np_localtailcall(label)                                      \
    MR_noprof_localtailcall(MR_add_prefix(label))

// The plain (possibly profiling, depending on #defines) versions of
// call, call_localret, localcall, tailcall, and localtailcall.
// These take an extra argument, the current label.

#define MR_call(proc, succ_cont, current_label)                         \
        do {                                                            \
            MR_PROFILE((proc), (current_label));                        \
            MR_set_prof_current_proc(proc);                             \
            MR_noprof_call((proc), (succ_cont));                        \
        } while (0)

#define MR_call_localret(proc, succ_cont, current_label)                \
        do {                                                            \
            MR_PROFILE((proc), (current_label));                        \
            MR_set_prof_current_proc(proc);                             \
            MR_noprof_call_localret(proc, succ_cont);                   \
        } while (0)

#define MR_localcall(label, succ_cont, current_label)                   \
        do {                                                            \
            MR_PROFILE(MR_LABEL(label), (current_label));               \
            MR_set_prof_current_proc(MR_LABEL(label));                  \
            MR_noprof_localcall(label, succ_cont);                      \
        } while (0)

#define MR_tailcall(proc, current_label)                                \
        do {                                                            \
            MR_PROFILE((proc), (current_label));                        \
            MR_set_prof_current_proc(proc);                             \
            MR_noprof_tailcall(proc);                                   \
        } while (0)

#define MR_localtailcall(label, current_label)                          \
        do {                                                            \
            MR_PROFILE(MR_LABEL(label), (current_label));               \
            MR_set_prof_current_proc(MR_LABEL(label));                  \
            MR_noprof_localtailcall(label);                             \
        } while (0)

// The macros for returning from calls from procedures.

#define MR_proceed()                                                    \
        do {                                                            \
            MR_debugproceed();                                          \
            MR_GOTO(MR_succip);                                         \
        } while (0)

#endif // not MERCURY_CALLS_H
