// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1995-2003, 2006, 2010 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_debug.h - definitions for debugging messages

#ifndef MERCURY_DEBUG_H
#define MERCURY_DEBUG_H

#include "mercury_types.h"          // for MR_Word and MR_Code
#include "mercury_type_info.h"      // for MR_TypeInfo
#include "mercury_memory_zones.h"   // for MR_MemoryZone
#include <stdio.h>                  // for FILE

////////////////////////////////////////////////////////////////////////////

#ifdef MR_DEBUG_ON
    #define MR_DEBUG(action)        action
#else
    #define MR_DEBUG(action)
#endif

#if !defined(MR_DEBUG_GOTOS)

#define MR_debuggoto(label)         ((void) 0)
#define MR_debugsreg()              ((void) 0)

#else

#define MR_debuggoto(label)                                             \
    MR_IF (MR_gotodebug,                                                \
        (MR_save_transient_registers(), MR_goto_msg(stdout, label)))

#define MR_debugsreg()                                                  \
    MR_IF (MR_sregdebug,                                                \
        (MR_save_transient_registers(), MR_reg_msg(stdout)))

#endif

#ifndef MR_DEBUG_HEAP_ALLOC

#define MR_debug_unravel_univ(univ, typeinfo, value)        ((void) 0)
#define MR_debug_new_univ_on_hp(univ, typeinfo, value)      ((void) 0)
#define MR_debug_tag_offset_incr_hp_base(ptr, tag, offset, count, is_atomic) \
                                ((void) 0)

#else

#define MR_debug_unravel_univ(univ, typeinfo, value)                    \
     MR_unravel_univ_msg((univ), (typeinfo), (value))

#define MR_debug_new_univ_on_hp(univ, typeinfo, value)                  \
     MR_new_univ_on_hp_msg((univ), (typeinfo), (value))

#define MR_debug_tag_offset_incr_hp_base(ptr, tag, offset, count, is_atomic) \
     MR_debug_tag_offset_incr_hp_base_msg((ptr), (tag),                      \
         (offset), (count), (is_atomic))

#endif

#ifndef MR_LOWLEVEL_DEBUG

#define MR_debugcr1(hp)                         ((void) 0)
#define MR_debugcr2(hp)                         ((void) 0)
#define MR_debugcr3(hp)                         ((void) 0)
#define MR_debugincrhp(val, hp)                 ((void) 0)
#define MR_debugincrsp(val, sp)                 ((void) 0)
#define MR_debugdecrsp(val, sp)                 ((void) 0)
#define MR_debugregs(msg)                       ((void) 0)
#define MR_debugframe(msg)                      ((void) 0)
#define MR_debugmkframe(predname)               ((void) 0)
#define MR_debugmktempframe()                   ((void) 0)
#define MR_debugmkdettempframe()                ((void) 0)
#define MR_debugsucceed()                       ((void) 0)
#define MR_debugsucceeddiscard()                ((void) 0)
#define MR_debugfail()                          ((void) 0)
#define MR_debugredo()                          ((void) 0)
#define MR_debugcall(proc, succ_cont)           ((void) 0)
#define MR_debugtailcall(proc)                  ((void) 0)
#define MR_debugproceed()                       ((void) 0)
#define MR_debugmsg0(msg)                       ((void) 0)
#define MR_debugmsg1(msg, arg1)                 ((void) 0)
#define MR_debugmsg2(msg, arg1, arg2)           ((void) 0)
#define MR_debugmsg3(msg, arg1, arg2, arg3)     ((void) 0)

#else

#define MR_debugcr1(hp)                                                 \
    MR_IF (MR_heapdebug,                                                \
        (MR_save_transient_registers(),                                 \
         MR_cr1_msg(stdout, hp)))

#define MR_debugcr2(hp)                                                 \
    MR_IF (MR_heapdebug,                                                \
        (MR_save_transient_registers(),                                 \
         MR_cr2_msg(stdout, hp)))

#define MR_debugcr3(hp)                                                 \
    MR_IF (MR_heapdebug,                                                \
        (MR_save_transient_registers(),                                 \
         MR_cr3_msg(stdout, hp)))

#define MR_debugincrhp(val, hp)                                         \
    MR_IF (MR_heapdebug,                                                \
        (MR_save_transient_registers(),                                 \
         MR_incr_hp_debug_msg(stdout, (val), (hp))))

#define MR_debugincrsp(val, sp)                                         \
    MR_IF (MR_detstackdebug,                                            \
        (MR_save_transient_registers(),                                 \
         MR_incr_sp_msg(stdout, (val), (sp))))

#define MR_debugdecrsp(val, sp)                                         \
    MR_IF (MR_detstackdebug,                                            \
        (MR_save_transient_registers(),                                 \
         MR_decr_sp_msg(stdout, (val), (sp))))

#define MR_debugregs(msg)                                               \
    MR_IF (MR_progdebug,                                                \
        (MR_save_transient_registers(),                                 \
         MR_printregs(stdout, msg)))

#define MR_debugframe(msg)                                              \
    MR_IF (MR_progdebug,                                                \
        (MR_save_transient_registers(),                                 \
         MR_printframe(stdout, msg)))

#define MR_debugmkframe(predname)                                       \
    MR_IF (MR_nondetstackdebug,                                         \
        (MR_save_transient_registers(),                                 \
         MR_mkframe_msg(stdout, predname)))

#define MR_debugmktempframe()                                           \
    MR_IF (MR_nondetstackdebug,                                         \
        (MR_save_transient_registers(),                                 \
         MR_mktempframe_msg(stdout)))

#define MR_debugmkdettempframe()                                        \
    MR_IF (MR_nondetstackdebug,                                         \
        (MR_save_transient_registers(),                                 \
         MR_mkdettempframe_msg(stdout)))

#define MR_debugsucceed()                                               \
    MR_IF (MR_calldebug,                                                \
        (MR_save_transient_registers(),                                 \
         MR_succeed_msg(stdout)))

#define MR_debugsucceeddiscard()                                        \
    MR_IF (MR_calldebug,                                                \
        (MR_save_transient_registers(),                                 \
         MR_succeeddiscard_msg(stdout)))

#define MR_debugfail()                                                  \
    MR_IF (MR_calldebug,                                                \
        (MR_save_transient_registers(),                                 \
         MR_fail_msg(stdout)))

#define MR_debugredo()                                                  \
    MR_IF (MR_calldebug,                                                \
        (MR_save_transient_registers(),                                 \
         MR_redo_msg(stdout)))

#define MR_debugcall(proc, succ_cont)                                   \
    MR_IF (MR_calldebug,                                                \
        (MR_save_transient_registers(),                                 \
         MR_call_msg(stdout, proc, succ_cont)))

#define MR_debugtailcall(proc)                                          \
    MR_IF (MR_calldebug,                                                \
        (MR_save_transient_registers(),                                 \
         MR_tailcall_msg(stdout, proc)))

#define MR_debugproceed()                                               \
    MR_IF (MR_calldebug,                                                \
        (MR_save_transient_registers(),                                 \
         MR_proceed_msg(stdout)))

#define MR_debugmsg0(msg)                                               \
    MR_IF (MR_progdebug, (fprintf(stdout, msg)))

#define MR_debugmsg1(msg, arg1)                                         \
    MR_IF (MR_progdebug, (fprintf(stdout, msg, arg1)))

#define MR_debugmsg2(msg, arg1, arg2)                                   \
    MR_IF (MR_progdebug, (fprintf(stdout, msg, arg1, arg2)))

#define MR_debugmsg3(msg, arg1, arg2, arg3)                             \
    MR_IF (MR_progdebug, (fprintf(stdout, msg, arg1, arg2, arg3)))

#endif // MR_LOWLEVEL_DEBUG

#define MR_print_deep_prof_vars(fp, msg)                                     \
    do {                                                                     \
        fprintf(fp, "%s\n", msg);                                            \
        MR_print_deep_prof_var(fp, "curcsd",  MR_current_call_site_dynamic); \
        MR_print_deep_prof_var(fp, "nextcsd", MR_next_call_site_dynamic);    \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#ifdef MR_DEBUG_HEAP_ALLOC
extern  void    MR_unravel_univ_msg(FILE *fp, MR_Word univ,
                    MR_TypeInfo type_info, MR_Word value);
extern  void    MR_new_univ_on_hp_msg(FILE *fp, MR_Word univ,
                    MR_TypeInfo type_info, MR_Word value);
extern  void    MR_debug_tag_offset_incr_hp_base_msg(FILE *fp, MR_Word ptr,
                    int tag, int offset, int count, int is_atomic);
#endif

#ifdef MR_LOWLEVEL_DEBUG
extern  void    MR_mkframe_msg(FILE *fp, const char *);
extern  void    MR_mktempframe_msg(FILE *fp);
extern  void    MR_mkdettempframe_msg(FILE *fp);
extern  void    MR_succeed_msg(FILE *fp);
extern  void    MR_succeeddiscard_msg(FILE *fp);
extern  void    MR_fail_msg(FILE *fp);
extern  void    MR_redo_msg(FILE *fp);
extern  void    MR_call_msg(FILE *fp, const MR_Code *proc,
                    const MR_Code *succ_cont);
extern  void    MR_tailcall_msg(FILE *fp, const MR_Code *proc);
extern  void    MR_proceed_msg(FILE *fp);
extern  void    MR_cr1_msg(FILE *fp, const MR_Word *addr);
extern  void    MR_cr2_msg(FILE *fp, const MR_Word *addr);
extern  void    MR_cr3_msg(FILE *fp, const MR_Word *addr);
extern  void    MR_incr_hp_debug_msg(FILE *fp, MR_Word val,
                    const MR_Word *addr);
extern  void    MR_incr_sp_msg(FILE *fp, MR_Word val, const MR_Word *addr);
extern  void    MR_decr_sp_msg(FILE *fp, MR_Word val, const MR_Word *addr);
#endif

#ifdef MR_DEBUG_GOTOS
extern  void    MR_goto_msg(FILE *fp, const MR_Code *addr);
extern  void    MR_reg_msg(FILE *fp);
#endif

#ifdef MR_LOWLEVEL_DEBUG
extern  void    MR_printint(FILE *fp, MR_Word n);
extern  void    MR_printstring(FILE *fp, const char *s);
extern  void    MR_printheap(FILE *fp, const MR_Word *h);
extern  void    MR_dumpframe(FILE *fp, const MR_Word *);
extern  void    MR_dumpnondetstack(FILE *fp);
extern  void    MR_printlist(FILE *fp, MR_Word p);
extern  void    MR_printframe(FILE *fp, const char *);
extern  void    MR_printregs(FILE *fp, const char *msg);
#endif

extern  void    MR_print_zone(FILE *fp, const MR_MemoryZone *zone);
extern  void    MR_print_zones(FILE *fp, const MR_MemoryZones *zones);

extern  void    MR_printdetstack(FILE *fp, const MR_Word *s);
extern  void    MR_print_detstackptr(FILE *fp, const MR_Word *s);
extern  void    MR_printnondetstack(FILE *fp, const MR_Word *s);
extern  void    MR_print_nondetstackptr(FILE *fp, const MR_Word *s);
extern  void    MR_print_heapptr(FILE *fp, const MR_Word *s);
extern  void    MR_print_label(FILE *fp, const MR_Code *w);
extern  void    MR_printlabel(FILE *fp, const MR_Code *w);
extern  void    MR_print_deep_prof_var(FILE *fp, const char *name,
                    MR_CallSiteDynamic *csd);

// Log a message for debugging purposes. This will log the message with
// threadscope if available. In other parallel grades it will print the
// address of the MercuryEngine structure with the message to stdout.
// In all other grades, it will print the message to standard output.
// There is never any need to put a newline character at the end
// of the message; this function does that automatically.

extern  void    MR_debug_log_message(const char *format, ...);

////////////////////////////////////////////////////////////////////////////

#endif // not MERCURY_DEBUG_H
