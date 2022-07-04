// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1996-2007, 2010 The University of Melbourne.
// Copyright (C) 2013-2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#include    "mercury_imp.h"
#include    "mercury_dlist.h"
#include    "mercury_regs.h"
#include    "mercury_trace_base.h"
#include    "mercury_label.h"
#include    "mercury_debug.h"

#include    <stdio.h>
#include    <stdarg.h>

static MR_bool  MR_find_zone_for_det_ptr_in_context(const MR_Word *ptr,
                    MR_Context *ctxt,
                    MR_MemoryZone **zone_ptr, int *zone_num_ptr);
static MR_bool  MR_find_zone_for_nondet_ptr_in_context(const MR_Word *ptr,
                    MR_Context *ctxt,
                    MR_MemoryZone **zone_ptr, int *zone_num_ptr);
static MR_bool  MR_find_zone_for_ptr(const MR_Word *ptr,
                    MR_MemoryZone *first_zone, MR_MemoryZones *later_zones,
                    MR_MemoryZone **zone_ptr, int *zone_num_ptr);
static MR_bool  MR_find_zone_for_det_ptr(const MR_Word *ptr,
                    MR_Context **ctxt_ptr,
                    MR_MemoryZone **zone_ptr, int *zone_num_ptr);
static MR_bool  MR_find_zone_for_nondet_ptr(const MR_Word *ptr,
                    MR_Context **ctxt_ptr,
                    MR_MemoryZone **zone_ptr, int *zone_num_ptr);

////////////////////////////////////////////////////////////////////////////

#ifdef  MR_DEEP_PROFILING
static void     MR_check_watch_csd_start(MR_Code *proc);
static MR_bool  MR_csds_are_different(const MR_CallSiteDynamic *csd1,
                    const MR_CallSiteDynamic *csd2);
static void     MR_assign_csd(MR_CallSiteDynamic *csd1,
                    const MR_CallSiteDynamic *csd2);
#endif

#ifdef MR_LOWLEVEL_DEBUG
static void     MR_count_call(FILE *fp, const MR_Code *proc);
static void     MR_print_ordinary_regs(FILE *fp);
static void     MR_do_watches(FILE *fp);
static MR_bool  MR_proc_matches_name(const MR_Code *proc, const char *name);
#endif

#ifdef  MR_LOWLEVEL_ADDR_DEBUG
  #define   MR_PRINT_RAW_ADDRS  MR_TRUE
#else
  #define   MR_PRINT_RAW_ADDRS  MR_FALSE
#endif

static  MR_bool MR_print_raw_addrs = MR_PRINT_RAW_ADDRS;

// Debugging messages.

#ifdef MR_DEBUG_HEAP_ALLOC

void
MR_unravel_univ_msg(FILE *fp, MR_Word univ, MR_TypeInfo type_info,
    MR_Word value)
{
    if (MR_lld_print_enabled && MR_heapdebug) {
        fprintf(fp, "unravel univ %p: typeinfo %p, value %p\n",
            (void *) univ, (void *) type_info, (void *) value);
        fflush(fp);
    }
}

void
MR_new_univ_on_hp_msg(FILE *fp, MR_Word univ, MR_TypeInfo type_info,
    MR_Word value)
{
    if (MR_lld_print_enabled && MR_heapdebug) {
        fprintf(fp"new univ on hp: typeinfo %p, value %p => %p\n",
            (void *) type_info, (void *) value, (void *) univ);
        fflush(fp);
    }
}

void
MR_debug_tag_offset_incr_hp_base_msg(FILE *fp, MR_Word ptr,
    int tag, int offset, int count, int is_atomic)
{
    if (MR_lld_print_enabled && MR_heapdebug) {
        fprintf(fp, "tag_offset_incr_hp: "
            "tag %d, offset %d, count %d%s => %p\n",
            tag, offset, count, (is_atomic ? ", atomic" : ""), (void *) ptr);
        fflush(fp);
    }
}

#endif // MR_DEBUG_HEAP_ALLOC

#ifdef MR_LOWLEVEL_DEBUG

void
MR_mkframe_msg(FILE *fp, const char *predname)
{
    MR_restore_transient_registers();

    if (!MR_lld_print_enabled) {
        return;
    }

    fprintf(fp, "\nnew choice point for procedure %s\n", predname);
    fprintf(fp, "new  fr: ");
    MR_printnondetstack(fp, MR_curfr);
    fprintf(fp, "\nprev fr: ");
    MR_printnondetstack(fp, MR_prevfr_slot(MR_curfr));
    fprintf(fp, "\nsucc fr: ");
    MR_printnondetstack(fp, MR_succfr_slot(MR_curfr));
    fprintf(fp, "\nsucc ip: ");
    MR_printlabel(fp, MR_succip_slot(MR_curfr));
    fprintf(fp, "redo fr: ");
    MR_printnondetstack(fp, MR_redofr_slot(MR_curfr));
    fprintf(fp, "\nredo ip: ");
    MR_printlabel(fp, MR_redoip_slot(MR_curfr));
#ifdef  MR_USE_MINIMAL_MODEL_OWN_STACKS
    fprintf(fp, "\ndet fr:  ");
    MR_printdetstack(fp, MR_table_detfr_slot(MR_curfr));
#endif
    fprintf(fp, "\n");

    if (MR_detaildebug) {
        MR_dumpnondetstack(fp);
    }

    fflush(fp);
}

void
MR_mktempframe_msg(FILE *fp)
{
    MR_restore_transient_registers();

    if (!MR_lld_print_enabled) {
        return;
    }

    fprintf(fp, "\nnew temp nondet frame");
    fprintf(fp, "\nnew  fr: ");
    MR_printnondetstack(fp, MR_maxfr);
    fprintf(fp, "\nprev fr: ");
    MR_printnondetstack(fp, MR_prevfr_slot(MR_maxfr));
    fprintf(fp, "\nredo fr: ");
    MR_printnondetstack(fp, MR_redofr_slot(MR_maxfr));
    fprintf(fp, "\nredo ip: ");
    MR_printlabel(fp, MR_redoip_slot(MR_maxfr));

    if (MR_detaildebug) {
        MR_dumpnondetstack(fp);
    }

    fflush(fp);
}

void
MR_mkdettempframe_msg(FILE *fp)
{
    MR_restore_transient_registers();

    if (!MR_lld_print_enabled) {
        return;
    }

    fprintf(fp, "\nnew det temp nondet frame");
    fprintf(fp, "\nnew  fr: ");
    MR_printnondetstack(fp, MR_maxfr);
    fprintf(fp, "\nprev fr: ");
    MR_printnondetstack(fp, MR_prevfr_slot(MR_maxfr));
    fprintf(fp, "\nredo fr: ");
    MR_printnondetstack(fp, MR_redofr_slot(MR_maxfr));
    fprintf(fp, "\nredo ip: ");
    MR_printlabel(fp, MR_redoip_slot(MR_maxfr));
    fprintf(fp, "det fr:  ");
    MR_printdetstack(fp, MR_tmp_detfr_slot(MR_maxfr));
    fprintf(fp, "\n");

    if (MR_detaildebug) {
        MR_dumpnondetstack(fp);
    }
}

void
MR_succeed_msg(FILE *fp)
{
    MR_restore_transient_registers();

    MR_do_watches(fp);

    if (!MR_lld_print_enabled) {
        return;
    }

    fprintf(fp, "\nsucceeding from procedure\n");
    fprintf(fp, "curr fr: ");
    MR_printnondetstack(fp, MR_curfr);
    fprintf(fp, "succ fr: ");
    MR_printnondetstack(fp, MR_succfr_slot(MR_curfr));
    fprintf(fp, "succ ip: ");
    MR_printlabel(fp, MR_succip_slot(MR_curfr));

    if (MR_detaildebug) {
        MR_printregs(fp, "registers at success");
    }
}

void
MR_succeeddiscard_msg(FILE *fp)
{
    MR_restore_transient_registers();

    MR_do_watches(fp);

    if (!MR_lld_print_enabled) {
        return;
    }

    fprintf(fp, "\nsucceeding from procedure\n");
    fprintf(fp, "curr fr: ");
    MR_printnondetstack(fp, MR_curfr);
    fprintf(fp, "succ fr: ");
    MR_printnondetstack(fp, MR_succfr_slot(MR_curfr));
    fprintf(fp, "succ ip: ");
    MR_printlabel(fp, MR_succip_slot(MR_curfr));

    if (MR_detaildebug) {
        MR_printregs(fp, "registers at success");
    }
}

void
MR_fail_msg(FILE *fp)
{
    MR_restore_transient_registers();

    MR_do_watches(fp);

    if (!MR_lld_print_enabled) {
        return;
    }

    fprintf(fp, "\nfailing from procedure\n");
    fprintf(fp, "curr fr: ");
    MR_printnondetstack(fp, MR_curfr);
    fprintf(fp, "\nfail fr: ");
    MR_printnondetstack(fp, MR_prevfr_slot(MR_curfr));
    fprintf(fp, "\nfail ip: ");
    MR_printlabel(fp, MR_redoip_slot(MR_prevfr_slot(MR_curfr)));
    fprintf(fp, "\n");
}

void
MR_redo_msg(FILE *fp)
{
    MR_restore_transient_registers();

    MR_do_watches(fp);

    if (!MR_lld_print_enabled) {
        return;
    }

    fprintf(fp, "\nredo from procedure");
    fprintf(fp, "\ncurr fr: ");
    MR_printnondetstack(fp, MR_curfr);
    fprintf(fp, "\nredo fr: ");
    MR_printnondetstack(fp, MR_maxfr);
    fprintf(fp, "\nredo ip: ");
    MR_printlabel(fp, MR_redoip_slot(MR_maxfr));
}

void
MR_call_msg(FILE *fp, const MR_Code *proc, const MR_Code *succ_cont)
{
    MR_count_call(fp, proc);

#ifdef  MR_DEEP_PROFILING
    MR_check_watch_csd_start(proc);
#endif  // MR_DEEP_PROFILING

    MR_do_watches(fp);

    if (!MR_lld_print_enabled) {
        return;
    }

    fprintf(fp, "\ncall %lu: ", MR_lld_cur_call);
    MR_printlabel(fp, proc);
    fprintf(fp, "cont ");
    MR_printlabel(fp, succ_cont);

    if (MR_anyregdebug) {
        MR_printregs(fp, "at call:");
    }

#ifdef  MR_DEEP_PROFILING
    MR_print_deep_prof_vars(fp, "MR_call_msg");
#endif
}

void
MR_tailcall_msg(FILE *fp, const MR_Code *proc)
{
    MR_restore_transient_registers();

    MR_count_call(fp, proc);

#ifdef  MR_DEEP_PROFILING
    MR_check_watch_csd_start(proc);
#endif  // MR_DEEP_PROFILING

    MR_do_watches(fp);

    if (!MR_lld_print_enabled) {
        return;
    }

    fprintf(fp, "\ntail call %lu: ", MR_lld_cur_call);
    MR_printlabel(fp, proc);
    fprintf(fp, "cont ");
    MR_printlabel(fp, MR_succip);

    if (MR_anyregdebug) {
        MR_printregs(fp, "at tailcall:");
    }

#ifdef  MR_DEEP_PROFILING
    MR_print_deep_prof_vars(fp, "MR_tailcall_msg");
#endif
}

void
MR_proceed_msg(FILE *fp)
{
    MR_do_watches(fp);

    if (!MR_lld_print_enabled) {
        return;
    }

    fprintf(fp, "\nreturning from determinate procedure\n");
    if (MR_anyregdebug) {
        MR_printregs(fp, "at proceed:");
    }

#ifdef  MR_DEEP_PROFILING
    MR_print_deep_prof_vars(fp, "MR_proceed_msg");
#endif
}

void
MR_cr1_msg(FILE *fp, const MR_Word *addr)
{
    if (!MR_lld_print_enabled) {
        return;
    }

#ifdef  MR_RECORD_TERM_SIZES
    fprintf(fp, "create1: put size %ld, value %9lx at ",
        (long) (MR_Integer) addr[-2],
        (long) (MR_Integer) addr[-1]);
#else
    fprintf(fp, "create1: put value %9lx at ",
        (long) (MR_Integer) addr[-1]);
#endif
    MR_printheap(fp, addr);
}

void
MR_cr2_msg(FILE *fp, const MR_Word *addr)
{
    if (!MR_lld_print_enabled) {
        return;
    }

#ifdef  MR_RECORD_TERM_SIZES
    fprintf(fp, "create2: put size %ld, values %9lx,%9lx at ",
        (long) (MR_Integer) addr[-3],
        (long) (MR_Integer) addr[-2],
        (long) (MR_Integer) addr[-1]);
#else
    fprintf(fp, "create2: put values %9lx,%9lx at ",
        (long) (MR_Integer) addr[-2],
        (long) (MR_Integer) addr[-1]);
#endif
    MR_printheap(fp, addr);
}

void
MR_cr3_msg(FILE *fp, const MR_Word *addr)
{
    if (!MR_lld_print_enabled) {
        return;
    }

#ifdef  MR_RECORD_TERM_SIZES
    fprintf(fp, "create3: put size %ld, values %9lx,%9lx,%9lx at ",
        (long) (MR_Integer) addr[-4],
        (long) (MR_Integer) addr[-3],
        (long) (MR_Integer) addr[-2],
        (long) (MR_Integer) addr[-1]);
#else
    fprintf(fp, "create3: put values %9lx,%9lx,%9lx at ",
        (long) (MR_Integer) addr[-3],
        (long) (MR_Integer) addr[-2],
        (long) (MR_Integer) addr[-1]);
#endif
    MR_printheap(fp, addr);
}

void
MR_incr_hp_debug_msg(FILE *fp, MR_Word val, const MR_Word *addr)
{
    if (!MR_lld_print_enabled) {
        return;
    }

#ifdef MR_CONSERVATIVE_GC
    fprintf(fp, "allocated %ld words at %p\n", (long) val, addr);
#else
    fprintf(fp, "increment hp by %ld from ", (long) (MR_Integer) val);
    MR_printheap(fp, addr);
#endif
}

void
MR_incr_sp_msg(FILE *fp, MR_Word val, const MR_Word *addr)
{
    if (!MR_lld_print_enabled) {
        return;
    }

    fprintf(fp, "increment sp by %ld from ", (long) (MR_Integer) val);
    MR_printdetstack(fp, addr);
}

void
MR_decr_sp_msg(FILE *fp, MR_Word val, const MR_Word *addr)
{
    if (!MR_lld_print_enabled) {
        return;
    }

    fprintf(fp, "decrement sp by %ld from ", (long) (MR_Integer) val);
    MR_printdetstack(fp, addr);
}

#endif // defined(MR_LOWLEVEL_DEBUG)

#ifdef MR_DEBUG_GOTOS

void
MR_goto_msg(FILE *fp, const MR_Code *addr)
{
    if (!MR_lld_print_enabled) {
        return;
    }

    if (addr == NULL) {
        fprintf(fp, "\ngoto NULL\n");
        MR_fatal_error("MR_goto_msg: NULL");
    }

    fprintf(fp, "\ngoto ");
    MR_printlabel(fp, addr);
}

void
MR_reg_msg(FILE *fp)
{
    int         i;
    MR_Integer  x;

    if (!MR_lld_print_enabled) {
        return;
    }

    for (i=1; i<=8; i++) {
        x = (MR_Integer) MR_get_reg(i);
#ifndef MR_CONSERVATIVE_GC
        if ((MR_Integer) MR_ENGINE(MR_eng_heap_zone)->MR_zone_min <= x
            && x < (MR_Integer) MR_ENGINE(MR_eng_heap_zone)->MR_zone_top)
        {
            x -= (MR_Integer) MR_ENGINE(MR_eng_heap_zone)->MR_zone_min;
        }
#endif
        fprintf(fp, "%8lx ", (long) x);
    }
    fprintf(fp, "\n");
}

#endif // defined(MR_DEBUG_GOTOS)

////////////////////////////////////////////////////////////////////////////

#ifdef MR_LOWLEVEL_DEBUG

// Debugging printing tools.

static void
MR_count_call(FILE *fp, const MR_Code *proc)
{
    MR_lld_cur_call++;
    if (!MR_lld_print_region_enabled) {
        if (MR_lld_cur_call == MR_lld_print_min) {
            MR_lld_print_region_enabled = MR_TRUE;
            fprintf(fp, "entering printed region\n");
            fprintf(fp, "min %lu, max %lu, more <%s>\n",
                MR_lld_print_min, MR_lld_print_max,
                MR_lld_print_more_min_max);
        }
    } else {
        if (MR_lld_cur_call == MR_lld_print_max) {
            MR_lld_print_region_enabled = MR_FALSE;
            MR_setup_call_intervals(&MR_lld_print_more_min_max,
                &MR_lld_print_min, &MR_lld_print_max);
            fprintf(fp, "leaving printed region\n");
            fprintf(fp, "min %lu, max %lu, more <%s>\n",
                MR_lld_print_min, MR_lld_print_max,
                MR_lld_print_more_min_max);
        }
    }

    if (MR_proc_matches_name(proc, MR_lld_start_name)) {
        MR_lld_print_name_enabled = MR_TRUE;
        MR_lld_start_until = MR_lld_cur_call + MR_lld_start_block;
        fprintf(fp, "entering printed name block %s\n", MR_lld_start_name);
    } else if (MR_lld_cur_call == MR_lld_start_until) {
        MR_lld_print_name_enabled = MR_FALSE;
        fprintf(fp, "leaving printed name block\n");
    }

#ifdef  MR_DEEP_PROFILING
    if (MR_watch_csd_addr == MR_next_call_site_dynamic
        && MR_watch_csd_addr != NULL)
    {
        MR_lld_print_csd_enabled = MR_TRUE;
        MR_lld_csd_until = MR_lld_cur_call + MR_lld_start_block;
        MR_watch_csd_started = MR_TRUE;
        fprintf(fp, "entering printed csd block %p\n", MR_watch_csd_addr);
    } else if (MR_lld_cur_call == MR_lld_csd_until) {
        MR_lld_print_csd_enabled = MR_FALSE;
        fprintf(fp, "leaving printed csd block\n");
    }
#endif

    // The bitwise ORs implement logical OR.
    MR_lld_print_enabled = MR_lld_print_region_enabled
        | MR_lld_print_name_enabled | MR_lld_print_csd_enabled
        | MR_lld_debug_enabled | MR_lld_print_always_enabled;
}

void
MR_printint(FILE *fp, MR_Word n)
{
    fprintf(fp, "int %ld\n", (long) (MR_Integer) n);
}

void
MR_printstring(FILE *fp, const char *s)
{
    if (MR_print_raw_addrs) {
        fprintf(fp, "string %p %s\n", (const void *) s, s);
    } else {
        fprintf(fp, "string %s\n", s);
    }
}

void
MR_printheap(FILE *fp, const MR_Word *h)
{
#ifndef MR_CONSERVATIVE_GC
    if (MR_print_raw_addrs) {
        fprintf(fp, "ptr %p, ", (const void *) h);
    }

    fprintf(fp, "offset %6ld words\n",
        (long) (MR_Integer) (h - MR_ENGINE(MR_eng_heap_zone)->min));
#else
    fprintf(fp, "ptr %p\n", (const void *) h);
#endif
}

void
MR_dumpframe(FILE *fp, const MR_Word *fr)
{
    int i;

    fprintf(fp, "frame at ");
    MR_printnondetstack(fp, fr),
    fprintf(fp, "\n");
    fprintf(fp, "\t succip    ");
    MR_printlabel(fp, MR_succip_slot(fr));
    fprintf(fp, "\t redoip    ");
    MR_printlabel(fp, MR_redoip_slot(fr));
    fprintf(fp, "\t succfr    ");
    MR_printnondetstack(fp, MR_succfr_slot(fr));
    fprintf(fp, "\t prevfr    ");
    MR_printnondetstack(fp, MR_prevfr_slot(fr));

    for (i = 1; &MR_based_framevar(fr,i) > MR_prevfr_slot(fr); i++) {
        fprintf(fp, "\t framevar(%d)  %ld 0x%lx\n",
            i, (long) (MR_Integer) MR_based_framevar(fr,i),
            (unsigned long) MR_based_framevar(fr,i));
    }
}

void
MR_dumpnondetstack(FILE *fp)
{
    MR_Word *fr;

    fprintf(fp, "\nnondetstack dump\n");
    for (fr = MR_maxfr; MR_above_bottom_nondet_frame(fr);
        fr = MR_prevfr_slot(fr))
    {
        MR_dumpframe(fp, fr);
    }
}

void
MR_printframe(FILE *fp, const char *msg)
{
    fprintf(fp, "\n%s\n", msg);
    MR_dumpframe(fp, MR_curfr);

    MR_print_ordinary_regs(fp);
}

void
MR_printregs(FILE *fp, const char *msg)
{
    MR_restore_transient_registers();

    fprintf(fp, "\n%s\n", msg);

    if (MR_sregdebug) {
        fprintf(fp, "%-9s", "succip:");
        MR_printlabel(fp, MR_succip);
        fprintf(fp, "%-9s", "curfr:");
        MR_printnondetstack(fp, MR_curfr);
        fprintf(fp, "%-9s", "maxfr:");
        MR_printnondetstack(fp, MR_maxfr);
        fprintf(fp, "%-9s", "hp:");
        MR_printheap(fp, MR_hp);
        fprintf(fp, "%-9s", "sp:");
        MR_printdetstack(fp, MR_sp);
    }

    if (MR_ordregdebug) {
        MR_print_ordinary_regs(fp);
    }
}

static void
MR_print_ordinary_regs(FILE *fp)
{
    int         i;
    MR_Integer  value;

    for (i = 0; i < 8; i++) {
        fprintf(fp, "r%d:      ", i + 1);
        value = (MR_Integer) MR_get_reg(i+1);

#ifndef MR_CONSERVATIVE_GC
        if ((MR_Integer) MR_ENGINE(MR_eng_heap_zone)->min <= value
            && value < (MR_Integer) MR_ENGINE(MR_eng_heap_zone)->top)
        {
            fprintf(fp, "(heap) ");
        }
#endif

        fprintf(fp, "%ld %lx\n", (long) value, (long) value);
    }
}

////////////////////////////////////////////////////////////////////////////

#ifdef  MR_DEEP_PROFILING

static struct MR_CallSiteDynamic_Struct MR_watched_csd_last_value =
{
    /* MR_csd_callee_ptr */     NULL,
    {
  #ifdef MR_DEEP_PROFILING_PORT_COUNTS
    #ifdef MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS
    /* MR_own_calls */          0,
    #else
    // Calls are computed from the other fields.
    #endif
    /* MR_own_exits */          0,
    /* MR_own_fails */          0,
    /* MR_own_redos */          0,
  #endif
  #ifdef MR_DEEP_PROFILING_TIMING
    /* MR_own_quanta */         0,
  #endif
  #ifdef MR_DEEP_PROFILING_CALL_SEQ
    /* MR_own_call_seqs */      0,
  #endif
  #ifdef MR_DEEP_PROFILING_MEMORY
    /* MR_own_allocs */         0,
    /* MR_own_words */          0,
  #endif
    },
    /* MR_csd_depth_count */    0
};

static void
MR_check_watch_csd_start(MR_Code *proc)
{
#if 0
    if (MR_watch_csd_start_name == NULL) {
        return;
    }

    if (MR_proc_matches_name(proc, MR_watch_csd_start_name)) {
        if (MR_watch_csd_addr == MR_next_call_site_dynamic) {
            // Optimize future checks and make MR_watch_csd_addr static.

            MR_watch_csd_started = MR_TRUE;
            MR_watch_csd_start_name = NULL;
        }
    }
#endif
}

static MR_bool
MR_csds_are_different(const MR_CallSiteDynamic *csd1,
    const MR_CallSiteDynamic *csd2)
{
    const MR_ProfilingMetrics *pm1;
    const MR_ProfilingMetrics *pm2;

    if (csd1->MR_csd_callee_ptr != csd2->MR_csd_callee_ptr) {
        return MR_TRUE;
    }

    pm1 = &csd1->MR_csd_own;
    pm2 = &csd2->MR_csd_own;

  #ifdef MR_DEEP_PROFILING_PORT_COUNTS
    #ifdef MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS
    if (pm1->MR_own_calls != pm2->MR_own_calls) {
        return MR_TRUE;
    }
    #endif
    if (pm1->MR_own_exits != pm2->MR_own_exits) {
        return MR_TRUE;
    }
    if (pm1->MR_own_fails != pm2->MR_own_fails) {
        return MR_TRUE;
    }
    if (pm1->MR_own_redos != pm2->MR_own_redos) {
        return MR_TRUE;
    }
  #endif
  #ifdef MR_DEEP_PROFILING_TIMING
    if (pm1->MR_own_quanta != pm2->MR_own_quanta) {
        return MR_TRUE;
    }
  #endif
  #ifdef MR_DEEP_PROFILING_CALL_SEQ
    if (pm1->MR_own_call_seqs != pm2->MR_own_call_seqs) {
        return MR_TRUE;
    }
  #endif
  #ifdef MR_DEEP_PROFILING_MEMORY
    if (pm1->MR_own_allocs != pm2->MR_own_allocs) {
        return MR_TRUE;
    }
    if (pm1->MR_own_words != pm2->MR_own_words) {
        return MR_TRUE;
    }
  #endif

    if (csd1->MR_csd_depth_count != csd2->MR_csd_depth_count) {
        return MR_TRUE;
    }

    return MR_FALSE;
};

static void
MR_assign_csd(MR_CallSiteDynamic *csd1, const MR_CallSiteDynamic *csd2)
{
    csd1->MR_csd_callee_ptr = csd2->MR_csd_callee_ptr;

  #ifdef MR_DEEP_PROFILING_PORT_COUNTS
    #ifdef MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS
    csd1->MR_csd_own.MR_own_calls = csd2->MR_csd_own.MR_own_calls;
    #endif
    csd1->MR_csd_own.MR_own_exits = csd2->MR_csd_own.MR_own_exits;
    csd1->MR_csd_own.MR_own_fails = csd2->MR_csd_own.MR_own_fails;
    csd1->MR_csd_own.MR_own_redos = csd2->MR_csd_own.MR_own_redos;
  #endif
  #ifdef MR_DEEP_PROFILING_TIMING
    csd1->MR_csd_own.MR_own_quanta = csd2->MR_csd_own.MR_own_quanta;
  #endif
  #ifdef MR_DEEP_PROFILING_CALL_SEQ
    csd1->MR_csd_own.MR_own_call_seqs = csd2->MR_csd_own.MR_own_call_seqs;
  #endif
  #ifdef MR_DEEP_PROFILING_MEMORY
    csd1->MR_csd_own.MR_own_allocs = csd2->MR_csd_own.MR_own_allocs;
    csd1->MR_csd_own.MR_own_words = csd2->MR_csd_own.MR_own_words;
  #endif

    csd1->MR_csd_depth_count = csd2->MR_csd_depth_count;
};

#endif  // MR_DEEP_PROFILING

////////////////////////////////////////////////////////////////////////////

static void
MR_do_watches(FILE *fp)
{
    if (MR_watch_addr != NULL) {
        fprintf(fp, "watch addr %p: 0x%lx %ld\n", MR_watch_addr,
            (long) *MR_watch_addr, (long) *MR_watch_addr);
    }

#ifdef  MR_DEEP_PROFILING
    if (MR_watch_csd_addr != NULL) {
        if (MR_watch_csd_started) {
            if (MR_csds_are_different(&MR_watched_csd_last_value,
                MR_watch_csd_addr))
            {
                MR_assign_csd(&MR_watched_csd_last_value, MR_watch_csd_addr);
                fprintf(fp, "current call: %lu\n", MR_lld_cur_call);
                MR_print_deep_prof_var(fp, "watch_csd", MR_watch_csd_addr);
            }
        }
    }
#endif  // MR_DEEP_PROFILING
}

static MR_bool
MR_proc_matches_name(const MR_Code *proc, const char *name)
{
#ifdef  MR_NEED_ENTRY_LABEL_ARRAY
    MR_Entry    *entry;

    entry = MR_prev_entry_by_addr(proc);
    if (entry != NULL && entry->MR_entry_addr == proc
            && entry->MR_entry_name != NULL)
    {
        if (MR_streq(entry->MR_entry_name, name)) {
            return MR_TRUE;
        }
    }

#endif  // MR_NEED_ENTRY_LABEL_ARRAY
    return MR_FALSE;
}

#endif // defined(MR_DEBUG_GOTOS)

#ifndef MR_HIGHLEVEL_CODE

void
MR_print_detstackptr(FILE *fp, const MR_Word *s)
{
    MR_MemoryZone   *zone;
    int             zone_num;

    if (MR_find_zone_for_det_ptr(s, NULL, &zone, &zone_num)) {
        if (zone_num == 0) {
            fprintf(fp, "det %3ld",
                (long) (MR_Integer) (s - zone->MR_zone_min));
        } else {
            fprintf(fp, "det %3ld, segment %d",
                (long) (MR_Integer) (s - zone->MR_zone_min), zone_num);
        }

        if (MR_print_raw_addrs) {
            fprintf(fp, " (%p)", (const void *) s);
        }
    } else {
        fprintf(fp, "det raw %p", (const void *) s);
    }
}

void
MR_print_zone(FILE *fp, const MR_MemoryZone *zone)
{
    fprintf(fp, "zone %p:\n", zone);
    fprintf(fp, "  bottom %p, top %p\n",
        zone->MR_zone_bottom, zone->MR_zone_top);
    fprintf(fp, "  min    %p, max %p",
        zone->MR_zone_min, zone->MR_zone_max);
#if defined(MR_PROTECTPAGE)
    fprintf(fp, ", hardmax %p", zone->MR_zone_hardmax);
#endif
#if defined(MR_STACK_SEGMENTS) && !defined(MR_HIGHLEVEL_CODE)
    fprintf(fp, ", extend %p",
        zone->MR_zone_extend_threshold);
#endif
    fprintf(fp, "\n");
}

void
MR_print_zones(FILE *fp, const MR_MemoryZones *zones)
{
    while (zones != NULL) {
        MR_print_zone(fp, zones->MR_zones_head);
        zones = zones->MR_zones_tail;
    }
}

void
MR_printdetstack(FILE *fp, const MR_Word *s)
{
    MR_MemoryZone   *zone;
    int             zone_num;

    if (MR_find_zone_for_det_ptr(s, NULL, &zone, &zone_num)) {
        if (MR_print_raw_addrs) {
            fprintf(fp, "ptr %p, ", (const void *) s);
        }

        if (zone_num == 0) {
            fprintf(fp, "offset %6ld words",
                (long) (MR_Integer) (s - zone->MR_zone_min));
        } else {
            fprintf(fp, "offset %6ld words in segment %d",
                (long) (MR_Integer) (s - zone->MR_zone_min), zone_num);
        }
    } else {
        fprintf(fp, "raw ptr %p", (const void *) s);
    }
}

void
MR_print_nondetstackptr(FILE *fp, const MR_Word *s)
{
    MR_MemoryZone   *zone;
    int             zone_num;

    if (MR_find_zone_for_nondet_ptr(s, NULL, &zone, &zone_num)) {
        if (zone_num == 0) {
            fprintf(fp, "non %3ld",
                (long) (MR_Integer) (s - zone->MR_zone_min));
        } else {
            fprintf(fp, "non %3ld, segment %d",
                (long) (MR_Integer) (s - zone->MR_zone_min), zone_num);
        }

        if (MR_print_raw_addrs) {
            fprintf(fp, " (%p)", (const void *) s);
        }
    } else {
        fprintf(fp, "non raw %p", (const void *) s);
    }
}

void
MR_printnondetstack(FILE *fp, const MR_Word *s)
{
    MR_MemoryZone   *zone;
    int             zone_num;

    if (MR_find_zone_for_nondet_ptr(s, NULL, &zone, &zone_num)) {
        if (MR_print_raw_addrs) {
            fprintf(fp, "ptr %p, ", (const void *) s);
        }

        if (zone_num == 0) {
            fprintf(fp, "offset %6ld words",
                (long) (MR_Integer) (s - zone->MR_zone_min));
        } else {
            fprintf(fp, "offset %6ld words in segment %d",
                (long) (MR_Integer) (s - zone->MR_zone_min), zone_num);
        }
    } else {
        fprintf(fp, "raw ptr %p", (const void *) s);
    }
}

#endif // !MR_HIGHLEVEL_CODE

void
MR_print_heapptr(FILE *fp, const MR_Word *s)
{
#ifdef  MR_CONSERVATIVE_GC
    fprintf(fp, "heap %" MR_INTEGER_LENGTH_MODIFIER "d", (MR_Integer) s);
#else
    fprintf(fp, "heap %3ld",
        (long) (MR_Integer) (s - MR_ENGINE(MR_eng_heap_zone)->MR_zone_min));
#endif

    if (MR_print_raw_addrs) {
        fprintf(fp, " (%p)", (const void *) s);
    }
}

// The code of MR_print_label is similar to, but significantly more powerful
// than, MR_lookup_entry_or_internal in mercury_label.c, since it does not
// have to return a single short constant string.

void
MR_print_label(FILE *fp, const MR_Code *w)
{
    MR_Internal *internal;

    internal = MR_lookup_internal_by_addr(w);
    if (internal != NULL) {
        if (internal->MR_internal_name != NULL) {
            fprintf(fp, "label %s", internal->MR_internal_name);
        } else {
            fprintf(fp, "unnamed label %p", internal->MR_internal_addr);
        }
#ifdef  MR_DEBUG_LABEL_GOAL_PATHS
        if (internal->MR_internal_layout != NULL) {
            fprintf(fp, " <%s>",
                MR_label_goal_path(internal->MR_internal_layout));
        }
#endif
    } else {
        MR_Entry    *entry;

        entry = MR_prev_entry_by_addr(w);
        if (entry != NULL && entry->MR_entry_addr == w) {
            if (entry->MR_entry_name != NULL) {
                fprintf(fp, "entry label %s", entry->MR_entry_name);
            } else {
                fprintf(fp, "unnamed entry label %p", entry->MR_entry_addr);
            }
        } else {
            fprintf(fp, "label UNKNOWN %p", w);
        }
    }

    if (MR_print_raw_addrs) {
        fprintf(fp, " (%p)", w);
    }
}

void
MR_printlabel(FILE *fp, const MR_Code *w)
{
    MR_print_label(fp, w);
    fprintf(fp, "\n");
}

void
MR_print_deep_prof_var(FILE *fp, const char *name, MR_CallSiteDynamic *csd)
{
#ifdef  MR_DEEP_PROFILING
    fprintf(fp, "%s: %p", name, csd);

    if (csd == NULL) {
        fprintf(fp, "\n");
    } else {
        const MR_ProcDynamic    *pd;
        const MR_ProcLayout     *pl;
        const MR_ProcStatic     *ps;
        const MR_ProcId         *proc_id;

        fprintf(fp, ", depth %d,", csd->MR_csd_depth_count);

#ifdef  MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS
        fprintf(fp, " calls %d,",
            csd->MR_csd_own.MR_own_calls);
#endif
        fprintf(fp, " exits %d, fails %d, redos %d\n",
            csd->MR_csd_own.MR_own_exits,
            csd->MR_csd_own.MR_own_fails,
            csd->MR_csd_own.MR_own_redos);

        pd = csd->MR_csd_callee_ptr;
        fprintf(fp, "  pd: %p", pd);
        if (pd == NULL) {
            fprintf(fp, "\n");
        } else if (pd->MR_pd_proc_layout == NULL) {
            fprintf(fp, ", pl is NULL\n");
        } else {
            pl = pd->MR_pd_proc_layout;
            ps = pl->MR_sle_proc_static;
            fprintf(fp, ", pl: %p, ps: %p\n", pl, ps);
            proc_id = &pl->MR_sle_proc_id;
            if (MR_PROC_ID_IS_UCI(*proc_id)) {
                fprintf(fp, "  %s:%s %s/%d-%d\n  ",
                    proc_id->MR_proc_uci.MR_uci_type_module,
                    proc_id->MR_proc_uci.MR_uci_type_name,
                    proc_id->MR_proc_uci.MR_uci_pred_name,
                    proc_id->MR_proc_uci.MR_uci_type_arity,
                    proc_id->MR_proc_uci.MR_uci_mode);
            } else {
                fprintf(fp, "  %s.%s/%d-%d\n  ",
                    proc_id->MR_proc_user.MR_user_decl_module,
                    proc_id->MR_proc_user.MR_user_name,
                    proc_id->MR_proc_user.MR_user_pred_form_arity,
                    proc_id->MR_proc_user.MR_user_mode);
            }

#ifdef  MR_USE_ACTIVATION_COUNTS
            fprintf(fp, "active %d, ", ps->MR_ps_activation_count);
#endif
            fprintf(fp, "outermost %p, array %d\n",
                ps->MR_ps_outermost_activation_ptr, ps->MR_ps_num_call_sites);
        }
    }
#endif
}

////////////////////////////////////////////////////////////////////////////

// Auxiliary routines for the code that prints debugging messages.

static MR_bool
MR_find_zone_for_det_ptr_in_context(const MR_Word *ptr, MR_Context *ctxt,
    MR_MemoryZone **zone_ptr, int *zone_num_ptr)
{
#ifdef  MR_HIGHLEVEL_CODE
    return MR_FALSE;
#else   // !MR_HIGHLEVEL_CODE
    return MR_find_zone_for_ptr(ptr,
        ctxt->MR_ctxt_detstack_zone,
        ctxt->MR_ctxt_prev_detstack_zones,
        zone_ptr, zone_num_ptr);
#endif  // !MR_HIGHLEVEL_CODE
}

static MR_bool
MR_find_zone_for_nondet_ptr_in_context(const MR_Word *ptr, MR_Context *ctxt,
    MR_MemoryZone **zone_ptr, int *zone_num_ptr)
{
#ifdef  MR_HIGHLEVEL_CODE
    return MR_FALSE;
#else   // !MR_HIGHLEVEL_CODE
    return MR_find_zone_for_ptr(ptr,
        ctxt->MR_ctxt_nondetstack_zone,
        ctxt->MR_ctxt_prev_nondetstack_zones,
        zone_ptr, zone_num_ptr);
#endif  // !MR_HIGHLEVEL_CODE
}

static MR_bool
MR_find_zone_for_ptr(const MR_Word *ptr,
    MR_MemoryZone *first_zone, MR_MemoryZones *later_zones,
    MR_MemoryZone **zone_ptr, int *zone_num_ptr)
{
#ifdef  MR_HIGHLEVEL_CODE
    return MR_FALSE;
#else   // !MR_HIGHLEVEL_CODE
    int             segment_number;
    MR_MemoryZones  *remaining_zones;
    MR_MemoryZone   *cur_zone;

    remaining_zones = later_zones;
    if (MR_in_zone(ptr, first_zone)) {
        if (zone_ptr != NULL) {
            *zone_ptr = first_zone;
        }
    } else {
        MR_bool found;

        found = MR_FALSE;
        while (remaining_zones != NULL) {
            cur_zone = remaining_zones->MR_zones_head;
            if (MR_in_zone(ptr, cur_zone)) {
                if (zone_ptr != NULL) {
                    *zone_ptr = cur_zone;
                }

                found = MR_TRUE;
                remaining_zones = remaining_zones->MR_zones_tail;
                break;
            }

            remaining_zones = remaining_zones->MR_zones_tail;
        }

        if (!found) {
            return MR_FALSE;
        }
    }

    if (zone_num_ptr != NULL) {
        segment_number = 0;
        while (remaining_zones != NULL) {
            segment_number++;
            remaining_zones = remaining_zones->MR_zones_tail;
        }

        *zone_num_ptr = segment_number;
    }

    return MR_TRUE;
#endif  // !MR_HIGHLEVEL_CODE
}

static MR_bool
MR_find_zone_for_det_ptr(const MR_Word *ptr, MR_Context **ctxt_ptr,
    MR_MemoryZone **zone_ptr, int *zone_num_ptr)
{
#ifdef  MR_USE_MINIMAL_MODEL_OWN_STACKS

    const MR_Dlist      *item;
    MR_Context          *ctxt;

    if (MR_find_zone_for_det_ptr_in_context(ptr,
        MR_ENGINE(MR_eng_main_context), zone_ptr, zone_num_ptr))
    {
        if (ctxt_ptr != NULL) {
            *ctxt_ptr = MR_ENGINE(MR_eng_main_context);
        }

        return MR_TRUE;
    }

    MR_for_dlist(item, MR_ENGINE(MR_eng_gen_contexts)) {
        ctxt = (MR_Context *) MR_dlist_data(item);
        if (MR_find_zone_for_det_ptr_in_context(ptr, ctxt,
            zone_ptr, zone_num_ptr))
        {
            if (ctxt_ptr != NULL) {
                *ctxt_ptr = ctxt;
            }

            return MR_TRUE;
        }
    }

#else   // !MR_USE_MINIMAL_MODEL_OWN_STACKS

    if (MR_find_zone_for_det_ptr_in_context(ptr, &MR_ENGINE(MR_eng_context),
        zone_ptr, zone_num_ptr))
    {
        if (ctxt_ptr != NULL) {
            *ctxt_ptr = &MR_ENGINE(MR_eng_context);
        }

        return MR_TRUE;
    }

#endif  // MR_USE_MINIMAL_MODEL_OWN_STACKS

    return MR_FALSE;
}

static MR_bool
MR_find_zone_for_nondet_ptr(const MR_Word *ptr, MR_Context **ctxt_ptr,
    MR_MemoryZone **zone_ptr, int *zone_num_ptr)
{
#ifdef  MR_USE_MINIMAL_MODEL_OWN_STACKS

    const MR_Dlist      *item;
    MR_Context          *ctxt;

    if (MR_find_zone_for_nondet_ptr_in_context(ptr,
        MR_ENGINE(MR_eng_main_context), zone_ptr, zone_num_ptr))
    {
        if (ctxt_ptr != NULL) {
            *ctxt_ptr = MR_ENGINE(MR_eng_main_context);
        }

        return MR_TRUE;
    }

    MR_for_dlist(item, MR_ENGINE(MR_eng_gen_contexts)) {
        ctxt = (MR_Context *) MR_dlist_data(item);
        if (MR_find_zone_for_nondet_ptr_in_context(ptr, ctxt,
            zone_ptr, zone_num_ptr))
        {
            if (ctxt_ptr != NULL) {
                *ctxt_ptr = ctxt;
            }

            return MR_TRUE;
        }
    }

#else   // !MR_USE_MINIMAL_MODEL_OWN_STACKS

    if (MR_find_zone_for_nondet_ptr_in_context(ptr, &MR_ENGINE(MR_eng_context),
        zone_ptr, zone_num_ptr))
    {
        if (ctxt_ptr != NULL) {
            *ctxt_ptr = &MR_ENGINE(MR_eng_context);
        }

        return MR_TRUE;
    }

#endif  // MR_USE_MINIMAL_MODEL_OWN_STACKS

    return MR_FALSE;
}

////////////////////////////////////////////////////////////////////////////

void
MR_debug_log_message(const char *format, ...)
{
    char    *buffer;
    int     len;
    int     result;
    va_list args;

    // This should be a reasonable estimate of the size of the buffer that we
    // need. At least twice the size of the format string or 128 bytes.

    len = strlen(format);
    len = len * 2;
    len = len < 128 ? 128 : len;

    buffer = MR_GC_malloc(len);
    while (1) {
        va_start(args, format);
        result = MR_vsnprintf(buffer, len, format, args);
        va_end(args);
        if (result < len) {
            break;
        }

        // Make the buffer bigger.
        len = len * 2;
        buffer = MR_GC_realloc(buffer, len);
    }

#if defined(MR_THREADSCOPE) && defined(MR_THREAD_SAFE)
    MR_threadscope_post_log_msg(buffer);
#elif defined(MR_THREADSCOPE)
    printf("Eng %p: %s\n", MR_thread_engine_base, buffer);
#else
    printf("%s\n", buffer);
#endif
}
