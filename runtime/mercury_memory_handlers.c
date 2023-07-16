// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998, 2000, 2002, 2005-2007, 2010-2011 The University of Melbourne.
// Copyright (C) 2014-2016, 2018, 2020 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module defines the signal handlers for memory zones.
// These handlers are invoked when memory is accessed outside of
// the memory zones, or at the protected region at the end of a
// memory zone (if available).

////////////////////////////////////////////////////////////////////////////

#ifndef _GNU_SOURCE
  // This must be defined for REG_RIP, etc.
  #define _GNU_SOURCE
#endif

#include "mercury_imp.h"

#ifdef MR_HAVE_UNISTD_H
  #include <unistd.h>
#endif

#include <stdio.h>
#include <string.h>

// This include must come before anything else that might include <signal.h>.
// See the comments in mercury_signal.h.

#include "mercury_signal.h"

#ifdef MR_HAVE_SYS_SIGINFO_H
  #include <sys/siginfo.h>
#endif

#ifdef  MR_HAVE_MPROTECT
  #include <sys/mman.h>
#endif

#ifdef  MR_HAVE_UCONTEXT_H
  #include <ucontext.h>
#endif

#ifdef  MR_HAVE_SYS_UCONTEXT_H
  #include <sys/ucontext.h>
#endif

#include "mercury_trace_base.h"
#include "mercury_memory_zones.h"
#include "mercury_memory_handlers.h"
#include "mercury_threadscope.h"

////////////////////////////////////////////////////////////////////////////

#ifdef MR_HAVE_SIGINFO
  #ifdef MR_HAVE_SIGINFO_T
    static void         complex_bushandler(int, siginfo_t *, void *);
    static void         complex_segvhandler(int, siginfo_t *, void *);
  #else
    #error "MR_HAVE_SIGINFO defined but don't know how to get it"
  #endif
#else
  static void           simple_sighandler(int);
#endif

// round_up(amount, align) returns `amount' rounded up to the nearest
// alignment boundary. `align' must be a power of 2.

static  void    MR_print_dump_stack(void);
static  MR_bool MR_try_munprotect(void *address, void *context);
static  char    *MR_explain_context(void *context);
#if defined(MR_NATIVE_GC) && !defined(MR_HIGHLEVEL_CODE)
   static  MR_Code *get_pc_from_context(void *the_context);
   static  MR_Word *get_sp_from_context(void *the_context);
   static  MR_Word *get_curfr_from_context(void *the_context);
#endif
static  void    leave_signal_handler(int sig);

#define STDERR 2

// Note that we cannot assume that the memory zones have been initialized here,
// since MR_setup_signals() gets called before MR_init_memory_zones().
// However, the code here will work fine if used_memory_zones is null.

static MR_bool
MR_try_munprotect(void *addr, void *context)
{
#if !defined(MR_HAVE_SIGINFO)
    return MR_FALSE;
#else
    MR_Word         *fault_addr;
    MR_MemoryZone   *zone;

    fault_addr = (MR_Word *) addr;

    zone = MR_get_used_memory_zones_readonly();

    if (MR_memdebug) {
        fprintf(stderr, "caught fault at %p\n", (void *)addr);
    }

    while (zone != NULL) {
  #ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
        if (MR_memdebug) {
            fprintf(stderr, "checking %s#%" MR_INTEGER_LENGTH_MODIFIER
                    "d: %p - %p\n",
                zone->MR_zone_name, zone->MR_zone_id,
                (void *) zone->MR_zone_redzone,
                (void *) zone->MR_zone_top);
        }

        if (zone->MR_zone_redzone <= fault_addr
            && fault_addr <= zone->MR_zone_top)
        {
            if (MR_memdebug) {
                fprintf(stderr, "address is in %s#% "
                        MR_INTEGER_LENGTH_MODIFIER "d redzone\n",
                    zone->MR_zone_name, zone->MR_zone_id);
            }

            return zone->MR_zone_handler(fault_addr, zone, context);
        }
  #endif
        zone = zone->MR_zone_next;
    }

    if (MR_memdebug) {
        fprintf(stderr, "address not in any redzone.\n");
    }

    return MR_FALSE;
#endif // MR_HAVE_SIGINFO
}

MR_bool
MR_null_handler(MR_Word *fault_addr, MR_MemoryZone *zone, void *context)
{
    return MR_FALSE;
}

// MR_fatal_abort() prints an error message, possibly a stack dump,
// and then exits. It is like MR_fatal_error(), except that it is safe to call
// from a signal handler.

static void
MR_fatal_abort(void *context, const char *main_msg, int dump)
{
    char    *context_msg;
    int     ret;

    context_msg = MR_explain_context(context);
    do {
        ret = write(STDERR, main_msg, strlen(main_msg));
    } while (ret == -1 && MR_is_eintr(errno));
    do {
        ret = write(STDERR, context_msg, strlen(context_msg));
    } while (ret == -1 && MR_is_eintr(errno));
    MR_trace_report_raw(STDERR);

    if (dump) {
        MR_print_dump_stack();
    }

    _exit(1);
}

MR_bool
MR_default_handler(MR_Word *fault_addr, MR_MemoryZone *zone, void *context)
{
#ifndef MR_CHECK_OVERFLOW_VIA_MPROTECT
    return MR_FALSE;
#else
    MR_Word *new_zone;
    size_t  zone_size;

    new_zone = (MR_Word *) MR_round_up((MR_Unsigned) fault_addr
        + sizeof(MR_Word), MR_unit);

    if (new_zone <= zone->MR_zone_hardmax) {
        zone_size = (char *) new_zone - (char *) zone->MR_zone_redzone;

        if (MR_memdebug) {
            fprintf(stderr, "trying to unprotect %s#%"
                MR_INTEGER_LENGTH_MODIFIER "d from %p to %p (%x)\n",
            zone->MR_zone_name, zone->MR_zone_id,
            (void *) zone->MR_zone_redzone, (void *) new_zone,
            (int) zone_size);
        }
        if (MR_protect_pages((char *) zone->MR_zone_redzone, zone_size,
            PROT_READ|PROT_WRITE) < 0)
        {
            char buf[2560];
            sprintf(buf, "Mercury runtime: cannot unprotect %s#%"
                    MR_INTEGER_LENGTH_MODIFIER "d zone",
                zone->MR_zone_name, zone->MR_zone_id);
            perror(buf);
            exit(1);
        }

        zone->MR_zone_redzone = new_zone;

        if (MR_memdebug) {
            fprintf(stderr, "successful: %s#%" MR_INTEGER_LENGTH_MODIFIER
                    "d redzone now %p to %p\n",
                zone->MR_zone_name, zone->MR_zone_id,
                (void *) zone->MR_zone_redzone, (void *) zone->MR_zone_top);
        }
      #if defined(MR_NATIVE_GC) && !defined(MR_HIGHLEVEL_CODE)
        MR_schedule_agc(get_pc_from_context(context),
            get_sp_from_context(context),
            get_curfr_from_context(context));
      #endif
        return MR_TRUE;
    } else {
        char buf[2560];
        if (MR_memdebug) {
            fprintf(stderr, "can't unprotect last page of %s#%"
                    MR_INTEGER_LENGTH_MODIFIER "d\n",
                zone->MR_zone_name, zone->MR_zone_id);
            fflush(stdout);
        }
#ifdef  MR_STACK_EXTEND_DEBUG
        MR_restore_transient_registers();
        fprintf(stderr, "sp = %p, maxfr = %p\n", MR_sp, MR_maxfr);
        MR_debug_memory_zone(stderr, zone);
#endif
        sprintf(buf, "\nMercury runtime: memory zone %s#%"
                MR_INTEGER_LENGTH_MODIFIER "d overflowed\n",
            zone->MR_zone_name, zone->MR_zone_id);
        MR_fatal_abort(context, buf, MR_TRUE);
    }

    return MR_FALSE;
#endif
}

void
MR_setup_signals(void)
{
// When using Microsoft Visual C structured exceptions don't set any
// signal handlers.
// See mercury_wrapper.c for the reason why.

#ifndef MR_MSVC_STRUCTURED_EXCEPTIONS

  #ifdef MR_HAVE_SIGINFO_T

    #ifdef SIGBUS
    MR_setup_signal(SIGBUS, (MR_Code *) complex_bushandler, MR_TRUE,
        "cannot set SIGBUS handler");
    #endif
    MR_setup_signal(SIGSEGV, (MR_Code *) complex_segvhandler, MR_TRUE,
        "cannot set SIGSEGV handler");

  #else

    #ifdef SIGBUS
    MR_setup_signal(SIGBUS, (MR_Code *) simple_sighandler, MR_FALSE,
        "cannot set SIGBUS handler");
    #endif
    MR_setup_signal(SIGSEGV, (MR_Code *) simple_sighandler, MR_FALSE,
        "cannot set SIGSEGV handler");

  #endif

#endif
}

static char *
MR_explain_context(void *the_context)
{
    static  char    buf[100];

#ifdef MR_HAVE_SIGINFO_T

  #ifdef MR_PC_ACCESS

    ucontext_t *context = the_context;

    #ifdef MR_PC_ACCESS_GREG
    sprintf(buf, "PC at signal: %ld (%lx)\n",
        (long) context->uc_mcontext.gregs[MR_PC_ACCESS],
        (long) context->uc_mcontext.gregs[MR_PC_ACCESS]);
    #else
    sprintf(buf, "PC at signal: %ld (%lx)\n",
        (long) context->uc_mcontext.MR_PC_ACCESS,
        (long) context->uc_mcontext.MR_PC_ACCESS);
    #endif

  #else // not MR_PC_ACCESS

    // If MR_PC_ACCESS is not set, we don't know the context,
    // therefore we return an empty string to be printed.
    buf[0] = '\0';

  #endif // not MR_PC_ACCESS

#else // not MR_HAVE_SIGINFO_T

    buf[0] = '\0';

#endif

    return buf;
}

#ifdef MR_HAVE_SIGINFO_T

static void
complex_bushandler(int sig, siginfo_t *info, void *context)
{
    fflush(stdout);

    if (sig != SIGBUS || !info || info->si_signo != SIGBUS) {
        MR_fatal_abort(context, "\n*** Mercury runtime: "
            "caught strange bus error ***\n", 1);
    }

    fprintf(stderr, "\n*** Mercury runtime: ");
    fprintf(stderr, "caught bus error ***\n");

    if (info->si_code > 0) {
        fprintf(stderr, "cause: ");
        switch (info->si_code)
        {
#ifdef BUS_ADRALN
        case BUS_ADRALN:
            fprintf(stderr, "invalid address alignment\n");
            break;
#endif

#ifdef BUS_ADRERR
        case BUS_ADRERR:
            fprintf(stderr, "non-existent physical address\n");
            break;
#endif

#ifdef BUS_OBJERR
        case BUS_OBJERR:
            fprintf(stderr, "object specific hardware error\n");
            break;
#endif

#ifdef BUS_PAGE_FAULT
        case BUS_PAGE_FAULT:
            fprintf(stderr, "page fault protection base\n");
            break;
#endif

#ifdef BUS_SEGNP_FAULT
        case BUS_SEGNP_FAULT:
            fprintf(stderr, "segment not present\n");
            break;
#endif

#ifdef BUS_STK_FAULT
        case BUS_STK_FAULT:
            fprintf(stderr, "stack segment\n");
            break;
#endif

#ifdef BUS_SEGM_FAULT
        case BUS_SEGM_FAULT:
            fprintf(stderr, "segment protection base\n");
            break;
#endif

        default:
            fprintf(stderr, "unknown\n");
            break;

        } // end switch

        fprintf(stderr, "%s", MR_explain_context(context));
        fprintf(stderr, "address involved: %p\n",
            (void *) info->si_addr);
    } // end if

    MR_trace_report(stderr);
    MR_print_dump_stack();
    MR_dump_prev_locations();
    leave_signal_handler(sig);
} // end complex_bushandler()

static void
MR_explain_segv(siginfo_t *info, void *context)
{
    fflush(stdout);

    fprintf(stderr, "\n*** Mercury runtime: ");
    fprintf(stderr, "caught segmentation violation ***\n");

    if (!info) {
        return;
    }

    if (info->si_code > 0) {
        fprintf(stderr, "cause: ");
        switch (info->si_code)
        {
#ifdef SEGV_MAPERR
        case SEGV_MAPERR:
            fprintf(stderr, "address not mapped to object\n");
            break;
#endif

#ifdef SEGV_ACCERR
        case SEGV_ACCERR:
            fprintf(stderr, "bad permissions for mapped object\n");
            break;
#endif

        default:
            fprintf(stderr, "unknown\n");
            break;
        }

        fprintf(stderr, "%s", MR_explain_context(context));
        fprintf(stderr, "address involved: %p\n",
            (void *) info->si_addr);

    } // end if
}

static void
complex_segvhandler(int sig, siginfo_t *info, void *context)
{
    if (sig != SIGSEGV || !info || info->si_signo != SIGSEGV) {
        MR_fatal_abort(context, "\n*** Mercury runtime: "
            "caught strange segmentation violation ***\n", 1);
    }

    // If we are debugging, print the segv explanation messages
    // before we call MR_try_munprotect. But if we are not debugging,
    // only print them if MR_try_munprotect fails.

    if (MR_memdebug) {
        MR_explain_segv(info, context);
    }

    if (MR_try_munprotect(info->si_addr, context)) {
        if (MR_memdebug) {
            fprintf(stderr, "returning from signal handler\n\n");
        }

        return;
    }

    if (!MR_memdebug) {
        MR_explain_segv(info, context);
    }

    MR_trace_report(stderr);
    MR_print_dump_stack();
    MR_dump_prev_locations();
    leave_signal_handler(sig);
} // end complex_segvhandler

#else // not MR_HAVE_SIGINFO_T

static void
simple_sighandler(int sig)
{
    fflush(stdout);
    fprintf(stderr, "*** Mercury runtime: ");

    switch (sig)
    {
#ifdef SIGBUS
    case SIGBUS:
        fprintf(stderr, "caught bus error ***\n");
        break;
#endif

    case SIGSEGV:
        fprintf(stderr, "caught segmentation violation ***\n");
        break;

    default:
        fprintf(stderr, "caught unknown signal %d ***\n", sig);
        break;
    }

    MR_print_dump_stack();
    MR_dump_prev_locations();
    leave_signal_handler(sig);
}

#endif // not MR_HAVE_SIGINFO_T

#ifdef MR_MSVC_STRUCTURED_EXCEPTIONS
static const char   *MR_find_exception_name(DWORD exception_code);
static void         MR_explain_exception_record(EXCEPTION_RECORD *rec);
static void         MR_dump_exception_record(EXCEPTION_RECORD *rec);
static MR_bool      MR_exception_record_is_access_violation(
                        EXCEPTION_RECORD *rec, void **address_ptr,
                        int *access_mode_ptr);

// Exception code and their string representation.

#define DEFINE_EXCEPTION_NAME(a)   {a,#a}

typedef struct
{
    DWORD       exception_code;
    const char  *exception_name;
} MR_ExceptionName;

static const
MR_ExceptionName MR_exception_names[] =
{
    DEFINE_EXCEPTION_NAME(EXCEPTION_ACCESS_VIOLATION),
    DEFINE_EXCEPTION_NAME(EXCEPTION_DATATYPE_MISALIGNMENT),
    DEFINE_EXCEPTION_NAME(EXCEPTION_BREAKPOINT),
    DEFINE_EXCEPTION_NAME(EXCEPTION_SINGLE_STEP),
    DEFINE_EXCEPTION_NAME(EXCEPTION_ARRAY_BOUNDS_EXCEEDED),
    DEFINE_EXCEPTION_NAME(EXCEPTION_FLT_DENORMAL_OPERAND),
    DEFINE_EXCEPTION_NAME(EXCEPTION_FLT_DIVIDE_BY_ZERO),
    DEFINE_EXCEPTION_NAME(EXCEPTION_FLT_INEXACT_RESULT),
    DEFINE_EXCEPTION_NAME(EXCEPTION_FLT_INVALID_OPERATION),
    DEFINE_EXCEPTION_NAME(EXCEPTION_FLT_OVERFLOW),
    DEFINE_EXCEPTION_NAME(EXCEPTION_FLT_STACK_CHECK),
    DEFINE_EXCEPTION_NAME(EXCEPTION_FLT_UNDERFLOW),
    DEFINE_EXCEPTION_NAME(EXCEPTION_INT_DIVIDE_BY_ZERO),
    DEFINE_EXCEPTION_NAME(EXCEPTION_INT_OVERFLOW),
    DEFINE_EXCEPTION_NAME(EXCEPTION_PRIV_INSTRUCTION),
    DEFINE_EXCEPTION_NAME(EXCEPTION_IN_PAGE_ERROR),
    DEFINE_EXCEPTION_NAME(EXCEPTION_ILLEGAL_INSTRUCTION),
    DEFINE_EXCEPTION_NAME(EXCEPTION_NONCONTINUABLE_EXCEPTION),
    DEFINE_EXCEPTION_NAME(EXCEPTION_STACK_OVERFLOW),
    DEFINE_EXCEPTION_NAME(EXCEPTION_INVALID_DISPOSITION),
    DEFINE_EXCEPTION_NAME(EXCEPTION_GUARD_PAGE),
    DEFINE_EXCEPTION_NAME(EXCEPTION_INVALID_HANDLE)
};

// Retrieve the name of a Win32 exception code as a string.

static const char *
MR_find_exception_name(DWORD exception_code)
{
    int i;
    for (i = 0; i < sizeof(MR_exception_names)
            / sizeof(MR_ExceptionName); i++)
    {
        if (MR_exception_names[i].exception_code == exception_code) {
            return MR_exception_names[i].exception_name;
        }
    }
    return "Unknown exception code";
}

// Was a page accessed read/write? The MSDN documentation doesn't define
// symbolic constants for these alternatives.

#define READ    0
#define WRITE   1

// Explain an EXCEPTION_RECORD content into stderr.

static void
MR_explain_exception_record(EXCEPTION_RECORD *rec)
{
    fprintf(stderr, "\n");
    fprintf(stderr, "\n*** Explanation of the exception record");
    if (rec == NULL) {
        fprintf(stderr, "\n***   Cannot explain because it is NULL");
        return;
    } else {
        void *address;
        int access_mode;

        // If the exception is an access violation.
        if (MR_exception_record_is_access_violation(rec,
            &address, &access_mode))
        {
            MR_MemoryZone *zone;

            // Display AV address and access mode.
            fprintf(stderr, "\n***   An access violation occured"
                    " at address 0x%08" MR_INTEGER_LENGTH_MODIFIER
                    "x, while attempting"
                    " to ", (MR_Word) address);

            if (access_mode == READ) {
                fprintf(stderr, "\n***   read inaccessible data");
            } else if (access_mode == WRITE) {
                fprintf(stderr, "\n***   write to an "
                    "inaccessible (or protected) address");
            } else {
                fprintf(stderr, "\n***   ? [unknown access "
                    "mode %d (strange...)]",
                    access_mode);
            }

            #if defined(MR_CHECK_OVERFLOW_VIA_MPROTECT)
            fprintf(stderr, "\n***   Trying to see if this "
                "stands within a mercury zone...");
            // Browse the Mercury memory zones to see if the
            // AV address references one of them.

            zone = MR_get_used_memory_zones_readonly();
            while (zone != NULL) {
                fprintf(stderr,
                    "\n***    Checking zone %s#%"
                    MR_INTEGER_LENGTH_MODIFIER "d: "
                    "0x%08" MR_INTEGER_LENGTH_MODIFIER "x - "
                    "0x%08" MR_INTEGER_LENGTH_MODIFIER "x - "
                    "0x%08" MR_INTEGER_LENGTH_MODIFIER "x",
                    zone->MR_zone_name, zone->MR_zone_id,
                    zone->MR_zone_bottom,
                    zone->MR_zone_redzone,
                    zone->MR_zone_top);

                if ((zone->MR_zone_redzone <= address) &&
                    (address <= zone->MR_zone_top))
                {
                    fprintf(stderr,
                        "\n***     Address is within redzone of "
                        "%s#%" MR_INTEGER_LENGTH_MODIFIER
                        "d (!!zone overflowed!!)\n",
                        zone->MR_zone_name, zone->MR_zone_id);
                } else if ((zone->MR_zone_bottom <= address) &&
                        (address <= zone->MR_zone_top))
                {
                    fprintf(stderr, "\n***     Address is"
                        " within zone %s#%" MR_INTEGER_LENGTH_MODIFIER "d\n",
                        zone->MR_zone_name, zone->MR_zone_id);
                }
                // Don't need to call handler, because it
                // has much less information than we do.

                // return zone->MR_zone_handler(fault_addr, zone, rec);
                zone = zone->MR_zone_next;
            }
            #endif // MR_CHECK_OVERFLOW_VIA_MPROTECT
        }
        return;
    }
}

// Dump an EXCEPTION_RECORD content into stderr.

static void
MR_dump_exception_record(EXCEPTION_RECORD *rec)
{
    int i;

    if (rec == NULL) {
        return;
    }

    fprintf(stderr, "\n***   Exception record at 0x%08"
        MR_INTEGER_LENGTH_MODIFIER "x:",
        (MR_Word) rec);
    fprintf(stderr, "\n***    MR_Code     : 0x%08"
        MR_INTEGER_LENGTH_MODIFIER "x (%s)",
        (MR_Word) rec->ExceptionCode,
        MR_find_exception_name(rec->ExceptionCode));
    fprintf(stderr, "\n***    Flags       : 0x%08"
        MR_INTEGER_LENGTH_MODIFIER "x",
        (MR_Word) rec->ExceptionFlags);
    fprintf(stderr, "\n***    Address     : 0x%08"
        MR_INTEGER_LENGTH_MODIFIER "x",
        (MR_Word) rec->ExceptionAddress);

    for (i = 0; i < rec->NumberParameters; i++) {
        fprintf(stderr, "\n***    Parameter %d : 0x%08lx", i,
            (unsigned long) rec->ExceptionInformation[i]);
    }
    fprintf(stderr, "\n***    Next record : 0x%08"
        MR_INTEGER_LENGTH_MODIFIER "x",
        (MR_Word) rec->ExceptionRecord);

    // Try to explain the exception more "gracefully".
    MR_explain_exception_record(rec);
    MR_dump_exception_record(rec->ExceptionRecord);
}

// Return MR_TRUE iff exception_ptrs indicates an access violation.
// If MR_TRUE, the dereferenced address_ptr is set to the accessed address and
// the dereferenced access_mode_ptr is set to the desired access
// (0 = read, 1 = write)

static MR_bool
MR_exception_record_is_access_violation(EXCEPTION_RECORD *rec,
    void **address_ptr, int *access_mode_ptr)
{
    if (rec->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
        if (rec->NumberParameters >= 2) {
            (*access_mode_ptr) = (int) rec->ExceptionInformation[0];
            (*address_ptr) = (void *) rec->ExceptionInformation[1];
            return MR_TRUE;
        }
    }
    return MR_FALSE;
}

// Filter a Win32 exception (to be called in the __except filter part).
// Possible return values are:
//
// EXCEPTION_CONTINUE_EXECUTION (-1)
//  Exception is dismissed. Continue execution at the point where
//  the exception occurred.
//
// EXCEPTION_CONTINUE_SEARCH (0)
//  Exception is not recognized. Continue to search up the stack for
//  a handler, first for containing try-except statements, then for
//  handlers with the next highest precedence.
//
// EXCEPTION_EXECUTE_HANDLER (1)
//  Exception is recognized. Transfer control to the exception handler
//  by executing the __except compound statement, then continue
//  execution at the assembly instruction that was executing
//  when the exception was raised.

int
MR_filter_win32_exception(LPEXCEPTION_POINTERS exception_ptrs)
{
    void *address;
    int access_mode;

    // Is the exception an access violation?
    if (MR_exception_record_is_access_violation(
        exception_ptrs->ExceptionRecord, &address, &access_mode))
    {

        // Can we unprotect the memory zone?
        if (MR_try_munprotect(address, exception_ptrs)) {
            if (MR_memdebug) {
                fprintf(stderr, "returning from signal handler\n\n");
            }
            // Continue execution where it stopped.
            return  EXCEPTION_CONTINUE_EXECUTION;
        }
    }

    // We can't handle the exception. Just dump all the information we got.

    fflush(stdout);
    fprintf(stderr, "\n*** Mercury runtime: Unhandled exception ");
    MR_dump_exception_record(exception_ptrs->ExceptionRecord);

    printf("\n");
    MR_print_dump_stack();
    MR_dump_prev_locations();

    fprintf(stderr, "\n\n*** Now passing exception to default handler\n\n");
    fflush(stderr);

    // Pass exception back to upper handler. In most cases, this means
    // activating UnhandledExceptionFilter, which will display a dialog box
    // asking to user ro activate the Debugger or simply to kill
    // the application.

    return  EXCEPTION_CONTINUE_SEARCH;
}
#endif // MR_MSVC_STRUCTURED_EXCEPTIONS

#if defined(MR_NATIVE_GC) && !defined(MR_HIGHLEVEL_CODE)

// get_pc_from_context:
// Given the signal context, return the program counter at the time
// of the signal, if available. If it is unavailable, return NULL.

static MR_Code *
get_pc_from_context(void *the_context)
{
    MR_Code *pc_at_signal = NULL;
#ifdef MR_HAVE_SIGINFO_T

  #ifdef MR_PC_ACCESS

    ucontext_t *context = the_context;

    #ifdef MR_PC_ACCESS_GREG
    pc_at_signal = (MR_Code *) context->uc_mcontext.gregs[MR_PC_ACCESS];
    #else
    pc_at_signal = (MR_Code *) context->uc_mcontext.MR_PC_ACCESS;
    #endif

  #else // not MR_PC_ACCESS

    // If MR_PC_ACCESS is not set, we don't know the context.
    pc_at_signal = (MR_Code *) NULL;

  #endif // not MR_PC_ACCESS

#else // not MR_HAVE_SIGINFO_T

    pc_at_signal = (MR_Code *) NULL;

#endif

    return pc_at_signal;
}

// get_sp_from_context:
// Given the signal context, return the Mercury register "MR_sp" at
// the time of the signal, if available. If it is unavailable, return NULL.
//
// XXX We only define this function in LLDS accurate gc grades for the moment,
// because it is unlikely to compile everywhere. It relies on
// MR_real_reg_number_sp being defined, which is the name/number of the
// machine register that is used for MR_sp.
// Need to fix this so it works when the register is in a fake reg too.

static MR_Word *
get_sp_from_context(void *the_context)
{
    MR_Word *sp_at_signal = NULL;
#if defined(MR_NATIVE_GC) && !defined(MR_HIGHLEVEL_CODE)
  #ifdef MR_HAVE_SIGINFO_T

    #ifdef MR_PC_ACCESS

    // XXX This probably needs to be cast to ucontext_t instead.
    struct sigcontext *context = the_context;

      #ifdef MR_PC_ACCESS_GREG
    sp_at_signal = (MR_Word *) context->gregs[MR_real_reg_number_sp];
      #else
    sp_at_signal = (MR_Word *) context->sc_regs[MR_real_reg_number_sp];
      #endif

    #else // not MR_PC_ACCESS

    // If MR_PC_ACCESS is not set, we don't know how to get at the registers.

    sp_at_signal = (MR_Word *) NULL;

    #endif // not MR_PC_ACCESS

  #else // not MR_HAVE_SIGINFO_T

    sp_at_signal = (MR_Word *) NULL;

  #endif
#else // !MR_NATIVE_GC
    sp_at_signal = (MR_Word *) NULL;
#endif // !MR_NATIVE_GC

    return sp_at_signal;
}

// get_sp_from_context:
// Given the signal context, return the Mercury register "MR_sp" at
// the time of the signal, if available. If it is unavailable, return NULL.
//
// XXX We only define this function in accurate gc grades for the moment,
// because it is unlikely to compile everywhere. It relies on
// MR_real_reg_number_sp being defined, which is the name/number of the
// machine register that is used for MR_sp.
// Need to fix this so it works when the register is in a fake reg too.

static MR_Word *
get_curfr_from_context(void *the_context)
{
    MR_Word *curfr_at_signal;

    // XXX This is implementation dependent, need a better way
    // to do register accesses at signals.
    //
    // It is in mr8 or mr9 which is in the fake regs on some architectures,
    // and is a machine register on others.
    // So don't run the garbage collector on those architectures.

    curfr_at_signal = MR_curfr;

    return curfr_at_signal;
}
#endif // MR_NATIVE_GC && not MR_HIGHLEVEL_CODE

static void
MR_print_dump_stack(void)
{
    const char *msg =
        "This may have been caused by a stack overflow, due to unbounded recursion.\n";
    int ret;

    do {
        ret = write(STDERR, msg, strlen(msg));
    } while (ret == -1 && MR_is_eintr(errno));
}

static void
leave_signal_handler(int sig)
{
    fprintf(stderr, "exiting from signal handler\n");
#if defined(MR_THREAD_SAFE) && defined(MR_THREADSCOPE)
    if (MR_all_engine_bases) {
        int i;
        for (i = 0; i < MR_max_engines; i++) {
            if (MR_all_engine_bases[i] &&
                MR_all_engine_bases[i]->MR_eng_ts_buffer)
            {
                MR_threadscope_finalize_engine(MR_all_engine_bases[i]);
            }
        }
    }
    MR_finalize_threadscope();
#endif
    MR_reset_signal(sig);
    raise(sig);
}
