/*
** Copyright (C) 1998, 2000, 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module defines the signal handlers for memory zones.
** These handlers are invoked when memory is accessed outside of
** the memory zones, or at the protected region at the end of a
** memory zone (if available).
*/

/*---------------------------------------------------------------------------*/

#include "mercury_imp.h"

#ifdef MR_HAVE_UNISTD_H
  #include <unistd.h>
#endif

#include <stdio.h>
#include <string.h>

/*
** This include must come before anything else that might include <signal.h>.
** See the commments in mercury_signal.h.
*/
#include "mercury_signal.h"

#ifdef MR_HAVE_SYS_SIGINFO_H
  #include <sys/siginfo.h>
#endif 

#ifdef MR_HAVE_SYS_SIGNAL_H
  /* on FREEBSD we need to include <sys/signal.h> before <ucontext.h> */
  #include <sys/signal.h>
#endif

#ifdef	MR_HAVE_MPROTECT
  #include <sys/mman.h>
#endif

#ifdef	MR_HAVE_UCONTEXT_H
  #include <ucontext.h>
#endif

#ifdef	MR_HAVE_SYS_UCONTEXT_H
  #include <sys/ucontext.h>
#endif

#include "mercury_trace_base.h"
#include "mercury_memory_zones.h"
#include "mercury_memory_handlers.h"
#include "mercury_faultaddr.h"

/*---------------------------------------------------------------------------*/

#ifdef MR_HAVE_SIGINFO
  #if defined(MR_HAVE_SIGCONTEXT_STRUCT)
    #if defined(MR_HAVE_SIGCONTEXT_STRUCT_3ARG)
      static	void	complex_sighandler_3arg(int, int, 
			      struct sigcontext_struct);
    #else
      static	void	complex_sighandler(int, struct sigcontext_struct);
    #endif
  #elif defined(MR_HAVE_SIGINFO_T)
    static	void	complex_bushandler(int, siginfo_t *, void *);
    static	void	complex_segvhandler(int, siginfo_t *, void *);
  #else
    #error "MR_HAVE_SIGINFO defined but don't know how to get it"
  #endif
#else
  static	void	simple_sighandler(int);
#endif


#ifdef MR_HAVE_SIGINFO
  #if defined(MR_HAVE_SIGCONTEXT_STRUCT)
    #if defined(MR_HAVE_SIGCONTEXT_STRUCT_3ARG)
      #define     bus_handler	complex_sighandler_3arg
      #define     segv_handler	complex_sighandler_3arg
    #else
      #define     bus_handler	complex_sighandler
      #define     segv_handler	complex_sighandler
    #endif
  #elif defined(MR_HAVE_SIGINFO_T)
    #define     bus_handler	complex_bushandler
    #define     segv_handler	complex_segvhandler
  #else
    #error "MR_HAVE_SIGINFO defined but don't know how to get it"
  #endif
#else
    #define     bus_handler	simple_sighandler
    #define     segv_handler	simple_sighandler
#endif


/*
** round_up(amount, align) returns `amount' rounded up to the nearest
** alignment boundary.  `align' must be a power of 2.
*/

static	void	print_dump_stack(void);
static	MR_bool	try_munprotect(void *address, void *context);
static	char	*explain_context(void *context);
static	MR_Code	*get_pc_from_context(void *the_context);
static	MR_Word	*get_sp_from_context(void *the_context);
static	MR_Word	*get_curfr_from_context(void *the_context);

#define STDERR 2

static MR_bool 
try_munprotect(void *addr, void *context)
{
#if !(defined(MR_HAVE_SIGINFO) || defined(MR_WIN32_VIRTUAL_ALLOC))
	return MR_FALSE;
#else
	MR_Word *    fault_addr;
	MR_MemoryZone *zone;

	fault_addr = (MR_Word *) addr;

	zone = MR_get_used_memory_zones();

	if (MR_memdebug) {
		fprintf(stderr, "caught fault at %p\n", (void *)addr);
	}

	while(zone != NULL) {
		if (MR_memdebug) {
			fprintf(stderr, "checking %s#%d: %p - %p\n",
				zone->name, zone->id, (void *) zone->redzone,
				(void *) zone->top);
		}

		if (zone->redzone <= fault_addr && fault_addr <= zone->top) {

			if (MR_memdebug) {
				fprintf(stderr, "address is in %s#%d redzone\n",
					zone->name, zone->id);
			}

			return zone->handler(fault_addr, zone, context);
		}
		zone = zone->next;
	}

	if (MR_memdebug) {
		fprintf(stderr, "address not in any redzone.\n");
	}

	return MR_FALSE;
#endif /* MR_HAVE_SIGINFO */
} 

MR_bool 
MR_null_handler(MR_Word *fault_addr, MR_MemoryZone *zone, void *context)
{
	return MR_FALSE;
}

/*
** MR_fatal_abort() prints an error message, possibly a stack dump,
** and then exits. It is like MR_fatal_error(), except that it is safe to call
** from a signal handler.
*/

static void 
MR_fatal_abort(void *context, const char *main_msg, int dump)
{
	char	*context_msg;

	context_msg = explain_context(context);
	write(STDERR, main_msg, strlen(main_msg));
	write(STDERR, context_msg, strlen(context_msg));
	MR_trace_report_raw(STDERR);

	if (dump) {
		print_dump_stack();
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
    size_t zone_size;

    new_zone = (MR_Word *) MR_round_up((MR_Unsigned) fault_addr
		    + sizeof(MR_Word), MR_unit);

    if (new_zone <= zone->hardmax) {
	zone_size = (char *)new_zone - (char *)zone->redzone;

	if (MR_memdebug) {
	    fprintf(stderr, "trying to unprotect %s#%d from %p to %p (%x)\n",
	    zone->name, zone->id, (void *) zone->redzone, (void *) new_zone,
	    (int)zone_size);
	}
	if (MR_protect_pages((char *)zone->redzone, zone_size,
		PROT_READ|PROT_WRITE) < 0)
	{
	    char buf[2560];
	    sprintf(buf, "Mercury runtime: cannot unprotect %s#%d zone",
		zone->name, zone->id);
	    perror(buf);
	    exit(1);
	}

	zone->redzone = new_zone;

	if (MR_memdebug) {
	    fprintf(stderr, "successful: %s#%d redzone now %p to %p\n",
		zone->name, zone->id, (void *) zone->redzone,
		(void *) zone->top);
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
	    fprintf(stderr, "can't unprotect last page of %s#%d\n",
		zone->name, zone->id);
	    fflush(stdout);
	}
	sprintf(buf, "\nMercury runtime: memory zone %s#%d overflowed\n",
		zone->name, zone->id);
	MR_fatal_abort(context, buf, MR_TRUE);
    }

    return MR_FALSE;
#endif
} 

void
MR_setup_signals(void)
{
/*
** When using Microsoft Visual C structured exceptions don't set any
** signal handlers.
** See mercury_wrapper.c for the reason why.
*/
#ifndef MR_MSVC_STRUCTURED_EXCEPTIONS
  #ifdef SIGBUS
	MR_setup_signal(SIGBUS, (MR_Code *) bus_handler, MR_TRUE,
		"cannot set SIGBUS handler");
  #endif
	MR_setup_signal(SIGSEGV, (MR_Code *) segv_handler, MR_TRUE,
		"cannot set SIGSEGV handler");
#endif
}

static char *
explain_context(void *the_context)
{
	static	char	buf[100];

#if defined(MR_HAVE_SIGCONTEXT_STRUCT)

  #ifdef MR_PC_ACCESS
	struct sigcontext_struct *context = the_context;
	void *pc_at_signal = (void *) context->MR_PC_ACCESS;

	sprintf(buf, "PC at signal: %ld (%lx)\n",
		(long)pc_at_signal, (long)pc_at_signal);
  #else
	buf[0] = '\0';
  #endif

#elif defined(MR_HAVE_SIGINFO_T)

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

  #else /* not MR_PC_ACCESS */

	/* if MR_PC_ACCESS is not set, we don't know the context */
	/* therefore we return an empty string to be printed  */
	buf[0] = '\0';

  #endif /* not MR_PC_ACCESS */

#else /* not MR_HAVE_SIGINFO_T && not MR_HAVE_SIGCONTEXT_STRUCT */

	buf[0] = '\0';

#endif

	return buf;
}

#if defined(MR_HAVE_SIGCONTEXT_STRUCT)
  #if defined(MR_HAVE_SIGCONTEXT_STRUCT_3ARG)
    static void
    complex_sighandler_3arg(int sig, int code,
		    struct sigcontext_struct sigcontext)
  #else
    static void
    complex_sighandler(int sig, struct sigcontext_struct sigcontext)
  #endif
{
	void *address = (void *) MR_GET_FAULT_ADDR(sigcontext);
  #ifdef MR_PC_ACCESS
	void *pc_at_signal = (void *) sigcontext.MR_PC_ACCESS;
  #endif

	switch(sig) {
		case SIGSEGV:
			/*
			** If we're debugging, print the segv explanation
			** messages before we call try_munprotect.  But if
			** we're not debugging, only print them if
			** try_munprotect fails.
			*/
			if (MR_memdebug) {
				fflush(stdout);
				fprintf(stderr, "\n*** Mercury runtime: "
					"caught segmentation violation ***\n");
			}
			if (try_munprotect(address, &sigcontext)) {
				if (MR_memdebug) {
					fprintf(stderr, "returning from "
						"signal handler\n\n");
				}
				return;
			}
			if (!MR_memdebug) {
				fflush(stdout);
				fprintf(stderr, "\n*** Mercury runtime: "
					"caught segmentation violation ***\n");
			}
			break;

#ifdef SIGBUS
		case SIGBUS:
			fflush(stdout);
			fprintf(stderr, "\n*** Mercury runtime: "
					"caught bus error ***\n");
			break;
#endif

		default:
			fflush(stdout);
			fprintf(stderr, "\n*** Mercury runtime: "
					"caught unknown signal %d ***\n", sig);
			break;
	}

  #ifdef MR_PC_ACCESS
	fprintf(stderr, "PC at signal: %ld (%lx)\n",
		(long) pc_at_signal, (long) pc_at_signal);
  #endif
	fprintf(stderr, "address involved: %p\n", address);

	MR_trace_report(stderr);
	print_dump_stack();
	MR_dump_prev_locations();
	fprintf(stderr, "exiting from signal handler\n");
	exit(1);
} /* end complex_sighandler() */


#elif defined(MR_HAVE_SIGINFO_T)

static void 
complex_bushandler(int sig, siginfo_t *info, void *context)
{
	fflush(stdout);

	if (sig != SIGBUS || !info || info->si_signo != SIGBUS) {
		fprintf(stderr, "\n*** Mercury runtime: ");
		fprintf(stderr, "caught strange bus error ***\n");
		exit(1);
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

		} /* end switch */

		fprintf(stderr, "%s", explain_context(context));
		fprintf(stderr, "address involved: %p\n",
			(void *) info->si_addr);
	} /* end if */

	MR_trace_report(stderr);
	print_dump_stack();
	MR_dump_prev_locations();
	fprintf(stderr, "exiting from signal handler\n");
	exit(1);
} /* end complex_bushandler() */

static void 
explain_segv(siginfo_t *info, void *context)
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

		fprintf(stderr, "%s", explain_context(context));
		fprintf(stderr, "address involved: %p\n",
			(void *) info->si_addr);

	} /* end if */
} /* end explain_segv() */

static void 
complex_segvhandler(int sig, siginfo_t *info, void *context)
{
	if (sig != SIGSEGV || !info || info->si_signo != SIGSEGV) {
		fprintf(stderr, "\n*** Mercury runtime: ");
		fprintf(stderr, "caught strange segmentation violation ***\n");
		exit(1);
	}

	/*
	** If we're debugging, print the segv explanation messages
	** before we call try_munprotect.  But if we're not debugging,
	** only print them if try_munprotect fails.
	*/

	if (MR_memdebug) {
		explain_segv(info, context);
	}

	if (try_munprotect(info->si_addr, context)) {
		if (MR_memdebug) {
			fprintf(stderr, "returning from signal handler\n\n");
		}

		return;
	}

	if (!MR_memdebug) {
		explain_segv(info, context);
	}

	MR_trace_report(stderr);
	print_dump_stack();
	MR_dump_prev_locations();
	fprintf(stderr, "exiting from signal handler\n");
	exit(1);
} /* end complex_segvhandler */

#else /* not MR_HAVE_SIGINFO_T && not MR_HAVE_SIGCONTEXT_STRUCT */

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

	print_dump_stack();
	MR_dump_prev_locations();
	fprintf(stderr, "exiting from signal handler\n");
	exit(1);
}

#endif /* not MR_HAVE_SIGINFO_T && not MR_HAVE_SIGCONTEXT_STRUCT */

#ifdef MR_MSVC_STRUCTURED_EXCEPTIONS
static const char *MR_find_exception_name(DWORD exception_code);
static void MR_explain_exception_record(EXCEPTION_RECORD *rec);
static void MR_dump_exception_record(EXCEPTION_RECORD *rec);
static MR_bool MR_exception_record_is_access_violation(EXCEPTION_RECORD *rec,
		void **address_ptr, int *access_mode_ptr);

/*
** Exception code and their string representation
*/
#define DEFINE_EXCEPTION_NAME(a)   {a,#a}

typedef struct
{
	DWORD		exception_code;
	const char	*exception_name;
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


/*
** Retrieve the name of a Win32 exception code as a string
*/
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

/*
** Was a page accessed read/write?  The MSDN documentation doens't define
** symbolic constants for these alternatives.
*/
#define READ	0
#define WRITE	1

/*
** Explain an EXCEPTION_RECORD content into stderr.
*/
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
		
		/* If the exception is an access violation */
		if (MR_exception_record_is_access_violation(rec,
					&address, &access_mode))
		{
			MR_MemoryZone *zone;

			/* Display AV address and access mode */
			fprintf(stderr, "\n***   An access violation occured"
					" at address 0x%08lx, while attempting"
					" to ", (unsigned long) address);
			
			if (access_mode == READ) {
				fprintf(stderr, "\n***   read "
						"inaccessible data");
			} else if (access_mode == WRITE) {
				fprintf(stderr, "\n***   write to an "
						"inaccessible (or protected)"
						" address");
			} else {
				fprintf(stderr, "\n***   ? [unknown access "
						"mode %d (strange...)]",
						access_mode);
			}
				
			fprintf(stderr, "\n***   Trying to see if this "
					"stands within a mercury zone...");
			/*
			** Browse the mercury memory zones to see if the
			** AV address references one of them.
			*/
			zone = MR_get_used_memory_zones();
			while(zone != NULL) {
				fprintf(stderr,
						"\n***    Checking zone %s#%d: "
						"0x%08lx - 0x%08lx - 0x%08lx",
						zone->name, zone->id,
						(unsigned long) zone->bottom,
						(unsigned long) zone->redzone,
						(unsigned long) zone->top);

				if ((zone->redzone <= address) &&
						(address <= zone->top))
				{
					fprintf(stderr,
						"\n***     Address is within"
						" redzone of "
						"%s#%d (!!zone overflowed!!)\n",
						zone->name, zone->id);
				} else if ((zone->bottom <= address) &&
						(address <= zone->top))
				{
					fprintf(stderr, "\n***     Address is"
							" within zone %s#%d\n",
							zone->name, zone->id);
				}
				/*
				** Don't need to call handler, because it
				** has much less information than we do.
				*/
				/* return zone->handler(fault_addr,
				 		zone, rec); */
				zone = zone->next;
			}
		}
		return;
	}
}

/*
** Dump an EXCEPTION_RECORD content into stderr.
*/
static void
MR_dump_exception_record(EXCEPTION_RECORD *rec)
{
	int i;
	
	if (rec == NULL) {
		return;
	}
	
	fprintf(stderr, "\n***   Exception record at 0x%08lx:",
			(unsigned long) rec);
	fprintf(stderr, "\n***    MR_Code        : 0x%08lx (%s)",
			(unsigned long) rec->ExceptionCode,
			MR_find_exception_name(rec->ExceptionCode));
	fprintf(stderr, "\n***    Flags       : 0x%08lx",
			(unsigned long) rec->ExceptionFlags);
	fprintf(stderr, "\n***    Address     : 0x%08lx",
			(unsigned long) rec->ExceptionAddress);

	for (i = 0; i < rec->NumberParameters; i++) {
		fprintf(stderr, "\n***    Parameter %d : 0x%08lx", i,
				(unsigned long) rec->ExceptionInformation[i]);
	}
	fprintf(stderr, "\n***    Next record : 0x%08lx",
			(unsigned long) rec->ExceptionRecord);
	
	/* Try to explain the exception more "gracefully" */
	MR_explain_exception_record(rec);
	MR_dump_exception_record(rec->ExceptionRecord);
}


/*
** Return MR_TRUE iff exception_ptrs indicates an access violation.
** If MR_TRUE, the dereferenced address_ptr is set to the accessed address and
** the dereferenced access_mode_ptr is set to the desired access
** (0 = read, 1 = write)
*/
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


/*
** Filter a Win32 exception (to be called in the __except filter part).
** Possible return values are:
**
** EXCEPTION_CONTINUE_EXECUTION (-1)
**  Exception is dismissed. Continue execution at the point where
**  the exception occurred.
**
** EXCEPTION_CONTINUE_SEARCH (0)
**  Exception is not recognized. Continue to search up the stack for
**  a handler, first for containing try-except statements, then for
**  handlers with the next highest precedence.
**
** EXCEPTION_EXECUTE_HANDLER (1)
**  Exception is recognized. Transfer control to the exception handler
**  by executing the __except compound statement, then continue
**  execution at the assembly instruction that was executing
**  when the exception was raised. 
*/
int
MR_filter_win32_exception(LPEXCEPTION_POINTERS exception_ptrs)
{
	void *address;
	int access_mode;

		/* If the exception is an access violation */
	if (MR_exception_record_is_access_violation(
			exception_ptrs->ExceptionRecord,
			&address, &access_mode))
	{

			/* If we can unprotect the memory zone */
		if (try_munprotect(address, exception_ptrs)) {
			if (MR_memdebug) {
				fprintf(stderr, "returning from "
						"signal handler\n\n");
			}
				/* Continue execution where it stopped */
			return  EXCEPTION_CONTINUE_EXECUTION;
		}
	}
		
	/*
	** We can't handle the exception. Just dump all the information we got
	*/
	fflush(stdout);
	fprintf(stderr, "\n*** Mercury runtime: Unhandled exception ");
	MR_dump_exception_record(exception_ptrs->ExceptionRecord);

	printf("\n");
	print_dump_stack();
	MR_dump_prev_locations();
	
	fprintf(stderr, "\n\n*** Now passing exception to default handler\n\n");
	fflush(stderr);
		  
	/*
	** Pass exception back to upper handler. In most cases, this
	** means activating UnhandledExceptionFilter, which will display
	** a dialog box asking to user ro activate the Debugger or simply
	** to kill the application
	*/
	return  EXCEPTION_CONTINUE_SEARCH;
}
#endif /* MR_MSVC_STRUCTURED_EXCEPTIONS */


/*
** get_pc_from_context:
** 	Given the signal context, return the program counter at the time
** 	of the signal, if available.  If it is unavailable, return NULL.
*/
static MR_Code *
get_pc_from_context(void *the_context)
{
	MR_Code *pc_at_signal = NULL;
#if defined(MR_HAVE_SIGCONTEXT_STRUCT)

  #ifdef MR_PC_ACCESS
	struct sigcontext_struct *context = the_context;

	pc_at_signal = (MR_Code *) context->MR_PC_ACCESS;
  #else
	pc_at_signal = (MR_Code *) NULL;
  #endif

#elif defined(MR_HAVE_SIGINFO_T)

  #ifdef MR_PC_ACCESS

	ucontext_t *context = the_context;

    #ifdef MR_PC_ACCESS_GREG
	pc_at_signal = (MR_Code *) context->uc_mcontext.gregs[MR_PC_ACCESS];
    #else
	pc_at_signal = (MR_Code *) context->uc_mcontext.MR_PC_ACCESS;
    #endif

  #else /* not MR_PC_ACCESS */

	/* if MR_PC_ACCESS is not set, we don't know the context */
	pc_at_signal = (MR_Code *) NULL;

  #endif /* not MR_PC_ACCESS */

#else /* not MR_HAVE_SIGINFO_T && not MR_HAVE_SIGCONTEXT_STRUCT */

	pc_at_signal = (MR_Code *) NULL;

#endif

	return pc_at_signal;
}

/*
** get_sp_from_context:
** 	Given the signal context, return the Mercury register "MR_sp" at
** 	the time of the signal, if available.  If it is unavailable,
** 	return NULL.
**
** XXX We only define this function in LLDS accurate gc grades for the moment,
** because it's unlikely to compile everywhere.  It relies on
** MR_real_reg_number_sp being defined, which is the name/number of the
** machine register that is used for MR_sp.
** Need to fix this so it works when the register is in a fake reg too.
*/
static MR_Word *
get_sp_from_context(void *the_context)
{
	MR_Word *sp_at_signal = NULL;
#if defined(MR_NATIVE_GC) && !defined(MR_HIGHLEVEL_CODE)
  #if defined(MR_HAVE_SIGCONTEXT_STRUCT)

    #ifdef MR_PC_ACCESS
	struct sigcontext_struct *context = the_context;

	sp_at_signal = (MR_Word *) context->MR_real_reg_number_sp;
    #else
	sp_at_signal = (MR_Word *) NULL;
    #endif

  #elif defined(MR_HAVE_SIGINFO_T)

    #ifdef MR_PC_ACCESS

	struct sigcontext *context = the_context;

      #ifdef MR_PC_ACCESS_GREG
	sp_at_signal = (MR_Word *) context->gregs[MR_real_reg_number_sp];
      #else
	sp_at_signal = (MR_Word *) context->sc_regs[MR_real_reg_number_sp];
      #endif

    #else /* not MR_PC_ACCESS */

	/* 
	** if MR_PC_ACCESS is not set, we don't know how to get at the
	** registers
	*/
	sp_at_signal = (MR_Word *) NULL;

    #endif /* not MR_PC_ACCESS */

  #else /* not MR_HAVE_SIGINFO_T && not MR_HAVE_SIGCONTEXT_STRUCT */

	sp_at_signal = (MR_Word *) NULL;

  #endif
#else /* !MR_NATIVE_GC */
	sp_at_signal = (MR_Word *) NULL;
#endif /* !MR_NATIVE_GC */

	return sp_at_signal;
}

/*
** get_sp_from_context:
** 	Given the signal context, return the Mercury register "MR_sp" at
** 	the time of the signal, if available.  If it is unavailable,
** 	return NULL.
**
** XXX We only define this function in accurate gc grades for the moment,
** because it's unlikely to compile everywhere.  It relies on
** MR_real_reg_number_sp being defined, which is the name/number of the
** machine register that is used for MR_sp.
** Need to fix this so it works when the register is in a fake reg too.
*/
static MR_Word *
get_curfr_from_context(void *the_context)
{
	MR_Word *curfr_at_signal;
	
	/*
	** XXX this is implementation dependent, need a better way
	** to do register accesses at signals.
	**
	** It's in mr8 or mr9 which is in the fake regs on some architectures,
	** and is a machine register on others.
	** So don't run the garbage collector on those archs.
	*/

	curfr_at_signal = MR_curfr;

	return curfr_at_signal;
}

static void 
print_dump_stack(void)
{
	const char *msg =
		"This may have been caused by a stack overflow, due to unbounded recursion.\n";
	write(STDERR, msg, strlen(msg));
}
