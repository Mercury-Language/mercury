/*
** Copyright (C) 1998 The University of Melbourne.
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

#include <unistd.h>
#include <stdio.h>
#include <string.h>

#ifdef HAVE_SIGCONTEXT_STRUCT
  /*
  ** Some versions of Linux call it struct sigcontext_struct, some call it
  ** struct sigcontext.  The following #define eliminates the differences.
  */
  #define sigcontext_struct sigcontext /* must be before #include <signal.h> */

  /*
  ** On some systems (e.g. most versions of Linux) we need to #define
  ** __KERNEL__ to get sigcontext_struct from <signal.h>.
  ** This stuff must come before anything else that might include <signal.h>,
  ** otherwise the #define __KERNEL__ may not work.
  */
  #define __KERNEL__
  #include <signal.h>	/* must come third */
  #undef __KERNEL__

  /*
  ** Some versions of Linux define it in <signal.h>, others define it in
  ** <asm/sigcontext.h>.  We try both.
  */
  #ifdef HAVE_ASM_SIGCONTEXT
    #include <asm/sigcontext.h>
  #endif 
#else
  #include <signal.h>
#endif

#ifdef HAVE_SYS_SIGINFO
  #include <sys/siginfo.h>
#endif 

#ifdef	HAVE_MPROTECT
  #include <sys/mman.h>
#endif

#ifdef	HAVE_UCONTEXT
  #include <ucontext.h>
#endif

#ifdef	HAVE_SYS_UCONTEXT
  #include <sys/ucontext.h>
#endif

#include "mercury_imp.h"
#include "mercury_signal.h"
#include "mercury_trace.h"
#include "mercury_memory_zones.h"
#include "mercury_memory_handlers.h"

/*---------------------------------------------------------------------------*/

#ifdef HAVE_SIGINFO
  #if defined(HAVE_SIGCONTEXT_STRUCT)
    static	void	complex_sighandler(int, struct sigcontext_struct);
  #elif defined(HAVE_SIGINFO_T)
    static	void	complex_bushandler(int, siginfo_t *, void *);
    static	void	complex_segvhandler(int, siginfo_t *, void *);
  #else
    #error "HAVE_SIGINFO defined but don't know how to get it"
  #endif
#else
  static	void	simple_sighandler(int);
#endif


#ifdef HAVE_SIGINFO
  #if defined(HAVE_SIGCONTEXT_STRUCT)
    #define     bus_handler	complex_sighandler
    #define     segv_handler	complex_sighandler
  #elif defined(HAVE_SIGINFO_T)
    #define     bus_handler	complex_bushandler
    #define     segv_handler	complex_segvhandler
  #else
    #error "HAVE_SIGINFO defined but don't know how to get it"
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
static	bool	try_munprotect(void *address, void *context);
static	char	*explain_context(void *context);
static	Code	*get_pc_from_context(void *the_context);
static	Word	*get_sp_from_context(void *the_context);

#define STDERR 2


static bool 
try_munprotect(void *addr, void *context)
{
#ifndef HAVE_SIGINFO
	return FALSE;
#else
	Word *    fault_addr;
	Word *    new_zone;
	MemoryZone *zone;

	fault_addr = (Word *) addr;

	zone = get_used_memory_zones();

	if (memdebug) {
		fprintf(stderr, "caught fault at %p\n", (void *)addr);
	}

	while(zone != NULL) {
		if (memdebug) {
			fprintf(stderr, "checking %s#%d: %p - %p\n",
				zone->name, zone->id, (void *) zone->redzone,
				(void *) zone->top);
		}

		if (zone->redzone <= fault_addr && fault_addr <= zone->top) {

			if (memdebug) {
				fprintf(stderr, "address is in %s#%d redzone\n",
					zone->name, zone->id);
			}

			return zone->handler(fault_addr, zone, context);
		}
		zone = zone->next;
	}

	if (memdebug) {
		fprintf(stderr, "address not in any redzone.\n");
	}

	return FALSE;
#endif /* HAVE_SIGINFO */
} 

bool 
null_handler(Word *fault_addr, MemoryZone *zone, void *context)
{
	return FALSE;
}

/*
** fatal_abort() prints an error message, possibly a stack dump, and then exits.
** It is like fatal_error(), except that it is safe to call
** from a signal handler.
*/

static void 
fatal_abort(void *context, const char *main_msg, int dump)
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

bool 
default_handler(Word *fault_addr, MemoryZone *zone, void *context)
{
#ifndef MR_CHECK_OVERFLOW_VIA_MPROTECT
	return FALSE;
#else
    Word *new_zone;
    size_t zone_size;

    new_zone = (Word *) round_up((Unsigned) fault_addr + sizeof(Word), unit);

    if (new_zone <= zone->hardmax) {
	zone_size = (char *)new_zone - (char *)zone->redzone;

	if (memdebug) {
	    fprintf(stderr, "trying to unprotect %s#%d from %p to %p (%x)\n",
	    zone->name, zone->id, (void *) zone->redzone, (void *) new_zone,
	    (int)zone_size);
	}
	if (mprotect((char *)zone->redzone, zone_size,
	    PROT_READ|PROT_WRITE) < 0)
	{
	    char buf[2560];
	    sprintf(buf, "Mercury runtime: cannot unprotect %s#%d zone",
		zone->name, zone->id);
	    perror(buf);
	    exit(1);
	}

	zone->redzone = new_zone;

	if (memdebug) {
	    fprintf(stderr, "successful: %s#%d redzone now %p to %p\n",
		zone->name, zone->id, (void *) zone->redzone,
		(void *) zone->top);
	}
  #ifdef NATIVE_GC
	MR_schedule_agc(get_pc_from_context(context),
		get_sp_from_context(context));
  #endif
	return TRUE;
    } else {
	char buf[2560];
	if (memdebug) {
	    fprintf(stderr, "can't unprotect last page of %s#%d\n",
		zone->name, zone->id);
	    fflush(stdout);
	}
	sprintf(buf, "\nMercury runtime: memory zone %s#%d overflowed\n",
		zone->name, zone->id);
	fatal_abort(context, buf, TRUE);
    }

    return FALSE;
#endif
} 

void
setup_signals(void)
{
	MR_setup_signal(SIGBUS, (Code *) bus_handler, TRUE,
		"Mercury runtime: cannot set SIGBUS handler");
	MR_setup_signal(SIGSEGV, (Code *) segv_handler, TRUE,
		"Mercury runtime: cannot set SIGSEGV handler");
}

static char *
explain_context(void *the_context)
{
	static	char	buf[100];

#if defined(HAVE_SIGCONTEXT_STRUCT)

  #ifdef PC_ACCESS
	struct sigcontext_struct *context = the_context;
	void *pc_at_signal = (void *) context->PC_ACCESS;

	sprintf(buf, "PC at signal: %ld (%lx)\n",
		(long)pc_at_signal, (long)pc_at_signal);
  #else
	buf[0] = '\0';
  #endif

#elif defined(HAVE_SIGINFO_T)

  #ifdef PC_ACCESS

	ucontext_t *context = the_context;

    #ifdef PC_ACCESS_GREG
	sprintf(buf, "PC at signal: %ld (%lx)\n",
		(long) context->uc_mcontext.gregs[PC_ACCESS],
		(long) context->uc_mcontext.gregs[PC_ACCESS]);
    #else
	sprintf(buf, "PC at signal: %ld (%lx)\n",
		(long) context->uc_mcontext.PC_ACCESS,
		(long) context->uc_mcontext.PC_ACCESS);
    #endif

  #else /* not PC_ACCESS */

	/* if PC_ACCESS is not set, we don't know the context */
	/* therefore we return an empty string to be printed  */
	buf[0] = '\0';

  #endif /* not PC_ACCESS */

#else /* not HAVE_SIGINFO_T && not HAVE_SIGCONTEXT_STRUCT */

	buf[0] = '\0';

#endif

	return buf;
}

#if defined(HAVE_SIGCONTEXT_STRUCT)

static void
complex_sighandler(int sig, struct sigcontext_struct sigcontext)
{
	void *address = (void *) sigcontext.cr2;
  #ifdef PC_ACCESS
	void *pc_at_signal = (void *) sigcontext.PC_ACCESS;
  #endif

	switch(sig) {
		case SIGSEGV:
			/*
			** If we're debugging, print the segv explanation
			** messages before we call try_munprotect.  But if
			** we're not debugging, only print them if
			** try_munprotect fails.
			*/
			if (memdebug) {
				fflush(stdout);
				fprintf(stderr, "\n*** Mercury runtime: "
					"caught segmentation violation ***\n");
			}
			if (try_munprotect(address, &sigcontext)) {
				if (memdebug) {
					fprintf(stderr, "returning from "
						"signal handler\n\n");
				}
				return;
			}
			if (!memdebug) {
				fflush(stdout);
				fprintf(stderr, "\n*** Mercury runtime: "
					"caught segmentation violation ***\n");
			}
			break;

		case SIGBUS:
			fflush(stdout);
			fprintf(stderr, "\n*** Mercury runtime: "
					"caught bus error ***\n");
			break;

		default:
			fflush(stdout);
			fprintf(stderr, "\n*** Mercury runtime: "
					"caught unknown signal %d ***\n", sig);
			break;
	}

  #ifdef PC_ACCESS
	fprintf(stderr, "PC at signal: %ld (%lx)\n",
		(long) pc_at_signal, (long) pc_at_signal);
  #endif
	fprintf(stderr, "address involved: %p\n", address);

	MR_trace_report(stderr);
	print_dump_stack();
	dump_prev_locations();
	fprintf(stderr, "exiting from signal handler\n");
	exit(1);
} /* end complex_sighandler() */


#elif defined(HAVE_SIGINFO_T)

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
		case BUS_ADRALN:
			fprintf(stderr, "invalid address alignment\n");
			break;

		case BUS_ADRERR:
			fprintf(stderr, "non-existent physical address\n");
			break;

		case BUS_OBJERR:
			fprintf(stderr, "object specific hardware error\n");
			break;

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
	dump_prev_locations();
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
		case SEGV_MAPERR:
			fprintf(stderr, "address not mapped to object\n");
			break;

		case SEGV_ACCERR:
			fprintf(stderr, "bad permissions for mapped object\n");
			break;

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

	if (memdebug) {
		explain_segv(info, context);
	}

	if (try_munprotect(info->si_addr, context)) {
		if (memdebug) {
			fprintf(stderr, "returning from signal handler\n\n");
		}

		return;
	}

	if (!memdebug) {
		explain_segv(info, context);
	}

	MR_trace_report(stderr);
	print_dump_stack();
	dump_prev_locations();
	fprintf(stderr, "exiting from signal handler\n");
	exit(1);
} /* end complex_segvhandler */

#else /* not HAVE_SIGINFO_T && not HAVE_SIGCONTEXT_STRUCT */

static void 
simple_sighandler(int sig)
{
	fflush(stdout);
	fprintf(stderr, "*** Mercury runtime: ");

	switch (sig)
	{
	case SIGBUS:
		fprintf(stderr, "caught bus error ***\n");
		break;

	case SIGSEGV:
		fprintf(stderr, "caught segmentation violation ***\n");
		break;

	default:
		fprintf(stderr, "caught unknown signal %d ***\n", sig);
		break;
	}

	print_dump_stack();
	dump_prev_locations();
	fprintf(stderr, "exiting from signal handler\n");
	exit(1);
}

#endif /* not HAVE_SIGINFO_T && not HAVE_SIGCONTEXT_STRUCT */

/*
** get_pc_from_context:
** 	Given the signal context, return the program counter at the time
** 	of the signal, if available.  If it is unavailable, return NULL.
*/
static Code *
get_pc_from_context(void *the_context)
{
	Code *pc_at_signal = NULL;
#if defined(HAVE_SIGCONTEXT_STRUCT)

  #ifdef PC_ACCESS
	struct sigcontext_struct *context = the_context;

	pc_at_signal = (Code *) context->PC_ACCESS;
  #else
	pc_at_signal = (Code *) NULL;
  #endif

#elif defined(HAVE_SIGINFO_T)

  #ifdef PC_ACCESS

	struct sigcontext *context = the_context;

    #ifdef PC_ACCESS_GREG
	pc_at_signal = (Code *) context->gregs[PC_ACCESS];
    #else
	pc_at_signal = (Code *) context->PC_ACCESS;
    #endif

  #else /* not PC_ACCESS */

	/* if PC_ACCESS is not set, we don't know the context */
	pc_at_signal = (Code *) NULL;

  #endif /* not PC_ACCESS */

#else /* not HAVE_SIGINFO_T && not HAVE_SIGCONTEXT_STRUCT */

	pc_at_signal = (Code *) NULL;

#endif

	return pc_at_signal;
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
static Word *
get_sp_from_context(void *the_context)
{
	Word *sp_at_signal = NULL;
#ifdef NATIVE_GC
  #if defined(HAVE_SIGCONTEXT_STRUCT)

    #ifdef PC_ACCESS
	struct sigcontext_struct *context = the_context;

	sp_at_signal = (Word *) context->MR_real_reg_number_sp;
    #else
	sp_at_signal = (Word *) NULL;
    #endif

  #elif defined(HAVE_SIGINFO_T)

    #ifdef PC_ACCESS

	struct sigcontext *context = the_context;

      #ifdef PC_ACCESS_GREG
	sp_at_signal = (Word *) context->gregs[MR_real_reg_number_sp];
      #else
	sp_at_signal = (Word *) context->sc_regs[MR_real_reg_number_sp];
      #endif

    #else /* not PC_ACCESS */

	/* 
	** if PC_ACCESS is not set, we don't know how to get at the
	** registers
	*/
	sp_at_signal = (Word *) NULL;

    #endif /* not PC_ACCESS */

  #else /* not HAVE_SIGINFO_T && not HAVE_SIGCONTEXT_STRUCT */

	sp_at_signal = (Word *) NULL;

  #endif
#else /* !NATIVE_GC */
	sp_at_signal = (Word *) NULL;
#endif /* !NATIVE_GC */

	return sp_at_signal;
}

static void 
print_dump_stack(void)
{

#ifndef	MR_LOWLEVEL_DEBUG

	const char *msg =
		"You can get a stack dump by using `--low-level-debug'\n";
	write(STDERR, msg, strlen(msg));

#else /* MR_LOWLEVEL_DEBUG */
	int	i;
	int	start;
	int	count;
	char	buf[2560];

	strcpy(buf, "A dump of the det stack follows\n\n");
	write(STDERR, buf, strlen(buf));

	i = 0;
	while (i < dumpindex) {
		start = i;
		count = 1;
		i++;

		while (i < dumpindex &&
			strcmp(((char **)(dumpstack_zone->min))[i],
				((char **)(dumpstack_zone->min))[start]) == 0)
		{
			count++;
			i++;
		}

		if (count > 1) {
			sprintf(buf, "%s * %d\n",
				((char **)(dumpstack_zone->min))[start], count);
		} else {
			sprintf(buf, "%s\n",
				((char **)(dumpstack_zone->min))[start]);
		}

		write(STDERR, buf, strlen(buf));
	} /* end while */

	strcpy(buf, "\nend of stack dump\n");
	write(STDERR, buf, strlen(buf));

#endif /* MR_LOWLEVEL_DEBUG */

} /* end print_dump_stack() */


