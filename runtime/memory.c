/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module defines the register array and data regions of the
** execution algorithm.
** They are defined together here to allow us to control how they map
** onto direct mapped caches.
** We allocate a large arena, preferably aligned on a boundary that
** is a multiple of both the page size and the primary cache size.
**
** We then allocate the heap and the stacks in such a way that
**
**	the register array 
**	the bottom of the heap 
**	the bottom of the detstack 
**	the bottom of the nondstack 
**
** all start at different offsets from the primary cache size.
** This should reduce cache conflicts (especially for small programs).
**
** If the operating system of the machine supports the mprotect syscall,
** we also protect a chunk at the end of each area against access,
** thus detecting area overflow.
*/

/*---------------------------------------------------------------------------*/

#include "imp.h"
#include "conf.h"

#include <unistd.h>

#ifdef	HAVE_SIGINFO
#include	<sys/siginfo.h>
#endif 

#include <stdio.h>
#include <string.h>

#ifdef	HAVE_MPROTECT
#include <sys/mman.h>
#endif

#include <signal.h>

#ifdef	HAVE_UCONTEXT
#include <ucontext.h>
#endif
#ifdef	HAVE_SYS_UCONTEXT
#include <sys/ucontext.h>
#endif

#if defined(HAVE_SYSCONF) && defined(_SC_PAGESIZE)
#define	getpagesize()	sysconf(_SC_PAGESIZE)
#else
#ifndef	HAVE_GETPAGESIZE
#define	getpagesize()	8192
#endif
#endif

#ifdef	CONSERVATIVE_GC
#define memalign(a,s)   GC_MALLOC_UNCOLLECTABLE(s)
#else
#ifdef	HAVE_MEMALIGN
extern	void	*memalign(size_t, size_t);
#else
#define	memalign(a,s)	malloc(s)
#endif
#endif

/*---------------------------------------------------------------------------*/

#ifdef	HAVE_SIGINFO
static	void	complex_bushandler(int, siginfo_t *, void *);
static	void	complex_segvhandler(int, siginfo_t *, void *);
#else
static	void	simple_sighandler(int);
#endif

/*
** round_up(amount, align) returns `amount' rounded up to the nearest
** alignment boundary.  `align' must be a power of 2.
*/

#define round_up(amount, align) ((((amount) - 1) | ((align) - 1)) + 1)

static	void	setup_mprotect(void);
#ifdef	HAVE_SIGINFO
static	bool	try_munprotect(void *);
#endif

static	void	setup_signal(void);

Word	fake_reg[MAX_FAKE_REG];

Word	virtual_reg_map[MAX_REAL_REG] = VIRTUAL_REG_MAP_BODY;

Word	num_uses[MAX_RN];

Word	*heap;
Word	*detstack;
Word	*nondstack;

Word	*heapmin;
Word	*detstackmin;
Word	*nondstackmin;

Word	*heapmax;
Word	*detstackmax;
Word	*nondstackmax;

Word	*heapend;
Word	*detstackend;
Word	*nondstackend;

char *	heap_zone;
char *	detstack_zone;
char *	nondstack_zone;

int	heap_zone_left = 0;
int	detstack_zone_left = 0;
int	nondstack_zone_left = 0;

static	unsigned	unit;
static	unsigned	page_size;

void init_memory(void)
{
	char		*arena;
	unsigned	total_size;
	unsigned	fake_reg_offset, heap_offset,
			detstack_offset, nondstack_offset;

	/*
	** Convert all the sizes are from kilobytes to bytes and
	** make sure they are multiples of the page and cache sizes.
	*/

	page_size = getpagesize();
	unit = max(page_size, pcache_size);

#ifdef CONSERVATIVE_GC
	heap_zone_size      = 0;
	heap_size	    = 0;
#else
	heap_zone_size      = round_up(heap_zone_size * 1024, unit);
	heap_size           = round_up(heap_size * 1024, unit);
#endif

	detstack_size       = round_up(detstack_size * 1024, unit);
	detstack_zone_size  = round_up(detstack_zone_size * 1024, unit);
	nondstack_size      = round_up(nondstack_size * 1024, unit);
	nondstack_zone_size = round_up(nondstack_zone_size * 1024, unit);

	/*
	** If the zone sizes were set to something too big, then
	** set them to a single unit.
	*/

#ifndef CONSERVATIVE_GC
	if (heap_zone_size >= heap_size)
		heap_zone_size = unit;
#endif

	if (detstack_zone_size >= detstack_size)
		detstack_zone_size = unit;

	if (nondstack_zone_size >= nondstack_size)
		nondstack_zone_size = unit;


	/*
	** Calculate how much memory to allocate, then allocate it
	** and divide it up among the areas.
	** We allocate 4 extra units, since we waste one unit each
	** for the heap, detstack, and nondstack to ensure they
	** are aligned at non-conflicting cache offsets, and we may
	** waste one unit aligning the whole arena on a unit boundary.
	*/

	total_size = heap_size + detstack_size + nondstack_size + 4 * unit;

	/*  get mem_size pages aligned on a page boundary */
	arena = memalign(unit, total_size);
	if (arena == NULL)
	{
		perror("Mercury runtime");
		fprintf(stderr, "cannot allocate arena: memalign() failed\n");
		exit(1);
	}
	arena = (char *) round_up((Unsigned) arena, unit);
	
	fake_reg_offset = (Unsigned) fake_reg % pcache_size;
	heap_offset = (fake_reg_offset + pcache_size / 4) % pcache_size;
	detstack_offset = (heap_offset + pcache_size / 4) % pcache_size;
	nondstack_offset = (detstack_offset + pcache_size / 4) % pcache_size;

#ifdef CONSERVATIVE_GC
	heap    = 0;
	heapmin = 0;
	heapend = 0;
#else
	heap    = (Word *) arena;
	heapmin = (Word *) ((char *) heap + heap_offset);
	heapend = (Word *) ((char *) heap + heap_size + unit);
	assert(((Unsigned) heapend) % unit == 0);
#endif

#ifdef CONSERVATIVE_GC
	detstack    = (Word *) arena;
#else
	detstack    = heapend;
#endif
	detstackmin = (Word *) ((char *) detstack + detstack_offset);
	detstackend = (Word *) ((char *) detstack + detstack_size + unit);
	assert(((Unsigned) detstackend) % unit == 0);

	nondstack    = detstackend;
	nondstackmin = (Word *) ((char *) nondstack + nondstack_offset);
	nondstackend = (Word *) ((char *) nondstack + nondstack_size + unit);
	assert(((Unsigned) nondstackend) % unit == 0);

#ifndef	SPEED
	nondstackmin[PREDNM] = (Word) "bottom";
#endif

	if (arena + total_size <= (char *) nondstackend)
	{
		fprintf(stderr, "Mercury runtime: internal error: "
				"allocated too much memory\n");
		exit(1);
	}

	setup_mprotect();
	setup_signal();

	if (memdebug)
	{
		fprintf(stderr, "\n");
		fprintf(stderr, "pcache_size  = %d (0x%x)\n",
			pcache_size, pcache_size);
		fprintf(stderr, "page_size    = %d (0x%x)\n",
			page_size, page_size);
		fprintf(stderr, "unit         = %d (0x%x)\n",
			unit, unit);

		fprintf(stderr, "\n");
		fprintf(stderr, "fake_reg       = %p (offset %ld)\n",
			(void *) fake_reg, (long) fake_reg & (unit-1));
		fprintf(stderr, "\n");

#ifndef CONSERVATIVE_GC
		fprintf(stderr, "heap           = %p (offset %ld)\n",
			(void *) heap, (long) heap & (unit-1));
		fprintf(stderr, "heapmin        = %p (offset %ld)\n",
			(void *) heapmin, (long) heapmin & (unit-1));
		fprintf(stderr, "heapend        = %p (offset %ld)\n",
			(void *) heapend, (long) heapend & (unit-1));
		fprintf(stderr, "heap_zone      = %p (offset %ld)\n",
			(void *) heap_zone, (long) heap_zone & (unit-1));
#endif

		fprintf(stderr, "\n");
		fprintf(stderr, "detstack       = %p (offset %ld)\n",
			(void *) detstack, (long) detstack & (unit-1));
		fprintf(stderr, "detstackmin    = %p (offset %ld)\n",
			(void *) detstackmin, (long) detstackmin & (unit-1));
		fprintf(stderr, "detstackend    = %p (offset %ld)\n",
			(void *) detstackend, (long) detstackend & (unit-1));
		fprintf(stderr, "detstack_zone  = %p (offset %ld)\n",
			(void *) detstack_zone, (long) detstack_zone & (unit-1));

		fprintf(stderr, "\n");
		fprintf(stderr, "nondstack      = %p (offset %ld)\n",
			(void *) nondstack, (long) nondstack & (unit-1));
		fprintf(stderr, "nondstackmin   = %p (offset %ld)\n",
			(void *) nondstackmin, (long) nondstackmin & (unit-1));
		fprintf(stderr, "nondstackend   = %p (offset %ld)\n",
			(void *) nondstackend, (long) nondstackend & (unit-1));
		fprintf(stderr, "nondstack_zone = %p (offset %ld)\n",
			(void *) nondstack_zone,
			(long) nondstack_zone & (unit-1));

		fprintf(stderr, "\n");
		fprintf(stderr, "arena start    = %p (offset %ld)\n",
			(void *) arena, (long) arena & (unit-1));
		fprintf(stderr, "arena end      = %p (offset %ld)\n",
			(void *) (arena+total_size),
			(long) (arena+total_size) & (unit-1));

		fprintf(stderr, "\n");
#ifndef CONSERVATIVE_GC
		fprintf(stderr, "heap size      = %ld (0x%lx)\n",
			(long) ((char *) heapend - (char *) heapmin),
			(long) ((char *) heapend - (char *) heapmin));
#endif
		fprintf(stderr, "detstack size  = %ld (0x%lx)\n",
			(long) ((char *) detstackend - (char *) detstackmin),
			(long) ((char *) detstackend - (char *) detstackmin));
		fprintf(stderr, "nondstack size = %ld (0x%lx)\n",
			(long) ((char *) nondstackend - (char *) nondstackmin),
			(long) ((char *) nondstackend - (char *) nondstackmin));
		fprintf(stderr, "arena size     = %ld (0x%lx)\n",
			(long) total_size, (long) total_size);
	}
}

#ifdef	HAVE_MPROTECT

/*
** DESCRIPTION
**  The function mprotect() changes the  access  protections  on
**  the mappings specified by the range [addr, addr + len) to be
**  that specified by prot.  Legitimate values for prot are  the
**  same  as  those  permitted  for  mmap  and  are  defined  in
**  <sys/mman.h> as:
**
** PROT_READ    page can be read 
** PROT_WRITE   page can be written
** PROT_EXEC    page can be executed
** PROT_NONE    page can not be accessed
*/

#ifdef CONSERVATIVE_GC
	/*
	** The conservative garbage collectors scans through
	** all these areas, so we need to allow reads.
	** XXX This probably causes efficiency problems:
	** too much memory for the GC to scan, and it probably
	** all gets paged in.
	*/
#define MY_PROT PROT_READ
#else
#define MY_PROT PROT_NONE
#endif

/* The BSDI BSD/386 1.1 headers don't define PROT_NONE */
#ifndef PROT_NONE
#define PROT_NONE 0
#endif

static void setup_mprotect(void)
{
	heap_zone_left = heap_zone_size;
	heap_zone = (char *) (heapend) - heap_zone_size;
	if (heap_zone_size > 0
	&& mprotect(heap_zone, heap_zone_size, MY_PROT) < 0)
	{
		if (memdebug)
			perror("Mercury runtime: warning: "
				"cannot protect heap redzone");
	}

	detstack_zone_left = detstack_zone_size;
	detstack_zone = (char *) (detstackend) - detstack_zone_size;
	if (detstack_zone_size > 0
	&& mprotect(detstack_zone, detstack_zone_size, MY_PROT) < 0)
	{
		if (memdebug)
			perror("Mercury runtime: warning: "
				"cannot protect detstack redzone");
	}

	nondstack_zone_left = nondstack_zone_size;
	nondstack_zone = (char *) (nondstackend) - nondstack_zone_size;
	if (nondstack_zone_size > 0
	&& mprotect(nondstack_zone, nondstack_zone_size, MY_PROT) < 0)
	{
		if (memdebug)
			perror("Mercury runtime: warning: "
				"cannot protect nondstack redzone");
	}
}

#ifdef HAVE_SIGINFO	/* try_munprotect is only useful if we have SIGINFO */

/*
** fatal_abort() prints an error message and then exits.
** It is like fatal_error(), except that it is safe to call from a signal
** handler
*/

#define STDERR 2

static void fatal_abort(const char *str)
{
	write(STDERR, str, strlen(str));
	_exit(1);
}

static bool try_munprotect(void *addr)
{
	char *	fault_addr;
	char *	new_zone;

	fault_addr = (char *) addr;

	if (heap_zone != NULL && heap_zone <= fault_addr
	&& fault_addr <= heap_zone + heap_zone_size)
	{
		if (memdebug)
			fprintf(stderr, "address is in heap red zone\n");

		new_zone = (char *) round_up((Unsigned) fault_addr +
							sizeof(Word), unit);
		if (new_zone <= heap_zone + heap_zone_left)
		{
			if (new_zone >= (char *) heapend)
			{
				if (memdebug)
				{
					fprintf(stderr, "can't unprotect last page\n");
					fflush(stdout);
				}

				fatal_abort("\nMercury runtime: heap overflow\n");
			}

			if (memdebug)
				fprintf(stderr, "trying to unprotect from %p to %p\n",
					(void *) heap_zone, (void *) new_zone);

			if (mprotect(heap_zone, new_zone-heap_zone,
				PROT_READ|PROT_WRITE) < 0)
			{
				perror("Mercury runtime: cannot unprotect heap");
				exit(1);
			}

			heap_zone_left -= new_zone-heap_zone;
			heap_zone = new_zone;
			if (memdebug)
			{
				fprintf(stderr, "successful, heap_zone now %p\n",
					(void *) heap_zone);
				/* fprintf(stderr, "value at fault addr %p is %d\n",
					(void *) addr, * ((Word *) addr)); */
			}

			return TRUE;
		}
	}
	else if (detstack_zone != NULL && detstack_zone <= fault_addr
	&& fault_addr <= detstack_zone + detstack_zone_size)
	{
		if (memdebug)
			fprintf(stderr, "address is in detstack red zone\n");

		new_zone = (char *) round_up((Unsigned) fault_addr +
							sizeof(Word), unit);
		if (new_zone <= detstack_zone + detstack_zone_left)
		{
			if (memdebug)
				fprintf(stderr, "trying to unprotect from %p to %p\n",
					(void *) detstack_zone, (void *) new_zone);

			if (new_zone >= (char *) detstackend)
			{
				if (memdebug)
				{
					fprintf(stderr, "cannot unprotect last page\n");
					fflush(stdout);
				}

				fatal_abort("\nMercury runtime: det stack overflow\n");
			}

			if (mprotect(detstack_zone, new_zone-detstack_zone,
				PROT_READ|PROT_WRITE) < 0)
			{
				perror("Mercury runtime: cannot unprotect detstack");
				exit(1);
			}

			detstack_zone_left -= new_zone-detstack_zone;
			detstack_zone = new_zone;
			if (memdebug)
			{
				fprintf(stderr, "successful, detstack_zone now %p\n",
					(void *) detstack_zone);
				/* fprintf(stderr, "value at fault addr %p is %d\n",
					(void *) addr, * ((Word *) addr)); */
			}

			return TRUE;
		}
	}
	else if (nondstack_zone != NULL && nondstack_zone <= fault_addr
	&& fault_addr <= nondstack_zone + nondstack_zone_size)
	{
		if (memdebug)
			fprintf(stderr, "address is in nondstack red zone\n");

		new_zone = (char *) round_up((Unsigned) fault_addr +
							sizeof(Word), unit);
		if (new_zone <= nondstack_zone + nondstack_zone_left)
		{
			if (memdebug)
				fprintf(stderr, "trying to unprotect from %p to %p\n",
					(void *) nondstack_zone, (void *) new_zone);

			if (new_zone >= (char *) nondstackend)
			{
				if (memdebug)
				{
					fprintf(stderr, "cannot unprotect last page\n");
					fflush(stdout);
				}

				fatal_abort("\nMercury runtime: nondet stack overflow\n");
			}

			if (mprotect(nondstack_zone, new_zone-nondstack_zone,
				PROT_READ|PROT_WRITE) < 0)
			{
				perror("Mercury runtime: cannot unprotect nondstack\n");
				exit(1);
			}

			nondstack_zone_left -= new_zone-nondstack_zone;
			nondstack_zone = new_zone;
			if (memdebug)
			{
				fprintf(stderr, "successful, nondstack_zone now %p\n",
					(void *) nondstack_zone);
				/* fprintf(stderr, "value at fault addr %p is %d\n",
					(void *) addr, * ((Word *) addr)); */
			}

			return TRUE;
		}
	}

	return FALSE;
}

#endif /* HAVE_SIGINFO */

#else /* not HAVE_MPROTECT */

static void setup_mprotect(void)
{
	heap_zone      = NULL;
	detstack_zone  = NULL;
	nondstack_zone = NULL;
}

#ifdef HAVE_SIGINFO	/* try_munprotect is only useful if we have SIGINFO */

static bool try_munprotect(void *addr)
{
	return FALSE;
}

#endif /* HAVE_SIGINFO */

#endif /* not HAVE_MPROTECT */

#ifdef	HAVE_SIGINFO

static void setup_signal(void)
{
	struct sigaction	act;

	act.sa_flags = SA_SIGINFO | SA_RESTART;
	if (sigemptyset(&act.sa_mask) != 0)
	{
		perror("Mercury runtime: cannot set clear signal mask");
		exit(1);
	}

	act.SIGACTION_FIELD = complex_bushandler;
	if (sigaction(SIGBUS, &act, NULL) != 0)
	{
		perror("Mercury runtime: cannot set SIGBUS handler");
		exit(1);
	}

	act.SIGACTION_FIELD = complex_segvhandler;
	if (sigaction(SIGSEGV, &act, NULL) != 0)
	{
		perror("Mercury runtime: cannot set SIGSEGV handler");
		exit(1);
	}
}

static void complex_bushandler(int sig, siginfo_t *info, void *context)
{
	fflush(stdout);

	if (sig != SIGBUS || !info || info->si_signo != SIGBUS)
	{
		fprintf(stderr, "\n*** Mercury runtime: ");
		fprintf(stderr, "caught strange bus error ***\n");
		exit(1);
	}

	fprintf(stderr, "\n*** Mercury runtime: ");
	fprintf(stderr, "caught bus error ***\n");

	if (info->si_code > 0)
	{
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

		}

#ifdef PC_ACCESS
		fprintf(stderr, "PC at signal: %ld (%lx)\n",
			(long) ((ucontext_t *) context)->uc_mcontext.PC_ACCESS,
			(long) ((ucontext_t *) context)->uc_mcontext.PC_ACCESS);
#endif
		fprintf(stderr, "address involved: %p\n",
			(void *) info->si_addr);
	}
	dump_prev_locations();
	fprintf(stderr, "exiting from signal handler\n");
	exit(1);
}

static void explain_segv(siginfo_t *info, void *context)
{
	fflush(stdout);

	fprintf(stderr, "\n*** Mercury runtime: ");
	fprintf(stderr, "caught segmentation violation ***\n");

	if (!info) return;

	if (info->si_code > 0)
	{
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

#ifdef PC_ACCESS
		fprintf(stderr, "PC at signal: %ld (%lx)\n",
			(long) ((ucontext_t *) context)->uc_mcontext.PC_ACCESS,
			(long) ((ucontext_t *) context)->uc_mcontext.PC_ACCESS);
#endif
		fprintf(stderr, "address involved: %p\n",
			(void *) info->si_addr);

	}
}

static void complex_segvhandler(int sig, siginfo_t *info, void *context)
{
	if (sig != SIGSEGV || !info || info->si_signo != SIGSEGV)
	{
		fprintf(stderr, "\n*** Mercury runtime: ");
		fprintf(stderr, "caught strange segmentation violation ***\n");
		exit(1);
	}

	/*
	** If we're debugging, print the segv explanation messages
	** before we call try_munprotect.  But if we're not debugging,
	** only print them if try_munprotect fails.
	*/

	if (memdebug)
		explain_segv(info, context);

	if (try_munprotect(info->si_addr))
	{
		if (memdebug)
			fprintf(stderr, "returning from signal handler\n\n");

		return;
	}

	if (!memdebug)
		explain_segv(info, context);

	dump_prev_locations();
	fprintf(stderr, "exiting from signal handler\n");
	exit(1);
}

#else

static void setup_signal(void)
{
	if (signal(SIGBUS, simple_sighandler) == SIG_ERR)
	{
		perror("cannot set SIGBUS handler");
		exit(1);
	}

	if (signal(SIGSEGV, simple_sighandler) == SIG_ERR)
	{
		perror("cannot set SIGSEGV handler");
		exit(1);
	}
}

static void simple_sighandler(int sig)
{
	fflush(stdout);
	fprintf(stderr, "*** Mercury runtime: ");

	switch (sig)
	{

case SIGBUS:
		fprintf(stderr, "caught bus error ***\n");
		break;

case SIGSEGV: 	fprintf(stderr, "caught segmentation violation ***\n");
		break;

default:	fprintf(stderr, "caught unknown signal %d ***\n", sig);
		break;

	}
	dump_prev_locations();
	fprintf(stderr, "exiting from signal handler\n");
	exit(1);
}

#endif
