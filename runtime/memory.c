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
** all start at different offsets from multiples of the primary cache size.
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
static	bool	try_munprotect(void *, void *);
static	char	*explain_context(ucontext_t *);
#endif

static	void	setup_signal(void);

Word	fake_reg[MAX_FAKE_REG];

Word	virtual_reg_map[MAX_REAL_REG] = VIRTUAL_REG_MAP_BODY;

Word	num_uses[MAX_RN];

MemoryZone zone_table[MAX_ZONES];

MemoryZone *used_memory_zones;
MemoryZone *free_memory_zones;

MemoryZone *detstack_zone;
#ifndef	SPEED
MemoryZone *dumpstack_zone;
#endif
MemoryZone *nondetstack_zone;
#ifndef CONSERVATIVE_GC
MemoryZone *heap_zone;
#endif

static	unsigned	unit;
static	unsigned	page_size;

MemoryZone	*construct_zone(const char *name, Word *base, unsigned size,
			unsigned offset, unsigned redsize);
MemoryZone	*create_zone(const char *name, unsigned size, unsigned offset,
			unsigned redsize);
MemoryZone	*get_zone(void);
void		unget_zone(MemoryZone *zone);

void init_memory(void)
{
	char		*arena;
	unsigned	total_size;
	unsigned	fake_reg_offset;
	unsigned	heap_offset;
	unsigned	detstack_offset;
	unsigned	nondstack_offset;
	int		i;
	Word		*detstack_base;
	Word		*nondetstack_base;
#ifndef	CONSERVATIVE_GC
	Word		*heap_base;
#endif

	/*
	** Initialize the MemoryZone table.
	*/
	used_memory_zones = NULL;
	free_memory_zones = zone_table;
	for(i=0; i < MAX_ZONES; i++)
	{
		strcpy(zone_table[i].name, "unused");
		zone_table[i].bottom = NULL;
		zone_table[i].top = NULL;
		zone_table[i].min = NULL;
#ifndef SPEED
		zone_table[i].max = NULL;
#endif
#ifdef	HAVE_MPROTECT
		zone_table[i].redzone = NULL;
		zone_table[i].hardmax = NULL;
#endif
		if (i+1 < MAX_ZONES)
			zone_table[i].next = &(zone_table[i+1]);
		else
			zone_table[i].next = NULL;
	}

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

	/*  get total_size bytes aligned on a page boundary */
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

#ifndef CONSERVATIVE_GC
	heap_base	 = (Word *) arena;
	detstack_base	 = round_up((Unsigned)heap_base+heap_size+unit, unit);
#else
	detstack_base	 = (Word *) arena;
#endif
	nondetstack_base = (Word *)
		round_up((Unsigned)detstack_base+detstack_size+unit, unit);


	fake_reg_offset = (Unsigned) fake_reg % pcache_size;
	heap_offset = (fake_reg_offset + pcache_size / 4) % pcache_size;
	detstack_offset = (heap_offset + pcache_size / 4) % pcache_size;
	nondstack_offset = (detstack_offset + pcache_size / 4) % pcache_size;

	/*
	** Create memory zones for the heap, det stack and nondet stack.
	*/

	detstack_zone = construct_zone("det stack", detstack_base,
			detstack_size, detstack_offset, detstack_zone_size);
	nondetstack_zone = construct_zone("nondet stack", nondetstack_base,
			nondstack_size, nondstack_offset, nondstack_zone_size);
#ifndef CONSERVATIVE_GC
	heap_zone = construct_zone("heap", heap_base, heap_size, heap_offset,
			heap_zone_size);
#endif


#ifndef	SPEED
	nondetzone->min[PREDNM] = (Word) "bottom";
#endif

#ifndef	SPEED
	/* We allocate as many `char *'s as there are words in the detstack. */
	/* In the worst-case, all detstack frames have only one word. */
	dumpstack_zone = create_zone("dumpstack",
		detstack_size * (sizeof(char *) / (sizeof(Word))), 0, 0);
#endif

	setup_signal();

	if (memdebug)
	{
		MemoryZone	*zone;

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

		for (zone = used_memory_zones; zone; zone = zone->next)
		{
			fprintf(stderr, "%-16s-base	= %p\n",
				zone->name, (void *) zone->bottom);
			fprintf(stderr, "%-16s-min		= %p\n",
				zone->name, (void *) zone->min);
			fprintf(stderr, "%-16s-top		= %p\n",
				zone->name, (void *) zone->top);
#ifdef	HAVE_MPROTECT
			fprintf(stderr, "%-16s-redzone		= %p\n",
				zone->name, (void *) zone->redzone);
			fprintf(stderr, "%-16s-hardmax		= %p\n",
				zone->name, (void *) zone->hardmax);
#endif
			fprintf(stderr, "%-16s-size		= %ld\n",
				zone->name,
				(char *)zone->hardmax - (char *)zone->min);
			fprintf(stderr, "\n");
		}
	}
}

MemoryZone *get_zone()
{
	MemoryZone *zone;

	/*
	** unlink the first zone on the free-list,
	** link it onto the used-list and return it.
	*/
	zone = free_memory_zones;
	if (zone == NULL)
	{
		fatal_error("no more memory zones");
	}
	free_memory_zones = free_memory_zones->next;

	zone->next = used_memory_zones;
	used_memory_zones = zone;

	return zone;
}

void unget_zone(MemoryZone *zone)
{
	MemoryZone *prev, *tmp;

	/*
	** Find the zone on the used list, and unlink it from
	** the list, then link it onto the start of the free-list.
	*/
	for(prev = NULL, tmp = used_memory_zones;
		tmp && tmp != zone; prev = tmp, tmp = tmp->next) ;
	if (tmp == NULL)
		fatal_error("memory zone not found!");
	if (prev == NULL)
	{
		used_memory_zones = used_memory_zones->next;
	}
	else
	{
		prev->next = tmp->next;
	}

	zone->next = free_memory_zones;
	free_memory_zones = zone;
}

MemoryZone *create_zone(const char *name, unsigned size, unsigned offset,
		unsigned redsize)
{
	Word		*base;
	unsigned	total_size;

		/*
		** total allocation is:
		**	unit		(roundup to page boundary)
		**	size		(including redzone)
		**	unit		(an extra page for protection if
		**			 mprotect is being used)
		*/
#ifdef	HAVE_MPROTECT
	total_size = size+2*unit;
#else
	total_size = size+unit;
#endif

	base = memalign(unit, total_size);
	if (base == NULL)
		fatal_error("failed to allocate zone");

	return construct_zone(name, base, size, offset, redsize);
}

MemoryZone *construct_zone(const char *name, Word *base, unsigned size,
		unsigned offset, unsigned redsize)
{
	MemoryZone	*zone;
	unsigned	total_size;

	if (base == NULL)
		fatal_error("construct_zone called with NULL pointer");

	zone = get_zone();

	strncpy(zone->name, name, MAX_ZONE_NAME);

	zone->bottom = base;

#ifdef 	HAVE_MPROTECT
	total_size = size+unit;
#else
	total_size = size;
#endif

	zone->top = (Word *) ((char *)base+total_size);
	zone->min = (Word *) ((char *)base+offset);
#ifndef SPEED
	zone->max = zone->min;
#endif

#ifdef	HAVE_MPROTECT
	/*
	** setup the redzone+hardzone
	*/
	zone->redzone = (Word *)
			round_up((Unsigned)base+size-redsize, unit);
	if (mprotect(zone->redzone, redsize+unit, MY_PROT) < 0)
	{
		char buf[2560];
		sprintf(buf, "unable to set %s redzone\n"
			"base=%p, redzone=%p",
			zone->name, zone->bottom, zone->redzone);
		fatal_error(buf);
	}
	zone->hardmax = (Word *) ((char *)zone->top-unit);
#endif

	return zone;
}

#ifdef	HAVE_MPROTECT

#ifdef HAVE_SIGINFO	/* try_munprotect is only useful if we have SIGINFO */

#define STDERR 2

#ifdef	SPEED

static void print_dump_stack(void)
{
	const char *msg = "You can get a stack dump by using grade debug\n";
	write(STDERR, msg, strlen(msg));
}

#else

static void print_dump_stack(void)
{
	int	i;
	int	start;
	int	count;
	char	buf[2560];

	strcpy(buf, "A dump of the det stack follows\n\n");
	write(STDERR, buf, strlen(buf));

	i = 0;
	while (i < dumpindex)
	{
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

		if (count > 1)
			sprintf(buf, "%s * %d\n",
				((char **)(dumpstack_zone->min)[start], count);
		else
			sprintf(buf, "%s\n",
				((char **)(dumpstack_zone->min)[start]);

		write(STDERR, buf, strlen(buf));
	}

	strcpy(buf, "\nend of stack dump\n");
	write(STDERR, buf, strlen(buf));

}

#endif

/*
** fatal_abort() prints an error message, possibly a stack dump, and then exits.
** It is like fatal_error(), except that it is safe to call
** from a signal handler.
*/

static void fatal_abort(void *context, const char *main_msg, int dump)
{
	char	*context_msg;

	context_msg = explain_context((ucontext_t *) context);
	write(STDERR, main_msg, strlen(main_msg));
	write(STDERR, context_msg, strlen(context_msg));

	if (dump)
		print_dump_stack();

	_exit(1);
}

static bool try_munprotect(void *addr, void *context)
{
    Word *    fault_addr;
    Word *    new_zone;
    MemoryZone *zone;
    unsigned zone_size;

    fault_addr = (Word *) addr;

    zone = used_memory_zones;

    if (memdebug)
	fprintf(stderr, "caught fault at %p\n", (void *)addr);

    while(zone != NULL)
    {
	if (memdebug)
	{
		fprintf(stderr, "checking %s: %p - %p\n",
			zone->name, (void *) zone->redzone, (void *) zone->top);
	}

        if (zone->redzone <= fault_addr
            && fault_addr <= zone->top)
	{

            if (memdebug)
                fprintf(stderr, "address is in %s red zone\n", zone->name);

            new_zone = (Word *) round_up((Unsigned) fault_addr +
                            sizeof(Word), unit);
        
            if (new_zone <= zone->hardmax)
	    {
                zone_size = (char *)new_zone - (char *)zone->redzone;

                if (memdebug)
		{
                    fprintf(stderr, "trying to unprotect %s from %p to %p (%x)\n",
                    zone->name, (void *) zone->redzone, (void *) new_zone,
		    zone_size);
		}
                if (mprotect(zone->redzone, zone_size,
                    PROT_READ|PROT_WRITE) < 0)
                {
                    perror("Mercury runtime: cannot unprotect zone");
                    exit(1);
                }

		zone->redzone = new_zone;

                if (memdebug)
		{
                    fprintf(stderr, "successful: %s redzone now %p to %p\n",
                        zone->name, (void *) zone->redzone,
			(void *) zone->top);
		}
                return TRUE;
            }
	    else
	    {
                if (memdebug)
                {
                    fprintf(stderr, "can't unprotect last page of %s\n",
			zone->name);
                    fflush(stdout);
                }

                fatal_abort(context, "\nMercury runtime: zone overflow\n", FALSE);
            }
        }
        zone = zone->next;
    }

    if (memdebug)
	fprintf(stderr, "address not in any redzone.\n");

    return FALSE;
}

#endif /* HAVE_SIGINFO */

#else /* not HAVE_MPROTECT */

#ifdef HAVE_SIGINFO	/* try_munprotect is only useful if we have SIGINFO */

static bool try_munprotect(void *addr, void *context)
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

		fprintf(stderr, "%s", explain_context((ucontext_t *) context));
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

		fprintf(stderr, "%s", explain_context((ucontext_t *) context));
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

	if (try_munprotect(info->si_addr, context))
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

static char *explain_context(ucontext_t *context)
{
	static	char	buf[100];

#ifdef PC_ACCESS
#ifdef PC_ACCESS_GREG
	sprintf(buf, "PC at signal: %ld (%lx)\n",
		(long) context->uc_mcontext.gregs[PC_ACCESS],
		(long) context->uc_mcontext.gregs[PC_ACCESS]);
#else
	sprintf(buf, "PC at signal: %ld (%lx)\n",
		(long) context->uc_mcontext.PC_ACCESS,
		(long) context->uc_mcontext.PC_ACCESS);
#endif
#else
	/* if PC_ACCESS is not set, we don't know the context */
	/* therefore we return an empty string to be printed  */
	buf[0] = '\0';
#endif

	return buf;
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
