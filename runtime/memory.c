#include <unistd.h>
#include "imp.h"

/*
** The important global variables of the execution algorithm.
** They are defined together here to allow us to control how they map
** onto direct mapped caches.
**
** We allocate a large arena, preferably aligned on a boundary that
** is a multiple of both the page size and the primary cache size.
**
** We then allocate the register arrays, the heap and the stacks
** in such a way that
**
**	the register array starts at the bottom of the primary cache
**	the bottom of the heap takes the rest of the first half of the cache
**	the bottom of the detstack takes the third quarter of the cache
**	the bottom of the nondstack takes the fourth quarter of the cache
**
** This should significantly reduce cache conflicts.
**
** If the operating system of the machine supports the mprotect syscall,
** we also protect a chunk at the end of each area against access,
** thus detecting area overflow.
*/

/* the following should be derived by autoconf */
#ifdef	__svr4__
#define	HAVE_MPROTECT
#define	HAVE_SIGINFO
#define	HAVE_SYSCONF
#define	HAVE_MEMALIGN
#endif

#ifdef	HAVE_MPROTECT
#include <sys/types.h>
#include <sys/mman.h>
#endif

#ifdef	HAVE_SIGINFO
#include <signal.h>
#include <siginfo.h>
#include <ucontext.h>
static	void	complex_bushandler(int, siginfo_t *, void *);
static	void	complex_segvhandler(int, siginfo_t *, void *);
#else
#include <signal.h>
static	void	simple_sighandler(int);
#endif

#ifdef	HAVE_SYSCONF
#define	getpagesize()	sysconf(_SC_PAGESIZE)
#else
extern	int	getpagesize(void);
#endif

#ifndef	HAVE_MEMALIGN
#define	memalign(a,s)	malloc(s)
#endif

static	int	roundup(int, int);
static	void	do_mprotect(void);
static	void	do_signal(void);

Word	unreal_reg_0;
Word	unreal_reg_1;
Word	unreal_reg_2;
Word	unreal_reg_3;
Word	unreal_reg_4;
Word	unreal_reg_5;
Word	unreal_reg_6;
Word	unreal_reg_7;
Word	unreal_reg_8;
Word	unreal_reg_9;
Word	unreal_reg_10;
Word	unreal_reg_11;
Word	unreal_reg_12;
Word	unreal_reg_13;
Word	unreal_reg_14;
Word	unreal_reg_15;
Word	unreal_reg_16;
Word	unreal_reg_17;
Word	unreal_reg_18;
Word	unreal_reg_19;
Word	unreal_reg_20;
Word	unreal_reg_21;
Word	unreal_reg_22;
Word	unreal_reg_23;
Word	unreal_reg_24;
Word	unreal_reg_25;
Word	unreal_reg_26;
Word	unreal_reg_27;
Word	unreal_reg_28;
Word	unreal_reg_29;
Word	unreal_reg_30;
Word	unreal_reg_31;
Word	unreal_reg_32;
Word	unreal_reg_33;
Word	unreal_reg_34;
Word	unreal_reg_35;
Word	unreal_reg_36;

Word	*saved_regs;
Word	*num_uses;

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

char	*heap_zone;
char	*detstack_zone;
char	*nondstack_zone;

static	int	unit;
static	int	page_size;

void init_memory(void)
{
	char	*arena;
	int	total_size;
	int	i;

	/*
	** Convert all the sizes are from kilobytes to bytes and
	** make sure they are multiples of the page and cache sizes.
	*/

	page_size = getpagesize();

	unit = page_size;
	while (unit < pcache_size)
		unit *= 2;

	heap_size           = roundup(heap_size * 1024, unit);
	detstack_size       = roundup(detstack_size * 1024, unit);
	nondstack_size      = roundup(nondstack_size * 1024, unit);
	heap_zone_size      = roundup(heap_zone_size * 1024, unit);
	detstack_zone_size  = roundup(detstack_zone_size * 1024, unit);
	nondstack_zone_size = roundup(nondstack_zone_size * 1024, unit);

	if (heap_zone_size >= heap_size)
		heap_zone_size = unit;

	if (detstack_zone_size >= detstack_size)
		detstack_zone_size = unit;

	if (nondstack_zone_size >= nondstack_size)
		nondstack_zone_size = unit;

	/*
	** Calculate how much memory to allocate, then allocate it
	** and divide it up among the areas.
	*/

	total_size = heap_size + detstack_size + nondstack_size + 4 * unit;

	/*  get mem_size pages aligned on a page boundary */
	arena = memalign(unit, total_size);
	if (arena == NULL)
	{
		printf("cannot allocate arena: memalign() failed\n");
		exit(1);
	}

	saved_regs  = (Word *) (arena + 0 * MAX_RN * sizeof(Word));
	num_uses    = (Word *) (arena + 1 * MAX_RN * sizeof(Word));

	for (i = 0; i < MAX_RN; i++)
		num_uses[i] = 0;

	if (2 * MAX_RN * sizeof(Word) < pcache_size/4)
	{
		heap    = (Word *) (arena + 0 * MAX_RN * sizeof(Word));
		heapmin = (Word *) (arena + 2 * MAX_RN * sizeof(Word));
	}
	else
	{
		if (2 * MAX_RN * sizeof(Word) >= unit)
		{
			printf("cache division strategy isn't working\n");
			exit(1);
		}

		heap    = (Word *) (arena + unit + 0 * MAX_RN * sizeof(Word));
		heapmin = (Word *) (arena + unit + 2 * MAX_RN * sizeof(Word));
	}

	heapend = (Word *) ((char *) heap + heap_size + unit);
	assert(((int) heapend) % unit == 0);

	detstack    = heapend;
	detstackmin = (Word *) ((char *) detstack + (pcache_size / 2));
	detstackend = (Word *) ((char *) detstack + detstack_size + unit);
	assert(((int) detstackend) % unit == 0);

	nondstack    = detstackend;
	nondstackmin = (Word *) ((char *) nondstack + (3 * pcache_size / 4));
	nondstackend = (Word *) ((char *) nondstack + nondstack_size + unit);
	assert(((int) nondstackend) % unit == 0);

	nondstackmin[PREDNM] = (Word) "bottom";

	if (memdebug)
	{

		printf("\n");
		printf("pcache_size  = %d (%x)\n", pcache_size, pcache_size);
		printf("page_size    = %d (%x)\n", page_size, page_size);
		printf("unit         = %d (%x)\n", unit, unit);

		printf("\n");
		printf("saved_regs   = %p (%d)\n", saved_regs,
			(int) saved_regs & (unit-1));
		printf("num_uses     = %p (%d)\n", num_uses,
			(int) num_uses & (unit-1));

		printf("\n");
		printf("heap         = %p (%d)\n", heap,
			(int) heap & (unit-1));
		printf("heapmin      = %p (%d)\n", heapmin,
			(int) heapmin & (unit-1));
		printf("heapend      = %p (%d)\n", heapend,
			(int) heapend & (unit-1));

		printf("\n");
		printf("detstack     = %p (%d)\n", detstack,
			(int) detstack & (unit-1));
		printf("detstackmin  = %p (%d)\n", detstackmin,
			(int) detstackmin & (unit-1));
		printf("detstackend  = %p (%d)\n", detstackend,
			(int) detstackend & (unit-1));

		printf("\n");
		printf("nondstack    = %p (%d)\n", nondstack,
			(int) nondstack & (unit-1));
		printf("nondstackmin = %p (%d)\n", nondstackmin,
			(int) nondstackmin & (unit-1));
		printf("nondstackend = %p (%d)\n", nondstackend,
			(int) nondstackend & (unit-1));

		printf("\n");
		printf("arena start  = %p (%d)\n", arena,
			(int) arena & (unit-1));
		printf("arena end    = %p (%d)\n", arena+total_size,
			(int) (arena+total_size) & (unit-1));

		printf("\n");
		printf("heap         = %d (%x)\n",
			(char *) heapend - (char *) heapmin,
			(char *) heapend - (char *) heapmin);
		printf("detstack     = %d (%x)\n",
			(char *) detstackend - (char *) detstackmin,
			(char *) detstackend - (char *) detstackmin);
		printf("nondstack    = %d (%x)\n",
			(char *) nondstackend - (char *) nondstackmin,
			(char *) nondstackend - (char *) nondstackmin);
		printf("arena size   = %d (%x)\n", total_size, total_size);
	}

	if (arena + total_size <= (char *) nondstackend)
	{
		printf("allocated too much memory\n");
		exit(0);
	}

	do_mprotect();
	do_signal();
}

static int roundup(int value, int align)
{
	if ((value & (align - 1)) != 0)
		value += align - (value & (align - 1));

	return value;
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

void do_mprotect(void)
{
	heap_zone = (char *) (heapend) - heap_zone_size;
	if (mprotect(heap_zone, heap_zone_size, PROT_NONE) != 0)
	{
		perror("cannot protect head redzone");
		exit(1);
	}

	detstack_zone = (char *) (detstackend) - detstack_zone_size;
	if (mprotect(detstack_zone, detstack_zone_size, PROT_NONE) != 0)
	{
		perror("cannot protect detstack redzone");
		exit(1);
	}

	nondstack_zone = (char *) (nondstackend) - nondstack_zone_size;
	if (mprotect(nondstack_zone, nondstack_zone_size, PROT_NONE) != 0)
	{
		perror("cannot protect nondstack redzone");
		exit(1);
	}
}

#else

void do_mprotect(void)
{
	heap_zone      = NULL;
	detstack_zone  = NULL;
	nondstack_zone = NULL;
}

#endif

#ifdef	HAVE_SIGINFO

void do_signal(void)
{
	struct sigaction	act;

	act.sa_flags = SA_SIGINFO;
	if (sigemptyset(&act.sa_mask) != 0)
	{
		perror("cannot set clear signal mask");
		exit(1);
	}

	act.sa_sigaction = complex_bushandler;
	if (sigaction(SIGBUS, &act, NULL) != 0)
	{
		perror("cannot set SIGBUS handler");
		exit(1);
	}

	act.sa_sigaction = complex_segvhandler;
	if (sigaction(SIGSEGV, &act, NULL) != 0)
	{
		perror("cannot set SIGSEGV handler");
		exit(1);
	}
}

static void complex_bushandler(int sig, siginfo_t *info, void *context)
{
	if (sig != SIGBUS || info->si_signo != SIGBUS)
	{
		printf("\n*** caught strange bus error ***\n");
		exit(1);
	}

	printf("\n*** caught bus error ***\n");

	if (info->si_code > 0)
	{
		printf("cause: ");
		switch (info->si_code)
		{

	case BUS_ADRALN:	printf("invalid address alignment\n");
	when BUS_ADRERR:	printf("non-existent physical address\n");
	when BUS_OBJERR:	printf("object specific hardware error\n");
	otherwise:		printf("unknown\n");

		}

		printf("address involved: %p\n", info->si_addr);
	}

	exit(1);
}

static void complex_segvhandler(int sig, siginfo_t *info, void *context)
{
	if (sig != SIGSEGV || info->si_signo != SIGSEGV)
	{
		printf("\n*** caught strange segmentation violation ***\n");
		exit(1);
	}

	printf("\n*** caught segmentation violation ***\n");

	if (info->si_code > 0)
	{
		printf("cause: ");
		switch (info->si_code)
		{

	case SEGV_MAPERR:	printf("address not mapped to object\n");
	when SEGV_ACCERR:	printf("invalid permissions for mapped object\n");
	otherwise:		printf("unknown\n");

		}

		printf("address involved: %p\n", info->si_addr);

		if (heap_zone != NULL && heap_zone <= info->si_addr
		&& info->si_addr <= heap_zone + heap_zone_size)
			printf("address is in heap red zone\n");
		or (detstack_zone != NULL && detstack_zone <= info->si_addr
		&& info->si_addr <= detstack_zone + detstack_zone_size)
			printf("address is in detstack red zone\n");
		or (nondstack_zone != NULL && nondstack_zone <= info->si_addr
		&& info->si_addr <= nondstack_zone + nondstack_zone_size)
			printf("address is in nondstack red zone\n");

		printf("PC at the time: %d (%x)\n",
			((ucontext_t *) context)->uc_mcontext.gregs[REG_PC],
			((ucontext_t *) context)->uc_mcontext.gregs[REG_PC]);
	}

	exit(1);
}

#else

void do_signal(void)
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
	switch (sig)
	{

case SIGBUS: 	printf("*** caught bus error ***\n");

when SIGSEGV: 	printf("*** caught segmentation violation ***\n");

otherwise:	printf("*** caught unknown signal ***\n");

	}

	exit(1);
}

#endif
