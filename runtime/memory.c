#include "imp.h"
#include <stdlib.h>
#include <unistd.h>

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

#ifdef	__svr4__

#include <sys/types.h>
#include <sys/mman.h>

#define	getpagesize()	sysconf(_SC_PAGESIZE)

#else

#define	memalign(a,s)	malloc(s)

#endif

#include <signal.h>

static	void	mer_sighandler(int);
static	int	roundup(int, int);

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

extern	int	pcache_size;

extern	int	heap_size;
extern	int	detstack_size;
extern	int	nondstack_size;
extern	int	heap_zone_size;
extern	int	detstack_zone_size;
extern	int	nondstack_zone_size;

void
init_memory(void)
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

#ifdef	__svr4__

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

#endif

	if (signal(SIGBUS, mer_sighandler) == SIG_ERR)
	{
		perror("cannot set SIGBUS handler");
		exit(1);
	}

	if (signal(SIGSEGV, mer_sighandler) == SIG_ERR)
	{
		perror("cannot set SIGSEGV handler");
		exit(1);
	}
}

static int
roundup(int value, int align)
{
	if ((value & (align - 1)) != 0)
		value += align - (value & (align - 1));

	return value;
}

static void
mer_sighandler(int sig)
{
	switch (sig)
	{

case SIGBUS: 	printf("*** caught bus error ***\n");

when SIGSEGV: 	printf("*** caught segmentation violation ***\n");

otherwise:	printf("*** caught unknown signal ***\n");

	}

	exit(1);
}
