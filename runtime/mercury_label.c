/*
** Copyright (C) 1994-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** label.c defines the label table, which is a pair of hash tables
** that map from procedure names to addresses and vice versa.
*/

#include	"mercury_imp.h"	/* we need libmer_dll.h for Windows DLLs */

#include	<stdio.h>
#include	<string.h>

#include	"mercury_conf.h"

#include	"mercury_label.h"

#include	"mercury_table.h"	/* for `Table' */
#include	"mercury_prof.h"	/* for prof_output_addr_decl() */
#include	"mercury_engine.h"	/* for `MR_progdebug' */
#include	"mercury_wrapper.h"	/* for do_init_modules() */

/*
** We record information about entry labels in an array that
** we sort by code address once all the entry labels have been inserted.
** Space for the array is provided by malloc, and it is expanded when
** needed.
**
** This array is needed only by accurate garbage collection and when
** doing low-level debugging.
*/

#ifdef	MR_NEED_ENTRY_LABEL_ARRAY

/* the number of entries in the initial array */
#define	INIT_ENTRY_SIZE	(1 << 8)

static	int		compare_entry_by_addr(const void *e1, const void *e2);

static	MR_Entry	*entry_array;
static	int		entry_array_size;	/* # of entries allocated */
static	int		entry_array_next;	/* # of entries used      */
static	bool		entry_array_sorted;

#endif	/* MR_NEED_ENTRY_LABEL_ARRAY */

/*
** We record information about internal labels in a hash table
** that is indexed by the code address of the label.
**
** This table is used by stack tracing and execution tracing.
** Since execution tracing and hence stack tracing can be required
** in any grade, we always include this table.
*/

#define	INTERNAL_SIZE	(1 << 16)	/* 64k */

static	const void	*internal_addr(const void *internal);
static	bool		equal_addr(const void *addr1, const void *addr2);
static	int		hash_addr(const void *addr);

static	Table		internal_addr_table = {INTERNAL_SIZE, NULL,
				internal_addr, hash_addr, equal_addr};

void 
MR_do_init_label_tables(void)
{
	static	bool	done = FALSE;

	if (!done) {
#ifdef	MR_NEED_ENTRY_LABEL_ARRAY
		entry_array_next = 0;
		entry_array_size = INIT_ENTRY_SIZE;
		entry_array_sorted = TRUE;
		entry_array = checked_malloc(entry_array_size
					* sizeof(MR_Entry));
#endif

		init_table(internal_addr_table);

		done = TRUE;
	}
}

#ifdef	MR_NEED_ENTRY_LABEL_INFO

static	int	compare_entry_addr(const void *e1, const void *e2);

void
MR_insert_entry_label(const char *name, Code *addr,
	const MR_Stack_Layout_Entry *entry_layout)
{
	MR_do_init_label_tables();

#ifdef	PROFILE_CALLS
	if (MR_profiling) MR_prof_output_addr_decl(name, addr);
#endif

#ifdef	MR_LOWLEVEL_DEBUG
	if (MR_progdebug) {
		printf("recording entry label %s at %p\n", name, addr);
	}
#endif

#ifdef	MR_NEED_ENTRY_LABEL_ARRAY

	if (entry_array_next >= entry_array_size) {
		entry_array_size *= 2;
		entry_array = realloc(entry_array, 
				entry_array_size * sizeof(MR_Entry));
		if (entry_array == NULL) {
			fatal_error("run out of memory for entry label array");
		}
	}

	entry_array[entry_array_next].e_addr = addr;
	entry_array[entry_array_next].e_name = name;
	entry_array[entry_array_next].e_layout = entry_layout;
	entry_array_next++;
	entry_array_sorted = FALSE;
#endif
}

#endif

#ifdef	MR_NEED_ENTRY_LABEL_ARRAY

static int
compare_entry_addr(const void *e1, const void *e2)
{
	const MR_Entry	*entry1;
	const MR_Entry	*entry2;

	entry1 = (const MR_Entry *) e1;
	entry2 = (const MR_Entry *) e2;

	if (entry1->e_addr > entry2->e_addr) {
		return 1;
	} else if (entry1->e_addr < entry2->e_addr) {
		return -1;
	} else {
		return 0;
	}
}

MR_Entry *
MR_prev_entry_by_addr(const Code *addr)
{
	int	lo;
	int	hi;
	int	mid;
	int	i;

	MR_do_init_label_tables();
	do_init_modules();

	if (!entry_array_sorted) {
		qsort(entry_array, entry_array_next, sizeof(MR_Entry),
			compare_entry_addr);

		entry_array_sorted = TRUE;
	}

	lo = 0;
	hi = entry_array_next-1;

	while (lo <= hi) {
		mid = (lo + hi) / 2;
		if (entry_array[mid].e_addr == addr) {
			return &entry_array[mid];
		} else if (entry_array[mid].e_addr < addr) {
			lo = mid + 1;
		} else {
			hi = mid - 1;
		}
	}

	if (entry_array[lo].e_addr < addr) {
		return &entry_array[lo];
	} else {
		return &entry_array[lo - 1];
	}
}

#endif

void
MR_insert_internal_label(const char *name, Code *addr,
	const MR_Stack_Layout_Label *label_layout)
{
	MR_Internal	*internal;

	MR_do_init_label_tables();

	internal = make(MR_Internal);
	internal->i_addr = addr;
	internal->i_layout = label_layout;
	internal->i_name = name;

#ifdef	MR_LOWLEVEL_DEBUG
	if (MR_progdebug) {
		printf("inserting internal label %s at %p\n", name, addr);
	}
#endif

	/* two labels at same location will happen quite often */
	/* when the code generated between them turns out to be empty */

	(void) insert_table(internal_addr_table, internal);
}

MR_Internal *
MR_lookup_internal_by_addr(const Code *addr)
{
	MR_do_init_label_tables();
	do_init_modules();

#ifdef	MR_LOWLEVEL_DEBUG
	if (MR_progdebug) {
		printf("looking for internal label at %p\n", addr);
	}
#endif

	return (MR_Internal *) lookup_table(internal_addr_table, addr);
}

static const void *
internal_addr(const void *internal)
{
	return (const void *) (((const MR_Internal *) internal)->i_addr);
}

static bool 
equal_addr(const void *addr1, const void *addr2)
{
	return ((const Code *) addr1) == ((const Code *) addr2);
}

static int 
hash_addr(const void *addr)
{
	return (((Unsigned) addr) >> 3) % INTERNAL_SIZE;
}

void
MR_process_all_internal_labels(void f(const void *))
{
	process_all_entries(internal_addr_table, f);
}
