/*
** Copyright (C) 1998, 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** File: mercury_heap_profile.h
** Main authors: zs, fjh
**
** This module records information about the allocations of cells on the heap.
** The information recorded here is used by mercury_prof.c and
** by library/benchmarking.m.
*/

/*---------------------------------------------------------------------------*/

#ifndef MERCURY_HEAP_PROFILE_H
#define MERCURY_HEAP_PROFILE_H

#include "mercury_types.h"	/* for `MR_Code' */

/*---------------------------------------------------------------------------*/

/*
** Due to garbage collection, the total amount of memory allocated can
** exceed the amount of real or even virtual memory available.  Hence
** the total amount of memory allocated by a long-running Mercury program
** might not fit into a single 32-bit `unsigned long'.
** Hence we record memory usage counts using either `unsigned long long',
** if available, or otherwise using a pair of unsigned longs (ugh).
*/

#ifdef MR_HAVE_LONG_LONG

  /* nice and simple */

  typedef unsigned long long MR_dword;

  #define MR_convert_dword_to_double(dword_form, double_form) \
		((double_form) = (double) (dword_form))

  #define MR_zero_dword(dword) \
		((dword) = 0)

  #define MR_increment_dword(dword, inc) \
		((dword) += (inc))

  #define MR_add_two_dwords(src_dest_dword, src_dword) \
		((src_dest_dword) += (src_dword))

#else /* not MR_HAVE_LONG_LONG */

  /* oh well, guess we have to do it the hard way :-( */

  typedef struct MR_dword
  {
	unsigned long	low_word;
	unsigned long	high_word;
  } MR_dword;

  #include <limits.h>

  #define MR_HIGHWORD_TO_DOUBLE	(((double) ULONG_MAX) + 1.0)

  #define MR_convert_dword_to_double(dword_form, double_form) 		\
	do {								\
		double_form = (MR_HIGHWORD_TO_DOUBLE			\
			* (double) (dword_form).high_word) 		\
			+ (double) (dword_form).low_word;		\
	} while (0)

  #define MR_zero_dword(dword) 						\
	do { 								\
		(dword).low_word = 0;					\
		(dword).high_word = 0;					\
	} while (0)

  #define MR_increment_dword(dword, inc) 				\
	do { 								\
		unsigned long	old_low_word = (dword).low_word;	\
		(dword).low_word += (inc);				\
		if ((dword).low_word < old_low_word) {			\
			(dword).high_word += 1;				\
		}							\
	} while (0)

  #define MR_add_two_dwords(src_dest_dword, src_dword) 			\
	do { 								\
		unsigned long	old_low_word = (src_dest_dword).low_word; \
		(src_dest_dword).low_word += (src_dword).low_word;	\
		if ((src_dest_dword).low_word < old_low_word) {		\
			(src_dest_dword).high_word += 1;		\
		}							\
		(src_dest_dword).high_word += (src_dword).high_word;	\
	} while (0)

#endif /* not MR_HAVE_LONG_LONG */

/*---------------------------------------------------------------------------*/

/* type declarations */

/*
** We count memory allocation in units of
**	- cells (i.e. individual allocations), and
**	- words
**
** We keep track of how much allocation occurs in each "period".
** A period ends (and a new period begins) at each call to
** report_stats or report_full_memory_stats in library/benchmarking.m.
** We also keep track of the total allocation over all periods.
**
** We keep track of how much memory was allocated
**	- by each procedure,
**	- for objects of each type,
**	- and an overall total
**
** The tables of counters for each procedure is represented
** as a binary search tree.  Similarly for the table of counters
** for each type.
*/

typedef	struct MR_memprof_counter
{
	MR_dword	cells_at_period_start;
	MR_dword	words_at_period_start;
	MR_dword	cells_since_period_start;
	MR_dword	words_since_period_start;
} MR_memprof_counter;

/* type representing a binary tree node */
typedef	struct MR_memprof_record
{
	const char			*name; /* of the type or procedure */
	MR_Code				*addr; /* for procedures only */
	MR_memprof_counter		counter;
	struct MR_memprof_record	*left;	/* left sub-tree */
	struct MR_memprof_record	*right;	/* right sub-tree */
} MR_memprof_record;

/* type representing a binary tree */
typedef	struct MR_memprof_table
{
	MR_memprof_record		*root;
	int				num_entries;
} MR_memprof_table;

/*---------------------------------------------------------------------------*/

/* global variables */

extern	MR_memprof_counter	MR_memprof_overall;
extern	MR_memprof_table	MR_memprof_procs;
extern	MR_memprof_table	MR_memprof_types;

/*---------------------------------------------------------------------------*/

/* function declarations */

/*
** MR_record_allocation(size, proc_addr, proc_name, type):
**	Record heap profiling information for an allocation of one cell
**	of `size' words by procedure `proc_name' with address `proc_addr'
**	for an object of type `type'.
**	The heap profiling information is recorded in the three global
**	variables above.
*/
extern void MR_record_allocation(int size, MR_Code *proc_addr,
		const char *proc_name, const char *type);

/*
** MR_prof_output_mem_tables():
**	Write out the information recorded by MR_record_allocation()
**	to a pair of files `Prof.MemoryMR_Words' and `Prof.MemoryCells'.
*/
extern void MR_prof_output_mem_tables(void);

/*---------------------------------------------------------------------------*/

#endif /* MERCURY_HEAP_PROFILE_H */

/*---------------------------------------------------------------------------*/
