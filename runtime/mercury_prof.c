/*
** Copyright (C) 1995-1998, 2000-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
**      Profiling module
**
**	Main Author : petdr
*/

#include        "mercury_imp.h"

#ifdef MR_HAVE_UNISTD_H
  #include	<unistd.h>
#endif

#include	<stdio.h>
#include	<errno.h>
#include	<string.h>

#include	"mercury_prof.h"
#include	"mercury_heap_profile.h" /* for MR_prof_output_mem_tables() */
#include	"mercury_prof_time.h"	 /* for MR_turn_on_time_profiling() */
#include	"mercury_prof_mem.h"	 /* for prof_malloc() */

#include	"mercury_signal.h"
#include        "mercury_std.h"
#include	"mercury_timing.h"
#include	"mercury_runtime_util.h"

#include	<signal.h>		/* for SIGINT */

/*
** XXX Ought to make these command line options
*/
#define CALL_TABLE_SIZE 4096
#define TIME_TABLE_SIZE 4096

/*
** profiling node information 
*/

typedef struct s_prof_call_node {
        MR_Code			*Callee;
        MR_Code			*Caller;
        unsigned long		count;
        struct s_prof_call_node	*left;
        struct s_prof_call_node	*right;
} prof_call_node;

typedef struct s_prof_time_node {
        MR_Code			*Addr;
        unsigned long		count;
        struct s_prof_time_node	*left;
        struct s_prof_time_node	*right;
} prof_time_node;

/* 
** Macro definitions 
*/

#define hash_addr_pair(Callee, Caller)                                      \
        (int) ((( (unsigned long)(Callee) ^ (unsigned long)(Caller) ) >> 2) \
                % CALL_TABLE_SIZE )

#define hash_prof_addr(Addr)                                                \
	(int) ( (( (unsigned long)(Addr) ) >> 2) % TIME_TABLE_SIZE )


/*
** Global Variables
*/

MR_Code *		volatile	MR_prof_current_proc;

#ifdef MR_MPROF_PROFILE_CALLS
  MR_Code *		MR_prof_ho_caller_proc;
#endif


/* 
** Private global variables
*/

static volatile	int		in_profiling_code = MR_FALSE;

#ifdef MR_MPROF_PROFILE_CALLS
  static FILE 		*MR_prof_decl_fptr = NULL;
  static prof_call_node	*addr_pair_table[CALL_TABLE_SIZE] = {NULL};
#endif

#ifdef MR_MPROF_PROFILE_TIME
  static prof_time_node	*addr_table[TIME_TABLE_SIZE] = {NULL};
#endif

/*
** Local function declarations
*/

#ifdef MR_MPROF_PROFILE_TIME
  static void prof_handle_tick(int);
  static void prof_output_addr_table(void);
  static void print_time_node(FILE *fptr, prof_time_node *node);
#endif

#ifdef MR_MPROF_PROFILE_CALLS
  static void print_addr_pair_node(FILE *fptr, prof_call_node *node);
  static void prof_output_addr_pair_table(void);
#endif

#ifdef MR_MPROF_PROFILE_MEMORY
  static void prof_output_mem_tables(void);
  static void print_memory_node(FILE *words_fptr, FILE *cells_fptr,
		MR_memprof_record *node);
#endif

#if defined(MR_MPROF_PROFILE_TIME) || defined(MR_MPROF_PROFILE_CALLS) \
		|| defined(MR_MPROF_PROFILE_MEMORY)
  static void prof_handle_sigint(void);
#endif

/* ======================================================================== */

#ifdef MR_MPROF_PROFILE_CALLS

/*
**	prof_call_profile:
**		Saves the callee, caller pair into a hash table. If the
**		address pair already exists then it increments a count.
*/

void
MR_prof_call_profile(MR_Code *Callee, MR_Code *Caller)
{
	prof_call_node	*node, **node_addr, *new_node;
	int		 hash_value;

	in_profiling_code = MR_TRUE;

	hash_value = hash_addr_pair(Callee, Caller);

	node_addr = &addr_pair_table[hash_value];
	while ((node = *node_addr) != NULL) {
		if (node->Callee < Callee) {
			node_addr = &node->left;
		} else if (node->Callee > Callee) {
			node_addr = &node->right;
		} else if (node->Caller < Caller) {
			node_addr = &node->left;
		} else if (node->Caller > Caller) {
			node_addr = &node->right;
		} else {
			node->count++;
			in_profiling_code = MR_FALSE;
			return;
		}
	}

	new_node = MR_PROF_NEW(prof_call_node);
	new_node->Callee = Callee;
	new_node->Caller = Caller;
	new_node->count  = 1;
	new_node->left   = NULL;
	new_node->right  = NULL;
	*node_addr = new_node;

	in_profiling_code = MR_FALSE;
	return;
}

#endif /* MR_MPROF_PROFILE_CALLS */

/* ======================================================================== */

#ifdef MR_MPROF_PROFILE_TIME

/*
**	prof_handle_tick:
**		Signal handler to be called whenever a profiling signal is
**		received. Saves the current code address into a hash table.
**		If the address already exists, it increments its count.
*/

static void
prof_handle_tick(int signum)
{
	prof_time_node	*node, **node_addr, *new_node;
	int		hash_value;
	MR_Code 		*current_proc;

	/* Ignore any signals we get in this function or in prof_call_profile */
	if (in_profiling_code) {
		return;
	}

	in_profiling_code = MR_TRUE;

	current_proc = MR_prof_current_proc;
	hash_value = hash_prof_addr(current_proc);

	node_addr = &addr_table[hash_value];
	while ((node = *node_addr) != NULL) {
		if (node->Addr < current_proc) {
			node_addr = &node->left;
		} else if (node->Addr > current_proc) {
			node_addr = &node->right;
		} else {
			node->count++;
			in_profiling_code = MR_FALSE;
			return;
		}
	}

	new_node = MR_PROF_NEW(prof_time_node);
	new_node->Addr  = current_proc;
	new_node->count = 1;
	new_node->left  = NULL;
	new_node->right = NULL;
	*node_addr = new_node;

	in_profiling_code = MR_FALSE;
	return;
} /* end prof_handle_tick() */

#endif /* MR_MPROF_PROFILE_TIME */

/* ======================================================================== */

#ifdef MR_MPROF_PROFILE_CALLS

/*
**	prof_output_addr_pair_table :
**		Writes the hash table to a file called "Prof.CallPair".
**		Caller then callee followed by count.
*/

static void
prof_output_addr_pair_table(void)
{
	FILE	*fptr;
	int	i;

	fptr = MR_checked_fopen("Prof.CallPair", "create", "w");

	for (i = 0; i < CALL_TABLE_SIZE ; i++) {
		print_addr_pair_node(fptr, addr_pair_table[i]);
	}

	MR_checked_fclose(fptr, "Prof.CallPair");
}

static void
print_addr_pair_node(FILE *fptr, prof_call_node *node)
{
	if (node != NULL) {
		fprintf(fptr, "%ld %ld %lu\n",
			(long) node->Caller, (long) node->Callee, node->count);
		print_addr_pair_node(fptr, node->left);
		print_addr_pair_node(fptr, node->right);
	}
}

#endif /* MR_MPROF_PROFILE_CALLS */

/* ======================================================================== */

#if defined(MR_MPROF_PROFILE_CALLS)

/*
**	prof_output_addr_decl:
**		Ouputs an entry label name and its corresponding machine
**		address to a file called "Prof.Decl".
**		This is called from insert_entry() in mercury_label.c.
*/

void
MR_prof_output_addr_decl(const char *name, const MR_Code *address)
{
	if (!MR_prof_decl_fptr) {
		MR_prof_decl_fptr =
			MR_checked_fopen("Prof.Decl", "create", "w");
	}
	fprintf(MR_prof_decl_fptr, "%ld\t%s\n", (long) address, name);
}

#endif /* MR_MPROF_PROFILE_CALLS */

/* ======================================================================== */

#ifdef MR_MPROF_PROFILE_TIME

/*
**	prof_output_addr_table:
**		Writes out the time profiling counts recorded
**		by the profile signal handler to the file `Prof.Counts'.
*/

static void
prof_output_addr_table(void)
{
	FILE *fptr;
	int  i;
	double scale;
	double rate;

	fptr = MR_checked_fopen("Prof.Counts", "create", "w");

	/*
	** Write out header line indicating what we are profiling,
	** the scale factor, and the units.
	** The scale factor is the time per profiling interrupt.
	** The units are seconds.
	*/
	scale = (double) MR_CLOCK_TICKS_PER_PROF_SIG /
		(double) MR_CLOCK_TICKS_PER_SECOND;
	fprintf(fptr, "%s %f %s\n", MR_time_method, scale, "seconds");

	/*
	** Write out the time profiling data: one one-line entry per node.
	*/
	for (i = 0; i < TIME_TABLE_SIZE ; i++) {
		print_time_node(fptr, addr_table[i]);
	}

	MR_checked_fclose(fptr, "Prof.Counts");
}

static void
print_time_node(FILE *fptr, prof_time_node *node)
{
	if (node != (prof_time_node *) NULL) {
		fprintf(fptr, "%ld %lu\n", (long) node->Addr, node->count);
		print_time_node(fptr, node->left);
		print_time_node(fptr, node->right);
	}
}

#endif /* MR_MPROF_PROFILE_TIME */

/* ======================================================================== */

#ifdef MR_MPROF_PROFILE_MEMORY

/*
**	prof_output_mem_tables:
**		Writes the by-procedure memory profiling counts to the files
**		`Prof.MemoryWords' and `Prof.MemoryCells'.
*/

static void
prof_output_mem_tables(void)
{
	FILE *words_fptr;
	FILE *cells_fptr;
	int  i;

	words_fptr = MR_checked_fopen("Prof.MemoryWords", "create", "w");
	cells_fptr = MR_checked_fopen("Prof.MemoryCells", "create", "w");

	fprintf(words_fptr, "%s %f %s\n",
		"memory-words", 0.001, "kilowords");
	fprintf(cells_fptr, "%s %f %s\n",
		"memory-cells", 0.001, "kilocells");

	print_memory_node(words_fptr, cells_fptr, MR_memprof_procs.root);

	MR_checked_fclose(words_fptr, "Prof.MemoryWords");
	MR_checked_fclose(cells_fptr, "Prof.MemoryCells");
}

static void
print_memory_node(FILE *words_fptr, FILE *cells_fptr, MR_memprof_record *node)
{
	if (node != NULL) {
		MR_Dword cells, words;
		double	 cells_double, words_double;

		cells = node->counter.cells_at_period_start;
		words = node->counter.words_at_period_start;

		MR_add_two_dwords(cells,
			node->counter.cells_since_period_start);
		MR_add_two_dwords(words,
			node->counter.words_since_period_start);

		MR_convert_dword_to_double(words, words_double);
		MR_convert_dword_to_double(cells, cells_double);

		fprintf(words_fptr, "%ld %.0f\n",
			(long) node->addr, words_double);
		fprintf(cells_fptr, "%ld %.0f\n",
			(long) node->addr, cells_double);

		print_memory_node(words_fptr, cells_fptr, node->left);
		print_memory_node(words_fptr, cells_fptr, node->right);
	}
}

#endif /* MR_MPROF_PROFILE_MEMORY */

/* ======================================================================== */

void
MR_prof_init(void)
{
#ifdef MR_MPROF_PROFILE_TIME
	MR_init_time_profile_method();
#endif

#if defined(MR_MPROF_PROFILE_TIME) || defined(MR_MPROF_PROFILE_CALLS) \
		|| defined(MR_MPROF_PROFILE_MEMORY)
	MR_checked_atexit(MR_prof_finish);
  #ifdef SIGINT
	MR_setup_signal(SIGINT, prof_handle_sigint, MR_FALSE,
		"cannot install signal handler");
  #endif
#endif
}

#if defined(MR_MPROF_PROFILE_TIME) || defined(MR_MPROF_PROFILE_CALLS) \
		|| defined(MR_MPROF_PROFILE_MEMORY)
static void
prof_handle_sigint(void)
{
	/*
	** exit() will call MR_prof_finish(), which we registered
	** with atexit().
	*/
	exit(EXIT_FAILURE);
}
#endif

void
MR_prof_finish(void)
{
	/* ensure this routine only gets run once, even if called twice */
	static MR_bool done = MR_FALSE;
	if (done) return;
	done = MR_TRUE;

#ifdef MR_MPROF_PROFILE_CALLS
	prof_output_addr_pair_table();
#endif

#ifdef MR_MPROF_PROFILE_TIME
	MR_turn_off_time_profiling();
	prof_output_addr_table();
#endif

#ifdef MR_MPROF_PROFILE_MEMORY
	prof_output_mem_tables();
#endif
}

void MR_close_prof_decl_file(void)
{
#ifdef MR_MPROF_PROFILE_CALLS
	if (MR_prof_decl_fptr) {
		MR_checked_fclose(MR_prof_decl_fptr, "Prof.Decl");
	}
#endif
}

#ifdef MR_MPROF_PROFILE_TIME

void
MR_prof_turn_on_time_profiling(void)
{
	MR_turn_on_time_profiling(prof_handle_tick);
}

void
MR_prof_turn_off_time_profiling(void)
{
	MR_turn_off_time_profiling();
}

#endif

/* ======================================================================== */
