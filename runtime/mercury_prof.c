/*
** Copyright (C) 1995-1998,2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
**      Profiling module
**
**	Main Author : petdr
*/

#include        "mercury_imp.h"

#ifdef HAVE_UNISTD_H
  #include	<unistd.h>
#endif

#include	<stdio.h>
#include	<errno.h>
#include	<string.h>

#include	"mercury_prof.h"
#include	"mercury_heap_profile.h" /* for MR_prof_output_mem_tables() */
#include	"mercury_prof_mem.h"	 /* for prof_malloc() */

#include	"mercury_signal.h"
#include        "mercury_std.h"
#include	"mercury_timing.h"

#if defined(PROFILE_TIME)

#include	<signal.h>

#ifdef HAVE_SYS_TIME
#include	<sys/time.h>
#endif

#if !defined(MR_CLOCK_TICKS_PER_SECOND) \
	|| !defined(HAVE_SETITIMER)
  #error "Time profiling not supported on this system"
#endif

static int	MR_itimer_sig;
static int	MR_itimer_type;
static const char * MR_time_method;

#endif	/* PROFILE_TIME */

/*
** XXX Ought to make these command line options
*/
#define CALL_TABLE_SIZE 4096
#define TIME_TABLE_SIZE 4096
#define MR_CLOCK_TICKS_PER_PROF_SIG	5

#define MR_USEC_PER_SEC            	1000000

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

/* 
** Private global variables
*/

static volatile	int		in_profiling_code = FALSE;


#ifdef PROFILE_CALLS
  FILE 			*MR_prof_decl_fptr = NULL;
  static prof_call_node	*addr_pair_table[CALL_TABLE_SIZE] = {NULL};
#endif

#ifdef PROFILE_TIME
  static bool		time_profiling_on = FALSE;
  static prof_time_node	*addr_table[TIME_TABLE_SIZE] = {NULL};
#endif

/*
** Local function declarations
*/

#ifdef PROFILE_TIME
  static void prof_init_time_profile_method(void);
  static void prof_time_profile(int);
  static void prof_output_addr_table(void);
  static void print_time_node(FILE *fptr, prof_time_node *node);
#endif

#ifdef PROFILE_CALLS
  static void print_addr_pair_node(FILE *fptr, prof_call_node *node);
  static void prof_output_addr_pair_table(void);
#endif

#ifdef PROFILE_MEMORY
  static void prof_output_mem_tables(void);
  static void print_memory_node(FILE *words_fptr, FILE *cells_fptr,
		MR_memprof_record *node);
#endif


/* ======================================================================== */

#ifndef HAVE_STRERROR

/*
** Apparently SunOS 4.1.3 doesn't have strerror()
**	(!%^&!^% non-ANSI systems, grumble...)
**
** This code should perhaps go somewhere other than in prof.c.
*/

extern int sys_nerr;
extern char *sys_errlist[];

char *
strerror(int errnum)
{
	if (errnum >= 0 && errnum < sys_nerr && sys_errlist[errnum] != NULL) {
		return sys_errlist[errnum];
	} else {
		static char buf[30];
		sprintf(buf, "Error %d", errnum);
		return buf;
	}
}

#endif

/* ======================================================================== */

/* utility routines for opening and closing files */

#if defined(PROFILE_TIME) || defined(PROFILE_CALLS) || defined(PROFILE_MEMORY)

static FILE *
checked_fopen(const char *filename, const char *message, const char *mode)
{
	FILE *file;

	errno = 0;
	file = fopen(filename, mode);
	if (!file) {
		fprintf(stderr, "Mercury runtime: couldn't %s file `%s': %s\n",
				message, filename, strerror(errno));
		exit(1);
	}
	return file;
}

static void
checked_fclose(FILE *file, const char *filename)
{
	errno = 0;
	if (fclose(file) != 0) {
		fprintf(stderr,
			"Mercury runtime: error closing file `%s': %s\n",
			filename, strerror(errno));
		exit(1);
	}
}

static void
checked_atexit(void (*func)(void))
{
	errno = 0;
	if (atexit(func) != 0) {
		fprintf(stderr,
			"Mercury runtime: error in call to atexit: %s\n",
			strerror(errno));
		exit(1);
	}
}

#endif /* PROFILE_TIME or PROFILE_CALLS or PROFILE_MEMORY */

/* ======================================================================== */

#ifdef	PROFILE_TIME

static void
checked_setitimer(int which, struct itimerval *value)
{
	errno = 0;
	if (setitimer(which, value, NULL) != 0) {
		perror("Mercury runtime: cannot set timer for profiling");
		exit(1);
	}
}

/*
**	prof_turn_on_time_profiling:
**		Sets up the profiling timer and starts it up. 
**		At the moment it is after every MR_CLOCK_TICKS_PER_PROF_SIG
**		ticks of the clock.
**
**		WARNING: SYSTEM SPECIFIC CODE.  
**		This code is not very portable, because it uses setitimer(),
**		which is not part of POSIX.1 or ANSI C.
*/

void
MR_prof_turn_on_time_profiling(void)
{
	FILE 	*fptr;
	struct itimerval itime;
	const long prof_sig_interval_in_usecs = MR_CLOCK_TICKS_PER_PROF_SIG *
		(MR_USEC_PER_SEC / MR_CLOCK_TICKS_PER_SECOND);

	time_profiling_on = TRUE;

	itime.it_value.tv_sec = 0;
	itime.it_value.tv_usec = prof_sig_interval_in_usecs;
	itime.it_interval.tv_sec = 0;
	itime.it_interval.tv_usec = prof_sig_interval_in_usecs;

	MR_setup_signal(MR_itimer_sig, prof_time_profile, FALSE,
		"Mercury runtime: cannot install signal handler");
	checked_setitimer(MR_itimer_type, &itime);
}

/*
**	prof_init_time_profile_method:
**		initializes MR_itimer_type and MR_itimer_sig
**		based on the setting of MR_time_profile_method.
*/
static void
prof_init_time_profile_method(void)
{
	switch (MR_time_profile_method) {
#if defined(ITIMER_REAL) && defined(SIGALRM)
		case MR_profile_real_time:
			MR_itimer_type = ITIMER_REAL;
			MR_itimer_sig  = SIGALRM;
			MR_time_method = "real-time";
			break;
#endif
#if defined(ITIMER_VIRTUAL) && defined(SIGVTALRM)
		case MR_profile_user_time:
			MR_itimer_type = ITIMER_VIRTUAL;
			MR_itimer_sig  = SIGVTALRM;
			MR_time_method = "user-time";
			break;
#endif
#if defined(ITIMER_VIRTUAL) && defined(SIGVTALRM)
		case MR_profile_user_plus_system_time:
			MR_itimer_type = ITIMER_PROF;
			MR_itimer_sig  = SIGPROF;
			MR_time_method = "user-plus-system-time";
			break;
#endif
		default:
			fatal_error("invalid time profile method");
	}
}
#endif /* PROFILE_TIME */

/* ======================================================================== */

#ifdef PROFILE_CALLS

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

	in_profiling_code = TRUE;

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
			in_profiling_code = FALSE;
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

	in_profiling_code = FALSE;
	return;
}

#endif /* PROFILE_CALLS */

/* ======================================================================== */

#ifdef PROFILE_TIME

/*
**	prof_time_profile:
**		Signal handler to be called whenever a profiling signal is
**		received. Saves the current code address into a hash table.
**		If the address already exists, it increments its count.
*/

static void
prof_time_profile(int signum)
{
	prof_time_node	*node, **node_addr, *new_node;
	int		hash_value;
	MR_Code 		*current_proc;

	/* Ignore any signals we get in this function or in prof_call_profile */
	if (in_profiling_code) {
		return;
	}

	in_profiling_code = TRUE;

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
			in_profiling_code = FALSE;
			return;
		}
	}

	new_node = MR_PROF_NEW(prof_time_node);
	new_node->Addr  = current_proc;
	new_node->count = 1;
	new_node->left  = NULL;
	new_node->right = NULL;
	*node_addr = new_node;

	in_profiling_code = FALSE;
	return;
} /* end prof_time_profile() */

/* ======================================================================== */

/*
**	prof_turn_off_time_profiling:
**		Turns off the time profiling.
*/

void
MR_prof_turn_off_time_profiling(void)
{
	struct itimerval itime;

	if (time_profiling_on == FALSE)
		return;

	itime.it_value.tv_sec = 0;
	itime.it_value.tv_usec = 0;
	itime.it_interval.tv_sec = 0;
	itime.it_interval.tv_usec = 0;

	checked_setitimer(MR_itimer_type, &itime);
}
	
#endif /* PROFILE_TIME */

/* ======================================================================== */

#ifdef PROFILE_CALLS

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

	fptr = checked_fopen("Prof.CallPair", "create", "w");

	for (i = 0; i < CALL_TABLE_SIZE ; i++) {
		print_addr_pair_node(fptr, addr_pair_table[i]);
	}

	checked_fclose(fptr, "Prof.CallPair");
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

#endif /* PROFILE_CALLS */

/* ======================================================================== */

#if defined(PROFILE_CALLS)

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
		MR_prof_decl_fptr = checked_fopen("Prof.Decl", "create", "w");
	}
	fprintf(MR_prof_decl_fptr, "%ld\t%s\n", (long) address, name);
}

#endif /* PROFILE_CALLS */

/* ======================================================================== */

#ifdef PROFILE_TIME

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

	fptr = checked_fopen("Prof.Counts", "create", "w");

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

	checked_fclose(fptr, "Prof.Counts");
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

#endif /* PROFILE_TIME */

/* ======================================================================== */

#ifdef PROFILE_MEMORY

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

	words_fptr = checked_fopen("Prof.MemoryWords", "create", "w");
	cells_fptr = checked_fopen("Prof.MemoryCells", "create", "w");

	fprintf(words_fptr, "%s %f %s\n",
		"memory-words", 0.001, "kilowords");
	fprintf(cells_fptr, "%s %f %s\n",
		"memory-cells", 0.001, "kilocells");

	print_memory_node(words_fptr, cells_fptr, MR_memprof_procs.root);

	checked_fclose(words_fptr, "Prof.MemoryWords");
	checked_fclose(cells_fptr, "Prof.MemoryCells");
}

static void
print_memory_node(FILE *words_fptr, FILE *cells_fptr, MR_memprof_record *node)
{
	if (node != NULL) {
		MR_dword cells, words;

		cells = node->counter.cells_at_period_start;
		words = node->counter.words_at_period_start;

		MR_add_two_dwords(cells,
			node->counter.cells_since_period_start);
		MR_add_two_dwords(words,
			node->counter.words_since_period_start);

		if (cells.high_word || words.high_word) {
			fprintf(stderr, "Mercury runtime: memory profile "
				"counter for `%s' overflowed\n",
				node->name);
		}
		fprintf(words_fptr, "%ld %lu\n",
			(long) node->addr, words.low_word);
		fprintf(cells_fptr, "%ld %lu\n",
			(long) node->addr, cells.low_word);

		print_memory_node(words_fptr, cells_fptr, node->left);
		print_memory_node(words_fptr, cells_fptr, node->right);
	}
}

#endif /* PROFILE_MEMORY */

/* ======================================================================== */

void
MR_prof_init(void)
{
#ifdef PROFILE_TIME
	prof_init_time_profile_method();
#endif

#if defined(PROFILE_TIME) || defined(PROFILE_CALLS) || defined(PROFILE_MEMORY)
	checked_atexit(MR_prof_finish);
#endif
}

void
MR_prof_finish(void)
{
	/* ensure this routine only gets run once, even if called twice */
	static bool done = FALSE;
	if (done) return;
	done = TRUE;

#ifdef PROFILE_TIME
	MR_prof_turn_off_time_profiling();
	prof_output_addr_table();
#endif

#ifdef PROFILE_CALLS
	if (MR_prof_decl_fptr) {
		checked_fclose(MR_prof_decl_fptr, "Prof.Decl");
	}
	prof_output_addr_pair_table();
#endif

#ifdef PROFILE_MEMORY
	prof_output_mem_tables();
#endif
}

/* ======================================================================== */
