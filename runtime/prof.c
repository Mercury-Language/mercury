/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
**      Profiling module
**
**	Main Author : petdr
*/

#include        "prof.h"
#include        "std.h"

#include	<unistd.h>
#include	<errno.h>
#include	<string.h>

#if defined(PROFILE_TIME)

#include	<signal.h>

#ifdef HAVE_SYS_PARAM
#include	<sys/param.h>
#endif

#ifdef HAVE_SYS_TIME
#include	<sys/time.h>
#endif

#if !defined(HZ) || !defined(SIGPROF) || !defined(HAVE_SETITIMER)
#error "Time profiling not supported on this system"
#endif

#endif	/* PROFILE_TIME */

/*******************
  Need to make these command line options
*******************/
#define CALL_TABLE_SIZE 4096
#define TIME_TABLE_SIZE 4096
#define CLOCK_TICKS     5

#define USEC            1000000

/*
** profiling node information 
*/
typedef struct s_prof_call_node
{
        Code *Callee, *Caller;
        unsigned long count;
        struct s_prof_call_node *next;
} prof_call_node;

typedef struct s_prof_time_node
{
        Code *Addr;
        unsigned long count;
        struct s_prof_time_node *next;
} prof_time_node;


/* 
** Macro definitions 
*/
#define hash_addr_pair(Callee, Caller)                                      \
        (int) ((( (unsigned long)(Callee) ^ (unsigned long)(Caller) ) >> 2) \
                % CALL_TABLE_SIZE )

#define hash_prof_addr(Addr)                                                \
        (int) ( (unsigned long)(Addr) % TIME_TABLE_SIZE )


/*
** Global Variables
*/
	Code		*prof_current_proc;

/* 
** Private global variables
*/
static	FILE	 	*declfptr = NULL;
static	prof_call_node	*addr_pair_table[CALL_TABLE_SIZE] = {NULL};
#ifdef PROFILE_TIME
static	prof_time_node	*addr_table[TIME_TABLE_SIZE] = {NULL};
#endif

/* ======================================================================== */

#ifdef PROFILE_TIME

/*
**	prof_init_time_profile:
**		Writes the value of HZ (no. of ticks per second.) at the start
**		of the file 'addr.out'.
**		Then sets up the profiling timer and start's it up. 
**		At the moment it is after every X ticks of the clock.
**		SYSTEM SPECIFIC CODE
*/

void prof_init_time_profile()
{
	FILE 	*fptr = NULL;
	struct itimerval *itime = NULL;

	/* output the value of HZ */
	errno = 0;
	if ( !(fptr = fopen("addr.out", "w")) ) {
		fprintf(stderr, "%s %s\n%s\n", "Mercury runtime: Couldn't open",
			"the file 'addr.out'!", strerror(errno));
		exit(1);
	}

	fprintf(fptr, "%d\n", HZ);
	fclose(fptr);

	itime = make(struct itimerval);
	itime->it_value.tv_sec = 0;
	itime->it_value.tv_usec = (long) (USEC / HZ) * CLOCK_TICKS; 
	itime->it_interval.tv_sec = 0;
	itime->it_interval.tv_usec = (long) (USEC / HZ) * CLOCK_TICKS;

	signal(SIGPROF, prof_time_profile);
	setitimer(ITIMER_PROF, itime, NULL);
}

#endif /* PROFILE_TIME */

/* ======================================================================== */

/*
**	prof_call_profile:
**		Saves the callee, caller pair into a hash table. If the
**		address pair already exists then it increments a count.
*/

void prof_call_profile(Code *Callee, Code *Caller)
{
        prof_call_node *temp, *prev, *new_node;
	int indice;

	indice = hash_addr_pair(Callee, Caller);

        temp = prev = addr_pair_table[indice];

	/* Special case of when pointer in array is NULL */
	if (!temp) {
		new_node = make(prof_call_node);
		new_node->Callee = Callee;
		new_node->Caller = Caller;
		new_node->count = 1;
		new_node->next = NULL;
		addr_pair_table[indice] = new_node;
		return;
	}

        while (temp) {
                if ( (temp->Callee == Callee) && (temp->Caller == Caller) ) {
                        temp->count++;
                        return;
                }
                prev = temp;
                temp = temp->next;
        }

        new_node = make(prof_call_node);
        new_node->Callee = Callee;
        new_node->Caller = Caller;
        new_node->count = 1;
        new_node->next = NULL;
        prev->next = new_node;

        return;
}

/* ======================================================================== */

#ifdef PROFILE_TIME

/*
**	prof_time_profile:
**		Signal handler to be called when ever a SIGPROF is received.
**		Saves the current code address into a hash table.  If the
**		address already exists, it increments it's count.
*/

void prof_time_profile(int signum)
{
        prof_time_node *temp, *prev, *new_node;
        int indice;

        indice = hash_prof_addr(prof_current_proc);

        temp = prev = addr_table[indice];

        /* Special case of when pointer in array is NULL */
        if (!temp) {
                new_node = make(prof_time_node);
                new_node->Addr = prof_current_proc;
                new_node->count = 1;
                new_node->next = NULL;
                addr_table[indice] = new_node;
		signal(SIGPROF, prof_time_profile);
                return;
        }

        while (temp) {
                if ( (temp->Addr == prof_current_proc) ) {
                        temp->count++;
			signal(SIGPROF, prof_time_profile);
                        return;
                }
                prev = temp;
                temp = temp->next;
        }

        new_node = make(prof_time_node);
        new_node->Addr = prof_current_proc;
        new_node->count = 1;
        new_node->next = NULL;
        prev->next = new_node;

	signal(SIGPROF, prof_time_profile);
        return;
}

/* ======================================================================== */

/*
**	prof_turn_off_time_profiling:
**		Turns off the time profiling.
*/

void prof_turn_off_time_profiling()
{
	struct itimerval *itime = NULL;

	itime = make(struct itimerval);
        itime->it_value.tv_sec = 0;
        itime->it_value.tv_usec = 0;
        itime->it_interval.tv_sec = 0;
        itime->it_interval.tv_usec = 0;

        setitimer(ITIMER_PROF, itime, NULL);
}
	
#endif /* PROFILE_TIME */

/* ======================================================================== */

/*
**	prof_output_addr_pair_table :
**		Writes the hash table to a file called "addrpair.out".
**		Caller then callee followed by count.
*/

void prof_output_addr_pair_table(void)
{
	FILE *fptr;
	int  i;
	prof_call_node *current;
	errno = 0;
	if ( (fptr = fopen("addrpair.out", "w")) ) {
		for (i = 0; i < CALL_TABLE_SIZE ; i++) {
			current = addr_pair_table[i];
			while (current) {
				fprintf(fptr, "%p %p %lu\n", current->Caller,
					current->Callee, current->count);
				current = current->next;
			}
		}
	}
	else {
		fprintf(stderr, "%s %s\n%s\n", "Mercury runtime: Couldn't",
			"create addrpair.out", strerror(errno));
		exit(1);
	}

}

/* ======================================================================== */

/*
**	prof_output_addr_decls:
**		Ouputs the main predicate labels as well as their machine
**		addresses to a file called "addrdecl.out".
**		At the moment I think the best place to insert this call
**		is in the insert_entry call in label.c
*/

void prof_output_addr_decls(const char *name, const Code *address)
{
	if (declfptr) {
		fprintf(declfptr, "%p\t%s\n", address, name);
	}
	else {
		errno = 0;
		if ( (declfptr = fopen("addrdecl.out", "w") ) ) {
			fprintf(declfptr, "%p\t%s\n", address, name);
		}
		else {
			fprintf(stderr, "%s %s\n%s\n", "Mercury runtime:",
			       "Couldn't create addrdecl.out", strerror(errno));
			exit(1);
		}
	}
	return;
}

/* ======================================================================== */

#ifdef PROFILE_TIME

/*
**	prof_output_addr_table:
**		Outputs the addresses saved whenever SIGPROF is received to
**		the file "addr.out"
*/

void prof_output_addr_table()
{
	FILE *fptr;
	int  i;
	prof_time_node *current;

	errno = 0;
	if ( (fptr = fopen("addr.out", "a")) ) {
		for (i = 0; i < TIME_TABLE_SIZE ; i++) {
			current = addr_table[i];
			while (current) {
				fprintf(fptr, "%p %lu\n", current->Addr,
					current->count);
				current = current->next;
			}
		}
	}
	else {
		fprintf(stderr, "%s %s\n%s\n", "Mercury runtime: Couldn't",
				"create addrpair.out", strerror(errno));
		exit(1);
	}
}

#endif /* PROFILE_TIME */
