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
#include	"imp.h"

#include	<signal.h>
#include	<sys/param.h>
#include	<sys/time.h>
#include	<unistd.h>

	Code		*prof_current_proc;
static	FILE	 	*declfptr = NULL;
static	prof_call_node	*addr_pair_table[CALL_TABLE_SIZE] = {NULL};
static	prof_time_node	*addr_table[TIME_TABLE_SIZE] = {NULL};

/* ======================================================================== */

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
	if ( !(fptr = fopen("addr.out", "w")) ) {
		fprintf(stderr, "%s %s", "prof_init_time_profile: Couldn't ",
			"open the file 'addr.out'!\n");
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
	
/* ======================================================================== */


/*
**	prof_output_addr_pair_table :
**		Writes the hash table to a file called "addrpair.out".
**		Callee then caller followed by count.
*/
void prof_output_addr_pair_table(void)
{
	FILE *fptr;
	int  i;
	prof_call_node *current;

	if ( (fptr = fopen("addrpair.out", "w")) ) {
		for (i = 0; i < CALL_TABLE_SIZE ; i++) {
			current = addr_pair_table[i];
			while (current) {
				fprintf(fptr, "%p %p %lu\n", current->Callee,
					current->Caller, current->count);
				current = current->next;
			}
		}
	}
	else {
		fprintf(stderr, "%p\nCouldn't create addrpair.out\n", fptr);
		exit(1);
	}

}


/* ======================================================================== */


/*
**	prof_output_addr_decls:
**		Ouputs the main predicate labels as well as their machine
**		addresses to a file called "addrdecl.out".
**		At the moment I think the best place to insert this call
**		is in the makeentry call in label.c
*/
void prof_output_addr_decls(const char *name, const Code *address)
{
	if (declfptr) {
		fprintf(declfptr, "%p\t%s\n", address, name);
	}
	else {
		if ( (declfptr = fopen("addrdecl.out", "w") ) ) {
			fprintf(declfptr, "%p\t%s\n", address, name);
		}
		else {
			fprintf(stderr, "\nCouldn't create addrdecl.out\n");
			exit(1);
		}
	}
	return;
}


/* ======================================================================== */


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
		fprintf(stderr, "%p\nCouldn't create addrpair.out\n", fptr);
		exit(1);
	}

}
