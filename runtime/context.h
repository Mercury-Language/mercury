/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** context.h - defines Mercury multithreading stuff.
**
** A Context is like a thread. It contains a detstack, a nondetstack, a trail,
** the various pointers that refer to them, a succip, and a thread-
** resumption continuation. Contexts are initally stored in a free-list.
** When one is running, the Unix process that is executing it has a pointer
** to its context structure `this_context'. When a context suspends, it
** calls `save_context(context_ptr)' which copies the context from the
** various registers and global variables into the structure refered to
** by `context_ptr'. The context contains no rN or fN registers - all
** registers are "context save" (by analogy to caller-save).
**
** When a new context is created information is passed to the new context
** on the stack. The top stackframe of the current context is copied to
** become the first det stackframe in the new process. (XXX this will need
** fixing eventually to include the nondet frame as well.)
**
** Threads can migrate transparently between multiple Unix processes.
** This is implicit since all the state of the thread is allocated in
** shared memory, and all the heaps are also in shared memory.
**
** Each Unix process has its own heap and solutions heap (both allocated
** in shared memory). This makes GC harder, but enables heap allocation
** to be done without locking.
** Each context has a copy of the heap pointer that is taken when it is
** switched out. If the Unix process' heap pointer is the same as the
** copied one when the context is switched back in, then it is safe for
** the context to do heap reclamation on failure.
**
** If PARALLEL is not defined, then everything gets executed within a
** single Unix process. No locking is required. No shared memory is
** required. Since there is only one process, no signalling is needed
** to wake suspended processes.
*/

#ifndef CONTEXT_H
#define CONTEXT_H

#include "regs.h"		/* for hp. Must come before system headers. */

#include <sys/types.h>		/* for pid_t */

#include "mercury_types.h"	/* for Word */
#include "mercury_trail.h"	/* for MR_TrailEntry */
#include "memory.h"		/* for MemoryZone */
#include "spinlock.h"		/* for SpinLock */
#include "goto.h"		/* for GOTO() */

/*
** If we have parallelism switched on (PARALLEL is defined),
** then we define how many processes should be used.
** Ultimately this should be configurable through the
** MERCURY_OPTIONS environment variable.
*/
#ifdef	PARALLEL
extern unsigned numprocs;
#endif

/*
** The number of context structures initially allocated.
** This allocation does not include the stacks that they
** refer to - only the actual context structures.
** At the moment if we need more than this number of contexts,
** we just die. In the longer term, we need to allocate more.
*/
#define INITIAL_NUM_CONTEXTS	20

/*
** The field names that correspond to virtual machine registers:
** 	sp, maxfr & curfr
** are prefixed with `context_' so that they don't get replaced
** during macro expansion.
*/
typedef struct CONTEXT Context;
struct CONTEXT {
	struct CONTEXT	*next;	
		/*
		** if this context is in the free-list `next' will point
		** to the next free context. If this context is suspended
		** waiting for a variable to become bound, `next' will point to
		** the next waiting context. If this context is runnable but not
		** currently running then `next' points to the next runnable
		** context in the runqueue.
		*/

	Code		*resume;
		/*
		** a pointer to the code at which execution should resume when
		** this context is next scheduled.
		*/

	Code		*context_succip;
		/* succip for this context */

	MemoryZone	*detstack_zone;
		/* pointer to the detstack_zone for this context */
	Word		*context_sp;
		/* saved stack pointer for this context */

	MemoryZone	*nondetstack_zone;
		/* pointer to the nondetstack_zone for this context */
	Word		*context_maxfr;
		/* saved maxfr pointer for this context */
	Word		*context_curfr;
		/* saved curfr pointer for this context */

#ifdef MR_USE_TRAIL
	MemoryZone	*trail_zone;
		/* pointer to the MR_trail_zone for this context */
	MR_TrailEntry	*context_trail_ptr;
		/* saved MR_trail_ptr for this context */
	MR_ChoicepointId context_ticket_counter;
		/* saved MR_ticket_counter for this context */
#endif

	Word		*context_hp;
		/* saved hp for this context */
	Word		*min_hp_rec;
		/*
		** this pointer marks the minimum value of hp to which we can
		** truncate the heap on backtracking. See comments before the
		** set_min_heap_reclamation_point macro (below).
		*/
};

/*
** free_context_list is a global linked list of unused context
** structures. If the MemoryZone pointers are not NULL,
** then they point to allocated MemoryZones, which will
** need to be reinitialized, but have space allocated to
** them. (see comments in memory.h about reset_zone())
*/
extern	Context **free_context_list_ptr;

/*
** the runqueue is a linked list of contexts that are
** runnable.
*/
extern	Context **runqueue_ptr;

/*
** this_context is a pointer to the currently executing
** context. the fields of this_context are not necessarily
** in sync with the real values, since *this_context only
** gets updated when save_context() gets called.
*/
extern	Context *this_context;

/* a pointer to a word used for the spinlock on the runqueue */
extern	SpinLock *runqueue_lock;

/*
** a pointer to a word used for the spinlock on the free
** context list
*/
extern	SpinLock *free_context_list_lock;

/*
** init_processes() forks new process (if necessary), and
** initializes the data-structures for managing the interactions
** between them.
*/
void	init_processes(void);

/*
** shutdown_processes() sends a signal to the other processes
** to tell them to shut down.  (NOT YET IMPLEMENTED - STUB ONLY.)
*/
void	shutdown_processes(void);

/*
** init_process_context() creates a top-level context for
** the original process, and allocates a heap and a solutions-
** heap for each process.
*/
void	init_process_context(void);

/*
** new_context() allocates and initializes a new context
** structure.
*/
Context	*new_context(void);

/*
** delete_context(ptr) returns the context structure pointed
** to by ptr to the free list, and releases resources as
** necessary.
*/
void	delete_context(Context *context);

/*
** flounder() aborts with a runtime error message. It is called if
** the runqueue becomes empty and none of the running processes are
** working - ie the computation has floundered.
*/
void	flounder(void);

/*
** procid[N] is the process id of the Nth process.
** procid[my_procnum] == getpid() == my_procid.
*/
extern	pid_t	*procid;

/*
** procwaiting[N] is true if the process procid[N] is
** suspended because the runqueue was empty when it
** called runnext().
** Although we semantically want bools here, we use
** words to ensure coherency. Since a bool may be
** smaller than a word, storing a bool may be implemented
** in a coherency-breaking manner.
** (Assuming that Words can be read and written in a
** coherent manner is sufficiently important in terms of
** simplifying the synchronization mechanisms, that
** we really need to do so -- or so says Tom, at least.
** I remain unconvinced. -Fergus.)
*/
typedef Word		AtomicBool;
extern	AtomicBool	*procwaiting;

/*
** my_procnum is the number of the current process.
** my_procnum == 0 is the original parent process.
*/
extern	int	my_procnum;
extern	pid_t	my_procid;

/* do a context switch */
Declare_entry(do_runnext);
#define	runnext()	GOTO(ENTRY(do_runnext));

/*
** schedule(Context *cptr, Code *resume):
*/
#define schedule(cptr, resume)	do {				\
		fatal_error("schedule not implemented");	\
	} while(0)

/*
** fork_new_context(Code *child, Code *parent, int numslots):
** create a new context to execute the code at `child', and
** copy the topmost `numslots' from the current stackframe.
** The new context gets put on the runqueue, and the current
** context resumes at `parent'.
*/
#define fork_new_context(child, parent, numslots) do {		\
		Context	*fork_new_context_context;		\
		int	fork_new_context_i;			\
		fork_new_context_context = new_context();	\
		for (fork_new_context_i = (numslots) ;		\
				fork_new_context_i > 0 ;	\
				fork_new_context_i--) {		\
			*(fork_new_context_context->context_sp) = \
				detstackvar(fork_new_context_i); \
			fork_new_context_context->context_sp++;	\
		}						\
		fork_new_context_context->resume = (child);	\
		schedule(fork_new_context_context, (parent));	\
	} while (0)

#ifndef	CONSERVATIVE_GC

/*
** To figure out the maximum amount of heap we can reclaim on backtracking,
** we compare hp with the context_hp.
**
** If context_hp == NULL then this is the first time this context has been
** scheduled, so the furthest back down the heap we can reclaim is to the
** current value of hp. 
**
** If hp > context_hp, another context has allocated data on the heap since
** we were last scheduled, so the furthest back that we can reclaim is to
** the current value of hp, so we set MR_min_hp_rec and the
** field of the same name in our context structure.
**
** If hp < context_hp, then another context has truncated the heap on failure.
** For this to happen, it must be the case that last time we were scheduled,
** that other context was the last one to allocate data on the heap, and we
** did not allocate any heap during that period of execution. That being the
** case, the furthest back to which we can reset the heap is to the current
** value of hp. This is a conservative approximation - it is possible that
** the current value of hp is the same as some previous value that we held,
** and we are now contiguous with our older data, so this algorithm will lead
** to holes in the heap, though GC will reclaim these.
**
** If hp == context_hp then no other process has allocated any heap since we
** were last scheduled, so we can proceed as if we had not stopped, and the
** furthest back that we can backtrack is the same as it was last time we
** were executing.
*/
#define set_min_heap_reclamation_point(ctxt)	do {		\
		if (hp != (ctxt)->context_hp 			\
			|| (ctxt)->context_hp == NULL)		\
		{						\
			MR_min_hp_rec = hp;	\
			(ctxt)->min_hp_rec = hp;\
		}						\
		else						\
		{						\
			MR_min_hp_rec =	(ctxt)->min_hp_rec;	\
		}						\
	} while (0)

#define	save_hp_in_context(ctxt)	do {		\
		(ctxt)->context_hp = hp;		\
		(ctxt)->min_hp_rec = MR_min_hp_rec;	\
	} while (0)

#else

#define set_min_heap_reclamation_point(ctxt)	do { } while (0)

#define	save_hp_in_context(ctxt)		do { } while (0)

#endif

#ifdef MR_USE_TRAIL
#define MR_IF_USE_TRAIL(x) x
#else
#define MR_IF_USE_TRAIL(x)
#endif

#define	load_context(cptr)	do {					\
		Context	*load_context_c;				\
		load_context_c = (cptr);				\
		MR_succip		= load_context_c->context_succip; \
		detstack_zone	= load_context_c->detstack_zone;	\
		MR_sp		= load_context_c->context_sp;		\
		nondetstack_zone = load_context_c->nondetstack_zone;	\
		MR_maxfr		= load_context_c->context_maxfr; \
		MR_curfr		= load_context_c->context_curfr; \
	        MR_IF_USE_TRAIL(					\
		    MR_trail_zone = load_context_c->trail_zone;		\
		    MR_trail_ptr = load_context_c->context_trail_ptr;	\
		    MR_ticket_counter =					\
				load_context_c->context_ticket_counter;	\
	    	)							\
		set_min_heap_reclamation_point(load_context_c);	\
	} while (0)

#define	save_context(cptr)	do {					\
		Context	*save_context_c;				\
		save_context_c = (cptr);				\
		save_context_c->context_succip	= MR_succip;		\
		save_context_c->detstack_zone	= detstack_zone;	\
		save_context_c->context_sp	 = MR_sp;		\
		save_context_c->nondetstack_zone = nondetstack_zone;	\
		save_context_c->context_maxfr	= MR_maxfr;		\
		save_context_c->context_curfr	= MR_curfr;		\
		MR_IF_USE_TRAIL(					\
		    save_context_c->trail_zone = MR_trail_zone;		\
		    save_context_c->context_trail_ptr = MR_trail_ptr;	\
		    save_context_c->context_ticket_counter =		\
						MR_ticket_counter;	\
		)							\
		save_hp_in_context(save_context_c);			\
	} while (0)

/*
** The following two macros join_and_terminate and join_and_continue
** both take a `sync_term' which has the following structure:
**	sync_term[SYNC_TERM_LOCK] is a SpinLock
**	sync_term[SYNC_TERM_COUNTER] is a counter for the number of
**			processes that need to synchronize before the
**			parent can proceed.
**	sync_term[SYNC_TERM_PARENT] is either NULL or a pointer to the
**			context of the parent process. If it is non-null
**			then the last process to arrive (the one that
**			decrements sync_term[SYNC_TERM_COUNTER] to 0)
**			must wake the parent.
** These terms are allocated and manipulated as normal Mercury terms by
** generated Mercury code.
*/

#define	SYNC_TERM_LOCK		0
#define	SYNC_TERM_COUNTER	1
#define	SYNC_TERM_PARENT	2

#define join_and_terminate(sync_term)	do {				\
		fatal_error("join_and_terminate not implemented");	\
	} while (0)

#define join_and_continue(sync_term, where_to)	do {			\
		fatal_error("join_and_continue not implemented");	\
	} while (0)

#endif

