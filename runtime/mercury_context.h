/*
** Copyright (C) 1997-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_context.h - defines Mercury multithreading stuff.
**
** A "context" is a Mercury thread.  (We use a different term than "thread"
** to avoid confusing Mercury threads and Posix threads.) 
** Each context is represented by a value of type MR_Context,
** which contains a detstack, a nondetstack, a trail, the various pointers
** that refer to them, a succip, and a thread-resumption continuation. 
** Contexts are initally stored in a free-list.
** When one is running, the Posix thread that is executing it has a pointer
** to its context structure `this_context'. When a context suspends, it
** calls `MR_save_context(context_ptr)' which copies the context from the
** various registers and global variables into the structure referred to
** by `context_ptr'. The context contains no rN or fN registers - all
** registers are "context save" (by analogy to caller-save).
**
** When a new context is created information is passed to the new context
** on the stack. The top stackframe of the current context is copied to
** become the first det stackframe in the new process. (XXX this will need
** fixing eventually to include the nondet frame as well.)
**
** Contexts can migrate transparently between multiple Posix threads.
**
** Each Posix thread has its own heap and solutions heap (both allocated
** in shared memory). This makes GC harder, but enables heap allocation
** to be done without locking which is very important for performance.
** Each context has a copy of the heap pointer that is taken when it is
** switched out. If the Posix thread's heap pointer is the same as the
** copied one when the context is switched back in, then it is safe for
** the context to do heap reclamation on failure.
**
** If MR_THREAD_SAFE is not defined, then everything gets executed within a
** single Posix thread. No locking is required.
*/

#ifndef MERCURY_CONTEXT_H
#define MERCURY_CONTEXT_H

#include "mercury_regs.h"		/* for MR_hp, etc. Must come before
					   system headers. */

#include <stdio.h>

#include "mercury_types.h"		/* for MR_Word */
#include "mercury_trail.h"		/* for MR_TrailEntry */
#include "mercury_memory.h"		/* for MR_MemoryZone */
#include "mercury_thread.h"		/* for MercuryLock */
#include "mercury_goto.h"		/* for MR_GOTO() */

#ifdef	MR_THREAD_SAFE
  #define MR_IF_THREAD_SAFE(x)	x
#else
  #define MR_IF_THREAD_SAFE(x)
#endif

typedef struct MR_Context_Struct MR_Context;
struct MR_Context_Struct {
	MR_Context	 *MR_ctxt_next;	
		/*
		** if this context is in the free-list `next' will point
		** to the next free context. If this context is suspended
		** waiting for a variable to become bound, `next' will point to
		** the next waiting context. If this context is runnable but not
		** currently running then `next' points to the next runnable
		** context in the runqueue.
		*/

	MR_Code		*MR_ctxt_resume;
		/*
		** a pointer to the code at which execution should resume when
		** this context is next scheduled.
		*/

#ifdef	MR_THREAD_SAFE
	MercuryThread	MR_ctxt_owner_thread;
		/*
		** The owner_thread field is used to ensure that when we
		** enter a mercury engine from C, we return to the same
		** engine. See the coments in mercury_engine.h
		*/
#endif

#ifndef MR_HIGHLEVEL_CODE
	MR_Code		*MR_ctxt_succip;
		/* succip for this context */

	MR_MemoryZone	*MR_ctxt_detstack_zone;
		/* pointer to the detstack_zone for this context */
	MR_Word		*MR_ctxt_sp;
		/* saved stack pointer for this context */

	MR_MemoryZone	*MR_ctxt_nondetstack_zone;
		/* pointer to the nondetstack_zone for this context */
	MR_Word		*MR_ctxt_maxfr;
		/* saved maxfr pointer for this context */
	MR_Word		*MR_ctxt_curfr;
		/* saved curfr pointer for this context */
  #ifdef MR_USE_MINIMAL_MODEL
	MR_MemoryZone	*MR_ctxt_genstack_zone;
		/* pointer to the genstack_zone for this context */
	MR_Integer	MR_ctxt_gen_next;
		/* saved generator stack index for this context */
	MR_MemoryZone	*MR_ctxt_cutstack_zone;
		/* pointer to the cutstack_zone for this context */
	MR_Integer	MR_ctxt_cut_next;
		/* saved cut stack index for this context */
  #endif /* MR_USE_MINIMAL_MODEL */
#endif /* !MR_HIGHLEVEL_CODE */

#ifdef	MR_USE_TRAIL
	MR_MemoryZone	*MR_ctxt_trail_zone;
		/* pointer to the MR_trail_zone for this context */
	MR_TrailEntry	*MR_ctxt_trail_ptr;
		/* saved MR_trail_ptr for this context */
	MR_ChoicepointId MR_ctxt_ticket_counter;
		/* saved MR_ticket_counter for this context */
	MR_ChoicepointId MR_ctxt_ticket_high_water;
		/* saved MR_ticket_high_water for this context */
#endif

	MR_Word		*MR_ctxt_hp;
		/* saved hp for this context */
	MR_Word		*MR_ctxt_min_hp_rec;
		/*
		** this pointer marks the minimum value of MR_hp to which we can
		** truncate the heap on backtracking. See comments before the
		** set_min_heap_reclamation_point macro (below).
		*/
};

typedef MR_Context Context;	/* for backwards compatibility */

/*
** The runqueue is a linked list of contexts that are runnable.
*/
extern		MR_Context	*MR_runqueue_head;
extern		MR_Context	*MR_runqueue_tail;
#ifdef	MR_THREAD_SAFE
  extern	MercuryLock	MR_runqueue_lock;
  extern	MercuryCond	MR_runqueue_cond;
#endif

/*
** As well as the runqueue, we maintain a linked list of contexts
** and associated file descriptors that are suspended blocked for
** reads/writes/exceptions. When the runqueue becomes empty, if
** this list is not empty then we call select and block until one
** or more of the file descriptors become ready for I/O, then
** wake the appropriate context.
** In addition, we should periodically check to see if the list of blocked
** contexts is non-empty and if so, poll to wake any contexts that
** can unblock. This, while not yielding true fairness (since this
** requires the current context to perform some yield-like action),
** ensures that it is possible for programmers to write concurrent
** programs with continuous computation and interleaved I/O dependent
** computation in a straight-forward manner. This polling is not
** currently implemented.
*/

typedef enum {
	MR_PENDING_READ  = 0x01,
	MR_PENDING_WRITE = 0x02,
	MR_PENDING_EXEC  = 0x04
} MR_WaitingMode;

typedef struct MR_PendingContext_Struct {
	struct MR_PendingContext_Struct	*next;
	MR_Context			*context;
	int				fd;
	MR_WaitingMode			waiting_mode;
} MR_PendingContext;

extern	MR_PendingContext	*MR_pending_contexts;
#ifdef	MR_THREAD_SAFE
  extern	MercuryLock	MR_pending_contexts_lock;
#endif

/*
** Initializes a context structure.
*/
extern	void		MR_init_context(MR_Context *context);

/*
** create_context() allocates and initializes a new context
** structure.
*/
extern	MR_Context 	*MR_create_context(void);

/*
** destroy_context(ptr) returns the context structure pointed
** to by ptr to the free list, and releases resources as
** necessary.
*/
extern	void		MR_destroy_context(MR_Context *context);

/*
** init_thread_stuff() initializes the lock structures for the runqueue.
*/
extern	void		MR_init_thread_stuff(void);

/*
** finialize_runqueue() finalizes the lock structures for the runqueue.
*/
extern	void		MR_finalize_runqueue(void);

/*
** flounder() aborts with a runtime error message. It is called if
** the runqueue becomes empty and none of the running processes are
** working - ie the computation has floundered.
*/
extern	void		MR_flounder(void);

/*
** schedule(MR_Context *cptr):
**	Append a context onto the end of the run queue.
*/

extern	void		MR_schedule(MR_Context *ctxt);

#ifndef MR_HIGHLEVEL_CODE
  MR_declare_entry(MR_do_runnext);
  #define MR_runnext()						\
	do {							\
		MR_GOTO(MR_ENTRY(MR_do_runnext));		\
	} while (0)
#endif

#ifdef	MR_THREAD_SAFE
  #define MR_IF_MR_THREAD_SAFE(x)	x
#else
  #define MR_IF_MR_THREAD_SAFE(x)
#endif

#ifndef MR_HIGHLEVEL_CODE
  /*
  ** fork_new_context(MR_Code *child, MR_Code *parent, int numslots):
  ** create a new context to execute the code at `child', and
  ** copy the topmost `numslots' from the current stackframe.
  ** The new context gets put on the runqueue, and the current
  ** context resumes at `parent'.
  */
  #define MR_fork_new_context(child, parent, numslots) do {		\
		MR_Context	*f_n_c_context;				\
		int		fork_new_context_i;			\
		f_n_c_context = MR_create_context();			\
		MR_IF_MR_THREAD_SAFE(					\
			f_n_c_context->owner_thread = NULL;		\
		)							\
		for (fork_new_context_i = (numslots) ;			\
				fork_new_context_i > 0 ;		\
				fork_new_context_i--) {			\
			*(f_n_c_context->context_sp) = 			\
				MR_stackvar(fork_new_context_i);	\
			f_n_c_context->MR_ctxt_sp++;			\
		}							\
		f_n_c_context->MR_ctxt_resume = (child);		\
		MR_schedule(f_n_c_context);				\
		MR_GOTO(parent);					\
	} while (0)
#endif /* MR_HIGHLEVEL_CODE */

#ifndef	MR_CONSERVATIVE_GC

  /*
  ** To figure out the maximum amount of heap we can reclaim on backtracking,
  ** we compare MR_hp with the MR_ctxt_hp.
  **
  ** If MR_ctxt_hp == NULL then this is the first time this context has been
  ** scheduled, so the furthest back down the heap we can reclaim is to the
  ** current value of MR_hp. 
  **
  ** If MR_hp > MR_ctxt_hp, another context has allocated data on the heap
  ** since we were last scheduled, so the furthest back that we can reclaim is
  ** to the current value of MR_hp, so we set MR_min_hp_rec and the
  ** field of the same name in our context structure.
  **
  ** If MR_hp < MR_ctxt_hp, then another context has truncated the heap on
  ** failure. For this to happen, it must be the case that last time we were
  ** that other context was the last one to allocate data on the heap, and we
  ** scheduled, did not allocate any heap during that period of execution.
  ** That being the case, the furthest back to which we can reset the heap is
  ** to the current value of hp. This is a conservative approximation - it is
  ** possible that the current value of hp is the same as some previous value
  ** that we held, and we are now contiguous with our older data, so this
  ** algorithm will lead to holes in the heap, though GC will reclaim these.
  **
  ** If hp == MR_ctxt_hp then no other process has allocated any heap since we
  ** were last scheduled, so we can proceed as if we had not stopped, and the
  ** furthest back that we can backtrack is the same as it was last time we
  ** were executing.
  */
  #define MR_set_min_heap_reclamation_point(ctxt)			\
	do {								\
		if (MR_hp != (ctxt)->MR_ctxt_hp 			\
			|| (ctxt)->MR_ctxt_hp == NULL)	{		\
			MR_min_hp_rec = MR_hp;				\
			(ctxt)->MR_ctxt_min_hp_rec = MR_hp;		\
		} else {						\
			MR_min_hp_rec =	(ctxt)->MR_ctxt_min_hp_rec;	\
			}						\
	} while (0)

  #define MR_save_hp_in_context(ctxt)				\
  	do {							\
		(ctxt)->MR_ctxt_hp = MR_hp;			\
		(ctxt)->MR_ctxt_min_hp_rec = MR_min_hp_rec;	\
	} while (0)

#else

  #define MR_set_min_heap_reclamation_point(ctxt)	do { } while (0)

  #define MR_save_hp_in_context(ctxt)			do { } while (0)

#endif

#ifdef MR_USE_TRAIL
  #define MR_IF_USE_TRAIL(x) x
#else
  #define MR_IF_USE_TRAIL(x)
#endif

#ifdef MR_USE_MINIMAL_MODEL
  #define MR_IF_USE_MINIMAL_MODEL(x) x
#else
  #define MR_IF_USE_MINIMAL_MODEL(x)
#endif

#ifndef MR_HIGHLEVEL_CODE
  #define MR_IF_NOT_HIGHLEVEL_CODE(x) x
#else
  #define MR_IF_NOT_HIGHLEVEL_CODE(x)
#endif

#define MR_load_context(cptr)						\
	do {								\
		MR_Context	*load_context_c;			\
									\
		load_context_c = (cptr);				\
		MR_IF_NOT_HIGHLEVEL_CODE(				\
			MR_succip  = load_context_c->MR_ctxt_succip;	\
			MR_sp	   = load_context_c->MR_ctxt_sp;	\
			MR_maxfr   = load_context_c->MR_ctxt_maxfr; 	\
			MR_curfr   = load_context_c->MR_ctxt_curfr;	\
		  MR_IF_USE_MINIMAL_MODEL(				\
			MR_gen_next = load_context_c->MR_ctxt_gen_next;	\
			MR_cut_next = load_context_c->MR_ctxt_cut_next;	\
		  )							\
		)							\
	        MR_IF_USE_TRAIL(					\
			MR_trail_zone = load_context_c->MR_ctxt_trail_zone;\
			MR_trail_ptr = load_context_c->MR_ctxt_trail_ptr;\
		    MR_ticket_counter =					\
				load_context_c->MR_ctxt_ticket_counter;	\
		    MR_ticket_high_water =				\
				 load_context_c->MR_ctxt_ticket_high_water;\
	    	)							\
		MR_IF_NOT_HIGHLEVEL_CODE(				\
		  MR_ENGINE(MR_eng_context).MR_ctxt_detstack_zone =	\
				load_context_c->MR_ctxt_detstack_zone;	\
		  MR_ENGINE(MR_eng_context).MR_ctxt_nondetstack_zone =	\
				load_context_c->MR_ctxt_nondetstack_zone;\
		  MR_IF_USE_MINIMAL_MODEL(				\
		    MR_ENGINE(MR_eng_context).MR_ctxt_genstack_zone =   \
				load_context_c->MR_ctxt_genstack_zone;	\
		    MR_ENGINE(MR_eng_context).MR_ctxt_cutstack_zone =   \
				load_context_c->MR_ctxt_cutstack_zone;	\
		    MR_gen_stack = (MR_GeneratorStackFrame *)		\
				MR_ENGINE(MR_eng_context).		\
					MR_ctxt_genstack_zone;		\
		    MR_cut_stack = (MR_CutStackFrame *)			\
				MR_ENGINE(MR_eng_context).		\
					MR_ctxt_cutstack_zone;		\
	    	  )							\
	    	)							\
		MR_set_min_heap_reclamation_point(load_context_c);	\
	} while (0)

#define MR_save_context(cptr)						\
	do {								\
		MR_Context	*save_context_c;			\
									\
		save_context_c = (cptr);				\
		MR_IF_NOT_HIGHLEVEL_CODE(				\
			save_context_c->MR_ctxt_succip	= MR_succip;	\
			save_context_c->MR_ctxt_sp	= MR_sp;	\
			save_context_c->MR_ctxt_maxfr   = MR_maxfr;	\
			save_context_c->MR_ctxt_curfr   = MR_curfr;	\
		  MR_IF_USE_MINIMAL_MODEL(				\
			save_context_c->MR_ctxt_gen_next = MR_gen_next;	\
			save_context_c->MR_ctxt_cut_next = MR_cut_next;	\
		  )							\
		)							\
		MR_IF_USE_TRAIL(					\
			save_context_c->MR_ctxt_trail_zone = MR_trail_zone;\
			save_context_c->MR_ctxt_trail_ptr = MR_trail_ptr;\
			save_context_c->MR_ctxt_ticket_counter =	\
						MR_ticket_counter;	\
			save_context_c->MR_ctxt_ticket_high_water =	\
						MR_ticket_high_water;	\
		)							\
		MR_IF_NOT_HIGHLEVEL_CODE(				\
		  save_context_c->MR_ctxt_detstack_zone =		\
				MR_ENGINE(MR_eng_context).		\
					MR_ctxt_detstack_zone;		\
		  save_context_c->MR_ctxt_nondetstack_zone =		\
				MR_ENGINE(MR_eng_context).		\
					MR_ctxt_nondetstack_zone;	\
		  MR_IF_USE_MINIMAL_MODEL(				\
		    save_context_c->MR_ctxt_genstack_zone =		\
				MR_ENGINE(MR_eng_context).		\
					MR_ctxt_genstack_zone;		\
		    save_context_c->MR_ctxt_cutstack_zone =		\
				MR_ENGINE(MR_eng_context).		\
					MR_ctxt_cutstack_zone;		\
		    assert(MR_gen_stack == (MR_GeneratorStackFrame *)	\
				MR_ENGINE(MR_eng_context).		\
					MR_ctxt_genstack_zone);		\
		    assert(MR_cut_stack == (MR_CutStackFrame *)		\
				MR_ENGINE(MR_eng_context).		\
					MR_ctxt_cutstack_zone);		\
		  )							\
		)							\
		MR_save_hp_in_context(save_context_c);			\
	} while (0)

typedef struct MR_Sync_Term_Struct MR_SyncTerm;
struct MR_Sync_Term_Struct {
  #ifdef MR_THREAD_SAFE
	MercuryLock	lock;
  #endif
	int		count;
	MR_Context	*parent;
};

#define MR_init_sync_term(sync_term, nbranches)				\
	do {								\
		SyncTerm *st = (SyncTerm *) sync_term;			\
		MR_IF_THREAD_SAFE(					\
			pthread_mutex_init(&(st->lock), MR_MUTEX_ATTR);	\
		)							\
		st->count = (nbranches);				\
		st->parent = NULL;					\
	} while (0)

#define MR_join_and_terminate(sync_term)				\
	do {								\
		SyncTerm *st = (SyncTerm *) sync_term;			\
		MR_LOCK(&(st->lock), "terminate");			\
		(st->count)--;						\
		if (st->count == 0) {					\
			assert(st->parent != NULL);			\
			MR_UNLOCK(&(st->lock), "terminate i");		\
			MR_schedule(st->parent);			\
		} else {						\
			assert(st->count > 0);				\
			MR_UNLOCK(&(st->lock), "terminate ii");		\
		}							\
		MR_destroy_context(MR_ENGINE(MR_eng_this_context));	\
		MR_runnext();						\
	} while (0)

#define MR_join_and_continue(sync_term, where_to)			\
	do {								\
		SyncTerm *st = (SyncTerm *) sync_term;			\
		MR_LOCK(&(st->lock), "continue");			\
		(st->count)--;						\
		if (st->count == 0) {					\
			MR_UNLOCK(&(st->lock), "continue i");		\
			MR_GOTO((where_to));				\
		}							\
		assert(st->count > 0);					\
		MR_save_context(MR_ENGINE(MR_eng_this_context));	\
		MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume = (where_to);\
		st->parent = MR_ENGINE(MR_eng_this_context);		\
		MR_UNLOCK(&(st->lock), "continue ii");			\
		MR_runnext();						\
	} while (0)

#endif /* not MERCURY_CONTEXT_H */
