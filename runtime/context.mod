/*
** Copyright (C) 1995-1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* context.mod - handles multithreading stuff. */

#include "imp.h"

#include <unistd.h>		/* for getpid() and fork() */
#ifdef PARALLEL
#include <signal.h>
#endif

#include "context.h"

#ifdef	PARALLEL
unsigned numprocs = 1;
#endif

#ifdef	PARALLEL
pid_t	*procid;
AtomicBool *procwaiting;
#endif
int	my_procnum;
pid_t	my_procid;
Word	*min_heap_reclamation_point;

Context	*this_context;
Context	**runqueue_ptr;
Context	**free_context_list_ptr;
SpinLock *runqueue_lock;
SpinLock *free_context_list_lock;

Context	*do_schedule_cptr;
Code	*do_schedule_resume;
Word     *do_join_and_terminate_sync_term;
Word     *do_join_and_continue_sync_term;
Code     *do_join_and_continue_where_to;

static void init_free_context_list(void);

void 
init_processes(void)
{
	int i;
	pid_t pid;

	my_procnum = 0;
	my_procid = getpid();

	runqueue_lock = allocate_lock();
	runqueue_ptr = allocate_object(Context *);
	*runqueue_ptr = NULL;

#ifdef	PARALLEL
	procid = allocate_array(pid_t, numprocs);
	procwaiting = allocate_array(AtomicBool, numprocs);
	procid[0] = my_procid;
	procwaiting[0] = FALSE;

	for (i = 1; i < numprocs; i++) {
		if ((pid = fork()) < 0) {
			fatal_error("failed to fork()");
		}
		if (pid == 0) { /* child */
			my_procnum = i;
			procid[i] = my_procid = getpid();
			procwaiting[i] = FALSE;
			return;
		}
	}
#endif

}

void 
init_process_context(void)
{
	init_heap();

	if (my_procnum == 0) { /* the original process */
		init_free_context_list();
		this_context = new_context();
			/* load the registers so we don't clobber hp */
		restore_transient_registers();
		load_context(this_context);
		save_transient_registers();
	}
}

static void 
init_free_context_list(void)
{
	int i;
	Context *tmp;

	free_context_list_lock = allocate_lock();
	free_context_list_ptr = allocate_object(Context *);
	*free_context_list_ptr = allocate_array(Context, INITIAL_NUM_CONTEXTS);
	tmp = *free_context_list_ptr;
	for (i = 0; i < INITIAL_NUM_CONTEXTS; i++) {
		if (i != INITIAL_NUM_CONTEXTS - 1) {
			tmp[i].next = &(tmp[i+1]);
		} else {
			tmp[i].next = NULL;
		}
		tmp[i].resume = NULL;
		tmp[i].context_succip = NULL;
		tmp[i].detstack_zone = NULL;
		tmp[i].context_sp = NULL;
		tmp[i].nondetstack_zone = NULL;
		tmp[i].context_curfr = NULL;
		tmp[i].context_maxfr = NULL;
	}
}

Context *
new_context(void)
{
	Context *c;

	get_lock(free_context_list_lock);

	MR_assert(free_context_list_ptr != NULL);
	if (*free_context_list_ptr == NULL) {
		fatal_error("no free contexts");
	} else {
		c = *free_context_list_ptr;
		*free_context_list_ptr = c->next;
	}
	release_lock(free_context_list_lock);

	c->next = NULL;
	c->resume = NULL;
	c->context_succip = ENTRY(do_not_reached);

	if (c->detstack_zone != NULL) {
		reset_zone(c->detstack_zone);
	} else {
		c->detstack_zone = create_zone("detstack", 0,
			detstack_size, next_offset(), detstack_zone_size, 
			default_handler);
	}
	c->context_sp = c->detstack_zone->min;

	if (c->nondetstack_zone != NULL) {
		reset_zone(c->nondetstack_zone);
	} else {
		c->nondetstack_zone = create_zone("nondetstack", 0,
			nondstack_size, next_offset(), nondstack_zone_size,
			default_handler);
	}
	c->context_maxfr = c->nondetstack_zone->min;
	c->context_curfr = c->nondetstack_zone->min;
	bt_redoip(c->context_curfr) = ENTRY(do_not_reached);
	bt_prevfr(c->context_curfr) = NULL;
	bt_succip(c->context_curfr) = ENTRY(do_not_reached);
	bt_succfr(c->context_curfr) = NULL;

	c->context_hp = NULL;

	return c;
}

void 
delete_context(Context *c)
{
	get_lock(free_context_list_lock);
	MR_assert(free_context_list_ptr != NULL);
	c->next = *free_context_list_ptr;
	*free_context_list_ptr = c;
	release_lock(free_context_list_lock);
}

void 
flounder(void)
{
	fatal_error("computation floundered.");
}

BEGIN_MODULE(context_module)

BEGIN_CODE

	/*
	** do a context switch: the previous context is assumed to have
	** been saved or deallocated or whatever.
	*/
do_runnext:
	while(1) {
#ifdef	PARALLEL
		/* If we're running in parallel, then we need to
		** do some signal magic in order to avoid a race-
		** condition if we have to suspend, waiting for
		** the runqueue to become non-empty.
		** The following algorithm is adapted from
		** "Advanced Programming in the UNIX Environment",
		** Stevens:
		**    - use sigprocmask to block SIGUSR1
		**    - obtain the spinlock on the runqueue
		**    - if the runqueue is not empty, get the
		**      next context off the queue, release the
		**	lock and reset the signal mask.
		**    - if the runqueue is empty, mark this process
		**	as waiting, release the lock and then
		**	use sigsuspend to atomically renable SIGUSR1
		**	and suspend the process. When we get a
		**	SIGUSR1 we resume and mark the process as not
		**	waiting, then try again to get a context off
		**	the runqueue.
		**    - this relies on the schedule code to send the
		**      SIGUSR1 signal while it has the spinlock to
		**	ensure that this process will only get sent
		**	a single signal.
		*/
		sigset_t newset, oldset, emptyset;
		sigemptyset(&newset);
		sigemptyset(&emptyset);
		sigaddset(&newset, SIGUSR1);
		/* block SIGUSR1 while we're in the critical region */
		sigprocmask(SIG_BLOCK, &newset, &oldset);
#endif
		get_lock(runqueue_lock);
		if (*runqueue_ptr != NULL) {
			this_context = *runqueue_ptr;
			*runqueue_ptr = (*runqueue_ptr)->next;
			release_lock(runqueue_lock);
#ifdef	PARALLEL
			/* restore the original set of signals */
			sigprocmask(SIG_SETMASK, &oldset, NULL);
#endif
			load_context(this_context);
			GOTO(this_context->resume);
		}
		else
		{
#ifdef	PARALLEL
			int i;
			bool is_runnable;

			procwaiting[my_procnum] = TRUE;

			/*
			** check to see that at least one process
			** is currently runnable. If none are, then
			** we've just floundered.
			*/
			is_runnable=FALSE;
			for(i = 0; i < numprocs; i++)
			{
				if (procwaiting[i] == FALSE)
				{
					is_runnable = TRUE;
					break;
				}
			}
			if (!is_runnable)
				flounder();
			
#endif
			release_lock(runqueue_lock);
#ifdef	PARALLEL
			sigsuspend(&emptyset);
			procwaiting[my_procnum] = FALSE;
#else
				/* if we're not using parallelism, then
				** the runqueue should never be empty.
				*/
			flounder();
#endif
		}
	}

	/*
	** do_schedule adds the context pointed to by do_schedule_cptr
	** to the runqueue, signalling a sleeping process to wake it if
	** the runqueue was previously empty.
	*/
do_schedule:
{
	Context *old;

	get_lock(runqueue_lock);
	old = *runqueue_ptr;
	do_schedule_cptr->next = *runqueue_ptr;
	*runqueue_ptr = do_schedule_cptr;
#ifdef	PARALLEL
	/* Check to see if we need to signal a sleeping process */
	if (old == NULL) {
		int i;
		for(i = 0; i < numprocs; i++) {
			if (procwaiting[i] == TRUE) {
				kill(procid[i], SIGUSR1);
				break;
			}
		}
	}
#endif
	release_lock(runqueue_lock);
	GOTO(do_schedule_resume);
}

	/*
	** do_join_and_terminate synchronises with the structure pointed to
	** by do_join_and_terminate_sync_term, then terminates the current
	** context and does a context switch. If the current context was the
	** last context to arrive at the synchronisation point, then we
	** resume the parent context rather than do a context switch.
	*/
do_join_and_terminate:
{
	register Word *sync_term;
	Context *ctxt;

	sync_term = do_join_and_terminate_sync_term;

	get_lock((SpinLock *)&sync_term[SYNC_TERM_LOCK]);
	if (--(sync_term[SYNC_TERM_COUNTER]) == 0) {
		MR_assert(sync_term[SYNC_TERM_PARENT] != NULL);
		release_lock((SpinLock *)&sync_term[SYNC_TERM_LOCK]);
		ctxt = (Context *) sync_term[SYNC_TERM_PARENT];
		delete_context(this_context);
		this_context = ctxt;
		load_context(this_context);
		GOTO(this_context->resume);
	} else {
		release_lock((SpinLock *)&sync_term[SYNC_TERM_LOCK]);
		delete_context(this_context);
		runnext();
	}
}

	/*
	** do_join_and_continue synchronises with the structure pointed to
	** by do_join_and_continue_sync_term. If we are the last context to
	** arrive here, then we branch to the continuation stored in
	** do_join_and_continue_where_to. If we have to wait for other contexts
	** to arrive, then we save the current context and store a pointer
	** to it in the synchronisation term before doing a context switch.
	*/
do_join_and_continue:
{
	register Word *sync_term;

	sync_term = do_join_and_continue_sync_term;

	get_lock((SpinLock *)&sync_term[SYNC_TERM_LOCK]);
	if (--(sync_term[SYNC_TERM_COUNTER]) == 0) {
		MR_assert(sync_term[SYNC_TERM_PARENT] == NULL);
		release_lock((SpinLock *)&sync_term[SYNC_TERM_LOCK]);
		GOTO(do_join_and_continue_where_to);
	} else {
		save_context(this_context);
		this_context->resume = do_join_and_continue_where_to;
		sync_term[SYNC_TERM_PARENT] = (Word) this_context;
		release_lock((SpinLock *)&sync_term[SYNC_TERM_LOCK]);
		runnext();
	}
}

END_MODULE
