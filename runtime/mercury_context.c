/*
INIT mercury_scheduler_wrapper
ENDINIT
*/
/*
** Copyright (C) 1995-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_context.c - handles multithreading stuff. */

#include "mercury_imp.h"

#include <stdio.h>
#ifdef MR_THREAD_SAFE
  #include "mercury_thread.h"
#endif
#ifdef MR_CAN_DO_PENDING_IO
  #include <sys/types.h>	/* for fd_set */
  #include <sys/time.h>		/* for struct timeval */
#endif

#include "mercury_memory_handlers.h"
#include "mercury_context.h"
#include "mercury_engine.h"		/* for `MR_memdebug' */
#include "mercury_reg_workarounds.h"	/* for `MR_fd*' stuff */

MR_Context	*MR_runqueue_head;
MR_Context	*MR_runqueue_tail;
#ifdef	MR_THREAD_SAFE
  MercuryLock	*MR_runqueue_lock;
  MercuryCond	*MR_runqueue_cond;
#endif

MR_PendingContext	*MR_pending_contexts;
#ifdef	MR_THREAD_SAFE
  MercuryLock		*MR_pending_contexts_lock;
#endif

/*
** free_context_list is a global linked list of unused context
** structures. If the MemoryZone pointers are not NULL,
** then they point to allocated MemoryZones, which will
** need to be reinitialized, but have space allocated to
** them. (see comments in mercury_memory.h about reset_zone())
*/
static MR_Context *free_context_list = NULL;
#ifdef	MR_THREAD_SAFE
  static MercuryLock *free_context_list_lock;
#endif

void
init_thread_stuff(void)
{
#ifdef	MR_THREAD_SAFE

	MR_runqueue_lock = make(MercuryLock);
	pthread_mutex_init(MR_runqueue_lock, MR_MUTEX_ATTR);

	MR_runqueue_cond = make(MercuryCond);
	pthread_cond_init(MR_runqueue_cond, MR_COND_ATTR);

	free_context_list_lock = make(MercuryLock);
	pthread_mutex_init(free_context_list_lock, MR_MUTEX_ATTR);

	pthread_mutex_init(&MR_global_lock, MR_MUTEX_ATTR);

	MR_pending_contexts_lock = make(MercuryLock);
	pthread_mutex_init(MR_pending_contexts_lock, MR_MUTEX_ATTR);

	MR_KEY_CREATE(&MR_engine_base_key, NULL);

#endif
}

void
finalize_runqueue(void)
{
#ifdef	MR_THREAD_SAFE
	pthread_mutex_destroy(MR_runqueue_lock);
	pthread_cond_destroy(MR_runqueue_cond);
	pthread_mutex_destroy(free_context_list_lock);
#endif
}

void 
init_context(MR_Context *c)
{
	c->next = NULL;
	c->resume = NULL;
#ifdef	MR_THREAD_SAFE
	c->owner_thread = (MercuryThread) NULL;
#endif
	c->context_succip = ENTRY(do_not_reached);

	if (c->detstack_zone != NULL) {
		reset_redzone(c->detstack_zone);
	} else {
		c->detstack_zone = create_zone("detstack", 0,
			detstack_size, next_offset(), detstack_zone_size, 
			default_handler);
	}
	c->context_sp = c->detstack_zone->min;

	if (c->nondetstack_zone != NULL) {
		reset_redzone(c->nondetstack_zone);
	} else {
		c->nondetstack_zone = create_zone("nondetstack", 0,
			nondstack_size, next_offset(), nondstack_zone_size,
			default_handler);
	}
	c->context_maxfr = c->nondetstack_zone->min;
	c->context_curfr = c->nondetstack_zone->min;
	MR_redoip_slot(c->context_curfr) = ENTRY(do_not_reached);
	MR_prevfr_slot(c->context_curfr) = NULL;
	MR_succip_slot(c->context_curfr) = ENTRY(do_not_reached);
	MR_succfr_slot(c->context_curfr) = NULL;

#ifdef	MR_USE_MINIMAL_MODEL
	if (c->generatorstack_zone != NULL) {
		reset_redzone(c->generatorstack_zone);
	} else {
		c->generatorstack_zone = create_zone("generatorstack", 0,
			generatorstack_size, next_offset(),
			generatorstack_zone_size, default_handler);
	}
	c->context_gen_next = 0;

	if (c->cutstack_zone != NULL) {
		reset_redzone(c->cutstack_zone);
	} else {
		c->cutstack_zone = create_zone("cutstack", 0,
			cutstack_size, next_offset(),
			cutstack_zone_size, default_handler);
	}
	c->context_cut_next = 0;
#endif

#ifdef MR_USE_TRAIL
	if (c->trail_zone != NULL) {
		reset_redzone(c->trail_zone);
	} else {
		c->trail_zone = create_zone("trail", 0,
			trail_size, next_offset(), trail_zone_size, 
			default_handler);
	}
	c->context_trail_ptr = (MR_TrailEntry *) c->trail_zone->min;
	c->context_ticket_counter = 0;
#endif

	c->context_hp = NULL;
}

MR_Context *
create_context(void)
{
	MR_Context *c;

	MR_LOCK(free_context_list_lock, "create_context");
	if (free_context_list == NULL) {
		MR_UNLOCK(free_context_list_lock, "create_context i");
		c = (MR_Context *) make(MR_Context);
		c->detstack_zone = NULL;
		c->nondetstack_zone = NULL;
#ifdef MR_USE_TRAIL
		c->trail_zone = NULL;
#endif
	} else {
		c = free_context_list;
		free_context_list = c->next;
		MR_UNLOCK(free_context_list_lock, "create_context ii");
	}

	init_context(c);

	return c;
}

void 
destroy_context(MR_Context *c)
{
	MR_LOCK(free_context_list_lock, "destroy_context");
	c->next = free_context_list;
	free_context_list = c;
	MR_UNLOCK(free_context_list_lock, "destroy_context");
}

void 
flounder(void)
{
	fatal_error("computation floundered");
}

/*
** Check to see if any contexts that blocked on IO have become
** runnable. Return the number of contexts that are still blocked.
** The parameter specifies whether or not the call to select should
** block or not.
*/
static int
check_pending_contexts(Bool block)
{
#ifdef	MR_CAN_DO_PENDING_IO

	int	err, max_id, n_ids;
	fd_set	rd_set, wr_set, ex_set;
	struct timeval timeout;
	MR_PendingContext *pctxt;

	if (MR_pending_contexts == NULL) {
		return 0;
	}

	MR_fd_zero(&rd_set); MR_fd_zero(&wr_set); MR_fd_zero(&ex_set);
	max_id = -1;
	for (pctxt = MR_pending_contexts ; pctxt ; pctxt = pctxt -> next) {
		if (pctxt->waiting_mode & MR_PENDING_READ) {
			if (max_id > pctxt->fd) max_id = pctxt->fd;
			FD_SET(pctxt->fd, &rd_set);
		}
		if (pctxt->waiting_mode & MR_PENDING_WRITE) {
			if (max_id > pctxt->fd) max_id = pctxt->fd;
			FD_SET(pctxt->fd, &wr_set);
		}
		if (pctxt->waiting_mode & MR_PENDING_EXEC) {
			if (max_id > pctxt->fd) max_id = pctxt->fd;
			FD_SET(pctxt->fd, &ex_set);
		}
	}
	max_id++;

	if (max_id == 0) {
		fatal_error("no fd's set!");
	}

	if (block) {
		err = select(max_id, &rd_set, &wr_set, &ex_set, NULL);
	} else {
		timeout.tv_sec = 0;
		timeout.tv_usec = 0;
		err = select(max_id, &rd_set, &wr_set, &ex_set, &timeout);
	}

	if (err < 0) {
		fatal_error("select failed!");
	}

	n_ids = 0;
	for (pctxt = MR_pending_contexts; pctxt; pctxt = pctxt -> next) {
		n_ids++;
		if (	((pctxt->waiting_mode & MR_PENDING_READ) 
				&& FD_ISSET(pctxt->fd, &rd_set))
		    ||	((pctxt->waiting_mode & MR_PENDING_WRITE)
				&& FD_ISSET(pctxt->fd, &wr_set))
		    ||	((pctxt->waiting_mode & MR_PENDING_EXEC)
				&& FD_ISSET(pctxt->fd, &ex_set))
		    )
		{
			schedule(pctxt->context);
		}
	}

	return n_ids;

#else	/* !MR_CAN_DO_PENDING_IO */

	fatal_error("select() unavailable!");

#endif
}


void
schedule(MR_Context *ctxt)
{
	ctxt->next = NULL;
	MR_LOCK(MR_runqueue_lock, "schedule");
	if (MR_runqueue_tail) {
		MR_runqueue_tail->next = ctxt;
		MR_runqueue_tail = ctxt;
	} else {
		MR_runqueue_head = ctxt;
		MR_runqueue_tail = ctxt;
	}
	MR_SIGNAL(MR_runqueue_cond);
	MR_UNLOCK(MR_runqueue_lock, "schedule");
}

Define_extern_entry(do_runnext);

BEGIN_MODULE(scheduler_module)
	init_entry_ai(do_runnext);
BEGIN_CODE

Define_entry(do_runnext);
#ifdef MR_THREAD_SAFE
{
	MR_Context *tmp, *prev;
	unsigned depth;
	MercuryThread thd;

	depth = MR_ENGINE(c_depth);
	thd = MR_ENGINE(owner_thread);

	MR_LOCK(MR_runqueue_lock, "do_runnext (i)");

	while (1) {
		if (MR_exit_now == TRUE) {
			MR_UNLOCK(MR_runqueue_lock, "do_runnext (ii)");
			destroy_thread(MR_engine_base);
		}
		tmp = MR_runqueue_head;
		/* XXX check pending io */
		prev = NULL;
		while(tmp != NULL) {
			if ((depth > 0 && tmp->owner_thread == thd)
			    || (tmp->owner_thread == (MercuryThread) NULL)) {
				break;
			}
			prev = tmp;
			tmp = tmp->next;
		}
		if (tmp != NULL) {
			break;
		}
		MR_WAIT(MR_runqueue_cond, MR_runqueue_lock);
	}
	MR_ENGINE(this_context) = tmp;
	if (prev != NULL) {
		prev->next = tmp->next;
	} else {
		MR_runqueue_head = tmp->next;
	}
	if (MR_runqueue_tail == tmp) {
		MR_runqueue_tail = prev;
	}
	MR_UNLOCK(MR_runqueue_lock, "do_runnext (iii)");
	load_context(MR_ENGINE(this_context));
	GOTO(MR_ENGINE(this_context)->resume);
}
#else /* !MR_THREAD_SAFE */
{
	if (MR_runqueue_head == NULL && MR_pending_contexts == NULL) {
		fatal_error("empty runqueue!");
	}

	while (MR_runqueue_head == NULL) {
		check_pending_contexts(TRUE); /* block */
	}

	MR_ENGINE(this_context) = MR_runqueue_head;
	MR_runqueue_head = MR_runqueue_head->next;
	if (MR_runqueue_head == NULL) {
		MR_runqueue_tail = NULL;
	}

	load_context(MR_ENGINE(this_context));
	GOTO(MR_ENGINE(this_context)->resume);
}
#endif

END_MODULE

void mercury_scheduler_wrapper(void); /* suppress gcc warning */
void mercury_scheduler_wrapper(void) {
	scheduler_module();
}
