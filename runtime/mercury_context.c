/*
INIT mercury_sys_init_context
ENDINIT
*/
/*
** Copyright (C) 1995-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_context.c - handles multithreading stuff. */

#include "mercury_imp.h"

#include <stdio.h>
#include <unistd.h>		/* for getpid() and fork() */
#ifdef MR_THREAD_SAFE
  #include "mercury_thread.h"
#endif

#include "mercury_memory_handlers.h"
#include "mercury_context.h"
#include "mercury_engine.h"	/* for `memdebug' */

MR_Context	*MR_runqueue;
#ifdef	MR_THREAD_SAFE
  MercuryLock	*MR_runqueue_lock;
  MercuryCond	*MR_runqueue_cond;
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
	bt_redoip(c->context_curfr) = ENTRY(do_not_reached);
	bt_prevfr(c->context_curfr) = NULL;
	bt_succip(c->context_curfr) = ENTRY(do_not_reached);
	bt_succfr(c->context_curfr) = NULL;

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

void mercury_sys_init_context(void); /* suppress gcc warning */
void mercury_sys_init_context(void) {
}
