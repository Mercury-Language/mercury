/*
INIT mercury_sys_init_scheduler_wrapper
ENDINIT
*/
/*
** Copyright (C) 1995-2002 The University of Melbourne.
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
  MercuryLock	MR_runqueue_lock;
  MercuryCond	MR_runqueue_cond;
#endif

MR_PendingContext	*MR_pending_contexts;
#ifdef	MR_THREAD_SAFE
  MercuryLock		MR_pending_contexts_lock;
#endif

/*
** free_context_list is a global linked list of unused context
** structures. If the MR_MemoryZone pointers are not NULL,
** then they point to allocated MR_MemoryZones, which will
** need to be reinitialized, but have space allocated to
** them. (see comments in mercury_memory.h about MR_reset_zone())
*/
static MR_Context *free_context_list = NULL;
#ifdef	MR_THREAD_SAFE
  static MercuryLock free_context_list_lock;
#endif

void
MR_init_thread_stuff(void)
{
#ifdef	MR_THREAD_SAFE

	pthread_mutex_init(&MR_runqueue_lock, MR_MUTEX_ATTR);

	pthread_cond_init(&MR_runqueue_cond, MR_COND_ATTR);

	pthread_mutex_init(&free_context_list_lock, MR_MUTEX_ATTR);

	pthread_mutex_init(&MR_global_lock, MR_MUTEX_ATTR);

	pthread_mutex_init(&MR_pending_contexts_lock, MR_MUTEX_ATTR);

	MR_KEY_CREATE(&MR_engine_base_key, NULL);

#endif
}

void
MR_finalize_runqueue(void)
{
#ifdef	MR_THREAD_SAFE
	pthread_mutex_destroy(&MR_runqueue_lock);
	pthread_cond_destroy(&MR_runqueue_cond);
	pthread_mutex_destroy(&free_context_list_lock);
#endif
}

void 
MR_init_context(MR_Context *c)
{
	c->MR_ctxt_next = NULL;
	c->MR_ctxt_resume = NULL;
#ifdef	MR_THREAD_SAFE
	c->MR_ctxt_owner_thread = (MercuryThread) NULL;
#endif

#ifndef MR_HIGHLEVEL_CODE
	c->MR_ctxt_succip = MR_ENTRY(MR_do_not_reached);

	if (c->MR_ctxt_detstack_zone != NULL) {
		MR_reset_redzone(c->MR_ctxt_detstack_zone);
	} else {
		c->MR_ctxt_detstack_zone = MR_create_zone("detstack", 0,
			MR_detstack_size, MR_next_offset(),
			MR_detstack_zone_size, MR_default_handler);
	}
	c->MR_ctxt_sp = c->MR_ctxt_detstack_zone->min;

	if (c->MR_ctxt_nondetstack_zone != NULL) {
		MR_reset_redzone(c->MR_ctxt_nondetstack_zone);
	} else {
		c->MR_ctxt_nondetstack_zone = MR_create_zone("nondetstack", 0,
			MR_nondstack_size, MR_next_offset(),
			MR_nondstack_zone_size, MR_default_handler);
	}
	/*
	** Note that maxfr and curfr point to the last word in the frame,
	** not to the first word, so we need to add the size of the frame,
	** minus one word, to the base address to get the maxfr/curfr pointer
	** for the first frame on the nondet stack.
	*/
	c->MR_ctxt_maxfr = c->MR_ctxt_nondetstack_zone->min +
		MR_NONDET_FIXED_SIZE - 1;
	c->MR_ctxt_curfr = c->MR_ctxt_maxfr;
	MR_redoip_slot(c->MR_ctxt_curfr) = MR_ENTRY(MR_do_not_reached);
	MR_redofr_slot(c->MR_ctxt_curfr) = NULL;
	MR_prevfr_slot(c->MR_ctxt_curfr) = NULL;
	MR_succip_slot(c->MR_ctxt_curfr) = MR_ENTRY(MR_do_not_reached);
	MR_succfr_slot(c->MR_ctxt_curfr) = NULL;

  #ifdef MR_USE_MINIMAL_MODEL
	if (c->MR_ctxt_generatorstack_zone != NULL) {
		MR_reset_redzone(c->MR_ctxt_generatorstack_zone);
	} else {
		c->MR_ctxt_generatorstack_zone = MR_create_zone(
			"generatorstack", 0,
			MR_generatorstack_size, MR_next_offset(),
			MR_generatorstack_zone_size, MR_default_handler);
	}
	c->MR_ctxt_gen_next = 0;

	if (c->MR_ctxt_cutstack_zone != NULL) {
		MR_reset_redzone(c->MR_ctxt_cutstack_zone);
	} else {
		c->MR_ctxt_cutstack_zone = MR_create_zone("cutstack", 0,
			MR_cutstack_size, MR_next_offset(),
			MR_cutstack_zone_size, MR_default_handler);
	}
	c->MR_ctxt_cut_next = 0;
  #endif /* MR_USE_MINIMAL_MODEL */
#endif /* !MR_HIGHLEVEL_CODE */

#ifdef MR_USE_TRAIL
	if (c->MR_ctxt_trail_zone != NULL) {
		MR_reset_redzone(c->MR_ctxt_trail_zone);
	} else {
		c->MR_ctxt_trail_zone = MR_create_zone("trail", 0,
			MR_trail_size, MR_next_offset(),
			MR_trail_zone_size, MR_default_handler);
	}
	c->MR_ctxt_trail_ptr = (MR_TrailEntry *) c->MR_ctxt_trail_zone->min;
	c->MR_ctxt_ticket_counter = 1;
	c->MR_ctxt_ticket_high_water = 1;
#endif

	c->MR_ctxt_hp = NULL;
}

MR_Context *
MR_create_context(void)
{
	MR_Context *c;

	MR_LOCK(&free_context_list_lock, "create_context");
	if (free_context_list == NULL) {
		MR_UNLOCK(&free_context_list_lock, "create_context i");
		c = MR_GC_NEW(MR_Context);
#ifndef MR_HIGHLEVEL_CODE
		c->MR_ctxt_detstack_zone = NULL;
		c->MR_ctxt_nondetstack_zone = NULL;
#endif
#ifdef MR_USE_TRAIL
		c->MR_ctxt_trail_zone = NULL;
#endif
	} else {
		c = free_context_list;
		free_context_list = c->MR_ctxt_next;
		MR_UNLOCK(&free_context_list_lock, "create_context ii");
	}

	MR_init_context(c);

	return c;
}

void 
MR_destroy_context(MR_Context *c)
{
	MR_LOCK(&free_context_list_lock, "destroy_context");
	c->MR_ctxt_next = free_context_list;
	free_context_list = c;
	MR_UNLOCK(&free_context_list_lock, "destroy_context");
}

void 
MR_flounder(void)
{
	MR_fatal_error("computation floundered");
}

/*
** Check to see if any contexts that blocked on IO have become
** runnable. Return the number of contexts that are still blocked.
** The parameter specifies whether or not the call to select should
** block or not.
*/
static int
MR_check_pending_contexts(MR_bool block)
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
			if (max_id > pctxt->fd) {
				max_id = pctxt->fd;
			}
			FD_SET(pctxt->fd, &rd_set);
		}
		if (pctxt->waiting_mode & MR_PENDING_WRITE) {
			if (max_id > pctxt->fd) {
				max_id = pctxt->fd;
			}
			FD_SET(pctxt->fd, &wr_set);
		}
		if (pctxt->waiting_mode & MR_PENDING_EXEC) {
			if (max_id > pctxt->fd) {
				max_id = pctxt->fd;
			}
			FD_SET(pctxt->fd, &ex_set);
		}
	}
	max_id++;

	if (max_id == 0) {
		MR_fatal_error("no fd's set!");
	}

	if (block) {
		err = select(max_id, &rd_set, &wr_set, &ex_set, NULL);
	} else {
		timeout.tv_sec = 0;
		timeout.tv_usec = 0;
		err = select(max_id, &rd_set, &wr_set, &ex_set, &timeout);
	}

	if (err < 0) {
		MR_fatal_error("select failed!");
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
			MR_schedule(pctxt->context);
		}
	}

	return n_ids;

#else	/* !MR_CAN_DO_PENDING_IO */

	MR_fatal_error("select() unavailable!");

#endif
}

void
MR_schedule(MR_Context *ctxt)
{
	ctxt->MR_ctxt_next = NULL;
	MR_LOCK(&MR_runqueue_lock, "schedule");
	if (MR_runqueue_tail) {
		MR_runqueue_tail->MR_ctxt_next = ctxt;
		MR_runqueue_tail = ctxt;
	} else {
		MR_runqueue_head = ctxt;
		MR_runqueue_tail = ctxt;
	}
	MR_SIGNAL(&MR_runqueue_cond);
	MR_UNLOCK(&MR_runqueue_lock, "schedule");
}

#ifndef MR_HIGHLEVEL_CODE

MR_define_extern_entry(MR_do_runnext);

MR_BEGIN_MODULE(scheduler_module)
	MR_init_entry_an(MR_do_runnext);
MR_BEGIN_CODE

MR_define_entry(MR_do_runnext);
#ifdef MR_THREAD_SAFE
{
	MR_Context *tmp, *prev;
	unsigned depth;
	MercuryThread thd;

	depth = MR_ENGINE(MR_eng_c_depth);
	thd = MR_ENGINE(MR_eng_owner_thread);

	MR_LOCK(&MR_runqueue_lock, "MR_do_runnext (i)");

	while (1) {
		if (MR_exit_now == MR_TRUE) {
			MR_UNLOCK(&MR_runqueue_lock, "MR_do_runnext (ii)");
			MR_destroy_thread(MR_cur_engine());
		}
		tmp = MR_runqueue_head;
		/* XXX check pending io */
		prev = NULL;
		while(tmp != NULL) {
			if ((depth > 0 && tmp->MR_ctxt_owner_thread == thd) ||
				(tmp->MR_ctxt_owner_thread ==
					(MercuryThread) NULL))
			{
				break;
			}
			prev = tmp;
			tmp = tmp->MR_ctxt_next;
		}
		if (tmp != NULL) {
			break;
		}
		MR_WAIT(&MR_runqueue_cond, &MR_runqueue_lock);
	}
	MR_ENGINE(MR_eng_this_context) = tmp;
	if (prev != NULL) {
		prev->MR_ctxt_next = tmp->MR_ctxt_next;
	} else {
		MR_runqueue_head = tmp->MR_ctxt_next;
	}
	if (MR_runqueue_tail == tmp) {
		MR_runqueue_tail = prev;
	}
	MR_UNLOCK(&MR_runqueue_lock, "MR_do_runnext (iii)");
	MR_load_context(MR_ENGINE(MR_eng_this_context));
	MR_GOTO(MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume);
}
#else /* !MR_THREAD_SAFE */
{
	if (MR_runqueue_head == NULL && MR_pending_contexts == NULL) {
		MR_fatal_error("empty runqueue!");
	}

	while (MR_runqueue_head == NULL) {
		MR_check_pending_contexts(MR_TRUE); /* block */
	}

	MR_ENGINE(MR_eng_this_context) = MR_runqueue_head;
	MR_runqueue_head = MR_runqueue_head->MR_ctxt_next;
	if (MR_runqueue_head == NULL) {
		MR_runqueue_tail = NULL;
	}

	MR_load_context(MR_ENGINE(MR_eng_this_context));
	MR_GOTO(MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume);
}
#endif

MR_END_MODULE

#endif /* !MR_HIGHLEVEL_CODE */

/* forward decls to suppress gcc warnings */
void mercury_sys_init_scheduler_wrapper_init(void);
void mercury_sys_init_scheduler_wrapper_init_type_tables(void);
#ifdef	MR_DEEP_PROFILING
void mercury_sys_init_scheduler_wrapper_write_out_proc_statics(FILE *fp);
#endif

void mercury_sys_init_scheduler_wrapper_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
	scheduler_module();
#endif
}

void mercury_sys_init_scheduler_wrapper_init_type_tables(void)
{
	/* no types to register */
}

#ifdef	MR_DEEP_PROFILING
void mercury_sys_init_scheduler_wrapper_write_out_proc_statics(FILE *fp)
{
	/* no proc_statics to write out */
}
#endif
