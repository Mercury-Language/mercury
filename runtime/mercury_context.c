/*
** vim: ts=4 sw=4 expandtab
*/
/*
INIT mercury_sys_init_scheduler_wrapper
ENDINIT
*/
/*
** Copyright (C) 1995-2006 The University of Melbourne.
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
  #ifdef MR_HAVE_UNISTD_H
	#include <unistd.h>	/* for select() on OS X */
  #endif	
#endif

#include "mercury_memory_handlers.h"
#include "mercury_context.h"
#include "mercury_engine.h"             /* for `MR_memdebug' */
#include "mercury_reg_workarounds.h"    /* for `MR_fd*' stuff */

static  void            MR_init_context_maybe_generator(MR_Context *c,
                            const char *id, MR_Generator *gen);

/*---------------------------------------------------------------------------*/

/*
** The run queue and spark queue are protected and signalled with the
** same lock and condition variable.
*/
MR_Context              *MR_runqueue_head;
MR_Context              *MR_runqueue_tail;
#ifndef MR_HIGHLEVEL_CODE
  MR_Spark              *MR_spark_queue_head;
  MR_Spark              *MR_spark_queue_tail;
#endif
#ifdef  MR_THREAD_SAFE
  MercuryLock           MR_runqueue_lock;
  MercuryCond           MR_runqueue_cond;
#endif

MR_PendingContext       *MR_pending_contexts;
#ifdef  MR_THREAD_SAFE
  MercuryLock           MR_pending_contexts_lock;
#endif

/*
** free_context_list and free_small_context_list are a global linked lists
** of unused context structures, with regular and small stacks respectively.
** If the MR_MemoryZone pointers are not NULL, then they point to allocated
** MR_MemoryZones.
*/
static MR_Context       *free_context_list = NULL;
static MR_Context       *free_small_context_list = NULL;
#ifdef  MR_THREAD_SAFE
  static MercuryLock    free_context_list_lock;
#endif

int MR_num_idle_engines = 0;
int MR_num_outstanding_contexts_and_sparks = 0;

/*---------------------------------------------------------------------------*/

void
MR_init_thread_stuff(void)
{
#ifdef  MR_THREAD_SAFE

    pthread_mutex_init(&MR_runqueue_lock, MR_MUTEX_ATTR);
    pthread_cond_init(&MR_runqueue_cond, MR_COND_ATTR);
    pthread_mutex_init(&free_context_list_lock, MR_MUTEX_ATTR);
    pthread_mutex_init(&MR_global_lock, MR_MUTEX_ATTR);
    pthread_mutex_init(&MR_pending_contexts_lock, MR_MUTEX_ATTR);
  #ifndef MR_THREAD_LOCAL_STORAGE
    MR_KEY_CREATE(&MR_engine_base_key, NULL);
  #endif
    MR_KEY_CREATE(&MR_exception_handler_key, NULL);

#endif
}

void
MR_finalize_runqueue(void)
{
#ifdef  MR_THREAD_SAFE
    pthread_mutex_destroy(&MR_runqueue_lock);
    pthread_cond_destroy(&MR_runqueue_cond);
    pthread_mutex_destroy(&free_context_list_lock);
#endif
}

static void 
MR_init_context_maybe_generator(MR_Context *c, const char *id,
    MR_Generator *gen)
{
    const char  *detstack_name;
    const char  *nondstack_name;
    size_t      detstack_size;
    size_t      nondstack_size;

    c->MR_ctxt_id = id;
    c->MR_ctxt_next = NULL;
    c->MR_ctxt_resume = NULL;
#ifdef  MR_THREAD_SAFE
    c->MR_ctxt_owner_thread = (MercuryThread) NULL;
#endif

#ifndef MR_HIGHLEVEL_CODE
    c->MR_ctxt_succip = MR_ENTRY(MR_do_not_reached);

    switch (c->MR_ctxt_size) {
        case MR_CONTEXT_SIZE_REGULAR:
            detstack_name  = "detstack";
            nondstack_name = "nondetstack";
            detstack_size  = MR_detstack_size;
            nondstack_size = MR_nondstack_size;
            break;
        case MR_CONTEXT_SIZE_SMALL:
            detstack_name  = "small_detstack";
            nondstack_name = "small_nondetstack";
            detstack_size  = MR_small_detstack_size;
            nondstack_size = MR_small_nondstack_size;
            break;
    }

    if (c->MR_ctxt_detstack_zone == NULL) {
        if (gen != NULL) {
            c->MR_ctxt_detstack_zone = MR_create_zone("gen_detstack",
                    0, MR_gen_detstack_size, MR_next_offset(),
                    MR_gen_detstack_zone_size, MR_default_handler);
        } else {
            c->MR_ctxt_detstack_zone = MR_create_zone(detstack_name,
                    0, detstack_size, MR_next_offset(),
                    MR_detstack_zone_size, MR_default_handler);
        }
    }
    c->MR_ctxt_sp = c->MR_ctxt_detstack_zone->MR_zone_min;

    if (c->MR_ctxt_nondetstack_zone == NULL) {
        if (gen != NULL) {
            c->MR_ctxt_nondetstack_zone = MR_create_zone("gen_nondetstack",
                    0, MR_gen_nonstack_size, MR_next_offset(),
                    MR_gen_nonstack_zone_size, MR_default_handler);
        } else {
            c->MR_ctxt_nondetstack_zone = MR_create_zone(nondstack_name,
                    0, nondstack_size, MR_next_offset(),
                    MR_nondstack_zone_size, MR_default_handler);
        }
    }
    /*
    ** Note that maxfr and curfr point to the last word in the frame,
    ** not to the first word, so we need to add the size of the frame,
    ** minus one word, to the base address to get the maxfr/curfr pointer
    ** for the first frame on the nondet stack.
    */
    c->MR_ctxt_maxfr = c->MR_ctxt_nondetstack_zone->MR_zone_min +
        MR_NONDET_FIXED_SIZE - 1;
    c->MR_ctxt_curfr = c->MR_ctxt_maxfr;
    MR_redoip_slot_word(c->MR_ctxt_curfr) = (MR_Word)
        MR_ENTRY(MR_do_not_reached);
    MR_redofr_slot_word(c->MR_ctxt_curfr) = (MR_Word) NULL;
    MR_prevfr_slot_word(c->MR_ctxt_curfr) = (MR_Word) NULL;
    MR_succip_slot_word(c->MR_ctxt_curfr) = (MR_Word)
        MR_ENTRY(MR_do_not_reached);
    MR_succfr_slot_word(c->MR_ctxt_curfr) = (MR_Word) NULL;

  #ifdef MR_USE_MINIMAL_MODEL_STACK_COPY
    if (gen != NULL) {
        MR_fatal_error("MR_init_context_maybe_generator: "
            "generator and stack_copy");
    }

    if (c->MR_ctxt_genstack_zone == NULL) {
        c->MR_ctxt_genstack_zone = MR_create_zone("genstack", 0,
            MR_genstack_size, MR_next_offset(),
            MR_genstack_zone_size, MR_default_handler);
    }
    c->MR_ctxt_gen_next = 0;

    if (c->MR_ctxt_cutstack_zone == NULL) {
        c->MR_ctxt_cutstack_zone = MR_create_zone("cutstack", 0,
            MR_cutstack_size, MR_next_offset(),
            MR_cutstack_zone_size, MR_default_handler);
    }
    c->MR_ctxt_cut_next = 0;

    if (c->MR_ctxt_pnegstack_zone == NULL) {
        c->MR_ctxt_pnegstack_zone = MR_create_zone("pnegstack", 0,
            MR_pnegstack_size, MR_next_offset(),
            MR_pnegstack_zone_size, MR_default_handler);
    }
    c->MR_ctxt_pneg_next = 0;
  #endif /* MR_USE_MINIMAL_MODEL_STACK_COPY */

  #ifdef MR_USE_MINIMAL_MODEL_OWN_STACKS
    c->MR_ctxt_owner_generator = gen;
  #endif /* MR_USE_MINIMAL_MODEL_OWN_STACKS */

    c->MR_ctxt_parent_sp = NULL;
    c->MR_ctxt_spark_stack = NULL;

#endif /* !MR_HIGHLEVEL_CODE */

#ifdef MR_USE_TRAIL
    if (gen != NULL) {
        MR_fatal_error("MR_init_context_maybe_generator: generator and trail");
    }

    if (c->MR_ctxt_trail_zone == NULL) {
        c->MR_ctxt_trail_zone = MR_create_zone("trail", 0,
            MR_trail_size, MR_next_offset(),
            MR_trail_zone_size, MR_default_handler);
    }
    c->MR_ctxt_trail_ptr =
        (MR_TrailEntry *) c->MR_ctxt_trail_zone->MR_zone_min;
    c->MR_ctxt_ticket_counter = 1;
    c->MR_ctxt_ticket_high_water = 1;
#endif

#ifndef MR_CONSERVATIVE_GC
    if (gen != NULL) {
        MR_fatal_error("MR_init_context: generator and no conservative gc");
    }

    c->MR_ctxt_hp = NULL;
    c->MR_ctxt_min_hp_rec = NULL;
#endif
}

MR_Context *
MR_create_context(const char *id, MR_ContextSize ctxt_size, MR_Generator *gen)
{
    MR_Context  *c;

    MR_LOCK(&free_context_list_lock, "create_context");

    MR_num_outstanding_contexts_and_sparks++;

    /*
    ** Regular contexts have stacks at least as big as
    ** small contexts, so we can return a regular context in place of
    ** a small context if one is already available.
    */
    if (ctxt_size == MR_CONTEXT_SIZE_SMALL && free_small_context_list) {
        c = free_small_context_list;
        free_small_context_list = c->MR_ctxt_next;
    } else if (free_context_list != NULL) {
        c = free_context_list;
        free_context_list = c->MR_ctxt_next;
    } else {
        c = NULL;
    }

    MR_UNLOCK(&free_context_list_lock, "create_context i");

    if (c == NULL) {
        c = MR_GC_NEW(MR_Context);
        c->MR_ctxt_size = ctxt_size;
#ifndef MR_HIGHLEVEL_CODE
        c->MR_ctxt_detstack_zone = NULL;
        c->MR_ctxt_nondetstack_zone = NULL;
#endif
#ifdef MR_USE_TRAIL
        c->MR_ctxt_trail_zone = NULL;
#endif
    }

    MR_init_context_maybe_generator(c, id, gen);
    return c;
}

void 
MR_destroy_context(MR_Context *c)
{
    MR_assert(c);

#ifndef MR_HIGHLEVEL_CODE
    MR_assert(c->MR_ctxt_spark_stack == NULL);
#endif

    /* XXX not sure if this is an overall win yet */
#if 0 && defined(MR_CONSERVATIVE_GC) && !defined(MR_HIGHLEVEL_CODE)
    /* Clear stacks to prevent retention of data. */
    MR_clear_zone_for_GC(c->MR_ctxt_detstack_zone,
        c->MR_ctxt_detstack_zone->MR_zone_min);
    MR_clear_zone_for_GC(c->MR_ctxt_nondetstack_zone,
        c->MR_ctxt_nondetstack_zone->MR_zone_min);
#endif /* defined(MR_CONSERVATIVE_GC) && !defined(MR_HIGHLEVEL_CODE) */

    MR_LOCK(&free_context_list_lock, "destroy_context");
    MR_num_outstanding_contexts_and_sparks--;

    switch (c->MR_ctxt_size) {
        case MR_CONTEXT_SIZE_REGULAR:
            c->MR_ctxt_next = free_context_list;
            free_context_list = c;
            break;
        case MR_CONTEXT_SIZE_SMALL:
            c->MR_ctxt_next = free_small_context_list;
            free_small_context_list = c;
            break;
    }
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
#ifdef  MR_CAN_DO_PENDING_IO
    int                 err;
    int                 max_id;
    int                 n_ids;
    fd_set              rd_set;
    fd_set              wr_set;
    fd_set              ex_set;
    struct timeval      timeout;
    MR_PendingContext   *pctxt;

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
        if (    ((pctxt->waiting_mode & MR_PENDING_READ) 
                && FD_ISSET(pctxt->fd, &rd_set))
            ||  ((pctxt->waiting_mode & MR_PENDING_WRITE)
                && FD_ISSET(pctxt->fd, &wr_set))
            ||  ((pctxt->waiting_mode & MR_PENDING_EXEC)
                && FD_ISSET(pctxt->fd, &ex_set))
            )
        {
            MR_schedule_context(pctxt->context);
        }
    }

    return n_ids;

#else   /* !MR_CAN_DO_PENDING_IO */

    MR_fatal_error("select() unavailable!");

#endif
}

void
MR_schedule_context(MR_Context *ctxt)
{
    MR_LOCK(&MR_runqueue_lock, "schedule_context");
    ctxt->MR_ctxt_next = NULL;
    if (MR_runqueue_tail) {
        MR_runqueue_tail->MR_ctxt_next = ctxt;
        MR_runqueue_tail = ctxt;
    } else {
        MR_runqueue_head = ctxt;
        MR_runqueue_tail = ctxt;
    }
#ifdef MR_THREAD_SAFE
    /*
    ** Wake one or more threads waiting in MR_do_runnext.  If there is a
    ** possibility that a woken thread might not accept this context then
    ** we wake up all the waiting threads.
    */
    if (ctxt->MR_ctxt_owner_thread == (MercuryThread) NULL) {
        MR_SIGNAL(&MR_runqueue_cond);
    } else {
        MR_BROADCAST(&MR_runqueue_cond);
    }
#endif
    MR_UNLOCK(&MR_runqueue_lock, "schedule_context");
}

#ifndef MR_HIGHLEVEL_CODE

void
MR_schedule_spark_globally(MR_Spark *spark)
{
    MR_LOCK(&MR_runqueue_lock, "schedule_spark_globally");
    if (MR_spark_queue_tail) {
        MR_spark_queue_tail->MR_spark_next = spark;
        MR_spark_queue_tail = spark;
    } else {
        MR_spark_queue_head = spark;
        MR_spark_queue_tail = spark;
    }
    MR_num_outstanding_contexts_and_sparks++;
  #ifdef MR_THREAD_SAFE
    MR_SIGNAL(&MR_runqueue_cond);
  #endif
    MR_UNLOCK(&MR_runqueue_lock, "schedule_spark_globally");
}


MR_define_extern_entry(MR_do_runnext);

MR_BEGIN_MODULE(scheduler_module)
    MR_init_entry_an(MR_do_runnext);
MR_BEGIN_CODE

MR_define_entry(MR_do_runnext);
#ifdef MR_THREAD_SAFE
{
    MR_Context      *tmp;
    MR_Context      *prev;
    MR_Spark        *spark;
    unsigned        depth;
    MercuryThread   thd;

    /*
    ** If this engine is holding onto a context, the context should not be in
    ** the middle of running some code.
    */
    assert(MR_ENGINE(MR_eng_this_context) == NULL ||
            MR_ENGINE(MR_eng_this_context)->MR_ctxt_spark_stack == NULL);

    depth = MR_ENGINE(MR_eng_c_depth);
    thd = MR_ENGINE(MR_eng_owner_thread);

    MR_LOCK(&MR_runqueue_lock, "MR_do_runnext (i)");

    MR_num_idle_engines++;

    while (1) {
        if (MR_exit_now == MR_TRUE) {
            /*
            ** The primordial thread has the responsibility of cleaning
            ** up the Mercury runtime.  It cannot exit by this route.
            */
            assert(thd != MR_primordial_thread);
            MR_UNLOCK(&MR_runqueue_lock, "MR_do_runnext (ii)");
            MR_destroy_thread(MR_cur_engine());
        }

        /* Search for a ready context which we can handle. */
        tmp = MR_runqueue_head;
        /* XXX check pending io */
        prev = NULL;
        while (tmp != NULL) {
            if ((depth > 0 && tmp->MR_ctxt_owner_thread == thd) ||
                (tmp->MR_ctxt_owner_thread == (MercuryThread) NULL))
            {
                MR_num_idle_engines--;
                goto ReadyContext;
            }
            prev = tmp;
            tmp = tmp->MR_ctxt_next;
        }

        /* Check if the spark queue is nonempty. */
        spark = MR_spark_queue_head;
        if (spark != NULL) {
            MR_num_idle_engines--;
            MR_num_outstanding_contexts_and_sparks--;
            goto ReadySpark;
        }

        /* Nothing to do, go back to sleep. */
        MR_WAIT(&MR_runqueue_cond, &MR_runqueue_lock);
    }

  ReadyContext:

    if (prev != NULL) {
        prev->MR_ctxt_next = tmp->MR_ctxt_next;
    } else {
        MR_runqueue_head = tmp->MR_ctxt_next;
    }
    if (MR_runqueue_tail == tmp) {
        MR_runqueue_tail = prev;
    }
    MR_UNLOCK(&MR_runqueue_lock, "MR_do_runnext (iii)");

    /* Discard whatever unused context we may have and switch to tmp. */
    if (MR_ENGINE(MR_eng_this_context) != NULL) {
        MR_destroy_context(MR_ENGINE(MR_eng_this_context));
    }
    MR_ENGINE(MR_eng_this_context) = tmp;
    MR_load_context(tmp);
    MR_GOTO(tmp->MR_ctxt_resume);

  ReadySpark:

    MR_spark_queue_head = spark->MR_spark_next;
    if (MR_spark_queue_tail == spark) {
        MR_spark_queue_tail = NULL;
    }
    MR_UNLOCK(&MR_runqueue_lock, "MR_do_runnext (iii)");

    /* Grab a new context if we haven't got one then begin execution. */
    if (MR_ENGINE(MR_eng_this_context) == NULL) {
        MR_ENGINE(MR_eng_this_context) = MR_create_context("from spark",
            MR_CONTEXT_SIZE_SMALL, NULL);
        MR_load_context(MR_ENGINE(MR_eng_this_context));
    }
    MR_parent_sp = spark->MR_spark_parent_sp;
    MR_GOTO(spark->MR_spark_resume);
}
#else /* !MR_THREAD_SAFE */
{
    /*
    ** We don't support actually putting things in the global spark queue
    ** in these grades.
    */
    assert(MR_spark_queue_head == NULL);

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
#ifdef  MR_DEEP_PROFILING
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

#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_scheduler_wrapper_write_out_proc_statics(FILE *fp)
{
    /* no proc_statics to write out */
}
#endif
