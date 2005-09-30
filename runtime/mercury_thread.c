/*
** Copyright (C) 1997-2001, 2003, 2005 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include "mercury_imp.h"
#include "mercury_regs.h"
#include "mercury_engine.h"
#include "mercury_memory.h"
#include "mercury_context.h"	/* for MR_do_runnext */
#include "mercury_thread.h"

#include <stdio.h>
#include <errno.h>

#ifdef	MR_THREAD_SAFE
  MercuryThreadKey MR_exception_handler_key;
  MercuryThreadKey MR_engine_base_key;
  MercuryLock MR_global_lock;
#endif

MR_bool	MR_exit_now;
MR_bool MR_debug_threads = MR_FALSE;

#ifdef MR_THREAD_SAFE

static void *
MR_create_thread_2(void *goal);

MercuryThread *
MR_create_thread(MR_ThreadGoal *goal)
{
	MercuryThread *thread;
	pthread_attr_t attrs;
	int err;

	thread = MR_GC_NEW(MercuryThread);
	pthread_attr_init(&attrs);
	err = pthread_create(thread, &attrs, MR_create_thread_2,
		(void *) goal);

#if 0
	fprintf(stderr, "pthread_create returned %d (errno = %d)\n",
		err, errno);
#endif

	if (err != 0)
		MR_fatal_error("error creating thread");

	return thread;
}

static void *
MR_create_thread_2(void *goal0)
{
	MR_ThreadGoal *goal;

	goal = (MR_ThreadGoal *) goal0;
	if (goal != NULL) {
		MR_init_thread(MR_use_now);
		(goal->func)(goal->arg);
	} else {
		MR_init_thread(MR_use_later);
	}

	return NULL;
}

#endif /* MR_THREAD_SAFE */

MR_bool
MR_init_thread(MR_when_to_use when_to_use)
{
	MercuryEngine *eng;

#ifdef MR_THREAD_SAFE
		/* 
		** Check to see whether there is already an engine 
		** that is initialized in this thread.  If so we just
		** return, there's nothing for us to do.
		*/
	if (MR_GETSPECIFIC(MR_engine_base_key)) {
		return MR_FALSE;
	}
#endif
	eng = MR_create_engine();

#ifdef MR_THREAD_SAFE
	pthread_setspecific(MR_engine_base_key, eng);
	MR_restore_registers();
  #ifdef MR_ENGINE_BASE_REGISTER
	MR_engine_base_word = (MR_Word) eng;
  #endif
#else
	MR_memcpy(&MR_engine_base, eng,
		sizeof(MercuryEngine));
	MR_restore_registers();
#endif
	MR_load_engine_regs(MR_cur_engine());
	MR_load_context(MR_ENGINE(MR_eng_this_context));

	MR_save_registers();

#ifdef	MR_THREAD_SAFE
	MR_ENGINE(MR_eng_owner_thread) = pthread_self();
#endif

	switch (when_to_use) {
		case MR_use_later :
#ifdef MR_HIGHLEVEL_CODE
			MR_fatal_error("Sorry, not implemented: "
				"--high-level-code and multiple engines");
#else
			(void) MR_call_engine(MR_ENTRY(MR_do_runnext),
					     MR_FALSE);
#endif
			MR_destroy_engine(eng);
			return MR_FALSE;

		case MR_use_now :
			return MR_TRUE;
		
		default:
			MR_fatal_error("init_thread was passed a bad value");
	}
}

/* 
** Release resources associated with this thread.
*/
void
MR_finalize_thread_engine(void)
{
#ifdef MR_THREAD_SAFE
	MercuryEngine *eng;

	eng = MR_GETSPECIFIC(MR_engine_base_key);
	pthread_setspecific(MR_engine_base_key, NULL);
	/*
	** XXX calling destroy_engine(eng) here appears to segfault.
	** This should probably be investigated and fixed.
	*/
	MR_finalize_engine(eng);
#endif
}

#ifdef	MR_THREAD_SAFE

void
MR_destroy_thread(void *eng0)
{
	MercuryEngine *eng = eng0;
	MR_destroy_engine(eng);
	pthread_exit(0);
}

#endif

#if defined(MR_THREAD_SAFE)

void
MR_mutex_lock(MercuryLock *lock, const char *from)
{
	int err;

	fprintf(stderr, "%ld locking on %p (%s)\n",
		(long)pthread_self(), lock, from);
	err = pthread_mutex_lock(lock);
	assert(err == 0);
}

void
MR_mutex_unlock(MercuryLock *lock, const char *from)
{
	int err;

	fprintf(stderr, "%ld unlocking on %p (%s)\n",
		(long)pthread_self(), lock, from);
	err = pthread_mutex_unlock(lock);
	assert(err == 0);
}

void
MR_cond_signal(MercuryCond *cond)
{
	int err;

	fprintf(stderr, "%ld signaling %p\n",
		(long)pthread_self(), cond);
	err = pthread_cond_broadcast(cond);
	assert(err == 0);
}

void
MR_cond_wait(MercuryCond *cond, MercuryLock *lock)
{
	int err;

	fprintf(stderr, "%ld waiting on %p (%p)\n",
		(long)pthread_self(), cond, lock);
	err = pthread_cond_wait(cond, lock);
	assert(err == 0);
}

#endif
