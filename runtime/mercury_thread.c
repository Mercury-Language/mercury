/*
INIT mercury_scheduler_wrapper
ENDINIT
*/
/*
** Copyright (C) 1997-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include "mercury_imp.h"
#include "mercury_regs.h"
#include "mercury_engine.h"
#include "mercury_memory.h"
#include "mercury_thread.h"

#include <stdio.h>
#include <errno.h>

#ifdef	MR_THREAD_SAFE
  MercuryThreadKey MR_engine_base_key;
#endif

bool	MR_exit_now;

void *init_thread(void *unused);

Declare_entry(do_runnext);

#ifdef MR_THREAD_SAFE
MercuryThread *
create_thread(int x)
{
	MercuryThread *thread;
	pthread_attr_t attrs;
	int err;

	thread = make(MercuryThread);
	pthread_attr_init(&attrs);
	err = pthread_create(thread, &attrs, init_thread, (void *) x);

#if 0
	fprintf(stderr, "pthread_create returned %d (errno = %d)\n",
		err, errno);
#endif

	if (err != 0)
		fatal_error("error creating thread");

	return thread;
}
#endif /* MR_THREAD_SAFE */

void *
init_thread(void *unused)
{
	MercuryEngine *eng;

	eng = create_engine();

#ifdef	MR_THREAD_SAFE
	pthread_setspecific(MR_engine_base_key, eng);
#endif

#ifdef MR_ENGINE_BASE_REGISTER
	restore_registers();

	MR_engine_base = eng;
	load_engine_regs(eng);
	load_context(eng->this_context);

	save_registers();
#else
	MR_memcpy((void *) &MR_engine_base, (void *) eng,
		sizeof(MercuryEngine));
	restore_registers();

	load_engine_regs(&MR_engine_base);
	load_context(MR_engine_base.this_context);

	save_registers();
#endif

#ifdef	MR_THREAD_SAFE
	MR_ENGINE(owner_thread) = pthread_self();
#endif

	if (unused == 0) {
		call_engine(ENTRY(do_runnext));

		destroy_engine(eng);
	}

	return NULL;
}

#ifdef	MR_THREAD_SAFE
void
destroy_thread(void *eng0)
{
	MercuryEngine *eng = eng0;
	destroy_engine(eng);
	pthread_exit(0);
}
#endif

#ifdef	MR_THREAD_SAFE
void
MR_mutex_lock(MercuryLock *lock, const char *from)
{
	int err;

#if 0
	fprintf(stderr, "%d locking on %p (%s)\n", pthread_self(), lock, from);
#endif
	err = pthread_mutex_lock(lock);
	assert(err == 0);
}

void
MR_mutex_unlock(MercuryLock *lock, const char *from)
{
	int err;

#if 0
	fprintf(stderr, "%d unlocking on %p (%s)\n",
		pthread_self(), lock, from);
#endif
	err = pthread_mutex_unlock(lock);
	assert(err == 0);
}

void
MR_cond_signal(MercuryCond *cond)
{
	int err;

#if 0
	fprintf(stderr, "%d signaling %p\n", pthread_self(), cond);
#endif
	err = pthread_cond_broadcast(cond);
	assert(err == 0);
}

void
MR_cond_wait(MercuryCond *cond, MercuryLock *lock)
{
	int err;

#if 0
	fprintf(stderr, "%d waiting on %p (%p)\n", pthread_self(), cond, lock);
#endif
	err = pthread_cond_wait(cond, lock);
	assert(err == 0);
}
#endif


Define_extern_entry(do_runnext);

BEGIN_MODULE(scheduler_module)
	init_entry(do_runnext);
BEGIN_CODE

Define_entry(do_runnext);
	runnext();

	fatal_error("Execution should never reach here.");

END_MODULE

void mercury_scheduler_wrapper(void); /* suppress gcc warning */
void mercury_scheduler_wrapper(void) {
	scheduler_module();
}

