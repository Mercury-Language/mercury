/*
** Copyright (C) 1997-1998, 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
#ifndef MERCURY_THREAD_H
#define MERCURY_THREAD_H

#include "mercury_std.h"

#ifdef	MR_THREAD_SAFE

  #include <signal.h>	/* for sigset_t on the SPARC */
  #include <pthread.h>

  #if defined(MR_DIGITAL_UNIX_PTHREADS)
    #define MR_MUTEX_ATTR		pthread_mutexattr_default
    #define MR_COND_ATTR		pthread_condattr_default
    #define MR_THREAD_ATTR		pthread_attr_default
  #else
    #define MR_MUTEX_ATTR		NULL
    #define MR_COND_ATTR		NULL
    #define MR_THREAD_ATTR	NULL
  #endif

  typedef pthread_t		MercuryThread;
  typedef pthread_key_t		MercuryThreadKey;
  typedef pthread_mutex_t	MercuryLock;
  typedef pthread_cond_t	MercuryCond;

  #ifndef MR_DEBUG_THREADS
	/*
	** The following macros should be used once the
	** use of locking in the generated code is considered
	** stable, since the alternative versions do the
	** same thing, but with debugging support.
	*/
    #define	MR_LOCK(lck, from)	pthread_mutex_lock((lck))
    #define	MR_UNLOCK(lck, from)	pthread_mutex_unlock((lck))

    #define	MR_SIGNAL(cnd)		pthread_cond_signal((cnd))
    #define	MR_WAIT(cnd, mtx)	pthread_cond_wait((cnd), (mtx))
  #else
    void MR_mutex_lock(MercuryLock *lock, const char *from);
    void MR_mutex_unlock(MercuryLock *lock, const char *from);
    void MR_cond_signal(MercuryCond *cond);
    void MR_cond_wait(MercuryCond *cond, MercuryLock *lock);

    #define	MR_LOCK(lck, from)	MR_mutex_lock((lck), (from))
    #define	MR_UNLOCK(lck, from)	MR_mutex_unlock((lck), (from))

    #define	MR_SIGNAL(cnd)		MR_cond_signal((cnd))
    #define	MR_WAIT(cnd, mtx)	MR_cond_wait((cnd), (mtx))
  #endif

    	/*
	** The following two macros are used to protect pragma c_code
	** predicates which are not thread-safe.
	** See the comments below.
	*/
  #define MR_OBTAIN_GLOBAL_LOCK(where)	MR_LOCK(&MR_global_lock, (where))
  #define MR_RELEASE_GLOBAL_LOCK(where)	MR_UNLOCK(&MR_global_lock, (where))

  #if defined(MR_DIGITAL_UNIX_PTHREADS)
    #define MR_GETSPECIFIC(key) 	({		\
		pthread_addr_t gstmp;			\
		pthread_getspecific((key), &gstmp);	\
		(void *) gstmp;				\
	})
    #define	MR_KEY_CREATE	pthread_keycreate
  #else
    #define 	MR_GETSPECIFIC(key)	pthread_getspecific((key))
    #define	MR_KEY_CREATE		pthread_key_create
  #endif

  typedef struct {
	  void	(*func)(void *);
	  void	*arg;
  } MR_ThreadGoal;

  /*
  ** create_thread(Goal) creates a new POSIX thread, and creates and
  ** initializes a new Mercury engine to run in that thread. If Goal
  ** is a NULL pointer, that thread will suspend on the global Mercury
  ** runqueue. If Goal is non-NULL, it is a pointer to a MR_ThreadGoal
  ** structure containing a function and an argument. The function will
  ** be called with the given argument in the new thread.
  */
  MercuryThread		*MR_create_thread(MR_ThreadGoal *);
  void			MR_destroy_thread(void *eng);
  extern MR_bool	MR_exit_now;

	/*
	** MR_global_lock is a mutex for ensuring that only one non-threadsafe
	** piece of pragma c code executes at a time. If `not_threadsafe' is
	** given or `threadsafe' is not given in the attributes of a pragma
	** c code definition of a predicate, then the generated code will
	** obtain this lock before executing the C code fragment, and then
	** release it afterwards.
	** XXX we should emit a warning if may_call_mercury and not_threadsafe
	** (the defaults) are specified since if you obtain the lock then
	** call back into Mercury deadlock could result.
	*/
  extern MercuryLock MR_global_lock;

#else /* not MR_THREAD_SAFE */

  #define MR_LOCK(nothing, from)	do { } while (0)
  #define MR_UNLOCK(nothing, from)	do { } while (0)

  #define MR_SIGNAL(nothing)		do { } while (0)
  #define MR_WAIT(no, thing)		do { } while (0)

  #define MR_OBTAIN_GLOBAL_LOCK(where)	do { } while (0)
  #define MR_RELEASE_GLOBAL_LOCK(where)	do { } while (0)

#endif

/*
** The following enum is used as the argument to init_thread.
** MR_use_now should be passed to init_thread to indicate that
** it has been called in a context in which it should initialize
** the current thread's environment and return.
** MR_use_later should be passed to indicate that the thread should
** be initialized, then suspend waiting for work to appear in the
** runqueue. The engine is destroyed when the execution of work from
** the runqueue returns.
*/
typedef enum { MR_use_now, MR_use_later } MR_when_to_use;

/*
** Create and initialize a new Mercury engine running in the current
** POSIX thread.
**
** See the comments above for the meaning of the argument.
** If there is already a Mercury engine running in the current POSIX
** thread then init_thread is just a no-op.
**
** Returns MR_TRUE if a Mercury engine was created as a result of this
** call *and* it is the caller's responsibility to finalize it (it is
** intended that the caller can store the return value and call
** finalize_thread_engine if it is true).
*/

extern	MR_bool	MR_init_thread(MR_when_to_use);

/*
** Finalize the thread engine running in the current POSIX thread.
** This will release the resources used by this thread -- this is very
** important because the memory used for the det stack for each thread
** can be re-used by the next init_thread.
*/

extern	void    MR_finalize_thread_engine(void);

#endif
