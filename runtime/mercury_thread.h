#ifndef MERCURY_THREAD_H
#define MERCURY_THREAD_H

#ifdef	MR_THREAD_SAFE

  #include <signal.h>	/* for sigset_t on the SPARC */
  #include <pthread.h>
  #include "mercury_std.h"

  #if defined(MR_DIGITAL_UNIX_PTHREADS)
    #define MR_MUTEX_ATTR		pthread_mutexattr_default
    #define MR_COND_ATTR		pthread_condattr_default
    #define MR_THREAD_ATTR	pthread_attr_default
  #else
    #define MR_MUTEX_ATTR		NULL
    #define MR_COND_ATTR		NULL
    #define MR_THREAD_ATTR	NULL
  #endif

typedef	pthread_t	MercuryThread;
typedef pthread_key_t	MercuryThreadKey;
typedef pthread_mutex_t	MercuryLock;
typedef pthread_cond_t	MercuryCond;

  #if 0
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

MercuryThread	*create_thread(int x);
void		destroy_thread(void *eng);
extern bool	MR_exit_now;

#else /* not MR_THREAD_SAFE */

  #define MR_LOCK(nothing, from)	do { } while (0)
  #define MR_UNLOCK(nothing, from)	do { } while (0)

  #define MR_SIGNAL(nothing)		do { } while (0)
  #define MR_WAIT(no, thing)		do { } while (0)

#endif

void	*init_thread(void *);

#endif
