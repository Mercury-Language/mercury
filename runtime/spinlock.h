#ifndef SPINLOCK_H
#define SPINLOCK_H

/*
** This part of the header file documents the abstract interface for a spinlock.
*/

#if 0 /* everything here is really implemented as macros */

/*
** You can assume that a SpinLock will fit into a Word,
** i.e. that sizeof(SpinLock) <= sizeof(Word).
** But you should not assume anything else.
*/
typedef ... SpinLock;

/*
** allocate_lock() returns a pointer to a new initialized lock,
** allocated in shared memory.
** deallocate_lock() deinitializes and deallocates a lock.
*/
extern	SpinLock *allocate_lock(void);
extern	void deallocate_lock(SpinLock *);

/*
** get_lock() and release_lock() are used to
** acquire and relinquish a lock.
*/
void get_lock(SpinLock *);
void release_lock(SpinLock *);

#endif /* end of #if 0 */

/*---------------------------------------------------------------------------*/

/*
** The following are implementation details.
** They're defined here in the header file so that they can be inlined,
** but code elsewhere should avoid depending on these details.
*/

#include "imp.h"	/* for `Word' */
#include <stddef.h>	/* for `NULL' */
#include "conf.h"	/* for `PARALLEL' */
#include "context.h"	/* for `numprocs' */

typedef Word SpinLock;

void do_spinlock(SpinLock *s);

void do_spinunlock(SpinLock *s);

#ifdef	PARALLEL

#define get_lock(addr)		do {			\
		if (numprocs != 1) {			\
			do_spinlock(addr);	\
		}					\
	} while(0)

#define	release_lock(addr)	do {			\
		if (numprocs != 1) {			\
			do_spinunlock(addr);	\
		}					\
	} while(0)

#define allocate_lock() 	allocate_type(SpinLock)

#define deallocate_lock(lock)	deallocate(lock)

#else

#define	get_lock(addr)		do { } while (0)

#define	release_lock(addr)	do { } while (0)

#define allocate_lock() 	NULL

#define deallocate_lock(lock)	do { } while(0)


#endif /* PARALLEL */

#endif
