#ifndef SPINLOCK_H
#define SPINLOCK_H

void do_spinlock(SpinLock *s);

void do_spinunlock(SpinLock *s);

#ifdef	PARALLEL

#define get_lock(addr)		do {			\
		if (numprocs != 1) {			\
			do_spinlock((SpinLock *)addr);	\
		}					\
	} while(0)

#define	release_lock(addr)	do {			\
		if (numprocs != 1) {			\
			do_spinunlock((SpinLock *)addr);	\
		}					\
	} while(0)

#else

#define	get_lock(addr)		do { } while (0)

#define	release_lock(addr)	do { } while (0)

#endif /* PARALLEL */

#endif
