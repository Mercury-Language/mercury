/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	EXT_SIGNAL_H
#define	EXT_SIGNAL_H

/* do we really need all this junk? */

#ifdef	HAVE_SIGINFO

#include	<sys/siginfo.h>

#ifndef __sgi
typedef struct {		/* signal set type */
	unsigned long	__sigbits[4];
} sigset_t;

struct sigaction {
	int sa_flags;
#if	defined(__sparc)
	union {
		void (*_handler)(int);
		void (*_sigaction)(int, siginfo_t *, void *);
	}	_funcptr;
#define	sa_handler	_funcptr._handler
#define	sa_sigaction	_funcptr._sigaction
#elif	defined(__sgi)
		void (*sa_handler)(int);
#else
#error	"unknown machine type"
#endif
	sigset_t sa_mask;
	int sa_resv[2];
};

/* definitions for the sa_flags field */
#define	SA_ONSTACK	0x00000001
#define	SA_RESETHAND	0x00000002
#define	SA_RESTART	0x00000004
#define	SA_SIGINFO	0x00000008
#define	SA_NODEFER	0x00000010

/* this is only valid for SIGCLD */
#define	SA_NOCLDWAIT	0x00010000	/* don't save zombie children	 */

/* this is only valid for SIGWAITING */
#define	SA_WAITSIG	0x00010000	/* send SIGWAITING if all lwps block */

/* this is only valid for SIGCLD */
#define	SA_NOCLDSTOP	0x00020000	/* don't send job control SIGCLD's */


/*
 * use of these symbols by applications is injurious
 *	to binary compatibility, use _sys_nsig instead
 */
#define	NSIG	44	/* valid signals range from 1 to NSIG-1 */
#define	MAXSIG	43	/* size of u_signal[], NSIG-1 <= MAXSIG */
			/* Note: when changing MAXSIG, be sure to update the */
			/* sizes of u_sigmask and u_signal in uts/adb/u.adb. */

#define	S_SIGNAL	1
#define	S_SIGSET	2
#define	S_SIGACTION	3
#define	S_NONE		4

#define	SIGSTKSZ	8192

#define	SS_ONSTACK	0x00000001
#define	SS_DISABLE	0x00000002

struct sigaltstack {
	char	*ss_sp;
	int	ss_size;
	int	ss_flags;
};

typedef struct sigaltstack stack_t;

extern int kill(pid_t, int);
extern int sigaction(int, const struct sigaction *, struct sigaction *);
#ifndef	sigaddset
extern int sigaddset(sigset_t *, int);
#endif
#ifndef	sigdelset
extern int sigdelset(sigset_t *, int);
#endif
#ifndef	sigemptyset
extern int sigemptyset(sigset_t *);
#endif
#ifndef	sigfillset
extern int sigfillset(sigset_t *);
#endif
#ifndef	sigismember
extern int sigismember(const sigset_t *, int);
#endif
extern int sigpending(sigset_t *);
extern int sigprocmask(int, const sigset_t *, sigset_t *);
extern int sigsuspend(const sigset_t *);

#endif

#endif /* HAVE_SIGINFO */

#endif /* EXT_SIGNAL_H */
