/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
/*
**	Various configuration parameters. Their meanings are:
**
**	HAVE_SYSCONF		the machine has the sysconf syscall.
**	HAVE_MEMALIGN		the machine has the memalign libcall.
**	HAVE_MPROTECT		the machine has the mprotect syscall.
**	HAVE_SIGINFO		sighandlers are given siginfo arguments.
**	HAVE_UCONTEXT		sighandlers are given ucontext arguments
**				and we need ucontext.h to get access.
**	PC_INDEX		index of PC in ucontexts.
**
**	At the moment, the answers are valid only for
**
**	Suns running SunOS 4.x
**	Suns running SunOS 5.x
**	SGIs running IRIX 4.x
**	SGIs running IRIX 5.x
*/

#if	defined(__sparc) && !defined(__svr4__)
/* e.g. cadillac */
#define	KNOWN
#endif

#if	defined(__sparc) && defined(__svr4__)
/* e.g. kryten */
#define	HAVE_MEMALIGN
#define	HAVE_MPROTECT
#define	HAVE_SIGINFO
#define	HAVE_SYSCONF
#define	HAVE_UCONTEXT
#define	PC_INDEX	REG_PC
#define	SIGACTION_FIELD	sa_sigaction
#define	KNOWN
#endif

#if	defined(__sgi) && !defined(__SYSTYPE_SVR4__)
/* e.g. munta */
#define HAVE_CWD_DECL
#define	KNOWN
#endif

#if	defined(__sgi) && defined(__SYSTYPE_SVR4__)
/* e.g. mundil */
#define	HAVE_MEMALIGN
#define	HAVE_MPROTECT
#define	HAVE_SIGINFO
#define	HAVE_SYSCONF
#define	HAVE_UCONTEXT
#define	PC_INDEX	CTX_EPC
#define	SIGACTION_FIELD	sa_handler
#define	KNOWN
#endif

#ifndef	KNOWN
#error	"machine type with unknown capabilities"
#endif
