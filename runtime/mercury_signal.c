/*
** Copyright (C) 1998,2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module defines functions for setting up signal handlers.
*/

/*---------------------------------------------------------------------------*/

#include "mercury_imp.h"

#ifdef HAVE_SIGCONTEXT_STRUCT
  /*
  ** Some versions of Linux call it struct sigcontext_struct, some call it
  ** struct sigcontext.  The following #define eliminates the differences.
  */
  #define sigcontext_struct sigcontext /* must be before #include <signal.h> */

  /*
  ** On some systems (e.g. most versions of Linux) we need to #define
  ** __KERNEL__ to get sigcontext_struct from <signal.h>.
  ** This stuff must come before anything else that might include <signal.h>,
  ** otherwise the #define __KERNEL__ may not work.
  */
  #define __KERNEL__
  #include <signal.h>	/* must come third */
  #undef __KERNEL__

  /*
  ** Some versions of Linux define it in <signal.h>, others define it in
  ** <asm/sigcontext.h>.  We try both.
  */
  #ifdef HAVE_ASM_SIGCONTEXT
    #include <asm/sigcontext.h>
  #endif 
#else
  #include <signal.h>
#endif

#ifdef HAVE_UNISTD_H
  #include <unistd.h>
#endif

#include <stdio.h>
#include <string.h>
#include <errno.h>

#ifdef HAVE_SYS_SIGINFO
  #include <sys/siginfo.h>
#endif 

#ifdef	HAVE_MPROTECT
  #include <sys/mman.h>
#endif

#ifdef	HAVE_UCONTEXT
  #include <ucontext.h>
#endif

#ifdef	HAVE_SYS_UCONTEXT
  #include <sys/ucontext.h>
#endif

#include "mercury_imp.h"
#include "mercury_signal.h"

/*---------------------------------------------------------------------------*/

/*
** If we don't have SA_RESTART or SA_SIGINFO, defined them as 0.
** It would be nice to have them, but it is still better to use
** sigaction without SA_RESTART or SA_SIGINFO than to use signal.
*/
#if	!defined(SA_RESTART)
  #define	SA_RESTART 0
#endif

#if	!defined(SA_SIGINFO)
  #define	SA_SIGINFO 0
#endif

void
MR_setup_signal(int sig, MR_Code *handler, bool need_info, 
		const char *error_message)
{
#if	defined(HAVE_SIGACTION)

	struct sigaction	act;

	if (need_info) {
	/*
	** If we are using sigcontext struct, it means we have
	** configured to not use siginfo, and so when we
	** request signals, we should not ask for SA_SIGINFO, since our
	** handler will not be of the right type.
	*/
#if	defined(HAVE_SIGCONTEXT_STRUCT)
		act.sa_flags = SA_RESTART;
#else	/* not HAVE_SIGCONTEXT_STRUCT */
		act.sa_flags = SA_SIGINFO | SA_RESTART;
#endif
	} else {
		act.sa_flags = SA_RESTART;
	}
	if (sigemptyset(&act.sa_mask) != 0) {
		perror("Mercury runtime: cannot set clear signal mask");
		exit(1);
	}
	errno = 0;

	act.SIGACTION_FIELD = handler;
	if (sigaction(sig, &act, NULL) != 0) {
		perror(error_message);
		exit(1);
	}

#else /* not HAVE_SIGACTION */

	if (signal(sig, handler) == SIG_ERR) {
		perror(error_message);
		exit(1);
	}
#endif /* not HAVE_SIGACTION */
}

