/*
** Copyright (C) 1998, 2000, 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_signal.h - functions for setting up signal handlers.
**
** This defines a generic signal handler setup mechanism.
**
** NOTE: If `struct sigcontext' is needed, mercury_signal.h must be
** included before anything which could include <signal.h>.
*/

#ifndef	MERCURY_SIGNAL_H
#define	MERCURY_SIGNAL_H

#include "mercury_conf.h"

#ifdef MR_HAVE_SIGCONTEXT_STRUCT
  /*
  ** Some versions of Linux call it struct sigcontext_struct, some call it
  ** struct sigcontext.  The following #define eliminates the differences.
  */
  #define sigcontext_struct sigcontext /* must be before #include <signal.h> */
  struct sigcontext; /* this forward decl avoids a gcc warning in signal.h */

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
  #ifdef MR_HAVE_ASM_SIGCONTEXT_H
    #include <asm/sigcontext.h>
  #endif 
#else
  #include <signal.h>
#endif

#include "mercury_types.h"
#include "mercury_std.h"

#ifdef MR_HAVE_SIGACTION
typedef struct sigaction	MR_signal_action;
#else
typedef MR_Code *		MR_signal_action;
#endif

	/*
	** MR_setup_signal sets a signal handler (handler) to handle
	** signals of the given signal type (sig).  
	** If the handler cannot be setup, it aborts with the given
	** error message.
	** 
	** If the signal handler requires siginfo to be provided (e.g.
	** it needs access to stored registers), need_info must be 
	** MR_TRUE.  Note that on some platforms, signal information is
	** provided regardless of the value of need_info.
	*/
extern void MR_setup_signal(int sig, MR_Code *handler, MR_bool need_info, 
	const char * error_message);

	/*
	** As above, but don't arrange for system calls to be
	** restarted if the signal is received.
	*/
extern void MR_setup_signal_no_restart(int sig, MR_Code *handler,
	MR_bool need_info, const char * error_message);

	/*
	** Get the current action for the given signal.
	** If the action cannot be retrieved, it aborts with the given
	** error message.
	*/
extern void MR_get_signal_action(int sig, MR_signal_action *old_action,
	const char *error_message);

	/*
	** Restore the action for the given signal to the result
	** of a previous call to MR_get_signal_action().
	** If the action cannot be set, it aborts with the given
	** error message.
	*/
extern void MR_set_signal_action(int sig, MR_signal_action *action,
	const char *error_message);

	/*
	** Change the behaviour of system calls when the
	** the specified signal is received.
	** If restart is MR_TRUE they will be restarted, if it
	** is MR_FALSE, they won't. This function may have no
	** effect on some systems.
	*/
extern void MR_signal_should_restart(int sig, MR_bool restart);

#endif /* not MERCURY_SIGNAL_H */
