/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_signal.h - functions for setting up signal handlers.
**
** This defines a generic signal handler setup mechanism.
*/

#ifndef	MERCURY_SIGNAL_H
#define	MERCURY_SIGNAL_H

	/*
	** MR_setup_signal sets a signal handler (handler) to handle
	** signals of the given signal type (sig).  
	** If the handler cannot be setup, it aborts with the given
	** error message.
	** 
	** If the signal handler requires siginfo to be provided (e.g.
	** it needs access to stored registers), need_info must be 
	** TRUE.  Note that on some platforms, signal information is
	** provided regardless of the value of need_info.
	*/
extern void MR_setup_signal(int sig, Code *handler, int need_info, 
	const char * error_message);


#endif /* not MERCURY_SIGNAL_H */
