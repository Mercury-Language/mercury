/*
** Copyright (C) 1998-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_reg_workarounds.h -	MR_memcpy(), MR_fd_zero()
*/

#ifndef	MERCURY_REG_WORKAROUNDS_H
#define	MERCURY_REG_WORKAROUNDS_H

#include "mercury_conf.h"

#ifdef MR_CAN_DO_PENDING_IO
  #include <sys/types.h>		/* for fd_set */
  #include <sys/time.h>			/* for FD_ZERO() */
#endif

/*
** We use our own version of memcpy because gcc recognises calls to the
** standard memcpy (even in things that do not mention memcpy by name, e.g.
** structure assignments) and generates inline code for them. Unfortunately
** this causes gcc to abort because it tries to use a register that we have
** already reserved.
** XXX We should fix this eventually by using -fno-builtin since pragma
** c_code may call the builtin functions.
*/
extern	void	MR_memcpy(void *dest, const void *src, size_t nbytes);

/*
** We use a forwarding function to FD_ZERO because the Linux headers
** use an asm fragment which conflicts with our use of global registers.
*/

#ifdef MR_CAN_DO_PENDING_IO
  void MR_fd_zero(fd_set *fdset);
#endif

#endif /* not MERCURY_REG_WORKAROUNDS_H */
