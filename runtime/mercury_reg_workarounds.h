/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_reg_workarounds.h -	MR_fd_zero
*/

#ifndef	MERCURY_REG_WORKAROUNDS_H
#define	MERCURY_REG_WORKAROUNDS_H

#ifdef MR_CAN_DO_PENDING_IO
  #include <sys/types.h>		/* for fd_set */
#endif

/*
** We use a forwarding function to FD_ZERO because the Linux headers
** use an asm fragment which conflicts with our use of global registers.
*/

#ifdef MR_CAN_DO_PENDING_IO
  void MR_fd_zero(fd_set *fdset);
#endif

#endif /* not MERCURY_REG_WORKAROUNDS_H */
