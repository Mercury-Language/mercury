/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** All the functions in this file work around problems caused by
** our use of global registers conflicting with the use of registers
** by gcc, or asm fragments in the GNU headers.
*/

#include "mercury_conf.h"
#include "mercury_reg_workarounds.h"

#ifdef	MR_CAN_DO_PENDING_IO

#include <sys/types.h>	/* for fd_set and FD_ZERO() */
#include <sys/time.h>	/* for FD_ZERO() */
#include <unistd.h>	/* for FD_ZERO() */

void
MR_fd_zero(fd_set *fdset)
{
	FD_ZERO(fdset);
}

#endif /* MR_CAN_DO_PENDING_IO */
