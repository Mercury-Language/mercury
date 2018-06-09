/*----------------------------------------------------------------------------*/
/* Copyright (C) 1999 The University of Melbourne.  			      */
/* Copyright (C) 2018 The Mercury team.                                       */
/* This file is distributed under the terms specified in COPYING.LIB.         */
/*----------------------------------------------------------------------------*/
/*									      */
/* This file contains a bunch of functions for working on fd_set objects.     */
/* The reason that these are necessary is that gcc generates inline assembler */
/* for these which conflicts with our use of global registers.		      */
/*									      */
/*----------------------------------------------------------------------------*/

#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>

#include "posix_workarounds.h"

void ME_fd_zero(fd_set *fds)
{
	FD_ZERO(fds);
}

void ME_fd_clr(int fd, fd_set *fds)
{
	FD_CLR(fd, fds);
}

void ME_fd_set(int fd, fd_set *fds)
{
	FD_SET(fd, fds);
}

int ME_fd_isset(int fd, fd_set *fds)
{
	return FD_ISSET(fd, fds);
}

