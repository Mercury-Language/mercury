/*----------------------------------------------------------------------------*/
/* Copyright (C) 1999 The University of Melbourne.  			      */
/* This file may only be copied under the terms of the GNU Library General    */
/* Public License - see the file COPYING.LIB in the Mercury distribution.     */
/*----------------------------------------------------------------------------*/

#ifndef ME_POSIX_WORKAROUNDS_H
#define ME_POSIX_WORKAROUNDS_H

void ME_fd_zero(fd_set *fds);

void ME_fd_clr(int fd, fd_set *fds);

void ME_fd_set(int fd, fd_set *fds);

int ME_fd_isset(int fd, fd_set *fds);

#endif
