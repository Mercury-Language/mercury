/*
** Copyright (C) 1993-1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	TIMING_H
#define	TIMING_H

/*---------------------------------------------------------------------------*/
/* I hacked this from toplev.c in the gcc source - fjh. */
/*---------------------------------------------------------------------------*/

/* Copyright (C) 1987, 1988, 1989, 1992, 1993 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Return time used so far, in milliseconds.  */

extern	int	get_run_time (void);

/* Print a message and a time in milliseconds. */

extern	void	print_time(const char *str, int total);

#define TIMEVAR(VAR, BODY)    					\
	do {							\
		int otime = get_run_time ();			\
		BODY;						\
		VAR = get_run_time () - otime;			\
	} while (0)	

#endif /* not TIMING_H */
