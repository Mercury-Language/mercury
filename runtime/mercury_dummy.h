/*
** Copyright (C) 1993-1995, 1997, 1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** File: mercury_dummy.h 
** Author: fjh
**
** Global variables and functions used purely for the purpose
** of suppressing over-zealous compiler optimizations.
*/

#ifndef	MERCURY_DUMMY_H
#define	MERCURY_DUMMY_H

extern	void	dummy_function_call(void);
extern	void	*dummy_identify_function(void *);
extern	void	*volatile global_pointer;
extern	void	*volatile global_pointer_2;

#endif /* not MERCURY_DUMMY_H */
