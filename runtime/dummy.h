/*
** Copyright (C) 1993-1995 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** File: dummy.h 
** Author: fjh
**
** Global variables and functions used purely for the purpose
** of suppressing over-zealous compiler optimizations.
*/

#ifndef	DUMMY_H
#define	DUMMY_H

extern	void	dummy_function_call(void);
extern	void	*global_pointer;
extern	void	*global_pointer_2;

#endif
