/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file defines stuff used by the automatically generated *_init.c files.
*/

#ifndef	INIT_H
#define	INIT_H

#include "imp.h"

Declare_entry(mercury__main_2_0);
Declare_entry(mercury__io__run_0_0);

extern	void		mercury_init_io(void);

extern	int		mercury_main(int argc, char **argv);

extern	Code *		library_entry_point; /* normally mercury__io__run_0_0 */
extern	Code *		program_entry_point; /* normally mercury__main_2_0; */
extern	void		(*address_of_mercury_init_io)(void);
extern	void		(*address_of_init_modules)(void);
#ifdef CONSERVATIVE_GC
extern	void		(*address_of_init_gc)(void);
#endif

#endif
