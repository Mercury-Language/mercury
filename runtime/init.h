/*
** Copyright (C) 1995-1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** init.h - this file defines stuff used by the automatically generated
** _init.c files.
*/

#ifndef	INIT_H
#define	INIT_H

#include "goto.h"		/* for Declare_entry */
#include "mercury_types.h"	/* for `Code *' */

/*
** mercury_main() is defined in the <module>_init.c file.
**
** The `argc' and `argv' parameters are as for main() in C.
** The `stack_bottom' parameter should be the address of a variable
** on the C stack.  The conservative garbage collector treats that
** address as the start of the stack, so anything older than that
** address won't get scanned; don't store pointers to GC'ed memory
** in local variables that are older than that.
**
** mercury_main() just does some stuff to initialize the garbage
** collector, sets some global variables, and then calls
** mercury_runtime_main().
*/
extern	int		mercury_main(int argc, char **argv, char *stack_bottom);

/*
** mercury_runtime_main() is defined in wrapper.mod.
** It does some stuff to initialize the garbage collector
** and the Mercury engine's data areas, and then calls call_engine()
** to start execution in the library entry point.  The library
** entry point initializes the io__state and then calls the program
** entry point.
*/
extern	int		mercury_runtime_main(int argc, char **argv);

/*
** mercury_main() takes the address of the following predicates/functions.
*/
Declare_entry(mercury__main_2_0);
Declare_entry(mercury__io__run_0_0);
extern	void		mercury_init_io(void);

/*
** The following global variables are defined in wrapper.mod,
** and set by mercury_main() on startup.
** The entry points are set based on the options to mkinit.c.
** The address_of_foo pointers are set to the address of
** the corresponding foo.
*/
extern	Code *		library_entry_point; /* normally mercury__io__run_0_0 */
extern	Code *		program_entry_point; /* normally mercury__main_2_0; */

extern	void		(*address_of_mercury_init_io)(void);
extern	void		(*address_of_init_modules)(void);
#ifdef CONSERVATIVE_GC
extern	void		(*address_of_init_gc)(void);
#endif

#endif /* INIT_H */
