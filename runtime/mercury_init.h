/*
** Copyright (C) 1993-1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** init.h - this file declares stuff defined in the automatically generated
** *_init.c files.  This is also the interface used by C code that
** wishes to interface to Mercury.
**
** It also declares some stuff that is used in the automatically
** generate *_init.c files.
*/

#ifndef	INIT_H
#define	INIT_H

/*
** The following must come before any definitions of global variables.
** This is necessary to support DLLs on Windows.
*/
#include "mercury_conf.h" /* for USE_DLLS */
#if USE_DLLS
  #include "libmer_dll.h"
#endif

/*---------------------------------------------------------------------------*/
/*
** This part is the interface that should be used by C programs that wish
** to interface to Mercury.
*/

/*
** mercury_main() is defined in the <module>_init.c file.
** It calls mercury_init(), mercury_call_main(), and then mercury_terminate().
*/
extern	int	mercury_main(int argc, char **argv);

/*
** mercury_init() is defined in the <module>_init.c file.
**
** The `argc' and `argv' parameters are as for main() in C.
** The `stack_bottom' parameter should be the address of a variable
** on the C stack.  The conservative garbage collector treats that
** address as the start of the stack, so anything older than that
** address won't get scanned; don't store pointers to GC'ed memory
** in local variables that are older than that.
**
** mercury_init() just does some stuff to initialize the garbage
** collector, sets some global variables, and then calls
** mercury_runtime_init().
*/
extern	void	mercury_init(int argc, char **argv, char *stack_bottom);

/*
** mercury_call_main() is defined in the <module>_init.c file.
** It just calls mercury_runtime_main(), which calls main/2
** in the Mercury program.
*/
extern	void	mercury_call_main(void);

/*
** mercury_terminate() is defined in the <module>_init.c file.
** It just calls mercury_runtime_terminate(), which performs
** any necessary cleanup, and then returns the appropriate
** exit status as set by io__set_exit_status.
*/
extern	int	mercury_terminate(void);

/*---------------------------------------------------------------------------*/

/*
** This part defines things which are used by the automatically
** generated *_init.c file.  These should not be used (directly)
** by C programs that wish to interface to Mercury.
*/

#include "goto.h"		/* for Declare_entry */
#include "mercury_types.h"	/* for `Word' */
#include "wrapper.h"		/* for do_init_modules,
				   mercury_runtime_init(),
				   mercury_runtime_main(),
				   mercury_runtime_terminate(),
				   etc. */

#ifdef CONSERVATIVE_GC
  #include "gc.h"
#endif

/*
** mercury_main() takes the address of the following predicates/functions,
** which are defined elsewhere.
*/
Declare_entry(mercury__main_2_0);		/* in the user's program */
extern	void	mercury_init_io(void);		/* in the Mercury library */
extern	void	ML_io_init_state(void);		/* in the Mercury library */
extern	void	ML_io_finalize_state(void);	/* in the Mercury library */

#endif /* not INIT_H */

/*---------------------------------------------------------------------------*/
