/*
** Copyright (C) 1993-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_init.h - this file declares stuff defined in the
** automatically generated *_init.c files.  This is also the interface
** used by C code that wishes to interface to Mercury.
**
** It also declares some stuff that is used in the automatically
** generate *_init.c files.
*/

#ifndef	MERCURY_INIT_H
#define	MERCURY_INIT_H

/*
** The following must come before any definitions of global variables.
** This is necessary to support DLLs on Windows.
*/
#include "mercury_conf.h" /* for USE_DLLS */
#if USE_DLLS
  #include "libmer_rt_dll.h"
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

#include "mercury_goto.h"		/* for Declare_entry */
#include "mercury_types.h"	/* for `Word' */
#include "mercury_wrapper.h"		/* for do_init_modules,
				   mercury_runtime_init(),
				   mercury_runtime_main(),
				   mercury_runtime_terminate(),
				   etc. */
#include "mercury_trace_base.h"	/* for MR_trace_port */

#ifdef CONSERVATIVE_GC
  #include "gc.h"
#endif

/*
** mercury_main() takes the address of the following predicates/functions,
** which are defined elsewhere.
**
** These declarations duplicate some of the contents of the automatically
** generated header files for some of the library modules, and therefore
** represent a potential double maintenance problem. At the moment we
** accept this because it avoids having the runtime rely on the library.
** However, the dependence on browser/debugger_interface.m is unnecessary,
** since the only code that relies on the ML_DI_* variables below is
** in the trace directory, which is allowed to rely on the browser
** directory.
*/

/* in the user's program */
Declare_entry(mercury__main_2_0);

/* in library/io.h */
extern	void	mercury_init_io(void);
extern	void	ML_io_init_state(void);
extern	void	ML_io_finalize_state(void);
extern	void	ML_io_stderr_stream(Word *);
extern	void	ML_io_stdout_stream(Word *);
extern	void	ML_io_stdin_stream(Word *);
extern	void	ML_io_print_to_cur_stream(Word, Word);
extern	void	ML_io_print_to_stream(Word, Word, Word);

/* in trace/mercury_trace_declarative.h */
extern	void	MR_edt_root_node(Word EDT, Word *Node);

/* in trace/mercury_trace_external.h */
extern	void	MR_trace_init_external(void);
extern	void	MR_trace_final_external(void);

/* in browser/debugger_interface.h */
extern	void	ML_DI_output_current_vars(Word, Word, Word);
		/* output_current_vars/4 */
extern	void	ML_DI_output_current_nth_var(Word, Word);
		/* output_current_nth_var/3 */
extern	void	ML_DI_output_current_live_var_names(Word, Word, Word);
		/* output_current_live_var_names/5 */
extern	void	ML_DI_output_current_slots(Integer, Integer, Integer, Word,
		String, String, Integer, Integer, Integer, String, Word);
		/* output_current_slots/13 */
extern	bool	ML_DI_found_match(Integer, Integer, Integer, Word, String,
		String, Integer, Integer, Integer, Word, String, Word);
		/* found_match/12 */
extern	Integer	ML_DI_get_var_number(Word);
extern	void	ML_DI_read_request_from_socket(Word, Word *, Integer *);

/* in library/std_util.m  */
extern	String	ML_type_name(Word);

/* in runtime/mercury_trace_base.c */
extern	Code	*MR_trace_fake(const MR_Stack_Layout_Label *, MR_Trace_Port,
			Unsigned, Unsigned, const char *, int);

/* in trace/mercury_trace.c */
extern	Code	*MR_trace_real(const MR_Stack_Layout_Label *, MR_Trace_Port,
			Unsigned, Unsigned, const char *, int);

/* in library/std_util.h  */
extern	String	ML_type_name(Word);

/*---------------------------------------------------------------------------*/

/*
** mercury__load_aditi_rl_code() is defined in the <module>_init.c file.
** It uploads all the Aditi-RL code for the program to a database to
** which the program currently has a connection, returning a status value
** as described in aditi2/src/api/aditi_err.h in the Aditi sources.
*/
extern	int	mercury__load_aditi_rl_code(void);

/*---------------------------------------------------------------------------*/

#endif /* not MERCURY_INIT_H */

/*---------------------------------------------------------------------------*/
