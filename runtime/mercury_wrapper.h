/*
** Copyright (C) 1994-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_wrapper.h - defines the interface to mercury_wrapper.c.
** See mercury_wrapper.c for documentation.
*/

#ifndef	MERCURY_WRAPPER_H
#define	MERCURY_WRAPPER_H

#include <stddef.h>			/* for `size_t' */
#include "mercury_std.h"		/* for `bool' */
#include "mercury_stack_layout.h"	/* for `MR_Stack_Layout_Label' */
#include "mercury_trace_base.h"		/* for `MR_trace_port' */

/*
** mercury_runtime_init() does some stuff to initialize the garbage collector
** and the Mercury engine's data areas, and then calls io__init_state/2
** in the Mercury library to initialize the io__state.
*/
extern	void	mercury_runtime_init(int argc, char **argv);

/*
** mercury_runtime_main() basically just calls main/2,
** with a bit of debugging scaffolding around it.
*/
extern	void	mercury_runtime_main(void);

/*
** mercury_runtime_terminate() does any necessary cleanup,
** and then returns mercury_exit_status.
*/
extern	int	mercury_runtime_terminate(void);

/*
** The following global variables are set by mercury_init() on startup.
** The entry points are set based on the options to mkinit.c.
** The address_of_foo pointers are set to the address of
** the corresponding foo.
*/
extern	Code 		*program_entry_point; /* normally mercury__main_2_0; */

extern	void		(*MR_library_initializer)(void);
extern	void		(*MR_library_finalizer)(void);

extern	void		(*MR_io_stderr_stream)(Word *);
extern	void		(*MR_io_stdout_stream)(Word *);
extern	void		(*MR_io_stdin_stream)(Word *);
extern	void		(*MR_io_print_to_cur_stream)(Word, Word);
extern	void		(*MR_io_print_to_stream)(Word, Word, Word);

extern	void		(*address_of_mercury_init_io)(void);
extern	void		(*address_of_init_modules)(void);

#ifdef CONSERVATIVE_GC
extern	void		(*address_of_init_gc)(void);
#endif

/*
** Similarly, these are for the debugger interface; they're defined in
** browser/debugger_interface.m.
** XXX These are obsolete; the browser can call the ML_ versions directly.
*/
extern	void	(*MR_DI_output_current_vars)(Word, Word, Word);
		/* output_current_vars/3 */
extern	void	(*MR_DI_output_current_nth_var)(Word, Word);
		/* output_current_nth_var/2 */
extern	void	(*MR_DI_output_current_live_var_names)(Word, Word, Word);
		/* output_current_live_var_names/5 */
extern	void	(*MR_DI_output_current_slots)(Integer, Integer, Integer, Word,
		String, String, Integer, Integer, Integer, String, Word);
		/* output_current_slots/13 */
extern	bool	(*MR_DI_found_match)(Integer, Integer, Integer, Word, String,
		String, Integer, Integer, Integer, Word, String, Word);
		/* found_match/12 */
extern	int	(*MR_DI_get_var_number)(Word);
		/* get_var_number/1 */
extern	void	(*MR_DI_read_request_from_socket)(Word, Word *, Integer *);
		/* read_request_from_socket/5 */

/*
** ML_type_name() is defined in library/std_util.m and used in
** trace/mercury_trace_external.c.
** XXX This is obsolete; the tracer can call the ML_ version directly.
*/

extern	String	(*MR_type_name)(Word);
		/* normally ML_type_name (type_name/1) */ 

/*
** XXX This is obsolete too.
** This variable has been replaced by MR_io_print_to_*_stream,
** but the installed mkinit executable may still generate references to it.
** We must therefore keep it until all obsolete mkinit executables have
** been retired.
*/

extern	Code	*MR_library_trace_browser;

/*
** MR_trace_func_ptr is set to either MR_trace_real (trace/mercury_trace.c), or
** MR_trace_fake (runtime/mercury_trace_base.c),
** depending on whether tracing was enabled when creating the _init.c
** file.  It is called from MR_trace (runtime/mercury_trace_base.c).
*/
extern	Code    *(*MR_trace_func_ptr)(const MR_Stack_Layout_Label *,
			MR_Trace_Port, Unsigned, Unsigned, const char *, int);

extern	void		do_init_modules(void);

extern	const char	*progname;
extern	int		mercury_argc;
extern	char		**mercury_argv;
extern	int		mercury_exit_status;

/* sizes of the data areas, *including* the red zone size */
extern	size_t		heap_size;
extern	size_t		detstack_size;
extern	size_t		nondstack_size;
extern	size_t		solutions_heap_size;
extern	size_t		trail_size;
extern	size_t		global_heap_size;
extern	size_t		debug_heap_size;

/* sizes of the red zones */
extern	size_t		heap_zone_size;
extern	size_t		detstack_zone_size;
extern	size_t		nondstack_zone_size;
extern	size_t		solutions_heap_zone_size;
extern	size_t		trail_zone_size;
extern	size_t		global_heap_zone_size;
extern	size_t		debug_heap_zone_size;

/* size of the primary cache */
extern	size_t		pcache_size;

extern	bool		check_space;

/* timing */
extern	int		time_at_start;
extern	int		time_at_last_stat;

/* time profiling */
enum MR_TimeProfileMethod {
	MR_profile_real_time,			/* i.e. ITIMER_REAL */
	MR_profile_user_time,			/* i.e. ITIMER_VIRTUAL */
	MR_profile_user_plus_system_time	/* i.e. ITIMER_PROF */
};
extern	enum MR_TimeProfileMethod
			MR_time_profile_method;

extern  bool MR_profiling;

#endif /* not MERCURY_WRAPPER_H */
