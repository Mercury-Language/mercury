/*
** Copyright (C) 1994-2000 The University of Melbourne.
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
#include "mercury_stack_layout.h"	/* for `MR_Stack_Layout_Label' etc */
#include "mercury_trace_base.h"		/* for `MR_trace_port' */
#include "mercury_stacks.h"		/* for `MR_{Cut,Generator}StackFrame' */

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
** MR_load_aditi_rl_code() uploads all the Aditi-RL code for
** the program to a database to which the program currently has a
** connection, returning a status value as described in
** aditi2/src/api/aditi_err.h in the Aditi sources.
** It aborts if the executable was not compiled for Aditi execution. 
*/
extern	int	MR_load_aditi_rl_code(void);

/*
** MR_init_conservative_GC() initializes the Boehm (et al)
** conservative collector.  For the LLDS back-end, it is normally
** called from mercury_runtime_init(), but for the MLDS
** (--high-level-code) back-end, it may be called directly
** from main().
*/
#ifdef CONSERVATIVE_GC
  extern void	MR_init_conservative_GC(void);
#endif

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

extern	int		(*MR_address_of_do_load_aditi_rl_code)(void);

/*
** MR_trace_getline(const char *, FILE *, FILE *) is defined in
** trace/mercury_trace_internal.c but is called in browser/util.m.  As
** we cannot do direct calls from browser/ to trace/, we do an indirect 
** call via the following pointer.
*/

extern	char *		(*MR_address_of_trace_getline)(const char *,
				FILE *, FILE *);

/*
** MR_trace_init_external() and MR_trace_final_external() are defined 
** in trace/mercury_trace_external.c but are called in
** runtime/mercury_trace_base.c. As we can not do direct calls from
** runtime/ to trace/, we do an indirect call via a function
** pointer MR_address_of_trace_init_external.
*/

extern	void		(*MR_address_of_trace_init_external)(void);
extern	void		(*MR_address_of_trace_final_external)(void);

/*
** XXX This is obsolete too.
** This variable has been replaced by MR_io_print_to_*_stream,
** but the installed mkinit executable may still generate references to it.
** We must therefore keep it until all obsolete mkinit executables have
** been retired.
*/

extern	Code		*MR_library_trace_browser;

/*
** MR_trace_func_ptr is set to either MR_trace_real (trace/mercury_trace.c),
** or MR_trace_fake (runtime/mercury_trace_base.c),
** depending on whether tracing was enabled when creating the _init.c
** file.  It is also temporarily set to MR_trace_interrupt by
** MR_trace_interrupt_handler if tracing was enabled and the
** process receives a SIGINT signal.
** It is called from MR_trace (runtime/mercury_trace_base.c).
**
** Since it is set from a signal handler, it must be declared `volatile'.
*/

extern	Code		*(*volatile MR_trace_func_ptr)(
				const MR_Stack_Layout_Label *);

/*
** If the init file was built with tracing enabled, then
** MR_address_of_trace_interrupt_handler points to
** MR_trace_interrupt_handler, otherwise it is NULL.
*/
extern	void		(*MR_address_of_trace_interrupt_handler)(void);

/*
** If the init file was built with tracing enabled, then
** MR_register_module_layout points to MR_register_module_layout_real,
** otherwise it is NULL.
*/
extern	void		(*MR_register_module_layout)(const MR_Module_Layout *);

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
extern	size_t		generatorstack_size;
extern	size_t		cutstack_size;

/* sizes of the red zones */
extern	size_t		heap_zone_size;
extern	size_t		detstack_zone_size;
extern	size_t		nondstack_zone_size;
extern	size_t		solutions_heap_zone_size;
extern	size_t		trail_zone_size;
extern	size_t		global_heap_zone_size;
extern	size_t		debug_heap_zone_size;
extern	size_t		generatorstack_zone_size;
extern	size_t		cutstack_zone_size;

/* file names for the mdb debugging streams */
extern	const char	*MR_mdb_in_filename;
extern	const char	*MR_mdb_out_filename;
extern	const char	*MR_mdb_err_filename;

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

extern	bool		MR_profiling;

#ifdef  MR_TYPE_CTOR_STATS

typedef	struct MR_TypeStat_Struct	MR_TypeStat;

extern	MR_TypeStat	MR_type_stat_mer_unify;
extern	MR_TypeStat	MR_type_stat_c_unify;
extern	MR_TypeStat	MR_type_stat_mer_index;
extern	MR_TypeStat	MR_type_stat_mer_compare;
extern	MR_TypeStat	MR_type_stat_c_compare;

extern	void		MR_register_type_ctor_stat(MR_TypeStat *type_stat,
				MR_TypeCtorInfo type_ctor_info);

#endif

#endif /* not MERCURY_WRAPPER_H */
