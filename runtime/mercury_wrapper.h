/*
** Copyright (C) 1994-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_wrapper.h - defines the interface to wrapper.c.
** See wrapper.c for documentation.
*/

#ifndef	MERCURY_WRAPPER_H
#define	MERCURY_WRAPPER_H

#include <stddef.h>	/* for `size_t' */
#include "mercury_std.h"	/* for `bool' */

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
extern	Code *		program_entry_point; /* normally mercury__main_2_0; */

extern	void		(*MR_library_initializer)(void);
extern	void		(*MR_library_finalizer)(void);
extern	Code		*MR_library_trace_browser;

extern	void		(*address_of_mercury_init_io)(void);
extern	void		(*address_of_init_modules)(void);

#ifdef CONSERVATIVE_GC
extern	void		(*address_of_init_gc)(void);
#endif

/*
** Similarly, these are for the debugger interface; they're defined in
** library/debugger_interface.m.
*/
void	(*MR_DI_output_current_vars)(Word, Word, Word);
		/* normally ML_DI_output_current_vars (output_current_vars/3) */
void	(*MR_DI_output_current_slots)(Integer, Integer, Integer, Word, String,
		String, Integer, Integer, Integer, String, Word);
		/* normally ML_DI_output_current_slots (output_current_slots/13) */
bool	(*MR_DI_found_match)(Integer, Integer, Integer, Word, String, String,
		Integer, Integer, Integer, Word, String, Word);
		/* normally ML_DI_found_match (found_match/12) */
void	(*MR_DI_read_request_from_socket)(Word, Word *, Integer *);
		/* normally ML_DI_read_request_from_socket
		   (read_request_from_socket/5) */

extern	void		do_init_modules(void);

extern	const char *	progname;
extern	int		mercury_argc;
extern	char **		mercury_argv;
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
