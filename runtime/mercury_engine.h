/*
** Copyright (C) 1994-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_engine.h - definitions for the Mercury runtime engine.
**
** For documentation, see also the comments in mercury_engine.c.
*/

#ifndef	MERCURY_ENGINE_H
#define	MERCURY_ENGINE_H

	/*
	** include mercury_regs.h first so that we don't have
	** any function prototypes before the global register
	** declarations.
	*/
#include "mercury_regs.h"		/* for NUM_REAL_REGS */

#include <setjmp.h>

#include "mercury_std.h"		/* for `bool' */
#include "mercury_types.h"		/* for `Code *' */
#include "mercury_goto.h"		/* for `Define_entry()' */
#include "mercury_thread.h"		/* for pthread types */
#include "mercury_context.h"		/* for MR_Context, MR_IF_USE_TRAIL */

/*---------------------------------------------------------------------------*/

/*
** Global flags that control the behaviour of the Mercury engine(s)
*/

extern	bool	MR_debugflag[];

#define	MR_PROGFLAG		0
#define	MR_GOTOFLAG		1
#define	MR_CALLFLAG		2
#define	MR_HEAPFLAG		3
#define	MR_DETSTACKFLAG		4
#define	MR_NONDSTACKFLAG	5
#define	MR_FINALFLAG		6
#define	MR_MEMFLAG		7
#define	MR_SREGFLAG		8
#define	MR_TRACEFLAG		9
#define	MR_TABLEFLAG		10
#define	MR_DETAILFLAG		11
#define	MR_MAXFLAG		12
/* MR_DETAILFLAG should be the last real flag */

#define	MR_progdebug		MR_debugflag[MR_PROGFLAG]
#define	MR_gotodebug		MR_debugflag[MR_GOTOFLAG]
#define	MR_calldebug		MR_debugflag[MR_CALLFLAG]
#define	MR_heapdebug		MR_debugflag[MR_HEAPFLAG]
#define	MR_detstackdebug	MR_debugflag[MR_DETSTACKFLAG]
#define	MR_nondstackdebug	MR_debugflag[MR_NONDSTACKFLAG]
#define	MR_finaldebug		MR_debugflag[MR_FINALFLAG]
#define	MR_memdebug		MR_debugflag[MR_MEMFLAG]
#define	MR_sregdebug		MR_debugflag[MR_SREGFLAG]
#define	MR_tracedebug		MR_debugflag[MR_TRACEFLAG]
#define	MR_tabledebug		MR_debugflag[MR_TABLEFLAG]
#define	MR_detaildebug		MR_debugflag[MR_DETAILFLAG]

	/* 
	** MR_setjmp and MR_longjmp are wrappers around setjmp and longjmp 
	** to ensure that
	**	 call C -> setjmp -> call Mercury -> call C -> longjmp 
	** works correctly. This is used by the exception handling code for
	** the ODBC interface, and probably shouldn't be used for anything
	** else.
	*/ 

typedef struct {
		jmp_buf *mercury_env;	/* 
					** used to save MR_ENGINE(e_jmp_buf )
					*/
		jmp_buf env;		/* 
					** used by calls to setjmp and longjmp 
					*/
		Word *saved_succip;
		Word *saved_sp;
		Word *saved_curfr;
		Word *saved_maxfr;
		MR_IF_USE_TRAIL(Word *saved_trail_ptr;)
		MR_IF_USE_TRAIL(Word *saved_ticket_counter;)

#if NUM_REAL_REGS > 0
		Word regs[NUM_REAL_REGS];
#endif /* NUM_REAL_REGS > 0 */

	} MR_jmp_buf;

/*---------------------------------------------------------------------------*/

/*
** Replacements for setjmp() and longjmp() that work
** across calls to Mercury code.
*/

	/*
	** MR_setjmp(MR_jmp_buf *env, longjmp_label)
	**
	** Save MR_ENGINE(e_jmp_buf), save the Mercury state, call setjmp(env), 
	** then fall through.
	**
	** When setjmp returns via a call to longjmp, control will pass to
	** longjmp_label.
	**
	** Notes:
	** - The Mercury registers must be valid before the call to MR_setjmp.
	** - The general-purpose registers r1, r2... are not restored and must
	** be saved by the caller.
	** - In grades without conservative garbage collection, the caller
	** must save and restore hp, sol_hp, heap_zone 
	** and solutions_heap_zone.
	*/
#define MR_setjmp(setjmp_env, longjmp_label)				\
	    do {							\
		(setjmp_env)->mercury_env = MR_ENGINE(e_jmp_buf);	\
		save_regs_to_mem((setjmp_env)->regs);			\
		(setjmp_env)->saved_succip = MR_succip;			\
		(setjmp_env)->saved_sp = MR_sp;				\
		(setjmp_env)->saved_curfr = MR_curfr;			\
		(setjmp_env)->saved_maxfr = MR_maxfr;			\
		MR_IF_USE_TRAIL((setjmp_env)->saved_trail_ptr = 	\
				MR_trail_ptr);				\
		MR_IF_USE_TRAIL((setjmp_env)->saved_ticket_counter =	\
				MR_ticket_counter);			\
		if (setjmp((setjmp_env)->env)) {			\
			MR_ENGINE(e_jmp_buf) = (setjmp_env)->mercury_env; \
			restore_regs_from_mem((setjmp_env)->regs);	\
			MR_succip = (setjmp_env)->saved_succip;		\
			MR_sp = (setjmp_env)->saved_sp;			\
			MR_curfr = (setjmp_env)->saved_curfr;		\
			MR_maxfr = (setjmp_env)->saved_maxfr;		\
			MR_IF_USE_TRAIL(MR_trail_ptr = 			\
					(setjmp_env)->saved_trail_ptr);	\
			MR_IF_USE_TRAIL(MR_ticket_counter = 		\
				(setjmp_env)->saved_ticket_counter);	\
			goto longjmp_label;				\
		}							\
	    } while (0)

	/*
	** MR_longjmp(MR_jmp_buf *env)
	** 
	** Call longjmp(), MR_setjmp() will handle the rest.
	*/
#define MR_longjmp(setjmp_env)	longjmp((setjmp_env)->env, 1)

/*---------------------------------------------------------------------------*/

#ifdef	MR_THREAD_SAFE
typedef struct MR_mercury_thread_list_struct {
	MercuryThread			thread;
	struct MR_mercury_thread_list_struct	*next;
} MercuryThreadList;
#endif

/*
** The Mercury engine structure.
**	Normally there is one of these for each Posix thread.
*/

typedef struct MR_mercury_engine_struct {
	Word		fake_reg[MAX_FAKE_REG];
		/* The fake reg vector for this engine. */
#ifndef CONSERVATIVE_GC
	Word		*e_hp;
		/* The heap pointer for this engine */
	Word		*e_sol_hp;
		/* The solutions heap pointer for this engine */
	Word		*e_global_hp;
		/* The global heap pointer for this engine */
#endif
	MR_Context	*this_context;
		/*
		** this_context points to the context currently
		** executing in this engine.
		*/
	MR_Context	context;
		/*
		** context stores all the context information
		** for the context executing in this engine.
		*/
#ifdef	MR_THREAD_SAFE
	MercuryThread	owner_thread;
	unsigned	c_depth;
	MercuryThreadList *saved_owners;
		/*
		** These three fields are used to ensure that when a
		** thread executing C code calls the Mercury engine
		** associated with that thread, the Mercury code
		** will finish in the same engine and return appropriately.
		** Each time C calls Mercury in a thread, the c_depth
		** is incremented, and the owner_thread field of the current
		** context is set to the id of the thread. While the
		** owner_thread is set, the context will not be scheduled
		** for execution by any other thread. When the call to
		** the Mercury engine finishes, c_depth is decremented and
		** the owner_thread field of the current context is restored
		** to its previous value.
		** The list `saved_owners' is used in call_engine_inner
		** to store the owner of a context across calls into Mercury.
		** At the moment this is only used for sanity checking - that
		** execution never returns into C in the wrong thread.
		*/
#endif
	jmp_buf		*e_jmp_buf;
#ifndef	CONSERVATIVE_GC
	MemoryZone	*heap_zone;
	MemoryZone	*solutions_heap_zone;
	MemoryZone	*global_heap_zone;
#endif
#ifdef	NATIVE_GC
	MemoryZone	*heap_zone2;
  #ifdef MR_DEBUG_AGC_PRINT_VARS
	MemoryZone	*debug_heap_zone;
  #endif
#endif
#ifdef	MR_LOWLEVEL_DEBUG
	MemoryZone	*dumpstack_zone;
	int		dumpindex;
#endif
} MercuryEngine;

/*
** MR_engine_base refers to the engine in which execution is taking place.
** In the non-thread-safe situation, it is just a global variable.
** In the thread-safe situation, MR_engine_base is either a global
** register (if one is available), or a macro that accesses thread-local
** storage. We provide two macros, MR_ENGINE(x) and MR_CONTEXT(x),
** that can be used in both kinds of situations to refer to fields
** of the engine structure, and to fields of the engine's current context.
*/

#ifdef	MR_THREAD_SAFE

  extern MercuryThreadKey	MR_engine_base_key;

  #define MR_thread_engine_base \
	((MercuryEngine *) MR_GETSPECIFIC(MR_engine_base_key))

  #if NUM_REAL_REGS > 0
    #define	MR_ENGINE_BASE_REGISTER
  	/*
	** MR_engine_base is defined in machdeps/{arch}.h
	*/
  #else
	#define	MR_engine_base	MR_thread_engine_base
  #endif

  #define MR_ENGINE(x)		(((MercuryEngine *)MR_engine_base)->x)
  #define MR_get_engine()	MR_thread_engine_base

#else 	/* !MR_THREAD_SAFE */

  extern MercuryEngine	MR_engine_base;
  #define MR_ENGINE(x)		(MR_engine_base.x)
  #define MR_get_engine()	(&MR_engine_base)

#endif	/* !MR_THREAD_SAFE */

#define	MR_CONTEXT(x)		(MR_ENGINE(context).x)

#ifndef CONSERVATIVE_GC
  #define IF_NOT_CONSERVATIVE_GC(x)	x
#else
  #define IF_NOT_CONSERVATIVE_GC(x)
#endif

#define load_engine_regs(eng)						\
  	do {								\
		IF_NOT_CONSERVATIVE_GC(MR_hp = (eng)->e_hp;)		\
		IF_NOT_CONSERVATIVE_GC(MR_sol_hp = (eng)->e_sol_hp;)	\
		IF_NOT_CONSERVATIVE_GC(MR_global_hp = (eng)->e_global_hp;) \
	} while (0)

#define save_engine_regs(eng)						\
  	do {								\
		IF_NOT_CONSERVATIVE_GC((eng)->e_hp = MR_hp;)		\
		IF_NOT_CONSERVATIVE_GC((eng)->e_sol_hp = MR_sol_hp;)	\
		IF_NOT_CONSERVATIVE_GC((eng)->e_global_hp = MR_global_hp;) \
	} while (0)

/*
** Macros for easy access to heap zones
*/
#ifndef	CONSERVATIVE_GC
  #define MR_heap_zone			MR_ENGINE(heap_zone)
  #define MR_solutions_heap_zone	MR_ENGINE(solutions_heap_zone)
  #define MR_global_heap_zone		MR_ENGINE(global_heap_zone)
#endif

/*
** Functions for creating/destroying a MercuryEngine.
*/
extern	MercuryEngine	*create_engine(void);
extern	void		destroy_engine(MercuryEngine *engine);

/*
** Functions for initializing/finalizing a MercuryEngine.
** These are like create/destroy except that they don't allocate/deallocate
** the MercuryEngine structure.
*/
extern	void	init_engine(MercuryEngine *engine);
extern	void	finalize_engine(MercuryEngine *engine);

/*
** Functions that act on the current Mercury engine.
*/
extern	void	call_engine(Code *entry_point);
extern	void	terminate_engine(void);
extern	void	dump_prev_locations(void);

/*---------------------------------------------------------------------------*/

/*
** Builtin labels that point to commonly used code fragments
*/

Declare_entry(do_redo);
Declare_entry(do_fail);
Declare_entry(do_reset_hp_fail);
Declare_entry(do_reset_framevar0_fail);
Declare_entry(do_succeed);
Declare_entry(do_not_reached);

#endif /* not MERCURY_ENGINE_H */
