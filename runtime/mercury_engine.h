/*
** Copyright (C) 1994-1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** engine.h - definitions for the Mercury runtime engine.
**
** For documentation, see the comments in engine.mod.
*/

#ifndef	ENGINE_H
#define	ENGINE_H

#include <setjmp.h>

#include "std.h"		/* for `bool' */
#include "mercury_types.h"	/* for `Code *' */
#include "goto.h"		/* for `Define_entry()' */
#include "regs.h"		/* for NUM_REAL_REGS */

#define	PROGFLAG	0
#define	GOTOFLAG	1
#define	CALLFLAG	2
#define	HEAPFLAG	3
#define	DETSTACKFLAG	4
#define	NONDSTACKFLAG	5
#define	FINALFLAG	6
#define	MEMFLAG		7
#define	SREGFLAG	8
#define	TRACEFLAG	9
#define	DETAILFLAG	10
#define	MAXFLAG		11
/* DETAILFLAG should be the last real flag */

#define	progdebug	debugflag[PROGFLAG]
#define	gotodebug	debugflag[GOTOFLAG]
#define	calldebug	debugflag[CALLFLAG]
#define	heapdebug	debugflag[HEAPFLAG]
#define	detstackdebug	debugflag[DETSTACKFLAG]
#define	nondstackdebug	debugflag[NONDSTACKFLAG]
#define	finaldebug	debugflag[FINALFLAG]
#define	memdebug	debugflag[MEMFLAG]
#define	sregdebug	debugflag[SREGFLAG]
#define	tracedebug	debugflag[TRACEFLAG]
#define	detaildebug	debugflag[DETAILFLAG]

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
					** used to save MR_engine_jmp_buf 
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

	/*
	** MR_setjmp(MR_jmp_buf *env, longjmp_label)
	**
	** Save MR_engine_jmp_buf, save the Mercury state, call setjmp(env), 
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
		(setjmp_env)->mercury_env = MR_engine_jmp_buf;		\
		save_regs_to_mem((setjmp_env)->regs);			\
		(setjmp_env)->saved_succip = succip;			\
		(setjmp_env)->saved_sp = sp;				\
		(setjmp_env)->saved_curfr = curfr;			\
		(setjmp_env)->saved_maxfr = maxfr;			\
		MR_IF_USE_TRAIL((setjmp_env)->saved_trail_ptr = 	\
				MR_trail_ptr);				\
		MR_IF_USE_TRAIL((setjmp_env)->saved_ticket_counter =	\
				MR_ticket_counter);			\
		if (setjmp((setjmp_env)->env)) {			\
			MR_engine_jmp_buf = (setjmp_env)->mercury_env;	\
			restore_regs_from_mem((setjmp_env)->regs);	\
			succip = (setjmp_env)->saved_succip;		\
			sp = (setjmp_env)->saved_sp;			\
			curfr = (setjmp_env)->saved_curfr;		\
			maxfr = (setjmp_env)->saved_maxfr;		\
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

	/* 
	** engine_jmp_buf should only be referred to in engine.c
	** and the MR_setjmp and MR_longjmp macros defined above.
	*/
extern	jmp_buf *MR_engine_jmp_buf;

extern	bool	debugflag[];

extern	void	init_engine(void);
extern	void	call_engine(Code *entry_point);
extern	void	terminate_engine(void);
extern	void	dump_prev_locations(void);

Declare_entry(do_redo);
Declare_entry(do_fail);
Declare_entry(do_reset_hp_fail);
Declare_entry(do_reset_framevar0_fail);
Declare_entry(do_succeed);
Declare_entry(do_not_reached);

#endif /* not ENGINE_H */
