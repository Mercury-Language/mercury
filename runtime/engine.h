/*
** Copyright (C) 1995-1997 University of Melbourne.
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

#include "std.h"		/* for `bool' */
#include "mercury_types.h"	/* for `Code *' */
#include "goto.h"		/* for `Define_entry()' */

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

extern	bool	debugflag[];

extern	void	init_engine(void);
extern	void	start_mercury_engine(Code *entry_point);
extern	void	call_engine(Code *entry_point);
extern	void	dump_prev_locations(void);

Declare_entry(do_redo);
Declare_entry(do_fail);
Declare_entry(do_reset_hp_fail);
Declare_entry(do_reset_framevar0_fail);
Declare_entry(do_succeed);
Declare_entry(do_not_reached);

#endif
