/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	MEMORY_H
#define	MEMORY_H

#include "regs.h"

/* these cannot be changed without lots of modifications elsewhere */
#define MAX_REAL_REG 32		/* r1 .. r32 */
#define NUM_SPECIAL_REG 5	/* succip, sp, hp, maxfr, curfr */

/* this can be changed at will */
#define MAX_VIRTUAL_REG	1024

/* allocate enough fake_regs to hold both the special regs */
/* and all the virtual registers */
#define MAX_FAKE_REG	(NUM_SPECIAL_REG + MAX_VIRTUAL_REG)
				/* mr0 .. mr36, mr(37) ... mr(1028) */

/* reserve MAX_FAKE_REG virtual regs, numbered from 0 to MAX_FAKE_REG-1 */
extern	Word	fake_reg[MAX_FAKE_REG];

/* used to lookup the fake_reg for a given real reg */
extern	Word	virtual_reg_map[MAX_REAL_REG];

/* used for counting register usage */
extern	Word 	num_uses[MAX_RN];

/* beginning of allocated areas */
extern	Word	*heap;
extern	Word	*detstack;
extern	Word	*nondstack;

/* beginning of used areas */
extern	Word	*heapmin;
extern	Word	*detstackmin;
extern	Word	*nondstackmin;

/* highest locations actually used */
extern	Word	*heapmax;
extern	Word	*detstackmax;
extern	Word	*nondstackmax;

/* end of allocated areas */
extern	Word	*heapend;
extern	Word	*detstackend;
extern	Word	*nondstackend;

/* beginning of redzones */
extern	char *	heap_zone;
extern	char *	detstack_zone;
extern	char *	nondstack_zone;

extern	int	heap_zone_left;
extern	int	detstack_zone_left;
extern	int	nondstack_zone_left;

#ifndef	SPEED
extern	const char	**dumpstack;
extern	int		dumpindex;
#endif

extern	void	init_memory(void);

#endif
