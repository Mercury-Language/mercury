/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* access.h - some unfinished junk that should probably be delted */

#ifndef	ACCESS_H
#define	ACCESS_H

#include <stdio.h>		/* for `FILE' */
#include "mercury_types.h"	/* for `Word' */
#include "dlist.h"		/* for `List' */

extern	void	reset(void);
extern	void	help(void);
extern	Word	get_reg(int);
extern	Word	set_reg(int, Word);
extern	Word	get_mem(Word *);
extern	Word	set_mem(Word *, Word);
extern	Word	createn(List *);
extern	int	getflexline(const char *, FILE *, char **, int *);

#endif
