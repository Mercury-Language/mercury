/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	ACCESS_H
#define	ACCESS_H

#ifndef		LIST_H
#include	"dlist.h"
#endif

extern	void	reset(void);
extern	void	help(void);
extern	Word	get_reg(int);
extern	Word	set_reg(int, Word);
extern	Word	get_mem(Word *);
extern	Word	set_mem(Word *, Word);
extern	Word	createn(List *);
extern	int	getflexline(const char *, FILE *, char **, int *);
#endif
