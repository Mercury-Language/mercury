/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	INIT_H
#define	INIT_H

#include "imp.h"

#ifdef CONSERVATIVE_GC
extern	void		init_gc(void);
#endif

#ifdef NATIVE_GC
extern  void		gc_continuation_table_init(void);
#endif

extern	void		do_init_modules(void);
extern	void		init_modules(void);
extern	Code		*entry_point;

#endif
