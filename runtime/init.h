#ifndef	INIT_H
#define	INIT_H

#include "imp.h"

#ifdef CONSERVATIVE_GC
extern	void		init_gc(void);
#endif

extern	void		do_init_modules(void);
extern	void		init_modules(void);
extern	Code		*default_entry;

#endif
