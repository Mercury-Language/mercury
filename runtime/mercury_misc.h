/*
** Copyright (C) 1995-1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_misc.h - debugging messages, fatal_error(), and checked_malloc() */

#ifndef	MERCURY_MISC_H
#define	MERCURY_MISC_H

#include "mercury_types.h"	/* for `Code *' */

#ifndef SPEED

extern	void	mkframe_debug(void);
extern	void	modframe_debug(void);
extern	void	succeed_debug(void);
extern	void	succeeddiscard_debug(void);
extern	void	fail_debug(void);
extern	void	redo_debug(void);
extern	void	call_debug(/* const */ Code *proc, /* const */ Code *succcont);
extern	void	tailcall_debug(/* const */ Code *proc);
extern	void	proceed_debug(void);
extern	void	cr1_debug(Word val0, const Word *addr);
extern	void	cr2_debug(Word val0, Word val1, const Word *addr);
extern	void	incr_hp_debug(Word val, const Word *addr);
extern	void	incr_sp_debug(Word val, const Word *addr);
extern	void	decr_sp_debug(Word val, const Word *addr);
extern	void	push_debug(Word val, const Word *addr);
extern	void	pop_debug(Word val, const Word *addr);
#endif

#if !defined(SPEED) || defined(DEBUG_GOTOS)

extern	void	goto_debug(/* const */ Code *addr);
extern	void	reg_debug(void);

#endif

#ifndef SPEED

extern	void	printint(Word n);
extern	void	printstring(const char *s);
extern	void	printheap(const Word *h);
extern	void	printdetstack(const Word *s);
extern	void	printnondstack(const Word *s);
extern	void	dumpframe(/* const */ Word *);
extern	void	dumpnondstack(void);
extern	void	printlist(Word p);
extern	void	printframe(const char *);
extern	void	printregs(const char *msg);

#endif

extern	void	printlabel(/* const */ Code *w);


#if __GNUC__
	#define NO_RETURN __attribute__((noreturn))
#else
	#define NO_RETURN
#endif
extern	void	fatal_error(const char *msg) NO_RETURN;

/* 
** XXX checked_malloc() should be moved to mercury_memory.h or mercury_heap.h
*/
#include <stddef.h>	/* for size_t */
void *checked_malloc(size_t n);

#endif /* not MERCURY_MISC_H */
