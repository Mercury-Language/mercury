/*
** Copyright (C) 1995-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_misc.h -	debugging messages,
**			MR_warning(),
**			fatal_error(),
**			MR_memcpy
**			MR_fd_zero
*/

#ifndef	MERCURY_MISC_H
#define	MERCURY_MISC_H

#include "mercury_types.h"	/* for `Code *' */
#include <stdlib.h>		/* for `size_t' */
#include <stdio.h>		/* for `FILE' */

#ifdef MR_LOWLEVEL_DEBUG
extern	void	mkframe_msg(const char *);
extern	void	succeed_msg(void);
extern	void	succeeddiscard_msg(void);
extern	void	fail_msg(void);
extern	void	redo_msg(void);
extern	void	call_msg(/* const */ Code *proc, /* const */ Code *succcont);
extern	void	tailcall_msg(/* const */ Code *proc);
extern	void	proceed_msg(void);
extern	void	cr1_msg(Word val0, const Word *addr);
extern	void	cr2_msg(Word val0, Word val1, const Word *addr);
extern	void	incr_hp_debug_msg(Word val, const Word *addr);
extern	void	incr_sp_msg(Word val, const Word *addr);
extern	void	decr_sp_msg(Word val, const Word *addr);
extern	void	push_msg(Word val, const Word *addr);
extern	void	pop_msg(Word val, const Word *addr);
#endif

#ifdef MR_DEBUG_GOTOS
extern	void	goto_msg(/* const */ Code *addr);
extern	void	reg_msg(void);
#endif

#ifdef MR_LOWLEVEL_DEBUG
extern	void	printint(Word n);
extern	void	printstring(const char *s);
extern	void	printheap(const Word *h);
extern	void	dumpframe(/* const */ Word *);
extern	void	dumpnondstack(void);
extern	void	printlist(Word p);
extern	void	printframe(const char *);
extern	void	printregs(const char *msg);
#endif

extern	void	printdetstack(const Word *s);
extern	void	MR_printdetstackptr(const Word *s);
extern	void	MR_print_detstackptr(FILE *fp, const Word *s);
extern	void	printnondstack(const Word *s);
extern	void	MR_printnondstackptr(const Word *s);
extern	void	MR_print_nondstackptr(FILE *fp, const Word *s);
extern	void	MR_print_heapptr(FILE *fp, const Word *s);
extern	void	MR_print_label(FILE *fp, /* const */ Code *w);
extern	void	printlabel(/* const */ Code *w);

#if __GNUC__
	#define NO_RETURN __attribute__((noreturn))
#else
	#define NO_RETURN
#endif
extern	void	MR_warning(const char *msg, ...);
extern	void	fatal_error(const char *msg, ...) NO_RETURN;

/*
** We use our own version of memcpy because gcc recognises calls to the
** standard memcpy (even in things that do not mention memcpy by name, e.g.
** structure assignments) and generates inline code for them. Unfortunately
** this causes gcc to abort because it tries to use a register that we have
** already reserved.
** XXX We should fix this eventually by using -fno-builtin since pragma
** c_code may call the builtin functions.
*/
extern	void	MR_memcpy(void *dest, const void *src, size_t nbytes);

#endif /* not MERCURY_MISC_H */
