/*
** Copyright (C) 1995-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_misc.h -	debugging messages,
**			fatal_error(),
**			checked_malloc(),
**			MR_memcpy
*/

#ifndef	MERCURY_MISC_H
#define	MERCURY_MISC_H

#include "mercury_types.h"	/* for `Code *' */

#ifdef MR_LOWLEVEL_DEBUG
extern	void	mkframe_msg(void);
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
** We use our own version of memcpy because gcc recognises calls to the
** standard memcpy and generates inline code for them. Unfortunately this
** causes it to abort because it tries to use a register that we're already
** reserved.
** XXX We should fix this eventually by using -fno-builtin since pragma
** c_code may call the builtin functions.
*/
extern	void	MR_memcpy(char *dest, const char *src, size_t nbytes);

#endif /* not MERCURY_MISC_H */
