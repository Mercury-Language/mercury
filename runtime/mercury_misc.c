/*
** Copyright (C) 1996-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include	"mercury_imp.h"
#include	"mercury_dlist.h"
#include	"mercury_regs.h"
#include	"mercury_misc.h"

#include	<stdio.h>

/*--------------------------------------------------------------------*/

static void print_ordinary_regs(void);

/* debugging messages */

#ifdef MR_LOWLEVEL_DEBUG

void 
mkframe_msg(void)
{
	restore_transient_registers();

	printf("\nnew choice point for procedure %s\n", curprednm);
	printf("new  fr: "); printnondstack(curfr);
	printf("prev fr: "); printnondstack(curprevfr);
	printf("succ fr: "); printnondstack(cursuccfr);
	printf("succ ip: "); printlabel(cursuccip);
	printf("redo ip: "); printlabel(curredoip);

	if (detaildebug) {
		dumpnondstack();
	}

	return;
}

void 
modframe_msg(void)
{
	restore_transient_registers();

	printf("\nmodifying choice point for procedure %s\n", curprednm);
	printf("redo ip: "); printlabel(curredoip);

	if (detaildebug) {
		dumpnondstack();
	}

	return;
}

void 
succeed_msg(void)
{
	restore_transient_registers();

	printf("\nsucceeding from procedure %s\n", curprednm);
	printf("curr fr: "); printnondstack(curfr);
	printf("succ fr: "); printnondstack(cursuccfr);
	printf("succ ip: "); printlabel(cursuccip);

	if (detaildebug) {
		printregs("registers at success");
	}
	
	return;
}

void 
succeeddiscard_msg(void)
{
	restore_transient_registers();

	printf("\nsucceeding from procedure %s, discarding frame\n", curprednm);
	printf("curr fr: "); printnondstack(curfr);
	printf("succ fr: "); printnondstack(cursuccfr);
	printf("succ ip: "); printlabel(cursuccip);

	if (detaildebug) {
		printregs("registers at success");
	}

	return;
}

void 
fail_msg(void)
{
	restore_transient_registers();

	printf("\nfailing from procedure %s\n", curprednm);
	printf("curr fr: "); printnondstack(curfr);
	printf("fail fr: "); printnondstack(curprevfr);
	printf("fail ip: "); printlabel(bt_redoip(curprevfr));

	return;
}

void 
redo_msg(void)
{
	restore_transient_registers();

	printf("\nredo from procedure %s\n", curprednm);
	printf("curr fr: "); printnondstack(curfr);
	printf("redo fr: "); printnondstack(maxfr);
	printf("redo ip: "); printlabel(bt_redoip(maxfr));

	return;
}

void 
call_msg(/* const */ Code *proc, /* const */ Code *succcont)
{
	printf("\ncalling      "); printlabel(proc);
	printf("continuation "); printlabel(succcont);
	printregs("registers at call");

	return;
}

void 
tailcall_msg(/* const */ Code *proc)
{
	restore_transient_registers();

	printf("\ntail calling "); printlabel(proc);
	printf("continuation "); printlabel(succip);
	printregs("registers at tailcall");

	return;
}

void 
proceed_msg(void)
{
	printf("\nreturning from determinate procedure\n");
	printregs("registers at proceed");

	return;
}

void 
cr1_msg(Word val0, const Word *addr)
{
	printf("put value %9lx at ", (long) (Integer) val0);
	printheap(addr);

	return;
}

void 
cr2_msg(Word val0, Word val1, const Word *addr)
{
	printf("put values %9lx,%9lx at ",	
		(long) (Integer) val0, (long) (Integer) val1);
	printheap(addr);

	return;
}

void 
incr_hp_debug_msg(Word val, const Word *addr)
{
#ifdef CONSERVATIVE_GC
	printf("allocated %ld words at 0x%p\n", (long) (Integer) val, addr);
#else
	printf("increment hp by %ld from ", (long) (Integer) val);
	printheap(addr);
#endif
	return;
}

void 
incr_sp_msg(Word val, const Word *addr)
{
	printf("increment sp by %ld from ", (long) (Integer) val);
	printdetstack(addr);

	return;
}

void 
decr_sp_msg(Word val, const Word *addr)
{
	printf("decrement sp by %ld from ", (long) (Integer) val);
	printdetstack(addr);

	return;
}

void 
push_msg(Word val, const Word *addr)
{
	printf("push value %9lx to ", (long) (Integer) val);
	printdetstack(addr);

	return;
}

void 
pop_msg(Word val, const Word *addr)
{
	printf("pop value %9lx from ", (long) (Integer) val);
	printdetstack(addr);

	return;
}

#endif /* defined(MR_LOWLEVEL_DEBUG) */

#if defined(MR_DEBUG_GOTOS)

void 
goto_msg(/* const */ Code *addr)
{
	printf("\ngoto ");
	printlabel(addr);

	if (detaildebug) {
		printregs("registers at goto");
	}

	return;
}

void 
reg_msg(void)
{
	int	i;
	Integer	x;

	for(i=1; i<=8; i++) {
		x = (Integer) get_reg(i);
#ifndef CONSERVATIVE_GC
		if ( (Integer) heap_zone->min <= x
				&& x < (Integer) heap_zone->top) {
			x -= (Integer) heap_zone->min;
		}
#endif
		printf("%8lx ", (long) x);
	}
	printf("\n");

	return;
}

#endif /* defined(MR_DEBUG_GOTOS) */

/*--------------------------------------------------------------------*/

#if defined(MR_DEBUG_GOTOS)

/* debugging printing tools */

void 
printint(Word n)
{
	printf("int %ld\n", (long) (Integer) n);

	return;
}

void 
printstring(const char *s)
{
	printf("string 0x%p %s\n", (const void *) s, s);

	return;
}

void 
printheap(const Word *h)
{
#ifndef CONSERVATIVE_GC
	printf("ptr 0x%p, offset %3ld words\n",
		(const void *) h, (long) (Integer) (h - heap_zone->min));
#else
	printf("ptr 0x%p\n",
		(const void *) h);
#endif
	return;
}

void 
printdetstack(const Word *s)
{
	printf("ptr 0x%p, offset %3ld words\n",
		(const void *) s, (long) (Integer) (s - detstack_zone->min));
	return;
}

void 
printnondstack(const Word *s)
{
#ifndef	MR_DEBUG_NONDET_STACK
	printf("ptr 0x%p, offset %3ld words\n",
		(const void *) s, (long) (Integer) (s - nondetstack_zone->min));
#else
	if (s > nondetstack_zone->min) {
		printf("ptr 0x%p, offset %3ld words, procedure %s\n",
			(const void *) s, 
			(long) (Integer) (s - nondetstack_zone->min),
			(const char *) s[PREDNM]);
	} else {
		/*
		** This handles the case where the prevfr of the first frame
		** is being printed.
		*/
		printf("ptr 0x%p, offset %3ld words\n",
			(const void *) s, 
			(long) (Integer) (s - nondetstack_zone->min));
	}
#endif
	return;
}

void 
dumpframe(/* const */ Word *fr)
{
	reg	int	i;

	printf("frame at ptr 0x%p, offset %3ld words\n",
		(const void *) fr, 
		(long) (Integer) (fr - nondetstack_zone->min));
#ifdef	MR_DEBUG_NONDET_STACK
	printf("\t predname  %s\n", bt_prednm(fr));
#endif
	printf("\t succip    "); printlabel(bt_succip(fr));
	printf("\t redoip    "); printlabel(bt_redoip(fr));
	printf("\t succfr    "); printnondstack(bt_succfr(fr));
	printf("\t prevfr    "); printnondstack(bt_prevfr(fr));

	for (i = 0; &bt_var(fr,i) > bt_prevfr(fr); i++) {
		printf("\t framevar(%d)  %ld 0x%lx\n",
			i, (long) (Integer) bt_var(fr,i),
			(unsigned long) bt_var(fr,i));
	}
	return;
}

void 
dumpnondstack(void)
{
	reg	Word	*fr;

	printf("\nnondstack dump\n");
	for (fr = maxfr; fr > nondetstack_zone->min; fr = bt_prevfr(fr)) {
		dumpframe(fr);
	}
	return;
}

void 
printframe(const char *msg)
{
	printf("\n%s\n", msg);
	dumpframe(curfr);

	print_ordinary_regs();
	return;
}

void 
printregs(const char *msg)
{
	restore_transient_registers();

	printf("\n%s\n", msg);

	printf("%-9s", "succip:");  printlabel(succip);
	printf("%-9s", "curfr:");   printnondstack(curfr);
	printf("%-9s", "maxfr:");   printnondstack(maxfr);
	printf("%-9s", "hp:");      printheap(hp);
	printf("%-9s", "sp:");      printdetstack(sp);

	print_ordinary_regs();

	return;
}

static void 
print_ordinary_regs(void)
{
	int	i;
	Integer	value;

	for (i = 0; i < 8; i++) {
		printf("r%d:      ", i + 1);
		value = (Integer) get_reg(i+1);

#ifndef	CONSERVATIVE_GC
		if ((Integer) heap_zone->min <= value
				&& value < (Integer) heap_zone->top) {
			printf("(heap) ");
		}
#endif

		printf("%ld\n", (long) value);
	}
}

#endif /* defined(MR_DEBUG_GOTOS) */

void 
printlabel(/* const */ Code *w)
{
	Label	*label;

	label = lookup_label_addr(w);
	if (label != NULL) {
		printf("label %s (0x%p)\n", label->e_name, w);
	} else {
		printf("label UNKNOWN (0x%p)\n", w);
	}
}


void *
newmem(size_t n)
{
	reg	void	*p;

#ifdef CONSERVATIVE_GC
	p = GC_MALLOC(n);
#else
	p = malloc(n);
#endif
	if (p == NULL && n != 0) {
		fatal_error("ran out of memory");
	}

	return p;
}

void 
oldmem(void *p)
{
#ifdef CONSERVATIVE_GC
	GC_FREE(p);
#else
	free(p);
#endif
}

void* 
resizemem(void *p, size_t size)
{
#ifdef CONSERVATIVE_GC
	p = GC_REALLOC(p, size);
#else
	p = realloc(p, size);
#endif
	if (p == NULL) {
		fatal_error("ran out of memory");
	}

	return p;
}

/* XXX will need to modify this to kill other processes if PARALLEL
 * (and cleanup resources, etc....)
 */
void 
fatal_error(const char *message) {
	fprintf(stderr, "Mercury runtime: %s\n", message);
	exit(1);
}


/*
**  Note that hash_string is actually defined as a macro in mercury_imp.h,
**  if we're using GNU C.  We define it here whether or not we're using
**  gcc, so that users can easily switch between gcc and cc without
**  rebuilding the libraries.
*/

#undef hash_string

int 
hash_string(Word s)
{
	HASH_STRING_FUNC_BODY
}
