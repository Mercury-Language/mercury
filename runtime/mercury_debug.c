/*
** Copyright (C) 1996-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include	"mercury_imp.h"
#include	"mercury_dlist.h"
#include	"mercury_regs.h"
#include	"mercury_trace_base.h"
#include	"mercury_label.h"
#include	"mercury_debug.h"

#include	<stdio.h>
#include	<stdarg.h>

/*--------------------------------------------------------------------*/

static void	print_ordinary_regs(void);
static void	MR_printdetslot_as_label(const MR_Integer offset);

/* debugging messages */

#ifdef MR_LOWLEVEL_DEBUG

void 
mkframe_msg(const char *predname)
{
	restore_transient_registers();

	printf("\nnew choice point for procedure %s\n", predname);
	printf("new  fr: "); printnondstack(MR_curfr);
	printf("prev fr: "); printnondstack(MR_prevfr_slot(MR_curfr));
	printf("succ fr: "); printnondstack(MR_succfr_slot(MR_curfr));
	printf("succ ip: "); printlabel(MR_succip_slot(MR_curfr));
	printf("redo ip: "); printlabel(MR_redoip_slot(MR_curfr));

	if (MR_detaildebug) {
		dumpnondstack();
	}
}

void 
succeed_msg(void)
{
	restore_transient_registers();

	printf("\nsucceeding from procedure\n");
	printf("curr fr: "); printnondstack(MR_curfr);
	printf("succ fr: "); printnondstack(MR_succfr_slot(MR_curfr));
	printf("succ ip: "); printlabel(MR_succip_slot(MR_curfr));

	if (MR_detaildebug) {
		printregs("registers at success");
	}
}

void 
succeeddiscard_msg(void)
{
	restore_transient_registers();

	printf("\nsucceeding from procedure\n");
	printf("curr fr: "); printnondstack(MR_curfr);
	printf("succ fr: "); printnondstack(MR_succfr_slot(MR_curfr));
	printf("succ ip: "); printlabel(MR_succip_slot(MR_curfr));

	if (MR_detaildebug) {
		printregs("registers at success");
	}
}

void 
fail_msg(void)
{
	restore_transient_registers();

	printf("\nfailing from procedure\n");
	printf("curr fr: "); printnondstack(MR_curfr);
	printf("fail fr: "); printnondstack(MR_prevfr_slot(MR_curfr));
	printf("fail ip: "); printlabel(MR_redoip_slot(MR_prevfr_slot(MR_curfr)));
}

void 
redo_msg(void)
{
	restore_transient_registers();

	printf("\nredo from procedure\n");
	printf("curr fr: "); printnondstack(MR_curfr);
	printf("redo fr: "); printnondstack(MR_maxfr);
	printf("redo ip: "); printlabel(MR_redoip_slot(MR_maxfr));
}

void 
call_msg(/* const */ MR_Code *proc, /* const */ MR_Code *succcont)
{
	printf("\ncalling      "); printlabel(proc);
	printf("continuation "); printlabel(succcont);
	printregs("registers at call");
}

void 
tailcall_msg(/* const */ MR_Code *proc)
{
	restore_transient_registers();

	printf("\ntail calling "); printlabel(proc);
	printf("continuation "); printlabel(MR_succip);
	printregs("registers at tailcall");
}

void 
proceed_msg(void)
{
	printf("\nreturning from determinate procedure\n");
	printregs("registers at proceed");
}

void 
cr1_msg(MR_Word val0, const MR_Word *addr)
{
	printf("put value %9lx at ", (long) (MR_Integer) val0);
	printheap(addr);
}

void 
cr2_msg(MR_Word val0, MR_Word val1, const MR_Word *addr)
{
	printf("put values %9lx,%9lx at ",	
		(long) (MR_Integer) val0, (long) (MR_Integer) val1);
	printheap(addr);
}

void 
incr_hp_debug_msg(MR_Word val, const MR_Word *addr)
{
#ifdef CONSERVATIVE_GC
	printf("allocated %ld words at %p\n", (long) val, addr);
#else
	printf("increment hp by %ld from ", (long) (MR_Integer) val);
	printheap(addr);
#endif
}

void 
incr_sp_msg(MR_Word val, const MR_Word *addr)
{
	printf("increment sp by %ld from ", (long) (MR_Integer) val);
	printdetstack(addr);
}

void 
decr_sp_msg(MR_Word val, const MR_Word *addr)
{
	printf("decrement sp by %ld from ", (long) (MR_Integer) val);
	printdetstack(addr);
}

void 
push_msg(MR_Word val, const MR_Word *addr)
{
	printf("push value %9lx to ", (long) (MR_Integer) val);
	printdetstack(addr);
}

void 
pop_msg(MR_Word val, const MR_Word *addr)
{
	printf("pop value %9lx from ", (long) (MR_Integer) val);
	printdetstack(addr);
}

#endif /* defined(MR_LOWLEVEL_DEBUG) */

#ifdef MR_DEBUG_GOTOS

void 
goto_msg(/* const */ MR_Code *addr)
{
	printf("\ngoto ");
	printlabel(addr);
}

void 
reg_msg(void)
{
	int	i;
	MR_Integer	x;

	for(i=1; i<=8; i++) {
		x = (MR_Integer) get_reg(i);
#ifndef CONSERVATIVE_GC
		if ((MR_Integer) MR_ENGINE(heap_zone)->min <= x
				&& x < (MR_Integer) MR_ENGINE(heap_zone)->top) {
			x -= (MR_Integer) MR_ENGINE(heap_zone)->min;
		}
#endif
		printf("%8lx ", (long) x);
	}
	printf("\n");
}

#endif /* defined(MR_DEBUG_GOTOS) */

/*--------------------------------------------------------------------*/

#ifdef MR_LOWLEVEL_DEBUG

/* debugging printing tools */

void 
printint(MR_Word n)
{
	printf("int %ld\n", (long) (MR_Integer) n);
}

void 
printstring(const char *s)
{
	printf("string %p %s\n", (const void *) s, s);
}

void 
printheap(const MR_Word *h)
{
#ifndef CONSERVATIVE_GC
	printf("ptr %p, offset %3ld words\n",
		(const void *) h,
		(long) (MR_Integer) (h - MR_ENGINE(heap_zone)->min));
#else
	printf("ptr %p\n",
		(const void *) h);
#endif
}

void 
dumpframe(/* const */ MR_Word *fr)
{
	reg	int	i;

	printf("frame at ptr %p, offset %3ld words\n",
		(const void *) fr, 
		(long) (MR_Integer) (fr - MR_CONTEXT(nondetstack_zone)->min));
	printf("\t succip    "); printlabel(MR_succip_slot(fr));
	printf("\t redoip    "); printlabel(MR_redoip_slot(fr));
	printf("\t succfr    "); printnondstack(MR_succfr_slot(fr));
	printf("\t prevfr    "); printnondstack(MR_prevfr_slot(fr));

	for (i = 1; &MR_based_framevar(fr,i) > MR_prevfr_slot(fr); i++) {
		printf("\t framevar(%d)  %ld 0x%lx\n",
			i, (long) (MR_Integer) MR_based_framevar(fr,i),
			(unsigned long) MR_based_framevar(fr,i));
	}
}

void 
dumpnondstack(void)
{
	reg	MR_Word	*fr;

	printf("\nnondstack dump\n");
	for (fr = MR_maxfr; fr > MR_CONTEXT(nondetstack_zone)->min;
			fr = MR_prevfr_slot(fr)) {
		dumpframe(fr);
	}
}

void 
printframe(const char *msg)
{
	printf("\n%s\n", msg);
	dumpframe(MR_curfr);

	print_ordinary_regs();
}

void 
printregs(const char *msg)
{
	restore_transient_registers();

	printf("\n%s\n", msg);

	printf("%-9s", "succip:");  printlabel(MR_succip);
	printf("%-9s", "curfr:");   printnondstack(MR_curfr);
	printf("%-9s", "maxfr:");   printnondstack(MR_maxfr);
	printf("%-9s", "hp:");      printheap(MR_hp);
	printf("%-9s", "sp:");      printdetstack(MR_sp);

	print_ordinary_regs();
}

static void 
print_ordinary_regs(void)
{
	int	i;
	MR_Integer	value;

	for (i = 0; i < 8; i++) {
		printf("r%d:      ", i + 1);
		value = (MR_Integer) get_reg(i+1);

#ifndef	CONSERVATIVE_GC
		if ((MR_Integer) MR_ENGINE(heap_zone)->min <= value &&
				value < (MR_Integer) MR_ENGINE(heap_zone)->top) {
			printf("(heap) ");
		}
#endif

		printf("%ld\n", (long) value);
	}

	if (MR_sp >= &MR_CONTEXT(detstack_zone)->min[300]) {
		for (i = 321; i < 335; i++) {
			MR_printdetslot_as_label(i);
		}
	}
}

#endif /* defined(MR_DEBUG_GOTOS) */

static void 
MR_printdetslot_as_label(const MR_Integer offset)
{
	MR_printdetstackptr(&MR_CONTEXT(detstack_zone)->min[offset]);
	printf(" ");
	printlabel((MR_Code *) (MR_CONTEXT(detstack_zone)->min[offset]));
}

void 
MR_printdetstackptr(const MR_Word *s)
{
	MR_print_detstackptr(stdout, s);
}

void 
MR_print_detstackptr(FILE *fp, const MR_Word *s)
{
	fprintf(fp, "det %3ld (%p)",
		(long) (MR_Integer) (s - MR_CONTEXT(detstack_zone)->min),
		(const void *) s);
}

void 
printdetstack(const MR_Word *s)
{
	printf("ptr %p, offset %3ld words\n",
		(const void *) s,
		(long) (MR_Integer) (s - MR_CONTEXT(detstack_zone)->min));
}

void 
MR_printnondstackptr(const MR_Word *s)
{
	MR_print_nondstackptr(stdout, s);
}

void 
MR_print_nondstackptr(FILE *fp, const MR_Word *s)
{
	fprintf(fp, "non %3ld (%p)",
		(long) (MR_Integer) (s - MR_CONTEXT(nondetstack_zone)->min),
		(const void *) s);
}

void 
printnondstack(const MR_Word *s)
{
	printf("ptr %p, offset %3ld words\n",
		(const void *) s,
		(long) (MR_Integer) (s - MR_CONTEXT(nondetstack_zone)->min));
}

void 
MR_print_heapptr(FILE *fp, const MR_Word *s)
{
#ifdef	CONSERVATIVE_GC
	fprintf(fp, "heap %ld (%p)",
		(long) s, (const void *) s);
#else
	fprintf(fp, "heap %3ld (%p)",
		(long) (MR_Integer) (s - MR_ENGINE(heap_zone)->min),
		(const void *) s);
#endif
}

void 
MR_print_label(FILE *fp, /* const */ MR_Code *w)
{
	MR_Internal	*internal;

	internal = MR_lookup_internal_by_addr(w);
	if (internal != NULL) {
		if (internal->i_name != NULL) {
			fprintf(fp, "label %s (%p)", internal->i_name, w);
		} else {
			fprintf(fp, "label (%p)", w);
		}
	} else {
#ifdef	MR_DEBUG_GOTOS
		MR_Entry	*entry;

		entry = MR_prev_entry_by_addr(w);
		if (entry != NULL && entry->e_addr == w
			&& entry->e_name != NULL)
		{
			fprintf(fp, "label %s (%p)", entry->e_name, w);
		} else {
			fprintf(fp, "label UNKNOWN (%p)", w);
		}
#else
		fprintf(fp, "label UNKNOWN (%p)", w);
#endif	/* not MR_DEBUG_GOTOS */
	}
}

void 
printlabel(/* const */ MR_Code *w)
{
	MR_print_label(stdout, w);
	fprintf(stdout, "\n");
}
