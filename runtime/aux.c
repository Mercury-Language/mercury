#include	"imp.h"
#include	"list.h"
#include	"access.h"

/*--------------------------------------------------------------------*/

static void print_ordinary_regs(void);

/* debugging messages */

void mkframe_msg(void)
{
	restore_transient_registers();

	printf("\nnew choice point for procedure %s\n", curprednm);
	printf("new  fr: "); printnondstack(curfr);
	printf("prev fr: "); printnondstack(curprevfr);
	printf("succ fr: "); printnondstack(cursuccfr);
	printf("succ ip: "); printlabel(cursuccip);
	printf("redo ip: "); printlabel(curredoip);

	if (detaildebug)
		dumpnondstack();
}

void modframe_msg(void)
{
	restore_transient_registers();

	printf("\nmodifying choice point for procedure %s\n", curprednm);
	printf("redo ip: "); printlabel(curredoip);

	if (detaildebug)
		dumpnondstack();
}

void succeed_msg(void)
{
	restore_transient_registers();

	printf("\nsucceeding from procedure %s\n", curprednm);
	printf("curr fr: "); printnondstack(curfr);
	printf("succ fr: "); printnondstack(cursuccfr);
	printf("succ ip: "); printlabel(cursuccip);

	if (detaildebug)
		printregs("registers at success");
}

void succeeddiscard_msg(void)
{
	restore_transient_registers();

	printf("\nsucceeding from procedure %s, discarding frame\n", curprednm);
	printf("curr fr: "); printnondstack(curfr);
	printf("succ fr: "); printnondstack(cursuccfr);
	printf("succ ip: "); printlabel(cursuccip);

	if (detaildebug)
		printregs("registers at success");
}

void fail_msg(void)
{
	restore_transient_registers();

	printf("\nfailing from procedure %s\n", curprednm);
	printf("curr fr: "); printnondstack(curfr);
	printf("fail fr: "); printnondstack(curprevfr);
	printf("fail ip: "); printlabel(bt_redoip(curprevfr));
}

void redo_msg(void)
{
	restore_transient_registers();

	printf("\nredo from procedure %s\n", curprednm);
	printf("curr fr: "); printnondstack(curfr);
	printf("redo fr: "); printnondstack(maxfr);
	printf("redo ip: "); printlabel(bt_redoip(maxfr));
}

void call_msg(const Code *proc, const Code *succcont)
{
	printf("\ncalling      "); printlabel(proc);
	printf("continuation "); printlabel(succcont);
	printregs("registers at call");
}

void tailcall_msg(const Code *proc)
{
	restore_transient_registers();

	printf("\ntail calling "); printlabel(proc);
	printf("continuation "); printlabel(succip);
	printregs("registers at tailcall");
}

void proceed_msg(void)
{
	printf("\nreturning from determinate procedure\n");
	printregs("registers at proceed");
}

void cr1_msg(Word val0, const Word *addr)
{
	printf("put value %9x at ", val0);
	printheap(addr);
}

void cr2_msg(Word val0, Word val1, const Word *addr)
{
	printf("put values %9x,%9x at ", val0, val1);
	printheap(addr);
}

void incr_hp_msg(Word val, const Word *addr)
{
	printf("increment hp by %d from ", val);
	printheap(addr);
}

void incr_sp_msg(Word val, const Word *addr)
{
	printf("increment sp by %d from ", val);
	printdetstack(addr);
}

void decr_sp_msg(Word val, const Word *addr)
{
	printf("decrement sp by %d from ", val);
	printdetstack(addr);
}

void push_msg(Word val, const Word *addr)
{
	printf("push value %9x to ", val);
	printdetstack(addr);
}

void pop_msg(Word val, const Word *addr)
{
	printf("pop value %9x from ", val);
	printdetstack(addr);
}

void goto_msg(const Code *addr)
{
	printf("\ngoto ");
	printlabel(addr);

	if (detaildebug)
		printregs("registers at goto");
}

/*--------------------------------------------------------------------*/

/* debugging printing tools */

void printint(Word n)
{
	printf("int %d\n", n);
}

void printstring(const char *s)
{
	printf("string 0x%p %s\n", (const void *) s, s);
}

void printheap(const Word *h)
{
	printf("ptr 0x%p, offset %3d words\n", (const void *) h, h - heapmin);
}

void printdetstack(const Word *s)
{
	printf("ptr 0x%p, offset %3d words\n",
		(const void *) s, s - detstackmin);
}

void printnondstack(const Word *s)
{
#ifdef	SPEED
	printf("ptr 0x%p, offset %3d words\n",
		(const void *) s, s - nondstackmin);
#else
	printf("ptr 0x%p, offset %3d words, procedure %s\n",
		(const void *) s, s - nondstackmin, (const char *) s[PREDNM]);
#endif
}

void dumpframe(const Word *fr)
{
	reg	int	i;

	printf("frame at ptr 0x%p, offset %3d words\n",
		(const void *) fr, fr - nondstackmin);
#ifndef	SPEED
	printf("\t predname  %s\n", bt_prednm(fr));
#endif
	printf("\t succip    "); printlabel(bt_succip(fr));
	printf("\t redoip    "); printlabel(bt_redoip(fr));
	printf("\t succfr    "); printnondstack(bt_succfr(fr));
	printf("\t prevfr    "); printnondstack(bt_prevfr(fr));

	for (i = 0; &bt_var(fr,i) > bt_prevfr(fr); i++)
		printf("\t framevar(%d)  %d 0x%x\n",
			i, bt_var(fr,i), bt_var(fr,i));
}

void dumpnondstack(void)
{
	reg	Word	*fr;

	printf("\nnondstack dump\n");
	for (fr = maxfr; fr > nondstackmin; fr = bt_prevfr(fr))
		dumpframe(fr);
}

#define	LIST_WRAP	4
#define	LIST_TRUNC	13

void printlist(Word p)
{
	Word	t;
	Word	lastt;
	Word	*ptr;
	int	c;

	t = p;
	c = 0;
	while (tag(t) != TAG_NIL && c < LIST_TRUNC)
	{
		if ((c % LIST_WRAP) == 0 && c != 0)
			printf("\n\t ");

		ptr = (Word *) body(t, TAG_CONS);
		if (((int)ptr & 0x3) || ptr == 0
		    || ptr < heapmin || ptr >= heapend)
		{
			printf("0x%x (%d)\n", t, t);
			return;
		}
		printf("(0x%p)%d.", (void *) & field(TAG_CONS,t,0),
			field(TAG_CONS,t,0));
		fflush(stdout);
		t = field(TAG_CONS, t, 1);

		c += 1;
	}

	if (tag(t) != TAG_NIL)
	{
		printf("ETC.");
		fflush(stdout);
		lastt = t;
		while (tag(t) != TAG_NIL)
		{
			lastt = t;
			t = field(TAG_CONS, t, 1);
		}

		printf("(0x%p)%d.", (void *) & field(TAG_CONS, lastt, 0),
			field(TAG_CONS, lastt, 0));
		fflush(stdout);
	}

	printf("nil\n");
	fflush(stdout);
}

void printlabel(const Code *w)
{
	Label	*label;

	label = lookup_label_addr(w);
	if (label != NULL)
		printf("label %s (0x%p)\n", label->e_name, w);
	else
		printf("label UNKNOWN (0x%p)\n", w);
}

void printframe(const char *msg)
{
	printf("\n%s\n", msg);
	dumpframe(curfr);

	print_ordinary_regs();
}

void printregs(const char *msg)
{
	restore_transient_registers();

	printf("\n%s\n", msg);

	printf("%-9s", "succip:");  printlabel(succip);
	printf("%-9s", "curfr:");   printnondstack(curfr);
	printf("%-9s", "maxfr:");   printnondstack(maxfr);
	printf("%-9s", "hp:");      printheap(hp);
	printf("%-9s", "sp:");      printdetstack(sp);

	print_ordinary_regs();
}

static void print_ordinary_regs(void)
{
	int	i;
	int	value;

	restore_transient_registers();
	for (i = 0; i < 8; i++)
	{
		printf("r%d:      ", i + 1);
		value = get_reg(i+1);

		if ((int) heapmin <= value && value < (int) heapend)
			printf("(heap) ");

		printf("%d\n", value);
	}
}

Word do_mklist(int start, int len)
{
	Word	curr;
	int	i;

	restore_transient_registers();
	curr = mkword(TAG_NIL, 0);
	for (i = 1; i <= len; i++)
	{
		curr = mkword(TAG_CONS, create2(start + len - i, curr));
	}
	save_transient_registers();
	return curr;
}

void *newmem(size_t n)
{
	reg	void	*p;

	p = malloc(n);
	if (p == NULL)
	{
		fatal_error("ran out of memory");
	}

	return p;
}

void oldmem(void *p)
{
	free(p);
}

void fatal_error(const char *message) {
	fprintf(stderr, "%s\n", message);
	exit(1);
}

#ifndef __GNUC__

/*
**  Note that hash_string is also defined in compiler/string.nl
**  and code/imp.h.  The three definitions must be kept equivalent.
*/

int hash_string(const char *s)
{
	int len = 0;
	int hash = 0;
	while(((char *)s)[len]) {
		hash ^= (hash << 5);
		hash ^= ((char *)s)[len];
		len++;
	}
	hash ^= len;
	return hash;
}

#endif /* not __GNUC__ */
