#include	"imp.h"
#include	"list.h"
#include	"access.h"

void mkframe_msg(void)
{
	restore_registers();
	printf("\nnew choice point for procedure %s\n", curprednm);
	printf("new  fr: "); printnondstack(curfr);
	printf("prev fr: "); printnondstack(curprevfr);
	printf("succ fr: "); printnondstack(cursuccfr);
	printf("succ ip: "); printlabel(cursuccip);
	printf("redo ip: "); printlabel(curredoip);
	if (detaildebug)
		dumpnondstack();
}

void mkreclaim_msg(void)
{
	restore_registers();
	printf("\nnew reclaim point for procedure %s\n", curprednm);
	printf("new  fr: "); printnondstack(curfr);
	printf("prev fr: "); printnondstack(recprevfr);
	printf("redo ip: "); printlabel(recredoip);
	printf("save hp: "); printheap(recsavehp);
	if (detaildebug)
		dumpnondstack();
}

void modframe_msg(void)
{
	restore_registers();
	printf("\nmodifying choice point for procedure %s\n", curprednm);
	printf("redo ip: "); printlabel(curredoip);
	if (detaildebug)
		dumpnondstack();
}

void succeed_msg(void)
{
	restore_registers();
	printf("\nsucceeding from procedure %s\n", curprednm);
	printf("curr fr: "); printnondstack(curfr);
	printf("succ fr: "); printnondstack(cursuccfr);
	printf("succ ip: "); printlabel(cursuccip);
	printregs("registers at success");
}

void succeeddiscard_msg(void)
{
	restore_registers();
	printf("\nsucceeding from procedure %s, discarding frame\n", curprednm);
	printf("curr fr: "); printnondstack(curfr);
	printf("succ fr: "); printnondstack(cursuccfr);
	printf("succ ip: "); printlabel(cursuccip);
	printregs("registers at success");
}

void fail_msg(void)
{
	restore_registers();
	printf("\nfailing from procedure %s\n", curprednm);
	printf("curr fr: "); printnondstack(curfr);
	printf("fail fr: "); printnondstack(curprevfr);
	printf("fail ip: "); printlabel(bt_redoip(curprevfr));
}

void redo_msg(void)
{
	restore_registers();
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
	restore_registers();
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

void printint(Word n)
{
	printf("int %d\n", n);
}

void printstring(const char *s)
{
	printf("string 0x%p %s\n", s, s);
}

void printheap(const Word *h)
{
	printf("ptr 0x%p, offset %3d words\n", h, h - heapmin);
}

void printdetstack(const Word *s)
{
	printf("ptr 0x%p, offset %3d words\n",
		s, s - detstackmin);
}

void printnondstack(const Word *s)
{
	printf("ptr 0x%p, offset %3d words, procedure %s\n",
		s, s - nondstackmin, (const char *) s[PREDNM]);
}

void dumpframe(const Word *fr)
{
	reg	int	i;

	if ((fr - bt_prevfr(fr)) == RECLAIM_SIZE)
	{
		printf("reclaim frame at ptr 0x%p, offset %3d words\n",
			fr, fr - nondstackmin);
		printf("\t predname  %s\n", bt_prednm(fr));
		printf("\t redoip    "); printlabel(bt_redoip(fr));
		printf("\t prevfr    "); printnondstack(bt_prevfr(fr));
		printf("\t savehp    "); printheap(bt_savehp(fr));
	}
	else
	{
		printf("frame at ptr 0x%p, offset %3d words\n",
			fr, fr - nondstackmin);
		printf("\t predname  %s\n", bt_prednm(fr));
		printf("\t succip    "); printlabel(bt_succip(fr));
		printf("\t redoip    "); printlabel(bt_redoip(fr));
		printf("\t succfr    "); printnondstack(bt_succfr(fr));
		printf("\t prevfr    "); printnondstack(bt_prevfr(fr));

		for (i = 0; &bt_var(fr,i) > bt_prevfr(fr); i++)
			printf("\t framevar(%d)  %d 0x%x\n",
				i, bt_var(fr,i), bt_var(fr,i));
	}
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
		    || ptr < heap || ptr >= heap + MAXHEAP)
		{
			printf("0x%x (%d)\n", t, t);
			return;
		}
		printf("(0x%p)%d.", & field(TAG_CONS,t,0), field(TAG_CONS,t,0));
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

		printf("(0x%p)%d.", & field(TAG_CONS, lastt, 0), field(TAG_CONS, lastt, 0));
		fflush(stdout);
	}

	printf("nil\n");
	fflush(stdout);
}

void printlabel(const Code *w)
{
	int	i;

	for (i = 0; i < cur_entry; i++)
		if (entries[i].e_addr == w)
		{
			printf("label %s (0x%p)\n", entries[i].e_name, w);
			return;
		}

	printf("label UNKNOWN (0x%p)\n", w);
}

int whichlabel(const char *name)
{
	reg	int	i;
	reg	bool	found;

	found = FALSE;
	for (i = 0; i < MAXLABELS; i++)
	{
		if (streq(name, entries[i].e_name))
			return i;
	}

	return -1;
}

#define	FNULL	((PrintRegFunc *) 0)
#define P_INT 	((PrintRegFunc *) printint)
#define P_STR	((PrintRegFunc *) printstring)
#define P_LIST 	((PrintRegFunc *) printlist)
#define P_LABEL ((PrintRegFunc *) printlabel)
#define P_STACK	((PrintRegFunc *) printdetstack)
#define P_HEAP	((PrintRegFunc *) printheap)

#if 0	/* this code no longer used */

/* The following table describes the contents of the
   the registers r1, r2, ... for the specified
   entry points.
*/

PrintRegFunc	*regtable[MAXENTRIES][32] =
{
/* APPEND_1 */
	{ P_LIST, P_LIST, P_LIST, FNULL },
/* APPEND_2 */
	{ P_LIST, P_LIST, P_LIST, FNULL },
/* NREV_1 */
	{ P_LIST, P_LIST, P_LIST, FNULL },
/* LENGTH_1 */
	{ P_LIST, P_INT, FNULL },
/* LENGTH_2 */
	{ P_LIST, P_INT, FNULL },
/* ACLENGTH_1 */
	{ P_LIST, P_INT, P_INT, FNULL },
/* MEMBER_1 */
	{ P_INT, P_LIST, FNULL },
/* MEMBER_2 */
	{ P_INT, P_LIST, FNULL },
/* MEMDET_1 */
	{ P_LIST, P_INT, P_INT, FNULL },
/* MKLIST_1 */
	{ P_LIST, FNULL },
/* HEAP_1 */
	{ P_LIST, P_INT, FNULL },
/* ONEDET_1 */
	{ P_INT, FNULL },
/* INT_1 */
	{ P_INT, P_INT, FNULL },
/* Q_1 */
	{ P_INT, P_INT, FNULL },
/* NOT_Q_1 */
	{ P_INT, P_INT, FNULL },
/* NOT_Q5_1 */
	{ P_INT, P_INT, FNULL },
/* DETNEG_1 */
	{ P_INT, P_INT, FNULL },
/* NONDETNEG_1 */
	{ P_INT, P_INT, FNULL },
/* A_1 */
	{ P_INT, P_INT, FNULL },
/* C_1 */
	{ P_INT, P_INT, FNULL },
/* D_1 */
	{ P_INT, P_INT, FNULL },
/* E_1 */
	{ P_INT, P_INT, FNULL },
/* F_1 */
	{ P_INT, P_INT, FNULL },
/* COLOR_1 */
	{ P_INT, P_INT, P_INT, P_INT, P_INT, FNULL },
/* NEXT_1 */
	{ P_INT, P_INT, FNULL },
/* NEXT_2 */
	{ P_INT, P_INT, FNULL },
/* NEXT_3 */
	{ P_INT, P_INT, FNULL },
/* OK_1 */
	{ P_INT, P_INT, FNULL },
/* OK_2 */
	{ P_INT, P_INT, FNULL },
/* OK_3 */
	{ P_INT, P_INT, FNULL },
/* OK_4 */
	{ P_INT, P_INT, FNULL },
/* DO_NOTHING_1 */
	{ FNULL },
/* QUEEN_1 */
	{ P_LIST, FNULL },
/* QPERM_1 */
	{ P_LIST, P_LIST, FNULL },
/* QDELETE_1 */
	{ P_INT, P_LIST, P_LIST, FNULL },
/* SAFE_1 */
	{ P_LIST, P_LIST, P_LIST, FNULL },
/* NODIAG_1 */
	{ P_INT, P_INT, P_LIST, FNULL }
};
#endif /* old code */

void printframe(const char *msg)
{
	reg	int	i;
	reg	int	value;

	printf("\n%s\n", msg);
	dumpframe(curfr);

	restore_registers();

	for (i = 0; i < 5; i++)
	{
		printf("r%d:      ", i + 1);
		value = get_reg(i+1);
		if ((int) heapmin <= value && value <= (int) heapmax)
			printlist(value);
		else
			printint(value);
	}
}

void printregs(const char *msg)
{
	reg	int	i;
	reg	int	value;

	restore_registers();

	printf("\n%s\n", msg);

	printf("%-9s", "succip:");  printlabel(succip);
	printf("%-9s", "curfr:");   printnondstack(curfr);
	printf("%-9s", "maxfr:");   printnondstack(maxfr);
	printf("%-9s", "childfr:"); printnondstack(maxfr);
	printf("%-9s", "hp:");      printheap(hp);
	printf("%-9s", "sp:");      printdetstack(sp);

	for (i = 0; i < 5; i++)
	{
		printf("r%d:      ", i + 1);
		value = get_reg(i+1);
		if ((int) heapmin <= value && value <= (int) heapmax)
			printlist(value);
		else
			printint(value);
	}
}

Word do_mklist(int start, int len)
{
	Word	curr;
	int	i;

	restore_registers();
	curr = mkword(TAG_NIL, 0);
	for (i = 1; i <= len; i++)
	{
		curr = mkword(TAG_CONS, create2(start + len - i, curr));
	}
	save_registers();
	return curr;
}

void *
newmem(int n)
{
	reg	void	*p;

	p = malloc((unsigned) n);
	if (p == (void *) NULL)
	{
		fprintf(stderr, "ran out of malloc\n");
		abort();
	}

	return p;
}

void
oldmem(void *p)
{
	free(p);
}
