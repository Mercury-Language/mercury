#include	"imp.h"
#include	"list.h"
#include	"access.h"

void mkcp_msg(void)
{
	restore_registers();
	printf("\nnew choice point for procedure %s\n", cpprednm);
	printf("new  cp: "); printcpstack(curcp);
	printf("prev cp: "); printcpstack(cpprevcp);
	printf("succ cp: "); printcpstack(cpsucccp);
	printf("succ ip: "); printlabel(cpsuccip);
	printf("redo ip: "); printlabel(cpredoip);
	if (detaildebug)
		dumpcpstack();
}

void mkreclaim_msg(void)
{
	restore_registers();
	printf("\nnew reclaim point for procedure %s\n", cpprednm);
	printf("new  cp: "); printcpstack(curcp);
	printf("prev cp: "); printcpstack(recprevcp);
	printf("redo ip: "); printlabel(recredoip);
	printf("save hp: "); printheap(recsavehp);
	if (detaildebug)
		dumpcpstack();
}

void modcp_msg(void)
{
	restore_registers();
	printf("\nmodifying choice point for procedure %s\n", cpprednm);
	printf("redo ip: "); printlabel(cpredoip);
	if (detaildebug)
		dumpcpstack();
}

void succeed_msg(void)
{
	restore_registers();
	printf("\nsucceeding from procedure %s\n", cpprednm);
	printf("curr cp: "); printcpstack(curcp);
	printf("succ cp: "); printcpstack(cpsucccp);
	printf("succ ip: "); printlabel(cpsuccip);
	printregs("registers at success");
}

void fail_msg(void)
{
	restore_registers();
	printf("\nfailing from procedure %s\n", cpprednm);
	printf("curr cp: "); printcpstack(curcp);
	printf("fail cp: "); printcpstack(cpprevcp);
	printf("fail ip: "); printlabel(bt_redoip(cpprevcp));
}

void redo_msg(void)
{
	restore_registers();
	printf("\nredo from procedure %s\n", cpprednm);
	printf("curr cp: "); printcpstack(curcp);
	printf("redo cp: "); printcpstack(maxcp);
	printf("redo ip: "); printlabel(bt_redoip(maxcp));
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
	printstack(addr);
}

void pop_msg(Word val, const Word *addr)
{
	printf("pop value %9x from ", val);
	printstack(addr);
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

void printstack(const Word *s)
{
	printf("ptr 0x%p, offset %3d words\n",
		s, s - stackmin);
}

void printcpstack(const Word *s)
{
	printf("ptr 0x%p, offset %3d words, procedure %s\n",
		s, s - cpstackmin, (const char *) s[PREDNM]);
}

void dumpframe(const Word *cp)
{
	reg	int	i;

	if ((cp - bt_prevcp(cp)) == RECLAIM_SIZE)
	{
		printf("reclaim frame at ptr 0x%p, offset %3d words\n",
			cp, cp - cpstackmin);
		printf("\t predname  %s\n", bt_prednm(cp));
		printf("\t redoip    "); printlabel(bt_redoip(cp));
		printf("\t prevcp    "); printcpstack(bt_prevcp(cp));
		printf("\t savehp    "); printheap(bt_savehp(cp));
	}
	else
	{
		printf("cp frame at ptr 0x%p, offset %3d words\n",
			cp, cp - cpstackmin);
		printf("\t predname  %s\n", bt_prednm(cp));
		printf("\t succip    "); printlabel(bt_succip(cp));
		printf("\t redoip    "); printlabel(bt_redoip(cp));
		printf("\t succcp    "); printcpstack(bt_succcp(cp));
		printf("\t prevcp    "); printcpstack(bt_prevcp(cp));

		for (i = 0; &bt_var(cp,i) > bt_prevcp(cp); i++)
			printf("\t cpvar(%d)  %d 0x%x\n",
				i, bt_var(cp,i), bt_var(cp,i));
	}
}

void dumpcpstack(void)
{
	reg	Word	*cp;

	printf("\ncpstack dump\n");
	for (cp = maxcp; cp > cpstackmin; cp = bt_prevcp(cp))
		dumpframe(cp);
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

int whichlabel(char *name)
{
	reg	int	i;
	reg	bool	found;

	found = FALSE;
	for (i = 0; i < MAXLABELS; i++)
		if (streq(optarg, entries[i].e_name))
			return i;

	return -1;
}

#define	FNULL	((PrintRegFunc *) 0)
#define P_INT 	((PrintRegFunc *) printint)
#define P_STR	((PrintRegFunc *) printstring)
#define P_LIST 	((PrintRegFunc *) printlist)
#define P_LABEL ((PrintRegFunc *) printlabel)
#define P_STACK	((PrintRegFunc *) printstack)
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

	printf("\n%s\n", msg);
	dumpframe(curcp);
	for (i = 0; i < 5; i++)
	{
		if (i < 10)
			printf("r%d:      ", i + 1);
		else
			printf("r%2d:     ", i + 1);

		printlist(get_reg(i + 1));
	}
}

void printregs(const char *msg)
{
	int	i;

	restore_registers();

	printf("\n%s\n", msg);

	printf("%-9s", "succip:");  printlabel(succip);
	printf("%-9s", "curcp:");   printcpstack(curcp);
	printf("%-9s", "maxcp:");   printcpstack(maxcp);
	printf("%-9s", "childcp:"); printcpstack(maxcp);
	printf("%-9s", "hp:");      printheap(hp);
	printf("%-9s", "sp:");      printstack(sp);

	for (i = 0; i < 5; i++)
	{
		printf("r%d:      ", i + 1);
		printlist(get_reg(i + 1));
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
