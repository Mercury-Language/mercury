#include	<assert.h>
#include	"imp.h"

static Word get_reg(int num);

void mkcp_msg(void)
{
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
	printf("\nmodifying choice point for procedure %s\n", cpprednm);
	printf("redo ip: "); printlabel(cpredoip);
}

void succeed_msg(void)
{
	printf("\nsucceeding from procedure %s\n", cpprednm);
	printf("curr cp: "); printcpstack(curcp);
	printf("succ cp: "); printcpstack(cpsucccp);
	printf("succ ip: "); printlabel(cpsuccip);
}

void fail_msg(void)
{
	printf("\nfailing from procedure %s\n", cpprednm);
	printf("curr cp: "); printcpstack(curcp);
	printf("fail cp: "); printcpstack(cpprevcp);
	printf("fail ip: "); printlabel((Code *)cpprevcp[REDOIP]);
}

void redo_msg(void)
{
	printf("\nredo from procedure %s\n", cpprednm);
	printf("curr cp: "); printcpstack(curcp);
	printf("redo cp: "); printcpstack(maxcp);
	printf("redo ip: "); printlabel((Code *) maxcp[REDOIP]);
}

void call_msg(const Code *proc, const Code *succcont)
{
	printf("\ncalling      "); printlabel(proc);
	printf("continuation "); printlabel(succcont);
}

void tailcall_msg(const Code *proc)
{
	printf("\ntail calling "); printlabel(proc);
	printf("continuation "); printlabel(succip);
}

void proceed_msg(void)
{
	printf("\nreturning from determinate procedure\n");
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

/*--------------------------------------------------------------------*/

void printint(Word n)
{
	printf("int %d\n", n);
}

void printstring(char *s)
{
	printf("string %p %s\n", s, s);
}

void printheap(const Word *h)
{
	printf("ptr %p, offset %3d words\n", h, h - heapmin);
}

void printstack(const Word *s)
{
	printf("ptr %p, offset %3d words\n",
		s, s - stackmin);
}

void printcpstack(const Word *s)
{
	printf("ptr %p, offset %3d words, procedure %s\n",
		s, s - cpstackmin, (const char *) s[PREDNM]);
}

void dumpframe(Word *cp)
{
	reg	int	i;

	if ((cp - (Word *) cp[PREVCP]) == RECLAIM_SIZE)
	{
		printf("reclaim frame at ptr %p, offset %3d words\n",
			cp, cp - cpstackmin);
		printf("\t predname  %s\n", (const char *)cp[PREDNM]);
		printf("\t redoip    "); printlabel((Code *)cp[REDOIP]);
		printf("\t prevcp    "); printcpstack((Word *)cp[PREVCP]);
		printf("\t savehp    "); printheap((Word *)cp[SAVEHP]);
	}
	else
	{
		printf("cp frame at ptr %p, offset %3d words\n",
			cp, cp - cpstackmin);
		printf("\t predname  %s\n", (const char *)cp[PREDNM]);
		printf("\t succip    "); printlabel((Code *)cp[SUCCIP]);
		printf("\t redoip    "); printlabel((Word *)cp[REDOIP]);
		printf("\t succcp    "); printcpstack((Word *)cp[SUCCCP]);
		printf("\t prevcp    "); printcpstack((Word *)cp[PREVCP]);

		for (i = 0; &cp[SAVEVAL-i] > (Word *) cp[PREVCP]; i++)
			printf("\t cpvar(%d)  %d %x\n", i, cp[SAVEVAL-i], cp[SAVEVAL-i]);
	}
}

void dumpcpstack(void)
{
	reg	Word	*cp;

	printf("\ncpstack dump\n");
	for (cp = maxcp; cp > cpstackmin; cp = (Word *) cp[PREVCP])
		dumpframe(cp);
}

#define	LIST_WRAP	4
#define	LIST_TRUNC	13

void printlist(Word p)
{
	Word	t;
	Word	lastt;
	int	c;

	t = p;
	c = 0;
	while (tag(t) != TAG_NIL && c < LIST_TRUNC)
	{
		if ((c % LIST_WRAP) == 0 && c != 0)
			printf("\n\t ");

		printf("(%p)%d.", & field(TAG_CONS, t, 0), field(TAG_CONS, t, 0));
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

		printf("(%p)%d.", & field(TAG_CONS, lastt, 0), field(TAG_CONS, lastt, 0));
		fflush(stdout);
	}

	printf("nil\n");
	fflush(stdout);
}

void printlabel(const Code *w)
{
	int	i;

	for (i = 0; i < MAXENTRIES; i++)
		if (entries[i].e_addr == w)
		{
			printf("label %s (%p)\n", entries[i].e_name, w);
			return;
		}

	for (i = STARTLABELS; i < cur_entry; i++)
		if (entries[i].e_addr == w)
		{
			printf("label %s (%p)\n", entries[i].e_name, w);
			return;
		}

	printf("label UNKNOWN (%p)\n", w);
}

#define	FNULL	((PrintRegFunc *) 0)
#define P_INT 	((PrintRegFunc *) printint)
#define P_STR	((PrintRegFunc *) printstring)
#define P_LIST 	((PrintRegFunc *) printlist)
#define P_LABEL ((PrintRegFunc *) printlabel)
#define P_STACK	((PrintRegFunc *) printstack)
#define P_HEAP	((PrintRegFunc *) printheap)

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
	{ P_INT, P_INT, FNULL },
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
	{ P_INT, P_INT, P_INT, P_INT, P_INT, FNULL },
};

void printframe(const char *msg)
{
	reg	int	i;

	printf("\n%s\n", msg);
	dumpframe(curcp);
	for (i = 0; i < 31 && regtable[which][i] != FNULL; i++)
	{
		if (i < 10)
			printf("r%d:      ", i + 1);
		else
			printf("r%2d:     ", i + 1);

		(*regtable[which][i])(get_reg(i + 1));
	}
}

void printregs(const char *msg)
{
	reg	int	i;

	printf("\n%s\n", msg);

	printf("%-9s", "succip:");  printlabel(succip);
	printf("%-9s", "curcp:");  printcpstack(curcp);
	printf("%-9s", "maxcp:");  printcpstack(maxcp);
	printf("%-9s", "childcp:");  printcpstack(maxcp);
	printf("%-9s", "hp:");  printheap(hp);
	printf("%-9s", "sp:");  printstack(sp);

	for (i = 0; i < 31 && regtable[which][i] != FNULL; i++)
	{
		if (i < 10)
			printf("r%d:      ", i + 1);
		else
			printf("r%2d:     ", i + 1);

		(*regtable[which][i])(get_reg(i + 1));
	}
}

static Word get_reg(int num)
{
 	switch(num) {
		case 1: return r1;
		case 2: return r2;
		case 3: return r3;
		case 4: return r4;
		case 5: return r5;
		case 6: return r6;
		case 7: return r7;
		case 8: return r8;
		case 9: return r9;
		case 10: return r10;
		case 11: return r11;
		case 12: return r12;
		case 13: return r13;
		case 14: return r14;
		case 15: return r15;
		case 16: return r16;
		case 17: return r17;
		case 18: return r18;
		case 19: return r19;
		case 20: return r20;
		case 21: return r21;
		case 22: return r22;
		case 23: return r23;
		case 24: return r24;
		case 25: return r25;
		case 26: return r26;
		case 27: return r27;
		case 28: return r28;
		case 29: return r29;
		case 30: return r30;
		case 31: return r31;
		case 32: return r32;
	}
	/* NOTREACHED */
	abort();
}

Word mklist(int start, int len)
{
	Word	curr;
	int	i;

	curr = mkword(TAG_NIL, 0);
	for (i = 1; i <= len; i++)
	{
		curr = mkword(TAG_CONS, create2(start + len - i, curr));
	}

	return curr;
}
