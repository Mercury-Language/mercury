#include	"imp.h"

void mkcp_msg()
{
	printf("\nnew choice point for procedure %s\n", cpprednm);
	printf("new  cp: "); printcpstack((Word) curcp);
	printf("prev cp: "); printcpstack((Word) cpprevcp);
	printf("succ cp: "); printcpstack((Word) cpsucccp);
	printf("succ ip: "); printlabel((Word) cpsuccip);
	printf("redo ip: "); printlabel((Word) cpredoip);
	if (detaildebug)
		dumpcpstack();
}

void mkreclaim_msg()
{
	printf("\nnew reclaim point for procedure %s\n", cpprednm);
	printf("new  cp: "); printcpstack((Word) curcp);
	printf("prev cp: "); printcpstack((Word) recprevcp);
	printf("redo ip: "); printlabel((Word) recredoip);
	printf("save hp: "); printheap((Word) recsavehp);
	if (detaildebug)
		dumpcpstack();
}

void modcp_msg()
{
	printf("\nmodifying choice point for procedure %s\n", cpprednm);
	printf("redo ip: "); printlabel((Word) cpredoip);
}

void succeed_msg()
{
	printf("\nsucceeding from procedure %s\n", cpprednm);
	printf("curr cp: "); printcpstack((Word) curcp);
	printf("succ cp: "); printcpstack((Word) cpsucccp);
	printf("succ ip: "); printlabel((Word) cpsuccip);
}

void fail_msg()
{
	printf("\nfailing from procedure %s\n", cpprednm);
	printf("curr cp: "); printcpstack((Word) curcp);
	printf("fail cp: "); printcpstack((Word) cpprevcp);
	printf("fail ip: "); printlabel((Word) (((Word *) cpprevcp)[REDOIP]));
}

void redo_msg()
{
	printf("\nredo from procedure %s\n", cpprednm);
	printf("curr cp: "); printcpstack((Word) curcp);
	printf("redo cp: "); printcpstack((Word) maxcp);
	printf("redo ip: "); printlabel((Word) maxcp[REDOIP]);
}

void call_msg(const Word *proc, const Word *succcont)
{
	printf("\ncalling      "); printlabel((Word) proc);
	printf("continuation "); printlabel((Word) succcont);
}

void proceed_msg(void)
{
	printf("\nreturning from determinate procedure\n");
}

void push_msg(Word val, const Word *addr)
{
	printf("push value %x to ", val);
	printstack((Word) addr);
}

void pop_msg(Word val, const Word *addr)
{
	printf("pop value %x from ", val);
	printstack((Word) addr);
}

void printtmps(void)
{
	printf("tmps x%x x%x x%x x%x x%x x%x x%x x%x\n",
		tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7);
}

void printint(Word n)
{
	printf("int %d\n", n);
}
void printheap(Word h)
{
	printf("ptr %x, offset %3d words\n",
		h, (Word *) h - heapmin);
}

void printstack(Word s)
{
	printf("ptr %x, offset %3d words\n",
		s, (Word *) s - stackmin);
}

void printcpstack(Word s)
{
	printf("ptr %x, offset %3d words, procedure %s\n",
		s, (Word *) s - cpstackmin,
		(const char *)(((Word *) s)[PREDNM]));
}

void dumpcpstack()
{
	reg	Word	*cp;
	reg	int	i;

	printf("\ncpstack dump\n");
	for (cp = maxcp; cp > cpstackmin; cp = (Word *) cp[PREVCP])
	{
		if ((cp - (Word *)cp[PREVCP]) == RECLAIM_SIZE)
		{
			printf("reclaim frame at ptr %p, offset %3d words\n",
				cp, cp - cpstackmin);
			printf("\tpredname  %s\n", (const char *)cp[PREDNM]);
			printf("\tredoip    "); printlabel(cp[REDOIP]);
			printf("\tprevcp    "); printcpstack(cp[PREVCP]);
			printf("\tsavehp    "); printheap(cp[SAVEHP]);
		}
		else
		{
			printf("cp frame at ptr %p, offset %3d words\n",
				cp, cp - cpstackmin);
			printf("\tpredname  %s\n", (const char *)cp[PREDNM]);
			printf("\tsuccip    "); printlabel(cp[SUCCIP]);
			printf("\tredoip    "); printlabel(cp[REDOIP]);
			printf("\tsucccp    "); printcpstack(cp[SUCCCP]);
			printf("\tprevcp    "); printcpstack(cp[PREVCP]);

			for (i = 0; &cp[SAVEVAL-i] > (Word *) cp[PREVCP]; i++)
				printf("\tcpvar(%d)  %x\n", i, cp[SAVEVAL-i]);
		}
	}
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

void printlabel(Word w)
{
	int	i;

	for (i = 0; i < MAXENTRIES; i++)
		if (entries[i].e_addr == (void *) w)
		{
			printf("label %s (%x)\n", entries[i].e_name, w);
			return;
		}

	for (i = STARTLABELS; i < cur_entry; i++)
		if (entries[i].e_addr == (void *) w)
		{
			printf("label %s (%x)\n", entries[i].e_name, w);
			return;
		}

	printf("label UNKNOWN (%x)\n", w);
}

#define	FNULL	((PrintRegFunc *)0)

PrintRegFunc	*regtable[MAXENTRIES][16] =
{
/* APPEND_1 */
	{ printlabel, printlist, printlist, printlist,
	FNULL, FNULL, printheap, printstack,
	FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL },
/* APPEND_2 */
	{ printlabel, printlist, printlist, printlist,
	FNULL, FNULL, printheap, printstack,
	FNULL, FNULL, FNULL, FNULL,
	FNULL, printcpstack, printcpstack, printcpstack },
/* NREV_1 */
	{ printlabel, printlist, printlist, printlist,
	FNULL, FNULL, printheap, printstack,
	FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL },
/* LENGTH_1 */
	{ printlabel, printlist, printint, FNULL,
	FNULL, FNULL, printheap, printstack,
	FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL },
/* LENGTH_2 */
	{ printlabel, printlist, printint, FNULL,
	FNULL, FNULL, printheap, printstack,
	FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL },
/* ACLENGTH_1 */
	{ printlabel, printlist, printint, printint,
	FNULL, FNULL, printheap, printstack,
	FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL },
/* MEMBER_1 */
	{ printlabel, printlist, printint, printint,
	FNULL, FNULL, printheap, printstack,
	FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL },
/* MEMBER_2 */
	{ printlabel, printlist, printint, printint,
	FNULL, FNULL, printheap, printstack,
	FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL },
/* MKLIST_1 */
	{ printlabel, printlist, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL,
	FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL, FNULL },
/* Q_1 */
	{ printlabel, printint, printint, FNULL, FNULL, FNULL, FNULL, FNULL,
	FNULL, FNULL, FNULL, FNULL,
	FNULL, printcpstack, printcpstack, printcpstack },
/* INT_1 */
	{ printlabel, printint, printint, FNULL, FNULL, FNULL, FNULL, FNULL,
	FNULL, FNULL, FNULL, FNULL,
	FNULL, printcpstack, printcpstack, printcpstack },
/* HEAP_1 */
	{ printlabel, printlist, printint, FNULL,
	FNULL, FNULL, printheap, printstack,
	FNULL, FNULL, FNULL, FNULL,
	FNULL, printcpstack, printcpstack, printcpstack },
};

void printregs(const char *msg)
{
	printf("\n%s\n", msg);

	if (*regtable[which][0] != FNULL)
	{
		printf("%-9s", "succip:");
		(*regtable[which][0])(r0);
	}

	if (*regtable[which][1] != FNULL)
	{
		printf("%-9s", "r1:");
		(*regtable[which][1])(r1);
	}

	if (*regtable[which][2] != FNULL)
	{
		printf("%-9s", "r2:");
		(*regtable[which][2])(r2);
	}

	if (*regtable[which][3] != FNULL)
	{
		printf("%-9s", "r3:");
		(*regtable[which][3])(r3);
	}

	if (*regtable[which][4] != FNULL)
	{
		printf("%-9s", "r4:");
		(*regtable[which][4])(r4);
	}

	if (*regtable[which][5] != FNULL)
	{
		printf("%-9s", "r5:");
		(*regtable[which][5])(r5);
	}

	if (*regtable[which][6] != FNULL)
	{
		printf("%-9s", "hp:");
		(*regtable[which][6])(r6);
	}

	if (*regtable[which][7] != FNULL)
	{
		printf("%-9s", "sp:");
		(*regtable[which][7])(r7);
	}

	if (*regtable[which][8] != FNULL)
	{
		printf("%-9s", "tmp0:");
		(*regtable[which][8])(tmp0);
	}

	if (*regtable[which][9] != FNULL)
	{
		printf("%-9s", "tmp1:");
		(*regtable[which][9])(tmp1);
	}

	if (*regtable[which][10] != FNULL)
	{
		printf("%-9s", "tmp2:");
		(*regtable[which][10])(tmp2);
	}

	if (*regtable[which][11] != FNULL)
	{
		printf("%-9s", "tmp3:");
		(*regtable[which][11])(tmp3);
	}

	if (*regtable[which][12] != FNULL)
	{
		printf("%-9s", "tmp4:");
		(*regtable[which][12])(tmp4);
	}

	if (*regtable[which][13] != FNULL)
	{
		printf("%-9s", "childcp:");
		(*regtable[which][13])(tmp5);
	}

	if (*regtable[which][14] != FNULL)
	{
		printf("%-9s", "curcp:");
		(*regtable[which][14])(tmp6);
	}

	if (*regtable[which][15] != FNULL)
	{
		printf("%-9s", "maxcp:");
		(*regtable[which][15])(tmp7);
	}
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

void mkinput(int r1val, int r2val, int r3val)
{
	if (*regtable[which][1] == printlist)
		r1 = mklist(1, r1val);
	else
		r1 = r1val;

	if (*regtable[which][2] == printlist)
		r2 = mklist(101, r2val);
	else
		r2 = r2val;

	if (*regtable[which][3] == printlist)
		r3 = mklist(201, r3val);
	else
		r3 = r3val;

	r4 = 0;
	r5 = 0;
}
