#include	<stdio.h>
#include	<assert.h>
#include	"getopt.h"
#include	"std.h"

/* GENERAL DEFINITIONS */

#define	WORDSIZE	4

typedef	uint	Word;
typedef void	Code;

/* DEFINITIONS FOR THE LABEL TABLE */

typedef struct s_entry
{
	const char	*e_name;   /* name of the procedure	     */
	Code		*e_addr;   /* address of the code	     */
	Code		*e_input;  /* address of the input generator */
} Entry;

/* when you modify this, you must modify the table in aux.c as well */
enum {
	APPEND_1,
	APPEND_2,
	NREV_1,
	LENGTH_1,
	LENGTH_2,
	ACLENGTH_1,
	MEMBER_1,
	MEMBER_2,
	MEMDET_1,
	MKLIST_1,
	HEAP_1,
	ONETEN_1,
	INT_1,
	Q_1,
	NOT_Q_1,
	NOT_Q5_1,
	DETNEG_1,
	NONDETNEG_1,
	A_1,
	C_1,
	D_1,
	E_1,
	F_1,
	COLOR_1,
	NEXT_1,
	NEXT_2,
	NEXT_3,
	OK_1,
	OK_2,
	OK_3,
	OK_4,

	MAXENTRIES
};

#define	STARTLABELS	100
#define	MAXLABELS	800

#define	makeentry(e, n, a, i)					\
			do {					\
				entries[e].e_name  = n;		\
				entries[e].e_addr  = a;		\
				entries[e].e_input = i;		\
			} while (0)

#define	makelabel(n, a)	do {					\
				entries[cur_entry].e_name  = n;	\
				entries[cur_entry].e_addr  = a;	\
				entries[cur_entry].e_input = NULL;\
				cur_entry += 1;			\
			} while (0)

/* a table of the entry points defined by the various modules */
extern	Entry	entries[];
extern	int	cur_entry;	/* next free slot in entries table   */
extern	int	which;		/* procedure called from interpreter */

/* standard labels in the entry table */
extern	Code	*doredo;
extern	Code	*dofail;
extern	Code	*doresethpfail;
extern	Code	*doresetcpvar0fail;
extern	Code	*dosucceed;
extern	Code	*doslownegfail;
extern	Code	*doslownegsucceed;
extern	Code	*dofastnegredo;
extern	Code	*dofastnegproceed;

/* DEFINITIONS FOR WORD LAYOUT */

#ifdef	HIGHTAGS

#define	mktag(t)	(t << 30)
#define	tag(w)		(w & 0xc0000000)
#define	body(w, t)	(w & ~0xc0000000)
#define	mkword(t, p)	((uint) t | (uint) p)
#define	field(t, p, i)	(* (Word *) (body(p) + i * WORDSIZE))

#else

#define	mktag(t)	(t)
#define	tag(w)		(w & 0x3)
#define	body(w, t)	(w - t)
#define	mkword(t, p)	((uint) p | (uint) t)
#define	field(t, p, i)	(* (Word *) (body(p, t) + i * WORDSIZE))

#endif

#define	bTAG_NIL	0
#define	bTAG_CONS	1
#define	bTAG_VAR	3

#define	TAG_NIL		mktag(bTAG_NIL)
#define	TAG_CONS	mktag(bTAG_CONS)
#define	TAG_VAR		mktag(bTAG_VAR)

#define	deref(p)	({					\
				reg	Word	pt;		\
								\
				pt = p;				\
				while (tag(pt) == TAG_VAR)	\
					pt = * (Word *)		\
						body(pt, TAG_VAR);\
				pt;				\
			})

/* DEFINITIONS FOR VIRTUAL MACHINE REGISTERS */

#include "regs.h"

/* DEFINITIONS FOR CALLS AND RETURNS */

#define	call(proc, succcont)					\
			do {					\
				debugcall(proc, succcont);	\
				succip = succcont;		\
				goto *proc;			\
			} while (0)

#define	callentry(entry, succcont)				\
			do {					\
				debugcall(entries[entry].e_addr, succcont);\
				succip = succcont;		\
				goto *entries[entry].e_addr;	\
			} while (0)

#define	tailcall(proc)	do {					\
				debugtailcall(proc);		\
				goto *proc;			\
			} while (0)

#define	tailcallentry(entry, succcont)				\
			do {					\
				debugtailcall(entries[entry].e_addr);\
				goto *entries[entry].e_addr;	\
			} while (0)

#define	proceed()	do {					\
				debugproceed();			\
				goto *succip;			\
			} while (0)

/* DEFINITIONS FOR VIRTUAL MACHINE DATA AREAS */

#define	MAXHEAP		0x10000
#define	MAXSTACK	0x10000
#define	MAXCPSTACK	0x10000
#define	CACHE_OFFSET	0x200

extern	Word	heap[];
extern	Word	stack[];
extern	Word	cpstack[];

extern	Word	*heapmax;
extern	Word	*stackmax;
extern	Word	*cpstackmax;

extern	Word	*heapmin;
extern	Word	*stackmin;
extern	Word	*cpstackmin;

/* DEFINITIONS FOR MANIPULATING THE HEAP */

#define create1(w1)	({					\
				reg	Word	p;		\
								\
				hp[0] = (Word) (w1);		\
				debugcr1(hp[0], hp);	\
				p = (Word) hp;			\
				hp += 1;			\
				heap_overflow_check();		\
				/* return */ p;			\
			})

#define create2(w1, w2)	({					\
				reg	Word	p;		\
								\
				hp[0] = (Word) (w1);		\
				hp[1] = (Word) (w2);		\
				debugcr2(hp[0], hp[1], hp);	\
				p = (Word) hp;			\
				hp += 2;			\
				heap_overflow_check();		\
				/* return */ p;			\
			})

/* DEFINITIONS FOR MANIPULATING THE STACK */

#define	stackvar(n)	sp[-n]

#define	push(w)		do {					\
				*sp = (Word) (w);		\
				debugpush(*sp, sp);		\
				sp += 1;			\
				stack_overflow_check();		\
			} while (0)

#define	pop()		({					\
				reg	Word	w;		\
								\
				sp -= 1;			\
				w = *sp;			\
				debugpop(*sp, sp);		\
				stack_underflow_check();	\
				/* return */ w;			\
			})

/* DEFINITIONS FOR CHOICE AND RECLAIM POINTS */

#define	PREDNM		-0	/* for debugging, set up at call 	*/
#define	REDOIP		-1	/* in this proc, set up at clause entry	*/
#define	PREVCP		-2	/* prev cp on stack, set up at call	*/
#define	SAVEHP		-3	/* in calling proc, set up at call	*/
#define	SUCCIP		-3	/* in calling proc, set up at call	*/
#define	SUCCCP		-4	/* cp of calling proc, set up at call	*/
#define	SAVEVAL		-5	/* saved values start at this offset	*/

#define	bt_prednm(curcp)	((const char *) curcp[PREDNM])
#define	bt_redoip(curcp)	((Code *) curcp[REDOIP])
#define	bt_prevcp(curcp)	((Word *) curcp[PREVCP])
#define	bt_savehp(curcp)	((Code *) curcp[SAVEHP])
#define	bt_succip(curcp)	((Code *) curcp[SUCCIP])
#define	bt_succcp(curcp)	((Word *) curcp[SUCCCP])
#define	bt_var(curcp,n)		curcp[SAVEVAL-n]

/* the offsets used by choice points */
#define	cpprednm	bt_prednm(curcp)
#define	cpredoip	bt_redoip(curcp)
#define	cpprevcp	bt_prevcp(curcp)
#define	cpsuccip	bt_succip(curcp)
#define	cpsucccp	bt_succcp(curcp)
#define	cpvar(n)	bt_var(curcp,n)

/* the offsets used by reclaim points */
#define	recprednm	bt_prednm(maxcp)
#define	recredoip	bt_redoip(maxcp)
#define	recprevcp	bt_prevcp(maxcp)
#define	recsavehp	bt_savehp(maxcp)

#define	RECLAIM_SIZE	4	/* units: words */

/* DEFINITIONS FOR MANIPULATING THE CHOICE POINT STACK */

#define	mkcp(prednm, n, redoip)					\
			do {					\
				reg	Word	*prevcp;	\
				reg	Word	*succcp;	\
								\
				prevcp = maxcp;			\
				succcp = curcp;			\
				maxcp += (-SAVEVAL + n);	\
				curcp = maxcp;			\
				curcp[PREDNM] = (Word) prednm;	\
				curcp[REDOIP] = (Word) redoip;	\
				curcp[PREVCP] = (Word) prevcp;	\
				curcp[SUCCIP] = (Word) succip;	\
				curcp[SUCCCP] = (Word) succcp;	\
				debugmkcp();			\
				cpstack_overflow_check();	\
			} while (0)

#define	mkreclaim(prednm)					\
			do {					\
				reg	Word	*prevcp;	\
								\
				prevcp = maxcp;			\
				maxcp += 4;			\
				maxcp[PREDNM] = (Word) prednm;	\
				maxcp[REDOIP] = (Word) doresethpfail;\
				maxcp[PREVCP] = (Word) prevcp;	\
				maxcp[SAVEHP] = (Word) hp;	\
				debugmkreclaim();		\
				cpstack_overflow_check();	\
			} while (0)

#define	modcp(redoip)	do {					\
				cpredoip = redoip;		\
				debugmodcp();			\
			} while (0)

#define	succeed()	do {					\
				debugsucceed();			\
				childcp = curcp;		\
				curcp = cpsucccp;		\
				goto * bt_succip(childcp);	\
			} while (0)

#define	fail()		do {					\
				debugfail();			\
				maxcp = cpprevcp;		\
				curcp = maxcp;			\
				cpstack_underflow_check();	\
				goto *cpredoip;			\
			} while (0)

#define	redo()		do {					\
				debugredo();			\
				curcp = maxcp;			\
				goto *cpredoip;			\
			} while (0)

#define	slowneg_setup(XXX)					\
			do {					\
				/* XXX */			\
			} while (0)

#define	fastneg_setup(XXX)					\
			do {					\
				push(cpredoip);			\
				push(maxcp);			\
				/* XXX */			\
			} while (0)

/* DEFINITIONS FOR OVERFLOW CHECKS */

#ifdef	SPEED

#define	heap_overflow_check()					\
			do { } while (0)
#define	stack_overflow_check()					\
			do { } while (0)
#define	stack_underflow_check()					\
			do { } while (0)
#define	cpstack_overflow_check()				\
			do { } while (0)
#define	cpstack_underflow_check()				\
			do { } while (0)
#else

#define	heap_overflow_check()					\
			do {					\
				if (hp >= &heap[MAXHEAP])	\
				{				\
					printf("heap overflow\n");\
					exit(1);		\
				}				\
				if (hp > heapmax)		\
					heapmax = hp;		\
			} while (0)

#define	stack_overflow_check()					\
			do {					\
				if (sp >= &stack[MAXSTACK])	\
				{				\
					printf("stack overflow\n");\
					exit(1);		\
				}				\
				if (sp > stackmax)		\
					stackmax = sp;		\
			} while (0)

#define	stack_underflow_check()					\
			do {					\
				if (sp < stackmin)		\
				{				\
					printf("stack underflow\n");\
					exit(1);		\
				}				\
			} while (0)

#define	cpstack_overflow_check()				\
			do {					\
				if (maxcp >= &cpstack[MAXCPSTACK])	\
				{				\
					printf("cpstack overflow\n");\
					exit(1);		\
				}				\
				if (maxcp > cpstackmax)		\
					cpstackmax = maxcp;	\
			} while (0)

#define	cpstack_underflow_check()				\
			do {					\
				if (maxcp < cpstackmin)		\
				{				\
					printf("cpstack underflow\n");\
					exit(1);		\
				}				\
			} while (0)

#endif

/* DEFINITIONS FOR DEBUGGING MESSAGES */

#ifdef	SPEED

#define	debugcr1(val0, hp)					\
			do { } while (0)
#define	debugcr2(val0, val1, hp)				\
			do { } while (0)
#define	debugpush(val, sp)					\
			do { } while (0)
#define	debugpop(val, sp)					\
			do { } while (0)
#define	debugregs(msg)						\
			do { } while (0)
#define	debugframe(msg)						\
			do { } while (0)
#define	debugmkcp()						\
			do { } while (0)
#define	debugmkreclaim()					\
			do { } while (0)
#define	debugmodcp()						\
			do { } while (0)
#define	debugsucceed()						\
			do { } while (0)
#define	debugfail()						\
			do { } while (0)
#define	debugredo()						\
			do { } while (0)
#define	debugcall(proc, succcont)				\
			do { } while (0)
#define	debugtailcall(proc)					\
			do { } while (0)
#define	debugproceed()						\
			do { } while (0)
#define	debugmsg0(msg)						\
			do { } while (0)
#define	debugmsg1(msg, arg1)					\
			do { } while (0)
#define	debugmsg2(msg, arg1, arg2)				\
			do { } while (0)
#define	debugmsg3(msg, arg1, arg2, arg3)			\
			do { } while (0)

#else

#define	debugcr1(val0, hp)					\
			do {					\
				if (heapdebug)			\
					cr1_msg(val0, hp);	\
			} while (0)

#define	debugcr2(val0, val1, hp)				\
			do {					\
				if (heapdebug)			\
					cr2_msg(val0, val1, hp);\
			} while (0)

#define	debugpush(val, sp)					\
			do {					\
				if (stackdebug)			\
					push_msg(val, sp);	\
			} while (0)

#define	debugpop(val, sp)					\
			do {					\
				if (stackdebug)			\
					pop_msg(val, sp);	\
			} while (0)

#define	debugregs(msg)	printregs(msg)

#define	debugframe(msg)	printframe(msg)

#define	debugmkcp()	do {					\
				if (cpstackdebug)		\
					mkcp_msg();		\
			} while (0)

#define	debugmkreclaim()					\
			do {					\
				if (cpstackdebug)		\
					mkreclaim_msg();	\
			} while (0)

#define	debugmodcp()	do {					\
				if (cpstackdebug)		\
					modcp_msg();		\
			} while (0)

#define	debugsucceed()	do {					\
				if (cpstackdebug)		\
					succeed_msg();		\
			} while (0)

#define	debugfail()	do {					\
				if (cpstackdebug)		\
					fail_msg();		\
			} while (0)

#define	debugredo()	do {					\
				if (cpstackdebug)		\
					redo_msg();		\
			} while (0)

#define	debugcall(proc, succcont)				\
			do {					\
				if (calldebug)			\
					call_msg(proc, succcont);\
			} while (0)

#define	debugtailcall(proc)					\
			do {					\
				if (calldebug)			\
					tailcall_msg(proc);	\
			} while (0)

#define	debugproceed()	do {					\
				if (calldebug)			\
					proceed_msg();		\
			} while (0)

#define	debugmsg0(msg)	do {					\
				printf(msg);			\
			} while (0)

#define	debugmsg1(msg, arg1)					\
			do {					\
				printf(msg, arg1);		\
			} while (0)

#define	debugmsg2(msg, arg1, arg2)				\
			do {					\
				printf(msg, arg1, arg2);	\
			} while (0)

#define	debugmsg3(msg, arg1, arg2, arg3)			\
			do {					\
				printf(msg, arg1, arg2, arg3);	\
			} while (0)

#endif

/* DEFINITIONS FOR SYSTEM INITIALIZATION */

extern	int	r1val;
extern	int	r2val;
extern	int	r3val;
extern	int	repcounter;

extern	void	list_module(void);
extern	void	nrev_module(void);
extern	void	length_module(void);
extern	void	aclength_module(void);
extern	void	member_module(void);
extern	void	memdet_module(void);
extern	void	mklist_module(void);
extern	void	ab_module(void);
extern	void	back_module(void);
extern	void	neg_module(void);
extern	void	heap_module(void);
extern	void	color_module(void);

extern	Word	mklist(int start, int len);

/* DEFINITIONS TO SUPPORT DEBUGGING */

/*
** For each entry point, a table function pointers which indicate
** which function should be called by debugregs() to print out
** each register.
*/

typedef void PrintRegFunc(Word);
extern	PrintRegFunc * regtable[MAXENTRIES][32];

extern	bool	calldebug;
extern	bool	heapdebug;
extern	bool	stackdebug;
extern	bool	cpstackdebug;
extern	bool	detaildebug;

/* debugging messages, defined in aux.c */

extern	void	mkcp_msg(void);
extern	void	mkreclaim_msg(void);
extern	void	modcp_msg(void);
extern	void	succeed_msg(void);
extern	void	fail_msg(void);
extern	void	redo_msg(void);
extern	void	call_msg(const Code *proc, const Code *succcont);
extern	void	tailcall_msg(const Code *proc);
extern	void	proceed_msg(void);
extern	void	cr1_msg(Word val0, const Word *addr);
extern	void	cr2_msg(Word val0, Word val1, const Word *addr);
extern	void	push_msg(Word val, const Word *addr);
extern	void	pop_msg(Word val, const Word *addr);

/* more debugging messages, defined in aux.c */

extern	void	printregs(const char *msg);
extern	void	printtmps(void);
extern	void	printint(Word n);
extern	void	printstring(char *s);
extern	void	printheap(const Word *h);
extern	void	printstack(const Word *s);
extern	void	printcpstack(const Word *s);
extern	void	printlist(Word p);
extern	void	printlabel(const Code *w);
extern	void	printregs(const char *);
extern	void	printframe(const char *);
extern	void	dumpframe(Word *);
extern	void	dumpcpstack(void);
