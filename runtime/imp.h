#include	<stdio.h>
#include	<assert.h>
#include	"getopt.h"
#include	"std.h"

typedef	uint	Word;
typedef void	Code;

typedef struct s_entry
{
	const char	*e_name;	/* the name of the procedure	*/
	Code		*e_addr;	/* the address of the code	*/
	Code		*e_input;	/* the address of the input generator */
} Entry;

/* when you modify this, modify the table in aux.c as well */
enum {
	APPEND_1,
	APPEND_2,
	NREV_1,
	LENGTH_1,
	LENGTH_2,
	ACLENGTH_1,
	MEMBER_1,
	MEMBER_2,
	MKLIST_1,
	Q_1,
	NEG_NOT_Q,
	INT_1,
	HEAP_1,
	ONETEN_1,
	MEMDET_1,
	MAXENTRIES
};

#define	STARTLABELS	40
#define	MAXLABELS	400

reg	Word	r0 __asm("s0");
reg	Word	r1 __asm("s1");
reg	Word	r2 __asm("s2");
reg	Word	r3 __asm("s3");
reg	Word	r4 __asm("s4");
reg	Word	r5 __asm("s5");
reg	Word	r6 __asm("s6");
reg	Word	r7 __asm("s7");

extern	Word	tmp0;
extern	Word	tmp1;
extern	Word	tmp2;
extern	Word	tmp3;
extern	Word	tmp4;
extern	Word	tmp5;
extern	Word	tmp6;
extern	Word	tmp7;

#define	hp	((Word *) r6)
#define	sp	((Word *) r7)

#define	succip	((Code *) r0)
#define	childcp	((Word *) tmp5)
#define	curcp	((Word *) tmp6)
#define	maxcp	((Word *) tmp7)

#define	MAXHEAP		0x10000
#define	MAXSTACK	0x10000
#define	MAXCPSTACK	0x10000
#define	CACHE_OFFSET	0x200

extern	int	cur_entry;
extern	Word	heap[];
extern	Word	stack[];
extern	Word	cpstack[];
extern	bool	calldebug;
extern	bool	heapdebug;
extern	bool	stackdebug;
extern	bool	cpstackdebug;
extern	bool	detaildebug;
extern	Word	*heapmax;
extern	Word	*stackmax;
extern	Word	*cpstackmax;
extern	Word	*heapmin;
extern	Word	*stackmin;
extern	Word	*cpstackmin;

/* for use only during input setup */
extern	int	r1val;
extern	int	r2val;
extern	int	r3val;
extern	int	repcounter;

extern	Code	*dofail;
extern	Code	*doresethpfail;
extern	Code	*doresetcpvar0fail;
extern	Code	*doredo;

#define	WORDSIZE	4

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

/* these offsets and used by both choice points and reclaim points */

#define	PREDNM		-0
#define	REDOIP		-1	/* in this proc, set up at clause entry	*/
#define	PREVCP		-2	/* prev cp on stack, set up at call	*/
#define	SAVEHP		-3	/* in calling proc, set up at call	*/
#define	SUCCIP		-3	/* in calling proc, set up at call	*/
#define	SUCCCP		-4	/* cp of calling proc, set up at call	*/
#define	SAVEVAL		-5

/* the offsets used by choice points */
#define	cpprednm	(const char *) curcp[PREDNM]
#define	cpredoip	(Code *) curcp[REDOIP]
#define	cpprevcp	(Word *) curcp[PREVCP]
#define	cpsuccip	(Code *) curcp[SUCCIP]
#define	cpsucccp	(Word *) curcp[SUCCCP]
#define	cpvar(n)	curcp[SAVEVAL-n]

/* the offsets used by reclaim points */
#define	recprednm	(const char *) maxcp[PREDNM]
#define	recredoip	(Code *) maxcp[REDOIP]
#define	recprevcp	(Word *) maxcp[PREVCP]
#define	recsavehp	(Word *) maxcp[SAVEHP]

#define	RECLAIM_SIZE	4				/* units: words */

#define	stackvar(n)	sp[-n]

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
				goto * (Code *) childcp[SUCCIP];\
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
#define	heap_cr1_msg(val0, hp)					\
			do { } while (0)
#define	heap_cr2_msg(val0, val1, hp)				\
			do { } while (0)
#define	stack_push_msg(val, sp)					\
			do { } while (0)
#define	stack_pop_msg(val, sp)					\
			do { } while (0)
#define	debugregs(msg)						\
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
#define	heap_cr1_msg(val0, hp)					\
			do {					\
				if (heapdebug)			\
					printf("put %x at %x\n",\
					val0, hp);		\
			} while (0)
#define	heap_cr2_msg(val0, val1, hp)				\
			do {					\
				if (heapdebug)			\
					printf("put %x,%x at %p\n",\
					val0, val1, hp);\
			} while (0)
#define	stack_push_msg(val, sp)					\
			do {					\
				if (stackdebug)			\
					push_msg(val, sp);	\
			} while (0)
#define	stack_pop_msg(val, sp)					\
			do {					\
				if (stackdebug)			\
					pop_msg(val, sp);	\
			} while (0)
#define	debugregs(msg)						\
			printregs(msg)
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

#define	deref(p)	({					\
				Word pt;			\
								\
				pt = p;				\
				while (tag(pt) == TAG_VAR)	\
					pt = * (Word *)		\
						body(pt, TAG_VAR);\
				pt;				\
			})

#define create1(w1)	({					\
				Word p;				\
								\
				hp[0] = (Word) (w1);		\
				heap_cr1_msg(hp[0], hp);	\
				p = (Word) hp;			\
				hp += 1;			\
				heap_overflow_check();		\
				p;				\
			})

#define create2(w1, w2)	({					\
				Word p;				\
								\
				hp[0] = (Word) (w1);		\
				hp[1] = (Word) (w2);		\
				heap_cr2_msg(hp[0], hp[1], hp);	\
				p = (Word) hp;			\
				hp += 2;			\
				heap_overflow_check();		\
				p;				\
			})

#define	push(w)		do {					\
				*sp = (Word) (w);		\
				stack_push_msg(*sp, sp);	\
				sp += 1;			\
				stack_overflow_check();		\
			} while (0)

#define	pop()		({					\
				Word w;				\
								\
				sp -= 1;			\
				w = *sp;			\
				stack_pop_msg(*sp, sp);		\
				stack_underflow_check();	\
				/* return */ w;			\
			})

extern	int	which;

/* A table of the entry points defined by the various modules */

extern	Entry	entries[];

/*
** For each entry point, a table function pointers which indicate
** which function should be called by debugregs() to print out
** each register.
*/

typedef void PrintRegFunc(Word);
extern	PrintRegFunc * regtable[MAXENTRIES][16];

/* prototypes for the modules */

extern	void	list_module(void);
extern	void	nrev_module(void);
extern	void	length_module(void);
extern	void	aclength_module(void);
extern	void	member_module(void);
extern	void	mklist_module(void);
extern	void	ab_module(void);
extern	void	ex_module(void);
extern	void	back_module(void);
extern	void	neg_module(void);
extern	void	heap_module(void);

/* old (obsolete?) */

extern	Word	mklist(int start, int len);
extern	void	mkinput(int r1val, int r2val, int r3val);

/* Debugging messages, defined in aux.c */

extern	void	mkcp_msg(void);
extern	void	mkreclaim_msg(void);
extern	void	modcp_msg(void);
extern	void	succeed_msg(void);
extern	void	fail_msg(void);
extern	void	redo_msg(void);
extern	void	call_msg(const Word *proc, const Word *succcont);
extern	void	tailcall_msg(const Word *proc);
extern	void	proceed_msg(void);
extern	void	push_msg(Word val, const Word *addr);
extern	void	pop_msg(Word val, const Word *addr);

/* More debugging messages, defined in aux.c */

extern	void	printregs(const char *msg);
extern	void	printtmps(void);
extern	void	printint(Word n);
extern	void	printheap(Word h);
extern	void	printstack(Word s);
extern	void	printcpstack(Word s);
extern	void	printlist(Word p);
extern	void	printlabel(Word w);
extern	void	printregs(const char *);
extern	void	dumpcpstack(void);

