#ifndef IMP_H
#define IMP_H

#ifdef SPEED
/* turn off `assert()'s */
#define NDEBUG
#endif

#include	<stdio.h>
#include	<assert.h>
#include	"getopt.h"
#include	"std.h"

/* GENERAL DEFINITIONS */

typedef	uint	Word;
typedef void	Code;

#define	WORDSIZE	sizeof(Word)

#define IF(cond, val)	((cond) ? ((val),(void)0) : (void)0)

/* DEFINITIONS FOR THE LABEL TABLE */

typedef struct s_label
{
	const char	*e_name;   /* name of the procedure	     */
	Code		*e_addr;   /* address of the code	     */
} Label;

#define	MAXLABELS	800

#define	makeentry(n, a)						\
			(					\
				assert(cur_entry >= 0),		\
				assert(cur_entry < MAXLABELS), 	\
				entries[cur_entry].e_name  = n,	\
				entries[cur_entry].e_addr  = a,	\
				cur_entry += 1,			\
				((void)0)			\
			)

/* taking the address of labels can inhibit gcc's optimization,
   because it assumes that anything can jump there,
   so we want to do so only if we're debugging */
#ifdef SPEED
#define makelabel(n, a)	/* nothing */
#else
#define	makelabel(n,a)	makeentry((n),(a))
#endif

/* a table of the entry points defined by the various modules */
/* TODO: replace this with a hash table */

extern	Label	entries[];
extern	int	cur_entry;	/* next free slot in entries table   */
extern	int	which;		/* procedure called from interpreter */

/* DEFINITIONS FOR WORD LAYOUT */

#include "tags.h"

/* DEFINITIONS FOR VIRTUAL MACHINE REGISTERS */

#include "regs.h"

/* DEFINITIONS FOR THE "PORTABLE ASSEMBLER" NON-LOCAL GOTOS */

#define paste(a,b) a##b

#ifdef USE_GCC_NONLOCAL_GOTOS
  #ifndef __GNUC__
  #error "You must use gcc if you define USE_GCC_NONLOCAL_GOTOS"
  #endif
  typedef void *EntryPoint;
  #define ENTRY(predname) 	paste(entry_,predname)
  #define LABEL(label)		(&&label)
  #define GOTO(label)		do { debuggoto(label); goto *(label); } while(0)
	  /*
	  ** GOTO_LABEL(label) is the same as GOTO(LABEL(label)) except
	  ** that it may allow gcc to generate slightly better code
	  */
  #define GOTO_LABEL(label) 	do { debuggoto(&&label); goto label; } while(0)
#else
  typedef Code *EntryPoint(void);
  #define ENTRY(predname) 	predname
  #define LABEL(label)		(label)
  #define GOTO(label)		do { debuggoto(label); return (label); } while(0)
  #define GOTO_LABEL(label) 	GOTO(LABEL(label))
#endif

/* STANDARD ENTRY POINTS */

/* these #defines are just for backwards compatibility with old .mod files */

#define doredo 			ENTRY(do_redo)
#define dofail 			ENTRY(do_fail)
#define doresethpfail		ENTRY(do_reset_hp_fail)
#define doresetcpvar0fail	ENTRY(do_reset_cpvar0_fail)
#define dosucceed		ENTRY(do_succeed)
#define doslownegfail		ENTRY(do_slowneg_fail)
#define doslownegsucceed	ENTRY(do_slowneg_succeed)
#define dofastnegredo		ENTRY(do_fastneg_redo)
#define dofastnegproceed	ENTRY(do_fastneg_proceed)

extern	EntryPoint	doredo;
extern	EntryPoint	dofail;
extern	EntryPoint	doresethpfail;
extern	EntryPoint	doresetcpvar0fail;
extern	EntryPoint	dosucceed;
extern	EntryPoint	doslownegfail;
extern	EntryPoint	doslownegsucceed;
extern	EntryPoint	dofastnegredo;
extern	EntryPoint	dofastnegproceed;

/* DEFINITIONS FOR CALLS AND RETURNS */

#define	call(proc, succcont)					\
			do {					\
				debugcall((proc), (succcont));	\
				succip = (succcont);		\
				GOTO(proc);			\
			} while (0)

#define	callentry(procname, succcont)				\
			do {					\
				extern EntryPoint ENTRY(procname); \
				call(ENTRY(procname), succcont); \
			} while (0)

#define	tailcall(proc)	do {					\
				debugtailcall(proc);		\
				GOTO(proc);			\
			} while (0)

#define	tailcallentry(procname)					\
			do {					\
				extern EntryPoint ENTRY(procname); \
				tailcall(ENTRY(procname));	\
			} while (0)

#define	proceed()	do {					\
				debugproceed();			\
				GOTO(succip);			\
			} while (0)

/* DEFINITIONS FOR VIRTUAL MACHINE DATA AREAS */

#define	MAXHEAP		0x100000
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

#define	incr_hp(n)	(					\
				hp += (n),			\
				heap_overflow_check(),		\
				(void)0				\
			)

/* Note that gcc optimizes `hp += 2; return hp - 2;' to
   `tmp = hp; hp += 2; return tmp;', so we don't need to
   use gcc's expression statements here */

#define create1(w1)	(					\
				hp[0] = (Word) (w1),		\
				debugcr1(hp[0], hp),		\
				hp += 1,			\
				heap_overflow_check(),		\
				/* return */ (Word) (hp - 1)	\
			)

#define create2(w1, w2)	(					\
				hp[0] = (Word) (w1),		\
				hp[1] = (Word) (w2),		\
				debugcr2(hp[0], hp[1], hp),	\
				hp += 2,			\
				heap_overflow_check(),		\
				/* return */ (Word) (hp - 2)	\
			)

/* DEFINITIONS FOR MANIPULATING THE STACK */

#define	stackvar(n)	sp[-n]

#define	incr_sp(n)	(					\
				sp += (n),			\
				stack_overflow_check(),		\
				(void)0				\
			)

#define	decr_sp(n)	(					\
				sp -= (n),			\
				stack_underflow_check(),	\
				(void)0				\
			)


#define	push(w)		(					\
				*sp = (Word) (w),		\
				debugpush(*sp, sp),		\
				sp += 1,			\
				stack_overflow_check(),		\
				(void)0				\
			)

#define	pop()		(					\
				sp -= 1,			\
				debugpop(*sp, sp),		\
				stack_underflow_check(),	\
				/* return */ *sp		\
			)

/* DEFINITIONS FOR CHOICE AND RECLAIM POINTS */

#define	PREDNM		(-0)	/* for debugging, set up at call 	*/
#define	REDOIP		(-1)	/* in this proc, set up at clause entry	*/
#define	PREVCP		(-2)	/* prev cp on stack, set up at call	*/
#define	SAVEHP		(-3)	/* in calling proc, set up at call	*/
#define	SUCCIP		(-3)	/* in calling proc, set up at call	*/
#define	SUCCCP		(-4)	/* cp of calling proc, set up at call	*/
#define	SAVEVAL		(-5)	/* saved values start at this offset	*/

#define	bt_prednm(curcp)	LVALUE_CAST(const char *, curcp[PREDNM])
#define	bt_redoip(curcp)	LVALUE_CAST(Code *, curcp[REDOIP])
#define	bt_prevcp(curcp)	LVALUE_CAST(Word *, curcp[PREVCP])
#define	bt_savehp(curcp)	LVALUE_CAST(Code *, curcp[SAVEHP])
#define	bt_succip(curcp)	LVALUE_CAST(Code *, curcp[SUCCIP])
#define	bt_succcp(curcp)	LVALUE_CAST(Word *, curcp[SUCCCP])
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

#define	RECLAIM_SIZE		4	/* units: words */
#define	CHOICE_POINT_SIZE	5	/* units: words */

/* DEFINITIONS FOR MANIPULATING THE CHOICE POINT STACK */

#define	mkcp(prednm, n, redoip)					\
			do {					\
				reg	Word	*prevcp;	\
				reg	Word	*succcp;	\
								\
				prevcp = maxcp;			\
				succcp = curcp;			\
				maxcp += (CHOICE_POINT_SIZE + n);	\
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
				maxcp[REDOIP] = (Word) doresethpfail;	\
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
				GOTO(bt_succip(childcp));	\
			} while (0)

#define	fail()		do {					\
				debugfail();			\
				maxcp = cpprevcp;		\
				curcp = maxcp;			\
				cpstack_underflow_check();	\
				GOTO(cpredoip);			\
			} while (0)

#define	redo()		do {					\
				debugredo();			\
				curcp = maxcp;			\
				GOTO(cpredoip);			\
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

#define	heap_overflow_check()		((void)0)
#define	stack_overflow_check()		((void)0)
#define	stack_underflow_check()		((void)0)
#define	cpstack_overflow_check()	((void)0)
#define	cpstack_underflow_check()	((void)0)

#else

#define	heap_overflow_check()					\
			(					\
				IF (hp >= &heap[MAXHEAP],(	\
					printf("heap overflow\n"),	\
					exit(1)			\
				)),				\
				IF (hp > heapmax,(		\
					heapmax = hp		\
				)),				\
				(void)0				\
			)

#define	stack_overflow_check()					\
			(					\
				IF (sp >= &stack[MAXSTACK],(	\
					printf("stack overflow\n"),	\
					exit(1)			\
				)),				\
				IF (sp > stackmax,(		\
					stackmax = sp		\
				)),				\
				(void)0				\
			)

#define	stack_underflow_check()					\
			(					\
				IF (sp < stackmin,(		\
					printf("stack underflow\n"),	\
					exit(1)			\
				)),				\
				(void)0				\
			)

#define	cpstack_overflow_check()				\
			(					\
				IF (maxcp >= &cpstack[MAXCPSTACK],(	\
					printf("cpstack overflow\n"),	\
					exit(1)			\
				)),				\
				IF (maxcp > cpstackmax,(	\
					cpstackmax = maxcp	\
				)),				\
				(void)0				\
			)

#define	cpstack_underflow_check()				\
			(					\
				IF (maxcp < cpstackmin,(	\
					printf("cpstack underflow\n"),	\
					exit(1)			\
				)),				\
				(void)0				\
			)

#endif

/* DEFINITIONS FOR DEBUGGING MESSAGES */

#ifdef	SPEED

#define	debugcr1(val0, hp)			((void)0)
#define	debugcr2(val0, val1, hp)		((void)0)
#define	debugpush(val, sp)			((void)0)
#define	debugpop(val, sp)			((void)0)
#define	debugregs(msg)				((void)0)
#define	debugframe(msg)				((void)0)
#define	debugmkcp()				((void)0)
#define	debugmkreclaim()			((void)0)
#define	debugmodcp()				((void)0)
#define	debugsucceed()				((void)0)
#define	debugfail()				((void)0)
#define	debugredo()				((void)0)
#define	debugcall(proc, succcont)		((void)0)
#define	debugtailcall(proc)			((void)0)
#define	debugproceed()				((void)0)
#define	debuggoto(label)			((void)0)
#define	debugmsg0(msg)				((void)0)
#define	debugmsg1(msg, arg1)			((void)0)
#define	debugmsg2(msg, arg1, arg2)		((void)0)
#define	debugmsg3(msg, arg1, arg2, arg3)	((void)0)

#else

#define	debugcr1(val0, hp) \
	IF (heapdebug, (save_registers(), cr1_msg(val0, hp)))

#define	debugcr2(val0, val1, hp) \
	IF (heapdebug, (save_registers(), cr2_msg(val0, val1, hp)))

#define	debugpush(val, sp) \
	IF (stackdebug, (save_registers(), push_msg((val), (sp))))

#define	debugpop(val, sp) \
	IF (stackdebug, (save_registers(), pop_msg(val, sp)))

#define	debugregs(msg)	(save_registers(), printregs(msg))

#define	debugmkcp() \
	IF (cpstackdebug, (save_registers(), mkcp_msg()))

#define	debugframe(msg)	(save_registers(), printframe(msg))

#define	debugmkreclaim() \
	IF (cpstackdebug, (save_registers(), mkreclaim_msg()))

#define	debugmodcp() \
	IF (cpstackdebug, (save_registers(), modcp_msg()))

#define	debugsucceed() \
	IF (cpstackdebug, (save_registers(), succeed_msg()))

#define	debugfail() \
	IF (cpstackdebug, (save_registers(), fail_msg()))

#define	debugredo() \
	IF (cpstackdebug, (save_registers(), redo_msg()))

#define	debugcall(proc, succcont) \
	IF (calldebug, (save_registers(), call_msg(proc, succcont)))

#define	debugtailcall(proc) \
	IF (calldebug, (save_registers(), tailcall_msg(proc)))

#define	debugproceed() \
	IF (calldebug, (save_registers(), proceed_msg()))

#define	debuggoto(label) \
	(assert(label), \
	IF (gotodebug, (save_registers(), goto_msg(label))))

#define	debugmsg0(msg) \
	printf(msg)

#define	debugmsg1(msg, arg1) \
	printf(msg, arg1)

#define	debugmsg2(msg, arg1, arg2) \
	printf(msg, arg1, arg2)

#define	debugmsg3(msg, arg1, arg2, arg3) \
	printf(msg, arg1, arg2, arg3)

#endif

/* DEFINITIONS TO SUPPORT DEBUGGING */

extern	int	r1val;
extern	int	r2val;
extern	int	r3val;
extern	int	repcounter;

extern	Word	do_mklist(int start, int len);
#ifdef __GNUC__
#define mklist(start,len) \
	({						\
		Word tmp;				\
		save_registers();			\
		tmp = do_mklist(start,len);		\
		restore_registers();			\
		/* return */ tmp;			\
	})
#else
	/*
	** if it's not gcc, then we can't be using global register variables,
	** so we don't need to worry about saving/restoring them
	** (which would have been slightly tricky to do in a portable macro)
	*/
#define mklist(start,len) do_mklist(start,len)
#endif

extern	bool	gotodebug;
extern	bool	calldebug;
extern	bool	heapdebug;
extern	bool	stackdebug;
extern	bool	cpstackdebug;
extern	bool	detaildebug;
extern	bool	finaldebug;

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
extern	void	goto_msg(const Code *addr);

/* more debugging messages, defined in aux.c */

extern	void	printregs(const char *msg);
extern	void	printtmps(void);
extern	void	printint(Word n);
extern	void	printstring(const char *s);
extern	void	printheap(const Word *h);
extern	void	printstack(const Word *s);
extern	void	printcpstack(const Word *s);
extern	void	printlist(Word p);
extern	void	printlabel(const Code *w);
extern	void	printregs(const char *);
extern	void	printframe(const char *);
extern	void	dumpframe(const Word *);
extern	void	dumpcpstack(void);

extern 	void	init_engine(void);
extern 	void	call_engine(Code *);

#endif /* IMP_H */
