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

/* DEFINITIONS FOR DEBUGGING FLAGS */

#define	PROGFLAG	0
#define	GOTOFLAG	1
#define	CALLFLAG	2
#define	HEAPFLAG	3
#define	DETSTACKFLAG	4
#define	NONDSTACKFLAG	5
#define	FINALFLAG	6
#define	DETAILFLAG	7
#define	MAXFLAG		8
/* DETAILFLAG should be the last real flag */

#define	progdebug	debugflag[GOTOFLAG]
#define	gotodebug	debugflag[GOTOFLAG]
#define	calldebug	debugflag[CALLFLAG]
#define	heapdebug	debugflag[HEAPFLAG]
#define	detstackdebug	debugflag[DETSTACKFLAG]
#define	nondstackdebug	debugflag[NONDSTACKFLAG]
#define	finaldebug	debugflag[FINALFLAG]
#define	detaildebug	debugflag[DETAILFLAG]

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
#define doresetframevar0fail	ENTRY(do_reset_framevar0_fail)
#define dosucceed		ENTRY(do_succeed)

extern	EntryPoint	doredo;
extern	EntryPoint	dofail;
extern	EntryPoint	doresethpfail;
extern	EntryPoint	doresetframevar0fail;
extern	EntryPoint	dosucceed;

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

#define	MAXHEAP		0x10000
#define	MAXDETSTACK	0x10000
#define	MAXNONDSTACK	0x10000
#define	CACHE_OFFSET	0x200

extern	Word	heap[];
extern	Word	detstack[];
extern	Word	nondstack[];

extern	Word	*heapmax;
extern	Word	*detstackmax;
extern	Word	*nondstackmax;

extern	Word	*heapmin;
extern	Word	*detstackmin;
extern	Word	*nondstackmin;

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
				hp = hp + 1,			\
				hp[-1] = (Word) (w1),		\
				debugcr1(hp[-1], hp),		\
				heap_overflow_check(),		\
				/* return */ (Word) (hp - 1)	\
			)

#define create2(w1, w2)	(					\
				hp = hp + 2,			\
				hp[-2] = (Word) (w1),		\
				hp[-1] = (Word) (w2),		\
				debugcr2(hp[-2], hp[-1], hp),	\
				heap_overflow_check(),		\
				/* return */ (Word) (hp - 2)	\
			)

#define create2_bf(w1)	(					\
				hp = hp + 2,			\
				hp[-2] = (Word) (w1),		\
				heap_overflow_check(),		\
				/* return */ (Word) (hp - 2)	\
			)

#define create2_fb(w2)	(					\
				hp = hp + 2,			\
				hp[-1] = (Word) (w2),		\
				heap_overflow_check(),		\
				/* return */ (Word) (hp - 2)	\
			)

/* DEFINITIONS FOR MANIPULATING THE STACK */

#define	detstackvar(n)	sp[-n]

#define	incr_sp(n)	(					\
				sp = sp + (n),			\
				detstack_overflow_check(),	\
				(void)0				\
			)

#define	decr_sp(n)	(					\
				sp = sp - (n),			\
				detstack_underflow_check(),	\
				(void)0				\
			)


#define	push(w)		(					\
				*sp = (Word) (w),		\
				debugpush(*sp, sp),		\
				sp = sp + 1,			\
				detstack_overflow_check(),	\
				(void)0				\
			)

#define	pop()		(					\
				sp = sp - 1,			\
				debugpop(*sp, sp),		\
				detstack_underflow_check(),	\
				/* return */ *sp		\
			)

/* DEFINITIONS FOR CHOICE AND RECLAIM POINTS */

#define	PREDNM		(-0)	/* for debugging, set up at call 	*/
#define	REDOIP		(-1)	/* in this proc, set up at clause entry	*/
#define	PREVFR		(-2)	/* prev frame on stack, set up at call	*/
#define	SAVEHP		(-3)	/* in caller proc, set up at call	*/
#define	SUCCIP		(-3)	/* in caller proc, set up at call	*/
#define	SUCCFR		(-4)	/* frame of caller proc, set up at call	*/
#define	SAVEVAL		(-5)	/* saved values start at this offset	*/

#define	bt_prednm(fr)	LVALUE_CAST(const char *, fr[PREDNM])
#define	bt_redoip(fr)	LVALUE_CAST(Code *, fr[REDOIP])
#define	bt_prevfr(fr)	LVALUE_CAST(Word *, fr[PREVFR])
#define	bt_savehp(fr)	LVALUE_CAST(Code *, fr[SAVEHP])
#define	bt_succip(fr)	LVALUE_CAST(Code *, fr[SUCCIP])
#define	bt_succfr(fr)	LVALUE_CAST(Word *, fr[SUCCFR])
#define	bt_var(fr,n)	fr[SAVEVAL-n]

/* the offsets used by choice points */
#define	curprednm	bt_prednm(curfr)
#define	curredoip	bt_redoip(curfr)
#define	curprevfr	bt_prevfr(curfr)
#define	cursuccip	bt_succip(curfr)
#define	cursuccfr	bt_succfr(curfr)
#define	framevar(n)	bt_var(curfr,n)

/* the offsets used by reclaim points */
#define	recprednm	bt_prednm(maxfr)
#define	recredoip	bt_redoip(maxfr)
#define	recprevfr	bt_prevfr(maxfr)
#define	recsavehp	bt_savehp(maxfr)

#define	RECLAIM_SIZE		4	/* units: words */
#define	CHOICE_POINT_SIZE	5	/* units: words */

/* DEFINITIONS FOR MANIPULATING THE CHOICE POINT STACK */

#define	mkframe(prednm, n, redoip)				\
			do {					\
				reg	Word	*prevfr;	\
				reg	Word	*succfr;	\
								\
				prevfr = maxfr;			\
				succfr = curfr;			\
				maxfr = maxfr + (CHOICE_POINT_SIZE + n);\
				curfr = maxfr;			\
				curfr[PREDNM] = (Word) prednm;	\
				curfr[REDOIP] = (Word) redoip;	\
				curfr[PREVFR] = (Word) prevfr;	\
				curfr[SUCCIP] = (Word) succip;	\
				curfr[SUCCFR] = (Word) succfr;	\
				debugmkframe();			\
				nondstack_overflow_check();	\
			} while (0)

#define	mkreclaim(prednm)					\
			do {					\
				reg	Word	*prevfr;	\
								\
				prevfr = maxfr;			\
				maxfr = maxfr + 4;		\
				maxfr[PREDNM] = (Word) prednm;	\
				maxfr[REDOIP] = (Word) doresethpfail;	\
				maxfr[PREVFR] = (Word) prevfr;	\
				maxfr[SAVEHP] = (Word) hp;	\
				debugmkreclaim();		\
				nondstack_overflow_check();	\
			} while (0)

#define	modframe(redoip)					\
			do {					\
				curredoip = redoip;		\
				debugmodframe();		\
			} while (0)

#define	succeed()	do {					\
				reg	Word	*childfr;	\
								\
				debugsucceed();			\
				childfr = curfr;		\
				curfr = cursuccfr;		\
				GOTO(bt_succip(childfr));	\
			} while (0)

#define	succeed_discard()					\
			do {					\
				reg	Word	*childfr;	\
								\
				debugsucceeddiscard();		\
				childfr = curfr;		\
				maxfr = curprevfr;		\
				curfr = cursuccfr;		\
				GOTO(bt_succip(childfr));	\
			} while (0)

#define	fail()		do {					\
				debugfail();			\
				maxfr = curprevfr;		\
				curfr = maxfr;			\
				nondstack_underflow_check();	\
				GOTO(curredoip);		\
			} while (0)

#define	redo()		do {					\
				debugredo();			\
				curfr = maxfr;			\
				GOTO(curredoip);		\
			} while (0)

/* DEFINITIONS FOR OVERFLOW CHECKS */

#ifdef	SPEED

#define	heap_overflow_check()		((void)0)
#define	detstack_overflow_check()	((void)0)
#define	detstack_underflow_check()	((void)0)
#define	nondstack_overflow_check()	((void)0)
#define	nondstack_underflow_check()	((void)0)

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

#define	detstack_overflow_check()				\
			(					\
				IF (sp >= &detstack[MAXDETSTACK],(	\
					printf("stack overflow\n"),	\
					exit(1)			\
				)),				\
				IF (sp > detstackmax,(		\
					detstackmax = sp	\
				)),				\
				(void)0				\
			)

#define	detstack_underflow_check()				\
			(					\
				IF (sp < detstackmin,(		\
					printf("stack underflow\n"),	\
					exit(1)			\
				)),				\
				(void)0				\
			)

#define	nondstack_overflow_check()				\
			(					\
				IF (maxfr >= &nondstack[MAXNONDSTACK],(	\
					printf("nondstack overflow\n"),	\
					exit(1)			\
				)),				\
				IF (maxfr > nondstackmax,(	\
					nondstackmax = maxfr	\
				)),				\
				(void)0				\
			)

#define	nondstack_underflow_check()				\
			(					\
				IF (maxfr < nondstackmin,(	\
					printf("nondstack underflow\n"),	\
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
#define	debugmkframe()				((void)0)
#define	debugmkreclaim()			((void)0)
#define	debugmodframe()				((void)0)
#define	debugsucceed()				((void)0)
#define	debugsucceeddiscard()			((void)0)
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
	IF (detstackdebug, (save_registers(), push_msg((val), (sp))))

#define	debugpop(val, sp) \
	IF (detstackdebug, (save_registers(), pop_msg(val, sp)))

#define	debugregs(msg) \
	IF (progdebug, (save_registers(), printregs(msg)))

#define	debugmkframe() \
	IF (nondstackdebug, (save_registers(), mkframe_msg()))

#define	debugframe(msg)	 \
	IF (progdebug, (save_registers(), printframe(msg)))

#define	debugmkreclaim() \
	IF (nondstackdebug, (save_registers(), mkreclaim_msg()))

#define	debugmodframe() \
	IF (nondstackdebug, (save_registers(), modframe_msg()))

#define	debugsucceed() \
	IF (nondstackdebug, (save_registers(), succeed_msg()))

#define	debugsucceeddiscard() \
	IF (nondstackdebug, (save_registers(), succeeddiscard_msg()))

#define	debugfail() \
	IF (nondstackdebug, (save_registers(), fail_msg()))

#define	debugredo() \
	IF (nondstackdebug, (save_registers(), redo_msg()))

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
	IF (progdebug, (printf(msg)))

#define	debugmsg1(msg, arg1) \
	IF (progdebug, (printf(msg, arg1)))

#define	debugmsg2(msg, arg1, arg2) \
	IF (progdebug, (printf(msg, arg1, arg2)))

#define	debugmsg3(msg, arg1, arg2, arg3) \
	IF (progdebug, (printf(msg, arg1, arg2, arg3)))

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

extern	bool	debugflag[];

/* debugging messages, defined in aux.c */

extern	void	mkframe_msg(void);
extern	void	mkreclaim_msg(void);
extern	void	modframe_msg(void);
extern	void	succeed_msg(void);
extern	void	succeeddiscard_msg(void);
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
extern	void	printdetstack(const Word *s);
extern	void	printnondstack(const Word *s);
extern	void	printlist(Word p);
extern	void	printlabel(const Code *w);
extern	void	printregs(const char *);
extern	void	printframe(const char *);
extern	void	dumpframe(const Word *);
extern	void	dumpnondstack(void);
extern	int	whichlabel(const char *name);

extern 	void	init_engine(void);
extern 	void	call_engine(Code *);

#endif /* IMP_H */
