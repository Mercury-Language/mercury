#ifndef IMP_H
#define IMP_H

#ifdef SPEED
/* turn off `assert()'s */
#define NDEBUG
#endif

#include	<stdio.h>
#include	<stdlib.h>
#include	<assert.h>
#include	<sys/types.h>
#include	"std.h"
#include	"conf.h"

/* GENERAL DEFINITIONS */

typedef	uint	Word;
typedef int	Integer;
typedef void	Code;		/* should be `typedef function_t Code' */

/* Note that we require sizeof(Word) == sizeof(Integer) == sizeof(Code*) */

#define	WORDSIZE	sizeof(Word)

#include	"tags.h"
#include	"regs.h"

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

/* DEFINITIONS FOR COMPUTED GOTOS */

#define COMPUTED_GOTO(val, labels) 			\
	{ static Code *jump_table[] = {			\
		labels					\
	  };						\
	  GOTO(jump_table[val]);			\
	}
#define AND ,	/* used to separate the labels */


#ifdef __GNUC__

/*
** Note that hash_string is also defined in compiler/string.nl and in
** code/aux.c.  The three definitions must be kept equivalent.
*/

#define hash_string(s)					\
	({ int len = 0;					\
	   int hash = 0;				\
	   while(((char *)s)[len]) {			\
		hash ^= (hash << 5);			\
		hash ^= ((char *)s)[len];		\
		len++;					\
	   }						\
	   hash ^= len;					\
	   hash;					\
	})
#else
extern	int	hash_string(const char *);
#endif

#include	"engine.h"

/* DEFINITIONS FOR CALLS AND RETURNS */

#define	localcall(label, succcont)				\
			do {					\
				debugcall(LABEL(label), (succcont)); \
				succip = (succcont);		\
				GOTO_LABEL(label);		\
			} while (0)

#define	call(proc, succcont)					\
			do {					\
				debugcall((proc), (succcont));	\
				succip = (succcont);		\
				GOTO(proc);			\
			} while (0)

#define	call_closure(succcont)					\
			do {					\
				Word tmp;			\
				tmp = r1;			\
				switch(field(0, tmp, 0)) {	\
					default: fatal_error("call_closure: too many arguments (> 32)"); \
					case 32: r32 = field(0, tmp, 33); \
					case 31: r31 = field(0, tmp, 32); \
					case 30: r30 = field(0, tmp, 31); \
					case 29: r29 = field(0, tmp, 30); \
					case 28: r28 = field(0, tmp, 29); \
					case 27: r27 = field(0, tmp, 28); \
					case 26: r26 = field(0, tmp, 27); \
					case 25: r25 = field(0, tmp, 26); \
					case 24: r24 = field(0, tmp, 25); \
					case 23: r23 = field(0, tmp, 24); \
					case 22: r22 = field(0, tmp, 23); \
					case 21: r21 = field(0, tmp, 22); \
					case 20: r20 = field(0, tmp, 21); \
					case 19: r19 = field(0, tmp, 20); \
					case 18: r18 = field(0, tmp, 19); \
					case 17: r17 = field(0, tmp, 18); \
					case 16: r16 = field(0, tmp, 17); \
					case 15: r15 = field(0, tmp, 16); \
					case 14: r14 = field(0, tmp, 15); \
					case 13: r13 = field(0, tmp, 14); \
					case 12: r12 = field(0, tmp, 13); \
					case 11: r11 = field(0, tmp, 12); \
					case 10: r10 = field(0, tmp, 11); \
					case 9: r9 = field(0, tmp, 10);	\
					case 8: r8 = field(0, tmp, 9);	\
					case 7: r7 = field(0, tmp, 8);	\
					case 6: r6 = field(0, tmp, 7);	\
					case 5: r5 = field(0, tmp, 6);	\
					case 4: r4 = field(0, tmp, 5);	\
					case 3: r3 = field(0, tmp, 4);	\
					case 2: r2 = field(0, tmp, 3);	\
					case 1: r1 = field(0, tmp, 2);	\
					case 0: ;			\
				}				\
				succip = (succcont);		\
				GOTO(field(0, tmp, 1));		\
			} while (0)

#define	call_semidet_closure(succcont)				\
			do {					\
				Word tmp;			\
				tmp = r1;			\
				switch(field(0, tmp, 0)) {	\
					default: fatal_error("call_closure: too many arguments (>= 32)"); \
					case 31: r32 = field(0, tmp, 32); \
					case 30: r31 = field(0, tmp, 31); \
					case 29: r30 = field(0, tmp, 30); \
					case 28: r29 = field(0, tmp, 29); \
					case 27: r28 = field(0, tmp, 28); \
					case 26: r27 = field(0, tmp, 27); \
					case 25: r26 = field(0, tmp, 26); \
					case 24: r25 = field(0, tmp, 25); \
					case 23: r24 = field(0, tmp, 24); \
					case 22: r23 = field(0, tmp, 23); \
					case 21: r22 = field(0, tmp, 22); \
					case 20: r21 = field(0, tmp, 21); \
					case 19: r20 = field(0, tmp, 20); \
					case 18: r19 = field(0, tmp, 19); \
					case 17: r18 = field(0, tmp, 18); \
					case 16: r17 = field(0, tmp, 17); \
					case 15: r16 = field(0, tmp, 16); \
					case 14: r15 = field(0, tmp, 15); \
					case 13: r14 = field(0, tmp, 14); \
					case 12: r13 = field(0, tmp, 13); \
					case 11: r12 = field(0, tmp, 12); \
					case 10: r11 = field(0, tmp, 11); \
					case 9: r10 = field(0, tmp, 10); \
					case 8: r9 = field(0, tmp, 9);	\
					case 7: r8 = field(0, tmp, 8);	\
					case 6: r7 = field(0, tmp, 7);	\
					case 5: r6 = field(0, tmp, 6);	\
					case 4: r5 = field(0, tmp, 5);	\
					case 3: r4 = field(0, tmp, 4);	\
					case 2: r3 = field(0, tmp, 3);	\
					case 1: r2 = field(0, tmp, 2);	\
					case 0: ;			\
				}				\
				succip = (succcont);		\
				GOTO(field(0, tmp, 1));		\
			} while (0)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define	callentry(procname, succcont)				\
			do {					\
				extern EntryPoint ENTRY(procname); \
				call(ENTRY(procname), succcont); \
			} while (0)

#define	localtailcall(label)					\
			do {					\
				debugtailcall(LABEL(label));	\
				GOTO_LABEL(label);		\
			} while (0)
#define	tailcall(proc)	do {					\
				debugtailcall(proc);		\
				GOTO(proc);			\
			} while (0)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define	tailcallentry(procname)					\
			do {					\
				extern EntryPoint ENTRY(procname); \
				tailcall(ENTRY(procname));	\
			} while (0)

#define	proceed()	do {					\
				debugproceed();			\
				GOTO(succip);			\
			} while (0)

/* DEFINITIONS FOR MANIPULATING THE HEAP */

#ifdef CONSERVATIVE_GC

#error "CONSERVATIVE_GC not yet implemented"

#else

#define	tag_incr_hp(dest,tag,count)	(			\
				(dest) = mkword(tag, (Word)hp),	\
				debugincrhp(count, hp),		\
				hp += (count),			\
				heap_overflow_check(),		\
				(void)0				\
			)

#define	incr_hp(dest,count)	(				\
				(dest) = (Word)hp,		\
				debugincrhp(count, hp),		\
				hp += (count),			\
				heap_overflow_check(),		\
				(void)0				\
			)

#define	mark_hp(dest)	(					\
				(dest) = (Word)hp,		\
				(void)0				\
			)

#define	restore_hp(src)	(					\
				LVALUE_CAST(Word,hp) = (src),	\
				(void)0				\
			)

#endif

/*
** Note that gcc optimizes `hp += 2; return hp - 2;'
** to `tmp = hp; hp += 2; return tmp;', so we don't need to use
** gcc's expression statements here
*/

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create1(w1)	(					\
				hp = hp + 1,			\
				hp[-1] = (Word) (w1),		\
				debugcr1(hp[-1], hp),		\
				heap_overflow_check(),		\
				/* return */ (Word) (hp - 1)	\
			)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create2(w1, w2)	(					\
				hp = hp + 2,			\
				hp[-2] = (Word) (w1),		\
				hp[-1] = (Word) (w2),		\
				debugcr2(hp[-2], hp[-1], hp),	\
				heap_overflow_check(),		\
				/* return */ (Word) (hp - 2)	\
			)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create2_bf(w1)	(					\
				hp = hp + 2,			\
				hp[-2] = (Word) (w1),		\
				heap_overflow_check(),		\
				/* return */ (Word) (hp - 2)	\
			)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create2_fb(w2)	(					\
				hp = hp + 2,			\
				hp[-1] = (Word) (w2),		\
				heap_overflow_check(),		\
				/* return */ (Word) (hp - 2)	\
			)

/* DEFINITIONS FOR MANIPULATING THE STACK */

#define	detstackvar(n)	sp[-n]

#define	incr_sp(n)	(					\
				debugincrsp(n, sp),		\
				sp = sp + (n),			\
				detstack_overflow_check(),	\
				(void)0				\
			)

#define	decr_sp(n)	(					\
				debugdecrsp(n, sp),		\
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

/* DEFINITIONS FOR NONDET STACK FRAMES */

#ifdef	SPEED

#define	REDOIP		(-0)	/* in this proc, set up at clause entry	*/
#define	PREVFR		(-1)	/* prev frame on stack, set up at call	*/
#define	SUCCIP		(-2)	/* in caller proc, set up at call	*/
#define	SUCCFR		(-3)	/* frame of caller proc, set up at call	*/
#define	SAVEVAL		(-4)	/* saved values start at this offset	*/

#define	NONDET_FIXED_SIZE	4	/* units: words */

#define	bt_prednm(fr)	"unknown"

#else

#define	PREDNM		(-0)	/* for debugging, set up at call 	*/
#define	REDOIP		(-1)	/* in this proc, set up at clause entry	*/
#define	PREVFR		(-2)	/* prev frame on stack, set up at call	*/
#define	SUCCIP		(-3)	/* in caller proc, set up at call	*/
#define	SUCCFR		(-4)	/* frame of caller proc, set up at call	*/
#define	SAVEVAL		(-5)	/* saved values start at this offset	*/

#define	NONDET_FIXED_SIZE	5	/* units: words */

#define	bt_prednm(fr)	LVALUE_CAST(const char *, fr[PREDNM])

#endif

#define	bt_redoip(fr)	LVALUE_CAST(Code *, fr[REDOIP])
#define	bt_prevfr(fr)	LVALUE_CAST(Word *, fr[PREVFR])
#define	bt_succip(fr)	LVALUE_CAST(Code *, fr[SUCCIP])
#define	bt_succfr(fr)	LVALUE_CAST(Word *, fr[SUCCFR])
#define	bt_var(fr,n)	fr[SAVEVAL-n]

#define	curprednm	bt_prednm(curfr)
#define	curredoip	bt_redoip(curfr)
#define	curprevfr	bt_prevfr(curfr)
#define	cursuccip	bt_succip(curfr)
#define	cursuccfr	bt_succfr(curfr)
#define	framevar(n)	bt_var(curfr,n)

/* DEFINITIONS FOR MANIPULATING THE NONDET STACK */

#ifdef	SPEED

#define	mkframe(prednm, n, redoip)				\
			do {					\
				reg	Word	*prevfr;	\
				reg	Word	*succfr;	\
								\
				prevfr = maxfr;			\
				succfr = curfr;			\
				maxfr = maxfr + (NONDET_FIXED_SIZE + n);\
				curfr = maxfr;			\
				curfr[REDOIP] = (Word) redoip;	\
				curfr[PREVFR] = (Word) prevfr;	\
				curfr[SUCCIP] = (Word) succip;	\
				curfr[SUCCFR] = (Word) succfr;	\
				debugmkframe();			\
				nondstack_overflow_check();	\
			} while (0)

#else

#define	mkframe(prednm, n, redoip)				\
			do {					\
				reg	Word	*prevfr;	\
				reg	Word	*succfr;	\
								\
				prevfr = maxfr;			\
				succfr = curfr;			\
				maxfr = maxfr + (NONDET_FIXED_SIZE + n);\
				curfr = maxfr;			\
				curfr[PREDNM] = (Word) prednm;	\
				curfr[REDOIP] = (Word) redoip;	\
				curfr[PREVFR] = (Word) prevfr;	\
				curfr[SUCCIP] = (Word) succip;	\
				curfr[SUCCFR] = (Word) succfr;	\
				debugmkframe();			\
				nondstack_overflow_check();	\
			} while (0)

#endif

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

#define IF(cond, val)	((cond) ? ((val),(void)0) : (void)0)

#ifdef	SPEED

#define	heap_overflow_check()		((void)0)
#define	detstack_overflow_check()	((void)0)
#define	detstack_underflow_check()	((void)0)
#define	nondstack_overflow_check()	((void)0)
#define	nondstack_underflow_check()	((void)0)

#else

#define	heap_overflow_check()					\
			(					\
				IF (hp >= heapend,(		\
					fatal_error("heap overflow") \
				)),				\
				IF (hp > heapmax,(		\
					heapmax = hp		\
				)),				\
				(void)0				\
			)

#define	detstack_overflow_check()				\
			(					\
				IF (sp >= detstackend,(		\
					fatal_error("stack overflow") \
				)),				\
				IF (sp > detstackmax,(		\
					detstackmax = sp	\
				)),				\
				(void)0				\
			)

#define	detstack_underflow_check()				\
			(					\
				IF (sp < detstackmin,(		\
					fatal_error("stack underflow") \
				)),				\
				(void)0				\
			)

#define	nondstack_overflow_check()				\
			(					\
				IF (maxfr >= nondstackend,(	\
					fatal_error("nondstack overflow") \
				)),				\
				IF (maxfr > nondstackmax,(	\
					nondstackmax = maxfr	\
				)),				\
				(void)0				\
			)

#define	nondstack_underflow_check()				\
			(					\
				IF (maxfr < nondstackmin,(	\
					fatal_error("nondstack underflow") \
				)),				\
				(void)0				\
			)

#endif

/* DEFINITIONS FOR DEBUGGING MESSAGES */

#ifdef	SPEED

#define	debugcr1(val0, hp)			((void)0)
#define	debugcr2(val0, val1, hp)		((void)0)
#define	debugincrhp(val, hp)			((void)0)
#define	debugincrsp(val, sp)			((void)0)
#define	debugdecrsp(val, sp)			((void)0)
#define	debugpush(val, sp)			((void)0)
#define	debugpop(val, sp)			((void)0)
#define	debugregs(msg)				((void)0)
#define	debugframe(msg)				((void)0)
#define	debugmkframe()				((void)0)
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

#define	debugincrhp(val, hp) \
	IF (heapdebug, (save_registers(), incr_hp_msg((val), (hp))))

#define	debugincrsp(val, sp) \
	IF (detstackdebug, (save_registers(), incr_sp_msg((val), (sp))))

#define	debugdecrsp(val, sp) \
	IF (detstackdebug, (save_registers(), decr_sp_msg((val), (sp))))

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

/* STRING HANDLING */

#define string_const(string, len) ((Word)string)
#define string_equal(s1,s2) (strcmp((char*)(s1),(char*)(s2))==0)

/* DEFINITIONS TO SUPPORT DEBUGGING */

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
	** if it's not gcc, then we can't use global register variables,
	** so we don't need to worry about saving/restoring them
	** (which would have been tricky to do in a portable macro)
	*/
#define mklist(start,len) do_mklist(start,len)
#endif

#include	"aux.h"
#include	"label.h"
#include	"memory.h"
#include	"wrapper.h"

#endif /* IMP_H */
